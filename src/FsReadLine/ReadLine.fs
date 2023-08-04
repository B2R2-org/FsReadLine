(*
  B2R2.FsReadLine - a GNU readline implementation in F#.

  Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

  Copyright (c) SoftSec Lab. @ KAIST, since 2016

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*)

module internal B2R2.FsReadLine.ReadLine

open System

let inline incCursorPos ctxt = ctxt.CursorPos <- ctxt.CursorPos + 1
let inline decCursorPos ctxt = ctxt.CursorPos <- ctxt.CursorPos - 1
let inline incCursorLim ctxt = ctxt.CursorLim <- ctxt.CursorLim + 1
let inline decCursorLim ctxt = ctxt.CursorLim <- ctxt.CursorLim - 1
let inline isStartOfLine ctxt = ctxt.CursorPos = 0
let inline isEndOfLine ctxt = ctxt.CursorPos = ctxt.CursorLim
let inline isStartOfTerminal () = Console.CursorLeft = 0
let inline isEndOfTerminal () = Console.CursorLeft = Console.BufferWidth - 1

let inline getSubstrLeftOfCursor ctxt =
  ctxt.Builder.ToString().Substring(ctxt.CursorPos)

let moveCursorEndOfAboveLine () =
  Console.SetCursorPosition (Console.BufferWidth - 1, Console.CursorTop - 1)

let moveCursorJustLeft () =
  Console.SetCursorPosition (Console.CursorLeft - 1, Console.CursorTop)

let moveCursorLeft ctxt =
  if isStartOfLine ctxt then ()
  elif isStartOfTerminal () then moveCursorEndOfAboveLine (); decCursorPos ctxt
  else moveCursorJustLeft (); decCursorPos ctxt

let moveCursorStartOfNextLine () =
  Console.SetCursorPosition (0, Console.CursorTop + 1)

let moveCursorJustRight () =
  Console.SetCursorPosition (Console.CursorLeft + 1, Console.CursorTop)

let moveCursorRight ctxt =
  if isEndOfLine ctxt then ()
  elif isEndOfTerminal () then moveCursorStartOfNextLine (); incCursorPos ctxt
  else moveCursorJustRight (); incCursorPos ctxt

let rec moveCursorHome ctxt =
  if isStartOfLine ctxt then ()
  else moveCursorLeft ctxt; moveCursorHome ctxt

let rec moveCursorEnd ctxt =
  if isEndOfLine ctxt then ()
  else moveCursorRight ctxt; moveCursorEnd ctxt

let writeCharEndOfLine ctxt (ch: char) =
  Console.Write (ch.ToString ())
  ctxt.Builder.Append (ch) |> ignore
  incCursorPos ctxt
  incCursorLim ctxt

let writeCharMiddle ctxt (ch: char) =
  let left = Console.CursorLeft
  let top = Console.CursorTop
  let str = getSubstrLeftOfCursor ctxt
  ctxt.Builder.Insert (ctxt.CursorPos, ch) |> ignore
  Console.Write (ch.ToString () + str)
  Console.SetCursorPosition (left, top)
  moveCursorRight ctxt
  incCursorLim ctxt

let writeChar ctxt ch =
  if isEndOfLine ctxt then writeCharEndOfLine ctxt ch
  else writeCharMiddle ctxt ch

let removeCharAtCursor ctxt =
  let _ = ctxt.Builder.Remove (ctxt.CursorPos, 1)
  let replacement = getSubstrLeftOfCursor ctxt
  let left = Console.CursorLeft
  let top = Console.CursorTop
  Console.Write("{0} ", replacement)
  Console.SetCursorPosition (left, top)
  decCursorLim ctxt

let backspace ctxt =
  if isStartOfLine ctxt then ()
  else moveCursorLeft ctxt; removeCharAtCursor ctxt

let delete ctxt =
  if isEndOfLine ctxt then ()
  else removeCharAtCursor ctxt

let getPrevWordPosition ctxt =
  let mutable pos = ctxt.CursorPos - 1
  while pos > 0 && ctxt.Builder.[pos] = ' ' do pos <- pos - 1
  while pos >= 0 && ctxt.Builder.[pos] <> ' ' do pos <- pos - 1
  pos + 1

let wordLeft ctxt action =
  let targetPos = getPrevWordPosition ctxt
  let rec loop ctxt =
    if not (isStartOfLine ctxt) && ctxt.CursorPos > targetPos then
      action ctxt
      loop ctxt
    else ()
  loop ctxt

let getNextWordPosition ctxt =
  let mutable pos = ctxt.CursorPos
  while pos < ctxt.CursorLim && ctxt.Builder.[pos] <> ' ' do pos <- pos + 1
  while pos < ctxt.CursorLim && ctxt.Builder.[pos] = ' ' do pos <- pos + 1
  pos

let wordRight ctxt action =
  let targetPos = getNextWordPosition ctxt
  let rec loop ctxt =
    if not (isEndOfLine ctxt) && ctxt.CursorPos < targetPos then
      action ctxt
      loop ctxt
    else ()
  loop ctxt

let rec removeFromBeginToCursor ctxt =
  if isStartOfLine ctxt then ()
  else backspace ctxt; removeFromBeginToCursor ctxt

let clearLine ctxt =
  moveCursorEnd ctxt
  removeFromBeginToCursor ctxt

let rec removeFromCursorToEnd ctxt =
  if isEndOfLine ctxt then ()
  else removeCharAtCursor ctxt; removeFromCursorToEnd ctxt

let writeStr ctxt str =
  str |> Seq.iter (writeChar ctxt)

let prevHistory ctxt =
  let h = ctxt.History
  match h.BwdList with
  | cmd :: rest ->
    clearLine ctxt
    writeStr ctxt cmd
    ctxt.History <- { h with FwdList = cmd :: h.FwdList; BwdList = rest }
  | [] -> ()

let nextHistory ctxt =
  let h = ctxt.History
  match h.FwdList with
  | hd :: cmd :: rest ->
    clearLine ctxt
    writeStr ctxt cmd
    ctxt.History <- { h with FwdList = cmd :: rest; BwdList = hd :: h.BwdList }
  | [cmd] ->
    ctxt.History <- { h with FwdList = []; BwdList = cmd :: h.BwdList }
    clearLine ctxt
  | [] -> ()

let appendNewLines cnt =
  Seq.replicate cnt Environment.NewLine
  |> String.concat ""
  |> Console.Write

let clearScreen ctxt =
  let input = ctxt.Builder.ToString ()
  clearLine ctxt
  Console.Clear ()
  Console.Write ctxt.Prompt
  writeStr ctxt input

let tabComplete ctxt =
  let input = ctxt.Builder.ToString ()
  match input |> TabCompletion.candidates ctxt.TabInfo with
  | [] -> ()
  | [candidate] -> clearLine ctxt; writeStr ctxt candidate
  | lst ->
    clearLine ctxt
    Console.WriteLine ()
    lst |> List.iter Console.WriteLine
    Console.Write (ctxt.Prompt)
    writeStr ctxt input

let keyHandle ctxt (info: ConsoleKeyInfo) =
  let isCtrlPushed = info.Modifiers = ConsoleModifiers.Control
  match info.Key with
  | ConsoleKey.Tab -> tabComplete ctxt
  | ConsoleKey.Backspace when isCtrlPushed -> wordLeft ctxt backspace
  | ConsoleKey.Backspace -> backspace ctxt
  | ConsoleKey.Delete -> delete ctxt
  | ConsoleKey.LeftArrow when isCtrlPushed -> wordLeft ctxt moveCursorLeft
  | ConsoleKey.LeftArrow -> moveCursorLeft ctxt
  | ConsoleKey.RightArrow when isCtrlPushed -> wordRight ctxt moveCursorRight
  | ConsoleKey.RightArrow -> moveCursorRight ctxt
  | ConsoleKey.Home -> moveCursorHome ctxt
  | ConsoleKey.End -> moveCursorEnd ctxt
  | ConsoleKey.UpArrow -> prevHistory ctxt
  | ConsoleKey.DownArrow -> nextHistory ctxt
  | ConsoleKey.W when isCtrlPushed -> wordLeft ctxt backspace
  | ConsoleKey.A when isCtrlPushed -> moveCursorHome ctxt
  | ConsoleKey.E when isCtrlPushed -> moveCursorEnd ctxt
  | ConsoleKey.B when isCtrlPushed -> moveCursorLeft ctxt
  | ConsoleKey.F when isCtrlPushed -> moveCursorRight ctxt
  | ConsoleKey.D when isCtrlPushed -> delete ctxt
  | ConsoleKey.U when isCtrlPushed -> removeFromBeginToCursor ctxt
  | ConsoleKey.P when isCtrlPushed -> prevHistory ctxt
  | ConsoleKey.N when isCtrlPushed -> nextHistory ctxt
  | ConsoleKey.K when isCtrlPushed -> removeFromCursorToEnd ctxt
  | ConsoleKey.L when isCtrlPushed -> clearScreen ctxt
  | ConsoleKey.Escape -> ()
  | _ -> info.KeyChar |> writeChar ctxt

let private updateHistory ctxt cmdline =
  ctxt.History <- History.Add ctxt.History cmdline

let rec private readLoop ctxt =
  let info = Console.ReadKey (true)
  if info.Key <> ConsoleKey.Enter then
    keyHandle ctxt info
    readLoop ctxt
  else Console.WriteLine ()

let private readCmdLine ctxt =
  readLoop ctxt
  let str = ctxt.Builder.ToString ()
  ReadLineContext.Clear ctxt
  str

let read ctxt =
  Console.Write (ctxt.Prompt)
  let cmdline = readCmdLine ctxt
  if String.IsNullOrWhiteSpace cmdline then ""
  else updateHistory ctxt cmdline; cmdline

let addCancelEventHandler ctxt handler =
  let myhandler sender (args: ConsoleCancelEventArgs) =
    let cancel = handler sender
    Console.WriteLine ()
    Console.Write ctxt.Prompt
    ReadLineContext.Clear ctxt
    args.Cancel <- cancel
  Console.CancelKeyPress.AddHandler myhandler
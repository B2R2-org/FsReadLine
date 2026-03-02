(*
  B2R2.FsReadLine - a GNU readline implementation in F#.

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

let inline incCursorPos ctx = ctx.CursorPos <- ctx.CursorPos + 1

let inline decCursorPos ctx = ctx.CursorPos <- ctx.CursorPos - 1

let inline incCursorLim ctx = ctx.CursorLim <- ctx.CursorLim + 1

let inline decCursorLim ctx = ctx.CursorLim <- ctx.CursorLim - 1

let inline isStartOfLine ctx = ctx.CursorPos = 0

let inline isEndOfLine ctx = ctx.CursorPos = ctx.CursorLim

let inline isStartOfTerminal () = Console.CursorLeft = 0

let inline isEndOfTerminal () = Console.CursorLeft = Console.BufferWidth - 1

let inline getSubstrLeftOfCursor ctx =
  ctx.Builder.ToString().Substring(ctx.CursorPos)

let moveCursorEndOfAboveLine () =
  Console.SetCursorPosition(Console.BufferWidth - 1, Console.CursorTop - 1)

let moveCursorJustLeft () =
  Console.SetCursorPosition(Console.CursorLeft - 1, Console.CursorTop)

let moveCursorLeft ctx =
  if isStartOfLine ctx then ()
  elif isStartOfTerminal () then moveCursorEndOfAboveLine (); decCursorPos ctx
  else moveCursorJustLeft (); decCursorPos ctx

let moveCursorStartOfNextLine () =
  Console.SetCursorPosition(0, Console.CursorTop + 1)

let moveCursorJustRight () =
  Console.SetCursorPosition(Console.CursorLeft + 1, Console.CursorTop)

let moveCursorRight ctx =
  if isEndOfLine ctx then ()
  elif isEndOfTerminal () then moveCursorStartOfNextLine (); incCursorPos ctx
  else moveCursorJustRight (); incCursorPos ctx

let rec moveCursorHome ctx =
  if isStartOfLine ctx then ()
  else moveCursorLeft ctx; moveCursorHome ctx

let rec moveCursorEnd ctx =
  if isEndOfLine ctx then ()
  else moveCursorRight ctx; moveCursorEnd ctx

let writeCharEndOfLine ctx (ch: char) =
  Console.Write(ch.ToString())
  ctx.Builder.Append(ch) |> ignore
  incCursorPos ctx
  incCursorLim ctx

let writeCharMiddle ctx (ch: char) =
  let left = Console.CursorLeft
  let top = Console.CursorTop
  let str = getSubstrLeftOfCursor ctx
  ctx.Builder.Insert(ctx.CursorPos, ch) |> ignore
  Console.Write(ch.ToString() + str)
  Console.SetCursorPosition(left, top)
  moveCursorRight ctx
  incCursorLim ctx

let writeChar ctx ch =
  if isEndOfLine ctx then writeCharEndOfLine ctx ch
  else writeCharMiddle ctx ch

let removeCharAtCursor ctx =
  let _ = ctx.Builder.Remove(ctx.CursorPos, 1)
  let replacement = getSubstrLeftOfCursor ctx
  let left = Console.CursorLeft
  let top = Console.CursorTop
  Console.Write("{0} ", replacement)
  Console.SetCursorPosition(left, top)
  decCursorLim ctx

let backspace ctx =
  if isStartOfLine ctx then ()
  else moveCursorLeft ctx; removeCharAtCursor ctx

let delete ctx =
  if isEndOfLine ctx then ()
  else removeCharAtCursor ctx

let getPrevWordPosition ctx =
  let mutable pos = ctx.CursorPos - 1
  while pos > 0 && ctx.Builder[pos] = ' ' do pos <- pos - 1
  while pos >= 0 && ctx.Builder[pos] <> ' ' do pos <- pos - 1
  pos + 1

let wordLeft ctx action =
  let targetPos = getPrevWordPosition ctx
  let rec loop ctx =
    if not (isStartOfLine ctx) && ctx.CursorPos > targetPos then
      action ctx
      loop ctx
    else ()
  loop ctx

let getNextWordPosition ctx =
  let mutable pos = ctx.CursorPos
  while pos < ctx.CursorLim && ctx.Builder[pos] <> ' ' do pos <- pos + 1
  while pos < ctx.CursorLim && ctx.Builder[pos] = ' ' do pos <- pos + 1
  pos

let wordRight ctx action =
  let targetPos = getNextWordPosition ctx
  let rec loop ctx =
    if not (isEndOfLine ctx) && ctx.CursorPos < targetPos then
      action ctx
      loop ctx
    else ()
  loop ctx

let rec removeFromBeginToCursor ctx =
  if isStartOfLine ctx then ()
  else backspace ctx; removeFromBeginToCursor ctx

let clearLine ctx =
  moveCursorEnd ctx
  removeFromBeginToCursor ctx

let rec removeFromCursorToEnd ctx =
  if isEndOfLine ctx then ()
  else removeCharAtCursor ctx; removeFromCursorToEnd ctx

let writeStr ctx str =
  str |> String.iter (writeChar ctx)

let prevHistory ctx =
  let h = ctx.History
  match h.BwdList with
  | cmd :: rest ->
    clearLine ctx
    writeStr ctx cmd
    ctx.History <- { h with FwdList = cmd :: h.FwdList; BwdList = rest }
  | [] -> ()

let nextHistory ctx =
  let h = ctx.History
  match h.FwdList with
  | hd :: cmd :: rest ->
    clearLine ctx
    writeStr ctx cmd
    ctx.History <- { h with FwdList = cmd :: rest; BwdList = hd :: h.BwdList }
  | [ cmd ] ->
    ctx.History <- { h with FwdList = []; BwdList = cmd :: h.BwdList }
    clearLine ctx
  | [] -> ()

let appendNewLines cnt =
  Seq.replicate cnt Environment.NewLine
  |> String.concat ""
  |> Console.Write

let clearScreen ctx =
  let input = ctx.Builder.ToString()
  clearLine ctx
  ctx.Callback.OnClearScreen ctx.Prompt
  writeStr ctx input

let tabComplete ctx =
  let input = ctx.Builder.ToString()
  match input |> ctx.TabCompletion.Candidates with
  | [] -> ()
  | [ candidate ] -> clearLine ctx; writeStr ctx candidate
  | lst ->
    clearLine ctx
    ctx.Callback.OnTabComplete(ctx.Prompt, lst)
    writeStr ctx input

let keyHandle ctx (info: ConsoleKeyInfo) =
  let isCtrlPushed = info.Modifiers = ConsoleModifiers.Control
  match info.Key with
  | ConsoleKey.Tab -> tabComplete ctx
  | ConsoleKey.Backspace when isCtrlPushed -> wordLeft ctx backspace
  | ConsoleKey.Backspace -> backspace ctx
  | ConsoleKey.Delete -> delete ctx
  | ConsoleKey.LeftArrow when isCtrlPushed -> wordLeft ctx moveCursorLeft
  | ConsoleKey.LeftArrow -> moveCursorLeft ctx
  | ConsoleKey.RightArrow when isCtrlPushed -> wordRight ctx moveCursorRight
  | ConsoleKey.RightArrow -> moveCursorRight ctx
  | ConsoleKey.Home -> moveCursorHome ctx
  | ConsoleKey.End -> moveCursorEnd ctx
  | ConsoleKey.UpArrow -> prevHistory ctx
  | ConsoleKey.DownArrow -> nextHistory ctx
  | ConsoleKey.W when isCtrlPushed -> wordLeft ctx backspace
  | ConsoleKey.A when isCtrlPushed -> moveCursorHome ctx
  | ConsoleKey.E when isCtrlPushed -> moveCursorEnd ctx
  | ConsoleKey.B when isCtrlPushed -> moveCursorLeft ctx
  | ConsoleKey.F when isCtrlPushed -> moveCursorRight ctx
  | ConsoleKey.D when isCtrlPushed -> delete ctx
  | ConsoleKey.U when isCtrlPushed -> removeFromBeginToCursor ctx
  | ConsoleKey.P when isCtrlPushed -> prevHistory ctx
  | ConsoleKey.N when isCtrlPushed -> nextHistory ctx
  | ConsoleKey.K when isCtrlPushed -> removeFromCursorToEnd ctx
  | ConsoleKey.L when isCtrlPushed -> clearScreen ctx
  | ConsoleKey.Escape -> ()
  | _ -> info.KeyChar |> writeChar ctx

let private updateHistory ctx cmdline =
  ctx.History <- ctx.History.Add cmdline

let rec private readLoop ctx =
  let info = Console.ReadKey(true)
  if info.Key <> ConsoleKey.Enter then
    keyHandle ctx info
    readLoop ctx
  else
    ()

let private readCmdLine ctx =
  readLoop ctx
  let str = ctx.Builder.ToString()
  ctx.Callback.OnReadLine str
  ReadLineContext.Clear ctx
  str

let read ctx =
  Console.Write(ctx.Prompt)
  let cmdline = readCmdLine ctx
  if String.IsNullOrWhiteSpace cmdline then ""
  else updateHistory ctx cmdline; cmdline

let addCancelEventHandler ctx handler =
  let myhandler sender (args: ConsoleCancelEventArgs) =
    let cancel = handler sender
    Console.WriteLine()
    Console.Write ctx.Prompt
    ReadLineContext.Clear ctx
    args.Cancel <- cancel
  Console.CancelKeyPress.AddHandler myhandler

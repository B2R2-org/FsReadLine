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

namespace B2R2.FsReadLine

open System.Runtime.InteropServices

type Console(prompt, cmds, [<Optional>] callback: ICallback) =
  let callback =
    if isNull callback then
      { new ICallback with
          member _.OnReadLine _ = System.Console.WriteLine()

          member _.OnTabComplete(prompt, lst) =
            match lst with
            | [||]
            | [| _ |] ->
              ()
            | lst ->
              System.Console.WriteLine()
              lst |> Array.iter System.Console.WriteLine
              System.Console.Write(prompt)

          member _.OnClearScreen(prompt) =
            System.Console.Clear()
            System.Console.Write prompt }
    else
      callback

  let ctx = ReadLineContext.Init(prompt, cmds, callback)

  /// Updates prompt string on the fly.
  member _.UpdatePrompt str =
    ctx.Prompt <- str

  /// Sets cancel key (ctrl+c) handler. The handler takes an event sender object
  /// as input and returns a boolean as output. When the handler returns false,
  /// the process will terminate.
  member _.SetCancelKeyHandler handler =
    ReadLine.addCancelEventHandler ctx handler

  member _.ReadLine() =
    ReadLine.read ctx

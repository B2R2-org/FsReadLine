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

type Console(prompt, cmds, [<Optional>] readLineCallback: ICallback) =
  let readLineCallback =
    if isNull readLineCallback then
      { new ICallback with member _.Invoke _ = System.Console.WriteLine() }
    else
      readLineCallback
  let ctx = ReadLineContext.Init(prompt, cmds, readLineCallback)

  /// Update prompt string on the fly.
  member _.UpdatePrompt str =
    ctx.Prompt <- str

  /// Set cancel key (ctrl+c) handler. The handler takes an event sender object
  /// as input and returns a boolean as output. When the handler returns false,
  /// the process will terminate.
  member _.SetCancelKeyHandler handler =
    ReadLine.addCancelEventHandler ctx handler

  member _.ReadLine() =
    ReadLine.read ctx

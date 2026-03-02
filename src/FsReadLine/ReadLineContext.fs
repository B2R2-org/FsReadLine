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

open System.Text

type ReadLineContext =
  { mutable Prompt: string
    mutable CursorPos: int
    mutable CursorLim: int
    mutable History: History
    TabCompletion: TabCompletion
    Builder: StringBuilder
    ReadLineCallback: ICallback }
with
  static member Init(prompt, cmds, readLineCallback) =
    { Prompt = prompt
      CursorPos = 0
      CursorLim = 0
      History = History.Init()
      TabCompletion = TabCompletion cmds
      Builder = StringBuilder()
      ReadLineCallback = readLineCallback }

  static member Clear ctx =
    ctx.CursorPos <- 0
    ctx.CursorLim <- 0
    ctx.Builder.Clear() |> ignore

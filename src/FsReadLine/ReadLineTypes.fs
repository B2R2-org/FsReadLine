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

namespace B2R2.FsReadLine

open System.Text

type History = private {
  FwdList: string list
  BwdList: string list
}
with
  static member Init () =
    { FwdList = []; BwdList = [] }

  static member Add history cmd =
    { history with BwdList = cmd :: history.BwdList }

type ReadLineContext = {
  mutable Prompt: string
  mutable CursorPos: int
  mutable CursorLim: int
  mutable History: History
  TabInfo: TabCompletionInfo
  Builder: StringBuilder
}
with
  static member Init prompt cmds =
    { Prompt = prompt
      CursorPos = 0
      CursorLim = 0
      History = History.Init ()
      TabInfo = TabCompletion.init cmds
      Builder = StringBuilder () }

  static member Clear ctxt =
    ctxt.CursorPos <- 0
    ctxt.CursorLim <- 0
    ctxt.Builder.Clear () |> ignore

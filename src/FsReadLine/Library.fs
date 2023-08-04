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

type Console (prompt, cmds) =
  let mutable ctxt = ReadLineContext.Init prompt cmds

  /// Update prompt string on the fly.
  member __.UpdatePrompt str =
    ctxt <- { ctxt with Prompt = str }

  /// Set cancel key (ctrl+c) handler. The handler takes an event sender object
  /// as input and returns a boolean as output. When the handler returns false,
  /// the process will terminate.
  member __.SetCancelKeyHandler handler =
    ReadLine.addCancelEventHandler ctxt handler

  member __.ReadLine () =
    ReadLine.read ctxt

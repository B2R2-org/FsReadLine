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

type TabCompletionInfo = {
  /// Prefix character to the list of cmd strings that start with the prefix.
  PrefixMap: Map<char, string list>
}

module internal TabCompletion =

  let private updatePrefixMap (cmd: string) map =
    let prefix = cmd.[0]
    if Map.containsKey prefix map then
      Map.add prefix (cmd :: Map.find prefix map) map
    else
      Map.add prefix [cmd] map

  let rec private buildPrefixMap map (cmds: string list) =
    match cmds with
    | cmd :: rest -> buildPrefixMap (updatePrefixMap cmd map) rest
    | [] -> map

  let init cmds =
    { PrefixMap = buildPrefixMap Map.empty cmds }

  let private cmdFilter input cmd =
    if String.length input >= String.length cmd then false
    else cmd.StartsWith (input)

  let candidates info (input: string) =
    let prefix = input.[0]
    match Map.tryFind prefix info.PrefixMap with
    | Some lst -> lst |> List.filter (cmdFilter input)
    | None -> []

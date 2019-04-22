(* Copyright (c) 2019, Dave Scott <dave@recoil.org>

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

type t = {
  bins: int array;
  start: float;
  width: float;
}
let make ~start ~width ~n () =
  let bins = Array.make n 0 in
  { start; width; bins }
let add ~value t =
  try
    (* Discard values which are out of range *)
    let bin = int_of_float ((value -. t.start) /. t.width) in
    t.bins.(bin) <- t.bins.(bin) + 1
  with _ -> ()
let to_segment ~label t =
  let points =
    t.bins
    |> Array.to_list
    |> List.mapi (fun idx v -> let t = float_of_int idx *. t.width +. t.start in t,float_of_int v) in

  C3.Segment.make ~label ~points ~kind:`Bar ()

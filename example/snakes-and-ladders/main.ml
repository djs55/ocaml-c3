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

(* Source square to destination square.
   If destination square > source square then this is a ladder.
   If destination square < source square then this is a snake. *)
let board = [
  4,14;
  9,31;
  17,7;
  20,38;
  28,84;
  40,59;
  54,34;
  62,18;
  63,81;
  71,91;
  87,24;
  93,73;
  95,75;
  99,78;
]

(* One character play, return the number of turns needed to finish. *)
let play () =
  let rec loop pos nturns =
    let roll = Random.int 6 + 1 in
    let pos = pos + roll in
    if pos >= 100
    then nturns (* we've won *)
    else
      (* look for a snake or ladder *)
      let pos = try List.assoc pos board with Not_found -> pos in
      loop pos (nturns + 1) in
  loop 1 0

(* Build a histogram of game lengths *)
let length () =
  let h = Histogram.make ~start:(-1.) ~width:1. ~n:200 () in
  let label = "Game length" in
  let chart =
    C3.Line.make ~kind:`XY ()
    |> C3.Line.add ~segment:(Histogram.to_segment ~label h)
    |> C3.Line.render ~bindto:"#length" in
  Lwt.async
    (fun () ->
      let open Lwt.Infix in
      let rec loop iterations =
        for _i = 0 to 250 do
          let nturns = play () in
          Histogram.add ~value:(float_of_int nturns) h
        done;
        C3.Line.update ~segments:[ Histogram.to_segment ~label h ] chart;
        Lwt_js.sleep 1.
        >>= fun () ->
        loop (iterations + 1) in
      loop 0
    )

let _ =
  Dom_html.window##.onload := Dom_html.handler
    (fun _ ->
      length ();
      Js._true
    )

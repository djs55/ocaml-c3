(* Copyright (c) 2015, Dave Scott <dave@recoil.org>

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

open Lwt

let d = Dom_html.document
let get_by_id id =
  Js.Opt.get (d##getElementById(Js.string id))
    (fun () -> assert false)

let rec update_graph_forever chart t () =
  let data = (
      [ t ],
      [ {
          C3.label = "sin(t)";
          values = [ sin t ];
          ty = C3.Area_step
        } ]
  ) in
  C3.flow chart ~flow_to:(`Delete 0) data;
  Lwt_js.sleep 0.1
  >>= fun () ->
  update_graph_forever chart (t +. 0.1) ()

let render () =
  (* Create an empty chart in the #chart div *)
  let chart = C3.generate "#chart" C3.empty in
  (* Update it in the background *)
  Lwt.async (update_graph_forever chart 0.)

let _ =
  Dom_html.window##onload <- Dom_html.handler
    (fun _ ->
      render ();
      Js._true
    )

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

let xychart () =
  let spec = { C3.Data.empty with
    C3.Data.x_axis = Some {
      C3.Axis.ty = C3.Axis_type.Line;
      format = "%d";
    };
    columns = (
      [ 0.1; 0.2; 0.3 ],
      [ { C3.Column.label = "";
          values = [0.1; 0.2; 0.3];
          ty = C3.Column_type.Line;
        } ]
    )
  } in
  let _ = C3.generate "#xychart" spec in
  ()

let rec update_graph_forever chart t () =
  if t > 10. then return ()
  else
  let data = (
      [ t ],
      [ {
          C3.Column.label = "sin(t)";
          values = [ sin t ];
          ty = C3.Column_type.Area_step
        } ]
  ) in
  C3.flow chart ~flow_to:(`Delete 0) data;
  Lwt_js.sleep 0.1
  >>= fun () ->
  update_graph_forever chart (t +. 0.1) ()

let timeseries () =
  let spec = { C3.Data.empty with
    C3.Data.x_axis = Some {
      C3.Axis.ty = C3.Axis_type.Timeseries;
      format = "%m/%d";
    }
  } in
  let chart = C3.generate "#timeserieschart" spec in
  Lwt.async (update_graph_forever chart 0.)

let _ =
  Dom_html.window##onload <- Dom_html.handler
    (fun _ ->
(*      xychart (); *)
      timeseries ();
      Js._true
    )

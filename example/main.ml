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

let multichart name =
  let spec = { C3.Data.empty with
    C3.Data.x_axis = Some {
      C3.Axis.ty = C3.Axis_type.Line;
      format = "%d";
    };
    columns =
      [ { C3.Column.label = "Area_step";
          tics = [ `X 0.1; `X 0.2; `X 0.3; `X 0.4; `X 0.5 ];
          values = [0.1; 0.2; 0.3; 0.2; 0.1];
          ty = C3.Column_type.Area_step;
        }; {
          C3.Column.label = "Line";
          tics = [ `X 0.1; `X 0.2; `X 0.3; `X 0.4; `X 0.5 ];
          values = [0.5; 0.4; 0.3; 0.2; 0.1];
          ty = C3.Column_type.Line;
        } ]
  } in
  let _ = C3.generate name spec in
  ()


let xychart ty name =
  let spec = { C3.Data.empty with
    C3.Data.x_axis = Some {
      C3.Axis.ty = C3.Axis_type.Line;
      format = "%d";
    };
    columns =
      [ { C3.Column.label = C3.Column_type.to_string ty;
          tics = [ `X 0.1; `X 0.2; `X 0.3; `X 0.4; `X 0.5 ];
          values = [0.1; 0.2; 0.3; 0.2; 0.1];
          ty;
        } ]
  } in
  let _ = C3.generate name spec in
  ()

let rec update_graph_forever chart t () =
  if t > 10. then return ()
  else
  let data =
      [ {
          C3.Column.label = "sin(t)";
          tics = [ `Time t ];
          values = [ sin t ];
          ty = C3.Column_type.Area_step
        } ] in
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
      multichart "#multichart";
      xychart C3.Column_type.Line "#xychart";
      xychart C3.Column_type.Area "#xyareachart";
      xychart C3.Column_type.Area_step "#xyareastepchart";
      xychart C3.Column_type.Spline "#xysplinechart";
      timeseries ();
      Js._true
    )

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
  let chart =
    C3.Line.make ~kind:`XY ()
    |> C3.Line.add_group
       ~segments: [ C3.Segment.make ~ty:C3.Column_type.Area_step
                    ~points:[ 0.1,0.1; 0.2,0.2; 0.3,0.3; 0.4,0.2; 0.5,0.1]
                    ~label:"Area_step 1" ();
                    C3.Segment.make ~ty:C3.Column_type.Area_step
                    ~points:[ 0.1,0.1; 0.2,0.2; 0.3,0.3; 0.4,0.2; 0.5,0.1]
                    ~label:"Area_step 2" (); ]
    |> C3.Line.add
       ~segment:(C3.Segment.make ~ty:C3.Column_type.Line
                 ~points:[ 0.1,0.5; 0.2,0.4; 0.3,0.3; 0.4,0.2; 0.5,0.1]
                 ~label:"Line" ())
    |> C3.Line.add
       ~segment:(C3.Segment.make ~ty:C3.Column_type.Bar
                 ~points:[ 0.1,0.1; 0.2,0.1; 0.3,0.1; 0.4,0.1; 0.5,0.1]
                 ~label:"Bar" ())
    |> C3.Line.to_chart in
  let _ = C3.generate name chart in
  ()


let xychart ty name =
  let chart =
    C3.Line.make ~kind:`XY ()
    |> C3.Line.add
       ~segment:(C3.Segment.make ~ty ~points:[0.1,0.1; 0.2,0.2; 0.3,0.3; 0.4,0.2; 0.5,0.1]
                 ~label:(C3.Column_type.to_string ty) ())
    |> C3.Line.to_chart in
    (*
  let spec = { C3.Chart.empty with
    C3.Chart.x_axis = Some {
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
  *)
  let _ = C3.generate name chart in
  ()

let piechart name donut =
  let pie = C3.Pie.make
    ?hole:(if donut then Some "hello" else None)
    ~columns:[ "a", 30.; "b", 120.; "c", 15.; "d", 90. ] () in
  let _ = C3.generate name (C3.Pie.to_chart pie) in
  ()

let gauge name =
  let gauge = C3.Gauge.make
    ~thresholds:[ 30., "#FF0000"; 60., "#F97600"; 90., "#F6C600"; 100., "#60B044" ]
    ~label:"hello"
    ~value:60.
    () in
  let _ = C3.generate name (C3.Gauge.to_chart gauge) in
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
  let spec = { C3.Chart.empty with
    C3.Chart.x_axis = Some {
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
      piechart "#piechart" false;
      piechart "#donutchart" true;
      gauge "#gauge";
      xychart C3.Column_type.Line "#xychart";
      xychart C3.Column_type.Area "#xyareachart";
      xychart C3.Column_type.Area_step "#xyareastepchart";
      xychart C3.Column_type.Spline "#xysplinechart";
      timeseries ();
      Js._true
    )

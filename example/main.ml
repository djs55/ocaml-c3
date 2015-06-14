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
  let _ =
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
    |> C3.Line.render name in
  ()


let xychart ty name =
  let _ =
    C3.Line.make ~kind:`XY ()
    |> C3.Line.add
       ~segment:(C3.Segment.make ~ty ~points:[0.1,0.1; 0.2,0.2; 0.3,0.3; 0.4,0.2; 0.5,0.1]
                 ~label:(C3.Column_type.to_string ty) ())
    |> C3.Line.render name in
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
  else begin
    C3.Line.update ~segments:[ C3.Segment.make ~label:"sin(t)" ~points:[t, sin t]
                               ~ty:C3.Column_type.Area_step () ]
                   ~flow_to:(`Delete 0)
                   chart;
    Lwt_js.sleep 0.1
    >>= fun () ->
    update_graph_forever chart (t +. 0.1) ()
  end

let timeseries () =
  let chart =
    C3.Line.make ~kind:`Timeseries ~x_format:"%m/%d" ()
    |> C3.Line.render "#timeserieschart" in

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

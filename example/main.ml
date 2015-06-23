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
    C3.Line.make ~kind:`XY ~x_label:"Some x_label" ()
    |> C3.Line.add_group
       ~segments: [ C3.Segment.make ~kind:`Area_step
                    ~points:[ 0.1,0.1; 0.2,0.2; 0.3,0.3; 0.4,0.2; 0.5,0.1]
                    ~label:"Area_step 1" ();
                    C3.Segment.make ~kind:`Area_step
                    ~points:[ 0.1,0.1; 0.2,0.2; 0.3,0.3; 0.4,0.2; 0.5,0.1]
                    ~label:"Area_step 2" (); ]
    |> C3.Line.add
       ~segment:(C3.Segment.make ~kind:`Line
                 ~points:[ 0.1,0.5; 0.2,0.4; 0.3,0.3; 0.4,0.2; 0.5,0.1]
                 ~label:"Line" ())
    |> C3.Line.add
       ~segment:(C3.Segment.make ~kind:`Bar
                 ~points:[ 0.1,0.1; 0.2,0.1; 0.3,0.1; 0.4,0.1; 0.5,0.1]
                 ~label:"Bar" ())
    |> C3.Line.render ~bindto:name in
  ()


let xychart kind name =
  let _ =
    C3.Line.make ~kind:`XY ()
    |> C3.Line.add
       ~segment:(C3.Segment.make ~kind ~points:[0.1,0.1; 0.2,0.2; 0.3,0.3; 0.4,0.2; 0.5,0.1]
                 ~label:(C3.Segment.string_of_kind kind) ())
    |> C3.Line.render ~bindto:name in
  ()

let piechart name donut =
  let _ =
    C3.Pie.make
      ?hole:(if donut then Some "hello" else None)
      ~sectors:[ "a", 30.; "b", 120.; "c", 15.; "d", 90. ] ()
    |> C3.Pie.render ~bindto:name in
  ()

let gauge name =
  let _ =
    C3.Gauge.make
      ~thresholds:[ 30., "#FF0000"; 60., "#F97600"; 90., "#F6C600"; 100., "#60B044" ]
      ~label:"hello"
      ~value:60.
      ()
    |> C3.Gauge.render ~bindto:name in
  ()



module Histogram = struct
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
  let to_segment t =
    let points =
      t.bins
      |> Array.to_list
      |> List.mapi (fun idx v -> let t = float_of_int idx *. t.width +. t.start in t,float_of_int v) in

    C3.Segment.make ~label:"histogram" ~points ~kind:`Bar ()
end

let pi = 4.0 *. atan(1.0)

let normal () =
  let h = Histogram.make ~start:(-1.) ~width:0.01 ~n:200 () in
  let chart =
    C3.Line.make ~kind:`XY ()
    |> C3.Line.add ~segment:(Histogram.to_segment h)
    |> C3.Line.render ~bindto:"#normal" in
  let sd = 0.1 in
  Lwt.async
    (fun () ->
      let rec loop iterations =
        for i = 0 to 250 do
          let u_1 = Random.float 1. in
          let u_2 = Random.float 1. in
          (* Box-Muller transform *)
          let z_0 = sqrt (-2.0 *. log(u_1)) *. cos(2. *. pi *. u_2) *. sd in
          let z_1 = sqrt (-2.0 *. log(u_1)) *. sin(2. *. pi *. u_2) *. sd in
          Histogram.add ~value:z_0 h;
          Histogram.add ~value:z_1 h;
        done;
        C3.Line.update ~segments:[ Histogram.to_segment h ] chart;
        Lwt_js.sleep 1.
        >>= fun () ->
        loop (iterations + 1) in
      loop 0
    )

let rec range i n =
  if i >= n then [] else i :: range (i + 1) n

let base =
  range 0 100
  |> List.map float_of_int
  |> List.map (fun x -> x /. 40.)

let timeseries () =
  let chart =
    C3.Line.make ~kind:`Timeseries ~x_format:"%m/%d" ()
    |> C3.Line.render ~bindto:"#timeserieschart" in

  let rec update_graph_forever chart t () =
    if t > 10. then return ()
    else begin
      let points = List.map (fun x -> x, sin (t +. x)) base in
      C3.Line.update ~segments:[ C3.Segment.make ~label:"sin(t)" ~points
                                 ~kind:`Area_spline () ]
                     chart;
      Lwt_js.sleep 0.1
      >>= fun () ->
      update_graph_forever chart (t +. 0.1) ()
    end in
  Lwt.async (update_graph_forever chart 0.)

let _ =
  Dom_html.window##onload <- Dom_html.handler
    (fun _ ->
      multichart "#multichart";
      piechart "#piechart" false;
      piechart "#donutchart" true;
      gauge "#gauge";
      xychart `Line "#xychart";
      xychart `Area "#xyareachart";
      xychart `Area_step "#xyareastepchart";
      xychart `Spline "#xysplinechart";
      normal ();
      timeseries ();
      Js._true
    )

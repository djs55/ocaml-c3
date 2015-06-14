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

module Column_type = struct
  type t = [
    | `Line
    | `Spline
    | `Area
    | `Area_spline
    | `Area_step
    | `Bar
    | `Pie (* columns are summed into individual sectors *)
    | `Donut (* same as pie *)
    | `Gauge (* single column *)
  ]

  let to_string = function
    | `Line -> "line"
    | `Spline -> "spline"
    | `Area -> "area"
    | `Area_spline -> "area-spline"
    | `Area_step -> "area-step"
    | `Bar -> "bar"
    | `Pie -> "pie"
    | `Donut -> "donut"
    | `Gauge -> "gauge"
end

module Tic = struct
  type t = [
    | `Time of float (* seconds since epoch *)
    | `X of float
  ]

  let to_float = function
    | `Time t -> t *. 1000. (* JS takes milliseconds since epoch *)
    | `X x -> x
end

module Column = struct
  type t = {
    label: string;
    tics: Tic.t list;
    values: float list;
    ty: Column_type.t;
  }
end

module Axis_type = struct
  type t = [
    | `Timeseries
    | `Line
  ]

  let to_string = function
    | `Timeseries -> "timeseries"
    | `Line -> "line"
end

module Axis = struct
  type t = {
    ty: Axis_type.t;
    format: string; (* eg '%m/%d' *)
  }
end

module Gauge_info = struct
  type colour = string

  type t = {
    min: float option;
    max: float option;
    units: string option;
    width: int option;
    thresholds: (float * colour) list option;
  }

  let make ?min ?max ?units ?width ?thresholds () =
    { min; max; units; width; thresholds }

  let default = {
    min = None; max = None; units = None; width = None; thresholds = None;
  }

  let to_gauge_obj x =
    let open Js.Unsafe in
    match x with
    | None -> []
    | Some x ->
      [ "gauge", obj (Array.of_list (
         ( match x.min with
           | None -> []
           | Some x -> [ "min", inject x ]
         ) @ (
           match x.max with
           | None -> []
           | Some x -> [ "max", inject x ]
         ) @ (
           match x.units with
           | None -> []
           | Some x -> [ "units", inject (Js.string x) ]
         ) @ (
           match x.width with
           | None -> []
           | Some x -> [ "width", inject x ]
         )))
      ]

  let to_color_obj x =
    let open Js.Unsafe in
    match x with
    | None -> []
    | Some { thresholds = None } -> []
    | Some { thresholds = Some ts } ->
      [ "color", obj [|
        "pattern", inject @@ Js.array @@ Array.of_list @@ List.map (fun x -> inject @@ Js.string @@ snd x) ts;
        "threshold", obj [|
          "values", inject @@ Js.array @@ Array.of_list @@ List.map (fun x -> inject @@ fst x) ts;
        |]
        |]
      ]
end

module Donut = struct
  type t = {
    title: string;
  }

  let to_donut_obj x =
    let open Js.Unsafe in
    match x with
    | None -> []
    | Some { title } -> [ "donut", obj [| "title", inject @@ Js.string title |] ]
end

module Chart = struct
  type t = {
    x_axis: Axis.t option;
    columns: Column.t list;
    donut: Donut.t option;
    gauge: Gauge_info.t option;
    groups: string list list;
  }

  let empty = {
    x_axis = None;
    columns =
      [ { Column.label = "";
          tics = [];
          values = [];
          ty = `Line;
        } ];
    donut = None;
    gauge = None;
    groups = [];
  }
end

module Pie = struct
  type t = {
    values: (string * float) list;
    hole: string option;
  }

  let empty = { values = []; hole = None }

  let add ~label ~value ~t () =
    { t with values = (label, value) :: t.values }

  let make ~columns ?hole () =
    let t = { empty with hole } in
    List.fold_left (fun acc (label, value) -> add ~label ~value ~t:acc ()) t columns

  let to_chart t =
    let ty = if t.hole = None then `Pie else `Donut in
    let column (label, value) =
      { Column.label; tics = []; values = [ value ]; ty } in
    let columns = List.map column t.values in
    let donut = match t.hole with
      | None -> None
      | Some title -> Some { Donut.title } in
    { Chart.empty with Chart.columns; donut }
end

module Gauge = struct
  type t = {
    value: float;
    label: string;
    info: Gauge_info.t;
  }

  let make ?min ?max ?units ?width ?thresholds ~value ~label () =
    let info = Gauge_info.make ?min ?max ?units ?width ?thresholds () in
    { value; label; info }

  let to_chart t =
    let columns = [ { Column.label = t.label; tics = []; values = [ t.value ]; ty = `Gauge} ] in
    let gauge = Some t.info in
    { Chart.empty with
      Chart.columns;
      gauge }
end

module Segment = struct
  type t = {
    points: (float * float) list;
    label: string;
    ty: Column_type.t;
  }

  let make ~points ~label ?(ty = `Line) () =
    { points; label; ty }

  let to_column kind t =
    let tics = match kind with
      | `XY -> List.map (fun x -> `X (fst x)) t.points
      | `Timeseries -> List.map (fun x -> `Time (fst x)) t.points in
    let values = List.map snd t.points in
    { Column.label = t.label; tics; values; ty = t.ty }
end


let js_of_columns columns =
  let tics = List.concat (List.map (fun c -> c.Column.tics) columns) in
  let data_columns =
    Js.Unsafe.(
      List.map (fun column ->
        Js.array (Array.of_list (inject (Js.string column.Column.label) :: (List.map inject column.Column.values)))
      ) columns
    ) in

  Js.Unsafe.(
    inject (Js.array (Array.of_list (
      match tics with
      | [] -> data_columns
      | tics ->
        let tics = Js.array (Array.of_list (inject (Js.string "x") :: (List.map (fun x -> inject (Tic.to_float x)) tics))) in
        tics :: data_columns
    )))
  )

let generate bindto data =
  let columns = js_of_columns data.Chart.columns in

  let axis =
    Js.Unsafe.(
      match data.Chart.x_axis with
      | None -> []
      | Some x -> [ "axis", obj [|
        "x", obj [|
          "type", inject (Js.string (Axis_type.to_string x.Axis.ty));
          "tick", obj [|
            "format", inject (Js.string x.Axis.format)
          |]
        |]
      |] ]
    ) in

    let data' =
      Js.Unsafe.(
        (if data.Chart.x_axis = None then [] else [
          "x", inject (Js.string "x");
          "xFormat", inject (Js.string "%s")
        ]) @ [
          "columns", columns;
          "types", obj (Array.of_list (List.map (fun column ->
            column.Column.label, inject (Js.string (Column_type.to_string column.Column.ty))
          ) data.Chart.columns));
          "groups", inject @@ Js.array @@ Array.of_list @@ List.map (fun g -> inject @@ Js.array @@ Array.of_list @@ List.map (fun x -> inject @@ Js.string x) g) data.Chart.groups;
        ]
      ) in

  let arg =
    Js.Unsafe.(obj
      (Array.of_list
        (axis @ [
          "bindto", inject (Js.string bindto);
          "data", obj (Array.of_list data')
        ] @ (Donut.to_donut_obj data.Chart.donut
        ) @ (Gauge_info.to_gauge_obj data.Chart.gauge
        ) @ (Gauge_info.to_color_obj data.Chart.gauge)
      ))) in
  Firebug.console##log(arg);

  let c3 = Js.Unsafe.global##c3 in

  Js.Unsafe.meth_call c3 "generate" [| arg |]

type flow_to = [
  | `OneInOneOut
  | `ToX of string
  | `Delete of int
]

let flow chart ?(flow_to = `OneInOneOut) cols : unit =
  let arg =
    Js.Unsafe.(obj
      (Array.of_list
        ((match flow_to with
          | `OneInOneOut -> []
          | `ToX x -> [ "to", inject (Js.string x) ]
          | `Delete n -> [ "length", inject n ] )
        @ [ "columns", js_of_columns cols ])
      )
    ) in
  Js.Unsafe.meth_call chart "flow" [| arg |]


module Line = struct
  type kind = [ `Timeseries | `XY ]

  type t = {
    kind: kind;
    x_format: string;
    groups: Segment.t list list;
  }

  let make ?(x_format = "%d") ~kind () =
    { kind; x_format; groups = [] }

  let add ~segment t =
    { t with groups = [ segment ] :: t.groups }

  let add_group ~segments t =
    { t with groups = segments :: t.groups }

  let to_chart t =
    let columns = List.map (Segment.to_column t.kind) (List.concat t.groups) in
    let groups = List.map (List.map (fun s -> s.Segment.label)) t.groups in
    let ty = match t.kind with `XY -> `Line | `Timeseries -> `Timeseries in
    let x_axis = Some {
      Axis.ty; format = t.x_format;
    } in
    { Chart.empty with Chart.x_axis; columns; groups }

  let render bindto t =
    let chart = generate bindto (to_chart t) in
    t.kind, chart

  let update ~segments ?flow_to (kind, chart) =
    let columns = List.map (Segment.to_column kind) segments in
    flow chart ?flow_to columns
end

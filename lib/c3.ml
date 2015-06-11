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
  type t =
    | Line
    | Area
    | Area_spline
    | Area_step

  let to_string = function
    | Line -> "line"
    | Area -> "area"
    | Area_spline -> "area-spline"
    | Area_step -> "area-step"
end

module Column = struct
  type t = {
    label: string;
    values: float list;
    ty: Column_type.t;
  }
end

module Axis_type = struct
  type t =
    | Timeseries
    | Line

  let to_string = function
    | Timeseries -> "timeseries"
    | Line -> "line"
end

module Axis = struct
  type t = {
    ty: Axis_type.t;
    format: string; (* eg '%m/%d' *)
  }
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


module Data = struct
  type t = {
    x_axis: Axis.t option;
    columns: (Tic.t list) * Column.t list;
  }

  let empty = {
    x_axis = None;
    columns = (
      [],
      [ { Column.label = "";
          values = [];
          ty = Column_type.Line;
        } ]
    )
  }
end


let js_of_columns (tics, columns) =
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
  let columns = js_of_columns data.Data.columns in

  let axis =
    Js.Unsafe.(
      match data.Data.x_axis with
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

    let data =
      Js.Unsafe.(
        (if data.Data.x_axis = None then [] else [
          "x", inject (Js.string "x");
          "xFormat", inject (Js.string "%s")
        ]) @ [
          "columns", columns;
          "types", obj (Array.of_list (List.map (fun column ->
            column.Column.label, inject (Js.string (Column_type.to_string column.Column.ty))
          ) (snd data.Data.columns)));
        ]
      ) in

  let arg =
    Js.Unsafe.(obj
      (Array.of_list
        (axis @ [
          "bindto", inject (Js.string bindto);
          "data", obj (Array.of_list data)
        ])
      ))  in
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

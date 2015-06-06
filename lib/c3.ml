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

type column_type =
  | Line
  | Area
  | Area_spline
  | Area_step

type column = {
  label: string;
  values: float list;
  ty: column_type;
}

type axis_ty =
  | Timeseries

type axis = {
  ty: axis_ty;
  format: string; (* eg '%m/%d' *)
}

type data = {
  x_axis: axis option;
  columns: (float list) * column list;
}

let example = {
  x_axis = Some {
    ty = Timeseries;
    format = "%m/%d";
  };
  columns = (
    [],
    [ { label = "host free memory";
        values = [];
        ty = Area_step;
      } ]
  )
}

let string_of_column_type = function
  | Line -> "line"
  | Area -> "area"
  | Area_spline -> "area-spline"
  | Area_step -> "area-step"

let string_of_axis_ty = function
  | Timeseries -> "timeseries"

let js_of_columns (tics, columns) =
  let data_columns =
    Js.Unsafe.(
      List.map (fun column ->
        Js.array (Array.of_list (inject (Js.string column.label) :: (List.map inject column.values)))
      ) columns
    ) in

  Js.Unsafe.(
    inject (Js.array (Array.of_list (
      match tics with
      | [] -> data_columns
      | tics ->
        let tics = Js.array (Array.of_list (inject (Js.string "x") :: (List.map (fun x -> inject (1000.0 *. x)) tics))) in
        tics :: data_columns
    )))
  )

let generate bindto data =
  let columns = js_of_columns data.columns in

  let axis =
    Js.Unsafe.(
      match data.x_axis with
      | None -> []
      | Some x -> [ "axis", obj [|
        "x", obj [|
          "type", inject (Js.string (string_of_axis_ty x.ty));
          "tick", obj [|
            "format", inject (Js.string x.format)
          |]
        |]
      |] ]
    ) in

    let data =
      Js.Unsafe.(
        (if data.x_axis = None then [] else [
          "x", inject (Js.string "x");
          "xFormat", inject (Js.string "%s")
        ]) @ [
          "columns", columns;
          "types", obj (Array.of_list (List.map (fun column ->
            column.label, inject (Js.string (string_of_column_type column.ty))
          ) (snd data.columns)));
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

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

module Pie : sig
  (** Pie charts, with or without a hole in the middle ("donut" charts). *)

  type t
  (** An unrendered pie chart *)

  val make: sectors:(string * float) list -> ?hole:string -> unit -> t
  (** Create a pie chart with a list of sectors. If [hole] is supplied
      then the pie chart will be a donut containing a string label. *)

  val add: label:string -> value:float -> t:t -> unit -> t
  (** Add a single sector to an unrendered pie chart *)

  type display
  (** A rendered pie chart *)

  val render: bindto:string -> t -> display
  (** Render a pie chart in a named <div> *)
end

module Gauge : sig
  (** A Gauge shows a single value within a defined minimum and maximum range *)

  type t
  (** An unrendered gauge *)

  type colour = string
  (** A colour, e.g. "#ffffff" *)

  val make: ?min:float -> ?max:float -> ?units:string
    -> ?width:int -> ?thresholds:(float * colour) list
    -> value:float -> label:string
    -> unit -> t
  (** Create a gauge with a given value and label. By default a gauge will
      be a percentage i.e. with min = 0, max = 100, units = " %". The
      thresholds allow customisation of the colour, e.g. green, amber, red
      if that helps interpret the meaning of the value. *)

  type display
  (** A rendered gauge *)

  val render: bindto:string -> t -> display
  (** Render a gauge chart in a named <div> *)
  
end

module Segment : sig
  type kind = [
  | `Line
  | `Spline
  | `Area
  | `Area_spline
  | `Area_step
  | `Bar
  ]
  val string_of_kind: kind -> string

  type t = {
    points: (float * float) list;
    label: string;
    kind: kind;
  }

  val make: points:(float * float) list -> label:string
    -> ?kind:kind -> unit -> t
end


type flow_to = [
  | `OneInOneOut
  | `ToX of string
  | `Delete of int
]

module Line : sig
  type kind = [ `Timeseries | `XY ]

  type t

  val make: ?x_format:string -> kind:kind -> unit -> t

  val add: segment:Segment.t -> t -> t

  val add_group: segments:Segment.t list -> t -> t

  type display

  val render: string -> t -> display

  val update: segments:Segment.t list -> ?flow_to:flow_to -> display -> unit
end

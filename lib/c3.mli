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
  (** A line segment within a Line chart *)

  type kind = [
    | `Line         (** Render with straight lines *)
    | `Spline       (** Render with splines to make it appear smooth *)
    | `Area         (** Render with straight lines with a translucent fill underneath *)
    | `Area_spline  (** Render with splines to make it appear smooth with a translucent fill underneath *)
    | `Area_step    (** Render as horizontal steps with a translucent fill underneath *)
    | `Bar          (** Render as discrete vertical bars *)
  ]
  val string_of_kind: kind -> string

  type t
  (** An unrendered line segment within a Line chart *)

  val make: points:(float * float) list -> label:string
    -> ?kind:kind -> unit -> t
  (** Create an unrendered line segment from a set of points and a label.
      By default it will render with straight lines. *)
end


type flow_to = [
  | `OneInOneOut    (** For every point added, remove the leftmost *)
  | `ToX of string  (** Move the minimum x co-ordinate to the given value *)
  | `Delete of int  (** Delete exactly n points from the leftmost edge *)
]

module Line : sig
  (** A line chart *)

  type kind = [ `Timeseries | `XY ]
  (** A line chart can show either timeseries data or arbitrary values on the
      X axis. *)

  type t
  (** An unrendered line chart *)

  val make: ?x_format:string -> kind:kind -> unit -> t
  (** Create an unrendered line chart, showing either `Timeseries or `XY data.
      The ?x_format is a format string for the labels on the x axis. *)

  val add: segment:Segment.t -> t -> t
  (** Add a line segment to an unrendered line chart. *)

  val add_group: segments:Segment.t list -> t -> t
  (** Add a group of line segments to an unrendered line chart. By grouping line
      segments they will be rendered stacked. *)

  type display
  (** A rendered line chart *)

  val render: bindto:string -> t -> display
  (** A rendered line chart *)

  val flow: segments:Segment.t list -> ?flow_to:flow_to -> display -> unit
  (** Dynamically extend a rendered line chart by adding a list of segments.
      The ?flow_to parameter customises how the chart will be added. *)

  val update: segments:Segment.t list -> display -> unit
  (** Replace the segments in an existing rendered chart with new ones.
      The old segments with the same label will be removed and replaced. *)
end

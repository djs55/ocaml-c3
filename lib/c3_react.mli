
module Segment : sig
  (** A reactive line segment within a Line chart *)

  type t
  (** An unrendered line segment within a Line chart *)

  val make:
    signal:(float * float) React.E.t -> label:string ->
    ?base:(float * float) list -> ?kind:C3.Segment.kind -> unit -> t
  (** Create an unrendered line segment from a set of an event of points
      and a label. By default it will render with straight lines.
      The argument [base] allow to provide a set of starting points.
  *)
end

module Line : sig
  (** A reactive line chart *)

  type t
  (** An unrendered line chart *)

  val make: ?x_format:string -> kind:C3.Line.kind -> unit -> t
  (** Create an unrendered line chart, showing either `Timeseries or `XY data.
      The ?x_format is a format string for the labels on the x axis. *)

  val add: segment:Segment.t -> t -> t
  (** Add a line segment to an unrendered line chart. *)

  val add_group: segments:Segment.t list -> t -> t
  (** Add a group of line segments to an unrendered line chart. By grouping line
      segments they will be rendered stacked. *)

  type flow = [
    | `OneInOneOut
    | `All
  ]

  val render: bindto:string -> flow:flow -> t -> unit
  (** Render a line chart and start listening to the events. *)
end

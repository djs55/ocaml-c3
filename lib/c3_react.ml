open Lwt_react


module Segment = struct

  type t = {
    segment : C3.Segment.t ;
    signal : (float * float) E.t ;
    label : string;
  }

  let make ~signal ~label ?(base= []) ?kind () =
    { segment = C3.Segment.make ?kind ~label ~points:base () ;
      signal ; label
    }

  let to_segment x = x.segment
  let to_pair_signal x = (x.label, x.signal)

end

module Line = struct

  type t = {
    line : C3.Line.t ;
    signals : (string * (float * float) E.t) list ;
  }

  let make ?x_format ~kind () =
    { line = C3.Line.make ?x_format ~kind () ; signals = [] }

  let add ~segment:{Segment. segment ; signal ; label} { line ; signals } =
    { line = C3.Line.add ~segment line ;
      signals = (label, signal) :: signals ;
    }

  let add_group ~segments { line ; signals } =
    { line =
        C3.Line.add_group
          ~segments:(List.map Segment.to_segment segments) line ;
      signals = (List.map Segment.to_pair_signal segments) @ signals ;
    }

  type flow = [
    | `OneInOneOut
    | `All
  ]

  let render ~bindto ~flow t =
    let d = C3.Line.render ~bindto t.line in
    let flow_to = match flow with
      | `OneInOneOut -> `OneInOneOut
      | `All -> `Delete 0
    in
    let update label p =
      let segments = [C3.Segment.make ~points:[p] ~label ()] in
      C3.Line.flow ~segments ~flow_to d
    in
    List.iter (fun (l, e) ->
      E.keep (E.map (update l) e)
    ) t.signals

end

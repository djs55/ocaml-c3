
open Lwt

let d = Dom_html.document
let get_by_id id =
  Js.Opt.get (d##getElementById(Js.string id))
    (fun () -> assert false)


let render () =
  Firebug.console##log (Js.string "rendering...");
  let chart = C3.generate "#chart" C3.example in

  Firebug.console##log (Js.string "... render complete")

let _ =
  Dom_html.window##onload <- Dom_html.handler
    (fun _ ->
      render ();
      Js._true
    )

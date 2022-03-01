(** main.ml *)

open Js_of_ocaml

let _ =
  Js.export "lmnAlpha"
    (object%js
       method add x y = x +. y

       method run code is_compile_only is_verbose does_trace =
         Js.string @@ String.concat "\n"
         @@ Runtime.main_javascript
              {
                verbose = is_verbose;
                trace = does_trace;
                file = Js.to_string code;
                compile_only = is_compile_only;
              }
    end);
  Runtime.main ()

(*
let () = Runtime.main ()

*)

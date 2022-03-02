(** main.ml *)

open Js_of_ocaml
open Eval
open Runtime
open Generator
open Util
open Compiler
open Runtime.Loader

(** The top level entry point for javascript *)
let main_javascript prop =
  try
    let insts = compile prop.file in
    if prop.compile_only then [ string_of_prog insts ]
    else
      (* VM の機能を使って参照を数値に変換してから pretty print あるいは dump する *)
      let string_of_atoms =
        (if prop.verbose then Dump.string_of_atom_list else Pretty.pretty_print)
        <. dump_atom_list
      in
      let printed_strs_ref = ref [] in
      let print str = printed_strs_ref := str :: !printed_strs_ref in
      let tracer =
        if prop.trace then fun i atoms ->
          print @@ string_of_int i ^ ": " ^ string_of_atoms atoms
        else fun _ _ -> ()
      in
      let print_rule =
        if prop.trace then print <. ( ^ ) "----> " else const ()
      in
      let print_final_state = print in
      run tracer string_of_atoms print_final_state print_rule @@ insts;
      !printed_strs_ref
  with
  | Compiler.CompileError message -> [ message ]
  | Failure message -> [ message ]

let _ =
  Js.export "lmnAlpha"
    (object%js
       method add x y = x +. y

       method run code is_compile_only is_verbose does_trace =
         Js.string @@ String.concat "\n"
         @@ main_javascript
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

(** Loader *)

open Util


     
type prop = {
  verbose: bool;
  trace: bool;
  file: string;
  compile_only: bool;
}



let usage_msg = "append [-t] [-v] <file1>"
let verbose = ref false
let trace = ref false
let input_files = ref []
let compile_only = ref false


let anon_fun filename =
  input_files := filename::!input_files



let speclist =
  [ ("-t", Arg.Set trace, "Trace")
  ; ("-v", Arg.Set verbose, "Output debug information")
  ; ("-c", Arg.Set compile_only, "Compile only")
  ]



(** Loader *)
let load () =
  Arg.parse speclist anon_fun usage_msg;
  (* Main functionality here *)
  match !input_files with
  | [] -> failwith @@ "no input file"
  | (_::_::_) -> failwith @@ "too many files"
  | [file_name] -> (* read_file file_name *)
     {
       verbose = !verbose;
       trace = !trace;
       file = read_file file_name;
       compile_only = !compile_only
     }



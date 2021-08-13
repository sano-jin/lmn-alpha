(** The toplevel of the runtime environment *)

open Compiler
open Eval
open Loader
open Generator
open Util
       


(** Reduce as many as possible.
    Tail recursive (as it should be).
 *) 
let rec run_many tracer print_rule rules i atom_list =
  tracer i atom_list; 
  match Eval.run_once atom_list rules with
  | None -> atom_list
  | Some (rule_name, atom_list) ->
     print_rule rule_name;
     run_many tracer print_rule rules (succ i) atom_list


(** 初期状態を構築した後，最後まで実行して，最終状態を表示する *)
let run tracer string_of_atoms print_rule (init_insts, rules) =
  let initial_atom_list = init_atoms init_insts in
  let final_state = run_many tracer print_rule rules 0 initial_atom_list in
  print_endline @@ "Final state: \n" ^ string_of_atoms final_state



(** The top level entry point *)
let main () =
  let prop = load () in
  let insts = compile prop.file in
  if prop.compile_only then
    print_string @@ string_of_prog insts
  else
    (* VM の機能を使って参照を数値に変換してから pretty print あるいは dump する *)
    let string_of_atoms = (if prop.verbose then Dump.string_of_atom_list else Pretty.pretty_print) <. dump_atom_list in
    let tracer = if prop.trace
		 then fun i atoms -> print_endline @@ string_of_int i ^ ": " ^ string_of_atoms atoms
		 else fun _ _ -> ()
    in
    let print_rule = if prop.show_rules then fun rule_name -> print_endline @@ "----> " ^ rule_name
		     else const ()
    in
    run tracer string_of_atoms print_rule @@ insts



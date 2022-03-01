(** Compiler *)

open Parse
include Corelang
open Analyze
open Generator
open Util

(** Compiles given string and return the generated intermediate code *)
let compile = gen_ic <. sem_graph_of_process <. corelang_of_ast <. parse

(*
let compile souce =
  let insts = compile souce in
  prerr_endline @@ string_of_prog insts;
  insts
 *)

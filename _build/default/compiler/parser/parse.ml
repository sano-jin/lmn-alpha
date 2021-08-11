(** Parse *)

open Util
include Syntax
include Pretty


(** @return AST of defshape *)
let parse = Parser.main Lexer.token <. Lexing.from_string


(** pretty printer for a process *)
let pretty = Pretty.string_of_proc



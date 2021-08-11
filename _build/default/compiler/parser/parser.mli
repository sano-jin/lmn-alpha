
(* The type of tokens. *)

type token = 
  | VBAR
  | RPAREN
  | RBRACKET
  | LinkName of (string)
  | LPAREN
  | LBRACKET
  | EQ
  | EOF
  | DOT
  | COMMA
  | COLMIN
  | AtomName of (string)
  | ATAT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.proc)

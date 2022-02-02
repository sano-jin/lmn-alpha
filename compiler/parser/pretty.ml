(* debug_syntax.ml *)

open Syntax
open Util

(** analyze atom name and obtain associativity and precedence *)
let analyze_atom_name = Parser.atom_name_type Lexer.token <. Lexing.from_string

(** アトムの引数部分の pretty printer *)
let rec string_of_arg precedence =
  let string_of_normal_arg atom_name = function
    | [] -> atom_name
    | args ->
        atom_name ^ "("
        ^ String.concat ", " (List.map (string_of_arg 0) args)
        ^ ")"
  in
  let may_parens prec str =
    if prec < precedence then "(" ^ str ^ ")" else str
  in
  function
  | IntData i -> string_of_int i
  | ProcCtx p -> "$" ^ p
  | Link x -> x
  | Atom ("[]", []) -> "[]"
  | Atom (p, [ x ]) -> (
      match analyze_atom_name p with
      | ANOp _ ->
          p ^ " " ^ may_parens 13 @@ string_of_arg 13 x
          (* unary 演算子の場合は，結合強度に応じて括弧をつける
             - ただし，unary 演算子の強度は最強（13）
          *)
      | ANQuoted ->
          string_of_normal_arg ("'" ^ p ^ "'") [ x ] (* quote が必要なアトム *)
      | ANSymbol -> string_of_normal_arg p [ x ] (* 普通のアトム *))
  | Atom (".", [ h; t ]) -> string_of_list h t
  | Atom (p, [ x; y ]) -> (
      match analyze_atom_name p with
      | ANQuoted ->
          string_of_normal_arg ("'" ^ p ^ "'") [ x; y ] (* quote が必要なアトム *)
      | ANSymbol -> string_of_normal_arg p [ x; y ] (* 普通のアトム *)
      | ANOp (assoc, prec) ->
          let p_x, p_y =
            match assoc with
            | AscLeft -> (prec, succ prec)
            | AscNone -> (succ prec, succ prec)
            | AscRight -> (succ prec, prec)
          in
          may_parens prec
            (string_of_arg p_x x ^ " " ^ p ^ " " ^ string_of_arg p_y y))
  | Atom (p, xs) -> (
      match analyze_atom_name p with
      | ANSymbol -> string_of_normal_arg p xs (* 普通のアトム *)
      | ANOp _ | ANQuoted ->
          string_of_normal_arg ("'" ^ p ^ "'") xs (* quote が必要なアトム *))

(** リストの略記法の pretty printer *)
and string_of_list h t =
  let rec string_of_list_inner = function
    | Atom (".", [ h; t ]) ->
        ", " ^ (string_of_arg 0) h ^ string_of_list_inner t
    | Atom ("[]", []) -> ""
    | arg -> " | " ^ (string_of_arg 0) arg
  in
  "[" ^ string_of_arg 0 h ^ string_of_list_inner t ^ "]"

(** アトムの pretty printer *)
let string_of_atom = function
  | Atom (".", [ x; t; h ]) -> (string_of_arg 5) h ^ " = " ^ string_of_list x t
  | atom -> string_of_arg 0 atom

(** ガードの pretty printer．
    ただし，これはアトムのリストになっているガードのための pretty printer で，
    構文解析直後に得られるガード（プロセスとして構文解析される）のためのものではない．
 *)
let string_of_guard = function
  | [] -> ""
  | guard -> String.concat ", " (List.map string_of_atom guard) ^ " | "

(** プロセスの pretty printer *)
let rec string_of_proc priority = function
  | Zero -> ""
  | Graph atom -> string_of_atom atom
  | Rule (maybe_name, (lhs, guard, rhs)) ->
      let maybe_name = ( ^ ) " @@ " <$> maybe_name in
      let str_of_guard =
        if guard = Zero then "" else string_of_proc 1 guard ^ " | "
      in
      let str_of_rule =
        maybe "" maybe_name ^ string_of_proc 1 lhs ^ " :- " ^ str_of_guard
        ^ string_of_proc 1 rhs
      in
      if priority > 1 then "(" ^ str_of_rule ^ ")" else str_of_rule
  | Mol (p, q) ->
      let str_of_mol i sep = string_of_proc i p ^ sep ^ string_of_proc i q in
      if priority = 0 then str_of_mol 0 ". "
      else if priority > 2 then "(" ^ str_of_mol 2 ", " ^ ")"
      else str_of_mol 2 ", "

let string_of_proc = string_of_proc 0

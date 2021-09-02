(** Guard evaluator *)

open Vm
open Util
open Parse

type guard_value = GIntVal of int | GBoolVal of bool

let guard_value_of_vm_atom = function
  | VMIntData i -> GIntVal i
  | NormalLink _ -> failwith @@ "link cannot be converted to a guard value"

let vm_atom_of_guard_value = function
  | GIntVal i -> VMIntData i
  | GBoolVal _ ->
      failwith @@ "boolean guard value cannot be converted to a vm_atom"

let rec eval_guard_exp env =
  let eval_binop_int op x y =
    match (eval_guard_exp env x, eval_guard_exp env y) with
    | GIntVal x, GIntVal y -> op x y
    | _ -> failwith @@ "Error: unexpected type"
  in
  function
  | Atom ("+", [ x; y ]) -> GIntVal (eval_binop_int ( + ) x y)
  | Atom ("-", [ x; y ]) -> GIntVal (eval_binop_int ( - ) x y)
  | Atom ("*", [ x; y ]) -> GIntVal (eval_binop_int ( * ) x y)
  | Atom ("/", [ x; y ]) -> GIntVal (eval_binop_int ( / ) x y)
  | Atom ("mod", [ x; y ]) -> GIntVal (eval_binop_int ( mod ) x y)
  | Atom ("<", [ x; y ]) -> GBoolVal (eval_binop_int ( < ) x y)
  | Atom ("<=", [ x; y ]) -> GBoolVal (eval_binop_int ( <= ) x y)
  | Atom (">", [ x; y ]) -> GBoolVal (eval_binop_int ( > ) x y)
  | Atom (">=", [ x; y ]) -> GBoolVal (eval_binop_int ( >= ) x y)
  | Atom ("=", [ x; y ]) -> GBoolVal (eval_binop_int ( = ) x y)
  | Atom ("/=", [ x; y ]) -> GBoolVal (eval_binop_int ( <> ) x y)
  | Atom _ as atom ->
      failwith @@ "Error: unexpected operator " ^ Parse.string_of_atom atom
      ^ " in guard"
  | IntData i -> GIntVal i
  | ProcCtx var -> List.assoc var env
  | Link _ -> failwith @@ "Error: Link cannot appear in guard"

let eval_guard_toplevel env = function
  | Atom ("=", [ ProcCtx x; y ]) -> (
      match List.assoc_opt x env with
      | Some x ->
          (* すでに束縛されているので，等価性比較（この式の評価）を行う *)
          if x = eval_guard_exp env y then Some env else None
      | None ->
          (* 新しい変数なので，束縛を行う *)
          Some ((x, eval_guard_exp env y) :: env))
  | atom -> (
      match eval_guard_exp env atom with
      | GBoolVal b -> if b then Some env else None
      | _ ->
          failwith @@ "Error: expected boolean result in " ^ string_of_atom atom
      )

let eval_guard env guard =
  let env = List.map (second guard_value_of_vm_atom) env in
  let+ env = foldM eval_guard_toplevel env guard in
  List.map (second vm_atom_of_guard_value) env

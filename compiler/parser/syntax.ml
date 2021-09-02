(** Syntax *)

(** argument of an atom *)
type arg =
  | Atom of string * arg list  (** atom. e.g. a(X, Y) *)
  | ProcCtx of string  (** unary process context. e.g. $p *)
  | Link of string  (** link. e.g. X *)
  | IntData of int  (** int. e.g. 1 *)

(*
type guard = arg list
(** guard *)
*)

(** process *)
type proc =
  | Zero  (** null. *)
  | Graph of arg  (** atom. e.g. a(Y, b(X)) *)
  | Mol of proc * proc  (** molecule. e.g. (P, Q) *)
  | Rule of string option * (proc * proc * proc)
      (** rule. e.g. P :- G1, ..., Gn | Q. *)

(** Precedence and associativity of atom names *)

(** associativity of operators *)
type atom_name_associativity = AscLeft | AscNone | AscRight

type atom_name_precedence = int
(** precedence of operators
    - >= 1 for an operator atom name
    - 0 for a normal symbol atom name
    - -1 for a quoted atom name
*)

(** Types of atom names *)
type atom_name_type =
  | ANOp of atom_name_associativity * int  (** operator  *)
  | ANSymbol  (** symbol atom *)
  | ANQuoted  (** failed to parse *)
(*  | ANInt  (** integer *) *)

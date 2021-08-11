(** Syntax *)

(** argument of an atom *)
type arg =
  | Atom of string * arg list   (** atom. e.g. a(X, Y) *)
  | Link of string              (** link. e.g. X *)

(** process *)
type proc = 
  | Zero					(** null. *)
  | Graph of arg				(** atom. e.g. a(Y, b(X)) *)
  | Mol of proc * proc				(** molecule. e.g. (P, Q) *)  
  | Rule of string option * (proc * proc)	(** rule. e.g. P :- Q. *)


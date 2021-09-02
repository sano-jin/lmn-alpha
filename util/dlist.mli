(* file: dlist.mli *)

type 'a t

type 'a element

val create : unit -> 'a t
(** Basic list operations  *)

val is_empty : 'a t -> bool

val null_elt : 'a -> 'a element

val map_value : ('a -> 'a) -> 'a element -> 'a element

val first : 'a t -> 'a element option
(** Navigation using [element]s *)

val next : 'a element -> 'a element option

val prev : 'a element -> 'a element option

val value : 'a element -> 'a

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Whole-data-structure iteration *)

val to_list : 'a t -> 'a list

val insert_first : 'a t -> 'a -> 'a element
(** Mutation *)

val remove : 'a t -> 'a element -> unit

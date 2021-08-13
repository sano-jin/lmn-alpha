(* file: dlist.mli *)

type 'a t
type 'a element


(** Basic list operations  *)
val create   : unit -> 'a t
val is_empty : 'a t -> bool
val null_elt : 'a -> 'a element
val map_value : ('a -> 'a) -> 'a element -> 'a element



(** Navigation using [element]s *)
val first : 'a t -> 'a element option
val next  : 'a element -> 'a element option
val prev  : 'a element -> 'a element option
val value : 'a element -> 'a



(** Whole-data-structure iteration *)
val fold    : 'a t -> 'b -> f:('a -> 'b -> 'b) -> 'b
val to_list : 'a t -> 'a list


					   
(** Mutation *)
val insert_first : 'a t -> 'a -> 'a element
val remove : 'a t -> 'a element -> unit

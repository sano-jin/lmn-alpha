(* file: dlist.ml *)

type 'a element =
    { value : 'a;
      mutable next : 'a element option;
      mutable prev : 'a element option
    }

type 'a t = 'a element option ref




(** Basic list operations  *)

let create () = ref None

let is_empty t = Option.is_none !t

(** 無効な初期値 *)
let null_elt a =
  { value = a; next = None; prev = None} 


let value elt = elt.value

let map_value f elt =
  {elt with value = f elt.value}


(** Navigation using [element]s *)

let first t = !t
let next elt = elt.next
let prev elt = elt.prev




(** Whole-data-structure iteration *)

let fold t acc ~f =
  let rec loop acc = function
    | None -> acc
    | Some el -> loop (f (value el) acc) (next el)
  in
  loop acc !t 


let to_list t =
  fold t [] ~f:List.cons
       


(** Mutation *)

let insert_first t value =
  let new_elt = { prev = None; next = !t; value } in
  ( match !t with
    | Some old_first -> old_first.prev <- Some new_elt
    | None -> ()
  );
  t := Some new_elt;
  new_elt


let remove t elt =
  let { prev; next; _ } = elt in
  ( match prev with
    | Some prev -> prev.next <- next
    | None -> t := next
  );
  ( match next with
    | Some next -> next.prev <- prev;
    | None -> ()
  );
  elt.prev <- None;
  elt.next <- None


		

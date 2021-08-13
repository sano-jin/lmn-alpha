(** Virtual machine *)

open Util
       


type vm_atom = string * link array
 and link = NormalLink of int * vm_atom ref  (** (port_i, atom) *)


type register = vm_atom ref
					  

(** レジスタを初期化するためだけのアドレス（本来はいらない） *)
let null_ptr: vm_atom ref = ref ("Null", [||])



(** リンクを初期化するためだけのアドレス（本来はいらない） *)
let null_link = NormalLink (0, ref ("Null", [||]))


		   

(** レジスタ *)
type registers = register array
    


(** 初期状態のレジスタを確保する *)
let init_register size = (Array.make size null_ptr: registers)
			      


(** NormalLink コンストラクタを剥がす *)
let get_normal_link = function
  | NormalLink (port_i, atom_ref) -> (port_i, atom_ref)




			  
(** Free memory fragment of the given address.
    Possibly implemented with [option] type and assign [None].
 *)
let free_atom atom_ref =
  update_ref (first @@ (^) "~") atom_ref



(** アトムリストの Map．
    - それぞれのファンクタ毎のアトムリストの Map になっている．
*)
module AtomLists = Map.Make(struct type t = string * int let compare = compare end)


			   

  

(** アトムリストの dumper *)

(** リンクの解消 *)
let dump_link atom_ref2atom_i = function
  | NormalLink (port_i, atom_ref) -> port_i, List.assq atom_ref atom_ref2atom_i
				     

let dump_atom atom_ref2atom_i atom_i (p, xs) =
  atom_i, (p, List.map (dump_link atom_ref2atom_i) @@ Array.to_list  xs)


let dump_atom_list atom_lists =
  let atom_refs = List.concat_map snd @@ AtomLists.bindings atom_lists in
  List.mapi (dump_atom @@ List.mapi (flip pair) atom_refs)
  @@ List.map (!) atom_refs
								       



								       

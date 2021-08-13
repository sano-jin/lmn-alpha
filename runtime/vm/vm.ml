(** Virtual machine *)

open Util



type vm_atom = string * link array 
 and vm_atom_elt = vm_atom DList.element
 and link = NormalLink of int * vm_atom_elt   (** (port_i, atom) *)



(** レジスタを初期化するためだけのアドレス（本来はいらない） *)
let null_atom_elt: vm_atom_elt = DList.null_elt ("Null", [||])



(** リンクを初期化するためだけのアドレス（本来はいらない） *)
let null_link = NormalLink (0, null_atom_elt)


		   

(** レジスタ *)
type registers = vm_atom_elt array
    


(** 初期状態のレジスタを確保する *)
let init_register size = (Array.make size null_atom_elt: registers)
			      


(** NormalLink コンストラクタを剥がす *)
let retrieve_normal_link = function
  | NormalLink (port_i, atom_ref) -> (port_i, atom_ref)



(** ファンクタを取得する *)				       
let get_functor register reg_i =
  second Array.length @@ DList.value register.(reg_i)
  
				       
				       
(** Free memory fragment of the given address.
    Possibly implemented with [option] type and assign [None].
 *)
let free_atom atom_ref =
  DList.map_value (first @@ (^) "~") atom_ref






(** アトムリストの Map．
    - それぞれのファンクタ毎のアトムリストの Map になっている．
*)
module AtomLists = Map.Make(struct type t = string * int let compare = compare end)


			   
(** アトムリスト *)
let atom_list: vm_atom DList.t = DList.create ()
			   

  

(** アトムリストの dumper *)

(** リンクの解消 *)
let dump_link atom2atom_i = function
  | NormalLink (port_i, atom_elt_ref) ->
     port_i, List.assq (DList.value atom_elt_ref) atom2atom_i
				     
		       
let dump_atom atom2atom_i atom_i (p, xs) =
  atom_i, (p, List.map (dump_link atom2atom_i) @@ Array.to_list xs)


let dump_atom_list atom_lists =
  let atoms =
    List.concat_map (DList.to_list <. snd)
    @@ AtomLists.bindings atom_lists
  in
  List.mapi (dump_atom @@ List.mapi (flip pair) atoms) atoms



								       

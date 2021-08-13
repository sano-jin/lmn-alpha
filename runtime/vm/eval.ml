(** eval.ml *)

open Util
include Vm
open Match
open Pushout


       
(** Try to reduce one step with the given atoms and a rule
    - ルール適用に成功したら Some で包んだ更新された atom_list を返す
 *)
let reduce atom_list (rule_name, (reg_size, (lhs_insts, rhs_insts))) =
  (* レジスタの確保 *)
  let register = init_register reg_size in
  if match_ register atom_list lhs_insts
  then Some (rule_name, pushouts register atom_list rhs_insts)
  else None

	 
	 
(** Try reduce one step with the given atoms and rules *)
let run_once rules = one_of @@ reduce rules



(** push the initial graph and return their references *)			   
let init_atoms (reg_size, rhs_insts) =
  (* レジスタの確保 *)
  let register = init_register reg_size in
  pushouts register AtomLists.empty rhs_insts
	   

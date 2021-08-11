(** Generates intermediate codes from the given semantic graph *)


open Analyze
open Register_table
include Instruction



(** ルールから中間命令列を生成する
    - 戻り値は，生成した中間命令列と必要になるレジスタの数
 *)
let gen_ic_of_rule (ARule (rule_name, (lhs, rhs))) =
  let reg_tbl, lhs_insts =
    Match.match_ lhs in
  
  let reg_size, rhs_insts =
    Pushout.push_atoms reg_tbl rhs in
  (* debug_print "genereted rhs_insts" @@ string_of_rhs_insts rhs_insts;  *)
  (* debug_print "register_size" @@ string_of_int reg_size; *)

  rule_name, (reg_size, (lhs_insts, rhs_insts))
						  


(** ルールセットから中間命令列とレジスタの数のタプルのリストを生成する *)
let gen_ic_of_rules = List.map gen_ic_of_rule



(** 初期状態を生成するための中間命令と必要なレジスタの数を返す *)
let gen_ic_of_init atoms = Pushout.push_atoms empty_reg_tbl @@ (atoms, [])



(** 初期状態とルールを受け取って，中間命令やレジスタの本数を返す *)					
let gen_ic (init_state, rules) =
  gen_ic_of_init init_state, gen_ic_of_rules rules




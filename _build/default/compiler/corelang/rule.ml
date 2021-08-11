(** ルールの解析 *)

open Compile_error
open Syntax
open Link



(** disjunctive_union *)
let sym_diff x y = LinkSet.union (LinkSet.diff x y) (LinkSet.diff y x)



(** ルールの解析を行う
    - 入力は既に解析済みのプロセス
    - サブルールの解析は入力として渡される前に既に行われている
      - 今回はサブルールは認めないことにしているのであまり意味はないが
 *)       
let corelang_of_rule rule_name
		      ((lhs_free_links, (lhs_atoms, lhs_conns)), lhs_rules)
		      ((rhs_free_links, rhs_graph), rhs_rules) =
  if lhs_rules <> [] then raise @@ CompileError "Rule(s) cannot appear on LHS"
  else if rhs_rules <> [] then raise @@ CompileError "Rule(s) on RHS is/are not supported"
  else if lhs_free_links <> rhs_free_links then
    let free_links = sym_diff lhs_free_links rhs_free_links in
    raise @@ CompileError ("Free link(s) " ^ string_of_link_set free_links ^ " of a rule")
  else if lhs_conns <> [] then
    raise @@ CompileError ("Unabsorbable connector(s) " ^ string_of_connectors lhs_conns ^ " on LHS of a rule")
  else			  
    CRule (rule_name, (lhs_atoms, rhs_graph))

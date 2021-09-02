(** パーザによって得られた抽象構文木を解析し，Core language に変換する *)

open Link
include Syntax
include Compile_error

(** アトムのリストの
    - リンク条件をチェックし，自由リンクを取得する 
    - コネクタをできるだけ吸収する
*)
let corelang_of_atoms atoms =
  let ((_, frees) as links) = Link.links_of atoms in
  (frees, Connector.normalize @@ Alpha.alpha_atoms links atoms)

(* プロセスの抽象構文木から Core language へ変換する *)
let rec corelang_of_proc ast =
  let atoms, rules = Partition.partition_proc ast in
  (corelang_of_atoms atoms, List.map corelang_of_rule rules)

and corelang_of_rule (rule_name, (p, guard, q)) =
  Rule.corelang_of_rule rule_name (corelang_of_proc p) guard
    (corelang_of_proc q)

(** 抽象構文木から Core language へ変換する *)
let corelang_of_ast ast =
  let (frees, (atoms, _)), rules = corelang_of_proc ast in
  if not @@ LinkSet.is_empty frees then
    raise
    @@ CompileError
         ("Free link(s) of a program " ^ string_of_link_set frees
        ^ " is not supported")
  else (atoms, rules)

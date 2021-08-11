(** Partition atoms and rules. *)


open Util
open Port
open Corelang
include Sem_graph

	  

(** Core language のグラフを解析して Semantic graph を生成する 
    - 現状アトムに 0 から連続した整数を振ってやって，ポートの位置情報を付加するだけ
*)
let sem_graph_of_atoms atoms =
  let atoms = List.mapi pair atoms in
  local_ports_of_atoms atoms, atoms



(** ルールの解析を行う *)
let sem_graph_of_rule (CRule (rule_name, (lhs_atoms, (rhs_atoms, rhs_conns)))) =
  ARule (rule_name, ( sem_graph_of_atoms lhs_atoms
		    , (sem_graph_of_atoms rhs_atoms, rhs_conns)))

	

(** Core language のグラフとルールセットを解析して Semantic graph を生成する 
    - 今後もしルールがサブルールも持てるようにするのであれば，
      上記のルール解析関数と合わせて相互再帰関数として定義する必要がある
*)
let sem_graph_of_process (atoms, rules) =
  sem_graph_of_atoms atoms
  , List.map sem_graph_of_rule rules



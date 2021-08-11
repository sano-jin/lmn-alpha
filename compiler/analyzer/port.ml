(** 局所リンクが出現したポート番号を収集する *)


open Util
open Sem_graph
open Corelang


(** ポートの引数番号とそのアトムの番号を取得する
    - アトムのリストを前から眺めていって，ポート情報を cons していくので，
      解析後のアトムのリストをまた前から眺めていって，ポート情報を car すると，
      接続先のポートの情報が取得できる
    - [0: a(X), 1: b(_, Y, X), 2: c(Y)] ---> [{ X -> [1/2, 0/0], Y -> [2/0, 1/1] }] 
    - TODO: Queue を使っても良いかも知れない
 *)
       

(** 局所リンクならポートの集合のマップに cons する *)
let add_local_port atom_i port_i = flip @@ function 
  | LocalLink x -> LocalPortMap.update x (Option.some <. List.cons (atom_i, port_i) <. maybe [])
  | FreeLink _ -> id (* 自由リンクなら何もしない *)



(** アトムのリストから局所リンクが出現したポートを収集する *)
let local_ports_of_atoms atoms =
  List.fold_left (uncurry <. flip (fold_lefti <. add_local_port)) LocalPortMap.empty
  @@ List.map (second snd) atoms



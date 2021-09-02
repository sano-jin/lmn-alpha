(** register_table.ml *)

open Instruction

type reg_tbl = {
  matched_functors : (functor_ * reg_i) list;
      (** ルール左辺でマッチしたアトムのファンクタとその参照が格納されているレジスタ番号の連想リスト 
      - アトムの非単射的マッチングを避けるために用いる
   *)
  matched_local_links : int list;
      (** ルール左辺でマッチした局所リンクの集合
      - 局所リンクを複数回マッチする（冗長なマッチを行う）のを防ぐために用いる
   *)
  atom2reg_i : (int * reg_i) list;
      (** マッチしたアトムのアドレスを格納しているレジスタ番号の連想リスト  
      - 局所リンクは必ず参照先のアトムもルール左辺で明示的に与えられるので，これがあれば十分なはず
   *)
  free2reg_i : (string * (reg_i * int)) list;
      (** ルール左辺でマッチした自由リンクを持つアトムが格納されているレジスタ番号とポートの番号の組への連想リスト *)
  free_reg_i : int;  (** まだ使っていないレジスタ番号の最小値（= レジスタの数） *)
}
(** An environment for the matching and pushout 
    - マッチしたレジスタを記録するために，
      簡易的に連想リストを用いているが，マップを使った方が良いかも知れない
 *)

(** 空のレジスタ *)
let empty_reg_tbl =
  {
    matched_functors = [];
    matched_local_links = [];
    atom2reg_i = [];
    free2reg_i = [];
    free_reg_i = 0;
  }

(** まだ使っていないレジスタ番号を取得する *)
let get_free_reg_i reg_tbl =
  (reg_tbl.free_reg_i, { reg_tbl with free_reg_i = succ reg_tbl.free_reg_i })

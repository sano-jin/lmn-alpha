(** pushatom.ml *)

open Analyze
open Corelang
open Util
open Register_table
open Instruction

let functor_of = second List.length

(** アトムを生成する命令を生成する *)
let push_atom_of reg_tbl (x, p_xs) =
  let reg_i, reg_tbl = get_free_reg_i reg_tbl in
  (reg_tbl, ((x, reg_i), PushAtom (reg_i, functor_of p_xs)))

(** アトムを生成する命令のリストを生成する 
   - 戻り値は [reg_tbl, (local2reg_i, push_atoms)]
     - ただし，[local2reg_i] は局所リンク名とレジスタ番号の連想リスト
     - [push_atoms] は PushAtom 命令のリスト
 *)
let push_atoms_of = second List.split <.. List.fold_left_map push_atom_of

(** リンクが参照するアドレスが格納されたレジスタ番号を取得する 
    - [free2reg_i] はマッチング終了後の [reg_tbl.free2reg_i]
    - [local2reg_i] は [push_local_atoms] によって得られた連想リスト
      - [reg_tbl.local2reg_i] ではない
*)
let set_link_of_link reg_tbl atom2reg_i src_reg_i src_port_i
    (local_port_map : local_port_map) = function
  | CIntData _ -> failwith @@ "Error: data atom on RHS is not supported yet"
  | CProcCtx var -> (local_port_map, SetData (src_reg_i, src_port_i, var))
  | FreeLink x ->
      let lhs_reg_i, lhs_port_i = List.assoc x reg_tbl.free2reg_i in
      (local_port_map, ReLink (src_reg_i, src_port_i, lhs_reg_i, lhs_port_i))
  | LocalLink x ->
      let dst_atom_i, dst_port_i =
        List.hd @@ LocalPortMap.find x local_port_map
      in

      (* 局所リンクの参照先を獲得する．
         hyper でない lmntal では局所リンクは 2 回出現するので必ず相手が取れるが，
         hyper へ拡張するなら，リストの最終要素以外のリストという風にする必要がある *)
      let dst_reg_i = List.assoc dst_atom_i atom2reg_i in

      (* 局所リンクに対応するポート情報のリストを回転させる *)
      let local_port_map =
        LocalPortMap.update x (Option.map roll) local_port_map
      in
      (local_port_map, SetLink (src_reg_i, src_port_i, dst_reg_i, dst_port_i))

(** 局所リンク・自由リンクに参照される（シンボル）アトムのリンクをセットする命令のリストを生成する 
    - [link2reg_i] は局所リンクに参照されるアトムを生成する場合は [local2reg_i]，
      自由リンクに参照されるアトムを生成する場合は [free2reg_i]
*)
let set_links_of_atom reg_tbl atom2reg_i (local_port_map : local_port_map)
    (atom_i, (_, xs)) =
  let src_reg_i = List.assoc atom_i atom2reg_i in
  (fold_left_mapi
     (set_link_of_link reg_tbl atom2reg_i src_reg_i)
     local_port_map xs
    : local_port_map * rhs_inst list)

(* link2reg_i は（恐らくは）単相性制限のために eta 変換できなかった *)
let set_links_of_atoms reg_tbl atom2reg_i local_port_map atoms =
  second List.concat
  @@ List.fold_left_map
       (set_links_of_atom reg_tbl atom2reg_i)
       local_port_map atoms

(** ルール左辺のアトムを消去する命令を生成する *)
let free_atom_of reg_i = FreeAtom reg_i

(** ルール左辺のアトムを消去する命令のリストを生成する *)
let free_atoms_of = List.map free_atom_of

(** 自由リンク同士の再接続を行う命令を生成する *)
let connect_of free2reg_i (x, y) =
  let reg_i, port_i = List.assoc x free2reg_i in
  let reg_j, port_j = List.assoc y free2reg_i in
  Connect (reg_i, port_i, reg_j, port_j)

(** リダイレクトを行う命令のリストを生成する *)
let connects_of = List.map <. connect_of

(** 確保すべきレジスタのサイズと生成した命令列を返す *)
let push_atoms reg_tbl ((local_port_map, atoms), connectors) =
  (* ルール右辺のアトムの生成 *)
  let reg_tbl, (atom2reg_i, push_atoms) = push_atoms_of reg_tbl atoms in

  (* ルール右辺の少なくとも片方が局所リンクとなっているリンクの接続を行う *)
  let _, set_or_re_links =
    set_links_of_atoms reg_tbl atom2reg_i local_port_map atoms
  in

  (* ルール右辺の引数がどちらも自由リンクとなっているコネクタの接続を行う *)
  let connects = connects_of reg_tbl.free2reg_i connectors in

  (* ルール左辺でマッチしたアトムを解放 *)
  let free_atoms = free_atoms_of @@ List.map snd reg_tbl.atom2reg_i in

  ( reg_tbl.free_reg_i (* 確保すべきレジスタのサイズ *),
    List.concat [ push_atoms; set_or_re_links; connects; free_atoms ] )

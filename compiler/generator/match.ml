(** match.ml *)

open Analyze
open Corelang
open Util
open Register_table
open Instruction


       
(** アトムの引数のリンクのマッチングを行う命令を生成する *)
let check_arg src_reg_i (local_port_map, reg_tbl) src_port_i =
  function
  | FreeLink x -> (* 自由リンクはマッチングに関しては特に何も行わない．後でルール右辺できちんと接続するための情報を取得しておくだけ *)
    (local_port_map, {reg_tbl with free2reg_i = (x, (src_reg_i, src_port_i))::reg_tbl.free2reg_i}), [] 
  | LocalLink x ->
     let dst_reg_i, reg_tbl = get_free_reg_i reg_tbl in  (* リンクの Dereference を行うためのレジスタ番号を確保する  *)
     let dst_atom_i, dst_port_i = List.hd @@ LocalPortMap.find x local_port_map in
     (* 局所リンクの参照先を獲得する．
	hyper でない lmntal では局所リンクは 2 回出現するので必ず相手が取れるが，
	hyper へ拡張するなら，リストの最終要素以外のリストという風にする必要がある *)
     let local_port_map = LocalPortMap.update x (Option.map roll) local_port_map in (* 局所リンクに対応するポート情報のリストを回転させる *)
     first (pair local_port_map) @@
       if List.mem x reg_tbl.matched_local_links then reg_tbl, [] (* すでにマッチングが完了した局所リンクの場合は何もしない *)
								 (* lmntal の局所リンクは常に相互につながっているため，相手がたをもう一度確認する必要はないため *)
       else
	 let reg_tbl = {reg_tbl with matched_local_links = x::reg_tbl.matched_local_links} in
	 let deref_atom = DerefAtom (dst_reg_i, src_reg_i, src_port_i, dst_port_i) in  (* 局所リンクの場合は [DerefAtom] 命令を用いる *)
	 match List.assoc_opt dst_atom_i reg_tbl.atom2reg_i with 
	 | None -> {reg_tbl with atom2reg_i = (dst_atom_i, dst_reg_i)::reg_tbl.atom2reg_i}, [deref_atom]
	 | Some reg_i -> reg_tbl, [deref_atom; CheckRefEq (dst_reg_i, reg_i)]
				    



(** リンクの非単射的マッチングを行うための命令 *)       
let check_ref_neq_of reg_i reg_j = CheckRefNeq (reg_i, reg_j)



(** アトムのマッチングを行う命令を生成する *)
let check_atom (local_port_map, reg_tbl) reg_i (p, xs) =
  let functor_ = (p, List.length xs) in
  let check_ref_neqs =
      let matched_atoms = List.filter ((=) functor_ <. fst) reg_tbl.matched_functors in
      List.map (check_ref_neq_of reg_i <. snd) matched_atoms
  in
  let reg_tbl = {reg_tbl with matched_functors = (functor_, reg_i)::reg_tbl.matched_functors} in
  let xs = List.mapi pair xs in
  let env, check_args = List.fold_left_map (uncurry <. check_arg reg_i) (local_port_map, reg_tbl) xs in
  env, List.concat @@ check_ref_neqs::check_args
		      


let functor_of_atom (p, xs) = (p, List.length xs)



(** アトムをアトムリストから適当に peak するか，すでに格納済みでレジスタから dereference するかを決める
    - reg_tbl を受け取って，更新した reg_tbl と 生成した中間命令列を返す *)
let find_atom (local_port_map, reg_tbl as env) (atom_i, atom) =
  match List.assoc_opt atom_i reg_tbl.atom2reg_i with
  | None -> (* Could not dereference. has not matched yet. *)
     (* アトムリストの先頭から随時アトムへの参照をレジスタ reg_i に格納する命令を発行する *)
     let reg_i, reg_tbl = get_free_reg_i reg_tbl in
     let reg_tbl = {reg_tbl with atom2reg_i = (atom_i, reg_i)::reg_tbl.atom2reg_i} in
     let peak_atom = PeakAtom (reg_i, functor_of_atom atom) in
     let env, insts = check_atom (local_port_map, reg_tbl) reg_i atom in
     env, peak_atom::insts

  | Some reg_i ->
     (* レジスタにすでに格納されているアトムへの参照の場合 *)
     let check_functor = CheckFunctor (reg_i, functor_of_atom atom) in
     let env, insts = check_atom env reg_i atom in	
     env, check_functor::insts 




(** 引数に (local_port_map, reg_tbl) atoms をとる *)
let find_atoms = second List.concat <.. List.fold_left_map find_atom




(** ルール左辺を中間命令列に変換する
    局所リンクのポート情報とアトムのリストを受け取って変換する
    @param local_port_map 局所リンクのポート情報
    @return 生成したレジスタ割り当てと中間命令列のタプル
 *)
let match_ ((local_port_map: local_port_map), (atoms: a_atom list)) =
  first snd @@ find_atoms (local_port_map, empty_reg_tbl) atoms
			

  
  

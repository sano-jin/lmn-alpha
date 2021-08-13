(** pushatom.ml *)

open Generator
open Util
open Vm


(** アトムリストにアトムを追加する *)
let push_atom functor_ new_atom atom_lists =
  match AtomLists.find_opt functor_ atom_lists with 
  | None ->
     let atom_list = DList.create () in
     let new_atom_elt =  (* atom_list に破壊的に挿入を行う *)
       DList.insert_first atom_list new_atom in
     new_atom_elt, AtomLists.add functor_ atom_list atom_lists
  | Some atom_list ->
     DList.insert_first atom_list new_atom, atom_lists
     
      


(** アトムリストからアトムを削除する *)
let remove_atom functor_ atom_elem atom_lists =
  (* このアトムはアトムリストに必ず存在するはずなので，find_opt でなく，find で必ず取れるはず *)
  let atom_list = 
    AtomLists.find functor_ atom_lists in
  DList.remove atom_list atom_elem;
  if DList.is_empty atom_list then
    AtomLists.remove functor_ atom_lists
  else atom_lists

	 

	 
(** ルール左辺でマッチしたアトムを代入したレジスタからデータを取得する 
    @return ルール左辺でマッチしたアトムが繋がっていたアトムのデータ
*)
let deref_lhs_register register reg_j port_j =
  (* ルール左辺でマッチしたアトム *)
  let (_, xs) = DList.value register.(reg_j) in
  (* ルール左辺でマッチしたアトムのリンク *)
  let link_obj_j = xs.(port_j) in
  (* ルール左辺でマッチしたアトムが元々指していたアトム *)
  let (port_j, atom_ref_j) = retrieve_normal_link link_obj_j in
    snd @@ DList.value atom_ref_j, port_j, atom_ref_j

					     
					 

(** ルール右辺の命令を一回実行して，更新したアトムリストを返す *)
let pushout register atom_lists = function
  | PushAtom (reg_i, functor_) ->
     (* ルール右辺で生成する入次数 [indeg]，ファンクタ [functor_] のシンボルアトムを生成し，
	レジスタ [reg_i] に代入して，アトムリストへ追加する
      *)
     let (p, arity) = functor_ in
     let new_atom = (p, Array.make arity null_link) in
     let new_atom_elt, atom_lists =
       push_atom functor_ new_atom atom_lists in
     register.(reg_i) <- new_atom_elt;
     atom_lists
		   
  | FreeAtom reg_i -> 
     (* レジスタ [reg_i] が参照する先のアトムをアトムリストから除去し，メモリを解放する *)
     let atom_ref = register.(reg_i) in
     let functor_ = get_functor register reg_i in
     let atom_lists = remove_atom functor_ atom_ref atom_lists in
     ignore @@ free_atom atom_ref; (* メモリ解放もどき *)
     atom_lists
       
  | SetLink (reg_i, port_i, reg_j, port_j) ->
  (* レジスタ [reg_i] が参照するアトムの [port_i] 番目のリンクと
     レジスタ [reg_j] が参照するアトムの [port_j] 番目のリンクを結ぶ
   *)
     let atom_ref_i = register.(reg_i) in
     let (_, xs) = DList.value atom_ref_i in

     let atom_ref_j = register.(reg_j) in
     let (_, ys) = DList.value atom_ref_j in

     xs.(port_i) <- NormalLink (port_j, atom_ref_j);
     ys.(port_j) <- NormalLink (port_i, atom_ref_i);
     atom_lists
			       
  | ReLink (reg_i, port_i, reg_j, port_j) ->
  (* レジスタ [reg_i] が参照するルール右辺で生成したアトムの [port_i] 番目のリンクと
     レジスタ [reg_j] が参照するルール左辺でマッチしたアトムの [port_j] 番目のリンクを繋ぐ
   *)
     let ys, port_j, atom_ref_j =
       deref_lhs_register register reg_j port_j in

     let atom_ref_i = register.(reg_i) in
     let (_, xs) = DList.value atom_ref_i in

     xs.(port_i) <- NormalLink (port_j, atom_ref_j);
     ys.(port_j) <- NormalLink (port_i, atom_ref_i);
     atom_lists
			       
  | Connect (reg_i, port_i, reg_j, port_j) ->
  (* レジスタ [reg_i] が参照するルール左辺でマッチしたアトムの [port_i] 番目のリンクと
     レジスタ [reg_j] が参照するルール左辺でマッチしたアトムの [port_j] 番目のリンクとを結ぶ
   *)
     let xs, port_i, atom_ref_i =
       deref_lhs_register register reg_i port_i in

     let ys, port_j, atom_ref_j =
       deref_lhs_register register reg_j port_j in

     xs.(port_i) <- NormalLink (port_j, atom_ref_j);
     ys.(port_j) <- NormalLink (port_i, atom_ref_i);
     atom_lists
			       
  | FailPushout message -> failwith message
     (* 仮想マシンを（途中で）強制終了する．デバッグのための命令 *)


(** ルール右辺の命令を実行する
    - アトムリストを受け取り，更新したアトムリストを返す
 *)
let pushouts = List.fold_left <. pushout

				   

(** pushatom.ml *)

open Generator
open Util
open Vm



(** ルール右辺の命令を一回実行して，更新したアトムリストを返す *)
let pushout register atom_list =
  (* atom_ref をアトムリストから除去する（メモリ解放はしない） *)
  let remove atom_ref = free_atom atom_ref; List.filter ((!=) atom_ref) atom_list in

  function
  | PushAtom (reg_i, functor_) ->
     (* ルール右辺で生成する入次数 [indeg]，ファンクタ [functor_] のシンボルアトムを生成し，
	レジスタ [reg_i] に代入して，アトムリストへ追加する
      *)
     let (p, arity) = functor_ in
     let atom_ref = ref (p, Array.make arity null_link) in
     register.(reg_i) <- atom_ref;
     atom_ref::atom_list
     
  | FreeAtom reg_i -> 
     (* レジスタ [reg_i] が参照する先のアトムをアトムリストから除去し，メモリを解放する *)
     let atom_ref = register.(reg_i) in
     (* アトムリストからこの参照を除去 *)
     let atom_list = remove atom_ref in 
     free_atom atom_ref; (* メモリ解放 *)
     atom_list
       
  | SetLink (reg_i, port_i, reg_j, port_j) ->
  (* レジスタ [reg_i] が参照するアトムの [port_i] 番目のリンクと
     レジスタ [reg_j] が参照するアトムの [port_j] 番目のリンクを結ぶ
   *)
     let atom_ref_i = register.(reg_i) in
     let atom_ref_j = register.(reg_j) in
     let (_, xs) = !atom_ref_i in
     let (_, ys) = !atom_ref_j in
     xs.(port_i) <- NormalLink (port_j, atom_ref_j);
     ys.(port_j) <- NormalLink (port_i, atom_ref_i);
     atom_list
			       
  | ReLink (reg_i, port_i, reg_j, port_j) ->
  (* レジスタ [reg_i] が参照するルール右辺で生成したアトムの [port_i] 番目のリンクと
     レジスタ [reg_j] が参照するルール左辺でマッチしたアトムの [port_j] 番目のリンクを繋ぐ
   *)
     let atom_ref_i = register.(reg_i) in
     let atom_ref_j = register.(reg_j) in
     let link_obj_j = (snd !atom_ref_j).(port_j) in
     let (_, xs) = !atom_ref_i in
     let (port_j, atom_ref_j) = get_normal_link link_obj_j in
     let (_, ys) = !atom_ref_j in
     xs.(port_i) <- NormalLink (port_j, atom_ref_j);
     ys.(port_j) <- NormalLink (port_i, atom_ref_i);
     atom_list
			       
  | Connect (reg_i, port_i, reg_j, port_j) ->
  (* レジスタ [reg_i] が参照するルール左辺でマッチしたアトムの [port_i] 番目のリンクと
     レジスタ [reg_j] が参照するルール左辺でマッチしたアトムの [port_j] 番目のリンクとを結ぶ
   *)
     let atom_ref_i = register.(reg_i) in
     let link_obj_i = (snd !atom_ref_i).(port_i) in
     let (port_i, atom_ref_i) = get_normal_link link_obj_i in
     let (_, xs) = !atom_ref_i in
     
     let atom_ref_j = register.(reg_j) in
     let link_obj_j = (snd !atom_ref_j).(port_j) in
     let (port_j, atom_ref_j) = get_normal_link link_obj_j in
     let (_, ys) = !atom_ref_j in

     xs.(port_i) <- NormalLink (port_j, atom_ref_j);
     ys.(port_j) <- NormalLink (port_i, atom_ref_i);
     atom_list
			       
  | FailPushout message -> failwith message
     (* 仮想マシンを（途中で）強制終了する．デバッグのための命令 *)


(** ルール右辺の命令を実行する
    - アトムリストを受け取り，更新したアトムリストを返す
 *)
let pushouts = List.fold_left <. pushout

				   

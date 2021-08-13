(** match.ml  *)

open Generator
open Vm



let check_port port_i = function
  | NormalLink (port_j, _) -> port_i = port_j
       

(** 巻き戻しの起点にならない命令を一回実行する *)
let exec_unrewindable_inst register = function
  | PeakAtom (_, _) -> failwith "Bug: PeakAtom is a rewindable instruction"
  | CheckFunctor (reg_i, functor_) -> 
     (* reg_i に格納したシンボルアトムのファンクタが functor_ であることを確認する *)
     let (p, xs) = (!) register.(reg_i) in
     (p, Array.length xs) = functor_
			      
		       
  | DerefAtom (dst_reg_i, src_reg_i, src_port_i, dst_port_i) ->
     (* レジスタ [src_reg_i] が参照するアトムの [src_port_i] 番目の引数の持つアトムへの参照を
	このリンクが相手先で [dst_port_i] 番目の引数に接続されていることを確認して，レジスタ [dst_reg_i] に格納する
      *)
     let (_, xs) = (!) register.(src_reg_i) in
     let link_obj = xs.(src_port_i) in (
       match link_obj with
       | NormalLink (port_i, atom_ref) ->
	  if dst_port_i = port_i then (
	    register.(dst_reg_i) <- atom_ref;
	    true	      
	  ) else (
	    false
	  )
     )
					 
  | CheckRefEq (reg_i, reg_j) ->
     (* レジスタ [reg_i] に格納されているアドレスと
	レジスタ [reg_j] に格納されているアドレスが等しいことを確認する
      *)
     register.(reg_i) == register.(reg_j)
				    
  | CheckRefNeq (reg_i, reg_j) ->
     (* レジスタ [reg_i] に格納されているアドレスと
	レジスタ [reg_j] に格納されているアドレスが異なることを確認する
      *)
     register.(reg_i) != register.(reg_j)

  | FailMatching message -> failwith message
     (* 仮想マシンを（途中で）強制終了する．デバッグのための命令 *)



(** ルール左辺のマッチング実行のトップレベル *)
let match_ register atom_lists =
  (* ルール左辺のマッチングを行う．
     最終引数は，まだ試していない残りの命令列
     @param atom_lists ファンクタごとにあるアトムリストのリスト（になる予定）
   *)
  let rec find_atom = function
    | [] -> true  (* successfully matched *)

    (* アトムリストの先頭から随時，ファンクタが functor_ であるアトムへの参照を，レジスタ reg_i に格納してゆく
       - アトムリストのリストから，ファンクタで問い合わせてそのファンクタのアトムリストを取得する
     *)
    | PeakAtom (reg_i, functor_) ::rest_insts -> 
       ( match AtomLists.find_opt functor_ atom_lists with
	 | None -> false   (* そのファンクタのアトムリストが存在しなかった *)
	 | Some atom_list -> (* アトムリストを取得できた *)
	    peak_atom (reg_i, functor_) rest_insts atom_list
       )
    | inst::rest_insts ->
       exec_unrewindable_inst register inst
       && find_atom rest_insts
		    
  (* 巻き戻しの起点となる命令を実行する
     - 成功すれば true，失敗すれば false を返す
     - アトムリストをファンクタで分類されているため，ファンクタが異なるため失敗すると言うことはない
   *)
  and peak_atom (reg_i, functor_) rest_insts = function
    | [] -> false  (* もうアトムリストにアトムが残っていない *)
    | atom_ref::t ->
       register.(reg_i) <- atom_ref;
       find_atom rest_insts (* 残りの命令も試す *)
       || peak_atom (reg_i, functor_) rest_insts t (* peak したけどその後のマッチングで失敗したのでアトムリストの tail を試す *)
  in find_atom
	      
	    

(** match.ml  *)

open Generator
open Vm
open Util

(** 巻き戻しの起点にならない，プロセス文脈に関係しない，マッチング命令を一回実行する *)
let exec_unrewindable_inst register = function
  | PeakAtom _ -> failwith "Bug: PeakAtom is a rewindable instruction"
  | BindData _ -> failwith "Bug: BindData is not a matching instruction"
  | CheckFunctor (reg_i, functor_) ->
      (* reg_i に格納したシンボルアトムのファンクタが functor_ であることを確認する *)
      get_functor register reg_i = functor_
  | DerefAtom (dst_reg_i, src_reg_i, src_port_i, dst_port_i) -> (
      (* レジスタ [src_reg_i] が参照するアトムの [src_port_i] 番目の引数の持つアトムへの参照を
         このリンクが相手先で [dst_port_i] 番目の引数に接続されていることを確認して，レジスタ [dst_reg_i] に格納する
      *)
      let _, xs = DList.value register.(src_reg_i) in
      let link_obj = xs.(src_port_i) in
      match link_obj with
      | VMIntData _ -> false
      | NormalLink (port_i, atom_ref) ->
          if dst_port_i = port_i then (
            register.(dst_reg_i) <- atom_ref;
            true)
          else false)
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

(** ルール左辺のマッチング実行のトップレベル
     @param atom_lists ファンクタごとにあるアトムリストのリスト
 *)
let match_ register atom_lists =
  (* ルール左辺のマッチングを行う．
     最終引数は，まだ試していない残りの命令列
     @param env プロセス文脈の環境
  *)
  let rec find_atom env = function
    | [] -> (true, env) (* successfully matched *)
    (* アトムリストの先頭から随時，ファンクタが functor_ であるアトムへの参照を，レジスタ reg_i に格納してゆく
       - アトムリストのリストから，ファンクタで問い合わせてそのファンクタのアトムリストを取得する
    *)
    | PeakAtom (reg_i, functor_) :: rest_insts -> (
        match AtomLists.find_opt functor_ atom_lists with
        | None -> (false, env) (* そのファンクタのアトムリストが存在しなかった *)
        | Some atom_list ->
            (* アトムリストを取得できた *)
            peak_atom (reg_i, functor_) env rest_insts @@ DList.first atom_list)
    | BindData (var, src_reg_i, src_port_i) :: rest_insts ->
        let _, xs = DList.value register.(src_reg_i) in
        let env = (var, xs.(src_port_i)) :: env in
        find_atom env rest_insts
    | inst :: rest_insts ->
        if exec_unrewindable_inst register inst then find_atom env rest_insts
        else (false, env)
  (* 巻き戻しの起点となる命令を実行する
     - 成功すれば true，失敗すれば false を返す
     - アトムリストをファンクタで分類されているため，ファンクタが異なるため失敗すると言うことはない
  *)
  and peak_atom (reg_i, functor_) env rest_insts = function
    | None -> (false, env) (* もうアトムリストにアトムが残っていない *)
    | Some elt ->
        register.(reg_i) <- elt;
        let is_matched, env' = find_atom env rest_insts in
        (* 残りの命令も試す *)
        if is_matched then (true, env')
        else
          (* peak したけどその後のマッチングで失敗したのでアトムリストの tail を試す *)
          peak_atom (reg_i, functor_) env rest_insts @@ DList.next elt
  in
  find_atom []

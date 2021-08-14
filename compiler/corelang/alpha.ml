(** 局所リンクに一意な id を振り，アトムを分解する *)

open Util
open Parse
open Syntax
open Link


(** アルファ変換とアトムの分解
    @param locals ユーザが明示的に書いた局所リンク名のリスト
    @param frees  自由リンク名のセット
    @param link_id 局所リンクに割り当てる用の id
    @return (更新された局所リンクに割り当てる用の id, (アルファ変換されたリンク名, 分解されたアトムのリスト))
 *)		
let rec alpha_arg (locals, frees) link_id = function
  | Link x -> if LinkSet.mem x frees then link_id, (FreeLink x, [])
	      else (
		match index_of x locals with
		| None   -> succ link_id, (LocalLink link_id, [])
		| Some x -> link_id, (LocalLink x, [])
	      )
  | Atom (p, xs) ->
     let link_id, (xs, atoms) = fold_left_map2 (alpha_arg (locals, frees)) link_id xs in
     succ link_id, (LocalLink link_id, (p, xs @ [LocalLink link_id])::atoms)
     

		     
(** アルファ変換とアトムの分解
    @param link_names ユーザが明示的に書いた局所・自由リンク名それぞれのマップオブジェクト
    @param link_id 局所リンクに割り当てる用の id
    @return (更新された局所リンクに割り当てる用の id, 分解されたアトムのリスト)
 *)		
let alpha_atom link_names link_id = function
  | Link _ -> failwith @@ "Bug: link appeared on top-level"
  | Atom (p, xs) ->
     let link_id, (xs, atoms) = fold_left_map2 (alpha_arg link_names) link_id xs in
     link_id, (p, xs)::atoms

			 

(** アルファ変換とアトムの分解
    - アトムを引数部分に埋め込む略記法に対して割り当てるリンク ID は，
    ユーザが直接書いた局所リンクの数以上の連続した整数となる．
    このために [List.length locals] がある．
    @param link_names ユーザが明示的に書いた局所・自由リンク名それぞれのマップオブジェクト
    @return 分解されたアトムのリスト
 *)
let alpha_atoms (locals, _ as link_names) =
  List.concat <. snd <. List.fold_left_map (alpha_atom link_names) @@ List.length locals






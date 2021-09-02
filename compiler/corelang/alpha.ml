(** 局所リンクに一意な id を振り，アトムを分解する *)

open Util
open Parse
open Syntax
open Link

(** アルファ変換とアトムの分解
    - アトムの引数部分を処理する
    @param locals ユーザが明示的に書いた局所リンク名のリスト
    @param frees  自由リンク名のセット
    @param link_id 局所リンクに割り当てる用の id
    @return (更新された局所リンクに割り当てる用の id, (アルファ変換されたリンク名, 分解されたアトムのリスト))
 *)
let rec alpha_arg (locals, frees) link_id = function
  | Link x -> (
      if LinkSet.mem x frees then (link_id, (FreeLink x, []))
      else
        match index_of x locals with
        | None -> (succ link_id, (LocalLink link_id, []))
        | Some x -> (link_id, (LocalLink x, [])))
  | Atom (p, xs) ->
      let link_id, (xs, atoms) =
        fold_left_map2 (alpha_arg (locals, frees)) link_id xs
      in
      ( succ link_id,
        (LocalLink link_id, (p, xs @ [ LocalLink link_id ]) :: atoms) )
  | ProcCtx var -> (link_id, (CProcCtx var, []))
  | IntData i -> (link_id, (CIntData i, []))

(** アルファ変換とアトムの分解
    - トップレベル（抽象構文木の根）のアトムを処理する
    @param link_names ユーザが明示的に書いた局所・自由リンク名それぞれのマップオブジェクト
    @param link_id 局所リンクに割り当てる用の id
    @return (更新された局所リンクに割り当てる用の id, 分解されたアトムのリスト)
 *)
let alpha_atom link_names link_id = function
  | Link _ ->
      failwith @@ "Bug: link appeared on top-level"
      (* トップレベルにリンクのみ出現するのは，構文解析の段階で弾かれるはずなので，この場合はコンパイルエラーではなく，バグ *)
  | ProcCtx _ ->
      failwith @@ "Error: process context on top-level is not supported yet"
      (* プロセス文脈はアトムに埋め込まれて記述されるものしかまだ対応していない *)
  | IntData _ ->
      failwith @@ "Error: data atom on top-level is not supported yet"
      (* データアトムはアトムに埋め込まれて記述されるものしかまだ対応していない *)
  | Atom (p, xs) ->
      let link_id, (xs, atoms) =
        fold_left_map2 (alpha_arg link_names) link_id xs
      in
      (link_id, (p, xs) :: atoms)

(** アルファ変換とアトムの分解
    - アトムを引数部分に埋め込む略記法に対して割り当てるリンク ID は，
      ユーザが直接書いた局所リンクの数 [List.length locals] 以上の連続した整数となる．
    @param link_names ユーザが明示的に書いた局所・自由リンク名それぞれのマップオブジェクト
    @return 分解されたアトムのリスト
 *)
let alpha_atoms ((locals, _) as link_names) =
  List.concat <. snd
  <. List.fold_left_map (alpha_atom link_names) @@ List.length locals

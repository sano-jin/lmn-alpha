(** Analyze guard *)

open Util
open Port
open Corelang
include Sem_graph

(** collect process contexts *)
let process_contexts_of_link = function
  | LocalLink _ -> None
  | FreeLink _ -> None
  | CProcCtx p -> Some p
  | CIntData _ -> None

(** initial environment *)
let process_contexts_of_atoms atoms =
  List.map (List.filter_map process_contexts_of_link) atoms

(** 左辺のプロセス文脈に同じ名前が出現しないことをチェックする *)
let check_dup process_contexts =
  match get_dup_opt @@ List.map fst process_contexts with
  | None -> ()
  | Some var ->
      raise
      @@ CompileError ("Process context " ^ var ^ " appeared more then once")

(** プロセス文脈の型 *)
type pctx_type =
  | TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyUnary
  | TyGround
  | TyUntyped

(** プロセス文脈の型を unification する *)
let unify_pctx_type ty_x ty_y =
  match (ty_x, ty_y) with
  | TyUntyped, ty_y -> Some ty_y
  | ty_x, TyUntyped -> Some ty_x
  | TyGround, ty_y -> Some ty_y
  | ty_x, TyGround -> Some ty_x
  | TyUnary, ty_y -> Some ty_y
  | ty_x, TyUnary -> Some ty_x
  | TyBool, TyBool -> Some TyBool
  | TyInt, TyInt -> Some TyInt
  | TyFloat, TyFloat -> Some TyFloat
  | TyString, TyString -> Some TyString
  | _ -> None
(* unification failed *)

(** Core language のグラフを解析して Semantic graph を生成する 
    - 現状アトムに 0 から連続した整数を振ってやって，ポートの位置情報を付加するだけ
*)
let sem_graph_of_atoms atoms =
  let atoms = List.mapi pair atoms in
  (local_ports_of_atoms atoms, atoms)

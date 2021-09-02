(** Analyze guard *)

(*
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

(** プロセス文脈に同じ名前が出現しないことをチェックする *)
let check_dup process_contexts =
  match get_dup_opt @@ List.map fst process_contexts with
  | None -> ()
  | Some var ->
      raise
      @@ CompileError ("Process context " ^ var ^ " appeared more then once")

type pctx_type =
  | IntType
  | FloatType
  | StringType
  | BoolType
  | GroundType
  | UnaryType

(** プロセス文脈の型の包含関係を与える *)
let compare_pctx_type pctx_type_x pctx_type_y =
  match pctx_type_x, pctx_type_y with
  | 
      
(** Core language のグラフを解析して Semantic graph を生成する 
    - 現状アトムに 0 から連続した整数を振ってやって，ポートの位置情報を付加するだけ
*)
let sem_graph_of_atoms atoms =
  let atoms = List.mapi pair atoms in
  (local_ports_of_atoms atoms, atoms)
*)

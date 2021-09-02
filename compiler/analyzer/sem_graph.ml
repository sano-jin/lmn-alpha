(** Semantic graph の型やその dumper など *)

open Util
open Corelang

(** ポート情報は [(アトムの位置, ポートの引数番号)] の組で表す *)

module LocalPortMap = Map.Make (Int)
(** 局所リンクのポートの収集のためのマップモジュール
    - [link_id -> (atom_i * port_i) list]
 *)

type local_port_map = (int * int) list LocalPortMap.t
(** アトムの位置とポートの引数番号の組のリストのマップ *)

(** ポート情報の dumper *)
let string_of_port (atom_i, port_i) =
  string_of_int atom_i ^ "/" ^ string_of_int port_i

(** local_port_map の binding の dumper *)
let string_of_local_port_binding (key, ports) =
  string_of_int key ^ " -> {"
  ^ String.concat ", " (List.map string_of_port ports)
  ^ "}"

(** local_port_map の dumper *)
let string_of_local_port_map =
  String.concat ", "
  <. List.map string_of_local_port_binding
  <. LocalPortMap.bindings

(** 解析後に得られる Semantic graph の型 *)

type a_atom = int * c_atom
(** アトムには一意な id を割り当てる *)

type a_graph = local_port_map * a_atom list
(** アトムのリストには局所リンクのポートの情報も付加する *)

type a_rule =
  | ARule of string * (a_graph * Parse.arg list * (a_graph * c_conn list))

(** dumper *)

let string_of_graph (ports, atoms) =
  "{"
  ^ string_of_local_port_map ports
  ^ "} "
  ^ string_of_atoms (List.map snd atoms)

let string_of_rule = function
  | ARule (rule_name, (p, guard, (q, rhs_conns))) ->
      rule_name ^ " @@ " ^ string_of_graph p ^ " :- "
      ^ Parse.string_of_guard guard
      ^ string_of_graph q ^ ", "
      ^ string_of_connectors rhs_conns

let string_of_sem_graph (graph, rules) =
  string_of_graph graph ^ "\n\t"
  ^ String.concat "\n\t" (List.map string_of_rule rules)

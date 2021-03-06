(** Dumper for debugging purpose
    - VM が提供する関数 [dump_atom_list] を使って参照を数値に変換したアトムリストを，そのまま文字列に dump する．
 *)

open Util
open Eval

let string_of_link = function
  | DIntData i -> string_of_int i
  | DLink (port_i, atom_i) -> Printf.sprintf "%d/%d" atom_i port_i

let string_of_atom (atom_i, (p, xs)) =
  string_of_int atom_i ^ ": '" ^ p ^ "' ["
  ^ String.concat "; " (List.map string_of_link xs)
  ^ "]"

let string_of_atom_list =
  ( ^ ) "\n" <. String.concat "\n" <. List.map string_of_atom

(** Partition atoms and rules. *)

open Util
open Parse

(** classify atoms and rules *)
let rec classify_proc = function
  | Zero -> []
  | Graph atom -> [ Either.Left atom ]
  | Mol (p, q) -> List.concat_map classify_proc [ p; q ]
  | Rule (maybe_name, p_q) as rule ->
      (* ルール名をユーザが書かなかった場合はルールのプリティプリントをルール名ということにする *)
      [ Either.Right (maybe (pretty rule) maybe_name, p_q) ]

(** partition atoms and rules *)
let partition_proc = partitionEithers <. classify_proc

(** absorb local fusions 
    - コネクタとシンボルアトムを分類する
    - 吸収可能なコネクタは吸収させる
*)

open Util
open Syntax




(** アトムをコネクタかシンボルアトムかで分類する *)
let classify_atom = function
  | ("=", [x; y]) -> Either.Right (x, y)  (* connector *)
  | atom          -> Either.Left  atom    (* symbol atom *)


(** アトムのリストをコネクタかシンボルアトムかで分類する *)
let partition_atoms = partitionEithers <. List.map classify_atom




       
(** どちらか片方でも局所リンクに繋がれているような connector だった場合は Some で包んで返す
    - つまり，吸収可能なコネクタを識別する
    - 必ず，最初の要素が LocalLink であるようにする
 *)
let find_fusion = function
  | (LocalLink _ as x), y | y, (LocalLink _ as x) -> Some (x, y)
  | _ -> None


		
	   
(** Z[Y/X] 
    - ただし，X は局所リンク（であるという前提）
*)
let substitute_link (x, y) z =
  if z = x then y else z

			 
(** p(X_1, ..., X_m)[Y/Z] *)			 
let substitute_atom = second <. List.map <. substitute_link






(** 吸収可能なコネクタがあった場合は吸収して，Some で包んで返す
    - これは一個のコネクタに関してしか処理しない．再帰呼び出しは [whileM] とかを後で使う
    - [free_connectors] は元々のアトムのリスト中に出現したコネクタのリストを反転させたもののリストを反転させたもの
    @param symbol_atoms シンボルアトムのリスト
    @param free_connectors 引数が全て自由リンクなコネクタ（substitution もできない）のリストのリスト
    @param connectors 引数に局所リンクが残っている（かも知れない）コネクタのリスト
    @return (シンボルアトムのリスト，引数が全て自由リンクなコネクタのリストのリスト)
 *)
let absorb_fusion (symbol_atoms, free_connectors, connectors) =
  let+ ((x, y), (l, r)) = break_opt find_fusion connectors in
  let symbol_atoms = List.map (substitute_atom (x, y)) symbol_atoms in
  let r = List.map (both @@ substitute_link (x, y)) r in
  (symbol_atoms, l::free_connectors, r)




(** 両方自由リンクに繋がれているコネクタを引数に取り，[FreeLink] コンストラクタを剥がす
    - 局所リンクが引数に来た場合はエラー
    - 必ず，吸収可能なコネクタを全て吸収し終えた後に呼ぶ
 *)
let free_connector_of = function
  | FreeLink x, FreeLink y -> x, y
  | x, y -> failwith @@ "Bug: cannot strip 'FreeLink' type constructor from "
			^ string_of_link x ^ " = " ^ string_of_link y


			  
					      
(** どちらか片方でも閉じたリンクを繋げるコネクタを吸収させる 
    - アトムのリストを左から舐めていって，すでに吸収可能なコネクタが存在しないことがわかったものは
      [absorbed_atoms] に追加してやる
    - コネクタの吸収は，必ず局所リンクを substitution するので
      吸収可能なコネクタではなかったもの（両方とも自由リンクに接続されていたコネクタ）が，
      substitution によって，吸収可能になることはない
    - [absorbed_atoms] はアトムのリストのリストになっている
*)
let normalize atoms =
  (* connector とシンボルアトムで分割 *)
  let symbol_atoms, connectors = partition_atoms atoms in

  (* 吸収可能なコネクタはできるだけ吸収する *)
  let symbol_atoms, free_connectors, connectors =
    whileM absorb_fusion (symbol_atoms, [[]], connectors) in

  let connectors = rev_concat_append free_connectors connectors in
  symbol_atoms, List.map free_connector_of connectors
  
			      

			

  
			      

			

					      

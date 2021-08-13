(** Pretty printer．
    仮想マシンが dump するアトムリストを文字列に整形する．以下のように細かいステップからなる．
    - VM の提供する関数 [dump_atom_list] を使って，
      アトムに id を振って，リンクはその id とポート番号の組にしたアトムリストを受け取り，
    + まずファンクタで並べ直す（アルファベット順は気にせず，アルファベットか？記号か？と引数の数で比較する）．
    + Topological sort を行い，できるだけ木の親が子よりもリストの先頭側にくるようにする．
    + リストを木構造のリスト（＝森）へ変換する．
    + 木構造を文字列に整形する（パーザに付随している Pretty printer をそのまま用いた）．
    + 文字列でまたソートする．
    + 最後にリストをドットで繋げる．
 *)


open Util
open Parse  


       
(** アトム名でソートするために補助的に用いるアトム名の分類のための型．
    OCaml の Pervasives の compare はユーザ定義の ADT でも勝手に帰納的に比較してくれるので，
    これを活用してこの型に変換してから compare と一緒にソート関数に投げれば，
    自前で比較関数を書かなくて良くなって，若干見通しが良くなる．
    この辺りの実装は実行時処理系の一部としてではなく，モジュール化して他のプログラムからも使えるようにしたい．
 *)
type atom_name_type =
	| ANAlphabet	(** アルファベットのアトム名． e.g. a, hoge, ...  *)
	| ANSymbol	(** 記号ライクなアトム名． e.g. '+', '.', ' ', ...  *)
	| ANData	(** データライクなアトム名． e.g. 1, 2, "hoge", ...  *)
	| ANUnexpected  (** 予想外のアトム名． e.g. 日本語, escape sequence, 空文字列, ...  *)



	    
(** 文字列のアトム名を [atom_name_type] に変換する
    - まだ，String 型に関しては実装していない．
 *)
let atom_name_type_of p =
  if p = "" then ANUnexpected
  else
    let c = Char.code @@ String.get p 0 in
    if      32  <= c && c <= 47  then ANSymbol		(* ' ' -- '!' *)
    else if 48  <= c && c <= 57  then ANData		(* 0 -- 9 *)
    else if 58  <= c && c <= 64  then ANSymbol		(* ':'--'@' *)
    else if 65  <= c && c <= 90  then ANAlphabet	(* 'A' -- 'Z' *)
    else if 91  <= c && c <= 96  then ANSymbol		(* '[' -- '`' *)
    else if 97  <= c && c <= 122 then ANAlphabet	(* 'a' -- 'z' *)
    else if 123 <= c && c <= 126 then ANSymbol		(* '{' -- '~' *)
    else ANUnexpected
      

(** ファンクタを比較する．
    まずはアトム名のタイプで比較して，同じなら引数の個数で比較する．
 *)	   
let compare_functor (_, (p, xs)) (_, (q, ys)) =
  compare (atom_name_type_of p, List.length xs) (atom_name_type_of q, List.length ys)


(** ファンクタでソートする *)	  
let functor_sort atoms = List.sort compare_functor atoms



				   
(** A helper function for [tpl_sort]
    - あまり美しくないのでリファクタが必要
 *)  
let rec visit atoms (l, visited) (atom_i, (_, xs) as src_atom) =
  if List.mem atom_i visited then (l, visited)
  else
    let visited = atom_i::visited in
    let traverse (dst_port_i, dst_atom_i) =
      let _, ys as dst_atom = List.assoc dst_atom_i atoms in
      if List.length ys - 1 = dst_port_i then Some (dst_atom_i, dst_atom)
      else None
    in
    let xs = List.filter_map traverse xs in
    let l, visited = List.fold_left (visit atoms) (l, visited) xs in
    src_atom::l, visited

		   
(** Topological sort *)
let tpl_sort atoms = fst @@ List.fold_left (visit atoms) ([], []) atoms





(** アトムリストを木構造へ変換する
    - あまり美しくないのでリファクタが必要
*)	       
let rec tree_of_link src_atom_i src_port_i (free_link, link2link_name, atoms) (dst_port_i, dst_atom_i as dst_link) =
  let leaf_of_link dst_link =
    match List.assoc_opt dst_link link2link_name with
    | None ->
       let link = Link ("L" ^ string_of_int free_link) in
       (succ free_link, ((src_port_i, src_atom_i), link)::link2link_name, atoms), link
    | Some link_name -> (free_link, link2link_name, atoms), link_name
  in    
  match List.assoc_opt dst_atom_i atoms with
  | None -> leaf_of_link dst_link
  | Some (p, xs) ->
     if List.length xs - 1 = dst_port_i then (* 最終リンクで接続されているなら埋め込みが可能 *)
       tree_of_atom (free_link, link2link_name, atoms) (dst_atom_i, (p, dropLast1 xs))
     else leaf_of_link dst_link
and tree_of_atom (free_link, link2link_name, atoms) (src_atom_i, (p, xs)) =
  let atoms = List.remove_assoc src_atom_i atoms in
  let env, xs = fold_left_mapi (tree_of_link src_atom_i) (free_link, link2link_name, atoms) xs in
  env, Atom (p, xs)


	    
(** アトムリストを森へ変換する
    - リストの中で，木の親が子よりも先に現れると仮定している（このために事前に topological sort を要求する）
    - あまり美しくないのでリファクタが必要
*)	       
let forest_of_atoms atoms =
  let rec dump_atoms (_, _, atoms as env)  =
    match atoms with
    | [] -> []
    | atom::_ ->
       let env, tree = tree_of_atom env atom in
       tree::dump_atoms env
  in
  dump_atoms (0, [], atoms)




let pretty_print atoms =
  String.concat ". "
  @@ List.sort compare
  @@ List.map Parse.string_of_atom
  @@ forest_of_atoms
  @@ tpl_sort
  @@ functor_sort atoms


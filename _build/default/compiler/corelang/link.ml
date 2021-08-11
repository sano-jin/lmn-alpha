(** リンク名の解析
    - 局所・自由リンクを収集する 
*)


open Util
open Parse
open Compile_error



(** リンク名の収集用のマップオブジェクト *)
module LinkMap = Map.Make(String)
       

(** 自由リンクの名前のためのセットオブジェクト *)
module LinkSet = Set.Make(String)


(** LinkSet の dumper *)			 
let string_of_link_set = String.concat ", " <. LinkSet.elements 



			 
       
(** リンク名を管理する multiset に insert する *)
let add_link = flip LinkMap.update (Option.some <. succ <. maybe 0)

					   
(** アトムからリンク名を収集する *)
let rec link_names_of_atom link_names = function
  | Atom (_, xs) -> List.fold_left link_names_of_atom link_names xs  
  | Link x -> add_link x link_names


(** アトムのリストからリンク名を収集する *)
let link_names_of_atoms =
  List.fold_left link_names_of_atom LinkMap.empty


		 
(** 3回以上現れているリンクがないかチェック *)
let check_link_occur_num link_name occur_num =
  if occur_num > 2 then
    raise @@ CompileError ("link " ^ link_name ^ " appeared more than twice")
  else ()
  

let check_link_occur_nums = LinkMap.iter check_link_occur_num

					   

(** 局所リンクと自由リンクを分割 *)		 
let partition_links = LinkMap.partition ((=) 2 <.. flip const)





(** 局所・自由リンクを収集する
    - リンク条件のチェックも行う
    @return (局所リンクのリスト，自由リンクのセット)
 *)					  
let links_of atoms =
  let link_names = link_names_of_atoms atoms in
  check_link_occur_nums link_names; 
  let locals, frees = partition_links link_names in
  List.map fst @@ LinkMap.bindings locals 
  , LinkSet.of_seq @@ Seq.map fst @@ LinkMap.to_seq frees






		 


(** Core language の構文の型やその dumper など *)


open Util



     
(** アルファ変換後のリンク名 *)
type c_link =
  | LocalLink of int    (** 局所リンクは一意な id に変換 *)
  | FreeLink of string  (** 自由リンク *)


type c_atom = string * c_link list

type c_conn = string * string			      

type c_rule = CRule of string * (c_atom list * (c_atom list * c_conn list))



		  


				 
(** dumper *)
let string_of_link = function
  | LocalLink link_id -> "_L" ^ string_of_int link_id
  | FreeLink x -> x


let string_of_atom (p, xs) =
  p ^ " [" ^ String.concat ", " (List.map string_of_link xs) ^ "]"

let string_of_atoms  =
  String.concat ", " <. List.map string_of_atom


				 
let string_of_connector (x, y) =
  x ^ " = " ^ y
		
let string_of_connectors = 
  String.concat ", " <. List.map string_of_connector 

		

let string_of_rhs (atoms, conns) =
  String.concat ", "
  @@ List.map string_of_atom atoms @ List.map string_of_connector conns


									 
let string_of_rule = function
  | CRule (rule_name, (p, q)) ->
     rule_name ^ " @@ " ^ string_of_atoms p ^ " :- " ^ string_of_rhs q



let string_of_sem_graph (atoms, rules) =
  string_of_atoms atoms ^ "\n\t" ^ String.concat "\n\t" (List.map string_of_rule rules)

						 




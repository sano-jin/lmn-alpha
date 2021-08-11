(* debug_syntax.ml *)


open Syntax
open Util



(** アトムの引数部分の pretty printer *)
let rec string_of_arg = function
  | Link x -> x
  | Atom (".", [h; t]) -> string_of_list h t
  | Atom (a, xs) ->
     if xs = [] then a 
     else a ^ "(" ^ String.concat ", " (List.map string_of_arg xs) ^ ")"
and string_of_list h t =
  let rec string_of_list_inner = function
    | Atom (".", [h; t]) ->
       ", " ^ string_of_arg h ^ string_of_list_inner t
    | Atom ("[]", []) -> ""
    | arg -> " | " ^ string_of_arg arg
  in
  "[" ^ string_of_arg h ^ string_of_list_inner t ^ "]"
     

(** アトムの pertty printer *)						     
let string_of_atom = function
  | Atom ("=", [x; y]) -> string_of_arg x ^ " = " ^ string_of_arg y
  | Atom (".", [h; t]) -> string_of_list h t
  | atom -> string_of_arg atom
  

(** プロセスの pretty printer *)								       
let rec string_of_proc priority = function
  | Zero -> ""
	      
  | Graph atom -> string_of_atom atom

  | Rule (maybe_name, (lhs, rhs)) ->
     let maybe_name = (^) " @@ " <$> maybe_name in
     let str_of_rule =
       maybe "" maybe_name 
       ^ string_of_proc 1 lhs ^ " :- " ^ string_of_proc 1 rhs
     in
     if priority > 1 then "(" ^ str_of_rule ^ ")"
     else str_of_rule

  | Mol (p, q) ->
     let str_of_mol i sep =
       string_of_proc i p ^ sep ^ string_of_proc i q
     in
     if priority = 0 then str_of_mol 0 ". "
     else if priority > 2 then "(" ^ str_of_mol 2 ", " ^ ")" 
     else str_of_mol 2 ", "								       


     
let string_of_proc = string_of_proc 0


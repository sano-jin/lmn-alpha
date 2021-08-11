(**  Parser *)
     
%{
  open Syntax
%}

(** tokens with values *)   
%token <string> AtomName (** x, y, abc, ... *)
%token <string> LinkName (** X, Y, ABC, ...  *)

(** operators *)
%token DOT		(**  '.' *)
%token COMMA		(**  ',' *)
%token EQ		(**  '='  *)
%token VBAR		(**  "|" *)
%token COLMIN		(**  ":-" *)
%token ATAT		(**  "@@" *)

(**  Parentheses *)
%token LPAREN   (**  '(' *)
%token RPAREN   (**  ')' *)
%token LBRACKET (**  '[' *)
%token RBRACKET (**  ']' *)

(**  End of file *)
%token EOF 

(**  Operator associativity *)
%nonassoc COLMIN
%left COMMA 

      
%start main
%type <Syntax.proc> main

%%


(**  Main part must end with EOF (End Of File) *)
main:
  | program EOF  { $1 }
;




(** inner arguments of an atom *)

(**  arguments of an atom separated by comma without parentheses *)
args_inner:
  | arg { [$1] }
  | arg COMMA args_inner { $1::$3 }
;

(**  argument of an atom *)
arg:
  | LinkName { Link $1 }
  | atom { $1 }
;
    


(** Syntax for an atom *)

(** normal notation for denoting an atom *)
normal_atom:
  | AtomName                    { Atom ($1, []) }       (** e.g. a *)
  | AtomName LPAREN RPAREN      { Atom ($1, []) }       (** e.g. a() *)
  | AtomName LPAREN args_inner RPAREN { Atom ($1, $3) } (** e.g. a(X_1, ..., X_m) *)
;

(** List abbreviation *)
cons_atom:
  | LBRACKET RBRACKET { Atom ("[]", []) }	(** e.g. [] *)
  | LBRACKET VBAR atom RBRACKET { $3 }		(** e.g. [|a] == a *)
  | LBRACKET args_inner VBAR arg RBRACKET	(** e.g. [X_1, ..., X_m | a] *)
     { List.fold_right (fun e l -> Atom (".", [e; l])) $2 $4 }   
  | LBRACKET args_inner RBRACKET		(** e.g. [X_1, ..., X_m] *)
     { List.fold_right (fun e l -> Atom (".", [e; l])) $2 (Atom ("[]", [])) }   
    
(** atom *)
atom:
  | normal_atom  { $1 }
  | cons_atom    { $1 }
  


(** Syntax for rules *)

(** rule *)
rule:
  | proc COLMIN proc {($1, $3)}
  | proc COLMIN {($1, Zero)}

(** rule with a name maybe  *)
maybe_name_rule:
  | AtomName ATAT rule { Rule (Some $1, $3)}
  | rule { Rule (None, $1)}



	 
(** rooted graph *)
graph:
  | atom { $1 }
  | arg EQ arg { Atom ("=", [$1; $3]) }  (** e.g. X = Y. R = append(nil, L) *)


  

(**  proccesses separeted by comma *)
proc:
  | graph { Graph $1 }

  | proc COMMA proc { Mol ($1, $3) }

  | maybe_name_rule {$1}
  
  | LPAREN proc RPAREN { $2 }
;

(** processes separeted by period *)
block:       
  | proc DOT block { Mol ($1, $3) }
  | proc DOT { $1 }
  | proc { $1 }
;





(** the whole program *)
program:
  | block { $1 }
    
  | error
    { 
      let message =
        Printf.sprintf 
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  }
;


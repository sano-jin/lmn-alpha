(**  Parser *)
     
%{
  open Syntax
%}

(** tokens with values *)   
(** Symbol atom name *)
%token <string> SYMBOL (** x, y, abc, ... *)
%token <int>    INT (** 1, 2, -3, ... *)

(** operator names *)
%token <string> OP12   (** #... *)
%token <string> OP11   (** \... *)
%token <string> OP10   (** **... *)
%token <string> OP9    (** *..., /..., MOD *)
%token <string> OP8    (** +..., -... *)
%token <string> OP7    (** :... *)
%token <string> OP6    (** @..., ^... *)
%token <string> OP5    (** =..., >..., <..., &..., $..., !..., |... *)
%token <string> OP4    (** && *)
%token <string> OP3    (** || *)
%token <string> OP2    (** <-, :=, -> *)
%token <string> OP1    (** ; *)

(** link name *)
%token <string> LINKNAME   (** X, Y, ABC, ...  *)

(** operators *)
%token MINUS		(**  '-' *)
%token DOLLAR		(**  '$' *)
%token DOT		(**  '.' *)
%token COMMA		(**  ',' *)
%token VBAR		(**  "|" *)
%token COLMIN		(**  ":-" *)
%token ATAT		(**  "@@" *)

(**  Parentheses *)
%token LPAREN   (**  '(' *)
%token RPAREN   (**  ')' *)
%token LBRACKET (**  '[' *)
%token RBRACKET (**  ']' *)
(*
%token LBRACE (**  '{' *)
%token RBRACE (**  '}' *)
 *)
       
(**  End of file *)
%token EOF 

(**  Operator associativity *)
%nonassoc COLMIN 
%nonassoc VBAR
%right    COMMA
%left     OP1
%nonassoc OP2
%left     OP3
%left     OP4
%left     OP5
%left     OP6
%right    OP7
%left     OP8 MINUS
%left     OP9
%right    OP10
%left     OP11
%left     OP12 

      
%start main
%type <Syntax.proc> main

%start atom_name_type
%type <Syntax.atom_name_type> atom_name_type

%%



(** associativity and precedence of an atom_name *)
atom_name_type:
  | op_asc_prcd EOF	{ $1 }
  | SYMBOL EOF		{ ANSymbol }
  | error		{ ANQuoted }
;


(** associativity and precedence of operators *)
op_asc_prcd:
  | OP1    { ANOp (Syntax.AscLeft, 1) }
  | OP2    { ANOp (Syntax.AscNone, 2) }
  | OP3    { ANOp (Syntax.AscLeft, 3) }
  | OP4    { ANOp (Syntax.AscLeft, 4) }
  | OP5    { ANOp (Syntax.AscLeft, 5) }
  | OP6    { ANOp (Syntax.AscLeft, 6) }
  | OP7    { ANOp (Syntax.AscRight, 7) }
  | OP8    { ANOp (Syntax.AscLeft, 8) }
  | MINUS  { ANOp (Syntax.AscLeft, 8) }
  | OP9    { ANOp (Syntax.AscLeft, 9) }
  | OP10   { ANOp (Syntax.AscRight, 10) }
  | OP11   { ANOp (Syntax.AscLeft, 11) }
  | OP12   { ANOp (Syntax.AscLeft, 12) }
;


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
  | LINKNAME { Link $1 }
  | atom { $1 }
;
    


(** Syntax for an atom *)

(** normal notation for denoting an atom *)
normal_atom:
  | SYMBOL				{ Atom ($1, []) }	(** e.g. a *)
  | SYMBOL LPAREN RPAREN		{ Atom ($1, []) }       (** e.g. a() *)
  | SYMBOL LPAREN args_inner RPAREN	{ Atom ($1, $3) }	(** e.g. a(X_1, ..., X_m) *)
;


(** List abbreviation *)
cons_atom:
  | LBRACKET RBRACKET { Atom ("[]", []) }	(** e.g. [] *)
  | LBRACKET VBAR atom RBRACKET { $3 }		(** e.g. [|a] == a *)
  | LBRACKET args_inner VBAR arg RBRACKET	(** e.g. [X_1, ..., X_m | a] *)
     { List.fold_right (fun e l -> Atom (".", [e; l])) $2 $4 }   
  | LBRACKET args_inner RBRACKET		(** e.g. [X_1, ..., X_m] *)
     { List.fold_right (fun e l -> Atom (".", [e; l])) $2 (Atom ("[]", [])) }   
;


(** operator names *)
op_name:
  | OP1		{ $1 }
  | OP2		{ $1 }
  | OP3		{ $1 }
  | OP4		{ $1 }
  | OP5		{ $1 }
  | OP6		{ $1 }
  | OP7		{ $1 }
  | OP8		{ $1 }
  | MINUS	{ "-" }
  | OP9		{ $1 }
  | OP10	{ $1 }
  | OP11	{ $1 }
  | OP12	{ $1 }
;


(** operators *)
op_atom:
  | arg OP1   arg { Atom ($2, [$1; $3]) }
  | arg OP2   arg { Atom ($2, [$1; $3]) }
  | arg OP3   arg { Atom ($2, [$1; $3]) }
  | arg OP4   arg { Atom ($2, [$1; $3]) }
  | arg OP5   arg { Atom ($2, [$1; $3]) }
  | arg OP6   arg { Atom ($2, [$1; $3]) }
  | arg OP7   arg { Atom ($2, [$1; $3]) }
  | arg OP8   arg { Atom ($2, [$1; $3]) }
  | arg MINUS arg { Atom ("-", [$1; $3]) }
  | arg OP9   arg { Atom ($2, [$1; $3]) }
  | arg OP10  arg { Atom ($2, [$1; $3]) }
  | arg OP11  arg { Atom ($2, [$1; $3]) }
  | arg OP12  arg { Atom ($2, [$1; $3]) }
;

  
(** process context *)
process_context:
  | DOLLAR SYMBOL 	{ ProcCtx $2 }
;

  
(** integer atom *)  
integer_atom:
  | INT                 { IntData $1 }
  | MINUS INT           { IntData (- $2) }
;


(** unary operator *)
unary_atom:
  | op_name normal_atom		{ Atom ($1, [$2]) }
  | op_name cons_atom		{ Atom ($1, [$2]) }
  | op_name process_context	{ Atom ($1, [$2]) }
  | op_name LPAREN atom RPAREN	{ Atom ($1, [$3]) }
  | op_name unary_atom		{ Atom ($1, [$2]) }


			
(** atom *)
atom:
  | normal_atom		{ $1 }
  | cons_atom		{ $1 }
  | op_atom		{ $1 }
  | process_context     { $1 }
  | integer_atom        { $1 }
  | LPAREN atom RPAREN  { $2 }
  | unary_atom          { $1 }
; 


(** Syntax for rules *)


(** rule *)
rule:
  | proc COLMIN proc VBAR proc	{ ($1, $3, $5) }
  | proc COLMIN proc VBAR       { ($1, $3, Zero) }
  | proc COLMIN VBAR proc     	{ ($1, Zero, $4) }
  | proc COLMIN VBAR	      	{ ($1, Zero, Zero) }
  | proc COLMIN proc	      	{ ($1, Zero, $3) }
  | proc COLMIN		      	{ ($1, Zero, Zero) }



(** rule with a name maybe  *)
maybe_name_rule:
  | SYMBOL ATAT rule { Rule (Some $1, $3)}
  | rule { Rule (None, $1)}
  

(**  proccesses separeted by comma *)
proc:
  | atom { Graph $1 }

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


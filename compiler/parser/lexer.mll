(** Lexer *)

{
  open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z' '_']
let alpha = lower | upper
let alnum = digit | alpha | '\''
let operator_symbol =
  ['+' '-' '/' '*' '<' '>' '=' '^' '~' '_' '&' '@' '$' '.' '?' '%' '#' '!' '|' ':' ';' '\'' '\\']
	       
			      
rule token = parse
  (** Operators *)
  | '-'			{ MINUS }
  | '$'			{ DOLLAR }
  | '.'			{ DOT }
  | ','			{ COMMA }
  | '|'			{ VBAR }
  | ":-"		{ COLMIN }
  | "@@"		{ ATAT }


  (** Parentheses *)
  | '('			{ LPAREN }
  | ')'			{ RPAREN }
  | '['			{ LBRACKET }
  | ']'			{ RBRACKET }
(*  | '{'			{ LBRACE }
    | '}'			{ RBRACE }
 *)
			
  (** operator names *)
  | '#' operator_symbol*
    { OP12 (Lexing.lexeme lexbuf) }
  
  | '\\' operator_symbol*
    { OP11 (Lexing.lexeme lexbuf) }
  
  | "**" 
    { OP10 (Lexing.lexeme lexbuf) }
  
  | ['*' '/'] operator_symbol*
    { OP9 (Lexing.lexeme lexbuf) }

  | "mod" 
    { OP9 (Lexing.lexeme lexbuf) }
  
  | ['+' '-'] operator_symbol*
    { OP8 (Lexing.lexeme lexbuf) }
  
  | [':'] operator_symbol*
    { OP7 (Lexing.lexeme lexbuf) }
  
  | ['@' '^'] operator_symbol*
    { OP6 (Lexing.lexeme lexbuf) }
  
  | ['=' '<' '>' '|' '&' '$' '!'] operator_symbol*
    { OP5 (Lexing.lexeme lexbuf) }
  
  | ("and" | "&&")
    { OP4 (Lexing.lexeme lexbuf) }
  
  | ("or" | "||")
    { OP3 (Lexing.lexeme lexbuf) }
  
  | ("<-" | ":=" | "->")
    { OP2 (Lexing.lexeme lexbuf) }
  
  | ';'
    { OP1 (Lexing.lexeme lexbuf) }

  (** data atom name *)
  | digit+
    { INT (int_of_string @@ Lexing.lexeme lexbuf)}
      
    
  (** symbol atom name *)
  | lower alnum*
    { SYMBOL (Lexing.lexeme lexbuf) }

  
  (** link name *)
  | upper alnum*
    { LINKNAME (Lexing.lexeme lexbuf) }


  (** end of file *)
  | eof       { EOF }

  (** spaces *)
  | space+    { token lexbuf }

  (** comments *)
  | '%' [^ '\n']*  { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "unknown token '%s' near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }

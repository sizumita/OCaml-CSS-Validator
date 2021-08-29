{
open Parser
}

let digit = ['0'-'9']
let unicode = '\\'['0'-'9' 'a'-'f'] ['0'-'9' 'a'-'f']? ['0'-'9' 'a'-'f']? ['0'-'9' 'a'-'f']? ['0'-'9' 'a'-'f']? ['0'-'9' 'a'-'f']? ('\r' '\n'|[ '\n' '\r' '\t'])?
let nonascii = [^'0'-'\177']
let escape = unicode|'\\'[^'\n' '\r' '0'-'9' 'a'-'f']
let ident = ['-']? (['_' 'a'-'z']|nonascii|escape)+
let nl = '\n' | '\r' '\n' | '\r'
let string = ('\"' ([^'\n' '\r' '\"'] | '\\' nl | escape)* '\"') | ("\'" ([^'\n' '\r' '\''] | nl | escape)* "\'")
let nmchar = ['_' 'a'-'z' '0'-'9' '-'] | nonascii | escape
let name = nmchar+
let num = ['+' '-']? (['0'-'9']+ | ['0'-'9']* '.' ['0'-'9']+) ('e'['+' '-']? ['0'-'9']+)?


rule tokenize = parse
  | string as str { STRING str }
  | ['\t' '\r' '\n']+ { S }
  | num "%" as numb { PERCENTAGE numb }
  | num ident as str { DIMENSION str }
  | "@" ident as str { ATKEYWORD str }
  | "#" name as str { HASH str }
  | ident as str { IDENT str }
  | num as numb { NUMBER numb }
  | "<!--" { CDO }
  | "-->" { CDC }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "(" { LP }
  | ")" { RP }
  | "{" { LB }
  | "}" { RB }
  | "[" { LS }
  | "]" { RS }
  | "~=" { INCLUDES }
  | "|=" { DASHMATCH }

  | eof { EOF }
  | _
    { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
and comment = parse
  | "*/" { () }
  | "/*"
    { comment lexbuf;
      comment lexbuf }
  | eof { Format.eprintf "warning: unterminated comment*." }
  | _ { comment lexbuf }

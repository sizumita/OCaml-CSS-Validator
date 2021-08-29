{
open Parser
}

let u_ = u|\\0{0,4}(55|75)(\r\n|[ \t\r\n\f])?|\\u
let r_ = r|\\0{0,4}(52|72)(\r\n|[ \t\r\n\f])?|\\r
let l_ = l|\\0{0,4}(4c|6c)(\r\n|[ \t\r\n\f])?|\\l
let w = [ \t\r\n\f]*
let nl = \n|\r\n|\r|\f
let string2 = "\'" ([^\n\r\f\"\'"]|\\nl|escape)* "\'"
let string1 = '\"' ([^\n\r\f\'\"']|\\nl|escape)* '\"'
let string = string1|string2
let num = [+-]?([0-9]+|[0-9]*\.[0-9]+)(e[+-]?[0-9]+)?
let nmchar = [_a-z0-9-]|nonascii|escape
let escape = unicode|\\[^\n\r\f0-9a-f]
let unicode = \\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?
let nonascii = [^\0-\177]
let nmstart = [_a-z]|nonascii|escape
let name = nmchar+
let ident = [-]?nmstart nmchar*

rule tokenize = parse
  | whitespace+ { tokenize lexbuf }
  | num ident as str { DIMENSION str }
  | num "%" as str { PERCENTAGE str }
  | "@" ident as str { ATKEYWORD str }
  | "#" name as str { HASH str }
  | ident as str { IDENT str }
  | string as str { STRING str }
  | num as number { NUMBER number }
  | u_ r_ l_ \(w string w\) as uri { URI uri }
  | u_ r_ l_\(w([!#$%&*-\[\]-~]|nonascii|escape)*w\) as uri { URI uri }
  | u\+[?]{1,6} as uni { UNICODE_RANGE uni }
  | u\+[0-9a-f]{1}[?]{0,5} as uni { UNICODE_RANGE uni }
  | u\+[0-9a-f]{2}[?]{0,4} as uni { UNICODE_RANGE uni }
  | u\+[0-9a-f]{3}[?]{0,3} as uni { UNICODE_RANGE uni }
  | u\+[0-9a-f]{4}[?]{0,2} as uni { UNICODE_RANGE uni }
  | u\+[0-9a-f]{5}[?]{0,1} as uni { UNICODE_RANGE uni }
  | u\+[0-9a-f]{6} as uni { UNICODE_RANGE uni }
  | u\+[0-9a-f]{1,6}-[0-9a-f]{1,6} as uni { UNICODE_RANGE uni }
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
  | [ \t\r\n\f]+ { S }
  | ident "{" as str { FUNCTION str }
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

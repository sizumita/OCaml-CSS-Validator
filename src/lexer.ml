open Parser

let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? 
  Opt (Chars "+-"),
 (Plus digit | (Star digit, '.', Plus digit)),
  Opt (Chars "eE", (Opt Chars "+-"), Plus digit)
]
let nonascii = [%sedlex.regexp? Compl 0 .. 177]
let unicode = [%sedlex.regexp? '\\', Rep (('0'..'9' | 'a'..'f'), 1 .. 6), Opt ("\r\n" | Chars "\n\r\t")]
let escape = [%sedlex.regexp? unicode | ('\\', Compl ('\n' | '\r' | '0'..'9' | 'a'..'f'))]
let nmchar = [%sedlex.regexp? (('_' | 'a'..'z' | '0'..'9' | '-') | nonascii | escape)]
let nmstart = [%sedlex.regexp? ('_' | 'a'..'z') | nonascii | escape]
let name = [%sedlex.regexp? Plus nmchar]
let ident = [%sedlex.regexp? Opt '-', nmstart, Star nmchar]
let nl = [%sedlex.regexp? '\n' | "\r\n" | '\r']
let string = [%sedlex.regexp? (
    '\"', Star (Compl (Chars "\n\r\"") | ('\\', nl) | escape), '\"'
  | '\'', Star (Compl (Chars "\n\r\'") | ('\\', nl) | escape), '\''
)]

type lexbuf = {
  stream : Sedlexing.lexbuf ;
  mutable pos : Lexing.position ;
}
let create_lexbuf ?(file="") stream =
  let pos = {Lexing.
    pos_fname = file;
    pos_lnum = 1; (* Start lines at 1, not 0 *)
    pos_bol = 0;
    pos_cnum = 0;
  }
  in { pos ; stream }

let new_line ?(n=0) lexbuf =
  let _ = n in
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
        pos_lnum = lcp.pos_lnum + 1;
        pos_bol = lcp.pos_cnum;
    }


let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos }

let lexeme { stream; _ } = Sedlexing.Utf8.lexeme stream


let rec lex lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
    | "/*" -> update lexbuf; comment lexbuf
    | '\n' ->
      update lexbuf; new_line lexbuf;
      lex lexbuf

    (** 空白文字 *)
    | white_space ->
      update lexbuf;
      lex lexbuf
    | string -> update lexbuf ; STRING (lexeme lexbuf)
    | number -> update lexbuf ; NUMBER (lexeme lexbuf)
    | number, '%' -> update lexbuf ; PERCENTAGE (lexeme lexbuf)
    | number, ident -> update lexbuf ; DIMENSION (lexeme lexbuf)
    | '@', ident -> update lexbuf ; ATKEYWORD (lexeme lexbuf)
    | '#', name -> update lexbuf ; HASH (lexeme lexbuf)
    | ident -> update lexbuf ; IDENT (lexeme lexbuf)
    | "<!--" -> update lexbuf ; CDO
    | "-->" -> update lexbuf ; CDC
    | ':' -> update lexbuf ; COLON
    | ';' -> update lexbuf ; SEMICOLON
    | '(' -> update lexbuf ; LP
    | ')' -> update lexbuf ; RP
    | '{' -> update lexbuf ; LB
    | '}' -> update lexbuf ; RB
    | '[' -> update lexbuf ; LS
    | ']' -> update lexbuf ; RS
    | "~=" -> update lexbuf ; INCLUDES
    | "|=" -> DASHMATCH

    | eof ->
      update lexbuf ;
      EOF
    | _ -> failwith "Parse error"
and comment lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | "*/" -> update lexbuf; lex lexbuf
  | '\n' -> update lexbuf; new_line lexbuf; comment lexbuf
  | eof -> failwith "Parse error"
  | any -> update lexbuf ; comment lexbuf
  | _ -> failwith "Parse error"

let parse f lexbuf =
  let lexer () =
    let ante_position = lexbuf.pos in
    let token = lex lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised f
  in
  try
    parser lexer
  with
    | Sedlexing.MalFormed
    | Sedlexing.InvalidCodepoint _
      -> failwith "parse error"

let parse_prog lexbuf =
  parse Parser.prog lexbuf

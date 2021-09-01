open Parser

let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? 
  Opt (Chars "+-"),
 (Plus digit | (Star digit, '.', Plus digit)),
  Opt (Chars "eE", (Opt Chars "+-"), Plus digit)
]
let u_char = [%sedlex.regexp? 
    'u'
  | ('\\', Rep ('0', 0 .. 4), ("55" | "75"), ("\r\n" | Chars "\t\r\n"))
  | ('\\', 'u')
]
let r_char = [%sedlex.regexp? 
    'r'
  | ('\\', Rep ('0', 0 .. 4), ("52" | "72"), ("\r\n" | Chars "\t\r\n"))
  | ('\\', 'r')
]
let l_char = [%sedlex.regexp? 
    'l'
  | ('\\', Rep ('0', 0 .. 4), ("4c" | "6c"), ("\r\n" | Chars "\t\r\n"))
  | ('\\', 'l')
]
let w = [%sedlex.regexp? Star Chars "\t\r\n"]
let nonascii = [%sedlex.regexp? 0x80 .. 0x10ffff]
let unicode = [%sedlex.regexp? '\\', Rep (('0'..'9' | 'A'..'Z' | 'a'..'f'), 1 .. 6), Opt ("\r\n" | Chars "\n\r\t")]
let escape = [%sedlex.regexp? unicode | ('\\', Compl ('\n' | '\r' | '0'..'9' | 'A'..'Z' | 'a'..'f'))]
let nmchar = [%sedlex.regexp? (('_' | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-') | nonascii | escape)]
let nmstart = [%sedlex.regexp? ('_' | 'A'..'Z' | 'a'..'z') | nonascii | escape]
let name = [%sedlex.regexp? Plus nmchar]
let ident = [%sedlex.regexp? Opt '-', nmstart, Star nmchar]
let nl = [%sedlex.regexp? '\n' | "\r\n" | '\r']
let string = [%sedlex.regexp? (
    '\"', Star (Compl (Chars "\n\r\"") | ('\\', nl) | escape), '\"'
  | '\'', Star (Compl (Chars "\n\r\'") | ('\\', nl) | escape), '\''
)]
let url = [%sedlex.regexp?
    (u_char, r_char, l_char, '(', w, string, w, ')')
  | (u_char, r_char, l_char, '(', w, Star (* これでいいのかわからない *)(Compl ')'), w, ')')
]
let unicode_range_start = [%sedlex.regexp? "u+" | "U+"]
let unicode_chars = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A' .. 'F']
let unicode_range = [%sedlex.regexp?
    unicode_range_start, Rep (unicode_chars, 1), Rep ('?', 0 .. 5)
  | unicode_range_start, Rep (unicode_chars, 2), Rep ('?', 0 .. 4)
  | unicode_range_start, Rep (unicode_chars, 3), Rep ('?', 0 .. 3)
  | unicode_range_start, Rep (unicode_chars, 4), Rep ('?', 0 .. 2)
  | unicode_range_start, Rep (unicode_chars, 5), Rep ('?', 0 .. 1)
  | unicode_range_start, Rep (unicode_chars, 6)
  | unicode_range_start, Rep (unicode_chars, 1 .. 6), '-', Rep (unicode_chars, 1 .. 6)
]

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

let string_of_ParseError (file, line, cnum, tok) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  Printf.sprintf
    "Parse error%s line %i, column %i, token %s"
    (file_to_string file)
    line cnum tok

let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos }

let lexeme { stream; _ } = Sedlexing.Utf8.lexeme stream

exception ParseError of (string * int * int * string)

let raise_ParseError lexbuf =
  let {pos; _} = lexbuf in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  let tok = lexeme lexbuf in
  raise @@ ParseError (pos.pos_fname, line, col, tok)

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
    | '!', Star white_space, "important" -> update lexbuf ; IMPORTANT
    | url -> update lexbuf ; URI (lexeme lexbuf)
    | unicode_range -> update lexbuf ; UNICODE_RANGE (lexeme lexbuf)
    | string -> update lexbuf ; STRING (lexeme lexbuf)
    | number -> update lexbuf ; NUMBER (lexeme lexbuf)
    | number, '%' -> update lexbuf ; PERCENTAGE (lexeme lexbuf)
    | number, ident -> update lexbuf ; DIMENSION (lexeme lexbuf)
    | '@', ident -> update lexbuf ; ATKEYWORD (lexeme lexbuf)
    | '#', name -> update lexbuf ; HASH (lexeme lexbuf)
    | ident, '(' -> update lexbuf ; FUNCTION (lexeme lexbuf)
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
    | ',' -> update lexbuf ; COMMA
    | '+' -> update lexbuf ; PLUS
    | "|=" -> update lexbuf ; DASHMATCH
    | "~=" -> update lexbuf ; SPACEINMATCH
    | "^=" -> update lexbuf ; STARTSMATCH
    | "$=" -> update lexbuf ; ENDSMATCH
    | "*=" -> update lexbuf ; INMATCH
    | '=' -> update lexbuf ; MATCH
    | '>' -> update lexbuf ; CHILD
    | '.' -> update lexbuf ; DOT
    | '~' -> update lexbuf ; SIBILING
    | '*' -> update lexbuf ; UNIVERSAL

    | eof ->
      update lexbuf ;
      EOF
    | _ -> update lexbuf; raise_ParseError lexbuf
    and comment lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | "*/" -> update lexbuf; lex lexbuf
  | '\n' -> update lexbuf; new_line lexbuf; comment lexbuf
  | eof -> update lexbuf; raise_ParseError lexbuf
  | any -> update lexbuf ; comment lexbuf
  | _ -> update lexbuf; raise_ParseError lexbuf

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

let parse_tokens lexbuf =
  parse Parser.tokens lexbuf

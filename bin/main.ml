open Css_validator

let pprint = function
  | Ast.Ident s -> "Ident " ^ s
  | Ast.String s -> "String " ^ s
  | Ast.Hash s -> "Hash " ^ s
  | Ast.AtKeyword s -> "AtKeyword " ^ s
  | Ast.Number s -> "Number " ^ s
  | Ast.Percentage s -> s
  | Ast.Dimension s -> "Dimension " ^ s


let _ =
  let lexbuf = Lexing.from_string "123%" in
  let expr = Parser.prog Lexer.tokenize lexbuf in
  Printf.printf "%s" (pprint expr)

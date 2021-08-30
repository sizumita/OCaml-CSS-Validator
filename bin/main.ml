open Css_validator

let pprint = function
  | Ast.Ident s -> "Ident " ^ s
  | Ast.String s -> "String " ^ s
  | Ast.Hash s -> "Hash " ^ s
  | Ast.AtKeyword s -> "AtKeyword " ^ s
  | Ast.Number s -> "Number " ^ s
  | Ast.Percentage s -> s
  | Ast.Dimension s -> "Dimension " ^ s
  | Ast.Url s -> "Url " ^ s
  | Ast.UnicodeRange s -> "UnicodeRange " ^ s

let parse () = 
  let lexbuf = Lexer.create_lexbuf @@ Sedlexing.Utf8.from_channel stdin in 
  Lexer.parse_prog lexbuf

let l = parse () |> List.map (fun x -> print_endline (pprint x))
let () = Printf.printf "%d exprs" @@ List.length l

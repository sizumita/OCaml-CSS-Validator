open Css_validator

let rec pprint = function
  | Ast.Ident s -> s
  | Ast.String s -> "\"" ^ s ^ "\""
  | Ast.Hash s -> "Hash " ^ s
  | Ast.AtKeyword s -> "AtKeyword " ^ s
  | Ast.Number s -> "Number " ^ s
  | Ast.Percentage s -> s
  | Ast.Dimension s -> "Dimension " ^ s
  | Ast.Uri s -> "Url " ^ s
  | Ast.UnicodeRange s -> "UnicodeRange " ^ s
  | Ast.Cdo -> "CDO"
  | Ast.Cdc -> "CDC"
  | Ast.Null -> "Null"
  | Ast.Includes -> "Includes"
  | Ast.Colon -> "Colon"
  | Ast.SemiColon -> "SemiColon"
  | Ast.Selector l -> "Selector: " ^ pprint_list l
  | Ast.RuleSet (Some l, None) -> "RuleSet: " ^ (pprint_list l) ^ " -> None"
  | Ast.RuleSet (Some l, Some l2) -> "RuleSet: " ^ (pprint_list l) ^ " -> " ^ pprint_list l2
  | Ast.RuleSet (None, Some l2) -> "RuleSet: " ^ "None" ^ " -> " ^ pprint_list l2
  | Ast.RuleSet (None, None) -> "RuleSet: None -> None"
  | Ast.Declaration (l, l2) -> "Declaration: " ^ (pprint l) ^ " -> " ^ (pprint_list l2)
  | Function (name, None) -> Printf.sprintf "Function(%s)()" name
  | Function (name, Some(values)) -> Printf.sprintf "Function(%s)(%s)" name @@ pprint_list values
  | Block None -> "{}"
  | PBlock None -> "()"
  | SBlock None -> "[]"
  | Block Some (l) -> Printf.sprintf "{%s}" @@ pprint_list l
  | PBlock Some (l) -> Printf.sprintf "(%s)" @@ pprint_list l
  | SBlock Some (l) -> Printf.sprintf "[%s]" @@ pprint_list l
  | AtRule (name, None, None) -> name ^ " () ()"
  | AtRule (name, Some(args), None) -> Printf.sprintf "%s (%s) ()" name (pprint_list args)
  | AtRule (name, None, Some(value)) -> Printf.sprintf "%s () (%s)" name (pprint value)
  | AtRule (name, Some(args), Some(value)) -> Printf.sprintf "%s (%s) (%s)" name (pprint_list args) (pprint value)
  | Comma -> "."
  | Plus -> "+"
  | Match (l, r) -> Printf.sprintf "Match(%s, %s)" (pprint l) (pprint r)
  | DashMatch (l, r) -> Printf.sprintf "DashMatch(%s, %s)" (pprint l) (pprint r)
  | StartsMatch (l, r) -> Printf.sprintf "StartsMatch(%s, %s)" (pprint l) (pprint r)
  | EndsMatch (l, r) -> Printf.sprintf "EndsMatch(%s, %s)" (pprint l) (pprint r)
  | SpaceInMatch (l, r) -> Printf.sprintf "SpaceInMatch(%s, %s)" (pprint l) (pprint r)
  | InMatch (l, r) -> Printf.sprintf "InMatch(%s, %s)" (pprint l) (pprint r)
  


and pprint_list l =
  "[" ^ (List.fold_left (fun a b -> (pprint b) ^ ", " ^ a) "" l) ^ "]"


let parse () = 
  match
    let lexbuf = Lexer.create_lexbuf @@ Sedlexing.Utf8.from_channel stdin in 
    Lexer.parse_prog lexbuf
  with
    | exception Lexer.ParseError e -> begin
      print_endline @@ Lexer.string_of_ParseError e;
      []
    end
    | stmt -> stmt

let l = parse () |> List.map (fun x -> print_endline (pprint x))
let () = Printf.printf "%d exprs" @@ List.length l

open Ast

let check_syntax expr =
  match expr with
  | Ident _ -> true
  | _ -> true

let check_syntax_list exprs =
  let results = exprs |> List.map check_syntax in
  results

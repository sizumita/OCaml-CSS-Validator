open OUnit2
open Css_validator

let parse text = 
  let lexbuf = Lexing.from_string text in
  let expr = Parser.prog Lexer.tokenize lexbuf in
  expr

let test_ident_1 _ = assert_equal (Ast.Ident "abc") (parse "abc")

let test_string_1 _ = assert_equal (Ast.String "\"abc\"") (parse "\"abc\"")
let test_string_2 _ = assert_equal (Ast.String "'abc'") (parse "'abc'")

let test_atkeyword_1 _ = assert_equal (Ast.AtKeyword "@abc") (parse "@abc")

let test_hash_1 _ = assert_equal (Ast.Hash "#abc") (parse "#abc")

let test_number_1 _ = assert_equal (Ast.Number "120") (parse "120")

let test_number_2 _ = assert_equal (Ast.Number "120.0") (parse "120.0")

let test_dimension_1 _ = assert_equal (Ast.Dimension "123abc") (parse "123abc")

let test_percentage_1 _ = assert_equal (Ast.Percentage "123%") (parse "123%")

let suite = 
  "suite">:::
  [
    "test_ident_1">:: test_ident_1;
    "test_string_1">:: test_string_1;
    "test_string_2">:: test_string_2;
    "test_atkeyword_1">:: test_atkeyword_1;
    "test_hash_1">:: test_hash_1;
    "test_number_1">:: test_number_1;
    "test_number_2">:: test_number_2;
    "test_dimension_1">:: test_dimension_1;
    "test_percentage_1">:: test_percentage_1;
  ]

let () = run_test_tt_main suite

open OUnit2
open Css_validator

let parse text = 
  let lexbuf = Lexer.create_lexbuf @@ Sedlexing.Utf8.from_string text in 
  Lexer.parse_tokens lexbuf |> List.hd
  (* let lexbuf = Lexing.from_string text in
  let expr = Parser.prog Lexer.tokenize lexbuf in
  expr *)

let parse2 text =
  let lexbuf = Lexer.create_lexbuf @@ Sedlexing.Utf8.from_string text in 
  Lexer.parse_prog lexbuf |> List.hd

let parse_file filename =
  let channel = open_in filename in
  let lexbuf = Lexer.create_lexbuf @@ Sedlexing.Utf8.from_channel channel in
  let _ = close_in channel in
  Lexer.parse_prog lexbuf

let test_ident_1 _ = assert_equal (Ast.Ident "abc") (parse "abc")
let test_ident_2 _ = assert_equal (Ast.Ident "-abc") (parse "-abc")

let test_string_1 _ = assert_equal (Ast.String "\"abc\"") (parse "\"abc\"")
let test_string_2 _ = assert_equal (Ast.String "'abc'") (parse "'abc'")

let test_atkeyword_1 _ = assert_equal (Ast.AtKeyword "@abc") (parse "@abc")

let test_hash_1 _ = assert_equal (Ast.Hash "#abc") (parse "#abc")

let test_number_1 _ = assert_equal (Ast.Number "120") (parse "120")

let test_number_2 _ = assert_equal (Ast.Number "120.0") (parse "120.0")

let test_number_3 _ = assert_equal (Ast.Number ".3") (parse ".3")

let test_number_4 _ = assert_equal (Ast.Number "12e5") (parse "12e5")
let test_number_5 _ = assert_equal (Ast.Number "12E6") (parse "12E6")

let test_dimension_1 _ = assert_equal (Ast.Dimension "123abc") (parse "123abc")

let test_percentage_1 _ = assert_equal (Ast.Percentage "123%") (parse "123%")

let test_comment_1 _ = assert_equal (Ast.Number "123") (parse "/* abc */ 123")

let test_uri_1 _ = assert_equal (Ast.Uri "url(http://example.com/abc.png)") (parse "url(http://example.com/abc.png)")

let test_uri_2 _ = assert_equal (Ast.Uri "url(\"http://example.com/abc.png\")") (parse "url(\"http://example.com/abc.png\")")

let test_uri_3 _ = assert_equal (Ast.Uri "url(\'http://example.com/abc.png\')") (parse "url(\'http://example.com/abc.png\')")

let test_unicode_range_1 _ = assert_equal (Ast.UnicodeRange "u+123456") (parse "u+123456")
let test_unicode_range_2 _ = assert_equal (Ast.UnicodeRange "u+123?") (parse "u+123?")

let test_unicode_range_3 _ = assert_equal (Ast.UnicodeRange "U+0-7F") (parse "U+0-7F")
let test_unicode_range_4 _ = assert_equal (Ast.UnicodeRange "U+0025-00FF") (parse "U+0025-00FF")

let suite = 
  "suite">:::
  [
    "test_ident_1">:: test_ident_1;
    "test_ident_2">:: test_ident_2;
    "test_string_1">:: test_string_1;
    "test_string_2">:: test_string_2;
    "test_atkeyword_1">:: test_atkeyword_1;
    "test_hash_1">:: test_hash_1;
    "test_number_1">:: test_number_1;
    "test_number_2">:: test_number_2;
    "test_number_3">:: test_number_3;
    "test_number_4">:: test_number_4;
    "test_number_5">:: test_number_5;
    "test_dimension_1">:: test_dimension_1;
    "test_percentage_1">:: test_percentage_1;
    "test_comment_1">:: test_comment_1;
    "test_url_1">:: test_uri_1;
    "test_url_2">:: test_uri_2;
    "test_url_3">:: test_uri_3;
    "test_unicode_range_1">:: test_unicode_range_1;
    "test_unicode_range_2">:: test_unicode_range_2;
    "test_unicode_range_3">:: test_unicode_range_3;
    "test_unicode_range_4">:: test_unicode_range_4;
  ]

let () = run_test_tt_main suite

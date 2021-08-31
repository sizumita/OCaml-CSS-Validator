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
  let channel = open_in ("../../../test/css/" ^ filename) in
  let lexbuf = Lexer.create_lexbuf @@ Sedlexing.Utf8.from_channel channel in
  let _ = Lexer.parse_prog lexbuf in true

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

let test_adjacent_sibling_combinator _ = assert_equal true (parse_file "adjacent_sibling_combinator.css")

let test_ID_selector _ = assert_equal true (parse_file "ID_selector.css")

let test_attribute_selector _ = assert_equal true (parse_file "attribute_selector.css")

let test_child_combinator _ = assert_equal true (parse_file "child_combinator.css")

let test_class_selector _ = assert_equal true (parse_file "class_selector.css")

let test_descendant_combinator _ = assert_equal true (parse_file "descendant_combinator.css")

let test_general_sibling_combinator _ = assert_equal true (parse_file "general_sibling_combinator.css")

let test_selector_list _ = assert_equal true (parse_file "selector_list.css")

let test_type_selector _ = assert_equal true (parse_file "type_selector.css")

let test_universal_selector _ = assert_equal true (parse_file "universal_selector.css")

let test_pseudo_class _ = assert_equal true (parse_file "pseudo_class.css")

let test_pseudo_element _ = assert_equal true (parse_file "pseudo_element.css")

let test_at_rule _ = assert_equal true (parse_file "at_rule.css")

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
    "test_adjacent_sibling_combinator">:: test_adjacent_sibling_combinator;
    "test_ID_selector">:: test_ID_selector;
    "test_attribute_selector">:: test_attribute_selector;
    "test_child_combinator">:: test_child_combinator;
    "test_class_selector">:: test_class_selector;
    "test_descendant_combinator">:: test_descendant_combinator;
    "test_general_sibling_combinator">:: test_general_sibling_combinator;
    "test_selector_list">:: test_selector_list;
    "test_type_selector">:: test_type_selector;
    "test_universal_selector">:: test_universal_selector;
    "test_pseudo_class">:: test_pseudo_class;
    "test_pseudo_element">:: test_pseudo_element;
    "test_at_rule">:: test_at_rule;
  ]

let () = run_test_tt_main suite

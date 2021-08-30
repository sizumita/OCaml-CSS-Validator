%{
open Ast
%}

%token <string> IDENT ATKEYWORD STRING HASH PERCENTAGE NUMBER DIMENSION URI UNICODE_RANGE FUNCTION
%token CDO CDC COLON SEMICOLON INCLUDES DASHMATCH
%token LP RP LB RB LS RS S
%token EOF


%start <Ast.expr list> prog

%start <Ast.expr list> tokens

%%

tokens:
  | tokens = list(token) EOF {tokens}

token:
  | s = PERCENTAGE { Percentage s }
  | s = STRING { String s }
  | s = IDENT EOF { Ident s }
  | s = HASH EOF { Hash s }
  | s = ATKEYWORD { AtKeyword s }
  | s = NUMBER { Number s }
  | s = DIMENSION { Dimension s }
  | s = URI { Uri s }
  | s = UNICODE_RANGE { UnicodeRange s }

prog:
  | statements = list(stylesheet) EOF {statements}

stylesheet:
  | CDO { Cdo }
  | CDC { Cdc }
  | set = ruleset { set }

ruleset:
  | selector_ = option(selector) LB RB { RuleSet (selector_, []) }
  | selector_ = option(selector) LB list = declaration_list; RB { RuleSet (selector_, list) }

declaration_list:
  | values = list(declaration) { values }

selector:
  | values = list(any) { Selector values }

declaration:
  | property = IDENT; COLON value = declaration_value; SEMICOLON { Declaration (Ident property, value) }

declaration_value:
  | value = any { value }
  | value = ATKEYWORD { AtKeyword value }

any:
  | value = IDENT { Ident value }
  | value = NUMBER { Number value }
  | value = PERCENTAGE { Percentage value }
  | value = DIMENSION { Dimension value }
  | value = STRING { String value }
  | value = URI { Uri value }
  | value = HASH { Hash value }
  | value = UNICODE_RANGE { UnicodeRange value }
  | INCLUDES { Includes }
  | DASHMATCH { DashMatch }
  | COLON { Colon }
  | value = FUNCTION; RP { Function (value, None)}





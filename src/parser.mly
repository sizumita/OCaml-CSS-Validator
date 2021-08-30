%{
open Ast
%}

%token <string> IDENT ATKEYWORD STRING HASH PERCENTAGE NUMBER DIMENSION URL UNICODE_RANGE
%token CDO CDC COLON SEMICOLON INCLUDES DASHMATCH
%token LP RP LB RB LS RS S
%token EOF


%start <Ast.expr list> prog

%%

prog:
  | statements = list(statement) EOF {statements}

statement:
  | s = PERCENTAGE { Percentage s }
  | s = STRING { String s }
  | s = IDENT EOF { Ident s }
  | s = HASH EOF { Hash s }
  | s = ATKEYWORD { AtKeyword s }
  | s = NUMBER { Number s }
  | s = DIMENSION { Dimension s }
  | s = URL { Url s }
  | s = UNICODE_RANGE { UnicodeRange s }

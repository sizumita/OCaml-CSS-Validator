%{
open Ast
%}

%token <string> IDENT ATKEYWORD STRING HASH PERCENTAGE NUMBER DIMENSION
%token CDO CDC COLON SEMICOLON INCLUDES DASHMATCH
%token LP RP LB RB LS RS S
%token EOF


%start <Ast.expr> prog

%%

prog:
  | s = PERCENTAGE { Percentage s }
  | s = STRING { String s }
  | s = IDENT EOF { Ident s }
  | s = HASH EOF { Hash s }
  | s = ATKEYWORD { AtKeyword s }
  | s = NUMBER { Number s }
  | s = DIMENSION { Dimension s }

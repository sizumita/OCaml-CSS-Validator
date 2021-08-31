%{
open Ast
%}

%token <string> IDENT ATKEYWORD STRING HASH PERCENTAGE NUMBER DIMENSION URI UNICODE_RANGE FUNCTION
%token CDO CDC COLON SEMICOLON
// = |= ~= ^= $= *=
%token MATCH DASHMATCH SPACEINMATCH STARTSMATCH ENDSMATCH INMATCH
%token COMMA PLUS
%token LP RP LB RB LS RS
%token EOF


%start <Ast.expr list> prog

%start <Ast.expr list> tokens

%%

tokens:
  | tokens = list(token) EOF {tokens}

token:
  | s = PERCENTAGE { Percentage s }
  | s = STRING { String s }
  | s = IDENT { Ident s }
  | s = HASH { Hash s }
  | s = ATKEYWORD { AtKeyword s }
  | s = NUMBER { Number s }
  | s = DIMENSION { Dimension s }
  | s = URI { Uri s }
  | s = UNICODE_RANGE { UnicodeRange s }

prog:
  | statements = list(stylesheet) EOF {statements}
  | error
    { failwith 
          (Printf.sprintf "parse error at line %d column %d"
              ($startpos.pos_lnum) ($startpos.pos_cnum - $startpos.pos_bol)
              )}

stylesheet:
  | CDO { Cdo }
  | CDC { Cdc }
  | stmt = statement { stmt }

statement:
  | set = ruleset { set }
  | rule = at_rule { rule }

// @media screen {...}
at_rule:
  | name = ATKEYWORD; component = option(list(component_value)) SEMICOLON; { AtRule (name, component, None) }
  | name = ATKEYWORD; component = option(list(component_value)) LB set = option(list(at_rule_value)); RB { AtRule (name, component, Some(Block set)) }

at_rule_value:
  | set = ruleset { set }
  | rule = at_rule { rule }

matchs:
  | l = component_value MATCH r = component_value { Match (l, r) }
  | l = component_value DASHMATCH r = component_value { DashMatch (l, r) }
  | l = component_value SPACEINMATCH r = component_value { SpaceInMatch (l, r) }
  | l = component_value STARTSMATCH r = component_value { StartsMatch (l, r) }
  | l = component_value ENDSMATCH r = component_value { EndsMatch (l, r) }
  | l = component_value INMATCH r = component_value { InMatch (l, r) }

component_value:
  | value = IDENT { Ident value }
  | value = STRING { String value }
  | value = PERCENTAGE { Percentage value }
  | value = HASH { Hash value }
  | value = NUMBER { Number value }
  | value = DIMENSION { Dimension value }
  | value = URI { Uri value }
  | value = UNICODE_RANGE { UnicodeRange value }
  | match_ = matchs { match_ }
  | COMMA { Comma }
  | PLUS { Plus }
  | COLON { Colon }
  | LP components = option(list(component_value)); RP { PBlock components }
  | LS components = option(list(component_value)); RS { SBlock components }
  | name = FUNCTION; components = option(list(component_value)); RP { Function (name, components) }
  | declaration_ = declaration { declaration_ }

// div {...}
ruleset:
  | component = option(list(component_value)); LB value = option(declaration_list); RB { RuleSet (component, value) }

ruleset_value:
  | IDENT COLON IDENT SEMICOLON { Null }

declaration_list:
  | first = declaration SEMICOLON rest = declaration_list { first :: rest }
  | first = declaration option(SEMICOLON) { [first] }

declaration:
  | prop = IDENT; COLON value = list(component_value) { Declaration (Ident prop, value) }
  | prop = IDENT; COLON block_ = block { Declaration (Ident prop, [block_])}

block:
  | LB components = option(list(component_value)); RB { Block components }



%{
open Ast
%}

%token <string> IDENT ATKEYWORD STRING HASH PERCENTAGE NUMBER DIMENSION URI UNICODE_RANGE FUNCTION
%token CDO CDC COLON SEMICOLON IMPORTANT
// = |= ~= ^= $= *=
%token MATCH DASHMATCH SPACEINMATCH STARTSMATCH ENDSMATCH INMATCH
%token <string> CHARSET
%token COMMA PLUS CHILD DOT SIBILING UNIVERSAL
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

charset:
  | value = CHARSET { Charset value }

// @media screen {...}
at_rule:
  | value = charset { value }
  | name = ATKEYWORD; component = option(list(at_rule_component_value)) SEMICOLON; { AtRule (name, component, None) }
  | name = ATKEYWORD; component = option(list(at_rule_component_value)) LB set = declaration_list; RB { AtRule (name, component, Some(Block set)) }
  | name = ATKEYWORD; component = option(list(at_rule_component_value)) LB set = list(at_rule_value); RB { AtRule (name, component, Some(Block set)) }
  | name = ATKEYWORD; component = option(list(at_rule_component_value)) LB RB { AtRule (name, component, Some(Block [])) }

at_rule_component_value:
  | COLON value = pseudo_value { PseudoClass value }
  | value = STRING { String value }
  | value = IDENT { Ident value }
  | LS declaration_ = declaration; RS { SBlock [declaration_] }
  | LP declaration_ = declaration; RP { SBlock [declaration_] }
  | uri = URI { Uri uri }
  | func = function_ { func }

at_rule_value:
  | set = ruleset { set }
  | rule = at_rule { rule }

component_value:
  | DOT value = IDENT { ClassName value }
  | value = IDENT { Ident value }
  | value = STRING { String value }
  | value = PERCENTAGE { Percentage value }
  | value = HASH { Hash value }
  | value = NUMBER { Number value }
  | value = DIMENSION { Dimension value }
  | value = URI { Uri value }
  | value = UNICODE_RANGE { UnicodeRange value }
  | match_ = matchs { match_ }
  | uri = URI { Uri uri }
  | COLON COLON component = component_value { PseudoElements component }
  | COLON component = component_value { PseudoClass component }
  | COMMA { Comma }
  | PLUS { Plus }
  | COLON { Colon }
  | CHILD { Child }
  | SIBILING { Sibiling }
  | UNIVERSAL { Universal }
  | LP RP { PBlock [] }
  | LS RS { SBlock [] }
  | IMPORTANT { Important }
  | LP components = list(component_value); RP { PBlock components }
  | LS components = list(component_value); RS { SBlock components }
  | LS declaration_ = declaration; RS { SBlock [declaration_] }
  | func = function_ { func }

// div {...}
ruleset:
  | component = option(list(ruleset_component_value)); LB value = option(declaration_list); RB { RuleSet (component, value) }

ruleset_component_value:
  | DOT value = IDENT { ClassName value }
  | value = IDENT { Ident value }
  | value = HASH { Hash value }
  | match_ = matchs { match_ }
  | COLON COLON value = pseudo_value { PseudoElements value }
  | COLON value = pseudo_value { PseudoClass value }
  | COMMA { Comma }
  | PLUS { Plus }
  | COLON { Colon }
  | CHILD { Child }
  | SIBILING { Sibiling }
  | UNIVERSAL { Universal }
  | LS RS { SBlock [] }
  | attr = attribute; { attr }
  | LS declaration_ = declaration; RS { SBlock [declaration_] }

attribute:
  | LS match_ = matchs; RS { Attribute match_ }
  | LS value = IDENT; RS { Attribute (Ident value) }

ruleset_value:
  | IDENT COLON IDENT SEMICOLON { Null }

pseudo_value:
  | func = function_ { func }
  | value = IDENT { Ident value }

matchs:
  | l = component_value; MATCH r = component_value; case = option(IDENT) { Match (l, r, case) }
  | l = component_value; DASHMATCH r = component_value; case = option(IDENT) { DashMatch (l, r, case) }
  | l = component_value; SPACEINMATCH r = component_value; case = option(IDENT) { SpaceInMatch (l, r, case) }
  | l = component_value; STARTSMATCH r = component_value; case = option(IDENT) { StartsMatch (l, r, case) }
  | l = component_value; ENDSMATCH r = component_value; case = option(IDENT) { EndsMatch (l, r, case) }
  | l = component_value; INMATCH r = component_value; case = option(IDENT) { InMatch (l, r, case) }

declaration_list:
  | first = declaration SEMICOLON rest = declaration_list { first :: rest }
  | first = declaration option(SEMICOLON) { [first] }

declaration:
  | prop = IDENT; COLON value = list(component_value) { Declaration (Ident prop, value) }
  | prop = IDENT; COLON block_ = block { Declaration (Ident prop, [block_])}

block:
  | LB components = list(component_value); RB { Block components }
  | LB RB { Block [] }

function_:
  | name = FUNCTION; components = option(list(component_value)); RP { Function (name, components) }

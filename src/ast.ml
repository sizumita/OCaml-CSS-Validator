type expr =
  | Ident of string
  | String of string
  | AtKeyword of string
  | Hash of string
  | Number of string
  | Dimension of string
  | Percentage of string
  | Uri of string
  | UnicodeRange of string
  | Cdo
  | Cdc
  | Null
  | Includes
  | DashMatch
  | Colon
  | SemiColon

  | Selector of expr list
  | RuleSet of expr option * expr list
  | Declaration of expr * expr
  | Function of string * expr list option

  [@@deriving show]

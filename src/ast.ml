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
  | Colon
  | SemiColon
  | Comma
  | Plus

  | Selector of expr list
  | RuleSet of expr list option * expr list option
  | Declaration of expr * expr list
  | Function of string * expr list option
  | Block of expr list option
  | PBlock of expr list option
  | SBlock of expr list option
  | AtRule of string * expr list option * expr option
  | Match of expr * expr
  | DashMatch of expr * expr
  | SpaceInMatch of expr * expr
  | StartsMatch of expr * expr
  | EndsMatch of expr * expr
  | InMatch of expr * expr

  [@@deriving show]

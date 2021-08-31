type expr =
  | Ident of string
  | String of string
  | Hash of string
  | Number of string
  | Dimension of string
  | Percentage of string
  | Uri of string
  | UnicodeRange of string
  | Cdo
  | Cdc
  | Colon
  | SemiColon
  | Comma
  | Plus
  | Child
  | Sibiling
  | Universal

  | ClassName of string
  | PseudoClass of expr
  | PseudoElements of expr
  | Selector of expr list
  | RuleSet of expr list option * expr list option
  | Declaration of expr * expr list
  | Function of string * expr list option
  | Block of expr list
  | PBlock of expr list
  | SBlock of expr list
  | AtRule of string * expr list option * expr option
  | Match of expr * expr * string option
  | DashMatch of expr * expr * string option
  | SpaceInMatch of expr * expr * string option
  | StartsMatch of expr * expr * string option
  | EndsMatch of expr * expr * string option
  | InMatch of expr * expr * string option

  [@@deriving show]

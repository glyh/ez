open Ez_ir

type placeholder = unit
type identifier = string

type expr0 =
  | BinOp of expr_binary_bin_op * expr0 * expr0
  | Val of value
  | Var of identifier
  | Call of identifier * expr0 list

type stmt0 =
  | Expr of expr0
  | Declaration of ez_type * identifier * expr0
  | If of expr0 * stmt0 * stmt0
  | Block of stmt0 list
  | While of expr0 * stmt0
  | Return of expr0
  | Assign of identifier * expr0

type def0 =
  | Function of ez_type * identifier * ez_typed_param list * stmt0
  | Extern of ez_type * identifier * ez_typed_param list
  | Require of string

type prog0 = def0 list

type def_precheck =
  | Function of ez_type * identifier * ez_typed_param list * stmt0
  | Extern of ez_type * identifier * ez_typed_param list

type prog_precheck = def_precheck list

let binop_to_string (binop : expr_binary_bin_op) =
  match binop with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Noteq -> "!="
  | Lessthan -> "<"
  | Lesseq -> "<="
  | Greaterthan -> ">"
  | Greatereq -> ">="
  | Land -> "and"
  | Lor -> "or"

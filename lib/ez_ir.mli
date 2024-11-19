
(** Code for ez_ir.proto *)

(* generated from "./proto/ez_ir.proto", do not edit *)



(** {2 Types} *)

type ez_type_non_ptr =
  | Void 
  | I64 
  | Str 
  | F64 
  | Bool 

type ez_type =
  | Non_ptr of ez_type_non_ptr
  | Ptr of ez_type

type value =
  | I64_val of int64
  | Str_val of string
  | F64_val of float
  | Bool_val of bool

type expr_binary_bin_op =
  | Add 
  | Sub 
  | Mul 
  | Div 
  | Mod 
  | Eq 
  | Noteq 
  | Lessthan 
  | Lesseq 
  | Greaterthan 
  | Greatereq 
  | Land 
  | Lor 

type expr_unary_un_op =
  | Not 
  | Ref 
  | Deref 

type expr_assign = {
  name : string;
  rhs : expr;
}

and expr_kind =
  | Assign of expr_assign
  | Binary of expr_binary
  | Unary of expr_unary
  | Value of value
  | Variable of string
  | Call of expr_call

and expr = {
  kind : expr_kind;
  expr_type : ez_type;
}

and expr_binary = {
  op : expr_binary_bin_op;
  lhs : expr;
  rhs : expr;
}

and expr_unary = {
  op : expr_unary_un_op;
  inner : expr;
}

and expr_call = {
  callee : string;
  args : expr list;
}

type statement_declaration = {
  name : string;
  rhs : expr;
}

type statement_if = {
  condition : expr;
  then_branch : statement;
  else_branch : statement;
}

and statement =
  | Expr of expr
  | Declaration of statement_declaration
  | If of statement_if
  | Block of statement_block
  | For of statement_for
  | Return of expr

and statement_block = {
  statements : statement list;
}

and statement_for = {
  initializer_ : expr;
  condition : expr;
  increment : expr;
  body : statement;
}

type definition_ez_typed_arg = {
  arg_type : ez_type;
  name : string;
}

type definition = {
  return_type : ez_type;
  name : string;
  args : definition_ez_typed_arg list;
  body : statement;
}

type program = {
  definitions : definition list;
}


(** {2 Basic values} *)

val default_ez_type_non_ptr : unit -> ez_type_non_ptr
(** [default_ez_type_non_ptr ()] is the default value for type [ez_type_non_ptr] *)

val default_ez_type : unit -> ez_type
(** [default_ez_type ()] is the default value for type [ez_type] *)

val default_value : unit -> value
(** [default_value ()] is the default value for type [value] *)

val default_expr_binary_bin_op : unit -> expr_binary_bin_op
(** [default_expr_binary_bin_op ()] is the default value for type [expr_binary_bin_op] *)

val default_expr_unary_un_op : unit -> expr_unary_un_op
(** [default_expr_unary_un_op ()] is the default value for type [expr_unary_un_op] *)

val default_expr_assign : 
  ?name:string ->
  ?rhs:expr ->
  unit ->
  expr_assign
(** [default_expr_assign ()] is the default value for type [expr_assign] *)

val default_expr_kind : unit -> expr_kind
(** [default_expr_kind ()] is the default value for type [expr_kind] *)

val default_expr : 
  ?kind:expr_kind ->
  ?expr_type:ez_type ->
  unit ->
  expr
(** [default_expr ()] is the default value for type [expr] *)

val default_expr_binary : 
  ?op:expr_binary_bin_op ->
  ?lhs:expr ->
  ?rhs:expr ->
  unit ->
  expr_binary
(** [default_expr_binary ()] is the default value for type [expr_binary] *)

val default_expr_unary : 
  ?op:expr_unary_un_op ->
  ?inner:expr ->
  unit ->
  expr_unary
(** [default_expr_unary ()] is the default value for type [expr_unary] *)

val default_expr_call : 
  ?callee:string ->
  ?args:expr list ->
  unit ->
  expr_call
(** [default_expr_call ()] is the default value for type [expr_call] *)

val default_statement_declaration : 
  ?name:string ->
  ?rhs:expr ->
  unit ->
  statement_declaration
(** [default_statement_declaration ()] is the default value for type [statement_declaration] *)

val default_statement_if : 
  ?condition:expr ->
  ?then_branch:statement ->
  ?else_branch:statement ->
  unit ->
  statement_if
(** [default_statement_if ()] is the default value for type [statement_if] *)

val default_statement : unit -> statement
(** [default_statement ()] is the default value for type [statement] *)

val default_statement_block : 
  ?statements:statement list ->
  unit ->
  statement_block
(** [default_statement_block ()] is the default value for type [statement_block] *)

val default_statement_for : 
  ?initializer_:expr ->
  ?condition:expr ->
  ?increment:expr ->
  ?body:statement ->
  unit ->
  statement_for
(** [default_statement_for ()] is the default value for type [statement_for] *)

val default_definition_ez_typed_arg : 
  ?arg_type:ez_type ->
  ?name:string ->
  unit ->
  definition_ez_typed_arg
(** [default_definition_ez_typed_arg ()] is the default value for type [definition_ez_typed_arg] *)

val default_definition : 
  ?return_type:ez_type ->
  ?name:string ->
  ?args:definition_ez_typed_arg list ->
  ?body:statement ->
  unit ->
  definition
(** [default_definition ()] is the default value for type [definition] *)

val default_program : 
  ?definitions:definition list ->
  unit ->
  program
(** [default_program ()] is the default value for type [program] *)


(** {2 Protobuf Encoding} *)

val encode_pb_ez_type_non_ptr : ez_type_non_ptr -> Pbrt.Encoder.t -> unit
(** [encode_pb_ez_type_non_ptr v encoder] encodes [v] with the given [encoder] *)

val encode_pb_ez_type : ez_type -> Pbrt.Encoder.t -> unit
(** [encode_pb_ez_type v encoder] encodes [v] with the given [encoder] *)

val encode_pb_value : value -> Pbrt.Encoder.t -> unit
(** [encode_pb_value v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr_binary_bin_op : expr_binary_bin_op -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr_binary_bin_op v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr_unary_un_op : expr_unary_un_op -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr_unary_un_op v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr_assign : expr_assign -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr_assign v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr_kind : expr_kind -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr_kind v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr : expr -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr_binary : expr_binary -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr_binary v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr_unary : expr_unary -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr_unary v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr_call : expr_call -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr_call v encoder] encodes [v] with the given [encoder] *)

val encode_pb_statement_declaration : statement_declaration -> Pbrt.Encoder.t -> unit
(** [encode_pb_statement_declaration v encoder] encodes [v] with the given [encoder] *)

val encode_pb_statement_if : statement_if -> Pbrt.Encoder.t -> unit
(** [encode_pb_statement_if v encoder] encodes [v] with the given [encoder] *)

val encode_pb_statement : statement -> Pbrt.Encoder.t -> unit
(** [encode_pb_statement v encoder] encodes [v] with the given [encoder] *)

val encode_pb_statement_block : statement_block -> Pbrt.Encoder.t -> unit
(** [encode_pb_statement_block v encoder] encodes [v] with the given [encoder] *)

val encode_pb_statement_for : statement_for -> Pbrt.Encoder.t -> unit
(** [encode_pb_statement_for v encoder] encodes [v] with the given [encoder] *)

val encode_pb_definition_ez_typed_arg : definition_ez_typed_arg -> Pbrt.Encoder.t -> unit
(** [encode_pb_definition_ez_typed_arg v encoder] encodes [v] with the given [encoder] *)

val encode_pb_definition : definition -> Pbrt.Encoder.t -> unit
(** [encode_pb_definition v encoder] encodes [v] with the given [encoder] *)

val encode_pb_program : program -> Pbrt.Encoder.t -> unit
(** [encode_pb_program v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_ez_type_non_ptr : Pbrt.Decoder.t -> ez_type_non_ptr
(** [decode_pb_ez_type_non_ptr decoder] decodes a [ez_type_non_ptr] binary value from [decoder] *)

val decode_pb_ez_type : Pbrt.Decoder.t -> ez_type
(** [decode_pb_ez_type decoder] decodes a [ez_type] binary value from [decoder] *)

val decode_pb_value : Pbrt.Decoder.t -> value
(** [decode_pb_value decoder] decodes a [value] binary value from [decoder] *)

val decode_pb_expr_binary_bin_op : Pbrt.Decoder.t -> expr_binary_bin_op
(** [decode_pb_expr_binary_bin_op decoder] decodes a [expr_binary_bin_op] binary value from [decoder] *)

val decode_pb_expr_unary_un_op : Pbrt.Decoder.t -> expr_unary_un_op
(** [decode_pb_expr_unary_un_op decoder] decodes a [expr_unary_un_op] binary value from [decoder] *)

val decode_pb_expr_assign : Pbrt.Decoder.t -> expr_assign
(** [decode_pb_expr_assign decoder] decodes a [expr_assign] binary value from [decoder] *)

val decode_pb_expr_kind : Pbrt.Decoder.t -> expr_kind
(** [decode_pb_expr_kind decoder] decodes a [expr_kind] binary value from [decoder] *)

val decode_pb_expr : Pbrt.Decoder.t -> expr
(** [decode_pb_expr decoder] decodes a [expr] binary value from [decoder] *)

val decode_pb_expr_binary : Pbrt.Decoder.t -> expr_binary
(** [decode_pb_expr_binary decoder] decodes a [expr_binary] binary value from [decoder] *)

val decode_pb_expr_unary : Pbrt.Decoder.t -> expr_unary
(** [decode_pb_expr_unary decoder] decodes a [expr_unary] binary value from [decoder] *)

val decode_pb_expr_call : Pbrt.Decoder.t -> expr_call
(** [decode_pb_expr_call decoder] decodes a [expr_call] binary value from [decoder] *)

val decode_pb_statement_declaration : Pbrt.Decoder.t -> statement_declaration
(** [decode_pb_statement_declaration decoder] decodes a [statement_declaration] binary value from [decoder] *)

val decode_pb_statement_if : Pbrt.Decoder.t -> statement_if
(** [decode_pb_statement_if decoder] decodes a [statement_if] binary value from [decoder] *)

val decode_pb_statement : Pbrt.Decoder.t -> statement
(** [decode_pb_statement decoder] decodes a [statement] binary value from [decoder] *)

val decode_pb_statement_block : Pbrt.Decoder.t -> statement_block
(** [decode_pb_statement_block decoder] decodes a [statement_block] binary value from [decoder] *)

val decode_pb_statement_for : Pbrt.Decoder.t -> statement_for
(** [decode_pb_statement_for decoder] decodes a [statement_for] binary value from [decoder] *)

val decode_pb_definition_ez_typed_arg : Pbrt.Decoder.t -> definition_ez_typed_arg
(** [decode_pb_definition_ez_typed_arg decoder] decodes a [definition_ez_typed_arg] binary value from [decoder] *)

val decode_pb_definition : Pbrt.Decoder.t -> definition
(** [decode_pb_definition decoder] decodes a [definition] binary value from [decoder] *)

val decode_pb_program : Pbrt.Decoder.t -> program
(** [decode_pb_program decoder] decodes a [program] binary value from [decoder] *)

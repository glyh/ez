
(** Code for ez_ir.proto *)

(* generated from "./proto/ez_ir.proto", do not edit *)



(** {2 Types} *)

type ez_type_non_ptr =
  | Unit 
  | I64 
  | Str 
  | F64 
  | Bool 

type ez_type =
  | Non_ptr of ez_type_non_ptr
  | Ptr of ez_type

type value_unit = unit

type value =
  | Unit_val
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

type expr_binary = {
  op : expr_binary_bin_op;
  lhs : expr;
  rhs : expr;
}

and expr_kind =
  | Binary of expr_binary
  | Value of value
  | Variable of string
  | Call of expr_call

and expr = {
  kind : expr_kind;
  expr_type : ez_type;
}

and expr_call = {
  callee : string;
  args : expr list;
}

type statement_declaration = {
  name : string;
  type_ : ez_type;
  rhs : expr;
}

type statement_assign = {
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
  | While of statement_while
  | Return of expr
  | Assign of statement_assign

and statement_block = {
  statements : statement list;
}

and statement_while = {
  condition : expr;
  body : statement;
}

type ez_typed_param = {
  param_type : ez_type;
  name : string;
}

type function_ = {
  return_type : ez_type;
  name : string;
  params : ez_typed_param list;
  body : statement;
}

type extern = {
  return_type : ez_type;
  name : string;
  params : ez_typed_param list;
}

type definition =
  | Func of function_
  | Extern of extern

type program = {
  definitions : definition list;
}


(** {2 Basic values} *)

val default_ez_type_non_ptr : unit -> ez_type_non_ptr
(** [default_ez_type_non_ptr ()] is the default value for type [ez_type_non_ptr] *)

val default_ez_type : unit -> ez_type
(** [default_ez_type ()] is the default value for type [ez_type] *)

val default_value_unit : unit
(** [default_value_unit ()] is the default value for type [value_unit] *)

val default_value : unit -> value
(** [default_value ()] is the default value for type [value] *)

val default_expr_binary_bin_op : unit -> expr_binary_bin_op
(** [default_expr_binary_bin_op ()] is the default value for type [expr_binary_bin_op] *)

val default_expr_binary : 
  ?op:expr_binary_bin_op ->
  ?lhs:expr ->
  ?rhs:expr ->
  unit ->
  expr_binary
(** [default_expr_binary ()] is the default value for type [expr_binary] *)

val default_expr_kind : unit -> expr_kind
(** [default_expr_kind ()] is the default value for type [expr_kind] *)

val default_expr : 
  ?kind:expr_kind ->
  ?expr_type:ez_type ->
  unit ->
  expr
(** [default_expr ()] is the default value for type [expr] *)

val default_expr_call : 
  ?callee:string ->
  ?args:expr list ->
  unit ->
  expr_call
(** [default_expr_call ()] is the default value for type [expr_call] *)

val default_statement_declaration : 
  ?name:string ->
  ?type_:ez_type ->
  ?rhs:expr ->
  unit ->
  statement_declaration
(** [default_statement_declaration ()] is the default value for type [statement_declaration] *)

val default_statement_assign : 
  ?name:string ->
  ?rhs:expr ->
  unit ->
  statement_assign
(** [default_statement_assign ()] is the default value for type [statement_assign] *)

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

val default_statement_while : 
  ?condition:expr ->
  ?body:statement ->
  unit ->
  statement_while
(** [default_statement_while ()] is the default value for type [statement_while] *)

val default_ez_typed_param : 
  ?param_type:ez_type ->
  ?name:string ->
  unit ->
  ez_typed_param
(** [default_ez_typed_param ()] is the default value for type [ez_typed_param] *)

val default_function_ : 
  ?return_type:ez_type ->
  ?name:string ->
  ?params:ez_typed_param list ->
  ?body:statement ->
  unit ->
  function_
(** [default_function_ ()] is the default value for type [function_] *)

val default_extern : 
  ?return_type:ez_type ->
  ?name:string ->
  ?params:ez_typed_param list ->
  unit ->
  extern
(** [default_extern ()] is the default value for type [extern] *)

val default_definition : unit -> definition
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

val encode_pb_value_unit : value_unit -> Pbrt.Encoder.t -> unit
(** [encode_pb_value_unit v encoder] encodes [v] with the given [encoder] *)

val encode_pb_value : value -> Pbrt.Encoder.t -> unit
(** [encode_pb_value v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr_binary_bin_op : expr_binary_bin_op -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr_binary_bin_op v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr_binary : expr_binary -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr_binary v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr_kind : expr_kind -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr_kind v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr : expr -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr v encoder] encodes [v] with the given [encoder] *)

val encode_pb_expr_call : expr_call -> Pbrt.Encoder.t -> unit
(** [encode_pb_expr_call v encoder] encodes [v] with the given [encoder] *)

val encode_pb_statement_declaration : statement_declaration -> Pbrt.Encoder.t -> unit
(** [encode_pb_statement_declaration v encoder] encodes [v] with the given [encoder] *)

val encode_pb_statement_assign : statement_assign -> Pbrt.Encoder.t -> unit
(** [encode_pb_statement_assign v encoder] encodes [v] with the given [encoder] *)

val encode_pb_statement_if : statement_if -> Pbrt.Encoder.t -> unit
(** [encode_pb_statement_if v encoder] encodes [v] with the given [encoder] *)

val encode_pb_statement : statement -> Pbrt.Encoder.t -> unit
(** [encode_pb_statement v encoder] encodes [v] with the given [encoder] *)

val encode_pb_statement_block : statement_block -> Pbrt.Encoder.t -> unit
(** [encode_pb_statement_block v encoder] encodes [v] with the given [encoder] *)

val encode_pb_statement_while : statement_while -> Pbrt.Encoder.t -> unit
(** [encode_pb_statement_while v encoder] encodes [v] with the given [encoder] *)

val encode_pb_ez_typed_param : ez_typed_param -> Pbrt.Encoder.t -> unit
(** [encode_pb_ez_typed_param v encoder] encodes [v] with the given [encoder] *)

val encode_pb_function_ : function_ -> Pbrt.Encoder.t -> unit
(** [encode_pb_function_ v encoder] encodes [v] with the given [encoder] *)

val encode_pb_extern : extern -> Pbrt.Encoder.t -> unit
(** [encode_pb_extern v encoder] encodes [v] with the given [encoder] *)

val encode_pb_definition : definition -> Pbrt.Encoder.t -> unit
(** [encode_pb_definition v encoder] encodes [v] with the given [encoder] *)

val encode_pb_program : program -> Pbrt.Encoder.t -> unit
(** [encode_pb_program v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_ez_type_non_ptr : Pbrt.Decoder.t -> ez_type_non_ptr
(** [decode_pb_ez_type_non_ptr decoder] decodes a [ez_type_non_ptr] binary value from [decoder] *)

val decode_pb_ez_type : Pbrt.Decoder.t -> ez_type
(** [decode_pb_ez_type decoder] decodes a [ez_type] binary value from [decoder] *)

val decode_pb_value_unit : Pbrt.Decoder.t -> value_unit
(** [decode_pb_value_unit decoder] decodes a [value_unit] binary value from [decoder] *)

val decode_pb_value : Pbrt.Decoder.t -> value
(** [decode_pb_value decoder] decodes a [value] binary value from [decoder] *)

val decode_pb_expr_binary_bin_op : Pbrt.Decoder.t -> expr_binary_bin_op
(** [decode_pb_expr_binary_bin_op decoder] decodes a [expr_binary_bin_op] binary value from [decoder] *)

val decode_pb_expr_binary : Pbrt.Decoder.t -> expr_binary
(** [decode_pb_expr_binary decoder] decodes a [expr_binary] binary value from [decoder] *)

val decode_pb_expr_kind : Pbrt.Decoder.t -> expr_kind
(** [decode_pb_expr_kind decoder] decodes a [expr_kind] binary value from [decoder] *)

val decode_pb_expr : Pbrt.Decoder.t -> expr
(** [decode_pb_expr decoder] decodes a [expr] binary value from [decoder] *)

val decode_pb_expr_call : Pbrt.Decoder.t -> expr_call
(** [decode_pb_expr_call decoder] decodes a [expr_call] binary value from [decoder] *)

val decode_pb_statement_declaration : Pbrt.Decoder.t -> statement_declaration
(** [decode_pb_statement_declaration decoder] decodes a [statement_declaration] binary value from [decoder] *)

val decode_pb_statement_assign : Pbrt.Decoder.t -> statement_assign
(** [decode_pb_statement_assign decoder] decodes a [statement_assign] binary value from [decoder] *)

val decode_pb_statement_if : Pbrt.Decoder.t -> statement_if
(** [decode_pb_statement_if decoder] decodes a [statement_if] binary value from [decoder] *)

val decode_pb_statement : Pbrt.Decoder.t -> statement
(** [decode_pb_statement decoder] decodes a [statement] binary value from [decoder] *)

val decode_pb_statement_block : Pbrt.Decoder.t -> statement_block
(** [decode_pb_statement_block decoder] decodes a [statement_block] binary value from [decoder] *)

val decode_pb_statement_while : Pbrt.Decoder.t -> statement_while
(** [decode_pb_statement_while decoder] decodes a [statement_while] binary value from [decoder] *)

val decode_pb_ez_typed_param : Pbrt.Decoder.t -> ez_typed_param
(** [decode_pb_ez_typed_param decoder] decodes a [ez_typed_param] binary value from [decoder] *)

val decode_pb_function_ : Pbrt.Decoder.t -> function_
(** [decode_pb_function_ decoder] decodes a [function_] binary value from [decoder] *)

val decode_pb_extern : Pbrt.Decoder.t -> extern
(** [decode_pb_extern decoder] decodes a [extern] binary value from [decoder] *)

val decode_pb_definition : Pbrt.Decoder.t -> definition
(** [decode_pb_definition decoder] decodes a [definition] binary value from [decoder] *)

val decode_pb_program : Pbrt.Decoder.t -> program
(** [decode_pb_program decoder] decodes a [program] binary value from [decoder] *)

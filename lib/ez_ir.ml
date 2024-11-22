[@@@ocaml.warning "-27-30-39-44"]

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

type definition_ez_typed_param = {
  param_type : ez_type;
  name : string;
}

type definition = {
  return_type : ez_type;
  name : string;
  params : definition_ez_typed_param list;
  body : statement;
}

type program = {
  definitions : definition list;
}

let rec default_ez_type_non_ptr () = (Unit:ez_type_non_ptr)

let rec default_ez_type () : ez_type = Non_ptr (default_ez_type_non_ptr ())

let rec default_value_unit = ()

let rec default_value (): value = Unit_val

let rec default_expr_binary_bin_op () = (Add:expr_binary_bin_op)

let rec default_expr_binary 
  ?op:((op:expr_binary_bin_op) = default_expr_binary_bin_op ())
  ?lhs:((lhs:expr) = default_expr ())
  ?rhs:((rhs:expr) = default_expr ())
  () : expr_binary  = {
  op;
  lhs;
  rhs;
}

and default_expr_kind () : expr_kind = Binary (default_expr_binary ())

and default_expr 
  ?kind:((kind:expr_kind) = Binary (default_expr_binary ()))
  ?expr_type:((expr_type:ez_type) = default_ez_type ())
  () : expr  = {
  kind;
  expr_type;
}

and default_expr_call 
  ?callee:((callee:string) = "")
  ?args:((args:expr list) = [])
  () : expr_call  = {
  callee;
  args;
}

let rec default_statement_declaration 
  ?name:((name:string) = "")
  ?type_:((type_:ez_type) = default_ez_type ())
  ?rhs:((rhs:expr) = default_expr ())
  () : statement_declaration  = {
  name;
  type_;
  rhs;
}

let rec default_statement_assign 
  ?name:((name:string) = "")
  ?rhs:((rhs:expr) = default_expr ())
  () : statement_assign  = {
  name;
  rhs;
}

let rec default_statement_if 
  ?condition:((condition:expr) = default_expr ())
  ?then_branch:((then_branch:statement) = default_statement ())
  ?else_branch:((else_branch:statement) = default_statement ())
  () : statement_if  = {
  condition;
  then_branch;
  else_branch;
}

and default_statement () : statement = Expr (default_expr ())

and default_statement_block 
  ?statements:((statements:statement list) = [])
  () : statement_block  = {
  statements;
}

and default_statement_while 
  ?condition:((condition:expr) = default_expr ())
  ?body:((body:statement) = default_statement ())
  () : statement_while  = {
  condition;
  body;
}

let rec default_definition_ez_typed_param 
  ?param_type:((param_type:ez_type) = default_ez_type ())
  ?name:((name:string) = "")
  () : definition_ez_typed_param  = {
  param_type;
  name;
}

let rec default_definition 
  ?return_type:((return_type:ez_type) = default_ez_type ())
  ?name:((name:string) = "")
  ?params:((params:definition_ez_typed_param list) = [])
  ?body:((body:statement) = default_statement ())
  () : definition  = {
  return_type;
  name;
  params;
  body;
}

let rec default_program 
  ?definitions:((definitions:definition list) = [])
  () : program  = {
  definitions;
}

type expr_binary_mutable = {
  mutable op : expr_binary_bin_op;
  mutable lhs : expr;
  mutable rhs : expr;
}

let default_expr_binary_mutable () : expr_binary_mutable = {
  op = default_expr_binary_bin_op ();
  lhs = default_expr ();
  rhs = default_expr ();
}

type expr_mutable = {
  mutable kind : expr_kind;
  mutable expr_type : ez_type;
}

let default_expr_mutable () : expr_mutable = {
  kind = Binary (default_expr_binary ());
  expr_type = default_ez_type ();
}

type expr_call_mutable = {
  mutable callee : string;
  mutable args : expr list;
}

let default_expr_call_mutable () : expr_call_mutable = {
  callee = "";
  args = [];
}

type statement_declaration_mutable = {
  mutable name : string;
  mutable type_ : ez_type;
  mutable rhs : expr;
}

let default_statement_declaration_mutable () : statement_declaration_mutable = {
  name = "";
  type_ = default_ez_type ();
  rhs = default_expr ();
}

type statement_assign_mutable = {
  mutable name : string;
  mutable rhs : expr;
}

let default_statement_assign_mutable () : statement_assign_mutable = {
  name = "";
  rhs = default_expr ();
}

type statement_if_mutable = {
  mutable condition : expr;
  mutable then_branch : statement;
  mutable else_branch : statement;
}

let default_statement_if_mutable () : statement_if_mutable = {
  condition = default_expr ();
  then_branch = default_statement ();
  else_branch = default_statement ();
}

type statement_block_mutable = {
  mutable statements : statement list;
}

let default_statement_block_mutable () : statement_block_mutable = {
  statements = [];
}

type statement_while_mutable = {
  mutable condition : expr;
  mutable body : statement;
}

let default_statement_while_mutable () : statement_while_mutable = {
  condition = default_expr ();
  body = default_statement ();
}

type definition_ez_typed_param_mutable = {
  mutable param_type : ez_type;
  mutable name : string;
}

let default_definition_ez_typed_param_mutable () : definition_ez_typed_param_mutable = {
  param_type = default_ez_type ();
  name = "";
}

type definition_mutable = {
  mutable return_type : ez_type;
  mutable name : string;
  mutable params : definition_ez_typed_param list;
  mutable body : statement;
}

let default_definition_mutable () : definition_mutable = {
  return_type = default_ez_type ();
  name = "";
  params = [];
  body = default_statement ();
}

type program_mutable = {
  mutable definitions : definition list;
}

let default_program_mutable () : program_mutable = {
  definitions = [];
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_ez_type_non_ptr (v:ez_type_non_ptr) encoder =
  match v with
  | Unit -> Pbrt.Encoder.int_as_varint (0) encoder
  | I64 -> Pbrt.Encoder.int_as_varint 1 encoder
  | Str -> Pbrt.Encoder.int_as_varint 2 encoder
  | F64 -> Pbrt.Encoder.int_as_varint 3 encoder
  | Bool -> Pbrt.Encoder.int_as_varint 4 encoder

let rec encode_pb_ez_type (v:ez_type) encoder = 
  begin match v with
  | Non_ptr x ->
    encode_pb_ez_type_non_ptr x encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  | Ptr x ->
    Pbrt.Encoder.nested encode_pb_ez_type x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  end

let rec encode_pb_value_unit (v:value_unit) encoder = 
()

let rec encode_pb_value (v:value) encoder = 
  begin match v with
  | Unit_val ->
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
    Pbrt.Encoder.empty_nested encoder
  | I64_val x ->
    Pbrt.Encoder.int64_as_varint x encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  | Str_val x ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  | F64_val x ->
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  | Bool_val x ->
    Pbrt.Encoder.bool x encoder;
    Pbrt.Encoder.key 5 Pbrt.Varint encoder; 
  end

let rec encode_pb_expr_binary_bin_op (v:expr_binary_bin_op) encoder =
  match v with
  | Add -> Pbrt.Encoder.int_as_varint (0) encoder
  | Sub -> Pbrt.Encoder.int_as_varint 1 encoder
  | Mul -> Pbrt.Encoder.int_as_varint 2 encoder
  | Div -> Pbrt.Encoder.int_as_varint 3 encoder
  | Mod -> Pbrt.Encoder.int_as_varint 4 encoder
  | Eq -> Pbrt.Encoder.int_as_varint 5 encoder
  | Noteq -> Pbrt.Encoder.int_as_varint 6 encoder
  | Lessthan -> Pbrt.Encoder.int_as_varint 7 encoder
  | Lesseq -> Pbrt.Encoder.int_as_varint 8 encoder
  | Greaterthan -> Pbrt.Encoder.int_as_varint 9 encoder
  | Greatereq -> Pbrt.Encoder.int_as_varint 10 encoder
  | Land -> Pbrt.Encoder.int_as_varint 11 encoder
  | Lor -> Pbrt.Encoder.int_as_varint 12 encoder

let rec encode_pb_expr_binary (v:expr_binary) encoder = 
  encode_pb_expr_binary_bin_op v.op encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.nested encode_pb_expr v.lhs encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  Pbrt.Encoder.nested encode_pb_expr v.rhs encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ()

and encode_pb_expr_kind (v:expr_kind) encoder = 
  begin match v with
  | Binary x ->
    Pbrt.Encoder.nested encode_pb_expr_binary x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | Value x ->
    Pbrt.Encoder.nested encode_pb_value x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  | Variable x ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  | Call x ->
    Pbrt.Encoder.nested encode_pb_expr_call x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  end

and encode_pb_expr (v:expr) encoder = 
  begin match v.kind with
  | Binary x ->
    Pbrt.Encoder.nested encode_pb_expr_binary x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | Value x ->
    Pbrt.Encoder.nested encode_pb_value x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  | Variable x ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  | Call x ->
    Pbrt.Encoder.nested encode_pb_expr_call x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  end;
  Pbrt.Encoder.nested encode_pb_ez_type v.expr_type encoder;
  Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  ()

and encode_pb_expr_call (v:expr_call) encoder = 
  Pbrt.Encoder.string v.callee encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_expr x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.args encoder;
  ()

let rec encode_pb_statement_declaration (v:statement_declaration) encoder = 
  Pbrt.Encoder.string v.name encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.nested encode_pb_ez_type v.type_ encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  Pbrt.Encoder.nested encode_pb_expr v.rhs encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_statement_assign (v:statement_assign) encoder = 
  Pbrt.Encoder.string v.name encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.nested encode_pb_expr v.rhs encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_statement_if (v:statement_if) encoder = 
  Pbrt.Encoder.nested encode_pb_expr v.condition encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.nested encode_pb_statement v.then_branch encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  Pbrt.Encoder.nested encode_pb_statement v.else_branch encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ()

and encode_pb_statement (v:statement) encoder = 
  begin match v with
  | Expr x ->
    Pbrt.Encoder.nested encode_pb_expr x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | Declaration x ->
    Pbrt.Encoder.nested encode_pb_statement_declaration x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  | If x ->
    Pbrt.Encoder.nested encode_pb_statement_if x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  | Block x ->
    Pbrt.Encoder.nested encode_pb_statement_block x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  | While x ->
    Pbrt.Encoder.nested encode_pb_statement_while x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  | Return x ->
    Pbrt.Encoder.nested encode_pb_expr x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  | Assign x ->
    Pbrt.Encoder.nested encode_pb_statement_assign x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  end

and encode_pb_statement_block (v:statement_block) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_statement x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.statements encoder;
  ()

and encode_pb_statement_while (v:statement_while) encoder = 
  Pbrt.Encoder.nested encode_pb_expr v.condition encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.nested encode_pb_statement v.body encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_definition_ez_typed_param (v:definition_ez_typed_param) encoder = 
  Pbrt.Encoder.nested encode_pb_ez_type v.param_type encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.string v.name encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_definition (v:definition) encoder = 
  Pbrt.Encoder.nested encode_pb_ez_type v.return_type encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.string v.name encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_definition_ez_typed_param x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.params encoder;
  Pbrt.Encoder.nested encode_pb_statement v.body encoder;
  Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_program (v:program) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_definition x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.definitions encoder;
  ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_ez_type_non_ptr d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Unit:ez_type_non_ptr)
  | 1 -> (I64:ez_type_non_ptr)
  | 2 -> (Str:ez_type_non_ptr)
  | 3 -> (F64:ez_type_non_ptr)
  | 4 -> (Bool:ez_type_non_ptr)
  | _ -> Pbrt.Decoder.malformed_variant "ez_type_non_ptr"

let rec decode_pb_ez_type d = 
  let rec loop () = 
    let ret:ez_type = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "ez_type"
      | Some (1, _) -> (Non_ptr (decode_pb_ez_type_non_ptr d) : ez_type) 
      | Some (2, _) -> (Ptr (decode_pb_ez_type (Pbrt.Decoder.nested d)) : ez_type) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_pb_value_unit d =
  match Pbrt.Decoder.key d with
  | None -> ();
  | Some (_, pk) -> 
    Pbrt.Decoder.unexpected_payload "Unexpected fields in empty message(value_unit)" pk

let rec decode_pb_value d = 
  let rec loop () = 
    let ret:value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "value"
      | Some (1, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Unit_val : value)
      end
      | Some (2, _) -> (I64_val (Pbrt.Decoder.int64_as_varint d) : value) 
      | Some (3, _) -> (Str_val (Pbrt.Decoder.string d) : value) 
      | Some (4, _) -> (F64_val (Pbrt.Decoder.float_as_bits64 d) : value) 
      | Some (5, _) -> (Bool_val (Pbrt.Decoder.bool d) : value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_pb_expr_binary_bin_op d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Add:expr_binary_bin_op)
  | 1 -> (Sub:expr_binary_bin_op)
  | 2 -> (Mul:expr_binary_bin_op)
  | 3 -> (Div:expr_binary_bin_op)
  | 4 -> (Mod:expr_binary_bin_op)
  | 5 -> (Eq:expr_binary_bin_op)
  | 6 -> (Noteq:expr_binary_bin_op)
  | 7 -> (Lessthan:expr_binary_bin_op)
  | 8 -> (Lesseq:expr_binary_bin_op)
  | 9 -> (Greaterthan:expr_binary_bin_op)
  | 10 -> (Greatereq:expr_binary_bin_op)
  | 11 -> (Land:expr_binary_bin_op)
  | 12 -> (Lor:expr_binary_bin_op)
  | _ -> Pbrt.Decoder.malformed_variant "expr_binary_bin_op"

let rec decode_pb_expr_binary d =
  let v = default_expr_binary_mutable () in
  let continue__= ref true in
  let rhs_is_set = ref false in
  let lhs_is_set = ref false in
  let op_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.op <- decode_pb_expr_binary_bin_op d; op_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_binary), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.lhs <- decode_pb_expr (Pbrt.Decoder.nested d); lhs_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_binary), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.rhs <- decode_pb_expr (Pbrt.Decoder.nested d); rhs_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_binary), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !rhs_is_set then Pbrt.Decoder.missing_field "rhs" end;
  begin if not !lhs_is_set then Pbrt.Decoder.missing_field "lhs" end;
  begin if not !op_is_set then Pbrt.Decoder.missing_field "op" end;
  ({
    op = v.op;
    lhs = v.lhs;
    rhs = v.rhs;
  } : expr_binary)

and decode_pb_expr_kind d = 
  let rec loop () = 
    let ret:expr_kind = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "expr_kind"
      | Some (1, _) -> (Binary (decode_pb_expr_binary (Pbrt.Decoder.nested d)) : expr_kind) 
      | Some (2, _) -> (Value (decode_pb_value (Pbrt.Decoder.nested d)) : expr_kind) 
      | Some (3, _) -> (Variable (Pbrt.Decoder.string d) : expr_kind) 
      | Some (4, _) -> (Call (decode_pb_expr_call (Pbrt.Decoder.nested d)) : expr_kind) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_pb_expr d =
  let v = default_expr_mutable () in
  let continue__= ref true in
  let expr_type_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.kind <- Binary (decode_pb_expr_binary (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.kind <- Value (decode_pb_value (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.kind <- Variable (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.kind <- Call (decode_pb_expr_call (Pbrt.Decoder.nested d));
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr), field(4)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.expr_type <- decode_pb_ez_type (Pbrt.Decoder.nested d); expr_type_is_set := true;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr), field(7)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !expr_type_is_set then Pbrt.Decoder.missing_field "expr_type" end;
  ({
    kind = v.kind;
    expr_type = v.expr_type;
  } : expr)

and decode_pb_expr_call d =
  let v = default_expr_call_mutable () in
  let continue__= ref true in
  let callee_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.args <- List.rev v.args;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.callee <- Pbrt.Decoder.string d; callee_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_call), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.args <- (decode_pb_expr (Pbrt.Decoder.nested d)) :: v.args;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_call), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !callee_is_set then Pbrt.Decoder.missing_field "callee" end;
  ({
    callee = v.callee;
    args = v.args;
  } : expr_call)

let rec decode_pb_statement_declaration d =
  let v = default_statement_declaration_mutable () in
  let continue__= ref true in
  let rhs_is_set = ref false in
  let type__is_set = ref false in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d; name_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_declaration), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.type_ <- decode_pb_ez_type (Pbrt.Decoder.nested d); type__is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_declaration), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.rhs <- decode_pb_expr (Pbrt.Decoder.nested d); rhs_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_declaration), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !rhs_is_set then Pbrt.Decoder.missing_field "rhs" end;
  begin if not !type__is_set then Pbrt.Decoder.missing_field "type_" end;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  ({
    name = v.name;
    type_ = v.type_;
    rhs = v.rhs;
  } : statement_declaration)

let rec decode_pb_statement_assign d =
  let v = default_statement_assign_mutable () in
  let continue__= ref true in
  let rhs_is_set = ref false in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d; name_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_assign), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.rhs <- decode_pb_expr (Pbrt.Decoder.nested d); rhs_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_assign), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !rhs_is_set then Pbrt.Decoder.missing_field "rhs" end;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  ({
    name = v.name;
    rhs = v.rhs;
  } : statement_assign)

let rec decode_pb_statement_if d =
  let v = default_statement_if_mutable () in
  let continue__= ref true in
  let else_branch_is_set = ref false in
  let then_branch_is_set = ref false in
  let condition_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.condition <- decode_pb_expr (Pbrt.Decoder.nested d); condition_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.then_branch <- decode_pb_statement (Pbrt.Decoder.nested d); then_branch_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.else_branch <- decode_pb_statement (Pbrt.Decoder.nested d); else_branch_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !else_branch_is_set then Pbrt.Decoder.missing_field "else_branch" end;
  begin if not !then_branch_is_set then Pbrt.Decoder.missing_field "then_branch" end;
  begin if not !condition_is_set then Pbrt.Decoder.missing_field "condition" end;
  ({
    condition = v.condition;
    then_branch = v.then_branch;
    else_branch = v.else_branch;
  } : statement_if)

and decode_pb_statement d = 
  let rec loop () = 
    let ret:statement = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "statement"
      | Some (1, _) -> (Expr (decode_pb_expr (Pbrt.Decoder.nested d)) : statement) 
      | Some (2, _) -> (Declaration (decode_pb_statement_declaration (Pbrt.Decoder.nested d)) : statement) 
      | Some (3, _) -> (If (decode_pb_statement_if (Pbrt.Decoder.nested d)) : statement) 
      | Some (4, _) -> (Block (decode_pb_statement_block (Pbrt.Decoder.nested d)) : statement) 
      | Some (5, _) -> (While (decode_pb_statement_while (Pbrt.Decoder.nested d)) : statement) 
      | Some (6, _) -> (Return (decode_pb_expr (Pbrt.Decoder.nested d)) : statement) 
      | Some (7, _) -> (Assign (decode_pb_statement_assign (Pbrt.Decoder.nested d)) : statement) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_pb_statement_block d =
  let v = default_statement_block_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.statements <- List.rev v.statements;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.statements <- (decode_pb_statement (Pbrt.Decoder.nested d)) :: v.statements;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_block), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    statements = v.statements;
  } : statement_block)

and decode_pb_statement_while d =
  let v = default_statement_while_mutable () in
  let continue__= ref true in
  let body_is_set = ref false in
  let condition_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.condition <- decode_pb_expr (Pbrt.Decoder.nested d); condition_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_while), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.body <- decode_pb_statement (Pbrt.Decoder.nested d); body_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_while), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !body_is_set then Pbrt.Decoder.missing_field "body" end;
  begin if not !condition_is_set then Pbrt.Decoder.missing_field "condition" end;
  ({
    condition = v.condition;
    body = v.body;
  } : statement_while)

let rec decode_pb_definition_ez_typed_param d =
  let v = default_definition_ez_typed_param_mutable () in
  let continue__= ref true in
  let name_is_set = ref false in
  let param_type_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.param_type <- decode_pb_ez_type (Pbrt.Decoder.nested d); param_type_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(definition_ez_typed_param), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d; name_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(definition_ez_typed_param), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  begin if not !param_type_is_set then Pbrt.Decoder.missing_field "param_type" end;
  ({
    param_type = v.param_type;
    name = v.name;
  } : definition_ez_typed_param)

let rec decode_pb_definition d =
  let v = default_definition_mutable () in
  let continue__= ref true in
  let body_is_set = ref false in
  let name_is_set = ref false in
  let return_type_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.params <- List.rev v.params;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.return_type <- decode_pb_ez_type (Pbrt.Decoder.nested d); return_type_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(definition), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d; name_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(definition), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.params <- (decode_pb_definition_ez_typed_param (Pbrt.Decoder.nested d)) :: v.params;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(definition), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.body <- decode_pb_statement (Pbrt.Decoder.nested d); body_is_set := true;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(definition), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !body_is_set then Pbrt.Decoder.missing_field "body" end;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  begin if not !return_type_is_set then Pbrt.Decoder.missing_field "return_type" end;
  ({
    return_type = v.return_type;
    name = v.name;
    params = v.params;
    body = v.body;
  } : definition)

let rec decode_pb_program d =
  let v = default_program_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.definitions <- List.rev v.definitions;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.definitions <- (decode_pb_definition (Pbrt.Decoder.nested d)) :: v.definitions;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(program), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    definitions = v.definitions;
  } : program)

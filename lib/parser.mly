%{
  open Ez_ir
  open Ast

%}

%token EOF
%token SEMICOL
%token COMMA
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LBKT
%token RBKT

%token COLON
%token ASSIGN

%token ADD
%token SUB
%token MUL
%token DIV
%token EQ
%token NEQ
%token LT
%token LE
%token GT
%token GE
%token AND
%token OR

%left OR
%left AND
%left EQ NEQ LT LE GT GE
%left ADD SUB
%left MUL DIV


(* Values *)
%token <string> IDENTIFIER
%token <string> STRING
%token <int64> INT
%token <float> F64
%token TRUE
%token FALSE
%token UNIT

%token UNIT_T
%token INT_T
%token STR_T
%token FLOAT_T
%token BOOL_T
%token PTR_T

%token IF
%token ELSE
%token WHILE
%token RETURN

%start <prog0> program_eof

%%

program_eof:
  | defs=list(definition) EOF { defs }

definition: 
  | t=type_param id=IDENTIFIER LPAREN RPAREN body=statement {
    (t, id, [], body) 
  }
  | t=type_param id=IDENTIFIER UNIT body=statement {
    (t, id, [], body) 
  }
  | t=type_param id=IDENTIFIER LPAREN args=arg_list RPAREN body=statement {
    (t, id, args, body) 
  }

type_param: 
  | UNIT_T { Non_ptr(Unit)  }
  | INT_T { Non_ptr(I64) }
  | STR_T { Non_ptr(Str) }
  | FLOAT_T { Non_ptr(F64) }
  | BOOL_T { Non_ptr(Bool) }
  | PTR_T LBKT inner=type_param RBKT {
    Ptr(inner)
  }

arg_list: 
  | a=arg { [a] } 
  | a=arg COMMA rest=arg_list { [a] @ rest }

arg:
  | param_type=type_param name=IDENTIFIER { { param_type; name } } 

statement: 
  | LBRACE stmts=list(statement) RBRACE {
    Block(stmts)
  }
  | RETURN e=expression SEMICOL { Return(e) }
  | id=IDENTIFIER COLON ty=type_param ASSIGN e=expression SEMICOL { Declaration(ty, id, e) }
  | id=IDENTIFIER ASSIGN e=expression SEMICOL { Assign(id, e) }
  | e=expression SEMICOL { Expr(e) }
  | IF LPAREN test=expression RPAREN then_clause=statement ELSE else_clause=statement {
    If(test, then_clause, else_clause) 
  }
  | WHILE LPAREN cond=expression RPAREN body=statement {
    While(cond, body) 
  }

%inline bin_op:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | EQ { Eq } 
  | NEQ { Noteq } 
  | LT { Lessthan } 
  | LE { Lesseq } 
  | GT { Greaterthan } 
  | GE { Greatereq } 
  | AND { Land } 
  | OR { Lor } 

param_list: 
  | e=expression { [e] } 
  | e=expression COMMA rest=param_list { [e] @ rest }

expression: 
  | id=IDENTIFIER LPAREN RPAREN { Call(id, []) }
  | id=IDENTIFIER UNIT { Call(id, []) }
  | id=IDENTIFIER LPAREN l=param_list RPAREN { Call(id, l) }
  | id=IDENTIFIER { Var(id) }
  | lhs=expression op=bin_op rhs=expression { BinOp(op, lhs, rhs) }
  | LPAREN inner=expression RPAREN { inner }
  | v=value  { Val v }

value:
  | UNIT { Unit_val } 
  | i=INT { I64_val(i) }
  | s=STRING { Str_val(s) }
  | f=F64 { F64_val(f) }
  | TRUE { Bool_val(true) }
  | FALSE { Bool_val(false) }

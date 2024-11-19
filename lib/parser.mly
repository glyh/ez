%{
  open Ez_ir
  open Ast

  let pass = Block []
  let pass_exp = Val(Bool_val(true))

%}

%token EOF
%token SEMICOL
%token COMMA
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN

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
%token NOT

%right ASSIGN
%left OR
%left AND
%left EQ NEQ LT LE GT GE
%left ADD SUB
%left MUL DIV
%nonassoc NOT 


(* Values *)
%token <string> IDENTIFIER
%token <string> STRING
%token <int64> INT
%token <float> F64
%token TRUE
%token FALSE

%token VOID_T
%token INT_T
%token STR_T
%token FLOAT_T
%token BOOL_T

%token IF
%token ELSE
%token WHEN
%token WHILE
%token FOR
%token RETURN

%start <prog0> program_eof

%%

program_eof:
  | defs=list(definition) EOF { defs }

definition: 
  | t=type_param id=IDENTIFIER LPAREN RPAREN body=statement {
    (t, id, [], body) 
  }
  | t=type_param id=IDENTIFIER LPAREN args=arg_list RPAREN body=statement {
    (t, id, args, body) 
  }

type_param: 
  | VOID_T { Non_ptr(Void)  }
  | INT_T { Non_ptr(I64) }
  | STR_T { Non_ptr(Str) }
  | FLOAT_T { Non_ptr(F64) }
  | BOOL_T { Non_ptr(Bool) }

arg_list: 
  | a=arg { [a] } 
  | a=arg COMMA rest=arg_list { [a] @ rest }

arg:
  | arg_type=type_param name=IDENTIFIER { { arg_type; name } } 

statement: 
  | LBRACE ss=list(statement) RBRACE {
    Block(ss)
  }
  | RETURN e=expression SEMICOL { Return(e) }
  | ty=type_param id=IDENTIFIER ASSIGN e=expression SEMICOL { Declaration(ty, id, e) }
  | e=expression SEMICOL { Expr(e) }
  | IF LPAREN test=expression RPAREN then_clause=statement ELSE else_clause=statement {
    If(test, then_clause, else_clause) 
  }
  | WHEN LPAREN test=expression RPAREN then_clause=statement {
    If(test, then_clause, pass) 
  }
  | WHILE LPAREN cond=expression RPAREN body=statement {
    For(pass_exp, cond, pass_exp, body) 
  }
  | FOR LPAREN initial=expression SEMICOL cond=expression SEMICOL step=expression SEMICOL body=statement {
    For(initial, cond, step, body)
  }
  | SEMICOL { pass }

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

%inline un_op:
  | NOT { Not }

param_list: 
  | e=expression { [e] } 
  | e=expression COMMA rest=param_list { [e] @ rest }

expression: 
  | lhs=IDENTIFIER ASSIGN rhs=expression { Assign(lhs, rhs) }
  | id=IDENTIFIER LPAREN RPAREN { Call(id, []) }
  | id=IDENTIFIER LPAREN l=param_list RPAREN { Call(id, l) }
  | id=IDENTIFIER { Var(id) }
  | lhs=expression op=bin_op rhs=expression { BinOp(op, lhs, rhs) }
  | op=un_op inner=expression { UnOp(op, inner) }
  | LPAREN inner=expression RPAREN { inner }
  | v=value  { Val v }

value:
  | i=INT { I64_val(i) }
  | s=STRING { Str_val(s) }
  | f=F64 { F64_val(f) }
  | TRUE { Bool_val(true) }
  | FALSE { Bool_val(false) }

include Nice_parser.Make (struct
  type result = Ast.prog0
  type token = Parser.token

  exception ParseError = Parser.Error

  let parse = Parser.program_eof

  include Lexer
end)

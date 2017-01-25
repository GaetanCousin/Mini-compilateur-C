
(* The type of tokens. *)

type token = 
  | WHILE
  | VOID
  | VAR
  | UNSIGNED
  | STRUCT
  | SIZEOF
  | SHORT
  | SEMI
  | RP
  | RETURN
  | RBRACKET
  | RB
  | PRINT
  | PP
  | POINTER
  | PLUS
  | OR
  | NOT
  | NEWLINE
  | NEQ
  | MULT
  | MODULO
  | MM
  | MINUS
  | LT
  | LP
  | LONG
  | LE
  | LBRACKET
  | LB
  | INT
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FOR
  | EXTERN
  | EXIT
  | EQ
  | EOF
  | ELSE
  | DOUBLE
  | DOT
  | DIV
  | CONST_STRING of (string)
  | CONST_INT of (int32)
  | CONST_DOUBLE of (float)
  | CONST_CHAR of (string)
  | CONST_BOOL of (bool)
  | COMMA
  | CINT of (Ast.signedness*Ast.num*int)
  | CHAR
  | ASSIGN
  | AND
  | ADDRESS

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.loc Ast.file)

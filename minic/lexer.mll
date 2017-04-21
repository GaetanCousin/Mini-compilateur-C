(* Analyse lexicale *)
{

  open Lexing
  open Parser
  open Ast

  (* Erreurs lexicales *)

  exception Lexical_error of string


  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum;
        pos_cnum=0 }

  let char_error s = raise (Lexical_error ("illegal character sequence: " ^ s))

  let lexical_error s = failwith ("Lexical error : " ^ s)

  let comment_cpt = ref 0

  let code_char c =
  match String.length c with
    |1 ->  Char.code c.[0]
    |2 |4 when c.[0] == '\\' ->
      begin
        match c.[1] with
        |'n' -> 10
        |'t' -> 9
        |'\'' -> 39
        |'\"' -> 34
        |'x' -> c.[0] <- '0'; (int_of_string c)
        | _ -> failwith c
      end
    | _ -> failwith c

  let str_buff = Buffer.create 512

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [
	"not",   NOT;
	"if",    IF;
	"else",  ELSE;
	"double", DOUBLE;
	"while", WHILE;
	"extern", EXTERN;
	"for",   FOR;
	"return", RETURN;
	"int",   INT;
	"long",	 LONG;
	"char",	 CHAR;
	"unsigned", UNSIGNED;
	"short", SHORT;
	"sizeof", SIZEOF;
	"struct", STRUCT;
	"extern", EXTERN;
	"void", VOID;

      ]	;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT s

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let integer = digit+
let float = (integer '.' digit* | '.' integer) (('e'|'E')'-'?integer)?
let ident = (alpha | '_') (digit | alpha | ['_'])*
let hexa = digit | ['a'-'f' 'A'-'F']
let char =
  [^'\000'-'\x1f' '\\' '\"' '\'' ]
  | '\\' ('n' | 't' | '\'' |'\"')
  | "\\x" hexa hexa

let chaine = char*
let integer = digit+

rule token = parse
  | '\n'
      { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | (digit+ '.' digit* | '.' digit+)(('e'|'E') '-'? digit+)?
	  { CONST_DOUBLE ( float_of_string(lexeme lexbuf)) }
  | "/*"
      { incr comment_cpt; comment lexbuf; token lexbuf }
  | "//" [^'\n']*'\n'?
      {
        let comment = lexeme lexbuf in
        let last_char = comment.[String.length comment - 1] in
        if last_char = '\n' then newline lexbuf;
        token lexbuf
      }
  | '\"'
    { Buffer.reset str_buff;
      string lexbuf }
  | '\'' (char as c) '\''
    { CINT (Signed, Int, code_char c)}
  | (integer as i)(['U''u']? as u)(['L''l']? as l)
		{
		let s = if u = "" then
					Ast.Signed
				else
					Ast.Unsigned in
					let t = if l = "" then
								Ast.Int
							else
								Ast.Long in
		try CINT(s, t , int_of_string i)
		with
		 _ -> raise (Lexical_error ("illegal integer constant: " ^i))
		}
  | ident
      { keyword_or_ident (lexeme lexbuf) }
  | "-"
      { MINUS }
  | "+"
      { PLUS }
  | "++"
	  { PP }
  | "--"
	  { MM }
  | "*"
      { MULT }
  | "/"
      { DIV }
  | "=="
      { EQ }
  | "!="
      { NEQ }
  | "!"
	  { NOT }
  | ">"
      { GT }
  | ">="
      { GE }
  | "<"
      { LT }
  | "<="
      { LE }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "%"
	  {MODULO}
  | "&"
	  {ADDRESS}
  | "->"
	  {POINTER}
  | "="
      { ASSIGN }
  | "["
      { LBRACKET }
  | "]"
      { RBRACKET }
  | "("
      { LP }
  | ")"
      { RP }
  | "{"
      { LB }
  | "}"
      { RB }
  | "."
      { DOT}
  | ","
	  { COMMA }
  | ";"
      { SEMI}
  | eof
      { EOF }
  | _
      { raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }

and comment = parse
  | "*/" { () }
  | '\n' { newline lexbuf; comment lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }
  | _    { comment lexbuf }

and string = parse
  | char as c { Buffer.add_char str_buff (Char.chr (code_char c));
                string lexbuf }
  | '\"' { CONST_STRING (Buffer.contents str_buff) }
  | eof  { raise (Lexical_error "unterminated string") }
  | _    { char_error (lexeme lexbuf) }

(* Arbres de syntaxe abstraite *)

type ('info, 'node) node = { info : 'info;
							 node : 'node }

type loc = Lexing.position * Lexing.position

type ident = (loc, string) node

type signedness = Unsigned | Signed 
type num = Char | Short | Long | Int

type c_type =
  | Tnull (* pour typer null *)
  | Tvoid
  | Tnum of (signedness * num)
  | Tdouble 
  | Tstruct of ident
  | Tpointer of c_type
  (* à completer *)

type constant =
  | Cint of (signedness*num*int)
  | Cdouble of float
  | Cstring of string


type binop = Add | Mult | Div | Sub | Mod | Eq | Neq | Ge | Gt
			| Le | Lt | And | Or 

type unop = Neg | Deref | Preincr | Postincr | Predecr | Postdecr

type 'info expr = ('info, 'info expr_node) node
and 'info expr_node =
   (*| EnullInséré automatiquement à partir de 0 *)
  | Econst of constant
  | Eident of ident
  | Esizeof of c_type
  | Ebinop of 'info expr * binop * 'info expr
  | Eunop of unop * 'info expr
  | Egetarr of 'info expr * 'info expr
  | Estructvar of 'info expr * ident 
  | Estructvarpointer of 'info expr * ident
  | Eassign of 'info expr * 'info expr
  | Ecall of ident * 'info expr list
  | Ecast of c_type * 'info expr 
  
  (* à compléter *)


type var_decl =  c_type * ident

type 'info statement = ('info, 'info statement_node) node

and 'info statement_node =
  | Sskip
  | Sexpr of 'info expr
  | Sif of 'info expr * 'info statement * 'info statement 
  | Sfor of 'info expr list *  ('info expr option ) * 'info expr list * 'info statement
  | Sreturn of 'info expr option 
  (* à compléter  FINI*)

and 'info block =
    var_decl list * 'info statement list

type 'info decl =
  | Dvar of var_decl
  | Dstruct of ident * var_decl list
  | Dfun of c_type * ident * var_decl list * ('info block option)
  (* à compléter  FINI*)

type 'info file =  'info decl list

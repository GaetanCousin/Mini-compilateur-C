/* Parseur pour le compilateur C */

%{
  open Ast


  let mk_loc e l = { info = l; node = e }

  let loc e =
    mk_loc e (Parsing.symbol_start_pos (),Parsing.symbol_end_pos())

  let loc_i i e =
    mk_loc e (Parsing.rhs_start_pos i, Parsing.rhs_end_pos i)

  let loc_dummy e =
    mk_loc e (Lexing.dummy_pos, Lexing.dummy_pos)
    

  type pvar =
    I of ident
  | P of pvar

  let rec unvar t v =
    match v with
    | I i -> (t, i)
    | P vv -> unvar (Tpointer t) vv

  
  type ptyp =
  | T of c_type
  | C of ptyp

  let rec untyp v =
    match v with
    | T t -> (t)
    | C vv -> Tpointer (untyp vv)

%}

%token EOF
%token <float> CONST_DOUBLE
%token <bool> CONST_BOOL
%token <string> CONST_CHAR
%token <string> CONST_STRING
%token PLUS MINUS MULT DIV PP MM
%token AND OR NOT
%token EQ NEQ
%token GE GT LE LT
%token IF ELSE
%token LP RP LB RB
%token <string> IDENT
%token ASSIGN
%token RETURN
%token SEMI
%token WHILE FOR
%token LBRACKET RBRACKET
%token INT LONG DOUBLE UNSIGNED SHORT CHAR COMMA 
%token SIZEOF VOID STRUCT EXTERN MODULO ADDRESS POINTER DOT
%token <Ast.signedness*Ast.num*int> CINT

(* priorite *)
%nonassoc thenif
%nonassoc ELSE

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left GE GT LE LT
%left PLUS MINUS
%left MULT DIV MODULO
%right NOT PP MM ADDRESS ustar uminus uplus
%left RP LBRACKET POINTER DOT



/* Point d'entrée */

%start file
%type <Ast.loc Ast.file> file

%%

file:
| l = list(decl) EOF {l}
;

/* déclarations */


decl:
  d = decl_var ; SEMI  {   Dvar (d ) }

| STRUCT ; i = ident; LB ;
    l = list (terminated(decl_var, SEMI)) ;
    RB; SEMI   {   Dstruct (i, l) }

| EXTERN; t = typ; v = var; LP;
  l = separated_list(COMMA, decl_var) ; RP;
  SEMI {
      let t, i = unvar t v in
      Dfun(t, i, l, None)
    }
| t = typ ;  v = var ; LP 
  l = separated_list(COMMA, decl_var) ; RP ;
  b = block;
	{
		let  t , i = unvar t v in 
			Dfun(t, i , l , Some b)
	}
		
		
block:
LB; lv = list (terminated(decl_var, SEMI)); li = list (instr);
RB							{ (lv, li) 				}
;
	
instr_:
| SEMI													{ Sskip 	}
| e = expr ; SEMI 										{ Sexpr e	}
| IF ; LP ; e = expr ; RP ; i1=instr; ELSE ; i2=instr   { Sif(e,i1,i2) }  				
| IF ; LP ; e=expr ; RP; i1= instr	%prec thenif		{ Sif(e,i1,loc_dummy Sskip ) }
| FOR ; LP ; l1 = l_expr; SEMI ; e = option(expr) ; SEMI; l2 = l_expr ; RP ; i = instr { Sfor(l1, e ,l2,i)}
| WHILE ; LP ; e =expr ; RP; i = instr 					{ Sfor( [], Some(e), [], i)  }
| RETURN ; e = option(expr) ; SEMI 							{ Sreturn e }
| b = block                                     { Sblock b}
;

instr:
|i = instr_ 	{ mk_loc i ($startpos,$endpos)	}
;

typ :
n=num ;	 					{ Tnum( Signed , n )   	}
|UNSIGNED ; n=num 			{ Tnum( Unsigned, n )  	}
|VOID						{ Tvoid   				}
|DOUBLE						{ Tdouble 				}
|STRUCT ; id = ident 		{ Tstruct id 			}
;	 

  /* À COMPLÉTER : autres règles */	
decl_var:
t = typ ; v = var       { unvar t v }    
;
  
ident_:
  i = IDENT    { i }
;

ident:
|i = ident_ 	{ mk_loc i ($startpos,$endpos)	}
;  


var:
  i = ident      	{ I (i) }
| MULT ; v = var    { P(v) }
; 

sign:
  { Signed }
| UNSIGNED  { Unsigned }
;

num:
CHAR	{	Char	}
|SHORT	{	Short	}
|INT	{	Int		}
|LONG	{	Long	}
;

const:
|d=CONST_DOUBLE { Econst (Cdouble d) }
|c=CONST_CHAR 	{ Econst (Cstring c) }
|s=CONST_STRING { Econst (Cstring s) }
|c = CINT 		{ let s, t, i = c in Econst(Cint(s, t, i))}
;

%inline op:
|PLUS       { Add }
|MINUS      { Sub }
|DIV        { Div }
|MULT       { Mult}
|MODULO     { Mod }
|EQ         { Eq  }
|NEQ        { Neq }
|GE         { Ge  }
|GT         { Gt  }
|LE         { Le  }
|LT         { Lt  }
|AND        { And }
|OR         { Or  }
;

(*
%inline incr:
|PP	{ incr }
|MM { decr }
*)

expr_:
| c = const							{ c 				}
| e1 = expr ; o = op ; e2 = expr	{ Ebinop(e1,o,e2)	}
| id = ident 						{ Eident id 		}
| MINUS ; e = expr	%prec uminus	{ Eunop(Neg, e)   }
| MULT ; e = expr 	%prec ustar		{ Eunop(Deref, e)	}
| NOT ; e = expr 					{ Eunop(Not, e)	}
| LP ; e = expr_ ; RP 				{ e 				}
| PLUS ; e = expr	%prec uplus		{ e.node 				}
| PP; e = expr	 					{ Eunop(Preincr,e)	}
| MM; e = expr	 					{ Eunop(Predecr,e)	}
| e = expr ; MM						{ Eunop(Postdecr,e)	}
| e = expr ; PP						{ Eunop(Postincr,e)	}
| e1 = expr ; LBRACKET ; e2 = expr ; RBRACKET 
									{Egetarr( e1, e2)	}
| e1 = expr ; DOT ; id = ident		{ Estructvar(e1, id) }
| e1 = expr ; POINTER ; id = ident  { Estructvarpointer( e1, id) }
| e1 = expr ; ASSIGN ; e2 = expr    { Eassign( e1, e2 ) }
| id = ident ; LP ; e = l_expr ; RP	{ Ecall( id, e) }
| SIZEOF ; LP ; ct = cplx_type ; RP	{ let t = untyp ct in Esizeof t }
| LP ; ct = cplx_type ; RP ; e = expr { let t = untyp ct in Ecast( t, e ) 	}
| ADDRESS ; e = expr      { Eunop(Addr,e) }
;



cplx_type :
| t = typ       		{ T(t) }
| v = cplx_type ; MULT  { C(v) }
;

expr:
| e = expr_ 	{ mk_loc e ($startpos,$endpos)	}
; 

l_expr:
| l = separated_list( COMMA, expr) {l}
;

open Ast

exception Type_error of (Ast.loc * string)

module Env =  Map.Make (String)

let error loc msg = raise(Type_error (loc, msg))
let mk_node t e = { info = t ; node = e } 
let global_env = Hashtbl.create 17
let struct_env : (string, var_decl list)Hashtbl.t = Hashtbl.create 17
let fun_env = Hashtbl.create 17

(* let global_t_signed_int = Tnum(Signed, Int) *)   

let add_global_env tab key v = 
	if Hashtbl.mem tab key.node  then 
		error key.info("Redéfinition de l'identifiant " ^ key.node)
	else 
		Hashtbl.add tab key.node v 
		
		
let num t  = match t with 
	|Tstruct _ | Tvoid -> false
	| _ -> true 
	
let arith t  = match t with 
	|Tstruct _ | Tvoid | Tpointer _ -> false
	| _ -> true   
		
let check_var_decl var_decl = 
	let _ =	
		List.fold_left( fun acc(typ, id) ->
			if typ = Tvoid then 
				error id.info "Type de variable invalide"
			else 
				if List.mem id.node acc then 
					error id.info("Redéfinition de l'identifiant" ^ id.node )
				else 
					id.node :: acc) [] var_decl 
	in var_decl 		

let rec check_wf t =
	match t with 
	| Tstruct  id -> Hashtbl.mem struct_env id.node 
	| Tpointer tt -> check_wf tt
	| _ -> true 
		
let compatible t1 t2 = 
	let rec compat_aux t1 t2 = 
		match t1, t2 with
		| Tstruct id1, Tstruct id2 -> id1.node = id2.node
		| Tpointer(Tvoid), Tpointer _ -> true
		| Tpointer tt1, Tpointer tt2 -> compat_aux tt1 tt2
		| Tnull , Tpointer _ -> true
		| ((Tdouble | Tnull | Tnum(_) ) ,
		   (Tdouble | Tnull | Tnum(_) )) -> true
		| _ -> false
	in
	compat_aux t1 t2 || compat_aux t2 t1

let rank t = 
	let rank_aux n = match n with
	   Char -> 7
	   | Short -> 15
	   | Int -> 31
	   | Long -> 63
	in
	match t with
	| Tnum(Signed, n) -> rank_aux n
	| Tnum(Unsigned, n) -> 1 + rank_aux n
	| Tdouble -> 100
	| Tnull -> 0
	| _ -> assert false

let inf_type t1 t2 = 
	rank t1 < rank t2

let max_type t1 t2 = 
	if inf_type t1 t2 then t2
	else t1

exception TypeError of loc * string
let error loc msg = raise (TypeError (loc, msg))

let rec type_bf t =
	match t with
	  Tpointer tt -> type_bf tt
	| Tstruct id -> Hashtbl.mem struct_env id.node
	| _ -> true
;;

let type_var_decl vd = 
    let _ = 
	List.fold_left (fun seen (t , id) ->
			if type_bf t && t <> Tvoid && not (List.mem id.node seen) then
				id.node :: seen
			else error id.info "champ ou variable incorrect"
			) [] vd
    in ()	

let add_env env vd = 
	List.fold_left (fun acc (t, id) -> Env.add id.node t acc ) env

let type_const c = 
	match c with
	| Cstring _ -> Tpointer ( Tnum( Signed, Char ))
	| Cdouble _ -> Tdouble
	| Cint( Signed, Int, 0) -> Tnull
	| Cint( s, t, _) -> Tnum(s, t) 
		

let rec type_expr env e = 
	match e.node with 
	| Econst c -> let tc = type_const c in 
					mk_node tc (Econst c)
	| Eunop (unop, e0) -> 
		begin match unop with
		| Neg -> let te0 = type_expr env e0 in
			if not (arith te0.info) then 
				error e0.info "Type invalide pour -"
			else 
				mk_node te0.info (Eunop(Neg, te0))
		| Deref -> let te0 = type_lvalue env e0 in 
					assert false
		| Preincr | Postincr | Predecr | Postdecr 
			-> let te0 = type_expr env e0 in
			if not (arith te0.info) then 
				error e0.info "Type invalide pour -"
			else  
				mk_node te0.info (Eunop(unop, te0))
		end
	| Eaccess (e0, x) ->
			let te0 = type_expr env e0 in
			begin 
				match te0.info with
					Tstruct id -> 
						let fields = Hashtbl.find struct_env id.node in 
						begin try
							let t, _ =
								List.find (fun (t, y) -> y.node = x.node) fields
							in
							mk_node t (Eaccess (te0, x))
						with
							Not_found -> error x.info "Champ de structure inconnu"
						end	
				| _ -> error e.info " accès a une valeur non structurelle"
			end
	| Ecall (f, params) ->
		let tparams = List.map (type_expr env) params in
		begin
			try 
				let tret, _ , args = Hashtbl.find fun_env f.node in
				try 
					(* Compare les deux listes deux à deux pour tester la cohérence de type *)
					List.iter2 (fun e  (t, x) ->  
						if not (compatible e.info t) then
							error x.info ("Type invalide pour le paramètre " ^ x.node ^ " de " ^ f.node))
							tparams
							args;
					mk_node tret (Ecall(f, tparams))
				with
					(* Les listes ont deux tailles différentes *) 
					Invalid_argument _ -> error f.info ("Nombre d'argument invalide pour " ^ f.node)	
			with 
				Not_found -> error f.info ("La fonction " ^ f.node ^ " n'existe pas")
		end
	(*| Ebinop(e0,op,e1) -> 
		let te0 = type_expr env e0 in
		begin 
			match op with
			| And | Or -> 
			| Eq | Neq | Ge | Gt | Le | Lt ->
			| Add | Mult | Div | Sub | Mod ->
	
	|Eassign (v, e0) -> 
		begin 
			let te0 = type_expr env e0 in
			*) 
		
	| _ -> type_lvalue env e  

and type_lvalue env e =
		match e.node with
		|Eident id -> 
			let t = 
				try 
					try
						Env.find id.node env
					with
						Not_found -> 
						Hashtbl.find global_env id.node
				with
					Not_found -> error id.info ("Variable non définie " ^ id.node)
			 in 
			 mk_node t (Eident id)
		(* voir tous les cas *)
		| _ -> error e.info "Valeur gauche attendue"

let rec type_instr ty env t = 
	match t.node with 
	| Sskip -> mk_node Tvoid Sskip
	| Sexpr e -> let te = type_expr env e in 
							mk_node te.info (Sexpr te)
	| _ -> assert false 

let type_block ty env (var_decl, instrs )  = 
	(check_var_decl var_decl,
	List.map (type_instr ty env) instrs)

let type_decl d = 
	match d with 
	| Dvar ((typ, id )) -> 
		if typ = Tvoid then
			error id.info "type de variable invalide" 
		else  
				begin 
					add_global_env global_env id typ;
					Dvar (( typ, id ))
				end
	| Dstruct ( id, var_decl) -> 
		add_global_env struct_env id var_decl;
		let t_var_decl = check_var_decl var_decl in 
		Dstruct(id, t_var_decl )
		
	| Dfun (ty, ident, args , bloc) -> 
		if not (type_bf ty) then
			error ident.info "Type de retour invalide" 
		else 
			begin
				add_global_env fun_env ident (ty, ident, args); 
				let t_args = check_var_decl args in 
				let t_block = match bloc with 
				| None -> None 
				| Some body ->
					let env = List.fold_left(fun acc(t, id) -> 
						Env.add id.node t acc) Env.empty args
					in 
					let t_body = type_block ty env body in 
					Some t_body
			in 
				Dfun(ty, ident, t_args, t_block) 
			end 
	
let type_prog prog = 
	List.map (type_decl) prog 

	


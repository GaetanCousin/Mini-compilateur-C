open Ast

exception Type_error of (Ast.loc * string)

module Env =  Map.Make (String)

let error loc msg = raise(Type_error (loc, msg))
let mk_node t e = { info = t ; node = e }
let global_env = Hashtbl.create 17
let struct_env : (string, var_decl list)Hashtbl.t = Hashtbl.create 17
let fun_env = Hashtbl.create 17

(* Constantes de type *)
let signed_char = Tnum(Signed, Char)
let signed_int = Tnum(Signed, Int)
let unsigned_int = Tnum(Unsigned, Int)
let signed_long = Tnum(Signed, Long)
let unsigned_long = Tnum(Unsigned, Long)

(* let global_t_signed_int = Tnum(Signed, Int) *)


let calls = [|"";"";"";"";"";"";"";"";"";""|]
let nb_appel = [|0;0;0;0;0;0;0;0;0;0|]
let cpt = [|0|]

let cpt_call = ref 0

let nb_function = ref 0

let totoo = ref 0
let toto = ref 0








let add_global_env tab key v =
	if Hashtbl.mem tab key.node  then
		error key.info("Redéfinition de l'identifiant " ^ key.node)
	else
		Hashtbl.add tab key.node v


let num t  = match t with
	| Tstruct _ | Tvoid -> false
	| _ -> true

let arith t  = match t with
	| Tstruct _ | Tvoid | Tpointer _ -> false
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


let rec type_eq t1 t2 =
	match t1, t2 with
	| Tnull, Tnull
	| Tvoid, Tvoid
	| Tdouble, Tdouble -> true
	| Tnum(s1, k1), Tnum(s2, k2) -> s1 = s2 && k1 = k2
	| Tstruct id1, Tstruct id2 -> id1.node = id2.node
	| Tpointer p1, Tpointer p2 -> type_eq p1 p2
	| _ -> false

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

let typ_lte t1 t2 =
	inf_type t1 t2 || type_eq t1 t2

let max_type t1 t2 =
	if inf_type t1 t2 then t2
	else t1

let rec type_bf t =
	match t with
	| Tpointer tt -> type_bf tt
	| Tstruct id -> Hashtbl.mem struct_env id.node
	| _ -> true

let mk_node t e = { info = t ; node = e }

let mk_cast t e =
	if not (compatible t e.info) then assert false
	else
		mk_node t (Ecast(t,e))

let type_var_decl vd =
    let _ =
	List.fold_left (fun seen (typ , id) ->
			if typ = Tvoid || not (type_bf typ) then
				error id.info "type de variable incorrect"
			else if List.mem id.node seen then
				error id.info ("Redéfinition de l'identifiant " ^ id.node)
			else
          		id.node :: seen) [] vd
  	in vd

let add_env env vd =
	List.fold_left (fun acc (t, id) -> Env.add id.node t acc ) env vd

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
				error e0.info "Type invalide pour (!)"
			else
				mk_node te0.info (Eunop(Neg, te0))
		| Deref -> let te0 = type_expr env e0 in
					begin
						match te0.info with
						| Tpointer tt -> mk_node tt (Eunop(unop,te0))
						| _ -> error e0.info "Type invalide pour *"
					end
		| Addr -> let te0 = type_lvalue env e0 in
					mk_node (Tpointer(te0.info)) (Eunop(unop,te0))
		| Preincr | Postincr | Predecr | Postdecr
			-> let te0 = type_expr env e0 in
			if not (num te0.info) then
				error e0.info "Type invalide pour ++ ou --"
			else
				mk_node te0.info (Eunop(unop, te0))
		| Not -> let te0 = type_expr env e0 in
			if not (num te0.info) then
				error e0.info "Type invalide pour -"
			else
				mk_node signed_int (Eunop(unop, te0))
		end
	| Estructvar (e0, x) ->
			type_eaccess type_lvalue env e0 x

	| Ecall (f, params) ->
		let tparams = List.map (type_expr env) params in
		begin
			try
				let tret, _ , args, _ = Hashtbl.find fun_env f.node in
				try
					let new_params =
						if args = [] then tparams
						else
						(* Compare les deux listes deux à deux pour tester la cohérence de type *)
						List.map2 (fun e  (t, x) ->
							if not (compatible e.info t) then
								error x.info ("Type invalide pour le paramètre " ^ x.node ^ " de " ^ f.node)
								else
									mk_node t (Ecast(t,e))
								)
								tparams
								args
					in
					mk_node tret (Ecall(f, new_params))
				with
					(* Les listes ont deux tailles différentes *)
					Invalid_argument _ -> error f.info ("Nombre d'argument invalide pour " ^ f.node)
			with
				Not_found -> error f.info ("La fonction " ^ f.node ^ " n'existe pas")
		end
	| Ebinop(e1, op, e2) ->
		let te1 = type_expr env e1 in
		let te2 = type_expr env e2 in
		let t1 = te1.info in
		let t2 = te2.info in
		let nte1, nte2 =
			if arith t1 && arith t2 && not (type_eq t1 t2) then
				if type_eq t1 Tdouble then te1, mk_cast Tdouble te2
				else if type_eq t2 Tdouble then mk_cast Tdouble te1, te2
				else
					let te1 = if inf_type t1 signed_int then mk_cast signed_int te1 else te1 in
					let te2 = if inf_type t2 signed_int then mk_cast signed_int te2 else te2 in
					let t1 = te1.info in
					let t2 = te2.info in
					if type_eq t1 unsigned_long then te1, mk_cast unsigned_long te2
					else if type_eq t2 unsigned_long then mk_cast unsigned_long te1, te2
					else if type_eq t1 signed_long then te1, mk_cast signed_long te2
					else if type_eq t2 signed_long then mk_cast signed_long te1, te2
					else if type_eq t1 unsigned_int then te1, mk_cast unsigned_int te2
					else if type_eq t2 unsigned_int then mk_cast unsigned_int te1, te2 else te1, te2
			else
				te1, te2
		in
		let t1 = nte1.info in
		let t2 = nte2.info in
		begin
			match op with
			| And | Or -> if compatible t1 t2 && compatible t1 Tdouble
						  then mk_node signed_int (Ebinop(nte1,op,nte2))
						  else error e.info "Type invalide pour && ou ||"
			| Add | Sub -> begin
							match t1,t2 with
							| Tpointer pt1, Tpointer pt2 when op = Sub && type_eq pt1 pt2 ->
					 			mk_node signed_long (Ebinop (nte1, Sub, nte2))
					 		|Tpointer (_), _ -> if(arith t2 && typ_lte (max_type t2 unsigned_long ) unsigned_long )
					 							then mk_node t1 (Ebinop(nte1,op,nte2))
					 							else error e.info "Type invalide pour + ou -"
					 		| _ , Tpointer pt2 when arith t1 && typ_lte (max_type t1 unsigned_long) unsigned_long
					 			&& op = Add->
					 			mk_node t2 (Ebinop (nte1, Add, nte2))
					 		| _ -> if compatible t1 t2 && compatible t2 Tdouble
								   then mk_node (max_type t1 t2) (Ebinop(nte1,(op),nte2))
								   else error e.info "Type invalide pour + ou -"
						   end

			| Mult | Div -> if compatible t1 t2 && compatible t2 Tdouble
									   then mk_node (max_type t1 t2) (Ebinop(nte1,op,nte2))
									   else error e.info "Type invalide pour * ou /"
			| Eq | Neq | Ge | Gt | Le | Lt -> if compatible t1 t2
											 then mk_node signed_int (Ebinop(nte1,op,nte2))
											 else error e.info "Type invalide pour ==, !=, >, >=, <, <="
			| Mod -> if compatible t1 t2 && typ_lte (max_type t1 t2) unsigned_long
					 then mk_node (max_type t1 t2) (Ebinop(nte1,op,nte2))
					 else error e.info "Type invalide pour %"

			| _ -> assert false
		end
	| Eassign (e1, e2) ->
			let te1 = type_lvalue env e1 in
			let te2 = type_expr env e2 in
			if not(compatible te1.info te2.info) then
				error e1.info "Type incompatible pour l'affectation"
			else
				mk_node te1.info (Eassign(te1, mk_cast te1.info te2))
	| _ -> type_lvalue env e
and type_lvalue env e =
		match e.node with
		| Eident id ->
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
		|Eunop(Deref,e0) -> let te0 = type_expr env e0 in
						begin
							match te0.info with
							| Tpointer(tt) -> mk_node tt (Eunop(Deref,te0))
							| _ -> error e0.info "Type invalide pour []"
						end
		| _ -> error e.info "Valeur gauche attendue"

and type_eaccess f_type env e0 x =
	let te0 = f_type env e0 in
			begin
				match te0.info with
					Tstruct id ->
						let fields = Hashtbl.find struct_env id.node in
						begin try
							let t, _ =
								List.find (fun (t, y) -> y.node = x.node) fields
							in
							mk_node t (Estructvar (te0, x))
						with
							Not_found -> error x.info "Champ de structure inconnu"
						end
				| _ -> error e0.info " accès a une valeur non structurelle"
			end

(* vérifier qu'il y a un return du bon type pour chaque instruction *)
let rec type_instr ty env t =
	match t.node with
	| Sskip -> mk_node Tvoid Sskip
	| Sexpr e -> let te = type_expr env e in
							mk_node te.info (Sexpr te)
	| Sif (e,i1,i2) ->
		let te = type_expr env e in
		if not (num te.info) then  error e.info "il faut un type num dans la condition if"
		else let ti1 = type_instr ty env i1 in
			 let ti2 = type_instr ty env i2 in
			 mk_node Tvoid (Sif(te,ti1,ti2))
	| Sfor (e1, c, e2, i) ->
		let  te1 = List.map (type_expr env) e1 in
		let tc = match c with
		|None -> None
		|Some c -> let tc = type_expr env c in
			    if not (num tc.info) then  error c.info "il faut un type num dans la condition for"
				else Some(tc)
		in
		let te2 = List.map (type_expr env) e2  in
		let ti = type_instr ty env i in
		mk_node Tvoid (Sfor(te1,tc,te2,ti))
	| Sblock b -> let tb = type_block ty env b in
		mk_node ty (Sblock(tb))

	| Sreturn e ->
		nb_function := !nb_function + 1;
		let te = match e with
		| None -> None
		| Some e -> Some(type_expr env e)
		in
		mk_node Tvoid (Sreturn te)


and type_block ty env (var_decl, instrs )  =
(*	(check_var_decl var_decl,
	List.map (type_instr ty env) instrs) *)

  let tvars = type_var_decl var_decl in
  let new_env = add_env env var_decl in
  let tinstrs = List.map (fun i ->
      let ti = type_instr ty new_env i in

      ti) instrs
  in
  (tvars, tinstrs)

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
		
		(* List.append liste_function [ident]; *)
		if not (type_bf ty) then
			error ident.info "Type de retour invalide"
		else
			begin
				add_global_env fun_env ident (ty, ident, args, bloc = None);
				let t_args = check_var_decl args in
				let t_block = match bloc with
				| None -> None
				| Some body ->
					let env = add_env Env.empty args in
					let t_body = type_block ty env body in
					Some t_body
			in
				Dfun(ty, ident, t_args, t_block)
			end

let type_prog prog =
	let t_prog = List.map (type_decl) prog in
	if not (Hashtbl.mem fun_env "main") then assert false
	else t_prog

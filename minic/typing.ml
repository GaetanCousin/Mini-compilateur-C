open Ast

module Env =  Map.Make (String)

let global_env = Hashtbl.create 17
let struct_env : (string, var_decl list)Hashtbl.t = Hashtbl.create 17
let fun_env = Hashtbl.create 17

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

let type_block t env block = assert false
(*	match block with ( vdl , insl) -> List.iter (type_var_decl) vdl;  List.iter (type_instr) t env insl;  *)
 
(*assert false *)

(*let type_instr t env ins = assert false *)

(*type instr et expr match.node*)

let type_decl d = 
	match d with 
	|Dvar (t, i) -> if type_bf t && t <> Tvoid && not (Hashtbl.mem global_env i.node) then begin
				Hashtbl.add global_env i.node t
			end else
				error i.info "declaration globale invalide"
	| Dstruct (id, var_decl) -> if Hashtbl.mem struct_env id.node then
					error id.info ("Redefinition de lq structure" ^ id.node)
				   else begin
					Hashtbl.add struct_env id.node var_decl;
					type_var_decl var_decl
				   end
	| Dfun (t, f, params, b) ->	
		if not (type_bf t) then error f.info ("Type de retour invalide pour " ^ f.node);
		if Hashtbl.mem fun_env f.node then error f.info ("Redefinition de la fonction" ^f.node);
		type_var_decl params;
		Hashtbl.add fun_env f.node (t, params);
		match b with
		 None -> ()
		| Some block -> let env = add_env Env.empty params in
			 type_block t env block

let type_prog l = 
	List.iter (type_decl) l

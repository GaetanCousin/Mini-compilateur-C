open Ast 

module Env = Map.Make (String)
let global_env = Hashtbl.create 17
let struct_env : (string , var_decl list)Hashtbl.t = Hashtbl.create 17
let fun_env = Hashtbl.create 17


exception TypeError of loc * string 
let error loc msg = raise (TypeError (loc , msg))

let rec type_bf t = 
	match t with 
	| Tpointer tt -> type_bf tt 
	| Tstruct id -> Hashbl.mem struct_env id.node
	| _ -> true 
	
;;


let type_var_decl vd = 
	let _ = 
		List.fold_left 
		(fun seen (t,td) -> 
			if type_bf t 
			&& t <> Tvoid 
			&& not (List.mem id.node seen) then 
				id.node :: seen 
			else error id.info "Champ ou variable inccorect" ) [] vd
	in
	()

let type_decl d = 
	match d with 
	|Dvar (t, i) -> if  type_bf t  
						&& t <> Tvoid 
						&& not (Hashtbl.mem global_env i.node) then begin
						Hashtbl.add global_env i.node t 
					and else 
						error i.info "Declaration globale invalide" 
						
	| Dstruct ( id, var_decl) -> if Hashtbl.mem struct_env id.node then 
									error id.info ("Redéfinition de la structure" ^ id.node)
								 else begin 
									Hashtbl.add struct_env id.node var_decl;
									type_var_decl var_decl 
								 end 
								 
	| Dfun ( tn f, params, b ) -> 
		if not (type_bf t) then error f.info("type de retour invalide pour " ^ f.node );
		if Hashtbl.mem fun_env f.node then error f.info ("redefinition de la fonction" ^ f.node );
		type_var_decl params ;
		Hashtbl.add fun_env f.node ( t, params );
		match b with 
		| None -> () 
		| Some block -> let env = add_env Env.empty params in 
							type_block t env block 
							
let type_prog l =
	List.iter (type_decl) l
	

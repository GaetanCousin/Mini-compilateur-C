open Ast 
open Amd64
open Typing


let size_of t = 
	match t with
	| Tvoid | Tnull -> assert false 
	| Tnum (_, Char) -> 1
	| Tnum (_, Short) -> 2
	| Tnum (_, Int) -> 4
	| Tnum (_, Long) | Tdouble | Tpointer _ -> 8
	| Tstruct _ -> assert false 

let align_of t = 
	match t with
	| Tstruct _ -> assert false 
	| _ -> size_of t



let round8 n = 
	if n mod 8 = 0 then n else ((n/8) + 1 ) * 8;;


let compile_const c = assert false 
let compile_cast t = assert false 

let rec compile_expr_reg env e = 
(* code qui place le résultat dans r10 *)
	match e.node with 
	| Econst c -> compile_const c 
	
	| Ecast (t, e0) -> 
	  compile_expr_reg env e0
	  ++ compile_cast t 
	  
	| _ -> failwith "todo"

(* renvoie (max_offset, code) *)
let rec compile_expr env e = 
	match e.info with 
	| Tstruct _ -> assert false (* A voir si on le traite ou pas *)
	| Tvoid -> compile_expr_reg env e
	
	| t when size_of t = 8 -> 
		compile_expr_reg env e ++ pushq ~%r10
		
	| t -> let n = size_of t in
	  let mask = if n = 4 then 0xffffffff
		else if n = 2 then 0xffff
		else 0xff
	  in
	  andq ~$mask ~%r10 ++
	  pushq ~%r10
	
		   
			



let compile_clean_expr env e =
	let ecode = compile_expr env e in
	ecode ++ (if e.info = Tvoid then nop else 
				popq ~%r10)

let rec compile_instr lab_fin rbp_offset env i = 
	match i.node with 
	| Sskip -> rbp_offset, nop
	| Sexpr e -> rbp_offset, compile_clean_expr env e 
	| Sreturn oe -> 
	  rbp_offset, (
		match oe with 
		| None -> nop
		| Some e -> compile_expr env e 
	  ) ++ jmp lab_fin
	  
 (* | d'autre a ajouter ici *)


(* renvoie (max_offset, code) *)
let compile_block lab_fin rbp_offset env (var_decls, instrs) =
	let new_offset, new_env, debug = 
		List.fold_left ( fun (aoffset, aenv, debug) (t, x) -> 
			let aenv = Env.add x.node aoffset aenv in
			let offset = aoffset - round8 (size_of t) in
			let debug = debug ++ 
						comment (Printf.sprintf "local: %s rbp[%d]"
									x.node aoffset)
			in
			(offset, aenv, debug)
		) (rbp_offset, env, nop) var_decls  
	in
	List.fold_left ( fun ( aoffset, acode) i -> 
		let ioffset , icode =
			compile_instr lab_fin new_offset new_env i
		in
		(min ioffset aoffset,
		acode ++ icode)) (new_offset, nop) instrs  
	

let compile_decl (atext, adata) d =
	match d with 
	| Dstruct _ -> assert false
	| Dvar (t, id) -> 
		atext,
		let n = size_of t in
		let a = align_of t in
		adata ++
		label id.node ++ 
			align a ++
			space n 
			
	| Dfun (_, _, _, None) -> atext, adata
	| Dfun (tret, f, params, Some body) ->
		
		let last_offset, env = 
			List.fold_left ( fun (aoffset, aenv) (t, x) -> 
				let offset = aoffset + round8 (size_of t) in 
				let aenv = Env.add x.node offset aenv in
				(offset, aenv)
			) (8, Env.empty) params 
		in
		let ret_offset = last_offset + round8 ( size_of tret) in
		let env = Env.empty in
		let lab_fin = f.node ^"_fin" in
		let max_rbp_offset, body_code = compile_block lab_fin (-8) env body in
		let code =
			glabel f.node ++
				comment (" On rentre dasn la fonction " ^ f.node) ++ 
				pushq ~%rbp ++
				mov ~%rsp ~%rbp ++
				addq ~$max_rbp_offset ~%rsp ++
				body_code ++ 
				
				(* beaucoup de chose à faire *)
				
				
				label lab_fin ++ 
				(if (tret <> Tvoid) then
					if f.node = "main" then popq ~%rax
					else
						popq ~%r10 ++
						mov ~%r10 (addr ~ofs:ret_offset ~%rbp)
				else nop) ++ 
				
				
				mov ~%rbp ~%rsp ++
				popq ~%rbp ++
				ret
				
		in
		atext ++ code , adata
			
		
let compile_prog p = 
	let text, data =
		List.fold_left compile_decl (nop, nop) p
	in 
{ 
	text = nop;
	
	data = nop;
}

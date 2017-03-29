open Ast 
open Amd64
open Typing

let string_env  = Hashtbl.create 17

let gen_label =
	let counter = ref (-1) in
	fun prefix -> Printf.sprintf "label_%s_%d" prefix !counter

let int_registers = [ rdi ; rsi ; rdx ; rcx ; r8 ; r9 ]

let rec fold2 f acc l1 l2 = 
	match l1, l2 with 
	| [], _ -> acc 
	| _, [] -> failwith "fold2"
	| x1 :: ll1, x2 :: ll2 ->
	fold2 f (f acc x1 x2) ll1 ll2 


let rec size_of t = 
	match t with
	| Tnull -> 8
	| Tvoid -> 0
	| Tnum (_, Char) -> 1
	| Tnum (_, Short) -> 2
	| Tnum (_, Int) -> 4
	| Tnum (_, Long) | Tdouble | Tpointer _ -> 8
	| Tstruct id -> assert false  
	
let is_signed t = match t with
	| Tnum(Signed, _ ) | Tpointer _ | Tnull -> true
	| _ -> false 
  
 
let align_of t = 
	match t with
	| Tstruct _ -> assert false 
	| _ -> size_of t



let round8 n = 
	if n mod 8 = 0 then n else ((n/8) + 1 ) * 8;;


let compile_const c = 
	match c with 
	| Cint (s, Int, i) -> movl ~$i ~%r10d
	| Cint (_, _, i) -> (* Long *) 
	  movabsq (string_of_int i) ~%r10
	
	| Cstring s -> 
		let label = 
			try 
				Hashtbl.find string_env s 
			with
				Not_found ->
				let lab = gen_label "string" in 
				Hashtbl.add string_env s lab;
				lab
		in
		mov ~:label ~%r10  


	| Cdouble _ -> assert false 


let reg_size_of t =
	match size_of t with 
	| 1 -> `b
	| 2 -> `w
	| 4 -> `l
	| _ -> `q
	
(* cast la valeur dans r10 de type tfrom en type tto *)
let compile_cast tfrom tto =
	let tfrom = if tfrom = Tnull then Tnum(Unsigned, Long) else tfrom in
	let tto = if tto = Tnull then Tnum(Unsigned, Long) else tto in 
	let size_tfrom = size_of tfrom in
	let size_tto = size_of tto in
	match tfrom, tto with 
	| (Tvoid | Tstruct _ ), _ -> assert false 
	| _, (Tvoid | Tstruct _) -> assert false 
	| _ when size_tfrom = size_tto -> nop
	| _ when size_tto < size_tfrom -> 
	  let mask = (1 lsl (size_tto * 8) ) - 1 in
	  andq ~$mask ~%r10
	| Tnum (Signed, Char), Tnum(_, Short) ->  movsbw ~%r10b ~%r10w
	| Tnum (Unsigned, Char), Tnum(_, Short) -> movzbw ~%r10b ~%r10w
	| Tnum (Signed, Char), Tnum(_, Int) -> movsbl ~%r10b ~%r10d
	| Tnum (Unsigned, Char), Tnum(_, Int) -> movzbl ~%r10b ~%r10d 
	| Tnum (Signed, Char), Tnum(_, Long) -> movsbq ~%r10b ~%r10
	| Tnum (Unsigned, Char), Tnum(_, Long) -> movzbq ~%r10b ~%r10
	
	| Tnum (Signed, Short), Tnum(_, Int) -> movswl ~%r10w ~%r10d
	| Tnum (Unsigned, Short), Tnum(_, Int) -> movzwl ~%r10w ~%r10d
	| Tnum (Signed, Short), Tnum(_, Long) -> movswq ~%r10w ~%r10
	| Tnum (Unsigned, Short), Tnum(_, Long) -> movzwq ~%r10w ~%r10
	
	| Tnum (Signed, Int), Tnum(_, Long) -> movslq ~%r10d ~%r10 
	| Tnum (Unsigned, Int), Tnum(_, Long) -> andq ~$0xffffffff ~%r10
	
	| _ -> assert false   


let rec compile_lvalue_reg env e = 
	(* code qui place l'adresse de e dans r10 *)
	match e.node with 
	| Eident x -> 
	  begin 
		try 
			let offset = Env.find x.node env in
			(* mov ~%rbp ~%r10 ++ 
			   addq ~%offset ~%r10
			*)
			leaq (addr ~%rbp ~ofs:offset) ~%r10
		with Not_found -> 
			movq ~:(x.node) ~%r10
	  end
	| _ -> failwith "todo lvalue"
	
	
and compile_expr_reg env e = 
(* code qui place le résultat dans r10 *)
	match e.node with 
	| Econst c -> compile_const c 
	
	| Eunop ( unop, e ) -> 
	  begin
		match unop with 
		| Neg -> failwith __LOC__
		| Deref -> failwith __LOC__
		| Preincr -> failwith __LOC__
		| Postincr -> failwith __LOC__
		| Predecr -> failwith __LOC__
		| Postdecr -> failwith __LOC__
		| Not -> failwith __LOC__
		| Addr -> failwith __LOC__
	  end
		
	| Ebinop (e1, op, e2) -> 
		let e1code = compile_expr env e1 in
		let e2code = compile_expr env e2 in
		e1code++
		e2code++ (* e2 dans r10 *)
		popq ~%r11 ++ (* e1 dans r11 *)
		begin 
			match op with
			| Div | Mod -> 
				let rsize = reg_size_of e1.info in
				let ra = rax_ rsize in
				let rd = rdx_ rsize in
				let re2 = r10_ rsize in
				let re1 = r11_ rsize in
				mov ~%re1 ~%ra ++ 
				(if is_signed e1.info then 
					(if rsize = `q then
						cqto ++ idivq ~%r10
					else
						cltd ++ idivl ~%r10d)
				 else
						xor ~%rd ~%rd ++ 
						(if rsize = `q then divq ~%r10
						 else divl  ~%r10d)
				)
				  ++ (if op = Div then mov ~%ra ~%re2 
					  else mov ~%rd ~%re2)
				
			| Add -> 
				
				
			| Sub -> failwith __LOC__
			| Mult -> failwith __LOC__
			| Eq -> failwith __LOC__
			| Neq -> failwith __LOC__
			| Ge -> failwith __LOC__
			| Gt -> failwith __LOC__
			| Le -> failwith __LOC__
			| Lt -> failwith __LOC__
			| And -> failwith __LOC__
			| Or -> failwith __LOC__ 
		end

	| Eident _ | Eunop (Deref, _) | Estructvar _ ->

		let reg10 = r10_ (reg_size_of e.info) in	 
	 	compile_lvalue_reg env e ++ 
	 	mov (addr ~%r10) ~%reg10

	| Ecall (f, args) -> 
		let tret, _, _, extern = Hashtbl.find fun_env f.node in
		if extern then
			let arg_code = fold2 (fun (a_code) e r -> 
					a_code ++
					compile_expr env e ++
					popq ~%r) nop args int_registers
			in
			arg_code ++
			xorq ~%rax ~%rax ++ (* on met le registre a 0 *)
			call f.node ++ 
			mov ~%rax ~%r10
		else 
			let size_ret = round8 (size_of tret) in
			let arg_size, arg_code =
				List.fold_left (fun (a_size, a_code) e -> 
					(a_size + round8 (size_of e.info),
					compile_expr env e ++ a_code) ) (0, nop)
				args
			in
			subq ~$size_ret ~%rsp ++
			arg_code ++
			call f.node ++
			addq ~$arg_size ~%rsp ++
			if ( tret <> Tvoid ) then popq ~%r10 else nop

	| Eunop (Addr, e0) -> compile_lvalue_reg env e0

	| Eassign (e1, e2) ->
		(* r10 = e1 et r11 = e2 *)
		let e2code = compile_expr env e2 in
		let e1code = compile_lvalue_reg env e1 in
		let reg = r11_ (reg_size_of e1.info) in
		e2code ++
		e1code ++
		popq ~%r11 ++ 
		mov ~%reg (addr ~%r10) ++
		movq ~%r11 ~%r10  

	
	| Ecast (t, e0) -> 
	  compile_expr_reg env e0
	  ++ compile_cast e0.info t 
	  
	| _ -> failwith __LOC__



(* renvoie (max_offset, code) *)
and compile_expr env e = 
	match e.info with 
	| Tstruct _ -> assert false (* A voir si on le traite ou pas *)
	| Tvoid -> compile_expr_reg env e
	
	| t when size_of t = 8 -> 
		compile_expr_reg env e ++ pushq ~%r10
		
	| t -> let n = size_of t in
	  let mask = ( 1 lsl n * 8) - 1 in
	  compile_expr_reg env e ++
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
	| Sif (e, i1, i2) ->  
		let e_comp = compile_expr env e in
		let offset1, i1_comp = compile_instr lab_fin rbp_offset env i1 in
		let offset2, i2_comp = compile_instr lab_fin rbp_offset env i2 in
		let label_else = gen_label "else_" in
		let label_end_if = gen_label "end_if_" in
    
    max offset1 offset2,
    e_comp ++
    popq ~%r10 ++
    cmpq ~$0 ~%r10  ++
    je label_else ++
    i1_comp ++
    jmp label_end_if ++
    label label_else ++
    i2_comp ++
    label label_end_if
	 
	| Sfor (l1, e, l2, i) ->
		let e_comp = 
			match e with
			| None -> pushq ~$1
			| Some e -> compile_expr env e 
		in
		let l1_comp = List.fold_left (fun acc e -> acc ++ compile_clean_expr env e) nop l1 in 
		let l2_comp = List.fold_left (fun acc e -> acc ++ compile_clean_expr env e) nop l2 in 
		let offset, i_comp = compile_instr lab_fin rbp_offset env i in
		let label_for = gen_label "for_" in
		let label_end_for = gen_label "end_for_" in
		
		offset, l1_comp ++
		label label_for ++ 
		e_comp ++
		popq ~%r10 ++ 
		cmpq ~$0 ~%r10 ++ 
		je label_end_for ++
		i_comp ++ 
		l2_comp ++
		jmp label_for ++
		label label_end_for 	
	
	| Sblock b -> compile_block lab_fin rbp_offset env b 


(* renvoie (max_offset, code) *)
and compile_block lab_fin rbp_offset env (var_decls, instrs) =
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
	let data = Hashtbl.fold  ( fun str lbl a_data -> 
		a_data ++
		label lbl ++
		string str) string_env data
	in 
{ 
	text = text;
	
	data = data;
}

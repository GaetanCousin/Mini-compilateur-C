open Ast 
open Amd64
open Typing

let string_env  = Hashtbl.create 17
let double_env = Hashtbl.create 17


let gen_label =
  let count = ref ~-1 in
  fun prefix -> incr count;
    prefix ^ (string_of_int !count)
;;

let int_registers = [ rdi ; rsi ; rdx ; rcx ; r8 ; r9 ]
let double_registers = [xmm0 ; xmm1 ; xmm2 ; xmm3 ; xmm4 ; xmm5 ; xmm6 ; xmm7]

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
	

let deref t = match t with
    Tpointer tt -> tt
  | _ -> assert false
	
let is_double d = 
	match d with
	| Tdouble -> true 
	| _ -> false 
	
	
let is_signed t = 
	match t with
	| Tnum(Signed, _ ) | Tpointer _ | Tnull -> true
	| _ -> false 
  
 let is_pointer t = 
	match t with
	| Tpointer tt -> true
	| _ ->  false
 
 let get_pointer t = 
	match t with
	| Tpointer tt -> tt
	| _ -> assert false
   
 
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
				let lab = gen_label "string_" in 
				Hashtbl.add string_env s lab;
				lab
		in
		mov ~:label ~%r10  


	| Cdouble d ->  
		let label = 
			try 
				Hashtbl.find double_env d 
			with
				Not_found -> 
				let lab = gen_label "double_" in (* attention *)
				Hashtbl.add double_env d lab;
				lab
		in
		mov ~:label ~%r10  ++
		mov (addr ~%r10) ~%r10


let reg_size_of t =
	match size_of t with 
	| 1 -> `b
	| 2 -> `w
	| 4 -> `l
	| _ -> `q
	
	
let reg_size_of_int t =
  match size_of t with
  | 4 -> `l
  | 8 -> `q
  | _ -> assert false
  
  
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
	begin
	match e.node with 
	| Econst c -> compile_const c 
	
	| Eunop ( unop, e ) -> 
	  let reg10 = r10_ (reg_size_of e.info) in
	  let reg11 = r11_ (reg_size_of e.info) in
	  begin
		match unop with 
		| Neg -> let ecode = compile_expr_reg env e in 
				ecode ++ (
				if is_double e.info then
					movq ~%r10 ~%xmm15 ++
					xor ~%r10 ~%r10 ++
					cvtsi2sdq ~%r10 ~%xmm14 ++
					subsd ~%xmm15  ~%xmm14 ++
					movq ~%xmm14 ~%r10
				else
					neg ~%reg10 )
		
		(* Deref est géré plus bas dans la fonction *)
		(* | Deref -> 	assert false *)
			
		| Preincr | Postincr | Predecr | Postdecr ->

        let ecode = compile_lvalue_reg env e in
        let te = e.info in
        let i = (if unop = Preincr ||  unop = Postincr then 1 else -1) in
        let i = i * if is_pointer e.info then size_of (deref e.info) else 1 in
        let pre = unop = Preincr || unop = Predecr in
        ecode ++
        if is_double te then
			movsd (addr ~%r10) ~%xmm15 ++
			movq ~$i ~%r11 ++
			cvtsi2sdq ~%r11  ~%xmm14 ++
			(
			if pre then
				addsd ~%xmm14 ~%xmm15 ++ movsd ~%xmm15 (addr ~%r10)
            else
				addsd ~%xmm15 ~%xmm14 ++ movsd ~%xmm14 (addr ~%r10)
			)
			++ movq ~%xmm15 ~%r10
		else
			let ri = r11_ (reg_size_of te) in
			let rres = r10_ (reg_size_of te) in
			movq ~%r10 ~%r12 ++
			mov ~$i ~%ri ++
			mov (addr ~%r12) ~%rres ++
            if pre then
				add ~%ri ~%rres ++  mov ~%rres (addr ~%r12)
            else
				add ~%rres ~%ri ++ mov ~%ri (addr ~%r12)		

		| Not -> 	let ecode = compile_lvalue_reg env e in
					ecode ++
					(
					if is_double e.info then
						xorq ~%r10 ~%r10 ++
						cvtsi2sdq ~%r10 ~%xmm14 ++
						ucomisd ~%xmm14 ~%xmm15
					else
						test ~%reg10 ~%reg10
						) ++
					sete ~%r10b ++ movzbl ~%r10b ~%r10d
		
		| Addr -> compile_lvalue_reg env e
	  end
		
	| Ebinop (e1, op, e2) -> 
		let e1code = compile_expr env e1 in
		let e2code = compile_expr env e2 in
		let reg10 = r10_ (reg_size_of_int e.info) in
		let reg11 = r11_ (reg_size_of_int e.info) in
		e1code++
		e2code++ (* e2 dans r10 *)
		popq ~%r11 ++ (* e1 dans r11 *)
		popq ~%r10 ++
		begin 
			match op with
        Add | Sub | Mult | Div when is_double e1.info ->
                assert(is_double e2.info);
                (* e1: xmm15, e2: xmm14 *)
                movq ~%r11 ~%xmm15 ++
                  movq ~%r10 ~%xmm14 ++
                  (List.assoc op
                              [Add, addsd; Sub, subsd; Mult, mulsd; Div, divsd])
                    ~%xmm14 ~%xmm15  ++
                  movq ~%xmm15 ~%r10
        | Add | Sub when (arith e1.info && arith e2.info) || (is_pointer e1.info && is_pointer e1.info) ->
         let ra = r10_ (reg_size_of_int e.info) in
         let rb = r11_ (reg_size_of_int e.info) in
         (if op = Add then add else sub) ~%ra ~%rb ++ mov ~%rb ~%ra

        | Add | Sub  when is_pointer e1.info ->
           imulq ~$(size_of (deref e1.info)) ~%r10 ++
             (if op = Add then add else sub) ~%r11 ~%r10
        | Add when is_pointer e2.info ->
           imulq ~$(size_of (deref e2.info)) ~%r11 ++
             add ~%r11 ~%r10

        | Mult ->
           let ra = r11_ (reg_size_of_int e.info) in
           let rb = r10_ (reg_size_of_int e.info) in
           imul ~%ra ~%rb
        | Div | Mod  ->
           movq ~%r11 ~%rax ++
             let rsize = reg_size_of_int e1.info in
             let r = rax_ rsize in
             let rr = rdx_ rsize in
             let rres = r10_ rsize in
             (if rsize = `q then
                cqto ++ idivq ~%r10
              else
                cltd ++ idivl ~%r10d) ++
               mov (if op = Div then ~%r else ~%rr) ~%rres
			
			
			| _ -> 
			
				let comp =
				match op with
				| Eq -> sete ~%r10b
				| Neq -> setne ~%r10b 
				| Ge -> setge ~%r10b
				| Gt -> setg ~%r10b
				| Le -> setle ~%r10b
				| Lt -> setl ~%r10b
				| _ -> assert false
				in
				
				cmp ~%reg11 ~%reg10 ++
				
				comp ++
				movzbq ~%r10b ~%r10
				
			end
			
			
	| Eident _ | Eunop (Deref, _) | Estructvar _ ->

		let reg10 = r10_ (reg_size_of e.info) in	 
	 	compile_lvalue_reg env e ++ 
	 	mov (addr ~%r10) ~%reg10
	 	

	| Ecall (f, args) -> 
		let tret, _, _, extern = Hashtbl.find fun_env f.node in
		if extern then
			let n_double, arg_code =
				assign_regs env args int_registers double_registers (0, nop)
			in
			arg_code ++
			mov ~$n_double ~%rax ++
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


	end

(* renvoie (max_offset, code) 00000000 00000000 00000000 11111111*)
and compile_expr env e = 
	match e.info with 
	| Tstruct _ -> assert false (* A voir si on le traite ou pas *)
	| Tvoid -> compile_expr_reg env e
	
	| t when size_of t = 8 -> 
		compile_expr_reg env e ++ pushq ~%r10
		
	| t -> let n = size_of t in
	  let mask = ( 1 lsl (n * 8)) - 1 in
	  compile_expr_reg env e ++
	  andq ~$mask ~%r10 ++
	  pushq ~%r10
	
		   

and assign_regs env args iregs dregs (d_acc, code_acc) = 
	match args, iregs, dregs with
	| [], _, _ -> d_acc, code_acc
	| e :: _, _, [] when e.info = Tdouble -> assert false 
	| e :: _, [], _ -> assert false 
	
	| e :: next_args, _, dreg :: next_dregs when e.info = Tdouble -> 
		assign_regs env next_args iregs next_dregs
			( 1 + d_acc, code_acc ++ compile_expr env e ++ popd ~%dreg)
			
	| e :: next_args, ireg :: next_iregs, _ -> 
		assign_regs env next_args next_iregs dregs
			(d_acc, code_acc ++ compile_expr env e ++ popq ~%ireg)
		



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
		cmpq ~$0 ~%r10  ++ 
		je label_end_for ++
		i_comp ++ 
		l2_comp ++
		jmp label_for ++
		label label_end_for 	
	
	| Sblock b -> compile_block lab_fin rbp_offset env b 


(* renvoie (max_offset, code) *)
and compile_block lab_fin rbp_offset env (var_decls, instrs) =
  let new_offset, new_env, debug =
    List.fold_left (fun (aoffset, aenv, debug) (t, x) ->
        let aenv = Env.add x.node aoffset aenv  in
        let offset = aoffset - round8 (size_of t) in
        let debug = debug ++
                    comment (Printf.sprintf "local: %s rbp[%d]"
                               x.node aoffset)
        in
        (offset, aenv, debug)
      ) (rbp_offset, env, nop) var_decls
  in
  List.fold_left (fun (aoffset, acode) i ->
      let ioffset, icode =
        compile_instr lab_fin new_offset new_env i
      in
      (min ioffset aoffset,
       acode ++ icode)) (new_offset, debug) instrs  
	
	
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
		(* let env = Env.empty in *)
		let lab_fin = f.node ^"_fin" in
		let max_rbp_offset, body_code = compile_block lab_fin (-8) env body in
		let code =
			glabel f.node ++
				comment (" On rentre dans la fonction " ^ f.node) ++ 
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

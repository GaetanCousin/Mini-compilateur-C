.text
	.globl f
f:
# On rentre dasn la fonction f
	pushq %rbp
	mov %rsp, %rbp
	addq $-8, %rsp
	movq $x, %r10
	mov 0(%r10), %r10d
	andq $127, %r10
	pushq %r10
	jmp f_fin
f_fin:
	popq %r10
	mov %r10, 24(%rbp)
	mov %rbp, %rsp
	popq %rbp
	ret
	.globl g
g:
# On rentre dasn la fonction g
	pushq %rbp
	mov %rsp, %rbp
	addq $-8, %rsp
	movq $x, %r10
	mov 0(%r10), %r10
	pushq %r10
	jmp g_fin
g_fin:
	popq %r10
	mov %r10, 24(%rbp)
	mov %rbp, %rsp
	popq %rbp
	ret
	.globl main
main:
# On rentre dasn la fonction main
	pushq %rbp
	mov %rsp, %rbp
	addq $-8, %rsp
	mov $label_string_-1, %r10
	pushq %r10
	popq %rdi
	subq $8, %rsp
	movl $1234, %r10d
	andq $127, %r10
	pushq %r10
	call f
	addq $8, %rsp
	andq $127, %r10
	pushq %r10
	popq %rsi
	xorq %rax, %rax
	call printf
	mov %rax, %r10
	andq $127, %r10
	pushq %r10
	popq %r10
	mov $label_string_-1, %r10
	pushq %r10
	popq %rdi
	subq $8, %rsp
	movabsq $1234, %r10
	pushq %r10
	call g
	addq $8, %rsp
	pushq %r10
	popq %rsi
	xorq %rax, %rax
	call printf
	mov %rax, %r10
	andq $127, %r10
	pushq %r10
	popq %r10
	movl $0, %r10d
	pushq %r10
	jmp main_fin
main_fin:
	popq %rax
	mov %rbp, %rsp
	popq %rbp
	ret
.data
label_string_-1:
	.string "\"%ld\\n\""
label_string_-1:
	.string "\"%d\\n\""

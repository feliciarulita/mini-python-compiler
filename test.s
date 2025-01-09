	.text
	.globl	plus
plus:
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	movq %rdi, 0(%rbp)
	movq %rsi, -8(%rbp)
	movq %rdx, -16(%rbp)
	movq %rcx, -24(%rbp)
	movq %r8, -32(%rbp)
	movq %r9, -40(%rbp)
	movq 0(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	moins
moins:
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	movq %rdi, 0(%rbp)
	movq %rsi, -8(%rbp)
	movq %rdx, -16(%rbp)
	movq %rcx, -24(%rbp)
	movq %r8, -32(%rbp)
	movq %r9, -40(%rbp)
	movq 0(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	subq %rax, %rbx
	movq %rbx, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	mult
mult:
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	movq %rdi, 0(%rbp)
	movq %rsi, -8(%rbp)
	movq %rdx, -16(%rbp)
	movq %rcx, -24(%rbp)
	movq %r8, -32(%rbp)
	movq %r9, -40(%rbp)
	movq 0(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	imulq %rbx, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	div
div:
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	movq %rdi, 0(%rbp)
	movq %rsi, -8(%rbp)
	movq %rdx, -16(%rbp)
	movq %rcx, -24(%rbp)
	movq %r8, -32(%rbp)
	movq %r9, -40(%rbp)
	movq 0(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	cmpq $0, %rax
	je runtime_error_division_by_zero
	movq %rax, %rbx
	popq %rax
	xorq %rdx, %rdx
	idivq %rbx
	movq %rbp, %rsp
	popq %rbp
	ret
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	modulo
modulo:
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	movq %rdi, 0(%rbp)
	movq %rsi, -8(%rbp)
	movq %rdx, -16(%rbp)
	movq %rcx, -24(%rbp)
	movq %r8, -32(%rbp)
	movq %r9, -40(%rbp)
	movq 0(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	movq %rax, %rbx
	popq %rax
	xorq %rdx, %rdx
	idivq %rbx
	movq %rdx, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $3, %rax
	movq %rax, -8(%rbp)
	movq $4, %rax
	movq %rax, -16(%rbp)
	movq $5, %rax
	movq %rax, -24(%rbp)
	movq -8(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	movq %rax, %rbx
	popq %rax
	xorq %rdx, %rdx
	idivq %rbx
	movq %rdx, %rax
	pushq %rax
	movq -24(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	pushq %rax
	movq $5, %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	imulq %rbx, %rax
	popq %rbx
	subq %rax, %rbx
	movq %rbx, %rax
	pushq %rax
	movq $1, %rax
	negq %rax
	pushq %rax
	movq $2, %rax
	cmpq $0, %rax
	je runtime_error_division_by_zero
	movq %rax, %rbx
	popq %rax
	xorq %rdx, %rdx
	idivq %rbx
	popq %rbx
	addq %rbx, %rax
	movq %rax, %rsi
	leaq fmt, %rdi
	movq $0, %rax
	call printf
	popq %rbp
	movq $0, %rdi
	call exit
runtime_error_division_by_zero:
	movq $1, %rdi
	call exit
my_malloc:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	call malloc
	movq %rbp, %rsp
	popq %rbp
	ret
my_strlen:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	call strlen
	addq $16, %rsp
	popq %rbp
	ret
my_strcpy:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	call strcpy
	addq $16, %rsp
	popq %rbp
	ret
my_strcat:
	pushq %rbp
	movq %rsp, %rbp
	xorq %rax, %rax
	movq %rsi, %rdx
concat_length_loop:
	testq %rdx, %rdx
	je concat_length_done
	movq %rdx, %rdi
	call strlen
	addq %rax, %rcx
	addq $1, %rcx
	addq $8, %rdx
	jmp concat_length_loop
concat_length_done:
	movq %rcx, %rdi
	call my_malloc
	movq %rax, %rsi
	movq %rsi, %rdx
	xorq %rcx, %rcx
	movq %rdx, %rdi
concat_copy_loop:
	testq %rdi, %rdi
	je concat_copy_done
	movq %rdi, %rsi
	cmpq $0, %rcx
	je first_copy
	call strcat
	addq $8, %rdi
	jmp concat_copy_loop
first_copy:
	call strcpy
	addq $8, %rdi
	incq %rcx
	jmp concat_copy_loop
concat_copy_done:
	movq %rdx, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
fmt:
	.string "%d\n"
true_msg:
	.string "True\n"
false_msg:
	.string "False\n"

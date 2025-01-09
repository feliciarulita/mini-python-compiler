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
	subq $8, %rsp
	movq %rdi, 0(%rbp)
	movq %rsi, -8(%rbp)
	movq 0(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	movq %rax, %rbx
	popq %rax
	xorq %rdx, %rdx
	idivq %rbx
	movq %rdx, %rax
	leave
	ret
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	movq $3, %rax
	movq %rax, -8(%rbp)
	movq $4, %rax
	movq %rax, -16(%rbp)
	movq $5, %rax
	movq %rax, -24(%rbp)

	movq -8(%rbp), %rax
	movq %rax, %rdi
	movq -16(%rbp), %rax
	movq %rax, %rsi
	call modulo
	movq -8(%rbp), %rsi
	leaq fmt, %rdi
	movq $0, %rax
	call printf
	

	movq %rax, %rdi
	movq -24(%rbp), %rax
	movq -24(%rbp), %rsi
	leaq fmt, %rdi
	movq $0, %rax
	call printf
	movq %rax, %rsi
	call plus


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
	.data
fmt:
	.string "%d\n"
true_msg:
	.string "True\n"
false_msg:
	.string "False\n"

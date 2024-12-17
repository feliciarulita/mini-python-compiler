	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $1, %rax
	pushq %rax
	movq $3, %rax
	popq %rbx
	addq %rbx, %rax
	movq %rax, %rsi
	leaq fmt, %rdi
	movq $0, %rax
	call printf
	popq %rbp
	movq $0, %rdi
	call exit
	.data
fmt:
	.string "%d\n"

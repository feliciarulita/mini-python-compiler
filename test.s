	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $0, %rax
	cmpq $0, %rax
	movq $0, %rax
	sete %al
	movq %rax, %rsi
	leaq fmt, %rdi
	movq $0, %rax
	call printf
	movq $1, %rax
	negq %rax
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

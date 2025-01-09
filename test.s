	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $3, %rax
	pushq %rax
	movq $3, %rax
	popq %rbx
	imulq %rbx, %rax
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

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
	movq $0, %rax
	cmpq $1, %rax
	je true_branch
	movq $0, %rax
	jmp end_label
true_branch:
	movq $1, %rax
end_label:
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

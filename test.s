	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	xorq %rax, %rax
	je false_branch
true_branch:
	leaq str_9e925e9341b490bfd3b4c4ca3b0c1ef2, %rax
	movq %rax, %rdi
	call puts
	jmp end_label
false_branch:
	leaq str_8b1d5ce1fc016d30d608c078819246bc, %rax
	movq %rax, %rdi
	call puts
end_label:
	popq %rbp
	movq $0, %rdi
	call exit
runtime_error_division_by_zero:
	movq $1, %rdi
	call exit
	.data
str_8b1d5ce1fc016d30d608c078819246bc:
	.string "answer this"
str_9e925e9341b490bfd3b4c4ca3b0c1ef2:
	.string "this"
fmt:
	.string "%d\n"
true_msg:
	.string "True\n"
false_msg:
	.string "False\n"

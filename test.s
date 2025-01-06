	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	leaq str_ef18d4ac47913fb4b552ad2472918073, %rax
	movq %rax, 0(%rbp)
	movq 0(%rbp), %rax
	movq %rax, %rsi
	leaq fmt, %rdi
	movq $0, %rax
	call printf
	popq %rbp
	movq $0, %rdi
	call exit
	.data
str_ef18d4ac47913fb4b552ad2472918073:
	.string "2 +3"
fmt:
	.string "%s\n"

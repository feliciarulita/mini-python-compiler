	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	leaq str_5d41402abc4b2a76b9719d911017c592, %rdi
	leaq str_bb5355cf57318e0e67f684433f19017d, %rsi
	
	call my_strcat

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

    subq $16, %rbp
    movq %rdi, 0(%rbp) 
    movq %rsi, -8(%rbp) 

    movq 0(%rbp), %rdi 
    call my_strlen  
    movq %rax, %rcx 

    

    
    movq -8(%rbp), %rdi
    call my_strlen
    addq %rax, %rcx
    addq $1, %rcx

    

    movq %rcx, %rdi
    call my_malloc
    movq %rax, %rdx

    movq %rcx, %rsi
	leaq fmt, %rdi
	movq $0, %rax
	call printf

    movq 0(%rbp), %rsi
    movq %rdx, %rdi
    call strcpy

    
    movq %rdx, %rdi  
    movq 8(%rbp), %rsi
    call strcat  
    
    movq %rax, %rsi
	leaq fmt, %rdi
	movq $0, %rax
	call printf

    
    addq $16, %rbp
    movq %rdx, %rax
    popq %rbp
    ret

	.data
str_bb5355cf57318e0e67f684433f19017d:
	.string "world\n"
str_5d41402abc4b2a76b9719d911017c592:
	.string "hello"
fmt:
	.string "%d\n"
true_msg:
	.string "True\n"
false_msg:
	.string "False\n"

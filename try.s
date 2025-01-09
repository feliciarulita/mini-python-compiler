    .section .data
src1:   .asciz "Hello, "          # First string with space for the second string.
src2:   .asciz "World!"           # Second string to append.
len1:   .long 0                   # Placeholder for the length of the result.

    .section .text
    .globl _start

_start:
    # Load addresses of strings
    lea src1(%rip), %rdi          # Load address of src1 into RDI
    lea src2(%rip), %rsi          # Load address of src2 into RSI

    # Find the end of src1
find_end:
    movb (%rdi), %al              # Load current byte of src1
    testb %al, %al                # Check if it's null terminator
    je append_start               # Jump to appending if null
    inc %rdi                      # Move to the next character
    jmp find_end                  # Repeat until null terminator

append_start:
    movb (%rsi), %al              # Load current byte of src2
    testb %al, %al                # Check if it's null terminator
    je done                       # If null, finish concatenation
    movb %al, (%rdi)              # Copy byte from src2 to end of src1
    inc %rdi                      # Advance src1 pointer
    inc %rsi                      # Advance src2 pointer
    jmp append_start              # Repeat for next byte

done:
    movb $0, (%rdi)               # Add null terminator at the end

    # Calculate length of concatenated string
    lea src1(%rip), %rdi          # Reload address of src1 into RDI
    mov $0, %ecx                  # Initialize length counter (ECX)
calc_length:
    movb (%rdi), %al              # Load current byte of src1
    testb %al, %al                # Check if it's null terminator
    je print_result               # If null, finish length calculation
    inc %rdi                      # Move to the next character
    inc %ecx                      # Increment length counter
    jmp calc_length               # Repeat

print_result:
    # Store length in len1
    mov %ecx, len1(%rip)

    # Print src1
    lea src1(%rip), %rdi          # Address of src1
    mov $1, %rax                  # syscall: write
    mov $1, %rdi                  # file descriptor: stdout
    lea src1(%rip), %rsi          # pointer to string (src1)
    mov len1(%rip), %edx          # length of the string
    syscall                       # make the system call

    # Exit the program
    mov $60, %rax                 # syscall: exit
    xor %rdi, %rdi                # exit code 0
    syscall

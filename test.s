			# This code was produced by the CERI Compiler
	.data
	.align 8
a:	.quad 0
b:	.quad 0
c:	.quad 0
z:	.quad 0
	.text		# The following lines contain the program
	.globl main	# The main function must be visible from outside
main:			# The main function body :
	movq %rsp, %rbp	# Save the position of the stack's top
	push $1
	pop a
TestWhile1:
	push a
	push $0
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	ja Vrai2	# If above
	push $0		# False
	jmp Suite2
Vrai2:
	push $0xFFFFFFFFFFFFFFFF		# True
Suite2:
	pop %rax
	cmpq $0, %rax
	je FinWhile1
	push $0
	pop a
	jmp TestWhile1
FinWhile1:
	movq %rbp, %rsp		# Restore the position of the stack's top
	ret			# Return from main function

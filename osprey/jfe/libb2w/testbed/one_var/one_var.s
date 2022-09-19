	#  /home/xc5/lib/gcc-lib/x86_64-open64-linux/5.0/be::5.0

	#-----------------------------------------------------------
	# Compiling one_var.c (one_var.B)
	#-----------------------------------------------------------

	#-----------------------------------------------------------
	# Options:
	#-----------------------------------------------------------
	#  Target:Wolfdale, ISA:ISA_1, Endian:little, Pointer Size:64
	#  -O0	(Optimization level)
	#  -g0	(Debug level)
	#  -m2	(Report advisories)
	#-----------------------------------------------------------

	.file	1	"/home/xc5/xc5/test/one_var/one_var.c"


	.text
	.align	2
	.section .text
	.p2align 5,,

	# Program Unit: main
.globl	main
	.type	main, @function
main:	# 0x0
	# .frame	%rbp, 16, %rbp
	# myvar = -12
	# _temp_reserved_spill0 = -8
	.loc	1	1	0
 #   1  int main(){
.LBB1_main:
	addq    $-8,%rsp
	fnstcw  (%rsp)
	andw    $0xfcff,(%rsp)
	orw     $768,(%rsp)
	fldcw   (%rsp)
	addq    $8,%rsp
	pushq %rbp                    	# [0] 
	movq %rsp,%rbp                	# [0] 
	addq $-16,%rsp                	# [0] 
	.loc	1	3	0
 #   2    int myvar;
 #   3    myvar = 2;
	movl $2,%edi                  	# [0] 
	movl %edi,-12(%rbp)           	# [0] myvar
	.loc	1	4	0
 #   4    return myvar;
	movl -12(%rbp),%eax           	# [0] myvar
	leave                         	# [0] 
	ret                           	# [0] 
.LDWend_main:
	.size main, .LDWend_main-main
	.section .text
	.align	4

	.section .eh_frame, "a",@progbits
.LEHCIE:
	.4byte	.LEHCIE_end - .LEHCIE_begin
.LEHCIE_begin:
	.4byte 0x0
	.byte	0x01, 0x00, 0x01, 0x78, 0x10, 0x0c, 0x07, 0x08
	.byte	0x90, 0x01
	.align 8
.LEHCIE_end:

	.section .debug_line, ""
	.section	.note.GNU-stack,"",@progbits
	.ident	"#Open64 Compiler Version 5.0 : one_var.c compiled with : -O0 -march=wolfdale -msse2 -msse3 -mno-3dnow -mno-sse4a -mssse3 -mno-sse41 -mno-sse42 -mno-aes -mno-pclmul -mno-avx -mno-xop -mno-fma -mno-fma4 -m64"


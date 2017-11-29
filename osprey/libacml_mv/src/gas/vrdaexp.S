
#
#  (C) 2008-2009 Advanced Micro Devices, Inc. All Rights Reserved.
#
#  This file is part of libacml_mv.
#
#  libacml_mv is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 2.1 of the License, or (at your option) any later version.
#
#  libacml_mv is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with libacml_mv.  If not, see
#  <http://www.gnu.org/licenses/>.
#
#





#
# vrdaexp.asm
#
# An array implementation of the exp libm function.
#
# Prototype:
#
#    void vrda_exp(int n, double *x, double *y);
#
#   Computes e raised to the x power for an array of input values.
#   Places the results into the supplied y array.
# Does not perform error checking.   Denormal results are truncated to 0.
#
#

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

# define local variable storage offsets
.equ	p_temp,0		# temporary for get/put bits operation
.equ	p_temp1,0x10		# temporary for exponent multiply

.equ	save_xa,0x020		#qword
.equ	save_ya,0x028		#qword
.equ	save_nv,0x030		#qword

.equ	p_iter,0x038		# qword	storage for number of loop iterations

.equ	p2_temp,0x40		# second temporary for get/put bits operation
				# large enough for two vectors
.equ	p2_temp1,0x60		# second temporary for exponent multiply
				# large enough for two vectors
.equ    save_rbx,0x080          #qword

.equ	stack_size,0x088

        .weak vrda_exp_
        .set vrda_exp_,__vrda_exp__
        .weak vrda_exp__
        .set vrda_exp__,__vrda_exp__

    .text
    .align 16
    .p2align 4,,15

#x/* a FORTRAN subroutine implementation of array exp
#**     VRDA_EXP(N,X,Y)
# C equivalent*/
#void vrda_exp__(int * n, double *x, double *y)
#{
#       vrda_exp(*n,x,y);
#}
.globl __vrda_exp__
    .type   __vrda_exp__,@function
__vrda_exp__:
    mov         (%rdi),%edi


        .align 16
        .p2align 4,,15


# parameters are passed in by gcc as:
# edi - int n
# rsi - double *x
# rdx - double *y


.globl vrda_exp
       .type   vrda_exp,@function
vrda_exp:

	sub		$stack_size,%rsp
        mov             %rbx,save_rbx(%rsp)

# save the arguments
	mov		%rsi,save_xa(%rsp)	# save x_array pointer
	mov		%rdx,save_ya(%rsp)	# save y_array pointer
#ifdef INTEGER64
        mov             %rdi,%rax
#else
        mov             %edi,%eax
        mov             %rax,%rdi
#endif

	mov		%rdi,save_nv(%rsp)	# save number of values
# see if too few values to call the main loop
	shr		$2,%rax			# get number of iterations
	jz		.L__vda_cleanup		# jump if only single calls
# prepare the iteration counts
	mov		%rax,p_iter(%rsp)	# save number of iterations
	shl		$2,%rax
	sub		%rax,%rdi		# compute number of extra single calls
	mov		%rdi,save_nv(%rsp)	# save number of left over values

# In this second version, process the array 4 values at a time.

.L__vda_top:
# build the input _m128d
	movapd	.L__real_thirtytwo_by_log2(%rip),%xmm3	#
	mov		save_xa(%rsp),%rsi	# get x_array pointer
	movlpd	(%rsi),%xmm0
	movhpd	8(%rsi),%xmm0
	prefetch	64(%rsi)
	add		$32,%rsi
	mov		%rsi,save_xa(%rsp)	# save x_array pointer

# compute the exponents

#      Step 1. Reduce the argument.
#        /* Find m, z1 and z2 such that exp(x) = 2**m * (z1 + z2) */
#    r = x * thirtytwo_by_logbaseof2;
		movapd	%xmm3,%xmm7
	movapd	 %xmm0,p_temp(%rsp)
	maxpd	.L__real_C0F0000000000000(%rip),%xmm0	# protect against very large negative, non-infinite numbers
	mulpd	%xmm0,%xmm3

		movlpd	-16(%rsi),%xmm6
		movhpd	-8(%rsi),%xmm6
		movapd	 %xmm6,p2_temp(%rsp)
		maxpd	.L__real_C0F0000000000000(%rip),%xmm6
		mulpd	%xmm6,%xmm7

# save x for later.
        minpd   .L__real_40F0000000000000(%rip),%xmm3    # protect against very large, non-infinite numbers

#    /* Set n = nearest integer to r */
	cvtpd2dq	%xmm3,%xmm4
	lea		.L__two_to_jby32_lead_table(%rip),%rdi
	lea		.L__two_to_jby32_trail_table(%rip),%rsi
	cvtdq2pd	%xmm4,%xmm1
        	minpd   .L__real_40F0000000000000(%rip),%xmm7    # protect against very large, non-infinite numbers

 #    r1 = x - n * logbaseof2_by_32_lead;
	movapd	.L__real_log2_by_32_lead(%rip),%xmm2	#
	mulpd	%xmm1,%xmm2				#
	movq	 %xmm4,p_temp1(%rsp)
	subpd	%xmm2,%xmm0	 			# r1 in xmm0,

		cvtpd2dq	%xmm7,%xmm2
		cvtdq2pd	%xmm2,%xmm8

#    r2 =   - n * logbaseof2_by_32_trail;
	mulpd	.L__real_log2_by_32_tail(%rip),%xmm1	# r2 in xmm1
#    j = n & 0x0000001f;
	mov		$0x01f,%r9
	mov		%r9,%r8
	mov		p_temp1(%rsp),%ecx
	and		%ecx,%r9d
		movq	 %xmm2,p2_temp1(%rsp)
		movapd	.L__real_log2_by_32_lead(%rip),%xmm9
		mulpd	%xmm8,%xmm9
		subpd	%xmm9,%xmm6					 			# r1b in xmm6
		mulpd	.L__real_log2_by_32_tail(%rip),%xmm8	# r2b in xmm8

	mov		p_temp1+4(%rsp),%edx
	and		%edx,%r8d
#    f1 = two_to_jby32_lead_table[j];
#    f2 = two_to_jby32_trail_table[j];

#    *m = (n - j) / 32;
	sub		%r9d,%ecx
	sar		$5,%ecx					#m
	sub		%r8d,%edx
	sar		$5,%edx


	movapd	%xmm0,%xmm2
	addpd	%xmm1,%xmm2   # r = r1 + r2

		mov		$0x01f,%r11
		mov		%r11,%r10
		mov		p2_temp1(%rsp),%ebx
		and		%ebx,%r11d
#      Step 2. Compute the polynomial.
#    q = r1 + (r2 +
#              r*r*( 5.00000000000000008883e-01 +
#                      r*( 1.66666666665260878863e-01 +
#                      r*( 4.16666666662260795726e-02 +
#                      r*( 8.33336798434219616221e-03 +
#                      r*( 1.38889490863777199667e-03 ))))));
#    q = r + r^2/2 + r^3/6 + r^4/24 + r^5/120 + r^6/720
	movapd	%xmm2,%xmm1
	movapd	.L__real_3f56c1728d739765(%rip),%xmm3	# 	1/720
	movapd	.L__real_3FC5555555548F7C(%rip),%xmm0	# 	1/6
# deal with infinite results
	mov		$1024,%rax
	movsx	%ecx,%rcx
	cmp		%rax,%rcx

	mulpd	%xmm2,%xmm3				# *x
	mulpd	%xmm2,%xmm0				# *x
	mulpd	%xmm2,%xmm1				# x*x
	movapd	%xmm1,%xmm4

	cmovg	%rax,%rcx				## if infinite, then set rcx to multiply
							# by infinity
	movsx	%edx,%rdx
	cmp		%rax,%rdx

		movapd	%xmm6,%xmm9
		addpd	%xmm8,%xmm9  #  rb = r1b + r2b
	addpd	.L__real_3F811115B7AA905E(%rip),%xmm3	# 	+ 1/120
	addpd	.L__real_3fe0000000000000(%rip),%xmm0	# 	+ .5
	mulpd	%xmm1,%xmm4				# x^4
	mulpd	%xmm2,%xmm3				# *x

	cmovg	%rax,%rdx				## if infinite, then set rcx to multiply
							# by infinity
# deal with denormal results
	xor		%rax,%rax
	add		$1023,%rcx			# add bias

	mulpd	%xmm1,%xmm0				# *x^2
	addpd	.L__real_3FA5555555545D4E(%rip),%xmm3	# 	+ 1/24
	addpd	%xmm2,%xmm0				# 	+ x
	mulpd	%xmm4,%xmm3				# *x^4

# check for infinity or nan
	movapd	p_temp(%rsp),%xmm2

	cmovs	%rax,%rcx			## if denormal, then multiply by 0
	shl		$52,%rcx		# build 2^n

		sub		%r11d,%ebx
		movapd	%xmm9,%xmm1
	addpd	%xmm3,%xmm0			# q = final sum
		movapd	.L__real_3f56c1728d739765(%rip),%xmm7	# 	1/720
		movapd	.L__real_3FC5555555548F7C(%rip),%xmm3	# 	1/6

#    *z2 = f2 + ((f1 + f2) * q);
	movlpd	(%rsi,%r9,8),%xmm5		# f2
	movlpd	(%rsi,%r8,8),%xmm4		# f2
	addsd	(%rdi,%r8,8),%xmm4		# f1 + f2
	addsd	(%rdi,%r9,8),%xmm5		# f1 + f2
		mov p2_temp1+4(%rsp),%r8d
		and	%r8d,%r10d
		sar	$5,%ebx			#m
		mulpd	%xmm9,%xmm7		# *x
		mulpd	%xmm9,%xmm3		# *x
		mulpd	%xmm9,%xmm1		# x*x
		sub		%r10d,%r8d
		sar		$5,%r8d
# check for infinity or nan
	andpd	.L__real_infinity(%rip),%xmm2
	cmppd	$0,.L__real_infinity(%rip),%xmm2
	add		$1023,%rdx		# add bias
	shufpd	$0,%xmm4,%xmm5
		movapd	%xmm1,%xmm4

	cmovs	%rax,%rdx			## if denormal, then multiply by 0
	shl		$52,%rdx		# build 2^n

	mulpd	%xmm5,%xmm0
	mov		 %rcx,p_temp1(%rsp) # get 2^n to memory
	mov		 %rdx,p_temp1+8(%rsp) # get 2^n to memory
	addpd	%xmm5,%xmm0			#z = z1 + z2   done with 1,2,3,4,5
		mov		$1024,%rax
		movsx	%ebx,%rbx
		cmp		%rax,%rbx
# end of splitexp
#        /* Scale (z1 + z2) by 2.0**m */
#          r = scaleDouble_1(z, n);


		cmovg	%rax,%rbx		## if infinite, then set rcx to multiply
						# by infinity
		movsx	%r8d,%rdx
		cmp		%rax,%rdx

	movmskpd	%xmm2,%r8d

		addpd	.L__real_3F811115B7AA905E(%rip),%xmm7	# 	+ 1/120
		addpd	.L__real_3fe0000000000000(%rip),%xmm3	# 	+ .5
		mulpd	%xmm1,%xmm4			# x^4
		mulpd	%xmm9,%xmm7			# *x
		cmovg	%rax,%rdx			## if infinite, then set rcx to multiply


		xor		%rax,%rax
		add		$1023,%rbx		# add bias

		mulpd	%xmm1,%xmm3			# *x^2
		addpd	.L__real_3FA5555555545D4E(%rip),%xmm7	# 	+ 1/24
		addpd	%xmm9,%xmm3			# 	+ x
		mulpd	%xmm4,%xmm7			# *x^4

		cmovs	%rax,%rbx			## if denormal, then multiply by 0
		shl		$52,%rbx		# build 2^n

#      Step 3. Reconstitute.

	mulpd	p_temp1(%rsp),%xmm0	# result *= 2^n
		addpd	%xmm7,%xmm3			# q = final sum

		movlpd	(%rsi,%r11,8),%xmm5 		# f2
		movlpd	(%rsi,%r10,8),%xmm4 		# f2
		addsd	(%rdi,%r10,8),%xmm4		# f1 + f2
		addsd	(%rdi,%r11,8),%xmm5		# f1 + f2

		add		$1023,%rdx		# add bias
		cmovs	%rax,%rdx			## if denormal, then multiply by 0
		shufpd	$0,%xmm4,%xmm5
		shl		$52,%rdx		# build 2^n

		mulpd	%xmm5,%xmm3
		mov		 %rbx,p2_temp1(%rsp) # get 2^n to memory
		mov		 %rdx,p2_temp1+8(%rsp) # get 2^n to memory
		addpd	%xmm5,%xmm3						#z = z1 + z2

		movapd	p2_temp(%rsp),%xmm2
		andpd	.L__real_infinity(%rip),%xmm2
		cmppd	$0,.L__real_infinity(%rip),%xmm2
		movmskpd	%xmm2,%ebx
	test		$3,%r8d
		mulpd	p2_temp1(%rsp),%xmm3	# result *= 2^n
# we'd like to avoid a branch, and can use cmp's and and's to
# eliminate them.  But it adds cycles for normal cases which
# are supposed to be exceptions.  Using this branch with the
# check above results in faster code for the normal cases.
	jnz			.L__exp_naninf

.L__vda_bottom1:
# store the result _m128d
	mov		save_ya(%rsp),%rdi	# get y_array pointer
	movlpd	%xmm0,(%rdi)
	movhpd	 %xmm0,8(%rdi)
		test		$3,%ebx
		jnz			.L__exp_naninf2

.L__vda_bottom2:

	prefetch	64(%rdi)
	add		$32,%rdi
	mov		%rdi,save_ya(%rsp)	# save y_array pointer

# store the result _m128d
		movlpd	%xmm3,-16(%rdi)
		movhpd	%xmm3,-8(%rdi)

	mov		p_iter(%rsp),%rax	# get number of iterations
	sub		$1,%rax
	mov		%rax,p_iter(%rsp)	# save number of iterations
	jnz		.L__vda_top


# see if we need to do any extras
	mov		save_nv(%rsp),%rax	# get number of values
	test	%rax,%rax
	jnz		.L__vda_cleanup


#
#
.L__final_check:
        mov             save_rbx(%rsp),%rbx             # restore rbx
	add		$stack_size,%rsp
	ret

# at least one of the numbers needs special treatment
.L__exp_naninf:
	lea		p_temp(%rsp),%rcx
	call  .L__naninf
	jmp		.L__vda_bottom1
.L__exp_naninf2:
	lea		p2_temp(%rsp),%rcx
	mov			%ebx,%r8d
	movapd	%xmm3,%xmm0
	call  .L__naninf
	movapd	%xmm0,%xmm3
	jmp		.L__vda_bottom2

# This subroutine checks a double pair for nans and infinities and
# produces the proper result from the exceptional inputs
# Register assumptions:
# Inputs:
# r8d - mask of errors
# xmm0 - computed result vector
# rcx - pointing to memory image of inputs
# Outputs:
# xmm0 - new result vector
# %rax,rdx,,%xmm2 all modified.
.L__naninf:
# check the first number
	test	$1,%r8d
	jz		.L__check2

	mov		(%rcx),%rdx
	mov		$0x0000FFFFFFFFFFFFF,%rax
	test	%rax,%rdx
	jnz		.L__enan1			# jump if mantissa not zero, so it's a NaN
# inf
	mov		%rdx,%rax
	rcl		$1,%rax
	jnc		.L__r1			# exp(+inf) = inf
	xor		%rdx,%rdx			# exp(-inf) = 0
	jmp		.L__r1

#NaN
.L__enan1:
	mov		$0x00008000000000000,%rax	# convert to quiet
	or		%rax,%rdx
.L__r1:
	movd	%rdx,%xmm2
	shufpd	$2,%xmm0,%xmm2
	movsd	%xmm2,%xmm0
# check the second number
.L__check2:
	test	$2,%r8d
	jz		.L__r3
	mov		8(%rcx),%rdx
	mov		$0x0000FFFFFFFFFFFFF,%rax
	test	%rax,%rdx
	jnz		.L__enan2			# jump if mantissa not zero, so it's a NaN
# inf
	mov		%rdx,%rax
	rcl		$1,%rax
	jnc		.L__r2			# exp(+inf) = inf
	xor		%rdx,%rdx			# exp(-inf) = 0
	jmp		.L__r2

#NaN
.L__enan2:
	mov		$0x00008000000000000,%rax	# convert to quiet
	or		%rax,%rdx
.L__r2:
	movd	%rdx,%xmm2
	shufpd	$0,%xmm2,%xmm0
.L__r3:
	ret

	.align	16
# we jump here when we have an odd number of exp calls to make at the
# end
#  we assume that rdx is pointing at the next x array element,
#  r8 at the next y array element.  The number of values left is in
#  save_nv
.L__vda_cleanup:
        mov             save_nv(%rsp),%rax      # get number of values
        test            %rax,%rax               # are there any values
        jz              .L__final_check         # exit if not

	mov		save_xa(%rsp),%rsi
	mov		save_ya(%rsp),%rdi
# fill in a m128d with zeroes and the extra values and then make a recursive call.
	xorpd		%xmm0,%xmm0
	movlpd	 	%xmm0,p2_temp+8(%rsp)
	movapd	 	%xmm0,p2_temp+16(%rsp)

	mov		(%rsi),%rcx		# we know there's at least one
	mov	 	%rcx,p2_temp(%rsp)
	cmp		$2,%rax
	jl		.L_vdacg

	mov		8(%rsi),%rcx		# do the second value
	mov	 	%rcx,p2_temp+8(%rsp)
	cmp		$3,%rax
	jl		.L_vdacg

	mov		16(%rsi),%rcx		# do the third value
	mov	 	%rcx,p2_temp+16(%rsp)

.L_vdacg:
	mov	$4,%rdi			# parameter for N
	lea	p2_temp(%rsp),%rsi	# &x parameter
	lea	p2_temp1(%rsp),%rdx	# &y parameter
	call	vrda_exp@PLT			# call recursively to compute four values

# now copy the results to the destination array
	mov		save_ya(%rsp),%rdi
	mov		save_nv(%rsp),%rax	# get number of values
	mov	 	p2_temp1(%rsp),%rcx
	mov		%rcx,(%rdi)		# we know there's at least one
	cmp		$2,%rax
	jl		.L_vdacgf

	mov	 	p2_temp1+8(%rsp),%rcx
	mov		%rcx,8(%rdi)		# do the second value
	cmp		$3,%rax
	jl		.L_vdacgf

	mov	 	p2_temp1+16(%rsp),%rcx
	mov		%rcx,16(%rdi)		# do the third value

.L_vdacgf:
	jmp		.L__final_check

	.data
        .align 64


.L__real_3ff0000000000000:	.quad 0x03ff0000000000000	# 1.0
				.quad 0x03ff0000000000000	# for alignment
.L__real_4040000000000000:	.quad 0x04040000000000000	# 32
				.quad 0x04040000000000000
.L__real_40F0000000000000:      .quad 0x040F0000000000000        # 65536, to protect against really large numbers
                                .quad 0x040F0000000000000
.L__real_C0F0000000000000:	.quad 0x0C0F0000000000000	# -65536, to protect against really large negative numbers
				.quad 0x0C0F0000000000000
.L__real_3FA0000000000000:	.quad 0x03FA0000000000000	# 1/32
				.quad 0x03FA0000000000000
.L__real_3fe0000000000000:	.quad 0x03fe0000000000000	# 1/2
				.quad 0x03fe0000000000000
.L__real_infinity:		.quad 0x07ff0000000000000	#
				.quad 0x07ff0000000000000	# for alignment
.L__real_ninfinity:		.quad 0x0fff0000000000000	#
				.quad 0x0fff0000000000000	# for alignment
.L__real_thirtytwo_by_log2: 	.quad 0x040471547652b82fe	# thirtytwo_by_log2
				.quad 0x040471547652b82fe
.L__real_log2_by_32_lead:  	.quad 0x03f962e42fe000000	# log2_by_32_lead
				.quad 0x03f962e42fe000000
.L__real_log2_by_32_tail:  	.quad 0x0Bdcf473de6af278e	# -log2_by_32_tail
				.quad 0x0Bdcf473de6af278e
.L__real_3f56c1728d739765:	.quad 0x03f56c1728d739765	# 1.38889490863777199667e-03
				.quad 0x03f56c1728d739765
.L__real_3F811115B7AA905E:	.quad 0x03F811115B7AA905E	# 8.33336798434219616221e-03
				.quad 0x03F811115B7AA905E
.L__real_3FA5555555545D4E:	.quad 0x03FA5555555545D4E	# 4.16666666662260795726e-02
				.quad 0x03FA5555555545D4E
.L__real_3FC5555555548F7C:	.quad 0x03FC5555555548F7C	# 1.66666666665260878863e-01
				.quad 0x03FC5555555548F7C


.L__two_to_jby32_lead_table:
	.quad	0x03ff0000000000000 # 1
	.quad	0x03ff059b0d0000000		# 1.0219
	.quad	0x03ff0b55860000000		# 1.04427
	.quad	0x03ff11301d0000000		# 1.06714
	.quad	0x03ff172b830000000		# 1.09051
	.quad	0x03ff1d48730000000		# 1.11439
	.quad	0x03ff2387a60000000		# 1.13879
	.quad	0x03ff29e9df0000000		# 1.16372
	.quad	0x03ff306fe00000000		# 1.18921
	.quad	0x03ff371a730000000		# 1.21525
	.quad	0x03ff3dea640000000		# 1.24186
	.quad	0x03ff44e0860000000		# 1.26905
	.quad	0x03ff4bfdad0000000		# 1.29684
	.quad	0x03ff5342b50000000		# 1.32524
	.quad	0x03ff5ab07d0000000		# 1.35426
	.quad	0x03ff6247eb0000000		# 1.38391
	.quad	0x03ff6a09e60000000		# 1.41421
	.quad	0x03ff71f75e0000000		# 1.44518
	.quad	0x03ff7a11470000000		# 1.47683
	.quad	0x03ff8258990000000		# 1.50916
	.quad	0x03ff8ace540000000		# 1.54221
	.quad	0x03ff93737b0000000		# 1.57598
	.quad	0x03ff9c49180000000		# 1.61049
	.quad	0x03ffa5503b0000000		# 1.64576
	.quad	0x03ffae89f90000000		# 1.68179
	.quad	0x03ffb7f76f0000000		# 1.71862
	.quad	0x03ffc199bd0000000		# 1.75625
	.quad	0x03ffcb720d0000000		# 1.79471
	.quad	0x03ffd5818d0000000		# 1.83401
	.quad	0x03ffdfc9730000000		# 1.87417
	.quad	0x03ffea4afa0000000		# 1.91521
	.quad	0x03fff507650000000		# 1.95714
	.quad 0					# for alignment
.L__two_to_jby32_trail_table:
	.quad	0x00000000000000000 # 0
	.quad	0x03e48ac2ba1d73e2a		# 1.1489e-008
	.quad	0x03e69f3121ec53172		# 4.83347e-008
	.quad	0x03df25b50a4ebbf1b		# 2.67125e-010
	.quad	0x03e68faa2f5b9bef9		# 4.65271e-008
	.quad	0x03e368b9aa7805b80		# 5.24924e-009
	.quad	0x03e6ceac470cd83f6		# 5.38622e-008
	.quad	0x03e547f7b84b09745		# 1.90902e-008
	.quad	0x03e64636e2a5bd1ab		# 3.79764e-008
	.quad	0x03e5ceaa72a9c5154		# 2.69307e-008
	.quad	0x03e682468446b6824		# 4.49684e-008
	.quad	0x03e18624b40c4dbd0		# 1.41933e-009
	.quad	0x03e54d8a89c750e5e		# 1.94147e-008
	.quad	0x03e5a753e077c2a0f		# 2.46409e-008
	.quad	0x03e6a90a852b19260		# 4.94813e-008
	.quad	0x03e0d2ac258f87d03		# 8.48872e-010
	.quad	0x03e59fcef32422cbf		# 2.42032e-008
	.quad	0x03e61d8bee7ba46e2		# 3.3242e-008
	.quad	0x03e4f580c36bea881		# 1.45957e-008
	.quad	0x03e62999c25159f11		# 3.46453e-008
	.quad	0x03e415506dadd3e2a		# 8.0709e-009
	.quad	0x03e29b8bc9e8a0388		# 2.99439e-009
	.quad	0x03e451f8480e3e236		# 9.83622e-009
	.quad	0x03e41f12ae45a1224		# 8.35492e-009
	.quad	0x03e62b5a75abd0e6a		# 3.48493e-008
	.quad	0x03e47daf237553d84		# 1.11085e-008
	.quad	0x03e6b0aa538444196		# 5.03689e-008
	.quad	0x03e69df20d22a0798		# 4.81896e-008
	.quad	0x03e69f7490e4bb40b		# 4.83654e-008
	.quad	0x03e4bdcdaf5cb4656		# 1.29746e-008
	.quad	0x03e452486cc2c7b9d		# 9.84533e-009
	.quad	0x03e66dc8a80ce9f09		# 4.25828e-008
	.quad 0					# for alignment


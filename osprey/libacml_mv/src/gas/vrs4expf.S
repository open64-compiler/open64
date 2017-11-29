
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
# __vrs4_expf.s
#
# A vector implementation of the expf libm function.
#  This routine implemented in single precision.  It is slightly
#  less accurate than the double precision version, but it will
#  be better for vectorizing.
#
# Prototype:
#
#     __m128 __vrs4_expf(__m128 x);
#
#   Computes e raised to the x power for 4 floats at a time.
# Does not perform error handling, but does return C99 values for error
# inputs.   Denormal results are truncated to 0.
#
#

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

        .text
        .align 16
        .p2align 4,,15

# define local variable storage offsets
.equ	p_temp,0	# temporary for get/put bits operation
.equ	p_ux,0x10		# local storage for ux array
.equ	p_m,0x20		# local storage for m array
.equ	p_j,0x30		# local storage for m array
.equ    save_rbx,0x040          #qword
.equ	stack_size,0x48



.globl __vrs4_expf
       .type   __vrs4_expf,@function
__vrs4_expf:
	sub	$stack_size,%rsp
        mov             %rbx,save_rbx(%rsp)


	movaps	 %xmm0,p_ux(%rsp)
        maxps   .L__real_m8192(%rip),%xmm0   # protect against small input values


#        /* Find m, z1 and z2 such that exp(x) = 2**m * (z1 + z2) */
#      Step 1. Reduce the argument.
	#    r = x * thirtytwo_by_logbaseof2;
	movaps	.L__real_thirtytwo_by_log2(%rip),%xmm2	#
	mulps	%xmm0,%xmm2
	xor		%rax,%rax
        minps   .L__real_8192(%rip),%xmm2   # protect against large input values

#    /* Set n = nearest integer to r */
	cvtps2dq	%xmm2,%xmm3
	lea		.L__two_to_jby32_table(%rip),%rdi
	cvtdq2ps	%xmm3,%xmm1


#    r1 = x - n * logbaseof2_by_32_lead;
	movaps	.L__real_log2_by_32_head(%rip),%xmm2
	mulps	%xmm1,%xmm2
	subps	%xmm2,%xmm0				# r1 in xmm0,

#    r2 = - n * logbaseof2_by_32_lead;
	mulps	.L__real_log2_by_32_tail(%rip),%xmm1

#    j = n & 0x0000001f;
	movdqa	%xmm3,%xmm4
	movdqa	.L__int_mask_1f(%rip),%xmm2
	pand	%xmm4,%xmm2
	movdqa	%xmm2,p_j(%rsp)
#    f1 = two_to_jby32_lead_table[j];

#    *m = (n - j) / 32;
	psubd	%xmm2,%xmm4
	psrad	$5,%xmm4
	movdqa	%xmm4,p_m(%rsp)

	movaps	%xmm0,%xmm3
	addps	%xmm1,%xmm3

	mov		p_j(%rsp),%eax 			# get an individual index
	mov		(%rdi,%rax,4),%edx		# get the f1 value
	mov		%edx,p_j(%rsp) 			# save the f1 value

#      Step 2. Compute the polynomial.
#     q = r1 +
#              r*r*( 5.00000000000000008883e-01 +
#                      r*( 1.66666666665260878863e-01 +
#                      r*( 4.16666666662260795726e-02 +
#                      r*( 8.33336798434219616221e-03 +
#                      r*( 1.38889490863777199667e-03 )))));
#    q = r + r^2/2 + r^3/6 + r^4/24 + r^5/120 + r^6/720
#    q = r + r^2/2 + r^3/6 + r^4/24 good enough for single precision
	movaps	%xmm3,%xmm4
	movaps	%xmm3,%xmm2			# x*x
	mulps	%xmm2,%xmm2
	mulps	.L__real_1_24(%rip),%xmm4	# /24

	mov		p_j+4(%rsp),%eax 			# get an individual index
	mov		(%rdi,%rax,4),%edx		# get the f1 value
	mov		%edx,p_j+4(%rsp) 			# save the f1 value

	addps 	.L__real_1_6(%rip),%xmm4	# +1/6

	mulps	%xmm2,%xmm3			# x^3
	mov		p_j+8(%rsp),%eax 			# get an individual index
	mov		(%rdi,%rax,4),%edx		# get the f1 value
	mov		%edx,p_j+8(%rsp) 			# save the f1 value
	mulps	.L__real_half(%rip),%xmm2	# x^2/2
	mulps	%xmm3,%xmm4			# *x^3

	mov		p_j+12(%rsp),%eax 			# get an individual index
	mov		(%rdi,%rax,4),%edx		# get the f1 value
	mov		%edx,p_j+12(%rsp) 			# save the f1 value
	addps	%xmm4,%xmm1			# +r2

	addps	%xmm2,%xmm1			# + x^2/2
	addps	%xmm1,%xmm0			# +r1

# deal with infinite or denormal results
        movdqa  p_m(%rsp),%xmm1
        movdqa  p_m(%rsp),%xmm2
        pcmpgtd .L__int_127(%rip),%xmm2
        pminsw  .L__int_128(%rip),%xmm1 # ceil at 128
        movmskps        %xmm2,%eax
        test            $0x0f,%eax

        paddd   .L__int_127(%rip),%xmm1 # add bias

#    *z2 = f2 + ((f1 + f2) * q);
        mulps   p_j(%rsp),%xmm0         # * f1
        addps   p_j(%rsp),%xmm0         # + f1
        jnz             .L__exp_largef
.L__check1:

	pxor	%xmm2,%xmm2			# floor at 0
	pmaxsw	%xmm2,%xmm1

	pslld	$23,%xmm1			# build 2^n

	movaps	%xmm1,%xmm2


# check for infinity or nan
	movaps	p_ux(%rsp),%xmm1
	andps	.L__real_infinity(%rip),%xmm1
	cmpps	$0,.L__real_infinity(%rip),%xmm1
	movmskps	%xmm1,%eax
	test		$0xf,%eax


# end of splitexp
#        /* Scale (z1 + z2) by 2.0**m */
#      Step 3. Reconstitute.

	mulps	%xmm2,%xmm0		# result *= 2^n

# we'd like to avoid a branch, and can use cmp's and and's to
# eliminate them.  But it adds cycles for normal cases
# to handle events that are supposed to be exceptions.
#  Using this branch with the
# check above results in faster code for the normal cases.
# And branch mispredict penalties should only come into
# play for nans and infinities.
	jnz		.L__exp_naninf

#
.L__final_check:
        mov             save_rbx(%rsp),%rbx             # restore rbx
	add		$stack_size,%rsp
	ret

#  deal with nans and infinities

.L__exp_naninf:
	movaps	%xmm0,p_temp(%rsp)		# save the computed values
	mov		%eax,%ecx				# save the error mask
	test	$1,%ecx					# first value?
	jz		.__Lni2
	mov		p_ux(%rsp),%edx			# get the input
	call	.L__naninf
	mov		%edx,p_temp(%rsp)		# save the new result
.__Lni2:
	test	$2,%ecx					# first value?
	jz		.__Lni3
	mov		p_ux+4(%rsp),%edx		# get the input
	call	.L__naninf
	mov		%edx,p_temp+4(%rsp)		# save the new result
.__Lni3:
	test	$4,%ecx					# first value?
	jz		.__Lni4
	mov		p_ux+8(%rsp),%edx		# get the input
	call	.L__naninf
	mov		%edx,p_temp+8(%rsp)		# save the new result
.__Lni4:
	test	$8,%ecx					# first value?
	jz		.__Lnie
	mov		p_ux+12(%rsp),%edx		# get the input
	call	.L__naninf
	mov		%edx,p_temp+12(%rsp)		# save the new result
.__Lnie:
	movaps	p_temp(%rsp),%xmm0		# get the answers
	jmp		.L__final_check


# a simple subroutine to check a scalar input value for infinity
# or NaN and return the correct result
# expects input in edx, and returns value in edx.  Destroys eax.
.L__naninf:
	mov	$0x0007FFFFF,%eax
	test	%eax,%edx
	jnz		.L__enan		# jump if mantissa not zero, so it's a NaN
# inf
	mov		%edx,%eax
	rcl		$1,%eax
	jnc		.L__r				# exp(+inf) = inf
	xor		%edx,%edx				# exp(-inf) = 0
	jmp		.L__r

#NaN
.L__enan:
	mov		$0x000400000,%eax	# convert to quiet
	or		%eax,%edx
.L__r:
	ret

        .align 16
#  deal with m > 127.  In some instances, rounding during calculations
#  can result in infinity when it shouldn't.  For these cases, we scale
#  m down, and scale the mantissa up.

.L__exp_largef:
        movdqa    %xmm0,p_temp(%rsp)    # save the mantissa portion
        movdqa    %xmm1,p_m(%rsp)       # save the exponent portion
        mov       %eax,%ecx              # save the error mask
        test    $1,%ecx                  # first value?
        jz       .L__Lf2
        mov      p_m(%rsp),%edx # get the exponent
        sub      $1,%edx                # scale it down
        mov      %edx,p_m(%rsp)       # save the exponent
        movss   p_temp(%rsp),%xmm3     # get the mantissa
        mulss   .L__real_two(%rip),%xmm3        # scale it up
        movss    %xmm3,p_temp(%rsp)   # save the mantissa
.L__Lf2:
        test    $2,%ecx                 # second value?
        jz       .L__Lf3
        mov      p_m+4(%rsp),%edx # get the exponent
        sub      $1,%edx                # scale it down
        mov      %edx,p_m+4(%rsp)       # save the exponent
        movss   p_temp+4(%rsp),%xmm3     # get the mantissa
        mulss   .L__real_two(%rip),%xmm3        # scale it up
        movss     %xmm3,p_temp+4(%rsp)   # save the mantissa
.L__Lf3:
        test    $4,%ecx                 # third value?
        jz       .L__Lf4
        mov      p_m+8(%rsp),%edx # get the exponent
        sub      $1,%edx                # scale it down
        mov      %edx,p_m+8(%rsp)       # save the exponent
        movss   p_temp+8(%rsp),%xmm3     # get the mantissa
        mulss   .L__real_two(%rip),%xmm3        # scale it up
        movss    %xmm3,p_temp+8(%rsp)   # save the mantissa
.L__Lf4:
        test    $8,%ecx                                 # fourth value?
        jz       .L__Lfe
        mov      p_m+12(%rsp),%edx        # get the exponent
        sub      $1,%edx                 # scale it down
        mov      %edx,p_m+12(%rsp)      # save the exponent
        movss   p_temp+12(%rsp),%xmm3    # get the mantissa
        mulss   .L__real_two(%rip),%xmm3        # scale it up
        movss     %xmm3,p_temp+12(%rsp)  # save the mantissa
.L__Lfe:
        movaps  p_temp(%rsp),%xmm0      # restore the mantissa portion back
        movdqa  p_m(%rsp),%xmm1         # restore the exponent portion
        jmp             .L__check1

	.data
        .align 64

.L__real_half:			.long 0x3f000000	# 1/2
				.long 0x3f000000
				.long 0x3f000000
				.long 0x3f000000

.L__real_two:			.long 0x40000000	# 2
				.long 0x40000000
				.long 0x40000000
				.long 0x40000000

.L__real_8192:			.long 0x46000000	# 8192, to protect against really large numbers
				.long 0x46000000
				.long 0x46000000
				.long 0x46000000
.L__real_m8192:			.long 0xC6000000	# -8192, to protect against really small numbers
				.long 0xC6000000
				.long 0xC6000000
				.long 0xC6000000

.L__real_thirtytwo_by_log2: 	.long 0x4238AA3B	# thirtytwo_by_log2
				.long 0x4238AA3B
				.long 0x4238AA3B
				.long 0x4238AA3B

.L__real_log2_by_32:		.long 0x3CB17218	# log2_by_32
				.long 0x3CB17218
				.long 0x3CB17218
				.long 0x3CB17218

.L__real_log2_by_32_head:	.long 0x3CB17000	# log2_by_32
				.long 0x3CB17000
				.long 0x3CB17000
				.long 0x3CB17000

.L__real_log2_by_32_tail:	.long 0xB585FDF4	# log2_by_32
				.long 0xB585FDF4
				.long 0xB585FDF4
				.long 0xB585FDF4

.L__real_1_6:			.long 0x3E2AAAAB	# 0.16666666666 used in polynomial
				.long 0x3E2AAAAB
				.long 0x3E2AAAAB
				.long 0x3E2AAAAB

.L__real_1_24:			.long 0x3D2AAAAB	# 0.041666668 used in polynomial
				.long 0x3D2AAAAB
				.long 0x3D2AAAAB
				.long 0x3D2AAAAB

.L__real_infinity:		.long 0x7f800000	# infinity
				.long 0x7f800000
				.long 0x7f800000
				.long 0x7f800000
.L__int_mask_1f:		.long 0x00000001f
				.long 0x00000001f
				.long 0x00000001f
				.long 0x00000001f
.L__int_128:			.long 0x000000080
				.long 0x000000080
				.long 0x000000080
				.long 0x000000080
.L__int_127:			.long 0x00000007f
				.long 0x00000007f
				.long 0x00000007f
				.long 0x00000007f


.L__two_to_jby32_table:
   .long  0x3F800000  #          1
   .long  0x3F82CD87  #  1.0218972
   .long  0x3F85AAC3  #  1.0442737
   .long  0x3F88980F  #  1.0671405
   .long  0x3F8B95C2  #  1.0905077
   .long  0x3F8EA43A  #  1.1143868
   .long  0x3F91C3D3  #  1.1387886
   .long  0x3F94F4F0  #  1.1637249
   .long  0x3F9837F0  #  1.1892071
   .long  0x3F9B8D3A  #  1.2152474
   .long  0x3F9EF532  #  1.2418578
   .long  0x3FA27043  #   1.269051
   .long  0x3FA5FED7  #  1.2968396
   .long  0x3FA9A15B  #  1.3252367
   .long  0x3FAD583F  #  1.3542556
   .long  0x3FB123F6  #  1.3839099
   .long  0x3FB504F3  #  1.4142135
   .long  0x3FB8FBAF  #  1.4451808
   .long  0x3FBD08A4  #  1.4768262
   .long  0x3FC12C4D  #  1.5091645
   .long  0x3FC5672A  #  1.5422108
   .long  0x3FC9B9BE  #  1.5759809
   .long  0x3FCE248C  #  1.6104903
   .long  0x3FD2A81E  #  1.6457555
   .long  0x3FD744FD  #  1.6817929
   .long  0x3FDBFBB8  #  1.7186193
   .long  0x3FE0CCDF  #  1.7562522
   .long  0x3FE5B907  #  1.7947091
   .long  0x3FEAC0C7  #  1.8340081
   .long  0x3FEFE4BA  #  1.8741677
   .long  0x3FF5257D  #  1.9152066
   .long  0x3FFA83B3  #  1.9571441
   .long 0					# for alignment


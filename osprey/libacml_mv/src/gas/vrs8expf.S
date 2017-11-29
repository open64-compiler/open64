
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
# vrs8expf.s
#
# A vector implementation of the expf libm function.
#
# Prototype:
#
#    void vs_expf(int n, float *x, float *y);
#
#   Computes e raised to the x power for a eight packed single values.
#   Places the results into xmm0 an xmm1.
#  This routine implemented in single precision.  It is slightly
#  less accurate than the double precision version, but it will
#  be better for vectorizing.
# Does not perform error handling, but does return C99 values for error
# inputs.   Denormal results are truncated to 0.

# This array version is basically a unrolling of the by4 scalar single
# routine.  The second set of operations is performed by the indented
# instructions interleaved into the first set.
# The scheduling is done by trial and error.  The resulting code represents
# the best time of many variations.  It would seem more interleaving could
# be done, as there is a long stretch of the second computation that is not
# interleaved.  But moving any of this code forward makes the routine
# slower.
#
#

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif


# define local variable storage offsets
.equ    p_ux,0x00               #qword
.equ    p_ux2,0x010             #qword

.equ    save_xa,0x020           #qword
.equ    save_ya,0x028           #qword
.equ    save_nv,0x030           #qword


.equ    p_iter,0x038            # qword storage for number of loop iterations

.equ    p_j,0x040               # second temporary for get/put bits operation
.equ    p_m,0x050               #qword
.equ    p_j2,0x060              # second temporary for exponent multiply
.equ    p_m2,0x070              #qword
.equ    save_rbx,0x080          #qword


.equ    stack_size,0x098


# parameters passed by gcc as:
# xmm0 - __m128d x1
# xmm1 - __m128d x2

    .text
    .align 16
    .p2align 4,,15
.globl __vrs8_expf
    .type   __vrs8_expf,@function
__vrs8_expf:
        sub             $stack_size,%rsp
        mov             %rbx,save_rbx(%rsp)


# Process the array 8 values at a time.

	movaps	.L__real_thirtytwo_by_log2(%rip),%xmm3	#

	movaps	 %xmm0,p_ux(%rsp)
        maxps   .L__real_m8192(%rip),%xmm0
		movaps	 %xmm1,p_ux2(%rsp)
                maxps   .L__real_m8192(%rip),%xmm1
		movaps	%xmm1,%xmm6
#        /* Find m, z1 and z2 such that exp(x) = 2**m * (z1 + z2) */
#      Step 1. Reduce the argument.
        #    r = x * thirtytwo_by_logbaseof2;
        movaps  .L__real_thirtytwo_by_log2(%rip),%xmm2  #

        mulps   %xmm0,%xmm2
        xor             %rax,%rax
        minps   .L__real_8192(%rip),%xmm2
                movaps  .L__real_thirtytwo_by_log2(%rip),%xmm5  #

                mulps   %xmm6,%xmm5
        	minps   .L__real_8192(%rip),%xmm5   # protect against large input values

#    /* Set n = nearest integer to r */
        cvtps2dq        %xmm2,%xmm3
        lea             .L__two_to_jby32_table(%rip),%rdi
        cvtdq2ps        %xmm3,%xmm1

                cvtps2dq        %xmm5,%xmm8
                cvtdq2ps        %xmm8,%xmm7
#    r1 = x - n * logbaseof2_by_32_lead;
        movaps  .L__real_log2_by_32_head(%rip),%xmm2
        mulps   %xmm1,%xmm2
        subps   %xmm2,%xmm0                             # r1 in xmm0,

                movaps  .L__real_log2_by_32_head(%rip),%xmm5
                mulps   %xmm7,%xmm5
                subps   %xmm5,%xmm6                             # r1 in xmm6,


#    r2 = - n * logbaseof2_by_32_lead;
        mulps   .L__real_log2_by_32_tail(%rip),%xmm1
                mulps   .L__real_log2_by_32_tail(%rip),%xmm7

#    j = n & 0x0000001f;
        movdqa  %xmm3,%xmm4
        movdqa  .L__int_mask_1f(%rip),%xmm2
                movdqa  %xmm8,%xmm9
                movdqa  .L__int_mask_1f(%rip),%xmm5
        pand    %xmm4,%xmm2
        movdqa   %xmm2,p_j(%rsp)
#    f1 = two_to_jby32_lead_table[j);

                pand    %xmm9,%xmm5
                movdqa   %xmm5,p_j2(%rsp)

#    *m = (n - j) / 32;
        psubd   %xmm2,%xmm4
        psrad   $5,%xmm4
        movdqa   %xmm4,p_m(%rsp)
                psubd   %xmm5,%xmm9
                psrad   $5,%xmm9
                movdqa   %xmm9,p_m2(%rsp)

        movaps  %xmm0,%xmm3
        addps   %xmm1,%xmm3                             # r = r1+ r2

        mov             p_j(%rsp),%eax                  # get an individual index

                movaps  %xmm6,%xmm8
        mov             (%rdi,%rax,4),%edx              # get the f1 value
                addps   %xmm7,%xmm8                             # r = r1+ r2
        mov              %edx,p_j(%rsp)                         # save the f1 value


#      Step 2. Compute the polynomial.
#    q = r1 +
#              r*r*( 5.00000000000000008883e-01 +
#                      r*( 1.66666666665260878863e-01 +
#                      r*( 4.16666666662260795726e-02 +
#                      r*( 8.33336798434219616221e-03 +
#                      r*( 1.38889490863777199667e-03 )))));
#    q = r + r^2/2 + r^3/6 + r^4/24 + r^5/120 + r^6/720
#    q = r + r^2/2 + r^3/6 + r^4/24 good enough for single precision
        movaps  %xmm3,%xmm4
        movaps  %xmm3,%xmm2
        mulps   %xmm2,%xmm2                     # x*x
        mulps   .L__real_1_24(%rip),%xmm4       # /24

        mov             p_j+4(%rsp),%eax                        # get an individual index

        mov             (%rdi,%rax,4),%edx              # get the f1 value
        mov              %edx,p_j+4(%rsp)                       # save the f1 value


        addps   .L__real_1_6(%rip),%xmm4                # +1/6

        mulps   %xmm2,%xmm3                     # x^3
        mov             p_j+8(%rsp),%eax                        # get an individual index

        mov             (%rdi,%rax,4),%edx              # get the f1 value
        mov              %edx,p_j+8(%rsp)                       # save the f1 value

        mulps   .L__real_half(%rip),%xmm2       # x^2/2
        mov             p_j+12(%rsp),%eax                       # get an individual index

        mov             (%rdi,%rax,4),%edx              # get the f1 value
        mov              %edx,p_j+12(%rsp)                      # save the f1 value

        mulps   %xmm3,%xmm4                     # *x^3
                mov             p_j2(%rsp),%eax                 # get an individual index
                mov             (%rdi,%rax,4),%edx              # get the f1 value
                mov              %edx,p_j2(%rsp)                        # save the f1 value


        addps   %xmm4,%xmm1                     # +r2

        addps   %xmm2,%xmm1                     # + x^2/2
        addps   %xmm1,%xmm0                     # +r1

                movaps  %xmm8,%xmm9
                mov             p_j2+4(%rsp),%eax                # get an individual index
                movaps  %xmm8,%xmm5
                mulps   %xmm5,%xmm5                     # x*x
                mulps   .L__real_1_24(%rip),%xmm9       # /24

                movaps  %xmm8,%xmm5
                mulps   %xmm5,%xmm5                     # x*x
                mulps   .L__real_1_24(%rip),%xmm9       # /24

                mov             (%rdi,%rax,4),%edx              # get the f1 value
                mov              %edx,p_j2+4(%rsp)                      # save the f1 value


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

        pxor    %xmm2,%xmm2                             # floor at 0
        pmaxsw  %xmm2,%xmm1

        pslld   $23,%xmm1                                       # build 2^n

        movaps  %xmm1,%xmm2



# check for infinity or nan
        movaps  p_ux(%rsp),%xmm1
        andps   .L__real_infinity(%rip),%xmm1
        cmpps   $0,.L__real_infinity(%rip),%xmm1
        movmskps        %xmm1,%ebx
        test            $0x0f,%ebx

# end of splitexp
#        /* Scale (z1 + z2) by 2.0**m */
#      Step 3. Reconstitute.

	mulps	%xmm2,%xmm0						# result *= 2^n

# we'd like to avoid a branch, and can use cmp's and and's to
# eliminate them.  But it adds cycles for normal cases
# to handle events that are supposed to be exceptions.
#  Using this branch with the
# check above results in faster code for the normal cases.
# And branch mispredict penalties should only come into
# play for nans and infinities.
	jnz		.L__exp_naninf
.L__vsa_bottom1:

                #    q = r + r^2/2 + r^3/6 + r^4/24 good enough for single precision
                addps   .L__real_1_6(%rip),%xmm9                # +1/6

                mulps   %xmm5,%xmm8                     # x^3
                mov             p_j2+8(%rsp),%eax               # get an individual index
                mov             (%rdi,%rax,4),%edx              # get the f1 value
                mov              %edx,p_j2+8(%rsp)              # save the f1 value

                mulps   .L__real_half(%rip),%xmm5       # x^2/2
                mulps   %xmm8,%xmm9                     # *x^3

                mov             p_j2+12(%rsp),%eax              # get an individual index
                mov             (%rdi,%rax,4),%edx              # get the f1 value
                mov              %edx,p_j2+12(%rsp)             # save the f1 value

                addps   %xmm9,%xmm7                     # +r2

                addps   %xmm5,%xmm7                     # + x^2/2
                addps   %xmm7,%xmm6                     # +r1


                # deal with infinite or denormal results
                movdqa  p_m2(%rsp),%xmm7
                movdqa  p_m2(%rsp),%xmm5
                pcmpgtd .L__int_127(%rip),%xmm5
                pminsw  .L__int_128(%rip),%xmm7 # ceil at 128
                movmskps        %xmm5,%eax
                test            $0x0f,%eax

                paddd   .L__int_127(%rip),%xmm7 # add bias

        #    *z2 = f2 + ((f1 + f2) * q);
                mulps   p_j2(%rsp),%xmm6                # * f1
                addps   p_j2(%rsp),%xmm6                # + f1
                jnz             .L__exp_largef2
.L__check2:
                pxor    %xmm1,%xmm1                             # floor at 0
                pmaxsw  %xmm1,%xmm7

                pslld   $23,%xmm7                                       # build 2^n

                movaps  %xmm7,%xmm1


                # check for infinity or nan
                movaps  p_ux2(%rsp),%xmm7
                andps   .L__real_infinity(%rip),%xmm7
                cmpps   $0,.L__real_infinity(%rip),%xmm7
                movmskps        %xmm7,%ebx
                test            $0x0f,%ebx


		# end of splitexp
		#        /* Scale (z1 + z2) by 2.0**m */
		#      Step 3. Reconstitute.

		mulps	%xmm6,%xmm1						# result *= 2^n

		jnz			.L__exp_naninf2

.L__vsa_bottom2:



#
.L__final_check:
	mov		save_rbx(%rsp),%rbx		# restore rbx
	add		$stack_size,%rsp
	ret

# at least one of the numbers needs special treatment
.L__exp_naninf:
	lea		p_ux(%rsp),%rcx
	lea		p_j(%rsp),%rsi
	call  .L__fexp_naninf
	jmp		.L__vsa_bottom1
.L__exp_naninf2:
	lea		p_ux2(%rsp),%rcx
	lea		p_j(%rsp),%rsi
	movaps	%xmm0,%xmm2
	movaps	%xmm1,%xmm0
	call  .L__fexp_naninf
	movaps	%xmm0,%xmm1
	movaps	%xmm2,%xmm0
	jmp		.L__vsa_bottom2

#  deal with nans and infinities
# This subroutine checks a packed single for nans and infinities and
# produces the proper result from the exceptional inputs
# Register assumptions:
# Inputs:
# rbx - mask of errors
# xmm0 - computed result vector
# Outputs:
# xmm0 - new result vector
# %rax,rdx,rbx,%xmm2 all modified.

.L__fexp_naninf:
	sub		$0x018,%rsp
	movaps	%xmm0,(%rsi)	# save the computed values
	test	$1,%ebx					# first value?
	jz		.L__Lni2
	mov		0(%rcx),%edx	# get the input
	call	.L__naninf
	mov		%edx,0(%rsi)	# copy the result
.L__Lni2:
	test	$2,%ebx					# second value?
	jz		.L__Lni3
	mov		4(%rcx),%edx	# get the input
	call	.L__naninf
	mov		%edx,4(%rsi)	# copy the result
.L__Lni3:
	test	$4,%ebx					# third value?
	jz		.L__Lni4
	mov		8(%rcx),%edx	# get the input
	call	.L__naninf
	mov		%edx,8(%rsi)	# copy the result
.L__Lni4:
	test	$8,%ebx					# fourth value?
	jz		.L__Lnie
	mov		12(%rcx),%edx	# get the input
	call	.L__naninf
	mov		%edx,12(%rsi)	# copy the result
.L__Lnie:
	movaps	(%rsi),%xmm0	# get the answers
	add		$0x018,%rsp
	ret

#
# a simple subroutine to check a scalar input value for infinity
# or NaN and return the correct result
# expects input in .Land,%edx returns value in edx.  Destroys eax.
.L__naninf:
	mov		$0x0007FFFFF,%eax
	test	%eax,%edx
	jnz		.L__enan		# jump if mantissa not zero, so it's a NaN
# inf
	mov		%edx,%eax
	rcl		$1,%eax
	jnc		.L__r			# exp(+inf) = inf
	xor		%edx,%edx		# exp(-inf) = 0
	jmp		.L__r

#NaN
.L__enan:
	mov		$0x000400000,%eax	# convert to quiet
	or		%eax,%edx
.L__r:
	ret
        .align  16
#  deal with m > 127.  In some instances, rounding during calculations
#  can result in infinity when it shouldn't.  For these cases, we scale
#  m down, and scale the mantissa up.

.L__exp_largef:
        movdqa    %xmm0,p_j(%rsp)    # save the mantissa portion
        movdqa    %xmm1,p_m(%rsp)       # save the exponent portion
        mov       %eax,%ecx              # save the error mask
        test    $1,%ecx                  # first value?
        jz       .L__Lf2
        mov      p_m(%rsp),%edx # get the exponent
        sub      $1,%edx                # scale it down
        mov      %edx,p_m(%rsp)       # save the exponent
        movss   p_j(%rsp),%xmm3     # get the mantissa
        mulss   .L__real_two(%rip),%xmm3        # scale it up
        movss    %xmm3,p_j(%rsp)   # save the mantissa
.L__Lf2:
        test    $2,%ecx                 # second value?
        jz       .L__Lf3
        mov      p_m+4(%rsp),%edx # get the exponent
        sub      $1,%edx                # scale it down
        mov      %edx,p_m+4(%rsp)       # save the exponent
        movss   p_j+4(%rsp),%xmm3     # get the mantissa
        mulss   .L__real_two(%rip),%xmm3        # scale it up
        movss     %xmm3,p_j+4(%rsp)   # save the mantissa
.L__Lf3:
        test    $4,%ecx                 # third value?
        jz       .L__Lf4
        mov      p_m+8(%rsp),%edx # get the exponent
        sub      $1,%edx                # scale it down
        mov      %edx,p_m+8(%rsp)       # save the exponent
        movss   p_j+8(%rsp),%xmm3     # get the mantissa
        mulss   .L__real_two(%rip),%xmm3        # scale it up
        movss    %xmm3,p_j+8(%rsp)   # save the mantissa
.L__Lf4:
        test    $8,%ecx                                 # fourth value?
        jz       .L__Lfe
        mov      p_m+12(%rsp),%edx        # get the exponent
        sub      $1,%edx                 # scale it down
        mov      %edx,p_m+12(%rsp)      # save the exponent
        movss   p_j+12(%rsp),%xmm3    # get the mantissa
        mulss   .L__real_two(%rip),%xmm3        # scale it up
        movss     %xmm3,p_j+12(%rsp)  # save the mantissa
.L__Lfe:
        movaps  p_j(%rsp),%xmm0      # restore the mantissa portion back
        movdqa  p_m(%rsp),%xmm1         # restore the exponent portion
        jmp             .L__check1

        .align  16

.L__exp_largef2:
        movdqa    %xmm6,p_j(%rsp)    # save the mantissa portion
        movdqa    %xmm7,p_m2(%rsp)      # save the exponent portion
        mov             %eax,%ecx                                       # save the error mask
        test    $1,%ecx                                 # first value?
        jz              .L__Lf22
        mov             p_m2+0(%rsp),%edx       # get the exponent
        sub             $1,%edx                                         # scale it down
        mov               %edx,p_m2+0(%rsp)     # save the exponent
        movss   p_j+0(%rsp),%xmm3    # get the mantissa
        mulss   .L__real_two(%rip),%xmm3        # scale it up
        movss     %xmm3,p_j+0(%rsp)  # save the mantissa
.L__Lf22:
        test    $2,%ecx                                 # second value?
        jz              .L__Lf32
        mov             p_m2+4(%rsp),%edx       # get the exponent
        sub             $1,%edx                                         # scale it down
        mov               %edx,p_m2+4(%rsp)     # save the exponent
        movss   p_j+4(%rsp),%xmm3    # get the mantissa
        mulss   .L__real_two(%rip),%xmm3        # scale it up
        movss     %xmm3,p_j+4(%rsp)  # save the mantissa
.L__Lf32:
        test    $4,%ecx                                 # third value?
        jz              .L__Lf42
        mov             p_m2+8(%rsp),%edx       # get the exponent
        sub             $1,%edx                                         # scale it down
        mov               %edx,p_m2+8(%rsp)     # save the exponent
        movss   p_j+8(%rsp),%xmm3    # get the mantissa
        mulss   .L__real_two(%rip),%xmm3        # scale it up
        movss     %xmm3,p_j+8(%rsp)  # save the mantissa
.L__Lf42:
        test    $8,%ecx                                 # fourth value?
        jz              .L__Lfe2
        mov             p_m2+12(%rsp),%edx      # get the exponent
        sub             $1,%edx                                         # scale it down
        mov               %edx,p_m2+12(%rsp)    # save the exponent
        movss   p_j+12(%rsp),%xmm3   # get the mantissa
        mulss   .L__real_two(%rip),%xmm3        # scale it up
        movss     %xmm3,p_j+12(%rsp) # save the mantissa
.L__Lfe2:
        movaps  p_j(%rsp),%xmm6      # restore the mantissa portion back
        movdqa  p_m2(%rsp),%xmm7                # restore the exponent portion
        jmp             .L__check2

	.data		# MUCH better performance without this on my tests
        .align  64
.L__real_half:                  .long 0x03f000000       # 1/2
                                .long 0x03f000000
                                .long 0x03f000000
                                .long 0x03f000000
.L__real_two:                   .long 0x40000000        # 2
                                .long 0x40000000
                                .long 0x40000000
                                .long 0x40000000

.L__real_8192:                  .long 0x46000000        # 8192, to protect against really large numbers
                                .long 0x46000000
                                .long 0x46000000
                                .long 0x46000000
.L__real_m8192:                 .long 0xC6000000        # -8192, to protect against really small numbers
                                .long 0xC6000000
                                .long 0xC6000000
                                .long 0xC6000000
.L__real_thirtytwo_by_log2:     .long 0x04238AA3B       # thirtytwo_by_log2
                                .long 0x04238AA3B
                                .long 0x04238AA3B
                                .long 0x04238AA3B
.L__real_log2_by_32:            .long 0x03CB17218       # log2_by_32
                                .long 0x03CB17218
                                .long 0x03CB17218
                                .long 0x03CB17218
.L__real_log2_by_32_head:       .long 0x03CB17000       # log2_by_32
                                .long 0x03CB17000
                                .long 0x03CB17000
                                .long 0x03CB17000
.L__real_log2_by_32_tail:       .long 0x0B585FDF4       # log2_by_32
                                .long 0x0B585FDF4
                                .long 0x0B585FDF4
                                .long 0x0B585FDF4
.L__real_1_6:                   .long 0x03E2AAAAB       # 0.16666666666 used in polynomial
                                .long 0x03E2AAAAB
                                .long 0x03E2AAAAB
                                .long 0x03E2AAAAB
.L__real_1_24:                  .long 0x03D2AAAAB       # 0.041666668 used in polynomial
                                .long 0x03D2AAAAB
                                .long 0x03D2AAAAB
                                .long 0x03D2AAAAB
.L__real_1_120:                 .long 0x03C088889       # 0.0083333338 used in polynomial
                                .long 0x03C088889
                                .long 0x03C088889
                                .long 0x03C088889
.L__real_infinity:              .long 0x07f800000       # infinity
                                .long 0x07f800000
                                .long 0x07f800000
                                .long 0x07f800000
.L__int_mask_1f:                .long 0x00000001f
                                .long 0x00000001f
                                .long 0x00000001f
                                .long 0x00000001f
.L__int_128:                    .long 0x000000080
                                .long 0x000000080
                                .long 0x000000080
                                .long 0x000000080
.L__int_127:                    .long 0x00000007f
                                .long 0x00000007f
                                .long 0x00000007f
                                .long 0x00000007f

.L__two_to_jby32_table:
        .long   0x03F800000             # 1.0000000000000000
        .long   0x03F82CD87             # 1.0218971486541166
        .long   0x03F85AAC3             # 1.0442737824274138
        .long   0x03F88980F             # 1.0671404006768237
        .long   0x03F8B95C2             # 1.0905077326652577
        .long   0x03F8EA43A             # 1.1143867425958924
        .long   0x03F91C3D3             # 1.1387886347566916
        .long   0x03F94F4F0             # 1.1637248587775775
        .long   0x03F9837F0             # 1.1892071150027210
        .long   0x03F9B8D3A             # 1.2152473599804690
        .long   0x03F9EF532             # 1.2418578120734840
        .long   0x03FA27043             # 1.2690509571917332
        .long   0x03FA5FED7             # 1.2968395546510096
        .long   0x03FA9A15B             # 1.3252366431597413
        .long   0x03FAD583F             # 1.3542555469368927
        .long   0x03FB123F6             # 1.3839098819638320
        .long   0x03FB504F3             # 1.4142135623730951
        .long   0x03FB8FBAF             # 1.4451808069770467
        .long   0x03FBD08A4             # 1.4768261459394993
        .long   0x03FC12C4D             # 1.5091644275934228
        .long   0x03FC5672A             # 1.5422108254079407
        .long   0x03FC9B9BE             # 1.5759808451078865
        .long   0x03FCE248C             # 1.6104903319492543
        .long   0x03FD2A81E             # 1.6457554781539649
        .long   0x03FD744FD             # 1.6817928305074290
        .long   0x03FDBFBB8             # 1.7186192981224779
        .long   0x03FE0CCDF             # 1.7562521603732995
        .long   0x03FE5B907             # 1.7947090750031072
        .long   0x03FEAC0C7             # 1.8340080864093424
        .long   0x03FEFE4BA             # 1.8741676341103000
        .long   0x03FF5257D             # 1.9152065613971474
        .long   0x03FFA83B3             # 1.9571441241754002
        .long 0                                 # for alignment


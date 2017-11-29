
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
# vrs4powxf.asm
#
# A vector implementation of the powf libm function.
# This routine raises the x vector to a constant y power.
#
# Prototype:
#
#     __m128 __vrs4_powxf(__m128 x,float y);
#
#   Computes x raised to the y power.  Returns proper C99 values.
#
#
#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif



# define local variable storage offsets
.equ	p_temp,0x00		# xmmword
.equ	p_negateres,0x10	# qword

.equ	save_rbx,0x020		#qword
.equ	save_rsi,0x028		#qword

.equ	p_xptr,0x030		# ptr to x values
.equ	p_y,0x038		# y value

.equ	p_inty,0x040		# integer y indicators

.equ	p_ux,0x050		# absolute x
.equ	p_ax,0x060		# absolute x
.equ	p_sx,0x070		# sign of x's

.equ	stack_size,0x088	#





    .text
    .align 16
    .p2align 4,,15
.globl __vrs4_powxf
    .type   __vrs4_powxf,@function
__vrs4_powxf:

	sub		$stack_size,%rsp
	mov		%rbx,save_rbx(%rsp)	# save rbx

	lea		p_ux(%rsp),%rcx
	mov		  %rcx,p_xptr(%rsp)		# save pointer to x
	movaps		%xmm0,(%rcx)
	movss	  %xmm1,p_y(%rsp)		# save y

	movdqa	%xmm1,%xmm4

	movaps	%xmm0,%xmm2
	andps	.L__mask_nsign(%rip),%xmm0		# get abs x
	andps	.L__mask_sign(%rip),%xmm2			# mask for the sign bits
	movaps	  %xmm0,p_ax(%rsp)		# save them
	movaps	  %xmm2,p_sx(%rsp)		# save them
# convert all four x's to double
	cvtps2pd   p_ax(%rsp),%xmm0
	cvtps2pd   p_ax+8(%rsp),%xmm1
#
# classify y
# vector 32 bit integer method	 25 cycles to here
#  /* See whether y is an integer.
#     inty = 0 means not an integer.
#  */
# get yexp
	mov		p_y(%rsp),%r8d						# r8 is uy
	mov		$0x07fffffff,%r9d
	and		%r8d,%r9d						# r9 is ay

## if |y| == 0	then return 1
	cmp		$0,%r9d							# is y a zero?
	jz		.Ly_zero

	mov		$0x07f800000,%eax				# EXPBITS_SP32
	and		%r9d,%eax						# y exp

	xor		%edi,%edi
	shr		$23,%eax			#>> EXPSHIFTBITS_SP32
	sub		$126,%eax		# - EXPBIAS_SP32 + 1   - eax is now the unbiased exponent
	mov		$1,%ebx
	cmp		%ebx,%eax			## if (yexp < 1)
	cmovl	%edi,%ebx
	jl		.Lsave_inty

	mov		$24,%ecx
	cmp		%ecx,%eax			## if (yexp >24)
	jle		.Linfy1
	mov		$2,%ebx
	jmp		.Lsave_inty
.Linfy1:						# else 1<=yexp<=24
	sub		%eax,%ecx			# build mask for mantissa
	shl		%cl,%ebx
	dec		%ebx				# rbx = mask = (1 << (24 - yexp)) - 1

	mov		%r8d,%eax
	and		%ebx,%eax			## if ((uy & mask) != 0)
	cmovnz	%edi,%ebx			#   inty = 0;
	jnz		.Lsave_inty

	not		%ebx				# else if (((uy & ~mask) >> (24 - yexp)) & 0x00000001)
	mov		%r8d,%eax
	and		%ebx,%eax
	shr		%cl,%eax
	inc		%edi
	and		%edi,%eax
	mov		%edi,%ebx			#  inty = 1
	jnz		.Lsave_inty
	inc		%ebx				# else	inty = 2


.Lsave_inty:
	mov		  %r8d,p_y+4(%rsp)						# r8d is ay
	mov		  %ebx,p_inty(%rsp)		# save inty
#
# do more x special case checking
#
	pxor	%xmm3,%xmm3
	xor		%eax,%eax
	mov		$0x07FC00000,%ecx
	cmp		$0,%ebx							# is y not an integer?
	cmovz	%ecx,%eax							# then set to return a NaN.  else 0.
	mov		$0x080000000,%ecx
	cmp		$1,%ebx							# is y an odd integer?
	cmovz	%ecx,%eax							# maybe set sign bit if so
	movd	%eax,%xmm5
	pshufd	$0,%xmm5,%xmm5

	pcmpeqd	p_sx(%rsp),%xmm3		## if the signs are set
	pandn	%xmm5,%xmm3						# then negateres gets the values as shown below
	movdqa	  %xmm3,p_negateres(%rsp)	# save negateres

#  /* p_negateres now means the following.
#     7FC00000 means x<0, y not an integer, return NaN.
#     80000000 means x<0, y is odd integer, so set the sign bit.
##     0 means even integer, and/or x>=0.
#  */

# **** Here starts the main calculations  ****
# The algorithm used is x**y = exp(y*log(x))
#  Extra precision is required in intermediate steps to meet the 1ulp requirement
#
# log(x) calculation
	call		__vrd4_log@PLT					# get the double precision log value
											# for all four x's
# y* logx
	cvtps2pd   p_y(%rsp),%xmm2		#convert the two packed single y's to double

#  /* just multiply by y */
	mulpd	%xmm2,%xmm0
	mulpd	%xmm2,%xmm1

#  /* The following code computes r = exp(w) */
	call		__vrd4_exp@PLT					# get the double exp value
											# for all four y*log(x)'s
#
# convert all four results to double
	cvtpd2ps	%xmm0,%xmm0
	cvtpd2ps	%xmm1,%xmm1
	movlhps		%xmm1,%xmm0

# perform special case and error checking on input values

# special case checking is done first in the scalar version since
# it allows for early fast returns.  But for vectors, we consider them
# to be rare, so early returns are not necessary.  So we first compute
# the x**y values, and then check for special cases.

# we do some of the checking in reverse order of the scalar version.
# apply the negate result flags
	orps	p_negateres(%rsp),%xmm0	# get negateres

## if y is infinite or so large that the result would overflow or underflow
	mov		p_y(%rsp),%edx			# get y
	and 	$0x07fffffff,%edx					# develop ay
	cmp		$0x04f000000,%edx
	ja		.Ly_large
.Lrnsx3:

## if x is infinite
	movdqa	p_ax(%rsp),%xmm4
	cmpps	$0,.L__mask_inf(%rip),%xmm4	# equal to infinity, ffs if so.
	movmskps %xmm4,%edx
	test	$0x0f,%edx
	jnz		.Lx_infinite
.Lrnsx1:
## if x is zero
	xorps	%xmm4,%xmm4
	cmpps	$0,p_ax(%rsp),%xmm4	# equal to zero, ffs if so.
	movmskps %xmm4,%edx
	test	$0x0f,%edx
	jnz		.Lx_zero
.Lrnsx2:
## if y is NAN
	movss	p_y(%rsp),%xmm4			# get y
	ucomiss	%xmm4,%xmm4						# comparing y to itself should
											# be true, unless y is a NaN. parity flag if NaN.
	jp		.Ly_NaN
.Lrnsx4:
## if x is NAN
	movdqa	p_ax(%rsp),%xmm4			# get x
	cmpps	$4,%xmm4,%xmm4						# a compare not equal  of x to itself should
											# be false, unless x is a NaN. ff's if NaN.
	movmskps %xmm4,%ecx
	test	$0x0f,%ecx
	jnz		.Lx_NaN
.Lrnsx5:

## if x == +1, return +1 for all x
	movdqa	.L__float_one(%rip),%xmm3	# one
	mov		p_xptr(%rsp),%rdx		# get pointer to x
	movdqa	%xmm3,%xmm2
	cmpps	$4,(%rdx),%xmm2		# not equal to +1.0?, ffs if not equal.
	andps	%xmm2,%xmm0						# keep the others
	andnps	%xmm3,%xmm2						# mask for ones
	orps	%xmm2,%xmm0

.L__powf_cleanup2:

	mov		save_rbx(%rsp),%rbx		# restore rbx
	add		$stack_size,%rsp
	ret

	.align 16
.Ly_zero:
## if |y| == 0	then return 1
	movdqa	.L__float_one(%rip),%xmm0	# one
	jmp		.L__powf_cleanup2
#      * y is a NaN.
.Ly_NaN:
	mov		p_y(%rsp),%r8d
	or		$0x000400000,%r8d				# convert to QNaNs
	movd	%r8d,%xmm0					# propagate to all results
	shufps	$0,%xmm0,%xmm0
	jmp	   	.Lrnsx4

#       y is a NaN.
.Lx_NaN:
	mov		p_xptr(%rsp),%rcx		# get pointer to x
	movdqa	(%rcx),%xmm4			# get x
	movdqa	%xmm4,%xmm3
	movdqa	%xmm4,%xmm5
	movdqa	.L__mask_sigbit(%rip),%xmm2	# get the signalling bits
	cmpps	$0,%xmm4,%xmm4						# a compare equal  of x to itself should
											# be true, unless x is a NaN. 0's if NaN.
	cmpps	$4,%xmm3,%xmm3						# compare not equal, ff's if NaN.
	andps	%xmm4,%xmm0						# keep the other results
	andps	%xmm3,%xmm2						# get just the right signalling bits
	andps	%xmm5,%xmm3						# mask for the NaNs
	orps	%xmm2,%xmm3	# convert to QNaNs
	orps	%xmm3,%xmm0						# combine
	jmp	   	.Lrnsx5

#      * y is infinite or so large that the result would
#         overflow or underflow.
.Ly_large:
	movdqa	  %xmm0,p_temp(%rsp)

	mov		p_xptr(%rsp),%rcx		# get pointer to x
	mov		(%rcx),%eax
	mov		p_y(%rsp),%ebx
	mov		p_inty(%rsp),%ecx
	sub		$8,%rsp
	call	.Lnp_special6					# call the handler for one value
	add		$8,%rsp
	mov		  %eax,p_temp(%rsp)

	mov		p_xptr(%rsp),%rcx		# get pointer to x
	mov		4(%rcx),%eax
	mov		p_y(%rsp),%ebx
	mov		p_inty(%rsp),%ecx
	sub		$8,%rsp
	call	.Lnp_special6					# call the handler for one value
	add		$8,%rsp
	mov		  %eax,p_temp+4(%rsp)

	mov		p_xptr(%rsp),%rcx		# get pointer to x
	mov		8(%rcx),%eax
	mov		p_y(%rsp),%ebx
	mov		p_inty(%rsp),%ecx
	sub		$8,%rsp
	call	.Lnp_special6					# call the handler for one value
	add		$8,%rsp
	mov		  %eax,p_temp+8(%rsp)

	mov		p_xptr(%rsp),%rcx		# get pointer to x
	mov		12(%rcx),%eax
	mov		p_y(%rsp),%ebx
	mov		p_inty(%rsp),%ecx
	sub		$8,%rsp
	call	.Lnp_special6					# call the handler for one value
	add		$8,%rsp
	mov		  %eax,p_temp+12(%rsp)

	movdqa	p_temp(%rsp),%xmm0
	jmp 	.Lrnsx3

# a subroutine to treat an individual x,y pair when y is large or infinity
# assumes x in .Ly(%rip),%eax in ebx.
# returns result in eax
.Lnp_special6:
# handle |x|==1 cases first
	mov		$0x07FFFFFFF,%r8d
	and		%eax,%r8d
	cmp		$0x03f800000,%r8d					  # jump if |x| !=1
	jnz		.Lnps6
	mov		$0x03f800000,%eax					  # return 1 for all |x|==1
	jmp 	.Lnpx64

# cases where  |x| !=1
.Lnps6:
	mov		$0x07f800000,%ecx
	xor		%eax,%eax							  # assume 0 return
	test	$0x080000000,%ebx
	jnz		.Lnps62							  # jump if y negative
# y = +inf
	cmp		$0x03f800000,%r8d
	cmovg	%ecx,%eax							  # return inf if |x| < 1
	jmp 	.Lnpx64
.Lnps62:
# y = -inf
	cmp		$0x03f800000,%r8d
	cmovl	%ecx,%eax							  # return inf if |x| < 1
	jmp 	.Lnpx64

.Lnpx64:
	ret

# handle cases where x is +/- infinity.  edx is the mask
	.align 16
.Lx_infinite:
	movdqa	  %xmm0,p_temp(%rsp)

	test	$1,%edx
	jz		.Lxinfa
	mov		p_xptr(%rsp),%rcx		# get pointer to x
	mov		(%rcx),%eax
	mov		p_y(%rsp),%ebx
	mov		p_inty(%rsp),%ecx
	sub		$8,%rsp
	call	.Lnp_special_x1					# call the handler for one value
	add		$8,%rsp
	mov		  %eax,p_temp(%rsp)
.Lxinfa:
	test	$2,%edx
	jz		.Lxinfb
	mov		p_xptr(%rsp),%rcx		# get pointer to x
	mov		p_y(%rsp),%ebx
	mov		4(%rcx),%eax
	mov		p_inty(%rsp),%ecx
	sub		$8,%rsp
	call	.Lnp_special_x1					# call the handler for one value
	add		$8,%rsp
	mov		  %eax,p_temp+4(%rsp)
.Lxinfb:
	test	$4,%edx
	jz		.Lxinfc
	mov		p_xptr(%rsp),%rcx		# get pointer to x
	mov		p_y(%rsp),%ebx
	mov		8(%rcx),%eax
	mov		p_inty(%rsp),%ecx
	sub		$8,%rsp
	call	.Lnp_special_x1					# call the handler for one value
	add		$8,%rsp
	mov		  %eax,p_temp+8(%rsp)
.Lxinfc:
	test	$8,%edx
	jz		.Lxinfd
	mov		p_xptr(%rsp),%rcx		# get pointer to x
	mov		p_y(%rsp),%ebx
	mov		12(%rcx),%eax
	mov		p_inty(%rsp),%ecx
	sub		$8,%rsp
	call	.Lnp_special_x1					# call the handler for one value
	add		$8,%rsp
	mov		  %eax,p_temp+12(%rsp)
.Lxinfd:
	movdqa	p_temp(%rsp),%xmm0
	jmp 	.Lrnsx1

# a subroutine to treat an individual x,y pair when x is +/-infinity
# assumes x in .Ly(%rip),%eax in ebx, inty in ecx.
# returns result in eax
.Lnp_special_x1:											# x is infinite
	test	$0x080000000,%eax								# is x positive
	jnz		.Lnsx11										# jump if not
	test	$0x080000000,%ebx								# is y positive
	jz		.Lnsx13											# just return if so
	xor		%eax,%eax										# else return 0
	jmp 	.Lnsx13

.Lnsx11:
	cmp		$1,%ecx										## if inty ==1
	jnz		.Lnsx12										# jump if not
	test	$0x080000000,%ebx								# is y positive
	jz		.Lnsx13											# just return if so
	mov		$0x080000000,%eax								# else return -0
	jmp 	.Lnsx13
.Lnsx12:													# inty <>1
	and		$0x07FFFFFFF,%eax								# return -x (|x|)  if y<0
	test	$0x080000000,%ebx								# is y positive
	jz		.Lnsx13											#
	xor		%eax,%eax										# return 0  if y >=0
.Lnsx13:
	ret


# handle cases where x is +/- zero.  edx is the mask of x,y pairs with |x|=0
	.align 16
.Lx_zero:
	movdqa	  %xmm0,p_temp(%rsp)

	test	$1,%edx
	jz		.Lxzera
	mov		p_xptr(%rsp),%rcx		# get pointer to x
	mov		p_y(%rsp),%ebx
	mov		(%rcx),%eax
	mov		p_inty(%rsp),%ecx
	sub		$8,%rsp
	call	.Lnp_special_x2					# call the handler for one value
	add		$8,%rsp
	mov		  %eax,p_temp(%rsp)
.Lxzera:
	test	$2,%edx
	jz		.Lxzerb
	mov		p_xptr(%rsp),%rcx		# get pointer to x
	mov		p_y(%rsp),%ebx
	mov		4(%rcx),%eax
	mov		p_inty(%rsp),%ecx
	sub		$8,%rsp
	call	.Lnp_special_x2					# call the handler for one value
	add		$8,%rsp
	mov		  %eax,p_temp+4(%rsp)
.Lxzerb:
	test	$4,%edx
	jz		.Lxzerc
	mov		p_xptr(%rsp),%rcx		# get pointer to x
	mov		p_y(%rsp),%ebx
	mov		8(%rcx),%eax
	mov		p_inty(%rsp),%ecx
	sub		$8,%rsp
	call	.Lnp_special_x2					# call the handler for one value
	add		$8,%rsp
	mov		  %eax,p_temp+8(%rsp)
.Lxzerc:
	test	$8,%edx
	jz		.Lxzerd
	mov		p_xptr(%rsp),%rcx		# get pointer to x
	mov		p_y(%rsp),%ebx
	mov		12(%rcx),%eax
	mov		p_inty(%rsp),%ecx
	sub		$8,%rsp
	call	.Lnp_special_x2					# call the handler for one value
	add		$8,%rsp
	mov		  %eax,p_temp+12(%rsp)
.Lxzerd:
	movdqa	p_temp(%rsp),%xmm0
	jmp 	.Lrnsx2

# a subroutine to treat an individual x,y pair when x is +/-0
# assumes x in .Ly(%rip),%eax in ebx, inty in ecx.
# returns result in eax
	.align 16
.Lnp_special_x2:
	cmp		$1,%ecx										## if inty ==1
	jz		.Lnsx21										# jump if so
# handle cases of x=+/-0, y not integer
	xor		%eax,%eax
	mov		$0x07f800000,%ecx
	test	$0x080000000,%ebx								# is ypos
	cmovnz	%ecx,%eax
	jmp		.Lnsx23
# y is an integer
.Lnsx21:
	xor		%r8d,%r8d
	mov		$0x07f800000,%ecx
	test	$0x080000000,%ebx								# is ypos
	cmovnz	%ecx,%r8d										# set to infinity if not
	and		$0x080000000,%eax								# pickup the sign of x
	or		%r8d,%eax										# and include it in the result
.Lnsx23:
	ret


        .data
        .align 64

.L__mask_sign:			.quad 0x08000000080000000	# a sign bit mask
				.quad 0x08000000080000000

.L__mask_nsign:			.quad 0x07FFFFFFF7FFFFFFF	# a not sign bit mask
				.quad 0x07FFFFFFF7FFFFFFF

# used by special case checking

.L__float_one:			.quad 0x03f8000003f800000	# one
				.quad 0x03f8000003f800000

.L__mask_inf:			.quad 0x07f8000007F800000	# inifinity
				.quad 0x07f8000007F800000

.L__mask_sigbit:		.quad 0x00040000000400000	# QNaN bit
				.quad 0x00040000000400000



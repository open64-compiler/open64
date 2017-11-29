
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





# v4help.s
#
# Helper routines for testing the x4 double and x8 single vector
# math functions.
#
# Prototype:
#
#     void v4exp(__m128d x1, __m128d x2, double * ya);
#
#   Computes 4 exp values simultaneously and returns them
#   in the v4a array.
#   Assumes that ya is 16 byte aligned.
#
#

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif


# %xmm0 - __m128d x1
# %xmm1 - __m128d x2
# rdi   - double *ya

.extern	__vrd4_exp
    .text
    .align 16
    .p2align 4,,15
.globl v4exp
    .type   v4exp,@function
v4exp:
	push	%rdi
	call	__vrd4_exp@PLT
	pop		%rdi
	movdqa	%xmm0,(%rdi)
	movdqa	%xmm1,16(%rdi)
	ret


# %xmm0,%rcx - __m128d x1
# %xmm1,%rdx - __m128d x2
# r8   - double *ya

.extern	__vrs8_expf
    .text
    .align 16
    .p2align 4,,15
.globl v8expf
    .type   v8expf,@function
v8expf:
	push	%rdi
	call	__vrs8_expf@PLT
	pop		%rdi
	movdqa	%xmm0,(%rdi)
	movdqa	%xmm1,16(%rdi)
	ret


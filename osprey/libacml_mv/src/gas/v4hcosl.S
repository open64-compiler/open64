
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





# v4hcosl.s
#
# Helper routines for testing the x4 double and x8 single vector
# math functions.
#
# Prototype:
#
#     void v4cos(__m128d x1, __m128d x2, double * ya);
#
#   Computes 4 cos values simultaneously and returns them
#   in the v4a array.
#   Assumes that ya is 16 byte aligned.
#
#

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

# xmm0 - __m128d x1
# xmm1 - __m128d x2
# rdi   - double *ya

.extern __vrd4_cos
    .text
    .align 16
    .p2align 4,,15
.globl v4cos
    .type   v4cos,@function
v4cos:
        push    %rdi
        call    __vrd4_cos@PLT
        pop             %rdi
        movdqa  %xmm0,(%rdi)
        movdqa  %xmm1,16(%rdi)
	ret

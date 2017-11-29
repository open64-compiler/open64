
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


#copysignf.S
#
# An implementation of the copysignf libm function.
#
# The copysign functions produce a value with the magnitude of x and the sign of y.
# They produce a NaN (with the sign of y) if x is a NaN. On implementations that
# represent a signed zero but do not treat negative zero consistently in arithmetic
# operations, the copysign functions regard the sign of zero as positive.
#
# Prototype:
#
#     float copysignf(float x, float y)#
#

#
#   Algorithm:
#

#include "fn_macros.h"
#define fname FN_PROTOTYPE(copysignf)

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

.text
.align 16
.p2align 4,,15
.globl fname
.type fname,@function
fname:
	#PANDN .L__fabsf_and_mask, %xmm1
	#POR %xmm1,%xmm0 

	PSLLD $1,%xmm0
	PSRLD $1,%xmm0
	PSRLD $31,%xmm1
	PSLLD $31,%xmm1
	POR   %xmm1,%xmm0
	
    ret

#.align 16
#.L__sign_mask:               .long 0x7FFFFFFF
                             .long 0x0
                             .quad 0x0


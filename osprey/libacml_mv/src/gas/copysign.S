
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


#copysign.S
#
# An implementation of the copysign libm function.
#
# The copysign functions produce a value with the magnitude of x and the sign of y.
# They produce a NaN (with the sign of y) if x is a NaN. On implementations that
# represent a signed zero but do not treat negative zero consistently in arithmetic
# operations, the copysign functions regard the sign of zero as positive.
#
#
# Prototype:
#
#     double copysign(float x, float y)
#
#
#
#   Algorithm:
#

#include "fn_macros.h"
#define fname FN_PROTOTYPE(copysign)

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

.text
.align 16
.p2align 4,,15
.globl fname
.type fname,@function
fname:

	PSLLQ $1,%xmm0
	PSRLQ $1,%xmm0
	PSRLQ $63,%xmm1
	PSLLQ $63,%xmm1
	POR   %xmm1,%xmm0
	
    ret

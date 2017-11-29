
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


#fmaxf.S
#
# An implementation of the fmaxf libm function.
#
# The fmax functions determine the maximum numeric value of their arguments.
#
# Prototype:
#
#     float fmaxf(float x, float y)
#

#
#   Algorithm:
#

#include "fn_macros.h"
#define fname FN_PROTOTYPE(fmaxf)

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

.text
.align 16
.p2align 4,,15
.globl fname
.type fname,@function
fname:

	MOVAPD %xmm0,%xmm3		
	
	MAXSS  %xmm1,%xmm0	
	MOVAPD %xmm0,%xmm2		

	#If the input is nan then specal case to return the other operand	
	CMPEQSS %xmm2,%xmm2	
	PAND	%xmm2,%xmm0
	
	PANDN   %xmm3,%xmm2
	POR		%xmm2,%xmm0

    ret


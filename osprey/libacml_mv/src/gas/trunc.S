
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


# trunc.S
#
# An implementation of the trunc libm function.
#
# The trunc functions round their argument to the integer value, in floating format,
# nearest to but no larger in magnitude than the argument.
#
#
# Prototype:
#
#     double trunc(double x);
#

#
#   Algorithm:
#

#include "fn_macros.h"
#define fname FN_PROTOTYPE(trunc)

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

.text
.align 16
.p2align 4,,15
.globl fname
.type fname,@function
fname:

	MOVAPD %xmm0,%xmm1	

#convert double to integer.	
	CVTTSD2SIQ %xmm0,%rax
	CMP .L__Erro_mask(%rip),%rax
	jz .Error_val	
#convert integer to double	
	CVTSI2SDQ %rax,%xmm0
	
	PSRLQ $63,%xmm1
	PSLLQ $63,%xmm1
	
	POR %xmm1,%xmm0
	
	
	ret
	
.Error_val:
	MOVAPD %xmm1,%xmm2
	CMPEQSD	%xmm1,%xmm1			
	ADDSD %xmm2,%xmm2	
	
	PAND %xmm1,%xmm0
	PANDN %xmm2,%xmm1
	POR %xmm1,%xmm0
	
		
    ret

.data
.align 16
.L__Erro_mask:  		.quad 0x8000000000000000
			        .quad 0x0

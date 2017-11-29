
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


# fabs.S
#
# An implementation of the fabs libm function.
#
# Prototype:
#
#     double fabs(double x);
#

#
#   Algorithm: AND the Most Significant Bit of the 
#              double precision number with 0 to get the 
#              floating point absolute.
#

#include "fn_macros.h"
#define fname FN_PROTOTYPE(fabs)
#define fname_special _fabs_special


#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

.text
.align 16
.p2align 4,,15
.globl fname
.type fname,@function
fname:
    #input is in xmm0, which contains the final result also.
    andpd .L__fabs_and_mask(%rip), %xmm0 # <result> latency = 3 
    ret


.align 16
.L__fabs_and_mask:          .quad 0x7FFFFFFFFFFFFFFF
                            .quad 0x0



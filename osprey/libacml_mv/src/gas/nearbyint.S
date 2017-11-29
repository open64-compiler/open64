
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
#   Algorithm:
#

#include "fn_macros.h"
#define fname FN_PROTOTYPE(nearbyint)
#define fname_special _nearbyint_special


# local variable storage offsets

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

.text
.align 16
.p2align 4,,15
.globl fname
.type fname,@function
fname:                                                      
    movsd .L__2p52_mask_64(%rip),%xmm2                             
    movsd .L__sign_mask_64(%rip),%xmm4                               
    movsd %xmm4,%xmm6                                                 
    movsd %xmm0,%xmm1  # move input to xmm register's xmm1 and xmm5 
    movsd %xmm0,%xmm5 
    pand  %xmm4,%xmm1  # xmm1 = abs(xmm1) 
    movsd %xmm1,%xmm3  # move xmm1 to xmm3
    comisd %xmm2,%xmm1 #  
    jnc   .L__greater_than_2p52                      # 
    jp    .L__is_infinity_nan  # parity flag is raised if one of the xmm2 or 
                               # xmm1 is Nan 
.L__normal_input_case:
    #sign.u32 = checkbits.u32[1] & 0x80000000;
    #xmm4 = sign.u32
    pandn %xmm5,%xmm4
    #val_2p52.u32[1] = sign.u32 | 0x43300000;
    #val_2p52.u32[0] = 0;
    por   %xmm4,%xmm2
    #val_2p52.f64 = (x + val_2p52.f64) - val_2p52.f64;
    addpd %xmm2,%xmm5 
    subpd %xmm5,%xmm2
    #val_2p52.u32[1] = ((val_2p52.u32[1] << 1) >> 1) | sign.u32;
    pand  %xmm6,%xmm2
    por   %xmm4,%xmm2
    movsd %xmm2,%xmm0 # move the result to xmm0 register 
    ret
.L__special_case:
.L__greater_than_2p52:
    ret # result is present in xmm0
.L__is_infinity_nan:
    addpd %xmm0,%xmm0
    ret
.align 16
.L__sign_mask_64:          .quad 0x7FFFFFFFFFFFFFFF
                           .quad 0
.L__2p52_mask_64:          .quad 0x4330000000000000 
                           .quad 0
.L__exp_mask_64:           .quad 0x7FF0000000000000
                           .quad 0







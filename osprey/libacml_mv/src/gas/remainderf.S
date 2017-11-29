
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


# remainderf.S
#
# An implementation of the fabs libm function.
#
# Prototype:
#
#     float remainderf(float x,float y);
#

#
#   Algorithm:
#

#include "fn_macros.h"
#define fname FN_PROTOTYPE(remainderf)
#define fname_special _remainderf_special


#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

.text
.align 16
.p2align 4,,15
.globl fname
.type fname,@function
fname:
    mov .L__exp_mask_64(%rip), %rdi
    movapd .L__sign_mask_64(%rip),%xmm6
    cvtss2sd %xmm0,%xmm2 # double x
    cvtss2sd %xmm1,%xmm3 # double y
    pand %xmm6,%xmm2
    pand %xmm6,%xmm3
    movd %xmm2,%rax
    movd %xmm3,%r8
    mov %rax,%r11
    mov %r8,%r9
    movsd %xmm2,%xmm4
    #take the exponents of both x and y
    and %rdi,%rax
    and %rdi,%r8
    ror $52, %rax
    ror $52, %r8
    #ifeither of the exponents is infinity 
    cmp $0X7FF,%rax 
    jz  .L__InputIsNaN 
    cmp $0X7FF,%r8 
    jz  .L__InputIsNaNOrInf

    cmp $0,%r8
    jz  .L__Divisor_Is_Zero

    cmp %r9, %r11
    jz  .L__Input_Is_Equal
    jb  .L__ReturnImmediate

    xor %rcx,%rcx
    mov $24,%rdx
    movsd .L__One_64(%rip),%xmm7 # xmm7 = scale
    cmp %rax,%r8 
    jae .L__y_is_greater
    #xmm3 = dy
    sub %r8,%rax
    div %dl       # al = ntimes
    mov %al,%cl   # cl = ntimes
    and $0xFF,%ax # set everything t o zero except al
    mul %dl       # ax = dl * al = 24* ntimes
    add $1023, %rax
    shl $52,%rax
    movd %rax,%xmm7 # xmm7 = scale
.L__y_is_greater:
    mulsd %xmm3,%xmm7 # xmm7 = scale * dy
    movsd .L__2pminus24_decimal(%rip),%xmm6

.align 16
.L__Start_Loop:
    dec %cl
    js .L__End_Loop
    divsd %xmm7,%xmm4     # xmm7 = (dx / w)
    cvttsd2siq %xmm4,%rax 
    cvtsi2sdq %rax,%xmm4  # xmm4 = t = (double)((int)(dx / w))
    mulsd  %xmm7,%xmm4    # xmm4 = w*t
    mulsd %xmm6,%xmm7     # w*= scale 
    subsd  %xmm4,%xmm2    # xmm2 = dx -= w*t  
    movsd %xmm2,%xmm4     # xmm4 = dx
    jmp .L__Start_Loop
.L__End_Loop:    
    divsd %xmm7,%xmm4     # xmm7 = (dx / w)
    cvttsd2siq %xmm4,%rax 
    cvtsi2sdq %rax,%xmm4  # xmm4 = t = (double)((int)(dx / w))
    and $0x01,%rax        # todd = todd = ((int)(dx / w)) & 1 
    mulsd  %xmm7,%xmm4    # xmm4 = w*t
    subsd  %xmm4,%xmm2    # xmm2 = dx -= w*t  
    movsd  %xmm7,%xmm6    # store w
    mulsd .L__Zero_Point_Five64(%rip),%xmm7 #xmm7 = 0.5*w
    
    cmp $0x01,%rax
    jnz .L__todd_is_even
    comisd %xmm2,%xmm7
    je .L__Subtract_w

.L__todd_is_even: 
    comisd %xmm2,%xmm7
    jnb .L__Dont_Subtract_w
    
.L__Subtract_w:    
    subsd %xmm6,%xmm2
    
.L__Dont_Subtract_w:
    comiss .L__Zero_64(%rip),%xmm0 
    jb .L__Negative
    cvtsd2ss %xmm2,%xmm0 
    ret
.L__Negative:
    movsd .L__MinusZero_64(%rip),%xmm0
    subsd %xmm2,%xmm0
    cvtsd2ss %xmm0,%xmm0 
    ret

.align 16
.L__Input_Is_Equal:
    cmp $0x7FF,%rax
    jz .L__Dividend_Is_Infinity
    cmp $0x7FF,%r8
    jz .L__InputIsNaNOrInf
    movsd %xmm0,%xmm1
    pand .L__sign_bit_32(%rip),%xmm1
    movss .L__Zero_64(%rip),%xmm0
    por  %xmm1,%xmm0
    ret

.L__InputIsNaNOrInf:
    comiss %xmm0,%xmm1
    jp .L__InputIsNaN
    ret
.L__Divisor_Is_Zero:
.L__InputIsNaN:
    por .L__exp_mask_32(%rip),%xmm0
.L__Dividend_Is_Infinity:
    por .L__QNaN_mask_32(%rip),%xmm0
    ret

#Case when x < y
    #xmm2 = dx
.L__ReturnImmediate:
    movsd %xmm3,%xmm5
    mulsd .L__Zero_Point_Five64(%rip), %xmm3 # xmm3 = 0.5*dy
    comisd %xmm3,%xmm2 # if (dx > 0.5*dy)
    jna .L__Finish_Immediate # xmm2 <= xmm3
    subsd %xmm5,%xmm2 #dx -= dy
    
.L__Finish_Immediate:
    comiss .L__Zero_64(%rip),%xmm0
    #xmm0 contains the input and is the result
    jz .L__Zero
    ja .L__Positive

    movsd .L__Zero_64(%rip),%xmm0
    subsd %xmm2,%xmm0
    cvtsd2ss %xmm0,%xmm0
    ret

.L__Zero:
    ret
        
.L__Positive:
    cvtsd2ss %xmm2,%xmm0
    ret
    


.align 32    
.L__sign_bit_32:           .quad 0x8000000080000000
                           .quad 0x0
.L__exp_mask_64:           .quad 0x7FF0000000000000
                           .quad 0x0
.L__exp_mask_32:           .quad 0x000000007F800000
                           .quad 0x0
.L__27bit_andingmask_64:   .quad 0xfffffffff8000000
                           .quad 0
.L__2p52_mask_64:          .quad 0x4330000000000000 
                           .quad 0
.L__One_64:                .quad 0x3FF0000000000000 
                           .quad 0
.L__Zero_64:               .quad 0x0 
                           .quad 0
.L__MinusZero_64:          .quad 0x8000000000000000 
                           .quad 0
.L__QNaN_mask_32:          .quad 0x0000000000400000
                           .quad 0
.L__sign_mask_64:          .quad 0x7FFFFFFFFFFFFFFF
                           .quad 0
.L__2pminus24_decimal:     .quad 0x3E70000000000000
                           .quad 0
.L__Zero_Point_Five64:     .quad 0x3FE0000000000000
                           .quad 0



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


#include "fn_macros.h"
#define fname FN_PROTOTYPE(exp10f)
#define fname_special _exp10f_special@PLT

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

.text
.p2align 4
.globl fname
.type fname,@function
fname:
    ucomiss .L__max_exp_arg(%rip), %xmm0
    ja .L__y_is_inf
    jp .L__y_is_nan
    ucomiss .L__min_exp_arg(%rip), %xmm0
    jb .L__y_is_zero

    cvtps2pd     %xmm0, %xmm0    #xmm0 = (double)x

    # x * (64/log10of(2))
    movapd      %xmm0,%xmm3      #xmm3 = (xouble)x
    mulsd       .L__real_64_by_log10of2(%rip), %xmm3  #xmm3 = x * (64/ln(2)

    # n = int( x * (64/log10of(2)) )
    cvtpd2dq    %xmm3, %xmm4  #xmm4 = (int)n
    cvtdq2pd    %xmm4, %xmm2  #xmm2 = (double)n

    # r = x - n * ln(2)/64
    # r *= ln(10)
    mulsd       .L__real_log10of2_by_64(%rip),%xmm2 #xmm2 = n * log10of(2)/64
    movd        %xmm4, %ecx     #ecx = n
    subsd       %xmm2, %xmm0    #xmm0 = r
    mulsd       .L__real_ln10(%rip),%xmm0 #xmm0 = r = r*ln10
    movapd      %xmm0, %xmm1    #xmm1 = r

    # q = r + r*r(1/2 + r*1/6)
    movapd       .L__real_1_by_6(%rip), %xmm3 
    mulsd       %xmm0, %xmm3 #xmm3 = 1/6 * r
    mulsd       %xmm1, %xmm0 #xmm0 =  r  * r
    addsd       .L__real_1_by_2(%rip), %xmm3 #xmm3 = 1/2 + (1/6 * r)
    mulsd       %xmm3, %xmm0  #xmm0 = r*r*(1/2 + (1/6 * r))
    addsd       %xmm1, %xmm0  #xmm0 = r+r*r*(1/2 + (1/6 * r))
    
    #j = n & 0x3f
    mov         $0x3f, %rax     #rax = 0x3f
    and         %ecx, %eax      #eax = j = n & 0x3f

    # f + (f*q)
    lea         L__two_to_jby64_table(%rip), %r10    
    mulsd       (%r10,%rax,8), %xmm0
    addsd       (%r10,%rax,8), %xmm0

    .p2align 4
    # m = (n - j) / 64        
    psrad       $6,%xmm4
    psllq       $52,%xmm4
    paddq       %xmm0, %xmm4
    cvtpd2ps    %xmm4, %xmm0
    ret

.p2align 4
.L__y_is_zero:
    pxor        %xmm1, %xmm1    #return value in xmm1,input in xmm0 before calling
    mov         $2, %edi        #code in edi
    #call        fname_special
    pxor        %xmm0,%xmm0#remove this if calling fname special
    ret         

.p2align 4
.L__y_is_inf:
    mov         $0x7f800000,%edx
    movd        %edx, %xmm1
    mov         $3, %edi
    #call        fname_special
    movdqa     %xmm1,%xmm0#remove this if calling fname special
    ret     

.p2align 4
.L__y_is_nan:
    movaps %xmm0,%xmm1
    addss  %xmm1,%xmm1
    mov         $1, %edi
    #call        fname_special
    movdqa %xmm1,%xmm0  #remove this if calling fname special
    ret       
    
.data
.align 16
.L__max_exp_arg:                 .long 0x421A209B
.L__min_exp_arg:                 .long 0xC23369F4
.L__real_64_by_log10of2:        .quad 0x406A934F0979A371 # 64/log10(2)
.L__real_log10of2_by_64:        .quad 0x3F734413509F79FF # log10of2_by_64
.L__real_ln10:                  .quad 0x40026BB1BBB55516 # ln(10)
.L__real_1_by_6:                .quad 0x3fc5555555555555 # 1/6
.L__real_1_by_2:                .quad 0x3fe0000000000000 # 1/2

.align 16
.type	L__two_to_jby64_table, @object
.size	L__two_to_jby64_table, 512
L__two_to_jby64_table:
    .quad 0x3ff0000000000000
    .quad 0x3ff02c9a3e778061
    .quad 0x3ff059b0d3158574
    .quad 0x3ff0874518759bc8
    .quad 0x3ff0b5586cf9890f
    .quad 0x3ff0e3ec32d3d1a2
    .quad 0x3ff11301d0125b51
    .quad 0x3ff1429aaea92de0
    .quad 0x3ff172b83c7d517b
    .quad 0x3ff1a35beb6fcb75
    .quad 0x3ff1d4873168b9aa
    .quad 0x3ff2063b88628cd6
    .quad 0x3ff2387a6e756238
    .quad 0x3ff26b4565e27cdd
    .quad 0x3ff29e9df51fdee1
    .quad 0x3ff2d285a6e4030b
    .quad 0x3ff306fe0a31b715
    .quad 0x3ff33c08b26416ff
    .quad 0x3ff371a7373aa9cb
    .quad 0x3ff3a7db34e59ff7
    .quad 0x3ff3dea64c123422
    .quad 0x3ff4160a21f72e2a
    .quad 0x3ff44e086061892d
    .quad 0x3ff486a2b5c13cd0
    .quad 0x3ff4bfdad5362a27
    .quad 0x3ff4f9b2769d2ca7
    .quad 0x3ff5342b569d4f82
    .quad 0x3ff56f4736b527da
    .quad 0x3ff5ab07dd485429
    .quad 0x3ff5e76f15ad2148
    .quad 0x3ff6247eb03a5585
    .quad 0x3ff6623882552225
    .quad 0x3ff6a09e667f3bcd
    .quad 0x3ff6dfb23c651a2f
    .quad 0x3ff71f75e8ec5f74
    .quad 0x3ff75feb564267c9
    .quad 0x3ff7a11473eb0187
    .quad 0x3ff7e2f336cf4e62
    .quad 0x3ff82589994cce13
    .quad 0x3ff868d99b4492ed
    .quad 0x3ff8ace5422aa0db
    .quad 0x3ff8f1ae99157736
    .quad 0x3ff93737b0cdc5e5
    .quad 0x3ff97d829fde4e50
    .quad 0x3ff9c49182a3f090
    .quad 0x3ffa0c667b5de565
    .quad 0x3ffa5503b23e255d
    .quad 0x3ffa9e6b5579fdbf
    .quad 0x3ffae89f995ad3ad
    .quad 0x3ffb33a2b84f15fb
    .quad 0x3ffb7f76f2fb5e47
    .quad 0x3ffbcc1e904bc1d2
    .quad 0x3ffc199bdd85529c
    .quad 0x3ffc67f12e57d14b
    .quad 0x3ffcb720dcef9069
    .quad 0x3ffd072d4a07897c
    .quad 0x3ffd5818dcfba487
    .quad 0x3ffda9e603db3285
    .quad 0x3ffdfc97337b9b5f
    .quad 0x3ffe502ee78b3ff6
    .quad 0x3ffea4afa2a490da
    .quad 0x3ffefa1bee615a27
    .quad 0x3fff50765b6e4540
    .quad 0x3fffa7c1819e90d8



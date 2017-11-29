
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
#define fname FN_PROTOTYPE(exp2)
#define fname_special _exp2_special@PLT
#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

.text
.p2align 4
.globl fname
.type fname,@function
fname:
    ucomisd      .L__max_exp2_arg(%rip), %xmm0
    ja           .L__y_is_inf
    jp           .L__y_is_nan
    ucomisd      .L__min_exp2_arg(%rip), %xmm0
    jbe          .L__y_is_zero

    # x * (64)
    movapd      %xmm0,%xmm2        
    mulsd       .L__real_64(%rip), %xmm2

    # n = int( x * (64))
    cvttpd2dq    %xmm2, %xmm1   #xmm1 = (int)n
    cvtdq2pd    %xmm1, %xmm2   #xmm2 = (double)n
    movd        %xmm1, %ecx

    # r = x - n * 1/64  
    #r *= ln2;      
    mulsd    .L__one_by_64(%rip),%xmm2
    addsd    %xmm0,%xmm2   #xmm2 = r    
    mulsd    .L__ln_2(%rip),%xmm2    
        
    #j = n & 0x3f    
    mov         $0x3f, %rax
    and         %ecx, %eax     #eax = j
    # m = (n - j) / 64      
    sar         $6, %ecx       #ecx = m

    # q = r + r^2*1/2 + r^3*1/6 + r^4 *1/24 + r^5*1/120 + r^6*1/720
    # q = r + r*r*(1/2 + r*(1/6+ r*(1/24 + r*(1/120 + r*(1/720)))))
    movapd       .L__real_1_by_720(%rip), %xmm3  #xmm3 = 1/720
    mulsd       %xmm2, %xmm3    #xmm3 = r*1/720
    movapd       .L__real_1_by_6(%rip), %xmm0    #xmm0 = 1/6    
    movapd      %xmm2, %xmm1 #xmm1 = r            
    mulsd       %xmm2, %xmm0    #xmm0 = r*1/6
    addsd       .L__real_1_by_120(%rip), %xmm3  #xmm3 = 1/120 + (r*1/720)
    mulsd       %xmm2, %xmm1    #xmm1 = r*r    
    addsd       .L__real_1_by_2(%rip), %xmm0  #xmm0 = 1/2 + (r*1/6)        
    movapd       %xmm1, %xmm4   #xmm4 = r*r
    mulsd       %xmm1, %xmm4    #xmm4 = (r*r) * (r*r)    
    mulsd       %xmm2, %xmm3    #xmm3 = r * (1/120 + (r*1/720))
    mulsd       %xmm1, %xmm0    #xmm0 = (r*r)*(1/2 + (r*1/6))
    addsd       .L__real_1_by_24(%rip), %xmm3  #xmm3 = 1/24 + (r * (1/120 + (r*1/720)))
    addsd       %xmm2, %xmm0   #xmm0 = r + ((r*r)*(1/2 + (r*1/6)))
    mulsd       %xmm4, %xmm3   #xmm3 = ((r*r) * (r*r)) * (1/24 + (r * (1/120 + (r*1/720))))
    addsd       %xmm3, %xmm0   #xmm0 = r + ((r*r)*(1/2 + (r*1/6))) + ((r*r) * (r*r)) * (1/24 + (r * (1/120 + (r*1/720))))
    
    # (f)*(q) + f2 + f1
    cmp         $0xfffffc02, %ecx # -1022    
    lea         .L__two_to_jby64_table(%rip), %rdx        
    lea         .L__two_to_jby64_tail_table(%rip), %r11       
    lea         .L__two_to_jby64_head_table(%rip), %r10      
    mulsd       (%rdx,%rax,8), %xmm0
    addsd       (%r11,%rax,8), %xmm0
    addsd       (%r10,%rax,8), %xmm0        

    jle         .L__process_denormal 
.L__process_normal:
    shl         $52, %rcx    
    movd        %rcx,%xmm2
    paddq       %xmm2, %xmm0
    ret

.p2align 4
.L__process_denormal:
    jl          .L__process_true_denormal
    ucomisd     .L__real_one(%rip), %xmm0
    jae         .L__process_normal
.L__process_true_denormal:
    # here ( e^r < 1 and m = -1022 ) or m <= -1023
    add         $1074, %ecx
    mov         $1, %rax    
    shl         %cl, %rax
    movd         %rax, %xmm2
    mulsd       %xmm2, %xmm0
    ret        
    
.p2align 4
.L__y_is_inf:
    mov         $0x7ff0000000000000,%rax
    movd       %rax, %xmm1
    mov         $3, %edi
    #call        fname_special
    movdqa     %xmm1,%xmm0 #remove this if call is made    
    ret     

.p2align 4
.L__y_is_nan:
    movapd      %xmm0,%xmm1
    addsd       %xmm0,%xmm1
    mov         $1, %edi
    #call        fname_special
    movdqa     %xmm1,%xmm0 #remove this if call is made    
    ret

.p2align 4
.L__y_is_zero:
    pxor        %xmm1,%xmm1
    mov         $2, %edi
    #call        fname_special
    movdqa     %xmm1,%xmm0 #remove this if call is made
    ret      
    
.data
.align 16
.L__max_exp2_arg:            .quad 0x4090000000000000
.L__min_exp2_arg:            .quad 0xc090c80000000000
.L__real_64:                 .quad 0x4050000000000000    # 64
.L__ln_2:                    .quad 0x3FE62E42FEFA39EF
.L__one_by_64:               .quad 0xbF90000000000000

.align 16
.L__real_1_by_720:              .quad 0x3f56c16c16c16c17    # 1/720
.L__real_1_by_120:              .quad 0x3f81111111111111    # 1/120
.L__real_1_by_6:                .quad 0x3fc5555555555555    # 1/6
.L__real_1_by_2:                .quad 0x3fe0000000000000    # 1/2
.L__real_1_by_24:               .quad 0x3fa5555555555555    # 1/24
.L__real_one:                   .quad 0x3ff0000000000000

.align 16
.L__two_to_jby64_table:
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

.align 16
.L__two_to_jby64_head_table:
    .quad 0x3ff0000000000000
    .quad 0x3ff02c9a30000000
    .quad 0x3ff059b0d0000000
    .quad 0x3ff0874510000000
    .quad 0x3ff0b55860000000
    .quad 0x3ff0e3ec30000000
    .quad 0x3ff11301d0000000
    .quad 0x3ff1429aa0000000
    .quad 0x3ff172b830000000
    .quad 0x3ff1a35be0000000
    .quad 0x3ff1d48730000000
    .quad 0x3ff2063b80000000
    .quad 0x3ff2387a60000000
    .quad 0x3ff26b4560000000
    .quad 0x3ff29e9df0000000
    .quad 0x3ff2d285a0000000
    .quad 0x3ff306fe00000000
    .quad 0x3ff33c08b0000000
    .quad 0x3ff371a730000000
    .quad 0x3ff3a7db30000000
    .quad 0x3ff3dea640000000
    .quad 0x3ff4160a20000000
    .quad 0x3ff44e0860000000
    .quad 0x3ff486a2b0000000
    .quad 0x3ff4bfdad0000000
    .quad 0x3ff4f9b270000000
    .quad 0x3ff5342b50000000
    .quad 0x3ff56f4730000000
    .quad 0x3ff5ab07d0000000
    .quad 0x3ff5e76f10000000
    .quad 0x3ff6247eb0000000
    .quad 0x3ff6623880000000
    .quad 0x3ff6a09e60000000
    .quad 0x3ff6dfb230000000
    .quad 0x3ff71f75e0000000
    .quad 0x3ff75feb50000000
    .quad 0x3ff7a11470000000
    .quad 0x3ff7e2f330000000
    .quad 0x3ff8258990000000
    .quad 0x3ff868d990000000
    .quad 0x3ff8ace540000000
    .quad 0x3ff8f1ae90000000
    .quad 0x3ff93737b0000000
    .quad 0x3ff97d8290000000
    .quad 0x3ff9c49180000000
    .quad 0x3ffa0c6670000000
    .quad 0x3ffa5503b0000000
    .quad 0x3ffa9e6b50000000
    .quad 0x3ffae89f90000000
    .quad 0x3ffb33a2b0000000
    .quad 0x3ffb7f76f0000000
    .quad 0x3ffbcc1e90000000
    .quad 0x3ffc199bd0000000
    .quad 0x3ffc67f120000000
    .quad 0x3ffcb720d0000000
    .quad 0x3ffd072d40000000
    .quad 0x3ffd5818d0000000
    .quad 0x3ffda9e600000000
    .quad 0x3ffdfc9730000000
    .quad 0x3ffe502ee0000000
    .quad 0x3ffea4afa0000000
    .quad 0x3ffefa1be0000000
    .quad 0x3fff507650000000
    .quad 0x3fffa7c180000000

.align 16
.L__two_to_jby64_tail_table:
    .quad 0x0000000000000000
    .quad 0x3e6cef00c1dcdef9
    .quad 0x3e48ac2ba1d73e2a
    .quad 0x3e60eb37901186be
    .quad 0x3e69f3121ec53172
    .quad 0x3e469e8d10103a17
    .quad 0x3df25b50a4ebbf1a
    .quad 0x3e6d525bbf668203
    .quad 0x3e68faa2f5b9bef9
    .quad 0x3e66df96ea796d31
    .quad 0x3e368b9aa7805b80
    .quad 0x3e60c519ac771dd6
    .quad 0x3e6ceac470cd83f5
    .quad 0x3e5789f37495e99c
    .quad 0x3e547f7b84b09745
    .quad 0x3e5b900c2d002475
    .quad 0x3e64636e2a5bd1ab
    .quad 0x3e4320b7fa64e430
    .quad 0x3e5ceaa72a9c5154
    .quad 0x3e53967fdba86f24
    .quad 0x3e682468446b6824
    .quad 0x3e3f72e29f84325b
    .quad 0x3e18624b40c4dbd0
    .quad 0x3e5704f3404f068e
    .quad 0x3e54d8a89c750e5e
    .quad 0x3e5a74b29ab4cf62
    .quad 0x3e5a753e077c2a0f
    .quad 0x3e5ad49f699bb2c0
    .quad 0x3e6a90a852b19260
    .quad 0x3e56b48521ba6f93
    .quad 0x3e0d2ac258f87d03
    .quad 0x3e42a91124893ecf
    .quad 0x3e59fcef32422cbe
    .quad 0x3e68ca345de441c5
    .quad 0x3e61d8bee7ba46e1
    .quad 0x3e59099f22fdba6a
    .quad 0x3e4f580c36bea881
    .quad 0x3e5b3d398841740a
    .quad 0x3e62999c25159f11
    .quad 0x3e668925d901c83b
    .quad 0x3e415506dadd3e2a
    .quad 0x3e622aee6c57304e
    .quad 0x3e29b8bc9e8a0387
    .quad 0x3e6fbc9c9f173d24
    .quad 0x3e451f8480e3e235
    .quad 0x3e66bbcac96535b5
    .quad 0x3e41f12ae45a1224
    .quad 0x3e55e7f6fd0fac90
    .quad 0x3e62b5a75abd0e69
    .quad 0x3e609e2bf5ed7fa1
    .quad 0x3e47daf237553d84
    .quad 0x3e12f074891ee83d
    .quad 0x3e6b0aa538444196
    .quad 0x3e6cafa29694426f
    .quad 0x3e69df20d22a0797
    .quad 0x3e640f12f71a1e45
    .quad 0x3e69f7490e4bb40b
    .quad 0x3e4ed9942b84600d
    .quad 0x3e4bdcdaf5cb4656
    .quad 0x3e5e2cffd89cf44c
    .quad 0x3e452486cc2c7b9d
    .quad 0x3e6cc2b44eee3fa4
    .quad 0x3e66dc8a80ce9f09
    .quad 0x3e39e90d82e90a7e



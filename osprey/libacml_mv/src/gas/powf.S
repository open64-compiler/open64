
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


# powf.S
#
# An implementation of the powf libm function.
#
# Prototype:
#
#     float powf(float x, float y);
#

#
#   Algorithm:
#       x^y = e^(y*ln(x))
#
#       Look in exp, log for the respective algorithms
#

#include "fn_macros.h"
#define fname FN_PROTOTYPE(powf)
#define fname_special _powf_special@PLT


# local variable storage offsets
.equ    save_x, 0x0
.equ    save_y, 0x10
.equ    p_temp_exp, 0x20
.equ    negate_result, 0x30
.equ    save_ax, 0x40
.equ    y_head, 0x50
.equ    p_temp_log, 0x60
.equ    stack_size, 0x78


#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

.text
.align 16
.p2align 4,,15
.globl fname
.type fname,@function
fname:

    sub         $stack_size, %rsp

    movss       %xmm0, save_x(%rsp)
    movss       %xmm1, save_y(%rsp)

    mov         save_x(%rsp), %edx
    mov         save_y(%rsp), %r8d

    mov         .L__f32_exp_mant_mask(%rip), %r10d
    and         %r8d, %r10d
    jz          .L__y_is_zero

    cmp         .L__f32_pos_one(%rip), %r8d
    je          .L__y_is_one

    mov         .L__f32_sign_mask(%rip), %r9d
    and         %edx, %r9d
    cmp         .L__f32_sign_mask(%rip), %r9d
    mov         .L__f32_pos_zero(%rip), %eax
    mov         %eax, negate_result(%rsp)    
    je          .L__x_is_neg

    cmp         .L__f32_pos_one(%rip), %edx
    je          .L__x_is_pos_one

    cmp         .L__f32_pos_zero(%rip), %edx
    je          .L__x_is_zero

    mov         .L__f32_exp_mask(%rip), %r9d
    and         %edx, %r9d
    cmp         .L__f32_exp_mask(%rip), %r9d
    je          .L__x_is_inf_or_nan
   
    mov         .L__f32_exp_mask(%rip), %r10d
    and         %r8d, %r10d
    cmp         .L__f32_ay_max_bound(%rip), %r10d
    jg          .L__ay_is_very_large

    mov         .L__f32_exp_mask(%rip), %r10d
    and         %r8d, %r10d
    cmp         .L__f32_ay_min_bound(%rip), %r10d
    jl          .L__ay_is_very_small

    # -----------------------------
    # compute log(x) here
    # -----------------------------
.L__log_x:

    movss       save_y(%rsp), %xmm7
    cvtss2sd    %xmm0, %xmm0
    cvtss2sd    %xmm7, %xmm7
    movsd       %xmm7, save_y(%rsp)

    # compute exponent part
    xor         %r8, %r8
    movdqa      %xmm0, %xmm3
    psrlq       $52, %xmm3
    movd        %xmm0, %r8
    psubq       .L__mask_1023(%rip), %xmm3
    movdqa      %xmm0, %xmm2
    cvtdq2pd    %xmm3, %xmm6 # xexp
    pand        .L__real_mant(%rip), %xmm2

    # compute index into the log tables
    mov         %r8, %r9
    and         .L__mask_mant_all7(%rip), %r8
    and         .L__mask_mant8(%rip), %r9
    shl         %r9
    add         %r9, %r8
    mov         %r8, p_temp_log(%rsp)

    # F, Y
    movsd       p_temp_log(%rsp), %xmm1
    shr         $45, %r8
    por         .L__real_half(%rip), %xmm2
    por         .L__real_half(%rip), %xmm1
    lea         .L__log_F_inv(%rip), %r9

    # f = F - Y, r = f * inv
    subsd       %xmm2, %xmm1
    mulsd       (%r9,%r8,8), %xmm1
    movsd       %xmm1, %xmm2

    lea         .L__log_128_table(%rip), %r9
    movsd       .L__real_log2(%rip), %xmm5
    movsd       (%r9,%r8,8), %xmm0

    # poly
    mulsd       %xmm2, %xmm1
    movsd       .L__real_1_over_4(%rip), %xmm4
    movsd       .L__real_1_over_2(%rip), %xmm3
    mulsd       %xmm2, %xmm4
    mulsd       %xmm2, %xmm3
    mulsd       %xmm2, %xmm1
    addsd       .L__real_1_over_3(%rip), %xmm4
    addsd       .L__real_1_over_1(%rip), %xmm3
    mulsd       %xmm1, %xmm4
    mulsd       %xmm2, %xmm3
    addsd       %xmm4, %xmm3  

    mulsd       %xmm6, %xmm5
    subsd       %xmm3, %xmm0
    addsd       %xmm5, %xmm0

    movsd       save_y(%rsp), %xmm7
    mulsd       %xmm7, %xmm0
 
    # v = y * ln(x)
    # xmm0 - v

    # -----------------------------
    # compute exp( y * ln(x) ) here
    # -----------------------------

    # x * (32/ln(2))
    movsd       .L__real_32_by_log2(%rip), %xmm7
    movsd       %xmm0, p_temp_exp(%rsp)
    mulsd       %xmm0, %xmm7
    mov         p_temp_exp(%rsp), %rdx

    # v < 128*ln(2), ( v * (32/ln(2)) ) < 32*128
    # v >= -150*ln(2), ( v * (32/ln(2)) ) >= 32*(-150)
    comisd      .L__real_p4096(%rip), %xmm7
    jae         .L__process_result_inf

    comisd      .L__real_m4768(%rip), %xmm7
    jb          .L__process_result_zero

    # n = int( v * (32/ln(2)) )
    cvtpd2dq    %xmm7, %xmm4
    lea         .L__two_to_jby32_table(%rip), %r10
    cvtdq2pd    %xmm4, %xmm1

    # r = x - n * ln(2)/32
    movsd       .L__real_log2_by_32(%rip), %xmm2
    mulsd       %xmm1, %xmm2
    movd        %xmm4, %ecx
    mov         $0x1f, %rax
    and         %ecx, %eax
    subsd       %xmm2, %xmm0
    movsd       %xmm0, %xmm1

    # m = (n - j) / 32
    sub         %eax, %ecx
    sar         $5, %ecx

    # q
    mulsd       %xmm0, %xmm1
    movsd       .L__real_1_by_24(%rip), %xmm4
    movsd       .L__real_1_by_2(%rip), %xmm3
    mulsd       %xmm0, %xmm4
    mulsd       %xmm0, %xmm3
    mulsd       %xmm0, %xmm1
    addsd       .L__real_1_by_6(%rip), %xmm4
    addsd       .L__real_1_by_1(%rip), %xmm3
    mulsd       %xmm1, %xmm4
    mulsd       %xmm0, %xmm3
    addsd       %xmm4, %xmm3  
    movsd       %xmm3, %xmm0

    add         $1023, %rcx
    shl         $52, %rcx

    # (f)*(1+q)
    movsd       (%r10,%rax,8), %xmm1
    mulsd       %xmm1, %xmm0
    addsd       %xmm1, %xmm0

    mov         %rcx, p_temp_exp(%rsp)
    mulsd       p_temp_exp(%rsp), %xmm0
    cvtsd2ss    %xmm0, %xmm0
    orps        negate_result(%rsp), %xmm0

.L__final_check:
    add         $stack_size, %rsp
    ret

.p2align 4,,15
.L__process_result_zero:
    mov         .L__f32_real_zero(%rip), %r11d
    or          negate_result(%rsp), %r11d
    jmp         .L__z_is_zero_or_inf
 
.p2align 4,,15
.L__process_result_inf:
    mov         .L__f32_real_inf(%rip), %r11d
    or          negate_result(%rsp), %r11d
    jmp         .L__z_is_zero_or_inf


.p2align 4,,15
.L__x_is_neg:

    mov         .L__f32_exp_mask(%rip), %r10d
    and         %r8d, %r10d
    cmp         .L__f32_ay_max_bound(%rip), %r10d
    jg          .L__ay_is_very_large

    # determine if y is an integer
    mov         .L__f32_exp_mant_mask(%rip), %r10d
    and         %r8d, %r10d
    mov         %r10d, %r11d
    mov         .L__f32_exp_shift(%rip), %ecx
    shr         %cl, %r10d
    sub         .L__f32_exp_bias(%rip), %r10d
    js          .L__x_is_neg_y_is_not_int
   
    mov         .L__f32_exp_mant_mask(%rip), %eax
    and         %edx, %eax
    mov         %eax, save_ax(%rsp)

    cmp         .L__yexp_24(%rip), %r10d
    mov         %r10d, %ecx
    jg          .L__continue_after_y_int_check

    mov         .L__f32_mant_full(%rip), %r9d
    shr         %cl, %r9d
    and         %r11d, %r9d
    jnz         .L__x_is_neg_y_is_not_int

    mov         .L__f32_1_before_mant(%rip), %r9d
    shr         %cl, %r9d
    and         %r11d, %r9d
    jz          .L__continue_after_y_int_check

    mov         .L__f32_sign_mask(%rip), %eax
    mov         %eax, negate_result(%rsp)    

.L__continue_after_y_int_check:

    cmp         .L__f32_neg_zero(%rip), %edx
    je          .L__x_is_zero

    cmp         .L__f32_neg_one(%rip), %edx
    je          .L__x_is_neg_one

    mov         .L__f32_exp_mask(%rip), %r9d
    and         %edx, %r9d
    cmp         .L__f32_exp_mask(%rip), %r9d
    je          .L__x_is_inf_or_nan
   
    movss       save_ax(%rsp), %xmm0
    jmp         .L__log_x

.p2align 4,,15
.L__x_is_pos_one:
    xor         %eax, %eax
    mov         .L__f32_exp_mask(%rip), %r10d
    and         %r8d, %r10d
    cmp         .L__f32_exp_mask(%rip), %r10d
    cmove       %r8d, %eax
    mov         .L__f32_mant_mask(%rip), %r10d
    and         %eax, %r10d
    jz          .L__final_check

    mov         .L__f32_qnan_set(%rip), %r10d
    and         %r8d, %r10d
    jnz         .L__final_check

    movss       save_x(%rsp), %xmm0
    movss       save_y(%rsp), %xmm1
    movss       .L__f32_pos_one(%rip), %xmm2
    mov         .L__flag_x_one_y_snan(%rip), %edi

    call        fname_special
    jmp         .L__final_check      

.p2align 4,,15
.L__y_is_zero:

    xor         %eax, %eax
    mov         .L__f32_exp_mask(%rip), %r9d
    mov         .L__f32_pos_one(%rip), %r11d
    and         %edx, %r9d
    cmp         .L__f32_exp_mask(%rip), %r9d
    cmove       %edx, %eax
    mov         .L__f32_mant_mask(%rip), %r9d
    and         %eax, %r9d
    jnz         .L__x_is_nan

    movss       .L__f32_pos_one(%rip), %xmm0
    jmp         .L__final_check

.p2align 4,,15
.L__y_is_one:
    xor         %eax, %eax
    mov         %edx, %r11d
    mov         .L__f32_exp_mask(%rip), %r9d
    or          .L__f32_qnan_set(%rip), %r11d
    and         %edx, %r9d
    cmp         .L__f32_exp_mask(%rip), %r9d
    cmove       %edx, %eax
    mov         .L__f32_mant_mask(%rip), %r9d
    and         %eax, %r9d
    jnz         .L__x_is_nan

    movd        %edx, %xmm0 
    jmp         .L__final_check

.p2align 4,,15
.L__x_is_neg_one:
    mov         .L__f32_pos_one(%rip), %edx
    or          negate_result(%rsp), %edx
    xor         %eax, %eax
    mov         %r8d, %r11d
    mov         .L__f32_exp_mask(%rip), %r10d
    or          .L__f32_qnan_set(%rip), %r11d
    and         %r8d, %r10d
    cmp         .L__f32_exp_mask(%rip), %r10d
    cmove       %r8d, %eax
    mov         .L__f32_mant_mask(%rip), %r10d
    and         %eax, %r10d
    jnz         .L__y_is_nan

    movd        %edx, %xmm0
    jmp         .L__final_check

.p2align 4,,15
.L__x_is_neg_y_is_not_int:
    mov         .L__f32_exp_mask(%rip), %r9d
    and         %edx, %r9d
    cmp         .L__f32_exp_mask(%rip), %r9d
    je          .L__x_is_inf_or_nan

    cmp         .L__f32_neg_zero(%rip), %edx
    je          .L__x_is_zero

    movss       save_x(%rsp), %xmm0
    movss       save_y(%rsp), %xmm1
    movss       .L__f32_qnan(%rip), %xmm2
    mov         .L__flag_x_neg_y_notint(%rip), %edi

    call        fname_special
    jmp         .L__final_check

.p2align 4,,15
.L__ay_is_very_large:
    mov         .L__f32_exp_mask(%rip), %r9d
    and         %edx, %r9d
    cmp         .L__f32_exp_mask(%rip), %r9d
    je          .L__x_is_inf_or_nan

    mov         .L__f32_exp_mant_mask(%rip), %r9d
    and         %edx, %r9d
    jz          .L__x_is_zero 

    cmp         .L__f32_neg_one(%rip), %edx
    je          .L__x_is_neg_one

    mov         %edx, %r9d
    and         .L__f32_exp_mant_mask(%rip), %r9d
    cmp         .L__f32_pos_one(%rip), %r9d
    jl          .L__ax_lt1_y_is_large_or_inf_or_nan
  
    jmp         .L__ax_gt1_y_is_large_or_inf_or_nan

.p2align 4,,15
.L__x_is_zero:
    mov         .L__f32_exp_mask(%rip), %r10d
    xor         %eax, %eax
    and         %r8d, %r10d
    cmp         .L__f32_exp_mask(%rip), %r10d
    je          .L__x_is_zero_y_is_inf_or_nan

    mov         .L__f32_sign_mask(%rip), %r10d
    and         %r8d, %r10d
    cmovnz      .L__f32_pos_inf(%rip), %eax
    jnz         .L__x_is_zero_z_is_inf

    movd        %eax, %xmm0
    orps        negate_result(%rsp), %xmm0
    jmp         .L__final_check

.p2align 4,,15
.L__x_is_zero_z_is_inf:

    movss       save_x(%rsp), %xmm0
    movss       save_y(%rsp), %xmm1
    movd        %eax, %xmm2
    orps        negate_result(%rsp), %xmm2
    mov         .L__flag_x_zero_z_inf(%rip), %edi

    call        fname_special
    jmp         .L__final_check    

.p2align 4,,15
.L__x_is_zero_y_is_inf_or_nan:
    mov         %r8d, %r11d
    cmp         .L__f32_neg_inf(%rip), %r8d
    cmove       .L__f32_pos_inf(%rip), %eax
    je          .L__x_is_zero_z_is_inf

    or          .L__f32_qnan_set(%rip), %r11d
    mov         .L__f32_mant_mask(%rip), %r10d
    and         %r8d, %r10d
    jnz         .L__y_is_nan

    movd        %eax, %xmm0
    jmp         .L__final_check

.p2align 4,,15
.L__x_is_inf_or_nan:
    xor         %r11d, %r11d
    mov         .L__f32_sign_mask(%rip), %r10d
    and         %r8d, %r10d
    cmovz       .L__f32_pos_inf(%rip), %r11d
    mov         %edx, %eax
    mov         .L__f32_mant_mask(%rip), %r9d
    or          .L__f32_qnan_set(%rip), %eax
    and         %edx, %r9d
    cmovnz      %eax, %r11d
    jnz         .L__x_is_nan

    xor         %eax, %eax
    mov         %r8d, %r9d
    mov         .L__f32_exp_mask(%rip), %r10d
    or          .L__f32_qnan_set(%rip), %r9d
    and         %r8d, %r10d
    cmp         .L__f32_exp_mask(%rip), %r10d
    cmove       %r8d, %eax
    mov         .L__f32_mant_mask(%rip), %r10d
    and         %eax, %r10d
    cmovnz      %r9d, %r11d
    jnz         .L__y_is_nan

    movd        %r11d, %xmm0
    orps        negate_result(%rsp), %xmm0
    jmp         .L__final_check

.p2align 4,,15
.L__ay_is_very_small:
    movss       .L__f32_pos_one(%rip), %xmm0
    addss       %xmm1, %xmm0
    jmp         .L__final_check


.p2align 4,,15
.L__ax_lt1_y_is_large_or_inf_or_nan:
    xor         %r11d, %r11d
    mov         .L__f32_sign_mask(%rip), %r10d
    and         %r8d, %r10d
    cmovnz      .L__f32_pos_inf(%rip), %r11d
    jmp         .L__adjust_for_nan

.p2align 4,,15
.L__ax_gt1_y_is_large_or_inf_or_nan:
    xor         %r11d, %r11d
    mov         .L__f32_sign_mask(%rip), %r10d
    and         %r8d, %r10d
    cmovz       .L__f32_pos_inf(%rip), %r11d

.p2align 4,,15
.L__adjust_for_nan:

    xor         %eax, %eax
    mov         %r8d, %r9d
    mov         .L__f32_exp_mask(%rip), %r10d
    or          .L__f32_qnan_set(%rip), %r9d
    and         %r8d, %r10d
    cmp         .L__f32_exp_mask(%rip), %r10d
    cmove       %r8d, %eax
    mov         .L__f32_mant_mask(%rip), %r10d
    and         %eax, %r10d
    cmovnz      %r9d, %r11d
    jnz         .L__y_is_nan

    test        %eax, %eax
    jnz         .L__y_is_inf

.p2align 4,,15
.L__z_is_zero_or_inf:

    mov         .L__flag_z_zero(%rip), %edi
    test        %r11d, %r11d
    cmovnz      .L__flag_z_inf(%rip), %edi
    
    movss       save_x(%rsp), %xmm0
    movss       save_y(%rsp), %xmm1
    movd        %r11d, %xmm2

    call        fname_special
    jmp         .L__final_check

.p2align 4,,15
.L__y_is_inf:

    movd        %r11d, %xmm0
    jmp         .L__final_check

.p2align 4,,15
.L__x_is_nan:

    xor         %eax, %eax
    mov         .L__f32_exp_mask(%rip), %r10d
    and         %r8d, %r10d
    cmp         .L__f32_exp_mask(%rip), %r10d
    cmove       %r8d, %eax
    mov         .L__f32_mant_mask(%rip), %r10d
    and         %eax, %r10d
    jnz         .L__x_is_nan_y_is_nan

    mov         .L__f32_qnan_set(%rip), %r9d
    and         %edx, %r9d
    movd        %r11d, %xmm0
    jnz         .L__final_check

    movss       save_x(%rsp), %xmm0
    movss       save_y(%rsp), %xmm1
    movd        %r11d, %xmm2
    mov         .L__flag_x_nan(%rip), %edi

    call        fname_special
    jmp         .L__final_check

.p2align 4,,15
.L__y_is_nan:

    mov         .L__f32_qnan_set(%rip), %r10d
    and         %r8d, %r10d
    movd        %r11d, %xmm0
    jnz         .L__final_check

    movss       save_x(%rsp), %xmm0
    movss       save_y(%rsp), %xmm1
    movd        %r11d, %xmm2
    mov         .L__flag_y_nan(%rip), %edi

    call        fname_special
    jmp         .L__final_check

.p2align 4,,15
.L__x_is_nan_y_is_nan:

    mov         .L__f32_qnan_set(%rip), %r9d
    and         %edx, %r9d
    jz          .L__continue_xy_nan

    mov         .L__f32_qnan_set(%rip), %r10d
    and         %r8d, %r10d
    jz          .L__continue_xy_nan

    movd        %r11d, %xmm0
    jmp         .L__final_check

.L__continue_xy_nan:    
    movss       save_x(%rsp), %xmm0
    movss       save_y(%rsp), %xmm1
    movd        %r11d, %xmm2
    mov         .L__flag_x_nan_y_nan(%rip), %edi

    call        fname_special
    jmp         .L__final_check  

.data

.align 16

# these codes and the ones in the corresponding .c file have to match
.L__flag_x_one_y_snan:          .long 1
.L__flag_x_zero_z_inf:          .long 2
.L__flag_x_nan:                 .long 3
.L__flag_y_nan:                 .long 4
.L__flag_x_nan_y_nan:           .long 5
.L__flag_x_neg_y_notint:        .long 6
.L__flag_z_zero:                .long 7
.L__flag_z_denormal:            .long 8
.L__flag_z_inf:                 .long 9

.align 16

.L__f32_ay_max_bound:           .long 0x4f000000
.L__f32_ay_min_bound:           .long 0x2e800000
.L__f32_sign_mask:              .long 0x80000000
.L__f32_sign_and_exp_mask:      .long 0x0ff800000
.L__f32_exp_mask:               .long 0x7f800000
.L__f32_neg_inf:                .long 0x0ff800000
.L__f32_pos_inf:                .long 0x7f800000
.L__f32_pos_one:                .long 0x3f800000
.L__f32_pos_zero:               .long 0x00000000
.L__f32_exp_mant_mask:          .long 0x7fffffff
.L__f32_mant_mask:              .long 0x007fffff

.L__f32_neg_qnan:               .long 0x0ffc00000
.L__f32_qnan:                   .long 0x7fc00000
.L__f32_qnan_set:               .long 0x00400000

.L__f32_neg_one:                .long 0x0bf800000
.L__f32_neg_zero:               .long 0x80000000

.L__f32_real_one:               .long 0x3f800000
.L__f32_real_zero:              .long 0x00000000
.L__f32_real_inf:               .long 0x7f800000

.L__yexp_24:                    .long 0x00000018

.L__f32_exp_shift:              .long 0x00000017
.L__f32_exp_bias:               .long 0x0000007f
.L__f32_mant_full:              .long 0x007fffff
.L__f32_1_before_mant:          .long 0x00800000

.align 16

.L__mask_mant_all7:         .quad 0x000fe00000000000
.L__mask_mant8:             .quad 0x0000100000000000

#---------------------
# log data
#---------------------

.align 16

.L__real_ninf:  .quad 0x0fff0000000000000   # -inf
                .quad 0x0000000000000000
.L__real_inf:   .quad 0x7ff0000000000000    # +inf
                .quad 0x0000000000000000
.L__real_nan:   .quad 0x7ff8000000000000    # NaN
                .quad 0x0000000000000000
.L__real_mant:  .quad 0x000FFFFFFFFFFFFF    # mantissa bits
                .quad 0x0000000000000000
.L__mask_1023:  .quad 0x00000000000003ff
                .quad 0x0000000000000000


.L__real_log2:      .quad 0x3fe62e42fefa39ef
                    .quad 0x0000000000000000

.L__real_two:       .quad 0x4000000000000000 # 2
                    .quad 0x0000000000000000

.L__real_one:       .quad 0x3ff0000000000000 # 1
                    .quad 0x0000000000000000

.L__real_half:      .quad 0x3fe0000000000000 # 1/2
                    .quad 0x0000000000000000

.L__real_1_over_1:  .quad 0x3ff0000000000000
                    .quad 0x0000000000000000
.L__real_1_over_2:  .quad 0x3fe0000000000000
                    .quad 0x0000000000000000
.L__real_1_over_3:  .quad 0x3fd5555555555555
                    .quad 0x0000000000000000
.L__real_1_over_4:  .quad 0x3fd0000000000000
                    .quad 0x0000000000000000


.align 16
.L__log_128_table:
                    .quad 0x0000000000000000
                    .quad 0x3f7fe02a6b106789
                    .quad 0x3f8fc0a8b0fc03e4
                    .quad 0x3f97b91b07d5b11b
                    .quad 0x3f9f829b0e783300
                    .quad 0x3fa39e87b9febd60
                    .quad 0x3fa77458f632dcfc
                    .quad 0x3fab42dd711971bf
                    .quad 0x3faf0a30c01162a6
                    .quad 0x3fb16536eea37ae1
                    .quad 0x3fb341d7961bd1d1
                    .quad 0x3fb51b073f06183f
                    .quad 0x3fb6f0d28ae56b4c
                    .quad 0x3fb8c345d6319b21
                    .quad 0x3fba926d3a4ad563
                    .quad 0x3fbc5e548f5bc743
                    .quad 0x3fbe27076e2af2e6
                    .quad 0x3fbfec9131dbeabb
                    .quad 0x3fc0d77e7cd08e59
                    .quad 0x3fc1b72ad52f67a0
                    .quad 0x3fc29552f81ff523
                    .quad 0x3fc371fc201e8f74
                    .quad 0x3fc44d2b6ccb7d1e
                    .quad 0x3fc526e5e3a1b438
                    .quad 0x3fc5ff3070a793d4
                    .quad 0x3fc6d60fe719d21d
                    .quad 0x3fc7ab890210d909
                    .quad 0x3fc87fa06520c911
                    .quad 0x3fc9525a9cf456b4
                    .quad 0x3fca23bc1fe2b563
                    .quad 0x3fcaf3c94e80bff3
                    .quad 0x3fcbc286742d8cd6
                    .quad 0x3fcc8ff7c79a9a22
                    .quad 0x3fcd5c216b4fbb91
                    .quad 0x3fce27076e2af2e6
                    .quad 0x3fcef0adcbdc5936
                    .quad 0x3fcfb9186d5e3e2b
                    .quad 0x3fd0402594b4d041
                    .quad 0x3fd0a324e27390e3
                    .quad 0x3fd1058bf9ae4ad5
                    .quad 0x3fd1675cababa60e
                    .quad 0x3fd1c898c16999fb
                    .quad 0x3fd22941fbcf7966
                    .quad 0x3fd2895a13de86a3
                    .quad 0x3fd2e8e2bae11d31
                    .quad 0x3fd347dd9a987d55
                    .quad 0x3fd3a64c556945ea
                    .quad 0x3fd404308686a7e4
                    .quad 0x3fd4618bc21c5ec2
                    .quad 0x3fd4be5f957778a1
                    .quad 0x3fd51aad872df82d
                    .quad 0x3fd5767717455a6c
                    .quad 0x3fd5d1bdbf5809ca
                    .quad 0x3fd62c82f2b9c795
                    .quad 0x3fd686c81e9b14af
                    .quad 0x3fd6e08eaa2ba1e4
                    .quad 0x3fd739d7f6bbd007
                    .quad 0x3fd792a55fdd47a2
                    .quad 0x3fd7eaf83b82afc3
                    .quad 0x3fd842d1da1e8b17
                    .quad 0x3fd89a3386c1425b
                    .quad 0x3fd8f11e873662c8
                    .quad 0x3fd947941c2116fb
                    .quad 0x3fd99d958117e08b
                    .quad 0x3fd9f323ecbf984c
                    .quad 0x3fda484090e5bb0a
                    .quad 0x3fda9cec9a9a084a
                    .quad 0x3fdaf1293247786b
                    .quad 0x3fdb44f77bcc8f63
                    .quad 0x3fdb9858969310fb
                    .quad 0x3fdbeb4d9da71b7c
                    .quad 0x3fdc3dd7a7cdad4d
                    .quad 0x3fdc8ff7c79a9a22
                    .quad 0x3fdce1af0b85f3eb
                    .quad 0x3fdd32fe7e00ebd5
                    .quad 0x3fdd83e7258a2f3e
                    .quad 0x3fddd46a04c1c4a1
                    .quad 0x3fde24881a7c6c26
                    .quad 0x3fde744261d68788
                    .quad 0x3fdec399d2468cc0
                    .quad 0x3fdf128f5faf06ed
                    .quad 0x3fdf6123fa7028ac
                    .quad 0x3fdfaf588f78f31f
                    .quad 0x3fdffd2e0857f498
                    .quad 0x3fe02552a5a5d0ff
                    .quad 0x3fe04bdf9da926d2
                    .quad 0x3fe0723e5c1cdf40
                    .quad 0x3fe0986f4f573521
                    .quad 0x3fe0be72e4252a83
                    .quad 0x3fe0e44985d1cc8c
                    .quad 0x3fe109f39e2d4c97
                    .quad 0x3fe12f719593efbc
                    .quad 0x3fe154c3d2f4d5ea
                    .quad 0x3fe179eabbd899a1
                    .quad 0x3fe19ee6b467c96f
                    .quad 0x3fe1c3b81f713c25
                    .quad 0x3fe1e85f5e7040d0
                    .quad 0x3fe20cdcd192ab6e
                    .quad 0x3fe23130d7bebf43
                    .quad 0x3fe2555bce98f7cb
                    .quad 0x3fe2795e1289b11b
                    .quad 0x3fe29d37fec2b08b
                    .quad 0x3fe2c0e9ed448e8c
                    .quad 0x3fe2e47436e40268
                    .quad 0x3fe307d7334f10be
                    .quad 0x3fe32b1339121d71
                    .quad 0x3fe34e289d9ce1d3
                    .quad 0x3fe37117b54747b6
                    .quad 0x3fe393e0d3562a1a
                    .quad 0x3fe3b68449fffc23
                    .quad 0x3fe3d9026a7156fb
                    .quad 0x3fe3fb5b84d16f42
                    .quad 0x3fe41d8fe84672ae
                    .quad 0x3fe43f9fe2f9ce67
                    .quad 0x3fe4618bc21c5ec2
                    .quad 0x3fe48353d1ea88df
                    .quad 0x3fe4a4f85db03ebb
                    .quad 0x3fe4c679afccee3a
                    .quad 0x3fe4e7d811b75bb1
                    .quad 0x3fe50913cc01686b
                    .quad 0x3fe52a2d265bc5ab
                    .quad 0x3fe54b2467999498
                    .quad 0x3fe56bf9d5b3f399
                    .quad 0x3fe58cadb5cd7989
                    .quad 0x3fe5ad404c359f2d
                    .quad 0x3fe5cdb1dc6c1765
                    .quad 0x3fe5ee02a9241675
                    .quad 0x3fe60e32f44788d9
                    .quad 0x3fe62e42fefa39ef

.align 16
.L__log_F_inv:
                    .quad 0x4000000000000000
                    .quad 0x3fffc07f01fc07f0
                    .quad 0x3fff81f81f81f820
                    .quad 0x3fff44659e4a4271
                    .quad 0x3fff07c1f07c1f08
                    .quad 0x3ffecc07b301ecc0
                    .quad 0x3ffe9131abf0b767
                    .quad 0x3ffe573ac901e574
                    .quad 0x3ffe1e1e1e1e1e1e
                    .quad 0x3ffde5d6e3f8868a
                    .quad 0x3ffdae6076b981db
                    .quad 0x3ffd77b654b82c34
                    .quad 0x3ffd41d41d41d41d
                    .quad 0x3ffd0cb58f6ec074
                    .quad 0x3ffcd85689039b0b
                    .quad 0x3ffca4b3055ee191
                    .quad 0x3ffc71c71c71c71c
                    .quad 0x3ffc3f8f01c3f8f0
                    .quad 0x3ffc0e070381c0e0
                    .quad 0x3ffbdd2b899406f7
                    .quad 0x3ffbacf914c1bad0
                    .quad 0x3ffb7d6c3dda338b
                    .quad 0x3ffb4e81b4e81b4f
                    .quad 0x3ffb2036406c80d9
                    .quad 0x3ffaf286bca1af28
                    .quad 0x3ffac5701ac5701b
                    .quad 0x3ffa98ef606a63be
                    .quad 0x3ffa6d01a6d01a6d
                    .quad 0x3ffa41a41a41a41a
                    .quad 0x3ffa16d3f97a4b02
                    .quad 0x3ff9ec8e951033d9
                    .quad 0x3ff9c2d14ee4a102
                    .quad 0x3ff999999999999a
                    .quad 0x3ff970e4f80cb872
                    .quad 0x3ff948b0fcd6e9e0
                    .quad 0x3ff920fb49d0e229
                    .quad 0x3ff8f9c18f9c18fa
                    .quad 0x3ff8d3018d3018d3
                    .quad 0x3ff8acb90f6bf3aa
                    .quad 0x3ff886e5f0abb04a
                    .quad 0x3ff8618618618618
                    .quad 0x3ff83c977ab2bedd
                    .quad 0x3ff8181818181818
                    .quad 0x3ff7f405fd017f40
                    .quad 0x3ff7d05f417d05f4
                    .quad 0x3ff7ad2208e0ecc3
                    .quad 0x3ff78a4c8178a4c8
                    .quad 0x3ff767dce434a9b1
                    .quad 0x3ff745d1745d1746
                    .quad 0x3ff724287f46debc
                    .quad 0x3ff702e05c0b8170
                    .quad 0x3ff6e1f76b4337c7
                    .quad 0x3ff6c16c16c16c17
                    .quad 0x3ff6a13cd1537290
                    .quad 0x3ff6816816816817
                    .quad 0x3ff661ec6a5122f9
                    .quad 0x3ff642c8590b2164
                    .quad 0x3ff623fa77016240
                    .quad 0x3ff6058160581606
                    .quad 0x3ff5e75bb8d015e7
                    .quad 0x3ff5c9882b931057
                    .quad 0x3ff5ac056b015ac0
                    .quad 0x3ff58ed2308158ed
                    .quad 0x3ff571ed3c506b3a
                    .quad 0x3ff5555555555555
                    .quad 0x3ff5390948f40feb
                    .quad 0x3ff51d07eae2f815
                    .quad 0x3ff5015015015015
                    .quad 0x3ff4e5e0a72f0539
                    .quad 0x3ff4cab88725af6e
                    .quad 0x3ff4afd6a052bf5b
                    .quad 0x3ff49539e3b2d067
                    .quad 0x3ff47ae147ae147b
                    .quad 0x3ff460cbc7f5cf9a
                    .quad 0x3ff446f86562d9fb
                    .quad 0x3ff42d6625d51f87
                    .quad 0x3ff4141414141414
                    .quad 0x3ff3fb013fb013fb
                    .quad 0x3ff3e22cbce4a902
                    .quad 0x3ff3c995a47babe7
                    .quad 0x3ff3b13b13b13b14
                    .quad 0x3ff3991c2c187f63
                    .quad 0x3ff3813813813814
                    .quad 0x3ff3698df3de0748
                    .quad 0x3ff3521cfb2b78c1
                    .quad 0x3ff33ae45b57bcb2
                    .quad 0x3ff323e34a2b10bf
                    .quad 0x3ff30d190130d190
                    .quad 0x3ff2f684bda12f68
                    .quad 0x3ff2e025c04b8097
                    .quad 0x3ff2c9fb4d812ca0
                    .quad 0x3ff2b404ad012b40
                    .quad 0x3ff29e4129e4129e
                    .quad 0x3ff288b01288b013
                    .quad 0x3ff27350b8812735
                    .quad 0x3ff25e22708092f1
                    .quad 0x3ff2492492492492
                    .quad 0x3ff23456789abcdf
                    .quad 0x3ff21fb78121fb78
                    .quad 0x3ff20b470c67c0d9
                    .quad 0x3ff1f7047dc11f70
                    .quad 0x3ff1e2ef3b3fb874
                    .quad 0x3ff1cf06ada2811d
                    .quad 0x3ff1bb4a4046ed29
                    .quad 0x3ff1a7b9611a7b96
                    .quad 0x3ff19453808ca29c
                    .quad 0x3ff1811811811812
                    .quad 0x3ff16e0689427379
                    .quad 0x3ff15b1e5f75270d
                    .quad 0x3ff1485f0e0acd3b
                    .quad 0x3ff135c81135c811
                    .quad 0x3ff12358e75d3033
                    .quad 0x3ff1111111111111
                    .quad 0x3ff0fef010fef011
                    .quad 0x3ff0ecf56be69c90
                    .quad 0x3ff0db20a88f4696
                    .quad 0x3ff0c9714fbcda3b
                    .quad 0x3ff0b7e6ec259dc8
                    .quad 0x3ff0a6810a6810a7
                    .quad 0x3ff0953f39010954
                    .quad 0x3ff0842108421084
                    .quad 0x3ff073260a47f7c6
                    .quad 0x3ff0624dd2f1a9fc
                    .quad 0x3ff05197f7d73404
                    .quad 0x3ff0410410410410
                    .quad 0x3ff03091b51f5e1a
                    .quad 0x3ff0204081020408
                    .quad 0x3ff0101010101010
                    .quad 0x3ff0000000000000

#---------------------
# exp data
#---------------------

.align 16

.L__real_zero:                  .quad 0x0000000000000000
                                .quad 0

.L__real_p4096:                 .quad 0x40b0000000000000
                                .quad 0
.L__real_m4768:                 .quad 0x0c0b2a00000000000
                                .quad 0

.L__real_32_by_log2:            .quad 0x40471547652b82fe # 32/ln(2)
                                .quad 0
.L__real_log2_by_32:            .quad 0x3f962e42fefa39ef # log2_by_32
                                .quad 0

.L__real_1_by_24:               .quad 0x3fa5555555555555 # 1/24
                                .quad 0
.L__real_1_by_6:                .quad 0x3fc5555555555555 # 1/6
                                .quad 0
.L__real_1_by_2:                .quad 0x3fe0000000000000 # 1/2
                                .quad 0
.L__real_1_by_1:                .quad 0x3ff0000000000000 # 1
                                .quad 0

.align 16

.L__two_to_jby32_table:
    .quad 0x3ff0000000000000
    .quad 0x3ff059b0d3158574
    .quad 0x3ff0b5586cf9890f
    .quad 0x3ff11301d0125b51
    .quad 0x3ff172b83c7d517b
    .quad 0x3ff1d4873168b9aa
    .quad 0x3ff2387a6e756238
    .quad 0x3ff29e9df51fdee1
    .quad 0x3ff306fe0a31b715
    .quad 0x3ff371a7373aa9cb
    .quad 0x3ff3dea64c123422
    .quad 0x3ff44e086061892d
    .quad 0x3ff4bfdad5362a27
    .quad 0x3ff5342b569d4f82
    .quad 0x3ff5ab07dd485429
    .quad 0x3ff6247eb03a5585
    .quad 0x3ff6a09e667f3bcd
    .quad 0x3ff71f75e8ec5f74
    .quad 0x3ff7a11473eb0187
    .quad 0x3ff82589994cce13
    .quad 0x3ff8ace5422aa0db
    .quad 0x3ff93737b0cdc5e5
    .quad 0x3ff9c49182a3f090
    .quad 0x3ffa5503b23e255d
    .quad 0x3ffae89f995ad3ad
    .quad 0x3ffb7f76f2fb5e47
    .quad 0x3ffc199bdd85529c
    .quad 0x3ffcb720dcef9069
    .quad 0x3ffd5818dcfba487
    .quad 0x3ffdfc97337b9b5f
    .quad 0x3ffea4afa2a490da
    .quad 0x3fff50765b6e4540



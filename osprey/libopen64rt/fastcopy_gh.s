/*
   Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.

   The Open64 Runtime Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The Open64 Runtime Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the Open64 Runtime Library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.
*/

        .text

################################################################################

/*
 # __fastcopy_stride64_gh(int *dst, int *src, long cnt, int exact)
 #
 # This code encapsulates iterations of 2 and greater including remainder
 # drain code in the loop epilog.  The cleanup code is only executed with
 # exact is false.
*/

################################################################################

        .align 16
.globl __fastcopy_stride64_gh
__fastcopy_stride64_gh:
        xorq       %r9, %r9
        movq       %rsi, %r8
        xorq       %rdi, %r8
        orq        %r8, %r9
        testq      $15, %r9
#       are both ptrs aligned together?
        jne        .fastcopy_stride64_gh_loop_default
#       now check if they are both 8 byte aligned
        movq       %rsi, %r9
        andq       $15, %r9
        cmpq       $8, %r9
        jz         .fastcopy_stride64_peeled_prolog
#       now check if they are 16 byte aligned
        testq      $15, %rsi
        je         .fastcopy_stride64_gh_superalign16
#       else fall through to default case
        .align 4
.fastcopy_stride64_gh_loop_default:
        movdqu       (%rsi), %xmm0
        movdqu     16(%rsi), %xmm1
        movdqu     %xmm0,    (%rdi)
        movdqu     %xmm1,  16(%rdi)
        movdqu     32(%rsi), %xmm0
        movdqu     48(%rsi), %xmm1
        movdqu     %xmm0,  32(%rdi)
        movdqu     %xmm1,  48(%rdi)
        leaq       64(%rsi), %rsi
        leaq       64(%rdi), %rdi
        dec        %rdx
        jnz     .fastcopy_stride64_gh_loop_default
        jmp     .fastcopy_stride64_gh_check_rem
.fastcopy_stride64_peeled_prolog:
#       single iter peel for align case
        movsd      (%rsi), %xmm0
        movsd      %xmm0,  (%rdi)
        leaq       8(%rsi), %rsi
        leaq       8(%rdi), %rdi
        dec        %rdx
        .align 4
.fastcopy_stride64_gh_superalign8:
        movdqa       (%rsi), %xmm0
        movdqa     16(%rsi), %xmm1
        movdqa     %xmm0,    (%rdi)
        movdqa     %xmm1,  16(%rdi)
        movdqa     32(%rsi), %xmm0
        movdqa     48(%rsi), %xmm1
        movdqa     %xmm0,  32(%rdi)
        movdqa     %xmm1,  48(%rdi)
        leaq       64(%rsi), %rsi
        leaq       64(%rdi), %rdi
        dec        %rdx
        jnz     .fastcopy_stride64_gh_superalign8
#       fixup epilog for 3 iters
        movdqa       (%rsi), %xmm0
        movdqa     16(%rsi), %xmm1
        movdqa     %xmm0,    (%rdi)
        movdqa     %xmm1,  16(%rdi)
        movdqa     32(%rsi), %xmm0
        movsd      48(%rsi), %xmm1
        movdqa     %xmm0,  32(%rdi)
        movsd      %xmm1,  48(%rdi)
        leaq       56(%rsi), %rsi
        leaq       56(%rdi), %rdi
        jmp     .fastcopy_stride64_gh_check_rem
#       fully aligned case
        .align 4
.fastcopy_stride64_gh_superalign16:
        movdqa       (%rsi), %xmm0
        movdqa     16(%rsi), %xmm1
        movdqa     %xmm0,    (%rdi)
        movdqa     %xmm1,  16(%rdi)
        movdqa     32(%rsi), %xmm0
        movdqa     48(%rsi), %xmm1
        movdqa     %xmm0,  32(%rdi)
        movdqa     %xmm1,  48(%rdi)
        leaq       64(%rsi), %rsi
        leaq       64(%rdi), %rdi
        dec        %rdx
        jnz     .fastcopy_stride64_gh_superalign16
#       check remainder case
.fastcopy_stride64_gh_check_rem:
        cmp     $0, %rcx
        jnz	.fastcopy_stride64_gh_no_rem_gh
#       do remainder
        movdqu        (%rsi), %xmm0
        movdqu     16 (%rsi), %xmm1
        movdqu     %xmm0,    (%rdi)
        movdqu     %xmm1,  16(%rdi)
.fastcopy_stride64_gh_no_rem_gh:
        ret
        .type __fastcopy_stride64_gh,@function
        .size __fastcopy_stride64_gh,.-__fastcopy_stride64_gh

################################################################################

/*
 # __fastcopy_stride64_gp(int *dst, int *src, long cnt, int exact)
 #
 # This code encapsulates iterations of 2 and greater including remainder
 # drain code in the loop epilog.  The cleanup code is only executed with
 # exact is false.
*/

        .align 16
.globl __fastcopy_stride64_gp
__fastcopy_stride64_gp:

        .align 4
.loop_copy64_gp:
        mov        (%rsi), %rax
        mov      8 (%rsi), %r11
        mov     16 (%rsi), %r9
        mov     24 (%rsi), %r10
        mov     %rax,    (%rdi)
        mov     %r11,  8 (%rdi)
        mov     %r9,  16 (%rdi)
        mov     %r10, 24 (%rdi)
        mov     32 (%rsi), %rax
        mov     40 (%rsi), %r11
        mov     48 (%rsi), %r9
        mov     56 (%rsi), %r10
        mov     %rax, 32 (%rdi)
        mov     %r11, 40 (%rdi)
        mov     %r9,  48 (%rdi)
        mov     %r10, 56 (%rdi)
        lea     64 (%rsi), %rsi
        lea     64 (%rdi), %rdi
        dec     %rdx
        jnz     .loop_copy64_gp

        cmp     $0, %rcx
        jnz	.no_rem_gp

        mov        (%rsi), %rax
        mov      8 (%rsi), %r11
        mov     16 (%rsi), %r9
        mov     24 (%rsi), %r10
        mov     %rax,    (%rdi)
        mov     %r11,  8 (%rdi)
        mov     %r9,  16 (%rdi)
        mov     %r10, 24 (%rdi)

.no_rem_gp:
        ret
        .type __fastcopy_stride64_gp,@function
        .size __fastcopy_stride64_gp,.-__fastcopy_stride64_gp

################################################################################

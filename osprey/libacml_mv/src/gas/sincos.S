
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


#
# An implementation of the sincos function.
#
# Prototype:
#
#   void sincos(double x, double* sinr, double* cosr);
#
#   Computes sincos
#   It will provide proper C99 return values,
#   but may not raise floating point status bits properly.
#   Based on the NAG C implementation.
#
#
#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif

.data
.align 16
.L__real_7fffffffffffffff: .quad 0x07fffffffffffffff  #Sign bit zero
                        .quad 0                       # for alignment
.L__real_3ff0000000000000: .quad 0x03ff0000000000000  # 1.0
                        .quad 0                          
.L__real_3fe0000000000000: .quad 0x03fe0000000000000  # 0.5
                        .quad 0
.L__real_3fc5555555555555: .quad 0x03fc5555555555555  # 0.166666666666
                        .quad 0
.L__real_3fe45f306dc9c883: .quad 0x03fe45f306dc9c883  # twobypi
                        .quad 0
.L__real_3ff921fb54400000: .quad 0x03ff921fb54400000  # piby2_1
                        .quad 0
.L__real_3dd0b4611a626331: .quad 0x03dd0b4611a626331  # piby2_1tail
                        .quad 0
.L__real_3dd0b4611a600000: .quad 0x03dd0b4611a600000  # piby2_2
                        .quad 0
.L__real_3ba3198a2e037073: .quad 0x03ba3198a2e037073  # piby2_2tail
                        .quad 0                
.L__real_fffffffff8000000: .quad 0x0fffffffff8000000  # mask for stripping head and tail
                        .quad 0                
.L__real_411E848000000000: .quad 0x415312d000000000   # 5e6 0x0411E848000000000  # 5e5
                        .quad 0

.align 16
.Lsincosarray:
    .quad    0x0bfc5555555555555                      # -0.16666666666666666    s1
    .quad    0x03fa5555555555555                      # 0.041666666666666664    c1   
    .quad    0x03f81111111110bb3                      # 0.00833333333333095     s2
    .quad    0x0bf56c16c16c16967                      # -0.0013888888888887398  c2
    .quad    0x0bf2a01a019e83e5c                      # -0.00019841269836761127 s3
    .quad    0x03efa01a019f4ec90                      # 2.4801587298767041E-05  c3
    .quad    0x03ec71de3796cde01                      # 2.7557316103728802E-06  s4
    .quad    0x0be927e4fa17f65f6                      # -2.7557317272344188E-07 c4
    .quad    0x0be5ae600b42fdfa7                      # -2.5051132068021698E-08 s5
    .quad    0x03e21eeb69037ab78                      # 2.0876146382232963E-09  c6
    .quad    0x03de5e0b2f9a43bb8                      # 1.5918144304485914E-10  s6
    .quad    0x0bda907db46cc5e42                      # -1.1382639806794487E-11 c7

.align 16
.Lcossinarray:
    .quad    0x03fa5555555555555                      # 0.0416667        c1
    .quad    0x0bfc5555555555555                      # -0.166667        s1
    .quad    0x0bf56c16c16c16967
    .quad    0x03f81111111110bb3                      # 0.00833333       s2
    .quad    0x03efa01a019f4ec90
    .quad    0x0bf2a01a019e83e5c                      # -0.000198413     s3
    .quad    0x0be927e4fa17f65f6
    .quad    0x03ec71de3796cde01                      # 2.75573e-006     s4
    .quad    0x03e21eeb69037ab78
    .quad    0x0be5ae600b42fdfa7                      # -2.50511e-008    s5
    .quad    0x0bda907db46cc5e42
    .quad    0x03de5e0b2f9a43bb8                      # 1.59181e-010     s6

.text
.align 16
.p2align 4,,15

#include "fn_macros.h"
#define fname FN_PROTOTYPE(sincos)
#define fname_special _sincos_special@PLT
    
# define local variable storage offsets
.equ    p_temp,    0x30                               # temporary for get/put bits operation
.equ    p_temp1,   0x40                               # temporary for get/put bits operation
.equ    r,         0x50                               # pointer to r for amd_remainder_piby2
.equ    rr,        0x60                               # pointer to rr for amd_remainder_piby2
.equ    region,    0x70                               # pointer to region for amd_remainder_piby2
.equ   stack_size, 0x98

.globl fname
.type  fname,@function

fname:
    sub     $stack_size, %rsp
    xorpd   %xmm2,%xmm2                               # zeroed out for later use

# GET_BITS_DP64(x, ux);
# get the input value to an integer register.
    movsd   %xmm0,p_temp(%rsp)
    mov     p_temp(%rsp),%rcx                         # rcx is ux

##  if NaN or inf
    mov     $0x07ff0000000000000,%rax
    mov     %rax,%r10
    and     %rcx,%r10
    cmp     %rax,%r10
    jz      .Lsincos_naninf

#  ax = (ux & ~SIGNBIT_DP64);
    mov     $0x07fffffffffffffff,%r10
    and     %rcx,%r10                # r10 is ax

##  if (ax <= 0x3fe921fb54442d18) /* abs(x) <= pi/4 */
    mov     $0x03fe921fb54442d18,%rax
    cmp     %rax,%r10
    jg      .Lsincos_reduce

##      if (ax < 0x3f20000000000000) /* abs(x) < 2.0^(-13) */
    mov     $0x03f20000000000000,%rax
    cmp     %rax,%r10
    jge     .Lsincos_small

##          if (ax < 0x3e40000000000000) /* abs(x) < 2.0^(-27) */
    mov     $0x03e40000000000000,%rax
    cmp     %rax,%r10
    jge     .Lsincos_smaller

                            # sin = x;
    movsd   .L__real_3ff0000000000000(%rip),%xmm1     # cos = 1.0;
    jmp     .Lsincos_cleanup   

##          else
.align 32
.Lsincos_smaller:
#              sin = x - x^3 * 0.1666666666666666666;
#              cos = 1.0 - x*x*0.5;

    movsd   %xmm0,%xmm2
    movsd   .L__real_3fc5555555555555(%rip),%xmm4     # 0.1666666666666666666
    mulsd   %xmm2,%xmm2                               # x^2
    movsd   .L__real_3ff0000000000000(%rip),%xmm1     # 1.0
    movsd   %xmm2,%xmm3                               # copy of x^2

    mulsd   %xmm0,%xmm2                               # x^3
    mulsd   .L__real_3fe0000000000000(%rip),%xmm3     # 0.5 * x^2
    mulsd   %xmm4,%xmm2                               # x^3 * 0.1666666666666666666
    subsd   %xmm2,%xmm0                               # x - x^3 * 0.1666666666666666666, sin
    subsd   %xmm3,%xmm1                               # 1 - 0.5 * x^2, cos

    jmp     .Lsincos_cleanup                


##      else

.align 16
.Lsincos_small:
#          sin = sin_piby4(x, 0.0);
    movsd   .L__real_3fe0000000000000(%rip),%xmm5     # .5

#  x2 = r * r;
    movsd   %xmm0,%xmm2
    mulsd   %xmm0,%xmm2                               # x2
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
# region 0 or 2     - do a sin calculation
#  zs = (s2 + x2 * (s3 + x2 * (s4 + x2 * (s5 + x2 * s6))));

    movlhps %xmm2,%xmm2
    movapd  .Lsincosarray+0x50(%rip),%xmm3            # s6
    movapd  %xmm2,%xmm1                               # move for x4
    movdqa  .Lsincosarray+0x20(%rip),%xmm5            # s3
    mulpd   %xmm2,%xmm3                               # x2s6
    addpd   .Lsincosarray+0x40(%rip),%xmm3            # s5+x2s6
    mulpd   %xmm2,%xmm5                               # x2s3
    movapd  %xmm4,p_temp(%rsp)                        # rr move to to memory
    mulpd   %xmm2,%xmm1                               # x4
    mulpd   %xmm2,%xmm3                               # x2(s5+x2s6)
    addpd   .Lsincosarray+0x10(%rip),%xmm5            # s2+x2s3
    movapd  %xmm1,%xmm4                               # move for x6
    addpd   .Lsincosarray+0x30(%rip),%xmm3            # s4 + x2(s5+x2s6)
    mulpd   %xmm2,%xmm5                               # x2(s2+x2s3)
    mulpd   %xmm2,%xmm4                               # x6
    addpd   .Lsincosarray(%rip),%xmm5                 # s1+x2(s2+x2s3)
    mulpd   %xmm4,%xmm3                               # x6(s4 + x2(s5+x2s6))

    movsd   %xmm2,%xmm4                               # xmm4 = x2 for 0.5x2 for cos
                                                      # xmm2 contains x2 for x3 for sin
    addpd   %xmm5,%xmm3                               # zs in lower and zc upper

    mulsd   %xmm0,%xmm2                               # xmm2=x3 for sin

    movhlps %xmm3,%xmm5                               # Copy z, xmm5 = cos , xmm3 = sin

    mulsd   .L__real_3fe0000000000000(%rip),%xmm4     # xmm4=r=0.5*x2 for cos term
    mulsd   %xmm2,%xmm3                               # sin *x3
    mulsd   %xmm1,%xmm5                               # cos *x4
    movsd   .L__real_3ff0000000000000(%rip),%xmm2     # 1.0
    subsd   %xmm4,%xmm2                               # t=1.0-r
    movsd   .L__real_3ff0000000000000(%rip),%xmm1     # 1.0
    subsd   %xmm2,%xmm1                               # 1 - t
    subsd   %xmm4,%xmm1                               # (1-t) -r
    addsd   %xmm5,%xmm1                               # ((1-t) -r) + cos
    addsd   %xmm3,%xmm0                               # xmm0= sin+x, final sin term
    addsd   %xmm2,%xmm1                               # xmm1 = t +{ ((1-t) -r) + cos}, final cos term

    jmp     .Lsincos_cleanup
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.align 16
.Lsincos_reduce:
# change rdx to rcx and r8 to r9
# rcx= ux, r10 = ax
# %r9,%rax are free

#  xneg = (ax != ux);
    cmp     %r10,%rcx
    mov     $0,%r11d

##  if (xneg) x = -x;
    jz      .LPositive
    mov     $1,%r11d
    subsd   %xmm0,%xmm2
    movsd   %xmm2,%xmm0

# rcx= ux, r10 = ax, r11= Sign
# %r9,%rax are free
# change rdx to rcx and r8 to r9

.align 16
.LPositive:
##  if (x < 5.0e5)
    cmp     .L__real_411E848000000000(%rip),%r10
    jae     .Lsincos_reduce_precise

# reduce  the argument to be in a range from -pi/4 to +pi/4
# by subtracting multiples of pi/2
    movsd   %xmm0,%xmm2
    movsd   .L__real_3fe45f306dc9c883(%rip),%xmm3     # twobypi
    movsd   %xmm0,%xmm4
    movsd   .L__real_3fe0000000000000(%rip),%xmm5     # .5
    mulsd   %xmm3,%xmm2

#/* How many pi/2 is x a multiple of? */
#       xexp  = ax >> EXPSHIFTBITS_DP64;
    shr     $52,%r10                                  # >>EXPSHIFTBITS_DP64

#       npi2  = (int)(x * twobypi + 0.5);
    addsd   %xmm5,%xmm2                               # npi2

    movsd   .L__real_3ff921fb54400000(%rip),%xmm3     # piby2_1
    cvttpd2dq    %xmm2,%xmm0                          # convert to integer
    movsd   .L__real_3dd0b4611a626331(%rip),%xmm1     # piby2_1tail
    cvtdq2pd    %xmm0,%xmm2                           # and back to float.

#       /* Subtract the multiple from x to get an extra-precision remainder */
#       rhead  = x - npi2 * piby2_1;
    mulsd   %xmm2,%xmm3
    subsd   %xmm3,%xmm4                               # rhead

#       rtail  = npi2 * piby2_1tail;
    mulsd   %xmm2,%xmm1
    movd    %xmm0,%eax


#       GET_BITS_DP64(rhead-rtail, uy);                   ; originally only rhead
    movsd   %xmm4,%xmm0
    subsd   %xmm1,%xmm0

    movsd   .L__real_3dd0b4611a600000(%rip),%xmm3     # piby2_2
    movsd   %xmm0,p_temp(%rsp)
    movsd   .L__real_3ba3198a2e037073(%rip),%xmm5     # piby2_2tail
    mov     %eax,%ecx
    mov     p_temp(%rsp),%r9                          # rcx is rhead-rtail

#    xmm0=r, xmm4=rhead, xmm1=rtail, xmm2=npi2, xmm3=temp for calc, xmm5= temp for calc
#       expdiff = xexp - ((uy & EXPBITS_DP64) >> EXPSHIFTBITS_DP64);
    shl     $1,%r9                                    # strip any sign bit
    shr     $53,%r9                                   # >> EXPSHIFTBITS_DP64 +1
    sub     %r9,%r10                                  # expdiff

##       if (expdiff > 15)
    cmp     $15,%r10
    jle     .Lexpdiff15

#          /* The remainder is pretty small compared with x, which
#             implies that x is a near multiple of pi/2
#             (x matches the multiple to at least 15 bits) */

#          t  = rhead;
    movsd   %xmm4,%xmm1

#          rtail  = npi2 * piby2_2;
    mulsd   %xmm2,%xmm3

#          rhead  = t - rtail;
    mulsd   %xmm2,%xmm5                               # npi2 * piby2_2tail
    subsd   %xmm3,%xmm4                               # rhead

#          rtail  = npi2 * piby2_2tail - ((t - rhead) - rtail);
    subsd   %xmm4,%xmm1                               # t - rhead
    subsd   %xmm3,%xmm1                               # -rtail
    subsd   %xmm1,%xmm5                               # rtail

#      r = rhead - rtail;
    movsd   %xmm4,%xmm0

#HARSHA
#xmm1=rtail
    movsd   %xmm5,%xmm1
    subsd   %xmm5,%xmm0
    
#    xmm0=r, xmm4=rhead, xmm1=rtail
.Lexpdiff15:
#      region = npi2 & 3;

    subsd   %xmm0,%xmm4                               # rhead-r
    subsd   %xmm1,%xmm4                               # rr = (rhead-r) - rtail

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
## if the input was close to a pi/2 multiple
# The original NAG code missed this trick.  If the input is very close to n*pi/2 after
# reduction,
# then the sin is ~ 1.0 , to within 53 bits, when r is < 2^-27.  We already
# have x at this point, so we can skip the sin polynomials.

    cmp     $0x03f2,%r9                               # if r  small.
    jge     .Lcossin_piby4                            # use taylor series if not
    cmp     $0x03de,%r9                               # if r really small.
    jle     .Lr_small                                 # then sin(r) = 0

    movsd   %xmm0,%xmm2
    mulsd   %xmm2,%xmm2                               # x^2

##      if region is 0 or 2 do a sin calc.
    and     $1,%ecx
    jnz     .Lregion13

# region 0 or 2 do a sincos calculation
# use simply polynomial
#              sin=x - x*x*x*0.166666666666666666;
    movsd   .L__real_3fc5555555555555(%rip),%xmm3     # 0.166666666
    mulsd   %xmm0,%xmm3                               # * x
    mulsd   %xmm2,%xmm3                               # * x^2
    subsd   %xmm3,%xmm0                               # xs
#              cos=1.0 - x*x*0.5;
    movsd   .L__real_3ff0000000000000(%rip),%xmm1     # 1.0
    mulsd   .L__real_3fe0000000000000(%rip),%xmm2     # 0.5 *x^2
    subsd   %xmm2,%xmm1                               # xc

    jmp     .Ladjust_region

.align 16
.Lregion13:
# region 1 or 3 do a cossin calculation
# use simply polynomial
#              sin=x - x*x*x*0.166666666666666666;
    movsd   %xmm0,%xmm1

    movsd   .L__real_3fc5555555555555(%rip),%xmm3     # 0.166666666
    mulsd   %xmm0,%xmm3                               # 0.166666666* x
    mulsd   %xmm2,%xmm3                               # 0.166666666* x * x^2
    subsd   %xmm3,%xmm1                               # xs
#              cos=1.0 - x*x*0.5;
    movsd   .L__real_3ff0000000000000(%rip),%xmm0     # 1.0
    mulsd   .L__real_3fe0000000000000(%rip),%xmm2     # 0.5 *x^2
    subsd   %xmm2,%xmm0                               # xc

    jmp     .Ladjust_region

.align 16
.Lr_small:
##      if region is 0 or 2    do a sincos calc.
    movsd   .L__real_3ff0000000000000(%rip),%xmm1     # cos(r) is a 1
    and     $1,%ecx
    jz      .Ladjust_region

##      if region is 1 or 3    do a cossin calc.
    movsd   %xmm0,%xmm1                               # sin(r) is r
    movsd   .L__real_3ff0000000000000(%rip),%xmm0     # cos(r) is a 1
    jmp     .Ladjust_region

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.align 16
.Lsincos_reduce_precise:
#      // Reduce x into range [-pi/4,pi/4]
#      __amd_remainder_piby2(x, &r, &rr, &region);

    mov     %rdi, p_temp1(%rsp)
    mov     %rsi, p_temp1+8(%rsp)
    mov     %r11,p_temp(%rsp)

    lea     region(%rsp),%rdx
    lea     rr(%rsp),%rsi
    lea     r(%rsp),%rdi
    
    call    __amd_remainder_piby2@PLT

    mov     p_temp1(%rsp), %rdi
    mov     p_temp1+8(%rsp), %rsi
    mov     p_temp(%rsp),%r11

    movsd   r(%rsp),%xmm0                             # x
    movsd   rr(%rsp),%xmm4                            # xx
    mov     region(%rsp),%eax                         # region to classify for sin/cos calc
    mov     %eax,%ecx                                 # region to get sign

# xmm0 = x, xmm4 = xx, r8d = 1, eax= region
.align 16
.Lcossin_piby4:
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
# perform taylor series to calc sinx, sinx
#  x2 = r * r;
#xmm4 = a part of rr for the sin path, xmm4 is overwritten in the sin path
#instead use xmm3 because that was freed up in the sin path, xmm3 is overwritten in sin path

    movsd   %xmm0,%xmm2
    mulsd   %xmm0,%xmm2                               #x2

##      if region is 0 or 2    do a sincos calc.
    and     $1,%ecx
    jz      .Lsincos02

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
# region 1 or 3     - do a cossin calculation
#  zc = (c2 + x2 * (c3 + x2 * (c4 + x2 * (c5 + x2 * c6))));


    movlhps %xmm2,%xmm2

    movapd  .Lcossinarray+0x50(%rip),%xmm3            # s6
    movapd  %xmm2,%xmm1                               # move for x4
    movdqa  .Lcossinarray+0x20(%rip),%xmm5            # s3
    mulpd   %xmm2,%xmm3                               # x2s6
    addpd   .Lcossinarray+0x40(%rip),%xmm3            # s5+x2s6
    mulpd   %xmm2,%xmm5                               # x2s3
    movsd   %xmm4,p_temp(%rsp)                        # rr move to to memory
    mulpd   %xmm2,%xmm1                               # x4
    mulpd   %xmm2,%xmm3                               # x2(s5+x2s6)
    addpd   .Lcossinarray+0x10(%rip),%xmm5            # s2+x2s3
    movapd  %xmm1,%xmm4                               # move for x6
    addpd   .Lcossinarray+0x30(%rip),%xmm3            # s4 + x2(s5+x2s6)
    mulpd   %xmm2,%xmm5                               # x2(s2+x2s3)
    mulpd   %xmm2,%xmm4                               # x6
    addpd   .Lcossinarray(%rip),%xmm5                 # s1+x2(s2+x2s3)
    mulpd   %xmm4,%xmm3                               # x6(s4 + x2(s5+x2s6))

    movsd   %xmm2,%xmm4                               # xmm4 = x2 for 0.5x2 cos
                                                      # xmm2 contains x2 for x3 sin

    addpd   %xmm5,%xmm3                               # zc in lower and zs in upper

    mulsd   %xmm0,%xmm2                               # xmm2=x3 for the sin term

    movhlps %xmm3,%xmm5                               # Copy z, xmm5 = sin, xmm3 = cos
    mulsd   .L__real_3fe0000000000000(%rip),%xmm4     # xmm4=r=0.5*x2 for cos term

    mulsd   %xmm2,%xmm5                               # sin *x3
    mulsd   %xmm1,%xmm3                               # cos *x4
    movsd   %xmm0,p_temp1(%rsp)                       # store x
    movsd   %xmm0,%xmm1

    movsd   .L__real_3ff0000000000000(%rip),%xmm2     # 1.0
    subsd   %xmm4,%xmm2                               # t=1.0-r

    movsd   .L__real_3ff0000000000000(%rip),%xmm0     # 1.0
    subsd   %xmm2,%xmm0                               # 1 - t

    mulsd   p_temp(%rsp),%xmm1                        # x*xx
    subsd   %xmm4,%xmm0                               # (1-t) -r
    subsd   %xmm1,%xmm0                               # ((1-t) -r) - x *xx

    mulsd   p_temp(%rsp),%xmm4                        # 0.5*x2*xx

    addsd   %xmm3,%xmm0                               # (((1-t) -r) - x *xx) + cos

    subsd   %xmm4,%xmm5                               # sin - 0.5*x2*xx

    addsd   %xmm2,%xmm0                               # xmm0 = t +{ (((1-t) -r) - x *xx) + cos}, final cos term

    addsd   p_temp(%rsp),%xmm5                        # sin + xx
    movsd   p_temp1(%rsp),%xmm1                       # load x
    addsd   %xmm5,%xmm1                               # xmm1= sin+x, final sin term

    jmp     .Ladjust_region

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.align 16
.Lsincos02:
# region 0 or 2 do a sincos calculation
    movlhps %xmm2,%xmm2

    movapd  .Lsincosarray+0x50(%rip),%xmm3            # s6
    movapd  %xmm2,%xmm1                               # move for x4
    movdqa  .Lsincosarray+0x20(%rip),%xmm5            # s3
    mulpd   %xmm2,%xmm3                               # x2s6
    addpd   .Lsincosarray+0x40(%rip),%xmm3            # s5+x2s6
    mulpd   %xmm2,%xmm5                               # x2s3
    movsd   %xmm4,p_temp(%rsp)                        # rr move to to memory
    mulpd   %xmm2,%xmm1                               # x4
    mulpd   %xmm2,%xmm3                               # x2(s5+x2s6)
    addpd   .Lsincosarray+0x10(%rip),%xmm5            # s2+x2s3
    movapd  %xmm1,%xmm4                               # move for x6
    addpd   .Lsincosarray+0x30(%rip),%xmm3            # s4 + x2(s5+x2s6)
    mulpd   %xmm2,%xmm5                               # x2(s2+x2s3)
    mulpd   %xmm2,%xmm4                               # x6
    addpd   .Lsincosarray(%rip),%xmm5                 # s1+x2(s2+x2s3)
    mulpd   %xmm4,%xmm3                               # x6(s4 + x2(s5+x2s6))

    movsd   %xmm2,%xmm4                               # xmm4 = x2 for 0.5x2 for cos
                                                      # xmm2 contains x2 for x3 for sin

    addpd   %xmm5,%xmm3                               # zs in lower and zc in upper

    mulsd   %xmm0,%xmm2                               # xmm2=x3 for sin

    movhlps %xmm3,%xmm5                               # Copy z, xmm5 = cos , xmm3 = sin

    mulsd   .L__real_3fe0000000000000(%rip),%xmm4     # xmm4=r=0.5*x2 for cos term

    mulsd   %xmm2,%xmm3                               # sin *x3
    mulsd   %xmm1,%xmm5                               # cos *x4

    movsd   .L__real_3ff0000000000000(%rip),%xmm2     # 1.0
    subsd   %xmm4,%xmm2                               # t=1.0-r

    movsd   .L__real_3ff0000000000000(%rip),%xmm1     # 1.0
    subsd   %xmm2,%xmm1                               # 1 - t

    movsd   %xmm0,p_temp1(%rsp)                       # store x
    mulsd   p_temp(%rsp),%xmm0                        # x*xx

    subsd   %xmm4,%xmm1                               # (1-t) -r
    subsd   %xmm0,%xmm1                               # ((1-t) -r) - x *xx

    mulsd   p_temp(%rsp),%xmm4                        # 0.5*x2*xx

    addsd   %xmm5,%xmm1                               # (((1-t) -r) - x *xx) + cos

    subsd   %xmm4,%xmm3                               # sin - 0.5*x2*xx

    addsd   %xmm2,%xmm1                               # xmm1 = t +{ (((1-t) -r) - x *xx) + cos}, final cos term

    addsd   p_temp(%rsp),%xmm3                        # sin + xx
    movsd   p_temp1(%rsp),%xmm0                       # load x
    addsd   %xmm3,%xmm0                               # xmm0= sin+x, final sin term

    jmp     .Ladjust_region

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#      switch (region)
.align 16
.Ladjust_region:        # positive or negative for sin return val in xmm0

    mov     %eax,%r9d

    shr     $1,%eax
    mov     %eax,%ecx
    and     %r11d,%eax

    not     %ecx
    not     %r11d
    and     %r11d,%ecx

    or      %ecx,%eax
    and     $1,%eax
    jnz     .Lcos_sign

## if the original region 0, 1 and arg is negative, then we negate the result.
## if the original region 2, 3 and arg is positive, then we negate the result.
    movsd   %xmm0,%xmm2
    xorpd   %xmm0,%xmm0
    subsd   %xmm2,%xmm0

.Lcos_sign:            # positive or negative for cos return val in xmm1
    add     $1,%r9
    and     $2,%r9d
    jz      .Lsincos_cleanup
## if the original region 1 or 2 then we negate the result.
    movsd   %xmm1,%xmm2
    xorpd   %xmm1,%xmm1
    subsd   %xmm2,%xmm1

#.align 16
.Lsincos_cleanup:
    movsd   %xmm0, (%rdi)                             # save the sin
    movsd   %xmm1, (%rsi)                             # save the cos

    add     $stack_size,%rsp
    ret

.align 16
.Lsincos_naninf:
   call     fname_special
   add      $stack_size, %rsp
   ret


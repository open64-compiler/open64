
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
#define fname FN_PROTOTYPE(expm1)

#ifdef __ELF__
    .section .note.GNU-stack,"",@progbits
#endif

	.text
	.p2align 4
.globl fname
	.type	fname, @function
		
fname:

    ucomisd .L__max_expm1_arg(%rip),%xmm0  #check if(x > 709.8)
    ja .L__Max_Arg
    jp .L__Max_Arg
    ucomisd .L__min_expm1_arg(%rip),%xmm0  #if(x < -37.42994775023704)
    jb .L__Min_Arg
    ucomisd .L__log_OneMinus_OneByFour(%rip),%xmm0
    jbe .L__Normal_Flow
    ucomisd .L__log_OnePlus_OneByFour(%rip),%xmm0
    jb .L__Small_Arg 
    
    .p2align 4
.L__Normal_Flow:
    movapd %xmm0,%xmm1  #xmm1 = x
    mulsd .L__thirtyTwo_by_ln2(%rip),%xmm1   #xmm1 = x*thirtyTwo_by_ln2
    ucomisd .L__zero(%rip),%xmm1             #check if temp < 0.0
    jae .L__Add_Point_Five
    subsd .L__point_Five(%rip),%xmm1
    jmp .L__next     
.L__Add_Point_Five:
    addsd .L__point_Five(%rip),%xmm1         #xmm1 = temp +/- 0.5
.L__next:
    cvttpd2dq %xmm1,%xmm2              #xmm2 = (int)n
    cvtdq2pd  %xmm2,%xmm1              #xmm1 = (double)n  
    movapd %xmm2,%xmm3                 #xmm3 = (int)n
    psrad $5,%xmm2                     #xmm2 = m
    pslld $27,%xmm3            
    psrld $27,%xmm3                    #xmm3 = j    
    movd %xmm3,%edx                    #edx = j
    movd %xmm2,%ecx                    #ecx = m
    
    movlhps %xmm1,%xmm1                #xmm1 = n,n
    mulpd .L__Ln2By32_MinusTrailLead(%rip),%xmm1 
    movapd %xmm0,%xmm2
    subsd %xmm1,%xmm2                  #xmm2 = r1
    psrldq $8,%xmm1                    #xmm1 = r2
    movapd %xmm2,%xmm3                 #xmm3 = r1    
    addsd %xmm1,%xmm3                  #xmm3 = r
    #q = r*(r*(A1.f64 + r*(A2.f64 + r*(A3.f64 + r*(A4.f64 + r*(A5.f64))))));    
    movapd %xmm3,%xmm4
    mulsd .L__A5(%rip),%xmm4
    addsd .L__A4(%rip),%xmm4
    mulsd %xmm3,%xmm4
    addsd .L__A3(%rip),%xmm4
    mulsd %xmm3,%xmm4
    addsd .L__A2(%rip),%xmm4
    mulsd %xmm3,%xmm4
    addsd .L__A1(%rip),%xmm4
    mulsd %xmm3,%xmm4
    mulsd %xmm4,%xmm3                #xmm3 = q
    
    shl $4,%edx
	lea  S_lead_and_trail_table(%rip),%rax
    movdqa  (%rax,%rdx,1),%xmm5       #xmm5 = S_T,S_L
    
    #p = (r2+q) + r1;
    addsd %xmm3,%xmm1
    addsd %xmm1,%xmm2                #xmm2 = p
    
    #s = S_L.f64 + S_T.f64;    
    movhlps %xmm5,%xmm4              #xmm4 = S_T
    movapd %xmm4,%xmm3               #xmm3 = S_T
    addsd %xmm5,%xmm3                #xmm3 = s
    
    cmp $52,%ecx        #check m > 52
    jg .L__M_Above_52
    cmp $-7,%ecx        #check if m < -7
    jl .L__M_Below_Minus7
    #(-8 < m) && (m < 53)
    movapd %xmm2,%xmm3               #xmm3 = p
    addsd .L__One(%rip),%xmm3  #xmm3 = 1+p
    mulsd %xmm4,%xmm3          #xmm3 = S_T.f64 *(1+p)
    mulsd %xmm5,%xmm2                #xmm2 = S_L*p
    addsd %xmm3,%xmm2 #xmm2 = (S_L.f64*p+ S_T.f64 *(1+p))
    mov $1023,%edx
    sub %ecx,%edx                    #edx = twopmm
    shl $52,%rdx
    movd %rdx,%xmm1            #xmm1 = twopmm
    subsd %xmm1,%xmm5    #xmm5 = S_L.f64 - twopmm.f64
    addsd %xmm5,%xmm2
    shl $52,%rcx
    movd %rcx,%xmm0      #xmm0 = twopm
    paddq %xmm2,%xmm0   #xmm0 = twopm *(xmm2)
    ret   
    
    .p2align 4  
.L__M_Above_52:
    cmp $1024,%ecx #check if m = 1024
    je .L__M_Equals_1024
    #twopm.f64 * (S_L.f64 + (s*p+(S_T.f64 - twopmm.f64)));// 2^-m should not be calculated if m>105
    mov $1023,%edx
    sub %ecx,%edx                    #edx = twopmm
    shl $52,%rdx
    movd %rdx,%xmm1            #xmm1 = twopmm
    subsd %xmm1,%xmm4  #xmm4 = S_T - twopmm
    mulsd %xmm3,%xmm2  #xmm2 = s*p
    addsd %xmm4,%xmm2 
    addsd %xmm5,%xmm2
    shl $52,%rcx
    movd %rcx,%xmm0      #xmm0 = twopm
    paddq %xmm2,%xmm0
    ret
    
    .p2align 4    
.L__M_Below_Minus7:
    #twopm.f64 * (S_L.f64 + (s*p + S_T.f64)) - 1;
    mulsd %xmm3,%xmm2    #xmm2 = s*p
    addsd %xmm4,%xmm2   #xmm2 = (s*p + S_T.f64)
    addsd %xmm5,%xmm2   #xmm2 = (S_L.f64 + (s*p + S_T.f64))
    shl $52,%rcx
    movd %rcx,%xmm0      #xmm0 = twopm
    paddq %xmm2,%xmm0   #xmm0 = twopm *(xmm2)
    subsd .L__One(%rip),%xmm0    
    ret
    
    .p2align 4
.L__M_Equals_1024:
    mov $0x4000000000000000,%rax #1024 at exponent
    mulsd %xmm3,%xmm2 #xmm2 = s*p
    addsd %xmm4,%xmm2 #xmm2 = (s*p) + S_T
    addsd %xmm5,%xmm2 #xmm2 = S_L + ((s*p) + S_T)
    movd %rax,%xmm1 #xmm1 = twopm
    paddq %xmm2,%xmm1
    movd %xmm1,%rax
    mov $0x7FF0000000000000,%rcx
    and %rcx,%rax
    cmp %rcx,%rax #check if we reached inf
    je .L__return_Inf
    movapd %xmm1,%xmm0                   
    ret
    
    .p2align 4
.L__Small_Arg:
    movapd %xmm0,%xmm1
    psllq $1,%xmm1
    psrlq $1,%xmm1            #xmm1 = abs(x)
    ucomisd .L__Five_Pont_FiveEMinus17(%rip),%xmm1
    jb .L__VeryTinyArg
    mov $0x01E0000000000000,%rax #30 in exponents place
    #u = (twop30.f64 * x + x) - twop30.f64 * x;    
    movd %rax,%xmm1
    paddq %xmm0,%xmm1  #xmm1 = twop30.f64 * x
    movapd %xmm1,%xmm2
    addsd %xmm0,%xmm2 #xmm2 = (twop30.f64 * x + x)
    subsd %xmm1,%xmm2 #xmm2 = u
    movapd %xmm0,%xmm1
    subsd %xmm2,%xmm1 #xmm1 = v = x-u
    movapd %xmm2,%xmm3 #xmm3 = u
    mulsd %xmm2,%xmm3 #xmm3 = u*u
    mulsd .L__point_Five(%rip),%xmm3 #xmm3 = y = u*u*0.5
    #z = v * (x + u) * 0.5;
    movapd %xmm0,%xmm4
    addsd %xmm2,%xmm4
    mulsd %xmm1,%xmm4
    mulsd .L__point_Five(%rip),%xmm4 #xmm4 = z   
    
    #q = x*x*x*(A1.f64 + x*(A2.f64 + x*(A3.f64 + x*(A4.f64 + x*(A5.f64 + x*(A6.f64 + x*(A7.f64 + x*(A8.f64 + x*(A9.f64)))))))));
    movapd %xmm0,%xmm5
    mulsd .L__B9(%rip),%xmm5
    addsd .L__B8(%rip),%xmm5
    mulsd %xmm0,%xmm5
    addsd .L__B7(%rip),%xmm5
    mulsd %xmm0,%xmm5
    addsd .L__B6(%rip),%xmm5
    mulsd %xmm0,%xmm5           
    addsd .L__B5(%rip),%xmm5
    mulsd %xmm0,%xmm5    
    addsd .L__B4(%rip),%xmm5
    mulsd %xmm0,%xmm5    
    addsd .L__B3(%rip),%xmm5
    mulsd %xmm0,%xmm5    
    addsd .L__B2(%rip),%xmm5
    mulsd %xmm0,%xmm5   
    addsd .L__B1(%rip),%xmm5
    mulsd %xmm0,%xmm5  
    mulsd %xmm0,%xmm5  
    mulsd %xmm0,%xmm5   #xmm5 = q
    
    ucomisd .L__TwopM7(%rip),%xmm3    
    jb .L__returnNext
    addsd %xmm4,%xmm1  #xmm1 = v+z
    addsd %xmm5,%xmm1  #xmm1 = q+(v+z)
    addsd %xmm3,%xmm2  #xmm2 = u+y
    addsd %xmm2,%xmm1
    movapd %xmm1,%xmm0
    ret    
    .p2align 4
.L__returnNext:
    addsd %xmm5,%xmm4  #xmm4 = q +z
    addsd %xmm4,%xmm3  #xmm3 = y+(q+z)
    addsd %xmm3,%xmm0    
    ret
    
    .p2align 4  
.L__VeryTinyArg:
    #(twop100.f64 * x + xabs.f64) * twopm100.f64);
    mov $0x0640000000000000,%rax #100 at exponent's place
    movd %rax,%xmm2
    paddq %xmm2,%xmm0
    addsd %xmm1,%xmm0
    psubq %xmm2,%xmm0
    ret    
      
    
    .p2align 4
.L__Max_Arg:
   movd %xmm0,%rcx
   mov $0x7ff0000000000000,%rax
   cmp %rax,%rcx                        #x is either Nan or Inf
   jb .L__return_Inf
   mov $0x000fffffffffffff,%rdx         #check if x is Nan
   and %rdx,%rcx
   jne .L__Nan
.L__return_Inf:
   movd %rax,%xmm0
   #call error_handler  
   ret
   .p2align 4 
.L__Nan:
    addsd   %xmm0,%xmm0
    ret      
   ret
    
    .p2align 4  
.L__Min_Arg:
    mov $0xBFF0000000000000,%rax   #return -1
    #call error handler
    movd %rax,%xmm0
    ret      
    
.data
.align 16
.L__max_expm1_arg:
    .quad 0x40862E6666666666    
.L__min_expm1_arg:
    .quad 0xC042B708872320E1
.L__log_OneMinus_OneByFour:
    .quad 0xBFD269621134DB93
.L__log_OnePlus_OneByFour:
    .quad 0x3FCC8FF7C79A9A22
.L__thirtyTwo_by_ln2:    
    .quad 0x40471547652B82FE
.L__zero:
    .quad 0x0000000000000000    
.L__point_Five:
    .quad 0x3FE0000000000000
    
.align 16    
.L__Ln2By32_MinusTrailLead:
    .octa 0xBD8473DE6AF278ED3F962E42FEF00000 
.L__A5:
    .quad 0x3F56C1728D739765
.L__A4:
    .quad 0x3F811115B7AA905E
.L__A3:
    .quad 0x3FA5555555545D4E
.L__A2:
    .quad 0x3FC5555555548F7C
.L__A1:
    .quad 0x3FE0000000000000      
.L__One:
    .quad 0x3FF0000000000000

.align 16
# .type	two_to_jby32_table, @object
# .size	two_to_jby32_table, 512
S_lead_and_trail_table:
	.octa  0x00000000000000003FF0000000000000
	.octa  0x3D0A1D73E2A475B43FF059B0D3158540
	.octa  0x3CEEC5317256E3083FF0B5586CF98900
	.octa  0x3CF0A4EBBF1AED933FF11301D0125B40
	.octa  0x3D0D6E6FBE4628763FF172B83C7D5140
	.octa  0x3D053C02DC0144C83FF1D4873168B980
	.octa  0x3D0C3360FD6D8E0B3FF2387A6E756200
	.octa  0x3D009612E8AFAD123FF29E9DF51FDEC0
	.octa  0x3CF52DE8D5A463063FF306FE0A31B700
	.octa  0x3CE54E28AA05E8A93FF371A7373AA9C0
	.octa  0x3D011ADA0911F09F3FF3DEA64C123400
	.octa  0x3D068189B7A04EF83FF44E0860618900
	.octa  0x3D038EA1CBD7F6213FF4BFDAD5362A00
	.octa  0x3CBDF0A83C49D86A3FF5342B569D4F80
	.octa  0x3D04AC64980A8C8F3FF5AB07DD485400
	.octa  0x3CD2C7C3E81BF4B73FF6247EB03A5580
	.octa  0x3CE921165F626CDD3FF6A09E667F3BC0
	.octa  0x3D09EE91B87977853FF71F75E8EC5F40
	.octa  0x3CDB5F54408FDB373FF7A11473EB0180
	.octa  0x3CF28ACF88AFAB353FF82589994CCE00
	.octa  0x3CFB5BA7C55A192D3FF8ACE5422AA0C0
	.octa  0x3D027A280E1F92A03FF93737B0CDC5C0
	.octa  0x3CF01C7C46B071F33FF9C49182A3F080
	.octa  0x3CFC8B424491CAF83FFA5503B23E2540
	.octa  0x3D06AF439A68BB993FFAE89F995AD380
	.octa  0x3CDBAA9EC206AD4F3FFB7F76F2FB5E40
	.octa  0x3CFC2220CB12A0923FFC199BDD855280
	.octa  0x3D048A81E5E8F4A53FFCB720DCEF9040
	.octa  0x3CDC976816BAD9B83FFD5818DCFBA480
	.octa  0x3CFEB968CAC39ED33FFDFC97337B9B40
	.octa  0x3CF9858F73A18F5E3FFEA4AFA2A490C0
	.octa  0x3C99D3E12DD8A18B3FFF50765B6E4540

.align 16
.L__Five_Pont_FiveEMinus17:
    .quad 0x3C90000000000000
.L__B9:
    .quad 0x3E5A2836AA646B96
.L__B8:
    .quad 0x3E928295484734EA
.L__B7:
    .quad 0x3EC71E14BFE3DB59
.L__B6:
    .quad 0x3EFA019F635825C4
.L__B5:
    .quad 0x3F2A01A01159DD2D
.L__B4:
    .quad 0x3F56C16C16CE14C6
.L__B3:
    .quad 0x3F8111111111A9F3
.L__B2:
    .quad 0x3FA55555555554B6
.L__B1:
    .quad 0x3FC5555555555549
.L__TwopM7:
    .quad 0x3F80000000000000

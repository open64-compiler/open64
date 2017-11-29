
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
#define fname FN_PROTOTYPE(expm1f)
#define fname_special _expm1f_special@PLT

#ifdef __ELF__
    .section .note.GNU-stack,"",@progbits
#endif

	.text
	.p2align 4
.globl fname
	.type	fname, @function

fname:
    ucomiss .L__max_expm1_arg(%rip),%xmm0         ##if(x > max_expm1_arg)
    ja .L__Max_Arg
    jp .L__Max_Arg
    ucomiss .L__log_OnePlus_OneByFour(%rip),%xmm0 ##if(x < log_OnePlus_OneByFour)
    jae .L__Normal_Flow
    ucomiss .L__log_OneMinus_OneByFour(%rip),%xmm0 ##if(x > log_OneMinus_OneByFour)
    ja .L__Small_Arg
    ucomiss .L__min_expm1_arg(%rip),%xmm0         ##if(x < min_expm1_arg)
    jb .L__Min_Arg
    
    .p2align 4
.L__Normal_Flow:
    movaps %xmm0,%xmm1     #xmm1 = x
    mulss .L__thirtyTwo_by_ln2(%rip),%xmm1   #xmm1 = x*thirtyTwo_by_ln2
    movd %xmm1,%eax        #eax = x*thirtyTwo_by_ln2
    and $0x80000000,%eax   #get the sign of x*thirtyTwo_by_ln2
    or  $0x3F000000,%eax   #make +/- 0.5    
    movd %eax,%xmm2        #xmm2 = +/- 0.5
    addss %xmm2,%xmm1      #xmm1 = (x*32/ln2) +/- 0.5        
	cvttps2dq %xmm1,%xmm2  #xmm2 = n = (int)(temp)
	mov $0x0000001f,%edx
	movd %edx,%xmm1
	andps %xmm2,%xmm1      #xmm1 = j
    movd %xmm2,%ecx        #ecx = n	
	sarl	$5, %ecx       #ecx = m = n >> 5    
	#xor %rdx,%rdx         #make it zeros, to be used for address	
	movd %xmm1,%edx        #edx = j
	lea  S_lead_and_trail_table(%rip),%rax	
	movsd  (%rax,%rdx,8),%xmm3 #xmm3 = S_T,S_L
    punpckldq %xmm2,%xmm1  #xmm1 = n,j    	
	psubd %xmm1,%xmm2      #xmm2 = n1
    punpcklqdq %xmm2,%xmm1 #xmm1 = n1,n,j    
	cvtdq2ps %xmm1,%xmm1   #xmm1 = (float)(n1,n,j)
		
	#r2 = -(n*ln2_by_ThirtyTwo_trail);
    #r1 = (x-n1*ln2_by_ThirtyTwo_lead) - j*ln2_by_ThirtyTwo_lead;	
    mulps .L__Ln2By32_LeadTrailLead(%rip),%xmm1
    movhlps %xmm1,%xmm2    #xmm2 = n1*ln2/32lead
    movaps %xmm0,%xmm4     #xmm4 = x
    subss %xmm2,%xmm4      #xmm4 = x - n1*ln2/32lead
    subss %xmm1,%xmm4      #xmm4 = r1
    psrldq $4,%xmm1        #xmm1 = -r2 should take care of sign later
    
    #r = r1 + r2;
    movaps %xmm4,%xmm7     #xmm7 = r1   
    subss %xmm1,%xmm4      #xmm4 = r = r1-(-r2) = r1 + r2
    
    #q = r*r*(B1+r*(B2));
    movaps %xmm4,%xmm6         #xmm6 = r
    mulss .L__B2_f(%rip),%xmm6 #xmm6 = r * B2
    addss .L__B1_f(%rip),%xmm6 #xmm6 = B1 + (r * B2)
    mulss %xmm4,%xmm6
    mulss %xmm4,%xmm6          #xmm6 = q    
    
    #p = (r2+q) + r1;
    subss %xmm1,%xmm6
    addss %xmm7,%xmm6          #xmm6 = p

    #s = S_L.f32 + S_T.f32;    
    movdqa %xmm3,%xmm2     #xmm2 = S_T,S_L
    psrldq $4,%xmm2        #xmm2 =     S_T
    movaps %xmm2,%xmm5     #xmm5 =     S_T
    addss %xmm3,%xmm2      #xmm2 = s    
    
    cmp  $0xfffffff9,%ecx  #Check m < -7
    jl .L__M_Below_Minus7
	cmp $23,%ecx           #Check m > 23
	jg .L__M_Above_23
	# -8 < m < 24
    #twopm.f32 * ((S_L.f32 - twopmm.f32) + (S_L.f32*p+ S_T.f32 *(1+p)));
    movaps %xmm3,%xmm2   #xmm2 = S_L
    mulss %xmm6,%xmm2     #xmm2 = S_L * p
    addss .L__One_f(%rip),%xmm6   #xmm6 = 1+p
    mulss %xmm5,%xmm6     #xmm6 = S_T *(1+p)
    addss %xmm6,%xmm2     #xmm2 = (S_L.f32*p+ S_T.f32 *(1+p))
    mov $127,%eax
    sub %ecx,%eax          #eax = 127 - m
	shl  $23,%eax          #eax = 2^-m    
    movd %eax,%xmm1    
    subss %xmm1,%xmm3     #xmm3 = (S_L.f32 - twopmm.f32)
    addss %xmm3,%xmm2     #xmm2 = ((S_L.f32 - twopmm.f32) + (S_L.f32*p+ S_T.f32 *(1+p)))   
    shl  $23,%ecx
    movd %ecx,%xmm0
    paddd %xmm2,%xmm0
    ret     
   
    .p2align 4
.L__M_Below_Minus7:
    #twopm.f32 * (S_L.f32 + (s*p + S_T.f32)) - 1;
    mulss %xmm6,%xmm2     #xmm2 = s*p
    addss %xmm5,%xmm2     #xmm2 = s*p + S_T
    addss %xmm3,%xmm2     #xmm2 = (S_L.f32 + (s*p + S_T.f32))
    shl  $23,%ecx  
    movd %ecx,%xmm0
    paddd %xmm2,%xmm0
    subss .L__One_f(%rip),%xmm0            
    ret  
            
    .p2align 4
.L__M_Above_23:
    #twopm.f32 * (S_L.f32 + (s*p+(S_T.f32 - twopmm.f32)));
    cmp  $0x00000080,%ecx  #Check m < 128    
    je .L__M_Equals_128        
    cmp  $47,%ecx          #Check m > 47
    ja .L__M_Above_47        
    mov $127,%eax
    sub %ecx,%eax          #eax = 127 - m
	shl  $23,%eax          #eax = 2^-m    
    movd %eax,%xmm1
    subss %xmm1,%xmm5      #xmm5 = S_T.f32 - twopmm.f32
    
    .p2align 4
.L__M_Above_47:    
    shl  $23,%ecx    
    mulss %xmm6,%xmm2      #xmm2 = s*p
    addss %xmm5,%xmm2
    addss %xmm3,%xmm2
    movd %ecx,%xmm0
    paddd %xmm2,%xmm0
    ret    
        
    .p2align 4	
.L__M_Equals_128:
    mov $0x3f800000,%ecx  #127 at exponent
    mulss %xmm6,%xmm2     #xmm2 = s*p
    addss %xmm5,%xmm2     #xmm2 = s*p + S_T
    addss %xmm3,%xmm2     #xmm2 = (S_L.f32 + (s*p + S_T.f32))
    movd %ecx,%xmm1       #127
    paddd %xmm2,%xmm1     #2^127*(S_L.f32 + (s*p + S_T.f32))
    mov $0x00800000,%ecx  #multiply with one more 2
    movd %ecx,%xmm2
    paddd %xmm2,%xmm1
    movd %xmm1,%ecx
    and $0x7f800000,%ecx  #check if we reached +inf
    cmp $0x7f800000,%ecx
    je .L__Overflow
    movdqa %xmm1,%xmm0
    ret	
	
	.p2align 4
.L__Small_Arg:
    movd %xmm0,%eax
    and $0x7fffffff,%eax    #eax = abs(x)
    cmp $0x33000000,%eax    #check abs(x) < 2^-25
    jl .L__VeryTiny_Arg
    #log(1-1/4) < x < log(1+1/4)
	#q = x*x*x*(A1 + x*(A2 + x*(A3 + x*(A4 + x*(A5)))));
	movdqa %xmm0,%xmm1
	mulss .L__A5_f(%rip),%xmm1
	addss .L__A4_f(%rip),%xmm1
	mulss %xmm0,%xmm1
	addss .L__A3_f(%rip),%xmm1
	mulss %xmm0,%xmm1
	addss .L__A2_f(%rip),%xmm1
	mulss %xmm0,%xmm1
	addss .L__A1_f(%rip),%xmm1
	mulss %xmm0,%xmm1
	mulss %xmm0,%xmm1
	mulss %xmm0,%xmm1
	cvtps2pd %xmm0,%xmm2
	movdqa %xmm2,%xmm0
	mulsd %xmm0,%xmm2
	mulsd .L__PointFive(%rip),%xmm2
	addsd %xmm2,%xmm0
	cvtps2pd %xmm1,%xmm2
	addsd %xmm0,%xmm2
	cvtpd2ps %xmm2,%xmm0	    
    ret
    
	.p2align 4    
.L__Min_Arg:
    mov $0xBF800000,%eax
    #call handle_error
    movd %eax,%xmm0
    ret    
    
	.p2align 4    
.L__Max_Arg:
    movd %xmm0,%eax
    and $0x7fffffff,%eax    #eax = abs(x)
    cmp $0x7f800000,%eax    #check for Nan
    jae .L__Nan
.L__Overflow:
    mov $0x7f800000,%eax
    #call handle_error
    movd %eax,%xmm0
    ret    
.L__Nan:
    and $0x007fffff,%eax
    je .L__Overflow
    addss %xmm0,%xmm0        
    ret    

    .p2align 4
.L__VeryTiny_Arg:
    #((twopm.f32 * x + xabs.f32) * twopmm.f32);
	movd    %eax, %xmm1                                 #xmm1 = abs(x)																		
	mov		$0x32000000, %eax 							#100 at exponent's place
	movd	%eax, %xmm2
	paddd	%xmm2, %xmm0								
	addss	%xmm1, %xmm0
	psubd   %xmm2, %xmm0
	ret	    
    
.data
.align 16
.type	S_lead_and_trail_table, @object
.size	S_lead_and_trail_table, 256
S_lead_and_trail_table:
	.quad	0x000000003F800000
	.quad	0x355315853F82CD80
	.quad	0x34D9F3123F85AAC0
	.quad	0x35E8092E3F889800
	.quad	0x3471F5463F8B95C0
	.quad	0x36E62D173F8EA400
	.quad	0x361B9D593F91C3C0
	.quad	0x36BEA3FC3F94F4C0
	.quad	0x36C146373F9837C0
	.quad	0x36E6E7553F9B8D00
	.quad	0x36C982473F9EF500
	.quad	0x34C0C3123FA27040
	.quad	0x36354D8B3FA5FEC0
	.quad	0x3655A7543FA9A140
	.quad	0x36FBA90B3FAD5800
	.quad	0x36D6074B3FB123C0
	.quad	0x36CCCFE73FB504C0
	.quad	0x36BD1D8C3FB8FB80
	.quad	0x368E7D603FBD0880
	.quad	0x35CCA6673FC12C40
	.quad	0x36A845543FC56700
	.quad	0x36F619B93FC9B980
	.quad	0x35C151F83FCE2480
	.quad	0x366C8F893FD2A800
	.quad	0x36F32B5A3FD744C0
	.quad	0x36DE5F6C3FDBFB80
	.quad	0x367761553FE0Ccc0
	.quad	0x355CEF903FE5B900
	.quad	0x355CFBA53FEAC0c0
	.quad	0x36E66F733FEFE480
	.quad	0x36F454923FF52540
	.quad	0x36CB6DC93FFA8380

.align 16
.L__Ln2By32_LeadTrailLead:
    .octa 0x333FBE8E3CB17200333FBE8E3CB17200
    
.L__max_expm1_arg:
   .long 0x42B19999
.L__log_OnePlus_OneByFour:
   .long 0x3E647FBF
  
.L__log_OneMinus_OneByFour:
   .long 0xBE934B11
       
.L__min_expm1_arg:
   .long 0xC18AA122
   
.L__thirtyTwo_by_ln2:  
   .long 0x4238AA3B
  
.align 16      
.L__B2_f:
   .long 0x3E2AAAEC      
.L__B1_f:
   .long 0x3F000044
.L__One_f:
   .long 0x3F800000  
.L__PointFive:
   .quad 0x3FE0000000000000

.align 16
.L__A1_f:
   .long 0x3E2AAAAA   
.L__A2_f:
   .long 0x3D2AAAA0
.L__A3_f:  
   .long 0x3C0889FF     
.L__A4_f:
   .long 0x3AB64DE5
.L__A5_f:
   .long 0x394AB327
    
    

    


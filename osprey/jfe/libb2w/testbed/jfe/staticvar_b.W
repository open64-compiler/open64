 LOC 0 0 source files:	1	"/home/xc5/alios-tes/staticvar_b.cxx"
FUNC_ENTRY <1,50,_Z3foov> {line: 1/4}
BODY
 BLOCK {line: 0/0}
 END_BLOCK
 BLOCK {line: 0/0}
 END_BLOCK
 BLOCK {line: 1/4}
 PRAGMA 0 119 <null-st> 0 (0x0) # PREAMBLE_END {line: 1/4}
  I4INTCONST 0 (0x0)
 I4STID 0 <2,4,a> T<4,.predef_I4,4> {line: 1/5}
   I4I4LDID 0 <2,4,a> T<4,.predef_I4,4>
   I4I4LDID 0 <1,51,MyVar1> T<4,.predef_I4,4>
  I4ADD
 I4STID 0 <2,4,a> T<4,.predef_I4,4> {line: 1/6}
 RETURN {line: 1/6}
 END_BLOCK

=======================================================================
SYMTAB for _Z3foov: level 2, st 4, label 0, preg 0, inito 1, st_attr 0
=======================================================================

Symbols:
[1]: __EH_INFO_PER_PU__	<2,1> Variable of type __EH_INFO_PER_PU__ (#53, KIND_ARRAY)
		Address: 0(__EH_INFO_PER_PU__<2,1>)  Alignment: 1 bytes
		location: file (null), line 0
		Flags:	0x00000008 initialized		Flags_ext:	0x00000001 one_per_pu, XLOCAL
		Sclass: EH_REGION_SUPP
[2]: __Exc_Ptr__   	<2,2> Variable of type .predef_U8 (#9, U8)
		Address: 0(__Exc_Ptr__<2,2>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000		Flags_ext:	0x00000001 one_per_pu, XLOCAL
		Sclass: AUTO
[3]: __Exc_Filter__	<2,3> Variable of type .predef_U8 (#9, U8)
		Address: 0(__Exc_Filter__<2,3>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000		Flags_ext:	0x00000001 one_per_pu, XLOCAL
		Sclass: AUTO
[4]: a             	<2,4> Variable of type .predef_I4 (#4, I4)
		Address: 0(a<2,4>)  Alignment: 4 bytes
		location: file (null), line 5
		Flags:	0x00000000, XLOCAL
		Sclass: AUTO
=======================================================================
Labels:
=======================================================================
Pseudo-registers:
=======================================================================
INITOs:
[1]: __EH_INFO_PER_PU__ (0x102):
 VAL: 514
 VAL: 770
 VAL: 0
 VAL: 0
=======================================================================
ST_ATTRs:
=======================================================================

=======================================================================
GLOBAL SYMTAB:
=======================================================================
File Info:
gp_group: 0, flags: 0x00000000
=======================================================================
Symbols:
[1]: .preg_B       	<1,1> Pseudo-Register of type .predef_B (#1, B)
		Address: 0(.preg_B<1,1>)  Alignment: 1 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[2]: .preg_I1      	<1,2> Pseudo-Register of type .predef_I1 (#2, I1)
		Address: 0(.preg_I1<1,2>)  Alignment: 1 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[3]: .preg_I2      	<1,3> Pseudo-Register of type .predef_I2 (#3, I2)
		Address: 0(.preg_I2<1,3>)  Alignment: 2 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[4]: .preg_I4      	<1,4> Pseudo-Register of type .predef_I4 (#4, I4)
		Address: 0(.preg_I4<1,4>)  Alignment: 4 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[5]: .preg_I8      	<1,5> Pseudo-Register of type .predef_I8 (#5, I8)
		Address: 0(.preg_I8<1,5>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[6]: .preg_U1      	<1,6> Pseudo-Register of type .predef_U1 (#6, U1)
		Address: 0(.preg_U1<1,6>)  Alignment: 1 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[7]: .preg_U2      	<1,7> Pseudo-Register of type .predef_U2 (#7, U2)
		Address: 0(.preg_U2<1,7>)  Alignment: 2 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[8]: .preg_U4      	<1,8> Pseudo-Register of type .predef_U4 (#8, U4)
		Address: 0(.preg_U4<1,8>)  Alignment: 4 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[9]: .preg_U8      	<1,9> Pseudo-Register of type .predef_U8 (#9, U8)
		Address: 0(.preg_U8<1,9>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[10]: .preg_F4      	<1,10> Pseudo-Register of type .predef_F4 (#10, F4)
		Address: 0(.preg_F4<1,10>)  Alignment: 4 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[11]: .preg_F8      	<1,11> Pseudo-Register of type .predef_F8 (#11, F8)
		Address: 0(.preg_F8<1,11>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[12]: .preg_F10     	<1,12> Pseudo-Register of type .predef_F10 (#12, F10)
		Address: 0(.preg_F10<1,12>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[13]: .preg_F16     	<1,13> Pseudo-Register of type .predef_F16 (#13, F16)
		Address: 0(.preg_F16<1,13>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[14]: .preg_FQ      	<1,14> Pseudo-Register of type .predef_FQ (#15, FQ)
		Address: 0(.preg_FQ<1,14>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[15]: .preg_C4      	<1,15> Pseudo-Register of type .predef_C4 (#17, C4)
		Address: 0(.preg_C4<1,15>)  Alignment: 4 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[16]: .preg_C8      	<1,16> Pseudo-Register of type .predef_C8 (#18, C8)
		Address: 0(.preg_C8<1,16>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[17]: .preg_CQ      	<1,17> Pseudo-Register of type .predef_CQ (#19, CQ)
		Address: 0(.preg_CQ<1,17>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[18]: .preg_BS      	<1,18> Pseudo-Register of type .predef_BS (#21, BS)
		Address: 0(.preg_BS<1,18>)  Alignment: 1 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[19]: .preg_A8      	<1,19> Pseudo-Register of type .predef_A8 (#22, KIND_VOID*)
		Address: 0(.preg_A8<1,19>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[20]: .preg_C10     	<1,20> Pseudo-Register of type .predef_C10 (#23, C10)
		Address: 0(.preg_C10<1,20>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[21]: .preg_C16     	<1,21> Pseudo-Register of type .predef_C16 (#24, C16)
		Address: 0(.preg_C16<1,21>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[22]: .preg_I16     	<1,22> Pseudo-Register of type .predef_I16 (#25, I16)
		Address: 0(.preg_I16<1,22>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[23]: .preg_U16     	<1,23> Pseudo-Register of type .predef_U16 (#26, U16)
		Address: 0(.preg_U16<1,23>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[24]: .preg_V16C4   	<1,24> Pseudo-Register of type .predef_V16C4 (#27, V16C4)
		Address: 0(.preg_V16C4<1,24>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[25]: .preg_V16C8   	<1,25> Pseudo-Register of type .predef_V16C8 (#28, V16C8)
		Address: 0(.preg_V16C8<1,25>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[26]: .preg_V16I1   	<1,26> Pseudo-Register of type .predef_V16I1 (#29, V16I1)
		Address: 0(.preg_V16I1<1,26>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[27]: .preg_V16I2   	<1,27> Pseudo-Register of type .predef_V16I2 (#30, V16I2)
		Address: 0(.preg_V16I2<1,27>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[28]: .preg_V16I4   	<1,28> Pseudo-Register of type .predef_V16I4 (#31, V16I4)
		Address: 0(.preg_V16I4<1,28>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[29]: .preg_V16I8   	<1,29> Pseudo-Register of type .predef_V16I8 (#32, V16I8)
		Address: 0(.preg_V16I8<1,29>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[30]: .preg_V16F4   	<1,30> Pseudo-Register of type .predef_V16F4 (#33, V16F4)
		Address: 0(.preg_V16F4<1,30>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[31]: .preg_V16F8   	<1,31> Pseudo-Register of type .predef_V16F8 (#34, V16F8)
		Address: 0(.preg_V16F8<1,31>)  Alignment: 16 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[32]: .preg_V8I1    	<1,32> Pseudo-Register of type .predef_V8I1 (#35, V8I1)
		Address: 0(.preg_V8I1<1,32>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[33]: .preg_V8I2    	<1,33> Pseudo-Register of type .predef_V8I2 (#36, V8I2)
		Address: 0(.preg_V8I2<1,33>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[34]: .preg_V8I4    	<1,34> Pseudo-Register of type .predef_V8I4 (#37, V8I4)
		Address: 0(.preg_V8I4<1,34>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[35]: .preg_V8I8    	<1,35> Pseudo-Register of type .predef_V8I8 (#38, V8I8)
		Address: 0(.preg_V8I8<1,35>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[36]: .preg_V8F4    	<1,36> Pseudo-Register of type .predef_V8F4 (#39, V8F4)
		Address: 0(.preg_V8F4<1,36>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[37]: .preg_M8I1    	<1,37> Pseudo-Register of type .predef_M8I1 (#40, M8I1)
		Address: 0(.preg_M8I1<1,37>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[38]: .preg_M8I2    	<1,38> Pseudo-Register of type .predef_M8I2 (#41, M8I2)
		Address: 0(.preg_M8I2<1,38>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[39]: .preg_M8I4    	<1,39> Pseudo-Register of type .predef_M8I4 (#42, M8I4)
		Address: 0(.preg_M8I4<1,39>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[40]: .preg_M8F4    	<1,40> Pseudo-Register of type .predef_M8F4 (#43, M8F4)
		Address: 0(.preg_M8F4<1,40>)  Alignment: 8 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[41]: .preg_V32C4   	<1,41> Pseudo-Register of type .predef_V32C4 (#44, V32C4)
		Address: 0(.preg_V32C4<1,41>)  Alignment: 32 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[42]: .preg_V32C8   	<1,42> Pseudo-Register of type .predef_V32C8 (#45, V32C8)
		Address: 0(.preg_V32C8<1,42>)  Alignment: 32 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[43]: .preg_V32I1   	<1,43> Pseudo-Register of type .predef_V32I1 (#46, V32I1)
		Address: 0(.preg_V32I1<1,43>)  Alignment: 32 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[44]: .preg_V32I2   	<1,44> Pseudo-Register of type .predef_V32I2 (#47, V32I2)
		Address: 0(.preg_V32I2<1,44>)  Alignment: 32 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[45]: .preg_V32I4   	<1,45> Pseudo-Register of type .predef_V32I4 (#48, V32I4)
		Address: 0(.preg_V32I4<1,45>)  Alignment: 32 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[46]: .preg_V32I8   	<1,46> Pseudo-Register of type .predef_V32I8 (#49, V32I8)
		Address: 0(.preg_V32I8<1,46>)  Alignment: 32 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[47]: .preg_V32F4   	<1,47> Pseudo-Register of type .predef_V32F4 (#50, V32F4)
		Address: 0(.preg_V32F4<1,47>)  Alignment: 32 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[48]: .preg_V32F8   	<1,48> Pseudo-Register of type .predef_V32F8 (#51, V32F8)
		Address: 0(.preg_V32F8<1,48>)  Alignment: 32 bytes
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[49]: .preg_return_val	<1,49> Pseudo-Register
		Address: 0(.preg_return_val<1,49>)  
		location: file (null), line 0
		Flags:	0x00000000, XLOCAL
		Sclass: REG
[50]: _Z3foov       	<1,50> Subprogram
		Returning .predef_V (#20, KIND_VOID)  PU[1] C++  flags: prototype
		Address: 0(_Z3foov<1,50>)  Alignment: 1 bytes
		location: file (null), line 0
		Flags:	0x00000000, XPREEMPTIBLE
		Sclass: TEXT
[51]: MyVar1        	<1,51> Variable of type .predef_I4 (#4, I4)
		Address: 0(MyVar1<1,51>)  Alignment: 4 bytes
		location: file (null), line 1
		Flags:	0x00000008 initialized, XLOCAL
		Sclass: FSTATIC
=======================================================================
PUs:
[1]: (anon) (#52) align 1, flags 0x0004000000000000,
	lexical level 2, LANG 0x04, TARGET_INFO 0,
	Misc. Info (misc) 258
=======================================================================
Types:
[1]: .predef_B     : (f: 0x0000) size 0 B: SCALAR (B)
[2]: .predef_I1    : (f: 0x0000) size 1 I1: SCALAR (I1)
[3]: .predef_I2    : (f: 0x0000) size 2 I2: SCALAR (I2)
[4]: .predef_I4    : (f: 0x0000) size 4 I4: SCALAR (I4)
[5]: .predef_I8    : (f: 0x0000) size 8 I8: SCALAR (I8)
[6]: .predef_U1    : (f: 0x0000) size 1 U1: SCALAR (U1)
[7]: .predef_U2    : (f: 0x0000) size 2 U2: SCALAR (U2)
[8]: .predef_U4    : (f: 0x0000) size 4 U4: SCALAR (U4)
[9]: .predef_U8    : (f: 0x0000) size 8 U8: SCALAR (U8)
[10]: .predef_F4    : (f: 0x0000) size 4 F4: SCALAR (F4)
[11]: .predef_F8    : (f: 0x0000) size 8 F8: SCALAR (F8)
[12]: .predef_F10   : (f: 0x0000) size 16 F10: SCALAR (F10)
[13]: .predef_F16   : (f: 0x0000) size 16 F16: SCALAR (F16)
[14]: .predef_STR   : (f: 0x0000) size 0 STR: SCALAR (STR)
[15]: .predef_FQ    : (f: 0x0000) size 16 FQ: SCALAR (FQ)
[16]: .predef_M     : (f: 0x0000) size 0 M: SCALAR (M)
[17]: .predef_C4    : (f: 0x0000) size 8 C4: SCALAR (C4)
[18]: .predef_C8    : (f: 0x0000) size 16 C8: SCALAR (C8)
[19]: .predef_CQ    : (f: 0x0000) size 32 CQ: SCALAR (CQ)
[20]: .predef_V     : (f: 0x0000) size 0 V: VOID
[21]: .predef_BS    : (f: 0x0000) size 0 BS: SCALAR (BS)
[22]: .predef_A8    : (f: 0x0000) size 8 A8: -> .predef_V (#20) align 1
[23]: .predef_C10   : (f: 0x0000) size 32 C10: SCALAR (C10)
[24]: .predef_C16   : (f: 0x0000) size 32 C16: SCALAR (C16)
[25]: .predef_I16   : (f: 0x0000) size 32 I16: SCALAR (I16)
[26]: .predef_U16   : (f: 0x0000) size 32 U16: SCALAR (U16)
[27]: .predef_V16C4 : (f: 0x0000) size 16 V16C4: SCALAR (V16C4)
[28]: .predef_V16C8 : (f: 0x0000) size 16 V16C8: SCALAR (V16C8)
[29]: .predef_V16I1 : (f: 0x0000) size 16 V16I1: SCALAR (V16I1)
[30]: .predef_V16I2 : (f: 0x0000) size 16 V16I2: SCALAR (V16I2)
[31]: .predef_V16I4 : (f: 0x0000) size 16 V16I4: SCALAR (V16I4)
[32]: .predef_V16I8 : (f: 0x0000) size 16 V16I8: SCALAR (V16I8)
[33]: .predef_V16F4 : (f: 0x0000) size 16 V16F4: SCALAR (V16F4)
[34]: .predef_V16F8 : (f: 0x0000) size 16 V16F8: SCALAR (V16F8)
[35]: .predef_V8I1  : (f: 0x0000) size 8 V8I1: SCALAR (V8I1)
[36]: .predef_V8I2  : (f: 0x0000) size 8 V8I2: SCALAR (V8I2)
[37]: .predef_V8I4  : (f: 0x0000) size 8 V8I4: SCALAR (V8I4)
[38]: .predef_V8I8  : (f: 0x0000) size 8 V8I8: SCALAR (V8I8)
[39]: .predef_V8F4  : (f: 0x0000) size 8 V8F4: SCALAR (V8F4)
[40]: .predef_M8I1  : (f: 0x0000) size 8 M8I1: SCALAR (M8I1)
[41]: .predef_M8I2  : (f: 0x0000) size 8 M8I2: SCALAR (M8I2)
[42]: .predef_M8I4  : (f: 0x0000) size 8 M8I4: SCALAR (M8I4)
[43]: .predef_M8F4  : (f: 0x0000) size 8 M8F4: SCALAR (M8F4)
[44]: .predef_V32C4 : (f: 0x0000) size 32 V32C4: SCALAR (V32C4)
[45]: .predef_V32C8 : (f: 0x0000) size 32 V32C8: SCALAR (V32C8)
[46]: .predef_V32I1 : (f: 0x0000) size 32 V32I1: SCALAR (V32I1)
[47]: .predef_V32I2 : (f: 0x0000) size 32 V32I2: SCALAR (V32I2)
[48]: .predef_V32I4 : (f: 0x0000) size 32 V32I4: SCALAR (V32I4)
[49]: .predef_V32I8 : (f: 0x0000) size 32 V32I8: SCALAR (V32I8)
[50]: .predef_V32F4 : (f: 0x0000) size 32 V32F4: SCALAR (V32F4)
[51]: .predef_V32F8 : (f: 0x0000) size 32 V32F8: SCALAR (V32F8)
[52]: (anon)        : (f: 0x0000) size 0 : FUNCTION (f: 0x0004)
	returns .predef_V (#20) align 1
[53]: __EH_INFO_PER_PU__: (f: 0x0000) size 16 M: ARRAY of .predef_U4 (#8) align 4 (0:3:4:)
[54]: anon_ptr.     : (f: 0x0000) size 8 U8: -> .predef_I4 (#4) align 4
=======================================================================
Blocks:
=======================================================================
Tcons:
[1] F4:     0.000000
[2] F8:     0.000000000000000
[3] U4: 514
[4] U4: 770
[5] U4: 0
[6] U4: 0
[7] I4: 98765432
=======================================================================
INITOs:
[1]: MyVar1 (0x3301):
 VAL: 98765432
=======================================================================
ST_ATTRs:
=======================================================================
String table size = 1225
=======================================================================


------------ INCLUDE_DIRECTORIES ------------

[1,0] ordinal(1) path(/home/xc5/alios-tes)

------------<<<<<<< FILES >>>>>>>------------

[2,0] ordinal(1) name(staticvar_b.cxx) path(1) size(197) modt(1535131331)

------------<<<<<< DST INFO >>>>>------------

[0,0]:compile_unit: name(staticvar_b.cxx) producer(openCC 5.0) language(C_plus_plus) case(case_sensitive)

  [0,120]:subprogram: file(1) line(4) column(0) name(foo) linkage_name(_Z3foov) external type(void) specification(void) inline(DW_INL_not_inlined) virtuality(DW_VIRTUALITY_none) vtable_elem_location(0) pc(ST(1,50))
------------<<<< BLOCK INFO >>>------------
block 0:  file_scope, size = 256
block 1:  include_dirs, size = 36
block 2:  file_names, size = 56
block 3:  file_scope, size = 12

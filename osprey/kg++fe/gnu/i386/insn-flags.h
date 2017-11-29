/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/* Generated automatically by the program `genflags'
   from the machine description file `md'.  */

#ifndef GCC_INSN_FLAGS_H
#define GCC_INSN_FLAGS_H

#define HAVE_cmpdi_ccno_1_rex64 (TARGET_64BIT && ix86_match_ccmode (insn, CCNOmode))
#define HAVE_cmpdi_1_insn_rex64 (TARGET_64BIT && ix86_match_ccmode (insn, CCmode))
#define HAVE_cmpqi_ext_3_insn (!TARGET_64BIT && ix86_match_ccmode (insn, CCmode))
#define HAVE_cmpqi_ext_3_insn_rex64 (TARGET_64BIT && ix86_match_ccmode (insn, CCmode))
#define HAVE_x86_fnstsw_1 (TARGET_80387)
#define HAVE_x86_sahf_1 (!TARGET_64BIT)
#define HAVE_popsi1 (!TARGET_64BIT)
#define HAVE_movsi_insv_1 (!TARGET_64BIT)
#define HAVE_pushdi2_rex64 (TARGET_64BIT)
#define HAVE_popdi1 (TARGET_64BIT)
#define HAVE_swapxf 1
#define HAVE_swaptf 1
#define HAVE_zero_extendhisi2_and (TARGET_ZERO_EXTEND_WITH_AND && !optimize_size)
#define HAVE_zero_extendsidi2_32 (!TARGET_64BIT)
#define HAVE_zero_extendsidi2_rex64 (TARGET_64BIT)
#define HAVE_zero_extendhidi2 (TARGET_64BIT)
#define HAVE_zero_extendqidi2 (TARGET_64BIT)
#define HAVE_extendsidi2_rex64 (TARGET_64BIT)
#define HAVE_extendhidi2 (TARGET_64BIT)
#define HAVE_extendqidi2 (TARGET_64BIT)
#define HAVE_extendhisi2 1
#define HAVE_extendqihi2 1
#define HAVE_extendqisi2 1
#define HAVE_truncdfsf2_3 (TARGET_80387)
#define HAVE_truncdfsf2_sse_only (!TARGET_80387 && TARGET_SSE2)
#define HAVE_fix_truncdi_nomemory (TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1])) \
   && (!SSE_FLOAT_MODE_P (GET_MODE (operands[1])) || !TARGET_64BIT))
#define HAVE_fix_truncdi_memory (TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1])) \
   && (!SSE_FLOAT_MODE_P (GET_MODE (operands[1])) || !TARGET_64BIT))
#define HAVE_fix_truncsfdi_sse (TARGET_64BIT && TARGET_SSE)
#define HAVE_fix_truncdfdi_sse (TARGET_64BIT && TARGET_SSE2)
#define HAVE_fix_truncsi_nomemory (TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1])) \
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[1])))
#define HAVE_fix_truncsi_memory (TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1])) \
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[1])))
#define HAVE_fix_truncsfsi_sse (TARGET_SSE)
#define HAVE_fix_truncdfsi_sse (TARGET_SSE2)
#define HAVE_fix_trunchi_nomemory (TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1])) \
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[1])))
#define HAVE_fix_trunchi_memory (TARGET_80387 && FLOAT_MODE_P (GET_MODE (operands[1])) \
   && !SSE_FLOAT_MODE_P (GET_MODE (operands[1])))
#define HAVE_x86_fnstcw_1 (TARGET_80387)
#define HAVE_x86_fldcw_1 (TARGET_80387)
#define HAVE_floathisf2 (TARGET_80387 && !TARGET_SSE)
#define HAVE_floathidf2 (TARGET_80387 && !TARGET_SSE2)
#define HAVE_floathixf2 (!TARGET_64BIT && TARGET_80387)
#define HAVE_floathitf2 (TARGET_80387)
#define HAVE_floatsixf2 (!TARGET_64BIT && TARGET_80387)
#define HAVE_floatsitf2 (TARGET_80387)
#define HAVE_floatdixf2 (!TARGET_64BIT && TARGET_80387)
#define HAVE_floatditf2 (TARGET_80387)
#define HAVE_addqi3_cc (ix86_binary_operator_ok (PLUS, QImode, operands))
#define HAVE_addsi_1_zext (TARGET_64BIT && ix86_binary_operator_ok (PLUS, SImode, operands))
#define HAVE_addqi_ext_1 (!TARGET_64BIT)
#define HAVE_subdi3_carry_rex64 (TARGET_64BIT && ix86_binary_operator_ok (MINUS, DImode, operands))
#define HAVE_subsi3_carry (ix86_binary_operator_ok (MINUS, SImode, operands))
#define HAVE_subsi3_carry_zext (TARGET_64BIT && ix86_binary_operator_ok (MINUS, SImode, operands))
#define HAVE_divqi3 (TARGET_QIMODE_MATH)
#define HAVE_udivqi3 (TARGET_QIMODE_MATH)
#define HAVE_divmodhi4 (TARGET_HIMODE_MATH)
#define HAVE_udivmoddi4 (TARGET_64BIT)
#define HAVE_udivmodsi4 1
#define HAVE_testsi_1 (ix86_match_ccmode (insn, CCNOmode))
#define HAVE_andqi_ext_0 1
#define HAVE_iorqi_ext_0 ((!TARGET_PARTIAL_REG_STALL || optimize_size))
#define HAVE_xorqi_ext_0 ((!TARGET_PARTIAL_REG_STALL || optimize_size))
#define HAVE_negsf2_memory (ix86_unary_operator_ok (NEG, SFmode, operands))
#define HAVE_negsf2_ifs (TARGET_SSE \
   && (reload_in_progress || reload_completed \
       || (register_operand (operands[0], VOIDmode) \
	   && register_operand (operands[1], VOIDmode))))
#define HAVE_negdf2_memory (ix86_unary_operator_ok (NEG, DFmode, operands))
#define HAVE_negdf2_ifs (!TARGET_64BIT && TARGET_SSE2 \
   && (reload_in_progress || reload_completed \
       || (register_operand (operands[0], VOIDmode) \
	   && register_operand (operands[1], VOIDmode))))
#define HAVE_abssf2_memory (ix86_unary_operator_ok (ABS, SFmode, operands))
#define HAVE_abssf2_ifs (TARGET_SSE \
   && (reload_in_progress || reload_completed \
       || (register_operand (operands[0], VOIDmode) \
	   && register_operand (operands[1], VOIDmode))))
#define HAVE_absdf2_memory (ix86_unary_operator_ok (ABS, DFmode, operands))
#define HAVE_absdf2_ifs (!TARGET_64BIT && TARGET_SSE2 \
   && (reload_in_progress || reload_completed \
       || (register_operand (operands[0], VOIDmode) \
	   && register_operand (operands[1], VOIDmode))))
#define HAVE_ashldi3_1 (!TARGET_64BIT && TARGET_CMOVE)
#define HAVE_x86_shld_1 1
#define HAVE_ashrdi3_63_rex64 (TARGET_64BIT && INTVAL (operands[2]) == 63 && (TARGET_USE_CLTD || optimize_size) \
   && ix86_binary_operator_ok (ASHIFTRT, DImode, operands))
#define HAVE_ashrdi3_1 (!TARGET_64BIT && TARGET_CMOVE)
#define HAVE_x86_shrd_1 1
#define HAVE_ashrsi3_31 (INTVAL (operands[2]) == 31 && (TARGET_USE_CLTD || optimize_size) \
   && ix86_binary_operator_ok (ASHIFTRT, SImode, operands))
#define HAVE_lshrdi3_1 (!TARGET_64BIT && TARGET_CMOVE)
#define HAVE_setcc_2 1
#define HAVE_jump 1
#define HAVE_doloop_end_internal (!TARGET_64BIT && TARGET_USE_LOOP)
#define HAVE_blockage 1
#define HAVE_return_internal (reload_completed)
#define HAVE_return_pop_internal (reload_completed)
#define HAVE_return_indirect_internal (reload_completed)
#define HAVE_nop 1
#define HAVE_set_got (!TARGET_64BIT)
#define HAVE_eh_return_si (!TARGET_64BIT)
#define HAVE_eh_return_di (TARGET_64BIT)
#define HAVE_leave (!TARGET_64BIT)
#define HAVE_leave_rex64 (TARGET_64BIT)
#define HAVE_ffssi_1 1
#define HAVE_sqrtsf2_1 (! TARGET_NO_FANCY_MATH_387 && TARGET_80387 \
   && (TARGET_SSE_MATH && TARGET_MIX_SSE_I387))
#define HAVE_sqrtsf2_1_sse_only (TARGET_SSE_MATH && (!TARGET_80387 || !TARGET_MIX_SSE_I387))
#define HAVE_sqrtsf2_i387 (! TARGET_NO_FANCY_MATH_387 && TARGET_80387 \
   && !TARGET_SSE_MATH)
#define HAVE_sqrtdf2_1 (! TARGET_NO_FANCY_MATH_387 && TARGET_80387 \
   && (TARGET_SSE2 && TARGET_SSE_MATH && TARGET_MIX_SSE_I387))
#define HAVE_sqrtdf2_1_sse_only (TARGET_SSE2 && TARGET_SSE_MATH && (!TARGET_80387 || !TARGET_MIX_SSE_I387))
#define HAVE_sqrtdf2_i387 (! TARGET_NO_FANCY_MATH_387 && TARGET_80387 \
   && (!TARGET_SSE2 || !TARGET_SSE_MATH))
#define HAVE_sqrtxf2 (!TARGET_64BIT && TARGET_80387 && !TARGET_NO_FANCY_MATH_387  \
   && (TARGET_IEEE_FP || flag_unsafe_math_optimizations) )
#define HAVE_sqrttf2 (! TARGET_NO_FANCY_MATH_387 && TARGET_80387  \
   && (TARGET_IEEE_FP || flag_unsafe_math_optimizations) )
#define HAVE_sindf2 (! TARGET_NO_FANCY_MATH_387 && TARGET_80387  \
   && flag_unsafe_math_optimizations)
#define HAVE_sinsf2 (! TARGET_NO_FANCY_MATH_387 && TARGET_80387  \
   && flag_unsafe_math_optimizations)
#define HAVE_sinxf2 (!TARGET_64BIT && TARGET_80387 && !TARGET_NO_FANCY_MATH_387 \
   && flag_unsafe_math_optimizations)
#define HAVE_sintf2 (! TARGET_NO_FANCY_MATH_387 && TARGET_80387  \
   && flag_unsafe_math_optimizations)
#define HAVE_cosdf2 (! TARGET_NO_FANCY_MATH_387 && TARGET_80387  \
   && flag_unsafe_math_optimizations)
#define HAVE_cossf2 (! TARGET_NO_FANCY_MATH_387 && TARGET_80387  \
   && flag_unsafe_math_optimizations)
#define HAVE_cosxf2 (!TARGET_64BIT && ! TARGET_NO_FANCY_MATH_387 && TARGET_80387 \
   && flag_unsafe_math_optimizations)
#define HAVE_costf2 (! TARGET_NO_FANCY_MATH_387 && TARGET_80387  \
   && flag_unsafe_math_optimizations)
#define HAVE_cld 1
#define HAVE_strmovdi_rex_1 (TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_strmovsi_1 (!TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_strmovsi_rex_1 (TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_strmovhi_1 (!TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_strmovhi_rex_1 (TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_strmovqi_1 (!TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_strmovqi_rex_1 (TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_rep_movdi_rex64 (TARGET_64BIT)
#define HAVE_rep_movsi (!TARGET_64BIT)
#define HAVE_rep_movsi_rex64 (TARGET_64BIT)
#define HAVE_rep_movqi (!TARGET_64BIT)
#define HAVE_rep_movqi_rex64 (TARGET_64BIT)
#define HAVE_strsetdi_rex_1 (TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_strsetsi_1 (!TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_strsetsi_rex_1 (TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_strsethi_1 (!TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_strsethi_rex_1 (TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_strsetqi_1 (!TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_strsetqi_rex_1 (TARGET_64BIT && (TARGET_SINGLE_STRINGOP || optimize_size))
#define HAVE_rep_stosdi_rex64 (TARGET_64BIT)
#define HAVE_rep_stossi (!TARGET_64BIT)
#define HAVE_rep_stossi_rex64 (TARGET_64BIT)
#define HAVE_rep_stosqi (!TARGET_64BIT)
#define HAVE_rep_stosqi_rex64 (TARGET_64BIT)
#define HAVE_cmpstrqi_nz_1 (!TARGET_64BIT)
#define HAVE_cmpstrqi_nz_rex_1 (TARGET_64BIT)
#define HAVE_cmpstrqi_1 (!TARGET_64BIT)
#define HAVE_cmpstrqi_rex_1 (TARGET_64BIT)
#define HAVE_strlenqi_1 (!TARGET_64BIT)
#define HAVE_strlenqi_rex_1 (TARGET_64BIT)
#define HAVE_x86_movdicc_0_m1_rex64 (TARGET_64BIT)
#define HAVE_x86_movsicc_0_m1 1
#define HAVE_pro_epilogue_adjust_stack_rex64 (TARGET_64BIT)
#define HAVE_sse_movsfcc (TARGET_SSE \
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM) \
   && (!TARGET_IEEE_FP \
       || (GET_CODE (operands[1]) != EQ && GET_CODE (operands[1]) != NE)))
#define HAVE_sse_movsfcc_eq (TARGET_SSE \
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM))
#define HAVE_sse_movdfcc (TARGET_SSE2 \
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM) \
   && (!TARGET_IEEE_FP \
       || (GET_CODE (operands[1]) != EQ && GET_CODE (operands[1]) != NE)))
#define HAVE_sse_movdfcc_eq (TARGET_SSE \
   && (GET_CODE (operands[2]) != MEM || GET_CODE (operands[3]) != MEM))
#define HAVE_allocate_stack_worker_1 (!TARGET_64BIT && TARGET_STACK_PROBE)
#define HAVE_allocate_stack_worker_rex64 (TARGET_64BIT && TARGET_STACK_PROBE)
#define HAVE_trap 1
#define HAVE_movv4sf_internal (TARGET_SSE)
#define HAVE_movv4si_internal (TARGET_SSE)
#define HAVE_movv2di_internal (TARGET_SSE)
#define HAVE_movv8qi_internal (TARGET_MMX \
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM))
#define HAVE_movv4hi_internal (TARGET_MMX \
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM))
#define HAVE_movv2si_internal (TARGET_MMX \
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM))
#define HAVE_movv2sf_internal (TARGET_3DNOW \
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM))
#define HAVE_movv2df_internal (TARGET_SSE2 \
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM))
#define HAVE_movv8hi_internal (TARGET_SSE2 \
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM))
#define HAVE_movv16qi_internal (TARGET_SSE2 \
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM))
#define HAVE_movti_internal (TARGET_SSE && !TARGET_64BIT \
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM))
#define HAVE_sse_movmskps (TARGET_SSE)
#define HAVE_mmx_pmovmskb (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_mmx_maskmovq ((TARGET_SSE || TARGET_3DNOW_A) && !TARGET_64BIT)
#define HAVE_mmx_maskmovq_rex ((TARGET_SSE || TARGET_3DNOW_A) && TARGET_64BIT)
#define HAVE_sse_movntv4sf (TARGET_SSE)
#define HAVE_sse_movntdi (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_sse_movhlps (TARGET_SSE)
#define HAVE_sse_movlhps (TARGET_SSE)
#define HAVE_sse_movhps (TARGET_SSE \
   && (GET_CODE (operands[1]) == MEM || GET_CODE (operands[2]) == MEM))
#define HAVE_sse_movlps (TARGET_SSE \
   && (GET_CODE (operands[1]) == MEM || GET_CODE (operands[2]) == MEM))
#define HAVE_sse_loadss_1 (TARGET_SSE)
#define HAVE_sse_movss (TARGET_SSE)
#define HAVE_sse_storess (TARGET_SSE)
#define HAVE_sse_shufps (TARGET_SSE)
#define HAVE_addv4sf3 (TARGET_SSE)
#define HAVE_vmaddv4sf3 (TARGET_SSE)
#define HAVE_subv4sf3 (TARGET_SSE)
#define HAVE_vmsubv4sf3 (TARGET_SSE)
#define HAVE_mulv4sf3 (TARGET_SSE)
#define HAVE_vmmulv4sf3 (TARGET_SSE)
#define HAVE_divv4sf3 (TARGET_SSE)
#define HAVE_vmdivv4sf3 (TARGET_SSE)
#define HAVE_rcpv4sf2 (TARGET_SSE)
#define HAVE_vmrcpv4sf2 (TARGET_SSE)
#define HAVE_rsqrtv4sf2 (TARGET_SSE)
#define HAVE_vmrsqrtv4sf2 (TARGET_SSE)
#define HAVE_sqrtv4sf2 (TARGET_SSE)
#define HAVE_vmsqrtv4sf2 (TARGET_SSE)
#define HAVE_sse2_andv2di3 (TARGET_SSE2 \
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM))
#define HAVE_sse2_nandv2di3 (TARGET_SSE2 \
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM))
#define HAVE_sse2_iorv2di3 (TARGET_SSE2 \
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM))
#define HAVE_sse2_xorv2di3 (TARGET_SSE2 \
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM))
#define HAVE_sse_clrv4sf (TARGET_SSE)
#define HAVE_sse_clrv2df (TARGET_SSE2)
#define HAVE_maskcmpv4sf3 (TARGET_SSE)
#define HAVE_maskncmpv4sf3 (TARGET_SSE)
#define HAVE_vmmaskcmpv4sf3 (TARGET_SSE)
#define HAVE_vmmaskncmpv4sf3 (TARGET_SSE)
#define HAVE_sse_comi (TARGET_SSE)
#define HAVE_sse_ucomi (TARGET_SSE)
#define HAVE_sse_unpckhps (TARGET_SSE)
#define HAVE_sse_unpcklps (TARGET_SSE)
#define HAVE_smaxv4sf3 (TARGET_SSE)
#define HAVE_vmsmaxv4sf3 (TARGET_SSE)
#define HAVE_sminv4sf3 (TARGET_SSE)
#define HAVE_vmsminv4sf3 (TARGET_SSE)
#define HAVE_cvtpi2ps (TARGET_SSE)
#define HAVE_cvtps2pi (TARGET_SSE)
#define HAVE_cvttps2pi (TARGET_SSE)
#define HAVE_cvtsi2ss (TARGET_SSE)
#define HAVE_cvtsi2ssq (TARGET_SSE && TARGET_64BIT)
#define HAVE_cvtss2si (TARGET_SSE)
#define HAVE_cvtss2siq (TARGET_SSE)
#define HAVE_cvttss2si (TARGET_SSE)
#define HAVE_cvttss2siq (TARGET_SSE && TARGET_64BIT)
#define HAVE_addv8qi3 (TARGET_MMX)
#define HAVE_addv4hi3 (TARGET_MMX)
#define HAVE_addv2si3 (TARGET_MMX)
#define HAVE_mmx_adddi3 (TARGET_MMX)
#define HAVE_ssaddv8qi3 (TARGET_MMX)
#define HAVE_ssaddv4hi3 (TARGET_MMX)
#define HAVE_usaddv8qi3 (TARGET_MMX)
#define HAVE_usaddv4hi3 (TARGET_MMX)
#define HAVE_subv8qi3 (TARGET_MMX)
#define HAVE_subv4hi3 (TARGET_MMX)
#define HAVE_subv2si3 (TARGET_MMX)
#define HAVE_mmx_subdi3 (TARGET_MMX)
#define HAVE_sssubv8qi3 (TARGET_MMX)
#define HAVE_sssubv4hi3 (TARGET_MMX)
#define HAVE_ussubv8qi3 (TARGET_MMX)
#define HAVE_ussubv4hi3 (TARGET_MMX)
#define HAVE_mulv4hi3 (TARGET_MMX)
#define HAVE_smulv4hi3_highpart (TARGET_MMX)
#define HAVE_umulv4hi3_highpart (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_mmx_pmaddwd (TARGET_MMX)
#define HAVE_mmx_iordi3 (TARGET_MMX)
#define HAVE_mmx_xordi3 (TARGET_MMX)
#define HAVE_mmx_clrdi (TARGET_MMX)
#define HAVE_mmx_anddi3 (TARGET_MMX)
#define HAVE_mmx_nanddi3 (TARGET_MMX)
#define HAVE_mmx_uavgv8qi3 (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_mmx_uavgv4hi3 (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_mmx_psadbw (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_mmx_pinsrw (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_mmx_pextrw (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_mmx_pshufw (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_eqv8qi3 (TARGET_MMX)
#define HAVE_eqv4hi3 (TARGET_MMX)
#define HAVE_eqv2si3 (TARGET_MMX)
#define HAVE_gtv8qi3 (TARGET_MMX)
#define HAVE_gtv4hi3 (TARGET_MMX)
#define HAVE_gtv2si3 (TARGET_MMX)
#define HAVE_umaxv8qi3 (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_smaxv4hi3 (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_uminv8qi3 (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_sminv4hi3 (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_ashrv4hi3 (TARGET_MMX)
#define HAVE_ashrv2si3 (TARGET_MMX)
#define HAVE_lshrv4hi3 (TARGET_MMX)
#define HAVE_lshrv2si3 (TARGET_MMX)
#define HAVE_mmx_lshrdi3 (TARGET_MMX)
#define HAVE_ashlv4hi3 (TARGET_MMX)
#define HAVE_ashlv2si3 (TARGET_MMX)
#define HAVE_mmx_ashldi3 (TARGET_MMX)
#define HAVE_mmx_packsswb (TARGET_MMX)
#define HAVE_mmx_packssdw (TARGET_MMX)
#define HAVE_mmx_packuswb (TARGET_MMX)
#define HAVE_mmx_punpckhbw (TARGET_MMX)
#define HAVE_mmx_punpckhwd (TARGET_MMX)
#define HAVE_mmx_punpckhdq (TARGET_MMX)
#define HAVE_mmx_punpcklbw (TARGET_MMX)
#define HAVE_mmx_punpcklwd (TARGET_MMX)
#define HAVE_mmx_punpckldq (TARGET_MMX)
#define HAVE_emms (TARGET_MMX)
#define HAVE_ldmxcsr (TARGET_SSE)
#define HAVE_stmxcsr (TARGET_SSE)
#define HAVE_addv2sf3 (TARGET_3DNOW)
#define HAVE_subv2sf3 (TARGET_3DNOW)
#define HAVE_subrv2sf3 (TARGET_3DNOW)
#define HAVE_gtv2sf3 (TARGET_3DNOW)
#define HAVE_gev2sf3 (TARGET_3DNOW)
#define HAVE_eqv2sf3 (TARGET_3DNOW)
#define HAVE_pfmaxv2sf3 (TARGET_3DNOW)
#define HAVE_pfminv2sf3 (TARGET_3DNOW)
#define HAVE_mulv2sf3 (TARGET_3DNOW)
#define HAVE_femms (TARGET_3DNOW)
#define HAVE_pf2id (TARGET_3DNOW)
#define HAVE_pf2iw (TARGET_3DNOW_A)
#define HAVE_pfacc (TARGET_3DNOW)
#define HAVE_pfnacc (TARGET_3DNOW_A)
#define HAVE_pfpnacc (TARGET_3DNOW_A)
#define HAVE_pi2fw (TARGET_3DNOW_A)
#define HAVE_floatv2si2 (TARGET_3DNOW)
#define HAVE_pavgusb (TARGET_3DNOW)
#define HAVE_pfrcpv2sf2 (TARGET_3DNOW)
#define HAVE_pfrcpit1v2sf3 (TARGET_3DNOW)
#define HAVE_pfrcpit2v2sf3 (TARGET_3DNOW)
#define HAVE_pfrsqrtv2sf2 (TARGET_3DNOW)
#define HAVE_pfrsqit1v2sf3 (TARGET_3DNOW)
#define HAVE_pmulhrwv4hi3 (TARGET_3DNOW)
#define HAVE_pswapdv2si2 (TARGET_3DNOW_A)
#define HAVE_pswapdv2sf2 (TARGET_3DNOW_A)
#define HAVE_addv2df3 (TARGET_SSE2)
#define HAVE_vmaddv2df3 (TARGET_SSE2)
#define HAVE_subv2df3 (TARGET_SSE2)
#define HAVE_vmsubv2df3 (TARGET_SSE2)
#define HAVE_mulv2df3 (TARGET_SSE2)
#define HAVE_vmmulv2df3 (TARGET_SSE2)
#define HAVE_divv2df3 (TARGET_SSE2)
#define HAVE_vmdivv2df3 (TARGET_SSE2)
#define HAVE_smaxv2df3 (TARGET_SSE2)
#define HAVE_vmsmaxv2df3 (TARGET_SSE2)
#define HAVE_sminv2df3 (TARGET_SSE2)
#define HAVE_vmsminv2df3 (TARGET_SSE2)
#define HAVE_sqrtv2df2 (TARGET_SSE2)
#define HAVE_vmsqrtv2df2 (TARGET_SSE2)
#define HAVE_maskcmpv2df3 (TARGET_SSE2)
#define HAVE_maskncmpv2df3 (TARGET_SSE2)
#define HAVE_vmmaskcmpv2df3 (TARGET_SSE2)
#define HAVE_vmmaskncmpv2df3 (TARGET_SSE2)
#define HAVE_sse2_comi (TARGET_SSE2)
#define HAVE_sse2_ucomi (TARGET_SSE2)
#define HAVE_sse2_movmskpd (TARGET_SSE2)
#define HAVE_sse2_pmovmskb (TARGET_SSE2)
#define HAVE_sse2_maskmovdqu (TARGET_SSE2)
#define HAVE_sse2_maskmovdqu_rex64 (TARGET_SSE2)
#define HAVE_sse2_movntv2df (TARGET_SSE2)
#define HAVE_sse2_movntv2di (TARGET_SSE2)
#define HAVE_sse2_movntsi (TARGET_SSE2)
#define HAVE_cvtdq2ps (TARGET_SSE2)
#define HAVE_cvtps2dq (TARGET_SSE2)
#define HAVE_cvttps2dq (TARGET_SSE2)
#define HAVE_cvtdq2pd (TARGET_SSE2)
#define HAVE_cvtpd2dq (TARGET_SSE2)
#define HAVE_cvttpd2dq (TARGET_SSE2)
#define HAVE_cvtpd2pi (TARGET_SSE2)
#define HAVE_cvttpd2pi (TARGET_SSE2)
#define HAVE_cvtpi2pd (TARGET_SSE2)
#define HAVE_cvtsd2si (TARGET_SSE2)
#define HAVE_cvtsd2siq (TARGET_SSE2 && TARGET_64BIT)
#define HAVE_cvttsd2si (TARGET_SSE2)
#define HAVE_cvttsd2siq (TARGET_SSE2 && TARGET_64BIT)
#define HAVE_cvtsi2sd (TARGET_SSE2)
#define HAVE_cvtsi2sdq (TARGET_SSE2 && TARGET_64BIT)
#define HAVE_cvtsd2ss (TARGET_SSE2)
#define HAVE_cvtss2sd (TARGET_SSE2)
#define HAVE_cvtpd2ps (TARGET_SSE2)
#define HAVE_cvtps2pd (TARGET_SSE2)
#define HAVE_addv16qi3 (TARGET_SSE2)
#define HAVE_addv8hi3 (TARGET_SSE2)
#define HAVE_addv4si3 (TARGET_SSE2)
#define HAVE_addv2di3 (TARGET_SSE2)
#define HAVE_ssaddv16qi3 (TARGET_SSE2)
#define HAVE_ssaddv8hi3 (TARGET_SSE2)
#define HAVE_usaddv16qi3 (TARGET_SSE2)
#define HAVE_usaddv8hi3 (TARGET_SSE2)
#define HAVE_subv16qi3 (TARGET_SSE2)
#define HAVE_subv8hi3 (TARGET_SSE2)
#define HAVE_subv4si3 (TARGET_SSE2)
#define HAVE_subv2di3 (TARGET_SSE2)
#define HAVE_sssubv16qi3 (TARGET_SSE2)
#define HAVE_sssubv8hi3 (TARGET_SSE2)
#define HAVE_ussubv16qi3 (TARGET_SSE2)
#define HAVE_ussubv8hi3 (TARGET_SSE2)
#define HAVE_mulv8hi3 (TARGET_SSE2)
#define HAVE_smulv8hi3_highpart (TARGET_SSE2)
#define HAVE_umulv8hi3_highpart (TARGET_SSE2)
#define HAVE_sse2_umulsidi3 (TARGET_SSE2)
#define HAVE_sse2_umulv2siv2di3 (TARGET_SSE2)
#define HAVE_sse2_pmaddwd (TARGET_SSE2)
#define HAVE_sse2_clrti (TARGET_SSE2)
#define HAVE_sse2_uavgv16qi3 (TARGET_SSE2)
#define HAVE_sse2_uavgv8hi3 (TARGET_SSE2)
#define HAVE_sse2_psadbw (TARGET_SSE2)
#define HAVE_sse2_pinsrw (TARGET_SSE2)
#define HAVE_sse2_pextrw (TARGET_SSE2)
#define HAVE_sse2_pshufd (TARGET_SSE2)
#define HAVE_sse2_pshuflw (TARGET_SSE2)
#define HAVE_sse2_pshufhw (TARGET_SSE2)
#define HAVE_eqv16qi3 (TARGET_SSE2)
#define HAVE_eqv8hi3 (TARGET_SSE2)
#define HAVE_eqv4si3 (TARGET_SSE2)
#define HAVE_gtv16qi3 (TARGET_SSE2)
#define HAVE_gtv8hi3 (TARGET_SSE2)
#define HAVE_gtv4si3 (TARGET_SSE2)
#define HAVE_umaxv16qi3 (TARGET_SSE2)
#define HAVE_smaxv8hi3 (TARGET_SSE2)
#define HAVE_uminv16qi3 (TARGET_SSE2)
#define HAVE_sminv8hi3 (TARGET_SSE2)
#define HAVE_ashrv8hi3 (TARGET_SSE2)
#define HAVE_ashrv4si3 (TARGET_SSE2)
#define HAVE_lshrv8hi3 (TARGET_SSE2)
#define HAVE_lshrv4si3 (TARGET_SSE2)
#define HAVE_lshrv2di3 (TARGET_SSE2)
#define HAVE_ashlv8hi3 (TARGET_SSE2)
#define HAVE_ashlv4si3 (TARGET_SSE2)
#define HAVE_ashlv2di3 (TARGET_SSE2)
#define HAVE_ashrv8hi3_ti (TARGET_SSE2)
#define HAVE_ashrv4si3_ti (TARGET_SSE2)
#define HAVE_lshrv8hi3_ti (TARGET_SSE2)
#define HAVE_lshrv4si3_ti (TARGET_SSE2)
#define HAVE_lshrv2di3_ti (TARGET_SSE2)
#define HAVE_ashlv8hi3_ti (TARGET_SSE2)
#define HAVE_ashlv4si3_ti (TARGET_SSE2)
#define HAVE_ashlv2di3_ti (TARGET_SSE2)
#define HAVE_sse2_ashlti3 (TARGET_SSE2)
#define HAVE_sse2_lshrti3 (TARGET_SSE2)
#define HAVE_sse2_unpckhpd (TARGET_SSE2)
#define HAVE_sse2_unpcklpd (TARGET_SSE2)
#define HAVE_sse2_packsswb (TARGET_SSE2)
#define HAVE_sse2_packssdw (TARGET_SSE2)
#define HAVE_sse2_packuswb (TARGET_SSE2)
#define HAVE_sse2_punpckhbw (TARGET_SSE2)
#define HAVE_sse2_punpckhwd (TARGET_SSE2)
#define HAVE_sse2_punpckhdq (TARGET_SSE2)
#define HAVE_sse2_punpcklbw (TARGET_SSE2)
#define HAVE_sse2_punpcklwd (TARGET_SSE2)
#define HAVE_sse2_punpckldq (TARGET_SSE2)
#define HAVE_sse2_punpcklqdq (TARGET_SSE2)
#define HAVE_sse2_punpckhqdq (TARGET_SSE2)
#define HAVE_sse2_movapd (TARGET_SSE2 \
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM))
#define HAVE_sse2_movupd (TARGET_SSE2 \
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM))
#define HAVE_sse2_movdqa (TARGET_SSE2 \
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM))
#define HAVE_sse2_movdqu (TARGET_SSE2 \
   && (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM))
#define HAVE_sse2_movdq2q (TARGET_SSE2 && !TARGET_64BIT)
#define HAVE_sse2_movdq2q_rex64 (TARGET_SSE2 && TARGET_64BIT)
#define HAVE_sse2_movq2dq (TARGET_SSE2 && !TARGET_64BIT)
#define HAVE_sse2_movq2dq_rex64 (TARGET_SSE2 && TARGET_64BIT)
#define HAVE_sse2_movq (TARGET_SSE2)
#define HAVE_sse2_loadd (TARGET_SSE2)
#define HAVE_sse2_stored (TARGET_SSE2)
#define HAVE_sse2_movhpd (TARGET_SSE2 && (GET_CODE (operands[1]) == MEM || GET_CODE (operands[2]) == MEM))
#define HAVE_sse2_movlpd (TARGET_SSE2 && (GET_CODE (operands[1]) == MEM || GET_CODE (operands[2]) == MEM))
#define HAVE_sse2_loadsd_1 (TARGET_SSE2)
#define HAVE_sse2_movsd (TARGET_SSE2)
#define HAVE_sse2_storesd (TARGET_SSE2)
#define HAVE_sse2_shufpd (TARGET_SSE2)
#define HAVE_sse2_clflush (TARGET_SSE2)
#define HAVE_mwait (TARGET_PNI)
#define HAVE_monitor (TARGET_PNI)
#define HAVE_addsubv4sf3 (TARGET_PNI)
#define HAVE_addsubv2df3 (TARGET_PNI)
#define HAVE_haddv4sf3 (TARGET_PNI)
#define HAVE_haddv2df3 (TARGET_PNI)
#define HAVE_hsubv4sf3 (TARGET_PNI)
#define HAVE_hsubv2df3 (TARGET_PNI)
#define HAVE_movshdup (TARGET_PNI)
#define HAVE_movsldup (TARGET_PNI)
#define HAVE_lddqu (TARGET_PNI)
#define HAVE_loadddup (TARGET_PNI)
#define HAVE_movddup (TARGET_PNI)
#define HAVE_cmpdi 1
#define HAVE_cmpsi 1
#define HAVE_cmphi 1
#define HAVE_cmpqi (TARGET_QIMODE_MATH)
#define HAVE_cmpdi_1_rex64 (TARGET_64BIT)
#define HAVE_cmpsi_1 1
#define HAVE_cmpqi_ext_3 1
#define HAVE_cmpxf (!TARGET_64BIT && TARGET_80387)
#define HAVE_cmptf (TARGET_80387)
#define HAVE_cmpdf (TARGET_80387 || TARGET_SSE2)
#define HAVE_cmpsf (TARGET_80387 || TARGET_SSE)
#define HAVE_movsi 1
#define HAVE_movhi 1
#define HAVE_movstricthi (! TARGET_PARTIAL_REG_STALL || optimize_size)
#define HAVE_movqi 1
#define HAVE_reload_outqi 1
#define HAVE_movstrictqi (! TARGET_PARTIAL_REG_STALL || optimize_size)
#define HAVE_movdi 1
#define HAVE_movsf 1
#define HAVE_movdf 1
#define HAVE_movxf (!TARGET_64BIT)
#define HAVE_movtf 1
#define HAVE_zero_extendhisi2 1
#define HAVE_zero_extendqihi2 1
#define HAVE_zero_extendqisi2 1
#define HAVE_zero_extendsidi2 1
#define HAVE_extendsidi2 1
#define HAVE_extendsfdf2 (TARGET_80387 || TARGET_SSE2)
#define HAVE_extendsfxf2 (!TARGET_64BIT && TARGET_80387)
#define HAVE_extendsftf2 (TARGET_80387)
#define HAVE_extenddfxf2 (!TARGET_64BIT && TARGET_80387)
#define HAVE_extenddftf2 (TARGET_80387)
#define HAVE_truncdfsf2 (TARGET_80387 || TARGET_SSE2)
#define HAVE_truncxfsf2 (!TARGET_64BIT && TARGET_80387)
#define HAVE_trunctfsf2 (TARGET_80387)
#define HAVE_truncxfdf2 (!TARGET_64BIT && TARGET_80387)
#define HAVE_trunctfdf2 (TARGET_80387)
#define HAVE_fix_truncxfdi2 (!TARGET_64BIT && TARGET_80387)
#define HAVE_fix_trunctfdi2 (TARGET_80387)
#define HAVE_fix_truncdfdi2 (TARGET_80387 || (TARGET_SSE2 && TARGET_64BIT))
#define HAVE_fix_truncsfdi2 (TARGET_80387 || (TARGET_SSE && TARGET_64BIT))
#define HAVE_fix_truncxfsi2 (!TARGET_64BIT && TARGET_80387)
#define HAVE_fix_trunctfsi2 (TARGET_80387)
#define HAVE_fix_truncdfsi2 (TARGET_80387 || TARGET_SSE2)
#define HAVE_fix_truncsfsi2 (TARGET_80387 || TARGET_SSE)
#define HAVE_fix_truncxfhi2 (!TARGET_64BIT && TARGET_80387)
#define HAVE_fix_trunctfhi2 (TARGET_80387)
#define HAVE_fix_truncdfhi2 (TARGET_80387 && !TARGET_SSE2)
#define HAVE_fix_truncsfhi2 (TARGET_80387 && !TARGET_SSE)
#define HAVE_floatsisf2 (TARGET_SSE || TARGET_80387)
#define HAVE_floatdisf2 ((TARGET_64BIT && TARGET_SSE) || TARGET_80387)
#define HAVE_floatsidf2 (TARGET_80387 || TARGET_SSE2)
#define HAVE_floatdidf2 ((TARGET_64BIT && TARGET_SSE2) || TARGET_80387)
#define HAVE_adddi3 1
#define HAVE_addsi3 1
#define HAVE_addhi3 (TARGET_HIMODE_MATH)
#define HAVE_addqi3 (TARGET_QIMODE_MATH)
#define HAVE_addxf3 (!TARGET_64BIT && TARGET_80387)
#define HAVE_addtf3 (TARGET_80387)
#define HAVE_adddf3 (TARGET_80387 || (TARGET_SSE2 && TARGET_SSE_MATH))
#define HAVE_addsf3 (TARGET_80387 || TARGET_SSE_MATH)
#define HAVE_subdi3 1
#define HAVE_subsi3 1
#define HAVE_subhi3 (TARGET_HIMODE_MATH)
#define HAVE_subqi3 (TARGET_QIMODE_MATH)
#define HAVE_subxf3 (!TARGET_64BIT && TARGET_80387)
#define HAVE_subtf3 (TARGET_80387)
#define HAVE_subdf3 (TARGET_80387 || (TARGET_SSE2 && TARGET_SSE_MATH))
#define HAVE_subsf3 (TARGET_80387 || TARGET_SSE_MATH)
#define HAVE_muldi3 (TARGET_64BIT)
#define HAVE_mulsi3 1
#define HAVE_mulhi3 (TARGET_HIMODE_MATH)
#define HAVE_mulqi3 (TARGET_QIMODE_MATH)
#define HAVE_umulqihi3 (TARGET_QIMODE_MATH)
#define HAVE_mulqihi3 (TARGET_QIMODE_MATH)
#define HAVE_umulditi3 (TARGET_64BIT)
#define HAVE_umulsidi3 (!TARGET_64BIT)
#define HAVE_mulditi3 (TARGET_64BIT)
#define HAVE_mulsidi3 (!TARGET_64BIT)
#define HAVE_umuldi3_highpart (TARGET_64BIT)
#define HAVE_umulsi3_highpart 1
#define HAVE_smuldi3_highpart (TARGET_64BIT)
#define HAVE_smulsi3_highpart 1
#define HAVE_mulxf3 (!TARGET_64BIT && TARGET_80387)
#define HAVE_multf3 (TARGET_80387)
#define HAVE_muldf3 (TARGET_80387 || (TARGET_SSE2 && TARGET_SSE_MATH))
#define HAVE_mulsf3 (TARGET_80387 || TARGET_SSE_MATH)
#define HAVE_divxf3 (!TARGET_64BIT && TARGET_80387)
#define HAVE_divtf3 (TARGET_80387)
#define HAVE_divdf3 (TARGET_80387 || (TARGET_SSE2 && TARGET_SSE_MATH))
#define HAVE_divsf3 (TARGET_80387 || TARGET_SSE_MATH)
#define HAVE_divmoddi4 (TARGET_64BIT)
#define HAVE_divmodsi4 1
#define HAVE_udivmodhi4 (TARGET_HIMODE_MATH)
#define HAVE_testsi_ccno_1 1
#define HAVE_testqi_ccz_1 1
#define HAVE_testqi_ext_ccno_0 1
#define HAVE_anddi3 (TARGET_64BIT)
#define HAVE_andsi3 1
#define HAVE_andhi3 (TARGET_HIMODE_MATH)
#define HAVE_andqi3 (TARGET_QIMODE_MATH)
#define HAVE_iordi3 (TARGET_64BIT)
#define HAVE_iorsi3 1
#define HAVE_iorhi3 (TARGET_HIMODE_MATH)
#define HAVE_iorqi3 (TARGET_QIMODE_MATH)
#define HAVE_xordi3 (TARGET_64BIT)
#define HAVE_xorsi3 1
#define HAVE_xorhi3 (TARGET_HIMODE_MATH)
#define HAVE_xorqi3 (TARGET_QIMODE_MATH)
#define HAVE_xorqi_cc_ext_1 1
#define HAVE_negdi2 1
#define HAVE_negsi2 1
#define HAVE_neghi2 (TARGET_HIMODE_MATH)
#define HAVE_negqi2 (TARGET_QIMODE_MATH)
#define HAVE_negsf2 (TARGET_80387)
#define HAVE_negdf2 (TARGET_80387)
#define HAVE_negxf2 (!TARGET_64BIT && TARGET_80387)
#define HAVE_negtf2 (TARGET_80387)
#define HAVE_abssf2 (TARGET_80387)
#define HAVE_absdf2 (TARGET_80387)
#define HAVE_absxf2 (!TARGET_64BIT && TARGET_80387)
#define HAVE_abstf2 (TARGET_80387)
#define HAVE_one_cmpldi2 (TARGET_64BIT)
#define HAVE_one_cmplsi2 1
#define HAVE_one_cmplhi2 (TARGET_HIMODE_MATH)
#define HAVE_one_cmplqi2 (TARGET_QIMODE_MATH)
#define HAVE_ashldi3 1
#define HAVE_x86_shift_adj_1 (TARGET_CMOVE)
#define HAVE_x86_shift_adj_2 1
#define HAVE_ashlsi3 1
#define HAVE_ashlhi3 (TARGET_HIMODE_MATH)
#define HAVE_ashlqi3 (TARGET_QIMODE_MATH)
#define HAVE_ashrdi3 1
#define HAVE_x86_shift_adj_3 1
#define HAVE_ashrsi3 1
#define HAVE_ashrhi3 (TARGET_HIMODE_MATH)
#define HAVE_ashrqi3 (TARGET_QIMODE_MATH)
#define HAVE_lshrdi3 1
#define HAVE_lshrsi3 1
#define HAVE_lshrhi3 (TARGET_HIMODE_MATH)
#define HAVE_lshrqi3 (TARGET_QIMODE_MATH)
#define HAVE_rotldi3 (TARGET_64BIT)
#define HAVE_rotlsi3 1
#define HAVE_rotlhi3 (TARGET_HIMODE_MATH)
#define HAVE_rotlqi3 (TARGET_QIMODE_MATH)
#define HAVE_rotrdi3 (TARGET_64BIT)
#define HAVE_rotrsi3 1
#define HAVE_rotrhi3 (TARGET_HIMODE_MATH)
#define HAVE_rotrqi3 (TARGET_QIMODE_MATH)
#define HAVE_extv 1
#define HAVE_extzv 1
#define HAVE_insv 1
#define HAVE_seq 1
#define HAVE_sne 1
#define HAVE_sgt 1
#define HAVE_sgtu 1
#define HAVE_slt 1
#define HAVE_sltu 1
#define HAVE_sge 1
#define HAVE_sgeu 1
#define HAVE_sle 1
#define HAVE_sleu 1
#define HAVE_sunordered (TARGET_80387 || TARGET_SSE)
#define HAVE_sordered (TARGET_80387)
#define HAVE_suneq (TARGET_80387 || TARGET_SSE)
#define HAVE_sunge (TARGET_80387 || TARGET_SSE)
#define HAVE_sungt (TARGET_80387 || TARGET_SSE)
#define HAVE_sunle (TARGET_80387 || TARGET_SSE)
#define HAVE_sunlt (TARGET_80387 || TARGET_SSE)
#define HAVE_sltgt (TARGET_80387 || TARGET_SSE)
#define HAVE_beq 1
#define HAVE_bne 1
#define HAVE_bgt 1
#define HAVE_bgtu 1
#define HAVE_blt 1
#define HAVE_bltu 1
#define HAVE_bge 1
#define HAVE_bgeu 1
#define HAVE_ble 1
#define HAVE_bleu 1
#define HAVE_bunordered (TARGET_80387 || TARGET_SSE)
#define HAVE_bordered (TARGET_80387 || TARGET_SSE)
#define HAVE_buneq (TARGET_80387 || TARGET_SSE)
#define HAVE_bunge (TARGET_80387 || TARGET_SSE)
#define HAVE_bungt (TARGET_80387 || TARGET_SSE)
#define HAVE_bunle (TARGET_80387 || TARGET_SSE)
#define HAVE_bunlt (TARGET_80387 || TARGET_SSE)
#define HAVE_bltgt (TARGET_80387 || TARGET_SSE)
#define HAVE_indirect_jump 1
#define HAVE_tablejump 1
#define HAVE_doloop_end (!TARGET_64BIT && TARGET_USE_LOOP)
#define HAVE_call_pop (!TARGET_64BIT)
#define HAVE_call 1
#define HAVE_call_value_pop (!TARGET_64BIT)
#define HAVE_call_value 1
#define HAVE_untyped_call 1
#define HAVE_return (ix86_can_use_return_insn_p ())
#define HAVE_prologue 1
#define HAVE_epilogue 1
#define HAVE_sibcall_epilogue 1
#define HAVE_eh_return 1
#define HAVE_ffssi2 1
#define HAVE_tls_global_dynamic_32 1
#define HAVE_tls_global_dynamic_64 1
#define HAVE_tls_local_dynamic_base_32 1
#define HAVE_tls_local_dynamic_base_64 1
#define HAVE_sqrtsf2 ((! TARGET_NO_FANCY_MATH_387 && TARGET_80387) || TARGET_SSE_MATH)
#define HAVE_sqrtdf2 ((! TARGET_NO_FANCY_MATH_387 && TARGET_80387) \
   || (TARGET_SSE2 && TARGET_SSE_MATH))
#define HAVE_movstrsi 1
#define HAVE_movstrdi (TARGET_64BIT)
#define HAVE_strmovdi_rex64 (TARGET_64BIT)
#define HAVE_strmovsi 1
#define HAVE_strmovsi_rex64 (TARGET_64BIT)
#define HAVE_strmovhi 1
#define HAVE_strmovhi_rex64 (TARGET_64BIT)
#define HAVE_strmovqi 1
#define HAVE_strmovqi_rex64 (TARGET_64BIT)
#define HAVE_clrstrsi 1
#define HAVE_clrstrdi (TARGET_64BIT)
#define HAVE_strsetdi_rex64 (TARGET_64BIT)
#define HAVE_strsetsi 1
#define HAVE_strsetsi_rex64 (TARGET_64BIT)
#define HAVE_strsethi 1
#define HAVE_strsethi_rex64 (TARGET_64BIT)
#define HAVE_strsetqi 1
#define HAVE_strsetqi_rex64 (TARGET_64BIT)
#define HAVE_cmpstrsi 1
#define HAVE_cmpintqi 1
#define HAVE_strlensi 1
#define HAVE_strlendi 1
#define HAVE_movdicc (TARGET_64BIT)
#define HAVE_movsicc 1
#define HAVE_movhicc (TARGET_CMOVE && TARGET_HIMODE_MATH)
#define HAVE_movsfcc (TARGET_CMOVE)
#define HAVE_movdfcc (TARGET_CMOVE)
#define HAVE_movxfcc (!TARGET_64BIT && TARGET_CMOVE)
#define HAVE_movtfcc (TARGET_CMOVE)
#define HAVE_minsf3 (TARGET_SSE)
#define HAVE_mindf3 (TARGET_SSE2 && TARGET_SSE_MATH)
#define HAVE_maxsf3 (TARGET_SSE)
#define HAVE_maxdf3 (TARGET_SSE2 && TARGET_SSE_MATH)
#define HAVE_pro_epilogue_adjust_stack 1
#define HAVE_allocate_stack_worker (TARGET_STACK_PROBE)
#define HAVE_allocate_stack (TARGET_STACK_PROBE)
#define HAVE_builtin_setjmp_receiver (!TARGET_64BIT && flag_pic)
#define HAVE_conditional_trap 1
#define HAVE_movti (TARGET_SSE || TARGET_64BIT)
#define HAVE_movv2df (TARGET_SSE2)
#define HAVE_movv8hi (TARGET_SSE2)
#define HAVE_movv16qi (TARGET_SSE2)
#define HAVE_movv4sf (TARGET_SSE)
#define HAVE_movv4si (TARGET_SSE)
#define HAVE_movv2di (TARGET_SSE)
#define HAVE_movv2si (TARGET_MMX)
#define HAVE_movv4hi (TARGET_MMX)
#define HAVE_movv8qi (TARGET_MMX)
#define HAVE_movv2sf (TARGET_3DNOW)
#define HAVE_sse_movaps (TARGET_SSE)
#define HAVE_sse_movups (TARGET_SSE)
#define HAVE_sse_loadss (TARGET_SSE)
#define HAVE_sse_andv4sf3 (TARGET_SSE)
#define HAVE_sse_nandv4sf3 (TARGET_SSE)
#define HAVE_sse_iorv4sf3 (TARGET_SSE)
#define HAVE_sse_xorv4sf3 (TARGET_SSE \
   && (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != MEM))
#define HAVE_sse2_andv2df3 (TARGET_SSE2)
#define HAVE_sse2_nandv2df3 (TARGET_SSE2)
#define HAVE_sse2_iorv2df3 (TARGET_SSE2)
#define HAVE_sse2_xorv2df3 (TARGET_SSE2)
#define HAVE_sfence (TARGET_SSE || TARGET_3DNOW_A)
#define HAVE_sse_prologue_save (TARGET_64BIT)
#define HAVE_prefetch (TARGET_PREFETCH_SSE || TARGET_3DNOW)
#define HAVE_sse2_loadsd (TARGET_SSE2)
#define HAVE_sse2_mfence (TARGET_SSE2)
#define HAVE_sse2_lfence (TARGET_SSE2)
extern rtx        gen_cmpdi_ccno_1_rex64              PARAMS ((rtx, rtx));
extern rtx        gen_cmpdi_1_insn_rex64              PARAMS ((rtx, rtx));
extern rtx        gen_cmpqi_ext_3_insn                PARAMS ((rtx, rtx));
extern rtx        gen_cmpqi_ext_3_insn_rex64          PARAMS ((rtx, rtx));
extern rtx        gen_x86_fnstsw_1                    PARAMS ((rtx));
extern rtx        gen_x86_sahf_1                      PARAMS ((rtx));
extern rtx        gen_popsi1                          PARAMS ((rtx));
extern rtx        gen_movsi_insv_1                    PARAMS ((rtx, rtx));
extern rtx        gen_pushdi2_rex64                   PARAMS ((rtx, rtx));
extern rtx        gen_popdi1                          PARAMS ((rtx));
extern rtx        gen_swapxf                          PARAMS ((rtx, rtx));
extern rtx        gen_swaptf                          PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendhisi2_and            PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendsidi2_32             PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendsidi2_rex64          PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendhidi2                PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendqidi2                PARAMS ((rtx, rtx));
extern rtx        gen_extendsidi2_rex64               PARAMS ((rtx, rtx));
extern rtx        gen_extendhidi2                     PARAMS ((rtx, rtx));
extern rtx        gen_extendqidi2                     PARAMS ((rtx, rtx));
extern rtx        gen_extendhisi2                     PARAMS ((rtx, rtx));
extern rtx        gen_extendqihi2                     PARAMS ((rtx, rtx));
extern rtx        gen_extendqisi2                     PARAMS ((rtx, rtx));
extern rtx        gen_truncdfsf2_3                    PARAMS ((rtx, rtx));
extern rtx        gen_truncdfsf2_sse_only             PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncdi_nomemory            PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_fix_truncdi_memory              PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_fix_truncsfdi_sse               PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncdfdi_sse               PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncsi_nomemory            PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_fix_truncsi_memory              PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_fix_truncsfsi_sse               PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncdfsi_sse               PARAMS ((rtx, rtx));
extern rtx        gen_fix_trunchi_nomemory            PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_fix_trunchi_memory              PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_x86_fnstcw_1                    PARAMS ((rtx));
extern rtx        gen_x86_fldcw_1                     PARAMS ((rtx));
extern rtx        gen_floathisf2                      PARAMS ((rtx, rtx));
extern rtx        gen_floathidf2                      PARAMS ((rtx, rtx));
extern rtx        gen_floathixf2                      PARAMS ((rtx, rtx));
extern rtx        gen_floathitf2                      PARAMS ((rtx, rtx));
extern rtx        gen_floatsixf2                      PARAMS ((rtx, rtx));
extern rtx        gen_floatsitf2                      PARAMS ((rtx, rtx));
extern rtx        gen_floatdixf2                      PARAMS ((rtx, rtx));
extern rtx        gen_floatditf2                      PARAMS ((rtx, rtx));
extern rtx        gen_addqi3_cc                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addsi_1_zext                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addqi_ext_1                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subdi3_carry_rex64              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subsi3_carry                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subsi3_carry_zext               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divqi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_udivqi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divmodhi4                       PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_udivmoddi4                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_udivmodsi4                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_testsi_1                        PARAMS ((rtx, rtx));
extern rtx        gen_andqi_ext_0                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_iorqi_ext_0                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xorqi_ext_0                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_negsf2_memory                   PARAMS ((rtx, rtx));
extern rtx        gen_negsf2_ifs                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_negdf2_memory                   PARAMS ((rtx, rtx));
extern rtx        gen_negdf2_ifs                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_abssf2_memory                   PARAMS ((rtx, rtx));
extern rtx        gen_abssf2_ifs                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_absdf2_memory                   PARAMS ((rtx, rtx));
extern rtx        gen_absdf2_ifs                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashldi3_1                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_x86_shld_1                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrdi3_63_rex64                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrdi3_1                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_x86_shrd_1                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrsi3_31                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrdi3_1                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_setcc_2                         PARAMS ((rtx, rtx));
extern rtx        gen_jump                            PARAMS ((rtx));
extern rtx        gen_doloop_end_internal             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_blockage                        PARAMS ((rtx));
extern rtx        gen_return_internal                 PARAMS ((void));
extern rtx        gen_return_pop_internal             PARAMS ((rtx));
extern rtx        gen_return_indirect_internal        PARAMS ((rtx));
extern rtx        gen_nop                             PARAMS ((void));
extern rtx        gen_set_got                         PARAMS ((rtx));
extern rtx        gen_eh_return_si                    PARAMS ((rtx));
extern rtx        gen_eh_return_di                    PARAMS ((rtx));
extern rtx        gen_leave                           PARAMS ((void));
extern rtx        gen_leave_rex64                     PARAMS ((void));
extern rtx        gen_ffssi_1                         PARAMS ((rtx, rtx));
extern rtx        gen_sqrtsf2_1                       PARAMS ((rtx, rtx));
extern rtx        gen_sqrtsf2_1_sse_only              PARAMS ((rtx, rtx));
extern rtx        gen_sqrtsf2_i387                    PARAMS ((rtx, rtx));
extern rtx        gen_sqrtdf2_1                       PARAMS ((rtx, rtx));
extern rtx        gen_sqrtdf2_1_sse_only              PARAMS ((rtx, rtx));
extern rtx        gen_sqrtdf2_i387                    PARAMS ((rtx, rtx));
extern rtx        gen_sqrtxf2                         PARAMS ((rtx, rtx));
extern rtx        gen_sqrttf2                         PARAMS ((rtx, rtx));
extern rtx        gen_sindf2                          PARAMS ((rtx, rtx));
extern rtx        gen_sinsf2                          PARAMS ((rtx, rtx));
extern rtx        gen_sinxf2                          PARAMS ((rtx, rtx));
extern rtx        gen_sintf2                          PARAMS ((rtx, rtx));
extern rtx        gen_cosdf2                          PARAMS ((rtx, rtx));
extern rtx        gen_cossf2                          PARAMS ((rtx, rtx));
extern rtx        gen_cosxf2                          PARAMS ((rtx, rtx));
extern rtx        gen_costf2                          PARAMS ((rtx, rtx));
extern rtx        gen_cld                             PARAMS ((void));
extern rtx        gen_strmovdi_rex_1                  PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_strmovsi_1                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_strmovsi_rex_1                  PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_strmovhi_1                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_strmovhi_rex_1                  PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_strmovqi_1                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_strmovqi_rex_1                  PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_rep_movdi_rex64                 PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_rep_movsi                       PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_rep_movsi_rex64                 PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_rep_movqi                       PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_rep_movqi_rex64                 PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_strsetdi_rex_1                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_strsetsi_1                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_strsetsi_rex_1                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_strsethi_1                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_strsethi_rex_1                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_strsetqi_1                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_strsetqi_rex_1                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rep_stosdi_rex64                PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_rep_stossi                      PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_rep_stossi_rex64                PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_rep_stosqi                      PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_rep_stosqi_rex64                PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_cmpstrqi_nz_1                   PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_cmpstrqi_nz_rex_1               PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_cmpstrqi_1                      PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_cmpstrqi_rex_1                  PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_strlenqi_1                      PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_strlenqi_rex_1                  PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_x86_movdicc_0_m1_rex64          PARAMS ((rtx));
extern rtx        gen_x86_movsicc_0_m1                PARAMS ((rtx));
extern rtx        gen_pro_epilogue_adjust_stack_rex64 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_movsfcc                     PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_sse_movsfcc_eq                  PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_sse_movdfcc                     PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_sse_movdfcc_eq                  PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_allocate_stack_worker_1         PARAMS ((rtx));
extern rtx        gen_allocate_stack_worker_rex64     PARAMS ((rtx));
extern rtx        gen_trap                            PARAMS ((void));
extern rtx        gen_movv4sf_internal                PARAMS ((rtx, rtx));
extern rtx        gen_movv4si_internal                PARAMS ((rtx, rtx));
extern rtx        gen_movv2di_internal                PARAMS ((rtx, rtx));
extern rtx        gen_movv8qi_internal                PARAMS ((rtx, rtx));
extern rtx        gen_movv4hi_internal                PARAMS ((rtx, rtx));
extern rtx        gen_movv2si_internal                PARAMS ((rtx, rtx));
extern rtx        gen_movv2sf_internal                PARAMS ((rtx, rtx));
extern rtx        gen_movv2df_internal                PARAMS ((rtx, rtx));
extern rtx        gen_movv8hi_internal                PARAMS ((rtx, rtx));
extern rtx        gen_movv16qi_internal               PARAMS ((rtx, rtx));
extern rtx        gen_movti_internal                  PARAMS ((rtx, rtx));
extern rtx        gen_sse_movmskps                    PARAMS ((rtx, rtx));
extern rtx        gen_mmx_pmovmskb                    PARAMS ((rtx, rtx));
extern rtx        gen_mmx_maskmovq                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_maskmovq_rex                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_movntv4sf                   PARAMS ((rtx, rtx));
extern rtx        gen_sse_movntdi                     PARAMS ((rtx, rtx));
extern rtx        gen_sse_movhlps                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_movlhps                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_movhps                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_movlps                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_loadss_1                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_movss                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_storess                     PARAMS ((rtx, rtx));
extern rtx        gen_sse_shufps                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_addv4sf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_vmaddv4sf3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv4sf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_vmsubv4sf3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulv4sf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_vmmulv4sf3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divv4sf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_vmdivv4sf3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rcpv4sf2                        PARAMS ((rtx, rtx));
extern rtx        gen_vmrcpv4sf2                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rsqrtv4sf2                      PARAMS ((rtx, rtx));
extern rtx        gen_vmrsqrtv4sf2                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sqrtv4sf2                       PARAMS ((rtx, rtx));
extern rtx        gen_vmsqrtv4sf2                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_andv2di3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_nandv2di3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_iorv2di3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_xorv2di3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_clrv4sf                     PARAMS ((rtx));
extern rtx        gen_sse_clrv2df                     PARAMS ((rtx));
extern rtx        gen_maskcmpv4sf3                    PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_maskncmpv4sf3                   PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_vmmaskcmpv4sf3                  PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_vmmaskncmpv4sf3                 PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_sse_comi                        PARAMS ((rtx, rtx));
extern rtx        gen_sse_ucomi                       PARAMS ((rtx, rtx));
extern rtx        gen_sse_unpckhps                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_unpcklps                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smaxv4sf3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_vmsmaxv4sf3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sminv4sf3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_vmsminv4sf3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_cvtpi2ps                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_cvtps2pi                        PARAMS ((rtx, rtx));
extern rtx        gen_cvttps2pi                       PARAMS ((rtx, rtx));
extern rtx        gen_cvtsi2ss                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_cvtsi2ssq                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_cvtss2si                        PARAMS ((rtx, rtx));
extern rtx        gen_cvtss2siq                       PARAMS ((rtx, rtx));
extern rtx        gen_cvttss2si                       PARAMS ((rtx, rtx));
extern rtx        gen_cvttss2siq                      PARAMS ((rtx, rtx));
extern rtx        gen_addv8qi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addv4hi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addv2si3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_adddi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ssaddv8qi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ssaddv4hi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_usaddv8qi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_usaddv4hi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv8qi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv4hi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv2si3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_subdi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sssubv8qi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sssubv4hi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ussubv8qi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ussubv4hi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulv4hi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smulv4hi3_highpart              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umulv4hi3_highpart              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_pmaddwd                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_iordi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_xordi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_clrdi                       PARAMS ((rtx));
extern rtx        gen_mmx_anddi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_nanddi3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_uavgv8qi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_uavgv4hi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_psadbw                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_pinsrw                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_mmx_pextrw                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_pshufw                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_eqv8qi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_eqv4hi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_eqv2si3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_gtv8qi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_gtv4hi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_gtv2si3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umaxv8qi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smaxv4hi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_uminv8qi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sminv4hi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrv4hi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrv2si3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrv4hi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrv2si3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_lshrdi3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlv4hi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlv2si3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_ashldi3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_packsswb                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_packssdw                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_packuswb                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_punpckhbw                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_punpckhwd                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_punpckhdq                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_punpcklbw                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_punpcklwd                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mmx_punpckldq                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_emms                            PARAMS ((void));
extern rtx        gen_ldmxcsr                         PARAMS ((rtx));
extern rtx        gen_stmxcsr                         PARAMS ((rtx));
extern rtx        gen_addv2sf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv2sf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subrv2sf3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_gtv2sf3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_gev2sf3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_eqv2sf3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_pfmaxv2sf3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_pfminv2sf3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulv2sf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_femms                           PARAMS ((void));
extern rtx        gen_pf2id                           PARAMS ((rtx, rtx));
extern rtx        gen_pf2iw                           PARAMS ((rtx, rtx));
extern rtx        gen_pfacc                           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_pfnacc                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_pfpnacc                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_pi2fw                           PARAMS ((rtx, rtx));
extern rtx        gen_floatv2si2                      PARAMS ((rtx, rtx));
extern rtx        gen_pavgusb                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_pfrcpv2sf2                      PARAMS ((rtx, rtx));
extern rtx        gen_pfrcpit1v2sf3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_pfrcpit2v2sf3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_pfrsqrtv2sf2                    PARAMS ((rtx, rtx));
extern rtx        gen_pfrsqit1v2sf3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_pmulhrwv4hi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_pswapdv2si2                     PARAMS ((rtx, rtx));
extern rtx        gen_pswapdv2sf2                     PARAMS ((rtx, rtx));
extern rtx        gen_addv2df3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_vmaddv2df3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv2df3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_vmsubv2df3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulv2df3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_vmmulv2df3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divv2df3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_vmdivv2df3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smaxv2df3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_vmsmaxv2df3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sminv2df3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_vmsminv2df3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sqrtv2df2                       PARAMS ((rtx, rtx));
extern rtx        gen_vmsqrtv2df2                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_maskcmpv2df3                    PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_maskncmpv2df3                   PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_vmmaskcmpv2df3                  PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_vmmaskncmpv2df3                 PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_sse2_comi                       PARAMS ((rtx, rtx));
extern rtx        gen_sse2_ucomi                      PARAMS ((rtx, rtx));
extern rtx        gen_sse2_movmskpd                   PARAMS ((rtx, rtx));
extern rtx        gen_sse2_pmovmskb                   PARAMS ((rtx, rtx));
extern rtx        gen_sse2_maskmovdqu                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_maskmovdqu_rex64           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_movntv2df                  PARAMS ((rtx, rtx));
extern rtx        gen_sse2_movntv2di                  PARAMS ((rtx, rtx));
extern rtx        gen_sse2_movntsi                    PARAMS ((rtx, rtx));
extern rtx        gen_cvtdq2ps                        PARAMS ((rtx, rtx));
extern rtx        gen_cvtps2dq                        PARAMS ((rtx, rtx));
extern rtx        gen_cvttps2dq                       PARAMS ((rtx, rtx));
extern rtx        gen_cvtdq2pd                        PARAMS ((rtx, rtx));
extern rtx        gen_cvtpd2dq                        PARAMS ((rtx, rtx));
extern rtx        gen_cvttpd2dq                       PARAMS ((rtx, rtx));
extern rtx        gen_cvtpd2pi                        PARAMS ((rtx, rtx));
extern rtx        gen_cvttpd2pi                       PARAMS ((rtx, rtx));
extern rtx        gen_cvtpi2pd                        PARAMS ((rtx, rtx));
extern rtx        gen_cvtsd2si                        PARAMS ((rtx, rtx));
extern rtx        gen_cvtsd2siq                       PARAMS ((rtx, rtx));
extern rtx        gen_cvttsd2si                       PARAMS ((rtx, rtx));
extern rtx        gen_cvttsd2siq                      PARAMS ((rtx, rtx));
extern rtx        gen_cvtsi2sd                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_cvtsi2sdq                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_cvtsd2ss                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_cvtss2sd                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_cvtpd2ps                        PARAMS ((rtx, rtx));
extern rtx        gen_cvtps2pd                        PARAMS ((rtx, rtx));
extern rtx        gen_addv16qi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addv8hi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addv4si3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addv2di3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ssaddv16qi3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ssaddv8hi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_usaddv16qi3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_usaddv8hi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv16qi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv8hi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv4si3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv2di3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sssubv16qi3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sssubv8hi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ussubv16qi3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ussubv8hi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulv8hi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smulv8hi3_highpart              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umulv8hi3_highpart              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_umulsidi3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_umulv2siv2di3              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_pmaddwd                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_clrti                      PARAMS ((rtx));
extern rtx        gen_sse2_uavgv16qi3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_uavgv8hi3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_psadbw                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_pinsrw                     PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_sse2_pextrw                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_pshufd                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_pshuflw                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_pshufhw                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_eqv16qi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_eqv8hi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_eqv4si3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_gtv16qi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_gtv8hi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_gtv4si3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umaxv16qi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smaxv8hi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_uminv16qi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sminv8hi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrv8hi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrv4si3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrv8hi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrv4si3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrv2di3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlv8hi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlv4si3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlv2di3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrv8hi3_ti                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrv4si3_ti                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrv8hi3_ti                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrv4si3_ti                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrv2di3_ti                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlv8hi3_ti                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlv4si3_ti                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlv2di3_ti                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_ashlti3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_lshrti3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_unpckhpd                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_unpcklpd                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_packsswb                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_packssdw                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_packuswb                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_punpckhbw                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_punpckhwd                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_punpckhdq                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_punpcklbw                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_punpcklwd                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_punpckldq                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_punpcklqdq                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_punpckhqdq                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_movapd                     PARAMS ((rtx, rtx));
extern rtx        gen_sse2_movupd                     PARAMS ((rtx, rtx));
extern rtx        gen_sse2_movdqa                     PARAMS ((rtx, rtx));
extern rtx        gen_sse2_movdqu                     PARAMS ((rtx, rtx));
extern rtx        gen_sse2_movdq2q                    PARAMS ((rtx, rtx));
extern rtx        gen_sse2_movdq2q_rex64              PARAMS ((rtx, rtx));
extern rtx        gen_sse2_movq2dq                    PARAMS ((rtx, rtx));
extern rtx        gen_sse2_movq2dq_rex64              PARAMS ((rtx, rtx));
extern rtx        gen_sse2_movq                       PARAMS ((rtx, rtx));
extern rtx        gen_sse2_loadd                      PARAMS ((rtx, rtx));
extern rtx        gen_sse2_stored                     PARAMS ((rtx, rtx));
extern rtx        gen_sse2_movhpd                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_movlpd                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_loadsd_1                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_movsd                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_storesd                    PARAMS ((rtx, rtx));
extern rtx        gen_sse2_shufpd                     PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_sse2_clflush                    PARAMS ((rtx));
extern rtx        gen_mwait                           PARAMS ((rtx, rtx));
extern rtx        gen_monitor                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addsubv4sf3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addsubv2df3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_haddv4sf3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_haddv2df3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_hsubv4sf3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_hsubv2df3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_movshdup                        PARAMS ((rtx, rtx));
extern rtx        gen_movsldup                        PARAMS ((rtx, rtx));
extern rtx        gen_lddqu                           PARAMS ((rtx, rtx));
extern rtx        gen_loadddup                        PARAMS ((rtx, rtx));
extern rtx        gen_movddup                         PARAMS ((rtx, rtx));
extern rtx        gen_cmpdi                           PARAMS ((rtx, rtx));
extern rtx        gen_cmpsi                           PARAMS ((rtx, rtx));
extern rtx        gen_cmphi                           PARAMS ((rtx, rtx));
extern rtx        gen_cmpqi                           PARAMS ((rtx, rtx));
extern rtx        gen_cmpdi_1_rex64                   PARAMS ((rtx, rtx));
extern rtx        gen_cmpsi_1                         PARAMS ((rtx, rtx));
extern rtx        gen_cmpqi_ext_3                     PARAMS ((rtx, rtx));
extern rtx        gen_cmpxf                           PARAMS ((rtx, rtx));
extern rtx        gen_cmptf                           PARAMS ((rtx, rtx));
extern rtx        gen_cmpdf                           PARAMS ((rtx, rtx));
extern rtx        gen_cmpsf                           PARAMS ((rtx, rtx));
extern rtx        gen_movsi                           PARAMS ((rtx, rtx));
extern rtx        gen_movhi                           PARAMS ((rtx, rtx));
extern rtx        gen_movstricthi                     PARAMS ((rtx, rtx));
extern rtx        gen_movqi                           PARAMS ((rtx, rtx));
extern rtx        gen_reload_outqi                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_movstrictqi                     PARAMS ((rtx, rtx));
extern rtx        gen_movdi                           PARAMS ((rtx, rtx));
extern rtx        gen_movsf                           PARAMS ((rtx, rtx));
extern rtx        gen_movdf                           PARAMS ((rtx, rtx));
extern rtx        gen_movxf                           PARAMS ((rtx, rtx));
extern rtx        gen_movtf                           PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendhisi2                PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendqihi2                PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendqisi2                PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendsidi2                PARAMS ((rtx, rtx));
extern rtx        gen_extendsidi2                     PARAMS ((rtx, rtx));
extern rtx        gen_extendsfdf2                     PARAMS ((rtx, rtx));
extern rtx        gen_extendsfxf2                     PARAMS ((rtx, rtx));
extern rtx        gen_extendsftf2                     PARAMS ((rtx, rtx));
extern rtx        gen_extenddfxf2                     PARAMS ((rtx, rtx));
extern rtx        gen_extenddftf2                     PARAMS ((rtx, rtx));
extern rtx        gen_truncdfsf2                      PARAMS ((rtx, rtx));
extern rtx        gen_truncxfsf2                      PARAMS ((rtx, rtx));
extern rtx        gen_trunctfsf2                      PARAMS ((rtx, rtx));
extern rtx        gen_truncxfdf2                      PARAMS ((rtx, rtx));
extern rtx        gen_trunctfdf2                      PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncxfdi2                  PARAMS ((rtx, rtx));
extern rtx        gen_fix_trunctfdi2                  PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncdfdi2                  PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncsfdi2                  PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncxfsi2                  PARAMS ((rtx, rtx));
extern rtx        gen_fix_trunctfsi2                  PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncdfsi2                  PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncsfsi2                  PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncxfhi2                  PARAMS ((rtx, rtx));
extern rtx        gen_fix_trunctfhi2                  PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncdfhi2                  PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncsfhi2                  PARAMS ((rtx, rtx));
extern rtx        gen_floatsisf2                      PARAMS ((rtx, rtx));
extern rtx        gen_floatdisf2                      PARAMS ((rtx, rtx));
extern rtx        gen_floatsidf2                      PARAMS ((rtx, rtx));
extern rtx        gen_floatdidf2                      PARAMS ((rtx, rtx));
extern rtx        gen_adddi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addsi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addhi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addqi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addxf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addtf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_adddf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addsf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subdi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subsi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subhi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subqi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subxf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subtf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subdf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subsf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_muldi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulhi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulqi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umulqihi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulqihi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umulditi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umulsidi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulditi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsidi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umuldi3_highpart                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umulsi3_highpart                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smuldi3_highpart                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smulsi3_highpart                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulxf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_multf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_muldf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divxf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divtf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divdf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divsf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divmoddi4                       PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_divmodsi4                       PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_udivmodhi4                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_testsi_ccno_1                   PARAMS ((rtx, rtx));
extern rtx        gen_testqi_ccz_1                    PARAMS ((rtx, rtx));
extern rtx        gen_testqi_ext_ccno_0               PARAMS ((rtx, rtx));
extern rtx        gen_anddi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_andsi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_andhi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_andqi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_iordi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_iorsi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_iorhi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_iorqi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xordi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xorsi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xorhi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xorqi3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xorqi_cc_ext_1                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_negdi2                          PARAMS ((rtx, rtx));
extern rtx        gen_negsi2                          PARAMS ((rtx, rtx));
extern rtx        gen_neghi2                          PARAMS ((rtx, rtx));
extern rtx        gen_negqi2                          PARAMS ((rtx, rtx));
extern rtx        gen_negsf2                          PARAMS ((rtx, rtx));
extern rtx        gen_negdf2                          PARAMS ((rtx, rtx));
extern rtx        gen_negxf2                          PARAMS ((rtx, rtx));
extern rtx        gen_negtf2                          PARAMS ((rtx, rtx));
extern rtx        gen_abssf2                          PARAMS ((rtx, rtx));
extern rtx        gen_absdf2                          PARAMS ((rtx, rtx));
extern rtx        gen_absxf2                          PARAMS ((rtx, rtx));
extern rtx        gen_abstf2                          PARAMS ((rtx, rtx));
extern rtx        gen_one_cmpldi2                     PARAMS ((rtx, rtx));
extern rtx        gen_one_cmplsi2                     PARAMS ((rtx, rtx));
extern rtx        gen_one_cmplhi2                     PARAMS ((rtx, rtx));
extern rtx        gen_one_cmplqi2                     PARAMS ((rtx, rtx));
extern rtx        gen_ashldi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_x86_shift_adj_1                 PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_x86_shift_adj_2                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlsi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlhi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlqi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrdi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_x86_shift_adj_3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrsi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrhi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrqi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrdi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrsi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrhi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrqi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotldi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotlsi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotlhi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotlqi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotrdi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotrsi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotrhi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotrqi3                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_extv                            PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_extzv                           PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_insv                            PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_seq                             PARAMS ((rtx));
extern rtx        gen_sne                             PARAMS ((rtx));
extern rtx        gen_sgt                             PARAMS ((rtx));
extern rtx        gen_sgtu                            PARAMS ((rtx));
extern rtx        gen_slt                             PARAMS ((rtx));
extern rtx        gen_sltu                            PARAMS ((rtx));
extern rtx        gen_sge                             PARAMS ((rtx));
extern rtx        gen_sgeu                            PARAMS ((rtx));
extern rtx        gen_sle                             PARAMS ((rtx));
extern rtx        gen_sleu                            PARAMS ((rtx));
extern rtx        gen_sunordered                      PARAMS ((rtx));
extern rtx        gen_sordered                        PARAMS ((rtx));
extern rtx        gen_suneq                           PARAMS ((rtx));
extern rtx        gen_sunge                           PARAMS ((rtx));
extern rtx        gen_sungt                           PARAMS ((rtx));
extern rtx        gen_sunle                           PARAMS ((rtx));
extern rtx        gen_sunlt                           PARAMS ((rtx));
extern rtx        gen_sltgt                           PARAMS ((rtx));
extern rtx        gen_beq                             PARAMS ((rtx));
extern rtx        gen_bne                             PARAMS ((rtx));
extern rtx        gen_bgt                             PARAMS ((rtx));
extern rtx        gen_bgtu                            PARAMS ((rtx));
extern rtx        gen_blt                             PARAMS ((rtx));
extern rtx        gen_bltu                            PARAMS ((rtx));
extern rtx        gen_bge                             PARAMS ((rtx));
extern rtx        gen_bgeu                            PARAMS ((rtx));
extern rtx        gen_ble                             PARAMS ((rtx));
extern rtx        gen_bleu                            PARAMS ((rtx));
extern rtx        gen_bunordered                      PARAMS ((rtx));
extern rtx        gen_bordered                        PARAMS ((rtx));
extern rtx        gen_buneq                           PARAMS ((rtx));
extern rtx        gen_bunge                           PARAMS ((rtx));
extern rtx        gen_bungt                           PARAMS ((rtx));
extern rtx        gen_bunle                           PARAMS ((rtx));
extern rtx        gen_bunlt                           PARAMS ((rtx));
extern rtx        gen_bltgt                           PARAMS ((rtx));
extern rtx        gen_indirect_jump                   PARAMS ((rtx));
extern rtx        gen_tablejump                       PARAMS ((rtx, rtx));
extern rtx        gen_doloop_end                      PARAMS ((rtx, rtx, rtx, rtx, rtx));
#define GEN_CALL_POP(A, B, C, D) gen_call_pop ((A), (B), (C), (D))
extern rtx        gen_call_pop                        PARAMS ((rtx, rtx, rtx, rtx));
#define GEN_CALL(A, B, C, D) gen_call ((A), (B), (C))
extern rtx        gen_call                            PARAMS ((rtx, rtx, rtx));
#define GEN_CALL_VALUE_POP(A, B, C, D, E) gen_call_value_pop ((A), (B), (C), (D), (E))
extern rtx        gen_call_value_pop                  PARAMS ((rtx, rtx, rtx, rtx, rtx));
#define GEN_CALL_VALUE(A, B, C, D, E) gen_call_value ((A), (B), (C), (D))
extern rtx        gen_call_value                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_untyped_call                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_return                          PARAMS ((void));
extern rtx        gen_prologue                        PARAMS ((void));
extern rtx        gen_epilogue                        PARAMS ((void));
extern rtx        gen_sibcall_epilogue                PARAMS ((void));
extern rtx        gen_eh_return                       PARAMS ((rtx));
extern rtx        gen_ffssi2                          PARAMS ((rtx, rtx));
extern rtx        gen_tls_global_dynamic_32           PARAMS ((rtx, rtx));
extern rtx        gen_tls_global_dynamic_64           PARAMS ((rtx, rtx));
extern rtx        gen_tls_local_dynamic_base_32       PARAMS ((rtx));
extern rtx        gen_tls_local_dynamic_base_64       PARAMS ((rtx));
extern rtx        gen_sqrtsf2                         PARAMS ((rtx, rtx));
extern rtx        gen_sqrtdf2                         PARAMS ((rtx, rtx));
extern rtx        gen_movstrsi                        PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movstrdi                        PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_strmovdi_rex64                  PARAMS ((rtx, rtx));
extern rtx        gen_strmovsi                        PARAMS ((rtx, rtx));
extern rtx        gen_strmovsi_rex64                  PARAMS ((rtx, rtx));
extern rtx        gen_strmovhi                        PARAMS ((rtx, rtx));
extern rtx        gen_strmovhi_rex64                  PARAMS ((rtx, rtx));
extern rtx        gen_strmovqi                        PARAMS ((rtx, rtx));
extern rtx        gen_strmovqi_rex64                  PARAMS ((rtx, rtx));
extern rtx        gen_clrstrsi                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_clrstrdi                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_strsetdi_rex64                  PARAMS ((rtx, rtx));
extern rtx        gen_strsetsi                        PARAMS ((rtx, rtx));
extern rtx        gen_strsetsi_rex64                  PARAMS ((rtx, rtx));
extern rtx        gen_strsethi                        PARAMS ((rtx, rtx));
extern rtx        gen_strsethi_rex64                  PARAMS ((rtx, rtx));
extern rtx        gen_strsetqi                        PARAMS ((rtx, rtx));
extern rtx        gen_strsetqi_rex64                  PARAMS ((rtx, rtx));
extern rtx        gen_cmpstrsi                        PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_cmpintqi                        PARAMS ((rtx));
extern rtx        gen_strlensi                        PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_strlendi                        PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movdicc                         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movsicc                         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movhicc                         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movsfcc                         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movdfcc                         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movxfcc                         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movtfcc                         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_minsf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mindf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_maxsf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_maxdf3                          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_pro_epilogue_adjust_stack       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_allocate_stack_worker           PARAMS ((rtx));
extern rtx        gen_allocate_stack                  PARAMS ((rtx, rtx));
extern rtx        gen_builtin_setjmp_receiver         PARAMS ((rtx));
extern rtx        gen_conditional_trap                PARAMS ((rtx, rtx));
extern rtx        gen_movti                           PARAMS ((rtx, rtx));
extern rtx        gen_movv2df                         PARAMS ((rtx, rtx));
extern rtx        gen_movv8hi                         PARAMS ((rtx, rtx));
extern rtx        gen_movv16qi                        PARAMS ((rtx, rtx));
extern rtx        gen_movv4sf                         PARAMS ((rtx, rtx));
extern rtx        gen_movv4si                         PARAMS ((rtx, rtx));
extern rtx        gen_movv2di                         PARAMS ((rtx, rtx));
extern rtx        gen_movv2si                         PARAMS ((rtx, rtx));
extern rtx        gen_movv4hi                         PARAMS ((rtx, rtx));
extern rtx        gen_movv8qi                         PARAMS ((rtx, rtx));
extern rtx        gen_movv2sf                         PARAMS ((rtx, rtx));
extern rtx        gen_sse_movaps                      PARAMS ((rtx, rtx));
extern rtx        gen_sse_movups                      PARAMS ((rtx, rtx));
extern rtx        gen_sse_loadss                      PARAMS ((rtx, rtx));
extern rtx        gen_sse_andv4sf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_nandv4sf3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_iorv4sf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse_xorv4sf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_andv2df3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_nandv2df3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_iorv2df3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_xorv2df3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sfence                          PARAMS ((void));
extern rtx        gen_sse_prologue_save               PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_prefetch                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sse2_loadsd                     PARAMS ((rtx, rtx));
extern rtx        gen_sse2_mfence                     PARAMS ((void));
extern rtx        gen_sse2_lfence                     PARAMS ((void));

#endif /* GCC_INSN_FLAGS_H */

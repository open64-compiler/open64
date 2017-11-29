/* Generated automatically by the program `genflags'
   from the machine description file `md'.  */

#ifndef GCC_INSN_FLAGS_H
#define GCC_INSN_FLAGS_H

#define HAVE_trap 1
#define HAVE_adddf3 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_addsf3 (TARGET_HARD_FLOAT)
#define HAVE_addsi3_internal (! TARGET_MIPS16 \
   && (TARGET_GAS \
       || GET_CODE (operands[2]) != CONST_INT \
       || INTVAL (operands[2]) != -32768))
#define HAVE_adddi3_internal_1 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
#define HAVE_adddi3_internal_2 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && (TARGET_GAS \
       || GET_CODE (operands[2]) != CONST_INT \
       || INTVAL (operands[2]) != -32768))
#define HAVE_adddi3_internal_3 (TARGET_64BIT \
   && !TARGET_MIPS16 \
   && (TARGET_GAS \
       || GET_CODE (operands[2]) != CONST_INT \
       || INTVAL (operands[2]) != -32768))
#define HAVE_addsi3_internal_2 (TARGET_64BIT \
   && !TARGET_MIPS16 \
   && (TARGET_GAS \
       || GET_CODE (operands[2]) != CONST_INT \
       || INTVAL (operands[2]) != -32768))
#define HAVE_subdf3 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_subsf3 (TARGET_HARD_FLOAT)
#define HAVE_subsi3_internal (!TARGET_MIPS16 \
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
#define HAVE_subdi3_internal (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
#define HAVE_subdi3_internal_2 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && INTVAL (operands[2]) != -32768)
#define HAVE_subdi3_internal_3 (TARGET_64BIT && !TARGET_MIPS16 \
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
#define HAVE_subsi3_internal_2 (TARGET_64BIT && !TARGET_MIPS16 \
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
#define HAVE_muldf3_internal (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !TARGET_MIPS4300)
#define HAVE_muldf3_r4300 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && TARGET_MIPS4300)
#define HAVE_mulsf3_internal (TARGET_HARD_FLOAT && !TARGET_MIPS4300)
#define HAVE_mulsf3_r4300 (TARGET_HARD_FLOAT && TARGET_MIPS4300)
#define HAVE_mulsi3_mult3 (GENERATE_MULT3_SI \
   || TARGET_MAD)
#define HAVE_mulsi3_internal (!TARGET_MIPS4000 || TARGET_MIPS16)
#define HAVE_mulsi3_r4000 (TARGET_MIPS4000 && !TARGET_MIPS16)
#define HAVE_muldi3_internal (TARGET_64BIT && !TARGET_MIPS4000 && !TARGET_MIPS16)
#define HAVE_muldi3_internal2 (TARGET_64BIT && (GENERATE_MULT3_DI || TARGET_MIPS4000 || TARGET_MIPS16))
#define HAVE_mulsidi3_internal (!TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4]))
#define HAVE_mulsidi3_64bit (TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4]))
#define HAVE_xmulsi3_highpart_internal (GET_CODE (operands[3]) == GET_CODE (operands[4]))
#define HAVE_xmulsi3_highpart_mulhi (ISA_HAS_MULHI \
   && TARGET_64BIT \
   && GET_CODE (operands[3]) == GET_CODE (operands[4]))
#define HAVE_smuldi3_highpart (TARGET_64BIT)
#define HAVE_umuldi3_highpart (TARGET_64BIT)
#define HAVE_madsi (TARGET_MAD)
#define HAVE_divdf3 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_divsf3 (TARGET_HARD_FLOAT)
#define HAVE_divmodsi4_internal (optimize)
#define HAVE_divmoddi4_internal (TARGET_64BIT && optimize)
#define HAVE_udivmodsi4_internal (optimize)
#define HAVE_udivmoddi4_internal (TARGET_64BIT && optimize)
#define HAVE_div_trap_normal (!TARGET_MIPS16)
#define HAVE_div_trap_mips16 (TARGET_MIPS16)
#define HAVE_divsi3_internal (!optimize)
#define HAVE_divdi3_internal (TARGET_64BIT && !optimize)
#define HAVE_modsi3_internal (!optimize)
#define HAVE_moddi3_internal (TARGET_64BIT && !optimize)
#define HAVE_udivsi3_internal (!optimize)
#define HAVE_udivdi3_internal (TARGET_64BIT && !optimize)
#define HAVE_umodsi3_internal (!optimize)
#define HAVE_umoddi3_internal (TARGET_64BIT && !optimize)
#define HAVE_sqrtdf2 (TARGET_HARD_FLOAT && HAVE_SQRT_P() && TARGET_DOUBLE_FLOAT)
#define HAVE_sqrtsf2 (TARGET_HARD_FLOAT && HAVE_SQRT_P())
#define HAVE_abssi2 (!TARGET_MIPS16)
#define HAVE_absdi2 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_absdf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_abssf2 (TARGET_HARD_FLOAT)
#define HAVE_ffssi2 (!TARGET_MIPS16)
#define HAVE_ffsdi2 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_negsi2 1
#define HAVE_negdi2_internal (! TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
#define HAVE_negdi2_internal_2 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_negdf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_negsf2 (TARGET_HARD_FLOAT)
#define HAVE_one_cmplsi2 1
#define HAVE_one_cmpldi2 1
#define HAVE_anddi3_internal1 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_xordi3_immed (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_truncdfsf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_truncdisi2 (TARGET_64BIT)
#define HAVE_truncdihi2 (TARGET_64BIT)
#define HAVE_truncdiqi2 (TARGET_64BIT)
#define HAVE_zero_extendsidi2_internal (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_extendhidi2_internal (TARGET_64BIT)
#define HAVE_extendhisi2_internal 1
#define HAVE_extendqihi2_internal 1
#define HAVE_extendqisi2_insn 1
#define HAVE_extendqidi2_insn (TARGET_64BIT)
#define HAVE_extendsfdf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_fix_truncdfsi2_insn (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && ISA_HAS_TRUNC_W)
#define HAVE_fix_truncdfsi2_macro (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !ISA_HAS_TRUNC_W)
#define HAVE_fix_truncsfsi2_insn (TARGET_HARD_FLOAT && ISA_HAS_TRUNC_W)
#define HAVE_fix_truncsfsi2_macro (TARGET_HARD_FLOAT && !ISA_HAS_TRUNC_W)
#define HAVE_fix_truncdfdi2 (TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT)
#define HAVE_fix_truncsfdi2 (TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT)
#define HAVE_floatsidf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_floatdidf2 (TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT)
#define HAVE_floatsisf2 (TARGET_HARD_FLOAT)
#define HAVE_floatdisf2 (TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT)
#define HAVE_movsi_ulw (!TARGET_MIPS16)
#define HAVE_movsi_usw (!TARGET_MIPS16)
#define HAVE_movdi_uld 1
#define HAVE_movdi_usd 1
#define HAVE_high (mips_split_addresses && !TARGET_MIPS16)
#define HAVE_low (mips_split_addresses && !TARGET_MIPS16)
#define HAVE_movdi_internal (!TARGET_64BIT && !TARGET_MIPS16 \
   && (register_operand (operands[0], DImode) \
       || register_operand (operands[1], DImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0) \
       || operands[1] == CONST0_RTX (DImode)))
#define HAVE_movdi_internal2 (TARGET_64BIT && !TARGET_MIPS16 \
   && (register_operand (operands[0], DImode) \
       || register_operand (operands[1], DImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0) \
       || operands[1] == CONST0_RTX (DImode)))
#define HAVE_movsi_internal (!TARGET_MIPS16 \
   && (register_operand (operands[0], SImode) \
       || register_operand (operands[1], SImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
#define HAVE_hilo_delay 1
#define HAVE_movcc (ISA_HAS_8CC && TARGET_HARD_FLOAT)
#define HAVE_movhi_internal (!TARGET_MIPS16 \
   && (register_operand (operands[0], HImode) \
       || register_operand (operands[1], HImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
#define HAVE_movqi_internal (!TARGET_MIPS16 \
   && (register_operand (operands[0], QImode) \
       || register_operand (operands[1], QImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
#define HAVE_movsf_internal1 (TARGET_HARD_FLOAT \
   && (register_operand (operands[0], SFmode) \
       || nonmemory_operand (operands[1], SFmode)))
#define HAVE_movsf_internal2 (TARGET_SOFT_FLOAT && !TARGET_MIPS16 \
   && (register_operand (operands[0], SFmode) \
       || nonmemory_operand (operands[1], SFmode)))
#define HAVE_movdf_internal1 (TARGET_HARD_FLOAT && !(TARGET_FLOAT64 && !TARGET_64BIT) \
   && TARGET_DOUBLE_FLOAT \
   && (register_operand (operands[0], DFmode) \
       || nonmemory_operand (operands[1], DFmode)))
#define HAVE_movdf_internal1a (TARGET_HARD_FLOAT && (TARGET_FLOAT64 && !TARGET_64BIT) \
   && TARGET_DOUBLE_FLOAT \
   && (register_operand (operands[0], DFmode) \
       || nonmemory_operand (operands[1], DFmode)))
#define HAVE_movdf_internal2 ((TARGET_SOFT_FLOAT || TARGET_SINGLE_FLOAT) && !TARGET_MIPS16 \
   && (register_operand (operands[0], DFmode) \
       || nonmemory_operand (operands[1], DFmode)))
#define HAVE_loadgp 1
#define HAVE_movstrsi_internal 1
#define HAVE_movstrsi_internal2 1
#define HAVE_movstrsi_internal3 1
#define HAVE_ashlsi3_internal1 (!TARGET_MIPS16)
#define HAVE_ashlsi3_internal2 (TARGET_MIPS16)
#define HAVE_ashldi3_internal (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
#define HAVE_ashldi3_internal2 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && (INTVAL (operands[2]) & 32) != 0)
#define HAVE_ashldi3_internal3 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && (INTVAL (operands[2]) & 63) < 32 \
   && (INTVAL (operands[2]) & 63) != 0)
#define HAVE_ashldi3_internal4 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_ashrsi3_internal1 (!TARGET_MIPS16)
#define HAVE_ashrsi3_internal2 (TARGET_MIPS16)
#define HAVE_ashrdi3_internal (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
#define HAVE_ashrdi3_internal2 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && (INTVAL (operands[2]) & 32) != 0)
#define HAVE_ashrdi3_internal3 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && (INTVAL (operands[2]) & 63) < 32 \
   && (INTVAL (operands[2]) & 63) != 0)
#define HAVE_ashrdi3_internal4 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_lshrsi3_internal1 (!TARGET_MIPS16)
#define HAVE_lshrsi3_internal2 (TARGET_MIPS16)
#define HAVE_lshrdi3_internal (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
#define HAVE_lshrdi3_internal2 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && (INTVAL (operands[2]) & 32) != 0)
#define HAVE_lshrdi3_internal3 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && (INTVAL (operands[2]) & 63) < 32 \
   && (INTVAL (operands[2]) & 63) != 0)
#define HAVE_lshrdi3_internal4 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_rotrsi3 (ISA_HAS_ROTR_SI)
#define HAVE_rotrdi3 (ISA_HAS_ROTR_DI)
#define HAVE_branch_fp (TARGET_HARD_FLOAT)
#define HAVE_branch_fp_inverted (TARGET_HARD_FLOAT)
#define HAVE_branch_zero (!TARGET_MIPS16)
#define HAVE_branch_zero_inverted (!TARGET_MIPS16)
#define HAVE_branch_zero_di (!TARGET_MIPS16)
#define HAVE_branch_zero_di_inverted (!TARGET_MIPS16)
#define HAVE_branch_equality (!TARGET_MIPS16)
#define HAVE_branch_equality_di (!TARGET_MIPS16)
#define HAVE_branch_equality_inverted (!TARGET_MIPS16)
#define HAVE_branch_equality_di_inverted (!TARGET_MIPS16)
#define HAVE_seq_si_zero (!TARGET_MIPS16)
#define HAVE_seq_di_zero (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_sne_si_zero (!TARGET_MIPS16)
#define HAVE_sne_di_zero (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_sgt_si (!TARGET_MIPS16)
#define HAVE_sgt_di (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_slt_si (!TARGET_MIPS16)
#define HAVE_slt_di (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_sle_si_const (!TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
#define HAVE_sle_di_const (TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
#define HAVE_sgtu_si (!TARGET_MIPS16)
#define HAVE_sgtu_di (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_sltu_si (!TARGET_MIPS16)
#define HAVE_sltu_di (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_sleu_si_const (!TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
#define HAVE_sleu_di_const (TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
#define HAVE_sunordered_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sunlt_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_suneq_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sunle_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_seq_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_slt_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sle_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sgt_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sge_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sunordered_sf (TARGET_HARD_FLOAT)
#define HAVE_sunlt_sf (TARGET_HARD_FLOAT)
#define HAVE_suneq_sf (TARGET_HARD_FLOAT)
#define HAVE_sunle_sf (TARGET_HARD_FLOAT)
#define HAVE_seq_sf (TARGET_HARD_FLOAT)
#define HAVE_slt_sf (TARGET_HARD_FLOAT)
#define HAVE_sle_sf (TARGET_HARD_FLOAT)
#define HAVE_sgt_sf (TARGET_HARD_FLOAT)
#define HAVE_sge_sf (TARGET_HARD_FLOAT)
#define HAVE_jump (!TARGET_MIPS16)
#define HAVE_indirect_jump_internal1 (!(Pmode == DImode))
#define HAVE_indirect_jump_internal2 (Pmode == DImode)
#define HAVE_tablejump_internal1 (!(Pmode == DImode))
#define HAVE_tablejump_internal2 (Pmode == DImode)
#define HAVE_casesi_internal (TARGET_EMBEDDED_PIC)
#define HAVE_casesi_internal_di (TARGET_EMBEDDED_PIC)
#define HAVE_blockage 1
#define HAVE_return (mips_can_use_return_insn ())
#define HAVE_return_internal 1
#define HAVE_get_fnaddr (TARGET_EMBEDDED_PIC \
   && GET_CODE (operands[1]) == SYMBOL_REF)
#define HAVE_eh_set_lr_si (! TARGET_64BIT)
#define HAVE_eh_set_lr_di (TARGET_64BIT)
#define HAVE_exception_receiver (TARGET_ABICALLS && (mips_abi == ABI_32 || mips_abi == ABI_O64))
#define HAVE_call_internal1 (!TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_call_internal2 (TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_call_internal3a (!TARGET_MIPS16 \
   && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_internal3b (!TARGET_MIPS16 \
   && Pmode == DImode && !TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_internal3c (TARGET_MIPS16 && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS \
   && GET_CODE (operands[2]) == REG && REGNO (operands[2]) == 31)
#define HAVE_call_internal4a (!(Pmode == DImode) && TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_internal4b (Pmode == DImode && TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_value_internal1 (!TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_call_value_internal2 (TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_call_value_internal3a (!TARGET_MIPS16 \
   && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_value_internal3b (!TARGET_MIPS16 \
   && Pmode == DImode && !TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_value_internal3c (TARGET_MIPS16 && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS \
   && GET_CODE (operands[3]) == REG && REGNO (operands[3]) == 31)
#define HAVE_call_value_internal4a (!(Pmode == DImode) && TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_value_internal4b (Pmode == DImode && TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_value_multiple_internal1 (!TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_call_value_multiple_internal2 (TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_prefetch_si_address (ISA_HAS_PREFETCH && Pmode == SImode)
#define HAVE_prefetch_si (ISA_HAS_PREFETCH && Pmode == SImode)
#define HAVE_prefetch_di_address (ISA_HAS_PREFETCH && Pmode == DImode)
#define HAVE_prefetch_di (ISA_HAS_PREFETCH && Pmode == DImode)
#define HAVE_nop 1
#define HAVE_consttable_qi (TARGET_MIPS16)
#define HAVE_consttable_hi (TARGET_MIPS16)
#define HAVE_consttable_si (TARGET_MIPS16)
#define HAVE_consttable_di (TARGET_MIPS16)
#define HAVE_consttable_sf (TARGET_MIPS16)
#define HAVE_consttable_df (TARGET_MIPS16)
#define HAVE_align_2 (TARGET_MIPS16)
#define HAVE_align_4 (TARGET_MIPS16)
#define HAVE_align_8 (TARGET_MIPS16)
#define HAVE_leasi (Pmode == SImode)
#define HAVE_leadi (Pmode == DImode)
#define HAVE_conditional_trap (ISA_HAS_COND_TRAP)
#define HAVE_addsi3 1
#define HAVE_adddi3 (TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
#define HAVE_subsi3 1
#define HAVE_subdi3 (TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
#define HAVE_muldf3 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_mulsf3 (TARGET_HARD_FLOAT)
#define HAVE_mulsi3 1
#define HAVE_muldi3 (TARGET_64BIT)
#define HAVE_mulsidi3 1
#define HAVE_umulsidi3 1
#define HAVE_smulsi3_highpart 1
#define HAVE_umulsi3_highpart 1
#define HAVE_divmodsi4 (optimize)
#define HAVE_divmoddi4 (TARGET_64BIT && optimize)
#define HAVE_udivmodsi4 (optimize)
#define HAVE_udivmoddi4 (TARGET_64BIT && optimize)
#define HAVE_div_trap 1
#define HAVE_divsi3 (!optimize)
#define HAVE_divdi3 (TARGET_64BIT && !optimize)
#define HAVE_modsi3 (!optimize)
#define HAVE_moddi3 (TARGET_64BIT && !optimize)
#define HAVE_udivsi3 (!optimize)
#define HAVE_udivdi3 (TARGET_64BIT && !optimize)
#define HAVE_umodsi3 (!optimize)
#define HAVE_umoddi3 (TARGET_64BIT && !optimize)
#define HAVE_negdi2 ((TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16)
#define HAVE_andsi3 1
#define HAVE_anddi3 1
#define HAVE_iorsi3 1
#define HAVE_iordi3 1
#define HAVE_xorsi3 1
#define HAVE_xordi3 1
#define HAVE_zero_extendsidi2 (TARGET_64BIT)
#define HAVE_zero_extendhisi2 1
#define HAVE_zero_extendhidi2 (TARGET_64BIT)
#define HAVE_zero_extendqihi2 1
#define HAVE_zero_extendqisi2 1
#define HAVE_zero_extendqidi2 (TARGET_64BIT)
#define HAVE_extendsidi2 (TARGET_64BIT)
#define HAVE_extendhidi2 (TARGET_64BIT)
#define HAVE_extendhisi2 1
#define HAVE_extendqihi2 1
#define HAVE_extendqisi2 1
#define HAVE_extendqidi2 (TARGET_64BIT)
#define HAVE_fix_truncdfsi2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_fix_truncsfsi2 (TARGET_HARD_FLOAT)
#define HAVE_fixuns_truncdfsi2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_fixuns_truncdfdi2 (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
#define HAVE_fixuns_truncsfsi2 (TARGET_HARD_FLOAT)
#define HAVE_fixuns_truncsfdi2 (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
#define HAVE_extv (!TARGET_MIPS16)
#define HAVE_extzv (!TARGET_MIPS16)
#define HAVE_insv (!TARGET_MIPS16)
#define HAVE_movdi 1
#define HAVE_reload_indi (TARGET_64BIT)
#define HAVE_reload_outdi (TARGET_64BIT)
#define HAVE_movsi 1
#define HAVE_reload_outsi (TARGET_64BIT || TARGET_MIPS16)
#define HAVE_reload_insi (TARGET_MIPS16)
#define HAVE_reload_incc (ISA_HAS_8CC && TARGET_HARD_FLOAT)
#define HAVE_reload_outcc (ISA_HAS_8CC && TARGET_HARD_FLOAT)
#define HAVE_movhi 1
#define HAVE_movqi 1
#define HAVE_movsf 1
#define HAVE_movdf 1
#define HAVE_movstrsi (!TARGET_MIPS16)
#define HAVE_ashlsi3 1
#define HAVE_ashldi3 (TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
#define HAVE_ashrsi3 1
#define HAVE_ashrdi3 (TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
#define HAVE_lshrsi3 1
#define HAVE_lshrdi3 (TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
#define HAVE_cmpsi 1
#define HAVE_tstsi 1
#define HAVE_cmpdi (TARGET_64BIT)
#define HAVE_tstdi (TARGET_64BIT)
#define HAVE_cmpdf (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_cmpsf (TARGET_HARD_FLOAT)
#define HAVE_bunordered 1
#define HAVE_bordered 1
#define HAVE_bunlt 1
#define HAVE_bunge 1
#define HAVE_buneq 1
#define HAVE_bltgt 1
#define HAVE_bunle 1
#define HAVE_bungt 1
#define HAVE_beq 1
#define HAVE_bne 1
#define HAVE_bgt 1
#define HAVE_bge 1
#define HAVE_blt 1
#define HAVE_ble 1
#define HAVE_bgtu 1
#define HAVE_bgeu 1
#define HAVE_bltu 1
#define HAVE_bleu 1
#define HAVE_seq 1
#define HAVE_sne (!TARGET_MIPS16)
#define HAVE_sgt 1
#define HAVE_sge 1
#define HAVE_slt 1
#define HAVE_sle 1
#define HAVE_sgtu 1
#define HAVE_sgeu 1
#define HAVE_sltu 1
#define HAVE_sleu 1
#define HAVE_indirect_jump 1
#define HAVE_tablejump 1
#define HAVE_tablejump_internal3 1
#define HAVE_tablejump_mips161 (TARGET_MIPS16 && !(Pmode == DImode))
#define HAVE_tablejump_mips162 (TARGET_MIPS16 && Pmode == DImode)
#define HAVE_tablejump_internal4 1
#define HAVE_casesi (TARGET_EMBEDDED_PIC)
#define HAVE_builtin_setjmp_setup (TARGET_ABICALLS)
#define HAVE_builtin_setjmp_setup_32 (TARGET_ABICALLS && ! (Pmode == DImode))
#define HAVE_builtin_setjmp_setup_64 (TARGET_ABICALLS && Pmode == DImode)
#define HAVE_builtin_longjmp (TARGET_ABICALLS)
#define HAVE_prologue 1
#define HAVE_epilogue 1
#define HAVE_eh_return 1
#define HAVE_call 1
#define HAVE_call_internal0 1
#define HAVE_call_value 1
#define HAVE_call_value_internal0 1
#define HAVE_call_value_multiple_internal0 1
#define HAVE_untyped_call 1
#define HAVE_prefetch (ISA_HAS_PREFETCH)
#define HAVE_movsicc (ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE)
#define HAVE_movdicc ((ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE) && TARGET_64BIT)
#define HAVE_movsfcc (ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT)
#define HAVE_movdfcc (ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
extern rtx        gen_trap                          PARAMS ((void));
extern rtx        gen_adddf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addsf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addsi3_internal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_adddi3_internal_1             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_adddi3_internal_2             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_adddi3_internal_3             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addsi3_internal_2             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subdf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subsf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subsi3_internal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subdi3_internal               PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_subdi3_internal_2             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_subdi3_internal_3             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subsi3_internal_2             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_muldf3_internal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_muldf3_r4300                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsf3_internal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsf3_r4300                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsi3_mult3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsi3_internal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsi3_r4000                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_muldi3_internal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_muldi3_internal2              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsidi3_internal             PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_mulsidi3_64bit                PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_xmulsi3_highpart_internal     PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_xmulsi3_highpart_mulhi        PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_smuldi3_highpart              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umuldi3_highpart              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_madsi                         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divdf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divsf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divmodsi4_internal            PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_divmoddi4_internal            PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_udivmodsi4_internal           PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_udivmoddi4_internal           PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_div_trap_normal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_div_trap_mips16               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divsi3_internal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divdi3_internal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_modsi3_internal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_moddi3_internal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_udivsi3_internal              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_udivdi3_internal              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umodsi3_internal              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umoddi3_internal              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sqrtdf2                       PARAMS ((rtx, rtx));
extern rtx        gen_sqrtsf2                       PARAMS ((rtx, rtx));
extern rtx        gen_abssi2                        PARAMS ((rtx, rtx));
extern rtx        gen_absdi2                        PARAMS ((rtx, rtx));
extern rtx        gen_absdf2                        PARAMS ((rtx, rtx));
extern rtx        gen_abssf2                        PARAMS ((rtx, rtx));
extern rtx        gen_ffssi2                        PARAMS ((rtx, rtx));
extern rtx        gen_ffsdi2                        PARAMS ((rtx, rtx));
extern rtx        gen_negsi2                        PARAMS ((rtx, rtx));
extern rtx        gen_negdi2_internal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_negdi2_internal_2             PARAMS ((rtx, rtx));
extern rtx        gen_negdf2                        PARAMS ((rtx, rtx));
extern rtx        gen_negsf2                        PARAMS ((rtx, rtx));
extern rtx        gen_one_cmplsi2                   PARAMS ((rtx, rtx));
extern rtx        gen_one_cmpldi2                   PARAMS ((rtx, rtx));
extern rtx        gen_anddi3_internal1              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xordi3_immed                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_truncdfsf2                    PARAMS ((rtx, rtx));
extern rtx        gen_truncdisi2                    PARAMS ((rtx, rtx));
extern rtx        gen_truncdihi2                    PARAMS ((rtx, rtx));
extern rtx        gen_truncdiqi2                    PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendsidi2_internal     PARAMS ((rtx, rtx));
extern rtx        gen_extendhidi2_internal          PARAMS ((rtx, rtx));
extern rtx        gen_extendhisi2_internal          PARAMS ((rtx, rtx));
extern rtx        gen_extendqihi2_internal          PARAMS ((rtx, rtx));
extern rtx        gen_extendqisi2_insn              PARAMS ((rtx, rtx));
extern rtx        gen_extendqidi2_insn              PARAMS ((rtx, rtx));
extern rtx        gen_extendsfdf2                   PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncdfsi2_insn           PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncdfsi2_macro          PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncsfsi2_insn           PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncsfsi2_macro          PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncdfdi2                PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncsfdi2                PARAMS ((rtx, rtx));
extern rtx        gen_floatsidf2                    PARAMS ((rtx, rtx));
extern rtx        gen_floatdidf2                    PARAMS ((rtx, rtx));
extern rtx        gen_floatsisf2                    PARAMS ((rtx, rtx));
extern rtx        gen_floatdisf2                    PARAMS ((rtx, rtx));
extern rtx        gen_movsi_ulw                     PARAMS ((rtx, rtx));
extern rtx        gen_movsi_usw                     PARAMS ((rtx, rtx));
extern rtx        gen_movdi_uld                     PARAMS ((rtx, rtx));
extern rtx        gen_movdi_usd                     PARAMS ((rtx, rtx));
extern rtx        gen_high                          PARAMS ((rtx, rtx));
extern rtx        gen_low                           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_movdi_internal                PARAMS ((rtx, rtx));
extern rtx        gen_movdi_internal2               PARAMS ((rtx, rtx));
extern rtx        gen_movsi_internal                PARAMS ((rtx, rtx));
extern rtx        gen_hilo_delay                    PARAMS ((rtx));
extern rtx        gen_movcc                         PARAMS ((rtx, rtx));
extern rtx        gen_movhi_internal                PARAMS ((rtx, rtx));
extern rtx        gen_movqi_internal                PARAMS ((rtx, rtx));
extern rtx        gen_movsf_internal1               PARAMS ((rtx, rtx));
extern rtx        gen_movsf_internal2               PARAMS ((rtx, rtx));
extern rtx        gen_movdf_internal1               PARAMS ((rtx, rtx));
extern rtx        gen_movdf_internal1a              PARAMS ((rtx, rtx));
extern rtx        gen_movdf_internal2               PARAMS ((rtx, rtx));
extern rtx        gen_loadgp                        PARAMS ((rtx, rtx));
extern rtx        gen_movstrsi_internal             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movstrsi_internal2            PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movstrsi_internal3            PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_ashlsi3_internal1             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlsi3_internal2             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashldi3_internal              PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_ashldi3_internal2             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_ashldi3_internal3             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_ashldi3_internal4             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrsi3_internal1             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrsi3_internal2             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrdi3_internal              PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_ashrdi3_internal2             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_ashrdi3_internal3             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_ashrdi3_internal4             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrsi3_internal1             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrsi3_internal2             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrdi3_internal              PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_lshrdi3_internal2             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_lshrdi3_internal3             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_lshrdi3_internal4             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotrsi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotrdi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_branch_fp                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_branch_fp_inverted            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_branch_zero                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_branch_zero_inverted          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_branch_zero_di                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_branch_zero_di_inverted       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_branch_equality               PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_branch_equality_di            PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_branch_equality_inverted      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_branch_equality_di_inverted   PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_seq_si_zero                   PARAMS ((rtx, rtx));
extern rtx        gen_seq_di_zero                   PARAMS ((rtx, rtx));
static inline rtx gen_seq_si                        PARAMS ((rtx, rtx, rtx));
static inline rtx
//gen_seq_si(a, b, c)
//     rtx a ATTRIBUTE_UNUSED;
//     rtx b ATTRIBUTE_UNUSED;
//     rtx c ATTRIBUTE_UNUSED;
gen_seq_si(rtx a , rtx b , rtx c)
{
  return 0;
}
static inline rtx gen_seq_di                        PARAMS ((rtx, rtx, rtx));
static inline rtx
//gen_seq_di(a, b, c)
//     rtx a ATTRIBUTE_UNUSED;
//     rtx b ATTRIBUTE_UNUSED;
//     rtx c ATTRIBUTE_UNUSED;
gen_seq_di(rtx a, rtx b, rtx c)
{
  return 0;
}
extern rtx        gen_sne_si_zero                   PARAMS ((rtx, rtx));
extern rtx        gen_sne_di_zero                   PARAMS ((rtx, rtx));
static inline rtx gen_sne_si                        PARAMS ((rtx, rtx, rtx));
static inline rtx
//gen_sne_si(a, b, c)
//     rtx a ATTRIBUTE_UNUSED;
//     rtx b ATTRIBUTE_UNUSED;
//     rtx c ATTRIBUTE_UNUSED;
gen_sne_si(rtx a, rtx b, rtx c)
{
  return 0;
}
static inline rtx gen_sne_di                        PARAMS ((rtx, rtx, rtx));
static inline rtx
//gen_sne_di(a, b, c)
//     rtx a ATTRIBUTE_UNUSED;
//     rtx b ATTRIBUTE_UNUSED;
//     rtx c ATTRIBUTE_UNUSED;
gen_sne_di(rtx a, rtx b, rtx c)
{
  return 0;
}
extern rtx        gen_sgt_si                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sgt_di                        PARAMS ((rtx, rtx, rtx));
static inline rtx gen_sge_si                        PARAMS ((rtx, rtx, rtx));
static inline rtx
//gen_sge_si(a, b, c)
//     rtx a ATTRIBUTE_UNUSED;
//     rtx b ATTRIBUTE_UNUSED;
//     rtx c ATTRIBUTE_UNUSED;
gen_sge_si(rtx a, rtx b, rtx c)
{
  return 0;
}
static inline rtx gen_sge_di                        PARAMS ((rtx, rtx, rtx));
static inline rtx
//gen_sge_di(a, b, c)
//     rtx a ATTRIBUTE_UNUSED;
//     rtx b ATTRIBUTE_UNUSED;
//     rtx c ATTRIBUTE_UNUSED;
gen_sge_di(rtx a, rtx b, rtx c)
{
  return 0;
}
extern rtx        gen_slt_si                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_slt_di                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sle_si_const                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sle_di_const                  PARAMS ((rtx, rtx, rtx));
static inline rtx gen_sle_si_reg                    PARAMS ((rtx, rtx, rtx));
static inline rtx
//gen_sle_si_reg(a, b, c)
//     rtx a ATTRIBUTE_UNUSED;
//     rtx b ATTRIBUTE_UNUSED;
//     rtx c ATTRIBUTE_UNUSED;
gen_sle_si_reg(rtx a, rtx b, rtx c)
{
  return 0;
}
static inline rtx gen_sle_di_reg                    PARAMS ((rtx, rtx, rtx));
static inline rtx
//gen_sle_di_reg(a, b, c)
//     rtx a ATTRIBUTE_UNUSED;
//     rtx b ATTRIBUTE_UNUSED;
//     rtx c ATTRIBUTE_UNUSED;
gen_sle_di_reg(rtx a, rtx b, rtx c)
{
  return 0;
}
extern rtx        gen_sgtu_si                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sgtu_di                       PARAMS ((rtx, rtx, rtx));
static inline rtx gen_sgeu_si                       PARAMS ((rtx, rtx, rtx));
static inline rtx
//gen_sgeu_si(a, b, c)
//     rtx a ATTRIBUTE_UNUSED;
//     rtx b ATTRIBUTE_UNUSED;
//     rtx c ATTRIBUTE_UNUSED;
gen_sgeu_si(rtx a, rtx b, rtx c)
{
  return 0;
}
static inline rtx gen_sgeu_di                       PARAMS ((rtx, rtx, rtx));
static inline rtx
//gen_sgeu_di(a, b, c)
//     rtx a ATTRIBUTE_UNUSED;
//     rtx b ATTRIBUTE_UNUSED;
//     rtx c ATTRIBUTE_UNUSED;
gen_sgeu_di(rtx a, rtx b, rtx c)
{
  return 0;
}
extern rtx        gen_sltu_si                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sltu_di                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sleu_si_const                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sleu_di_const                 PARAMS ((rtx, rtx, rtx));
static inline rtx gen_sleu_si_reg                   PARAMS ((rtx, rtx, rtx));
static inline rtx
//gen_sleu_si_reg(a, b, c)
//     rtx a ATTRIBUTE_UNUSED;
//     rtx b ATTRIBUTE_UNUSED;
//     rtx c ATTRIBUTE_UNUSED;
gen_sleu_si_reg(rtx a, rtx b, rtx c)
{
  return 0;
}
static inline rtx gen_sleu_di_reg                   PARAMS ((rtx, rtx, rtx));
static inline rtx
//gen_sleu_di_reg(a, b, c)
//     rtx a ATTRIBUTE_UNUSED;
//     rtx b ATTRIBUTE_UNUSED;
//     rtx c ATTRIBUTE_UNUSED;
gen_sleu_di_reg(rtx a, rtx b, rtx c)
{
  return 0;
}
extern rtx        gen_sunordered_df                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sunlt_df                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_suneq_df                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sunle_df                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_seq_df                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_slt_df                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sle_df                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sgt_df                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sge_df                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sunordered_sf                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sunlt_sf                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_suneq_sf                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sunle_sf                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_seq_sf                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_slt_sf                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sle_sf                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sgt_sf                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sge_sf                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_jump                          PARAMS ((rtx));
extern rtx        gen_indirect_jump_internal1       PARAMS ((rtx));
extern rtx        gen_indirect_jump_internal2       PARAMS ((rtx));
extern rtx        gen_tablejump_internal1           PARAMS ((rtx, rtx));
extern rtx        gen_tablejump_internal2           PARAMS ((rtx, rtx));
extern rtx        gen_casesi_internal               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_casesi_internal_di            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_blockage                      PARAMS ((void));
extern rtx        gen_return                        PARAMS ((void));
extern rtx        gen_return_internal               PARAMS ((rtx));
extern rtx        gen_get_fnaddr                    PARAMS ((rtx, rtx));
extern rtx        gen_eh_set_lr_si                  PARAMS ((rtx));
extern rtx        gen_eh_set_lr_di                  PARAMS ((rtx));
extern rtx        gen_exception_receiver            PARAMS ((void));
extern rtx        gen_call_internal1                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_call_internal2                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_call_internal3a               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_call_internal3b               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_call_internal3c               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_call_internal4a               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_call_internal4b               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_call_value_internal1          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_call_value_internal2          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_call_value_internal3a         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_call_value_internal3b         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_call_value_internal3c         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_call_value_internal4a         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_call_value_internal4b         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_call_value_multiple_internal1 PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_call_value_multiple_internal2 PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_prefetch_si_address           PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_prefetch_si                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_prefetch_di_address           PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_prefetch_di                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_nop                           PARAMS ((void));
extern rtx        gen_consttable_qi                 PARAMS ((rtx));
extern rtx        gen_consttable_hi                 PARAMS ((rtx));
extern rtx        gen_consttable_si                 PARAMS ((rtx));
extern rtx        gen_consttable_di                 PARAMS ((rtx));
extern rtx        gen_consttable_sf                 PARAMS ((rtx));
extern rtx        gen_consttable_df                 PARAMS ((rtx));
extern rtx        gen_align_2                       PARAMS ((void));
extern rtx        gen_align_4                       PARAMS ((void));
extern rtx        gen_align_8                       PARAMS ((void));
extern rtx        gen_leasi                         PARAMS ((rtx, rtx));
extern rtx        gen_leadi                         PARAMS ((rtx, rtx));
extern rtx        gen_conditional_trap              PARAMS ((rtx, rtx));
extern rtx        gen_addsi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_adddi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subsi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subdi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_muldf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsf3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_muldi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsidi3                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umulsidi3                     PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smulsi3_highpart              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umulsi3_highpart              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divmodsi4                     PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_divmoddi4                     PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_udivmodsi4                    PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_udivmoddi4                    PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_div_trap                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divsi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divdi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_modsi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_moddi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_udivsi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_udivdi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umodsi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umoddi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_negdi2                        PARAMS ((rtx, rtx));
extern rtx        gen_andsi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_anddi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_iorsi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_iordi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xorsi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xordi3                        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_zero_extendsidi2              PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendhisi2              PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendhidi2              PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendqihi2              PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendqisi2              PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendqidi2              PARAMS ((rtx, rtx));
extern rtx        gen_extendsidi2                   PARAMS ((rtx, rtx));
extern rtx        gen_extendhidi2                   PARAMS ((rtx, rtx));
extern rtx        gen_extendhisi2                   PARAMS ((rtx, rtx));
extern rtx        gen_extendqihi2                   PARAMS ((rtx, rtx));
extern rtx        gen_extendqisi2                   PARAMS ((rtx, rtx));
extern rtx        gen_extendqidi2                   PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncdfsi2                PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncsfsi2                PARAMS ((rtx, rtx));
extern rtx        gen_fixuns_truncdfsi2             PARAMS ((rtx, rtx));
extern rtx        gen_fixuns_truncdfdi2             PARAMS ((rtx, rtx));
extern rtx        gen_fixuns_truncsfsi2             PARAMS ((rtx, rtx));
extern rtx        gen_fixuns_truncsfdi2             PARAMS ((rtx, rtx));
extern rtx        gen_extv                          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_extzv                         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_insv                          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movdi                         PARAMS ((rtx, rtx));
extern rtx        gen_reload_indi                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_reload_outdi                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_movsi                         PARAMS ((rtx, rtx));
extern rtx        gen_reload_outsi                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_reload_insi                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_reload_incc                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_reload_outcc                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_movhi                         PARAMS ((rtx, rtx));
extern rtx        gen_movqi                         PARAMS ((rtx, rtx));
extern rtx        gen_movsf                         PARAMS ((rtx, rtx));
extern rtx        gen_movdf                         PARAMS ((rtx, rtx));
extern rtx        gen_movstrsi                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_ashlsi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashldi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrsi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrdi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrsi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrdi3                       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_cmpsi                         PARAMS ((rtx, rtx));
extern rtx        gen_tstsi                         PARAMS ((rtx));
extern rtx        gen_cmpdi                         PARAMS ((rtx, rtx));
extern rtx        gen_tstdi                         PARAMS ((rtx));
extern rtx        gen_cmpdf                         PARAMS ((rtx, rtx));
extern rtx        gen_cmpsf                         PARAMS ((rtx, rtx));
extern rtx        gen_bunordered                    PARAMS ((rtx));
extern rtx        gen_bordered                      PARAMS ((rtx));
extern rtx        gen_bunlt                         PARAMS ((rtx));
extern rtx        gen_bunge                         PARAMS ((rtx));
extern rtx        gen_buneq                         PARAMS ((rtx));
extern rtx        gen_bltgt                         PARAMS ((rtx));
extern rtx        gen_bunle                         PARAMS ((rtx));
extern rtx        gen_bungt                         PARAMS ((rtx));
extern rtx        gen_beq                           PARAMS ((rtx));
extern rtx        gen_bne                           PARAMS ((rtx));
extern rtx        gen_bgt                           PARAMS ((rtx));
extern rtx        gen_bge                           PARAMS ((rtx));
extern rtx        gen_blt                           PARAMS ((rtx));
extern rtx        gen_ble                           PARAMS ((rtx));
extern rtx        gen_bgtu                          PARAMS ((rtx));
extern rtx        gen_bgeu                          PARAMS ((rtx));
extern rtx        gen_bltu                          PARAMS ((rtx));
extern rtx        gen_bleu                          PARAMS ((rtx));
extern rtx        gen_seq                           PARAMS ((rtx));
extern rtx        gen_sne                           PARAMS ((rtx));
extern rtx        gen_sgt                           PARAMS ((rtx));
extern rtx        gen_sge                           PARAMS ((rtx));
extern rtx        gen_slt                           PARAMS ((rtx));
extern rtx        gen_sle                           PARAMS ((rtx));
extern rtx        gen_sgtu                          PARAMS ((rtx));
extern rtx        gen_sgeu                          PARAMS ((rtx));
extern rtx        gen_sltu                          PARAMS ((rtx));
extern rtx        gen_sleu                          PARAMS ((rtx));
extern rtx        gen_indirect_jump                 PARAMS ((rtx));
extern rtx        gen_tablejump                     PARAMS ((rtx, rtx));
extern rtx        gen_tablejump_internal3           PARAMS ((rtx, rtx));
extern rtx        gen_tablejump_mips161             PARAMS ((rtx, rtx));
extern rtx        gen_tablejump_mips162             PARAMS ((rtx, rtx));
extern rtx        gen_tablejump_internal4           PARAMS ((rtx, rtx));
extern rtx        gen_casesi                        PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_builtin_setjmp_setup          PARAMS ((rtx));
extern rtx        gen_builtin_setjmp_setup_32       PARAMS ((rtx));
extern rtx        gen_builtin_setjmp_setup_64       PARAMS ((rtx));
extern rtx        gen_builtin_longjmp               PARAMS ((rtx));
extern rtx        gen_prologue                      PARAMS ((void));
extern rtx        gen_epilogue                      PARAMS ((void));
extern rtx        gen_eh_return                     PARAMS ((rtx, rtx));
#define GEN_CALL(A, B, C, D) gen_call ((A), (B), (C), (D))
extern rtx        gen_call                          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_call_internal0                PARAMS ((rtx, rtx, rtx));
#define GEN_CALL_VALUE(A, B, C, D, E) gen_call_value ((A), (B), (C), (D))
extern rtx        gen_call_value                    PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_call_value_internal0          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_call_value_multiple_internal0 PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_untyped_call                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_prefetch                      PARAMS ((rtx, rtx, rtx));
extern rtx        gen_movsicc                       PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movdicc                       PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movsfcc                       PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movdfcc                       PARAMS ((rtx, rtx, rtx, rtx));

#endif /* GCC_INSN_FLAGS_H */

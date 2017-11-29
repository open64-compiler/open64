/* Generated automatically by the program `genflags'
   from the machine description file `md'.  */

#ifndef GCC_INSN_FLAGS_H
#define GCC_INSN_FLAGS_H

#define HAVE_movbi 1
#define HAVE_movsi_symbolic 1
#define HAVE_movdi_symbolic 1
#define HAVE_load_gprel 1
#define HAVE_gprel64_offset 1
#define HAVE_load_ltoff_dtpmod 1
#define HAVE_load_ltoff_dtprel 1
#define HAVE_load_ltoff_tprel 1
#define HAVE_extendqidi2 1
#define HAVE_extendhidi2 1
#define HAVE_extendsidi2 1
#define HAVE_zero_extendqidi2 1
#define HAVE_zero_extendhidi2 1
#define HAVE_zero_extendsidi2 1
#define HAVE_extendsfdf2 1
#define HAVE_extendsftf2 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_extenddftf2 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_truncdfsf2 1
#define HAVE_trunctfsf2 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_trunctfdf2 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_floatditf2 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_floatdidf2 (!INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_floatdisf2 (!INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_fix_truncsfdi2 1
#define HAVE_fix_truncdfdi2 1
#define HAVE_fix_trunctfdi2 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_fix_trunctfdi2_alts (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_floatunsdisf2 1
#define HAVE_floatunsdidf2 1
#define HAVE_floatunsditf2 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_fixuns_truncsfdi2 1
#define HAVE_fixuns_truncdfdi2 1
#define HAVE_fixuns_trunctfdi2 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_fixuns_trunctfdi2_alts (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_extv 1
#define HAVE_extzv 1
#define HAVE_shift_mix4left 1
#define HAVE_mix4right 1
#define HAVE_andbi3 1
#define HAVE_iorbi3 1
#define HAVE_one_cmplbi2 1
#define HAVE_mulhi3 1
#define HAVE_addsi3 1
#define HAVE_subsi3 1
#define HAVE_mulsi3 1
#define HAVE_maddsi4 1
#define HAVE_negsi2 1
#define HAVE_divsi3_internal (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV)
#define HAVE_adddi3 1
#define HAVE_subdi3 1
#define HAVE_muldi3 1
#define HAVE_madddi4 1
#define HAVE_smuldi3_highpart 1
#define HAVE_umuldi3_highpart 1
#define HAVE_negdi2 1
#define HAVE_divdi3_internal_lat (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_LAT)
#define HAVE_divdi3_internal_thr (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_THR)
#define HAVE_addsf3 1
#define HAVE_subsf3 1
#define HAVE_mulsf3 1
#define HAVE_abssf2 1
#define HAVE_negsf2 1
#define HAVE_minsf3 1
#define HAVE_maxsf3 1
#define HAVE_divsf3_internal_lat (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT)
#define HAVE_divsf3_internal_thr (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR)
#define HAVE_adddf3 1
#define HAVE_subdf3 1
#define HAVE_muldf3 1
#define HAVE_absdf2 1
#define HAVE_negdf2 1
#define HAVE_mindf3 1
#define HAVE_maxdf3 1
#define HAVE_divdf3_internal_lat (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT)
#define HAVE_divdf3_internal_thr (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR)
#define HAVE_addtf3 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_subtf3 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_multf3 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_abstf2 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_negtf2 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_mintf3 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_maxtf3 (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_divtf3_internal_lat (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT)
#define HAVE_divtf3_internal_thr (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR)
#define HAVE_ashldi3 1
#define HAVE_ashrdi3 1
#define HAVE_lshrdi3 1
#define HAVE_one_cmplsi2 1
#define HAVE_anddi3 1
#define HAVE_iordi3 1
#define HAVE_xordi3 1
#define HAVE_one_cmpldi2 1
#define HAVE_doloop_end_internal 1
#define HAVE_call_nogp 1
#define HAVE_call_value_nogp 1
#define HAVE_sibcall_nogp 1
#define HAVE_call_gp 1
#define HAVE_call_value_gp 1
#define HAVE_sibcall_gp 1
#define HAVE_return_internal 1
#define HAVE_return (ia64_direct_return ())
#define HAVE_jump 1
#define HAVE_indirect_jump 1
#define HAVE_prologue_allocate_stack 1
#define HAVE_epilogue_deallocate_stack 1
#define HAVE_prologue_use 1
#define HAVE_alloc 1
#define HAVE_gr_spill_internal 1
#define HAVE_gr_restore_internal 1
#define HAVE_fr_spill 1
#define HAVE_fr_restore 1
#define HAVE_bsp_value 1
#define HAVE_set_bsp 1
#define HAVE_flushrs 1
#define HAVE_nop 1
#define HAVE_nop_m 1
#define HAVE_nop_i 1
#define HAVE_nop_f 1
#define HAVE_nop_b 1
#define HAVE_nop_x 1
#define HAVE_bundle_selector 1
#define HAVE_blockage 1
#define HAVE_insn_group_barrier 1
#define HAVE_break_f 1
#define HAVE_prefetch 1
#define HAVE_builtin_setjmp_receiver 1
#define HAVE_fetchadd_acq_si 1
#define HAVE_fetchadd_acq_di 1
#define HAVE_cmpxchg_acq_si 1
#define HAVE_cmpxchg_acq_di 1
#define HAVE_xchgsi 1
#define HAVE_xchgdi 1
#define HAVE_pred_rel_mutex 1
#define HAVE_safe_across_calls_all 1
#define HAVE_safe_across_calls_normal 1
#define HAVE_ptr_extend 1
#define HAVE_movqi 1
#define HAVE_movhi 1
#define HAVE_movsi 1
#define HAVE_movdi 1
#define HAVE_load_fptr 1
#define HAVE_load_gprel64 1
#define HAVE_load_symptr 1
#define HAVE_load_dtprel 1
#define HAVE_add_dtprel (!TARGET_TLS64)
#define HAVE_load_tprel 1
#define HAVE_add_tprel (!TARGET_TLS64)
#define HAVE_movti 1
#define HAVE_reload_inti 1
#define HAVE_reload_outti 1
#define HAVE_movsf 1
#define HAVE_movdf 1
#define HAVE_movtf (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_insv 1
#define HAVE_abssi2 1
#define HAVE_sminsi3 1
#define HAVE_smaxsi3 1
#define HAVE_uminsi3 1
#define HAVE_umaxsi3 1
#define HAVE_divsi3 (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV)
#define HAVE_modsi3 (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV)
#define HAVE_udivsi3 (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV)
#define HAVE_umodsi3 (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV)
#define HAVE_absdi2 1
#define HAVE_smindi3 1
#define HAVE_smaxdi3 1
#define HAVE_umindi3 1
#define HAVE_umaxdi3 1
#define HAVE_ffsdi2 1
#define HAVE_divdi3 (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV)
#define HAVE_moddi3 (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV)
#define HAVE_udivdi3 (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV)
#define HAVE_umoddi3 (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV)
#define HAVE_divsf3 (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV)
#define HAVE_divdf3 (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV)
#define HAVE_divtf3 (INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV)
#define HAVE_ashlsi3 1
#define HAVE_ashrsi3 1
#define HAVE_lshrsi3 1
#define HAVE_rotrsi3 1
#define HAVE_rotlsi3 1
#define HAVE_rotrdi3 1
#define HAVE_rotldi3 1
#define HAVE_cmpbi 1
#define HAVE_cmpsi 1
#define HAVE_cmpdi 1
#define HAVE_cmpsf 1
#define HAVE_cmpdf 1
#define HAVE_cmptf (INTEL_EXTENDED_IEEE_FORMAT)
#define HAVE_beq 1
#define HAVE_bne 1
#define HAVE_blt 1
#define HAVE_ble 1
#define HAVE_bgt 1
#define HAVE_bge 1
#define HAVE_bltu 1
#define HAVE_bleu 1
#define HAVE_bgtu 1
#define HAVE_bgeu 1
#define HAVE_bunordered 1
#define HAVE_bordered 1
#define HAVE_doloop_end 1
#define HAVE_seq 1
#define HAVE_sne 1
#define HAVE_slt 1
#define HAVE_sle 1
#define HAVE_sgt 1
#define HAVE_sge 1
#define HAVE_sltu 1
#define HAVE_sleu 1
#define HAVE_sgtu 1
#define HAVE_sgeu 1
#define HAVE_sunordered 1
#define HAVE_sordered 1
#define HAVE_call 1
#define HAVE_sibcall 1
#define HAVE_call_value 1
#define HAVE_sibcall_value 1
#define HAVE_untyped_call 1
#define HAVE_tablejump 1
#define HAVE_prologue 1
#define HAVE_epilogue 1
#define HAVE_sibcall_epilogue 1
#define HAVE_gr_spill 1
#define HAVE_gr_restore 1
#define HAVE_trap 1
#define HAVE_conditional_trap 1
#define HAVE_save_stack_nonlocal 1
#define HAVE_nonlocal_goto 1
#define HAVE_eh_epilogue 1
#define HAVE_restore_stack_nonlocal 1
#define HAVE_mf 1
extern rtx        gen_movbi                     PARAMS ((rtx, rtx));
extern rtx        gen_movsi_symbolic            PARAMS ((rtx, rtx));
extern rtx        gen_movdi_symbolic            PARAMS ((rtx, rtx));
extern rtx        gen_load_gprel                PARAMS ((rtx, rtx));
extern rtx        gen_gprel64_offset            PARAMS ((rtx, rtx));
extern rtx        gen_load_ltoff_dtpmod         PARAMS ((rtx, rtx));
extern rtx        gen_load_ltoff_dtprel         PARAMS ((rtx, rtx));
extern rtx        gen_load_ltoff_tprel          PARAMS ((rtx, rtx));
extern rtx        gen_extendqidi2               PARAMS ((rtx, rtx));
extern rtx        gen_extendhidi2               PARAMS ((rtx, rtx));
extern rtx        gen_extendsidi2               PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendqidi2          PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendhidi2          PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendsidi2          PARAMS ((rtx, rtx));
extern rtx        gen_extendsfdf2               PARAMS ((rtx, rtx));
extern rtx        gen_extendsftf2               PARAMS ((rtx, rtx));
extern rtx        gen_extenddftf2               PARAMS ((rtx, rtx));
extern rtx        gen_truncdfsf2                PARAMS ((rtx, rtx));
extern rtx        gen_trunctfsf2                PARAMS ((rtx, rtx));
extern rtx        gen_trunctfdf2                PARAMS ((rtx, rtx));
extern rtx        gen_floatditf2                PARAMS ((rtx, rtx));
extern rtx        gen_floatdidf2                PARAMS ((rtx, rtx));
extern rtx        gen_floatdisf2                PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncsfdi2            PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncdfdi2            PARAMS ((rtx, rtx));
extern rtx        gen_fix_trunctfdi2            PARAMS ((rtx, rtx));
extern rtx        gen_fix_trunctfdi2_alts       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_floatunsdisf2             PARAMS ((rtx, rtx));
extern rtx        gen_floatunsdidf2             PARAMS ((rtx, rtx));
extern rtx        gen_floatunsditf2             PARAMS ((rtx, rtx));
extern rtx        gen_fixuns_truncsfdi2         PARAMS ((rtx, rtx));
extern rtx        gen_fixuns_truncdfdi2         PARAMS ((rtx, rtx));
extern rtx        gen_fixuns_trunctfdi2         PARAMS ((rtx, rtx));
extern rtx        gen_fixuns_trunctfdi2_alts    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_extv                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_extzv                     PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_shift_mix4left            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mix4right                 PARAMS ((rtx, rtx));
extern rtx        gen_andbi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_iorbi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_one_cmplbi2               PARAMS ((rtx, rtx));
extern rtx        gen_mulhi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_maddsi4                   PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_negsi2                    PARAMS ((rtx, rtx));
extern rtx        gen_divsi3_internal           PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_adddi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subdi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_muldi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_madddi4                   PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_smuldi3_highpart          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umuldi3_highpart          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_negdi2                    PARAMS ((rtx, rtx));
extern rtx        gen_divdi3_internal_lat       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divdi3_internal_thr       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addsf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subsf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_abssf2                    PARAMS ((rtx, rtx));
extern rtx        gen_negsf2                    PARAMS ((rtx, rtx));
extern rtx        gen_minsf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_maxsf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divsf3_internal_lat       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divsf3_internal_thr       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_adddf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subdf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_muldf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_absdf2                    PARAMS ((rtx, rtx));
extern rtx        gen_negdf2                    PARAMS ((rtx, rtx));
extern rtx        gen_mindf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_maxdf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divdf3_internal_lat       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divdf3_internal_thr       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addtf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subtf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_multf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_abstf2                    PARAMS ((rtx, rtx));
extern rtx        gen_negtf2                    PARAMS ((rtx, rtx));
extern rtx        gen_mintf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_maxtf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divtf3_internal_lat       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divtf3_internal_thr       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashldi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrdi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrdi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_one_cmplsi2               PARAMS ((rtx, rtx));
extern rtx        gen_anddi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_iordi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xordi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_one_cmpldi2               PARAMS ((rtx, rtx));
extern rtx        gen_doloop_end_internal       PARAMS ((rtx, rtx));
extern rtx        gen_call_nogp                 PARAMS ((rtx, rtx));
extern rtx        gen_call_value_nogp           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sibcall_nogp              PARAMS ((rtx));
extern rtx        gen_call_gp                   PARAMS ((rtx, rtx));
extern rtx        gen_call_value_gp             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sibcall_gp                PARAMS ((rtx));
extern rtx        gen_return_internal           PARAMS ((rtx));
extern rtx        gen_return                    PARAMS ((void));
extern rtx        gen_jump                      PARAMS ((rtx));
extern rtx        gen_indirect_jump             PARAMS ((rtx));
extern rtx        gen_prologue_allocate_stack   PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_epilogue_deallocate_stack PARAMS ((rtx, rtx));
extern rtx        gen_prologue_use              PARAMS ((rtx));
extern rtx        gen_alloc                     PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_gr_spill_internal         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_gr_restore_internal       PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_fr_spill                  PARAMS ((rtx, rtx));
extern rtx        gen_fr_restore                PARAMS ((rtx, rtx));
extern rtx        gen_bsp_value                 PARAMS ((rtx));
extern rtx        gen_set_bsp                   PARAMS ((rtx));
extern rtx        gen_flushrs                   PARAMS ((void));
extern rtx        gen_nop                       PARAMS ((void));
extern rtx        gen_nop_m                     PARAMS ((void));
extern rtx        gen_nop_i                     PARAMS ((void));
extern rtx        gen_nop_f                     PARAMS ((void));
extern rtx        gen_nop_b                     PARAMS ((void));
extern rtx        gen_nop_x                     PARAMS ((void));
extern rtx        gen_bundle_selector           PARAMS ((rtx));
extern rtx        gen_blockage                  PARAMS ((void));
extern rtx        gen_insn_group_barrier        PARAMS ((rtx));
extern rtx        gen_break_f                   PARAMS ((void));
extern rtx        gen_prefetch                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_builtin_setjmp_receiver   PARAMS ((rtx));
extern rtx        gen_fetchadd_acq_si           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_fetchadd_acq_di           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_cmpxchg_acq_si            PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_cmpxchg_acq_di            PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_xchgsi                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xchgdi                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_pred_rel_mutex            PARAMS ((rtx));
extern rtx        gen_safe_across_calls_all     PARAMS ((void));
extern rtx        gen_safe_across_calls_normal  PARAMS ((void));
extern rtx        gen_ptr_extend                PARAMS ((rtx, rtx));
extern rtx        gen_movqi                     PARAMS ((rtx, rtx));
extern rtx        gen_movhi                     PARAMS ((rtx, rtx));
extern rtx        gen_movsi                     PARAMS ((rtx, rtx));
extern rtx        gen_movdi                     PARAMS ((rtx, rtx));
extern rtx        gen_load_fptr                 PARAMS ((rtx, rtx));
extern rtx        gen_load_gprel64              PARAMS ((rtx, rtx));
extern rtx        gen_load_symptr               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_load_dtprel               PARAMS ((rtx, rtx));
extern rtx        gen_add_dtprel                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_load_tprel                PARAMS ((rtx, rtx));
extern rtx        gen_add_tprel                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_movti                     PARAMS ((rtx, rtx));
extern rtx        gen_reload_inti               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_reload_outti              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_movsf                     PARAMS ((rtx, rtx));
extern rtx        gen_movdf                     PARAMS ((rtx, rtx));
extern rtx        gen_movtf                     PARAMS ((rtx, rtx));
extern rtx        gen_insv                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_abssi2                    PARAMS ((rtx, rtx));
extern rtx        gen_sminsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smaxsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_uminsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umaxsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_modsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_udivsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umodsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_absdi2                    PARAMS ((rtx, rtx));
extern rtx        gen_smindi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smaxdi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umindi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umaxdi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ffsdi2                    PARAMS ((rtx, rtx));
extern rtx        gen_divdi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_moddi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_udivdi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umoddi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divsf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divdf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divtf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotrsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotlsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotrdi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotldi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_cmpbi                     PARAMS ((rtx, rtx));
extern rtx        gen_cmpsi                     PARAMS ((rtx, rtx));
extern rtx        gen_cmpdi                     PARAMS ((rtx, rtx));
extern rtx        gen_cmpsf                     PARAMS ((rtx, rtx));
extern rtx        gen_cmpdf                     PARAMS ((rtx, rtx));
extern rtx        gen_cmptf                     PARAMS ((rtx, rtx));
extern rtx        gen_beq                       PARAMS ((rtx));
extern rtx        gen_bne                       PARAMS ((rtx));
extern rtx        gen_blt                       PARAMS ((rtx));
extern rtx        gen_ble                       PARAMS ((rtx));
extern rtx        gen_bgt                       PARAMS ((rtx));
extern rtx        gen_bge                       PARAMS ((rtx));
extern rtx        gen_bltu                      PARAMS ((rtx));
extern rtx        gen_bleu                      PARAMS ((rtx));
extern rtx        gen_bgtu                      PARAMS ((rtx));
extern rtx        gen_bgeu                      PARAMS ((rtx));
extern rtx        gen_bunordered                PARAMS ((rtx));
extern rtx        gen_bordered                  PARAMS ((rtx));
extern rtx        gen_doloop_end                PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_seq                       PARAMS ((rtx));
extern rtx        gen_sne                       PARAMS ((rtx));
extern rtx        gen_slt                       PARAMS ((rtx));
extern rtx        gen_sle                       PARAMS ((rtx));
extern rtx        gen_sgt                       PARAMS ((rtx));
extern rtx        gen_sge                       PARAMS ((rtx));
extern rtx        gen_sltu                      PARAMS ((rtx));
extern rtx        gen_sleu                      PARAMS ((rtx));
extern rtx        gen_sgtu                      PARAMS ((rtx));
extern rtx        gen_sgeu                      PARAMS ((rtx));
extern rtx        gen_sunordered                PARAMS ((rtx));
extern rtx        gen_sordered                  PARAMS ((rtx));
#define GEN_CALL(A, B, C, D) gen_call ((A), (B), (C), (D))
extern rtx        gen_call                      PARAMS ((rtx, rtx, rtx, rtx));
#define GEN_SIBCALL(A, B, C, D) gen_sibcall ((A), (B), (C), (D))
extern rtx        gen_sibcall                   PARAMS ((rtx, rtx, rtx, rtx));
#define GEN_CALL_VALUE(A, B, C, D, E) gen_call_value ((A), (B), (C), (D), (E))
extern rtx        gen_call_value                PARAMS ((rtx, rtx, rtx, rtx, rtx));
#define GEN_SIBCALL_VALUE(A, B, C, D, E) gen_sibcall_value ((A), (B), (C), (D), (E))
extern rtx        gen_sibcall_value             PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_untyped_call              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_tablejump                 PARAMS ((rtx, rtx));
extern rtx        gen_prologue                  PARAMS ((void));
extern rtx        gen_epilogue                  PARAMS ((void));
extern rtx        gen_sibcall_epilogue          PARAMS ((void));
extern rtx        gen_gr_spill                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_gr_restore                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_trap                      PARAMS ((void));
extern rtx        gen_conditional_trap          PARAMS ((rtx, rtx));
extern rtx        gen_save_stack_nonlocal       PARAMS ((rtx, rtx));
extern rtx        gen_nonlocal_goto             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_eh_epilogue               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_restore_stack_nonlocal    PARAMS ((rtx, rtx));
extern rtx        gen_mf                        PARAMS ((void));

#endif /* GCC_INSN_FLAGS_H */

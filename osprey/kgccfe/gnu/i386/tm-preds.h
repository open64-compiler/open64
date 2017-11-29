/* Generated automatically by the program `genpreds'.  */

#ifndef GCC_TM_PREDS_H
#define GCC_TM_PREDS_H

#ifdef RTX_CODE

extern int x86_64_immediate_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_nonmemory_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_movabs_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_szext_nonmemory_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_general_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_szext_general_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_zext_immediate_operand PARAMS ((rtx, enum machine_mode));
extern int shiftdi_operand PARAMS ((rtx, enum machine_mode));
extern int const_int_1_operand PARAMS ((rtx, enum machine_mode));
extern int const_int_1_31_operand PARAMS ((rtx, enum machine_mode));
extern int symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int aligned_operand PARAMS ((rtx, enum machine_mode));
extern int pic_symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int call_insn_operand PARAMS ((rtx, enum machine_mode));
extern int constant_call_address_operand PARAMS ((rtx, enum machine_mode));
extern int const0_operand PARAMS ((rtx, enum machine_mode));
extern int const1_operand PARAMS ((rtx, enum machine_mode));
extern int const248_operand PARAMS ((rtx, enum machine_mode));
extern int incdec_operand PARAMS ((rtx, enum machine_mode));
extern int mmx_reg_operand PARAMS ((rtx, enum machine_mode));
extern int reg_no_sp_operand PARAMS ((rtx, enum machine_mode));
extern int general_no_elim_operand PARAMS ((rtx, enum machine_mode));
extern int nonmemory_no_elim_operand PARAMS ((rtx, enum machine_mode));
extern int index_register_operand PARAMS ((rtx, enum machine_mode));
extern int q_regs_operand PARAMS ((rtx, enum machine_mode));
extern int non_q_regs_operand PARAMS ((rtx, enum machine_mode));
extern int fcmov_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int sse_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int ix86_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int cmp_fp_expander_operand PARAMS ((rtx, enum machine_mode));
extern int ext_register_operand PARAMS ((rtx, enum machine_mode));
extern int binary_fp_operator PARAMS ((rtx, enum machine_mode));
extern int mult_operator PARAMS ((rtx, enum machine_mode));
extern int div_operator PARAMS ((rtx, enum machine_mode));
extern int arith_or_logical_operator PARAMS ((rtx, enum machine_mode));
extern int promotable_binary_operator PARAMS ((rtx, enum machine_mode));
extern int memory_displacement_operand PARAMS ((rtx, enum machine_mode));
extern int cmpsi_operand PARAMS ((rtx, enum machine_mode));
extern int long_memory_operand PARAMS ((rtx, enum machine_mode));
extern int tls_symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int global_dynamic_symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int local_dynamic_symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int initial_exec_symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int local_exec_symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int any_fp_register_operand PARAMS ((rtx, enum machine_mode));
extern int register_and_not_any_fp_reg_operand PARAMS ((rtx, enum machine_mode));
extern int fp_register_operand PARAMS ((rtx, enum machine_mode));
extern int register_and_not_fp_reg_operand PARAMS ((rtx, enum machine_mode));
extern int vector_move_operand PARAMS ((rtx, enum machine_mode));
extern int no_seg_address_operand PARAMS ((rtx, enum machine_mode));

#endif /* RTX_CODE */

#endif /* GCC_TM_PREDS_H */

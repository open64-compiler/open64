/* Generated automatically by the program `genpreds'.  */

#ifndef GCC_TM_PREDS_H
#define GCC_TM_PREDS_H

#ifdef RTX_CODE

extern int any_operand PARAMS ((rtx, enum machine_mode));
extern int zero_constant PARAMS ((rtx, enum machine_mode));
extern int short_cint_operand PARAMS ((rtx, enum machine_mode));
extern int u_short_cint_operand PARAMS ((rtx, enum machine_mode));
extern int non_short_cint_operand PARAMS ((rtx, enum machine_mode));
extern int exact_log2_cint_operand PARAMS ((rtx, enum machine_mode));
extern int gpc_reg_operand PARAMS ((rtx, enum machine_mode));
extern int cc_reg_operand PARAMS ((rtx, enum machine_mode));
extern int cc_reg_not_cr0_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_short_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_neg_short_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_aligned_short_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_u_short_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_cint_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_arith_cint_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_add_cint64_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_sub_cint64_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_logical_cint_operand PARAMS ((rtx, enum machine_mode));
extern int got_operand PARAMS ((rtx, enum machine_mode));
extern int got_no_const_operand PARAMS ((rtx, enum machine_mode));
extern int easy_fp_constant PARAMS ((rtx, enum machine_mode));
extern int zero_fp_constant PARAMS ((rtx, enum machine_mode));
extern int reg_or_mem_operand PARAMS ((rtx, enum machine_mode));
extern int lwa_operand PARAMS ((rtx, enum machine_mode));
extern int volatile_mem_operand PARAMS ((rtx, enum machine_mode));
extern int offsettable_mem_operand PARAMS ((rtx, enum machine_mode));
extern int mem_or_easy_const_operand PARAMS ((rtx, enum machine_mode));
extern int add_operand PARAMS ((rtx, enum machine_mode));
extern int non_add_cint_operand PARAMS ((rtx, enum machine_mode));
extern int and_operand PARAMS ((rtx, enum machine_mode));
extern int and64_operand PARAMS ((rtx, enum machine_mode));
extern int and64_2_operand PARAMS ((rtx, enum machine_mode));
extern int logical_operand PARAMS ((rtx, enum machine_mode));
extern int non_logical_cint_operand PARAMS ((rtx, enum machine_mode));
extern int mask_operand PARAMS ((rtx, enum machine_mode));
extern int mask_operand_wrap PARAMS ((rtx, enum machine_mode));
extern int mask64_operand PARAMS ((rtx, enum machine_mode));
extern int mask64_2_operand PARAMS ((rtx, enum machine_mode));
extern int count_register_operand PARAMS ((rtx, enum machine_mode));
extern int xer_operand PARAMS ((rtx, enum machine_mode));
extern int symbol_ref_operand PARAMS ((rtx, enum machine_mode));
extern int call_operand PARAMS ((rtx, enum machine_mode));
extern int current_file_function_operand PARAMS ((rtx, enum machine_mode));
extern int input_operand PARAMS ((rtx, enum machine_mode));
extern int load_multiple_operation PARAMS ((rtx, enum machine_mode));
extern int store_multiple_operation PARAMS ((rtx, enum machine_mode));
extern int vrsave_operation PARAMS ((rtx, enum machine_mode));
extern int branch_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int branch_positive_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int scc_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int trap_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int boolean_operator PARAMS ((rtx, enum machine_mode));
extern int boolean_or_operator PARAMS ((rtx, enum machine_mode));
extern int altivec_register_operand PARAMS ((rtx, enum machine_mode));
extern int min_max_operator PARAMS ((rtx, enum machine_mode));

#endif /* RTX_CODE */

#endif /* GCC_TM_PREDS_H */

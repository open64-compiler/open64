/* Generated automatically by the program `genpreds'.  */

#ifndef GCC_TM_PREDS_H
#define GCC_TM_PREDS_H

#ifdef RTX_CODE

extern int call_operand PARAMS ((rtx, enum machine_mode));
extern int got_symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int sdata_symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int function_operand PARAMS ((rtx, enum machine_mode));
extern int setjmp_operand PARAMS ((rtx, enum machine_mode));
extern int destination_operand PARAMS ((rtx, enum machine_mode));
extern int not_postinc_memory_operand PARAMS ((rtx, enum machine_mode));
extern int move_operand PARAMS ((rtx, enum machine_mode));
extern int gr_register_operand PARAMS ((rtx, enum machine_mode));
extern int fr_register_operand PARAMS ((rtx, enum machine_mode));
extern int grfr_register_operand PARAMS ((rtx, enum machine_mode));
extern int gr_nonimmediate_operand PARAMS ((rtx, enum machine_mode));
extern int fr_nonimmediate_operand PARAMS ((rtx, enum machine_mode));
extern int grfr_nonimmediate_operand PARAMS ((rtx, enum machine_mode));
extern int gr_reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int gr_reg_or_5bit_operand PARAMS ((rtx, enum machine_mode));
extern int gr_reg_or_6bit_operand PARAMS ((rtx, enum machine_mode));
extern int gr_reg_or_8bit_operand PARAMS ((rtx, enum machine_mode));
extern int grfr_reg_or_8bit_operand PARAMS ((rtx, enum machine_mode));
extern int gr_reg_or_8bit_adjusted_operand PARAMS ((rtx, enum machine_mode));
extern int gr_reg_or_8bit_and_adjusted_operand PARAMS ((rtx, enum machine_mode));
extern int gr_reg_or_14bit_operand PARAMS ((rtx, enum machine_mode));
extern int gr_reg_or_22bit_operand PARAMS ((rtx, enum machine_mode));
extern int shift_count_operand PARAMS ((rtx, enum machine_mode));
extern int shift_32bit_count_operand PARAMS ((rtx, enum machine_mode));
extern int shladd_operand PARAMS ((rtx, enum machine_mode));
extern int fetchadd_operand PARAMS ((rtx, enum machine_mode));
extern int fr_reg_or_fp01_operand PARAMS ((rtx, enum machine_mode));
extern int normal_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int adjusted_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int signed_inequality_operator PARAMS ((rtx, enum machine_mode));
extern int predicate_operator PARAMS ((rtx, enum machine_mode));
extern int condop_operator PARAMS ((rtx, enum machine_mode));
extern int ar_lc_reg_operand PARAMS ((rtx, enum machine_mode));
extern int ar_ccv_reg_operand PARAMS ((rtx, enum machine_mode));
extern int ar_pfs_reg_operand PARAMS ((rtx, enum machine_mode));
extern int general_tfmode_operand PARAMS ((rtx, enum machine_mode));
extern int destination_tfmode_operand PARAMS ((rtx, enum machine_mode));
extern int tfreg_or_fp01_operand PARAMS ((rtx, enum machine_mode));
extern int basereg_operand PARAMS ((rtx, enum machine_mode));

#endif /* RTX_CODE */

#endif /* GCC_TM_PREDS_H */

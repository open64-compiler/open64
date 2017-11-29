/* Generated automatically by the program `genpreds'.  */

#ifndef GCC_TM_PREDS_H
#define GCC_TM_PREDS_H

#ifdef RTX_CODE

extern int uns_arith_operand PARAMS ((rtx, enum machine_mode));
extern int arith_operand PARAMS ((rtx, enum machine_mode));
extern int arith32_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int true_reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int small_int PARAMS ((rtx, enum machine_mode));
extern int large_int PARAMS ((rtx, enum machine_mode));
extern int mips_const_double_ok PARAMS ((rtx, enum machine_mode));
extern int const_float_1_operand PARAMS ((rtx, enum machine_mode));
extern int simple_memory_operand PARAMS ((rtx, enum machine_mode));
extern int equality_op PARAMS ((rtx, enum machine_mode));
extern int cmp_op PARAMS ((rtx, enum machine_mode));
extern int trap_cmp_op PARAMS ((rtx, enum machine_mode));
extern int pc_or_label_operand PARAMS ((rtx, enum machine_mode));
extern int call_insn_operand PARAMS ((rtx, enum machine_mode));
extern int move_operand PARAMS ((rtx, enum machine_mode));
extern int movdi_operand PARAMS ((rtx, enum machine_mode));
extern int se_register_operand PARAMS ((rtx, enum machine_mode));
extern int se_reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int se_uns_arith_operand PARAMS ((rtx, enum machine_mode));
extern int se_arith_operand PARAMS ((rtx, enum machine_mode));
extern int se_nonmemory_operand PARAMS ((rtx, enum machine_mode));
extern int consttable_operand PARAMS ((rtx, enum machine_mode));
extern int fcc_register_operand PARAMS ((rtx, enum machine_mode));
extern int extend_operator PARAMS ((rtx, enum machine_mode));
extern int highpart_shift_operator PARAMS ((rtx, enum machine_mode));

#endif /* RTX_CODE */

#endif /* GCC_TM_PREDS_H */

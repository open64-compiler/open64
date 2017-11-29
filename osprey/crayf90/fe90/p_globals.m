/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* USMID:  "\n@(#)5.0_pl/macros/p_globals.m	5.1	04/29/99 21:22:31\n" */
 

/*****************\
|* MISCELLANEOUS *|
\*****************/

# define BLK_HEAD_IDX			1


/********************\
|* SIZES AND LIMITS *|
\********************/

# define BLK_STK_SIZE			20
# define MAX_BLK_LOOP_NUM	      4095


/******************************\
|* OBJECT REPLACEMENT STRINGS *|
\******************************/

# define CURR_BLK			blk_stk[blk_stk_idx].fld.blk_type
# define CURR_BLK_ERR			blk_stk[blk_stk_idx].fld.blk_err
# define CURR_BLK_FIRST_SH_IDX		blk_stk[blk_stk_idx].fld.first_sh_idx
# define CURR_BLK_FND_DEFAULT		blk_stk[blk_stk_idx].fld.fnd_default
# define CURR_BLK_LABEL			blk_stk[blk_stk_idx].fld.label_idx
# define CURR_BLK_NAME			blk_stk[blk_stk_idx].fld.name_idx
# define CURR_BLK_NO_EXEC		blk_stk[blk_stk_idx].fld.no_exec
# define CURR_BLK_DEF_LINE		blk_stk[blk_stk_idx].fld.def_line
# define CURR_BLK_DEF_COLUMN		blk_stk[blk_stk_idx].fld.def_column

# define BLK_BLOCKABLE_DIR_SH_IDX(IDX)  blk_stk[IDX].fld.blkbl_dir_sh_idx
# define BLK_BLOCKABLE_NEST_OK(IDX)	blk_stk[IDX].fld.blkbl_nest_ok
# define BLK_BLOCKABLE_NUM_LCVS(IDX)	blk_stk[IDX].fld.blkbl_num_lcvs
# define BLK_CASE_DEFAULT_LBL_COL_NUM(IDX)				       \
					blk_stk[IDX].fld.multiuse_opnd.col_num
# define BLK_CASE_DEFAULT_LBL_FLD(IDX)	blk_stk[IDX].fld.multiuse_opnd.fld
# define BLK_CASE_DEFAULT_LBL_IDX(IDX)	blk_stk[IDX].fld.multiuse_opnd.idx
# define BLK_CASE_DEFAULT_LBL_LINE_NUM(IDX)				       \
					blk_stk[IDX].fld.multiuse_opnd.line_num
# define BLK_CASE_DEFAULT_LBL_OPND(IDX)	blk_stk[IDX].fld.multiuse_opnd
# define BLK_CIF_SCOPE_ID(IDX)		blk_stk[IDX].fld.tc_temp_idx
# define BLK_CYCLE_STMT(IDX)		blk_stk[IDX].fld.has_cycle_stmt
# define BLK_DEF_COLUMN(IDX) 		blk_stk[IDX].fld.def_column
# define BLK_DEF_LINE(IDX)		blk_stk[IDX].fld.def_line
# define BLK_DIR_NEST_CHECK_NUM_LCVS(IDX) blk_stk[IDX].fld.dir_nest_num_lcvs
# define BLK_DIR_NEST_CHECK_SH_IDX(IDX)	blk_stk[IDX].fld.dir_nest_ck_sh_idx
# define BLK_DO_TYPE(IDX)		blk_stk[IDX].fld.do_type
# define BLK_DO_VAR_COL_NUM(IDX)	blk_stk[IDX].fld.multiuse_opnd.col_num
# define BLK_DO_VAR_FLD(IDX)		blk_stk[IDX].fld.multiuse_opnd.fld
# define BLK_DO_VAR_IDX(IDX)		blk_stk[IDX].fld.multiuse_opnd.idx
# define BLK_DO_VAR_LINE_NUM(IDX)	blk_stk[IDX].fld.multiuse_opnd.line_num
# define BLK_DO_VAR_OPND(IDX)		blk_stk[IDX].fld.multiuse_opnd
# define BLK_ENDDO_PARALLEL_SH_IDX(IDX) blk_stk[IDX].fld.multiuse_opnd.idx
# define BLK_ENDPDO_SH_IDX(IDX)		blk_stk[IDX].fld.multiuse_opnd.idx
# define BLK_ERR(IDX)			blk_stk[IDX].fld.blk_err
# define BLK_EXIT_STMT(IDX)		blk_stk[IDX].fld.has_exit_stmt
# define BLK_FIRST_SH_IDX(IDX)		blk_stk[IDX].fld.first_sh_idx
# define BLK_FND_DEFAULT(IDX)		blk_stk[IDX].fld.fnd_default
# define BLK_HAS_NESTED_LOOP(IDX)	blk_stk[IDX].fld.has_nested_loop
# define BLK_INC_TEMP_IDX(IDX)		blk_stk[IDX].fld.inc_temp_idx
# define BLK_INDUC_TEMP_IDX(IDX)	blk_stk[IDX].fld.induc_temp_idx
# define BLK_INTERCHANGE_NUM_LCVS(IDX)	blk_stk[IDX].fld.intchg_num_lcvs
# define BLK_INTERCHANGE_DIR_SH_IDX(IDX)  blk_stk[IDX].fld.intchg_dir_sh_idx
# define BLK_IS_PARALLEL_REGION(IDX)	blk_stk[IDX].fld.parallel_region
# define BLK_LABEL(IDX)			blk_stk[IDX].fld.label_idx
# define BLK_LAST_CPNT_IDX(IDX)		blk_stk[IDX].fld_long.field32_5_1
# define BLK_LOOP_NUM(IDX)		blk_stk[IDX].fld.loop_num
# define BLK_NAME(IDX)			blk_stk[IDX].fld.name_idx
# define BLK_NO_EXEC(IDX)		blk_stk[IDX].fld.no_exec
# define BLK_NUM_CASES(IDX)		blk_stk[IDX].fld.top_lbl_idx
#ifdef KEY /* Bug 10572 */
# define BLK_ENUM_EMPTY(IDX)		blk_stk[IDX].fld.skip_lbl_idx
# define BLK_ENUM_COUNTER(IDX)		blk_stk[IDX].fld.top_lbl_idx
#endif /* KEY Bug 10572 */
# define BLK_SKIP_LBL_IDX(IDX)		blk_stk[IDX].fld.skip_lbl_idx
# define BLK_START_TEMP_IDX(IDX)	blk_stk[IDX].fld.start_temp_idx
# define BLK_TC_TEMP_IDX(IDX)		blk_stk[IDX].fld.tc_temp_idx
# define BLK_TOP_LBL_IDX(IDX)		blk_stk[IDX].fld.top_lbl_idx
# define BLK_TYPE(IDX)			blk_stk[IDX].fld.blk_type
# define BLK_UNNAMED_INTERFACE(IDX)	blk_stk[IDX].fld.inc_temp_idx

# define BLK_AT_IDX(IDX)		blk_stk[IDX].fld.top_lbl_idx
# define BLK_BD_IDX(IDX)		blk_stk[IDX].fld_long.field32_5_1
# define BLK_CN_IDX(IDX)		blk_stk[IDX].fld.induc_temp_idx
# define BLK_CP_IDX(IDX)		blk_stk[IDX].fld.start_temp_idx
# define BLK_NP_IDX(IDX)		blk_stk[IDX].fld.skip_lbl_idx
# define BLK_SB_IDX(IDX)		blk_stk[IDX].fld_long.field32_5_2
# define BLK_SN_IDX(IDX)	        blk_stk[IDX].fld.label_idx
# define BLK_TYP_IDX(IDX)		blk_stk[IDX].fld_long.field32_6_2

/***********************************\
|* CONDITIONAL REPLACEMENT STRINGS *|
\***********************************/

/* Nonzero = TRUE if statement can't be in current block */

# define STMT_CANT_BE_IN_BLK(STMT, BLKTYPE)				       \
				(((stmt_in_blk[STMT] >> BLKTYPE) & 1) != 0)

/* Nonzero = TRUE if statement can be in current block */

# define STMT_LEGAL_IN_BLK(STMT, BLKTYPE)				       \
				(((stmt_in_blk[STMT] >> BLKTYPE) & 1) == 0)

# define STMT_OUT_OF_ORDER(CNTXT,STMT)					       \
				(CNTXT > stmt_top_cat[STMT])

# define SET_DIRECTIVE_STATE(REGION)    directive_state |= (1 << REGION)

# define CLEAR_DIRECTIVE_STATE(REGION)  directive_state &= (~(1 << REGION))


# ifdef _SAVE_IO_STMT
# define INSERT_IO_START                                                       \
	{int	_ir_idx; int	_cn_idx;                                       \
	 _cn_idx = ntr_io_string_constant();                                   \
         NTR_IR_TBL(_ir_idx);                                                  \
         IR_OPR(_ir_idx)        = Start_Io_Opr;                                \
         IR_TYPE_IDX(_ir_idx)   = TYPELESS_DEFAULT_TYPE;                       \
         IR_LINE_NUM(_ir_idx)   = stmt_start_line;                             \
         IR_COL_NUM(_ir_idx)    = stmt_start_col;                              \
         IR_LINE_NUM_L(_ir_idx) = stmt_start_line;                             \
         IR_COL_NUM_L(_ir_idx)  = stmt_start_col;                              \
         IR_FLD_L(_ir_idx)      = CN_Tbl_Idx;                                  \
         IR_IDX_L(_ir_idx)      = _cn_idx;                                     \
         gen_sh(Before, stmt_type, stmt_start_line,		               \
                   stmt_start_col, FALSE, FALSE, TRUE);                        \
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = _ir_idx;                   \
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;                  \
	}
# else
# define INSERT_IO_START
# endif

# ifdef _SAVE_IO_STMT
# define INSERT_IO_END                                                         \
	{int	_ir_idx;                                                       \
         NTR_IR_TBL(_ir_idx);                                                  \
         IR_TYPE_IDX(_ir_idx) = TYPELESS_DEFAULT_TYPE;                         \
         IR_OPR(_ir_idx)      = End_Io_Opr;                                    \
         IR_LINE_NUM(_ir_idx) = stmt_start_line;                               \
         IR_COL_NUM(_ir_idx)  = stmt_start_col;                                \
         gen_sh(After, stmt_type, stmt_start_line,	     	               \
                   stmt_start_col, FALSE, FALSE, TRUE);                        \
         SH_IR_IDX(curr_stmt_sh_idx)     = _ir_idx;                            \
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;                               \
	}
# else
# define INSERT_IO_END
# endif


/***********************************************\
|* STATEMENT/FUNCTION-LIKE REPLACEMENT STRINGS *|
\***********************************************/

# define MATCHED_TOKEN_CLASS(class)					       \
		get_token (class)

# define POP_BLK_STK							       \
	        blk_stk_idx--;

# define PUSH_BLK_STK(NEW_BLK)						       \
		TBL_REALLOC_CK (blk_stk, 1);				       \
		CLEAR_TBL_NTRY(blk_stk, blk_stk_idx);			       \
		CURR_BLK_DEF_LINE	= stmt_start_line;		       \
                CURR_BLK_DEF_COLUMN     = stmt_start_col;                      \
		CURR_BLK		= NEW_BLK;

/* Only attach the current SH to a valid parent blocking stmt SH.  Note that  */
/* blocking stmts at the procedure level have no parent because they are      */
/* already at the outermost level.               			      */

# define LINK_TO_PARENT_BLK                                                    \
                { int	_blk_idx;                                              \
                if (BLK_TYPE(blk_stk_idx - 1) == Doall_Blk ||                  \
                    BLK_TYPE(blk_stk_idx - 1) == Do_Parallel_Blk) {            \
                   _blk_idx = blk_stk_idx - 2;                                 \
                } else {                                                       \
                   _blk_idx = blk_stk_idx - 1;                                 \
                }                                                              \
                if ((Do_Blk <= BLK_TYPE(_blk_idx)  &&                          \
                    BLK_TYPE(_blk_idx) <= Case_Blk) ||                         \
                   (Parallel_Blk <= BLK_TYPE(_blk_idx)  &&                     \
                    BLK_TYPE(_blk_idx) <= Parallel_Case_Blk)) {                \
                   SH_PARENT_BLK_IDX(curr_stmt_sh_idx) =                       \
                         BLK_FIRST_SH_IDX(_blk_idx);                           \
                }}


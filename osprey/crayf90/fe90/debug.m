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


/* USMID:  "\n@(#)5.0_pl/macros/debug.m	5.1	04/29/99 21:22:31\n" */
 

# define INDENT_SIZE	3

# if defined(_HOST32) && defined(_TARGET64)
#    define LONG_TYPE_FMT	"lld"
#    define LONG_TYPE_X_FMT	"llx"
# else
#    define LONG_TYPE_FMT	"d"
#    define LONG_TYPE_X_FMT	"x"
# endif

# ifdef _DEBUG
#    define PRINT_BLK_STK						       \
		if (dump_flags.blk_stk) {				       \
		   print_blk_tbl();					       \
		}
# else
#    define PRINT_BLK_STK
# endif

# ifdef _DEBUG
#    define PRINT_BD_TBL						       \
		if (dump_flags.bd_tbl) {				       \
		   print_bd_tbl();					       \
		}
# else
#    define PRINT_BD_TBL
# endif

# ifdef _DEBUG
#    define PRINT_CMD_LINE_TBLS						       \
		if (dump_flags.cmd_line_tbls) {				       \
		   print_cmd_tbl ();					       \
		}
# else
#    define PRINT_CMD_LINE_TBLS
# endif

# ifdef _DEBUG
#    define PRINT_CN_TBL						       \
		if (dump_flags.cn_tbl) {				       \
		   print_cn_tbl();					       \
		}
# else
#    define PRINT_CN_TBL
# endif

# ifdef _DEBUG
#    define PRINT_DEF_TBL						       \
		if (dump_flags.defines) {				       \
		   print_defines();					       \
		}
# else
#    define PRINT_DEF_TBL
# endif

# ifdef _DEBUG
#    define PRINT_EQV_TBL						       \
	  	if (dump_flags.scp_tbl) {        		               \
		   print_eq_tbl();		                               \
		}
# else
#    define PRINT_EQV_TBL
# endif

# ifdef _DEBUG
#    define PRINT_GL_TBL						       \
		if (dump_flags.gl_tbl) {				       \
		   print_gl_tbl ();					       \
		}
# else
#    define PRINT_GL_TBL
# endif

# ifdef _DEBUG
#    define PRINT_GN_TBL						       \
		if (dump_flags.name_tbls) {				       \
		   print_gn_tbl ();					       \
		}
# else
#    define PRINT_GN_TBL
# endif

# ifdef _DEBUG
#    define PRINT_INTRIN						       \
	  	if (dump_flags.intrin_tbl) {        		               \
		   print_sytb(INTRINSIC_SCP_IDX, FALSE, TRUE);                 \
		}
# else
#    define PRINT_INTRIN
# endif

# ifdef _DEBUG
#    define PRINT_FORTRAN_OUT                                                  \
		if (dump_flags.fort_out) {                                     \
                   print_expanded_stmt();				       \
     		}
# else
#    define PRINT_FORTRAN_OUT
# endif

# ifdef _DEBUG
#    define PRINT_IR_TBL                                                       \
		if (dump_flags.ir1_tbl) {                                      \
		   print_sh_tbl (TRUE);                                        \
     		}
# else
#    define PRINT_IR_TBL
# endif

# ifdef _DEBUG
#    define PRINT_IR_TBL2                                                      \
		if (dump_flags.ir2_tbl) {                                      \
		   print_sh_tbl (TRUE);                                        \
     		}
# else
#    define PRINT_IR_TBL2
# endif

# ifdef _DEBUG
#    define PRINT_IR_TBL3                                                      \
		if (dump_flags.ir3_tbl) {                                      \
		   print_sytb(curr_scp_idx, FALSE, TRUE);                      \
		   print_sh_tbl (TRUE);                                        \
     		}
# else
#    define PRINT_IR_TBL3
# endif

# ifdef _DEBUG
#    define PRINT_IR_TBL4                                                      \
		if (dump_flags.ir4_tbl) {                                      \
		   print_sytb(curr_scp_idx, FALSE, TRUE);                      \
		   print_sh_tbl (TRUE);                                        \
     		}
# else
#    define PRINT_IR_TBL4
# endif

# ifdef _DEBUG
#    define PRINT_NAME_TBLS						       \
                if (dump_flags.name_tbls) {				       \
                   print_ln_tbl ();					       \
                   print_hn_tbl ();					       \
                }
# else
#    define PRINT_NAME_TBLS
# endif

# ifdef _DEBUG
#    define PRINT_FP_TBL						       \
                if (dump_flags.fp_tbl) {				       \
                   print_fp_tbl ();					       \
                }
# else
#    define PRINT_FP_TBL
# endif

# ifdef _DEBUG
#    define PRINT_SB_TBL						       \
		if (dump_flags.sb_tbl) {				       \
		   print_sb_tbl();					       \
		}
# else
#    define PRINT_SB_TBL
# endif

# ifdef _DEBUG
#    define PRINT_SCP_TBL						       \
	  	if (dump_flags.scp_tbl) {        		               \
		   print_scp_tbl();		                               \
		}
# else
#    define PRINT_SCP_TBL
# endif

# ifdef _DEBUG
#    define PRINT_DBG_SYTB						       \
	  	if (SCP_DBG_PRINT_SYTB(curr_scp_idx)) {		               \
		   print_sytb(curr_scp_idx, FALSE, TRUE);                      \
		}
# else
#    define PRINT_DBG_SYTB
# endif

# ifdef _DEBUG
#    define PRINT_DBG_STMT						       \
	  	if (SCP_DBG_PRINT_STMT(curr_scp_idx)) {		               \
		   print_sh_tbl(FALSE);			                       \
		}
# else
#    define PRINT_DBG_STMT
# endif

# ifdef _DEBUG
#    define PRINT_SYTB							       \
	  	if (dump_flags.sytb) {	        		               \
		   print_sytb(1, TRUE, TRUE);	                               \
		}
# else
#    define PRINT_SYTB
# endif

# ifdef _DEBUG
#    define PRINT_TYP_TBL						       \
	  	if (dump_flags.typ_tbl) { 	       		               \
		   print_typ_tbl();		                               \
		}
# else
#    define PRINT_TYP_TBL
# endif

# ifdef _DEBUG
#    define TRACE(TRACE_TYPE, FUNC_NAME, INFO)				       \
		if (dump_flags.ftrace_info) {				       \
		   dump_func_trace_info (TRACE_TYPE, FUNC_NAME, (INFO));       \
		}
# else
#    define TRACE(ARG1, ARG2, ARG3)
# endif

# ifdef _DEBUG
#    define OUTPUT_PASS_HEADER(PASS)					       \
                if (dump_flags.ftrace_info) {      			       \
                   dump_func_trace_info (PASS, NULL, NULL); 		       \
                }
# else
#    define OUTPUT_PASS_HEADER(ARG1)
# endif

# ifdef _DEBUG
#    define TRACE_NEW_STMT(ARG1)					       \
		if (dump_flags.ftrace_info) {				       \
		   dump_func_trace_info (Stmt_Start, NULL, ARG1);	       \
		}
# else
#    define TRACE_NEW_STMT(ARG1)
# endif

# ifdef _DEBUG
# define PRINT_ALL_SYM_TBLS	{					       \
      PRINT_DEF_TBL;                                                           \
      PRINT_SCP_TBL;							       \
      PRINT_FP_TBL;							       \
      PRINT_SYTB;							       \
      PRINT_NAME_TBLS;							       \
      PRINT_BD_TBL;							       \
      PRINT_CN_TBL;							       \
      PRINT_SB_TBL;							       \
      PRINT_TYP_TBL;							       \
      PRINT_IR_TBL2;							       \
				}
# else
# define PRINT_ALL_SYM_TBLS
# endif

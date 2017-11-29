/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/p_dcl_pu.c	5.5	09/01/99 09:11:00\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "p_globals.m"
# include "debug.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"
# ifdef KEY
# include "i_cvrt.h"
# endif


/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static	void	gen_end_prologue_debug_label (int);
static	void	parse_dummy_args (int);
static	void	parse_prefix_spec (void);
static	void	set_function_rslt (int, boolean);
static	void	start_new_scp (void);
static	int	start_new_subpgm (pgm_unit_type, boolean, boolean);


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF     - BLOCK DATA [block-data-name]                                *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_block_stmt (void)
{
		int		defer_msg	= 0;
   static 	char		num_unnamed	= 'A';
   		boolean		parse_error;
		boolean		unnamed_blk	= FALSE;


   TRACE (Func_Entry, "parse_block_stmt", NULL);

   if (matched_specific_token(Tok_Kwd_Data, Tok_Class_Keyword)) {
      parse_error = FALSE;

      if (LA_CH_VALUE == EOS) { /* create BLCKDAT# where # = A, B, C, .. Z.   */
         unnamed_blk		= TRUE;
	 TOKEN_STR(token)[0]	= 'B';
	 TOKEN_STR(token)[1]	= 'L';
	 TOKEN_STR(token)[2]	= 'K';
# if defined(_NO_AT_SIGN_IN_NAMES)
	 TOKEN_STR(token)[3]	= '.';
# else
	 TOKEN_STR(token)[3]	= '@';
# endif
	 TOKEN_STR(token)[4]	= 'D';
	 TOKEN_STR(token)[5]	= 'A';
	 TOKEN_STR(token)[6]	= 'T';
         TOKEN_STR(token)[7]	= num_unnamed;
         TOKEN_LEN(token)	= 8;
         TOKEN_VALUE(token)	= Tok_Id;
	 TOKEN_LINE(token)	= stmt_start_line;
	 TOKEN_COLUMN(token)	= stmt_start_col;

         if (num_unnamed > 'Z') {

            /* > 26 unnamed BLOCK DATA pgm units.  Fix the name, so junk */
            /* does not print in the error message header.               */

            TOKEN_STR(token)[7]	= 'a';
         }
      }
      else if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         parse_err_flush(Find_EOS, "block-data-name");
         parse_error		= TRUE;
         token			= main_token;
         TOKEN_LINE(token)	= stmt_start_line;		  
         TOKEN_COLUMN(token)	= stmt_start_col;		 
      } 
   }
   else {
      parse_err_flush(Find_EOS, "DATA");
      parse_error		= TRUE;
      token			= main_token;
      TOKEN_LINE(token)		= stmt_start_line;		  
      TOKEN_COLUMN(token)	= stmt_start_col;		 
   }

   start_new_prog_unit(Blockdata, 
                       Blockdata_Blk,
                       FALSE,
                       parse_error,
                      &defer_msg);
   CURR_BLK_NO_EXEC	= TRUE;

   if (unnamed_blk) {
      CURR_BLK_NAME	= NULL_IDX;	/* Clear for unnamed blockdata */

      if (num_unnamed > 'Z') {	/* > 26 unnamed BLOCK DATA pgm units */
         PRINTMSG(stmt_start_line, 29, Error, stmt_start_col);
      } 
      else if (num_unnamed > 'A') {	/* > 1 unnamed BLOCK DATA is non-ansi */
         PRINTMSG(stmt_start_line, 30, Ansi, stmt_start_col);
      }
      num_unnamed++;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }  

   NEXT_LA_CH;		/* Consume EOS */

   TRACE (Func_Exit, "parse_block_stmt", NULL);

   return;

}  /* parse_block_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF     - ENTRY entry-name [([dummy-arg-list])[RESULT (result-name)]] *|
|*              - dummy-arg     =>      dummy-arg-name or *		      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
void parse_entry_stmt (void)

{
   int			attr_idx		= NULL_IDX;
   boolean		blk_err			= FALSE;
   int			branch_around_lbl_idx;
   int			host_attr_idx;
   int			host_name_idx;
   int			ir_idx;
   boolean		issue_msg;
   int			length;
   int			list_idx;
   int			name_idx;
   pgm_unit_type	pgm_unit;
   atp_proc_type	proc_type;
   int			save_scp_idx;
   obj_type		sem_type;


   TRACE (Func_Entry, "parse_entry_stmt", NULL);

   if (STMT_CANT_BE_IN_BLK(Entry_Stmt, CURR_BLK) && iss_blk_stk_err()) {

      /* Issued blk error - err issued by rtn */

      blk_err = TRUE;

   }
   else if (curr_stmt_category < Implicit_None_Stmt_Cat) {

      /* The ENTRY statement must follow all USE statements, so set curr_stmt */
      /* _category so that anymore USE statements will be out of context.     */

      curr_stmt_category = Implicit_None_Stmt_Cat;
   }

   if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
      attr_idx = srch_sym_tbl(TOKEN_STR(token),
                              TOKEN_LEN(token),
                              &name_idx);

      pgm_unit	= (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Function) ?
                   Function : Subroutine;

      proc_type	= (atp_proc_type) ATP_PROC(SCP_ATTR_IDX(curr_scp_idx));

      /* This must be an external or a module procedure */

      if (ATP_PROC(SCP_ATTR_IDX(curr_scp_idx)) == Module_Proc) {
         host_attr_idx = srch_host_sym_tbl(TOKEN_STR(token), 
                                           TOKEN_LEN(token),
                                           &host_name_idx,
                                           FALSE);  /* Don't search intrinsic */

         if (host_attr_idx == NULL_IDX) {

            if (attr_idx == NULL_IDX) {
               save_scp_idx	= curr_scp_idx;
               curr_scp_idx	= SCP_PARENT_IDX(curr_scp_idx);
               host_attr_idx	= ntr_sym_tbl(&token, host_name_idx);
               curr_scp_idx	= save_scp_idx;
               attr_idx		= srch_sym_tbl(TOKEN_STR(token),
                                               TOKEN_LEN(token),
                                               &name_idx);

               /* Enter in local scope - but share attrs */

               attr_idx		= ntr_host_in_sym_tbl(&token,
                                                      name_idx,
                                                      host_attr_idx,
                                                      host_name_idx,
                                                      FALSE);

               LN_DEF_LOC(name_idx)		= TRUE;
               LN_DEF_LOC(host_name_idx)	= TRUE;
               AT_OBJ_CLASS(attr_idx)		= Pgm_Unit;
               ATP_PGM_UNIT(attr_idx)		= pgm_unit;
               ATP_PROC(attr_idx)		= proc_type;
            }
            else { /* Have local attr.  Use it for the host as well. */

               sem_type = (pgm_unit == Function) ? Obj_Entry_Func :
                                                   Obj_Entry_Subr;

               if (fnd_semantic_err(sem_type,
                                    TOKEN_LINE(token),
                                    TOKEN_COLUMN(token),
                                    attr_idx,
                                    TRUE)) {
                  CREATE_ERR_ATTR(attr_idx,
                                  TOKEN_LINE(token),
                                  TOKEN_COLUMN(token),
	            		  Pgm_Unit);

                  ATP_PGM_UNIT(attr_idx)	= pgm_unit;
                  ATP_PROC(attr_idx)		= proc_type;
               }
               else {
                  LN_DEF_LOC(name_idx)		= TRUE;
               }

               save_scp_idx			= curr_scp_idx;
               curr_scp_idx			= SCP_PARENT_IDX(curr_scp_idx);
               host_attr_idx			= ntr_sym_tbl(&token,
                                                              host_name_idx);
               curr_scp_idx			= save_scp_idx;
               attr_tbl_idx--;
               attr_aux_tbl_idx--;
               LN_ATTR_IDX(host_name_idx)	= attr_idx;
               LN_NAME_IDX(host_name_idx)	= AT_NAME_IDX(attr_idx);
               LN_DEF_LOC(host_name_idx)	= TRUE;

               if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
                  chg_data_obj_to_pgm_unit(attr_idx,
                                           pgm_unit,
                                           proc_type);
               }
               else if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
                  ATP_PGM_UNIT(attr_idx)	= pgm_unit;
                  ATP_PROC(attr_idx)		= proc_type;
               }
            }
         }
         else {
            issue_msg	= TRUE;

            if (attr_idx != NULL_IDX) { 
               sem_type = (pgm_unit == Function) ? Obj_Entry_Func :
                                                   Obj_Entry_Subr;

               if (fnd_semantic_err(sem_type,
                                    TOKEN_LINE(token),
                                    TOKEN_COLUMN(token),
                                    attr_idx,
                                    TRUE)) {

                  CREATE_ERR_ATTR(attr_idx,
                                  TOKEN_LINE(token),
                                  TOKEN_COLUMN(token),
                                  Pgm_Unit);

                  ATP_PGM_UNIT(attr_idx)	= pgm_unit;
                  ATP_PROC(attr_idx)		= proc_type;
                  issue_msg			= FALSE;
               }
               else {
                  LN_DEF_LOC(name_idx)	= TRUE;
               }
            }

            sem_type = (pgm_unit == Function) ? Obj_Module_Func :
                                                Obj_Module_Subr;

            if (AT_OBJ_CLASS(host_attr_idx) == Interface &&
                ATI_PROC_IDX(host_attr_idx) != NULL_IDX) {
                host_attr_idx = ATI_PROC_IDX(host_attr_idx);
            }
            
            if (fnd_semantic_err(sem_type,
                                 TOKEN_LINE(token),
                                 TOKEN_COLUMN(token),
                                 host_attr_idx,
                                 issue_msg)) {
               CREATE_ERR_ATTR(host_attr_idx,
                               TOKEN_LINE(token),
                               TOKEN_COLUMN(token),
      			       Pgm_Unit);
               ATP_PGM_UNIT(host_attr_idx)	= pgm_unit;
               ATP_PROC(host_attr_idx)		= proc_type;

            }
            else if (AT_OBJ_CLASS(host_attr_idx) == Data_Obj) {
               chg_data_obj_to_pgm_unit(host_attr_idx,
                                        pgm_unit,
                                        proc_type);
            }
            else if (ATP_PROC(host_attr_idx) == Module_Proc &&
                     ATP_EXPL_ITRFC(host_attr_idx)) {

               /* This is already declared as a module procedure. */

               PRINTMSG(TOKEN_LINE(token), 1529, Error,
                        TOKEN_COLUMN(token),
                        AT_OBJ_NAME_PTR(host_attr_idx));
            }
            else {
               ATP_PGM_UNIT(host_attr_idx)	= pgm_unit;
               ATP_PROC(host_attr_idx)		= proc_type;
            }

            if (attr_idx == NULL_IDX) {   /* No local attr - just a host attr */
               attr_idx	= ntr_host_in_sym_tbl(&token,
                                              name_idx,
                                              host_attr_idx,
                                              host_name_idx,
                                              FALSE);
               LN_DEF_LOC(name_idx) = TRUE;
            }
            else {

               /* Have both a local and a host attr.  The host attr can only  */
               /* be a generic interface or have PUBLIC or PRIVATE set.  The  */
               /* local attr can have all sorts of typing and dimension info  */
               /* set if this is a FUNCTION.  If this is a SUBROUTINE, it can */
               /* only have PUBLIC or PRIVATE set.  (And this cannot happen   */
               /* in a module procedure, because these can only be set in     */
               /* modules.  If function, use the local attr as the function   */
               /* result if there were no problems with the local attr.       */
               /* (issue_msg = TRUE)                                          */

               if (issue_msg && pgm_unit == Function) {
                  ATP_RSLT_IDX(host_attr_idx)	= attr_idx;
                  ATD_CLASS(attr_idx)		= Function_Result;
                  ATD_FUNC_IDX(attr_idx)	= host_attr_idx;
               }
            }
            LN_ATTR_IDX(name_idx)	= host_attr_idx;
            LN_NAME_IDX(name_idx)	= AT_NAME_IDX(host_attr_idx);
            LN_DEF_LOC(host_name_idx)	= TRUE;
            attr_idx			= host_attr_idx;
         }

         ATP_EXT_NAME_IDX(attr_idx)	= make_in_parent_string(
                                                  AT_NAME_IDX(attr_idx),
                                                  AT_NAME_LEN(attr_idx),
                                                  SCP_PARENT_IDX(curr_scp_idx),
                                                  &length);
         ATP_EXT_NAME_LEN(attr_idx)	= length;
      }
      else if (attr_idx == NULL_IDX) {
         attr_idx			= ntr_sym_tbl(&token, name_idx);
         LN_DEF_LOC(name_idx)		= TRUE;  /* Not host associable */
         AT_OBJ_CLASS(attr_idx)		= Pgm_Unit;
         ATP_PGM_UNIT(attr_idx)		= pgm_unit;
         ATP_PROC(attr_idx)		= ATP_PROC(SCP_ATTR_IDX(curr_scp_idx));
         MAKE_EXTERNAL_NAME(attr_idx,
         	            AT_NAME_IDX(attr_idx),
        	            AT_NAME_LEN(attr_idx));
      }
      else {
         sem_type = (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Function) ?
                     Obj_Entry_Func : Obj_Entry_Subr;

         if (fnd_semantic_err(sem_type,
                              TOKEN_LINE(token),
                              TOKEN_COLUMN(token),
                              attr_idx,
                              TRUE)) {
            CREATE_ERR_ATTR(attr_idx,
                            TOKEN_LINE(token),
                            TOKEN_COLUMN(token),
			    Pgm_Unit);

            ATP_PGM_UNIT(attr_idx)      = pgm_unit;
            ATP_PROC(attr_idx)	        = ATP_PROC(SCP_ATTR_IDX(curr_scp_idx));
            MAKE_EXTERNAL_NAME(attr_idx,
            	            AT_NAME_IDX(attr_idx),
           	            AT_NAME_LEN(attr_idx));
         }
         else { 
            LN_DEF_LOC(name_idx)= TRUE;

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
               chg_data_obj_to_pgm_unit(attr_idx,
                                        pgm_unit,
                                        (atp_proc_type) 
                                        ATP_PROC(SCP_ATTR_IDX(curr_scp_idx)));
            }
            else if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
               ATP_PGM_UNIT(attr_idx)	= pgm_unit;
               ATP_PROC(attr_idx)	= proc_type;
            }
         }
      }

      ATP_SCP_IDX(attr_idx)	= curr_scp_idx;
      ATP_ALT_ENTRY(attr_idx)	= TRUE;
      ATP_RECURSIVE(attr_idx)	= ATP_RECURSIVE(SCP_ATTR_IDX(curr_scp_idx));
      ATP_ELEMENTAL(attr_idx)	= ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx));
      ATP_PURE(attr_idx)	= ATP_PURE(SCP_ATTR_IDX(curr_scp_idx));
      ATP_SCP_ALIVE(attr_idx)	= TRUE;
      ATP_EXPL_ITRFC(attr_idx)  = TRUE;
      ATP_MAY_INLINE(attr_idx)	= ATP_MAY_INLINE(SCP_ATTR_IDX(curr_scp_idx));

      if ((cif_flags & XREF_RECS) != 0) {
         cif_usage_rec(attr_idx,
                       AT_Tbl_Idx,
                       TOKEN_LINE(token),
                       TOKEN_COLUMN(token),
                       CIF_Symbol_Declaration);
      }

      NTR_ATTR_LIST_TBL(list_idx);
      AL_ATTR_IDX(list_idx)		= attr_idx;
      AL_NEXT_IDX(list_idx)		= SCP_ENTRY_IDX(curr_scp_idx);
      SCP_ENTRY_IDX(curr_scp_idx)	= list_idx;

      if (SCP_ALT_ENTRY_CNT(curr_scp_idx) >= MAX_ALTERNATE_ENTRIES) {
         PRINTMSG(TOKEN_LINE(token), 1115, Limit,
                  TOKEN_COLUMN(token),
                  MAX_ALTERNATE_ENTRIES);
      }

      SCP_ALT_ENTRY_CNT(curr_scp_idx)	= SCP_ALT_ENTRY_CNT(curr_scp_idx) + 1;
      AT_DCL_ERR(attr_idx)	       |= blk_err;

      if (LA_CH_VALUE != EOS && LA_CH_VALUE != LPAREN) {
         parse_err_flush(Find_Lparen, "( or " EOS_STR );
      }

      if (CURR_BLK != Interface_Body_Blk &&
          (cmd_line_flags.runtime_argument ||
           cmd_line_flags.runtime_arg_entry)) {

         ATP_ARGCHCK_ENTRY(attr_idx) = TRUE;
      }

      if (LA_CH_VALUE == LPAREN) {
         parse_dummy_args(attr_idx);
      }

      if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Function) {
         set_function_rslt(attr_idx, FALSE);

         if (LA_CH_VALUE != EOS) {
            parse_err_flush(Find_EOS, EOS_STR);
         }
      }
      else if (LA_CH_VALUE != EOS) {

         if (matched_specific_token (Tok_Kwd_Result, Tok_Class_Keyword)){

            /* result keyword not allowed in subroutine subprogram */

            PRINTMSG(TOKEN_LINE(token), 122, Error, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
         }
         else {
            parse_err_flush(Find_EOS, EOS_STR);
         }
      }

      branch_around_lbl_idx = gen_internal_lbl(TOKEN_LINE(token));

      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx)       = ir_idx;
      SH_COMPILER_GEN(curr_stmt_sh_idx) = TRUE;
      SH_STMT_TYPE(curr_stmt_sh_idx)    = Goto_Stmt;
      IR_OPR(ir_idx)                    = Br_Uncond_Opr;
      IR_TYPE_IDX(ir_idx)               = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)               = TOKEN_LINE(token);
      IR_COL_NUM(ir_idx)                = TOKEN_COLUMN(token);
      IR_FLD_R(ir_idx)                  = AT_Tbl_Idx;
      IR_IDX_R(ir_idx)                  = branch_around_lbl_idx;
      IR_COL_NUM_R(ir_idx)              = TOKEN_COLUMN(token);
      IR_LINE_NUM_R(ir_idx)             = TOKEN_LINE(token);

      gen_sh(After, stmt_type, TOKEN_LINE(token), TOKEN_COLUMN(token),
             FALSE, FALSE, FALSE);
   
      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      IR_OPR(ir_idx)              = Entry_Opr;
      IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)         = TOKEN_LINE(token);
      IR_COL_NUM(ir_idx)          = TOKEN_COLUMN(token);
      IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)            = attr_idx;
      IR_COL_NUM_L(ir_idx)        = TOKEN_COLUMN(token);
      IR_LINE_NUM_L(ir_idx)       = TOKEN_LINE(token);

      if (attr_idx != NULL_IDX) {
         ATP_FIRST_SH_IDX(attr_idx)  = curr_stmt_sh_idx;
      }
   
      gen_sh(After, Continue_Stmt, TOKEN_LINE(token), TOKEN_COLUMN(token),
             FALSE, TRUE, TRUE);
   
      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx)       = ir_idx;
      IR_OPR(ir_idx)                    = Label_Opr;
      IR_TYPE_IDX(ir_idx)               = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)               = TOKEN_LINE(token);
      IR_COL_NUM(ir_idx)                = TOKEN_COLUMN(token);
      IR_FLD_L(ir_idx)                  = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)                  = branch_around_lbl_idx;
      IR_COL_NUM_L(ir_idx)              = TOKEN_COLUMN(token);
      IR_LINE_NUM_L(ir_idx)             = TOKEN_LINE(token);

      if (attr_idx != NULL_IDX) {
         ATP_ENTRY_LABEL_SH_IDX(attr_idx)  = curr_stmt_sh_idx;
      }
   
      if (cmd_line_flags.debug_lvl <= Debug_Lvl_1) {  /* -ez -ed -G0 -G1 */
         gen_end_prologue_debug_label(attr_idx);
      }
   }
   else {
      parse_err_flush(Find_EOS, "entry-name");
   }

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_entry_stmt", NULL);

   return;

}  /* parse_entry_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF     - [ prefix ] FUNCTION function-name                           *|
|*                                   ( [ dummy-arg-name-list ] )              *|
|*                                   [ RESULT ( result-name ) ]               *|
|*                                                                            *|
|*      prefix  -    type-spec [ RECURSIVE ]                                  *|
|*                or RECURSIVE [ type-spec ]				      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
void parse_function_stmt (void)

{
   int			attr_idx;
   int			defer_msg;
   boolean		err_fnd		= FALSE;
   token_type		save_token;


   TRACE (Func_Entry, "parse_function_stmt", NULL);

   if (curr_stmt_category > Sub_Func_Stmt_Cat) {
      err_fnd			= TRUE;
      iss_blk_stk_err();
   }

   if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
      parse_err_flush(Find_Lparen, "function-name");
      token			= main_token;
      TOKEN_LINE(token)		= stmt_start_line;		  
      TOKEN_COLUMN(token)	= stmt_start_col;		 
      err_fnd			= TRUE;
   }
   else if (LA_CH_VALUE != LPAREN) {
      save_token		= token;
      parse_err_flush(Find_Lparen, "(");
      err_fnd			= TRUE;
      token			= save_token;
   }

   if (curr_stmt_category == Init_Stmt_Cat) {   /* Start new compilation unit */
      defer_msg			= 0;
      attr_idx			= start_new_prog_unit(Function,
                                                      Function_Blk,
                                                      FALSE,
                                                      err_fnd,
                                                      &defer_msg);
      ATP_PROC(attr_idx)	= Extern_Proc;
   }
   else {

      /* Create a scope for this contained routine, but leave curr_scp_idx   */
      /* still pointing to parent's scope.  After the call to start_new_pgm, */
      /* curr_scp_idx will be set correctly.  TRUE means save table idxs if  */
      /* if this is an interface body, for use in collapsing later.          */

      start_new_scp();
      curr_scp_idx		= SCP_PARENT_IDX(curr_scp_idx);
      attr_idx			= start_new_subpgm(Function, err_fnd, TRUE);
   }

   /* Flag problems with the FUNCTION name */

   SCP_IN_ERR(curr_scp_idx)	= SCP_IN_ERR(curr_scp_idx) ||
                                  AT_DCL_ERR(attr_idx);

   if (CURR_BLK != Interface_Body_Blk &&
       (cmd_line_flags.runtime_argument ||
        cmd_line_flags.runtime_arg_entry)) {

      ATP_ARGCHCK_ENTRY(attr_idx) = TRUE;
   }

   if (LA_CH_VALUE == LPAREN) {
      parse_dummy_args(attr_idx);
   }

   set_function_rslt(attr_idx, FALSE);

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_function_stmt", NULL);

   return;

}  /* parse_function_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF     -  MODULE module-name                                         *|
|*              or MODULE PROCEDURE procedure-name-list                       *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
void parse_module_stmt (void)

{
   int		attr_idx;
   int		defer_msg;
   boolean	found_comma;
   int		host_name_idx;
   int		interface_idx		= NULL_IDX;

# if defined(_SPLIT_STATIC_STORAGE_M)
   id_str_type	name;
   int		new_idx;
# endif

   int		name_idx;
   int		new_attr_idx;
   boolean	parse_error;
   int		sn_idx;
   int		stmt_number;
   int		tmp_attr_idx;


   TRACE (Func_Entry, "parse_module_stmt", NULL);

   stmt_number = statement_number;

   if (curr_stmt_category != Init_Stmt_Cat &&
       matched_specific_token (Tok_Kwd_Procedure, Tok_Class_Keyword)) {

      /* If it is Init_Stmt_Cat it must be the start of a MODULE.        */
      /* Send case of a module named PROCEDURE in the correct direction. */

      stmt_type				= Module_Proc_Stmt;
      SH_STMT_TYPE(curr_stmt_sh_idx)	= Module_Proc_Stmt;
     
      if (CURR_BLK == Interface_Blk) {

         if (CURR_BLK_NAME == NULL_IDX) {
            PRINTMSG(stmt_start_line, 4, Error, stmt_start_col);
         }
         else {
            curr_stmt_category	= Sub_Func_Stmt_Cat;

            if (cif_flags & MISC_RECS) {
               cif_stmt_type_rec(TRUE, CIF_Module_Procedure_Stmt, stmt_number);
            }
         }

         interface_idx = CURR_BLK_NAME;
      }
      else if (!iss_blk_stk_err()) {
         curr_stmt_category	= Sub_Func_Stmt_Cat;
      }

      do {
         if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

            /* loc name table being entered and searched is the local name    */
            /* table for the parent.                                          */

            attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                    &name_idx);

            if (attr_idx == NULL_IDX) {
               attr_idx = srch_host_sym_tbl(TOKEN_STR(token), 
                                            TOKEN_LEN(token),
                                            &host_name_idx,
                                            FALSE);

               if (attr_idx == NULL_IDX) {
                  attr_idx			= ntr_sym_tbl(&token, name_idx);
                  LN_DEF_LOC(name_idx)		= TRUE;
                  AT_OBJ_CLASS(attr_idx)	= Pgm_Unit;  
                  ATP_PROC(attr_idx)		= Module_Proc;
                  ATP_SCP_IDX(attr_idx)		= curr_scp_idx;
                  MAKE_EXTERNAL_NAME(attr_idx,
            	                  AT_NAME_IDX(attr_idx),
           	                  AT_NAME_LEN(attr_idx));
               }
               else {  /* Found in host scope - but not local scope */

                  if (AT_OBJ_CLASS(attr_idx) == Interface &&
                      ATI_PROC_IDX(attr_idx) != NULL_IDX) {
                     attr_idx = ATI_PROC_IDX(attr_idx);
                  }

                  if (AT_NOT_VISIBLE(attr_idx)) {
                     PRINTMSG(TOKEN_LINE(token), 486, Error,
                              TOKEN_COLUMN(token),
                              AT_OBJ_NAME_PTR(attr_idx),
                              AT_OBJ_NAME_PTR(AT_MODULE_IDX((attr_idx))));
                     CREATE_ERR_ATTR(attr_idx,
                                     TOKEN_LINE(token),
                                     TOKEN_COLUMN(token),
                                     Pgm_Unit);
                     ATP_PROC(attr_idx)		= Module_Proc;
                     ATP_SCP_IDX(attr_idx)	= curr_scp_idx;
                     MAKE_EXTERNAL_NAME(attr_idx,
            	                     AT_NAME_IDX(attr_idx),
           	                     AT_NAME_LEN(attr_idx));
                  }
                  else if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
                           ATP_PROC(attr_idx) == Module_Proc) {

                     /* Enter in local sytb, but share attrs. */

                     attr_idx = ntr_host_in_sym_tbl(&token,
                                                    name_idx,
                                                    attr_idx,
                                                    host_name_idx,
                                                    FALSE);
                     LN_DEF_LOC(name_idx) = TRUE;
                  }
                  else if (AT_OBJ_CLASS(attr_idx) == Interface) {
                     NTR_ATTR_TBL(tmp_attr_idx);
                     COPY_COMMON_ATTR_INFO(attr_idx, tmp_attr_idx, Pgm_Unit);
                     ATI_PROC_IDX(attr_idx)	= tmp_attr_idx;
                     attr_idx			= tmp_attr_idx;
                     AT_USE_ASSOCIATED(attr_idx)= FALSE;
                     AT_IS_INTRIN(attr_idx)	= FALSE;
                     AT_ELEMENTAL_INTRIN(attr_idx)	= FALSE;
                     MAKE_EXTERNAL_NAME(attr_idx,
            	                     AT_NAME_IDX(attr_idx),
           	                     AT_NAME_LEN(attr_idx));
                     ATP_PROC(attr_idx)		= Module_Proc;
                     AT_DEF_LINE(attr_idx)	= TOKEN_LINE(token);
                     AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(token);
                  }
                  else if (fnd_semantic_err(Obj_Module_Proc,
                                            TOKEN_LINE(token),
                                            TOKEN_COLUMN(token),
                                            attr_idx,
                                            FALSE)) {

                     /* Just look for an error - don't issue it.  This one  */
                     /* needs to issue a special error, 707.                */

                     PRINTMSG(TOKEN_LINE(token), 707, Error,
                              TOKEN_COLUMN(token), 
                              AT_OBJ_NAME_PTR(attr_idx));

                     CREATE_ERR_ATTR(attr_idx,
                                     TOKEN_LINE(token),
                                     TOKEN_COLUMN(token),
                                     Pgm_Unit);

                     ATP_PROC(attr_idx)		= Module_Proc;
                     ATP_SCP_IDX(attr_idx)	= curr_scp_idx;
                     MAKE_EXTERNAL_NAME(attr_idx,
            	                        AT_NAME_IDX(attr_idx),
           	                        AT_NAME_LEN(attr_idx));
                  }
                  else {  /* Must just have PUBLIC/PRIVATE specified */
                          /* Enter in local sytb, but share attrs.   */

                     attr_idx = ntr_host_in_sym_tbl(&token, name_idx,
                                                    attr_idx, host_name_idx,
                                                    FALSE);

                     if (AT_OBJ_CLASS(attr_idx) != Pgm_Unit) {
                        CLEAR_VARIANT_ATTR_INFO(attr_idx, Pgm_Unit);
                        MAKE_EXTERNAL_NAME(attr_idx,
            	                           AT_NAME_IDX(attr_idx),
           	                           AT_NAME_LEN(attr_idx));
                     }

                     ATP_PROC(attr_idx)		= Module_Proc;
                     LN_DEF_LOC(name_idx)	= TRUE;
                  }
               }
            }
            else { /* Found this attr in the local scope */

               if (AT_OBJ_CLASS(attr_idx) == Interface &&
                   ATI_PROC_IDX(attr_idx) == NULL_IDX ||
                   AT_IS_INTRIN(attr_idx)) {

                  /* If AT_IS_INTRIN is TRUE, we are overloading an   */
                  /* intrinsic.  Treat this as a new attr.  The       */
                  /* intrinsic will stay in the interface list, but   */
                  /* below this one, where it won't be found.         */

                  /* Make local version and connect to the interface. */

                  NTR_ATTR_TBL(tmp_attr_idx);
#ifdef KEY /* Bug 4197 */
		  /* We want the newly added specific procedure to be
		   * non-intrinsic just like the interface itself. */
                  AT_IS_INTRIN(attr_idx)	= FALSE;
                  AT_ELEMENTAL_INTRIN(attr_idx)	= FALSE;
                  COPY_COMMON_ATTR_INFO(attr_idx, tmp_attr_idx, Pgm_Unit);
#else /* KEY Bug 4197 */
                  COPY_COMMON_ATTR_INFO(attr_idx, tmp_attr_idx, Pgm_Unit);
                  AT_IS_INTRIN(attr_idx)	= FALSE;
                  AT_ELEMENTAL_INTRIN(attr_idx)	= FALSE;
#endif  /* KEY Bug 4197 */
                  ATI_PROC_IDX(attr_idx)	= tmp_attr_idx;
                  attr_idx			= tmp_attr_idx;
                  AT_USE_ASSOCIATED(attr_idx)	= FALSE;
                  MAKE_EXTERNAL_NAME(attr_idx,
                                     AT_NAME_IDX(attr_idx),
                                     AT_NAME_LEN(attr_idx));
                  ATP_PROC(attr_idx)		= Module_Proc;
                  AT_DEF_LINE(attr_idx)		= TOKEN_LINE(token);
                  AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(token);
               }
               else {

                  if (AT_OBJ_CLASS(attr_idx) == Interface) {
                     attr_idx = ATI_PROC_IDX(attr_idx);
                  }

                  if (AT_NOT_VISIBLE(attr_idx) ||
                      AT_OBJ_CLASS(attr_idx) != Pgm_Unit ||
                      ATP_PROC(attr_idx) != Module_Proc) {

                     if (fnd_semantic_err(Obj_Module_Proc,
                                          TOKEN_LINE(token),
                                          TOKEN_COLUMN(token),
                                          attr_idx,
                                          TRUE)) {
                        CREATE_ERR_ATTR(attr_idx,
                                        TOKEN_LINE(token),
                                        TOKEN_COLUMN(token),
                                        Pgm_Unit);
                        ATP_SCP_IDX(attr_idx) = curr_scp_idx;
                     }
                     else if (AT_OBJ_CLASS(attr_idx) != Pgm_Unit) {
                        CLEAR_VARIANT_ATTR_INFO(attr_idx, Pgm_Unit);
                     }

                     MAKE_EXTERNAL_NAME(attr_idx,
                                        AT_NAME_IDX(attr_idx),
                                        AT_NAME_LEN(attr_idx));
                  }

                  ATP_PROC(attr_idx)		= Module_Proc;
               }
            }

            if (ATP_SCP_ALIVE(attr_idx) && !ATP_RECURSIVE(attr_idx) &&
                !on_off_flags.recursive) {
               PRINTMSG(TOKEN_LINE(token), 708, Warning, 
                        TOKEN_COLUMN(token), 
                        AT_OBJ_NAME_PTR(attr_idx));
            }


            /* If the context is okay, interface_idx will be non NULL.  If */
            /* so enter into interface list and set ATI_INTERFACE_CLASS.   */

            if (interface_idx != NULL_IDX) {

               /* Generic or Defined interface - Check if already on list. */

               sn_idx		= ATI_FIRST_SPECIFIC_IDX(interface_idx);
               new_attr_idx	= srch_linked_sn(TOKEN_STR(token),
                                                 TOKEN_LEN(token),
                                                 &sn_idx);

               if (new_attr_idx == NULL_IDX) {
                  NTR_INTERFACE_IN_SN_TBL(sn_idx,
                                          attr_idx,
                                          interface_idx,
                                          TOKEN_LINE(token),
                                          TOKEN_COLUMN(token));

                  if (ATI_INTERFACE_CLASS(interface_idx) ==
                                               Generic_Unknown_Interface &&
                      ATP_PGM_UNIT(attr_idx) != Pgm_Unknown) {

                     ATI_INTERFACE_CLASS(interface_idx) =
                                      (ATP_PGM_UNIT(attr_idx) == Function) ?
                                           Generic_Function_Interface:
                                           Generic_Subroutine_Interface;
                  }
               }
               else if (ATP_SCP_IDX(attr_idx) == curr_scp_idx) {

                  if (AT_USE_ASSOCIATED(new_attr_idx) && 
                      AT_PRIVATE(new_attr_idx)) {

                     /* Found, but the name is private in this scope */
                     /* because it is use associated and only the    */
                     /* generic name is public.  Add - OK            */

                  }
                  else if (AT_IS_INTRIN(new_attr_idx)) {

                     /* The user is overloading intrinsics - allow */
                  }
                  else if (!AT_DCL_ERR(attr_idx)) {
                     PRINTMSG(TOKEN_LINE(token), 671, Error,
                              TOKEN_COLUMN(token),
                              AT_OBJ_NAME_PTR(attr_idx),
                              AT_OBJ_NAME_PTR(interface_idx));
                     AT_DCL_ERR(attr_idx)  = TRUE;

                     /* Add, but it is marked in error. */
                  }


                  NTR_INTERFACE_IN_SN_TBL(sn_idx,
                                          attr_idx,
                                          interface_idx,
                                          TOKEN_LINE(token),
                                          TOKEN_COLUMN(token));
               }
               else {  /* Found, but it is from a different scope. */
                  NTR_INTERFACE_IN_SN_TBL(sn_idx,
                                          attr_idx,
                                          interface_idx,
                                          TOKEN_LINE(token),
                                          TOKEN_COLUMN(token));
               }
            }

            if (LA_CH_VALUE != COMMA && LA_CH_VALUE != EOS) {
               parse_err_flush(Find_Comma, ", or " EOS_STR);
            }
	 }
	 else {
	    parse_err_flush(Find_Comma, "procedure-name");
	 }

         found_comma = (LA_CH_VALUE == COMMA);
         NEXT_LA_CH;
      }
      while (found_comma);
   }
   else {

      if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         parse_err_flush(Find_EOS, "module-name");
         token			= main_token;
         TOKEN_LINE(token)	= stmt_start_line;		  
         TOKEN_COLUMN(token)	= stmt_start_col;		 
         parse_error		= TRUE;
      }
      else {
         parse_error		= FALSE;
      }

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, CIF_Module_Stmt, stmt_number);
      }

      SB_MODULE(SCP_SB_STATIC_IDX(curr_scp_idx))	= TRUE;
      SB_BLK_TYPE(SCP_SB_STATIC_IDX(curr_scp_idx))	= Static;
      SB_RUNTIME_INIT(SCP_SB_STATIC_IDX(curr_scp_idx))	= FALSE;

# if defined(_SPLIT_STATIC_STORAGE_M)

      /* Create an entry for a separate data block for data initialized vars. */
      /* This is only done for static storage in modules.                     */

      CREATE_ID(name, sb_name[Data_Init_Blk], sb_len[Data_Init_Blk]);
      new_idx				= ntr_stor_blk_tbl(name.string,
                                                          sb_len[Data_Init_Blk],
                                                          stmt_start_line,
                                                          stmt_start_col,
                                                          Static);
      SCP_SB_STATIC_INIT_IDX(curr_scp_idx)	= new_idx;
      SB_PAD_BLK(new_idx)			= cmd_line_flags.pad;
      SB_MODULE(new_idx)			= TRUE;

      if (cmd_line_flags.pad_amount != 0) {
         SB_PAD_AMOUNT(new_idx)		= cmd_line_flags.pad_amount;
         SB_PAD_AMOUNT_SET(new_idx)	= TRUE;
      }

# elif defined(_SPLIT_STATIC_STORAGE_2)

      /* Leave data initialized separate from uninitialized. */

      SB_MODULE(SCP_SB_STATIC_INIT_IDX(curr_scp_idx)) = TRUE;

# elif defined(_SPLIT_STATIC_STORAGE_3)

      /* The only way something could have been assigned to             */
      /* SCP_SB_STATIC_INIT_IDX is in an error situation, so we're safe */
      /* to make SCP_SB_STATIC_INIT_IDX point to SCP_SB_STATIC_IDX.  We */
      /* want to do this because it's only local static data that needs */
      /* to go in separate storage blocks, not module data.             */

      SCP_SB_STATIC_INIT_IDX(curr_scp_idx)   = SCP_SB_STATIC_IDX(curr_scp_idx);
      SCP_SB_STATIC_UNINIT_IDX(curr_scp_idx) = SCP_SB_STATIC_IDX(curr_scp_idx);
# else

      /* The only way something could have been assigned to             */
      /* SCP_SB_STATIC_INIT_IDX is in an error situation, so we're safe */
      /* to make SCP_SB_STATIC_INIT_IDX point to SCP_SB_STATIC_IDX.  We */
      /* want to do this because it's only local static data that needs */
      /* to go in separate storage blocks, not module data.             */

      SCP_SB_STATIC_INIT_IDX(curr_scp_idx) = SCP_SB_STATIC_IDX(curr_scp_idx);
# endif

      defer_msg	= 0;
      attr_idx	= start_new_prog_unit(Module, 
                                      Module_Blk,
                                      FALSE,
                                      parse_error,
                                      &defer_msg);

      name_idx				= check_global_pgm_unit(attr_idx);
      ATP_MODULE_STR_IDX(attr_idx)	= GN_NAME_IDX(name_idx);

      CURR_BLK_NO_EXEC			= TRUE;

# if defined(_MODULE_TO_DOT_o)

      if (!cmd_line_flags.binary_output) {
         PRINTMSG(TOKEN_LINE(token), 301, Warning, TOKEN_COLUMN(token),
                  AT_OBJ_NAME_PTR(attr_idx));
      }
# endif

      if (LA_CH_VALUE != EOS) {
         parse_err_flush(Find_EOS, EOS_STR);
      }  

      NEXT_LA_CH;
   }

   TRACE (Func_Exit, "parse_module_stmt", NULL);

   return;

}  /* parse_module_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This function handles the following syntax:                           *|
|*	   program-stmt 	=> PROGRAM program-name [(string of chars)]   *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
void parse_program_stmt (void)

{
   int		defer_msg	= 0;
   boolean	err_fnd;


   TRACE (Func_Entry, "parse_program_stmt", NULL);

   if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
      parse_err_flush(Find_EOS, "program-name");
      token			= main_token;
      TOKEN_LINE(token)		= stmt_start_line;		  
      TOKEN_COLUMN(token)	= stmt_start_col;		 
      err_fnd			= TRUE;
   }
   else {
      err_fnd			= FALSE;
   }

   start_new_prog_unit(Program,
                       Program_Blk,
                       FALSE,
                       err_fnd,
                      &defer_msg);

   if (LA_CH_VALUE == LPAREN) {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Program_Str)) {

         /* Arguments to a PROGRAM statement are nonstandard. */
         /* The get routine does NOT fill in the TOKEN WORD.  */
         /* Only TOKEN_LINE and TOKEN_COLUMN get set.         */

         PRINTMSG(TOKEN_LINE(token), 31, Ansi, TOKEN_COLUMN(token));
      }
      else {	/* get_token has issued a parse error - Just flush */
         parse_err_flush(Find_EOS, NULL);
      }
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_program_stmt", NULL);

   return;

}  /* parse_program_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF     ELEMENTAL [ type-spec ]					      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_elemental_stmt (void)
{
   TRACE (Func_Entry, "parse_elemental_stmt", NULL);

   CLEAR_ATTR_NTRY(AT_WORK_IDX);
   AT_OBJ_CLASS(AT_WORK_IDX)	= Pgm_Unit;
   ATP_ELEMENTAL(AT_WORK_IDX)	= TRUE;
   parse_prefix_spec();

   TRACE (Func_Exit, "parse_elemental_stmt", NULL);

   return;

}  /* parse_elemental_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF     PURE [ type-spec ]					      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_pure_stmt (void)

{
   TRACE (Func_Entry, "parse_pure_stmt", NULL);

   CLEAR_ATTR_NTRY(AT_WORK_IDX);
   AT_OBJ_CLASS(AT_WORK_IDX)	= Pgm_Unit;
   ATP_PURE(AT_WORK_IDX)	= TRUE;
   parse_prefix_spec();

   TRACE (Func_Exit, "parse_pure_stmt", NULL);

   return;

}  /* parse_pure_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF     RECURSIVE [ type-spec ]					      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_recursive_stmt (void)
{
   TRACE (Func_Entry, "parse_recursive_stmt", NULL);

   CLEAR_ATTR_NTRY(AT_WORK_IDX);
   AT_OBJ_CLASS(AT_WORK_IDX)	= Pgm_Unit;
   ATP_RECURSIVE(AT_WORK_IDX)	= TRUE;
   parse_prefix_spec();

   TRACE (Func_Exit, "parse_recursive_stmt", NULL);

   return;

}  /* parse_recursive_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF     PURE [ type-spec ]					      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static	void	parse_prefix_spec (void)

{
   int			attr_idx;
   blk_cntxt_type	blk_type;
   int			defer_msg;
   boolean		elemental_set;
   boolean		matched;
   pgm_unit_type	pgm_type;
   boolean		pure_set;
   boolean		recursive_set;


   TRACE (Func_Entry, "parse_prefix_spec", NULL);

   while (matched = MATCHED_TOKEN_CLASS(Tok_Class_Keyword)) {

      switch (TOKEN_VALUE(token)) {
      case Tok_Kwd_Recursive:

         if (ATP_ELEMENTAL(AT_WORK_IDX)) {

            /* RECURSIVE and ELEMENTAL should not be set for same subprogram */

            PRINTMSG(TOKEN_LINE(token), 1261, Error, TOKEN_COLUMN(token));
            AT_DCL_ERR(AT_WORK_IDX)	= TRUE;
         }
         else if (ATP_RECURSIVE(AT_WORK_IDX)) {  /* Duplicate declaration */
            PRINTMSG(TOKEN_LINE(token), 1260, Error, TOKEN_COLUMN(token),
                     "RECURSIVE");
            AT_DCL_ERR(AT_WORK_IDX)	= TRUE;
         }
         else {
            ATP_RECURSIVE(AT_WORK_IDX)	= TRUE;
         }
         continue;

      case Tok_Kwd_Elemental:

         if (ATP_RECURSIVE(AT_WORK_IDX)) {

            /* RECURSIVE and ELEMENTAL should not be set for same subprogram */

            PRINTMSG(TOKEN_LINE(token), 1261, Error, TOKEN_COLUMN(token));
            AT_DCL_ERR(AT_WORK_IDX)	= TRUE;
         }
         else if (ATP_ELEMENTAL(AT_WORK_IDX)) {  /* Duplicate declaration */
            PRINTMSG(TOKEN_LINE(token), 1260, Error, TOKEN_COLUMN(token),
                     "ELEMENTAL");
            AT_DCL_ERR(AT_WORK_IDX)	= TRUE;
         }
         else {
            ATP_ELEMENTAL(AT_WORK_IDX)	= TRUE;
         }
         continue;

      case Tok_Kwd_Pure:

         if (ATP_PURE(AT_WORK_IDX)) {  /* Duplicate declaration */
            PRINTMSG(TOKEN_LINE(token), 1260, Error, TOKEN_COLUMN(token),
                     "PURE");
            AT_DCL_ERR(AT_WORK_IDX)	= TRUE;
         }
         ATP_PURE(AT_WORK_IDX)	= TRUE;
         continue;

      case Tok_Kwd_Logical:
      case Tok_Kwd_Integer:
      case Tok_Kwd_Double:
      case Tok_Kwd_Real:
      case Tok_Kwd_Complex:
      case Tok_Kwd_Character:
      case Tok_Kwd_Type:
            parse_typed_function_stmt();  /* Pure, ele ect are in AT_WORK_IDX */
            goto EXIT;

         default:
            break;
      }
      break;
   }

   recursive_set	= ATP_RECURSIVE(AT_WORK_IDX);
   elemental_set	= ATP_ELEMENTAL(AT_WORK_IDX);
   pure_set		= ATP_PURE(AT_WORK_IDX);

   if (TOKEN_VALUE(token) == Tok_Kwd_Subroutine) {
      stmt_type					= Subroutine_Stmt;
      SH_STMT_TYPE(curr_stmt_sh_idx)		= Subroutine_Stmt;
      parse_subroutine_stmt();
      ATP_RECURSIVE(SCP_ATTR_IDX(curr_scp_idx))	= recursive_set;
      ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))	= elemental_set;
      ATP_PURE(SCP_ATTR_IDX(curr_scp_idx))	= pure_set;
   }
   else if (TOKEN_VALUE(token) == Tok_Kwd_Function) {
      stmt_type					= Function_Stmt;
      SH_STMT_TYPE(curr_stmt_sh_idx)		= Function_Stmt;
      parse_function_stmt();
      ATP_RECURSIVE(SCP_ATTR_IDX(curr_scp_idx))	= recursive_set;
      ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))	= elemental_set;
      ATP_PURE(SCP_ATTR_IDX(curr_scp_idx))	= pure_set;
   }
   else if (curr_stmt_category > Sub_Func_Stmt_Cat) {
      iss_blk_stk_err();
      parse_err_flush(Find_EOS, NULL);
      NEXT_LA_CH;  /* Skip EOS */
   }
   else {  /* Assume this is a Function or subroutine statement */

      /* Reset to start of word, if matched something, so msg gets correct */
      /* column and line number.                                           */

      if (matched) {
         reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
      }

      parse_err_flush(Find_Lparen, "FUNCTION, SUBROUTINE, INTEGER, "
           "LOGICAL, DOUBLE PRECISION, REAL, COMPLEX, CHARACTER or TYPE");

      token			= main_token;
      TOKEN_LINE(token)		= stmt_start_line;		  
      TOKEN_COLUMN(token)	= stmt_start_col;		 
      pgm_type			= Subroutine;
      blk_type			= Subroutine_Blk;

      if (curr_stmt_category == Init_Stmt_Cat) {
         defer_msg		= 0;
         attr_idx		= start_new_prog_unit(pgm_type, 
                                                      blk_type, 
                                                      FALSE,
                                                      TRUE,
                                                      &defer_msg);
         ATP_PROC(attr_idx)	= Extern_Proc;
      }
      else { /* Create a scope for this contained routine, but leave          */
             /* curr_scp_idx still pointing to parent's scope.  After the     */
             /* call start_new_subpgm, curr_scp_idx will be set correctly.    */

         start_new_scp();
         curr_scp_idx		= SCP_PARENT_IDX(curr_scp_idx);
         attr_idx		= start_new_subpgm(pgm_type, TRUE, FALSE);
      }

      CURR_BLK_ERR		= TRUE;
      SCP_IN_ERR(curr_scp_idx)	= TRUE;
      ATP_RECURSIVE(attr_idx)	= recursive_set;
      ATP_ELEMENTAL(attr_idx)	= elemental_set;
      ATP_PURE(attr_idx)	= pure_set;

      if (CURR_BLK != Interface_Body_Blk &&
          (cmd_line_flags.runtime_argument ||
           cmd_line_flags.runtime_arg_entry)) {

         ATP_ARGCHCK_ENTRY(attr_idx) = TRUE;
      }

      if (LA_CH_VALUE == LPAREN) {
         parse_dummy_args(attr_idx);
      }

      if (LA_CH_VALUE == 'R') {

         /* Subroutine until now - switch to Function - Interface_Body_Blk,   */
         /* Module_Proc_Blk, Internal_Blk - no change                         */

         ATP_PGM_UNIT(attr_idx)	= Function;

         if (CURR_BLK == Subroutine_Blk) {
            CURR_BLK	= Function_Blk;
         }
         set_function_rslt(attr_idx, FALSE);
      }

      if (LA_CH_VALUE != EOS) {
         parse_err_flush(Find_EOS, EOS_STR);
      }

      NEXT_LA_CH;  /* Skip EOS */
   }

EXIT:

   TRACE (Func_Exit, "parse_prefix_spec", NULL);

   return;

}  /* parse_prefix_spec */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	subroutine-stmt		=> [ RECURSIVE ] SUBROUTINE subroutine-name   *|
|*				                   [ ( [ dummy-arg-list ] ) ] *|
|*	dummy-arg 		=> dummy-arg-name			      *|
|*				   or *					      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
void parse_subroutine_stmt (void)

{
   int		attr_idx;
   int		defer_msg;
   boolean	err_fnd		= FALSE;


   TRACE (Func_Entry, "parse_subroutine_stmt", NULL);

   if (curr_stmt_category > Sub_Func_Stmt_Cat) {
      iss_blk_stk_err();
      err_fnd			= TRUE;
   }

   if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
      parse_err_flush(Find_Lparen, "subroutine-name");
      token			= main_token;
      TOKEN_LINE(token)		= stmt_start_line;		  
      TOKEN_COLUMN(token)	= stmt_start_col;		 
      err_fnd			= TRUE;
   }

   if (curr_stmt_category == Init_Stmt_Cat) {  /* Start new compilation unit */
      defer_msg			= 0;
      attr_idx			= start_new_prog_unit(Subroutine,
                                                      Subroutine_Blk,
                                                      FALSE,
                                                      err_fnd,
                                                      &defer_msg);
      ATP_PROC(attr_idx)	= Extern_Proc;
   }
   else {

      /* Save the starting indexes of all the tables, to be used when        */
      /* collapsing the interface stuff back into the parent scope.          */

      /* Create a scope for this contained routine, but leave curr_scp_idx   */
      /* still pointing to parent's scope.  After the call to start_new_pgm, */
      /* curr_scp_idx will be set correctly.  TRUE means save table idxs if  */
      /* if this is an interface body, for use in collapsing later.          */

      start_new_scp();
      curr_scp_idx	= SCP_PARENT_IDX(curr_scp_idx);
      attr_idx		= start_new_subpgm(Subroutine, err_fnd, TRUE);
   }

   SCP_IN_ERR(curr_scp_idx)	= AT_DCL_ERR(attr_idx);

   if (CURR_BLK != Interface_Body_Blk &&
       (cmd_line_flags.runtime_argument ||
        cmd_line_flags.runtime_arg_entry)) {

      ATP_ARGCHCK_ENTRY(attr_idx) = TRUE;
   }

   if (LA_CH_VALUE == LPAREN) {
      parse_dummy_args(attr_idx);
   }

#ifdef KEY /* Bug 14150 */
   if (matched_specific_token(Tok_Kwd_Bind, Tok_Class_Keyword)) {
      if (AT_IS_DARG(attr_idx)) {
	parse_language_binding_spec(0);
	AT_BIND_ATTR(attr_idx) = TRUE;
      }
      else {
	parse_language_binding_spec(&new_binding_label);
	set_binding_label(AT_Tbl_Idx, attr_idx, &new_binding_label);
      }
   }
   else
#endif /* KEY Bug 14150 */
   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

   NEXT_LA_CH;
    
   TRACE (Func_Exit, "parse_subroutine_stmt", NULL);
   
   return;

}  /* parse_subroutine_stmt */
#ifdef KEY /* Bug 14150 */
/*
 * Parse RESULT clause for FUNCTION suffix.
 * attr_idx		AT_Tbl_Idx for function
 * out_rslt_idx		AT_Tbl_Idx for function result
 * return		TRUE if error found
 */
static int
help_set_function_rslt(int attr_idx, int *out_result_idx) {
   token_type save_token;
   int rslt_idx;
   int name_idx;
   int err_found = FALSE;

   if (LA_CH_VALUE != LPAREN) {
      parse_err_flush(Find_EOS, "(");
      err_found = TRUE;
   }
   else {
      NEXT_LA_CH;

      if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
	 parse_err_flush(Find_EOS, "result-name");
	 err_found = TRUE;
      }
      else {

	 if (LA_CH_VALUE == RPAREN) {
	    NEXT_LA_CH;
	 }
	 else {
	    save_token	= token;
	    parse_err_flush(Find_EOS, ")");
	    err_found	= TRUE;
	    token		= save_token;
	 }

	 rslt_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
				 &name_idx);

	 if (rslt_idx == NULL_IDX) {
	    rslt_idx			= ntr_sym_tbl(&token, name_idx);
	    LN_DEF_LOC(name_idx)	= TRUE;
	    AT_OBJ_CLASS(rslt_idx)	= Data_Obj;
	 }
	 else if (!ATP_ALT_ENTRY(attr_idx)) {

	    /* The function result should not exist in the symbol table */

	    PRINTMSG(TOKEN_LINE(token), 1471, Error, TOKEN_COLUMN(token),
		     AT_OBJ_NAME_PTR(rslt_idx));
	    CREATE_ERR_ATTR(rslt_idx,
			    TOKEN_LINE(token),
			    TOKEN_COLUMN(token),
			    Data_Obj);
	 }
	 else if (fnd_semantic_err(Obj_Ntry_Func_Result,
				   TOKEN_LINE(token),
				   TOKEN_COLUMN(token),
				   rslt_idx,
				   TRUE)) {
	    CREATE_ERR_ATTR(rslt_idx,
			    TOKEN_LINE(token),
			    TOKEN_COLUMN(token),
			    Data_Obj);
	 }
	 else if (AT_REFERENCED(rslt_idx) == Char_Rslt_Bound_Ref) {
	    AT_ATTR_LINK(rslt_idx)	= NULL_IDX;
	    LN_DEF_LOC(name_idx)	= TRUE;
	 }

	 if ((cif_flags & XREF_RECS) != 0) {
	    cif_usage_rec(rslt_idx,
			  AT_Tbl_Idx,
			  TOKEN_LINE(token),
			  TOKEN_COLUMN(token),
			  CIF_Symbol_Declaration);
	 }

	 ATD_CLASS(rslt_idx)		= Function_Result;
	 ATP_RSLT_NAME(attr_idx)	= TRUE;
      }
   } 
   *out_result_idx = rslt_idx;
   return err_found;
}
#endif /* KEY Bug 14150 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine handles the FUNCTION result, whether there is a          *|
|*	result-name or not.  Every function gets a function result.           *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx - attribute index of the function.			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
static void set_function_rslt(int	attr_idx,
                              boolean	type_err)

{
   boolean		err_found	= FALSE;
   int			func_rslt_idx;
   int			rslt_idx	= NULL_IDX;


   TRACE (Func_Entry, "set_function_rslt", NULL);

#ifdef KEY /* Bug 14150 */
   int bind_ok = 1;
   int result_ok = 1;
   while (LA_CH_VALUE != EOS) {
     if (bind_ok && matched_specific_token(Tok_Kwd_Bind, Tok_Class_Keyword)) {
       bind_ok = 0;
       int line, column;
       if (AT_IS_DARG(attr_idx)) {
	 parse_language_binding_spec(0);
	 AT_BIND_ATTR(attr_idx) = TRUE;
       }
       else {
	 parse_language_binding_spec(&new_binding_label);
	 set_binding_label(AT_Tbl_Idx, attr_idx, &new_binding_label);
       }
     }
     else if (result_ok && matched_specific_token(Tok_Kwd_Result,
       Tok_Class_Keyword)) {
       result_ok = 0;
       err_found = help_set_function_rslt(attr_idx, &rslt_idx);
     } else {
       parse_err_flush(Find_EOS, "BIND, RESULT or " EOS_STR);
       err_found = TRUE;
       break;
     }
   }
#endif /* KEY Bug 14150 */

   func_rslt_idx	= ATP_RSLT_IDX(attr_idx);

   if (rslt_idx == NULL_IDX) {

      if (func_rslt_idx == NULL_IDX) {
         NTR_ATTR_TBL(rslt_idx);
         COPY_COMMON_ATTR_INFO(attr_idx, rslt_idx, Data_Obj);
         ATD_CLASS(rslt_idx)		= Function_Result;
      }
      else {
         rslt_idx			= func_rslt_idx;
      }
   }
   else if (func_rslt_idx != NULL_IDX) {

      /* The function entry has a result already.  The result is semantically */
      /* correct.  If it exists, (depending on the type of function), it will */
      /* have access set, be typed, be an array, be a pointer, or be a dummy  */
      /* arg.  The dummy arg part is handled by the caller.   A check has to  */
      /* be made if it is a pointer or array, because these items must be     */
      /* specified using the RESULT name if it exists.  Double typing on a    */
      /* function statement will be handled by parse_typed_function.   Access */
      /* just gets copied.  It can also be a target.                          */

      if (ATD_ARRAY_IDX(func_rslt_idx) != NULL_IDX) {
         err_found = TRUE;
         PRINTMSG(TOKEN_LINE(token), 27, Error, TOKEN_COLUMN(token),
                  AT_OBJ_NAME_PTR(attr_idx), AT_OBJ_NAME_PTR(rslt_idx));

         if (ATD_ARRAY_IDX(rslt_idx) == NULL_IDX) {
            ATD_ARRAY_IDX(rslt_idx) = ATD_ARRAY_IDX(func_rslt_idx);
         }
      }

      if (ATD_POINTER(func_rslt_idx)) {
         err_found = TRUE;
         PRINTMSG(TOKEN_LINE(token), 36, Error, TOKEN_COLUMN(token),
                  AT_OBJ_NAME_PTR(attr_idx), AT_OBJ_NAME_PTR(rslt_idx));

         if (!ATD_POINTER(rslt_idx)) {
            ATD_POINTER(rslt_idx)   = TRUE;
            ATD_IM_A_DOPE(rslt_idx) = TRUE;
         }
      }

      if (ATD_TARGET(func_rslt_idx)) {
         err_found = TRUE;
         PRINTMSG(TOKEN_LINE(token), 132, Error, TOKEN_COLUMN(token),
                  AT_OBJ_NAME_PTR(attr_idx), AT_OBJ_NAME_PTR(rslt_idx));

         if (!ATD_TARGET(rslt_idx)) {
            ATD_TARGET(rslt_idx)   = TRUE;
         }
      }

      if (AT_TYPED(func_rslt_idx)) { 
         err_found = TRUE;
         PRINTMSG(TOKEN_LINE(token), 185, Error, TOKEN_COLUMN(token),
                  AT_OBJ_NAME_PTR(attr_idx), AT_OBJ_NAME_PTR(rslt_idx));

         if (!AT_TYPED(rslt_idx)) {
            ATD_TYPE_IDX(rslt_idx)	= ATD_TYPE_IDX(func_rslt_idx);
            AT_TYPED(rslt_idx)		= AT_TYPED(func_rslt_idx);
         }
      }
      AT_ACCESS_SET(rslt_idx)		= AT_ACCESS_SET(func_rslt_idx);
      AT_PRIVATE(rslt_idx)		= AT_PRIVATE(func_rslt_idx);

      /* Do not use the old result index from the FUNCTION.  Clear it and     */
      /* mark as error - so it will not be used later.                        */

      CLEAR_ATTR_NTRY(func_rslt_idx);
      AT_DCL_ERR(func_rslt_idx)		= TRUE;
   }

   if (!AT_TYPED(rslt_idx) || type_err) {

      if (!AT_DCL_ERR(rslt_idx)) { 
         SET_IMPL_TYPE(rslt_idx);
      }
      else if (ATD_TYPE_IDX(rslt_idx) == NULL_IDX) {
         ATD_TYPE_IDX(rslt_idx)	= TYPELESS_DEFAULT_TYPE;
      }
   }

   ATP_RSLT_IDX(attr_idx)	= rslt_idx;
   ATD_FUNC_IDX(rslt_idx)	= attr_idx;
   AT_DCL_ERR(rslt_idx)		= err_found || AT_DCL_ERR(rslt_idx);
   AT_DCL_ERR(attr_idx)		= AT_DCL_ERR(attr_idx) || AT_DCL_ERR(rslt_idx);

   TRACE (Func_Exit, "set_function_rslt", NULL);
   
   return;

}  /* set_function_rslt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the dummy argument list.                          *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
static void parse_dummy_args(int	pgm_attr_idx)
{
   int		attr_idx;
   boolean	found_end		= FALSE;
   int		list_idx;
   int		name_idx;
   int		sn_idx;
   int		sn_attr_idx;


   TRACE (Func_Entry, "parse_dummy_args", NULL);

# ifdef _DEBUG
   if (LA_CH_VALUE != LPAREN) {
      PRINTMSG(TOKEN_LINE(token), 295, Internal, TOKEN_COLUMN(token),
               "parse_dummy_args", "LPAREN");
   }
# endif

   NEXT_LA_CH;   /* Consume Lparen */

   if (LA_CH_VALUE == RPAREN) {		/* Empty argument list */
      NEXT_LA_CH;
      return;
   }

   /* Reserve a spot for an additional dummy argument, in case this function */
   /* result turns out to need a zero'th dummy argument to return the result */
   /* This should always be reserved right before the darg list in the tbl.  */

   NTR_SN_TBL(sn_attr_idx);

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;
            AT_OBJ_CLASS(attr_idx)	= Data_Obj;
            ATD_CLASS(attr_idx)		= Dummy_Argument;
            SET_IMPL_TYPE(attr_idx);
            AT_IS_DARG(attr_idx)	= TRUE;
            AT_DCL_ERR(attr_idx)	= AT_DCL_ERR(pgm_attr_idx);

            /* Create a list of all unique dummy args in the program unit. */

            NTR_ATTR_LIST_TBL(list_idx);
            AL_NEXT_IDX(list_idx)	= SCP_DARG_LIST(curr_scp_idx);
            AL_ATTR_IDX(list_idx)	= attr_idx;
            SCP_DARG_LIST(curr_scp_idx)	= list_idx;
         }
         else if (!fnd_semantic_err(Obj_Dummy_Arg,
                                    TOKEN_LINE(token),
                                    TOKEN_COLUMN(token),
                                    attr_idx,
                                    TRUE)) {

            AT_DCL_ERR(attr_idx)	= AT_DCL_ERR(pgm_attr_idx);
  
            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)   = NULL_IDX;
               LN_DEF_LOC(name_idx)     = TRUE;
               CLEAR_VARIANT_ATTR_INFO(attr_idx, Data_Obj);
               ATD_CLASS(attr_idx)	= Dummy_Argument;
               SET_IMPL_TYPE(attr_idx);
            }
            else if ((AT_REFERENCED(attr_idx) == Referenced ||
                      AT_DEFINED(attr_idx)) && !AT_IS_DARG(attr_idx)) {

               /* If this is an entry statement, the darg may be used in a    */
               /* bounds specification expression (Dcl_Bound_Ref), but it     */
               /* may not have been used in an executable statement.          */

               PRINTMSG(TOKEN_LINE(token), 529, Error, TOKEN_COLUMN(token),
                        AT_OBJ_NAME_PTR(attr_idx));
            }

            if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {

               if (ATP_PROC(attr_idx) != Dummy_Proc) {
                  ATP_PROC(attr_idx)	=  Dummy_Proc;
               }
            }
            else if (ATD_CLASS(attr_idx) != Dummy_Argument) {
               ATD_CLASS(attr_idx)	= Dummy_Argument;
            }

            /* Create a list of all unique dummy args in the program unit. */

            if (!AT_IS_DARG(attr_idx)) {
               NTR_ATTR_LIST_TBL(list_idx);
               AL_NEXT_IDX(list_idx)		= SCP_DARG_LIST(curr_scp_idx);
               AL_ATTR_IDX(list_idx)		= attr_idx;
               SCP_DARG_LIST(curr_scp_idx)	= list_idx;
            }
            AT_IS_DARG(attr_idx)		= TRUE;
         }

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
         }


         if ((cif_flags & XREF_RECS) != 0) {
            cif_usage_rec(attr_idx,
                          AT_Tbl_Idx,
                          TOKEN_LINE(token),
                          TOKEN_COLUMN(token),
                          CIF_Symbol_Is_Dummy_Arg);
         }

	 /* Enter dummy arg name into the secondary name table */

         sn_attr_idx = srch_kwd_name(TOKEN_STR(token), TOKEN_LEN(token),
                                     pgm_attr_idx, &sn_idx);

	 if (sn_attr_idx != NULL_IDX) { /* Have duplicate dummy arg */
	    PRINTMSG(TOKEN_LINE(token), 10, Error, TOKEN_COLUMN(token),
                     TOKEN_STR(token));
	 }
         else { 
            NTR_SN_TBL(sn_idx);
            SN_ATTR_IDX(sn_idx)		= attr_idx;
            SN_NAME_LEN(sn_idx)		= AT_NAME_LEN(attr_idx);
            SN_NAME_IDX(sn_idx)		= AT_NAME_IDX(attr_idx);
            SN_LINE_NUM(sn_idx)		= TOKEN_LINE(token);
            SN_COLUMN_NUM(sn_idx)	= TOKEN_COLUMN(token);

            if (ATP_FIRST_IDX(pgm_attr_idx) == NULL_IDX) {
               ATP_FIRST_IDX(pgm_attr_idx) = sn_idx;
            }
            ATP_NUM_DARGS(pgm_attr_idx) += 1;
         }
      }
      else if (LA_CH_VALUE == STAR && 
               ATP_PGM_UNIT(pgm_attr_idx) == Subroutine) {

         /* Generate a compiler temp and point the secondary name table to it.*/
         /* Do this first to get the line and column number correct.          */

         attr_idx = gen_compiler_tmp(LA_CH_LINE, LA_CH_COLUMN, Shared, TRUE);

         NEXT_LA_CH;		/* Skip star */


         AT_REFERENCED(attr_idx)		= Referenced;
         AT_DEFINED(attr_idx)			= TRUE;
         AT_SEMANTICS_DONE(attr_idx)		= TRUE;
         ATD_TYPE_IDX(attr_idx)			= INTEGER_DEFAULT_TYPE;
         ATD_STOR_BLK_IDX(attr_idx)		= SCP_SB_DARG_IDX(curr_scp_idx);
         ATD_CLASS(attr_idx)			= Dummy_Argument;
         AT_IS_DARG(attr_idx)			= TRUE;
         ATP_HAS_ALT_RETURN(pgm_attr_idx)	= TRUE;
         ATP_HAS_ALT_RETURN(SCP_ATTR_IDX(curr_scp_idx))	= TRUE;

	 NTR_SN_TBL(sn_attr_idx);
         SN_ATTR_IDX(sn_attr_idx)		= attr_idx;
         SN_NAME_IDX(sn_attr_idx)		= AT_NAME_IDX(attr_idx);
         SN_LINE_NUM(sn_attr_idx)		= LA_CH_LINE;
         SN_COLUMN_NUM(sn_attr_idx)		= LA_CH_COLUMN;

	 if (ATP_FIRST_IDX(pgm_attr_idx) == NULL_IDX) {
	    ATP_FIRST_IDX(pgm_attr_idx) = sn_attr_idx;
	 }
	 ATP_NUM_DARGS(pgm_attr_idx) +=1;
      }
      else {
         parse_err_flush(Find_Comma_Rparen, "dummy-arg-name");
         found_end = (LA_CH_VALUE == EOS);
      }

      if (LA_CH_VALUE != RPAREN && LA_CH_VALUE != COMMA && !found_end) {
         parse_err_flush(Find_Comma_Rparen, ", or )");
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else {
         found_end = TRUE;
      }
   }  /* end do while */
   while (!found_end);

   /* check if this is the largest arg list seen yet.  */
   /* Since some of these could be optional must check */
   /* both here and parse_actual_arg_spec.             */

   if (ATP_NUM_DARGS(pgm_attr_idx) > max_call_list_size) {
      max_call_list_size = (long) ATP_NUM_DARGS(pgm_attr_idx);
   }

   if (LA_CH_VALUE == RPAREN) {
      NEXT_LA_CH;	/* Consume RPAREN */
   }

   TRACE (Func_Exit, "parse_dummy_args", NULL);

   return;

}  /* parse_dummy_args */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Routine called by parse_function and parse_subroutine.  It performs   *|
|*	common tasks such as srch and ntr the program name into the symbol    *|
|*	table,  sets the appropriate fields in the attr entry, establishes    *|
|*	the proper block.  If appropriate, it enters the subprogram name      *|
|*	into a new scope.						      *|
|*									      *|
|*									      *|
|* Input parameters:							      *|
|*	pgm_type	The type of program being processed.                  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static int start_new_subpgm(pgm_unit_type	pgm_type,
			    boolean		has_error,
			    boolean		save_idxs)

{
   int			attr_idx;
   int			host_name_idx;
   int			interface_idx		= NULL_IDX;
   int			ir_idx;
   int			length;
   int			loc_name_idx;
   int			name_idx;
   atp_proc_type	proc_type;
   int			sb_idx;
   int			sn_idx;
   int			tmp_attr_idx;
   obj_type		type_of_obj;			


   TRACE (Func_Entry, "start_new_subpgm", NULL);

   if (CURR_BLK == Interface_Blk) {
      interface_idx	= CURR_BLK_NAME;

      if (interface_idx) {

         if (ATI_INTERFACE_CLASS(interface_idx) == Generic_Unknown_Interface) {
            ATI_INTERFACE_CLASS(interface_idx) = (pgm_type == Function) ?
                                                  Generic_Function_Interface :
                                                  Generic_Subroutine_Interface;
         }
      }
      else {
         interface_idx = BLK_UNNAMED_INTERFACE(blk_stk_idx);
      }

      ATI_HAS_NON_MOD_PROC(interface_idx)       = TRUE;

      /* This assumes that the attr table is always initialized with */
      /* some kind of entries at start up.                           */

      if (save_idxs && BLK_AT_IDX(blk_stk_idx) == NULL_IDX) {
         BLK_AT_IDX(blk_stk_idx)	= attr_tbl_idx;
         BLK_BD_IDX(blk_stk_idx)	= bounds_tbl_idx;
         BLK_CN_IDX(blk_stk_idx)	= const_tbl_idx;
         BLK_CP_IDX(blk_stk_idx)	= const_pool_idx;
         BLK_NP_IDX(blk_stk_idx)	= name_pool_idx;
         BLK_SB_IDX(blk_stk_idx)	= stor_blk_tbl_idx;
         BLK_SN_IDX(blk_stk_idx)	= sec_name_tbl_idx;
         BLK_TYP_IDX(blk_stk_idx)	= type_tbl_idx;
      }
         
      PUSH_BLK_STK(Interface_Body_Blk);
      CURR_BLK_NO_EXEC	= TRUE;
      proc_type		= Extern_Proc;
      type_of_obj	= (pgm_type == Function) ? Obj_Interface_Func :
                                                   Obj_Interface_Subr;
   }
   else if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {

      /* Do not check if comp unit is module, because only the first contains */
      /* in a MODULE is a module procedure.  Subsequent ones are Internal proc*/

      PUSH_BLK_STK(Module_Proc_Blk);
      type_of_obj	= (pgm_type == Function) ? Obj_Module_Func :
                                                   Obj_Module_Subr;
      proc_type		= Module_Proc;
   }
   else { /* Must be Internal_Blk */
      PUSH_BLK_STK(Internal_Blk);
      proc_type		= Intern_Proc;
      type_of_obj	= (pgm_type == Function) ? Obj_Intern_Func :
                                                   Obj_Intern_Subr;
   }

   /* Search parent for subprogram name. */

   attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);

   if (CURR_BLK == Interface_Body_Blk) {

      /* If the attr_idx was not found in the interface block's parent     */
      /* entry and this is an interface block inside a contains block -    */
      /* and NOT inside a contains block inside a contains block.  Search  */
      /* the interface block's parent's host for the name.  If the name is */
      /* found and it is the main entry name or an alternate entry name -  */
      /* issue an error.  Recovery = treat this as a new interface entry.  */

      if (SCP_LEVEL(curr_scp_idx) == 1 &&
          (attr_idx == NULL_IDX || AT_OBJ_CLASS(attr_idx) == Interface)) {

         attr_idx = srch_host_sym_tbl(TOKEN_STR(token), 
                                      TOKEN_LEN(token),
                                      &host_name_idx,
                                      TRUE);

         if (attr_idx != NULL_IDX &&
             !SH_ERR_FLG(curr_stmt_sh_idx) &&
             (SCP_ATTR_IDX(SCP_PARENT_IDX(curr_scp_idx)) == attr_idx ||
              AT_OBJ_CLASS(attr_idx) == Pgm_Unit && 
              ATP_ALT_ENTRY(attr_idx)) ) {

            /* Issue error - The entry pointer name of the host, "%s" must */
            /*               not be redefined in an interface body.        */
            /* Set SCP_ATTR so that the message pgm unit will be correct.  */

            curr_scp_idx		= SCP_LAST_CHILD_IDX(curr_scp_idx);
            SCP_ATTR_IDX(curr_scp_idx)	= attr_idx;
            PRINTMSG(TOKEN_LINE(token), 44, Error, TOKEN_COLUMN(token),
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx)	= TRUE;
            curr_scp_idx		= SCP_PARENT_IDX(curr_scp_idx);
         }
         attr_idx = NULL_IDX;
      }
   }

   if (attr_idx == NULL_IDX) {		/* enter into parent's host   */
      attr_idx			= ntr_sym_tbl(&token, name_idx);
      AT_OBJ_CLASS(attr_idx)	= Pgm_Unit;
      LN_DEF_LOC(name_idx)	= TRUE;
      ATP_PROC(attr_idx)	= proc_type;
      ATP_PGM_UNIT(attr_idx)	= pgm_type;
   }
   else if (AT_NOT_VISIBLE(attr_idx)) { /* Not visible in parent's host */
      curr_scp_idx			= SCP_LAST_CHILD_IDX(curr_scp_idx);
      SCP_ATTR_IDX(curr_scp_idx)	= attr_idx;  /* Set for message */

      PRINTMSG(TOKEN_LINE(token), 486, Error,
               TOKEN_COLUMN(token),
               AT_OBJ_NAME_PTR(attr_idx),
               AT_OBJ_NAME_PTR(AT_MODULE_IDX((attr_idx))));

      CREATE_ERR_ATTR(attr_idx,
                      TOKEN_LINE(token), 
                      TOKEN_COLUMN(token),
                      Pgm_Unit);

      ATP_PROC(attr_idx)		= proc_type;
      ATP_PGM_UNIT(attr_idx)		= pgm_type;
      AT_TYPED(attr_idx)		= FALSE;
      curr_scp_idx			= SCP_PARENT_IDX(curr_scp_idx);
   }
   else if (CURR_BLK == Interface_Body_Blk && interface_idx == attr_idx) {

      /* This subprogram name is the same as it's generic interface - legal */

      NTR_ATTR_TBL(tmp_attr_idx);
      COPY_COMMON_ATTR_INFO(attr_idx, tmp_attr_idx, Pgm_Unit);
      ATI_PROC_IDX(attr_idx)	= tmp_attr_idx;
      attr_idx			= tmp_attr_idx;
      ATP_PROC(attr_idx)	= proc_type;
      ATP_PGM_UNIT(attr_idx)	= pgm_type;
      AT_DEF_LINE(attr_idx)	= TOKEN_LINE(token);
      AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(token);
      AT_IS_INTRIN(attr_idx)	= FALSE;
      AT_ELEMENTAL_INTRIN(attr_idx)	= FALSE;
   }
   else if (CURR_BLK == Interface_Body_Blk && 
            SCP_ATTR_IDX(curr_scp_idx) == attr_idx) {

      /* Allow the user to specify an interface block description for the     */
      /* function or subroutine being compiled.  Issue an ANSI message later. */

      NTR_ATTR_TBL(tmp_attr_idx);
      COPY_COMMON_ATTR_INFO(attr_idx, tmp_attr_idx, Pgm_Unit);
      ATP_DUPLICATE_INTERFACE_IDX(attr_idx)	= tmp_attr_idx;
      attr_idx					= tmp_attr_idx;
      ATP_PROC(attr_idx)			= proc_type;
      ATP_PGM_UNIT(attr_idx)			= pgm_type;
      AT_DEF_LINE(attr_idx)			= TOKEN_LINE(token);
      AT_DEF_COLUMN(attr_idx)			= TOKEN_COLUMN(token);
      AT_IS_INTRIN(attr_idx)			= FALSE;
      AT_ELEMENTAL_INTRIN(attr_idx)		= FALSE;
   }
   else {

      if (AT_OBJ_CLASS(attr_idx) == Interface && 
          ATI_PROC_IDX(attr_idx) != NULL_IDX) {
         attr_idx = ATI_PROC_IDX(attr_idx);
      }

      if (proc_type == Intern_Proc &&
          AT_ATTR_LINK(attr_idx) != NULL_IDX && 
          AT_LOCKED_IN(attr_idx)) {

         do {
            tmp_attr_idx = AT_ATTR_LINK(attr_idx);
         }
         while (AT_ATTR_LINK(tmp_attr_idx) != NULL_IDX);

         if (AT_OBJ_CLASS(tmp_attr_idx) == Data_Obj &&
             ATD_CLASS(tmp_attr_idx) == Constant) {

            /* If an error is issued, it will get caught in the next if   */
            /* statement and will go through error processing.            */

            curr_scp_idx		= SCP_LAST_CHILD_IDX(curr_scp_idx);
            SCP_ATTR_IDX(curr_scp_idx)	= attr_idx;  /* Set for msg  */

            PRINTMSG(TOKEN_LINE(token), 919, Error,
                     TOKEN_COLUMN(token),
                     AT_OBJ_NAME_PTR(attr_idx),
                     (pgm_type == Function) ? "FUNCTION" : "SUBROUTINE");
            curr_scp_idx		= SCP_PARENT_IDX(curr_scp_idx);
         }
      }

      if (SH_ERR_FLG(curr_stmt_sh_idx) || 
          attr_idx == glb_tbl_idx[Main_Attr_Idx] ||
          AT_DCL_ERR(attr_idx) ||
          fnd_semantic_err(type_of_obj,
                           TOKEN_LINE(token), 
                           TOKEN_COLUMN(token),
                           attr_idx,
                           TRUE)) {

         /* If this already has an error, or it is $MAIN, or there is a    */
         /* semantic error ==> To prevent errs, create a second attr entry */
         /* for this name.  The local name points to the first (marked in  */
         /* error), the 2nd is used to finish parsing this statement.      */

         CREATE_ERR_ATTR(attr_idx,
                         TOKEN_LINE(token), 
                         TOKEN_COLUMN(token),
                         Pgm_Unit);
         AT_TYPED(attr_idx)	= FALSE;
         ATP_PROC(attr_idx)	= proc_type;
         ATP_PGM_UNIT(attr_idx)	= pgm_type;
      }
      else if (CURR_BLK != Interface_Body_Blk && 
               proc_type == Module_Proc &&
               (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
                ATP_PROC(attr_idx) == Module_Proc &&
                ATP_EXPL_ITRFC(attr_idx))) {

         /* This is already declared as a module procedure. */

         curr_scp_idx			= SCP_LAST_CHILD_IDX(curr_scp_idx);
         SCP_ATTR_IDX(curr_scp_idx)	= attr_idx;  /* Set for msg  */

         PRINTMSG(TOKEN_LINE(token), 1529, Error,
                  TOKEN_COLUMN(token),
                  AT_OBJ_NAME_PTR(attr_idx));
         curr_scp_idx			= SCP_PARENT_IDX(curr_scp_idx);
      }
      else if (AT_OBJ_CLASS(attr_idx) == Interface) {

         if (AT_IS_INTRIN(attr_idx) && !LN_DEF_LOC(name_idx)) {

            /* This is just an intrinsic, because of a reference.  It has not */
            /* been declared in an intrinsic statement or as an interface.    */

            CLEAR_VARIANT_ATTR_INFO(attr_idx, Pgm_Unit);
            AT_ATTR_LINK(attr_idx)	= NULL_IDX;
            AT_IS_INTRIN(attr_idx)	= FALSE;
            AT_ELEMENTAL_INTRIN(attr_idx)= FALSE;
            ATP_PROC(attr_idx)		= proc_type;
            ATP_PGM_UNIT(attr_idx)	= pgm_type;
            AT_USE_ASSOCIATED(attr_idx)	= FALSE;
            AT_DEF_LINE(attr_idx)	= TOKEN_LINE(token);
            AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(token);
            LN_DEF_LOC(name_idx)	= TRUE;
         }
         else {
            NTR_ATTR_TBL(tmp_attr_idx);
            COPY_COMMON_ATTR_INFO(attr_idx, tmp_attr_idx, Pgm_Unit);
            ATI_PROC_IDX(attr_idx)	= tmp_attr_idx;
            attr_idx			= tmp_attr_idx;
            AT_USE_ASSOCIATED(attr_idx)	= FALSE;
            MAKE_EXTERNAL_NAME(attr_idx,
                               AT_NAME_IDX(attr_idx),
                               AT_NAME_LEN(attr_idx));
            ATP_PROC(attr_idx)		= proc_type;
            ATP_PGM_UNIT(attr_idx)	= pgm_type;
            AT_DEF_LINE(attr_idx)	= TOKEN_LINE(token);
            AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(token);
            AT_IS_INTRIN(attr_idx)	= FALSE;
            AT_ELEMENTAL_INTRIN(attr_idx)	= FALSE;
         }
      }
      else {

         /* Break any links that this found attr has with host association.*/
         /* Because this subpgm definition makes this the correct thing    */
         /* for the current scope to use.  If it has a function result     */
         /* break the function result links also.                          */

         AT_ATTR_LINK(attr_idx)	= NULL_IDX;
         LN_DEF_LOC(name_idx)	= TRUE;

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            chg_data_obj_to_pgm_unit(attr_idx, pgm_type, proc_type);
         }
         else {
            ATP_PROC(attr_idx)		= proc_type;
            ATP_PGM_UNIT(attr_idx)	= pgm_type;

            if (pgm_type == Function && ATP_RSLT_IDX(attr_idx) != NULL_IDX) {
               AT_ATTR_LINK(ATP_RSLT_IDX(attr_idx))	= NULL_IDX;
            }
         }
         AT_DEF_LINE(attr_idx)		= TOKEN_LINE(token);
         AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(token);
      }

   }

   /* The scope for this routine was created before calling this routine */
   /* so that any type variables would get into the correct scope.  At   */
   /* the time this routine was called, curr_scp_idx points to the       */
   /* parent.  Now set curr_scp_idx and SCP_ATTR_IDX correctly.          */

   curr_scp_idx			= SCP_LAST_CHILD_IDX(curr_scp_idx);
   SCP_ATTR_IDX(curr_scp_idx)	= attr_idx;  /* Set in case of msg  */
   ATP_SCP_IDX(attr_idx)	= curr_scp_idx;
   AT_DCL_ERR(attr_idx)		= AT_DCL_ERR(attr_idx) || has_error;

   /* Add entry to local scope - but point to parent's attr.  */

   tmp_attr_idx			= srch_sym_tbl(TOKEN_STR(token), 
                                              TOKEN_LEN(token),
                                              &loc_name_idx);

   /* Carry the index to the parent's attr.  This is for use in USE    */
   /* processing.  If the module/internal procedure is written out and */
   /* then USEd back again for inlining purposes, we need to know the  */
   /* parent so we can send the procedure IR/SH through the interface. */

   ATP_PARENT_IDX(attr_idx)	= SCP_ATTR_IDX(SCP_PARENT_IDX(curr_scp_idx));

   if (tmp_attr_idx != NULL_IDX) {     /* Have fatal error - must not be seen */
      fnd_semantic_err(type_of_obj,
                       TOKEN_LINE(token),
                       TOKEN_COLUMN(token),
                       tmp_attr_idx,
                       TRUE);

      /* KAY - This is where some of the scoping gets fixed.  Think about     */
      /*       AT_NOT_VISIBLE here.                                           */

      /* This is found in the following situation.  FUNCTION is an internal   */
      /* or module procedure.   CHARACTER*(A) FUNCTION A()     The A is in    */
      /* the local table already.  Replace that attr with the host's attr.    */
      /* AT_DCL_ERR for tmp_attr_idx is set by fnd_semantic_err.              */
      /* During attr resolution, the host symbol table will be researched for */
      /* A.  Then AT_ATTR_LINK for A will point to the internal procedure.    */
      /* This will get flagged when the bounds are checked.                   */

      LN_ATTR_IDX(loc_name_idx)	= attr_idx;
      LN_NAME_IDX(loc_name_idx)	= AT_NAME_IDX(attr_idx);
   }
   else {

      /* Enter in local symbol table, but do NOT create a new attr entry.     */
      /* FALSE means use the same attr entry for the host and the local.      */

      attr_idx	= ntr_host_in_sym_tbl(&token, loc_name_idx, attr_idx, name_idx,
                                      FALSE);
   }

   LN_DEF_LOC(loc_name_idx)	= TRUE;
   curr_stmt_category		= Dir_Integer_Stmt_Cat;
   CURR_BLK_NAME		= attr_idx;
   ATP_EXPL_ITRFC(attr_idx)	= TRUE;

   if ((cif_flags & XREF_RECS) != 0) {
      cif_usage_rec(attr_idx,
                    AT_Tbl_Idx,
                    TOKEN_LINE(token),
                    TOKEN_COLUMN(token),
                    CIF_Symbol_Declaration);
   }

   /* Generate a SH for interface blocks.  This is needed to make bounds */
   /* resolution work.  This will be thrown out later.                   */

   CURR_BLK_FIRST_SH_IDX = 
                  (SH_STMT_TYPE(SCP_FIRST_SH_IDX(curr_scp_idx)) != Label_Def) ?
                            SCP_FIRST_SH_IDX(curr_scp_idx) :
                            IR_IDX_L(SH_IR_IDX(SCP_FIRST_SH_IDX(curr_scp_idx)));

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
   IR_OPR(ir_idx)              = Entry_Opr; 
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)         = TOKEN_LINE(token);
   IR_COL_NUM(ir_idx)          = TOKEN_COLUMN(token);
   IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
   IR_IDX_L(ir_idx)            = attr_idx;
   IR_COL_NUM_L(ir_idx)        = TOKEN_COLUMN(token);
   IR_LINE_NUM_L(ir_idx)       = TOKEN_LINE(token);

   ATP_SCP_ALIVE(attr_idx) = TRUE;

   if (CURR_BLK == Interface_Body_Blk) {
      MAKE_EXTERNAL_NAME(attr_idx,
                         AT_NAME_IDX(attr_idx),
                         AT_NAME_LEN(attr_idx));
      ATP_IN_INTERFACE_BLK(attr_idx)	= TRUE;
      ATP_IN_UNNAMED_INTERFACE(attr_idx)= ATI_UNNAMED_INTERFACE(interface_idx);

      if (interface_idx != NULL_IDX) {     /* Generic or Defined interface    */
         sn_idx		= ATI_FIRST_SPECIFIC_IDX(interface_idx);
         tmp_attr_idx	= srch_linked_sn(TOKEN_STR(token),
                                         TOKEN_LEN(token),
                                         &sn_idx);

         if (tmp_attr_idx == NULL_IDX) {

            /* Not found - intentionally blank */
         }
         else if (AT_IS_INTRIN(tmp_attr_idx)) {

            /* The user is overloading intrinsics - allow */
         }
         else if (ATP_SCP_IDX(attr_idx) == curr_scp_idx &&
                  !AT_USE_ASSOCIATED(attr_idx) &&
                  !ATI_UNNAMED_INTERFACE(interface_idx)) {

            if (!AT_DCL_ERR(attr_idx)) {
               PRINTMSG(TOKEN_LINE(token), 671, Error,
                        TOKEN_COLUMN(token),
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(interface_idx));
               AT_DCL_ERR(attr_idx)  = TRUE;
            }

            /* Add it, marked in error for better error recovery. */

         }
         else {  /* Intentionally blank */

            /* Found, but it is from a different scope. */
         }

#ifdef KEY /* Bug 4197 */
	 /*
	  * Remove intrinsics if necessary to implement this provision of
	  * the F95 standard (14.1.2.3): "If a generic name is the same as
	  * the name of a generic intrinsic procedure, the generic intrinsic
	  * procedure is not accessible if the procedures in the interface
	  * and the intrinsic procedure are not all functions or not all
	  * subroutines."
	  */
	 if (AT_IS_INTRIN(interface_idx)) {
	   boolean adding_subroutine = (Subroutine == ATP_PGM_UNIT(attr_idx));
	   if (adding_subroutine != (Generic_Subroutine_Interface ==
	     ATI_INTERFACE_CLASS(interface_idx))) {
	     AT_IS_INTRIN(interface_idx) = FALSE;
	     ATI_FIRST_SPECIFIC_IDX(interface_idx) = NULL_IDX;
	     ATI_NUM_SPECIFICS(interface_idx) = 0;
	     ATI_INTERFACE_CLASS(interface_idx) = adding_subroutine ?
	       Generic_Subroutine_Interface :
	       Generic_Function_Interface;
	   }
	 }
#endif /* KEY Bug 4197 */

         NTR_INTERFACE_IN_SN_TBL(sn_idx,
                                 attr_idx,
                                 interface_idx,
                                 TOKEN_LINE(token),
                                 TOKEN_COLUMN(token));
      }

      /* All static blocks inside interface bodies will be treated as if  */
      /* they are host associated.                                        */

      sb_idx			= SCP_SB_STATIC_IDX(curr_scp_idx);
      SB_HOSTED_STATIC(sb_idx)	= TRUE;
      SB_BLK_TYPE(sb_idx)	= Static;
      SB_RUNTIME_INIT(sb_idx)	= FALSE;

      SCP_SB_HOSTED_STATIC_IDX(curr_scp_idx)	= sb_idx;

      /* The @DATA block needs to be renamed to @DATA_in_PGM_UNIT_NAME_in_.. */

      SB_NAME_IDX(sb_idx)	= make_in_parent_string(SB_NAME_IDX(sb_idx),
                                                        SB_NAME_LEN(sb_idx),
                                                        curr_scp_idx,
                                                        &length);
      SB_NAME_LEN(sb_idx)	= length;

      if (sb_idx != SCP_SB_STATIC_INIT_IDX(curr_scp_idx)) {
         sb_idx                    	= SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
         SB_HOSTED_STATIC(sb_idx)  	= TRUE;
         SB_BLK_TYPE(sb_idx)       	= Static_Named;
         SB_RUNTIME_INIT(sb_idx)	= FALSE;
         SCP_SB_HOSTED_DATA_IDX(curr_scp_idx) = sb_idx;

         SB_NAME_IDX(sb_idx)       = make_in_parent_string(SB_NAME_IDX(sb_idx),
                                                           SB_NAME_LEN(sb_idx),
                                                           curr_scp_idx,
                                                           &length);
         SB_NAME_LEN(sb_idx)       = length;
      }

      if (sb_idx != SCP_SB_STATIC_UNINIT_IDX(curr_scp_idx)) {
         sb_idx                    = SCP_SB_STATIC_UNINIT_IDX(curr_scp_idx);
         SB_HOSTED_STATIC(sb_idx)  = TRUE;
         SB_BLK_TYPE(sb_idx)       = Static_Named;
         SB_RUNTIME_INIT(sb_idx)   = FALSE;
         SB_NAME_IDX(sb_idx)       = make_in_parent_string(SB_NAME_IDX(sb_idx),
                                                           SB_NAME_LEN(sb_idx),
                                                           curr_scp_idx,
                                                           &length);
         SB_NAME_LEN(sb_idx)       = length;
      }
   }
   else {  /* Give internal and module procs, unique external names */
      ATP_EXT_NAME_IDX(attr_idx) = make_in_parent_string(AT_NAME_IDX(attr_idx),
                                                  AT_NAME_LEN(attr_idx),
                                                  SCP_PARENT_IDX(curr_scp_idx),
                                                  &length);
      ATP_EXT_NAME_LEN(attr_idx) = length;

      if (cmd_line_flags.debug_lvl <= Debug_Lvl_1) {  /* -ez -ed  -G0 -G1 */
         gen_end_prologue_debug_label(attr_idx);
      }

      /* The @DATA block needs to be renamed to @DATA_in_PGM_UNIT_NAME_in_.. */

      sb_idx			= SCP_SB_STATIC_IDX(curr_scp_idx);
      SB_NAME_IDX(sb_idx)	= make_in_parent_string(SB_NAME_IDX(sb_idx),
                                                        SB_NAME_LEN(sb_idx),
                                                        curr_scp_idx,
                                                        &length);
      SB_NAME_LEN(sb_idx)	= length;

      if (sb_idx != SCP_SB_STATIC_INIT_IDX(curr_scp_idx)) {
         sb_idx                    = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
         SB_NAME_IDX(sb_idx)       = make_in_parent_string(SB_NAME_IDX(sb_idx),
                                                           SB_NAME_LEN(sb_idx),
                                                           curr_scp_idx,
                                                           &length);
         SB_NAME_LEN(sb_idx)       = length;
      }

      if (sb_idx != SCP_SB_STATIC_UNINIT_IDX(curr_scp_idx)) {
         sb_idx                    = SCP_SB_STATIC_UNINIT_IDX(curr_scp_idx);
         SB_NAME_IDX(sb_idx)       = make_in_parent_string(SB_NAME_IDX(sb_idx),
                                                           SB_NAME_LEN(sb_idx),
                                                           curr_scp_idx,
                                                           &length);
         SB_NAME_LEN(sb_idx)       = length;
      }

      ATP_MAY_INLINE(attr_idx) = 
              ATP_MAY_INLINE(SCP_ATTR_IDX(SCP_PARENT_IDX(curr_scp_idx)));
   }

   /* If basic CIF records were requested, output the Begin Scope record for  */
   /* this interface body, module procedure, or internal procedure.           */

   if (cif_flags & BASIC_RECS) {
      cif_begin_scope_rec();
   }

   if (CURR_BLK == Interface_Body_Blk) {

      /* Reset this so that implicit_use_semantics will happen for interfaces */
      cdir_switches.implicit_use_idx	= cmd_line_flags.implicit_use_idx;
   }

   implicit_use_semantics();

   TRACE (Func_Exit, "start_new_subpgm", NULL);

   return(attr_idx);

}  /* start_new_subpgm */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Generate a debug label for the end of prologue code.		      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	attr_idx   Index to the new attribute entry.			      *|
|*									      *|
\******************************************************************************/
static	void	gen_end_prologue_debug_label(int	attr_idx)

{
   int		ir_idx;
   int		lbl_attr_idx;


   TRACE (Func_Entry, "gen_end_prologue_debug_label", NULL);

   /* Generate a debug label for the end of prologue.  The label must   */
   /* have the same name as the program unit.                           */

   NTR_ATTR_TBL(lbl_attr_idx);
   COPY_COMMON_ATTR_INFO(attr_idx, lbl_attr_idx, Label);
   AT_DEFINED(lbl_attr_idx)		= TRUE;
   ATL_CLASS(lbl_attr_idx)		= Lbl_Debug;
   ATL_DEBUG_CLASS(lbl_attr_idx)	= Ldbg_End_Prologue;

   if (ATP_EXT_NAME_IDX(attr_idx) != NULL_IDX) {
      AT_NAME_LEN(lbl_attr_idx)		= ATP_EXT_NAME_LEN(attr_idx);
      AT_NAME_IDX(lbl_attr_idx)		= ATP_EXT_NAME_IDX(attr_idx);
   }

   ADD_ATTR_TO_LOCAL_LIST(lbl_attr_idx);

   gen_sh(After,
          Continue_Stmt,
          SH_GLB_LINE(curr_stmt_sh_idx),
          SH_COL_NUM(curr_stmt_sh_idx),
          FALSE,                        /* No errors */
          TRUE,                         /* Labeled */
          TRUE);                        /* Compiler generated */

   SH_P2_SKIP_ME(curr_stmt_sh_idx)      = TRUE;

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx)		= ir_idx;
   IR_OPR(ir_idx)               	= Label_Opr;
   IR_TYPE_IDX(ir_idx)          	= TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)          	= SH_GLB_LINE(curr_stmt_sh_idx);
   IR_COL_NUM(ir_idx)           	= SH_COL_NUM(curr_stmt_sh_idx);
   IR_LINE_NUM_L(ir_idx)        	= SH_GLB_LINE(curr_stmt_sh_idx);
   IR_COL_NUM_L(ir_idx)         	= SH_COL_NUM(curr_stmt_sh_idx);
   IR_FLD_L(ir_idx)             	= AT_Tbl_Idx;
   IR_IDX_L(ir_idx)             	= lbl_attr_idx;
   ATL_DEF_STMT_IDX(lbl_attr_idx)	= curr_stmt_sh_idx;

   TRACE (Func_Exit, "gen_end_prologue_debug_label", NULL);

   return;

}  /* gen_end_prologue_debug_label */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Common code to start all new external program units.		      *|
|*									      *|
|* Input parameters:							      *|
|*	pgm_type   Module, Program, Function, Subroutine, Blockdata           *|
|*	blk_type   The block type.                                            *|
|*      no_name_entry   TRUE if this is $MAIN entry                           *|
|*      parse_error     TRUE if there is a parse error on the prog unit stmt. *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      defer_msg  If this is nonzero, then set this to 1003 or 1009 if either*|
|*                 of the messages needs to be issued, but do not issue the   *|
|*                 message.  This delay is necessary for some error recovery  *|
|*                 situations.                                                *|
|*									      *|
|* Returns:								      *|
|*	attr_idx   Index to the new attribute entry.			      *|
|*									      *|
\******************************************************************************/
#ifdef KEY /* Bug 8261 */
int start_new_prog_unit_by_token(pgm_unit_type, blk_cntxt_type, boolean,
  boolean, int *, token_type *);
#endif /* KEY Bug 8261 */

int	start_new_prog_unit(pgm_unit_type	pgm_type,
			    blk_cntxt_type	blk_type,
			    boolean		no_name_entry,
			    boolean		parse_error,
                            int                *defer_msg)
#ifdef KEY /* Bug 8261 */
{
  return start_new_prog_unit_by_token(pgm_type, blk_type, no_name_entry,
    parse_error, defer_msg, &token);
}

/* Like start_new_prog_unit, but takes token as an argument instead of
 * assuming it can use the global variable */
int start_new_prog_unit_by_token(pgm_unit_type pgm_type,
  blk_cntxt_type blk_type, boolean  no_name_entry, boolean  parse_error,
  int *defer_msg, token_type *token)
#endif /* KEY Bug 8261 */
{
   		int	attr_idx;
   static	int	num_main_program	= 0;
   static	int	num_no_name_entry	= 0;
		boolean	has_task_dirs		= FALSE;
   		int	ir_idx;
		int	length;
		int	message;
   		int	name_idx;
   		int	save_sh_idx;
   		int	sb_idx;


   TRACE (Func_Entry, "start_new_prog_unit", NULL);

   if (!no_name_entry) {

      if (curr_stmt_category != Init_Stmt_Cat) {
         iss_blk_stk_err();
         SCP_IN_ERR(curr_scp_idx) = TRUE;
 
         /* DO NOT SET CURR_BLK_ERR here - because if context error - this */
         /* stmt cleared the block stack and started over.                 */
      }

      curr_stmt_category	= Dir_Integer_Stmt_Cat;		
   }

#ifdef KEY /* Bug 8261 */
   attr_idx = srch_sym_tbl(TOKEN_STR(*token), TOKEN_LEN(*token), &name_idx);
#else /* KEY Bug 8261 */
   attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx);
#endif /* KEY Bug 8261 */

   if (attr_idx == NULL_IDX) {
#ifdef KEY /* Bug 8261 */
      attr_idx				= ntr_sym_tbl(token, name_idx);
#else /* KEY Bug 8261 */
      attr_idx				= ntr_sym_tbl(&token, name_idx);
#endif /* KEY Bug 8261 */
      AT_DCL_ERR(attr_idx)		= parse_error;
      SCP_ATTR_IDX(curr_scp_idx)	= attr_idx;
      message				= 0;

      if (no_name_entry) {

         /* This routine was called to handle a missing PROGRAM statement. */
         /* TOKEN has been set to main_token and is $MAIN.  The ntr caused */
         /* $MAIN to be in the name_pool twice.  That shouldn't cause      */
         /* problems.  The 2nd attr entry is deleted.  Needed to call      */
         /* ntr_sym_tbl to get name into the local name table.             */

         has_task_dirs = ATP_HAS_TASK_DIRS(glb_tbl_idx[Main_Attr_Idx]);
         attr_idx			= glb_tbl_idx[Main_Attr_Idx];
#ifdef KEY /* Bug 8261 */
         AT_DEF_LINE(attr_idx)		= TOKEN_LINE(*token);
         AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(*token);
         AT_NAME_LEN(attr_idx)		= TOKEN_LEN(*token);
#else /* KEY Bug 8261 */
         AT_DEF_LINE(attr_idx)		= TOKEN_LINE(token);
         AT_DEF_COLUMN(attr_idx)	= TOKEN_COLUMN(token);
         AT_NAME_LEN(attr_idx)		= TOKEN_LEN(token);
#endif /* KEY Bug 8261 */
         AT_NAME_IDX(attr_idx)		= LN_NAME_IDX(name_idx);
         AT_DEFINED(attr_idx)		= TRUE;
         LN_ATTR_IDX(name_idx)		= attr_idx;
         SCP_ATTR_IDX(curr_scp_idx)	= attr_idx;
         attr_tbl_idx--;                /* Delete the new $MAIN attr, use old */
         attr_aux_tbl_idx--;            /* Delete the new $MAIN attr, use old */

         if (++num_no_name_entry == 2) {
            message     = 1003;   /* Issue for 2nd unnamed pgm unit only */
         }
         else if (++num_main_program == 2) {
            message     = 1009;  /* Issue for 2nd pgm unit only */
         }
      }
      else if (pgm_type == Program && ++num_main_program == 2) {
         message        = 1009;  /* Issue for 2nd pgm unit only */
      }

      if (message != 0 && !parse_error) {

         if (*defer_msg > 0) {
            *defer_msg = message;
         }
         else if (!parse_error) {

#ifdef KEY /* Bug 8261 */
            PRINTMSG(TOKEN_LINE(*token), message, 
# if defined(_ERROR_DUPLICATE_GLOBALS)
                     Error, 
# else
                     Warning,
# endif
                     TOKEN_COLUMN(*token));
#else /* KEY Bug 8261 */
            PRINTMSG(TOKEN_LINE(token), message, 
# if defined(_ERROR_DUPLICATE_GLOBALS)
                     Error, 
# else
                     Warning,
# endif
                     TOKEN_COLUMN(token));
#endif /* KEY Bug 8261 */
         }
      }
   }
   else if (pgm_type == Function) {
      SCP_ATTR_IDX(curr_scp_idx)	= attr_idx;

      /* CHARACTER*(BAD) FUNCTION BAD()   - illegal - Cannot have been found  */

#ifdef KEY /* Bug 8261 */
      PRINTMSG(TOKEN_LINE(*token), 666, Error, TOKEN_COLUMN(*token),
               AT_OBJ_NAME_PTR(attr_idx));
      CREATE_ERR_ATTR(attr_idx, TOKEN_LINE(*token),
                      TOKEN_COLUMN(*token), Pgm_Unit);
#else /* KEY Bug 8261 */
      PRINTMSG(TOKEN_LINE(token), 666, Error, TOKEN_COLUMN(token),
               AT_OBJ_NAME_PTR(attr_idx));
      CREATE_ERR_ATTR(attr_idx, TOKEN_LINE(token),
                      TOKEN_COLUMN(token), Pgm_Unit);
#endif /* KEY Bug 8261 */
      SCP_IN_ERR(curr_scp_idx)		= TRUE;
      SCP_ATTR_IDX(curr_scp_idx)	= attr_idx;
   }
   else {
      SCP_ATTR_IDX(curr_scp_idx)	= attr_idx;

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
          ATD_CLASS(attr_idx) == Variable &&
          ATD_SYMBOLIC_CONSTANT(attr_idx)) {
      }
      else {
#ifdef KEY /* Bug 8261 */
         PRINTMSG(TOKEN_LINE(*token), 180, Internal, TOKEN_COLUMN(*token),
                  TOKEN_STR(*token), "attr_tbl");
#else /* KEY Bug 8261 */
         PRINTMSG(TOKEN_LINE(token), 180, Internal, TOKEN_COLUMN(token),
                  TOKEN_STR(token), "attr_tbl");
#endif /* KEY Bug 8261 */
      }
   }

   LN_DEF_LOC(name_idx)		= TRUE;
   AT_OBJ_CLASS(attr_idx)	= Pgm_Unit;
   ATP_PGM_UNIT(attr_idx)	= pgm_type;			      
   ATP_HAS_TASK_DIRS(attr_idx)  = has_task_dirs;

#ifdef KEY /* Bug 5089 */
   /* F2003 intrinsic module needs to avoid collision with like-named
    * nonintrinsic (user-created) module when generating linker symbols */
   if (on_off_flags.intrinsic_module_gen &&
     Pgm_Unit == AT_OBJ_CLASS(attr_idx) && Module == ATP_PGM_UNIT(attr_idx)) {
     AT_IS_INTRIN(attr_idx) = TRUE;
   }
#endif /* KEY Bug 5089 */

   MAKE_EXTERNAL_NAME(attr_idx, AT_NAME_IDX(attr_idx), AT_NAME_LEN(attr_idx));

   ATP_SCP_ALIVE(attr_idx)	= TRUE;
   ATP_EXPL_ITRFC(attr_idx)     = TRUE;
   ATP_SCP_IDX(attr_idx)	= curr_scp_idx;

   if (cif_flags  &&  pgm_type == Program) {
      AT_CIF_SYMBOL_ID(attr_idx) = 2;       /* Reserved for name of main pgm. */
   }

   ATP_MAY_INLINE(attr_idx)	= opt_flags.modinline || 
				  (pgm_type != Module && dump_flags.preinline);
   if (pgm_type <= Program) {
      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)		= Entry_Opr; 
      IR_TYPE_IDX(ir_idx)       = TYPELESS_DEFAULT_TYPE;
#ifdef KEY /* Bug 8261 */
      IR_LINE_NUM(ir_idx)	= TOKEN_LINE(*token);
      IR_COL_NUM(ir_idx)	= TOKEN_COLUMN(*token);
#else /* KEY Bug 8261 */
      IR_LINE_NUM(ir_idx)	= TOKEN_LINE(token);
      IR_COL_NUM(ir_idx)	= TOKEN_COLUMN(token);
#endif /* KEY Bug 8261 */
      IR_FLD_L(ir_idx)		= AT_Tbl_Idx;
      IR_IDX_L(ir_idx)		= attr_idx;
#ifdef KEY /* Bug 8261 */
      IR_COL_NUM_L(ir_idx)	= TOKEN_COLUMN(*token);
      IR_LINE_NUM_L(ir_idx)	= TOKEN_LINE(*token);
#else /* KEY Bug 8261 */
      IR_COL_NUM_L(ir_idx)	= TOKEN_COLUMN(token);
      IR_LINE_NUM_L(ir_idx)	= TOKEN_LINE(token);
#endif /* KEY Bug 8261 */

      if (no_name_entry ) {

         /* Need to have a statement header for this MAIN entry, before    */
         /* the first statement header of the program.  curr_stmt_sh_idx   */
         /* may not be the first SH, because some statements generate more */
         /* than one SH, so save curr_stmt_sh_idx, set it to the first SH, */
         /* call gen_sh, and then restore curr_stmt_sh_idx.  If gen_sh     */
         /* called with the Before option ever messes with curr_stmt_sh_idx*/
         /* this could cause troubles.                                     */

         save_sh_idx		= curr_stmt_sh_idx;
         curr_stmt_sh_idx	= SCP_FIRST_SH_IDX(curr_scp_idx);

         gen_sh(Before,
                Program_Stmt,
                stmt_start_line,
                stmt_start_col,
                FALSE,               /* No Errors                    */
                FALSE,               /* Not labeled                  */
                TRUE);               /* Compiler generated statement */

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))    = ir_idx;
         curr_stmt_sh_idx            = SCP_FIRST_SH_IDX(curr_scp_idx);

         if (cmd_line_flags.debug_lvl <= Debug_Lvl_1) {  /* -ez -ed -G0 -G1  */
            gen_end_prologue_debug_label(attr_idx);
         }

         curr_stmt_sh_idx    = save_sh_idx;
      }
      else {
         SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

         if (cmd_line_flags.debug_lvl <= Debug_Lvl_1) {  /* -ez -ed  -G0 -G1 */
            gen_end_prologue_debug_label(attr_idx);
         }
      }
   }

   /* The @DATA block needs to be renamed to @DATA_in_PGM_UNIT_NAME.     */

   sb_idx		= SCP_SB_STATIC_IDX(curr_scp_idx);
   SB_NAME_IDX(sb_idx)	= make_in_parent_string(SB_NAME_IDX(sb_idx),
                                                SB_NAME_LEN(sb_idx),
                                                curr_scp_idx,
                                                &length);
   SB_NAME_LEN(sb_idx)	= length;

   if (sb_idx != SCP_SB_STATIC_INIT_IDX(curr_scp_idx)) {
      sb_idx               = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
      SB_NAME_IDX(sb_idx)  = make_in_parent_string(SB_NAME_IDX(sb_idx),
                                                   SB_NAME_LEN(sb_idx),
                                                   curr_scp_idx,
                                                   &length);
      SB_NAME_LEN(sb_idx)  = length;
   }

   if (sb_idx != SCP_SB_STATIC_UNINIT_IDX(curr_scp_idx)) {
      sb_idx               = SCP_SB_STATIC_UNINIT_IDX(curr_scp_idx);
      SB_NAME_IDX(sb_idx)  = make_in_parent_string(SB_NAME_IDX(sb_idx),
                                                   SB_NAME_LEN(sb_idx),
                                                   curr_scp_idx,
                                                   &length);
      SB_NAME_LEN(sb_idx)  = length;
   }

   CURR_BLK		= blk_type;
   CURR_BLK_NAME	= attr_idx;		           
   CURR_BLK_DEF_LINE	= stmt_start_line;
   CURR_BLK_DEF_COLUMN	= stmt_start_col;

   if (cif_flags & XREF_RECS) {
      cif_usage_rec(attr_idx,
                    AT_Tbl_Idx,
#ifdef KEY /* Bug 8261 */
                    TOKEN_LINE(*token),
                    TOKEN_COLUMN(*token),
#else /* KEY Bug 8261 */
                    TOKEN_LINE(token),
                    TOKEN_COLUMN(token),
#endif /* KEY Bug 8261 */
                    CIF_Symbol_Declaration);
   }

   if (!no_name_entry) {

      /* If this is an unnamed program unit, implicit_use_semantics */
      /* was called in parse_prog_unit in p_driver.                 */

      implicit_use_semantics();
   }

   TRACE (Func_Exit, "start_new_prog_unit", NULL);

   return(attr_idx);

}  /* start_new_prog_unit */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF     - [ prefix ] FUNCTION function-name                           *|
|*                                   ( [ dummy-arg-name-list ] )              *|
|*                                   [ RESULT ( result-name ) ]               *|
|*                                                                            *|
|*      prefix  -    type-spec [ RECURSIVE ]                                  *|
|*                or RECURSIVE [ type-spec ]				      *|
|*									      *|
|*      At entry, if AT_TYPED in AT_WORK_IDX is FALSE, the type needs to be  *|
|*                parsed and we need to do error checks for the keyword       *|
|*                FUNCTION.  If AT_TYPED in TRUE, the type is in AT_WORK_IDX *|
|*                and LA_CH is set to pick up the ID.  The FUNCTION keyword   *|
|*                has been verified.                                          *|
|*      Must be in correct context when this routine is called.               *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
void parse_typed_function_stmt()

{
   boolean	assumed_size_ch	= FALSE;
   int		attr_idx;
   int		defer_msg;
   boolean	elemental_set;
   boolean	err_fnd		= FALSE;
   char		err_str[45];
   int		idx;
   int		local_scp_idx	= curr_scp_idx;
   boolean	matched;
   int		interface_idx;
   boolean	pure_set;
   boolean	recursive_set;
   int		rslt_idx;
   int		stmt_number;
   boolean	type_err;
#ifdef KEY /* Bug 8261 */
   token_type   saved_id;
   boolean      use_saved_id = FALSE;
#endif /* KEY Bug 8261 */

   TRACE (Func_Entry, "parse_typed_function_stmt", NULL);

   stmt_type				= Function_Stmt;
   SH_STMT_TYPE(curr_stmt_sh_idx)	= Function_Stmt;
   stmt_number				= statement_number;

   if (curr_stmt_category == Sub_Func_Stmt_Cat) {

      /* Save the starting indexes of all the tables, to be used when         */
      /* collapsing the interface stuff back into the parent scope.           */


      if (CURR_BLK == Interface_Blk) {
         interface_idx	= CURR_BLK_NAME;

         if (interface_idx == NULL_IDX) {
            interface_idx = BLK_UNNAMED_INTERFACE(blk_stk_idx);
         }

         ATI_HAS_NON_MOD_PROC(interface_idx)       = TRUE;

         /* This assumes that the attr table is always initialized with */
         /* some kind of entries at start up.                           */

         if (BLK_AT_IDX(blk_stk_idx) == NULL_IDX) {
            BLK_AT_IDX(blk_stk_idx)	= attr_tbl_idx;
            BLK_BD_IDX(blk_stk_idx)	= bounds_tbl_idx;
            BLK_CN_IDX(blk_stk_idx)	= const_tbl_idx;
            BLK_CP_IDX(blk_stk_idx)	= const_pool_idx;
            BLK_NP_IDX(blk_stk_idx)	= name_pool_idx;
            BLK_SB_IDX(blk_stk_idx)	= stor_blk_tbl_idx;
            BLK_SN_IDX(blk_stk_idx)	= sec_name_tbl_idx;
            BLK_TYP_IDX(blk_stk_idx)	= type_tbl_idx;
         }
      }

      /* Create a scope for this contained routine, but leave curr_scp_idx    */
      /* still pointing to parent's scope.  After calling start_new_subpgm,   */
      /* curr_scp_idx will be set correctly.                                  */

      start_new_scp();
      curr_scp_idx	= SCP_PARENT_IDX(curr_scp_idx);
   }

   if (AT_OBJ_CLASS(AT_WORK_IDX) == Pgm_Unit) {

      /* Control came in from parse_prefix_spec.  Type has not been processed */
      /* yet.  Only recusive, pure and/or elemental have been seen.  Need to  */
      /* save these, becase parse_type_spec will clear AT_WORK_IDX.           */

      err_fnd		= AT_DCL_ERR(AT_WORK_IDX);
      recursive_set	= ATP_RECURSIVE(AT_WORK_IDX);
      elemental_set	= ATP_ELEMENTAL(AT_WORK_IDX);
      pure_set		= ATP_PURE(AT_WORK_IDX);
   }
   else {
      recursive_set	= FALSE;
      elemental_set	= FALSE;
      pure_set		= FALSE;
   }

   if (AT_TYPED(AT_WORK_IDX)) {
      type_err	= AT_DCL_ERR(AT_WORK_IDX);
   }
   else {

      /* Will always go through here for internal and module procedures.      */
      /* External procedures will have their type already.                    */
      /* parse_type_spec may use stmt_type to handle derived type host        */
      /* association.  stmt_type will always be Function_Stmt for this call.  */

      if (curr_stmt_category == Sub_Func_Stmt_Cat) {

         /* Set scope to new scope, so that type info goes into correct scope.*/
         /* Set SCP_ATTR_IDX so that messages are correct.                    */
         /* The current scope becomes the new scope created at entry to this  */
         /* routine.  It doesn't have an attribute yet, because we haven't    */
         /* parsed the name, so we set the name to the parent for now.        */

         curr_scp_idx		    =SCP_LAST_CHILD_IDX(curr_scp_idx);
         SCP_ATTR_IDX(curr_scp_idx) =SCP_ATTR_IDX(SCP_PARENT_IDX(curr_scp_idx));
      }

      type_err	= !parse_type_spec(TRUE);  /* TRUE - Check for kind type. */
      err_fnd	= type_err;

      if (curr_stmt_category == Sub_Func_Stmt_Cat) {

         /* Set scope back to parent, for call to start_new_subpgm */

         local_scp_idx	= curr_scp_idx;
         curr_scp_idx	= SCP_PARENT_IDX(curr_scp_idx);
      }
   }

   while (matched = MATCHED_TOKEN_CLASS(Tok_Class_Keyword)) {

      switch (TOKEN_VALUE(token)) {
      case Tok_Kwd_Recursive:

         if (elemental_set) {

            /* RECURSIVE and ELEMENTAL should not be set for same subprogram */

            PRINTMSG(TOKEN_LINE(token), 1261, Error, TOKEN_COLUMN(token));
            err_fnd	= TRUE;
         }
         else if (recursive_set) {  /* Duplicate declaration */
            PRINTMSG(TOKEN_LINE(token), 1260, Error, TOKEN_COLUMN(token),
                     "RECURSIVE");
            err_fnd	= TRUE;
         }
         else {
            recursive_set	= TRUE;
         }
         continue;

      case Tok_Kwd_Elemental:

         if (recursive_set) {

            /* RECURSIVE and ELEMENTAL should not be set for same subprogram */

            PRINTMSG(TOKEN_LINE(token), 1261, Error, TOKEN_COLUMN(token));
            err_fnd	= TRUE;
         }
         else if (elemental_set) {  /* Duplicate declaration */
            PRINTMSG(TOKEN_LINE(token), 1260, Error, TOKEN_COLUMN(token),
                     "ELEMENTAL");
            err_fnd	= TRUE;
         }
         else {
            elemental_set	= TRUE;
         }
         continue;

      case Tok_Kwd_Pure:

         if (pure_set) {  /* Duplicate declaration */
            PRINTMSG(TOKEN_LINE(token), 1260, Error, TOKEN_COLUMN(token),
                     "PURE");
            err_fnd	= TRUE;
         }
         pure_set	= TRUE;
         continue;

      case Tok_Kwd_Function:

         if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
            parse_err_flush(Find_Lparen, "function-name");
            token			= main_token;
            TOKEN_LINE(token)		= stmt_start_line;
            TOKEN_COLUMN(token)		= stmt_start_col;
            err_fnd			= TRUE;
         }
#ifdef KEY /* Bug 8261 */
	 /* Extension allows *ddd after the function name, e.g.
	  * "integer function i*ddd()" */
         else if (STAR == LA_CH_VALUE) {
	   saved_id = token;
	   use_saved_id = TRUE;
	   int type_idx = ATD_TYPE_IDX(AT_WORK_IDX);
	   if (Character != TYP_TYPE(type_idx) &&
	      !on_off_flags.issue_ansi_messages) {
	      NEXT_LA_CH;
	      ATD_TYPE_IDX(AT_WORK_IDX) = parse_non_char_kind_selector(FALSE);
	   }
	   else {
	     parse_length_selector(AT_WORK_IDX, FALSE, TRUE);
	     TYP_DESC(TYP_WORK_IDX) = TYP_DESC(type_idx);
	     TYP_DCL_VALUE(TYP_WORK_IDX) = TYP_DCL_VALUE(type_idx);
	     ATD_TYPE_IDX(AT_WORK_IDX) = ntr_type_tbl();
	   }
	 }
#endif /* KEY Bug 8261 */
         break;

      default:
         matched = FALSE;
         reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
         break;
      }
      break;
   }

   if (!matched) {
      err_str[0]	= '\0';

      if (!recursive_set) {
         strcat(err_str, "[RECURSIVE] ");
      }
      if (!elemental_set) {
         strcat(err_str, "[ELEMENTAL] ");
      }
      if (!pure_set) {
         strcat(err_str, "[PURE] ");
      }

      strcat(err_str, "FUNCTION");

      parse_err_flush(Find_EOS, err_str);
      token			= main_token;
      TOKEN_LINE(token)		= stmt_start_line;
      TOKEN_COLUMN(token)	= stmt_start_col;
      err_fnd			= TRUE;
   }

   if (TYP_TYPE(ATD_TYPE_IDX(AT_WORK_IDX)) == Character) {

      if (TYP_CHAR_CLASS(ATD_TYPE_IDX(AT_WORK_IDX)) == Assumed_Size_Char) {
         assumed_size_ch = TRUE;
      }
      else if (TYP_CHAR_CLASS(ATD_TYPE_IDX(AT_WORK_IDX)) == Var_Len_Char ||
               TYP_CHAR_CLASS(ATD_TYPE_IDX(AT_WORK_IDX)) == Unknown_Char) {
      
         /* This is a variable length character.  Go thru the symbol table  */
         /* and mark everything in the local scope as Char_Rslt_Bound_Ref.  */

         for (idx = SCP_LN_FW_IDX(local_scp_idx); 
              idx < SCP_LN_LW_IDX(local_scp_idx); idx++) {
             AT_REFERENCED(LN_ATTR_IDX(idx)) = Char_Rslt_Bound_Ref;
         }
      }
   }

   if (curr_stmt_category != Sub_Func_Stmt_Cat) {
      defer_msg			= 0;
#ifdef KEY /* Bug 8261 */
      attr_idx= start_new_prog_unit_by_token(Function, Function_Blk, FALSE,
	err_fnd, &defer_msg, (use_saved_id ? (&saved_id) : (&token)));
#else /* KEY Bug 8261 */
      attr_idx			= start_new_prog_unit(Function,
                                                      Function_Blk,
                                                      FALSE,
                                                      err_fnd,
                                                      &defer_msg);
#endif /* KEY Bug 8422 */
      ATP_PROC(attr_idx)	= Extern_Proc;
   }
   else {
      attr_idx			= start_new_subpgm(Function, err_fnd, FALSE);
   }

   if (assumed_size_ch) {  /* Obsolescent */
      PRINTMSG(AT_DEF_LINE(attr_idx), 1565,
#ifdef KEY /* Bug 318, 321 */
	       Ansi,
#else /* KEY Bug 318, 321 */
	       Comment,
#endif /* KEY Bug 318, 321 */
               AT_DEF_COLUMN(attr_idx));

      if (ATP_PROC(attr_idx) == Intern_Proc ||
         ATP_PROC(attr_idx) == Module_Proc) {

         /* An internal or module procedure cannot be assumed size char. */
         /* Allow it to be character for error recovery.                 */

         PRINTMSG(AT_DEF_LINE(attr_idx), 367, Error,
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx));
         AT_DCL_ERR(attr_idx)		= TRUE;
         ATD_TYPE_IDX(AT_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
      }
      else if (CURR_BLK == Interface_Body_Blk) {

         /* An interface block may be typed as assumed size character */
         /* but it cannot be invoked.                                 */

         PRINTMSG(AT_DEF_LINE(attr_idx), 1566, Warning,
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx));
      }
      else if (recursive_set) { /* Recursive is not allowed to be assumed size*/
                                /* char.  Allow it to be char for err recovery*/
         PRINTMSG(AT_DEF_LINE(attr_idx), 506, Error,
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx));
         AT_DCL_ERR(attr_idx)		= TRUE;
         ATD_TYPE_IDX(AT_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
      }
   }

   if ((cif_flags & MISC_RECS)  &&  ! err_fnd) {
      cif_stmt_type_rec(TRUE, CIF_Function_Stmt, stmt_number);
   }

   SCP_IN_ERR(curr_scp_idx)			= AT_DCL_ERR(attr_idx);
   SCP_IN_ERR(SCP_PARENT_IDX(curr_scp_idx))	= AT_DCL_ERR(attr_idx);
   CURR_BLK_ERR					= AT_DCL_ERR(attr_idx);
   ATP_RECURSIVE(attr_idx)			= recursive_set;
   ATP_ELEMENTAL(attr_idx)			= elemental_set;
   ATP_PURE(attr_idx)				= pure_set;

   if (CURR_BLK != Interface_Body_Blk &&
       (cmd_line_flags.runtime_argument ||
        cmd_line_flags.runtime_arg_entry)) {

      ATP_ARGCHCK_ENTRY(attr_idx) = TRUE;
   }

   /* If there was no FUNCTION keyword - careful on further messages. */

   if (LA_CH_VALUE == LPAREN || (!err_fnd &&
                                  parse_err_flush(Find_Lparen, "(") )) {
      parse_dummy_args(attr_idx);
   }

   set_function_rslt(attr_idx, type_err);

   rslt_idx			= ATP_RSLT_IDX(attr_idx);
   AT_TYPED(rslt_idx)		= TRUE;
   ATD_TYPE_IDX(rslt_idx)	= ATD_TYPE_IDX(AT_WORK_IDX);

#ifdef KEY
// Bug 2164
   if (AT_OBJ_CLASS(rslt_idx) == Data_Obj && !AT_IS_INTRIN(rslt_idx) &&
       TYP_LINEAR(ATD_TYPE_IDX(rslt_idx)) == Real_4 &&
       Check_FF2C_Script(AT_OBJ_NAME_PTR(rslt_idx), 0) )
   {
     ATD_TYPE_IDX(rslt_idx) = Real_8;
   }
#endif
   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_typed_function_stmt", NULL);

   return;

}  /* parse_typed_function_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine starts a new scope.  New scopes are started whenever a   *|
|*      FUNCTION or SUBROUTINE is found, that is inside a contains or an      *|
|*      interface block.  The interface statement does NOT cause a new scope  *|
|*      scope to start.  This routine links the new scope into the scope      *|
|*      table and the sibling/parent lists.  It also sets the implicit table  *|
|*      for the new scope.  The statement header list is updated.  The        *|
|*      parent's last statement header next index is set to NULL, so that     *|
|*      this new scopes IR is not linked to the old scopes IR.   It is        *|
|*      called by an internal, interface, or contains FUNCTION or SUBROUTINE. *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void start_new_scp(void)

{
   int    	first_sh_idx;
   int		idx;
   int		name_idx;
   int		npes_attr;
   token_type	npes_token;
   int		parent_idx;
   int		parent_name_idx;
   int		save_scp;


   TRACE (Func_Entry, "start_new_scp", NULL);

   parent_idx				= curr_scp_idx;
   NTR_SCP_TBL(curr_scp_idx);

   /* Insert new scope at the end of the parent's child list */

   if (SCP_FIRST_CHILD_IDX(parent_idx) == NULL_IDX) {
      SCP_FIRST_CHILD_IDX(parent_idx) = curr_scp_idx;
   }
   else {
      SCP_SIBLING_IDX(SCP_LAST_CHILD_IDX(parent_idx)) = curr_scp_idx;
   }

   SCP_LAST_CHILD_IDX(parent_idx)       = curr_scp_idx;
   SCP_NUM_CHILDREN(parent_idx)		= SCP_NUM_CHILDREN(parent_idx) + 1;
   SCP_PARENT_IDX(curr_scp_idx)		= parent_idx;
   SCP_LEVEL(curr_scp_idx)		= SCP_LEVEL(parent_idx) + 1;
   SCP_IMPL_NONE(curr_scp_idx)		= FALSE;

   /* If this statement is labeled - it has 2 statement headers, so need to   */
   /* back up to the first statement header.   If the statement is not        */
   /* labeled, curr_stmt_sh_idx points to the first statement header for this */
   /* statement.                                                              */

   first_sh_idx = SH_LABELED(curr_stmt_sh_idx) ? SH_PREV_IDX(curr_stmt_sh_idx) :
                                                 curr_stmt_sh_idx;

   SCP_FIRST_SH_IDX(curr_scp_idx)	= first_sh_idx;

   /* Break the linkage between the previous Statement Header (of the host   */
   /* scoping unit) and the current subprogram Statement Header.             */

   SCP_LAST_SH_IDX(parent_idx)			= SH_PREV_IDX(first_sh_idx);
   SH_PREV_IDX(first_sh_idx)			= NULL_IDX;
   SH_NEXT_IDX(SH_PREV_IDX(first_sh_idx))	= NULL_IDX;

   /* Reset ln and sb first and last words.  TRUE -> Also, do storage table  */

   init_name_and_stor_tbls(curr_scp_idx, TRUE);  

   if (CURR_BLK == Interface_Blk) {
      SCP_PARENT_NONE(curr_scp_idx)	= FALSE;
      SCP_IS_INTERFACE(curr_scp_idx)	= TRUE;

      /* Initialize to reflect the default table setting, not the parent's */

      for (idx = 0; idx < MAX_IMPL_CHS; idx++) {
         IM_TYPE_IDX(curr_scp_idx, idx)	= REAL_DEFAULT_TYPE;
         IM_SET(curr_scp_idx, idx)	= FALSE;
      }

      for (idx = IMPL_IDX('I'); idx <= IMPL_IDX('N'); idx++) {
         IM_TYPE_IDX(curr_scp_idx, idx)	= INTEGER_DEFAULT_TYPE;
      }

      /* After an interface block is processed, the only thing left on the  */
      /* stack should be compiler tmps and the function result.  These need */
      /* to go on the parent's stack, so by setting the parent's stack as   */
      /* the default stack, they get there by default.  Compression of      */
      /* interface blocks will remove things put on the stack that are not  */
      /* needed.                                                            */

      SCP_SB_STACK_IDX(curr_scp_idx)	= SCP_SB_STACK_IDX(parent_idx);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

      /* Set the scope for the darg block to the parent scope. */

      SB_SCP_IDX(SCP_SB_DARG_IDX(curr_scp_idx)) =
                     SB_SCP_IDX(SCP_SB_DARG_IDX(parent_idx));
# endif

   }
   else { /* Use parent's implicit settings - accumulate IMPLICIT NONE's */
      SCP_PARENT_NONE(curr_scp_idx)		= SCP_IMPL_NONE(parent_idx) ||
                                                  SCP_PARENT_NONE(parent_idx);

      for (idx = 0; idx < MAX_IMPL_CHS; idx++) {
      /* IM_SET(curr_scp_idx, idx)		= FALSE;  ntr_scp_tbl clears */
         IM_TYPE_IDX(curr_scp_idx, idx) 	= IM_TYPE_IDX(parent_idx, idx);
      }
   }

   /* Add N$PES to the new scope. */

   CREATE_ID(TOKEN_ID(npes_token), "N$PES", 5);

   TOKEN_COLUMN(npes_token)	= 1;
   TOKEN_LEN(npes_token)	= 5;
   TOKEN_LINE(npes_token)	= stmt_start_line;
   npes_attr			= srch_sym_tbl(TOKEN_STR(npes_token),
                                               TOKEN_LEN(npes_token),
                                               &name_idx);
   npes_attr			= ntr_sym_tbl(&npes_token,name_idx);
   LN_DEF_LOC(name_idx)		= TRUE;
   save_scp			= curr_scp_idx;
   curr_scp_idx			= parent_idx;
   npes_attr			= srch_sym_tbl(TOKEN_STR(npes_token),
                                               TOKEN_LEN(npes_token),
                                               &parent_name_idx);

   /* Assumption is that we will always find it. */

   LN_ATTR_IDX(name_idx)	= npes_attr;
   LN_NAME_IDX(name_idx)	= AT_NAME_IDX(npes_attr);
   curr_scp_idx			= save_scp;

   TRACE (Func_Exit, "start_new_scp", NULL);

   return;

}  /* start_new_scp */

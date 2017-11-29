/*
 * Copyright (C) 2007, 2008. PathScale, LLC. All Rights Reserved.
 */
/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/p_end.c	5.7	09/01/99 09:11:00\n";

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
# include "p_end.h"


/*****************************************************************\
|* Function prototypes of static functions declared in this file *|
\*****************************************************************/

/* The prototypes for the end_.. routines are in p_end.h.  They are needed */
/* there so that the end routine calling table can be initialized.         */

static char	*blk_desc_str(int);
static boolean	 end_task_do_blk(void);
static void	 finish_cdir_id(void);
static void	 loop_end_processing(void);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
static void      check_loop_bottom_nesting(void);
# endif


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      If there were CDIR ID directives in the program unit, terminate the   *|
|*      USMID string with a null character.                                   *|
|*                                                                            *|
|*      Here's how this works.  Each !DIR$ ID line is assigned its own        *|
|*      compiler temp.  The size of the temp is the number or chars in the    *|
|*      string.  There is no null character at the end of the string.  For    *|
|*      instance if !DIR$ ID="hello", at temp is created of type CHAR*5.      *|
|*      This routine is called after all !DIR$ ID's have been parsed (at the  *|
|*      END statement.  Another temp is created whose length is at least one  *|
|*      character with the size such that the whole block is word aligned.    *|
|*      All the temps are in the same storage block and their offsets are     *|
|*      such that all the temps are concatenated together with the NULL at    *|
|*      the end of the last temp.                                             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
 
static void finish_cdir_id(void)
{

   size_offset_type	aligned_length;
   int                  column;
   int                  init_idx;
   size_offset_type	length;
   int                  line;
   int                  list_idx;
   id_str_type		name;
   int                  new_str_idx;
   opnd_type            opnd;
   int                  prog_unit_has_id_line;
   int			tmp_idx;
   int                  type_idx;


   TRACE (Func_Entry, "finish_cdir_id", NULL);
   
   CREATE_ID(name, sb_name[What_Blk], sb_len[What_Blk]);
   
   prog_unit_has_id_line = srch_stor_blk_tbl(name.string,
                                             sb_len[What_Blk],
                                             curr_scp_idx);

   if (prog_unit_has_id_line != NULL_IDX) {

      /* Must have an ID string.  Get a temp, initialize to null characters,  */
      /* then put into named static block.				      */

      line                            = curr_glb_line;
      column                          = 0;
      tmp_idx                    = gen_compiler_tmp(line, column, Shared, TRUE);
      ATD_STOR_BLK_IDX(tmp_idx)       = prog_unit_has_id_line;
      ATD_TMP_SEMANTICS_DONE(tmp_idx) = TRUE;
      ATD_OFFSET_ASSIGNED(tmp_idx)    = TRUE;

# if defined(_DEBUG)

      /* Must be a constant length. */

      if (SB_LEN_FLD(ATD_STOR_BLK_IDX(tmp_idx)) != CN_Tbl_Idx) {
         PRINTMSG(line, 1201, Internal, column, 
                  SB_NAME_PTR(ATD_STOR_BLK_IDX(tmp_idx)));
      }
# endif

      length.idx		= SB_LEN_IDX(ATD_STOR_BLK_IDX(tmp_idx));
      length.fld		= SB_LEN_FLD(ATD_STOR_BLK_IDX(tmp_idx));
      ATD_OFFSET_IDX(tmp_idx)	= length.idx;
      ATD_OFFSET_FLD(tmp_idx)	= length.fld;
      aligned_length.idx	= CN_INTEGER_CHAR_BIT_IDX;
      aligned_length.fld	= CN_Tbl_Idx;

      if (!size_offset_binary_calc(&length, &aligned_length, Plus_Opr, 
                                   &aligned_length)) {
         AT_DCL_ERR(tmp_idx)	= TRUE;
      }

      align_bit_length(&aligned_length, TARGET_BITS_PER_WORD);

      if (!size_offset_binary_calc(&aligned_length,&length,Minus_Opr,&length)) {
         AT_DCL_ERR(tmp_idx)	= TRUE;
      }

      if (aligned_length.fld == NO_Tbl_Idx) {
         aligned_length.fld	= CN_Tbl_Idx;
         aligned_length.idx	= ntr_const_tbl(aligned_length.type_idx,
                                                FALSE,
                                                aligned_length.constant);
      }

      SB_LEN_FLD(ATD_STOR_BLK_IDX(tmp_idx)) = aligned_length.fld;
      SB_LEN_IDX(ATD_STOR_BLK_IDX(tmp_idx)) = aligned_length.idx;

      /* We now have the length in bits of the null temp in length.      */
      /* This needs to be translated to # of chars for character length. */

      aligned_length.fld	= CN_Tbl_Idx;
      aligned_length.idx	= CN_INTEGER_CHAR_BIT_IDX;

      if (!size_offset_binary_calc(&length, &aligned_length, Div_Opr, &length)){
         AT_DCL_ERR(tmp_idx)	= TRUE;
      }

      if (length.fld == NO_Tbl_Idx) {
         length.fld	= CN_Tbl_Idx;
         length.idx	= ntr_const_tbl(length.type_idx,
                                        FALSE,
                                        length.constant);
      }

      OPND_FLD(opnd)               = AT_Tbl_Idx;
      OPND_IDX(opnd)               = tmp_idx;
      OPND_LINE_NUM(opnd)          = line;
      OPND_COL_NUM(opnd)           = column;

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)       = Character;
      TYP_LINEAR(TYP_WORK_IDX)     = CHARACTER_DEFAULT_TYPE;
      TYP_DESC(TYP_WORK_IDX)       = Default_Typed;
      TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
      TYP_FLD(TYP_WORK_IDX)        = CN_Tbl_Idx;
      TYP_IDX(TYP_WORK_IDX)        = length.idx;
      type_idx                     = ntr_type_tbl();

      /* Call ntr_const_tbl with NULL for the incoming constant so it         */
      /* will just allocate the amount of space needed.   ntr_const_tbl does  */
      /* zero out the memory.                                                 */

      new_str_idx = ntr_const_tbl(type_idx, TRUE, NULL);

      ATD_TYPE_IDX(tmp_idx) = CN_TYPE_IDX(new_str_idx);

      gen_whole_substring(&opnd, 0);

      /* Initialize the temp.  						      */

      NTR_IR_TBL(init_idx);
      IR_OPR(init_idx) = Init_Opr;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      IR_OPR(init_idx) = Null_Opr;
# endif

      /* Must have a type idx.						      */

      IR_TYPE_IDX(init_idx)	= ATD_TYPE_IDX(tmp_idx);
      IR_LINE_NUM(init_idx)	= line;
      IR_COL_NUM(init_idx)	= column;
      IR_LINE_NUM_R(init_idx)	= line;
      IR_COL_NUM_R(init_idx)	= column;
      COPY_OPND(IR_OPND_L(init_idx), opnd);

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(init_idx)	= IL_Tbl_Idx;
      IR_IDX_R(init_idx)	= list_idx;
      IR_LIST_CNT_R(init_idx)	= 3;
      IL_IDX(IR_IDX_R(init_idx))= new_str_idx;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx))	= list_idx;
      IL_FLD(list_idx)					= CN_Tbl_Idx;
      IL_IDX(list_idx)					= new_str_idx;
      IL_LINE_NUM(list_idx)				= line;
      IL_COL_NUM(list_idx)				= column;

      list_idx			= IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx)		= CN_Tbl_Idx;
      IL_IDX(list_idx)		= CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list_idx)	= line;
      IL_COL_NUM(list_idx)	= column;
         
      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx					   = IL_NEXT_LIST_IDX(list_idx);
 
      IL_FLD(list_idx)		= CN_Tbl_Idx;
      IL_IDX(list_idx)		= CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx)	= line;
      IL_COL_NUM(list_idx)	= column;
         
      gen_sh(Before, Assignment_Stmt, line, column,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = init_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      
   }

   TRACE (Func_Exit, "finish_cdir_id", NULL);

} /* finish_cdir_id */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse the END statement, find out what kind of end it is, check for   *|
|*      block errors, and call the particular end processing routine.         *|
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

void parse_end_stmt (void)

{
   int			blk_idx;
   int			buf_idx;
   boolean		found_name;
   token_values_type	keyword;
   boolean		match_name	= TRUE;
   boolean              msg_issued;
   int			stmt_num;


   TRACE (Func_Entry, "parse_end_stmt", NULL);

   end_of_contains = FALSE;

   if (LA_CH_VALUE == EOS) {

      /* If the END stmt is the action-stmt of a logical IF, don't parse it.  */

      if (if_stmt_lbl_idx != NULL_IDX) {
         NEXT_LA_CH;
         goto EXIT;
      }
         
      check_for_vestigial_task_blks();

      if (CURR_BLK == Contains_Blk) {
         end_contains(FALSE);
      }

      if (stmt_label_idx != NULL_IDX) {
         gen_attr_and_IR_for_lbl(FALSE);
      }

      blk_idx = blk_stk_idx;

      if (CURR_BLK > Interface_Body_Blk) {

         /* Search for the nearest program unit block if it's just an END     */
         /* stmt or if the current block was a popped CONTAINS block.  If     */
         /* there is a parse error, then just pop the current block.  Have    */
         /* end_of_contains check, because if a CONTAINS block is popped,     */
         /* then need to pop a program unit block.                            */

# ifdef _DEBUG
         if (blk_stk_idx == NULL_IDX) {
            PRINTMSG(stmt_start_line, 160, Internal, stmt_start_col, NULL_IDX);
         }
# endif

         if (stmt_label_idx == NULL_IDX) {

            for (blk_idx = blk_stk_idx;
                 BLK_TYPE(blk_idx) > Interface_Body_Blk;
                 blk_idx--);
         }
         else {

            /* If this END statement is the termination statement for a DO    */
            /* loop, issue an error message and pop the DO block.             */

            msg_issued = FALSE;
            blk_idx    = blk_stk_idx;

            while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {

               if (stmt_label_idx == BLK_LABEL(blk_idx)) { 

                  if (! msg_issued) {
                     PRINTMSG(stmt_start_line, 244, Error, stmt_start_col,
                              stmt_type_str[stmt_type]);
                     msg_issued = TRUE;
                  }

                  if (blk_idx != blk_stk_idx) {
                     pop_and_err_blk_stk(blk_idx, FALSE);
                  }

                  move_blk_to_end(blk_idx);

                  POP_BLK_STK;
               }
               --blk_idx;
            }
         }
         blk_idx = blk_match_err(BLK_TYPE(blk_idx), FALSE, FALSE);
      }
   }
   else if (MATCHED_TOKEN_CLASS (Tok_Class_Keyword)) {
      keyword	= TOKEN_VALUE(token);
      buf_idx   = TOKEN_BUF_IDX(token);
      stmt_num  = TOKEN_STMT_NUM(token);

      check_for_vestigial_task_blks();

      if (keyword == Tok_Kwd_File) {
         stmt_type			= Endfile_Stmt;
         SH_STMT_TYPE(curr_stmt_sh_idx)	= Endfile_Stmt;

         if (stmt_label_idx != NULL_IDX) {
            gen_attr_and_IR_for_lbl(FALSE);
         }

         parse_endfile_stmt();
         goto EXIT;
      }
    
      /* If the END stmt is the action-stmt of a logical IF, don't parse it.  */

      if (if_stmt_lbl_idx != NULL_IDX) {
         parse_err_flush(Find_EOS, NULL);
         NEXT_LA_CH;
         goto EXIT;
      }
         
      if (keyword == Tok_Kwd_Block && 
          !matched_specific_token(Tok_Kwd_Data, Tok_Class_Keyword)) {
         reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
         keyword = Tok_Id;  /* Force to default clause for error */
      }

#ifdef KEY /* Bug 5550 */
      /* If we are ending an interface block which began with "operator(.x.)"
       * or "assignment(=)", the "end interface" may have an optional
       * "operator(.x.)" or "assignment(=)" too. Notice that there's no
       * confusion between "interface x" and "interface operator(.x.)" here
       * because the former generates upper case token "X" but the latter
       * generates lower case token "x". */
      int blk_name = BLK_NAME(blk_stk_idx);
      if (AT_OBJ_CLASS(blk_name) == Interface)
      {
	found_name = (LA_CH_VALUE != EOS) && parse_generic_spec();
      }
      else
#endif /* KEY Bug 5550 */

      found_name = MATCHED_TOKEN_CLASS(Tok_Class_Id);

      if (CURR_BLK == Contains_Blk && 
          (keyword == Tok_Kwd_Module   || keyword == Tok_Kwd_Program    ||
           keyword == Tok_Kwd_Function || keyword == Tok_Kwd_Subroutine ||
           keyword == Tok_Kwd_Block)) {

         /* Because a contains may happen in a BLOCKDATA unit (an error is    */
         /* issued), it must be popped off at the end of the blockdata.       */

         end_contains(FALSE);
      }

      if (found_name) {
         match_name =  (CURR_BLK_NAME != NULL_IDX) ?
                              (compare_names(TOKEN_ID(token).words,
                                             TOKEN_LEN(token),
                                             AT_OBJ_NAME_LONG(CURR_BLK_NAME),
                                             AT_NAME_LEN(CURR_BLK_NAME)) == 0) :
                              FALSE;
      }

      if (stmt_label_idx != NULL_IDX  &&  keyword != Tok_Kwd_Type) {
         gen_attr_and_IR_for_lbl(FALSE);
      } 

      blk_idx = blk_stk_idx;

      switch (keyword) {

	 case Tok_Kwd_Block:
            stmt_type			    = End_Blockdata_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)  = End_Blockdata_Stmt;
            SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;


            /* Issue any deferred src_input messages.                         */

            issue_deferred_msgs();


            if (CURR_BLK != Blockdata_Blk || !match_name) {
               blk_idx = blk_match_err(Blockdata_Blk, found_name, FALSE);

               if (CURR_BLK != Blockdata_Blk) {
                  SCP_IN_ERR(curr_scp_idx)	= TRUE;
               }
            }

	    break;


	 case Tok_Kwd_Module:
            stmt_type			    = End_Module_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)  = End_Module_Stmt;
            SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;


            /* Issue any deferred src_input messages.                         */

            issue_deferred_msgs();


            if (CURR_BLK != Module_Blk || !match_name) {
               blk_idx = blk_match_err(Module_Blk, found_name, FALSE);

               if (CURR_BLK != Module_Blk) {
                  SCP_IN_ERR(curr_scp_idx)	= TRUE;
               }
            }

	    break;


	 case Tok_Kwd_Program:
            stmt_type			    = End_Program_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)  = End_Program_Stmt;

            if (stmt_label_idx != NULL_IDX) {
               end_labeled_do();
               blk_idx = blk_stk_idx;	/* end_labeled_do may have popped stk.*/
            }


            /* Issue any deferred src_input messages.                         */

            issue_deferred_msgs();


            if (CURR_BLK != Program_Blk || !match_name) {
               blk_idx = blk_match_err(Program_Blk, found_name, FALSE);

               if (CURR_BLK != Program_Blk) {
                  SCP_IN_ERR(curr_scp_idx)	= TRUE;
               }
            }

	    break;


	 case Tok_Kwd_Subroutine:
            stmt_type			    = End_Subroutine_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)  = End_Subroutine_Stmt;

            if (stmt_label_idx != NULL_IDX) {
               end_labeled_do();
               blk_idx = blk_stk_idx;	/* end_labeled_do may have popped stk.*/
            }


            /* Issue any deferred src_input messages.                         */

            issue_deferred_msgs();


            if (STMT_CANT_BE_IN_BLK(End_Subroutine_Stmt, CURR_BLK) ||
                !match_name || ATP_PGM_UNIT(CURR_BLK_NAME) != Subroutine) {
               blk_idx = blk_match_err(Subroutine_Blk, found_name, FALSE);
               SCP_IN_ERR(curr_scp_idx)	= TRUE;
            }

	    break;


	 case Tok_Kwd_Function:
            stmt_type			    = End_Function_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)  = End_Function_Stmt;

            if (stmt_label_idx != NULL_IDX) {
               end_labeled_do();
               blk_idx = blk_stk_idx;	/* end_labeled_do may have popped stk.*/
            }


            /* Issue any deferred src_input messages.                         */

            issue_deferred_msgs();


            if (STMT_CANT_BE_IN_BLK(End_Function_Stmt, CURR_BLK) ||
                !match_name || ATP_PGM_UNIT(CURR_BLK_NAME) != Function) {
               blk_idx = blk_match_err(Function_Blk, found_name, FALSE);
               SCP_IN_ERR(curr_scp_idx)	= TRUE;
            }

	    break;


	 case Tok_Kwd_Interface:

            stmt_type				= End_Interface_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)	= End_Interface_Stmt;

            if (CURR_BLK != Interface_Blk || !match_name) {
               blk_idx = blk_match_err(Interface_Blk, found_name, FALSE);
            }

            if (blk_idx != NULL_IDX) {
               curr_stmt_category = Declaration_Stmt_Cat;
            }

	    break;

#ifdef KEY /* Bug 10572 */
	 case Tok_Kwd_Enum:

            stmt_type				= End_Enum_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)	= End_Enum_Stmt;

            if (CURR_BLK != Enum_Blk || !match_name) {
               blk_idx = blk_match_err(Enum_Blk, found_name, FALSE);
            }

            if (blk_idx != NULL_IDX) {
               curr_stmt_category = Declaration_Stmt_Cat;
            }

	    break;
#endif /* KEY Bug 10572 */


	 case Tok_Kwd_Type:

            stmt_type				= End_Type_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)	= End_Type_Stmt;

            if (CURR_BLK != Derived_Type_Blk  ||  !match_name) {
               blk_idx = blk_match_err(Derived_Type_Blk, found_name, FALSE);
            }

            if (blk_idx != NULL_IDX) {
               curr_stmt_category = Declaration_Stmt_Cat;
            }

	    break;


	 case Tok_Kwd_If:
            stmt_type				= End_If_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)	= End_If_Stmt;


            /* The END IF stmt must have a construct name if the              */
            /* corresponding IF stmt has a construct name.  The names must    */
            /* be the same, of course.					      */

            if (CURR_BLK_NAME != NULL_IDX  &&  ! found_name) {
               match_name = FALSE;   
            }

            if (STMT_CANT_BE_IN_BLK(End_If_Stmt, CURR_BLK)  ||  ! match_name) {
               blk_idx = blk_match_err(If_Blk, found_name, TRUE);
            }

            if ((cif_flags & XREF_RECS)  &&  found_name  &&  match_name) {

               /* There are 3 cases to consider at this point:		      */
               /*   - The construct-name on the END IF matches the construct- */
               /*     name on the current IF construct.			      */
               /*   - The construct-name on the END IF matches the construct- */
               /*     name on some other IF construct.			      */
               /*   - No matching construct-name on an IF construct was found */
               /*     but the current block is an IF construct with a         */
               /*     construct-name.					      */
               /* The result of each of these cases is that the current Block */
               /* Stack frame has a nonnull BLK_NAME.  There is no way to tell*/
               /* whether or not the construct-name matches the END IF        */
               /* construct-name other than by comparing the strings.         */
               /* We also must ensure that CURR_BLK_NAME is not null (this    */
               /* happens if no match was found and the top Block Stack frame */
               /* has no name).						      */
               /* We can not call cif_usage_rec unless we find a matching     */
               /* name because an Attr only exists if the name is defined     */
               /* somewhere (and we need the Attr to hold the CIF symbol id). */

               cif_usage_rec(CURR_BLK_NAME,
                             AT_Tbl_Idx,
		             TOKEN_LINE(token),
                             TOKEN_COLUMN(token),
		   	     CIF_Construct_Name_Reference);
            }
            
	    break;


	 case Tok_Kwd_Do:
            stmt_type				= End_Do_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)	= End_Do_Stmt;


            /* The END DO stmt must have a construct name if the              */
            /* corresponding DO stmt has a construct name.  The names must    */
            /* be the same, of course.					      */

            if (CURR_BLK_NAME != NULL_IDX  &&   ! found_name) {
               match_name = FALSE;   
            }

            if (STMT_CANT_BE_IN_BLK(End_Do_Stmt, CURR_BLK) || !match_name) {
               blk_idx = blk_match_err(Do_Blk, found_name, TRUE);
            }

            if ((cif_flags & XREF_RECS) && found_name && match_name) {

               /* See the comment block in the END IF code above for the      */
               /* cases that must be considered.	   		      */

               cif_usage_rec(CURR_BLK_NAME,
                             AT_Tbl_Idx,
                             TOKEN_LINE(token),
                             TOKEN_COLUMN(token),
                             CIF_Construct_Name_Reference);
            }

	    break;


	 case Tok_Kwd_Select:
            stmt_type				= End_Select_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)	= End_Select_Stmt;


            /* The END SELECT stmt must have a construct name if the          */
            /* corresponding SELECT CASE stmt has a construct name.  The      */
            /* names must be the same, of course.			      */

            if (CURR_BLK_NAME != NULL_IDX  &&  ! found_name) {
               match_name = FALSE; 
            }

            if (STMT_CANT_BE_IN_BLK(End_Select_Stmt, CURR_BLK) || !match_name) {
               blk_idx = blk_match_err(Select_Blk, found_name, TRUE);
            }


            if ((cif_flags & XREF_RECS)  &&  found_name  &&  match_name) {

               /* See the comment block in the END IF code above for the      */
               /* cases that must be considered.			      */

               cif_usage_rec(CURR_BLK_NAME,
                             AT_Tbl_Idx,
		             TOKEN_LINE(token),
                             TOKEN_COLUMN(token),
		   	     CIF_Construct_Name_Reference);
            }

	    break;

	 case Tok_Kwd_Forall:

            stmt_type				= End_Forall_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)	= End_Forall_Stmt;

            /* The END FORALL stmt must have a construct name if the          */
            /* corresponding FORALL stmt has a construct name.  The names must*/
            /* be the same, of course.                                        */

            if (CURR_BLK_NAME != NULL_IDX  &&  ! found_name) {
               match_name = FALSE;
            }

            if (STMT_CANT_BE_IN_BLK(End_Forall_Stmt, CURR_BLK)  || !match_name){
               blk_idx = blk_match_err(Forall_Blk, found_name, TRUE);
            }

	    break;

	 case Tok_Kwd_Where:

            stmt_type				= End_Where_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)	= End_Where_Stmt;

            /* The END WHERE stmt must have a construct name if the           */
            /* corresponding WHERE stmt has a construct name.  The names must */
            /* be the same, of course.                                        */

            if (CURR_BLK_NAME != NULL_IDX  &&  ! found_name) {
               match_name = FALSE;
            }

            if (STMT_CANT_BE_IN_BLK(End_Where_Stmt, CURR_BLK) || !match_name) {
               blk_idx = blk_match_err(Where_Then_Blk, found_name, TRUE);
            }

	    break;


	 default:
            reset_lex(buf_idx, stmt_num);
            MATCHED_TOKEN_CLASS(Tok_Class_Id);

            /* Msg is expecting END, for whatever is the CURRENT block.  If   */
            /* the current block is a CONTAINS block, pop it and we'll assume */
            /* this is supposed to be a END PROGRAM, END MODULE, END FUNCTION */
            /* or END SUBROUTINE statement.  The SELECT CASE construct is     */
            /* weird because its END statement does not make use of both of   */
            /* the construct keywords SELECT and CASE.                        */

            if (CURR_BLK == Contains_Blk) {
               end_contains(FALSE);
            }

            PRINTMSG(TOKEN_LINE(token), 186, Error, TOKEN_COLUMN(token),
                     (CURR_BLK == Select_Blk) ? "SELECT" :
                        blk_desc_str(blk_stk_idx),
                     TOKEN_STR(token));

            /* Do not POP the block - but mark it in error - (like popping)   */

            CURR_BLK_ERR			= TRUE;
            SH_ERR_FLG(curr_stmt_sh_idx)	= TRUE;
            blk_idx				= NULL_IDX;
            parse_err_flush(Find_EOS, NULL);
	    break;

      }  /* switch */

      if (LA_CH_VALUE != EOS) {
         parse_err_flush(Find_EOS, EOS_STR);
      }

   }
   else {  /* Didn't find EOS and didn't find keyword. */
      PRINTMSG(LA_CH_LINE, 769, Error, LA_CH_COLUMN,
               (CURR_BLK == Select_Blk) ? "SELECT" : blk_desc_str(blk_stk_idx),
               LA_CH_VALUE);

      parse_err_flush(Find_EOS, NULL);

      /* Do not POP the block - but mark it in error - (like popping)   */

      if (if_stmt_lbl_idx == NULL_IDX) {
         CURR_BLK_ERR = TRUE;
      }

      blk_idx = NULL_IDX;
   }


   if (blk_idx != NULL_IDX) {   /* call the END routine for block */

      /* Finish up any CDIR$ ID strings.                                      */

      if ( (BLK_TYPE(blk_idx) >= Blockdata_Blk) && 
           (BLK_TYPE(blk_idx) <= Subroutine_Blk) ) {
         finish_cdir_id();
      }

      (*end_blocks[BLK_TYPE(blk_idx)]) (FALSE);
   }

   /* We should be sitting at EOS.  Check to see if this is a continued END   */
   /* statement.  If it is, issue an ANSI.  stmt_line_idx is from src_input   */

   if (LA_CH_VALUE == EOS && stmt_line_idx > 1) {
      PRINTMSG(LA_CH_LINE, 1640, Ansi, LA_CH_COLUMN);
   }

   /* Save the line and column of the EOS (which tells us where the END stmt  */
   /* really ends).  Eat the EOS.  If we are not at the end of the file, then */
   /* the next program unit either starts on the current line or the line     */
   /* immediately following.						      */

   cif_end_unit_line   = LA_CH_LINE;
   cif_end_unit_column = LA_CH_COLUMN - 1;

   NEXT_LA_CH;

   if (EOPU_encountered  &&  LA_CH_CLASS != Ch_Class_EOF) {
      cif_pgm_unit_start_line =
         (LA_CH_LINE == cif_end_unit_line) ? cif_end_unit_line :
                                             cif_end_unit_line + 1;
   }

EXIT:

   TRACE (Func_Exit, "parse_end_stmt", NULL);

   return;

}  /* parse_end_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for a main program.      *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_program_unit(boolean	err_call)

{
   int          ir_idx;
   int          glb_idx;
   int          rtn_idx;
   int          act_file_line;


   TRACE (Func_Entry, "end_program_unit", NULL);


   do_cmic_blk_checks();

   if (glb_tbl_idx[End_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[End_Attr_Idx] = create_lib_entry_attr(END_LIB_ENTRY,
                                                        END_NAME_LEN,
                                                        TOKEN_LINE(token),
                                                        TOKEN_COLUMN(token));
      ATP_NOSIDE_EFFECTS(glb_tbl_idx[End_Attr_Idx])	= TRUE;
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[End_Attr_Idx]);

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)              = Call_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)         = TOKEN_LINE(token);
   IR_COL_NUM(ir_idx)          = TOKEN_COLUMN(token);

   GLOBAL_LINE_TO_FILE_LINE(TOKEN_LINE(token), glb_idx, act_file_line);
   GL_SOURCE_LINES(global_line_tbl_idx) = act_file_line;
   set_related_gl_source_lines(global_line_tbl_idx);

   IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
   IR_IDX_L(ir_idx)            = glb_tbl_idx[End_Attr_Idx];
   IR_COL_NUM_L(ir_idx)        = IR_COL_NUM(ir_idx);
   IR_LINE_NUM_L(ir_idx)       = IR_LINE_NUM(ir_idx); 

   if (err_call) {  /* This is an error situation */
      gen_sh(Before, End_Program_Stmt, stmt_start_line, stmt_start_col,
             TRUE, FALSE, FALSE);

      SCP_LAST_SH_IDX(curr_scp_idx)		= SH_PREV_IDX(curr_stmt_sh_idx);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))	= ir_idx;
      SCP_IN_ERR(curr_scp_idx)			= TRUE;
   }
   else {
      SCP_LAST_SH_IDX(curr_scp_idx) = curr_stmt_sh_idx;
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

      NTR_IR_TBL(rtn_idx);
      IR_TYPE_IDX(rtn_idx) = TYPELESS_DEFAULT_TYPE;
      IR_OPR(rtn_idx) = Return_Opr;
      IR_LINE_NUM(rtn_idx) = IR_LINE_NUM(ir_idx);
      IR_COL_NUM(rtn_idx) = IR_COL_NUM(ir_idx);
      gen_sh(After,
             Return_Stmt,
             IR_LINE_NUM(ir_idx),
             IR_COL_NUM(ir_idx),
             FALSE,
             TRUE,
             TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = rtn_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      if (stmt_label_idx != NULL_IDX && !err_call) {
         ATL_CLASS(stmt_label_idx) = Lbl_User;
         ATL_DEBUG_CLASS(stmt_label_idx) = Ldbg_User_Lbl;
         ATL_EXECUTABLE(stmt_label_idx)	= TRUE;

         if (!AT_DEFINED(stmt_label_idx)  && 
              ATL_FWD_REF_IDX(stmt_label_idx) != NULL_IDX) {
            resolve_fwd_lbl_refs();
         }

         stmt_label_idx = NULL_IDX;
      }

      if (cmd_line_flags.debug_lvl == Debug_Lvl_0) {  /* -ed  -G0 */

         gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
      }
      else if (cmd_line_flags.debug_lvl == Debug_Lvl_1) {  /* -ez -G1 */
         gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Start_Epilogue, NULL_IDX);
      }
   }

   EOPU_encountered			     = TRUE;
   curr_stmt_category			     = Init_Stmt_Cat;

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_End_Program_Stmt, statement_number);
   }

   if (cif_flags & BASIC_RECS) {
      cif_end_scope_rec();
   }

   if (err_call  &&  ! clearing_blk_stk) {

      /* Needed for both CIF and buffered message file.			      */

      if (cif_flags & BASIC_RECS) {
         cif_send_attr(SCP_ATTR_IDX(curr_scp_idx), NULL_IDX);
      }
      cif_end_unit_rec(AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   }

   ATP_SCP_ALIVE(SCP_ATTR_IDX(curr_scp_idx)) = FALSE;

   POP_BLK_STK;
   PRINT_SCP_TBL;
   PRINT_EQV_TBL;

   TRACE (Func_Exit, "end_program_unit", NULL);

   return;

}  /* end_program_unit */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for a FUNCTION program.  *|
|*	This is for external FUNCTIONS only.  Internal, module, and interface *|
|*	FUNCTIONS have their own end routines.                                *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_function(boolean	err_call)
{
   int		idx;
   int		ir_idx;
   int		glb_idx;
   int		act_file_line;


   TRACE (Func_Entry, "end_function", NULL);

   do_cmic_blk_checks();

   NTR_IR_TBL(ir_idx);

   IR_OPR(ir_idx)	= Return_Opr;
   IR_TYPE_IDX(ir_idx)  = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)	= TOKEN_LINE(token);
   IR_COL_NUM(ir_idx)	= TOKEN_COLUMN(token);

   GLOBAL_LINE_TO_FILE_LINE(TOKEN_LINE(token), glb_idx, act_file_line);
   GL_SOURCE_LINES(global_line_tbl_idx) = act_file_line;
   set_related_gl_source_lines(global_line_tbl_idx);

   if (err_call) {  /* This is an error situation */
      gen_sh(Before, End_Function_Stmt, stmt_start_line, stmt_start_col,
             TRUE, FALSE, FALSE);

      SCP_LAST_SH_IDX(curr_scp_idx)		= SH_PREV_IDX(curr_stmt_sh_idx);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))	= ir_idx;
      SCP_IN_ERR(curr_scp_idx)			= TRUE;
   }
   else {
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      SCP_LAST_SH_IDX(curr_scp_idx) = curr_stmt_sh_idx;
      SH_STMT_TYPE(curr_stmt_sh_idx) = End_Function_Stmt;

      if (stmt_label_idx != NULL_IDX) {
         ATL_CLASS(stmt_label_idx) = Lbl_User;
         ATL_DEBUG_CLASS(stmt_label_idx) = Ldbg_User_Lbl;
         ATL_EXECUTABLE(stmt_label_idx)	= TRUE;

         if (!AT_DEFINED(stmt_label_idx)  && 
              ATL_FWD_REF_IDX(stmt_label_idx) != NULL_IDX) {
            resolve_fwd_lbl_refs();
         }

         stmt_label_idx = NULL_IDX;
      }

      if (cmd_line_flags.debug_lvl == Debug_Lvl_0) {  /* -ed -G0 */
         gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
      }
      else if (cmd_line_flags.debug_lvl == Debug_Lvl_1) {  /* -ez -G1 */
         gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Start_Epilogue, NULL_IDX);
      }
   } 

   EOPU_encountered	           = TRUE;
   curr_stmt_category	           = Init_Stmt_Cat;

   /* Entry points are linked thru the current scopes SCP_ENTRY_IDX.  This   */
   /* points to the attr list table, which contains a list of all entry pts. */

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_End_Function_Stmt, statement_number);
   }
 
   if (cif_flags & BASIC_RECS) {
      cif_end_scope_rec();
   }

   if (err_call  &&  ! clearing_blk_stk) {

      /* Needed for both CIF and buffered message file.			      */

      if (cif_flags & BASIC_RECS) {
         cif_send_attr(SCP_ATTR_IDX(curr_scp_idx), NULL_IDX);
      }
      cif_end_unit_rec(AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));

      ATP_SCP_ALIVE(SCP_ATTR_IDX(curr_scp_idx))	= FALSE;
      idx					= SCP_ENTRY_IDX(curr_scp_idx);

      while (idx != NULL_IDX) {

         if (cif_flags & BASIC_RECS) {
            cif_send_attr(AL_ATTR_IDX(idx), NULL_IDX);
         }
         ATP_SCP_ALIVE(AL_ATTR_IDX(idx))	= FALSE;
         idx				= AL_NEXT_IDX(idx);
      }
   }
   else {

      ATP_SCP_ALIVE(SCP_ATTR_IDX(curr_scp_idx))	= FALSE;
      idx					= SCP_ENTRY_IDX(curr_scp_idx);

      while (idx != NULL_IDX) {
         ATP_SCP_ALIVE(AL_ATTR_IDX(idx))	= FALSE;
         idx				= AL_NEXT_IDX(idx);
      }
   }

   POP_BLK_STK;
   PRINT_SCP_TBL;
   PRINT_EQV_TBL;

   TRACE (Func_Exit, "end_function", NULL);

   return;

}  /* end_function */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for a SUBROUTINE program.*|
|*	This is for external SUBROUTINES only.  Internal, module, and         *|
|*	interface SUBROUTINES have their own end routines.                    *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_subroutine(boolean	err_call)

{
   int		idx;
   int		ir_idx;
   int		glb_idx;
   int		act_file_line;


   TRACE (Func_Entry, "end_subroutine", NULL);

   do_cmic_blk_checks();

   NTR_IR_TBL(ir_idx);

   IR_OPR(ir_idx)	= Return_Opr;
   IR_TYPE_IDX(ir_idx)  = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)	= TOKEN_LINE(token);
   IR_COL_NUM(ir_idx)	= TOKEN_COLUMN(token);

   GLOBAL_LINE_TO_FILE_LINE(TOKEN_LINE(token), glb_idx, act_file_line);
   GL_SOURCE_LINES(global_line_tbl_idx) = act_file_line;
   set_related_gl_source_lines(global_line_tbl_idx);

   if (ATP_HAS_ALT_RETURN(SCP_ATTR_IDX(curr_scp_idx))) {
      /* if no alt return spec was specified, supply zero */
      IR_FLD_L(ir_idx) = CN_Tbl_Idx;
      IR_IDX_L(ir_idx) = CN_INTEGER_ZERO_IDX;
      IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM_L(ir_idx)  = TOKEN_COLUMN(token);
   }

   if (err_call) {  /* This is an error situation */
      gen_sh(Before, End_Subroutine_Stmt, stmt_start_line, stmt_start_col,
             TRUE, FALSE, FALSE);

      SCP_LAST_SH_IDX(curr_scp_idx) = SH_PREV_IDX(curr_stmt_sh_idx);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
      SCP_IN_ERR(curr_scp_idx) = TRUE;
   }
   else {
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      SCP_LAST_SH_IDX(curr_scp_idx) = curr_stmt_sh_idx;

      if (stmt_label_idx != NULL_IDX) {
         ATL_CLASS(stmt_label_idx) = Lbl_User;
         ATL_DEBUG_CLASS(stmt_label_idx) = Ldbg_User_Lbl;
         ATL_EXECUTABLE(stmt_label_idx)	= TRUE;

         if (!AT_DEFINED(stmt_label_idx)  && 
              ATL_FWD_REF_IDX(stmt_label_idx) != NULL_IDX) {
            resolve_fwd_lbl_refs();
         }

         end_labeled_do();
         stmt_label_idx = NULL_IDX;
      }

      if (cmd_line_flags.debug_lvl == Debug_Lvl_0) {  /* -ed -G0 */
         gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
      }
      else if (cmd_line_flags.debug_lvl == Debug_Lvl_1) {  /* -ez -G1 */
         gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Start_Epilogue, NULL_IDX);
      }
   }

   EOPU_encountered		   = TRUE;
   curr_stmt_category		   = Init_Stmt_Cat;

   /* Entry points are linked thru the current scopes SCP_ENTRY_IDX.  This   */
   /* points to the attr list table, which contains a list of all entry pts. */

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_End_Subroutine_Stmt, statement_number);
   }
 
   if (cif_flags & BASIC_RECS) {
      cif_end_scope_rec();
   }

   if (err_call  &&  ! clearing_blk_stk) {

      /* Needed for both CIF and buffered message file.			      */

      if (cif_flags & BASIC_RECS) {
         cif_send_attr(SCP_ATTR_IDX(curr_scp_idx), NULL_IDX);
      }
      cif_end_unit_rec(AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));

      ATP_SCP_ALIVE(SCP_ATTR_IDX(curr_scp_idx))	= FALSE;
      idx					= SCP_ENTRY_IDX(curr_scp_idx);

      while (idx != NULL_IDX) {

         if (cif_flags & BASIC_RECS) {
            cif_send_attr(AL_ATTR_IDX(idx), NULL_IDX);
         }
         ATP_SCP_ALIVE(AL_ATTR_IDX(idx))	= FALSE;
         idx					= AL_NEXT_IDX(idx);
      }
   }
   else {
      ATP_SCP_ALIVE(SCP_ATTR_IDX(curr_scp_idx))	= FALSE;
      idx					= SCP_ENTRY_IDX(curr_scp_idx);

      while (idx != NULL_IDX) {
         ATP_SCP_ALIVE(AL_ATTR_IDX(idx))	= FALSE;
         idx					= AL_NEXT_IDX(idx);
      }
   }

   POP_BLK_STK;
   PRINT_SCP_TBL;
   PRINT_EQV_TBL;

   TRACE (Func_Exit, "end_subroutine", NULL);

   return;

}  /* end_subroutine */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for a MODULE program.    *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_module(boolean	err_call)
{

   int      act_file_line;
   int      glb_idx;


   TRACE (Func_Entry, "end_module", NULL);

   GLOBAL_LINE_TO_FILE_LINE(stmt_start_line, glb_idx, act_file_line);
   GL_SOURCE_LINES(global_line_tbl_idx) = act_file_line;
   set_related_gl_source_lines(global_line_tbl_idx);

   if (err_call) {  /* This is an error situation */
      gen_sh(Before, End_Module_Stmt, stmt_start_line, stmt_start_col,
             TRUE, FALSE, FALSE);
      SCP_LAST_SH_IDX(curr_scp_idx) = SH_PREV_IDX(curr_stmt_sh_idx);
      SCP_IN_ERR(curr_scp_idx) = TRUE;
   }
   else {
      SCP_LAST_SH_IDX(curr_scp_idx) = curr_stmt_sh_idx;

      if (stmt_label_idx != NULL_IDX && !err_call) {
         ATL_CLASS(stmt_label_idx) = Lbl_User;
         ATL_DEBUG_CLASS(stmt_label_idx) = Ldbg_User_Lbl;
         ATL_EXECUTABLE(stmt_label_idx)	= TRUE;

         if (!AT_DEFINED(stmt_label_idx) && 
              ATL_FWD_REF_IDX(stmt_label_idx) != NULL_IDX) {
            resolve_fwd_lbl_refs();
         }
         stmt_label_idx = NULL_IDX;
      }
   }

   EOPU_encountered		    	     = TRUE;
   curr_stmt_category			     = Init_Stmt_Cat;

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_End_Module_Stmt, statement_number);
   }
 
   if (cif_flags & BASIC_RECS) {
      cif_end_scope_rec();
   }

   if (err_call  &&  ! clearing_blk_stk) {

      /* Needed for both CIF and buffered message file.			      */

      if (cif_flags & BASIC_RECS) {
         cif_send_attr(SCP_ATTR_IDX(curr_scp_idx), NULL_IDX);
      }
      cif_end_unit_rec(AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   }

   ATP_SCP_ALIVE(SCP_ATTR_IDX(curr_scp_idx)) = FALSE;

   POP_BLK_STK;
   PRINT_SCP_TBL;
   PRINT_EQV_TBL;

   TRACE (Func_Exit, "end_module", NULL);

   return;

}  /* end_module */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for a BLOCKDATA program. *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_blockdata(boolean	err_call)
{

int       glb_idx;
int       act_file_line;

   TRACE (Func_Entry, "end_blockdata", NULL);

   GLOBAL_LINE_TO_FILE_LINE(stmt_start_line, glb_idx, act_file_line);
   GL_SOURCE_LINES(global_line_tbl_idx) = act_file_line;
   set_related_gl_source_lines(global_line_tbl_idx);

   if (err_call) {  /* This is an error situation */
      gen_sh(Before, End_Blockdata_Stmt, stmt_start_line, stmt_start_col,
             TRUE, FALSE, FALSE);
      SCP_LAST_SH_IDX(curr_scp_idx) = SH_PREV_IDX(curr_stmt_sh_idx);
      SCP_IN_ERR(curr_scp_idx) = TRUE;
   }
   else {
      SCP_LAST_SH_IDX(curr_scp_idx) = curr_stmt_sh_idx;

      if (stmt_label_idx != NULL_IDX && !err_call) {
         ATL_CLASS(stmt_label_idx) = Lbl_User;
         ATL_DEBUG_CLASS(stmt_label_idx) = Ldbg_User_Lbl;
         ATL_EXECUTABLE(stmt_label_idx)	= TRUE;

         if (!AT_DEFINED(stmt_label_idx)  && 
              ATL_FWD_REF_IDX(stmt_label_idx) != NULL_IDX) {
            resolve_fwd_lbl_refs();
         }
         stmt_label_idx = NULL_IDX;
      }
   }

   EOPU_encountered			     = TRUE;
   curr_stmt_category			     = Init_Stmt_Cat;
   ATP_SCP_ALIVE(SCP_ATTR_IDX(curr_scp_idx)) = FALSE;

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_End_Block_Data_Stmt, statement_number);
   }
 
   if (cif_flags & BASIC_RECS) {
      cif_end_scope_rec();
   }

   if (err_call  &&  ! clearing_blk_stk) {

      /* Needed for both CIF and buffered message file.			      */

      if (cif_flags & BASIC_RECS) {
         cif_send_attr(SCP_ATTR_IDX(curr_scp_idx), NULL_IDX);
      }
      cif_end_unit_rec(AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   }

   ATP_SCP_ALIVE(SCP_ATTR_IDX(curr_scp_idx)) = FALSE;

   POP_BLK_STK;
   PRINT_SCP_TBL;
   PRINT_EQV_TBL;

   TRACE (Func_Exit, "end_blockdata", NULL);

   return;

}  /* end_blockdata */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for an internal procedure*|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_internal_proc(boolean	err_call)

{
   int		 attr_idx;
   int		 ir_idx;

   TRACE (Func_Entry, "end_internal_proc", NULL);

#ifdef KEY /* Bug 14110 */
   revisit_volatile();
#endif /* KEY Bug 14110 */

   do_cmic_blk_checks();

   NTR_IR_TBL(ir_idx);

   IR_OPR(ir_idx)	= Return_Opr;
   IR_TYPE_IDX(ir_idx)  = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)	= TOKEN_LINE(token);
   IR_COL_NUM(ir_idx)	= TOKEN_COLUMN(token);
   attr_idx		= SCP_ATTR_IDX(curr_scp_idx);

   if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Subroutine &&
       ATP_HAS_ALT_RETURN(SCP_ATTR_IDX(curr_scp_idx))) {
      /* if no alt return spec was specified, supply zero */
      IR_FLD_L(ir_idx) = CN_Tbl_Idx;
      IR_IDX_L(ir_idx) = CN_INTEGER_ZERO_IDX;
      IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM_L(ir_idx)  = TOKEN_COLUMN(token);
   }

   if (err_call) {  /* This is an error situation */
      gen_sh(Before,
             stmt_type,
             stmt_start_line,
             stmt_start_col,
             TRUE,
             FALSE,
             FALSE);

      SCP_LAST_SH_IDX(curr_scp_idx)		= SH_PREV_IDX(curr_stmt_sh_idx);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))	= ir_idx;
      SCP_IN_ERR(curr_scp_idx)			= TRUE;
   }
   else {
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      SCP_LAST_SH_IDX(curr_scp_idx) = curr_stmt_sh_idx;

      if (stmt_label_idx != NULL_IDX) {
         ATL_CLASS(stmt_label_idx) = Lbl_User;
         ATL_DEBUG_CLASS(stmt_label_idx) = Ldbg_User_Lbl;
         ATL_EXECUTABLE(stmt_label_idx) = TRUE;

         if (!AT_DEFINED(stmt_label_idx) && 
              ATL_FWD_REF_IDX(stmt_label_idx) != NULL_IDX) {
            resolve_fwd_lbl_refs();
         }
         stmt_label_idx = NULL_IDX;
      }

      if (cmd_line_flags.debug_lvl == Debug_Lvl_0) {  /* -ed -G0 */
         gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
      }
      else if (cmd_line_flags.debug_lvl == Debug_Lvl_1) {  /* -ez -G1 */
         gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Start_Epilogue, NULL_IDX);
      }
   }

   if (cif_flags & BASIC_RECS) {
      cif_end_scope_rec();
   }

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, (ATP_PGM_UNIT(attr_idx) == Function) ?
                               CIF_End_Function_Stmt : CIF_End_Subroutine_Stmt,
                        statement_number);
   }
 
   if (stmt_type == End_Stmt) {
      PRINTMSG(stmt_start_line, 86, Error, stmt_start_col,
               "internal-procedure",
               (ATP_PGM_UNIT(attr_idx) == Function) ? "FUNCTION" :
                                                      "SUBROUTINE");
   }

   curr_stmt_category		   = Sub_Func_Stmt_Cat;
   ATP_SCP_ALIVE(attr_idx)	   = FALSE;
   curr_scp_idx			   = SCP_PARENT_IDX(curr_scp_idx);

   /* SH_NEXT_IDX of curr_stmt_sh_idx is NULL_IDX and will stay that way,   */
   /* because if there are SH's for the parent, curr_stmt_sh_idx will be    */
   /* set to the last of them.  If there are no stmt headers for the parent */
   /* a new statement header will be generated.  The only way there should  */
   /* be no SH's for the parent is if the parent is an interface block.  If */
   /* this is true, we're in a scoping error situation, because internal    */
   /* procedures cannot be in an interface block.  We check for this to     */
   /* prevent aborts and other bad things, if we're in an error situation.  */

   if (SCP_LAST_SH_IDX(curr_scp_idx) == NULL_IDX) { /* No IR for parent scp */
      curr_stmt_sh_idx			= ntr_sh_tbl();
      SCP_FIRST_SH_IDX(curr_scp_idx)	= curr_stmt_sh_idx;
      need_new_sh			= FALSE;   /* Use this SH */
   }
   else {
      curr_stmt_sh_idx			= SCP_LAST_SH_IDX(curr_scp_idx);
   }

   POP_BLK_STK;
   PRINT_SCP_TBL;
   PRINT_EQV_TBL;

   TRACE (Func_Exit, "end_internal_proc", NULL);

   return;

}  /* end_internal_proc */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for a module procedure.  *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_module_proc(boolean	err_call)

{
   int		attr_idx;
   int		idx;
   int		ir_idx;


   TRACE (Func_Entry, "end_module_proc", NULL);

#ifdef KEY /* Bug 14110 */
   revisit_volatile();
#endif /* KEY Bug 14110 */

   do_cmic_blk_checks();

   NTR_IR_TBL(ir_idx);

   IR_OPR(ir_idx)		= Return_Opr;
   IR_TYPE_IDX(ir_idx)          = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)		= TOKEN_LINE(token);
   IR_COL_NUM(ir_idx)		= TOKEN_COLUMN(token);
   attr_idx			= SCP_ATTR_IDX(curr_scp_idx);

   if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Subroutine &&
       ATP_HAS_ALT_RETURN(SCP_ATTR_IDX(curr_scp_idx))) {
      /* if no alt return spec was specified, supply zero */
      IR_FLD_L(ir_idx) = CN_Tbl_Idx;
      IR_IDX_L(ir_idx) = CN_INTEGER_ZERO_IDX;
      IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM_L(ir_idx)  = TOKEN_COLUMN(token);
   }

   if (err_call) {  /* This is an error situation */
      gen_sh(Before,
             stmt_type,
             stmt_start_line,
             stmt_start_col,
             TRUE,
             FALSE,
             FALSE);

      SCP_LAST_SH_IDX(curr_scp_idx) = SH_PREV_IDX(curr_stmt_sh_idx);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
      SCP_IN_ERR(curr_scp_idx) = TRUE;
   }
   else {
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      SCP_LAST_SH_IDX(curr_scp_idx) = curr_stmt_sh_idx;

      if (stmt_label_idx != NULL_IDX) {
         ATL_CLASS(stmt_label_idx)      	= Lbl_User;
         ATL_DEBUG_CLASS(stmt_label_idx)	= Ldbg_User_Lbl;
         ATL_EXECUTABLE(stmt_label_idx) 	= TRUE;

         if (!AT_DEFINED(stmt_label_idx) && 
              ATL_FWD_REF_IDX(stmt_label_idx) != NULL_IDX) {
            resolve_fwd_lbl_refs();
         }

         stmt_label_idx = NULL_IDX;
      }

      if (cmd_line_flags.debug_lvl == Debug_Lvl_0) {  /* -ed -G0 */
         gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
      }
      else if (cmd_line_flags.debug_lvl == Debug_Lvl_1) {  /* -ez -G1 */
         gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Start_Epilogue, NULL_IDX);
      }
   }

   if (cif_flags & BASIC_RECS) {
      cif_end_scope_rec();
   }

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, (ATP_PGM_UNIT(CURR_BLK_NAME) == Function) ?
                               CIF_End_Function_Stmt : CIF_End_Subroutine_Stmt,
                        statement_number);
   }

   if (stmt_type == End_Stmt) {
      PRINTMSG(stmt_start_line, 86, Error,
               stmt_start_col,
               "module-procedure",
               (ATP_PGM_UNIT(CURR_BLK_NAME) == Function) ? "FUNCTION" :
                                                           "SUBROUTINE");
   }

   ATP_SCP_ALIVE(attr_idx) = FALSE;

   /* Entry points are linked thru the current scopes SCP_ENTRY_IDX.  This   */
   /* points to the attr list table, which contains a list of all entry pts. */

   idx = SCP_ENTRY_IDX(curr_scp_idx);

   while (idx != NULL_IDX) {
      ATP_SCP_ALIVE(AL_ATTR_IDX(idx)) = FALSE;
      idx = AL_NEXT_IDX(idx);
   }

   curr_stmt_category = Sub_Func_Stmt_Cat;
   curr_scp_idx	= SCP_PARENT_IDX(curr_scp_idx);

   /* SH_NEXT_IDX of curr_stmt_sh_idx is NULL_IDX and will stay that way,   */
   /* because if there are SH's for the parent, curr_stmt_sh_idx will be    */
   /* set to the last of them.  If there are no stmt headers for the parent */
   /* a new statement header will be generated.  The only way there should  */
   /* be no SH's for the parent is if the parent is an interface block.  If */
   /* this is true, we're in a scoping error situation, because internal    */
   /* procedures cannot be in an interface block.  We check for this to     */
   /* prevent aborts and other bad things, if we're in an error situation.  */


   if (SCP_LAST_SH_IDX(curr_scp_idx) == NULL_IDX) { /* No IR for parent scp */
      curr_stmt_sh_idx			= ntr_sh_tbl();
      SCP_FIRST_SH_IDX(curr_scp_idx)	= curr_stmt_sh_idx;
      need_new_sh			= FALSE;   /* Use this SH */
   }
   else {
      curr_stmt_sh_idx			= SCP_LAST_SH_IDX(curr_scp_idx);
   }

   POP_BLK_STK;
   PRINT_SCP_TBL;
   PRINT_EQV_TBL;

   TRACE (Func_Exit, "end_module_proc", NULL);

   return;

}  /* end_module_proc */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for an interface body.   *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_interface_body(boolean	err_call)

{
   int		interface_idx;
   int		parent_idx;
   int		save_curr_scp_idx;
   int		sibling_idx;


   TRACE (Func_Entry, "end_interface_body", NULL);

   if (cif_flags & BASIC_RECS) {
      cif_scope_info_rec();
      cif_end_scope_rec();
   }

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE,
                        (ATP_PGM_UNIT(CURR_BLK_NAME) == Function) ?
                        CIF_End_Function_Stmt : CIF_End_Subroutine_Stmt,
                        statement_number);
   }

   curr_stmt_sh_idx	= SCP_FIRST_SH_IDX(curr_scp_idx);
   stmt_start_line	= SH_GLB_LINE(curr_stmt_sh_idx);
   stmt_start_col	= SH_COL_NUM(curr_stmt_sh_idx);
   need_new_sh		= TRUE;

   interface_semantics_pass_driver();

   parent_idx		= SCP_PARENT_IDX(curr_scp_idx);

   free_attr_list(SCP_TMP_FW_IDX2(curr_scp_idx));
   free_attr_list(SCP_TMP_FW_IDX(curr_scp_idx));
   free_attr_list(SCP_ENTRY_IDX(curr_scp_idx));
   free_attr_list(SCP_TMP_LIST(curr_scp_idx));
   free_attr_list(SCP_ATTR_LIST(curr_scp_idx));

   SCP_ATTR_LIST(curr_scp_idx)		= NULL_IDX;
   SCP_TMP_FW_IDX2(curr_scp_idx)	= NULL_IDX;
   SCP_TMP_FW_IDX(curr_scp_idx)		= NULL_IDX;
   SCP_ENTRY_IDX(curr_scp_idx)		= NULL_IDX;
   SCP_TMP_LIST(curr_scp_idx)		= NULL_IDX;

   remove_hidden_name_tbl(curr_scp_idx);

   if (!SCP_IN_ERR(curr_scp_idx) &&
        BLK_TYPE(blk_stk_idx - 1) == Interface_Blk) {
      blk_stk_idx--;
      interface_idx = (BLK_NAME(blk_stk_idx) == NULL_IDX) ?
                       BLK_UNNAMED_INTERFACE(blk_stk_idx) :
                       BLK_NAME(blk_stk_idx);

      if ((ATI_NUM_SPECIFICS(interface_idx) % 8) == 0 &&
          !AT_DCL_ERR(interface_idx) && ATI_HAS_NON_MOD_PROC(interface_idx)) {
         save_curr_scp_idx			= curr_scp_idx;
         curr_scp_idx				= parent_idx;
         collapse_interface_blk(interface_idx);
         ATI_HAS_NON_MOD_PROC(interface_idx)	= FALSE;
         BLK_AT_IDX(blk_stk_idx)		= NULL_IDX;
         BLK_BD_IDX(blk_stk_idx)		= NULL_IDX;
         BLK_CN_IDX(blk_stk_idx)		= NULL_IDX;
         BLK_CP_IDX(blk_stk_idx)		= NULL_IDX;
         BLK_NP_IDX(blk_stk_idx)		= NULL_IDX;
         BLK_SB_IDX(blk_stk_idx)		= NULL_IDX;
         BLK_SN_IDX(blk_stk_idx)		= NULL_IDX;
         BLK_TYP_IDX(blk_stk_idx)		= NULL_IDX;
         curr_scp_idx				= save_curr_scp_idx;
      }
      blk_stk_idx++;
   }

   if (SCP_FIRST_CHILD_IDX(curr_scp_idx) == NULL_IDX) {

      if (SCP_SIBLING_IDX(curr_scp_idx) == NULL_IDX) {

         /* NO block errors - so reuse loc name table space */

         loc_name_tbl_idx	= SCP_LN_FW_IDX(curr_scp_idx) - 1;
      }
      scp_tbl_idx		= curr_scp_idx - 1;
      
      if (SCP_NUM_CHILDREN(parent_idx) == 1) {          /* only one child */
         SCP_FIRST_CHILD_IDX(parent_idx) = NULL_IDX;
         SCP_LAST_CHILD_IDX(parent_idx)  = NULL_IDX;
         SCP_NUM_CHILDREN(parent_idx)    = 0;
      }
      else { /* find next to last sibling scp */
         sibling_idx = SCP_FIRST_CHILD_IDX(parent_idx);

         while (SCP_SIBLING_IDX(sibling_idx) != curr_scp_idx) {
            sibling_idx = SCP_SIBLING_IDX(sibling_idx);
         }

         SCP_SIBLING_IDX(sibling_idx)   = NULL_IDX;
         SCP_LAST_CHILD_IDX(parent_idx) = sibling_idx;
         (SCP_NUM_CHILDREN(parent_idx))--;
      }
   }
   else { /* There are one or more block errors, so remove this interface  */
          /* body carefully.  Merge the interface's sibling list with the  */
          /* interface's child's sibling list.  Then  curr_scp_idx becomes */
          /* inaccessible, because it is out of the scope table link.      */

      SCP_PARENT_IDX(SCP_FIRST_CHILD_IDX(curr_scp_idx)) = parent_idx;

      sibling_idx		= SCP_FIRST_CHILD_IDX(curr_scp_idx);

      while (SCP_SIBLING_IDX(sibling_idx) != NULL_IDX) {
         sibling_idx = SCP_SIBLING_IDX(sibling_idx);
         SCP_PARENT_IDX(sibling_idx) = parent_idx;
      }

      if (SCP_NUM_CHILDREN(parent_idx) == 1) {    /* only one child */
         SCP_FIRST_CHILD_IDX(parent_idx) = SCP_FIRST_CHILD_IDX(curr_scp_idx);
         SCP_LAST_CHILD_IDX(parent_idx)  = SCP_LAST_CHILD_IDX(curr_scp_idx);
         SCP_NUM_CHILDREN(parent_idx)    = SCP_NUM_CHILDREN(curr_scp_idx);
      }
      else { /* find next to last sibling scp */
         sibling_idx = SCP_FIRST_CHILD_IDX(parent_idx);

         while (SCP_SIBLING_IDX(sibling_idx) != curr_scp_idx) {
            sibling_idx = SCP_SIBLING_IDX(sibling_idx);
         }

         SCP_SIBLING_IDX(sibling_idx)   = SCP_FIRST_CHILD_IDX(curr_scp_idx);
         SCP_LAST_CHILD_IDX(parent_idx) = SCP_LAST_CHILD_IDX(curr_scp_idx);
         SCP_NUM_CHILDREN(parent_idx)   = SCP_NUM_CHILDREN(parent_idx) - 1 +
                                          SCP_NUM_CHILDREN(curr_scp_idx);
      }
   }

   curr_scp_idx		= parent_idx;
   curr_stmt_category	= Sub_Func_Stmt_Cat;

   /* If there are no stmt headers for the parent, always reuse curr_stmt_sh */
   /* _idx because even if this statement is labeled, it doesn't need a SH.  */
   /* The only IR that can be generated for this interface body is labels.   */
   /* These can never be referenced, because you can't jump from one scope   */
   /* to another, and you can't have a goto inside an interface body.        */
   /* There will be no SH's for the parent if the parent is an interface     */
   /* block.  This is perfectly legal.                                       */

   if (SCP_LAST_SH_IDX(curr_scp_idx) == NULL_IDX) { /* No IR for parent scp */
      SCP_FIRST_SH_IDX(curr_scp_idx)	= curr_stmt_sh_idx;
      SH_PREV_IDX(curr_stmt_sh_idx)	= NULL_IDX;
      need_new_sh			= FALSE;
   }
   else {
      curr_stmt_sh_idx			= SCP_LAST_SH_IDX(curr_scp_idx);
      need_new_sh			= TRUE;
   }

   POP_BLK_STK;
   PRINT_SCP_TBL;
   PRINT_EQV_TBL;

   TRACE (Func_Exit, "end_interface_body", NULL);

   return;

}  /* end_interface_body */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END FORALL statement.	              *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_forall_blk(boolean	err_call)

{
   TRACE (Func_Entry, "end_forall_blk", NULL);

   if (cmd_line_flags.debug_lvl <= Debug_Lvl_1  &&  ! err_call) { 

      /*-ez -ed -G0 -G1*/

      gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
   }

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_End_Forall_Stmt, statement_number);
   }

   SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = CURR_BLK_FIRST_SH_IDX;

   IR_FLD_L(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) = SH_Tbl_Idx;
   IR_IDX_L(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) = curr_stmt_sh_idx;
   IR_LINE_NUM_L(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) = 
                       SH_GLB_LINE(CURR_BLK_FIRST_SH_IDX);
   IR_COL_NUM_L(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) = 
                       SH_COL_NUM(CURR_BLK_FIRST_SH_IDX);
   
   /* LRR:  Before popping the Blocking Stmt stack, need to use it to get to  */
   /* the FORALL SH to get to each of the index name Attrs and clear the flag */
   /* that says it's alive.						      */

   POP_BLK_STK;

   TRACE (Func_Exit, "end_forall_blk", NULL);

   return;

}  /* end_forall_blk */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END WHERE statement.	              *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_where_blk(boolean	err_call)

{
   int		sh_idx;

   TRACE (Func_Entry, "end_where_blk", NULL);

   if (cmd_line_flags.debug_lvl <= Debug_Lvl_1 && !err_call) { 
      /*-ez -ed -G0 -G1*/
      gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
   }

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_End_Where_Stmt, statement_number);
   }

   if (CURR_BLK == Where_Then_Blk ||
       CURR_BLK == Where_Else_Blk ||
       CURR_BLK == Where_Else_Mask_Blk) {

      SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = CURR_BLK_FIRST_SH_IDX;

      sh_idx = SH_PARENT_BLK_IDX(curr_stmt_sh_idx);
      while (sh_idx != NULL_IDX &&
             SH_STMT_TYPE(sh_idx) != Where_Cstrct_Stmt) {

         sh_idx = SH_PARENT_BLK_IDX(sh_idx);
      }

      if (sh_idx != NULL_IDX) {
         IR_FLD_R(SH_IR_IDX(sh_idx)) = SH_Tbl_Idx;
         IR_IDX_R(SH_IR_IDX(sh_idx)) = curr_stmt_sh_idx;
      }
   }

   POP_BLK_STK;

   TRACE (Func_Exit, "end_where_blk", NULL);

   return;

}  /* end_where_blk */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for a SELECT construct.  *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_select_blk(boolean	err_call)

{
   int		blk_idx;
   int          il_idx_1;
   int          il_idx_2;
   int		name_idx;
   long		num_cases_value;
   int		ir_idx;
   int		save_curr_stmt_sh_idx;
   int		sh_idx;
 

   TRACE (Func_Entry, "end_select_blk", NULL);

   if (err_call) {  /* This is an error situation */
      gen_sh(Before, End_Select_Stmt, stmt_start_line, stmt_start_col,
             TRUE, FALSE, FALSE);
      curr_stmt_sh_idx	= SH_PREV_IDX(curr_stmt_sh_idx);
   }

   if (CURR_BLK == Case_Blk) {
      POP_BLK_STK; 
   }

   if (CURR_BLK == Select_Blk) {

      if (CURR_BLK_ERR) {
         goto EXIT;
      }

      /* Generate a CONTINUE stmt before the END SELECT stmt to define the    */
      /* branch-around label.  If the END SELECT stmt already has a user      */
      /* label, insert the CONTINUE stmt ahead of the Label_Def SH.	      */

      save_curr_stmt_sh_idx = curr_stmt_sh_idx;

      if (SH_LABELED(curr_stmt_sh_idx)) {
         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
      }

      gen_sh(Before, Continue_Stmt, stmt_start_line, stmt_start_col,
             FALSE, TRUE, TRUE);


      sh_idx           = SH_PREV_IDX(curr_stmt_sh_idx);
      curr_stmt_sh_idx = save_curr_stmt_sh_idx;

      if (cmd_line_flags.debug_lvl == Debug_Lvl_0) {  /* -ed -G0 */
         gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
      }

      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(sh_idx)     = ir_idx;
      IR_OPR(ir_idx)        = Label_Opr;
      IR_TYPE_IDX(ir_idx)   = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)   = stmt_start_line;
      IR_COL_NUM(ir_idx)    = stmt_start_col;
      IR_LINE_NUM_L(ir_idx) = stmt_start_line;
      IR_COL_NUM_L(ir_idx)  = stmt_start_col;
      IR_FLD_L(ir_idx)      = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)      = CURR_BLK_LABEL;

      /* CURR_BLK_LABEL is an internal label generated by the select stmt */

      AT_DEFINED(CURR_BLK_LABEL)       = TRUE;
      AT_DEF_LINE(CURR_BLK_LABEL)      = stmt_start_line;
      ATL_DEF_STMT_IDX(CURR_BLK_LABEL) = sh_idx;

      if (cmd_line_flags.debug_lvl == Debug_Lvl_1) {  /* -ez -G1 */
         ATL_DEBUG_CLASS(CURR_BLK_LABEL) = Ldbg_Stmt_Lbl;
      }

      /* Now go back to the Select IR and generate the ILs to hold the number */
      /* of cases, the branch-around label, and (if it exists) the CASE       */
      /* DEFAULT label.							      */

      ir_idx = IR_IDX_L(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX));

      NTR_IR_LIST_TBL(il_idx_1);
      IR_FLD_R(ir_idx) = IL_Tbl_Idx;
      IR_IDX_R(ir_idx) = il_idx_1;
      IL_FLD(il_idx_1) = CN_Tbl_Idx;
      num_cases_value  = (long) BLK_NUM_CASES(blk_stk_idx);
      IL_IDX(il_idx_1) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, num_cases_value); 
      IL_LINE_NUM(il_idx_1) = stmt_start_line;
      IL_COL_NUM(il_idx_1)  = stmt_start_col;

      NTR_IR_LIST_TBL(il_idx_2);
      IL_NEXT_LIST_IDX(il_idx_1) = il_idx_2;
      IL_PREV_LIST_IDX(il_idx_2) = il_idx_1;
      IL_LINE_NUM(il_idx_2)      = stmt_start_line;
      IL_COL_NUM(il_idx_2)       = stmt_start_col;
      IL_FLD(il_idx_2)           = AT_Tbl_Idx;
      IL_IDX(il_idx_2)           = CURR_BLK_LABEL;

      if (BLK_CASE_DEFAULT_LBL_FLD(blk_stk_idx) == NO_Tbl_Idx) {
         IR_LIST_CNT_R(ir_idx) = 2;
      }
      else {
         IR_LIST_CNT_R(ir_idx) = 3;
         il_idx_1              = il_idx_2;

         NTR_IR_LIST_TBL(il_idx_2);
         IL_NEXT_LIST_IDX(il_idx_1) = il_idx_2;
         IL_PREV_LIST_IDX(il_idx_2) = il_idx_1;
         COPY_OPND(IL_OPND(il_idx_2), BLK_CASE_DEFAULT_LBL_OPND(blk_stk_idx));
      }

   }
   else {

      /* Error blocks got in the way - try to find the Select_Blk.            */

      name_idx = BLK_NAME(blk_stk_idx + 1);

      for (blk_idx = blk_stk_idx;  blk_idx > 0;  --blk_idx) {

         if (BLK_TYPE(blk_idx) == Select_Blk  &&
             BLK_NAME(blk_idx) == name_idx) {
            blk_idx = move_blk_to_end(blk_idx);
            break;
         }
      }
   }


EXIT:

   SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = CURR_BLK_FIRST_SH_IDX;

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_End_Select_Stmt, statement_number);
   }


   /* Check before popping it because the program could be really messed up   */
   /* which would cause the Block Stack to be equally messed up.              */

   if (CURR_BLK == Select_Blk) {
      POP_BLK_STK;
   }


   if (err_call) {
      curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
   }
 
   TRACE (Func_Exit, "end_select_blk", NULL);

   return;

}  /* end_select_blk */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This procedure is called from the main parse driver and the "end      *|
|*      program unit" routines in this file when the current statement is     *|
|*      labeled.  It determines whether or not the labeled statement is       *|
|*      terminating one or more DO loops.				      *|
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

void end_labeled_do()

{
   int		blk_idx;
#ifdef KEY /* Bug 10177 */
   boolean	error_flag = FALSE;
#else /* KEY Bug 10177 */
   boolean	error_flag;
#endif /* KEY Bug 10177 */
   int		fake_blk_stk_idx;
   int		loop_num = 0;
   boolean	msg_issued;
   int		save_blk_stk_idx;
   int		save_sh_err_flg;


   TRACE (Func_Entry, "end_labeled_do", NULL);


   if (stmt_label_idx == NULL_IDX) {
      return;  /* This requires that stmt_label_idx be valid */
   }

   /* Does this statement label terminate a DO loop?			      */

   if (stmt_label_idx == CURR_BLK_LABEL) {
      blk_idx = blk_stk_idx;
   }
   else {

      if (blk_stk_idx > 1) {
         blk_idx = blk_stk_idx - 1;

         while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {
         
            if (stmt_label_idx == BLK_LABEL(blk_idx)) {
               goto FOUND_DO_BLK;
            }
            --blk_idx;
         }
      }

      goto EXIT;
   }


   /* Verify that the termination statement is acceptable.                    */

FOUND_DO_BLK:
  
   if (cmd_line_flags.debug_lvl == Debug_Lvl_0) {  /* -ed -G0 */
      gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
   }

   if (stmt_type != End_Do_Stmt  &&  BLK_NAME(blk_idx) != NULL_IDX) {
      PRINTMSG(stmt_start_line, 669, Error, stmt_start_col);
   }

   switch (stmt_type) {

      case Continue_Stmt:
         if (BLK_LOOP_NUM(blk_idx) > 1) {
            PRINTMSG(stmt_start_line, 241,
#ifdef KEY /* Bug 318, 321 */
	      Ansi,
#else /* KEY Bug 318, 321 */
	      Comment,
#endif /* KEY Bug 318, 321 */
	      stmt_start_col);
         }

         break;

      case End_Do_Stmt:
         break;

      case Goto_Stmt:
         if (IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) == Br_Uncond_Opr) {
            PRINTMSG(stmt_start_line, 242, Error, stmt_start_col);
         }
         else if (IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) == Br_Asg_Opr) {
            PRINTMSG(stmt_start_line, 243, Error, stmt_start_col);
         }
         else {
            PRINTMSG(stmt_start_line, 241,
#ifdef KEY /* Bug 318, 321 */
	      Ansi,
#else /* KEY Bug 318, 321 */
	      Comment,
#endif /* KEY Bug 318, 321 */
	      stmt_start_col);
         }                   
             
         break;

      case Outmoded_If_Stmt:
         PRINTMSG(stmt_start_line, 246, Error, stmt_start_col);
         break;

      case Do_Iterative_Stmt:
      case Do_While_Stmt:
      case Do_Infinite_Stmt:
      case If_Cstrct_Stmt:
      case Select_Stmt:
      case Where_Cstrct_Stmt:
         msg_issued       = FALSE;
         save_sh_err_flg  = SH_ERR_FLG(curr_stmt_sh_idx);

         if (stmt_type == If_Cstrct_Stmt) {
            blk_idx          = blk_stk_idx - 2;
            fake_blk_stk_idx = blk_stk_idx - 2;
         }
         else {
            blk_idx          = blk_stk_idx - 1;
            fake_blk_stk_idx = blk_stk_idx - 1;
         }

         while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {

            if (stmt_label_idx == BLK_LABEL(blk_idx)) { 

               if (! msg_issued) {
                  PRINTMSG(stmt_start_line, 244, Error, stmt_start_col,
                           stmt_type_str[stmt_type]);
                  msg_issued = TRUE;
               }

               if (blk_idx != fake_blk_stk_idx) {
                  save_blk_stk_idx = blk_stk_idx;
                  blk_stk_idx      = fake_blk_stk_idx;
                  error_flag = pop_and_err_blk_stk(blk_idx, FALSE);
                  blk_stk_idx      = save_blk_stk_idx;
               }

               move_blk_to_end(blk_idx);

               if (msg_issued || error_flag) {
                  SH_ERR_FLG(CURR_BLK_FIRST_SH_IDX) = TRUE;
               }
               POP_BLK_STK;

               if (CURR_BLK == Doall_Blk) {
                  POP_BLK_STK;
                  cdir_switches.doall_region = FALSE;
                  CLEAR_DIRECTIVE_STATE(Doall_Region);
               }
               else if (CURR_BLK == SGI_Parallel_Do_Blk) {
                  POP_BLK_STK;
                  CLEAR_DIRECTIVE_STATE(Parallel_Do_Region);
               }
               else if (CURR_BLK == SGI_Doacross_Blk) {
                  POP_BLK_STK;
                  CLEAR_DIRECTIVE_STATE(Doacross_Region);
               }

               --fake_blk_stk_idx;
            }

            --blk_idx;
         }

         SH_ERR_FLG(curr_stmt_sh_idx) = save_sh_err_flg;
         goto EXIT;
         
      case End_Stmt:
      case End_Function_Stmt:
      case End_Program_Stmt:
      case End_Subroutine_Stmt:
         msg_issued = FALSE;
         blk_idx    = blk_stk_idx;

         while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {

            if (stmt_label_idx == BLK_LABEL(blk_idx)) {
               PRINTMSG(stmt_start_line, 244, Error, stmt_start_col,
                        stmt_type_str[stmt_type]);
               msg_issued = TRUE;
               break;
            }

            --blk_idx;
         }
           
         if (msg_issued) {
            break;
         }
         else { 
            goto EXIT;
         }

      case Arith_If_Stmt:
      case Cycle_Stmt:
      case Exit_Stmt:
      case Return_Stmt:
      case Stop_Stmt:
         PRINTMSG(stmt_start_line, 244, Error, stmt_start_col,
                  stmt_type_str[stmt_type]);
         break;

      case End_If_Stmt:
      case End_Select_Stmt:
      case End_Where_Stmt:
      case Case_Stmt:
      case Else_Stmt:
      case Else_If_Stmt:
      case Else_Where_Stmt:
         PRINTMSG(stmt_start_line, 244, Error, stmt_start_col,
                  stmt_type_str[stmt_type]);
        
         blk_idx = blk_stk_idx;

         while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {

            if (stmt_label_idx == BLK_LABEL(blk_idx)) {

               if (blk_idx != blk_stk_idx) {
                  move_blk_to_end(blk_idx);
               }

               SH_ERR_FLG(CURR_BLK_FIRST_SH_IDX) = TRUE;
               POP_BLK_STK;

               if (CURR_BLK == Doall_Blk) {
                  POP_BLK_STK;
                  cdir_switches.doall_region = FALSE;
                  CLEAR_DIRECTIVE_STATE(Doall_Region);
               }
               else if (CURR_BLK == SGI_Parallel_Do_Blk) {
                  POP_BLK_STK;
                  CLEAR_DIRECTIVE_STATE(Parallel_Do_Region);
               }
               else if (CURR_BLK == SGI_Doacross_Blk) {
                  POP_BLK_STK;
                  CLEAR_DIRECTIVE_STATE(Doacross_Region);
               }
            }

            --blk_idx;
         }
      
         goto EXIT;

      default:
         if (ATL_EXECUTABLE(stmt_label_idx)) {
            PRINTMSG(stmt_start_line, 241,
#ifdef KEY /* Bug 318, 321 */
	      Ansi,
#else /* KEY Bug 318, 321 */
	      Comment,
#endif /* KEY Bug 318, 321 */
	      stmt_start_col);
         }
         else {
            PRINTMSG(stmt_start_line, 544, Error, stmt_start_col);
         }
   }

   /* remove any Do Parallel blocks that are above the do block */

   blk_idx = blk_stk_idx;

   while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {

      if (BLK_TYPE(blk_idx) == Do_Parallel_Blk ||
          BLK_TYPE(blk_idx) == SGI_Pdo_Blk ||
          BLK_TYPE(blk_idx) == Open_Mp_Do_Blk ||
          BLK_TYPE(blk_idx) == Open_Mp_Parallel_Do_Blk) {

         if (BLK_TYPE(blk_idx) == Do_Parallel_Blk) {
            CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
         }
         else if (BLK_TYPE(blk_idx) == SGI_Pdo_Blk) {
            CLEAR_DIRECTIVE_STATE(Pdo_Region);
         }
         else if (BLK_TYPE(blk_idx) == Open_Mp_Do_Blk) {
            CLEAR_DIRECTIVE_STATE(Open_Mp_Do_Region);
         }
         else if (BLK_TYPE(blk_idx) == Open_Mp_Parallel_Do_Blk) {
            CLEAR_DIRECTIVE_STATE(Open_Mp_Parallel_Do_Region);
         }

         move_blk_to_end(blk_idx);
         POP_BLK_STK;
         blk_idx--;
         continue;
      }

      if (stmt_label_idx == BLK_LABEL(blk_idx)) {
         break;
      }
      --blk_idx;
   }

   /* Now close all loops this statement terminates.                          */

   blk_idx = blk_stk_idx;

   while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {

      if (stmt_label_idx == BLK_LABEL(blk_idx)) { 

         if (blk_idx != blk_stk_idx) {
            error_flag = pop_and_err_blk_stk(blk_idx, FALSE);
            move_blk_to_end(blk_idx);

            if (error_flag) {
               SH_ERR_FLG(CURR_BLK_FIRST_SH_IDX) = TRUE;
            }
         }

         if (! SH_ERR_FLG(curr_stmt_sh_idx)  &&
             ! SH_ERR_FLG(CURR_BLK_FIRST_SH_IDX)) {
            loop_end_processing();
            loop_num = BLK_LOOP_NUM(blk_idx);
         }

         POP_BLK_STK;

         if (CURR_BLK == Doall_Blk) {
            POP_BLK_STK;
            cdir_switches.doall_region = FALSE;
            CLEAR_DIRECTIVE_STATE(Doall_Region);
         }
         else if (CURR_BLK == SGI_Parallel_Do_Blk) {
            POP_BLK_STK;
            CLEAR_DIRECTIVE_STATE(Parallel_Do_Region);
         }
         else if (CURR_BLK == SGI_Doacross_Blk) {
            POP_BLK_STK;
            CLEAR_DIRECTIVE_STATE(Doacross_Region);
         }
      }  
      else if (loop_num > 1 &&
               end_task_do_blk ()) {
         POP_BLK_STK;
      }

      --blk_idx;
   }

EXIT:

   (void) end_task_do_blk();

   TRACE (Func_Exit, "end_labeled_do", NULL);

   return;

}  /* end_labeled_do */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static boolean end_task_do_blk(void)

{
   int		ir_idx;
   boolean	left_on_stk = FALSE;

   TRACE (Func_Entry, "end_task_do_blk", NULL);

   if (CURR_BLK == Do_Parallel_Blk &&
       BLK_ENDDO_PARALLEL_SH_IDX(blk_stk_idx) == NULL_IDX) {

      left_on_stk = TRUE;

      /* insert enddo parallel statement after */
      need_new_sh = TRUE;

      gen_sh(After, End_Do_Parallel_Stmt, stmt_start_line, stmt_start_col,
             FALSE, FALSE, TRUE);

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx) = Enddo_Cmic_Opr;

      /* must have a type idx */

      IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx) = stmt_start_line;
      IR_COL_NUM(ir_idx)  = stmt_start_col;

      SH_IR_IDX(curr_stmt_sh_idx)  = ir_idx;

      BLK_ENDDO_PARALLEL_SH_IDX(blk_stk_idx) = curr_stmt_sh_idx;
   }
   else if ((CURR_BLK == SGI_Pdo_Blk ||
             CURR_BLK == Open_Mp_Do_Blk ||
             CURR_BLK == Open_Mp_Parallel_Do_Blk) &&
             BLK_ENDPDO_SH_IDX(blk_stk_idx) == NULL_IDX) {


      /* insert endpdo statement after */
      need_new_sh = TRUE;


      NTR_IR_TBL(ir_idx);
      IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx) = stmt_start_line;
      IR_COL_NUM(ir_idx)  = stmt_start_col;

      IR_LINE_NUM_R(ir_idx) = stmt_start_line;
      IR_COL_NUM_R(ir_idx)  = stmt_start_col;
      IR_FLD_R(ir_idx) = SH_Tbl_Idx;
      IR_IDX_R(ir_idx) = CURR_BLK_FIRST_SH_IDX;

      switch (CURR_BLK) {
         case SGI_Pdo_Blk:
            IR_OPR(ir_idx) = End_Pdo_Par_Opr;
            gen_sh(After, SGI_End_Pdo_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);
            CLEAR_DIRECTIVE_STATE(Pdo_Region);
            POP_BLK_STK;
            break;

         case Open_Mp_Do_Blk:
            IR_OPR(ir_idx) = Enddo_Open_Mp_Opr;
            gen_sh(After, Open_MP_End_Do_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);
            CLEAR_DIRECTIVE_STATE(Open_Mp_Do_Region);
            POP_BLK_STK;
            break;

         case Open_Mp_Parallel_Do_Blk:
            IR_OPR(ir_idx) = Endparalleldo_Open_Mp_Opr;
            gen_sh(After, Open_MP_End_Do_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);
            CLEAR_DIRECTIVE_STATE(Open_Mp_Parallel_Do_Region);
            POP_BLK_STK;
            break;
      }

      SH_IR_IDX(curr_stmt_sh_idx)  = ir_idx;
   }


   TRACE (Func_Exit, "end_task_do_blk", NULL);

   return(left_on_stk);

}  /* end_task_do_blk */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END DO statement.		      *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_do_blk(boolean	err_call)

{
   int		blk_idx;
   boolean      loop_end_ir_gend	= FALSE;
   boolean	msg_issued;
   boolean	no_err			= TRUE;
   int		unlabeled_do_idx;


   TRACE (Func_Entry, "end_do_blk", NULL);

   /* If err_call is TRUE, it means this procedure is being called due to a   */
   /* blocking stmt mismatch to close out the current Block Stack frame.  Pop */
   /* the Block Stack.							      */

   if (err_call) {
      POP_BLK_STK;
      if (CURR_BLK == Doall_Blk) {
         POP_BLK_STK;
         cdir_switches.doall_region = FALSE;
         CLEAR_DIRECTIVE_STATE(Doall_Region);
      }
      else if (CURR_BLK == SGI_Parallel_Do_Blk) {
         POP_BLK_STK;
         CLEAR_DIRECTIVE_STATE(Parallel_Do_Region);
      }
      else if (CURR_BLK == SGI_Doacross_Blk) {
         POP_BLK_STK;
         CLEAR_DIRECTIVE_STATE(Doacross_Region);
      }

      goto EXIT;
   }

   /* Remove any Do Parallel blocks that are above the DO block.              */

   blk_idx = blk_stk_idx;

   while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {

      if (BLK_TYPE(blk_idx) == Do_Parallel_Blk ||
          BLK_TYPE(blk_idx) == SGI_Pdo_Blk ||
          BLK_TYPE(blk_idx) == Open_Mp_Do_Blk ||
          BLK_TYPE(blk_idx) == Open_Mp_Parallel_Do_Blk) {

         if (BLK_TYPE(blk_idx) == Do_Parallel_Blk) {
            CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
         }
         else if (BLK_TYPE(blk_idx) == SGI_Pdo_Blk) {
            CLEAR_DIRECTIVE_STATE(Pdo_Region);
         }
         else if (BLK_TYPE(blk_idx) == Open_Mp_Do_Blk) {
            CLEAR_DIRECTIVE_STATE(Open_Mp_Do_Region);
         }
         else if (BLK_TYPE(blk_idx) == Open_Mp_Parallel_Do_Blk) {
            CLEAR_DIRECTIVE_STATE(Open_Mp_Parallel_Do_Region);
         }

         move_blk_to_end(blk_idx);
         POP_BLK_STK;
         blk_idx--;
         continue;
      }
      else if (BLK_TYPE(blk_idx) == Do_Blk) {
         break;
      }

      --blk_idx;
   }


   if (cmd_line_flags.debug_lvl == Debug_Lvl_0) {  /* -ed -G0 */
      gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
   }
 
   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_End_Do_Stmt, statement_number);
   }

   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

   if (stmt_label_idx == NULL_IDX) {
      blk_idx = blk_stk_idx;

      /* Must search back through the Block Stack because there could be      */
      /* labeled DOs between this unlabeled END DO and an unlabeled DO.       */

      while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {
            
         if (BLK_TYPE(blk_idx) == Do_Blk  &&  BLK_LABEL(blk_idx) == NULL_IDX) {
               
            if (blk_idx == blk_stk_idx) {

               if (! SH_ERR_FLG(CURR_BLK_FIRST_SH_IDX)) {
                  loop_end_processing();
                  loop_end_ir_gend = TRUE;
               }

            }
            else {
               pop_and_err_blk_stk(blk_idx, FALSE);
               move_blk_to_end(blk_idx);
            }

            POP_BLK_STK;

            if (CURR_BLK == Doall_Blk) {
               POP_BLK_STK;
               cdir_switches.doall_region = FALSE;
               CLEAR_DIRECTIVE_STATE(Doall_Region);
            }
            else if (CURR_BLK == SGI_Parallel_Do_Blk) {
               POP_BLK_STK;
               CLEAR_DIRECTIVE_STATE(Parallel_Do_Region);
            }
            else if (CURR_BLK == SGI_Doacross_Blk) {
               POP_BLK_STK;
               CLEAR_DIRECTIVE_STATE(Doacross_Region);
            }

            goto EXIT;
         }

         --blk_idx;
      }

      PRINTMSG(stmt_start_line, 289, Error, stmt_start_col, "END DO", "DO");
   }
   else {
      unlabeled_do_idx = NULL_IDX;
      blk_idx          = blk_stk_idx;
      msg_issued       = FALSE;

      while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {
            
         if (BLK_TYPE(blk_idx) == Do_Blk) {

            if (BLK_LABEL(blk_idx) != NULL_IDX) {
       
               if (BLK_LABEL(blk_idx) == stmt_label_idx) {
                     
                  if (blk_idx == blk_stk_idx) {
      
                     if (BLK_LOOP_NUM(blk_stk_idx) > 1) {

                        if (! msg_issued) {
                           PRINTMSG(stmt_start_line, 735, Ansi, stmt_start_col);
      			   msg_issued = TRUE;
                        }
                     }

                     if (! SH_ERR_FLG(CURR_BLK_FIRST_SH_IDX)) {
                        loop_end_processing();
                        loop_end_ir_gend = TRUE;
                     }
                     else {
                        no_err = FALSE;
                     }
                  }
                  else {
                     pop_and_err_blk_stk(blk_idx, FALSE);
                     move_blk_to_end(blk_idx);
                     no_err = FALSE;
                  }

                  POP_BLK_STK;

                  if (CURR_BLK == Doall_Blk) {
                     POP_BLK_STK;
                     cdir_switches.doall_region = FALSE;
                     CLEAR_DIRECTIVE_STATE(Doall_Region);
                  }
                  else if (CURR_BLK == SGI_Parallel_Do_Blk) {
                     POP_BLK_STK;
                     CLEAR_DIRECTIVE_STATE(Parallel_Do_Region);
                  }
                  else if (CURR_BLK == SGI_Doacross_Blk) {
                     POP_BLK_STK;
                     CLEAR_DIRECTIVE_STATE(Doacross_Region);
                  }
               }
            }
            else if (unlabeled_do_idx == NULL_IDX) {
               unlabeled_do_idx = blk_idx;
            }
         }

         --blk_idx;
      }


      /* If the end-of-loop IR has not been generated, it means we didn't     */
      /* find a DO stmt with a matching label or there was something else     */
      /* wrong with the loop.  If we didn't find any UNlabeled DO stmts       */
      /* either, issue an error.  Otherwise, use the END DO to match the      */
      /* unlabeled DO.							      */

      if (! loop_end_ir_gend  &&  no_err) {

         if (unlabeled_do_idx == NULL_IDX) {
            PRINTMSG(stmt_start_line, 289, Error, stmt_start_col,
                     "END DO", "DO");
         }
         else {

            if (unlabeled_do_idx == blk_stk_idx) {

               if (! SH_ERR_FLG(CURR_BLK_FIRST_SH_IDX)) {
                  loop_end_processing();
               }
            }
            else {
               pop_and_err_blk_stk(blk_idx, FALSE);
               move_blk_to_end(blk_idx);
            }
   
            POP_BLK_STK;

            if (CURR_BLK == Doall_Blk) {
               POP_BLK_STK;
               cdir_switches.doall_region = FALSE;
               CLEAR_DIRECTIVE_STATE(Doall_Region);
            }
            else if (CURR_BLK == SGI_Parallel_Do_Blk) {
               POP_BLK_STK;
               CLEAR_DIRECTIVE_STATE(Parallel_Do_Region);
            }
            else if (CURR_BLK == SGI_Doacross_Blk) {
               POP_BLK_STK;
               CLEAR_DIRECTIVE_STATE(Doacross_Region);
            }
         }
      }
   }


EXIT:

   (void) end_task_do_blk();

   TRACE (Func_Exit, "end_do_blk", NULL);

   return;

}  /* end_do_blk */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END IF statement for an IF construct.  *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_if_blk(boolean	err_call)

{
   int		blk_idx;
   boolean	error;
   int		name_idx;

# ifdef _HIGH_LEVEL_IF_FORM
# ifdef KEY /* Bug 10177 */
   int		curr_sh = 0;
# else /* KEY Bug 10177 */
   int		curr_sh;
# endif /* KEY Bug 10177 */
   int		ir_idx;
# endif



   TRACE (Func_Entry, "end_if_blk", NULL);

   if (err_call) {  /* This is an error situation */
      gen_sh(Before, End_If_Stmt, stmt_start_line, stmt_start_col,
             TRUE, FALSE, FALSE);
      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   }
   else {

#ifdef _HIGH_LEVEL_IF_FORM

      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
      curr_sh = curr_stmt_sh_idx;

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)      = Endif_Opr;
      IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx) = stmt_start_line;
      IR_COL_NUM(ir_idx)  = stmt_start_col;

      SH_IR_IDX(curr_sh) = ir_idx;
#endif

      if (cmd_line_flags.debug_lvl <= Debug_Lvl_1) {  /* -ez -ed -G0 -G1 */
         gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
      }
   }

   error = err_call || CURR_BLK_ERR || SH_ERR_FLG(CURR_BLK_FIRST_SH_IDX);

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_End_If_Stmt, statement_number);
   }



   
   if (CURR_BLK == If_Else_If_Blk  ||  CURR_BLK == If_Else_Blk) {
      SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = CURR_BLK_FIRST_SH_IDX;
   }

   POP_BLK_STK;     /* Pop the Then, Else, or Else if */

   if (CURR_BLK != If_Blk) {

      /* Error blocks got in the way - try to find the If_Blk.		      */

      name_idx = BLK_NAME(blk_stk_idx + 1);

      for (blk_idx = blk_stk_idx;  blk_idx > 0;  --blk_idx) {

         if (BLK_TYPE(blk_idx) == If_Blk  &&  BLK_NAME(blk_idx) == name_idx) {
            blk_idx = move_blk_to_end(blk_idx);
            break;
         }
      }
   }


   /* Before popping the If_Blk, make sure that if the IF-THEN stmt is        */
   /* marked in error, the END IF is too. 				      */

   SH_ERR_FLG(curr_stmt_sh_idx) = error;



   /* Check before popping it because the program could be really messed up   */
   /* which would cause the Block Stack to be equally messed up.	      */

   if (CURR_BLK == If_Blk) {

      if (SH_PARENT_BLK_IDX(curr_stmt_sh_idx) == NULL_IDX) {
         SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = CURR_BLK_FIRST_SH_IDX;
      }

#ifdef _HIGH_LEVEL_IF_FORM
      if (! error) {
         /* set the right opnd of the Br_True_Opr of the If to point */
         /* to point to curr stmt. */

# if defined(_DEBUG)
         if (IR_OPR(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) != Br_True_Opr) {
            PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                     "Br_True_Opr", "end_if_blk");
         }
# endif

         IR_FLD_R(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) = SH_Tbl_Idx;
         IR_IDX_R(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) = curr_sh;

         /* have the left opnd of the Endif_Opr point to the If stmt */
         IR_FLD_L(SH_IR_IDX(curr_sh)) = SH_Tbl_Idx;
         IR_IDX_L(SH_IR_IDX(curr_sh)) = CURR_BLK_FIRST_SH_IDX;
      }
# endif
      POP_BLK_STK;
   }


   TRACE (Func_Exit, "end_if_blk", NULL);

   return;

}  /* end_if_blk */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for an INTERFACE block.  *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_interface_blk(boolean	err_call)

{
   int		attr_idx;
   boolean	found;
   int		interface_idx;
   int		sn_idx;


   TRACE (Func_Entry, "end_interface_blk", NULL);

   /* Do not need to mess with scope here, because we are in the parent's */
   /* scope.  The INTERFACE and the END INTERFACE routines do not cause   */
   /* a new scope to be entered.  It is the FUNCTION/END FUNCTION and the */
   /* SUBROUTINE/END SUBROUTINE that cause new scopes and delete old      */
   /* interface scopes.                                                   */

   /* Make sure that if the interface block shares a name with a pgm unit */
   /* that the pgm unit is in the generic interface.                      */

   if (CURR_BLK_NAME != NULL_IDX && 
       ATI_PROC_IDX(CURR_BLK_NAME) != NULL_IDX &&
       !AT_DCL_ERR(ATI_PROC_IDX(CURR_BLK_NAME)) && !err_call) {
      attr_idx	= ATI_PROC_IDX(CURR_BLK_NAME);
      found	= FALSE;
      sn_idx	= ATI_FIRST_SPECIFIC_IDX(CURR_BLK_NAME);

      while (sn_idx != NULL_IDX) {

         if (attr_idx == SN_ATTR_IDX(sn_idx)) {
            found = TRUE;
            break;
         }

         sn_idx = SN_SIBLING_LINK(sn_idx);
      }

      if (!found) {
         AT_DCL_ERR(attr_idx)		= TRUE;
         AT_DCL_ERR(CURR_BLK_NAME)	= TRUE;
         PRINTMSG(AT_DEF_LINE(attr_idx), 713, Error, AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx));
      }
   }

   if (!SCP_IN_ERR(curr_scp_idx)) {
      interface_idx = (BLK_NAME(blk_stk_idx) == NULL_IDX) ?
                       BLK_UNNAMED_INTERFACE(blk_stk_idx):BLK_NAME(blk_stk_idx);

      if (!AT_DCL_ERR(interface_idx) && ATI_HAS_NON_MOD_PROC(interface_idx) &&
          BLK_AT_IDX(blk_stk_idx) != NULL_IDX) {
         collapse_interface_blk(interface_idx);
         ATI_HAS_NON_MOD_PROC(interface_idx) = FALSE;
      }
   }

   if (cif_flags & BASIC_RECS) {
      cif_end_scope_rec();
   }

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_End_Interface_Stmt, statement_number);
   }

   POP_BLK_STK;

   TRACE (Func_Exit, "end_interface_blk", NULL);

   return;

}  /* end_interface_blk */


#ifdef KEY /* Bug 10572 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for an enum block.  *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_enum_blk(boolean	err_call)

{
   int		attr_idx;
   boolean	found;
   int		enum_idx;
   int		sn_idx;


   TRACE (Func_Entry, "end_enum_blk", NULL);

   if (BLK_ENUM_EMPTY(blk_stk_idx) && !(BLK_ERR(blk_stk_idx) || err_call)) {
     PRINTMSG(stmt_start_line, 197, Error, stmt_start_col, "ENUMERATOR",
       "END ENUM");
   }

   POP_BLK_STK;

   TRACE (Func_Exit, "end_enum_blk", NULL);

}  /* end_enum_blk */

#endif /* KEY Bug 10572 */
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Complete the processing of the END statement for a contains block.    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

static void end_contains(boolean	err_call)

{
   TRACE (Func_Entry, "end_contains", NULL);

   POP_BLK_STK;                                 /* Pop the contains */

   /* Illegal to have a CONTAINS with nothing in it.  The only way this   */
   /* check will not work, is if there have been previous block, scoping  */
   /* errors with an interface block.  If that happens, the program is so */
   /* messed up, this check wouldn't matter much.  In all valid cases,    */
   /* SCP_FIRST_CHILD_IDX will be clear at the CONTAINS statement, and    */
   /* must be set by having a least one routine inside the CONTAINS block.*/

   if (SCP_FIRST_CHILD_IDX(curr_scp_idx) == NULL_IDX && 
       !SCP_IN_ERR(curr_scp_idx)) {
      PRINTMSG(stmt_start_line, 387, Error, stmt_start_col);
   }

   end_of_contains = TRUE;

   TRACE (Func_Exit, "end_contains", NULL);

   return;

}  /* end_contains */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for an TYPE block.       *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_type_blk(boolean	err_call)

{
   boolean		aligned;
   size_offset_type	bit_len;

# if defined(_TARGET_DOUBLE_ALIGN)
   int			i;
   int			sn_idx;
# endif


   TRACE (Func_Entry, "end_type_blk", NULL);

   if (err_call) {

      /* Intentionally blank. */
   }
   else if (ATT_NUM_CPNTS(CURR_BLK_NAME) == 0) {
      PRINTMSG(CURR_BLK_DEF_LINE, 290, Error, CURR_BLK_DEF_COLUMN,
               AT_OBJ_NAME_PTR(CURR_BLK_NAME));
   }
   else {

# ifdef _DEBUG
      if (!ATT_CHAR_CPNT(CURR_BLK_NAME) &
          !ATT_NUMERIC_CPNT(CURR_BLK_NAME) &
#ifdef KEY /* Bug 6845 */
          !ATT_ALLOCATABLE_CPNT(CURR_BLK_NAME) &
#endif /* KEY Bug 6845 */
          !ATT_POINTER_CPNT(CURR_BLK_NAME)) {
         PRINTMSG(stmt_start_line, 193, Internal, stmt_start_col,
                  FALSE,
#ifdef KEY /* Bug 6845 */
                  "ATT_CHAR_CPNT, ATT_NUMERIC_CPNT, ATT_POINTER_CPNT",
#else /* KEY Bug 6845 */
                  "ATT_CHAR_CPNT, ATT_NUMERIC_CPNT, ATT_POINTER_CPNT,"
		  " ATT_ALLOCATABLE_CPNT",
#endif /* KEY Bug 6845 */
                  CURR_BLK_NAME);
      }
# endif

      ATT_CHAR_SEQ(CURR_BLK_NAME) = !ATT_NUMERIC_CPNT(CURR_BLK_NAME) &&
#ifdef KEY /* Bug 6845 */
                                    !ATT_ALLOCATABLE_CPNT(CURR_BLK_NAME) &&
#endif /* KEY Bug 6845 */
                                    !ATT_POINTER_CPNT(CURR_BLK_NAME);

      ATT_DCL_NUMERIC_SEQ(CURR_BLK_NAME) = !ATT_POINTER_CPNT(CURR_BLK_NAME) &&
#ifdef KEY /* Bug 6845 */
				    !ATT_ALLOCATABLE_CPNT(CURR_BLK_NAME) &&
#endif /* KEY Bug 6845 */
				    !ATT_CHAR_CPNT(CURR_BLK_NAME) &&
				    ATT_SEQUENCE_SET(CURR_BLK_NAME);

# if defined(_TARGET_DOUBLE_ALIGN)

      if (!cmd_line_flags.dalign &&
          ATT_DCL_NUMERIC_SEQ(CURR_BLK_NAME) && 
          ATT_DALIGN_ME(CURR_BLK_NAME)) {

         /* These components have been daligned, but cannot be because */
         /* this is a numeric sequence derived type and -a dalign has  */
         /* not been specified on the commandline.  Clear the block    */
         /* length and reassign offsets to all the components.         */

         ATT_DALIGN_ME(CURR_BLK_NAME)	    	= FALSE;
         ATT_STRUCT_BIT_LEN_FLD(CURR_BLK_NAME)	= CN_Tbl_Idx;
         ATT_STRUCT_BIT_LEN_IDX(CURR_BLK_NAME)	= CN_INTEGER_ZERO_IDX;
         sn_idx				    = ATT_FIRST_CPNT_IDX(CURR_BLK_NAME);

         for (i = 0; i < ATT_NUM_CPNTS(CURR_BLK_NAME); i++) {
            ATD_CPNT_OFFSET_IDX(SN_ATTR_IDX(sn_idx))	= CN_INTEGER_ZERO_IDX;
            ATD_OFFSET_FLD(SN_ATTR_IDX(sn_idx))		= CN_Tbl_Idx;
            assign_offset(SN_ATTR_IDX(sn_idx)); /* reassign to components */
            sn_idx = SN_SIBLING_LINK(sn_idx);
         }
      }
# endif

      if (ATT_NUMERIC_CPNT(CURR_BLK_NAME) || ATT_POINTER_CPNT(CURR_BLK_NAME)
#ifdef KEY /* Bug 6845 */
        || ATT_ALLOCATABLE_CPNT(CURR_BLK_NAME)
#endif /* KEY Bug 6845 */
      ) {
         bit_len.fld	= ATT_STRUCT_BIT_LEN_FLD(CURR_BLK_NAME);
         bit_len.idx	= ATT_STRUCT_BIT_LEN_IDX(CURR_BLK_NAME);
         aligned	= FALSE;

# if defined(_TARGET_PACK_HALF_WORD_TYPES)

# ifdef _WHIRL_HOST64_TARGET64
         {
            /* check to see if all components have alignment restrictions
             * less than or equal to Align_32.
             * If they do, then change alignment of structure to 32
             * bits.
             */

            int     i;
            int	    sn_idx;
            boolean use_align_32 = TRUE;

            sn_idx = ATT_FIRST_CPNT_IDX(CURR_BLK_NAME);

            for (i = 0; i < ATT_NUM_CPNTS(CURR_BLK_NAME); i++) {
               if (ATD_ALIGNMENT(SN_ATTR_IDX(sn_idx)) > Align_32) {
                  use_align_32 = FALSE;
                  break;
               }
               sn_idx = SN_SIBLING_LINK(sn_idx);
            }

            if (use_align_32)
               ATT_ALIGNMENT(CURR_BLK_NAME) = Align_32;
         }
# endif /* _WHIRL_HOST64_TARGET64 */

         if (!aligned && ATT_ALIGNMENT(CURR_BLK_NAME) == Align_32) {

            /* This structure contains all 32 bit components */
            /* or structures made up of 32 bit components.   */

            align_bit_length(&bit_len, TARGET_BITS_PER_WORD/2);
            aligned	= TRUE;
         }
# endif

         if (!aligned) {

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

            switch(ATT_ALIGNMENT(CURR_BLK_NAME)) {
            case Align_Bit:
               break;           /* Should not see character here */

/* For the forseeable future we are going to pad integer(kind=1) and */
/* integer(kind=2) to full words.                                    */

            case Align_8:
            case Align_16:
               align_bit_length(&bit_len, TARGET_BITS_PER_WORD);
               break;

            case Align_32:
               align_bit_length(&bit_len, 32);
               break;

            case Align_64:
               align_bit_length(&bit_len, 64);
               break;

            case Align_Double:
            case Align_128:
               align_bit_length(&bit_len, 128);
               break;
            }
# else
            align_bit_length(&bit_len, TARGET_BITS_PER_WORD);
# endif
         }

         if (bit_len.fld == NO_Tbl_Idx) {
            ATT_STRUCT_BIT_LEN_FLD(CURR_BLK_NAME) = CN_Tbl_Idx;
            ATT_STRUCT_BIT_LEN_IDX(CURR_BLK_NAME) = ntr_const_tbl(
                                                         bit_len.type_idx,
                                                         FALSE,
                                                         bit_len.constant);
         }
         else {
            ATT_STRUCT_BIT_LEN_FLD(CURR_BLK_NAME) = bit_len.fld;
            ATT_STRUCT_BIT_LEN_IDX(CURR_BLK_NAME) = bit_len.idx;
         }
      }

# if defined(_TARGET_DOUBLE_ALIGN)

      if (ATT_DALIGN_ME(CURR_BLK_NAME)) {

         /* This structure has been daligned.  Make sure the total   */
         /* length is a double length, so that elements of arrays of */
         /* this structure are double aligned.                       */

         bit_len.fld	= ATT_STRUCT_BIT_LEN_FLD(CURR_BLK_NAME);
         bit_len.idx	= ATT_STRUCT_BIT_LEN_IDX(CURR_BLK_NAME);

         align_bit_length(&bit_len, (TARGET_BITS_PER_WORD * 2));

         if (bit_len.fld == NO_Tbl_Idx) {
            ATT_STRUCT_BIT_LEN_FLD(CURR_BLK_NAME) = CN_Tbl_Idx;
            ATT_STRUCT_BIT_LEN_IDX(CURR_BLK_NAME) = ntr_const_tbl(
                                                        bit_len.type_idx,
                                                        FALSE,
                                                        bit_len.constant);
         }
         else {
            ATT_STRUCT_BIT_LEN_FLD(CURR_BLK_NAME) = bit_len.fld;
            ATT_STRUCT_BIT_LEN_IDX(CURR_BLK_NAME) = bit_len.idx;
         }
      }
# endif

   }
    
   /* Note that no Attr entry is created for the label (if any) on an END     */
   /* TYPE statement here because check_for_dup_derived_type_lbl in           */
   /* p_driver.c makes sure that the label on the END TYPE statement does     */
   /* not duplicate any other label in the derived type and creates the Attr  */
   /* entry for the label.						      */


   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_End_Type_Stmt, statement_number);
   }

   POP_BLK_STK;

   TRACE (Func_Exit, "end_type_blk", NULL);

   return;

}  /* end_type_blk */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Perform tasks that must be done at the end of a loop.  This	      *|
|*      procedure is called by both end_do_blk and end_labeled_do.	      *|
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

static void loop_end_processing()

{
   int		attr_idx;
   int		ir_idx;
   int		save_curr_stmt_sh_idx; 
   int		sh_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN) 
   int		blk_idx;
# endif


   TRACE (Func_Entry, "loop_end_processing", NULL);


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN) 

   /* If the current loop is in a loop nest preceded by a BLOCKABLE directive,*/
   /* make sure all the loops in the DO-variable list of the directive are    */
   /* accounted for.  This means if this is the first loop ending, the value  */
   /* of BLK_BLOCKABLE_NUM_LCVS in the current Block Stack entry had better   */
   /* 1.  If not, issue an error, and clear all knowledge that the loop nest  */
   /* was subject to a BLOCKABLE directive so that no other processing will   */
   /* take place.			      				      */

   if (BLK_BLOCKABLE_NUM_LCVS(blk_stk_idx) == 1) {

      for (blk_idx = blk_stk_idx;  blk_idx > 1;  --blk_idx) {

         if (BLK_BLOCKABLE_DIR_SH_IDX(blk_idx) != NULL_IDX) {
            BLK_BLOCKABLE_NEST_OK(blk_idx) = TRUE;
            break;
         }
      }
   }
   else if (BLK_BLOCKABLE_NUM_LCVS(blk_stk_idx) > 1) {

      for (blk_idx = blk_stk_idx;  blk_idx > 1;  --blk_idx) {

         if (BLK_BLOCKABLE_DIR_SH_IDX(blk_idx) != NULL_IDX) {
            break;
         }
      }

      if (! BLK_BLOCKABLE_NEST_OK(blk_idx)) {
         PRINTMSG(stmt_start_line, 1389, Error, 0);

         for (blk_idx = blk_stk_idx;  blk_idx > 1;  --blk_idx) {
            BLK_BLOCKABLE_NUM_LCVS(blk_idx) = 0;

            if (BLK_BLOCKABLE_DIR_SH_IDX(blk_idx) != NULL_IDX) {
               SH_ERR_FLG(BLK_BLOCKABLE_DIR_SH_IDX(blk_idx)) = TRUE;
               BLK_BLOCKABLE_DIR_SH_IDX(blk_idx)             = NULL_IDX;
               break;
            }
         }
      }
   }

# endif


   /* If we're generating the Loop Definition record, output a		      */
   /* Statement_Num_Stmt SH because we need to put the statement number of    */
   /* loop ending statement in the Loop Definition record for Apprentice's    */
   /* use.  								      */
   /*                          SLEAZY TRICK ALERT!!			      */
   /* The statement number is stored in the SH_PARENT_BLK_IDX field.          */
   /* 									      */
   /* If the statement is labeled and it's not an END DO stmt, it has been    */
   /* completely processed by the time we get here which means its EOS has    */
   /* been eaten which means statement_number has been incremented already.   */
   /* If the loop-ending stmt is an END DO, this procedure is called before   */
   /* the EOS is eaten, so the current statement_number can be used.	      */
   /* As this code stands right now, the Statement_Num SH will be inserted    */
   /* between the statement's SH and its Label_Def SH.  If proves to be a     */
   /* problem, then insert code to search back to find the Label_Def SH and   */
   /* insert the Statement_Num SH ahead of it.				      */
   /* We also use the Statement_Num_Stmt SH to hold the position of the very  */
   /* last character of the DO loop (used in the Loop Definition record).     */

   if (cif_flags & MISC_RECS) {
      save_curr_stmt_sh_idx = 0;

      if (SH_COMPILER_GEN(curr_stmt_sh_idx)) {

         /* The current SH is compiler-generated so it means the loop-ending  */
         /* statement is labeled and that compiler-generated SH's were        */
         /* produced to represent the current statement.  We need to back up  */
         /* to the statement's actual SH and insert the Statement_Num SH      */
         /* ahead of it.  Otherwise, the Statement_Num SH would get buried    */
         /* among the compiler-generated SH's and possibly get unlinked       */
         /* (tossed) later.						      */

         sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         while (! SH_LABELED(sh_idx)) {
            sh_idx = SH_PREV_IDX(sh_idx);
         }

         save_curr_stmt_sh_idx = curr_stmt_sh_idx;
         curr_stmt_sh_idx      = sh_idx;
      }
         
      if (SH_LABELED(curr_stmt_sh_idx)  &&  stmt_type != End_Do_Stmt) {
         gen_sh(Before, Statement_Num_Stmt, stmt_end_line, stmt_end_col,
                FALSE, FALSE, TRUE);
         SH_PARENT_BLK_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) =
            statement_number - 1;
      }
      else {
         gen_sh(Before, Statement_Num_Stmt, LA_CH_LINE, LA_CH_COLUMN - 1,
                FALSE, FALSE, TRUE);
         SH_PARENT_BLK_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = statement_number;
      }

      if (save_curr_stmt_sh_idx != 0) {
         curr_stmt_sh_idx = save_curr_stmt_sh_idx;
      }
   }


   /* If a CYCLE exists that references the name of the current loop,         */
   /* generate a CONTINUE statement to define the CYCLE point label.          */
   /* Note:  Obtaining the label would be quicker (but at the expense of a    */
   /* larger Block Stack entry) if the label was generated and its index      */
   /* saved in the Block Stack entry when the CYCLE statement is parsed.      */
   /* Another alternative is to just always generate the label (don't even    */
   /* have a flag in the Block Stack entry).                                  */

   if (BLK_CYCLE_STMT(blk_stk_idx)) {
      attr_idx = gen_loop_lbl_name(blk_stk_idx, Cycle_Lbl);

      gen_sh(After, Continue_Stmt, stmt_start_line, stmt_start_col,
             FALSE, TRUE, TRUE);

      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      IR_OPR(ir_idx)              = Label_Opr;
      IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)         = stmt_start_line;
      IR_COL_NUM(ir_idx)          = stmt_start_col;
      IR_LINE_NUM_L(ir_idx)       = stmt_start_line;
      IR_COL_NUM_L(ir_idx)        = stmt_start_col;
      IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)            = attr_idx;

      AT_DEFINED(attr_idx)        = TRUE;
      AT_DEF_LINE(attr_idx)       = stmt_start_line;
      AT_REFERENCED(attr_idx)     = Referenced;
      ATL_DEF_STMT_IDX(attr_idx)  = curr_stmt_sh_idx;
      ATL_CYCLE_LBL(attr_idx)     = TRUE;
   }


# ifdef _HIGH_LEVEL_DO_LOOP_FORM

   gen_sh(After, Continue_Stmt, stmt_start_line, stmt_start_col,
          FALSE,		/* Set SH_ERR_FLAG to this value. */
          FALSE,		/* Not labeled.			  */
          TRUE);		/* Compiler-generated.		  */

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx)   = ir_idx;
   SH_LOOP_END(curr_stmt_sh_idx) = TRUE;
   IR_OPR(ir_idx)                = Loop_End_Opr;
   IR_TYPE_IDX(ir_idx)           = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)           = stmt_start_line;
   IR_COL_NUM(ir_idx)            = stmt_start_col;

   SH_LOOP_END(curr_stmt_sh_idx)       = TRUE;
   SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = CURR_BLK_FIRST_SH_IDX;
 
   IR_FLD_L(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) = SH_Tbl_Idx;
   IR_IDX_L(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) = curr_stmt_sh_idx;


# endif


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
       
   /* If the current loop is in a loop nest preceded by an INTERCHANGE,       */
   /* PDO, PARALLELDO, or DOACROSS directive, check to see if the current     */
   /* loop ending is perfectly nested in its containing loop.		      */

   if (BLK_INTERCHANGE_NUM_LCVS(blk_stk_idx) > 1  ||
       BLK_DIR_NEST_CHECK_NUM_LCVS(blk_stk_idx) > 1) {
      check_loop_bottom_nesting();
   }

# endif


   /* If an EXIT exists that references the name of the current loop,         */
   /* generate a CONTINUE statement to define the EXIT point label.           */
   /* Note:  Obtaining the label would be quicker (but at the expense of a    */
   /* larger Block Stack entry) if the label was generated and its index      */
   /* saved in the Block Stack entry when the EXIT statement is parsed.       */
   /* Another alternative is to just always generate the label (don't even    */
   /* have a flag in the Block Stack entry).                                  */

   if (BLK_EXIT_STMT(blk_stk_idx)) {
      attr_idx = gen_loop_lbl_name(blk_stk_idx, Exit_Lbl);

      gen_sh(After, Continue_Stmt, stmt_start_line, stmt_start_col,
             FALSE, TRUE, TRUE);

#ifndef _HIGH_LEVEL_DO_LOOP_FORM

      /* (For a high level iterative DO IR form, these links are on the       */
      /* Loop_End SH).							      */

      SH_LOOP_END(curr_stmt_sh_idx)       = TRUE;
      SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = CURR_BLK_FIRST_SH_IDX;

      IR_FLD_L(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) = SH_Tbl_Idx;
      IR_IDX_L(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) = curr_stmt_sh_idx;

#endif

      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      IR_OPR(ir_idx)              = Label_Opr;
      IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)         = stmt_start_line;
      IR_COL_NUM(ir_idx)          = stmt_start_col;
      IR_LINE_NUM_L(ir_idx)       = stmt_start_line;
      IR_COL_NUM_L(ir_idx)        = stmt_start_col;
      IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)            = attr_idx;

      AT_DEFINED(attr_idx)        = TRUE;
      AT_DEF_LINE(attr_idx)       = stmt_start_line;
      AT_REFERENCED(attr_idx)     = Referenced;
      ATL_DEF_STMT_IDX(attr_idx)  = curr_stmt_sh_idx;
   }


   /* When generating IR for low-level DO loop forms, always generate a       */
   /* CONTINUE statement to define the end of the loop (even if the loop is   */
   /* an infinite DO or "one-trip" DO loops were specified).  We need this    */
   /* SH to find the actual end of the loop because if a loop ends with an    */
   /* executable stmt, that stmt may get expanded into multiple SH's.  This   */
   /* CONTINUE statement can also be used to define the branch-around label.  */

# ifndef _HIGH_LEVEL_DO_LOOP_FORM

   gen_sh(After, Continue_Stmt, stmt_start_line, stmt_start_col,
          FALSE,			/* Set SH_ERR_FLAG to this value.     */
          TRUE,		  		/* Labeled.			      */
          TRUE);			/* Compiler-generated.		      */

   if (BLK_DO_TYPE(blk_stk_idx) != Infinite_Loop) {
      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      IR_OPR(ir_idx)              = Label_Opr;
      IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)         = stmt_start_line;
      IR_COL_NUM(ir_idx)          = stmt_start_col;
      IR_LINE_NUM_L(ir_idx)       = stmt_start_line;
      IR_COL_NUM_L(ir_idx)        = stmt_start_col;
      IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)            = BLK_SKIP_LBL_IDX(blk_stk_idx);
      AT_DEFINED(BLK_SKIP_LBL_IDX(blk_stk_idx))       = TRUE;
      AT_DEF_LINE(BLK_SKIP_LBL_IDX(blk_stk_idx))      = stmt_start_line;
      AT_REFERENCED(BLK_SKIP_LBL_IDX(blk_stk_idx))    = Referenced;
      ATL_DEF_STMT_IDX(BLK_SKIP_LBL_IDX(blk_stk_idx)) = curr_stmt_sh_idx;
   }

   if (! BLK_EXIT_STMT(blk_stk_idx)) {
      SH_LOOP_END(curr_stmt_sh_idx)       = TRUE;
      SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = CURR_BLK_FIRST_SH_IDX;
 
      IR_FLD_L(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) = SH_Tbl_Idx;
      IR_IDX_L(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) = curr_stmt_sh_idx;
   }

# endif


   TRACE (Func_Exit, "loop_end_processing", NULL);

   return;

}  /* loop_end_processing */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Called if bad block is found on the block stack.                      *|
|*	Issues internal error message and aborts compilation.                 *|
|*									      *|
|* Input parameters:							      *|
|*	err_call => Boolean - TRUE if this is called from an error situation. *|
|*	            This means the compiler is trying to clean up the block   *|
|*	            stack and do error recovery.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void end_internal_err(boolean	err_call)

{
   TRACE (Func_Entry, "end_internal_err", NULL);

   PRINTMSG(stmt_start_line, 160, Internal, stmt_start_col);

   TRACE (Func_Exit, "end_internal_err", NULL);

   return;

}  /* end_internal_err */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Sets char ptrs for issuing meaningful end block error msgs.	      *|
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

static char *blk_desc_str(int	 blk_idx)
{
#ifdef KEY /* Bug 10177 */
   char		*blk_stmt_str = 0;
#else /* KEY Bug 10177 */
   char		*blk_stmt_str;
#endif /* KEY Bug 10177 */
   int		 idx;

   TRACE (Func_Entry, "blk_desc_str", NULL);

   /* The cases in the following switch statement are in the same order as    */
   /* the values in blk_cntxt_values in p_globals.h.			      */

   switch (BLK_TYPE(blk_idx)) {

      case Unknown_Blk:
         PRINTMSG(stmt_start_line, 160, Internal, stmt_start_col);
         break;

      case Blockdata_Blk:
         blk_stmt_str = "BLOCKDATA";
         break;

      case Module_Blk:
         blk_stmt_str = "MODULE";
         break;

      case Program_Blk:
         blk_stmt_str = "PROGRAM";
         break;

      case Function_Blk:
         blk_stmt_str = "FUNCTION";
         break;

      case Subroutine_Blk:
         blk_stmt_str = "SUBROUTINE";
         break;

      case Internal_Blk:
      case Module_Proc_Blk:
      case Interface_Body_Blk:
         blk_stmt_str = (ATP_PGM_UNIT(BLK_NAME(blk_idx)) == Function) ?
                         "FUNCTION" :  "SUBROUTINE";
         break;

      case Do_Blk:
         blk_stmt_str = "DO";
         break;

      case Forall_Blk:
         blk_stmt_str = "FORALL";
         break;

      case If_Blk:
      case If_Then_Blk:
      case If_Else_If_Blk:
      case If_Else_Blk:
         blk_stmt_str = "IF";
         break;

      case Select_Blk:
      case Case_Blk:
         blk_stmt_str = "SELECT CASE";
         break;

      case Where_Then_Blk:
      case Where_Else_Blk:
      case Where_Else_Mask_Blk:
         blk_stmt_str = "WHERE";
         break;

      case Parallel_Blk:
         blk_stmt_str = "PARALLEL";
         break;

      case Doall_Blk:
         blk_stmt_str = "DOALL";
         break;

      case Do_Parallel_Blk:
         blk_stmt_str = "DO PARALLEL";
         break;

      case Guard_Blk:
         blk_stmt_str = "GUARD";
         break;

      case Parallel_Case_Blk:
         blk_stmt_str = "CASE";
         break;

      case Wait_Blk:
         blk_stmt_str = "WAIT";
         break;

      case SGI_Doacross_Blk:
         blk_stmt_str = "DOACROSS";
         break;

      case SGI_Psection_Blk:
         blk_stmt_str = "PSECTION";
         break;

      case SGI_Section_Blk:
         blk_stmt_str = "SECTION";
         break;

      case SGI_Pdo_Blk:
         blk_stmt_str = "PDO";
         break;

      case SGI_Parallel_Do_Blk:
         blk_stmt_str = "PARALLEL DO";
         break;

      case SGI_Parallel_Blk:
         blk_stmt_str = "PARALLEL";
         break;

      case SGI_Critical_Section_Blk:
         blk_stmt_str = "CRITICAL SECTION";
         break;

      case SGI_Single_Process_Blk:
         blk_stmt_str = "SINGLE PROCESS";
         break;

      case SGI_Region_Blk:
         blk_stmt_str = "REGION";
         break;

      case Open_Mp_Parallel_Blk:
         blk_stmt_str = "!$OMP PARALLEL";
         break;

      case Open_Mp_Do_Blk:
         blk_stmt_str = "!$OMP DO";
         break;

      case Open_Mp_Parallel_Sections_Blk:
         blk_stmt_str = "!$OMP PARALLEL SECTIONS";
         break;

      case Open_Mp_Sections_Blk:
         blk_stmt_str = "!$OMP SECTIONS";
         break;

      case Open_Mp_Section_Blk:
         blk_stmt_str = "!$OMP SECTION";
         break;

      case Open_Mp_Single_Blk:
         blk_stmt_str = "!$OMP SINGLE";
         break;

      case Open_Mp_Parallel_Do_Blk:
         blk_stmt_str = "!$OMP PARALLEL DO";
         break;

      case Open_Mp_Master_Blk:
         blk_stmt_str = "!$OMP MASTER";
         break;

      case Open_Mp_Critical_Blk:
         blk_stmt_str = "!$OMP CRITICAL";
         break;

      case Open_Mp_Ordered_Blk:
         blk_stmt_str = "!$OMP ORDERED";
         break;

      case Contains_Blk:
         for (idx = blk_idx;
              idx > NULL_IDX && (BLK_TYPE(idx) > Blockdata_Blk);
              idx--);

         /* First entry will always be at least $MAIN */

         blk_stmt_str = blk_desc_str(idx);
         break;

      case Interface_Blk:
         blk_stmt_str = "INTERFACE";
         break;

#ifdef KEY /* Bug 10572 */
      case Enum_Blk:
         blk_stmt_str = "ENUM";
         break;
#endif /* KEY Bug 10572 */

      case Derived_Type_Blk:
         blk_stmt_str = "TYPE";
         break;

   }  /* End switch */

   TRACE (Func_Exit, "blk_desc_str", NULL);
   return(blk_stmt_str);

}  /* blk_desc_str */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This issues the error message if there is a block matching error.     *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

int	blk_match_err(blk_cntxt_type	blk_type,
		      boolean		has_name,
		      boolean		all_match)

{
   int			blk_idx;
   boolean		name_err	= FALSE;
   pgm_unit_type	pgm_type;


   TRACE (Func_Entry, "blk_match_err", NULL);

   if (stmt_type == End_Subroutine_Stmt || stmt_type == End_Function_Stmt) {
   
      /* Need to search for Internal_Blk, Module_Proc_Blk, Interface_Body_Blk */
      /* Subroutine_Blk or Function_Blk                                       */

      pgm_type = (stmt_type == End_Function_Stmt) ? Function : Subroutine;

      /* Check to see if this matchs the first block.  If it does, then only  */
      /* the name is in error.                                                */

      name_err = (STMT_LEGAL_IN_BLK(stmt_type, CURR_BLK) &&
                  ATP_PGM_UNIT(CURR_BLK_NAME) == pgm_type);

      for (blk_idx = blk_stk_idx; blk_idx > NULL_IDX; blk_idx--) {

         if (STMT_LEGAL_IN_BLK(stmt_type, BLK_TYPE(blk_idx)) &&
             ATP_PGM_UNIT(BLK_NAME(blk_idx)) == pgm_type) {

            if (!has_name || 
                (compare_names(TOKEN_ID(token).words,
                               TOKEN_LEN(token),
                               AT_OBJ_NAME_LONG(BLK_NAME(blk_idx)),
                               AT_NAME_LEN(BLK_NAME(blk_idx))) == 0)) {
               break;   /* Found the match */
            }
         }
      }
   }
   else {
      name_err = STMT_LEGAL_IN_BLK(stmt_type, CURR_BLK);

      for (blk_idx = blk_stk_idx; blk_idx > NULL_IDX; blk_idx--) {

         if (STMT_LEGAL_IN_BLK(stmt_type, BLK_TYPE(blk_idx))) {

            if (stmt_type == Else_If_Stmt  ||  stmt_type == Else_Stmt  ||
                stmt_type == Case_Stmt) {
               name_err = FALSE;

               if (has_name) {

                  if (BLK_NAME(blk_idx) == NULL_IDX) {
                     PRINTMSG(TOKEN_LINE(token), 285, Error,
			      TOKEN_COLUMN(token),
                              blk_desc_str(blk_idx),
			      stmt_type_str[stmt_type]);
                  }
                  else if (compare_names(TOKEN_ID(token).words,
				  	 TOKEN_LEN(token),
                                         AT_OBJ_NAME_LONG(BLK_NAME(blk_idx)),
                                         AT_NAME_LEN(BLK_NAME(blk_idx))) != 0) {
                     PRINTMSG(TOKEN_LINE(token), 284, Error,
                              TOKEN_COLUMN(token),
                              blk_desc_str(blk_idx),
                              AT_OBJ_NAME_PTR(BLK_NAME(blk_idx)),
                              stmt_type_str[stmt_type]);
                  }
               }

               break;
            }
            else {

               if (!has_name) {

                  /* If all_match is set, then if the block has a name, the   */
                  /* thing that's looking for the match must have the same    */
                  /* name.						      */

                  if (!all_match || BLK_NAME(blk_idx) == NULL_IDX) {
                     break;
                  }
               }
               else if (BLK_NAME(blk_idx) != NULL_IDX && 
                        (compare_names(TOKEN_ID(token).words,
                                       TOKEN_LEN(token),
                                       AT_OBJ_NAME_LONG(BLK_NAME(blk_idx)),
                                       AT_NAME_LEN(BLK_NAME(blk_idx))) == 0)) {
                  break;
               }
            }
         }
      }
   }

   if (blk_idx == NULL_IDX && name_err) {

      /* Didn't find a match on the block, so if the statement fits in the    */
      /* current block, assume there is just a name error and issue it.       */

      /* Possible combinations:                                               */
      /*   CURR_BLK_NAME       has_name        stmt_type                      */
      /*     NULL_IDX            TRUE       all stmts                         */
      /*     NULL_IDX            FALSE      Won't be here.  This is a match.  */
      /*     have name           TRUE       Must be name mismatch             */
      /*     have name           FALSE      Only err for END of IF, DO, SELCT */

      switch (stmt_type) {
         case End_If_Stmt:
         case End_Do_Stmt:
         case End_Select_Stmt:
         case Else_If_Stmt:
         case Else_Stmt:
         case Then_Stmt:
         case Case_Stmt:
         case End_Forall_Stmt:
         case Else_Where_Stmt:
         case Else_Where_Mask_Stmt:
         case End_Where_Stmt:
            if (CURR_BLK_NAME == NULL_IDX) {
               PRINTMSG(TOKEN_LINE(token), 285, Error, TOKEN_COLUMN(token),
                        blk_desc_str(blk_stk_idx), stmt_type_str[stmt_type]);
            }
            else {
               PRINTMSG((has_name) ? TOKEN_LINE(token) : stmt_start_line,
                        284, Error,
                        (has_name) ? TOKEN_COLUMN(token) : stmt_start_col,
                        blk_desc_str(blk_stk_idx),
                        AT_OBJ_NAME_PTR(CURR_BLK_NAME),
                        stmt_type_str[stmt_type]);
            }
            break;

         case End_Blockdata_Stmt:
         case End_Program_Stmt:
            if (CURR_BLK_NAME == NULL_IDX) {

               if (stmt_type == End_Blockdata_Stmt) { /* No BLOCKDATA name */
                  PRINTMSG(TOKEN_LINE(token), 158, Error,
                           TOKEN_COLUMN(token));
               }
               else {  /* No PROGRAM statement */
                  PRINTMSG(TOKEN_LINE(token), 40, Error, 
                           TOKEN_COLUMN(token));
               }
   
               /* We've found an END BLOCK DATA or (more likely) END PROGRAM  */
               /* stmt that has no matching BLOCK DATA or (more likely)       */
               /* PROGRAM stmt.  blk_match_err is going to call the           */
               /* appropriate end-of-program unit routine which will produce  */
               /* the CIF End Scope and End Unit records.  They will go in    */
               /* the temporary CIF.  When control returns to p_driver.c, it  */
               /* will think a new program unit is beginning so it will call  */
               /* cif_unit_rec which will produce the Unit record then copy   */
               /* the temporary CIF to the actual CIF.  This means that the   */
               /* End Scope and End Unit will get copied to the actual CIF.   */
               /* THEN p_driver.c will call cif_scope_rec to issue the Begin  */
               /* Scope record, which will, of course, now be out of order.   */
               /* (Not to mention that the end-of-program unit routine has    */
               /* reset blk_stk_idx to 0 which means the CIF routines will    */
               /* be accessing the 0th (bad) Block Stack entry.  To recover   */
               /* from this weird case, set cif_pgm_unit_error_recovery to    */
               /* TRUE here.  When cif_unit_rec sees it is TRUE, it won't     */
               /* copy the temporary CIF to the actual CIF.  When p_driver.c  */
               /* sees its TRUE, it'll produce the Begin Scope record, THEN   */
               /* copy the temporary CIF to the actual CIF.		      */

               cif_pgm_unit_error_recovery = TRUE;

               break;
            }

            /* Break intentionally not here.  Should fall through.            */

         case End_Module_Stmt:
         case End_Function_Stmt:
         case End_Subroutine_Stmt:
         case End_Interface_Stmt:
         case End_Type_Stmt: /* These must have a CURR_BLK_NAME */
            PRINTMSG(TOKEN_LINE(token), 283, Error, TOKEN_COLUMN(token),
                     stmt_type_str[stmt_type],
                     blk_desc_str(blk_stk_idx),
                     AT_OBJ_NAME_PTR(CURR_BLK_NAME));
            break;

#ifdef KEY /* Bug 10572 */
         case End_Enum_Stmt:
	    PRINTMSG(TOKEN_LINE(token), 197, Error, TOKEN_COLUMN(token),
	      EOS_STR, TOKEN_STR(token));
	    break;
#endif /* KEY Bug 10572 */
# ifdef _DEBUG
         default:
            PRINTMSG(stmt_start_line, 179, Internal,
                     stmt_start_col, "blk_match_err");
            break;
# endif
      }  /* End switch */
      SH_ERR_FLG(curr_stmt_sh_idx)	= TRUE;
      blk_idx				= blk_stk_idx;
   }
   else if ((stmt_type == Else_Stmt  &&  CURR_BLK == If_Else_Blk)  ||
            (stmt_type == Else_Where_Stmt  &&  CURR_BLK == Where_Else_Blk) ) {

      /* An IF construct may contain only a single ELSE block and a WHERE     */
      /* construct may contain only a single ELSEWHERE block.                 */

      PRINTMSG(stmt_start_line, 43, Error, stmt_start_col, 
               stmt_type_str[stmt_type], blk_desc_str(blk_stk_idx));
   }
   else if (stmt_type == Else_If_Stmt  &&  CURR_BLK == If_Else_Blk) {

      /* An ELSE IF block must not follow an ELSE block.                      */

      PRINTMSG(stmt_start_line, 1158, Error, stmt_start_col);
   } 
   else if (stmt_type == Else_Where_Mask_Stmt  &&  CURR_BLK == Where_Else_Blk) {

      /* An ELSE WHERE (mask) block must not follow an ELSE WHERE block.      */

      PRINTMSG(stmt_start_line, 1609, Error, stmt_start_col);
   } 
   else if (blk_idx == NULL_IDX) { 

      /* Push the blk_type on the stack, so blk_desc_str can describe it for  */
      /* the error message.  Then POP it.  It is only needed for the message. */

      PUSH_BLK_STK(blk_type);
      PRINTMSG(stmt_start_line, 289, Error, stmt_start_col,
               stmt_type_str[stmt_type], blk_desc_str(blk_stk_idx));
      POP_BLK_STK;

      SH_ERR_FLG(curr_stmt_sh_idx)	= TRUE;
   }
   else {                   /* Have a match, but not to the current block.    */

      pop_and_err_blk_stk(blk_idx, (BLK_TYPE(blk_idx) <= Interface_Body_Blk));

      if (BLK_TYPE(blk_idx) > Interface_Body_Blk && 
          BLK_TYPE(blk_idx) != Select_Blk) {
         blk_idx = move_blk_to_end(blk_idx);
      }
   }

   TRACE (Func_Exit, "blk_match_err", NULL);

   return(blk_idx);

}  /* blk_match_err */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Go through the blk_stk until match_idx is reached.  For each blk,     *|
|*	passed up; issue a missing end err if the err is not already set;     *|
|*	call the appropriate end routine for the block, and then pop it.      *|
|*	When the matched item is reached, just leave it and don't pop it.     *|
|*	It is the responsibility of the calling routine to handle the         *|
|*	match block.                                                          *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE - if error was issued, else FALSE                                *|
|*									      *|
\******************************************************************************/

boolean	pop_and_err_blk_stk(int		match_idx,
			    boolean	pop_the_blks)

{
   int		blk_idx;
   int		blk_line_idx;
   boolean	issued_error = FALSE;
   boolean	save_sh_err_flg;
   int		sh_idx;


   TRACE (Func_Entry, "pop_and_err_blk_stk", NULL);

   blk_idx = blk_stk_idx;

   while (blk_idx > match_idx) {

      if (BLK_TYPE(blk_idx) == Do_Parallel_Blk ||
          BLK_TYPE(blk_idx) == SGI_Pdo_Blk ||
          BLK_TYPE(blk_idx) == Open_Mp_Do_Blk ||
          BLK_TYPE(blk_idx) == Open_Mp_Parallel_Do_Blk) {

         if (BLK_TYPE(blk_idx) == Do_Parallel_Blk) {
            CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
         }
         else if (BLK_TYPE(blk_idx) == SGI_Pdo_Blk) {
            CLEAR_DIRECTIVE_STATE(Pdo_Region);
         }
         else if (BLK_TYPE(blk_idx) == Open_Mp_Do_Blk) {
            CLEAR_DIRECTIVE_STATE(Open_Mp_Do_Region);
         }
         else if (BLK_TYPE(blk_idx) == Open_Mp_Parallel_Do_Blk) {
            CLEAR_DIRECTIVE_STATE(Open_Mp_Parallel_Do_Region);
         }

         move_blk_to_end(blk_idx);
         POP_BLK_STK;
         blk_idx--;
         continue;
      }

      /* Issue missing end msg if it doesn't already have a msg issued for */
      /* it.  (blk_err=TRUE).  Also, don't issue msgs for contains blocks. */

      if (!BLK_ERR(blk_idx) &&
           BLK_TYPE(blk_idx) != Contains_Blk) {

         if (BLK_TYPE(blk_idx) != Program_Blk) {

            save_sh_err_flg = SH_ERR_FLG(curr_stmt_sh_idx);

           /* Missing termination or END statement.                          */

            if (BLK_TYPE(blk_idx) == Do_Blk) {
   
               if (stmt_label_idx == NULL_IDX  || 
                   stmt_label_idx != BLK_LABEL(blk_idx)) {
                  PRINTMSG(BLK_DEF_LINE(blk_idx), 288, Error,
                           BLK_DEF_COLUMN(blk_idx));
                  issued_error = TRUE;
                  SH_ERR_FLG(BLK_FIRST_SH_IDX(blk_idx)) = TRUE;
               }
            }
            else if (BLK_TYPE(blk_idx) == Parallel_Blk) {
               PRINTMSG(BLK_DEF_LINE(blk_idx), 1217, Error,
                        BLK_DEF_COLUMN(blk_idx),
                        "END PARALLEL","PARALLEL");
               issued_error = TRUE;
               SH_ERR_FLG(BLK_FIRST_SH_IDX(blk_idx)) = TRUE;
            }
            else if (BLK_TYPE(blk_idx) == Guard_Blk) {
               PRINTMSG(BLK_DEF_LINE(blk_idx), 1217, Error,
                        BLK_DEF_COLUMN(blk_idx),
                        "END GUARD","GUARD");
               issued_error = TRUE;
               SH_ERR_FLG(BLK_FIRST_SH_IDX(blk_idx)) = TRUE;
            }
            else if (BLK_TYPE(blk_idx) == Parallel_Case_Blk) {
               PRINTMSG(BLK_DEF_LINE(blk_idx), 1217, Error,
                        BLK_DEF_COLUMN(blk_idx),
                        "END CASE","CASE");
               issued_error = TRUE;
               SH_ERR_FLG(BLK_FIRST_SH_IDX(blk_idx)) = TRUE;
            }
            else if (BLK_TYPE(blk_idx) == Wait_Blk) {
               PRINTMSG(BLK_DEF_LINE(blk_idx), 1217, Error,
                        BLK_DEF_COLUMN(blk_idx),
                        "SEND","WAIT");
               issued_error = TRUE;
               SH_ERR_FLG(BLK_FIRST_SH_IDX(blk_idx)) = TRUE;
            }
            else {
               blk_line_idx = blk_idx;

               if (BLK_TYPE(blk_idx) == Case_Blk) {
                  for (; BLK_TYPE(blk_line_idx) != Select_Blk; blk_line_idx--);
               }
               else if (BLK_TYPE(blk_idx) == If_Then_Blk ||
                        BLK_TYPE(blk_idx) == If_Else_If_Blk ||
                        BLK_TYPE(blk_idx) == If_Else_Blk) {
                  do {

                    if (BLK_TYPE(blk_line_idx) == If_Then_Blk ||
                        BLK_TYPE(blk_line_idx) == If_Else_If_Blk ||
                        BLK_TYPE(blk_line_idx) == If_Else_Blk) {
                       BLK_ERR(blk_line_idx)                      = TRUE;
                       SH_ERR_FLG(BLK_FIRST_SH_IDX(blk_line_idx)) = TRUE;
                    }
                    blk_line_idx--;
                 } while (BLK_TYPE(blk_line_idx) != If_Blk);
               }

               if (!BLK_ERR(blk_line_idx)) {  /* Prevent duplicate errors */
                  PRINTMSG(BLK_DEF_LINE(blk_line_idx), 291, Error,
                           BLK_DEF_COLUMN(blk_line_idx), 
                           blk_desc_str(blk_idx));
                  issued_error = TRUE;
                  BLK_ERR(blk_line_idx)                      = TRUE;
                  SH_ERR_FLG(BLK_FIRST_SH_IDX(blk_line_idx)) = TRUE;
               }
            }

            SH_ERR_FLG(curr_stmt_sh_idx) = save_sh_err_flg;

         }
         else {   /* Main program unit is missing its END statement. */

            if (SCP_ATTR_IDX(curr_scp_idx) == glb_tbl_idx[Main_Attr_Idx]) {

               /* This is $MAIN.  Make sure this isn't just a bad end stmt  */
               /* that results from an earlier mess, the compiler is trying */
               /* to clean up.  If so, do not issue an error.               */

               if (!AT_DCL_ERR(glb_tbl_idx[Main_Attr_Idx]) && 
                   !SCP_IN_ERR(curr_scp_idx)) {
                  PRINTMSG(BLK_DEF_LINE(blk_idx), 293, Error,
                           BLK_DEF_COLUMN(blk_idx));
                  issued_error = TRUE;
               }
            } 
            else {
               PRINTMSG(BLK_DEF_LINE(blk_idx), 955, Error,
                        BLK_DEF_COLUMN(blk_idx),
                        AT_OBJ_NAME_PTR(CURR_BLK_NAME));
               issued_error = TRUE;
            } 

            if (need_new_sh) {
               sh_idx				= curr_stmt_sh_idx;
               curr_stmt_sh_idx			= ntr_sh_tbl();
               SH_NEXT_IDX(sh_idx)		= curr_stmt_sh_idx;
               SH_PREV_IDX(curr_stmt_sh_idx)	= sh_idx;
            }

            SH_GLB_LINE(curr_stmt_sh_idx)	= stmt_start_line;
            SH_COL_NUM(curr_stmt_sh_idx)	= stmt_start_col;
            SH_STMT_TYPE(curr_stmt_sh_idx)	= End_Stmt;
         }
      }

      BLK_ERR(blk_idx) = TRUE;

      if (pop_the_blks) { /* Call the end routine for each block to pop. */

         if (BLK_TYPE(blk_idx) == Case_Blk ||
             BLK_TYPE(blk_idx) == If_Then_Blk ||
             BLK_TYPE(blk_idx) == If_Else_If_Blk ||
             BLK_TYPE(blk_idx) == If_Else_Blk) {
            (*end_blocks[BLK_TYPE(blk_idx)]) (TRUE);	/* Pop 2 blocks */
            blk_idx--;
         }
         else {
            (*end_blocks[BLK_TYPE(blk_idx)]) (TRUE);
         }
      }

      /* Always pop the Where_Else_Blk and If_Else_Blk, if another Where_Else */
      /* _Blk or If_Else_Blk follows it, to prevent multiple Else_Blks from   */
      /* getting on the stack.                                                */

      else if (BLK_TYPE(blk_idx) == Where_Else_Blk &&
               (stmt_type == Else_Where_Stmt || 
                stmt_type == Else_Where_Mask_Stmt)) { 
         move_blk_to_end(blk_idx);
         POP_BLK_STK;
      }
      else if (BLK_TYPE(blk_idx) == If_Else_Blk &&
               (stmt_type == Else_Stmt || stmt_type == Else_If_Stmt)) { 
         move_blk_to_end(blk_idx);
         POP_BLK_STK;
      }

      blk_idx--;
   }  /* End while */

   TRACE (Func_Exit, "pop_and_err_blk_stk", NULL);

   return(issued_error);

}  /* pop_and_err_blk_stk */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This procedure moves a block to the end of a block stack.  It's used  *|
|*      for popping bad blocks.                                               *|
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

int move_blk_to_end(int	blk_idx)

{
   int		new_idx;

   TRACE (Func_Entry, "move_blk_to_end", NULL);

   if (blk_idx != blk_stk_idx) {

      PUSH_BLK_STK(BLK_TYPE(blk_idx));    		/* Make a spot for blk*/
      blk_stk[blk_stk_idx]	= blk_stk[blk_idx];	/* Copy block to end */

      if (BLK_TYPE(blk_idx) == Do_Blk &&
          BLK_TYPE(blk_idx - 1) == Doall_Blk) {

         for (new_idx = blk_idx - 1; new_idx < blk_stk_idx - 1; new_idx++) {
             blk_stk[new_idx]	= blk_stk[new_idx + 2];
         }

         POP_BLK_STK;
         POP_BLK_STK;
         cdir_switches.doall_region = FALSE;
      }
      else {
         for (new_idx = blk_idx; new_idx < blk_stk_idx; new_idx++) {
             blk_stk[new_idx]	= blk_stk[new_idx + 1];
         }

         POP_BLK_STK;
      }
   }

   TRACE (Func_Exit, "move_blk_to_end", NULL);

   return(blk_stk_idx);

}  /* move_blk_to_end */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void end_parallel_blk(boolean  err_call)

{

   TRACE (Func_Entry, "end_parallel_blk", NULL);

   if (CURR_BLK == Do_Parallel_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
   }

   if (CURR_BLK == SGI_Pdo_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Pdo_Region);
   }

   if (! err_call) {

      if (cdir_switches.dopar_sh_idx != NULL_IDX) {
         PRINTMSG(SH_GLB_LINE(cdir_switches.dopar_sh_idx), 1219, Error,
                  SH_COL_NUM(cdir_switches.dopar_sh_idx),
                  "DO PARALLEL");
         cdir_switches.dopar_sh_idx = NULL_IDX;
      }

      if (STMT_CANT_BE_IN_BLK(End_Parallel_Stmt, CURR_BLK)) {
         blk_match_err(Parallel_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Parallel_Blk) {
         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_parallel_blk", NULL);

   return;

}  /* end_parallel_blk */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void end_doall_blk(boolean  err_call)

{

   TRACE (Func_Entry, "end_doall_blk", NULL);

   POP_BLK_STK;

   TRACE (Func_Exit, "end_doall_blk", NULL);

   return;

}  /* end_doall_blk */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void end_wait_blk(boolean  err_call)

{

   TRACE (Func_Entry, "end_wait_blk", NULL);

   /* NOTE - This routine should never be called.  Everything is handled */
   /*        in directive.c under the SEND directive.                    */

   POP_BLK_STK;

   TRACE (Func_Exit, "end_wait_blk", NULL);

   return;

}  /* end_wait_blk */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void end_do_parallel_blk(boolean  err_call)

{
   int		sh_idx;


   TRACE (Func_Entry, "end_do_parallel_blk", NULL);

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(End_Do_Parallel_Stmt, CURR_BLK)) {

         if (cdir_switches.dopar_sh_idx != NULL_IDX) {
            PRINTMSG(SH_GLB_LINE(cdir_switches.dopar_sh_idx), 1219, Error,
                     SH_COL_NUM(cdir_switches.dopar_sh_idx),
                     "DO PARALLEL");
            cdir_switches.dopar_sh_idx = NULL_IDX;
         }
         else {
            blk_match_err(Do_Parallel_Blk, FALSE, FALSE);
         }
      }

      if (CURR_BLK == Do_Parallel_Blk) {

         /* remove the enddo statement that was generated at the */
         /* end of the Do Blk. This one supersedes it. */

         if (BLK_ENDDO_PARALLEL_SH_IDX(blk_stk_idx) != NULL_IDX) {
            sh_idx = BLK_ENDDO_PARALLEL_SH_IDX(blk_stk_idx);
            SH_PREV_IDX(SH_NEXT_IDX(sh_idx)) = SH_PREV_IDX(sh_idx);
            SH_NEXT_IDX(SH_PREV_IDX(sh_idx)) = SH_NEXT_IDX(sh_idx);

            FREE_IR_NODE(SH_IR_IDX(sh_idx));
            FREE_SH_NODE(sh_idx);
         }

         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_do_parallel_blk", NULL);

   return;

}  /* end_do_parallel_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_pdo_blk(boolean  err_call)

{
   int          sh_idx;


   TRACE (Func_Entry, "end_pdo_blk", NULL);

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(SGI_End_Pdo_Stmt, CURR_BLK)) {

         if (cdir_switches.pdo_sh_idx != NULL_IDX) {
            PRINTMSG(SH_GLB_LINE(cdir_switches.pdo_sh_idx), 1219, Error,
                     SH_COL_NUM(cdir_switches.pdo_sh_idx),
                     "PDO");
            cdir_switches.pdo_sh_idx = NULL_IDX;
         }
         else {
            blk_match_err(SGI_Pdo_Blk, FALSE, FALSE);
         }
      }

      if (CURR_BLK == SGI_Pdo_Blk) {
         /* remove the enddo statement that was generated at the */
         /* end of the Do Blk. This one supersedes it. */

         if (BLK_ENDPDO_SH_IDX(blk_stk_idx) != NULL_IDX) {
            sh_idx = BLK_ENDPDO_SH_IDX(blk_stk_idx);
            SH_PREV_IDX(SH_NEXT_IDX(sh_idx)) = SH_PREV_IDX(sh_idx);
            SH_NEXT_IDX(SH_PREV_IDX(sh_idx)) = SH_NEXT_IDX(sh_idx);

            FREE_IR_NODE(SH_IR_IDX(sh_idx));
            FREE_SH_NODE(sh_idx);
         }

         IR_LINE_NUM_R(SH_IR_IDX(curr_stmt_sh_idx)) = stmt_start_line;
         IR_COL_NUM_R(SH_IR_IDX(curr_stmt_sh_idx))  = stmt_start_col;
         IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) = SH_Tbl_Idx;
         IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)) = CURR_BLK_FIRST_SH_IDX;

         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_pdo_blk", NULL);

   return;

}  /* end_pdo_blk */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void end_guard_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_guard_blk", NULL);

   if (CURR_BLK == Do_Parallel_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
   }

   if (CURR_BLK == SGI_Pdo_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Pdo_Region);
   }

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(End_Guard_Stmt, CURR_BLK)) {
         blk_match_err(Guard_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Guard_Blk) {
         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_guard_blk", NULL);

   return;

}  /* end_guard_blk */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void end_parallel_case_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_parallel_case_blk", NULL);

   if (CURR_BLK == Do_Parallel_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
   }

   if (CURR_BLK == SGI_Pdo_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Pdo_Region);
   }

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(stmt_type, CURR_BLK)) {
         blk_match_err(Parallel_Case_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Parallel_Case_Blk) {
         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_parallel_case_blk", NULL);

   return;

}  /* end_parallel_case_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_SGI_parallel_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_SGI_parallel_blk", NULL);

   while (blk_stk_idx > 0 &&
          (CURR_BLK == Do_Parallel_Blk ||
           CURR_BLK == SGI_Pdo_Blk     ||
           CURR_BLK == SGI_Psection_Blk ||
           CURR_BLK == SGI_Section_Blk ||
           CURR_BLK == SGI_Single_Process_Blk ||
           CURR_BLK == SGI_Critical_Section_Blk)) {

      switch (CURR_BLK) {
         case Do_Parallel_Blk:
            CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
            break;

         case SGI_Pdo_Blk:
            CLEAR_DIRECTIVE_STATE(Pdo_Region);
            break;

         case SGI_Psection_Blk:
            CLEAR_DIRECTIVE_STATE(Parallel_Section_Region);
            break;

         case SGI_Section_Blk:
            CLEAR_DIRECTIVE_STATE(Parallel_Section_Region);
            break;

         case SGI_Single_Process_Blk:
            CLEAR_DIRECTIVE_STATE(Single_Process_Region);
            break;

         case SGI_Critical_Section_Blk:
            CLEAR_DIRECTIVE_STATE(Critical_Section_Region);
            break;
      }

      /* just pop it */
      POP_BLK_STK;
   }

   if (! err_call) {

      if (cdir_switches.pdo_sh_idx != NULL_IDX) {
         PRINTMSG(SH_GLB_LINE(cdir_switches.pdo_sh_idx), 1219, Error,
                  SH_COL_NUM(cdir_switches.pdo_sh_idx),
                  "PDO");
         cdir_switches.pdo_sh_idx = NULL_IDX;
      }

      if (STMT_CANT_BE_IN_BLK(SGI_End_Parallel_Stmt, CURR_BLK)) {
         blk_match_err(SGI_Parallel_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == SGI_Parallel_Blk) {
         /* point to the PARALLEL directive */
         IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) = SH_Tbl_Idx;
         IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)) = CURR_BLK_FIRST_SH_IDX;

         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_SGI_parallel_blk", NULL);

   return;

}  /* end_SGI_parallel_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_doacross_blk(boolean  err_call)

{

   TRACE (Func_Entry, "end_doacross_blk", NULL);

   POP_BLK_STK;

   TRACE (Func_Exit, "end_doacross_blk", NULL);

   return;

}  /* end_doacross_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_critical_section_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_critical_section_blk", NULL);

   if (CURR_BLK == Do_Parallel_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
   }

   if (CURR_BLK == SGI_Pdo_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Pdo_Region);
   }

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(SGI_End_Critical_Section_Stmt, CURR_BLK)) {
         blk_match_err(SGI_Critical_Section_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == SGI_Critical_Section_Blk) {
         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_critical_section_blk", NULL);

   return;

}  /* end_critical_section_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_psection_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_psection_blk", NULL);

   if (CURR_BLK == Do_Parallel_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
   }

   if (CURR_BLK == SGI_Pdo_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Pdo_Region);
   }

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(SGI_End_Psection_Stmt, CURR_BLK)) {
         blk_match_err(SGI_Section_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == SGI_Section_Blk) {
         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   if (CURR_BLK == SGI_Psection_Blk) {
      /* point to the PSECTION directive */
      IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) = SH_Tbl_Idx;
      IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)) = CURR_BLK_FIRST_SH_IDX;

      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_psection_blk", NULL);

   return;

}  /* end_psection_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_single_process_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_single_process_blk", NULL);

   if (CURR_BLK == Do_Parallel_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
   }

   if (CURR_BLK == SGI_Pdo_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Pdo_Region);
   }

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(SGI_End_Single_Process_Stmt, CURR_BLK)) {
         blk_match_err(SGI_Single_Process_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == SGI_Single_Process_Blk) {
         /* point to the SINGLE PROCESS directive */
         IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) = SH_Tbl_Idx;
         IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)) = CURR_BLK_FIRST_SH_IDX;

         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_single_process_blk", NULL);

   return;

}  /* end_single_process_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_region_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_region_blk", NULL);

   if (CURR_BLK == Do_Parallel_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
   }

   if (CURR_BLK == SGI_Pdo_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Pdo_Region);
   }

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(SGI_Region_End_Stmt, CURR_BLK)) {
         blk_match_err(SGI_Region_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == SGI_Region_Blk) {
         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_region_blk", NULL);

   return;

}  /* end_region_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_open_mp_parallel_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_open_mp_parallel_blk", NULL);

   if (CURR_BLK == Open_Mp_Do_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Open_Mp_Do_Region);
   }

   if (CURR_BLK == Open_Mp_Parallel_Do_Blk) {
      /* just pop it */
      POP_BLK_STK;
      CLEAR_DIRECTIVE_STATE(Open_Mp_Parallel_Do_Region);
   }


   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(Open_MP_End_Parallel_Stmt, CURR_BLK)) {
         blk_match_err(Open_Mp_Parallel_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Open_Mp_Parallel_Blk) {
         /* point to the PARALLEL directive */
         IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) = SH_Tbl_Idx;
         IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)) = CURR_BLK_FIRST_SH_IDX;

         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_open_mp_parallel_blk", NULL);

   return;

}  /* end_open_mp_parallel_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_open_mp_do_blk(boolean  err_call)

{
   int		sh_idx;


   TRACE (Func_Entry, "end_open_mp_do_blk", NULL);

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(Open_MP_End_Do_Stmt, CURR_BLK)) {
         blk_match_err(Open_Mp_Do_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Open_Mp_Do_Blk) {
         /* remove the enddo statement that was generated at the */
         /* end of the Do Blk. This one supersedes it. */

         if (BLK_ENDPDO_SH_IDX(blk_stk_idx) != NULL_IDX) {
            sh_idx = BLK_ENDPDO_SH_IDX(blk_stk_idx);
            SH_PREV_IDX(SH_NEXT_IDX(sh_idx)) = SH_PREV_IDX(sh_idx);
            SH_NEXT_IDX(SH_PREV_IDX(sh_idx)) = SH_NEXT_IDX(sh_idx);

            FREE_IR_NODE(SH_IR_IDX(sh_idx));
            FREE_SH_NODE(sh_idx);
         }

         IR_LINE_NUM_R(SH_IR_IDX(curr_stmt_sh_idx)) = stmt_start_line;
         IR_COL_NUM_R(SH_IR_IDX(curr_stmt_sh_idx))  = stmt_start_col;
         IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) = SH_Tbl_Idx;
         IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)) = CURR_BLK_FIRST_SH_IDX;

         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_open_mp_do_blk", NULL);

   return;

}  /* end_open_mp_do_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_open_mp_parallel_sections_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_open_mp_parallel_sections_blk", NULL);

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(Open_MP_End_Parallel_Sections_Stmt, CURR_BLK)) {
         blk_match_err(Open_Mp_Parallel_Sections_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Open_Mp_Parallel_Sections_Blk) {
         /* point to the PARALLEL SECTIONS directive */
         IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) = SH_Tbl_Idx;
         IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)) = CURR_BLK_FIRST_SH_IDX;

         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_open_mp_parallel_sections_blk", NULL);

   return;

}  /* end_open_mp_parallel_sections_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_open_mp_sections_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_open_mp_sections_blk", NULL);

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(Open_MP_End_Sections_Stmt, CURR_BLK)) {
         blk_match_err(Open_Mp_Sections_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Open_Mp_Sections_Blk) {
         /* point to the SECTIONS directive */
         IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) = SH_Tbl_Idx;
         IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)) = CURR_BLK_FIRST_SH_IDX;

         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_open_mp_sections_blk", NULL);

   return;

}  /* end_open_mp_sections_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_open_mp_section_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_open_mp_section_blk", NULL);

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(Open_MP_End_Section_Stmt, CURR_BLK)) {
         blk_match_err(Open_Mp_Section_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Open_Mp_Section_Blk) {
         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_open_mp_section_blk", NULL);

   return;

}  /* end_open_mp_section_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_open_mp_single_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_open_mp_single_blk", NULL);

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(Open_MP_End_Single_Stmt, CURR_BLK)) {
         blk_match_err(Open_Mp_Single_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Open_Mp_Single_Blk) {
         /* point to the SINGLE directive */
         IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) = SH_Tbl_Idx;
         IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)) = CURR_BLK_FIRST_SH_IDX;

         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_open_mp_single_blk", NULL);

   return;

}  /* end_open_mp_single_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_open_mp_parallel_do_blk(boolean  err_call)

{
   int		sh_idx;


   TRACE (Func_Entry, "end_open_mp_parallel_do_blk", NULL);

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(Open_MP_End_Parallel_Do_Stmt, CURR_BLK)) {
         blk_match_err(Open_Mp_Parallel_Do_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Open_Mp_Parallel_Do_Blk) {
         /* remove the enddo statement that was generated at the */
         /* end of the Do Blk. This one supersedes it. */

         if (BLK_ENDPDO_SH_IDX(blk_stk_idx) != NULL_IDX) {
            sh_idx = BLK_ENDPDO_SH_IDX(blk_stk_idx);
            SH_PREV_IDX(SH_NEXT_IDX(sh_idx)) = SH_PREV_IDX(sh_idx);
            SH_NEXT_IDX(SH_PREV_IDX(sh_idx)) = SH_NEXT_IDX(sh_idx);

            FREE_IR_NODE(SH_IR_IDX(sh_idx));
            FREE_SH_NODE(sh_idx);
         }

         IR_LINE_NUM_R(SH_IR_IDX(curr_stmt_sh_idx)) = stmt_start_line;
         IR_COL_NUM_R(SH_IR_IDX(curr_stmt_sh_idx))  = stmt_start_col;
         IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) = SH_Tbl_Idx;
         IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)) = CURR_BLK_FIRST_SH_IDX;

         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_open_mp_parallel_do_blk", NULL);

   return;
}

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_open_mp_master_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_open_mp_master_blk", NULL);

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(Open_MP_End_Master_Stmt, CURR_BLK)) {
         blk_match_err(Open_Mp_Master_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Open_Mp_Master_Blk) {
         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_open_mp_master_blk", NULL);

   return;

}  /* end_open_mp_master_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_open_mp_critical_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_open_mp_critical_blk", NULL);

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(Open_MP_End_Critical_Stmt, CURR_BLK)) {
         blk_match_err(Open_Mp_Critical_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Open_Mp_Critical_Blk) {
         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_open_mp_critical_blk", NULL);

   return;

}  /* end_open_mp_critical_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_open_mp_ordered_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_open_mp_ordered_blk", NULL);

   if (! err_call) {

      if (STMT_CANT_BE_IN_BLK(Open_MP_End_Ordered_Stmt, CURR_BLK)) {
         blk_match_err(Open_Mp_Ordered_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Open_Mp_Ordered_Blk) {
         POP_BLK_STK;
      }
   }
   else {
      POP_BLK_STK;
   }

   TRACE (Func_Exit, "end_open_mp_ordered_blk", NULL);

   return;

}  /* end_open_mp_ordered_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_open_mp_parallel_workshare_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_open_mp_parallel_workshare_blk", NULL);

   if (! err_call) {
      if (STMT_CANT_BE_IN_BLK(Open_MP_End_Parallel_Workshare_Stmt, CURR_BLK)) {
	 blk_match_err(Open_Mp_Parallel_Workshare_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Open_Mp_Parallel_Workshare_Blk) {
	 IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) = SH_Tbl_Idx;
	 IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)) = CURR_BLK_FIRST_SH_IDX;

	 POP_BLK_STK;
      }
   }
   else{
      POP_BLK_STK;
   }

   TRACE(Func_Exit, "end_open_mp_parallel_workshare_blk", NULL);

   return;
} /* end_open_mp_parallel_workshare_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void end_open_mp_workshare_blk(boolean  err_call)

{
   TRACE (Func_Entry, "end_open_mp_workshare_blk", NULL);

   if (! err_call) {
      if (STMT_CANT_BE_IN_BLK(Open_MP_End_Workshare_Stmt, CURR_BLK)) {
	 blk_match_err(Open_Mp_Workshare_Blk, FALSE, FALSE);
      }

      if (CURR_BLK == Open_Mp_Workshare_Blk) {
	 IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) = SH_Tbl_Idx;
	 IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)) = CURR_BLK_FIRST_SH_IDX;

	 POP_BLK_STK;
      }
   }
   else{
      POP_BLK_STK;
   }

   TRACE(Func_Exit, "end_open_mp_workshare_blk", NULL);

   return;
} /* end_open_mp_workshare_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Look for Do_Parallel_Blk's on the block stack. If we are outside the  *|
|*      associated Do_Blk, remove the Do_Parallel_Blk. If we are inside the   *|
|*      associated Do_Blk, and cannot_nest is true, issue an error.           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

boolean remove_do_parallel_blk(boolean  cannot_nest,
                               char     *str,
                               int      line,
                               int      col)


{
   int          blk_idx;
   boolean	err = FALSE;

   TRACE (Func_Entry, "remove_do_parallel_blk", NULL);

   blk_idx = blk_stk_idx;

   while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {

      if (BLK_TYPE(blk_idx) == Do_Parallel_Blk) {
         if (blk_idx < blk_stk_idx &&
             BLK_TYPE(blk_idx + 1) == Do_Blk &&
             BLK_DEF_LINE(blk_idx) == BLK_DEF_LINE(blk_idx + 1)) {

            /* inside the do block for the do parallel */
            if (cannot_nest) {
               PRINTMSG(line, 1289, Error, col, str);
               err = TRUE;
            }
            else {
               /* intentionally blank, just leave as is */
            }
         }
         else {
            /* outside the do block for the do parallel */
            /* just remove the do parallal blk          */

            move_blk_to_end(blk_idx);
            CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
            POP_BLK_STK;
         }
      }

      blk_idx--;
   }

   TRACE (Func_Exit, "remove_do_parallel_blk", NULL);

   return(err);

}  /* remove_do_parallel_blk */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Look for SGI_Pdo_Blk's on the block stack. If we are outside the      *|
|*      associated Do_Blk, remove the SGI_Pdo_Blk. If we are inside the       *|
|*      associated Do_Blk, and cannot_nest is true, issue an error.           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

boolean remove_pdo_blk(boolean  cannot_nest,
                       char     *str,
                       int      line,
                       int      col)


{
   int          blk_idx;
   boolean      err = FALSE;

   TRACE (Func_Entry, "remove_pdo_blk", NULL);

   blk_idx = blk_stk_idx;

   while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {

      if (BLK_TYPE(blk_idx) == SGI_Pdo_Blk) {
         if (blk_idx < blk_stk_idx &&
             BLK_TYPE(blk_idx + 1) == Do_Blk &&
             BLK_DEF_LINE(blk_idx) == BLK_DEF_LINE(blk_idx + 1)) {

            /* inside the do block for the pdo */
            if (cannot_nest) {
               /* need new message BHJ */
               PRINTMSG(line, 1289, Error, col, str);
               err = TRUE;
            }
            else {
               /* intentionally blank, just leave as is */
            }
         }
         else {
            /* outside the do block for the pdo */
            /* just remove the pdo blk          */

            move_blk_to_end(blk_idx);
            CLEAR_DIRECTIVE_STATE(Pdo_Region);
            POP_BLK_STK;
         }
      }

      blk_idx--;
   }

   TRACE (Func_Exit, "remove_pdo_blk", NULL);

   return(err);

}  /* remove_pdo_blk */


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Check to see if the current iterative DO ending statement immediately *|
|*      follows a preceding iterative DO ending statement.  That is, check to *|
|*      see if the bottom of the current loop is perfectly nested within its  *|
|*      containing iterative DO loop.                                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if the current DO ending statement immediately follows a         *|
|*      preceding iterative DO ending statement.                              *|
|*                                                                            *|
\******************************************************************************/

static void check_loop_bottom_nesting(void)

{
   int		blk_idx;
   boolean	perfectly_nested	= FALSE;
   int		sh_idx;
   char		str[80];


   TRACE (Func_Entry, "check_loop_bottom_nesting", NULL);

   /* Look back through the SH's to find the loop ending IR for the next      */
   /* inner loop.  The following SH types can occur between the current loop  */
   /* ending IR and the inner loop ending IR:				      */
   /*									      */
   /* * Anything that is compiler-generated (we're looking for the compiler-  */
   /*   generated Loop_End IR).						      */
   /*									      */
   /* * END DO statements.  A nest of loops that end with END DO's looks like:*/
   /*      SH:  End_Do_Stmt						      */
   /*      SH:  CG Continue (Loop_End)					      */
   /*      SH:  End_Do_Stmt						      */
   /*      SH:  CG Continue (Loop_End)					      */
   /*   So, if we're at the second Loop_End, it's OK to pass through the      */
   /*   End_Do above it to get to the next Loop_End.			      */
   /*									      */
   /* * The IR that represents a labeled stmt that terminates the loop.  An   */
   /*   example:							      */
   /*									      */
   /*       C*$*  INTERCHANGE(m, i, n)					      */
   /*									      */
   /*             DO 30, i = 1, 3					      */
   /*               DO 30, m = 1, 3					      */
   /*                 DO n = 1, 3					      */
   /*                   PRINT *, 'In innermost loop', n			      */
   /*                 END DO						      */
   /*       30    array2(i,m) = m + i					      */
   /*									      */
   /*   produces the IR:						      */
   /*      SH:  End_Do							      */
   /*      SH:  CG CONTINUE  (Loop_End)					      */
   /*      SH:  Label_Def    (30)					      */
   /*      SH:  Assignment						      */
   /*      SH:  CG CONTINUE  (Loop_End)					      */
   /*      SH:  CG CONTINUE  (Loop_End)					      */
   /*									      */
   /*   So when the middle Loop_End is the current loop being ended, we need  */
   /*   to be able to pass through the labeled stmt to make sure the inner    */
   /*   loop is perfectly nested in the middle loop.  We can tell this        */
   /*   labeled stmt is OK because if stmt_label_idx is not null, it means    */
   /*   the labeled stmt is still being worked on which means the labeled     */
   /*   stmt is the loop ending stmt.					      */

   sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   while (SH_COMPILER_GEN(sh_idx)              ||
          SH_STMT_TYPE(sh_idx) == End_Do_Stmt  ||
          (stmt_label_idx != NULL_IDX  &&
           (SH_LABELED(sh_idx)  ||  SH_STMT_TYPE(sh_idx) == Label_Def))) { 

      if (SH_LOOP_END(sh_idx)) {
         perfectly_nested = TRUE;
         break;
      }
      else {
         sh_idx = SH_PREV_IDX(sh_idx);
      }
   }

   if (! perfectly_nested) {

      if (SH_LOOP_END(sh_idx)) {
         perfectly_nested = TRUE;
      }
   }

  
   /* The loop inside the current loop is not perfectly nested.  Find the     */
   /* statement that interrupted the nesting and issue an error message.      */
   /* The search starts with the Loop_End IR.  The previous SH could be for   */
   /* an END DO (the actual end of the current loop) so pass through it (and  */
   /* its label if it has one) to get to the offending statement.	      */
   /* Use "DO" as the default insert in the message.  Fill in the correct     */
   /* insert as the type of directive is determined.			      */

   if (! perfectly_nested) {

      if (BLK_HAS_NESTED_LOOP(blk_stk_idx)) {
         sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         while (SH_COMPILER_GEN(sh_idx)              ||
                SH_STMT_TYPE(sh_idx) == End_Do_Stmt  ||
                SH_STMT_TYPE(sh_idx) == Label_Def) {
            sh_idx = SH_PREV_IDX(sh_idx);
         }

      }
      
      strcpy(str, "DO");

      for (blk_idx = blk_stk_idx;  blk_idx > 1;  --blk_idx) {

         if (BLK_TYPE(blk_idx) == Do_Blk) {

            if (BLK_DIR_NEST_CHECK_SH_IDX(blk_idx) != NULL_IDX) {

               switch (IR_OPR(SH_IR_IDX(BLK_DIR_NEST_CHECK_SH_IDX(blk_idx)))) {

                  case Pdo_Par_Opr:
                     strcpy(str, "PDO");
                     break;

                  case Parallel_Do_Par_Opr:
                     strcpy(str, "PARALLEL DO");
                     break;

                  case Doacross_Dollar_Opr:
                     strcpy(str, "DOACROSS");
                     break;

                  default:
                     strcpy(str, "DO");
                     break;
               }

               break;
            }
            else if (BLK_INTERCHANGE_DIR_SH_IDX(blk_idx) != NULL_IDX) {
               strcpy(str,"INTERCHANGE");
               break;
            }
         }
      }

      PRINTMSG(SH_GLB_LINE(sh_idx), 1380, Error, SH_COL_NUM(sh_idx),
               str); 

      for (blk_idx = blk_stk_idx;  blk_idx > 1;  --blk_idx) {
      
         if (BLK_TYPE(blk_idx) == Do_Blk) {
            BLK_INTERCHANGE_NUM_LCVS(blk_idx)    = 0;
            BLK_DIR_NEST_CHECK_NUM_LCVS(blk_idx) = 0;
            BLK_HAS_NESTED_LOOP(blk_idx)         = FALSE;

            if (BLK_DIR_NEST_CHECK_SH_IDX(blk_idx) != NULL_IDX) {
               SH_ERR_FLG(BLK_DIR_NEST_CHECK_SH_IDX(blk_idx)) = TRUE;
               BLK_DIR_NEST_CHECK_SH_IDX(blk_idx)             = NULL_IDX;
            }

            if (BLK_INTERCHANGE_DIR_SH_IDX(blk_idx) != NULL_IDX) {
               SH_ERR_FLG(BLK_INTERCHANGE_DIR_SH_IDX(blk_idx)) = TRUE;
               BLK_INTERCHANGE_DIR_SH_IDX(blk_idx)             = NULL_IDX;
               break;
            }
         }
      }
   }

   TRACE (Func_Exit, "loop_bottom_is_perfectly_nested", NULL);

   return;

}  /* check_loop_bottom_nesting */

# endif

# if defined(_EXPRESSION_EVAL)

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Simulate an END statement.                                            *|
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

void expression_eval_end (void)
{

   TRACE (Func_Entry, "expression_eval_end", NULL);

   end_of_contains			= FALSE;
   stmt_type				= End_Program_Stmt;
   SH_STMT_TYPE(curr_stmt_sh_idx)	= End_Program_Stmt;

   if (stmt_label_idx != NULL_IDX) {
      end_labeled_do();
   }

   /* Issue any deferred src_input messages.                         */

   issue_deferred_msgs();

   if (CURR_BLK != Program_Blk) {
      SCP_IN_ERR(curr_scp_idx)	= TRUE;
   }

   end_program_unit(FALSE);

   /* Save the line and column of the EOS (which tells us where the END stmt  */
   /* really ends).                                                           */

   cif_end_unit_line   = LA_CH_LINE;
   cif_end_unit_column = LA_CH_COLUMN - 1;

   TRACE (Func_Exit, "expression_eval_end", NULL);

   return;

}  /* expression_eval_end */

# endif

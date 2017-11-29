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



static char USMID[] = "\n@(#)5.0_pl/sources/s_end.c	5.2	06/16/99 10:02:23\n";


# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "s_globals.m"
# include "debug.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "s_globals.h"


/*****************************************************************\
|* Function prototypes of static functions declared in this file *|
\*****************************************************************/



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	ADD DESCRIPTION HERE						      *|
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

void end_stmt_semantics (void)

{
   int          new_end_idx;
   int          new_start_idx;
   int          ptr;


   TRACE (Func_Entry, "end_stmt_semantics", NULL);

   ptr = SCP_EXIT_IR_SH_IDX(curr_scp_idx);

   if (ptr) {
      while (SH_NEXT_IDX(ptr) != NULL_IDX) {
         ptr = SH_NEXT_IDX(ptr);
      }

      copy_entry_exit_sh_list(SCP_EXIT_IR_SH_IDX(curr_scp_idx), ptr,
                              &new_start_idx, &new_end_idx);

      insert_sh_chain_before(new_start_idx);
   }

   if (opt_flags.inline_lvl > Inline_Lvl_0 ||
       opt_flags.modinline || dump_flags.preinline) {
      gen_directive_ir(Inline_Cdir_Opr);
   }

   if (cdir_switches.bounds) {
      gen_directive_ir(Nobounds_Cdir_Opr);
   }

   TRACE (Func_Exit, "end_stmt_semantics", NULL);

   return;

}  /* end_stmt_semantics */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      ADD DESCRIPTION HERE                                                  *|
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

void end_subroutine_semantics (void)

{
   int          new_end_idx;
   int          new_start_idx;
   int          ptr;


   TRACE (Func_Entry, "end_subroutine_semantics", NULL);

   ptr = SCP_EXIT_IR_SH_IDX(curr_scp_idx);

   if (ptr) {
      while (SH_NEXT_IDX(ptr) != NULL_IDX) {
         ptr = SH_NEXT_IDX(ptr);
      }

      copy_entry_exit_sh_list(SCP_EXIT_IR_SH_IDX(curr_scp_idx), ptr,
                              &new_start_idx, &new_end_idx);
   
      insert_sh_chain_before(new_start_idx);
   }

   if (opt_flags.inline_lvl > Inline_Lvl_0 ||
       opt_flags.modinline || dump_flags.preinline) {
      gen_directive_ir(Inline_Cdir_Opr);
   }

   if (cdir_switches.bounds) {
      gen_directive_ir(Nobounds_Cdir_Opr);
   }

   TRACE (Func_Exit, "end_subroutine_semantics", NULL);

   return;

}  /* end_subroutine_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END statement for a FUNCTION           *|
|*      subprogram.							      *|
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

void end_function_semantics (void)

{
   int                  idx;
   int                  ir_idx;
   int                  new_end_idx;
   size_offset_type     new_size;
   int                  new_start_idx;
   int                  ptr;
   int                  rslt_idx;
   size_offset_type     result;
   size_offset_type     size;


   TRACE (Func_Entry, "end_function_semantics", NULL);

   rslt_idx = ATP_RSLT_IDX(SCP_ATTR_IDX(curr_scp_idx));

   if (!ATD_IM_A_DOPE(rslt_idx) &&
       ATD_ARRAY_IDX(rslt_idx) == NULL_IDX &&
       TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) != Structure &&
       TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) != Character) {

      ir_idx			= SH_IR_IDX(curr_stmt_sh_idx);

# ifdef _SEPARATE_FUNCTION_RETURNS

      if (SCP_ALT_ENTRY_CNT(curr_scp_idx) != 0 &&
          SCP_RETURN_LABEL(curr_scp_idx) != NULL_IDX) {

         /* change return to goto to multiple return code block */

         IR_OPR(ir_idx)   = Br_Uncond_Opr;
         IR_FLD_R(ir_idx) = AT_Tbl_Idx;
         IR_IDX_R(ir_idx) = SCP_RETURN_LABEL(curr_scp_idx);
         IR_LINE_NUM_R(ir_idx)  = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_R(ir_idx)   = IR_COL_NUM(ir_idx);
      }
      else {
         IR_FLD_R(ir_idx)       = AT_Tbl_Idx;
         IR_IDX_R(ir_idx)       = rslt_idx;
         IR_LINE_NUM_R(ir_idx)  = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_R(ir_idx)   = IR_COL_NUM(ir_idx);
      }
# else

      IR_FLD_R(ir_idx)		= AT_Tbl_Idx;
      IR_LINE_NUM_R(ir_idx)	= IR_LINE_NUM(ir_idx);
      IR_COL_NUM_R(ir_idx)	= IR_COL_NUM(ir_idx);

      if (SCP_ENTRY_IDX(curr_scp_idx)) {
         idx	= SCP_ENTRY_IDX(curr_scp_idx);
         size	= stor_bit_size_of(rslt_idx, TRUE, FALSE);

         /* KAY - Do not allow n$pes in alternate entry function results */

         while (idx != NULL_IDX) {
            new_size	= stor_bit_size_of(ATP_RSLT_IDX(AL_ATTR_IDX(idx)),
                                           TRUE,
                                           FALSE);
            size_offset_logical_calc(&new_size, &size, Gt_Opr, &result);

            if (THIS_IS_TRUE(result.constant, result.type_idx)) {
               size	= new_size;
               rslt_idx	= ATP_RSLT_IDX(AL_ATTR_IDX(idx));
            }
            idx		= AL_NEXT_IDX(idx);
         }
      }

      IR_IDX_R(ir_idx)		= rslt_idx;
# endif
   }
   else {

      /* Set the function result even if this ends up looking like a          */
      /* subroutine.  This lets PDGCS get function not defined messages right.*/

      ir_idx			= SH_IR_IDX(curr_stmt_sh_idx);
      IR_FLD_R(ir_idx)		= AT_Tbl_Idx;
      IR_LINE_NUM_R(ir_idx)	= IR_LINE_NUM(ir_idx);
      IR_COL_NUM_R(ir_idx)	= IR_COL_NUM(ir_idx);
      IR_IDX_R(ir_idx)		= rslt_idx;
   }

   ptr = SCP_EXIT_IR_SH_IDX(curr_scp_idx);

   if (ptr) {
      while (SH_NEXT_IDX(ptr) != NULL_IDX) {
         ptr = SH_NEXT_IDX(ptr);
      }

      copy_entry_exit_sh_list(SCP_EXIT_IR_SH_IDX(curr_scp_idx), ptr,
                              &new_start_idx, &new_end_idx);

      insert_sh_chain_before(new_start_idx);
   }

   if (opt_flags.inline_lvl > Inline_Lvl_0 ||
       opt_flags.modinline || dump_flags.preinline) {
      ir_idx = gen_directive_ir(Inline_Cdir_Opr);
   }

   if (cdir_switches.bounds) {
      ir_idx = gen_directive_ir(Nobounds_Cdir_Opr);
   }

   TRACE (Func_Exit, "end_function_semantics", NULL);

   return;

}  /* end_function_semantics */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure completes the processing of the END FORALL statement.  *|
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

void end_forall_semantics (void)

{
   int                  ir_idx;
   int			list_idx;
   int                  sh_idx;


   TRACE (Func_Entry, "end_forall_semantics", NULL);

   sh_idx = SH_PARENT_BLK_IDX(curr_stmt_sh_idx);

   if (SH_ERR_FLG(curr_stmt_sh_idx) || SH_ERR_FLG(sh_idx)) {
      goto EXIT;
   }

# ifdef _DEBUG
   if (sh_idx == NULL_IDX) {
      PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 626, Internal,
               SH_COL_NUM(curr_stmt_sh_idx),
               "SH_PARENT_BLK_IDX", "end_forall_semantics");
   }
# endif

   ir_idx = SH_IR_IDX(sh_idx);

# ifdef _DEBUG
   if (IR_OPR(ir_idx) != Forall_Opr) {
      PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 626, Internal,
               SH_COL_NUM(curr_stmt_sh_idx),
               "Forall_Opr", "end_forall_semantics");
   }
# endif

   list_idx = IR_IDX_R(ir_idx);

   while (list_idx &&
          IL_FLD(list_idx) == IL_Tbl_Idx) {

      AT_ATTR_LINK(IL_IDX(IL_IDX(list_idx))) = NULL_IDX;
      AT_IGNORE_ATTR_LINK(IL_IDX(IL_IDX(list_idx))) = FALSE;

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

EXIT:

   active_forall_sh_idx = SH_PARENT_BLK_IDX(active_forall_sh_idx);

   if (active_forall_sh_idx == NULL_IDX) {
      within_forall_construct = FALSE;
   }

   TRACE (Func_Exit, "end_forall_semantics", NULL);

   return;

}  /* end_forall_semantics */

#ifndef _HIGH_LEVEL_IF_FORM


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END SELECT statement.		      *|
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

void end_if_semantics (void)

{
   int  	if_ir_idx;
   int  	if_sh_idx;
   int		il_idx;
   int		ir_idx;
   int  	lbl_idx;
   int  	sh_idx;
   opnd_type	tmp_opnd;


   TRACE (Func_Entry, "end_if_semantics", NULL);


   /* Walk back through the IF construct to find the IF construct SH.         */
   /* If it's marked in error, bail.					      */

   if_sh_idx = SH_PARENT_BLK_IDX(curr_stmt_sh_idx);

   while (SH_STMT_TYPE(if_sh_idx) != If_Cstrct_Stmt) {

      if (SH_STMT_TYPE(if_sh_idx) == Else_Stmt) {
         if_sh_idx = IR_IDX_L(SH_IR_IDX(if_sh_idx));
      }
      else {
         if_sh_idx = IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(SH_IR_IDX(if_sh_idx))));
      }
   }

   if (SH_ERR_FLG(if_sh_idx)) {
      goto EXIT;
   }

   lbl_idx = IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(SH_IR_IDX(if_sh_idx))));
   AT_DEFINED(lbl_idx)       = TRUE;
   AT_DEF_LINE(lbl_idx)      = stmt_start_line;
   ATL_DEF_STMT_IDX(lbl_idx) = curr_stmt_sh_idx;
   AT_REFERENCED(lbl_idx)    = Referenced;


   /* If the last clause of the IF construct is an ELSE IF, generate a        */
   /* CONTINUE statement to define its branch-around label.                   */

   if (SH_STMT_TYPE(SH_PARENT_BLK_IDX(curr_stmt_sh_idx)) == Else_If_Stmt) {
      gen_sh(Before, Continue_Stmt, stmt_start_line, stmt_start_col,
             FALSE,
             TRUE,					/* Labeled.	      */
             TRUE);					/* Compiler-generated */

      sh_idx              = SH_PREV_IDX(curr_stmt_sh_idx);
      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(sh_idx)   = ir_idx;
      IR_OPR(ir_idx)      = Label_Opr;
      IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx) = stmt_start_line;
      IR_COL_NUM(ir_idx)  = stmt_start_col;
      IR_LINE_NUM_L(ir_idx) = stmt_start_line;
      IR_COL_NUM_L(ir_idx)  = stmt_start_col;
      IR_FLD_L(ir_idx)    = AT_Tbl_Idx;
      lbl_idx             =
         IL_IDX(IR_IDX_R(SH_IR_IDX(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))));
      IR_IDX_L(ir_idx)    = lbl_idx; 

      AT_DEFINED(lbl_idx)       = TRUE;
      AT_DEF_LINE(lbl_idx)      = stmt_start_line;
      ATL_DEF_STMT_IDX(lbl_idx) = sh_idx;
      AT_REFERENCED(lbl_idx)    = Referenced;
   }


   /* Generate a CONTINUE statement to define the "end IF" label.             */
   /* If there were no ELSE IF's and no ELSE, get the label from the first IL */
   /* attached to the right operand of the If_Opr IR attached to the If_Cstrct*/
   /* SH.  Otherwise, get the label from the second IL.			      */

   gen_sh(Before, Continue_Stmt, stmt_start_line, stmt_start_col,
          FALSE,
          TRUE,						/* Labeled.	      */
          TRUE);					/* Compiler-generated */

   sh_idx                            = SH_PREV_IDX(curr_stmt_sh_idx);
   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(sh_idx)                 = ir_idx;
   IR_OPR(ir_idx)                    = Label_Opr;
   IR_TYPE_IDX(ir_idx)               = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)               = stmt_start_line;
   IR_COL_NUM(ir_idx)                = stmt_start_col;
   IR_LINE_NUM_L(ir_idx)             = stmt_start_line;
   IR_COL_NUM_L(ir_idx)              = stmt_start_col;
   IR_FLD_L(ir_idx)                  = AT_Tbl_Idx;

   if_ir_idx = SH_IR_IDX(if_sh_idx);

   if (SH_STMT_TYPE(SH_PARENT_BLK_IDX(curr_stmt_sh_idx)) == If_Cstrct_Stmt) {
      lbl_idx = IL_IDX(IR_IDX_R(if_ir_idx)); 
   }
   else {
      lbl_idx = IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(if_ir_idx)));
   }
   
   IR_IDX_L(ir_idx)           = lbl_idx;

   AT_DEFINED(lbl_idx)        = TRUE;
   AT_DEF_LINE(lbl_idx)       = stmt_start_line;
   ATL_DEF_STMT_IDX(lbl_idx)  = sh_idx;
   AT_REFERENCED(lbl_idx)     = Referenced;


   /* Walk back through the IF construct and transfer the branch around label */
   /* for each ELSE IF and for the IF itself to the right operand of each     */
   /* Br_True IR (replacing the IL list).  The IL_OPND is copied to a temp    */
   /* first because sometimes assignments get a little funky using these      */
   /* macros if the target is also being used to access the source.           */
   /* LRR:  If we're gettting tight on space, could also delete the IL nodes. */

   if_sh_idx = SH_PARENT_BLK_IDX(curr_stmt_sh_idx);

   while (SH_STMT_TYPE(if_sh_idx) != If_Cstrct_Stmt) {

      if (SH_STMT_TYPE(if_sh_idx) == Else_Stmt) {
         sh_idx            = if_sh_idx;
         if_sh_idx         = IR_IDX_L(SH_IR_IDX(if_sh_idx));
         SH_IR_IDX(sh_idx) = NULL_IDX;
      }
      else {
         il_idx = IL_NEXT_LIST_IDX(IR_IDX_R(SH_IR_IDX(if_sh_idx)));
         COPY_OPND(tmp_opnd, IL_OPND(IR_IDX_R(SH_IR_IDX(if_sh_idx))));
         COPY_OPND(IR_OPND_R(SH_IR_IDX(if_sh_idx)), tmp_opnd);
         if_sh_idx = IL_IDX(il_idx);
      }
   }

   COPY_OPND(tmp_opnd, IL_OPND(IR_IDX_R(if_ir_idx)));
   COPY_OPND(IR_OPND_R(if_ir_idx), tmp_opnd);

EXIT:

   TRACE (Func_Exit, "end_if_semantics", NULL);

   return;

}  /* end_if_semantics */

#endif


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END SELECT statement.		      *|
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

void end_select_semantics (void)

{
   int	i;
   int  il_idx;
   int	ir_idx;
   int	next_il_idx;


   TRACE (Func_Entry, "end_select_semantics", NULL);

   if (! SH_ERR_FLG(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))) {

      ir_idx = SH_IR_IDX(SH_PARENT_BLK_IDX(curr_stmt_sh_idx));
      SH_IR_IDX(SH_PARENT_BLK_IDX(curr_stmt_sh_idx)) = IR_IDX_L(ir_idx);

      il_idx = IR_IDX_R(ir_idx);
     
      for (i = 1;  i <= IR_LIST_CNT_R(ir_idx); i++) {
         next_il_idx = IL_NEXT_LIST_IDX(il_idx);
         FREE_IR_LIST_NODE(il_idx);
         il_idx = next_il_idx;
      }

      FREE_IR_NODE(ir_idx);
   }

   TRACE (Func_Exit, "end_select_semantics", NULL);

   return;

}  /* end_select_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing of the END WHERE statement.	              *|
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

void end_where_semantics (void)

{
   int		sh_idx;

   TRACE (Func_Entry, "end_where_semantics", NULL);

   where_ir_idx = NULL_IDX;

   if (where_dealloc_stmt_idx) {
      SH_NEXT_IDX(where_dealloc_stmt_idx) = SH_NEXT_IDX(curr_stmt_sh_idx);
      SH_PREV_IDX(where_dealloc_stmt_idx) = curr_stmt_sh_idx;
      SH_PREV_IDX(SH_NEXT_IDX(curr_stmt_sh_idx)) = where_dealloc_stmt_idx;
      SH_NEXT_IDX(curr_stmt_sh_idx) = where_dealloc_stmt_idx;
      
      where_dealloc_stmt_idx = NULL_IDX;
   }

   sh_idx = SH_PARENT_BLK_IDX(curr_stmt_sh_idx);

   while (sh_idx != NULL_IDX &&
          SH_STMT_TYPE(sh_idx) != Where_Cstrct_Stmt) {
      sh_idx = SH_PARENT_BLK_IDX(sh_idx);
   }

   if (sh_idx != NULL_IDX &&
       (SH_PARENT_BLK_IDX(sh_idx) == NULL_IDX ||
        (SH_STMT_TYPE(SH_PARENT_BLK_IDX(sh_idx)) != Where_Cstrct_Stmt &&
         SH_STMT_TYPE(SH_PARENT_BLK_IDX(sh_idx)) != Else_Where_Stmt &&
         SH_STMT_TYPE(SH_PARENT_BLK_IDX(sh_idx)) != Else_Where_Mask_Stmt))) {

      alloc_block_start_idx = NULL_IDX;
      alloc_block_end_idx = NULL_IDX;
   }

   if (sh_idx != NULL_IDX &&
       SH_PARENT_BLK_IDX(sh_idx) != NULL_IDX) {

      sh_idx = SH_PARENT_BLK_IDX(sh_idx);

      if (SH_STMT_TYPE(sh_idx) == Where_Cstrct_Stmt ||
          SH_STMT_TYPE(sh_idx) == Else_Where_Stmt ||
          SH_STMT_TYPE(sh_idx) == Else_Where_Mask_Stmt) {

         if (IR_FLD_L(SH_IR_IDX(sh_idx)) == IL_Tbl_Idx) {
            where_ir_idx = IL_IDX(IR_IDX_L(SH_IR_IDX(sh_idx)));
         }
      }
      else if (SH_STMT_TYPE(sh_idx) == Forall_Cstrct_Stmt) {
         active_forall_sh_idx = sh_idx;
      }
# ifdef _DEBUG
      else {
         PRINTMSG(SH_GLB_LINE(sh_idx), 626, Internal, SH_COL_NUM(sh_idx),
                  "Forall_Opr", "end_where_semantics");
      }
# endif
   }

   TRACE (Func_Exit, "end_where_semantics", NULL);

   return;

}  /* end_where_semantics */

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



static char USMID[] = "\n@(#)5.0_pl/sources/ir.c	5.6	10/05/99 14:45:20\n";

# include <stdarg.h>

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
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static boolean	compare_il(int, int, int, int);
static boolean	compare_ir(int, int);
static boolean	il_is_symbolic_constant(int);
static boolean	ir_is_symbolic_constant(int);

static	int	label_copy_al_idx = NULL_IDX;
static	boolean	gen_lbl_copy = FALSE;


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Generate a new Statement Header (SH) either before or after the       *|
|*      current SH.							      *|
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

void gen_sh(sh_position_type 	position,
  	    stmt_type_type 	stmt_type,
	    int			line_num,
	    int			col_num,
	    boolean		err_flag,
	    boolean		labeled,
	    boolean		compiler_gen)
#ifdef KEY /* Bug 4811 */
{
  int sh_idx = gen_sh_at(position, stmt_type, line_num, col_num, err_flag,
    labeled, compiler_gen, curr_stmt_sh_idx);
  if (position == After) {
    curr_stmt_sh_idx = sh_idx;
  }
}


/******************************************************************************\
|* Description:								      *|
|*	Like gen_sh(), but takes "insertion_point" as an argument, instead of *|
|*      using the global variable "curr_stmt_sh_idx", so one may insert the   *|
|*      new statement at any point; doesn't update the value of               *|
|*      curr_stmt_sh_idx; and returns the newly generated sh_idx.             *|
\******************************************************************************/
int gen_sh_at(sh_position_type 	position,
  	    stmt_type_type 	stmt_type,
	    int			line_num,
	    int			col_num,
	    boolean		err_flag,
	    boolean		labeled,
	    boolean		compiler_gen,
	    int			insertion_idx)
{
   int		next_idx;
   int		prev_idx;
   int		sh_idx;


   TRACE (Func_Entry, "gen_sh_at", NULL);

# ifdef _DEBUG
   if (defer_stmt_expansion) {
      PRINTMSG(line_num, 626, Internal, col_num,
               "no defer_stmt_expansion", "gen_sh_at");
   }

   if (insertion_idx == NULL_IDX) {
      PRINTMSG(line_num, 626, Internal, col_num,
               "valid insertion_idx", "gen_sh_at");
   }
# endif

   sh_idx		   = ntr_sh_tbl();
   SH_STMT_TYPE(sh_idx)	   = stmt_type;
   SH_GLB_LINE(sh_idx)	   = line_num;
   SH_ERR_FLG(sh_idx)	   = err_flag;
   SH_COL_NUM(sh_idx)	   = col_num;
   SH_LABELED(sh_idx)	   = labeled;
   SH_COMPILER_GEN(sh_idx) = compiler_gen;
   
   if (stmt_type == Construct_Def) {
      SH_P2_SKIP_ME(sh_idx) = TRUE;
   }

   if (position == Before) {
      SH_NEXT_IDX(sh_idx) = insertion_idx;

      if (insertion_idx == SCP_FIRST_SH_IDX(curr_scp_idx)) {           
         SCP_FIRST_SH_IDX(curr_scp_idx) = sh_idx; 
      }
      else {
         prev_idx = SH_PREV_IDX(insertion_idx);

         if (prev_idx != NULL_IDX) { 
            SH_PREV_IDX(sh_idx)		= prev_idx;
            SH_NEXT_IDX(prev_idx)	= sh_idx;
         }
      }

      SH_PREV_IDX(insertion_idx) = sh_idx;
   }
   else {
      SH_PREV_IDX(sh_idx)	= insertion_idx;
      next_idx			= SH_NEXT_IDX(insertion_idx);
 
      if (next_idx != NULL_IDX) {
         SH_NEXT_IDX(sh_idx)	= next_idx;
         SH_PREV_IDX(next_idx)	= sh_idx;
      }

      SH_NEXT_IDX(insertion_idx)    = sh_idx;
   }

   TRACE (Func_Exit, "gen_sh", NULL);

   return sh_idx;

}  /* gen_sh */
#else /* KEY Bug 4811 */
{
   int		next_idx;
   int		prev_idx;
   int		sh_idx;


   TRACE (Func_Entry, "gen_sh", NULL);

# ifdef _DEBUG
   if (defer_stmt_expansion) {
      PRINTMSG(line_num, 626, Internal, col_num,
               "no defer_stmt_expansion", "gen_sh");
   }

   if (curr_stmt_sh_idx == NULL_IDX) {
      PRINTMSG(line_num, 626, Internal, col_num,
               "valid curr_stmt_sh_idx", "gen_sh");
   }
# endif

   sh_idx		   = ntr_sh_tbl();
   SH_STMT_TYPE(sh_idx)	   = stmt_type;
   SH_GLB_LINE(sh_idx)	   = line_num;
   SH_ERR_FLG(sh_idx)	   = err_flag;
   SH_COL_NUM(sh_idx)	   = col_num;
   SH_LABELED(sh_idx)	   = labeled;
   SH_COMPILER_GEN(sh_idx) = compiler_gen;
   
   if (stmt_type == Construct_Def) {
      SH_P2_SKIP_ME(sh_idx) = TRUE;
   }

   if (position == Before) {
      SH_NEXT_IDX(sh_idx) = curr_stmt_sh_idx;

      if (curr_stmt_sh_idx == SCP_FIRST_SH_IDX(curr_scp_idx)) {           
         SCP_FIRST_SH_IDX(curr_scp_idx) = sh_idx; 
      }
      else {
         prev_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         if (prev_idx != NULL_IDX) { 
            SH_PREV_IDX(sh_idx)		= prev_idx;
            SH_NEXT_IDX(prev_idx)	= sh_idx;
         }
      }

      SH_PREV_IDX(curr_stmt_sh_idx) = sh_idx;
   }
   else {
      SH_PREV_IDX(sh_idx)	= curr_stmt_sh_idx;
      next_idx			= SH_NEXT_IDX(curr_stmt_sh_idx);
 
      if (next_idx != NULL_IDX) {
         SH_NEXT_IDX(sh_idx)	= next_idx;
         SH_PREV_IDX(next_idx)	= sh_idx;
      }

      SH_NEXT_IDX(curr_stmt_sh_idx)    = sh_idx;
      curr_stmt_sh_idx                 = sh_idx;
   }

   TRACE (Func_Exit, "gen_sh", NULL);

   return;

}  /* gen_sh */
#endif /* KEY Bug 4811 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Generate a new Statement Header (SH) either before or after the       *|
|*      current SH.                                                           *|
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

void gen_gl_sh(sh_position_type    position,
            stmt_type_type      stmt_type,
            int                 line_num,
            int                 col_num,
            boolean             err_flag,
            boolean             labeled,
            boolean             compiler_gen)

{
   int          next_idx;
   int          prev_idx;
   int          sh_idx;


   TRACE (Func_Entry, "gen_gl_sh", NULL);

   sh_idx                  = ntr_gl_sh_tbl();
   GL_SH_STMT_TYPE(sh_idx)    = stmt_type;
   GL_SH_GLB_LINE(sh_idx)     = line_num;
   GL_SH_ERR_FLG(sh_idx)      = err_flag;
   GL_SH_COL_NUM(sh_idx)      = col_num;
   GL_SH_LABELED(sh_idx)      = labeled;
   GL_SH_COMPILER_GEN(sh_idx) = compiler_gen;

   if (stmt_type == Construct_Def) {
      GL_SH_P2_SKIP_ME(sh_idx) = TRUE;
   }

   if (curr_gl_stmt_sh_idx == NULL_IDX) {

      if (global_stmt_sh_idx == NULL_IDX) {
         global_stmt_sh_idx = sh_idx;
      }
      curr_gl_stmt_sh_idx = sh_idx;
   }
   else if (position == Before) {

      GL_SH_NEXT_IDX(sh_idx) = curr_gl_stmt_sh_idx;

      if (curr_gl_stmt_sh_idx == global_stmt_sh_idx) {
         global_stmt_sh_idx = sh_idx;
      }
      else {
         prev_idx = GL_SH_PREV_IDX(curr_gl_stmt_sh_idx);

         if (prev_idx != NULL_IDX) {
            GL_SH_PREV_IDX(sh_idx)         = prev_idx;
            GL_SH_NEXT_IDX(prev_idx)       = sh_idx;
         }
      }

      GL_SH_PREV_IDX(curr_gl_stmt_sh_idx) = sh_idx;
   }
   else {
      GL_SH_PREV_IDX(sh_idx)    = curr_gl_stmt_sh_idx;
      next_idx                  = GL_SH_NEXT_IDX(curr_gl_stmt_sh_idx);

      if (next_idx != NULL_IDX) {
         GL_SH_NEXT_IDX(sh_idx)    = next_idx;
         GL_SH_PREV_IDX(next_idx)  = sh_idx;
      }

      GL_SH_NEXT_IDX(curr_gl_stmt_sh_idx) = sh_idx;
      curr_gl_stmt_sh_idx                 = sh_idx;
   }

   TRACE (Func_Exit, "gen_gl_sh", NULL);

   return;

}  /* gen_gl_sh */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      compare_ir takes 2 seperate ir streams and compares them.  If the     *|
|*      operators, types and operands are the same, the ir streams are the    *|
|*      same.  Different ir/il indexes are okay.  If the underlying ir is     *|
|*      is the same, then the ir is the same.                                 *|
|*									      *|
|* Input parameters:							      *|
|*      ir_idx1  -> Index to start of ir stream 1.                            *|
|*      ir_idx2  -> Index to start of ir stream 2.                            *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if the streams are exact matches, False otherwise.               *|
|*	NOTE:  The line and column numbers do not have to match.              *|
|*									      *|
\******************************************************************************/
static boolean	compare_ir(int	ir_idx1,
			   int	ir_idx2)
{
   boolean	matched;
   int		type1;
   int		type2;

   TRACE (Func_Entry, "compare_ir", NULL);


   /* BHJ - Try to optimize by using words, masks, and shifts rather than     */
   /*       field accesses.                                                   */


   type1 = IR_TYPE_IDX(ir_idx1);
   type2 = IR_TYPE_IDX(ir_idx2);

   if (IR_OPR(ir_idx1) == IR_OPR(ir_idx2) && type1 == type2 &&
       IR_FLD_L(ir_idx1) == IR_FLD_L(ir_idx2) &&
       IR_FLD_R(ir_idx1) == IR_FLD_R(ir_idx2)) {

      /* Operators, types, and fld types of both ir streams match.          */
      /* The indexes will match if they are both attribute or constant      */
      /* entries.  If they are IR or IL entries the indexes cannot match.   */
      /* Call appropriate compare routines to check.                        */
      /* Do the left side first, then the right side first.                 */

      if (IR_IDX_L(ir_idx1) == IR_IDX_L(ir_idx2)) {
         matched = TRUE;
      }
      else if (IR_FLD_L(ir_idx1) == IR_Tbl_Idx) { 
         matched = compare_ir(IR_IDX_L(ir_idx1), IR_IDX_L(ir_idx2));
      }
      else if (IR_FLD_L(ir_idx1) == IL_Tbl_Idx) {
         matched = compare_il(IR_IDX_L(ir_idx1), IR_IDX_L(ir_idx2),
                              IR_LIST_CNT_L(ir_idx1), IR_LIST_CNT_L(ir_idx2));
      }
      else {
         matched = FALSE;
      }

      if (matched) {  /* Try right side. */

         if (IR_IDX_R(ir_idx1) == IR_IDX_R(ir_idx2)) {
            matched = TRUE;
         }
         else if (IR_FLD_R(ir_idx1) == IR_Tbl_Idx) {
            matched = compare_ir(IR_IDX_R(ir_idx1), IR_IDX_R(ir_idx2));
         }
         else if (IR_FLD_R(ir_idx1) == IL_Tbl_Idx) {
            matched = compare_il(IR_IDX_R(ir_idx1), IR_IDX_R(ir_idx2),
                                 IR_LIST_CNT_R(ir_idx1),IR_LIST_CNT_R(ir_idx2));
         }
         else {
            matched = FALSE;
         }
      }
   }
   else {
      matched = FALSE;
   }

   TRACE (Func_Exit, "compare_ir", NULL);

   return(matched);

}  /* compare_ir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      compare_il takes 2 seperate il streams and compares them.  If the     *|
|*      list items are the same then the lists are the same.  Different       *|
|*      ir/il indexes are the same, as long as the underlying lists or ir     *|
|*      streams are the same.                                                 *|
|*									      *|
|* Input parameters:							      *|
|*      il_idx1  -> Index to start of ir stream 1.                            *|
|*      il_idx2  -> Index to start of ir stream 2.                            *|
|*	list_cnt1-> # of items in list 1.                                     *|
|*	list_cnt2-> # of items in list 2.                                     *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if the streams are exact matches, False otherwise.               *|
|*	NOTE:  The line and column numbers do not have to match.              *|
|*									      *|
\******************************************************************************/
static boolean	compare_il(int	il_idx1,
			   int	il_idx2,
                           int  list_cnt1,
                           int  list_cnt2)
{
   boolean	matched;


   TRACE (Func_Entry, "compare_il", NULL);

   if (list_cnt1 != list_cnt2) {
      matched = FALSE;
   }
   else {

      do {
         if (IL_FLD(il_idx1) == IL_FLD(il_idx2)) {

            /* These fields are the same.  Now check indexes.  If the list  */
            /* item is ir or another list, the indexes will be different,   */
            /* so call the appropriate compare routines.                    */

            if (IL_IDX(il_idx1) == IL_IDX(il_idx2)) {
               matched = TRUE;
            }
            else if (IL_FLD(il_idx1) == IR_Tbl_Idx) { 
               matched = compare_ir(IL_IDX(il_idx1), IL_IDX(il_idx2));
            }
            else if (IL_FLD(il_idx1) == IL_Tbl_Idx) {
               matched = compare_il(IL_IDX(il_idx1), IL_IDX(il_idx2),
                                    IL_LIST_CNT(il_idx1), IL_LIST_CNT(il_idx2));
            }
            else {
               matched = FALSE;
            }
         }
         else {
            matched = FALSE;
         }

         il_idx1 = IL_NEXT_LIST_IDX(il_idx1);
         il_idx2 = IL_NEXT_LIST_IDX(il_idx2);

         /* List counts are the same, so only have to check for one to go NULL*/
         /* If one goes NULL before the other, the compiler is messed up.     */

      }  while (matched && il_idx1 != NULL_IDX);
   }

   TRACE (Func_Exit, "compare_il", NULL);

   return(matched);

}  /* compare_il */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      free_ir_stream returns ir to the free list.                           *|
|*									      *|
|* Input parameters:							      *|
|*      ir_idx -> Start of ir stream to clear.                                *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
void	free_ir_stream(int	ir_idx)
{
   TRACE (Func_Entry, "free_ir_stream", NULL);

   if (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {
      free_ir_stream(IR_IDX_L(ir_idx));
   }
   else if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
      free_ir_list(IR_IDX_L(ir_idx));
   }
   
   if (IR_FLD_R(ir_idx) == IR_Tbl_Idx) {
      free_ir_stream(IR_IDX_R(ir_idx));
   }
   else if (IR_FLD_R(ir_idx) == IL_Tbl_Idx) {
      free_ir_list(IR_IDX_R(ir_idx));
   }

   FREE_IR_NODE(ir_idx);

   TRACE (Func_Exit, "free_ir_stream", NULL);

   return;

}  /* free_ir_stream */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      free_ir_list returns il to the free list.                             *|
|*									      *|
|* Input parameters:							      *|
|*      il_idx   -> Start of ir stream to clear.                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
void	free_ir_list(int	il_idx)
{
   int		next_il;


   TRACE (Func_Entry, "free_ir_list", NULL);

   while (il_idx != NULL_IDX) {

      if (IL_FLD(il_idx) == IR_Tbl_Idx) {
         free_ir_stream(IL_IDX(il_idx));
      }
      else if (IL_FLD(il_idx) == IL_Tbl_Idx) {
         free_ir_list(IL_IDX(il_idx));
      }

      next_il = IL_NEXT_LIST_IDX(il_idx);
      FREE_IR_LIST_NODE(il_idx);
      il_idx = next_il;
   }

   TRACE (Func_Exit, "free_ir_list", NULL);

   return;

}  /* free_ir_list */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create new ir, sh or il entries that are a copy of the input entry.   *|
|*									      *|
|* Input parameters:							      *|
|*	idx	table idx of entry to be copied.                              *|
|*      fld     fld type of idx.                                              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	idx of new entry, same fld type as the input idx.                     *|
|*									      *|
\******************************************************************************/

void copy_subtree(opnd_type	*old_opnd,
                  opnd_type	*new_opnd)

{
   int		idx;
   int		list_idx;
   int		list2_idx;
   int		new_root = NULL_IDX;
   opnd_type	opnd_o;
   opnd_type	opnd_n;


   TRACE (Func_Entry, "copy_subtree", NULL);

   COPY_OPND((*new_opnd), (*old_opnd));
   idx = OPND_IDX((*old_opnd));

   if (idx) {

      switch(OPND_FLD((*old_opnd))) {

         case NO_Tbl_Idx   :
            break;

         case IR_Tbl_Idx :
            
            if (IR_OPR(idx) == Stmt_Expansion_Opr) {
               /* just copy the left opnd */

               COPY_OPND(opnd_o, IR_OPND_L(idx));
               copy_subtree(&opnd_o, &opnd_n);
               COPY_OPND((*new_opnd), opnd_n);
            }
            else {
               NTR_IR_TBL(new_root);

               COPY_TBL_NTRY(ir_tbl, new_root, idx);
               OPND_IDX((*new_opnd)) = new_root;

               COPY_OPND(opnd_o, IR_OPND_L(idx));
               copy_subtree(&opnd_o, &opnd_n);
               COPY_OPND(IR_OPND_L(new_root), opnd_n);

               COPY_OPND(opnd_o, IR_OPND_R(idx));
               copy_subtree(&opnd_o, &opnd_n);
               COPY_OPND(IR_OPND_R(new_root), opnd_n);
            }
            break;

         case AT_Tbl_Idx :

            if (gen_lbl_copy &&
                AT_OBJ_CLASS(idx) == Label &&
                ATL_CLASS(idx) == Lbl_Internal) {

               if (ATL_NEW_LBL_IDX(idx) == NULL_IDX) {
                  /* gen a new label */
                  ATL_NEW_LBL_IDX(idx) = gen_internal_lbl(AT_DEF_LINE(idx));

                  NTR_ATTR_LIST_TBL(list_idx);
                  AL_ATTR_IDX(list_idx) = idx;

                  AL_NEXT_IDX(list_idx) = label_copy_al_idx;

                  label_copy_al_idx = list_idx;
               }

               OPND_IDX((*new_opnd)) = ATL_NEW_LBL_IDX(idx);
            }
            break;

         case CN_Tbl_Idx :
            break;

         case SH_Tbl_Idx :

            new_root	= ntr_sh_tbl();

            COPY_TBL_NTRY(sh_tbl, new_root, idx);

            OPND_FLD(opnd_o) = IR_Tbl_Idx;
            OPND_IDX(opnd_o) = SH_IR_IDX(idx);
            copy_subtree(&opnd_o, &opnd_n);
            SH_IR_IDX(new_root) = OPND_IDX(opnd_n);
            OPND_IDX((*new_opnd)) = new_root;
            break;

         case IL_Tbl_Idx :

            NTR_IR_LIST_TBL(new_root);

            COPY_TBL_NTRY(ir_list_tbl, new_root, idx);
            OPND_IDX((*new_opnd)) = new_root;

            COPY_OPND(opnd_o, IL_OPND(idx));
            copy_subtree(&opnd_o, &opnd_n);
            COPY_OPND(IL_OPND(new_root), opnd_n);

            list2_idx        = new_root;

            idx              = IL_NEXT_LIST_IDX(idx);

            while (idx != NULL_IDX) {
               NTR_IR_LIST_TBL(list_idx);

               COPY_TBL_NTRY(ir_list_tbl, list_idx, idx);

               if (! IL_ARG_DESC_VARIANT(idx)) {
                  IL_PREV_LIST_IDX(list_idx)  = list2_idx;
               }

               IL_NEXT_LIST_IDX(list2_idx) = list_idx;
               list2_idx                   = list_idx;

               COPY_OPND(opnd_o, IL_OPND(idx));
               copy_subtree(&opnd_o, &opnd_n);
               COPY_OPND(IL_OPND(list_idx), opnd_n);

               idx              = IL_NEXT_LIST_IDX(idx);
            }
            break;
      }
   }

   TRACE (Func_Exit, "copy_subtree", NULL);

   return;

}  /* copy_subtree */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Create new ir, sh or il entries that are a copy of the input entry.   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      idx     table idx of entry to be copied.                              *|
|*      fld     fld type of idx.                                              *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      idx of new entry, same fld type as the input idx.                     *|
|*                                                                            *|
\******************************************************************************/

int copy_to_gl_subtree(int      idx,
                 fld_type fld)

{
   int          new_idx;
   int          list_idx;
   int          list2_idx;
   int		name_idx;
   int          new_root = NULL_IDX;


   TRACE (Func_Entry, "copy_to_gl_subtree", NULL);

   if (idx != NULL_IDX) {

      switch(fld) {

         case NO_Tbl_Idx   :
            break;

         case IR_Tbl_Idx :

            NTR_GL_IR_TBL(new_root);

            COPY_GL_TBL_NTRY(global_ir_tbl, ir_tbl, new_root, idx);

            new_idx = copy_to_gl_subtree(IR_IDX_L(idx), IR_FLD_L(idx));
            GL_IR_IDX_L(new_root) = new_idx;

            new_idx = copy_to_gl_subtree(IR_IDX_R(idx), IR_FLD_R(idx));
            GL_IR_IDX_R(new_root) = new_idx;

            break;

         case AT_Tbl_Idx :

            if (srch_global_name_tbl(AT_OBJ_NAME_PTR(idx),
                            AT_NAME_LEN(idx),
                            &name_idx)) {
            }
            else {
               ntr_global_name_tbl(idx, NULL_IDX, name_idx);
            }

            new_root = GN_ATTR_IDX(name_idx);
            break;

         case CN_Tbl_Idx :

            /* Until we have a global constant table, I'm using this big */
            /* KLUDGE. This assumes that any constant will be integer and*/
            /* will fit in 24 bits.                                      */

# ifdef _DEBUG
            if (TYP_TYPE(CN_TYPE_IDX(idx)) != Integer) {
               PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                        "Integer constant", "copy_to_gl_subtree");
            }
# endif

            new_root = CN_INT_TO_C(idx);  /* KAY - BRIANJ */
            break;

         case SH_Tbl_Idx :

            new_root    = ntr_gl_sh_tbl();

            COPY_GL_TBL_NTRY(global_sh_tbl, sh_tbl, new_root, idx);

            new_idx = copy_to_gl_subtree(SH_IR_IDX(idx), IR_Tbl_Idx);
            GL_SH_IR_IDX(new_root) = new_idx;
            break;

         case IL_Tbl_Idx :

            NTR_GL_IR_LIST_TBL(new_root);

            COPY_GL_TBL_NTRY(global_ir_list_tbl, ir_list_tbl, new_root, idx);

            new_idx = copy_to_gl_subtree(IL_IDX(idx), IL_FLD(idx));
            GL_IL_IDX(new_root) = new_idx;
            list2_idx        = new_root;

            idx              = IL_NEXT_LIST_IDX(idx);

            while (idx != NULL_IDX) {
               NTR_GL_IR_LIST_TBL(list_idx);

               COPY_GL_TBL_NTRY(global_ir_list_tbl, ir_list_tbl, list_idx, idx);

               if (! IL_ARG_DESC_VARIANT(idx)) {
                  GL_IL_PREV_LIST_IDX(list_idx)  = list2_idx;
               }

               GL_IL_NEXT_LIST_IDX(list2_idx) = list_idx;
               list2_idx                   = list_idx;

               new_idx = copy_to_gl_subtree(IL_IDX(idx), IL_FLD(idx));
               GL_IL_IDX(list_idx) = new_idx;
               idx              = IL_NEXT_LIST_IDX(idx);
            }
            break;
      }
   }

   TRACE (Func_Exit, "copy_to_gl_subtree", NULL);

   return(new_root);

}  /* copy_to_gl_subtree */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Create new ir, sh or il entries that are a copy of the input entry.   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      idx     table idx of entry to be copied.                              *|
|*      fld     fld type of idx.                                              *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      idx of new entry, same fld type as the input idx.                     *|
|*                                                                            *|
\******************************************************************************/

int copy_from_gl_subtree(int      idx,
                 fld_type fld)

{
   int          new_idx;
   int          list_idx;
   int          list2_idx;
   int		name_idx;
   int          new_root = NULL_IDX;
   long_type	the_constant;


   TRACE (Func_Entry, "copy_from_gl_subtree", NULL);

   if (idx != NULL_IDX) {

      switch(fld) {

         case NO_Tbl_Idx   :
            break;

         case IR_Tbl_Idx :

            NTR_IR_TBL(new_root);

            COPY_GL_TBL_NTRY(ir_tbl, global_ir_tbl, new_root, idx);

            new_idx = copy_from_gl_subtree(GL_IR_IDX_L(idx), GL_IR_FLD_L(idx));
            IR_IDX_L(new_root) = new_idx;

            new_idx = copy_from_gl_subtree(GL_IR_IDX_R(idx), GL_IR_FLD_R(idx));
            IR_IDX_R(new_root) = new_idx;

            break;

         case AT_Tbl_Idx :

            new_root = srch_sym_tbl(GA_OBJ_NAME_PTR(idx),
                                    GA_NAME_LEN(idx),
                                    &name_idx);

            /* it had better be there */
            break;

         case CN_Tbl_Idx :

            /* Until we have a global constant table, I'm using this big */
            /* KLUDGE. This assumes that any constant will be integer and*/
            /* will fit in 24 bits.                                      */

            /* BRIANJ */

            the_constant = idx;
            new_root = ntr_const_tbl(CG_INTEGER_DEFAULT_TYPE,
                                     FALSE,
                                    &the_constant);

            break;

         case SH_Tbl_Idx :

            new_root    = ntr_sh_tbl();

            COPY_GL_TBL_NTRY(sh_tbl, global_sh_tbl, new_root, idx);

            new_idx = copy_from_gl_subtree(GL_SH_IR_IDX(idx), IR_Tbl_Idx);
            SH_IR_IDX(new_root) = new_idx;

            SH_COMPILER_GEN(new_root) = TRUE;
            break;

         case IL_Tbl_Idx :

            NTR_IR_LIST_TBL(new_root);

            COPY_GL_TBL_NTRY(ir_list_tbl, global_ir_list_tbl, new_root, idx);

            new_idx = copy_from_gl_subtree(GL_IL_IDX(idx), GL_IL_FLD(idx));
            IL_IDX(new_root) = new_idx;
            list2_idx        = new_root;

            idx              = GL_IL_NEXT_LIST_IDX(idx);

            while (idx != NULL_IDX) {
               NTR_IR_LIST_TBL(list_idx);

               COPY_GL_TBL_NTRY(ir_list_tbl, global_ir_list_tbl, list_idx, idx);

               if (! GL_IL_ARG_DESC_VARIANT(idx)) {
                  IL_PREV_LIST_IDX(list_idx)  = list2_idx;
               }

               IL_NEXT_LIST_IDX(list2_idx) = list_idx;
               list2_idx                   = list_idx;

               new_idx = copy_from_gl_subtree(GL_IL_IDX(idx), GL_IL_FLD(idx));
               IL_IDX(list_idx) = new_idx;
               idx              = GL_IL_NEXT_LIST_IDX(idx);
            }
            break;
      }
   }

   TRACE (Func_Exit, "copy_from_gl_subtree", NULL);

   return(new_root);

}  /* copy_from_gl_subtree */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Insert a stmt header chain at the current scps main entry and then    *|
|*      copy it at every other entry.                                         *|
|*      It puts the original chain after the main entry.                      *|
|*									      *|
|* Input parameters:							      *|
|*	head_idx, tail_idx - indexes of beginning and end of chain.           *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void insert_sh_chain_after_entries(int	head_idx,
                                   int	tail_idx)

{
   int		entry_idx;
   int		entry_sh_idx;
   int		idx;
   int		next_sh_idx;
   int		new_end_sh_idx;
   int		new_start_sh_idx;


   TRACE (Func_Entry, "insert_sh_chain_after_entries", NULL);

   SH_NEXT_IDX(tail_idx) = SH_NEXT_IDX(SCP_FIRST_SH_IDX(curr_scp_idx));
   SH_PREV_IDX(head_idx) = SCP_FIRST_SH_IDX(curr_scp_idx);
   SH_NEXT_IDX(SCP_FIRST_SH_IDX(curr_scp_idx)) = head_idx;
   SH_PREV_IDX(SH_NEXT_IDX(tail_idx))          = tail_idx;

   idx    = SCP_ENTRY_IDX(curr_scp_idx);

   while (idx != NULL_IDX) {
      /* copy sh chain after each entry opr */

      entry_idx = AL_ATTR_IDX(idx);

      entry_sh_idx           = ATP_FIRST_SH_IDX(entry_idx);
      next_sh_idx            = SH_NEXT_IDX(entry_sh_idx);

      copy_entry_exit_sh_list(head_idx,
                              tail_idx,
                             &new_start_sh_idx,
                             &new_end_sh_idx);

      SH_NEXT_IDX(entry_sh_idx)     = new_start_sh_idx;
      SH_PREV_IDX(new_start_sh_idx) = entry_sh_idx;

      SH_PREV_IDX(next_sh_idx) = new_end_sh_idx;
      SH_NEXT_IDX(new_end_sh_idx) = next_sh_idx;

      idx = AL_NEXT_IDX(idx);
   }


   TRACE (Func_Exit, "insert_sh_chain_after_entries", NULL);

   return;

}  /* insert_sh_chain_after_entries */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	inserts an sh chain Before or After curr_stmt_sh_idx.                 *|
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

void insert_sh_chain(int		start_idx,
		     int		end_idx,
                     sh_position_type   position)

{


   TRACE (Func_Entry, "insert_sh_chain", NULL);

   if (position == Before) {
      if (SH_PREV_IDX(curr_stmt_sh_idx) != NULL_IDX) {
         SH_NEXT_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = start_idx;
      }
      SH_PREV_IDX(start_idx) = SH_PREV_IDX(curr_stmt_sh_idx);

      SH_PREV_IDX(curr_stmt_sh_idx) = end_idx;
      SH_NEXT_IDX(end_idx)          = curr_stmt_sh_idx;
   }
   else {
      SH_PREV_IDX(start_idx) = curr_stmt_sh_idx;
      SH_NEXT_IDX(end_idx) = SH_NEXT_IDX(curr_stmt_sh_idx);
      if (SH_NEXT_IDX(curr_stmt_sh_idx) != NULL_IDX) {
         SH_PREV_IDX(SH_NEXT_IDX(curr_stmt_sh_idx)) = end_idx;
      }
      SH_NEXT_IDX(curr_stmt_sh_idx) = start_idx;
   }

   TRACE (Func_Exit, "insert_sh_chain", NULL);

   return;

}  /* insert_sh_chain */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      compare_opnds is the driver for compare_il and compare_ir.  It takes  *|
|*      as input 2 operands.  First the fields are checked.  If they are      *|
|*      different, FALSE is returned.  If they are the same, then if they     *|
|*      are IR entries, compare_ir is called.  If they are list entries,      *|
|*      compare_il is called.  Otherwise, if the indexes are the same,        *|
|*      TRUE is returned.                                                     *|
|*									      *|
|* Input parameters:							      *|
|*      opnd1  -> Ptr to first operand to compare.                            *|
|*      opnd2  -> Ptr to second operand to compare.                           *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if the operands are the same.                                    *|
|*									      *|
\******************************************************************************/
boolean	compare_opnds(opnd_type		*opnd1,
	              opnd_type		*opnd2)
{
   boolean	matched;

   TRACE (Func_Entry, "compare_opnds", NULL);

   if (OPND_FLD((*opnd1)) != OPND_FLD((*opnd2))) {
      matched = FALSE;
   }
   else {

      switch (OPND_FLD((*opnd1))) {

         case IR_Tbl_Idx:
            matched = compare_ir(OPND_IDX((*opnd1)), OPND_IDX((*opnd2)));
            break;

         case IL_Tbl_Idx:
            matched = compare_il(OPND_IDX((*opnd1)),
                                 OPND_IDX((*opnd2)),
                                 OPND_LIST_CNT((*opnd1)),
                                 OPND_LIST_CNT((*opnd2)));
            break;

         default:
            matched = OPND_IDX((*opnd1)) == OPND_IDX((*opnd2));
            break;
      }
   }

   TRACE (Func_Exit, "compare_opnds", NULL);

   return(matched);

}  /* compare_opnds */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine inserts an sh chain before the curr_stmt_sh_idx.         *|
|*      It searches from the input sh idx to find the beginning and end       *|
|*      of the chain.                                                         *|
|*									      *|
|* Input parameters:							      *|
|*	sh_idx - idx of some sh node within the chain.                        *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void insert_sh_chain_before(int		sh_idx)

{
   int		start_idx;
   int		end_idx;

   TRACE (Func_Entry, "insert_sh_chain_before", NULL);

   start_idx = sh_idx;
   while (SH_PREV_IDX(start_idx)) {
      start_idx = SH_PREV_IDX(start_idx);
   }

   end_idx = sh_idx;
   while (SH_NEXT_IDX(end_idx)) {
      end_idx = SH_NEXT_IDX(end_idx);
   }

   insert_sh_chain(start_idx, end_idx, Before);

   TRACE (Func_Exit, "insert_sh_chain_before", NULL);

   return;

}  /* insert_sh_chain_before */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Go through an ir stream, looking for the specified attr. 	      *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx	- attr_idx to search for.			      *|
|*	ir_idx		- ir to search.					      *|
|*									      *|
|* Output parameters:							      *|
|*	*opnd		- Place to copy ir information to, when it's found.   *|
|*									      *|
|* Returns:								      *|
|*	TRUE if attr is found, else FALSE				      *|
|*									      *|
\******************************************************************************/

boolean	find_attr_in_ir(int		 attr_idx,
			int		 ir_idx,
			opnd_type	*opnd)
{

   TRACE (Func_Entry, "find_attr_in_ir", NULL);

   switch (IR_FLD_L(ir_idx)) {
   case AT_Tbl_Idx:

      if (IR_IDX_L(ir_idx) == attr_idx) {
         COPY_OPND((*opnd), IR_OPND_L(ir_idx));
         return(TRUE);
      }
      break;

   case IR_Tbl_Idx:

      if (find_attr_in_ir(attr_idx, IR_IDX_L(ir_idx), opnd)) {
         return(TRUE);
      }
      break;

   case IL_Tbl_Idx:

      if (find_attr_in_il(attr_idx, IR_IDX_L(ir_idx), opnd)) {
         return(TRUE);
      }
      break;

   case CN_Tbl_Idx:
   case NO_Tbl_Idx:
   case SH_Tbl_Idx:
      break;
   }

   switch (IR_FLD_R(ir_idx)) {
   case AT_Tbl_Idx:

      if (IR_IDX_R(ir_idx) == attr_idx) {
         COPY_OPND((*opnd), IR_OPND_R(ir_idx));
         return(TRUE);
      }
      break;

   case IR_Tbl_Idx:

      if (find_attr_in_ir(attr_idx, IR_IDX_R(ir_idx), opnd)) {
         return(TRUE);
      }
      break;

   case IL_Tbl_Idx:

      if (find_attr_in_il(attr_idx, IR_IDX_R(ir_idx), opnd)) {
         return(TRUE);
      }
      break;

   case CN_Tbl_Idx:
   case NO_Tbl_Idx:
   case SH_Tbl_Idx:
      break;
   }

   TRACE (Func_Exit, "find_attr_in_ir", NULL);

   return(FALSE);

}  /* find_attr_in_ir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Go through an il, looking for the specified attr.	 	      *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx	- attr_idx to search for.			      *|
|*	il_idx		- il to search.					      *|
|*									      *|
|* Output parameters:							      *|
|*	*opnd		- Place to copy il information to, when it's found.   *|
|*									      *|
|* Returns:								      *|
|*	TRUE if attr is found, else FALSE				      *|
|*									      *|
\******************************************************************************/

boolean	find_attr_in_il(int		 attr_idx,
			int		 il_idx,
			opnd_type	*opnd)
{

   TRACE (Func_Entry, "find_attr_in_il", NULL);

   while (il_idx != NULL_IDX) {

      switch (IL_FLD(il_idx)) {

      case AT_Tbl_Idx:

         if (IL_IDX(il_idx) == attr_idx) {
            COPY_OPND((*opnd), IL_OPND(il_idx));
            return(TRUE);
         }
         break;

      case IR_Tbl_Idx:

         if (find_attr_in_ir(attr_idx, IL_IDX(il_idx), opnd)) {
            return(TRUE);
         }
         break;

      case IL_Tbl_Idx:

         if (find_attr_in_il(attr_idx, IL_IDX(il_idx), opnd)) {
            return(TRUE);
         }
         break;

      case CN_Tbl_Idx:
      case NO_Tbl_Idx:
      case SH_Tbl_Idx:
         break;
      }

      il_idx = IL_NEXT_LIST_IDX(il_idx);
   }

   TRACE (Func_Exit, "find_attr_in_il", NULL);

   return(FALSE);

}  /* find_attr_in_il */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	expr_is_symbolic_constant checks the expression and returns TRUE      *|
|*	if this expression meets the criteria for a symbolic constant.        *|
|*	If this expression is a symbolic constant, the operators are changed. *|
|*	A correct expression contains only plus, minus, multiply, divide,     *|
|*	unary plus and unary minus.                                           *|
|*									      *|
|*	Assumption:  This is an integer expression.                           *|
|*									      *|
|* Input parameters:							      *|
|*	*opnd - A ptr to the expression to check.			      *|
|*									      *|
|* Output parameters:							      *|
|*	*opnd - Will contain an expression with correct symbolic constant     *|
|*	        operators.                                                    *|
|*									      *|
|* Returns:								      *|
|*	TRUE if this is a symbolic constant, else FALSE.		      *|
|*									      *|
\******************************************************************************/
boolean	expr_is_symbolic_constant(opnd_type	*opnd)

{
   int		idx;
   opnd_type	loc_opnd;
   opnd_type	new_opnd;
   boolean	symbolic;


   TRACE (Func_Entry, "expr_is_symbolic_constant", NULL);

   switch (OPND_FLD((*opnd))) {
   case CN_Tbl_Idx:
      symbolic	= TRUE;
      break;

   case IL_Tbl_Idx:
      COPY_OPND(loc_opnd, (*opnd));
      copy_subtree(&loc_opnd, &new_opnd);
      idx = OPND_IDX(new_opnd);
      symbolic	= il_is_symbolic_constant(idx);

      if (symbolic) {
         OPND_IDX((*opnd)) = idx;
      }
      else {
         free_ir_list(idx);
      }
      break;

   case IR_Tbl_Idx:
      COPY_OPND(loc_opnd, (*opnd));
      copy_subtree(&loc_opnd, &new_opnd);
      idx = OPND_IDX(new_opnd);
      symbolic	= ir_is_symbolic_constant(idx);

      if (symbolic) {
         OPND_IDX((*opnd)) = idx;
      }
      else {
         free_ir_stream(idx);
      }
      break;

   case AT_Tbl_Idx:
      symbolic = (AT_OBJ_CLASS(OPND_IDX((*opnd))) == Data_Obj &&
                  (ATD_SYMBOLIC_CONSTANT(OPND_IDX((*opnd))) ||
                  (ATD_CLASS(OPND_IDX((*opnd))) == Constant &&
                   ATD_FLD(OPND_IDX((*opnd))) == CN_Tbl_Idx)));
      break;

   case SH_Tbl_Idx:
      symbolic	= FALSE;
      break;

   default:
      symbolic	= FALSE;
      break;
   }

   TRACE (Func_Exit, "expr_is_symbolic_constant", NULL);

   return(symbolic);

}  /* expr_is_symbolic_constant */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Go through an ir stream, checking to see if it is a symbolic constant *|
|*									      *|
|* Input parameters:							      *|
|*	ir_idx		- ir to search.					      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE		     			  			      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if ir stream is a symbolic constant.  FALSE otherwise.           *|
|*									      *|
\******************************************************************************/
static	boolean	ir_is_symbolic_constant(int		 ir_idx)

{
   boolean	symbolic;


   TRACE (Func_Entry, "ir_is_symbolic_constant", NULL);

   switch (IR_OPR(ir_idx)) {
   case Mult_Opr:
      IR_OPR(ir_idx)	= Symbolic_Mult_Opr;
      break;

   case Div_Opr:
      IR_OPR(ir_idx)	= Symbolic_Div_Opr;
      break;

   case Uplus_Opr:
      IR_OPR(ir_idx)	= Symbolic_Uplus_Opr;
      break;

   case Uminus_Opr:
      IR_OPR(ir_idx)	= Symbolic_Uminus_Opr;
      break;

   case Plus_Opr:
      IR_OPR(ir_idx)	= Symbolic_Plus_Opr;
      break;

   case Minus_Opr:
      IR_OPR(ir_idx)	= Symbolic_Minus_Opr;
      break;

   default:
      symbolic = FALSE;
      goto EXIT;
   }

   switch (IR_FLD_L(ir_idx)) {
   case AT_Tbl_Idx:
      symbolic = (AT_OBJ_CLASS(IR_IDX_L(ir_idx)) == Data_Obj &&
                  (ATD_SYMBOLIC_CONSTANT(IR_IDX_L(ir_idx)) ||
                  (ATD_CLASS(IR_IDX_L(ir_idx)) == Constant &&
                   ATD_FLD(IR_IDX_L(ir_idx)) == CN_Tbl_Idx)));
      break;

   case IR_Tbl_Idx:
      symbolic = ir_is_symbolic_constant(IR_IDX_L(ir_idx));
      break;

   case IL_Tbl_Idx:
      symbolic = il_is_symbolic_constant(IR_IDX_L(ir_idx));
      break;

   case CN_Tbl_Idx:
   case NO_Tbl_Idx:
      symbolic	= TRUE;
      break;

   case SH_Tbl_Idx:
   default:
      symbolic	= FALSE;
      break;
   }

   if (symbolic) {

      switch (IR_FLD_R(ir_idx)) {
      case AT_Tbl_Idx:
         symbolic = (AT_OBJ_CLASS(IR_IDX_R(ir_idx)) == Data_Obj &&
                     (ATD_SYMBOLIC_CONSTANT(IR_IDX_R(ir_idx)) ||
                     (ATD_CLASS(IR_IDX_R(ir_idx)) == Constant &&
                      ATD_FLD(IR_IDX_R(ir_idx)) == CN_Tbl_Idx)));
         break;

      case IR_Tbl_Idx:
         symbolic = ir_is_symbolic_constant(IR_IDX_R(ir_idx));
         break;

      case IL_Tbl_Idx:
         symbolic = il_is_symbolic_constant(IR_IDX_R(ir_idx));
         break;

      case CN_Tbl_Idx:
      case NO_Tbl_Idx:
         symbolic = TRUE;
         break;

      case SH_Tbl_Idx:
      default:
         symbolic = FALSE;
         break;
      }
   }

   EXIT:

   TRACE (Func_Exit, "ir_is_symbolic_constant", NULL);

   return(symbolic);

}  /* ir_is_symbolic_constant */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Go through an il stream, checking to see if it is a symbolic constant *|
|*									      *|
|* Input parameters:							      *|
|*	il_idx		- il to search.					      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if il stream is a symbolic constant.  FALSE otherwise.           *|
|*									      *|
\******************************************************************************/
static	boolean	il_is_symbolic_constant(int	 il_idx)
{
   boolean	symbolic	= TRUE;


   TRACE (Func_Entry, "il_is_symbolic_constant", NULL);

   while (symbolic == TRUE && il_idx != NULL_IDX) {

      switch (IL_FLD(il_idx)) {

      case AT_Tbl_Idx:
         symbolic = (AT_OBJ_CLASS(IL_IDX(il_idx)) == Data_Obj &&
                     (ATD_SYMBOLIC_CONSTANT(IL_IDX(il_idx)) ||
                     (ATD_CLASS(IL_IDX(il_idx)) == Constant &&
                      ATD_FLD(IL_IDX(il_idx)) == CN_Tbl_Idx)));
         break;

      case IR_Tbl_Idx:
         symbolic = ir_is_symbolic_constant(IL_IDX(il_idx));
         break;

      case IL_Tbl_Idx:
         symbolic = il_is_symbolic_constant(IL_IDX(il_idx));
         break;

      case CN_Tbl_Idx:
      case NO_Tbl_Idx:
         symbolic = TRUE;
         break;

      case SH_Tbl_Idx:
      default:
         symbolic = FALSE;
         break;
      }

      il_idx = IL_NEXT_LIST_IDX(il_idx);
   }

   TRACE (Func_Exit, "il_is_symbolic_constant", NULL);

   return(symbolic);

}  /* il_is_symbolic_constant */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine copies a linked list of statements and replaces any      *|
|*	internal label with a new internal label.                             *|
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

void copy_entry_exit_sh_list(int	start_sh_idx,
			     int	end_sh_idx,
			     int	*new_start_sh_idx,
			     int	*new_end_sh_idx)

{
   int		al_idx;
   int		new_sh_idx;
   opnd_type	opnd_o;
   opnd_type	opnd_n;
   int		sh_idx;

   TRACE (Func_Entry, "copy_entry_exit_sh_list", NULL);

   sh_idx = start_sh_idx;
   *new_start_sh_idx = NULL_IDX;
   label_copy_al_idx = NULL_IDX;
   gen_lbl_copy = TRUE;

   while (sh_idx != SH_NEXT_IDX(end_sh_idx)) {

      /* do not copy init statements. These are possibly */
      /* generated for runtime argument checking.        */

      if (IR_OPR(SH_IR_IDX(sh_idx)) != Init_Opr &&
          IR_OPR(SH_IR_IDX(sh_idx)) != Init_Reloc_Opr) {

         OPND_FLD(opnd_o) = SH_Tbl_Idx;
         OPND_IDX(opnd_o) = sh_idx;
         copy_subtree(&opnd_o, &opnd_n);
         new_sh_idx = OPND_IDX(opnd_n);

         if (*new_start_sh_idx == NULL_IDX) {
            *new_start_sh_idx = new_sh_idx;
            SH_PREV_IDX((*new_start_sh_idx)) = NULL_IDX;
            SH_NEXT_IDX((*new_start_sh_idx)) = NULL_IDX;
            *new_end_sh_idx = new_sh_idx;
         }
         else {
            SH_NEXT_IDX((*new_end_sh_idx)) = new_sh_idx;
            SH_PREV_IDX(new_sh_idx) = *new_end_sh_idx;
            SH_NEXT_IDX(new_sh_idx) = NULL_IDX;
            *new_end_sh_idx = new_sh_idx;
         }
      }

      sh_idx = SH_NEXT_IDX(sh_idx);
   }

   gen_lbl_copy = FALSE;
   al_idx	= label_copy_al_idx;

   while (al_idx != NULL_IDX) {
      ATL_NEW_LBL_IDX(AL_ATTR_IDX(al_idx))	= NULL_IDX;
      al_idx					= AL_NEXT_IDX(al_idx);
   }

   free_attr_list(label_copy_al_idx);

   label_copy_al_idx = NULL_IDX;

   TRACE (Func_Exit, "copy_entry_exit_sh_list", NULL);

   return;

}  /* copy_entry_exit_sh_list */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Generate an Stmt_Expansion_Opr. They use this form ...                *|
|*                                                                            *|
|*                  (Stmt_Expansion_Opr)                                      *|
|*                 /                    \                                     *|
|*           result temp                 |-> Before list, start (SH_Tbl_Idx)  *|
|*              opnd                     |                                    *|
|*                                       |-> Before list, end (SH_Tbl_Idx)    *|
|*                                       |                                    *|
|*                                       |-> After list, start (SH_Tbl_Idx)   *|
|*                                       |                                    *|
|*                                       |-> After list, end (SH_Tbl_Idx)     *|
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

int	gen_stmt_expansion_opr(int	line,
                               int	col)

{
   int		ir_idx;
   int		list_idx;

   TRACE (Func_Entry, "gen_stmt_expansion_opr", NULL);

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Stmt_Expansion_Opr;
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx) = col;

   NTR_IR_LIST_TBL(list_idx);
   IL_FLD(list_idx) = SH_Tbl_Idx;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx) = col;
   IR_FLD_R(ir_idx) = IL_Tbl_Idx;
   IR_IDX_R(ir_idx) = list_idx;
   IR_LIST_CNT_R(ir_idx) = 4;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);
   IL_FLD(list_idx) = SH_Tbl_Idx;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx) = col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);
   IL_FLD(list_idx) = SH_Tbl_Idx;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx) = col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);
   IL_FLD(list_idx) = SH_Tbl_Idx;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx) = col;

   if (defer_stmt_expansion) {
      number_of_functions++;
   }

   TRACE (Func_Exit, "gen_stmt_expansion_opr", NULL);

   return(ir_idx);

}  /* gen_stmt_expansion_opr */

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

void free_stmt_expansion_opr(int	ir_idx)

{
   int		free_list;
   int		list_idx;

   TRACE (Func_Entry, "free_stmt_expansion_opr", NULL);

   list_idx = IR_IDX_R(ir_idx);
   while (list_idx) {
      free_list = list_idx;

      list_idx = IL_NEXT_LIST_IDX(list_idx);
      FREE_IR_LIST_NODE(free_list);
   }

   FREE_IR_NODE(ir_idx);

   TRACE (Func_Exit, "free_stmt_expansion_opr", NULL);

   return;

}  /* free_stmt_expansion_opr */

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

int     gen_ir(fld_type         l_fld,
               int              l_idx,
               operator_type    opr,
               int              type_idx,
               int              line,
               int              col,
               fld_type         r_fld,
               int              r_idx)

{

   int		cnt;
   int          ir_idx;
   int		list_idx;
   int          rank_l = 0;
   int          rank_r = 0;

   TRACE (Func_Entry, "gen_ir", NULL);

# ifdef _DEBUG
   if (opr == Subscript_Opr ||
       opr == Whole_Subscript_Opr ||
       opr == Section_Subscript_Opr ||
       opr == Substring_Opr ||
       opr == Whole_Substring_Opr ||
       opr == Struct_Opr) {

      PRINTMSG(line, 626, Internal, col,
               "no reference operator",
               "gen_ir");
   }

   if ((opr == Loc_Opr || opr == Aloc_Opr) &&
       (l_fld == CN_Tbl_Idx ||
        l_fld == SB_Tbl_Idx ||
        l_fld == IL_Tbl_Idx ||
        l_fld == SH_Tbl_Idx ||
        (l_fld == IR_Tbl_Idx &&
         IR_OPR(l_idx) != Struct_Opr &&
         IR_OPR(l_idx) != Dv_Deref_Opr &&
         IR_OPR(l_idx) != Subscript_Opr &&
         IR_OPR(l_idx) != Whole_Subscript_Opr &&
         IR_OPR(l_idx) != Section_Subscript_Opr &&
         IR_OPR(l_idx) != Substring_Opr &&
         IR_OPR(l_idx) != Whole_Substring_Opr))) {

      PRINTMSG(line, 626, Internal, col,
               "valid LOC opr",
               "gen_ir");
   }
        
# endif

   NTR_IR_TBL(ir_idx);
   IR_TYPE_IDX(ir_idx)   = type_idx;
   IR_OPR(ir_idx)        = opr;
   IR_LINE_NUM(ir_idx)   = line;
   IR_COL_NUM(ir_idx)    = col;

   if (l_fld != NO_Tbl_Idx) {
      IR_LINE_NUM_L(ir_idx) = line;
      IR_COL_NUM_L(ir_idx)  = col;
      IR_FLD_L(ir_idx)      = l_fld;
      IR_IDX_L(ir_idx)      = l_idx;

      if (l_fld == IR_Tbl_Idx) {
         rank_l = IR_RANK(l_idx);
      }
      else if (l_fld == IL_Tbl_Idx) {
         list_idx = l_idx;
         cnt = 0;
         while (list_idx) {

            if (IL_FLD(list_idx) == IR_Tbl_Idx &&
                IR_RANK(IL_IDX(list_idx)) > rank_l) {
               rank_l = IR_RANK(IL_IDX(list_idx));
            }
            cnt++;
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         IR_LIST_CNT_L(ir_idx) = cnt;
      }
   }

   if (r_fld != NO_Tbl_Idx) {
      IR_LINE_NUM_R(ir_idx) = line;
      IR_COL_NUM_R(ir_idx)  = col;
      IR_FLD_R(ir_idx)      = r_fld;
      IR_IDX_R(ir_idx)      = r_idx;

      if (r_fld == IR_Tbl_Idx) {
         rank_r = IR_RANK(r_idx);
      }
      else if (r_fld == IL_Tbl_Idx) {
         list_idx = r_idx;
         cnt = 0;
         while (list_idx) {

            if (IL_FLD(list_idx) == IR_Tbl_Idx &&
                IR_RANK(IL_IDX(list_idx)) > rank_r) {
               rank_r = IR_RANK(IL_IDX(list_idx));
            }
            cnt++;
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         IR_LIST_CNT_R(ir_idx) = cnt;
      }
   }

   IR_RANK(ir_idx) = (rank_l > rank_r ? rank_l : rank_r);

   IR_ARRAY_SYNTAX(ir_idx) = (IR_RANK(ir_idx) ? TRUE : FALSE);

   TRACE (Func_Exit, "gen_ir", NULL);

   return(ir_idx);

}  /* gen_ir */

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

void gen_opnd(opnd_type *opnd, int idx, fld_type fld, int line, int col)

{

   TRACE (Func_Entry, "gen_opnd", NULL);

   *opnd = null_opnd;

   OPND_FLD((*opnd)) = fld;
   OPND_IDX((*opnd)) = idx;
   if (fld == IL_Tbl_Idx) {
      OPND_LIST_CNT((*opnd)) = line;
   }
   else if (fld == AT_Tbl_Idx || fld == CN_Tbl_Idx) {
      OPND_LINE_NUM((*opnd)) = line;
      OPND_COL_NUM((*opnd)) = col;
   }

   TRACE (Func_Exit, "gen_opnd", NULL);

   return;

}  /* gen_opnd */

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

void stmt_expansion_control_start(void)

{
   int	col;
   int	line;
   int	save_curr_stmt_sh_idx;

   TRACE (Func_Entry, "stmt_expansion_control_start", NULL);
   if (defer_stmt_expansion) {
      PUSH_CURR_STMT;
      save_curr_stmt_sh_idx = curr_stmt_sh_idx;
      curr_stmt_sh_idx = ntr_sh_tbl();
      PUSH_CURR_STMT;
      line = SH_GLB_LINE(save_curr_stmt_sh_idx);
      col = SH_COL_NUM(save_curr_stmt_sh_idx);
      SH_ERR_FLG(curr_stmt_sh_idx) = SH_ERR_FLG(save_curr_stmt_sh_idx);
      SH_COL_NUM(curr_stmt_sh_idx) = col;
      SH_GLB_LINE(curr_stmt_sh_idx) = line;
      SH_STMT_TYPE(curr_stmt_sh_idx) = SH_STMT_TYPE(save_curr_stmt_sh_idx);

      /* place two empty stmts around the current one. */
      /* This is because some stmts generators expect to alway */
      /* have a 'NEXT' stmt */

      defer_stmt_expansion = FALSE;
      gen_sh(Before, Null_Stmt, line, col, FALSE, FALSE, TRUE);
      gen_sh(After, Null_Stmt, line, col, FALSE, FALSE, TRUE);
      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
      defer_stmt_expansion = TRUE;
   }

   TRACE (Func_Exit, "stmt_expansion_control_start", NULL);

   return;

}  /* stmt_expansion_control_start */

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

void stmt_expansion_control_end(opnd_type *opnd)

{
   int		col;
   int		line;
   int		next_idx;
   int		prev_idx;
   int		sh_idx;
   int		stmt_expansion_idx;

   TRACE (Func_Entry, "stmt_expansion_control_end", NULL);

   if (defer_stmt_expansion) {
      POP_CURR_STMT;

      /* remove the bounding empty stmts. */

      next_idx = NULL_IDX;
      prev_idx = NULL_IDX;

      sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      while(SH_PREV_IDX(sh_idx)) {
         prev_idx = sh_idx;
         sh_idx = SH_PREV_IDX(sh_idx);
      }

# ifdef _DEBUG
      if (SH_STMT_TYPE(sh_idx) != Null_Stmt) {
         PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                  "valid stmt_expansion_control", 
                  "stmt_expansion_control_end");
      }
# endif

      FREE_SH_NODE(sh_idx);

      sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);

      while(SH_NEXT_IDX(sh_idx)) {
         next_idx = sh_idx;
         sh_idx = SH_NEXT_IDX(sh_idx);
      }

# ifdef _DEBUG
      if (SH_STMT_TYPE(sh_idx) != Null_Stmt) {
         PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                  "valid stmt_expansion_control", 
                  "stmt_expansion_control_end");
      }
# endif

      FREE_SH_NODE(sh_idx);

      if (prev_idx || next_idx) {
         find_opnd_line_and_column(opnd, &line, &col);
         stmt_expansion_idx = 
                 gen_stmt_expansion_opr(line, col);
         if (prev_idx) {
            STMT_EXPAND_BEFORE_END_SH(stmt_expansion_idx) =
                                       SH_PREV_IDX(curr_stmt_sh_idx);
            STMT_EXPAND_BEFORE_START_SH(stmt_expansion_idx) = prev_idx;
            SH_PREV_IDX(prev_idx) = NULL_IDX;
            SH_NEXT_IDX(STMT_EXPAND_BEFORE_END_SH(stmt_expansion_idx)) = 
                                                                NULL_IDX;
         }
         if (next_idx) {
           STMT_EXPAND_AFTER_START_SH(stmt_expansion_idx) =
                                            SH_NEXT_IDX(curr_stmt_sh_idx);
            STMT_EXPAND_AFTER_END_SH(stmt_expansion_idx) = next_idx;
            SH_NEXT_IDX(next_idx) = NULL_IDX;
            SH_PREV_IDX(STMT_EXPAND_AFTER_START_SH(stmt_expansion_idx)) = 
                                                           NULL_IDX;
         }
         COPY_OPND(IR_OPND_L(stmt_expansion_idx), (*opnd));
         OPND_FLD((*opnd)) = IR_Tbl_Idx;
         OPND_IDX((*opnd)) = stmt_expansion_idx;
      }
      FREE_SH_NODE(curr_stmt_sh_idx);
      POP_CURR_STMT;
   }

   TRACE (Func_Exit, "stmt_expansion_control_end", NULL);

   return;

}  /* stmt_expansion_control_end */

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

void remove_sh(int	sh_idx)

{


   TRACE (Func_Entry, "remove_sh", NULL);

   if (SH_NEXT_IDX(sh_idx)) {
      SH_PREV_IDX(SH_NEXT_IDX(sh_idx)) = SH_PREV_IDX(sh_idx);
   }

   if (SH_PREV_IDX(sh_idx)) {
      SH_NEXT_IDX(SH_PREV_IDX(sh_idx)) = SH_NEXT_IDX(sh_idx);
   }

   TRACE (Func_Exit, "remove_sh", NULL);

   return;

}  /* remove_sh */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	generate a list. The varargs to this routine are 'count' number of    *|
|*	pairs of fld, idx.                                                    *|
|*									      *|
|* Input parameters:							      *|
|*	count		- number of fld/idx pairs. (number of list items)     *|
|*	arg_desc	- TRUE is this is IL_ARG_DESC_VARIANT il.	      *|
|*	line		- line.                                               *|
|*	col		- col.                                                *|
|*	...		- list item operands. (fld/idx pairs)                 *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	The idx of the first list item.                                       *|
|*									      *|
\******************************************************************************/

int gen_il(int		count, 
           boolean	arg_desc,
           int		line, 
           int		col, 
           ...)

{
# define MAX_LIST_ARGS	40

   va_list	arg_ptr;
   int		i;
   int		k;
   int		list_idx;
   int		list_idx2;
   int		start_idx = NULL_IDX;
   int		fld[MAX_LIST_ARGS];
   int		idx[MAX_LIST_ARGS];

   TRACE (Func_Entry, "gen_il", NULL);

   if (count <= 0) {
      goto EXIT;
   }

# ifdef _DEBUG
   if (count > MAX_LIST_ARGS) {
      PRINTMSG(line, 626, Internal, col,
               "count <= MAX_LIST_ARGS", 
               "gen_il");
   }
# endif

   va_start (arg_ptr, col);

   for (i = 0; i < count; i++) {
      fld[i] = va_arg(arg_ptr, long);
      idx[i] = va_arg(arg_ptr, long);
   }

   va_end(arg_ptr);

   NTR_IR_LIST_TBL(list_idx);
   start_idx = list_idx;

   if (arg_desc) {
      IL_ARG_DESC_VARIANT(list_idx) = TRUE;
   }

   IL_FLD(list_idx) = (fld_type) fld[0];
   IL_IDX(list_idx) = idx[0];

   if (IL_FLD(list_idx) == IL_Tbl_Idx) {
      k = 0;
      list_idx2 = IL_IDX(list_idx);
      while(list_idx2) {
         k++;
         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
      }

      IL_LIST_CNT(list_idx) = k;
   }
   else if (IL_FLD(list_idx) != IR_Tbl_Idx) {
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = col;
   }

   for (i = 1; i < count; i++) {

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));

      if (arg_desc) {
         IL_ARG_DESC_VARIANT(IL_NEXT_LIST_IDX(list_idx)) = TRUE;
      }
      else {
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      }
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      IL_FLD(list_idx) = (fld_type) fld[i];
      IL_IDX(list_idx) = idx[i];

      if (IL_FLD(list_idx) == IL_Tbl_Idx) {
         k = 0;
         list_idx2 = IL_IDX(list_idx);
         while(list_idx2) {
            k++;
            list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
         }

         IL_LIST_CNT(list_idx) = k;
      }
      else if (IL_FLD(list_idx) != IR_Tbl_Idx) {
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx) = col;
      }
   }

EXIT:

   TRACE (Func_Exit, "gen_il", NULL);

   return(start_idx);

}  /* gen_il */

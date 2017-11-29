/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/inline.c	5.8	08/09/99 17:48:48\n";

# include "defines.h"           /* Machine dependent ifdefs */

# include "host.m"              /* Host machine dependent macros.*/
# include "host.h"              /* Host machine dependent header.*/
# include "target.m"            /* Target machine dependent macros.*/
# include "target.h"            /* Target machine dependent header.*/
# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "debug.m"
# include "s_globals.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"
# include "s_globals.h"


static	int		parallel_region;
static	int		call_sh;
static	int		sh_count;
static	int		npi;
static  int		loop_nest;
static  int		pgm_attr_idx;
static  int		entry_label_attr_idx;
static  int		exit_label_attr_idx;
static	int		call_line_number;
static	int		call_col_number;
static	int		number_of_actual_args;
static	int		number_of_dummy_args;
static	boolean		table_overflow;
static	boolean		function_call;
static	boolean		something_was_inlined;
static	boolean		processing_ENTRY_called;
static	boolean		inlinable   			= TRUE;
static	boolean		noinline_in_effect;
static	boolean		inline_in_effect;
static	int		copy_head;
static	int		next_label_slot;
static	int		next_copy_out_sh_idx;
static  int           	old_label[MAX_INLINE_LABELS];
static  int           	new_label[MAX_INLINE_LABELS];
static  int             actual_arg_attrs[MAX_INLINE_ARGS];
static  opnd_type       flipped_opnd[MAX_INLINE_ARGS];
static  opnd_type     	actual_opnd[MAX_INLINE_ARGS];
static  opnd_type     	dummy_opnd[MAX_INLINE_ARGS];
static  opnd_type     	subscripting_tree[MAX_INLINE_ARGS];
static  opnd_type     	substringing_tree[MAX_INLINE_ARGS];
static  opnd_type     	struct_tree[MAX_INLINE_ARGS];
static  opnd_type     	substring_offset[MAX_INLINE_ARGS];
static  opnd_type     	linearized_offset[MAX_INLINE_ARGS][9];
static  opnd_type     	substring_len[MAX_INLINE_ARGS];
static  int           	copy_out_sh[MAX_INLINE_ARGS];
static  int             next_pgm_idx[MAX_INLINED_ROUTINES];
static  opnd_type     	subscript[9];
static  opnd_type     	subscript_attr[9];



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine validates the mapping between an actual and dummy        *|
|*	argument.                                                             *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if name substitution is possible                                 *|
|*									      *|
\******************************************************************************/
boolean check_actual_and_dummy(opnd_type	actual,
                               opnd_type   	dummy,
			       int		arg)  /* JEFFL - not used */

{

   int           	actual_bd_idx;
   int           	dummy_bd_idx;
   int           	i;
   boolean           	result =               FALSE;

TRACE (Func_Entry, "check_actual_and_dummy", NULL);

if (inlinable) {
   switch(OPND_FLD(actual)) {
      case CN_Tbl_Idx :
         if (TYP_TYPE(CN_TYPE_IDX(OPND_IDX(actual))) !=
             TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(dummy)))) {
            inlinable = FALSE;
            PRINTMSG(call_line_number,
                     1328,
                     Inline,
                     call_col_number,
                     AT_OBJ_NAME_PTR(pgm_attr_idx));
         }
         else {
            if ((TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(dummy))) == Real ||
                 TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(dummy))) == Complex ||
                 TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(dummy))) == Integer) &&
                TYP_LINEAR(CN_TYPE_IDX(OPND_IDX(actual))) !=
                TYP_LINEAR(ATD_TYPE_IDX(OPND_IDX(dummy)))) {
               inlinable = FALSE;
               PRINTMSG(call_line_number,
                        1328,
                        Inline,
                        call_col_number,
                        AT_OBJ_NAME_PTR(pgm_attr_idx));
            }
         }

         if (ATD_ARRAY_IDX(OPND_IDX(dummy)) != NULL_IDX) {
            inlinable = FALSE;
            PRINTMSG(call_line_number,
                     1330,
                     Inline,
                     call_col_number, 
                     AT_OBJ_NAME_PTR(pgm_attr_idx));
         }

         break;

      case AT_Tbl_Idx :
         if (AT_OBJ_CLASS(OPND_IDX(dummy)) == Data_Obj &&
             AT_OBJ_CLASS(OPND_IDX(actual)) == Data_Obj) {

         if (TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(actual))) !=
             TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(dummy)))) {
            inlinable = FALSE;
            PRINTMSG(call_line_number,
                     1328,
                     Inline,
                     call_col_number, 
                     AT_OBJ_NAME_PTR(pgm_attr_idx));
         }
         else {
            if ((TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(dummy))) == Real ||
                 TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(dummy))) == Complex ||
                 TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(dummy))) == Integer) &&
                TYP_LINEAR(ATD_TYPE_IDX(OPND_IDX(actual))) !=
                TYP_LINEAR(ATD_TYPE_IDX(OPND_IDX(dummy)))) {
               inlinable = FALSE;
               PRINTMSG(call_line_number,
                        1328,
                        Inline,
                        call_col_number, 
                        AT_OBJ_NAME_PTR(pgm_attr_idx));
            }
         }

         if ((ATD_RESHAPE_ARRAY_OPT(OPND_IDX(actual)) ||
              ATD_RESHAPE_ARRAY_OPT(OPND_IDX(dummy))) &&
             ATD_ARRAY_IDX(OPND_IDX(actual)) != NULL_IDX &&
             ATD_ARRAY_IDX(OPND_IDX(dummy)) != NULL_IDX &&
             BD_RANK(ATD_ARRAY_IDX(OPND_IDX(actual))) <   
             BD_RANK(ATD_ARRAY_IDX(OPND_IDX(dummy)))) {  
            inlinable = FALSE;
            PRINTMSG(call_line_number, 
	             1646,
	             Error, 
	             call_col_number, 
                     AT_OBJ_NAME_PTR(OPND_IDX(actual)),  
                     AT_OBJ_NAME_PTR(OPND_IDX(dummy)));
         }

         if (inlinable &&
	     ATD_ARRAY_IDX(OPND_IDX(actual)) != NULL_IDX &&
             ATD_ARRAY_IDX(OPND_IDX(dummy)) != NULL_IDX &&
             BD_RANK(ATD_ARRAY_IDX(OPND_IDX(actual))) ==  
             BD_RANK(ATD_ARRAY_IDX(OPND_IDX(dummy)))) {  

            actual_bd_idx = ATD_ARRAY_IDX(OPND_IDX(actual));
            dummy_bd_idx = ATD_ARRAY_IDX(OPND_IDX(dummy));

            result = TRUE;
            for (i = 1; i < BD_RANK(dummy_bd_idx); i++) {
               if (!(BD_LB_FLD(actual_bd_idx, i) == CN_Tbl_Idx &&
                     BD_LB_FLD(dummy_bd_idx, i) == CN_Tbl_Idx &&
                     fold_relationals(BD_LB_IDX(actual_bd_idx,i),
                                      BD_LB_IDX(dummy_bd_idx,i),
                                      Eq_Opr) &&
                     BD_UB_FLD(actual_bd_idx, i) == CN_Tbl_Idx &&
                     BD_UB_FLD(dummy_bd_idx, i) == CN_Tbl_Idx &&
                     fold_relationals(BD_UB_IDX(actual_bd_idx,i),
                                      BD_UB_IDX(dummy_bd_idx,i),
                                      Eq_Opr))) {
                   result = FALSE;
               }
            }

            if (!(BD_LB_FLD(actual_bd_idx, i) == CN_Tbl_Idx &&
                  BD_LB_FLD(dummy_bd_idx, i) == CN_Tbl_Idx &&
                  fold_relationals(BD_LB_IDX(actual_bd_idx,i),
                                   BD_LB_IDX(dummy_bd_idx,i),
                                   Eq_Opr))) {
               result = FALSE;
            }

            /*
            We will not do name substitution with structure components.
            We will not do name substitution if dummy argument was scoped.
            We will not do name substitution with character.
            */
            if (ATD_CLASS(OPND_IDX(actual)) == Struct_Component ||
                ATD_WAS_SCOPED(OPND_IDX(dummy)) ||
                TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(actual))) == Character) { 
               result = FALSE;
            }
         }

         if (ATD_ARRAY_IDX(OPND_IDX(actual)) == NULL_IDX &&
             ATD_ARRAY_IDX(OPND_IDX(dummy)) != NULL_IDX) {
            inlinable = FALSE;
            PRINTMSG(call_line_number,
                     1330,
                     Inline,
                     call_col_number, 
                     AT_OBJ_NAME_PTR(pgm_attr_idx));
         }

         if (ATD_PE_ARRAY_IDX(OPND_IDX(actual)) != NULL_IDX) {
            inlinable = FALSE;
            PRINTMSG(call_line_number,
                     1612,
                     Inline,
                     call_col_number,
                     AT_OBJ_NAME_PTR(pgm_attr_idx));
         }

         }
         break;
   }
}
         
TRACE (Func_Exit, "check_actual_and_dummy", NULL);

return(result);

}  /* check_actual_and_dummy */




/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Map a scalar dope-vector actual argument		              *|
|*      onto a scalar non dope-vector dummy argument.    		      *|
|*	argument.                                                             *|
|*									      *|
|* Input parameters:							      *|
|*	i     								      *|
|*	dummy_referenced 				      		      *|
|*									      *|
|* Output parameters:							      *|
|*	copy_out_DV_scalar						      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void scalar_dope_to_scalar(int  	i,
                           int  	*copy_out_DV_scalar,
                           boolean  	dummy_referenced)

{

   int          asg_idx;
   int          div_idx;
   int          dv_deref_idx;
   int          tmp_attr;


TRACE (Func_Entry, "scalar_dope_to_scalar", NULL);

   if (TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(dummy_opnd[i]))) == Character &&
       TYP_CHAR_CLASS(ATD_TYPE_IDX(OPND_IDX(dummy_opnd[i]))) ==
                                                        Assumed_Size_Char) {
      NTR_IR_TBL(asg_idx);
      IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_OPR(asg_idx) = Dv_Access_El_Len;
      IR_LINE_NUM(asg_idx) = call_line_number;
      IR_COL_NUM(asg_idx) = call_col_number;
      IR_FLD_L(asg_idx) = AT_Tbl_Idx;
      IR_IDX_L(asg_idx) = IR_IDX_L(OPND_IDX(actual_opnd[i]));
      IR_LINE_NUM_L(asg_idx) = call_line_number;
      IR_COL_NUM_L(asg_idx) = call_col_number;

      NTR_IR_TBL(div_idx);
      IR_TYPE_IDX(div_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_OPR(div_idx) = Shiftr_Opr;
      IR_LINE_NUM(div_idx) = call_line_number;
      IR_COL_NUM(div_idx) = call_col_number;
      IR_FLD_L(div_idx) = IR_Tbl_Idx;
      IR_IDX_L(div_idx) = asg_idx;
      IR_LINE_NUM_L(div_idx) = call_line_number;
      IR_COL_NUM_L(div_idx) = call_col_number;
      IR_FLD_R(div_idx) = CN_Tbl_Idx;
      IR_IDX_R(div_idx) = CN_INTEGER_THREE_IDX;
      IR_LINE_NUM_R(div_idx) = call_line_number;
      IR_COL_NUM_R(div_idx) = call_col_number;
      OPND_IDX(substring_len[i]) = div_idx;
      OPND_FLD(substring_len[i]) = IR_Tbl_Idx;
   }

   COPY_OPND(actual_opnd[i], IR_OPND_L(OPND_IDX(actual_opnd[i])));

   NTR_IR_TBL(dv_deref_idx);
   IR_TYPE_IDX(dv_deref_idx) = ATD_TYPE_IDX(OPND_IDX(dummy_opnd[i]));
   IR_OPR(dv_deref_idx) = Dv_Deref_Opr;
   IR_LINE_NUM(dv_deref_idx) = call_line_number;
   IR_COL_NUM(dv_deref_idx) = call_col_number;
   IR_FLD_L(dv_deref_idx) = OPND_FLD(actual_opnd[i]);
   IR_IDX_L(dv_deref_idx) = OPND_IDX(actual_opnd[i]);
   IR_LINE_NUM_L(dv_deref_idx) = call_line_number;
   IR_COL_NUM_L(dv_deref_idx) = call_col_number;

   OPND_IDX(subscripting_tree[i]) = dv_deref_idx;
   OPND_FLD(subscripting_tree[i]) = IR_Tbl_Idx;

   tmp_attr = gen_compiler_tmp(call_line_number,
                               call_col_number,
                               Priv, 
	  		       TRUE);
   ATD_STOR_BLK_IDX(tmp_attr) = SCP_SB_STACK_IDX(curr_scp_idx);
   ATD_TYPE_IDX(tmp_attr) = ATD_TYPE_IDX(OPND_IDX(dummy_opnd[i]));
   AT_SEMANTICS_DONE(tmp_attr) = TRUE;

   if (inlinable && dummy_referenced) {
      NTR_IR_TBL(asg_idx);
      IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(OPND_IDX(dummy_opnd[i]));
      IR_OPR(asg_idx) = Asg_Opr;
      IR_LINE_NUM(asg_idx) = call_line_number;
      IR_COL_NUM(asg_idx) = call_col_number;
      IR_FLD_L(asg_idx) = AT_Tbl_Idx;
      IR_IDX_L(asg_idx) = tmp_attr;
      IR_LINE_NUM_L(asg_idx) = call_line_number;
      IR_COL_NUM_L(asg_idx) = call_col_number;
      IR_FLD_R(asg_idx) = OPND_FLD(subscripting_tree[i]);
      IR_IDX_R(asg_idx) = OPND_IDX(subscripting_tree[i]);
      IR_LINE_NUM_R(asg_idx) = call_line_number;
      IR_COL_NUM_R(asg_idx) = call_col_number;

      curr_stmt_sh_idx = call_sh;
      gen_sh(Before,
             Assignment_Stmt,
             call_line_number,
             call_col_number,
             FALSE,
             FALSE,
             TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }

   OPND_IDX(actual_opnd[i]) = tmp_attr;
   OPND_FLD(actual_opnd[i]) = AT_Tbl_Idx;
   *copy_out_DV_scalar = tmp_attr;
         
TRACE (Func_Exit, "scalar_dope_to_scalar", NULL);

return;

}  /* scalar_dope_to_scalar */





/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Map an array element actual argument     	                      *|
|*      onto a scalar dummy argument.    		                      *|
|*	argument.                                                             *|
|*									      *|
|* Input parameters:							      *|
|*	i     								      *|
|*	dummy_referenced 				      		      *|
|*	dummy_modified    				      		      *|
|*									      *|
|* Output parameters:							      *|
|*	copy_out_array_element		      				      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void array_element_to_scalar(int  	i,
                             int  	*copy_out_array_element,
                             boolean  	dummy_referenced,
                             boolean  	dummy_modified)

{

   int          l;
   int          line;
   int          col;
   int          asg_idx;
   int          attr_idx;
   int          list_idx;
   int          tmp_attr;


TRACE (Func_Entry, "array_element_to_scalar", NULL);
   COPY_OPND(subscripting_tree[i], actual_opnd[i]);

   /*
   If we are going to have to do a copy out, then you
   need to save the index expressions of the array 
   reference on entry to the inlined code.
   */
   if (dummy_modified) {
      list_idx = IR_IDX_R(OPND_IDX(actual_opnd[i]));
      for (l = 1; 
           l <= IR_LIST_CNT_R(OPND_IDX(actual_opnd[i])); 
           l++) {
          COPY_OPND(subscript[l], IL_OPND(list_idx));
          list_idx = IL_NEXT_LIST_IDX(list_idx);

          tmp_attr = gen_compiler_tmp(call_line_number,
                                      call_col_number,
                                      Priv, 
   				      TRUE);
          ATD_STOR_BLK_IDX(tmp_attr) = SCP_SB_STACK_IDX(curr_scp_idx);
          ATD_TYPE_IDX(tmp_attr) = INTEGER_DEFAULT_TYPE;
          AT_SEMANTICS_DONE(tmp_attr) = TRUE;

          NTR_IR_TBL(asg_idx);
          IR_TYPE_IDX(asg_idx) = INTEGER_DEFAULT_TYPE;
          IR_OPR(asg_idx) = Asg_Opr;
          IR_LINE_NUM(asg_idx) = call_line_number;
          IR_COL_NUM(asg_idx) = call_col_number;
          IR_FLD_L(asg_idx) = AT_Tbl_Idx;
          IR_IDX_L(asg_idx) = tmp_attr;
          IR_LINE_NUM_L(asg_idx) = call_line_number;
          IR_COL_NUM_L(asg_idx) = call_col_number;
          IR_FLD_R(asg_idx) = OPND_FLD(subscript[l]);
          IR_IDX_R(asg_idx) = OPND_IDX(subscript[l]);
          IR_LINE_NUM_R(asg_idx) = call_line_number;
          IR_COL_NUM_R(asg_idx) = call_col_number;

          curr_stmt_sh_idx = call_sh;
          gen_sh(Before,
                 Assignment_Stmt,
                 call_line_number,
                 call_col_number,
                 FALSE,
                 FALSE,
                 TRUE);
          SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
          SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

          OPND_IDX(subscript_attr[l]) = tmp_attr;
          OPND_FLD(subscript_attr[l]) = AT_Tbl_Idx;
          OPND_LINE_NUM(subscript_attr[l]) = call_line_number;
          OPND_COL_NUM(subscript_attr[l]) = call_col_number;
       }
    }

    tmp_attr = gen_compiler_tmp(call_line_number,
                                call_col_number,
                                Priv, 
				TRUE);
    ATD_STOR_BLK_IDX(tmp_attr) = SCP_SB_STACK_IDX(curr_scp_idx);

    attr_idx = find_base_attr(&actual_opnd[i],
                              &line,
                              &col);

    ATD_TYPE_IDX(tmp_attr) = ATD_TYPE_IDX(attr_idx);
    AT_SEMANTICS_DONE(tmp_attr) = TRUE;

    if (dummy_referenced) {
       NTR_IR_TBL(asg_idx);
       IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(attr_idx);
       IR_OPR(asg_idx) = Asg_Opr;
       IR_LINE_NUM(asg_idx) = call_line_number;
       IR_COL_NUM(asg_idx) = call_col_number;
       IR_FLD_L(asg_idx) = AT_Tbl_Idx;
       IR_IDX_L(asg_idx) = tmp_attr;
       IR_LINE_NUM_L(asg_idx) = call_line_number;
       IR_COL_NUM_L(asg_idx) = call_col_number;
       IR_FLD_R(asg_idx) = OPND_FLD(actual_opnd[i]);
       IR_IDX_R(asg_idx) = OPND_IDX(actual_opnd[i]);
       IR_LINE_NUM_R(asg_idx) = call_line_number;
       IR_COL_NUM_R(asg_idx) = call_col_number;

       curr_stmt_sh_idx = call_sh;
       gen_sh(Before,
              Assignment_Stmt,
              call_line_number,
              call_col_number,
              FALSE,
              FALSE,
              TRUE);
       SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
       SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
    }

    OPND_IDX(actual_opnd[i]) = tmp_attr;
    OPND_FLD(actual_opnd[i]) = AT_Tbl_Idx;
    *copy_out_array_element = tmp_attr;
         
TRACE (Func_Exit, "array_element_to_scalar", NULL);

return;

}  /* array_element_to_scalar */




/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Map a scalar character actual argument		              	      *|
|*      onto a scalar character dummy argument.    		      	      *|
|*	Map an character array actual argument		              	      *|
|*      onto a character array dummy argument.    		      	      *|
|*									      *|
|* Input parameters:							      *|
|*	i     								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE						      		      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void character_to_character(int  	i)

{

   int          asg_idx;
   int          minus_idx;
   int          substring_list_idx;
   int          tmp_attr;


TRACE (Func_Entry, "character_to_character", NULL);

   COPY_OPND(substringing_tree[i], actual_opnd[i]);

   substring_list_idx = IR_IDX_R(OPND_IDX(actual_opnd[i]));
   NTR_IR_TBL(minus_idx);
   IR_OPR(minus_idx) = Minus_Opr;
   IR_TYPE_IDX(minus_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(minus_idx) = call_line_number;
   IR_COL_NUM(minus_idx) = call_col_number;
   COPY_OPND(IR_OPND_L(minus_idx), IL_OPND(substring_list_idx));
   IR_LINE_NUM_L(minus_idx) = call_line_number;
   IR_COL_NUM_L(minus_idx) = call_col_number;
   IR_IDX_R(minus_idx) = CN_INTEGER_ONE_IDX;
   IR_FLD_R(minus_idx) = CN_Tbl_Idx;
   IR_LINE_NUM_R(minus_idx) = call_line_number;
   IR_COL_NUM_R(minus_idx) = call_col_number;
   OPND_IDX(substring_offset[i]) = minus_idx;
   OPND_FLD(substring_offset[i]) = IR_Tbl_Idx;
 
   if (TYP_CHAR_CLASS(ATD_TYPE_IDX(OPND_IDX(dummy_opnd[i]))) ==
                                                      Assumed_Size_Char) {
      substring_list_idx = IL_NEXT_LIST_IDX(substring_list_idx);
      substring_list_idx = IL_NEXT_LIST_IDX(substring_list_idx);
      COPY_OPND(substring_len[i], IL_OPND(substring_list_idx));
   }

   COPY_OPND(actual_opnd[i], IR_OPND_L(OPND_IDX(actual_opnd[i])));

   if (OPND_FLD(actual_opnd[i]) == IR_Tbl_Idx &&
       IR_OPR(OPND_IDX(actual_opnd[i])) == Dv_Deref_Opr) {
      inlinable = FALSE;
      PRINTMSG(call_line_number,
               1202,
               Inline,
               call_col_number, 
               AT_OBJ_NAME_PTR(pgm_attr_idx),
               "the compiler cannot support this mapping");
   }

   if (OPND_FLD(actual_opnd[i]) == IR_Tbl_Idx &&
       IR_OPR(OPND_IDX(actual_opnd[i])) == Subscript_Opr &&
       ATD_ARRAY_IDX(OPND_IDX(dummy_opnd[i])) == NULL_IDX) {
      inlinable = FALSE;
      PRINTMSG(call_line_number,
               1202,
               Inline,
               call_col_number, 
               AT_OBJ_NAME_PTR(pgm_attr_idx),
               "the compiler cannot support this mapping");
   }

   tmp_attr = gen_compiler_tmp(call_line_number, 
   		               call_col_number,
                               Priv, 
			       TRUE);
   ATD_STOR_BLK_IDX(tmp_attr) = SCP_SB_STACK_IDX(curr_scp_idx);
   ATD_TYPE_IDX(tmp_attr) = CG_INTEGER_DEFAULT_TYPE;
   AT_SEMANTICS_DONE(tmp_attr) = TRUE;

   NTR_IR_TBL(asg_idx);
   IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_OPR(asg_idx) = Asg_Opr;
   IR_LINE_NUM(asg_idx) = call_line_number;
   IR_COL_NUM(asg_idx) = call_col_number;
   IR_FLD_L(asg_idx) = AT_Tbl_Idx;
   IR_IDX_L(asg_idx) = tmp_attr;
   IR_LINE_NUM_L(asg_idx) = call_line_number;
   IR_COL_NUM_L(asg_idx) = call_col_number;
   IR_FLD_R(asg_idx) = OPND_FLD(substring_offset[i]);
   IR_IDX_R(asg_idx) = OPND_IDX(substring_offset[i]);
   IR_LINE_NUM_R(asg_idx) = call_line_number;
   IR_COL_NUM_R(asg_idx) = call_col_number;

   curr_stmt_sh_idx = call_sh;
   gen_sh(Before,
          Assignment_Stmt,
          call_line_number,
          call_col_number,
          FALSE,
          FALSE,
          TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   OPND_IDX(substring_offset[i]) = tmp_attr;
   OPND_FLD(substring_offset[i]) = AT_Tbl_Idx;
         
TRACE (Func_Exit, "character_to_character", NULL);

return;

}  /* character_to_character */





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

int copy_sbtree(int	  idx,
                fld_type  fld)

{
   id_str_type  name;
   int          i;
   int          j;
   int          k;
   int          sub;
   int          trp;
   int		list_idx;
   int		attr_idx;
   int		original_idx;
   int          cn_idx;
   int		plus_idx;
   int		sb_idx;
   int		il_idx;
   int		tmp_idx1;
   int		tmp_idx2;
   int		outer_sb_idx;
   int		new_label_attr;
   int		module_attr_idx;
   int		name_idx;
   int		new_root 		= NULL_IDX;
   int		new_idx;
   int		function_attr;
#ifdef KEY /* Bug 10177 */
   int          flipped_bd_idx = 0;
#else /* KEY Bug 10177 */
   int          flipped_bd_idx;
#endif /* KEY Bug 10177 */
   int          dummy_bd_idx;
   int		new_blk;
   int          flipped_array 		= 0;
   int		type_idx2;
   int		trail;
   int		match			= 0;
   boolean      found;
   long_type    cnst[MAX_WORDS_FOR_INTEGER];
   long_type    folded_const[MAX_WORDS_FOR_NUMERIC];


   TRACE (Func_Entry, "copy_sbtree", NULL);

   if (idx != NULL_IDX) {

      switch(fld) {

         case NO_Tbl_Idx :
            break;

         case IR_Tbl_Idx :
            NTR_IR_TBL(new_root);

            /*
            This check here is a saftey value.   Table size
            is checked here.   If we are approaching dangerous limits,
            we just stop inlining.  The value is arbitrary.
            */
            if (new_root > MAX_INLINE_IR) {
               table_overflow = TRUE;
               inlinable = FALSE;
            }

            COPY_TBL_NTRY(ir_tbl, new_root, idx);
            IR_LINE_NUM(new_root) = call_line_number;
            IR_COL_NUM(new_root) = call_col_number;

            new_idx = copy_sbtree(IR_IDX_L(idx), IR_FLD_L(idx));
            IR_IDX_L(new_root) = new_idx;
            if (IR_FLD_L(idx) == AT_Tbl_Idx) {
               for (i = 0; i <= number_of_dummy_args; i++) {
                  if (IR_IDX_L(idx) == OPND_IDX(dummy_opnd[i])) {
                     match = i;

                     if ((IR_OPR(idx) == Subscript_Opr ||
                          IR_OPR(idx) == Section_Subscript_Opr ||
                          IR_OPR(idx) == Whole_Subscript_Opr) &&
                         ATD_RESHAPE_ARRAY_OPT(OPND_IDX(flipped_opnd[i]))) {
                        flipped_bd_idx =
                                 ATD_ARRAY_IDX(OPND_IDX(flipped_opnd[i]));
                        dummy_bd_idx =
                                 ATD_ARRAY_IDX(OPND_IDX(dummy_opnd[i]));

                        if (dummy_bd_idx != NULL_IDX &&
                            flipped_bd_idx != NULL_IDX &&
                            BD_RANK(dummy_bd_idx) < BD_RANK(flipped_bd_idx)) {
                           flipped_array = BD_RANK(flipped_bd_idx) -
                                           BD_RANK(dummy_bd_idx) ;
                        }
                     }

                     if (OPND_FLD(actual_opnd[i]) == AT_Tbl_Idx &&
                         ATD_AUTOMATIC(OPND_IDX(actual_opnd[i]))) {
                        COPY_OPND(IR_OPND_L(new_root), actual_opnd[i]);
                     }
                     else if (OPND_IDX(struct_tree[i]) != NULL_IDX) {
                        COPY_OPND(IR_OPND_L(new_root), struct_tree[i]);
                     }
                     else {
                        COPY_OPND(IR_OPND_L(new_root), actual_opnd[i]);
                     }
                  }
               }

               if (AT_OBJ_CLASS(IR_IDX_L(idx)) == Label) {
                  for (k = 0; k < next_label_slot; k++) {
                     if (IR_IDX_L(idx) == old_label[k]) {
                        break;
                     }
                  }

                  if (k < next_label_slot) {
                     IR_IDX_L(new_root) = new_label[k];
                     IR_FLD_L(new_root) = AT_Tbl_Idx;
                  }
                  else {
                     old_label[next_label_slot] = IR_IDX_L(idx);
                     new_label_attr = gen_internal_lbl(call_line_number);
                     COPY_COMMON_ATTR_INFO(IR_IDX_L(idx), 
                                           new_label_attr, 
                                           Label);
                     COPY_VARIANT_ATTR_INFO(IR_IDX_L(idx), 
                                            new_label_attr, 
                                            Label);
                     AT_ATTR_LINK(new_label_attr) = NULL_IDX;
                     new_label[next_label_slot] = new_label_attr;
 
                     IR_IDX_L(new_root) = new_label_attr;
                     IR_FLD_L(new_root) = AT_Tbl_Idx;

                     if (ATL_DIRECTIVE_LIST(new_label_attr) != NULL_IDX) {
                        il_idx = IL_IDX(ATL_DIRECTIVE_LIST(new_label_attr)) + 
                                 Cache_Bypass_Dir_Idx;

                        if (IL_FLD(il_idx) == IL_Tbl_Idx) {
                           il_idx = IL_IDX(il_idx);

                           while (il_idx != NULL_IDX) {
                              for (i = 1; i <= number_of_dummy_args; i++) {
                                 if (OPND_IDX(dummy_opnd[i]) == IL_IDX(il_idx)){
                                    IL_IDX(il_idx) = OPND_IDX(actual_opnd[i]);
                                    break;
                                 }
                              }
                              il_idx = IL_NEXT_LIST_IDX(il_idx);
                           }
                        }
                     }
 
                     next_label_slot = next_label_slot + 1;
                     if (next_label_slot == MAX_INLINE_LABELS) {
                        next_label_slot = next_label_slot - 1;
                        inlinable = FALSE;
                        table_overflow = TRUE;
                     }
                  }
               }
            }


            if (IR_FLD_L(idx) != IL_Tbl_Idx) {
               IR_LINE_NUM_L(new_root) = call_line_number;
               IR_COL_NUM_L(new_root) = call_col_number;
            }

            if (IR_FLD_R(idx) == IL_Tbl_Idx && IR_LIST_CNT_R(idx) == 0) {
               new_idx = NULL_IDX;
            }
            else {
               new_idx = copy_sbtree(IR_IDX_R(idx), IR_FLD_R(idx));
            }

            IR_IDX_R(new_root) = new_idx;

            if (flipped_array > 0) {
               tmp_idx1 = NULL_IDX;
               tmp_idx2 = NULL_IDX;

               il_idx = new_idx;
               while (il_idx != NULL_IDX) {
                  tmp_idx1 = il_idx;
                  il_idx = IL_NEXT_LIST_IDX(il_idx);
               }

               for (i = 1; i <= flipped_array; i++) {
                  NTR_IR_LIST_TBL(tmp_idx2);
                  IL_FLD(tmp_idx2) = CN_Tbl_Idx;
                  IL_IDX(tmp_idx2) = CN_INTEGER_ONE_IDX;
                  IL_LINE_NUM(tmp_idx2) = call_line_number;
                  IL_COL_NUM(tmp_idx2) = call_col_number;
                  IL_PREV_LIST_IDX(tmp_idx2) = tmp_idx1;
                  IL_NEXT_LIST_IDX(tmp_idx1) = tmp_idx2;
                  tmp_idx1 = tmp_idx2;
               }

               IR_LIST_CNT_R(new_root) = BD_RANK(flipped_bd_idx);
            }

            if (IR_FLD_R(idx) == AT_Tbl_Idx) {
               for (i = 0; i <= number_of_dummy_args; i++) {
                  if (IR_IDX_R(idx) == OPND_IDX(dummy_opnd[i])) {
                     if (OPND_FLD(actual_opnd[i]) == AT_Tbl_Idx &&
                         ATD_AUTOMATIC(OPND_IDX(actual_opnd[i]))) {
                        COPY_OPND(IR_OPND_R(new_root), actual_opnd[i]);
                     }
                     else if (OPND_IDX(struct_tree[i]) != NULL_IDX) {
                        COPY_OPND(IR_OPND_R(new_root), struct_tree[i]);
                     }
                     else {
                        COPY_OPND(IR_OPND_R(new_root), actual_opnd[i]);
                     }
                  }
               }

               if (AT_OBJ_CLASS(IR_IDX_R(idx)) == Label) {
                  for (k = 0; k < next_label_slot; k++) {
                     if (IR_IDX_R(idx) == old_label[k]) {
                        break;
                     }
                  }

                  if (k < next_label_slot) {
                     IR_IDX_R(new_root) = new_label[k];
                     IR_FLD_R(new_root) = AT_Tbl_Idx;
                  }
                  else {
                     old_label[next_label_slot] = IR_IDX_R(idx);
                     new_label_attr = gen_internal_lbl(call_line_number);
                     COPY_COMMON_ATTR_INFO(IR_IDX_R(idx), 
                                           new_label_attr, 
                                           Label);

                     COPY_VARIANT_ATTR_INFO(IR_IDX_R(idx),
                                            new_label_attr, 
                                            Label);

                     AT_ATTR_LINK(new_label_attr) = NULL_IDX;
                     new_label[next_label_slot] = new_label_attr;
 
                     IR_IDX_R(new_root) = new_label_attr;
                     IR_FLD_R(new_root) = AT_Tbl_Idx;

                     if (ATL_DIRECTIVE_LIST(new_label_attr) != NULL_IDX) {
                        il_idx = IL_IDX(ATL_DIRECTIVE_LIST(new_label_attr)) + 
                                 Cache_Bypass_Dir_Idx;

                        if (IL_FLD(il_idx) == IL_Tbl_Idx) {
                           il_idx = IL_IDX(il_idx);

                           while (il_idx != NULL_IDX) {
                              for (i = 1; i <= number_of_dummy_args; i++) {
                                 if (OPND_IDX(dummy_opnd[i]) == IL_IDX(il_idx)){
                                    IL_IDX(il_idx) = OPND_IDX(actual_opnd[i]);
                                    break;
                                 }
                              }

                              il_idx = IL_NEXT_LIST_IDX(il_idx);
                           }
                        }
                     }
 
                     next_label_slot = next_label_slot + 1;
                     if (next_label_slot == MAX_INLINE_LABELS) {
                        next_label_slot = next_label_slot - 1;
                        inlinable = FALSE;
                        table_overflow = TRUE;
                     }
                  }
               }
            }

            if (IR_FLD_R(idx) != IL_Tbl_Idx) {
               IR_LINE_NUM_R(new_root) = call_line_number;
               IR_COL_NUM_R(new_root) = call_col_number;
            }

            switch (IR_OPR(idx)) {
              case Whole_Substring_Opr :
              case Substring_Opr :
                 attr_idx = find_left_attr(&IR_OPND_L(idx));
                 if (IR_FLD_L(idx) == AT_Tbl_Idx &&
                     !ATD_IM_A_DOPE(attr_idx)) { 

                    for (i = 0; i <= number_of_dummy_args; i++) {
                       if (attr_idx == OPND_IDX(dummy_opnd[i])) {
                          sub = IR_IDX_R(new_root);
                          for (j = 1; j <= 2; j++) {
                             NTR_IR_TBL(plus_idx);
                             IR_OPR(plus_idx) = Plus_Opr;
                             IR_TYPE_IDX(plus_idx) = CG_INTEGER_DEFAULT_TYPE;
                             IR_LINE_NUM(plus_idx) = call_line_number;
                             IR_COL_NUM(plus_idx) = call_col_number;
                             COPY_OPND(IR_OPND_L(plus_idx), IL_OPND(sub));
                             IR_LINE_NUM_L(plus_idx) = call_line_number;
                             IR_COL_NUM_L(plus_idx) = call_col_number;
                             COPY_OPND(IR_OPND_R(plus_idx),
                                       substring_offset[i]);
                             IR_LINE_NUM_R(plus_idx) = call_line_number;
                             IR_COL_NUM_R(plus_idx) = call_col_number;

                             IL_FLD(sub) = IR_Tbl_Idx;
                             IL_IDX(sub) = plus_idx;
                             sub = IL_NEXT_LIST_IDX(sub);
                          }
                       }
                    }
                 }
                 break;


              case Whole_Subscript_Opr :
              case Section_Subscript_Opr :
              case Subscript_Opr :
                 attr_idx = find_left_attr(&IR_OPND_L(idx));
                 for (i = 0; i <= number_of_dummy_args; i++) {
                    if (attr_idx == OPND_IDX(dummy_opnd[i])) {
                       sub = IR_IDX_R(new_root);
                       k = 1;
                       while (sub != NULL_IDX) {
                       if (IL_FLD(sub) == IR_Tbl_Idx &&
                           IR_OPR(IL_IDX(sub)) == Triplet_Opr) {
                          trp = IR_IDX_L(IL_IDX(sub));
                          for (j = 1; j <= 2; j++) {
                             NTR_IR_TBL(plus_idx);
                             IR_OPR(plus_idx) = Plus_Opr;
                             IR_TYPE_IDX(plus_idx) = CG_INTEGER_DEFAULT_TYPE;
                             IR_LINE_NUM(plus_idx) = call_line_number;
                             IR_COL_NUM(plus_idx) = call_col_number;
                             COPY_OPND(IR_OPND_L(plus_idx), IL_OPND(trp));
                             IR_LINE_NUM_L(plus_idx) = call_line_number;
                             IR_COL_NUM_L(plus_idx) = call_col_number;
                             COPY_OPND(IR_OPND_R(plus_idx), 
                                       linearized_offset[i][k]);
                             IR_LINE_NUM_R(plus_idx) = call_line_number;
                             IR_COL_NUM_R(plus_idx) = call_col_number;
                           
                             IL_FLD(trp) = IR_Tbl_Idx;
                             IL_IDX(trp) = plus_idx;
                             trp = IL_NEXT_LIST_IDX(trp);
                          }
                       }
                       else {
                          NTR_IR_TBL(plus_idx);
                          IR_OPR(plus_idx) = Plus_Opr;
                          IR_TYPE_IDX(plus_idx) = CG_INTEGER_DEFAULT_TYPE;
                          IR_LINE_NUM(plus_idx) = call_line_number;
                          IR_COL_NUM(plus_idx) = call_col_number;
                          COPY_OPND(IR_OPND_L(plus_idx), IL_OPND(sub));
                          IR_LINE_NUM_L(plus_idx) = call_line_number;
                          IR_COL_NUM_L(plus_idx) = call_col_number;
                          COPY_OPND(IR_OPND_R(plus_idx), 
                                    linearized_offset[i][k]);
                          IR_LINE_NUM_R(plus_idx) = call_line_number;
                          IR_COL_NUM_R(plus_idx) = call_col_number;
                        
                          IL_FLD(sub) = IR_Tbl_Idx;
                          IL_IDX(sub) = plus_idx;
                       }
                       sub = IL_NEXT_LIST_IDX(sub);
                       k = k + 1;
                       }
                    }
                 }
                 break;


              case Asg_Opr :
                 if (TYP_TYPE(IR_TYPE_IDX(idx)) == CRI_Ch_Ptr ||
                     TYP_TYPE(IR_TYPE_IDX(idx)) == CRI_Ptr) {  
                    if ((IR_FLD_R(idx) == CN_Tbl_Idx &&
                         TYP_TYPE(CN_TYPE_IDX(IR_IDX_R(idx))) == Integer) ||
                        (IR_FLD_R(idx) == AT_Tbl_Idx &&
                         AT_OBJ_CLASS(idx) == Data_Obj &&
                         TYP_TYPE(ATD_TYPE_IDX(IR_IDX_R(idx))) == Integer) ||
                        (IR_FLD_R(idx) == IR_Tbl_Idx &&
                         TYP_TYPE(IR_TYPE_IDX(IR_IDX_R(idx))) == Integer)) {
                       if (inlinable) {
                          inlinable = FALSE;
                          PRINTMSG(call_line_number,
                                   1652,
                                   Inline,
                                   call_col_number, 
                                   AT_OBJ_NAME_PTR(pgm_attr_idx));
                       }
                    }
                 }

                 if (IR_FLD_L(new_root) == CN_Tbl_Idx) {
                    if (inlinable) {
                       inlinable = FALSE;
                       PRINTMSG(call_line_number,
                                1325,
                                Inline,
                                call_col_number, 
                                AT_OBJ_NAME_PTR(pgm_attr_idx));
                    }
                 }

                 if (IR_FLD_L(new_root) == AT_Tbl_Idx &&
                     AT_OBJ_CLASS(IR_IDX_L(new_root)) == Label) {
                    if (inlinable) {
                       inlinable = FALSE;
                       PRINTMSG(call_line_number,
                                1331,
                                Inline,
                                call_col_number, 
                                AT_OBJ_NAME_PTR(pgm_attr_idx));
                    }
                 }
                 break;

              case User_Code_Start_Opr :
                 IR_OPR(new_root) = Null_Opr;
                 break;

              case Doall_Cmic_Opr :
                 if (parallel_region > 0) {
                    if (inlinable) {
                       inlinable = FALSE;
                       PRINTMSG(call_line_number,
                                1556,
                                Inline,
                                call_col_number,
                                AT_OBJ_NAME_PTR(pgm_attr_idx));
                    }
                 }
                 break;

              case Entry_Opr :
                 if (strcmp(AT_OBJ_NAME_PTR(pgm_attr_idx), 
                            AT_OBJ_NAME_PTR(IR_IDX_L(idx))) == 0) { 
                    processing_ENTRY_called = TRUE;
                    IR_OPR(new_root) = Label_Opr;
                    IR_IDX_L(new_root) = entry_label_attr_idx;
                 }
                 else {
                    /*
                    ENTRY operations must be cleared out in the
                    text of the inlined routine.
                    */
                    IR_OPR(new_root) = Null_Opr;
                    processing_ENTRY_called = FALSE;
                    next_pgm_idx[npi] = IR_IDX_L(idx);
                    npi = npi + 1;
                 }
                 break;

              case Init_Reloc_Opr :
              case Init_Opr :
                 /*
                 CDIR ID's are completely ignored when a routine is
                 inlined.   CDIR ID's within the callee have no effect
                 on the CDIR ID lines within the caller.
                 */
                 if (!(IR_FLD_L(idx) == IR_Tbl_Idx && 
                       IR_OPR(IR_IDX_L(idx)) == Implied_Do_Opr)) {
                    attr_idx = find_left_attr(&IR_OPND_L(idx));
                    sb_idx = ATD_STOR_BLK_IDX(attr_idx);

                    CREATE_ID(name, sb_name[What_Blk], sb_len[What_Blk]);

                    if (sb_idx != NULL_IDX) {
                       if (strcmp(SB_NAME_PTR(sb_idx), 
                                  (char *)&name.string) == 0) {
                          IR_OPR(new_root) = Null_Opr;
                       }
                    }
                 }

                 i = 0;
                 while (next_pgm_idx[i] != NULL_IDX) {
                    if (strcmp(AT_OBJ_NAME_PTR(next_pgm_idx[i]), 
                               AT_OBJ_NAME_PTR(pgm_attr_idx)) == 0) {
                       IR_OPR(new_root) = Null_Opr;
                       break;
                    }

                    i = i + 1;
                 }
                 break;

              case Not_Opr :
                 if (IR_FLD_L(idx) == IR_Tbl_Idx &&
                     IR_OPR(IR_IDX_L(idx)) == Argchck_Present_Opr) {
                    cn_idx = set_up_logical_constant(cnst,
                                            CG_LOGICAL_DEFAULT_TYPE,
                                            FALSE_VALUE,
                                            TRUE);                      
                    IR_FLD_L(new_root) = CN_Tbl_Idx;
                    IR_IDX_L(new_root) = cn_idx;
                 }
                 break;

              case Use_Opr :
                 module_attr_idx = IR_IDX_L(idx);
                 name_idx = NULL_IDX;

                 /* Check to make sure that this module is available in */
                 /* this compile.   ATP_MOD_PATH_IDX will be set if the */
                 /* module was USEd from a different compilation.       */
                 /* We know that ATP_IN_CURRENT_COMPILE does not need   */
                 /* to be set then.  Otherwise search the global name   */
                 /* table to make sure this module was seen during this */
                 /* compilation.                                        */

                 if (ATP_MOD_PATH_IDX(module_attr_idx) == NULL_IDX &&
                     !srch_global_name_tbl(AT_OBJ_NAME_PTR(module_attr_idx),
                                           AT_NAME_LEN(module_attr_idx),
                                           &name_idx)) {
                    inlinable = FALSE;
                    PRINTMSG(call_line_number, 1346, Inline,
                             call_col_number,
                             AT_OBJ_NAME_PTR(pgm_attr_idx));
                 }
                 else if (name_idx != NULL_IDX &&
                          GA_DEFINED(GN_ATTR_IDX(name_idx))){
                    ATP_IN_CURRENT_COMPILE(module_attr_idx) = TRUE;
                 }
                 break;

              case Clen_Opr :
                 if (match != 0 && IR_FLD_L(idx) == AT_Tbl_Idx) {
                    if (TYP_CHAR_CLASS(ATD_TYPE_IDX(IR_IDX_L(idx))) ==
                                                 Assumed_Size_Char) {
                       IR_OPR(new_root) = Int_Opr;
                       COPY_OPND(IR_OPND_L(new_root), substring_len[match]);
                    }
                 }
                 break;

              case Aloc_Opr :
                 if (match != 0 &&
                     OPND_FLD(actual_opnd[match]) == CN_Tbl_Idx) {
                    IR_OPR(new_root) = Const_Tmp_Loc_Opr;
                    COPY_OPND(IR_OPND_L(new_root), actual_opnd[match]);
                 }
                 break;

              case Loc_Opr :
                 if (inlinable) {
                    attr_idx = find_left_attr(&IR_OPND_L(idx));
                    if (attr_idx != NULL_IDX &&
                        AT_OBJ_CLASS(attr_idx) == Data_Obj &&
                        ATD_IN_COMMON(attr_idx)) {
                       inlinable = FALSE;
                       PRINTMSG(call_line_number,
                                1358,
                                Inline,
                                call_col_number, 
                                AT_OBJ_NAME_PTR(pgm_attr_idx));
                    }

                    if (inlinable) {
                       if (match != 0 &&
                           OPND_FLD(actual_opnd[match]) == CN_Tbl_Idx) {
                          inlinable = FALSE;
                          PRINTMSG(call_line_number,
                                   1437,
                                   Inline,
                                   call_col_number,
                                   AT_OBJ_NAME_PTR(pgm_attr_idx));
                       }
                    }
                 }
                 break;

              case Numarg_Opr :
                 if (inlinable) {
                    inlinable = FALSE;
                    PRINTMSG(call_line_number,
                             1329,
                             Inline,
                             call_col_number, 
                             AT_OBJ_NAME_PTR(pgm_attr_idx));
                 }
                 break;

              case Integer_Cdir_Opr :
                 if (inlinable) {
                    inlinable = FALSE;
                    PRINTMSG(call_line_number,
                             1409,
                             Inline,
                             call_col_number, 
                             AT_OBJ_NAME_PTR(pgm_attr_idx));
                 }
                 break;

              case Call_Opr :
                 if ((AT_OBJ_NAME_PTR(IR_IDX_L(idx)))[0] != '_') {
                    if (inlinable && 
                        opt_flags.inline_lvl == Inline_Lvl_3 &&
                        !ATP_INLINE_ALWAYS(pgm_attr_idx) &&
                        !inline_in_effect) {
                       inlinable = FALSE;
                       PRINTMSG(call_line_number,
                                1543,
                                Inline,
                                call_col_number,
                                AT_OBJ_NAME_PTR(pgm_attr_idx));
                    }
                 }
                 break;

              case Present_Opr :
                 if (inlinable) {
                    inlinable = FALSE;
                    PRINTMSG(call_line_number,
                             1327,
                             Inline,
                             call_col_number, 
                             AT_OBJ_NAME_PTR(pgm_attr_idx));
                 }
                 break;

              case Br_Asg_Opr :
                 if (inlinable) {
                    inlinable = FALSE;
                    PRINTMSG(call_line_number,
                             1331,
                             Inline,
                             call_col_number, 
                             AT_OBJ_NAME_PTR(pgm_attr_idx));
                 }
                 break;

              case Return_Opr :
                 if (IR_IDX_L(idx) != NULL_IDX) {
                    if (inlinable) {
                       inlinable = FALSE;
                       PRINTMSG(call_line_number,
                                1326,
                                Inline,
                                call_col_number, 
                                AT_OBJ_NAME_PTR(pgm_attr_idx));
                    }
                 }
                 else {
                    IR_OPR(new_root) = Br_Uncond_Opr;
                    IR_IDX_R(new_root) = exit_label_attr_idx;
                    IR_FLD_R(new_root) = AT_Tbl_Idx;
                 }
                 break;
            }
            break;

         case AT_Tbl_Idx :
            if (AT_OBJ_CLASS(idx) == Data_Obj) {
               sb_idx = ATD_STOR_BLK_IDX(idx);
 
               /*
               We may need to process the storage block for the pointer.
               So, call sb_tree with the pointer to process the storage 
               block.
               */
               if (ATD_CLASS(idx) == CRI__Pointee) {
                  copy_sbtree(ATD_PTR_IDX(idx), AT_Tbl_Idx);
               }

               if (inlinable) {
                  /*
                  When inlining multi entry functions, we
                  will not inline the function if any two entries
                  have the same data type and kind type.   The reason
                  is that the inliner does not create an equivalence
                  group for the different function results and it
                  is possible for the user to define the function 
                  result thru a different function result variable
                  than the one associated with the entry taken.
                  */
                  if (ATD_CLASS(idx) == Function_Result) {
                     function_attr = NULL_IDX;

                     if (OPND_FLD(dummy_opnd[0]) == AT_Tbl_Idx) {
                        function_attr = OPND_IDX(dummy_opnd[0]); 
                     }

                     if (OPND_FLD(dummy_opnd[1]) == AT_Tbl_Idx &&
                        ATD_CLASS(OPND_IDX(dummy_opnd[1])) == Function_Result) {
                        function_attr = OPND_IDX(dummy_opnd[1]); 
                     }

                     if (function_attr != NULL_IDX) {
                        if (idx != function_attr) {
                           if (TYP_TYPE(ATD_TYPE_IDX(idx)) == 
                               TYP_TYPE(ATD_TYPE_IDX(function_attr))) { 
                              inlinable = FALSE;
                              PRINTMSG(call_line_number,
                                       1388,
                                       Inline,
                                       call_col_number,
                                       AT_OBJ_NAME_PTR(pgm_attr_idx));
                           }
                        }
                     }
                  }

                  if (ATD_CLASS(idx) == Dummy_Argument) {
                     found = FALSE;
                     for (i = 1; i <= number_of_dummy_args; i++) {
                        if (OPND_IDX(dummy_opnd[i]) == idx) {
                           found = TRUE;
                           break;
                        }
                     }

                     if (processing_ENTRY_called) {
                        if (!found && !AT_HOST_ASSOCIATED(idx)) {
                           inlinable = FALSE;
                           PRINTMSG(call_line_number,
                                    1345,
                                    Inline,
                                    call_col_number,
                                    AT_OBJ_NAME_PTR(pgm_attr_idx));
                        }
                     }
                  }

                  if (ATD_CLASS(idx) == Compiler_Tmp &&
                      ATD_TMP_INIT_NOT_DONE(idx)) {
                     insert_init_stmt_for_tmp(idx);
                  }

                  if (ATD_CLASS(idx) == CRI__Pointee &&
                      SB_SCP_IDX(sb_idx) != curr_scp_idx) {

                     /* we need a new attr in the local scope */
                     /* and a new segment in the local scope */

                     NTR_ATTR_TBL(new_idx);
                     COPY_ATTR_NTRY(new_idx, idx);
                     idx = new_idx;
                     ADD_ATTR_TO_LOCAL_LIST(idx);

                     new_blk = ntr_stor_blk_tbl(SB_NAME_PTR(sb_idx),
                                                SB_NAME_LEN(sb_idx),
                                                call_line_number,
                                                call_col_number,
                                                SB_BLK_TYPE(sb_idx));

                     COPY_TBL_NTRY(stor_blk_tbl,
                                   new_blk,
                                   sb_idx);

                     SB_SCP_IDX(new_blk) = curr_scp_idx;
                     ATD_STOR_BLK_IDX(idx) = new_blk;
                  }


                  /*
                  If the same COMMON block exists 
                  in the caller and the callee,
                  then an EQUIVALENCE needs to be faked for the two
                  COMMON blocks in question. 
                  The two blocks will be collapsed into one
                  block and the EQUIV bit will be set on all
                  attrs in that block.
                  */
                  if (sb_idx != NULL_IDX &&
                     (SB_BLK_TYPE(sb_idx) == Common ||
                      SB_BLK_TYPE(sb_idx) == Task_Common ||
                      SB_BLK_TYPE(sb_idx) == Threadprivate ||
                      SB_BLK_TYPE(sb_idx) == Static_Named ||
                      SB_BLK_TYPE(sb_idx) == Static_Local ||
                      SB_BLK_TYPE(sb_idx) == Static)) {

                     if (TYP_TYPE(ATD_TYPE_IDX(idx)) == CRI_Ptr ||
                         TYP_TYPE(ATD_TYPE_IDX(idx)) == CRI_Ch_Ptr) {
                        inlinable = FALSE;
                        PRINTMSG(call_line_number, 1359,
                                 Inline,
                                 call_col_number, 
                                 AT_OBJ_NAME_PTR(pgm_attr_idx));
                     }

                     if (ATD_PE_ARRAY_IDX(idx) != NULL_IDX) {
                        inlinable = FALSE;
                        PRINTMSG(call_line_number,
                                 1613,
                                 Inline,
                                 call_col_number,
                                 AT_OBJ_NAME_PTR(pgm_attr_idx));
                     }

                     if (SB_BLK_TYPE(sb_idx) == Static_Local ||
                         SB_BLK_TYPE(sb_idx) == Static_Named) {
                        SB_BLK_TYPE(sb_idx) = Common;
                     }

                     outer_sb_idx = srch_stor_blk_tbl(SB_NAME_PTR(sb_idx),
                                                      SB_NAME_LEN(sb_idx),
                                                      curr_scp_idx);

                     if (outer_sb_idx != NULL_IDX) {
                        original_idx = idx;
                
                        attr_idx = SB_FIRST_ATTR_IDX(outer_sb_idx); 

                        while (attr_idx != NULL_IDX) {
                           type_idx2 = CG_LOGICAL_DEFAULT_TYPE;

                           /*
                           If two objects exactly overlay each other
                           in the two different COMMON blocks, then
                           we will use the attr from the caller's
                           COMMON block.   This helps optimization.
                           */

                           if (idx != NULL_IDX &&
                               ATD_OFFSET_FLD(attr_idx) == CN_Tbl_Idx &&
                               ATD_OFFSET_FLD(idx) == CN_Tbl_Idx) {
                              if (folder_driver(
                                (char *)&CN_CONST(ATD_OFFSET_IDX(attr_idx)),
                                CN_TYPE_IDX(ATD_OFFSET_IDX(attr_idx)),
                                (char *)&CN_CONST(ATD_OFFSET_IDX(idx)),
                                CN_TYPE_IDX(ATD_OFFSET_IDX(idx)),
                                folded_const,
                                &type_idx2,
                                call_line_number,
                                call_col_number,
                                2,
                                Eq_Opr)) {
                              }

                              if (THIS_IS_TRUE(folded_const, type_idx2)) {
                                 if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX &&
                                     ATD_ARRAY_IDX(idx) == NULL_IDX) {
                                    if (TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) ==
                                        TYP_LINEAR(ATD_TYPE_IDX(idx))) {
                                       idx = attr_idx;
                                    }
                                 }

                                 if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
                                     ATD_ARRAY_IDX(idx) != NULL_IDX) {
                                    if (TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) ==
                                        TYP_LINEAR(ATD_TYPE_IDX(idx)) &&
                                        BD_RANK(ATD_ARRAY_IDX(attr_idx)) ==
                                        BD_RANK(ATD_ARRAY_IDX(idx))) {
                                        for (i = 1; 
                                             i <= BD_RANK(ATD_ARRAY_IDX(idx)); 
                                             i++) {


     if (BD_XT_FLD(ATD_ARRAY_IDX(attr_idx), i) == CN_Tbl_Idx &&
         BD_XT_FLD(ATD_ARRAY_IDX(idx), i) == CN_Tbl_Idx) { 

        if (folder_driver(
               (char *)&CN_CONST(BD_XT_IDX(ATD_ARRAY_IDX(attr_idx), i)),
               CN_TYPE_IDX(BD_XT_IDX(ATD_ARRAY_IDX(attr_idx), i)),
               (char *)&CN_CONST(BD_XT_IDX(ATD_ARRAY_IDX(idx), i)),
               CN_TYPE_IDX(BD_XT_IDX(ATD_ARRAY_IDX(idx), i)),
               folded_const,
               &type_idx2,
               call_line_number,
               call_col_number,
               2,
               Eq_Opr)) {
        }

        if (THIS_IS_TRUE(folded_const, type_idx2)) {
           idx = attr_idx;
        }
     }
                                     
                                        
                                       }
                                    }
                                 }
                              }
                           }
                               
                           attr_idx = ATD_NEXT_MEMBER_IDX(attr_idx);
                        }


                        /*
                        Increase the length of the caller's storage
                        segment if the callee's was larger.
                        */
                        if (SB_LEN_FLD(sb_idx) == CN_Tbl_Idx &&
                            SB_LEN_FLD(outer_sb_idx) == CN_Tbl_Idx) {
                           type_idx2 = CG_LOGICAL_DEFAULT_TYPE;

                           if (folder_driver(
                                (char *)&CN_CONST(SB_LEN_IDX(sb_idx)),
                                CN_TYPE_IDX(SB_LEN_IDX(sb_idx)),
                                (char *)&CN_CONST(SB_LEN_IDX(outer_sb_idx)),
                                CN_TYPE_IDX(SB_LEN_IDX(outer_sb_idx)),
                                folded_const,
                                &type_idx2,
                                call_line_number,
                                call_col_number,
                                2,
                                Gt_Opr)) {
                           }


                           if (THIS_IS_TRUE(folded_const, type_idx2)) {
                              if (inlinable &&
                                 (strcmp(SB_NAME_PTR(outer_sb_idx), 
                                         sb_name[What_Blk]) != 0)) {

                                 SB_LEN_IDX(outer_sb_idx) = SB_LEN_IDX(sb_idx);
                                 PRINTMSG(call_line_number,
                                          1524,
                                          Warning,
                                          call_col_number, 
                                          SB_BLANK_COMMON(outer_sb_idx) ?
                                          "" : SB_NAME_PTR(outer_sb_idx),
                                          AT_OBJ_NAME_PTR(pgm_attr_idx));
                              }
                           }
                        }

                        if (original_idx == idx) {
                           SB_DEF_MULT_SCPS(outer_sb_idx) = TRUE;
                        }

                        ATD_STOR_BLK_IDX(idx) = outer_sb_idx;
      
                        /* PDGCS does not optimize these correctly. */
                        if (ATD_POINTER(idx)) {
                           inlinable = FALSE;
                           PRINTMSG(call_line_number,
                                    1337,
                                    Inline,
                                    call_col_number, 
                                    AT_OBJ_NAME_PTR(pgm_attr_idx));
                        }
                     }
                     else {  /* not found in the caller's scope */
                        /*
                        If this storage block is not in the current
                        scope, then we need to make a copy of the
                        storage block and move it into the current
                        scope.   The variable being processed then
                        becomes part of the newly created storage
                        block.
                        */
                        if (SB_SCP_IDX(sb_idx) != curr_scp_idx) {
                           new_blk = ntr_stor_blk_tbl(SB_NAME_PTR(sb_idx),
                                                      SB_NAME_LEN(sb_idx),
                                                      call_line_number,
                                                      call_col_number,
                                                      SB_BLK_TYPE(sb_idx));

                           COPY_TBL_NTRY(stor_blk_tbl, 
                                         new_blk, 
                                         sb_idx);

                           SB_SCP_IDX(new_blk) = curr_scp_idx;
                           ATD_STOR_BLK_IDX(idx) = new_blk;
                        }
                     }
                  }
               }
            }

            new_root = idx;
            break;

         case CN_Tbl_Idx :
            new_root = idx;
            break;

         case SH_Tbl_Idx :
            new_root = NULL_IDX;
            break;

         case IL_Tbl_Idx :
            trail = NULL_IDX;
            while (idx != NULL_IDX) {
               NTR_IR_LIST_TBL(list_idx);
               COPY_OPND(IL_OPND(list_idx), IL_OPND(idx));
               IL_PREV_LIST_IDX(list_idx) = NULL_IDX;
               IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;

               if (IL_ARG_DESC_VARIANT(idx)) {
                  IL_ARG_DESC_VARIANT(list_idx) = TRUE;
                  IL_ARG_DESC_IDX(list_idx) = IL_ARG_DESC_IDX(idx);
               }
               else {
                  IL_PREV_LIST_IDX(list_idx) = trail;
               }

               if (trail != NULL_IDX) {
                  IL_NEXT_LIST_IDX(trail) = list_idx;
               }
               else {
                  new_root = list_idx;
               }

               IL_PE_SUBSCRIPT(list_idx) = IL_PE_SUBSCRIPT(idx);

               new_idx = copy_sbtree(IL_IDX(idx), IL_FLD(idx));
               IL_IDX(list_idx) = new_idx;

               if (IL_FLD(idx) == AT_Tbl_Idx) {
                  for (i = 0; i <= number_of_dummy_args; i++) {
                     if (IL_IDX(idx) == OPND_IDX(dummy_opnd[i])) {
                        if (OPND_FLD(actual_opnd[i]) == AT_Tbl_Idx &&
                            ATD_AUTOMATIC(OPND_IDX(actual_opnd[i]))) {
                           COPY_OPND(IL_OPND(list_idx), actual_opnd[i]);
                        }
                        else if (OPND_IDX(struct_tree[i]) != NULL_IDX) {
                           COPY_OPND(IL_OPND(list_idx), struct_tree[i]);
                        }
                        else {
                           COPY_OPND(IL_OPND(list_idx), actual_opnd[i]);
                        }
                     }
                  }

                  if (AT_OBJ_CLASS(IL_IDX(idx)) == Label) {
                     for (k = 0; k < next_label_slot; k++) {
                        if (IL_IDX(idx) == old_label[k]) {
                           break;
                        }
                     }

                     if (k < next_label_slot) {
                        IL_IDX(list_idx) = new_label[k];
                        IL_FLD(list_idx) = AT_Tbl_Idx;
                     }
                     else {
                        old_label[next_label_slot] = IL_IDX(idx);
                        new_label_attr = gen_internal_lbl(call_line_number);
                        COPY_COMMON_ATTR_INFO(IL_IDX(idx), 
                                              new_label_attr, 
					      Label);
                        COPY_VARIANT_ATTR_INFO(IL_IDX(idx), 
					       new_label_attr, 
					       Label);
                        AT_ATTR_LINK(new_label_attr) = NULL_IDX;
                        new_label[next_label_slot] = new_label_attr;
 
                        IL_IDX(list_idx) = new_label_attr;
                        IL_FLD(list_idx) = AT_Tbl_Idx;

                        if (ATL_DIRECTIVE_LIST(new_label_attr) != NULL_IDX) {
                           il_idx = IL_IDX(ATL_DIRECTIVE_LIST(new_label_attr)) +
                                    Cache_Bypass_Dir_Idx;

                           if (IL_FLD(il_idx) == IL_Tbl_Idx) {
                              il_idx = IL_IDX(il_idx);

                              while (il_idx != NULL_IDX) {
                                 for (i = 1; i <= number_of_dummy_args; i++) {
                                    if (OPND_IDX(dummy_opnd[i]) == 
                                                              IL_IDX(il_idx)) {
                                       IL_IDX(il_idx)=OPND_IDX(actual_opnd[i]);
                                       break;
                                    }
                                 }

                                 il_idx = IL_NEXT_LIST_IDX(il_idx);
                              }
                           }
                        }
 
                        next_label_slot = next_label_slot + 1;
                        if (next_label_slot == MAX_INLINE_LABELS) {
                           next_label_slot = next_label_slot - 1;
                           inlinable = FALSE;
                           table_overflow = TRUE;
                        }
                     }
                  }
               }

               if (IL_FLD(idx) != IL_Tbl_Idx) {
                  IL_LINE_NUM(list_idx) = call_line_number;
                  IL_COL_NUM(list_idx) = call_col_number;
               }

               trail = list_idx;
               idx = IL_NEXT_LIST_IDX(idx);
            }
            break;
      }
   }


   TRACE (Func_Exit, "copy_sbtree", NULL);

   return(new_root);

}  /* copy_sbtree */




/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      This routine is the driver to create a copy of the called routine.    *|
|*      A copy is created from the template of the routine.		      *|
|*									      *|
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
void	 make_copy_of_routine(int	 original_head)

{
   int           copy_trail;
   int           original_sh;
   int           new_sh;
   int           new_ir;

   TRACE (Func_Entry, "make_copy_of_routine", NULL);
      copy_head = NULL_IDX;

      original_sh = original_head;
      copy_trail = copy_head;

      while (original_sh != NULL_IDX) {
         new_sh = ntr_sh_tbl();
         if (copy_head == NULL_IDX) {
            copy_head = new_sh;
         }
         COPY_TBL_NTRY(sh_tbl, new_sh, original_sh);
         SH_NEXT_IDX(new_sh) = NULL_IDX;
         SH_PREV_IDX(new_sh) = NULL_IDX;
         SH_GLB_LINE(new_sh) = call_line_number;
         SH_COL_NUM(new_sh) = call_col_number;
         new_ir = copy_sbtree(SH_IR_IDX(original_sh), IR_Tbl_Idx);
         sh_count = sh_count + 1;
         SH_IR_IDX(new_sh) = new_ir;

         SH_PREV_IDX(new_sh) = copy_trail;
         if (SH_PREV_IDX(new_sh) != NULL_IDX) {
            SH_NEXT_IDX(SH_PREV_IDX(new_sh)) = new_sh;
         }
         copy_trail = new_sh;
         original_sh = SH_NEXT_IDX(original_sh);
      }
         
   TRACE (Func_Exit, "make_copy_of_routine", NULL);

   return;

}  /* make_copy_of_routine */






/******************************************************************************\
|*									      *|
|* Description:								      *|
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
void	srch_for_calls(int         ir_idx,
                       fld_type    field)
{

   id_str_type          stor_name;
   int			actual_arg_list_idx;
   int			new_darg_attr;
   int			struct_base_attr_idx	= NULL_IDX;
   int			copy_in_sh;
   int			idx;
   int			sub;
   int			list_idx;
   int			list_idx1;
   int			list_idx2;
   int			attr_idx;
   int			type_idx;
   int			loc_idx;
   fld_type    		loc_fld;
   int			based_blk;
   int			div_idx;
   int			asg_idx;
   int			new_idx;
   int                  al_idx;
   int                  tmp_al_idx;
   int                  flipped_bd_idx;
   int                  dummy_bd_idx;
   int           	actual_bd_idx;
   int			tmp_attr;
   int			tmp_sh;
   int			minus_idx;
   int			i;
   int			j;
   int			k;
   int			l;
   int			line;
   int			col;
   int           	copy_out_array_element;
   int           	copy_out_DV_scalar;
   opnd_type		opnd;
   boolean		name_substitution;
   boolean		call_by_value;
   boolean		dummy_modified;
   boolean		dummy_referenced;


   TRACE (Func_Entry, "srch_for_calls", NULL);

   switch (field) {
      case NO_Tbl_Idx :
      break;

      case AT_Tbl_Idx :
      break;

      case IR_Tbl_Idx :
      switch (IR_OPR(ir_idx)) { 
         case Noinline_Cdir_Opr :
         noinline_in_effect = TRUE;
         inline_in_effect = FALSE;
         break;

         case Inline_Cdir_Opr :
         noinline_in_effect = FALSE;
         inline_in_effect = TRUE;
         break;

         case Call_Opr :
         call_line_number = IR_LINE_NUM_L(SH_IR_IDX(call_sh));
         call_col_number = IR_COL_NUM_L(SH_IR_IDX(call_sh));

         next_label_slot = 0;
         pgm_attr_idx = IR_IDX_L(ir_idx);

         /* 
         Starting processing for a new Call_Opr in the
         IR stream.   Clean up everything.   Clear out
         all the tables.
         */
         if (ATP_PROC(pgm_attr_idx) != Intrin_Proc &&
             !SH_INLINING_ATTEMPTED(call_sh) &&
             AT_OBJ_NAME(pgm_attr_idx) != '$' &&
             AT_OBJ_NAME(pgm_attr_idx) != '_') {
            inlinable = !table_overflow;
            SH_INLINING_ATTEMPTED(call_sh) = TRUE;
            next_copy_out_sh_idx = 0;

            for (i = 0; i <= 8; i++) {
               subscript[i] = null_opnd; 
               subscript_attr[i] = null_opnd;
            }

            for (i = 0; i <= MAX_INLINE_ARGS-1; i++) {
               copy_out_sh[i] = NULL_IDX;
               actual_arg_attrs[i] = NULL_IDX;
               flipped_opnd[i] = null_opnd;
               actual_opnd[i] = null_opnd; 
               dummy_opnd[i] = null_opnd;
               struct_tree[i] = null_opnd;
               subscripting_tree[i] = null_opnd;
               substringing_tree[i] = null_opnd;
               OPND_IDX(substring_offset[i]) = CN_INTEGER_ZERO_IDX;
               OPND_FLD(substring_offset[i]) = CN_Tbl_Idx;
               for (k = 0; k <= 8; k++) {
                  OPND_IDX(linearized_offset[i][k]) = CN_INTEGER_ZERO_IDX;
                  OPND_FLD(linearized_offset[i][k]) = CN_Tbl_Idx;
               }

            }

            number_of_actual_args = IR_LIST_CNT_R(ir_idx);

            if (cmd_line_flags.runtime_argument ||
                cmd_line_flags.runtime_arg_call ||
                cmd_line_flags.runtime_arg_count_only) {
               number_of_actual_args = number_of_actual_args - 1;
            }

            /*
            This check here is a saftey valve.   Table sizes
            are checked here.   If we are approaching dangerous limits,
            we just stop inlining.  The values are arbitrary.
            */
            if (attr_list_tbl_idx > 60536) {   /*  2 ** 16 - 5000  */
               inlinable = FALSE;
               PRINTMSG(call_line_number,
                        1202,
                        Inline,
                        call_col_number, 
                        AT_OBJ_NAME_PTR(pgm_attr_idx),
                        "internal table(s) almost full");
            }

            if (inlinable && ATP_PROC(pgm_attr_idx) == Dummy_Proc) {
               inlinable = FALSE;
               PRINTMSG(call_line_number,
                        1333,
                        Inline,
                        call_col_number, 
                        AT_OBJ_NAME_PTR(pgm_attr_idx));
            }

            if (inlinable && ATP_ELEMENTAL(pgm_attr_idx)) {
               inlinable = FALSE;
               PRINTMSG(call_line_number,
                        1657,
                        Inline,
                        call_col_number, 
                        AT_OBJ_NAME_PTR(pgm_attr_idx));
            }

            if (inlinable && ATP_FIRST_SH_IDX(pgm_attr_idx) == NULL_IDX) {

               if (! find_prog_unit_tbl(pgm_attr_idx)) {
                  inlinable = FALSE;
                  if (ATP_PROC(pgm_attr_idx) == Module_Proc) {
                     PRINTMSG(call_line_number, 
                              1495, 
                              Inline, 
                              call_col_number,
                              AT_OBJ_NAME_PTR(pgm_attr_idx));
                  }
                  else {
                     PRINTMSG(call_line_number,
                              1344,
                              Inline,
                              call_col_number,
                              AT_OBJ_NAME_PTR(pgm_attr_idx));
                  }
               }
               else {
                  if (ATP_PGM_UNIT(pgm_attr_idx) == Function &&
                      ATP_PGM_UNIT(AT_ATTR_LINK(pgm_attr_idx)) == Function &&
                      ATP_RSLT_IDX(pgm_attr_idx) != NULL_IDX &&
                      ATP_RSLT_IDX(AT_ATTR_LINK(pgm_attr_idx)) != NULL_IDX &&
                      TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(pgm_attr_idx))) !=
                      TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(
                                              AT_ATTR_LINK(pgm_attr_idx))))) {
                     inlinable = FALSE;
                     PRINTMSG(call_line_number,
                              1425,
                              Inline,
                              call_col_number,
                              AT_OBJ_NAME_PTR(pgm_attr_idx));
                  }
                  else {
                     ATP_FIRST_SH_IDX(pgm_attr_idx) = 
                     ATP_FIRST_SH_IDX(AT_ATTR_LINK(pgm_attr_idx));

                     ATP_RSLT_IDX(pgm_attr_idx) = 
                     ATP_RSLT_IDX(AT_ATTR_LINK(pgm_attr_idx));

                     ATP_NUM_DARGS(pgm_attr_idx) = 
                     ATP_NUM_DARGS(AT_ATTR_LINK(pgm_attr_idx));

                     ATP_FIRST_IDX(pgm_attr_idx) = 
                     ATP_FIRST_IDX(AT_ATTR_LINK(pgm_attr_idx));

                     ATP_INLINE_NEVER(pgm_attr_idx) =
                     ATP_INLINE_NEVER(pgm_attr_idx) ||
                     ATP_INLINE_NEVER(AT_ATTR_LINK(pgm_attr_idx));

                     ATP_RECURSIVE(pgm_attr_idx) =
                     ATP_RECURSIVE(pgm_attr_idx) ||
                     ATP_RECURSIVE(AT_ATTR_LINK(pgm_attr_idx));

                     ATP_INLINE_ALWAYS(pgm_attr_idx) =
                     ATP_INLINE_ALWAYS(pgm_attr_idx) ||
                     ATP_INLINE_ALWAYS(AT_ATTR_LINK(pgm_attr_idx));

                     ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) =
                     ATP_HAS_TASK_DIRS(pgm_attr_idx) ||
                     ATP_HAS_TASK_DIRS(AT_ATTR_LINK(pgm_attr_idx)) ||
                     ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx));  

                     ATP_HAS_OVER_INDEXING(SCP_ATTR_IDX(curr_scp_idx)) =
                     ATP_HAS_OVER_INDEXING(pgm_attr_idx) ||
                     ATP_HAS_OVER_INDEXING(AT_ATTR_LINK(pgm_attr_idx)) ||
                     ATP_HAS_OVER_INDEXING(SCP_ATTR_IDX(curr_scp_idx)); 
  
                     AT_ATTR_LINK(pgm_attr_idx) = NULL_IDX;
                  }
               }
            }

            number_of_dummy_args = ATP_NUM_DARGS(pgm_attr_idx);

            /*
            The FUNCTION result gets stuffed into the 0th element.
            Otherwise, the 0th element is empty.
            */
            if (function_call) { 
               idx = SH_IR_IDX(call_sh);
               COPY_OPND(actual_opnd[0], IR_OPND_L(idx));
               OPND_IDX(dummy_opnd[0]) = ATP_RSLT_IDX(pgm_attr_idx);
               OPND_FLD(dummy_opnd[0]) = AT_Tbl_Idx; 
            }

            if (inlinable && opt_flags.inline_lvl == Inline_Lvl_1) {
               if (!ATP_INLINE_ALWAYS(pgm_attr_idx) &&
                   !inline_in_effect) {
                  inlinable = FALSE;
                  PRINTMSG(call_line_number,
                           1335,
                           Inline,
                           call_col_number, 
                           AT_OBJ_NAME_PTR(pgm_attr_idx));
               }
            }

            if (inlinable && opt_flags.inline_lvl == Inline_Lvl_2) {
               if (!ATP_INLINE_ALWAYS(pgm_attr_idx) &&
                   !inline_in_effect &&
                   loop_nest <= 0) {
                  inlinable = FALSE;
                  PRINTMSG(call_line_number,
                           1336,
                           Inline,
                           call_col_number,
                           AT_OBJ_NAME_PTR(pgm_attr_idx));
               }
            }

            if (inlinable && noinline_in_effect) {
               inlinable = FALSE;
               PRINTMSG(call_line_number,
                        1338,
                        Inline,
                        call_col_number, 
                        AT_OBJ_NAME_PTR(pgm_attr_idx));
            }

            if (inlinable && ATP_INLINE_NEVER(pgm_attr_idx)) {
               inlinable = FALSE;
               PRINTMSG(call_line_number,
                        1339,
                        Inline,
                        call_col_number, 
                        AT_OBJ_NAME_PTR(pgm_attr_idx));
            }

            if (inlinable && (number_of_dummy_args != number_of_actual_args)) {
               inlinable = FALSE;
               PRINTMSG(call_line_number,
                        1342,
                        Inline,
                        call_col_number, 
                        AT_OBJ_NAME_PTR(pgm_attr_idx));
            }

            if (inlinable && (number_of_actual_args >= MAX_INLINE_ARGS)) {
               inlinable = FALSE;
               PRINTMSG(call_line_number,
                        1343,
                        Inline,
                        call_col_number, 
                        AT_OBJ_NAME_PTR(pgm_attr_idx));
            }

            if (inlinable && ATP_RECURSIVE(pgm_attr_idx)) {
               inlinable = FALSE;
               PRINTMSG(call_line_number,
                        1332,
                        Inline,
                        call_col_number, 
                        AT_OBJ_NAME_PTR(pgm_attr_idx));
            }

            if (OPND_IDX(dummy_opnd[0]) != NULL_IDX &&
                inlinable && AT_HOST_ASSOCIATED(OPND_IDX(dummy_opnd[0]))) {
               inlinable = FALSE;
               PRINTMSG(call_line_number,
                        1357,
                        Inline,
                        call_col_number, 
                        AT_OBJ_NAME_PTR(pgm_attr_idx));
            }

            actual_arg_list_idx = IR_IDX_R(ir_idx);
            for (k = 1; k <= number_of_actual_args; k++) {
               attr_idx = find_base_attr(&IL_OPND(actual_arg_list_idx),
                                         &line,
                                         &col);
               actual_arg_attrs[k] = attr_idx;
               actual_arg_list_idx = IL_NEXT_LIST_IDX(actual_arg_list_idx);
            }

            /*    
            This WHILE loop processes all the actual arguments in the
            actual arg list hanging off the Call_Opr.
            */
            i = 1;
            j = ATP_FIRST_IDX(pgm_attr_idx);
            actual_arg_list_idx = IR_IDX_R(ir_idx);
            while (actual_arg_list_idx != NULL_IDX &&
                   number_of_actual_args > 0 &&
                   inlinable &&
                   i <= number_of_actual_args) {
               copy_out_array_element = NULL_IDX;
               copy_out_DV_scalar = NULL_IDX;

               OPND_IDX(dummy_opnd[i]) = SN_ATTR_IDX(j);
               OPND_FLD(dummy_opnd[i]) = AT_Tbl_Idx; 

               /*
               Determine if the dummy argument every gets modified
               by the inlined code.
               */
               dummy_modified = AT_DEFINED(SN_ATTR_IDX(j)) ||
                                AT_ACTUAL_ARG(SN_ATTR_IDX(j)) ||
                                AT_DEF_IN_CHILD(SN_ATTR_IDX(j));

               if (AT_OBJ_CLASS(SN_ATTR_IDX(j)) == Data_Obj &&
                   ATD_INTENT(SN_ATTR_IDX(j)) == Intent_In) {
                  dummy_modified = FALSE;
               }

               dummy_referenced = AT_REFERENCED(SN_ATTR_IDX(j)) == Referenced;

               if (AT_OBJ_CLASS(SN_ATTR_IDX(j)) == Data_Obj &&
                   ATD_INTENT(SN_ATTR_IDX(j)) == Intent_Out) {
                  dummy_referenced = FALSE;
               }

               /*
               If we have a derived type containing dope vectors, some 
               initialization of the DV may have occured.   We cannot
               assume Intent_Out.
               */
               if (AT_OBJ_CLASS(SN_ATTR_IDX(j)) == Data_Obj &&
                   TYP_TYPE(ATD_TYPE_IDX(SN_ATTR_IDX(j))) == Structure) {
                  dummy_referenced = TRUE;
               }

               if (inlinable && AT_HOST_ASSOCIATED(OPND_IDX(dummy_opnd[i]))) {
                  inlinable = FALSE;
                  PRINTMSG(call_line_number,
                           1341,
                           Inline,
                           call_col_number, 
                           AT_OBJ_NAME_PTR(pgm_attr_idx));
               }

               if (inlinable && AT_OPTIONAL(OPND_IDX(dummy_opnd[i]))) {
                  inlinable = FALSE;
                  PRINTMSG(call_line_number,
                           1334,
                           Inline,
                           call_col_number, 
                           AT_OBJ_NAME_PTR(pgm_attr_idx));
               }

               if (inlinable) { 
                  if (AT_OBJ_CLASS(OPND_IDX(dummy_opnd[i])) != Data_Obj) {
                     inlinable = FALSE;
                     PRINTMSG(call_line_number,
                              1340,
                              Inline,
                              call_col_number, 
                              AT_OBJ_NAME_PTR(pgm_attr_idx));
                     break;
                  }
                  else {   /* we have a Data_Obj */
                     if (TYP_TYPE(ATD_TYPE_IDX(SN_ATTR_IDX(j))) == CRI_Ptr ||
                         TYP_TYPE(ATD_TYPE_IDX(SN_ATTR_IDX(j))) == CRI_Ch_Ptr) {
                        inlinable = FALSE;
                        PRINTMSG(call_line_number,
                                 1355,
                                 Inline,
                                 call_col_number, 
                                 AT_OBJ_NAME_PTR(pgm_attr_idx));
                     }

                     if (ATD_PE_ARRAY_IDX(OPND_IDX(dummy_opnd[i])) !=NULL_IDX) {
                        inlinable = FALSE;
                        PRINTMSG(call_line_number,
                                 1601,
                                 Inline,
                                 call_col_number,
                                 AT_OBJ_NAME_PTR(pgm_attr_idx));
                     }
                  }
               }

               call_by_value = FALSE;
               if (IL_FLD(actual_arg_list_idx) == IR_Tbl_Idx &&
                   (IR_OPR(IL_IDX(actual_arg_list_idx)) == Aloc_Opr ||
                    IR_OPR(IL_IDX(actual_arg_list_idx)) == Const_Tmp_Loc_Opr)) {
                  COPY_OPND(actual_opnd[i],
                            IR_OPND_L(IL_IDX(actual_arg_list_idx)));
               }
               else {
                  COPY_OPND(actual_opnd[i], IL_OPND(actual_arg_list_idx));

                  /*
                  The only time there should be an AT passed as
                  an actual argument is when it is call by value.
                  */
                  if (OPND_FLD(actual_opnd[i]) == AT_Tbl_Idx) {
                     call_by_value = TRUE;
                  }
               }

   	       /*
   	       This IF is for mapping dope-vector based scalar
   	       actual arguments to scalar dummy arguments.

   	       COMPLEX CPOINTER, COMPLEX1
   	       POINTER CPOINTER
   	       TARGET COMPLEX1

   	       CPOINTER => COMPLEX1
   	       CPOINTER = (-100,100)
   	       CALL ASSGN(CPOINTER)
   	       PRINT*, CPOINTER
   	       END

   	       SUBROUTINE ASSGN(COMPLEX1)
   	       COMPLEX  COMPLEX1
   	       COMPLEX1 = (-1,-1)
   	       END
   	       */
   	       if (inlinable &&
                   OPND_FLD(actual_opnd[i]) == IR_Tbl_Idx &&
       	           IR_OPR(OPND_IDX(actual_opnd[i])) == Dv_Access_Base_Addr &&
       		   ATD_ARRAY_IDX(OPND_IDX(dummy_opnd[i])) == NULL_IDX) {
		  scalar_dope_to_scalar(i, 
    				        &copy_out_DV_scalar, 
				        dummy_referenced);
   	       }


               /*
               This IF is for processing an array element actual
               argument mapped to a scalar dummy argument.
               before inlining: 
                                   PROGRAM C
                                   DIMENSION A(10)
                                   COMMON // I
                                   I = 4
                                   CALL SAM(A(I))
                                   END
 
                                   SUBROUTINE SAM(S)
                                   COMMON // I
                                   S = S + 5.0    
                                   I = I + 1
                                   END
               after inlining:
                                   PROGRAM C
                                   DIMENSION A(10)
                                   t$1 = I
                                   t$2 = A(I)
                                   t$2 = t$2 + 5.0
                                   I = I + 1
                                   A(t$1) = t$2
                                   END
               */
               if (inlinable &&
                   OPND_FLD(actual_opnd[i]) == IR_Tbl_Idx &&
                   IR_OPR(OPND_IDX(actual_opnd[i])) == Subscript_Opr &&
                   ATD_ARRAY_IDX(OPND_IDX(dummy_opnd[i])) == NULL_IDX) {
		  array_element_to_scalar(i, 
    			  	          &copy_out_array_element, 
				          dummy_referenced,
				          dummy_modified);

               }

               /*
               This IF block processes character mappings.
               */
               if (inlinable &&
                   OPND_FLD(actual_opnd[i]) == IR_Tbl_Idx &&
                   (IR_OPR(OPND_IDX(actual_opnd[i])) == Whole_Substring_Opr ||
                    IR_OPR(OPND_IDX(actual_opnd[i])) == Substring_Opr)) {
		  character_to_character(i);
               }

               /*
               This IF block processes structure mappings.
               */
               if (inlinable &&
                   OPND_FLD(actual_opnd[i]) == IR_Tbl_Idx &&
                   IR_OPR(OPND_IDX(actual_opnd[i])) == Struct_Opr) {
                  COPY_OPND(struct_tree[i], actual_opnd[i]);

                  struct_base_attr_idx = find_base_attr(&actual_opnd[i],
                                                        &line,
                                                        &col);

                  COPY_OPND(actual_opnd[i], 
                            IR_OPND_L(OPND_IDX(actual_opnd[i])));
               }

               if ((call_by_value && inlinable) ||
                   (OPND_FLD(actual_opnd[i]) == IR_Tbl_Idx &&
                    inlinable &&
                    (IR_OPR(OPND_IDX(actual_opnd[i])) == Dv_Access_Base_Addr ||
                     IR_OPR(OPND_IDX(actual_opnd[i])) == Subscript_Opr))) {
                  COPY_OPND(subscripting_tree[i], actual_opnd[i]);
                  new_darg_attr = gen_compiler_tmp(call_line_number,
                                                   call_col_number,
                                                   Priv, TRUE);

                  attr_idx = find_base_attr(&actual_opnd[i],
                                            &line,
                                            &col);

                  if (struct_base_attr_idx != NULL_IDX) {
                     attr_idx = struct_base_attr_idx;
                  }

                  OPND_IDX(flipped_opnd[i]) = attr_idx;
                  OPND_FLD(flipped_opnd[i]) = AT_Tbl_Idx;
                  OPND_IDX(actual_opnd[i]) = attr_idx;
                  OPND_FLD(actual_opnd[i]) = AT_Tbl_Idx;

                  name_substitution =
                     check_actual_and_dummy(actual_opnd[i], dummy_opnd[i], i);

                  /*
                  If the call list contains more than one referenced to 
                  the same array,  name substitution will NOT be performed.
                     eg.
                         call sam(A(4), A(8), B(1))   
                  */
                  for (k = 1; k <= number_of_actual_args; k++) {
                     if (k != i &&
                         OPND_IDX(actual_opnd[i]) == actual_arg_attrs[k]) {
                        name_substitution = FALSE;
                     }
                  }

                  /*
                  Save away the expressions that will be used to linearize
                  the references to the corresponding dummy arguments in 
                  the inlined code.
                  */
 
                  if (name_substitution) {
                     actual_bd_idx = ATD_ARRAY_IDX(OPND_IDX(actual_opnd[i]));
                     sub = IR_IDX_R(OPND_IDX(subscripting_tree[i]));
                     k = 1;
                     while (sub != NULL_IDX) {
   		       NTR_IR_TBL(minus_idx);
   		       IR_OPR(minus_idx) = Minus_Opr;
   		       IR_TYPE_IDX(minus_idx) = CG_INTEGER_DEFAULT_TYPE;
   		       IR_LINE_NUM(minus_idx) = call_line_number;
   		       IR_COL_NUM(minus_idx) = call_col_number;
   		       COPY_OPND(IR_OPND_L(minus_idx), IL_OPND(sub));
   		       IR_LINE_NUM_L(minus_idx) = call_line_number;
   		       IR_COL_NUM_L(minus_idx) = call_col_number;
   		       IR_IDX_R(minus_idx) = BD_LB_IDX(actual_bd_idx, k);
   		       IR_FLD_R(minus_idx) = BD_LB_FLD(actual_bd_idx, k);
   		       IR_LINE_NUM_R(minus_idx) = call_line_number;
   		       IR_COL_NUM_R(minus_idx) = call_col_number;
   		       OPND_IDX(linearized_offset[i][k]) = minus_idx;
   		       OPND_FLD(linearized_offset[i][k]) = IR_Tbl_Idx;

                       sub = IL_NEXT_LIST_IDX(sub);
                       k = k + 1;
                    }
                  }

                  /*
                  Insert the Copy_In_Opr prior to the expanded code.
                  Insert the Copy_Out_Opr after the expanded code.
                  */
                  copy_in_sh = call_sh;
                  if (!name_substitution && inlinable &&
                      OPND_FLD(subscripting_tree[i]) == IR_Tbl_Idx &&
                      IR_OPR(OPND_IDX(subscripting_tree[i])) == Subscript_Opr) {
                     NTR_IR_TBL(asg_idx);
                     IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
                     IR_OPR(asg_idx) = Copy_In_Opr;
                     IR_LINE_NUM(asg_idx) = call_line_number;
                     IR_COL_NUM(asg_idx) = call_col_number;
                     IR_FLD_L(asg_idx) = AT_Tbl_Idx;
                     IR_IDX_L(asg_idx) = new_darg_attr;
                     IR_FLD_R(asg_idx) = OPND_FLD(subscripting_tree[i]);
                     IR_IDX_R(asg_idx) = OPND_IDX(subscripting_tree[i]);
                     IR_LINE_NUM_L(asg_idx) = call_line_number;
                     IR_COL_NUM_L(asg_idx) = call_col_number;
                     IR_LINE_NUM_R(asg_idx) = call_line_number;
                     IR_COL_NUM_R(asg_idx) = call_col_number;

                     curr_stmt_sh_idx = call_sh;
                     gen_sh(Before,
                            Assignment_Stmt,
                            call_line_number,
                            call_col_number,
                            FALSE,
                            FALSE,
                            TRUE);
                     SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
                     SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
                     copy_in_sh = SH_PREV_IDX(curr_stmt_sh_idx);

                     NTR_IR_TBL(asg_idx);
                     IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
                     IR_OPR(asg_idx) = Copy_Out_Opr;
                     IR_LINE_NUM(asg_idx) = call_line_number;
                     IR_COL_NUM(asg_idx) = call_col_number;
                     IR_FLD_L(asg_idx) = OPND_FLD(subscripting_tree[i]);
                     IR_IDX_L(asg_idx) = OPND_IDX(subscripting_tree[i]);
                     IR_FLD_R(asg_idx) = AT_Tbl_Idx;
                     IR_IDX_R(asg_idx) = new_darg_attr;
                     IR_LINE_NUM_L(asg_idx) = call_line_number;
                     IR_COL_NUM_L(asg_idx) = call_col_number;
                     IR_LINE_NUM_R(asg_idx) = call_line_number;
                     IR_COL_NUM_R(asg_idx) = call_col_number;

                     curr_stmt_sh_idx = SH_NEXT_IDX(call_sh);
                     gen_sh(Before,
                            Assignment_Stmt,
                            call_line_number,
                            call_col_number,
                            FALSE,
                            FALSE,
                            TRUE);
                     SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
                     SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
                  }

                  tmp_attr = gen_compiler_tmp(call_line_number, 
                                              call_col_number,
                                              Priv, TRUE);

                  ATD_STOR_BLK_IDX(tmp_attr) = SCP_SB_STACK_IDX(curr_scp_idx);
                  AT_SEMANTICS_DONE(tmp_attr) = TRUE;
                  AT_DEFINED(tmp_attr) = TRUE;

                  if (TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(dummy_opnd[i]))) == 
                                                                    Character) {
                     ATD_TYPE_IDX(tmp_attr) = CRI_Ch_Ptr_8;

                     if (TYP_CHAR_CLASS(ATD_TYPE_IDX(OPND_IDX(dummy_opnd[i]))) 
                                                     == Assumed_Size_Char &&
                         OPND_FLD(subscripting_tree[i]) == IR_Tbl_Idx &&
                         IR_OPR(OPND_IDX(subscripting_tree[i])) == 
                                                      Dv_Access_Base_Addr) {
                        NTR_IR_TBL(asg_idx);
                        IR_TYPE_IDX(asg_idx) = CG_INTEGER_DEFAULT_TYPE;
                        IR_OPR(asg_idx) = Dv_Access_El_Len;
                        IR_LINE_NUM(asg_idx) = call_line_number;
                        IR_COL_NUM(asg_idx) = call_col_number;
                        IR_FLD_L(asg_idx) = AT_Tbl_Idx;
                        IR_IDX_L(asg_idx) =
                                IR_IDX_L(OPND_IDX(subscripting_tree[i]));
                        IR_LINE_NUM_L(asg_idx) = call_line_number;
                        IR_COL_NUM_L(asg_idx) = call_col_number;

                        NTR_IR_TBL(div_idx);
                        IR_TYPE_IDX(div_idx) = CG_INTEGER_DEFAULT_TYPE;
                        IR_OPR(div_idx) = Shiftr_Opr;
                        IR_LINE_NUM(div_idx) = call_line_number;
                        IR_COL_NUM(div_idx) = call_col_number;
                        IR_FLD_L(div_idx) = IR_Tbl_Idx;
                        IR_IDX_L(div_idx) = asg_idx;
                        IR_LINE_NUM_L(div_idx) = call_line_number;
                        IR_COL_NUM_L(div_idx) = call_col_number;
                        IR_FLD_R(div_idx) = CN_Tbl_Idx;
                        IR_IDX_R(div_idx) = CN_INTEGER_THREE_IDX;
                        IR_LINE_NUM_R(div_idx) = call_line_number;
                        IR_COL_NUM_R(div_idx) = call_col_number;
                        OPND_IDX(substring_len[i]) = div_idx;
                        OPND_FLD(substring_len[i]) = IR_Tbl_Idx;
                     }
                  }
                  else {
                     ATD_TYPE_IDX(tmp_attr) = CG_INTEGER_DEFAULT_TYPE;
                  }

                  COPY_COMMON_ATTR_INFO(OPND_IDX(dummy_opnd[i]), 
                                        new_darg_attr, 
                                        Data_Obj);

                  COPY_VARIANT_ATTR_INFO(OPND_IDX(dummy_opnd[i]), 
                                         new_darg_attr, 
                                         Data_Obj);

                  if ((OPND_IDX(flipped_opnd[i]) != NULL_IDX) &&
                      ATD_RESHAPE_ARRAY_OPT(OPND_IDX(flipped_opnd[i]))) {
                     flipped_bd_idx = ATD_ARRAY_IDX(OPND_IDX(flipped_opnd[i]));
                     dummy_bd_idx = ATD_ARRAY_IDX(OPND_IDX(dummy_opnd[i]));

                     if (BD_RANK(flipped_bd_idx) > BD_RANK(dummy_bd_idx)) { 
                        /*
                        Move the bounds information from the actual
                        argument to the new automatic array.
                        The automatic array must inherit the 
                        bounds information from the actual argument 
                        because all dummy argument references within 
                        the inlined code will be re-written with more
                        subscript expressions.
                        */
                        ATD_ARRAY_IDX(new_darg_attr) =
                           ATD_ARRAY_IDX(OPND_IDX(flipped_opnd[i]));
                        ATD_RESHAPE_ARRAY_IDX(new_darg_attr) =
                           ATD_RESHAPE_ARRAY_IDX(OPND_IDX(flipped_opnd[i]));

                        ATD_RESHAPE_ARRAY_OPT(new_darg_attr) = TRUE;

                     }
                     else {
                        /*
                        Move the bounds information from the dummy
                        argument to the new automatic array.
                        */
                        ATD_RESHAPE_ARRAY_IDX(new_darg_attr) =
                           ATD_RESHAPE_ARRAY_IDX(OPND_IDX(dummy_opnd[i]));
                     }

                     if (ATD_RESHAPE_ARRAY_OPT(new_darg_attr)) {
                        /*
                        Attach an AL entry at the head of the list for
                        the current scope.   The attribute being attached
                        is the new automatic array.
                        */
                        NTR_ATTR_LIST_TBL(tmp_al_idx);
                        AL_ATTR_IDX(tmp_al_idx) = new_darg_attr;
                        al_idx = SCP_RESHAPE_ARRAY_LIST(curr_scp_idx);
                        SCP_RESHAPE_ARRAY_LIST(curr_scp_idx) = tmp_al_idx;
                        AL_NEXT_IDX(tmp_al_idx) = al_idx;
                     }
                  }


                  loc_fld = IR_Tbl_Idx;
                  NTR_IR_TBL(loc_idx);
                  IR_OPR(loc_idx) = Aloc_Opr;
                  IR_TYPE_IDX(loc_idx) = ATD_TYPE_IDX(tmp_attr);
                  IR_LINE_NUM(loc_idx) = call_line_number;
                  IR_COL_NUM(loc_idx) = call_col_number;

                  if (ATD_TYPE_IDX(tmp_attr) == CRI_Ch_Ptr_8) {
                     IR_IDX_L(loc_idx) = OPND_IDX(substringing_tree[i]);
                     IR_FLD_L(loc_idx) = OPND_FLD(substringing_tree[i]);

                     /*
                     Clear the substring offsets here because they have
                     already been absorbed into the pointer of the 
                     based array.
                     */
                     for (k = 0; k <= MAX_INLINE_ARGS-1; k++) {
                        OPND_IDX(substring_offset[k]) = CN_INTEGER_ZERO_IDX;
                        OPND_FLD(substring_offset[k]) = CN_Tbl_Idx;
                     }

                     if (TYP_CHAR_CLASS(ATD_TYPE_IDX(new_darg_attr)) ==
                                                       Assumed_Size_Char) {
                        CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
                        TYP_TYPE(TYP_WORK_IDX) = Character;
                        TYP_LINEAR(TYP_WORK_IDX) = CHARACTER_DEFAULT_TYPE;
                        TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
                        TYP_RESOLVED(TYP_WORK_IDX) = TRUE;
                        type_idx = ntr_type_tbl();
                        ATD_TYPE_IDX(new_darg_attr) = type_idx;

                        TYP_FLD(ATD_TYPE_IDX(new_darg_attr)) = 
                           TYP_FLD(ATD_TYPE_IDX(OPND_IDX(actual_opnd[i]))); 
                        TYP_IDX(ATD_TYPE_IDX(new_darg_attr)) = 
                           TYP_IDX(ATD_TYPE_IDX(OPND_IDX(actual_opnd[i]))); 
                        TYP_CHAR_CLASS(ATD_TYPE_IDX(new_darg_attr)) =
                        TYP_CHAR_CLASS(ATD_TYPE_IDX(OPND_IDX(actual_opnd[i]))); 
                     }
                  }
                  else {
                     IR_IDX_L(loc_idx) = OPND_IDX(subscripting_tree[i]);
                     IR_FLD_L(loc_idx) = OPND_FLD(subscripting_tree[i]);
                  }

                  IR_LINE_NUM_L(loc_idx) = call_line_number;
                  IR_COL_NUM_L(loc_idx) = call_col_number;

                  if (IR_OPR(OPND_IDX(subscripting_tree[i])) == 
                                                        Dv_Access_Base_Addr ||
                      call_by_value) {
                     loc_idx = OPND_IDX(subscripting_tree[i]);
                     loc_fld = OPND_FLD(subscripting_tree[i]);
                  }

                  AT_ATTR_LINK(new_darg_attr) = NULL_IDX;
                  AT_COMPILER_GEND(new_darg_attr) = TRUE;
                  AT_DEFINED(new_darg_attr) = TRUE;
                  AT_IS_DARG(new_darg_attr) = FALSE;
                  ATD_CLASS(new_darg_attr) = Variable;
                  ATD_AUTOMATIC(new_darg_attr) = TRUE;
                  ATD_AUTO_BASE_IDX(new_darg_attr) = tmp_attr;
                  CREATE_ID(stor_name, sb_name[Based_Blk], sb_len[Based_Blk]);
                  based_blk = ntr_stor_blk_tbl(stor_name.string, 
                                               sb_len[Based_Blk],
                                               call_line_number,
                                               call_col_number,
                                               Based);
                  ATD_STOR_BLK_IDX(new_darg_attr) = based_blk;
                  if (name_substitution) {
                     /* intentionally blank */
                  }
                  else {
                     OPND_IDX(actual_opnd[i]) = new_darg_attr;
                     OPND_FLD(actual_opnd[i]) = AT_Tbl_Idx;
                  }

                  if (inlinable && !name_substitution) {
                     NTR_IR_TBL(asg_idx);
                     IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(tmp_attr);
                     IR_OPR(asg_idx) = Asg_Opr;
                     IR_LINE_NUM(asg_idx) = call_line_number;
                     IR_COL_NUM(asg_idx) = call_col_number;
                     IR_FLD_L(asg_idx) = AT_Tbl_Idx;
                     IR_IDX_L(asg_idx) = tmp_attr;
                     IR_LINE_NUM_L(asg_idx) = call_line_number;
                     IR_COL_NUM_L(asg_idx) = call_col_number;
                     IR_FLD_R(asg_idx) = loc_fld;
                     IR_IDX_R(asg_idx) = loc_idx;
                     IR_LINE_NUM_R(asg_idx) = call_line_number;
                     IR_COL_NUM_R(asg_idx) = call_col_number;

                     curr_stmt_sh_idx = copy_in_sh;
                     gen_sh(Before,
                            Assignment_Stmt,
                            call_line_number,
                            call_col_number,
                            FALSE,
                            FALSE,
                            TRUE);
                     SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
                     SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
                  }
               }

               if (inlinable &&
                   dummy_modified &&
                   copy_out_array_element != NULL_IDX) {
                  NTR_IR_TBL(asg_idx);
                  IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(copy_out_array_element);
                  IR_OPR(asg_idx) = Asg_Opr;
                  IR_LINE_NUM(asg_idx) = call_line_number;
                  IR_COL_NUM(asg_idx) = call_col_number;
                  IR_FLD_L(asg_idx) = OPND_FLD(subscripting_tree[i]);
                  copy_subtree(&(subscripting_tree[i]), &opnd);
                  COPY_OPND(IR_OPND_L(asg_idx), opnd);
                  new_idx = OPND_IDX(opnd);
                  l = 1;
                  list_idx = IR_IDX_R(new_idx);
                  while (OPND_IDX(subscript_attr[l]) != NULL_IDX) {
                     COPY_OPND(IL_OPND(list_idx), subscript_attr[l]);
                     l = l + 1;
                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                  }

                  IR_FLD_R(asg_idx) = AT_Tbl_Idx;
                  IR_IDX_R(asg_idx) = copy_out_array_element;
                  IR_LINE_NUM_L(asg_idx) = call_line_number;
                  IR_COL_NUM_L(asg_idx) = call_col_number;
                  IR_LINE_NUM_R(asg_idx) = call_line_number;
                  IR_COL_NUM_R(asg_idx) = call_col_number;

                  curr_stmt_sh_idx = SH_NEXT_IDX(call_sh);
                  gen_sh(Before,
                         Assignment_Stmt,
                         call_line_number,
                         call_col_number,
                         FALSE,
                         FALSE,
                         TRUE);
            
                  copy_out_sh[next_copy_out_sh_idx] = 
                                           SH_PREV_IDX(curr_stmt_sh_idx);
                  next_copy_out_sh_idx = next_copy_out_sh_idx + 1;

                  SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
                  SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
               }

               if (inlinable &&
                   dummy_modified &&
                   copy_out_DV_scalar != NULL_IDX) {
                  NTR_IR_TBL(asg_idx);
                  IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(copy_out_DV_scalar);
                  IR_OPR(asg_idx) = Asg_Opr;
                  IR_LINE_NUM(asg_idx) = call_line_number;
                  IR_COL_NUM(asg_idx) = call_col_number;
                  IR_FLD_L(asg_idx) = OPND_FLD(subscripting_tree[i]);
                  IR_IDX_L(asg_idx) = OPND_IDX(subscripting_tree[i]);
                  IR_FLD_R(asg_idx) = AT_Tbl_Idx;
                  IR_IDX_R(asg_idx) = copy_out_DV_scalar;
                  IR_LINE_NUM_L(asg_idx) = call_line_number;
                  IR_COL_NUM_L(asg_idx) = call_col_number;
                  IR_LINE_NUM_R(asg_idx) = call_line_number;
                  IR_COL_NUM_R(asg_idx) = call_col_number;

                  curr_stmt_sh_idx = SH_NEXT_IDX(call_sh);
                  gen_sh(Before,
                         Assignment_Stmt,
                         call_line_number,
                         call_col_number,
                         FALSE,
                         FALSE,
                         TRUE);

                  copy_out_sh[next_copy_out_sh_idx] = 
                                              SH_PREV_IDX(curr_stmt_sh_idx);
                  next_copy_out_sh_idx = next_copy_out_sh_idx + 1;

                  SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
                  SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
               }

               /*
               This is so that scalar optimization sees the
               potential alias.
               */
               if (inlinable && 
                   OPND_FLD(actual_opnd[i]) == AT_Tbl_Idx &&
                   AT_OBJ_CLASS(OPND_IDX(actual_opnd[i])) == Data_Obj &&
                   ATD_IM_A_DOPE(OPND_IDX(actual_opnd[i]))) {

                  tmp_sh = call_sh;
                  while (tmp_sh != NULL_IDX) {
                    if (IR_OPR(SH_IR_IDX(tmp_sh)) == Dv_Whole_Copy_Opr ||
                        IR_OPR(SH_IR_IDX(tmp_sh)) == Dv_Def_Asg_Opr) {
                       attr_idx = find_left_attr(&IR_OPND_L(SH_IR_IDX(tmp_sh)));
                       if (attr_idx == OPND_IDX(actual_opnd[i])) {

                          if (IR_OPR(SH_IR_IDX(tmp_sh)) == Dv_Def_Asg_Opr) {
                             attr_idx = 
                             find_left_attr(
                               &IL_OPND(IR_IDX_L(IR_IDX_R(SH_IR_IDX(tmp_sh)))));
                          }
                          else {
                             attr_idx = 
                               find_left_attr(&IR_OPND_R(SH_IR_IDX(tmp_sh)));
                          }

                          tmp_attr = NULL_IDX;
                          if (IR_FLD_R(SH_IR_IDX(tmp_sh)) == IR_Tbl_Idx &&
                              IR_OPR(IR_IDX_R(SH_IR_IDX(tmp_sh)))==Struct_Opr) {
                             tmp_attr = 
                               find_base_attr(&IR_OPND_R(SH_IR_IDX(tmp_sh)),
                                              &line,
                                              &col);
                          }

                          if (attr_idx != NULL_IDX &&
                              attr_idx != OPND_IDX(actual_opnd[i])) {
                             NTR_ATTR_LIST_TBL(list_idx1);
                             AL_ATTR_IDX(list_idx1) = attr_idx; 
                             ATD_DV_ALIAS(OPND_IDX(actual_opnd[i])) = list_idx1;

                             if (tmp_attr != NULL_IDX) {
                                NTR_ATTR_LIST_TBL(list_idx2);
                                AL_ATTR_IDX(list_idx2) = tmp_attr;
                                AL_NEXT_IDX(list_idx1) = list_idx2;
                             }
                          }
                          break;
                       }
                    }
                    tmp_sh = SH_PREV_IDX(tmp_sh);
                  }
               }

               if (OPND_FLD(actual_opnd[i]) == AT_Tbl_Idx &&
                   struct_base_attr_idx != NULL_IDX &&
                   TYP_TYPE(ATD_TYPE_IDX(OPND_IDX(actual_opnd[i]))) == 
                                                                   Structure) {
                  OPND_IDX(actual_opnd[i]) = struct_base_attr_idx;
                  OPND_FLD(actual_opnd[i]) = AT_Tbl_Idx;
               }

               check_actual_and_dummy(actual_opnd[i], dummy_opnd[i], i);

               actual_arg_list_idx = IL_NEXT_LIST_IDX(actual_arg_list_idx);
               i = i + 1;
               j = j + 1;
               struct_base_attr_idx = NULL_IDX;
            }

            /*
            Make a copy of the routine to be linked in place of the call.
            */
            sh_count = 0;
            processing_ENTRY_called = FALSE;
            if (inlinable) {
               entry_label_attr_idx = gen_internal_lbl(call_line_number);
               exit_label_attr_idx = gen_internal_lbl(call_line_number);
               make_copy_of_routine(ATP_FIRST_SH_IDX(pgm_attr_idx));
            }
 
            /*
            This routine had more than 350 statement headers in
            the IR which represents that routine.   This is the
            threshold used to determine that the routine contains
            too much text to be inlined.   Stop inlining.   If the
            user has specified an INLINEALWAYS directive on this 
            routine, then ignore this limit.
            */
            if (!ATP_INLINE_ALWAYS(pgm_attr_idx)) {
               if (sh_count > 350) {
                  inlinable = FALSE;
                  PRINTMSG(call_line_number,
                           1347,
                           Inline,
                           call_col_number,
                           AT_OBJ_NAME_PTR(pgm_attr_idx));
               }
            }


            /*
            This check here is a saftey valve.   Table sizes
            are checked here.   If we are approaching dangerous limits,
            we just stop inlining.  The values are arbitrary.
            */
            if (npi > (MAX_INLINED_ROUTINES - 5) || table_overflow) {
               inlinable = FALSE;
               PRINTMSG(call_line_number,
                        1202,
                        Inline,
                        call_col_number,
                        AT_OBJ_NAME_PTR(pgm_attr_idx),
                        "internal table(s) almost full");
            }


            /*
            Link the IR of the routine in place of the call.
            NOTE: There may have been reasons that a routine can
            not be inlined that were encountered while trying to 
            make the copy of that routine.  If so, inlinable will
            have been set to FALSE.        
            */
            if (inlinable) {  
               NTR_IR_TBL(asg_idx);
               IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
               IR_OPR(asg_idx) = Br_Uncond_Opr;
               IR_LINE_NUM(asg_idx) = call_line_number;
               IR_COL_NUM(asg_idx) = call_col_number;
               IR_OPND_L(asg_idx) = null_opnd;
               IR_FLD_R(asg_idx) = AT_Tbl_Idx;
               IR_IDX_R(asg_idx) = entry_label_attr_idx;
               IR_LINE_NUM_R(asg_idx) = call_line_number;
               IR_COL_NUM_R(asg_idx) = call_col_number;
               curr_stmt_sh_idx = call_sh;
               gen_sh(Before,
                      Goto_Stmt,
                      call_line_number,
                      call_col_number,
                      FALSE,
                      FALSE,
                      TRUE);
               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


               NTR_IR_TBL(asg_idx);
               IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
               IR_OPR(asg_idx) = Label_Opr;
               IR_LINE_NUM(asg_idx) = call_line_number;
               IR_COL_NUM(asg_idx) = call_col_number;
               IR_FLD_L(asg_idx) = AT_Tbl_Idx;
               IR_IDX_L(asg_idx) = exit_label_attr_idx;
               IR_OPND_R(asg_idx) = null_opnd;
               IR_LINE_NUM_L(asg_idx) = call_line_number;
               IR_COL_NUM_L(asg_idx) = call_col_number;
               curr_stmt_sh_idx = call_sh;
               gen_sh(After,
                      Continue_Stmt,
                      call_line_number,
                      call_col_number,
                      FALSE,
                      TRUE,
                      TRUE);
               SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

               curr_stmt_sh_idx = call_sh;
               insert_sh_chain_before(copy_head);

               something_was_inlined = TRUE;
               SH_IR_IDX(call_sh) = Null_Opr;

               next_pgm_idx[npi] = pgm_attr_idx;
               npi = npi + 1;

               PRINTMSG(call_line_number,
                        1204,
                        Inline,
                        call_col_number, 
                        AT_OBJ_NAME_PTR(pgm_attr_idx));
            }
            else {
               /* 
               During the creation of the copy of the routine
               something was encountered that has made it 
               impossible to inline this particular call site.
               As a result, we must clear any of the copy out
               text that was created at argument setup time.
               */
               for (i = 0; i <= MAX_INLINE_ARGS-1; i++) {
                  if (copy_out_sh[i] != NULL_IDX) {
                     SH_IR_IDX(copy_out_sh[i]) = Null_Opr;
                  }
               }
            }
         }
         break;
   
         default :
         function_call = TRUE;
         srch_for_calls(IR_IDX_L(ir_idx), IR_FLD_L(ir_idx));
         function_call = FALSE;

         function_call = TRUE;
         srch_for_calls(IR_IDX_R(ir_idx), IR_FLD_R(ir_idx));
         function_call = FALSE;
         break;
      }
      break;

      default :
      break;
   }
    
TRACE (Func_Exit, "srch_for_calls", NULL);

return;

}  /* srch_for_calls */





/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This is the main driver for inline processing.   This routine         *|
|*	traverses the statement headers for the current routine being         *|
|*	compiled.                                                             *|
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
void inline_processing(int	first_sh_idx)

{
   int           sh;
   int           i;
   int           save_curr_stmt_sh_idx;
   int           save_curr_scp_idx;
   int           ncs		 			 = 0;
   int           child_scopes[MAX_INLINED_ROUTINES];


   TRACE (Func_Entry, "inline_processing", NULL);

   for (i = 0; i <= MAX_INLINED_ROUTINES-1; i++) {
      child_scopes[i] = NULL_IDX;
   }
   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
   save_curr_scp_idx = curr_scp_idx;

PROCESS_CHILD:

PROCESS_SIBLING:

   table_overflow = FALSE;

   npi = 0;
   for (i = 0; i <= MAX_INLINED_ROUTINES-1; i++) {
      next_pgm_idx[i] = NULL_IDX; 
   }

ANOTHER_PASS:

   inline_in_effect = FALSE;
   noinline_in_effect = FALSE;
   something_was_inlined = FALSE;
   loop_nest = 0;
   parallel_region = 0;

   sh = first_sh_idx;

   while (sh != NULL_IDX) {
      if (SH_IR_IDX(sh) != NULL_IDX) {
         if (IR_OPR(SH_IR_IDX(sh)) == Loop_Info_Opr) {
            loop_nest = loop_nest + 1;
         }
         else if (SH_PARENT_BLK_IDX(sh) != NULL_IDX &&
                  SH_STMT_TYPE(sh) == Continue_Stmt &&
                  IR_OPR(SH_IR_IDX(SH_PARENT_BLK_IDX(sh))) == Loop_Info_Opr) {
            loop_nest = loop_nest - 1;
         }

         if (IR_OPR(SH_IR_IDX(sh)) == Doall_Cmic_Opr) {
            parallel_region = parallel_region + 1;
         }

         if (SH_DOALL_LOOP_END(sh)) {
            parallel_region = parallel_region - 1;
         }

         call_sh = sh;
         srch_for_calls(SH_IR_IDX(sh), IR_Tbl_Idx);
      }

      sh = SH_NEXT_IDX(sh);
   }

   if (something_was_inlined) {
      goto ANOTHER_PASS;
   }


   /*
   Check to see if there is a child scope of this current scope.
   If so, save it away to be processed later.
   */
   if (SCP_FIRST_CHILD_IDX(curr_scp_idx) != NULL_IDX) {
      ncs = ncs + 1;
      if (ncs >= MAX_INLINED_ROUTINES) {
         PRINTMSG(call_line_number,
                  1315,
                  Internal,
                  call_col_number);
      }
      child_scopes[ncs] = SCP_FIRST_CHILD_IDX(curr_scp_idx);
   }

   /*
   Process the next sibling scope of the current scope being
   processed.
   */
   if (SCP_SIBLING_IDX(curr_scp_idx) != NULL_IDX) {
      curr_scp_idx = SCP_SIBLING_IDX(curr_scp_idx);
      first_sh_idx = SCP_FIRST_SH_IDX(curr_scp_idx);
      goto PROCESS_SIBLING;
   }

   /*
   Process any child scope which was saved away.
   */
   for (i = 1; i <= MAX_INLINED_ROUTINES-1; i++) {
      if (child_scopes[i] != NULL_IDX) {
         curr_scp_idx = child_scopes[i];
         first_sh_idx = SCP_FIRST_SH_IDX(curr_scp_idx);
         child_scopes[i] = NULL_IDX;
         goto PROCESS_CHILD;
      }
   }

   curr_stmt_sh_idx = save_curr_stmt_sh_idx; 
   curr_scp_idx = save_curr_scp_idx; 
         
   TRACE (Func_Exit, "inline_processing", NULL);

   return;

}  /* inline_processing */



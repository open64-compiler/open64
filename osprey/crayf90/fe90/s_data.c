/*
 * Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
 */
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



static char USMID[] = "\n@(#)5.0_pl/sources/s_data.c	5.7	09/02/99 17:06:53\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "debug.m"
# include "s_globals.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "s_globals.h"
# include "s_data.h"


/******************************************************************\
|* Function prototypes of static functions declared in this file. *|
\******************************************************************/

static  void	adjust_char_value_len (int, int, long64, long64);
static  void    build_loop_tbl (int, boolean);
static  boolean	check_target_and_value (int, int);
static  void	data_imp_do_semantics (int, int, boolean, boolean *);
static	void	fold_all_subscripts (opnd_type *);
static	void	gen_section_ref(int,long64,int,int,int,long64 *,long64 *,
                                long64 *);
static  boolean good_data_imp_do_expr(int);
static	boolean	imp_do_metamorphed (int);
static	boolean init_whole_array(int, long64 *, int, int, boolean *);
static  void	interpret_data_imp_do(int);
static	void	object_semantics (opnd_type *, expr_mode_type, expr_arg_type *,
				  boolean, boolean);
static	boolean	optimize_whole_array_init(int);
static	void	process_data_imp_do_target(int, int, long64);
static	void	section_semantics (int, opnd_type *, int *);
static	void	set_global_value_variables (opnd_type *, opnd_type *, int);
static 	void	vv_subscript_semantics(int, int, expr_arg_type *);


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure handles initialization of a whole array, as in:        *|
|*									      *|
|*           INTEGER array(10,10)					      *|
|*           DATA array /45*0, 50*1, 5*2/				      *|
|*                                                                            *|
|*      It does this by overlaying a single dimension compiler-generated      *|
|*      array variable on the base array.  If the base array is a single      *|
|*      dimension array, no overlay is made.				      *|
|*									      *|
|*      On the first call to this procedure for each whole array	      *|
|*      initialization, the size of the array is calculated, the c-g variable *|
|*      is generated, and the IR is generated to assign the first (and        *|
|*      possibly only) set of values to the array.  If the values are split   *|
|*      up like the above example, successive calls to this procedure will    *|
|*      generate IR representing each [rep-factor*]value.      		      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      whole_sub_ir_idx : index of the Whole_Subscript IR		      *|
|*      dup_count        : number of values available to be assigned to the   *|
|*                           array on this pass through the array             *|
|*      root_ir_idx      : if the Whole_Subscript IR is not the root IR of    *|
|*                           the reference tree, root_ir_idx points to the    *|
|*                           root IR					      *|
|*      init_ir_idx      : index of the Init IR				      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*	optimized	 : TRUE if the value list was converted to a single   *|
|*                           typeless glob of bits			      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Algorithm notes:							      *|
|*      value_opnd is only used when the CRI extension is being processed     *|
|*                                                                            *|
\******************************************************************************/

static boolean init_whole_array(int		 whole_sub_ir_idx,
                                long64	        *dup_count,
			        int		 root_ir_idx,
                                int		 init_ir_idx,
				boolean		*optimized)

{
   static	int			attr_idx;
		int			bd_idx;
   static	long64			curr_subscript;
                int			curr_subscript_idx;
		int			eq_idx;
		int			eq_tmp_idx;
		boolean			first_call;
                int			il_idx;
                int			ir_idx; 
                size_offset_type	length;
		boolean			long_value;
		boolean			ok 			= TRUE;
		opnd_type		opnd;
   		int			overlay_attr_idx;
                size_offset_type	result;
   		int			var_attr_idx;
		boolean			word_size_target;

# if defined(_SEPARATE_NONCOMMON_EQUIV_GROUPS)
		int			sb_idx;
# endif
            

   TRACE(Func_Entry, "init_whole_array", NULL);

   if (obj_count == 0) {
      first_call = TRUE;

      /* If a single dimension array is being initialized, we can just use    */
      /* the array itself.  Otherwise, generate a single dimension temp array */
      /* and overlay it on the actual array because we can generate many      */
      /* fewer calls to PDGCS by using a single dimension array.  For example,*/
      /* the DATA statement    DATA ((array(i,j), i=1,10), j=1,10) /100*0/    */
      /* would generate 10 calls (one for each iteration of J) if left as a   */
      /* multidimension array because CCG only understands a single stride.   */
      /* But only a single call is generated if it is overlayed with a single */
      /* dimension array.						      */
      /* gen_compiler_tmp is used to generate an Attr with a temp name.  Since*/
      /* the base array Attr is just copied on top of it, any temp type can   */
      /* be used in the call.  gen_compiler_tmp must be used because the temp */
      /* counter used to create the name is local to it.		      */

      if (IR_FLD_L(whole_sub_ir_idx) == AT_Tbl_Idx) {
         attr_idx = IR_IDX_L(whole_sub_ir_idx);
      }
      else {
         attr_idx = IR_IDX_R(IR_IDX_L(whole_sub_ir_idx));
      }

      obj_count = CN_INT_TO_C(BD_LEN_IDX(ATD_ARRAY_IDX(attr_idx)));

      if (BD_RANK(ATD_ARRAY_IDX(attr_idx)) == 1 ) {
         curr_subscript_idx = BD_LB_IDX(ATD_ARRAY_IDX(attr_idx), 1);
         curr_subscript     = CN_INT_TO_C(curr_subscript_idx);
      }
      else { 
         AT_DEFINED(attr_idx) = TRUE;

         overlay_attr_idx = gen_compiler_tmp(IR_LINE_NUM_L(whole_sub_ir_idx), 
                                             IR_COL_NUM_L(whole_sub_ir_idx),
                                             Shared, TRUE);

         ATD_TYPE_IDX(overlay_attr_idx)         = ATD_TYPE_IDX(attr_idx);
         ATD_STOR_BLK_IDX(overlay_attr_idx)     = ATD_STOR_BLK_IDX(attr_idx);
         ATD_EQUIV(overlay_attr_idx)     	= TRUE;
         AT_REFERENCED(overlay_attr_idx) 	= Referenced;
         AT_SEMANTICS_DONE(overlay_attr_idx) 	= TRUE;
         
         if (ATD_CLASS(attr_idx) != Struct_Component) {

            ATD_OFFSET_FLD(overlay_attr_idx)      = ATD_OFFSET_FLD(attr_idx);
            ATD_OFFSET_IDX(overlay_attr_idx)      = ATD_OFFSET_IDX(attr_idx);
            ATD_OFFSET_ASSIGNED(overlay_attr_idx) = 
                                                 ATD_OFFSET_ASSIGNED(attr_idx);

            /* The overlay tmp and the variable must have the same offset.    */
            /* Find the equivalence group for the variable and add the tmp to */
            /* the equivalence group.  To do this, create a new equivalence   */
            /* table entry, add it to the group and make ATD_OFFSET be the    */
            /* same for both.  (ATD_OFFSET can be set, even if ATD_OFFSET     */
            /* ASSIGNED is FALSE because this is the equivalence group        */
            /* offset).							      */

            if (ATD_EQUIV(attr_idx)) {
               eq_idx	= SCP_FIRST_EQUIV_GRP(curr_scp_idx);

               while (eq_idx != NULL_IDX) {
                  eq_tmp_idx	= eq_idx;
                  eq_idx	= EQ_NEXT_EQUIV_GRP(eq_idx);

                  while (eq_tmp_idx != NULL_IDX) {

                     if (EQ_ATTR_IDX(eq_tmp_idx) == attr_idx) { /* Found */
                        NTR_EQ_TBL(eq_idx);
                        COPY_TBL_NTRY(equiv_tbl, eq_idx, eq_tmp_idx);
                        EQ_NEXT_EQUIV_OBJ(eq_tmp_idx)	= eq_idx;
                        EQ_ATTR_IDX(eq_idx)		= overlay_attr_idx;
                        ATD_OFFSET_FLD(overlay_attr_idx)= 
                                                       ATD_OFFSET_FLD(attr_idx);
                        ATD_OFFSET_IDX(overlay_attr_idx)= 
                                                       ATD_OFFSET_IDX(attr_idx);
                        ATD_EQUIV(attr_idx)		= TRUE;
                        goto FOUND;
                     }
                     eq_tmp_idx = EQ_NEXT_EQUIV_OBJ(eq_tmp_idx);
                  }
               }
            }

            /* It is not in an equivalence group or it is not   */
            /* equivalenced, so make its own equivalence group. */

            NTR_EQ_TBL(eq_idx);
            NTR_EQ_TBL(eq_tmp_idx);

            EQ_NEXT_EQUIV_GRP(eq_idx)	= SCP_FIRST_EQUIV_GRP(curr_scp_idx);
            SCP_FIRST_EQUIV_GRP(curr_scp_idx)	= eq_idx;
            EQ_ATTR_IDX(eq_idx)			= attr_idx;
            EQ_ATTR_IDX(eq_tmp_idx)		= overlay_attr_idx;
            EQ_NEXT_EQUIV_OBJ(eq_idx)		= eq_tmp_idx;
            ATD_EQUIV(attr_idx)			= TRUE;
            ATD_VARIABLE_TMP_IDX(attr_idx)	= overlay_attr_idx;
            ATD_FLD(attr_idx)			= AT_Tbl_Idx;

# if defined(_SEPARATE_NONCOMMON_EQUIV_GROUPS)

            sb_idx	= ATD_STOR_BLK_IDX(attr_idx);

            if (sb_idx == NULL_IDX ||
                (!SB_MODULE(sb_idx) && !SB_IS_COMMON(sb_idx))) {

               if (SB_HOSTED_STATIC(sb_idx)) {
                  sb_idx = create_equiv_stor_blk(attr_idx, SB_BLK_TYPE(sb_idx));
                  SB_HOSTED_STATIC(sb_idx)	= TRUE;
               }
               else {
                  sb_idx = create_equiv_stor_blk(attr_idx, SB_BLK_TYPE(sb_idx));
               }

               ATD_STOR_BLK_IDX(attr_idx)		= sb_idx;
               ATD_STOR_BLK_IDX(overlay_attr_idx)	= sb_idx;
            }
# endif

         }
         else {

            ATD_OFFSET_FLD(overlay_attr_idx) = ATD_OFFSET_FLD(attr_idx);
            ATD_OFFSET_IDX(overlay_attr_idx) = ATD_CPNT_OFFSET_IDX(attr_idx);
            ATD_OFFSET_ASSIGNED(overlay_attr_idx) = 
                                                 ATD_OFFSET_ASSIGNED(attr_idx);

            /* If the array being initialized is a structure component, we   */
            /* must calculate its offset within the structure here because   */
            /* the derived type definition could be shared by a number of    */
            /* structures.  (Different structures sharing it means the       */
            /* derived type could appear at different offsets within the     */
            /* different structures.)  Add its offset inside the structure   */
            /* to the variable's offset.  This is the offset for the overlay */
            /* tmp.  If this is in a common block, set ATD_OFFSET for the    */
            /* tmp to the component offset and then add it to an equivalence */
            /* group.  Equivalence processing for common blocks expects      */
            /* offsets for equivalence groups to be in ATD_OFFSET when       */
            /* ATD_OFFSET_ASSIGNED is FALSE.                                 */

            ir_idx		= IR_IDX_L(whole_sub_ir_idx);

            if (ATD_OFFSET_IDX(overlay_attr_idx) == NULL_IDX) {
               ATD_OFFSET_FLD(overlay_attr_idx)	= CN_Tbl_Idx;
               ATD_OFFSET_IDX(overlay_attr_idx)	= CN_INTEGER_ZERO_IDX;
            }

# if defined(_DEBUG)

            /* Must be a constant length. */

            if (ATD_OFFSET_FLD(overlay_attr_idx) != CN_Tbl_Idx) {
               PRINTMSG(AT_DEF_LINE(overlay_attr_idx), 1201, Internal, 
                        AT_DEF_COLUMN(overlay_attr_idx),
                        AT_OBJ_NAME_PTR(overlay_attr_idx));
            }
# endif

            result.fld = ATD_OFFSET_FLD(overlay_attr_idx);
            result.idx = ATD_OFFSET_IDX(overlay_attr_idx);

            while (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {     /* Must be Struct IR.*/
               ir_idx = IR_IDX_L(ir_idx);
               length.fld = ATD_OFFSET_FLD(IR_IDX_R(ir_idx));
               length.idx = ATD_CPNT_OFFSET_IDX(IR_IDX_R(ir_idx));

               if (!size_offset_binary_calc(&length,
                                            &result,
                                            Plus_Opr,
                                            &result)) {
                  break;
               }
            }

            if (result.fld == NO_Tbl_Idx) {
               ATD_OFFSET_FLD(overlay_attr_idx) = CN_Tbl_Idx;
               ATD_OFFSET_IDX(overlay_attr_idx) = ntr_const_tbl(result.type_idx,
                                                               FALSE,
                                                               result.constant);
            }
            else {
               ATD_OFFSET_FLD(overlay_attr_idx) = result.fld;
               ATD_OFFSET_IDX(overlay_attr_idx) = result.idx;
            }

            var_attr_idx = IR_IDX_L(ir_idx);
            ATD_OFFSET_ASSIGNED(overlay_attr_idx) = FALSE;
            ATD_DATA_INIT(overlay_attr_idx) = TRUE;

            ATD_STOR_BLK_IDX(overlay_attr_idx) = ATD_STOR_BLK_IDX(var_attr_idx);

            if (ATD_EQUIV(var_attr_idx)) {
               eq_idx = SCP_FIRST_EQUIV_GRP(curr_scp_idx);

               while (eq_idx != NULL_IDX) {
                  eq_tmp_idx = eq_idx;
                  eq_idx = EQ_NEXT_EQUIV_GRP(eq_idx);

                  while (eq_tmp_idx != NULL_IDX) {

                     if (EQ_ATTR_IDX(eq_tmp_idx) == var_attr_idx) {

                        if (ATD_OFFSET_IDX(var_attr_idx) == NULL_IDX) {
                           ATD_OFFSET_FLD(var_attr_idx)	= CN_Tbl_Idx;
                           ATD_OFFSET_IDX(var_attr_idx)	= CN_INTEGER_ZERO_IDX;
                        }
                        NTR_EQ_TBL(eq_idx);
                        COPY_TBL_NTRY(equiv_tbl, eq_idx, eq_tmp_idx);
                        EQ_NEXT_EQUIV_OBJ(eq_tmp_idx) = eq_idx;
                        EQ_ATTR_IDX(eq_idx) = overlay_attr_idx;

                        result.fld = EQ_OFFSET_FLD(eq_idx);
                        result.idx = EQ_OFFSET_IDX(eq_idx);
                        length.fld = ATD_OFFSET_FLD(overlay_attr_idx);
                        length.idx = ATD_OFFSET_IDX(overlay_attr_idx);

                        if (!size_offset_binary_calc(&result,
                                                     &length,
                                                     Plus_Opr,
                                                     &result)) {
                           break;
                        }

                        if (result.fld == NO_Tbl_Idx) {
                           EQ_OFFSET_FLD(eq_idx) = CN_Tbl_Idx;
                           EQ_OFFSET_IDX(eq_idx) = ntr_const_tbl(
                                                             result.type_idx,
                                                             FALSE,
                                                             result.constant);
                        }
                        else if (result.fld == CN_Tbl_Idx) {
                           EQ_OFFSET_FLD(eq_idx) = result.fld;
                           EQ_OFFSET_IDX(eq_idx) = result.idx;
                        }

                        result.fld = ATD_OFFSET_FLD(var_attr_idx);
                        result.idx = ATD_OFFSET_IDX(var_attr_idx);
                        
                        if (!size_offset_binary_calc(&length,
                                                     &result,
                                                     Plus_Opr,
                                                     &result)) {
                           break;
                        }

                        if (result.fld == NO_Tbl_Idx) {
                           ATD_OFFSET_FLD(overlay_attr_idx) = CN_Tbl_Idx;
                           ATD_OFFSET_IDX(overlay_attr_idx) = ntr_const_tbl(
                                                               result.type_idx,
                                                               FALSE,
                                                               result.constant);
                        }
                        else {
                           ATD_OFFSET_FLD(overlay_attr_idx) = result.fld;
                           ATD_OFFSET_IDX(overlay_attr_idx) = result.idx;
                        }

                        ATD_EQUIV(var_attr_idx)	= TRUE;
                        goto FOUND;
                     }
                     eq_tmp_idx = EQ_NEXT_EQUIV_OBJ(eq_tmp_idx);
                  }
               }
            }

            /* It is not in an equivalence group or it is not   */
            /* equivalenced, so make its own equivalence group. */

            NTR_EQ_TBL(eq_idx);
            NTR_EQ_TBL(eq_tmp_idx);

            EQ_NEXT_EQUIV_GRP(eq_idx)	= SCP_FIRST_EQUIV_GRP(curr_scp_idx);
            SCP_FIRST_EQUIV_GRP(curr_scp_idx)= eq_idx;
            EQ_OFFSET_IDX(eq_tmp_idx)	= ATD_OFFSET_IDX(overlay_attr_idx);
            EQ_OFFSET_FLD(eq_tmp_idx)	= ATD_OFFSET_FLD(overlay_attr_idx);
            EQ_ATTR_IDX(eq_idx)		= var_attr_idx;
            EQ_ATTR_IDX(eq_tmp_idx)	= overlay_attr_idx;
            EQ_NEXT_EQUIV_OBJ(eq_idx)	= eq_tmp_idx;
            ATD_EQUIV(var_attr_idx)	= TRUE;

# if defined(_SEPARATE_NONCOMMON_EQUIV_GROUPS)

            sb_idx = ATD_STOR_BLK_IDX(var_attr_idx);

            if (sb_idx == NULL_IDX ||
                (!SB_MODULE(sb_idx) && !SB_IS_COMMON(sb_idx))) {

               if (SB_HOSTED_STATIC(sb_idx)) {
                  sb_idx = create_equiv_stor_blk(attr_idx, SB_BLK_TYPE(sb_idx));
                  SB_HOSTED_STATIC(sb_idx) = TRUE;
               }
               else {
                  sb_idx = create_equiv_stor_blk(attr_idx, SB_BLK_TYPE(sb_idx));
               }

               ATD_STOR_BLK_IDX(var_attr_idx) = sb_idx;
               ATD_STOR_BLK_IDX(overlay_attr_idx) = sb_idx;
            }
# endif

            NTR_IR_LIST_TBL(il_idx);
            IL_IDX(il_idx) = overlay_attr_idx;
            IL_FLD(il_idx) = AT_Tbl_Idx;
            IL_LINE_NUM(il_idx) = stmt_start_line;
            IL_COL_NUM(il_idx) = stmt_start_col;

            if (ATD_FLD(var_attr_idx) == NO_Tbl_Idx) {
               ATD_FLD(var_attr_idx) = IL_Tbl_Idx;
               IL_LIST_CNT(il_idx) = 1;
            }
            else {
               IL_LIST_CNT(il_idx) = 1 +
                                IL_LIST_CNT(ATD_VARIABLE_TMP_IDX(var_attr_idx));
               IL_NEXT_LIST_IDX(il_idx) = ATD_VARIABLE_TMP_IDX(var_attr_idx);
            }
            ATD_VARIABLE_TMP_IDX(var_attr_idx) = il_idx;
         }

FOUND:;

         /* Create new bound entry as a one-dimension array. */

         bd_idx			= reserve_array_ntry(1);
         BD_RESOLVED(bd_idx)    = TRUE;
         BD_LEN_FLD(bd_idx)	= CN_Tbl_Idx; 
         BD_LEN_IDX(bd_idx)	= BD_LEN_IDX(ATD_ARRAY_IDX(attr_idx));
         BD_RANK(bd_idx)	= 1;
         BD_ARRAY_CLASS(bd_idx)	= Explicit_Shape;
         BD_ARRAY_SIZE(bd_idx)	= Constant_Size;
         BD_LINE_NUM(bd_idx)	= IR_LINE_NUM_L(whole_sub_ir_idx);
         BD_COLUMN_NUM(bd_idx)	= IR_COL_NUM_L(whole_sub_ir_idx);
         BD_LB_FLD(bd_idx,1)	= CN_Tbl_Idx; 
         BD_LB_IDX(bd_idx,1)	= CN_INTEGER_ONE_IDX;
         BD_UB_FLD(bd_idx,1)	= CN_Tbl_Idx; 
         BD_UB_IDX(bd_idx,1)	= BD_LEN_IDX(ATD_ARRAY_IDX(attr_idx));
         BD_XT_FLD(bd_idx,1)	= CN_Tbl_Idx; 
         BD_XT_IDX(bd_idx,1)	= BD_LEN_IDX(ATD_ARRAY_IDX(attr_idx));
         BD_SM_FLD(bd_idx,1)	= CN_Tbl_Idx; 
         BD_SM_IDX(bd_idx,1)	= BD_SM_IDX(ATD_ARRAY_IDX(attr_idx),1);

         ATD_ARRAY_IDX(overlay_attr_idx) = ntr_array_in_bd_tbl(bd_idx);

         curr_subscript = 1;
         curr_subscript_idx = CN_INTEGER_ONE_IDX;

         attr_idx = overlay_attr_idx;
      }
   }
   else {
      first_call = FALSE;
      curr_subscript    += *dup_count;
      curr_subscript_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                       curr_subscript);
   }

   word_size_target = FALSE;

   if (TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) == INTEGER_DEFAULT_TYPE  ||
       TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) == REAL_DEFAULT_TYPE) {

      if (storage_bit_size_tbl[TYP_LINEAR(ATD_TYPE_IDX(attr_idx))] ==
            TARGET_BITS_PER_WORD) {
         word_size_target = TRUE;
      }
   }

   long_value = FALSE;

   if (value_desc.type == Typeless) {

      if (TYP_BIT_LEN(CN_TYPE_IDX(OPND_IDX(value_opnd))) >
             TARGET_BITS_PER_WORD) {
         long_value = TRUE;
      }
   }
   else if (value_desc.type == Character) {

      if (CN_INT_TO_C(TYP_IDX(value_desc.type_idx)) > TARGET_CHARS_PER_WORD) {
         long_value = TRUE;
      }
   }

   if (word_size_target  &&  long_value) {
      PRINTMSG(OPND_LINE_NUM(value_opnd), 733, Error, OPND_COL_NUM(value_opnd));
   }
   else {

      /* If								      */
      /*    (1) this is the first initialization action for this array,       */
      /*    (2) it is not a structure component (this restriction can be      */
      /*        adjusted with experience), and		     		      */
      /*    (3) it is not an array of structures (that is, it is an intrinsic */
      /*        type,							      */
      /* then see if the whole array init can be turned into the assignment   */
      /* of an array constructor value to the array.  If the value list is all*/
      /* individual values, this transformation changes the internal form from*/
      /* <n> individual assignments to a single assignment of one large       */
      /* typeless blob to the array.		 			      */

      if (first_call                                      &&
           IR_FLD_L(IR_IDX_L(init_ir_idx)) == AT_Tbl_Idx  &&
           TYP_TYPE(ATD_TYPE_IDX(IR_IDX_L(IR_IDX_L(init_ir_idx)))) !=
              Structure) {

         if (ATD_CLASS(attr_idx) == Compiler_Tmp) {
            IR_IDX_L(IR_IDX_L(init_ir_idx)) = attr_idx;
         }

         if (TYP_TYPE(CN_TYPE_IDX(OPND_IDX(value_opnd))) == Character &&
             (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Integer ||
              TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Real)) {
            *optimized = FALSE;
         } 
         else {
            *optimized = optimize_whole_array_init(init_ir_idx);
         }

         if (SH_ERR_FLG(curr_stmt_sh_idx)) {
            ok = FALSE;
            goto EXIT;
         }

         if (*optimized) {
            goto EXIT;
         }
      }
      else {
         *optimized = FALSE;
      }
    }


   /* Make a copy of the reference IR tree.  Locate the Whole_Subscript IR    */
   /* and change it to a Subscript IR.  Use the IL to which the Triplet IR is */
   /* attached to indicate the element at which the initialization is to      */
   /* begin.  (The Triplet IR is abandoned.)				      */
   /* LRR:  Should we go to the work to free up the space for the Triplet IR  */
   /*       and its ILs?						      */

   gen_opnd(&opnd, root_ir_idx, IR_Tbl_Idx, stmt_start_line, stmt_start_col);
   copy_subtree(&opnd, &opnd);
   ir_idx                = OPND_IDX(opnd);
   IR_FLD_L(init_ir_idx) = IR_Tbl_Idx;
   IR_IDX_L(init_ir_idx) = ir_idx;

   while (IR_OPR(ir_idx) != Whole_Subscript_Opr) {
      ir_idx = IR_IDX_L(ir_idx);
   } 
 
   IR_OPR(ir_idx)  = Subscript_Opr;
   IR_RANK(ir_idx) = 1;


   /* If attr_idx is pointing at a compiler temp, it means a multidimension   */
   /* array has been overlayed.  If the left operand of the Subscript IR is   */
   /* an Attr index, just update the index to point at the temp's Attr.       */
   /* If the left operand is another IR, it had better be a Struct IR.  It    */
   /* means the temp is overlaying a multidimensional structure component, so */
   /* replace the whole reference to the component with a reference to the    */
   /* temp's Attr.							      */

   if (ATD_CLASS(attr_idx) == Compiler_Tmp) {

      if (IR_FLD_L(ir_idx) == AT_Tbl_Idx) {
         IR_IDX_L(ir_idx) = attr_idx;
      }
      else {					/* Had better be a Struct IR. */
         IR_FLD_L(ir_idx) = AT_Tbl_Idx;    
         IR_IDX_L(ir_idx) = attr_idx;
         IR_LINE_NUM_L(ir_idx) = stmt_start_line;
         IR_COL_NUM_L(ir_idx)  = stmt_start_col;
      }
   }

   il_idx                   = IR_IDX_R(ir_idx);
   IL_NEXT_LIST_IDX(il_idx) = NULL_IDX;
   IR_LIST_CNT_R(ir_idx)    = 1;

   IL_FLD(il_idx) = CN_Tbl_Idx;
   IL_IDX(il_idx) = curr_subscript_idx;
   IL_LINE_NUM(il_idx) = stmt_start_line;
   IL_COL_NUM(il_idx)  = stmt_start_col;
   

EXIT:

   TRACE(Func_Exit, "init_whole_array", NULL);

   return(ok);

}  /* init_whole_array */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Perform semantic checks for the DATA statement and generate IR for    *|
|*      the PDGCS interface.						      *|
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

void data_stmt_semantics(void)
{
#ifdef KEY /* Bug 10177 */
   int			array_ir_idx = 0;
#else /* KEY Bug 10177 */
   int			array_ir_idx;
#endif /* KEY Bug 10177 */
   int			attr_idx;
   int			column;
#ifdef KEY /* Bug 10177 */
   boolean		compiler_gen_imp_do = FALSE;
#else /* KEY Bug 10177 */
   boolean		compiler_gen_imp_do;
#endif /* KEY Bug 10177 */
   int			const_il_idx;
   int			dim_item_idx;
   int    		dup_cnt_il_idx;
   opnd_type		dup_cnt_opnd;
   long64		dup_count;
   boolean		dup_count_calculated	= FALSE;
   boolean		first_obj		= TRUE;
   int			first_triplet_idx;
   int			i;
   int			il_idx;
   int			init_ir_idx;
   int			ir_idx;
   size_offset_type	length;
   int			line;
   boolean		metamorphed;
   expr_arg_type	obj_desc;
   opnd_type		obj_opnd;
   boolean		optimized;
   opnd_type		rep_factor_opnd;
#ifdef KEY /* Bug 10177 */
   int			root_ir_idx = 0;
#else /* KEY Bug 10177 */
   int			root_ir_idx;
#endif /* KEY Bug 10177 */
   long64		section_inc_value;
   long64		section_start_value	= 0;
   int			stride_il_idx;
   size_offset_type	stride_in_bits;
   opnd_type 		stride_opnd;
#ifdef KEY /* Bug 10177 */
   int			struct_ir_idx = 0;
   /* int		substring_ir_idx; */
   int 			target_attr_idx = 0;
   boolean		vv_sub_present = FALSE;
#else /* KEY Bug 10177 */
   int			struct_ir_idx;
   /* int		substring_ir_idx; */
   int 			target_attr_idx;
   boolean		vv_sub_present;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "data_stmt_semantics", NULL);

   OPND_IDX(rep_factor_opnd) = NULL_IDX;
   init_ir_idx               = SH_IR_IDX(curr_stmt_sh_idx);
   obj_il_idx                = IR_IDX_L(init_ir_idx);
   value_il_idx              = IR_IDX_R(init_ir_idx);
   metamorphed		     = FALSE;
   obj_count                 = 0;
   rep_factor                = 0;

   while (obj_il_idx != NULL_IDX) {

      if (first_obj) {
         first_obj = FALSE;
      }
      else {
         gen_sh(After, Data_Stmt, IL_LINE_NUM(obj_il_idx), 
                IL_COL_NUM(obj_il_idx), FALSE, FALSE, TRUE);
         
         NTR_IR_TBL(init_ir_idx);
         SH_IR_IDX(curr_stmt_sh_idx) = init_ir_idx;
         IR_OPR(init_ir_idx)         = Init_Opr;
         IR_TYPE_IDX(init_ir_idx)    = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(init_ir_idx)    = IL_LINE_NUM(obj_il_idx);
         IR_COL_NUM(init_ir_idx)     = IL_COL_NUM(obj_il_idx);
      }

RESTART:

      if (obj_count == 0) {
         stride_opnd           = null_opnd;
         array_ir_idx          = NULL_IDX;
         struct_ir_idx         = NULL_IDX;
         /* substring_ir_idx   = NULL_IDX; */
         target_attr_idx       = NULL_IDX;
         obj_desc.rank         = 0;
         compiler_gen_imp_do   = FALSE;
         vv_sub_present        = FALSE;

         COPY_OPND(obj_opnd, IL_OPND(obj_il_idx));

         if (OPND_FLD(obj_opnd) == AT_Tbl_Idx  ||
             (OPND_FLD(obj_opnd) == IR_Tbl_Idx  &&  
              IR_OPR(OPND_IDX(obj_opnd)) != Implied_Do_Opr)) {
            object_semantics(&obj_opnd,
                              Data_Stmt_Target,
                             &obj_desc,
                              TRUE,
                              metamorphed);

            if (OPND_FLD(obj_opnd) == AT_Tbl_Idx) {
               root_ir_idx = NULL_IDX;
            }
            else {
               root_ir_idx = OPND_IDX(obj_opnd);

/*
               if (IR_OPR(root_ir_idx) == Whole_Substring_Opr  ||
                   IR_OPR(root_ir_idx) == Substring_Opr) {
                  substring_ir_idx = root_ir_idx;
               }
*/

               /* Get to the Attr for the item actually being initialized.    */

               ir_idx = OPND_IDX(obj_opnd);

               while (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {

                  if (IR_OPR(ir_idx) == Struct_Opr) {
                     break;
                  }
                  else {
                    ir_idx = IR_IDX_L(ir_idx);
                  }
               }

               target_attr_idx = (IR_OPR(ir_idx) == Struct_Opr) ?
                                  IR_IDX_R(ir_idx) : IR_IDX_L(ir_idx);
            }

            if (! SH_ERR_FLG(curr_stmt_sh_idx)) {
               COPY_OPND(IR_OPND_L(init_ir_idx), obj_opnd);
            }
            else {
               goto EXIT;
            }
         }
      }

      /* We have to keep processing the DATA stmt even if there are no values */
      /* left (value_il_idx is NULL_IDX) to make sure that all remaining      */
      /* targets (including implied-DOs) are zero-sized (because zero-sized   */
      /* don't contribute any variables to the target list).                  */
      /* If rep_factor is not 0, it means part of the value was not used up   */
      /* a previous target.						      */

      if (rep_factor == 0) {
         set_global_value_variables(&rep_factor_opnd, 
                                    &dup_cnt_opnd,   
                                    target_attr_idx);

         if (SH_ERR_FLG(curr_stmt_sh_idx)) {
            goto EXIT;
         }
      }

      /* If the target item is a whole array or section, the "array" IR is    */
      /* the one that is of interest.  Otherwise, get down through possible   */
      /* Whole_Substring, Substring, or Struct IR to the Attr for the scalar  */
      /* item actually being initialized.  If the target is character, we'll  */
      /* deal with blank padding after the target is initially processed.     */
      
PROCESS_THE_TARGET:

      /* NULL initializations are thrown out after semantics are done.  We    */
      /* initialize pointers to NULL by default, so we do not have to gen     */
      /* code to do it.  Just make sure the object is a pointer.              */

      if (obj_desc.rank > 0 && !obj_desc.pointer) {

         /* Find the IR (either Whole_Subscript or Section_Subscript) that    */
         /* produces the nonzero rank.					      */

         if (array_ir_idx == NULL_IDX) {
            array_ir_idx = OPND_IDX(obj_opnd);

            while (IR_OPR(array_ir_idx) != Whole_Subscript_Opr  &&
                   IR_OPR(array_ir_idx) != Section_Subscript_Opr) {

/*
               if (IR_OPR(array_ir_idx) == Substring_Opr ||
                   IR_OPR(array_ir_idx) == Whole_Substring_Opr) {
                  substring_ir_idx = array_ir_idx;
               }
*/

               if (IR_OPR(array_ir_idx) == Struct_Opr) {
                  struct_ir_idx = array_ir_idx;
               }

               array_ir_idx = IR_IDX_L(array_ir_idx);
            }
         }

         if (IR_OPR(array_ir_idx) == Whole_Subscript_Opr) {

            /* A zero-sized array contributes no variables to the list so     */
            /* just iterate to the next object if the array is zero-sized.    */

            if (IR_FLD_L(array_ir_idx) == AT_Tbl_Idx) {
               dim_item_idx = IR_IDX_L(array_ir_idx);
            }
            else {

               /* If there was no Struct IR ahead of the Whole_Subscript IR,  */
               /* it means we're processing a component that is an array.     */
               /* The Whole_Subscript IR must be pointing at a Struct IR.     */
               /* If there WAS a Struct IR ahead of the Whole_Subscript IR,   */
               /* it means we're processing a component out of each element   */
               /* of an array of structures.				      */

               if (struct_ir_idx == NULL_IDX) {
                  dim_item_idx = IR_IDX_R(IR_IDX_L(array_ir_idx));
               }
               else {
                  ir_idx = IR_IDX_L(array_ir_idx);

                  while (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {
                     ir_idx = IR_IDX_L(ir_idx);
                  }

                  dim_item_idx = IR_IDX_L(ir_idx);
               }

            }

            if (compare_cn_and_value(BD_LEN_IDX(ATD_ARRAY_IDX(dim_item_idx)),
                                     0, Eq_Opr)) {
               SH_IR_IDX(curr_stmt_sh_idx) = NULL_IDX;
               obj_il_idx = IL_NEXT_LIST_IDX(obj_il_idx);
               continue; 	/* the big obj_il_idx loop */
            }

            if (struct_ir_idx == NULL_IDX  ||
                (struct_ir_idx != NULL_IDX  &&  obj_desc.rank == 1)) {

               if (init_whole_array(array_ir_idx,
                                    &dup_count,
                                    root_ir_idx,
                                    init_ir_idx,
				    &optimized)) {
                  
                  if (optimized) {
                     obj_il_idx = IL_NEXT_LIST_IDX(obj_il_idx);
                     continue;  /* the big obj_il_idx loop */
                  }
               }
               else {
                  goto EXIT;
               }
            }
            else {
               IR_OPR(array_ir_idx) = Section_Subscript_Opr;
               goto PROCESS_THE_TARGET;
            }
         } 
         else {

            /* Process the Section_Subscript IR. */

            if (obj_count == 0) {
               il_idx = IR_IDX_R(array_ir_idx);

               for (i = 1;  i <= IR_LIST_CNT_R(array_ir_idx);  ++i) {
       
                  if (IL_VECTOR_SUBSCRIPT(il_idx)) {
                     vv_sub_present = TRUE;
                     break;
                  }
        
                  il_idx = IL_NEXT_LIST_IDX(il_idx);
               }

               /* If there is at least one vector valued subscript present,   */
               /* just turn the whole reference into a (perhaps nested) set   */
               /* of implied-DOs.  Then call data_imp_do_semantics to verify  */
               /* that all the values are assignment compatible with the      */
               /* target.						      */

               if (vv_sub_present) {
                  vv_subscript_semantics(init_ir_idx,
                             		 array_ir_idx,
             				 &obj_desc);

                  data_imp_do_semantics(init_ir_idx,
                                        IR_IDX_L(init_ir_idx),
                                        TRUE,
                                        &metamorphed);

                  obj_il_idx = IL_NEXT_LIST_IDX(obj_il_idx);
                  continue; 	/* the big obj_il_idx loop */
               }
               else {
                  section_semantics(array_ir_idx,
    	         	            &stride_opnd,
                                    &first_triplet_idx);
               }
            }
      
            if (SH_ERR_FLG(curr_stmt_sh_idx)) {
               goto EXIT;
            }

            if (obj_count != 0) {
               gen_section_ref( array_ir_idx,
                                rep_factor,
                    	        first_triplet_idx,
       			        root_ir_idx,
          		        init_ir_idx,
			       &dup_count,
			       &section_start_value,
                               &section_inc_value);
               dup_count_calculated = TRUE;
            }  
            else {
               SH_IR_IDX(curr_stmt_sh_idx) = NULL_IDX;
               obj_il_idx = IL_NEXT_LIST_IDX(obj_il_idx);
               continue; 	/* the big obj_il_idx loop */
            }
         }
      }
      else {

         /* We are dealing with a scalar target or an implied-DO. */

         if (OPND_FLD(obj_opnd) == AT_Tbl_Idx) {
            obj_count = 1;
            target_attr_idx = OPND_IDX(obj_opnd);
         }
         else {

            /* OPND_FLD(obj_opnd) must be IR_Tbl_Idx. */
      
            if (IR_OPR(OPND_IDX(obj_opnd)) == Implied_Do_Opr) {

               data_imp_do_semantics(init_ir_idx,
                                     IL_IDX(obj_il_idx),
                                     compiler_gen_imp_do,
                                     &metamorphed);

               if (SH_ERR_FLG(curr_stmt_sh_idx)) {
                  goto EXIT;
               }

               if (metamorphed) {
                  IL_FLD(obj_il_idx) = IL_FLD(IR_IDX_L(init_ir_idx));
                  IL_IDX(obj_il_idx) = IL_IDX(IR_IDX_L(init_ir_idx));
                  goto RESTART; 
               }

               COPY_OPND(IR_OPND_L(init_ir_idx), obj_opnd);
               obj_il_idx = IL_NEXT_LIST_IDX(obj_il_idx);

               continue; 	/* the big obj_il_idx loop */
            }
            else {

               /* Here if object is a character item (due to Whole_Substring */
               /* or Substring IR), an array element, or a structure         */
               /* component.						     */

               obj_count = 1;

/* 
               if (OPND_FLD(obj_opnd) == IR_Tbl_Idx &&
                   (IR_OPR(OPND_IDX(obj_opnd)) == Substring_Opr ||
                    IR_OPR(OPND_IDX(obj_opnd)) == Whole_Substring_Opr)) {

                  substring_ir_idx = OPND_IDX(obj_opnd);
               }
*/
            }
         }
      } 

      /* Have we run out of values but not out of targets? */

      if (value_il_idx == NULL_IDX) {
         find_opnd_line_and_column(&obj_opnd, &line, &column);
         PRINTMSG(line, 667, Error, column);
         goto EXIT;
      } 
          
      /* Verify that the value is assignment compatible with the target. */
 
      if (! check_target_and_value(target_attr_idx, init_ir_idx)) {
         goto EXIT;
      }

      /* Generate the IL that holds the value. */

      NTR_IR_LIST_TBL(const_il_idx);
      IR_LIST_CNT_R(init_ir_idx) = 3;
      IR_FLD_R(init_ir_idx)      = IL_Tbl_Idx;
      IR_IDX_R(init_ir_idx)      = const_il_idx;
      COPY_OPND(IL_OPND(const_il_idx), value_opnd);

      /* Generate the IL that holds the repetition count. */

      NTR_IR_LIST_TBL(dup_cnt_il_idx);
      IL_PREV_LIST_IDX(dup_cnt_il_idx) = const_il_idx;
      IL_NEXT_LIST_IDX(const_il_idx)   = dup_cnt_il_idx;
   
      if (OPND_IDX(rep_factor_opnd) == NULL_IDX) {
         find_opnd_line_and_column(&value_opnd, &line, &column);
         IL_LINE_NUM(dup_cnt_il_idx) = line;
         IL_COL_NUM(dup_cnt_il_idx) = column;
         IL_FLD(dup_cnt_il_idx) = CN_Tbl_Idx;
         IL_IDX(dup_cnt_il_idx) = CN_INTEGER_ONE_IDX;
         dup_count = 1;
      }
      else {

         if (dup_count_calculated) {
            dup_count_calculated = FALSE;
         }
         else {
            dup_count = (obj_count <= rep_factor) ? obj_count : rep_factor;
         }

         OPND_IDX(dup_cnt_opnd) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              dup_count);
         COPY_OPND(IL_OPND(dup_cnt_il_idx), dup_cnt_opnd);
      }

      /* Generate the IR that holds the stride value.  A single item has a    */
      /* stride of 0. 							      */

      NTR_IR_LIST_TBL(stride_il_idx);
      IL_PREV_LIST_IDX(stride_il_idx) = dup_cnt_il_idx;
      IL_NEXT_LIST_IDX(dup_cnt_il_idx) = stride_il_idx;
   
      if (dup_count == 1  || 
          (OPND_IDX(stride_opnd) == NULL_IDX  &&  array_ir_idx == NULL_IDX)) {
         find_opnd_line_and_column(&obj_opnd, &line, &column);
         IL_LINE_NUM(stride_il_idx) = line;
         IL_COL_NUM(stride_il_idx) = column;
         IL_FLD(stride_il_idx) = CN_Tbl_Idx;
         IL_IDX(stride_il_idx) = CN_INTEGER_ZERO_IDX;
      }
      else {

         /* If the stride has not yet been computed, compute it in bits.      */
         /* Note:  OPND_FLD is used rather than OPND_IDX.  Section processing */
         /* sets OPND_IDX but leaves OPND_FLD as NO_Tbl_Idx to signal that    */
         /* the actual bit stride has not yet been calculated.		      */

         if (OPND_FLD(stride_opnd) == NO_Tbl_Idx) {

            if (struct_ir_idx == NULL_IDX) {
               attr_idx = (IR_FLD_L(array_ir_idx) == AT_Tbl_Idx) ?
	                  IR_IDX_L(array_ir_idx) :
			  IR_IDX_R(IR_IDX_L(array_ir_idx));
            }
            else {
               ir_idx = array_ir_idx;

               while (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {
                  ir_idx = IR_IDX_L(ir_idx);
               }

               attr_idx = IR_IDX_L(ir_idx);
            }

            stride_in_bits = stor_bit_size_of(attr_idx, FALSE, FALSE);

            if (OPND_IDX(stride_opnd) != NULL_IDX) {
               length.fld = CN_Tbl_Idx;
               length.idx = OPND_IDX(stride_opnd);

               size_offset_binary_calc(&stride_in_bits,
                                       &length,
                                       Mult_Opr,
                                       &stride_in_bits);
            }

            if (stride_in_bits.fld == NO_Tbl_Idx) {
               OPND_FLD(stride_opnd) = CN_Tbl_Idx;
               OPND_IDX(stride_opnd) = ntr_const_tbl(stride_in_bits.type_idx,
   						     FALSE,
                                                     stride_in_bits.constant);
            }
            else {
               OPND_FLD(stride_opnd) = stride_in_bits.fld;
               OPND_IDX(stride_opnd) = stride_in_bits.idx;
            }

            OPND_LINE_NUM(stride_opnd) = stmt_start_line;
            OPND_COL_NUM(stride_opnd)  = stmt_start_col;
         }

         COPY_OPND(IL_OPND(stride_il_idx), stride_opnd);
      }
   
      /* If the item is type character, see if the initialization value needs */
      /* to be blank padded.                                                  */

      if (TYP_TYPE(ATD_TYPE_IDX(target_attr_idx)) == Character) {
         adjust_char_value_len(init_ir_idx,
			       array_ir_idx,
                               section_start_value,
                               section_inc_value);
      }

      /* If we have assigned values to all the (possibly expanded) objects    */
      /* represented by the current object IL, move ahead to the next object  */
      /* IL.  Similarly, if we've used up all the values represented by the   */
      /* current value IL, move ahead to the next value IL.		      */

      if ((obj_count -= dup_count) == 0) {
         obj_il_idx = IL_NEXT_LIST_IDX(obj_il_idx);
      }

      if ((rep_factor -= dup_count) == 0) {
         value_il_idx = IL_NEXT_LIST_IDX(value_il_idx);
         /* BHJ */
         if (value_il_idx != NULL_IDX) {
            IL_PREV_LIST_IDX(value_il_idx) = NULL_IDX;
         }

         if (value_il_idx == NULL_IDX  &&  obj_count != 0) {

            /* If obj_count != 0, it means we're still working on an array    */
            /* reference of some kind.  CF77 has an outmoded feature that     */
            /* allows the number of values in the constant list to be less    */
            /* than the number of targets as long as the last target is a     */
            /* whole array reference.  Do not extend this extension by        */
            /* considering it OK if the whole array reference is not the last */
            /* thing in the target list but everything following it is zero-  */
            /* sized.							      */

            if (OPND_FLD(obj_opnd) == IR_Tbl_Idx                   &&
                IR_OPR(OPND_IDX(obj_opnd)) == Whole_Subscript_Opr  &&
                IL_NEXT_LIST_IDX(obj_il_idx) == NULL_IDX) {

               if (IR_OPR(init_ir_idx) == Init_Opr) {
                  PRINTMSG(IR_LINE_NUM_L(OPND_IDX(obj_opnd)), 698, Ansi,
                           IR_COL_NUM_L(OPND_IDX(obj_opnd)));
               }

               break;
            }
            else {
               find_opnd_line_and_column(&obj_opnd, &line, &column);
               PRINTMSG(line, 667, Error, column);
               obj_il_idx = NULL_IDX;
            }
         } 
      }

   }  /* while (obj_il_idx != NULL_IDX) */

   if (value_il_idx != NULL_IDX) {
      PRINTMSG(IL_LINE_NUM(value_il_idx), 668, Error, IL_COL_NUM(value_il_idx));
   }

EXIT:
  
   TRACE (Func_Exit, "data_stmt_semantics", NULL);

   return;

}  /* data_stmt_semantics */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure performs semantic analysis on the current object.      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      obj_opnd         : the operand representing the object		      *|
|*      target_expr_mode : the expr_semantics mode in which the DATA should   *|
|*                           be evaluated                                     *|
|*      fold_subscripts  : TRUE if subscripts are to be folded (the target is *|
|*                           NOT an implied-DO target; implied-DO targets are *|
|*                           handled by the interface)			      *|
|*      metamorphed      : TRUE if an implied-DO has been metamorphed into    *|
|*                           a whole array or section ref initialization      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      obj_opnd : the updated operand					      *|
|*      obj_desc : the expression descriptor returned by expr_semantics	      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

static void object_semantics(opnd_type		*obj_opnd,
			     expr_mode_type	 target_expr_mode,
			     expr_arg_type	*obj_desc,
			     boolean		 fold_subscripts,
			     boolean		 metamorphed)

{
   int			attr_idx;
   opnd_type		data_obj;


   TRACE (Func_Entry, "object_semantics", NULL);

   /* Get down to the Attr for the target.				      */
   /* If there's something wrong with the target, just give up.               */

   COPY_OPND(data_obj, *obj_opnd);

   while (OPND_FLD(data_obj) == IR_Tbl_Idx) {
      COPY_OPND(data_obj, IR_OPND_L(OPND_IDX(data_obj)));
   } 

   if (AT_DCL_ERR(OPND_IDX(data_obj))) {
      SH_ERR_FLG(curr_stmt_sh_idx) = TRUE; 
      goto EXIT;
   }

   /* Evaluate the target.						      */

   expr_mode      = target_expr_mode;
   obj_desc->rank = 0;

   /* Add 100 to the "modification" value to signal the item is being         */
   /* initialized.							      */

   xref_state = (metamorphed) ? CIF_No_Usage_Rec : 
                                (cif_usage_code_type)
                                     (CIF_Symbol_Modification + 100);

   if (expr_semantics(obj_opnd, obj_desc)) {

      COPY_OPND(data_obj, *obj_opnd);

      while (OPND_FLD(data_obj) == IR_Tbl_Idx) {
         COPY_OPND(data_obj, IR_OPND_L(OPND_IDX(data_obj)));
      } 

      attr_idx = OPND_IDX(data_obj);

      /* Constraint checks:	 					      */
      /* * A variable that is a member of blank common should not be 	      */
      /*   initialized.						      	      */
      /* * A variable that is a member of a named common block should only be */
      /*   initialized in a block data program unit.			      */
      /* * A variable that is a member of a task common block must not be     */
      /*   initialized.							      */
      /* * From a CF77 SPR:  If an object in a Block Data program unit is NOT */
      /*   in a common block (and is not equivalenced to an object in common) */
      /*   but IS initialized, issue a warning. 			      */
      /* * F95 -> An item must not be specified in a DATA statement if it is  */
      /*   of a default initialized type.                                     */

      if (ATD_IN_COMMON(attr_idx)) {

         if (SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx)) == Common) {

            if (! metamorphed) {

               if (SB_BLANK_COMMON(ATD_STOR_BLK_IDX(attr_idx))) {
                  PRINTMSG(OPND_LINE_NUM(data_obj), 1109, Ansi,
                           OPND_COL_NUM(data_obj));
               }
               else if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Blockdata) {

# if defined(_ALLOW_DATA_INIT_OF_COMMON)
                  PRINTMSG(OPND_LINE_NUM(data_obj), 692, Ansi,
                           OPND_COL_NUM(data_obj));
# else
                  PRINTMSG(OPND_LINE_NUM(data_obj), 1542, Warning,
                           OPND_COL_NUM(data_obj));
# endif
               }
            }
         }
         else if (SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx)) == Task_Common) {
            PRINTMSG(OPND_LINE_NUM(data_obj), 851, Error,
                     OPND_COL_NUM(data_obj));
         }
      }
      else if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Blockdata  &&
               !(ATD_EQUIV(attr_idx) &&
                 SB_IS_COMMON(ATD_STOR_BLK_IDX(attr_idx)))) {
         PRINTMSG(OPND_LINE_NUM(data_obj), 825, Warning,
                  OPND_COL_NUM(data_obj));
      }


      /* There is no way to initialize a CRI character pointer.               */

      if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ch_Ptr) {
# ifdef _EXTENDED_CRI_CHAR_POINTER
         transform_cri_ch_ptr(obj_opnd);
# else
         PRINTMSG(OPND_LINE_NUM(data_obj), 695, Error, OPND_COL_NUM(data_obj));
# endif
      } 

      if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure &&
          ATT_DEFAULT_INITIALIZED(TYP_IDX(ATD_TYPE_IDX(attr_idx)))) {
         PRINTMSG(OPND_LINE_NUM(data_obj), 1589, Error, 
                  OPND_COL_NUM(data_obj),
                  AT_OBJ_NAME_PTR(attr_idx),
                  AT_OBJ_NAME_PTR(TYP_IDX(ATD_TYPE_IDX(attr_idx))));
      }
      
      /* If the target is subscripted, fold the subscript expressions that    */
      /* may have been too complicated for expr_semantics to have folded when */
      /* the target reference was evaluated.				      */
      /* Note that object_semantics is only called for a scalar target or a   */
      /* a target of an implied-DO.  fold_all_subscripts is only called for a */
      /* scalar target.							      */

      if (OPND_FLD((*obj_opnd)) == IR_Tbl_Idx  &&  fold_subscripts) {
         fold_all_subscripts(obj_opnd);
      }
   }

EXIT:

   expr_mode = Regular_Expr;

   TRACE (Func_Exit, "object_semantics", NULL);
 
   return;

}  /* object_semantics */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure sets up rep_factor_opnd, value_desc, and advances      *|
|*      value_il_idx in some situations.                                      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      rep_factor_opnd : the opnd representing the rep factor                *|
|*      dup_cnt_opnd    : a copy of the rep factor opnd                       *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

static void set_global_value_variables(opnd_type           *rep_factor_opnd,
                            	       opnd_type           *dup_cnt_opnd,
                                       int                 target_attr_idx)
{
   expr_arg_type        expr_desc;
#ifdef KEY /* Bug 10177 */
   int                  rep_count_ir_idx = 0;
#else /* KEY Bug 10177 */
   int                  rep_count_ir_idx;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "set_global_value_variables", NULL);

   /* Loop even though the rep factor is 0 because a value with a rep factor  */
   /* of 0 contributes no values to the list.                                 */

   while (rep_factor == 0  &&  value_il_idx != NULL_IDX) {

      if (IL_FLD(value_il_idx) == IR_Tbl_Idx  &&
          IR_OPR(IL_IDX(value_il_idx)) == Rep_Count_Opr) {
         rep_count_ir_idx = IL_IDX(value_il_idx);
         COPY_OPND(*rep_factor_opnd, IR_OPND_L(rep_count_ir_idx));

# ifdef _DEBUG

         if (OPND_FLD((*rep_factor_opnd)) != CN_Tbl_Idx) {
            PRINTMSG(IR_LINE_NUM(rep_count_ir_idx), 626, Internal,
                     IR_COL_NUM(rep_count_ir_idx),
                     "CN_Tbl_Idx", "set_global_value_variables");
         }

# endif

         expr_desc.type_idx       = CN_TYPE_IDX(OPND_IDX((*rep_factor_opnd)));
         expr_desc.type           = TYP_TYPE(expr_desc.type_idx);
         expr_desc.linear_type    = TYP_LINEAR(expr_desc.type_idx);

         rep_factor = CN_INT_TO_C(OPND_IDX((*rep_factor_opnd)));

         if (rep_factor > 0) {
            COPY_OPND(*dup_cnt_opnd, *rep_factor_opnd);
            COPY_OPND(value_opnd, IR_OPND_R(rep_count_ir_idx));
         }
         else if (rep_factor == 0) {
            OPND_IDX((*rep_factor_opnd)) = NULL_IDX;

            if (IL_PREV_LIST_IDX(value_il_idx) != NULL_IDX) {
               IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(value_il_idx)) =
                  IL_NEXT_LIST_IDX(value_il_idx);
            }

            if (IL_NEXT_LIST_IDX(value_il_idx) != NULL_IDX) {
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(value_il_idx)) =
                  IL_PREV_LIST_IDX(value_il_idx);
            }

            value_il_idx = IL_NEXT_LIST_IDX(value_il_idx);
            continue;
         }
         else {  /* Must be positive or zero */
            PRINTMSG(OPND_LINE_NUM((*rep_factor_opnd)), 679, Error,
                     OPND_COL_NUM((*rep_factor_opnd)));
            goto EXIT;
         }
      }
      else {
         COPY_OPND(value_opnd, IL_OPND(value_il_idx));
         rep_factor                   = 1;
         OPND_IDX((*rep_factor_opnd)) = NULL_IDX;
         rep_count_ir_idx             = NULL_IDX;
      }

   }  /* while (rep_factor == 0  &&  value_il_idx != NULL_IDX) */

   if (value_il_idx == NULL_IDX) {
      goto EXIT;
   }


   /* value_opnd contains either the value that was in the value list or the  */
   /* value to the right of the rep factor.                                   */

   if (OPND_FLD(value_opnd) == CN_Tbl_Idx) {
      value_desc.type_idx    = CN_TYPE_IDX(OPND_IDX(value_opnd));
      value_desc.type        = TYP_TYPE(value_desc.type_idx);
      value_desc.linear_type = TYP_LINEAR(value_desc.type_idx);
   }
   else if (OPND_FLD(value_opnd) == AT_Tbl_Idx               &&
            AT_OBJ_CLASS(OPND_IDX(value_opnd)) == Data_Obj   &&
            ATD_CLASS(OPND_IDX(value_opnd)) == Compiler_Tmp  &&
            ATD_FLD(OPND_IDX(value_opnd)) == CN_Tbl_Idx)  {

      value_desc.type_idx    = ATD_TYPE_IDX(OPND_IDX(value_opnd));
      value_desc.type        = TYP_TYPE(value_desc.type_idx);
      value_desc.linear_type = TYP_LINEAR(value_desc.type_idx);

      OPND_FLD(value_opnd) = CN_Tbl_Idx;
      OPND_IDX(value_opnd) = ATD_TMP_IDX(OPND_IDX(value_opnd));

      if (rep_count_ir_idx == NULL_IDX) {
         COPY_OPND(IL_OPND(value_il_idx), value_opnd);
      }
      else {
         COPY_OPND(IR_OPND_R(rep_count_ir_idx), value_opnd);
      }
   }
   else if (OPND_FLD(value_opnd) == IR_Tbl_Idx &&
            IR_OPR(OPND_IDX(value_opnd)) == Null_Intrinsic_Opr) {
      value_desc.type_idx    = ATD_TYPE_IDX(target_attr_idx);
      value_desc.type        = TYP_TYPE(ATD_TYPE_IDX(target_attr_idx));
      value_desc.linear_type = TYP_LINEAR(ATD_TYPE_IDX(target_attr_idx));
   }

# ifdef _DEBUG

   else {
      PRINTMSG(IR_LINE_NUM(rep_count_ir_idx), 626, Internal,
               IR_COL_NUM(rep_count_ir_idx),
               "CN_Tbl_Idx or AT_Tbl_Idx", "set_global_value_variables");
   }

# endif


EXIT:

   TRACE (Func_Exit, "set_global_value_variables", NULL);

   return;

}  /* set_global_value_variables */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure sets up the loop_tbl and calculates the number of      *|
|*	targets represented by the section reference.			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      section_sub_ir_idx : the index of the Section_Subscript IR	      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      stride_opnd       : set to the stride value from the first section    *|
|*                          triplet					      *|
|*      first_triplet_idx : the index of the first loop_tbl entry that        *|
|*                          represents a triplet section subscript	      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

static void section_semantics(int	 	 section_sub_ir_idx,
			      opnd_type		*stride_opnd,
			      int		*first_triplet_idx)

{
   long64		actual_stride;	
   int			attr_idx;
   int			bd_idx;
   long64		dcl_lb;
   long64		dcl_ub;
   int			end_il_idx;
   boolean		error_found;
   expr_arg_type	expr_desc;
   opnd_type		expr_opnd;
   int			i;
   int			ignore_this_arg;
   int			ignore_this_arg_too;
   int			il_idx;
   int			last_triplet_idx	= NULL_IDX;
   long64		num_iterations;
   int			start_il_idx;
   int			stride_il_idx;


   TRACE(Func_Entry, "section_semantics", NULL);

   obj_count          = 1;
   *first_triplet_idx = NULL_IDX;

   /* Capture the subscript info in the loop_tbl.			      */

   expr_desc             = init_exp_desc;
   expr_desc.type        = Integer;
   expr_desc.type_idx    = INTEGER_DEFAULT_TYPE;
   expr_desc.linear_type = INTEGER_DEFAULT_TYPE;

   il_idx = IR_IDX_R(section_sub_ir_idx);

   for (lt_idx = 1;  lt_idx <= IR_LIST_CNT_R(section_sub_ir_idx);  ++lt_idx) {
      loop_tbl[lt_idx].sibling_idx = NULL_IDX;

      if (IL_FLD(il_idx) == CN_Tbl_Idx) {
         loop_tbl[lt_idx].curr_value = CN_INT_TO_C(IL_IDX(il_idx));
      }
      else if (IL_FLD(il_idx) == IR_Tbl_Idx) {

         /* Had better be a Triplet IR.  				      */

         if (IR_OPR(IL_IDX(il_idx)) != Triplet_Opr) {
            PRINTMSG(IL_LINE_NUM(il_idx), 704, Internal, IL_COL_NUM(il_idx));
         }

         error_found = FALSE;


         /* Get the declared lower and upper bounds for this dimension.       */
         /* (Bound info might be associated with a structure component.)      */
         /* find_base_attr does NOT change the opnd.			      */

         attr_idx = find_base_attr(&IR_OPND_L(section_sub_ir_idx),
                      		   &ignore_this_arg,
                      		   &ignore_this_arg_too);

         dcl_lb = CN_INT_TO_C(BD_LB_IDX(ATD_ARRAY_IDX(attr_idx), lt_idx));
         dcl_ub = CN_INT_TO_C(BD_UB_IDX(ATD_ARRAY_IDX(attr_idx), lt_idx));


         /* Capture the start value.					      */

         start_il_idx = IR_IDX_L(IL_IDX(il_idx));

         if (IL_FLD(start_il_idx) == CN_Tbl_Idx) {
            /* Nuttin' to do.						      */
         }
         else if (IL_FLD(start_il_idx) == IR_Tbl_Idx) {
            COPY_OPND(expr_opnd, IL_OPND(start_il_idx));

            if (fold_aggragate_expression(&expr_opnd, &expr_desc, TRUE)) {
               COPY_OPND(IL_OPND(start_il_idx), expr_opnd); 
            }
            else {
               PRINTMSG(IR_LINE_NUM(IL_IDX(start_il_idx)),
                        861,
                        Internal,
                        IR_COL_NUM(IL_IDX(start_il_idx)),
                        "section_semantics");
            }
         }
         else {
            PRINTMSG(IR_LINE_NUM(IL_IDX(start_il_idx)),
                     704,
                     Internal,
                     IR_COL_NUM(IL_IDX(start_il_idx)));
         }

         loop_tbl[lt_idx].start_value = CN_INT_TO_C(IL_IDX(start_il_idx));
         loop_tbl[lt_idx].curr_value = loop_tbl[lt_idx].start_value;


         /* Capture the end value.					      */

         end_il_idx = IL_NEXT_LIST_IDX(start_il_idx);

         if (IL_FLD(end_il_idx) == CN_Tbl_Idx) {
            /* Nuttin' to do.						      */
         }
         else if (IL_FLD(end_il_idx) == IR_Tbl_Idx) {
            COPY_OPND(expr_opnd, IL_OPND(end_il_idx));

            if (fold_aggragate_expression(&expr_opnd, &expr_desc, TRUE)) {
               COPY_OPND(IL_OPND(end_il_idx), expr_opnd); 
            }
            else {
               PRINTMSG(IR_LINE_NUM(IL_IDX(end_il_idx)),
                        861,
                        Internal,
                        IR_COL_NUM(IL_IDX(end_il_idx)),
                        "section_semantics");
            }
         }
         else {
            PRINTMSG(IR_LINE_NUM(IL_IDX(end_il_idx)),
                     704,
                     Internal,
                     IR_COL_NUM(IL_IDX(end_il_idx)));
         }

         loop_tbl[lt_idx].end_value = CN_INT_TO_C(IL_IDX(end_il_idx));


         /* Capture the stride value.					      */
              
         stride_il_idx = IL_NEXT_LIST_IDX(end_il_idx);

         if (IL_FLD(stride_il_idx) == CN_Tbl_Idx) {
            /* Nuttin' to do.						      */
         }
         else if (IL_FLD(stride_il_idx) == IR_Tbl_Idx) {
            COPY_OPND(expr_opnd, IL_OPND(stride_il_idx));

            if (fold_aggragate_expression(&expr_opnd, &expr_desc, TRUE)) {
               COPY_OPND(IL_OPND(stride_il_idx), expr_opnd); 
            }
            else {
               PRINTMSG(IR_LINE_NUM(IL_IDX(stride_il_idx)),
                        861,
                        Internal,
                        IR_COL_NUM(IL_IDX(stride_il_idx)),
                        "section_semantics");
            }
         }
         else {
            PRINTMSG(IR_LINE_NUM(IL_IDX(stride_il_idx)),
                     704,
                     Internal,
                     IR_COL_NUM(IL_IDX(stride_il_idx)));
         }

         loop_tbl[lt_idx].inc_value = CN_INT_TO_C(IL_IDX(stride_il_idx));
   

         /* Check on the values of the start, end, and stride values.	      */

         if (loop_tbl[lt_idx].inc_value > 0) {
            
            if (loop_tbl[lt_idx].start_value < dcl_lb) {

               /* The start value of the section subscript triplet is less    */
               /* than the declared lower bound.			      */

               PRINTMSG(IL_LINE_NUM(start_il_idx),
                        841,
			Error,
                        IL_COL_NUM(start_il_idx)); 
               error_found = TRUE;
            }

            if (loop_tbl[lt_idx].start_value > dcl_ub) {

               /* The start value of the section subscript triplet is greater */
               /* than the declared upper bound.			      */

               PRINTMSG(IL_LINE_NUM(start_il_idx),
                        849,
			Error,
                        IL_COL_NUM(start_il_idx)); 
               error_found = TRUE;
            }

            num_iterations =
               (loop_tbl[lt_idx].end_value - loop_tbl[lt_idx].start_value +
                loop_tbl[lt_idx].inc_value) /
               loop_tbl[lt_idx].inc_value;

            if (num_iterations > 0) {
               obj_count *= num_iterations;

               if ((loop_tbl[lt_idx].start_value +
                   (num_iterations - 1)*loop_tbl[lt_idx].inc_value) > dcl_ub) {

                  /* The section subscript triplet produces a subscript value */
                  /* greater than the declared upper bound.		      */

                  PRINTMSG(IL_LINE_NUM(start_il_idx),
                           905,
			   Error,
                           IL_COL_NUM(start_il_idx)); 
                  error_found = TRUE;
               }
            }
            else {
               obj_count = 0;
            }
         }
         else if (loop_tbl[lt_idx].inc_value < 0) {
            
            if (loop_tbl[lt_idx].start_value > dcl_ub) {

               /* The start value of the section subscript triplet is greater */
               /* than the declared upper bound.			      */

               PRINTMSG(IL_LINE_NUM(start_il_idx),
                        849,
			Error,
                        IL_COL_NUM(start_il_idx)); 
               error_found = TRUE;
            }

            if (loop_tbl[lt_idx].start_value < dcl_lb) {

               /* The start value of the section subscript triplet is less    */
               /* than the declared lower bound.			      */

               PRINTMSG(IL_LINE_NUM(start_il_idx),
                        841,
			Error,
                        IL_COL_NUM(start_il_idx)); 
               error_found = TRUE;
            }

            num_iterations =
               (loop_tbl[lt_idx].end_value - loop_tbl[lt_idx].start_value +
                loop_tbl[lt_idx].inc_value) /
               loop_tbl[lt_idx].inc_value;
  
            if (num_iterations > 0) {
               obj_count *= num_iterations;

               if ((loop_tbl[lt_idx].start_value +
                   (num_iterations - 1)*loop_tbl[lt_idx].inc_value) < dcl_lb) {

                  /* The section subscript triplet produces an end value less */
                  /* than the declared lower bound.			      */

                  PRINTMSG(IL_LINE_NUM(start_il_idx),
                           997,
			   Error,
                           IL_COL_NUM(start_il_idx)); 
                  error_found = TRUE;
               }
            }
            else { 
               obj_count = 0;
            }
         }
         else {

            /* The stride value of the section subscript must be nonzero.     */

            PRINTMSG(IL_LINE_NUM(stride_il_idx),
                     998,
		     Error,
                     IL_COL_NUM(stride_il_idx)); 
            error_found = TRUE;
         }
           
         if (! error_found) {

            /* Save information about the first triplet subscript because this*/
            /* is the only one we can use as a "loop" (the CRI back-end only  */
            /* accepts a single stride value).				      */
            /* Note:  OPND_FLD is set to NO_Tbl_Idx to indicate that the      */
            /* stride was calculated by section processing.  See the code in  */
            /* data_stmt_semantics that generates the stride IL.	      */
                   
            if (*first_triplet_idx == NULL_IDX) {
               *first_triplet_idx = lt_idx;
               COPY_OPND(*stride_opnd, IL_OPND(stride_il_idx));
               OPND_FLD((*stride_opnd)) = NO_Tbl_Idx;

               if (lt_idx != 1) {
                  actual_stride = CN_INT_TO_C(IL_IDX(stride_il_idx));
                  bd_idx = ATD_ARRAY_IDX(IR_IDX_L(section_sub_ir_idx));

                  for (i = 1;  i < lt_idx;  ++i) {
                     actual_stride *= CN_INT_TO_C(BD_XT_IDX(bd_idx, i));
                  }

                  OPND_IDX((*stride_opnd)) =C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                        actual_stride);
               }
            }
            else {
               loop_tbl[last_triplet_idx].sibling_idx = lt_idx;
            }

            last_triplet_idx = lt_idx;
         }
      }

      il_idx = IL_NEXT_LIST_IDX(il_idx);

   }  /* for loop */

   TRACE(Func_Exit, "section_semantics", NULL);

   return;

}  /* section_semantics */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure generates the Subscript IR to represent where          *|
|*	initialization is to begin for a piece of an array section.           *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      section_sub_ir_idx : The index to the original Section_Subscript IR   *|
|*                           that represents the section reference as seen in *|
|*                           the user program.				      *|
|*      value_count        : The number of values available to be used to     *|
|*                           initialize the section (or portion of it).       *|
|*      first_triplet_idx  : The index into the loop_tbl of the first section *|
|*                           subscript.					      *|
|*      root_ir_idx        : NULL_IDX if the Section_Subscript IR is the root *|
|*                           IR; otherwise, index of the root IR.	      *|
|*      init_ir_idx        : Index of the Init IR.			      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      dup_count           : The number of elements that can be initialized  *|
|*                            on this pass through the section.  The maximum  *|
|*                            number is the extent of the first section	      *|
|*                            subscript.				      *|
|*      section_start_value : If the target needs to be blank padded, an      *|
|*                            implied-DO is needed.  This value is needed as  *|
|*                            the implied-DO start value.		      *|
|*      section_inc_value   : If the target needs to be blank padded, an      *|
|*                            implied-DO is needed.  This value is needed as  *|
|*                            the implied-DO inc value.			      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

static void	gen_section_ref(int		  section_sub_ir_idx,
				long64	  	  value_count,
                                int	  	  first_triplet_idx,
			        int	  	  root_ir_idx,
				int	  	  init_ir_idx,
      	     	                long64		 *dup_count,
				long64		 *section_start_value,
				long64		 *section_inc_value) 

{
   int		i;
   int		il_idx;
   int		last_il_idx;
   long64	local_obj_count;
   int		ir_idx;
   opnd_type	opnd;
 

   TRACE(Func_Entry, "gen_section_ref", NULL);

   /* Make a copy of the reference IR tree.  Locate the Section_Subscript IR  */
   /* and change it to a Subscript IR.  Use one of the ILs attached to the    */
   /* Triplet IR to indicate the element at which the initialization is to    */
   /* begin. 								      */

   gen_opnd(&opnd, root_ir_idx, IR_Tbl_Idx, stmt_start_line, stmt_start_col);
   copy_subtree(&opnd, &opnd);
   ir_idx                = OPND_IDX(opnd);
   IR_FLD_L(init_ir_idx) = IR_Tbl_Idx;
   IR_IDX_L(init_ir_idx) = ir_idx;

   while (IR_OPR(ir_idx) != Section_Subscript_Opr) {
      ir_idx = IR_IDX_L(ir_idx);
   }

   IR_OPR(ir_idx)  = Subscript_Opr;
   IR_RANK(ir_idx) = 1;

   NTR_IR_LIST_TBL(il_idx);
   IR_FLD_R(ir_idx) = IL_Tbl_Idx;
   IR_IDX_R(ir_idx) = il_idx;
   IL_FLD(il_idx)   = CN_Tbl_Idx;
   IL_IDX(il_idx)   = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                  loop_tbl[1].curr_value);
   IL_LINE_NUM(il_idx) = stmt_start_line;
   IL_COL_NUM(il_idx)  = stmt_start_col;

   last_il_idx      = il_idx;

   for (i = 2;  i <= IR_LIST_CNT_R(section_sub_ir_idx);  ++i) {
      NTR_IR_LIST_TBL(il_idx);
      IL_NEXT_LIST_IDX(last_il_idx) = il_idx;
      IL_PREV_LIST_IDX(il_idx) = last_il_idx;
      last_il_idx              = il_idx;
      IL_FLD(il_idx)           = CN_Tbl_Idx;
      IL_IDX(il_idx)           = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                             loop_tbl[i].curr_value);
      IL_LINE_NUM(il_idx) = stmt_start_line;
      IL_COL_NUM(il_idx)  = stmt_start_col;
   }

   /* Before loop_tbl[first_triplet_idx].curr_value gets updated, capture it  */
   /* to pass it out.  It will be needed if the target needs to be blank      */
   /* padded.								      */

   *section_start_value = loop_tbl[first_triplet_idx].curr_value;
   *section_inc_value   = loop_tbl[first_triplet_idx].inc_value;

   /* Calculate the number of elements to be initialized.  The maximum that   */
   /* can be initialized at one time is the extent of the first section       */
   /* subscript.						              */

   local_obj_count = (loop_tbl[first_triplet_idx].end_value -
                      loop_tbl[first_triplet_idx].curr_value +
                      loop_tbl[first_triplet_idx].inc_value) /
                     loop_tbl[first_triplet_idx].inc_value;

   /* If the number of values available exceeds the number of elements        */
   /* available, it means we'll use up all (remaining) elements of the first  */
   /* triplet so we need to reset the triplet to its start value and increment*/
   /* triplets to the right as necessary.				      */
   /* If the number of elements exceeds the number of values, just increment  */
   /* the leftmost triplet so it's ready on the next pass through this        */
   /* procedure.							      */

   if (local_obj_count <= value_count) {
      *dup_count = local_obj_count;

      if (obj_count != local_obj_count) {
         loop_tbl[first_triplet_idx].curr_value = 
            loop_tbl[first_triplet_idx].start_value;
         lt_idx = loop_tbl[first_triplet_idx].sibling_idx;

         while (lt_idx != NULL_IDX) {
            loop_tbl[lt_idx].curr_value += loop_tbl[lt_idx].inc_value;
            
            if ((loop_tbl[lt_idx].inc_value > 0  &&
                 loop_tbl[lt_idx].curr_value <= loop_tbl[lt_idx].end_value)  ||
                (loop_tbl[lt_idx].inc_value < 0  &&
                 loop_tbl[lt_idx].curr_value >= loop_tbl[lt_idx].end_value)) {
               break;
            }

            loop_tbl[lt_idx].curr_value = loop_tbl[lt_idx].start_value;
            lt_idx                      = loop_tbl[lt_idx].sibling_idx;
         }
                
      }
   }
   else {
      *dup_count = value_count;
      loop_tbl[first_triplet_idx].curr_value +=
         value_count * loop_tbl[first_triplet_idx].inc_value;
   }
      
   TRACE(Func_Exit, "gen_section_ref", NULL);

   return;

}  /* gen_section_ref */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure generates implied-DOs to represent a section           *|
|*	initialization when at least one subscript is a vector valued	      *|
|*      subscript.							      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      init_ir_idx     : index of the Init IR 				      *|
|*      array_ir_idx	: index of the Section_Subscript IR 		      *|
|*      obj_desc	: expression descriptor for the target		      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE								      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

static void	vv_subscript_semantics(int	 	 init_ir_idx,
				       int	 	 array_ir_idx,
				       expr_arg_type	*obj_desc)
{
   int			do_var_il_idx;
   int			end_il_idx;
   int   		i;
   int			il_idx;
   int			imp_do_ir_idx;
   int			inc_il_idx;
   expr_arg_type	shape_desc;
   int			shape_idx;
   opnd_type		shape_opnd;
   int			start_il_idx;
   int			subscript_il_idx;
   int			tmp_idx;
#ifdef KEY /* Bug 10177 */
   int			triplet_ir_idx = 0;
#else /* KEY Bug 10177 */
   int			triplet_ir_idx;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "vv_subscript_semantics", NULL);

   /* Loop through the subscripts of the Section_Subscript IR to find out     */
   /* which ones represent vector valued subscripts and which ones represent  */
   /* triplet subscripts or constants (if any).				      */

   shape_idx        = -1;
   subscript_il_idx = IR_IDX_R(array_ir_idx);

   for (i = 1;  i <= IR_LIST_CNT_R(array_ir_idx);  ++i) {
      
      switch (IL_FLD(subscript_il_idx)) {

         case CN_Tbl_Idx:
            break;

         case IR_Tbl_Idx:

            /* Since the subscript is represented by IR, it must be a vector  */
            /* valued subscript (possibly an expression like (V + 1) where V  */
            /* is a vector), or it must be a Triplet IR.		      */
            /*								      */
            /* Generate an Implied_Do IR and its left operand IL.  As we move */
            /* "left" to "right" through the subscript list, we are building  */
            /* up inner to outer loops so the Implied_Do IR is always         */
            /* attached to the left operand of the Init IR.  Whatever was     */
            /* formerly attached to the left operand of the Init IR then      */
            /* becomes the object of the new implied-DO.                      */

            ++shape_idx;

            NTR_IR_TBL(imp_do_ir_idx);
            IR_OPR(imp_do_ir_idx)      = Implied_Do_Opr;
            IR_TYPE_IDX(imp_do_ir_idx) = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(imp_do_ir_idx) = IR_LINE_NUM(init_ir_idx); 
            IR_COL_NUM(imp_do_ir_idx)  = IR_COL_NUM(init_ir_idx); 

            NTR_IR_LIST_TBL(il_idx);
            IR_LIST_CNT_L(imp_do_ir_idx) = 1;
            IR_FLD_L(imp_do_ir_idx)      = IL_Tbl_Idx;
            IR_IDX_L(imp_do_ir_idx)      = il_idx;

            COPY_OPND(IL_OPND(il_idx), IR_OPND_L(init_ir_idx));

            IR_IDX_L(init_ir_idx) = imp_do_ir_idx; 
            
            /* If this is a Triplet IR, save its index.  Its index will be    */
            /* used later to get at the ILs attached to it.		      */

            if (IR_OPR(IL_IDX(subscript_il_idx)) == Triplet_Opr) {
               triplet_ir_idx = IL_IDX(subscript_il_idx);
            }
         
            /* Generate an integer temp to serve as the DO variable of the    */
            /* implied-DO.  Generate the IL for the DO variable (the temp)    */
            /* and attach the IL to the right operand of the Implied_Do IR.   */
            /* If the subscript is a vector valued subscript, go into the     */
            /* expression descriptor and generate the implied-DO's start,     */
            /* end, and inc values from the shape of the current subscript    */
            /* (start and inc are always 1).  If the subscript is a triplet,  */
            /* let the start, end, and inc values be the values that were     */
            /* attached to the Triplet IR.		      		      */

            tmp_idx = gen_compiler_tmp(IR_LINE_NUM(imp_do_ir_idx),
                                       IR_COL_NUM(imp_do_ir_idx),
                                       Priv, TRUE);
            AT_SEMANTICS_DONE(tmp_idx) = TRUE;
            ATD_TYPE_IDX(tmp_idx)      = INTEGER_DEFAULT_TYPE;
            ATD_STOR_BLK_IDX(tmp_idx)  = SCP_SB_STACK_IDX(curr_scp_idx);
            ATD_LCV_IS_CONST(tmp_idx)  = TRUE;

            /* clear the referenced field so that this tmp does */
            /* not get sent to mif. BHJ                         */

            AT_REFERENCED(tmp_idx)     = Not_Referenced;

            NTR_IR_LIST_TBL(do_var_il_idx);
            IL_LINE_NUM(do_var_il_idx)      = IR_LINE_NUM(imp_do_ir_idx);
            IL_COL_NUM(do_var_il_idx)       = IR_COL_NUM(imp_do_ir_idx);
            IL_FLD(do_var_il_idx)           = AT_Tbl_Idx;
            IL_IDX(do_var_il_idx)           = tmp_idx;

            if (IL_VECTOR_SUBSCRIPT(subscript_il_idx)) {
               NTR_IR_LIST_TBL(start_il_idx);
               IL_NEXT_LIST_IDX(do_var_il_idx) = start_il_idx;
               IL_PREV_LIST_IDX(start_il_idx)  = do_var_il_idx;
               IL_LINE_NUM(start_il_idx)       = IR_LINE_NUM(init_ir_idx);
               IL_COL_NUM(start_il_idx)        = IR_COL_NUM(init_ir_idx);
               IL_FLD(start_il_idx)            = CN_Tbl_Idx;
               IL_IDX(start_il_idx)            = CN_INTEGER_ONE_IDX;
 
               NTR_IR_LIST_TBL(end_il_idx);
               IL_NEXT_LIST_IDX(start_il_idx) = end_il_idx;
               IL_PREV_LIST_IDX(end_il_idx)   = start_il_idx;

               if (obj_desc->shape[shape_idx].fld != CN_Tbl_Idx) {
                  COPY_OPND(shape_opnd, obj_desc->shape[shape_idx]);

                  shape_desc             = init_exp_desc;
                  shape_desc.type        = Integer;
                  shape_desc.type_idx    = INTEGER_DEFAULT_TYPE;
                  shape_desc.linear_type = INTEGER_DEFAULT_TYPE;
                 
                  if (fold_aggragate_expression(&shape_opnd, 
                              		        &shape_desc,
						 TRUE)) {
                     COPY_OPND(IL_OPND(end_il_idx), shape_opnd);
                  }
                  else {
                     PRINTMSG(obj_desc->shape[shape_idx].line_num,
                              861, 
                              Internal,
                              obj_desc->shape[shape_idx].col_num,
                              "vv_subscript_semantics");
                  }
               }
               else {
                  IL_LINE_NUM(end_il_idx) = IR_LINE_NUM(init_ir_idx);
                  IL_COL_NUM(end_il_idx)  = IR_COL_NUM(init_ir_idx);
                  IL_FLD(end_il_idx)      = CN_Tbl_Idx;
                  IL_IDX(end_il_idx)      = obj_desc->shape[shape_idx].idx;
               } 
 
               NTR_IR_LIST_TBL(inc_il_idx);
               IL_NEXT_LIST_IDX(end_il_idx) = inc_il_idx;
               IL_PREV_LIST_IDX(inc_il_idx) = end_il_idx;
               IL_LINE_NUM(inc_il_idx)      = IR_LINE_NUM(init_ir_idx);
               IL_COL_NUM(inc_il_idx)       = IR_COL_NUM(init_ir_idx);
               IL_FLD(inc_il_idx)           = CN_Tbl_Idx;
               IL_IDX(inc_il_idx)           = CN_INTEGER_ONE_IDX;

               /* Generate a special 5th IL to point at the vector valued     */
               /* subscript tree so that the PDGCS interface can get each     */
               /* value in the vector.					      */

               NTR_IR_LIST_TBL(il_idx);
               IL_NEXT_LIST_IDX(inc_il_idx) = il_idx;
               IL_PREV_LIST_IDX(il_idx)     = inc_il_idx;
               COPY_OPND(IL_OPND(il_idx), IL_OPND(subscript_il_idx));

               IR_LIST_CNT_R(imp_do_ir_idx) = 5;
            }
            else {
               IL_NEXT_LIST_IDX(do_var_il_idx) = IR_IDX_L(triplet_ir_idx);
               IR_LIST_CNT_R(imp_do_ir_idx)    = 4;
            }

            IR_FLD_R(imp_do_ir_idx) = IL_Tbl_Idx;
            IR_IDX_R(imp_do_ir_idx) = do_var_il_idx;

            IL_FLD(subscript_il_idx) = AT_Tbl_Idx;
            IL_IDX(subscript_il_idx) = tmp_idx;
            IL_LINE_NUM(subscript_il_idx) = IR_LINE_NUM(init_ir_idx);
            IL_COL_NUM(subscript_il_idx)  = IR_COL_NUM(init_ir_idx);
            
            break;

         default:
            PRINTMSG(IR_LINE_NUM(init_ir_idx), 179, Internal, 
                     IR_COL_NUM(init_ir_idx), "vv_section_semantics");
      }  

      subscript_il_idx = IL_NEXT_LIST_IDX(subscript_il_idx);
   }
   
   TRACE (Func_Exit, "vv_subscript_semantics", NULL);

   return;

}  /* vv_subscript_semantics */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure handles the optimized initialization of a whole        *|
|*      array, as in:						              *|
|*                                                                            *|
|*           INTEGER array(500)                                               *|
|*           DATA array / <500 individual values> /                           *|
|*                                                                            *|
|*      It does this by overlaying a single dimension compiler-generated      *|
|*      array variable on the base array.  If the base array is a single      *|
|*      dimension array, no overlay is made.                                  *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      init_ir_idx      : index of the Init IR                               *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      optimized	 : a flag passed back to the caller indicating        *|
|*			   whether or not we were able to transform the       *|
|*			   initialization				      *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

static boolean optimize_whole_array_init(int	init_ir_idx)
{
   int			attr_idx;
   int			i;
   opnd_type            ignore_this_opnd;
   int			ir_idx;
   expr_arg_type        loc_exp_desc;
   opnd_type            opnd;
   boolean		optimized	= TRUE;
   opnd_type            rep_factor_opnd;
   boolean		save_insert_subs_ok;
   opnd_type	 	save_left_opnd;
   long64		save_rep_factor;
   opnd_type	 	save_right_opnd;
   expr_arg_type        save_value_desc;
   int			save_value_il_idx;
   opnd_type	 	save_value_opnd;


   TRACE(Func_Entry, "optimize_whole_array_init", NULL);

   if (value_il_idx == NULL_IDX) {
      optimized = FALSE;
      goto EXIT;
   }

   COPY_OPND(save_left_opnd, IR_OPND_L(init_ir_idx));
   COPY_OPND(save_right_opnd, IR_OPND_R(init_ir_idx));
   COPY_OPND(save_value_opnd, value_opnd);
   save_value_il_idx = value_il_idx;
   save_value_desc = value_desc;
   save_rep_factor = rep_factor;

   IR_LIST_CNT_R(init_ir_idx) = 0;

   if (IR_FLD_R(init_ir_idx) == NO_Tbl_Idx) {
      IR_FLD_R(init_ir_idx) = IL_Tbl_Idx;
      IR_IDX_R(init_ir_idx) = value_il_idx;
   }

   COPY_OPND(opnd, IR_OPND_L(init_ir_idx));

   while (OPND_FLD(opnd) == IR_Tbl_Idx) {

      if (IR_OPR(OPND_IDX(opnd)) == Substring_Opr ||
          IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr) {
         break;
      }

      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }

   attr_idx = IR_IDX_L(IR_IDX_L(init_ir_idx));

   for (i = 1;  i <= obj_count;  ++i) {
 
     /* Someday we have to duplicate some of the main line DATA code to eat   */
     /* up as values as we need and leave the Rep_Count IR in shape for the   */
     /* next target if there are any values left in it.			      */
     /* For now, if we see an IR index, we assume it's a Rep_Count IR, we     */
     /* restore everything to what it was when we came in, and we give up.    */
     if (IL_FLD(value_il_idx) == IR_Tbl_Idx) {
        optimized = FALSE;
        COPY_OPND(IR_OPND_L(init_ir_idx), save_left_opnd);
        COPY_OPND(IR_OPND_R(init_ir_idx), save_right_opnd);
        COPY_OPND(value_opnd, save_value_opnd);
        value_il_idx = save_value_il_idx;
        value_desc = save_value_desc;
        rep_factor = save_rep_factor;
        goto EXIT;
     }

     if (check_target_and_value(attr_idx, init_ir_idx)) {
        --rep_factor;

        if (rep_factor == 0) {
           ++IR_LIST_CNT_R(init_ir_idx);
           value_il_idx = IL_NEXT_LIST_IDX(value_il_idx);
        
           if (value_il_idx == NULL_IDX) {
              break;
           }
           else {
              set_global_value_variables(&rep_factor_opnd, 
                                         &ignore_this_opnd,
                                         attr_idx);

              if (SH_ERR_FLG(curr_stmt_sh_idx)) {
                 goto EXIT;
              }
           }
        }
     }
     else {
        goto EXIT;
     }
   }

   if (value_il_idx != NULL_IDX) {
      IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(value_il_idx)) = NULL_IDX;
      IL_PREV_LIST_IDX(value_il_idx) = NULL_IDX;
   }
 

   /* CF77 (and thus our little compiler) has an outmoded feature that allows */
   /* the number of values in the constant list to be less than the number of */
   /* target elements as long as the target array is the last item in the     */
   /* DATA target list.							      */

   if (IR_LIST_CNT_R(init_ir_idx) < obj_count) {

      if (IL_NEXT_LIST_IDX(obj_il_idx) == NULL_IDX) {
         PRINTMSG(IR_LINE_NUM_L(init_ir_idx), 698, Ansi,
                  IR_COL_NUM_L(init_ir_idx));
      }
      else {
         PRINTMSG(IR_LINE_NUM_L(init_ir_idx), 667, Error, 
                  IR_COL_NUM_L(init_ir_idx));
         optimized  = FALSE;   
         obj_il_idx = NULL_IDX;
         goto EXIT;
      }
   }


   /* Convert the value list to an array constructor.			      */
   /* target_array_idx, target_type_idx, and insert_subs_ok are global        */
   /* variables used by array constructor code.				      */

   OPND_FLD(init_target_opnd) = AT_Tbl_Idx;
   OPND_IDX(init_target_opnd) = attr_idx;
   OPND_LINE_NUM(init_target_opnd) = stmt_start_line;
   OPND_COL_NUM(init_target_opnd) = stmt_start_col;

   target_array_idx             = ATD_ARRAY_IDX(attr_idx);
   target_type_idx              = ATD_TYPE_IDX(attr_idx);
   check_type_conversion        = TRUE;
   save_insert_subs_ok		= insert_subs_ok;
   insert_subs_ok	        = FALSE;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)      = Constant_Array_Construct_Opr;
   IR_LINE_NUM(ir_idx) = stmt_start_line;
   IR_COL_NUM(ir_idx)  = stmt_start_col;
   IR_TYPE_IDX(ir_idx) = target_type_idx;

   COPY_OPND(IR_OPND_R(ir_idx), IR_OPND_R(init_ir_idx));

   OPND_IDX(opnd) = ir_idx;
   OPND_FLD(opnd) = IR_Tbl_Idx;

   loc_exp_desc              = init_exp_desc;
   loc_exp_desc.type_idx     = target_type_idx;
   loc_exp_desc.type         = TYP_TYPE(target_type_idx);
   loc_exp_desc.linear_type  = TYP_LINEAR(target_type_idx);
   loc_exp_desc.rank         = 1;

   if (IR_LIST_CNT_R(init_ir_idx) == obj_count) {
      loc_exp_desc.shape[0].fld = BD_XT_FLD(target_array_idx, 1);
      loc_exp_desc.shape[0].idx = BD_XT_IDX(target_array_idx, 1);
   }
   else {
      loc_exp_desc.shape[0].fld = CN_Tbl_Idx;
      loc_exp_desc.shape[0].idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              IR_LIST_CNT_R(init_ir_idx));
   }

   loc_exp_desc.constructor_size_level = Simple_Expr_Size;

   create_constructor_constant(&opnd, &loc_exp_desc);

   init_target_opnd = null_opnd;
   target_array_idx             = NULL_IDX;
   insert_subs_ok		= save_insert_subs_ok;

   remove_sh(curr_stmt_sh_idx);
   curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   obj_count = 0;

EXIT:

   TRACE(Func_Exit, "optimize_whole_array_init", NULL);

   return(optimized);

}  /* optimize_whole_array_init */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure drives the processing of a DATA implied-DO and nested  *|
|*      loops, if any exist.						      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      init_ir_idx  : the index of the current Init IR			      *|
|*      imp_do_idx   : the index of the current Implied_Do IR 		      *|
|*      compiler_gen_imp_do : TRUE if the compiler produced this implied-DO   *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      metamorphed : TRUE if the implied-DO underwent metamorphosis into a   *|
|*                    whole array or array section initialization.	      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

static void data_imp_do_semantics(int                 init_ir_idx,
                                  int                 imp_do_idx,
                                  boolean             compiler_gen_imp_do,
                                  boolean	     *metamorphed)

{
   int		 il_idx;
   int           local_rep_count_ir_idx;
   long64	 local_rep_factor;
   int           local_value_il_idx;
   int           rep_count_ir_idx;
   boolean	 save_runtime_bounds;


   TRACE (Func_Entry, "data_imp_do_semantics", NULL);
   save_runtime_bounds = cdir_switches.bounds;
   cdir_switches.bounds = TRUE;

   *metamorphed = FALSE;


   /* The expression descriptor for each implied-DO expression must be saved  */
   /* if the expression is truly an expression (involving IR).  There is a    */
   /* dynamic table where copies of the expression descriptors can be saved.  */
   /* The table is reset to "empty" after each statement.  Capture its base   */
   /* now.  Entries will be added to it by build_loop_tbl.		      */

   arg_info_list_base = arg_info_list_top;


   /* Build the loop_tbl entry for this loop level.                           */

   last_lt_idx     = NULL_IDX;
   curr_parent_idx = NULL_IDX;

   build_loop_tbl(imp_do_idx, compiler_gen_imp_do);

   if (SH_ERR_FLG(curr_stmt_sh_idx)) {
      goto EXIT;
   }


   /* See if the implied-DO can be metamorphed into a much more efficient     */
   /* whole array or array section initialization (it must be the only or     */
   /* last item in the original DATA statement; see imp_do_metamorphed for    */
   /* further explanation).						      */

   if (IL_NEXT_LIST_IDX(obj_il_idx) == NULL_IDX) {

      if (imp_do_metamorphed(init_ir_idx)) {
         *metamorphed = TRUE;
         goto EXIT;
      }
   }

      
   /* First, if the current value is a rep-factor/value pair and a            */
   /* previous target has used some of the values, make sure that the current */
   /* value of *rep_factor is represented by the Rep_Count IR because the     */
   /* implied-DO could use the Rep_Count IR directly.                         */

   if (IL_FLD(value_il_idx) == IR_Tbl_Idx  &&
       rep_factor != CN_INT_TO_C(IR_IDX_L(IL_IDX(value_il_idx)))) {

      IR_IDX_L(IL_IDX(value_il_idx)) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   rep_factor);
   }

   /* Now match the targets against the values.  If the implied-DO is part of */
   /* a DATA statement that contains other targets, split out the values that */
   /* will be assigned to the implied-DO target(s).                           */
   /* The rep factor and value for the first value have already been          */
   /* processed by data_stmt_semantics so they are ready for use.             */

   IR_LIST_CNT_R(init_ir_idx) = 1;
   IR_FLD_R(init_ir_idx) = IL_Tbl_Idx;
   IR_IDX_R(init_ir_idx) = value_il_idx;

   lt_idx = 1;

   interpret_data_imp_do(init_ir_idx);

   if (SH_ERR_FLG(curr_stmt_sh_idx)) {
      goto EXIT;
   }

   if (rep_factor == 0) {

      /* BHJ, LRR what is this ? */

      if (value_il_idx != NULL_IDX) {
         IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(value_il_idx)) = NULL_IDX;
      }
   }
   else if (IL_FLD(value_il_idx) == IR_Tbl_Idx) {

      /* A single value has a rep_factor of 1.  The above test is asking if   */
      /* there was originally a user-specified rep factor.		      */
      /* Since the rep_factor has not been counted down to 0, there are some  */
      /* values left over.  If the number of values used is greater than 1,   */
      /* make of copy of the original Rep_Count IR and attach it to the       */
      /* implied-DO's value chain to represent the number of values used.     */
      /* Otherwise, if the number of values used is 1, just copy the value    */
      /* from the original value IL.					      */

      rep_count_ir_idx = IL_IDX(value_il_idx);

      local_rep_factor = CN_INT_TO_C(IR_IDX_L(rep_count_ir_idx)) - rep_factor; 

      local_value_il_idx = IL_PREV_LIST_IDX(value_il_idx);

      NTR_IR_LIST_TBL(il_idx);

      if (IR_LIST_CNT_R(init_ir_idx) == 1) {
         IR_IDX_R(init_ir_idx) = il_idx;
      }
      else {
         IL_NEXT_LIST_IDX(local_value_il_idx) = il_idx;
         IL_PREV_LIST_IDX(il_idx) = local_value_il_idx;
      }

      local_value_il_idx = il_idx;

      if (local_rep_factor > 1) {
         NTR_IR_TBL(local_rep_count_ir_idx);
         IR_TYPE_IDX(local_rep_count_ir_idx) = TYPELESS_DEFAULT_TYPE;
         IL_FLD(local_value_il_idx) = IR_Tbl_Idx;
         IL_IDX(local_value_il_idx) = local_rep_count_ir_idx;
         COPY_TBL_NTRY(ir_tbl, local_rep_count_ir_idx, rep_count_ir_idx);

         IR_IDX_L(local_rep_count_ir_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                        local_rep_factor);
         COPY_OPND(IR_OPND_R(local_rep_count_ir_idx), value_opnd);
      }
      else {
         COPY_OPND(IL_OPND(local_value_il_idx), value_opnd);
      }

      /* If rep_factor is now 1, make the original value IL point directly at */
      /* the value (abandon the original Rep_Count IR).  Otherwise, replace   */
      /* the left operand (rep factor) of the original Rep_Count IR with the  */
      /* new rep_factor value.						      */

      if (rep_factor == 1) {
         COPY_OPND(IL_OPND(value_il_idx), IR_OPND_R(rep_count_ir_idx));
      }
      else {
         IR_IDX_L(rep_count_ir_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                  rep_factor);
      }
   }

EXIT:

   cdir_switches.bounds = save_runtime_bounds;

   TRACE (Func_Exit, "data_imp_do_semantics", NULL);

   return;

}  /* data_imp_do_semantics */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure fills in the loop_tbl that represents the implied-DO   *|
|*      loops.								      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      imp_do_idx          : the index of the current Implied_Do IR.	      *|
|*      compiler_gen_imp_do : TRUE if the compiler generated the implied-DO   *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Algorithm note:							      *|
|*      This procedure is recursive.					      *|
|*                                                                            *|
\******************************************************************************/

static void build_loop_tbl(int 	 	imp_do_idx,
			   boolean	compiler_gen_imp_do)

{
   int			attr_idx;
   int			column;
   int			do_var_tmp_idx;
   expr_arg_type	expr_desc;
   int			il_idx;
   int			lcv_col;
   int			lcv_line;
   int			line;
   opnd_type		opnd;
#ifdef KEY /* Bug 10177 */
   boolean		save_in_implied_do = FALSE;
   boolean		save_imp_do_lcv = FALSE;
   int			search_idx;
   boolean		semantics_ok = FALSE;
#else /* KEY Bug 10177 */
   boolean		save_in_implied_do;
   boolean		save_imp_do_lcv;
   int			search_idx;
   boolean		semantics_ok;
#endif /* KEY Bug 10177 */
   int			target_idx;
   int			temp_ir_idx;


   TRACE (Func_Entry, "build_loop_tbl", NULL);
   
   if (++last_lt_idx > LOOP_TBL_SIZE) {
      PRINTMSG(IR_LINE_NUM(imp_do_idx), 237, Internal, IR_COL_NUM(imp_do_idx),
               "DATA implied-DO loop_tbl");
   }

   lt_idx = last_lt_idx;

   /* Initialize fields of the current table entry.  If we're at the outermost*/
   /* loop, indicate there is no parent.  Otherwise, "link" the current       */
   /* table entry to its parent (and sibling, if it exists).  		      */
   /* NOTE:  curr_value is used by this procedure to locate the last sibling  */
   /*        in a sibling chain so we don't have to search to the end to      */
   /*        attach a subsequent sibling.				      */
   /*
  -------------------------------------------------------------------------
  | lcv_idx                  |        | target_list             | num_   8|
  |                       24 |      8 |                      24 | targets |
  |-----------------------------------------------------------------------|
  | start_idx                | start_ | start_expr_desc_idx     | parent_ |
  |                       24 | fld  8 |                         | idx    8|
  |-----------------------------------------------------------------------|
  | end_idx                  | end_   | end_expr_desc_idx       |sibling_ |
  |                       24 | fld  8 |                         |idx     8|
  |-----------------------------------------------------------------------|
  | inc_idx                  | inc_   | inc_expr_desc_idx       |offspring|
  |                       24 | fld  8 |                         |_idx    8|
  |-----------------------------------------------------------------------|
  |                                start_value                            |
  |-----------------------------------------------------------------------|
  |                                 end_value                             |
  |-----------------------------------------------------------------------|
  |                                 inc_value                             |
  |-----------------------------------------------------------------------|
  |                                 curr_value                            |
  -------------------------------------------------------------------------

*/

   loop_tbl[lt_idx].num_targets   = 0;
   loop_tbl[lt_idx].sibling_idx   = NULL_IDX;
   loop_tbl[lt_idx].offspring_idx = NULL_IDX;
   loop_tbl[lt_idx].target_list   = NULL_IDX;
   loop_tbl[lt_idx].curr_value    = NULL_IDX;
  
   if (curr_parent_idx == NULL_IDX) {
      loop_tbl[lt_idx].parent_idx = NULL_IDX;
   }
   else {
      loop_tbl[lt_idx].parent_idx = curr_parent_idx;
       
      if (loop_tbl[curr_parent_idx].offspring_idx == NULL_IDX) {
         loop_tbl[curr_parent_idx].offspring_idx = lt_idx;
      }
      else {
         loop_tbl[loop_tbl[curr_parent_idx].curr_value].sibling_idx = lt_idx;
      }

      loop_tbl[curr_parent_idx].curr_value = lt_idx;
   }

   attr_idx = NULL_IDX;


   /* Capture the start value.  If it's a constant, get it's value.  If it's  */
   /* an Attr index it should be a loop control variable of an outer loop.    */
   /* Search back through the parent table entries to find the loop control   */
   /* variable and get its current value.  If it's an expression, point to    */
   /* the expression tree.						      */
   /* Note:  expr_sem is called rather than expr_semantics so that subobjects */
   /*        of named constants will not be folded.  If the expression is a   */
   /*        tree when it comes back from expr_sem, we need to check it to    */
   /*        see if it contains any subobjects of named constants.	      */

   il_idx = IL_NEXT_LIST_IDX(IR_IDX_R(imp_do_idx));

   if (compiler_gen_imp_do) {
      loop_tbl[lt_idx].start_value = CN_INT_TO_C(IL_IDX(il_idx));
   }
   else {
      COPY_OPND(opnd, IL_OPND(il_idx));
      expr_mode      = Restricted_Imp_Do_Expr;
      expr_desc.rank = 0;
      xref_state     = CIF_Symbol_Reference;

      if (! expr_sem(&opnd, &expr_desc)) {

         /* It is possible for expr_sem to return a value of false without    */
         /* issuing an error message.					      */

         SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;
         goto EXIT;
      }

      if (expr_desc.linear_type == Short_Typeless_Const) {
         OPND_IDX(opnd) = cast_typeless_constant(OPND_IDX(opnd),
                                                 INTEGER_DEFAULT_TYPE,
						 OPND_LINE_NUM(opnd),
 						 OPND_COL_NUM(opnd));
         expr_desc.type_idx = INTEGER_DEFAULT_TYPE;
         expr_desc.type = Integer;
         expr_desc.linear_type = INTEGER_DEFAULT_TYPE;
      }

   
      if (expr_desc.rank == 0  &&
          expr_desc.type == Integer) {
         COPY_OPND(IL_OPND(il_idx), opnd);
         loop_tbl[lt_idx].start_fld = IL_FLD(il_idx);
         loop_tbl[lt_idx].start_idx = IL_IDX(il_idx);
   
         switch (loop_tbl[lt_idx].start_fld) {

            case CN_Tbl_Idx:
               loop_tbl[lt_idx].start_value = CN_INT_TO_C(IL_IDX(il_idx));
               break;
   
            case AT_Tbl_Idx:
               search_idx = loop_tbl[lt_idx].parent_idx;
   
               while (search_idx != NULL_IDX) {

                  if (loop_tbl[search_idx].lcv_idx == IL_IDX(il_idx)) {
                     loop_tbl[lt_idx].start_idx = search_idx;
                     break;
                  }
                  else {
                     search_idx = loop_tbl[search_idx].parent_idx;
                  }
     
               }

               if (search_idx == NULL_IDX) {
                  PRINTMSG(IL_LINE_NUM(il_idx), 658, Error, IL_COL_NUM(il_idx),
                           AT_OBJ_NAME_PTR(IL_IDX(il_idx)));
                  goto EXIT;
               }

               break;

            case IR_Tbl_Idx:
               if (good_data_imp_do_expr(loop_tbl[lt_idx].start_idx)) {
                  arg_info_list_top = arg_info_list_base + 1;

                  loop_tbl[lt_idx].start_expr_desc_idx = arg_info_list_top;
   
                  if (arg_info_list_top > arg_info_list_size) {
                     enlarge_info_list_table();
                  }

                  arg_info_list[arg_info_list_top]    = init_arg_info;
                  arg_info_list[arg_info_list_top].ed = expr_desc;
               }

               break; 
  
            default:
               PRINTMSG(IR_LINE_NUM(imp_do_idx), 179, Internal,
                        IR_COL_NUM(imp_do_idx), "build_loop_tbl");
         }    
      }
      else {
         PRINTMSG(IL_LINE_NUM(il_idx), 936, Error, IL_COL_NUM(il_idx));
      }
   }


   /* Capture the end value.  The processing is the same as for the start     */
   /* value.								      */

   il_idx = IL_NEXT_LIST_IDX(il_idx);

   if (compiler_gen_imp_do) {
      loop_tbl[lt_idx].end_value = CN_INT_TO_C(IL_IDX(il_idx));
   }
   else {
      COPY_OPND(opnd, IL_OPND(il_idx));
      expr_desc.rank = 0;
      xref_state = CIF_Symbol_Reference;

      if (! expr_sem(&opnd, &expr_desc)) {
         SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;
         goto EXIT;
      }

      if (expr_desc.linear_type == Short_Typeless_Const) {
         OPND_IDX(opnd) = cast_typeless_constant(OPND_IDX(opnd),
                                                 INTEGER_DEFAULT_TYPE,
                                                 OPND_LINE_NUM(opnd),
                                                 OPND_COL_NUM(opnd));
         expr_desc.type_idx = INTEGER_DEFAULT_TYPE;
         expr_desc.type = Integer;
         expr_desc.linear_type = INTEGER_DEFAULT_TYPE;
      }

      if (expr_desc.rank == 0  &&
          expr_desc.type == Integer) {

         COPY_OPND(IL_OPND(il_idx), opnd);
         loop_tbl[lt_idx].end_fld = IL_FLD(il_idx);
         loop_tbl[lt_idx].end_idx = IL_IDX(il_idx);
   
         switch (loop_tbl[lt_idx].end_fld) {

            case CN_Tbl_Idx:
               loop_tbl[lt_idx].end_value = CN_INT_TO_C(IL_IDX(il_idx));
               break;
   
            case AT_Tbl_Idx:
               search_idx = loop_tbl[lt_idx].parent_idx;

               while (search_idx != NULL_IDX) {
         
                  if (loop_tbl[search_idx].lcv_idx == IL_IDX(il_idx)) {
                     loop_tbl[lt_idx].end_idx   = search_idx;
                     break;
                  }
                  else {
                     search_idx = loop_tbl[search_idx].parent_idx;
                  }
     
               }

               if (search_idx == NULL_IDX) {
                  PRINTMSG(IL_LINE_NUM(il_idx), 658, Error, IL_COL_NUM(il_idx),
                           AT_OBJ_NAME_PTR(IL_IDX(il_idx)));
                  goto EXIT;
               }

               break;

            case IR_Tbl_Idx:
               if (good_data_imp_do_expr(loop_tbl[lt_idx].end_idx)) {
                  arg_info_list_top = arg_info_list_base + 1;

                  loop_tbl[lt_idx].end_expr_desc_idx = arg_info_list_top;
  
                  if (arg_info_list_top > arg_info_list_size) {
                     enlarge_info_list_table();
                  }

                  arg_info_list[arg_info_list_top]    = init_arg_info;
                  arg_info_list[arg_info_list_top].ed = expr_desc;
               }

               break; 
  
            default:
               PRINTMSG(IR_LINE_NUM(imp_do_idx), 179, Internal,
                        IR_COL_NUM(imp_do_idx), "build_loop_tbl");
         }
      }
      else {
         PRINTMSG(IL_LINE_NUM(il_idx), 936, Error, IL_COL_NUM(il_idx));
      }
   }


   /* If no increment value was supplied, use 1.  Otherwise, capture the      */
   /* value in the same way the start and end values were captured.           */
   /* Generate an IL to represent the value 1 for the interface's convenience.*/

   if (IL_NEXT_LIST_IDX(il_idx) == NULL_IDX) {
      loop_tbl[lt_idx].inc_fld = CN_Tbl_Idx;
      loop_tbl[lt_idx].inc_idx = CN_INTEGER_ONE_IDX;
      loop_tbl[lt_idx].inc_value = CN_INT_TO_C(CN_INTEGER_ONE_IDX);

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(il_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(il_idx)) = il_idx; 
      il_idx = IL_NEXT_LIST_IDX(il_idx);
      IL_LINE_NUM(il_idx) = IL_LINE_NUM(IL_PREV_LIST_IDX(il_idx));
      IL_COL_NUM(il_idx) = IL_COL_NUM(IL_PREV_LIST_IDX(il_idx));
      IL_FLD(il_idx) = CN_Tbl_Idx; 
      IL_IDX(il_idx) = CN_INTEGER_ONE_IDX;
      ++IR_LIST_CNT_R(imp_do_idx);
   }
   else if (compiler_gen_imp_do) {
      il_idx = IL_NEXT_LIST_IDX(il_idx);
      loop_tbl[lt_idx].inc_value = CN_INT_TO_C(IL_IDX(il_idx));
   }
   else {
      il_idx = IL_NEXT_LIST_IDX(il_idx);
      COPY_OPND(opnd, IL_OPND(il_idx));
      expr_desc.rank = 0;
      xref_state = CIF_Symbol_Reference;

      if (! expr_sem(&opnd, &expr_desc)) {
         SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;
         goto EXIT;
      }

      if (expr_desc.linear_type == Short_Typeless_Const) {
         OPND_IDX(opnd) = cast_typeless_constant(OPND_IDX(opnd),
                                                 INTEGER_DEFAULT_TYPE,
                                                 OPND_LINE_NUM(opnd),
                                                 OPND_COL_NUM(opnd));
         expr_desc.type_idx = INTEGER_DEFAULT_TYPE;
         expr_desc.type = Integer;
         expr_desc.linear_type = INTEGER_DEFAULT_TYPE;
      }

      if (expr_desc.rank == 0  &&
          expr_desc.type == Integer) {

         COPY_OPND(IL_OPND(il_idx), opnd);
         loop_tbl[lt_idx].inc_fld = IL_FLD(il_idx);
         loop_tbl[lt_idx].inc_idx = IL_IDX(il_idx);
   
         switch (loop_tbl[lt_idx].inc_fld) {

            case CN_Tbl_Idx:
               if (fold_relationals(IL_IDX(il_idx),
                                    CN_INTEGER_ZERO_IDX,
                                    Eq_Opr)) {
                  PRINTMSG(IL_LINE_NUM(il_idx), 1084, Error, 
                           IL_COL_NUM(il_idx));
               }
               else {
                  loop_tbl[lt_idx].inc_value = CN_INT_TO_C(IL_IDX(il_idx));
               }

               break;
   
            case AT_Tbl_Idx:
               search_idx = loop_tbl[lt_idx].parent_idx;

               while (search_idx != NULL_IDX) {
            
                  if (loop_tbl[search_idx].lcv_idx == IL_IDX(il_idx)) {
                     loop_tbl[lt_idx].inc_idx = search_idx;
                     break;
                  }
                  else {
                     search_idx = loop_tbl[search_idx].parent_idx;
                  }
     
               }

               if (search_idx == NULL_IDX) {
                  PRINTMSG(IL_LINE_NUM(il_idx), 658, Error, IL_COL_NUM(il_idx),
                           AT_OBJ_NAME_PTR(IL_IDX(il_idx)));
                  goto EXIT;
               }
      
               break;

            case IR_Tbl_Idx:
               if (good_data_imp_do_expr(loop_tbl[lt_idx].inc_idx)) {
                  arg_info_list_top = arg_info_list_base + 1;

                  loop_tbl[lt_idx].inc_expr_desc_idx = arg_info_list_top;

                  if (arg_info_list_top > arg_info_list_size) {
                     enlarge_info_list_table();
                  }

                  arg_info_list[arg_info_list_top]    = init_arg_info;
                  arg_info_list[arg_info_list_top].ed = expr_desc;
               }

               break; 
  
            default:
               PRINTMSG(IR_LINE_NUM(imp_do_idx), 179, Internal,
                        IR_COL_NUM(imp_do_idx), "build_loop_tbl");
         }
      }
      else {
         PRINTMSG(IL_LINE_NUM(il_idx), 936, Error, IL_COL_NUM(il_idx));
      }
   }


   /* Resolve the loop control variable.  If an Attr for the same name exists */
   /* at an outer level, apply the semantic checks to that Attr; else use the */
   /* implied-DO local Attr.  Verify that the entity is a data object of type */
   /* integer and that it does not have the same name as a named constant.    */

   il_idx = IR_IDX_R(imp_do_idx);

   if (compiler_gen_imp_do) {
      loop_tbl[lt_idx].lcv_idx = IL_IDX(il_idx);
   }
   else {
      COPY_OPND(opnd, IL_OPND(il_idx));
      expr_desc.rank = 0;
      expr_mode = Regular_Expr;
      xref_state = CIF_No_Usage_Rec;
      save_in_implied_do = in_implied_do;
      in_implied_do = FALSE;
     
      lcv_line = OPND_LINE_NUM(opnd);
      lcv_col = OPND_COL_NUM(opnd);
      attr_idx = find_base_attr(&opnd, &lcv_line, &lcv_col);

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
         save_imp_do_lcv = ATD_IMP_DO_LCV(attr_idx);
         ATD_IMP_DO_LCV(attr_idx) = TRUE;
      }

      semantics_ok = expr_semantics(&opnd, &expr_desc);

      COPY_OPND(IL_OPND(il_idx), opnd);
      in_implied_do = save_in_implied_do;

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
         ATD_IMP_DO_LCV(attr_idx) = save_imp_do_lcv;
      }

      if (expr_desc.reference) {

         if (expr_desc.type != Integer) {
            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 675, Error, column);
            semantics_ok = FALSE;
         }

         if (OPND_FLD(opnd) == IR_Tbl_Idx  &&
             IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr) {
            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
         }

         if (OPND_FLD(opnd) == IR_Tbl_Idx  &&
             IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr) {
            COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
         }

         /* The implied-DO variable must be an unqualified name.              */

         if (semantics_ok  &&  OPND_FLD(opnd) != AT_Tbl_Idx) {
            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 199, Error, column);
            semantics_ok = FALSE;
         }
         else {
            attr_idx = OPND_IDX(opnd);
         }

         if (semantics_ok  &&  expr_desc.rank != 0) {
            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 837, Ansi, column);
         }
      }
      else {

         /* The implied-DO variable must be a variable (as opposed to an      */
         /* expression or a constant, for instance).			      */

         find_opnd_line_and_column(&opnd, &line, &column);
         PRINTMSG(line, 675, Error, column);
         semantics_ok = FALSE;
      }

      if (semantics_ok) {
         find_opnd_line_and_column(&opnd, &line, &column);

         if (AT_ATTR_LINK(attr_idx)) {
            PRINTMSG(line, 533, Error, column,
                     AT_OBJ_NAME_PTR(attr_idx));
            semantics_ok = FALSE;
         }
         else {
            do_var_tmp_idx = gen_compiler_tmp(line, column, Priv, TRUE);
            AT_SEMANTICS_DONE(do_var_tmp_idx) = TRUE;
            ATD_TYPE_IDX(do_var_tmp_idx) = ATD_TYPE_IDX(attr_idx);
            ATD_STOR_BLK_IDX(do_var_tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

            AT_ATTR_LINK(attr_idx) = do_var_tmp_idx;
            AT_IGNORE_ATTR_LINK(attr_idx) = TRUE;

            ATD_IMP_DO_LCV(do_var_tmp_idx) = TRUE;
            ATD_LCV_IS_CONST(do_var_tmp_idx) = TRUE;
            ATD_TMP_NEEDS_CIF(do_var_tmp_idx) = TRUE;

            /* change name to original name */
            AT_NAME_IDX(do_var_tmp_idx) = AT_NAME_IDX(attr_idx);
            AT_NAME_LEN(do_var_tmp_idx) = AT_NAME_LEN(attr_idx);

            /* clear the referenced field so that this tmp does */
            /* not get sent to mif.  BHJ                        */

            AT_REFERENCED(do_var_tmp_idx) = Not_Referenced;

            IL_FLD(il_idx) = AT_Tbl_Idx;
            IL_IDX(il_idx) = do_var_tmp_idx;
            IL_LINE_NUM(il_idx) = line;
            IL_COL_NUM(il_idx) = column;

            loop_tbl[lt_idx].lcv_idx = do_var_tmp_idx;

            /* issue a usage rec if needed */
            if ((cif_flags & XREF_RECS) != 0) {
               cif_usage_rec(do_var_tmp_idx, AT_Tbl_Idx, line, column,
                             CIF_Symbol_Modification);
            }

         }
      }
   }


   if (SH_ERR_FLG(curr_stmt_sh_idx)) {
      goto EXIT;
   }

   /* Count the number of target variables at this level of implied-DO.       */
   /* Example:   DATA ((tgt(i,j), i=1,10), a1(j), a2(j), j=1,3,2)             */
   /* The inner loop has one target and the outer has two.		      */

   in_implied_do = TRUE;
   target_idx = IR_IDX_L(imp_do_idx);
   loop_tbl[lt_idx].target_list = target_idx;

   while (target_idx != NULL_IDX) {
      ++loop_tbl[lt_idx].num_targets;
      
      if (IL_FLD(target_idx) == IR_Tbl_Idx  &&
          IR_OPR(IL_IDX(target_idx)) == Implied_Do_Opr) {
         curr_parent_idx = lt_idx;
         build_loop_tbl(IL_IDX(target_idx), compiler_gen_imp_do);
      }
      else if (! compiler_gen_imp_do) {
         COPY_OPND(opnd, IL_OPND(target_idx));
         object_semantics(&opnd,
                           Restricted_Imp_Do_Target,
                          &expr_desc,
                           FALSE,
			   FALSE);

         if (! SH_ERR_FLG(curr_stmt_sh_idx)) {

            /* An implied-DO target can only be an array element or a scalar  */
            /* structure component reference (note that there is no rule      */
            /* that the component reference must have an interior subscript   */
            /* list). 							      */

            if (expr_desc.rank != 0  ||  OPND_FLD(opnd) != IR_Tbl_Idx) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 709, Error, column);
               goto EXIT;
            }

            /* The Whole_Substring or Substring IR is annoyingly at the top   */
            /* of the reference tree so they must be skipped over to really   */
            /* tell what kind of reference we have.			      */

            temp_ir_idx = OPND_IDX(opnd);

            if (IR_OPR(temp_ir_idx) == Whole_Substring_Opr  ||
                IR_OPR(temp_ir_idx) == Substring_Opr) {
               temp_ir_idx = IR_IDX_L(temp_ir_idx);
            }
 
            if (IR_OPR(temp_ir_idx) != Subscript_Opr  &&
                IR_OPR(temp_ir_idx) != Struct_Opr) { 
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 709, Error, column);
               goto EXIT;
            }
      
            COPY_OPND(IL_OPND(target_idx), opnd);
         }
      }

      target_idx = IL_NEXT_LIST_IDX(target_idx);
   }


   /* Pop out to the containing implied-DO, if there is one. */

   lt_idx = curr_parent_idx;
   
   if (curr_parent_idx != NULL_IDX) {
      curr_parent_idx = loop_tbl[lt_idx].parent_idx;
   }


EXIT:


   /* This implied-DO is done so clear the AT_ATTR_LINK field of the */
   /* implied-DO variable so no processing will go on to the temp.   */

   if (semantics_ok  &&  attr_idx != NULL_IDX) {
      AT_ATTR_LINK(attr_idx) = NULL_IDX;
      AT_IGNORE_ATTR_LINK(attr_idx) = FALSE;
   }

   in_implied_do = save_in_implied_do;
   expr_mode = Regular_Expr;

   TRACE (Func_Exit, "build_loop_tbl", NULL);
  
   return;

}  /* build_loop_tbl */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure checks to see that an IR tree returned from expr_sem   *|
|*      for a DATA implied-DO loop control expression does not contain a      *|
|*      subobject of a named constant.					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      ir_do_idx : the index of the IR tree				      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if the expression does not contain a subobject designator        *|
|*                                                                            *|
\******************************************************************************/

static boolean good_data_imp_do_expr(int          ir_idx)

{
   boolean	result	= TRUE;


   TRACE (Func_Entry, "good_data_imp_do_expr", NULL);

   switch (IR_OPR(ir_idx)) {

      case Power_Opr:
      case Mult_Opr:
      case Div_Opr:
      case Uplus_Opr:
      case Uminus_Opr:
      case Plus_Opr:
      case Minus_Opr:
      case Paren_Opr:
      case Cvrt_Opr:
      case Cvrt_Unsigned_Opr:
         if (IR_FLD_L(ir_idx) == IR_Tbl_Idx) {
     
            if (! good_data_imp_do_expr(IR_IDX_L(ir_idx))) {
               result = FALSE;
            }
         }

         if (IR_FLD_R(ir_idx) == IR_Tbl_Idx) {
     
            if (! good_data_imp_do_expr(IR_IDX_R(ir_idx))) {
               result = FALSE;
            }
         }

         break;

      case Struct_Opr:
      case Subscript_Opr:
#ifdef KEY /* Bug 14171 */
	 /* Restriction was removed between F90 and F95 */
  	 result = TRUE;
#else /* KEY Bug 14171 */
         PRINTMSG(IR_LINE_NUM(ir_idx), 1081, Error, IR_COL_NUM(ir_idx));
         result = FALSE;
#endif /* KEY Bug 14171 */
         break;

      default:
         PRINTMSG(IR_LINE_NUM(ir_idx), 179, Internal, 0,
                  "good_data_imp_do_expr");
   }

   TRACE (Func_Exit, "good_data_imp_do_expr", NULL);

   return(result);

}  /* good_data_imp_do_expr */
  

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Due to the characteristics of the PDGCS interface for implied-DOs,    *|
|*      implied-DOs that initialize many array elements can take an           *|
|*      excruciatingly long time to complete.  On the other hand, whole array *|
|*      and array section initialization forms run significantly faster.  So  *|
|*      this procedure is called to snoop through an implied-DO to see if it  *|
|*      can undergo metamorphosis from a nasty, crawling implied-DO           *|
|*      caterpillar to a lovely, dainty whole array butterfly or at least to  *|
|*      a less elegant (but yet winged) array section moth.  In order for the *|
|*      metamorphosis to take place, a number of environmental conditions     *|
|*      must be met:							      *|
|*                                                                            *|
|*        1. The innermost loop can only have a single target and all outer   *|
|*           loops can only be used to drive the innermost loop.  That is,    *|
|*           a loop of the form:					      *|
|*                                                                            *|
|*                DATA ((arr(i,j), i = 1, 10), j = 1, 10) /.../  	      *|
|*                                                                            *|
|*           can undergo metamorphosis but a loop of either of the following  *|
|*           forms can not:                 				      *|
|*                                                                            *|
|*                DATA ((a1(i,j), a2(i,j), i = 1, 10), j = 1, 10) /.../       *|
|*                DATA ((a1(i,j), i = 1, 10), a2(j), j = 1, 10)   /.../       *|
|*                                                                            *|
|*        2. The target must be of numeric or logical type.  All of the loops *|
|*           that cause the compiler problems at this point are FORTRAN 77    *|
|*           implied-DOs.  We don't need to worry yet about a program that    *|
|*           tries to initialize a large number of elements of a large array  *|
|*           of structures.  Likely if people start using arrays of           *|
|*           structures, they will also use other Fortran 90 initialization   *|
|*           concepts such as whole array initialization or initialization on *|
|*           the type declaration statement.				      *|
|*                                                                            *|
|*        3. The target must be a simple array element reference.  Again, all *|
|*           of the problem implied-DOs are 77-style loops.  Therefore, we    *|
|*           should not have to worry about initialization of a structure     *|
|*           component that is a large array or initializing an array         *|
|*           constructed by taking one component from each element of an      *|
|*           array of structures.  This rule eliminates having to dig through *|
|*           an arbitrarily complex structure reference tree to find the      *|
|*           entity that's actually being initialized.			      *|
|*                                                                            *|
|*        4. Each subscript in the array element reference must be an         *|
|*           implied-DO variable or a constant.  An expression prevents       *|
|*           metamorphosis because it can't be translated into a whole array  *|
|*           initialization and sometimes not even into an array section      *|
|*           section initialization.  It's too much work to allow in SOME     *|
|*           expressions because it's too much work to see if they are        *|
|*           satisfactory.  For example, the first implied-DO below can be    *|
|*           metamorphed but the second can not:			      *|
|*                                                                            *|
|*                DATA (arr(i + 1), i = 1, 10)   /.../                        *|
|*                DATA ((arr(i + j, j), i = 1, 10), j = 1, 10)   /.../        *|
|*                                                                            *|
|*        5. Each subscript that is an implied-DO variable must be in the     *|
|*           same order as the loop nest.  That is, the subscripts that are   *|
|*           variables must vary the fastest from the left to the right (from *|
|*           the innermost loop to the outermost) because this is the way     *|
|*           whole array and section subscripting works.  For example, the    *|
|*           first implied-DO below can be metamorphed but the second can     *|
|*           not:							      *|
|*                                                                            *|
|*                DATA ((arr(i,2,j), i = 1, 10), j = 1, 10)   /.../           *|
|*                DATA ((arr(j,i), i = 1, 10), j = 1, 10)     /.../           *|
|*                                                                            *|
|*        6. Every loop control expression of every loop in the nest must be  *|
|*           constant.  Again, due to the way subscripting works in a whole   *|
|*           array or section reference, a loop control expression can not    *|
|*           involve an implied-DO variable from an outer loop.		      *|
|*                                                                            *|
|*        7. No value in the value list can be a long Hollerith or character  *|
|*           constant.  We don't do whole array initializations or array      *|
|*           section initializations when the values are character forms so   *|
|*           we can't change an implied-DO into either one.  And it's too     *|
|*           darn much work to sift through the values matching them to see   *|
|*           that the character values do or do not get assigned to the       *|
|*           implied-DO target so we take the easier route of quitting if a   *|
|*           character value shows up anywhere in the value list.             *|
|*                                                                            *|
|*        8. The number of array elements being initialized must be equal to  *|
|*           the number of values in the value list.  The Cray version of     *|
|*           this compiler has an extension whereby when a whole array is     *|
|*           being initialized, if it is the only array or the last array in  *|
|*           the object list, the number of values can be less than the       *|
|*           of array elements.  If we do not make the count comparison in    *|
|*           this rule and change an implied-DO into a whole array init, we   *|
|*           could lose the count mismatch diagnostic and would thereby allow *|
|*           an otherwise invalid implied-DO through.			      *|
|*                                                                            *|
|*      If all the above requirements are met then if each loop range matches *|
|*      the declared bounds of the target array, the implied-DO caterpillar   *|
|*      will undergo metamorphosis to a whole array butterfly; otherwise, it  *|
|*      will become an array section moth.				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      init_ir_idx : The index of the Init IR that heads the implied-DO.     *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if the implied-DO underwent metamorphosis.			      *|
|*                                                                            *|
\******************************************************************************/

static boolean imp_do_metamorphed(int	init_ir_idx)
{
   int			attr_idx;
   expr_arg_type	expr_desc;
   opnd_type		expr_opnd;
   int			i;
   int			il_idx;
   int			ir_idx;
   int			iter_count_ir_idx;
   int			local_lt_idx;
   boolean		loops_match_bounds;
   boolean		metamorphed;
   int			num_elements_idx;
   long 		num_single_values;
   int			num_single_values_idx;
   int			num_values_idx;
   int                  result_type_idx;
   long_type            result_value[MAX_WORDS_FOR_NUMERIC];
   int			subscript_ir_idx;
   int			triplet_ir_idx;
   int			t1_il_idx;
   int			t2_il_idx;


   TRACE (Func_Entry, "imp_do_metamorphed", NULL);

   metamorphed           = FALSE;
   num_elements_idx      = CN_INTEGER_ONE_IDX;
   num_values_idx        = CN_INTEGER_ZERO_IDX;
   num_single_values     = 0;
   num_single_values_idx = CN_INTEGER_ZERO_IDX;


   /* 1. The innermost loop can only have a single target and all outer loops */
   /*    can only be used to drive the innermost loop (therefore, each loop   */
   /*    in the nest can only have a single target).			      */

   for (i = 1;  i <= last_lt_idx;  ++i) {

      if (loop_tbl[i].num_targets != 1) {
         goto EXIT;
      }
   }


   /* 2. The target must be of numeric or logical type.			      */
   /*                                                                         */
   /* 3. The target must be a simple array element reference.                 */

   subscript_ir_idx = IL_IDX(loop_tbl[last_lt_idx].target_list);

   if (IR_OPR(subscript_ir_idx) != Subscript_Opr  ||
       IR_FLD_L(subscript_ir_idx) != AT_Tbl_Idx) {
      goto EXIT;
   }

   attr_idx = IR_IDX_L(subscript_ir_idx);

   if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Integer  &&
       TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Real     &&
       TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Complex  &&
       TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Logical) {
      goto EXIT;
   }


   /* 4. Each subscript in the array element reference must be an implied-DO  */
   /*    variable or a constant.  				              */
   /*                                                                         */
   /* 5. Each subscript that is an implied-DO variable must be in the same    */
   /*    order as the loop nest. 					      */
   /*                                                                         */
   /* 6. Every loop control expression of every loop in the nest must be      */
   /*    constant.							      */

   loops_match_bounds = TRUE;
   local_lt_idx       = last_lt_idx;
   il_idx             = IR_IDX_R(subscript_ir_idx);


   for (i = 1;  i <= IR_LIST_CNT_R(subscript_ir_idx);  ++i) {
   
      if (IL_FLD(il_idx) == AT_Tbl_Idx) {
      
         if (IL_IDX(il_idx) != loop_tbl[local_lt_idx].lcv_idx) {
            goto EXIT;
         }

         if (loop_tbl[local_lt_idx].start_fld == CN_Tbl_Idx  &&
             loop_tbl[local_lt_idx].end_fld   == CN_Tbl_Idx  &&
             loop_tbl[local_lt_idx].inc_fld   == CN_Tbl_Idx) {

            if (fold_relationals(loop_tbl[local_lt_idx].start_idx,
                                 BD_LB_IDX(ATD_ARRAY_IDX(attr_idx), i),
                                 Ne_Opr)  ||
                fold_relationals(loop_tbl[local_lt_idx].end_idx,
                                 BD_UB_IDX(ATD_ARRAY_IDX(attr_idx), i),
                                 Ne_Opr)  ||
                fold_relationals(loop_tbl[local_lt_idx].inc_idx,
                                 CN_INTEGER_ONE_IDX,
                                 Ne_Opr)) {
               loops_match_bounds = FALSE;
            }


            /* Build an IR tree to calculate the number of times the loop     */
            /* will execute.     					      */

            NTR_IR_TBL(iter_count_ir_idx);
            IR_OPR(iter_count_ir_idx)      = Minus_Opr;
            IR_TYPE_IDX(iter_count_ir_idx) = INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(iter_count_ir_idx) = stmt_start_line;
            IR_COL_NUM(iter_count_ir_idx)  = stmt_start_col;
            IR_FLD_L(iter_count_ir_idx)    = CN_Tbl_Idx;
            IR_IDX_L(iter_count_ir_idx)    = loop_tbl[local_lt_idx].end_idx;
            IR_LINE_NUM_L(iter_count_ir_idx) = stmt_start_line;
            IR_COL_NUM_L(iter_count_ir_idx)  = stmt_start_col;
            IR_FLD_R(iter_count_ir_idx)    = CN_Tbl_Idx;
            IR_IDX_R(iter_count_ir_idx)    = loop_tbl[local_lt_idx].start_idx;
            IR_LINE_NUM_R(iter_count_ir_idx) = stmt_start_line;
            IR_COL_NUM_R(iter_count_ir_idx)  = stmt_start_col;

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)      = Plus_Opr;
            IR_TYPE_IDX(ir_idx) = INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(ir_idx) = stmt_start_line;
            IR_COL_NUM(ir_idx)  = stmt_start_col;
            IR_FLD_L(ir_idx)    = IR_Tbl_Idx;
            IR_IDX_L(ir_idx)    = iter_count_ir_idx;
            IR_FLD_R(ir_idx)    = CN_Tbl_Idx;
            IR_IDX_R(ir_idx)    = loop_tbl[local_lt_idx].inc_idx;
            IR_LINE_NUM_R(ir_idx) = stmt_start_line;
            IR_COL_NUM_R(ir_idx)  = stmt_start_col;

            NTR_IR_TBL(iter_count_ir_idx);
            IR_OPR(iter_count_ir_idx)      = Div_Opr;
            IR_TYPE_IDX(iter_count_ir_idx) = INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(iter_count_ir_idx) = stmt_start_line;
            IR_COL_NUM(iter_count_ir_idx)  = stmt_start_col;
            IR_FLD_L(iter_count_ir_idx)    = IR_Tbl_Idx;
            IR_IDX_L(iter_count_ir_idx)    = ir_idx;
            IR_FLD_R(iter_count_ir_idx)    = CN_Tbl_Idx;
            IR_IDX_R(iter_count_ir_idx)    = loop_tbl[local_lt_idx].inc_idx;
            IR_LINE_NUM_R(iter_count_ir_idx) = stmt_start_line;
            IR_COL_NUM_R(iter_count_ir_idx)  = stmt_start_col;

            OPND_FLD(expr_opnd) = IR_Tbl_Idx;
            OPND_IDX(expr_opnd) = iter_count_ir_idx;

            if (! expr_semantics(&expr_opnd, &expr_desc)) {
               PRINTMSG(IR_LINE_NUM(init_ir_idx), 857, Internal,
                        IR_COL_NUM(init_ir_idx));
            }


            /* Add in this loop's iteration count to the total.		      */
 
            result_type_idx = INTEGER_DEFAULT_TYPE;

            if (folder_driver( (char *) &CN_CONST(num_elements_idx),
                               CN_TYPE_IDX(num_elements_idx),
                               (char *) &CN_CONST(OPND_IDX(expr_opnd)),
                               expr_desc.type_idx,
                               result_value,
                              &result_type_idx,
                               IR_LINE_NUM(init_ir_idx),
                               IR_COL_NUM(init_ir_idx),
                               2,
                               Mult_Opr)) {
               num_elements_idx = ntr_const_tbl(INTEGER_DEFAULT_TYPE,
                                                FALSE,
                                                result_value);
            }
            else {
               PRINTMSG(IR_LINE_NUM(init_ir_idx), 1024, Internal,
                        IR_COL_NUM(init_ir_idx));
            }

            --local_lt_idx;
         }
         else {
            goto EXIT;
         }
      }
      else if (IL_FLD(il_idx) == CN_Tbl_Idx) {
         loops_match_bounds = FALSE;
      }
      else {
         goto EXIT;
      }

      il_idx = IL_NEXT_LIST_IDX(il_idx);
   }


   /* 7. No value in the value list can be a long Hollerith or a character    */
   /*    constant.         						      */
   /*								              */
   /* The value can be a stand-alone value or it can be the object of a rep   */
   /* factor (the right operand of a Rep_Count IR).			      */

   il_idx = value_il_idx;

   while (il_idx != NULL_IDX) {

      if (IL_FLD(il_idx) == CN_Tbl_Idx) {
 
         if (TYP_TYPE(CN_TYPE_IDX(IL_IDX(il_idx))) != Character  &&
             TYP_LINEAR(CN_TYPE_IDX(IL_IDX(il_idx))) != Long_Typeless) {
            ++num_single_values;
         }
         else {
            goto EXIT;
         }
      }
      else if (IL_FLD(il_idx) == IR_Tbl_Idx) {
         ir_idx = IL_IDX(il_idx);

         if (IR_OPR(ir_idx) == Rep_Count_Opr) {
         
            if (IR_FLD_R(ir_idx) == CN_Tbl_Idx  &&
                (TYP_TYPE(CN_TYPE_IDX(IR_IDX_R(ir_idx))) == Character  ||
                 TYP_LINEAR(CN_TYPE_IDX(IR_IDX_R(ir_idx))) == Long_Typeless)) {
               goto EXIT;
            }

            /* Add the repetition count to the total number of values.        */
 
            result_type_idx = INTEGER_DEFAULT_TYPE;

            if (folder_driver( (char *) &CN_CONST(num_values_idx),
                               CN_TYPE_IDX(num_values_idx),
                               (char *) &CN_CONST(IR_IDX_L(ir_idx)),
                               CN_TYPE_IDX(IR_IDX_L(ir_idx)),
                               result_value,
                              &result_type_idx,
                               IR_LINE_NUM(ir_idx),
                               IR_COL_NUM(ir_idx),
                               2,
                               Plus_Opr)) {
               num_values_idx = ntr_const_tbl(INTEGER_DEFAULT_TYPE,
                                              FALSE,
                                              result_value);
            }
            else {
               PRINTMSG(IL_LINE_NUM(il_idx), 1024, Internal,
                        IL_COL_NUM(il_idx));
            }
         }
         else {

            /* Assume it is a unary + or - operator.			      */

            if (TYP_TYPE(CN_TYPE_IDX(IR_IDX_L(ir_idx))) != Character  &&
                TYP_LINEAR(CN_TYPE_IDX(IR_IDX_L(ir_idx))) != Long_Typeless) {
               ++num_single_values;
            }
            else {
               goto EXIT;
            }
         }
      }

      il_idx = IL_NEXT_LIST_IDX(il_idx);
   }

 
   /* 8. The number of array elements being initialized must be equal to the  */
   /*    the number of values in the value list.                 	      */

   num_single_values_idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                       num_single_values);

   if (num_single_values_idx != CN_INTEGER_ZERO_IDX  &&
       num_values_idx != CN_INTEGER_ZERO_IDX) {
 
      result_type_idx = INTEGER_DEFAULT_TYPE;

      if (folder_driver( (char *) &CN_CONST(num_single_values_idx),
                         CG_INTEGER_DEFAULT_TYPE,
                         (char *) &CN_CONST(num_values_idx),
                         CN_TYPE_IDX(num_values_idx),
                         result_value,
                         &result_type_idx,
                         IR_LINE_NUM(init_ir_idx),
                         IR_COL_NUM(init_ir_idx),
                         2,
                         Plus_Opr)) {

         num_values_idx = ntr_const_tbl(INTEGER_DEFAULT_TYPE,
                                        FALSE,
                                        result_value);
      }
      else {
         PRINTMSG(IR_LINE_NUM(init_ir_idx), 1024, Internal,
                  IR_COL_NUM(init_ir_idx));
      }
   }

   if (fold_relationals(num_values_idx, CN_INTEGER_ZERO_IDX, Eq_Opr)) {
      num_values_idx = num_single_values_idx;
   }

   if (fold_relationals(num_elements_idx, num_values_idx, Ne_Opr)) {
      goto EXIT;
   }

   
   /* If control gets here, all the rules have been met so the implied-DO can */
   /* be metamorphed.							      */

   metamorphed = TRUE;

   PRINTMSG(IR_LINE_NUM(init_ir_idx), 1021, Note, IR_COL_NUM(init_ir_idx));

   if (loops_match_bounds) {

      /* The loop can be transformed into a whole array initialization.       */
      /* If the implied-DO is the first item or the only item in the list,    */
      /* the Init IR's left operand will be pointing at the IL that in turn   */
      /* points at the implied-DO IR tree.  To fake out data_stmt_semantics,  */
      /* put the array name in the IL attached to the Init IR.		      */
      /* If the implied-DO is NOT the first or the only item in the list, a   */
      /* new, empty Init IR was generated by data_stmt_semantics.  To fake    */
      /* out data_stmt_semantics, attach an IL to the Init IR's left operand  */
      /* and put the array name in the IL.				      */

      if (IR_FLD_L(init_ir_idx) == NO_Tbl_Idx) {
         NTR_IR_LIST_TBL(IR_IDX_L(init_ir_idx));
         IR_LIST_CNT_L(init_ir_idx) = 1;
         IR_FLD_L(init_ir_idx)      = IL_Tbl_Idx;

      }

      IL_FLD(IR_IDX_L(init_ir_idx)) = AT_Tbl_Idx;
      IL_IDX(IR_IDX_L(init_ir_idx)) = attr_idx;
      IL_LINE_NUM(IR_IDX_L(init_ir_idx)) = IR_LINE_NUM(init_ir_idx);
      IL_COL_NUM(IR_IDX_L(init_ir_idx))  = IR_COL_NUM(init_ir_idx);
   }
   else {

      /* The loop can be transformed into an array section initialization.    */
      /* To fake out data_stmt_semantics, change the Subscript IR that's the  */
      /* target of the implied-DO into a section reference and attach it to   */
      /* the IL attached to the Init IR.				      */

      IL_FLD(IR_IDX_L(init_ir_idx)) = IR_Tbl_Idx;
      IL_IDX(IR_IDX_L(init_ir_idx)) = subscript_ir_idx;

      local_lt_idx = last_lt_idx;
      il_idx       = IR_IDX_R(subscript_ir_idx);

      for (i = 1;  i <= IR_LIST_CNT_R(subscript_ir_idx);  ++i) {

         if (IL_FLD(il_idx) == AT_Tbl_Idx) {
            NTR_IR_TBL(triplet_ir_idx);
            IL_FLD(il_idx) = IR_Tbl_Idx;
            IL_IDX(il_idx) = triplet_ir_idx;

            IR_OPR(triplet_ir_idx)      = Triplet_Opr;
            IR_TYPE_IDX(triplet_ir_idx) = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(triplet_ir_idx) = IL_LINE_NUM(il_idx);
            IR_COL_NUM(triplet_ir_idx)  = IL_COL_NUM(il_idx);
            
            NTR_IR_LIST_TBL(t1_il_idx);

            IR_LIST_CNT_L(triplet_ir_idx) = 1;
            IR_FLD_L(triplet_ir_idx)      = IL_Tbl_Idx;
            IR_IDX_L(triplet_ir_idx)      = t1_il_idx;

            IL_LINE_NUM(t1_il_idx) = IL_LINE_NUM(il_idx);
            IL_COL_NUM(t1_il_idx)  = IL_COL_NUM(il_idx);
            IL_FLD(t1_il_idx)      = CN_Tbl_Idx;
            IL_IDX(t1_il_idx)      = loop_tbl[local_lt_idx].start_idx;

            NTR_IR_LIST_TBL(t2_il_idx);

            ++IR_LIST_CNT_L(triplet_ir_idx);
            IL_NEXT_LIST_IDX(t1_il_idx) = t2_il_idx;
            IL_PREV_LIST_IDX(t2_il_idx) = t1_il_idx;

            IL_LINE_NUM(t2_il_idx) = IL_LINE_NUM(il_idx);
            IL_COL_NUM(t2_il_idx)  = IL_COL_NUM(il_idx);
            IL_FLD(t2_il_idx)      = CN_Tbl_Idx;
            IL_IDX(t2_il_idx)      = loop_tbl[local_lt_idx].end_idx;

            t1_il_idx = t2_il_idx;

            NTR_IR_LIST_TBL(t2_il_idx);

            ++IR_LIST_CNT_L(triplet_ir_idx);
            IL_NEXT_LIST_IDX(t1_il_idx) = t2_il_idx;
            IL_PREV_LIST_IDX(t2_il_idx) = t1_il_idx;

            IL_LINE_NUM(t2_il_idx) = IL_LINE_NUM(il_idx);
            IL_COL_NUM(t2_il_idx)  = IL_COL_NUM(il_idx);
            IL_FLD(t2_il_idx)      = CN_Tbl_Idx;
            IL_IDX(t2_il_idx)      = loop_tbl[local_lt_idx].inc_idx;

            --local_lt_idx;
         }

         il_idx = IL_NEXT_LIST_IDX(il_idx);
      }
   }

EXIT:

   TRACE (Func_Exit, "imp_do_metamorphed", NULL);

   return(metamorphed);

}  /* imp_do_metamorphed */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*   Interpret the DATA implied-DO to match its targets and values.	      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      init_ir_idx : the index to the current Init IR			      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

static void interpret_data_imp_do(int	 init_ir_idx)
{

   expr_arg_type	expr_desc;
   opnd_type		expr_opnd;
   boolean		first_offspring_imp_do;
   int			i;
   long_type            loc_value[MAX_WORDS_FOR_NUMERIC];
   long64          	num_iterations;
#ifdef KEY /* Bug 10177 */
   int			sister_idx = 0;
#else /* KEY Bug 10177 */
   int			sister_idx;
#endif /* KEY Bug 10177 */
   int			target_il_idx;


   TRACE (Func_Entry, "interpret_data_imp_do", NULL);


   /* Save the guts of the lcv_idx attr.   Store them in a Constant entry     */
   /* pointed to by ATD_TMP_IDX(lcv_idx).            			      */

   GET_LCV_CONST(loop_tbl[lt_idx].lcv_idx, loc_value[0],  /* target const */
           num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(loop_tbl[lt_idx].lcv_idx))]);

   ATD_FLD(loop_tbl[lt_idx].lcv_idx) = CN_Tbl_Idx;
   ATD_TMP_IDX(loop_tbl[lt_idx].lcv_idx) = 
                 ntr_const_tbl(ATD_TYPE_IDX(loop_tbl[lt_idx].lcv_idx),
                               FALSE,
                               loc_value);


   OPND_FLD(expr_opnd) = IR_Tbl_Idx;

   if (loop_tbl[lt_idx].start_fld == AT_Tbl_Idx) {
      loop_tbl[lt_idx].start_value =
         loop_tbl[loop_tbl[lt_idx].start_idx].curr_value;
   }
   else if (loop_tbl[lt_idx].start_fld == IR_Tbl_Idx) {
      OPND_IDX(expr_opnd) = loop_tbl[lt_idx].start_idx;
     
      expr_desc = arg_info_list[loop_tbl[lt_idx].start_expr_desc_idx].ed;
     
      if (! fold_aggragate_expression(&expr_opnd, &expr_desc, TRUE)) {
         goto EXIT;
      }

      loop_tbl[lt_idx].start_value = CN_INT_TO_C(OPND_IDX(expr_opnd));
   }

   if (loop_tbl[lt_idx].end_fld == AT_Tbl_Idx) {
      loop_tbl[lt_idx].end_value =
         loop_tbl[loop_tbl[lt_idx].end_idx].curr_value;
   }
   else if (loop_tbl[lt_idx].end_fld == IR_Tbl_Idx) {
      OPND_IDX(expr_opnd) = loop_tbl[lt_idx].end_idx;

      expr_desc = arg_info_list[loop_tbl[lt_idx].end_expr_desc_idx].ed;
     
      if (! fold_aggragate_expression(&expr_opnd, &expr_desc, TRUE)) {
         goto EXIT;
      }

      loop_tbl[lt_idx].end_value = CN_INT_TO_C(OPND_IDX(expr_opnd));
   }

   if (loop_tbl[lt_idx].inc_fld == AT_Tbl_Idx) {
      loop_tbl[lt_idx].inc_value =
         loop_tbl[loop_tbl[lt_idx].inc_idx].curr_value;
   }
   else if (loop_tbl[lt_idx].inc_fld == IR_Tbl_Idx) {
      OPND_IDX(expr_opnd) = loop_tbl[lt_idx].inc_idx;
     
      expr_desc = arg_info_list[loop_tbl[lt_idx].inc_expr_desc_idx].ed;
     
      if (! fold_aggragate_expression(&expr_opnd, &expr_desc, TRUE)) {
         goto EXIT;
      }

      loop_tbl[lt_idx].inc_value = CN_INT_TO_C(OPND_IDX(expr_opnd));
   }

   num_iterations =
      (loop_tbl[lt_idx].end_value - loop_tbl[lt_idx].start_value +
       loop_tbl[lt_idx].inc_value) /
      loop_tbl[lt_idx].inc_value;

   if (num_iterations < 0) {
      num_iterations = 0;
   }

   if (num_iterations == 0) {
      goto EXIT;
   }

   for (loop_tbl[lt_idx].curr_value = loop_tbl[lt_idx].start_value;
        (loop_tbl[lt_idx].inc_value > 0) ?
           loop_tbl[lt_idx].curr_value <= loop_tbl[lt_idx].end_value :
	   loop_tbl[lt_idx].curr_value >= loop_tbl[lt_idx].end_value;
        loop_tbl[lt_idx].curr_value += loop_tbl[lt_idx].inc_value) {

      C_TO_F_INT(loc_value, 
                 loop_tbl[lt_idx].curr_value,
                 TYP_LINEAR(ATD_TYPE_IDX(loop_tbl[lt_idx].lcv_idx)));
#ifdef KEY
      SET_LCV_CONST(loop_tbl[lt_idx].lcv_idx, 
                    (loc_value[0]),
                    num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(
                                          loop_tbl[lt_idx].lcv_idx))], 
                    num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(
                                          loop_tbl[lt_idx].lcv_idx))]);
#else
      SET_LCV_CONST(loop_tbl[lt_idx].lcv_idx, 
                    (loc_value[0]),
                    num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(
                                          loop_tbl[lt_idx].lcv_idx))]);
#endif

      target_il_idx          = loop_tbl[lt_idx].target_list;
      first_offspring_imp_do = TRUE;

      for (i = 1;  i <= loop_tbl[lt_idx].num_targets;  ++i) {
      
         if (IR_OPR(IL_IDX(target_il_idx)) == Implied_Do_Opr) {

            /* If this is the first offspring that is an implied-DO, get the  */
            /* lt_idx from the offspring_idx field of the current loop_tbl    */
            /* entry and save the index to its sibling (which might be 0).    */
            /* Otherwise, use the saved sibling index to get to the next      */
            /* child of this implied-DO.				      */

            if (first_offspring_imp_do) {
               lt_idx     = loop_tbl[lt_idx].offspring_idx; 
               sister_idx = loop_tbl[lt_idx].sibling_idx;
               first_offspring_imp_do = FALSE;
            }
            else {
               lt_idx     = sister_idx;
               sister_idx = loop_tbl[lt_idx].sibling_idx;
            }


            interpret_data_imp_do(init_ir_idx);

            if (SH_ERR_FLG(curr_stmt_sh_idx)) {
               goto EXIT;
            }
         }
         else {

            process_data_imp_do_target(init_ir_idx,
				       target_il_idx,
				       num_iterations);

            if (SH_ERR_FLG(curr_stmt_sh_idx)) {
               goto EXIT;
            }

            if (loop_tbl[lt_idx].num_targets == 1) {
               goto EXIT;
            }
         }
    
         target_il_idx = IL_NEXT_LIST_IDX(target_il_idx);
      }
   } 

EXIT:

   /* Restore the guts of the LCV temp Attr.				      */
#ifdef KEY
   SET_LCV_CONST(loop_tbl[lt_idx].lcv_idx, 
                 CN_CONST(ATD_TMP_IDX(loop_tbl[lt_idx].lcv_idx)),
            num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(loop_tbl[lt_idx].lcv_idx))], num_host_wds[TYP_LINEAR(CN_TYPE_IDX(ATD_TMP_IDX(loop_tbl[lt_idx].lcv_idx)))]);
#else
   SET_LCV_CONST(loop_tbl[lt_idx].lcv_idx, 
                 CN_CONST(ATD_TMP_IDX(loop_tbl[lt_idx].lcv_idx)),
            num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(loop_tbl[lt_idx].lcv_idx))]);
#endif

   lt_idx = loop_tbl[lt_idx].parent_idx;

   TRACE (Func_Exit, "interpret_data_imp_do", NULL);

   return;

}  /* interpret_data_imp_do */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*   See enormously long description below.				      *|
|*     									      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      init_ir_idx    : the index of the Init IR                             *|
|*      target_il_idx  : IL index of current target			      *|
|*      num_iterations : the number of times the current implied-DO will be   *|
|*                      executed					      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE								      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

/******************************************************************************\
                                 N O T E S

  Among the basic design tenets of DATA processing are the following which 
  directly relate to the relationship between the Semantics Pass and the 
  (PDGCS) Interface:

    * The Interface expects each implied-DO target to have a corresponding 
      value.  This means that the Semantics Pass must ensure that the *number*
      of targets matches the number of values.

    * The Interface should not have to contend with DATA statement semantics
      (error checking).  This means that the Semantics Pass must ensure that
      the *type* of each value is suitable for assignment to the corresponding
      target.

  Normally, this is relatively straightforward because this relationship   
  (expectations) between the Interface and the Semantics Pass reflects the 
  standard.  However, CF77 has an extension that allows a long string (either
  a Hollerith or character literal constant) to be assigned piecemeal to 
  targets of an implied-DO or array initialization.  This procedure
  (process_data_imp_do_target) exists to match a target with a value (in some
  cases match a number of targets with a number of values) and to ensure that
  the target(s) and value(s) are assignment compatible.  (CF77 only allows
  initialization of multiple word-length items with long string constants.
  This means, for example, that double precision and complex items can *not*
  be initialized with a long string.  And CF77 only allows initialization of
  word-length targets with word-length (or less) values, so, for example, a
  double precision variable can not be initialized with a character/Hollerith
  constant.  CF90 relaxes the latter rule in 2.0 and beyond because programs
  exist that are intended to be run on 32-bit machines and that initialize
  multi-word numeric items with Hollerith strings.  However, CF90 will *not*
  enhance the "long string" extension to work with multi-word targets.  And
  since CF77 does not support array section notation in DATA statements, CF90
  does *not* support initialization of array sections with long strings.  And
  finally, since CF77 does not support initialization on type declaration
  statements, CF90 does *not* support the "long string" extension in that
  context either.

  Since a long string can be split up among arbitrary implied-DO targets,
  a number of the bookkeeping variables like value_count are
  static, wither by being global to this file or static in this procedure.
  For whole array initialization, the long string is *not* split across 
  multiple targets.  This means you can *not* do the following:

       integer i(2), k(2)
       data i, k  /'abcdef123456ABCDEF654321'/

  The long string is broken down into word-size pieces and each piece is
  reentered into the Constant Table.  Note that the last piece may be less
  than a word in length.  If the implied-DO does not finish off the long
  string, the remainder is set up as the next value (for the target 
  following the implied-DO) by data_imp_do_semantics (when the (outermost)
  implied-DO is completely finished).  This is not done for whole array 
  initialization.  That is, the "remainder" is not passed on to another 
  variable.  The value must break down into the number of values required by
  the target array (except if the array is the last thing in the list, in
  which case another CRI extension is invoked whereby the last array in the
  list need not be completely initialized).

  The following general rule was deduced from CF77 by running variations on a
  number of programs:

       A long string can be utilized by any combination of numeric targets
       at any level of nesting.  The string can slop over onto an 
       initialization following the implied-DO.  The initialization rules
       for the item following the implied-DO are then in effect.

  There is a minor difference between the CF77 and CF90 implementations of
  the "long string" extension.  In an initialization of the form:

            INTEGER array(3)
            DATA (array(i), i = 1, 3), k  /2 * 16H1234567887654321/

  CF77 issues an error message saying that there are too few values because
  it does not use whatever is left over in the string when the implied-DO
  completes IF THERE IS A REP FACTOR PRESENT.  If the example was written as:

            INTEGER array(2)
            DATA (array(i), i = 1, 2), k  /24H1234567887654321zzzzzzzz/

  CF77 WOULD use the remaining part of the string as the value to be assigned
  to K.  CF90 always uses all of the string in a consistent manner.


  This procedure has two basic processing sections:

    * If the current implied-DO (which may be an inner implied-DO of a 
      nested set) has only a single target (a typical case so it's worth
      the optimization), control remains in this procedure for all of the
      iterations of the current implied-DO.  That is, this routine will 
      continue advancing through the values until the current implied-DO
      runs out of targets.  For example,

           CHARACTER*8  array(4096)
           DATA (array(i), i = 1, 4096)
          &     /1000*'a', 1000*'b', 1000*'c', 1000*'d', 96*' '/

      This procedure will process the entire value list for the implied-DO
      before it returns to interpret_data_imp_do.  This is also true for 
      the inner loop of an example of the form:

           INTEGER  k1(10,10), k2(10)
           DATA ((k1(i,j), i = 1, 10), k2(j), j = 1, 10) /500*0, 500*1/

      That is, 10 values will be processed for K1 then control will return.
      When this procedure is entered again, a single value will be processed
      for K2.  And when the procedure is entered for the third time, another
      10 values will be processed for K1 and so on.

    * Otherwise, the current implied-DO has more than one target.  "More 
      than one target" means a single implied-DO loop has multiple array
      element targets or an implied-DO contains at least one nested 
      implied-DO.  For these cases, this procedure is called once for each
      target (which is why the case above that checks for a single target
      exists:  to reduce the number of calls to this procedure).  

\******************************************************************************/
static void process_data_imp_do_target(int      init_ir_idx,
				       int	target_il_idx,
				       long64	num_iterations)
{
   	   opnd_type	ignore_this_opnd;
   	   int		il_idx;
           boolean	long_value;
   	   int        	ir_idx;
   	   opnd_type	rep_factor_opnd;
   	   int         	target_attr_idx;
           boolean	word_size_target;


   TRACE (Func_Entry, "process_data_imp_do_target", NULL);

   ir_idx = IL_IDX(target_il_idx);

   if (IR_OPR(ir_idx) == Whole_Substring_Opr  ||
       IR_OPR(ir_idx) == Substring_Opr) {
      ir_idx = IR_IDX_L(ir_idx);
   }
  
   /* Section_Subscript_Opr gets into the act if this is a compiler-gen'd     */
   /* implied-DO to represent an array initialization where the array has at  */
   /* one vector valued subscript.					      */
 
   if (IR_OPR(ir_idx) == Subscript_Opr  ||
       IR_OPR(ir_idx) == Section_Subscript_Opr) {
       
      /* Subscript must be pointing at Attr entry or Struct IR.               */

      target_attr_idx = (IR_FLD_L(ir_idx) == AT_Tbl_Idx) ?
                        IR_IDX_L(ir_idx) : IR_IDX_R(IR_IDX_L(ir_idx));
   }
   else {
      target_attr_idx = IR_IDX_R(ir_idx);            /* Must be Struct IR. */
   }

   if (loop_tbl[lt_idx].num_targets == 1) {

      /* Implied-DO contains a single target.  Process all iterations of this */
      /* implied-DO.							      */

      obj_count = num_iterations * loop_tbl[lt_idx].num_targets;

      while (obj_count > 0) {

         if (rep_factor == 0) {
            set_global_value_variables(&rep_factor_opnd, 
                                       &ignore_this_opnd, 
                                       target_attr_idx);

            if (SH_ERR_FLG(curr_stmt_sh_idx)) {
               goto EXIT;
            }

            ++IR_LIST_CNT_R(init_ir_idx);
         }

            word_size_target = FALSE;

            if (TYP_LINEAR(ATD_TYPE_IDX(target_attr_idx)) ==
                   INTEGER_DEFAULT_TYPE                         ||
                TYP_LINEAR(ATD_TYPE_IDX(target_attr_idx)) ==
                   REAL_DEFAULT_TYPE) {

               if (storage_bit_size_tbl[
                      TYP_LINEAR(ATD_TYPE_IDX(target_attr_idx))] ==
                         TARGET_BITS_PER_WORD) {
                  word_size_target = TRUE;
               }
            }

            long_value = FALSE;

            if (value_desc.type == Typeless) {
               if (TYP_BIT_LEN(CN_TYPE_IDX(OPND_IDX(value_opnd))) >
                      TARGET_BITS_PER_WORD) {
                  long_value = TRUE;
               }
            }
            else if (value_desc.type == Character) {

               if (CN_INT_TO_C(TYP_IDX(value_desc.type_idx)) >
                      TARGET_CHARS_PER_WORD) {
                  long_value = TRUE;
               }
            }

            if (word_size_target  &&  long_value) { 
               PRINTMSG(OPND_LINE_NUM(value_opnd), 733, Error,
                        OPND_COL_NUM(value_opnd));

               --IR_LIST_CNT_R(init_ir_idx);


               /* Hollerith constants are always a word multiple in length.   */
               /*  They are padded on the right or on the left with blanks    */
               /* or nulls, depending on the Hollerith specifier.             */

               if (TYP_TYPE(CN_TYPE_IDX(OPND_IDX(value_opnd))) == Typeless) {
                  ls_word_len =
                     TYP_BIT_LEN(CN_TYPE_IDX(OPND_IDX(value_opnd))) /
                     TARGET_BITS_PER_WORD;
               }
               else {
                  ls_word_len =
                     CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(OPND_IDX(value_opnd)))) /
                     TARGET_CHARS_PER_WORD;

                  if ((long)
                      CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(OPND_IDX(value_opnd)))) %
		         TARGET_CHARS_PER_WORD) {
                     ++ls_word_len;
                  }
               }
            }

            check_target_and_value(target_attr_idx, init_ir_idx);

            if (SH_ERR_FLG(curr_stmt_sh_idx)) {
               goto EXIT;
            }

            if (obj_count == rep_factor) {
               obj_count  = 0;
               rep_factor = 0;

               if (IL_FLD(value_il_idx) == CN_Tbl_Idx) {
                  IL_IDX(value_il_idx) = OPND_IDX(value_opnd);
               }
               else {
                  IR_IDX_R(IL_IDX(value_il_idx)) = OPND_IDX(value_opnd);
               }

               value_il_idx = IL_NEXT_LIST_IDX(value_il_idx);
            }
            else if (obj_count > rep_factor) {
               obj_count  -= rep_factor;
               rep_factor  = 0;

               if (IL_FLD(value_il_idx) == CN_Tbl_Idx) {
                  IL_IDX(value_il_idx) = OPND_IDX(value_opnd);
               }
               else {
                  IR_IDX_R(IL_IDX(value_il_idx)) = OPND_IDX(value_opnd);
               }

               value_il_idx = IL_NEXT_LIST_IDX(value_il_idx);

               if (value_il_idx == NULL_IDX) {
                  PRINTMSG(IR_LINE_NUM(init_ir_idx), 667, Error,
                           IR_COL_NUM(init_ir_idx));
                  goto EXIT;
               }
            }
            else {

               /* This case (where there are values left over) is taken care  */
               /* of upon return to data_imp_do_semantics.		      */

               rep_factor -= obj_count;
               obj_count   = 0;
            }
      }
   }
   else {
    
      /* Implied-DO has more than one target.  Process only the current       */
      /* target.							      */

      if (rep_factor == 0) {
         set_global_value_variables(&rep_factor_opnd, 
                                    &ignore_this_opnd, 
                                    target_attr_idx);

         if (SH_ERR_FLG(curr_stmt_sh_idx)) {
            goto EXIT;
         }

         ++IR_LIST_CNT_R(init_ir_idx);
      }

      /* CRI extension:  (same as described above)			      */
      if ((TYP_TYPE(ATD_TYPE_IDX(target_attr_idx)) == Integer  ||
           TYP_LINEAR(ATD_TYPE_IDX(target_attr_idx)) == REAL_DEFAULT_TYPE) &&
           (value_desc.linear_type == Long_Typeless || 
            (CN_HOLLERITH_TYPE(OPND_IDX(value_opnd)) != Not_Hollerith &&
             TYP_BIT_LEN(CN_TYPE_IDX(OPND_IDX(value_opnd))) >
                                                    TARGET_BITS_PER_WORD) ||
           (value_desc.type == Character  &&
            CN_INT_TO_C(TYP_IDX(value_desc.type_idx)) > 
                                          TARGET_CHARS_PER_WORD))){

         PRINTMSG(OPND_LINE_NUM(value_opnd), 
                  733, Error,
                  OPND_COL_NUM(value_opnd));
      }

      check_target_and_value(target_attr_idx, init_ir_idx);

      if (rep_factor == 1) {
         rep_factor   = 0;
         value_il_idx = IL_NEXT_LIST_IDX(value_il_idx);
      }
      else {				            /* rep_factor must be > 1 */
         --rep_factor;
      }
   }

EXIT:

   /* In order to get an IR display, restore the correct value in             */
   /* IR_LIST_CNT_R (because it was set to 1 earlier and in the normal case   */
   /* will be incremented as each value IL is used).			      */

   if (SH_ERR_FLG(curr_stmt_sh_idx)) {
      il_idx = IR_IDX_R(init_ir_idx);
      IR_LIST_CNT_R(init_ir_idx) = 1;

      while (IL_NEXT_LIST_IDX(il_idx) != NULL_IDX) {
         il_idx = IL_NEXT_LIST_IDX(il_idx);
         ++IR_LIST_CNT_R(init_ir_idx);
      }
   }

   TRACE (Func_Exit, "process_data_imp_do_target", NULL);

   return;

}  /* process_data_imp_do_target */
#ifdef KEY /* Bug 2949 and Bug 5295 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      If target is type character and source is "H" hollerith, convert      *|
|*      source to character string. The goal is to be compatible in certain   *|
|*      contexts (assignment whose rhs is a single hollerith; parameter       *|
|*      statement; data statement) with g77. G77 doesn't provide "R" or "L"   *|
|*      holleriths, so we don't need to handle them.                          *|
|*
|*      Also allow an integer to initialize a single character unless -ansi   *|
|*      is in effect                                                          *|
|*
|*                                                                            *|
|* Input parameters:                                                          *|
|*	target_idx: linear type of target                                     *|
|*      source_idx: index into const_tbl for source constant                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      source_idx: (possibly changed) index into const_tbl                   *|
|*      NONE								      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if we performed the conversion                                   *|
|*                                                                            *|
\******************************************************************************/
static boolean force_hollerith_or_int_to_character(linear_type_type target_idx,
  int *source_idx)
{
  if (CHARACTER_DEFAULT_TYPE != target_idx) {
    return FALSE;
    }

  int type_byte_len = 1;
  boolean hollerith = FALSE;
  int orig_source_idx = *source_idx;
  basic_type_type orig_source_type = TYP_TYPE(CN_TYPE_IDX(orig_source_idx));

  /* We assume that the existing -ansi "Hollerith is nonstandard" error
   * message is sufficient to indicate that the assignment is also nonstandard,
   * so we don't have to test for ansi-ness here
   */
  if (H_Hollerith == CN_HOLLERITH_TYPE(orig_source_idx)) {
    // See print_const() in fortout.c for this computation
    type_byte_len = TARGET_CHARS_PER_WORD *
      ((TYP_BIT_LEN(CN_TYPE_IDX(orig_source_idx)) + TARGET_BITS_PER_WORD - 1) /
	TARGET_BITS_PER_WORD);
    CN_HOLLERITH_TYPE(orig_source_idx) = Not_Hollerith;
    CN_HOLLERITH_ENDIAN(orig_source_idx) = FALSE;
    hollerith = TRUE;
    }

  /* z'nnn' literals show up as "typeless" */
  else if ((!on_off_flags.issue_ansi_messages) &&
    (orig_source_type == Integer || orig_source_type == Typeless)) {
    /* Done */
    }

  else {
    return FALSE;
    }

  /* See put_c_str_in_cn() in s_utils.c */
  CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
  TYP_TYPE(TYP_WORK_IDX) = Character;
  TYP_LINEAR(TYP_WORK_IDX) = CHARACTER_DEFAULT_TYPE;
  TYP_DESC(TYP_WORK_IDX) = Default_Typed;
  TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
  TYP_FLD(TYP_WORK_IDX) = CN_Tbl_Idx;
  TYP_IDX(TYP_WORK_IDX) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
    type_byte_len);
  int type_index = ntr_type_tbl();

  /* Integer constants are shared, so we must duplicate it before changing
   * it's type. That appears not to be a problem with holleriths. */
  if (hollerith) {
    CN_TYPE_IDX(orig_source_idx) = type_index;
    }
  else {
    *source_idx = ntr_const_tbl(type_index, FALSE,
      &(CN_CONST(orig_source_idx)));
    }

  return TRUE;
}
#endif /* KEY Bug 2949 and Bug 5295 */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Verify that the target and value are assignment compatible.  If the   *|
|*      target is type character and the value is shorter than the target,    *|
|*      calculate the number of padding blanks needed.			      *|
|*									      *|
|*      This routine is extern'd (in globals.h) so that the implied-DO code   *|
|*      in the PDGCS interface can call it.				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      attr_idx : the target's Attr table index			      *|
|*      init_ir_idx : index into ir_tbl for initialization value              *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE								      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if the target and value are compatible.			      *|
|*                                                                            *|
\******************************************************************************/

static boolean check_target_and_value(int	attr_idx,
                                      int       init_ir_idx)

{
   long_type    another_constant[MAX_WORDS_FOR_NUMERIC];
   int		column;
   int		line;
   boolean	result 			= TRUE;
   int		type_idx;


   TRACE (Func_Entry, "check_target_and_value", NULL);

   if (value_desc.linear_type == Long_Typeless) { 
      PRINTMSG(OPND_LINE_NUM(value_opnd), 1133, Error,
               OPND_COL_NUM(value_opnd));
      result = FALSE;
      goto EXIT;
   }

#ifdef KEY /* Bug 2949 and Bug 5295 */
   int source_idx = OPND_IDX(value_opnd);
#endif /* KEY Bug 2949 and Bug 5295 */
   if (check_asg_semantics(ATD_TYPE_IDX(attr_idx),
                           value_desc.type_idx,
                           OPND_LINE_NUM(value_opnd),
                           OPND_COL_NUM(value_opnd))) {

      if ((ATD_POINTER(attr_idx) &&
           (OPND_FLD(value_opnd) != IR_Tbl_Idx ||
            IR_OPR(OPND_IDX(value_opnd)) != Null_Intrinsic_Opr)) ||

          (!ATD_POINTER(attr_idx) &&
           OPND_FLD(value_opnd) == IR_Tbl_Idx &&
           IR_OPR(OPND_IDX(value_opnd)) == Null_Intrinsic_Opr)) {
         find_opnd_line_and_column(&value_opnd, &line, &column);
         PRINTMSG(line, 1559, Error, column);
      }

      if (ATD_POINTER(attr_idx) &&
          OPND_FLD(value_opnd) == IR_Tbl_Idx &&
          IR_OPR(OPND_IDX(value_opnd)) == Null_Intrinsic_Opr) {
         IR_OPR(init_ir_idx) = Null_Opr;
      }

      if (CN_BOZ_CONSTANT(OPND_IDX(value_opnd))  &&
          TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Integer) {

         PRINTMSG(OPND_LINE_NUM(value_opnd), 729, Ansi,
                  OPND_COL_NUM(value_opnd), 
                  AT_OBJ_NAME_PTR(attr_idx));
      }

      if ((TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Character  &&  
           value_desc.type == Character) ||
          value_desc.linear_type == Short_Typeless_Const) {

         /* Cast the typeless or character (boolean) constant to the */
         /* type of the target.                                      */

         OPND_IDX(value_opnd) = cast_typeless_constant(OPND_IDX(value_opnd),
                                                    ATD_TYPE_IDX(attr_idx),
                                                    OPND_LINE_NUM(value_opnd),
                                                    OPND_COL_NUM(value_opnd));
         value_desc.type        = TYP_TYPE(ATD_TYPE_IDX(attr_idx));
         value_desc.type_idx    = ATD_TYPE_IDX(attr_idx);
         value_desc.linear_type = TYP_LINEAR(ATD_TYPE_IDX(attr_idx));
      }
      else if (TYP_LINEAR(value_desc.type_idx) != 
                              TYP_LINEAR(ATD_TYPE_IDX(attr_idx))   &&
               TYP_TYPE(value_desc.type_idx) != Character          &&
               TYP_TYPE(value_desc.type_idx) != Typeless           &&
               TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != CRI_Ptr         &&
               TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != CRI_Parcel_Ptr  &&
               TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != CRI_Ch_Ptr) {

         /* PDGCS does not like it if the value is not the same size as the   */
         /* target; for example, the value is a double precision constant and */
         /* the target is a single precision variable.  So explicitly convert */
         /* the value to the type and kind type parameter of the target for   */
         /* all combinations to be consistent.				      */

         type_idx = ATD_TYPE_IDX(attr_idx);

         if (folder_driver( (char *) &CN_CONST(OPND_IDX(value_opnd)),
                            value_desc.type_idx,
                            NULL,
                            NULL_IDX,
                            another_constant,
                           &type_idx,
                            OPND_LINE_NUM(value_opnd),
                            OPND_COL_NUM(value_opnd),
                            1,
                            Cvrt_Opr)) {

            value_desc.type_idx    = type_idx;
            value_desc.linear_type = TYP_LINEAR(type_idx);
            value_desc.type        = TYP_TYPE(type_idx);
            OPND_IDX(value_opnd)   = ntr_const_tbl(ATD_TYPE_IDX(attr_idx),
                                                   FALSE,
                                                   another_constant);
         }
      }
   }
#ifdef KEY /* Bug 2949 and Bug 5295 */
   else if (force_hollerith_or_int_to_character(
     TYP_LINEAR(ATD_TYPE_IDX(attr_idx)), &source_idx)) {
     OPND_IDX(value_opnd) = source_idx;
   }
#endif /* KEY Bug 2949 and Bug 5295 */
   else {
      find_opnd_line_and_column(&value_opnd, &line, &column);
      PRINTMSG(line, 97, Error, column, AT_OBJ_NAME_PTR(attr_idx));
      result = FALSE;
   }

EXIT:

   TRACE (Func_Exit, "check_target_and_value", NULL);

   return(result);

}  /* check_target_and_value" */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Make sure the character-valued initialization value is the same       *|
|*	length as the target.  If necessary, generate a secondary DATA stmt   *|
|* 	to blank pad the initialization value.  			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      init_ir_idx         : the index of the Init IR		     	      *|
|*      array_ir_idx	    : contains an IR index only if the target is an   *|
|* 			      array					      *|
|*      section_start_value : if the target is a section, this is the current *|
|*                            value of the first section triplet subscript    *|
|*      section_inc_value   : if the target is a section, this is inc value   *|
|*                            in the first section triplet subscript          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

static void adjust_char_value_len(int	 	init_ir_idx,
				  int  	        array_ir_idx,
				  long64 	section_start_value,
        			  long64	section_inc_value)
{

   int		end_il_idx;
   long64	i;
   int		il_idx;
   int		imp_do_ir_idx;
   int		inc_il_idx;
   int		ir_idx;
   int		new_init_ir_idx;
   int		new_str_idx;
   char	       *new_str_ptr;
   long64	numeric_value;
   char	       *old_str_ptr;
   opnd_type	opnd;
   int		original_end_il_idx;
   long64	original_end_val;
   long64	original_start_val;
   long64	rep_count;
   int		rep_count_il_idx;
   int     	rep_count_ir_idx;
   int		start_il_idx;
   int		substring_ir_idx;
   long64	target_length;
#ifdef KEY /* Bug 10177 */
   int		temp_idx = 0;
#else /* KEY Bug 10177 */
   int		temp_idx;
#endif /* KEY Bug 10177 */
   int		type_idx;
   int		value_idx;
   long64	value_length;


   /* -------------------  Problematic code warning  ------------------------ */
   /*									      */
   /* By design, each entry in the Constant Table must start on a word        */
   /* boundary.  The following declaration pushes the single character        */
   /* value to the LEFT end of the word.  Some architectures may prefer it    */
   /* to be on the right end so someone may later have to ifdef the following */
   /* declaration to work on those other architectures.	       LRR    	      */
   /*									      */
   /* -------------------  Problematic code warning  ------------------------ */


  
# if defined(_HOST_LITTLE_ENDIAN)
   long_type    single_blank    = (long_type)' ';
#else
   long_type	single_blank	= (long_type)' ' << 
                                       (sizeof(long_type)*CHAR_BIT - CHAR_BIT);
# endif


   TRACE (Func_Entry, "adjust_char_value_len", NULL);

   substring_ir_idx    = IR_IDX_L(init_ir_idx);
   il_idx              = IR_IDX_R(substring_ir_idx);
   original_start_val  = CN_INT_TO_C(IL_IDX(il_idx));
   original_end_il_idx = IL_NEXT_LIST_IDX(il_idx);
   original_end_val    = CN_INT_TO_C(IL_IDX(original_end_il_idx));
   target_length       = original_end_val - original_start_val + 1;

   if (target_length > 0) {
      value_idx    = IL_IDX(IR_IDX_R(init_ir_idx));
      value_length = CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(value_idx)));

      if (target_length == value_length) {
         goto EXIT;
      }

      if (target_length < value_length) {

         /* Need to replace the initialization value with a shorter length    */
         /* string to match the length of the target.			      */
         
         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)       = Character;
         TYP_LINEAR(TYP_WORK_IDX)     = CHARACTER_DEFAULT_TYPE;
         TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)        = CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX)        = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                    target_length);
         type_idx                     = ntr_type_tbl();

         /* Call ntr_const_tbl with NULL for the incoming constant so it will */
         /* just allocate the amount of space needed.  Copy the original      */
         /* string to the new string by hand.				      */
         /* Replace the value's Constant table index with the Constant table  */
         /* index of the shortened string.   			      	      */

         new_str_idx = ntr_const_tbl(type_idx, TRUE, NULL);
         new_str_ptr = (char *) &CN_CONST(new_str_idx);  /* KAYKAY */
         old_str_ptr = (char *) &CN_CONST(value_idx);

         for (i = 0;  i < target_length;  i++) {
            new_str_ptr[i] = old_str_ptr[i];
         }

         while (target_length % TARGET_BYTES_PER_WORD != 0) {
            new_str_ptr[target_length] = ' ';
            target_length++;
         }

         IL_IDX(IR_IDX_R(init_ir_idx)) = new_str_idx;
      }
      else {

         /* Future optimizations:					      */
         /*  - If the target is a scalar and the value is "small", reenter    */
         /*    the constant with blank padding rather than generating the     */
         /*    implied-DO to do the blank padding.			      */
         /*  - If the value is a zero-length constant, no tree duplication    */
         /*    needs to be done.				              */
         /*								      */
         /* Must do blank padding.  There are two ways to do this:            */
         /*   (a) generate a new value (char constant) that is the same       */
         /*       length as the target and is blank padded, or                */
         /*   (b) generate an implied-DO to do the blank padding.  For the    */
         /*       array case, the implied-DO makes a second pass through the  */
         /*       array (after the value is assigned to each element via      */
         /*       whole array or section initialization) adding the blanks.   */
         /* It is a design trade-off for array initialization between eating  */
         /* up time by running through an implied-DO in the interface vs.     */
         /* potentially eating up a lot of CN table space if a number of new  */
         /* character constants need to be built.  The "if" stmt below        */
         /* controls when we'll switch from the implied-DO method to the new  */
         /* constant method.  As it stands now, there is both an array element*/
         /* count threshhold and an array element length threshhold.  As we   */
         /* learn more about trade-offs between the implied-DO method and the */
         /* new constant method, the threshholds can easily be changed.  They */
         /* are currently set to switch to the new constant method if at      */
         /* least 100 array elements are being initialized where the length   */
         /* of each element is no more than 256 characters.  The numbers were */
         /* essentially chosen at random but with an eye to performance on    */
         /* both Crays and workstations. 				      */
      
         rep_count_il_idx = IL_NEXT_LIST_IDX(IR_IDX_R(init_ir_idx));
         rep_count        = CN_INT_TO_C(IL_IDX(rep_count_il_idx));

         if (array_ir_idx != NULL_IDX  &&  rep_count >= 100  &&
             target_length <= 256) {

            CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
            TYP_TYPE(TYP_WORK_IDX)      = Character;
            TYP_LINEAR(TYP_WORK_IDX)    = CHARACTER_DEFAULT_TYPE;
            TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
            TYP_FLD(TYP_WORK_IDX)       = CN_Tbl_Idx;
            TYP_IDX(TYP_WORK_IDX)       = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      target_length);
            type_idx                    = ntr_type_tbl();

            /* Call ntr_const_tbl with NULL for the incoming constant so it   */
            /* will just allocate the amount of space needed.  Copy the       */
            /* original string to the new string by hand and blank pad the    */
            /* new string by hand.					      */
            /* Replace the value's Constant table index with the Constant     */
            /* table index of the padded string.			      */

            new_str_idx = ntr_const_tbl(type_idx, TRUE, NULL);
            new_str_ptr = (char *) &CN_CONST(new_str_idx);
            old_str_ptr = (char *) &CN_CONST(value_idx);

            for (i = 0;  i < value_length;  i++) {
               new_str_ptr[i] = old_str_ptr[i];
            }

            for (i = value_length;  i < target_length;  i++) {
               new_str_ptr[i] = ' ';
            }

            while (target_length % TARGET_BYTES_PER_WORD != 0) {
               new_str_ptr[target_length] = ' ';
               target_length++;
            }

            IL_IDX(IR_IDX_R(init_ir_idx)) = new_str_idx;
         }
         else {

            /* Use the implied-DO method.  				      */
            /*   - Generate a DATA stmt SH following the current SH.	      */
            /*   - Change the substring operator to Substring_Opr in case it  */
            /*     was originally Whole_Substring_Opr.			      */
            /*   - If a single target is being initialized, the original      */
            /*     initialization IR tree can be duplicated and altered to do */
            /*     blank padding.  Otherwise, the padding must be done by     */
            /*     generating implied-DO IR.				      */

            gen_sh(After, Data_Stmt,
                   IR_LINE_NUM(init_ir_idx), IR_COL_NUM(init_ir_idx),
                   FALSE, FALSE, TRUE);

            if (rep_count == 1) {
               gen_opnd(&opnd, init_ir_idx, IR_Tbl_Idx, 
                        IR_LINE_NUM(init_ir_idx),
                        IR_COL_NUM(init_ir_idx));

               copy_subtree(&opnd, &opnd);
               new_init_ir_idx = OPND_IDX(opnd);
               SH_IR_IDX(curr_stmt_sh_idx) = new_init_ir_idx;

               substring_ir_idx            = IR_IDX_L(new_init_ir_idx);
               IR_OPR(substring_ir_idx)    = Substring_Opr;

               /* In the new tree, replace the value's Constant table index   */
               /* with the Constant table index of a single blank.	      */

               il_idx         = IR_IDX_R(new_init_ir_idx);
               IL_IDX(il_idx) = ntr_const_tbl(CHARACTER_DEFAULT_TYPE,
					      FALSE,
		                              (long_type *) &single_blank);

               /* In the new tree, update the rep count.		      */

               il_idx         = IL_NEXT_LIST_IDX(il_idx);
               numeric_value  = target_length - value_length;
               IL_IDX(il_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
     			                    numeric_value);

               /* In the new tree, set the stride to 8 (bits).		      */

               il_idx         = IL_NEXT_LIST_IDX(il_idx);
               numeric_value  = 8;
               IL_IDX(il_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                               	            numeric_value);

               /* Pick up common processing below the implied-DO code.        */
            }
            else {

               /* Generate a new Init IR and set up the value list attached   */
               /* to it to represent the blank padding.  The value list is a  */
               /* single IL.  The IL points at a Rep_Count IR whose left      */
               /* operand is the number of array elements to initialize.      */
	       /* Its right operand points at another Rep_Count IR that       */
	       /* represents the number of padding blanks needed.	      */

               NTR_IR_TBL(new_init_ir_idx);
               SH_IR_IDX(curr_stmt_sh_idx)  = new_init_ir_idx;
               IR_OPR(new_init_ir_idx)      = Init_Opr;
               IR_TYPE_IDX(new_init_ir_idx) = TYPELESS_DEFAULT_TYPE;
               IR_LINE_NUM(new_init_ir_idx) = IR_LINE_NUM(init_ir_idx);
               IR_COL_NUM(new_init_ir_idx)  = IR_COL_NUM(init_ir_idx);

               NTR_IR_LIST_TBL(il_idx);
               IR_LIST_CNT_R(new_init_ir_idx) = 1;
               IR_FLD_R(new_init_ir_idx)      = IL_Tbl_Idx;
               IR_IDX_R(new_init_ir_idx)      = il_idx;

               NTR_IR_TBL(rep_count_ir_idx);
               IL_FLD(il_idx)                = IR_Tbl_Idx;
               IL_IDX(il_idx)                = rep_count_ir_idx;
               IR_OPR(rep_count_ir_idx)      = Rep_Count_Opr;
               IR_TYPE_IDX(rep_count_ir_idx) = TYPELESS_DEFAULT_TYPE;
               IR_LINE_NUM(rep_count_ir_idx) = IR_LINE_NUM(init_ir_idx);
               IR_COL_NUM(rep_count_ir_idx)  = IR_COL_NUM(init_ir_idx);

               COPY_OPND(IR_OPND_L(rep_count_ir_idx),
                         IL_OPND(rep_count_il_idx));
   
               NTR_IR_TBL(ir_idx);
               IR_FLD_R(rep_count_ir_idx) = IR_Tbl_Idx;
               IR_IDX_R(rep_count_ir_idx) = ir_idx;
               IR_OPR(ir_idx) 	          = Rep_Count_Opr;
               IR_TYPE_IDX(ir_idx)        = TYPELESS_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx)	  = IR_LINE_NUM(init_ir_idx);
               IR_COL_NUM(ir_idx)	  = IR_COL_NUM(init_ir_idx);

               IR_FLD_L(ir_idx)	  = CN_Tbl_Idx;
               numeric_value	  = target_length - value_length;
               IR_IDX_L(ir_idx)	  = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
              			                numeric_value);
               IR_LINE_NUM_L(ir_idx)	  = IR_LINE_NUM(init_ir_idx);
               IR_COL_NUM_L(ir_idx)	  = IR_COL_NUM(init_ir_idx);

               IR_FLD_R(ir_idx) = CN_Tbl_Idx;
               IR_IDX_R(ir_idx) = ntr_const_tbl(CHARACTER_DEFAULT_TYPE,
					        FALSE,
                                                (long_type *) &single_blank);
               IR_LINE_NUM_R(ir_idx)	  = IR_LINE_NUM(init_ir_idx);
               IR_COL_NUM_R(ir_idx)	  = IR_COL_NUM(init_ir_idx);

               /* Generate an implied-DO to loop through the target array     */
               /* elements.						      */

               NTR_IR_TBL(imp_do_ir_idx);
               IR_FLD_L(new_init_ir_idx)    = IR_Tbl_Idx;
               IR_IDX_L(new_init_ir_idx)    = imp_do_ir_idx;
               IR_OPR(imp_do_ir_idx)        = Implied_Do_Opr;
               IR_TYPE_IDX(imp_do_ir_idx)   = TYPELESS_DEFAULT_TYPE;
               IR_LINE_NUM(imp_do_ir_idx)   = IR_LINE_NUM(init_ir_idx);
               IR_COL_NUM(imp_do_ir_idx)    = IR_COL_NUM(init_ir_idx);

               /* The 4 IL's attached to the right opnd of the implied-DO IR  */
               /* are:							      */
               /*    - implied-DO variable : a temp			      */
               /*    - start value         : the value from the IL attached to*/
               /*                            the Subscript IR in the original */
               /*                            tree			      */
               /*    - end value           : start value + rep count (of array*/
               /*                            elements) - 1		      */
               /*    - increment value     : 1				      */

               temp_idx = gen_compiler_tmp(IR_LINE_NUM(init_ir_idx),
                                           IR_COL_NUM(init_ir_idx),
                                           Priv, TRUE);
               AT_SEMANTICS_DONE(temp_idx) = TRUE;
               ATD_TYPE_IDX(temp_idx)	   = INTEGER_DEFAULT_TYPE;
               ATD_STOR_BLK_IDX(temp_idx)  = SCP_SB_STACK_IDX(curr_scp_idx);
               ATD_LCV_IS_CONST(temp_idx)  = TRUE;

               /* clear the referenced field so that this tmp does */
               /* not get sent to mif. BHJ                         */

               AT_REFERENCED(temp_idx)     = Not_Referenced;


               NTR_IR_LIST_TBL(il_idx);
               IR_LIST_CNT_R(imp_do_ir_idx) = 1;
               IR_FLD_R(imp_do_ir_idx)      = IL_Tbl_Idx;
               IR_IDX_R(imp_do_ir_idx)      = il_idx;
               IL_FLD(il_idx)               = AT_Tbl_Idx;
               IL_IDX(il_idx)               = temp_idx;
               IL_LINE_NUM(il_idx)          = stmt_start_line;
               IL_COL_NUM(il_idx)           = stmt_start_col;

               /* Produce the ILs that will hold the implied-DO start, end,   */
               /* and increment values.  They get filled differently depending*/
               /* on whether the implied-DO is being generated due to a whole */
               /* array or a section initialization.			      */

               NTR_IR_LIST_TBL(start_il_idx);
               IR_LIST_CNT_R(imp_do_ir_idx)   = 2;
               IL_NEXT_LIST_IDX(il_idx)       = start_il_idx;
               IL_PREV_LIST_IDX(start_il_idx) = il_idx;

               NTR_IR_LIST_TBL(end_il_idx);
               IR_LIST_CNT_R(imp_do_ir_idx)   = 3;
               IL_NEXT_LIST_IDX(start_il_idx) = end_il_idx;
               IL_PREV_LIST_IDX(end_il_idx)   = start_il_idx;

               NTR_IR_LIST_TBL(inc_il_idx);
               IR_LIST_CNT_R(imp_do_ir_idx) = 4;
               IL_NEXT_LIST_IDX(end_il_idx) = inc_il_idx;
               IL_PREV_LIST_IDX(inc_il_idx) = end_il_idx;

               if (section_start_value == 0) {
                  ir_idx = IR_IDX_L(substring_ir_idx);

                  while (IR_OPR(ir_idx) != Subscript_Opr) {
                     ir_idx = IR_IDX_L(ir_idx);
                  }

                  COPY_OPND(IL_OPND(start_il_idx), IL_OPND(IR_IDX_R(ir_idx)));

                  numeric_value = CN_INT_TO_C(IL_IDX(start_il_idx))+rep_count-1;
            
                  IL_FLD(end_il_idx) = CN_Tbl_Idx;
                  IL_IDX(end_il_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   numeric_value);
                  IL_LINE_NUM(end_il_idx)          = stmt_start_line;
                  IL_COL_NUM(end_il_idx)           = stmt_start_col;

                  IL_FLD(inc_il_idx) = CN_Tbl_Idx;
                  IL_IDX(inc_il_idx) = CN_INTEGER_ONE_IDX;
                  IL_LINE_NUM(inc_il_idx)          = stmt_start_line;
                  IL_COL_NUM(inc_il_idx)           = stmt_start_col;
               }
               else {

                  /* We're processing a section reference.  		      */
                  /* section_start_value and section_inc_value are used for   */
                  /* the loop start and inc values.  The loop end value is    */
                  /* calculated.		      			      */

                  IL_FLD(start_il_idx) = CN_Tbl_Idx;
                  IL_IDX(start_il_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                     section_start_value);
                  IL_LINE_NUM(start_il_idx)          = stmt_start_line;
                  IL_COL_NUM(start_il_idx)           = stmt_start_col;

                  numeric_value =
                     section_start_value + (rep_count - 1)*section_inc_value;

                  IL_FLD(end_il_idx) = CN_Tbl_Idx;
                  IL_IDX(end_il_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   numeric_value);
                  IL_LINE_NUM(end_il_idx)          = stmt_start_line;
                  IL_COL_NUM(end_il_idx)           = stmt_start_col;

                  IL_FLD(inc_il_idx) = CN_Tbl_Idx;
                  IL_IDX(inc_il_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   section_inc_value);
                  IL_LINE_NUM(inc_il_idx)          = stmt_start_line;
                  IL_COL_NUM(inc_il_idx)           = stmt_start_col;
               }

               /* Make a copy of the original reference IR tree and attach it */
               /* to the left operand of the implied-DO IR via an IL.	      */
    
               NTR_IR_LIST_TBL(il_idx);
               IR_LIST_CNT_L(imp_do_ir_idx) = 1;
               IR_FLD_L(imp_do_ir_idx)      = IL_Tbl_Idx;
               IR_IDX_L(imp_do_ir_idx)      = il_idx;

               copy_subtree(&IR_OPND_L(init_ir_idx), &opnd);
               COPY_OPND(IL_OPND(il_idx), opnd);

               substring_ir_idx         = IL_IDX(il_idx); 
               IR_OPR(substring_ir_idx) = Substring_Opr;
            }
         
            IR_OPR(IR_IDX_L(init_ir_idx)) = Substring_Opr;
  
            /* In the original tree, replace the substring end value.         */
  
            numeric_value               = original_start_val + value_length - 1;
            IL_IDX(original_end_il_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
						      numeric_value); 

            /* In the original tree, update the substring length to be the    */
            /* length of the initialization value.			      */

            il_idx         = IL_NEXT_LIST_IDX(original_end_il_idx);
            IL_IDX(il_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
        			         value_length);

            /* In the new tree, update the substring start value, end value   */
            /* (end value == start value), and length.			      */

            ++numeric_value;
  
            il_idx         = IR_IDX_R(substring_ir_idx);
            IL_IDX(il_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
				         numeric_value);

            il_idx         = IL_NEXT_LIST_IDX(il_idx);
            IL_IDX(il_idx) = IL_IDX(IL_PREV_LIST_IDX(il_idx));

            il_idx         = IL_NEXT_LIST_IDX(il_idx);
            IL_IDX(il_idx) = CN_INTEGER_ONE_IDX;
 
            if (rep_count > 1) {

               /* An implied-DO IR was generated.  Change the target's        */
               /* subscript to point at the implied-DO temp.		      */

               ir_idx = IR_IDX_L(substring_ir_idx);
  
               while (IR_OPR(ir_idx) != Subscript_Opr) {
                  ir_idx = IR_IDX_L(ir_idx);
               }
  
               IL_FLD(IR_IDX_R(ir_idx)) = AT_Tbl_Idx;
               IL_IDX(IR_IDX_R(ir_idx)) = temp_idx;
               IL_LINE_NUM(IR_IDX_R(ir_idx)) = stmt_start_line;
               IL_COL_NUM(IR_IDX_R(ir_idx)) = stmt_start_col;
               
            }
         }
      }
   }
   else {

      /* Future optimization:  If the target is zero-length, can we just eat  */
      /* the current value and eliminate the DATA SH and IR?	              */

   }

EXIT:

   TRACE (Func_Exit, "adjust_char_value_len", NULL);

   return;

}  /* adjust_char_value_len */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Climb the DATA target reference tree to find all subscript expressions*|
|*      that were too complicated for expr_semantics to fold.  Fold them now. *|
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

static void fold_all_subscripts(opnd_type	*opnd)
{
   int			attr_idx;
   expr_arg_type	expr_desc;
   int			i;
   int			il_idx;
   int			ir_idx;
   opnd_type		local_opnd;
   opnd_type		my_opnd;

   
   TRACE (Func_Entry, "fold_all_subscripts", NULL);

   COPY_OPND(local_opnd, (*opnd));

   expr_desc             = init_exp_desc;
   expr_desc.type        = Integer;
   expr_desc.type_idx    = INTEGER_DEFAULT_TYPE;
   expr_desc.linear_type = INTEGER_DEFAULT_TYPE;

   while (OPND_FLD(local_opnd) == IR_Tbl_Idx) {

      ir_idx = OPND_IDX(local_opnd);

      switch (IR_OPR(ir_idx)) {

         case Subscript_Opr:
        
            attr_idx = (IR_FLD_L(ir_idx) == AT_Tbl_Idx) ?
                          IR_IDX_L(ir_idx) :
                          IR_IDX_R(IR_IDX_L(ir_idx));

            il_idx = IR_IDX_R(ir_idx);

            for (i = 1;  i <= IR_LIST_CNT_R(ir_idx);  ++i) {

# ifdef _F_MINUS_MINUS
               if (IL_PE_SUBSCRIPT(il_idx)) {
                  continue;
               }
# endif

               if (IL_FLD(il_idx) == IR_Tbl_Idx) {
                  COPY_OPND(my_opnd, IL_OPND(il_idx));
                  fold_all_subscripts(&my_opnd);

                  if (IL_FLD(il_idx) == IR_Tbl_Idx) {

                     if (fold_aggragate_expression(&my_opnd,
          					   &expr_desc,
						    TRUE)) {
                        COPY_OPND(IL_OPND(il_idx), my_opnd);
                     }
                     else {
                        PRINTMSG(IR_LINE_NUM(IL_IDX(il_idx)),
                                 861,
                                 Internal,
                                 IR_COL_NUM(IL_IDX(il_idx)),
                                 "object semantics");
                     }
                  }
               }

               if (fold_relationals(IL_IDX(il_idx),
                                    BD_LB_IDX(ATD_ARRAY_IDX(attr_idx), i),
                                    Lt_Opr)) {
                  PRINTMSG(IL_LINE_NUM(il_idx),
                           831,
                           Error,
                           IL_COL_NUM(il_idx));
               }

               if (fold_relationals(IL_IDX(il_idx),
                                    BD_UB_IDX(ATD_ARRAY_IDX(attr_idx), i),
                                    Gt_Opr)) {
                  PRINTMSG(IL_LINE_NUM(il_idx),
                           996,
                           Error,
                           IL_COL_NUM(il_idx));
               }

               il_idx = IL_NEXT_LIST_IDX(il_idx);
            }
   
            COPY_OPND(local_opnd, IR_OPND_L(ir_idx));
         
            break;

         case Struct_Opr:
         case Whole_Substring_Opr:
         case Substring_Opr:
            COPY_OPND(local_opnd, IR_OPND_L(ir_idx));
            break;

         default:
            goto EXIT;
      }
   }

EXIT:

   TRACE (Func_Exit, "fold_all_subscripts", NULL);

   return;

}  /* fold_all_subscripts */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	When a long hollerith is broken up between data targets, the rest of  *|
|*      the constant is entered back into the constant table. It must be      *|
|*      entered as hollerith, so this wrapper routine is needed.              *|
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


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure performs semantic analysis on the data-stmt-repeat     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      repeat_ir_idx	-> IR index of parsed repeat.			      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void	data_repeat_semantics(int	repeat_ir_idx)

{
   int			column;
   expr_arg_type	expr_desc;
   int			line;
   int			ok		= TRUE;
   opnd_type		opnd;
   int			save_attr	= NULL_IDX;
   expr_mode_type	save_expr_mode	= expr_mode;


   TRACE (Func_Entry, "data_repeat_semantics", NULL);

   COPY_OPND(opnd, IR_OPND_L(repeat_ir_idx));

   expr_desc.rank	= 0;
   xref_state		= CIF_Symbol_Reference;

   /* Any subscripts must be initialization expressions */

   expr_mode		= Initialization_Expr;

   switch (OPND_FLD(opnd)) {

   case IR_Tbl_Idx:

      if (IR_OPR(OPND_IDX(opnd)) == Paren_Opr) {

         if (IR_FLD_L(OPND_IDX(opnd)) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(IR_IDX_L(OPND_IDX(opnd))) == Data_Obj) {
            save_attr			 = IR_IDX_L(OPND_IDX(opnd));
            ATD_PARENT_OBJECT(save_attr) = TRUE;
         }
      }
      break;

   case AT_Tbl_Idx:

      if (AT_OBJ_CLASS(OPND_IDX(opnd)) == Data_Obj) {
         save_attr			= OPND_IDX(opnd);
         ATD_PARENT_OBJECT(save_attr)	= TRUE;
      }
   }


   if (expr_semantics(&opnd, &expr_desc)) {
      find_opnd_line_and_column(&opnd, &line, &column);

      /* If the rep factor is represented by an Attr table entry, */
      /* the only thing it can be is a scalar named constant.     */

      switch (OPND_FLD(opnd)) {
      case CN_Tbl_Idx:    /* Great - ok */
         break;

      case AT_Tbl_Idx:
         ok = FALSE;
         PRINTMSG(line, 677, Error, column); /* Must be a constant */
         break;

      default:
         ok = FALSE;
         PRINTMSG(line, 678, Error, column); /* Must be a int scalar constant */
         break;
      }

      if (!ok) {

         /* Intentionally blank */
     
      }
      else if (expr_desc.type != Integer && expr_desc.type != Typeless) {
         PRINTMSG(line, 678, Error, column);
      }
      else if (expr_desc.linear_type == Long_Typeless) {
         PRINTMSG(line, 1133, Error, column);
      }
      else if (expr_desc.rank > 0) {
         PRINTMSG(line, 678, Error, column);
      }
      else if (expr_desc.linear_type == Short_Typeless_Const) {
         OPND_IDX(opnd) = cast_typeless_constant(OPND_IDX(opnd),
                                                 INTEGER_DEFAULT_TYPE,
                                                 line,
                                                 column);
      }
   }

   COPY_OPND(IR_OPND_L(repeat_ir_idx), opnd);

   expr_mode = save_expr_mode;

   if (save_attr != NULL_IDX) {
      ATD_PARENT_OBJECT(save_attr)	= FALSE;
   }

   TRACE (Func_Exit, "data_repeat_semantics", NULL);

   return;

}  /* data_repeat_semantics */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure performs semantic analysis on the data-stmt-constant   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      const_ir_idx	-> IR index of parsed constant.			      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void	constant_value_semantics(opnd_type	*opnd,
				 int		 uopr_ir_idx)

{
#ifdef KEY /* Bug 10177 */
   int                  boz_const_col_num = 0;
#else /* KEY Bug 10177 */
   int                  boz_const_col_num;
#endif /* KEY Bug 10177 */
   int                  boz_const_line_num      = 0;
   int			column;
   expr_arg_type	expr_desc;
   boolean		have_null		= FALSE;
   int			line;
   int			save_attr		= NULL_IDX;
   expr_mode_type	save_expr_mode		= expr_mode;


   TRACE (Func_Entry, "constant_value_semantics", NULL);

   switch (OPND_FLD((*opnd))) {
   case IR_Tbl_Idx:
      find_opnd_line_and_column(opnd, &line, &column);

      if (IR_OPR(OPND_IDX((*opnd))) == Call_Opr &&
          AT_IS_INTRIN(IR_IDX_L(OPND_IDX((*opnd)))) &&
          strcmp(AT_OBJ_NAME_PTR(IR_IDX_L(OPND_IDX((*opnd)))), "NULL") == 0) {
         have_null = TRUE;

         if (IR_IDX_R(OPND_IDX((*opnd))) != NULL_IDX) { /* MOLD is specified */
            PRINTMSG(line, 1573, Error, column);
            IR_OPND_R(OPND_IDX((*opnd))) = null_opnd;  /* Clear the argument */
         }
      }

      if (IR_OPR(OPND_IDX((*opnd))) == Paren_Opr) {

         if (IR_FLD_L(OPND_IDX((*opnd))) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(IR_IDX_L(OPND_IDX((*opnd)))) == Data_Obj) {
            save_attr			 = IR_IDX_L(OPND_IDX((*opnd)));
            ATD_PARENT_OBJECT(save_attr) = TRUE;
         }
      }

      if (uopr_ir_idx != NULL_IDX) {

         /* expr_semantics treats a unary minus on a BOZ constant as */
         /* an expression which means the column pointer comes back  */
         /* pointing to the minus sign rather than to the BOZ        */
         /* constant.  Save the line and column here and use them in */
         /* the result opnd below.                                   */

         boz_const_line_num = line;
         boz_const_col_num  = column;

         COPY_OPND(IR_OPND_L(uopr_ir_idx), (*opnd));
         OPND_FLD((*opnd)) = IR_Tbl_Idx;
         OPND_IDX((*opnd)) = uopr_ir_idx;
      }
      break;

   case AT_Tbl_Idx:

      if (AT_OBJ_CLASS(OPND_IDX((*opnd))) == Data_Obj) {
         save_attr			= OPND_IDX((*opnd));
         ATD_PARENT_OBJECT(save_attr)	= TRUE;
      }

      if (uopr_ir_idx != NULL_IDX) {
         PRINTMSG(IR_LINE_NUM(uopr_ir_idx), 958, Error,
                  IR_COL_NUM(uopr_ir_idx));
      }
      break;

   case CN_Tbl_Idx:

      if (uopr_ir_idx != NULL_IDX) {

         if (CN_BOZ_CONSTANT(OPND_IDX((*opnd)))) {
            PRINTMSG(IR_LINE_NUM(uopr_ir_idx), 957, Ansi,
                     IR_COL_NUM(uopr_ir_idx));
         }
         else if (TYP_TYPE(CN_TYPE_IDX(OPND_IDX((*opnd)))) != Integer  &&
                  TYP_TYPE(CN_TYPE_IDX(OPND_IDX((*opnd)))) != Real  &&
                  ! CN_BOOLEAN_CONSTANT(OPND_IDX((*opnd)))) {

            /* A sign is only permitted for integer or real literal constants */

            PRINTMSG(IR_LINE_NUM(uopr_ir_idx), 958, Error,
                     IR_COL_NUM(uopr_ir_idx));
         }
         COPY_OPND(IR_OPND_L(uopr_ir_idx), (*opnd));
         OPND_FLD((*opnd)) = IR_Tbl_Idx;
         OPND_IDX((*opnd)) = uopr_ir_idx;
      }
      break;
   }   /* end switch */

   /* Any subscripts must be initialization expressions */

   expr_desc.rank = 0;
   expr_mode 	  = Initialization_Expr;
   xref_state	  = CIF_Symbol_Reference;

   /* set comp_gen_expr to TRUE. This forces the fold of REAL   */
   /* constant expressions. When -Oieeeconform is specified,    */
   /* the folding of Real and Complex expressions is prevented. */

   comp_gen_expr = TRUE;

   if (expr_semantics(opnd, &expr_desc)) {
      find_opnd_line_and_column(opnd, &line, &column);

      switch (OPND_FLD((*opnd))) {
      case CN_Tbl_Idx:

         if (boz_const_line_num != 0) {
            OPND_LINE_NUM((*opnd)) = boz_const_line_num;
            OPND_COL_NUM((*opnd))  = boz_const_col_num;
            line		 = boz_const_line_num;
            column		 = boz_const_col_num;
         }

         break;

      case AT_Tbl_Idx:

         if (AT_OBJ_CLASS(OPND_IDX((*opnd))) == Data_Obj   &&
             ATD_CLASS(OPND_IDX((*opnd))) == Compiler_Tmp  &&
             ATD_FLD(OPND_IDX((*opnd))) == CN_Tbl_Idx)  {

            if (!expr_desc.constant) {
               PRINTMSG(line, 906, Error, column);
               *opnd = null_opnd;
            }
         }
         else {
            PRINTMSG(line, 1101, Error, column);
         }
         break;

      case IR_Tbl_Idx:

         if (!have_null) {
            PRINTMSG(line, 1648, Error, column);
            *opnd = null_opnd;
         }
         break;

      }  /* End switch */
   }

   if (save_attr != NULL_IDX) {
      ATD_PARENT_OBJECT(save_attr)	= FALSE;
   }

   /* reset comp_gen_expr to FALSE. end of */
   /* compiler generated expression        */

   comp_gen_expr	= FALSE;
   expr_mode		= save_expr_mode;

   TRACE (Func_Exit, "constant_value_semantics", NULL);

   return;

}  /* constant_value_semantics */

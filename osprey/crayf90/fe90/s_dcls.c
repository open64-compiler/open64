/*
 * Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/s_dcls.c	5.7	09/29/99 17:38:13\n";

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


/*********************************************************\
|* Globals used between decl_semantics and attr_semantics |
\*********************************************************/

	int     allocatable_list_idx;
	int     alt_entry_equiv_blk;
	int     alt_entry_equiv_grp;
	int	init_sh_start_idx;
	int	init_sh_end_idx;
	int     namelist_list_idx;
	int     number_of_allocatables;
	int     pointee_based_blk;
	int     reshape_array_list;


/*****************************************************************\
|* Function prototypes of static functions declared in this file *|
\*****************************************************************/

static  void    assign_offsets_for_equiv_groups(void);
static	void	attr_semantics(int, boolean);
static	void	bound_resolution(int);
static	boolean	compare_darg_or_rslt_types(int, int);
static	void	compare_duplicate_interface_bodies(int);
static	void	compare_entry_to_func_rslt(int, int);
static	boolean	darg_in_entry_list(int, int);
static  void    deallocate_local_allocatables(void);
static	void	distribution_resolution(int);
static	void	equivalence_semantics(void);
static	void	gen_assumed_shape_copy(opnd_type *);
static	int	gen_auto_length(int, opnd_type *);
static  void    gen_branch_around_ir(int, int, int);
static  int     gen_darg_branch_test(int);
static	boolean	gen_ir_at_this_entry(int, int);
#ifndef KEY /* Bug 4955 */
static	void	gen_present_ir(int, int, int);
#endif /* KEY Bug 4955 */
static	void	gen_single_automatic_allocate(int);
static	void	gen_tmp_eq_zero_ir(int);
static  void	insert_argchck_calls(int, int);
static	void	insert_sh_after_entries(int, int, int, boolean, boolean);
static	void	linearize_list_for_equiv(int);
static	int	merge_entry_lists(int, int);
static	int	merge_entry_list_count(int, int);
static	void	merge_equivalence_groups1(void);
static	void	merge_equivalence_groups2(void);
static  boolean must_reassign_XT_temp(opnd_type *);
static	void	namelist_resolution(int);
static	int	ntr_bnds_sh_tmp_list(opnd_type *, int, int, boolean, int);
static	void	reshape_array_semantics(void);
static	void	tmp_il_resolution(int);
static	void	tmp_ir_resolution(int);
static	void	verify_interface (int);
static	void	gen_allocatable_ptr_ptee(int);
static	int	set_up_bd_tmps(int, int, int, int, boolean);

# if defined(_TARGET_WORD_ADDRESS) ||  \
     (defined(_HEAP_REQUEST_IN_WORDS) && defined(_TARGET_BYTE_ADDRESS))
static	void	gen_word_align_byte_length_ir(opnd_type *);
# endif

# if !defined(_SINGLE_ALLOCS_FOR_AUTOMATIC)
static	void	gen_multiple_automatic_allocate(int);
# endif

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# pragma inline create_equiv_stor_blk
# else
# pragma _CRI inline create_equiv_stor_blk
# endif


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Perform semantic checks for EQUIVALENCE statements.                   *|
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
static void     equivalence_semantics(void)
{

   int			attr_idx;
   boolean		automatic;
   int			common_attr_idx;
   int			common_sb_idx;
   boolean		default_numeric_sequence;
   boolean		default_numeric_type;
   boolean		default_character_sequence;
   boolean		default_character_type;
   int			group;
   int			il_idx;
   int			ir_idx;
   boolean		is_volatile;
   int			item;
   int			list_idx;
   int			new_idx;
   int			nondefault_sequence_type;
   int			nondefault_intrinsic_type;
   int			offset_idx;
   boolean		ok;
   opnd_type		opnd;
   expr_arg_type	opnd_desc;
   long_type		result[MAX_WORDS_FOR_INTEGER];
   cif_usage_code_type	save_xref_state;
   int			sb_idx;
   int			subscript_count;
   int			substring_list;
   int			type_idx;


   TRACE (Func_Entry, "equivalence_semantics", NULL);

   group = SCP_FIRST_EQUIV_GRP(curr_scp_idx);

   while (group != NULL_IDX) {
      item		= group;
      common_attr_idx	= NULL_IDX;
      common_sb_idx	= NULL_IDX;

      while (item != NULL_IDX) {

         if (ATD_IN_COMMON(EQ_ATTR_IDX(item))) {

            if (common_sb_idx == NULL_IDX) {
               common_attr_idx	= EQ_ATTR_IDX(item);
               common_sb_idx	= ATD_STOR_BLK_IDX(common_attr_idx);
            }
            else if (common_sb_idx != ATD_STOR_BLK_IDX(common_attr_idx)) {

               /* Two different items from the same common */
               /* block are equivalenced together.         */

               PRINTMSG(EQ_LINE_NUM(item), 826, Error, EQ_COLUMN_NUM(item),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)),
                        AT_OBJ_NAME_PTR(common_attr_idx));
            }

            if (SB_BLK_HAS_NPES(common_sb_idx)) {
               PRINTMSG(EQ_LINE_NUM(item), 1228, Error, EQ_COLUMN_NUM(item),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)),
                        SB_BLANK_COMMON(common_sb_idx) ?
                        "" : SB_NAME_PTR(common_sb_idx));
               AT_DCL_ERR(EQ_ATTR_IDX(item))	= TRUE;
            }

         }

         if (EQ_OPND_FLD(item) == NO_Tbl_Idx) {

            /* if stand alone name, then offset is set to 0 */

            NTR_IR_LIST_TBL(new_idx);
            EQ_LIST_IDX(item)		= new_idx;
            IL_FLD(new_idx)		= CN_Tbl_Idx;
            IL_IDX(new_idx)		= CN_INTEGER_ZERO_IDX;
            IL_LINE_NUM(new_idx)	= 1;
            IL_COL_NUM(new_idx)		= 0;
         }
         else if ((!EQ_SUBSTRINGED(item) && 
                   ATD_ARRAY_IDX(EQ_ATTR_IDX(item)) == NULL_IDX) ||
                  (EQ_SUBSTRINGED(item) &&
                   TYP_TYPE(ATD_TYPE_IDX(EQ_ATTR_IDX(item))) != Character)) {
            AT_DCL_ERR(EQ_ATTR_IDX(item)) = TRUE;
            PRINTMSG(EQ_LINE_NUM(item), 840, Error,
                     EQ_COLUMN_NUM(item),
                     AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
            NTR_IR_LIST_TBL(new_idx);
            EQ_LIST_IDX(item)		= new_idx;
            IL_FLD(new_idx)		= CN_Tbl_Idx;
            IL_IDX(new_idx)		= CN_INTEGER_ZERO_IDX;
            IL_LINE_NUM(new_idx)	= 1;
            IL_COL_NUM(new_idx)		= 0;
         }
         else {

            /* this is true only if something follows the object */
            /* that is a subscript and or substring              */

            OPND_FLD(opnd)		= EQ_OPND_FLD(item);
            OPND_IDX(opnd)		= EQ_OPND_IDX(item);
            OPND_LINE_NUM(opnd)		= EQ_LINE_NUM(item);
            OPND_COL_NUM(opnd)		= EQ_COLUMN_NUM(item);
            opnd_desc.rank		= 0;
            expr_mode			= Initialization_Expr;
            save_xref_state		= xref_state;
            xref_state			= CIF_Symbol_Reference;
            attr_idx			= find_left_attr(&opnd);
            ATD_PARENT_OBJECT(attr_idx)	= TRUE;
            ok				= expr_semantics(&opnd, &opnd_desc);
            xref_state			= save_xref_state;
            expr_mode			= Regular_Expr;
            ATD_PARENT_OBJECT(attr_idx)	= FALSE;

            if (!ok) {
               EQ_LIST_IDX(item)= NULL_IDX;
               EQ_ERROR(item)	= TRUE;
               item		= EQ_NEXT_EQUIV_OBJ(item);
               continue;
            }

            /* Break the subscripts and substrings up. */

            subscript_count	= 0;
            substring_list	= NULL_IDX;

            ir_idx = (OPND_FLD(opnd) == IR_Tbl_Idx) ? OPND_IDX(opnd): NULL_IDX;

            if (ir_idx != NULL_IDX &&
                (IR_OPR(ir_idx) == Substring_Opr ||
                 IR_OPR(ir_idx) == Whole_Substring_Opr)) {
               EQ_SUBSTRINGED(item)	= TRUE;
               substring_list		= IR_IDX_R(ir_idx);
               ir_idx = (IR_FLD_L(ir_idx) == IR_Tbl_Idx) ? IR_IDX_L(ir_idx) :
                                                           NULL_IDX;
            }

            if (ir_idx != NULL_IDX &&
                 IR_OPR(ir_idx) == Whole_Subscript_Opr) {
               ir_idx = (IR_FLD_L(ir_idx) == IR_Tbl_Idx) ? IR_IDX_L(ir_idx) :
                                                           NULL_IDX;
            }

            if (ir_idx != NULL_IDX && 
                (IR_OPR(ir_idx) == Section_Subscript_Opr ||
                 IR_OPR(ir_idx) == Struct_Opr)) {

               if (IR_OPR(ir_idx) == Section_Subscript_Opr) {
                  PRINTMSG(EQ_LINE_NUM(item), 250, Error, EQ_COLUMN_NUM(item));
               }
               else {
                  PRINTMSG(EQ_LINE_NUM(item), 1537, Error, EQ_COLUMN_NUM(item));
               }


               EQ_LIST_IDX(item)	= NULL_IDX;
               EQ_ERROR(item)		= TRUE;
               item			= EQ_NEXT_EQUIV_OBJ(item);
               continue;
            }

            if (ir_idx != NULL_IDX && 
                (IR_OPR(ir_idx) == Subscript_Opr ||
                 IR_OPR(ir_idx) == Whole_Subscript_Opr ||
                 IR_OPR(ir_idx) == Section_Subscript_Opr)) {
               subscript_count	 = IR_LIST_CNT_R(ir_idx);
               EQ_LIST_IDX(item) = IR_IDX_R(ir_idx);
            }

            if (substring_list != NULL_IDX) {  /* Add the substring list */

               if (EQ_LIST_IDX(item) == NULL_IDX) {
                  EQ_LIST_IDX(item) = substring_list;
               }
               else {
                  il_idx = EQ_LIST_IDX(item);

                  while (IL_NEXT_LIST_IDX(il_idx) != NULL_IDX) {
                     il_idx = IL_NEXT_LIST_IDX(il_idx);
                  }
                  IL_NEXT_LIST_IDX(il_idx) = substring_list;
               }

               il_idx = IL_NEXT_LIST_IDX(substring_list);  /* End substring*/
               il_idx = IL_NEXT_LIST_IDX(il_idx);

               /* il_idx is now the character length in the substring.  */
               /* This is not needed, but a NULL entry is, so clear it. */
               /* But check for a zero length substring first.          */

               if (IL_FLD(il_idx) == CN_Tbl_Idx) {
                  type_idx    = CG_LOGICAL_DEFAULT_TYPE;

                  folder_driver((char *) &CN_CONST(IL_IDX(il_idx)),
                                         CN_TYPE_IDX(IL_IDX(il_idx)),
                                (char *) &CN_CONST(CN_INTEGER_ZERO_IDX),
                                         CN_TYPE_IDX(CN_INTEGER_ZERO_IDX),
                                         result,
                                         &type_idx,
                                         EQ_LINE_NUM(item),
                                         EQ_COLUMN_NUM(item),
                                         2,
                                         Le_Opr);

                 if (THIS_IS_TRUE(result, type_idx)) {
                    PRINTMSG(EQ_LINE_NUM(item), 1627,Error,EQ_COLUMN_NUM(item));
                 }
               }
               IL_OPND(il_idx) = null_opnd;
            }
            else if (EQ_LIST_IDX(item) != NULL_IDX) {

               /* Just have subscripts.  Find end of list and add NULL */

               il_idx = EQ_LIST_IDX(item);

               while (IL_NEXT_LIST_IDX(il_idx) != NULL_IDX) {
                  il_idx = IL_NEXT_LIST_IDX(il_idx);
               }
               NTR_IR_LIST_TBL(new_idx);
               IL_NEXT_LIST_IDX(il_idx)	= new_idx;
               IL_OPND(new_idx)		= null_opnd;
               IL_LINE_NUM(new_idx)	= EQ_LINE_NUM(item);
               IL_COL_NUM(new_idx)	= EQ_COLUMN_NUM(item);
            }

            EQ_OPND_FLD(item)	= NO_Tbl_Idx;
            EQ_OPND_IDX(item)	= NULL_IDX;

            if (ATD_ARRAY_IDX(EQ_ATTR_IDX(item)) > 0) {

               if (! dump_flags.no_dimension_padding &&
                   subscript_count < BD_RANK(ATD_ARRAY_IDX(EQ_ATTR_IDX(item)))){
                  PRINTMSG(EQ_LINE_NUM(item), 375, Warning, 
                           EQ_COLUMN_NUM(item));
               }
               else if (subscript_count > 
                        BD_RANK(ATD_ARRAY_IDX(EQ_ATTR_IDX(item)))) {
                  PRINTMSG(EQ_LINE_NUM(item), 204, Error, 
                           EQ_COLUMN_NUM(item));

                  /* Do not want to call linearize_list_for_equiv because the */
                  /* rank of the array is less than the number of dimension.  */

                  item = EQ_NEXT_EQUIV_OBJ(item);
                  continue;
               }
            }

            linearize_list_for_equiv(item);
         }

         item = EQ_NEXT_EQUIV_OBJ(item);
      }
      group = EQ_NEXT_EQUIV_GRP(group);
   }

   merge_equivalence_groups1();

   assign_offsets_for_equiv_groups();

   merge_equivalence_groups2();

   group = SCP_FIRST_EQUIV_GRP(curr_scp_idx);

   while (group != NULL_IDX) {
      item		= group;
      sb_idx		= NULL_IDX;
      automatic		= FALSE;
      is_volatile	= FALSE;

      while (item != NULL_IDX) {

         if (EQ_ERROR(item)) {
            item = EQ_NEXT_EQUIV_OBJ(item);
            continue;
         }

         attr_idx       = EQ_ATTR_IDX(item);

         if (!EQ_SEARCH_DONE(item) &&
             (ATD_CLASS(EQ_ATTR_IDX(item)) == Variable &&
              ATD_EQUIV_LIST(EQ_ATTR_IDX(item)) != NULL_IDX)) {

            /* This attr is in this equivalence group more than once. */
            /* All these items need to have the same offset.  We make */
            /* the assumption that the constant table shares entries, */
            /* so all these offset indexes should be the same.  If    */
            /* they are not, issue an error.                          */

            list_idx			= ATD_EQUIV_LIST(EQ_ATTR_IDX(item));
            offset_idx			= EQ_OFFSET_IDX(item);
            EQ_SEARCH_DONE(item)	= TRUE;

            while (list_idx != NULL_IDX) {

            if (fold_relationals(EQ_OFFSET_IDX(AL_EQ_IDX(list_idx)),
                                 offset_idx,
                                 Ne_Opr)) {

                  PRINTMSG(EQ_LINE_NUM(item), 528, Error,
                           EQ_COLUMN_NUM(item),
                           AT_OBJ_NAME_PTR(attr_idx));
               }

               list_idx	= AL_NEXT_IDX(list_idx);
            }
         }

         if (sb_idx != NULL_IDX && sb_idx != ATD_STOR_BLK_IDX(attr_idx) &&
             SB_IS_COMMON(ATD_STOR_BLK_IDX(attr_idx)) &&
             SB_IS_COMMON(sb_idx)) {
            PRINTMSG(EQ_LINE_NUM(item), 823, Error,
                     EQ_COLUMN_NUM(item),
                     SB_BLANK_COMMON(sb_idx) ?
                     "" : SB_NAME_PTR(sb_idx),
                     SB_BLANK_COMMON(ATD_STOR_BLK_IDX(attr_idx)) ?
                     "" : SB_NAME_PTR(ATD_STOR_BLK_IDX(attr_idx)));
         }

         automatic   |= ATD_STACK(attr_idx);
         is_volatile |= ATD_VOLATILE(attr_idx);

         /* if item is in a common block move all items to that block */

         if (SB_IS_COMMON(ATD_STOR_BLK_IDX(attr_idx))) {
            sb_idx = ATD_STOR_BLK_IDX(attr_idx);

            /* If any item is in a common block and dalign is not     */
            /* specified on the commandline, none of the items in     */
            /* the equivalence group can be double aligned.           */

            EQ_DO_NOT_DALIGN(group) = !cmd_line_flags.dalign;
         }
         else if (SB_HOSTED_STATIC(ATD_STOR_BLK_IDX(attr_idx))) {
 
            if (sb_idx == NULL_IDX || !SB_IS_COMMON(sb_idx)) {
               sb_idx = ATD_STOR_BLK_IDX(attr_idx);
            }
         }
         else if (SB_HOSTED_STACK(ATD_STOR_BLK_IDX(attr_idx))) {

            if (sb_idx == NULL_IDX || 
                (!SB_IS_COMMON(sb_idx) && !SB_HOSTED_STATIC(sb_idx))) {
               sb_idx = ATD_STOR_BLK_IDX(attr_idx);
            }
         }
         else if (SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx)) == Static ||
                  SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx)) == Static_Named ||
                  SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx)) == Static_Local) {

            if (sb_idx == NULL_IDX) {

               /* if no storage block yet and item is in @DATA */
               /* move all items to @DATA                      */

               sb_idx = ATD_STOR_BLK_IDX(attr_idx);
            }
         }

         if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure &&
             !cmd_line_flags.dalign &&
             ATT_DCL_NUMERIC_SEQ(TYP_IDX(ATD_TYPE_IDX(attr_idx)))) {
            EQ_DO_NOT_DALIGN(group) = TRUE;
         }

         item = EQ_NEXT_EQUIV_OBJ(item);
      }

      if (sb_idx == NULL_IDX) {
         sb_idx = create_equiv_stor_blk(EQ_ATTR_IDX(group), Stack);
      }

# if defined(_SEPARATE_NONCOMMON_EQUIV_GROUPS)

      else if (SB_HOSTED_STATIC(sb_idx)) {
         sb_idx = create_equiv_stor_blk(EQ_ATTR_IDX(group), Static);
         SB_HOSTED_STATIC(sb_idx)	= TRUE;
      }
      else if (SB_HOSTED_STACK(sb_idx)) {
         sb_idx = create_equiv_stor_blk(EQ_ATTR_IDX(group), Stack);
         SB_HOSTED_STACK(sb_idx)	= TRUE;
      }
      else if ((SB_BLK_TYPE(sb_idx) == Static ||
                SB_BLK_TYPE(sb_idx) == Static_Named ||
                SB_BLK_TYPE(sb_idx) == Static_Local) &&
               !SB_MODULE(sb_idx)) {
         sb_idx = create_equiv_stor_blk(EQ_ATTR_IDX(group),SB_BLK_TYPE(sb_idx));
      }
# endif

      SB_EQUIVALENCED(sb_idx)		= TRUE;

      if (SB_PAD_BLK(sb_idx) && !SB_IS_COMMON(sb_idx)) {
         PRINTMSG(EQ_LINE_NUM(group), 1352, Warning, EQ_COLUMN_NUM(group));
      }

      item				= group;
      default_numeric_sequence		= FALSE;
      default_numeric_type		= FALSE;
      default_character_sequence	= FALSE;
      default_character_type		= FALSE;
      nondefault_sequence_type		= NULL_IDX;
      nondefault_intrinsic_type		= NULL_IDX;

      /* An item in an equivalence group can be one of 6 type categories */
      /* according to the standard.  The standard only allows mixing of  */
      /* certain categories and Cray allows a few extra extensions.      */

      /* The categories are:                                             */
      /*   default_numeric_sequence   -> A derived type whose components */
      /*                                 are all default numeric types.  */
      /*   default_numeric_type       -> The type must be a default      */
      /*                                 numeric type.  (Not character,  */
      /*                                 derived type, or CRI pointer.)  */
      /*   default_character_sequence -> A derived type whose components */
      /*                                 are all default character types.*/
      /*   default_character_type     -> The type is default character.  */
      /*   nondefault_sequence_type   -> A derived type with mixed       */
      /*                                 components, both numeric and    */
      /*                                 character or non-default numeric*/
      /*   nondefault_intrinsic_type  -> The type is not a default type. */


      while (item != NULL_IDX) {

         if (EQ_ERROR(item)) {
            item = EQ_NEXT_EQUIV_OBJ(item);
            continue;
         }

         ATD_VOLATILE(EQ_ATTR_IDX(item))	= is_volatile;

         if (SB_IS_COMMON(sb_idx)) {

            if (ATD_SAVED(EQ_ATTR_IDX(item))) {

               /* An object with the SAVE attribute may not be */
               /* equivalenced to an object in a common block. */

               PRINTMSG(EQ_LINE_NUM(item), 1256, Error, EQ_COLUMN_NUM(item),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)),
                        "SAVE");
            }

            if (ATD_STACK(EQ_ATTR_IDX(item))) {

               /* An object with the AUTOMATIC attribute may not be */
               /* equivalenced to an object in a common block.      */

               PRINTMSG(EQ_LINE_NUM(item), 1256, Error, EQ_COLUMN_NUM(item),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)),
                        "AUTOMATIC");
            }

            if (TYP_TYPE(ATD_TYPE_IDX(EQ_ATTR_IDX(item))) == Structure &&
               ATT_DEFAULT_INITIALIZED(TYP_IDX(
                                       ATD_TYPE_IDX(EQ_ATTR_IDX(item))))) {
               PRINTMSG(EQ_LINE_NUM(item), 1591, Error, EQ_COLUMN_NUM(item),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)),
                        AT_OBJ_NAME_PTR(TYP_IDX(
                                        ATD_TYPE_IDX(EQ_ATTR_IDX(item)))));
            }
         }
         else if (automatic && !ATD_STACK(EQ_ATTR_IDX(item))) {

            /* All must have the automatic attribute.  */

            PRINTMSG(EQ_LINE_NUM(item), 1257, Error, EQ_COLUMN_NUM(item),
                     AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)),
                     "AUTOMATIC", "AUTOMATIC");
         }

         ATD_STOR_BLK_IDX(EQ_ATTR_IDX(item)) = sb_idx;
         type_idx			     = ATD_TYPE_IDX(EQ_ATTR_IDX(item));

         if (TYP_TYPE(type_idx) == Structure) {

            if (!ATT_SEQUENCE_SET(TYP_IDX(type_idx))) {
               PRINTMSG(EQ_LINE_NUM(item), 294, Error,
                        EQ_COLUMN_NUM(item),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
            }

            if (ATT_POINTER_CPNT(TYP_IDX(type_idx))
#ifdef KEY /* Bug 6845 */
	      || ATT_ALLOCATABLE_CPNT(TYP_IDX(type_idx))
#endif /* KEY Bug 6845 */
	    ) {
               PRINTMSG(EQ_LINE_NUM(item), 354, Error,
                        EQ_COLUMN_NUM(item),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
            }

            if (ATT_CHAR_SEQ(TYP_IDX(type_idx))) {

               /* default_character_sequence */

               if (default_numeric_sequence || default_numeric_type) {
                  PRINTMSG(EQ_LINE_NUM(item), 1239, Error,
                           EQ_COLUMN_NUM(item),
                           AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
               }
               else if (nondefault_sequence_type != NULL_IDX) {
                  PRINTMSG(EQ_LINE_NUM(nondefault_sequence_type), 1242, Error,
                           EQ_COLUMN_NUM(nondefault_sequence_type),
                           AT_OBJ_NAME_PTR(EQ_ATTR_IDX(
                                           nondefault_sequence_type)));
               }
               else if (nondefault_intrinsic_type != NULL_IDX) {
                  PRINTMSG(EQ_LINE_NUM(nondefault_intrinsic_type), 1241, Error,
                           EQ_COLUMN_NUM(nondefault_intrinsic_type),
                           AT_OBJ_NAME_PTR(EQ_ATTR_IDX(
                                           nondefault_intrinsic_type)));
               }
               else {
                  default_character_sequence	= TRUE;
               }
            }
            else if (!ATT_NON_DEFAULT_CPNT(TYP_IDX(type_idx)) &&
                      ATT_DCL_NUMERIC_SEQ(TYP_IDX(type_idx))) {

               /* default_numeric_sequence */

               if (default_character_sequence || default_character_type) {
                  PRINTMSG(EQ_LINE_NUM(item), 1240, Error,
                           EQ_COLUMN_NUM(item),
                           AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
               }
               else if (nondefault_sequence_type != NULL_IDX) {
                  PRINTMSG(EQ_LINE_NUM(nondefault_sequence_type), 1242, Error,
                           EQ_COLUMN_NUM(nondefault_sequence_type),
                           AT_OBJ_NAME_PTR(EQ_ATTR_IDX(
                                           nondefault_sequence_type)));
               }
               else if (nondefault_intrinsic_type != NULL_IDX) {
                  PRINTMSG(EQ_LINE_NUM(nondefault_intrinsic_type), 1241, Error,
                           EQ_COLUMN_NUM(nondefault_intrinsic_type),
                           AT_OBJ_NAME_PTR(EQ_ATTR_IDX(
                                           nondefault_intrinsic_type)));
               }

               else {

# if defined(_ACCEPT_CMD_s_32)
                  if (cmd_line_flags.s_default32) {
                     PRINTMSG(EQ_LINE_NUM(item), 1244, Warning,
                              EQ_COLUMN_NUM(item),
                              AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)),
                              AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
                  }
# endif
                  default_numeric_sequence	= TRUE;
               }
            }
            else {  /* nondefault sequence type */

               if (default_character_sequence || default_character_type) {
                  PRINTMSG(EQ_LINE_NUM(item), 1240, Error,
                           EQ_COLUMN_NUM(item),
                           AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
               }
               else if (default_numeric_sequence || default_numeric_type) {
                  PRINTMSG(EQ_LINE_NUM(item), 1239, Error,
                              EQ_COLUMN_NUM(item),
                              AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
               }
               else if (nondefault_intrinsic_type != NULL_IDX) {
                  PRINTMSG(EQ_LINE_NUM(nondefault_intrinsic_type), 1241, Error,
                           EQ_COLUMN_NUM(nondefault_intrinsic_type),
                           AT_OBJ_NAME_PTR(EQ_ATTR_IDX(
                                           nondefault_intrinsic_type)));
               }
               else {
                  nondefault_sequence_type	= item;
               }
            }
         }
         else if (TYP_TYPE(type_idx) == Character) {

            if (default_numeric_sequence) {
               PRINTMSG(EQ_LINE_NUM(item), 1239, Error,
                        EQ_COLUMN_NUM(item),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
            }
            else if (default_numeric_type) {
               PRINTMSG(EQ_LINE_NUM(item), 522, Ansi,
                        EQ_COLUMN_NUM(item));
               default_character_type   = TRUE;
            }
            else if (nondefault_sequence_type != NULL_IDX) {
               PRINTMSG(EQ_LINE_NUM(nondefault_sequence_type), 1242, Error,
                        EQ_COLUMN_NUM(nondefault_sequence_type),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(nondefault_sequence_type)));
            }
            else if (nondefault_intrinsic_type != NULL_IDX) {
               PRINTMSG(EQ_LINE_NUM(nondefault_intrinsic_type), 522, Ansi,
                        EQ_COLUMN_NUM(nondefault_intrinsic_type));
               default_character_type	= TRUE;
            }
            else {
               default_character_type	= TRUE;
            }
         }
         else if (TYP_DESC(type_idx) == Default_Typed ||
                  TYP_LINEAR(type_idx) == INTEGER_DEFAULT_TYPE ||
                  TYP_LINEAR(type_idx) == LOGICAL_DEFAULT_TYPE ||
                  TYP_LINEAR(type_idx) == REAL_DEFAULT_TYPE ||
                  TYP_LINEAR(type_idx) == DOUBLE_DEFAULT_TYPE ||
                  TYP_LINEAR(type_idx) == COMPLEX_DEFAULT_TYPE) {

            if (default_character_sequence) {
               PRINTMSG(EQ_LINE_NUM(item), 1240, Error,
                        EQ_COLUMN_NUM(item),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
            }
            else if (default_character_type) {
               PRINTMSG(EQ_LINE_NUM(item), 522, Ansi,
                        EQ_COLUMN_NUM(item));
               default_numeric_type             = TRUE;
            }
            else if (nondefault_sequence_type != NULL_IDX) {
               PRINTMSG(EQ_LINE_NUM(nondefault_sequence_type), 1242, Error,
                        EQ_COLUMN_NUM(nondefault_sequence_type),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(nondefault_sequence_type)));
            }
            else if (nondefault_intrinsic_type != NULL_IDX) {
               PRINTMSG(EQ_LINE_NUM(nondefault_intrinsic_type), 1097, Ansi,
                        EQ_COLUMN_NUM(nondefault_intrinsic_type));
               default_numeric_type		= TRUE;
            }
            else {
               default_numeric_type		= TRUE;
            }
         }
         else {

            if (default_character_sequence) {
               PRINTMSG(EQ_LINE_NUM(item), 1240, Error,
                        EQ_COLUMN_NUM(item),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
            }
            else if (default_character_type) {
               PRINTMSG(EQ_LINE_NUM(item), 522, Ansi, EQ_COLUMN_NUM(item));
               nondefault_intrinsic_type                = item;
            }
            else if (default_numeric_type) {
               PRINTMSG(EQ_LINE_NUM(item), 1097, Ansi, EQ_COLUMN_NUM(item));
               nondefault_intrinsic_type		= item;
            }
            else if (default_numeric_sequence) {
               PRINTMSG(EQ_LINE_NUM(item), 1239, Error,
                        EQ_COLUMN_NUM(item),
                        AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
            }
            else if (nondefault_intrinsic_type != NULL_IDX &&
                     TYP_LINEAR(ATD_TYPE_IDX(
                                EQ_ATTR_IDX(nondefault_intrinsic_type))) !=
                     TYP_LINEAR(type_idx)) {
               PRINTMSG(EQ_LINE_NUM(item), 1097, Ansi, EQ_COLUMN_NUM(item));
               nondefault_intrinsic_type		= item;
            }
            else {
               nondefault_intrinsic_type		= item;
            }
         }
         
         item = EQ_NEXT_EQUIV_OBJ(item);
      }

      group = EQ_NEXT_EQUIV_GRP(group);
   }

   TRACE (Func_Exit, "equivalence_semantics", NULL);

   return;

}  /* equivalence_semantics */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Linearize an EQUIVALENCE subscript/substring reference.               *|
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
static void     linearize_list_for_equiv(int	item)
{
   int         		attr_idx;
   int         		bd_idx;
   size_offset_type	bit_offset;
   int         		dim;
   int         		l_idx;
   int         		list_idx;
   size_offset_type	left;
   size_offset_type	result;
   size_offset_type	right;
   int         		start_expr_idx;
#ifdef KEY /* Bug 10177 */
   int         		trail_l_idx = 0;
#else /* KEY Bug 10177 */
   int         		trail_l_idx;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "linearize_list_for_equiv", NULL);

   attr_idx		= EQ_ATTR_IDX(item);
   list_idx		= EQ_LIST_IDX(item);
   bit_offset.fld	= CN_Tbl_Idx;
   bit_offset.idx	= CN_INTEGER_ZERO_IDX;

   if (list_idx != NULL_IDX) {

      if (!EQ_SUBSTRINGED(item)) {

         if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
            bd_idx	= ATD_ARRAY_IDX(attr_idx);
            dim		= 1;
            l_idx	= list_idx;

            while (l_idx != NULL_IDX && IL_FLD(l_idx) != NO_Tbl_Idx) {
               right.fld	= BD_LB_FLD(bd_idx,dim);
               right.idx	= BD_LB_IDX(bd_idx,dim);
               left.fld		= IL_FLD(l_idx);
               left.idx		= IL_IDX(l_idx);

               if (!size_offset_binary_calc(&left, &right, Minus_Opr, &result)){
                  break;
               }

               left.fld		= BD_SM_FLD(bd_idx,dim);
               left.idx		= BD_SM_IDX(bd_idx,dim);

               if (!size_offset_binary_calc(&left, &result, Mult_Opr, &result)){
                  break;
               }

               if (!size_offset_binary_calc(&bit_offset,
                                            &result,
                                             Plus_Opr,
                                            &bit_offset)) {
                  break;
               }

               l_idx = IL_NEXT_LIST_IDX(l_idx);
               dim++;
            }
         }
      }
      else { /* it is substringed */

         if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
            l_idx = list_idx;

            while (l_idx != NULL_IDX && IL_FLD(l_idx) != NO_Tbl_Idx) {
               trail_l_idx	= l_idx;
               l_idx		= IL_NEXT_LIST_IDX(l_idx);
            }

            start_expr_idx	= IL_PREV_LIST_IDX(trail_l_idx); 

            left.fld		= IL_FLD(start_expr_idx);
            left.idx		= IL_IDX(start_expr_idx);
            right.fld		= CN_Tbl_Idx;
            right.idx		= CN_INTEGER_ONE_IDX;

            size_offset_binary_calc(&left, &right, Minus_Opr, &bit_offset);

            IL_FLD(start_expr_idx) = NO_Tbl_Idx;

            if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {

               if (IL_FLD(list_idx) == NO_Tbl_Idx) {
                  AT_DCL_ERR(attr_idx)	= TRUE;
                  PRINTMSG(IL_LINE_NUM(list_idx), 250, Error,
                           IL_COL_NUM(list_idx));
               }

               bd_idx	= ATD_ARRAY_IDX(attr_idx);
               dim	= 1;
               l_idx	= list_idx;

               while (l_idx != NULL_IDX && IL_FLD(l_idx) != NO_Tbl_Idx) {

                  left.fld	= IL_FLD(l_idx);
                  left.idx	= IL_IDX(l_idx);
                  right.fld	= BD_LB_FLD(bd_idx, dim);
                  right.idx	= BD_LB_IDX(bd_idx, dim);

                  if (!size_offset_binary_calc(&left, 
                                               &right,
                                                Minus_Opr,
                                               &result)) {
                     break;
                  }
   
                  left.fld	= BD_SM_FLD(bd_idx, dim);
                  left.idx	= BD_SM_IDX(bd_idx, dim);

                  if (!size_offset_binary_calc(&left, 
                                               &result,
                                                Mult_Opr,
                                               &result)) {
                     break;
                  }
   
                  if (!size_offset_binary_calc(&bit_offset,
                                               &result,
                                                Plus_Opr,
                                               &bit_offset)) {
                     break;
                  }
   
                  l_idx = IL_NEXT_LIST_IDX(l_idx);
                  dim = dim + 1;
               }
            }
         }
      }  /* it is substringed */
   }

   if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
      result.fld	= CN_Tbl_Idx;
      result.idx	= CN_INTEGER_CHAR_BIT_IDX;
   }
   else {
      result.fld	= CN_Tbl_Idx;
      result.idx	= CN_INTEGER_BITS_PER_WORD_IDX;

# if defined(_TARGET_OS_MAX) || defined(_WHIRL_HOST64_TARGET64)

      /* Complex_4 does not go here because it is aligned for 64 bits.     */
      /* The stride multiplier for one of these types is based on 32 bits  */
      /* not the standard 64 bits.  (MPP only)                             */

      if (PACK_HALF_WORD_TEST_CONDITION(ATD_TYPE_IDX(attr_idx))) {
         C_TO_F_INT(result.constant,
                    TARGET_BITS_PER_WORD / 2, 
                    CG_INTEGER_DEFAULT_TYPE);
         result.fld		= NO_Tbl_Idx;
         result.type_idx	= CG_INTEGER_DEFAULT_TYPE;
      }
# endif

# if defined(_INTEGER_1_AND_2)

      if (on_off_flags.integer_1_and_2) {

         if (PACK_8_BIT_TEST_CONDITION(ATD_TYPE_IDX(attr_idx))) {
            C_TO_F_INT(result.constant, 8, CG_INTEGER_DEFAULT_TYPE);
            result.fld		= NO_Tbl_Idx;
            result.type_idx	= CG_INTEGER_DEFAULT_TYPE;
         }
         else if (PACK_16_BIT_TEST_CONDITION(ATD_TYPE_IDX(attr_idx))) {
            C_TO_F_INT(result.constant, 16, CG_INTEGER_DEFAULT_TYPE);
            result.fld		= NO_Tbl_Idx;
            result.type_idx	= CG_INTEGER_DEFAULT_TYPE;
         }
      }

# endif
   }

   size_offset_binary_calc(&bit_offset, &result, Mult_Opr, &bit_offset);

   if (bit_offset.fld == NO_Tbl_Idx) {
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = ntr_const_tbl(bit_offset.type_idx,
                                       FALSE,
                                       bit_offset.constant);
   }
   else {
      IL_FLD(list_idx) = bit_offset.fld;
      IL_IDX(list_idx) = bit_offset.idx;
   }

   IL_LINE_NUM(list_idx) = 1;
   IL_COL_NUM(list_idx)  = 0;

   TRACE (Func_Exit, "linearize_list_for_equiv", NULL);

   return;

}  /* linearize_list_for_equiv */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This merge routine will search through two equivalence groups at a    *|
|*      time.  If an identical object is found in both groups those two       *|
|*      groups are merged into one equivalence group.   Identical means       *|
|*      that we are looking at the same attr and the bit offset value is      *|
|*      identical on these two objects.  Because we are merging only when     *|
|*      the offsets on the two objects are identical there is no need to      *|
|*      adjust offsets for the objects in the merged groups.                  *|
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
static void     merge_equivalence_groups1(void)
{

   int           group;
   int           group_end;
   int           item;
   int           list_idx;
   int           list_item;
#ifdef KEY /* Bug 10177 */
   int           prev_group = 0;
#else /* KEY Bug 10177 */
   int           prev_group;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "merge_equivalence_groups1", NULL);

   group = SCP_FIRST_EQUIV_GRP(curr_scp_idx);

   while (group != NULL_IDX) {

      if (EQ_MERGED(group)) {

         /* This group has been merged with a previous   */
         /* group, so remove it from the group list.     */
         
         EQ_NEXT_EQUIV_GRP(prev_group) = EQ_NEXT_EQUIV_GRP(group);
      }
      else {
         group_end	= EQ_GRP_END_IDX(group);
         item		= group;
   
         while (item != NULL_IDX) {

            if (EQ_ERROR(item)) {
               item		= EQ_NEXT_EQUIV_OBJ(item);
               continue;
            }

            if (EQ_SEARCH_DONE(item)) {

               /* This item has been merged into this group because it */
               /* Matches another item in this group.  Do not search   */
               /* again.  It is a waste of time because we've already  */
               /* searched all occurences of this item.  We will not   */
               /* come across this eq item in this routine again,      */
               /* because we are doing only one pass through all       */
               /* groups and items, so turn off the flag so it can be  */
               /* used in the group2 merge later on.                   */

               EQ_SEARCH_DONE(item) = FALSE;
            }
            else if (ATD_CLASS(EQ_ATTR_IDX(item)) == Variable &&
                     ATD_EQUIV_LIST(EQ_ATTR_IDX(item)) != NULL_IDX) {

               /* This attr is in more than one equivalence group. */

               list_idx = ATD_EQUIV_LIST(EQ_ATTR_IDX(item));

               while (list_idx != NULL_IDX) {
                  list_item = AL_EQ_IDX(list_idx);

                  if (list_item != item && EQ_GRP_IDX(list_item) != group &&
                      (IL_IDX(EQ_LIST_IDX(list_item)) ==
                                IL_IDX(EQ_LIST_IDX(item)))) {

                     /* Same attr with same offset.  Merge them.  Do not */
                     /* merge if this item is already in this group.     */

                     /* 1) Mark list item to prevent researching.        */
                     /* 2) Merge the new group to the end of the old.    */
                     /* 3) Mark the merged group as merged, so it can    */
                     /*    be removed from the group list.               */
                     /* 4) Set EQ_GRP_IDX for all members of new group.  */

                     EQ_SEARCH_DONE(list_item)	 	= TRUE;
                     EQ_NEXT_EQUIV_OBJ(group_end)	= EQ_GRP_IDX(list_item);
                     EQ_MERGED(EQ_GRP_IDX(list_item))	= TRUE;

                     group_end	= EQ_GRP_END_IDX(EQ_GRP_IDX(list_item));
                     list_item	= EQ_GRP_IDX(list_item);  /* Group start */

                     while (list_item != NULL_IDX) {
                        EQ_GRP_IDX(list_item)	= group;
                        list_item		= EQ_NEXT_EQUIV_OBJ(list_item);
                     }
                  }
                  list_idx	= AL_NEXT_IDX(list_idx);
               }
            }
            item		= EQ_NEXT_EQUIV_OBJ(item);
         }
         EQ_GRP_END_IDX(group)	= group_end;
         prev_group		= group;
      }
      group			= EQ_NEXT_EQUIV_GRP(group);
   }

   TRACE (Func_Exit, "merge_equivalence_groups1", NULL);

   return;

}  /* merge_equivalence_groups1 */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This merge routine is slightly different than 			      *|
|*	merge_equivalence_groups1 in that two groups are merged if they       *|
|*	contain an identical object regardless of the offset attached to      *|
|*	that object.  At this point we know that the offsets attached to      *|
|*	the objects are different so we will have to adjust all the offsets   *|
|*	in one of the two groups by the difference in the offsets of the      *|
|*	two identical objects. 				                      *|
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
static void     merge_equivalence_groups2(void)
{
   boolean		adjust;
   size_offset_type	adjust_by;
   int          	group;
   int          	group_end;
   int          	item;
   size_offset_type	left;
   int          	list_idx;
   int          	list_item;
#ifdef KEY /* Bug 10177 */
   int          	prev_group = 0;
#else /* KEY Bug 10177 */
   int          	prev_group;
#endif /* KEY Bug 10177 */
   size_offset_type	result;
   size_offset_type	right;


   TRACE (Func_Entry, "merge_equivalence_groups2", NULL);

   group = SCP_FIRST_EQUIV_GRP(curr_scp_idx);

   while (group != NULL_IDX) {

      if (EQ_MERGED(group)) {

         /* This group has been merged with a previous   */
         /* group, so remove it from the group list.     */
         
         EQ_NEXT_EQUIV_GRP(prev_group) = EQ_NEXT_EQUIV_GRP(group);
      }
      else {
         group_end	= EQ_GRP_END_IDX(group);
         item		= group;
   
         while (item != NULL_IDX) {

            if (EQ_ERROR(item)) {
               item		= EQ_NEXT_EQUIV_OBJ(item);
               continue;
            }

            if (ATD_CLASS(EQ_ATTR_IDX(item)) == Variable &&
                ATD_EQUIV_LIST(EQ_ATTR_IDX(item)) != NULL_IDX) {

               /* This attr is in more than one equivalence group. */

               list_idx = ATD_EQUIV_LIST(EQ_ATTR_IDX(item));

               while (list_idx != NULL_IDX) {
                  list_item = AL_EQ_IDX(list_idx);

                  if (list_item != item && EQ_GRP_IDX(list_item) != group) {

                     /* Do not merge if item is already in this group.   */

                     /* 1) Merge the new group to the end of the old.    */
                     /* 2) Mark the merged group as merged, so it can    */
                     /*    be removed from the group list.               */
                     /* 3) Adjust the offsets for all groups if the      */
                     /*    offsets are different.                        */

                     if (EQ_OFFSET_IDX(list_item) != EQ_OFFSET_IDX(item) ||
                         EQ_OFFSET_FLD(list_item) != EQ_OFFSET_FLD(item)) {
                        left.fld	= EQ_OFFSET_FLD(list_item);
                        left.idx	= EQ_OFFSET_IDX(list_item);
                        right.fld	= EQ_OFFSET_FLD(item);
                        right.idx	= EQ_OFFSET_IDX(item);

                        if (!size_offset_binary_calc(&left,
                                                     &right,
                                                      Minus_Opr,
                                                     &adjust_by)) {
                           adjust = FALSE;
                           break;
                        }
                        adjust = TRUE;
                     }
                     else {
                        adjust = FALSE;
                     }

                     EQ_NEXT_EQUIV_OBJ(group_end)	= EQ_GRP_IDX(list_item);
                     EQ_MERGED(EQ_GRP_IDX(list_item))	= TRUE;

                     group_end	= EQ_GRP_END_IDX(EQ_GRP_IDX(list_item));
                     list_item	= EQ_GRP_IDX(list_item);  /* Group start */

                     if (adjust) {

                        while (list_item != NULL_IDX) {
                           EQ_GRP_IDX(list_item)= group;
                           left.fld		= EQ_OFFSET_FLD(list_item);
                           left.idx		= EQ_OFFSET_IDX(list_item);

                           if (!size_offset_binary_calc(&left,
                                                        &adjust_by,
                                                         Minus_Opr,
                                                        &result)) {
                              break;
                           }

                           if (result.fld == NO_Tbl_Idx) {
                              EQ_OFFSET_FLD(list_item) = CN_Tbl_Idx;
                              EQ_OFFSET_IDX(list_item) = ntr_const_tbl(
                                                               result.type_idx,
                                                               FALSE,
                                                               result.constant);
                           }
                           else {
                              EQ_OFFSET_FLD(list_item) = result.fld;
                              EQ_OFFSET_IDX(list_item) = result.idx;
                           }

                           list_item		= EQ_NEXT_EQUIV_OBJ(list_item);
                        }
                     }
                     else {
                        while (list_item != NULL_IDX) {
                           EQ_GRP_IDX(list_item)	= group;
                           list_item		= EQ_NEXT_EQUIV_OBJ(list_item);
                        }
                     }
                  }
                  list_idx	= AL_NEXT_IDX(list_idx);
               }
            }
            item		= EQ_NEXT_EQUIV_OBJ(item);
         }
         EQ_GRP_END_IDX(group)	= group_end;
         prev_group		= group;
      }
      group			= EQ_NEXT_EQUIV_GRP(group);
   }

   TRACE (Func_Exit, "merge_equivalence_groups2", NULL);

   return;

}  /* merge_equivalence_groups2 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Assign offsets to the items in equivalence groups.                    *|
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
static void     assign_offsets_for_equiv_groups(void)
{
   int			group;
   int			item;
   size_offset_type	largest_offset;
   size_offset_type	result;


   TRACE (Func_Entry, "assign_offsets_for_equiv_groups", NULL);

   group = SCP_FIRST_EQUIV_GRP(curr_scp_idx);

   while (group != NULL_IDX) {

      item		 = group;
      largest_offset.idx = CN_INTEGER_ZERO_IDX;
      largest_offset.fld = CN_Tbl_Idx;

      while (item != NULL_IDX) {

         if (!EQ_ERROR(item) &&
             IL_IDX(EQ_LIST_IDX(item)) != CN_INTEGER_ZERO_IDX &&
             fold_relationals(IL_IDX(EQ_LIST_IDX(item)),
                              largest_offset.idx,
                              Ge_Opr)) {
            largest_offset.fld	= IL_FLD(EQ_LIST_IDX(item));
            largest_offset.idx	= IL_IDX(EQ_LIST_IDX(item));
         }

         item = EQ_NEXT_EQUIV_OBJ(item);
      }

      if (largest_offset.idx != CN_INTEGER_ZERO_IDX) {

         /* If the largest is zero - then they are all zero, */
         /* so we don't need to do the subtraction.          */

         item = group;

         while (item != NULL_IDX) {

            if (EQ_ERROR(item)) {
               item = EQ_NEXT_EQUIV_OBJ(item);
               continue;
            }

            /* largest_offset_idx - IL_IDX(EQ_LIST_IDX(item)) */

            if (fold_relationals(IL_IDX(EQ_LIST_IDX(item)),
                                 CN_INTEGER_ZERO_IDX,
                                 Eq_Opr)) {
               EQ_OFFSET_FLD(item)	= largest_offset.fld;
               EQ_OFFSET_IDX(item)	= largest_offset.idx;
            }
            else {
               result.fld		= IL_FLD(EQ_LIST_IDX(item));
               result.idx		= IL_IDX(EQ_LIST_IDX(item));

               if (size_offset_binary_calc(&largest_offset,
                                           &result,
                                            Minus_Opr,
                                           &result)) {

                  if (result.fld == NO_Tbl_Idx) {
                     EQ_OFFSET_FLD(item) = CN_Tbl_Idx;
                     EQ_OFFSET_IDX(item) = ntr_const_tbl(result.type_idx,
                                                         FALSE,
                                                         result.constant);
                  }
                  else {
                     EQ_OFFSET_FLD(item) = result.fld;
                     EQ_OFFSET_IDX(item) = result.idx;
                  }
               }
               else {
                   break;
               }
            }
            item = EQ_NEXT_EQUIV_OBJ(item);
         }
      }

      group = EQ_NEXT_EQUIV_GRP(group);
   }


   TRACE (Func_Exit, "assign_offsets_for_equiv_groups", NULL);

   return;

}  /* assign_offsets_for_equiv_groups */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine resolves the lower and upper bounds to a constant or a   *|
|*	temp.  Calculate the extent and stride multiplier for each dimension. *|
|*									      *|
|* Input parameters:							      *|
|*      attr_idx -> Index to attribute for array.                             *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
void	array_dim_resolution(int 	attr_idx,
			     boolean	need_const_array)
{
   bd_array_size_type	array_size_type;
   int			at_idx;
   int			bd_idx;
   int			column;
   int			cvrt_idx;
   int			dim;
   int			entry_count;
   int			entry_list;
   expr_arg_type	expr_desc;
   int			extent_entry_idx	= NULL_IDX;
   fld_type		extent_fld;
   int			extent_idx;
   int			ir_idx;
   boolean		is_interface;
   int			len_ir_idx;
   int			length_idx;
   int			length_entry_idx	= NULL_IDX;
   int			line;
   int			mult_idx;
   int			new_bd_idx;
   int			next_ir_idx;
   opnd_type		opnd;
   int			sh_idx;
   int			stride_entry_idx	= NULL_IDX;
   int			stride_entry_count;
   size_offset_type	stride;
#ifdef KEY /* Bug 10177 */
   int			type = 0;
#else /* KEY Bug 10177 */
   int			type;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "array_dim_resolution", NULL);

   is_interface	= SCP_IS_INTERFACE(curr_scp_idx);
   bd_idx	= ATD_ARRAY_IDX(attr_idx);

   if (ATD_CLASS(attr_idx) == Function_Result) {
      entry_list	= ATP_NO_ENTRY_LIST(ATD_FUNC_IDX(attr_idx));
   }
   else {
      entry_list	= ATD_NO_ENTRY_LIST(attr_idx);
   }

   if (BD_ARRAY_CLASS(bd_idx) == Deferred_Shape) {

      /* This is called by PARAMETER processing.  This must be an explicit */
      /* shape constant size array.  PARAMETER processing will issue the   */
      /* error.  If this is needed elsewhere, it will come through again   */
      /* during decl_semantics.                                            */

      if (need_const_array) {
         goto EXIT;
      }

      if (ATD_CLASS(attr_idx) == Compiler_Tmp && ATD_IM_A_DOPE(attr_idx)) {
         goto EXIT;              /* everything is ok */
      }

      ATD_IM_A_DOPE(attr_idx) = TRUE;

#ifdef KEY /* Bug 6845 */
      if (ATD_CLASS(attr_idx) == Dummy_Argument &&
        !(ATD_POINTER(attr_idx) || ATD_ALLOCATABLE(attr_idx)))
#else /* KEY Bug 6845 */
      if (ATD_CLASS(attr_idx) == Dummy_Argument && !ATD_POINTER(attr_idx))
#endif /* KEY Bug 6845 */
      {

         /* Don't convert intrinsic dargs to assumed shape */

         if (ATD_INTRIN_DARG(attr_idx)) {
            goto EXIT;
         }

         new_bd_idx			= reserve_array_ntry(BD_RANK(bd_idx));
         BD_RANK(new_bd_idx)		= BD_RANK(bd_idx);
         BD_DCL_ERR(new_bd_idx)		= BD_DCL_ERR(bd_idx);
         BD_ARRAY_CLASS(new_bd_idx)	= Assumed_Shape;
         BD_ARRAY_SIZE(new_bd_idx)	= Constant_Size;
         BD_LINE_NUM(new_bd_idx)	= BD_LINE_NUM(bd_idx);
         BD_COLUMN_NUM(new_bd_idx)	= BD_COLUMN_NUM(bd_idx);

         for (dim = 1; dim <= BD_RANK(new_bd_idx); dim++) {
            BD_LB_FLD(new_bd_idx, dim)	= CN_Tbl_Idx;
            BD_LB_IDX(new_bd_idx, dim)	= CN_INTEGER_ONE_IDX;
         }

         bd_idx				= ntr_array_in_bd_tbl(new_bd_idx);
         BD_ARRAY_SIZE(bd_idx)		= Constant_Size;
         BD_RESOLVED(bd_idx)		= TRUE;
         ATD_ARRAY_IDX(attr_idx)	= bd_idx;

         if (ATD_IGNORE_TKR(attr_idx)) {
            AT_DCL_ERR(attr_idx)	= TRUE;
#ifdef KEY /* Bug 5040 */
            PRINTMSG(AT_DEF_LINE(attr_idx), 1459, Error, 
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     "IGNORE_TKR",
                     "assumed-shape DIMENSION",
		     AT_DEF_LINE(attr_idx));
#else /* KEY Bug 5040 */
            PRINTMSG(AT_DEF_LINE(attr_idx), 1459, Error, 
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     "IGNORE_TKR",
                     "assumed-shape DIMENSION");
#endif /* KEY Bug 5040 */
         }

# if defined(_TARGET_OS_MAX)
         if (ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX) {
            AT_DCL_ERR(attr_idx)        = TRUE;
            PRINTMSG(BD_LINE_NUM(ATD_PE_ARRAY_IDX(attr_idx)), 1583, Error,
                     BD_COLUMN_NUM(ATD_PE_ARRAY_IDX(attr_idx)),
                     "co-array dimensions",
                     "assumed-shape arrays");
         }
# endif
      }
      else if (!ATD_POINTER(attr_idx) && !ATD_ALLOCATABLE(attr_idx)) {
         AT_DCL_ERR(attr_idx)		= TRUE;

         if (ATD_CLASS(attr_idx) == Function_Result) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 571, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
         else {
            PRINTMSG(AT_DEF_LINE(attr_idx), 353, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
      }
      else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character &&
               TYP_CHAR_CLASS(ATD_TYPE_IDX(attr_idx)) == Var_Len_Char) {
         entry_list = merge_entry_lists(entry_list,
                           ATD_NO_ENTRY_LIST(TYP_IDX(ATD_TYPE_IDX(attr_idx))));

         if (entry_list != NULL_IDX &&
             (SCP_ALT_ENTRY_CNT(curr_scp_idx)+1) == AL_ENTRY_COUNT(entry_list)){
            PRINTMSG(AT_DEF_LINE(attr_idx), 662, Error,
                     AT_DEF_COLUMN(attr_idx), 
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx)	= TRUE;
         }
      }
      goto EXIT;
   }

   if (BD_ARRAY_CLASS(bd_idx) == Assumed_Shape) {

      /* This is called by PARAMETER processing.  This must be an explicit */
      /* shape constant size array.  PARAMETER processing will issue the   */
      /* error.  If this is needed elsewhere, it will come through again   */
      /* during decl_semantics.                                            */

      if (need_const_array) {
         goto EXIT;
      }

      /* These must always be dummy arguments, so they can never be automatic */

      ATD_IM_A_DOPE(attr_idx) = TRUE;

      if (!BD_RESOLVED(bd_idx)) {
         BD_RESOLVED(bd_idx)	= TRUE;
         array_size_type	= Constant_Size;
         length_entry_idx	= NULL_IDX;

         for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

            if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
               at_idx = BD_LB_IDX(bd_idx, dim);

               if (ATD_CLASS(at_idx) == Constant) {
                  BD_LB_FLD(bd_idx, dim)	= CN_Tbl_Idx;
                  BD_LB_IDX(bd_idx, dim)	= ATD_CONST_IDX(at_idx);
               }
               else if (ATD_SYMBOLIC_CONSTANT(at_idx)) {
                  array_size_type		= Symbolic_Constant_Size;
               }
               else {
                  length_entry_idx = merge_entry_lists(
                                      length_entry_idx,
                                      ATD_NO_ENTRY_LIST(BD_LB_IDX(bd_idx,dim)));
                  array_size_type  = Var_Len_Array;
               }
            }
         }

         BD_ARRAY_SIZE(bd_idx) = array_size_type;

         if (length_entry_idx != NULL_IDX) {
            entry_count	= SCP_ALT_ENTRY_CNT(curr_scp_idx) + 1; 

            if (entry_count == AL_ENTRY_COUNT(length_entry_idx))  {

               /* Error if problem with lower and/or upper bounds coming in   */
               /* different entry points.  Bounds for this array declaration  */
               /* cannot be calculated at any entry point, because dummy args */
               /* used in the expression do not enter at all the same points. */

               PRINTMSG(AT_DEF_LINE(attr_idx), 660, Error,
                        AT_DEF_COLUMN(attr_idx), 
                        AT_OBJ_NAME_PTR(attr_idx));
               AT_DCL_ERR(attr_idx)	= TRUE;
            }
            else {

               if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character &&
                   TYP_FLD(ATD_TYPE_IDX(attr_idx)) == AT_Tbl_Idx) {

                  length_entry_idx = merge_entry_lists(length_entry_idx,
                            ATD_NO_ENTRY_LIST(TYP_IDX(ATD_TYPE_IDX(attr_idx))));

                  if (entry_count == AL_ENTRY_COUNT(length_entry_idx))  {

                     /* Bounds for this array declaration cannot be calculated*/
                     /* at any entry point, because dummy arguments used in   */
                     /* the expression do not enter at all the same points.   */

                     PRINTMSG(AT_DEF_LINE(attr_idx), 661, Error,
                              AT_DEF_COLUMN(attr_idx), 
                              AT_OBJ_NAME_PTR(attr_idx));
                     AT_DCL_ERR(attr_idx)	= TRUE;
                  }
               }

               if (!AT_DCL_ERR(attr_idx) && entry_list != NULL_IDX) {
                  length_entry_idx = merge_entry_lists(length_entry_idx,
                                                       entry_list);

                  if (length_entry_idx != NULL_IDX &&
                      entry_count == AL_ENTRY_COUNT(length_entry_idx))  {

                     /* This array and its bounds variables do not enter at   */
                     /* the same entry point.                                 */

                     PRINTMSG(AT_DEF_LINE(attr_idx), 662, Error,
                              AT_DEF_COLUMN(attr_idx), 
                              AT_OBJ_NAME_PTR(attr_idx));
                     AT_DCL_ERR(attr_idx)	= TRUE;
                  }
               }
            }
         }
      }
             
      if (ATD_CLASS(attr_idx) != Dummy_Argument || ATD_POINTER(attr_idx)) {
         AT_DCL_ERR(attr_idx) = TRUE;
         PRINTMSG(AT_DEF_LINE(attr_idx), 351, Error,
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx));
      }

      goto EXIT;
   }

   /* If this array bounds entry has already been resolved, skip the section  */
   /* that calculates the extent, length, and stride multiplier.              */
   /* The only array entries that are shared are of the same type.  Each attr */
   /* will have to calculate it's own automatic stuff.                        */

   if (BD_RESOLVED(bd_idx)) {
      goto NEXT;
   }

   array_size_type	= Constant_Size;

   for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

      if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx) {

         if (ATD_CLASS(BD_LB_IDX(bd_idx, dim)) == Constant) { 
            BD_LB_FLD(bd_idx, dim)	= CN_Tbl_Idx;
            BD_LB_IDX(bd_idx, dim)	= ATD_CONST_IDX(BD_LB_IDX(bd_idx, dim));
         }
         else if (ATD_SYMBOLIC_CONSTANT(BD_LB_IDX(bd_idx, dim))) {
            array_size_type	= Symbolic_Constant_Size;
         }
         else {
            array_size_type	= Var_Len_Array;
            OPND_FLD(opnd)	= BD_LB_FLD(bd_idx, dim);
            OPND_IDX(opnd)	= BD_LB_IDX(bd_idx, dim);
            OPND_LINE_NUM(opnd)	= BD_LINE_NUM(bd_idx);
            OPND_COL_NUM(opnd)	= BD_COLUMN_NUM(bd_idx);
         }
      }
   
      if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx) {

         if (ATD_CLASS(BD_UB_IDX(bd_idx, dim)) == Constant) {
            BD_UB_FLD(bd_idx, dim)	= CN_Tbl_Idx;
            BD_UB_IDX(bd_idx, dim)	= ATD_CONST_IDX(BD_UB_IDX(bd_idx, dim));
         }
         else if (ATD_SYMBOLIC_CONSTANT(BD_UB_IDX(bd_idx, dim))) {

            if (array_size_type != Var_Len_Array) {
               array_size_type	= Symbolic_Constant_Size;
            }
         }
         else {
            array_size_type	= Var_Len_Array;
            OPND_FLD(opnd)	= BD_UB_FLD(bd_idx, dim);
            OPND_IDX(opnd)	= BD_UB_IDX(bd_idx, dim);
            OPND_LINE_NUM(opnd)	= BD_LINE_NUM(bd_idx);
            OPND_COL_NUM(opnd)	= BD_COLUMN_NUM(bd_idx);
         }
      }
   }

   if (BD_ARRAY_CLASS(bd_idx) == Assumed_Size) {

      /* This is called by PARAMETER processing.  This must be an explicit */
      /* shape constant size array.  PARAMETER processing will issue the   */
      /* error.  If this is needed elsewhere, it will come through again   */
      /* during decl_semantics.                                            */

      if (need_const_array) {
         goto EXIT;
      }

      BD_ARRAY_SIZE(bd_idx)	= array_size_type;
   }
   else { 
      BD_ARRAY_SIZE(bd_idx)	= array_size_type;

      if (array_size_type == Var_Len_Array) {

         BD_ARRAY_SIZE(bd_idx)	= Var_Len_Array;

         /* This is called by PARAMETER processing.  This must be an explicit */
         /* shape constant size array.  PARAMETER processing will issue the   */
         /* error.  If this is needed elsewhere, it will come through again   */
         /* during decl_semantics.                                            */

         if (need_const_array) {
            goto EXIT;
         }

         if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Function &&
             ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Subroutine) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 131, Error,
                     AT_DEF_COLUMN(attr_idx), 
                     AT_OBJ_NAME_PTR(attr_idx));
            BD_DCL_ERR(bd_idx) = TRUE;
         }
      }
   }

   BD_RESOLVED(bd_idx)	= TRUE;

   set_stride_for_first_dim(ATD_TYPE_IDX(attr_idx), &stride);

   if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character &&
      stride.fld == AT_Tbl_Idx && 
      ATD_NO_ENTRY_LIST(stride.idx) != NULL_IDX) {
      stride_entry_idx	= merge_entry_lists(NULL_IDX,
                                            ATD_NO_ENTRY_LIST(stride.idx));
   }
   else {
      stride_entry_idx	= NULL_IDX;
   }

   NTR_IR_TBL(len_ir_idx);
   IR_TYPE_IDX(len_ir_idx) = SA_INTEGER_DEFAULT_TYPE;

   BD_LEN_IDX(bd_idx)	= len_ir_idx;   /* Save this so it can be folded */
   BD_LEN_FLD(bd_idx)	= IR_Tbl_Idx;
   length_entry_idx	= NULL_IDX;
   line			= BD_LINE_NUM(bd_idx);
   column		= BD_COLUMN_NUM(bd_idx);

   for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {
      BD_SM_FLD(bd_idx, dim)	= stride.fld;
      BD_SM_IDX(bd_idx, dim)	= stride.idx;

      if (extent_entry_idx != NULL_IDX) {
         free_attr_list(extent_entry_idx);
         extent_entry_idx	= NULL_IDX;
      }

      if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
         at_idx = BD_LB_IDX(bd_idx, dim);

         if (ATD_NO_ENTRY_LIST(at_idx) != NULL_IDX) {
            extent_entry_idx = merge_entry_lists(NULL_IDX, 
                                                 ATD_NO_ENTRY_LIST(at_idx));
         }
      }
   
      if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
         at_idx = BD_UB_IDX(bd_idx, dim);

         if (ATD_NO_ENTRY_LIST(at_idx) != NULL_IDX) {
            extent_entry_idx = merge_entry_lists(extent_entry_idx, 
                                                 ATD_NO_ENTRY_LIST(at_idx));
         }
      }

      if (BD_LB_FLD(bd_idx, dim) == CN_Tbl_Idx &&
          fold_relationals(BD_LB_IDX(bd_idx, dim),
                           CN_INTEGER_ONE_IDX,
                           Eq_Opr)) {

         /* If the lb is a one, just use the ub for the extent */

         extent_fld = BD_UB_FLD(bd_idx, dim);
         extent_idx = BD_UB_IDX(bd_idx, dim);
      }
      else {
         NTR_IR_TBL(ir_idx);			/* Create 1 - lower */
         IR_OPR(ir_idx)				= Minus_Opr;
         IR_TYPE_IDX(ir_idx)			= SA_INTEGER_DEFAULT_TYPE;
         IR_FLD_L(ir_idx)			= CN_Tbl_Idx;
         IR_IDX_L(ir_idx)			= CN_INTEGER_ONE_IDX;
         IR_LINE_NUM_L(ir_idx)			= line;
         IR_COL_NUM_L(ir_idx)			= column;
         IR_FLD_R(ir_idx)			= BD_LB_FLD(bd_idx, dim);
         IR_IDX_R(ir_idx)			= BD_LB_IDX(bd_idx, dim);
         IR_LINE_NUM_R(ir_idx)			= line;
         IR_COL_NUM_R(ir_idx)			= column;
         IR_LINE_NUM(ir_idx)			= line;
         IR_COL_NUM(ir_idx)			= column;

         NTR_IR_TBL(next_ir_idx);		/* Upper + (1 - lower) */
         IR_OPR(next_ir_idx)			= Plus_Opr;
         IR_TYPE_IDX(next_ir_idx)		= SA_INTEGER_DEFAULT_TYPE;
         IR_IDX_L(next_ir_idx)			= BD_UB_IDX(bd_idx, dim);
         IR_FLD_L(next_ir_idx)			= BD_UB_FLD(bd_idx, dim);
         IR_LINE_NUM_L(next_ir_idx)		= line;
         IR_COL_NUM_L(next_ir_idx)		= column;
         IR_FLD_R(next_ir_idx)			= IR_Tbl_Idx;
         IR_IDX_R(next_ir_idx)			= ir_idx;
         IR_LINE_NUM_R(next_ir_idx)		= line;
         IR_COL_NUM_R(next_ir_idx)		= column;
         IR_LINE_NUM(next_ir_idx)		= line;
         IR_COL_NUM(next_ir_idx)		= column;

         if (BD_ARRAY_SIZE(bd_idx) == Symbolic_Constant_Size) {
            IR_OPR(next_ir_idx)	= Symbolic_Plus_Opr;
            IR_OPR(ir_idx)	= Symbolic_Minus_Opr;
            extent_idx		= gen_compiler_tmp(line, column, Priv, TRUE);
            extent_fld		= AT_Tbl_Idx;

            ATD_SYMBOLIC_CONSTANT(extent_idx)	= TRUE;
            ATD_TYPE_IDX(extent_idx)		= SA_INTEGER_DEFAULT_TYPE;
            ATD_FLD(extent_idx)			= IR_Tbl_Idx;
            ATD_TMP_IDX(extent_idx)		= next_ir_idx;


            /* KAY - Some of this may be folded if they are both not */
            /*       symbolic constants.                             */
         }
         else {

            OPND_FLD(opnd)			= IR_Tbl_Idx;
            OPND_IDX(opnd)			= next_ir_idx;
            OPND_LINE_NUM(opnd)			= stmt_start_line;
            OPND_COL_NUM(opnd)			= stmt_start_col;

            sh_idx				= ntr_sh_tbl();
            SH_GLB_LINE(sh_idx)			= stmt_start_line;
            SH_COL_NUM(sh_idx)			= stmt_start_col;
            SH_STMT_TYPE(sh_idx)		= Automatic_Base_Size_Stmt;
            SH_COMPILER_GEN(sh_idx)		= TRUE;
            SH_P2_SKIP_ME(sh_idx)		= TRUE;

            expr_desc.rank = 0;
            xref_state     = CIF_No_Usage_Rec;

            /* This is in terms of tmps - so it will never   */
            /* generate more than one statement.             */

            issue_overflow_msg_719 = FALSE;

            if (!expr_semantics(&opnd, &expr_desc)) {

               if (need_to_issue_719) {
                      
                  need_to_issue_719	= FALSE;
                  PRINTMSG(AT_DEF_LINE(attr_idx), 951, Error,
                           AT_DEF_COLUMN(attr_idx), 
                           dim,
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               AT_DCL_ERR(attr_idx)	= TRUE;
            }

            if (OPND_FLD(opnd) == CN_Tbl_Idx) {
               extent_fld	= CN_Tbl_Idx;
               extent_idx	= OPND_IDX(opnd);
               FREE_SH_NODE(sh_idx);
            }
            else {
               extent_fld = AT_Tbl_Idx;
               extent_idx = ntr_bnds_sh_tmp_list(&opnd,
                                                 extent_entry_idx,
                                                 is_interface ? NULL_IDX:sh_idx,
                                                 FALSE,
                                                 SA_INTEGER_DEFAULT_TYPE);
            }
         }
      }

      if (extent_fld == CN_Tbl_Idx) {

         if (compare_cn_and_value(extent_idx, 0, Lt_Opr)) {
            extent_idx = CN_INTEGER_ZERO_IDX;
         }
      }
      else {  /* Generate  tmp = max(0, extent) */

         OPND_FLD(opnd)		= extent_fld;
         OPND_IDX(opnd)		= extent_idx;
         OPND_LINE_NUM(opnd)	= line;
         OPND_COL_NUM(opnd)	= column;

         gen_tmp_equal_max_zero(&opnd, 
                                SA_INTEGER_DEFAULT_TYPE,
                                extent_entry_idx,
                                (BD_ARRAY_SIZE(bd_idx)==Symbolic_Constant_Size),
                                is_interface);
         extent_fld		= OPND_FLD(opnd);
         extent_idx		= OPND_IDX(opnd);
      }

      BD_XT_FLD(bd_idx, dim)	= extent_fld;
      BD_XT_IDX(bd_idx, dim)	= extent_idx;

      /* STRIDE = STRIDE * (EXTENT of previous dimension) */
      /* Fix stride for next dimension.                   */
      /* Calculate length.                                */

      if (dim < BD_RANK(bd_idx)) {
         NTR_IR_TBL(ir_idx);		/* Create Stride * Extent */
         IR_OPR(ir_idx)			= Mult_Opr;
         IR_TYPE_IDX(ir_idx)		= SA_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)		= BD_LINE_NUM(bd_idx);
         IR_COL_NUM(ir_idx)		= BD_COLUMN_NUM(bd_idx);
         IR_FLD_L(ir_idx)		= stride.fld;
         IR_IDX_L(ir_idx)		= stride.idx;
         IR_LINE_NUM_L(ir_idx)		= BD_LINE_NUM(bd_idx);
         IR_COL_NUM_L(ir_idx)		= BD_COLUMN_NUM(bd_idx);
         IR_FLD_R(ir_idx)		= extent_fld;
         IR_IDX_R(ir_idx)		= extent_idx;
         IR_LINE_NUM_R(ir_idx)		= BD_LINE_NUM(bd_idx);
         IR_COL_NUM_R(ir_idx)		= BD_COLUMN_NUM(bd_idx);

         if (BD_ARRAY_SIZE(bd_idx) == Symbolic_Constant_Size) {
            IR_OPR(ir_idx)	= Symbolic_Mult_Opr;
            stride.fld		= AT_Tbl_Idx;
            stride.idx		= gen_compiler_tmp(line, column, Priv, TRUE);

            ATD_TYPE_IDX(stride.idx)		= SA_INTEGER_DEFAULT_TYPE;
            ATD_FLD(stride.idx)			= IR_Tbl_Idx;
            ATD_TMP_IDX(stride.idx)		= ir_idx;
            ATD_SYMBOLIC_CONSTANT(stride.idx)	= TRUE;
         }
         else {
            OPND_FLD(opnd)		= IR_Tbl_Idx;
            OPND_IDX(opnd)		= ir_idx;
            OPND_LINE_NUM(opnd)		= stmt_start_line;
            OPND_COL_NUM(opnd)		= stmt_start_col;

            sh_idx			= ntr_sh_tbl();
            SH_STMT_TYPE(sh_idx)	= Automatic_Base_Size_Stmt;
            SH_COMPILER_GEN(sh_idx)	= TRUE;
            SH_P2_SKIP_ME(sh_idx)	= TRUE;
            SH_GLB_LINE(sh_idx)		= stmt_start_line;
            SH_COL_NUM(sh_idx)		= stmt_start_col;

            expr_desc.rank 		= 0;
            xref_state     		= CIF_No_Usage_Rec;

            expr_semantics(&opnd, &expr_desc);

            if (OPND_FLD(opnd) == CN_Tbl_Idx) {
               stride.fld		= CN_Tbl_Idx;
               stride.idx		= OPND_IDX(opnd);
               FREE_SH_NODE(sh_idx);
            }
            else {

               if (!is_interface) {

                  /* Stride must be non-constant, if extent is non-constant */

                  if (extent_entry_idx != NULL_IDX) {
                     stride_entry_idx = merge_entry_lists(stride_entry_idx, 
                                                          extent_entry_idx);
                     length_entry_idx = merge_entry_lists(length_entry_idx, 
                                                          extent_entry_idx);
                  }
               }

               stride.fld = AT_Tbl_Idx;
               stride.idx = ntr_bnds_sh_tmp_list(&opnd,
                                              stride_entry_idx,
                                              (is_interface) ? NULL_IDX: sh_idx,
                                              FALSE,
                                              SA_INTEGER_DEFAULT_TYPE);
            }
         }

         NTR_IR_TBL(mult_idx);   /* Create length = extent * extent */
         IR_LINE_NUM(mult_idx)		= BD_LINE_NUM(bd_idx);
         IR_COL_NUM(mult_idx)		= BD_COLUMN_NUM(bd_idx);

         if (BD_ARRAY_SIZE(bd_idx) == Symbolic_Constant_Size) {
            IR_OPR(mult_idx)		= Symbolic_Mult_Opr;
         }
         else {
            IR_OPR(mult_idx)		= Mult_Opr;
         }

         IR_TYPE_IDX(mult_idx)          = SA_INTEGER_DEFAULT_TYPE;
         IR_IDX_R(len_ir_idx)		= mult_idx;
         IR_FLD_R(len_ir_idx)		= IR_Tbl_Idx;
         IR_LINE_NUM_R(len_ir_idx)	= BD_LINE_NUM(bd_idx);
         IR_COL_NUM_R(len_ir_idx)	= BD_COLUMN_NUM(bd_idx);
         IR_IDX_L(mult_idx)		= extent_idx;
         IR_FLD_L(mult_idx)		= extent_fld;
         IR_LINE_NUM_L(mult_idx)	= BD_LINE_NUM(bd_idx);
         IR_COL_NUM_L(mult_idx)		= BD_COLUMN_NUM(bd_idx);
         len_ir_idx			= mult_idx;
      }
      else if (dim == 1) {

         /* Last dimension is the only dimension, so length = xtent */

         BD_LEN_FLD(bd_idx)	= extent_fld;
         BD_LEN_IDX(bd_idx)	= extent_idx;
         length_entry_idx	= extent_entry_idx;
         stride_entry_idx	= merge_entry_lists(stride_entry_idx,
                                                    extent_entry_idx);
         extent_entry_idx	= NULL_IDX;  /* List now pointed by length.. */

         if (length_entry_idx != NULL_IDX) {  /* Alt entries - need tmp = 0 */
            gen_tmp_eq_zero_ir(extent_idx);
         }
      }

      /* Last dimension */

      else if (BD_ARRAY_SIZE(bd_idx) == Symbolic_Constant_Size) {
         IR_IDX_R(len_ir_idx)		= extent_idx;
         IR_FLD_R(len_ir_idx)		= extent_fld;
         IR_LINE_NUM_R(len_ir_idx)	= BD_LINE_NUM(bd_idx);
         IR_COL_NUM_R(len_ir_idx)	= BD_COLUMN_NUM(bd_idx);
         OPND_FLD(opnd)			= IR_FLD_R(BD_LEN_IDX(bd_idx));
         OPND_IDX(opnd)			= IR_IDX_R(BD_LEN_IDX(bd_idx));

         BD_LEN_FLD(bd_idx)	= AT_Tbl_Idx;
         BD_LEN_IDX(bd_idx)	= gen_compiler_tmp(line, column, Priv, TRUE);

         ATD_TYPE_IDX(BD_LEN_IDX(bd_idx))	= SA_INTEGER_DEFAULT_TYPE;
         ATD_FLD(BD_LEN_IDX(bd_idx))		= OPND_FLD(opnd);
         ATD_TMP_IDX(BD_LEN_IDX(bd_idx))	= OPND_IDX(opnd);

         ATD_SYMBOLIC_CONSTANT(BD_LEN_IDX(bd_idx))	= TRUE;
      }
      else {
         IR_IDX_R(len_ir_idx)		= extent_idx;
         IR_FLD_R(len_ir_idx)		= extent_fld;
         IR_LINE_NUM_R(len_ir_idx)	= BD_LINE_NUM(bd_idx);
         IR_COL_NUM_R(len_ir_idx)	= BD_COLUMN_NUM(bd_idx);
         OPND_FLD(opnd)			= IR_FLD_R(BD_LEN_IDX(bd_idx));
         OPND_IDX(opnd)			= IR_IDX_R(BD_LEN_IDX(bd_idx));
         OPND_LINE_NUM(opnd)            = BD_LINE_NUM(bd_idx);
         OPND_COL_NUM(opnd)             = BD_COLUMN_NUM(bd_idx);

         sh_idx				= ntr_sh_tbl();
         SH_STMT_TYPE(sh_idx)		= Automatic_Base_Size_Stmt;
         SH_COMPILER_GEN(sh_idx)	= TRUE;
         SH_P2_SKIP_ME(sh_idx)		= TRUE;
         SH_GLB_LINE(sh_idx)		= stmt_start_line;
         SH_COL_NUM(sh_idx)		= stmt_start_col;

         /* expr_semantics needs curr_stmt_sh_idx set to something valid.  */
         /* It does not need SH_IR_IDX(curr_stmt_sh_idx) set to something. */

         expr_desc.rank = 0;
         xref_state     = CIF_No_Usage_Rec;

# if defined(_CHECK_MAX_MEMORY)

         if (!target_t3e) {
            issue_overflow_msg_719 = FALSE;
         }
# endif

         if (!expr_semantics(&opnd, &expr_desc)) {

            if (need_to_issue_719) {

               /* We have overflowed - Reattempt with a bigger int type */

               if (OPND_FLD(opnd) == IR_Tbl_Idx) {
                  IR_TYPE_IDX(OPND_IDX(opnd)) = SA_INTEGER_DEFAULT_TYPE;

                  switch (IR_FLD_L(OPND_IDX(opnd))) {
                  case AT_Tbl_Idx:
                     type = TYP_LINEAR(ATD_TYPE_IDX(IR_IDX_L(OPND_IDX(opnd))));
                     break;

                  case IR_Tbl_Idx:
                     type = TYP_LINEAR(IR_TYPE_IDX(IR_IDX_L(OPND_IDX(opnd))));
                     break;

                  case CN_Tbl_Idx:
                     type = TYP_LINEAR(CN_TYPE_IDX(IR_IDX_L(OPND_IDX(opnd))));
                     break;
                  }

                  if (type < SA_INTEGER_DEFAULT_TYPE) {
                     NTR_IR_TBL(cvrt_idx);
                     IR_OPR(cvrt_idx)            = Cvrt_Opr;
                     IR_TYPE_IDX(cvrt_idx)       = SA_INTEGER_DEFAULT_TYPE;
                     IR_LINE_NUM(cvrt_idx)       = BD_LINE_NUM(bd_idx);
                     IR_COL_NUM(cvrt_idx)        = BD_COLUMN_NUM(bd_idx);
                     COPY_OPND(IR_OPND_L(cvrt_idx), IR_OPND_L(OPND_IDX(opnd)));
                     IR_FLD_L(OPND_IDX(opnd))    = IR_Tbl_Idx;
                     IR_IDX_L(OPND_IDX(opnd))    = cvrt_idx;
                  }

                  switch (IR_FLD_R(OPND_IDX(opnd))) {
                  case AT_Tbl_Idx:
                     type = TYP_LINEAR(ATD_TYPE_IDX(IR_IDX_R(OPND_IDX(opnd))));
                     break;

                  case IR_Tbl_Idx:
                     type = TYP_LINEAR(IR_TYPE_IDX(IR_IDX_R(OPND_IDX(opnd))));
                     break;

                  case CN_Tbl_Idx:
                     type = TYP_LINEAR(CN_TYPE_IDX(IR_IDX_R(OPND_IDX(opnd))));
                     break;
                  }

                  if (type < SA_INTEGER_DEFAULT_TYPE) {
                     NTR_IR_TBL(cvrt_idx);
                     IR_OPR(cvrt_idx)            = Cvrt_Opr;
                     IR_TYPE_IDX(cvrt_idx)       = SA_INTEGER_DEFAULT_TYPE;
                     IR_LINE_NUM(cvrt_idx)       = BD_LINE_NUM(bd_idx);
                     IR_COL_NUM(cvrt_idx)        = BD_COLUMN_NUM(bd_idx);
                     COPY_OPND(IR_OPND_L(cvrt_idx), IR_OPND_R(OPND_IDX(opnd)));
                     IR_FLD_R(OPND_IDX(opnd))    = IR_Tbl_Idx;
                     IR_IDX_R(OPND_IDX(opnd))    = cvrt_idx;
                  }
                  need_to_issue_719	= FALSE;
               }

               if (!expr_semantics(&opnd, &expr_desc)) {

                  if (!target_t3e) {
                     AT_DCL_ERR(attr_idx)	= TRUE;
                  }
               }
            }
            else if (!target_t3e) {
               AT_DCL_ERR(attr_idx)	= TRUE;
            }

            if (need_to_issue_719) {
               need_to_issue_719	= FALSE;
               AT_DCL_ERR(attr_idx)	= TRUE;
               ISSUE_STORAGE_SIZE_EXCEEDED_MSG(attr_idx, Error);
            }
         }

         if (OPND_FLD(opnd) == CN_Tbl_Idx) {
            BD_LEN_FLD(bd_idx)	= CN_Tbl_Idx;
            BD_LEN_IDX(bd_idx)	= OPND_IDX(opnd);
            FREE_SH_NODE(sh_idx);
         }
         else {

            if (!is_interface) {

               if (extent_entry_idx != NULL_IDX) {
                  stride_entry_idx = merge_entry_lists(stride_entry_idx, 
                                                       extent_entry_idx);
                  length_entry_idx = merge_entry_lists(length_entry_idx, 
                                                       extent_entry_idx);
               }
            }

            length_idx = ntr_bnds_sh_tmp_list(&opnd,
                                              length_entry_idx,
                                              (is_interface) ? NULL_IDX:sh_idx,
                                              TRUE,
                                              SA_INTEGER_DEFAULT_TYPE);
            BD_LEN_IDX(bd_idx) = length_idx;
            BD_LEN_FLD(bd_idx) = AT_Tbl_Idx;
         }
      }
   }

   /* After the dimensions are processed, stride_entry_idx contains a list   */
   /* of all bad entry points, for the array - including all extents and     */
   /* type information.  Stride is calculated from the (previous dimension's */
   /* extent) * (previous dimension's stride).  A stride_entry_idx is made   */
   /* for the last dimension, even though actual stride isn't calculated for */
   /* this dimension.                                                        */

   if (stride_entry_idx != NULL_IDX) {
      entry_count	= SCP_ALT_ENTRY_CNT(curr_scp_idx) + 1; 

      if (length_entry_idx != NULL_IDX &&
          entry_count == AL_ENTRY_COUNT(length_entry_idx))  {

         /* Error if problem with lower and/or upper bounds coming in    */
         /* different entry points.  Bounds for this array declaration   */
         /* cannot be calculated at any entry point, because dummy args  */
         /* used in the expression do not enter at all the same points.  */

         PRINTMSG(AT_DEF_LINE(attr_idx), 660, Error,
                  AT_DEF_COLUMN(attr_idx), 
                  AT_OBJ_NAME_PTR(attr_idx));
         AT_DCL_ERR(attr_idx)	= TRUE;
      }
      else if (entry_count == AL_ENTRY_COUNT(stride_entry_idx))  {

         /* If the length is okay, but there's a problem with the        */
         /* stride, that means that it's a character and a bound         */
         /* forming the char length, doesn't enter the same as all       */
         /* the dimension bounds.  Bounds for this array declaration     */
         /* cannot be calculated at any entry point, because dummy args  */
         /* used in the expression de not enter at all the same points.  */

         PRINTMSG(AT_DEF_LINE(attr_idx), 661, Error,
                  AT_DEF_COLUMN(attr_idx), 
                  AT_OBJ_NAME_PTR(attr_idx));
         AT_DCL_ERR(attr_idx)	= TRUE;
      }
      else if (entry_list != NULL_IDX) {
         stride_entry_count = merge_entry_list_count(stride_entry_idx,
                                                     entry_list);

         if (entry_count == stride_entry_count) {

            /* This array and its bound variables do not enter at the  */
            /* same entry point.                                       */

            PRINTMSG(AT_DEF_LINE(attr_idx), 662, Error,
                     AT_DEF_COLUMN(attr_idx), 
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx)	= TRUE;
         }
      }
   }

NEXT:

   /* Every array must have the following semantic checks.  So even if the   */
   /* bounds for the array are already resolved, it still must get these     */
   /* checks.                                                                */

   if (BD_ARRAY_CLASS(bd_idx) == Explicit_Shape &&
       BD_ARRAY_SIZE(bd_idx) == Constant_Size) {

      /* Check so the item does not exceed max storage size.  Do it here,     */
      /* even though it is also done in final_decl_semantics because this     */
      /* may be a constant array involved in data or parameter statements or  */
      /* it may get folded.                                                   */

      stor_bit_size_of(attr_idx, TRUE, TRUE);
   }
   else if (need_const_array) {  

      /* Need an explicit_shape constant size array for parameter processing */
      /* An error will be issued in PARAMETER processing if this isn't a     */
      /* constant size array.                                                */

      /* This if block is intentionally blank. */
   }
   else {

      if (BD_ARRAY_SIZE(bd_idx) == Symbolic_Constant_Size) {
         fnd_semantic_err(Obj_Sym_Constant_Arr,
                          AT_DEF_LINE(attr_idx),
                          AT_DEF_COLUMN(attr_idx),
                          attr_idx,
                          TRUE);

         if (ATD_STOR_BLK_IDX(attr_idx) != NULL_IDX) {
            SB_BLK_HAS_NPES(ATD_STOR_BLK_IDX(attr_idx)) = TRUE;
         }

         if (cmd_line_flags.malleable) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 1232, Error,
                     AT_DEF_COLUMN(attr_idx));
         }
      }

      if (BD_ARRAY_CLASS(bd_idx) == Assumed_Size) {

         /* This is called by PARAMETER processing.  This must be an explicit */
         /* shape constant size array.  PARAMETER processing will issue the   */
         /* error.  If this is needed elsewhere, it will come through again   */
         /* during decl_semantics.                                            */

         if (ATD_CLASS(attr_idx) == CRI__Pointee) {

            if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
               AT_DCL_ERR(attr_idx) = TRUE;
               PRINTMSG(AT_DEF_LINE(attr_idx), 1419, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
         }
         else if (ATD_CLASS(attr_idx) != Dummy_Argument) {

            /* Must be dummy arg or CRI pointee.  */

            if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
               AT_DCL_ERR(attr_idx) = TRUE;
               PRINTMSG(AT_DEF_LINE(attr_idx), 501, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
            else {
               AT_DCL_ERR(attr_idx) = TRUE;
               PRINTMSG(AT_DEF_LINE(attr_idx), 500, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
         }

# if defined(_TARGET_OS_MAX)
         if (ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX) {
            AT_DCL_ERR(attr_idx)        = TRUE;
            PRINTMSG(BD_LINE_NUM(ATD_PE_ARRAY_IDX(attr_idx)), 1583, Error,
                     BD_COLUMN_NUM(ATD_PE_ARRAY_IDX(attr_idx)),
                     "co-array dimensions",
                     "assumed-size arrays");
         }
# endif
      }
      else if (BD_ARRAY_SIZE(bd_idx) == Var_Len_Array) {
         fnd_semantic_err(Obj_Var_Len_Arr,
                          AT_DEF_LINE(attr_idx),
                          AT_DEF_COLUMN(attr_idx),
                          attr_idx,
                          TRUE);

         if (ATD_CLASS(attr_idx) == Variable) {

            if (ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX) {
               AT_DCL_ERR(attr_idx)     = TRUE;
               PRINTMSG(AT_DEF_LINE(attr_idx), 1577, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
            else {
               ATD_AUTOMATIC(attr_idx) = TRUE;
            }

            if (stride_entry_idx != NULL_IDX) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1046, Caution,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
         }
      }
   }
 

EXIT:

   if (stride_entry_idx != NULL_IDX) {
      free_attr_list(stride_entry_idx);
   }

   if (length_entry_idx != NULL_IDX) {
      free_attr_list(length_entry_idx);
   }

   if (ATD_CLASS(attr_idx) == Function_Result) {
      ATP_NO_ENTRY_LIST(ATD_FUNC_IDX(attr_idx))	= entry_list;
   }
   else {
      ATD_NO_ENTRY_LIST(attr_idx)		= entry_list;
   }

   TRACE (Func_Exit, "array_dim_resolution", NULL);

   return;

}  /* array_dim_resolution */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine resolves the lower and upper bounds to a constant or a   *|
|*      temp.  Calculate the extent and stride multiplier for each dimension. *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      attr_idx -> Index to attribute for array.                             *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

void    pe_array_dim_resolution(int        attr_idx)

{
   bd_array_size_type   array_size_type;
   int                  at_idx;
   int                  bd_idx;
   int                  dim;
   int                  entry_count;
   int                  entry_list;
   expr_arg_type        expr_desc;
   int                  extent_entry_idx	= NULL_IDX;
   fld_type             extent_fld;
   int                  extent_idx;
   int                  ir_idx;
   boolean              is_interface;
   int                  len_ir_idx;
   int                  length_idx;
   int                  length_entry_idx	= NULL_IDX;
   int                  mult_idx;
   int                  next_ir_idx;
   opnd_type            opnd;
   int                  sh_idx;
   int                  stride_entry_idx        = NULL_IDX;
   int                  stride_entry_count;
   size_offset_type     stride;


   TRACE (Func_Entry, "pe_array_dim_resolution", NULL);

   is_interface = SCP_IS_INTERFACE(curr_scp_idx);
   bd_idx       = ATD_PE_ARRAY_IDX(attr_idx);

   if (ATD_CLASS(attr_idx) == Function_Result) {
      entry_list        = ATP_NO_ENTRY_LIST(ATD_FUNC_IDX(attr_idx));
   }
   else {
      entry_list        = ATD_NO_ENTRY_LIST(attr_idx);
   }

   if (BD_ARRAY_CLASS(bd_idx) == Deferred_Shape) {

      if (! ATD_ALLOCATABLE(attr_idx)) {
         PRINTMSG(AT_DEF_LINE(attr_idx), 1587, Error, AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx));
         BD_DCL_ERR(bd_idx) = TRUE;
         AT_DCL_ERR(attr_idx) = TRUE;
      }

      goto EXIT;
   }

   /* If this array bounds entry has already been resolved, skip the section  */
   /* that calculates the extent, length, and stride multiplier.              */
   /* The only array entries that are shared are of the same type.  Each attr */
   /* will have to calculate it's own automatic stuff.                        */

   if (BD_RESOLVED(bd_idx)) {
      goto NEXT;
   }

   array_size_type      = Constant_Size;

   for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

      if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx) {

         if (ATD_CLASS(BD_LB_IDX(bd_idx, dim)) == Constant) {
            BD_LB_FLD(bd_idx, dim)      = CN_Tbl_Idx;
            BD_LB_IDX(bd_idx, dim)      = ATD_CONST_IDX(BD_LB_IDX(bd_idx, dim));
         }
         else if (ATD_SYMBOLIC_CONSTANT(BD_LB_IDX(bd_idx, dim))) {
            array_size_type     = Symbolic_Constant_Size;
         }
         else {
            array_size_type     = Var_Len_Array;
         }
      }

      if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx) {

         if (ATD_CLASS(BD_UB_IDX(bd_idx, dim)) == Constant) {
            BD_UB_FLD(bd_idx, dim)      = CN_Tbl_Idx;
            BD_UB_IDX(bd_idx, dim)      = ATD_CONST_IDX(BD_UB_IDX(bd_idx, dim));
         }
         else if (ATD_SYMBOLIC_CONSTANT(BD_UB_IDX(bd_idx, dim))) {

            if (array_size_type != Var_Len_Array) {
               array_size_type  = Symbolic_Constant_Size;
            }
         }
         else {
            array_size_type     = Var_Len_Array;
         }
      }
   }

   if (BD_ARRAY_CLASS(bd_idx) == Assumed_Size) {

      /* This is called by PARAMETER processing.  This must be an explicit */
      /* shape constant size array.  PARAMETER processing will issue the   */
      /* error.  If this is needed elsewhere, it will come through again   */
      /* during decl_semantics.                                            */

      BD_ARRAY_SIZE(bd_idx)     = array_size_type;
   }
   else {
      BD_ARRAY_SIZE(bd_idx)     = array_size_type;

      if (array_size_type == Var_Len_Array) {

         BD_ARRAY_SIZE(bd_idx)  = Var_Len_Array;

         /* This is called by PARAMETER processing.  This must be an explicit */
         /* shape constant size array.  PARAMETER processing will issue the   */
         /* error.  If this is needed elsewhere, it will come through again   */
         /* during decl_semantics.                                            */

         if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Function &&
             ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Subroutine) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 131, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
            BD_DCL_ERR(bd_idx) = TRUE;
         }
      }
   }

   BD_RESOLVED(bd_idx)  = TRUE;

   /* stride for first pe dim is always 1 */

   stride.fld = CN_Tbl_Idx;
   stride.idx = CN_INTEGER_ONE_IDX;

   stride_entry_idx  = NULL_IDX;

   NTR_IR_TBL(len_ir_idx);
   IR_TYPE_IDX(len_ir_idx) = INTEGER_DEFAULT_TYPE;

   BD_LEN_IDX(bd_idx)   = len_ir_idx;   /* Save this so it can be folded */
   BD_LEN_FLD(bd_idx)   = IR_Tbl_Idx;
   length_entry_idx     = NULL_IDX;

   for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {
      BD_SM_FLD(bd_idx, dim)    = stride.fld;
      BD_SM_IDX(bd_idx, dim)    = stride.idx;

      if (extent_entry_idx != NULL_IDX) {
         free_attr_list(extent_entry_idx);
         extent_entry_idx          = NULL_IDX;
      }

      if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
         at_idx = BD_LB_IDX(bd_idx, dim);

         if (ATD_NO_ENTRY_LIST(at_idx) != NULL_IDX) {
            extent_entry_idx = merge_entry_lists(NULL_IDX,
                                                 ATD_NO_ENTRY_LIST(at_idx));
         }
      }

      if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
         at_idx = BD_UB_IDX(bd_idx, dim);

         if (ATD_NO_ENTRY_LIST(at_idx) != NULL_IDX) {
            extent_entry_idx = merge_entry_lists(extent_entry_idx,
                                                 ATD_NO_ENTRY_LIST(at_idx));
         }
      }

      if (BD_LB_FLD(bd_idx, dim) == CN_Tbl_Idx &&
          fold_relationals(BD_LB_IDX(bd_idx, dim),
                           CN_INTEGER_ONE_IDX,
                           Eq_Opr)) {

         /* If the lb is a one, just use the ub for the extent */

         extent_fld = BD_UB_FLD(bd_idx, dim);
         extent_idx = BD_UB_IDX(bd_idx, dim);
      }
      else {
         NTR_IR_TBL(ir_idx);                    /* Create 1 - lower */
         IR_OPR(ir_idx)                         = Minus_Opr;
         IR_TYPE_IDX(ir_idx)                    = INTEGER_DEFAULT_TYPE;
         IR_FLD_L(ir_idx)                       = CN_Tbl_Idx;
         IR_IDX_L(ir_idx)                       = CN_INTEGER_ONE_IDX;
         IR_LINE_NUM_L(ir_idx)                  = BD_LINE_NUM(bd_idx);
         IR_COL_NUM_L(ir_idx)                   = BD_COLUMN_NUM(bd_idx);
         IR_FLD_R(ir_idx)                       = BD_LB_FLD(bd_idx, dim);
         IR_IDX_R(ir_idx)                       = BD_LB_IDX(bd_idx, dim);
         IR_LINE_NUM_R(ir_idx)                  = BD_LINE_NUM(bd_idx);
         IR_COL_NUM_R(ir_idx)                   = BD_COLUMN_NUM(bd_idx);
         IR_LINE_NUM(ir_idx)                    = BD_LINE_NUM(bd_idx);
         IR_COL_NUM(ir_idx)                     = BD_COLUMN_NUM(bd_idx);

         NTR_IR_TBL(next_ir_idx);               /* Upper + (1 - lower) */
         IR_OPR(next_ir_idx)                    = Plus_Opr;
         IR_TYPE_IDX(next_ir_idx)               = INTEGER_DEFAULT_TYPE;
         IR_IDX_L(next_ir_idx)                  = BD_UB_IDX(bd_idx, dim);
         IR_FLD_L(next_ir_idx)                  = BD_UB_FLD(bd_idx, dim);
         IR_LINE_NUM_L(next_ir_idx)             = BD_LINE_NUM(bd_idx);
         IR_COL_NUM_L(next_ir_idx)              = BD_COLUMN_NUM(bd_idx);
         IR_FLD_R(next_ir_idx)                  = IR_Tbl_Idx;
         IR_IDX_R(next_ir_idx)                  = ir_idx;
         IR_LINE_NUM_R(next_ir_idx)             = BD_LINE_NUM(bd_idx);
         IR_COL_NUM_R(next_ir_idx)              = BD_COLUMN_NUM(bd_idx);
         IR_LINE_NUM(next_ir_idx)               = BD_LINE_NUM(bd_idx);
         IR_COL_NUM(next_ir_idx)                = BD_COLUMN_NUM(bd_idx);

         if (BD_ARRAY_SIZE(bd_idx) == Symbolic_Constant_Size) {
            IR_OPR(next_ir_idx) = Symbolic_Plus_Opr;
            IR_OPR(ir_idx)      = Symbolic_Minus_Opr;
            extent_idx          = gen_compiler_tmp(BD_LINE_NUM(bd_idx),
                                                   BD_COLUMN_NUM(bd_idx),
                                                   Priv, TRUE);
            extent_fld          = AT_Tbl_Idx;

            ATD_SYMBOLIC_CONSTANT(extent_idx)   = TRUE;
            ATD_TYPE_IDX(extent_idx)            = CG_INTEGER_DEFAULT_TYPE;
            ATD_FLD(extent_idx)                 = IR_Tbl_Idx;
            ATD_TMP_IDX(extent_idx)             = next_ir_idx;


            /* KAY - Some of this may be folded if they are both not */
            /*       symbolic constants.                             */
         }
         else {

            OPND_FLD(opnd)                      = IR_Tbl_Idx;
            OPND_IDX(opnd)                      = next_ir_idx;
            OPND_LINE_NUM(opnd)                 = stmt_start_line;
            OPND_COL_NUM(opnd)                  = stmt_start_col;

            sh_idx                              = ntr_sh_tbl();
            SH_GLB_LINE(sh_idx)                 = stmt_start_line;
            SH_COL_NUM(sh_idx)                  = stmt_start_col;
            SH_STMT_TYPE(sh_idx)                = Automatic_Base_Size_Stmt;
            SH_COMPILER_GEN(sh_idx)             = TRUE;
            SH_P2_SKIP_ME(sh_idx)               = TRUE;

            expr_desc.rank = 0;
            xref_state     = CIF_No_Usage_Rec;

            /* This is in terms of tmps - so it will never   */
            /* generate more than one statement.             */

            if (!expr_semantics(&opnd, &expr_desc)) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 951, Error,
                        AT_DEF_COLUMN(attr_idx),
                        dim,
                        AT_OBJ_NAME_PTR(attr_idx));
               AT_DCL_ERR(attr_idx)     = TRUE;
            }

            if (OPND_FLD(opnd) == CN_Tbl_Idx) {
               extent_fld       = CN_Tbl_Idx;
               extent_idx       = OPND_IDX(opnd);
               FREE_SH_NODE(sh_idx);
            }
            else {
               extent_fld       = AT_Tbl_Idx;
               extent_idx       = ntr_bnds_sh_tmp_list(&opnd,
                                               extent_entry_idx,
                                               (is_interface) ? NULL_IDX:sh_idx,
                                               FALSE,
                                               SA_INTEGER_DEFAULT_TYPE);
            }
         }
      }

      if (extent_fld == CN_Tbl_Idx) {

         if (compare_cn_and_value(extent_idx, 0, Lt_Opr)) {
            extent_idx = CN_INTEGER_ZERO_IDX;
         }
      }
      else {  /* Generate  tmp = max(0, extent) */

         OPND_FLD(opnd)		= extent_fld;
         OPND_IDX(opnd)		= extent_idx;
         OPND_LINE_NUM(opnd)	= BD_LINE_NUM(bd_idx);
         OPND_COL_NUM(opnd)	= BD_COLUMN_NUM(bd_idx);

         gen_tmp_equal_max_zero(&opnd, 
                                INTEGER_DEFAULT_TYPE,
                                extent_entry_idx,
                                (BD_ARRAY_SIZE(bd_idx)==Symbolic_Constant_Size),
                                is_interface);

         extent_fld		= OPND_FLD(opnd);
         extent_idx		= OPND_IDX(opnd);
      }

      BD_XT_FLD(bd_idx, dim)    = extent_fld;
      BD_XT_IDX(bd_idx, dim)    = extent_idx;

      /* STRIDE = STRIDE * (EXTENT of previous dimension) */
      /* Fix stride for next dimension.                   */
      /* Calculate length.                                */

      if (dim < BD_RANK(bd_idx)) {
         NTR_IR_TBL(ir_idx);            /* Create Stride * Extent */
         IR_OPR(ir_idx)                 = Mult_Opr;
         IR_TYPE_IDX(ir_idx)            = INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)            = BD_LINE_NUM(bd_idx);
         IR_COL_NUM(ir_idx)             = BD_COLUMN_NUM(bd_idx);
         IR_FLD_L(ir_idx)               = stride.fld;
         IR_IDX_L(ir_idx)               = stride.idx;
         IR_LINE_NUM_L(ir_idx)          = BD_LINE_NUM(bd_idx);
         IR_COL_NUM_L(ir_idx)           = BD_COLUMN_NUM(bd_idx);
         IR_FLD_R(ir_idx)               = extent_fld;
         IR_IDX_R(ir_idx)               = extent_idx;
         IR_LINE_NUM_R(ir_idx)          = BD_LINE_NUM(bd_idx);
         IR_COL_NUM_R(ir_idx)           = BD_COLUMN_NUM(bd_idx);

         if ((extent_fld == AT_Tbl_Idx && ATD_SYMBOLIC_CONSTANT(extent_idx)) ||
             (stride.fld == AT_Tbl_Idx && ATD_SYMBOLIC_CONSTANT(stride.idx))) {
            IR_OPR(ir_idx)      = Symbolic_Mult_Opr;
            stride.fld          = AT_Tbl_Idx;
            stride.idx          = gen_compiler_tmp(BD_LINE_NUM(bd_idx),
                                                   BD_COLUMN_NUM(bd_idx),
                                                   Priv, TRUE);

            ATD_TYPE_IDX(stride.idx)            = CG_INTEGER_DEFAULT_TYPE;
            ATD_FLD(stride.idx)                 = IR_Tbl_Idx;
            ATD_TMP_IDX(stride.idx)             = ir_idx;
            ATD_SYMBOLIC_CONSTANT(stride.idx)   = TRUE;
         }
         else {
            OPND_FLD(opnd)              = IR_Tbl_Idx;
            OPND_IDX(opnd)              = ir_idx;
            OPND_LINE_NUM(opnd)         = stmt_start_line;
            OPND_COL_NUM(opnd)          = stmt_start_col;

            sh_idx                      = ntr_sh_tbl();
            SH_STMT_TYPE(sh_idx)        = Automatic_Base_Size_Stmt;
            SH_COMPILER_GEN(sh_idx)     = TRUE;
            SH_P2_SKIP_ME(sh_idx)       = TRUE;
            SH_GLB_LINE(sh_idx)         = stmt_start_line;
            SH_COL_NUM(sh_idx)          = stmt_start_col;

            expr_desc.rank              = 0;
            xref_state                  = CIF_No_Usage_Rec;

            expr_semantics(&opnd, &expr_desc);

            if (OPND_FLD(opnd) == CN_Tbl_Idx) {
               stride.fld               = CN_Tbl_Idx;
               stride.idx               = OPND_IDX(opnd);
               FREE_SH_NODE(sh_idx);
            }
            else {

               if (!is_interface) {

                  /* Stride must be non-constant, if extent is non-constant */

                  if (extent_entry_idx != NULL_IDX) {
                     stride_entry_idx = merge_entry_lists(stride_entry_idx,
                                                          extent_entry_idx);
                     length_entry_idx = merge_entry_lists(length_entry_idx,
                                                          extent_entry_idx);
                  }
               }

               stride.fld = AT_Tbl_Idx;
               stride.idx = ntr_bnds_sh_tmp_list(&opnd,
                                                 stride_entry_idx,
                                                 is_interface ? NULL_IDX:sh_idx,
                                                 FALSE,
                                                 SA_INTEGER_DEFAULT_TYPE);
            }
         }

         NTR_IR_TBL(mult_idx);   /* Create length = extent * extent */
         IR_LINE_NUM(mult_idx)          = BD_LINE_NUM(bd_idx);
         IR_COL_NUM(mult_idx)           = BD_COLUMN_NUM(bd_idx);
         IR_OPR(mult_idx)               = (extent_fld == AT_Tbl_Idx &&
                                          ATD_SYMBOLIC_CONSTANT(extent_idx)) ?
                                          Symbolic_Mult_Opr : Mult_Opr;
         IR_TYPE_IDX(mult_idx)          = INTEGER_DEFAULT_TYPE;
         IR_IDX_R(len_ir_idx)           = mult_idx;
         IR_FLD_R(len_ir_idx)           = IR_Tbl_Idx;
         IR_LINE_NUM_R(len_ir_idx)      = BD_LINE_NUM(bd_idx);
         IR_COL_NUM_R(len_ir_idx)       = BD_COLUMN_NUM(bd_idx);
         IR_IDX_L(mult_idx)             = extent_idx;
         IR_FLD_L(mult_idx)             = extent_fld;
         IR_LINE_NUM_L(mult_idx)        = BD_LINE_NUM(bd_idx);
         IR_COL_NUM_L(mult_idx)         = BD_COLUMN_NUM(bd_idx);
         len_ir_idx                     = mult_idx;
      }
      else if (dim == 1) {

         /* Last dimension is the only dimension, so length = xtent */

         BD_LEN_FLD(bd_idx)     = extent_fld;
         BD_LEN_IDX(bd_idx)     = extent_idx;
         length_entry_idx       = extent_entry_idx;
         stride_entry_idx       = merge_entry_lists(stride_entry_idx,
                                                    extent_entry_idx);

         extent_entry_idx	= NULL_IDX;  /* Now length holds list */

         if (length_entry_idx != NULL_IDX) {  /* Alt entries - need tmp = 0 */
            gen_tmp_eq_zero_ir(extent_idx);
         }
      }

      /* Last dimension */

      else if (BD_ARRAY_SIZE(bd_idx) == Symbolic_Constant_Size) {
         IR_IDX_R(len_ir_idx)           = extent_idx;
         IR_FLD_R(len_ir_idx)           = extent_fld;
         IR_LINE_NUM_R(len_ir_idx)      = BD_LINE_NUM(bd_idx);
         IR_COL_NUM_R(len_ir_idx)       = BD_COLUMN_NUM(bd_idx);

         BD_LEN_FLD(bd_idx)     = AT_Tbl_Idx;
         BD_LEN_IDX(bd_idx)     = gen_compiler_tmp(BD_LINE_NUM(bd_idx),
                                                   BD_COLUMN_NUM(bd_idx),
                                                   Priv, TRUE);
         ATD_TYPE_IDX(BD_LEN_IDX(bd_idx))       = CG_INTEGER_DEFAULT_TYPE;
         ATD_FLD(BD_LEN_IDX(bd_idx))            = IR_FLD_R(BD_LEN_IDX(bd_idx));
         ATD_TMP_IDX(BD_LEN_IDX(bd_idx))        = IR_IDX_R(BD_LEN_IDX(bd_idx));

         ATD_SYMBOLIC_CONSTANT(BD_LEN_IDX(bd_idx))      = TRUE;
      }
      else {
         IR_IDX_R(len_ir_idx)           = extent_idx;
         IR_FLD_R(len_ir_idx)           = extent_fld;
         IR_LINE_NUM_R(len_ir_idx)      = BD_LINE_NUM(bd_idx);
         IR_COL_NUM_R(len_ir_idx)       = BD_COLUMN_NUM(bd_idx);
         OPND_FLD(opnd)                 = IR_FLD_R(BD_LEN_IDX(bd_idx));
         OPND_IDX(opnd)                 = IR_IDX_R(BD_LEN_IDX(bd_idx));
         OPND_LINE_NUM(opnd)            = BD_LINE_NUM(bd_idx);
         OPND_COL_NUM(opnd)             = BD_COLUMN_NUM(bd_idx);

         sh_idx                         = ntr_sh_tbl();
         SH_STMT_TYPE(sh_idx)           = Automatic_Base_Size_Stmt;
         SH_COMPILER_GEN(sh_idx)        = TRUE;
         SH_P2_SKIP_ME(sh_idx)          = TRUE;
         SH_GLB_LINE(sh_idx)            = stmt_start_line;
         SH_COL_NUM(sh_idx)             = stmt_start_col;

         /* expr_semantics needs curr_stmt_sh_idx set to something valid.  */
         /* It does not need SH_IR_IDX(curr_stmt_sh_idx) set to something. */

         expr_desc.rank = 0;
         xref_state     = CIF_No_Usage_Rec;

         if (!expr_semantics(&opnd, &expr_desc)) {

# if defined(_CHECK_MAX_MEMORY)

            if (!target_t3e) {
               AT_DCL_ERR(attr_idx)	= TRUE;
            }
# endif
         }

         if (OPND_FLD(opnd) == CN_Tbl_Idx) {
            BD_LEN_FLD(bd_idx)  = CN_Tbl_Idx;
            BD_LEN_IDX(bd_idx)  = OPND_IDX(opnd);
            FREE_SH_NODE(sh_idx);
         }
         else {

            if (!is_interface) {

               if (extent_entry_idx != NULL_IDX) {
                  stride_entry_idx = merge_entry_lists(stride_entry_idx,
                                                       extent_entry_idx);
                  length_entry_idx = merge_entry_lists(length_entry_idx,
                                                       extent_entry_idx);
               }
            }

            length_idx = ntr_bnds_sh_tmp_list(&opnd,
                                              length_entry_idx,
                                              (is_interface) ? NULL_IDX:sh_idx,
                                              TRUE,
                                              SA_INTEGER_DEFAULT_TYPE);
            BD_LEN_IDX(bd_idx) = length_idx;
            BD_LEN_FLD(bd_idx) = AT_Tbl_Idx;
         }
      }
   }

   /* After the dimensions are processed, stride_entry_idx contains a list   */
   /* of all bad entry points, for the array - including all extents and     */
   /* type information.  Stride is calculated from the (previous dimension's */
   /* extent) * (previous dimension's stride).  A stride_entry_idx is made   */
   /* for the last dimension, even though actual stride isn't calculated for */
   /* this dimension.                                                        */

   if (stride_entry_idx != NULL_IDX) {
      entry_count       = SCP_ALT_ENTRY_CNT(curr_scp_idx) + 1;

      if (length_entry_idx != NULL_IDX &&
          entry_count == AL_ENTRY_COUNT(length_entry_idx))  {

         /* Error if problem with lower and/or upper bounds coming in    */
         /* different entry points.  Bounds for this array declaration   */
         /* cannot be calculated at any entry point, because dummy args  */
         /* used in the expression do not enter at all the same points.  */

         PRINTMSG(AT_DEF_LINE(attr_idx), 660, Error,
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx));
         AT_DCL_ERR(attr_idx)   = TRUE;
      }
      else if (entry_count == AL_ENTRY_COUNT(stride_entry_idx))  {

         /* If the length is okay, but there's a problem with the        */
         /* stride, that means that it's a character and a bound         */
         /* forming the char length, doesn't enter the same as all       */
         /* the dimension bounds.  Bounds for this array declaration     */
         /* cannot be calculated at any entry point, because dummy args  */
         /* used in the expression de not enter at all the same points.  */

         PRINTMSG(AT_DEF_LINE(attr_idx), 661, Error,
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx));
         AT_DCL_ERR(attr_idx)   = TRUE;
      }
      else if (entry_list != NULL_IDX) {
         stride_entry_count = merge_entry_list_count(stride_entry_idx,
                                                     entry_list);

         if (entry_count == stride_entry_count) {

            /* This array and its bound variables do not enter at the  */
            /* same entry point.                                       */

            PRINTMSG(AT_DEF_LINE(attr_idx), 662, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx)        = TRUE;
         }
      }
   }

NEXT:

   /* Every array must have the following semantic checks.  So even if the   */
   /* bounds for the array are already resolved, it still must get these     */
   /* checks.                                                                */


EXIT:

   if (stride_entry_idx != NULL_IDX) {
      free_attr_list(stride_entry_idx);
   }

   if (length_entry_idx != NULL_IDX) {
      free_attr_list(length_entry_idx);
   }

   if (ATD_CLASS(attr_idx) == Function_Result) {
      ATP_NO_ENTRY_LIST(ATD_FUNC_IDX(attr_idx)) = entry_list;
   }
   else {
      ATD_NO_ENTRY_LIST(attr_idx)               = entry_list;
   }

   TRACE (Func_Exit, "pe_array_dim_resolution", NULL);

   return;

}  /* pe_array_dim_resolution */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	It does semantic checking and tries to fold the bound.  If the bound  *|
|*	folds to a constant, ATD_FLD(tmp) is set to CN_Tbl_Idx and            *|
|*	ATD_TMP_IDX(tmp) is set to the constant table index of the constant.  *|
|*	AT_REFERENCED(tmp) = Not_Referenced, so the temp doesn't get added to *|
|*      the IR stream at entry point processing.  array_dim_resolution and    *|
|*      char_len_resolution then check ATD_FLD(tmp).  If it is CN_Tbl_Idx     *|
|*      the item resolves to a constant bounded item.  If it doesn't resolve  *|
|*	to a folded item, ATD_FLD(tmp) = SH_Tbl_Idx and ATD_TMP_IDX(tmp)      *|
|*	is the index to the first statement header for the bound.  A bound    *|
|*	may have more than one statement, after going through expr_semantics. *|
|*	The statements are all linked together.                               *|
|*									      *|
|*	Assumption:  All non-interface blocks have a valid curr_stmt_sh_idx   *|
|*	It is set to the Entry SH when decl_semantics is called.  All bounds  *|
|*	IR SH's go in following this and curr_stmt_sh_idx is advanced.        *|
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
static	void	bound_resolution(int	attr_idx)

{
   boolean		ansi;
   msg_lvl_type		save_msg_level;
   int			start_sh_idx;


   TRACE (Func_Entry, "bound_resolution", NULL);

   if (ATD_CLASS(attr_idx) == Constant) {

      /* Intentionally blank */
   }
   else if (AT_REFERENCED(attr_idx) == Not_Referenced) {

      /* These are tmps that are only here, because CIF generation is on. */
      /* These are shared tmps and normally would not have been kept      */
      /* around.  Call expr_semantics with them, so the proper CIF calls  */
      /* can be generated and then free the IR.                           */

      xref_state			= CIF_Symbol_Reference;
      cif_tmp_so_no_msg			= TRUE;
      no_func_expansion			= TRUE;
      save_msg_level			= cmd_line_flags.msg_lvl_suppressed;
      ansi				= on_off_flags.issue_ansi_messages;
      cmd_line_flags.msg_lvl_suppressed	= Error_Lvl;

      bound_semantics(attr_idx, FALSE);          /* Don't get stmts */

      if (ATD_CLASS(attr_idx) != Constant) {
         ATD_TMP_IDX(attr_idx)		= NULL_IDX;
         ATD_FLD(attr_idx)		= NO_Tbl_Idx;
      }

      AT_REFERENCED(attr_idx)		= Not_Referenced;
      AT_DEFINED(attr_idx)		= FALSE;
      no_func_expansion			= FALSE;
      cmd_line_flags.msg_lvl_suppressed	= save_msg_level;
      on_off_flags.issue_ansi_messages	= ansi;
      cif_tmp_so_no_msg			= FALSE;
   }
   else {

      if (ATD_TMP_SEMANTICS_DONE(attr_idx)) {

         /* These are tmps that were folded during pass1, because they were */
         /* referenced in a bound for a parameterized character or array.   */
         /* These did not fold to a constant, so they must be sent thru     */
         /* expression semantics, so that everything gets folded and/or     */
         /* expanded correctly.   Stop message issuing, because it has been */
         /* done once already.                                              */

         save_msg_level			   = cmd_line_flags.msg_lvl_suppressed;
         ansi				   = on_off_flags.issue_ansi_messages;
         cmd_line_flags.msg_lvl_suppressed = Error_Lvl;

         /* If this isn't an interface block - generate stmts after */
         /* curr_stmt_sh_idx for this bound.                        */

         start_sh_idx = bound_semantics(attr_idx,
                                        !SCP_IS_INTERFACE(curr_scp_idx));

         cmd_line_flags.msg_lvl_suppressed = save_msg_level;
         on_off_flags.issue_ansi_messages  = ansi;
      }
      else {

         /* If this isn't an interface block - generate stmts after */
         /* curr_stmt_sh_idx for this bound.                        */

         xref_state   = CIF_Symbol_Reference;
         start_sh_idx = bound_semantics(attr_idx,
                                        !SCP_IS_INTERFACE(curr_scp_idx));
      }

      if (ATD_CLASS(attr_idx) != Constant &&
          !ATD_SYMBOLIC_CONSTANT(attr_idx) &&
          SCP_ENTRY_IDX(curr_scp_idx) != NULL_IDX) {

         /* Enter the code at each alternate entry.  We do generate tmp = 0   */
         /* code, because the bounds can be referenced for whole subscript    */
         /* and whole substring references.  These are all bounds tmps for    */
         /* arrays (upper or lower) or character length. We do not have to    */
         /* worry about OPTIONAL dummy arguments here because it is illegal   */
         /* to use an OPTIONAL dummy argument in a bound expression.  Start   */
         /* the copy at SH_PREV_IDX(start_sh_idx) and end at curr_stmt_sh_idx.*/

         insert_sh_after_entries(attr_idx, 
                                 SH_PREV_IDX(start_sh_idx),
                                 curr_stmt_sh_idx,
                                 TRUE,
                                 TRUE);     /* Advance ATP_FIRST_SH_IDX */
      }
   }

   TRACE (Func_Exit, "bound_resolution", NULL);

   return;
   
}  /* bound_resolution */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine calls expr_semantics for a declaration bound and         *|
|*	handles semantic checking.  If the bounds folds to a constant,        *|
|*	ATD_FLD(tmp) is set to CN_Tbl_Idx and ATD_TMP_IDX(tmp) is set to the  *|
|*	constant table index of the constant.  AT_REFERENCED(tmp) =           *|
|*	Not_Referenced, so the temp does not get used in other phases of      *|
|*	compilation.						              *|
|*	Also, if non-constant a cvrt opr will be added if necessary to set    *|
|*	the type to the correct size/addresss/offset type.                    *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx	    - Index of bound tmp to call expr_semantics for.  *|
|*	insert_in_SH_stream - TRUE if IR should be inserted in IR stream.     *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	bound_sh_idx	    - Index of statement header for bound.            *|
|*									      *|
\******************************************************************************/
int	bound_semantics(int		attr_idx,
                        boolean		insert_in_SH_stream)

{
   int			bound_sh_idx;
   expr_arg_type	expr_desc;
   int			list_idx;
   fld_type		new_fld;
   int			new_ir_idx;
   opnd_type		opnd;
   int			save_sh_idx;
   int			type_idx;


   TRACE (Func_Entry, "bound_semantics", NULL);

   if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
       ATD_CLASS(attr_idx) != Compiler_Tmp) {
      return(NULL_IDX);
   }

   expr_mode				= Specification_Expr;
   expr_desc.rank			= 0;
   ATD_TMP_SEMANTICS_DONE(attr_idx)	= TRUE;

   /* Save a copy of the IR.  If this does not fold to a constant, we need    */
   /* to keep the IR before it goes through expr_semantics.  This ir is used, */
   /* if this bound is part of a description of an interface for a function.  */
   /* (Interface block, internal function or module procedure function.)      */

   gen_opnd(&opnd, ATD_TMP_IDX(attr_idx), (fld_type) ATD_FLD(attr_idx),
            stmt_start_line, stmt_start_col);
   copy_subtree(&opnd, &opnd);
   new_ir_idx = OPND_IDX(opnd);
   new_fld = OPND_FLD(opnd);

   /* Create a stmt header to link the IR to.  This way if expr_semantics  */
   /* generates some statements, they get attached where they need to be.  */

   bound_sh_idx				= ntr_sh_tbl();
   SH_IR_IDX(bound_sh_idx)		= ATD_TMP_IDX(attr_idx);
   SH_STMT_TYPE(bound_sh_idx)		= Automatic_Base_Size_Stmt;
   SH_COMPILER_GEN(bound_sh_idx)	= TRUE;
   SH_P2_SKIP_ME(bound_sh_idx)		= TRUE;
   SH_GLB_LINE(bound_sh_idx)		= stmt_start_line;
   SH_COL_NUM(bound_sh_idx)		= stmt_start_col;
   save_sh_idx				= curr_stmt_sh_idx;
   curr_stmt_sh_idx			= bound_sh_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

   if (ATD_TMP_HAS_CVRT_OPR(attr_idx)) {

      /* Need to do expr_semantics without the cvrt to do error checking */

      COPY_OPND(opnd, IR_OPND_L(IR_IDX_R(ATD_TMP_IDX(attr_idx))));
   }
   else {
      COPY_OPND(opnd, IR_OPND_R(ATD_TMP_IDX(attr_idx)));
   }
# else

   COPY_OPND(opnd, IR_OPND_R(ATD_TMP_IDX(attr_idx)));
# endif

   if (!expr_semantics(&opnd, &expr_desc)) {

      /* There were problems with this expression.  Replace it with a   */
      /* constant one.  Constant bound processing will free the IR.     */

      OPND_IDX(opnd)		= CN_INTEGER_ONE_IDX;
      OPND_FLD(opnd)		= CN_Tbl_Idx;
      OPND_LINE_NUM(opnd)	= stmt_start_line;
      OPND_COL_NUM(opnd)	= stmt_start_col;

      /* This is a newly created list after each call to expr_semantics.*/
      /* It contains dargs found in this specification expression.      */

      free_attr_list(SCP_TMP_LIST(curr_scp_idx));
      SCP_TMP_LIST(curr_scp_idx) = NULL_IDX;  /* Clear in case of list */
   }
   else if (expr_desc.rank != 0) {
      PRINTMSG(AT_DEF_LINE(attr_idx), 907, Error,
              AT_DEF_COLUMN(attr_idx));
      AT_DCL_ERR(attr_idx) = TRUE;
   }
   else if (expr_desc.type != Integer) {

      /* The tmp must be integer.  This must be its first pass thru and no */
      /* no previous error messages must have been issued about this tmp.  */

      if (expr_desc.linear_type == Typeless_4 ||
          expr_desc.linear_type == Typeless_8) { 
         PRINTMSG(AT_DEF_LINE(attr_idx), 221, Ansi, 
                  AT_DEF_COLUMN(attr_idx));
      }
      else if (expr_desc.linear_type == Short_Typeless_Const) {
         PRINTMSG(AT_DEF_LINE(attr_idx), 221, Ansi,
                  AT_DEF_COLUMN(attr_idx));
         OPND_IDX(opnd) = cast_typeless_constant(OPND_IDX(opnd),
                                                 INTEGER_DEFAULT_TYPE,
						 OPND_LINE_NUM(opnd),
						 OPND_COL_NUM(opnd));
         expr_desc.type_idx    = INTEGER_DEFAULT_TYPE;
         expr_desc.type        = Integer;
         expr_desc.linear_type = INTEGER_DEFAULT_TYPE;

      }
      else {

         if (!AT_DCL_ERR(attr_idx)) {

            if (expr_desc.linear_type == Long_Typeless) { 

               /* hollerith too long */

               PRINTMSG(AT_DEF_LINE(attr_idx), 1133, Error, 
                        AT_DEF_COLUMN(attr_idx));
            }
            else {  /* bad type */
               PRINTMSG(AT_DEF_LINE(attr_idx), 488, Error,
                        AT_DEF_COLUMN(attr_idx),
                        get_basic_type_str(expr_desc.type_idx));
            }
            AT_DCL_ERR(attr_idx) = TRUE;
         }

         /* There were problems with this expression.  Replace with a one. */

         OPND_IDX(opnd)		= CN_INTEGER_ONE_IDX;
         OPND_FLD(opnd)		= CN_Tbl_Idx;
         OPND_LINE_NUM(opnd)	= AT_DEF_LINE(attr_idx);
         OPND_COL_NUM(opnd)	= AT_DEF_COLUMN(attr_idx);

         /* This is a newly created list after each call to expr_semantics.*/
         /* It contains dargs found in this specification expression.      */

         free_attr_list(SCP_TMP_LIST(curr_scp_idx));
         SCP_TMP_LIST(curr_scp_idx) = NULL_IDX;  /* Clear in case of list */
      }
   }
   else if (expr_desc.has_symbolic) {

      /* This expression contains a reference to a symbolic constant.     */

      /* Determine if this is a symbolic constant expression or not.      */
      /* If this is a symbolic constant expression, ATD_SYMBOLIC_CONSTANT */
      /* will be set on the compiler temp.                                */

      ATD_SYMBOLIC_CONSTANT(attr_idx) = expr_is_symbolic_constant(&opnd);
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

   else if (ATD_TMP_HAS_CVRT_OPR(attr_idx)) {
      COPY_OPND(IR_OPND_L(IR_IDX_R(ATD_TMP_IDX(attr_idx))), opnd);

      if (OPND_FLD(opnd) == CN_Tbl_Idx) {
         COPY_OPND(opnd, IR_OPND_R(ATD_TMP_IDX(attr_idx)));
         expr_semantics(&opnd, &expr_desc);
      }
      else {
         COPY_OPND(opnd, IR_OPND_R(ATD_TMP_IDX(attr_idx)));
      }
   }
# endif

   if (OPND_FLD(opnd) == CN_Tbl_Idx) {

      /* Folded to a constant.   NOTE:  Cannot free IR, because IR can  */
      /* be shared and you could free IR that is used in other places.  */

      /* Change the tmp to a constant, so it gets folded whenever it is  */
      /* referenced.  AT_DEFINED is left clear.  It is set on declared   */
      /* parameters, so that parameter constants can be differentiated   */
      /* from compiler tmp constants.  CIF wants all parameters, whether */
      /* they are referenced or not, so AT_DEFINED is used to tell the   */
      /* difference between them.                                        */

      CLEAR_VARIANT_ATTR_INFO(attr_idx, Data_Obj);

      ATD_CLASS(attr_idx)	= Constant;
      AT_TYPED(attr_idx)	= TRUE;
      ATD_TYPE_IDX(attr_idx)	= CN_TYPE_IDX(OPND_IDX(opnd));
      AT_REFERENCED(attr_idx)	= Not_Referenced;  /* Temp not used      */
      AT_DEFINED(attr_idx)	= FALSE;           /* Temp not defined   */
      ATD_CONST_IDX(attr_idx)	= OPND_IDX(opnd);
      ATD_FLD(attr_idx)		= CN_Tbl_Idx;
      curr_stmt_sh_idx		= save_sh_idx;
      FREE_SH_NODE(bound_sh_idx);
      bound_sh_idx		= NULL_IDX;
   }
   else if (ATD_SYMBOLIC_CONSTANT(attr_idx)) {

      /* This is a symbolic constant expression.  A temp holds it. */

      curr_stmt_sh_idx		= save_sh_idx;
      FREE_SH_NODE(bound_sh_idx);
      bound_sh_idx		= NULL_IDX;
      ATD_FLD(attr_idx)		= OPND_FLD(opnd);
      ATD_TMP_IDX(attr_idx)	= OPND_IDX(opnd);
   }
   else { 

      if (OPND_FLD(opnd) == AT_Tbl_Idx) {

         if (AT_IS_DARG(OPND_IDX(opnd))) {

            /* CIF wants to know if a bound is made up of just one dummy */
            /* argument.  NO expression.  AT_CIF_USE_IN_BND is set when  */
            /* this is found for a dummy argument.                       */

            AT_CIF_USE_IN_BND(OPND_IDX(opnd))	= TRUE;
         }

         /* Let PDGCS know if a temp is set to one var.  Give them       */
         /* the link between them.  Use ATD_DEFINING_ATTR_IDX.           */

         ATD_DEFINING_ATTR_IDX(attr_idx)	= OPND_IDX(opnd);
      }

      /* Make sure this is set to the correct addressing/offset type. */

      type_idx	= check_type_for_size_address(&opnd);

      COPY_OPND(IR_OPND_R(ATD_TMP_IDX(attr_idx)), opnd);

      /* Reset type if necessary on the Asg_Opr and bound tmp. */

      ATD_TYPE_IDX(attr_idx)			= type_idx;
      IR_TYPE_IDX(ATD_TMP_IDX(attr_idx))	= type_idx;

      /* SCP_TMP_LIST contains a list of dummy args referenced in this */
      /* expression.  If there are NO alternate entries, SCP_TMP_LIST  */
      /* will always be NULL.                                          */

      if (SCP_TMP_LIST(curr_scp_idx) != NULL_IDX) {

         /* Convert the bounds list of dargs that are used in this      */
         /* expression, but do not come in at every entry point, into   */
         /* a list of entry points where this expression cannot be.     */

         list_idx = SCP_TMP_LIST(curr_scp_idx);

         while (list_idx != NULL_IDX) {
            ATD_NO_ENTRY_LIST(attr_idx) = 
                merge_entry_lists(ATD_NO_ENTRY_LIST(attr_idx),
                            (AT_OBJ_CLASS(AL_ATTR_IDX(list_idx)) == Data_Obj) ?
                                    ATD_NO_ENTRY_LIST(AL_ATTR_IDX(list_idx)) :
                                    ATP_NO_ENTRY_LIST(AL_ATTR_IDX(list_idx)));
            list_idx = AL_NEXT_IDX(list_idx);
         }

         free_attr_list(SCP_TMP_LIST(curr_scp_idx));
         SCP_TMP_LIST(curr_scp_idx) = NULL_IDX;
      }

      if (!insert_in_SH_stream) {

         /* Statement headers are not wanted.  Leave this as IR.  These tmps  */
         /* become place holders.  If this is a parameter bound, this is an   */
         /* error situation.  If this is an interface block, all these tmps   */
         /* are just place holders.  NOTE:  Cannot free IR, because IR can    */
         /* be shared and you could free IR that is used in other places.     */

         AT_REFERENCED(attr_idx)	= Not_Referenced;
         AT_DEFINED(attr_idx)		= FALSE;

         while (curr_stmt_sh_idx != NULL_IDX) {
            bound_sh_idx	= curr_stmt_sh_idx;
            curr_stmt_sh_idx	= SH_PREV_IDX(curr_stmt_sh_idx);
            FREE_SH_NODE(bound_sh_idx);
         }
         bound_sh_idx			= NULL_IDX;
         curr_stmt_sh_idx		= save_sh_idx;
      }
      else {

         /* can't assume that the SH_NEXT_IDX(save_sh_idx) is null */

         if (SH_NEXT_IDX(save_sh_idx) != NULL_IDX) {
            while (SH_NEXT_IDX(bound_sh_idx) != NULL_IDX) {
               bound_sh_idx = SH_NEXT_IDX(bound_sh_idx);
            }
            SH_NEXT_IDX(bound_sh_idx)      = SH_NEXT_IDX(save_sh_idx);
            if (SH_NEXT_IDX(bound_sh_idx)) {
               SH_PREV_IDX(SH_NEXT_IDX(bound_sh_idx)) = bound_sh_idx;
            }
         }

         while (SH_PREV_IDX(bound_sh_idx) != NULL_IDX) {
            bound_sh_idx = SH_PREV_IDX(bound_sh_idx);
         }

         SH_PREV_IDX(bound_sh_idx)	= save_sh_idx;
         SH_NEXT_IDX(save_sh_idx)	= bound_sh_idx;
         AT_DEFINED(attr_idx)		= TRUE;
         AT_REFERENCED(attr_idx)	= Referenced;
      }

      /* Save the unexpanded IR, so it can be expanded later if this  */
      /* is part of a function that may be called.                    */

      /* Adjust type if necessary in the save unexpanded IR. */

      OPND_FLD(opnd)			= new_fld;
      OPND_IDX(opnd)			= new_ir_idx;
      OPND_LINE_NUM(opnd)		= AT_DEF_LINE(attr_idx);
      OPND_COL_NUM(opnd)		= AT_DEF_COLUMN(attr_idx);

      type_idx	= check_type_for_size_address(&opnd);

      ATD_FLD(attr_idx)			= OPND_FLD(opnd);
      ATD_TMP_IDX(attr_idx)		= OPND_IDX(opnd);
   }

   expr_mode = Regular_Expr;

   TRACE (Func_Exit, "bound_semantics", NULL);

   return(bound_sh_idx);

}  /* bound_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine resolves the character length to a temp.                 *|
|*      NOTE:  This does not handle component character lengths.  They are    *|
|*      done in parse_cpnt_dcl_stmt.                                          *|
|*									      *|
|* Input parameters:							      *|
|*      attr_idx -> Index to attribute for array.                             *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
void	char_len_resolution(int		attr_idx,
			    boolean	must_be_const_array)

{
   int		column;
   int		entry_count;
   int		ir_idx;
   boolean	is_interface;
   int		len_entry_count;
   int		len_idx;
   int		line;
   int		list_idx;
   int		max_idx;
   int		new_len_idx;
   opnd_type	opnd;
#ifdef KEY /* Bug 10177 */
   int		sh_idx = 0;
#else /* KEY Bug 10177 */
   int		sh_idx;
#endif /* KEY Bug 10177 */
   int		tmp_attr_idx;
   int		t_idx;
   int		type_idx;
   int		zero_idx;


   TRACE (Func_Entry, "char_len_resolution", NULL);

   is_interface	= SCP_IS_INTERFACE(curr_scp_idx);
   type_idx	= ATD_TYPE_IDX(attr_idx);

   if (TYP_CHAR_CLASS(type_idx) == Unknown_Char) {

      if (AT_OBJ_CLASS(TYP_IDX(type_idx)) == Data_Obj && 
          ATD_CLASS(TYP_IDX(type_idx)) == Constant) {
         TYP_IDX(type_idx)		= ATD_CONST_IDX(TYP_IDX(type_idx));
         TYP_FLD(type_idx)		= CN_Tbl_Idx;
         TYP_CHAR_CLASS(type_idx)	= Const_Len_Char;
      }
      else if (AT_OBJ_CLASS(TYP_IDX(type_idx)) == Data_Obj && 
               ATD_SYMBOLIC_CONSTANT(TYP_IDX(type_idx))) {

         PRINTMSG(AT_DEF_LINE(attr_idx), 1211, Error, 
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx));
         AT_DCL_ERR(attr_idx)	= TRUE;
         ATD_TYPE_IDX(attr_idx) = CHARACTER_DEFAULT_TYPE;
      }
      else {
         TYP_CHAR_CLASS(type_idx)	= Var_Len_Char;
         TYP_ORIG_LEN_IDX(type_idx)	= TYP_IDX(type_idx);
      }
   }

   if (TYP_CHAR_CLASS(type_idx) == Var_Len_Char) {

      /* This is called from PARAMETER processing.  This must be a const  */
      /* length array.  If it is not, do not process now.  It will happen */
      /* at decl_sematics time.  PARAMETER processing will issue error.   */

      if (must_be_const_array) {
         goto EXIT;
      }

      if (fnd_semantic_err(Obj_Var_Len_Ch,
                           AT_DEF_LINE(attr_idx),
                           AT_DEF_COLUMN(attr_idx),
                           attr_idx,
                           TRUE)) {
         ATD_TYPE_IDX(attr_idx) = CHARACTER_DEFAULT_TYPE;
      }
      else if (ATD_CLASS(attr_idx) == Function_Result && 
               !ATP_EXPL_ITRFC(ATD_FUNC_IDX(attr_idx))) {
         PRINTMSG(AT_DEF_LINE(attr_idx), 916, Error, 
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(ATD_FUNC_IDX(attr_idx)));
         AT_DCL_ERR(attr_idx)	= TRUE;
         ATD_TYPE_IDX(attr_idx) = CHARACTER_DEFAULT_TYPE;
      }
      else {

         if (!TYP_RESOLVED(type_idx)) {

            /* generate max(0,length) - then switch length to new tmp */

            NTR_IR_TBL(max_idx);
            IR_OPR(max_idx)		= Max_Opr;
            IR_LINE_NUM(max_idx)	= AT_DEF_LINE(attr_idx);
            IR_COL_NUM(max_idx)		= AT_DEF_COLUMN(attr_idx);
            IR_LIST_CNT_L(max_idx)	= 2;

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_L(max_idx)		= IL_Tbl_Idx;
            IR_IDX_L(max_idx)		= list_idx;

            OPND_FLD(opnd)		= TYP_FLD(type_idx);
            OPND_IDX(opnd)		= TYP_IDX(type_idx);
            OPND_LINE_NUM(opnd)		= AT_DEF_LINE(attr_idx);
            OPND_COL_NUM(opnd)		= AT_DEF_COLUMN(attr_idx);
            t_idx			= check_type_for_size_address(&opnd);

            COPY_OPND(IL_OPND(list_idx), opnd);

            IR_TYPE_IDX(max_idx)	= t_idx;

            NTR_IR_LIST_TBL(zero_idx);
            IL_NEXT_LIST_IDX(list_idx)	= zero_idx;
            IL_PREV_LIST_IDX(zero_idx)	= list_idx;
            IL_FLD(zero_idx)       	= CN_Tbl_Idx;
            IL_IDX(zero_idx)       	= CN_INTEGER_ZERO_IDX;
            IL_LINE_NUM(zero_idx)	= AT_DEF_LINE(attr_idx);
            IL_COL_NUM(zero_idx)	= AT_DEF_COLUMN(attr_idx);

            if (!is_interface) {
               sh_idx			= ntr_sh_tbl();
               SH_STMT_TYPE(sh_idx)	= Automatic_Base_Size_Stmt;
               SH_P2_SKIP_ME(sh_idx)	= TRUE;
               SH_COMPILER_GEN(sh_idx)	= TRUE;
               SH_GLB_LINE(sh_idx)	= stmt_start_line;
               SH_COL_NUM(sh_idx)	= stmt_start_col;
            }

            OPND_FLD(opnd)	= IR_Tbl_Idx;
            OPND_IDX(opnd)	= max_idx;
            OPND_LINE_NUM(opnd)	= stmt_start_line;
            OPND_COL_NUM(opnd)	= stmt_start_col;
            new_len_idx		= ntr_bnds_sh_tmp_list(
                                           &opnd,
                                           ATD_NO_ENTRY_LIST(TYP_IDX(type_idx)),
                                           (is_interface) ? NULL_IDX : sh_idx,
                                           TRUE,
                                           t_idx);

            TYP_FLD(type_idx)	= AT_Tbl_Idx;
            TYP_IDX(type_idx)	= new_len_idx;

            if (ATD_NO_ENTRY_LIST(new_len_idx) != NULL_IDX) {
               entry_count = SCP_ALT_ENTRY_CNT(curr_scp_idx) + 1;

               if (entry_count==AL_ENTRY_COUNT(ATD_NO_ENTRY_LIST(new_len_idx))){

                  /* The length for this character cannot be calculated at    */
                  /* any entry point, because dargs used in the expression do */
                  /* not enter at all the same points.                        */

                  PRINTMSG(AT_DEF_LINE(attr_idx), 659, Error,
                           AT_DEF_COLUMN(attr_idx), 
                           AT_OBJ_NAME_PTR(attr_idx));
                  AT_DCL_ERR(attr_idx)	= TRUE;
               }
               else if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) {
                  len_entry_count =
                      merge_entry_list_count(ATD_NO_ENTRY_LIST(new_len_idx),
                            ((ATD_CLASS(attr_idx) == Function_Result) ?
                                 ATP_NO_ENTRY_LIST(ATD_FUNC_IDX(attr_idx)) :
                                 ATD_NO_ENTRY_LIST(attr_idx)));
   
                  if (entry_count == len_entry_count) {
                     PRINTMSG(AT_DEF_LINE(attr_idx), 662, Error,
                              AT_DEF_COLUMN(attr_idx), 
                              AT_OBJ_NAME_PTR(attr_idx));
                     AT_DCL_ERR(attr_idx) = TRUE;
                  }
                  else if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
                           ATD_CLASS(attr_idx) == Variable) {
                     PRINTMSG(AT_DEF_LINE(attr_idx), 1046, Caution,
                              AT_DEF_COLUMN(attr_idx),
                              AT_OBJ_NAME_PTR(attr_idx));
                  }
               }
            }
         }

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {

            if (ATD_CLASS(attr_idx) != Function_Result &&
                ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Function &&
                ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Subroutine) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1014, Error,
                        AT_DEF_COLUMN(attr_idx), 
                        AT_OBJ_NAME_PTR(attr_idx));
               AT_DCL_ERR(attr_idx) = TRUE;
            }
            if (ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1577, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
               AT_DCL_ERR(attr_idx) = TRUE;
            }
            else if (ATD_CLASS(attr_idx) == Variable) {
               ATD_AUTOMATIC(attr_idx)  = TRUE;
            }
         }
      }
   }
   else if (TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char) {

      /* This is called from PARAMETER processing.  This must be a const  */
      /* length array.  If it is not, do not process now.  It will happen */
      /* at decl_sematics time.  PARAMETER processing will issue error.   */

      if (must_be_const_array) {
         goto EXIT;
      }

      if (AT_OBJ_CLASS(attr_idx) == Stmt_Func) {
         ATD_TYPE_IDX(attr_idx) = CHARACTER_DEFAULT_TYPE;
      }
      else {

         switch (ATD_CLASS(attr_idx)) {
         case Function_Result:

            if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX || ATD_POINTER(attr_idx)) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 507, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
               AT_DCL_ERR(attr_idx)			= TRUE;
               AT_DCL_ERR(ATD_FUNC_IDX(attr_idx))	= TRUE;
               break;
            }

            if (ATD_FUNC_IDX(attr_idx) != SCP_ATTR_IDX(curr_scp_idx) &&
                !ATP_ALT_ENTRY(ATD_FUNC_IDX(attr_idx)) &&
                ATP_PROC(ATD_FUNC_IDX(attr_idx)) != Dummy_Proc) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1107, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(ATD_FUNC_IDX(attr_idx)));
               AT_DCL_ERR(attr_idx)			= TRUE;
               AT_DCL_ERR(ATD_FUNC_IDX(attr_idx))	= TRUE;
               break;
            }

            /* This is an intentional fall through.  All character*(*) */
            /* function results will be passed thru the interface as   */
            /* dummy arguments.                                        */

         case Dummy_Argument:

            /* Generate tmp = clen(attr).  This must go on the bound list */
            /* because this is a dummy argument.                          */

            NTR_IR_TBL(len_idx);
            IR_OPR(len_idx)		= Clen_Opr;
            IR_TYPE_IDX(len_idx)        = SA_INTEGER_DEFAULT_TYPE;
            IR_LINE_NUM(len_idx)	= AT_DEF_LINE(attr_idx);  
            IR_COL_NUM(len_idx)		= AT_DEF_COLUMN(attr_idx);  

            if (ATD_CLASS(attr_idx) == Function_Result &&
                ATP_PROC(ATD_FUNC_IDX(attr_idx)) == Dummy_Proc) {
               IR_IDX_L(len_idx)	= ATD_FUNC_IDX(attr_idx); 
            }
            else {
               IR_IDX_L(len_idx)	= attr_idx;
            }

            IR_FLD_L(len_idx)		= AT_Tbl_Idx;
            IR_LINE_NUM_L(len_idx)	= AT_DEF_LINE(attr_idx);  
            IR_COL_NUM_L(len_idx)	= AT_DEF_COLUMN(attr_idx);  

            OPND_FLD(opnd)		= IR_Tbl_Idx;
            OPND_IDX(opnd)		= len_idx;
            OPND_LINE_NUM(opnd)		= AT_DEF_LINE(attr_idx);
            OPND_COL_NUM(opnd)		= AT_DEF_COLUMN(attr_idx);

            {
               expr_arg_type	exp_desc;
               fold_clen_opr(&opnd, &exp_desc);
            }

            gen_sh(After,
                   Automatic_Base_Size_Stmt,
                   stmt_start_line,
                   stmt_start_col,
                   FALSE,
                   FALSE,
                   TRUE);

            find_opnd_line_and_column(&opnd, &line, &column);
            GEN_COMPILER_TMP_ASG(ir_idx,
                                 tmp_attr_idx,
                                 TRUE,		/* Semantics is done */
                                 line,
                                 column,
                                 SA_INTEGER_DEFAULT_TYPE,
                                 Priv);
   
            COPY_OPND(IR_OPND_R(ir_idx), opnd);      /* IR_OPND_R = opnd */

            SH_P2_SKIP_ME(curr_stmt_sh_idx)	= TRUE;
            SH_IR_IDX(curr_stmt_sh_idx)		= ir_idx;
            ATD_TMP_IDX(tmp_attr_idx)		= ir_idx;
            ATD_FLD(tmp_attr_idx)		= IR_Tbl_Idx;

            /* Create new entry, because each assumed sized darg has a     */
            /* different tmp to go with it.                                */

            type_tbl[TYP_WORK_IDX]	= type_tbl[ATD_TYPE_IDX(attr_idx)];
            TYP_FLD(TYP_WORK_IDX)	= AT_Tbl_Idx;
            TYP_IDX(TYP_WORK_IDX)	= tmp_attr_idx;

            ATD_TYPE_IDX(attr_idx)	= ntr_type_tbl();

            /* insert_sh_after_entries will handle this code at alternate  */
            /* entry points.  It will also take care of any OPTIONAL stuff */
            /* that needs to be generated.                                 */

            insert_sh_after_entries(attr_idx,
                                    SH_PREV_IDX(curr_stmt_sh_idx),
                                    curr_stmt_sh_idx,
                                    FALSE,     /* Don't generate tmp = 0  */
                                    TRUE);     /* Advance ATP_FIRST_SH_IDX */
            break;

         case CRI__Pointee:
           
            /* TYP_IDX becomes the attr index of the pointer.  A new entry    */
            /* is made, because this entry cannot share with another.         */

            type_tbl[TYP_WORK_IDX]	= type_tbl[ATD_TYPE_IDX(attr_idx)];
            TYP_FLD(TYP_WORK_IDX)	= AT_Tbl_Idx;
            TYP_IDX(TYP_WORK_IDX)	= ATD_PTR_IDX(attr_idx);

            ATD_TYPE_IDX(attr_idx)	= ntr_type_tbl();
            break;

         case Constant:
            break;

         default: /* This must be a darg, constant, or CRI pointee */
            PRINTMSG(AT_DEF_LINE(attr_idx), 350, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx) = TRUE;
            break;
         }  /* End switch */
      }
   }
   else if (TYP_CHAR_CLASS(type_idx) == Const_Len_Char) {

      if (compare_cn_and_value(TYP_IDX(type_idx), 0, Lt_Opr)) {
         type_tbl[TYP_WORK_IDX]		= type_tbl[type_idx];
         TYP_IDX(TYP_WORK_IDX)		= CN_INTEGER_ZERO_IDX;
         ATD_TYPE_IDX(attr_idx)		= ntr_type_tbl();
      }
      else if (compare_cn_and_value(TYP_IDX(type_idx), 
                                    max_character_length, 
                                    Gt_Opr) &&
               TYP_TYPE(CN_TYPE_IDX(TYP_IDX(type_idx))) == Integer) {

         PRINTMSG(AT_DEF_LINE(attr_idx), 35, Error,
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx),
                  max_character_length);

         type_tbl[TYP_WORK_IDX]	= type_tbl[type_idx];
         TYP_IDX(TYP_WORK_IDX)	= C_INT_TO_CN(CN_TYPE_IDX(TYP_IDX(type_idx)),
                                              max_character_length);
         ATD_TYPE_IDX(attr_idx)	= ntr_type_tbl();
         AT_DCL_ERR(attr_idx)	= TRUE;
      }
   }

EXIT:

   TYP_RESOLVED(ATD_TYPE_IDX(attr_idx))	= TRUE;

   TRACE (Func_Exit, "char_len_resolution", NULL);

   return;

}  /* char_len_resolution */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      compares two dummy arguments for type, kind type, and rank.           *|
|*	This is used for verifyng interfaces and for interface resolution.    *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      idx1, idx2 - the two dummies.                                         *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE is same in all three categories.                                 *|
|*      FALSE otherwise.                                                      *|
|*                                                                            *|
\******************************************************************************/
boolean  compare_dummy_arguments(int      idx1,
				 int      idx2)

{
   int		i;
   boolean      same		= TRUE;


   TRACE (Func_Entry, "compare_dummy_arguments", NULL);

   if (AT_OBJ_CLASS(idx1) == AT_OBJ_CLASS(idx2)) {

      if (AT_OBJ_CLASS(idx1) == Pgm_Unit) {

         if (!ATP_EXPL_ITRFC(idx1) || !ATP_EXPL_ITRFC(idx2)) {

            /* We can only disambiguate, if an explicit interface */ 
            /* is specified for the dummy procedure.              */

            same = FALSE;
         }
         else if (ATP_PGM_UNIT(idx1) != ATP_PGM_UNIT(idx2) &&
                  ATP_PGM_UNIT(idx1) != Pgm_Unknown &&
                  ATP_PGM_UNIT(idx2) != Pgm_Unknown) {
            same = FALSE; /* Have func vs Subr */
         }
         else {

            if (ATP_PGM_UNIT(idx1) == ATP_PGM_UNIT(idx2) &&
                ATP_PGM_UNIT(idx2) == Function) {

               /* Both functions - compare results */

               same = compare_darg_or_rslt_types(ATP_RSLT_IDX(idx1),
                                                 ATP_RSLT_IDX(idx2));
            }

            if (same) {  /* Compare the dargs */

               if (ATP_NUM_DARGS(idx1) != ATP_NUM_DARGS(idx2)) {
                  same = FALSE;
               }
               else {

                  /* We know the result type is the same, so either both */
                  /* have ATP_EXTRA_DARG set or both have it FALSE.      */

                  for (i = (ATP_EXTRA_DARG(idx1) ? 1 : 0);
                       i < ATP_NUM_DARGS(idx1); i++) {
                     same = compare_dummy_arguments(
                                    SN_ATTR_IDX((ATP_FIRST_IDX(idx1)+i)),
                                    SN_ATTR_IDX((ATP_FIRST_IDX(idx2)+i)));

                     if (!same) break;
                  }
               }
            }
         }
      }
      else if (AT_OBJ_CLASS(idx1) == Data_Obj) {

         if (ATD_CLASS(idx1) == ATD_CLASS(idx2)) {

            /* If either one is IGNORE_TKR they are the same type and rank. */

            if (ATD_CLASS(idx1) == Dummy_Argument && 
                !ATD_IGNORE_TKR(idx1) && !ATD_IGNORE_TKR(idx2)) {
               same = compare_darg_or_rslt_types(idx1, idx2);
            }
         }
         else {
            same = FALSE;
         }
      }
   }
   else {
      same = FALSE;
   }

   TRACE (Func_Exit, "compare_dummy_arguments", NULL);

   return(same);

}  /* compare_dummy_arguments */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine does the semantic error checking between the function    *|
|*	result name and entry names.                                          *|
|*									      *|
|* Input parameters:							      *|
|*	rslt_idx        -> attr idx for the result name.		      *|
|*	pgm_rslt_idx    -> Result index for the external program.	      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
static void compare_entry_to_func_rslt(int	attr_idx,
				       int	pgm_rslt_idx)
{
   int		column;
   int		idx;
   int		line;
   int		loop;
#ifdef KEY /* Bug 10177 */
   boolean	not_a_match = FALSE;
#else /* KEY Bug 10177 */
   boolean	not_a_match;
#endif /* KEY Bug 10177 */
   int		pgm_type_idx;
   int		rslt_idx;
   int		rslt_type_idx;


   TRACE (Func_Entry, "compare_entry_to_func_rslt", NULL);

   line		= AT_DEF_LINE(attr_idx);
   column	= AT_DEF_COLUMN(attr_idx);
   rslt_idx	= ATP_RSLT_IDX(attr_idx);
   rslt_type_idx= ATD_TYPE_IDX(rslt_idx);
   pgm_type_idx	= ATD_TYPE_IDX(pgm_rslt_idx);


   if (ATD_ARRAY_IDX(rslt_idx) != NULL_IDX &&
       BD_ARRAY_SIZE(ATD_ARRAY_IDX(rslt_idx)) == Symbolic_Constant_Size) {
      PRINTMSG(line, 1230, Error, column, AT_OBJ_NAME_PTR(attr_idx));
   }
   else if (ATD_ARRAY_IDX(rslt_idx) != ATD_ARRAY_IDX(pgm_rslt_idx) &&
       !compare_array_entries(ATD_ARRAY_IDX(rslt_idx),
                              ATD_ARRAY_IDX(pgm_rslt_idx))) {
      PRINTMSG(line, 673, Error, column, AT_OBJ_NAME_PTR(pgm_rslt_idx),
                                         AT_OBJ_NAME_PTR(rslt_idx));
   }
   else if (ATD_POINTER(pgm_rslt_idx) != ATD_POINTER(rslt_idx)) {
         PRINTMSG(line, 674, Error, column, AT_OBJ_NAME_PTR(pgm_rslt_idx),
                                           AT_OBJ_NAME_PTR(rslt_idx));
   }
   else if (TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) !=
            TYP_TYPE(ATD_TYPE_IDX(pgm_rslt_idx))) {

      if (TYP_TYPE(rslt_type_idx) > Complex || 
          TYP_TYPE(pgm_type_idx) > Complex) {
         PRINTMSG(line, 21, Error, column, AT_OBJ_NAME_PTR(pgm_rslt_idx),
                                           AT_OBJ_NAME_PTR(rslt_idx));
      }
# if defined(_TARGET_OS_MAX)

      else if (cmd_line_flags.integer_32 &&
               !cmd_line_flags.s_default32 &&
               ((TYP_TYPE(rslt_type_idx) == Integer &&
                 TYP_DESC(rslt_type_idx) == Default_Typed) ||
                (TYP_TYPE(pgm_type_idx) == Integer &&
                 TYP_DESC(pgm_type_idx) == Default_Typed))) {

         if (TYP_TYPE(rslt_type_idx) == Integer) {
            PRINTMSG(line, 1195, Warning, column, 
                     AT_OBJ_NAME_PTR(rslt_idx),
                     AT_OBJ_NAME_PTR(pgm_rslt_idx));
         }
         else {
            PRINTMSG(line, 1195, Warning, column, 
                     AT_OBJ_NAME_PTR(pgm_rslt_idx),
                     AT_OBJ_NAME_PTR(rslt_idx));
         }
      }
# endif
      else if (on_off_flags.issue_ansi_messages ||
               GET_MESSAGE_TBL(message_warning_tbl, 22) ||
               GET_MESSAGE_TBL(message_error_tbl, 22)) {

         /* The standard requires mixed types (COMPLEX, LOGICAL, INTEGER,  */
         /* REAL) to all be of default type.  If ANSI checking is on, this */
         /* for/switch checks the rslt_idx and then the pgm_rslt_idx to see*/
         /* if any are non_default types.  An ANSI msg is issued if found. */
           
         idx	= rslt_idx;

         for (loop = 1; loop <=2; loop++) {

            switch (TYP_TYPE(ATD_TYPE_IDX(idx))) {
            case Logical:
               not_a_match = TYP_LINEAR(ATD_TYPE_IDX(idx)) != 
                                                          LOGICAL_DEFAULT_TYPE;
               break;

            case Complex:
               not_a_match = TYP_LINEAR(ATD_TYPE_IDX(idx)) !=
                                                          COMPLEX_DEFAULT_TYPE;
               break;

            case Integer:
               not_a_match = TYP_LINEAR(ATD_TYPE_IDX(idx)) !=
                                                          INTEGER_DEFAULT_TYPE;
               break;

            case Real:
               not_a_match = (TYP_LINEAR(ATD_TYPE_IDX(idx)) !=
                                           REAL_DEFAULT_TYPE &&
                              TYP_LINEAR(ATD_TYPE_IDX(idx)) !=
                                           DOUBLE_DEFAULT_TYPE);
               break;
            }  /* switch */

            if (not_a_match) {
               PRINTMSG(line, 22, Ansi, column, AT_OBJ_NAME_PTR(idx));
            }

            idx = pgm_rslt_idx;
         }  /* end FOR */
      }
   }
   else if (TYP_LINEAR(rslt_type_idx) == TYP_LINEAR(pgm_type_idx) &&
            TYP_IDX(rslt_type_idx) == TYP_IDX(pgm_type_idx)) {

      /* This is the same linear type or the same character length or the */
      /* same structure.   Intentionally left blank.                      */
   }
   else if (TYP_TYPE(rslt_type_idx) == Character) {

      /* Do not issue the error, if they are both variable length, because */
      /* this cannot be detected at compile time.                          */

      if (TYP_CHAR_CLASS(rslt_type_idx) == Const_Len_Char &&
          TYP_CHAR_CLASS(pgm_type_idx) == Const_Len_Char &&
          fold_relationals(TYP_IDX(rslt_type_idx), 
                           TYP_IDX(pgm_type_idx), Ne_Opr)) {
         PRINTMSG(line, 21, Error, column, AT_OBJ_NAME_PTR(pgm_rslt_idx),
                                           AT_OBJ_NAME_PTR(rslt_idx));
      }
   }
   else if (TYP_TYPE(rslt_type_idx) == Structure) {

      /* Both are structures with different structure indexes.   Check if  */
      /* the structures are the same.                                      */

      if (!compare_derived_types(rslt_type_idx, pgm_type_idx)) {
         PRINTMSG(line, 21, Error, column, AT_OBJ_NAME_PTR(pgm_rslt_idx),
                                           AT_OBJ_NAME_PTR(rslt_idx));
      }
   }
   else if (on_off_flags.issue_ansi_messages ||
            GET_MESSAGE_TBL(message_warning_tbl, 13) ||
            GET_MESSAGE_TBL(message_error_tbl, 13)) {


      /* Types match, but TYPE_IDX differs.  This means that both cannot   */
      /* be default types (unless one is default real and the other is     */
      /* default double precision.), so if ANSI checking, issue msg.       */

      if ((TYP_TYPE(rslt_type_idx) == Real) &&
          (TYP_LINEAR(rslt_type_idx) == REAL_DEFAULT_TYPE ||
           TYP_LINEAR(rslt_type_idx) == DOUBLE_DEFAULT_TYPE) &&
          (TYP_LINEAR(pgm_type_idx) == REAL_DEFAULT_TYPE ||
           TYP_LINEAR(pgm_type_idx) == DOUBLE_DEFAULT_TYPE)) {
         /* This is double precision default and a real default -ok no msg */
      }
      else {
         PRINTMSG(line, 13, Ansi, column, AT_OBJ_NAME_PTR(pgm_rslt_idx),
                                          AT_OBJ_NAME_PTR(rslt_idx));
      }
   }

   TRACE (Func_Exit, "compare_entry_to_func_rslt", NULL);

   return;

}  /* compare_entry_to_func_rslt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This does semantic checking for the declaration statements.           *|
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
void	decl_semantics(void)

{
   int		al_idx;
   int		attr_idx;
   int		count;
   int		darg_idx;
   int		darg_list_idx;
   int		eq_idx;
   int		entry_attr_idx;
   int		entry_idx;
   int		entry_list_idx;
   int		group;
   int		idx;
   int		label_sh_idx;
   int		line;
   int		list_idx;
   int		list_idx2;
   int		name_idx;
   opnd_type	opnd;
   int		pgm_attr_idx;
   int		prev_idx;
   boolean	recursive;
   int		rslt_idx;
   int		save_curr_stmt_sh_idx;
   int          sh_after_entry_idx;


   TRACE (Func_Entry, "decl_semantics",  NULL);

   pgm_attr_idx	= SCP_ATTR_IDX(curr_scp_idx);

   /* Implement the save all commandline option  -ev */

   if (on_off_flags.save_all_vars) {

      if (ATP_RECURSIVE(pgm_attr_idx)) {
         PRINTMSG(AT_DEF_LINE(pgm_attr_idx), 1103, Caution,
                  AT_DEF_COLUMN(pgm_attr_idx),
                  AT_OBJ_NAME_PTR(pgm_attr_idx));
      }
      else if (SCP_PARENT_IDX(curr_scp_idx) != NULL_IDX) {

            /* Check if the parent is recursive. */

         idx            = SCP_PARENT_IDX(curr_scp_idx);
         recursive      = FALSE;

         do {

            if (ATP_RECURSIVE(SCP_ATTR_IDX(idx))) {
               recursive = TRUE;
               break;
            }
            idx = SCP_PARENT_IDX(idx);
         }
         while (idx != NULL_IDX);

         if (!recursive) {
            ATP_SAVE_ALL(pgm_attr_idx) = TRUE;
         }
      }
      else {
         ATP_SAVE_ALL(pgm_attr_idx) = TRUE;
      }
   }

   /* Set the default storage for this procedure. */

   if (ATP_PGM_UNIT(pgm_attr_idx) == Module) {
       SCP_DEFAULT_STORAGE(curr_scp_idx) = Static;
   }
   else if (!ATP_SAVE_ALL(pgm_attr_idx) ||
             ATP_STACK_DIR(pgm_attr_idx) ||
             ATP_RECURSIVE(pgm_attr_idx) ||
             (on_off_flags.recursive &&
              (ATP_PGM_UNIT(pgm_attr_idx) == Function ||
               ATP_PGM_UNIT(pgm_attr_idx) == Subroutine))) {
       SCP_DEFAULT_STORAGE(curr_scp_idx) = Stack;
   }
   else {
       SCP_DEFAULT_STORAGE(curr_scp_idx) = Static;
   }

   /* Set up global variables needed for decl_semantics and attr_semantics.  */

   allocatable_list_idx		= NULL_IDX;
   namelist_list_idx		= NULL_IDX;
   number_of_allocatables	= 0;
   pointee_based_blk		= NULL_IDX;
   alt_entry_equiv_blk		= NULL_IDX;
   alt_entry_equiv_grp		= NULL_IDX;
   reshape_array_list		= NULL_IDX;
   init_sh_start_idx		= NULL_IDX;
   init_sh_end_idx		= NULL_IDX;

   /* At entry curr_stmt_sh_idx is set to the first stmt of the pgm unit.    */
   /* All entry code will insert after curr_stmt_sh_idx.  After the symbol   */
   /* table is gone through the rest of the IR must be connected back up to  */
   /* curr_stmt_sh_idx.                                                      */

   sh_after_entry_idx			= SH_NEXT_IDX(curr_stmt_sh_idx);
   SH_NEXT_IDX(curr_stmt_sh_idx)        = NULL_IDX;
   SH_PREV_IDX(sh_after_entry_idx)      = NULL_IDX;

   if (cmd_line_flags.debug_lvl <= Debug_Lvl_1 &&
       ATP_PGM_UNIT(pgm_attr_idx) <= Program) {

      /* If -G0 or  -G1 specified and this is not a module or blockdata, */
      /* we need to correct the line number for the Ldbg_End_Prologue    */
      /* label.  The label needs to point to the first executable        */
      /* statement.  Pass up all data and initialization statements.     */
      /* These do not count as executable statements for debug.          */

      idx		= SH_NEXT_IDX(sh_after_entry_idx);
      label_sh_idx	= sh_after_entry_idx;

      while (SH_STMT_TYPE(idx) == Type_Init_Stmt ||
             SH_STMT_TYPE(idx) == Data_Stmt) {
            idx = SH_NEXT_IDX(idx);
      }

      if (idx != SH_NEXT_IDX(sh_after_entry_idx)) {
 
         /* Move End_Prologue_Label after initialization statements. */
         /* Do not reconnect SH_NEXT_IDX of curr_stmt_sh_idx.  It    */
         /* will be connected after decl_semantics.                  */

         sh_after_entry_idx			= SH_NEXT_IDX(label_sh_idx);
         SH_PREV_IDX(sh_after_entry_idx)        = NULL_IDX;

         SH_NEXT_IDX(label_sh_idx) 		= idx;

         if (SH_PREV_IDX(idx)) {
            SH_NEXT_IDX(SH_PREV_IDX(idx))		= label_sh_idx;
         }
         SH_PREV_IDX(label_sh_idx)		= SH_PREV_IDX(idx);
         SH_PREV_IDX(idx)			= label_sh_idx;
      }

      line						= SH_GLB_LINE(idx);
      SH_GLB_LINE(label_sh_idx)				= line;
      IR_LINE_NUM(SH_IR_IDX(label_sh_idx))		= line;
      IR_LINE_NUM_L(SH_IR_IDX(label_sh_idx))		= line;
      AT_DEF_LINE(IR_IDX_L(SH_IR_IDX(label_sh_idx)))	= line;
   }

   if (SCP_ALT_ENTRY_CNT(curr_scp_idx) > 0) {

      if (ATP_PGM_UNIT(pgm_attr_idx) == Function &&
          (TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(pgm_attr_idx))) == Character ||
           ATD_ARRAY_IDX(ATP_RSLT_IDX(pgm_attr_idx)) != NULL_IDX)) {
         entry_idx = SCP_ENTRY_IDX(curr_scp_idx);

         /* Add the main entry point to all the alternate entry points, */
         /* so that tmps generated for bounds for the main entry point  */
         /* will not show up at other entry points.                     */

         while (entry_idx != NULL_IDX) {

            /* Add the main attr to the entry attr list. */
   
            NTR_ATTR_LIST_TBL(list_idx);
            AL_ATTR_IDX(list_idx)	= pgm_attr_idx;
            entry_attr_idx		= AL_ATTR_IDX(entry_idx);

            if (ATP_NO_ENTRY_LIST(entry_attr_idx) != NULL_IDX) {
               AL_NEXT_IDX(list_idx)	= ATP_NO_ENTRY_LIST(entry_attr_idx);
               AL_ENTRY_COUNT(list_idx)	= 
                                       AL_ENTRY_COUNT(AL_NEXT_IDX(list_idx))+ 1;
            }
            else {
               AL_ENTRY_COUNT(list_idx)	= 1;
            }

            ATP_NO_ENTRY_LIST(entry_attr_idx)	= list_idx;

            /* Add the entry attr to the main attr's list */

            NTR_ATTR_LIST_TBL(list_idx);
            AL_ATTR_IDX(list_idx)		= entry_attr_idx;
   
            if (ATP_NO_ENTRY_LIST(pgm_attr_idx) != NULL_IDX) {
               AL_NEXT_IDX(list_idx)	 = ATP_NO_ENTRY_LIST(pgm_attr_idx);
               AL_ENTRY_COUNT(list_idx)	 = 
                                      AL_ENTRY_COUNT(AL_NEXT_IDX(list_idx)) + 1;
            }
            else {
               AL_ENTRY_COUNT(list_idx)	 = 1;
            }

            ATP_NO_ENTRY_LIST(pgm_attr_idx) = list_idx;

            entry_list_idx = SCP_ENTRY_IDX(curr_scp_idx);

            while (entry_list_idx != NULL_IDX) {

               if (entry_attr_idx != AL_ATTR_IDX(entry_list_idx)) {
                  NTR_ATTR_LIST_TBL(list_idx);
                  AL_ATTR_IDX(list_idx)	= entry_attr_idx;

                  if (ATP_NO_ENTRY_LIST(AL_ATTR_IDX(entry_list_idx)) != 
                                                                     NULL_IDX) {
                     AL_NEXT_IDX(list_idx) = 
                                 ATP_NO_ENTRY_LIST(AL_ATTR_IDX(entry_list_idx));
                     AL_ENTRY_COUNT(list_idx) = 
                                 AL_ENTRY_COUNT(AL_NEXT_IDX(list_idx)) + 1;
                  }
                  else {
                     AL_ENTRY_COUNT(list_idx) = 1;
                  }
                  ATP_NO_ENTRY_LIST(AL_ATTR_IDX(entry_list_idx)) = list_idx;
               }
               entry_list_idx = AL_NEXT_IDX(entry_list_idx);
            }
            entry_idx = AL_NEXT_IDX(entry_idx);
         }
      }

      /* Create a list for each darg, of entry points the darg is NOT at.    */
      /* Also, create a list of the SH index for each alternate entry point. */

      /* Process !DIR$ IGNORE TYPE AND KIND directive */

      darg_list_idx	= SCP_DARG_LIST(curr_scp_idx);

      while (darg_list_idx != NULL_IDX) {
         darg_idx	= AL_ATTR_IDX(darg_list_idx);
         darg_list_idx	= AL_NEXT_IDX(darg_list_idx);
         list_idx	= NULL_IDX;

           
         if (SCP_IGNORE_TKR(curr_scp_idx) &&
             AT_OBJ_CLASS(darg_idx) == Data_Obj) {

            if (!fnd_semantic_err(Obj_Ignore_TKR,
                                  AT_DEF_LINE(darg_idx),
                                  AT_DEF_COLUMN(darg_idx),
                                  darg_idx,
                                  TRUE)) {
               ATD_IGNORE_TKR(darg_idx)	= TRUE;
            }
         }

         if (!darg_in_entry_list(darg_idx, pgm_attr_idx)) {
            NTR_ATTR_LIST_TBL(list_idx);
            AL_ATTR_IDX(list_idx)	= pgm_attr_idx;
            AT_ALT_DARG(darg_idx)	= TRUE;
            AL_ENTRY_COUNT(list_idx)	= 1;

            if (AT_OBJ_CLASS(darg_idx) == Data_Obj) {
               ATD_NO_ENTRY_LIST(darg_idx)	= list_idx;
            }
            else {
               ATP_NO_ENTRY_LIST(darg_idx)	= list_idx;
            }
         }

         entry_list_idx = SCP_ENTRY_IDX(curr_scp_idx);

         while(entry_list_idx != NULL_IDX) {
            entry_attr_idx	= AL_ATTR_IDX(entry_list_idx);
            entry_list_idx	= AL_NEXT_IDX(entry_list_idx);

            if (!darg_in_entry_list(darg_idx, entry_attr_idx)) {
               prev_idx				= list_idx;
               NTR_ATTR_LIST_TBL(list_idx);

               if (prev_idx == NULL_IDX) {
                  AL_ENTRY_COUNT(list_idx)	= 1;

                  if (AT_OBJ_CLASS(darg_idx) == Data_Obj) {
                     ATD_NO_ENTRY_LIST(darg_idx) = list_idx;
                  }
                  else {
                     ATP_NO_ENTRY_LIST(darg_idx) = list_idx;
                  }
               }
               else {
                  AL_NEXT_IDX(prev_idx)			 = list_idx;

                  if (AT_OBJ_CLASS(darg_idx) == Data_Obj) {
                     AL_ENTRY_COUNT(ATD_NO_ENTRY_LIST(darg_idx))+=1;
                  }
                  else {
                     AL_ENTRY_COUNT(ATP_NO_ENTRY_LIST(darg_idx))+=1;
                  }
               }
               AL_ATTR_IDX(list_idx)		= entry_attr_idx;
               AT_ALT_DARG(darg_idx)		= TRUE;
            }
         }
      }
   }
   else {
      darg_list_idx	= SCP_DARG_LIST(curr_scp_idx);

      while (darg_list_idx != NULL_IDX) {
         darg_idx	= AL_ATTR_IDX(darg_list_idx);
         darg_list_idx	= AL_NEXT_IDX(darg_list_idx);
           
         if (SCP_IGNORE_TKR(curr_scp_idx) &&
             AT_OBJ_CLASS(darg_idx) == Data_Obj) {

            if (!fnd_semantic_err(Obj_Ignore_TKR,
                                  AT_DEF_LINE(darg_idx),
                                  AT_DEF_COLUMN(darg_idx),
                                  darg_idx,
                                  TRUE)) {
               ATD_IGNORE_TKR(darg_idx)	= TRUE;
            }
         }
      }
   }

   if (opt_flags.reshape) {

      /* Set ATD_RESHAPE_ARRAY_OPT for specific attrs */
      /* that are specified on the commandline.        */

      reshape_array_semantics();

   }

   /* There may be bounds temps hidden in the implicit table that need to be  */
   /* folded.  These come up when something like IMPLICIT CHARACTER*(n) (a-z) */
   /* is specified.  This code checks the implicit table for this scope.      */

   for (idx = 0; idx < MAX_IMPL_CHS; idx++) {

      if (IM_SET(curr_scp_idx, idx) && 
          TYP_TYPE(IM_TYPE_IDX(curr_scp_idx, idx)) == Character &&
          TYP_FLD(IM_TYPE_IDX(curr_scp_idx, idx)) == AT_Tbl_Idx) {
         attr_semantics(TYP_IDX(IM_TYPE_IDX(curr_scp_idx, idx)), TRUE);
      }
   }

   /* Process the program name first, so that any other object that needs */
   /* to refer to it or check against it, gets the correct information.   */

   attr_semantics(pgm_attr_idx, FALSE);

   /* There are seperate lists for stride multipliers, extents, array lengths,*/
   /* and max(0,char length) tmps.  These can never share with character len, */
   /* lower bound and upper bounds tmps, because the stride ect..  tmps all   */
   /* reference other tmps in their expressions.  IR is generated and         */
   /* attached to curr_stmt_sh_idx for extents, strides, and lengths.  If the */
   /* object is an automatic object, the allocate IR will then generate.      */
   /* This allocate will always follow its length IR(s) and will be of the    */
   /* tmp = form.                                                             */
   
   for (name_idx = SCP_LN_FW_IDX(curr_scp_idx) + 1; 
        name_idx < SCP_LN_LW_IDX(curr_scp_idx); name_idx++) {

      attr_idx = LN_ATTR_IDX(name_idx);
      attr_semantics(attr_idx, FALSE);
   }

   al_idx = SCP_ATTR_LIST(curr_scp_idx);

   while (al_idx != NULL_IDX) {
      attr_idx	= AL_ATTR_IDX(al_idx);
      al_idx	= AL_NEXT_IDX(al_idx);

      attr_semantics(attr_idx, FALSE);
   }

# if !defined(_SINGLE_ALLOCS_FOR_AUTOMATIC)

  /* Force saved automatic ir into statements */

   gen_multiple_automatic_allocate(NULL_IDX); 

# endif

   /* There may be statements before sh_after_entry_idx.       */
   /* Find the beginning before hooking up sh_after_entry_idx. */

   while (SH_PREV_IDX(sh_after_entry_idx) != NULL_IDX) {
      sh_after_entry_idx = SH_PREV_IDX(sh_after_entry_idx);
   }

   /* There may be statements following curr_stmt_sh_idx. */
   /* Find the end before hooking up sh_after_entry_idx.  */

   while (SH_NEXT_IDX(curr_stmt_sh_idx) != NULL_IDX) {
      curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
   }

   if (init_sh_start_idx != NULL_IDX) {

      /* Insert any default initialization Init_Oprs */

      SH_NEXT_IDX(init_sh_end_idx)		= SH_NEXT_IDX(curr_stmt_sh_idx);
      SH_NEXT_IDX(curr_stmt_sh_idx)		= init_sh_start_idx;
      SH_PREV_IDX(init_sh_start_idx)		= curr_stmt_sh_idx;
      SH_PREV_IDX(SH_NEXT_IDX(init_sh_end_idx))	= init_sh_end_idx;
      curr_stmt_sh_idx		    		= init_sh_end_idx;
   }

   SH_NEXT_IDX(curr_stmt_sh_idx)   = sh_after_entry_idx;
   SH_PREV_IDX(sh_after_entry_idx) = curr_stmt_sh_idx;

   if (ATP_PGM_UNIT(pgm_attr_idx) == Function &&
       SCP_ENTRY_IDX(curr_scp_idx) != NULL_IDX &&
       TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(pgm_attr_idx))) == Character) {

      /* If this is a character function with character alternate entries */
      /* equivalence all the character size temps together.               */

      /* BHJ - JBL - You may want to make temps for constant  */
      /* size entries as well and equiv them too, but I'm not */
      /* going to take the implementation quite that far.     */

      count	= 0;
      al_idx	= SCP_ENTRY_IDX(curr_scp_idx);	

      while (al_idx != NULL_IDX) {
         rslt_idx = ATP_RSLT_IDX(AL_ATTR_IDX(al_idx));

         if (TYP_FLD(ATD_TYPE_IDX(rslt_idx)) == AT_Tbl_Idx) {
            NTR_EQ_TBL(eq_idx);
            EQ_LINE_NUM(eq_idx)		= AT_DEF_LINE(rslt_idx);
            EQ_COLUMN_NUM(eq_idx)	= AT_DEF_COLUMN(rslt_idx);
            EQ_ATTR_IDX(eq_idx) 	= TYP_IDX(ATD_TYPE_IDX(rslt_idx));
            ATD_EQUIV(EQ_ATTR_IDX(eq_idx)) = TRUE;
            group			= SCP_FIRST_EQUIV_GRP(curr_scp_idx);

            if (count == 0) {
               EQ_NEXT_EQUIV_GRP(eq_idx)		= group;
               SCP_FIRST_EQUIV_GRP(curr_scp_idx)	= eq_idx;
               group					= eq_idx;
            }
            else {
               EQ_NEXT_EQUIV_OBJ(EQ_GRP_END_IDX(group))	= eq_idx;
            }
            EQ_GRP_END_IDX(group)		= eq_idx;
            EQ_GRP_IDX(eq_idx)			= group;
            count++;
         }
         al_idx	= AL_NEXT_IDX(al_idx);
      }

      if (count > 0 &&
          TYP_FLD(ATD_TYPE_IDX(ATP_RSLT_IDX(pgm_attr_idx))) == AT_Tbl_Idx) {
         NTR_EQ_TBL(eq_idx);
         rslt_idx			= ATP_RSLT_IDX(pgm_attr_idx);
         EQ_LINE_NUM(eq_idx)		= AT_DEF_LINE(rslt_idx);
         EQ_COLUMN_NUM(eq_idx)		= AT_DEF_COLUMN(rslt_idx);
         EQ_ATTR_IDX(eq_idx)	 	= TYP_IDX(ATD_TYPE_IDX(rslt_idx));
         ATD_EQUIV(EQ_ATTR_IDX(eq_idx)) = TRUE;
         group				= SCP_FIRST_EQUIV_GRP(curr_scp_idx);
         EQ_NEXT_EQUIV_OBJ(EQ_GRP_END_IDX(group)) = eq_idx;
         EQ_GRP_END_IDX(group)		= eq_idx;
         EQ_GRP_IDX(eq_idx)		= group;
      }
      else if (count == 1) { /* Only one item on the list - loose it */
         SCP_FIRST_EQUIV_GRP(curr_scp_idx) = 
               EQ_NEXT_EQUIV_GRP(SCP_FIRST_EQUIV_GRP(curr_scp_idx));
     }
   }

   if (ATP_ARGCHCK_ENTRY(pgm_attr_idx)) {
      insert_argchck_calls(sh_after_entry_idx, pgm_attr_idx);

      if (SCP_ALT_ENTRY_CNT(curr_scp_idx) != 0) {
         entry_list_idx = SCP_ENTRY_IDX(curr_scp_idx);

         while (entry_list_idx != NULL_IDX) {
            insert_argchck_calls(ATP_ENTRY_LABEL_SH_IDX(AL_ATTR_IDX(
                                                        entry_list_idx)),
                                 AL_ATTR_IDX(entry_list_idx));
            entry_list_idx = AL_NEXT_IDX(entry_list_idx);
         }
      }
   }

   if (SCP_FIRST_EQUIV_GRP(curr_scp_idx) != NULL_IDX &&
       num_prog_unit_errors == 0) { 
      equivalence_semantics();
   }

   /* Put the list of alternate returns on the equiv list, if it exists. */
   /* Do now, so it doesn't go throuh equivalence_semantics.             */

   if (alt_entry_equiv_grp != NULL_IDX) {
      EQ_NEXT_EQUIV_GRP(alt_entry_equiv_grp)= SCP_FIRST_EQUIV_GRP(curr_scp_idx);
      EQ_SEMANTICS_DONE(alt_entry_equiv_grp)= TRUE;
      SCP_FIRST_EQUIV_GRP(curr_scp_idx)	    = alt_entry_equiv_grp;
   }

   if (namelist_list_idx != NULL_IDX) {
      namelist_resolution(namelist_list_idx);
   }

   if (allocatable_list_idx != NULL_IDX) {
      deallocate_local_allocatables();
   }

   if (ATP_PGM_UNIT(pgm_attr_idx) == Function ||
       ATP_PGM_UNIT(pgm_attr_idx) == Subroutine) {

      if (SCP_COPY_ASSUMED_SHAPE(curr_scp_idx) &&
          SCP_COPY_ASSUMED_LIST(curr_scp_idx) != NULL_IDX &&
          IL_FLD(SCP_COPY_ASSUMED_LIST(curr_scp_idx)) != NO_Tbl_Idx) {

        /* this is an error situation */
        PRINTMSG(IL_LINE_NUM(IL_IDX(SCP_COPY_ASSUMED_LIST(curr_scp_idx))),
                 1281, Error, 
                 IL_COL_NUM(IL_IDX(SCP_COPY_ASSUMED_LIST(curr_scp_idx))));
      }
      else if (SCP_COPY_ASSUMED_SHAPE(curr_scp_idx)) {

         idx = SCP_DARG_LIST(curr_scp_idx);

         list_idx = NULL_IDX;
         OPND_IDX(opnd) = NULL_IDX;

         while (idx != NULL_IDX) {

            attr_idx = AL_ATTR_IDX(idx);

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
                ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
                BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Assumed_Shape) {

               if (list_idx == NULL_IDX) {
                  NTR_IR_LIST_TBL(list_idx);
                  OPND_FLD(opnd) = IL_Tbl_Idx;
                  OPND_IDX(opnd) = list_idx;
                  OPND_LIST_CNT(opnd) = 1;
               }
               else {
                  NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
                  OPND_LIST_CNT(opnd) += 1;
               }

               IL_FLD(list_idx) = AT_Tbl_Idx;
               IL_IDX(list_idx) = attr_idx;
               IL_LINE_NUM(list_idx) = 
                            IL_LINE_NUM(SCP_COPY_ASSUMED_LIST(curr_scp_idx));
               IL_COL_NUM(list_idx) = 
                            IL_COL_NUM(SCP_COPY_ASSUMED_LIST(curr_scp_idx));
            }

            idx = AL_NEXT_IDX(idx);
         }

         if (OPND_IDX(opnd) != NULL_IDX) {
            reassign_XT_temps = must_reassign_XT_temp(&opnd);
            shared_bd_idx = -1;
            list_idx = OPND_IDX(opnd);

            while (list_idx != NULL_IDX) {
               curr_stmt_sh_idx = sh_after_entry_idx;
               gen_assumed_shape_copy(&IL_OPND(list_idx));
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         }
         else {
            PRINTMSG(IL_LINE_NUM(SCP_COPY_ASSUMED_LIST(curr_scp_idx)),
                     1304, Caution,
                     IL_COL_NUM(SCP_COPY_ASSUMED_LIST(curr_scp_idx)));
         }
      }
      else if (SCP_COPY_ASSUMED_LIST(curr_scp_idx) != NULL_IDX) {
         list_idx = SCP_COPY_ASSUMED_LIST(curr_scp_idx);

         while (list_idx) {
            shared_bd_idx = -1;
            COPY_OPND(opnd, IL_OPND(list_idx));
            reassign_XT_temps = must_reassign_XT_temp(&opnd);
            list_idx2 = OPND_IDX(opnd);

            while (list_idx2) {
               if (AT_DCL_ERR(IL_IDX(list_idx2))) {
                  /* intentionally blank */
               }
               else if (AT_OBJ_CLASS(IL_IDX(list_idx2)) != Data_Obj ||
                        ATD_ARRAY_IDX(IL_IDX(list_idx2)) == NULL_IDX ||
                        BD_ARRAY_CLASS(ATD_ARRAY_IDX(IL_IDX(list_idx2))) != 
                                                 Assumed_Shape) {
   
                  PRINTMSG(IL_LINE_NUM(list_idx2), 1303, Error, 
                           IL_COL_NUM(list_idx2));
               }
               else {
                  curr_stmt_sh_idx = sh_after_entry_idx;
                  gen_assumed_shape_copy(&IL_OPND(list_idx2));
               }

               list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
      }

      shared_bd_idx = NULL_IDX;
   }
   else {  /* Module, blockdata or program */

      if (SCP_COPY_ASSUMED_LIST(curr_scp_idx) != NULL_IDX) {
         list_idx = SCP_COPY_ASSUMED_LIST(curr_scp_idx);

         while (list_idx) {
            COPY_OPND(opnd, IL_OPND(list_idx));
            list_idx2 = OPND_IDX(opnd);

            while (list_idx2) {

               if (AT_DCL_ERR(IL_IDX(list_idx2))) {
                  /* intentionally blank */
               }
               else {
                  PRINTMSG(IL_LINE_NUM(list_idx2), 1303, Error, 
                           IL_COL_NUM(list_idx2));
               }
               list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
      }
   }

# ifdef _F_MINUS_MINUS

# if ! defined(_TARGET_OS_MAX)

   if (cmd_line_flags.co_array_fortran &&
       ATP_PGM_UNIT(pgm_attr_idx) == Program) {
      /* insert call to start_pes(0) */
      save_curr_stmt_sh_idx = curr_stmt_sh_idx;
      curr_stmt_sh_idx = SCP_FIRST_SH_IDX(curr_scp_idx);

      OPND_FLD(opnd) = CN_Tbl_Idx;
      OPND_IDX(opnd) = CN_INTEGER_ZERO_IDX;
      OPND_LINE_NUM(opnd) = AT_DEF_LINE(pgm_attr_idx);
      OPND_COL_NUM(opnd) = AT_DEF_COLUMN(pgm_attr_idx);

      gen_internal_call_stmt(START_PES_LIB_ENTRY,
                             &opnd,
                             After);

      PRINTMSG(AT_DEF_LINE(pgm_attr_idx), 1460, Warning, 
               AT_DEF_COLUMN(pgm_attr_idx));
      
      curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   }
# endif
# endif

   if (SCP_DARG_LIST(curr_scp_idx) != NULL_IDX) {
      free_attr_list(SCP_DARG_LIST(curr_scp_idx));
      SCP_DARG_LIST(curr_scp_idx)	= NULL_IDX;
   }

   SCP_RESHAPE_ARRAY_LIST(curr_scp_idx)	= reshape_array_list;
   reshape_array_list			= NULL_IDX;

   TRACE (Func_Exit, "decl_semantics", NULL);

   return;

}  /* decl_semantics */

#ifdef KEY /* Bug 6845 */
/* Create symbol for "dealloc" the first time we need it */
static long
lazy_create_dealloc(int line, int col) {
   if (glb_tbl_idx[Dealloc_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Dealloc_Attr_Idx] = create_lib_entry_attr(DEALLOC_LIB_ENTRY,
	 DEALLOC_NAME_LEN, line, col);
   }
   return glb_tbl_idx[Dealloc_Attr_Idx];
}
/*
 * Generate a statement to deallocate a single simple entity. The code in this
 * function is modeled after deallocate_local_allocatables(), with
 * _SEPARATE_DEALLOCATES==true and _ALLOCATE_IS_CALL==false. It is generalized
 * slightly from the original to allow deallocation of a structure component.
 *
 * line		source line
 * col		source column
 * fld		AT_Tbl_Idx or IR_Tbl_Idx
 * idx		index of arg or variable to deallocate
 * has_pe_ref	used only for locals, not dummies
 * do_gen_sh	call gen_sh to generate statement to perform dealloc
 * optional	is optional dummy variable
 * return	sh_idx for newly generated statement
 */
int
help_dealloc(int line, int col, fld_type fld, int idx,
   boolean has_pe_ref, boolean do_gen_sh, boolean optional) {

   if (do_gen_sh) {
      gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(curr_stmt_sh_idx)   = TRUE;
   }

   int list_idx;
   NTR_IR_LIST_TBL(list_idx);
   IL_FLD(list_idx) = IR_Tbl_Idx;
   IL_IDX(list_idx) = gen_ir(fld, idx, Aloc_Opr, CRI_Ptr_8,
     line, col, NO_Tbl_Idx, NULL_IDX);

   int asg_idx = SH_IR_IDX(curr_stmt_sh_idx) = gen_ir(IL_Tbl_Idx, list_idx,
     Deallocate_Opr, TYPELESS_DEFAULT_TYPE, line, col, NO_Tbl_Idx, NULL_IDX);

   int cn_idx;
   IR_FLD_R(asg_idx) = IL_Tbl_Idx;
   IR_LIST_CNT_R(asg_idx) = 3;
   IR_IDX_R(asg_idx) = gen_il(3, FALSE, line, col, AT_Tbl_Idx,
      lazy_create_dealloc(line, col), CN_Tbl_Idx,
      gen_alloc_header_const(Integer_8, 1, has_pe_ref, &cn_idx), CN_Tbl_Idx,
      CN_INTEGER_ZERO_IDX);

   if (optional) {
     curr_stmt_sh_idx = gen_present_ir(idx, curr_stmt_sh_idx,
       curr_stmt_sh_idx);
   }

   return curr_stmt_sh_idx;
}
/*
 * line			source line
 * col			source column
 * attr_idx		idx for variable or expr of type structure
 * attr_fld		which table attr_idx applies to
 * cpnt_attr_idx	AT_Tbl_Idx for a component of that structure
 * return		IR_Tbl_Idx for Struct_Opr referring to component
 */
int
do_make_struct_opr(int line, int col, int attr_idx, fld_type attr_fld,
   int cpnt_attr_idx) {
   int cpnt_ir_idx;
   NTR_IR_TBL(cpnt_ir_idx);
   IR_OPR(cpnt_ir_idx) = Struct_Opr;
   IR_TYPE_IDX(cpnt_ir_idx) = ATD_TYPE_IDX(cpnt_attr_idx);
   IR_LINE_NUM(cpnt_ir_idx) = line;
   IR_COL_NUM(cpnt_ir_idx)  = col;
   IR_FLD_L(cpnt_ir_idx) = attr_fld;
   IR_IDX_L(cpnt_ir_idx) = attr_idx;
   IR_FLD_R(cpnt_ir_idx) = AT_Tbl_Idx;
   IR_IDX_R(cpnt_ir_idx) = cpnt_attr_idx;
   IR_LINE_NUM_L(cpnt_ir_idx) = IR_LINE_NUM_R(cpnt_ir_idx) = line;
   IR_COL_NUM_L(cpnt_ir_idx) = IR_COL_NUM_R(cpnt_ir_idx)  = col;
   return cpnt_ir_idx;
}

static void help_dealloc_components(int, int, fld_type, int, boolean,
  boolean *);
static void dealloc_allocatables(int, int, int, fld_type, int, boolean, boolean *);

/*
 * Loop through elements of a nonallocatable array whose element type is a
 * structure containing allocatable components or subcomponents, and deallocate
 * them.
 *
 * line		Source line
 * col		Source column
 * fld		AT_Tbl_Idx or IR_Tbl_Idx
 * idx		Index for variable or Struct_Opr whose allocatable components
 *		we want to deallocate
 * has_pe_ref	Who knows?
 * first	If null, create statement header for each deallocation.
 *		Otherwise, caller passes a variable which is used to suppress
 *		the first statement header (don't ask--it's historical.)
 */
static void
help_dealloc_array_of_struct(int line, int col, fld_type fld, int idx,
  boolean has_pe_ref, boolean *first) {
  opnd_type opnd;
  expr_arg_type exp_desc;
  int next_sh_idx = NULL_IDX;
  int placeholder_sh_idx = pre_gen_loops(line, col, &next_sh_idx);
  OPND_FLD(opnd) = fld;
  OPND_IDX(opnd) = idx;
  OPND_LINE_NUM(opnd) = line;
  OPND_COL_NUM(opnd) = col;
  gen_whole_subscript(&opnd, &exp_desc);
  gen_loops(&opnd, 0, TRUE);
  help_dealloc_components(line, col, OPND_FLD(opnd), OPND_IDX(opnd),
    has_pe_ref, first);
  post_gen_loops(placeholder_sh_idx, next_sh_idx);
}
/*
 * cpnt_attr_idx	AT_Tbl_Idx for a structure component
 * return true if the component is a structure containing allocatable
 *			components, which therefore requires automatic
 *			allocation and deallocation
 */
int
allocatable_structure_component(int cpnt_attr_idx) {
  if (ATD_POINTER(cpnt_attr_idx)) { /* Bug 14293 */
    return 0;
  }
  int type_idx = ATD_TYPE_IDX(cpnt_attr_idx);
  return Structure == TYP_TYPE(type_idx) &&
    ATT_ALLOCATABLE_CPNT(TYP_IDX(type_idx));
}
/*
 * Recursively deallocate allocatables associated with a variable or component
 * whose data type is "structure"
 * line		Source line
 * col		Source column
 * fld		AT_Tbl_Idx or IR_Tbl_Idx
 * idx		Index for structure variable or Struct_Opr
 *		whose components we need to deallocate
 * has_pe_ref	Who knows?
 * first	If null, create statement header for each deallocation.
 *		Otherwise, caller passes a variable which is used to suppress
 *		the first statement header (don't ask--it's historical.)
 */
static void
help_dealloc_components(int line, int col, fld_type fld, int idx,
   boolean has_pe_ref, boolean *first) {
   int struct_idx = (fld == IR_Tbl_Idx) ? IR_TYPE_IDX(idx) : ATD_TYPE_IDX(idx);
   for (int sn_idx = ATT_FIRST_CPNT_IDX(TYP_IDX(struct_idx));
      sn_idx != NULL_IDX;
      sn_idx = SN_SIBLING_LINK(sn_idx)) {
      int cpnt_attr_idx = SN_ATTR_IDX(sn_idx);
      int type_idx = ATD_TYPE_IDX(cpnt_attr_idx);

      if (ATD_ALLOCATABLE(cpnt_attr_idx) ||
        allocatable_structure_component(cpnt_attr_idx)) {
	dealloc_allocatables(line, col, cpnt_attr_idx, IR_Tbl_Idx,
	  do_make_struct_opr(line, col, idx, fld, cpnt_attr_idx), has_pe_ref,
	  first);
      }
   }
}
/*
 * Recursively deallocate allocatables associated with a variable or component
 *
 * line		source line
 * col		source column
 * attr_idx	AT_Tbl_Idx of variable or structure component 
 * fld		AT_Tbl_Idx or IR_Tbl_Idx corresponding to idx
 * idx		Index of variable or Struct_Opr expression which refers to
 *		the variable or component attr_idx
 * has_pe_ref	Who knows?
 * first	If null, create statement header for each deallocation.
 *		Otherwise, caller passes a variable which is used to suppress
 *		the first statement header (don't ask--it's historical.)
 */
static void
dealloc_allocatables(int line, int col, int attr_idx, fld_type fld, int idx,
  boolean has_pe_ref, boolean *first) {
  int type_idx = ATD_TYPE_IDX(attr_idx);

  /* Ordinary allocatable array */
  if (ATD_ALLOCATABLE(attr_idx)) {
    help_dealloc(line, col, fld, idx, has_pe_ref,
      first ? (!*first) : TRUE, AT_OPTIONAL(attr_idx));
    if (first) {
      *first = FALSE;
    }
  }

  else if (allocatable_structure_component(attr_idx)) {
    int line = SH_GLB_LINE(curr_stmt_sh_idx);
    int col = SH_COL_NUM(curr_stmt_sh_idx);

    /* Non-allocatable array of structure having allocatable components or
     * subcomponents. */
    if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
      help_dealloc_array_of_struct(line, col, fld, idx, has_pe_ref,
        first);
    }

    /* Scalar structure having allocatable components or subcomponents:
     * recursively deallocate. */
    else {
      help_dealloc_components(line, col, fld, idx, has_pe_ref, first);
    }
  }
}

#endif /* KEY Bug 6845 */
#ifdef KEY /* Bug 9029 */
/*
 * Generate a warning message if a variable in a "threadprivate" directive
 * violates the rules
 * attr_idx	AT_Tbl_Idx for the variable
 */
static void
threadprivate_check(int attr_idx) {
  /*
   * A threadprivate individual variable must not be:
   *   equivalenced, or
   *   in common (an entire common block an be theadprivate)
   * but must be:
   *   saved, or
   *   in a program unit where all vars are saved, or
   *   declared in a module
   */
  if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
     return; /* Just in case */
  }
  int sb_idx = ATD_STOR_BLK_IDX(attr_idx);
  char *msg_str = 0;
  if (sb_idx != NULL_IDX && SB_BLK_TYPE(sb_idx) == Threadprivate) {
    /* Would disallow common, but at this point we can't tell whether the
     * variable is marked "threadprivate" individually (ok) or by virtue of
     * its common block being marked "threadprivate" (also ok) or both at
     * once (not ok.) */
    if (ATD_IN_COMMON(attr_idx)) {
      return;
    }
    if (ATD_EQUIV(attr_idx)) {
      msg_str = "EQUIVALENCED";
    }
    if (msg_str) {
      PRINTMSG(AT_DEF_LINE(attr_idx), 1441, Error, AT_DEF_COLUMN(attr_idx),
	AT_OBJ_NAME_PTR(attr_idx),
	msg_str,
	"THREADPRIVATE",
	AT_DEF_LINE(attr_idx));
    }
    
    else {
      int scp_attr_idx = SCP_ATTR_IDX(curr_scp_idx);
      if (!(ATD_SAVED(attr_idx) ||
	ATP_SAVE_ALL(scp_attr_idx) ||
	(AT_OBJ_CLASS(scp_attr_idx) == Pgm_Unit &&
	  ATP_PGM_UNIT(scp_attr_idx) == Program) ||
	AT_MODULE_OBJECT(attr_idx))) {
	PRINTMSG(AT_DEF_LINE(attr_idx), 1687, Warning, AT_DEF_COLUMN(attr_idx),
	  AT_OBJ_NAME_PTR(attr_idx));
      }
    }
  }
}
#endif /* KEY Bug 9029 */
#ifdef KEY /* Bug 14255 */
/*
 * Given an attribute which has been marked as a dummy variable or procedure,
 * report an error if it isn't really a dummy argument
 *
 * attr_idx	AT_Tbl_Idx for suspect attribute
 */
static void
error_not_darg(int attr_idx) {
  char *problem = 0;
  int err_number = 352;
  if (AT_IS_DARG(attr_idx)) {
    return;
  }
  if (AT_OPTIONAL(attr_idx)) {
    problem = "OPTIONAL";
  }
  else if (ATD_VALUE_ATTR(attr_idx)) {
    problem = "VALUE";
  }
  else if (ATD_INTENT(attr_idx) > Intent_Unseen) {
    problem = "INTENT";
  }
  else if (ATD_IGNORE_TKR(attr_idx)) {
    problem = "IGNORE_TKR";
    err_number = 1505;
  }
  AT_DCL_ERR(attr_idx) = TRUE;
  PRINTMSG(AT_DEF_LINE(attr_idx), err_number, Error,
	   AT_DEF_COLUMN(attr_idx),
	   AT_OBJ_NAME_PTR(attr_idx), problem);
}
#endif /* KEY Bug 14255 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	attr_semantics calls itself recursively to find all attr              *|
|*	dependencies.  Then it does all the semantic checking it can think of.*|
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
static	void	attr_semantics(int	attr_idx,
				boolean	bound_attr)

{
   int			al_idx;
   int			bd_idx;
   int			column;
   int			count;
   int			curr_fwd_ref_idx;
   int			darg_idx;
   int			dim;
   int			dt_idx;
   int			end_entry_sh_idx;
   int			entry_sh_idx;
   int			eq_idx;
   expr_arg_type	expr_desc;
   int			first_idx;
   int			i;
   int			ir_idx;
   boolean		is_interface;
   int			line;
   int			link_idx;
   int			name_idx;
   int			new_bd_idx;
   int			old_fwd_ref_idx;
   opnd_type		opnd;
   int			pgm_attr_idx;
   int			pgm_idx;
   int			pointer_idx;
   int			proc_idx;
   char		       *pure_str;
#ifdef KEY /* Bug 10177 */
   int			rslt_idx = 0;
#else /* KEY Bug 10177 */
   int			rslt_idx;
#endif /* KEY Bug 10177 */
   int			scp_idx;
   int			sf_attr_idx;
   int			sn_attr_idx;
   int			sn_idx;
   id_str_type		storage_name;
   int			tmp_ir_idx;
#ifdef KEY /* Bug 10177 */
   int			type_idx = 0;
#else /* KEY Bug 10177 */
   int			type_idx;
#endif /* KEY Bug 10177 */
   boolean		type_resolved;
   size_offset_type     storage_size;

# if defined(_TARGET_OS_MAX) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int			tmp_idx;
# endif


   TRACE (Func_Entry, "attr_semantics", NULL);

   is_interface	= SCP_IS_INTERFACE(curr_scp_idx);

   if (AT_SEMANTICS_DONE(attr_idx) || 
       AT_DCL_ERR(attr_idx) ||
       AT_ATTR_LINK(attr_idx) != NULL_IDX) {
      AT_SEMANTICS_DONE(attr_idx) = TRUE;

      if (AT_OBJ_CLASS(attr_idx) != Interface ||
          AT_DCL_ERR(attr_idx) ||
          AT_ATTR_LINK(attr_idx) == NULL_IDX) {

         if (is_interface) {

            switch(AT_OBJ_CLASS(attr_idx)) {
            case Pgm_Unit:
               ATP_SCP_IDX(attr_idx)	= SCP_PARENT_IDX(curr_scp_idx);
               break;

            case Derived_Type:
               ATT_SCP_IDX(attr_idx)	= SCP_PARENT_IDX(curr_scp_idx);
               break;
            }
         }

         return;
      }
   }

   pgm_attr_idx	= SCP_ATTR_IDX(curr_scp_idx);

   /* Mark this flag TRUE, for all objects declared in the module, if */
   /* this is a module.  The purpose of this flag is to separate      */
   /* objects from any module procedures from the objects in the      */
   /* module itself.  The classic case is the same named derived type */
   /* declared in the module and the module procedure.  The module    */
   /* procedure is of this type.  Because we match on AT_MODULE_IDX   */
   /* and AT_USE_ASSOCIATED in resolve_attr during use processing, we */
   /* have no way of knowing that these two types are not the same.   */
   /* This flag will differentiate between them, because only the     */
   /* module procedure name itself will come through this routine     */
   /* when pgm_attr_idx is set to the module.  AT_MODULE_IDX and      */
   /* AT_USE_ASSOCIATED cannot be used to determine this, because     */
   /* they are set for everything coming out of a module at USE time. */

   if (ATP_PGM_UNIT(pgm_attr_idx) == Module && !AT_USE_ASSOCIATED(attr_idx)) {
      AT_MODULE_OBJECT(attr_idx) = TRUE;
   }

   switch(AT_OBJ_CLASS(attr_idx)) {
   case Data_Obj:

      switch (ATD_CLASS(attr_idx)) {
      case Atd_Unknown:

         /* All data objs that do not resolve to something else are variables.*/

         ATD_CLASS(attr_idx) = Variable;
         break;

      case Function_Result:

         /* These are done when the pgm_unit is processed, */
         /* so process the program unit now.               */

         attr_semantics(ATD_FUNC_IDX(attr_idx), FALSE);

         return;

      case Compiler_Tmp:

         if (AT_REFERENCED(attr_idx) == Not_Referenced) {

            /* LRR - You're going to get more than bound attrs here. */

            /* Bound tmp saved just for CIF - These are bound_attrs, but  */
            /* only process them if CIF XREFS is on.                      */

            if ((cif_flags & XREF_RECS) != 0) {
               bound_attr = TRUE;
            }
            else {
               goto EXIT;
            }
         }
         break;

      case Constant:

         if (ATP_PGM_UNIT(pgm_attr_idx) == Module &&
             ATD_FLD(attr_idx) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(ATD_CONST_IDX(attr_idx)) == Data_Obj &&
             ATD_CLASS(ATD_CONST_IDX(attr_idx)) == Compiler_Tmp &&
             ATD_TMP_INIT_NOT_DONE(ATD_CONST_IDX(attr_idx))) {

            /* Do all the init stmts for module parameters */

            insert_init_stmt_for_tmp(ATD_CONST_IDX(attr_idx));
         }
         break;

      }  /* End switch */

      type_idx = ATD_TYPE_IDX(attr_idx);

      if (TYP_TYPE(type_idx) == Structure) {
          
         if (AT_ATTR_LINK(TYP_IDX(type_idx)) != NULL_IDX) {

            /* If this derived type is host associated (AT_ATTR_LINK is set)  */
            /* change the type table to point to the original type.  It is    */
            /* okay to change the type table, because every attr of this type */
            /* needs to do this.                                              */

            link_idx = TYP_IDX(type_idx);

            while (AT_ATTR_LINK(link_idx) != NULL_IDX) {
               link_idx = AT_ATTR_LINK(link_idx);
            }

            TYP_IDX(type_idx) = link_idx;
         }

         attr_semantics(TYP_IDX(type_idx), FALSE);
      }

      if (ATP_PGM_UNIT(pgm_attr_idx) == Module && 
          ATD_CLASS(attr_idx) != Struct_Component) {

         if (TYP_TYPE(type_idx) == Structure &&
             !AT_PRIVATE(attr_idx) &&
             AT_PRIVATE(TYP_IDX(type_idx)) &&
             !AT_USE_ASSOCIATED(TYP_IDX(type_idx))) {  /* Interp 161 */
            PRINTMSG(AT_DEF_LINE(attr_idx), 598, Error, 
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
         }

         if (ATD_CLASS(attr_idx) == CRI__Pointee) {
            attr_semantics(ATD_PTR_IDX(attr_idx), FALSE);

            if (AT_PRIVATE(attr_idx) != AT_PRIVATE(ATD_PTR_IDX(attr_idx))) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 697, Error, 
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(ATD_PTR_IDX(attr_idx)),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
         }
      }

      if (ATP_PURE(pgm_attr_idx) || ATP_ELEMENTAL(pgm_attr_idx)) {

         if (ATD_IN_COMMON(attr_idx) ||
             AT_USE_ASSOCIATED(attr_idx) ||
             AT_HOST_ASSOCIATED(attr_idx) ||
             (ATD_CLASS(attr_idx) == Dummy_Argument &&
              (ATP_PGM_UNIT(pgm_attr_idx) == Function ||
                (ATP_PGM_UNIT(pgm_attr_idx) == Subroutine &&
                 ATD_INTENT(attr_idx) == Intent_In)))) {

            /* Mark this, so that this object does not get defined. */

            ATD_PURE(attr_idx)	= TRUE;
         }

         if (ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX) {
            PRINTMSG(BD_LINE_NUM(ATD_PE_ARRAY_IDX(attr_idx)), 1580, Error, 
                     BD_COLUMN_NUM(ATD_PE_ARRAY_IDX(attr_idx)),
                     AT_OBJ_NAME_PTR(pgm_attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
      } 

      if (AT_USE_ASSOCIATED(attr_idx)) {
         goto EXIT;
      }

      if (bound_attr && ATD_CLASS(attr_idx) == Compiler_Tmp) {

         if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {
            attr_semantics(ATD_TMP_IDX(attr_idx), FALSE);
         }
         else if (ATD_FLD(attr_idx) == IR_Tbl_Idx) {
            ir_idx = ATD_TMP_IDX(attr_idx);

            switch (IR_FLD_R(ir_idx)) {
            case AT_Tbl_Idx:
               attr_semantics(IR_IDX_R(ir_idx), FALSE);
               break;

            case IR_Tbl_Idx:
               tmp_ir_resolution(IR_IDX_R(ir_idx));
               break;

            case IL_Tbl_Idx:
               tmp_il_resolution(IR_IDX_R(ir_idx));
               break;
            }
         }
   
#ifdef KEY /* Bug 10675 */
	 /*
	  * A fundamental flaw in this front end is that when it generates a
	  * statement during the semantics phase and inserts it in the series
	  * of statements generated by the parse phase, there's no way to
	  * "glue together" the original statement and its progeny so they
	  * act like a single statement. This is a problem when a compiler
	  * directive like "omp atomic" refers to "the next statement" (because
	  * post-semantics-phase it needs to refer to a block of statements.)
	  * And it's a problem here: if bound_resolution() processes a
	  * declaration which calls a procedure whose arglist requires the
	  * allocation of a temporary, then it returns with an "alloc"
	  * statement prepended to the "call" statement and a "dealloc"
	  * statement appended. But the "call" statement is still marked as
	  * the current one. If attr_semantics() now decides to allocate a
	  * temporary here and deallocate it in the epilog, it will
	  * append the "alloc" to the current statement. But that will
	  * actually insert the "alloc" inside the trio of statements which
	  * represents the call, in front of the "dealloc" belonging to the
	  * call. Since "alloc" and "dealloc" need to be nested properly
	  * (they work by moving the stack pointer) this is a tragedy. The
	  * comprehensive fix would be to invent something in the front end
	  * IR to represent a "block of statements which act like a single
	  * one"; the quick fix is to move the current statement pointer past
	  * the end of that block.
	  */
	 int save_next_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
         bound_resolution(attr_idx);
	 for (;
	   curr_stmt_sh_idx != NULL_IDX &&
	     SH_NEXT_IDX(curr_stmt_sh_idx) != save_next_stmt_sh_idx &&
	     SH_P2_SKIP_ME(curr_stmt_sh_idx);
	     curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx))
	   ;
#else /* KEY Bug 10675 */
         bound_resolution(attr_idx);
#endif /* KEY Bug 10675 */
      }


      if (TYP_TYPE(type_idx) == Character) {

         if (TYP_FLD(type_idx) == AT_Tbl_Idx) {
            attr_semantics(TYP_IDX(type_idx), TRUE);
         }
      }

      bd_idx	= ATD_ARRAY_IDX(attr_idx);

      if (bd_idx != NULL_IDX && BD_ARRAY_CLASS(bd_idx) != Deferred_Shape) {

         for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

            if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
               attr_semantics(BD_LB_IDX(bd_idx, dim), TRUE);
            }

            if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
               attr_semantics(BD_UB_IDX(bd_idx, dim), TRUE);
            }
         }
      }

      bd_idx    = ATD_PE_ARRAY_IDX(attr_idx);

      if (bd_idx != NULL_IDX && BD_ARRAY_CLASS(bd_idx) != Deferred_Shape) {


         for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

            if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
               attr_semantics(BD_LB_IDX(bd_idx, dim), TRUE);
            }

            if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
               attr_semantics(BD_UB_IDX(bd_idx, dim), TRUE);
            }
         }
      }


      if (!AT_TYPED(attr_idx)) { 

         if (SCP_IMPL_NONE(curr_scp_idx)) {
            AT_DCL_ERR(attr_idx) = TRUE;
            PRINTMSG(AT_DEF_LINE(attr_idx), 113, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
         else if (!IM_SET(curr_scp_idx, IMPL_IDX(AT_OBJ_NAME(attr_idx)))) {

            if (SCP_PARENT_NONE(curr_scp_idx)) {
               AT_DCL_ERR(attr_idx) = TRUE;
               PRINTMSG(AT_DEF_LINE(attr_idx), 297, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
            else if (on_off_flags.implicit_none) {
               AT_DCL_ERR(attr_idx) = TRUE;
               PRINTMSG(AT_DEF_LINE(attr_idx), 1171, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
         }
      }

      /* char_len_resolution MUST happen before array_dim_resolution  */
      /* because the character length is used to calculate the stride */
      /* multiplier stored in the bounds table array entry.           */

      if (TYP_TYPE(type_idx) == Character) { 
         char_len_resolution(attr_idx, FALSE);

         /* reset the type_idx in case it changes */

         type_idx = ATD_TYPE_IDX(attr_idx);
      }

      if (AT_DCL_ERR(attr_idx)) {
         goto EXIT;
      }

      if (ATD_ALLOCATABLE(attr_idx)) {
         ATD_IM_A_DOPE(attr_idx) = TRUE;

         if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX ||
             BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) != Deferred_Shape) {
            AT_DCL_ERR(attr_idx) = TRUE;
            PRINTMSG(AT_DEF_LINE(attr_idx), 570, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
         }

# ifdef _F_MINUS_MINUS
         if (ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX &&
             BD_ARRAY_CLASS(ATD_PE_ARRAY_IDX(attr_idx)) != Deferred_Shape) {
            AT_DCL_ERR(attr_idx) = TRUE;
            PRINTMSG(AT_DEF_LINE(attr_idx), 1552, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
# endif
      }

      if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {

         /* If -O fld is set and this is an explicit shape (rank > 1)   */
         /* array that has not been specified in a -O fld=array_name    */
         /* option, then set ATD_RESHAPE_ARRAY_OPT to TRUE.            */

         if (opt_flags.reshape_all_arrays &&
             BD_RANK(ATD_ARRAY_IDX(attr_idx)) > 1 &&
             BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Explicit_Shape &&
             (ATD_CLASS(attr_idx) != CRI__Pointee &&
              ATD_CLASS(attr_idx) != Constant) &&
             BD_LB_FLD(ATD_ARRAY_IDX(attr_idx), 
                       BD_RANK(ATD_ARRAY_IDX(attr_idx))) == CN_Tbl_Idx &&
             compare_cn_and_value(BD_LB_IDX(ATD_ARRAY_IDX(attr_idx), 
                                            BD_RANK(ATD_ARRAY_IDX(attr_idx))),
                                  1,
                                  Eq_Opr) &&
             BD_UB_FLD(ATD_ARRAY_IDX(attr_idx), 
                       BD_RANK(ATD_ARRAY_IDX(attr_idx))) == CN_Tbl_Idx &&
             compare_cn_and_value(BD_UB_IDX(ATD_ARRAY_IDX(attr_idx), 
                                            BD_RANK(ATD_ARRAY_IDX(attr_idx))),
                                  16,
                                  Lt_Opr) &&
             !ATD_RESHAPE_ARRAY_OPT(attr_idx)) {

            if (ATD_DATA_INIT(attr_idx)) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1644, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
            ATD_RESHAPE_ARRAY_OPT(attr_idx) = TRUE;
            NTR_ATTR_LIST_TBL(al_idx);
            AL_ATTR_IDX(al_idx) = attr_idx;
            AL_NEXT_IDX(al_idx) = reshape_array_list;
            reshape_array_list  = al_idx;
         }

         if (ATD_RESHAPE_ARRAY_OPT(attr_idx)) {

            PRINTMSG(AT_DEF_LINE(attr_idx), 1637, Optimization, 0,
                     "-O reshape",
                     AT_OBJ_NAME_PTR(attr_idx));

            /* create the new bounds entry with the swapped dimensions */

            bd_idx = ATD_ARRAY_IDX(attr_idx);

            new_bd_idx                 = reserve_array_ntry(BD_RANK(bd_idx));
            BD_RANK(new_bd_idx)        = BD_RANK(bd_idx);
            BD_LINE_NUM(new_bd_idx)    = BD_LINE_NUM(bd_idx);
            BD_COLUMN_NUM(new_bd_idx)  = BD_COLUMN_NUM(bd_idx);
            BD_ARRAY_CLASS(new_bd_idx) = BD_ARRAY_CLASS(bd_idx);
            BD_RESOLVED(new_bd_idx)    = FALSE;

            dim = 1;

            BD_LB_FLD(new_bd_idx,dim) = BD_LB_FLD(bd_idx,BD_RANK(bd_idx));
            BD_LB_IDX(new_bd_idx,dim) = BD_LB_IDX(bd_idx,BD_RANK(bd_idx));

            BD_UB_FLD(new_bd_idx,dim) = BD_UB_FLD(bd_idx,BD_RANK(bd_idx));
            BD_UB_IDX(new_bd_idx,dim) = BD_UB_IDX(bd_idx,BD_RANK(bd_idx));

            for (i = 1; i < BD_RANK(bd_idx); i++) {
               dim++;
               BD_LB_FLD(new_bd_idx,dim) = BD_LB_FLD(bd_idx,i);
               BD_LB_IDX(new_bd_idx,dim) = BD_LB_IDX(bd_idx,i);

               BD_UB_FLD(new_bd_idx,dim) = BD_UB_FLD(bd_idx,i);
               BD_UB_IDX(new_bd_idx,dim) = BD_UB_IDX(bd_idx,i);
            }

            new_bd_idx =  ntr_array_in_bd_tbl(new_bd_idx);

            array_dim_resolution(attr_idx, FALSE);
            bd_idx = ATD_ARRAY_IDX(attr_idx);

            if (! AT_DCL_ERR(attr_idx) &&
                ! BD_DCL_ERR(bd_idx)) {

               ATD_ARRAY_IDX(attr_idx) = new_bd_idx;
               array_dim_resolution(attr_idx, FALSE);
               ATD_RESHAPE_ARRAY_IDX(attr_idx) = ATD_ARRAY_IDX(attr_idx);
               ATD_ARRAY_IDX(attr_idx) = bd_idx;
            }
         }
         else {
            array_dim_resolution(attr_idx, FALSE);
         }
      }

      if (ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX) {
         pe_array_dim_resolution(attr_idx);
      }

      if (ATD_DISTRIBUTION_IDX(attr_idx) != NULL_IDX) {
         distribution_resolution(attr_idx);
      }

      if (ATD_POINTER(attr_idx) && ATD_CLASS(attr_idx) != Dummy_Argument) {
         ATD_IM_A_DOPE(attr_idx)	= TRUE;
      }


      if (ATD_AUTOMATIC(attr_idx)) {

         if (ATD_IM_A_DOPE(attr_idx)) { /* If defrd array, its not auto */
            ATD_NO_ENTRY_LIST(attr_idx)	= NULL_IDX; /* Only good for autos */
            ATD_AUTOMATIC(attr_idx)	= FALSE;
         }
         else if (!is_interface) {

            if (ATP_SYMMETRIC(pgm_attr_idx)) {

               /* Check to see if this can be switched to symmetric. */
               /* The only thing AUTOMATIC can be, that SYMMETRIC    */
               /* cannot be is TARGET, so check that.                */

               if (ATD_TARGET(attr_idx)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1234, Caution,
                           AT_DEF_COLUMN(attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "TARGET");
               }
               else {
                  ATD_SYMMETRIC(attr_idx)	= TRUE;
               }
            }

# if defined(_SINGLE_ALLOCS_FOR_AUTOMATIC)
            gen_single_automatic_allocate(attr_idx);
# else

            if (TYP_TYPE(type_idx) == Character ||
                (TYP_TYPE(type_idx) == Structure && 
                 ATT_CHAR_SEQ(TYP_IDX(type_idx)))) {
               gen_single_automatic_allocate(attr_idx);
            }
            else {
               gen_multiple_automatic_allocate(attr_idx);
            }
# endif
         }
      }

      if (TYP_TYPE(type_idx) == Character &&
          ATD_CLASS(attr_idx) != CRI__Pointee &&
          TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char &&
          TYP_FLD(type_idx) == AT_Tbl_Idx &&
          AT_OBJ_CLASS(TYP_IDX(type_idx)) == Data_Obj) {
          
         tmp_ir_idx = ATD_TMP_IDX(TYP_IDX(type_idx));

         COPY_OPND(opnd, IR_OPND_R(tmp_ir_idx));
         fold_clen_opr(&opnd, &expr_desc);
         COPY_OPND(IR_OPND_R(tmp_ir_idx), opnd);
      }

      if (TYP_TYPE(type_idx) == Structure &&
          ATT_DEFAULT_INITIALIZED(TYP_IDX(type_idx))) {

         if (ATD_IN_COMMON(attr_idx)) {
            AT_DCL_ERR(attr_idx)        = TRUE;
            PRINTMSG(AT_DEF_LINE(attr_idx), 1600, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     AT_OBJ_NAME_PTR(TYP_IDX(ATD_TYPE_IDX(attr_idx))));
         }
         else if (ATD_CLASS(attr_idx) == CRI__Pointee) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 1647, Warning,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
      }

#ifdef KEY /* Bug 6845 */
      /* Dummy intent(out) must be deallocated on entry if it's an allocatable
       * array or a struct with allocatable components */
      if (Dummy_Argument == ATD_CLASS(attr_idx)) {
        if ((!is_interface) && Intent_Out == ATD_INTENT(attr_idx)) {
	  dealloc_allocatables(SH_GLB_LINE(curr_stmt_sh_idx),
	    SH_COL_NUM(curr_stmt_sh_idx), attr_idx, AT_Tbl_Idx, attr_idx,
	    FALSE, 0);
        }
	/* Fortran 90 required a constraint warning for this */
	if (ATD_ALLOCATABLE(attr_idx)) {
	  PRINTMSG(AT_DEF_LINE(attr_idx), 1679, Ansi, AT_DEF_COLUMN(attr_idx));
	}
      }
#endif /* KEY Bug 6845 */


#   ifdef KEY /* Bug 431, (1046, 1289, 8717) */
	   /* It appears that before the code arrived at Pathscale, in the
	    * process of removing the "ATD_TARGET" test, somebody messed up
	    * the grouping near the end of this impressive boolean expression,
	    * causing bug 431. If the variable was declared with an
	    * initializer (ATD_DATA_INIT) using a structure type which itself
	    * has a default init (ATT_DEFAULT_INITIALIZED), we don't want to
	    * call gen_entry_dope_code() to emit the initialization dictated
	    * by the structure type, because the variable initialization
	    * (which is emitted elsewhere) overrides it.
	    *
	    * At Pathscale, the first (and second and third) attempts to fix
	    * bug 431 focused on suppressing the resultant WHIRL "store" via
	    * code in fei_store(), but that caused bug 8717, because in other
	    * circumstances the "store" is needed. Better not to emit the
	    * unwanted type-initializer assignment into the IR in the first
	    * place.
	    */
      if (!is_interface &&

          (ATD_IM_A_DOPE(attr_idx) &&
           (ATD_CLASS(attr_idx) != Dummy_Argument ||
            (ATD_ARRAY_IDX(attr_idx) &&
             BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Assumed_Shape))) ||

           /* Follows is the default init check */

          (TYP_TYPE(type_idx) == Structure &&
           ATD_CLASS(attr_idx) != Constant &&
           (ATD_CLASS(attr_idx) != Dummy_Argument ||
              ATD_INTENT(attr_idx) == Intent_Out) &&
            ATD_CLASS(attr_idx) != CRI__Pointee &&
           ((ATT_POINTER_CPNT(TYP_IDX(type_idx)) ||
#ifdef KEY /* Bug 6845 */
           ATT_ALLOCATABLE_CPNT(TYP_IDX(type_idx)) ||
#endif /* KEY Bug 6845 */
             ATT_DEFAULT_INITIALIZED(TYP_IDX(type_idx))) &&
            !ATD_DATA_INIT(attr_idx))))
#   else /* KEY Bug 431, (1046, 1289, 8717) */
      if (!is_interface &&

          (ATD_IM_A_DOPE(attr_idx) &&
           (ATD_CLASS(attr_idx) != Dummy_Argument ||
            (ATD_ARRAY_IDX(attr_idx) &&
             BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Assumed_Shape))) ||

           /* Follows is the default init check */

          (TYP_TYPE(type_idx) == Structure &&
           ATD_CLASS(attr_idx) != Constant &&
           (ATD_CLASS(attr_idx) != Dummy_Argument ||
              ATD_INTENT(attr_idx) == Intent_Out) &&
            ATD_CLASS(attr_idx) != CRI__Pointee &&
           (ATT_POINTER_CPNT(TYP_IDX(type_idx)) ||
            (ATT_DEFAULT_INITIALIZED(TYP_IDX(type_idx)) &&
            !ATD_DATA_INIT(attr_idx)))))
#   endif /* KEY Bug 431, (1046, 1289, 8717) */
      {

         entry_sh_idx		= curr_stmt_sh_idx;
         end_entry_sh_idx	= SH_NEXT_IDX(curr_stmt_sh_idx);

         if (ATD_IM_A_DOPE(attr_idx)                                   &&
             ATD_CLASS(attr_idx)                     == Dummy_Argument &&
             ATD_ARRAY_IDX(attr_idx)                                   &&
             BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Assumed_Shape) {

            /* Fill in the lower bound of Assumed Shape dummy arg here */
            /* TARGET will go here also */

            for (i = 1; i <= BD_RANK(ATD_ARRAY_IDX(attr_idx)); i++) {

               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)      = Dv_Set_Low_Bound;
               IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx) = SH_GLB_LINE(curr_stmt_sh_idx);
               IR_COL_NUM(ir_idx)  = SH_COL_NUM(curr_stmt_sh_idx);
               IR_FLD_L(ir_idx)    = AT_Tbl_Idx;
               IR_IDX_L(ir_idx)    = attr_idx;
               IR_LINE_NUM_L(ir_idx) = SH_GLB_LINE(curr_stmt_sh_idx);
               IR_COL_NUM_L(ir_idx)  = SH_COL_NUM(curr_stmt_sh_idx);

               IR_FLD_R(ir_idx) = BD_LB_FLD(ATD_ARRAY_IDX(attr_idx), i);
               IR_IDX_R(ir_idx) = BD_LB_IDX(ATD_ARRAY_IDX(attr_idx), i);
               IR_LINE_NUM_R(ir_idx) = SH_GLB_LINE(curr_stmt_sh_idx);
               IR_COL_NUM_R(ir_idx)  = SH_COL_NUM(curr_stmt_sh_idx);

               IR_DV_DIM(ir_idx) = i;

               gen_sh(After, Assignment_Stmt, SH_GLB_LINE(curr_stmt_sh_idx), 
                      SH_COL_NUM(curr_stmt_sh_idx), FALSE, FALSE, TRUE);

               SH_IR_IDX(curr_stmt_sh_idx)    = ir_idx;
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
            }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

# endif /* (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)) */
         }
         else if (ATP_PGM_UNIT(pgm_attr_idx) != Blockdata &&
                  (ATD_CLASS(attr_idx) != Dummy_Argument ||
                   (ATD_INTENT(attr_idx) == Intent_Out &&
                    ATT_DEFAULT_INITIALIZED(TYP_IDX(type_idx))))) {

            /* Do not generate entry code for block data program units.  */
            /* It is meaningless and PVP codegen blows up.               */

            gen_entry_dope_code(attr_idx);
         }

         if (end_entry_sh_idx == NULL_IDX) {

            /* find the end of the gen'd stmts */

            end_entry_sh_idx = entry_sh_idx;

            while (SH_NEXT_IDX(end_entry_sh_idx) != NULL_IDX) {
               end_entry_sh_idx = SH_NEXT_IDX(end_entry_sh_idx);
            }
         }
         else {
            end_entry_sh_idx = SH_PREV_IDX(end_entry_sh_idx);
         }

         if (ATD_AUTOMATIC(attr_idx)) {

            /* reset the curr_stmt_sh_idx if automatic, to get order right */

            curr_stmt_sh_idx = entry_sh_idx;
         }

         if (
#ifdef KEY /* Bug 6845, 10835 */
	     /* Allocatable array */
	     (ATD_ALLOCATABLE(attr_idx) ||
	       /* Non-pointer structure with allocatable component(s) or
		* subcomponent(s) */
	       (Structure == TYP_TYPE(ATD_TYPE_IDX(attr_idx)) &&
		 ATT_ALLOCATABLE_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx))) &&
		 !ATD_POINTER(attr_idx))) &&
             /* Allocatable dummy must not be deallocated on exit */
	     ATD_CLASS(attr_idx) != Dummy_Argument &&
#else /* KEY Bug 6845, 10835 */
	 ATD_ALLOCATABLE(attr_idx)            &&
#endif /* KEY Bug 6845, 10835 */
             ATP_PGM_UNIT(pgm_attr_idx) != Module &&
             ! ATP_SAVE_ALL(pgm_attr_idx)         &&
             ! ATD_DATA_INIT(attr_idx)            &&
             ! ATD_SAVED(attr_idx))               {

            NTR_SN_TBL(sn_idx);

            SN_SIBLING_LINK(sn_idx)   = allocatable_list_idx;
            allocatable_list_idx      = sn_idx;
            SN_ATTR_IDX(sn_idx)       = attr_idx;
            number_of_allocatables++;
         }

         insert_sh_after_entries(attr_idx, 
                                 entry_sh_idx,
                                 end_entry_sh_idx,
                                 FALSE,   /* Don't generate tmp = 0  */
                                 (ATD_AUTOMATIC(attr_idx) ? FALSE : TRUE));

      }

      if (ATD_AUXILIARY(attr_idx)) {

         if (ATP_PGM_UNIT(pgm_attr_idx) == Module && !ATD_IN_COMMON(attr_idx)) {

            /* Cray is not allowing non-COMMON AUXILIARY data in a MODULE blk */

            PRINTMSG(AT_DEF_LINE(attr_idx), 876, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx)	= TRUE;
         }
         else if (TYP_TYPE(type_idx) == Character) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 535, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx)	= TRUE;
         }
         else if (TYP_TYPE(type_idx) == Structure &&
                  (ATT_POINTER_CPNT(TYP_IDX(type_idx)) ||
#ifdef KEY /* Bug 6845 */
                  ATT_ALLOCATABLE_CPNT(TYP_IDX(type_idx)) ||
#endif /* KEY Bug 6845 */
                   ATT_CHAR_CPNT(TYP_IDX(type_idx))) ) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 536, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
            AT_DCL_ERR(attr_idx)	= TRUE;
         }
      }

      if (ATD_PERMUTATION(attr_idx)) {  /* Must be integer array. */

         if (ATD_ARRAY_IDX(attr_idx) == NULL_IDX ||
             TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Integer) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 1126, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx)	= TRUE;
         }
      }

      switch (ATD_CLASS(attr_idx)) {
      case Variable:

#ifdef KEY /* Bug 9029 */
         threadprivate_check(attr_idx);
#endif /* KEY Bug 9029 */
         if (ATD_EQUIV(attr_idx) &&
             AL_NEXT_IDX(ATD_EQUIV_LIST(attr_idx)) == NULL_IDX) {

            /* Only one item on list so, clear it for faster equiv processing */

            ATD_EQUIV_LIST(attr_idx) = NULL_IDX;
         }

         /* Intentional fall through */

      case Compiler_Tmp:

         if (ATD_IN_COMMON(attr_idx)) {

            if (TYP_TYPE(type_idx) == Structure &&
#ifdef KEY /* Bug 14150 */
                !(ATT_SEQUENCE_SET(TYP_IDX(type_idx)) ||
		 AT_BIND_ATTR(TYP_IDX(type_idx)))
#else /* KEY Bug 14150 */
                !ATT_SEQUENCE_SET(TYP_IDX(type_idx))
#endif /* KEY Bug 14150 */
		) {
                AT_DCL_ERR(attr_idx) = TRUE;
                PRINTMSG(AT_DEF_LINE(attr_idx), 373, Error,
                         AT_DEF_COLUMN(attr_idx),
                         AT_OBJ_NAME_PTR(attr_idx),
                         AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
            }
#ifdef KEY /* Bug 6845 */
            if (TYP_TYPE(type_idx) == Structure &&
                ATT_ALLOCATABLE_CPNT(TYP_IDX(type_idx))) {
                AT_DCL_ERR(attr_idx) = TRUE;
                PRINTMSG(AT_DEF_LINE(attr_idx), 691, Error,
                         AT_DEF_COLUMN(attr_idx),
                         AT_OBJ_NAME_PTR(attr_idx));
	    }
#endif /* KEY Bug 6845 */

            if (SB_BLK_HAS_NPES(ATD_STOR_BLK_IDX(attr_idx)) &&
                ATD_DATA_INIT(attr_idx)) {
                PRINTMSG(AT_DEF_LINE(attr_idx), 1227, Error, 
                         AT_DEF_COLUMN(attr_idx),
                         AT_OBJ_NAME_PTR(attr_idx),
                         SB_BLANK_COMMON(ATD_STOR_BLK_IDX(attr_idx)) ?
                         "" : SB_NAME_PTR(ATD_STOR_BLK_IDX(attr_idx)));
                AT_DCL_ERR(attr_idx) = TRUE;
            }
         }
         else {

            if (ATD_SYMMETRIC(attr_idx)) {

               if (AT_HOST_ASSOCIATED(attr_idx)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1235, Error,
                           AT_DEF_COLUMN(attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx));

                  ATD_SYMMETRIC(attr_idx)   = FALSE;
               }
            }
            else if (ATP_SYMMETRIC(pgm_attr_idx)) {

               /* Check to see if this item should be switched to symmetric */

               if (fnd_semantic_err(Obj_Symmetric,
                                    AT_DEF_LINE(attr_idx),
                                    AT_DEF_COLUMN(attr_idx),
                                    attr_idx,
                                    FALSE)) {

                   /* Blank until caution messages can be issued. */

                  if (AT_HOST_ASSOCIATED(attr_idx)) {
                     PRINTMSG(AT_DEF_LINE(attr_idx), 1236, Caution,
                              AT_DEF_COLUMN(attr_idx),
                              AT_OBJ_NAME_PTR(attr_idx));
                  }
                  else {

                     if (ATD_TARGET(attr_idx)) {
                        PRINTMSG(AT_DEF_LINE(attr_idx), 1234, Caution,
                                 AT_DEF_COLUMN(attr_idx),
                                 AT_OBJ_NAME_PTR(attr_idx),
                                 "TARGET");
                     }
                     else if (ATD_DATA_INIT(attr_idx)) {
                        PRINTMSG(AT_DEF_LINE(attr_idx), 1234, Caution,
                                 AT_DEF_COLUMN(attr_idx),
                                 AT_OBJ_NAME_PTR(attr_idx),
                                 "DATA initialized");
                     }
                     else if (ATD_SAVED(attr_idx)) {
                        PRINTMSG(AT_DEF_LINE(attr_idx), 1234, Caution,
                                 AT_DEF_COLUMN(attr_idx),
                                 AT_OBJ_NAME_PTR(attr_idx),
                                 "SAVE");
                     }
                     else if (ATD_POINTER(attr_idx)) {
                        PRINTMSG(AT_DEF_LINE(attr_idx), 1234, Caution,
                                 AT_DEF_COLUMN(attr_idx),
                                 AT_OBJ_NAME_PTR(attr_idx),
                                 "POINTER");
                     }
                     else if (ATD_EQUIV(attr_idx)) {
                        PRINTMSG(AT_DEF_LINE(attr_idx), 1234, Caution,
                                 AT_DEF_COLUMN(attr_idx),
                                 AT_OBJ_NAME_PTR(attr_idx),
                                 "EQUIVALENCE");
                     }
                     else if (ATD_ALLOCATABLE(attr_idx)) {
                        PRINTMSG(AT_DEF_LINE(attr_idx), 1234, Caution,
                                 AT_DEF_COLUMN(attr_idx),
                                 AT_OBJ_NAME_PTR(attr_idx),
                                 "ALLOCATABLE");
                     }
                     else if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
                              BD_ARRAY_CLASS(attr_idx) == Deferred_Shape) {
                        PRINTMSG(AT_DEF_LINE(attr_idx), 1234, Caution,
                                 AT_DEF_COLUMN(attr_idx),
                                 AT_OBJ_NAME_PTR(attr_idx),
                                 "deferred-shape DIMENSION");
                     }
                  }
               }
               else {
                  ATD_SYMMETRIC(attr_idx)	= TRUE;
               }
            }

            if (ATD_STOR_BLK_IDX(attr_idx) == NULL_IDX) {
               assign_storage_blk(attr_idx);
            }
         }

         break;

      case Dummy_Argument:
         ATD_STOR_BLK_IDX(attr_idx) = SCP_SB_DARG_IDX(curr_scp_idx);

         if (ATD_AUXILIARY(attr_idx)) {
            SB_AUXILIARY(ATD_STOR_BLK_IDX(attr_idx)) = TRUE;
         }

         if (!AT_IS_DARG(attr_idx)) {
#ifdef KEY /* Bug 14255 */
           error_not_darg(attr_idx);
#endif /* KEY Bug 14255 */
         }
         else if (TYP_TYPE(type_idx) == Structure &&
                  ATT_DEFAULT_INITIALIZED(TYP_IDX(type_idx)) &&
                  ATD_INTENT(attr_idx) == Intent_Out &&
                  ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
                  BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Assumed_Size) {
            AT_DCL_ERR(attr_idx) = TRUE;
            PRINTMSG(AT_DEF_LINE(attr_idx), 1590, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
         }
#ifdef KEY /* Bug 14150 */
         else if (ATD_VALUE_ATTR(attr_idx) &&
	   !length_type_param_is_one(attr_idx)) {
           PRINTMSG(AT_DEF_LINE(attr_idx), 1695, Error, 
	     AT_DEF_COLUMN(attr_idx), AT_OBJ_NAME_PTR(attr_idx));
	 }
         break;
#endif /* KEY Bug 14150 */

         break;

      case CRI__Pointee:

         if (pointee_based_blk == NULL_IDX) {

            /* Create a based entry for PDGCS to use for cri_pointees */

            CREATE_ID(storage_name, sb_name[Pointee_Blk], sb_len[Pointee_Blk]);
            pointee_based_blk = ntr_stor_blk_tbl(storage_name.string,
                                                 sb_len[Pointee_Blk],
                                                 AT_DEF_LINE(attr_idx),
                                                 AT_DEF_COLUMN(attr_idx),
                                                 Based);
         }

         ATD_STOR_BLK_IDX(attr_idx)	= pointee_based_blk;
         pointer_idx			= ATD_PTR_IDX(attr_idx);

         if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {

            if (ATD_PTR_TYPE_SET(pointer_idx)) {  /* Pointer locked in */

               if (TYP_LINEAR(ATD_TYPE_IDX(pointer_idx)) != CRI_Ch_Ptr_8) {

                  /* Error - Mixing char and non-char pointers */

                  AT_DCL_ERR(attr_idx)		= TRUE;
                  AT_DCL_ERR(pointer_idx)	= TRUE;
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1428, Error,
                           AT_DEF_COLUMN(attr_idx),
                           AT_OBJ_NAME_PTR(pointer_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
            }
            else {
               ATD_PTR_TYPE_SET(pointer_idx)	= TRUE;
               ATD_TYPE_IDX(pointer_idx)	= CRI_Ch_Ptr_8;
            }
            break;
         }
         else if (ATD_PTR_TYPE_SET(pointer_idx)) {  /* Pointer locked in */

            if (TYP_LINEAR(ATD_TYPE_IDX(pointer_idx)) == CRI_Ch_Ptr_8) {

               /* Error - Mixing char and non-char pointers */

               AT_DCL_ERR(attr_idx)	= TRUE;
               AT_DCL_ERR(pointer_idx)	= TRUE;
               PRINTMSG(AT_DEF_LINE(attr_idx), 1427, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(pointer_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
         }
         

# if defined(_TARGET_OS_MAX)

         if (PACK_HALF_WORD_TEST_CONDITION(ATD_TYPE_IDX(attr_idx))) {

            if (ATD_PTR_TYPE_SET(pointer_idx)) {

               if (TYP_PTR_INCREMENT(ATD_TYPE_IDX(pointer_idx)) != 32) {
                  PRINTMSG(AT_DEF_LINE(pointer_idx), 1092, Error,
                           AT_DEF_COLUMN(pointer_idx),
                           AT_OBJ_NAME_PTR(pointer_idx));
               }
            }
            else {
               CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
               TYP_TYPE(TYP_WORK_IDX)		= CRI_Ptr;
               TYP_LINEAR(TYP_WORK_IDX)		= CRI_Ptr_8;
               TYP_PTR_INCREMENT(TYP_WORK_IDX)	= 32;
               ATD_TYPE_IDX(pointer_idx)	= ntr_type_tbl();
            }
         }
         else if (ATD_PTR_TYPE_SET(pointer_idx)) {

            if (TYP_PTR_INCREMENT(ATD_TYPE_IDX(pointer_idx)) != 64) {
               PRINTMSG(AT_DEF_LINE(pointer_idx), 1092, Error,
                        AT_DEF_COLUMN(pointer_idx),
                        AT_OBJ_NAME_PTR(pointer_idx));
            }
         }  /* Else type uses default pointer type */

# elif defined(_TARGET_OS_UNICOS)

         /* Issue caution if we are mixing potential 32 bit types with      */
         /* 64 bit types.  This works on the PVP okay, but is not portable. */

         if (TARGET_MAX_HALF_WORD_STORAGE_TYPE(ATD_TYPE_IDX(attr_idx))) {

            if (ATD_PTR_TYPE_SET(pointer_idx)) {

               if (!ATD_PTR_HALF_WORD(pointer_idx)) {
                  PRINTMSG(AT_DEF_LINE(pointer_idx), 1102, Caution,
                           AT_DEF_COLUMN(pointer_idx),
                           AT_OBJ_NAME_PTR(pointer_idx));
               }
            }
            else {
               ATD_PTR_HALF_WORD(pointer_idx)	= TRUE;
            }
         }
         else if (ATD_PTR_TYPE_SET(pointer_idx)) {

            if (ATD_PTR_HALF_WORD(pointer_idx)) {
               PRINTMSG(AT_DEF_LINE(pointer_idx), 1102, Caution,
                        AT_DEF_COLUMN(pointer_idx),
                        AT_OBJ_NAME_PTR(pointer_idx));
            }
         }

# elif defined(_TARGET_OS_SOLARIS) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

#ifdef KEY /* Bug 7299 */
	 /* We want to use default pointer type for Cray pointers */
#else /* KEY Bug 7299 */
         if (TARGET_32BIT_DOUBLE_WORD_STORAGE_TYPE(ATD_TYPE_IDX(attr_idx))) {

            if (ATD_PTR_TYPE_SET(pointer_idx)) {

               if (TYP_PTR_INCREMENT(ATD_TYPE_IDX(pointer_idx)) != 64) {
                  PRINTMSG(AT_DEF_LINE(pointer_idx), 1092, Error,
                           AT_DEF_COLUMN(pointer_idx),
                           AT_OBJ_NAME_PTR(pointer_idx));
               }
            }
            else {
               CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
               TYP_TYPE(TYP_WORK_IDX)		= CRI_Ptr;
               TYP_LINEAR(TYP_WORK_IDX)		= CRI_Ptr_8;
               TYP_PTR_INCREMENT(TYP_WORK_IDX)	= 64;
               ATD_TYPE_IDX(pointer_idx)	= ntr_type_tbl();
            }
         }
         else if (ATD_PTR_TYPE_SET(pointer_idx)) {

            if (TYP_PTR_INCREMENT(ATD_TYPE_IDX(pointer_idx)) != 32) {
               PRINTMSG(AT_DEF_LINE(pointer_idx), 1092, Error,
                        AT_DEF_COLUMN(pointer_idx),
                        AT_OBJ_NAME_PTR(pointer_idx));
            }
         }  /* Else type uses default pointer type */
#endif /* KEY Bug 7299 */

# endif
         ATD_PTR_TYPE_SET(pointer_idx) = TRUE;
         break;

      }  /* End switch */

      if (ATP_PURE(pgm_attr_idx) || ATP_ELEMENTAL(pgm_attr_idx)) {
         pure_str	= ATP_PURE(pgm_attr_idx) ? "PURE" : "ELEMENTAL";

         if (ATD_SAVED(attr_idx)) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 1264, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     pure_str,
                     AT_OBJ_NAME_PTR(pgm_attr_idx),
                     "SAVE");
         }

         if (ATD_DATA_INIT(attr_idx)) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 1264, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     pure_str,
                     AT_OBJ_NAME_PTR(pgm_attr_idx),
                     "DATA initialized");
         }

         if (ATD_CLASS(attr_idx) == Dummy_Argument) {

            if (!ATD_POINTER(attr_idx) && ATD_INTENT(attr_idx) != Intent_In) {

               if (ATP_PGM_UNIT(pgm_attr_idx) == Function) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1265, Error,
                           AT_DEF_COLUMN(attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           pure_str,
                           AT_OBJ_NAME_PTR(pgm_attr_idx));
               }
               else if (ATP_PGM_UNIT(pgm_attr_idx) == Subroutine &&
                  ATD_INTENT(attr_idx) == Intent_Unseen) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1266, Error,
                           AT_DEF_COLUMN(attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           pure_str,
                           AT_OBJ_NAME_PTR(pgm_attr_idx));
               }
            }

            if (ATP_ELEMENTAL(pgm_attr_idx) && 
                (ATD_POINTER(attr_idx) || ATD_ARRAY_IDX(attr_idx) != NULL_IDX)){
               PRINTMSG(AT_DEF_LINE(attr_idx), 1267, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(pgm_attr_idx));
            }
         }
      }

      if (ATP_PGM_UNIT(pgm_attr_idx) == Module &&
          TYP_TYPE(type_idx) == Structure &&
          ATT_DEFAULT_INITIALIZED(TYP_IDX(type_idx)) &&
          !ATD_IN_COMMON(attr_idx) &&
          (ATD_CLASS(attr_idx) == Atd_Unknown ||
           ATD_CLASS(attr_idx) == Variable) &&
          !ATD_POINTER(attr_idx) &&
          !ATD_ALLOCATABLE(attr_idx) &&
#ifdef KEY /* Bug 7967 */
	  /* "save<newline>" doesn't mark each individual variable in scope */
          !(ATD_SAVED(attr_idx) || ATP_SAVE_ALL(pgm_attr_idx))
#else /* KEY Bug 7967 */
          !ATD_SAVED(attr_idx)
#endif /* KEY Bug 7967 */
	  ) {
         PRINTMSG(AT_DEF_LINE(attr_idx), 1641, Ansi,
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx),
                  AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
      }
      break;


   case Pgm_Unit: 

      /* Set in case we have an overloaded intrinsic that references the */
      /* standard intrinsic.                                             */

      AT_SEMANTICS_DONE(attr_idx) = TRUE;

      if (ATP_PROC(attr_idx) == Intern_Proc || 
          ATP_PROC(attr_idx) == Module_Proc) {

         if (ATP_SCP_IDX(attr_idx) != curr_scp_idx) {

            /* This is an internal or module procedure that is in its   */
            /* parent's scope.  Process this when its own scope is done.*/

            AT_SEMANTICS_DONE(attr_idx) = FALSE;
            return;
         }

         /* If this pgm unit is pure and elemental, the parent        */
         /* procedures can be anything and do not need to be checked. */

         if (ATP_PROC(attr_idx) == Intern_Proc &&
             (!ATP_PURE(attr_idx) || !ATP_ELEMENTAL(attr_idx))) {
             scp_idx = SCP_PARENT_IDX(curr_scp_idx);

            while (scp_idx != NULL_IDX) {

               /* Parent is pure, so child must be too.  This only goes back */

               if (ATP_PURE(SCP_ATTR_IDX(scp_idx)) && 
                   !ATP_PURE(attr_idx) && !ATP_ELEMENTAL(attr_idx)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1272, Error,
                           AT_DEF_COLUMN(attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           ATP_PURE(SCP_ATTR_IDX(scp_idx))?"pure":"elemental",
                           AT_OBJ_NAME_PTR(SCP_ATTR_IDX(scp_idx)),
                           ATP_PURE(SCP_ATTR_IDX(scp_idx))?"PURE":"ELEMENTAL");
               }

               if (ATP_ELEMENTAL(SCP_ATTR_IDX(scp_idx)) && 
                   !ATP_ELEMENTAL(attr_idx)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1272, Error,
                           AT_DEF_COLUMN(attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "elemental",
                           AT_OBJ_NAME_PTR(SCP_ATTR_IDX(scp_idx)),
                           "ELEMENTAL");
               }
               scp_idx	= SCP_PARENT_IDX(scp_idx);
            }
         }
      }

      if (ATP_PGM_UNIT(attr_idx) == Function) {
         rslt_idx = ATP_RSLT_IDX(attr_idx);
         type_idx = ATD_TYPE_IDX(rslt_idx);

         if (TYP_TYPE(type_idx) == Structure) {

#ifdef KEY /* Bug 11741 */
	    /* Someday it would be well to rewrite the processing of:
	     *
	     *  type(t) function f()
	     *
	     * so that we treat it as:
	     *
	     *  function() result(f)
	     *    <import, implicit, and other specifications>
	     *    type(t) :: f
	     *
	     * which would eliminate a bunch of special cases caused by the
	     * attempt to process "type(t)" before we have seen the decls
	     * within the function. At this spot, we face the special case that
	     * occurs because the function type appeared prior to a no-list
	     * "import" statement which would have accessed the host.
	     */
	    int type_attr = TYP_IDX(type_idx);
	    if ((!AT_DEFINED(type_attr)) && ATP_IN_INTERFACE_BLK(attr_idx) &&
	      SCP_IMPORT(curr_scp_idx)) {
	      /* *#$*! srch_sym_tbl() requires padded name */
	      token_type t = initial_token;
	      char *name = AT_OBJ_NAME_PTR(type_attr);
	      int name_len = strlen(name);
	      memcpy(TOKEN_STR(t), name, name_len);
	      import_from_host(TOKEN_STR(t), name_len, 0, type_attr);
	    }
#endif /* KEY Bug 11741 */

            if (AT_ATTR_LINK(TYP_IDX(type_idx)) != NULL_IDX) {

               /* If this derived type is host associated (AT_ATTR_LINK is   */
               /* set) change the type table to point to the original type.  */
               /* It is okay to change the type table, because every attr of */
               /* this type needs to do this.                                */

               link_idx = TYP_IDX(type_idx);

               while (AT_ATTR_LINK(link_idx) != NULL_IDX) {
                  link_idx = AT_ATTR_LINK(link_idx);
               }

               TYP_IDX(type_idx) = link_idx;
            }
            attr_semantics(TYP_IDX(type_idx), FALSE);
         }

         bd_idx = ATD_ARRAY_IDX(rslt_idx);

         if (TYP_TYPE(type_idx) == Character) {

            if (TYP_FLD(type_idx) == AT_Tbl_Idx) {
               attr_semantics(TYP_IDX(type_idx), TRUE);
            }

            if (TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char) {

               if (ATP_ELEMENTAL(attr_idx)) {
                  PRINTMSG(AT_DEF_LINE(rslt_idx), 1564, Error,
                           AT_DEF_COLUMN(rslt_idx),
                           AT_OBJ_NAME_PTR(rslt_idx), "ELEMENTAL");
               }
               else if (ATP_PURE(attr_idx)) { 
                  PRINTMSG(AT_DEF_LINE(rslt_idx), 1564, Error,
                           AT_DEF_COLUMN(rslt_idx),
                           AT_OBJ_NAME_PTR(rslt_idx), "PURE");
               }
            }
         }

         if (bd_idx != NULL_IDX && BD_ARRAY_CLASS(bd_idx) != Deferred_Shape) {

            for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

               if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
                  attr_semantics(BD_LB_IDX(bd_idx, dim), TRUE);
               }

               if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
                  attr_semantics(BD_UB_IDX(bd_idx, dim), TRUE);
               }
            }
         }
#ifdef KEY /* Bug 4865 */
	 /* Setting AT_SEMANTICS_DONE on the result variable here bypasses the
          * code inside a later call to function attr_semantics(rslt_idx)
	  * which would perform default initialization if the result variable
	  * is a derived type that needs it.
	  * Since we don't know why the original author wanted to bypass most
	  * of the execution of that function, it seems safer to force the
	  * initialization here than to remove the bypassing. */
	 
	 if (TYP_TYPE(type_idx) == Structure &&
	    ATT_DEFAULT_INITIALIZED(TYP_IDX(type_idx))) {
#ifdef KEY /* Bug 7856 */ 
	    /* Here's something else from attr_semantics() that we're missing.
	     * And there's a lot more...some day consider removing all the
	     * code inside "if (ATP_PGM_UNIT(attr_idx) == Function)" and just
	     * calling attr_semantics(rslt_idx), and seeing what if any reason
	     * there is to replicate an increasing fraction of attr_semantics()
	     * here instead of just calling the function outright. */
	    if (bd_idx != NULL_IDX) {
	      array_dim_resolution(rslt_idx, FALSE);
	    }
#endif /* KEY Bug 7856 */ 
            gen_entry_dope_code(rslt_idx);
	 }
#endif /* KEY Bug 4865 */
         AT_SEMANTICS_DONE(rslt_idx) = TRUE;
      }

# if defined(_TARGET_OS_SOLARIS) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

      /* These return charcter results on the SPARC but not Cray. */

      if (ATP_PROC(attr_idx) != Intrin_Proc ||
          AT_OBJ_NAME(attr_idx) != '_' ||
          (!(strcmp(AT_OBJ_NAME_PTR(attr_idx), "_DATE") == 0)) &&
# ifdef KEY
          (!(strcmp(AT_OBJ_NAME_PTR(attr_idx), "_FDATE") == 0)) &&
# endif
          (!(strcmp(AT_OBJ_NAME_PTR(attr_idx), "_JDATE") == 0)) &&
          (!(strcmp(AT_OBJ_NAME_PTR(attr_idx), "_CLOCK") == 0))) {
# endif

         if (AT_USE_ASSOCIATED(attr_idx) || AT_IS_INTRIN(attr_idx)) {
            goto EXIT;
         }

# if defined(_TARGET_OS_SOLARIS) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      }
      else {
         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_TYPE(TYP_WORK_IDX)		= Character;
         TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
         TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
         TYP_FLD(TYP_WORK_IDX)		= CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX)		= C_INT_TO_CN(SA_INTEGER_DEFAULT_TYPE,
                                                      8);
         ATD_TYPE_IDX(rslt_idx)		= ntr_type_tbl();
      }
# endif

      if (ATP_PGM_UNIT(attr_idx) == Function) {

         if (!AT_TYPED(rslt_idx) && ATP_PROC(attr_idx) != Intrin_Proc) {

            if (SCP_IMPL_NONE(curr_scp_idx)) {
               AT_DCL_ERR(rslt_idx) = TRUE;
               PRINTMSG(AT_DEF_LINE(rslt_idx), 232, Error, 
                        AT_DEF_COLUMN(rslt_idx),
                        AT_OBJ_NAME_PTR(rslt_idx));
            }
            else if (!IM_SET(curr_scp_idx, IMPL_IDX(AT_OBJ_NAME(rslt_idx)))) {

               if (SCP_PARENT_NONE(curr_scp_idx)) {
                  AT_DCL_ERR(rslt_idx) = TRUE;
                  PRINTMSG(AT_DEF_LINE(rslt_idx), 233, Error, 
                           AT_DEF_COLUMN(rslt_idx),
                           AT_OBJ_NAME_PTR(rslt_idx));
               }
               else if (is_interface && attr_idx == pgm_attr_idx &&
                        SCP_IMPL_NONE(SCP_PARENT_IDX(curr_scp_idx))) {
                  AT_DCL_ERR(rslt_idx) = TRUE;
                  PRINTMSG(AT_DEF_LINE(rslt_idx), 233, Error, 
                           AT_DEF_COLUMN(rslt_idx),
                           AT_OBJ_NAME_PTR(rslt_idx));
               }
               else if (on_off_flags.implicit_none) {
                  AT_DCL_ERR(attr_idx) = TRUE;
                  PRINTMSG(AT_DEF_LINE(rslt_idx), 1171, Error,
                           AT_DEF_COLUMN(rslt_idx),
                           AT_OBJ_NAME_PTR(rslt_idx));
               }
            }
         }

         if (TYP_TYPE(type_idx) == Character) {
            char_len_resolution(rslt_idx, FALSE);

            /* reset the type_idx in case it changes */

            type_idx = ATD_TYPE_IDX(rslt_idx);
         }

         if (ATD_ARRAY_IDX(rslt_idx) != NULL_IDX) {
            array_dim_resolution(rslt_idx, FALSE);

            if (!ATP_EXPL_ITRFC(attr_idx) && !AT_DCL_ERR(rslt_idx)) {
               PRINTMSG(AT_DEF_LINE(rslt_idx), 914, Error, 
                        AT_DEF_COLUMN(rslt_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
               AT_DCL_ERR(rslt_idx)	= TRUE;
            }
         }

         if (ATD_POINTER(rslt_idx) && !ATP_EXPL_ITRFC(attr_idx)) {
            PRINTMSG(AT_DEF_LINE(rslt_idx), 915, Error, 
                     AT_DEF_COLUMN(rslt_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(rslt_idx)	= TRUE;
         }

         if (ATD_AUTOMATIC(rslt_idx) &&
             (ATD_ARRAY_IDX(rslt_idx) != NULL_IDX ||
              ATD_POINTER(rslt_idx) ||
              TYP_TYPE(type_idx) == Structure ||
              TYP_TYPE(type_idx) == Character)) {
            PRINTMSG(AT_DEF_LINE(rslt_idx), 1255, Error, 
                     AT_DEF_COLUMN(rslt_idx),
                     AT_OBJ_NAME_PTR(rslt_idx));
            AT_DCL_ERR(rslt_idx)	= TRUE;
         }
         
         if (AT_DCL_ERR(rslt_idx)) {
            AT_DCL_ERR(attr_idx)	= TRUE;
         }

         if (TYP_TYPE(type_idx) == Character &&
             TYP_CHAR_CLASS(type_idx) == Assumed_Size_Char &&
             TYP_FLD(type_idx) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(TYP_IDX(type_idx)) == Data_Obj) {

            tmp_ir_idx = ATD_TMP_IDX(TYP_IDX(type_idx));

            COPY_OPND(opnd, IR_OPND_R(tmp_ir_idx));
            fold_clen_opr(&opnd, &expr_desc);
            COPY_OPND(IR_OPND_R(tmp_ir_idx), opnd);
         }

         /* All character, structure and array-valued function results */
         /* become the zeroth darg.  All scalar function results with  */
         /* alternate entries are stored in the equivalence block.     */

        
#ifdef KEY /* Bug 5089 */
         if (FUNCTION_MUST_BE_SUBROUTINE(attr_idx, rslt_idx))
#else /* KEY Bug 5089 */
         if (FUNCTION_MUST_BE_SUBROUTINE(rslt_idx))
#endif /* KEY Bug 5089 */
	 {

            ATP_EXTRA_DARG(attr_idx)      = TRUE;

            if (ATP_EXPL_ITRFC(attr_idx)) {
               ATD_STOR_BLK_IDX(rslt_idx) = SCP_SB_DARG_IDX(curr_scp_idx);

               /* Insert the function result as the zero'th darg */

               if (ATP_FIRST_IDX(attr_idx) == NULL_IDX) {
                  NTR_SN_TBL(sn_idx);
               }
               else {
                  sn_idx = ATP_FIRST_IDX(attr_idx) - 1;
               }
               ATP_FIRST_IDX(attr_idx)    = sn_idx;
               ATP_NUM_DARGS(attr_idx)   += 1;
               SN_NAME_LEN(sn_idx)        = AT_NAME_LEN(rslt_idx);
               SN_NAME_IDX(sn_idx)        = AT_NAME_IDX(rslt_idx);
               SN_ATTR_IDX(sn_idx)        = rslt_idx;
               SN_LINE_NUM(sn_idx)        = AT_DEF_LINE(rslt_idx);
               SN_COLUMN_NUM(sn_idx)      = AT_DEF_COLUMN(rslt_idx);
            }
         }
         else if (SCP_ALT_ENTRY_CNT(curr_scp_idx) > 0 &&
                  (attr_idx == pgm_attr_idx || ATP_ALT_ENTRY(attr_idx))) {

            if (alt_entry_equiv_blk == NULL_IDX) {

               /* Create an equivalence entry for PDGCS to use for alternate */
               /* function results.  The offset is always zero.              */

               alt_entry_equiv_blk = create_equiv_stor_blk(attr_idx, Stack);
            }

            if (ATP_RSLT_IDX(attr_idx) != NULL_IDX) {
               storage_size = stor_bit_size_of(ATP_RSLT_IDX(attr_idx), 
                                               TRUE,
                                               FALSE);

               /* KAY - Set SB_LEN correctly here when storage_size is fixed.*/

               if (storage_size.fld == NO_Tbl_Idx) {
                  storage_size.fld	= CN_Tbl_Idx;
                  storage_size.idx	= ntr_const_tbl(storage_size.type_idx,
                                                        FALSE,
                                                        storage_size.constant);
               }

# if defined(_TARGET_OS_MAX)

               else if (storage_size.fld == IR_Tbl_Idx || 
                        storage_size.fld == IL_Tbl_Idx) {
                  tmp_idx = gen_compiler_tmp(SB_DEF_LINE(alt_entry_equiv_blk),
                                             SB_DEF_COLUMN(alt_entry_equiv_blk),
                                             Priv, TRUE);
                  ATD_TYPE_IDX(tmp_idx)			= INTEGER_DEFAULT_TYPE;
                  ATD_TMP_IDX(tmp_idx)			= storage_size.idx;
                  ATD_FLD(tmp_idx)			= storage_size.fld;
                  ATD_SYMBOLIC_CONSTANT(tmp_idx)	= TRUE;
                  storage_size.fld			= AT_Tbl_Idx;
                  storage_size.idx			= tmp_idx;
               }

               if (attr_idx == pgm_attr_idx &&
                   ATD_ARRAY_IDX(rslt_idx) != NULL_IDX &&
                   BD_ARRAY_SIZE(ATD_ARRAY_IDX(rslt_idx)) == 
                                               Symbolic_Constant_Size){
                  PRINTMSG(AT_DEF_LINE(rslt_idx), 1230, Error, 
                           AT_DEF_COLUMN(rslt_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
                  AT_DCL_ERR(rslt_idx)	= TRUE;
               }
# endif

               SB_LEN_FLD(alt_entry_equiv_blk) = storage_size.fld;
               SB_LEN_IDX(alt_entry_equiv_blk) = storage_size.idx;
            }

            ATD_STOR_BLK_IDX(rslt_idx)		= alt_entry_equiv_blk;
            ATD_EQUIV(rslt_idx)			= TRUE;
            ATD_OFFSET_ASSIGNED(rslt_idx)	= TRUE;
            ATD_OFFSET_FLD(rslt_idx)		= CN_Tbl_Idx;
            ATD_OFFSET_IDX(rslt_idx)		= CN_INTEGER_ZERO_IDX;

            if (alt_entry_equiv_grp == NULL_IDX) {
               NTR_EQ_TBL(alt_entry_equiv_grp);
               EQ_GRP_END_IDX(alt_entry_equiv_grp)	= alt_entry_equiv_grp;
               eq_idx					= alt_entry_equiv_grp;
               EQ_GRP_IDX(eq_idx)			= alt_entry_equiv_grp;
            }
            else {
               NTR_EQ_TBL(eq_idx);
               EQ_NEXT_EQUIV_OBJ(EQ_GRP_END_IDX(alt_entry_equiv_grp)) = eq_idx;
               EQ_GRP_END_IDX(alt_entry_equiv_grp)		      = eq_idx;
               EQ_GRP_IDX(eq_idx)	= alt_entry_equiv_grp;
            }

            EQ_LINE_NUM(eq_idx)		= AT_DEF_LINE(rslt_idx);
            EQ_COLUMN_NUM(eq_idx)	= AT_DEF_COLUMN(rslt_idx);
            EQ_ATTR_IDX(eq_idx)		= rslt_idx;
         }
         else {
            ATD_STOR_BLK_IDX(rslt_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
         }

         if (ATP_ALT_ENTRY(attr_idx)) {
            compare_entry_to_func_rslt(attr_idx, ATP_RSLT_IDX(pgm_attr_idx));
         }
         
         if (ATP_ELEMENTAL(attr_idx) && 
             (ATD_POINTER(rslt_idx) || ATD_ARRAY_IDX(rslt_idx) != NULL_IDX)) {
            PRINTMSG(AT_DEF_LINE(rslt_idx), 1268, Error, 
                     AT_DEF_COLUMN(rslt_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     AT_OBJ_NAME_PTR(rslt_idx));
            AT_DCL_ERR(rslt_idx)	= TRUE;
         }
      }
      else if (ATP_PGM_UNIT(attr_idx) == Subroutine) {

         if (ATP_HAS_ALT_RETURN(attr_idx)) {

            if (ATP_ELEMENTAL(pgm_attr_idx)) {

               /* Illegal to have alternate return in an elemental. */
               /* Find location and issue an error.                 */

               for (sn_idx = ATP_FIRST_IDX(pgm_attr_idx);
                    sn_idx <= ATP_FIRST_IDX(pgm_attr_idx) + 
                              ATP_NUM_DARGS(pgm_attr_idx);
                    sn_idx++) {

                  if (AT_OBJ_CLASS(SN_ATTR_IDX(sn_idx)) == Data_Obj &&
                      ATD_CLASS(SN_ATTR_IDX(sn_idx)) == Dummy_Argument &&
                      AT_COMPILER_GEND(SN_ATTR_IDX(sn_idx))) {
                     PRINTMSG(AT_DEF_LINE(SN_ATTR_IDX(sn_idx)), 1269, Error, 
                              AT_DEF_COLUMN(SN_ATTR_IDX(sn_idx)),
                              AT_OBJ_NAME_PTR(pgm_attr_idx));
                     AT_DCL_ERR(pgm_attr_idx)	= TRUE;
                  }
               }
            }

            /* The interface needs to have a function result for this  */
            /* subroutine because of the alternate return.             */

            CREATE_FUNC_RSLT(attr_idx, rslt_idx);
            AT_DEFINED(rslt_idx)	= TRUE;
            ATD_TYPE_IDX(rslt_idx)	= CG_INTEGER_DEFAULT_TYPE;
            ATD_STOR_BLK_IDX(rslt_idx)	= SCP_SB_STACK_IDX(curr_scp_idx);

            if (ATP_ALT_ENTRY(attr_idx)) {

               if (alt_entry_equiv_blk == NULL_IDX) {

                  /* Create an equivalence entry for PDGCS   */
                  /* to use for alternate function results.  */

                  alt_entry_equiv_blk = create_equiv_stor_blk(attr_idx, Stack);
               }

               if (alt_entry_equiv_grp == NULL_IDX) {
                  NTR_EQ_TBL(alt_entry_equiv_grp);
                  EQ_GRP_END_IDX(alt_entry_equiv_grp)	= alt_entry_equiv_grp;
                  eq_idx				= alt_entry_equiv_grp;
                  EQ_GRP_IDX(eq_idx)			= alt_entry_equiv_grp;
               }
               else {
                  NTR_EQ_TBL(eq_idx);
                  EQ_NEXT_EQUIV_OBJ(EQ_GRP_END_IDX(alt_entry_equiv_grp))=eq_idx;
                  EQ_GRP_END_IDX(alt_entry_equiv_grp)	= eq_idx;
                  EQ_GRP_IDX(eq_idx)			= alt_entry_equiv_grp;
               }

               EQ_LINE_NUM(eq_idx)		= AT_DEF_LINE(rslt_idx);
               EQ_COLUMN_NUM(eq_idx)		= AT_DEF_COLUMN(rslt_idx);
               EQ_ATTR_IDX(eq_idx)		= rslt_idx;
               ATD_STOR_BLK_IDX(rslt_idx)	= alt_entry_equiv_blk;
            }
         }
      }
      else if (ATP_PGM_UNIT(attr_idx) == Pgm_Unknown) {

         if (ATP_PROC(attr_idx) == Module_Proc) {

            /* MODULE PROCEDURE specified in INTERFACE, but the MODULE  */
            /* PROCEDURE was never accessed in the MODULE or from USE.  */

            AT_DCL_ERR(attr_idx) = TRUE;
            PRINTMSG(AT_DEF_LINE(attr_idx), 368, Error, 
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
         else if (ATP_PROC(attr_idx) == Dummy_Proc) {

            /* dummy arg has been declared in external stmt   */
            /* but it is still unknown prog.  Valid Fortran.  */
            /* Leave it as a Pgm_Unknown, but implicitly type */
            /* this, just in case a function is passed in     */
            /* as an actual argument for this dummy proc.     */

            CREATE_FUNC_RSLT(attr_idx, rslt_idx);
            SET_IMPL_TYPE(rslt_idx);
         }
      }

      if (ATP_PGM_UNIT(attr_idx) != Module && 
          ATP_FIRST_IDX(attr_idx) != NULL_IDX) {    /* Process the dargs */

         for (i = (ATP_EXTRA_DARG(attr_idx) ? 1 : 0);
              i < ATP_NUM_DARGS(attr_idx); i++) {
            darg_idx = SN_ATTR_IDX(ATP_FIRST_IDX(attr_idx) + i);
            attr_semantics(darg_idx, FALSE);
         }
      }

      /* Vfunction infers no side effects - so set it now */

      ATP_NOSIDE_EFFECTS(attr_idx) = ATP_NOSIDE_EFFECTS(attr_idx) |
                                     ATP_VFUNCTION(attr_idx);

      /* If this is the program unit being defined in the interface, set   */
      /* ATP_SCP_IDX to the parent's scope, otherwise clear it, because    */
      /* this is an invalid scope id, when the interface scope is removed. */

      if (is_interface) {
         ATP_SCP_IDX(attr_idx) = SCP_PARENT_IDX(curr_scp_idx);
      }

      if (ATP_PROC(attr_idx) == Dummy_Proc) {

#ifdef KEY /* Bug 14255 */
         if (!AT_IS_DARG(attr_idx)) {
	   error_not_darg(attr_idx);
	 }
#endif /* KEY Bug 14255 */

         /* If this is an interface specific, pgm_attr_idx is set to the */
         /* specific.  The correct attr to check is the program unit     */
         /* containing the procedure.                                    */

         proc_idx = is_interface ? SCP_ATTR_IDX(SCP_PARENT_IDX(curr_scp_idx)) :
                                   pgm_attr_idx;

         if (ATP_ELEMENTAL(proc_idx)) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 1267, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     AT_OBJ_NAME_PTR(proc_idx));
         }
         else if (ATP_PURE(proc_idx) && !ATP_PURE(attr_idx)) {

            /* Dummy procedures must be given the PURE attribute */

            PRINTMSG(AT_DEF_LINE(attr_idx), 1271, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     AT_OBJ_NAME_PTR(proc_idx));
         }
      }

      if (ATP_DUPLICATE_INTERFACE_IDX(attr_idx) != NULL_IDX) {

         /* An interface body has been specified for the program unit */
         /* being compiled.  Verify that they are identical.  If they */
         /* are, issue an ansi message, otherwise, issue an error.    */

         compare_duplicate_interface_bodies(attr_idx);
      }


      break;

   case Label:

      if (!AT_DEFINED(attr_idx)) {
# ifdef _DEBUG
         if (ATL_FWD_REF_IDX(attr_idx) == NULL_IDX  &&
             (ATL_CLASS(attr_idx) == Lbl_User  ||
              ATL_CLASS(attr_idx) == Lbl_Format)) {
            PRINTMSG(stmt_start_line, 9, Internal, 
                     stmt_start_col, AT_OBJ_NAME_PTR(attr_idx));
         }
# endif
         curr_fwd_ref_idx = ATL_FWD_REF_IDX(attr_idx);
      
         while (curr_fwd_ref_idx != NULL_IDX) {
            if (IL_FLD(curr_fwd_ref_idx) == IL_Tbl_Idx) {
               line = IL_LINE_NUM(IL_IDX(curr_fwd_ref_idx));
               column = IL_COL_NUM(IL_IDX(curr_fwd_ref_idx));
            }
            else {
               line = IL_LINE_NUM(curr_fwd_ref_idx);
               column = IL_COL_NUM(curr_fwd_ref_idx);
            }
            PRINTMSG(line, 23, Error, column,
                     AT_OBJ_NAME_PTR(attr_idx));
            old_fwd_ref_idx  = curr_fwd_ref_idx;
            curr_fwd_ref_idx = IL_NEXT_LIST_IDX(curr_fwd_ref_idx);
            FREE_IR_LIST_NODE(old_fwd_ref_idx);
         }
                    
         ATL_FWD_REF_IDX(attr_idx) = NULL_IDX;
      } 
      break;

   case Derived_Type:

      /* Set in case, any components are ptrs to the derived type. */

      AT_SEMANTICS_DONE(attr_idx)	= TRUE;
      sn_idx				= ATT_FIRST_CPNT_IDX(attr_idx);

      while (sn_idx != NULL_IDX) {
         type_idx = ATD_TYPE_IDX(SN_ATTR_IDX(sn_idx));

         if (TYP_TYPE(type_idx) == Structure) {
            dt_idx = TYP_IDX(type_idx);
            attr_semantics(dt_idx, FALSE);

            if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module &&
                !AT_PRIVATE(attr_idx) &&
                !ATT_PRIVATE_CPNT(attr_idx) &&
                 AT_PRIVATE(dt_idx) &&
                !AT_USE_ASSOCIATED(dt_idx)) { /* interp 161 */
               PRINTMSG(AT_DEF_LINE(SN_ATTR_IDX(sn_idx)), 45, Error,
                        AT_DEF_COLUMN(SN_ATTR_IDX(sn_idx)),
                        AT_OBJ_NAME_PTR(SN_ATTR_IDX(sn_idx)),
                        AT_OBJ_NAME_PTR(dt_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }

            if (!AT_USE_ASSOCIATED(attr_idx) &&
                ATT_SEQUENCE_SET(attr_idx) && !ATT_SEQUENCE_SET(dt_idx)) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 140, Error,
                        AT_DEF_COLUMN(attr_idx));
            }

         }

         if (!AT_USE_ASSOCIATED(attr_idx) &&
             ATD_CPNT_INIT_IDX(SN_ATTR_IDX(sn_idx)) != NULL_IDX) {
#ifdef KEY /* Bug 6845 */
	    int cpnt_idx = SN_ATTR_IDX(sn_idx);
            if (ATD_ALLOCATABLE(cpnt_idx)) {
	      PRINTMSG(AT_DEF_LINE(cpnt_idx), 1680, Error,
	        AT_DEF_COLUMN(cpnt_idx), AT_OBJ_NAME_PTR(cpnt_idx));
	    }
	    else {
	      default_init_semantics(SN_ATTR_IDX(sn_idx));
	    }
#else /* KEY Bug 6845 */
            default_init_semantics(SN_ATTR_IDX(sn_idx));
#endif /* KEY Bug 6845 */
         }
         sn_idx = SN_SIBLING_LINK(sn_idx);
      }

      if (!AT_DEFINED(attr_idx)) {
         issue_undefined_type_msg(attr_idx, 
                                  AT_DEF_LINE(attr_idx),
                                  AT_DEF_COLUMN(attr_idx));
      }

      if (is_interface) {
         ATT_SCP_IDX(attr_idx)	= SCP_PARENT_IDX(curr_scp_idx);
      }

      if (ATT_LABEL_LIST_IDX(attr_idx) != NULL_IDX) {

         /* This list is used for parsing only.  Free and clear the field */

         free_attr_list(ATT_LABEL_LIST_IDX(attr_idx));
         ATT_LABEL_LIST_IDX(attr_idx)	= NULL_IDX;
      }

      break;

   case Interface: 

      if (!ATI_UNNAMED_INTERFACE(attr_idx)) {

         if (!AT_IS_INTRIN(attr_idx)) {

            /* If there is a program unit with the same name, make sure it */
            /* is in this interface block.                                 */
 
            pgm_idx = ATI_PROC_IDX(attr_idx);

            if (pgm_idx != NULL_IDX && ATP_PROC(pgm_idx) == Module_Proc) {

               if (ATP_PGM_UNIT(pgm_idx) == Pgm_Unknown) {

                  /* Need to search host for this module procedure */

                  sn_attr_idx = srch_host_sym_tbl(AT_OBJ_NAME_PTR(pgm_idx),
                                                  AT_NAME_LEN(pgm_idx),
                                                  &name_idx,
                                                  FALSE);

                  if (sn_attr_idx != NULL_IDX &&
                      AT_OBJ_CLASS(sn_attr_idx) == Interface &&
                      ATI_PROC_IDX(sn_attr_idx) != NULL_IDX) {
                     AT_ATTR_LINK(pgm_idx)	= ATI_PROC_IDX(sn_attr_idx);
                     ATI_PROC_IDX(attr_idx)	= ATI_PROC_IDX(sn_attr_idx);
                  }
                  else if (sn_attr_idx != NULL_IDX &&
                           AT_OBJ_CLASS(sn_attr_idx) == Pgm_Unit &&
                           ATP_PROC(sn_attr_idx) == Module_Proc) {
                     ATI_PROC_IDX(attr_idx)	= sn_attr_idx;
                     AT_ATTR_LINK(pgm_idx)		= sn_attr_idx;
                  }
                  else if (!AT_DCL_ERR(pgm_idx)) { 
                     PRINTMSG(AT_DEF_LINE(pgm_idx), 368, Error, 
                              AT_DEF_COLUMN(pgm_idx),
                              AT_OBJ_NAME_PTR(pgm_idx));
                     AT_DCL_ERR(pgm_idx)	= TRUE;
                     AT_DCL_ERR(attr_idx)	= TRUE;
                  }
               }

               sn_idx		= ATI_FIRST_SPECIFIC_IDX(attr_idx);
               sn_attr_idx	= srch_linked_sn(AT_OBJ_NAME_PTR(attr_idx),
                                                 AT_NAME_LEN(attr_idx),
                                                 &sn_idx);

               if (sn_attr_idx == NULL_IDX) {
                  AT_DCL_ERR(attr_idx) = TRUE;
                  PRINTMSG(AT_DEF_LINE(ATI_PROC_IDX(attr_idx)), 712, Error, 
                           AT_DEF_COLUMN(ATI_PROC_IDX(attr_idx)),
                           AT_OBJ_NAME_PTR(attr_idx),
                           (ATP_PGM_UNIT(ATI_PROC_IDX(attr_idx)) == Function) ? 
                                         "FUNCTION" : "SUBROUTINE",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else {

                  /* Need to generate a usage record for the module procedure */
                  /* definition here.  Could not do it earlier, as we did     */
                  /* might not have the proper attr.                          */

                  if ((cif_flags & XREF_RECS) != 0) {
                     cif_usage_rec(attr_idx,
                                   AT_Tbl_Idx,
                                   SN_LINE_NUM(sn_attr_idx),
                                   SN_COLUMN_NUM(sn_attr_idx),
                                   CIF_Symbol_Declaration);
                  }
               }
            }
     
            if (AT_TYPED(attr_idx)) {
               AT_DCL_ERR(attr_idx) = TRUE;
               PRINTMSG(AT_DEF_LINE(attr_idx), 949, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
         }
         else if (AT_TYPED(attr_idx)) { /* The intrinsic has been typed. */

            PRINTMSG(AT_DEF_LINE(attr_idx), 711, Caution,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));

            type_idx = ATD_TYPE_IDX(attr_idx);

            if (TYP_TYPE(type_idx) == Structure) {
          
               if (AT_ATTR_LINK(TYP_IDX(type_idx)) != NULL_IDX) {

                  /* If this derived type is host associated (AT_ATTR_LINK  */
                  /* is set) change the type table to point to the original */
                  /* type.  It is okay to change the type table, because    */
                  /* every attr of this type needs to do this.              */
 
                  link_idx = TYP_IDX(type_idx);
 
                  while (AT_ATTR_LINK(link_idx) != NULL_IDX) {
                     link_idx = AT_ATTR_LINK(link_idx);
                  }
 
                  TYP_IDX(type_idx) = link_idx;
               }
 
               attr_semantics(TYP_IDX(type_idx), FALSE);
            }

            if (AT_USE_ASSOCIATED(attr_idx)) {
               goto EXIT;
            }

            if (TYP_TYPE(type_idx) == Character) {

               if (TYP_FLD(type_idx) == AT_Tbl_Idx) {
                  attr_semantics(TYP_IDX(type_idx), TRUE);
               }
            }

            if (AT_DCL_ERR(attr_idx)) {
               goto EXIT;
            }
         }

         /* We allow inline and ipa directives on interfaces.    */
         /* Do some semantics here.  First they can only be      */
         /* specified on intrinsics that have user specified     */
         /* intrinsics.  Second, set the flags on the specifics. */

         if (ATI_INLINE_ALWAYS(attr_idx) ||
             ATI_INLINE_NEVER(attr_idx) ||
             ATI_SGI_ROUTINE_INLINE(attr_idx) ||
             ATI_SGI_ROUTINE_NOINLINE(attr_idx)) {

            if (AT_IS_INTRIN(attr_idx) && !ATI_USER_SPECIFIED(attr_idx)) {

               if (ATI_IPA_DIR_SPECIFIED(attr_idx)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1655, Error,
                           AT_DEF_COLUMN(attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "IPA");
               }
               else {  /* INLINE directive */
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1655, Error,
                           AT_DEF_COLUMN(attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "INLINE");
               }
            }
            else {  /* Set flags on specifics */
               sn_idx = ATI_FIRST_SPECIFIC_IDX(attr_idx);

               while (sn_idx != NULL_IDX) {

                  if (!AT_IS_INTRIN(SN_ATTR_IDX(sn_idx))) {
                     ATP_INLINE_ALWAYS(SN_ATTR_IDX(sn_idx)) =
                                                   ATI_INLINE_ALWAYS(attr_idx);
                     ATP_INLINE_NEVER(SN_ATTR_IDX(sn_idx)) =
                                                   ATI_INLINE_NEVER(attr_idx);
                     ATP_SGI_ROUTINE_INLINE(SN_ATTR_IDX(sn_idx)) =
                                            ATI_SGI_ROUTINE_INLINE(attr_idx);
                     ATP_SGI_ROUTINE_NOINLINE(SN_ATTR_IDX(sn_idx)) =
                                            ATI_SGI_ROUTINE_NOINLINE(attr_idx);
                  }
                  sn_idx = SN_SIBLING_LINK(sn_idx);
               }
            }
         }

         verify_interface(attr_idx);
      }
      break;

   case Namelist_Grp:

      NTR_SN_TBL(sn_idx);

      SN_SIBLING_LINK(sn_idx)	= namelist_list_idx;
      namelist_list_idx		= sn_idx;
      SN_ATTR_IDX(sn_idx)	= attr_idx;
            
      break;

   case Stmt_Func:

      if (AT_COMPILER_GEND(attr_idx)) {
         break;
      }

      type_idx = ATD_TYPE_IDX(attr_idx);

      if (TYP_TYPE(type_idx) == Structure) {
          
         if (AT_ATTR_LINK(TYP_IDX(type_idx)) != NULL_IDX) {

            /* If this derived type is host associated (AT_ATTR_LINK is set)  */
            /* change the type table to point to the original type.  It is    */
            /* okay to change the type table, because every attr of this type */
            /* needs to do this.                                              */

            link_idx = TYP_IDX(type_idx);

            while (AT_ATTR_LINK(link_idx) != NULL_IDX) {
               link_idx = AT_ATTR_LINK(link_idx);
            }

            TYP_IDX(type_idx) = link_idx;
         }

         attr_semantics(TYP_IDX(type_idx), FALSE);
      }

      if (ATP_PGM_UNIT(pgm_attr_idx) == Module) { 

         if (TYP_TYPE(type_idx) == Structure &&
             !AT_PRIVATE(attr_idx) &&
             AT_PRIVATE(TYP_IDX(type_idx)) &&
             !AT_USE_ASSOCIATED(TYP_IDX(type_idx))) { /* Interp 161 */
            PRINTMSG(AT_DEF_LINE(attr_idx), 598, Error, 
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
         }
      }

      if (AT_USE_ASSOCIATED(attr_idx)) {
         goto EXIT;
      }

      if (TYP_TYPE(type_idx) == Character) {

         if (TYP_FLD(type_idx) == AT_Tbl_Idx) {
            attr_semantics(TYP_IDX(type_idx), TRUE);
         }
      }

      if (AT_DCL_ERR(attr_idx)) {
         goto EXIT;
      }

      if (!AT_TYPED(attr_idx)) {
             
         if (SCP_IMPL_NONE(curr_scp_idx)) {
            AT_DCL_ERR(attr_idx) = TRUE;
            PRINTMSG(AT_DEF_LINE(attr_idx), 740, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
         else if (!IM_SET(curr_scp_idx, IMPL_IDX(AT_OBJ_NAME(attr_idx)))) {

            if (SCP_PARENT_NONE(curr_scp_idx)) {
               AT_DCL_ERR(attr_idx) = TRUE;
               PRINTMSG(AT_DEF_LINE(attr_idx), 742, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
            else if (on_off_flags.implicit_none) {
               AT_DCL_ERR(attr_idx) = TRUE;
               PRINTMSG(AT_DEF_LINE(attr_idx), 1171, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
         }
      }

      if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) { 
         char_len_resolution(attr_idx, FALSE);
         type_idx = ATD_TYPE_IDX(attr_idx); /* Reset type_idx */
      }

      /* Check the dummy arguments. */

      first_idx = ATP_FIRST_IDX(attr_idx);
      count     = ATP_NUM_DARGS(attr_idx);

      for (i = first_idx; i < (first_idx + count); i++) {
         sf_attr_idx = SN_ATTR_IDX(i);

         if (TYP_TYPE(ATD_TYPE_IDX(sf_attr_idx)) == Character) { 

            if (TYP_TYPE(ATD_TYPE_IDX(sf_attr_idx)) == Character) {

               if (TYP_FLD(ATD_TYPE_IDX(sf_attr_idx)) == AT_Tbl_Idx) {
                  attr_semantics(TYP_IDX(ATD_TYPE_IDX(sf_attr_idx)), TRUE);
               }
            }

            type_resolved = TYP_RESOLVED(ATD_TYPE_IDX(sf_attr_idx));
            char_len_resolution(sf_attr_idx, TRUE);

            if (TYP_CHAR_CLASS(ATD_TYPE_IDX(sf_attr_idx)) != Const_Len_Char) {

               if (!AT_DCL_ERR(sf_attr_idx)) {
                  PRINTMSG(AT_DEF_LINE(sf_attr_idx), 215, Error,
                           AT_DEF_COLUMN(sf_attr_idx),
                           AT_OBJ_NAME_PTR(sf_attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
                  AT_DCL_ERR(sf_attr_idx) = TRUE;
               }

               /* Reset so that if type needs to be resolved for use as a */
               /* variable that it happens and error recovery is good.    */

               TYP_RESOLVED(ATD_TYPE_IDX(sf_attr_idx)) = type_resolved;
               ATD_TYPE_IDX(sf_attr_idx) = CHARACTER_DEFAULT_TYPE;
            }
         }

         if (!AT_TYPED(sf_attr_idx)) {
          
            if (SCP_IMPL_NONE(curr_scp_idx)) {
               AT_DCL_ERR(sf_attr_idx) = TRUE;
               PRINTMSG(AT_DEF_LINE(sf_attr_idx), 741, Error,
                        AT_DEF_COLUMN(sf_attr_idx),
                        AT_OBJ_NAME_PTR(sf_attr_idx));
            }
            else if (!IM_SET(curr_scp_idx, IMPL_IDX(AT_OBJ_NAME(sf_attr_idx)))){

               if (SCP_PARENT_NONE(curr_scp_idx)) {
                  AT_DCL_ERR(sf_attr_idx) = TRUE;
                  PRINTMSG(AT_DEF_LINE(sf_attr_idx), 743, Error,
                           AT_DEF_COLUMN(sf_attr_idx),
                           AT_OBJ_NAME_PTR(sf_attr_idx));
               }
               else if (on_off_flags.implicit_none) {
                  AT_DCL_ERR(attr_idx) = TRUE;
                  PRINTMSG(AT_DEF_LINE(sf_attr_idx), 1171, Error,
                           AT_DEF_COLUMN(sf_attr_idx),
                           AT_OBJ_NAME_PTR(sf_attr_idx));
               }
            }
         }

         darg_idx = srch_sym_tbl(AT_OBJ_NAME_PTR(sf_attr_idx),
                                 AT_NAME_LEN(sf_attr_idx),
                                 &name_idx);

         if (darg_idx != NULL_IDX && AT_OBJ_CLASS(darg_idx) == Data_Obj &&
             TYP_TYPE(ATD_TYPE_IDX(darg_idx)) != 
                               TYP_TYPE(ATD_TYPE_IDX(sf_attr_idx))) {

            PRINTMSG(AT_DEF_LINE(sf_attr_idx), 940, Ansi,
                     AT_DEF_COLUMN(sf_attr_idx),
                     AT_OBJ_NAME_PTR(sf_attr_idx));
         }
      }
      break;

   }  /* End switch */

# ifdef _F_MINUS_MINUS
   if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
       ATD_CLASS(attr_idx) == Variable &&
       ATD_ALLOCATABLE(attr_idx) &&
       ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX &&
       ATD_VARIABLE_TMP_IDX(attr_idx) == NULL_IDX &&
       ! AT_DCL_ERR(attr_idx)) {

      /* set up ptr/pointee pair with explicit bd entries */

      gen_allocatable_ptr_ptee(attr_idx);
   }
# endif
#ifdef KEY /* Bug 14150 */
   check_interoperable_constraints(attr_idx);
#endif /* KEY Bug 14150 */

EXIT:

   AT_SEMANTICS_DONE(attr_idx) = TRUE;

   TRACE (Func_Exit, "attr_semantics", NULL);

   return;

}  /* attr_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This does semantic checking for the end of an interface block.        *|
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
static void namelist_resolution(int	namelist_idx)
{

   int		attr_idx;
   int		entry_idx;
   boolean	namelist_err;
   int		namelist_grp_attr;
   int		scp_idx;
   int		sn_idx;
   boolean	taskcommon;


   TRACE (Func_Entry, "namelist_resolution", NULL);

   taskcommon = cmd_line_flags.taskcommon;

   while (namelist_idx != NULL_IDX) {
      namelist_grp_attr	= SN_ATTR_IDX(namelist_idx);
      sn_idx		= ATN_FIRST_NAMELIST_IDX(namelist_grp_attr);

      if (!AT_USE_ASSOCIATED(namelist_grp_attr)) {
         namelist_err	= FALSE;

         while (sn_idx != NULL_IDX) {
            attr_idx = SN_ATTR_IDX(sn_idx);

            while (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
               attr_idx = AT_ATTR_LINK(attr_idx);
            }

            /* If they have the same name, this will always be the pgm unit */

            if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
                ATP_PGM_UNIT(attr_idx) == Function &&
                ATP_PROC(attr_idx) != Intrin_Proc) {

               if (attr_idx == SCP_ATTR_IDX(curr_scp_idx) ||
                   (ATP_ALT_ENTRY(attr_idx) &&
                    SCP_PARENT_IDX(curr_scp_idx) == NULL_IDX)) {
                  goto FOUND;
               }

               scp_idx = curr_scp_idx;

               while (scp_idx != NULL_IDX) {

                  if (attr_idx == SCP_ATTR_IDX(scp_idx)) {
                     goto FOUND;
                  }

                  entry_idx = SCP_ENTRY_IDX(scp_idx);

                  while (entry_idx != NULL_IDX) {

                     if (attr_idx == AL_ATTR_IDX(entry_idx)) {
                        goto FOUND;
                     }
                     entry_idx = AL_NEXT_IDX(entry_idx);
                  }
                  scp_idx = SCP_PARENT_IDX(scp_idx);
               }

               PRINTMSG(SN_LINE_NUM(sn_idx), 657, Error, SN_COLUMN_NUM(sn_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
               AT_DCL_ERR(attr_idx)			= TRUE;
               AT_DCL_ERR(ATP_RSLT_IDX(attr_idx))	= TRUE;
               namelist_err				= TRUE;

FOUND:
               if (!ATP_RSLT_NAME(attr_idx)) {

                  /* If the function and the result name are the same name    */
                  /* switch it to use the result name.  If they are different */
                  /* this will be caught by fnd_semantic_err.                 */

                  attr_idx = ATP_RSLT_IDX(attr_idx);
               }
            }

            AT_NAMELIST_OBJ(attr_idx) = TRUE;
            SN_ATTR_IDX(sn_idx) = attr_idx;

            if (!AT_DCL_ERR(attr_idx) &&
                !fnd_semantic_err(Obj_Namelist_Obj,
                                  SN_LINE_NUM(sn_idx),
                                  SN_COLUMN_NUM(sn_idx),
                                  attr_idx,
                                  TRUE)) {

               if (ATD_STOR_BLK_IDX(attr_idx) != NULL_IDX &&
                   SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx)) == Task_Common) {

                  taskcommon = TRUE;
               }

               if (ATD_STOR_BLK_IDX(attr_idx) != NULL_IDX &&
                   SB_IS_COMMON(ATD_STOR_BLK_IDX(attr_idx)) &&
                   SB_AUXILIARY(ATD_STOR_BLK_IDX(attr_idx))) {
                  PRINTMSG(SN_LINE_NUM(sn_idx), 663, Error, 
                           SN_COLUMN_NUM(sn_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           SB_NAME_PTR(ATD_STOR_BLK_IDX(attr_idx)));
                  AT_DCL_ERR(attr_idx)	= TRUE;  /* Needed to prevent dup msg */
                  namelist_err		= TRUE;
               }
   
               if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure           &&
#ifdef KEY /* Bug 6845 */
                   (ATT_ALLOCATABLE_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx))) ||
                   ATT_POINTER_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx))))
#else /* KEY Bug 6845 */
                   ATT_POINTER_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx)))
#endif /* KEY Bug 6845 */
		   ) {
                  PRINTMSG(SN_LINE_NUM(sn_idx), 484, Error, 
                           SN_COLUMN_NUM(sn_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
                  namelist_err = TRUE;
               }

               if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module && 
                   !AT_PRIVATE(namelist_grp_attr) && AT_PRIVATE(attr_idx) &&
                   !AT_USE_ASSOCIATED(attr_idx)) {  /* Interp 161 */

                  PRINTMSG(SN_LINE_NUM(sn_idx), 438, Error, 
                           SN_COLUMN_NUM(sn_idx),
                           AT_OBJ_NAME_PTR(namelist_grp_attr),
                           AT_OBJ_NAME_PTR(attr_idx));
                  namelist_err = TRUE;
               }
               else if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module &&
                        !AT_PRIVATE(namelist_grp_attr) &&
                        TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure &&
                        ATT_PRIVATE_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx)))) {

                  PRINTMSG(SN_LINE_NUM(sn_idx), 1085, Error,
                           SN_COLUMN_NUM(sn_idx),
                           AT_OBJ_NAME_PTR(namelist_grp_attr),
                           AT_OBJ_NAME_PTR(attr_idx));
                  namelist_err = TRUE;
               }
            }
            else {
               namelist_err = TRUE;
            }
          
            sn_idx = SN_SIBLING_LINK(sn_idx);
         }

         if (namelist_err) {
            AT_DCL_ERR(namelist_grp_attr) = TRUE;
         }
         else if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Module ||
                  ! taskcommon) {
            /* need to have a way to know if it is referenced */

            create_namelist_descriptor(namelist_grp_attr);
         }
      }
      else if (ATN_NAMELIST_DESC(namelist_grp_attr)) {
         AT_REFERENCED(ATN_NAMELIST_DESC(namelist_grp_attr)) = Referenced;
         ADD_ATTR_TO_LOCAL_LIST(ATN_NAMELIST_DESC(namelist_grp_attr));
         /* check for rename of group name */
      }
 
      namelist_idx = SN_SIBLING_LINK(namelist_idx);

   }

   TRACE (Func_Exit, "namelist_resolution", NULL);

   return;

}  /* namelist_resolution */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine takes a statement and looks for a matching statement in  *|
|*      the bounds list.   If a match is found, it returns the attr_idx of    *|
|*      the matched bound tmp.  If there is no match, a compiler temp is      *|
|*      generated and added to the end of the bounds tmp list.   This assumes *|
|*      that the ir pointed to by ATD_TMP_IDX is always of the form           *|
|*      TMP = ir_stream, so it passes to compare_ir the right operand of      *|
|*      the compiler temp.  And then if a new temp is needed, this routine    *|
|*      generates the TMP =.  An assumption is made that this tmp can never   *|
|*      have more than one statement generated for it.  This is because tmp   *|
|*      stuff called with this routine is always made up of other tmps.       *|
|*      Stuff that goes through here is extents, stride multipliers, and      *|
|*      lengths.                                                              *|
|*									      *|
|* Input parameters:							      *|
|*	opnd   A pointer to an operand pointing to the attribute or ir stream *|
|*	       that needs a temp.  This should NOT have TMP = generated yet.  *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	attr_idx  Index to attr table for this temp.                          *|
|*									      *|
\******************************************************************************/

static int ntr_bnds_sh_tmp_list(opnd_type	*opnd,
                         	int		 no_entry_list,
                         	int		 sh_idx,
			        boolean		 gen_tmp_eq_0,
				int		 type_idx)

{
   int		al_idx;
#ifdef KEY /* Bug 10177 */
   int		attr_idx = 0;
#else /* KEY Bug 10177 */
   int		attr_idx;
#endif /* KEY Bug 10177 */
   int		column;
   int		ir_idx;
   int		line;
   int		prev_al		= NULL_IDX;


   TRACE (Func_Entry, "ntr_bnds_sh_tmp_list", NULL);

   find_opnd_line_and_column(opnd, &line, &column);

   if (SCP_IS_INTERFACE(curr_scp_idx)) {

      /* This is in an interface block - so do not generate statement headers */

      GEN_COMPILER_TMP_ASG(ir_idx, 
                           attr_idx,
                           TRUE,		/* Semantics is done */
                           line,
                           column,
                           type_idx,
                           Priv);

      IR_IDX_R(ATD_TMP_IDX(attr_idx))	= OPND_IDX((*opnd));
      IR_FLD_R(ATD_TMP_IDX(attr_idx))	= OPND_FLD((*opnd));
      IR_LINE_NUM_R(ATD_TMP_IDX(attr_idx)) = line;
      IR_COL_NUM_R(ATD_TMP_IDX(attr_idx))  = column;
      
      AT_REFERENCED(attr_idx)		= Not_Referenced;
      goto EXIT;
   }

   al_idx = SCP_TMP_FW_IDX2(curr_scp_idx);

   while (al_idx != NULL_IDX) {
      attr_idx	= AL_ATTR_IDX(al_idx);

      /* Okay to pass a pointer to the operand here, because it should */
      /* not move.  This is only a call to compare operands.           */

      if (compare_opnds(opnd, &(IR_OPND_R((ATD_TMP_IDX(attr_idx)))))) {
         break;
      }
      prev_al	= al_idx;
      al_idx	= AL_NEXT_IDX(al_idx);
   }

   if (al_idx == NULL_IDX) {  /* At the end of bounds list.  Add new temp. */
      GEN_COMPILER_TMP_ASG(ir_idx, 
                           attr_idx,
                           TRUE,		/* Semantics is done */
                           line,
                           column,
                           type_idx,
                           Priv);

      COPY_OPND(IR_OPND_R(ir_idx), (*opnd));		 /* IR_OPND_R = *opnd */
      SH_IR_IDX(sh_idx)			= ir_idx;

      /* can't assume that the SH_NEXT_IDX(save_sh_idx) is null */
      /* I do assume that sh_idx is a stand alone sh. BHJ */

      if (SH_NEXT_IDX(curr_stmt_sh_idx) != NULL_IDX) {
         SH_NEXT_IDX(sh_idx) = SH_NEXT_IDX(curr_stmt_sh_idx);
         SH_PREV_IDX(SH_NEXT_IDX(sh_idx)) = sh_idx;
      }

      SH_NEXT_IDX(curr_stmt_sh_idx)	= sh_idx;
      SH_PREV_IDX(sh_idx)		= curr_stmt_sh_idx;
      curr_stmt_sh_idx			= sh_idx;

      NTR_ATTR_LIST_TBL(al_idx);
      AL_ATTR_IDX(al_idx)		= attr_idx;

      /* Bounds must always go at the end of the list.  */

      if (prev_al == NULL_IDX) {  /* List is empty.  Add first tmp to list */
         SCP_TMP_FW_IDX2(curr_scp_idx)	= al_idx;
      }
      else {
         AL_NEXT_IDX(prev_al)		= al_idx;
      }

      if (SCP_ENTRY_IDX(curr_scp_idx) != NULL_IDX) {
         ATD_NO_ENTRY_LIST(attr_idx) = merge_entry_lists(NULL_IDX,
                                                         no_entry_list);
         insert_sh_after_entries(attr_idx, 
                                 SH_PREV_IDX(curr_stmt_sh_idx), 
                                 curr_stmt_sh_idx,
                                 gen_tmp_eq_0,
                                 TRUE);     /* Advance ATP_FIRST_SH_IDX */
      }
   }
   else {

      /* If this shared bound is only used in alternate entries and  */
      /* gen_tmp_eq_0 is set, make sure that tmp = 0 gets generated  */
      /* in the entry points where tmp = IR is not generated.        */
      /* ATD_TMP_GEN_ZERO is set once tmp = 0 has been generated.    */

      if (no_entry_list != NULL_IDX && !ATD_TMP_GEN_ZERO(attr_idx)) {
         gen_tmp_eq_zero_ir(attr_idx);
      }

      FREE_SH_NODE(sh_idx);
   }

EXIT:

   TRACE (Func_Exit, "ntr_bnds_sh_tmp_list", NULL);

   return (attr_idx);

}  /* ntr_bnds_sh_tmp_list */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine merges two no entry lists.  The combined list is the     *|
|*      first list.                                                           *|
|*									      *|
|* Input parameters:							      *|
|*	merged_list - Index to the list to have attr added to it.             *|
|*	new_list    - List to add to the merged list.                         *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	 An index to the start of the merged list.                            *|
|*									      *|
\******************************************************************************/
static int merge_entry_lists(int	merged_list,
			     int	new_list)

{
   int		list_idx		= NULL_IDX;
   int		merged_list_start;
#ifdef KEY /* Bug 10177 */
   int		prev_idx = 0;
#else /* KEY Bug 10177 */
   int		prev_idx;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "merge_entry_lists", NULL);

   merged_list_start	= merged_list;

   if (merged_list == NULL_IDX) { /* Just make a new list */

      while (new_list != NULL_IDX) {
         prev_idx	= list_idx;
         NTR_ATTR_LIST_TBL(list_idx);

         if (prev_idx == NULL_IDX) {
            merged_list_start		= list_idx;
            AL_ENTRY_COUNT(merged_list_start)	= AL_ENTRY_COUNT(new_list);
         }
         else {
            AL_NEXT_IDX(prev_idx)	= list_idx;
         }

         AL_ATTR_IDX(list_idx)	= AL_ATTR_IDX(new_list);
         new_list		= AL_NEXT_IDX(new_list);
      }
   }
   else {

      while (new_list != NULL_IDX) {

         list_idx = merged_list;

         while (list_idx != NULL_IDX && 
                AL_ATTR_IDX(new_list) != AL_ATTR_IDX(list_idx)) {
            prev_idx = list_idx;
            list_idx = AL_NEXT_IDX(list_idx);
         }

         /* If list_idx is NULL, the attr was not found on the list, so add  */
         /* the attribute to the bottom of the list.  Prev_idx is pointing   */
         /* to the bottom of the list.                                       */

         if (list_idx == NULL_IDX) {
            NTR_ATTR_LIST_TBL(list_idx);
            AL_NEXT_IDX(prev_idx)	= list_idx;
            AL_ATTR_IDX(list_idx)	= AL_ATTR_IDX(new_list);
            AL_ENTRY_COUNT(merged_list)	+= 1;
         }

         new_list		= AL_NEXT_IDX(new_list);
      }
   }

   TRACE (Func_Exit, "merge_entry_lists", NULL);

   return(merged_list_start);

}  /* merge_entry_lists */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine merges two no entry lists.  The combined list is the     *|
|*      first list.                                                           *|
|*									      *|
|* Input parameters:							      *|
|*	merged_list - Index to the list to have attr added to it.             *|
|*	new_list    - List to add to the merged list.                         *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	 An index to the start of the merged list.                            *|
|*									      *|
\******************************************************************************/
static int merge_entry_list_count(int	merged_list,
			     	  int	new_list)

{
   int		count;
   int		list_idx	= NULL_IDX;


   TRACE (Func_Entry, "merge_entry_list_count", NULL);

   if (merged_list == NULL_IDX) {
      count = (new_list != NULL_IDX) ? AL_ENTRY_COUNT(new_list) : 0;
   }
   else {

      /* Count the different members of the two lists */

      count	= AL_ENTRY_COUNT(merged_list);

      while (new_list != NULL_IDX) {
         list_idx = merged_list;

         while (list_idx != NULL_IDX && 
                AL_ATTR_IDX(new_list) != AL_ATTR_IDX(list_idx)) {
            list_idx = AL_NEXT_IDX(list_idx);
         }

         /* If list_idx is NULL, the attr was not found on the list,   */
         /* so add one to the count.  The assumption is that there are */
         /* never duplicates on a list.                                */

         if (list_idx == NULL_IDX) {
            count++;
         }

         new_list		= AL_NEXT_IDX(new_list);
      }
   }

   TRACE (Func_Exit, "merge_entry_list_count", NULL);

   return(count);

}  /* merge_entry_list_count */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This returns TRUE if the entry point is NOT in the NO_ENTRY_LIST      *|
|*      for the given attr.                                                   *|
|*									      *|
|* Input parameters:							      *|
|*	entry_attr  - Entry point attr to check                               *|
|*	attr_idx    - Attr_idx with list.                                     *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	 FALSE if the entry attr is on the list.                              *|
|*									      *|
\******************************************************************************/
static boolean gen_ir_at_this_entry(int	  entry_attr,
				    int   attr_idx)

{
   boolean	not_in_list	= TRUE;
   int		list_idx;


   TRACE (Func_Entry, "gen_ir_at_this_entry", NULL);

   list_idx	= (ATD_CLASS(attr_idx) == Function_Result) ?
                   ATP_NO_ENTRY_LIST(ATD_FUNC_IDX(attr_idx)) :
                   ATD_NO_ENTRY_LIST(attr_idx);

   while (list_idx != NULL_IDX) {

      if (AL_ATTR_IDX(list_idx) == entry_attr) {
         not_in_list = FALSE;
         break;
      }

      list_idx = AL_NEXT_IDX(list_idx);
   }

   TRACE (Func_Exit, "gen_ir_at_this_entry", NULL);

   return(not_in_list);

}  /* gen_ir_at_this_entry */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This generates tmp code at entry points.  It works off of             *|
|*	ATD_NO_ENTRY_LIST for the tmp.  If gen_tmp_eq_0 is TRUE, tmp = 0      *|
|*	is generated at those entry points where the IR cannot be generated.  *|
|*	NOTE: There is an assumption that if gen_tmp_eq_0 is TRUE, there is   *|
|*	      only one SH for this bound.  There is a debug check for this.   *|
|*            If there were multiple SH's, we wouldn't know which one to      *|
|*            replace with the tmp = 0.                                       *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx     - Attr index of tmp (or the attr to use for the          *|
|*                     ATD_NO_ENTRY_LIST and AT_OPTIONAL.                     *|
|*	start_sh_idx - SH index to index BEFORE the first SH to be copied.    *|
|*	end_sh_idx   - SH index of last SH to be copied.                      *|
|*	gen_tmp_eq_0 - If TRUE, need tmp = 0, gen'd where tmp = IR can't be.  *|
|*      advance_first_sh - If TRUE, advance ATP_FIRST_SH_IDX, else don't.     *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	 NONE								      *|
|*									      *|
\******************************************************************************/
static	void	insert_sh_after_entries(int		attr_idx,
					int		start_sh_idx,
                                        int		end_sh_idx,
					boolean		gen_tmp_eq_0,
                                        boolean         advance_first_sh)
{
   boolean	bump_curr_sh;
   int		entry_attr_idx;
   int		entry_list_idx;
   int		entry_sh_idx;
   int		ir_idx;
   int		new_start_sh_idx;
   int		new_end_sh_idx;
   int		next_sh_idx;
   int		no_entry_list;
   int		save_curr_sh_idx;
   int		sh_idx;


   TRACE (Func_Entry, "insert_sh_after_entries", NULL);

   if (SH_NEXT_IDX(start_sh_idx) == NULL_IDX) {
      return;		   /* Nothing to add */
   }

   entry_list_idx	= SCP_ENTRY_IDX(curr_scp_idx);

   no_entry_list	= (ATD_CLASS(attr_idx) == Function_Result) ?
                          ATP_NO_ENTRY_LIST(ATD_FUNC_IDX(attr_idx)) :
                          ATD_NO_ENTRY_LIST(attr_idx);

   while (entry_list_idx != NULL_IDX) {
      entry_attr_idx	= AL_ATTR_IDX(entry_list_idx);

      if (no_entry_list == NULL_IDX ||
          gen_ir_at_this_entry(entry_attr_idx, attr_idx)) {
         entry_sh_idx		= ATP_FIRST_SH_IDX(entry_attr_idx);
         next_sh_idx		= SH_NEXT_IDX(entry_sh_idx);

         copy_entry_exit_sh_list(SH_NEXT_IDX(start_sh_idx),
                                 end_sh_idx,
                                 &new_start_sh_idx,
                                 &new_end_sh_idx);

         if (new_start_sh_idx != NULL_IDX) {
            SH_NEXT_IDX(entry_sh_idx)     = new_start_sh_idx;
            SH_PREV_IDX(new_start_sh_idx) = entry_sh_idx;

            entry_sh_idx = new_end_sh_idx;

            SH_PREV_IDX(next_sh_idx)       = entry_sh_idx;
            SH_NEXT_IDX(entry_sh_idx)      = next_sh_idx;

            if (AT_OPTIONAL(attr_idx)) {
               gen_present_ir(attr_idx, 
                              SH_NEXT_IDX(ATP_FIRST_SH_IDX(entry_attr_idx)), 
                              entry_sh_idx);
               entry_sh_idx	= SH_NEXT_IDX(entry_sh_idx);
            }

            if (advance_first_sh) {
               ATP_FIRST_SH_IDX(entry_attr_idx)    = entry_sh_idx;
            }
         }
      }

      else if (gen_tmp_eq_0) {

         /* This tmp is used to generate a length.  If the length can't be */
         /* calculated at this entry point, generate  tmp = 0              */

         save_curr_sh_idx	= curr_stmt_sh_idx;
         curr_stmt_sh_idx	= ATP_FIRST_SH_IDX(entry_attr_idx);

         /* Find Entry_Opr */

         while (IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) != Entry_Opr) {
            curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
         }

         gen_sh(After, 
                Assignment_Stmt,
                SH_GLB_LINE(curr_stmt_sh_idx),
                SH_COL_NUM(curr_stmt_sh_idx),
                FALSE,  /* Err flag           */
                FALSE,  /* labeled            */
                TRUE);  /* Compiler generated */

         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(curr_stmt_sh_idx)		= ir_idx;

         COPY_TBL_NTRY(ir_tbl, ir_idx, ATD_TMP_IDX(attr_idx));

         IR_FLD_R(ir_idx)			= CN_Tbl_Idx;
         IR_IDX_R(ir_idx)			= CN_INTEGER_ZERO_IDX;
         IR_LINE_NUM_R(ir_idx) = SH_GLB_LINE(curr_stmt_sh_idx);
         IR_COL_NUM_R(ir_idx)  = SH_COL_NUM(curr_stmt_sh_idx);

         ATD_TMP_GEN_ZERO(attr_idx)		= TRUE;

         /* ignore the advance_first_sh flag, not needed here */

         if (IR_OPR(SH_IR_IDX(ATP_FIRST_SH_IDX(entry_attr_idx))) == Entry_Opr) {
            ATP_FIRST_SH_IDX(entry_attr_idx) = curr_stmt_sh_idx;
         }

         curr_stmt_sh_idx			= save_curr_sh_idx;
      }
      entry_list_idx	= AL_NEXT_IDX(entry_list_idx);
   }

   end_sh_idx		= SH_NEXT_IDX(end_sh_idx);

   if (no_entry_list != NULL_IDX &&
       !gen_ir_at_this_entry(SCP_ATTR_IDX(curr_scp_idx), attr_idx)) {

      /* At this point, start_sh_idx points to the stmt header BEFORE   */
      /* the first stmt header to delete or replace.  end_sh_idx points */
      /* to the stmt header after the last statement to delete.         */

      /* Remove it from the main entry, if it doesn't belong here. */

      sh_idx			= SH_NEXT_IDX(start_sh_idx);
      curr_stmt_sh_idx		= start_sh_idx;

      do {
         next_sh_idx		= SH_NEXT_IDX(sh_idx);
         FREE_SH_NODE(sh_idx);
         sh_idx			= next_sh_idx;
      }
      while (sh_idx != end_sh_idx);

      SH_NEXT_IDX(start_sh_idx)	= end_sh_idx;

      if (end_sh_idx != NULL_IDX) {
         SH_PREV_IDX(end_sh_idx)	= start_sh_idx;
      }

      if (gen_tmp_eq_0) {

         /* Insert tmp = 0, but these must be inserted first after the  */
         /* Entry_Opr because variable length character functions have  */
         /* their length temps equivalenced.  Thus we have to make sure */
         /* that tmp1 = 0 happens before tmp2 = I in case tmp1 and tmp2 */
         /* are equivalenced together.                                  */

         save_curr_sh_idx	= curr_stmt_sh_idx;
         bump_curr_sh		= TRUE;

         /* Find Entry_Opr */

         while (IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) != Entry_Opr) {
            curr_stmt_sh_idx	= SH_PREV_IDX(curr_stmt_sh_idx);
            bump_curr_sh	= FALSE;
         }

         gen_sh(After, 
                Assignment_Stmt,
                SH_GLB_LINE(curr_stmt_sh_idx),
                SH_COL_NUM(curr_stmt_sh_idx),
                FALSE,  /* Err flag           */
                FALSE,  /* labeled            */
                TRUE);  /* Compiler generated */

         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(curr_stmt_sh_idx)	= ir_idx;
         IR_OPR(ir_idx)			= Asg_Opr;
         IR_TYPE_IDX(ir_idx)            = ATD_TYPE_IDX(attr_idx);
         IR_LINE_NUM(ir_idx)		= AT_DEF_LINE(attr_idx);
         IR_COL_NUM(ir_idx)		= AT_DEF_COLUMN(attr_idx);
         IR_FLD_L(ir_idx)		= AT_Tbl_Idx;
         IR_IDX_L(ir_idx)		= attr_idx;
         IR_LINE_NUM_L(ir_idx)		= AT_DEF_LINE(attr_idx);
         IR_COL_NUM_L(ir_idx)		= AT_DEF_COLUMN(attr_idx);
         IR_LINE_NUM_R(ir_idx)		= AT_DEF_LINE(attr_idx);
         IR_COL_NUM_R(ir_idx)		= AT_DEF_COLUMN(attr_idx);
         IR_FLD_R(ir_idx)		= CN_Tbl_Idx;
         IR_IDX_R(ir_idx)		= CN_INTEGER_ZERO_IDX;
         ATD_TMP_GEN_ZERO(attr_idx)	= TRUE;

         curr_stmt_sh_idx = (bump_curr_sh) ? SH_NEXT_IDX(save_curr_sh_idx) :
                                             save_curr_sh_idx;
      }
   }
   else if (AT_OPTIONAL(attr_idx)) {
      gen_present_ir(attr_idx, 
                     SH_NEXT_IDX(start_sh_idx), 
                     curr_stmt_sh_idx);
      curr_stmt_sh_idx	= SH_NEXT_IDX(curr_stmt_sh_idx);
   }

   TRACE (Func_Exit, "insert_sh_after_entries", NULL);

   return;

}  /* insert_sh_after_entries */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This generated tmp = 0 IR for entry points, where tmp = IR cannot be  *|
|*      generated.  This assumes that TMP=IR has been generated previously.   *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx    - Attr_idx of tmp.                                        *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	 NONE								      *|
|*									      *|
\******************************************************************************/
static	void	gen_tmp_eq_zero_ir(int		attr_idx)
{
   int		entry_attr_idx;
   int		entry_list_idx;
   int		entry_sh_idx;
   int		ir_idx;
   int		next_sh_idx;
   int		new_sh_idx;


   TRACE (Func_Entry, "gen_tmp_eq_zero_ir", NULL);

   entry_list_idx	= SCP_ENTRY_IDX(curr_scp_idx);

   while (entry_list_idx != NULL_IDX) {
      entry_attr_idx	= AL_ATTR_IDX(entry_list_idx);

      if (!gen_ir_at_this_entry(entry_attr_idx, attr_idx)) {
         entry_sh_idx = ATP_FIRST_SH_IDX(entry_attr_idx);
         next_sh_idx				= SH_NEXT_IDX(entry_sh_idx);
         new_sh_idx				= ntr_sh_tbl();
         SH_NEXT_IDX(entry_sh_idx)		= new_sh_idx;
         SH_NEXT_IDX(new_sh_idx)		= next_sh_idx;
         SH_PREV_IDX(new_sh_idx)		= entry_sh_idx;
         SH_PREV_IDX(next_sh_idx)		= new_sh_idx;
         SH_STMT_TYPE(new_sh_idx)		= Automatic_Base_Size_Stmt;
         SH_GLB_LINE(new_sh_idx)		= AT_DEF_LINE(attr_idx);
         SH_COL_NUM(new_sh_idx)			= AT_DEF_COLUMN(attr_idx);
         SH_COMPILER_GEN(new_sh_idx)		= TRUE;
         SH_P2_SKIP_ME(new_sh_idx)		= TRUE;
         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(new_sh_idx)			= ir_idx;
         IR_OPR(ir_idx)				= Asg_Opr;
         IR_TYPE_IDX(ir_idx)                    = ATD_TYPE_IDX(attr_idx);
         IR_FLD_L(ir_idx)			= AT_Tbl_Idx;
         IR_IDX_L(ir_idx)			= attr_idx;
         IR_FLD_R(ir_idx)			= CN_Tbl_Idx;
         IR_IDX_R(ir_idx)			= CN_INTEGER_ZERO_IDX;
         IR_LINE_NUM_L(ir_idx)			= AT_DEF_LINE(attr_idx);
         IR_LINE_NUM_R(ir_idx)			= AT_DEF_LINE(attr_idx);
         IR_LINE_NUM(ir_idx)			= AT_DEF_LINE(attr_idx);
         IR_COL_NUM_L(ir_idx)			= AT_DEF_COLUMN(attr_idx);
         IR_COL_NUM_R(ir_idx)			= AT_DEF_COLUMN(attr_idx);
         IR_COL_NUM(ir_idx)			= AT_DEF_COLUMN(attr_idx);
         ATD_TMP_GEN_ZERO(attr_idx)		= TRUE;
         ATP_FIRST_SH_IDX(entry_attr_idx)	= new_sh_idx;
         ATD_TMP_GEN_ZERO(attr_idx)		= TRUE;
      }
      entry_list_idx	= AL_NEXT_IDX(entry_list_idx);
   }

   if (!gen_ir_at_this_entry(SCP_ATTR_IDX(curr_scp_idx), attr_idx)) {
      new_sh_idx			= ntr_sh_tbl();
      SH_NEXT_IDX(curr_stmt_sh_idx)	= new_sh_idx;
      SH_PREV_IDX(new_sh_idx)		= curr_stmt_sh_idx;
      SH_STMT_TYPE(new_sh_idx)		= Automatic_Base_Size_Stmt;
      SH_GLB_LINE(new_sh_idx)		= AT_DEF_LINE(attr_idx);
      SH_COL_NUM(new_sh_idx)		= AT_DEF_COLUMN(attr_idx);
      SH_COMPILER_GEN(new_sh_idx)	= TRUE;
      SH_P2_SKIP_ME(new_sh_idx)		= TRUE;
      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(new_sh_idx)		= ir_idx;
      IR_OPR(ir_idx)			= Asg_Opr;
      IR_TYPE_IDX(ir_idx)               = ATD_TYPE_IDX(attr_idx);
      IR_FLD_L(ir_idx)			= AT_Tbl_Idx;
      IR_IDX_L(ir_idx)			= attr_idx;
      IR_FLD_R(ir_idx)			= CN_Tbl_Idx;
      IR_IDX_R(ir_idx)			= CN_INTEGER_ZERO_IDX;
      IR_LINE_NUM_L(ir_idx)		= AT_DEF_LINE(attr_idx);
      IR_LINE_NUM_R(ir_idx)		= AT_DEF_LINE(attr_idx);
      IR_LINE_NUM(ir_idx)		= AT_DEF_LINE(attr_idx);
      IR_COL_NUM_L(ir_idx)		= AT_DEF_COLUMN(attr_idx);
      IR_COL_NUM_R(ir_idx)		= AT_DEF_COLUMN(attr_idx);
      IR_COL_NUM(ir_idx)		= AT_DEF_COLUMN(attr_idx);
      ATD_TMP_GEN_ZERO(attr_idx)	= TRUE;
      curr_stmt_sh_idx			= new_sh_idx;
   }

   TRACE (Func_Exit, "gen_tmp_eq_zero_ir", NULL);

   return;

}  /* gen_tmp_eq_zero_ir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This generates if PRESENT code for optional dummy arguments.          *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx     - Attr_idx of darg that is optional                      *|
|*	start_sh_idx - Index to start of IR to have an if present put around  *|
|*	end_sh_idx   - Index to end of IR to have an if present put around    *|
|*                     This gets updated to point to the new last sh idx.     *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	 NONE								      *|
|*									      *|
\******************************************************************************/
#ifdef KEY /* Bug 4955, 6845 */
/* Return SH_IDX for last statement generated */
int	gen_present_ir(int	attr_idx,
			       int	start_sh_idx,
                               int	end_sh_idx)
#else /* KEY Bug 4955, 6845 */
static	void	gen_present_ir(int	attr_idx,
			       int	start_sh_idx,
                               int	end_sh_idx)
#endif /* KEY Bug 4955, 6845 */
{
   int		br_around_opt;
   int		br_idx;
   int		cont_idx;
   int		present_idx;
   int		not_idx;
   int		save_sh_idx;


   TRACE (Func_Entry, "gen_present_ir", NULL);

   save_sh_idx		= curr_stmt_sh_idx;
   curr_stmt_sh_idx	= start_sh_idx;

   gen_sh(Before, 
          Goto_Stmt,
          SH_GLB_LINE(start_sh_idx),
          SH_COL_NUM(start_sh_idx),
          FALSE,
          FALSE,
          TRUE);

   SH_P2_SKIP_ME(SH_PREV_IDX(start_sh_idx))	= TRUE;

   br_around_opt	= gen_internal_lbl(stmt_start_line);

   NTR_IR_TBL(br_idx);
   NTR_IR_TBL(present_idx);
   NTR_IR_TBL(not_idx);

   IR_OPR(br_idx)	= Br_True_Opr;
   IR_OPR(not_idx)	= Not_Opr;
   IR_OPR(present_idx)	= Present_Opr;
   IR_TYPE_IDX(present_idx) = LOGICAL_DEFAULT_TYPE;
   IR_TYPE_IDX(br_idx)  = LOGICAL_DEFAULT_TYPE;
   IR_TYPE_IDX(not_idx)  = LOGICAL_DEFAULT_TYPE;

   SH_IR_IDX(SH_PREV_IDX(start_sh_idx))	= br_idx;
   IR_LINE_NUM(br_idx)		= AT_DEF_LINE(attr_idx);
   IR_COL_NUM(br_idx)		= AT_DEF_COLUMN(attr_idx);
   IR_LINE_NUM(not_idx)		= AT_DEF_LINE(attr_idx);
   IR_COL_NUM(not_idx)		= AT_DEF_COLUMN(attr_idx);
   IR_LINE_NUM(present_idx)	= AT_DEF_LINE(attr_idx);
   IR_COL_NUM(present_idx)	= AT_DEF_COLUMN(attr_idx);

   IR_FLD_R(br_idx)		= AT_Tbl_Idx;
   IR_IDX_R(br_idx)		= br_around_opt;
   IR_COL_NUM_R(br_idx)		= AT_DEF_COLUMN(attr_idx);
   IR_LINE_NUM_R(br_idx)	= AT_DEF_LINE(attr_idx);

   IR_FLD_L(br_idx)		= IR_Tbl_Idx;
   IR_IDX_L(br_idx)		= not_idx;

   IR_FLD_L(not_idx)		= IR_Tbl_Idx;
   IR_IDX_L(not_idx)		= present_idx;

   IR_FLD_L(present_idx)	= AT_Tbl_Idx;
   IR_IDX_L(present_idx)	= attr_idx;
   IR_COL_NUM_L(present_idx)	= AT_DEF_COLUMN(attr_idx);
   IR_LINE_NUM_L(present_idx)	= AT_DEF_LINE(attr_idx);

   NTR_IR_TBL(cont_idx);
   IR_OPR(cont_idx)		= Label_Opr;
   IR_TYPE_IDX(cont_idx)        = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(cont_idx)	= AT_DEF_LINE(attr_idx);
   IR_COL_NUM(cont_idx)		= AT_DEF_COLUMN(attr_idx);
   IR_IDX_L(cont_idx)		= br_around_opt;
   IR_FLD_L(cont_idx)		= AT_Tbl_Idx;
   IR_LINE_NUM_L(cont_idx)	= AT_DEF_LINE(attr_idx);
   IR_COL_NUM_L(cont_idx)	= AT_DEF_COLUMN(attr_idx);
   curr_stmt_sh_idx		= end_sh_idx;

   gen_sh(After, 
          Continue_Stmt,
          SH_GLB_LINE(end_sh_idx),
          SH_COL_NUM(end_sh_idx),
          FALSE,
          TRUE,
          TRUE);

   SH_P2_SKIP_ME(curr_stmt_sh_idx)	= TRUE;
   SH_IR_IDX(curr_stmt_sh_idx)		= cont_idx;
#ifdef KEY /* Bug 6845 */
   int last_idx = curr_stmt_sh_idx;
#endif /* KEY Bug 6845 */
   curr_stmt_sh_idx			= save_sh_idx;

   TRACE (Func_Exit, "gen_present_ir", NULL);

#ifdef KEY /* Bug 6845 */
   return last_idx;
#else /* KEY Bug 6845 */
   return;
#endif /* KEY Bug 6845 */

}  /* gen_present_ir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*									      *|
|* Input parameters:							      *|
|*	 ir_idx => ir to check						      *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	 NONE								      *|
|*									      *|
\******************************************************************************/
static	void	tmp_ir_resolution(int	ir_idx)
{

   TRACE (Func_Entry, "tmp_ir_resolution", NULL);


   switch (IR_FLD_L(ir_idx)) {

   case AT_Tbl_Idx:
      attr_semantics(IR_IDX_L(ir_idx), FALSE);
      break;

   case IR_Tbl_Idx:
      tmp_ir_resolution(IR_IDX_L(ir_idx));
      break;

   case IL_Tbl_Idx:
      tmp_il_resolution(IR_IDX_L(ir_idx));
      break;
   }


   switch (IR_FLD_R(ir_idx)) {

   case AT_Tbl_Idx:
      attr_semantics(IR_IDX_R(ir_idx), FALSE);
      break;

   case IR_Tbl_Idx:
      tmp_ir_resolution(IR_IDX_R(ir_idx));
      break;

   case IL_Tbl_Idx:
      tmp_il_resolution(IR_IDX_R(ir_idx));
      break;
   }

   TRACE (Func_Exit, "tmp_ir_resolution", NULL);

   return;

}  /* tmp_ir_resolution */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*									      *|
|* Input parameters:							      *|
|*	 list_idx => il to check					      *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	 NONE								      *|
|*									      *|
\******************************************************************************/
static	void	tmp_il_resolution(int	list_idx)
{

   TRACE (Func_Entry, "tmp_il_resolution", NULL);

   while (list_idx != NULL_IDX) {

      switch (IL_FLD(list_idx)) {

      case AT_Tbl_Idx:
         attr_semantics(IL_IDX(list_idx), FALSE);
         break;

      case IR_Tbl_Idx:
         tmp_ir_resolution(IL_IDX(list_idx));
         break;

      case IL_Tbl_Idx:
         tmp_il_resolution(IL_IDX(list_idx));
         break;

      }
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   TRACE (Func_Exit, "tmp_il_resolution", NULL);

   return;

}  /* tmp_il_resolution */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Go through the list pointed to by allocatable_list_idx and set up a   *|
|*      call to _DEALLOC for all the local allocatable arrays.                *|
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
#ifdef KEY /* Bug 6845 */
static void deallocate_local_allocatables(void)

{
   /* The "Bug 6845" version handles local variables which are derived types
    * containing components which are allocatable; but it assumes that
    * _SEPARATE_DEALLOCATES==true and _ALLOCATE_IS_CALL==false. */

   TRACE (Func_Entry, "deallocate_local_allocatables", NULL);

   int line = stmt_start_line;
   int col  = stmt_start_col;
   int save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   ADD_ATTR_TO_LOCAL_LIST(lazy_create_dealloc(line, col));

   boolean first = TRUE;
   int start_sh_idx                  = ntr_sh_tbl();
   curr_stmt_sh_idx                  = start_sh_idx;
   SH_STMT_TYPE(curr_stmt_sh_idx)    = Assignment_Stmt;
   SH_GLB_LINE(curr_stmt_sh_idx)     = line;
   SH_COL_NUM(curr_stmt_sh_idx)      = col;
   SH_COMPILER_GEN(curr_stmt_sh_idx) = TRUE;
   SH_P2_SKIP_ME(curr_stmt_sh_idx)   = TRUE;

   for (int sn_idx = allocatable_list_idx;
      sn_idx;
      sn_idx = SN_SIBLING_LINK(sn_idx)) {
      int sn_attr_idx = SN_ATTR_IDX(sn_idx);
      int has_pe_ref = (ATD_ALLOCATABLE(sn_attr_idx) &&
        ATD_PE_ARRAY_IDX(sn_attr_idx) != NULL_IDX);
      dealloc_allocatables(line, col, sn_attr_idx, AT_Tbl_Idx, sn_attr_idx,
        has_pe_ref, &first);
   }

   while (SH_PREV_IDX(start_sh_idx)) {
      start_sh_idx = SH_PREV_IDX(start_sh_idx);
   }

   if (SH_NEXT_IDX(curr_stmt_sh_idx)) {
      curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
   }

   if (SCP_EXIT_IR_SH_IDX(curr_scp_idx) != NULL_IDX) {
      SH_NEXT_IDX(curr_stmt_sh_idx)	= SCP_EXIT_IR_SH_IDX(curr_scp_idx);
      SCP_EXIT_IR_SH_IDX(curr_scp_idx)	= start_sh_idx;
   }
   else {
      SCP_EXIT_IR_SH_IDX(curr_scp_idx)	= start_sh_idx;
   }

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "deallocate_local_allocatables", NULL);

   return;

}  /* deallocate_local_allocatables */

#else /* KEY Bug 6845 */
static void deallocate_local_allocatables(void)

{
   int          asg_idx;
   int		cn_idx;
   int		col;
   boolean      has_normal_ref	= FALSE;
   boolean      has_pe_ref	= FALSE;
   int		line;
   int          list_idx;
   int		loc_idx;
   int		save_curr_stmt_sh_idx;
   int		sn_idx;
   int		start_sh_idx;


   TRACE (Func_Entry, "deallocate_local_allocatables", NULL);

   line = stmt_start_line;
   col  = stmt_start_col;
   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Dealloc_Attr_Idx]);

# ifdef _SEPARATE_DEALLOCATES
   sn_idx = allocatable_list_idx;

   start_sh_idx                 = ntr_sh_tbl();
   curr_stmt_sh_idx             = start_sh_idx;

   SH_STMT_TYPE(curr_stmt_sh_idx)    = Assignment_Stmt;
   SH_GLB_LINE(curr_stmt_sh_idx)     = line;
   SH_COL_NUM(curr_stmt_sh_idx)      = col;
   SH_COMPILER_GEN(curr_stmt_sh_idx) = TRUE;
   SH_P2_SKIP_ME(curr_stmt_sh_idx)   = TRUE;

   while (sn_idx) {

      if (ATD_ALLOCATABLE(SN_ATTR_IDX(sn_idx)) &&
          ATD_PE_ARRAY_IDX(SN_ATTR_IDX(sn_idx)) != NULL_IDX) {
         has_pe_ref = TRUE;
      }
      else {
         has_pe_ref = FALSE;
      }

      NTR_IR_LIST_TBL(list_idx);
      asg_idx = gen_ir(IL_Tbl_Idx, list_idx,
                   Deallocate_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                       NO_Tbl_Idx, NULL_IDX);

      loc_idx = gen_ir(AT_Tbl_Idx, SN_ATTR_IDX(sn_idx),
                   Aloc_Opr, CRI_Ptr_8, line, col,
                       NO_Tbl_Idx, NULL_IDX);

      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = loc_idx;

      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;

# ifdef _ALLOCATE_IS_CALL
      set_up_allocate_as_call(asg_idx,
                              glb_tbl_idx[Dealloc_Attr_Idx],
                              NULL_IDX,
                              has_pe_ref);
# else

      list_idx = gen_il(3, FALSE, line, col,
                        AT_Tbl_Idx, glb_tbl_idx[Dealloc_Attr_Idx],
                        CN_Tbl_Idx, gen_alloc_header_const(Integer_8,
                                                           1,
                                                           has_pe_ref,
                                                           &cn_idx),
                        CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);
      IR_FLD_R(asg_idx) = IL_Tbl_Idx;
      IR_IDX_R(asg_idx) = list_idx;
      IR_LIST_CNT_R(asg_idx) = 3;
# endif


      sn_idx = SN_SIBLING_LINK(sn_idx);

      if (sn_idx) {
         gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      }
   }

# else

   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Deallocate_Opr;
   IR_TYPE_IDX(asg_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx)  = col;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_L(asg_idx) = IL_Tbl_Idx;
   IR_IDX_L(asg_idx) = list_idx;
   IR_LIST_CNT_L(asg_idx) = number_of_allocatables;

   sn_idx = allocatable_list_idx;

   while (sn_idx) {

      if (ATD_ALLOCATABLE(SN_ATTR_IDX(sn_idx)) &&
          ATD_PE_ARRAY_IDX(SN_ATTR_IDX(sn_idx)) != NULL_IDX) {
         has_pe_ref = TRUE;
      }
      else {
         has_normal_ref = TRUE;
      }

      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx)             = Aloc_Opr;
      IR_TYPE_IDX(loc_idx)        = CRI_Ptr_8;
      IR_FLD_L(loc_idx)           = AT_Tbl_Idx;
      IR_IDX_L(loc_idx)           = SN_ATTR_IDX(sn_idx);
      IR_LINE_NUM(loc_idx)        = line;
      IR_COL_NUM(loc_idx)         = col;
      IR_LINE_NUM_L(loc_idx)      = line;
      IR_COL_NUM_L(loc_idx)       = col;
      IL_FLD(list_idx)            = IR_Tbl_Idx;
      IL_IDX(list_idx)            = loc_idx;

      sn_idx = SN_SIBLING_LINK(sn_idx);

      if (sn_idx) {
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   start_sh_idx                 = ntr_sh_tbl();
   curr_stmt_sh_idx             = start_sh_idx;

   SH_STMT_TYPE(curr_stmt_sh_idx)       = Assignment_Stmt;
   SH_GLB_LINE(curr_stmt_sh_idx)        = line;
   SH_COL_NUM(curr_stmt_sh_idx)         = col;
   SH_COMPILER_GEN(curr_stmt_sh_idx)    = TRUE;
   SH_P2_SKIP_ME(curr_stmt_sh_idx)      = TRUE;

   SH_IR_IDX(curr_stmt_sh_idx)          = asg_idx;

   if (has_pe_ref && has_normal_ref) {
      /* must pull the normal refs off on their own call */
      gen_split_alloc(asg_idx,
                      glb_tbl_idx[Dealloc_Attr_Idx],
                      NULL_IDX);
   }


# ifdef _ALLOCATE_IS_CALL
   set_up_allocate_as_call(asg_idx,
                           glb_tbl_idx[Dealloc_Attr_Idx],
                           NULL_IDX,
                           has_pe_ref);
# else
   list_idx = gen_il(3, FALSE, line, col,
                     AT_Tbl_Idx, glb_tbl_idx[Dealloc_Attr_Idx],
                     CN_Tbl_Idx, 
                         gen_alloc_header_const(Integer_8,
                                                number_of_allocatables, 
                                                has_pe_ref,
                                                &cn_idx),
                     CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);
   IR_FLD_R(asg_idx) = IL_Tbl_Idx;
   IR_IDX_R(asg_idx) = list_idx;
   IR_LIST_CNT_R(asg_idx) = 3;

# endif
# endif

   while (SH_PREV_IDX(start_sh_idx)) {
      start_sh_idx = SH_PREV_IDX(start_sh_idx);
   }

   if (SH_NEXT_IDX(curr_stmt_sh_idx)) {
      curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
   }

   if (SCP_EXIT_IR_SH_IDX(curr_scp_idx) != NULL_IDX) {
      SH_NEXT_IDX(curr_stmt_sh_idx)	= SCP_EXIT_IR_SH_IDX(curr_scp_idx);
      SCP_EXIT_IR_SH_IDX(curr_scp_idx)	= start_sh_idx;
   }
   else {
      SCP_EXIT_IR_SH_IDX(curr_scp_idx)	= start_sh_idx;
   }

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "deallocate_local_allocatables", NULL);

   return;

}  /* deallocate_local_allocatables */
#endif /* KEY Bug 6845 */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      darg_in_entry_list searches the secondary name table entries of       *|
|*      an explicit interface for an attr.  The entries in the secondary      *|
|*      name table must be in sequential order.                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*	srch_idx	Attribute index to search for.            	      *|
|*      entry_idx	Attribute index of entry that contains the list of    *|
|*                      dummy arguments to search.                            *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                      				              *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if this darg's attr is in the entry list, otherwise FALSE        *|
|*                                                                            *|
\******************************************************************************/
static	boolean darg_in_entry_list (int		srch_idx,
          	                    int		entry_idx)
{
   register int          i;
   register boolean	 matched	= FALSE;
   register int          member_cnt;
   register long        *sn_tbl_base;


   TRACE (Func_Entry, "darg_in_entry_list", NULL);

   member_cnt	= ATP_NUM_DARGS(entry_idx);

#ifdef _HOST_LITTLE_ENDIAN
   /* found by PV 778027 */

   for (i = ATP_FIRST_IDX(entry_idx);
        i < ATP_FIRST_IDX(entry_idx) + member_cnt;
        i++) {
     if (SN_ATTR_IDX(i) == srch_idx) {
        matched = TRUE;
        break;
     }
   } /* for i */
#else

   sn_tbl_base	= (long *) (sec_name_tbl + ATP_FIRST_IDX(entry_idx)) + 
                                           (NUM_SN_WDS - 1);

#  pragma _CRI ivdep

   for (i = 0; i < member_cnt; i++) {

      if ((sn_tbl_base[0] & 077777777) == srch_idx) {
         matched = TRUE;
         break;
      }
      sn_tbl_base	= sn_tbl_base + NUM_SN_WDS;
   }

#endif

   TRACE (Func_Exit, "darg_in_entry_list", NULL);

   return (matched); 

}  /*  darg_in_entry_list  */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This generates ir to change a byte length into a word aligned length  *|
|*      The new length is a word length.                                      *|
|*                                                                            *|
|* Input/Output parameters:                                                   *|
|*      len_opnd   Operand containing the length to be converted.  Should     *|
|*                 have a valid line and column number.  At return, len_opnd  *|
|*		   contains the new word aligned word length.                 *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                      				              *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      The new length in len opnd.                                           *|
|*                                                                            *|
\******************************************************************************/
# if defined(_TARGET_WORD_ADDRESS) ||  \
     (defined(_HEAP_REQUEST_IN_WORDS) && defined(_TARGET_BYTE_ADDRESS))
static	void gen_word_align_byte_length_ir(opnd_type	*len_opnd)
{
   int		column;
   int		div_idx;
   int		line;
   int		paren_idx;
   int		plus_idx;
   int		type_idx;


   TRACE (Func_Entry, "gen_word_align_byte_length_ir", NULL);

   line		= OPND_LINE_NUM((*len_opnd));
   column	= OPND_COL_NUM((*len_opnd));

   NTR_IR_TBL(div_idx);
   NTR_IR_TBL(paren_idx);
   NTR_IR_TBL(plus_idx);
   IR_LINE_NUM(div_idx)		= line;
   IR_COL_NUM(div_idx)		= column;
   IR_LINE_NUM(paren_idx)	= line;
   IR_COL_NUM(paren_idx)	= column;
   IR_LINE_NUM(plus_idx)	= line;
   IR_COL_NUM(plus_idx)		= column;

   type_idx			= check_type_for_size_address(len_opnd);

   COPY_OPND(IR_OPND_L(plus_idx), (*len_opnd));

   /* Div_Opr    (Left is paren IR, Right is number of bytes per word) */

   IR_OPR(div_idx)		= Div_Opr;
   IR_TYPE_IDX(div_idx)		= type_idx;
   IR_FLD_L(div_idx)		= IR_Tbl_Idx;
   IR_IDX_L(div_idx)		= paren_idx;
   IR_FLD_R(div_idx)		= CN_Tbl_Idx;
   IR_IDX_R(div_idx)		= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              TARGET_BYTES_PER_WORD);
   IR_LINE_NUM_R(div_idx)	= line;
   IR_COL_NUM_R(div_idx)	= column;

   /* Paren_Opr  (Left is plus IR, Right is NULL) */

   IR_OPR(paren_idx)		= Paren_Opr;
   IR_TYPE_IDX(div_idx)		= type_idx;
   IR_TYPE_IDX(paren_idx)	= type_idx;
   IR_FLD_L(paren_idx)		= IR_Tbl_Idx;
   IR_IDX_L(paren_idx)		= plus_idx;
   IR_LINE_NUM_L(paren_idx)	= line;
   IR_COL_NUM_L(paren_idx)	= column;

   /* Plus_Opr  (Left is num of bytes, Right is (word byte size - 1)) */

   IR_OPR(plus_idx)		= Plus_Opr;
   IR_TYPE_IDX(div_idx)		= type_idx;
   IR_LINE_NUM_R(plus_idx)	= line;
   IR_COL_NUM_R(plus_idx)	= column;
   IR_FLD_R(plus_idx)		= CN_Tbl_Idx;
   IR_IDX_R(plus_idx)		= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              TARGET_BYTES_PER_WORD - 1);
   OPND_FLD((*len_opnd))	= IR_Tbl_Idx;
   OPND_IDX((*len_opnd))	= div_idx;

   TRACE (Func_Exit, "gen_word_align_byte_length_ir", NULL);

   return;

}  /* gen_word_align_byte_length_ir */
# endif

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This names a new equiv block.                                         *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                      				              *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      The new blocks index.                                                 *|
|*                                                                            *|
\******************************************************************************/
int	create_equiv_stor_blk(int		attr_idx,
			      sb_type_type	sb_type)
{

   static	char		equivblk[8];
   static	int		ceb		= 64;
		id_str_type	storage_name;
		int		sb_idx;


   TRACE (Func_Entry, "create_equiv_stor_blk", NULL);

   ceb = ceb + 1;

   if (ceb == 91) {
      ceb = 65;  /* start over at "A" again */
   }

# if defined(_NO_AT_SIGN_IN_NAMES)
   equivblk[0] = '.';
# else
   equivblk[0] = '@';
# endif
   equivblk[1] = 'E';
   equivblk[2] = 'Q';
   equivblk[3] = 'U';
   equivblk[4] = 'I';
   equivblk[5] = 'V';
   equivblk[6] = (char)ceb;

   CREATE_ID(storage_name, equivblk, 7);

   if (sb_type == Stack) {
      sb_type = Equivalenced;
   }

   sb_idx = ntr_stor_blk_tbl(storage_name.string, 7,
                             AT_DEF_LINE(attr_idx),
                             AT_DEF_COLUMN(attr_idx),
                             sb_type);

   SB_EQUIVALENCED(sb_idx) = TRUE;

   TRACE (Func_Exit, "create_equiv_stor_blk", NULL);

   return(sb_idx);

}  /* create_equiv_stor_blk */

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

static void insert_argchck_calls(int		sh_idx,
                                 int		pgm_attr_idx)

{
   int		argchck_darg_idx;
   int		br_true_idx;
   int		col;
   int		ir_idx;
   int		label_idx;
   int		line;
   int		list_idx;
   int		loc_idx;
   int		not_idx;
   opnd_type	opnd;
   int		save_curr_stmt_sh_idx;



   TRACE (Func_Entry, "insert_argchck_calls", NULL);

   line = SH_GLB_LINE(curr_stmt_sh_idx);
   col  = SH_COL_NUM(curr_stmt_sh_idx);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
   curr_stmt_sh_idx = sh_idx;

   /* create branch around test on argchck flag present */

   label_idx = gen_internal_lbl(line);
   
   NTR_IR_TBL(ir_idx);
   IR_TYPE_IDX(ir_idx) = LOGICAL_DEFAULT_TYPE;
   IR_OPR(ir_idx) = Argchck_Present_Opr;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   NTR_IR_TBL(not_idx);
   IR_TYPE_IDX(not_idx) = LOGICAL_DEFAULT_TYPE;
   IR_OPR(not_idx) = Not_Opr;
   IR_LINE_NUM(not_idx) = line;
   IR_COL_NUM(not_idx)  = col;

   IR_FLD_L(not_idx) = IR_Tbl_Idx;
   IR_IDX_L(not_idx) = ir_idx;

   NTR_IR_TBL(br_true_idx);
   IR_OPR(br_true_idx)        = Br_True_Opr;
   IR_TYPE_IDX(br_true_idx)   = LOGICAL_DEFAULT_TYPE;
   IR_LINE_NUM(br_true_idx)   = line;
   IR_COL_NUM(br_true_idx)    = col;
   IR_FLD_R(br_true_idx)      = AT_Tbl_Idx;
   IR_IDX_R(br_true_idx)      = label_idx;
   IR_LINE_NUM_R(br_true_idx) = line;
   IR_COL_NUM_R(br_true_idx)  = col;

   IR_FLD_L(br_true_idx)      = IR_Tbl_Idx;
   IR_IDX_L(br_true_idx)      = not_idx;

   gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = br_true_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /* put call to argchck routine here */

   OPND_FLD(opnd)	= AT_Tbl_Idx;
   OPND_IDX(opnd)	= pgm_attr_idx;
   OPND_LINE_NUM(opnd)	= line;
   OPND_COL_NUM(opnd)	= col;
   argchck_darg_idx	= create_argchck_descriptor(&opnd);
   
   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Call_Opr;
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   if (glb_tbl_idx[Argchck_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Argchck_Attr_Idx] = create_lib_entry_attr(ARGCHCK_LIB_ENTRY,
                                                            ARGCHCK_NAME_LEN,
                                                            line,
                                                            col);
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Argchck_Attr_Idx]);

   IR_FLD_L(ir_idx) = AT_Tbl_Idx;
   IR_IDX_L(ir_idx) = glb_tbl_idx[Argchck_Attr_Idx];
   IR_LINE_NUM_L(ir_idx) = line;
   IR_COL_NUM_L(ir_idx)  = col;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(ir_idx) = IL_Tbl_Idx;
   IR_IDX_R(ir_idx) = list_idx;
   IR_LIST_CNT_R(ir_idx) = 2;
   
   NTR_IR_TBL(loc_idx);
   IR_OPR(loc_idx)              = Argchck_Loc_Opr;
   IR_TYPE_IDX(loc_idx)         = CRI_Ptr_8;
   IR_LINE_NUM(loc_idx)         = line;
   IR_COL_NUM(loc_idx)          = col;
   IL_FLD(list_idx)             = IR_Tbl_Idx;
   IL_IDX(list_idx)             = loc_idx;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   NTR_IR_TBL(loc_idx);
   IR_OPR(loc_idx)              = Aloc_Opr;
   IR_TYPE_IDX(loc_idx)         = CRI_Ptr_8;
   IR_LINE_NUM(loc_idx)         = line;
   IR_COL_NUM(loc_idx)          = col;
   IR_FLD_L(loc_idx)            = AT_Tbl_Idx;
   IR_IDX_L(loc_idx)            = argchck_darg_idx;
   IR_LINE_NUM_L(loc_idx)       = line;
   IR_COL_NUM_L(loc_idx)        = col;
   IL_FLD(list_idx)             = IR_Tbl_Idx;
   IL_IDX(list_idx)             = loc_idx;

   gen_sh(Before, Call_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


   /* now, put label in it's place */

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)              = Label_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)         = line;
   IR_COL_NUM(ir_idx)          = col;
   IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
   IR_IDX_L(ir_idx)            = label_idx;
   IR_COL_NUM_L(ir_idx)        = col;
   IR_LINE_NUM_L(ir_idx)       = line;

   AT_DEFINED(label_idx)       = TRUE;

   gen_sh(Before, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "insert_argchck_calls", NULL);

   return;

}  /* insert_argchck_calls */

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

static void gen_assumed_shape_copy(opnd_type *top_opnd)

{
   int                  addr_asg_idx;
   int                  addr_tmp_idx;
   int                  asg_idx;
   int			attr_idx;
   int			br_true_idx;
   int			cn_idx;
   int                  col;
   opnd_type            dv_opnd;
   int                  entry_attr_idx;
   int                  entry_list_idx;
   expr_arg_type        exp_desc;
   int			i;
   int			intent;
   int                  ir_idx;
   int                  label_idx1;
   int                  label_idx2;
   int                  label_idx3;
   expr_arg_type	l_exp_desc;
   opnd_type            left_opnd;
   int                  line;
   int                  ne_idx;
   int                  new_end_idx;
   int                  new_start_idx;
   opnd_type            opnd;
   int                  place_holder_sh_idx;
   expr_arg_type	r_exp_desc;
   opnd_type            right_opnd;
   int                  save_curr_stmt_sh_idx;
   cif_usage_code_type  save_xref_state;
   int			save_sh;
   int                  sh_idx;
   int                  tmp_idx;


   TRACE (Func_Entry, "gen_assumed_shape_copy", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
   attr_idx = OPND_IDX((*top_opnd));
   line = OPND_LINE_NUM((*top_opnd));
   col  = OPND_COL_NUM((*top_opnd));

   set_up_which_entry_tmp();

   /* gen a stmt to hold onto any stmts generated by create_tmp_asg */

   curr_stmt_sh_idx = ntr_sh_tbl();
   SH_STMT_TYPE(curr_stmt_sh_idx)    = Assignment_Stmt;
   SH_GLB_LINE(curr_stmt_sh_idx)     = line;
   SH_COL_NUM(curr_stmt_sh_idx)      = col;

   place_holder_sh_idx = curr_stmt_sh_idx;

   OPND_FLD(right_opnd) = AT_Tbl_Idx;
   OPND_IDX(right_opnd) = attr_idx;
   OPND_LINE_NUM(right_opnd) = line;
   OPND_COL_NUM(right_opnd)  = col;

   exp_desc = init_exp_desc;
   exp_desc.rank = 0;

   save_xref_state   = xref_state;
   xref_state        = CIF_No_Usage_Rec;
   expr_semantics(&right_opnd, &exp_desc);
   xref_state        = save_xref_state;

   label_idx1 = gen_internal_lbl(line);
   label_idx2 = gen_internal_lbl(line);
   label_idx3 = gen_internal_lbl(line);

   /* find the dope vector opnd */

   OPND_FLD(dv_opnd) = AT_Tbl_Idx;
   OPND_IDX(dv_opnd) = attr_idx;
   OPND_LINE_NUM(dv_opnd) = line;
   OPND_COL_NUM(dv_opnd)  = col;

   /* generate if (contig)  for contig_test_ir_idx */

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Access_A_Contig;
   IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   COPY_OPND(IR_OPND_L(ir_idx), dv_opnd);

   NTR_IR_TBL(ne_idx);
   IR_OPR(ne_idx) = Ne_Opr;
   IR_TYPE_IDX(ne_idx) = LOGICAL_DEFAULT_TYPE;

   IR_LINE_NUM(ne_idx) = line;
   IR_COL_NUM(ne_idx) = col;

   IR_FLD_L(ne_idx) = IR_Tbl_Idx;
   IR_IDX_L(ne_idx) = ir_idx;

   IR_FLD_R(ne_idx) = CN_Tbl_Idx;
   IR_IDX_R(ne_idx) = CN_INTEGER_ONE_IDX;
   IR_LINE_NUM_R(ne_idx) = line;
   IR_COL_NUM_R(ne_idx)  = col;

   br_true_idx = gen_ir(IR_Tbl_Idx, ne_idx,
                    Br_True_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                        AT_Tbl_Idx, label_idx1);

   gen_opnd(&opnd, ne_idx, IR_Tbl_Idx, line, col);
   copy_subtree(&opnd, &opnd);
   IR_OPR(OPND_IDX(opnd)) = Eq_Opr;

   contig_test_ir_idx    = OPND_IDX(opnd);

   /* generate branch around label_idx3 After */

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)              = Label_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)         = line;
   IR_COL_NUM(ir_idx)          = col;
   IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
   IR_IDX_L(ir_idx)            = label_idx3;
   IR_COL_NUM_L(ir_idx)        = col;
   IR_LINE_NUM_L(ir_idx)       = line;

   AT_DEFINED(label_idx3)      = TRUE;

   gen_sh(After, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

   ATL_DEF_STMT_IDX(label_idx3) = curr_stmt_sh_idx;

   curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   /* do copy in Before */

   intent = Intent_Inout;

   if (ATD_INTENT(attr_idx) == Intent_Out) {
      intent = Intent_Out;
   }
   else if (ATD_INTENT(attr_idx) == Intent_In) {
      intent = Intent_In;
   }

   tmp_idx = create_tmp_asg(&right_opnd,
                            &exp_desc,
                            &left_opnd,
                            intent,
                            FALSE,
                            FALSE);

   addr_tmp_idx = ATD_AUTO_BASE_IDX(tmp_idx);

   sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   while (sh_idx) {

      if (IR_OPR(SH_IR_IDX(sh_idx)) == Asg_Opr &&
          IR_FLD_R(SH_IR_IDX(sh_idx)) == IR_Tbl_Idx &&
          IR_OPR(IR_IDX_R(SH_IR_IDX(sh_idx))) == Alloc_Opr) {

         break;
      }

      sh_idx = SH_PREV_IDX(sh_idx);

# ifdef _DEBUG
      if (sh_idx == NULL_IDX) {
         PRINTMSG(line, 626, Internal, col,
                  "Alloc_Opr", "gen_assumed_shape_copy");
      }
# endif
   }

   curr_stmt_sh_idx = sh_idx;

   /* generate if (contig)  Before */

   gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = br_true_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   contig_test_ir_idx = NULL_IDX;

   /* set address temp = address from dope vector */
   /*                             Before          */

   if (cmd_line_flags.runtime_conformance) {
      get_shape_from_attr(&l_exp_desc,
                          tmp_idx,
                          BD_RANK(ATD_ARRAY_IDX(tmp_idx)),
                          line,
                          col);
      l_exp_desc.rank = BD_RANK(ATD_ARRAY_IDX(tmp_idx));

      get_shape_from_attr(&r_exp_desc,
                          attr_idx,
                          BD_RANK(ATD_ARRAY_IDX(attr_idx)),
                          line,
                          col);
      r_exp_desc.rank = BD_RANK(ATD_ARRAY_IDX(attr_idx));

      OPND_FLD(opnd) = AT_Tbl_Idx;
      OPND_IDX(opnd) = tmp_idx;
      OPND_LINE_NUM(opnd) = line;
      OPND_COL_NUM(opnd)  = col;

      gen_runtime_conformance(&opnd,
                              &l_exp_desc,
                              &right_opnd,
                              &r_exp_desc);

   }

   NTR_IR_TBL(addr_asg_idx);
   IR_OPR(addr_asg_idx) = Asg_Opr;
   IR_FLD_L(addr_asg_idx)    = AT_Tbl_Idx;
   IR_IDX_L(addr_asg_idx)    = addr_tmp_idx;
   IR_TYPE_IDX(addr_asg_idx) = ATD_TYPE_IDX(addr_tmp_idx);

   IR_LINE_NUM(addr_asg_idx) = line;
   IR_COL_NUM(addr_asg_idx)  = col;
   IR_LINE_NUM_L(addr_asg_idx) = line;
   IR_COL_NUM_L(addr_asg_idx)  = col;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)   = Dv_Access_Base_Addr;
   IR_TYPE_IDX(ir_idx) = SA_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   COPY_OPND(IR_OPND_L(ir_idx), dv_opnd);

   IR_FLD_R(addr_asg_idx) = IR_Tbl_Idx;
   IR_IDX_R(addr_asg_idx) = ir_idx;

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = addr_asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /* generate goto label_idx2 Before */

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)        = Br_Uncond_Opr;
   IR_TYPE_IDX(ir_idx)   = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)   = line;
   IR_COL_NUM(ir_idx)    = col;
   IR_FLD_R(ir_idx)      = AT_Tbl_Idx;
   IR_IDX_R(ir_idx)      = label_idx2;
   IR_LINE_NUM_R(ir_idx) = line;
   IR_COL_NUM_R(ir_idx)  = col;

   gen_sh(Before, Goto_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = ir_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   /* insert label_idx1 */

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)              = Label_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)         = line;
   IR_COL_NUM(ir_idx)          = col;
   IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
   IR_IDX_L(ir_idx)            = label_idx1;
   IR_COL_NUM_L(ir_idx)        = col;
   IR_LINE_NUM_L(ir_idx)       = line;

   AT_DEFINED(label_idx1)      = TRUE;

   gen_sh(Before, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;

   ATL_DEF_STMT_IDX(label_idx1) = SH_PREV_IDX(curr_stmt_sh_idx);

   curr_stmt_sh_idx = place_holder_sh_idx;

   /* insert label_idx2 Before */

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)              = Label_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)         = line;
   IR_COL_NUM(ir_idx)          = col;
   IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
   IR_IDX_L(ir_idx)            = label_idx2;
   IR_COL_NUM_L(ir_idx)        = col;
   IR_LINE_NUM_L(ir_idx)       = line;

   AT_DEFINED(label_idx2)      = TRUE;

   gen_sh(Before, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;

   ATL_DEF_STMT_IDX(label_idx2) = SH_PREV_IDX(curr_stmt_sh_idx);



   /* generate if (!contig) test After */

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = Dv_Access_A_Contig;
   IR_TYPE_IDX(ir_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;

   COPY_OPND(IR_OPND_L(ir_idx), dv_opnd);

   NTR_IR_TBL(ne_idx);
   IR_OPR(ne_idx) = Eq_Opr;
   IR_TYPE_IDX(ne_idx) = LOGICAL_DEFAULT_TYPE;

   IR_LINE_NUM(ne_idx) = line;
   IR_COL_NUM(ne_idx) = col;

   IR_FLD_L(ne_idx) = IR_Tbl_Idx;
   IR_IDX_L(ne_idx) = ir_idx;

   IR_FLD_R(ne_idx) = CN_Tbl_Idx;
   IR_IDX_R(ne_idx) = CN_INTEGER_ONE_IDX;
   IR_LINE_NUM_R(ne_idx) = line;
   IR_COL_NUM_R(ne_idx) = col;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)        = Br_True_Opr;
   IR_TYPE_IDX(ir_idx)   = LOGICAL_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)   = line;
   IR_COL_NUM(ir_idx)    = col;
   IR_FLD_R(ir_idx)      = AT_Tbl_Idx;
   IR_IDX_R(ir_idx)      = label_idx3;
   IR_LINE_NUM_R(ir_idx) = line;
   IR_COL_NUM_R(ir_idx)  = col;

   IR_FLD_L(ir_idx)      = IR_Tbl_Idx;
   IR_IDX_L(ir_idx)      = ne_idx;

   gen_sh(After, If_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

   curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   ATD_SF_ARG_IDX(attr_idx) = tmp_idx;
   ATD_COPY_ASSUMED_SHAPE(attr_idx) = TRUE;

   ATD_TMP_IDX(tmp_idx) = attr_idx;
   ATD_FLD(tmp_idx) = AT_Tbl_Idx;

   ATD_COPY_ASSUMED_SHAPE(tmp_idx) = TRUE;

   /* find beginning sh idx */

   sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   while(SH_PREV_IDX(sh_idx)) {
      sh_idx = SH_PREV_IDX(sh_idx);
   }

   /* check OPTIONAL darg's presence */

   if (AT_OPTIONAL(attr_idx)) {
      gen_present_ir(attr_idx, sh_idx, SH_PREV_IDX(curr_stmt_sh_idx));

      sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      while(SH_PREV_IDX(sh_idx)) {
         sh_idx = SH_PREV_IDX(sh_idx);
      }
   }

   if (shared_bd_idx < 0) {
      shared_bd_idx = ATD_ARRAY_IDX(tmp_idx);

      if (reassign_XT_temps) {
         /* preset XT temp to -1 */
         save_sh = curr_stmt_sh_idx;
         curr_stmt_sh_idx = sh_idx;

         cn_idx = CN_INTEGER_NEG_ONE_IDX;

         for (i = 1; i <= BD_RANK(shared_bd_idx); i++) {
            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx) = Asg_Opr;
            IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(BD_XT_IDX(shared_bd_idx,i));
            IR_LINE_NUM(asg_idx) = line;
            IR_COL_NUM(asg_idx)  = col;
            IR_FLD_L(asg_idx) = AT_Tbl_Idx;
            IR_IDX_L(asg_idx) = BD_XT_IDX(shared_bd_idx,i);
            IR_LINE_NUM_L(asg_idx) = line;
            IR_COL_NUM_L(asg_idx)  = col;
            IR_FLD_R(asg_idx) = CN_Tbl_Idx;
            IR_IDX_R(asg_idx) = cn_idx;
            IR_LINE_NUM_R(asg_idx) = line;
            IR_COL_NUM_R(asg_idx)  = col;
           
            gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
         }

         curr_stmt_sh_idx = save_sh;

         sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         while(SH_PREV_IDX(sh_idx)) {
            sh_idx = SH_PREV_IDX(sh_idx);
         }
      }
   }

   if (gen_ir_at_this_entry(SCP_ATTR_IDX(curr_scp_idx), attr_idx)) {
      SH_PREV_IDX(sh_idx) = SH_PREV_IDX(save_curr_stmt_sh_idx);
      SH_NEXT_IDX(SH_PREV_IDX(sh_idx)) = sh_idx;
      SH_PREV_IDX(save_curr_stmt_sh_idx) = SH_PREV_IDX(curr_stmt_sh_idx);
      SH_NEXT_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = save_curr_stmt_sh_idx;
   }


   entry_list_idx       = SCP_ENTRY_IDX(curr_scp_idx);

   while (entry_list_idx != NULL_IDX) {
      entry_attr_idx    = AL_ATTR_IDX(entry_list_idx);

      if (gen_ir_at_this_entry(entry_attr_idx, attr_idx)) {
         copy_entry_exit_sh_list(sh_idx, SH_PREV_IDX(curr_stmt_sh_idx),
                                 &new_start_idx, &new_end_idx);

         /* insert the stmt string before ATP_ENTRY_LABEL_SH_IDX */

         SH_PREV_IDX(new_start_idx) =
               SH_PREV_IDX(ATP_ENTRY_LABEL_SH_IDX(entry_attr_idx));
         SH_NEXT_IDX(SH_PREV_IDX(new_start_idx)) = new_start_idx;
         SH_NEXT_IDX(new_end_idx) = ATP_ENTRY_LABEL_SH_IDX(entry_attr_idx);
         SH_PREV_IDX(ATP_ENTRY_LABEL_SH_IDX(entry_attr_idx)) = new_end_idx;

      }

      entry_list_idx = AL_NEXT_IDX(entry_list_idx);
   }

   /* find end sh idx */

   sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);

   while(SH_NEXT_IDX(sh_idx)) {
      sh_idx = SH_NEXT_IDX(sh_idx);
   }

   /* check OPTIONAL darg's presence */

   if (AT_OPTIONAL(attr_idx)) {
      gen_present_ir(attr_idx, SH_NEXT_IDX(curr_stmt_sh_idx), sh_idx);

      sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);

      while(SH_NEXT_IDX(sh_idx)) {
         sh_idx = SH_NEXT_IDX(sh_idx);
      }
   }

   if (SCP_ENTRY_IDX(curr_scp_idx) != NULL_IDX) {
      gen_branch_around_ir(gen_darg_branch_test(attr_idx),
                           SH_NEXT_IDX(curr_stmt_sh_idx), sh_idx);

      sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);

      while(SH_NEXT_IDX(sh_idx)) {
         sh_idx = SH_NEXT_IDX(sh_idx);
      }
   }

   if (sh_idx) {
      if (SCP_EXIT_IR_SH_IDX(curr_scp_idx) != NULL_IDX) {
         SH_NEXT_IDX(sh_idx)              = SCP_EXIT_IR_SH_IDX(curr_scp_idx);
         SCP_EXIT_IR_SH_IDX(curr_scp_idx) = SH_NEXT_IDX(curr_stmt_sh_idx);
      }
      else {
         SCP_EXIT_IR_SH_IDX(curr_scp_idx) = SH_NEXT_IDX(curr_stmt_sh_idx);
      }
   }

   FREE_SH_NODE(curr_stmt_sh_idx);
   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "gen_assumed_shape_copy", NULL);

   return;

}  /* gen_assumed_shape_copy */

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

static int      gen_darg_branch_test(int        attr_idx)

{
   int                  al_idx;
   int                  col;
   int                  entry_al_idx;
   int                  i;
   int                  ir_idx;
   int                  line;
   opnd_type            opnd;
   int                  or_idx;
   int                  pgm_idx;
   long_type            the_constant;

   TRACE (Func_Entry, "gen_darg_branch_test", NULL);

   the_constant = 1;
   pgm_idx = SCP_ATTR_IDX(curr_scp_idx);
   line = AT_DEF_LINE(pgm_idx);
   col  = AT_DEF_COLUMN(pgm_idx);

   gen_opnd(&opnd, NULL_IDX, NO_Tbl_Idx, line, col);

   al_idx = ATD_NO_ENTRY_LIST(attr_idx);

   while (al_idx) {

      if (pgm_idx == AL_ATTR_IDX(al_idx)) {
         /* branch around on this */

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Eq_Opr;
         IR_TYPE_IDX(ir_idx) = CG_LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx) = line;
         IR_COL_NUM(ir_idx)  = col;
         IR_FLD_L(ir_idx) = AT_Tbl_Idx;
         IR_IDX_L(ir_idx) = SCP_WHICH_ENTRY_TMP(curr_scp_idx);
         IR_LINE_NUM_L(ir_idx) = line;
         IR_COL_NUM_L(ir_idx)  = col;

         IR_FLD_R(ir_idx) = CN_Tbl_Idx;
         IR_IDX_R(ir_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                        the_constant);
         IR_LINE_NUM_R(ir_idx) = line;
         IR_COL_NUM_R(ir_idx)  = col;

         if (OPND_FLD(opnd) == NO_Tbl_Idx) {
            OPND_FLD(opnd) = IR_Tbl_Idx;
            OPND_IDX(opnd) = ir_idx;
         }
         else {
            NTR_IR_TBL(or_idx);
            IR_OPR(or_idx) = Or_Opr;
            IR_TYPE_IDX(or_idx) = CG_LOGICAL_DEFAULT_TYPE;
            IR_LINE_NUM(or_idx) = line;
            IR_COL_NUM(or_idx)  = col;

            IR_FLD_R(or_idx) = IR_Tbl_Idx;
            IR_IDX_R(or_idx) = ir_idx;

            COPY_OPND(IR_OPND_L(or_idx), opnd);
            OPND_FLD(opnd) = IR_Tbl_Idx;
            OPND_IDX(opnd) = or_idx;
         }

         break;
      }
      al_idx = AL_NEXT_IDX(al_idx);
   }

   entry_al_idx = SCP_ENTRY_IDX(curr_scp_idx);

   for (i = 0; i < SCP_ALT_ENTRY_CNT(curr_scp_idx); i++) {
      the_constant++;
      pgm_idx = AL_ATTR_IDX(entry_al_idx);

      al_idx = ATD_NO_ENTRY_LIST(attr_idx);

      while (al_idx) {
         if (pgm_idx == AL_ATTR_IDX(al_idx)) {
            /* branch around on this */
            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx) = Eq_Opr;
            IR_TYPE_IDX(ir_idx) = CG_LOGICAL_DEFAULT_TYPE;
            IR_LINE_NUM(ir_idx) = line;
            IR_COL_NUM(ir_idx)  = col;
            IR_FLD_L(ir_idx) = AT_Tbl_Idx;
            IR_IDX_L(ir_idx) = SCP_WHICH_ENTRY_TMP(curr_scp_idx);
            IR_LINE_NUM_L(ir_idx) = line;
            IR_COL_NUM_L(ir_idx)  = col;

            IR_FLD_R(ir_idx) = CN_Tbl_Idx;
            IR_IDX_R(ir_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                           the_constant);
            IR_LINE_NUM_R(ir_idx) = line;
            IR_COL_NUM_R(ir_idx)  = col;

            if (OPND_FLD(opnd) == NO_Tbl_Idx) {
               OPND_FLD(opnd) = IR_Tbl_Idx;
               OPND_IDX(opnd) = ir_idx;
            }
            else {
               NTR_IR_TBL(or_idx);
               IR_OPR(or_idx) = Or_Opr;
               IR_TYPE_IDX(or_idx) = CG_LOGICAL_DEFAULT_TYPE;
               IR_LINE_NUM(or_idx) = line;
               IR_COL_NUM(or_idx)  = col;

               IR_FLD_R(or_idx) = IR_Tbl_Idx;
               IR_IDX_R(or_idx) = ir_idx;

               COPY_OPND(IR_OPND_L(or_idx), opnd);
               OPND_FLD(opnd) = IR_Tbl_Idx;
               OPND_IDX(opnd) = or_idx;
            }

            break;
         }
         al_idx = AL_NEXT_IDX(al_idx);
      }

      entry_al_idx = AL_NEXT_IDX(entry_al_idx);
   }


   TRACE (Func_Exit, "gen_darg_branch_test", NULL);

   return(OPND_IDX(opnd));

}  /* gen_darg_branch_test */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This generates if condition code for a branch around test             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      condition_idx- Index to an IR_Tbl_Idx for the branch around condition *|
|*      start_sh_idx - Index to start of IR to have an if present put around  *|
|*      end_sh_idx   - Index to end of IR to have an if present put around    *|
|*                     This gets updated to point to the new last sh idx.     *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*       NONE                                                                 *|
|*                                                                            *|
|* Returns:                                                                   *|
|*       NONE                                                                 *|
|*                                                                            *|
\******************************************************************************/
static  void    gen_branch_around_ir(int      condition_idx,
                                     int      start_sh_idx,
                                     int      end_sh_idx)
{
   int          br_around_opt;
   int          br_idx;
   int          col;
   int          cont_idx;
   int          line;
   int          save_sh_idx;


   TRACE (Func_Entry, "gen_branch_around_ir", NULL);

   save_sh_idx          = curr_stmt_sh_idx;
   curr_stmt_sh_idx     = start_sh_idx;
   line = SH_GLB_LINE(start_sh_idx);
   col = SH_COL_NUM(start_sh_idx);

   gen_sh(Before,
          Goto_Stmt,
          line,
          col,
          FALSE,
          FALSE,
          TRUE);

   SH_P2_SKIP_ME(SH_PREV_IDX(start_sh_idx))     = TRUE;

   br_around_opt        = gen_internal_lbl(line);

   NTR_IR_TBL(br_idx);

   IR_OPR(br_idx)       = Br_True_Opr;
   IR_TYPE_IDX(br_idx)  = LOGICAL_DEFAULT_TYPE;

   SH_IR_IDX(SH_PREV_IDX(start_sh_idx)) = br_idx;
   IR_LINE_NUM(br_idx)          = line;
   IR_COL_NUM(br_idx)           = col;

   IR_FLD_R(br_idx)             = AT_Tbl_Idx;
   IR_IDX_R(br_idx)             = br_around_opt;
   IR_COL_NUM_R(br_idx)         = col;
   IR_LINE_NUM_R(br_idx)        = line;

   IR_FLD_L(br_idx)             = IR_Tbl_Idx;
   IR_IDX_L(br_idx)             = condition_idx;

   NTR_IR_TBL(cont_idx);
   IR_OPR(cont_idx)             = Label_Opr;
   IR_TYPE_IDX(cont_idx)        = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(cont_idx)        = line;
   IR_COL_NUM(cont_idx)         = col;
   IR_IDX_L(cont_idx)           = br_around_opt;
   IR_FLD_L(cont_idx)           = AT_Tbl_Idx;
   IR_LINE_NUM_L(cont_idx)      = line;
   IR_COL_NUM_L(cont_idx)       = col;
   curr_stmt_sh_idx             = end_sh_idx;

   gen_sh(After,
          Continue_Stmt,
          line,
          col,
          FALSE,
          TRUE,
          TRUE);

   SH_P2_SKIP_ME(curr_stmt_sh_idx)      = TRUE;
   SH_IR_IDX(curr_stmt_sh_idx)          = cont_idx;
   curr_stmt_sh_idx                     = save_sh_idx;

   TRACE (Func_Exit, "gen_branch_around_ir", NULL);

   return;

}  /* gen_branch_around_ir */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Go through a list of assumed shape attrs for COPY_ASSUMED_SHAPE to    *|
|*      look for optional args. If a nonoptional arg exists, put in first     *|
|*      and return false (no need to reassign extent temps). If all are       *|
|*      optional, return true (the shared extent temp must have assignment    *|
|*      statements generated for each darg).                                  *|
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

static boolean must_reassign_XT_temp(opnd_type *top_opnd)

{
   boolean      all_optional = TRUE;
   int          list_idx;

   TRACE (Func_Entry, "must_reassign_XT_temp", NULL);

   list_idx = OPND_IDX((*top_opnd));

   if (! AT_OPTIONAL(IL_IDX(list_idx))) {
      all_optional = FALSE;
   }
   else {
      while (list_idx) {
         if (! AT_OPTIONAL(IL_IDX(list_idx))) {
            all_optional = FALSE;
            break;
         }

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }  

      if (! all_optional) {
         /* move the non optional attr to the front */

         if (IL_PREV_LIST_IDX(list_idx) != NULL_IDX) {
            IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list_idx)) = 
                                               IL_NEXT_LIST_IDX(list_idx);
         }

         if (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = 
                                               IL_PREV_LIST_IDX(list_idx);
         }

         IL_NEXT_LIST_IDX(list_idx) = OPND_IDX((*top_opnd));
         if (OPND_IDX((*top_opnd)) != NULL_IDX) {
            IL_PREV_LIST_IDX(OPND_IDX((*top_opnd))) = list_idx;
         }

         OPND_IDX((*top_opnd)) = list_idx;

      }
   }

   TRACE (Func_Exit, "must_reassign_XT_temp", NULL);

   return(all_optional);

}  /* must_reassign_XT_temp */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
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
static int	gen_auto_length(int		 attr_idx,
				opnd_type	*len_opnd)

{
   int			bd_idx;
   int			column;
   expr_arg_type	expr_desc;
   int			len_idx;
   int			line;

   opnd_type		opnd1;
   opnd_type		opnd2;
   int			result_type_idx;
   int			type_idx;
   int			type1_idx;
   int			type2_idx;

# if !defined(_TARGET_WORD_ADDRESS)
   int			mult_idx;
   long			word_byte_size;
# endif


   TRACE (Func_Entry, "gen_auto_length", NULL);

   bd_idx	= ATD_ARRAY_IDX(attr_idx);
   type_idx	= ATD_TYPE_IDX(attr_idx);
   line		= AT_DEF_LINE(attr_idx);
   column	= AT_DEF_COLUMN(attr_idx);

   if (TYP_TYPE(type_idx) == Character ||
       (TYP_TYPE(type_idx) == Structure && ATT_CHAR_SEQ(TYP_IDX(type_idx)) ) ) {

      /* The allocation is in bytes for SGI and solaris.     */
      /* The allocation is in words for Crays.               */
      /* The allocation is in words for _TARGET_OS_MAX, but  */
      /* we calculate the allocation length in bytes because */
      /* TARGET_OS_MAX is for a byte addressable machine and */
      /* all tmps that address into the allocated area must  */
      /* be in bytes.  This way we use already existing code */
      /* and the only thing we have to do special is divide  */
      /* by TARGET_BYTES_PER_WORD to get the allocation      */
      /* length in words.                                    */

      /* this assumes that chars are one byte BHJ */

      /* Get character length */

      OPND_LINE_NUM((*len_opnd))= line;
      OPND_COL_NUM((*len_opnd))	= column;

      if (TYP_TYPE(type_idx) == Structure) {
         OPND_FLD(opnd1)	= BD_LEN_FLD(bd_idx);
         OPND_IDX(opnd1)	= BD_LEN_IDX(bd_idx);
         OPND_LINE_NUM(opnd1)	= line;
         OPND_COL_NUM(opnd1)	= column;

         type1_idx		= check_type_for_size_address(&opnd1);

         OPND_FLD(opnd2)	= BD_SM_FLD(bd_idx,1);
         OPND_IDX(opnd2)	= BD_SM_IDX(bd_idx,1);
         OPND_LINE_NUM(opnd2)	= line;
         OPND_COL_NUM(opnd2)	= column;

         type2_idx		= check_type_for_size_address(&opnd2);

         result_type_idx	= TYP_LINEAR(type1_idx) > TYP_LINEAR(type2_idx)?
                                  type1_idx : type2_idx;

         /* If this is a character sequence structure, we know the size  */
         /* of the structure.  Assume this must be an array.  The stride */
         /* multiplier of the array is set to number of bytes for char   */
         /* sequence structures.                                         */

         NTR_IR_TBL(len_idx);
         IR_OPR(len_idx)	= Mult_Opr;
         IR_TYPE_IDX(len_idx)	= result_type_idx;
         IR_LINE_NUM(len_idx)   = line;
         IR_COL_NUM(len_idx)    = column;
         COPY_OPND(IR_OPND_L(len_idx), opnd2);
         COPY_OPND(IR_OPND_R(len_idx), opnd1);

         OPND_FLD((*len_opnd))	= IR_Tbl_Idx;
         OPND_IDX((*len_opnd))	= len_idx;
      }
      else if (bd_idx == NULL_IDX) {
         OPND_FLD((*len_opnd))	= AT_Tbl_Idx;
         OPND_IDX((*len_opnd))	= TYP_IDX(type_idx);
         result_type_idx	= check_type_for_size_address(&(*len_opnd));
      }
      else { /* If array - multiply num of chars by num of elements.     */
         OPND_FLD(opnd1)	= BD_LEN_FLD(bd_idx);
         OPND_IDX(opnd1)	= BD_LEN_IDX(bd_idx);
         OPND_LINE_NUM(opnd1)	= line;
         OPND_COL_NUM(opnd1)	= column;

         type1_idx		= check_type_for_size_address(&opnd1);

         OPND_FLD(opnd2)	= TYP_FLD(type_idx);
         OPND_IDX(opnd2)	= TYP_IDX(type_idx);
         OPND_LINE_NUM(opnd2)	= line;
         OPND_COL_NUM(opnd2)	= column;

         type2_idx		= check_type_for_size_address(&opnd2);

         result_type_idx	= TYP_LINEAR(type1_idx) > TYP_LINEAR(type2_idx)?
                                  type1_idx : type2_idx;
         NTR_IR_TBL(len_idx);
         IR_OPR(len_idx)	= Mult_Opr;
         IR_TYPE_IDX(len_idx)	= result_type_idx;
         IR_LINE_NUM(len_idx)   = line;
         IR_COL_NUM(len_idx)    = column;

         COPY_OPND(IR_OPND_L(len_idx), opnd2);
         COPY_OPND(IR_OPND_R(len_idx), opnd1);

         OPND_FLD((*len_opnd))	= IR_Tbl_Idx;
         OPND_IDX((*len_opnd))	= len_idx;
      }

# ifdef _TARGET_WORD_ADDRESS

      /* Alloc is in words, but all character lengths are in number of       */
      /* chars.  Change byte length to word length.  DO NOT do this for      */
      /* byte addressable machines, because addressing needs to be in bytes. */
      /* If heap allocation must be in words, it  will be switched to words  */
      /* right before the allocation IR is added to the ir stream.           */

      gen_word_align_byte_length_ir(len_opnd);

# endif

   } 
   else {  /* Non-Character */
      OPND_FLD(opnd1)		= BD_LEN_FLD(bd_idx);
      OPND_IDX(opnd1)		= BD_LEN_IDX(bd_idx);
      OPND_LINE_NUM(opnd1)	= line;
      OPND_COL_NUM(opnd1)	= column;

      type1_idx			= check_type_for_size_address(&opnd1);

      OPND_FLD(opnd2)		= BD_SM_FLD(bd_idx,1);
      OPND_IDX(opnd2)		= BD_SM_IDX(bd_idx,1);
      OPND_LINE_NUM(opnd2)	= line;
      OPND_COL_NUM(opnd2)	= column;

      type2_idx			= check_type_for_size_address(&opnd2);

      result_type_idx		= TYP_LINEAR(type1_idx) > TYP_LINEAR(type2_idx)?
                                  type1_idx : type2_idx;

      /* If this is a character sequence structure, we know the size  */

      NTR_IR_TBL(len_idx);
      IR_OPR(len_idx)		= Mult_Opr;
      IR_TYPE_IDX(len_idx)	= result_type_idx;
      IR_LINE_NUM(len_idx)	= line;
      IR_COL_NUM(len_idx)	= column;

      COPY_OPND(IR_OPND_L(len_idx), opnd2);
      COPY_OPND(IR_OPND_R(len_idx), opnd1);

# ifdef _TARGET_WORD_ADDRESS

      /* addressing is words */

      OPND_FLD((*len_opnd))	= IR_Tbl_Idx;
      OPND_IDX((*len_opnd))	= len_idx;
# else

      /* addressing is bytes */

      NTR_IR_TBL(mult_idx);
      IR_OPR(mult_idx)		= Mult_Opr;
      IR_TYPE_IDX(mult_idx)	= result_type_idx;
      IR_LINE_NUM(mult_idx)	= line;
      IR_COL_NUM(mult_idx)	= column;
      IR_LINE_NUM_L(mult_idx)	= line;
      IR_COL_NUM_L(mult_idx)	= column;

      IR_FLD_L(mult_idx)	= CN_Tbl_Idx;

#if defined(_TARGET_PACK_HALF_WORD_TYPES)

      /* Check if this is a packed storage type.  If  */
      /* so, it only needs one half word for storage. */

      if (TARGET_MAX_HALF_WORD_STORAGE_TYPE(type_idx)) {
         word_byte_size		= TARGET_BYTES_PER_WORD / 2;
      }
      else {
         word_byte_size		= TARGET_BYTES_PER_WORD;
      }
# else
      word_byte_size		= TARGET_BYTES_PER_WORD;
# endif

      IR_IDX_L(mult_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              word_byte_size);

      IR_LINE_NUM_R(mult_idx)	= line;
      IR_COL_NUM_R(mult_idx)	= column;
      IR_FLD_R(mult_idx)	= IR_Tbl_Idx;
      IR_IDX_R(mult_idx)	= len_idx;
         
      OPND_FLD((*len_opnd))	= IR_Tbl_Idx;
      OPND_IDX((*len_opnd))	= mult_idx;

# endif

   }

   expr_desc.rank		= 0;
   xref_state			= CIF_No_Usage_Rec;

   if (!expr_semantics(&(*len_opnd), &expr_desc)) {

# if defined(_CHECK_MAX_MEMORY)

      if (!target_t3e) {
         AT_DCL_ERR(attr_idx)	= TRUE;
      }
# endif
   }

   TRACE (Func_Exit, "gen_auto_length", NULL);

   return(result_type_idx);

}  /* gen_auto_length */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine generates the allocatation IR for automatic arrays ands  *|
|*	character.   It is called from char_len_resolution for scalar chars,  *|
|*	and from array_dim_resolution for ALL automatic objects.  This is     *|
|*	where arrays of automatic characters get handled.  The IR is inserted *|
|*	using curr_stmt_sh_idx.  This routine generates an allocate for each  *|
|*	automatic within the program unit.                                    *|
|*									      *|
|*	NOTE:  The type of the base is CRI_Ptr for the single allocate case.  *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx  - The attr idx for the automatic object.                    *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void gen_single_automatic_allocate(int	attr_idx)
{
   int			alloc_idx;
   int			base_ir_idx;
   int			base_tmp_idx;
   int			base_tmp_type_idx;
   int			column;
   int			dealloc_idx;
   int			line;
   opnd_type		opnd;
   int			save_next_sh_idx;
   int			sh_idx;
   int			start_sh_idx;

# if defined(_HEAP_REQUEST_IN_WORDS) && defined(_TARGET_BYTE_ADDRESS)
   expr_arg_type	expr_desc;
# endif


   TRACE (Func_Entry, "gen_single_automatic_allocate", NULL);

   save_next_sh_idx	= SH_NEXT_IDX(curr_stmt_sh_idx);
   start_sh_idx		= curr_stmt_sh_idx;

   base_tmp_type_idx	= gen_auto_length(attr_idx, &opnd);

   /* Do not need allocate or deallocate for an automatic pointee. */

   if (ATD_CLASS(attr_idx) == CRI__Pointee) {
      goto EXIT;
   }

   line			= AT_DEF_LINE(attr_idx);
   column		= AT_DEF_COLUMN(attr_idx);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character ||
       (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure &&
        ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(attr_idx))))) {
      base_tmp_type_idx = CRI_Ch_Ptr_8;
   }
   else {
      base_tmp_type_idx = CRI_Ptr_8;
   }
# endif

   NTR_IR_TBL(alloc_idx);
   IR_TYPE_IDX(alloc_idx)	= TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(alloc_idx)	= line;
   IR_COL_NUM(alloc_idx)	= column;
   COPY_OPND(IR_OPND_L(alloc_idx), opnd);

   NTR_IR_TBL(dealloc_idx);
   IR_TYPE_IDX(dealloc_idx)	= TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(dealloc_idx)	= line;
   IR_COL_NUM(dealloc_idx)	= column;

   if (ATD_AUXILIARY(attr_idx)) {
      IR_OPR(alloc_idx)		= SSD_Alloc_Opr;
      IR_OPR(dealloc_idx)	= SSD_Dealloc_Opr;
   }
   else if (ATD_SYMMETRIC(attr_idx)) {
      IR_OPR(alloc_idx)		= Symmetric_Alloc_Opr;
      IR_OPR(dealloc_idx)	= Symmetric_Dealloc_Opr;
   }
   else {
      IR_OPR(alloc_idx)		= Alloc_Opr;
      IR_OPR(dealloc_idx)	= Dealloc_Opr;
   }

   GEN_COMPILER_TMP_ASG(base_ir_idx,
                        base_tmp_idx,
                        TRUE,		/* Semantics is done */
                        stmt_start_line,
                        stmt_start_col,
                        base_tmp_type_idx,
                        Priv);

   AT_SEMANTICS_DONE(base_tmp_idx)	= TRUE;

   ATD_STOR_BLK_IDX(base_tmp_idx)	= SCP_SB_STACK_IDX(curr_scp_idx);
   ATD_AUTO_BASE_IDX(attr_idx)		= base_tmp_idx;

   IR_FLD_R(base_ir_idx)		= IR_Tbl_Idx;
   IR_IDX_R(base_ir_idx)		= alloc_idx;
   IR_LINE_NUM_R(base_ir_idx)		= line;
   IR_COL_NUM_R(base_ir_idx)		= column;

   /* If the address is in bytes, but the allocation is in words, generate */
   /* additional code on the allocation to change the byte length to a     */
   /* word length.                                                         */

   /* NOTE: We do not handle the case of HEAP_REQUEST_IN_BYTES and TARGET_ */
   /* _WORD_ADDRESS.  Code will have to be added, if that case comes up.   */

# if defined(_HEAP_REQUEST_IN_WORDS) && defined(_TARGET_BYTE_ADDRESS)

   COPY_OPND(opnd, IR_OPND_L(alloc_idx));
   gen_word_align_byte_length_ir(&opnd);

   expr_desc.rank	= 0;
   xref_state		= CIF_No_Usage_Rec;

   expr_semantics(&opnd, &expr_desc);

   COPY_OPND(IR_OPND_L(alloc_idx), opnd);
# endif

   sh_idx	= curr_stmt_sh_idx;

   gen_sh(After,
          Automatic_Base_Calc_Stmt,
          AT_DEF_LINE(base_tmp_idx),
          AT_DEF_COLUMN(base_tmp_idx),
          FALSE,
          FALSE,
          TRUE);  /* Compiler generated */

   SH_IR_IDX(curr_stmt_sh_idx)		= base_ir_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx)	= TRUE;

   if (SCP_ENTRY_IDX(curr_scp_idx) != NULL_IDX) { /* Add at alternate entries */

      if (save_next_sh_idx != NULL_IDX) {
         sh_idx = SH_PREV_IDX(save_next_sh_idx);
      }
      else {

         sh_idx = curr_stmt_sh_idx;

         while (SH_NEXT_IDX(sh_idx) != NULL_IDX) {
            sh_idx = SH_NEXT_IDX(sh_idx);
         }
      }

      insert_sh_after_entries(attr_idx, 
                              start_sh_idx,
                              sh_idx,
                              FALSE,     /* Don't generate tmp = 0  */
                              TRUE);     /* Advance ATP_FIRST_SH_IDX */
   }

   /* Generate the dealloc */
   
   IR_FLD_L(dealloc_idx)	= AT_Tbl_Idx;
   IR_IDX_L(dealloc_idx)	= base_tmp_idx;
   IR_LINE_NUM_L(dealloc_idx)	= line;
   IR_COL_NUM_L(dealloc_idx)	= column;

   sh_idx			= ntr_sh_tbl();
   SH_COMPILER_GEN(sh_idx)	= TRUE;
   SH_P2_SKIP_ME(sh_idx)	= TRUE;
   SH_GLB_LINE(sh_idx)		= stmt_start_line;
   SH_COL_NUM(sh_idx)		= stmt_start_col;
   SH_IR_IDX(sh_idx)		= dealloc_idx;

   if (SCP_EXIT_IR_SH_IDX(curr_scp_idx) != NULL_IDX) {
      SH_NEXT_IDX(sh_idx) = SCP_EXIT_IR_SH_IDX(curr_scp_idx);
      SH_PREV_IDX(SCP_EXIT_IR_SH_IDX(curr_scp_idx)) = sh_idx;
   }

   SCP_EXIT_IR_SH_IDX(curr_scp_idx)	= sh_idx;

EXIT:

   TRACE (Func_Exit, "gen_single_automatic_allocate", NULL);

   return;

}  /* gen_single_automatic_allocate */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine generates the allocatation IR for automatic arrays ands  *|
|*	character.   It is called from char_len_resolution for scalar chars,  *|
|*	and from array_dim_resolution for ALL automatic objects.  This is     *|
|*	where arrays of automatic characters get handled.  The IR is inserted *|
|*	using curr_stmt_sh_idx.  This routine generates one allocate for all  *|
|*	automatics within the program unit.                                   *|
|*									      *|
|*	NOTE:  The type of the base is Integer for the multiple allocate case.*|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx  - The attr idx for the automatic object.                    *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
# if !defined(_SINGLE_ALLOCS_FOR_AUTOMATIC)
static void gen_multiple_automatic_allocate(int	attr_idx)
{

   		boolean		adjust			= FALSE;
   		int		al_idx;
   		int		alloc_idx;
   static	int		auto_aux_base_ir_idx	= NULL_IDX;
   static	int		auto_aux_base_len_idx	= NULL_IDX;
   static	int		auto_aux_base_tmp_idx	= NULL_IDX;
   static	boolean		auto_aux_base_word_align= FALSE;
   static	int		auto_base_ir_idx	= NULL_IDX;
   static	int		auto_base_len_idx	= NULL_IDX;
   static	int		auto_base_list_end	= NULL_IDX;
   static	int		auto_base_list_start	= NULL_IDX;
   static	int		auto_base_tmp_idx	= NULL_IDX;
   static	boolean		auto_base_word_align	= FALSE;
   static	int		auto_sym_base_ir_idx	= NULL_IDX;
   static	int		auto_sym_base_len_idx	= NULL_IDX;
   static	int		auto_sym_base_tmp_idx	= NULL_IDX;
   static	boolean		auto_sym_base_word_align= FALSE;
   		int		base_ir_idx;
   		fld_type	base_len_fld;
   		int		base_len_idx;
   		int		base_tmp_idx;
   		boolean		base_word_align;
		int		column;
		int		div_idx;
# if defined(_HEAP_REQUEST_IN_WORDS) && defined(_TARGET_BYTE_ADDRESS)
   		expr_arg_type	expr_desc;
# endif
   		int		ir_idx;
   		int		len_ir_idx;
		int		line;
		int		mult_idx;
   		int		new_len_idx;
   		int		new_base_attr_idx;
   		int		new_base_ir_idx;
   		opnd_type	opnd;
   		operator_type	opr;
		int		plus_idx;
		int		result_type_idx;
		int		save_next_sh_idx;
   		int		sh_idx;
		int		start_sh_idx;
		int		type_idx;
   		int		tmp_ir_idx;
   		long		word_byte_size		= TARGET_BYTES_PER_WORD;
   		int		word_byte_size_idx;


   TRACE (Func_Entry, "gen_multiple_automatic_allocate", NULL);

   if (attr_idx == NULL_IDX) {
      goto FINISH;
   }

   save_next_sh_idx	= SH_NEXT_IDX(curr_stmt_sh_idx);
   start_sh_idx		= curr_stmt_sh_idx;
   line			= AT_DEF_LINE(attr_idx);
   column		= AT_DEF_COLUMN(attr_idx);
   type_idx		= ATD_TYPE_IDX(attr_idx);

   result_type_idx	= gen_auto_length(attr_idx, &opnd);

   /* Do not need allocate or deallocate for an automatic pointee. */

   if (ATD_CLASS(attr_idx) == CRI__Pointee) {
      goto EXIT;
   }

   if (ATD_AUXILIARY(attr_idx)) {
      base_tmp_idx	= auto_aux_base_tmp_idx;
      base_len_idx	= auto_aux_base_len_idx;
      base_ir_idx	= auto_aux_base_ir_idx;
      base_word_align	= auto_aux_base_word_align;
      opr		= SSD_Alloc_Opr;
   }
   else if (ATD_SYMMETRIC(attr_idx)) {
      base_tmp_idx	= auto_sym_base_tmp_idx;
      base_len_idx	= auto_sym_base_len_idx;
      base_ir_idx	= auto_sym_base_ir_idx;
      base_word_align	= auto_sym_base_word_align;
      opr		= Symmetric_Alloc_Opr;
   }
   else {
      base_tmp_idx	= auto_base_tmp_idx;
      base_len_idx	= auto_base_len_idx;
      base_ir_idx	= auto_base_ir_idx;
      base_word_align	= auto_base_word_align;
      opr		= Alloc_Opr;
   }

   /* There are two global variables and one static variable to control */
   /* the automatic implementation.                                     */
   /* auto_base_tmp_idx -> This is the allocation tmp.                  */
   /* auto_base_len_idx -> This is the accumulated length tmp.          */
   /*                      This increases for each new automatic var.   */
   /* base_ir_idx       -> This is the ir index to the allocation length*/
   /*                      for auto_base_tmp_idx.  It gets updated each */
   /*                      time there is a new length.                  */

   if (base_tmp_idx == NULL_IDX) {

      /* First automatic or auxiliary variable.  There are three lists. */
      /* Plain, AUXILIARY and SYMMETRIC automatics.                     */

      base_len_fld	= CN_Tbl_Idx;
      base_len_idx	= CN_INTEGER_ZERO_IDX;

      GEN_COMPILER_TMP_ASG(base_ir_idx,
                           base_tmp_idx,
                           TRUE,		/* Semantics is done */
                           stmt_start_line,
                           stmt_start_col,
                           CG_INTEGER_DEFAULT_TYPE,
                           Priv);

      AT_SEMANTICS_DONE(base_tmp_idx)	= TRUE;
      ATD_STOR_BLK_IDX(base_tmp_idx)	= SCP_SB_STACK_IDX(curr_scp_idx);

      NTR_IR_TBL(alloc_idx);
      IR_OPR(alloc_idx)		= opr;
      IR_TYPE_IDX(alloc_idx)	= TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(alloc_idx)	= line;
      IR_COL_NUM(alloc_idx)	= column;
      IR_LINE_NUM_L(alloc_idx)	= line;
      IR_COL_NUM_L(alloc_idx)	= column;
      IR_LINE_NUM_R(alloc_idx)	= line;
      IR_COL_NUM_R(alloc_idx)	= column;

      /* IR_IDX_L(alloc_idx) gets filled in with length, each time the */
      /* length changes, so at the end, it has the correct length.     */
      /* This opr has no IR_FLD_R or IR_IDX_R.                         */

      IR_FLD_R(base_ir_idx)	= IR_Tbl_Idx;
      IR_IDX_R(base_ir_idx)	= alloc_idx;
      IR_LINE_NUM_R(base_ir_idx)= line;
      IR_COL_NUM_R(base_ir_idx)	= column;
      base_ir_idx		= alloc_idx;

      NTR_ATTR_LIST_TBL(al_idx);
      AL_ATTR_IDX(al_idx)	= base_tmp_idx;

      if (auto_base_list_start == NULL_IDX) {
         auto_base_list_start	= al_idx;
         auto_base_list_end	= al_idx;
      }
      else {
         AL_NEXT_IDX(auto_base_list_end)	= al_idx;
         auto_base_list_end			= al_idx;
      }

      /* Generate the dealloc */
   
      NTR_IR_TBL(ir_idx);

      if (ATD_AUXILIARY(attr_idx)) {
         IR_OPR(ir_idx)		= SSD_Dealloc_Opr;
      }
      else if (ATD_SYMMETRIC(attr_idx)) {
         IR_OPR(ir_idx)		= Symmetric_Dealloc_Opr;
      }
      else {
         IR_OPR(ir_idx)		= Dealloc_Opr;
      }
      IR_TYPE_IDX(ir_idx)	= TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)	= line;
      IR_COL_NUM(ir_idx)	= column;
      IR_FLD_L(ir_idx)		= AT_Tbl_Idx;
      IR_IDX_L(ir_idx)		= base_tmp_idx;
      IR_LINE_NUM_L(ir_idx)	= line;
      IR_COL_NUM_L(ir_idx)	= column;
      sh_idx			= ntr_sh_tbl();
      SH_COMPILER_GEN(sh_idx)	= TRUE;
      SH_P2_SKIP_ME(sh_idx)	= TRUE;
      SH_GLB_LINE(sh_idx)	= stmt_start_line;
      SH_COL_NUM(sh_idx)	= stmt_start_col;
      SH_IR_IDX(sh_idx)		= ir_idx;

      if (SCP_EXIT_IR_SH_IDX(curr_scp_idx) != NULL_IDX) {
         SH_NEXT_IDX(sh_idx) = SCP_EXIT_IR_SH_IDX(curr_scp_idx);
         SH_PREV_IDX(SCP_EXIT_IR_SH_IDX(curr_scp_idx)) = sh_idx;
      }

      SCP_EXIT_IR_SH_IDX(curr_scp_idx) = sh_idx;

      /* Set base_word_align for next call to this routine. */

# if defined(_TARGET_BYTE_ADDRESS)

      if (TYP_TYPE(type_idx) == Character ||
          (TYP_TYPE(type_idx) == Structure && 
          ATT_CHAR_SEQ(TYP_IDX(type_idx)) ) ) {
         base_word_align	= FALSE;
      }
      else {

# if defined(_TARGET_OS_MAX)
         base_word_align	= !(PACK_HALF_WORD_TEST_CONDITION(type_idx));
# else
         base_word_align	= TRUE;
# endif
      }
# endif
   }
   else {
      base_len_fld	= AT_Tbl_Idx;
      adjust		= FALSE;

# if defined(_TARGET_BYTE_ADDRESS)

      /* If this type is numeric, it needs to be aligned on a word boundary. */
      /* This check if it is necessary to generate code to do this.          */

      if (TYP_TYPE(type_idx) == Character ||
          (TYP_TYPE(type_idx) == Structure && 
           ATT_CHAR_SEQ(TYP_IDX(type_idx)) ) ) {

         /* Intentionally blank */

         base_word_align	= FALSE;
      }
      else {

         if (!base_word_align) {
            adjust		= TRUE;
         }

# if defined(_TARGET_OS_MAX)

         /* We do double word packing on MPP.  This word needs to */
         /* be either aligned on a word boundary or a half word   */
         /* boundary, so make sure it is by checking the type.    */

         if (PACK_HALF_WORD_TEST_CONDITION(type_idx)) {
            word_byte_size	= TARGET_BYTES_PER_WORD / 2;
            base_word_align	= FALSE;
         }
         else {
            word_byte_size	= TARGET_BYTES_PER_WORD;
            base_word_align	= TRUE;
         }
# else
         word_byte_size		= TARGET_BYTES_PER_WORD;
         base_word_align	= TRUE;
# endif

      }

# endif

# if defined(_TARGET_DOUBLE_ALIGN)

      /* Check if this next item needs to be double aligned.  If it does  */
      /* make sure that the accumulated length is a double word boundary. */

      if (DALIGN_TEST_CONDITION(type_idx)) {
         word_byte_size		= (2 * TARGET_BYTES_PER_WORD);
         adjust			= TRUE;
         base_word_align	= TRUE;
      }
# endif

      if (adjust) {
         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)		= Plus_Opr;
         IR_TYPE_IDX(ir_idx)	= result_type_idx;
         IR_LINE_NUM(ir_idx)	= line;
         IR_COL_NUM(ir_idx)	= column;
         IR_LINE_NUM_L(ir_idx)	= line;
         IR_COL_NUM_L(ir_idx)	= column;
         IR_LINE_NUM_R(ir_idx)	= line;
         IR_COL_NUM_R(ir_idx)	= column;
         IR_FLD_L(ir_idx)	= CN_Tbl_Idx;
         IR_IDX_L(ir_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              (word_byte_size - 1));
         IR_FLD_R(ir_idx)	= AT_Tbl_Idx;
         IR_IDX_R(ir_idx)	= base_len_idx;

         NTR_IR_TBL(div_idx);
         IR_OPR(div_idx)	= Div_Opr;
         IR_TYPE_IDX(div_idx)	= result_type_idx;
         IR_LINE_NUM(div_idx)	= line;
         IR_COL_NUM(div_idx)	= column;
         IR_LINE_NUM_L(div_idx)	= line;
         IR_COL_NUM_L(div_idx)	= column;
         IR_LINE_NUM_R(div_idx)	= line;
         IR_COL_NUM_R(div_idx)	= column;
         IR_FLD_R(div_idx)	= CN_Tbl_Idx;
         word_byte_size_idx	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                               	              word_byte_size);
         IR_IDX_R(div_idx)	= word_byte_size_idx;
         IR_FLD_L(div_idx)	= IR_Tbl_Idx;
         IR_IDX_L(div_idx)	= ir_idx;

         NTR_IR_TBL(mult_idx);
         IR_OPR(mult_idx)	= Mult_Opr;
         IR_TYPE_IDX(mult_idx)	= result_type_idx;
         IR_LINE_NUM(mult_idx)	= line;
         IR_COL_NUM(mult_idx)	= column;
         IR_LINE_NUM_L(mult_idx)= line;
         IR_COL_NUM_L(mult_idx)	= column;
         IR_LINE_NUM_R(mult_idx)= line;
         IR_COL_NUM_R(mult_idx)	= column;
         IR_FLD_R(mult_idx)	= CN_Tbl_Idx;
         IR_IDX_R(mult_idx)	= word_byte_size_idx;
         IR_FLD_L(mult_idx)	= IR_Tbl_Idx;
         IR_IDX_L(mult_idx)	= div_idx;

         GEN_COMPILER_TMP_ASG(tmp_ir_idx,
                              base_len_idx,
                              TRUE,		/* Semantics is done */
                              stmt_start_line,
                              stmt_start_col,
                              result_type_idx,
                              Priv);

         IR_FLD_R(tmp_ir_idx)	= IR_Tbl_Idx;
         IR_IDX_R(tmp_ir_idx)	= mult_idx;

         gen_sh(After,
                Automatic_Base_Size_Stmt,
                line,
                column,
                FALSE,
                FALSE,
                TRUE);  /* Compiler generated */
   
         base_len_fld				= AT_Tbl_Idx;
         SH_IR_IDX(curr_stmt_sh_idx)		= tmp_ir_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx)	= TRUE;
      }
   }

   /* Generate a base for this automatic.  It is equal to the base_tmp_idx */
   /* plus the accumulated length  (base_len_idx).   This tmp goes on the  */
   /* automatic tmp list, because it cannot be added to the IR until all   */
   /* the lengths for all the automatics have been generated.              */

   GEN_COMPILER_TMP_ASG(new_base_ir_idx,
                        new_base_attr_idx,
                        TRUE,		/* Semantics is done */
                        line,
                        column,
                        result_type_idx,
                        Priv);

   NTR_IR_TBL(plus_idx);

   IR_IDX_R(new_base_ir_idx)		= plus_idx;
   IR_FLD_R(new_base_ir_idx)		= IR_Tbl_Idx;
   IR_LINE_NUM_R(new_base_ir_idx)	= line;
   IR_COL_NUM_R(new_base_ir_idx)	= column;
   ATD_AUTO_BASE_IDX(attr_idx)		= new_base_attr_idx;

   IR_OPR(plus_idx)		= Plus_Opr;
   IR_TYPE_IDX(plus_idx)	= result_type_idx;
   IR_IDX_L(plus_idx)		= base_tmp_idx;  /* Alloc base */
   IR_FLD_L(plus_idx)		= AT_Tbl_Idx;
   IR_IDX_R(plus_idx)		= base_len_idx;  /* Old accumulated len */
   IR_FLD_R(plus_idx)		= base_len_fld;
   IR_LINE_NUM(plus_idx)	= line;
   IR_COL_NUM(plus_idx)		= column;
   IR_LINE_NUM_L(plus_idx)	= line;
   IR_COL_NUM_L(plus_idx)	= column;
   IR_LINE_NUM_R(plus_idx)	= line;
   IR_COL_NUM_R(plus_idx)	= column;

   NTR_ATTR_LIST_TBL(al_idx);
   AL_ATTR_IDX(al_idx)			= new_base_attr_idx;
   AL_NEXT_IDX(auto_base_list_end)	= al_idx;
   auto_base_list_end			= al_idx;

   /* Generate  tmp = auto_base_len_idx (tmp holding old accumulated len) + */
   /* len of this variable.  This new tmp then becomes base_len_idx.        */

   NTR_IR_TBL(new_len_idx);
   IR_OPR(new_len_idx)		= Plus_Opr;
   IR_TYPE_IDX(new_len_idx)	= result_type_idx;
   IR_IDX_L(new_len_idx)	= base_len_idx;  /* Old accumulated len */
   IR_FLD_L(new_len_idx)	= base_len_fld;
   IR_LINE_NUM(new_len_idx)	= line;
   IR_COL_NUM(new_len_idx)	= column;
   IR_LINE_NUM_L(new_len_idx)	= line;
   IR_COL_NUM_L(new_len_idx)	= column;
   IR_LINE_NUM_R(new_len_idx)	= line;
   IR_COL_NUM_R(new_len_idx)	= column;

   /* The right side of new_len_idx gets the length accumulation  */
   /* for the variable.                                           */

   COPY_OPND(IR_OPND_R(new_len_idx), opnd);

   gen_sh(After,
          Automatic_Base_Size_Stmt,
          line,
          column,
          FALSE,
          FALSE,
          TRUE);  /* Compiler generated */
    
   GEN_COMPILER_TMP_ASG(len_ir_idx,
                        base_len_idx,    /* New accumulated length */
                        TRUE,		/* Semantics is done      */
                        line,
                        column,
                        result_type_idx,
                        Priv);

   base_len_fld				= AT_Tbl_Idx;
   SH_IR_IDX(curr_stmt_sh_idx)		= len_ir_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx)	= TRUE;
   IR_FLD_R(len_ir_idx)			= IR_Tbl_Idx;
   IR_IDX_R(len_ir_idx)			= new_len_idx;
   IR_LINE_NUM_R(len_ir_idx)		= line;
   IR_COL_NUM_R(len_ir_idx)		= column;

   /* Change the length being allocated to the new accumulated length. */

   IR_IDX_L(base_ir_idx)		= base_len_idx;
   IR_FLD_L(base_ir_idx)		= base_len_fld;

   if (SCP_ENTRY_IDX(curr_scp_idx) != NULL_IDX) {

      if (save_next_sh_idx != NULL_IDX) {
         sh_idx = SH_PREV_IDX(save_next_sh_idx);
      }
      else {

         sh_idx = curr_stmt_sh_idx;

         while (SH_NEXT_IDX(sh_idx) != NULL_IDX) {
            sh_idx = SH_NEXT_IDX(sh_idx);
         }
      }

      insert_sh_after_entries(attr_idx, 
                              start_sh_idx,
                              sh_idx,
                              FALSE,     /* Don't generate tmp = 0  */
                              TRUE);     /* Advance ATP_FIRST_SH_IDX */
   }

   if (ATD_AUXILIARY(attr_idx)) {
      auto_aux_base_tmp_idx	= base_tmp_idx;
      auto_aux_base_len_idx	= base_len_idx;
      auto_aux_base_ir_idx	= base_ir_idx;
      auto_aux_base_word_align	= base_word_align;
   }
   else if (ATD_SYMMETRIC(attr_idx)) {
      auto_sym_base_tmp_idx	= base_tmp_idx;
      auto_sym_base_len_idx	= base_len_idx;
      auto_sym_base_ir_idx	= base_ir_idx;
      auto_sym_base_word_align	= base_word_align;
   }
   else {
      auto_base_tmp_idx		= base_tmp_idx;
      auto_base_len_idx		= base_len_idx;
      auto_base_ir_idx		= base_ir_idx;
      auto_base_word_align	= base_word_align;
   }

FINISH:

   if (attr_idx == NULL_IDX) {

      /* Automatics are done for this scope.  Generate the rest of bounds ir */
      /* and clear static variables for the next scope.                      */

      /* If the address is in bytes, but the allocation is in words,    */
      /* generate additional code on the allocation to change the byte  */
      /* length to a word length.                                       */

      /* NOTE: We do not handle the case of HEAP_REQUEST_IN_BYTES and   */
      /* TARGET_WORD_ADDRESS.  Code will have to be added, if that case */
      /* comes up.                                                      */

# if defined(_HEAP_REQUEST_IN_WORDS) && defined(_TARGET_BYTE_ADDRESS)

      if (auto_base_ir_idx != NULL_IDX) {
         COPY_OPND(opnd, IR_OPND_L(auto_base_ir_idx));
         gen_word_align_byte_length_ir(&opnd);

         expr_desc.rank	= 0;
         xref_state	= CIF_No_Usage_Rec;

         expr_semantics(&opnd, &expr_desc);

         COPY_OPND(IR_OPND_L(auto_base_ir_idx), opnd);
      }

      if (auto_aux_base_ir_idx != NULL_IDX) {
         COPY_OPND(opnd, IR_OPND_L(auto_aux_base_ir_idx));
         gen_word_align_byte_length_ir(&opnd);

         expr_desc.rank	= 0;
         xref_state	= CIF_No_Usage_Rec;

         expr_semantics(&opnd, &expr_desc);

         COPY_OPND(IR_OPND_L(auto_aux_base_ir_idx), opnd);
      }

      if (auto_sym_base_ir_idx != NULL_IDX) {
         COPY_OPND(opnd, IR_OPND_L(auto_sym_base_ir_idx));
         gen_word_align_byte_length_ir(&opnd);

         expr_desc.rank	= 0;
         xref_state	= CIF_No_Usage_Rec;

         expr_semantics(&opnd, &expr_desc);

         COPY_OPND(IR_OPND_L(auto_sym_base_ir_idx), opnd);
      }

# endif

      al_idx	= auto_base_list_start;
      sh_idx	= curr_stmt_sh_idx;

      while (al_idx != NULL_IDX) {
         gen_sh(After,
                Automatic_Base_Calc_Stmt,
                AT_DEF_LINE(AL_ATTR_IDX(al_idx)),
                AT_DEF_COLUMN(AL_ATTR_IDX(al_idx)),
                FALSE,
                FALSE,
                TRUE);  /* Compiler generated */

         SH_IR_IDX(curr_stmt_sh_idx)	= ATD_TMP_IDX(AL_ATTR_IDX(al_idx));
         SH_P2_SKIP_ME(curr_stmt_sh_idx)= TRUE;
         al_idx				= AL_NEXT_IDX(al_idx);
      }

      if (auto_base_list_start != NULL_IDX &&
          SCP_ENTRY_IDX(curr_scp_idx) != NULL_IDX) {
         insert_sh_after_entries(auto_base_tmp_idx, 
                                 sh_idx,
                                 curr_stmt_sh_idx,
                                 FALSE,     /* Don't generate tmp = 0  */
                                 TRUE);     /* Advance ATP_FIRST_SH_IDX */
      }

      auto_base_ir_idx		= NULL_IDX;
      auto_base_len_idx		= NULL_IDX;
      auto_base_list_end	= NULL_IDX;
      auto_base_list_start	= NULL_IDX;
      auto_base_tmp_idx		= NULL_IDX;
      auto_base_word_align	= TRUE;
      auto_aux_base_ir_idx	= NULL_IDX;
      auto_aux_base_len_idx	= NULL_IDX;
      auto_aux_base_tmp_idx	= NULL_IDX;
      auto_aux_base_word_align	= TRUE;
      auto_sym_base_ir_idx	= NULL_IDX;
      auto_sym_base_len_idx	= NULL_IDX;
      auto_sym_base_tmp_idx	= NULL_IDX;
      auto_sym_base_word_align	= TRUE;
   }

EXIT:

   TRACE (Func_Exit, "gen_multiple_automatic_allocate", NULL);

   return;

}  /* gen_multiple_automatic_allocate */
# endif

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine resolves the lower and upper bounds to a constant or a   *|
|*	temp.  Calculate the extent and stride multiplier for each dimension. *|
|*									      *|
|* Input parameters:							      *|
|*      attr_idx -> Index to attribute for array.                             *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
static	void	distribution_resolution(int 	attr_idx)
{
   int			bd_idx;
   int			dim;
   expr_arg_type	expr_desc;
   opnd_type		opnd;


   TRACE (Func_Entry, "distribution_resolution", NULL);

   bd_idx	= ATD_DISTRIBUTION_IDX(attr_idx);

   if (!BD_RESOLVED(bd_idx)) {

      for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

         if (BD_CYCLIC_FLD(bd_idx, dim) != NO_Tbl_Idx) {
            OPND_FLD(opnd)		= BD_CYCLIC_FLD(bd_idx, dim);
            OPND_IDX(opnd)		= BD_CYCLIC_IDX(bd_idx, dim);
            OPND_LINE_NUM(opnd)		= BD_LINE_NUM(bd_idx);
            OPND_COL_NUM(opnd)		= BD_COLUMN_NUM(bd_idx);
   
            expr_desc.rank 		= 0;
            xref_state     		= CIF_No_Usage_Rec;
   
            expr_semantics(&opnd, &expr_desc);

            BD_CYCLIC_FLD(bd_idx, dim)	= OPND_FLD(opnd);
            BD_CYCLIC_IDX(bd_idx, dim)	= OPND_IDX(opnd);
         }
   
         if (BD_ONTO_FLD(bd_idx, dim) != NO_Tbl_Idx) {
            OPND_FLD(opnd)		= BD_ONTO_FLD(bd_idx, dim);
            OPND_IDX(opnd)		= BD_ONTO_IDX(bd_idx, dim);
            OPND_LINE_NUM(opnd)		= BD_LINE_NUM(bd_idx);
            OPND_COL_NUM(opnd)		= BD_COLUMN_NUM(bd_idx);

            expr_desc.rank 		= 0;
            xref_state     		= CIF_No_Usage_Rec;
   
            expr_semantics(&opnd, &expr_desc);

            BD_ONTO_FLD(bd_idx, dim)	= OPND_FLD(opnd);
            BD_ONTO_IDX(bd_idx, dim)	= OPND_IDX(opnd);
         }
      }
   }

   /* KAY - Semantic checks here */

   TRACE (Func_Exit, "distribution_resolution", NULL);

   return;

}  /* distribution_resolution */

#ifdef KEY /* Bug 4197 */
/*
 * This implements the passage of the F95 standard (14.1.2.3): "If a generic
 * name is the same as the name of a generic intrinsic procedure, the generic
 * intrinsic procedure is not accessible if the procedures in the interface
 * and the intrinsic procedure are not all functions or not all subroutines."
 *
 * For generic interfaces using interface bodies, we were able to remove
 * the intrinsics from the interface in start_new_subpgm() before we added
 * the user-provided procedures. But for generic interfaces using "module
 * procedure", we added the user-provided procedures before we knew
 * whether they were functions, and "verify_interface()" is the place to
 * remove the intrinsics if need be.
 *
 * Call ignore_intrinsics() when the first specific procedure in the list
 * disagrees with the generic interface w.r.t. function-ness. If the generic
 * interface is intrinsic and the first specific procedure is not intrinsic,
 * then get rid of the specific intrinsic procedures, change the interface to
 * match the first specific procedure, and proceed with the checking. If
 * there's still a conflict after the intrinsics are gone, this really is
 * an error.
 *
 * BTW, can't test AT_INTRINSIC(interface_idx) because when one of the
 * specific procedures named in the "module procedure" statement overloads
 * the interface name itself, that attribute has been turned off.
 *
 * we rely on the fact that intrinsics follow non-intrinsics in the list.
 */
static boolean
ignore_intrinsics(int first, int interface_idx, int specific_attr_idx,
  interface_type desired_type)
{
  if ((!first) || ATP_PROC(specific_attr_idx) == Intrin_Proc) {
    return 0;
  }
  ATI_INTERFACE_CLASS(interface_idx) = desired_type;
  AT_IS_INTRIN(interface_idx) = FALSE;
  for (int curr_sn_idx = ATI_FIRST_SPECIFIC_IDX(interface_idx);
    curr_sn_idx != NULL_IDX; ) {
    int next_sn_idx = SN_SIBLING_LINK(curr_sn_idx);
    int next_attr_idx = SN_ATTR_IDX(next_sn_idx);
    /* ATP_PROC tells us this is a "real" intrinsic */
    if ((AT_OBJ_CLASS(next_attr_idx) == Pgm_Unit) &&
      ATP_PROC(next_attr_idx) == Intrin_Proc) {
      SN_SIBLING_LINK(curr_sn_idx) = NULL_IDX;
      break;
    }
    /* AT_IS_INTRIN merely tells us this has the same name as an intrinsic */
    AT_IS_INTRIN(next_attr_idx) = FALSE;
    curr_sn_idx = next_sn_idx;
  }
  return 1;
}
#endif /* KEY Bug 4197 */
/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Verify the specific interfaces in a generic interface.                *|
|*      Check for ambiguity and other rules for generic interfaces and        *|
|*      overloaded operators and assignment.                                  *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      interface_idx - index to generic interface attr.                      *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
static void verify_interface(int        interface_idx)

{
   boolean	ambiguous;
   int		attr_idx;
   int		correct_num;
   int		curr_attr_idx;
   int		curr_darg_idx;
   int		curr_darg_sn_idx;
   int		curr_num_dargs;
   int		curr_sn_idx;
   int		curr_type_idx;
   int		darg_idx;
   int		darg_sn_idx;
   boolean	found_intrin		= FALSE;
   int		i;
   int		idx;
   int		ktr_sn_idx;
   int		kwd_darg_idx;
   int		kwd_sn_idx;
   int		loop_cnt;
   int		num_dargs;
   int		optional_sn_idx;
#ifdef KEY /* Bug 10177 */
   int		rank_l = 0;
   int		rank_r = 0;
#else /* KEY Bug 10177 */
   int		rank_l;
   int		rank_r;
#endif /* KEY Bug 10177 */
   boolean	same_dargs;
   int		save_curr_darg_sn_idx;
   int		save_curr_num_dargs;
   int		save_darg_sn_idx;
   int		save_num_dargs;
   int		sn_idx;
#ifdef KEY /* Bug 10177 */
   int		type_idx_l = 0;
   int		type_idx_r = 0;
#else /* KEY Bug 10177 */
   int		type_idx_l;
   int		type_idx_r;
#endif /* KEY Bug 10177 */



   TRACE (Func_Entry, "verify_interface", NULL);

   if (AT_DCL_ERR(interface_idx)) {
      goto EXIT;
   }

   curr_sn_idx = ATI_FIRST_SPECIFIC_IDX(interface_idx);

#ifdef KEY /* Bug 4197 */
   boolean first = TRUE;
#endif /* KEY Bug 4197 */
   while (curr_sn_idx != NULL_IDX) {
      curr_attr_idx = SN_ATTR_IDX(curr_sn_idx);

      if (AT_IS_INTRIN(curr_attr_idx)) {
         found_intrin  = TRUE;
         curr_type_idx = (ATP_PGM_UNIT(curr_attr_idx) != Function) ? NULL_IDX :
                              ATD_TYPE_IDX(ATP_RSLT_IDX(curr_attr_idx));

         if (AT_DCL_ERR(curr_attr_idx)) {
            curr_sn_idx = SN_SIBLING_LINK(curr_sn_idx);
            continue;
         }
      }
      else {

         if (found_intrin) { /* A non-intrinsic follows the intrinsics */
            PRINTMSG(AT_DEF_LINE(curr_attr_idx), 1534, Internal,
                     AT_DEF_COLUMN(curr_attr_idx),
                     AT_OBJ_NAME_PTR(curr_attr_idx));
         }

         if (ATP_PROC(curr_attr_idx) == Module_Proc &&
             ATP_PGM_UNIT(curr_attr_idx) == Pgm_Unknown) {

            while (AT_ATTR_LINK(curr_attr_idx) != NULL_IDX) {
               curr_attr_idx = AT_ATTR_LINK(curr_attr_idx);
            }

            if (AT_OBJ_CLASS(curr_attr_idx) == Interface) {
               curr_attr_idx = ATI_PROC_IDX(curr_attr_idx);
            }

            if (curr_attr_idx == NULL_IDX) {

               if (!AT_DCL_ERR(SN_ATTR_IDX(curr_sn_idx))) {

                  /* The module procedure must be defined or use associated */

                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 368, Error, 
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(SN_ATTR_IDX(curr_sn_idx)));
               }
               curr_attr_idx		 = SN_ATTR_IDX(curr_sn_idx);
               AT_DCL_ERR(curr_attr_idx) = TRUE;
               AT_DCL_ERR(interface_idx) = TRUE;
               break;
            }

            if (AT_OBJ_CLASS(curr_attr_idx) != Pgm_Unit ||
                ATP_PROC(curr_attr_idx) != Module_Proc ||
                ATP_PGM_UNIT(curr_attr_idx) == Pgm_Unknown) {

               /* MODULE PROCEDURE specified in INTERFACE, but the MODULE  */
               /* PROCEDURE was never accessed in the MODULE or from USE.  */

               if (!AT_DCL_ERR(curr_attr_idx) && 
                   !AT_DCL_ERR(SN_ATTR_IDX(curr_sn_idx))) {
                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 368, Error, 
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(curr_attr_idx));
               }
               curr_attr_idx		 = SN_ATTR_IDX(curr_sn_idx);
               AT_DCL_ERR(curr_attr_idx) = TRUE;
               AT_DCL_ERR(interface_idx) = TRUE;
               break;
            }

            SN_ATTR_IDX(curr_sn_idx) = curr_attr_idx;
            SN_NAME_IDX(curr_sn_idx) = AT_NAME_IDX(curr_attr_idx);
         }


         attr_semantics(curr_attr_idx, FALSE);

         curr_type_idx = (ATP_PGM_UNIT(curr_attr_idx) != Function) ? NULL_IDX :
                              ATD_TYPE_IDX(ATP_RSLT_IDX(curr_attr_idx));

         if (AT_DCL_ERR(curr_attr_idx)) {
            curr_sn_idx = SN_SIBLING_LINK(curr_sn_idx);
            continue;
         }

         if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module && 
             !AT_PRIVATE(interface_idx)) {

            if (ATP_PGM_UNIT(curr_attr_idx) == Function &&
                TYP_TYPE(curr_type_idx) == Structure &&
                AT_PRIVATE(TYP_IDX(curr_type_idx)) &&
                !AT_USE_ASSOCIATED(TYP_IDX(curr_type_idx))) { /* Interp 161 */

               /* Issue error if generic interface is PUBLIC, but one of its  */
               /* routines has a FUNCTION result that is a private type.      */

               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 686, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(interface_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx));
               AT_DCL_ERR(interface_idx) = TRUE;
            }

            /* Check everything in the darg list to make sure there  */
            /* are no PRIVATE types used for the dummy arguments.    */
            /* Don't check intrinsic dargs.  They are not typed.     */

            for (i = (ATP_EXTRA_DARG(curr_attr_idx) ? 1 : 0);
                 i < ATP_NUM_DARGS(curr_attr_idx); i++) {
               darg_idx = SN_ATTR_IDX(ATP_FIRST_IDX(curr_attr_idx) + i);

               /* Issue error if the Module procedure is PUBLIC, */
               /* but one of its dummy arguments is a PRIVATE    */
               /* type, unless interp 161 applies.               */

               if (AT_OBJ_CLASS(darg_idx) == Data_Obj &&
                   TYP_TYPE(ATD_TYPE_IDX(darg_idx)) == Structure &&
                   AT_PRIVATE(TYP_IDX(ATD_TYPE_IDX(darg_idx))) &&
                   !AT_USE_ASSOCIATED(TYP_IDX(ATD_TYPE_IDX(darg_idx)))) {
                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 687, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(interface_idx),
                           AT_OBJ_NAME_PTR(darg_idx),
                           AT_OBJ_NAME_PTR(curr_attr_idx));
                  AT_DCL_ERR(interface_idx) = TRUE;
               }
            }
         }
      }

      /* single attr checks here */

      switch (ATI_INTERFACE_CLASS(interface_idx)) {
      case Generic_Unknown_Interface:
         ATI_INTERFACE_CLASS(interface_idx) = 
                                 (ATP_PGM_UNIT(curr_attr_idx) == Function) ?
                                       Generic_Function_Interface:
                                       Generic_Subroutine_Interface;
         break;

      case Generic_Function_Interface :

         if (ATP_PGM_UNIT(curr_attr_idx) == Subroutine &&
             !AT_DCL_ERR(interface_idx))               {
#ifdef KEY /* Bug 4197 */
            if (ignore_intrinsics(first, interface_idx, curr_attr_idx,
	      Generic_Subroutine_Interface)) {
	      found_intrin = FALSE;
	      break;
	    }
#endif /* KEY Bug 4197 */
            PRINTMSG(AT_DEF_LINE(interface_idx), 1059, Error,
                     AT_DEF_COLUMN(interface_idx),
                     AT_OBJ_NAME_PTR(interface_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
         }
         break;

      case Generic_Subroutine_Interface :
            
         if (ATP_PGM_UNIT(curr_attr_idx) == Function &&
             !AT_DCL_ERR(interface_idx))             {
#ifdef KEY /* Bug 4197 */
            if (ignore_intrinsics(first, interface_idx, curr_attr_idx,
	      Generic_Function_Interface)) {
	      found_intrin = FALSE;
	      break;
	    }
#endif /* KEY Bug 4197 */
            PRINTMSG(AT_DEF_LINE(interface_idx), 1059, Error,
                     AT_DEF_COLUMN(interface_idx),
                     AT_OBJ_NAME_PTR(interface_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
         }
         break;

      case Defined_Assign_Interface :

         /* must be subroutine with two arguments */

         if (ATP_PGM_UNIT(curr_attr_idx) != Subroutine) {
            PRINTMSG(SN_LINE_NUM(curr_sn_idx), 475, Error,
                     SN_COLUMN_NUM(curr_sn_idx),
                     AT_OBJ_NAME_PTR(curr_attr_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
            AT_DCL_ERR(curr_attr_idx) = TRUE;
         }

         correct_num = (ATP_EXTRA_DARG(curr_attr_idx)) ? 3 : 2;

         if (ATP_NUM_DARGS(curr_attr_idx) != correct_num) {
            PRINTMSG(SN_LINE_NUM(curr_sn_idx), 489, Error,
                     SN_COLUMN_NUM(curr_sn_idx),
                     AT_OBJ_NAME_PTR(curr_attr_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
            AT_DCL_ERR(curr_attr_idx) = TRUE;
         }
         else {
            sn_idx	= (ATP_EXTRA_DARG(curr_attr_idx)) ?
                                         (ATP_FIRST_IDX(curr_attr_idx) + 1) :
                                          ATP_FIRST_IDX(curr_attr_idx);
            attr_idx	= SN_ATTR_IDX(sn_idx);

            if (AT_OPTIONAL(attr_idx)) {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1071, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        "ASSIGNMENT",
                        AT_OBJ_NAME_PTR(interface_idx),  /* Interface name */
                        AT_OBJ_NAME_PTR(curr_attr_idx),  /* Procedure name */
                        AT_OBJ_NAME_PTR(attr_idx));      /* Dummy Arg name */
               AT_DCL_ERR(interface_idx) = TRUE;
            }

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
               type_idx_l	= ATD_TYPE_IDX(attr_idx);
               rank_l		= (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) ?
                                       0 : BD_RANK(ATD_ARRAY_IDX(attr_idx));

               /* first intent = OUT or INOUT           */

               if (ATD_INTENT(attr_idx) == Intent_In ||
                   ATD_INTENT(attr_idx) == Intent_Unseen) {

                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1074, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                           AT_OBJ_NAME_PTR(attr_idx),      /* Dummy Arg name */
                           "INOUT");
                  AT_DCL_ERR(interface_idx) = TRUE;
               }
            }
# ifdef _DEBUG
            else if (AT_OBJ_CLASS(attr_idx) != Pgm_Unit) {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 884, Internal,
                        SN_COLUMN_NUM(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx));
            }
# endif
            else {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1073, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                        "ASSIGNMENT",
                        AT_OBJ_NAME_PTR(interface_idx), /* interface name */
                        AT_OBJ_NAME_PTR(attr_idx));     /* Dummy Arg name */
               AT_DCL_ERR(interface_idx) = TRUE;
            }

            sn_idx++;
            attr_idx = SN_ATTR_IDX(sn_idx);

            if (AT_OPTIONAL(attr_idx)) {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1071, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        "ASSIGNMENT",
                        AT_OBJ_NAME_PTR(interface_idx),  /* Interface name */
                        AT_OBJ_NAME_PTR(curr_attr_idx),  /* Procedure name */
                        AT_OBJ_NAME_PTR(attr_idx));      /* Dummy Arg name */
               AT_DCL_ERR(interface_idx) = TRUE;
            }

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
               type_idx_r	= ATD_TYPE_IDX(attr_idx);
               rank_r		= (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) ?
                                       0 : BD_RANK(ATD_ARRAY_IDX(attr_idx));

               /* second intent = IN                    */

               if (ATD_INTENT(attr_idx) != Intent_In) {
                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1074, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                           AT_OBJ_NAME_PTR(attr_idx),      /* Dummy Arg name */
                           "IN");
                  AT_DCL_ERR(interface_idx) = TRUE;
               }
               else if (operation_is_intrinsic((operator_type)
                                               ATI_DEFINED_OPR(interface_idx),
                                               type_idx_l,
                                               rank_l,
                                               type_idx_r,
                                               rank_r)) {

                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 495, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(curr_attr_idx),
                           AT_OBJ_NAME_PTR(interface_idx));
                  AT_DCL_ERR(interface_idx) = TRUE;
               }
            }
# ifdef _DEBUG
            else if (AT_OBJ_CLASS(attr_idx) != Pgm_Unit) {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 884, Internal,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx));
            }
# endif
            else {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1073, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                        "ASSIGNMENT",
                        AT_OBJ_NAME_PTR(interface_idx), /* interface name */
                        AT_OBJ_NAME_PTR(attr_idx));     /* Dummy Arg name */
               AT_DCL_ERR(interface_idx) = TRUE;
            }
         }
         break;


      case Defined_Unary_Interface : /* must be function with one argument */

         if (ATP_PGM_UNIT(curr_attr_idx) != Function) {
            PRINTMSG(SN_LINE_NUM(curr_sn_idx), 492, Error,
                     SN_COLUMN_NUM(curr_sn_idx),
                     AT_OBJ_NAME_PTR(curr_attr_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
            AT_DCL_ERR(curr_attr_idx) = TRUE;
         }
         else if (TYP_TYPE(curr_type_idx) == Character &&
                  TYP_CHAR_CLASS(curr_type_idx) == Assumed_Size_Char) {

            /* function result cannot have assumed char length */

            PRINTMSG(SN_LINE_NUM(curr_sn_idx), 493, Error,
                     SN_COLUMN_NUM(curr_sn_idx),
                     AT_OBJ_NAME_PTR(curr_attr_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
            AT_DCL_ERR(curr_attr_idx) = TRUE;
         }

         correct_num = (ATP_EXTRA_DARG(curr_attr_idx)) ? 2 : 1;

         if (ATP_NUM_DARGS(curr_attr_idx) != correct_num) {
            PRINTMSG(SN_LINE_NUM(curr_sn_idx), 494, Error,
                     SN_COLUMN_NUM(curr_sn_idx),
                     AT_OBJ_NAME_PTR(curr_attr_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
            AT_DCL_ERR(curr_attr_idx) = TRUE;
         }
         else {
            sn_idx	= (ATP_EXTRA_DARG(curr_attr_idx)) ? 
                                    (ATP_FIRST_IDX(curr_attr_idx) + 1) :
                                     ATP_FIRST_IDX(curr_attr_idx);
            attr_idx	= SN_ATTR_IDX(sn_idx);

            if (AT_OPTIONAL(attr_idx)) {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1071, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        "OPERATOR",
                        AT_OBJ_NAME_PTR(interface_idx),  /* Interface name */
                        AT_OBJ_NAME_PTR(curr_attr_idx),  /* Procedure name */
                        AT_OBJ_NAME_PTR(attr_idx));      /* Dummy Arg name */
               AT_DCL_ERR(interface_idx) = TRUE;
            }

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
               type_idx_l	= ATD_TYPE_IDX(attr_idx);
               rank_l		= (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) ?
                                       0 : BD_RANK(ATD_ARRAY_IDX(attr_idx));

               /* intent = IN */

               if (ATD_INTENT(attr_idx) != Intent_In) {
                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1072, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(interface_idx), /* interface name */
                           AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                           AT_OBJ_NAME_PTR(attr_idx));     /* Dummy Arg name */

                  AT_DCL_ERR(interface_idx) = TRUE;
               }

               type_idx_r	= TYPELESS_DEFAULT_TYPE;
               rank_r	= 0;

               if (operation_is_intrinsic((operator_type)
                                          ATI_DEFINED_OPR(interface_idx),
                                          type_idx_l,
                                          rank_l,
                                          type_idx_r,
                                          rank_r)) {

                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 495, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(curr_attr_idx),
                           AT_OBJ_NAME_PTR(interface_idx));
                  AT_DCL_ERR(interface_idx) = TRUE;
               }
            }
# ifdef _DEBUG
            else if (AT_OBJ_CLASS(attr_idx) != Pgm_Unit) {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 884, Internal,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx));
            }
# endif
            else {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1073, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                        "OPERATOR",
                        AT_OBJ_NAME_PTR(interface_idx), /* interface name */
                        AT_OBJ_NAME_PTR(attr_idx));     /* Dummy Arg name */
               AT_DCL_ERR(interface_idx) = TRUE;
            }
         }
         break;


      case Defined_Binary_Interface : /* must be function with two arguments  */

         if (ATP_PGM_UNIT(curr_attr_idx) != Function) {
            PRINTMSG(SN_LINE_NUM(curr_sn_idx), 492, Error,
                     SN_COLUMN_NUM(curr_sn_idx),
                     AT_OBJ_NAME_PTR(curr_attr_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
            AT_DCL_ERR(curr_attr_idx) = TRUE;
         }
         else if (TYP_TYPE(curr_type_idx) == Character &&
                  TYP_CHAR_CLASS(curr_type_idx) == Assumed_Size_Char) {

            /* function result cannot have assumed char length */

            PRINTMSG(SN_LINE_NUM(curr_sn_idx), 493, Error,
                     SN_COLUMN_NUM(curr_sn_idx),
                     AT_OBJ_NAME_PTR(curr_attr_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
            AT_DCL_ERR(curr_attr_idx) = TRUE;
         }

         correct_num = (ATP_EXTRA_DARG(curr_attr_idx)) ? 3 : 2;

         if (ATP_NUM_DARGS(curr_attr_idx) != correct_num) {
            PRINTMSG(SN_LINE_NUM(curr_sn_idx), 496, Error,
                     SN_COLUMN_NUM(curr_sn_idx),
                     AT_OBJ_NAME_PTR(curr_attr_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
            AT_DCL_ERR(curr_attr_idx) = TRUE;
         }
         else {
            sn_idx   = (ATP_EXTRA_DARG(curr_attr_idx)) ? 
                                      (ATP_FIRST_IDX(curr_attr_idx) + 1) :
                                       ATP_FIRST_IDX(curr_attr_idx);
            attr_idx = SN_ATTR_IDX(sn_idx);

            if (AT_OPTIONAL(attr_idx)) {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1071, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        "OPERATOR",
                        AT_OBJ_NAME_PTR(interface_idx),  /* Interface name */
                        AT_OBJ_NAME_PTR(curr_attr_idx),  /* Procedure name */
                        AT_OBJ_NAME_PTR(attr_idx));      /* Dummy Arg name */
               AT_DCL_ERR(interface_idx) = TRUE;
            }

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
               type_idx_l	= ATD_TYPE_IDX(attr_idx);
               rank_l		=  (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) ?
                                        0 : BD_RANK(ATD_ARRAY_IDX(attr_idx));

               /* first intent = IN */

               if (ATD_INTENT(attr_idx) != Intent_In) {
                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1072, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(interface_idx), /* interface name */
                           AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                           AT_OBJ_NAME_PTR(attr_idx));     /* Dummy Arg name */
                  AT_DCL_ERR(interface_idx) = TRUE;
               }
            }
# ifdef _DEBUG
            else if (AT_OBJ_CLASS(attr_idx) != Pgm_Unit) {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 884, Internal,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx));
            }
# endif
            else {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1073, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                        "OPERATOR",
                        AT_OBJ_NAME_PTR(interface_idx), /* interface name */
                        AT_OBJ_NAME_PTR(attr_idx));     /* Dummy Arg name */
               AT_DCL_ERR(interface_idx) = TRUE;
            }

            sn_idx++;
            attr_idx = SN_ATTR_IDX(sn_idx);

            if (AT_OPTIONAL(attr_idx)) {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1071, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        "OPERATOR",
                        AT_OBJ_NAME_PTR(interface_idx),  /* Interface name */
                        AT_OBJ_NAME_PTR(curr_attr_idx),  /* Procedure name */
                        AT_OBJ_NAME_PTR(attr_idx));      /* Dummy Arg name */
               AT_DCL_ERR(interface_idx) = TRUE;
            }

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
               type_idx_r	= ATD_TYPE_IDX(attr_idx);
               rank_r		= (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) ?
                                       0 : BD_RANK(ATD_ARRAY_IDX(attr_idx));

               /* second intent = IN                    */

               if (ATD_INTENT(attr_idx) != Intent_In) {
                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1072, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(interface_idx), /* interface name */
                           AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                           AT_OBJ_NAME_PTR(attr_idx));     /* Dummy Arg name */
                  AT_DCL_ERR(interface_idx) = TRUE;
               }
               else if (operation_is_intrinsic((operator_type)
                                               ATI_DEFINED_OPR(interface_idx),
                                               type_idx_l,
                                               rank_l,
                                               type_idx_r,
                                               rank_r)) {

                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 495, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(curr_attr_idx),
                           AT_OBJ_NAME_PTR(interface_idx));
                  AT_DCL_ERR(interface_idx) = TRUE;
               }
            }
# ifdef _DEBUG
            else if (AT_OBJ_CLASS(attr_idx) != Pgm_Unit) {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 884, Internal,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx));
            }
# endif
            else {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1073, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                        "OPERATOR",
                        AT_OBJ_NAME_PTR(interface_idx), /* interface name */
                        AT_OBJ_NAME_PTR(attr_idx));     /* Dummy Arg name */
               AT_DCL_ERR(interface_idx) = TRUE;
            }
         }
         break;


      case Defined_Unary_Or_Binary_Interface :

         /* must be function with one or two arguments      */

         if (ATP_PGM_UNIT(curr_attr_idx) != Function) {
            PRINTMSG(SN_LINE_NUM(curr_sn_idx), 492, Error,
                     SN_COLUMN_NUM(curr_sn_idx),
                     AT_OBJ_NAME_PTR(curr_attr_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
            AT_DCL_ERR(curr_attr_idx) = TRUE;
         }
         else if (TYP_TYPE(curr_type_idx) == Character &&
                  TYP_CHAR_CLASS(curr_type_idx) == Assumed_Size_Char) {

            /* function result cannot have assumed char length */

            PRINTMSG(SN_LINE_NUM(curr_sn_idx), 493, Error,
                     SN_COLUMN_NUM(curr_sn_idx),
                     AT_OBJ_NAME_PTR(curr_attr_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
            AT_DCL_ERR(curr_attr_idx) = TRUE;
         }

         correct_num = (ATP_EXTRA_DARG(curr_attr_idx)) ? 2 : 1;

         if (ATP_NUM_DARGS(curr_attr_idx) != correct_num &&
             ATP_NUM_DARGS(curr_attr_idx) != correct_num + 1) {
            PRINTMSG(SN_LINE_NUM(curr_sn_idx), 497, Error,
                     SN_COLUMN_NUM(curr_sn_idx),
                     AT_OBJ_NAME_PTR(curr_attr_idx));
            AT_DCL_ERR(interface_idx) = TRUE;
            AT_DCL_ERR(curr_attr_idx) = TRUE;
         }
         else {
            sn_idx	= (ATP_EXTRA_DARG(curr_attr_idx)) ? 
                                         (ATP_FIRST_IDX(curr_attr_idx) + 1) :
                                          ATP_FIRST_IDX(curr_attr_idx);
            attr_idx	= SN_ATTR_IDX(sn_idx);

            if (AT_OPTIONAL(attr_idx)) {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1071, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        "OPERATOR",
                        AT_OBJ_NAME_PTR(interface_idx),  /* Interface name */
                        AT_OBJ_NAME_PTR(curr_attr_idx),  /* Procedure name */
                        AT_OBJ_NAME_PTR(attr_idx));      /* Dummy Arg name */
               AT_DCL_ERR(interface_idx) = TRUE;
            }

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
               type_idx_l	= ATD_TYPE_IDX(attr_idx);
               rank_l		= (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) ? 
                                       0 : BD_RANK(ATD_ARRAY_IDX(attr_idx));

               /* first intent = IN              */

               if (ATD_INTENT(attr_idx) != Intent_In) {
                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1072, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(interface_idx), /* interface name */
                           AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                           AT_OBJ_NAME_PTR(attr_idx));     /* Dummy Arg name */
                  AT_DCL_ERR(interface_idx) = TRUE;
               }
            }
# ifdef _DEBUG
            else if (AT_OBJ_CLASS(attr_idx) != Pgm_Unit) {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 884, Internal,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx));
            }
# endif
            else {
               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1073, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                        "OPERATOR",
                        AT_OBJ_NAME_PTR(interface_idx), /* interface name */
                        AT_OBJ_NAME_PTR(attr_idx));     /* Dummy Arg name */
               AT_DCL_ERR(interface_idx) = TRUE;
            }

            if (ATP_NUM_DARGS(curr_attr_idx) == correct_num + 1) {
               sn_idx++;
               attr_idx = SN_ATTR_IDX(sn_idx);

               if (AT_OPTIONAL(attr_idx)) {
                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1071, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           "OPERATOR",
                           AT_OBJ_NAME_PTR(interface_idx),  /* Interface name */
                           AT_OBJ_NAME_PTR(curr_attr_idx),  /* Procedure name */
                           AT_OBJ_NAME_PTR(attr_idx));      /* Dummy Arg name */
                  AT_DCL_ERR(interface_idx) = TRUE;
               }

               if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
                  type_idx_r	= ATD_TYPE_IDX(attr_idx);
                  rank_r	= (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) ?
                                       0 : BD_RANK(ATD_ARRAY_IDX(attr_idx));

                  /* second intent = IN                    */

                  if (ATD_INTENT(attr_idx) != Intent_In) {
                     PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1072, Error,
                              SN_COLUMN_NUM(curr_sn_idx),
                              AT_OBJ_NAME_PTR(interface_idx), /*interface name*/
                              AT_OBJ_NAME_PTR(curr_attr_idx), /*procedure name*/
                              AT_OBJ_NAME_PTR(attr_idx));     /*Dummy Arg name*/
                     AT_DCL_ERR(interface_idx) = TRUE;
                  }
               }
# ifdef _DEBUG
               else if (AT_OBJ_CLASS(attr_idx) != Pgm_Unit) {
                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 884, Internal,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           AT_OBJ_NAME_PTR(curr_attr_idx));
               }
# endif
               else {
                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 1073, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(curr_attr_idx), /* procedure name */
                           "OPERATOR",
                           AT_OBJ_NAME_PTR(interface_idx), /* interface name */
                           AT_OBJ_NAME_PTR(attr_idx));     /* Dummy Arg name */
                  AT_DCL_ERR(interface_idx) = TRUE;
               }
            }
            else {
               type_idx_r = TYPELESS_DEFAULT_TYPE;
            }

            if (!AT_DCL_ERR(interface_idx) &&
                operation_is_intrinsic((operator_type)
                                       ATI_DEFINED_OPR(interface_idx),
                                       type_idx_l,
                                       rank_l,
                                       type_idx_r,
                                       rank_r)) {

               PRINTMSG(SN_LINE_NUM(curr_sn_idx), 495, Error,
                        SN_COLUMN_NUM(curr_sn_idx),
                        AT_OBJ_NAME_PTR(curr_attr_idx),
                        AT_OBJ_NAME_PTR(interface_idx));
               AT_DCL_ERR(interface_idx) = TRUE;
            }
         }
         break;
      }  /* End switch */ 

      /* Go through the rest of the procedures in this interface.  Compare */
      /* them to the present procedure.  Look for ambiguities.             */
      /* Do not do comparisons for intrinsics.                             */

      sn_idx   = (AT_IS_INTRIN(curr_attr_idx)) ? NULL_IDX :
                                                 SN_SIBLING_LINK(curr_sn_idx);

      while (sn_idx != NULL_IDX) {

         attr_idx = SN_ATTR_IDX(sn_idx);

         if (AT_IS_INTRIN(attr_idx)) {

            /* Assume all intrinsics are at the end */

            break;
         }

         if (ATP_EXTRA_DARG(curr_attr_idx)) {
            curr_num_dargs   = ATP_NUM_DARGS(curr_attr_idx) - 1;
            curr_darg_sn_idx = ATP_FIRST_IDX(curr_attr_idx) + 1;
         }
         else {
            curr_num_dargs   = ATP_NUM_DARGS(curr_attr_idx);
            curr_darg_sn_idx = ATP_FIRST_IDX(curr_attr_idx);
         }

         if (ATP_EXTRA_DARG(attr_idx)) {
            num_dargs   = ATP_NUM_DARGS(attr_idx) - 1;
            darg_sn_idx = ATP_FIRST_IDX(attr_idx) + 1;
         }
         else {
            num_dargs   = ATP_NUM_DARGS(attr_idx);
            darg_sn_idx = ATP_FIRST_IDX(attr_idx);
         }

         if (ATP_PGM_UNIT(curr_attr_idx) == ATP_PGM_UNIT(attr_idx) &&
             (curr_num_dargs == num_dargs ||
               ATI_INTERFACE_CLASS(interface_idx) < Defined_Interface)) {
            save_num_dargs		= num_dargs;
            save_darg_sn_idx		= darg_sn_idx;
            save_curr_num_dargs		= curr_num_dargs;
            save_curr_darg_sn_idx	= curr_darg_sn_idx;
            ambiguous			= TRUE;
            loop_cnt			= (curr_num_dargs > num_dargs) ? 
                                          curr_num_dargs: num_dargs;

            for (i = 0; i < loop_cnt; i++) { /* get the dummy arg indexes */

               if (curr_num_dargs != NULL_IDX) {
                  curr_darg_idx    = SN_ATTR_IDX(curr_darg_sn_idx);
                  curr_darg_sn_idx++;
                  curr_num_dargs--;
               }
               else {
                  curr_darg_idx = NULL_IDX;
               }

               if (num_dargs != NULL_IDX) {
                  darg_idx    = SN_ATTR_IDX(darg_sn_idx);
                  darg_sn_idx++;
                  num_dargs--;
               }
               else {
                  darg_idx = NULL_IDX;
               }

               /* Compare Kind, Type and Rank of the dummy arguments.    */
               /* For defined interfaces this is all we need to compare. */
               /* For generic interfaces, we need to compare alot more.  */

               if (curr_darg_idx == NULL_IDX || darg_idx == NULL_IDX) {
                  same_dargs = FALSE;
               }
               else {
                  same_dargs = compare_dummy_arguments(curr_darg_idx, darg_idx);
               }

               if (ATI_INTERFACE_CLASS(interface_idx) >=  Defined_Interface) {

                  if (!same_dargs) {      /* Generic */
                     ambiguous	= FALSE;
                     break;
                  }
                  continue;
               }

               if (curr_darg_idx != NULL_IDX && !AT_OPTIONAL(curr_darg_idx)) {

                  if (same_dargs && 
                      !AT_OPTIONAL(darg_idx) &&
                      !SN_MATCHED_DARG(darg_sn_idx - 1) &&
                      !SN_MATCHED_DARG(curr_darg_sn_idx - 1)) {

                     /* Attempt to match up all non optional dargs. */

                     SN_MATCHED_DARG(darg_sn_idx-1)		= TRUE;
                     SN_MATCHED_DARG(curr_darg_sn_idx-1)	= TRUE;
                  }

                  /* At least one of them shall have both                */

                  /* A nonoptional dummy argument that corresponds by    */
                  /* position in the argument list to a dummy argument   */
                  /* not present in the other, present with a different  */
                  /* type, present with a different kind type parameter, */
                  /* or present with a different rank.                   */

                  /*                        AND                          */

                  /* A nonoptional dummy argument that corresponds by    */
                  /* argument keyword to a dummy argument not present    */
                  /* in the other, present with a different type,        */
                  /* present with a different kind type parameter,       */
                  /* or present with a different rank.                   */

                  if (!same_dargs) {

                     /* This differs by position.  Does it differ by kwd? */

                     kwd_darg_idx =srch_kwd_name(AT_OBJ_NAME_PTR(curr_darg_idx),
                                                 AT_NAME_LEN(curr_darg_idx),
                                                 attr_idx,
                                                &kwd_sn_idx);

                     if (kwd_darg_idx == NULL_IDX) {
                        ambiguous	= FALSE;
                        break;
                     }

                     if (!compare_dummy_arguments(curr_darg_idx, kwd_darg_idx)){
                        ambiguous	= FALSE;
                        break;
                     }

                     if (!AT_OPTIONAL(kwd_darg_idx) &&
                         !SN_MATCHED_DARG(kwd_sn_idx) &&
                         !SN_MATCHED_DARG(curr_darg_sn_idx - 1)) {

                        /* Attempt to match up all non optional dargs. */

                        SN_MATCHED_DARG(curr_darg_sn_idx - 1)	= TRUE;
                        SN_MATCHED_DARG(kwd_sn_idx)		= TRUE;
                     }
                  }

                  /*                       OR                          */

                  /* one of them must have more nonoptional dummy      */
                  /* arguments of a particular data type, kind type    */
                  /* parameter, and rank than the other has dummy      */
                  /* arguments (including optional dummy arguments)    */
                  /* of that data type, kind type parameter, and rank. */

                  /* Check for a non optional match on the curr darg   */
   
                  if (!SN_MATCHED_DARG(curr_darg_sn_idx - 1)) {
                     ktr_sn_idx		= save_darg_sn_idx;
                     optional_sn_idx	= NULL_IDX;
   
                     /* Loop through the dummy args looking for a match */
   
                     for (idx = 0; idx < save_num_dargs; idx++) {
   
                        if (SN_MATCHED_DARG(ktr_sn_idx)) { 
                           ktr_sn_idx++;
                           continue;
                        }

                        if (compare_dummy_arguments(curr_darg_idx,
                                                    SN_ATTR_IDX(ktr_sn_idx))) {

                           /* We want to match all non optionals first,  */
                           /* because we need to make sure they are all  */
                           /* checked for a match.  Keep track of the    */
                           /* optional match, in case we need to use it. */
       

                           if (AT_OPTIONAL(SN_ATTR_IDX(ktr_sn_idx))) {
                              optional_sn_idx			= ktr_sn_idx;
                           }
                           else {
                              SN_MATCHED_DARG(ktr_sn_idx)	  = TRUE;
                              SN_MATCHED_DARG(curr_darg_sn_idx-1) = TRUE;
                              break;
                           }
                        }
                        ktr_sn_idx++;
                     }

                     if (!SN_MATCHED_DARG(curr_darg_sn_idx-1) && 
                         optional_sn_idx != NULL_IDX) {

                        /* Matched to an optional - set it */

                        SN_MATCHED_DARG(optional_sn_idx)	= TRUE;
                        SN_MATCHED_DARG(curr_darg_sn_idx-1)	= TRUE;
                     }

                     /* This non optional does not have a match. */
                     /* This makes this interface unambiguous.   */

                     if (!SN_MATCHED_DARG(curr_darg_sn_idx-1)) { 
                        ambiguous	= FALSE;
                        break;
                     }
                  }
               }

               if (darg_idx != NULL_IDX && !AT_OPTIONAL(darg_idx)) {

                  /* At least one of them shall have both                */

                  /* A nonoptional dummy argument that corresponds by    */
                  /* position in the argument list to a dummy argument   */
                  /* not present in the other, present with a different  */
                  /* type, present with a different kind type parameter, */
                  /* or present with a different rank.                   */

                  /*                        AND                          */

                  /* A nonoptional dummy argument that corresponds by    */
                  /* argument keyword to a dummy argument not present    */
                  /* in the other, present with a different type,        */
                  /* present with a different kind type parameter,       */
                  /* or present with a different rank.                   */

                  if (!same_dargs) {

                     /* This differs by position.  Does it differ by kwd? */

                     kwd_darg_idx = srch_kwd_name(AT_OBJ_NAME_PTR(darg_idx),
                                                  AT_NAME_LEN(darg_idx),
                                                  curr_attr_idx,
                                                 &kwd_sn_idx);

                     if (kwd_darg_idx == NULL_IDX) {
                        ambiguous	= FALSE;
                        break;
                     }

                     if (!compare_dummy_arguments(darg_idx, kwd_darg_idx)){
                        ambiguous	= FALSE;
                        break;
                     }

                     if (!AT_OPTIONAL(kwd_darg_idx) &&
                         !SN_MATCHED_DARG(kwd_sn_idx) &&
                         !SN_MATCHED_DARG(darg_sn_idx - 1)) {

                        /* Attempt to match up all non optional dargs. */

                        SN_MATCHED_DARG(darg_sn_idx - 1)	= TRUE;
                        SN_MATCHED_DARG(kwd_sn_idx)		= TRUE;
                     }
                  }

                  /* Check for a non optional match on darg   */

                  if (!SN_MATCHED_DARG(darg_sn_idx - 1)) {
                     ktr_sn_idx		= save_curr_darg_sn_idx;
                     optional_sn_idx	= NULL_IDX;
   
                     /* Loop through the dummy args looking for a match */
   
                     for (idx = 0; idx < save_curr_num_dargs; idx++) {
   
                        if (SN_MATCHED_DARG(ktr_sn_idx)) { 
                           ktr_sn_idx++;
                           continue;
                        }

                        if (compare_dummy_arguments(darg_idx,
                                                    SN_ATTR_IDX(ktr_sn_idx))) {

                           /* We want to match all non optionals first,  */
                           /* because we need to make sure they are all  */
                           /* checked for a match.  Keep track of the    */
                           /* optional match, in case we need to use it. */
       

                           if (AT_OPTIONAL(SN_ATTR_IDX(ktr_sn_idx))) {
                              optional_sn_idx			= ktr_sn_idx;
                           }
                           else {
                              SN_MATCHED_DARG(ktr_sn_idx)	= TRUE;
                              SN_MATCHED_DARG(darg_sn_idx-1)	= TRUE;
                              break;
                           }
                        }
                        ktr_sn_idx++;
                     }

                     if (!SN_MATCHED_DARG(darg_sn_idx-1) && 
                         optional_sn_idx != NULL_IDX) {

                        /* Matched to an optional - set it */

                        SN_MATCHED_DARG(optional_sn_idx)	= TRUE;
                        SN_MATCHED_DARG(darg_sn_idx-1)		= TRUE;
                     }

                     /* This non optional does not have a match. */
                     /* This makes this interface unambiguous.   */

                     if (!SN_MATCHED_DARG(darg_sn_idx-1)) { 
                        ambiguous	= FALSE;
                        break;
                     }
                  }
               }
            }	/* for loop for dummy args */

            /* If generic clear the SN_MATCHED_DARG flag */

            if (ATI_INTERFACE_CLASS(interface_idx) < Defined_Interface) {
               ktr_sn_idx = save_darg_sn_idx;

               for (idx = 0; idx < save_num_dargs; idx++) {
                  SN_MATCHED_DARG(ktr_sn_idx)	= FALSE;
                  ktr_sn_idx++;
               }

               ktr_sn_idx = save_curr_darg_sn_idx;

               for (idx = 0; idx < save_curr_num_dargs; idx++) {
                  SN_MATCHED_DARG(ktr_sn_idx)	= FALSE;
                  ktr_sn_idx++;
               }
            }

            if (ambiguous) {	/* ambiguous interface, two specs the same */

               if (compare_names(AT_OBJ_NAME_LONG(curr_attr_idx),
                                 AT_NAME_LEN(curr_attr_idx),
                                 AT_OBJ_NAME_LONG(attr_idx),
                                 AT_NAME_LEN(attr_idx)) == 0) {

                  /* These have the same name.  If they are from the     */
                  /* same original module.  Then do not issue a message. */
                  /* Otherwise issue a message.                          */

                  /* KAY - It might be nice to unhook duplicates like this. */

                  if (AT_MODULE_IDX(curr_attr_idx) == NULL_IDX ||
                      AT_MODULE_IDX(attr_idx) == NULL_IDX ||
                      ATP_MODULE_STR_IDX(AT_MODULE_IDX(curr_attr_idx)) !=
                      ATP_MODULE_STR_IDX(AT_MODULE_IDX(attr_idx))) {
                     PRINTMSG(SN_LINE_NUM(curr_sn_idx), 991, Error,
                              SN_COLUMN_NUM(curr_sn_idx),
                              AT_OBJ_NAME_PTR(curr_attr_idx),
                              (ATI_INTERFACE_CLASS(interface_idx) ? "GENERIC" :
                                                                    "DEFINED"),
                              AT_OBJ_NAME_PTR(interface_idx));
                     AT_DCL_ERR(interface_idx) = TRUE;
                  }
               }
               else {
                  PRINTMSG(SN_LINE_NUM(curr_sn_idx), 487, Error,
                           SN_COLUMN_NUM(curr_sn_idx),
                           AT_OBJ_NAME_PTR(curr_attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           (ATI_INTERFACE_CLASS(interface_idx) ? "GENERIC" :
                                                                 "DEFINED"),
                           AT_OBJ_NAME_PTR(interface_idx));
                  AT_DCL_ERR(interface_idx) = TRUE;
               }
            }
         } /* if .. */

         sn_idx   = SN_SIBLING_LINK(sn_idx);
      }

      curr_sn_idx = SN_SIBLING_LINK(curr_sn_idx);
#ifdef KEY /* Bug 4197 */
      first = FALSE;
#endif /* KEY Bug 4197 */
   }

EXIT:

   TRACE (Func_Exit, "verify_interface", NULL);

   return;

}  /* verify_interface */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      If a procedure has multiple specific interfaces, verify that they are *|
|*      the same.                                                             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      attr_idx - The program unit to compare.                               *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
static void compare_duplicate_interface_bodies(int        attr_idx)

{
   int		dup_attr_idx;
   int		idx;
   int		idx1;
   int		idx2;
   int          rank1;
   int          rank2;
   boolean      same	= TRUE;


   TRACE (Func_Entry, "compare_duplicate_interface_bodies", NULL);

   dup_attr_idx	= ATP_DUPLICATE_INTERFACE_IDX(attr_idx);
   ATP_DUPLICATE_INTERFACE_IDX(attr_idx) = NULL_IDX;

   if (ATP_PGM_UNIT(attr_idx) != ATP_PGM_UNIT(dup_attr_idx) ||
       ATP_NUM_DARGS(attr_idx) != ATP_NUM_DARGS(dup_attr_idx) ||
       ATP_RSLT_NAME(attr_idx) != ATP_RSLT_NAME(dup_attr_idx)) {

      /* One is a function and one is a subroutine, or they have */
      /* a different number of dummy arguments and/or one has    */
      /* a result name and the other does not.                   */

      same = FALSE;
   }
   else {  /* Compare results and individual dummy arguments. */

      if (ATP_PGM_UNIT(attr_idx) == Function) {
         idx1 = ATP_RSLT_IDX(attr_idx);
         idx2 = ATP_RSLT_IDX(dup_attr_idx);

         if (ATP_RSLT_NAME(attr_idx) &&
             (compare_names(AT_OBJ_NAME_LONG(idx1),
                            AT_NAME_LEN(idx1),
                            AT_OBJ_NAME_LONG(idx2),
                            AT_NAME_LEN(idx2)) != 0)) {
            same = FALSE;
         }
         else {  /* Compare kind, type and rank of result */

            if (TYP_TYPE(ATD_TYPE_IDX(idx1)) != TYP_TYPE(ATD_TYPE_IDX(idx2))) {
               same = FALSE;
            }
            else if (TYP_TYPE(ATD_TYPE_IDX(idx1)) == Structure &&
                     !compare_derived_types(ATD_TYPE_IDX(idx1), 
                                            ATD_TYPE_IDX(idx2))) {
               same = FALSE;
            }
            else if (TYP_TYPE(ATD_TYPE_IDX(idx1)) != Character &&
                     TYP_TYPE(ATD_TYPE_IDX(idx1)) != Structure &&
                     TYP_LINEAR(ATD_TYPE_IDX(idx1)) != 
                                             TYP_LINEAR(ATD_TYPE_IDX(idx2))) {
               same = FALSE;
            }

            if (same) {
               rank1 = (ATD_ARRAY_IDX(idx1) == NULL_IDX) ? 0 :
                                               BD_RANK(ATD_ARRAY_IDX(idx1));
               rank2 = (ATD_ARRAY_IDX(idx2) == NULL_IDX) ? 0 :
                                               BD_RANK(ATD_ARRAY_IDX(idx2));

               if (rank1 != rank2) {
                  same = FALSE;
               }
            }
         }
      }

      if (same) { /* Check the dummy arguments. */
         idx1 = ATP_FIRST_IDX(attr_idx);
         idx2 = ATP_FIRST_IDX(dup_attr_idx);

         for (idx = 0; idx < ATP_NUM_DARGS(attr_idx); idx++) {

             if (compare_names(AT_OBJ_NAME_LONG(SN_ATTR_IDX(idx1)),
                               AT_NAME_LEN(SN_ATTR_IDX(idx1)),
                               AT_OBJ_NAME_LONG(SN_ATTR_IDX(idx2)),
                               AT_NAME_LEN(SN_ATTR_IDX(idx2))) != 0) {
                same = FALSE;  /* Keyword names differ */
                break;
            }

            if (!compare_dummy_arguments(SN_ATTR_IDX(idx1),SN_ATTR_IDX(idx2))) {
               same = FALSE;
               break;
            }
            idx1++; idx2++;
         }
      }
   }

   if (same) {  /* Issue ANSI */
      PRINTMSG(AT_DEF_LINE(dup_attr_idx), 1515, Ansi,
               AT_DEF_COLUMN(dup_attr_idx),
               AT_OBJ_NAME_PTR(dup_attr_idx));
   }
   else {  /* They are different. */
      PRINTMSG(AT_DEF_LINE(dup_attr_idx), 1516, Error,
               AT_DEF_COLUMN(dup_attr_idx),
               AT_OBJ_NAME_PTR(dup_attr_idx));
   }

   TRACE (Func_Exit, "compare_duplicate_interface_bodies", NULL);

   return;

}  /* compare_duplicate_interface_bodies */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Check for reshape arrays and set ATD_RESHAPE_ARRAY_OPT if okay.      *|
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
static void reshape_array_semantics(void)
{
   int			al_idx;
   int			attr_idx;
   int			fp_idx;
   int			name_idx;
   token_type		name_token;


   TRACE (Func_Entry, "reshape_array_semantics", NULL);

   fp_idx = opt_flags.reshape_idx;

   while (fp_idx != NULL_IDX) {
      CREATE_ID(TOKEN_ID(name_token),(FP_NAME_PTR(fp_idx)),FP_NAME_LEN(fp_idx));

      TOKEN_COLUMN(name_token)	= 1;
      TOKEN_LEN(name_token)	= FP_NAME_LEN(fp_idx);
      TOKEN_LINE(name_token)	= stmt_start_line;

      attr_idx = srch_sym_tbl(TOKEN_STR(name_token),
                              TOKEN_LEN(name_token),
                              &name_idx);

      if (attr_idx != NULL_IDX) {  /* Name exists in symbol table already */

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
             ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {

            if (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Explicit_Shape &&
                (ATD_CLASS(attr_idx) != CRI__Pointee &&
                 ATD_CLASS(attr_idx) != Constant) &&
                BD_RANK(ATD_ARRAY_IDX(attr_idx)) > 1) {
               ATD_RESHAPE_ARRAY_OPT(attr_idx)    = TRUE;

               NTR_ATTR_LIST_TBL(al_idx);
               AL_ATTR_IDX(al_idx)      = attr_idx;
               AL_NEXT_IDX(al_idx)      = reshape_array_list;
               reshape_array_list       = al_idx;
               if (ATD_DATA_INIT(attr_idx)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1644, Error,
                           AT_DEF_COLUMN(attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
            }
            else {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1539, Error,
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
         }
         else { /* This is already something else in this scope.  */
            PRINTMSG(AT_DEF_LINE(attr_idx), 1538, Warning,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
         }
      }

      fp_idx = FP_NEXT_FILE_IDX(fp_idx);
   }

   TRACE (Func_Exit, "reshape_array_semantics", NULL);

   return;

}  /*  reshape_array_semantics  */

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

static void gen_allocatable_ptr_ptee(int	attr_idx)

{
   int		col;
   int		line;
   int		ptr_idx;
   int		ptee_idx;
   id_str_type	storage_name;


   TRACE (Func_Entry, "gen_allocatable_ptr_ptee", NULL);

   line = AT_DEF_LINE(attr_idx);
   col = AT_DEF_COLUMN(attr_idx);

   ptr_idx  = gen_compiler_tmp(line, col, Shared, TRUE);

   if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
      ATD_TYPE_IDX(ptr_idx) = CRI_Ch_Ptr_8;
   }
   else {
      ATD_TYPE_IDX(ptr_idx) = CRI_Ptr_8;
   }
   AT_SEMANTICS_DONE(ptr_idx) = TRUE;

# ifdef _DEBUG
   if (ATD_STOR_BLK_IDX(attr_idx) == NULL_IDX) {
      PRINTMSG(line, 626, Internal, col,
               "valid ATD_STOR_BLK_IDX",
               "gen_allocatable_ptr_ptee");
   }
# endif
   ATD_STOR_BLK_IDX(ptr_idx) = ATD_STOR_BLK_IDX(attr_idx);

   ptee_idx = gen_compiler_tmp(line, col, Shared, TRUE);
   ATD_CLASS(ptee_idx) = CRI__Pointee;
   AT_SEMANTICS_DONE(ptee_idx) = TRUE;

   if (pointee_based_blk == NULL_IDX) {

      /* Create a based entry for PDGCS to use for cri_pointees */

      CREATE_ID(storage_name, sb_name[Pointee_Blk], sb_len[Pointee_Blk]);
      pointee_based_blk = ntr_stor_blk_tbl(storage_name.string,
                                           sb_len[Pointee_Blk],
                                           AT_DEF_LINE(attr_idx),
                                           AT_DEF_COLUMN(attr_idx),
                                           Based);
   }

   ATD_STOR_BLK_IDX(ptee_idx)     = pointee_based_blk;

   ATD_TYPE_IDX(ptee_idx) = ATD_TYPE_IDX(attr_idx);
   ATD_PTR_IDX(ptee_idx) = ptr_idx;

   ATD_ARRAY_IDX(ptee_idx) = set_up_bd_tmps(BD_RANK(ATD_ARRAY_IDX(attr_idx)),
                                            line, 
                                            col,
                                            ATD_STOR_BLK_IDX(attr_idx),
                                            FALSE);
   ATD_PE_ARRAY_IDX(ptee_idx) = 
                   set_up_bd_tmps(BD_RANK(ATD_PE_ARRAY_IDX(attr_idx)),
                                            line, 
                                            col,
                                            ATD_STOR_BLK_IDX(attr_idx),
                                            TRUE);

   ATD_FLD(attr_idx) = AT_Tbl_Idx;
   ATD_VARIABLE_TMP_IDX(attr_idx) = ptee_idx;

   TRACE (Func_Exit, "gen_allocatable_ptr_ptee", NULL);

   return;

}  /* gen_allocatable_ptr_ptee */

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

static int set_up_bd_tmps(int		rank,
                          int		line,
                          int   	col,
                          int		stor_blk_idx,
			  boolean	assumed_size)

{
   int		bd_idx;
   int		i;
   int		tmp_idx;


   TRACE (Func_Entry, "set_up_bd_tmps", NULL);

   bd_idx                 = reserve_array_ntry(rank);
   BD_RANK(bd_idx)        = rank;
   BD_LINE_NUM(bd_idx)    = line;
   BD_COLUMN_NUM(bd_idx)  = col;
   BD_ARRAY_SIZE(bd_idx)  = Var_Len_Array;
   BD_ARRAY_CLASS(bd_idx) = (assumed_size ? Assumed_Size : Explicit_Shape);
   BD_RESOLVED(bd_idx)    = TRUE;

   for (i =1; i <= rank; i++) {

      tmp_idx = gen_compiler_tmp(line, col, Shared, TRUE);
      ATD_TYPE_IDX(tmp_idx) = SA_INTEGER_DEFAULT_TYPE;
      ATD_STOR_BLK_IDX(tmp_idx) = stor_blk_idx;
      AT_SEMANTICS_DONE(tmp_idx) = TRUE;

      BD_LB_FLD(bd_idx,i) = AT_Tbl_Idx;
      BD_LB_IDX(bd_idx,i) = tmp_idx;


      if (assumed_size && i == rank) {
         BD_XT_FLD(bd_idx,i) = CN_Tbl_Idx;
         BD_XT_IDX(bd_idx,i) = CN_INTEGER_ONE_IDX;

         BD_UB_FLD(bd_idx,i) = BD_LB_FLD(bd_idx,i);
         BD_UB_IDX(bd_idx,i) = BD_LB_IDX(bd_idx,i);
      }
      else {
         tmp_idx = gen_compiler_tmp(line, col, Shared, TRUE);
         ATD_TYPE_IDX(tmp_idx) = SA_INTEGER_DEFAULT_TYPE;
         ATD_STOR_BLK_IDX(tmp_idx) = stor_blk_idx;
         AT_SEMANTICS_DONE(tmp_idx) = TRUE;

         BD_XT_FLD(bd_idx,i) = AT_Tbl_Idx;
         BD_XT_IDX(bd_idx,i) = tmp_idx;


         tmp_idx = gen_compiler_tmp(line, col, Shared, TRUE);
         ATD_TYPE_IDX(tmp_idx) = SA_INTEGER_DEFAULT_TYPE;
         ATD_STOR_BLK_IDX(tmp_idx) = stor_blk_idx;
         AT_SEMANTICS_DONE(tmp_idx) = TRUE;

         BD_UB_FLD(bd_idx,i) = AT_Tbl_Idx;
         BD_UB_IDX(bd_idx,i) = tmp_idx;
      }

      tmp_idx = gen_compiler_tmp(line, col, Shared, TRUE);
      ATD_TYPE_IDX(tmp_idx) = SA_INTEGER_DEFAULT_TYPE;
      ATD_STOR_BLK_IDX(tmp_idx) = stor_blk_idx;
      AT_SEMANTICS_DONE(tmp_idx) = TRUE;

      BD_SM_FLD(bd_idx,i) = AT_Tbl_Idx;
      BD_SM_IDX(bd_idx,i) = tmp_idx;
   }

   tmp_idx = gen_compiler_tmp(line, col, Shared, TRUE);
   ATD_TYPE_IDX(tmp_idx) = SA_INTEGER_DEFAULT_TYPE;
   ATD_STOR_BLK_IDX(tmp_idx) = stor_blk_idx;
   AT_SEMANTICS_DONE(tmp_idx) = TRUE;

   BD_LEN_FLD(bd_idx) = AT_Tbl_Idx;
   BD_LEN_IDX(bd_idx) = tmp_idx;


   BD_FLOW_DEPENDENT(bd_idx) = TRUE;

   bd_idx =  ntr_array_in_bd_tbl(bd_idx);

   TRACE (Func_Exit, "set_up_bd_tmps", NULL);

   return(bd_idx);

}  /* set_up_bd_tmps */

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

int	gen_tmp_equal_max_zero(opnd_type	*opnd,
			       int		 type_idx,
			       int		 entry_idx,
			       boolean		 is_symbolic_constant,
			       boolean		 is_interface)

{
   int		column;
   int		defining_attr;
   int		line;
   int		list_idx;
   int		max_idx;
#ifdef KEY /* Bug 10177 */
   int		sh_idx = 0;
#else /* KEY Bug 10177 */
   int		sh_idx;
#endif /* KEY Bug 10177 */
   int		tmp_idx;
   int		zero_idx;


   TRACE (Func_Entry, "gen_tmp_equal_max_zero", NULL);

   /* Generate  tmp = max(0, extent) */

   line				= OPND_LINE_NUM((*opnd));
   column			= OPND_COL_NUM((*opnd));

   NTR_IR_TBL(max_idx);
   IR_OPR(max_idx)		= Max_Opr;
   IR_TYPE_IDX(max_idx)		= type_idx;
   IR_LINE_NUM(max_idx)		= line;
   IR_COL_NUM(max_idx)		= column;
   IR_LIST_CNT_L(max_idx)	= 2;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_L(max_idx)		= IL_Tbl_Idx;
   IR_IDX_L(max_idx)		= list_idx;

   COPY_OPND(IL_OPND(list_idx), (*opnd));

   NTR_IR_LIST_TBL(zero_idx);
   IL_NEXT_LIST_IDX(list_idx)	= zero_idx;
   IL_PREV_LIST_IDX(zero_idx)	= list_idx;
   IL_FLD(zero_idx)       	= CN_Tbl_Idx;
   IL_IDX(zero_idx)       	= CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(zero_idx)  	= line;
   IL_COL_NUM(zero_idx)   	= column;

   if (OPND_FLD((*opnd)) == AT_Tbl_Idx &&
       AT_OBJ_CLASS(OPND_IDX((*opnd))) == Data_Obj &&
       ATD_CLASS(OPND_IDX((*opnd))) == Compiler_Tmp) {
      defining_attr	= ATD_DEFINING_ATTR_IDX(OPND_IDX((*opnd)));
   }
   else {
      defining_attr	= NULL_IDX;
   }

   if (is_symbolic_constant) {
      IR_OPR(max_idx)	= Symbolic_Max_Opr;
      OPND_FLD((*opnd))	= AT_Tbl_Idx;
      OPND_IDX((*opnd))	= gen_compiler_tmp(line,
                                           column,
                                           Priv, TRUE);

      ATD_TYPE_IDX(OPND_IDX((*opnd)))		= type_idx;
      ATD_FLD(OPND_IDX((*opnd)))		= IR_Tbl_Idx;
      ATD_TMP_IDX(OPND_IDX((*opnd)))		= max_idx;
      ATD_SYMBOLIC_CONSTANT(OPND_IDX((*opnd)))	= TRUE;
      ATD_DEFINING_ATTR_IDX(OPND_IDX((*opnd)))	= defining_attr;
   }
   else {
      OPND_FLD((*opnd))		= IR_Tbl_Idx;
      OPND_IDX((*opnd))		= max_idx;
         

      if (!is_interface) {
         sh_idx			= ntr_sh_tbl();
         SH_STMT_TYPE(sh_idx)	= Automatic_Base_Size_Stmt;
         SH_GLB_LINE(sh_idx)	= line;
         SH_COL_NUM(sh_idx)	= column;
         SH_COMPILER_GEN(sh_idx)= TRUE;
         SH_P2_SKIP_ME(sh_idx)	= TRUE;
      }

      tmp_idx = ntr_bnds_sh_tmp_list(opnd,
                                     entry_idx,
                                     (is_interface) ? NULL_IDX : sh_idx,
                                     FALSE,
                                     type_idx);
      OPND_FLD((*opnd))			= AT_Tbl_Idx;
      OPND_IDX((*opnd))			= tmp_idx;
      ATD_DEFINING_ATTR_IDX(tmp_idx)	= defining_attr;
   }

   TRACE (Func_Exit, "gen_tmp_equal_max_zero", NULL);

   return(max_idx);

}  /* gen_tmp_equal_max_zero */

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
static	boolean	compare_darg_or_rslt_types(int	idx1,
					   int	idx2)
{
   boolean	intrin1;
   boolean	intrin2;
   int          linear_type1;
   int          linear_type2;
   int          rank1;
   int          rank2;
   boolean	same		= TRUE;


   TRACE (Func_Entry, "compare_darg_or_rslt_types", NULL);

   intrin1 = (ATD_CLASS(idx1) == Dummy_Argument) && ATD_INTRIN_DARG(idx1);
   intrin2 = (ATD_CLASS(idx2) == Dummy_Argument) && ATD_INTRIN_DARG(idx2);

   if (intrin1 || intrin2) {
      rank1 = (ATD_ARRAY_IDX(idx1) == NULL_IDX) ? 
               0 : BD_RANK(ATD_ARRAY_IDX(idx1));
      rank2 = (ATD_ARRAY_IDX(idx2) == NULL_IDX) ? 
               0 : BD_RANK(ATD_ARRAY_IDX(idx2));

      if (!intrin1) {

         if (TYP_TYPE(ATD_TYPE_IDX(idx1)) == Character ||
             TYP_TYPE(ATD_TYPE_IDX(idx1)) == Structure) {
            same = FALSE;
            goto DONE;
         }

         linear_type1 = TYP_LINEAR(ATD_TYPE_IDX(idx1));
         linear_type1 = 1 << linear_type1;
      }
      else {
         linear_type1 = ATD_INTRIN_DARG_TYPE(idx1);
      }

      if (!intrin2) {

         if (TYP_TYPE(ATD_TYPE_IDX(idx2)) == Character ||
             TYP_TYPE(ATD_TYPE_IDX(idx2)) == Structure) {
            same = FALSE;
            goto DONE;
         }

         linear_type2 = TYP_LINEAR(ATD_TYPE_IDX(idx2));
         linear_type2 = 1 << linear_type2;
      }
      else {
         linear_type2 = ATD_INTRIN_DARG_TYPE(idx2);
      }

      if ((linear_type1 & linear_type2) == 0) {
         same = FALSE;
      }

      if (rank1 != rank2) {
         same = FALSE;
      }
   }
   else {

      if (TYP_TYPE(ATD_TYPE_IDX(idx1)) != TYP_TYPE(ATD_TYPE_IDX(idx2))){
         same = FALSE;
      }
      else if (TYP_TYPE(ATD_TYPE_IDX(idx1)) == Structure &&
               !compare_derived_types(ATD_TYPE_IDX(idx1), ATD_TYPE_IDX(idx2))) {
         same = FALSE;
      }
      else if (TYP_TYPE(ATD_TYPE_IDX(idx1)) != Character &&
               TYP_TYPE(ATD_TYPE_IDX(idx1)) != Structure &&
               TYP_LINEAR(ATD_TYPE_IDX(idx1)) != 
               TYP_LINEAR(ATD_TYPE_IDX(idx2))) {
         same = FALSE;
      }

      if (same) {
         rank1 = (ATD_ARRAY_IDX(idx1) == NULL_IDX) ? 
                  0 : BD_RANK(ATD_ARRAY_IDX(idx1));
         rank2 = (ATD_ARRAY_IDX(idx2) == NULL_IDX) ? 
                  0 : BD_RANK(ATD_ARRAY_IDX(idx2));

         if (rank1 != rank2) {
            same = FALSE;
         }
      }
   }

DONE: 

   TRACE (Func_Exit, "compare_darg_or_rslt_types", NULL);

   return(same);

}  /* compare_darg_or_rslt_types */

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/nameres.c	5.3	06/01/99 13:21:01\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "debug.m"
# include "p_globals.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "nameres.h"
# include "p_globals.h"


/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Assumes that the attr in attr_idx has correct flag and field          *|
|*	combinations set.  This routine checks to see if the new_obj attribute*|
|*      or name or whatever can be added to the current attr.  If it finds an *|
|*      error it issues it and returns FALSE.  AT_DCL_ERR is set TRUE, if this*|
|*      is a declaration check as opposed to a use check.  new_obj is an enum *|
|*      that covers most of what something can be declared as or used as.  It *|
|*      resides in globals.h.  This routine uses a series of 3 bit vector     *|
|*      tables.  Every obj_type has an entry in the attribute semantic table, *|
|*      the name semantic table, and the other semantic table.  An obj_type   *|
|*      entry in any of the tables is a long, that is a series of bits.  Each *|
|*      bit represents an enum in the table.  (ie-> The attribute semantic    *|
|*      table has an attr enum declared in sytb.h.) There is one bit on an    *|
|*      object entry for each item in the attr enum.)  So all this routine    *|
|*      does is go through the attr entry, checking to see if what is already *|
|*      declared on the attr entry is compatible with the new declaration or  *|
|*      use of that attr.  To check something, is just a table look up.  (And *|
|*      then a mask and a shift.)  If a 1 pops up, it's an illegal combination*|
|*      and an error is issued.  See more details in nameres.h. and the       *|
|*      actual tables.                                                        *|
|*									      *|
|* Input parameters:							      *|
|*      new_obj   -> An enum describing what is to be added to the attr entry.*|
|*      line      -> A line number where the addition or use is taking place. *|
|*                   Used to issue error messages.                            *|
|*      column    -> A line number where the addition or use is taking place. *|
|*                   Used to issue error messages.                            *|
|*      attr_idx  -> The attribute to add the new thing to (or to use it.)    *|
|*      issue_msg -> TRUE if message should be issued.  FALSE if fnd_semantic *|
|*                   _err should just check if this combination is allowed.   *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if it found an error.					      *|
|*									      *|
\******************************************************************************/

boolean	fnd_semantic_err(obj_type	new_obj,
			 int		line,
			 int		column,
			 int		attr_idx,
			 boolean	issue_msg)

{
   int  	 array_idx;
   long		 attr_obj_ntry;
   long		 dir_obj_ntry;
   int  	 func_idx;
   int 		 msg_num 		= 0;
   char		*msg_str		= NULL;
   long		 name_obj_ntry;
   long		 other_obj_ntry;
   int  	 rslt_idx;
   boolean  	 set_dcl_err		= issue_msg;


   TRACE (Func_Entry, "fnd_semantic_err", obj_type_str[new_obj]);

   attr_obj_ntry	= obj_to_attr[new_obj];
   dir_obj_ntry		= obj_to_dir[new_obj];
   name_obj_ntry	= obj_to_name[new_obj];
   other_obj_ntry	= obj_to_other[new_obj];

   if (AT_OBJ_CLASS(attr_idx) == Data_Obj && ATD_SYMBOLIC_CONSTANT(attr_idx)) {

      if (other_obj_ntry & (1 << Other_Npes)) {
         msg_num = other_msg_num[new_obj][Other_Npes];

         if (new_obj == Obj_Use_Init_Expr && issue_msg) {
            PRINTMSG(line, 1212, Error, column,
                     AT_OBJ_NAME_PTR(attr_idx));

            /* This will prevent the msg from being issued twice, but   */
            /* will still cause fnd_semantic_err to return FALSE.       */

            issue_msg = FALSE;
            goto ISSUE_ERR;
         }
         goto ISSUE_ERR;
      }  
      /* If there is an error a message will be printed.  If no error   */
      /* msg_num is set to zero, so message issuing will be bypassed    */
      /* and control will exit this routine.                            */
   }

   if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {

      /* This may end up not being host associated. */

      if (other_obj_ntry & (1 << Other_Use_Char_Rslt)) {
         msg_num = other_msg_num[new_obj][Other_Use_Char_Rslt];
         goto ISSUE_ERR;
      }
   }
   else if (AT_ATTR_LINK(attr_idx) != NULL_IDX) {   /* Host associated item */

      if ((other_obj_ntry & (1 << Other_Host_Assoc)) &&
          !(AT_OBJ_CLASS(AT_ATTR_LINK(attr_idx)) == Pgm_Unit &&
            ATP_PROC(AT_ATTR_LINK(attr_idx)) == Intrin_Proc)) {
         msg_num = other_msg_num[new_obj][Other_Host_Assoc];
         goto ISSUE_ERR;
      }
   }

   if (AT_USE_ASSOCIATED(attr_idx)) {

      if (other_obj_ntry & (1 << Other_Use_Assoc)) {

         if (AT_OBJ_CLASS(attr_idx) != Pgm_Unit || 
             ATP_PGM_UNIT(attr_idx) != Module) {
            msg_num = other_msg_num[new_obj][Other_Use_Assoc];
            goto ISSUE_ERR;
         }
      }

      if (AT_NOT_VISIBLE(attr_idx)) { 
         msg_num = 486;

         if (issue_msg) {
            PRINTMSG(line, 486, Error, column,
                     AT_OBJ_NAME_PTR(attr_idx),
                     AT_OBJ_NAME_PTR(AT_MODULE_IDX((attr_idx))));

            /* This will prevent the msg from being issued twice, but   */
            /* will still cause fnd_semantic_err to return FALSE.       */

            issue_msg = FALSE;
            goto ISSUE_ERR;
         }
      }
   }

   switch (AT_OBJ_CLASS(attr_idx)) {
   case Data_Obj:

      switch (ATD_CLASS(attr_idx)) {
      case Atd_Unknown:
         if ((AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref || 
              AT_DEFINED(attr_idx)) &&
             (other_obj_ntry & (1 << Other_Use_Variable))) {
            msg_num = other_msg_num[new_obj][Other_Use_Variable];
            goto ISSUE_ERR;
         }
         else if (ATD_AUXILIARY(attr_idx) && 
                  (dir_obj_ntry & (1 << Dir_Auxiliary))) {
            msg_num = dir_msg_num[new_obj][Dir_Auxiliary];
            msg_str = dir_str[Dir_Auxiliary];
            goto ISSUE_ERR;
         }
#ifdef KEY /* Bug 14150 */
         if (AT_BIND_ATTR(attr_idx) && 
                  (attr_obj_ntry & (1 << Attr_Bind))) {
            msg_num = attr_msg_num[new_obj][Attr_Bind];
            msg_str = attr_str[Attr_Bind];
            goto ISSUE_ERR;
         }
#endif /* KEY Bug 14150 */
         break;

      case Variable:

         /* Only a variable can be data initialized, equivalenced, */
         /*      in a common block,  in auxiliary or saved.        */

#ifdef KEY /* Bug 14150 */
         if (AT_BIND_ATTR(attr_idx) && 
                  (attr_obj_ntry & (1 << Attr_Bind))) {
            msg_num = attr_msg_num[new_obj][Attr_Bind];
            msg_str = attr_str[Attr_Bind];
            goto ISSUE_ERR;
         }
	 /* Special case: we need to check bind attr on common block itself
	  * versus equivalence att on common block object; we need to force
	  * the message to cite "equivalence vs bind" even if the
	  * characteristic now being added is "common" */
	 if ((ATD_EQUIV(attr_idx) || new_obj == Obj_Equiv) &&
	   (ATD_IN_COMMON(attr_idx) || new_obj == Obj_Common_Obj) &&
	   (SB_BIND_ATTR(ATD_STOR_BLK_IDX(attr_idx)))) {
	   msg_num = 550;
	   new_obj = Obj_Equiv;
	   msg_str = attr_str[Attr_Bind];
            goto ISSUE_ERR;
         }
# endif
         if (ATD_DATA_INIT(attr_idx) && 
                  (attr_obj_ntry & (1 << Attr_Data_Init))) {
            msg_num = attr_msg_num[new_obj][Attr_Data_Init];
            msg_str = attr_str[Attr_Data_Init];
            goto ISSUE_ERR;
         }
         else if (ATD_EQUIV(attr_idx) && 
                  (attr_obj_ntry & (1 << Attr_Equivalence))) {
            msg_num = attr_msg_num[new_obj][Attr_Equivalence];
            msg_str = attr_str[Attr_Equivalence];
            goto ISSUE_ERR;
         }
         else if (ATD_SAVED(attr_idx) && 
                  (attr_obj_ntry & (1 << Attr_Save))) {
            msg_num = attr_msg_num[new_obj][Attr_Save];
            msg_str = attr_str[Attr_Save];
            goto ISSUE_ERR;
         }
         else if (ATD_IN_COMMON(attr_idx) &&
                  (name_obj_ntry & (1 << Name_Common_Obj))) {
            msg_num = name_msg_num[new_obj][Name_Common_Obj];
            msg_str = name_str[Name_Common_Obj];

            if (issue_msg) {

               if (new_obj == Obj_Common_Obj) {

                  /* duplicate entry in common block*/

                  PRINTMSG(line, name_msg_num[Obj_Common_Obj][Name_Common_Obj],
                           Error, column,
                           AT_OBJ_NAME_PTR(attr_idx),
                           SB_NAME_PTR(ATD_STOR_BLK_IDX(attr_idx)));

                  /* This will prevent the msg from being issued twice, but   */
                  /* will still cause fnd_semantic_err to return FALSE.       */

                  issue_msg = FALSE;
               }
            }
            goto ISSUE_ERR;
         }
         else if (ATD_AUXILIARY(attr_idx) && 
                  (dir_obj_ntry & (1 << Dir_Auxiliary))) {
            msg_num = dir_msg_num[new_obj][Dir_Auxiliary];
            msg_str = dir_str[Dir_Auxiliary];
            goto ISSUE_ERR;
         }
         else if (ATD_SYMMETRIC(attr_idx) && 
                  (dir_obj_ntry & (1 << Dir_Symmetric))) {
            msg_num = dir_msg_num[new_obj][Dir_Symmetric];
            msg_str = dir_str[Dir_Symmetric];
            goto ISSUE_ERR;
         }
         else if ((AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref ||
                  AT_DEFINED(attr_idx)) &&
                  (other_obj_ntry & (1 << Other_Use_Variable))) {
            msg_num = other_msg_num[new_obj][Other_Use_Variable];
            goto ISSUE_ERR;
         }
         break;  /* End Variable case */

      case Dummy_Argument:

         /* Make sure it has been declared as a DARG before issuing msg */
         /* It may just have OPTIONAL or INTENT declared.               */

         if (AT_IS_DARG(attr_idx) && 
             (name_obj_ntry & (1 << Name_Dummy_Arg))) {
            msg_num = name_msg_num[new_obj][Name_Dummy_Arg];
            msg_str = name_str[Name_Dummy_Arg];
            goto ISSUE_ERR;
         }
         else if (AT_OPTIONAL(attr_idx) &&
                  (attr_obj_ntry & (1 << Attr_Optional) ) ) {
            msg_num = attr_msg_num[new_obj][Attr_Optional];
            msg_str = attr_str[Attr_Optional];
            goto ISSUE_ERR;
         }
         else if (ATD_INTENT(attr_idx) != Intent_Unseen &&
                  (attr_obj_ntry & (1 << Attr_Intent)) ) {
            msg_num = attr_msg_num[new_obj][Attr_Intent];
            msg_str = attr_str[Attr_Intent];
            goto ISSUE_ERR;
         }
         else if (ATD_AUXILIARY(attr_idx) && 
                  (dir_obj_ntry & (1 << Dir_Auxiliary))) {
            msg_num = dir_msg_num[new_obj][Dir_Auxiliary];
            msg_str = dir_str[Dir_Auxiliary];
            goto ISSUE_ERR;
         }
         else if (ATD_IGNORE_TKR(attr_idx) && 
                  (dir_obj_ntry & (1 << Dir_Ignore_TKR)) ) {
            msg_num = dir_msg_num[new_obj][Dir_Ignore_TKR];
            msg_str = dir_str[Dir_Ignore_TKR];
            goto ISSUE_ERR;
         }
         else if ((AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref || 
              AT_DEFINED(attr_idx)) &&
             (other_obj_ntry & (1 << Other_Use_Dummy_Arg))) {
            msg_num = other_msg_num[new_obj][Other_Use_Dummy_Arg];
            goto ISSUE_ERR;
         }
#ifdef KEY /* Bug 14150 */
	 /* No evident way to issue Ansi instead of Error via tables */
	 else if (on_off_flags.issue_ansi_messages &&
	   ((ATD_INTENT(attr_idx) != Intent_Unseen && new_obj == Obj_Pointer) ||
	   (ATD_POINTER(attr_idx) && new_obj == Obj_Intent))) {
	   
	   PRINTMSG(line, 1697, Ansi, column, AT_OBJ_NAME_PTR(attr_idx),
	     obj_str[Obj_Pointer], obj_str[Obj_Intent], AT_DEF_LINE(attr_idx));
	 }
	 else if (ATD_VALUE_ATTR(attr_idx) &&
	    (attr_obj_ntry & (1 << Attr_Value))) {
	    msg_num = attr_msg_num[new_obj][Attr_Value];
	    msg_str = attr_str[Attr_Value];
	    goto ISSUE_ERR;
	 }
#endif /* KEY Bug 14150 */
         break;	/* End Dummy_Arg case */

      case Function_Result:

         if (name_obj_ntry & (1 << Name_Func_Result)) {
            msg_num = name_msg_num[new_obj][Name_Func_Result];
            msg_str = name_str[Name_Func_Result];
            goto ISSUE_ERR;
         }

         func_idx = ATD_FUNC_IDX(attr_idx);

         if (AT_ATTR_LINK(func_idx) != NULL_IDX) {   /* Host associated item */

               if ((other_obj_ntry & (1 << Other_Host_Assoc)) &&
                   !(AT_OBJ_CLASS(AT_ATTR_LINK(func_idx)) == Pgm_Unit &&
                     ATP_PROC(AT_ATTR_LINK(func_idx)) == Intrin_Proc)) {
               msg_num = other_msg_num[new_obj][Other_Host_Assoc];
               goto ISSUE_ERR;
            }
         }

         if (!ATP_EXPL_ITRFC(func_idx)) {

            if (ATP_VFUNCTION(func_idx) && 
                (dir_obj_ntry & (1<<Dir_Vfunction))) {
               msg_num = dir_msg_num[new_obj][Dir_Vfunction];
               msg_str = dir_str[Dir_Vfunction];
               goto ISSUE_ERR;
            }

            if (ATP_NOSIDE_EFFECTS(func_idx) &&
                (dir_obj_ntry & (1 << Dir_No_Side_Effects)) ) {
               msg_num = dir_msg_num[new_obj][Dir_No_Side_Effects];
               msg_str = dir_str[Dir_No_Side_Effects];
               goto ISSUE_ERR;
            }

            if (ATP_DCL_EXTERNAL(func_idx) &&
                attr_obj_ntry & (1 << Attr_External)) {
                  msg_num = attr_msg_num[new_obj][Attr_External];
                  msg_str = attr_str[Attr_External];
                  goto ISSUE_ERR;
            }

            if (AT_OPTIONAL(func_idx) && attr_obj_ntry & (1 << Attr_Optional)) {
               msg_num = attr_msg_num[new_obj][Attr_Optional];
               msg_str = attr_str[Attr_Optional];
               goto ISSUE_ERR;
            }
         }

         break;	/* End Function_Result case */

      case CRI__Pointee:

         if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character &&
             AT_TYPED(attr_idx) &&
             (name_obj_ntry & (1 << Name_Cri_Ch_Pointee))) {
            msg_num = name_msg_num[new_obj][Name_Cri_Ch_Pointee];
            msg_str = name_str[Name_Cri_Ch_Pointee];
            goto ISSUE_ERR;
         }
         else if (name_obj_ntry & (1 << Name_Cri_Pointee)) {
            msg_num = name_msg_num[new_obj][Name_Cri_Pointee];
            msg_str = name_str[Name_Cri_Pointee];
            goto ISSUE_ERR;
         }
         break;	/* End CRI__Pointee case */

      case Constant:
         if (attr_obj_ntry & (1 << Attr_Parameter)) {
            msg_num = attr_msg_num[new_obj][Attr_Parameter];
            msg_str = attr_str[Attr_Parameter];
            goto ISSUE_ERR;
         }
         break;	/* End constant case */

      case Compiler_Tmp:
      case Struct_Component:
         break;

      }  /* End switch */

      if (ATD_VOLATILE(attr_idx) && (attr_obj_ntry & (1 << Attr_Volatile))) {
         msg_num = attr_msg_num[new_obj][Attr_Volatile];
         msg_str = attr_str[Attr_Volatile];
         goto ISSUE_ERR;
      }

      if ((ATD_COPY_ASSUMED_SHAPE(attr_idx) ||
           (SCP_COPY_ASSUMED_SHAPE(curr_scp_idx) &&
            ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
            BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Assumed_Shape)) &&
          dir_obj_ntry & (1 << Dir_Copy_Assumed_Shape)) {
         msg_num = dir_msg_num[new_obj][Dir_Copy_Assumed_Shape];
         msg_str = dir_str[Dir_Copy_Assumed_Shape];
         goto ISSUE_ERR;
      }

      if (ATD_ALLOCATABLE(attr_idx) && 
          (attr_obj_ntry & (1 << Attr_Allocatable))) {
         msg_num = attr_msg_num[new_obj][Attr_Allocatable];
         msg_str = attr_str[Attr_Allocatable];
         goto ISSUE_ERR;
      }

      if (ATD_STACK(attr_idx) && 
          (attr_obj_ntry & (1 << Attr_Automatic))) {
         msg_num = attr_msg_num[new_obj][Attr_Automatic];
         msg_str = attr_str[Attr_Automatic];
         goto ISSUE_ERR;
      }

      if (ATD_FILL_SYMBOL(attr_idx) &&
          dir_obj_ntry & (1 << Dir_Fill_Symbol)) {
         msg_num = dir_msg_num[new_obj][Dir_Fill_Symbol];
         msg_str = dir_str[Dir_Fill_Symbol];
         goto ISSUE_ERR;
      }

      if (ATD_ALIGN_SYMBOL(attr_idx) &&
          dir_obj_ntry & (1 << Dir_Align_Symbol)) {
         msg_num = dir_msg_num[new_obj][Dir_Align_Symbol];
         msg_str = dir_str[Dir_Align_Symbol];
         goto ISSUE_ERR;
      }

      if (ATD_SECTION_GP(attr_idx) &&
          dir_obj_ntry & (1 << Dir_Section_Gp)) {
         msg_num = dir_msg_num[new_obj][Dir_Section_Gp];
         msg_str = dir_str[Dir_Section_Gp];
         goto ISSUE_ERR;
      }

      if (ATD_SECTION_NON_GP(attr_idx) &&
          dir_obj_ntry & (1 << Dir_Section_Non_Gp)) {
         msg_num = dir_msg_num[new_obj][Dir_Section_Non_Gp];
         msg_str = dir_str[Dir_Section_Non_Gp];
         goto ISSUE_ERR;
      }

      array_idx = ATD_ARRAY_IDX(attr_idx);

      if (array_idx != NULL_IDX) {

         if (attr_obj_ntry & (1 << Attr_Dimension)) {
            msg_num = attr_msg_num[new_obj][Attr_Dimension];
            msg_str = attr_str[Attr_Dimension];
            goto ISSUE_ERR;
         }

         switch (BD_ARRAY_CLASS(array_idx)) {
         case Explicit_Shape:

            switch (BD_ARRAY_SIZE(array_idx)) {
            case Unknown_Size:
            case Constant_Size:
            case Symbolic_Constant_Size:

               if (attr_obj_ntry & (1 << Attr_Explicit_Shp_Arr)) {
                  msg_num = attr_msg_num[new_obj][Attr_Explicit_Shp_Arr];
                  msg_str = attr_str[Attr_Explicit_Shp_Arr];
                  goto ISSUE_ERR;
               }
               break;

            case Var_Len_Array:

               if (other_obj_ntry & (1 << Other_Var_Len_Arr)) {
                  msg_num = other_msg_num[new_obj][Other_Var_Len_Arr];
                  goto ISSUE_ERR;
               }
               break;

            }
            break;

         case Assumed_Size:
            if (attr_obj_ntry & (1 << Attr_Assumed_Size_Arr)) {
               msg_num = attr_msg_num[new_obj][Attr_Assumed_Size_Arr];
               msg_str = attr_str[Attr_Assumed_Size_Arr];
               goto ISSUE_ERR;
            }
            break;

         case Deferred_Shape:
            if (attr_obj_ntry & (1 << Attr_Deferred_Shp_Arr)) {
               msg_num = attr_msg_num[new_obj][Attr_Deferred_Shp_Arr];
               msg_str = attr_str[Attr_Deferred_Shp_Arr];
               goto ISSUE_ERR;
            }
            break;

         case Assumed_Shape:
            if (attr_obj_ntry & (1 << Attr_Assumed_Shp_Arr)) {
               msg_num = attr_msg_num[new_obj][Attr_Assumed_Shp_Arr];
               msg_str = attr_str[Attr_Assumed_Shp_Arr];
               goto ISSUE_ERR;
            }
            break;

# ifdef _DEBUG
         default:
            PRINTMSG(AT_DEF_LINE(attr_idx), 179, Internal, 
                     AT_DEF_COLUMN(attr_idx), "fnd_semantic_err");
            break;
# endif
         }  /* End switch */
      }

# ifdef _F_MINUS_MINUS
      array_idx = ATD_PE_ARRAY_IDX(attr_idx);

      if (array_idx != NULL_IDX) {
         if (attr_obj_ntry & (1 << Attr_Co_Array)) {
            msg_num = attr_msg_num[new_obj][Attr_Co_Array];
            msg_str = attr_str[Attr_Co_Array];
            goto ISSUE_ERR;
         }
      }
# endif

      if (AT_TYPED(attr_idx)) {

         if (ATD_TYPE_IDX(attr_idx) == CRI_Ptr_8     ||
             ATD_TYPE_IDX(attr_idx) == CRI_Ch_Ptr_8) {

            if (name_obj_ntry & (1 << Name_Cri_Pointer)) {
               msg_num = name_msg_num[new_obj][Name_Cri_Pointer];
               msg_str = name_str[Name_Cri_Pointer];
               goto ISSUE_ERR;
            }
         }
         else if (attr_obj_ntry & (1 << Attr_Type)) {
            msg_num = attr_msg_num[new_obj][Attr_Type];
            msg_str = get_basic_type_str(ATD_TYPE_IDX(attr_idx));

            if (new_obj == Obj_Typed) {

               /* Try to get a real nice message.  Check to see if this is */
               /* being retyped as the same type.  If it is, use a better  */
               /* message.                                                 */

               if (strcmp(msg_str, obj_str[new_obj]) == 0) {
                  msg_num = 554;  /* Retype as same type message */
               }
            }

            goto ISSUE_ERR;
         }
         else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {

            if (TYP_CHAR_CLASS(ATD_TYPE_IDX(attr_idx)) == Assumed_Size_Char) { 

               if (attr_obj_ntry & (1 << Attr_Assumed_Type_Ch)) {
                  msg_num = attr_msg_num[new_obj][Attr_Assumed_Type_Ch];
                  msg_str = attr_str[Attr_Assumed_Type_Ch];
                  goto ISSUE_ERR;
               }
            }
            else if (TYP_CHAR_CLASS(ATD_TYPE_IDX(attr_idx)) == Var_Len_Char) {

               if (other_obj_ntry & (1 << Other_Var_Len_Ch)) {
                  msg_num = other_msg_num[new_obj][Other_Var_Len_Ch];
                  goto ISSUE_ERR;
               }
            }
         }
      }

      if (ATD_POINTER(attr_idx) && (attr_obj_ntry & (1 << Attr_Pointer))) {
         msg_num = attr_msg_num[new_obj][Attr_Pointer];
         msg_str = attr_str[Attr_Pointer];
         goto ISSUE_ERR;
      }
      else if (ATD_TARGET(attr_idx) && (attr_obj_ntry & (1 << Attr_Target))) {
         msg_num = attr_msg_num[new_obj][Attr_Target];
         msg_str = attr_str[Attr_Target];
         goto ISSUE_ERR;
      }
      else if (AT_NAMELIST_OBJ(attr_idx) && 
               (name_obj_ntry & (1 << Name_Namelist_Group_Obj))) {
         msg_num = name_msg_num[new_obj][Name_Namelist_Group_Obj];
         msg_str = name_str[Name_Namelist_Group_Obj];
         goto ISSUE_ERR;
      }
      break;


   case Pgm_Unit:

#ifdef KEY /* Bug 14150 */
     if (AT_BIND_ATTR(attr_idx) && 
	      (attr_obj_ntry & (1 << Attr_Bind))) {
	msg_num = attr_msg_num[new_obj][Attr_Bind];
	msg_str = attr_str[Attr_Bind];
	goto ISSUE_ERR;
      }
#endif /* KEY Bug 14150 */

      switch (ATP_PGM_UNIT(attr_idx)) {
      case Program:
         if (name_obj_ntry & (1 << Name_Program)) {
            msg_num = name_msg_num[new_obj][Name_Program];
            msg_str = name_str[Name_Program];
            goto ISSUE_ERR;
         }
         break;

      case Blockdata:
         if (name_obj_ntry & (1 << Name_Blockdata)) {
            msg_num = name_msg_num[new_obj][Name_Blockdata];
            msg_str = name_str[Name_Blockdata];
            goto ISSUE_ERR;
         }
         break;

      case Module:
         if (name_obj_ntry & (1 << Name_Module)) {
            msg_num = name_msg_num[new_obj][Name_Module];
            msg_str = name_str[Name_Module];
            goto ISSUE_ERR;
         }
         break;

      case Subroutine:

         /* Check if this is the current pgm unit, because the current pgm */
         /* unit will always have ATP_EXPL_ITRFC set.  It's okay to define */
         /* things in the current program unit, just not in other scoping  */
         /* units.  Need to check scope alive with alternate entry to make */
         /* sure this is a current alternate entry and not one that was    */
         /* declared in a previous module procedure within this module.    */

         if (attr_idx == SCP_ATTR_IDX(curr_scp_idx) ||
             (ATP_ALT_ENTRY(attr_idx) && ATP_SCP_ALIVE(attr_idx))) {

            if (name_obj_ntry & (1 << Name_Curr_Subr)) {
               msg_num = name_msg_num[new_obj][Name_Curr_Subr];
               msg_str = name_str[Name_Curr_Subr];
               goto ISSUE_ERR;
            }
         }
         else {
            switch (ATP_PROC(attr_idx)) {
            case Dummy_Proc:
               if (ATP_EXPL_ITRFC(attr_idx)) {

                  if (other_obj_ntry & (1 << Other_Expl_Interface)) {
                     msg_num = other_msg_num[new_obj][Other_Expl_Interface];
                     goto ISSUE_ERR;
                  }
               }
               else if (ATP_DCL_EXTERNAL(attr_idx)) {

                  if (attr_obj_ntry & (1 << Attr_External)) {
                     msg_num = attr_msg_num[new_obj][Attr_External];
                     msg_str = attr_str[Attr_External];
                     goto ISSUE_ERR;
                  }
               }
               else if (AT_OPTIONAL(attr_idx) &&
                        attr_obj_ntry & (1 << Attr_Optional)) {
                  msg_num = attr_msg_num[new_obj][Attr_Optional];
                  msg_str = attr_str[Attr_Optional];
                  goto ISSUE_ERR;
               }
               else if (AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref || 
                        other_obj_ntry & (1 << Other_Use_Subr)) {
                  msg_num = other_msg_num[new_obj][Other_Use_Subr];
                  goto ISSUE_ERR;
               }
               break;

            case Extern_Proc:
               if (ATP_EXPL_ITRFC(attr_idx)) {

                  if (other_obj_ntry & (1 << Other_Expl_Interface)) {
                     msg_num = other_msg_num[new_obj][Other_Expl_Interface];
                     goto ISSUE_ERR;
                  }
               }
               else if (ATP_DCL_EXTERNAL(attr_idx)) {

                  if (attr_obj_ntry & (1 << Attr_External)) {
                     msg_num = attr_msg_num[new_obj][Attr_External];
                     msg_str = attr_str[Attr_External];
                     goto ISSUE_ERR;
                  }
               }
               else if (AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref || 
                        other_obj_ntry & (1 << Other_Use_Subr)) {
                  msg_num = other_msg_num[new_obj][Other_Use_Subr];
                  goto ISSUE_ERR;
               }
               break;


            case Intrin_Proc:

               if (name_obj_ntry & (1 << Name_Intrinsic_Subr)) {
                     msg_num = name_msg_num[new_obj][Name_Intrinsic_Subr];
                     msg_str = name_str[Name_Intrinsic_Subr];
                     goto ISSUE_ERR;
               }
               break;

            case Module_Proc:

               if (ATP_EXPL_ITRFC(attr_idx)) {

                  if (other_obj_ntry & (1 << Other_Expl_Interface)) {
                     msg_num = other_msg_num[new_obj][Other_Expl_Interface];
                     goto ISSUE_ERR;
                  }
               }
               else if (name_obj_ntry & (1 << Name_Module_Proc)) {
                  msg_num = name_msg_num[new_obj][Name_Module_Proc];
                  msg_str = name_str[Name_Module_Proc];
                  goto ISSUE_ERR;
               }
               break;

            case Intern_Proc:
               if (name_obj_ntry & (1 << Name_Internal_Subr)) {
                  msg_num = name_msg_num[new_obj][Name_Internal_Subr];
                  msg_str = name_str[Name_Internal_Subr];
                  goto ISSUE_ERR;
               }
               break;

            case Unknown_Proc:
               if (other_obj_ntry & (1 << Other_Use_Subr)) {
                  msg_num = other_msg_num[new_obj][Other_Use_Subr];
                  goto ISSUE_ERR;
               }
               break;
            }        /* End switch */


            if (ATP_VFUNCTION(attr_idx) &&
                (dir_obj_ntry & (1 << Dir_Vfunction)) ) {
               msg_num = dir_msg_num[new_obj][Dir_Vfunction];
               msg_str = dir_str[Dir_Vfunction];
               goto ISSUE_ERR;
            }
            else if (ATP_NOSIDE_EFFECTS(attr_idx) &&
                    (dir_obj_ntry & (1 << Dir_No_Side_Effects)) ) {
               msg_num = dir_msg_num[new_obj][Dir_No_Side_Effects];
               msg_str = dir_str[Dir_No_Side_Effects];
               goto ISSUE_ERR;
            }
            else if (ATP_NAME_IN_STONE(attr_idx) &&
                     (dir_obj_ntry & (1 << Dir_Name)) ) {
               msg_num = dir_msg_num[new_obj][Dir_Name];
               msg_str = dir_str[Dir_Name];
               goto ISSUE_ERR;
            }
            else if ((ATP_INLINE_ALWAYS(attr_idx) || 
                      ATP_INLINE_NEVER(attr_idx))&&
                     (dir_obj_ntry & (1 << Dir_Inline)) ) {
               msg_num = dir_msg_num[new_obj][Dir_Inline];
               msg_str = dir_str[Dir_Inline];
               goto ISSUE_ERR;
            }
         }
         break;


      case Function:

         /* Check if this is the current pgm unit, because the current pgm */
         /* unit will always have ATP_EXPL_ITRFC set.  It's okay to define */
         /* things in the current program unit, just not in other scoping  */
         /* units.  Need to check scope alive with alternate entry to make */
         /* sure this is a current alternate entry and not one that was    */
         /* declared in a previous module procedure within this module.    */

         if (attr_idx == SCP_ATTR_IDX(curr_scp_idx) ||
             (ATP_ALT_ENTRY(attr_idx) && ATP_SCP_ALIVE(attr_idx))) {

            if (name_obj_ntry & (1 << Name_Curr_Func)) {
               msg_num = name_msg_num[new_obj][Name_Curr_Func];
               msg_str = name_str[Name_Curr_Func];
               goto ISSUE_ERR;
            } 
         }
         else {

            switch (ATP_PROC(attr_idx)) {
            case Dummy_Proc:
               if (ATP_EXPL_ITRFC(attr_idx)) {

                  if (other_obj_ntry & (1 << Other_Expl_Interface)) {
                     msg_num = other_msg_num[new_obj][Other_Expl_Interface];
                     goto ISSUE_ERR;
                  }
               }
               else if (ATP_DCL_EXTERNAL(attr_idx)) {

                  if (attr_obj_ntry & (1 << Attr_External)) {
                     msg_num = attr_msg_num[new_obj][Attr_External];
                     msg_str = attr_str[Attr_External];
                     goto ISSUE_ERR;
                  }
               }
               else if (AT_OPTIONAL(attr_idx) &&
                        attr_obj_ntry & (1 << Attr_Optional)) {
                  msg_num = attr_msg_num[new_obj][Attr_Optional];
                  msg_str = attr_str[Attr_Optional];
                  goto ISSUE_ERR;
               }
               else if ((AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref || 
                         AT_DEFINED(attr_idx)) &&
                        (other_obj_ntry & (1 << Other_Use_Func))) {
                  msg_num = other_msg_num[new_obj][Other_Use_Func];
                  goto ISSUE_ERR;
               }
               break;

            case Extern_Proc:
               if (ATP_EXPL_ITRFC(attr_idx)) {

                  if (other_obj_ntry & (1 << Other_Expl_Interface)) {
                     msg_num = other_msg_num[new_obj][Other_Expl_Interface];
                     goto ISSUE_ERR;
                  }
               }
               else if (ATP_DCL_EXTERNAL(attr_idx)) {

                  if (attr_obj_ntry & (1 << Attr_External)) {
                     msg_num = attr_msg_num[new_obj][Attr_External];
                     msg_str = attr_str[Attr_External];
                     goto ISSUE_ERR;
                  }
               }
               else if ((AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref || 
                         AT_DEFINED(attr_idx)) &&
                        (other_obj_ntry & (1 << Other_Use_Func))) {
                  msg_num = other_msg_num[new_obj][Other_Use_Func];
                  goto ISSUE_ERR;
               }
               break;

            case Intrin_Proc:

               if (name_obj_ntry & (1 << Name_Intrinsic_Func)) {
                  msg_num = name_msg_num[new_obj][Name_Intrinsic_Func];
                  msg_str = name_str[Name_Intrinsic_Func];
                  goto ISSUE_ERR;
               }
               break;

            case Module_Proc:

               if (ATP_EXPL_ITRFC(attr_idx)) {

                  if (other_obj_ntry & (1 << Other_Expl_Interface)) {
                     msg_num = other_msg_num[new_obj][Other_Expl_Interface];
                     goto ISSUE_ERR;
                  }
               }
               else if (name_obj_ntry & (1 << Name_Module_Proc)) {
                  msg_num = name_msg_num[new_obj][Name_Module_Proc];
                  msg_str = name_str[Name_Module_Proc];
                  goto ISSUE_ERR;
               }
               break;

            case Intern_Proc:
               if (name_obj_ntry & (1 << Name_Internal_Func)) {
                  msg_num = name_msg_num[new_obj][Name_Internal_Func];
                  msg_str = name_str[Name_Internal_Func];
                  goto ISSUE_ERR;
               }
               break;

            case Unknown_Proc:

               if ((AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref || 
                    AT_DEFINED(attr_idx)) &&
                   (other_obj_ntry & (1 << Other_Use_Func))) {
                  msg_num = other_msg_num[new_obj][Other_Use_Func];
                  goto ISSUE_ERR;
               } 
               break;
            }        /* End switch */

            if (ATP_VFUNCTION(attr_idx) &&
                     (dir_obj_ntry & (1 << Dir_Vfunction)) ) {
               msg_num = dir_msg_num[new_obj][Dir_Vfunction];
               msg_str = dir_str[Dir_Vfunction];
               goto ISSUE_ERR;
            }
            else if (ATP_NOSIDE_EFFECTS(attr_idx) &&
                (dir_obj_ntry & (1 << Dir_No_Side_Effects)) ) {
               msg_num = dir_msg_num[new_obj][Dir_No_Side_Effects];
               msg_str = dir_str[Dir_No_Side_Effects];
               goto ISSUE_ERR;
            }
            else if (ATP_NAME_IN_STONE(attr_idx) &&
                     (dir_obj_ntry & (1 << Dir_Name)) ) {
               msg_num = dir_msg_num[new_obj][Dir_Name];
               msg_str = dir_str[Dir_Name];
               goto ISSUE_ERR;
            }
            else if (name_obj_ntry & (1 << Name_Function)) {
               msg_num = name_msg_num[new_obj][Name_Function];
               msg_str = name_str[Name_Function];
               goto ISSUE_ERR;
            } 
         }

         rslt_idx    = ATP_RSLT_IDX(attr_idx);
         array_idx   = ATD_ARRAY_IDX(rslt_idx);

         if (array_idx != NULL_IDX) {

            if (attr_obj_ntry & (1 << Attr_Dimension)) {
               msg_num = attr_msg_num[new_obj][Attr_Dimension];
               msg_str = attr_str[Attr_Dimension];
               goto ISSUE_ERR;
            }

            switch (BD_ARRAY_CLASS(array_idx)) {
            case Explicit_Shape:

               switch (BD_ARRAY_SIZE(array_idx)) {
               case Unknown_Size:
               case Constant_Size:
               case Symbolic_Constant_Size:
                  if (attr_obj_ntry & (1 << Attr_Explicit_Shp_Arr)) {
                     msg_num = attr_msg_num[new_obj][Attr_Explicit_Shp_Arr];
                     msg_str = attr_str[Attr_Explicit_Shp_Arr];
                     goto ISSUE_ERR;
                  }
                  break;

               case Var_Len_Array:
                  if (other_obj_ntry & (1 << Other_Var_Len_Arr)) {
                     msg_num = other_msg_num[new_obj][Other_Var_Len_Arr];
                     goto ISSUE_ERR;
                  }
                  break;
               }  /* End switch */
               break;

            case Deferred_Shape:
               if (attr_obj_ntry & (1 << Attr_Deferred_Shp_Arr)) {
                  msg_num = attr_msg_num[new_obj][Attr_Deferred_Shp_Arr];
                  msg_str = attr_str[Attr_Deferred_Shp_Arr];
                  goto ISSUE_ERR;
               }
               break;
            }  /* End switch */
         }

         if (AT_TYPED(rslt_idx)) {

            if (attr_obj_ntry & (1 << Attr_Type)) {
               msg_num = attr_msg_num[new_obj][Attr_Type];
               msg_str = get_basic_type_str(ATD_TYPE_IDX(rslt_idx));

               if (new_obj == Obj_Typed) {

                  /* Try to get a real nice message.  Check to see if this is */
                  /* being retyped as the same type.  If it is, use a better  */
                  /* message.                                                 */

                  if (strcmp(msg_str, obj_str[new_obj]) == 0) {
                     msg_num = 554;  /* Retype as same type message */
                  }
               }
               goto ISSUE_ERR;
            }
            else if (TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) == Character) {

               if (TYP_CHAR_CLASS(ATD_TYPE_IDX(rslt_idx)) == Assumed_Size_Char){

                  if (attr_obj_ntry & (1 << Attr_Assumed_Type_Ch)) {
                     msg_num = attr_msg_num[new_obj][Attr_Assumed_Type_Ch];
                     msg_str = attr_str[Attr_Assumed_Type_Ch];
                     goto ISSUE_ERR;
                  }
               }
               else if (TYP_CHAR_CLASS(ATD_TYPE_IDX(rslt_idx)) == Var_Len_Char){

                  if (other_obj_ntry & (1 << Other_Var_Len_Ch)) {
                     msg_num = other_msg_num[new_obj][Other_Var_Len_Ch];
                     goto ISSUE_ERR;
                  }
               }
            }
         }

         if (ATD_POINTER(rslt_idx) && (attr_obj_ntry & (1 << Attr_Pointer))){
            msg_num = attr_msg_num[new_obj][Attr_Pointer];
            msg_str = attr_str[Attr_Pointer];
            goto ISSUE_ERR;
         }
         else if (ATD_TARGET(rslt_idx) && (attr_obj_ntry & (1 << Attr_Target))){
            msg_num = attr_msg_num[new_obj][Attr_Target];
            msg_str = attr_str[Attr_Target];
            goto ISSUE_ERR;
         }
         break;


      case Pgm_Unknown:

         switch (ATP_PROC(attr_idx)) {
         case Dummy_Proc:
         case Extern_Proc:
            if (ATP_DCL_EXTERNAL(attr_idx)) {

               if (attr_obj_ntry & (1 << Attr_External)) {
                  msg_num = attr_msg_num[new_obj][Attr_External];
                  msg_str = attr_str[Attr_External];
                  goto ISSUE_ERR;
               }
            }
            else if (AT_OPTIONAL(attr_idx) &&
                     attr_obj_ntry & (1 << Attr_Optional)) {
               msg_num = attr_msg_num[new_obj][Attr_Optional];
               msg_str = attr_str[Attr_Optional];
               goto ISSUE_ERR;
            }
            else if (ATP_NAME_IN_STONE(attr_idx) &&
                     (dir_obj_ntry & (1 << Dir_Name)) ) {
               msg_num = dir_msg_num[new_obj][Dir_Name];
               msg_str = dir_str[Dir_Name];
               goto ISSUE_ERR;
            }
            else if ((ATP_INLINE_ALWAYS(attr_idx) || 
                      ATP_INLINE_NEVER(attr_idx))&&
                     (dir_obj_ntry & (1 << Dir_Inline)) ) {
               msg_num = dir_msg_num[new_obj][Dir_Inline];
               msg_str = dir_str[Dir_Inline];
               goto ISSUE_ERR;
            }
            break;

         case Intrin_Proc:
            break;

         case Module_Proc:

            if (name_obj_ntry & (1 << Name_Module_Proc)) {
               msg_num = name_msg_num[new_obj][Name_Module_Proc];
               msg_str = name_str[Name_Module_Proc];
               goto ISSUE_ERR;
            }
            break;
# ifdef _DEBUG
         default:
            break;
# endif
         }        /* End switch */

         if (ATP_VFUNCTION(attr_idx) && (dir_obj_ntry & (1 << Dir_Vfunction))) {
            msg_num = dir_msg_num[new_obj][Dir_Vfunction];
            msg_str = dir_str[Dir_Vfunction];
            goto ISSUE_ERR;
         }
         else if (ATP_NOSIDE_EFFECTS(attr_idx) &&
             (dir_obj_ntry & (1 << Dir_No_Side_Effects)) ) {
            msg_num = dir_msg_num[new_obj][Dir_No_Side_Effects];
            msg_str = dir_str[Dir_No_Side_Effects];
            goto ISSUE_ERR;
         }
         else if ((ATP_INLINE_ALWAYS(attr_idx) || 
                   ATP_INLINE_NEVER(attr_idx))&&
                  (dir_obj_ntry & (1 << Dir_Inline)) ) {
            msg_num = dir_msg_num[new_obj][Dir_Inline];
            msg_str = dir_str[Dir_Inline];
            goto ISSUE_ERR;
         }
         break;

      }     /* End switch */

      if (ATP_OPTIONAL_DIR(attr_idx) && (dir_obj_ntry & (1 << Dir_Optional)) ) {
         msg_num = dir_msg_num[new_obj][Dir_Optional];
         msg_str = dir_str[Dir_Optional];
         goto ISSUE_ERR;
      }

      break;


   case Label:

# ifdef _DEBUG
      if (ATL_DEBUG_CLASS(attr_idx) != Ldbg_Construct_Name) {
         PRINTMSG(line, 257, Internal, column,
                  ATL_CLASS(attr_idx), "ATL_CLASS");
      }
# endif

      if (name_obj_ntry & (1 << Name_Construct)) {
         msg_num = name_msg_num[new_obj][Name_Construct];
         msg_str = name_str[Name_Construct];
         goto ISSUE_ERR;
      }
      break;

   case Derived_Type:

      if (name_obj_ntry & (1 << Name_Derived_Type)) {
         msg_num = name_msg_num[new_obj][Name_Derived_Type];
         msg_str = name_str[Name_Derived_Type];
         goto ISSUE_ERR;
      }
#ifdef KEY /* Bug 14150 */
      if (AT_BIND_ATTR(attr_idx) && 
	       (attr_obj_ntry & (1 << Attr_Bind))) {
	 msg_num = attr_msg_num[new_obj][Attr_Bind];
	 msg_str = attr_str[Attr_Bind];
	 goto ISSUE_ERR;
      }
#endif /* KEY Bug 14150 */
      break;

   case Interface:

      if (AT_IS_INTRIN(attr_idx) && !ATI_USER_SPECIFIED(attr_idx)) {

         if (ATI_DCL_INTRINSIC(attr_idx) && 
            (attr_obj_ntry & (1 << Attr_Intrinsic))) {
            msg_num = attr_msg_num[new_obj][Attr_Intrinsic];
            msg_str = attr_str[Attr_Intrinsic];
            goto ISSUE_ERR;
         }

         if (ATI_INTERFACE_CLASS(attr_idx) == Generic_Subroutine_Interface) {

            if (name_obj_ntry & (1 << Name_Intrinsic_Subr)) {
               msg_num = name_msg_num[new_obj][Name_Intrinsic_Subr];
               msg_str = name_str[Name_Intrinsic_Subr];
               goto ISSUE_ERR;
            }
         }
         else if (name_obj_ntry & (1 << Name_Intrinsic_Func)) {
            msg_num = name_msg_num[new_obj][Name_Intrinsic_Func];
            msg_str = name_str[Name_Intrinsic_Func];
            goto ISSUE_ERR;
         }
      }
      else {

         if (name_obj_ntry & (1 << Name_Generic_Interface)) {
            msg_num = name_msg_num[new_obj][Name_Generic_Interface];
            msg_str = name_str[Name_Generic_Interface];
            goto ISSUE_ERR;
         }
      }

      if (AT_TYPED(attr_idx) && (attr_obj_ntry & (1 << Attr_Type))) {
         msg_num = attr_msg_num[new_obj][Attr_Type];
         msg_str = get_basic_type_str(ATD_TYPE_IDX(attr_idx));

         if (new_obj == Obj_Typed) {

            /* Try to get a real nice message.  Check to see if this is */
            /* being retyped as the same type.  If it is, use a better  */
            /* message.                                                 */

            if (strcmp(msg_str, obj_str[new_obj]) == 0) {
               msg_num = 554;  /* Retype as same type message */
            }
         }
         goto ISSUE_ERR;
      }

      break;

   case Namelist_Grp:

      if (name_obj_ntry & (1 << Name_Namelist_Group)) {
         msg_num = name_msg_num[new_obj][Name_Namelist_Group];
         msg_str = name_str[Name_Namelist_Group];
         goto ISSUE_ERR;
      }
      break;

   case Stmt_Func:

      if (name_obj_ntry & (1 << Name_Statement_Func)) {
         msg_num = name_msg_num[new_obj][Name_Statement_Func];
         msg_str = name_str[Name_Statement_Func];
         goto ISSUE_ERR;
      }

      if (AT_TYPED(attr_idx) && (attr_obj_ntry & (1 << Attr_Type))) {
         msg_num = attr_msg_num[new_obj][Attr_Type];
         msg_str = get_basic_type_str(ATD_TYPE_IDX(attr_idx));

         if (new_obj == Obj_Typed) {

            /* Try to get a real nice message.  Check to see if this is */
            /* being retyped as the same type.  If it is, use a better  */
            /* message.                                                 */

            if (strcmp(msg_str, obj_str[new_obj]) == 0) {
               msg_num = 554;  /* Retype as same type message */
            }
         }
         goto ISSUE_ERR;
      }

      break;

   }  /* End switch */

   if ((attr_obj_ntry & (1 << Attr_Public)) ||
       (attr_obj_ntry & (1 << Attr_Private))) {

      if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module &&
          AT_ACCESS_SET(attr_idx)) {		/* Access only in module */

         if (AT_PRIVATE(attr_idx)) {
            msg_num = attr_msg_num[new_obj][Attr_Private];
            msg_str = attr_str[Attr_Private];
         }
         else {
            msg_num = attr_msg_num[new_obj][Attr_Public];
            msg_str = attr_str[Attr_Public];
         }
      }
   }

ISSUE_ERR:

   if (msg_num != 0) {

      if (issue_msg) {

         if (msg_str == NULL) {  /* This is an Other.  Does not need msg_str. */
            PRINTMSG(line, msg_num, Error, column,
                     AT_OBJ_NAME_PTR(attr_idx),
                     obj_str[new_obj]);
         }
         else if (new_obj > Obj_Name_Done) {
            PRINTMSG(line, msg_num, Error, column,
                     AT_OBJ_NAME_PTR(attr_idx),
                     msg_str);
         }
         else {
#ifdef KEY /* Bug 5040 */
	    /* Added the initial-declaration line number to all of the
	     * messages in attr_msg_num which previously took 3 arg strings;
	     * because AT_DEF_LINE(attr_idx) is at the end of the varargs list,
	     * it's harmless if a message doesn't want to use it.
	     */
            PRINTMSG(line, msg_num, Error, column,
                     AT_OBJ_NAME_PTR(attr_idx),
                     msg_str,
                     obj_str[new_obj],
		     AT_DEF_LINE(attr_idx));
#else /* KEY Bug 5040 */
            PRINTMSG(line, msg_num, Error, column,
                     AT_OBJ_NAME_PTR(attr_idx),
                     msg_str,
                     obj_str[new_obj]);
#endif /* KEY Bug 5040 */
         }
      }

      if (set_dcl_err) {

         switch (new_obj) {

            case Obj_Use_Extern_Func:
            case Obj_Use_Extern_Subr:
            case Obj_Use_In_Expr:
            case Obj_Use_Spec_Expr:
            case Obj_Use_Init_Expr:
               break;

            default:
               AT_DCL_ERR(attr_idx) = TRUE;
               break;

         }  /* End switch */
      }
   }

   TRACE (Func_Exit, "fnd_semantic_err", NULL);

   return(msg_num);

}  /* fnd_semantic_err */
# ifdef _DEBUG

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This is called from main initialization of the compiler to verify     *|
|*	that every bit set in obj_to_attr, obj_to_name, and obj_to_other      *|
|*	has a msg number in attr_msg_num, name_msg_num, and other_msg_num.    *|
|*	It also checks the reverse combination.  If it a missing message,     *|
|*	or too many messages, it issues a fatal error for each one it finds.  *|
|*	Then after it has gone thru the complete tables, it will issue an.    *|
|*	internal abort.                                                       *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if it found an error.					      *|
|*									      *|
\******************************************************************************/
void	verify_semantic_tbls()
{

   long		attr_entry;
   long		dir_entry;
   boolean	found_err	= FALSE;
   long		idx;
   long		j;
   long		name_entry;
   long		other_entry;


   TRACE (Func_Entry, "verify_semantic_tbls", NULL);

   for (idx = 0; idx < Obj_Done; idx++) {
      attr_entry	= obj_to_attr[idx];
      dir_entry		= obj_to_dir[idx];
      name_entry	= obj_to_name[idx];
      other_entry	= obj_to_other[idx];

      for (j = 0; j < Attr_Done; j++) {

         if ((1 & attr_entry) != 0) {

            if (attr_msg_num[idx][j] == 0) {
               PRINTMSG(1, 225, Error, 0, obj_type_str[idx],
                        attr_obj_type_str[j], "obj_to_attr");
               found_err = TRUE;
            }
         }
         else if (attr_msg_num[idx][j] != 0) {
            PRINTMSG(1, 227, Error, 0, obj_type_str[idx],
                     attr_obj_type_str[j], "attr_msg_num", 
                     attr_msg_num[idx][j]);
            found_err = TRUE;
         }
         attr_entry = attr_entry >> 1;
      }

      for (j = 0; j < Dir_Done; j++) {

         if ((1 & dir_entry) != 0) {

            if (dir_msg_num[idx][j] == 0) {
               PRINTMSG(1, 225, Error, 0, obj_type_str[idx],
                        dir_obj_type_str[j], "obj_to_dir");
               found_err = TRUE;
            }
         }
         else if (dir_msg_num[idx][j] != 0) {
            PRINTMSG(1, 227, Error, 0, obj_type_str[idx],
                     dir_obj_type_str[j], "dir_msg_num", 
                     dir_msg_num[idx][j]);
            found_err = TRUE;
         }
         dir_entry = dir_entry >> 1;
      }

      for (j = 0; j < Name_Done; j++) {

         if ((1 & name_entry) != 0) {

            if (name_msg_num[idx][j] == 0) {
               PRINTMSG(1, 225, Error, 0, obj_type_str[idx], 
                        name_obj_type_str[j], "obj_to_name");
               found_err = TRUE;
            }
         }
         else if (name_msg_num[idx][j] != 0) {
            PRINTMSG(1, 227, Error, 0, obj_type_str[idx],
                     name_obj_type_str[j], "name_msg_num", 
                     name_msg_num[idx][j]);
            found_err = TRUE;
         }
         name_entry = name_entry >> 1;
      }

      for (j = 0; j < Other_Done; j++) {

         if ((1 & other_entry) != 0) {

            if (other_msg_num[idx][j] == 0) {
               PRINTMSG(1, 225, Error, 0, obj_type_str[idx],
                        other_obj_type_str[j], "obj_to_other");
               found_err = TRUE;
            }
         }
         else if (other_msg_num[idx][j] != 0) {
            PRINTMSG(1, 227, Error, 0, obj_type_str[idx],
                     other_obj_type_str[j], "other_msg_num", 
                     other_msg_num[idx][j]);
            found_err = TRUE;
         }
         other_entry = other_entry >> 1;
      }
   }

   if (found_err) {			/* If problems - halt compilation */
      PRINTMSG(1, 226, Internal, 0);
   }

   TRACE (Func_Exit, "verify_semantic_tbls", NULL);

   return;

}  /* verify_semantic_tbls */
# endif

/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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



const static char *source_file = __FILE__;
const static char *rcs_id = "$Source: common/com/SCCS/s.dwarf_DST.cxx $ $Revision: 1.5 $";

#include "dwarf_DST.h"
#include "errors.h"         /* in ../common/util */


     /*---------------------------------*
      * Macros used for error reporting *
      *---------------------------------*/

#define DST_ASSERT(truth, msg) Is_True(truth, (msg))


    /*-------------------------------------------------------------
     * Routine defining the parent->child relation of info entries
     *--------------------------------------------------------------*/


/* Gets a pointer to the child field of "parent", if one exists,
 * otherwise NULL is returned.  WARNING:  The parent->child
 * relation is localized to this routine.  For any change in this
 * relation (e.g. by adding a new node with child attribute to the
 * DST specification) a corresponding change should be made to this
 * function.
*/
static DST_INFO_IDX *
DST_get_ptr_to_firstChildField(DST_INFO *parent) 
{
   const DST_ATTR_IDX attr = DST_INFO_attributes(parent);
   const DST_flag     flag = DST_INFO_flag(parent);
   DST_INFO_IDX      *field_ptr = NULL;
   
   switch (DST_INFO_tag(parent))
   {
   case DW_TAG_compile_unit:
      field_ptr = &DST_COMPILE_UNIT_first_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_COMPILE_UNIT));
      break;

#ifdef KEY /* Bug 3507 */
   case DW_TAG_module:
      field_ptr = &DST_MODULE_first_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_MODULE));
      break;
#endif /* KEY Bug 3507 */

   case DW_TAG_subprogram:
      if (DST_IS_memdef(flag))
        field_ptr = &DST_SUBPROGRAM_memdef_first_child(
                        DST_ATTR_IDX_TO_PTR(attr, DST_SUBPROGRAM));
      else if (DST_IS_declaration(flag))
        field_ptr = &DST_SUBPROGRAM_decl_first_child(
                        DST_ATTR_IDX_TO_PTR(attr, DST_SUBPROGRAM));
      else
        field_ptr = &DST_SUBPROGRAM_def_first_child(
                        DST_ATTR_IDX_TO_PTR(attr, DST_SUBPROGRAM));
      break;

   case DW_TAG_entry_point:
     field_ptr = &DST_ENTRY_POINT_first_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_ENTRY_POINT));
     break;

   case DW_TAG_inlined_subroutine:
      field_ptr = &DST_INLINED_SUBROUTINE_first_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_INLINED_SUBROUTINE));
      break;

   case DW_TAG_common_block:
     field_ptr = &DST_COMMON_BLOCK_first_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_COMMON_BLOCK));
     break;

   case DW_TAG_lexical_block:
      field_ptr = &DST_LEXICAL_BLOCK_first_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_LEXICAL_BLOCK));
      break;

   case DW_TAG_array_type:
      field_ptr = &DST_ARRAY_TYPE_first_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_ARRAY_TYPE));
      break;

   case DW_TAG_structure_type:
      field_ptr = &DST_STRUCTURE_TYPE_first_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_STRUCTURE_TYPE));
      break;

   case DW_TAG_union_type:
      field_ptr = &DST_UNION_TYPE_first_child(
	              DST_ATTR_IDX_TO_PTR(attr, DST_UNION_TYPE));
      break;

   case DW_TAG_class_type:
      field_ptr = &DST_CLASS_TYPE_first_child(
	              DST_ATTR_IDX_TO_PTR(attr, DST_CLASS_TYPE));
      break;

   case DW_TAG_enumeration_type:
      field_ptr = &DST_ENUMERATION_TYPE_first_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_ENUMERATION_TYPE));
      break;

   case DW_TAG_subroutine_type:
      field_ptr = &DST_SUBROUTINE_TYPE_first_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_SUBROUTINE_TYPE));
      break;

   default:
      field_ptr = NULL;
      break; 
   }
   return field_ptr;
} /* DST_get_ptr_to_firstChildField */


/*
#if !(defined(MONGOOSE_BE)) || defined(_STANDALONE_INLINER) || defined(_SUPPORT_IPA)
*/
/* Same as DST_get_ptr_to_childfield(), but for the "last_child" field.
 * For any change in this relation (e.g. by adding a new node with 
 * child attribute to the DST specification) a corresponding change 
 * should be made to this function.
*/
static DST_INFO_IDX *
DST_get_ptr_to_lastChildField(DST_INFO *parent) 
{
   const DST_ATTR_IDX attr = DST_INFO_attributes(parent);
   const DST_flag     flag = DST_INFO_flag(parent);
   DST_INFO_IDX      *field_ptr = NULL;
   
   switch (DST_INFO_tag(parent))
   {
   case DW_TAG_compile_unit:
      field_ptr = &DST_COMPILE_UNIT_last_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_COMPILE_UNIT));
      break;

#ifdef KEY /* Bug 3507 */
   case DW_TAG_module:
      field_ptr = &DST_MODULE_last_child(
		      DST_ATTR_IDX_TO_PTR(attr, DST_MODULE));
      break;
#endif /* KEY Bug 3507 */

   case DW_TAG_subprogram:
      if (DST_IS_memdef(flag))
        field_ptr = &DST_SUBPROGRAM_memdef_last_child(
                        DST_ATTR_IDX_TO_PTR(attr, DST_SUBPROGRAM));
      else if (DST_IS_declaration(flag))
        field_ptr = &DST_SUBPROGRAM_decl_last_child(
                        DST_ATTR_IDX_TO_PTR(attr, DST_SUBPROGRAM));
      else
        field_ptr = &DST_SUBPROGRAM_def_last_child(
                        DST_ATTR_IDX_TO_PTR(attr, DST_SUBPROGRAM));
      break;

   case DW_TAG_entry_point:
     field_ptr = &DST_ENTRY_POINT_last_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_ENTRY_POINT));
     break;

   case DW_TAG_inlined_subroutine:
      field_ptr = &DST_INLINED_SUBROUTINE_last_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_INLINED_SUBROUTINE));
      break;

   case DW_TAG_common_block:
     field_ptr = &DST_COMMON_BLOCK_last_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_COMMON_BLOCK));
     break;

   case DW_TAG_lexical_block:
      field_ptr = &DST_LEXICAL_BLOCK_last_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_LEXICAL_BLOCK));
      break;

   case DW_TAG_array_type:
      field_ptr = &DST_ARRAY_TYPE_last_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_ARRAY_TYPE));
      break;

   case DW_TAG_structure_type:
      field_ptr = &DST_STRUCTURE_TYPE_last_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_STRUCTURE_TYPE));
      break;

   case DW_TAG_union_type:
      field_ptr = &DST_UNION_TYPE_last_child(
	              DST_ATTR_IDX_TO_PTR(attr, DST_UNION_TYPE));
      break;

   case DW_TAG_class_type:
      field_ptr = &DST_CLASS_TYPE_last_child(
	              DST_ATTR_IDX_TO_PTR(attr, DST_CLASS_TYPE));
      break;

   case DW_TAG_enumeration_type:
      field_ptr = &DST_ENUMERATION_TYPE_last_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_ENUMERATION_TYPE));
      break;

   case DW_TAG_subroutine_type:
      field_ptr = &DST_SUBROUTINE_TYPE_last_child(
                      DST_ATTR_IDX_TO_PTR(attr, DST_SUBROUTINE_TYPE));
      break;

   default:
      field_ptr = NULL;
      break; 
   }
   return field_ptr;
} /* DST_get_ptr_to_lastChildField */
/*
#endif * !MONGOOSE_BE || _STANDALONE_INLINER || _SUPPORT_IPA */


   /*-----------------
    * Visible routines
    *-----------------*/


DST_INFO_IDX
DST_first_child(DST_INFO_IDX parent) 
{
   DST_INFO_IDX *child_ptr;
   
   child_ptr = DST_get_ptr_to_firstChildField(DST_INFO_IDX_TO_PTR(parent));
   if (child_ptr == NULL)
      return DST_INVALID_IDX;
   else
      return *child_ptr;
}



/* 
#if !(defined(MONGOOSE_BE)) || defined(_STANDALONE_INLINER) || defined(_SUPPORT_IPA)
*/
DST_INFO_IDX
DST_last_child(DST_INFO_IDX parent) 
{
   DST_INFO_IDX *child_ptr;
   
   child_ptr = DST_get_ptr_to_lastChildField(DST_INFO_IDX_TO_PTR(parent));
   if (child_ptr == NULL)
      return DST_INVALID_IDX;
   else
      return *child_ptr;
}


/* Appends a child to the end of the list of children (linked by sibling 
 * indices) for the given "info" record.
*/
void
DST_append_child(DST_INFO_IDX parent, DST_INFO_IDX child)
{
   DST_INFO_IDX *last_child_field;
   DST_INFO     *child_info;
   DST_INFO	*parent_info = DST_INFO_IDX_TO_PTR(parent);

   FmtAssert(parent_info != NULL, ("Illegal attempt to append DST child"));
   last_child_field = DST_get_ptr_to_lastChildField(parent_info);
   DST_ASSERT(last_child_field != NULL, "Illegal attempt to append DST child");
   
   if (DST_IS_NULL(*last_child_field))
   {
      /* Set the pointer to the first child for this parent */
      *DST_get_ptr_to_firstChildField(parent_info) = child;
   }
   else
   {
      /* Append the new child after the last child */
      child_info = DST_INFO_IDX_TO_PTR(*last_child_field);
      DST_INFO_sibling(child_info) = child;
   }

   /* The new child is now the last_child */
   *last_child_field = child;
}


void
DST_preorder_visit(
   DST_INFO_IDX i,
   INT32        init_val, 
   INT32 (*action)(INT32, DST_DW_tag, DST_flag, DST_ATTR_IDX, DST_INFO_IDX))
{
   DST_INFO    *info;
   DST_INFO_IDX idx, *idx_ptr;
   INT32        val;
   
   DST_ASSERT(!DST_IS_NULL(i), "Attempt to visit invalid DST info");

   /* Visit this node */
   info = DST_INFO_IDX_TO_PTR(i);
   val = (*action)(init_val, 
		   DST_INFO_tag(info), 
		   DST_INFO_flag(info), 
		   DST_INFO_attributes(info), 
		   i);

   /* Now, visit each child */
   idx_ptr = DST_get_ptr_to_firstChildField(info);
   if (idx_ptr != NULL)
   {
      for (idx = *idx_ptr;
	   !DST_IS_NULL(idx);
	   idx = DST_INFO_sibling(DST_INFO_IDX_TO_PTR(idx)))
      {
	 DST_preorder_visit(idx, val, action);
      }
   }
}
/*
#endif * MONGOOSE_BE || _STANDALONE_INLINER || _SUPPORT_IPA */



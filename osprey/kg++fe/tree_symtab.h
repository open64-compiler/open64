/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
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


/* translate gnu decl trees to symtab references */

#ifndef tree_symtab_INCLUDED
#define tree_symtab_INCLUDED

#include "wfe_dst.h"

extern TY_IDX Create_TY_For_Tree (tree, TY_IDX idx = TY_IDX_ZERO);
extern "C" ST* Create_ST_For_Tree (tree);


#ifdef EXTRA_WORD_IN_TREE_NODES

/*
 * Hacked macro defintions to get things to compile.
 * DECL_NAME was chosen completely arbitrarily.
 */

#define TYPE_TY_IDX(tree) ((TY_IDX) DECL_NAME(tree))
#define DECL_ST(tree)     ((ST *)   DECL_NAME(tree))
#define DECL_SYMTAB_IDX(tree) ((SYMTAB_IDX) DECL_NAME(tree))
#define DECL_LABEL_IDX(tree) ((LABEL_IDX) DECL_NAME(tree))
#define TREE_STRING_ST(tree) ((ST *) DECL_NAME(tree))
#define DECL_LABEL_DEFINED(tree) ((BOOL) DECL_NAME(tree))
#define DECL_FIELD_ID(tree) ((INT32) DECL_NAME(tree))
#define DECL_DST_IDX(tree) ((INT32) DECL_NAME(tree))
#define DECL_DST_FIELD_IDX(tree) ((INT32) DECL_NAME(tree))
#define DECL_DST_ABSTRACT_ROOT_IDX(tree) ((INT32) DECL_NAME(tree))
#define DECL_DST_SPECIFICATION_IDX(tree) ((INT32) DECL_NAME(tree))
#define TYPE_DST_IDX(tree) ((INT32) DECL_NAME(tree))


#else /* EXTRA_WORD_IN_TREE_NODES */

// These are defined in tree.h.
#undef TREE_STRING_ST
#undef TYPE_TY_IDX
#undef TYPE_FIELD_IDS_USED
#undef TYPE_DST_IDX
#undef DECL_ST
#undef DECL_FIELD_ID
#undef DECL_DST_IDX

TY_IDX& TYPE_TY_IDX(tree);
#ifdef KEY
extern "C" void add_duplicates (tree, tree);
extern "C" void erase_duplicates (tree);
void set_DECL_ST(tree, ST*);
ST*& get_DECL_ST(tree);
BOOL& expanded_decl(tree);
BOOL& func_PU_uplevel(tree);
#define DECL_ST(x)	get_DECL_ST(x)
#else
ST*& DECL_ST(tree);
#endif
SYMTAB_IDX& DECL_SYMTAB_IDX(tree);
LABEL_IDX& DECL_LABEL_IDX(tree);
ST*& TREE_STRING_ST(tree);
BOOL& DECL_LABEL_DEFINED(tree);
INT32& DECL_FIELD_ID(tree);
INT32& TYPE_FIELD_IDS_USED(tree);
INT32& SCOPE_NUMBER(tree);
tree& LABEL_SCOPE(tree);
DST_INFO_IDX& DECL_DST_IDX(tree);
DST_INFO_IDX& DECL_DST_FIELD_IDX(tree);
DST_INFO_IDX& TYPE_DST_IDX(tree);
DST_INFO_IDX& DECL_DST_SPECIFICATION_IDX(tree);
DST_INFO_IDX& DECL_DST_ABSTRACT_ROOT_IDX(tree);
LABEL_IDX& HANDLER_LABEL(tree);
#ifdef KEY
tree& PARENT_SCOPE(tree);
INT32& WEAK_WORKAROUND(ST*);
#endif


#endif /* EXTRA_WORD_IN_TREE_NODES */

#ifdef KEY
// States for tracking whether we want to make a symbol weak as a workaround
// for emitting all symbols referenced in cleanup code.
#define WEAK_WORKAROUND_unknown		0
#define WEAK_WORKAROUND_dont_make_weak	1
#define WEAK_WORKAROUND_made_weak	2
#endif

/* 
 * either return a previously created TY_IDX associated with a type,
 * or create a new one.
 */
inline TY_IDX
Get_TY (tree type_tree)
{
#ifdef KEY
	// If the type is a nested record and we are generating DSTs, then
	// create the type for the enclosing record first, so that the current
	// record can be specified as a child of the parent in the DST.
	// Bug 4168.
	if (TYPE_CONTEXT(type_tree) &&
	    (TREE_CODE(type_tree) == RECORD_TYPE ||
	     TREE_CODE(type_tree) == UNION_TYPE) &&
	    (TREE_CODE(TYPE_CONTEXT(type_tree)) == RECORD_TYPE ||
	     TREE_CODE(TYPE_CONTEXT(type_tree)) == UNION_TYPE) &&
	    !TYPE_TY_IDX(TYPE_CONTEXT(type_tree))) {
	  Get_TY(TYPE_CONTEXT(type_tree));	// get type for parent record
	}
#endif
	TY_IDX idx = TYPE_TY_IDX(type_tree);
        if (idx > 1) {
	    if (TREE_CODE(type_tree) == RECORD_TYPE ||
	        TREE_CODE(type_tree) == UNION_TYPE) {
	      FLD_HANDLE elt_fld = TY_fld(idx);
	      if (elt_fld.Is_Null() && TYPE_METHODS(type_tree) == NULL) 
		return Create_TY_For_Tree (type_tree, idx); // forward declared
	      else return idx;
	    }
	    else return idx;
        }
	return Create_TY_For_Tree (type_tree, TY_IDX_ZERO);
}

/*
 * either return a previously created ST associated with a
 * var-decl/parm-decl/function_decl, or create a new one.
 */
inline ST *
Get_ST (tree decl_tree)
{
	ST *st = DECL_ST(decl_tree);
        if (st != NULL) {
		if (TREE_CODE(decl_tree) == VAR_DECL &&
		    ST_sclass(st) == SCLASS_EXTERN   &&
		    !DECL_EXTERNAL(decl_tree)        &&
		    !DECL_INITIAL(decl_tree))
			Set_ST_sclass (st, SCLASS_UGLOBAL);
#ifdef KEY // the earlier definition may not have the complete type
		if (TREE_CODE(decl_tree) == VAR_DECL) {
		  TY_IDX ty_idx = Get_TY(TREE_TYPE(decl_tree));
		  if (ty_idx && TY_IDX_index(ty_idx) != TY_IDX_index(st->u2.type)) {
		    // Preserve volatile.
		    if (TY_is_volatile(ST_type(st)))
		      Set_TY_is_volatile(ty_idx);
		    st->u2.type = ty_idx;
		  }
		}
#endif
        }
	else st = Create_ST_For_Tree (decl_tree);
#ifdef KEY
	if (ST_is_thread_private(st) && CURRENT_SYMTAB != GLOBAL_SYMTAB)
	  Set_PU_has_mp(Get_Current_PU());
#endif
	return st;
}

bool is_empty_base_class (tree type_tree);
tree next_real_or_virtual_field (tree type_tree, tree field);
size_t Roundup (size_t offset, int alignment);
size_t Type_Size_Without_Vbases (tree type_tree);

#endif

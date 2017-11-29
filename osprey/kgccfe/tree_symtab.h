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

#include <cmplrs/rcodes.h>

extern TY_IDX Create_TY_For_Tree (tree, TY_IDX idx = TY_IDX_ZERO);
extern "C" ST* Create_ST_For_Tree (tree);
#ifdef KEY
extern "C" void Create_DST_For_Tree (tree, ST*);
extern "C" BOOL Is_shared_mp_var (tree);
#endif // KEY

/* 
 * either return a previously created TY_IDX associated with a type,
 * or create a new one.
 */
inline TY_IDX
Get_TY (tree type_tree)
{
	if (TREE_CODE(type_tree) == ERROR_MARK)
	    exit (RC_USER_ERROR);
	TY_IDX idx = TYPE_TY_IDX(type_tree);
        if (idx != 0) {
	    if (TREE_CODE(type_tree) == RECORD_TYPE ||
	        TREE_CODE(type_tree) == UNION_TYPE) {
	      FLD_HANDLE elt_fld = TY_fld(idx);
	      if (elt_fld.Is_Null()) 
		return Create_TY_For_Tree (type_tree, idx); // forward declared
	      else return idx;
	    }
	    else return idx;
        }
	return Create_TY_For_Tree (type_tree, TY_IDX_ZERO);
}

#ifdef PATHSCALE_MERGE
// bug fix for OSP_133
// return section name for corresponding ST via st_attr table
struct find_st_attr_section_name {
        ST_IDX st;
	find_st_attr_section_name (const ST *s) : st (ST_st_idx (s)) {}

	BOOL operator () (UINT, const ST_ATTR *st_attr) const {
	                return (ST_ATTR_kind (*st_attr) == ST_ATTR_SECTION_NAME &&
				ST_ATTR_st_idx (*st_attr) == st);
	}
};
#endif
/*
 * either return a previously created ST associated with a
 * var-decl/parm-decl/function_decl, or create a new one.
 */
inline ST *
Get_ST (tree decl_tree)
{
	ST *st = DECL_ST(decl_tree);
        if (st != NULL) {
#ifndef KEY
		if (TREE_CODE(decl_tree) == VAR_DECL &&
		    ST_sclass(st) == SCLASS_EXTERN   &&
		    !ST_is_weak_symbol(st)           &&
		    !DECL_EXTERNAL(decl_tree)        &&
		    !DECL_INITIAL(decl_tree))
			Set_ST_sclass (st, SCLASS_UGLOBAL);
#else
		// bug 3117: mimick Create_ST_For_Tree:
		// Mark as COMMON if appropriate, instead of UGLOBAL
		// And change sclass only if it is TREE_STATIC
		if (TREE_CODE(decl_tree) == VAR_DECL &&
		    ST_sclass(st) == SCLASS_EXTERN   &&
		    !ST_is_weak_symbol(st)           &&
		    !DECL_EXTERNAL(decl_tree)        &&
		    !DECL_INITIAL(decl_tree)         &&
		    TREE_STATIC(decl_tree))
		{
#ifdef PATHSCALE_MERGE
			    // bug fix for OSP_133
		    if (flag_no_common || DECL_SECTION_NAME(decl_tree)) {
			/* if st has declared section name, and the section name isn't 
			 * the same as current tree node's section name attribute, store 
			 * the new section name in the str table and assign it to st*/
			if (DECL_SECTION_NAME(decl_tree)) {
			    ST_ATTR_IDX st_attr_idx;
			    ST_IDX idx = ST_st_idx (st);
			    // search for section name attribute in st_attr table
			    st_attr_idx = For_all_until (St_Attr_Table, 
							 ST_IDX_level (idx),
							 find_st_attr_section_name(st));
			    if (st_attr_idx){
			    STR_IDX str_index = ST_ATTR_section_name(St_Attr_Table(ST_IDX_level (idx), st_attr_idx));
			    // only when they have different section names
			    if(strcmp(Index_To_Str(str_index), 
			       TREE_STRING_POINTER (DECL_SECTION_NAME (decl_tree))))
			       Set_ST_ATTR_section_name(St_Attr_Table(ST_IDX_level (idx), st_attr_idx), 
							Save_Str (TREE_STRING_POINTER (DECL_SECTION_NAME (decl_tree))));}
			    // bug fix for OSP_136
			    /* if the old tree node doesn't have section name 
			     * attribute, create one for current tree node */
			    else {
			        DevWarn ("section %s specified for %s", 
				    TREE_STRING_POINTER (DECL_SECTION_NAME (decl_tree)), ST_name (st));
				ST_ATTR&    st_attr = New_ST_ATTR (ST_IDX_level (idx), st_attr_idx);
				ST_ATTR_Init (st_attr, idx, ST_ATTR_SECTION_NAME, 
					Save_Str (TREE_STRING_POINTER (DECL_SECTION_NAME (decl_tree))));
                            }
			}
#endif
			Set_ST_sclass (st, SCLASS_UGLOBAL);
		    }
		    else {
		      if (Debug_Level >= 2) { 
			  // Bug 6183 
			  // If this symbol was declared extern before and 
			  // later instantiated in this file, then we need to 
			  // create a DST entry now.
		          Create_DST_For_Tree(decl_tree, st);
			}
		        Set_ST_sclass (st, SCLASS_COMMON);
		    }
		}
		// the earlier definition may not have the complete type
		if (TREE_CODE(decl_tree) == VAR_DECL) {
		  TY_IDX ty_idx = Get_TY(TREE_TYPE(decl_tree));
		  if (ty_idx && 
                      (TY_IDX_index(ty_idx) != TY_IDX_index(ST_type(st))) ||
		      // Update alignment.  Bug 13186.
		      (TY_align(ty_idx) != TY_align(ST_type(st)))) {
		    Set_ST_type(st, ty_idx);
		  }
		}
#endif
        }
	else
		st = Create_ST_For_Tree (decl_tree);
	if ((CURRENT_SYMTAB > GLOBAL_SYMTAB + 1) &&
	    ((TREE_CODE(decl_tree) == VAR_DECL) ||
	     (TREE_CODE(decl_tree) == PARM_DECL)) &&
	    (ST_level(st) < CURRENT_SYMTAB) &&
	    (ST_level(st) > GLOBAL_SYMTAB)) {
		Set_ST_has_nested_ref (st);
		ST *base_st = st;
		while (base_st != ST_base (base_st)) {
			base_st = ST_base (base_st);
			Set_ST_has_nested_ref (base_st);
		}
	}
#ifdef KEY
	if (ST_is_thread_private(st) && CURRENT_SYMTAB != GLOBAL_SYMTAB)
		Set_PU_has_mp(Get_Current_PU());
#endif
	Is_True(ST_st_idx(st) != 0, ("st_idx is 0?"));
	Is_True(ST_st_idx(st) != -1, ("st_idx is -1?"));
	Is_True(ST_IDX_level(ST_st_idx(st)) != 0, ("st_level is 0?"));
	Is_True(ST_IDX_index(ST_st_idx(st)) != 0, ("st_index is 0?"));
	return st;
}

#endif

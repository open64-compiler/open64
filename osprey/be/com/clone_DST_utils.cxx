/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#include "defs.h"

#include "symtab.h"               // SYMTAB
#include "strtab.h"		// MEM_strtab_pool_ptr
#include "cxx_memory.h"		// CXX_NEW
#include "dwarf_DST.h"		// DST_INFO
#include "dwarf_DST_producer.h"	// DST_mk_ functions
#include "dwarf_DST_mem.h"	// DST_IDX, DST_TYPE    
#include "clone.h"              // IPO_SYMTAB
#include "clone_DST_utils.h"    // DST_enter_inline_subroutine


INT  num_gen_names = 1;

#ifndef _LEGO_CLONER
extern MEM_POOL Ipo_mem_pool;
#endif

const DST_INFO_IDX foreign_origin = DST_FOREIGN_INIT;


#define GET_ABSTRACT_ORIGIN(x) ((caller_file_dst == callee_file_dst)? get_abstract_origin(x): foreign_origin)


#if !defined(_STANDALONE_INLINER)
static char *
create_ipa_internal_name(void)
{
    char *name;

#if defined(__GNUC__)    
    // GNU C/C++ uses a real preprocessor, so we can't put #if inside
    // macro arguments.
#if defined(_LEGO_CLONER)
    name = (char *) CXX_NEW_ARRAY(char, strlen("***ipa_intnl***")+20,
			    MEM_src_pool_ptr);
#else
    name = (char *) CXX_NEW_ARRAY(char, strlen("***ipa_intnl***")+20,
			    &Ipo_mem_pool);
#endif // _LEGO_CLONER
#else
    name = (char *)CXX_NEW_ARRAY(char, strlen("***ipa_intnl***")+20, 
#ifndef _LEGO_CLONER
                           &Ipo_mem_pool);
#else
                           MEM_src_pool_ptr);
#endif
#endif // __GNUC__

    if (name) 
        sprintf(name, "%s_%d", "***ipa_intnl***", num_gen_names++);
    return (name);
}
#endif //  !defined(_STANDALONE_INLINER)


DST_IDX
get_abstract_origin(DST_IDX concrete_instance)
{
    DST_INFO *dst = DST_INFO_IDX_TO_PTR(concrete_instance);

    if (DST_IS_FOREIGN_OBJ(concrete_instance))
	return concrete_instance;

    switch (DST_INFO_tag(dst)) {
    case DW_TAG_inlined_subroutine:
        {
            DST_INLINED_SUBROUTINE *attr =
                DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(dst),
                                    DST_INLINED_SUBROUTINE);
            if (DST_IS_NULL(DST_INLINED_SUBROUTINE_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_INLINED_SUBROUTINE_abstract_origin(attr));
        }

    case DW_TAG_label:
        {
            DST_LABEL *attr =  DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(dst), DST_LABEL);
            if (DST_IS_NULL(DST_LABEL_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_LABEL_abstract_origin(attr));
        }

    case DW_TAG_variable:
        {
            DST_VARIABLE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(dst), DST_VARIABLE);
            if (DST_IS_NULL(DST_VARIABLE_def_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_VARIABLE_def_abstract_origin(attr));
        }

    case DW_TAG_formal_parameter:
        {
            DST_FORMAL_PARAMETER *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (dst), DST_FORMAL_PARAMETER);
            if (DST_IS_NULL(DST_FORMAL_PARAMETER_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_FORMAL_PARAMETER_abstract_origin(attr));
        }

    case DW_TAG_lexical_block:
        {
            DST_LEXICAL_BLOCK *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (dst), DST_LEXICAL_BLOCK);
            if (DST_IS_NULL(DST_LEXICAL_BLOCK_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_LEXICAL_BLOCK_abstract_origin(attr));
        }

    case DW_TAG_unspecified_parameters:
        {
            DST_UNSPECIFIED_PARAMETERS *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (dst), DST_UNSPECIFIED_PARAMETERS);
            if (DST_IS_NULL(DST_UNSPECIFIED_PARAMETERS_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_UNSPECIFIED_PARAMETERS_abstract_origin(attr));
        }

    case DW_TAG_typedef:
        {
            DST_TYPEDEF *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (dst), DST_TYPEDEF);
            if (DST_IS_NULL(DST_TYPEDEF_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_TYPEDEF_abstract_origin(attr));
        }

    case DW_TAG_array_type:
        {
            DST_ARRAY_TYPE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (dst), DST_ARRAY_TYPE);
            if (DST_IS_NULL(DST_ARRAY_TYPE_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_ARRAY_TYPE_abstract_origin(attr));
        }

    case DW_TAG_structure_type:
        {
            DST_STRUCTURE_TYPE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (dst), DST_STRUCTURE_TYPE);
            if (DST_IS_NULL(DST_STRUCTURE_TYPE_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_STRUCTURE_TYPE_abstract_origin(attr));
        }

    case DW_TAG_union_type:
        {
            DST_UNION_TYPE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (dst), DST_UNION_TYPE);
            if (DST_IS_NULL(DST_UNION_TYPE_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_UNION_TYPE_abstract_origin(attr));
        }

    case DW_TAG_class_type:
        {
            DST_CLASS_TYPE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (dst), DST_CLASS_TYPE);
            if (DST_IS_NULL(DST_CLASS_TYPE_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_CLASS_TYPE_abstract_origin(attr));
        }

    case DW_TAG_enumeration_type:
        {
            DST_ENUMERATION_TYPE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (dst), DST_ENUMERATION_TYPE);
            if (DST_IS_NULL(DST_ENUMERATION_TYPE_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_ENUMERATION_TYPE_abstract_origin(attr));
        }

    case DW_TAG_subroutine_type:
        {
            DST_SUBROUTINE_TYPE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (dst), DST_SUBROUTINE_TYPE);
            if (DST_IS_NULL(DST_SUBROUTINE_TYPE_abstract_origin(attr)))
                return concrete_instance;
            else
                return get_abstract_origin(DST_SUBROUTINE_TYPE_abstract_origin(attr));
        }

    }

    return concrete_instance;
}

typedef std::pair<DST_TYPE, DST_IDX> DST_PAIR;

DST_PAIR
get_abstract_origin_and_dst_for_subroutine(DST_IDX concrete_instance,
                                           DST_TYPE file_dst)
{
    DST_TYPE abstract_dst;
    DST_IDX  idx;
    DST_PAIR pair;
    DST_TYPE old_dst = Current_DST;
    Current_DST = file_dst;

    DST_INFO *dst = DST_INFO_IDX_TO_PTR(concrete_instance);

    if (DST_IS_FOREIGN_OBJ(concrete_instance))
        return std::make_pair(file_dst, concrete_instance);

    DST_INLINED_SUBROUTINE *attr;

    switch (DST_INFO_tag(dst)) {
        case DW_TAG_subprogram: 
        {
            pair = std::make_pair(file_dst, concrete_instance);
            break;
        }
        case DW_TAG_inlined_subroutine:
        {
            attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(dst), DST_INLINED_SUBROUTINE);
            if (DST_IS_NULL(DST_INLINED_SUBROUTINE_abstract_origin(attr))) {
                pair = std::make_pair(file_dst, concrete_instance);
            }
            else {
                pair = get_abstract_origin_and_dst_for_subroutine(
                        DST_INLINED_SUBROUTINE_abstract_origin(attr), 
                        DST_INLINED_SUBROUTINE_abstract_dst(attr));
            }
            break;
        }
        default:
        {
            Fail_FmtAssertion("get_abstract_origin_and_dst_for_subroutine: Unexpected dst info tag");
            break;
        }
    }
    Current_DST = old_dst;
    return pair;
}

static void
DST_enter_cloned_childs(DST_IDX parent, 
			DST_IDX inl_routine, 
			IPO_SYMTAB *symtab, 
			DST_TYPE caller_file_dst, 
			DST_TYPE callee_file_dst, 
			mUINT16 parent_file_index, 
			BOOL inlined,
			MEM_POOL *caller_file_m,
			MEM_POOL *callee_file_m)
{
    DST_INFO_IDX child_idx;
    USRCPOS decl;

    USRCPOS_clear(decl);
    /* set Current_Symtab to callee's symtab so that St_Table would
     * work properly
     */
    Scope_tab = symtab->Get_orig_scope_tab();
    CURRENT_SYMTAB = symtab->Get_orig_level();

    /* go down all the child of the current DST and 
     * generate appropiate entries for that in the concrete instance
     */
#if (!defined(_LEGO_CLONER))
    	    Current_DST = callee_file_dst;
#endif
    for (child_idx = DST_first_child(inl_routine); !DST_IS_NULL(child_idx);
                child_idx = DST_INFO_sibling(DST_INFO_IDX_TO_PTR(child_idx))) {
        DST_INFO_IDX idx;

	switch (DST_INFO_tag(DST_INFO_IDX_TO_PTR(child_idx))) {
        case DW_TAG_formal_parameter: 
	    {
            /* take care of formal parameters here */
            ST *param_st;
            DST_ASSOC_INFO parm;

#if (!defined(_LEGO_CLONER))
    	    Current_DST = callee_file_dst;
#endif

            DST_FORMAL_PARAMETER *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (DST_INFO_IDX_TO_PTR(child_idx)), DST_FORMAL_PARAMETER);

            param_st = symtab->Get_Cloned_ST(&St_Table[
		DST_ASSOC_INFO_st_idx(DST_FORMAL_PARAMETER_st (attr))]);

	    if (param_st) {
		char *name = NULL;
		DST_ASSOC_INFO_st_idx(parm) = ST_st_idx(param_st);
		decl = DST_FORMAL_PARAMETER_decl(attr);

		name = DST_STR_IDX_TO_PTR(DST_FORMAL_PARAMETER_name(attr));
#if (!defined(_LEGO_CLONER))
    	        Current_DST = caller_file_dst;
#endif
		if (parent_file_index)
		    USRCPOS_filenum(decl) = parent_file_index;

                idx = DST_mk_formal_parameter(decl,
					  name,
                                          DST_INVALID_IDX,
                                          ST_st_idx(param_st),
                                          GET_ABSTRACT_ORIGIN(child_idx),     /* abstract_origin */
                                          DST_INVALID_IDX,
                                          FALSE,
                                          FALSE,
                                          FALSE, // is_artificial
					  FALSE); // is_declaration_only
                DST_append_child(parent, idx);
	    }
	    break;
	    }
        case DW_TAG_variable:
	    {
	    if (!(DST_IS_declaration(DST_INFO_flag(DST_INFO_IDX_TO_PTR(child_idx)))) &&
                DST_IS_automatic(DST_INFO_flag(DST_INFO_IDX_TO_PTR(child_idx)))) {
                /* Take care of local variables here */

                ST *local_st;
                DST_ASSOC_INFO var;

#if (!defined(_LEGO_CLONER))
    	        Current_DST = callee_file_dst;
#endif

                DST_VARIABLE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(DST_INFO_IDX_TO_PTR(child_idx)), DST_VARIABLE);

                local_st = symtab->Get_Cloned_ST(&St_Table[
			DST_ASSOC_INFO_st_idx(DST_VARIABLE_def_st (attr))]);
		if (local_st) {
		    char *name = NULL;

		    DST_ASSOC_INFO_st_idx(var) = ST_st_idx(local_st);

		    decl = DST_VARIABLE_constant_decl(attr);

		    name = DST_STR_IDX_TO_PTR(DST_VARIABLE_def_name(attr));
#if (!defined(_LEGO_CLONER))
    	            Current_DST = caller_file_dst;
#endif
		    if (parent_file_index)
		        USRCPOS_filenum(decl) = parent_file_index;

                    idx = DST_mk_variable(decl,
				name,
                                DST_INVALID_IDX,
                                0,
				ST_st_idx(local_st),
                                GET_ABSTRACT_ORIGIN(child_idx),  	/* abstract_origin */
                                FALSE,
                                FALSE,
                                FALSE,
                                FALSE);
                    DST_append_child(parent, idx);
		}
            }
	    break;
	    }
        case DW_TAG_label:
	    {
            ST_IDX lbl;
	    char *label_name = NULL;

#if (!defined(_LEGO_CLONER))
    	    Current_DST = callee_file_dst;
#endif

            DST_LABEL *attr =  DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(DST_INFO_IDX_TO_PTR(child_idx)), DST_LABEL);

	    lbl = make_ST_IDX(
		DST_ASSOC_INFO_st_index(DST_LABEL_low_pc (attr)) 
			+ symtab->Get_cloned_label_last_idx(),
		symtab->Get_cloned_level() );

#if (!defined(_STANDALONE_INLINER) && !defined(_LEGO_CLONER))

	    if (DST_IS_NULL(DST_LABEL_name(attr))) {
	        label_name = create_ipa_internal_name();
	        if (label_name) {
		    /* we need to fix up the callee's label name also */
		    MEM_POOL *old_src_pool = MEM_src_pool_ptr;
		    if (inlined && (caller_file_m != callee_file_m)) {
			MEM_src_pool_ptr = callee_file_m;
		    }
		    DST_label_add_name(attr, label_name);
		    MEM_src_pool_ptr = old_src_pool;
		}
	    }
	    else 
		 label_name = DST_STR_IDX_TO_PTR(DST_LABEL_name(attr));
    	    Current_DST = caller_file_dst;
#endif

            idx = DST_mk_label(label_name, lbl, 
					GET_ABSTRACT_ORIGIN(child_idx));

            DST_append_child(parent, idx);
	    break;
	    }
        /* take care of nested inline subroutines */
        case DW_TAG_inlined_subroutine:
	    {
            /* what we are looking here is a concrete instance already */
#if (!defined(_LEGO_CLONER))
    	    Current_DST = callee_file_dst;
#endif

            DST_INLINED_SUBROUTINE *attr =  DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(DST_INFO_IDX_TO_PTR(child_idx)), DST_INLINED_SUBROUTINE);

            LABEL_IDX b_lbl = DST_ASSOC_INFO_st_index(DST_INLINED_SUBROUTINE_low_pc (attr)) +  symtab->Get_cloned_label_last_idx();
            LABEL_IDX e_lbl = DST_ASSOC_INFO_st_index(DST_INLINED_SUBROUTINE_high_pc (attr)) +  symtab->Get_cloned_label_last_idx();

            (void)DST_enter_inlined_subroutine(parent,
			     child_idx,
                             b_lbl,
                             e_lbl,		
			     caller_file_dst,
			     callee_file_dst,
			     symtab,
			     caller_file_m,
			     callee_file_m,
			     parent_file_index);
	    break;
	    }
	case DW_TAG_lexical_block:
	    {
	    ST_IDX low_pc;
	    ST_IDX high_pc;

#if (!defined(_LEGO_CLONER))
            Current_DST = callee_file_dst;
#endif

	    DST_LEXICAL_BLOCK *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(DST_INFO_IDX_TO_PTR(child_idx)), DST_LEXICAL_BLOCK);

	    low_pc = make_ST_IDX(
		DST_ASSOC_INFO_st_index(DST_LEXICAL_BLOCK_low_pc (attr)) 
			+ symtab->Get_cloned_label_last_idx(),
		symtab->Get_cloned_level() );

	    high_pc = make_ST_IDX(
		DST_ASSOC_INFO_st_index(DST_LEXICAL_BLOCK_high_pc (attr)) 
			+ symtab->Get_cloned_label_last_idx(),
		symtab->Get_cloned_level() );

	    char *block_name = NULL;

#if ( !defined(_STANDALONE_INLINER) && !defined(_LEGO_CLONER))

	    if (DST_IS_NULL(DST_LEXICAL_BLOCK_name(attr))) {
		block_name = create_ipa_internal_name();
		if (block_name) {
		    /* we need to fix up the callee's lexical_block name also */
		    MEM_POOL *old_src_pool = MEM_src_pool_ptr;
		    if (inlined && (caller_file_m != callee_file_m)) {
			MEM_src_pool_ptr = callee_file_m;
		    }
		    DST_lexical_block_add_name(attr, block_name);
		    MEM_src_pool_ptr = old_src_pool;
		}
	    }
	    else 
	        block_name = DST_STR_IDX_TO_PTR(DST_LEXICAL_BLOCK_name(attr));
            Current_DST = caller_file_dst;
#endif

	    idx = DST_mk_lexical_block(block_name,
				       low_pc,
				       high_pc,
                             GET_ABSTRACT_ORIGIN(child_idx));

	    DST_append_child(parent, idx);

	    (void)DST_enter_cloned_childs(idx, child_idx, 
			symtab, caller_file_dst, callee_file_dst, 
			parent_file_index, inlined, caller_file_m, callee_file_m);

	   break;
	   }

        case DW_TAG_unspecified_parameters:
            {
	    if (inlined) 
		break;
	    else {
#if (!defined(_LEGO_CLONER))
    	    Current_DST = callee_file_dst;
#endif
                DST_UNSPECIFIED_PARAMETERS *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (DST_INFO_IDX_TO_PTR(child_idx)), DST_UNSPECIFIED_PARAMETERS);

#if (!defined(_LEGO_CLONER))
    	        Current_DST = caller_file_dst;
#endif

                idx = DST_mk_unspecified_parameters(
		    DST_UNSPECIFIED_PARAMETERS_decl(attr),
                    GET_ABSTRACT_ORIGIN(child_idx));     /* abstract_origin */

                DST_append_child(parent, idx);
	        break;
            }
	    }
    
        case DW_TAG_typedef:
            {
	    if (inlined) 
		break;
	    else {
#if (!defined(_LEGO_CLONER))
    	        Current_DST = callee_file_dst;
#endif

                DST_TYPEDEF *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (DST_INFO_IDX_TO_PTR(child_idx)), DST_TYPEDEF);

#if (!defined(_LEGO_CLONER))
    	        Current_DST = caller_file_dst;
#endif

                idx = DST_mk_typedef(
		    DST_TYPEDEF_decl(attr),
		    NULL,
		    DST_TYPEDEF_type(attr),
                    GET_ABSTRACT_ORIGIN(child_idx));     /* abstract_origin */

                DST_append_child(parent, idx);
	        break;
            }
	    }
    
        case DW_TAG_array_type:
            {
	    if (inlined) 
		break;
	    else {
#if (!defined(_LEGO_CLONER))
    	        Current_DST = callee_file_dst;
#endif

                DST_ARRAY_TYPE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (DST_INFO_IDX_TO_PTR(child_idx)), DST_ARRAY_TYPE);

#if (!defined(_LEGO_CLONER))
    	        Current_DST = caller_file_dst;
#endif

                idx = DST_mk_array_type(
		    DST_ARRAY_TYPE_decl(attr),
		    NULL,
		    DST_ARRAY_TYPE_type(attr),
		    DST_ARRAY_TYPE_byte_size(attr),
                    GET_ABSTRACT_ORIGIN(child_idx),     /* abstract_origin */
		    FALSE);

                DST_append_child(parent, idx);
	        break;
            }
	    }
    
        case DW_TAG_structure_type:
            {
	    if (inlined) 
		break;
	    else {
#if (!defined(_LEGO_CLONER))
    	        Current_DST = callee_file_dst;
#endif

                DST_STRUCTURE_TYPE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (DST_INFO_IDX_TO_PTR(child_idx)), DST_STRUCTURE_TYPE);

#if (!defined(_STANDALONE_INLINER) && !defined(_LEGO_CLONER))
    	        Current_DST = caller_file_dst;
#endif

                idx = DST_mk_structure_type(
		    DST_STRUCTURE_TYPE_decl(attr),
		    NULL,
		    DST_STRUCTURE_TYPE_byte_size(attr),
                    GET_ABSTRACT_ORIGIN(child_idx),     /* abstract_origin */
		    FALSE);

                DST_append_child(parent, idx);
	        break;
            }
	    }
    
        case DW_TAG_union_type:
            {
	    if (inlined) 
		break;
	    else {
#if (!defined(_STANDALONE_INLINER) && !defined(_LEGO_CLONER))
    	        Current_DST = callee_file_dst;
#endif

                DST_UNION_TYPE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (DST_INFO_IDX_TO_PTR(child_idx)), DST_UNION_TYPE);

#if (!defined(_STANDALONE_INLINER) && !defined(_LEGO_CLONER))
    	        Current_DST = caller_file_dst;
#endif

                idx = DST_mk_union_type(
		    DST_UNION_TYPE_decl(attr),
		    NULL,
		    DST_UNION_TYPE_byte_size(attr),
                    GET_ABSTRACT_ORIGIN(child_idx),     /* abstract_origin */
		    FALSE);

                DST_append_child(parent, idx);
            }
	    }
    
        case DW_TAG_class_type:
            {
	    if (inlined) 
		break;
	    else {
#if (!defined(_STANDALONE_INLINER) && !defined(_LEGO_CLONER))
    	        Current_DST = callee_file_dst;
#endif

                DST_CLASS_TYPE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (DST_INFO_IDX_TO_PTR(child_idx)), DST_CLASS_TYPE);

#if (!defined(_STANDALONE_INLINER) && !defined(_LEGO_CLONER))
    	        Current_DST = caller_file_dst;
#endif

                idx = DST_mk_class_type(
		    DST_CLASS_TYPE_decl(attr),
		    NULL,
		    DST_CLASS_TYPE_byte_size(attr),
                    GET_ABSTRACT_ORIGIN(child_idx),     /* abstract_origin */
		    FALSE);

                DST_append_child(parent, idx);
            }
	    }
    
        case DW_TAG_enumeration_type:
            {
	    if (inlined) 
		break;
	    else {
#if (!defined(_STANDALONE_INLINER) && !defined(_LEGO_CLONER))
    	        Current_DST = callee_file_dst;
#endif

                DST_ENUMERATION_TYPE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (DST_INFO_IDX_TO_PTR(child_idx)), DST_ENUMERATION_TYPE);

#if (!defined(_STANDALONE_INLINER) && !defined(_LEGO_CLONER))
    	        Current_DST = caller_file_dst;
#endif

                idx = DST_mk_enumeration_type(
		    DST_ENUMERATION_TYPE_decl(attr),
		    NULL,
		    DST_ENUMERATION_TYPE_byte_size(attr),
                    GET_ABSTRACT_ORIGIN(child_idx),     /* abstract_origin */
		    FALSE);

                DST_append_child(parent, idx);
            }
	    }
    
        case DW_TAG_subroutine_type:
            {
	    if (inlined) 
	  	break;
	    else {
#if (!defined(_STANDALONE_INLINER) && !defined(_LEGO_CLONER))
    	        Current_DST = callee_file_dst;
#endif

                DST_SUBROUTINE_TYPE *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes (DST_INFO_IDX_TO_PTR(child_idx)), DST_SUBROUTINE_TYPE);

#if (!defined(_STANDALONE_INLINER) && !defined(_LEGO_CLONER))
    	        Current_DST = caller_file_dst;
#endif

                idx = DST_mk_subroutine_type(
		    DST_SUBROUTINE_TYPE_decl(attr),
		    NULL,
		    DST_SUBROUTINE_TYPE_type(attr),
                    GET_ABSTRACT_ORIGIN(child_idx),     /* abstract_origin */
		    FALSE);

                DST_append_child(parent, idx);
		break;
            }
	    }
        }
#if (!defined(_STANDALONE_INLINER) && !defined(_LEGO_CLONER))
    	Current_DST = callee_file_dst;
#endif
    }
}



#if (!defined(_LEGO_CLONER))
mUINT16
DST_Enter_Callee_File_Dst(DST_TYPE caller_file_dst,
                          DST_TYPE callee_file_dst)
{
    DST_TYPE old_Current_DST = Current_DST;
    char *filename = 0;
    char *dirname = 0;
    UINT64   file_size;
    UINT64   fmod_time;
    Current_DST = callee_file_dst;

    DST_FILE_IDX callee_idx = DST_get_file_names();
    DST_FILE_NAME *file;
    mUINT16 index = 1;

    while (!DST_IS_NULL(callee_idx)) {
        Current_DST = callee_file_dst;
        file = DST_FILE_IDX_TO_PTR(callee_idx);
        file_size = DST_FILE_NAME_size(file);
        fmod_time = DST_FILE_NAME_modt(file);

        dirname = DST_get_dirname(DST_FILE_NAME_dir(file));
        filename =  DST_STR_IDX_TO_PTR(DST_FILE_NAME_name(file));

        if (filename) {
            Current_DST = caller_file_dst;
            /* Get file id in caller file dst; if no, create a new entry */
            DST_get_cross_inlined_file_id (filename,
                                           dirname,
                                           file_size,
                                           fmod_time); 
        }

        Current_DST = callee_file_dst;
        callee_idx = DST_FILE_NAME_next(file);
        index++;
    }
    Current_DST = old_Current_DST;
}

mUINT16
DST_get_cross_file_id(DST_IDX parent,
		       DST_IDX inl_routine, 
		       DST_TYPE caller_file_dst,
		       DST_TYPE callee_file_dst)
{
    DST_TYPE old_Current_DST = Current_DST;
    char *filename = 0;
    char *dirname = 0;
    UINT64   file_size;
    UINT64   fmod_time;
    mUINT16  ret_val = 0;
	    
    Current_DST = caller_file_dst;

    DST_SUBPROGRAM *attr =
	DST_ATTR_IDX_TO_PTR (DST_INFO_attributes (DST_INFO_IDX_TO_PTR(parent)),
			     DST_SUBPROGRAM); 

    Current_DST = callee_file_dst;
    DST_INFO_IDX abstract_origin = get_abstract_origin(inl_routine);
    
    DST_SUBPROGRAM *inl_attr = DST_ATTR_IDX_TO_PTR (DST_INFO_attributes (DST_INFO_IDX_TO_PTR (abstract_origin)), 
						    DST_SUBPROGRAM);

    filename = DST_get_file (USRCPOS_filenum (DST_SUBPROGRAM_decl_decl (inl_attr)), 
			     &file_size, &fmod_time, &dirname); 
	    
    if (filename) {

        Current_DST = caller_file_dst;
	
	/* find the file with SRCPOS info */
        ret_val = DST_get_cross_inlined_file_id (filename, dirname,
						 file_size, fmod_time); 
    }
    Current_DST = old_Current_DST;

    return (ret_val);
}
#endif // !defined(_LEGO_CLONER)


		       
void
DST_enter_inlined_subroutine(DST_IDX parent,
                             DST_IDX inl_routine,
                             LABEL_IDX begin_label,
                             LABEL_IDX end_label,
			     DST_TYPE caller_file_dst,
			     DST_TYPE callee_file_dst,
			     IPO_SYMTAB *symtab, 
			     MEM_POOL *caller_file_m,
			     MEM_POOL *callee_file_m,
			     mUINT16 cross_file_id)
{
    DST_INFO_IDX idx;
    SCOPE *old_scope = symtab->Get_orig_scope_tab();
    SYMTAB_IDX old_callee_level = symtab->Get_orig_level();
    ST_IDX low_pc, high_pc;

    Scope_tab = symtab->Get_orig_scope_tab();
    CURRENT_SYMTAB = symtab->Get_orig_level();

#if !defined(_LEGO_CLONER)
    DST_TYPE old_Current_DST = Current_DST;
    Current_DST = callee_file_dst;
#endif
    DST_PAIR pair = get_abstract_origin_and_dst_for_subroutine(inl_routine,
                                                               callee_file_dst);
    DST_TYPE abstract_file_dst = pair.first;
    DST_INFO_IDX abstract_origin = pair.second;
    mUINT16 file_index = cross_file_id;

#if !defined(_LEGO_CLONER)
    Current_DST = caller_file_dst;
#endif


    low_pc = make_ST_IDX(
	begin_label,
	symtab->Get_cloned_level() );

    high_pc = make_ST_IDX(
	end_label,
	symtab->Get_cloned_level() );

    if (caller_file_dst == callee_file_dst) {
        idx = DST_mk_inlined_subroutine (low_pc, high_pc,
					     abstract_origin, abstract_file_dst); 

        DST_enter_cloned_childs (idx, inl_routine, symtab,
				     caller_file_dst, callee_file_dst,
				     file_index, TRUE, caller_file_m,
				     callee_file_m); 

        DST_append_child(parent, idx);
    }
#if (!defined(_LEGO_CLONER))
    else {
        DST_SUBPROGRAM *attr;

	attr = DST_ATTR_IDX_TO_PTR (DST_INFO_attributes (DST_INFO_IDX_TO_PTR(parent)),
					DST_SUBPROGRAM); 
	char *filename;
	char *routine_name;
	char *dirname;
	UINT64   file_size;
	UINT64   fmod_time;
	    
    	Current_DST = abstract_file_dst;
	    
        DST_SUBPROGRAM *inl_attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(DST_INFO_IDX_TO_PTR(abstract_origin)),
							   DST_SUBPROGRAM); 

	filename = DST_get_file (USRCPOS_filenum (DST_SUBPROGRAM_decl_decl(inl_attr)),
				     &file_size, &fmod_time, &dirname); 
	    
  	routine_name = DST_STR_IDX_TO_PTR(DST_SUBPROGRAM_decl_name(inl_attr));
    	    Current_DST = caller_file_dst;
	
	if (filename != NULL) {

	    idx = DST_mk_cross_inlined_subroutine(low_pc, 
						      high_pc, 
						      routine_name,
						      &file_index,
						      file_size,
						      fmod_time,
						      DST_SUBPROGRAM_decl_decl(attr),
						      filename,
						      dirname, 
						      abstract_origin,
						      abstract_file_dst);

            DST_append_child(parent, idx);

            DST_enter_cloned_childs(idx, inl_routine, symtab,
					caller_file_dst, callee_file_dst,
					file_index, TRUE, caller_file_m,
					callee_file_m); 
	}
    }
#endif // !defined(_LEGO_CLONER)


    CURRENT_SYMTAB = old_callee_level;
    Scope_tab = old_scope;

#if (!defined(_LEGO_CLONER))
    Current_DST = old_Current_DST;
#endif
} // DST_enter_inlined_subroutine



DST_IDX
DST_enter_cloned_subroutine(DST_IDX parent, 
                            DST_IDX orig_node, 
                            ST *cloned_st, 
                            DST_TYPE cur_file_dst, 
                            IPO_SYMTAB *sym)
{

  DST_ASSOC_INFO subst;
  DST_INFO_IDX idx = DST_INVALID_INIT;
  DST_SUBPROGRAM *attr = 
    DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(DST_INFO_IDX_TO_PTR(orig_node)), DST_SUBPROGRAM);

  Scope_tab = sym->Get_orig_scope_tab();
  CURRENT_SYMTAB = sym->Get_orig_level();

  if (cloned_st) {

    DST_ASSOC_INFO_st_idx(subst) = ST_st_idx(cloned_st);

    idx = DST_mk_cloned_subprogram(DST_SUBPROGRAM_def_decl(attr),
                                   ST_name(cloned_st),
                                   DST_SUBPROGRAM_def_type(attr),
                                   orig_node,
                                   DST_ASSOC_INFO_st_idx(subst),
                                   DST_SUBPROGRAM_def_inline(attr),
                                   DST_SUBPROGRAM_def_virtuality(attr));

    DST_append_child(parent, idx);

    DST_enter_cloned_childs(idx, orig_node, sym, cur_file_dst, 
                            cur_file_dst, 0, FALSE, NULL, NULL);
  }

  return idx;
}



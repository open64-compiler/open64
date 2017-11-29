/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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


//-*-c++-*-
/* ====================================================================
 * ====================================================================
 *
 * Module: clone.cxx
 * $Revision:
 * $Date: 
 * $Author:
 * $Source:
 *
 * Description:
 *	clone a procedure and its local symbol table
 *
 * ====================================================================
 * ====================================================================
 */

#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>		  
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>		  
#endif /* defined(BUILD_OS_DARWIN) */

// ======================================================================
#include "defs.h"                       // pre-defined types
#include "strtab.h"		        // Save_Str()
#include "irbdata.h"	                // INITO
#include "cxx_memory.h"		        // CXX_NEW ()
#include "wn.h"			        // WN
#ifndef _LEGO_CLONER
#include "config_ipa.h"                 // IPA_Enable_Merge_ty 
#include "ipo_parent.h"                 // for WN_Parentize
#endif

#include "clone.h"                      // IPO_CLONE, IPO_SYMTAB, 
#include "targ_sim.h"
#include "constraint_graph.h"

extern BOOL Alias_Nystrom_Analyzer;

// ======================================================================
// For easy switching of Scope_tab
// ======================================================================
class SCOPE_CONTEXT
{
private:
    SCOPE* _scope_tab;

public:
    SCOPE_CONTEXT (SCOPE *scope) {
	_scope_tab = Scope_tab;
	Scope_tab = scope;
    }

    ~SCOPE_CONTEXT () {
	Scope_tab = _scope_tab;
    }
}; // SCOPE_CONTEXT


// ======================================================================

// ======================================================================
// 
// IPO_CLONE Member Functions
//
// ======================================================================

INT IPO_CLONE::_label = 0;	    // initialize static class member


// ======================================================================
// create a new global ST entry for this entry point. Note, this
// is to be used by the distribution cloner, since it doesn't need
// to set the symbol to hidden. It could reside in a DSO and be
// called from a main program
// ======================================================================

void
IPO_CLONE::Set_Entry_Point(WN *wn, WN *cloned_wn, ST *cloned_st)
{
  Is_True(wn && cloned_wn && cloned_st && _sym,
          ("IPO_CLONE::Set_Entry_Point parameter is null"));
  Is_True(WN_opcode(wn) == OPC_FUNC_ENTRY || 
          WN_opcode(wn) == OPC_ALTENTRY,
          ("Set_Entry_Point can only be invoked on FUNCTIONs\n"));

  WN_st_idx(cloned_wn) = ST_st_idx(cloned_st);

#ifndef _LEGO_CLONER
  // reset this bit since all the unknown edges which may be
  // due to addr taken and saved will point to the original procedure
  Set_ST_addr_not_saved(cloned_st);

  Set_ST_export (cloned_st, EXPORT_INTERNAL);
#endif

#if _THIS_SEEMS_TO_BE_DOING_NOTHING_
  ST *st = WN_st(wn);
  _sym->Set_Cloned_ST(st, cloned_st);
  _sym->Hide_Cloned_ST(st);     // so that recursive calls go to the orig.
#endif
} // IPO_CLONE::Set_Entry_Point




// ======================================================================
// Fix up the ST pointer of the given node.  Clone if necessary.
// ======================================================================

void
IPO_CLONE::Fix_ST (WN* cloned_wn, WN* wn)
{
  ST *st = NULL;
  ST *cloned_st;
  
  FmtAssert(wn && cloned_wn && _sym,("IPO_CLONE::Fix_ST parameter is null"));

  OPCODE op = WN_opcode(wn);
  if (OPCODE_has_sym(op))
    st = WN_st(wn);

  if (st == NULL) {
    // label opcodes don't always have a valid st
    INT lab_level, lab_index;
    if (OPCODE_has_label(op)) {
      lab_level = _sym->Get_cloned_level();
      lab_index = LABEL_IDX_index(WN_label_number(cloned_wn));
      lab_index += _sym->Get_cloned_label_last_idx();
      WN_label_number(cloned_wn) = make_LABEL_IDX(lab_index, lab_level);
    }
    if (OPCODE_has_last_label(op)) {
      lab_level = _sym->Get_cloned_level();
      lab_index = LABEL_IDX_index(WN_last_label(cloned_wn));
      lab_index += _sym->Get_cloned_label_last_idx();
      WN_last_label(cloned_wn) = make_LABEL_IDX(lab_index, lab_level);
    }
    return;
  }

  if (WN_operator(wn) == OPR_PRAGMA &&
      WN_pragma(wn) == WN_PRAGMA_ASM_CONSTRAINT) {
    cloned_st = _sym->Get_Cloned_ST (&St_Table[(ST_IDX) WN_pragma_arg1(wn)]); 
    if ((cloned_st != NULL) && (ST_level(cloned_st) == GLOBAL_SYMTAB)) {
      WN_pragma_arg1(wn) = (INT32) ST_st_idx(cloned_st);
    }
    else {
      cloned_st = _sym->Get_ST(&St_Table[(ST_IDX) WN_pragma_arg1(wn)]);
    }
    WN_pragma_arg1(cloned_wn) = (INT32) ST_st_idx(cloned_st);
  }

  if (ST_class(st) == CLASS_PREG) {
    // the preg number is in the WHIRL node, not in the ST
    WN_OFFSET offst;

    if (WN_operator(wn) == OPR_PRAGMA) {
      if (WN_pragma(wn) != WN_PRAGMA_ASM_CONSTRAINT) {
	offst = WN_pragma_arg1(cloned_wn);
	if (!Preg_Is_Dedicated(offst))
	  WN_pragma_arg1(cloned_wn) += _sym->Get_cloned_preg_last_idx();
      }
    }
    else {
      offst = WN_offset(cloned_wn);
      if (!Preg_Is_Dedicated(offst))
	WN_offset(cloned_wn) += _sym->Get_cloned_preg_last_idx();
    }

    return;
  }

  cloned_st = _sym->Get_Cloned_ST (st);

  if ((cloned_st != NULL) && (ST_level(cloned_st) == GLOBAL_SYMTAB)) {
						// Promoted as global
      if (Alias_Nystrom_Analyzer) {
        ConstraintGraph::updatePromoteStIdxMap(WN_st_idx(wn) , ST_st_idx(cloned_st));
      }
      WN_st_idx(wn) = ST_st_idx(cloned_st);	// So fix up the orig tree also
  }
  else 
      cloned_st = _sym->Get_ST(st);

  WN_st_idx(cloned_wn) = ST_st_idx(cloned_st);
  
} // IPO_CLONE::Fix_ST 


// ======================================================================
// Fix up the INITO pointers
// ======================================================================

void
IPO_CLONE::Fix_INITO(WN* cloned_wn, WN* wn)
{
  FmtAssert(wn && cloned_wn && _sym,("IPO_CLONE::Fix_INITO parameter is null"));

  INITO_IDX init_idx, init_cp_idx;
  if (init_idx = WN_ereg_supp(wn)) {
      if (_sym) {
          // if Get_Cloned_INITO_IDX is true, then it is a promoted static
	  if (init_cp_idx = _sym->Get_Cloned_INITO_IDX(_sym->Get_INITO(init_idx))) {
              WN_ereg_supp(cloned_wn) = init_cp_idx;
              WN_ereg_supp(wn) = init_cp_idx;
          }
          else 
              WN_ereg_supp(cloned_wn) = _sym->Get_INITO_IDX(WN_ereg_supp(wn));
      }
      else {
          WN_ereg_supp(cloned_wn) = WN_ereg_supp(wn);
      }
  }
}


// ======================================================================
// Copy some of the PU flags that inlining/cloning would need.
// ======================================================================
static void
Copy_PU_Flags(PU_IDX old_pu, PU_IDX new_pu) 
{
#if _DONT_COPY_ALL_THE_FLAGS_
    Pu_Table[new_pu].flags |=
    (Pu_Table[old_pu].flags & (PU_HAS_EXC_SCOPES |
                                      PU_NEEDS_FILL_ALIGN_LOWERING |
                                      PU_HAS_VERY_HIGH_WHIRL |
                                      PU_MP_NEEDS_LNO |
                                      PU_HAS_ALLOCA |
                                      PU_HAS_MP |
                                      PU_HAS_NAMELIST |
                                      PU_UPLEVEL |
                                      PU_HAS_REGION ));
#else
    Pu_Table[new_pu].src_lang = Pu_Table[old_pu].src_lang;
    Pu_Table[new_pu].flags = Pu_Table[old_pu].flags;
#endif
}


// ======================================================================
// taken from WN_CopyNode () in wn.c.  Optimized for IPA. 
// ======================================================================

WN *
IPO_CLONE::Copy_Node (const WN *src_wn)
{
    INT16 next_prev_ptrs
	= (OPERATOR_has_next_prev(WN_operator(src_wn)) ? 1 : 0);
    INT16 size = (sizeof(WN) +
		  (sizeof(WN *) * (MAX(0,WN_kid_count(src_wn)-2))) +
		  next_prev_ptrs * (sizeof(mUINT64) + (2 * sizeof(WN *))));

    size = (size + 7) & (~7);	    // 8 byte aligned

    /* avoid calling MEM_POOL_Alloc for every single node */
    if (_raw_buf_size < size) {
	UINT new_size = _default_buf_size;
	while (new_size < size)
	    new_size *= 2;
	_raw_buffer = (WN *)MEM_POOL_Alloc(WN_mem_pool_ptr, new_size);
 	BZERO (_raw_buffer, new_size);
	_raw_buf_size = new_size;
    }

    WN *wn = _raw_buffer;
    _raw_buffer = (WN *) ((char *) _raw_buffer + size);
    _raw_buf_size -= size;

    if (next_prev_ptrs) {
	STMT_WN *stmt_wn = (STMT_WN *) wn;
	wn = (WN *)&(WN_real_fields(stmt_wn));

	WN_linenum(wn) = WN_linenum(src_wn);
	if (_cloned_node_file_id)
	    USRCPOS_filenum(*(USRCPOS *)&(WN_linenum(wn))) = _cloned_node_file_id;
    }

    wn->common = src_wn->common;
    WN_set_map_id(wn, (WN_MAP_ID) (-1));

    WN_Copy_u1u2 (wn, src_wn);
    WN_Copy_u3 (wn, src_wn);
	
#if defined(TARG_SL) || defined(TARG_SL2)
//  need copy these sl2 special flag also see bug 154 
    WN_Copy_sl_ext(wn, src_wn); 
#endif 

    // trace copy wn node here
    Trace_Wn_Copy(wn, src_wn);

    return(wn);
} // Copy_Node


//======================================================================
// Walk the tree nodes and clone each one of them.  Take care of the
// STs, LABELs, INITOs and PREGs at the same time.  The goal to to
// minimize the work of the caller to Clone_Tree so that the caller
// does not have to walk the tree and fix up the symtab-related pointers
// as much.
//======================================================================
WN *
IPO_CLONE::Clone_Tree (WN *wn, ST *clone_st)
{
  WN *ret_wn = NULL;
  WN* kid;
  WN* ret_kid;
  WN* prev_wn;
  OPCODE op;
  
  if (wn == NULL)
	return NULL;

  ret_wn = Copy_Node (wn);

  // Clone the WN to CGNodeId mapping for the cloned node into the caller
  if (Alias_Nystrom_Analyzer)
    ConstraintGraph::cloneWNtoCallSiteCGNodeIdMap(wn, ret_wn, this);

  op = WN_opcode(wn);

  if (_sym) {

      // when we are fixing the original tree after moving statics
      // and initos to the global symtab
      // which occur in the exception handling opcode
      // Exceptions now come as REGIONS (not as EXC_SCOPE_BEGINS)
      if ((WN_operator(wn) == OPR_REGION) && (WN_region_is_EH(wn))) {
        // else we need to fix up the inito's occuring in whirl
            Fix_INITO(ret_wn, wn);
  }


      	/* handle entry points last so that Fix_ST() won't treat the entry
            points as a promoted statics */
      	if (op == OPC_ALTENTRY || op == OPC_FUNC_ENTRY) {
	    if (_sym->Is_new_clone())
		Set_Entry_Point (wn, ret_wn, clone_st);
	}
	else if (OPCODE_has_sym(op) || OPCODE_has_label(op))
            Fix_ST (ret_wn, wn);

       // Nystrom alias analyzer:
       if (Alias_Nystrom_Analyzer) {
         // Map the original st_idx to its clone for non globals
         if (OPCODE_has_sym(op) && !OPCODE_is_call(WN_opcode(wn)) && 
             WN_st(wn) && (ST_IDX_level(WN_st_idx(wn)) != GLOBAL_SYMTAB))
           ConstraintGraph::updateOrigToCloneStIdxMap(WN_st_idx(wn),
                                                      WN_st_idx(ret_wn));
       }
  }

          
  if (op == OPC_BLOCK) {
      ret_kid = NULL;
      kid = WN_first (wn);
      if (kid) {
          ret_kid = Clone_Tree (kid);
          WN_prev (ret_kid) = NULL;
          WN_first(ret_wn) = ret_kid;

          if (_parent_map)
		IPA_WN_MAP_Set (_cloned_map_tab, _parent_map, ret_kid, ret_wn);

          kid = WN_next (kid);
          prev_wn = ret_kid;

          while (kid) {
		ret_kid = Clone_Tree (kid);
		WN_next (prev_wn) = ret_kid;
		WN_prev (ret_kid) = prev_wn;

		if (_parent_map)
                    IPA_WN_MAP_Set (_cloned_map_tab, _parent_map, ret_kid, ret_wn);
		
		prev_wn = ret_kid;
		kid = WN_next(kid);
          }

          WN_next(ret_kid) = NULL;
      
      } else
          WN_first(ret_wn) = NULL;

      WN_last(ret_wn) = ret_kid;

  } else {
      INT kidno;
      for (kidno = 0; kidno < WN_kid_count(wn); kidno++) {
          if (WN_kid (wn, kidno)) { 
	      ret_kid = Clone_Tree (WN_kid (wn, kidno));

              if (_parent_map)
                  IPA_WN_MAP_Set (_cloned_map_tab, _parent_map,
                          ret_kid, ret_wn); 
        
	      WN_kid (ret_wn, kidno) = ret_kid;
          } else 
	      WN_kid (ret_wn, kidno) = NULL;
      }
  }

  return ret_wn;
  
} // IPO_CLONE::Clone_Tree

// ======================================================================
// New_Clone should be called if the consumer needs to clone an entire
// function.  This will:
// 1) create a local symtab for the cloned PU
// 2) promote all the static variables from the original and cloned PU
//    to be global
// 3) clone the tree of the PU and fix up all its symtab related pointers
// 4) copy the symbol table flags from the orig. PU to the cloned PU 
// ======================================================================

void
IPO_CLONE::New_Clone (ST *clone_st)
{
  _sym->New_Symtab();
  
  _sym->Promote_Statics ();

  FmtAssert(_orig_pu,("IPO_CLONE::orig_pu is null"));
  ST * s = WN_st(_orig_pu);

  _cloned_map_tab = WN_MAP_TAB_Create (_mem);

  _cloned_pu = Clone_Tree (_orig_pu, clone_st);

  Copy_PU_Flags(ST_pu(s), ST_pu(WN_st(_cloned_pu)));

#ifndef _LEGO_CLONER
  _parent_map = IPA_WN_MAP_Create (_cloned_map_tab, _mem);
  WN_Parentize (_cloned_pu, _parent_map, _cloned_map_tab);
#endif
  
} // IPO_CLONE::New_Clone



// ======================================================================
// IPO_SYMTAB Related Functions
// ======================================================================


// ======================================================================
// Fix up ST entries
// ======================================================================

template <>
inline void 
IPO_SYMTAB::fix_table_entry<ST>::operator () (UINT idx, ST* st) const
{
    Set_ST_st_idx(st, make_ST_IDX(idx, _sym->Get_cloned_level()));
    if (ST_IDX_level (ST_base_idx (st)) == _sym->Get_orig_level ())
	Set_ST_base_idx(st, make_ST_IDX (ST_IDX_index (ST_base_idx (st)) +
					 _sym->Get_cloned_st_last_idx (),
					 _sym->Get_cloned_level ()));
    if (ST_base_idx(st) == ST_st_idx(st))
        Set_ST_ofst(st, 0);
#ifdef KEY
// Certain exception handling information should have 1 instance per pu
    if (ST_one_per_pu(st)) {
    	Set_ST_is_not_used(st);
	return;
    }
#endif
    switch (ST_sclass(st)) {
    case SCLASS_FORMAL:
    case SCLASS_FORMAL_REF:
	// We don't need these entries since they would be entered
	// as local symbols later during formal parameter processing
	Set_ST_is_not_used(st);
	break;
    default:
	break;
    }
}

// ======================================================================
// Fix up INITO entries
// ======================================================================
template <>
inline void 
IPO_SYMTAB::fix_table_entry<INITO>::operator () (UINT idx, INITO* inito) const
{
    Set_INITO_st_idx(*inito, make_ST_IDX(ST_IDX_index(INITO_st_idx(*inito))+_sym->Get_cloned_st_last_idx(), _sym->Get_cloned_level()));
}

// ======================================================================
// Fix up ST_ATTR entries
// ======================================================================
template <>
inline void 
IPO_SYMTAB::fix_table_entry<ST_ATTR>::operator () (UINT idx, ST_ATTR* st_attr) const
{
    // bug fix for OSP_125
    Is_True (ST_ATTR_kind (*st_attr) == ST_ATTR_DEDICATED_REGISTER || 
             ST_ATTR_kind (*st_attr) == ST_ATTR_SECTION_NAME,
	     ("expecting ST_ATTR_DEDICATED_REGISTER or ST_ATTR_SECTION_NAME"));
    ST_IDX st_idx = ST_ATTR_st_idx (*st_attr);
    Set_ST_ATTR_st_idx (*st_attr, make_ST_IDX (ST_IDX_index (st_idx) +
					 _sym->Get_cloned_st_last_idx(),
					 ST_IDX_level (st_idx)));
}


// ======================================================================
// Main function to create/update the local symbol table for the cloned PU
// Copy Local tables from orig to cloned SYMTAB using block copy
// This is to take advantage of the NEW SYMTAB organizaion and minimize
// individual symbol table entry insertion.  Should be better for
// performance.
// If label_only is TRUE, only the LABEL table will be copied
// ======================================================================

void
IPO_SYMTAB::Copy_Local_Tables(BOOL label_only)
{

  UINT32 start_idx = 0;

  if (_is_new_clone == FALSE)
	start_idx = 1;   // don't need to copy the 0'th entry

  if (label_only == FALSE) {
      (void)Copy_array_range(*_orig_scope_tab[_orig_level].st_tab, 
		         *_cloned_scope_tab[_cloned_level].st_tab, 
			 start_idx, 
			 (_orig_scope_tab[_orig_level].st_tab)->Size());

      (void)Copy_array_range(*_orig_scope_tab[_orig_level].preg_tab, 
			 *_cloned_scope_tab[_cloned_level].preg_tab, 
			 start_idx, 
			 (_orig_scope_tab[_orig_level].preg_tab)->Size());

      (void)Copy_array_range(*_orig_scope_tab[_orig_level].st_attr_tab, 
			 *_cloned_scope_tab[_cloned_level].st_attr_tab, 
			 start_idx, 
			 (_orig_scope_tab[_orig_level].st_attr_tab)->Size());

      (void)Copy_array_range(*_orig_scope_tab[_orig_level].inito_tab, 
			 *_cloned_scope_tab[_cloned_level].inito_tab, 
			 start_idx, 
			 (_orig_scope_tab[_orig_level].inito_tab)->Size());

  }
  else {
	// Need to reset _cloned_label_last_idx to reflect the current cloned SYMTAB
	Set_cloned_label_last_idx((_cloned_scope_tab[_cloned_level].label_tab)->Size()-1);
	Set_cloned_inito_last_idx((_cloned_scope_tab[_cloned_level].inito_tab)->Size()-1);
#ifdef KEY
	if (PU_src_lang (Get_Current_PU()) & PU_CXX_LANG)
	{
	    // bug 4091: for C++ copy all INITOs
	    // We really need to clone only the EH initos here, but if
	    // we only clone them, we will need to do lots of fixups.
            (void)Copy_array_range(*_orig_scope_tab[_orig_level].inito_tab, 
			           *_cloned_scope_tab[_cloned_level].inito_tab, 
			           start_idx, 
			           (_orig_scope_tab[_orig_level].inito_tab)->Size());
	}
#endif
  }

  (void)Copy_array_range(*_orig_scope_tab[_orig_level].label_tab, 
			 *_cloned_scope_tab[_cloned_level].label_tab, 
			 start_idx, 
			 (_orig_scope_tab[_orig_level].label_tab)->Size());

  (void)Delete_array_item(*_orig_scope_tab[_orig_level].label_tab,
                         *_cloned_scope_tab[_cloned_level].label_tab,
                         start_idx,
                         (_orig_scope_tab[_orig_level].label_tab)->Size());                                  
  
  if ((_is_new_clone == FALSE) && (label_only == FALSE)) {
      // The tables are appended to the end of the cloned table, so 
      // we have to fix up the values in those tables

      For_all_entries(*_cloned_scope_tab[_cloned_level].st_tab, 
	fix_table_entry<ST> (this), _cloned_st_last_idx+1);

      For_all_entries(*_cloned_scope_tab[_cloned_level].st_attr_tab, 
	fix_table_entry<ST_ATTR> (this), _cloned_st_attr_last_idx+1);
  }

  For_all_entries(*_cloned_scope_tab[_cloned_level].inito_tab, 
      fix_table_entry<INITO> (this), _cloned_inito_last_idx+1);

}


// ======================================================================
// This function should be called if a PU needs to be cloned into a 
// seperate PU, as needed in constant propagation.
// Create a new scope_tab array for cloned_scope_tab and initialize it
// then create a local symbol table for the cloned_scope_tab
// ======================================================================
void
IPO_SYMTAB::New_Symtab (void)
{

  FmtAssert(!PU_has_altentry(Pu_Table[ST_pu(_orig_scope_tab[_orig_level].st)]),
            ("Can't clone procedures with multiple entry point"));

  SCOPE *_cloned_scope_tab = (SCOPE *) MEM_POOL_Alloc (_mem,
                                          (_orig_level+1) * sizeof(SCOPE));


  // Copy all except the current _level for this cloned symtab
  // memcpy(_cloned_scope_tab, _orig_scope_tab, _orig_level*sizeof(SCOPE));
  SYMTAB_IDX i;
  for (i = 1; i < _orig_level; ++i) {
      _cloned_scope_tab[i] = _orig_scope_tab[i];
  }

  Set_Cloned_Symtab(_cloned_scope_tab);

  // Set the current Scope_tab to the cloned one
  // Scope_tab = _cloned_scope_tab;
  SCOPE_CONTEXT switch_scope(_cloned_scope_tab);

  // The following would creat instances of ST_TAB, LABEL_TABLE, PREG_TAB and
  // INITO_TAB
  New_Scope(_orig_level, _mem, FALSE);

  Copy_Local_Tables(FALSE);

} // IPO_SYMTAB::New_Symtab


// ======================================================================
// This function should be called for inlining purposes -- the PU
// is NOT cloned as a seperate PU but is actually integrated into
// another PU.
// Append symtab from orig_scope_tab to cloned_scope_tab
// If label_only is TRUE, only the LABEL table will be copied
// ======================================================================
void
IPO_SYMTAB::Update_Symtab (BOOL label_only)
{

  FmtAssert(!PU_has_altentry(Pu_Table[ST_pu(_orig_scope_tab[_orig_level].st)]),
            ("Can't inline procedures with multiple entry point"));

  FmtAssert(_cloned_scope_tab, ("Cloned scope tab is not set up in Update_Symtab"));

  Copy_Local_Tables(label_only);
}

#ifdef KEY
// bug 3089: Fix traversal of INITVs, the original code never actually
// traversed the tree of INITVs, so even though the ST was promoted to
// global symtab, the ST reference in an initv was not being updated.
static inline void
traverse_initvs (INITV_IDX start, ST_IDX old, ST_IDX copy)
{
    if (! start) return;

    switch (INITV_kind (start))
    {
      case INITVKIND_BLOCK:
	traverse_initvs (INITV_blk (start), old, copy);
	traverse_initvs (INITV_next (start), old, copy);
        break;
      case INITVKIND_SYMOFF:
      {	
	ST_IDX st = INITV_st (start);
	if (st == old)
	  Set_INITV_st (start, copy);
        // fall through
      }
      default:
        traverse_initvs (INITV_next (start), old, copy);
        break;
    }
}
#endif // KEY

// ======================================================================
// Walk the ST list and for those that are PU-level static, move them and
// their correcponding INITO to the Global Symtab
// ======================================================================
template<>
inline void
IPO_SYMTAB::promote_entry<ST>::operator () (UINT idx, ST* old_st) const
{
    ST *copy_st;
    ST_IDX  old_st_idx = ST_st_idx(old_st);
    INITO_TAB *it_tab = Scope_tab[ST_IDX_level(old_st_idx)].inito_tab;
    int it_tab_size = it_tab->Size();

    switch (ST_sclass(old_st)) {
    case SCLASS_PSTATIC:
    case SCLASS_FSTATIC:
	copy_st = _sym->IPO_Copy_ST(old_st, GLOBAL_SYMTAB);
	Set_ST_sclass(copy_st, SCLASS_FSTATIC);
	Set_ST_is_not_used(old_st);
#ifdef TARG_X8664
	// Bug 1881 - when IPA clones or inlines a function, there may be 
	// several copies of PSTATIC variables and these variables are promoted
	// to the global symbol table to maintain consistent values across
	// many copies. We should align any variables here because the 
	// vectorizer expects IPA will align them. We have already done the 
	// work in IPA symbol table merge. This should take care of PSTATIC 
	// copies. Avoid Fortran equivalenced arrays (bug 1988).
	if ( ST_sym_class(copy_st) == CLASS_VAR &&
	     !ST_is_equivalenced(copy_st) ) {
	  TY_IDX ty = ST_type(copy_st);
	  if (TY_kind(ST_type(copy_st)) == KIND_POINTER) {
	    ty = TY_pointed(ST_type(copy_st));
	  }
	  if (TY_kind(ty) != KIND_FUNCTION) {
	    TY_IDX st_ty_idx = ST_type(copy_st);
	    Set_TY_align_exp(st_ty_idx, 4);
	    Set_ST_type(copy_st, st_ty_idx);
	  }
	}
#endif
        // Walk through the INITV entries that was pointed to by each INITO of current PU
        // if the INITV¡¯s type is INITVKIND_SYMOFF and the symbol is being promoted
        // to global symtab, update it with the new st in global symtab
	for(INITO_IDX it_idx = 1; it_idx<(INITO_IDX)it_tab_size; it_idx++) {
	  INITV_IDX iv_idx = (*it_tab)[it_idx].val;
#ifdef KEY
          traverse_initvs (iv_idx, ST_st_idx (old_st), ST_st_idx (copy_st));
#else
	  INITV &iv = Initv_Table[iv_idx];
          switch (INITV_kind(iv)) {
          case INITVKIND_SYMOFF:
	      {
	        ST* initv_st = &St_Table[INITV_st(iv)];
                if(initv_st == old_st)
                  Set_INITV_st(iv_idx, ST_st_idx(copy_st));
              }
              break;
          case INITVKIND_SYMDIFF:
          case INITVKIND_SYMDIFF16:
          case INITVKIND_LABEL:
          case INITVKIND_BLOCK:
    	  default:
              break;
          }
#endif // KEY
	}	
	break;
    default:
	break;
    }
}

// bug fix for OSP_125
template <>
inline void
IPO_SYMTAB::promote_entry<ST_ATTR>::operator () (UINT idx, ST_ATTR* old_attr) const
{
    // If the ST entry of the ST_ATTR has been promoted, then need to
    // promote this ST_ATTR also ( only ST_ATTR_SECTION_NAME )
    ST *orig_ST = &St_Table[ST_ATTR_st_idx (*old_attr)];
    ST *cloned_ST = _sym->Get_Cloned_ST(orig_ST);
    if ( cloned_ST ) {
      ST_ATTR_IDX new_attr_idx;
      ST_ATTR& new_attr = New_ST_ATTR(GLOBAL_SYMTAB, new_attr_idx);
      ST_ATTR_Init(new_attr, ST_st_idx(cloned_ST),
		   ST_ATTR_kind(*old_attr), ST_ATTR_section_name(*old_attr) );
    }
}


// ======================================================================
// Walk the ST list and for those that are PU-level static that have
// a different base, need to fix it
// ======================================================================

template <>
inline void
IPO_SYMTAB::fix_base<ST>::operator () (UINT idx, ST* old_st) const
{
    ST *copy_st = NULL;

    switch (ST_sclass(old_st)) {
    case SCLASS_PSTATIC:
    case SCLASS_FSTATIC:
	copy_st = _sym->Get_Cloned_ST(old_st);
	Is_True(copy_st, ("PSTATIC symbol must have been promoted at this point"));
	if (ST_base_idx (old_st) != ST_st_idx (old_st)) {
	    // Need to fix up its base idx
            ST *st = _sym->Get_Cloned_ST(ST_base(old_st));
	    Is_True(st, ("Base of PSTATIC symbol must have been promoted at this point"));
            Set_ST_base_idx(copy_st, ST_st_idx(st));
	}
	break;
    default:
	break;
    }
}

template <>
inline void
IPO_SYMTAB::promote_entry<INITO>::operator () (UINT idx, INITO* old_inito) const
{
    // If the ST entry of the INITO has been promoted, then need to
    // promote this INITO also
    ST *cloned_ST = _sym->Get_Cloned_ST(INITO_st(old_inito));
    if (cloned_ST) { 
	INITO_IDX new_inito_idx = New_INITO(ST_st_idx(cloned_ST), old_inito->val);
	_sym->Set_Cloned_INITO(old_inito, new_inito_idx);
    }
}

void
IPO_SYMTAB::Promote_Statics (void)
{

  For_all (St_Table, _orig_level, promote_entry<ST>(this));
  For_all (St_Table, _orig_level, fix_base<ST>(this));
  For_all (Inito_Table, _orig_level, promote_entry<INITO>(this));
  // bug fix for OSP_125
  For_all (St_Attr_Table, _orig_level, promote_entry<ST_ATTR>(this));

} // IPO_SYMTAB::Promote_Statics

// ======================================================================
// Copy the ST, INITO (from orig_scope_tab, to cloned_scope_tab)
// ======================================================================
ST*
IPO_SYMTAB::IPO_Copy_ST (ST* st, SYMTAB_IDX scope)
{
  ST* new_st;
  
  new_st = Get_Cloned_ST(st);

// comma nodes ==> there can be calls within calls : nested calls
// hence we use cached name ONLY if its the same as the new_name
//
  if (new_st && (scope == GLOBAL_SYMTAB))
    return new_st;

  // Set the current Scope_tab to the cloned one
  // Scope_tab = _cloned_scope_tab;
  SCOPE_CONTEXT switch_scope(_cloned_scope_tab);

  new_st = Copy_ST_No_Base(st, scope);

  Set_Cloned_ST(st, new_st);

  return new_st;
} // IPO_SYMTAB::IPO_Copy_ST 


INITO_IDX
IPO_SYMTAB::Copy_INITO(INITO_IDX orig_init_idx)
{
  FmtAssert(FALSE,("IPO_SYMTAB::Copy_INITO NOT IMPLEMENTED FOR NEW SYMTAB\n"));
  return orig_init_idx;
}

void
IPO_SYMTAB::Set_Cloned_ST (ST *old_st, ST* new_st)
{

  _hash_maps->Insert(old_st, new_st);
}

void
IPO_SYMTAB::Set_Cloned_INITO (INITO *old_inito, INITO_IDX new_inito)
{

  _hash_maps->Insert(old_inito, (void *)(INTPTR)new_inito);
}

INITV_IDX
IPO_SYMTAB::Clone_INITVs_For_EH (INITV_IDX inov, INITO_IDX ino)
{
  INITV_IDX head=0, tail=0;

  while (inov) {
    INITV_IDX cloned_iv = Copy_INITV(tail, ino, inov);
    INITV &iv = Initv_Table[inov];
    INITV &c_iv = Initv_Table[cloned_iv];

    if (head == 0)
        head = tail = cloned_iv;

    tail = cloned_iv;

    switch (INITV_kind(iv)) {
    case INITVKIND_SYMOFF: 
	{
	  ST* initv_st = Get_Orig_ST(INITV_st(iv));
	  ST* cloned_st;

	  if (ST_level(initv_st) == GLOBAL_SYMTAB) 
	      cloned_st = initv_st;
	  else
	      cloned_st = Get_Cloned_ST(initv_st);
	  
	  if (cloned_st == NULL) {
	      cloned_st = Get_ST(initv_st);

	      switch (ST_sclass(cloned_st)) {
	      case SCLASS_FORMAL:
	      case SCLASS_FORMAL_REF:
		  if (ST_is_not_used(cloned_st)) {
		  // This ST should have been copied and turned into 
		  // an AUTO var
		      cloned_st = Get_Cloned_ST(cloned_st);
		      Is_True(cloned_st, ("FORMAL ST should have been cloned"));
		  }
		  break;
	      default:
		  break;
	      }
	  }

	  Set_INITV_st (cloned_iv, ST_st_idx(cloned_st));
	} 
        break;
    case INITVKIND_SYMDIFF:
    case INITVKIND_SYMDIFF16:
	{
	  ST* initv_st = Get_Orig_ST(INITV_st2(iv));
	  ST* cloned_st;

	  if (ST_level(initv_st) == GLOBAL_SYMTAB) 
	      cloned_st = initv_st;
	  else
	      cloned_st = Get_Cloned_ST(initv_st);
	
	  if (cloned_st == NULL) {
	      cloned_st = Get_ST(initv_st);

	      switch (ST_sclass(cloned_st)) {
	      case SCLASS_FORMAL:
	      case SCLASS_FORMAL_REF:
		  if (ST_is_not_used(cloned_st)) {
		  // This ST should have been copied and turned into 
		  // an AUTO var
		      cloned_st = Get_Cloned_ST(cloned_st);
		      Is_True(cloned_st, ("FORMAL ST should have been cloned"));
		  }
		  break;
	      default:
		  break;
	      }
	  }
	  INT lab_level = LABEL_IDX_level(INITV_lab1(iv));
	  INT lab_index = LABEL_IDX_index(INITV_lab1(iv)) + 
	  		  Get_cloned_label_last_idx();
	  Set_INITV_lab1 (cloned_iv, make_LABEL_IDX(lab_index, lab_level));

	  Set_INITV_st2 (cloned_iv, ST_st_idx(cloned_st));
	} 
	break;

    case INITVKIND_LABEL:
       {
	INT lab_level = LABEL_IDX_level(INITV_lab(iv));
	INT lab_index = LABEL_IDX_index(INITV_lab(iv)) + 
			Get_cloned_label_last_idx();
	Set_INITV_lab (cloned_iv, make_LABEL_IDX(lab_index, lab_level));
	break;
       }
    
    case INITVKIND_BLOCK:
        Set_INITV_blk(cloned_iv, Clone_INITVs_For_EH (INITV_blk(iv), ino));
        break;

    default:
        break;
    }


    inov = INITV_next(iv);
  }

  return head;
  
} // IPO_SYMTAB::Clone_INITVs_For_EH

// ---------------------------------------------------------------------
// 
// IPO_ADDR_HASH Member Functions
//
// ---------------------------------------------------------------------

const INT IPO_ADDR_HASH::hash_size;

void
IPO_ADDR_HASH::Insert (void *orig, void *copy)
{
    struct hash_node *p = CXX_NEW (struct hash_node, mem);
    INT hash_value = hash ((INT)(INTPS) orig);

    p->orig = orig;
    p->copy = copy;
    p->next = table[hash_value];
    table[hash_value] = p;
    table_empty = FALSE;

} // IPO_ADDR_HASH::Insert

void *
IPO_ADDR_HASH::Lookup (void *key)
{
    if (key == 0 || table_empty)
        return 0;

    INT hash_value = hash ((INT)(INTPS) key);
    struct hash_node *p = table[hash_value];

    while (p) {
        if (p->orig == key)
            return p->copy;
        p = p->next;
    }

    return 0;
} // IPO_ADDR_HASH::Lookup


//--------------------------------------------------------------------
// this routine is used to reset the copied entry. This is needed
// for fortran based pu static variables
//--------------------------------------------------------------------
void
IPO_ADDR_HASH::Reset_Lookup (void *key)
{
  if (key == 0 || table_empty)
    return;

  INT hash_value = hash ((INT)(INTPS) key);
  struct hash_node *p = table[hash_value];
  struct hash_node *prev = table[hash_value];

  while (p) {
    if (p->orig == key)
      {
	if (table[hash_value] == p)
	  table[hash_value] = p->next;
	else
	  prev->next = p->next;
	CXX_DELETE(p, mem);
	return;
      }
    prev = p;
    p = p->next;
  }
} // IPO_ADDR_HASH::Reset_Lookup


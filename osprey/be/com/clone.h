/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
#ifndef clone_INCLUDED
#define clone_INCLUDED
// ====================================================================
// ====================================================================
//
// Module: 
// $Revision: 
// $Date: 
// $Author: 
// $Source: 
//
// Revision history:
//  22-Sep-97 - Original Version 
//
// Description:
//
// ======================= PU and Symtab clone routines =======================

// CLONE LEGALITY:
// (a) Cannot clone recursive PUs (TODO: investigate how to do this)
// (b) Cannot clone alternate entry pts PUS

// CLONE USAGE:
// * During inlining (clone callee)
// * During constant propogation (function cloning): 
//   - need to generate unique names, need to create new "scope" struct



#include <algorithm>

#include <ext/hash_map>

#ifndef CXX_MEMORY_INCLUDED
#include "cxx_memory.h"                 // For CXX_NEW
#endif /* CXX_MEMORY_INCLUDED */

#include "defs.h"

#include "symtab.h"                     // for Scope_tab
#include "wn.h"			        // includes wn_map.h

// ======================================================================
// IPO_SYMTAB class: encapsulates LOCAL symbol table info
// Under the new symtab design, the local symbol table contains info on
// a. LABEL_TAB, PREG_TAB (local ONLY)
// b. ST_TAB, INITO_TAB, INITV_TAB, and ST_ATTR_TAB (local/global)
// Maps SYMTAB_IDX (original symtab) ---> SYMTAB_IDX (cloned symtab)
// ======================================================================
// (1) Function Inlining
//     - create copy of callee whirl tree
//     - Promote PU Statics to File Statics (These go into IPA Global symtab)
//     - Append callee local tables to caller local tables
//       (label, preg, st, inito): compute offsets for each of these tables
//     - Assume: Global Tables already fixed when PUs are read in 
//       ie IPA Global symtab is all set and has necessary maps
//     - Fix/Patch ST and TY in callee using offsets and Global Maps
//     - Clone INITVs, INITOs
// (2) Function Cloning
//    - create copy of callee whirl tree
//    - Note: pu, cloned_pu are in same_file
//    - Create copy of scope[orig_symtab_idx] : the "new" symtab for cloned pu
//      (copy all 4 local tables: label, preg, st, inito
//    - Note: during function cloning "global" tables don't change EXCEPT
//    - Create new ST in global st table for "cloned" pu
//    - Fix/Patch local table info in cloned pu (using the copy of scope[..]
//    - Promote PU Statics to File Statics (These go into IPA Global symtab)
//      Needed because both pu and cloned_pu need to access same static
// 

// Generic hash table for mapping a pair of addresses.  Used for
// mapping the address of a TY/ST to that of its copy. 
//
class IPO_ADDR_HASH {

private:

  static const INT hash_size = 256;

  struct hash_node {
    void *orig;
    void *copy;
    struct hash_node *next;
  } *table[hash_size];

  MEM_POOL *mem;

  BOOL table_empty;

  INT hash (INT key) {
    return (key & 0xff) ^ ((key >> 8) & 0xff);
  };

public:

  IPO_ADDR_HASH (MEM_POOL *m) {
    mem = m;
    table_empty = TRUE;
    BZERO (table, sizeof(table));
  };

  ~IPO_ADDR_HASH () {
    for (int i = 0; i < hash_size; i++){
      struct hash_node *cur = table[i];
      while(cur){
        struct hash_node *tmp = cur;
        cur = cur->next;
        CXX_DELETE(tmp, mem);
      }
    }
  }

  void Insert (void *orig, void *copy);

  void *Lookup (void *);

  void Clear (void) {
    if (!table_empty) {
      BZERO (table, sizeof(table));
      table_empty = TRUE;
    }
  };

  void Reset_Lookup (void *key);
}; // IPO_ADDR_HASH


class IPO_SYMTAB {

private:
  
  SCOPE *_orig_scope_tab;             // Original (input) scope table
  SCOPE *_cloned_scope_tab;           // Cloned   (output) scope table
  SYMTAB_IDX _orig_level;             // The level of current PU in orig scope_table
  SYMTAB_IDX _cloned_level;           // The level of cloned PU in cloned scope_table
                                      // Same as _orig_level for PU cloning
  MEM_POOL *_mem;                     // for temp. data structures
  IPO_ADDR_HASH *_hash_maps;          // maps for lookup
                                      // used for initos and STs

  BOOL _same_file;                    // both symtabs come from the same file
  UINT _cloned_st_last_idx;	      // If the table is newly cloned,
				      // then this has value = 0
				      // otherwise it contains the
				      // last st_idx of the ST_TAB
				      // of the passed-in _cloned_level
				      // usually for inlining purpose
  UINT _cloned_inito_last_idx;	      // If the table is newly cloned,
				      // then this has value = 0
				      // otherwise it contains the
				      // last inito_idx of the INITO_TAB
				      // of the passed-in _cloned_level
				      // usually for inlining purpose
  UINT _cloned_label_last_idx;	      // If the table is newly cloned,
				      // then this has value = 0
				      // otherwise it contains the
				      // last st_idx of the LABEL_TAB
				      // of the passed-in _cloned_level
				      // usually for inlining purpose
  UINT _cloned_preg_last_idx;	      // If the table is newly cloned,
				      // then this has value = 0
				      // otherwise it contains the
				      // last preg_idx of the PREG_TAB
				      // of the passed-in _cloned_level
  UINT _cloned_st_attr_last_idx;      // If the table is newly cloned,
				      // then this has value = 0
				      // otherwise it contains the
				      // last st_attr_idx of the ST_ATTR_TAB
				      // of the passed-in _cloned_level
  BOOL _is_new_clone;		      // True if really creates a brand new
				      // PU, false as in the case of inlining
	

  void Copy_Local_Tables(BOOL);       // LABEL, PREG, ST, INITO, ST_ATTR

  template <class T>
  struct fix_table_entry
  {
    IPO_SYMTAB *_sym;

    fix_table_entry(IPO_SYMTAB *sym): _sym(sym) {}
    
    void operator () (UINT idx, T *entry) const;
  };

  template <class T>
  struct promote_entry 
  {
    IPO_SYMTAB *_sym;

    promote_entry(IPO_SYMTAB *sym): _sym(sym) {}

    void operator () (UINT idx, T* entry) const;
  };

  template <class T>
  struct fix_base 
  {
    IPO_SYMTAB *_sym;

    fix_base(IPO_SYMTAB *sym): _sym(sym) {}

    void operator () (UINT idx, T* entry) const;
  };

public:


  // Constructor
  // Calls to do cloning for constant propagation
  IPO_SYMTAB (SCOPE *orig_scope_tab, 
			SYMTAB_IDX symtab_idx, MEM_POOL *m) :
  _orig_scope_tab(orig_scope_tab), _cloned_scope_tab(NULL), _mem(m), 
  _same_file(TRUE), _orig_level(symtab_idx), _cloned_level(symtab_idx) {
    _cloned_st_last_idx = 0;
    _cloned_inito_last_idx = 0;
    _cloned_label_last_idx = 0;
    _cloned_preg_last_idx = 0;
    _cloned_st_attr_last_idx = 0;
    _is_new_clone = TRUE;
    _hash_maps = CXX_NEW (IPO_ADDR_HASH (m), m);
  };

  ~IPO_SYMTAB (){
    CXX_DELETE(_hash_maps, Malloc_Mem_Pool);
  }

  // Calls to do cloning for inlining
  IPO_SYMTAB (SCOPE *orig_scope_tab, SCOPE *cloned_scope_tab, 
			SYMTAB_IDX orig_symtab_idx, 
			SYMTAB_IDX cloned_symtab_idx, MEM_POOL *m, 
              		BOOL Same_file = TRUE) :
  _orig_scope_tab(orig_scope_tab), _cloned_scope_tab(cloned_scope_tab),
   _mem(m), _same_file(Same_file), _orig_level(orig_symtab_idx), _cloned_level(cloned_symtab_idx) {
    _cloned_st_last_idx = cloned_scope_tab[cloned_symtab_idx].st_tab->Size()-1;
    _cloned_inito_last_idx = cloned_scope_tab[cloned_symtab_idx].inito_tab->Size()-1;
    _cloned_label_last_idx = cloned_scope_tab[cloned_symtab_idx].label_tab->Size()-1;
    _cloned_preg_last_idx = cloned_scope_tab[cloned_symtab_idx].preg_tab->Size()-1;
    _cloned_st_attr_last_idx = cloned_scope_tab[cloned_symtab_idx].st_attr_tab->Size()-1;
    _is_new_clone = FALSE;
    _hash_maps = CXX_NEW (IPO_ADDR_HASH (m), m);
  };

  // Member functions: initializer
  void New_Symtab ();                // Creates "new" cloned_scope_tab
                                     // Used for cloning

  void Update_Symtab (BOOL);         // Appends to cloned_scope_tab
                                     // Used for inlining

  void Promote_Statics ();           // move local statics to IPA global symtab

  INITO_IDX Copy_INITO(INITO_IDX orig_init=0);
                                     // Initialized data objects
                                     // special case for C++ EH

  INITV_IDX Clone_INITVs_For_EH (INITV_IDX , INITO_IDX );

  // member access functions

  SCOPE* Get_orig_scope_tab    () const { return _orig_scope_tab; }
  SCOPE* Get_cloned_scope_tab   () const { return _cloned_scope_tab; }
  SYMTAB_IDX Get_orig_level     () const { return _orig_level;}
  SYMTAB_IDX Get_cloned_level   () const { return _cloned_level;}
  UINT Get_cloned_st_last_idx   () const { return _cloned_st_last_idx;}
  UINT Get_cloned_inito_last_idx() const { return _cloned_inito_last_idx;}
  void Set_cloned_inito_last_idx   (UINT idx) { _cloned_inito_last_idx = idx;}
  UINT Get_cloned_label_last_idx   () const { return _cloned_label_last_idx;}
  void Set_cloned_label_last_idx   (UINT idx) { _cloned_label_last_idx = idx;}
  UINT Get_cloned_preg_last_idx() const { return _cloned_preg_last_idx;}
  UINT Get_cloned_st_attr_last_idx() const { return _cloned_st_attr_last_idx;}
  BOOL Is_new_clone             () const { return _is_new_clone; }
  BOOL Same_file                () const { return _same_file; }

  void Set_Cloned_Symtab (SCOPE *scope_tab) { 
	_cloned_scope_tab = scope_tab;
  }

  void Set_Cloned_ST(ST* old_st, ST* new_st);

  ST* Get_Cloned_ST(ST* old_st) {return (ST *) _hash_maps->Lookup (old_st); };

  void Hide_Cloned_ST (ST *st) { _hash_maps->Reset_Lookup (st); };

  void Set_Cloned_INITO(INITO* old_inito, INITO_IDX new_inito);

  INITO_IDX Get_Cloned_INITO_IDX(INITO* old_inito) {
	return (INITO_IDX)(INTPTR)_hash_maps->Lookup (old_inito);
  }

  ST* Get_ST(ST* old_st) {	// Get the copy of the orig st
	return ((ST_level(old_st) != _orig_level)? old_st : &_cloned_scope_tab[_cloned_level].st_tab->Entry(ST_index(old_st)+_cloned_st_last_idx));
  }

  INITO_IDX Get_INITO_IDX(INITO_IDX old_inito) {
	return make_INITO_IDX(INITO_IDX_index(old_inito)+_cloned_inito_last_idx, _cloned_level);
  }

  INITO* Get_INITO(INITO_IDX idx) {
	SYMTAB_IDX level = INITO_IDX_level (idx);
        UINT32 index = INITO_IDX_index (idx);
	return (&_orig_scope_tab[_orig_level].inito_tab->Entry(index));
  }

  ST* Get_Orig_ST(ST_IDX idx) {
	return (&_orig_scope_tab[ST_IDX_level(idx)].st_tab->Entry(ST_IDX_index(idx)));
  }

  ST* IPO_Copy_ST (ST* st, SYMTAB_IDX scope);

  BOOL Is_Cloned_ST(ST* st) { return ((ST_level(st) != _cloned_level) || 
	(ST_index(st) > _cloned_st_last_idx)); }

}; // IPO_SYMTAB

// ======================================================================
// class IPO_CLONE 
// ======================================================================
// used during fn INLINING and fn CLONING

// maps a PU into a cloned PU
// Main functions:  used by others (such as IPO, IPA)
//  a. Constructor
//  b. Clone_Tree
//  c. New_Clone

class IPO_CLONE {

private:

  WN *_orig_pu;                  // original tree
  WN *_cloned_pu;                // cloned tree

  IPO_SYMTAB *_sym;              // handle cloning of symtab

  static INT _label;             // unique labels for cloned function name
  static const INT _default_buf_size = 2048; 

  WN_MAP_TAB *_orig_map_tab;     // map tables
  WN_MAP_TAB *_cloned_map_tab;   

  WN_MAP _parent_map;            // parent pointers

  MEM_POOL *_mem;                // mem pool for cloned map tables

  BOOL _same_file;               // if src and dest are from the same file

  mUINT16 _cloned_node_file_id;  // file id of a cloned node in the
                                 // current file, for cross-file inlining (DST)

  WN *_raw_buffer;               // pre-allocated buffer for copied nodes
  UINT _raw_buf_size;

  // For each of the next three functions use the offsets determined by
  // (1)  copying callee tables into caller tables (inlining) OR by
  // (2)  maps from orig_symtab to cloned_symtab

  void Fix_ST (WN *, WN *);     // fix the ST pointer in a tree node: 
  void Fix_TY (WN *, WN *);     // fix the TY pointer in a tree node
  void Fix_INITO(WN* cloned_wn, WN* wn);// fix the INITO in a tree node

  WN *Copy_Node (const WN *src_wn); // copy one tree node

public:
  
  // Constructor(s) used by MAIN IPA to clone a WN *pu
  // 1. Used only in function cloning (_same_file is true: also for IPO_SYMTAB)
  // Caller needs to specify scope_tab and symtab_idx of this PU

  IPO_CLONE (WN *pu, 
             SCOPE *scope_tab,
             SYMTAB_IDX symtab_idx, 
             WN_MAP_TAB* map_tab,
             MEM_POOL *map_pool,
             MEM_POOL *m) : 
    _orig_map_tab(map_tab), _cloned_map_tab(NULL), _parent_map(0),
    _sym(CXX_NEW (IPO_SYMTAB (scope_tab, symtab_idx, m), m)), 
    _orig_pu(pu), _cloned_pu(NULL), 
    _mem(map_pool),             // mem pool for map tables
    _cloned_node_file_id(0), 
    _same_file(TRUE), _raw_buf_size(0)
  {
  };


  // 2. used only by inlining 
  //    a. to make copy of callee, and patch symtabs
  //    b. to make copy of whirl tree
  // Called from IPO_Copy_Tree defined in ipo_clone.cxx, used in ipo_inline.cxx
  IPO_CLONE (WN_MAP_TAB *caller, 
             WN_MAP_TAB *callee, 
             WN_MAP parent,
             SCOPE *callee_scope_tab=NULL,
             SCOPE *caller_scope_tab=NULL,
             SYMTAB_IDX callee_symtab_idx=0, 
             SYMTAB_IDX caller_symtab_idx=0, 
	     IPO_SYMTAB *cloned_symtab=NULL,
             MEM_POOL *map_pool=NULL, 
             BOOL same_file=TRUE,
             mUINT16 filenum=0) :
    _orig_pu(NULL), _cloned_pu(NULL),
#if (!((defined(linux) || defined(BUILD_OS_DARWIN)) && defined(_LP64)))
    _sym(cloned_symtab? cloned_symtab : 
	(callee_scope_tab == NULL? NULL : 
				  CXX_NEW(IPO_SYMTAB(callee_scope_tab,
						    caller_scope_tab, 
						    callee_symtab_idx, 
						    caller_symtab_idx, 
						    Malloc_Mem_Pool, same_file),
 						    Malloc_Mem_Pool))),
#endif // (!((defined(linux) || defined(BUILD_OS_DARWIN)) && defined(_LP64)))
    _orig_map_tab (callee), _cloned_map_tab(caller), _parent_map(parent),
    _mem(map_pool),             // mem pool for map tables
    _cloned_node_file_id(filenum), 
    _same_file(same_file), _raw_buf_size (0)
  {
#if (defined(linux) || defined(BUILD_OS_DARWIN)) && defined(_LP64)
    if ((cloned_symtab == NULL) && (callee_scope_tab != NULL))
      _sym = CXX_NEW (IPO_SYMTAB (callee_scope_tab,
				  caller_scope_tab, 
				  callee_symtab_idx, 
				  caller_symtab_idx, 
				  Malloc_Mem_Pool, same_file),
		      Malloc_Mem_Pool);
    else
      _sym = cloned_symtab;
#endif // (!((defined(linux) || defined(BUILD_OS_DARWIN)) && defined(_LP64)))
  };

  // 3. used only for cloning dynamic array bound expressions
  IPO_CLONE (IPO_SYMTAB *ipo_symtab) : 
    _orig_map_tab(NULL), _cloned_map_tab(NULL), _parent_map(0),
    _sym(ipo_symtab), 
    _orig_pu(NULL), _cloned_pu(NULL),
    _mem(NULL), 
    _cloned_node_file_id(0), 
    _same_file(ipo_symtab->Same_file()), _raw_buf_size(0)
  {
  };

  // Extern interface 
  void Set_Entry_Point (WN *, WN *, ST *); // Set node as entry point

  // Actual copying of the tree
  WN *Clone_Tree (WN *, ST *clone_st = NULL); 

  // Create a new copy of _sym
  void New_Clone (ST *clone_st = NULL);   

  // Promote Statics 
  void Promote_Statics() { _sym ->Promote_Statics();}

  //data Access functions
  WN *Get_Cloned_PU () const { return _cloned_pu; };

  SCOPE* Get_Cloned_Symtab () const { return _sym->Get_cloned_scope_tab (); };

  char *Get_Func_Name () { return ST_name(WN_st(_cloned_pu)); };

  WN_MAP_TAB *Get_Orig_maptab()    { return _orig_map_tab; }
  WN_MAP_TAB *Get_Cloned_maptab () { return _cloned_map_tab; };

  WN_MAP Get_parent_map () { return _parent_map; };

  ST *Get_Func_ST () { return WN_st(_cloned_pu); };

  IPO_SYMTAB *Get_sym() { return _sym; };

  
}; // IPO_CLONE


#endif // clone_INCLUDED


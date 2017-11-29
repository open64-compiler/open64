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
#ifndef cxx_ipa_section_annot_INCLUDED
#define cxx_ipa_section_annot_INCLUDED

#ifndef cxx_hash_INCLUDED
#include "cxx_hash.h"
#endif

#ifndef cxx_template_INCLUDED
#include "cxx_template.h"
#endif

#ifndef ipl_summary_INCLUDED
#include "ipl_summary.h"
#endif

#ifndef mempool_INCLUDED
#include "mempool.h"
#endif 

#ifndef ipa_section_INCLUDED
#include "ipa_section.h"
#endif

extern BOOL CFG_scalar_kill(IPA_NODE* caller);
extern BOOL CFG_scalar_euse(IPA_NODE* caller);
extern BOOL Merge_Section(IPA_NODE* caller);
extern void Map_callee_region_to_caller(const IPA_NODE* caller,
                                        const IPA_NODE* callee,
                                        const SUMMARY_CALLSITE* callsite,
                                        PROJECTED_REGION* caller_region, 
                                        PROJECTED_REGION* callee_region);

//-------------------------------------------------------------
// state information
//-------------------------------------------------------------
class STATE
{
#define STATE_IS_MUST_KILL 1
#define STATE_IS_MAY_KILL 2
#define STATE_IS_EUSE 4 
#define STATE_IS_USE 8
#define STATE_IS_MUST_REDUC 16
#define STATE_IS_MAY_REDUC 32
#define STATE_IS_SCALAR 64
#define STATE_IS_REMOVED 128 

  INT _state;
  
  // for arrays create a projected region
  PROJECTED_REGION* _mod;
  PROJECTED_REGION* _ref;
  PROJECTED_REGION* _dcl; 
 
public:

  STATE() : _state(0), _mod(0), _ref(0), _dcl(0) {}

  void Init() { _state = 0; _mod = NULL; _ref = NULL; _dcl = NULL; }

  BOOL Is_must_kill() const { return _state & STATE_IS_MUST_KILL;};
  void Set_must_kill() { _state = _state | STATE_IS_MUST_KILL;};
  
  BOOL Is_may_kill() const { return _state & STATE_IS_MAY_KILL;};
  void Set_may_kill() { _state = _state | STATE_IS_MAY_KILL;};

  BOOL Is_euse() const { return _state & STATE_IS_EUSE;};
  void Set_euse() { _state = _state | STATE_IS_EUSE;};

  BOOL Is_use() const { return _state & STATE_IS_USE;};
  void Set_use() { _state = _state | STATE_IS_USE;};

  BOOL Is_must_reduc() const { return _state & STATE_IS_MUST_REDUC;};
  void Set_must_reduc() { _state = _state | STATE_IS_MUST_REDUC;};

  BOOL Is_may_reduc() const { return _state & STATE_IS_MAY_REDUC;};
  void Set_may_reduc() { _state = _state | STATE_IS_MAY_REDUC;};

  BOOL Is_scalar() const { return _state & STATE_IS_SCALAR;};
  void Set_is_scalar() { _state = _state | STATE_IS_SCALAR;};

  BOOL Is_removed() const { return _state & STATE_IS_REMOVED;};
  void Set_is_removed() { _state = _state | STATE_IS_REMOVED;};
  
  void Set_projected_mod_region(PROJECTED_REGION *r) { _mod = r;};
  PROJECTED_REGION* Get_projected_mod_region() { return _mod;};

  void Set_projected_ref_region(PROJECTED_REGION *r) {_ref = r;};
  PROJECTED_REGION* Get_projected_ref_region() { return _ref;};
    
  void Set_projected_dcl_region(PROJECTED_REGION *r) {_dcl = r;};
  PROJECTED_REGION* Get_projected_dcl_region() { return _dcl;};

  void Print(FILE* fp = stderr);
};


typedef DYN_ARRAY<STATE> STATE_ARRAY;


//----------------------------------------------------------------
// a node of a linked list which describes the mod/ref sections
// for a global array
//----------------------------------------------------------------
class GLOBAL_ARRAY_INFO: public SLIST_NODE {
private:
  STATE _state;         // mod, ref, and dcl sections
  ST_IDX _st_idx;       // ST_IDX of the array    
  
public:
  DECLARE_SLIST_NODE_CLASS(GLOBAL_ARRAY_INFO);
  
  GLOBAL_ARRAY_INFO (ST_IDX st_idx) : 
    _state (STATE()), 
    _st_idx (st_idx)
  {}
  
  void Set_projected_mod_region (PROJECTED_REGION* mod) 
  {
    _state.Set_projected_mod_region(mod);
  }
  PROJECTED_REGION* Get_projected_mod_region ()
  {
    return _state.Get_projected_mod_region();
  }
  
  void Set_projected_ref_region (PROJECTED_REGION* ref) 
  {
    _state.Set_projected_ref_region(ref);
  }
  PROJECTED_REGION* Get_projected_ref_region ()
  { 
    return _state.Get_projected_ref_region();
  }
  
  void Set_projected_dcl_region (PROJECTED_REGION* dcl) 
  {
    _state.Set_projected_dcl_region(dcl);
  }
  PROJECTED_REGION* Get_projected_dcl_region ()
  { 
    return _state.Get_projected_dcl_region();
  }

  void Set_St_Idx (ST_IDX st_idx) { _st_idx = st_idx; }
  ST_IDX St_Idx () const          { return _st_idx; }

  STATE* Get_state () { return &_state; }

  void Print (FILE* fp);
};

//--------------------------------------------------------------------
// a list containing the sections for element of a common block
// TODO:::at some point we need an index into the merged symbol table
//--------------------------------------------------------------------
class GLOBAL_ARRAY_LIST:  public SLIST 
{
private:
  ST_IDX _st_idx;
  UINT _is_messy : 1;

public:
  DECLARE_SLIST_CLASS(GLOBAL_ARRAY_LIST, GLOBAL_ARRAY_INFO);

  GLOBAL_ARRAY_LIST (ST_IDX st_idx) :
    _st_idx (st_idx),
    _is_messy (FALSE)
  {}

  void Set_St_Idx (ST_IDX st_idx) { _st_idx = st_idx; }
  ST_IDX St_Idx () const          { return _st_idx; }
  
  // if there is a shape that cannot be mapped to due to equivalences
  // then the shape must be declared as messy for the whole common
  void Set_is_messy ()   { _is_messy = TRUE; }  
  void Reset_is_messy () { _is_messy = FALSE; }
  BOOL Is_messy () const { return _is_messy; }
 
  char* Get_name() const { return ST_name(_st_idx); }

  GLOBAL_ARRAY_INFO* Find_Global_Array_Info(ST_IDX st_idx);

  GLOBAL_ARRAY_INFO* Find_Global_Array_Info(const SUMMARY_SYMBOL* s) 
  {
    return Find_Global_Array_Info(s->St_idx());
  }

  GLOBAL_ARRAY_INFO* Append(ST_IDX st_idx, MEM_POOL* pool) 
  {
    GLOBAL_ARRAY_INFO* array_info = CXX_NEW(GLOBAL_ARRAY_INFO(st_idx), pool);
    Append(array_info);
    return array_info;
  }
  
  GLOBAL_ARRAY_INFO* Add_Global_Array_Info(const SUMMARY_SYMBOL* s, 
                                           MEM_POOL* pool) 
  {
    return Append(s->St_idx(), pool);
  }

  BOOL Merge(const IPA_NODE* caller,
             const IPA_NODE* callee,
             const SUMMARY_CALLSITE* call,
	     GLOBAL_ARRAY_LIST* caller_list, 
	     MEM_POOL* caller_annot_pool);

  void Print(FILE* fp);
};


//-----------------------------------------------------------------
// a hash table containing sections for all commons for each PU
//-----------------------------------------------------------------
typedef HASH_TABLE<ST_IDX, GLOBAL_ARRAY_LIST*> GLOBAL_ARRAY_TABLE;

typedef HASH_TABLE_ITER<ST_IDX, GLOBAL_ARRAY_LIST*> GLOBAL_ARRAY_TABLE_ITER;

class GLOBAL_ARRAY_LIST_ITER: public SLIST_ITER 
{
public:
  DECLARE_SLIST_ITER_CLASS(GLOBAL_ARRAY_LIST_ITER,GLOBAL_ARRAY_INFO,GLOBAL_ARRAY_LIST);
};

//------------------------------------------------------------------
// node annotation for sections
//------------------------------------------------------------------
class IPA_NODE_SECTION_INFO
{
private:
  CFG_NODE_INFO* _cfg_node;
  CFG_NODE_INFO* _cfg_entry_node; // the entry node
  STATE_ARRAY* _formals;          // state information for formals
  IPA_NODE** _callsite_map;       // a mapping from callsite ids to ipa nodes
  INT _callsite_count;            // call site count
  MEM_POOL* _mem;                 // mem pool used for node
  GLOBAL_ARRAY_TABLE* _glob_table;// hash table used for globals
  DYN_ARRAY<SUMMARY_VALUE>* _value; // used for execution summary 
  DYN_ARRAY<SUMMARY_EXPR>* _expr;
  
public:
  IPA_NODE_SECTION_INFO (MEM_POOL* mem_pool) :
    _cfg_node(0),
    _cfg_entry_node(0),
    _formals(0),
    _callsite_map(0),
    _callsite_count(0),
    _mem(mem_pool),
    _value(0),
    _expr(0),
    _glob_table(CXX_NEW(GLOBAL_ARRAY_TABLE(8, mem_pool), mem_pool))
  {}
  
  void Set_cfg_node(CFG_NODE_INFO* cfg_node) {_cfg_node = cfg_node; }
  CFG_NODE_INFO* Get_cfg_node() const        { return _cfg_node; }

  void Set_cfg_entry_node(CFG_NODE_INFO* entry) {_cfg_entry_node = entry; }
  CFG_NODE_INFO* Get_cfg_entry_node() const     { return _cfg_entry_node; }

  void Set_callsite_map(IPA_NODE** map) { _callsite_map = map; }
  IPA_NODE** Get_callsite_map()         { return _callsite_map; }

  void Set_callsite_count(INT c) { _callsite_count = c; }
  INT Get_callsite_count() const { return _callsite_count; }

  void Set_formals(STATE_ARRAY* f) { _formals = f; }
  STATE_ARRAY* Get_formals() const { return _formals;};

  INT Get_formal_count() const { return (_formals ? _formals->Elements() : 0);}
  STATE* Get_formal(INT i) const { return &(*_formals)[i]; };

  void Set_formal_mod_region(INT i, PROJECTED_REGION *p) 
  { 
    Get_formal(i)->Set_projected_mod_region(p);
  }
  PROJECTED_REGION* Get_formal_mod_region(INT i) const 
  {
    return (*_formals)[i].Get_projected_mod_region();
  }

  void Set_formal_ref_region(INT i, PROJECTED_REGION *p) 
  { 
    Get_formal(i)->Set_projected_ref_region(p);
  }
  PROJECTED_REGION* Get_formal_ref_region(INT i) const 
  {
    return (*_formals)[i].Get_projected_ref_region();
  }

  void Set_formal_dcl_region(INT i, PROJECTED_REGION *p) 
  { 
    Get_formal(i)->Set_projected_dcl_region(p);
  }
  PROJECTED_REGION* Get_formal_dcl_region(INT i) const 
  {
    return (*_formals)[i].Get_projected_dcl_region();
  }

  MEM_POOL* Mem_Pool() const { return _mem; }

  void Set_value(DYN_ARRAY<SUMMARY_VALUE>* sv) { _value = sv; }
  DYN_ARRAY<SUMMARY_VALUE>* Get_value() const  { return _value; }

  void Set_expr(DYN_ARRAY<SUMMARY_EXPR>* sx) { _expr = sx; }
  DYN_ARRAY<SUMMARY_EXPR>* Get_expr() const  { return _expr; }

  GLOBAL_ARRAY_TABLE* Global_Array_Table() const { return _glob_table; }

  GLOBAL_ARRAY_LIST* Find_Global_Array_List(const SUMMARY_SYMBOL* s) 
  {
    Is_True(ST_IDX_level(s->St_idx()) == GLOBAL_SYMTAB, 
            ("Find_Global_Array_List: Symbol is NOT global!\n"));
    return _glob_table->Find(ST_base_idx(ST_ptr(s->St_idx())));
    // TODO_FOR_C: Don't look for the base of C/C++ global arrays
  }

  GLOBAL_ARRAY_LIST* Add_Global_Array_List(const SUMMARY_SYMBOL* s) 
  {
    Is_True(ST_IDX_level(s->St_idx()) == GLOBAL_SYMTAB, 
             ("Global_Array_List: Symbol is NOT global!\n"));
    ST_IDX base_st_idx = ST_base_idx(ST_ptr(s->St_idx()));
    GLOBAL_ARRAY_LIST* list = CXX_NEW(GLOBAL_ARRAY_LIST(base_st_idx), _mem);
    Global_Array_Table()->Enter(base_st_idx, list);
    return list;
    // TODO_FOR_C: Don't look for the base of C/C++ global arrays
  }

  GLOBAL_ARRAY_INFO* Find_Global_Array_Info(const SUMMARY_SYMBOL* s) 
  {
    Is_True(ST_IDX_level(s->St_idx()) == GLOBAL_SYMTAB, 
            ("Find_Global_Array_Info: Symbol is NOT global!\n"));
    GLOBAL_ARRAY_LIST* list = Find_Global_Array_List(s);
    return (list ? list->Find_Global_Array_Info(s->St_idx()) : 0);
  }

  GLOBAL_ARRAY_INFO* Add_Global_Array_Info(const SUMMARY_SYMBOL* s) 
  {
    Is_True(ST_IDX_level(s->St_idx()) == GLOBAL_SYMTAB, 
            ("Find_Global_Array_Info: Symbol is NOT global!\n"));
    GLOBAL_ARRAY_LIST* list = Find_Global_Array_List(s);
    if (!list) {
      list = Add_Global_Array_List(s);
    }
    return list->Add_Global_Array_Info(s, _mem);
  }

  STATE* Find_Global_Array_Sections(const SUMMARY_SYMBOL* s) 
  {
    Is_True(ST_IDX_level(s->St_idx()) == GLOBAL_SYMTAB, 
            ("Find_Global_Array_Sections: Symbol is NOT global!\n"));
    GLOBAL_ARRAY_INFO* info = Find_Global_Array_Info(s);
    return (info ? info->Get_state() : 0);
  }

  PROJECTED_REGION* Global_Array_Region(const SUMMARY_SYMBOL* s,
                                        BOOL* is_messy, 
                                        PROJECTED_REGION* region, 
                                        BOOL is_mod);

  BOOL Set_Global_Array_List_To_Messy(const SUMMARY_SYMBOL* s);

  void Print (FILE* fp);
  void Print_Global_Sections (FILE* fp);

  INT Get_formal_euse_count();
  INT Get_formal_kill_count();
};


//---------------------------------------------------------------------
// this contains the information about variables on a per file basis
// additional variables are added during the propagation, when mapping
// callee annotations to caller annotations
//---------------------------------------------------------------------
class SECTION_FILE_ANNOT 
{
private:
  IVAR* _iv_base;
  IVAR_ARRAY* _iv_grow;
  MEM_POOL* _m;

public:
  SECTION_FILE_ANNOT (IVAR* ivar, MEM_POOL* m) :
    _iv_base (ivar),
    _iv_grow (0),
    _m (m)
  {}

  IVAR* Get_ivar () const { return _iv_base; }

  IVAR_ARRAY* Get_ivar_array () const { return _iv_grow; }

  MEM_POOL* Mem_Pool () const { return _m; }

  INT32 Find_ivar (const IPA_NODE* node, const IVAR& ivar);

  INT32 Add_ivar (const IPA_NODE* node, const IVAR& ivar);
};

#endif // cxx_ipa_section_annot_INCLUDED



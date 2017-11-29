//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_exc.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_exc.h,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// ====================================================================
// ====================================================================


#ifndef opt_exc_INCLUDED
#define opt_exc_INCLUDED	"opt_exc.h"
#ifdef _KEEP_RCS_ID
static char *opt_excrcs_id = 	opt_exc_INCLUDED"$Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#ifndef defs_INCLUDED
#include "defs.h"
#endif
#ifndef tracing_INCLUDED
#include "tracing.h"
#endif
#ifndef mempool_INCLUDED
#include "mempool.h"
#endif
#ifndef cxx_memory_INCLUDED
#include "cxx_memory.h"
#endif
#ifndef cxx_base_INCLUDED
#include "cxx_base.h"
#endif
#ifndef cxx_template_INCLUDED
#include "cxx_template.h"
#endif
#ifndef opt_defs_INCLUDED
#include "opt_defs.h"
#endif
#ifndef opt_mu_chi_INCLUDED
#include "opt_mu_chi.h"
#endif

class CFG;
class STMTREP;

// the information for exception handling scope are kept in this class
// the original goto label, need to be converted to the BB_NODE*.
// the original list of destructor calls for cleanup region should be
// converted to a list of objects that are defined at the exception
// region begin.  A flag field identify if there is INITO information
// in the original EXC_SCOPE_BEGIN node, which tells if it is created
// for the summary of outer region in REGION_BEGIN area.

class EXC_SCOPE {
  EXC            *_exc;      // the pointer to EXC, to get global resource
  EXC_SCOPE      *_parent;   // the parent exc_scope
  WN             *_begin_wn; // the copied region node
  WN		 *_vcall;    // to make Destruct_object faster
  DYN_ARRAY<WN*> *_call_list;// list of call node inside this scope

  // private constructor so it cannot be used
  EXC_SCOPE(void);
  EXC_SCOPE(const EXC_SCOPE&);
  EXC_SCOPE& operator = (const EXC_SCOPE&);

public:
  EXC_SCOPE(WN *begin_wn, EXC *exc);
  ~EXC_SCOPE(void)                        { }

  EXC       *Exc(void) const              { return _exc; }
  void       Set_parent(EXC_SCOPE *es)    { _parent = es; }
  EXC_SCOPE *Parent(void) const           { return _parent; }
  void       Set_begin_wn(WN *wn)         { _begin_wn = wn; }
  WN        *Begin_wn(void) const         { return _begin_wn; }
  BOOL       Is_try_region(void) const;
  void       Add_call(WN *wn)             { _call_list->AddElement(wn); }
  mINT32     Call_count(void) const       { return _call_list->Lastidx()+1; }
  WN        *Get_call(INT i)              { return (*_call_list)[i]; }
};

class EXC_SCOPE_TRY_ITER {
private:
  EXC_SCOPE	*_exc_scope;	// the related exc_scope;
  CHI_LIST	*_chi_list;	// conglomerate chi list for all catches
  CHI_LIST_ITER _chi_iter;
  MEM_POOL	_mem_pool;	// local mempool for chi list copy

  // private constructor so it cannot be used
  EXC_SCOPE_TRY_ITER(void);
  EXC_SCOPE_TRY_ITER(const EXC_SCOPE_TRY_ITER&);
  EXC_SCOPE_TRY_ITER& operator = (const EXC_SCOPE_TRY_ITER&);

  // private methods
  AUX_ID Elem(CHI_NODE *chi);

public:
  EXC_SCOPE_TRY_ITER(EXC_SCOPE *exc_scope);
  ~EXC_SCOPE_TRY_ITER(void) {
    OPT_POOL_Pop(&_mem_pool, EXC_TRACE_FLAG);
    OPT_POOL_Delete(&_mem_pool, EXC_TRACE_FLAG);
  }
  void    Init(void)                   { }
  BOOL    Is_Empty(void)               { return _chi_iter.Is_Empty();}
  AUX_ID  First_elem(void)             { return Elem(_chi_iter.First()); }
  AUX_ID  Next_elem(void)              { return Elem(_chi_iter.Next()); }  
};


class EXC {
  CFG                  *_cfg;         // handle on the control-flow graph
  OPT_STAB             *_opt_stab;    // the symbol table handler
  MEM_POOL             *_mem_pool;    // the memory pool for the exc_scope
  WN_MAP                _exc_map;     // map from WN to the exc_scope object
  STACK<EXC_SCOPE*>    *_exc_scope;   // the exception scopes
  DYN_ARRAY<EXC_SCOPE*>*_exc_scope_list;
  BOOL                  _tracing;     // is general tracing enabled?
 
  // private constructor so it cannot be used
  EXC(void);
  EXC(const EXC&);
  EXC& operator = (const EXC&);

  // Currently private access functions
  void       Push_exc_scope(EXC_SCOPE *scope)  { _exc_scope->Push(scope); }

public:
  EXC(CFG *cfg, OPT_STAB *opt_stab, MEM_POOL *stack_pool) {
    _cfg = cfg;
    _opt_stab = opt_stab;
    _mem_pool = stack_pool;
    OPT_POOL_Push ( stack_pool, MEM_DUMP_FLAG+21 );
    _exc_map = WN_MAP_Create(stack_pool);
    _tracing = Get_Trace( TP_GLOBOPT, EXC_TRACE_FLAG );
    _exc_scope = CXX_NEW(STACK<EXC_SCOPE*>(stack_pool), stack_pool);
    _exc_scope_list = CXX_NEW(DYN_ARRAY<EXC_SCOPE*>(stack_pool), stack_pool);
    _exc_scope->Clear();
  }
  ~EXC(void)                                   { WN_MAP_Delete(_exc_map);
                                                 OPT_POOL_Pop ( _mem_pool,
							    MEM_DUMP_FLAG+21);
                                               }

  void       Clear(void)                       { _exc_scope->Clear();
                                                 _exc_scope_list->Resetidx();
                                               }
  CFG       *Cfg(void) const                   { return _cfg; }
  OPT_STAB  *Opt_stab(void) const              { return _opt_stab; }
  MEM_POOL  *Mem_pool(void) const              { return _mem_pool; }
  mINT32     Elements(void) const              { return _exc_scope->Elements();}
  EXC_SCOPE *Push_exc_scope(WN *scope_begin);
  EXC_SCOPE *Pop_exc_scope(void)               { return (_exc_scope)?
                                                   _exc_scope->Pop():NULL; }
  EXC_SCOPE *Top_exc_scope(void) const         { return _exc_scope->Top(); }
  EXC_SCOPE *Bottom_nth_exc_scope(INT32 n)const{ return _exc_scope->Bottom_nth(n);}
  BOOL       NULL_exc_scope(void) const        { return _exc_scope->Is_Empty();}
  mINT32     Exc_scope_count(void) const       { return _exc_scope_list->Lastidx()+1; }
  EXC_SCOPE *Get_exc_scope(INT i) const        { return (*_exc_scope_list)[i];}
  void       Link_top_es(WN *wn);
  void       Link_wn_es(WN *wn, EXC_SCOPE *es) { WN_MAP_Set(_exc_map, wn, es);}
  EXC_SCOPE *Get_es_link(WN *wn)               { return (EXC_SCOPE *)
                                                   WN_MAP_Get(_exc_map, wn); }
};

#endif  // opt_exc_INCLUDED

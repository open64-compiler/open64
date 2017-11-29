//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Copyright (C) 2007, University of Delaware, Hewlett-Packard Company, 
//  All Rights Reserved.
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
// ====================================================================


#include <vector>

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */

#include "defs.h"
#include "config.h"
#include "config_opt.h"
#include "stab.h"
#include "wn.h"
#include "pu_info.h"
#include "opt_points_to.h"
#include "opt_alias_rule.h"
#include "opt_points_to_summary.h"

using idmap::ID_MAP;
using std::vector;

// add the <name> to the name space. The newly added name should be placed
// at the end of internal data structure <_all_names>.
void
UNAME_SPACE::Append_new_name (UNIFORM_NAME* name) {
  
  Is_True (_all_names.size () == _next_id-1, 
           ("next_id-1 should be equal to the size of vector"));

  name->Set_unique_id (_next_id++);
  _all_names.push_back (name); 

  switch (name->Type()) {
  case UN_NAMED_GLOBAL:
    _global_map.Insert (ST_index(name->ST_for_named_global ()), name); 
    break;

  case UN_MALLOC:
  case UN_ALLOCA:
    _malloc_alloca.push_back (name); break;

  case UN_FORMAL:
    _formals.push_back (name); break;

  case UN_CALLER_LOCAL:
    _caller_locals.push_back (name); break;

  default:
    FmtAssert (FALSE, ("Unknown type %d", (INT)name->Type()));
  }
}

UNIFORM_NAME* 
UNAME_SPACE::Add_global (ST* st) {
  UNIFORM_NAME* na = Get_global (st);
  if (na == NULL) {
    na = CXX_NEW (UNIFORM_NAME(st, UN_NAMED_GLOBAL), _mp);
    Append_new_name (na);
  }
  return na;
}

UNIFORM_NAME*
UNAME_SPACE::Add_malloc (mUINT64 mallocid) {
  UNIFORM_NAME* na = Get_malloc (mallocid);
  if (na == NULL) {
    na = CXX_NEW(UNIFORM_NAME(mallocid, UN_MALLOC), _mp);
    Append_new_name (na);
  }
  return na;
}

UNIFORM_NAME*
UNAME_SPACE::Add_alloca (mUINT64 allocaid) {
  UNIFORM_NAME* na = Get_alloca (allocaid);
  if (na == NULL) {
    na = CXX_NEW(UNIFORM_NAME(allocaid, UN_ALLOCA), _mp);
    Append_new_name (na);
  }
  return na;
}

void
PT_SET::Copy (PT_SET& that) {

  _all_points_to.clear ();

  for (PT_VECTOR_ITER iter = that._all_points_to.begin ();
       iter != that._all_points_to.end (); iter++) {
    POINTS_TO* t = CXX_NEW(POINTS_TO, _mp);
    t->Copy_fully(*iter);

    // we do not check the duplication for the sake of efficiency  
    _all_points_to.push_back (t);
  }
  _addr_space = that._addr_space;
  _may_or_must = that._may_or_must;
  _has_unknown_pt = that._has_unknown_pt;
  _certainty = that._certainty;
}

PT_SET::PT_SET(PT_SET& pts, MEM_POOL* mp) : 
  _mp(mp), _all_points_to(mp) {
  Copy (pts);
}

// Add a points-to to points-to set  
void
PT_SET::Add_points_to (POINTS_TO* pt, PT_CERTAINTY, PT_MAY_OR_MUST) {
  
  POINTS_TO* t = CXX_NEW(POINTS_TO, _mp);
  t->Copy_fully(pt);

  // we do not check the duplication for the sake of efficiency  
  _all_points_to.push_back (t);

  // TODO:: meet the addr-space 
  if (pt->Base() == NULL && pt->Malloc_id() == 0) {
    _has_unknown_pt = TRUE;
  }
}


// check to see whether given points-to alias with the points-tos hold
// by this set.
BOOL
PT_SET::Aliased (ALIAS_RULE* al, POINTS_TO* pt) {
   
   if (Has_unknown_pt ()) { return TRUE; }

   for (PT_VECTOR_ITER iter = _all_points_to.begin ();
        iter != _all_points_to.end (); iter++) {
      if (al->Aliased_Memop (*iter, pt)) 
        return TRUE;
   }
   return FALSE;
}

void
PT_SET::Meet (POINTS_TO* pt) {
  pt->Init();
  pt->Set_expr_kind (EXPR_IS_INVALID); 
  
  if (Cardinality () != 0) {
    PT_VECTOR_ITER iter = _all_points_to.begin ();
    pt->Copy_fully (*iter);
   
    for (iter++; iter != _all_points_to.end (); iter++) {
      pt->Meet (*iter, NULL);
    }
  }
}

BOOL
PT_SET::Aliased (ALIAS_RULE* al, PT_SET* pt_set) {
  
  if (Has_unknown_pt() || pt_set->Has_unknown_pt()) {
    return TRUE;
  }

  for (PT_VECTOR_ITER iter = _all_points_to.begin ();
       iter != _all_points_to.end (); iter++) {
    if (pt_set->Aliased(al, *iter))
      return TRUE;
  }   
  
  return FALSE;
}

// PU_POINTS_TO_SUMMARY_MGR as it name suggests, it manage 
// the PU_POINTS_TO_SUMMARYs in a clean way.
// 
// NOTE: PU_POINTS_TO_SUMMARY should be live through the 
// whole compilation of the the current src file. Therefor, 
// use MEM_src_pool for the allocation.
// 
class PU_POINTS_TO_SUMMARY_MGR {
private:
  ID_MAP<PU_POINTS_TO_SUMMARY*, UINT32> _st_idx_to_summary;
  typedef mempool_allocator<PU_POINTS_TO_SUMMARY*>  PU_SUM_ALLOC;
  vector<PU_POINTS_TO_SUMMARY*, PU_SUM_ALLOC> _summaries; 

public:
  PU_POINTS_TO_SUMMARY_MGR (void) : 
    _st_idx_to_summary(256/* init capacity*/, NULL, 
                       &MEM_src_pool, FALSE),
    _summaries (&MEM_src_pool) { _st_idx_to_summary.Init(); }
                       
  ~PU_POINTS_TO_SUMMARY_MGR (void) {};

  PU_POINTS_TO_SUMMARY* Get (ST* pu_st) { 
    FmtAssert (ST_class(pu_st) == CLASS_FUNC, 
               ("symbol '%s' is not function", ST_name(pu_st)));
    return _st_idx_to_summary.Lookup (ST_index(pu_st));
  }
  
  void Set (ST* pu_st, PU_POINTS_TO_SUMMARY* sum) {
    FmtAssert (ST_class(pu_st) == CLASS_FUNC, 
               ("symbol '%s' is not function", ST_name(pu_st)));
    _st_idx_to_summary.Insert (ST_index(pu_st), sum);
  }

  PU_POINTS_TO_SUMMARY* Allocate_PU_Points_To_Summary (ST* pu) {
    FmtAssert (ST_class(pu) == CLASS_FUNC, 
               ("symbol '%s' is not function", ST_name(pu)));
    PU_POINTS_TO_SUMMARY* t = 
      CXX_NEW(PU_POINTS_TO_SUMMARY(&MEM_src_pool), &MEM_src_pool);
    _summaries.push_back (t);       
    Set (pu, t);
    return t;
  }

  void Print (FILE* f);
};

static PU_POINTS_TO_SUMMARY_MGR* Pu_pt_summary_mgr = NULL;

PU_POINTS_TO_SUMMARY*
Allocate_PU_Points_To_Summary (ST* pu) {
  if (Pu_pt_summary_mgr == NULL) {
    Pu_pt_summary_mgr = CXX_NEW(PU_POINTS_TO_SUMMARY_MGR, &MEM_src_pool);
  }
  return Pu_pt_summary_mgr->Allocate_PU_Points_To_Summary (pu); 
}

PU_POINTS_TO_SUMMARY*
Allocate_PU_Points_To_Summary (void) {
  if (Pu_pt_summary_mgr == NULL) {
    Pu_pt_summary_mgr = CXX_NEW(PU_POINTS_TO_SUMMARY_MGR, &MEM_src_pool);
  }
  ST& st = St_Table[PU_Info_proc_sym(Current_PU_Info)];
  return Pu_pt_summary_mgr->Allocate_PU_Points_To_Summary (&st); 
}

PU_POINTS_TO_SUMMARY*
Get_points_to_summary (ST* pu) {
  if (Pu_pt_summary_mgr) 
    return Pu_pt_summary_mgr->Get (pu);
  return NULL;
}

PU_POINTS_TO_SUMMARY*
Get_points_to_summary (void) {
  ST& st = St_Table[PU_Info_proc_sym(Current_PU_Info)];
  return Get_points_to_summary (&st);
}

void
UNIFORM_NAME::Print (FILE* f) {
  fprintf (f, "[%-3d] ", _unique_id); 
  switch (_type) {
  case UN_NAMED_GLOBAL: 
    fprintf (f, "%s (global)", ST_name(_name.sym)); break;
  case UN_MALLOC:
    fprintf (f, "%llx (malloc)", (unsigned long long)_name.line_num); break;
  case UN_ALLOCA:
    fprintf (f, "%llx (alloca)", (unsigned long long)_name.line_num); break;
  case UN_FORMAL:
    fprintf (f, "%d (formal)", _name.idx); break;
  case UN_CALLER_LOCAL:
    fprintf (f, "%p (local)", _name.sym); break;
  default:
    fprintf (f, "unknown");
  }
}

void
UNAME_SPACE::Print (FILE* f, BOOL verbose) {

  // print the header 
  if (verbose) {
    fprintf (f, "G : global sym, M : malloc, A : alloca, F : "
                "formal idx, CL : caller local ST");
  }

  INT idx = 0;
  for (UNAME_VECTOR_ITER iter = _all_names.begin ();
       iter != _all_names.end (); iter++) {
    fprintf (f, "%d:", idx);
    (*iter)->Print(stderr);
    fprintf (f, "\n");
  }
}

void
PT_SET::Print (FILE* f) {

  fprintf (f, "ADDRSPACE:");
  switch (_addr_space) {
  case PTAS_INVALID: fprintf (f, "inval,"); break;
  case PTAS_UNKNOWN: fprintf (f, "unknown,"); break;
  case PTAS_TEXT:    fprintf (f, "text,"); break;
  case PTAS_GLOBAL:  fprintf (f, "global,"); break;
  case PTAS_FSTATIC: fprintf (f, "file static,"); break;
  case PTAS_LOCAL:   fprintf (f, "local,"); break;
  case PTAS_INVISIBLE: fprintf (f, "invisible,"); break;
  default:
    FmtAssert (FALSE, ("invalid address space"));
  };

  fprintf (f, "CERTAINTY:%s,", 
           (_certainty == PTC_POSSIBLE) ? "possible" : "definite");

  fprintf (f, "MAY/MUST:%s,", 
           (_may_or_must == PT_MAY_POINTS_TO) ? "may" : "must");

  if (_has_unknown_pt != 0)
    fprintf (f, "has unknown pt,");

  fprintf (f, "Pt as follows:\n");

  for (PT_VECTOR_ITER iter = _all_points_to.begin ();
       iter != _all_points_to.end (); iter++) {
    fprintf (f, "  -.");
    (*iter)->Print(f);
  }
}

void
PU_POINTS_TO_SUMMARY::Print (FILE* f) {

  fprintf (f, "Points-to hold at entry:\n");
  PT_SET_MGR& in = In_set ();     
  UNAME_VECTOR& in_names = in.Name_space()->All_names ();

  // loop over over names in the name space 
  for (UNAME_VECTOR_ITER iter = in_names.begin (); 
       iter != in_names.end (); iter++) {
    UNIFORM_NAME* name = *iter; 
    fprintf (f, "PTR-NAME: ");
    name->Print(f);
    fprintf (f, "\n");

    fprintf (f, "POINTS-TOs: ");
    PT_SET* pt_sets = in.Points_to_set (name);
    if (pt_sets)
      pt_sets->Print (f);
  }
  
  fprintf (f, "Points-to hold at exit:\n");
  PT_SET_MGR& out = Out_set ();     
  UNAME_VECTOR& out_names = out.Name_space()->All_names ();
  for (UNAME_VECTOR_ITER iter = out_names.begin (); 
       iter != out_names.end (); iter++) {
    UNIFORM_NAME* name = *iter; 
    fprintf (f, "PTR-NAME: ");
    name->Print(f);
    fprintf (f, "\n");

    fprintf (f, "POINTS-TOs: ");
    PT_SET* pt_sets = out.Points_to_set (name);
    if (pt_sets)
      pt_sets->Print (f);
  }
}

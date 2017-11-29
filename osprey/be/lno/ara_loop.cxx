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


// This may look like C code, but it is really -*- C++ -*-

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h> 
#include <limits.h>
#include <math.h>
#include "pu_info.h"
#include "lnopt_main.h"
#include "config_targ.h"
#include "glob.h"
#include "ara_loop.h"
#include "lwn_util.h"
#include "dep_graph.h"
#include "lnoutils.h"
#include "ff_utils.h"
#include "opt_alias_interface.h"
#include "opt_alias_mgr.h"
#include "opt_du.h"
#include "region_util.h"
#include "wn_pragmas.h"
#include "fusion.h"
#include "ara_utils.h"
#include "debug.h"
#include "lego_util.h"
#include "snl.h"
#include "snl_deps.h"
#include "snl_utils.h"
#include "parallel.h"
#include "const.h"
#include "targ_const.h"
#include "parmodel.h"
#include "config.h"
#include "stab.h"
#include <alloca.h>
#include "fiz_fuse.h"
#include "parids.h"
#include "call_info.h"
#include "cxx_hash.h"
#include "omp_lower.h"
#include "tlog.h"
#include "be_symtab.h"
#include "ipa_lno_read.h" 
#include "lnodriver.h" 
#include "ipa_lno_cost.h" 


extern INT Parallel_Debug_Level; 
extern ARRAY_DIRECTED_GRAPH16 *Array_Dependence_Graph; 
  // PU array dependence graph
extern REDUCTION_MANAGER *red_manager;  // PU reduction manager
extern ALIAS_MANAGER *Alias_Mgr; // Alias manager
extern DU_MANAGER *Du_Mgr; // DU manager
#ifdef KEY // bug 7772
#define MAX_PROCS (LNO_Num_Processors == 0 ? 8 : LNO_Num_Processors)
#else
const INT MAX_PROCS = 128;
#endif

ARA_LOOP_INFO::ARA_LOOP_INFO():
  _children(&ARA_memory_pool),
  _def(&ARA_memory_pool),
  _may_def(&ARA_memory_pool),
  _use(&ARA_memory_pool),
  _pri(&ARA_memory_pool),
  _scalar_def(&ARA_memory_pool),
  _scalar_use(&ARA_memory_pool),
  _scalar_pri(&ARA_memory_pool),
  _scalar_may_def(&ARA_memory_pool),
  _scalar_last_value(&ARA_memory_pool),
  _bad_alias(&ARA_memory_pool),
  _scalar_always_defined(&ARA_memory_pool),
  _scalar_vars(&ARA_memory_pool),
  _scalar_alias(&ARA_memory_pool),
  _scalar_no_final(&ARA_memory_pool),
  _dep_vars(&ARA_memory_pool),
  _dep_source(&ARA_memory_pool),
  _dep_sink(&ARA_memory_pool),
  _ln_dep_source(&ARA_memory_pool),
  _ln_dep_sink(&ARA_memory_pool),
  _scalar_bad_peel(&ARA_memory_pool),
  _ln_scalar_bad_peel(&ARA_memory_pool),
  _dep_bad_peel(&ARA_memory_pool),
  _ln_dep_bad_peel(&ARA_memory_pool),
  _partial_array_sec(&ARA_memory_pool),
  _call_no_dep_vars(&ARA_memory_pool),
  _ln_call_no_dep_vars(&ARA_memory_pool),
  _array_no_dep_vars(&ARA_memory_pool),
  _ln_array_no_dep_vars(&ARA_memory_pool),
  _ln_misc_no_dep_vars(&ARA_memory_pool),
  _reduction(&ARA_memory_pool)
{
}

ARA_LOOP_INFO::ARA_LOOP_INFO(WN* wn,
                             ARA_LOOP_INFO *p,
                             const BOOL inv):
  _children(&ARA_memory_pool),
  _def(&ARA_memory_pool),
  _may_def(&ARA_memory_pool),
  _use(&ARA_memory_pool),
  _pri(&ARA_memory_pool),
  _scalar_def(&ARA_memory_pool),
  _scalar_use(&ARA_memory_pool),
  _scalar_pri(&ARA_memory_pool),
  _scalar_may_def(&ARA_memory_pool),
  _scalar_last_value(&ARA_memory_pool),
  _bad_alias(&ARA_memory_pool),
  _scalar_always_defined(&ARA_memory_pool),
  _scalar_vars(&ARA_memory_pool),
  _scalar_alias(&ARA_memory_pool),
  _scalar_no_final(&ARA_memory_pool),
  _dep_vars(&ARA_memory_pool),
  _dep_source(&ARA_memory_pool),
  _dep_sink(&ARA_memory_pool),
  _ln_dep_source(&ARA_memory_pool),
  _ln_dep_sink(&ARA_memory_pool),
  _scalar_bad_peel(&ARA_memory_pool),
  _ln_scalar_bad_peel(&ARA_memory_pool),
  _dep_bad_peel(&ARA_memory_pool),
  _ln_dep_bad_peel(&ARA_memory_pool),
  _partial_array_sec(&ARA_memory_pool),
  _call_no_dep_vars(&ARA_memory_pool),
  _ln_call_no_dep_vars(&ARA_memory_pool),
  _array_no_dep_vars(&ARA_memory_pool),
  _ln_array_no_dep_vars(&ARA_memory_pool),
  _ln_misc_no_dep_vars(&ARA_memory_pool),
  _reduction(&ARA_memory_pool)
{
  _peel_value = -1;
  _is_good = TRUE;
  _dep_dist = 0;
  _parent = p;
  _loop  = wn;
  _live_use = NULL;
  _info = NULL;
  _do_stack = NULL;
  _invariant_symbols = CXX_NEW(STACK<WN*>(&ARA_memory_pool), &ARA_memory_pool);
  _processed = CXX_NEW(STACK<WN*>(&ARA_memory_pool), &ARA_memory_pool);
  _kernels = CXX_NEW(KERNEL_LIST, &ARA_memory_pool);
  _invariant = FALSE;
  _has_last_value_array = FALSE;
  _has_bad_region = FALSE;
  _inner_loop_is_suggested_parallel = FALSE;
  if (wn != NULL && WN_opcode(wn) == OPC_DO_LOOP){
    _info  = Get_Do_Loop_Info(wn);
    Is_True(_info,("ARA_LOOP_INFO::ARA_LOOP_INFO: No DO_LOOP_INFO"));
    _invariant = inv && _info && !_info->Has_Exits
      &&!_info->Has_Bad_Mem;
    _do_stack = CXX_NEW(DOLOOP_STACK(&ARA_memory_pool), &ARA_memory_pool);
    Build_Doloop_Stack(wn, _do_stack);
  }
}

ARA_LOOP_INFO::ARA_LOOP_INFO(ARA_LOOP_INFO* p):
  _children(&ARA_memory_pool),
  _reduction(&ARA_memory_pool),
  _def(&ARA_memory_pool), 
  _may_def(&ARA_memory_pool), 
  _use(&ARA_memory_pool), 
  _pri(&ARA_memory_pool), 
  _scalar_def(&ARA_memory_pool),
  _scalar_use(&ARA_memory_pool),
  _scalar_pri(&ARA_memory_pool),
  _scalar_may_def(&ARA_memory_pool),
  _scalar_last_value(&ARA_memory_pool),
  _bad_alias(&ARA_memory_pool),
  _scalar_always_defined(&ARA_memory_pool),
  _scalar_vars(&ARA_memory_pool),
  _scalar_alias(&ARA_memory_pool),
  _scalar_no_final(&ARA_memory_pool),
  _scalar_bad_peel(&ARA_memory_pool),
  _ln_scalar_bad_peel(&ARA_memory_pool),
  _dep_vars(&ARA_memory_pool),
  _dep_source(&ARA_memory_pool),
  _dep_sink(&ARA_memory_pool),
  _ln_dep_source(&ARA_memory_pool),
  _ln_dep_sink(&ARA_memory_pool),
  _dep_bad_peel(&ARA_memory_pool),
  _ln_dep_bad_peel(&ARA_memory_pool),
  _partial_array_sec(&ARA_memory_pool),
  _call_no_dep_vars(&ARA_memory_pool),
  _ln_call_no_dep_vars(&ARA_memory_pool),
  _array_no_dep_vars(&ARA_memory_pool),
  _ln_array_no_dep_vars(&ARA_memory_pool),
  _ln_misc_no_dep_vars(&ARA_memory_pool)
{
  INT i; 
  FmtAssert(p != NULL, ("ARA_LOOP_INFO: NULL pointer passed"));  
  _invariant = p->_invariant;
  for (i = 0; i < _children.Elements(); i++)
    _children.Push(p->_children.Bottom_nth(i));
  _parent = p->_parent;
  _loop = p->_loop;
  _info = p->_info;
  _kernels = NULL; 
  KERNEL_SLIST_ITER kiter(p->_kernels);
  for (KERNEL_IMAGE* k = kiter.First(); !kiter.Is_Empty(); k = kiter.Next()) 
    _kernels->Append(CXX_NEW(KERNEL_IMAGE(k), &ARA_memory_pool));
  _do_stack = NULL; 
  if (p->_do_stack != NULL) { 
    _do_stack = CXX_NEW(DOLOOP_STACK(&ARA_memory_pool), &ARA_memory_pool);
    for (i = 0; i < p->_do_stack->Elements(); i++)
      _do_stack->Push(p->_do_stack->Bottom_nth(i));
  } 
  _invariant_symbols = CXX_NEW(STACK<WN*>(&ARA_memory_pool), &ARA_memory_pool);
  if (p->_invariant_symbols != NULL)
    for (i = 0; i < p->_invariant_symbols->Elements(); i++)
      _invariant_symbols->Push(p->_invariant_symbols->Bottom_nth(i));
  _processed = CXX_NEW(STACK<WN*>(&ARA_memory_pool), &ARA_memory_pool);
  if (p->_processed != NULL)
    for (i = 0; i < p->_processed->Elements(); i++)
      _processed->Push(p->_processed->Bottom_nth(i));
  for (i = 0; i < p->_reduction.Elements(); i++)
    _reduction.Push(p->_reduction.Bottom_nth(i));
  _inner_loop_is_suggested_parallel = p->_inner_loop_is_suggested_parallel;
  _has_bad_region = p->_has_bad_region;
  for (i = 0; i < p->_def.Elements(); i++)
    _def.Push(p->_def.Bottom_nth(i));
  for (i = 0; i < p->_may_def.Elements(); i++)
    _may_def.Push(p->_may_def.Bottom_nth(i));
  for (i = 0; i < p->_use.Elements(); i++)
    _use.Push(p->_use.Bottom_nth(i));
  for (i = 0; i < p->_pri.Elements(); i++)
    _pri.Push(p->_pri.Bottom_nth(i));
  for (i = 0; i < p->_scalar_def.Elements(); i++) 
    _scalar_def.Add_Scalar_Node(p->_scalar_def.Bottom_nth(i));
  for (i = 0; i < p->_scalar_use.Elements(); i++) 
    _scalar_use.Add_Scalar_Node(p->_scalar_use.Bottom_nth(i));
  for (i = 0; i < p->_scalar_pri.Elements(); i++) 
    _scalar_pri.Add_Scalar_Node(p->_scalar_pri.Bottom_nth(i));
  for (i = 0; i < p->_scalar_may_def.Elements(); i++) 
    _scalar_may_def.Add_Scalar_Node(p->_scalar_may_def.Bottom_nth(i));
  for (i = 0; i < p->_scalar_last_value.Elements(); i++)
    _scalar_last_value.Push(p->_scalar_last_value.Bottom_nth(i));
  for (i = 0; i < p->_bad_alias.Elements(); i++)
    _bad_alias.Push(p->_bad_alias.Bottom_nth(i));
  for (i = 0; i < p->_scalar_always_defined.Elements(); i++)
    _scalar_always_defined.Push(p->_scalar_always_defined.Bottom_nth(i));
  _dep_dist = p->_dep_dist;
  _is_good = p->_is_good;
  _has_last_value_array = p->_has_last_value_array;
  _peel_value = p->_peel_value;
  for (i = 0; i < p->_scalar_vars.Elements(); i++)
    _scalar_vars.Push(p->_scalar_vars.Bottom_nth(i));
  for (i = 0; i < p->_scalar_alias.Elements(); i++)
    _scalar_alias.Push(p->_scalar_alias.Bottom_nth(i));
  for (i = 0; i < p->_scalar_no_final.Elements(); i++)
    _scalar_no_final.Push(p->_scalar_no_final.Bottom_nth(i));
  for (i = 0; i < p->_scalar_bad_peel.Elements(); i++)
    _scalar_bad_peel.Push(p->_scalar_bad_peel.Bottom_nth(i));
  for (i = 0; i < p->_ln_scalar_bad_peel.Elements(); i++)
    _ln_scalar_bad_peel.Push(p->_ln_scalar_bad_peel.Bottom_nth(i));
  for (i = 0; i < p->_dep_vars.Elements(); i++)
    _dep_vars.Push(p->_dep_vars.Bottom_nth(i));
  for (i = 0; i < p->_dep_source.Elements(); i++)
    _dep_source.Push(p->_dep_source.Bottom_nth(i));
  for (i = 0; i < p->_dep_sink.Elements(); i++)
    _dep_sink.Push(p->_dep_sink.Bottom_nth(i));
  for (i = 0; i < p->_ln_dep_source.Elements(); i++)
    _ln_dep_source.Push(p->_ln_dep_source.Bottom_nth(i));
  for (i = 0; i < p->_ln_dep_sink.Elements(); i++)
    _ln_dep_sink.Push(p->_ln_dep_sink.Bottom_nth(i));
  for (i = 0; i < p->_dep_bad_peel.Elements(); i++)
    _dep_bad_peel.Push(p->_dep_bad_peel.Bottom_nth(i));
  for (i = 0; i < p->_ln_dep_bad_peel.Elements(); i++)
    _ln_dep_bad_peel.Push(p->_ln_dep_bad_peel.Bottom_nth(i));
  for (i = 0; i < p->_partial_array_sec.Elements(); i++)
    _partial_array_sec.Push(p->_partial_array_sec.Bottom_nth(i));
  for (i = 0; i < p->_call_no_dep_vars.Elements(); i++)
    _call_no_dep_vars.Push(p->_call_no_dep_vars.Bottom_nth(i));
  for (i = 0; i < p->_ln_call_no_dep_vars.Elements(); i++)
    _ln_call_no_dep_vars.Push(p->_ln_call_no_dep_vars.Bottom_nth(i));
  for (i = 0; i < p->_array_no_dep_vars.Elements(); i++)
    _array_no_dep_vars.Push(p->_array_no_dep_vars.Bottom_nth(i));
  for (i = 0; i < p->_ln_array_no_dep_vars.Elements(); i++)
    _ln_array_no_dep_vars.Push(p->_ln_array_no_dep_vars.Bottom_nth(i));
  for (i = 0; i < p->_ln_misc_no_dep_vars.Elements(); i++)
    _ln_misc_no_dep_vars.Push(p->_ln_misc_no_dep_vars.Bottom_nth(i));
  _live_use = NULL;
  if (p->_live_use != NULL) { 
    HASH_TABLE_ITER<ST*,BOOL> hiter(p->_live_use);
    ST* st; 
    BOOL b;
    while (hiter.Step(&st, &b)) 
      _live_use->Enter(st, b);
  } 
}

void ARA_LOOP_INFO::Copy_Some_Values(ARA_LOOP_INFO *p)
{
  _peel_value = p->_peel_value;
  _is_good = p->_is_good;
  _dep_dist = 0;
  _parent = p;
  _loop  = p->_loop;
  _live_use = p->_live_use;
  _info = p->_info;
  _do_stack = p->_do_stack;
  _invariant_symbols = p->_invariant_symbols;
  _processed = p->_processed;
  _kernels = p->_kernels;
  _invariant = p->_invariant;
  _has_last_value_array = p->_has_last_value_array;
  _has_bad_region = p->_has_bad_region;
  _inner_loop_is_suggested_parallel = p->_inner_loop_is_suggested_parallel;
  _info = p->_info;
  _do_stack = p->_do_stack;
}

ARA_LOOP_INFO::~ARA_LOOP_INFO()
{
  if (_live_use) 
    CXX_DELETE(_live_use, &ARA_memory_pool);
}

KERNEL_IMAGE::KERNEL_IMAGE(KERNEL_IMAGE* k)
{
  INT i;
  _kernel = CXX_NEW(ACCESS_ARRAY(k->_kernel, &ARA_memory_pool), 
    &ARA_memory_pool);
  _region = CXX_NEW(REGION(k->_region), &ARA_memory_pool); 
  _depth = k->_depth;
  _projected_level = k->_projected_level;
  _decoupled = k->_decoupled;
  _is_independent = CXX_NEW_ARRAY(BOOL, _depth, &ARA_memory_pool);
  for (i = 0; i < _depth; i++)
    _is_independent[i] = k->_is_independent[i]; 
  _changed = CXX_NEW_ARRAY(BOOL, _kernel->Num_Vec(), &ARA_memory_pool);
  for (i = 0; i < _depth; i++)
    _changed[i] = k->_changed[i];
} 

KERNEL_IMAGE::KERNEL_IMAGE(const ACCESS_ARRAY * a, ARA_LOOP_INFO *ara_info)
{

  _kernel = (ACCESS_ARRAY *) a; 
  _region = NULL;
  _depth = ara_info->Depth()+1;
  _projected_level= _depth+1;
  _decoupled = TRUE;
//  _changed = CXX_NEW_ARRAY(BOOL, a->Num_Vec(), &ARA_memory_pool);
  _is_independent = CXX_NEW_ARRAY(BOOL, _depth, &ARA_memory_pool);

  INT i;
  for (i=0; i<_depth; ++i)
    _is_independent[i]=TRUE;
  
  for (i=0; i<a->Num_Vec(); ++i) 
    for (INT j=0; j<_depth; ++j)
      if (a->Dim(i)->Loop_Coeff(j)!=0) _is_independent[j]=FALSE;

}

KERNEL_IMAGE::KERNEL_IMAGE(const ACCESS_ARRAY * a)
{

  _kernel = (ACCESS_ARRAY *) a; 
  _region = NULL;
  if (a->Num_Vec() >= 1)
    _depth = a->Dim(0)->Nest_Depth();
  else 
    _depth = -1;
  _projected_level= _depth+1;
  _decoupled = TRUE;
  _is_independent = CXX_NEW_ARRAY(BOOL, _depth, &ARA_memory_pool);
  INT i;
  for (i=0; i<_depth; ++i)
    _is_independent[i]=TRUE;
  
  for (i=0; i<a->Num_Vec(); ++i) 
    for (INT j=0; j<_depth; ++j)
      if (a->Dim(i)->Loop_Coeff(j)!=0) _is_independent[j]=FALSE;

}

// ***************************************************************************
//
// This method builds the image of the region accessed by the
// kernel matrix within the iteration space. The loop_info is
// for the inner most loop enclosing the kernel.
//
// ***************************************************************************
// void KERNEL_IMAGE::Build_Region(ARA_LOOP_INFO &loop_info)
// {  
//
//   DevWarn("TODO: implementing KERNEL_IMAGE::Build_Region");
// 
// }    

BOOL 
Is_Same_Array(const SYMBOL &a, INT32 offset_a, const SYMBOL &b, INT32 offset_b)
{
  return ((a.St()==b.St()) && (a.WN_Offset()==b.WN_Offset()) &&
	  (offset_a == offset_b));
}

BOOL ARA_REF::Has_Formal_Parameter()
{
  return _image.Has_Formal_Parameter();
} 

void
ARA_REF::Set_Whole_Array(BOOL set_invariant)
{
  ST* st = _array->St();

  TY_IDX ty = ST_type(st);

  if ((ST_sclass(st) == SCLASS_FORMAL) && (TY_kind(ty) == KIND_POINTER) ||
      (ST_sclass(st) == SCLASS_PSTATIC || ST_sclass(st) == SCLASS_AUTO ||
       (ST_base(st) != st && ST_sclass(ST_base(st)) == SCLASS_AUTO)) &&
      (TY_kind(ty) == KIND_POINTER))
    ty = TY_pointed(ty);

  if (TY_kind(ty) == KIND_ARRAY) {
    INT ndims = TY_AR_ndims(ty);
    if (ndims == 0) return;
    INT i;
    for (i = 0; i < ndims; ++i) {
      if (!TY_AR_const_lbnd(ty, i))
        return;
    }

    REGION *found = NULL;
    REGION_ITER iter(&_image);
    for (REGION* cur = iter.First(); !iter.Is_Empty(); cur = iter.Next()) {
      if (cur->_dim == ndims && cur->_type == ARA_NORMAL) {
	for (i = 0; i < ndims; ++i) {
	  if (cur->_axle[i].up == NULL) {
	    break;
	  } else {
	    INT64 lb = TY_AR_lbnd_val(ty,i);
	    INT64 ub = TY_AR_ubnd_val(ty,i);
	    if (PU_src_lang(Get_Current_PU()) == PU_F77_LANG ||
		PU_src_lang(Get_Current_PU()) == PU_F90_LANG) {
	      lb = lb - 1;
	      ub = ub - 1;
	    }
	    INT64 lb1 = cur->_axle[i].lo->Access_Vector()->Const_Offset;
	    INT64 ub1 = cur->_axle[i].up->Access_Vector()->Const_Offset;
	  
	    if (! (cur->_axle[i].step == 1 &&
		   cur->_axle[i].lo->Coeff() == NULL &&
		   cur->_axle[i].lo->Access_Vector()->Is_Const() &&
		   lb1 == lb &&
		   cur->_axle[i].up->Coeff() == NULL &&
		   cur->_axle[i].up->Access_Vector()->Is_Const() &&
		   ub1 == ub ) )
	      break;
	  }
	}
	if (i==ndims) {
	  found = cur;
	  found->_type = ARA_TOP;
	  break;
	}
      }
    }

    if (found) {
      if (set_invariant) Set_Donot_Care_Invariant();
      REGION *prev = NULL;
      REGION *cur = iter.First();
      while (! iter.Is_Empty() ) {
        if (cur != found) {
          for (i = 0; i < cur->_wn_list.Elements(); ++i)
            found->_wn_list.Push(cur->_wn_list.Bottom_nth(i));
          CXX_DELETE(_image.Remove(prev,cur), &ARA_memory_pool);
        }
        prev = cur;
        cur = iter.Next();
      }
    }

  }
}

//============================================================================
//
// Note that ARA_REF *a is consumed
//
//============================================================================
void 
ARA_REF::Add_Ref(ARA_REF *a, const ARA_LOOP_INFO &ali)
{

  Is_True(Is_Same_Array(*_array,_offset,*a->_array,a->_offset), 
	  ("ARA_REF::Add_Ref: Try to merge different arrays"));

  while (!a->_image.Is_Empty()) {
    REGION * a_cur = a->_image.Remove_Headnode();
    _image.Add_Region(a_cur, ali);
  }

  CXX_DELETE(a,&ARA_memory_pool);

}

void
ARA_REF::Print(FILE *fp) const
{
  if (_array) {
    _array->Print(fp);
    if (_has_bad_alias) fprintf(fp,"Has bad alias\n");
    _image.Print(fp);
  }
}

void
ARA_REF::WB_Print(FILE *fp) const
{
  char bf[MAX_TLOG_CHARS]; 
  WB_Print(bf, 0);
  fprintf(fp, "%s", bf);
}

INT 
ARA_REF::WB_Print(char* bf, INT ccount) const
{
  INT new_ccount = ccount;
  if (_array) {
    new_ccount = _array->Print(bf, new_ccount);
    if (_has_bad_alias) 
      new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, 
        "(Has bad alias)");
    new_ccount = _image.WB_Print(bf, new_ccount);
  }
  return new_ccount;
}

void
ARA_REF::Print_Analysis_Info(FILE *fp, INT indent, DOLOOP_STACK &do_stack)
{

  if (_array) {
    fprintf(fp,"%s (\n", _array->Name());
    _image.Print_Analysis_Info(fp, indent+3, do_stack);
    print_indent(fp, indent);
    fprintf(fp,")\n");
  }

}

void
ARA_LOOP_INFO::Print(FILE *fp, BOOL terse) const 
{
  if (!terse || _def.Elements() > 0 || _scalar_def.Elements() > 0) { 
    fprintf(fp,"DEF: \n");
    for (INT i = 0; i < _def.Elements(); ++i) {
      _def.Bottom_nth(i)->Print(fp);
    }
  } 
  if (!terse || _scalar_def.Elements() > 0) { 
    for (INT i = 0; i < _scalar_def.Elements(); ++i) {
      SCALAR_STACK & stk = (SCALAR_STACK &) _scalar_def;
      stk.Bottom_nth(i)->_scalar.Print(fp);
      fprintf(fp,"\n");
    }
  } 
  if (!terse || _may_def.Elements() > 0 || _scalar_may_def.Elements() > 0) { 
    fprintf(fp,"May DEF: \n");
    for (INT i = 0; i < _may_def.Elements(); ++i) {
      _may_def.Bottom_nth(i)->Print(fp);
    }
  } 
  if (!terse || _scalar_may_def.Elements() > 0) { 
    for (INT i = 0; i < _scalar_may_def.Elements(); ++i) {
      SCALAR_STACK & stk = (SCALAR_STACK &) _scalar_may_def;
      stk.Bottom_nth(i)->_scalar.Print(fp);
      fprintf(fp,"\n");
    }
  } 
  if (!terse || _use.Elements() > 0 || _scalar_use.Elements() > 0) { 
    fprintf(fp,"USE: \n");
    for (INT i = 0; i < _use.Elements(); ++i) {
      _use.Bottom_nth(i)->Print(fp);
    }
  }
  if (!terse || _scalar_use.Elements() > 0) {  
    for (INT i = 0; i < _scalar_use.Elements(); ++i) {
      SCALAR_STACK & stk = (SCALAR_STACK &) _scalar_use;
      stk.Bottom_nth(i)->_scalar.Print(fp);
      fprintf(fp,"\n");
    }
  } 
  if (!terse || _pri.Elements() > 0 || _scalar_pri.Elements() > 0) { 
    fprintf(fp,"PRI: \n");
    for (INT i = 0; i < _pri.Elements(); ++i) {
      _pri.Bottom_nth(i)->Print(fp);
      if (_pri.Bottom_nth(i)->Is_Loop_Invariant())
	fprintf(fp, "Loop Invariant\n");
      if (_pri.Bottom_nth(i)->Need_Last_Value())
	fprintf(fp,"Need Last Value\n");
      if (_pri.Bottom_nth(i)->Is_Unknown_Size())
	fprintf(fp,"Unknown Size\n");
    }
  } 
  if (!terse || _scalar_pri.Elements() > 0) {
    for (INT i = 0; i < _scalar_pri.Elements(); ++i) {
      SCALAR_STACK & stk = (SCALAR_STACK &) _scalar_pri;
      stk.Bottom_nth(i)->_scalar.Print(fp);
      if (_scalar_last_value.Elements()>i && _scalar_last_value.Bottom_nth(i))
	fprintf(fp,"Need Last Value\n");
      fprintf(fp,"\n");
    }
  } 
}

void 
ARA_LOOP_INFO::WB_Print(FILE *fp, BOOL terse) const
{
  if (_has_last_value_array)
    fprintf(fp, "HAS LAST VALUE ARRAY\n");
  if (!terse || _def.Elements() > 0 || _scalar_def.Elements() > 0) { 
    fprintf(fp,"MUST DEFS: \n");
    for (INT i = 0; i < _def.Elements(); ++i) {
      fprintf(fp, "  "); 
      _def.Bottom_nth(i)->WB_Print(fp);
    }
  } 
  if (!terse || _scalar_def.Elements() > 0) { 
    fprintf(fp, "  Scalars: "); 
    for (INT i = 0; i < _scalar_def.Elements(); ++i) {
      SCALAR_STACK & stk = (SCALAR_STACK &) _scalar_def;
      stk.Bottom_nth(i)->_scalar.Print(fp);
      if (!stk.Bottom_nth(i)->_scalar.Is_Formal()
	  && ST_class(stk.Bottom_nth(i)->_scalar.St()) == CLASS_PREG)
	fprintf(fp, "<PREG>");
      if (i < _scalar_def.Elements() - 1)
	fprintf(fp, ","); 
    }
    fprintf(fp,"\n");
  } 
  if (!terse || _may_def.Elements() > 0 || _scalar_may_def.Elements() > 0) { 
    fprintf(fp,"MAY DEFS: \n");
    for (INT i = 0; i < _may_def.Elements(); ++i) {
      fprintf(fp, "  "); 
      _may_def.Bottom_nth(i)->WB_Print(fp);
    }
  } 
  if (!terse || _scalar_may_def.Elements() > 0) { 
    fprintf(fp, "  Scalars: "); 
    for (INT i = 0; i < _scalar_may_def.Elements(); ++i) {
      SCALAR_STACK & stk = (SCALAR_STACK &) _scalar_may_def;
      stk.Bottom_nth(i)->_scalar.Print(fp);
      if (!stk.Bottom_nth(i)->_scalar.Is_Formal()
	  && ST_class(stk.Bottom_nth(i)->_scalar.St()) == CLASS_PREG)
	fprintf(fp, "<PREG>");
      if (i < _scalar_may_def.Elements() - 1)
	fprintf(fp, ","); 
    }
    fprintf(fp,"\n");
  } 
  if (!terse || _use.Elements() > 0 || _scalar_use.Elements() > 0) { 
    fprintf(fp,"USES: \n");
    for (INT i = 0; i < _use.Elements(); ++i) {
      fprintf(fp, "  "); 
      _use.Bottom_nth(i)->WB_Print(fp);
    }
  } 
  if (!terse || _scalar_use.Elements() > 0) {
    fprintf(fp, "  Scalars: "); 
    for (INT i = 0; i < _scalar_use.Elements(); ++i) {
      SCALAR_STACK & stk = (SCALAR_STACK &) _scalar_use;
      stk.Bottom_nth(i)->_scalar.Print(fp);
      if (!stk.Bottom_nth(i)->_scalar.Is_Formal() 
	  && ST_class(stk.Bottom_nth(i)->_scalar.St()) == CLASS_PREG)
	fprintf(fp, "<PREG>");
      if (i < _scalar_use.Elements() - 1)
	fprintf(fp, ","); 
    }
    fprintf(fp,"\n");
  } 
  if (!terse || _pri.Elements() > 0 || _scalar_pri.Elements() > 0) { 
    fprintf(fp,"PRIVATES: \n");
    for (INT i = 0; i < _pri.Elements(); ++i) {
      fprintf(fp, "  "); 
      _pri.Bottom_nth(i)->WB_Print(fp);
      if (_pri.Bottom_nth(i)->Is_Loop_Invariant())
	fprintf(fp, "    Loop Invariant\n");
      if (_pri.Bottom_nth(i)->Need_Last_Value())
	fprintf(fp,"    Need Last Value\n");
    }
  } 
  if (!terse || _scalar_pri.Elements() > 0) { 
    fprintf(fp, "  Scalars: "); 
    for (INT i = 0; i < _scalar_pri.Elements(); ++i) {
      SCALAR_STACK & stk = (SCALAR_STACK &) _scalar_pri;
      stk.Bottom_nth(i)->_scalar.Print(fp);
      if (_scalar_last_value.Elements()>i && _scalar_last_value.Bottom_nth(i))
	fprintf(fp," (Need Last Value)");
      if (!stk.Bottom_nth(i)->_scalar.Is_Formal()
	  && ST_class(stk.Bottom_nth(i)->_scalar.St()) == CLASS_PREG)
	fprintf(fp, "<PREG>");
      if (i < _scalar_pri.Elements() - 1)
	fprintf(fp, ","); 
    }
    fprintf(fp,"\n");
  } 
}

void ARA_LOOP_INFO::CI_Print(FILE* fp)
{ 
  INT i; 
  for (i = 0; i < _may_def.Elements(); i++) {
    fprintf(fp, "  ");
    fprintf(fp, "MOD Array   ");
    _may_def.Bottom_nth(i)->WB_Print(fp);
  } 
  for (i = 0; i < _scalar_may_def.Elements(); i++) {
    fprintf(fp, "  ");
    fprintf(fp, "MOD Scalar  ");
    SCALAR_STACK& stk = (SCALAR_STACK&) _scalar_may_def;
    TYPE_ID type_id = stk.Bottom_nth(i)->_scalar.Type;
    fprintf(fp, "%s ", MTYPE_name(type_id));
    stk.Bottom_nth(i)->_scalar.Print(fp);
    fprintf(fp, "\n");
  }
  for (i = 0; i < _use.Elements(); i++) {
    fprintf(fp, "  ");
    fprintf(fp, "REF Array   ");
    _use.Bottom_nth(i)->WB_Print(fp);
  } 
  for (i = 0; i < _scalar_use.Elements(); i++) {
    fprintf(fp, "  ");
    fprintf(fp, "REF Scalar  ");
    SCALAR_STACK& stk = (SCALAR_STACK&) _scalar_use;
    TYPE_ID type_id = stk.Bottom_nth(i)->_scalar.Type;
    fprintf(fp, "%s ", MTYPE_name(type_id));
    stk.Bottom_nth(i)->_scalar.Print(fp);
    fprintf(fp, "\n");
  }
} 

void ARA_LOOP_INFO::Tlog_CI_Print()
{ 
  INT i; 
  char bf[MAX_TLOG_CHARS];
  for (i = 0; i < _may_def.Elements(); i++) {
    INT new_ccount = 0;
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "MOD Array   ");
    new_ccount = _may_def.Bottom_nth(i)->WB_Print(bf, new_ccount);
    Generate_Tlog("LNO", "Call_Info", (SRCPOS) 0, "", "", "", bf);
  } 
  for (i = 0; i < _scalar_may_def.Elements(); i++) {
    INT new_ccount = 0;
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "MOD Scalar  ");
    SCALAR_STACK& stk = (SCALAR_STACK&) _scalar_may_def;
    new_ccount = stk.Bottom_nth(i)->_scalar.Print(bf, new_ccount);
    Generate_Tlog("LNO", "Call_Info", (SRCPOS) 0, "", "", "", bf);
  }
  for (i = 0; i < _use.Elements(); i++) {
    INT new_ccount = 0;
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "REF Array   ");
    new_ccount = _use.Bottom_nth(i)->WB_Print(bf, new_ccount);
    Generate_Tlog("LNO", "Call_Info", (SRCPOS) 0, "", "", "", bf);
  } 
  for (i = 0; i < _scalar_use.Elements(); i++) {
    INT new_ccount = 0;
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "REF Scalar  ");
    SCALAR_STACK& stk = (SCALAR_STACK&) _scalar_use;
    new_ccount = stk.Bottom_nth(i)->_scalar.Print(bf, new_ccount);
    Generate_Tlog("LNO", "Call_Info", (SRCPOS) 0, "", "", "", bf);
  }
} 

//-----------------------------------------------------------------------
// NAME: Invariant_Loop_Count
// FUNCTION: For the SNL whose outermost loop is 'wn_outer', return the 
//   number of loops in the invariant subnest of that SNL containing 
//   'wn_outer'. 
//-----------------------------------------------------------------------

static INT Invariant_Loop_Count(WN* wn_outer) 
{ 
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT nloops = SNL_Loop_Count(wn_outer); 
  INT* permutation = CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool);
  INT i;
  for (i = 0; i < nloops; i++)
    permutation[i] = i;
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  INT inner_depth = Do_Loop_Depth(wn_inner);
  for (i = 2; i <= nloops; i++) { 
    if (!General_Permutation(wn_outer, permutation, i))
      return i - 1;
    for (INT j = 0; j < i - 1; j++)  
      if (!SNL_Is_Invariant(&stack, outer_depth + j, outer_depth + i - 1))
        return i - 1; 
  }
  return nloops; 
} 

//-----------------------------------------------------------------------
// NAME: Single_Iteration_Cost
// FUNCTION: Return aan estimate of the cost of executing a single iteration
//   of a loop 'wn_loop'.  If 'include_calls' is TRUE, include a constant
//   estimate for any calls in the loop, otherwise count the cost of calls
//   as 0. 
//-----------------------------------------------------------------------

static double Single_Iteration_Cost(WN* wn_loop,
				    BOOL include_calls)
{ 
  double mach_cost_per_iter = 0, est_num_iters;
  INT nloops = SNL_Loop_Count(wn_loop); 
  double machine_cycles = SNL_Machine_Cost(wn_loop, nloops, 0, NULL,
      &mach_cost_per_iter, include_calls);
  INT *ident_perm = CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool);
  for (INT i = 0; i < nloops; i++)
    ident_perm[i] = i;
  double cache_cycles = SNL_Cache_Cost(wn_loop, ident_perm, nloops,
    Do_Loop_Depth(wn_loop), -1, NULL, &est_num_iters);
  CXX_DELETE_ARRAY(ident_perm, &LNO_local_pool);
  return Compute_Work_Estimate(mach_cost_per_iter, cache_cycles/est_num_iters);
} 

//-----------------------------------------------------------------------
// NAME: Const_Work_Estimate
// FUNCTION: Returns either a constant valued estimate of the serial work 
//   which would be used in the new form of the IF clause for automatic 
//   parallelization, or a lower bound of that estimate.  If a constant 
//   estimate is computed 'minimum_only' set to TRUE, otherwise it is set
//   to FALSE. 
//-----------------------------------------------------------------------

float ARA_LOOP_INFO::Const_Work_Estimate(WN* wn_loop, 
			                 BOOL* minimum_only)
{
  INT nloops = SNL_Loop_Count(wn_loop); 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  float const_work_estimate = (INT) (dli->Has_Calls 
    ? Single_Iteration_Cost(wn_loop, FALSE) : dli->Work_Estimate);
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_loop, nloops);
  INT inv_nloops = Invariant_Loop_Count(wn_loop);   
  INT var_nloops = nloops - inv_nloops; 
  INT count = 0; 
  BOOL min_only = FALSE; 
  if (dli->Has_Calls)
    min_only = TRUE;
  for (WN* wn = wn_inner; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      if (count++ < var_nloops
	  || !Upper_Bound_Standardize(WN_end(wn), TRUE)) {
        DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
        const_work_estimate *= dli->Est_Num_Iterations;
      } else { 
	WN* wn_trip_count = Trip_Count(wn);
        if (WN_operator(wn_trip_count) != OPR_INTCONST) {
#ifdef KEY
	  if (Cur_PU_Feedback && LNO_Apo_use_feedback &&
	      Cur_PU_Feedback->Get_index_loop(wn) != 0) {
	    FB_Info_Loop loop_freq = Cur_PU_Feedback->Query_loop(wn);
	    const_work_estimate *= loop_freq.freq_iterate.Value();
	    if (loop_freq.freq_positive.Value() > 0.0)
	      const_work_estimate /= loop_freq.freq_positive.Value();
	  }
	  else
#endif
	  min_only = TRUE; 
	} else {  
          const_work_estimate *= WN_const_val(wn_trip_count);     
	} 
	LWN_Delete_Tree(wn_trip_count); 
      }
    }
    if (wn == wn_loop)
      break; 
  }
  *minimum_only = min_only; 
  return const_work_estimate; 
}

//-----------------------------------------------------------------------
// NAME: Not_Enough_Parallel_Work
// FUNCTION: Return TRUE if we know that the new form IF condition for 
//   automatic parallelization of loop 'wn_outer' will always be FALSE, 
//   return FALSE if there is enough or if we are not sure.  
//-----------------------------------------------------------------------

BOOL ARA_LOOP_INFO::Not_Enough_Parallel_Work() 
{
  if (LNO_Run_AP == 2)
    return FALSE; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(_loop);
  if (dli->Has_Calls && dli->Has_Unsummarized_Call_Cost) 
    return FALSE; 
  BOOL minimum_only = FALSE; 
  for (WN* wn = _loop; wn != NULL; wn = LWN_Get_Parent(wn)) { 
    if (WN_opcode(wn) == OPC_DO_LOOP) { 
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
      if (dli->Not_Enough_Parallel_Work)
	return TRUE; 
    } 
  } 
  float const_work_estimate = Const_Work_Estimate(_loop, &minimum_only);
  if (const_work_estimate == -1 || minimum_only)
    return FALSE;
  float tc = Tc_Parallel_Cost();
  float tp = Tp_Parallel_Cost();
  float W = const_work_estimate;
  float p_min = (W - tc) / (2 * tp);
  float f_min = tp * p_min * p_min + (tc - W) * p_min + W;
  if (f_min >= 0)
    return TRUE;
  float p_left = ((W - tc) - sqrt((tc - W) * (tc - W) - 4 * tp * W))/(2 * tp);
  float p_right = ((W - tc) + sqrt((tc - W) * (tc - W) - 4 * tp * W))/(2 * tp);
  if (p_left > (float) MAX_PROCS || p_right < (float) 1)
    return TRUE;
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Always_Enough_Parallel_Work
// FUNCTION: Return TRUE if we know that the new form IF condition for 
//   automatic parallelization of loop 'wn_outer' will always be TRUE, 
//   return FALSE if there is not enough or if we are not sure.  
//-----------------------------------------------------------------------

BOOL ARA_LOOP_INFO::Always_Enough_Parallel_Work(BOOL* has_left_right,
                                                INT* left,
                                                INT* right)
{
  *has_left_right = FALSE;
  if (LNO_Run_AP == 2)
    return TRUE; 
  BOOL minimum_only = FALSE; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(_loop);
  if (dli->Is_Concurrent_Call && dli->Has_Unsummarized_Call_Cost)
    return TRUE; 
  if (dli->Has_Calls && dli->Has_Unsummarized_Call_Cost)
    return TRUE; 
  float const_work_estimate = Const_Work_Estimate(_loop, &minimum_only);
  if (const_work_estimate == -1 || minimum_only) 
    return FALSE; 
  float W = const_work_estimate;
  float tc = Tc_Parallel_Cost();
  float tp = Tp_Parallel_Cost();
  float p_min = (W - tc) / (2 * tp);
  float f_min = tp * p_min * p_min + (tc - W) * p_min + W;
  if (f_min >= 0.0)
    return FALSE;
  *has_left_right = TRUE;
  float p_left = ((W - tc) - sqrt((tc - W) * (tc - W) - 4 * tp * W))/(2 * tp);
  float p_right = ((W - tc) + sqrt((tc - W) * (tc - W) - 4 * tp * W))/(2 * tp);
  *left = (INT) ceil(p_left);
  *right = (INT) p_right;
  return (p_left <= 1 && p_right >= MAX_PROCS);
}

void
ARA_LOOP_INFO::Print_Analysis_Info()
{
  INT i;
  for (i = 0; i < _children.Elements(); ++i) 
    _children.Bottom_nth(i)->Print_Analysis_Info();

  INT indent = 0;
  fprintf(LNO_Analysis,"(LNO_ARA_Info\n");
  indent += 3;
  print_indent(LNO_Analysis,indent);

  if (Is_Parallel()) 
    fprintf(LNO_Analysis,"(%s %d %s)\n",
	    Cur_PU_Name, Srcpos_To_Line(WN_Get_Linenum(_loop)), "PARALLEL");
  else {
    fprintf(LNO_Analysis,"(%s %d %s)\n",
	    Cur_PU_Name, Srcpos_To_Line(WN_Get_Linenum(_loop)), "SEQUENTIAL");
    for (i = 0; i < _dep_vars.Elements(); ++i) {
      print_indent(LNO_Analysis,indent);
      fprintf(LNO_Analysis,"(%s %s)\n",
	      "DEPENDENCE_VAR",_dep_vars.Bottom_nth(i).Name());
    }
  }
  
  for (i = 0; i < _pri.Elements(); ++i) {
    print_indent(LNO_Analysis, indent);
    if (_pri.Bottom_nth(i)->Need_Last_Value())
      fprintf(LNO_Analysis,"(LAST_LOCAL_ARRAY ");
    else
      fprintf(LNO_Analysis,"(LOCAL_ARRAY ");
    _pri.Bottom_nth(i)->Print_Analysis_Info(LNO_Analysis,indent+3,Do_Stack());
    print_indent(LNO_Analysis, indent);
    fprintf(LNO_Analysis,")\n");
  }

  for (i = 0; i < _scalar_pri.Elements(); ++i) {
    print_indent(LNO_Analysis, indent);
    if (_scalar_last_value.Elements()>i && _scalar_last_value.Bottom_nth(i))
      fprintf(LNO_Analysis,"(LAST_LOCAL_SCALAR ");
    else
      fprintf(LNO_Analysis,"(LOCAL_SCALAR ");
    SCALAR_STACK & stk = (SCALAR_STACK &) _scalar_pri;
    stk.Bottom_nth(i)->_scalar.Print(LNO_Analysis);
    fprintf(LNO_Analysis,")\n");
  }

  for (i = 0; i < _def.Elements(); ++i) {
    print_indent(LNO_Analysis, indent);
    fprintf(LNO_Analysis,"(KILL_ARRAY ");
    _def.Bottom_nth(i)->Print_Analysis_Info(LNO_Analysis,indent+3,Do_Stack());
    print_indent(LNO_Analysis, indent);
    fprintf(LNO_Analysis,")\n");
  }

  for (i = 0; i < _scalar_def.Elements(); ++i) {
    print_indent(LNO_Analysis, indent);
    fprintf(LNO_Analysis,"(KILL_SCALAR ");
    SCALAR_STACK & stk = (SCALAR_STACK &) _scalar_def;
    stk.Bottom_nth(i)->_scalar.Print(LNO_Analysis);
    fprintf(LNO_Analysis,")\n");
  }

  for (i = 0; i < _may_def.Elements(); ++i) {
    print_indent(LNO_Analysis, indent);
    fprintf(LNO_Analysis,"(DEF_ARRAY ");
    _may_def.Bottom_nth(i)->Print_Analysis_Info(LNO_Analysis,
      indent+3,Do_Stack());
    print_indent(LNO_Analysis, indent);
    fprintf(LNO_Analysis,")\n");
  }
  for (i = 0; i < _scalar_may_def.Elements(); ++i) {
    print_indent(LNO_Analysis, indent);
    fprintf(LNO_Analysis,"(DEF_SCALAR ");
    SCALAR_STACK & stk = (SCALAR_STACK &) _scalar_may_def;
    stk.Bottom_nth(i)->_scalar.Print(LNO_Analysis);
    fprintf(LNO_Analysis,")\n");
  }

  for (i = 0; i < _use.Elements(); ++i) {
    print_indent(LNO_Analysis, indent);
    fprintf(LNO_Analysis,"(EXPOSED_ARRAY_USE ");
    _use.Bottom_nth(i)->Print_Analysis_Info(LNO_Analysis,indent+3,Do_Stack());
    print_indent(LNO_Analysis, indent);
    fprintf(LNO_Analysis,")\n");
  }

  for (i = 0; i < _scalar_use.Elements(); ++i) {
    print_indent(LNO_Analysis, indent);
    fprintf(LNO_Analysis,"(EXPOSED_SCALAR_USE ");
    SCALAR_STACK & stk = (SCALAR_STACK &) _scalar_use;
    stk.Bottom_nth(i)->_scalar.Print(LNO_Analysis);
    fprintf(LNO_Analysis,")\n");
  }
  
  fprintf(LNO_Analysis,")\n");
}

static
BOOL Is_Unknown_Size_Array(const SYMBOL* array)
{
  ST* st = array->St();
  TY_IDX ty = ST_type(st);

  if (ST_class(st) == CLASS_PREG) 
    return TRUE;

  if (TY_kind(ty) == KIND_POINTER) {
    // mp lower doesn't know how to handle Cray pointer (PV# 509260)
    ty = TY_pointed(ty);
    if (ST_sclass(st) != SCLASS_FORMAL && TY_size(ty) != 0) 
      return TRUE; 
  }

  if (TY_kind(ty) != KIND_ARRAY) 
    return TRUE;
  
  INT ndims = TY_AR_ndims(ty);
  for (INT i = 0; i < ndims; i++) {
    if (!TY_AR_const_lbnd(ty, i)) 
      return TRUE; 
    if (!TY_AR_const_ubnd(ty,i))
      return TRUE; 
    if (!TY_AR_const_stride(ty,i))
      return TRUE; 
    INT diff = TY_AR_ubnd_val(ty,i) - TY_AR_lbnd_val(ty,i) + 1;
    if (diff <= 1) 
      return TRUE;
  }
  return FALSE;
}

ARA_REF::ARA_REF(WN *array_wn, INT32 offset, ARA_LOOP_INFO  *ali)
{
  Is_True(WN_operator(array_wn) == OPR_ARRAY, 
	  ("ARA_REF::ARA_REF called on a non-array"));

  WN *base = WN_array_base(array_wn);
  while (WN_operator(base) == OPR_ARRAY)
    base = WN_array_base(base);

  _donot_care_invariant = FALSE;
  _is_too_messy = FALSE;

  if (WN_operator(base) != OPR_LDID &&
      WN_operator(base) != OPR_LDA) {
    ali->Set_Bad_Region();
    _array = NULL;
    _offset = 0;
    _has_bad_alias = TRUE;
    _is_loop_invariant = FALSE;
    _is_too_messy = TRUE; 
    return;
  } 
    
  _array = CXX_NEW(SYMBOL(base),&ARA_memory_pool);
  _is_unknown_size = Is_Unknown_Size_Array(_array);
  _offset = offset;
  REGION *new_region = CXX_NEW(REGION(array_wn, ali), &ARA_memory_pool);
  _image.Add_Region(new_region, *ali);
  _has_bad_alias = FALSE;
  _need_last_value = TRUE;
  ACCESS_ARRAY *s_array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, array_wn);
  if (s_array!=NULL)
    _is_loop_invariant = Loop_Invariant_Access(array_wn, ali->Loop()) &&
      ! new_region->Is_Too_Messy();
  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout,"ARA_REF::ARA_REF: the region is\n");
    _image.Print(stdout);
  }
}

ARA_REF::ARA_REF(SYMBOL *array_sym, REGION* new_region, ARA_LOOP_INFO  *ali,
	BOOL is_invariant)
{
  _array = CXX_NEW(SYMBOL(array_sym),&ARA_memory_pool);
  _is_unknown_size = Is_Unknown_Size_Array(_array);
  _offset = array_sym->WN_Offset();
  _image.Add_Region(new_region, *ali);
  _has_bad_alias = FALSE;
  _need_last_value = TRUE;
  _is_loop_invariant = is_invariant && ! new_region->Is_Too_Messy();
  _is_too_messy = new_region->_type == ARA_TOO_MESSY; 
  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout,"ARA_REF::ARA_REF: the region is\n");
    _image.Print(stdout);
  }
}

void
ARA_LOOP_INFO::Annotate_Invariant_Pri()
{

  for (INT i = 0; i < _pri.Elements(); ++i) {
    _pri.Bottom_nth(i)->Set_Loop_Invariant(_loop);
  }

}

void
ARA_LOOP_INFO::Annotate_Invariant_Def()
{

  for (INT i = 0; i < _def.Elements(); ++i) {
    ARA_REF *cur_def = _def.Bottom_nth(i);
    cur_def->Set_Loop_Invariant(_loop);
  }

}

ARA_REF * Contains(ARA_REF_ST & ara_st, WN *array_wn)
{

  for (INT i = 0; i < ara_st.Elements(); ++i) {
    ARA_REF *cur = ara_st.Bottom_nth(i);
    if (OPCODE_has_sym(WN_opcode(WN_array_base(array_wn))))
      {
	if (WN_st(WN_array_base(array_wn)) == cur->Array().St())
	  if (!cur->Has_Bad_Alias() && cur->Image().Contains(array_wn))
	    return cur;
      }
    else
      return NULL;
  }
  return NULL;
}

void 
Add_Helper(ARA_REF *ara_ref, ARA_REF_ST & ara_st, ARA_LOOP_INFO &ali)
{
  for (INT i = 0; i < ara_st.Elements(); ++i) {
    ARA_REF *cur = ara_st.Bottom_nth(i);
    if (Is_Same_Array(cur->Array(),cur->Offset(),
		      ara_ref->Array(),ara_ref->Offset())) {
      BOOL donot_care_invariant = ara_ref->Donot_Care_Invariant();
      cur->Add_Ref(ara_ref, ali);
      if (donot_care_invariant) cur->Set_Donot_Care_Invariant();
      cur->Set_Whole_Array();
      return;
    }
  }
  
  // It hasn't been seen before, add a new one
  ara_st.Push(ara_ref);
  ara_ref->Set_Whole_Array();
}
  
void 
ARA_LOOP_INFO::Add_Def(ARA_REF *ara_ref)
{
  Add_Helper(ara_ref, _def, *this);
}

void 
ARA_LOOP_INFO::Add_May_Def(ARA_REF *ara_ref)
{
  Add_Helper(ara_ref, _may_def, *this);
}

void 
ARA_LOOP_INFO::Add_Use(ARA_REF *ara_ref)
{
  Add_Helper(ara_ref, _use, *this);
}

void 
ARA_LOOP_INFO::Add_Pri(ARA_REF *ara_ref)
{
  Add_Helper(ara_ref, _pri, *this);
}

void 
ARA_LOOP_INFO::Remove_Array_Info()
{ 
  _def.Clear();
  _use.Clear();
  _may_def.Clear();
  _pri.Clear();
} 

BOOL 
ARA_LOOP_INFO::Is_Covered(ARA_REF *ara_ref)
{

  for (INT i = 0; i<_def.Elements(); ++i) {
    ARA_REF *cur = _def.Bottom_nth(i);
    if (Is_Same_Array(cur->Array(), cur->Offset(), 
		      ara_ref->Array(), ara_ref->Offset())) {
      if (cur->Image().Is_Included(ara_ref->Image(), *this)) {
	if (cur->Donot_Care_Invariant()) {
	  ara_ref->Set_Donot_Care_Invariant();
	}
	return TRUE;
      }
      else
	return FALSE;
    }
  }

  return FALSE;

}

BOOL 
Is_Unconditional(WN *wn, WN* do_loop)
{

  DOLOOP_STACK doloop_stack(&ARA_memory_pool);
  for (WN *wn_temp = wn; wn_temp && wn_temp != do_loop; 
       wn_temp = LWN_Get_Parent(wn_temp)) {
    if (WN_opcode(wn_temp) == OPC_DO_LOOP) 
      doloop_stack.Push(wn_temp);
    else if (WN_opcode(wn_temp) == OPC_IF) {
      for (INT i = 0; i < doloop_stack.Elements(); ++i) {
	DO_LOOP_INFO *dli = Get_Do_Loop_Info(doloop_stack.Bottom_nth(i));
	if (dli && dli->Guard == wn_temp) return TRUE;
      }
      return FALSE;
    } else if (OPCODE_is_scf(WN_opcode(wn_temp)) && WN_opcode(wn_temp) != OPC_BLOCK)
      return FALSE;
  }

  return TRUE;

}

// Alias analysis for privatization.  No privatizable array may be 
// aliased with anything else in the use or def lists. 

static BOOL Identical(ARA_REF* ara_ref_one,
		      ARA_REF* ara_ref_two)
{ 
  ST* st_array_one = ara_ref_one->Array().St();
  ST* st_array_two = ara_ref_two->Array().St();
  return st_array_one == st_array_two; 
} 

static BOOL Identical(SCALAR_NODE* sn_one, 
		      SCALAR_NODE* sn_two)
{ 
  FmtAssert(!sn_one->_scalar.Is_Formal(),
    ("Identical: expecting non-formal: sn_one"));
  FmtAssert(!sn_two->_scalar.Is_Formal(),
    ("Identical: expecting non-formal: sn_two"));
  ST* st_scalar_one = sn_one->_scalar.St();
  ST* st_scalar_two = sn_two->_scalar.St();
  if (st_scalar_one != st_scalar_two) 
    return FALSE; 
  INT64 st_offset_one = sn_one->_scalar.ST_Offset();
  INT64 st_offset_two = sn_two->_scalar.ST_Offset();
  if (st_offset_one != st_offset_two)
    return FALSE; 
  INT st_size_one = MTYPE_RegisterSize(sn_one->_scalar.Type);
  INT st_size_two = MTYPE_RegisterSize(sn_two->_scalar.Type);
  if (st_size_one != st_size_two) 
    return FALSE;
  return TRUE;  
} 

static POINTS_TO* Points_To(ARA_REF* ara_ref)
{ 
  return Points_To(ara_ref, &ARA_memory_pool);
} 

static POINTS_TO* Points_To(SCALAR_NODE* sn)
{ 
  return Points_To(sn, &ARA_memory_pool);
} 

static BOOL Test_Alias_Ara_Ref_Array(ARA_REF* ara_ref, 
				     POINTS_TO* pt_ref,
				     ARA_REF_ST* st_array)
{ 
  for (INT j = 0; j < st_array->Elements(); j++) { 
    ARA_REF* ara_ref_use = st_array->Bottom_nth(j);
    if (Identical(ara_ref, ara_ref_use))
      continue; 
    POINTS_TO* pt_use = Points_To(ara_ref_use);
    if (Alias_Mgr->Aliased(pt_ref, pt_use)) { 
      ara_ref->Set_Bad_Alias();
      return TRUE; 
    } 
  } 
  return FALSE; 
} 

static BOOL Test_Alias_Ara_Ref_Scalar(ARA_REF* ara_ref,
                                      POINTS_TO* pt_ref,
                                      SCALAR_STACK* st_scalar)
{
  for (INT j = 0; j < st_scalar->Elements(); j++) { 
    SCALAR_NODE* sn_use = st_scalar->Bottom_nth(j);
    POINTS_TO* pt_use = Points_To(sn_use);
    if (Alias_Mgr->Aliased(pt_ref, pt_use)) { 
      ara_ref->Set_Bad_Alias();
      return TRUE; 
    } 
  } 
  return FALSE; 
} 

static BOOL Test_Alias_Scalar_Node_Array(POINTS_TO* pt_ref,
					 ARA_REF_ST* st_array,
					 STACK<BOOL>* bad_alias)
{ 
  for (INT j = 0; j < st_array->Elements(); j++) { 
    ARA_REF* ara_ref_use = st_array->Bottom_nth(j);
    POINTS_TO* pt_use = Points_To(ara_ref_use);
    if (Alias_Mgr->Aliased(pt_ref, pt_use)) { 
      bad_alias->Push(TRUE);
      return TRUE; 
    } 
  } 
  return FALSE; 
} 

static BOOL Test_Alias_Scalar_Node_Scalar(SCALAR_NODE* sn_ref, 	
					  POINTS_TO* pt_ref,
					  SCALAR_STACK* st_scalar,
					  STACK<BOOL>* bad_alias)
{ 
  for (INT j = 0; j < st_scalar->Elements(); j++) { 
    SCALAR_NODE* sn_use = st_scalar->Bottom_nth(j);
    if (Identical(sn_ref, sn_use))
      continue; 
    POINTS_TO* pt_use = Points_To(sn_use);
    if (Alias_Mgr->Aliased(pt_ref, pt_use)) { 
      bad_alias->Push(TRUE);
      return TRUE; 
    } 
  } 
  return FALSE; 
} 

void ARA_LOOP_INFO::Test_Alias()
{ 
  INT i;
  // Privatizable array aliasing  
  for (i = 0; i < _pri.Elements(); i++) { 
    ARA_REF* ara_ref_pri = _pri.Bottom_nth(i);
    POINTS_TO* pt_pri = Points_To(ara_ref_pri);
    if (Test_Alias_Ara_Ref_Array(ara_ref_pri, pt_pri, &_use))
      continue; 
    if (Test_Alias_Ara_Ref_Array(ara_ref_pri, pt_pri, &_def))
      continue;
    if (Test_Alias_Ara_Ref_Scalar(ara_ref_pri, pt_pri, &_scalar_use))
      continue; 
    if (Test_Alias_Ara_Ref_Scalar(ara_ref_pri, pt_pri, &_scalar_may_def))
      continue; 
  } 

  // Privatizable scalar aliasing 
  for (i = 0; i < _scalar_pri.Elements(); i++) { 
    SCALAR_NODE* sn_pri = _scalar_pri.Bottom_nth(i);
    POINTS_TO* pt_pri = Points_To(sn_pri);
    if (Test_Alias_Scalar_Node_Array(pt_pri, &_use, &_bad_alias))
      continue; 
    if (Test_Alias_Scalar_Node_Array(pt_pri, &_def, &_bad_alias))
      continue;
    if (Test_Alias_Scalar_Node_Scalar(sn_pri, pt_pri, &_scalar_use, 
	&_bad_alias))
      continue; 
    if (Test_Alias_Scalar_Node_Scalar(sn_pri, pt_pri, &_scalar_def, 
	&_bad_alias))
      continue; 
    if (Test_Alias_Scalar_Node_Scalar(sn_pri, pt_pri, &_scalar_may_def, 
	&_bad_alias))
      continue; 
    _bad_alias.Push(FALSE);
  } 
} 

void
ARA_LOOP_INFO::Projection()
{

  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout, "Before Projection: \n");
    this->Print(stdout);
  }

  INT i;
  for (i = 0; i < _def.Elements(); ++i) {
    _def.Bottom_nth(i)->Image().RegionUN_Projection(Depth(), *this);
    _def.Bottom_nth(i)->Set_Whole_Array(FALSE);
  }

  for (i = 0; i < _may_def.Elements(); ++i) {
    _may_def.Bottom_nth(i)->Image().RegionUN_Projection(Depth(), *this);
    _may_def.Bottom_nth(i)->Set_Whole_Array(FALSE);
  }

  for (i = 0; i < _use.Elements(); ++i) {
    _use.Bottom_nth(i)->Image().RegionUN_Projection(Depth(), *this);
    _use.Bottom_nth(i)->Set_Whole_Array(FALSE);  
  }

  for (i = 0; i < _pri.Elements(); ++i) {
    _pri.Bottom_nth(i)->Image().RegionUN_Projection(Depth(), *this);
    _pri.Bottom_nth(i)->Set_Whole_Array(FALSE);
  }

  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout, "After Projection: \n");
    this->Print(stdout);
  }

}

void
ARA_LOOP_INFO::Set_Whole_Array()
{
  INT i;
  for (i = 0; i < _def.Elements(); ++i) {
    _def.Bottom_nth(i)->Set_Whole_Array();
  }

  for (i = 0; i < _may_def.Elements(); ++i) {
    _may_def.Bottom_nth(i)->Set_Whole_Array();
  }

  for (i = 0; i < _use.Elements(); ++i) {
    _use.Bottom_nth(i)->Set_Whole_Array();  
  }

  for (i = 0; i < _pri.Elements(); ++i) {
    _pri.Bottom_nth(i)->Set_Whole_Array();
  }

  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout, "After Set_Whole_Array: \n");
    this->Print(stdout);
  }
}
  
// determine if an array reference is loop invariant for the 'loop'
// in
BOOL Loop_Invariant_Inside(ACCESS_ARRAY* array, const WN* loop) {

  if (WN_operator(loop) != OPR_DO_LOOP) 
    return FALSE;

  INT loopno=Do_Loop_Depth((WN *) loop);

  if (array->Non_Const_Loops() > loopno) 
    return FALSE;
  for (INT i=0; i<array->Num_Vec(); i++) {
    ACCESS_VECTOR *av = array->Dim(i);
    if (av->Too_Messy)
      return FALSE;
    for (INT j=loopno; j < av->Nest_Depth(); j++) {
      if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) 
	fprintf(stdout,"Loop coefficient is: %d\n", av->Loop_Coeff(j)); 
      if ((av->Loop_Coeff(j) != 0)) 
        return FALSE;
    }
  }

  return TRUE;
}

BOOL Loop_Invariant_Access(ACCESS_ARRAY *array, const WN *loop)
{
  if (WN_operator(loop) != OPR_DO_LOOP) 
    return FALSE;
  INT loopno = Do_Loop_Depth((WN *) loop);
  if (array->Non_Const_Loops() > loopno) 
    return FALSE;

  for (INT i=0; i<array->Num_Vec(); i++) {
    ACCESS_VECTOR *av = array->Dim(i);
    if (av->Too_Messy)
      return FALSE;
    for (INT j=0; j<=loopno; j++) {
      if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) 
	fprintf(stdout,"Loop coefficient is: %d\n", av->Loop_Coeff(j)); 
      if (av->Loop_Coeff(j) != 0) 
        return FALSE;
    }
  }
  return TRUE;
}

// determine if an array reference is loop invariant or not
BOOL Loop_Invariant_Access(WN* wn_array, const WN* loop) 
{
  ACCESS_ARRAY *array = 
    (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn_array);
  if (WN_operator(loop) != OPR_DO_LOOP) 
    return FALSE;

  INT loopno = Do_Loop_Depth((WN *) loop);
  if (array->Non_Const_Loops() > loopno) 
    return FALSE;

  WN *wn;
  for (wn = wn_array; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_DO_LOOP)
      break;
  WN* wn_inner = wn; 
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);

  for (INT i=0; i<array->Num_Vec(); i++) {
    ACCESS_VECTOR *av = array->Dim(i);
    if (av->Too_Messy)
      return FALSE;
    INT j;
    for (j=0; j<=loopno; j++) {
      if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) 
	fprintf(stdout,"Loop coefficient is: %d\n", av->Loop_Coeff(j)); 
      if (av->Loop_Coeff(j) != 0) 
        return FALSE;
    }
    for (j = 0; j < av->Nest_Depth(); j++) {
      if (av->Loop_Coeff(j) != 0) {
        WN* wn_loop = stack.Bottom_nth(j); 
        DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop); 
        INT k;
        for (k = 0; k < dli_loop->LB->Num_Vec(); k++) { 
          ACCESS_VECTOR* avk = dli_loop->LB->Dim(k);
	  if (avk->Too_Messy) 
            return FALSE; 
          if (avk->Non_Const_Loops() > loopno)
	    return FALSE; 
        }
        for (k = 0; k < dli_loop->UB->Num_Vec(); k++) { 
          ACCESS_VECTOR* avk = dli_loop->UB->Dim(k);
	  if (avk->Too_Messy) 
            return FALSE; 
          if (avk->Non_Const_Loops() > loopno)
	    return FALSE; 
        }
      }  
    }
  }

  return TRUE;
}

void
ARA_REF::Set_Loop_Invariant(WN *loop)
{
  
  if (!_is_loop_invariant && !_donot_care_invariant) {
    REGION_ITER iter(&_image);
    for (REGION *cur = iter.First(); !iter.Is_Empty(); cur = iter.Next()) {
      if (!cur->Is_Loop_Invariant(loop)) return;
      for (INT i = 0; i < cur->_wn_list.Elements(); ++ i) {
	ACCESS_ARRAY *s_array = 
	  (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, cur->_wn_list.Bottom_nth(i));
	if (!Loop_Invariant_Access(s_array,loop)) 
          return;
      }
    }
    _is_loop_invariant = TRUE;
  }

}

//=============================================================================
//
// Determine if an array reference is 
// privatizable in the loop.  
//
//=============================================================================
BOOL ARA_LOOP_INFO::Is_Privatizable(WN* wn, BOOL definitely)
{
  
  if ((WN_operator(wn) == OPR_ILOAD) &&
      (WN_operator(WN_kid0(wn)) == OPR_ARRAY)) {
#ifdef KEY // bug 10707: screen out cases where the variable is of complex type
    	   //		 but the code is operating on float types
    if (WN_operator(WN_kid0(WN_kid0(wn))) == OPR_LDID) {
      TY_IDX ty = ST_type(WN_st(WN_kid0(WN_kid0(wn))));
      if (TY_kind(ty) == KIND_POINTER)
	ty = TY_pointed(ty);
      if (TY_kind(ty) == KIND_ARRAY) {
	ty = TY_etype(ty);
	if (! MTYPE_is_complex(WN_rtype(wn)) && MTYPE_is_complex(TY_mtype(ty)))
	  return FALSE;
      }
    }
#endif

    // First test if it is loop invariant
    ACCESS_ARRAY * s_array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, WN_kid0(wn));
    Is_True(s_array, ("ARA_LOOP_INFO::Is_Privatizable: No acccess array for source"));
    ARA_REF * found = Contains(_pri, WN_kid0(wn));
    BOOL is_ok = (found && found->Is_Loop_Invariant() && 
		  !found->Is_Unknown_Size());
    if (!definitely) 
      return is_ok;
    else 
      return (is_ok && (!found->Need_Last_Value() || 
			Is_Covered(found)));
      
  } else if ((WN_operator(wn) == OPR_ISTORE) &&
	     (WN_operator(WN_kid1(wn)) == OPR_ARRAY)) {
#ifdef KEY // bug 10707: screen out cases where the variable is of complex type
    	   //		 but the code is operating on float types
    if (WN_operator(WN_kid0(WN_kid1(wn))) == OPR_LDID) {
      TY_IDX ty = ST_type(WN_st(WN_kid0(WN_kid1(wn))));
      if (TY_kind(ty) == KIND_POINTER)
	ty = TY_pointed(ty);
      if (TY_kind(ty) == KIND_ARRAY) {
	ty = TY_etype(ty);
	if (! MTYPE_is_complex(WN_desc(wn)) && MTYPE_is_complex(TY_mtype(ty)))
	  return FALSE;
      }
    }
#endif

    // First test if it is loop invariant
    ACCESS_ARRAY * s_array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, WN_kid1(wn));
    Is_True(s_array, ("ARA_LOOP_INFO::Is_Privatizable: No acccess array for source"));

    ARA_REF * found = Contains(_def, WN_kid1(wn));
    if (found) return (found->Is_Loop_Invariant() && !found->Is_Unknown_Size());
    
    found = Contains(_pri, WN_kid1(wn));
    if (found) return (found->Is_Loop_Invariant() && 
		       !found->Is_Unknown_Size() && 
		       (!found->Need_Last_Value() || Is_Covered(found)));

  }
  
  return FALSE;

}

void
ARA_LOOP_INFO::Print_Loop_Property()
{

  for (INT i = 0; i < _children.Elements(); ++i)
    _children.Bottom_nth(i)->Print_Loop_Property();

  if (Is_Parallel()) { 
    fprintf(stdout,"Loop %s is parallel\n", WB_Whirl_Symbol(_loop));
  } else {
    fprintf(stdout,"Loop %s is sequential\n", WB_Whirl_Symbol(_loop));
    if (_info == NULL)
      fprintf(stdout, "_info is NULL\n");
    if (_info != NULL && _info->Has_Gotos)
      fprintf(stdout, "_info has gotos\n");
    if (_info != NULL && _info->Has_Gotos_This_Level)
      fprintf(stdout, "_info has gotos this level\n");
    if (_info != NULL && _info->Has_Exits)
      fprintf(stdout, "_info has exits\n");
    if (_info != NULL &&_info->Has_Bad_Mem)
      fprintf(stdout, "_info has bad mem\n"); 
    if (_info != NULL &&_info->Has_Calls)
      fprintf(stdout, "_info has calls\n"); 
    if (!_is_good)
      fprintf(stdout, "has bad dependence\n"); 
    if (_dep_dist!=0)
      fprintf(stdout, "has loop carried dependence\n");
    if (!Upper_Bound_Standardize(WN_end(_loop),TRUE))
      fprintf(stdout, "non-standard upper bound\n"); 
    if (_info != NULL && _info->Pragma_Cannot_Concurrentize)
      fprintf(stdout, "has NO CONCURRENTIZE directive"); 
    if (_info != NULL && _info->Serial_Version_of_Concurrent_Loop)
      fprintf(stdout, "is in serial version of concurrent loop"); 
  }  
  this->Print(stdout);

}

//========================================================================
// Add the summary of a SCF 'ali' to the current total info 'this'
// seen_non_scf says if there  is a potential goto before this scf
// if not, we know that this scf must be executed
//========================================================================
void 
ARA_LOOP_INFO::Merge_Info(ARA_LOOP_INFO *ali, BOOL seen_non_scf)
{
  INT i;
  // Handle array region sets
  for (i = 0; i < ali->_use.Elements(); ++i) {
    ARA_REF *cur_clone = CXX_NEW(ARA_REF(*(ali->_use.Bottom_nth(i))), 
      &ARA_memory_pool);
    if (Is_Covered(cur_clone)) 
      Add_Pri(cur_clone);
    else
      Add_Use(cur_clone);
  }

  for (i = 0; i < ali->_pri.Elements(); ++i) {
    ARA_REF *cur_clone = CXX_NEW(ARA_REF(*(ali->_pri.Bottom_nth(i))), 
        &ARA_memory_pool);
    Add_Pri(cur_clone);
    
  }

  if (!Info()->Has_Gotos || 
       ((!Info()->Has_Gotos_This_Level || !seen_non_scf) && !Info()->Has_Exits)) {
    for (i = 0; i < ali->_def.Elements(); ++i) {
      ARA_REF *cur_clone = CXX_NEW(ARA_REF(*(ali->_def.Bottom_nth(i))), 
        &ARA_memory_pool);
      Add_Def(cur_clone);
    }
  }

  for (i = 0; i < ali->_may_def.Elements(); ++i) {
    ARA_REF *cur_clone = CXX_NEW(ARA_REF(*(ali->_may_def.Bottom_nth(i))), 
      &ARA_memory_pool);
    Add_May_Def(cur_clone);
  }

  // Handle scalar sets
  Merge_Scalar_List(&ali->_scalar_may_def, &_scalar_may_def);
  Merge_Scalar_List_Covered(&ali->_scalar_use, this, &_scalar_pri, 
    &_scalar_use);
  
  for (i = 0; i < ali->_reduction.Elements(); ++i) 
    if (Is_Covered(ali->_reduction.Bottom_nth(i)) ) 
      _scalar_pri.Add_Scalar(ali->_reduction.Bottom_nth(i),0);
    else
      _reduction.Push(ali->_reduction.Bottom_nth(i));

  Merge_Scalar_List(&ali->_scalar_pri, &_scalar_pri);
  
  if (!Info()->Has_Gotos || 
       ((!Info()->Has_Gotos_This_Level || 
	!seen_non_scf) && !Info()->Has_Exits)) {
    Merge_Scalar_List(&ali->_scalar_def, &_scalar_def);
  } 

  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout, "After Merge_Info: \n");
    this->Print(stdout);
  }
  
}

//=======================================================================
// Find the matching ARA_REF to 'a'
//=======================================================================
ARA_REF *
ARA_LOOP_INFO::Has_Matching(ARA_REF_ST &ara_s, ARA_REF *a)
{
  for (INT i = 0; i < ara_s.Elements(); ++i) {
    ARA_REF *cur = ara_s.Bottom_nth(i);
    if (Is_Same_Array(cur->Array(), cur->Offset(),
		      a->Array(), a->Offset())) {
      if (cur->Image().Is_Included(a->Image(), *this) &&
	  a->Image().Is_Included(cur->Image(), *this))
	return cur;
    }
  }

  return NULL;
}

SCALAR_NODE *
ARA_LOOP_INFO::Has_Matching(SCALAR_STACK &st, SCALAR_NODE *sn)
{
  for (INT i = 0; i < st.Elements(); ++i) {
    SCALAR_NODE *cur = st.Bottom_nth(i);
    if (cur->_scalar == sn->_scalar)
      return cur;
  }
  return NULL;
}

//=======================================================================
// Merge info from 'ara_then' and 'ara_else' to this
//=======================================================================
void ARA_LOOP_INFO::Merge_then_else(ARA_LOOP_INFO *ara_then, 
				    ARA_LOOP_INFO *ara_else)
{
  INT i;

  // _use is a union of both
  for (i = 0; i < ara_then->_use.Elements(); ++i) {
    ARA_REF *cur_clone = 
      CXX_NEW(ARA_REF(*(ara_then->_use.Bottom_nth(i))), &ARA_memory_pool);
    Add_Use(cur_clone);
  }
  for (i = 0; i < ara_else->_use.Elements(); ++i) {
    ARA_REF *cur_clone = 
      CXX_NEW(ARA_REF(*(ara_else->_use.Bottom_nth(i))), &ARA_memory_pool);
    Add_Use(cur_clone);
  }
  // _may_def is a union of both
  for (i = 0; i < ara_then->_may_def.Elements(); ++i) {
    ARA_REF *cur_clone = 
      CXX_NEW(ARA_REF(*(ara_then->_may_def.Bottom_nth(i))), &ARA_memory_pool);
    Add_May_Def(cur_clone);
  }
  for (i = 0; i < ara_else->_may_def.Elements(); ++i) {
    ARA_REF *cur_clone = 
      CXX_NEW(ARA_REF(*(ara_else->_may_def.Bottom_nth(i))), &ARA_memory_pool);
    Add_May_Def(cur_clone);
  }
  for (i = 0; i < ara_then->_pri.Elements(); ++i) {
    ARA_REF *cur = ara_then->_pri.Bottom_nth(i);
    ARA_REF *cur_clone = CXX_NEW(ARA_REF(*cur), &ARA_memory_pool);
    Add_Pri(cur_clone);
  }
  for (i = 0; i < ara_else->_pri.Elements(); ++i) {
    ARA_REF *cur = ara_else->_pri.Bottom_nth(i);
    ARA_REF *cur_clone = CXX_NEW(ARA_REF(*cur), &ARA_memory_pool);
    Add_Pri(cur_clone);
  }
  
  // _def only if both of them are the same 
  for (i = 0; i < ara_then->_def.Elements(); ++i) {
    ARA_REF *cur = ara_then->_def.Bottom_nth(i);
    ARA_REF *match = Has_Matching(ara_else->_def, cur);
    if (match != NULL) {
      ARA_REF *cur_clone = CXX_NEW(ARA_REF(*cur), &ARA_memory_pool);
      Add_Def(cur_clone);
      cur_clone = CXX_NEW(ARA_REF(*match), &ARA_memory_pool);
      Add_Def(cur_clone);
    } else {
      ARA_REF *cur_clone = CXX_NEW(ARA_REF(*cur), &ARA_memory_pool);
      Add_Pri(cur_clone);
    }
  }

  for (i = 0; i < ara_else->_def.Elements(); ++i) {
    ARA_REF *cur = ara_else->_def.Bottom_nth(i);
    ARA_REF *match = Has_Matching(ara_then->_def, cur);
    if (match == NULL) {
      ARA_REF *cur_clone = CXX_NEW(ARA_REF(*cur), &ARA_memory_pool);
      Add_Pri(cur_clone);
    }
  }

  // scalar sets
  Merge_Scalar_List(&ara_then->_scalar_may_def, &_scalar_may_def);
  Merge_Scalar_List(&ara_else->_scalar_may_def, &_scalar_may_def);
  Merge_Scalar_List(&ara_then->_scalar_use, &_scalar_use);
  Merge_Scalar_List(&ara_else->_scalar_use, &_scalar_use);

  for (i = 0; i < ara_then->_reduction.Elements(); ++i) 
    _reduction.Push(ara_then->_reduction.Bottom_nth(i));
  for (i = 0; i < ara_else->_reduction.Elements(); ++i) 
    _reduction.Push(ara_else->_reduction.Bottom_nth(i));
  
  Merge_Scalar_List(&ara_then->_scalar_pri, &_scalar_pri);
  Merge_Scalar_List(&ara_else->_scalar_pri, &_scalar_pri);

  for (i = 0; i < ara_then->_scalar_def.Elements(); ++i) {
    SCALAR_NODE *cur = ara_then->_scalar_def.Bottom_nth(i);
    SCALAR_NODE *match = Has_Matching(ara_else->_scalar_def,cur);
    if (match != NULL) {
      SYMBOL* sym_cur = &cur->_scalar; 
      SYMBOL* sym_match = &match->_scalar; 
      INT j;
      for (j = 0; j < cur->Elements(); ++j) { 
	WN* wn_cur = cur->Bottom_nth(j)->Wn;
	if (OPCODE_is_call(WN_opcode(wn_cur))
	    || WN_operator(wn_cur) == OPR_LDA) { 
	  _scalar_def.Add_Scalar(wn_cur, sym_cur, 0);
	} else { 
	  _scalar_def.Add_Scalar(wn_cur, 0);
	} 
      } 
      for (j = 0; j < match->Elements(); ++j) { 
	WN* wn_match = match->Bottom_nth(j)->Wn;
	if (OPCODE_is_call(WN_opcode(wn_match))
	    || WN_operator(wn_match) == OPR_LDA) { 
	  _scalar_def.Add_Scalar(wn_match, sym_match, 0);
	} else { 
	  _scalar_def.Add_Scalar(wn_match, 0);
	} 
      } 
    }
  }

  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout, "After Merge_then_else: \n");
    this->Print(stdout);
  }

}

//=======================================================================
// Walk the RHS and collect exposed uses, skip the 'skip_store_id' WN
//=======================================================================
void ARA_LOOP_INFO::Walk_Rhs(WN *wn, WN *skip_store_id)
{
  LWN_ITER *rhs = LWN_WALK_TreeIter(wn);
  while (rhs) {
    
    WN* wn = rhs->wn;
    rhs = LWN_WALK_TreeNext(rhs);

    if (wn == skip_store_id) {
      wn = rhs->wn;
      rhs = LWN_WALK_TreeNext(rhs);
      skip_store_id = NULL;
    }
    
    if ((WN_operator(wn) == OPR_ILOAD) &&
	(WN_operator(WN_kid0(wn)) == OPR_ARRAY)) {
      ARA_REF *new_use  
	= CXX_NEW(ARA_REF(WN_kid0(wn),WN_offset(wn),this), &ARA_memory_pool);
      if (new_use->Has_Bad_Alias()) 
	CXX_DELETE(new_use, &ARA_memory_pool);
      else {
	if (Is_Covered(new_use))
	  Add_Pri(new_use);
	else
	  Add_Use(new_use);
      }

      // Skip the LDID for OPR_ARRAY
      rhs = LWN_WALK_TreeNext(rhs);
      rhs = LWN_WALK_TreeNext(rhs);

    } else if (WN_operator(wn) == OPR_LDID) {
      if (Is_Covered(wn))
	_scalar_pri.Add_Scalar(wn,0);
      else if (red_manager && red_manager->Which_Reduction(wn) != RED_NONE) {
	Add_Reduction(wn);
	_scalar_use.Add_Scalar(wn,0);
      } else
	_scalar_use.Add_Scalar(wn,0);
    } else if (WN_operator(wn) == OPR_PARM
	&& WN_operator(WN_kid0(wn)) == OPR_LDA) { 
      WN* wn_lda = WN_kid0(wn);
      WN* wn_call = LWN_Get_Parent(wn);
      INT i;
      for (i = 0; i < WN_kid_count(wn_call); i++)
	if (WN_kid(wn_call, i) == wn)
	  break; 
      TYPE_ID type_id = IPA_LNO_File != NULL 
	? Formal_Machine_Type(wn_call, i, IPA_LNO_File)
	: TY_mtype(ST_type(WN_st(wn_lda)));
      SYMBOL sym_lda(WN_st(wn_lda), WN_offset(wn_lda), type_id);
      if (Is_Covered(wn_lda)) { 
	_scalar_pri.Add_Scalar(wn_lda, &sym_lda, 0);
      } else { 
	_scalar_use.Add_Scalar(wn_lda, &sym_lda, 0);
      } 
    } 
  }
}

//=========================================================================
// Walk a OPC_BLOCK stmt
//=========================================================================
void
ARA_LOOP_INFO::Walk_Block(WN *block_stmt)
{
  Is_True(WN_opcode(block_stmt) == OPC_BLOCK, 
	  ("ARA_LOOP_INFO::Walk_Block: not a OPC_BLOCK stmt"));

  LWN_ITER *stmt_iter = LWN_WALK_StmtIter(block_stmt);
  stmt_iter = LWN_WALK_StmtNext(stmt_iter);

  BOOL seen_non_scf = FALSE;
  while (stmt_iter) {
    WN* stmt = stmt_iter->wn;
    stmt_iter = LWN_WALK_StmtNext(stmt_iter);
    OPCODE   op = WN_opcode(stmt);
    OPERATOR opr = OPCODE_operator(op);

    // Array assignment is an interesting statement
    if ((opr == OPR_ISTORE) && 
	(WN_operator(WN_kid1(stmt)) == OPR_ARRAY)) {

      // The LDID for the array name in OPR_ISTORE has to be skipped
      WN* lfs = WN_kid1(stmt);
      WN *skip_store_id = WN_kid0(lfs);

      // Process the reads (RHS)
      Walk_Rhs(stmt, skip_store_id);
      
      // Process the LHS for write
      ARA_REF* new_def = CXX_NEW(ARA_REF(lfs,WN_offset(stmt),this),&ARA_memory_pool);
      if (new_def->Has_Bad_Alias()) 
	CXX_DELETE(new_def, &ARA_memory_pool);
      else {
        if (!Info()->Has_Gotos||
	     ((!Info()->Has_Gotos_This_Level || !seen_non_scf) 
		&& !Info()->Has_Exits)) {
	  Add_Def(new_def);
        } else {
	  Add_May_Def(new_def);
	}
      }

      continue;

    } else if (opr == OPR_STID) {

      // Process the reads (RHS)
      Walk_Rhs(WN_kid0(stmt));

      // Process the LHS for scalar write
      if (!Info()->Has_Gotos||
	     ((!Info()->Has_Gotos_This_Level || !seen_non_scf) 
		&& !Info()->Has_Exits)) {
#ifdef KEY // bug 10316: cannot privatize a field in a struct because rest of
	   //	struct may be shared
	if (TY_kind(ST_type(WN_st(stmt))) != KIND_STRUCT ||
	    strncmp(TY_name(ST_type(WN_st(stmt))), ".dope.", 6) != 0)
#endif
        _scalar_def.Add_Scalar(stmt,0);
      }
      _scalar_may_def.Add_Scalar(stmt,0);

      continue;
      
    } else if (opr == OPR_CALL) {

      if (Has_Call_Info(stmt)==FALSE)
	continue;

      CALL_INFO* cli = Get_Call_Info(stmt);
      ARA_LOOP_INFO* ali = cli->Call_Ara_Info();
      Is_True(ali, 
	("ARA_LOOP_INFO::Walk_Block: No ARA_LOOP_INFO for this call"));

      // merge to current info
      Merge_Info(ali,seen_non_scf);

      continue;

    } else if (opr == OPR_DO_LOOP) {

      DO_LOOP_INFO* dli = Get_Do_Loop_Info(stmt);
      if (dli->Has_Exits) seen_non_scf = TRUE;
      Is_True(dli, ("ARA_LOOP_INFO::Walk_Block: No DO_LOOP_INFO for this loop"));
      ARA_LOOP_INFO* ali = dli->ARA_Info;
      Is_True(ali, ("ARA_LOOP_INFO::Walk_Block: No ARA_LOOP_INFO for this loop"));
      ali->Walk_Loop();

      // merge to current info
      Merge_Info(ali,seen_non_scf);

      // skip the rest and continue onto next statement
      // this is not very efficient
      do 
	stmt_iter = LWN_WALK_StmtNext(stmt_iter);
      while (stmt_iter && Wn_Is_Inside(stmt_iter->wn,stmt));
	  
      continue;

    } else if (opr == OPR_IF) {
      seen_non_scf = TRUE; // there might be a jump out of the if

      Walk_Rhs(WN_kid0(stmt));

      IF_INFO* ii = Walk_If(stmt);
      Is_True(ii, ("ARA_LOOP_INFO::Walk_Block: no IF_INFO"));
      ARA_LOOP_INFO *ali = ii->ARA_common();
      Is_True(ali, ("ARA_LOOP_INFO::Walk_Block: no ARA_common after Walk_If"));
      
      Merge_Info(ali,seen_non_scf);

      // skip the rest and continue onto next statement
      // this is not very efficient
      do 
	stmt_iter = LWN_WALK_StmtNext(stmt_iter);
      while (stmt_iter && Wn_Is_Inside(stmt_iter->wn,stmt));

      continue;

    } else if (OPCODE_is_non_scf(op) || OPCODE_is_scf(op)) {
      seen_non_scf = TRUE;
    } else if (OPCODE_is_not_executable(op) ||
	       OPCODE_is_prefetch(op))
      continue;
#ifdef KEY
    // Bug 7274 - check the ASM_INPUT if any
    else if (opr == OPR_ASM_STMT) {
      for (INT kid = 2; kid < WN_kid_count(stmt); ++kid) {
	WN* asm_input = WN_kid(stmt, kid);
	WN* load = WN_kid0(asm_input);
	// Process the reads
	Walk_Rhs(WN_kid0(asm_input));

	continue;
      }
    }
#endif

    // Process the reads (RHS)
    if (OPCODE_is_expression(op) || OPCODE_is_non_scf(op)) Walk_Rhs(stmt);

  } // while (stmt_iter)

}

static BOOL
Is_Loop_Guard(WN *if_stmt)
{
  OPERATOR opr = WN_operator(if_stmt);
  Is_True(opr == OPR_IF, ("Is_Loop_Guard: not a IF stmt"));
  
  if (WN_else_is_empty(if_stmt)) {
    WN *wn_then = WN_then(if_stmt);
    WN *wn = WN_first(wn_then);
    if (wn) {
      opr = WN_operator(wn);
      if (wn == WN_last(wn_then) && opr == OPR_DO_LOOP) {
	DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
	Is_True(dli, ("Is_Loop_Guard: No DO_LOOP_INFO for this loop"));
	return (dli->Guard == if_stmt);
      }
    }
  }
  return FALSE;
}

//=======================================================================
// Walk a structured IF stmt
//=======================================================================
IF_INFO *
ARA_LOOP_INFO::Walk_If(WN *if_stmt)
{
  IF_INFO* ii = (IF_INFO *) WN_MAP_Get(LNO_Info_Map, if_stmt);
  Is_True(ii, ("ARA_LOOP_INFO::Walk_If: no IF_INFO"));

  // Ignore the IF condition when the IF is the guard of a loop
  // TODO need guard this kind of loop if privatization is performed
  // outer loops.
  if (Is_Loop_Guard(if_stmt)) {
    WN *loop = WN_first(WN_then(if_stmt));
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
    Is_True(dli, ("ARA_LOOP_INFO::Walk_Loop: No DO_LOOP_INFO for this loop"));
    ARA_LOOP_INFO* ali = dli->ARA_Info;
    Is_True(ali, ("ARA_LOOP_INFO::Walk_Loop: No ARA_LOOP_INFO for this loop"));
    ali->Walk_Loop();
    ii->Set_ARA_common(ali);
    return ii;
  }

  ARA_LOOP_INFO *ara_common = CXX_NEW(ARA_LOOP_INFO(), &ARA_memory_pool);
  ara_common->Copy_Some_Values(this);
  ii->Set_ARA_common(ara_common);

  ARA_LOOP_INFO *ara_then = CXX_NEW(ARA_LOOP_INFO(), &ARA_memory_pool);
  ara_then->Copy_Some_Values(this);
  ii->Set_ARA_then(ara_then);

  ARA_LOOP_INFO *ara_else = CXX_NEW(ARA_LOOP_INFO(), &ARA_memory_pool);
  ara_else->Copy_Some_Values(this);
  ii->Set_ARA_else(ara_else);

  ara_then->Walk_Block(WN_then(if_stmt));
  ara_else->Walk_Block(WN_else(if_stmt));

  ara_common->Merge_then_else(ara_then, ara_else);

  return ii;
}

void
ARA_LOOP_INFO::Walk_Loop()
{
  if (_info->Suggested_Parallel) { 
    for (WN* wn = LWN_Get_Parent(_loop);wn != NULL;wn = LWN_Get_Parent(wn)) {
      if (WN_opcode(wn) == OPC_DO_LOOP) {
	DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
	if (dli->ARA_Info != NULL) 
	  dli->ARA_Info->_inner_loop_is_suggested_parallel = TRUE; 
      }
    }
  }
 
  if ( _info->Has_EH_Regions ) {
    Default_For_Bad_Loop();
    Set_To_Sequential();
    return;
  }
 
  if ( _info->Has_Exits || _info->Has_Bad_Mem ) {
    Default_For_Bad_Loop();
    return;
  }

  // Handle the initialization statement
  WN *stmt = WN_start(_loop);

  // Process the reads (RHS)
  Walk_Rhs(WN_kid0(stmt));

  _scalar_def.Add_Scalar(stmt,0);
  _scalar_may_def.Add_Scalar(stmt,0);

  // Walk the statements of the loop
  Walk_Block(WN_do_body(_loop));

  // Handles the step and end statements
  stmt = WN_step(_loop);

  Walk_Rhs(WN_kid0(stmt));

  _scalar_def.Add_Scalar(stmt,0);
  _scalar_may_def.Add_Scalar(stmt,0);
  
  // end test
  Walk_Rhs(WN_end(_loop));

  // aggregation
  Projection();

  // Handle Alias
  Test_Alias();

  INT i;
  // Set loop to sequential if there are scalar dependence.
  for (i = 0; i < _scalar_use.Elements(); ++i) {
    BOOL all_reduction = FALSE;
    if (red_manager) {
      all_reduction = TRUE;
      for (INT j = 0; 
	   all_reduction && (j<_scalar_use.Bottom_nth(i)->Elements()); ++j) {
	WN * use_wn = _scalar_use.Bottom_nth(i)->Bottom_nth(j)->Wn;
	if (red_manager->Which_Reduction(use_wn) == RED_NONE) {
	  all_reduction = FALSE;
	  break;
	}
      }
    }
    
    if (!all_reduction) {
      SYMBOL & use_sym = _scalar_use.Bottom_nth(i)->_scalar;
      for (INT j = 0; j < _scalar_may_def.Elements(); ++j) {
	if (use_sym == _scalar_may_def.Bottom_nth(j)->_scalar) {
	  if (LNO_Prompl) {
            INT k;
	    for (k = 0; k < _scalar_vars.Elements(); k++) 
              if (_scalar_vars.Bottom_nth(k) == use_sym)
	        break;
	    if (k == _scalar_vars.Elements())
	      _scalar_vars.Push(use_sym); 
	  } 
	  Set_To_Sequential();
	  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
	    this->Print(stdout);
	  } else if (!LNO_Prompl)
	    break;
	}
      }
    }
  }

  if (red_manager) {
    for (i = 0; i < _reduction.Elements(); ++i) {
      WN *red = _reduction.Bottom_nth(i);
      if (WN_operator(red) == OPR_STID || WN_operator(red) == OPR_LDID) {
	SYMBOL sym(red);
	for (INT j = 0; j < _scalar_may_def.Elements(); ++j) {
	  SCALAR_NODE * scalar_node = _scalar_may_def.Bottom_nth(j);
	  if (sym == scalar_node->_scalar) {
	    for (INT l = 0; l < scalar_node->Elements(); ++l) {
	      WN * def_scalar = scalar_node->Bottom_nth(l)->Wn;
	      if (red_manager->Which_Reduction(def_scalar) != 
		  red_manager->Which_Reduction(red)) {
		Set_To_Sequential();
		if (LNO_Prompl) {
                  INT k;
		  for (k = 0; k < _scalar_vars.Elements(); k++) 
		    if (_scalar_vars.Bottom_nth(k) == sym)
		      break;
		  if (k == _scalar_vars.Elements())
		    _scalar_vars.Push(sym); 
		}
		break;
	      }
	    }
	    break;
	  }
	}
      }
    }
  }

  // Definition overlap (array base changes in loop)
  for (i = 0; i < _scalar_may_def.Elements(); ++i) {
    SYMBOL & sym = _scalar_may_def.Bottom_nth(i)->_scalar;
    INT j;
    for (j = 0; j < _use.Elements(); ++j) {
      ARA_REF *ref = _use.Bottom_nth(j);
      if (sym == ref->Array()) {
	Set_To_Sequential();
	if (LNO_Prompl) {
          INT k;
	  for (k = 0; k < _scalar_vars.Elements(); k++) 
	    if (_scalar_vars.Bottom_nth(k) == sym)
	      break;
	  if (k == _scalar_vars.Elements())
	    _scalar_vars.Push(sym); 
	}
	goto next_s_def;
      }
    }
    for (j = 0; j < _def.Elements(); ++j) {
      ARA_REF *ref = _def.Bottom_nth(j);
      if (sym == ref->Array()) {
	Set_To_Sequential();
	if (LNO_Prompl) {
          INT k;
	  for (k = 0; k < _scalar_vars.Elements(); k++) 
	    if (_scalar_vars.Bottom_nth(k) == sym)
	      break;
	  if (k == _scalar_vars.Elements())
	    _scalar_vars.Push(sym); 
	}
	goto next_s_def;
      }
    }
    for (j = 0; j < _pri.Elements(); ++j) {
      ARA_REF *ref = _pri.Bottom_nth(j);
      if (sym == ref->Array()) {
	Set_To_Sequential();
	if (LNO_Prompl) {
          INT k;
	  for (k = 0; k < _scalar_vars.Elements(); k++) 
	    if (_scalar_vars.Bottom_nth(k) == sym)
	      break;
	  if (k == _scalar_vars.Elements())
	    _scalar_vars.Push(sym); 
	}
	goto next_s_def;
      }
    }

  next_s_def:
    ;
  }

  for (i = 0; i < _bad_alias.Elements(); ++i) {
    if (_bad_alias.Bottom_nth(i)) {
      if (LNO_Prompl){ 
	SYMBOL use_sym = _scalar_pri.Bottom_nth(i)->_scalar; 
        INT j;
	for (j = 0; j < _scalar_alias.Elements(); j++) 
	  if (_scalar_alias.Bottom_nth(j) == use_sym)
	    break; 
	if (j == _scalar_alias.Elements())
	  _scalar_alias.Push(use_sym); 
      } 
      Set_To_Sequential();
      if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
	fprintf(stdout,"Loop is sequential because bad alias for scalars\n");
      } else if (!!LNO_Prompl) 
	break;
    }
  }
  
  // Set the def only variables to be private
  for (i = 0; i < _scalar_def.Elements(); ++i) {
    SYMBOL &def_sym = _scalar_def.Bottom_nth(i)->_scalar;
    INT j;
    for (j = 0; j < _reduction.Elements(); ++j) {
      WN * cur = _reduction.Bottom_nth(j);
      if (WN_operator(cur) == OPR_LDID) {
	SYMBOL symbol(cur);
	if (symbol==def_sym) {
	  goto next_iteration;
	}
      }
    }
    for (j = 0; j < _scalar_use.Elements(); ++j) {
      if (def_sym == _scalar_use.Bottom_nth(j)->_scalar) {
	break;
      }
    }
    if (j==_scalar_use.Elements()) {
      SCALAR_NODE* sn_def = _scalar_def.Bottom_nth(i);
      SYMBOL* sym_def = &sn_def->_scalar;
      for (INT j = 0; j < sn_def->Elements(); ++j) {
	WN* wn_def = sn_def->Bottom_nth(j)->Wn;
#ifdef KEY // bug 10316: cannot privatize a field in a struct because rest of
	   //	struct may be shared
	if (WN_operator(wn_def) == OPR_STID &&
	    TY_kind(ST_type(WN_st(wn_def))) == KIND_STRUCT &&
	    strncmp(TY_name(ST_type(WN_st(wn_def))), ".dope.", 6) == 0)
	  break;
#endif
	if (OPCODE_is_call(WN_opcode(wn_def))
	    || WN_operator(wn_def) == OPR_LDA) { 
	  _scalar_pri.Add_Scalar(wn_def, sym_def, 0);
        } else { 
	  _scalar_pri.Add_Scalar(wn_def, 0);
        } 
      }
    }
  next_iteration:
      ;
  }

  // Set ARA_REF in Pri to be loop invariant 
  // if every WN in the set is loop invariant
  Annotate_Invariant_Def();
  Annotate_Invariant_Pri();

  for (i = 0; i < _def.Elements(); i++) {
    if (_def.Bottom_nth(i)->Is_Loop_Invariant()   
        && Overlap_Exposed_Array(_def.Bottom_nth(i)->Array())) { 
      Set_To_Sequential(); 
      if (LNO_Prompl) { 
	const SYMBOL& sym_array = _def.Bottom_nth(i)->Array();
        INT j;
	for (j = 0; j < Partial_Array_Sec().Elements(); j++) 
	  if (Partial_Array_Sec().Bottom_nth(j) == sym_array)
	    break; 
	if (j == Partial_Array_Sec().Elements())  
	  Partial_Array_Sec().Push(sym_array);
      } 
      break;
    }
  } 

  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    this->Print(stdout);
  }

}

//-----------------------------------------------------------------------
// NAME: ARA_LOOP_INFO::Bad_Array_Dependence
// FUNCTION: Record that there is a loop carried dependence on the loop 
//   represented by the ARA_LOOP_INFO between 'wn_source' and 'wn_sink'.  
//-----------------------------------------------------------------------

void ARA_LOOP_INFO::Bad_Array_Dependence(WN* wn_source, 
			                 WN* wn_sink)
{
  if (!LNO_Analysis && !LNO_Prompl) 
    return;
  INT ln_source = WN_Whirl_Linenum(wn_source); 
  INT ln_sink = WN_Whirl_Linenum(wn_sink); 
  WN* wn_array_source = WN_Array_Symbol(wn_source);
  WN* wn_array_sink = WN_Array_Symbol(wn_sink);
  // Unknown reason should be printed in the following case. 
  if (wn_array_source == NULL || wn_array_sink == NULL)
    return; 
  SYMBOL sym_source = SYMBOL(wn_array_source); 
  SYMBOL sym_sink = SYMBOL(wn_array_sink); 
  INT i;
  for (i = 0; i < Dep_Vars().Elements(); i++) 
    if (Dep_Source().Bottom_nth(i) == sym_source
        && Dep_Sink().Bottom_nth(i) == sym_sink
        && Ln_Dep_Source().Bottom_nth(i) == ln_source 
	&& Ln_Dep_Sink().Bottom_nth(i) == ln_sink)
      break; 
  if (i == Dep_Vars().Elements()) {
    Dep_Vars().Push(sym_source);
    Dep_Source().Push(sym_source);
    Dep_Sink().Push(sym_sink);
    Ln_Dep_Source().Push(ln_source); 
    Ln_Dep_Sink().Push(ln_sink); 
  }
}

//=============================================================================
// 
// Set the enclosing loops' dependence distance according to the wn's 
// dependences. We only need to look at the edges where 'wn' are the 
// sinks.
//
//=============================================================================
void Parallelization_Process_Deps(WN *wn)
{

  VINDEX16 array_v = Array_Dependence_Graph->Get_Vertex(wn);
  Is_True(array_v,("Parallelization_Process_Deps: WN with NULL vertex"));
  
  for (EINDEX16 in_edge=Array_Dependence_Graph->Get_In_Edge(array_v); 
       in_edge; in_edge=Array_Dependence_Graph->Get_Next_In_Edge(in_edge)) {
    WN *source = Array_Dependence_Graph->Get_Wn(Array_Dependence_Graph->Get_Source(in_edge));
    if (Edge_Is_Reduction_Dependence(in_edge, Array_Dependence_Graph, red_manager)) {

      if (WN_operator(wn) == OPR_ISTORE ||
	  WN_operator(wn) == OPR_STID) {
	ACCESS_ARRAY *s_array = NULL;
        if (WN_operator(wn) == OPR_ISTORE) {
          s_array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,WN_kid1(wn));
        }

	DEPV_ARRAY *d_array = Array_Dependence_Graph->Depv_Array(in_edge);
	for (INT i = 0; i < d_array->Num_Vec(); ++i) {
	  DEPV* depv = d_array->Depv(i);
	  
          INT j;
	  for (j = 0; (j<d_array->Num_Dim()) && 
	       (DEP_Direction(DEPV_Dep(depv,j))==DIR_EQ); ++j);
	
	  if (j==d_array->Num_Dim()) continue;

	  // Found the loop level that carries the dependency
	  INT carry_level = j+d_array->Num_Unused_Dim();
	
	  // Set the dependence distance in the carry_level
	  WN* common_loop = LNO_Common_Loop(source,wn);
	  Is_True(common_loop,("Walk_Loop_Dependence: No common loop between source and sink"));
	
	  DO_LOOP_INFO* dli = Get_Do_Loop_Info(common_loop);
	  FmtAssert(dli && (dli->Depth>=carry_level),
		    ("Walk_Loop_Dependence: No common loop carries the dependence between source and sink"));

	  while (dli->Depth>carry_level) {
	    common_loop = Enclosing_Do_Loop(LWN_Get_Parent(common_loop));
	    dli = Get_Do_Loop_Info(common_loop);
	  }
	 
	  Is_True(dli->Depth==carry_level,("Walk_Loop_Dependence: dli->Depth != carry_level"));
	
	  ARA_LOOP_INFO* ali = dli->ARA_Info;
	  Is_True(ali,
		  ("Parallelization_Process_Deps: No ARA_LOOP_INFO for reduction"));
	  if (!LNO_Prompl && (!ali->Dep_Is_Good() || ali->Dep_Dist() != 0) ) continue;

	  BOOL is_invariant;
          if (s_array == NULL) {
            // was an OPR_STID, is invariant
            is_invariant = TRUE;
          }
          else {
            is_invariant = s_array&&Loop_Invariant_Inside(s_array,common_loop);
          }
#ifdef KEY // bug 10707: screen out cases where the variable is of complex type
    	   //		 but the code is operating on float types
          if (WN_operator(wn) == OPR_ISTORE &&
	      WN_operator(WN_kid1(wn)) == OPR_ARRAY &&
    	      WN_operator(WN_kid0(WN_kid1(wn))) == OPR_LDID) {
	    TY_IDX ty = ST_type(WN_st(WN_kid0(WN_kid1(wn))));
	    if (TY_kind(ty) == KIND_POINTER)
	      ty = TY_pointed(ty);
	    if (TY_kind(ty) == KIND_ARRAY) {
	      ty = TY_etype(ty);
	    if (! MTYPE_is_complex(WN_desc(wn)) && MTYPE_is_complex(TY_mtype(ty)))
	      is_invariant = FALSE;
	    }
	  }
#endif
	  BOOL privatizable = FALSE;
	  // OK, one final check to see if the array caused the dependence
	  // is privatizable.
	  // First check if the dependence is memory related 
	  if (WN_operator(source) == OPR_ILOAD) {
	    privatizable = ali->Is_Privatizable(source);
	  } else if (WN_operator(wn) == OPR_ILOAD) {
	    privatizable = ali->Is_Privatizable(wn);
	  } else {// two stores
	    privatizable = (ali->Is_Privatizable(source) && 
			    ali->Is_Privatizable(wn));
	  }

	  if (privatizable && !ali->Need_Copyin()) continue;

	  if (!privatizable && is_invariant
#ifdef KEY // bug 9112 : MP lower cannot handle extra offset field in reduction
	   //		 based on an invariant array element
	      && (s_array == NULL || WN_offset(wn) == 0)
#endif
	     ) 
	    ali->Add_Reduction(wn);
	  else	if (DEP_IsDistance(DEPV_Dep(depv,j))) {
	    ali->Add_Dependence(DEP_Distance(DEPV_Dep(depv,j)));
	    ali->Bad_Array_Dependence(source, wn); 
	  } else {
	    ali->Set_To_Sequential();
	    ali->Bad_Array_Dependence(source, wn); 
	  }
	}
      }
      
    } else {
      DEPV_ARRAY *d_array = Array_Dependence_Graph->Depv_Array(in_edge);
      for (INT i = 0; i < d_array->Num_Vec(); ++i) {
	DEPV* depv = d_array->Depv(i);
	
        INT j;
	for (j = 0; (j<d_array->Num_Dim()) && 
	     (DEP_Direction(DEPV_Dep(depv,j))==DIR_EQ); ++j);
	
	if (j==d_array->Num_Dim()) continue;

	// Found the loop level that carries the dependency
	INT carry_level = j+d_array->Num_Unused_Dim();
	
	// Set the dependence distance in the carry_level
	WN* common_loop = LNO_Common_Loop(source,wn);
	Is_True(common_loop,("Walk_Loop_Dependence: No common loop between source and sink"));
	
	DO_LOOP_INFO* dli = Get_Do_Loop_Info(common_loop);
	FmtAssert(dli && (dli->Depth>=carry_level),
		("Walk_Loop_Dependence: No common loop carries the dependence between source and sink"));

	while (dli->Depth>carry_level) {
	  common_loop = Enclosing_Do_Loop(LWN_Get_Parent(common_loop));
	  dli = Get_Do_Loop_Info(common_loop);
	}
	 
	Is_True(dli->Depth==carry_level,("Walk_Loop_Dependence: dli->Depth != carry_level"));
	
	ARA_LOOP_INFO* ali = dli->ARA_Info;
	Is_True(ali,("Walk_Loop_Dependence: No ARA_LOOP_INFO"));

	if (!LNO_Prompl && (!ali->Dep_Is_Good() || ali->Dep_Dist() != 0) ) continue;

	// OK, one final check to see if the array caused the dependence
	// is privatizable.
	// First check if the dependence is memory related 
	if (WN_operator(source) == OPR_ILOAD) {
	  if (ali->Is_Privatizable(source) && !ali->Need_Copyin())
	    continue;
	} else if (WN_operator(wn) == OPR_ILOAD) {
	  if (ali->Is_Privatizable(wn) && !ali->Need_Copyin())
	    continue;
	} else {// two stores
	  if (ali->Is_Privatizable(source) && ali->Is_Privatizable(wn) &&
	      !ali->Need_Copyin()) continue;
	}

	if (DEP_IsDistance(DEPV_Dep(depv,j))) {
	  ali->Add_Dependence(DEP_Distance(DEPV_Dep(depv,j)));
	} else {
	  ali->Set_To_Sequential();
	}
	ali->Bad_Array_Dependence(source, wn); 
      }
    }
  }
}
	  
//=============================================================================
//
// Walk the WN's of a function to determine array dependences for each loop
//
//=============================================================================
extern void Walk_Loop_Dependence(WN * func_nd)
{
  
  // Walk through to collect the refs
  for (LWN_ITER *iter = LWN_WALK_TreeIter(func_nd);
       iter; iter = LWN_WALK_TreeNext(iter)) {

    WN * wn = iter->wn;
    
    OPCODE op = WN_opcode(wn);
    if (OPCODE_is_load(op) || OPCODE_is_store(op) || OPCODE_is_call(op)) {
      if (Array_Dependence_Graph->Get_Vertex(wn)) 
	Parallelization_Process_Deps(wn);
      else if (OPCODE_operator(op) != OPR_LDID && OPCODE_operator(op) != OPR_STID) {

	// Something is dependent on everything, no enclosing loop can be
	// parallel
	for (WN* loop_enc = Enclosing_Loop(wn); 
	     loop_enc; loop_enc = Enclosing_Loop(LWN_Get_Parent(loop_enc))) {
	  if (WN_operator(loop_enc) == OPR_DO_LOOP) {
	    DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop_enc);
	    Is_True(dli,("Walk_Loop_Dependence: No DO_LOOP_INFO")); 
	    ARA_LOOP_INFO* ali = dli->ARA_Info;
	    Is_True(ali,("Walk_Loop_Dependence: No ARA_LOOP_INFO")); 
	    if (LNO_Prompl) {
	      INT ln = WN_Whirl_Linenum(wn); 
	      if (OPCODE_is_call(WN_opcode(wn))) {  
		const char* call_name = WB_Whirl_Symbol(wn);
		if (call_name == NULL)
		  call_name = "";
		INT length = strlen(call_name);
		char* name = 
			CXX_NEW_ARRAY(char, length + 1, &ARA_memory_pool);
		strcpy(name, call_name);
		if (WN_operator(wn) == OPR_CALL
		    && PU_src_lang(Get_Current_PU()) == PU_F77_LANG)
		  name[length - 1] = '\0';
                INT i;
		for (i = 0; i < ali->Call_No_Dep_Vars().Elements(); i++) 
		  if (!strcmp(ali->Call_No_Dep_Vars().Bottom_nth(i), name)
		      && ali->Ln_Call_No_Dep_Vars().Bottom_nth(i) == ln)
		    break; 
		if (i == ali->Call_No_Dep_Vars().Elements()) {
		  ali->Call_No_Dep_Vars().Push(name); 
		  ali->Ln_Call_No_Dep_Vars().Push(ln); 
		} 
	      } else if (WN_operator(wn) == OPR_ISTORE 
		|| WN_operator(wn) == OPR_ILOAD) {
		SYMBOL* sym = CXX_NEW(SYMBOL(WN_Array_Symbol(wn)),
		  &ARA_memory_pool);   
                INT i;
		for (i = 0; i < ali->Array_No_Dep_Vars().Elements(); i++) 
		  if (ali->Array_No_Dep_Vars().Bottom_nth(i) == *sym
		      && ali->Ln_Array_No_Dep_Vars().Bottom_nth(i) == ln)
		    break; 
		if (i == ali->Array_No_Dep_Vars().Elements()) {
		  ali->Array_No_Dep_Vars().Push(*sym);  
		  ali->Ln_Array_No_Dep_Vars().Push(ln);  
                }
              } else { 
                INT i;
		for (i = 0; i < ali->Ln_Misc_No_Dep_Vars().Elements(); i++) 
		  if (ali->Ln_Misc_No_Dep_Vars().Bottom_nth(i) == ln)
		    break; 
		if (i == ali->Ln_Misc_No_Dep_Vars().Elements())
		  ali->Ln_Misc_No_Dep_Vars().Push(ln);  
	      } 
	    } 
	    ali->Set_To_Sequential();
	    if (LNO_Analysis) {
	      WN* wn_array = (WN_operator(wn) == OPR_ISTORE) ?
		WN_kid1(wn) : WN_kid0(wn);
	      if (WN_operator(wn_array) == OPR_PARM) 
		wn_array = WN_kid0(wn_array);
	      SYMBOL sym = (WN_operator(wn_array) == OPR_ARRAY)?
		SYMBOL(WN_array_base(wn_array)) : SYMBOL(wn_array);
              INT i;
	      for (i = 0; i < ali->Dep_Vars().Elements(); ++i) {
		if (sym==ali->Dep_Vars().Bottom_nth(i)) break;
	      }
	      if (i==ali->Dep_Vars().Elements())
		ali->Dep_Vars().Push(sym);
	    }

	  }
	}
      }
    }
  }

}

static
void Set_Enclosing_If_Has_Region(WN *wn)
{
  for (WN *wn_temp = wn; wn_temp; wn_temp = LWN_Get_Parent(wn_temp)) {
    if (WN_opcode(wn_temp) == OPC_IF) {
      IF_INFO * ii = Get_If_Info(wn_temp, TRUE);
      if (ii) ii->Contains_Regions = TRUE;
    }
  }
}

// Count the number of floating point operations
static
INT64 Loop_FP_Size(WN* wn)
{
  OPCODE opcode = WN_opcode(wn);
  if (OPCODE_is_leaf(opcode))
    return 1;
  else if (OPCODE_is_load(opcode))
    return 1;
  else if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    INT64 count = 0;
    while (kid) {
      count += Loop_FP_Size(kid);
      kid = WN_next(kid);
    }
    return count;
  } else if (opcode == OPC_DO_LOOP) {
    INT64 count = Loop_FP_Size(WN_start(wn));
    count += Loop_FP_Size(WN_end(wn));
    INT64 count1 = Loop_FP_Size(WN_do_body(wn));
    count1 += Loop_FP_Size(WN_step(wn));
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
    if (dli) {
      count1 *= MAX(1,dli->Est_Num_Iterations);
    }
    return (count+count1);
  }

  OPERATOR oper = OPCODE_operator(opcode);
  
  INT64 count = 0;
  INT kid_cnt = WN_kid_count(wn);

  if ((oper == OPR_TRUNC) || (oper == OPR_RND) ||
      (oper == OPR_CEIL) || (oper == OPR_FLOOR) || (oper == OPR_INTRINSIC_OP)) {
    count++;
  } else if ((oper == OPR_REALPART) || (oper == OPR_IMAGPART) ||
	     (oper == OPR_PARM) || (oper == OPR_PAREN)) {
    // no-ops
  } else if (OPCODE_is_expression(opcode) && (oper != OPR_CONST)) {
    if ((oper == OPR_MAX) || (oper == OPR_MIN) || 
	(oper == OPR_ADD) || (oper == OPR_SUB) || (oper == OPR_MPY) ||
	(oper == OPR_NEG))
      count++;
    else if ((oper == OPR_DIV || oper == OPR_SQRT))
      count = count + 10;
    
  } else if (OPCODE_is_store(opcode)) {
    count++;
    kid_cnt = kid_cnt - 1;
  }
  
  for (INT kidno=0; kidno<kid_cnt; kidno++) {
    WN *kid = WN_kid(wn,kidno);
    count += Loop_FP_Size(kid);
  }

  return count;
  
}

// ========================================================================
// Determine if any of its inner loops has bounds depends its index
// ========================================================================
BOOL
ARA_LOOP_INFO::Bounds_Depend_On_Index(INT depth)
{
  if (!_info) return FALSE;

  INT i;
  if (!_info->LB->Too_Messy) {
    for (i = 0; i < _info->LB->Num_Vec(); ++i) {
      if (!_info->LB->Too_Messy && _info->LB->Dim(i)->Loop_Coeff(depth)!=0) 
	return TRUE;
    }
  }
  if (!_info->UB->Too_Messy) {
    for (i = 0; i < _info->UB->Num_Vec(); ++i) {
      if (!_info->UB->Too_Messy && _info->UB->Dim(i)->Loop_Coeff(depth)!=0)
	return TRUE;
    }
  }
  for (i = 0; i < _children.Elements(); ++i) {
    if (_children.Bottom_nth(i)->Bounds_Depend_On_Index(depth))
      return TRUE;
  }

  return FALSE;
}
  
BOOL
ARA_LOOP_INFO::Variable_Load()
{
  for (INT i = 0; i < _children.Elements(); ++i) {
    if (_children.Bottom_nth(i)->Bounds_Depend_On_Index(Depth())) return TRUE;
  }
  return FALSE;
}

WN* ARA_LOOP_INFO::Create_Old_IF_Clause()
{
  INT64 fpcount = Loop_FP_Size(WN_do_body(_loop));
  fpcount += Loop_FP_Size(WN_step(_loop));
  if (fpcount==0) {
    return LWN_Make_Icon(Boolean_type, 0);
  }

  INT64 threshed = 1200/fpcount;
  if (threshed<2) {
    return LWN_Make_Icon(Boolean_type, 1);
  }

  ACCESS_VECTOR *svec = _info->Step;
  if (svec->Too_Messy || !svec->Is_Const() || (svec->Const_Offset == 0)) 
    return NULL;
  threshed *= svec->Const_Offset;
  threshed -= svec->Const_Offset;
  
  WN* init = LWN_Copy_Tree(WN_kid0(WN_start(_loop)));
  LWN_Copy_Def_Use(WN_kid0(WN_start(_loop)), init, Du_Mgr);
  WN* end = LWN_Copy_Tree(WN_kid1(WN_end(_loop)));
  LWN_Copy_Def_Use(WN_kid1(WN_end(_loop)), end, Du_Mgr);

  TYPE_ID ind_type = Do_Wtype(_loop);
  OPCODE subop = OPCODE_make_op(OPR_SUB, Promote_Type(ind_type), MTYPE_V);
  WN* cons_threshed = LWN_Make_Icon(Promote_Type(ind_type), threshed);
  WN* trip_count = LWN_CreateExp2(subop, end, init);

  WN *if_clause = NULL;
  if (svec->Const_Offset > 0) {
    OPCODE geop = OPCODE_make_op(OPR_GE, Boolean_type, Promote_Type(ind_type));
    if_clause = LWN_CreateExp2(geop, trip_count, cons_threshed);
  } else {
    OPCODE leop = OPCODE_make_op(OPR_LE, Boolean_type, Promote_Type(ind_type));
    if_clause = LWN_CreateExp2(leop, trip_count, cons_threshed);
  }
  return if_clause;
}

//-----------------------------------------------------------------------
// NAME: Current_Numprocs
// FUNCTION: Return the number of processors which we believe will execute 
//   if 'wn_loop' goes parallel.  If 'is_pdo', we are creating a PDO, other-
//   wise a DOACROSS loop. 
//-----------------------------------------------------------------------

static WN* Current_Numprocs(WN* wn_loop,
			    BOOL is_pdo)
{ 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  WN* wn_num_threads = NULL; 
  if (!LNO_Pseudo_Lower || dli->Mp_Info != NULL 
      && dli->Mp_Info->Plower_Disabled()) {
    wn_num_threads = LWN_Make_Icon(MTYPE_I4, NOMINAL_PROCS); 
  } else if (is_pdo) { 
    wn_num_threads = Get_Runtime_Cur_Numthreads_Ldid(); 
  } else { 
    wn_num_threads = Get_Runtime_Numthreads_Ldid(); 
  } 
  return wn_num_threads; 
}

// Given an ARRAY node that's the kid of a REDUCTION XPRAGMA, return
// the array base ST.

static ST *
Find_Reduction_Array_Base(WN *wn_array)
{
  Is_True(WN_operator(wn_array) == OPR_ARRAY, ("not an ARRAY node"));
  WN *base = WN_kid0(wn_array);
  Is_True(base, ("NULL array base?!?"));

  switch (WN_operator(base)) {
  case OPR_LDA:   // base is a variable's address
  case OPR_LDID:  // base stored in scalar pointer
    Is_True(WN_st(base), ("NULL array base?!?"));
    return WN_st(base);
  case OPR_ILOAD: // indirect another level to find the base
    return Find_Reduction_Array_Base(WN_kid0(base));
  default:
    break;
  }

  Fail_FmtAssertion("unexpected operator %d in base expression",
                    (INT) WN_operator(base));
  return NULL; 
}

static void
Set_Reduction_Array_Base_is_shared_auto(WN *wn_array)
{
  Is_True(WN_operator(wn_array) == OPR_ARRAY, ("not an ARRAY node"));
  WN *base = WN_kid0(wn_array);
  Is_True(base, ("NULL array base?!?"));

  switch (WN_operator(base)) {
  case OPR_LDA:
    if (WN_sclass(base) == SCLASS_AUTO)
      Set_ST_is_shared_auto(*WN_st(base));  // fixed-length auto array
    break;
  case OPR_LDID:                            // variable-length auto array
    break;  // don't set the bit: memory is alloca'd, which is treated
            // more conservatively by optimizer than plain stack variables
  default:
    break;
  }
}

// Return list of STORE/ISTORE nodes for reductions.  In general there can
// be more than one node per reduction (e.g. on different branches of an
// IF), so return just the first one we find for each reduction.
void ARA_LOOP_INFO::Reduction_List(REDUCTION_LIST *rlist)
{
  REDUCTION_MANAGER *rm = red_manager;
  if (rm == NULL || _reduction.Elements() == 0)
    return;

    // REDUCTION pragmas not created yet, so make temporary ones
  DYN_ARRAY<WN *> reductions(&LNO_local_pool);
  INT i;
  for (i = 0; i < _reduction.Elements(); i++) {
    WN *wn = _reduction.Bottom_nth(i), *prag;
    if (WN_operator(wn) == OPR_ISTORE) {
      prag = WN_CreateXpragma(WN_PRAGMA_REDUCTION, ST_IDX_ZERO, 1);
      WN_kid0(prag) = LWN_Copy_Tree(WN_kid1(wn));
    } else
      prag = WN_CreatePragma(WN_PRAGMA_REDUCTION, WN_st(wn), 
                             WN_offset(wn), 0);

    reductions.AddElement(prag);
  }

    // maps REDUCTION pragmas to corresponding store nodes
  HASH_TABLE<WN *, WN *> redn_to_store(17, &LNO_local_pool);

  LWN_ITER* itr = LWN_WALK_TreeIter(_loop);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    OPERATOR opr = WN_operator(wn);

    if ((opr != OPR_STID && opr != OPR_ISTORE) ||
        rm->Which_Reduction(wn) == RED_NONE)
      continue;

    for (i = 0; i < reductions.Elements(); i++) {
      WN *redn = reductions[i];
      if (WN_Store_Target_Matches_Reduction(wn, redn) &&
          !redn_to_store.Find(redn)) {
        redn_to_store.Enter(redn, wn);
        rlist->AddElement(wn);
      }
    }
  }

#ifdef Is_True_On
    // validate that we found at least one store per reduction
  for (i = 0; i < reductions.Elements(); i++) {
    WN *redn = reductions[i];
    if (redn_to_store.Find(redn))
      continue;
    Fail_FmtAssertion("could not find any store nodes for reduction");
  }
#endif

  for (i = 0; i < reductions.Elements(); i++)
    LWN_Delete_Tree(reductions[i]);
}

//-----------------------------------------------------------------------
// NAME: ARA_LOOP_INFO::Tc_Parallel_Cost
// FUNCTION: Return the value of Tc in the formula for parallel run-time
//   cost heuristic described below. 
//-----------------------------------------------------------------------

float ARA_LOOP_INFO::Tc_Parallel_Cost()
{ 
  BOOL dummy; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(_loop);
  REDUCTION_LIST rlist(&LNO_local_pool);
  Reduction_List(&rlist);
  float Basic_Overhead = (float) LNO_Parallel_Overhead;
  float Doacross_Overhead = dli->Is_Doacross 
    ? (float) dli->Doacross_Overhead : 0.0; 
  float Reduction_Overhead = rlist.Elements() > 0
    ? (float) MP_Reduction_Combine_Cycles(&rlist, &dummy) : 0.0; 
  return Basic_Overhead + Doacross_Overhead + Reduction_Overhead; 
} 

//-----------------------------------------------------------------------
// NAME: ARA_LOOP_INFO::Tp_Parallel_Cost
// FUNCTION: Return the value of Tp in the formula for parallel run-time
//   cost heuristic described below. 
//-----------------------------------------------------------------------

float ARA_LOOP_INFO::Tp_Parallel_Cost()
{ 
#ifdef KEY // bug 7772
  return LNO_Parallel_per_proc_overhead;
#else
  return 123.0;
#endif
} 

//-----------------------------------------------------------------------
// NAME: WN_Single_Iteration_Cost
// FUNCTION: Create a whirl expression which is an estimate of cost of 
//   executing a single iteration of the loop 'wn_loop'.  If 'include_calls',
//   include the cost of executing the calls, otherwise count the cost of
//   calls as 0. 
//-----------------------------------------------------------------------

static WN* WN_Single_Iteration_Cost(WN* wn_loop,  
				    BOOL include_calls)
{ 
  double cost_wo_calls = Single_Iteration_Cost(wn_loop, FALSE);
  if (!include_calls)
    return Make_Const(Host_To_Targ_Float(MTYPE_F8, (float) cost_wo_calls));
  WN* wn_base = NULL; 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) { 
    WN* wn = itr->wn; 
    if (WN_operator(wn) == OPR_CALL) { 
      WN* wn_call_cost = Execution_Cost(IPA_LNO_File, wn, MTYPE_I4); 
      if (wn_base == NULL) { 
	wn_base = wn_call_cost;
      } else { 
	TYPE_ID mtype = Cast_Float_Operands(&wn_base, &wn_call_cost);
        OPCODE op = OPCODE_make_op(OPR_ADD, mtype, MTYPE_V);
	wn_base = LWN_CreateExp2(op, wn_base, wn_call_cost);
      } 
    } 
  } 
  WN* wn_float_call_cost = wn_base; 
  TYPE_ID mtype_start = OPCODE_rtype(WN_opcode(wn_base));
  if (mtype_start != MTYPE_F8) { 
    OPCODE op_cvt = OPCODE_make_op(OPR_CVT, MTYPE_F8, mtype_start);
    wn_float_call_cost = LWN_CreateExp1(op_cvt, wn_base);
  } 
  WN* wn_float_other_cost 
    = Make_Const(Host_To_Targ_Float(MTYPE_F8, (float) cost_wo_calls));
  OPCODE op_add = OPCODE_make_op(OPR_ADD, MTYPE_F8, MTYPE_V);
  return LWN_CreateExp2(op_add, wn_float_call_cost, wn_float_other_cost); 
} 

//-----------------------------------------------------------------------
// NAME: ARA_LOOP_INFO::Create_New_IF_Clause 
// FUNCTION: Create an IF clause to test whether we go parallel according 
//   to this formula (for release 7.2):
//
//      Tc + P * Tp + W / P < W
//
//   where:
//
//      Tc is a constant overhead for going parallel (currently assumed to
//         be 1411 cycles, based on measurement of empty DOACROSS loops).
//         In addition, if the loop is to be parallelized using the
//         "doacross" rather than "doall" technique, we add to Tc an
//         estimated cost for the additional pipeline and synchronization
//         costs of "doacross".  Since the real "doacross" costs vary with
//         P, the array size, and the tile size, this constant is obviously
//         a crude approximation.  If the loop contains any reductions,
//         we also add in an estimated cost for combining the partial
//         reduction results computed by each processor.
//      Tp is a per-processor overhead for going parallel (currently
//         assumed to be 123 cycles, based on measurement of empty DOACROSS
//         loops)
//      P is the number of processors (from mp_sug_numthreads)
//      W is an estimate of the serial work (number of cycles) per
//         iteration of the loop, based on a combination of the machine
//         operation and cache miss cost estimates for the loop
//
//   To avoid the divide and redundant calculation of W, we rewrite the
//   formula as:
//
//      P * (Tc + P * Tp) < W * (P - 1)
//
//   Most of the arithmetic is done in floating point because we believe
//   this will be faster than the corresponding integer arithmetic.
//-----------------------------------------------------------------------

WN* ARA_LOOP_INFO::Create_New_IF_Clause(BOOL is_pdo)
{
  if (!PU_has_mp(Get_Current_PU()))
    Mp_File_Init();
  WN* wn_outer = _loop; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_outer); 
  if (dli->Suggested_Parallel && dli->Work_Estimate == 0) 
    DevWarn("Work Estimate for loop %s at %d is 0", WB_Whirl_Symbol(wn_outer),
      Srcpos_To_Line(WN_linenum(wn_outer))); 
  if (!dli->Suggested_Parallel) {
      // This can only happen in the case that Robert's and Peng's
      // selections of which loop to parallelize are not in agreement.
      // This problem, hence the following code, should go away in the
      // future--DRK.

    dli->Work_Estimate = Single_Iteration_Cost(wn_outer, TRUE);
    DevWarn("Parallelizing Unexpected Loop: Using Work Estimate of %.2f", 
      dli->Work_Estimate); 
  }

  // Never enough work. 
  if (Not_Enough_Parallel_Work()) 
    return LWN_Make_Icon(MTYPE_I4, 0); 

  // Always enough work. 
  BOOL has_left_right = FALSE;
  INT left = -1;
  INT right = -1;
  if (Always_Enough_Parallel_Work(&has_left_right, &left, &right))
    return LWN_Make_Icon(MTYPE_I4, 1);  

  // Amount of work depends only on number of processors. 
  if (has_left_right) {
    if (left > 1 && right < MAX_PROCS) {
      WN* wn_procs1 = Current_Numprocs(_loop, is_pdo);
      WN* wn_left = LWN_Make_Icon(WN_rtype(wn_procs1), left);
      OPCODE op_ge = OPCODE_make_op(OPR_GE, Boolean_type, WN_rtype(wn_procs1));
      WN* wn_lb = LWN_CreateExp2(op_ge, wn_procs1, wn_left);
      WN* wn_procs2 = Current_Numprocs(_loop, is_pdo);
      WN* wn_right = LWN_Make_Icon(WN_rtype(wn_procs2), right);
      OPCODE op_le = OPCODE_make_op(OPR_LE, Boolean_type, WN_rtype(wn_procs2));
      WN* wn_ub = LWN_CreateExp2(op_le, wn_procs2, wn_right);
      OPCODE op_land = OPCODE_make_op(OPR_LAND, Boolean_type, MTYPE_V);
      return LWN_CreateExp2(op_land, wn_lb, wn_ub);
    } else if (left > 1) {
      WN* wn_procs1 = Current_Numprocs(_loop, is_pdo);
      WN* wn_left = LWN_Make_Icon(WN_rtype(wn_procs1), left);
      OPCODE op_ge = OPCODE_make_op(OPR_GE, Boolean_type, WN_rtype(wn_procs1));
      return LWN_CreateExp2(op_ge, wn_procs1, wn_left);
    } else if (right < MAX_PROCS) {
      WN* wn_procs2 = Current_Numprocs(_loop, is_pdo);
      WN* wn_right = LWN_Make_Icon(WN_rtype(wn_procs2), right);
      OPCODE op_le = OPCODE_make_op(OPR_LE, Boolean_type, WN_rtype(wn_procs2));
      return LWN_CreateExp2(op_le, wn_procs2, wn_right);
    }
  } 

  // create WHIRL expression for number of iterations, in wn_base
  INT nloops = SNL_Loop_Count(wn_outer); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  INT inv_nloops = Invariant_Loop_Count(wn_outer);   
  INT var_nloops = nloops - inv_nloops; 
  INT count = 0; 
  WN* wn_base = NULL; 
  if (LNO_IPA_Enabled && dli->Has_Calls) { 
    wn_base = WN_Single_Iteration_Cost(wn_outer, TRUE); 
  } else { 
    wn_base = Make_Const(Host_To_Targ_Float(MTYPE_F8, (float)
      dli->Work_Estimate)); 
  } 
#ifdef KEY // bug 7772
  INT64 wn_base_ival = 0;
  WN* wn_prod_trips = NULL;
  if (WN_operator(wn_base) == OPR_CONST)
    wn_base_ival = (INT64)TCON_dval(ST_tcon_val(WN_st(wn_base)));
#endif
  for (WN* wn = wn_inner; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      WN* wn_prod = NULL; 
      if (count++ < var_nloops
	  || !Upper_Bound_Standardize(WN_end(wn), TRUE)) { 
	DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
#ifdef KEY // bug 7772
	wn_prod = LWN_Make_Icon(MTYPE_I4, dli->Est_Num_Iterations);
#else
	wn_prod = Make_Const(Host_To_Targ_Float(MTYPE_F8, 
	  (float) dli->Est_Num_Iterations));  
#endif
      } else { 
        WN* wn_trip_count = Trip_Count(wn); 
#ifdef KEY // bug 7772
	wn_prod = wn_trip_count;
#else
	wn_prod = LWN_CreateExp1(OPCODE_make_op(OPR_CVT, MTYPE_F8, 
          Promote_Type(Do_Wtype(wn))), wn_trip_count); 
#endif
      } 
#ifdef KEY // bug 7772
      if (wn == wn_inner)
	wn_prod_trips = wn_prod;
      else wn_prod_trips = LWN_CreateExp2(OPC_I4MPY, wn_prod_trips, wn_prod);
#else
      wn_base = LWN_CreateExp2(OPC_F8MPY, wn_base, wn_prod);  
#endif
    } 
    if (wn == wn_outer) 
      break;  
  }
 
    // create WHIRL IF-test for whether to go parallel, in wn_result

      // first get P - 1, in wn_pminus1_float
  WN* wn_procs1 = Current_Numprocs(_loop, is_pdo);  
  WN* wn_one = LWN_Make_Icon(MTYPE_I4, 1);
  TYPE_ID proc_type;  // P could be from either a variable or func. call
  if (OPCODE_is_load(WN_opcode(wn_procs1))) {
    proc_type = WN_desc(wn_procs1);
  } else {
    proc_type = WN_rtype(wn_procs1);
  }
  OPCODE subop = OPCODE_make_op(OPR_SUB, proc_type, MTYPE_V);
  WN* wn_pminus1 = LWN_CreateExp2(subop, wn_procs1, wn_one); 
#ifdef KEY // bug 7772
  WN* wn_rhs = LWN_CreateExp2(OPC_I8MPY, wn_prod_trips, wn_pminus1);
      // get P * Tp in wn_PTp
  WN* wn_procs2 = Current_Numprocs(_loop, is_pdo);  
  WN* wn_Tp = LWN_Make_Icon(MTYPE_I4, (INT) Tp_Parallel_Cost());
  WN* wn_PTp = LWN_CreateExp2(OPC_I8MPY, wn_procs2, wn_Tp); 
      // get LHS expression P * (Tc + P * Tp) in wn_lhs
  WN* wn_Tc = LWN_Make_Icon(MTYPE_I8, (INT64) Tc_Parallel_Cost());
  WN* wn_lhs_sum = LWN_CreateExp2(OPC_I8ADD, wn_Tc, wn_PTp);
  WN* wn_procs3 = Current_Numprocs(_loop, is_pdo);
  WN* wn_lhs = LWN_CreateExp2(OPC_I8MPY, wn_procs3, wn_lhs_sum);
  WN* wn_result;
  if (wn_base_ival != 0) {
    if (WN_operator(wn_lhs) == OPR_INTCONST) {
      WN_const_val(wn_lhs) = WN_const_val(wn_lhs) / wn_base_ival;
      wn_result = LWN_CreateExp2(OPC_I4I8GT, wn_rhs, wn_lhs);
    }
    else wn_result = LWN_CreateExp2(OPC_I4I8GT, 
    			       LWN_CreateExp2(OPC_I8MPY, wn_rhs,
			       		LWN_Make_Icon(MTYPE_I8, wn_base_ival)),
			       wn_lhs);
  }
  else { // use F8 type
    wn_rhs = LWN_CreateExp1(OPCODE_make_op(OPR_CVT, MTYPE_F8, MTYPE_I8),wn_rhs);
    wn_lhs = LWN_CreateExp1(OPCODE_make_op(OPR_CVT, MTYPE_F8, MTYPE_I8),wn_lhs);
    wn_result = LWN_CreateExp2(OPC_I4F8GT, 
    			       LWN_CreateExp2(OPC_F8MPY, wn_rhs, wn_base),
			       wn_lhs);
  }
#else
  WN* wn_pminus1_float = LWN_CreateExp1(OPCODE_make_op(OPR_CVT, MTYPE_F8, 
    MTYPE_I4), wn_pminus1); 
      // get RHS expression W * (P - 1) in wn_rhs
  WN* wn_rhs = LWN_CreateExp2(OPC_F8MPY, wn_base, wn_pminus1_float);
      // get P * Tp in wn_PTp
  WN* wn_procs2 = Current_Numprocs(_loop, is_pdo);  
  WN* wn_p_float = LWN_CreateExp1(OPCODE_make_op(OPR_CVT, MTYPE_F8, MTYPE_I4),
    wn_procs2); 
  WN* wn_Tp = Make_Const(Host_To_Targ_Float(MTYPE_F8, Tp_Parallel_Cost()));
  WN* wn_PTp = LWN_CreateExp2(OPC_F8MPY, wn_p_float, wn_Tp); 
      // get LHS expression P * (Tc + P * Tp) in wn_lhs
  WN* wn_Tc = Make_Const(Host_To_Targ_Float(MTYPE_F8, Tc_Parallel_Cost()));
  WN* wn_lhs_sum = LWN_CreateExp2(OPC_F8ADD, wn_Tc, wn_PTp);
  WN* wn_procs3 = Current_Numprocs(_loop, is_pdo);
  WN* wn_p_float2 = LWN_CreateExp1(OPCODE_make_op(OPR_CVT, MTYPE_F8,
    MTYPE_I4), wn_procs3);
  WN* wn_lhs = LWN_CreateExp2(OPC_F8MPY, wn_p_float2, wn_lhs_sum);

  WN* wn_result = LWN_CreateExp2(OPC_I4F8GT, wn_rhs, wn_lhs);
#endif
  return wn_result; 
}


WN* ARA_LOOP_INFO::Create_IF_Clause(BOOL is_pdo)
{
  if (Get_Trace(TP_LNOPT2, TT_LNO_NO_AUTO_PARALLEL))
    return Create_Old_IF_Clause(); 
  else 
    return Create_New_IF_Clause(is_pdo); 
}

//-----------------------------------------------------------------------
// NAME: Has_No_Concurrentize_Directive 
// FUNCTION: Returns TRUE if 'wn_loop' has a NO CONCURRENTIZE directive, 
//   FALSE otherwise 
//-----------------------------------------------------------------------

extern BOOL Has_No_Concurrentize_Directive(WN* wn_loop)
{
  for (WN* wn = WN_prev(wn_loop); wn != NULL; wn = WN_prev(wn)) {
    if (WN_opcode(wn) == OPC_PRAGMA
        && WN_pragma(wn) == WN_PRAGMA_KAP_NOCONCURRENTIZE)
      return TRUE;
    if (WN_opcode(wn) == OPC_DO_LOOP)
      break;
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Inside_Lego_Tiled_Loop 
// FUNCTION: Returns TRUE if 'wn_loop' is inside an outer lego tile loop,
//   or is an outer lego tile loop itself, FALSE otherwise 
//-----------------------------------------------------------------------

extern BOOL Inside_Lego_Tiled_Loop(WN* wn_loop)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  for (WN* wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
      if (dli->Is_Outer_Lego_Tile || dli->Is_Inner_Lego_Tile)
        return TRUE; 
    }
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Print_Non_Parallel_Loop 
// FUNCTION: Prints a message indicating that we decided not to parallelize
//   this loop after all, because its trip count is too small. 
//-----------------------------------------------------------------------

static void Print_Non_Parallel_Loop(FILE* fp, 
				    WN* wn_loop)
{
  fprintf(fp, "NOT Auto Parallelizing Loop %s at %d (SMALL TRIP COUNT)\n",
    WB_Whirl_Symbol(wn_loop), Srcpos_To_Line(WN_linenum(wn_loop)));
}

#ifdef KEY
static void Replace_Preg_With_Symbol (WN* node, WN* replace, WN* def,
				      WN_OFFSET preg_num, ST* preg_st)
{
  if (WN_operator(node) == OPR_BLOCK) {
    for (WN* stmt = WN_first(node); stmt; stmt = WN_next(stmt))
      Replace_Preg_With_Symbol(stmt, replace, def, preg_num, preg_st);
  } 
  else if (WN_operator(node) == OPR_LDID) {
    if (ST_class(WN_st(node)) == CLASS_PREG &&
	WN_st(node) == preg_st &&
	WN_offset(node) == preg_num) {
      WN* parent = LWN_Get_Parent(node);
      INT kid = 0;
      WN *ldid = LWN_Copy_Tree(replace);
      for (; kid < WN_kid_count(parent); kid ++)
	if (WN_kid(parent, kid) == node) break;
      WN_kid(parent, kid) = ldid;
      Du_Mgr->Add_Def_Use(def, ldid);

      // Delete the def->use for old node and then delete that node itself.
      DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(node);
      DEF_LIST_ITER iter(def_list);
      const DU_NODE *du_node = iter.First();
      const DU_NODE *next;
      Is_True(!iter.Is_Empty(),("Empty def list in Delete_Def_Use"));
      for(next = iter.Next(); du_node; du_node=next, next=iter.Next()){
	WN *def_node = (WN *) du_node->Wn();
	// Add def_use for new node. 
	// TODO: Do once. This is repeated many times unnecessarily.
	Du_Mgr->Add_Def_Use(def_node, WN_kid0(def));
	Du_Mgr->Delete_Def_Use(def_node,node);
      }
      LWN_Delete_Tree(node);
    }
  } 
  // Recurse.
  else
    for (INT kid = 0; kid < WN_kid_count(node); kid ++)
      Replace_Preg_With_Symbol(WN_kid(node, kid), replace, def, 
			       preg_num, preg_st);
}
#endif
// ============================================================================
//
// Generate parallel pragma for outer-most parallel loops
//
// (1) DOACROSS
// (2) LOCAL 
// (3) LASTLOCAL
// (4) SHARED
//
// ============================================================================
void
ARA_LOOP_INFO::Generate_Parallel_Pragma()
{
#ifdef KEY
  Last_Apo_Loop_Id++;
  if (Last_Apo_Loop_Id > LNO_Apo_Loop_Skip_After ||
      Last_Apo_Loop_Id < LNO_Apo_Loop_Skip_Before ||
      Last_Apo_Loop_Id == LNO_Apo_Loop_Skip_Equal)
    return;
#endif

  // Set parallel debug level
  INT parallel_debug_level = Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG)
    ? Parallel_Debug_Level : 0;

  // Loop is marked as parallel by user
  if (_info->Mp_Info) 
    return;

  // Adjust Suggested_Parallel for convexity considerations. 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(_loop); 
#ifdef KEY //bug 14284 : don't parallelize if contains calls to nested functions
  if(!dli || dli->Has_Nested_Calls)
    return;
#endif
  if (dli->Suggested_Parallel && _peel_value == -1) {
    dli->Suggested_Parallel = FALSE; 
    if (Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG))
      fprintf(stdout, 
        "Convex Problem: Not parallelizing %s\n", WB_Whirl_Symbol(_loop));
  } 

  if (Is_Parallel()) {

    if (!Get_Trace(TP_LNOPT2, TT_LNO_NO_AUTO_PARALLEL)
	&& !_info->Suggested_Parallel)
      DevWarn("Auto-parallelizing unexpected loop %s at %d", 
	WB_Whirl_Symbol(_loop), Srcpos_To_Line(WN_linenum(_loop))); 

    _info->Auto_Parallelized = TRUE; 
    if (Has_Last_Value_Array()) {
      Generate_Copyout_Loop();
      return;
    }
    WN *if_cond = NULL;

    if (LNO_Run_AP==1 && !_info->Pragma_Prefer_Concurrentize) {
      if_cond = Create_IF_Clause(FALSE);
      if (if_cond && WN_operator(if_cond) == OPR_INTCONST) {
	if (WN_const_val(if_cond) == 0) {
	  LWN_Delete_Tree(if_cond);
	  DO_LOOP_INFO* dli = Get_Do_Loop_Info(_loop); 
	  dli->Suggested_Parallel = FALSE; 
	  dli->Not_Enough_Parallel_Work = TRUE; 
          dli->Auto_Parallelized = FALSE; 
	  if (LNO_Verbose || parallel_debug_level >= 1) {
	    Print_Non_Parallel_Loop(stdout, _loop); 
	    Print_Non_Parallel_Loop(TFile, _loop); 
          } 
	  return;
	} else {
	  LWN_Delete_Tree(if_cond);
	  if_cond = NULL;
	}
      }
    }

    if (_peel_value > 0) {
      Last_Value_Peeling_On(); 
      Post_loop_peeling(_loop, _peel_value, TRUE);  
      Last_Value_Peeling_Off(); 
    } 

    WN * interleave = NULL;
    if (Variable_Load()) {
      interleave = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                                   ST_IDX_ZERO,
                                   (INT32) WN_PRAGMA_SCHEDTYPE_INTERLEAVE,
                                   0);
      WN_set_pragma_compiler_generated(interleave);
    }

    WN * parent_block = LWN_Get_Parent(_loop);
    WN * wn_after_loop = WN_next(_loop);
    RID *p_rid = Get_Enclosing_Region_ID(_loop);
    FmtAssert(p_rid, ("ARA_LOOP_INFO::Generate_Parallel_Pragma: can't find parent RID"));
    
    WN * do_loop = LWN_Extract_From_Block(_loop);
    
    // create a new parallel region around the parallel loop
    WN * region = WN_CreateRegion(REGION_KIND_MP,do_loop,NULL,NULL,
				  RID_CREATE_NEW_ID,(INITO_IDX) NULL);
    REGION_INFO* rgi = CXX_NEW(REGION_INFO(TRUE), &LNO_default_pool);
    WN_MAP_Set(LNO_Info_Map, region, (void *) rgi);
    
    // Create the pragmas
    WN *prag = WN_CreatePragma(WN_PRAGMA_PARALLEL_DO, ST_IDX_ZERO, 0, 1);
    WN_set_pragma_compiler_generated(prag); 

    WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
    LWN_Insert_Block_Before(WN_region_pragmas(region),NULL,prag);

    if (if_cond != NULL) {
      prag = WN_CreateXpragma(WN_PRAGMA_IF, ST_IDX_ZERO, 1);
      WN_kid0(prag) = if_cond;
      LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
    }
    
    if (interleave != NULL) {
      LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, interleave);
    }

    INT i;
    for (i = 0; i < _pri.Elements(); ++i) {
      if (_pri.Bottom_nth(i)->Is_Loop_Invariant() && 
	  !_pri.Bottom_nth(i)->Is_Unknown_Size()) {
	if (_pri.Bottom_nth(i)->Need_Last_Value()) {
	  prag = WN_CreatePragma(WN_PRAGMA_LASTLOCAL, 
	    _pri.Bottom_nth(i)->Array().St(), 
	    _pri.Bottom_nth(i)->Array().WN_Offset(), 0);
	  WN_set_pragma_compiler_generated(prag);
	} else { 
	  prag = WN_CreatePragma(WN_PRAGMA_LOCAL, 
	    _pri.Bottom_nth(i)->Array().St(), 
	    _pri.Bottom_nth(i)->Array().WN_Offset(), 0);
	  WN_set_pragma_compiler_generated(prag);
	} 
      } else {
	prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
	  _pri.Bottom_nth(i)->Array().St(), 
	  _pri.Bottom_nth(i)->Array().WN_Offset(), 0);
        WN_set_pragma_compiler_generated(prag);
      }
      WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
      LWN_Insert_Block_Before(WN_region_pragmas(region),NULL,prag);
    }
    
    for (i = 0; i < _scalar_pri.Elements(); ++i) {
      if (_scalar_last_value.Bottom_nth(i)) {
	prag = WN_CreatePragma(WN_PRAGMA_LASTLOCAL, 
	  _scalar_pri.Bottom_nth(i)->_scalar.St(), 
	  _scalar_pri.Bottom_nth(i)->_scalar.WN_Offset(), 0);
        WN_set_pragma_compiler_generated(prag);
      }
      else {
	prag = WN_CreatePragma(WN_PRAGMA_LOCAL, 
	  _scalar_pri.Bottom_nth(i)->_scalar.St(), 
	  _scalar_pri.Bottom_nth(i)->_scalar.WN_Offset(), 0);
        WN_set_pragma_compiler_generated(prag);
      }

      WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
      LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
    }

    for (i = 0; i < _use.Elements(); ++i) {
      if (Overlap_Local_Array(_use.Bottom_nth(i)->Array(), _use.Bottom_nth(i)->Offset()))
        FmtAssert(FALSE,("ARA_LOOP_INFO::Generate_Parallel_Pragma, exposed use overlaps with local array, need renaming"));
      prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
	_use.Bottom_nth(i)->Array().St(), 
	_use.Bottom_nth(i)->Array().WN_Offset(), 0);
      WN_set_pragma_compiler_generated(prag);
      WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
      LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
    }

    for (i = 0; i < _scalar_use.Elements(); ++ i) {
      if (Overlap_Local_Scalar(_scalar_use.Bottom_nth(i)->_scalar))
	FmtAssert(FALSE,("ARA_LOOP_INFO::Generate_Parallel_Pragma, exposed scalar use overlaps with local scalar, something is wrong"));
      if (!Overlap_Reduction_Scalar(_scalar_use.Bottom_nth(i)->_scalar)) {
#ifdef KEY
	// bug 11914: pregs with negative offset are used for ASM outputs
	if (ST_class(_scalar_use.Bottom_nth(i)->_scalar.St()) == CLASS_PREG &&
	    _scalar_use.Bottom_nth(i)->_scalar.WN_Offset() < 0)
	  continue;
	// Bug 6386 
	// need to use a temporary instead of a preg for shared variables.
	if (ST_class(_scalar_use.Bottom_nth(i)->_scalar.St()) == CLASS_PREG) {
	  TYPE_ID rtype = _scalar_use.Bottom_nth(i)->_scalar.Type;
	  ST * comma_st = Gen_Temp_Symbol (MTYPE_TO_TY_array[rtype], "_ubtmp");
	  OPCODE ldid_opc = OPCODE_make_op(OPR_LDID,Promote_Type(rtype),rtype);
	  WN_OFFSET preg_num = _scalar_use.Bottom_nth(i)->_scalar.WN_Offset();
	  ST * preg_st = _scalar_use.Bottom_nth(i)->_scalar.St();
	  WN * ldid = WN_CreateLdid(ldid_opc, preg_num, preg_st, 
				    Be_Type_Tbl(rtype));	
	  WN * stid = WN_Stid (rtype, 0, comma_st, WN_ty (ldid), ldid);
	  WN_Set_Linenum (stid, WN_Get_Linenum(_loop));
	  LWN_Parentize(stid);
	  LWN_Insert_Block_Before (parent_block, wn_after_loop, stid);
	  LWN_Set_Parent(stid, parent_block);
	  WN *replace = WN_Ldid (rtype, 0, comma_st, WN_ty (ldid));
	  Replace_Preg_With_Symbol(_loop, replace, stid, 
				   preg_num, preg_st);
	  prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
				 WN_st(replace), 
				 WN_offset(replace), 0);
	  WN_set_pragma_compiler_generated(prag);
	  WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
	  LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
	  // Update access vectors
	  DOLOOP_STACK dostack(&ARA_memory_pool);
	  Build_Doloop_Stack(parent_block, &dostack);
	  LNO_Build_Access(_loop, &dostack, &LNO_default_pool);
	  continue;
	}
#endif
	prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
			       _scalar_use.Bottom_nth(i)->_scalar.St(), 
			       _scalar_use.Bottom_nth(i)->_scalar.WN_Offset(), 0);
	WN_set_pragma_compiler_generated(prag);
	WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
	LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
      }
    }

    for (i = 0; i < _def.Elements(); ++ i) {
      ARA_REF *cur = _def.Bottom_nth(i);
      if (!Overlap_Local_Array(cur->Array(), cur->Offset()) && 
	  !Overlap_Exposed_Array(cur->Array(), cur->Offset())) {
	if (cur->Is_Loop_Invariant() && !cur->Is_Unknown_Size()) {
	  prag = WN_CreatePragma(WN_PRAGMA_LASTLOCAL, 
	    cur->Array().St(), cur->Array().WN_Offset(), 0);
	} else { 
	  prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
	    cur->Array().St(), cur->Array().WN_Offset(), 0);
	} 
 	WN_set_pragma_compiler_generated(prag);
	WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
	LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
      }
    }
	
    for (i = 0; i < _scalar_def.Elements(); ++ i) {
      if (!Overlap_Local_Scalar(_scalar_def.Bottom_nth(i)->_scalar) && 
	  !Overlap_Exposed_Scalar(_scalar_def.Bottom_nth(i)->_scalar)) {
	prag = WN_CreatePragma(WN_PRAGMA_LASTLOCAL, 
          _scalar_def.Bottom_nth(i)->_scalar.St(), 
          _scalar_def.Bottom_nth(i)->_scalar.WN_Offset(), 0);
        WN_set_pragma_compiler_generated(prag);
	WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
	LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
      }
    }

    for (i = 0; i < _scalar_may_def.Elements(); ++ i) {
      if (!Overlap_Local_Scalar(_scalar_may_def.Bottom_nth(i)->_scalar) &&
	  !Overlap_Exposed_Scalar(_scalar_may_def.Bottom_nth(i)->_scalar) &&
	  !Overlap_Kill_Scalar(_scalar_may_def.Bottom_nth(i)->_scalar) &&
	  !Overlap_Reduction_Scalar(_scalar_may_def.Bottom_nth(i)->_scalar)) {
	prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
	  _scalar_may_def.Bottom_nth(i)->_scalar.St(), 
          _scalar_may_def.Bottom_nth(i)->_scalar.WN_Offset(), 0);
	WN_set_pragma_compiler_generated(prag);
	WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
	LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
      }
    }

    for (i = 0; i < _reduction.Elements(); ++i) {
      WN* cur_wn = _reduction.Bottom_nth(i);
      if (WN_operator(cur_wn) == OPR_ISTORE) {
	WN* wn_array = WN_kid1(cur_wn);
        prag = WN_CreateXpragma(WN_PRAGMA_REDUCTION, ST_IDX_ZERO, 1);
	WN_kid0(prag) = LWN_Copy_Tree(wn_array);
	LWN_Copy_Def_Use(wn_array, WN_kid0(prag), Du_Mgr);
        WN_set_pragma_compiler_generated(prag);
#ifndef KEY // bug 9112 : WN_prefetch_flag overlaps with WN_st_idx, so is bad
        WN_prefetch_flag(prag) = WN_offset(cur_wn);
#else
	if (WN_offset(cur_wn)) {
	  TYPE_ID rtype = WN_rtype(WN_kid0(prag));
	  WN* wn_ofst = LWN_Make_Icon(rtype, WN_offset(cur_wn));
	  OPCODE addop = OPCODE_make_op(OPR_ADD, rtype, MTYPE_V);
	  WN_kid0(prag) = LWN_CreateExp2(addop, WN_kid0(prag), wn_ofst);
	}
#endif
        Set_Reduction_Array_Base_is_shared_auto(wn_array);
      } else {  
	prag = WN_CreatePragma(WN_PRAGMA_REDUCTION, WN_st(cur_wn), 
          WN_offset(cur_wn),0);
        WN_set_pragma_compiler_generated(prag);
        if (WN_sclass(cur_wn) == SCLASS_AUTO)
          Set_ST_is_shared_auto(*WN_st(cur_wn));
      } 
      WN_pragma_arg2(prag) = REDUCTION_TYPE_to_OPERATOR(
	red_manager->Which_Reduction(cur_wn));
      WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
      LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
    }
    
    if (dli->Is_Doacross) {
      prag = WN_CreatePragma(WN_PRAGMA_SYNC_DOACROSS, ST_IDX_ZERO, 0, 0);
      WN_set_pragma_compiler_generated(prag);
      WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
      LWN_Insert_Block_After(WN_region_pragmas(region),
			   WN_first(WN_region_pragmas(region)), prag);
    }

    // create a RID for the region and add to map
    RID *rid = RID_Create(WN_region_id(region),RID_depth(p_rid)+1,region);

    RID_level(rid) = RL_LNO;
    RID_TYPE_mp_Set(rid);
    WN_MAP_Set(RID_map,region,(void *)rid);
    
    RID_Add_kid(rid, p_rid);

    // Insert the region back
    LWN_Parentize(region);    
    LWN_Insert_Block_Before(parent_block, wn_after_loop, region);

    if (dli->Is_Doacross) {
      WN* wn_comment=
        WN_CreateComment(
     "A doacross loop with synchronization is generated for the following loop"
        );
      LWN_Insert_Block_Before(parent_block, region, wn_comment);
    }

    DOLOOP_STACK copy_stack(&ARA_memory_pool);
    Build_Doloop_Stack(WN_region_pragmas(region), &copy_stack);
    LNO_Build_Access(WN_region_pragmas(region), &copy_stack, 
	  &LNO_default_pool);

    // Set the fields in DO_LOOP_INFO
    Contains_MP = TRUE;
    DO_LOOP_INFO * dli = _info;
    MP_INFO * mp_info = CXX_NEW(MP_INFO(MP_SCHED_SIMPLE, FALSE),
	&LNO_default_pool);
    dli->Mp_Info = mp_info;

    if (!PU_has_mp(Get_Current_PU()))
      Mp_File_Init();     
#ifdef _NEW_SYMTAB
    Set_PU_has_mp(Get_Current_PU());
    Set_PU_has_region(Get_Current_PU());
    Set_PU_uplevel(Get_Current_PU());
#else
    Set_SYMTAB_has_mp(Current_Symtab);
    Set_SYMTAB_has_rgn(Current_Symtab);
    Set_SYMTAB_uplevel(Current_Symtab);
#endif

    Set_Enclosing_If_Has_Region(region);
    if (Index_Variable_Live_At_Exit(_loop)) { 
      WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(_loop)); 
      WN* wn_first = WN_first(WN_region_pragmas(wn_region)); 
      SYMBOL sym_index(WN_index(_loop)); 
      for (WN* wn = wn_first; wn != NULL; wn = WN_next(wn)) { 
	if (WN_pragma(wn) == WN_PRAGMA_LOCAL && sym_index.St()
	    == WN_st(wn) && sym_index.WN_Offset() == WN_pragma_arg1(wn)) {
	  WN_pragma(wn) = WN_PRAGMA_LASTLOCAL; 
          WN_set_pragma_compiler_generated(wn);
        }
      } 
    }

  } else {
    if (Is_OK_Parallel())
      DevWarn("Heuristic prefers INNER parallel loop to loop %s at %d", 
        WB_Whirl_Symbol(_loop), Srcpos_To_Line(WN_linenum(_loop)));
    for (INT i = 0; i < _children.Elements(); ++i) 
      _children.Bottom_nth(i)->Generate_Parallel_Pragma();
  }
}

// ============================================================================
//
// Peel the last iteration and generate the parallel directives
//
// (1) PARALLEL REGION
// (2) PDO
// (3) SINGLE PROCESS
// (4) SHARED, LOCAL, LASTLOCAL, REDUCTION
// 
// The result is like the following
//
//     PARALLEL shared(all the variables)
//        PDO local(...), lastlocal(...), reduction(...)
//            first n-1 iterations of the loop
//        END PDO nowait
//        SINGLE_PROCESS
//            last iteration of the loop
//        END SINGLE_PROCESS nowait
//     END PARALLEL
//
// ============================================================================
void
ARA_LOOP_INFO::Generate_Copyout_Loop()
{
  // Set parallel debug level
  INT parallel_debug_level = Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG)
    ? Parallel_Debug_Level : 0;

  WN *if_cond = NULL;
  if (LNO_Run_AP==1) {
    if_cond = Create_IF_Clause(TRUE);
    if (if_cond && WN_operator(if_cond) == OPR_INTCONST) {
      if (WN_const_val(if_cond) == 0) {
	LWN_Delete_Tree(if_cond);
	DO_LOOP_INFO* dli = Get_Do_Loop_Info(_loop);
        dli->Suggested_Parallel = FALSE;
        dli->Not_Enough_Parallel_Work = TRUE;
        dli->Auto_Parallelized = FALSE; 
        if (LNO_Verbose || parallel_debug_level >= 1) {
          Print_Non_Parallel_Loop(stdout, _loop);
          Print_Non_Parallel_Loop(TFile, _loop);
        }
	return;
      } else {
	LWN_Delete_Tree(if_cond);
	if_cond = NULL;
      }
    }
  }

  if (_peel_value > 0) {
    Last_Value_Peeling_On(); 
    Post_loop_peeling(_loop, _peel_value, TRUE); 
    Last_Value_Peeling_Off(); 
  } 

  WN * interleave = NULL;
  if (Variable_Load()) {
    interleave = WN_CreatePragma(WN_PRAGMA_MPSCHEDTYPE,
                                 ST_IDX_ZERO,
                                 (INT32) WN_PRAGMA_SCHEDTYPE_INTERLEAVE,
                                 0);
    WN_set_pragma_compiler_generated(interleave);
  }

  WN * parent_block = LWN_Get_Parent(_loop);
  WN * wn_after_loop = WN_next(_loop);
  RID *p_rid = Get_Enclosing_Region_ID(_loop);
  FmtAssert(p_rid, ("ARA_LOOP_INFO::Generate_Parallel_Pragma: can't find parent RID"));
    
  WN * do_loop = LWN_Extract_From_Block(_loop);
    
  // create a new parallel region around the parallel loop
  WN * region = WN_CreateRegion(REGION_KIND_MP,do_loop,NULL,NULL,
				RID_CREATE_NEW_ID,
                                (INITO_IDX) NULL);
  REGION_INFO* rgi = CXX_NEW(REGION_INFO(TRUE), &LNO_default_pool); 
  WN_MAP_Set(LNO_Info_Map, region, (void *) rgi); 
    
  // Create parallel region pragmas
  WN *prag = WN_CreatePragma(WN_PRAGMA_PARALLEL_BEGIN, ST_IDX_ZERO, 0, 0);
  WN_set_pragma_compiler_generated(prag);
  WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
  LWN_Insert_Block_Before(WN_region_pragmas(region),NULL,prag);
    
  if (if_cond != NULL) {
    prag = WN_CreateXpragma(WN_PRAGMA_IF, ST_IDX_ZERO, 1);
    WN_kid0(prag) = if_cond;
    LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
  }

  // If an array needs last value assignment, put it in the SHARED
  // clause, otherwise, put it in the local clause for better locality.
  INT i;
  for (i = 0; i < _pri.Elements(); ++i) {
    if (_pri.Bottom_nth(i)->Is_Loop_Invariant() && 
	!_pri.Bottom_nth(i)->Is_Unknown_Size()) {
      if (_pri.Bottom_nth(i)->Need_Last_Value()) {
	prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
          _pri.Bottom_nth(i)->Array().St(), 
	  _pri.Bottom_nth(i)->Array().WN_Offset(), 0);
      } else { 
	prag = WN_CreatePragma(WN_PRAGMA_LOCAL, 
	  _pri.Bottom_nth(i)->Array().St(), 
	  _pri.Bottom_nth(i)->Array().WN_Offset(), 0);
      } 
      WN_set_pragma_compiler_generated(prag);
      WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
      LWN_Insert_Block_Before(WN_region_pragmas(region),NULL,prag);
    }
  }
    
  for (i = 0; i < _scalar_pri.Elements(); ++i) {
    if (_scalar_last_value.Bottom_nth(i)) {
      prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
	_scalar_pri.Bottom_nth(i)->_scalar.St(), 
	_scalar_pri.Bottom_nth(i)->_scalar.WN_Offset(), 0);
    } else{
      prag = WN_CreatePragma(WN_PRAGMA_LOCAL, 
	_scalar_pri.Bottom_nth(i)->_scalar.St(), 
	_scalar_pri.Bottom_nth(i)->_scalar.WN_Offset(), 0);
    }
    WN_set_pragma_compiler_generated(prag);
    WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
    LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
  }

  for (i = 0; i < _use.Elements(); ++i) {
    if (Overlap_Local_Array(_use.Bottom_nth(i)->Array(),
			    _use.Bottom_nth(i)->Offset())) 
      FmtAssert(FALSE,("ARA_LOOP_INFO::Generate_Parallel_Pragma, exposed use overlaps with local array, need renaming"));
    prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
	_use.Bottom_nth(i)->Array().St(), 
	_use.Bottom_nth(i)->Array().WN_Offset(), 0);
    WN_set_pragma_compiler_generated(prag);
    WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
    LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
  }

  for (i = 0; i < _scalar_use.Elements(); ++ i) {
    if (Overlap_Local_Scalar(_scalar_use.Bottom_nth(i)->_scalar))
      FmtAssert(FALSE,("ARA_LOOP_INFO::Generate_Parallel_Pragma, exposed scalar use overlaps with local scalar, something is wrong"));
    if (!Overlap_Reduction_Scalar(_scalar_use.Bottom_nth(i)->_scalar)) {
      prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
	_scalar_use.Bottom_nth(i)->_scalar.St(), 
	_scalar_use.Bottom_nth(i)->_scalar.WN_Offset(), 0);
      WN_set_pragma_compiler_generated(prag);
      WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
      LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
    }
  }

  for (i = 0; i < _def.Elements(); ++ i) {
    prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
	_def.Bottom_nth(i)->Array().St(), 
	_def.Bottom_nth(i)->Array().WN_Offset(), 0);
    WN_set_pragma_compiler_generated(prag);
    WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
    LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
  }
	
  for (i = 0; i < _scalar_def.Elements(); ++ i) {
    if (!Overlap_Local_Scalar(_scalar_def.Bottom_nth(i)->_scalar) && 
	!Overlap_Exposed_Scalar(_scalar_def.Bottom_nth(i)->_scalar) &&
	!Overlap_Reduction_Scalar(_scalar_def.Bottom_nth(i)->_scalar)) {
      prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
	_scalar_def.Bottom_nth(i)->_scalar.St(), 
	_scalar_def.Bottom_nth(i)->_scalar.WN_Offset(), 0);
      WN_set_pragma_compiler_generated(prag);
      WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
      LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
    }
  }

  for (i = 0; i < _scalar_may_def.Elements(); ++ i) {
    if (!Overlap_Local_Scalar(_scalar_may_def.Bottom_nth(i)->_scalar) &&
	!Overlap_Exposed_Scalar(_scalar_may_def.Bottom_nth(i)->_scalar) &&
	!Overlap_Kill_Scalar(_scalar_may_def.Bottom_nth(i)->_scalar) &&
	!Overlap_Reduction_Scalar(_scalar_may_def.Bottom_nth(i)->_scalar)) {
      prag = WN_CreatePragma(WN_PRAGMA_SHARED, 
        _scalar_may_def.Bottom_nth(i)->_scalar.St(), 
        _scalar_may_def.Bottom_nth(i)->_scalar.WN_Offset(), 0);
      WN_set_pragma_compiler_generated(prag); 
      WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
      LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
    }
  }

    // PV 656616 : all PDO reduction variables must be SHARED on
    // the PARALLEL region (else there's a reprivatization error
    // if the reduction variable is a PREG)
  for (i = 0; i < _reduction.Elements(); ++i) {
    WN* cur_wn = _reduction.Bottom_nth(i); 
    if (WN_operator(cur_wn) == OPR_ISTORE) {
      WN* wn_array = WN_kid1(cur_wn);
      prag = WN_CreatePragma(WN_PRAGMA_SHARED,
              Find_Reduction_Array_Base(wn_array), WN_offset(cur_wn), 0);
    } else {  
      prag = WN_CreatePragma(WN_PRAGMA_SHARED, WN_st(cur_wn), 
	WN_offset(cur_wn),0);
    } 
    WN_set_pragma_compiler_generated(prag);
    WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
    LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
  }

  if (Get_Do_Loop_Info(_loop)->Is_Doacross) {
    prag = WN_CreatePragma(WN_PRAGMA_SYNC_DOACROSS, ST_IDX_ZERO, 0, 0);
    WN_set_pragma_compiler_generated(prag); 
    WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
    LWN_Insert_Block_After(WN_region_pragmas(region),
	   WN_first(WN_region_pragmas(region)), prag);
  }
    
  // create a RID for the region and add to map
  RID *rid = RID_Create(WN_region_id(region),RID_depth(p_rid)+1,region);

  RID_level(rid) = RL_LNO;
  RID_TYPE_mp_Set(rid);
  WN_MAP_Set(RID_map,region,(void *)rid);
  
  RID_Add_kid(rid, p_rid);
  
  // Insert the region back
  LWN_Parentize(region);    
  LWN_Insert_Block_Before(parent_block, wn_after_loop, region);

  if (Get_Do_Loop_Info(_loop)->Is_Doacross) {
    WN* wn_comment= WN_CreateComment(
   "A doacross loop with synchronization is generated for the following loop");
    LWN_Insert_Block_Before(parent_block, region, wn_comment);
  }

  // Now, peel the last iteration of the loop
  Last_Value_Peeling_On(); 
  Post_loop_peeling(_loop, 1);
  Last_Value_Peeling_Off(); 

  // Put a PDO region around the main loop together with
  // LOCAL info
  parent_block = LWN_Get_Parent(_loop);
  wn_after_loop = WN_next(_loop);
  p_rid = Get_Enclosing_Region_ID(_loop);
  FmtAssert(p_rid, ("ARA_LOOP_INFO::Generate_Parallel_Pragma: can't find parent RID"));
    
  do_loop = LWN_Extract_From_Block(_loop);
    
  // create a new parallel region around the parallel loop
  region = WN_CreateRegion(REGION_KIND_MP,do_loop,NULL,NULL,
			   RID_CREATE_NEW_ID,
                           (INITO_IDX) NULL);
  rgi = CXX_NEW(REGION_INFO(TRUE), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, region, (void *) rgi);
    
  // Create the pragmas
  prag = WN_CreatePragma(WN_PRAGMA_PDO_BEGIN, ST_IDX_ZERO, 0, 0);
  WN_set_pragma_compiler_generated(prag); 
  WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
  LWN_Insert_Block_Before(WN_region_pragmas(region),NULL,prag);

  if (interleave != NULL) {
    LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, interleave);
  }
    
  for (i = 0; i < _pri.Elements(); ++i) {
    if (_pri.Bottom_nth(i)->Is_Loop_Invariant() && 
	!_pri.Bottom_nth(i)->Is_Unknown_Size()) {
      prag = WN_CreatePragma(WN_PRAGMA_LOCAL, 
	_pri.Bottom_nth(i)->Array().St(), 
        _pri.Bottom_nth(i)->Array().WN_Offset(), 0);
      WN_set_pragma_compiler_generated(prag);
      WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
      LWN_Insert_Block_Before(WN_region_pragmas(region),NULL,prag);
    }
  }
    
  for (i = 0; i < _def.Elements(); ++ i) {
    ARA_REF *cur = _def.Bottom_nth(i);
    if (!Overlap_Local_Array(cur->Array(), cur->Offset()) && 
	!Overlap_Exposed_Array(cur->Array(),cur->Offset()) &&
	cur->Is_Loop_Invariant() && !cur->Is_Unknown_Size()) {
      prag = WN_CreatePragma(WN_PRAGMA_LOCAL, 
	cur->Array().St(), cur->Array().WN_Offset(), 0);
      WN_set_pragma_compiler_generated(prag);
      WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
      LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
    }
  }

  for (i = 0; i < _scalar_pri.Elements(); ++i) {
    if (_scalar_last_value.Bottom_nth(i)) {
      prag = WN_CreatePragma(WN_PRAGMA_LOCAL, 
        _scalar_pri.Bottom_nth(i)->_scalar.St(), 
        _scalar_pri.Bottom_nth(i)->_scalar.WN_Offset(), 0);
      WN_set_pragma_compiler_generated(prag);
      WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
      LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
    }
  }

  for (i = 0; i < _reduction.Elements(); ++i) {
    WN* cur_wn = _reduction.Bottom_nth(i); 
    if (WN_operator(cur_wn) == OPR_ISTORE) {
      WN* wn_array = WN_kid1(cur_wn);
      prag = WN_CreateXpragma(WN_PRAGMA_REDUCTION,  ST_IDX_ZERO, 1);
      WN_kid0(prag) = LWN_Copy_Tree(wn_array);
      LWN_Copy_Def_Use(wn_array, WN_kid0(prag), Du_Mgr);
#ifndef KEY // bug 9112 : WN_prefetch_flag overlaps with WN_st_idx, so is bad
      WN_prefetch_flag(prag) = WN_offset(cur_wn);
#else
      if (WN_offset(cur_wn)) {
	TYPE_ID rtype = WN_rtype(WN_kid0(prag));
	WN* wn_ofst = LWN_Make_Icon(rtype, WN_offset(cur_wn));
	OPCODE addop = OPCODE_make_op(OPR_ADD, rtype, MTYPE_V);
	WN_kid0(prag) = LWN_CreateExp2(addop, WN_kid0(prag), wn_ofst);
      }
#endif
      Set_Reduction_Array_Base_is_shared_auto(wn_array);
    } else {  
      prag = WN_CreatePragma(WN_PRAGMA_REDUCTION, WN_st(cur_wn), 
	WN_offset(cur_wn),0);
      if (WN_sclass(cur_wn) == SCLASS_AUTO)
        Set_ST_is_shared_auto(*WN_st(cur_wn));
    } 
    WN_set_pragma_compiler_generated(prag);
    WN_pragma_arg2(prag) = REDUCTION_TYPE_to_OPERATOR(
      red_manager->Which_Reduction(cur_wn));
    WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
    LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
  }

  // Insert an END_PDO nowait to the pragma list
  if (!_reduction.Elements() && !Index_Variable_Live_At_Exit(_loop) &&
      !Get_Do_Loop_Info(_loop)->Is_Doacross) {
    prag = WN_CreatePragma(WN_PRAGMA_NOWAIT, ST_IDX_ZERO, 0,0);
    WN_set_pragma_compiler_generated(prag);
    WN_Set_Linenum(prag, WN_Get_Linenum(_loop));
    LWN_Insert_Block_Before(WN_region_pragmas(region), NULL, prag);
  } 
    
  // create a RID for the region and add to map
  rid = RID_Create(WN_region_id(region),RID_depth(p_rid)+1,region);

  RID_level(rid) = RL_LNO;
  RID_TYPE_mp_Set(rid);
  WN_MAP_Set(RID_map,region,(void *)rid);
    
  RID_Add_kid(rid, p_rid);

  // Insert the region back
  LWN_Parentize(region);    
  LWN_Insert_Block_Before(parent_block, wn_after_loop, region);

  DOLOOP_STACK copy_stack(&ARA_memory_pool);
  Build_Doloop_Stack(WN_region_pragmas(region), &copy_stack);
  LNO_Build_Access(WN_region_pragmas(region), &copy_stack, 
	&LNO_default_pool);
  
  // Set the fields in DO_LOOP_INFO
  Contains_MP = TRUE;
  DO_LOOP_INFO * dli = _info;
  MP_INFO * mp_info = CXX_NEW(MP_INFO(MP_SCHED_SIMPLE, TRUE),
    &LNO_default_pool);
  dli->Mp_Info = mp_info;
  dli->No_Fission = TRUE;
  dli->No_Fusion = TRUE;
  dli->Cannot_Interchange = TRUE;
  dli->Cannot_Block = TRUE;
  dli->Required_Unroll = 1;
    
  if (!PU_has_mp(Get_Current_PU()))
    Mp_File_Init();     
#ifdef _NEW_SYMTAB
    Set_PU_has_mp(Get_Current_PU());
    Set_PU_has_region(Get_Current_PU());
    Set_PU_uplevel(Get_Current_PU());
#else
    Set_SYMTAB_has_mp(Current_Symtab);
    Set_SYMTAB_has_rgn(Current_Symtab);
    Set_SYMTAB_uplevel(Current_Symtab);
#endif
  Create_Single_Region(parent_block, wn_after_loop, NULL); 
  Set_Enclosing_If_Has_Region(region);
  if (Index_Variable_Live_At_Exit(_loop)) { 
    WN* wn_inner_region = LWN_Get_Parent(LWN_Get_Parent(_loop)); 
    WN* wn_outer_region = LWN_Get_Parent(LWN_Get_Parent(wn_inner_region)); 
    WN* wn_first = WN_first(WN_region_pragmas(wn_outer_region)); 
    SYMBOL sym_index(WN_index(_loop)); 
    for (WN* wn = wn_first; wn != NULL; wn = WN_next(wn)) { 
      if (WN_pragma(wn) == WN_PRAGMA_LOCAL && sym_index.St()
	  == WN_st(wn) && sym_index.WN_Offset() == WN_pragma_arg1(wn)) {
        WN_pragma(wn) = WN_PRAGMA_SHARED; 
	Add_Pragma_To_MP_Region(wn_inner_region, WN_st(wn), 
	  WN_pragma_arg1(wn), WN_PRAGMA_LASTLOCAL, TRUE); 
      } 
    } 
  }
}  

BOOL
ARA_LOOP_INFO::Is_Covered(WN *wn)
{
  SYMBOL symbol(wn);

  INT i;
  for (i=0; i<_scalar_use.Elements(); ++i) {
    if (symbol == _scalar_use.Bottom_nth(i)->_scalar) 
      return FALSE;
  }

  for (i=0; i<_scalar_def.Elements(); ++i) 
    if (symbol == _scalar_def.Bottom_nth(i)->_scalar) {
      return TRUE;
    }

  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Is_Problem_Scalar 
// FUNCTION: Returns TRUE if 'wn' is a scalar which prevents paralleliza-
//   tion of the loop either because 
//   it is in a MAY DEF set, and in an (exposed) USE set for the loop. 
//   it is in a MAY DEF set, and not in a MUST DEF set, and is live out 
//     of the loop corresponding to the ARA_LOOP_INFO. 
//-----------------------------------------------------------------------

BOOL 
ARA_LOOP_INFO::Is_Problem_Scalar(WN* wn)
{
  DU_MANAGER* du = Du_Mgr; 
  OPERATOR opr = WN_operator(wn); 
  if (opr != OPR_LDID && opr != OPR_STID)
    return FALSE; 

  REDUCTION_MANAGER* rm = red_manager; 
  if (rm != NULL && rm->Which_Reduction(wn) != RED_NONE)
    return FALSE; 

  SYMBOL sym(wn); 
  INT i;
  for (i = 0; i < _scalar_may_def.Elements(); i++) 
    if (_scalar_may_def.Bottom_nth(i)->_scalar == sym)
      break;
  if (i ==  _scalar_may_def.Elements())
    return FALSE; 
  for (i = 0; i < _scalar_use.Elements(); i++) 
    if (_scalar_use.Bottom_nth(i)->_scalar == sym)
      break; 
  if (i < _scalar_use.Elements())
    return TRUE; 
  for (i = 0; i < _scalar_def.Elements(); i++) 
    if (_scalar_def.Bottom_nth(i)->_scalar == sym)
       break;
  if (i < _scalar_def.Elements())
    return FALSE; 
  USE_LIST *use_list = du->Du_Get_Use(wn);
  if (use_list == NULL) 
    return FALSE; 
  if (use_list->Incomplete())
    return TRUE; 
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_use = node->Wn();
    if (!Wn_Is_Inside(wn_use, _loop))
      return TRUE; 
  } 
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: ARA_REF::Is_Messy
// FUNCTION: Return TRUE if the ARA_REF is too messy, or if any of its 
//   constituent REGIONs are too messy.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL ARA_REF::Is_Messy() 
{
  if (Is_Too_Messy())
    return TRUE; 
  REGION_ITER iter(&Image());
  const REGION *first = iter.First();
  for (const REGION *node=first; !iter.Is_Empty(); node = iter.Next()) {
    if (node->Is_Too_Messy())
      return TRUE; 
  } 
  return FALSE; 
} 

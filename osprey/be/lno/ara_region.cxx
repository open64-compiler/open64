/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.ara_region.cxx $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <limits.h>
#include "pu_info.h"
#include "ara_region.h"
#include "ara_loop.h"
#include "wn_map.h"
#include "lnopt_main.h"
#include "lno_bv.h"
#include "ara_utils.h"
#include "mempool.h"
#include "ipa_lno_util.h"

KERNEL_IMAGE::~KERNEL_IMAGE()
{ 
    if (_region) CXX_DELETE(_region, &ARA_memory_pool);
//    CXX_DELETE_ARRAY(_changed, &ARA_memory_pool);
    CXX_DELETE_ARRAY(_is_independent, &ARA_memory_pool);
}

CON_PAIR::CON_PAIR(CON_PAIR* c, INT dim)
{ 
  _ac_v = NULL; 
  if (c->_ac_v != NULL)
    _ac_v = CXX_NEW(ACCESS_VECTOR(c->_ac_v, &ARA_memory_pool), 
      &ARA_memory_pool);
  _coeff = NULL; 
  if (c->_coeff != NULL) {
    _coeff = CXX_NEW_ARRAY(INT, dim, &ARA_memory_pool);
    for (INT i = 0; i < dim; i++)
      _coeff[i] = c->_coeff[i];
  } 
} 

CON_PAIR::CON_PAIR(const CON_PAIR &a, const INT dim)
{
  if (a._ac_v) 
    _ac_v = CXX_NEW(ACCESS_VECTOR(a._ac_v, &ARA_memory_pool),&ARA_memory_pool);
  else
    _ac_v = NULL;

  if (a._coeff) {
    _coeff = CXX_NEW_ARRAY(INT, dim, &ARA_memory_pool);
    for (INT i = 0; i < dim; ++i) _coeff[i] = a._coeff[i];
  } else
    _coeff = NULL;
}

void
CON_PAIR::Print(FILE *fp, INT dim) const
{

  if (_coeff != NULL) {
    fprintf(fp,"(");
    for (INT i = 0; i < dim; ++i) 
      fprintf(fp," %d ",_coeff[i]);
    fprintf(fp,")");
  }
  if (_ac_v) 
    _ac_v->Print(fp);

}

void
CON_PAIR::WB_Print(FILE *fp, INT dim) const
{
  char bf[MAX_TLOG_CHARS];
  WB_Print(bf, dim, 0);
  fprintf(fp, "%s", bf);
}

INT
CON_PAIR::WB_Print(char* bf, INT ccount, INT dim) const
{
  INT new_ccount = ccount; 
  if (_coeff != NULL) {
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "(");
    for (INT i = 0; i < dim; ++i) {
      new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, " "); 
      new_ccount = snprintfd(bf, new_ccount, MAX_TLOG_CHARS, _coeff[i]);
      new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, " "); 
    }
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, ")");
  }
  if (_ac_v) 
    new_ccount = _ac_v->Print(bf, new_ccount, FALSE, FALSE);
  return new_ccount; 
}

void
CON_PAIR::Print_Analysis_Info(FILE *fp, INT dim, DOLOOP_STACK &do_stack)
{

  if (_coeff != NULL) {
    fprintf(fp,"(");
    for (INT i = 0; i < dim; ++i) 
      fprintf(fp," %d ",_coeff[i]);
    fprintf(fp,")");
  }
  if (_ac_v) _ac_v->Print_Analysis_Info(fp, do_stack);

}

BOOL CON_PAIR::Has_Formal_Parameter()
{
  return _ac_v->Has_Formal_Parameter();
} 

AXLE_NODE::AXLE_NODE(AXLE_NODE* a, INT dim)
{ 
  if (a->lo) {
    lo = CXX_NEW(CON_PAIR(a->lo, dim), &ARA_memory_pool);
  } else {
    lo = NULL;
  }

  if (a->up) {
    up = CXX_NEW(CON_PAIR(a->up, dim), &ARA_memory_pool);
  } else {
    up = NULL;
  }
  step = a->step;
} 

void
AXLE_NODE::Print(FILE *fp, INT dim) const
{

  if (lo) lo->Print(fp, dim);

  if (up) {
    fprintf(fp," : ");
    up->Print(fp, dim);
    fprintf(fp," : %d", step);
  }

}

void  
AXLE_NODE::WB_Print(FILE *fp, INT dim) const
{
  char bf[MAX_TLOG_CHARS]; 
  WB_Print(bf, dim, 0);
  fprintf(fp, "%s", bf);
}

INT   
AXLE_NODE::WB_Print(char* bf, INT ccount, INT dim) const
{
  INT new_ccount = ccount; 
  if (lo) 
    new_ccount = lo->WB_Print(bf, new_ccount, dim);
  if (up) {
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, ":");
    new_ccount = up->WB_Print(bf, new_ccount, dim);
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, ":");
    new_ccount = snprintfd(bf, new_ccount, MAX_TLOG_CHARS, step);
  }
  return new_ccount; 
}

void
AXLE_NODE::Print_Analysis_Info(FILE *fp, INT dim, INT indent, DOLOOP_STACK &do_stack)
{

  print_indent(fp,indent);
  fprintf(fp,"(\"");
  if (lo) lo->Print_Analysis_Info(fp, dim, do_stack);
  fprintf(fp,"\"");

  if (up) {
    fprintf(fp," \"");
    up->Print_Analysis_Info(fp, dim, do_stack);
    fprintf(fp,"\" \"%d\"", step);
  } 
  fprintf(fp,")\n");

}

BOOL AXLE_NODE::Has_Formal_Parameter()
{
  if (lo->Has_Formal_Parameter())
    return TRUE; 
  if (up->Has_Formal_Parameter())
    return TRUE; 
  return FALSE; 
} 

void 
REGION::Print(FILE *fp) const
{

  switch (_type) {
  case ARA_TOP:
    fprintf(fp,"Top \n");
    break;
  case ARA_BOTTOM:
    fprintf(fp,"Bottom \n");
    break;
  case ARA_TOO_MESSY:
    fprintf(fp,"Unknown \n");
    break;
  default:
    fprintf(fp,"[");
    INT i;
    for (i = 0; i < _dim-1; ++i) {
      _axle[i].Print(fp,_dim);
      fprintf(fp,",");
    }
    _axle[i].Print(fp,_dim);
    fprintf(fp,"] \n");
  }

}

void 
REGION::WB_Print(FILE *fp) const
{
  char bf[MAX_TLOG_CHARS]; 
  WB_Print(bf, 0);
  fprintf(fp, "%s", bf);
}

INT 
REGION::WB_Print(char* bf, INT ccount) const
{ 
  INT new_ccount = ccount; 
  switch (_type) {
  case ARA_TOP:
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "<Top>");
    break;
  case ARA_BOTTOM:
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "<Bottom>");
    break;
  case ARA_TOO_MESSY:
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "<Unknown>");
    break;
  default:
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "[");
    INT i;
    for (i = 0; i < _dim-1; ++i) {
      new_ccount = _axle[i].WB_Print(bf, new_ccount, _dim);
      new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "][");
    }
    new_ccount = _axle[i].WB_Print(bf, new_ccount, _dim);
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "]");
  }
  return new_ccount; 
}

void 
REGION::Print_Analysis_Info(FILE *fp, INT indent, DOLOOP_STACK &do_stack)
{

  switch (_type) {
  case ARA_TOP:
    print_indent(fp,indent);
    fprintf(fp,"(ALL)\n");
    break;
  case ARA_BOTTOM:
    print_indent(fp,indent);
    fprintf(fp,"(NULL)\n");
    break;
  case ARA_TOO_MESSY:
    print_indent(fp,indent);
    fprintf(fp,"(UNKNOWN)\n");
    break;
  default:
    for (INT i = 0; i < _dim; ++i) {
      _axle[i].Print_Analysis_Info(fp,_dim, indent, do_stack);
    }
  }

}

REGION::REGION(REGION* r) :
  _wn_list(&ARA_memory_pool)
{ 
  INT i; 
  _dim = r->_dim;
  _axle = CXX_NEW(AXLE_NODE(r->_axle, _dim), &ARA_memory_pool);
  _depth = r->_depth;
  _type = r->_type; 
  _coupled = r->_coupled; 
  _conditions = NULL; 
  if (r->_conditions != NULL) 
    _conditions = CXX_NEW(ACCESS_ARRAY(r->_conditions, &ARA_memory_pool), 
      &ARA_memory_pool);
  FmtAssert(_kernel == NULL, 
    ("REGION::REGION: Not sure how ro replicate this otherwise"));
  _kernel = NULL; 
  for (i = 0; i < r->_wn_list.Elements(); i++) 
    _wn_list.Push(r->_wn_list.Bottom_nth(i));
} 


REGION::REGION(const REGION &a) :
 _wn_list(&ARA_memory_pool) 
{

  _dim = a._dim;
  _type = a._type;
  _depth = a._depth;
  _coupled = a._coupled;
  _conditions = NULL;
  _axle = NULL;
  if (a._axle) {
    _axle = CXX_NEW_ARRAY(AXLE_NODE,_dim,&ARA_memory_pool);
    for (INT i = 0; i < _dim; ++i) 
      _axle[i].Set_Axle(a._axle[i].lo, a._axle[i].up, a._axle[i].step, _dim);
  } 
    
  if (a._conditions) 
    _conditions = CXX_NEW(ACCESS_ARRAY(a._conditions,&ARA_memory_pool),
			  &ARA_memory_pool);
  _kernel = a._kernel;
  for (INT i = 0; i < a._wn_list.Elements(); ++i)
    _wn_list.Push(a._wn_list.Bottom_nth(i));
  
}

BOOL REGION::Has_Formal_Parameter()
{
  for (INT i = 0; i < _dim; i++)
    if (_axle[i].Has_Formal_Parameter())
      return TRUE; 
  return FALSE; 
} 

void 
REGION_UN::Print(FILE *fp) const
{

  REGION_CONST_ITER iter(this);
  fprintf(fp,"{ \n");
  for (const REGION* cur = iter.First(); 
       !iter.Is_Empty(); cur = iter.Next())
    cur->Print(fp);
  fprintf(fp,"\n } \n");

}

void 
REGION_UN::WB_Print(FILE *fp) const
{
  char bf[MAX_TLOG_CHARS]; 
  WB_Print(bf, 0);
  fprintf(fp, "%s", bf);
}

INT 
REGION_UN::WB_Print(char* bf, INT ccount) const
{
  INT new_ccount = ccount; 
  REGION_CONST_ITER iter(this);
  INT count = 0;
  const REGION* cur = 0;
  for (cur = iter.First(); !iter.Is_Empty(); cur = iter.Next())
    count++; 
  if (count == 0)
    return new_ccount;
  if (count > 1)
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "{");
  for (cur = iter.First(); !iter.Is_Empty(); cur = iter.Next())
    new_ccount = cur->WB_Print(bf, new_ccount);
  if (count > 1)
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "} ");
  new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "\n");
  return new_ccount; 
}

void 
REGION_UN::Print_Analysis_Info(FILE *fp, INT indent, DOLOOP_STACK &do_stack)
{

  REGION_ITER iter(this);
  for (REGION* cur = iter.First(); 
       !iter.Is_Empty(); cur = iter.Next()) {
    print_indent(fp, indent); 
    fprintf(fp,"(SECTION \n");
    cur->Print_Analysis_Info(fp,indent+3,do_stack);
    print_indent(fp, indent); fprintf(fp, ")\n");
  }

}

BOOL REGION_UN::Has_Formal_Parameter()
{
  REGION_ITER riter(this);
  for (REGION* rg = riter.First(); !riter.Is_Empty(); rg = riter.Next()) {
    if (rg->_type == ARA_TOP || rg->_type == ARA_BOTTOM
          || rg->_type == ARA_TOO_MESSY)
        continue;
    if (rg->Has_Formal_Parameter())
      return TRUE; 
  } 
  return FALSE; 
} 

void
AXLE_NODE::Init_To_Access(ACCESS_VECTOR *av)
{
  lo = CXX_NEW(CON_PAIR(av), &ARA_memory_pool);
  up = NULL;
  step = 1;
}
  
void
AXLE_NODE::Set_Axle(const CON_PAIR *lo_new, const CON_PAIR *up_new,
		    const INT16 step_new, const INT dim)
{
  if (lo) CXX_DELETE(lo,&ARA_memory_pool);
  if (up) CXX_DELETE(up,&ARA_memory_pool);
  if (lo_new) lo = CXX_NEW(CON_PAIR(*lo_new,dim),&ARA_memory_pool);
  if (up_new) up = CXX_NEW(CON_PAIR(*up_new,dim),&ARA_memory_pool);
  step = step_new;
}

void
AXLE_NODE::Clear()
{
  if (lo) CXX_DELETE(lo,&ARA_memory_pool);
  if (up) CXX_DELETE(up,&ARA_memory_pool);
  lo = up = NULL;
  step = 0;
}

//=============================================================================
// Two axles are equivalent if their steps and constraints are the same.
//=============================================================================
BOOL
Equivalent(const AXLE_NODE &a, const AXLE_NODE &b, const INT dim)
{
  return ((a.step == b.step) &&
	  ((a.lo == NULL && b.lo == NULL) || 
	   (a.lo != NULL && b.lo != NULL && Equivalent(*a.lo,*b.lo,dim))) &&
	  ((a.up == NULL && b.up == NULL) || 
	   (a.up != NULL && b.up != NULL && Equivalent(*a.up,*b.up,dim))));
}

//=============================================================================
// Two constraint pairs are equivalent if their corresponding coefficients
// are the same.
//=============================================================================
BOOL
Equivalent(const CON_PAIR &a, const CON_PAIR &b, const INT dim)
{

  if (a._ac_v) {
    if (b._ac_v == NULL) 
      return FALSE;
    else if ( !(*a._ac_v == *b._ac_v) )
      return FALSE;
  } else if (b._ac_v != NULL) 
    return FALSE;
  
  if (a._coeff) {
    if (b._coeff == NULL)
      return FALSE;
    else
      for (INT i = 0; i < dim; ++i) 
	if (a._coeff[i]!=b._coeff[i]) return FALSE;
  } else if (b._coeff != NULL) 
    return FALSE;
  
  return TRUE;

}

// Create a region node from array access node wn
REGION::REGION(WN* wn, ARA_LOOP_INFO *ara_loop_info):_wn_list(&ARA_memory_pool)
{
  Is_True(ara_loop_info,("Must have ARA loop info!\n"));
  KERNEL_LIST & kernels = ara_loop_info->Kernels();
  INT16 depth = ara_loop_info->Depth()+1;

  // Defaults of dynamic allocated variables
  _type         = ARA_TOO_MESSY;
  _axle         = NULL;
  _conditions   = NULL;
  _kernel       = NULL;
  _depth        = depth;
  _dim          = WN_num_dim(wn);
  _wn_list.Push(wn);

  ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn);

  // Before we allocate the space, let's see if the access pattern is good.
  if (!array || array->Too_Messy) return;

  INT i;
  for (i = 0; i < array->Num_Vec(); ++i) {
    ACCESS_VECTOR *av = array->Dim(i);
    if (av->Too_Messy || av->Contains_Non_Lin_Symb() || av->Delinearized_Symbol) return;
  }
  
  // Now, the access array is good. Build the REGION node.
  _type = ARA_NORMAL;

  _axle = CXX_NEW_ARRAY(AXLE_NODE,_dim,&ARA_memory_pool);
  for (i = 0; i < array->Num_Vec(); ++i) {
    ACCESS_VECTOR *av = array->Dim(i);
    _axle[i].Init_To_Access(av);
  }

  // Find the kernel for the region
  KERNEL_SLIST_ITER iter(&kernels);
  for (KERNEL_IMAGE *kern = iter.First(); _kernel==NULL &&
       !iter.Is_Empty(); kern = iter.Next()) {

    if (kern->Depth() != depth) continue;

    ACCESS_ARRAY *cur = kern->Get_Kernel();
    if (array->Num_Vec() != cur->Num_Vec()) continue;
    BOOL match=TRUE;
    
    for (INT i = 0; i < array->Num_Vec(); ++i) {

      ACCESS_VECTOR *av = array->Dim(i);
      ACCESS_VECTOR *ker_av = cur->Dim(i);

      INT j;
      for (j = 0; j < depth; ++j) 
	if (av->Loop_Coeff(j)!= ker_av->Loop_Coeff(j)) break;
      
      if (j != depth) {
	match = FALSE;
	break;
      }

    }

    if (match) {
      _kernel = kern;
      break;
    }

  }
  
  if (_kernel == NULL) {
    _kernel = CXX_NEW(KERNEL_IMAGE(array, ara_loop_info), &ARA_memory_pool);
    kernels.Append(_kernel);
  }

}

// Create a region node from an wn (e.g. a call) and its access_array
// a call can access many arrays and therefore can have many access_arrays
REGION::REGION(WN* wn, ACCESS_ARRAY* array):_wn_list(&ARA_memory_pool)
{
  INT16 depth = Do_Loop_Depth(Enclosing_Do_Loop(wn))+1;

  // Defaults of dynamic allocated variables
  _type         = ARA_TOO_MESSY;
  _axle         = NULL;
  _conditions   = NULL;
  _kernel       = NULL;
  _depth        = depth;
  _dim          = array->Num_Vec();
  _wn_list.Push(wn);

  // Before we allocate the space, let's see if the access pattern is good.
  if (!array || array->Too_Messy) return;

  INT i;
  for (i = 0; i < array->Num_Vec(); ++i) {
    ACCESS_VECTOR *av = array->Dim(i);
    if (av->Too_Messy || av->Contains_Non_Lin_Symb() || av->Delinearized_Symbol) return;
  }
  
  // Now, the access array is good. Build the REGION node.
  _type = ARA_NORMAL;

  _axle = CXX_NEW_ARRAY(AXLE_NODE,_dim,&ARA_memory_pool);
  for (i = 0; i < array->Num_Vec(); ++i) {
    ACCESS_VECTOR *av = array->Dim(i);
    _axle[i].Init_To_Access(av);
  }

  if (_kernel == NULL) {
    _kernel = CXX_NEW(KERNEL_IMAGE(array), &ARA_memory_pool);
  }

}

INT
Find_Non_Const_Loops(const SYMBOL &x, const ARA_LOOP_INFO &ara_loop_info)
{

  const ARA_LOOP_INFO *ara_info = &ara_loop_info;

  while (ara_info) {
    if (!ara_info->Is_Invariant(x)) return (ara_info->Depth() + 1);
    ara_info = ara_info->Parent();
  }

  return 0;

}

INT Locate_Sym(SYMBOL_LIST *syms, const SYMBOL &x, SYSTEM_OF_EQUATIONS *soe, INT_ST & non_const_loops, const ARA_LOOP_INFO & ara_loop_info)
{

  SYMBOL_ITER iter(syms);
  INT loc = 0;

  for (SYMBOL_NODE *s = iter.First(); !iter.Is_Empty(); 
       ++loc, s =  iter.Next()) 
    if (s->Symbol == x) return loc;
  
  // a new symbol
  syms->Append(CXX_NEW(SYMBOL_NODE(x,FALSE), &LNO_local_pool));
  non_const_loops.Push(Find_Non_Const_Loops(x,ara_loop_info));
  soe->Add_Vars(1);
  return loc;

}

enum ARA_ACT_TYPE
{
  ARA_LO_BD, ARA_UP_BD, ARA_EQN
};

//======================================================================
// Add an access vector to the system of equations
//
// The system of equations is setup as:
// 
// A*X + B*Y + C*Z <= b
//  ---- X is a vector representing the axles of the region
//  ---- Y is a vector representing the enclosing loop indices
//  ---- Z is a list of symbolic variables in the linear term
//  ---- b is a vector of constant offset
//====================================================================== 
void
Add_Access(ACCESS_VECTOR *av, const INT32 coeff[],
	   SYSTEM_OF_EQUATIONS *soes, SYMBOL_LIST *syms, 
	   INT_ST & non_const_loops,
	   const mUINT16 depth, const INT num_dim, 
	   const INT axle, const ARA_ACT_TYPE act, 
	   const ARA_LOOP_INFO & ara_info,
	   BOOL ignore_sym = FALSE)
{

  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout,"Add access vector: \n");
    av->Print(stdout);
    fprintf(stdout,"\n To SOE: \n");
    soes->Print(stdout);
  }

  Is_True(!av->Too_Messy,("Add_Access: Too messy access vector passed in"));
  INT vsz = ((av && av->Lin_Symb && !ignore_sym) ? av->Lin_Symb->Len() : 0) + 
    syms->Len() + num_dim + depth + 1;
  
  mINT32 *v = CXX_NEW_ARRAY(mINT32, vsz, &LNO_local_pool);

  bzero(v, sizeof(mINT32) * vsz);

  // set the value of matrix A
  if (coeff!=NULL) 
    for (INT i = 0; i < num_dim; ++i) 
      v[i] = coeff[i];
  else if (act == ARA_LO_BD)
    v[axle] = -1;
  else
    v[axle] = 1;


  if (av) {
    // Loop indices
    for (INT i = 0; i < depth; ++i) {


      // Note that num_dim+i might have been set by v[axle]
      v[num_dim+i] = av->Loop_Coeff(i);
    }
  
    // Symbolic constants
    if (av->Contains_Lin_Symb() && !ignore_sym) {
      INTSYMB_ITER iter(av->Lin_Symb);
      for (INTSYMB_NODE *cur = iter.First(); 
	   !iter.Is_Empty(); cur = iter.Next()) {
	INT pos = Locate_Sym(syms,cur->Symbol,soes, non_const_loops, ara_info);
	v[num_dim+depth+pos] = cur->Coeff;
      }
    }
  }

  if (act != ARA_LO_BD) {
    for (INT i = num_dim; i < vsz; ++i) 
      v[i] = -v[i];
  }

  // constant offset on the rhs
  INT c = av ? av->Const_Offset : 0 ;
  if (act == ARA_LO_BD) c = -c;

  if (act != ARA_EQN)
    soes->Add_Le(v, c);
  else
    soes->Add_Eq(v, c);

  CXX_DELETE_ARRAY(v, &LNO_local_pool);

  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout,"New SOE is: \n");
    soes->Print(stdout);
  }

}

//======================================================================
// Add a bound to the system of equations 
// (almost the same as Add_Access, but for UB and LB)
//
// The system of equations is setup as:
// 
// A*X + B*Y + C*Z <= b
//  ---- X is a vector representing the axles of the region
//  ---- Y is a vector representing the enclosing loop indices
//  ---- Z is a list of symbolic variables in the linear term
//  ---- b is a vector of constant offset
//====================================================================== 
void
Add_Bound(ACCESS_VECTOR *av, 
	  SYSTEM_OF_EQUATIONS *soes, SYMBOL_LIST *syms, 
	  INT_ST & non_const_loops,
	  const mUINT16 depth, const INT num_dim, 
	  const ARA_LOOP_INFO & ara_info)
{

  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout,"Add access vector: \n");
    av->Print(stdout);
    fprintf(stdout,"\n To SOE: \n");
    soes->Print(stdout);
  }

  INT vsz = ((av && av->Lin_Symb) ? av->Lin_Symb->Len() : 0) + 
    syms->Len() + num_dim + depth + 1;
  
  mINT32 *v = CXX_NEW_ARRAY(mINT32, vsz, &LNO_local_pool);

  bzero(v, sizeof(mINT32) * vsz);

  if (av) {
    // Loop indices
    for (INT i = 0; i < depth; ++i) {


      // Note that num_dim+i might have been set by v[axle]
      v[num_dim+i] = av->Loop_Coeff(i);
    }
  
    // Symbolic constants
    if (av->Contains_Lin_Symb()) {
      INTSYMB_ITER iter(av->Lin_Symb);
      for (INTSYMB_NODE *cur = iter.First(); 
	   !iter.Is_Empty(); cur = iter.Next()) {
	INT pos = Locate_Sym(syms,cur->Symbol,soes, non_const_loops, ara_info);
	v[num_dim+depth+pos] = cur->Coeff;
      }
    }
  }

  // constant offset on the rhs
  INT c = av ? av->Const_Offset : 0 ;

  soes->Add_Le(v, c);

  CXX_DELETE_ARRAY(v, &LNO_local_pool);

  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout,"New SOE is: \n");
    soes->Print(stdout);
  }

}

//============================================================================
//
// Add the axle 'pos' of REGION 'a' to the SOE 'soe'.
// Convert one equation into two inequalities if convert_equation is TRUE
//
//============================================================================
void
Add_To_SOE(const REGION &a, const INT pos, SYSTEM_OF_EQUATIONS *soe,
	   SYMBOL_LIST *syms, INT_ST & non_const_loops, 
	   const BOOL convert_equation, const ARA_LOOP_INFO &ara_info)
{

  if (a._axle[pos].up != NULL){
    Add_Access(a._axle[pos].lo->_ac_v, a._axle[pos].lo->_coeff, soe, syms, 
	       non_const_loops, a._depth, a._dim, pos, ARA_LO_BD, ara_info);
    Add_Access(a._axle[pos].up->_ac_v, a._axle[pos].up->_coeff, soe, syms, 
	       non_const_loops, a._depth, a._dim, pos, ARA_UP_BD, ara_info);
  } else {
    if (convert_equation) {
      Add_Access(a._axle[pos].lo->_ac_v, a._axle[pos].lo->_coeff, soe, syms,
		 non_const_loops, a._depth, a._dim, pos, ARA_LO_BD, ara_info);
      Add_Access(a._axle[pos].lo->_ac_v, a._axle[pos].lo->_coeff, soe, syms,
		 non_const_loops, a._depth, a._dim, pos, ARA_UP_BD, ara_info);
    } else
      Add_Access(a._axle[pos].lo->_ac_v, a._axle[pos].lo->_coeff, soe, syms,
		 non_const_loops, a._depth, a._dim, pos, ARA_EQN, ara_info);
  }

}

//============================================================================
//
// Determine if two inequalities are actually one equality.
// It returns TRUE if the vector sum of them is a zero vector.
//
//============================================================================
BOOL
is_equality(const SYSTEM_OF_EQUATIONS *soe, const INT i, const INT j)
{

  for (INT k = 0; k < soe->Num_Vars(); ++k) 
    if ((soe->Work(i,k)+soe->Work(j,k)) != 0) return FALSE;

  return (soe->Work_Const(i) + soe->Work_Const(j) == 0);

}

INT
Max_Non_Const_Loop(const SYSTEM_OF_EQUATIONS *soe, const INT i, const INT offset, const INT which_array, const INT_ST & non_const_loops)
{

  INT res = 0;
  INT k;
  switch (which_array) {
  case 0:
    for (k = 0; k < non_const_loops.Elements(); ++k) {
      if (soe->Work(i,offset+k)!=0)
	res = MAX(res, non_const_loops.Bottom_nth(k));
    }
    break;
  case 1:
    for (k = 0; k < non_const_loops.Elements(); ++k) {
      if (soe->Aeq()(i,offset+k)!=0)
	res = MAX(res, non_const_loops.Bottom_nth(k));
    }
    break;
  case 2:
    for (k = 0; k < non_const_loops.Elements(); ++k) {
      if (soe->Ale()(i,offset+k)!=0)
	res = MAX(res, non_const_loops.Bottom_nth(k));
    }
    break;
  default:
    FmtAssert(0,("Max_Non_Const_Loop: illegal array specification"));
  }

  return res;

}

void
AXLE_NODE::Set_Axle(const SYSTEM_OF_EQUATIONS *soe,
		    const INT i, const INT j, const SYMBOL_LIST *syms,
		    const INT depth, const INT dim,
		    const INT_ST & non_const_loops,
		    const INT stride)
{

  if (lo) CXX_DELETE(lo,&ARA_memory_pool);
  if (up) CXX_DELETE(up,&ARA_memory_pool);

  step = stride;

  // Two inequalities form an equality
  if (is_equality(soe, i, j)) {
    up = NULL;
    lo = CXX_NEW(CON_PAIR(), &ARA_memory_pool);
    lo->_ac_v = 
      CXX_NEW(ACCESS_VECTOR(soe,i,syms,depth,dim,
			    Max_Non_Const_Loop(soe,i,dim+depth,0,non_const_loops),
			    0, TRUE, &ARA_memory_pool),&ARA_memory_pool);
    
    INT k;
    for (k = 0; k<dim; ++k)
      if (soe->Work(i,k) != 0 && 2*k != i) {
	lo->_coeff = CXX_NEW_ARRAY(INT32, dim, &ARA_memory_pool);
	for (INT l = 0; l < dim; ++l) lo->_coeff[l] = 0;
	break;
      }

    if (lo->_coeff)
      for (k = 0; k<dim; ++k)
	lo->_coeff[k] = soe->Work(i,k);

    return;
  }
  
  // Set the lower and upper bounds
  lo = CXX_NEW(CON_PAIR(), &ARA_memory_pool);
  lo->_ac_v = 
    CXX_NEW(ACCESS_VECTOR(soe,i,syms,depth,dim,
			  Max_Non_Const_Loop(soe,i,dim+depth,0,non_const_loops),
			  0, TRUE ,&ARA_memory_pool),&ARA_memory_pool);

  INT k;
  for (k = 0; k<dim; ++k)
    if (soe->Work(i,k) != 0 && 2*k != i) {
      lo->_coeff = CXX_NEW_ARRAY(INT32, dim, &ARA_memory_pool);
      for (INT l = 0; l < dim; ++l) lo->_coeff[l] = 0;
      break;
    }

  if (lo->_coeff)
    for (k = 0 ; k < dim; ++k)
      lo->_coeff[k] = soe->Work(i,k);
  
  up = CXX_NEW(CON_PAIR(), &ARA_memory_pool);
  up->_ac_v = CXX_NEW(ACCESS_VECTOR(soe,j,syms,depth,dim,
				    Max_Non_Const_Loop(soe,j,dim+depth,0,non_const_loops),
				    0, FALSE ,&ARA_memory_pool),&ARA_memory_pool);
  for (k = 0; k<dim; ++k)
    if (soe->Work(i,k) != 0 && 2*k != i) {
      up->_coeff = CXX_NEW_ARRAY(INT32, dim, &ARA_memory_pool);
      for (INT l = 0; l < dim; ++l) up->_coeff[l] = 0;
      break;
    }

  if (up->_coeff)
    for (k = 0; k < dim; ++k) 
      up->_coeff[k] = soe->Work(i,k);

}
  
void 
AXLE_NODE::Set_Axle_Eq(const SYSTEM_OF_EQUATIONS *soe,
		       const INT i, const INT j, const SYMBOL_LIST *syms,
		       const INT depth, const INT dim,
		       const INT_ST & non_const_loops,
		       const INT stride)
{

  step = stride;

  // Set the lower or upper bounds
  if (lo==NULL) lo = CXX_NEW(CON_PAIR(),&ARA_memory_pool);

  if (lo->_ac_v) CXX_DELETE(lo->_ac_v, &ARA_memory_pool);
  if (lo->_coeff) {
    CXX_DELETE_ARRAY(lo->_coeff, &ARA_memory_pool);
    lo->_coeff = NULL;
  }

  if (up) {
    CXX_DELETE(up, &ARA_memory_pool);
    up = NULL;
  }

  lo->_ac_v = 
    CXX_NEW(ACCESS_VECTOR(soe,i,syms,depth,dim,
			  Max_Non_Const_Loop(soe,i,dim+depth,1,non_const_loops),
			  TRUE,1,&ARA_memory_pool),&ARA_memory_pool);

  INT k;
  for (k = 0; k<dim; ++k)
    if (soe->Aeq()(i,k) != 0 && k!=j) {
      lo->_coeff = CXX_NEW_ARRAY(INT32, dim, &ARA_memory_pool);
      for (INT l = 0; l < dim; ++l) lo->_coeff[l] = 0;
      break;
    }

  if (lo->_coeff)
    for (k = 0; k < dim; ++k)
      lo->_coeff[k] = soe->Aeq()(i,k);

}

void
AXLE_NODE::Set_Axle_Le(const SYSTEM_OF_EQUATIONS *soe,
		       const INT i, const INT j, const SYMBOL_LIST *syms,
		       const INT depth, const INT dim,
		       const INT_ST & non_const_loops,
		       const INT stride)
{

  step = stride;

  // Set the lower bound
  if (soe->Ale()(i,j)<0) {
    if (lo==NULL) lo = CXX_NEW(CON_PAIR(),&ARA_memory_pool);

    if (lo->_ac_v) CXX_DELETE(lo->_ac_v, &ARA_memory_pool);
    if (lo->_coeff) {
      CXX_DELETE_ARRAY(lo->_coeff, &ARA_memory_pool);
      lo->_coeff = NULL; 
    }
    lo->_ac_v = 
      CXX_NEW(ACCESS_VECTOR(soe,i,syms,depth,dim,
			    Max_Non_Const_Loop(soe,i,dim+depth,2,non_const_loops),
			    2,TRUE,&ARA_memory_pool),&ARA_memory_pool);

    INT k;
    for (k = 0; k<dim; ++k)
      if (soe->Ale()(i,k) != 0 && k != j) {
	lo->_coeff = CXX_NEW_ARRAY(INT32, dim, &ARA_memory_pool);
	for (INT l = 0; l < dim; ++l) lo->_coeff[l] = 0;
	break;
      }

    if (lo->_coeff)
      for (k = 0; k < dim; ++k)
	lo->_coeff[k] = soe->Ale()(i,k);

  } else {

    if (up==NULL) up = CXX_NEW(CON_PAIR(),&ARA_memory_pool);
    if (up->_ac_v) CXX_DELETE(up->_ac_v,&ARA_memory_pool);
    if (up->_coeff) {
      CXX_DELETE_ARRAY(up->_coeff,&ARA_memory_pool);
      up->_coeff = NULL;
    }

    up->_ac_v = 
      CXX_NEW(ACCESS_VECTOR(soe,i,syms,depth,dim,
			    Max_Non_Const_Loop(soe,j,dim+depth,2,non_const_loops),
			    2,FALSE,&ARA_memory_pool),&ARA_memory_pool);

    INT k;
    for (k = 0; k<dim; ++k)
      if (soe->Ale()(i,k) != 0 && k != j) {
	up->_coeff = CXX_NEW_ARRAY(INT32, dim, &ARA_memory_pool);
	for (INT l = 0; l < dim; ++l) up->_coeff[l] = 0;
	break;
      }
    if (up->_coeff)
      for (k = 0; k < dim; ++k)
	up->_coeff[k] = soe->Ale()(i,k);

  }

}
  
//======================================================================
//
// Set this region according to the SOE 'soe'
// Simply read the coefficients in the SOE and set the corresponding
// axles of the region.
//
//======================================================================
void 
REGION::Set_Region(const SYSTEM_OF_EQUATIONS * soe, 
		   const SYMBOL_LIST * syms,
		   const INT_ST & non_const_loops,
		   const INT strides[])
{

  Is_True(soe, ("Null pointer passed to Set_Region"));

  _type = ARA_NORMAL;
  if (_axle==NULL) _axle = CXX_NEW_ARRAY(AXLE_NODE,_dim,&ARA_memory_pool);
  
  // Now, read in each dimension
  for (INT i = 0; i < _dim; ++i) {

    // The inequalities are suposed to be in the correct order
    Is_True(soe->Work(2*i,i) && soe->Work(2*i+1,i),
	    ("SOE is not in correct order"));

    _axle[i].Set_Axle(soe, 2*i, 2*i+1, syms, _depth, _dim,
		      non_const_loops, strides[i]);
  }

}
  
//=============================================================================
//
// Set region according to the SOE, pivot_row and stride
//
//=============================================================================
void
REGION::Set_Region(const SYSTEM_OF_EQUATIONS * soe,
		   const SYMBOL_LIST * syms,
		   const INT_ST & non_const_loops,
		   INT   strides[],
		   const INT pivot_row,
		   const INT pos,
		   const INT loop_step,
		   const INT projected_axle)
{
  
  Is_True(soe, ("Null SOE pointer passed to Set_Region"));
  
  _type = ARA_NORMAL;
  if (_axle==NULL) _axle = CXX_NEW_ARRAY(AXLE_NODE,_dim,&ARA_memory_pool);

  INT i;
  for (i = 0; i < _dim; ++i) _axle[i].Clear();

  // Set the stride of the pivoting axle
  INT pivot_column = _dim+pos;
  INT multiple = - soe->Aeq()(pivot_row,pivot_column);
  for (i = 0; i < _dim; ++i) {
    if (soe->Aeq()(pivot_row,i)!=0) {
      strides[i]=multiple*loop_step;
      break;
    }
  }
  
  MEM_POOL_Push(&LNO_local_pool);
  {
    BIT_VECTOR *ancestor_lo = CXX_NEW(BIT_VECTOR(_dim,&LNO_local_pool), &LNO_local_pool);

    BIT_VECTOR *ancestor_up = CXX_NEW(BIT_VECTOR(_dim,&LNO_local_pool), &LNO_local_pool);

    // Set the equality axles first
    INT k = 0;
    for (i = 0; i < soe->Num_Eq_Constraints(); ++i) 
      if (i!=pivot_row) {
#if Is_True_On
	BOOL found = FALSE;
#endif
	for (; k < _dim; ++k) 
	  if (k!=projected_axle && soe->Aeq()(i,k)!=0) {
	    _axle[k].Set_Axle_Eq(soe, i, k, syms, _depth, _dim, non_const_loops, strides[k]);
	    Is_True(!ancestor_lo->Test(k),("REGION::Set_Region: axle already set"));
	    ancestor_lo->Set(k);
	    ancestor_up->Set(k);
#if Is_True_On
	    found = TRUE;
#endif
	    break;
	  }
	Is_True(found,("REGION::Set_Region: cannot find an axle to set"));
      }
  
    Is_True((_dim-ancestor_lo->Pop_Count())*2 == soe->Num_Le_Constraints(),("REGION:Set_Region: Inequality constraints do not match the number of dimensions"));

    // Filling the axles according to the topologic order (dependance)
    BOOL progress = TRUE;
    while (progress && ((ancestor_up->Pop_Count() != _dim) ||
	   (ancestor_lo->Pop_Count() != _dim))) {

      progress = FALSE;

      for (i = 0; i < soe->Num_Le_Constraints(); ++i) {
	INT axle = -1;
	for (INT k = 0; k < _dim; ++k) 
	  if (soe->Ale()(i,k)<0 && !ancestor_lo->Test(k) ||
	      soe->Ale()(i,k)>0 && !ancestor_up->Test(k)) {
	    if (axle>=0) {
	      axle = -1;
	      break;
	    } else
	      axle = k;
	  }

	if (axle>=0) { 

	// Found one that only depends on the already converted axles.
	  progress = TRUE;

	  if (soe->Ale()(i,axle)<0) {
	    _axle[axle].Set_Axle_Le(soe, i, axle, syms, _depth, _dim, non_const_loops, strides[axle]);
	    ancestor_lo->Set(axle);
	  } else {
	    _axle[axle].Set_Axle_Le(soe, i, axle, syms, _depth, _dim, non_const_loops, strides[axle]);
	    ancestor_up->Set(axle);
	  }

	}

      } // for
      
    } // while

    if (!progress) {
      Set_Too_Messy();
      if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
	fprintf(stdout,"REGION::Set_Region: No progress\n");
      }
    }

    CXX_DELETE(ancestor_lo, &LNO_local_pool);
    CXX_DELETE(ancestor_up, &LNO_local_pool);
  }
  MEM_POOL_Pop(&LNO_local_pool);

}

//=============================================================================
//
// Return the intersection of two REGIONs.
// Return NULL if the underestimation is BOTTOM.
//
//=============================================================================
REGION *
Region_Intersect(const REGION &a, const REGION &b, const ARA_LOOP_INFO & ara_info)
{

  // Check if the coefficients on unprojected loop indices
  // are the same.  If not, the intersection is empty.
  // First, the simple tests.
  if (/* a._kernel != b._kernel || */ a._type==ARA_BOTTOM||b._type==ARA_BOTTOM) 
    return NULL;

  // the simple tests.
  if (a._type==ARA_TOP) {
    REGION* result = CXX_NEW(REGION(b),&ARA_memory_pool);
    for (INT i = 0; i < a._wn_list.Elements(); ++i)
      result->_wn_list.Push(a._wn_list.Bottom_nth(i));
    return result;
  }

  if (b._type==ARA_TOP) {
    REGION* result = CXX_NEW(REGION(a),&ARA_memory_pool);
    for (INT i = 0; i < b._wn_list.Elements(); ++i)
      result->_wn_list.Push(b._wn_list.Bottom_nth(i));
    return result;
  }

  // dimension are not the same, hence we cannot create a region.
  // simply return NULL
  if (a._dim != b._dim)
    return NULL;

  Is_True(a._dim==b._dim,("Try to merge arrays with different dimensions"));

  // Now the offset on the equations should be the same
  // TODO: Add a method to access_vector.h that compares
  // only the non-kernel part of the vector.
  INT i;
  for (i = 0; i < a._dim; ++i) {
    if (a._axle[i].up == NULL && b._axle[i].up == NULL &&
	!(Equivalent(*a._axle[i].lo, *b._axle[i].lo, a._dim)))
      return NULL;
  }

  // Now, create a new region.
  INT depth = MIN(a._depth, b._depth);
  INT dim   = a._dim;
  REGION * result = CXX_NEW(REGION(depth,dim), &ARA_memory_pool);
  result->_dim = dim;
  result->_depth = depth;
  result->_axle = CXX_NEW_ARRAY(AXLE_NODE, result->_dim, &ARA_memory_pool);
  
  // copy the WN stacks
  for (i = 0; i < a._wn_list.Elements(); ++i) 
    result->_wn_list.Push(a._wn_list.Bottom_nth(i));
  for (i = 0; i < b._wn_list.Elements(); ++i)
    result->_wn_list.Push(b._wn_list.Bottom_nth(i));

  MEM_POOL_Push(&LNO_local_pool);
  {
    // Find the intersection using SOE
    SYSTEM_OF_EQUATIONS *soe = CXX_NEW(SYSTEM_OF_EQUATIONS(0,0,dim+depth,&LNO_local_pool), &LNO_local_pool);

    SYMBOL_LIST *syms = CXX_NEW(SYMBOL_LIST, &LNO_local_pool);

    // New non_const_loops for each access vector
    INT_ST non_const_loops(&LNO_local_pool);

    // New stride for each dimension
    INT *strides = CXX_NEW_ARRAY(INT, result->_dim, &LNO_local_pool);
  
    // Build the system of equations for the constraints
    for (i = 0; i < dim; ++i) {

      if (a._axle[i].up == NULL && b._axle[i].up == NULL) {

	// If the two equalities are the same, use one of them
	ACCESS_VECTOR *av = a._axle[i].lo->_ac_v;
	INT32         *coeff = a._axle[i].lo->_coeff;
	strides[i] = 1;

	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_LO_BD, ara_info);
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_UP_BD, ara_info);

	/* added by nawaaz */
	av = b._axle[i].lo->_ac_v;
	coeff = b._axle[i].lo->_coeff;
      
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_LO_BD, ara_info);
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_UP_BD, ara_info);

      } else if (a._axle[i].up == NULL && b._axle[i].up != NULL) {
	// One equality against several inequalities.
	// Change the equality into two inequalities and put them into
	// the system of equations

	ACCESS_VECTOR *av = a._axle[i].lo->_ac_v;
	INT32         *coeff = a._axle[i].lo->_coeff;


	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_LO_BD, ara_info);
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_UP_BD, ara_info);
      
	av = b._axle[i].lo->_ac_v;
	coeff = b._axle[i].lo->_coeff;
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_LO_BD, ara_info);

	av = b._axle[i].up->_ac_v;
	coeff = b._axle[i].up->_coeff;
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_UP_BD, ara_info);

      } else if (a._axle[i].up != NULL && b._axle[i].up == NULL) {

	ACCESS_VECTOR *av = a._axle[i].lo->_ac_v;
	INT32         *coeff = a._axle[i].lo->_coeff;
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_LO_BD, ara_info);

	av = a._axle[i].up->_ac_v;
	coeff = a._axle[i].up->_coeff;
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_UP_BD, ara_info);

	av = b._axle[i].lo->_ac_v;
	coeff = b._axle[i].lo->_coeff;
      
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_LO_BD, ara_info);

	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_UP_BD, ara_info);
      
      } else {
	// Both of them are inequalities

	ACCESS_VECTOR *av = a._axle[i].lo->_ac_v;
	INT32         *coeff = a._axle[i].lo->_coeff;
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_LO_BD, ara_info);

	av = a._axle[i].up->_ac_v;
	coeff = a._axle[i].up->_coeff;
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_UP_BD, ara_info);

	av = b._axle[i].lo->_ac_v;
	coeff = b._axle[i].lo->_coeff;
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_LO_BD, ara_info);

	av = b._axle[i].up->_ac_v;
	coeff = b._axle[i].up->_coeff;
	Add_Access(av, coeff, soe, syms, non_const_loops, depth, dim, i, ARA_UP_BD, ara_info);

      }

      // Handle the step of the intersection
      Is_True(a._axle[i].step > 0 && b._axle[i].step > 0, ("Negative stride found"));
      strides[i] = a._axle[i].step;

      if (a._axle[i].step != b._axle[i].step) {
	const IMAT & ale = soe->Ale();
	INT row = ale.Rows();
	for (INT k = 0; k < ale.Cols(); ++k) {
	  if (ale(row-1,k) != ale(row-3,k)) {
	    CXX_DELETE_ARRAY(strides, &LNO_local_pool);
	    CXX_DELETE(result, &ARA_memory_pool);
	    CXX_DELETE(syms, &LNO_local_pool);
	    CXX_DELETE(soe, &LNO_local_pool);
	    result = NULL;
	    goto return_point;
	  }
	}
      
	INT64 *ble = soe->Ble();
	INT offset1 = -ble[row-1];
	INT offset2 = -ble[row-3];
	INT diff = abs(offset1-offset2);
      
	strides[i] = MAX(a._axle[i].step,b._axle[i].step);
	if (diff == 0) continue;

	// Now do a GCD test
	INT g = Gcd(a._axle[i].step,b._axle[i].step);
	if ((diff % g) != 0) {
	  CXX_DELETE_ARRAY(strides, &LNO_local_pool);
	  CXX_DELETE(result, &ARA_memory_pool);
	  CXX_DELETE(syms, &LNO_local_pool);
	  CXX_DELETE(soe, &LNO_local_pool);
	  result = NULL;
	  goto return_point;
	}
      
	// Now, the two regions must meet at some points.
	// Let's do a search on the minimum one.
	while (offset1 != offset2) {
	  if (offset1 < offset2) 
	    offset1 += a._axle[i].step;
	  else 
	    offset2 += b._axle[i].step;
	} 

	// Set the new constant offset for the lower bound.
	// Get rid one of the redundant inequality.
	ble[row-3] = -offset1;
	soe->Remove_Le_Number(row-1);

      }
    }

    // Now, simplify the constraints and read back the intersection
    if (soe->Copy_To_Work()) {
      INT num_con = soe->Work_Constraints();

      // For uncoupled regions, the following is sufficient to find
      // the minimum constraints
      soe->Elim_Simple_Redundant();

      if (2*soe->Work_Constraints() == num_con) {
	// Two constraints for each axle. It is already minimum.
	// Read back the constraints.
	if (soe->Is_Consistent_Work()) {

	  // recopy the constraints -- added by nawaaz
	  soe->Copy_To_Work();
	  soe->Elim_Simple_Redundant();
	  soe->Remove_Last_Le(num_con);
	  soe->Add_Work_Le();

	  // make sure the soe is in the right format
	  // each dimension i has its constraints at 2*i and 2*i+1
	  INT * sort_criteria =  CXX_NEW_ARRAY(INT, soe->Num_Le_Constraints(), &LNO_local_pool);
	  for (INT i = 0; i < soe->Num_Le_Constraints(); ++i) {
	    BOOL var = -1;
	    for (INT j = 0; j < a._dim; ++j) {
	      if (soe->Work(i,j) != 0) {
		var = j;
		break;
	      }
	    }

	    FmtAssert(var != -1, (("region is not bounded")));
	    if (soe->Work(i,var) > 0) {
	      sort_criteria[i] = 2*var+1;
	    } else {
	      sort_criteria[i] = 2*var;
	    }
	  }
	  soe->Sort_Le(sort_criteria, FALSE);
	  soe->Copy_To_Work();


	  // Set the region according to SOE solution
	  result->Set_Region(soe, syms, non_const_loops, strides);
	  CXX_DELETE(syms, &LNO_local_pool);
	  CXX_DELETE_ARRAY(strides, &LNO_local_pool);
	  CXX_DELETE(soe,&LNO_local_pool);
	  CXX_DELETE_ARRAY(sort_criteria, &LNO_local_pool);
	  goto return_point;
	}

      } else if (soe->Is_Consistent_Work()) {
	// Now the expensive test to figure out the coupled regions
	// First, make the constraints in the work array be the new soe.

	// recopy the constraints -- added by nawaaz
	soe->Copy_To_Work();
	soe->Elim_Simple_Redundant();
	soe->Remove_Last_Le(num_con);
	soe->Add_Work_Le();

	// TODO: There is a more efficient way to incrementally add the
	// rows of work to the soe such that only the suspected redundant
	// rows will need to be tested for redundancy.
	BOOL *is_redundant = 
	  CXX_NEW_ARRAY(BOOL, soe->Num_Le_Constraints(), &LNO_local_pool);
	soe->Mark_Redundant(is_redundant);
      
	INT limit = soe->Num_Le_Constraints();
	for (INT i = 0; i < limit; ++i) {
	  if (is_redundant[i]) soe->Remove_Le_Number(i);
	}


	if (soe->Num_Le_Constraints() == num_con) {
	  // Copy to work because region is read from the work array
	  soe->Copy_To_Work();

	  // Read back the costraints
	  result->Set_Region(soe, syms, non_const_loops, strides);
	  CXX_DELETE_ARRAY(strides, &LNO_local_pool);
	  CXX_DELETE(syms, &LNO_local_pool);
	  CXX_DELETE(soe, &LNO_local_pool);
	  goto return_point;
	}
      }
    }

    // If it falls through the crack, we don't know the precise intersection,
    // return an under-estimation.
    CXX_DELETE_ARRAY(strides, &LNO_local_pool);
    CXX_DELETE(result, &ARA_memory_pool);
    CXX_DELETE(syms, &LNO_local_pool);
    CXX_DELETE(soe, &LNO_local_pool);
    result = NULL;
    goto return_point;
  }

 return_point:

  MEM_POOL_Pop(&LNO_local_pool);
  return result;

}

BOOL
REGION::Is_Included(const REGION &a, const ARA_LOOP_INFO &ara_info)
{
  INT res = Region_Compare(*this, a, ara_info);
  return (res == 2 || res == 3);
}

BOOL
REGION::Contains(WN * array_wn)
{

  for (INT i=0; i<_wn_list.Elements(); ++i) {
    WN *cur = _wn_list.Bottom_nth(i);
    if (array_wn == _wn_list.Bottom_nth(i)) return TRUE;
  }
  return FALSE;

}
  
//=============================================================================
// This routine determine the relationship between two regions:
//       0 ---- Not comparable
//       1 ---- a <= b
//       2 ---- b <= a
//       3 ---- a == b
//=============================================================================
INT
Region_Compare(const REGION &a, const REGION &b, const ARA_LOOP_INFO &ara_info)
{

  // First, the simple tests.
  if (a._type==b._type) {
    if (a._type==ARA_TOP || a._type == ARA_BOTTOM)
      return 3;
    else if (a._type == ARA_TOO_MESSY)
      return 0;
  }
  
  if (b._type == ARA_TOP) return 1;
  if (a._type == ARA_TOP) return 2;
  if (a._type == ARA_TOO_MESSY || b._type == ARA_TOO_MESSY) return 0;

  // This can happen as the result of FORTRAN90 reshape (e.g. see PV 657151).
  if (a._dim != b._dim)
    return 0;


  INT result = 0;
  // Now the offset on the equations should be the same
  // TODO: Add a method to access_vector.h that compares
  // only the non-kernel part of the vector.
  INT i;
  for (i = 0; i < a._dim; ++i) {
    if (a._axle[i].up == NULL && b._axle[i].up == NULL &&
	!(Equivalent(*a._axle[i].lo, *b._axle[i].lo, a._dim)))
      return 0;
  }
  
  MEM_POOL_Push(&LNO_local_pool);
  {
    // Build the SOE to find the relations.
    SYSTEM_OF_EQUATIONS *soe = CXX_NEW(SYSTEM_OF_EQUATIONS(0,0,a._dim+a._depth,&LNO_local_pool), &LNO_local_pool);
    SYMBOL_LIST *syms = CXX_NEW(SYMBOL_LIST, &LNO_local_pool);
    INT_ST non_const_loops(&LNO_local_pool);

    // An array to hold the relation of each axle of 'a' and 'b'
    // relations[i] = 3  a._axle[i] == b._axle[i]
    //              = 1  a._axle[i] <  b._axle[i]
    //              = 2  a._axle[i] >  b._axle[i]
    //              = 0  relation unknown

    for (i = 0; i < a._dim; ++i) {
      Add_To_SOE(a, i, soe, syms, non_const_loops, TRUE, ara_info);
      Add_To_SOE(b, i, soe, syms, non_const_loops, TRUE, ara_info);
    }

    if (!soe->Copy_To_Work()) goto return_point;
    INT16 * lower = CXX_NEW_ARRAY(INT16, a._dim, &LNO_local_pool);
    INT16 * upper = CXX_NEW_ARRAY(INT16, a._dim, &LNO_local_pool);

    for (i = 0; i < a._dim; ++i) {
      lower[i] = soe->Simple_Redundant(4*i, 4*i+2);
      upper[i] = soe->Simple_Redundant(4*i+1, 4*i+3);
    }

    // First test if they are all the same
    for (i = 0; i < a._dim; ++i) {
      if (lower[i]!=3 || upper[i]!=3) 
	break;
    }

    if (i==a._dim) {
      result = 3;
      goto return_point;
    }

    BOOL exists_1 = FALSE;
    BOOL exists_2 = FALSE;

    for (; i < a._dim; ++i) {
      exists_1 = (exists_1 || lower[i]==1 || upper[i]==1);
      exists_2 = (exists_2 || lower[i]==2 || upper[i]==2);
      if (exists_1 && exists_2) {
	result = 0;
	goto return_point;
      }
    }
    
    // Now, they are some undecided axles.  Try to use the more powerful
    // (expensive) redundancy identification technique.
    BOOL redundant_1 = FALSE;
    BOOL redundant_2 = FALSE;

    if (!exists_2) redundant_1 = soe->Prove_Redundant(0, a._dim);
    if (redundant_1 && exists_1) {
      result = 2;
      goto return_point;
    }

    if (!exists_1) redundant_2 = soe->Prove_Redundant(1, a._dim);
    if (redundant_2 && exists_2) {
      result = 1;
      goto return_point;
    }

    if (redundant_1 && redundant_2) {
      result = 3;
      goto return_point;
    }

  }
  return_point: ;
    
  MEM_POOL_Pop(&LNO_local_pool);
  return result;

}

//============================================================================
// This is a simple approach to compute the union of convex regions.
// It returns the union of two regions if they are only different
// on one axle and the sections on that axle can be concatenated into a
// single convex region.
// May conservatively return NULL if cannot find a precise union that
// is also convex.
//============================================================================
REGION *
Region_Union(const REGION &a, const REGION &b, const ARA_LOOP_INFO &ara_info)
{
  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout,"Union two REGIONs \n");
    a.Print(stdout);
    b.Print(stdout);
  }

  // First, the simple tests.
  if (a._type==ARA_TOP) {
    REGION* result = CXX_NEW(REGION(a),&ARA_memory_pool);
    for (INT i = 0; i < b._wn_list.Elements(); ++i)
      result->_wn_list.Push(b._wn_list.Bottom_nth(i));
    return result;
  }

  if (b._type==ARA_TOP) {
    REGION* result = CXX_NEW(REGION(b),&ARA_memory_pool);
    for (INT i = 0; i < a._wn_list.Elements(); ++i)
      result->_wn_list.Push(a._wn_list.Bottom_nth(i));
    return result;
  }

  if (a._type==ARA_BOTTOM || b._type==ARA_TOO_MESSY) {
    REGION* result = CXX_NEW(REGION(b),&ARA_memory_pool);
    for (INT i = 0; i < a._wn_list.Elements(); ++i)
      result->_wn_list.Push(a._wn_list.Bottom_nth(i));
    return result;
  }
    
  if (b._type==ARA_BOTTOM || a._type==ARA_TOO_MESSY) {
    REGION* result = CXX_NEW(REGION(a),&ARA_memory_pool);
    for (INT i = 0; i < b._wn_list.Elements(); ++i)
      result->_wn_list.Push(b._wn_list.Bottom_nth(i));
    return result;
  }

  // dimension are not the same, hence we cannot create a region.
  // simply return NULL
  if (a._dim != b._dim)
    return NULL;

  Is_True(a._dim==b._dim,("Try to merge arrays with different dimensions"));

  // Now, find out if they only differ on one axle
  INT pos = -1;
  for (INT i = 0; i < a._dim; ++i) 
    if (!Equivalent(a._axle[i],b._axle[i],a._dim)) {
      if (pos>=0) return NULL;
      pos = i;
    }
  
  if (pos<0) {
    REGION* result = CXX_NEW(REGION(a),&ARA_memory_pool);
    for (INT i = 0; i < b._wn_list.Elements(); ++i)
      result->_wn_list.Push(b._wn_list.Bottom_nth(i));
    return result;
  }

  // Now, the regions only differ on axle 'pos'.
  // Try to merge the two sections into one section.
  AXLE_NODE & ax = a._axle[pos];
  AXLE_NODE & bx = b._axle[pos];

  if (ax.step != bx.step && ax.up != NULL && bx.up != NULL) return NULL;

  // Now, try to merge them
  INT step = MAX(ax.step, bx.step);
  REGION * result = NULL;

  MEM_POOL_Push(&LNO_local_pool);
  {

    SYSTEM_OF_EQUATIONS *soe = 
      CXX_NEW(SYSTEM_OF_EQUATIONS(0,0,a._dim+a._depth,&LNO_local_pool), &LNO_local_pool);
    SYMBOL_LIST *syms = CXX_NEW(SYMBOL_LIST, &LNO_local_pool);
    INT_ST non_const_loops(&LNO_local_pool);
     
    // Build the system of inequalities for axle 'pos'
    Add_To_SOE(a, pos, soe, syms, non_const_loops, TRUE, ara_info);
    Add_To_SOE(b, pos, soe, syms, non_const_loops, TRUE, ara_info);
     
    // Now, enlarge the upper bounds of 'ax' and 'bx' in the
    // SOE by its stride.
    // This will work only when the upper bound of 'ax' is always
    // reached, which is the case when the stride is 1. (For other
    // strides, we need to guard the loop to ensure it.)
    INT64 *ble = soe->Ble();
    ble[1] += step;
    ble[3] += step;

    // Get rid of the redundant constraints to find the
    // intersection.
    if (soe->Copy_To_Work()) {
      BOOL *is_redundant = 
	CXX_NEW_ARRAY(BOOL, soe->Num_Le_Constraints(), &LNO_local_pool);

      INT num_con = soe->Num_Le_Constraints();

      // The following is the first part of SOE::Elim_Simple_Redundant()
      num_con -= soe->Mark_Simple_Redundant(is_redundant);

      if (num_con > 2) {
	// SOE::Mark_New_Redundant() skips the inequalities that
	// are already marked as redundant in the parameter.
	num_con -= soe->Mark_New_Redundant(is_redundant);
      }
      
      // If we have only two constraints left, the redundant pairs
      // consist the union.
      if (num_con == 2) {
	//
	// TODO: handle cases where stride is not 1 using test or guard.
	// 
	if (soe->Is_Consistent() && ax.step == 1) {
	  if ((!is_redundant[0] && !is_redundant[2]) ||
	      (!is_redundant[1] && !is_redundant[3])) {
	    goto return_point;
	  }
	  
	  if (is_redundant[0]) {
	    result = CXX_NEW(REGION(a), &ARA_memory_pool);
	    result->_axle[pos].Set_Axle(ax.lo, (bx.up) ? bx.up : bx.lo, 
					step, a._dim);
	    if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
	      fprintf(stdout,"0 is redundant \n");
	      result->Print(stdout);
	    }

	  } else { // is_redundant[1]
	    result = CXX_NEW(REGION(a), &ARA_memory_pool);
	    result->_axle[pos].Set_Axle(bx.lo, (ax.up) ? ax.up : ax.lo, 
					step, a._dim);
	    if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
	      fprintf(stdout,"1 is redundant \n");
	      result->Print(stdout);
	    }
	  }
	
	  // Add the WNs of b
	  for (INT i = 0; i < b._wn_list.Elements(); ++i)
	    result->_wn_list.Push(b._wn_list.Bottom_nth(i));

	  goto return_point;
	}
      }
       
    }
  }
 return_point:
  MEM_POOL_Pop(&LNO_local_pool);

  return result;

}

//=============================================================================
//
// This function handles the region projection.  For uncoupled 
// subscript, the dimensions that are already ranges need not 
// be changed.
// For instance: 
//    A(1:I,I) after projection on 1<=I<=n becomes 
//             {A(1:d,d),1<=d<=n}
//
// For coupled subscript, the dimensions that are already ranges
// will have to be splitted if the loop index of the projection
// appears in the range expressions.
// For instance:
//    A(I:I+m,J) after projection on 1<=I<=n becomes 
//               {A(min(1,1+m):max(m+1,m+n),J)}
// and there might be some problems such as the stride of I etc.
// The key issue is that after the projection, the region may not be
// a convex set (there are holes in the set and the stride may not be
// regular).  
// On the other hand, if 'I' also occurs in another dimension
//    A(I:I+m,I) then the projection is the same as the first case:
//    {A(d:d+m,d),1<=d<=n}
// The key is that the access matrix cannot be singular.  If it is not,
// the integer lattice theory based on HNF comes to rescue.
//
//=============================================================================
REGION &
REGION::Region_Projection(const INT pos, const ARA_LOOP_INFO & ara_info)
{

  if (_type != ARA_NORMAL) return *this;
  
  // Check to see if it is independent of the loop index
  if (_kernel->Get_Independent_Loops()[pos]) 
    return *this;

  // Check if the image of the its kernel on the i'th
  // loop is already projected.

  if (_kernel->Projected_Level()>pos) {
    _kernel->Project(pos, ara_info);
  }

  if (_kernel->Region() && _kernel->Region()->Is_Too_Messy()) {
    this->Set_Too_Messy();
    _depth = pos;
    return *this;
  }

  // After the kernel is projected.  Copy the kernel region
  // and adjust with offset.
  for (INT i = 0; i < _dim; ++i) {
    AXLE_NODE & ax = _axle[i];
    if ((ax.lo->_ac_v && ax.lo->_ac_v->Loop_Coeff(pos)!=0) ||
	(ax.up && ax.up->_ac_v && ax.up->_ac_v->Loop_Coeff(pos)!=0)) {
      ax.Set_To_Kernel_Image(_kernel->Region()->Dim(i),_dim, _kernel->Get_Kernel()->Dim(i)->Const_Offset);
    }
  }
  _depth = pos;
 
  return *this;
}

//============================================================================
//
// Set according to the kernel image and keep the original offset
//
//============================================================================
void
AXLE_NODE::Set_To_Kernel_Image(const AXLE_NODE &a, const INT dim, const INT kernel_offset)
{

  if (a.up) {
    if (!up) {
      if (lo) 
	up = CXX_NEW(CON_PAIR(*lo, dim), &ARA_memory_pool);
      else
	up = CXX_NEW(CON_PAIR(), &ARA_memory_pool);
    }

    if (a.up->_ac_v) {

      if (!up->_ac_v) {
	up->_ac_v = CXX_NEW(ACCESS_VECTOR(a.up->_ac_v,&ARA_memory_pool), &ARA_memory_pool);
      } else {

	for (INT i = 0; i < up->_ac_v->Nest_Depth(); ++i) 
	  up->_ac_v->Set_Loop_Coeff(i,0);
	up->_ac_v->Const_Offset -= kernel_offset;
	ACCESS_VECTOR *old_av = up->_ac_v;
	old_av->Set_Nest_Depth(a.up->_ac_v->Nest_Depth());
	up->_ac_v = Merge(a.up->_ac_v, old_av, &ARA_memory_pool);
	CXX_DELETE(old_av, &ARA_memory_pool);

      }

      up->_ac_v->Set_Non_Const_Loops(a.up->_ac_v->Non_Const_Loops());

    }

    if (a.up->_coeff) {

      if (!up->_coeff) 
	up->_coeff = CXX_NEW_ARRAY(INT32, dim, &ARA_memory_pool);

      for (INT i = 0; i < dim; ++i)
	  up->_coeff[i] = a.up->_coeff[i];

    }

  }
    
  if (a.lo) {

    if (!lo) lo = CXX_NEW(CON_PAIR(), &ARA_memory_pool);

    if (a.lo->_ac_v) {

      if (!lo->_ac_v) {

	lo->_ac_v = 
	  CXX_NEW(ACCESS_VECTOR(a.lo->_ac_v,&ARA_memory_pool),&ARA_memory_pool);
      } else {

	for (INT i = 0; i < lo->_ac_v->Nest_Depth(); ++i) 
	  lo->_ac_v->Set_Loop_Coeff(i,0);
	lo->_ac_v->Const_Offset -= kernel_offset;
	ACCESS_VECTOR *old_av = lo->_ac_v;

	old_av->Set_Nest_Depth(a.lo->_ac_v->Nest_Depth());
	lo->_ac_v = Merge(a.lo->_ac_v, old_av, &ARA_memory_pool);
	CXX_DELETE(old_av, &ARA_memory_pool);

      }

      lo->_ac_v->Set_Non_Const_Loops(a.lo->_ac_v->Non_Const_Loops());

    }
    
    if (a.lo->_coeff) {

      if (!lo->_coeff) 
	lo->_coeff = CXX_NEW_ARRAY(INT32, dim, &ARA_memory_pool);

      for (INT i = 0; i < dim; ++i)
	  lo->_coeff[i] = a.lo->_coeff[i];

    }

  }
  
}    
    
//=============================================================================
//
// Here is where the real work of Region_Projection is
// Given a kernel, find its region after all the enclosing loops
// from 'level' up is projected.
//
//=============================================================================
void
KERNEL_IMAGE::Project(const INT level, const ARA_LOOP_INFO &ara_info)
{

  if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
    fprintf(stdout,"enters KERNEL_IMAGE::Project \n");
    if (_region) _region->Print(stdout);
  }
    
  // First test if 'level' is already projected in the region
  if (_projected_level <= level) return;
  _projected_level = level;
  
  MEM_POOL_Push(&LNO_local_pool);
  {
    // Build the system of equations for the access region of the kernel
    SYSTEM_OF_EQUATIONS *soe = 
      CXX_NEW(SYSTEM_OF_EQUATIONS(0,0,_kernel->Num_Vec()+_depth,&LNO_local_pool), &LNO_local_pool);
    SYMBOL_LIST *syms = CXX_NEW(SYMBOL_LIST, &LNO_local_pool);
    INT_ST non_const_loops(&LNO_local_pool);
    INT *strides = CXX_NEW_ARRAY(INT, _kernel->Num_Vec(), &LNO_local_pool);
    INT *which_axle = CXX_NEW_ARRAY(INT, _kernel->Num_Vec(), &LNO_local_pool);
    INT eq_count = 0;

    // Test if the region was ever projected.
    if (_region) {

      for (INT i = 0; i < _region->Num_Dim(); ++i) {
	Add_To_SOE(*_region, i, soe, syms, non_const_loops, FALSE, ara_info);
	strides[i] = _region->_axle[i].step;
	if (_region->_axle[i].up == NULL) {
	  which_axle[eq_count] = i;
	  eq_count++;
	}
      }
      
    } else {

      // if not, build the SOE from the access vector
      for (INT i = 0; i < _kernel->Num_Vec(); ++i) {
	Add_Access(_kernel->Dim(i), NULL, soe, syms, non_const_loops,
		   _depth, _kernel->Num_Vec(), i, ARA_EQN, ara_info, TRUE);
	strides[i] = 1;
	which_axle[i] = i;
      }
      _region = CXX_NEW(REGION(_depth,_kernel->Num_Vec()),&ARA_memory_pool);
      
    }

    // Now, set the lower bound and upper bound of the loop indices that
    // need to be projected.
    INT pivot_row;
    INT step;
    DOLOOP_STACK &  do_stack = ((ARA_LOOP_INFO &) ara_info).Do_Stack();
    for (INT i = do_stack.Elements() - 1; i >= level; --i) {
      DO_LOOP_INFO *dli = Get_Do_Loop_Info(do_stack.Bottom_nth(i));
      ACCESS_ARRAY *lb = dli->LB;
      ACCESS_ARRAY *ub = dli->UB;
      step = dli->Step->Const_Offset;
      
      if (lb->Num_Vec()>1 || ub->Num_Vec()>1 || !dli->Step->Is_Const()) {
	_region->Set_Too_Messy();

	goto return_point;
      }
    
      Add_Bound(lb->Dim(0), soe, syms, non_const_loops, _depth, _kernel->Num_Vec(),
	       ara_info);
      Add_Bound(ub->Dim(0), soe, syms, non_const_loops, _depth, _kernel->Num_Vec(),
	       ara_info);
      BOOL is_inconsistent = FALSE;


      if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
	fprintf(stdout,"Before base change, the SOE is: \n");
	soe->Print(stdout);
      }

      // This projection is different from the fourier elimination
      // in its handling of equations and coupled subscripts
      pivot_row = soe->Change_Base(_kernel->Num_Vec(), i, &LNO_local_pool);
    
      if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
	fprintf(stdout,"After base change, the SOE is: \n");
	soe->Print(stdout);
      }
      if (pivot_row < 0) { 
	_region->Set_Too_Messy();
      
	goto return_point;
      }
    }
  
    _region->Set_Region(soe, syms, non_const_loops, strides, pivot_row, level, step, which_axle[pivot_row]);
  
    if (Get_Trace(TP_LNOPT2,TT_LNO_ARA_DEBUG)) {
      fprintf(stdout,"exits KERNEL_IMAGE::Project \n");
      if (_region) _region->Print(stdout);
    }

  }

 return_point:
  MEM_POOL_Pop(&LNO_local_pool);

}

WN * 
REGION_UN::Any_Wn()
{

  Is_True(!Is_Empty(),("Try to get WN from an empty REGION_UN"));

  if (Is_Empty()) return NULL;

  REGION *cur = Head();
  if (cur->_wn_list.Elements()) 
    return cur->_wn_list.Bottom_nth(0);
  else
    return NULL;

}
  
BOOL
REGION_UN::Is_Included(const REGION &a, const ARA_LOOP_INFO &ara_info)
{

  REGION_ITER reg_iter(this);
  for (REGION *cur = reg_iter.First(); 
       !reg_iter.Is_Empty(); cur= reg_iter.Next()) 
    if (cur->Is_Included(a, ara_info)) return TRUE;
    
  return FALSE;

}

BOOL
REGION_UN::Is_Included(const REGION_UN &a, const ARA_LOOP_INFO &ara_info)
{
  REGION_CONST_ITER a_iter(&a);
  
  for (const REGION *a_cur = a_iter.First(); 
       !a_iter.Is_Empty(); a_cur = a_iter.Next()) {
    REGION_ITER my_iter(this);
    BOOL covered = FALSE;
    for (REGION *my_cur = my_iter.First(); 
	 !my_iter.Is_Empty(); my_cur = my_iter.Next()) {
      if (my_cur->Is_Included(*a_cur, ara_info)) {
	covered = TRUE;
	break;
      }

    }
    if (!covered) return FALSE;
  }

  return TRUE;

}

BOOL
REGION_UN::Contains(WN *array_wn)
{

  REGION_ITER iter(this);
  for (REGION *cur = iter.First(); !iter.Is_Empty(); cur = iter.Next())
    if (cur->Contains(array_wn)) return TRUE;
  
  return FALSE;

}

//============================================================================
//
//  Note that the region *a passed in maybe consumed, so the pointer 'a'
//  may point to NULL.
//
//============================================================================
REGION_UN &
REGION_UN::Add_Region(REGION *a, const ARA_LOOP_INFO &ara_info)
{
  
  REGION* add_reg = a;
  a = NULL;

  REGION_ITER reg_iter(this);
  REGION *prev = NULL;
  REGION *cur = reg_iter.First();

  while (!reg_iter.Is_Empty()) {

    REGION *new_reg = Region_Union(*cur, *add_reg, ara_info);
    if (new_reg==NULL) {
      prev = cur;
      cur = reg_iter.Next();
    } else {
      
      // Find an exact union.  
      // Remove the two regions generating the union and try to 
      // add the union to the list.
      CXX_DELETE(this->Remove(prev,cur),&ARA_memory_pool);
      CXX_DELETE(add_reg,&ARA_memory_pool);
      add_reg = new_reg;

      // Go to the first of the loop to see if it can be merged to a larger
      // region.  Not very efficient, later, we may define an order
      // of regions to avoid visiting the same node more than once.
      reg_iter.Init(this);
      cur = reg_iter.First();
      prev = NULL;
    }

  }

  this->Append(add_reg);
  return *this;

}

REGION_UN * 
RegionUN_Intersect(const REGION_UN &a, const REGION_UN &b, ARA_LOOP_INFO &ara_info)
{

  REGION_UN* result=CXX_NEW(REGION_UN(),&ARA_memory_pool);
  REGION_CONST_ITER a_iter(&a);
  REGION_CONST_ITER b_iter(&b);
  for (const REGION* a_cur = a_iter.First(); !a_iter.Is_Empty(); a_cur = a_iter.Next()) 
    for (const REGION *b_cur = b_iter.First(); !b_iter.Is_Empty(); b_cur = b_iter.Next()) {
      REGION* intersect = Region_Intersect(*a_cur,*b_cur,ara_info);
      if (intersect!=NULL) result->Add_Region(intersect, ara_info);
    }

  return result;

}

REGION_UN * 
RegionUN_Union(const REGION_UN &a, const REGION_UN &b, const ARA_LOOP_INFO &ara_info)
{

  REGION_UN* result = CXX_NEW(REGION_UN(),&ARA_memory_pool);
  REGION_CONST_ITER a_iter(&a);
  REGION_CONST_ITER b_iter(&b);

  for (const REGION* a_cur=a_iter.First();!a_iter.Is_Empty();a_cur=a_iter.Next()) 
    result->Add_Region(CXX_NEW(REGION(*a_cur),&ARA_memory_pool), ara_info);
  for (const REGION* b_cur=b_iter.First();!b_iter.Is_Empty();b_cur=b_iter.Next()) 
    result->Add_Region(CXX_NEW(REGION(*b_cur),&ARA_memory_pool), ara_info);

  return result;

}
 
BOOL
RegionUN_LE(const REGION_UN &a, const REGION_UN &b, const ARA_LOOP_INFO &ara_info)
{

  return ((REGION_UN &) b).Is_Included(a, ara_info);

}
    
BOOL
RegionUN_EQ(const REGION_UN &a, const REGION_UN &b, const ARA_LOOP_INFO &ara_info)
{

  return (RegionUN_LE(a,b,ara_info) && RegionUN_LE(b,a,ara_info));

}

REGION_UN &
REGION_UN::RegionUN_Projection(const INT depth, ARA_LOOP_INFO &ara_info)
{

  MEM_POOL_Push(&LNO_local_pool);
  {
    REGION_UN *tmp = CXX_NEW(REGION_UN(),&LNO_local_pool);
    REGION    *cur;
    while (!Is_Empty()) {
      cur = Remove_Headnode();
      cur->Region_Projection(depth, ara_info);
      tmp->Append(cur);
    }

    while (!tmp->Is_Empty()) {
      cur = tmp->Remove_Headnode();
      Add_Region(cur, ara_info);
    }
  }
  MEM_POOL_Pop(&LNO_local_pool);

  return *this;

}

BOOL 
REGION::Is_Loop_Invariant(WN *loop)
{
  if (WN_operator(loop) != OPR_DO_LOOP) return FALSE;
  INT loopno = Do_Loop_Depth(loop);
  
  if ( _type != ARA_NORMAL ) return TRUE;
  for (INT i = 0; i < Num_Dim(); i++) {
    AXLE_NODE &ax = Dim(i);
    if (ax.lo) {
      ACCESS_VECTOR *av = ax.lo->Access_Vector();
      if (av->Non_Const_Loops() > loopno) return FALSE;
      for (INT j = 0; j <= loopno; ++j) {
	if (av->Loop_Coeff(j) != 0) return FALSE;
      }
    }
    if (ax.up) {
      ACCESS_VECTOR *av = ax.up->Access_Vector();
      if (av->Non_Const_Loops() > loopno) return FALSE;
      for (INT j = 0; j <= loopno; ++j) {
	if (av->Loop_Coeff(j) != 0) return FALSE;
      }
    }
  }
  return TRUE;
}

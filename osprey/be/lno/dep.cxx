/*
 * Copyright (C) 2008 Advanced Micro Devices, Inc.  All Rights Reserved.
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


//-*-c++-*-
//                     Dependence Analysis Routines
//                     ----------------------------
//
// Description:
//
//     These are the basic routines to compute dependence information
//     between two array statements.  These are basically interface
//     routines that take access_arrays as input, use the SOEs to
//     compute the dependences and then output dependence vectors
//
//

/* ====================================================================
 * ====================================================================
 *
 * Module: dep.cxx  
 * $Revision: 1.6 $
 * $Date: 05/02/24 20:35:26-08:00 $
 * $Author: kannann@iridot.keyresearch $
 * $Revision: 1.6 $
 * $Date: 05/02/24 20:35:26-08:00 $
 * $Author: kannann@iridot.keyresearch $
 * $Source: be/lno/SCCS/s.dep.cxx $
 *
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Compute dependence info
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

const static char *source_file = __FILE__;
const static char *rcs_id = "$Source: be/lno/SCCS/s.dep.cxx $ $Revision: 1.6 $";

#include <sys/types.h>
#include "pu_info.h"
#include "soe.h"
#include "dep.h"
#include "lnoptimizer.h"
#include "lnopt_main.h"
#include "lwn_util.h"
#include "opt_alias_interface.h"
#include "opt_alias_mgr.h"
#include "opt_du.h"
#include "call_info.h"
#include "ipa_lno_util.h"
#ifdef KEY
#include "wn_simp.h"    // for WN_Simp_Compare_Trees
#endif

void DEPV_ARRAY::Print(FILE *fp) const
{
  for (INT i=0; i<_num_vec; i++) {
    DEPV_Print(Depv(i),fp,Num_Dim());
  }
  fprintf(fp,"\n");
}

extern DEPV_ARRAY *
  Create_DEPV_ARRAY(mUINT8 num_vec, mUINT8 num_dim, mUINT8 num_unused_dim,
		MEM_POOL *pool)
{
  // the size of the data is sizeof(DEPV) * num_vec*num_dim
  DEPV_ARRAY *result = (DEPV_ARRAY *) 
   	MEM_POOL_Alloc(pool,sizeof(DEPV_ARRAY)+
	(num_vec*num_dim-1)*sizeof(DEPV));
  FmtAssert(num_dim <= 15,
	("num_dim = %d is too large in Create_DEPV_ARRAY",num_dim));
  FmtAssert(num_unused_dim <= 15,
     ("num_unused_dim = %d is too large in Create_DEPV_ARRAY",num_unused_dim));
  result->_num_vec = num_vec;
  result->_dim = (num_unused_dim << 4) | num_dim;
  return(result);
}

extern DEPV_ARRAY *Create_DEPV_ARRAY(DEPV_LIST *depv_list, MEM_POOL *pool)
{
  MEM_POOL_Push(&LNO_local_pool);

  UINT8 num_dim = depv_list->Num_Dim();
  UINT8 num_unused_dim = depv_list->Num_Unused_Dim();

  UINT num_vec = depv_list->Len();
  if (num_vec > UINT8_MAX) {  // overflow, conservatively collapse the vector 
			      // into one
    DEPV *tmp =  DEPV_Create(&LNO_local_pool,num_dim);
    DEPV_ITER iter(depv_list);
    DEPV_NODE *node = iter.First();
    for (INT j=0; j<num_dim; j++) {
      DEPV_Dep(tmp,j) = DEPV_Dep(node->Depv,j);
    }
    if (iter.Is_Empty()) {
      return(NULL);
    }

    for (INT i=0; !iter.Is_Empty(); node=iter.Next(), i++) {
      DEPV *depv = node->Depv;
      for (INT j=0; j<num_dim; j++) {
        DEPV_Dep(tmp,j) = DEP_UnionDirection(DEPV_Dep(tmp,j),
		DEP_Direction(DEPV_Dep(depv,j)));
      }
    }

    DEPV_LIST *new_depvlist = CXX_NEW(DEPV_LIST(num_dim,num_unused_dim,
	&LNO_local_pool),&LNO_local_pool);
    new_depvlist->Append(tmp,num_unused_dim);

    // now we have a collapsed vector, but if the original vector
    // was lexicographically positive, we need to expand
    // this to remove lexicographically negative components 
    // i.e. given ((+,-) (=,+)) collapsing would give (+=,+-) which
    // contains (= -)

    if (depv_list->Is_Lexpos()) {
      DEPV_LIST *pos = CXX_NEW(DEPV_LIST(num_dim,num_unused_dim,
	&LNO_local_pool),&LNO_local_pool);
      DEPV_LIST *neg = CXX_NEW(DEPV_LIST(num_dim,num_unused_dim,
	&LNO_local_pool),&LNO_local_pool);
      new_depvlist->Lex_Pos_Decompose(&LNO_local_pool,pos,neg,
					depv_list->Contains_All_Equals(),0);
      
      depv_list = pos;
    } else {
      depv_list = new_depvlist;
    }
    num_vec = depv_list->Len();
    FmtAssert(num_vec <= UINT8_MAX,
      ("Impossible overflow in Create_DEPV_ARRAY"));
  }

  DEPV_ITER iter(depv_list);
  DEPV_NODE *node = iter.First();
  if (iter.Is_Empty()) {
    return(NULL);
  }


  DEPV_ARRAY *result = Create_DEPV_ARRAY(num_vec,num_dim,num_unused_dim,pool);
  for (INT i=0; !iter.Is_Empty(); node=iter.Next(), i++) {
    DEPV *list_depv = node->Depv;
    DEPV *array_depv = result->Depv(i);
    for (INT j=0; j<num_dim; j++) {
      DEPV_Dep(array_depv,j) = DEPV_Dep(list_depv,j);
    }
  }
  MEM_POOL_Pop(&LNO_local_pool);
  return(result);
}

extern DEPV_ARRAY *Create_DEPV_ARRAY(const DEPV_ARRAY *orig, MEM_POOL *pool)
{
  UINT8 num_vec = orig->Num_Vec();
  UINT8 num_dim = orig->Num_Dim();
  UINT8 num_unused_dim = orig->Num_Unused_Dim();
  DEPV_ARRAY *result = Create_DEPV_ARRAY(num_vec,num_dim,num_unused_dim,pool);
  for (INT i=0; i<num_vec; i++) {
    const DEPV *orig_depv = orig->Depv(i);
    DEPV *res_depv = result->Depv(i);
    for (INT j=0; j<num_dim; j++) {
      DEPV_Dep(res_depv,j) = DEPV_Dep(orig_depv,j);
    }
  }
  return(result);
}

extern void Delete_DEPV_ARRAY(DEPV_ARRAY *array, MEM_POOL *pool)
{
  MEM_POOL_FREE(pool,array);
}

// Union all the dependences in the array into one depv
DEPV *DEPV_ARRAY::Union(MEM_POOL *pool)
{
  DEPV *depv = CXX_NEW_ARRAY(DEPV,Num_Dim(),pool);
  // initialize to the first element in the array
  for (INT j=0; j<Num_Dim(); j++) {
    DEPV_Dep(depv,j) = DEPV_Dep(Depv(0),j);
  }

  // union in the rest
  for (INT i=1; i<Num_Vec(); i++) {
    DEPV *tmp = Depv(i);
    for (INT j=0; j<Num_Dim(); j++) {
      if (!DEP_IsDistance(DEPV_Dep(tmp,j)) || 
	  !DEP_IsDistance(DEPV_Dep(depv,j)) ||
	  DEP_Distance(DEPV_Dep(depv,j)) != DEP_Distance(DEPV_Dep(tmp,j))) {
	    DEPV_Dep(depv,j) = DEP_UnionDirection(DEPV_Dep(depv,j),
				DEP_Direction(DEPV_Dep(tmp,j)));
      }
    }
  }
  return depv;
}

// Return a DEPV_ARRAY with the inner dimensions chopped
DEPV_ARRAY *DEPV_ARRAY::Shorten(UINT num_dim,MEM_POOL *pool)
{
  FmtAssert (num_dim > 0, ("number dimensions should be at least 1."));
  if (Num_Dim() <= num_dim) { // already small enough
    return Create_DEPV_ARRAY(this,pool);
  } 
  DEPV_ARRAY *da = Create_DEPV_ARRAY(Num_Vec(),num_dim,Num_Unused_Dim(),pool);
  for (INT i=0; i<Num_Vec(); i++) {
    DEPV *oldd = Depv(i);
    DEPV *newd = da->Depv(i);
    for (INT j=0; j<num_dim; j++) {
      DEPV_Dep(newd,j) = DEPV_Dep(oldd,j);
    }
  }
  return da;
}


// Convert this array of one dimensional dependences into a DEP
DEP *DEPV_ARRAY::Shorten_To_Dep(MEM_POOL *pool)
{
  DEP *dep = NULL;
  for (INT i=0; i<Num_Vec(); i++) {
    DEPV *depv = Depv(i);
    BOOL good_vector = TRUE;  // all non-inner dims have equal in them
    for (INT j=0; j<Num_Dim()-1 && good_vector; j++) {
      DIRECTION dir = DEP_Direction(DEPV_Dep(depv,j));
      if ((dir != DIR_EQ) && (dir != DIR_POSEQ) && (dir != DIR_NEGEQ) &&
	  (dir != DIR_STAR)) {
	  good_vector = FALSE;
      }
    }
    if (good_vector) {
      if (!dep) {
	dep = CXX_NEW(DEP,pool);
	*dep = DEPV_Dep(depv,Num_Dim()-1);
      } else {
        DEP tmp = DEPV_Dep(depv,Num_Dim()-1);
        if (DEP_IsDistance(tmp)) {
          if (!DEP_IsDistance(*dep) || 
	      (DEP_Distance(*dep) != DEP_Distance(tmp))) {
	    *dep = DEP_UnionDirection(*dep,DEP_Direction(tmp));
          }
        } else {
          *dep = DEP_UnionDirection(*dep,DEP_Direction(tmp));
        }
      }
    }
  }
  return(dep);
}

INT DEPV_ARRAY::Max_Level() const
{
  INT result=0;
  INT num_dim = Num_Dim();
  for (INT i=0; i<Num_Vec(); i++) {
    const DEPV *depv = Depv(i);
    INT j=0;
    while (j<num_dim && ((DEP_Direction(DEPV_Dep(depv,j))==DIR_EQ)  ||
			(DEP_Direction(DEPV_Dep(depv,j))==DIR_POSEQ) ||
			(DEP_Direction(DEPV_Dep(depv,j))==DIR_NEGEQ) ||
			(DEP_Direction(DEPV_Dep(depv,j))==DIR_STAR))) { 
      j++;
    }
    INT current_level = j + Num_Unused_Dim();
    if (current_level > result) {
      result = current_level;
    }
  }
  return result;
}

/***********************************************************************
 *
 * Return true if all dependences upto (and including) depth are EQ,
 * false otherwise.
 *
 ***********************************************************************/
BOOL DEPV_ARRAY::Equal_Through_Depth (INT depth) {
  INT num_dim = Num_Dim();
  INT num_unused = Num_Unused_Dim();
  for (INT i=0; i<Num_Vec(); i++) {
    DEPV *depv = Depv(i);
    for (INT j=num_unused; j<=depth; j++) {
      if (DEP_Direction(DEPV_Dep(depv,j-num_unused)) != DIR_EQ) return FALSE;
    }
  }
  return TRUE;
}

BOOL DEPV_ARRAY::One_Equal_Through_Depth (INT depth) {
  INT num_dim = Num_Dim();
  INT num_unused = Num_Unused_Dim();
  for (INT i=0; i<Num_Vec(); i++) {
    DEPV *depv = Depv(i);
    INT j;
    for (j=num_unused; j<=depth; j++) {
      DIRECTION dir = DEP_Direction(DEPV_Dep(depv, j-num_unused));
      if (dir == DIR_POS || dir == DIR_NEG || dir == DIR_POSNEG) 
	break; 
    }
    if (j > depth) 
      return TRUE; 
  }
  return FALSE;
}

BOOL DEPV_ARRAY::Loop_Carrying_Dependence() {
  INT num_dim = Num_Dim();
  INT num_unused = Num_Unused_Dim();
  INT result = -1; 
  for (INT i=0; i<Num_Vec(); i++) {
    DEPV *depv = Depv(i);
    INT j;
    for (j=num_unused; j<num_unused+num_dim; j++) {
      DIRECTION dir = DEP_Direction(DEPV_Dep(depv, j-num_unused));
      if (dir == DIR_POS)
        break;
    }
    if (j == num_unused+num_dim)
      continue; 
    if (j > result)
      result = j; 
  }
  return result; 
}

BOOL DEPV_ARRAY::Is_Blockable(INT start_depth, 
			      INT stop_depth)
{ 
  INT num_dim = Num_Dim();
  INT num_unused = Num_Unused_Dim();
  for (INT i = 0; i < Num_Vec(); i++) {
    for (INT j = num_unused; j <= stop_depth; j++) { 
      DIRECTION dir = DEP_Direction(DEPV_Dep(Depv(i), j - num_unused)); 
      if (j < start_depth && dir == DIR_POS)
        return TRUE; 
      if (j >= start_depth && (dir == DIR_NEG || dir == DIR_POSNEG 
	  || dir == DIR_NEGEQ || dir == DIR_STAR))
	return FALSE; 
    } 
  }
  return TRUE; 
} 
    
void DEPV_NODE::Print(FILE *fp, mUINT8 num_dim) const
{
  DEPV_Print(Depv,fp,num_dim);
}

BOOL DEPV_NODE::Equal(DEPV_NODE *node2, mUINT8 num_dim) 
{
  DEPV *d1 = Depv;
  DEPV *d2 = node2->Depv;
  for (INT i=0; i<num_dim; i++) {
    if (d1[i] != d2[i]) {
      return FALSE;
    }
  }
  return TRUE;
}
  


DEPV_LIST::DEPV_LIST(WN *ref1, WN *ref2, mUINT8 common_nest, mUINT8 dv_dim,
	      BOOL use_bounds, MEM_POOL *pool, const DOLOOP_STACK *s1,
	      const DOLOOP_STACK *s2) 
{
  DEPV_COMPUTE compute;
  _pool = pool;
  _dv_dim = dv_dim;
  _num_unused_dim = common_nest-dv_dim;
  OPCODE opc1 = WN_opcode(ref1);
  OPCODE opc2 = WN_opcode(ref2);
  if (OPCODE_is_call(opc1) && Get_Call_Info(ref1) != NULL)
    Get_Call_Info(ref1)->Evaluate();
  if (ref1 != ref2 && OPCODE_is_call(opc2) && Get_Call_Info(ref2) != NULL)
    Get_Call_Info(ref2)->Evaluate();
  if (OPCODE_is_call(opc1)) {
    if (OPCODE_is_call(opc2)) { // call vrs call
      ARA_LOOP_INFO *ali1 = Get_Call_Info(ref1)->Call_Ara_Info();
      ARA_REF_ST &writes1 = ali1->MAY_DEF();
      INT i;
      for (i=0; i<writes1.Elements(); i++) {
        ARA_REF *write1 = writes1.Bottom_nth(i);
        ARA_LOOP_INFO *ali2 = Get_Call_Info(ref2)->Call_Ara_Info();

        ARA_REF_ST &writes2 = ali2->MAY_DEF();
        INT j;
        for (j=0; j<writes2.Elements(); j++) {
          ARA_REF *write2 = writes2.Bottom_nth(j);
          compute.Compute(this,ref1,write1,ref2,write2,common_nest,dv_dim,use_bounds,pool,s1,s2);
        }

        ARA_REF_ST &reads2 = ali2->USE();
        for (j=0; j<reads2.Elements(); j++) {
          ARA_REF *read2 = reads2.Bottom_nth(j);
          compute.Compute(this,ref1,write1,ref2,read2,common_nest,dv_dim,use_bounds,pool,s1,s2);
        }
      }

      ARA_REF_ST &reads1 = ali1->USE();
      for (i=0; i<reads1.Elements(); i++) {
        ARA_REF *read1 = reads1.Bottom_nth(i);
        ARA_LOOP_INFO *ali2 = Get_Call_Info(ref2)->Call_Ara_Info();

        ARA_REF_ST &writes2 = ali2->MAY_DEF();
        for (INT j=0; j<writes2.Elements(); j++) {
          ARA_REF *write2 = writes2.Bottom_nth(j);
          compute.Compute(this,ref1,read1,ref2,write2,common_nest,dv_dim,use_bounds,pool,s1,s2);
        }
      }
    } else {  // call vrs read/write 
      ARA_LOOP_INFO *ali = Get_Call_Info(ref1)->Call_Ara_Info();
      ARA_REF_ST &writes = ali->MAY_DEF();
      for (INT i=0; i<writes.Elements(); i++) {
        ARA_REF *write = writes.Bottom_nth(i);
        compute.Compute(this,ref1,write,ref2,NULL,common_nest,dv_dim,use_bounds,pool,s1,s2);
      } 
      if (OPCODE_is_store(opc2)) {
        ARA_REF_ST &reads = ali->USE();
        for (INT i=0; i<reads.Elements(); i++) {
          ARA_REF *read = reads.Bottom_nth(i);
          compute.Compute(this,ref1,read,ref2,NULL,common_nest,dv_dim,use_bounds,pool,s1,s2);
        } 
      }
    }
    Remove_Duplicates();
  } else if (OPCODE_is_call(opc2)) {  // read/write vrs call
    ARA_LOOP_INFO *ali = Get_Call_Info(ref2)->Call_Ara_Info();
    ARA_REF_ST &writes = ali->MAY_DEF();
    for (INT i=0; i<writes.Elements(); i++) {
      ARA_REF *write = writes.Bottom_nth(i);
      compute.Compute(this,ref1,NULL,ref2,write,common_nest,dv_dim,use_bounds,pool,s1,s2);
    } 
    if (OPCODE_is_store(opc1)) {
      ARA_REF_ST &reads = ali->USE();
      for (INT i=0; i<reads.Elements(); i++) {
        ARA_REF *read = reads.Bottom_nth(i);
        compute.Compute(this,ref1,NULL,ref2,read,common_nest,dv_dim,use_bounds,pool,s1,s2);
      } 
    }
    Remove_Duplicates();
  } else { // read/write vrs read/write
    compute.Compute(this,ref1,NULL,ref2,NULL,common_nest,dv_dim,use_bounds,pool,s1,s2);
  }
  if (OPCODE_is_call(opc1) && Get_Call_Info(ref1) != NULL)
    Get_Call_Info(ref1)->Unevaluate();
  if (ref1 != ref2 && OPCODE_is_call(opc2) && Get_Call_Info(ref2) != NULL)
    Get_Call_Info(ref2)->Unevaluate();
}




void DEPV_LIST::Print(FILE *fp) const
{
  DEPV_CONST_ITER iter(this);
  for (const DEPV_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
      node->Print(fp,Num_Dim());
  }
}

// Remove all duplicates from the DEPV_LIST
void DEPV_LIST::Remove_Duplicates()
{
  mUINT8 num_dim = Num_Dim();
  DEPV_ITER iter(this);
  for (DEPV_NODE *node=iter.First(); node; node = iter.Next()) {
    // loop through all possible later nodes, removing duplicates
    DEPV_ITER iter2(node);
    DEPV_NODE *first = iter2.First();
    first = iter2.Next(); // first is one after node
    DEPV_NODE *prev_node = node;
    DEPV_NODE *next_node = NULL;
    for (DEPV_NODE *node2=first; node2; node2 = next_node) {
      next_node = iter2.Next();
      if (node->Equal(node2,num_dim)) {
	MEM_POOL_Set_Default(_pool);
	CXX_DELETE(Remove(prev_node,node2),_pool);
      } else {
        prev_node = node2;
      }
    }
  }
}


DEPV_LIST::DEPV_LIST(DEPV_ARRAY *array, MEM_POOL *pool)
{
  _dv_dim = array->Num_Dim();
  _num_unused_dim = array->Num_Unused_Dim();
  _pool = pool;
  for (INT i=0; i<array->Num_Vec(); i++) {
    Append(CXX_NEW(DEPV_NODE(
	DEPV_Copy(pool,array->Depv(i),array->Num_Dim())),pool));
  }
}

// depv is a depv computed assuming that the outer dimensions are all equals
// now add in the outer dimensions, assuming worst case behavior
void DEPV_LIST::Append(DEPV *depv, INT num_bad_outer)
{
  if (!num_bad_outer) {
    if (depv) {
      Append(CXX_NEW(DEPV_NODE(DEPV_Copy(_pool,depv,_dv_dim)), _pool));
    }
    return;
  }

  for (INT i=0; i<num_bad_outer; i++) {
    DEPV *pos = DEPV_Create(_pool,_dv_dim);
    DEPV *neg = DEPV_Create(_pool,_dv_dim);
    INT j;
    for (j=0; j<i; j++) {
      DEPV_Dep(pos,j) = DEP_SetDirection(DIR_STAR);
      DEPV_Dep(neg,j) = DEP_SetDirection(DIR_STAR);
    }
    DEPV_Dep(pos,i) = DEP_SetDirection(DIR_POS);
    DEPV_Dep(neg,i) = DEP_SetDirection(DIR_NEG);
    for (j=i+1; j<_dv_dim; j++) {
      DEPV_Dep(pos,j) = DEP_SetDirection(DIR_STAR);
      DEPV_Dep(neg,j) = DEP_SetDirection(DIR_STAR);
    }
    Append(CXX_NEW(DEPV_NODE(pos),_pool));
    Append(CXX_NEW(DEPV_NODE(neg),_pool));
  }
  if (depv) {
    DEPV *eq = DEPV_Create(_pool,_dv_dim);
    INT j;
    for (j=0; j<num_bad_outer; j++) {
      DEPV_Dep(eq,j) = DEP_SetDirection(DIR_EQ);
    }
    for (j=num_bad_outer; j<_dv_dim; j++) {
      DEPV_Dep(eq,j) = DEPV_Dep(depv,j-num_bad_outer);
    }
    Append(CXX_NEW(DEPV_NODE(eq),_pool));
  }
}

void DEPV_LIST::Normalize_Step(INT64 *step)
{
  BOOL is_indep;
  for (INT i=0; i<Num_Dim(); i++) {
    if (step[i] != 1) {
      DEPV_ITER iter(this);
      DEPV_NODE *next_node=NULL;
      DEPV_NODE *prev_node = NULL;
      for (DEPV_NODE *node = iter.First(); node; node=next_node) {
	next_node = iter.Next();
	node->Normalize_Step(i,step[i],&is_indep);
	if (is_indep) {
          MEM_POOL_Set_Default(_pool);
	  CXX_DELETE(Remove(prev_node,node),_pool);
	} else {
	  prev_node = node;
        }
      }
    }
  }
}

void DEPV_NODE::Normalize_Step(INT dim, INT64 step, BOOL *is_indep)
{
  *is_indep = FALSE;

  if (step == 0) {  // an unkown step
    // we don't know anything about the step
    // if the dep contains any non-zero dep, we must assume all non-zero deps
    DIRECTION dir = DEP_Direction(DEPV_Dep(Depv,dim));
    if ((dir == DIR_POS) || (dir == DIR_NEG) || (dir == DIR_POSNEG)){
      DEPV_Dep(Depv,dim) = DEP_SetDirection(DIR_POSNEG);
    } else if ((dir == DIR_POSEQ) || (dir == DIR_NEGEQ) || (dir == DIR_STAR)) {
      DEPV_Dep(Depv,dim) = DEP_SetDirection(DIR_STAR);
    } else { // a DIR_EQ
      ;
    }
    return;
  }

  if (DEP_IsDistance(DEPV_Dep(Depv,dim))) {
    INT dist = DEP_Distance(DEPV_Dep(Depv,dim));
    if ((dist % step) != 0) {  // Can't normalize but isn't necessarily 
				// independent
      DIRECTION dir = DEP_Direction(DEPV_Dep(Depv,dim));
      if ((dir == DIR_POS) || (dir == DIR_NEG) || (dir == DIR_POSNEG)){
        DEPV_Dep(Depv,dim) = DEP_SetDirection(DIR_POSNEG);
      } else if ((dir == DIR_POSEQ) || (dir == DIR_NEGEQ) || 
		 (dir == DIR_STAR)) {
        DEPV_Dep(Depv,dim) = DEP_SetDirection(DIR_STAR);
      } else { // a DIR_EQ
        ;
      }
      return;
    } else {
      DEPV_Dep(Depv,dim) = DEP_SetDistance(dist / step);
    }
  } else if (step > 0) {
    return;
  } else {
    DEPV_Dep(Depv,dim) = DEP_Negate(DEPV_Dep(Depv,dim));
  }
}

static INT Max_Level(DEPV_LIST *dl) 
{
  INT result=0;
  INT num_dim = dl->Num_Dim();
  DEPV_CONST_ITER iter(dl);
  for (const DEPV_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
    const DEPV *depv = node->Depv;
    INT j=0;
    while (j<num_dim && ((DEP_Direction(DEPV_Dep(depv,j))==DIR_EQ)  ||
			(DEP_Direction(DEPV_Dep(depv,j))==DIR_POSEQ) ||
			(DEP_Direction(DEPV_Dep(depv,j))==DIR_NEGEQ) ||
			(DEP_Direction(DEPV_Dep(depv,j))==DIR_STAR))) { 
      j++;
    }
    INT current_level = j + dl->Num_Unused_Dim();
    if (current_level > result) {
      result = current_level;
    }
  }
  return result;
}

// Is there a dependence carried by the inner loop that is a
// single distance
BOOL DEPV_LIST::Is_Inner_Single_Distance()
{
  if (Max_Level(this) < Num_Dim()-1) return FALSE;
  BOOL seen_distance = FALSE;
  INT dist;
  DEPV_CONST_ITER iter(this);
  for (const DEPV_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
    DEP dep = DEPV_Dep(node->Depv,Num_Dim()-1);
    if (!DEP_IsDistance(dep)) {
      return FALSE;
    } else {
      if (seen_distance) {
	if (DEP_Distance(dep) != dist) {
	  return FALSE;
        } 
      } else {
	dist = DEP_Distance(dep);
	seen_distance = TRUE;
      }
    }
  }
  return seen_distance;
}

BOOL DEPV_LIST::Is_Inner_Non_Zero_Single_Distance()
{
  if (Max_Level(this) < Num_Dim()-1) return FALSE;
  BOOL seen_distance = FALSE;
  INT dist;
  DEPV_CONST_ITER iter(this);
  for (const DEPV_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
    DEP dep = DEPV_Dep(node->Depv,Num_Dim()-1);
    if (!DEP_IsDistance(dep) || (DEP_Distance(dep) == 0)) {
      return FALSE;
    } else {
      if (seen_distance) {
	if (DEP_Distance(dep) != dist) {
	  return FALSE;
        } 
      } else {
	dist = DEP_Distance(dep);
	seen_distance = TRUE;
      }
    }
  }
  return seen_distance;
}

// Get rid of dependences of the form (=,=,.....,!=)
// Note this does not get rid of (+=,=,...,!=)
void DEPV_LIST::Eliminate_Inner_Carried()
{
  DEPV_ITER iter(this);
  DEPV_NODE *next_node=NULL;
  DEPV_NODE *prev_node = NULL;
  for (DEPV_NODE *node = iter.First(); node; node=next_node) {
    next_node = iter.Next();
    BOOL done = FALSE;
    INT i;
    for (i=0; !done && i<Num_Dim()-1; i++) {
      DIRECTION dir = DEP_Direction(DEPV_Dep(node->Depv,i));
      if (dir != DIR_EQ) {
	done = TRUE;
      }
    }
    if (!done) {
      DIRECTION dir = DEP_Direction(DEPV_Dep(node->Depv,Num_Dim()-1));
      if ((dir == DIR_POS) || (dir == DIR_NEG) ||
         (dir == DIR_POSNEG)) {
        MEM_POOL_Set_Default(_pool);
        CXX_DELETE(Remove(prev_node,node),_pool);
      } else {
        DEPV_Dep(node->Depv,i) = DEP_SetDistance(0);
        prev_node = node;
      }
    } else {
      prev_node = node;
    }
  }
}

// Get rid of dependences of the form (=,=,.....,*)
// Note this does not get rid of (+=,=,...,*)
void DEPV_LIST::Eliminate_Inner_Carried_Or_All_Equals()
{
  DEPV_ITER iter(this);
  DEPV_NODE *next_node=NULL;
  DEPV_NODE *prev_node = NULL;
  for (DEPV_NODE *node = iter.First(); node; node=next_node) {
    next_node = iter.Next();
    BOOL done = FALSE;
    for (INT i=0; !done && i<Num_Dim()-1; i++) {
      DIRECTION dir = DEP_Direction(DEPV_Dep(node->Depv,i));
      if (dir != DIR_EQ) {
	done = TRUE;
      }
    }
    if (!done) {
      MEM_POOL_Set_Default(_pool);
      CXX_DELETE(Remove(prev_node,node),_pool);
    } else {
      prev_node = node;
    }
  }
}

// Eliminate non-distance dependences caried by 'i'
void DEPV_LIST::Eliminate_Non_Distance_Carried_By(INT i)
{
  // Step 1. Expand out the compound dependences
  DEPV_ITER iter(this);
  DEPV_NODE* node = 0;
  for (node = iter.First(); node; node=iter.Next()) {
    for (INT j=0; j<=i; j++) {
      DIRECTION dir = DEP_Direction(DEPV_Dep(node->Depv,j));
      if (dir == DIR_POSEQ) {
	DEPV *new_depv = DEPV_Copy(_pool,node->Depv,_dv_dim);
	DEPV_Dep(new_depv,j) = DEP_SetDirection(DIR_EQ);
	DEPV_Dep(node->Depv,j) = DEP_SetDirection(DIR_POS);
        Append(CXX_NEW(DEPV_NODE(new_depv),_pool));
      } else if (dir == DIR_NEGEQ) {
	DEPV *new_depv = DEPV_Copy(_pool,node->Depv,_dv_dim);
	DEPV_Dep(new_depv,j) = DEP_SetDirection(DIR_EQ);
	DEPV_Dep(node->Depv,j) = DEP_SetDirection(DIR_NEG);
        Append(CXX_NEW(DEPV_NODE(new_depv),_pool));
      } else if (dir == DIR_POSNEG) {
	DEPV *new_depv = DEPV_Copy(_pool,node->Depv,_dv_dim);
	DEPV_Dep(new_depv,j) = DEP_SetDirection(DIR_POS);
	DEPV_Dep(node->Depv,j) = DEP_SetDirection(DIR_NEG);
        Append(CXX_NEW(DEPV_NODE(new_depv),_pool));
      } else if (dir == DIR_STAR) {
	DEPV *new_depv = DEPV_Copy(_pool,node->Depv,_dv_dim);
	DEPV *new_depv2 = DEPV_Copy(_pool,node->Depv,_dv_dim);
	DEPV_Dep(new_depv,j) = DEP_SetDirection(DIR_POS);
	DEPV_Dep(new_depv2,j) = DEP_SetDirection(DIR_EQ);
	DEPV_Dep(node->Depv,j) = DEP_SetDirection(DIR_NEG);
        Append(CXX_NEW(DEPV_NODE(new_depv),_pool));
        Append(CXX_NEW(DEPV_NODE(new_depv2),_pool));
      }
    }
  }

  // Now get rid of the relevent ones
  DEPV_ITER iter2(this);
  DEPV_NODE *next_node=NULL;
  DEPV_NODE *prev_node = NULL;
  for (node = iter2.First(); node; node=next_node) {
    next_node = iter2.Next();
    BOOL carried_earlier = FALSE;
    for (INT j=0; j<i && !carried_earlier; j++) {
      DIRECTION dir = DEP_Direction(DEPV_Dep(node->Depv,j));
      if (dir != DIR_EQ) {
	carried_earlier = TRUE;
      }
    }
    DEP dep = DEPV_Dep(node->Depv,i);
    if (!carried_earlier && !DEP_IsDistance(dep) &&
	 (DEP_Direction(dep) != DIR_EQ)) {
      MEM_POOL_Set_Default(_pool);
      CXX_DELETE(Remove(prev_node,node),_pool);
    } else {
      prev_node = node;
    }
  }
}



// delete a list, including every node on it
// this assumes that all elements are from the same mempool
DEPV_LIST::~DEPV_LIST()
{
  MEM_POOL_Set_Default(_pool);
  while (!Is_Empty()) CXX_DELETE(Remove_Headnode(),Default_Mem_Pool);
}

// Find the lexicographic decomposition of this and of -this
void DEPV_LIST::Lex_Pos_Decompose(MEM_POOL *pool, DEPV_LIST *pos, 
      DEPV_LIST *neg, BOOL keep_pos_equals, BOOL keep_neg_equals) 
{
  Is_True(pos->Num_Dim() == Num_Dim(),
      ("Bad pos in DEPV_LIST::Lex_Pos_Decompose"));
  Is_True(neg->Num_Dim() == Num_Dim(),
      ("Bad neg in DEPV_LIST::Lex_Neg_Decompose"));
  DEPV_ITER iter(this);
  for (DEPV_NODE *node = iter.First(); !iter.Is_Empty(); 
      node=iter.Next()) {
      node->Lex_Pos_Decompose(pool,pos,neg,_dv_dim,0,keep_pos_equals,
				keep_neg_equals);
  }
}

void DEPV_LIST::Blockable_Part(MEM_POOL *pool, 
                               DEPV_LIST* dl_block,
                               mUINT8 num_unused_dim,
                               mUINT8 dv_dim,
                               INT start_depth,
                               INT stop_depth)
{
  DEPV_ITER iter(this);
  DEPV_NODE* node = NULL; 
  for (node = iter.First(); !iter.Is_Empty(); node=iter.Next()) 
    node->Blockable_Part(pool, dl_block, num_unused_dim, dv_dim, 
      start_depth, stop_depth);  
}

// Convert this list of one dimensional dependences into a DEP
DEP DEPV_LIST::Convert_To_Dep()
{
  Is_True(Num_Dim() == 1,
      ("Num_Dim() is not 1 in DEPV_LIST::Convert_To_Dep"));
  DEPV_ITER iter(this);
  DEPV_NODE *node = iter.First();
  DEP dep = DEPV_Dep(node->Depv,0);
  for (node = iter.Next(); !iter.Is_Empty(); node=iter.Next()) {
    DEP tmp = DEPV_Dep(node->Depv,0);
    if (DEP_IsDistance(tmp)) {
      if (!DEP_IsDistance(dep) || (DEP_Distance(dep) != DEP_Distance(tmp))) {
	dep = DEP_UnionDirection(dep,DEP_Direction(tmp));
      }
    } else {
      dep = DEP_UnionDirection(dep,DEP_Direction(tmp));
    }
  }
  return(dep);
}

void DEPV_NODE::Blockable_Part(MEM_POOL* pool, 
                               DEPV_LIST* dl_block,   
                               mUINT8 num_unused_dim, 
                               mUINT8 dv_dim, 
                               INT start_depth, 
                               INT stop_depth) 
{
  if (start_depth > stop_depth) {
    DEPV_NODE* dvn = CXX_NEW(DEPV_NODE(DEPV_Copy(pool, Depv, dv_dim)), pool);
    dl_block->Append(dvn); 
    return; 
  } 
  INT start_dim = start_depth - num_unused_dim; 
  DEP orig = DEPV_Dep(Depv, start_dim);
  switch (DEP_Direction(DEPV_Dep(Depv, start_dim))) { 
  case DIR_POS: 
  case DIR_POSEQ:
  case DIR_EQ:
    Blockable_Part(pool, dl_block, num_unused_dim, dv_dim, start_depth + 1,
      stop_depth);
    break;
  case DIR_POSNEG: 
    DEPV_Dep(Depv, start_dim) = DEP_SetDirection(DIR_POS);
    Blockable_Part(pool, dl_block, num_unused_dim, dv_dim, start_depth + 1,
      stop_depth);
    break;
  case DIR_NEGEQ:
    DEPV_Dep(Depv, start_dim) = DEP_SetDirection(DIR_EQ);
    Blockable_Part(pool, dl_block, num_unused_dim, dv_dim, start_depth + 1,
      stop_depth);
    break;
  case DIR_STAR: 
    DEPV_Dep(Depv, start_dim) = DEP_SetDirection(DIR_POS);
    Blockable_Part(pool, dl_block, num_unused_dim, dv_dim, start_depth + 1,
      stop_depth);
    DEPV_Dep(Depv, start_dim) = DEP_SetDirection(DIR_EQ);
    Blockable_Part(pool, dl_block, num_unused_dim, dv_dim, start_depth + 1,
      stop_depth);
    break;                                                  
  } 
  DEPV_Dep(Depv, start_dim) = orig;
} 

// Decompose one vector
// All dimensions before first_dim are DIR_EQ
void DEPV_NODE::Lex_Pos_Decompose(MEM_POOL *pool, DEPV_LIST *pos, 
	DEPV_LIST *neg, mUINT8 dv_dim,INT first_dim, BOOL keep_pos_equals,
	BOOL keep_neg_equals) 
{
  if (first_dim == dv_dim) {  // an all equals
    if (keep_pos_equals) {  
      pos->Append(CXX_NEW(DEPV_NODE(DEPV_Copy(pool,Depv,dv_dim)),pool));
    }
    if (keep_neg_equals) {  
      neg->Append(CXX_NEW(DEPV_NODE(DEPV_Copy(pool,Depv,dv_dim)),pool));
    }
    return;
  }

  DEP orig = DEPV_Dep(Depv,first_dim);
  if (DEP_Direction(DEPV_Dep(Depv,first_dim)) == DIR_POS) {

    pos->Append(CXX_NEW(DEPV_NODE(DEPV_Copy(pool,Depv,dv_dim)),pool));

  } else if (DEP_Direction(DEPV_Dep(Depv,first_dim)) == DIR_NEG) {

    DEPV_NODE *n = CXX_NEW(DEPV_NODE(DEPV_Copy(pool,Depv,dv_dim)),pool);
    for (INT i=first_dim; i<dv_dim; i++) {
      DEPV_Dep(n->Depv,i) = DEP_Negate(DEPV_Dep(Depv,i));
    }
    neg->Append(n);

  } else if (DEP_Direction(DEPV_Dep(Depv,first_dim)) == DIR_POSNEG) {

    DEPV_Dep(Depv,first_dim) = DEP_SetDirection(DIR_POS);
    pos->Append(CXX_NEW(DEPV_NODE(DEPV_Copy(pool,Depv,dv_dim)),pool));

    DEPV_NODE *n = CXX_NEW(DEPV_NODE(DEPV_Copy(pool,Depv,dv_dim)),pool);
    DEPV_Dep(n->Depv,first_dim) = DEP_SetDirection(DIR_POS);
    for (INT i=first_dim+1; i<dv_dim; i++) {
      DEPV_Dep(n->Depv,i) = DEP_Negate(DEPV_Dep(Depv,i));
    }
    neg->Append(n);

  } else if (DEP_Direction(DEPV_Dep(Depv,first_dim)) == DIR_POSEQ) {

    DEPV_Dep(Depv,first_dim) = DEP_SetDirection(DIR_POS);
    pos->Append(CXX_NEW(DEPV_NODE(DEPV_Copy(pool,Depv,dv_dim)),pool));
    DEPV_Dep(Depv,first_dim) = DEP_SetDirection(DIR_EQ);
    Lex_Pos_Decompose(pool,pos,neg,dv_dim,first_dim+1,keep_pos_equals,
							keep_neg_equals);

  } else if (DEP_Direction(DEPV_Dep(Depv,first_dim)) == DIR_NEGEQ) {

    DEPV_NODE *n = CXX_NEW(DEPV_NODE(DEPV_Copy(pool,Depv,dv_dim)),pool);
    DEPV_Dep(n->Depv,first_dim) = DEP_SetDirection(DIR_POS);
    for (INT i=first_dim+1; i<dv_dim; i++) {
      DEPV_Dep(n->Depv,i) = DEP_Negate(DEPV_Dep(Depv,i));
    }
    neg->Append(n);

    DEPV_Dep(Depv,first_dim) = DEP_SetDirection(DIR_EQ);
    Lex_Pos_Decompose(pool,pos,neg,dv_dim,first_dim+1,keep_pos_equals,
							keep_neg_equals);

  } else if (DEP_Direction(DEPV_Dep(Depv,first_dim)) == DIR_STAR) {

    DEPV_Dep(Depv,first_dim) = DEP_SetDirection(DIR_POS);
    pos->Append(CXX_NEW(DEPV_NODE(DEPV_Copy(pool,Depv,dv_dim)),pool));

    DEPV_NODE *n = CXX_NEW(DEPV_NODE(DEPV_Copy(pool,Depv,dv_dim)),pool);
    for (INT i=first_dim+1; i<dv_dim; i++) {
      DEPV_Dep(n->Depv,i) = DEP_Negate(DEPV_Dep(Depv,i));
    }
    neg->Append(n);

    DEPV_Dep(Depv,first_dim) = DEP_SetDirection(DIR_EQ);
    Lex_Pos_Decompose(pool,pos,neg,dv_dim,first_dim+1,keep_pos_equals,
							keep_neg_equals);

  } else {	// an DIR_EQ
    Lex_Pos_Decompose(pool,pos,neg,dv_dim,first_dim+1,keep_pos_equals,
							keep_neg_equals);
  }
  DEPV_Dep(Depv,first_dim)=orig;
}

// Reverse the effect of the decomposition
// This destroys pos and neg
extern DEPV_LIST *Lex_Pos_Compose(MEM_POOL *pool,DEPV_LIST *pos,DEPV_LIST *neg) 
{
  DEPV_LIST *result=CXX_NEW(DEPV_LIST(pos->Num_Dim(),pos->Num_Unused_Dim(),
				pool), pool);
  while (!pos->Is_Empty()) {
    DEPV_NODE *node = pos->Remove_Headnode();
    result->Append(node);
  }
  while (!neg->Is_Empty()) {
    DEPV_NODE *node = neg->Remove_Headnode();
    for (INT i=0; i<neg->Num_Dim(); i++) {
      DEPV_Dep(node->Depv,i) = DEP_Negate(DEPV_Dep(node->Depv,i));
    }
    result->Append(node);
  }
  return result;
}

BOOL Is_Lexpos(DEPV* dv, INT dims)
{
   for (INT i = 0; i < dims; i++) {
      switch (DEP_Direction(DEPV_Dep(dv,i))) {
	case DIR_EQ:
	case DIR_POSEQ:
	  continue;
	case DIR_POS:
	  return TRUE;
	case DIR_NEG:
	case DIR_NEGEQ:
	case DIR_POSNEG:
	case DIR_STAR:
	  return FALSE;
      }
    }
    return TRUE;
}

BOOL DEPV_LIST::Is_Lexpos() const
{
  DEPV_CONST_ITER iter(this);
  for (const DEPV_NODE *node = iter.First(); !iter.Is_Empty();
	    node=iter.Next()) {
    if (!::Is_Lexpos(node->Depv,_dv_dim)) {
      return FALSE;
    }
  }
  return TRUE;
}

BOOL DEPV_LIST::Contains_All_Equals() const
{
  DEPV_CONST_ITER iter(this);
  for (const DEPV_NODE *node = iter.First(); !iter.Is_Empty();
	    node=iter.Next()) {
    DEPV *depv = node->Depv;
    BOOL found_non_eq = FALSE;
    for (INT i=0; i<_dv_dim && !found_non_eq; i++)  {
      switch (DEP_Direction(DEPV_Dep(depv,i))) {
	case DIR_EQ:
	case DIR_POSEQ:
	case DIR_NEGEQ:
	case DIR_STAR:
	  continue;
	case DIR_POS:
	case DIR_NEG:
	case DIR_POSNEG:
	  found_non_eq = TRUE;
      }
    }
    if (!found_non_eq) return TRUE;
  }
  return FALSE;
}

static BOOL Is_All_Equals(DEPV *depv, INT dv_dim);
static INT debug;

// search for symbol on the stack, if it's not their enter it
// return its position
// if symbol is null, always add a new NULL symbol
static INT Find_Enter_Symbol(SYMBOL_STACK *stack, const SYMBOL *symbol)
{
  if (!symbol) {
    INT i = stack->Elements();
    stack->Push(NULL);
    return i;
  }
  INT i;
  for (i=0; i<stack->Elements(); i++) {
    if (stack->Bottom_nth(i)) {
      if (*stack->Bottom_nth(i) == *symbol) return i;
    } 
  }
  stack->Push(symbol);
  return(i);
}



// Compute the dependence info,
// Append the result in result
// This routine basically puts things in matrix form for use by SOE routines
// We use the following mappings of variables to colums
//  the nesting depth of the first reference is _nd1 and the second _nd2 
//  common_nest is the number of common enclosing loops
//  _dv_dim is the number of dimensions for which we're computing dvectors
//	This allows us to compute distances on only the inner loop, for example
//  Let nondv_dim = common_nest-dv_dim (common dims not used for dvectors)
//  Columns 0..nondv_dim-1 are for the outermost common loop vars of both refs
//  Columns nondv_dim..nd1-1 are for the remaining loop vars of ref 1
//  Columns nd1..nd1+nd2-nondv_dim-1 are for the remain loop vars of ref 2
//  Columns nd1+nd2-nondv_dim ... are for symbols symbol_stack[0]...
//  If ref1 is a call, ara_ref1 is the particular array_ref of the 
//		call that we're intereseted in
//  If ref2 is a call, array_ref2 is the particular array_ref of the 
//		call that we're intereseted in
void DEPV_COMPUTE::Compute(DEPV_LIST *result,WN *ref1, ARA_REF *ara_ref1,
		     WN *ref2, ARA_REF *ara_ref2,UINT8 common_nest, 
		     mUINT8 dv_dim, BOOL use_bounds, MEM_POOL *pool, 
		     const DOLOOP_STACK *s1, const DOLOOP_STACK *s2)
{
  Is_True(dv_dim > 0,
    ("Dv_dim = %d is too small in DEPV_COMPUTE::Compute\n",dv_dim));
  Is_True(dv_dim <= common_nest,
    ("Dv_dim = %d is too large in DEPV_COMPUTE::Compute\n",dv_dim));
  Is_True(s1,("s1 is NULL in DEPV_COMPUTE::Compute"));
  Is_True(s2,("s2 is NULL in DEPV_COMPUTE::Compute"));
  Is_True(pool != &LNO_local_pool,
	("Cannot use LNO_local_pool in DEPV_COMPUTE::Compute"));

  if (Get_Trace(TP_LNOPT,TT_LNO_DEP2)) {
    debug = 2;
  } else if (Get_Trace(TP_LNOPT,TT_LNO_DEP)) {
    debug = 1;
  } else {
    debug = 0;
  }
  if (debug >= 2) {
    fprintf(TFile,"Dependence analysis comparing two array references\n");
  }


  MEM_POOL_Push(&LNO_local_pool);
  _pool = pool;
  Set_Step(s1,s2);
  
  DEP_RESULT dr; 
  SYMBOL_STACK *symbol_stack = CXX_NEW(SYMBOL_STACK(&LNO_local_pool), 
	&LNO_local_pool); 
	
  // Look at the array bases, this is the scalar part of dependency analysis 
  dr = Base_Test(ref1,ara_ref1,ref2,ara_ref2); 
  if (dr == DEP_INDEPENDENT) {
    if (debug >= 2) {
      fprintf(TFile,"Base test proves them independent \n");
    }
    goto return_point;
  } else if (dr == DEP_DEPENDENT) {
    result->Append(CXX_NEW(DEPV_NODE(DEPV_CreateStar(pool,dv_dim)),pool));
    if (debug >= 2) {
      fprintf(TFile,"Base test forces us to assume dependence\n");
    }
    goto return_point;
  }

  {
   WN *array1=NULL,*array2=NULL;
   if (!ara_ref1) {
     array1=(OPCODE_is_load(WN_opcode(ref1))?(WN_kid0(ref1)):(WN_kid1(ref1)));
     if (WN_operator(array1) == OPR_ADD) {
       if (WN_operator(WN_kid0(array1)) == OPR_ARRAY) {
         array1 = WN_kid0(array1);
       } else {
         array1 = WN_kid1(array1);
       }
     }
   }

   if (!ara_ref2) {
     array2=(OPCODE_is_load(WN_opcode(ref2))?(WN_kid0(ref2)):(WN_kid1(ref2)));
     if (WN_operator(array2) == OPR_ADD) {
       if (WN_operator(WN_kid0(array2)) == OPR_ARRAY) {
         array2 = WN_kid0(array2);
       } else {
         array2 = WN_kid1(array2);
       }
     }
   }
 


   ACCESS_ARRAY *a1 = NULL;
   INT num_vec1,num_vec2;
   if (array1) {
     a1 = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array1);
     num_vec1 = a1->Num_Vec();
   } else {
     REGION_ITER iter(&ara_ref1->Image());
     num_vec1 = iter.First()->Num_Dim();
   }
   ACCESS_ARRAY *a2 = NULL;
   if (array2) {
     a2 = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array2);
     num_vec2 = a2->Num_Vec();
   } else {
     REGION_ITER iter(&ara_ref2->Image());
     num_vec2 = iter.First()->Num_Dim();
   }
   if (debug >= 2) {
     fprintf(TFile,"The two access arrays are \n");
     if (a1) a1->Print(TFile); if (a2) a2->Print(TFile); 
   }
 
   BOOL delin_ok = TRUE;
   if (!a1) { // a1 is a call
     if (a2) {
       for (INT i=0; i<num_vec2 && delin_ok ; i++) {
         ACCESS_VECTOR *av2 = a2->Dim(i);
         if (av2->Delinearized_Symbol) {
	   delin_ok = FALSE;  // can't compare a call to a delinearized array
         }
       }
     }
   } else if (!a2) {  // a2 is a call
     for (INT i=0; i<num_vec1 && delin_ok ; i++) {
       ACCESS_VECTOR *av1 = a1->Dim(i);
       if (av1->Delinearized_Symbol) {
	 delin_ok = FALSE;
       }
     }
   } else { // neither are calls
     delin_ok = (num_vec1 == num_vec2);
     for (INT i=0; i<num_vec1 && delin_ok ; i++) {
       ACCESS_VECTOR *av1 = a1->Dim(i);
       ACCESS_VECTOR *av2 = a2->Dim(i);
       if (av1->Delinearized_Symbol && av2->Delinearized_Symbol) {
         if (!(*av1->Delinearized_Symbol == *av2->Delinearized_Symbol)) {
	   delin_ok = FALSE;
         }
       } else if (av1->Delinearized_Symbol || av2->Delinearized_Symbol) {
         delin_ok = FALSE;
       }
     }
   }
   if (!delin_ok) {
     result->Append(CXX_NEW(DEPV_NODE( DEPV_CreateStar(pool,dv_dim)),pool));
     if (debug >= 2) {
       fprintf(TFile,"Dependent due to different delinearizations \n");
     }
     goto return_point;
   }




   BOOL *non_trivial_dim = CXX_NEW_ARRAY(BOOL,num_vec1,&LNO_local_pool);
 
   BOOL same_monotonic = FALSE; 
   if (a1 && a2) {
     same_monotonic=Same_Monotonic(array1,array2,a1,a2,common_nest,dv_dim,s1);
   }
 

   if (a1 && a2) {
     if (Simple_Gcd_Indep(a1,a2,common_nest,dv_dim)) {
       if (debug >= 2) {
         fprintf(TFile,"Proven independent by Simple_Gcd_Indep \n");
       }
       goto return_point;
     }
   }

   // how many outer loops have symbols that vary unpredictably
   // we compute dependences for the inner_more loops and then prepend
   // the results with !='s
   //
   // Note that if we have something like
   // a[n][i] where 'n' varies unpredictably, num_bad_outer is 0
   // we compute dependences for all the loops but throw out the
   // first dimension of 'a'
   INT num_bad_outer=INT32_MAX;
   if (a1) {
     num_bad_outer =  a1->Dim(0)->Non_Const_Loops();
     for (INT i=1; i<num_vec1; i++) {
       num_bad_outer = MIN(num_bad_outer,a1->Dim(i)->Non_Const_Loops());
     }
   } else {
     REGION_ITER iter(&ara_ref1->Image());
     const REGION *first = iter.First();
     for (const REGION *node=first; !iter.Is_Empty(); node = iter.Next()) {
       AXLE_NODE *axle = node->_axle;
       CON_PAIR *lo = axle->lo;
       FmtAssert(lo != NULL, ("Compute: axle->lo is NULL"));
       num_bad_outer = MIN(num_bad_outer,lo->_ac_v->Non_Const_Loops());
       CON_PAIR *up = axle->up;
       FmtAssert(up != NULL, ("Compute: axle->hi is NULL"));
       if (up) {
         num_bad_outer = MIN(num_bad_outer,up->_ac_v->Non_Const_Loops());
       }
     }
   }

   if (a2) {
     num_bad_outer =  a2->Dim(0)->Non_Const_Loops();
     for (INT i=1; i<num_vec2; i++) {
       num_bad_outer = MIN(num_bad_outer,a2->Dim(i)->Non_Const_Loops());
     }
   } else {
     REGION_ITER iter(&ara_ref2->Image());
     const REGION *first = iter.First();
     for (const REGION *node=first; !iter.Is_Empty(); node = iter.Next()) {
       AXLE_NODE *axle = node->_axle;
       CON_PAIR *lo = axle->lo;
       num_bad_outer = MIN(num_bad_outer,lo->_ac_v->Non_Const_Loops());
       CON_PAIR *up = axle->up;
       if (up) {
         num_bad_outer = MIN(num_bad_outer,up->_ac_v->Non_Const_Loops());
       }
     }
   }
 

   num_bad_outer -= (common_nest-dv_dim);
   num_bad_outer = MAX(num_bad_outer,0);

   if (num_bad_outer >= dv_dim) {
     result->Append(CXX_NEW(DEPV_NODE(
		 DEPV_CreateStar(pool,dv_dim)),pool));
     if (debug >= 2) {
       fprintf(TFile,"Shown dependent since no good dimensions\n");
     }
     goto return_point;
   }


   // Do some trivial tests (constants, too_messy)
   BOOL seen_non_trivial = FALSE;
   if (a1 && a2) {
     dr = Trivial_Test(a1,a2,dv_dim-num_bad_outer,common_nest,non_trivial_dim,
				 &seen_non_trivial);
   } else {
     seen_non_trivial = TRUE;
     dr = DEP_CONTINUE;
   }
   if (dr == DEP_INDEPENDENT) {
     if (debug >= 2) {
       fprintf(TFile,"Proven independent by trivial tests \n");
     }
     goto return_point;
   } else if ((dr == DEP_DEPENDENT)  && !same_monotonic) {
     result->Append(CXX_NEW(DEPV_NODE(
		 DEPV_CreateStar(pool,dv_dim)),pool));
     if (debug >= 2) {
       fprintf(TFile,"Shown dependent by trivial tests \n");
     }
     goto return_point;
   }
 
   if (same_monotonic) {
     if (array1 == array2) {
       if (debug >= 2) {
         fprintf(TFile,"Proven independent by monotonicity tests \n");
       }
     } else {
       result->Append(CXX_NEW(DEPV_NODE(
		 DEPV_CreateEqual(pool,dv_dim)),pool));
       if (debug >= 2) {
         fprintf(TFile,"Two equivalent monotonically changing refs, ");
	 fprintf(TFile,"all equals dependence. n");
       }
     }
     goto return_point;
   }


   // set up the column positions
   _nd1 = s1->Elements();
   _nd2 = s2->Elements();
   _first_dv1 = common_nest-(dv_dim-num_bad_outer);  // first distance vector
						     // var of ref 1
   _first_non_com1 = common_nest;  // first non common var of ref 1
   _first_dv2 = _nd1;		// first distnace vector var of ref2
   _first_non_com2 = _nd1+(dv_dim-num_bad_outer);  // first non common var 
						   // of ref 2
   _first_symbol = _nd1+_nd2-_first_dv1; // first symbolic variable
   _work_cols = _first_symbol;
   _work_le_rows = 0;
 
   // put the equality constraints (and potentially inequality if
   // there are calls) into the work array
   _work_eq_rows = 0;
   if (seen_non_trivial) {
     if (a1 && a2 ) {
       if (!Copy_Equals_To_Work(a1,a2,symbol_stack,non_trivial_dim)) {
         result->Append(CXX_NEW(DEPV_NODE(
	     DEPV_CreateStar(pool,dv_dim)),pool));
         if (debug >= 2) {
           fprintf(TFile,"Assume dependent because of overflow \n");
         }
         goto return_point;
       }
     } else {  // a call
       INT32 first_dummy;
       if (!Create_Dummy_Vars(num_vec1,symbol_stack,first_dummy)) {
         result->Append(CXX_NEW(DEPV_NODE(
	     DEPV_CreateStar(pool,dv_dim)),pool));
         if (debug >= 2) {
           fprintf(TFile,"Assume dependent because of overflow \n");
         }
         goto return_point;
       }
 
       if (!a1 && a2) {  // a1 is a call, a2 is not
         REGION_UN &image1 = ara_ref1->Image();
         if (image1.Is_All()) {
           result->Append(CXX_NEW(DEPV_NODE(
	         DEPV_CreateStar(pool,dv_dim)),pool));
           if (debug >= 2) {
             fprintf(TFile,
	        "Assume dependent because summary is Too_Messy or All\n");
           }
           goto return_point;
         } else if (!image1.Is_Empty()) {
	   DEPV_COEFF coeff(CXX_NEW_ARRAY(INT32,num_vec1,&LNO_local_pool),
			    first_dummy,num_vec1);
           if (!Copy_Call_To_Work(image1,symbol_stack,&coeff,TRUE)) {
             result->Append(CXX_NEW(DEPV_NODE(
	         DEPV_CreateStar(pool,dv_dim)),pool));
             if (debug >= 2) {
               fprintf(TFile,"Assume dependent because of overflow \n");
             }
             goto return_point;
           } 
	   if (!Copy_Call_Ref_To_Work(a2,&coeff,symbol_stack,FALSE)) {
             result->Append(CXX_NEW(DEPV_NODE(
	       DEPV_CreateStar(pool,dv_dim)),pool));
             if (debug >= 2) {
               fprintf(TFile,"Assume dependent because of overflow \n");
             }
             goto return_point;
           } 
         }
       } else if (!a2 && a1) {
         REGION_UN &image2 = ara_ref2->Image();
         if (image2.Is_All()) {
           result->Append(CXX_NEW(DEPV_NODE(
	         DEPV_CreateStar(pool,dv_dim)),pool));
           if (debug >= 2) {
             fprintf(TFile,
	        "Assume dependent because summary is Too_Messy or All\n");
           }
           goto return_point;
         } else if (!image2.Is_Empty()) {
	   DEPV_COEFF coeff(CXX_NEW_ARRAY(INT32,num_vec2,&LNO_local_pool),
			    first_dummy,num_vec2);
           if (!Copy_Call_To_Work(image2,symbol_stack,&coeff,FALSE)) {
             result->Append(CXX_NEW(DEPV_NODE(
	         DEPV_CreateStar(pool,dv_dim)),pool));
             if (debug >= 2) {
               fprintf(TFile,"Assume dependent because of overflow \n");
             }
             goto return_point;
           } 
	   if (!Copy_Call_Ref_To_Work(a1,&coeff,symbol_stack,TRUE)) {
             result->Append(CXX_NEW(DEPV_NODE(
	       DEPV_CreateStar(pool,dv_dim)),pool));
             if (debug >= 2) {
               fprintf(TFile,"Assume dependent because of overflow \n");
             }
             goto return_point;
           } 
         }
       } else {  // both references are calls
         REGION_UN &image1 = ara_ref1->Image();
         REGION_UN &image2 = ara_ref2->Image();
         if (image1.Is_All() || image2.Is_All()) {
           result->Append(CXX_NEW(DEPV_NODE(
	         DEPV_CreateStar(pool,dv_dim)),pool));
           if (debug >= 2) {
             fprintf(TFile,
	        "Assume dependent because summary is Too_Messy or All\n");
           }
	   goto return_point;
         } else if (!image1.Is_Empty() && !image2.Is_Empty()) {
	   DEPV_COEFF coeff(CXX_NEW_ARRAY(INT32,num_vec1,&LNO_local_pool),
			    first_dummy,num_vec1);
           if (!Copy_Call_To_Work(image1,symbol_stack,&coeff,TRUE) ||
	       !Copy_Call_To_Work(image2,symbol_stack,&coeff,FALSE)) {
             result->Append(CXX_NEW(DEPV_NODE(
	         DEPV_CreateStar(pool,dv_dim)),pool));
             if (debug >= 2) {
               fprintf(TFile,"Assume dependent because of overflow \n");
             }
	     goto return_point;
           }
         }
       }
     }
   }

   // Deal with indirections off of permutation arrays
   if (a1 && a2 && Permutation_Arrays->Elements() && 
		     (num_vec1 == WN_num_dim(array1))) {
     for (INT i=0; i<num_vec1; i++) {
       if (a1->Dim(i)->Too_Messy && a2->Dim(i)->Too_Messy) {
	 ACCESS_ARRAY *perm1=NULL;
	 ACCESS_ARRAY *perm2=NULL;
	 if (Same_Permutation(WN_array_index(array1,i),
		WN_array_index(array2,i), &perm1,&perm2,
		s1->Bottom_nth(common_nest-(dv_dim-num_bad_outer)))) {
	   if (!perm1->Too_Messy && 
	       !perm2->Too_Messy && perm1->Num_Vec()==perm2->Num_Vec()) {
	     BOOL delin_ok = TRUE;
             for (INT dl=0; dl<perm1->Num_Vec() && delin_ok ; dl++) {
               ACCESS_VECTOR *dl1 = perm1->Dim(dl);
               ACCESS_VECTOR *dl2 = perm2->Dim(dl);
               if (dl1->Delinearized_Symbol && dl2->Delinearized_Symbol) {
                 if (!(*dl1->Delinearized_Symbol==*dl2->Delinearized_Symbol)){
	           delin_ok = FALSE;
                 }
               } else if (dl1->Delinearized_Symbol||dl2->Delinearized_Symbol){
                 delin_ok = FALSE;
               }
             }
	     if (delin_ok) {
               BOOL *non_trivial_dim2 = 
		CXX_NEW_ARRAY(BOOL,perm1->Num_Vec(),&LNO_local_pool);
	       BOOL this_seen_non_trivial = FALSE;
               DEP_RESULT dr = Trivial_Test(perm1,perm2,dv_dim-num_bad_outer,
			common_nest, non_trivial_dim2,&this_seen_non_trivial);
               if (this_seen_non_trivial) seen_non_trivial = TRUE;
               if (dr == DEP_INDEPENDENT) {
                 if (debug >= 2) {
                   fprintf(TFile,"Indirection proven independent by trivial tests \n");
                 }
                 goto return_point;
               } else if (dr != DEP_DEPENDENT) {
		 if (this_seen_non_trivial) {
                   if (!Copy_Equals_To_Work(perm1,perm2,symbol_stack,
							non_trivial_dim2)) {
                     result->Append(CXX_NEW(DEPV_NODE(
			DEPV_CreateStar(pool,dv_dim)),pool));
                     if (debug >= 2) {
                      fprintf(TFile,"Assume dependent because of overflow \n");
                     }
                     goto return_point;
                   }
                 }
               }
             }
           }
         }
       }
     }
   }
   if (!seen_non_trivial) {
     result->Append(CXX_NEW(DEPV_NODE(
		 DEPV_CreateStar(pool,dv_dim)),pool));
     if (debug >= 2) {
       fprintf(TFile,"Shown dependent by trivial tests \n");
     }
     goto return_point;
   }
         
      

   if (use_bounds) {
     if (!Copy_Bounds_To_Work(s1,s2,symbol_stack)) {
       result->Append(CXX_NEW(DEPV_NODE(
		 DEPV_CreateStar(pool,dv_dim)),pool));
       if (debug >= 2) {
         fprintf(TFile,"Assume dependent because of overflow \n");
       }
       goto return_point;
     }
   }

   // Find the constant distance dependences
   // These are used as a starting value for the dependence vector routines
   BOOL *is_used = CXX_NEW_ARRAY(BOOL,_work_cols,&LNO_local_pool);
   DEPV *init_distance = DEPV_Create(&LNO_local_pool,(dv_dim-num_bad_outer));
   dr = Find_Init_Distance_Used(init_distance,is_used,(dv_dim-num_bad_outer));
   if (dr == DEP_INDEPENDENT) {
     if (debug >= 2) {
       fprintf(TFile,"Proved independent finding initial distance\n");
     }
     result->Append(NULL,num_bad_outer);
     goto return_point;
   } else if (dr == DEP_DEPENDENT) {
     result->Append(CXX_NEW(DEPV_NODE(
	 DEPV_CreateStar(pool,dv_dim)),pool));
     if (debug >= 2) {
       fprintf(TFile,"Shown dependent finding initial distance\n");
     }
     goto return_point;
   }
 
   // we don't want all equal dependences from a reference to itself
   if ((ref1 == ref2) && 
	 Is_All_Equals(init_distance,dv_dim-num_bad_outer)) {
     if (debug >= 2) {
       fprintf(TFile,"Shown independent by Is_All_Equals \n");
     }
     result->Append(NULL,num_bad_outer);
     goto return_point;
   }

   INT num_bounds_used=0;
   BOOL *bounds_used=0;
   if (use_bounds) {
     bounds_used = CXX_NEW_ARRAY(BOOL,_work_le_rows,&LNO_local_pool);
     Bounds_Set_Is_Used(is_used,bounds_used,&num_bounds_used);
   }
 
   INT num_vars_used;  // how many variables are used
   INT *map_used = CXX_NEW_ARRAY(INT,_work_cols,&LNO_local_pool);
   // if 'i' is the n'th (starting at zero) used variable then map_used[i] = n
   Set_Map_Used(is_used,&num_vars_used,map_used);
 
   // Create the system of equations, to avoid reallocation give it MAX_ROWS
   // rows, this doesn't effect correctness as the system grows automatically
   SYSTEM_OF_EQUATIONS *soe = CXX_NEW(SYSTEM_OF_EQUATIONS(
     MAX_ROWS,MAX_ROWS,num_vars_used,&LNO_local_pool),&LNO_local_pool);
   soe->Add_Eq(_work_eq_rows);
   if (num_bounds_used > 0) soe->Add_Le(num_bounds_used);
   Copy_To_Soe(is_used,bounds_used,map_used,soe);
   if (!soe->Is_Consistent()) {
     goto return_point;
   }
 
   // Compute the dependence vectors
   if (use_bounds) {
     Compute_Dep_Vectors(soe,is_used,map_used,init_distance,result,
		 (ref1 == ref2),dv_dim-num_bad_outer,num_bad_outer);
   // (if we're not using the bounds, can't really get any more info
   // trying differet dependence vectors)
   } else {
     if (num_bad_outer == 0) {
       result->Append(
	 CXX_NEW(DEPV_NODE(DEPV_Copy(_pool,init_distance,result->_dv_dim)),
			 _pool));
     } else {
       result->Append(DEPV_Copy(&LNO_local_pool,init_distance,
     	 dv_dim-num_bad_outer),num_bad_outer);
     }
   }
   result->Normalize_Step(&_step1[common_nest-dv_dim]);
  }

return_point:  
  if (debug >= 2) {
    fprintf(TFile,"result is "); result->Print(TFile); fprintf(TFile,"\n");
  }
  CXX_DELETE_ARRAY(_step1,_pool);
  CXX_DELETE_ARRAY(_step2,_pool);
  MEM_POOL_Pop(&LNO_local_pool);
}

// Create number dummy variables, return the column position of the first one
// return FALSE on error
BOOL DEPV_COMPUTE::Create_Dummy_Vars(INT32 number, SYMBOL_STACK *symbol_stack,
					INT32 &result)
{
  result = _first_symbol+Find_Enter_Symbol(symbol_stack,NULL);
  _work_cols++;
  for (INT i=1; i<number; i++) {
    Find_Enter_Symbol(symbol_stack,NULL);
    _work_cols++;
  }
  if(_work_cols > MAX_COLS) {
    Is_True(0, ("Column Overflow in DEPV_COMPUTE::Create_Dummy_Vars"));
    MEM_POOL_Pop(&LNO_local_pool);
    return(FALSE);
  }
  for (INT j=0; j<number; j++) {
    INT i;
    for (i=0; i<=_work_eq_rows; i++) {
      _work_eq[i][_work_cols-j] = 0;
    } 
    for (i=0; i<=_work_le_rows; i++) {
      _work_le[i][_work_cols-j] = 0;
    } 
  }
  return TRUE;
}

// If all the dimensions are too_messy or contain non-linear terms then
//	return DEP_DEPENDENT
// if for all the dimensions, Non_Const_Loops >= (common_nest-dv_dim+1) 
//	return DEP_DEPENDENT (i.e. return dependent if there is a
//	symbolic that varies in a loop for which we're computing a
//	distance/direction.  We don't care if they vary in the more outer loops
// If any dimension has two non-equal constants, return DEP_INDEPENDENT
// Otherwise return DEP_CONTINUE
// Set non_trivial_dim[i] to TRUE iff it's non trivial
//
DEP_RESULT DEPV_COMPUTE::Trivial_Test( const ACCESS_ARRAY *a1, 
		const ACCESS_ARRAY *a2, mUINT8 dv_dim,mUINT16 common_nest,
		BOOL *non_trivial_dim, BOOL *seen_non_trivial) const
{
  Is_True(a1->Num_Vec() == a2->Num_Vec(),
  ("DEPV_COMPUTE::Trivial_Test access_arrays have different numbers of dims"));


  for (INT i=0; i<a1->Num_Vec(); i++) {
    ACCESS_VECTOR *av1 = a1->Dim(i);
    ACCESS_VECTOR *av2 = a2->Dim(i);
    non_trivial_dim[i] = FALSE;

    if (!av1->Too_Messy && !av2->Too_Messy &&       
	(av1->Contains_Non_Lin_Symb() == av2->Contains_Non_Lin_Symb())) { 
      if ( av1->Non_Const_Loops() < (common_nest-dv_dim+1) &&
		av2->Non_Const_Loops() < (common_nest-dv_dim+1)) {
        // Non-linear terms allowed if they're const and the same in both refs
	if (!av1->Contains_Non_Lin_Symb() || 
		(*av1->Non_Lin_Symb == *av2->Non_Lin_Symb)) {
          if (!av1->Contains_Lin_Symb() && !av2->Contains_Lin_Symb() && 
	    !av1->Has_Loop_Coeff() && !av2->Has_Loop_Coeff()) { // constant
            if (av1->Const_Offset != av2->Const_Offset) {
              return(DEP_INDEPENDENT);
	    }
          } else {
	    non_trivial_dim[i] = TRUE;
	    *seen_non_trivial = TRUE;
          }
        }
      } 
    } 
  }

  return(DEP_CONTINUE);
}

// are the two access arrays to the same monotonically increasing/decreasing
// expression
//
// Always safe to return FALSE
BOOL DEPV_COMPUTE::Same_Monotonic(WN *array1, WN *array2, 
	const ACCESS_ARRAY *a1, const ACCESS_ARRAY *a2, 
	mUINT16 common_nest, mUINT16 dv_dim,const DOLOOP_STACK *s1) const
{
  if (WN_num_dim(array1) != a1->Num_Vec()) return FALSE;
  for (INT i=0; i<a1->Num_Vec(); i++) {
    ACCESS_VECTOR *av1 = a1->Dim(i);
    ACCESS_VECTOR *av2 = a2->Dim(i);
    if (av1->Non_Const_Loops() >= (common_nest-dv_dim+1) ||
        av2->Non_Const_Loops() >= (common_nest-dv_dim+1)) {
       if (Same_Monotonic(WN_array_index(array1,i),WN_array_index(array2,i),
		av1,av2,common_nest,dv_dim,s1)) {
         return TRUE;
        }
    }
  }
  return FALSE;
}


// are the two access vectors to the same monotonically increasing/decreasing
// expression
//
// Always safe to return FALSE
BOOL DEPV_COMPUTE::Same_Monotonic(WN *array1_index, WN *array2_index, 
	const ACCESS_VECTOR *av1, const ACCESS_VECTOR *av2, 
	mUINT16 common_nest, mUINT16 dv_dim,const DOLOOP_STACK *s1) const
{
  if (!(*av1 == *av2)) {
    return FALSE;
  }
  if (av1->Too_Messy || av1->Contains_Non_Lin_Symb()) {
    return(FALSE);
  }
  if (av1->Has_Loop_Coeff()) {
    for (INT i=0; i<common_nest; i++) {
      if (av1->Loop_Coeff(i)) return FALSE;
    }
  }

  BOOL seen_increase = FALSE;
  BOOL seen_decrease = FALSE;
  if (av1->Lin_Symb) {
    INTSYMB_ITER iter(av1->Lin_Symb);

    for (INTSYMB_NODE *node=iter.First();!iter.Is_Empty(); node=iter.Next()) {
      INT coeff = node->Coeff;
      // find the wn corresponding to the symbol in the access vector for each ref
      // it doesn't matter if there is more than one occurence of the symbol (ie a(n+n))
      // since they must all have the same defs

      const WN *wn1 = Find_First_Ldid_For_Symbol(array1_index,&node->Symbol);
      const WN *wn2 = Find_First_Ldid_For_Symbol(array2_index,&node->Symbol);
      if (!wn1 || !wn2) return FALSE;

      // the two wns need to have a single def and that def must be an increment/decrement
      // and that increment must not be defined anywhere in the dv_dim loops
      DEF_LIST *def1 = Du_Mgr->Ud_Get_Def((WN *) wn1);
      if (!def1 || def1->Incomplete() || def1->Loop_stmt()) return FALSE;
      DEF_LIST_ITER iter1(def1);
      DU_NODE *du_node1 = iter1.First();
      if (!du_node1 || iter1.Next()) return FALSE;
      WN *def_wn1 = du_node1->Wn();

      DEF_LIST *def2 = Du_Mgr->Ud_Get_Def((WN *) wn2);
      if (!def2 || def2->Incomplete() || def2->Loop_stmt()) return FALSE;
      DEF_LIST_ITER iter2(def2);
      DU_NODE *du_node2 = iter2.First();
      if (!du_node2 || iter2.Next()) return FALSE;
      WN *def_wn2 = du_node2->Wn();

      if (def_wn1 != def_wn2) return FALSE;

      // the def has to be executed on every common iteration
      if (LWN_Get_Parent(def_wn1) != WN_do_body(s1->Bottom_nth(common_nest-1))) {
	return FALSE;
      }

      if (WN_operator(def_wn1) != OPR_STID) {
	return FALSE;
      }
      if (!(SYMBOL(def_wn1) == node->Symbol)) return FALSE;

      WN *incr = WN_kid0(def_wn1);
      OPERATOR incr_oper = WN_operator(incr);
      if ((incr_oper != OPR_ADD) && (incr_oper != OPR_SUB)) return FALSE;

      WN *kid0 = WN_kid0(incr);
      OPERATOR oper0 = WN_operator(kid0);
      WN *kid1 = WN_kid1(incr);
      OPERATOR oper1 = WN_operator(kid1);
      WN *rhs;
      if ((oper0 == OPR_INTCONST) && (oper1 == OPR_LDID)) {
	if (incr_oper == OPR_SUB) return FALSE;
	if (!(SYMBOL(kid1) == node->Symbol)) return FALSE;
	if ((WN_const_val(kid0) > 0) == (coeff > 0)) {
	  seen_increase = TRUE;
        } else {
	  seen_decrease = TRUE;
        }
	rhs = kid1;
      } else if ((oper1 == OPR_INTCONST) && (oper0 == OPR_LDID)) {
	if (!(SYMBOL(kid0) == node->Symbol)) return FALSE;
	if (incr_oper == OPR_ADD) {
	  if ((WN_const_val(kid1) > 0) == (coeff > 0)) {
	    seen_increase = TRUE;
          } else {
	    seen_decrease = TRUE;
          }
        } else {	
	  if ((WN_const_val(kid1) > 0) == (coeff > 0)) {
	    seen_decrease = TRUE;
          } else {
	    seen_increase = TRUE;
          }
        }
	rhs = kid0;
      } else {
        return FALSE;
      }
      // the rhs must only be defined by def_wn and things
      // farther out than dv_dim
      DEF_LIST *incr_defs = Du_Mgr->Ud_Get_Def(rhs);
      if (!incr_defs || incr_defs->Incomplete())  return FALSE;
      DEF_LIST_ITER incr_iter(incr_defs);
      for(DU_NODE *node=incr_iter.First(); !incr_iter.Is_Empty(); node=incr_iter.Next()) {
	WN *def = node->Wn();
	if ((def != def_wn1) && Is_Descendent(def,s1->Bottom_nth(common_nest-dv_dim))) {
	  return FALSE;
        }
      }
    }
  } else {
    return FALSE;
  }
  if (seen_increase && seen_decrease) {
    return FALSE;
  }
  return TRUE;
}

// given that wn is an expression subtree, find the use of symb in 
// the subtree.  Return null if can't find it (should never happen)
const WN *DEPV_COMPUTE::Find_First_Ldid_For_Symbol(const WN *wn, const SYMBOL *symb) const
{
  if (WN_operator(wn) == OPR_LDID) {
    if (SYMBOL(wn) == *symb) {
      return wn;
    }
  }
  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    const WN *tmp =  Find_First_Ldid_For_Symbol(WN_kid(wn,kidno),symb);
    if (tmp) {
      return tmp;
    }
  }
  return NULL;
}

//
// If for some dimension, Non_Const_Loops >= (common_nest-dv_dim+1),
//   perform a simple GCD test.  This is ment to catch things like
//   a(4*n) vrs a(4*n+1), where n is varying unpredictably
BOOL DEPV_COMPUTE::Simple_Gcd_Indep(ACCESS_ARRAY *a1, ACCESS_ARRAY *a2,
			INT common_nest, INT dv_dim) const
{
  for (INT i=0; i<a1->Num_Vec(); i++) {
    ACCESS_VECTOR *av1 = a1->Dim(i);
    ACCESS_VECTOR *av2 = a2->Dim(i);
    if (av1->Non_Const_Loops() >= (common_nest-dv_dim+1) ||
        av2->Non_Const_Loops() >= (common_nest-dv_dim+1)) {
      if (Simple_Gcd_Indep(av1,av2)) {
	return TRUE;
      }
    }
  }
  return FALSE;
}



// Apply the gcd test to the two access vectors
// Because of unpredictably varying symbolics, our regular analysis
// won't apply for these vectors
BOOL DEPV_COMPUTE::Simple_Gcd_Indep(ACCESS_VECTOR *av1, 
					ACCESS_VECTOR *av2) const
{
  if (av1->Too_Messy || av2->Too_Messy || av1->Contains_Non_Lin_Symb() ||
      av2->Contains_Non_Lin_Symb()) {
    return(FALSE);
  }

  INT64 c = abs(av2->Const_Offset - av1->Const_Offset);
  if (!c) return(FALSE);

  BOOL seen_coeff = FALSE;
  INT gcd;

  // induction variables
  if (av1->Has_Loop_Coeff()) {
    for (INT i=0; i<av1->Nest_Depth(); i++) {
      INT coeff = abs(av1->Loop_Coeff(i));
      if (coeff) {
	if (coeff == 1) {
	  return(FALSE);   // the gcd will be 1
        }
	if (!seen_coeff) {
	  gcd = coeff;
        } else {
	  gcd = Gcd(gcd,coeff);
        }
	seen_coeff = 1;
      }
    }
  }
  if (av2->Has_Loop_Coeff()) {
    for (INT i=0; i<av2->Nest_Depth(); i++) {
      INT coeff = abs(av2->Loop_Coeff(i));
      if (coeff) {
	if (coeff == 1) {
	  return(FALSE);   // the gcd will be 1
        }
	if (!seen_coeff) {
	  gcd = coeff;
        } else {
	  gcd = Gcd(gcd,coeff);
        }
	seen_coeff = 1;
      }
    }
  }

  // symbolics 
  if (av1->Lin_Symb) {
    INTSYMB_ITER iter(av1->Lin_Symb);
    for (INTSYMB_NODE *node=iter.First();!iter.Is_Empty(); node=iter.Next()) {
      INT coeff = abs(node->Coeff);
      if (coeff) {
	if (coeff == 1) {
	  return(FALSE);   // the gcd will be 1
        }
	if (!seen_coeff) {
	  gcd = coeff;
        } else {
	  gcd = Gcd(gcd,coeff);
        }
	seen_coeff = 1;
      }
    }
  }
  if (av2->Lin_Symb) {
    INTSYMB_ITER iter(av2->Lin_Symb);
    for (INTSYMB_NODE *node=iter.First();!iter.Is_Empty(); node=iter.Next()) {
      INT coeff = abs(node->Coeff);
      if (coeff) {
	if (coeff == 1) {
	  return(FALSE);   // the gcd will be 1
        }
	if (!seen_coeff) {
	  gcd = coeff;
        } else {
	  gcd = Gcd(gcd,coeff);
        }
	seen_coeff = 1;
      }
    }
  }
  if (!seen_coeff || ((c % gcd) != 0)) return(TRUE);
  return(FALSE);
}

static POINTS_TO* Points_To_For_Array_Star(ARA_REF *ara_ref1)
{
  ST* st1 = ara_ref1->Array().St();
  INT64 offset1 = ara_ref1->Array().ST_Offset();
  OPCODE opc_ldid = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);
  WN* wn_ldid = WN_CreateLdid(opc_ldid, offset1, st1, ST_type(st1));
  TYPE_ID wtype = TY_mtype(TY_AR_etype(TY_pointed(ST_type(st1))));
  OPCODE opc_iload = OPCODE_make_op(OPR_ILOAD, Promote_Type(wtype), wtype);
  TY_IDX wty = Be_Type_Tbl(wtype);
  TY_IDX pty = Make_Pointer_Type(Be_Type_Tbl(wtype));
  WN* wn_iload = WN_CreateIload(opc_iload, 0, wty, pty, wn_ldid);
  Create_alias(Alias_Mgr, wn_iload);
  IDTYPE id = Alias_Mgr->Id(wn_iload);
  POINTS_TO* pt = Alias_Mgr->Pt(id);
  WN_Delete(wn_ldid);
  WN_Delete(wn_iload);
  return pt; 
} 

// check the bases and the bounds of the two arrays 
// if the bases are the same and the bounds match return DEP_CONTINUE
//  	(i.e. we need the subscripts to compute dependence info)
// if the bases are disjoint return DEP_INDEPENDENT
// if the bases might overlap or the bounds don't match return DEP_DEPENDENT
DEP_RESULT DEPV_COMPUTE::Base_Test(const WN *ref1,  ARA_REF *ara_ref1,
			const WN *ref2, ARA_REF *ara_ref2) 
{
  // Find POINTS_TOs for each ara_ref, if needed.
  WN *array1,*array2;
  ALIAS_RESULT aresult;
  POINTS_TO* pt1 = NULL;
  if (ara_ref1 != NULL) {
    ST* st1 = ara_ref1->Array().St();
    if (TY_kind(ST_type(st1)) == KIND_POINTER) { 
      pt1 = Points_To_For_Array_Star(ara_ref1);
    } else { 
      INT64 offset1 = ara_ref1->Array().ST_Offset();
      INT64 size1 = TY_size(ST_type(st1));
      pt1 = CXX_NEW(POINTS_TO(st1, offset1, size1), &LNO_local_pool);
    } 
  }
  POINTS_TO* pt2 = NULL;
  if (ara_ref2 != NULL) {
    ST* st2 = ara_ref2->Array().St();
    if (TY_kind(ST_type(st2)) == KIND_POINTER) { 
      pt2 = Points_To_For_Array_Star(ara_ref2);
    } else { 
      INT64 offset2 = ara_ref2->Array().ST_Offset();
      INT64 size2 = TY_size(ST_type(st2));
      pt2 = CXX_NEW(POINTS_TO(st2, offset2, size2), &LNO_local_pool);
    } 
  }

  // If both references are from IPA CALL information
  if (ara_ref1 != NULL && ara_ref2 != NULL) {
    ALIAS_RESULT ar = Alias_Mgr->Aliased(pt1, pt2);
    if (ar == NOT_ALIASED)
      return DEP_INDEPENDENT;
    if (ara_ref1->Is_Whole_Array() || ara_ref1->Is_Messy())
      return DEP_DEPENDENT;
    if (ara_ref2->Is_Whole_Array() || ara_ref2->Is_Messy())
      return DEP_DEPENDENT;
    if (ara_ref1->Array().St() != ara_ref2->Array().St())
      return DEP_DEPENDENT;
    if (!Equiv_Dims(ara_ref1, ara_ref2))
      return DEP_DEPENDENT;
    return DEP_CONTINUE;
  }

  // If one references is from IPA call information, and the other
  // an ILOAD or ISTORE directly referenced in this subroutine.
  if (ara_ref1 != NULL || ara_ref2 != NULL) {
    POINTS_TO* pt = ara_ref1 != NULL ? pt1 : pt2;
    const WN* wn_ref = ara_ref1 == NULL ? ref1 : ref2;
    ARA_REF* ara_ref = ara_ref1 != NULL ? ara_ref1 : ara_ref2;
    ALIAS_RESULT ar = Alias_Mgr->Aliased(pt, (WN*) wn_ref);
    if (ar == NOT_ALIASED)
      return DEP_INDEPENDENT;
    if (ara_ref->Is_Whole_Array() || ara_ref->Is_Messy())
      return DEP_DEPENDENT;
    const WN* wn_array = (OPCODE_is_load(WN_opcode(wn_ref)) ?
      WN_kid0(wn_ref) : WN_kid1(wn_ref));
    // TODO conservatively ignore structures for now
    if (WN_operator(wn_array) != OPR_ARRAY)
      return DEP_DEPENDENT;
    WN* wn_base = WN_array_base(wn_array);
    OPERATOR opr_base = WN_operator(wn_base);
    if (opr_base != OPR_LDA && opr_base != OPR_LDID)
      return DEP_DEPENDENT;
    SYMBOL sym(wn_base);
    if (sym.St() != ara_ref->Array().St())
      return DEP_DEPENDENT;
    if (!Equiv_Dims(wn_array, ara_ref))
      return DEP_DEPENDENT;
    return DEP_CONTINUE;
  }

  // At this point, both should be ILOADs and/or ISTOREs directly
  // referenced in the subroutine.
  FmtAssert(ara_ref1 == NULL && ara_ref2 == NULL,
    ("Base_Test: Should be analyze two arrays at this point"));

  aresult = Overlapped_base(Alias_Mgr,ref1,ref2);

  if (aresult == NOT_ALIASED) {
    return DEP_INDEPENDENT;
  }
  Is_True(OPCODE_is_load(WN_opcode(ref1)) || OPCODE_is_store(WN_opcode(ref1)),
	  ("Non-load/store for ref1 in DEPV_COMPUTE::Base_Test"));
  Is_True(OPCODE_is_load(WN_opcode(ref2)) || OPCODE_is_store(WN_opcode(ref2)),
	  ("Non-load/store for ref2 in DEPV_COMPUTE::Base_Test"));

  INT type_size = MAX(MTYPE_RegisterSize(WN_desc(ref1)),
                 MTYPE_RegisterSize(WN_desc(ref2)));
  WN *const1,*const2;
  array1 = (OPCODE_is_load(WN_opcode(ref1)) ? (WN_kid0(ref1)):(WN_kid1(ref1)));
  array2 = (OPCODE_is_load(WN_opcode(ref2)) ? (WN_kid0(ref2)):(WN_kid1(ref2)));
  if (WN_operator(array1) == OPR_ADD) {
    if (WN_operator(array2) != OPR_ADD) {
      return DEP_DEPENDENT;
    }
    if (WN_operator(WN_kid0(array1)) == OPR_ARRAY) {
      array1 = WN_kid0(array1);
      const1 = WN_kid1(array1);
    } else {
      array1 = WN_kid1(array1);
      const1 = WN_kid0(array1);
    }
    if (WN_operator(WN_kid0(array2)) == OPR_ARRAY) {
      array2 = WN_kid0(array2);
      const2 = WN_kid1(array2);
    } else {
      array2 = WN_kid1(array2);
      const2 = WN_kid0(array2);
    }
    if ((WN_operator(const1) != OPR_INTCONST) ||
        (WN_operator(const2) != OPR_INTCONST)) {
      return DEP_DEPENDENT;
    }
    if (WN_element_size(array1) != WN_element_size(array2)) {
      return DEP_DEPENDENT;
    }
    INT diff = abs(WN_const_val(const1) + WN_offset(ref1) 
	           -WN_const_val(const2) - WN_offset(ref2));
    if (diff != 0) {
      if ((diff >= type_size) && (diff < WN_element_size(array1))) {
        return DEP_INDEPENDENT;  // different structure elements
      } else {
	return DEP_DEPENDENT;
      }
    }
  } else if (WN_operator(array2) == OPR_ADD) {
    return DEP_DEPENDENT;
  } else {
    if (WN_element_size(array1) != WN_element_size(array2)) {
      return DEP_DEPENDENT;
    }
    INT diff = abs(WN_offset(ref1) - WN_offset(ref2));
#ifdef KEY
    // Bug 3834 - When one reference accesses a field in a structure 
    // and the other writes the structure (for example, read array[i].field 
    // and write array[i]), then have to continue the base test.
    if (((WN_desc(ref1) == MTYPE_M && WN_offset(ref1) == 0 &&
	  OPCODE_is_store(WN_opcode(ref1))) ||
	 (WN_desc(ref2) == MTYPE_M && WN_offset(ref2) == 0 &&
	  OPCODE_is_store(WN_opcode(ref2)))) &&
	WN_Simp_Compare_Trees(array1, array2) == 0)
      diff = 0;
#endif
    if (diff != 0) {
      if ((diff >= type_size) && (diff < WN_element_size(array1))) {
        return DEP_INDEPENDENT;
      } else {
        return DEP_DEPENDENT;
      }
    }
  }


  WN *base1 = WN_array_base(array1);
  WN *base2 = WN_array_base(array2);

  OPERATOR ob1 = WN_operator(base1);
  OPERATOR ob2 = WN_operator(base2);

  if (ob1 != ob2) return (DEP_DEPENDENT);

  BOOL different_structure_offsets = FALSE;
  if (ob1 == OPR_ADD) {
    INT offset1,offset2;
    if (WN_operator(WN_kid0(base1)) == OPR_INTCONST) { 
      offset1 = WN_const_val(WN_kid0(base1));
      base1 = WN_kid1(base1);
      ob1 = WN_operator(base1);
    } else if (WN_operator(WN_kid1(base1)) == OPR_INTCONST) { 
      offset1 = WN_const_val(WN_kid1(base1));
      base1 = WN_kid0(base1);
      ob1 = WN_operator(base1);
    } else {
      return(DEP_DEPENDENT);
    }
    if (WN_operator(WN_kid0(base2)) == OPR_INTCONST) { 
      offset2 = WN_const_val(WN_kid0(base2));
      base2 = WN_kid1(base2);
      ob2 = WN_operator(base2);
    } else if (WN_operator(WN_kid1(base2)) == OPR_INTCONST) { 
      offset2 = WN_const_val(WN_kid1(base2));
      base2 = WN_kid0(base2);
      ob2 = WN_operator(base2);
    } else {
      return(DEP_DEPENDENT);
    }
    different_structure_offsets = (offset1 != offset2);
  }
  if (ob1 != ob2) {
    return (DEP_DEPENDENT);
  }
  if (ob1 == OPR_LDA) {
    if (ST_ofst(WN_st(base1)) != ST_ofst(WN_st(base2)) ||
        ST_base(WN_st(base1)) != ST_base(WN_st(base2)) ||
        WN_offset(base1) != WN_offset(base2)) {

      return(DEP_DEPENDENT);
    }
  } else if (ob1 == OPR_LDID) {
    // only continue if the two ldids are identical or have the same, 
    // single, def 
    ALIAS_RESULT result = Aliased(Alias_Mgr, base1, base2);
    if (result != SAME_LOCATION) {
      WN *def1 = Find_Def(base1);
      if (!def1) return (DEP_DEPENDENT); 
      WN *def2 = Find_Def(base2);
      if (!def2) return (DEP_DEPENDENT); 
      if (def1 != def2) return (DEP_DEPENDENT);
      if (WN_operator(def1) != OPR_STID) {
	return DEP_DEPENDENT;
      }
      if (WN_desc(def1) !=
	  WN_desc(base1)) {
        return DEP_DEPENDENT;
      }
    }
  }
  else if (ob1 == OPR_ILOAD) {
    // The bases of array1 and array2 are both indirect loads.
    // Continue if the indirect loads are loop invariants
    // and the addresses of indirect loads are the same.

    if (WN_offset(base1) != WN_offset(base2))
      return DEP_DEPENDENT;

    if (!Is_Loop_Invariant_Indir(base1)
	|| !Is_Loop_Invariant_Indir(base2))
      return DEP_DEPENDENT;

    WN * addr1 = WN_kid0(base1);
    WN * addr2 = WN_kid0(base2);

    if (WN_Simp_Compare_Trees(addr1, addr2) != 0)
      return DEP_DEPENDENT;

    if (Base_Test(base1, NULL, base2, NULL) != DEP_CONTINUE)
      return DEP_DEPENDENT;

  }
  else {
    return (DEP_DEPENDENT);
  }

  if (different_structure_offsets) return DEP_INDEPENDENT;

  // By this point the two arrays are the same array
  // check that the dimensions are equivalent 
  // with F77 they should always be, with reshaping they might not
  if (!Equiv_Dims(array1,array2)) {
    return(DEP_DEPENDENT);
  }

  return(DEP_CONTINUE);
}

// Find the STID or OPC_FUNC_ENTRY that defines this ldid
// Return nill if there is none or more than one
// But, if all of the defs are FUNC_ENTRIES or
// ALTENTRIES then return the FUNC_ENTRY (even if multiple defs)
WN *DEPV_COMPUTE::Find_Def(WN *wn)
{
  DEF_LIST *defs = Du_Mgr->Ud_Get_Def(wn);
  if (!defs || defs->Is_Empty()) {
    DevWarn("No defs in DEPV_COMPUTE::Find_Def");
    return NULL;
  }
  if (defs->Incomplete()) return NULL;

  DEF_LIST_ITER iter(defs);
  BOOL seen_non_entry = FALSE;
  BOOL seen_one = FALSE;
  BOOL seen_mult = FALSE;
  WN *def;
  for (const DU_NODE *node = iter.First();
        !iter.Is_Empty(); node=iter.Next()) {
    if (seen_one) seen_mult = TRUE;
    def = (WN *) node->Wn();
    OPERATOR opr = WN_operator(def);
    if (opr == OPR_STID) {
      seen_non_entry = TRUE;
    } else if ((opr != OPR_FUNC_ENTRY) && (opr != OPR_ALTENTRY)) {
	return NULL;
    }
    if (seen_mult && seen_non_entry) return NULL;
    seen_one = TRUE;
  }
  if (seen_non_entry) { // must be a single STID
    return def;
  } 
  // return the FUNC_ENTRY
  WN *tmp = wn;
  while (LWN_Get_Parent(tmp)) tmp = LWN_Get_Parent(tmp);
  Is_True(WN_opcode(tmp) == OPC_FUNC_ENTRY,("Root isn't FUNC_ENTRY"));
  return tmp;
}

// how many of the outer loops on the two stacks are shared
mUINT16 DEPV_COMPUTE::Common_Nest(const DOLOOP_STACK *s1, 
				const DOLOOP_STACK *s2) const
{
  INT i;
  for (i=0; i<MIN(s1->Elements(),s2->Elements()); i++) {
    if (s1->Bottom_nth(i) != s2->Bottom_nth(i)) return(i);
  }
  return(i);
}

// Are the dimensions for the two array statments equivalent
// We ignore the first dimension as it doesn't contribute to the address
BOOL DEPV_COMPUTE::Equiv_Dims(const WN *array1, const WN *array2) 
{
  if (WN_num_dim(array1) != WN_num_dim(array2)) {
    return(FALSE);
  }

  for (INT i=1; i<WN_num_dim(array1); i++) {
    if (!Equiv_Dim(WN_array_dim(array1,i), WN_array_dim(array2,i))) {
      return(FALSE);
    }
  }
  return(TRUE);
}

BOOL DEPV_COMPUTE::Equiv_Dims(const WN* wn_array, ARA_REF* ara_ref)
{
  if (ara_ref->Is_Whole_Array() || ara_ref->Is_Messy())
    return FALSE;
  REGION_UN* ru_ref = &ara_ref->Image();
  if (ru_ref->Is_Bottom() || ru_ref->Is_All())
    return FALSE;
  REGION_ITER iter(ru_ref);
  INT num_dim = -1;
  for (REGION* rg = iter.First(); !iter.Is_Empty(); rg = iter.Next()) {
    if (num_dim == -1 && rg->Num_Dim() != -1)
      num_dim = rg->Num_Dim();
    else if (rg->Num_Dim() != num_dim)
      return FALSE;
  }
  if (WN_num_dim(wn_array) != num_dim)
    return FALSE;
  // At this point, maybe we should go into the symbol table and actually
  // check the bounds that are stored there, using them for the IPA call
  // reference. (RJC)
  return TRUE;
}

BOOL DEPV_COMPUTE::Equiv_Dims(ARA_REF* ara_ref, const WN* wn_array)
{
  return Equiv_Dims(wn_array, ara_ref);
}

BOOL DEPV_COMPUTE::Equiv_Dims(ARA_REF* ara_ref1, ARA_REF* ara_ref2)
{
  if (ara_ref1->Is_Whole_Array() || ara_ref1->Is_Messy())
    return FALSE;
  REGION_UN* ru_ref1 = &ara_ref1->Image();
  if (ru_ref1->Is_Bottom() || ru_ref1->Is_All())
    return FALSE;
  REGION_ITER iter1(ru_ref1);
  INT num_dim1 = -1;
  REGION* rg = 0;
  for (rg = iter1.First(); !iter1.Is_Empty(); rg = iter1.Next()) {
    if (num_dim1 == -1)
      num_dim1 = rg->Num_Dim();
    else if (rg->Num_Dim() != num_dim1)
      return FALSE;
  }
  if (ara_ref2->Is_Whole_Array() || ara_ref2->Is_Messy())
    return FALSE;
  REGION_UN* ru_ref2 = &ara_ref2->Image();
  if (ru_ref2->Is_Bottom() || ru_ref2->Is_All())
    return FALSE;
  INT num_dim2 = -1;
  REGION_ITER iter2(ru_ref2);
  for (rg = iter2.First(); !iter2.Is_Empty(); rg = iter2.Next()) {
    if (num_dim2 == -1)
      num_dim2 = rg->Num_Dim();
    else if (rg->Num_Dim() != num_dim2)
      return FALSE;
  }
  if (num_dim1 != num_dim2)
    return FALSE;
  return TRUE;
}

BOOL DEPV_COMPUTE::Equiv_Dim(const WN *exp1, const WN *exp2) 
{
  if (WN_opcode(exp1) != WN_opcode(exp2)) return(FALSE);
  if (WN_kid_count(exp1) != WN_kid_count(exp2)) return(FALSE);
  if (OPCODE_is_load(WN_opcode(exp1))) {  // make sure we're loading the same 
					  // value, and that its loop-indep
    OPERATOR oper = WN_operator(exp1);
    if (oper == OPR_LDA) {
      if (WN_load_offset(exp1) != WN_load_offset(exp2)) return(FALSE);
      if (WN_st(exp1) != WN_st(exp2)) return(FALSE);
    } else if (oper == OPR_LDID) {
      // only continue if the two ldids are the same variable or if
      // they have the same, single, def and
      // if that def is not inside a loop
      if (WN_load_offset(exp1) == WN_load_offset(exp2) &&
	  WN_st(exp1) == WN_st(exp2)) {
        return TRUE;
      }
      WN *def1 = Find_Def((WN *)exp1);
      if (!def1) return (FALSE); 
      WN *def2 = Find_Def((WN *)exp2);
      if (!def2) return (FALSE); 
      if (def1 != def2) return (FALSE);

      if (WN_opcode(def1) == OPC_FUNC_ENTRY) {
        if (ST_ofst(WN_st(exp1)) != ST_ofst(WN_st(exp2)) ||
            ST_base(WN_st(exp1)) != ST_base(WN_st(exp2)) ||
            WN_offset(exp1) != WN_offset(exp2)) {
          return(FALSE);
        }
      } else {
        while (def1) {
          if (WN_opcode(def1) == OPC_DO_LOOP) {
	    return FALSE;
          }
          def1 = LWN_Get_Parent(def1);
        }
      }
    } else {
      return FALSE;
    }
  } else if (WN_operator(exp1) == OPR_CONST) {
    if (WN_st(exp1) != WN_st(exp2)) return(FALSE);
  } else if (WN_operator(exp1) == OPR_INTCONST) {
    if (WN_const_val(exp1) != WN_const_val(exp2)) return(FALSE);
  } else {
    for (INT kidno=0; kidno<WN_kid_count(exp1); kidno++) {
      if (!Equiv_Dim(WN_kid(exp1,kidno),WN_kid(exp2,kidno))) {
	return FALSE;
      }
    }
  }
  return(TRUE);
}


// Copy all the equality constraints into the work array
// We use a work array so we can get rid of unused variables
// The work array will contain the expression a2-a1=0
// Only use the access_vectors with non_trivial_dim = TRUE
// Put all the symbols into symbol_stack
// Return FALSE on failure (out of space)
BOOL DEPV_COMPUTE::Copy_Equals_To_Work(const ACCESS_ARRAY *a1, 
	const ACCESS_ARRAY *a2, SYMBOL_STACK *symbol_stack, 
	const BOOL *non_trivial_dim) 
{
  // find the first non-trivial dim (there has to be one)
  INT i = 0;
  while (!non_trivial_dim[i]) i++;

  if (_first_symbol > MAX_COLS) {
    Is_True(0,("Overflow in DEPV_COMPUTE::Copy_Equals_To_Work"));
    return FALSE;
  }
  _work_cols=_first_symbol;

  while (i < a1->Num_Vec()) {
    if (non_trivial_dim[i]) {
      if(_work_eq_rows >= MAX_ROWS) {
	Is_True(0,("Row Overflow in DEPV_COMPUTE::Copy_Equals_To_Work"));
	return(FALSE);
      }
      ACCESS_VECTOR *av1 = a1->Dim(i);
      ACCESS_VECTOR *av2 = a2->Dim(i);
      if (!Copy_Equal_To_Work(av1,av2,symbol_stack)) {
	Is_True(0,("Overflow in DEPV_COMPUTE::Copy_Equals_To_Work"));
	return(FALSE);
      }
    }
    i++;
  }
  return(TRUE);
}

// copy a single equals constraint
// return FALSE on error
BOOL DEPV_COMPUTE::Copy_Equal_To_Work(ACCESS_VECTOR *av1, ACCESS_VECTOR *av2,
					SYMBOL_STACK *symbol_stack) 
{
  if (av1->Too_Messy || av2->Too_Messy) return TRUE;
  if(_work_eq_rows >= MAX_ROWS) {
    Is_True(0,("Row Overflow in DEPV_COMPUTE::Copy_Equal_To_Work"));
    return(FALSE);
  }
  _work_eq_const[_work_eq_rows] = av1->Const_Offset - av2->Const_Offset;

  // do the loop variables
  if (av1->Has_Loop_Coeff()) {
    for (INT j=0; j<_nd1; j++) {
      _work_eq[_work_eq_rows][j] = -av1->Loop_Coeff(j);
    }
  } else {
    for (INT j=0; j<_nd1; j++) {
     _work_eq[_work_eq_rows][j] = 0;
    }
  }

  if (av2->Has_Loop_Coeff()) {
    INT j;
    for (j=0; j<_first_dv1; j++) {
      _work_eq[_work_eq_rows][j] += av2->Loop_Coeff(j);
    }
    for (j=_nd1; j<_first_symbol; j++) {
      _work_eq[_work_eq_rows][j] = +av2->Loop_Coeff(j-_nd1+_first_dv1);
    }
  } else {
    for (INT j=_nd1; j<_first_symbol; j++) {
      _work_eq[_work_eq_rows][j] = 0;
    }
  }

  // now do the symbols
  for (INT j=_first_symbol; j<_work_cols; j++) {
    _work_eq[_work_eq_rows][j] = 0;
  }

  if (av1->Contains_Lin_Symb()) {
    INTSYMB_ITER iter(av1->Lin_Symb);
    for (INTSYMB_NODE *node = iter.First(); !iter.Is_Empty(); 
							node=iter.Next()) {
      INT j = Find_Enter_Symbol(symbol_stack,&node->Symbol);
      if (j+_first_symbol >= _work_cols) {  // a new symbol
        _work_cols=j+1+_first_symbol;
        if(_work_cols > MAX_COLS) {
	  Is_True(0, ("Column Overflow in DEPV_COMPUTE::Copy_Equal_To_Work"));
	  return(FALSE);
	}
        INT k;
	for (k=0; k<=_work_eq_rows; k++) {
	  _work_eq[k][_work_cols-1] = 0;
	} 
	for (k=0; k<=_work_le_rows; k++) {
	  _work_le[k][_work_cols-1] = 0;
	} 
      }
      _work_eq[_work_eq_rows][j+_first_symbol] -= node->Coeff;
    }
  }
  if (av2->Contains_Lin_Symb()) {
    INTSYMB_ITER iter(av2->Lin_Symb);
    for (INTSYMB_NODE *node = iter.First(); !iter.Is_Empty(); 
							node=iter.Next()) {
      INT j = Find_Enter_Symbol(symbol_stack,&node->Symbol);
      if (j+_first_symbol >= _work_cols) {  // a new symbol
  	    _work_cols=j+1+_first_symbol;
        if(_work_cols > MAX_COLS) {
	  Is_True(0,("Column Overflow in DEPV_COMPUTE::Copy_Equals_To_Work"));
	  return(FALSE);
        }
        INT k;
	for (k=0; k<=_work_eq_rows; k++) {
	  _work_eq[k][_work_cols-1] = 0;
	} 
	for (k=0; k<=_work_le_rows; k++) {
	  _work_le[k][_work_cols-1] = 0;
        }
      }
      _work_eq[_work_eq_rows][j+_first_symbol] += node->Coeff;
    }
  }
  _work_eq_rows++;
  return(TRUE);
}

// copy in the constraint av + coeff = 0
// return FALSE on error
BOOL DEPV_COMPUTE::Copy_Equal_To_Work(ACCESS_VECTOR *av, DEPV_COEFF *coeff, 
	SYMBOL_STACK *symbol_stack, BOOL is_first_ref) 
{
  if (av->Too_Messy) return TRUE;
  if(_work_eq_rows >= MAX_ROWS) {
    Is_True(0,("Row Overflow in DEPV_COMPUTE::Copy_Equal_To_Work"));
    return(FALSE);
  }
  _work_eq_const[_work_eq_rows] = -av->Const_Offset;

  INT j;
  for (j=0; j<_work_cols; j++) {
    _work_eq[_work_eq_rows][j] = 0;
  }

  // do the loop variables
  if (is_first_ref) {
    if (av->Has_Loop_Coeff()) {
      for (INT j=0; j<_nd1; j++) {
        _work_eq[_work_eq_rows][j] = av->Loop_Coeff(j);
      }
    } 
  } else {
    if (av->Has_Loop_Coeff()) {
      INT j;
      for (j=0; j<_first_dv1; j++) {
        _work_eq[_work_eq_rows][j] = av->Loop_Coeff(j);
      }
      for (j=_nd1; j<_first_symbol; j++) {
        _work_eq[_work_eq_rows][j] = +av->Loop_Coeff(j-_nd1+_first_dv1);
      }
    } 
  }

  // now do the symbols
  if (av->Contains_Lin_Symb()) {
    INTSYMB_ITER iter(av->Lin_Symb);
    for (INTSYMB_NODE *node = iter.First(); !iter.Is_Empty(); 
							node=iter.Next()) {
      INT j = Find_Enter_Symbol(symbol_stack,&node->Symbol);
      if (j+_first_symbol >= _work_cols) {  // a new symbol
        _work_cols=j+1+_first_symbol;
        if(_work_cols > MAX_COLS) {
	  Is_True(0, ("Column Overflow in DEPV_COMPUTE::Copy_Equal_To_Work"));
	  return(FALSE);
	}
        INT k;
	for (k=0; k<=_work_eq_rows; k++) {
	  _work_eq[k][_work_cols-1] = 0;
	} 
	for (k=0; k<=_work_le_rows; k++) {
	  _work_le[k][_work_cols-1] = 0;
	} 
      }
      _work_eq[_work_eq_rows][j+_first_symbol] += node->Coeff;
    }
  }

  // now do the coeff
  for (j=0; j<coeff->_num_dim; j++) {
    _work_eq[_work_eq_rows][j+coeff->_first_var] = coeff->_coeff[j];
  }
  _work_eq_rows++;
  return(TRUE);
}


// Copy into the work array the constraints associated with a call
//    coeff + lb <= 0 (if ub, = rather than <= otherwise)
//    coeff - ub <= 0
// If there is a stride, also put in a constraint for it
// is_first_ref is true if this is the first reference, false if the second
// Return FALSE on failure (out of space)
BOOL DEPV_COMPUTE::Copy_Call_To_Work(REGION_UN &image,
	SYMBOL_STACK *symbol_stack, DEPV_COEFF *coeff, BOOL is_first_ref) 
{
  REGION_ITER region_iter(&image);
  const REGION *first = region_iter.First();
  INT num_dim = first->_dim;

  for (const REGION *region_node=first; !region_iter.Is_Empty(); 
				region_node = region_iter.Next()) {
  if (!region_node->Is_All() && !region_node->Is_Too_Messy() && 
      !region_node->Is_Empty()) {
    for (INT i=0; i<num_dim; i++) {
      AXLE_NODE &axle = region_node->_axle[i];
      if (axle.step < 0) {
        Is_True(0, ("Negative strides not supported"));
	return FALSE;
      }
      CON_PAIR *lo = axle.lo;
      CON_PAIR *up = axle.up;
      ACCESS_VECTOR *av_low = lo->Access_Vector();
      if (lo->_coeff) {
	for (INT c=0; c<num_dim; c++) {
	  coeff->_coeff[c] = lo->_coeff[c];
        }
      } else {
	for (INT c=0; c<num_dim; c++) {
	  coeff->_coeff[c] = 0;
        }
	coeff->_coeff[i] = -1;
      }

      if (up) { // lower bound is a bound
        ACCESS_VECTOR *av_up = up->Access_Vector();
        if (axle.step > 1) {
	  if (!Copy_Stride_To_Work(av_low,coeff,axle.step,
						symbol_stack,is_first_ref)) {
	    return FALSE;
          }
        }
	Copy_Le_To_Work(av_low,coeff,symbol_stack,is_first_ref,FALSE);
        if (up->_coeff) {
	  for (INT c=0; c<num_dim; c++) {
	    coeff->_coeff[c] = up->_coeff[c];
          }
        } else {
	  for (INT c=0; c<num_dim; c++) {
	    coeff->_coeff[c] = 0;
          }
	  coeff->_coeff[i] = 1;
        }
	Copy_Le_To_Work(av_up,coeff,symbol_stack,is_first_ref,TRUE);
      } else {
	Copy_Equal_To_Work(av_low,coeff,symbol_stack,is_first_ref);
      }
    }
   }
  }
  return TRUE;
}

// Copy into the work array the constraints dim = ref
// This is used when one of the references is a call, and the other is an
// array reference
// is_first_ref is true if this is the first reference, false if the second
// Return FALSE on failure (out of space)
BOOL DEPV_COMPUTE::Copy_Call_Ref_To_Work(ACCESS_ARRAY *aa,
	DEPV_COEFF *coeff, SYMBOL_STACK *symbol_stack, BOOL is_first_ref) 
{
  INT num_dim = coeff->_num_dim;
  for (INT c=0; c<num_dim; c++) {
    coeff->_coeff[c] = 0;
  }
  if (!aa->Too_Messy) {
    for (INT i=0; i<coeff->_num_dim; i++) {
      ACCESS_VECTOR *av = aa->Dim(i);
      coeff->_coeff[i] = -1;
      if (!Copy_Equal_To_Work(av,coeff,symbol_stack,is_first_ref)) {
	coeff->_coeff[i] = 0;
	return FALSE;
      }
      coeff->_coeff[i] = 0;
    }
  }
  return TRUE;
}

// Add in the constraint av + coeff + stride1*dummy = 0
BOOL DEPV_COMPUTE::Copy_Stride_To_Work(ACCESS_VECTOR *av, DEPV_COEFF *coeff,
		INT stride, SYMBOL_STACK *symbol_stack, BOOL is_first_ref) 
{
  INT j = Find_Enter_Symbol(symbol_stack,NULL);
  _work_cols=j+1+_first_symbol;
  if(_work_cols > MAX_COLS) {
    Is_True(0, ("Column Overflow in DEPV_COMPUTE::Copy_Stride_To_Work"));
    return(FALSE);
  }
  INT k;
  for (k=0; k<=_work_eq_rows; k++) {
    _work_eq[k][_work_cols-1] = 0;
  }
  for (k=0; k<=_work_le_rows; k++) {
    _work_le[k][_work_cols-1] = 0;
  }

  if (!Copy_Equal_To_Work(av,coeff,symbol_stack,is_first_ref)) {
    return(FALSE);
  }
  _work_eq[_work_eq_rows-1][_work_cols-1] = -stride;
  return TRUE;
}


// If not negate_av Copy to work_le the constraint av + coeff <= 0
// If negate_av -av + coeff <=0
BOOL DEPV_COMPUTE::Copy_Le_To_Work(ACCESS_VECTOR *av, DEPV_COEFF *coeff,
	SYMBOL_STACK *symbol_stack, BOOL is_first_ref, BOOL negate_av) 
{
  if (av->Too_Messy) return TRUE;
  if(_work_le_rows >= MAX_ROWS) {
    Is_True(0,("Row Overflow in DEPV_COMPUTE::Copy_Le_To_Work"));
    return(FALSE);
  }
  if (negate_av) {
    _work_le_const[_work_le_rows] = av->Const_Offset;
  } else {
    _work_le_const[_work_le_rows] = -av->Const_Offset;
  }

  INT j;
  for (j=0; j<_work_cols; j++) {
    _work_le[_work_le_rows][j] = 0;
  }

  // do the loop variables
  if (is_first_ref) {
    if (av->Has_Loop_Coeff()) {
      for (INT j=0; j<_nd1; j++) {
        if (negate_av) {
          _work_le[_work_le_rows][j] = -av->Loop_Coeff(j);
        } else {
          _work_le[_work_le_rows][j] = av->Loop_Coeff(j);
        }
      }
    } 
  } else {
    if (av->Has_Loop_Coeff()) {
      INT j;
      for (j=0; j<_first_dv1; j++) {
        if (negate_av) {
          _work_le[_work_le_rows][j] = -av->Loop_Coeff(j);
	} else {
          _work_le[_work_le_rows][j] = av->Loop_Coeff(j);
        }
      }
      for (j=_nd1; j<_first_symbol; j++) {
        if (negate_av) {
          _work_le[_work_le_rows][j] = -av->Loop_Coeff(j-_nd1+_first_dv1);
	} else {
          _work_le[_work_le_rows][j] = +av->Loop_Coeff(j-_nd1+_first_dv1);
        }
      }
    } 
  }

  if (av->Contains_Lin_Symb()) {
    INTSYMB_ITER iter(av->Lin_Symb);
    for (INTSYMB_NODE *node = iter.First(); !iter.Is_Empty(); 
							node=iter.Next()) {
      INT j = Find_Enter_Symbol(symbol_stack,&node->Symbol);
      if (j+_first_symbol >= _work_cols) {  // a new symbol
        _work_cols=j+1+_first_symbol;
        if(_work_cols > MAX_COLS) {
	  Is_True(0, ("Column Overflow in DEPV_COMPUTE::Copy_Le_To_Work"));
	  return(FALSE);
	}
        INT k;
	for (k=0; k<=_work_le_rows; k++) {
	  _work_le[k][_work_cols-1] = 0;
	} 
	for (k=0; k<=_work_le_rows; k++) {
	  _work_le[k][_work_cols-1] = 0;
	} 
      }
      if (negate_av) {
        _work_le[_work_le_rows][j+_first_symbol] -= node->Coeff;
      } else {
        _work_le[_work_le_rows][j+_first_symbol] += node->Coeff;
      }
    }
  }

  // now do the coeff
  for (j=0; j<coeff->_num_dim; j++) {
    _work_le[_work_le_rows][j+coeff->_first_var] = coeff->_coeff[j];
  }
  _work_le_rows++;
  return(TRUE);
}



// Copy all the bounds constraints into the work array
// We use a work array so we can get rid of unused variables
// Put all the symbols into symbol_stack
// This routine is free to conservatively ignore any bounds it wants
// Return FALSE on failure (out of space)
BOOL DEPV_COMPUTE::Copy_Bounds_To_Work(const DOLOOP_STACK *s1,
	const DOLOOP_STACK *s2, SYMBOL_STACK *symbol_stack)
{

  // We mark the row positions of the first and last constraints for
  // direction-vector variables for reference 1
  // We'll later just copy (with a column shift) these constraints to get
  // the bounds for the corresponding variables for reference 2
  INT first_dv_bound=0; 
  INT last_dv_bound=-1; // last bound for a dv variable for ref1

  // first do the bounds for reference 1
  INT i;
  for (i=0; i<_first_dv2; i++) {
    if (i==_first_dv1) {
      first_dv_bound = _work_le_rows;
    } 
    
    DO_LOOP_INFO *dli = (DO_LOOP_INFO *)
      WN_MAP_Get(LNO_Info_Map,s1->Bottom_nth(i));

    INT step=_step1[i];

    // first the lower bounds (could be more than one if max)
    if (step) {
      ACCESS_ARRAY *lb,*ub;
      lb = dli->LB;
      ub = dli->UB;
      INT j;
      for (j=0; j<lb->Num_Vec(); j++) {
        ACCESS_VECTOR *lbv = lb->Dim(j);
        if (!lbv->Too_Messy && !lbv->Contains_Non_Lin_Symb() &&
	     (lbv->Non_Const_Loops() <= _first_dv1)) { // it's a good bound
	  if (!Copy_Bound_To_Work(i,lbv,symbol_stack,TRUE)) return(FALSE);
        }
      }

      // now the upper bounds 
      for (j=0; j<ub->Num_Vec(); j++) {
        ACCESS_VECTOR *ubv = ub->Dim(j);
        if (!ubv->Too_Messy && !ubv->Contains_Non_Lin_Symb() &&
	     (ubv->Non_Const_Loops() <= _first_dv1)) { // it's a good bound
	  if (!Copy_Bound_To_Work(i,ubv,symbol_stack,TRUE)) return(FALSE);
        }
      }
    }

    if (i == _first_non_com1-1) { // this is the last dv variable
      if (_first_non_com1 > _first_dv1) {  // there was a dv variable
        last_dv_bound = _work_le_rows-1;
      }
    }
  }

  // now do the bounds for the common dv_dim loops of reference 2
  // (these can be determined solely from the bounds for reference 1)
  if (_work_le_rows +(last_dv_bound-first_dv_bound+1) > MAX_ROWS) {
    Is_True(0,("Row overflow in DEPV_COMPUTE::Copy_Bounds_To_Work"));
    return FALSE;
  }
  for (i=first_dv_bound; i<= last_dv_bound; i++) {
    _work_le_const[_work_le_rows+i-first_dv_bound] = _work_le_const[i];
    INT j;
    for (j=0; j<_first_dv1; j++) {
      _work_le[_work_le_rows+i-first_dv_bound][j] = _work_le[i][j];
    }
    for (j=_first_dv1; j<_first_dv2; j++) {
      _work_le[_work_le_rows+i-first_dv_bound][j] = 0;
    }
    for (j=_first_dv2; j<_first_symbol; j++) {
      _work_le[_work_le_rows+i-first_dv_bound][j] = 
				_work_le[i][j-_first_dv2+_first_dv1];
    }
    for (j=_first_symbol; j<_work_cols; j++) {
      _work_le[_work_le_rows+i-first_dv_bound][j] = _work_le[i][j];
    }
  }
  _work_le_rows += (last_dv_bound-first_dv_bound+1);

  // now do the bounds for the non-common loops of reference 2
  for (i=_first_non_com2; i<_first_symbol; i++) {
    DO_LOOP_INFO *dli = (DO_LOOP_INFO *)WN_MAP_Get(
      LNO_Info_Map,s2->Bottom_nth(i-_first_non_com2+_first_non_com1));

    INT step=_step2[i-_first_non_com2+_first_non_com1];
    if (step) {
      ACCESS_ARRAY *lb,*ub;
      lb = dli->LB;
      ub = dli->UB;
      INT j;
      for (j=0; j<lb->Num_Vec(); j++) {
        ACCESS_VECTOR *lbv = lb->Dim(j);
        if (!lbv->Too_Messy && !lbv->Contains_Non_Lin_Symb() &&
	     (lbv->Non_Const_Loops() <= _first_dv1)) { // it's a good bound
	  if (!Copy_Bound_To_Work(i,lbv,symbol_stack,FALSE)) return(FALSE);
        }
      }

      for (j=0; j<ub->Num_Vec(); j++) {
        ACCESS_VECTOR *ubv = ub->Dim(j);
        if (!ubv->Too_Messy && !ubv->Contains_Non_Lin_Symb() &&
	     (ubv->Non_Const_Loops() <= _first_dv1)) { // it's a good bound
	 if (!Copy_Bound_To_Work(i,ubv,symbol_stack,FALSE))return(FALSE);
        }
      }
    }
  }
  return(TRUE);
}

// Copy a bound into the work array
// var_num is the variable whose bound we're copying
// bound is the bound
// if is_ref1 is true, we're computing a bound for reference 1, otherwise
//   we're computing a bound for reference 2 (we need to know since the
//   column mappings are different for the two cases)
// return FALSE on failure (overflow) 
BOOL DEPV_COMPUTE::Copy_Bound_To_Work(INT var_num, ACCESS_VECTOR *bound,
    SYMBOL_STACK *symbol_stack, BOOL is_ref1)
{
  if (_work_le_rows > MAX_ROWS) {
    Is_True(0,("Row overflow in DEPV_COMPUTE::Copy_Bound_To_Work"));
    return FALSE;
  }  

  // constant portions
  _work_le_const[_work_le_rows] = bound->Const_Offset;

  // trapezoidal portions
  if (is_ref1) {
    if (bound->Has_Loop_Coeff()) {
      for (INT i=0; i <= var_num; i++) {
	_work_le[_work_le_rows][i] = bound->Loop_Coeff(i);
      }
    } else {
      for (INT i=0; i <= var_num; i++) {
        _work_le[_work_le_rows][i] = 0;
      }
    }
    for (INT i=var_num+1; i<_work_cols; i++) {
      _work_le[_work_le_rows][i] = 0;
    }
  } else {  // is reference 2
    if (bound->Has_Loop_Coeff()) {
      INT i;
      for (i=0; i < _first_dv1; i++) {
	_work_le[_work_le_rows][i] = bound->Loop_Coeff(i);
      }
      for (i=_first_dv1; i<_first_dv2; i++) {
        _work_le[_work_le_rows][i] = 0;
      }
      for (i=_first_dv2; i<=var_num; i++) {
	_work_le[_work_le_rows][i]=bound->Loop_Coeff(i-_first_dv2+_first_dv1);
      }
      for (i=var_num+1; i<_work_cols; i++) {
	_work_le[_work_le_rows][i] = 0;
      }
    } else {
      for (INT i=0; i < _work_cols; i++) {
        _work_le[_work_le_rows][i] = 0;
      }
    }
  }

  // symbolic portions
  if (bound->Contains_Lin_Symb()) {
    INTSYMB_ITER iter(bound->Lin_Symb);
    for (INTSYMB_NODE *node = iter.First();!iter.Is_Empty();node=iter.Next()) {
      INT i = Find_Enter_Symbol(symbol_stack,&node->Symbol);
      if (i+_first_symbol >= _work_cols) {  // a new symbol
        _work_cols=i+1+_first_symbol;
        if(_work_cols > MAX_COLS) {
	  Is_True(0,("Column Overflow in DEPV_COMPUTE::Copy_Bounds_To_Work"));
	  return(FALSE);
	}
        INT j;
	for (j=0; j<=_work_eq_rows; j++) {
	  _work_eq[j][_work_cols-1] = 0;
  	}
	for (j=0; j<=_work_le_rows; j++) {
	  _work_le[j][_work_cols-1] = 0;
  	}
      }
      _work_le[_work_le_rows][i+_first_symbol] += node->Coeff;
    }
  }
  _work_le_rows++;
  return(TRUE);
}

// Find the constant distances, this will be an initial value for the result 
// Find an init valued for is_used (mark all variables used in a ref as used)
// For the distance vector variables, mark a conjugate of a used variable
//    as used
// dv_dim is the number of dimensions of the resultant distance vector
// Retun DEP_INDEPENDENT if system proved independent, DEP_CONTINUE otherwise
DEP_RESULT DEPV_COMPUTE::Find_Init_Distance_Used(DEPV *init_distance,
			BOOL *is_used, INT dv_dim) const
{
  INT i;
  for (i=0; i<_work_cols; i++) {
    is_used[i] = FALSE;
  }

  for (i=0; i<dv_dim; i++) {
    DEPV_Dep(init_distance,i) = DEP_SetDirection(DIR_STAR);
  }


  for (i=0; i<_work_eq_rows; i++) {
    INT num_vars=0;
    INT first_var;
    for (INT j=0; j<_work_cols; j++) {
      if (_work_eq[i][j] != 0) {
	if (num_vars==0) {
	  first_var=j;
	}
	num_vars++;
	is_used[j] = TRUE;

	// is it a distance vector variable, if so, set its conjugate as well
        if ((j >= _first_dv1) && (j < _first_non_com1)) {
	  is_used[_first_dv2+j-_first_dv1] = TRUE;
	} else if ((j >= _first_dv2) && (j < _first_non_com2)) {
	  is_used[_first_dv1+j-_first_dv2] = TRUE;
	}
      }
    }
    // is this constraint a constant distance constraint
    if (num_vars == 2)  {
      if ((first_var >= _first_dv1) && (first_var < _first_non_com1)) {
	INT corres_var= _first_dv2 + first_var-_first_dv1;
	if (_work_eq[i][first_var] == -_work_eq[i][corres_var]) {
	  DEP dep;
	  if (_work_eq[i][first_var] == 1) {
	    dep = DEP_SetDistance(-_work_eq_const[i]);
	  } else if (_work_eq[i][first_var] == -1) {
	    dep = DEP_SetDistance(_work_eq_const[i]);
	  } else if ((_work_eq_const[i] % _work_eq[i][first_var]) == 0) {
	    dep = DEP_SetDistance(-_work_eq_const[i]/_work_eq[i][first_var]);
	  } else {
	    return(DEP_INDEPENDENT); // simple gcd test failure
	  }
	  DEP current_dep = DEPV_Dep(init_distance,first_var-_first_dv1);
	  if (DEP_Direction(current_dep) == DIR_STAR) {
		DEPV_Dep(init_distance,first_var-_first_dv1) = dep;
	  } else if (DEP_Distance(current_dep) != DEP_Distance(dep)) {
	    return(DEP_INDEPENDENT); // two contradictory dependences
	  }
	}
      }
    }
  }

  // For calls, we might have constraints inside _work_le, 
  // mark all such variables as used
  for (i=0; i<_work_le_rows; i++) {
    for (INT j=0; j<_work_cols; j++) {
      if (_work_le[i][j] != 0) {
	is_used[j] = TRUE;

	// is it a distance vector variable, if so, set its conjugate as well
        if ((j >= _first_dv1) && (j < _first_non_com1)) {
	  is_used[_first_dv2+j-_first_dv1] = TRUE;
	} else if ((j >= _first_dv2) && (j < _first_non_com2)) {
	  is_used[_first_dv1+j-_first_dv2] = TRUE;
	}
      }
    }
  }
  return(DEP_CONTINUE);
}

// on input, is_used[i] = TRUE iff variable i (or its conjugate) 
//   is used in work_eq 
//
// For non-symbols this routine sets is_used[i] to TRUE if i is used in a bound
// of a used variable..
// i is used in a bound if there exists a variable, i' <> i, such
// that i is used in the bound of i'.
//
// For symbols, we use a version of the acyclic test.
// If a symbol is not used in an equals constraint, and is only used
// with either a pos or a neg coefficient (but not both), we zap it
// and all bounds using it.
// This is because when we have things like do i=1,n, and 'n' doesn't
// appear anywhere else, 'n' does not affect any solution
//
// This routine then sets bounds_used[i] to TRUE iff a bound equation uses
// a used variable
// Also set num_bounds_used to the amount of bounds equations used
//
void DEPV_COMPUTE::Bounds_Set_Is_Used(BOOL *is_used,BOOL *bounds_used, 
			INT *num_bounds_used) const
{
  mBOOL *uses_zapped_symbol=CXX_NEW_ARRAY(mBOOL,_work_le_rows,&LNO_local_pool);

  // first set is_used

  // first the symbols
  INT j;
  for (j=_first_symbol; j<_work_cols; j++) {
    if (!is_used[j]) {
      BOOL seen_pos=FALSE;
      BOOL seen_neg = FALSE;
      for (INT i=0; i<_work_le_rows; i++) {
        if (_work_le[i][j] < 0) {
	  seen_neg = TRUE;
	} else if (_work_le[i][j] > 0) {
	  seen_pos = TRUE;
	}
      }
      if (seen_pos && seen_neg) {
	is_used[j] = TRUE;
      }
    }
  }

  INT i;
  for (i=0; i<_work_le_rows; i++) {
    bounds_used[i] = FALSE;
    uses_zapped_symbol[i] = FALSE;
    for (INT j=_first_symbol; j<_work_cols; j++) {
      if (!is_used[j] && _work_le[i][j]) {
	uses_zapped_symbol[i] = TRUE;
	break;
      }
    }
  }

  // now the index variables
  // This assumes that the bounds are arranged from outer loop variables
  // down to inner loop variables.  This means that if we're visiting
  // a bound for variable 'x' and x is not used, it's not possible that
  // a later (smaller row number) constraint will make 'x' become used.
  // if this assumption ever becomes not true, the code will conservatively
  // mark too many variables as not used
  for (i=_work_le_rows-1; i>=0; i--) {
    if (!uses_zapped_symbol[i]) {  // iff this constraint uses a zapped
				  // symbol, then acyclic says its useless
      INT j=_first_symbol-1;
      while (_work_le[i][j] == 0) j--;  // This is the variable being bounded
      if (is_used[j]) { 
        for (j=j-1; j>=0;j--) {
	  if (!is_used[j]) {
            if (_work_le[i][j] != 0) {
	      is_used[j] = TRUE;
	      // is it a distance vector variable, if so, set conjugate as well
              if ((j >= _first_dv1) && (j < _first_non_com1)) {
	        is_used[_first_dv2+j-_first_dv1] = TRUE;
	      } else if ((j >= _first_dv2) && (j < _first_non_com2)) {
	        is_used[_first_dv1+j-_first_dv2] = TRUE;
	      }
	    }
          }
        }
      }
    }
  }


  // Now set bounds_used
  // A bound is used if the variable being bounded is a used variable
  // and if it doesn't refer to a zapped symbol
  *num_bounds_used=0;
  for (i=0; i<_work_le_rows; i++) {
    if (!uses_zapped_symbol[i]) {
      j=_first_symbol-1;
      while (_work_le[i][j] == 0) j--;  // This is the variable being bounded
      if (is_used[j]) {
	bounds_used[i] = TRUE;
        (*num_bounds_used)++;
      }
    }
  }
}



// Set map_used and num_vars_used
// if 'i' is the n'th (starting at zero) used variable then map_used[i] = n
// Otherwise, map_used is undefined
void DEPV_COMPUTE::Set_Map_Used(const BOOL *is_used,INT *num_vars_used,
						INT *map_used) const
{
  *num_vars_used=0;
  for (INT i=0; i<_work_cols; i++) {
    if (is_used[i]) {
      map_used[i] = (*num_vars_used)++;
    }
  }
}
	      
// Copy the constraints to the soe
// mapping away the unused variables and unused bounds constraints
void DEPV_COMPUTE::Copy_To_Soe(const BOOL *is_used,const BOOL *bounds_used,
		       const INT *map_used, SYSTEM_OF_EQUATIONS *soe) const
{
  // first the equality constraint
  INT64 *Beq = soe->Beq();
  for (INT j=0; j<_work_cols; j++) {
    if (is_used[j]) {
      INT map_col = map_used[j];
      for (INT i=0; i<_work_eq_rows; i++) {
	soe->Aeq()(i,map_col) = _work_eq[i][j];
      }
    }
  }
  INT i;
  for (i=0; i<_work_eq_rows; i++) {
    Beq[i] = _work_eq_const[i];
  }

  // now the inequality constraints
  INT64 *Ble = soe->Ble();
  INT i2 = 0;
  for (i=0; i<_work_le_rows; i++) {
    if (bounds_used[i]) {
      Ble[i2] = _work_le_const[i];
      for (INT j=0; j<_work_cols; j++) {
        if (is_used[j]) {
	  soe->Ale()(i2,map_used[j]) = _work_le[i][j];
	}
      }
      i2++;
    }
  }
}

// Find all the dependence vectors
// This is basically Burke-Cytron's algorithm
// Find the first star in input_dependence (looking only at used variables)
// If there isn't any stars, place input_dependence on output stack and return
// Replace star with -,+,= in sequence
// For each replacement, check consistency
// If inconsistent, return
// If consistent, call this routine recursively
//
// input_dependence contains all the dependence variables (even the unused
// ones)
//
// same reference is TRUE iff we're computing a dependence from a ref
// to itself (we need to know this since we don't add all equals deps
// in this case)
void DEPV_COMPUTE::Compute_Dep_Vectors(SYSTEM_OF_EQUATIONS *soe,
		const INT *is_used, const INT *map_used, 
		DEPV *input_dependence,DEPV_LIST *result,
		BOOL same_reference, INT dv_dim,INT num_bad_outer) 
{	
  BOOL same = same_reference;

  INT i=First_Star(input_dependence,is_used);
  if (i == -1) {
    if (num_bad_outer == 0) {
      result->Append(
        CXX_NEW(DEPV_NODE(DEPV_Copy(_pool,input_dependence,dv_dim)),
							_pool));
    } else {
      result->Append( DEPV_Copy(_pool,input_dependence,dv_dim),
						num_bad_outer);
    }
    return;
  }

  Add_Direction(soe,i,map_used,DIR_POS);
  DEPV_Dep(input_dependence,i) = DEP_SetDirection(DIR_POS);

  if (debug >= 2) {
    fprintf(TFile,"trying the direction "); 
    DEPV_Print(input_dependence,TFile,dv_dim); 
    fprintf(TFile,"\n");
  }
  if (soe->Is_Consistent()) {
    Compute_Dep_Vectors(soe,is_used,map_used,input_dependence,result,same,
				dv_dim,num_bad_outer);
  }
  soe->Remove_Last_Le();
  DEPV_Dep(input_dependence,i) = DEP_SetDirection(DIR_STAR);

  // before doing the equal make sure not an all equals from a ref to itself
  if (!same || (i != (dv_dim -1)) || 
	!Is_All_Equals(input_dependence,dv_dim-1)) {
    Add_Direction(soe,i,map_used,DIR_EQ);
    DEPV_Dep(input_dependence,i) = DEP_SetDirection(DIR_EQ);

    if (debug >= 2) {
      fprintf(TFile,"trying the direction "); 
      DEPV_Print(input_dependence,TFile,dv_dim); 
      fprintf(TFile,"\n");
    }
    if (soe->Is_Consistent()) {
      Compute_Dep_Vectors(soe,is_used,map_used,input_dependence,result,same,
				dv_dim,num_bad_outer);
    }
    DEPV_Dep(input_dependence,i) = DEP_SetDirection(DIR_STAR);
    soe->Remove_Last_Eq();
  }

  Add_Direction(soe,i,map_used,DIR_NEG);
  DEPV_Dep(input_dependence,i) = DEP_SetDirection(DIR_NEG);

  if (debug >= 2) {
    fprintf(TFile,"trying the direction "); 
    DEPV_Print(input_dependence,TFile,dv_dim); 
    fprintf(TFile,"\n");
  }
  if (soe->Is_Consistent()) {
    Compute_Dep_Vectors(soe,is_used,map_used,input_dependence,result,same,
				dv_dim,num_bad_outer);
  }
  soe->Remove_Last_Le();
  DEPV_Dep(input_dependence,i) = DEP_SetDirection(DIR_STAR);
}


// Add the direction constraint into the SOE
void DEPV_COMPUTE::Add_Direction(SYSTEM_OF_EQUATIONS *soe, INT i,
		const INT *map_used, DIRECTION direction)
{
  for (INT j=0; j<soe->Num_Vars(); j++) {
    _tmp[j] = 0;
  }

  if (direction == DIR_EQ) {
    _tmp[map_used[_first_dv1+i]] = 1;
    _tmp[map_used[_first_dv2+i]] = -1;
    soe->Add_Eq(_tmp,0);
  } else if (direction == DIR_NEG) {
    _tmp[map_used[_first_dv1+i]] = -1;
    _tmp[map_used[_first_dv2+i]] = 1;
    soe->Add_Le(_tmp,-1);
  } else if (direction == DIR_POS) {
    _tmp[map_used[_first_dv1+i]] = 1;
    _tmp[map_used[_first_dv2+i]] = -1;
    soe->Add_Le(_tmp,-1);
  } else {
    Is_True(0,("Illegal direction in DEPV_COMPUTE::Add_Direction"));
  }
}


// Find the first star in the dependence vector
// Only look at used variables
// Return -1 if there aren't any
INT DEPV_COMPUTE::First_Star(const DEPV *vector, const INT *is_used) const
{
  for (INT i=_first_dv1; i<_first_non_com1; i++) {
    if (is_used[i]) {
      if (DEP_Direction(DEPV_Dep(vector,i-_first_dv1)) == DIR_STAR) {
	return(i-_first_dv1);
      }
    }
  }
  return(-1);
}

void DEPV_COMPUTE::Print(FILE *fp) const
{
  fprintf(fp,"_nd1,_nd2, = %d %d \n",_nd1,_nd2);
  fprintf(fp,"_first_dv1,_first_non_com1 = %d %d \n",_first_dv1,
	     _first_non_com1);
  fprintf(fp,"_first_dv2,_first_non_com2 = %d %d \n",_first_dv2,
	     _first_non_com2);
  fprintf(fp,"_first_symbol is %d \n",_first_symbol);
  fprintf(fp,"work_eq is \n");
  INT i;
  for (i=0; i<_work_eq_rows; i++) {
    for (INT j=0; j<_work_cols; j++) {
      fprintf(fp," %d ",_work_eq[i][j]);
    }
    fprintf(fp,"    %lld \n",_work_eq_const[i]);
  }
  fprintf(fp,"work_le is \n");
  for (i=0; i<_work_le_rows; i++) {
    for (INT j=0; j<_work_cols; j++) {
      fprintf(fp," %d ",_work_le[i][j]);
    }
    fprintf(fp,"    %lld \n",_work_le_const[i]);
  }
}

// Set an array of the step sizes
// if not constant set the step to 0
void DEPV_COMPUTE::Set_Step(const DOLOOP_STACK *s1, const DOLOOP_STACK *s2)
{
  _step1 = CXX_NEW_ARRAY(INT64,s1->Elements(),_pool);
  _step2 = CXX_NEW_ARRAY(INT64,s2->Elements(),_pool);
  INT i;
  for (i=0; i<s1->Elements(); i++) {
    DO_LOOP_INFO *dli = (DO_LOOP_INFO *)
      	WN_MAP_Get(LNO_Info_Map,s1->Bottom_nth(i));
    ACCESS_VECTOR *step = dli->Step;
    if (!step->Is_Const()) {
      _step1[i] = 0;
    } else {
      _step1[i] = step->Const_Offset;
    }
  }
  for (i=0; i<s2->Elements(); i++) {
    DO_LOOP_INFO *dli = (DO_LOOP_INFO *)
      	WN_MAP_Get(LNO_Info_Map,s2->Bottom_nth(i));
    ACCESS_VECTOR *step = dli->Step;
    if (!step->Is_Const()) {
      _step2[i] = 0;
    } else {
      _step2[i] = step->Const_Offset;
    }
  }
}
    

//Is the dependence vector all equals
static BOOL Is_All_Equals(DEPV *depv, INT dv_dim)
{
  for (INT i=0; i<dv_dim; i++) {
    if (DEP_Direction(DEPV_Dep(depv,i)) != DIR_EQ) {
      return(FALSE);
    }
  }
  return TRUE;
}

// Are the two expressions equivalent and loop invariant
// Conservatvie to return FALSE
static BOOL Same_Invariant_Expression(WN *wn1, WN *wn2, WN *loop) 
{
  if (!WN_Equiv (wn1, wn2)) return FALSE;
  OPCODE opc = WN_opcode(wn1);
  if (OPCODE_is_load(opc)) {
    OPERATOR oper = OPCODE_operator(opc);
    if (oper != OPR_LDID) return FALSE;
    return Is_Loop_Invariant_Use(wn1,loop);
  }
  for (INT kidno=0; kidno<WN_kid_count(wn1); kidno++) {
    if (!Same_Invariant_Expression(WN_kid(wn1,kidno),
		WN_kid(wn2,kidno),loop)) {
      return FALSE;
    }
  }
  return TRUE;
}

static BOOL Add_Or_Subtract(OPCODE opc) 
{
  OPERATOR opr = OPCODE_operator(opc);
  if ((opr == OPR_ADD) || (opr == OPR_SUB)) {
    return TRUE;
  } 
  return FALSE;
}

// Are these two references indirects (or indirects plus/minus a constant expression)
// off of the same permuation array.  If so, we essentially replace the
// ref with the index expressio for the permutation array
// If true, set perm1 and perm2 to the access array for the indirect array
//
// Conservatvie to return FALSE
BOOL DEPV_COMPUTE::Same_Permutation(WN *ref1, WN *ref2, ACCESS_ARRAY **perm1,
	  ACCESS_ARRAY **perm2, WN *outer_loop)
{
  OPCODE opc1 = WN_opcode(ref1);
  OPCODE opc2 = WN_opcode(ref2);
  if (opc1 != opc2) return FALSE;

  // check for adding or subtracting constants
  if (Add_Or_Subtract(opc1)) {
    WN *kid01 = WN_kid0(ref1);
    WN *kid02 = WN_kid0(ref2);
    WN *kid11 = WN_kid1(ref1);
    WN *kid12 = WN_kid1(ref2);
    if (Same_Invariant_Expression(kid01,kid02,outer_loop)) {
      return Same_Permutation(kid11,kid12,perm1,perm2,outer_loop);
    } else if (Same_Invariant_Expression(kid11,kid12,outer_loop)) {
      return Same_Permutation(kid01,kid02,perm1,perm2,outer_loop);
    } else {
      return FALSE;
    }
  } else if (OPCODE_operator(opc1) == OPR_NEG) {
      return Same_Permutation(WN_kid0(ref1),WN_kid0(ref2),perm1,perm2,outer_loop);
  }
  if (!OPCODE_is_load(opc1) || !WN_kid_count(ref1)) return FALSE;
  if (WN_offset(ref1) != WN_offset(ref2)) return FALSE;
  WN *array1 = WN_kid0(ref1);
  WN *array2 = WN_kid0(ref2);
  OPCODE array_opc1 = WN_opcode(array1);
  OPCODE array_opc2 = WN_opcode(array2);
  if (array_opc1 != array_opc2 || (OPCODE_operator(array_opc1) != OPR_ARRAY)) {
    return FALSE;
  }
  WN *base1 = WN_array_base(array1);
  WN *base2 = WN_array_base(array2);
  if (WN_offset(base1) != WN_offset(base2)) return FALSE;
  if (WN_st(base1) != WN_st(base2)) return FALSE;

  if (Base_Test(ref1,NULL,ref2,NULL) != DEP_CONTINUE) return FALSE;

  // now we know we have a good indirection, but is it a permutation
  ST *st = WN_st(base1);
  for (INT i=0; i<Permutation_Arrays->Elements(); i++) {
    if (st == Permutation_Arrays->Bottom_nth(i)._st) {
      if (Permutation_Arrays->Bottom_nth(i)._is_good) {
	*perm1 = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array1);
	*perm2 = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array2);
	return TRUE;
      } else {
	return FALSE;
      }
    }
  }
  return FALSE;
}

void DEPV_COMPUTE::Print_Work(FILE *fp)
{
  fprintf(fp,"work_le,const_le is \n");
  INT32 i;
  for (i=0; i<_work_le_rows; i++) {
    for (INT32 j=0; j<_work_cols; j++) {
      fprintf(fp," %d ",_work_le[i][j]);
    }
    fprintf(fp,"    %lld \n",_work_le_const[i]);
  }
  fprintf(fp,"\n");
  fprintf(fp,"work_eq, const_eq is \n");
  for (i=0; i<_work_eq_rows; i++) {
    for (INT32 j=0; j<_work_cols; j++) {
      fprintf(fp," %d ",_work_eq[i][j]);
    }
    fprintf(fp,"    %lld \n",_work_eq_const[i]);
  }
  fprintf(fp,"\n");
}



	


// statics
mINT32 DEPV_COMPUTE::_work_eq[MAX_ROWS][MAX_COLS];
mINT32 DEPV_COMPUTE::_tmp[MAX_COLS];
INT64 DEPV_COMPUTE::_work_eq_const[MAX_ROWS];
mINT32 DEPV_COMPUTE::_work_le[MAX_ROWS][MAX_COLS];
INT64 DEPV_COMPUTE::_work_le_const[MAX_ROWS];




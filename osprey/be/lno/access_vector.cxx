/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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

// ====================================================================
// ====================================================================
//
// Module: access_vector.cxx
// $Revision: 1.8 $
// $Date: 04/12/29 20:05:39-08:00 $
// $Author: kannann@iridot.keyresearch $
//
// Revision history:
//  dd-mmm-94 - Original Version
//
// Description:		Access Vectors
//
// This is the basic data structure used to represent array
// expressions.  It allows us to succintly represent the locations
// accessed by array instructions, in terms of loop and symbolic
// variables.
//
// ====================================================================
// ====================================================================

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <stdint.h>
#include <sys/types.h>
#include <alloca.h>
#ifdef LNO
#include "lnopt_main.h"
#else
#include "mempool.h"
static MEM_POOL LNO_local_pool;
#endif
#include "mat.h"
#include "access_vector.h"
#include "stab.h"
#include "lwn_util.h"
#include "opt_du.h"
#include "soe.h"
#ifdef LNO
#include "lnoutils.h"
#include "move.h"
#include "errors.h"
#include "erbe.h"
#endif
#include "targ_sim.h"

#ifndef LNO
static DU_MANAGER *Du_Mgr;
extern void Initialize_Access_Vals(DU_MANAGER* du_mgr, FILE *tfile);
extern void Finalize_Access_Vals();
extern WN* LNO_Common_Loop(WN* wn1, WN* wn2);
extern WN* UBvar (WN *end);	/* In lieu of lnoutils.h for IPL */
template <> MEM_POOL* MAT<mINT32>::_default_pool = &LNO_local_pool;
// since wopt.so is loaded dynamically in ipl
#pragma weak Add_Def_Use__10DU_MANAGERGP2WNT1
#endif

#ifdef LNO
extern BOOL Is_Consistent_Condition(ACCESS_VECTOR*, WN*);
extern BOOL Is_Const_Array_Addr(WN *);
extern BOOL Is_Loop_Invariant_Use(WN *, WN *);
extern BOOL Is_Loop_Invariant_Indir(WN*);
#endif

// The following functions are used in place of snprintf(), which belongs 
// to a new version of <stdio.h>, but is not available in the version of 
// <stdio.h> which is available in this compiler. 

INT snprintfs(char* buf, 
	      INT ccount, 
              INT tcount, 
	      const char* fstring)
{
  INT len = strlen(fstring);
  if (ccount + len < tcount) {
    INT new_char_count = sprintf(buf + ccount, fstring);
    return ccount + new_char_count;
  } 
  for (INT i = 0; i < ccount; i++)
    sprintf(buf + i, "%c", '&');
  sprintf(buf + ccount, "%c", '\0');
  return tcount - 1;
} 

INT snprintfd(char* buf, 
	      INT ccount, 
              INT tcount, 
	      INT32 value)
{
  // A 32 bit integer can be expressed in 10 digits plus sign
  const INT int_size = 11;
  if (ccount + int_size < tcount) { 
    INT new_char_count = sprintf(buf + ccount, "%d", value);
    return ccount + new_char_count;
  } 
  for (INT i = 0; i < ccount; i++)
    sprintf(buf + i, "%c", '&');
  sprintf(buf + ccount, "%c", '\0');
  return tcount - 1;
} 

INT snprintfll(char* buf, 
	       INT ccount, 
	       INT tcount, 
	       INT64 value)
{ 
  // A 64 bit integer can be expressed in 20 digits plus sign
  const INT ll_size = 21;
  if (ccount + ll_size < tcount) {
    INT new_char_count = sprintf(buf + ccount, "%lld", value);
    return ccount + new_char_count;
  } 
  for (INT i = 0; i < ccount; i++)
    sprintf(buf + i, "%c", '&');
  sprintf(buf + ccount, "%c", '\0');
  return tcount - 1;
} 

INT snprintfx(char* buf, 
	      INT ccount, 
	      INT tcount, 
	      INT32 value)
{ 
  // A 32 bit integer can be expressed in 8 hexdigits plus "0x"
  const INT x_size = 10;
  if (ccount + x_size < tcount) {
    INT new_char_count = sprintf(buf + ccount, "0x%x", value);
    return ccount + new_char_count;
  } 
  for (INT i = 0; i < ccount; i++)
    sprintf(buf + i, "%c", '&');
  sprintf(buf + ccount, "%c", '\0');
  return tcount - 1;
} 

INT Num_Lands(WN *wn);
INT Num_Liors(WN *wn);

BOOL LNO_Debug_Delinearization;
BOOL LNO_Allow_Nonlinear = TRUE;

#define MAX_NAME_SIZE 66

INT Num_Lower_Bounds(WN* wn, 
			    ACCESS_VECTOR* step)
{ 
  INT num_bounds=0;
  WN *l = WN_start(wn);
  WN *kid = WN_kid(l,0);
  if (step->Const_Offset > 0) {
    if (WN_operator(kid) == OPR_MAX) { 
      num_bounds = Num_Maxs(kid);
    } else if (WN_operator(kid) == OPR_SUB) { 
      num_bounds = Num_Maxs(WN_kid0(kid));
    } else if (WN_operator(kid) == OPR_ADD) { 
      num_bounds = Num_Maxs(WN_kid0(kid));
      if (!num_bounds) num_bounds = Num_Maxs(WN_kid1(kid));
    }
  } else {
    if (WN_operator(kid) == OPR_MIN) { 
      num_bounds = Num_Mins(kid);
    } else if (WN_operator(kid) == OPR_SUB) { 
      num_bounds = Num_Mins(WN_kid0(kid));
    } else if (WN_operator(kid) == OPR_ADD) { 
      num_bounds = Num_Mins(WN_kid0(kid));
      if (!num_bounds) num_bounds = Num_Maxs(WN_kid1(kid));
    }
  }
  return num_bounds+1;
}

extern INT Num_Upper_Bounds(WN* wn)
{
  INT num_bounds = 0; 
  WN *u = WN_end(wn);
  OPERATOR compare = WN_operator(u);
  if ((compare == OPR_LE) || (compare == OPR_LT)) {
    num_bounds = Num_Mins(WN_kid(u,1));
    if (!num_bounds) num_bounds = Num_Maxs(WN_kid(u,0));
  } else {
    num_bounds = Num_Maxs(WN_kid(u,1));
    if (!num_bounds) num_bounds = Num_Mins(WN_kid(u,0));
  }
  return num_bounds+1; 
}

extern BOOL Bound_Is_Too_Messy(ACCESS_ARRAY* aa)
{
  if (aa->Too_Messy)
    return TRUE;
  for (INT i = 0; i < aa->Num_Vec(); i++)
    if (aa->Dim(i)->Too_Messy)
      return TRUE;
  return FALSE;
}

#ifdef LNO

extern BOOL Promote_Messy_Bound(WN* wn_loop, 
			        WN* wn_bound,
                                char name[],
                                DU_MANAGER* du)
{
  if (UBvar(WN_end(wn_loop)) == NULL)
    return FALSE; 
  WN* wn_parent = LWN_Get_Parent(wn_bound);
  INT i;
  for (i = 0; i < WN_kid_count(wn_parent); i++) 
    if (wn_bound == WN_kid(wn_parent, i))
      break;
  FmtAssert(i < WN_kid_count(wn_parent), ("Could not find kid for parent."));
  INT kid = i; 
  TYPE_ID type = WN_desc(WN_start(wn_loop));
  OPCODE preg_s_opcode = OPCODE_make_op(OPR_STID, MTYPE_V, type);
  WN_OFFSET preg_num = Create_Preg(type, name);
  ST* preg_st = MTYPE_To_PREG(type);
  WN* wn_stid = LWN_CreateStid(preg_s_opcode, preg_num, preg_st,
                               Be_Type_Tbl(type), wn_bound);
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_loop), wn_loop, wn_stid);
  WN* wn_ldid = LWN_CreateLdid(WN_opcode(UBvar(WN_end(wn_loop))), wn_stid);
  WN_kid(wn_parent, kid) = wn_ldid; 
  LWN_Set_Parent(wn_ldid, wn_parent); 
  du->Add_Def_Use(wn_stid, wn_ldid);
  INT hoist_level = Hoistable_Statement(wn_stid, du);
  if (hoist_level < Loop_Depth(wn_stid))
    Hoist_Statement(wn_stid, hoist_level);
  return TRUE; 
}

#endif 

void INDX_RANGE::Union(INT64 offset, BOOL offset_valid, INT64 mult, INT64 size)
{
  if (Valid) { // a true union
    if (Size == size) {
      if (abs(mult) > abs(Mult)) {
        Mult = mult;
	if (offset_valid) {
	  Min = Max = offset;
	  Min_Max_Valid = TRUE;
        } else {
	  Min_Max_Valid = FALSE;
        }
      } else if (mult == Mult) {
	if (offset_valid && Min_Max_Valid) {
	  Min = MIN(Min,offset);
	  Max = MAX(Max,offset);
        } else if (offset_valid) {
	  Min = Max = offset;
	  Min_Max_Valid = TRUE;
	}
      }
    } else {  // two different sized arrays
	      // chose the smaller
      if (abs((size+mult-1)/mult) < Maxsize()) {
        Mult = mult;
        Size = size;
        if (offset_valid) {
          Min = Max = offset;
          Min_Max_Valid = TRUE;
        } else {
          Min_Max_Valid = FALSE;
        }
      }
    }
  } else { // a set
    Valid = TRUE;
    Mult = mult;
    Size = size;
    if (offset_valid) {
      Min = Max = offset;
      Min_Max_Valid = TRUE;
    } else {
      Min_Max_Valid = FALSE;
    }
  }
}

INT64 INDX_RANGE::Maxsize() const 
{
  if (!Valid) return -1;
  INT diff = 0;
  if (Min_Max_Valid) {
    diff = Max-Min;
  }
  INT64 c = abs(Mult);
  INT64 ans = (Size - diff + (c-1))/c;
  return ans <= 0 ? -1 : ans;
}



void ACCESS_ARRAY::Print(FILE *fp, BOOL is_bound) const
{
  if (Too_Messy) {
    fprintf(fp,"Too_Messy\n");
    return;
  }

  for (INT32 i=0; i<_num_vec; i++) {
    Dim(i)->Print(fp,is_bound);
  }
  fprintf(fp,"\n");
}

mUINT16 ACCESS_ARRAY::Non_Const_Loops() const
{
  mUINT16 result = Dim(0)->Non_Const_Loops();
  for (INT32 i=1; i<_num_vec; i++) {
    result = MAX(result, Dim(i)->Non_Const_Loops());
  }
  return result;
}

// set an ACCESS_ARRAY to be a copy of another
ACCESS_ARRAY::ACCESS_ARRAY(const ACCESS_ARRAY *a, MEM_POOL *pool)
{
  _dim = NULL;
  Init(a,pool);
}

BOOL ACCESS_ARRAY::Has_Formal_Parameter()
{ 
  if (Too_Messy)
    return FALSE; 
  for (INT i = 0; i< _num_vec; i++) 
    if (Dim(i)->Has_Formal_Parameter())
      return TRUE; 
  return FALSE; 
} 

BOOL ACCESS_VECTOR::Has_Formal_Parameter()
{
  if (Too_Messy)
    return FALSE; 
  if (Lin_Symb != NULL) { 
    INTSYMB_ITER ii(Lin_Symb);
    for (INTSYMB_NODE* nn = ii.First(); !ii.Is_Empty(); nn = ii.Next()) 
      if (nn->Symbol.Is_Formal())
	return TRUE; 
  } 
  if (Non_Lin_Symb != NULL) {
    SUMPROD_ITER ii(Non_Lin_Symb);
    for (SUMPROD_NODE* nn = ii.First(); !ii.Is_Empty(); nn = ii.Next()) {
      SYMBOL_ITER iii(nn->Prod_List);
      for (SYMBOL_NODE* nnn = iii.First(); !iii.Is_Empty(); nnn = iii.Next()) 
	if (nnn->Symbol.Is_Formal())
	  return TRUE; 
    }
  }
  return FALSE; 
} 

void ACCESS_VECTOR::Substitute(INT formal_number, 
                               WN* wn_sub,
                               DOLOOP_STACK* stack,
                               BOOL allow_nonlin)
{
  if (Contains_Lin_Symb()) { 
    INTSYMB_ITER iter(Lin_Symb);
    INTSYMB_NODE* nnode = NULL;
    INTSYMB_NODE* node = iter.First();
    for (; !iter.Is_Empty(); nnode = node, node = iter.Next()) {
      if (node->Symbol.Is_Formal() 
	  && node->Symbol.Formal_Number() == formal_number) {
	if (wn_sub == NULL) {
	  Too_Messy = TRUE; 
	  return;
	} 
	OPERATOR opr_sub = WN_operator(wn_sub);
	if (opr_sub != OPR_LDID && opr_sub != OPR_LDA) { 
	  Too_Messy = TRUE; 
	  return; 
	} 
	SYMBOL sym_sub(WN_st(wn_sub), WN_offset(wn_sub) 
	  + node->Symbol.WN_Offset(), node->Symbol.Type);
        INT32 coeff = node->Coeff;
        Lin_Symb->Remove(nnode, node);
        node = nnode;
        Add_Symbol(coeff, sym_sub, stack, NULL);
        CXX_DELETE(node, _mem_pool);
      }
    }
  }
  if (allow_nonlin && Contains_Non_Lin_Symb()) {
    SUMPROD_ITER iter(Non_Lin_Symb);
    SUMPROD_NODE* node = iter.First();
    for (; !iter.Is_Empty(); node = iter.Next()) {
      INT symbol_count = 0;
      SYMBOL_ITER iiter(node->Prod_List);
      SYMBOL_NODE* snode = iiter.First();
      for (; !iiter.Is_Empty(); snode = iiter.Next())
        if (snode->Symbol.Is_Formal() 
	    && snode->Symbol.Formal_Number() == formal_number)
          symbol_count++;
      if (symbol_count > 0) { 
	// We are not expecting non-linear terms, since substituitable 
        // formals arise only from LINEXs.  But just in case, go conser-
	// vative.
	DevWarn("ACCESS_VECTOR::Substitute: Found non-linear term");
	Too_Messy = TRUE; 
	return;  
      } 
    }
  }
}

void ACCESS_ARRAY::Substitute(INT formal_number, 
                              WN* wn_sub,
                              DOLOOP_STACK* stack,
                              BOOL allow_nonlin)
{
  if (Too_Messy)
    return;
  for (INT i = 0; i < Num_Vec(); i++)
    Dim(i)->Substitute(formal_number, wn_sub, stack, allow_nonlin);
} 

ACCESS_ARRAY::ACCESS_ARRAY(
   UINT16 num_vec,ACCESS_VECTOR* dim[], MEM_POOL *mem_pool=0) 
{
  _dim = CXX_NEW_ARRAY(ACCESS_VECTOR,num_vec,mem_pool);
  _mem_pool=mem_pool;
  for (INT32 i=0; i<num_vec; i++) {
    _dim[i].Init(dim[i],mem_pool);
  }
  Too_Messy = TRUE;
  _num_vec = num_vec;
}

ACCESS_ARRAY::ACCESS_ARRAY(
   UINT16 num_vec,UINT16 nest_depth,MEM_POOL *mem_pool=0) 
{
  _dim = CXX_NEW_ARRAY(ACCESS_VECTOR,num_vec,mem_pool);
  _mem_pool=mem_pool;
  for (INT32 i=0; i<num_vec; i++) {
    _dim[i].Init(nest_depth,mem_pool);
  }
  Too_Messy = TRUE;
  _num_vec = num_vec;
}


void ACCESS_ARRAY::Init(const ACCESS_ARRAY *a, MEM_POOL *pool)
{
  if (_dim != NULL) {
     CXX_DELETE_ARRAY(_dim,_mem_pool);
  }
  _mem_pool = pool;
  Too_Messy = a->Too_Messy;
  _num_vec = a->_num_vec;
  _dim = CXX_NEW_ARRAY(ACCESS_VECTOR,_num_vec,_mem_pool);

  for (INT32 i=0; i<_num_vec; i++) {
    _dim[i].Init(a->Dim(i),pool);
  }
}

BOOL ACCESS_ARRAY::operator ==(const ACCESS_ARRAY& a) const
{
  if (Too_Messy || a.Too_Messy) return (FALSE);
  if (_num_vec != a._num_vec) return(FALSE);
  for (INT32 i=0; i<_num_vec; i++) {
    if (!(*Dim(i) == *a.Dim(i))) {
      return(FALSE);
    }
  }
  return(TRUE);
}

void ACCESS_VECTOR::Print(FILE *fp, BOOL is_bound, BOOL print_brackets) const
{
  char bf[MAX_TLOG_CHARS]; 
  Print(bf, 0, is_bound, print_brackets);
  fprintf(fp, "%s", bf);
}

INT ACCESS_VECTOR::Print(char* bf, 
		         INT ccount, 
			 BOOL is_bound, 
			 BOOL print_brackets) const
{
  INT new_ccount = ccount; 
  if (Too_Messy) {
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "[Too_Messy]");
    return new_ccount;
  }
  if (!is_bound && print_brackets) 
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "[");

  // first the loop variable terms
  BOOL seen_something = FALSE;
  if (!is_bound) {
    if (Const_Offset != 0) {
      if (print_brackets) {
        new_ccount = snprintfll(bf, new_ccount, MAX_TLOG_CHARS, Const_Offset);  
	new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, " ");
      } else {
        new_ccount = snprintfll(bf, new_ccount, MAX_TLOG_CHARS, Const_Offset);
      } 
      seen_something = TRUE;
    }
  }
  for (INT32 i = 0; i < Nest_Depth(); i++) {
    if (Loop_Coeff(i) != 0) {
      if (!seen_something) {
        seen_something = TRUE;
	new_ccount = snprintfd(bf, new_ccount, MAX_TLOG_CHARS, Loop_Coeff(i));
	new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "*loop_var");
	new_ccount = snprintfd(bf, new_ccount, MAX_TLOG_CHARS, i);
	new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, " ");
      } else {
	new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "+ ");
	new_ccount = snprintfd(bf, new_ccount, MAX_TLOG_CHARS, Loop_Coeff(i));
	new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "*loop_var");
	new_ccount = snprintfd(bf, new_ccount, MAX_TLOG_CHARS, i);
	new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, " ");
      }
    }
  }
  if (Lin_Symb != NULL && !Lin_Symb->Is_Empty()) {
    if (seen_something) {
      new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "+ ");
    }
    seen_something = TRUE;
    new_ccount = Lin_Symb->Print(bf, new_ccount);
  }
  if (Non_Lin_Symb != NULL && !Non_Lin_Symb->Is_Empty()) {
    new_ccount = Non_Lin_Symb->Print(bf, new_ccount);
  }
  if (!is_bound && (Const_Offset == 0) && !seen_something) {
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "0");
  }
  if (!is_bound) {
    if (print_brackets)
      new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "]");
  } else {
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, " <= ");
    new_ccount = snprintfll(bf, new_ccount, MAX_TLOG_CHARS, Const_Offset);
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, ";  ");
  }
  if (_non_const_loops) {
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, 
      " non_const_loops is ");
    new_ccount = snprintfd(bf, new_ccount, MAX_TLOG_CHARS, _non_const_loops);
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, " \n");
  }
  if (Delinearized_Symbol != NULL) { 
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, 
      " delin_symbol is ");
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, 
      Delinearized_Symbol->Name());
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, " \n");
  } 
  return new_ccount; 
}

void ACCESS_VECTOR::Print_Analysis_Info(FILE *fp, 
				        DOLOOP_STACK &do_stack, 
				        BOOL is_bound) const
{
  if (Too_Messy) {
    fprintf(fp,"Too_Messy\n");
    return;
  }

  // first the loop variable terms
  BOOL seen_something = FALSE;
  if (!is_bound) {
    if (Const_Offset != 0) {
      fprintf(fp,"%lld ",Const_Offset);
      seen_something = TRUE;
    }
  }

  

  for (INT32 i=0; i< Nest_Depth(); i++) {
    if (Loop_Coeff(i) != 0) {

      if (i >=  do_stack.Elements()) 
        continue;

      SYMBOL sym(WN_index(do_stack.Bottom_nth(i)));
      if (!seen_something) {
        seen_something = TRUE;
	fprintf(fp,"%d*%s", Loop_Coeff(i), sym.Name());
      } else {
	fprintf(fp," + %d*%s", Loop_Coeff(i), sym.Name());
      }
    }
  }
  if (Lin_Symb != NULL && !Lin_Symb->Is_Empty()) {
    if (seen_something) {
      fprintf(fp," + ");
    }
    seen_something = TRUE;
    Lin_Symb->Print(fp);
  }
  if (Non_Lin_Symb != NULL && !Non_Lin_Symb->Is_Empty()) {
    Non_Lin_Symb->Print(fp);
  }
  if (!is_bound && (Const_Offset == 0) && !seen_something) {
    fprintf(fp,"0");
  }
  if (is_bound) {
    fprintf(fp," <= %lld;  ",Const_Offset);
  }
  if (_non_const_loops && Lin_Symb && !Lin_Symb->Is_Empty()) {
    fprintf(fp,"non_const_loops is %d \n",_non_const_loops);
  }
}

BOOL ACCESS_VECTOR::operator ==(const ACCESS_VECTOR& av) const
{
  if (Too_Messy || av.Too_Messy) return (FALSE);
  if (Const_Offset != av.Const_Offset) return(FALSE);
  if (_non_const_loops != av._non_const_loops) return FALSE;
  if ((Delinearized_Symbol != 0) != (av.Delinearized_Symbol != 0)) {
    return FALSE;
  }
  if (Delinearized_Symbol && 
      (*Delinearized_Symbol != *av.Delinearized_Symbol)) return FALSE;
  INT common_depth = MIN(_nest_depth,av._nest_depth);
  INT32 i;

  for (i=0; i< common_depth; i++) {
    if (Loop_Coeff(i) != av.Loop_Coeff(i)) {
      return FALSE;
    }
  }
  for (i=common_depth; i<_nest_depth; i++) {
    if (Loop_Coeff(i)) {
      return FALSE;
    }
  }
  for (i=common_depth; i<av._nest_depth; i++) {
    if (av.Loop_Coeff(i)) {
      return FALSE;
    }
  }

  if (Lin_Symb != NULL && !Lin_Symb->Is_Empty()) { // this has a lin symb
    if (av.Lin_Symb == NULL || av.Lin_Symb->Is_Empty() ||
	!(*Lin_Symb == *av.Lin_Symb)) {
      return(FALSE);
    }
  } else if (av.Lin_Symb != NULL && !av.Lin_Symb->Is_Empty()) {
    return(FALSE);
  }

  if (Non_Lin_Symb != NULL && !Non_Lin_Symb->Is_Empty()) {
    if (av.Non_Lin_Symb == NULL || av.Non_Lin_Symb->Is_Empty() ||
	!(*Non_Lin_Symb == *av.Non_Lin_Symb)) {
      return(FALSE);
    }
  } else if (av.Non_Lin_Symb != NULL && !av.Non_Lin_Symb->Is_Empty()) {
    return(FALSE);
  }

  return(TRUE);
}



// set an ACCESS_VECTOR to be a copy of another
ACCESS_VECTOR::ACCESS_VECTOR(const ACCESS_VECTOR *a, MEM_POOL *pool)
{
  Init(a,pool);
}

void ACCESS_VECTOR::Init(const ACCESS_VECTOR *a, MEM_POOL *pool)
{
  _mem_pool = pool;
  _nest_depth = a->_nest_depth;
  _non_const_loops = a->_non_const_loops;
  Delinearized_Symbol = a->Delinearized_Symbol;
  if (a->_lcoeff) {
    _lcoeff = CXX_NEW_ARRAY(mINT32,_nest_depth,_mem_pool);
    for (INT i=0; i < _nest_depth; i++) {
      _lcoeff[i] = a->_lcoeff[i];
    }
  } else {
    _lcoeff = NULL;
  }
  Too_Messy = a->Too_Messy;
  Const_Offset = a->Const_Offset;
  if (a->Lin_Symb) {
    Lin_Symb = CXX_NEW(INTSYMB_LIST,_mem_pool);
    Lin_Symb->Init(a->Lin_Symb,_mem_pool);
  } else {
    Lin_Symb = NULL;
  }
  if (a->Non_Lin_Symb) {
    Non_Lin_Symb = CXX_NEW(SUMPROD_LIST,_mem_pool);
    Non_Lin_Symb->Init(a->Non_Lin_Symb,_mem_pool);
  } else {
    Non_Lin_Symb = NULL;
  }
}

void ACCESS_VECTOR::Set_Loop_Coeff(UINT16 i, INT32 val)
{
  if (!_lcoeff) {
    _lcoeff = CXX_NEW_ARRAY(mINT32, _nest_depth, _mem_pool);
    for (INT j=0; j < _nest_depth; j++) {
      _lcoeff[j] = 0;
    }
  }
  _lcoeff[i] = val;
}

BOOL ACCESS_VECTOR::Is_Const() const
{
  if (Too_Messy ||
      (Lin_Symb && !Lin_Symb->Is_Empty()) ||
      (Non_Lin_Symb && !Non_Lin_Symb->Is_Empty())) {
    return(FALSE);
  }
  if (!_lcoeff) return(TRUE);
  for (INT32 i=0; i< _nest_depth; i++) {
    if (_lcoeff[i] != 0) return(FALSE);
  }
  return(TRUE);
}


void INTSYMB_LIST::Print(FILE *fp) const
{
  char bf[MAX_TLOG_CHARS]; 
  Print(bf, 0);
  fprintf(fp, "%s", bf);
} 

INT INTSYMB_LIST::Print(char* bf, INT ccount) const
{
  INT new_ccount = ccount; 
  INTSYMB_CONST_ITER iter(this);
  const INTSYMB_NODE* first = iter.First();
  for (const INTSYMB_NODE *node=first; !iter.Is_Empty(); node = iter.Next()) {
      if (node != first) 
        new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "+ ");
      new_ccount = node->Print(bf, new_ccount);
  }
  return new_ccount; 
}

BOOL INTSYMB_LIST::operator ==(const INTSYMB_LIST& isl) const
{
  INTSYMB_CONST_ITER iter(this);
  INTSYMB_CONST_ITER iter2(&isl);
  const INTSYMB_NODE *node2 = iter2.First();
  for (const INTSYMB_NODE *node=iter.First(); !iter.Is_Empty(); ) {
    if (iter2.Is_Empty() || !(*node == *node2)) return(FALSE); 
    node = iter.Next(); 
    node2 = iter2.Next(); 
  }
  if (!iter2.Is_Empty()) return(FALSE);
  return (TRUE);
}

void INTSYMB_LIST::Init(INTSYMB_LIST *il,MEM_POOL *mem_pool)
{
  INTSYMB_ITER iter(il);
  for (INTSYMB_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
	Append(CXX_NEW(INTSYMB_NODE(node),mem_pool));
  }
}

// delete a list, including every node on it
// this assumes that all elements are from the same mempool
// and that the default mempool has been set
INTSYMB_LIST::~INTSYMB_LIST()
{
  while (!Is_Empty()) CXX_DELETE(Remove_Headnode(),Default_Mem_Pool);
}

// Subtract two INTSYMB_LISTs 
// This is a n^2 process.  If these lists become big, we might want to
// sort them at some point
INTSYMB_LIST *Subtract(INTSYMB_LIST *list1, INTSYMB_LIST *list2,
				     MEM_POOL *pool)
{
  INTSYMB_LIST *res = CXX_NEW(INTSYMB_LIST,pool);
  if (list1) res->Init(list1,pool);  // init result to list1
  if (!list2) return (res);

  INTSYMB_ITER iter2(list2);
  for (INTSYMB_NODE *node2 = iter2.First(); !iter2.Is_Empty(); 
	node2 = iter2.Next()) {  // go through every element of list2
    // search for node2 in the result list
    INTSYMB_ITER iterr(res);
    INTSYMB_NODE *noder=iterr.First(), *prevnoder=NULL;
    while (!iterr.Is_Empty() && !(noder->Symbol == node2->Symbol)) {
      prevnoder = noder;
      noder = iterr.Next();
    }
    if (iterr.Is_Empty()) { // It's not in the result list
      res->Prepend(CXX_NEW(INTSYMB_NODE(node2->Symbol,-node2->Coeff),pool));
    } else {
      noder->Coeff -= node2->Coeff;
      if (noder->Coeff == 0) { // get rid of it
	if (noder == iterr.First()) {
	  CXX_DELETE(res->Remove_Headnode(),pool);
	} else {
	  CXX_DELETE(res->Remove(prevnoder,noder),pool);
	}
      }
    }
  }
  if (res->Is_Empty()) return (NULL);
  return(res);
}

// Add two INTSYMB_LISTs 
// This is a n^2 process.  If these lists become big, we might want to
// sort them at some point
INTSYMB_LIST *Add(INTSYMB_LIST *list1, INTSYMB_LIST *list2,
				     MEM_POOL *pool)
{
  INTSYMB_LIST *res = CXX_NEW(INTSYMB_LIST,pool);
  if (list1) res->Init(list1,pool);  // init result to list1
  if (!list2) return (res);

  INTSYMB_ITER iter2(list2);
  for (INTSYMB_NODE *node2 = iter2.First(); !iter2.Is_Empty(); 
	node2 = iter2.Next()) {  // go through every element of list2
    // serach for node2 in the result list
    INTSYMB_ITER iterr(res);
    INTSYMB_NODE *noder=iterr.First(), *prevnoder=NULL;
    while (!iterr.Is_Empty() && !(noder->Symbol == node2->Symbol)) {
      prevnoder = noder;
      noder = iterr.Next();
    }
    if (iterr.Is_Empty()) { // It's not in the result list
      res->Prepend(CXX_NEW(INTSYMB_NODE(node2->Symbol,node2->Coeff),pool));
    } else {
      noder->Coeff += node2->Coeff;
      if (noder->Coeff == 0) { // get rid of it
	if (noder == iterr.First()) {
	  CXX_DELETE(res->Remove_Headnode(),pool);
	} else {
	  CXX_DELETE(res->Remove(prevnoder,noder),pool);
	}
      }
    }
  }
  if (res->Is_Empty()) return (NULL);
  return(res);
}

// Mul and INTSYMB_LIST and an integer constant.

INTSYMB_LIST *Mul(INT c, INTSYMB_LIST *list, MEM_POOL *pool)
{
  if (list == NULL || c == 0)
    return NULL;

  INTSYMB_LIST *res = CXX_NEW(INTSYMB_LIST,pool);
  res->Init(list,pool);

  INTSYMB_ITER iter(res);
  for (INTSYMB_NODE* node = iter.First(); !iter.Is_Empty(); node = iter.Next())
    node->Coeff *= c;

  return(res);
}


void INTSYMB_NODE::Print(FILE *fp) const
{
  char bf[MAX_TLOG_CHARS]; 
  Print(bf, 0);
  fprintf(fp, "%s", bf);
}

INT INTSYMB_NODE::Print(char* bf, INT ccount) const
{
  INT new_ccount = ccount; 
  new_ccount = snprintfd(bf, new_ccount, MAX_TLOG_CHARS, Coeff);
  new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "*");
  new_ccount = Symbol.Print(bf, new_ccount);
  return new_ccount; 
}

void SUMPROD_LIST::Print(FILE *fp) const
{
  char bf[MAX_TLOG_CHARS]; 
  Print(bf, 0);
  fprintf(fp, "%s", bf);
} 

INT SUMPROD_LIST::Print(char* bf, INT ccount) const
{
  INT new_ccount = ccount;
  SUMPROD_CONST_ITER iter(this);
  for (const SUMPROD_NODE *node = iter.First(); !iter.Is_Empty(); 
	node = iter.Next()) {
      new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "+ ");
      new_ccount = node->Print(bf, new_ccount);
  }
  return new_ccount;
} 

INT SUMPROD_LIST::Negate_Me() 
{
  SUMPROD_ITER iter(this);
  for (SUMPROD_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
    INT64 coeff = -node->Coeff;
    if ((coeff >= (INT32_MAX-1)) || (coeff<=(INT32_MIN+1))) {
      return 0;
    }
    node->Coeff = coeff;
  }
  return 1;
}

void SUMPROD_LIST::Merge(SUMPROD_LIST *sl)
{
  while (!sl->Is_Empty()) Append(sl->Remove_Headnode());
}
  

BOOL SUMPROD_LIST::operator ==(const SUMPROD_LIST& sl) const
{
  SUMPROD_CONST_ITER iter(this);
  SUMPROD_CONST_ITER iter2(&sl);
  const SUMPROD_NODE *node2 = iter2.First();
  for (const SUMPROD_NODE *node=iter.First(); !iter.Is_Empty(); ) {
    if (iter2.Is_Empty() || !(*node == *node2)) return(FALSE); 
    node = iter.Next(); 
    node2 = iter2.Next(); 
  }
  if (!iter2.Is_Empty()) return(FALSE);
  return (TRUE);
}


void SUMPROD_LIST::Init(SUMPROD_LIST *sp,MEM_POOL *mem_pool)
{
  SUMPROD_ITER iter(sp);
  for (SUMPROD_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
	Append(CXX_NEW(SUMPROD_NODE(node,mem_pool),mem_pool));
  }
}

SUMPROD_LIST::~SUMPROD_LIST()
{
  while (!Is_Empty()) CXX_DELETE(Remove_Headnode(),Default_Mem_Pool);
}

void SUMPROD_NODE::Print(FILE *fp) const
{
  char bf[MAX_TLOG_CHARS]; 
  Print(bf, 0);
  fprintf(fp, "%s", bf);
}

INT SUMPROD_NODE::Print(char* bf, INT ccount) const
{
  INT new_ccount = ccount; 
  new_ccount = snprintfd(bf, new_ccount, MAX_TLOG_CHARS, Coeff);
  new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "*");
  new_ccount = Prod_List->Print(bf, new_ccount, TRUE);
  return new_ccount; 
}

void SYMBOL_LIST::Print(FILE *fp, BOOL starsep) const
{
  char bf[MAX_TLOG_CHARS]; 
  Print(bf, 0, starsep);
  fprintf(fp, "%s", bf);
} 

INT SYMBOL_LIST::Print(char* bf, INT ccount, BOOL starsep) const
{
  INT new_ccount = ccount; 
  SYMBOL_CONST_ITER iter(this);
  for (const SYMBOL_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
    new_ccount = node->Print(bf, new_ccount);
    if (iter.Peek_Next() != NULL) {
      new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, 
        starsep ? "*":",");
    } 
  }
  return new_ccount; 
} 

BOOL SYMBOL_LIST::Contains(const SYMBOL *s) 
{
  SYMBOL_CONST_ITER iter(this);
  for (const SYMBOL_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
    if (node->Symbol == *s) return TRUE;
  }
  return FALSE;
}

BOOL SYMBOL_LIST::operator ==(const SYMBOL_LIST& sl) const
{
  SYMBOL_CONST_ITER iter(this);
  SYMBOL_CONST_ITER iter2(&sl);
  const SYMBOL_NODE *node2 = iter2.First();
  for (const SYMBOL_NODE *node=iter.First(); !iter.Is_Empty(); ) {
    if (iter2.Is_Empty() || !(*node == *node2)) return(FALSE); 
    node = iter.Next(); 
    node2 = iter2.Next(); 
  }
  if (!iter2.Is_Empty()) return(FALSE);
  return (TRUE);
}

void SYMBOL_LIST::Init(const SYMBOL_LIST *sl,MEM_POOL *mem_pool)
{
  SYMBOL_CONST_ITER iter(sl);
  for (const SYMBOL_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
	Append(CXX_NEW(SYMBOL_NODE(node),mem_pool));
  }
}

SYMBOL_LIST::~SYMBOL_LIST()
{
  while (!Is_Empty()) CXX_DELETE(Remove_Headnode(),Default_Mem_Pool);
}

void SYMBOL_NODE::Print(FILE *fp) const
{
  char bf[MAX_TLOG_CHARS]; 
  Print(bf, 0);
  fprintf(fp, "%s", bf);
}

INT SYMBOL_NODE::Print(char* bf, INT ccount) const
{
  INT new_ccount = ccount; 
  new_ccount = Symbol.Print(bf, new_ccount);
  return new_ccount; 
}

void SYMBOL::Print(FILE *fp) const
{
  char bf[MAX_TLOG_CHARS]; 
  Print(bf, 0);
  fprintf(fp, "%s", bf);
}

INT SYMBOL::Print(char* bf, INT ccount) const
{
  // call SYMBOL::Name() to keep all symbol printing code in one place.
  // It would be faster to not do the extra copy into the buf (by duplicating
  // the printing code in this routine), but this is safer, and printing
  // to a file is already slow enough that who cares about an extra copy.

  INT new_ccount = ccount; 
  if (_is_formal) { 
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "#");
    new_ccount = snprintfd(bf, new_ccount, MAX_TLOG_CHARS, u._formal_number);
  } else {  
    const INT bufsz = 256;
    char buf[bufsz]; 
    (void) Name(buf, bufsz);
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, buf);
  } 
  return new_ccount; 
}

char* SYMBOL::Name() const
{
  const INT   bufsz = 128;
  static char name[bufsz];
  if (_is_formal) { 
    sprintf(name, "#%d", u._formal_number);
    return name; 
  } else { 
    return Name(name, bufsz);
  } 
}

static INT Num_Chars_Needed(INT number)
{
  INT value = number; 
  if (value == 0)
    return 2;
  INT count = 1;
  if (value < 0) {
    value = -value; 
    count++; 
  }
  while (value > 0) {
    value /= 10; 
    count++; 
  } 
  return count; 
}  

char* SYMBOL::Name(char* buf, INT bufsz) const
{
  if (buf == NULL) {
    DevWarn("SYMBOL::Name(buf, bufsz) shouldn't be called with buf == NULL");
    return Name();
  }
  if (bufsz < 1) {
    DevWarn("SYMBOL::Name(buf, bufsz) shouldn't be called with bufsz < 1");
    return NULL; 
  } 
  if (_is_formal) {
    INT chars = Num_Chars_Needed(u._formal_number);
    char* goalname = (char*) alloca(chars + 1);
    sprintf(goalname, "#%d", u._formal_number);
    if (bufsz < chars + 1) {
      return NULL;
    } else {
      strcpy(buf, goalname);
      return buf;  
    } 
  } 

  char*     goalname;
  INT       max_woff_len = 3*sizeof(WN_OFFSET);

  // step 1: allocate space for entire name, put it in goalname.

  if (u._st == NULL) {
    goalname = (char*) alloca(max_woff_len + strlen("$null_st") + 2);
    sprintf(goalname, "$null_st.%d", WN_Offset());
  }
  else if (ST_class(u._st) == CLASS_PREG) {
    const char* name;
    BOOL        use_woff = TRUE;
    if (WN_Offset() > Last_Dedicated_Preg_Offset) {
      name = Preg_Name(WN_Offset());
      if (name == NULL || name[0] == '\0')
        name = "$preg.noname";
      else if (strcmp(name,"<preg>") == 0)
	name = "preg";
      else
        use_woff = FALSE;
    }
    else
      name = "$preg.dedicated";
    goalname = (char*) alloca(max_woff_len + strlen(name) + 2);
    if (use_woff)
      sprintf(goalname, "%s%d", name, WN_Offset());
    else
      sprintf(goalname, "%s", name);
  }
  else {
    BOOL slow = ST_Offset() != 0 || WN_Offset() != 0;
    char* true_name = ST_name(u._st);

    char* name = 0;
    if (ST_Base()) {
      name = ST_name(ST_Base());
      if (name == NULL || name[0] == '\0') {
        name = (char*) alloca(strlen("$noname0x") + 10); 
        sprintf(name, "$noname0x%p", ST_Base());
      }
    }
    else
      name = const_cast<char*>("$nobase");

    goalname = 
      (char*) alloca(strlen(name) + strlen(true_name) + 30 + max_woff_len);
    if (slow || ST_Base() != u._st)
      sprintf(goalname, "%s(%s.%lld.%d)", true_name, 
	name, ST_Offset(), WN_Offset());
    else
      sprintf(goalname, "%s", name);
  }

  // truncate into buf.

  if (strlen(goalname) < bufsz)
    strcpy(buf, goalname);
  else {
    strncpy(buf, goalname, bufsz-1);
    buf[bufsz-1] = '\0';
    DevWarn("Symbol name %s shortened to %s", goalname, buf);
  }

  return buf;
}

char* SYMBOL::Prompf_Name() const
{
  const INT name_sz = 128; 
  static char buf[name_sz]; 
  const char* name = NULL;
  INT i;

  if (_is_formal) {
    sprintf(buf, "#%d", u._formal_number);
    return buf;
  } 

  if (u._st == NULL) {
    name = "<NULL SYMBOL>"; 
  } else if (ST_class(u._st) == CLASS_PREG) {
    if (WN_Offset() > Last_Dedicated_Preg_Offset) 
      name = Preg_Name(WN_Offset());
    else
      name = "<DEDICATED PREG>";
  } else {
    name = ST_name(St());
  }

  for (i = 0; i < name_sz - 1 && name[i] != '\0'; i++)
    buf[i] = name[i]; 
  buf[i] = '\0';
  return buf; 
}

// All the routines to build access arrays

// Find the line number associated with this references
#ifdef LNO
static SRCPOS Find_Line(WN *wn)
{
  WN *tmp_wn = wn;
  while (OPCODE_is_expression(WN_opcode(tmp_wn))) {
    tmp_wn = LWN_Get_Parent(tmp_wn);
  }
  return WN_Get_Linenum(tmp_wn);
}
#endif

// Given an array statment and a stack of all the enclosing do loops
// Build the access array
void ACCESS_ARRAY::Set_Array(WN *wn, DOLOOP_STACK *stack)
{
  INT32 i;

  Is_True(WN_operator(wn) == OPR_ARRAY,
    ("ACCESS_ARRAY::Set_Array called on a non-array"));
  Is_True(_num_vec == WN_num_dim(wn),
  ("ACCESS_ARRAY::Set_Array called with an inconsistent number of dimensions"));
  Too_Messy = FALSE;

  for (i=0; i<_num_vec; i++) {
    _dim[i].Set(WN_array_index(wn,i),stack,1,0,LNO_Allow_Nonlinear);
  }

#ifdef LNO
  if (LNO_Allow_Delinearize)
    Delinearize(stack,wn);
#endif 
  
  // If any non-linear terms left, update non-const loops
  for (i=0; i<_num_vec; i++) {
    if (_dim[i].Contains_Non_Lin_Symb()) {
      _dim[i].Update_Non_Const_Loops_Nonlinear(stack);
    }
  }

  // look at the base, if it varies, update non const loops
  WN *base = WN_array_base(wn);
  if (WN_operator(base) == OPR_LDID) {
    Update_Non_Const_Loops(base,stack);
#ifdef KEY // Bug 5057 - tolerate base addresses of the form (+ const LDID).
  } else if (WN_operator(base) == OPR_ADD &&
	     (WN_operator(WN_kid0(base)) == OPR_INTCONST &&
	      WN_operator(WN_kid1(base)) == OPR_LDID)) {
    Update_Non_Const_Loops(WN_kid1(base),stack);
  } else if (WN_operator(base) == OPR_ADD &&
	     (WN_operator(WN_kid1(base)) == OPR_INTCONST &&
	      WN_operator(WN_kid0(base)) == OPR_LDID)) {
    Update_Non_Const_Loops(WN_kid0(base),stack);
#endif
  }
#ifdef LNO
  else if (Is_Loop_Invariant_Indir(base)) 
    Update_Non_Const_Loops(WN_kid0(WN_kid0(base)), stack);
#endif
  else if (WN_operator(base) != OPR_LDA) {
    for (INT32 i=0; i<_num_vec; i++) {
      Dim(i)->Max_Non_Const_Loops(stack->Elements());
    }
  }

  // look at the array bounds, if they vary, update non const loops
  // (We don't care about 0 since it doesn't affect the shape of the array)
  for (i=1; i<WN_num_dim(wn); i++) {
    Update_Non_Const_Loops(WN_array_dim(wn,i),stack);
  }

  // Do some error checking for split commons
  // for a reference to a[i], check that the upper bound 
  // of i is smaller than the bounds of a.
  // Just catches the easy cases because at this late date, I'd rather
  // avoid false negatives than false positives
#ifdef LNO
  if (_num_vec == 1) {
    if (WN_operator(base) == OPR_LDA) {
#ifdef _NEW_SYMTAB
      if (ST_base_idx(WN_st(base)) != ST_st_idx(WN_st(base)) &&
#else
      if ((ST_sclass(WN_st(base)) == SCLASS_BASED) &&
#endif
          (ST_sclass(ST_base(WN_st(base))) == SCLASS_COMMON)) {
	WN *array_dim = WN_array_dim(wn,0);
	if (WN_operator(array_dim) == OPR_INTCONST) {
	  INT64 dim = WN_const_val(array_dim);
	  if (!Too_Messy && !_dim[0].Too_Messy &&
              !_dim[0].Contains_Lin_Symb() &&
	      !_dim[0].Contains_Non_Lin_Symb()) {
            INT is_ai= TRUE;
	    ACCESS_VECTOR *av = &_dim[0];
	    for (INT i=0; i<av->Nest_Depth()-1; i++) {
	      if (av->Loop_Coeff(i)) is_ai = FALSE;
            }
	    if (av->Loop_Coeff(av->Nest_Depth()-1) != 1) is_ai = FALSE;
	    // check that the statement isn't conditional
	    WN *parent = LWN_Get_Parent(wn);
	    while (WN_opcode(parent) != OPC_DO_LOOP && is_ai) {
	      if (OPCODE_is_scf(WN_opcode(parent))) is_ai=FALSE;
	      parent = LWN_Get_Parent(parent);
	    }
	    if (is_ai) {
	      DO_LOOP_INFO *dli = Get_Do_Loop_Info(stack->Top_nth(0));
	      ACCESS_ARRAY *uba = dli->UB;
	      if (!uba->Too_Messy) {
	        if (uba->Num_Vec() == 1) {
		  ACCESS_VECTOR *ub = uba->Dim(0);
		  INT const_bound = TRUE;
		  if (!ub->Too_Messy && !ub->Contains_Lin_Symb() &&
		      !ub->Contains_Non_Lin_Symb()) {
	            for (INT i=0; i<ub->Nest_Depth()-1; i++) {
	              if (ub->Loop_Coeff(i)) const_bound = FALSE;
                    }
		    if (ub->Loop_Coeff(ub->Nest_Depth()-1) != 1) {
		      const_bound = FALSE;
                    }
		    if (const_bound) {
		      if (ub->Const_Offset > dim) {
			ErrMsgSrcpos(EC_LNO_Generic,Find_Line(wn),
			 "Out of bounds array reference, results unpredictable."
			 );
		      }
		    }
                  }
	        }
	      }
	    }
	  }
	}
      }
    }
  }
#endif
}

#ifdef LNO

// Delinearize access arrays
void ACCESS_ARRAY::Delinearize(DOLOOP_STACK *stack, WN *wn)
{
  if (Too_Messy) return;
  INT i=0;
  while ((i<_num_vec) && 
         (Dim(i)->Too_Messy || !Dim(i)->Contains_Non_Lin_Symb())) i++;

  // found a non-linear term
  if (i < _num_vec) {
    if (LNO_Debug_Delinearization) {
      fprintf(TFile,"Trying to delinearize \n");
      fprintf(TFile,"Before delinearization the access array was"); 
      Print(TFile); fprintf(TFile,"\n");
    }
    if (Delinearize(stack,i,wn)) { 
       // we delinearized a dimension, there could be more dimensions
       // call recursively since Dims might have completely changed
      Delinearize(stack, wn);
      if (LNO_Debug_Delinearization) {
        fprintf(TFile,"succeeded \n");
        fprintf(TFile,"After delinearization the access array is"); 
        Print(TFile); fprintf(TFile,"\n");
      }
    } else if (LNO_Debug_Delinearization) {
      fprintf(TFile,"failed \n");
    }
  }
}

// Try to delinearize dimension dim
// Return TRUE if succeeded
INT ACCESS_ARRAY::Delinearize(DOLOOP_STACK *stack, INT dim, WN *wn)
{
  ACCESS_VECTOR *av = Dim(dim);
  Is_True(av->Non_Lin_Symb && !av->Non_Lin_Symb->Is_Empty(),
	  ("ACCESS_ARRAY::Delinearize called on linear vector"));

  // look for a non-loop variable symbol that's on every non-linear list

  // iterate over every symbol on the first list
  SUMPROD_CONST_ITER tmp_iter(av->Non_Lin_Symb);
  SYMBOL_LIST *first = tmp_iter.First()->Prod_List;
  SYMBOL_CONST_ITER iter(first);
  const SYMBOL *delin_symbol = NULL;
  for (const SYMBOL_NODE *node = iter.First(); !iter.Is_Empty();
	      node=iter.Next()) {
    if (!node->Is_Loop_Var) { // search for this symbol on all the other lists
      delin_symbol = &node->Symbol;
      BOOL this_one_good = TRUE;
      SUMPROD_CONST_ITER iter2(av->Non_Lin_Symb);
      const SUMPROD_NODE *node2 = iter2.First();
      for (node2 = iter2.Next(); !iter2.Is_Empty() && this_one_good ;
	      					node2=iter2.Next()) {
	this_one_good &= node2->Prod_List->Contains(delin_symbol);
      }
      if (this_one_good && av->Can_Delinearize(wn,delin_symbol)) {
	return Delinearize(stack,dim,delin_symbol);
      }
    }
  }
  return FALSE;
}

// Given that delin_symbol is a non-loop variable present in every
// nonlinear term, can we delinearize this vector 
// wn is the array expression
BOOL ACCESS_VECTOR::Can_Delinearize(WN *wn, const SYMBOL *delin_symbol)
{
  // create access vector containing everything not multiplied by delin_symbol
  MEM_POOL_Push(&LNO_local_pool);
  ACCESS_VECTOR *tmp =CXX_NEW(ACCESS_VECTOR(Nest_Depth(),&LNO_local_pool),
				&LNO_local_pool);
  tmp->Too_Messy = FALSE;
  tmp->Const_Offset = Const_Offset;
  for (INT i=0; i<Nest_Depth(); i++) {
    tmp->Set_Loop_Coeff(i,Loop_Coeff(i));
  }

  tmp->Lin_Symb = CXX_NEW(INTSYMB_LIST,&LNO_local_pool);
  INTSYMB_ITER lin_iter(Lin_Symb);
  for (INTSYMB_NODE *lin_node=lin_iter.First(); !lin_iter.Is_Empty();
	lin_node = lin_iter.Next()) {
    if (!(lin_node->Symbol == *delin_symbol)) {
      tmp->Lin_Symb->Append(CXX_NEW(
	INTSYMB_NODE(lin_node->Symbol,lin_node->Coeff), &LNO_local_pool));
    }
  }

  // in order to delinearize, we must prove that 
  //  0 <= tmp < delin_symbol

  // is it possible that tmp < 0 (tmp <= -1)
  tmp->Const_Offset = -tmp->Const_Offset-1; 
  if (Is_Consistent_Condition(tmp,wn)) {
    MEM_POOL_Pop(&LNO_local_pool);
    return FALSE;
  }
  // is it possible that tmp >= delin_symbol (delin_symbol -tmp <= 0)
  // (where tmp is the original tmp)
  tmp->Const_Offset++;  // tmp <= 0
  tmp->Negate_Me();     // -tmp <= 0
  INTSYMB_NODE *delin_node = 
	CXX_NEW(INTSYMB_NODE(*delin_symbol,1),&LNO_local_pool);
  tmp->Lin_Symb->Prepend(delin_node);

  if (Is_Consistent_Condition(tmp,wn)) {
    MEM_POOL_Pop(&LNO_local_pool);
    return FALSE;
  }
  MEM_POOL_Pop(&LNO_local_pool);

  return TRUE;
}

#endif 

// Use the non-linear terms to update Non_Const_Loops
// i.e. given a[n*i], use i to update Non_Const_Loops
// we assume that the effects of 'n' have already been taken
// care of
void ACCESS_VECTOR::Update_Non_Const_Loops_Nonlinear(DOLOOP_STACK *stack)
{
  if (!Non_Lin_Symb) return;
  SUMPROD_CONST_ITER sp_iter(Non_Lin_Symb);
  for (const SUMPROD_NODE *sp_node=sp_iter.First(); !sp_iter.Is_Empty();
	sp_node = sp_iter.Next()) {
    SYMBOL_LIST *sl = sp_node->Prod_List;
    SYMBOL_CONST_ITER iter(sl);
    for (const SYMBOL_NODE *node = iter.First(); !iter.Is_Empty();
			  node=iter.Next()) {
      if (node->Is_Loop_Var) { 
	SYMBOL symbol = node->Symbol;
	INT i = 0;
	while (! (SYMBOL(WN_index(stack->Bottom_nth(i))) == 
		symbol)) i++;
	_non_const_loops = MAX(_non_const_loops,i+1);
      }
    }
  }
}

#ifdef LNO
// do the delinearization
// return FALSE if something went wrong (overflow)
INT ACCESS_ARRAY::Delinearize(DOLOOP_STACK *stack, INT dim, 
						const SYMBOL *delin_symbol)
{
  ACCESS_VECTOR *av = Dim(dim);
  // create a new array of ACCESS_VECTORS, unfortunately, because it's
  // an array, we have to copy all the other dimensions

  ACCESS_VECTOR *new_dim = 
    CXX_NEW_ARRAY(ACCESS_VECTOR,Num_Vec()+1,_mem_pool);
  INT i;
  for (i=0; i<dim; i++) {
    new_dim[i].Init(Dim(i),_mem_pool);
  }
  for (i=dim+1; i<Num_Vec(); i++) {
    new_dim[i+1].Init(Dim(i),_mem_pool);
  }

  // now set  new_dim[dim] to the part of av that's multiplied by the symbol
  // and new_dim[dim+1] to the part of av that's not
  new_dim[dim].Init(av->Nest_Depth(),_mem_pool);
  new_dim[dim].Too_Messy = FALSE;
  new_dim[dim].Delinearized_Symbol = CXX_NEW(SYMBOL(delin_symbol),_mem_pool);

  // search through the linear part of av, this affects the constant offset
  INTSYMB_ITER lin_iter(av->Lin_Symb);
  INTSYMB_NODE* lin_node = 0;
  for (lin_node=lin_iter.First(); !lin_iter.Is_Empty();
	lin_node = lin_iter.Next()) {
    if (lin_node->Symbol == *delin_symbol) {
      INT64 coeff = new_dim[dim].Const_Offset + lin_node->Coeff;
      if ((coeff >= (INT32_MAX-1)) || (coeff<=(INT32_MIN+1))) {
	return FALSE;
      }
      new_dim[dim].Const_Offset = coeff;
    }
  }

  // search through the non-linear part
  SUMPROD_CONST_ITER nonlin_iter(av->Non_Lin_Symb);
  for (const SUMPROD_NODE *nonlin_node= nonlin_iter.First(); 
	!nonlin_iter.Is_Empty(); nonlin_node=nonlin_iter.Next()) {
    SYMBOL_LIST *prod_list = nonlin_node->Prod_List;
    // check how many variables are on the prod list
    // if it's one or two, this is switching into a linear term
    INT length=prod_list->Len();
    if (length == 1) {
      INT64 coeff = new_dim[dim].Const_Offset + nonlin_node->Coeff;
      if ((coeff >= (INT32_MAX-1)) || (coeff<=(INT32_MIN+1))) {
	return FALSE;
      }
      new_dim[dim].Const_Offset = coeff;
    } else if (length==2) {  // find the other one
      SYMBOL_CONST_ITER iter(prod_list);
      const SYMBOL_NODE *node = iter.First();
      if (node->Symbol == *delin_symbol) node = iter.Next();
      new_dim[dim].Add_Symbol(nonlin_node->Coeff,node->Symbol,stack,NULL);
      if (new_dim[dim].Too_Messy) return FALSE;
    } else { // it's still nonlinear
      SYMBOL_LIST *new_prod_list = CXX_NEW(SYMBOL_LIST,_mem_pool);
      SUMPROD_NODE *new_node = 
	CXX_NEW(SUMPROD_NODE(new_prod_list,nonlin_node->Coeff),_mem_pool);
      SYMBOL_CONST_ITER iter(nonlin_node->Prod_List);
      BOOL seen_delin_symbol = FALSE;
      for (const SYMBOL_NODE *node= iter.First();
			!iter.Is_Empty(); node=iter.Next()) {
	if (seen_delin_symbol || !(node->Symbol == *delin_symbol)) { 
	  SYMBOL_NODE *new_symb_node = CXX_NEW(SYMBOL_NODE(node->Symbol,
		node->Is_Loop_Var),_mem_pool);
	  new_prod_list->Append(new_symb_node);
        } else {
	  seen_delin_symbol = TRUE;
        }
      }
      if (!new_dim[dim].Non_Lin_Symb) {
	new_dim[dim].Non_Lin_Symb = CXX_NEW(SUMPROD_LIST,_mem_pool);
      }
      new_dim[dim].Non_Lin_Symb->Append(new_node);
    }
  }

  // new_dim[dim+1] is the same as the original tmp above
  new_dim[dim+1].Init(av->Nest_Depth(),_mem_pool);
  new_dim[dim+1].Too_Messy = FALSE;
  new_dim[dim+1].Const_Offset = av->Const_Offset;
  for (i=0; i<av->Nest_Depth(); i++) {
    new_dim[dim+1].Set_Loop_Coeff(i,av->Loop_Coeff(i));
  }
  new_dim[dim+1].Lin_Symb = CXX_NEW(INTSYMB_LIST,_mem_pool);
  lin_iter.Init(av->Lin_Symb);
  for (lin_node=lin_iter.First(); !lin_iter.Is_Empty();
	lin_node = lin_iter.Next()) {
    if (!(lin_node->Symbol == *delin_symbol)) {
      new_dim[dim+1].Lin_Symb->Append(CXX_NEW(
	INTSYMB_NODE(lin_node->Symbol,lin_node->Coeff), _mem_pool));
    }
  }

  if (av->Non_Const_Loops()) {
    if (new_dim[dim].Contains_Non_Lin_Symb() ||
	new_dim[dim].Contains_Lin_Symb()) {
      new_dim[dim].Set_Non_Const_Loops(av->Non_Const_Loops());
    }
    if (new_dim[dim+1].Contains_Non_Lin_Symb() ||
	new_dim[dim+1].Contains_Lin_Symb()) {
      new_dim[dim+1].Set_Non_Const_Loops(av->Non_Const_Loops());
    }
  }

  _dim = new_dim;
  _num_vec++;
  return TRUE;
}
#endif

// Given an expression for the lb of a DO loop, set the access array
void ACCESS_ARRAY::Set_LB(WN *wn, DOLOOP_STACK *stack, INT64 step)
{
  Too_Messy = FALSE;
  if (step > 0) {
    // only top level maxs
    if ((Num_Vec() == 1) || (WN_operator(wn) == OPR_MAX)) {
      Set_LB_r(wn,stack,0,step);
    // there are maxs one level down
    } else if (WN_operator(wn) == OPR_SUB) {
      Set_LB_r(WN_kid0(wn),stack,0,step);
      for (INT i=0; i<Num_Vec(); i++) {
        _dim[i].Add(WN_kid1(wn),stack,-1);
      }
    } else if (WN_operator(wn) == OPR_ADD) {
      if (WN_operator(WN_kid0(wn)) == OPR_MAX) {
        Set_LB_r(WN_kid0(wn),stack,0,step);
        for (INT i=0; i<Num_Vec(); i++) {
          _dim[i].Add(WN_kid1(wn),stack,1);
        }
      } else {
        Set_LB_r(WN_kid1(wn),stack,0,step);
        for (INT i=0; i<Num_Vec(); i++) {
          _dim[i].Add(WN_kid0(wn),stack,1);
        }
      }
    }
  } else {
    // only top level mins
    if ((Num_Vec() == 1) || (WN_operator(wn) == OPR_MIN)) {
      Set_LB_r(wn,stack,0,step);
    // there are mins one level down
    } else if (WN_operator(wn) == OPR_SUB) {
      Set_LB_r(WN_kid0(wn),stack,0,step);
      for (INT i=0; i<Num_Vec(); i++) {
        _dim[i].Add(WN_kid1(wn),stack,1);
      }
    } else if (WN_operator(wn) == OPR_ADD) {
      if (WN_operator(WN_kid0(wn)) == OPR_MIN) {
        Set_LB_r(WN_kid0(wn),stack,0,step);
        for (INT i=0; i<Num_Vec(); i++) {
          _dim[i].Add(WN_kid1(wn),stack,-1);
        }
      } else {
        Set_LB_r(WN_kid1(wn),stack,0,step);
        for (INT i=0; i<Num_Vec(); i++) {
          _dim[i].Add(WN_kid0(wn),stack,-1);
        }
      }
    }
  }
}
// The recursive version of above, we're currently working on dim i 
// Return the next dimension to work on
INT ACCESS_ARRAY::Set_LB_r(WN *wn, DOLOOP_STACK *stack, INT i, INT64 step)
{
  if ((step > 0) && WN_operator(wn) == OPR_MAX) {
    INT res = Set_LB_r(WN_kid(wn,0),stack,i,step);
    res = Set_LB_r(WN_kid(wn,1),stack,res,step);
    return(res);
  } else if ((step < 0) && WN_operator(wn) == OPR_MIN) {
    INT res = Set_LB_r(WN_kid(wn,0),stack,i,step);
    res = Set_LB_r(WN_kid(wn,1),stack,res,step);
    return(res);
  } else if ((step > 0) && WN_operator(wn) ==
	     OPR_INTRINSIC_OP) {
    INT32 intr = WN_intrinsic(wn);
    if ((step > 0) &&
	((intr == INTRN_I4DIVFLOOR) || (intr == INTRN_I8DIVFLOOR) ||
	 (intr == INTRN_U4DIVFLOOR) || (intr == INTRN_U8DIVFLOOR))) {
      WN *const_kid = WN_kid0 (WN_kid1 (wn));
      if ((WN_operator (const_kid) == OPR_INTCONST) &&
	  (WN_const_val (const_kid) > 0) &&
	  (WN_const_val (const_kid) < INT32_MAX)) {
	FmtAssert (OPR_PARM == 
		   WN_operator (WN_kid0 (wn)),
		   ("Child of an intrn not a parm!"));
	_dim[i].Set(WN_kid0 (WN_kid0 (wn)), stack, 1, 
		    1 - WN_const_val (const_kid));
	_dim[i].Const_Offset = - _dim[i].Const_Offset;
	INT depth = _dim[i].Nest_Depth();
	_dim[i].Set_Loop_Coeff (depth-1, -WN_const_val (const_kid));
	_dim[i].Too_Messy = FALSE;
      } else 
	_dim[i].Too_Messy = TRUE;
      return (i+1);
    } else if ((step > 0) &&
	       ((intr == INTRN_I4DIVCEIL)||(intr == INTRN_I8DIVCEIL)||
		(intr == INTRN_U4DIVCEIL)||(intr == INTRN_U8DIVCEIL))) {
      WN *const_kid = WN_kid0 (WN_kid1 (wn));
      if ((WN_operator (const_kid) == OPR_INTCONST) &&
	  (WN_const_val (const_kid) > 0) &&
	  (WN_const_val (const_kid) < INT32_MAX)) {
	FmtAssert (OPR_PARM == 
		   WN_operator (WN_kid0 (wn)),
		   ("Child of an intrn not a parm!"));
	_dim[i].Set(WN_kid0 (WN_kid0 (wn)), stack, 1, 0);
	_dim[i].Const_Offset = - _dim[i].Const_Offset;
	INT depth = _dim[i].Nest_Depth();
	_dim[i].Set_Loop_Coeff (depth-1, -WN_const_val (const_kid));
	_dim[i].Too_Messy = FALSE;
      } else
	_dim[i].Too_Messy = TRUE;
      return (i+1);
    } else {
      _dim[i].Too_Messy = TRUE;
      return (i+1);
    }
  } else {
    INT depth = _dim[i].Nest_Depth();
    if (step > 0) {
      _dim[i].Set(wn,stack,1,0);
      if (_dim[i].Loop_Coeff(depth-1)) { // i = i + .. (i is on the rhs)
	_dim[i].Too_Messy = TRUE;
      } else {
	_dim[i].Set_Loop_Coeff (depth-1, -1);
	_dim[i].Const_Offset = - _dim[i].Const_Offset;
      }
    } else {
      _dim[i].Set(wn,stack,-1,0);
      if (_dim[i].Loop_Coeff(depth-1)) { // i = i + .. (i is on the rhs)
	_dim[i].Too_Messy = TRUE;
      } else {
	_dim[i].Set_Loop_Coeff (depth-1, 1);
	_dim[i].Const_Offset = - _dim[i].Const_Offset;
      }
    }
    return(i+1);
  }
}

// Given the comparison for the ub of a DO loop, set the access array
void ACCESS_ARRAY::Set_UB(WN *compare, DOLOOP_STACK *stack)
{
  Too_Messy = FALSE;
  INT sign,offset;

  if (WN_operator(compare) == OPR_LE) {
    sign = 1;
    offset = 0;
  } else if (WN_operator(compare) == OPR_GE) {
    sign = -1;
    offset = 0;
  } else if (WN_operator(compare) == OPR_LT) {
    sign = 1;
    offset = 1;
  } else if (WN_operator(compare) == OPR_GT) {
    sign = -1;
    offset = 1;
  } else {
    Is_True(0, ("ACCESS_ARRAY::Set_UB: Unknown comparison "));
  }

  if ((WN_operator(WN_kid0(compare)) == OPR_MIN) ||
      (WN_operator(WN_kid0(compare)) == OPR_MAX) ||
      (WN_operator(WN_kid0(compare))  == OPR_INTRINSIC_OP)) {
    for (INT i=0; i<Num_Vec(); i++) {
      _dim[i].Set(WN_kid1(compare),stack,-sign,offset);
    }
    if (!_dim[0].Too_Messy) {
      Set_UB_r(WN_kid0(compare),stack,0,-sign);
    }
  } else {
    for (INT i=0; i<Num_Vec(); i++) {
      _dim[i].Set(WN_kid0(compare),stack,sign,offset);
    }
    if (!_dim[0].Too_Messy) {
      Set_UB_r(WN_kid1(compare),stack,0,sign);
    }
  }
}

// The recursive version of above, we're currently working on dim i 
// Return the next dimension to work on
INT ACCESS_ARRAY::Set_UB_r(WN *wn, DOLOOP_STACK *stack, INT i, INT sign)
{
  OPERATOR oper = WN_operator(wn);
  if ((sign > 0) && oper == OPR_MIN) {
    INT res = Set_UB_r(WN_kid(wn,0),stack,i,sign);
    res = Set_UB_r(WN_kid(wn,1),stack,res,sign);
    return(res);
  } else if ((sign < 0) && oper == OPR_MAX) {
    INT res = Set_UB_r(WN_kid(wn,0),stack,i,sign);
    res = Set_UB_r(WN_kid(wn,1),stack,res,sign);
    return(res);
  } else if (oper == OPR_INTRINSIC_OP) {
    INT32 intr = WN_intrinsic(wn);
    if ((sign > 0) && 
	((intr == INTRN_I4DIVFLOOR) || (intr == INTRN_I8DIVFLOOR) ||
         (intr == INTRN_U4DIVFLOOR) || (intr == INTRN_U8DIVFLOOR))) {
      WN *const_kid = WN_kid0(WN_kid1(wn));
      if ((WN_operator(const_kid) == OPR_INTCONST) &&
	  (WN_const_val(const_kid) > 0)&&(WN_const_val(const_kid)<INT32_MAX)){
        _dim[i].Mul(WN_const_val(const_kid));
        _dim[i].Add(WN_kid0(WN_kid0(wn)),stack,-sign);
        _dim[i].Const_Offset = -_dim[i].Const_Offset;
        return(i+1);
      } else {
        _dim[i].Too_Messy = TRUE;
        return(i+1);
      }
    } else if ((sign < 0) && 
	((intr == INTRN_I4DIVCEIL) || (intr == INTRN_I8DIVCEIL) ||
         (intr == INTRN_U4DIVCEIL) || (intr == INTRN_U8DIVCEIL))) {
      WN *const_kid = WN_kid0(WN_kid1(wn));
      if ((WN_operator(const_kid) == OPR_INTCONST) &&
	  (WN_const_val(const_kid) > 0)&&(WN_const_val(const_kid)<INT32_MAX)) {
        _dim[i].Mul(WN_const_val(const_kid));
        _dim[i].Add(WN_kid0(WN_kid0(wn)),stack,-sign);
        _dim[i].Const_Offset = -_dim[i].Const_Offset;
        return(i+1);
      } else {
        _dim[i].Too_Messy = TRUE;
        return(i+1);
      }
    } else { 
      _dim[i].Too_Messy = TRUE;
      return(i+1);
    }
  } else {
    _dim[i].Add(wn,stack,-sign);
    _dim[i].Const_Offset = -_dim[i].Const_Offset;
    return(i+1);
  }
}

// Set the array for a condition statement
// we're currently working on dim i, return the next dimension to work on
INT ACCESS_ARRAY::Set_IF(WN *wn, DOLOOP_STACK *stack, BOOL negate, 
			  BOOL is_and, INT i)
{
  Too_Messy = FALSE;
  if (is_and && (WN_operator(wn) == OPR_LAND
      || WN_operator(wn) == OPR_CAND)) {
    INT res = Set_IF(WN_kid0(wn),stack,negate,is_and,i);
    return Set_IF(WN_kid1(wn),stack,negate,is_and,res);
  } 
  if (!is_and && (WN_operator(wn) == OPR_LIOR
      || WN_operator(wn) == OPR_CIOR)) {
    INT res = Set_IF(WN_kid0(wn),stack,negate,is_and,i);
    return Set_IF(WN_kid1(wn),stack,negate,is_and,res);
  } 
  _dim[i].Set_Condition(wn,stack,negate);
  return(i+1);
}

// set one component of the condition to the condition rooted at wn
// if negate is true, negate the condition
void ACCESS_VECTOR::Set_Condition(WN *wn, DOLOOP_STACK *stack, BOOL negate)
{
  Too_Messy = FALSE;

  if (WN_operator(wn) == OPR_LNOT) {
    wn = WN_kid0(wn);
    negate = !negate;
  }

  // 785479: 
  // i + (-2) <= 1 is NOT equivalent to i <= 3 
  // when the comparison is unsigned because of the overflow wraparound; 
  // this sequence came out of gccfe from the test (i == 2 || i == 3)
  if (OPERATOR_is_compare(WN_operator(wn)) && MTYPE_is_unsigned(WN_desc(wn))) {
    Too_Messy = TRUE;
    return;
  }
  
  INT sign,offset;
  sign = negate ? -1 : 1;
  if (WN_operator(wn) == OPR_LE) {
    offset = sign > 0 ? 0 : 1;
  } else if (WN_operator(wn) == OPR_GE) {
    offset = sign > 0 ? 0 : 1;
    sign = -sign;
  } else if (WN_operator(wn) == OPR_LT) {
    offset = sign > 0 ? 1 : 0;
  } else if (WN_operator(wn) == OPR_GT) {
    offset = sign > 0 ? 1 : 0;
    sign = -sign;
  } else if (WN_operator(wn) == OPR_INTCONST) {
    INT is_true = !(WN_const_val(wn) == 0);
    if (negate) is_true = !is_true;
    Set(wn,stack,0,0);
    if (is_true) {
      Const_Offset = 0;
    } else {
      Const_Offset = -1;
    }
    return;
  } else {
    Too_Messy = TRUE;
    return;
  }

  // initialize to the left hand side of the compare
  Set(WN_kid0(wn),stack,sign,offset);
  
  // add in the right hand side 
  if (!Too_Messy) {
    Add(WN_kid1(wn),stack,-sign);
    Const_Offset = -Const_Offset;
  }
}


// Given an expression and a stack of all the enclosing do loops,
// build the access vector.
// We only build non-linear terms if allow_nonline, otherwise, if there are 
// any, this vector is set to too_messy
// Add offset to the expression
// Multiply the expression by sign
void ACCESS_VECTOR::Set(WN *wn, DOLOOP_STACK *stack,INT8 sign,INT offset,
			BOOL allow_nonlin)
{
  Too_Messy = FALSE;
  Const_Offset = (INT64) offset;
  Non_Lin_Symb = NULL;
  Delinearized_Symbol = NULL;
  Add_Sum(wn,(INT64) sign,stack,allow_nonlin);
}

// Add coeff*(the term routed at wn) to this
void ACCESS_VECTOR::Add(WN *wn, DOLOOP_STACK *stack,INT8 sign)
{
#ifdef LNO
  Add_Sum(wn,(INT64) sign,stack);
#else
  Add_Sum(wn,(INT64) sign,stack,LNO_Allow_Nonlinear);
#endif
}

#ifdef KEY
//whether or not it is used for array addresses calculation.
static BOOL WN_Under_Array(WN *wn)
{
    WN* parent = LWN_Get_Parent(wn);
    while(parent && WN_operator(parent) != OPR_DO_LOOP) {
      if (WN_operator(parent) == OPR_ARRAY)
        return TRUE;
      parent = LWN_Get_Parent(parent);
    }
  return FALSE;
}
#endif

// Add coeff*(expression represented by wn) to this vector
void ACCESS_VECTOR::Add_Sum(WN *wn, INT64 coeff, DOLOOP_STACK *stack,
				BOOL allow_nonlin)
{
  if  (Too_Messy) return;

  if (WN_operator(wn) == OPR_ADD) {
    Add_Sum(WN_kid(wn,0),coeff,stack,allow_nonlin);
    Add_Sum(WN_kid(wn,1),coeff,stack,allow_nonlin);
  } else if (WN_operator(wn) == OPR_SUB) {
    Add_Sum(WN_kid(wn,0),coeff,stack,allow_nonlin);
    Add_Sum(WN_kid(wn,1),-coeff,stack,allow_nonlin);
  } else if (WN_operator(wn) == OPR_NEG) {
    Add_Sum(WN_kid(wn,0),-coeff,stack,allow_nonlin);
  } else if (WN_operator(wn) == OPR_MPY) { 
    if (WN_operator(WN_kid(wn,0)) == OPR_INTCONST) {
      Add_Sum(WN_kid(wn,1),coeff*WN_const_val(WN_kid(wn,0)),stack,
							allow_nonlin);
    } else if (WN_operator(WN_kid(wn,1)) == OPR_INTCONST) {
      Add_Sum(WN_kid(wn,0),coeff*WN_const_val(WN_kid(wn,1)),stack,
							allow_nonlin);
    } else if (allow_nonlin) {
      if ((coeff >= (INT32_MAX-1)) || (coeff<=(INT32_MIN+1))) {
	Too_Messy = TRUE;
      } else {
	MEM_POOL_Push(&LNO_local_pool);
	if (!Non_Lin_Symb) {
	  SUMPROD_LIST *list = CXX_NEW(SUMPROD_LIST,_mem_pool);
          SYMBOL_LIST *sl= CXX_NEW(SYMBOL_LIST,_mem_pool);
          SUMPROD_NODE *node = CXX_NEW(SUMPROD_NODE(sl,coeff),_mem_pool);
          list->Append(node);
          Non_Lin_Symb = Add_Nonlin(wn,list,stack);
	  if (!Non_Lin_Symb) {
	    Too_Messy = TRUE;
          }
        } else {
	  SUMPROD_LIST *list = CXX_NEW(SUMPROD_LIST,&LNO_local_pool);
          SYMBOL_LIST *sl= CXX_NEW(SYMBOL_LIST,_mem_pool);
          SUMPROD_NODE *node = CXX_NEW(SUMPROD_NODE(sl,coeff),_mem_pool);
          list->Append(node);
	  SUMPROD_LIST *tmp = Add_Nonlin(wn,list,stack);
	  if (tmp) {
            Non_Lin_Symb->Merge(tmp);
          } else {
	    Too_Messy = TRUE;
          }
        }
	// some of the non-linear symbols may actually be linear
	// go through the list and move those to the linear list
	// i.e. given n*(i-1) both n*i and n*-1 will be on non-linear list
	// we want to move n*-1 to the linear one.
	SUMPROD_ITER sp_iter(Non_Lin_Symb);
	SUMPROD_NODE *sp_prev = NULL;
	SUMPROD_NODE *sp_next = NULL;
	for (SUMPROD_NODE *sp_node=sp_iter.First(); !sp_iter.Is_Empty();
	  	sp_node = sp_next) {
          sp_next = sp_iter.Next();
	  SYMBOL_LIST *sl = sp_node->Prod_List;
	  INT length=sl->Len();
	  SYMBOL_ITER iter(sl);
	  SYMBOL_NODE *node = iter.First(); 
	  if (!node) { // a constant
	    Const_Offset += sp_node->Coeff;
	    if (sp_prev) {
	      CXX_DELETE(Non_Lin_Symb->Remove(sp_prev,sp_node),_mem_pool);
	    } else {
	      CXX_DELETE(Non_Lin_Symb->Remove_Headnode(),_mem_pool);
            }
	  } else if (length==1) { // one element on list so really linear
	    Add_Symbol(sp_node->Coeff,node->Symbol,stack,NULL);
	    if (sp_prev) {
	      CXX_DELETE(Non_Lin_Symb->Remove(sp_prev,sp_node),_mem_pool);
	    } else {
	      CXX_DELETE(Non_Lin_Symb->Remove_Headnode(),_mem_pool);
            }
	  } else {
	    sp_prev = sp_node;
          }
        }
	MEM_POOL_Pop(&LNO_local_pool);
      }
    } else {
      Too_Messy = TRUE;
    }
  } else if (WN_operator(wn) == OPR_LDID) {
    SYMBOL symb(wn);
    Add_Symbol((INT64) coeff,symb,stack,wn);
  } else if (WN_operator(wn) == OPR_INTCONST) {
    if (coeff == 1) {
      Const_Offset += WN_const_val(wn);
    } else if (coeff == -1) {
      Const_Offset -= WN_const_val(wn);
    } else {
      Const_Offset += coeff*WN_const_val(wn);
    }
  } else if (WN_operator(wn) == OPR_PAREN) {
    Add_Sum(WN_kid(wn,0),coeff,stack,allow_nonlin);
  } else if (WN_opcode(wn) == OPC_I8I4CVT
	     || WN_opcode(wn) == OPC_U8I4CVT ) {
    // Bug 14132 -- OPC_U8I4CVT should also be ok
    Add_Sum(WN_kid(wn,0),coeff,stack,allow_nonlin);
  }
#ifdef KEY 
  // Bug 4525 - tolerate CVTs in the access vector for -m64 compilation
  // when the type of loop variable is I8 but the rest of the ARRAY kids 
  // are of type U4/I4. The CVT and the associated CVTL introduced by the 
  // front-end or inliner can be ignored. The return type can be assumed 
  // to be of type I4.
  else if (WN_opcode(wn) == OPC_I4U8CVT &&
	     WN_opcode(WN_kid0(wn)) == OPC_U8CVTL &&
	     WN_cvtl_bits(WN_kid0(wn)) == 32) {
    Add_Sum(WN_kid0(WN_kid0(wn)),coeff,stack,allow_nonlin);  
  } 
//BUG 14400: don't abruptly consider access vector Too_Messy for shifts
//BUG 14495: limit this only for building access vectors for arrays
  else if (WN_operator(wn) == OPR_SHL && WN_Under_Array(wn)){
    if (WN_operator(WN_kid(wn,1)) == OPR_INTCONST) {
      Add_Sum(WN_kid(wn,0),coeff<<WN_const_val(WN_kid(wn,1)),stack,
                                                        allow_nonlin);
    }else Too_Messy = TRUE;
  } 
  else if (WN_operator(wn) == OPR_ASHR && WN_Under_Array(wn)){
    if (WN_operator(WN_kid(wn,1)) == OPR_INTCONST) {
      Add_Sum(WN_kid(wn,0),coeff>>WN_const_val(WN_kid(wn,1)),stack,
                                                        allow_nonlin);
    }else Too_Messy = TRUE;
  }
#endif
  else {
    Too_Messy = TRUE;
  }
}


// add to the vector the term coeff*symbol
// if it's not null, wn is the wn of the load
//  and update non_const_loops for wn
void ACCESS_VECTOR::Add_Symbol(INT64 coeff, SYMBOL symbol, 
				DOLOOP_STACK *stack, WN *wn)
{
  if (wn && TY_is_volatile(WN_ty(wn))) {
    Too_Messy = TRUE;
    return;
  }

  if ((coeff >= (INT32_MAX-1)) || (coeff<=(INT32_MIN+1))) {
    Too_Messy = TRUE;  // Overflow
    return;
  }

  // check to see if it's a loop variable
  BOOL is_iv=FALSE;
  INT32 i;

  for (i=0; i<stack->Elements() && !is_iv; i++) {
    WN *do_wn = stack->Bottom_nth(i);
    SYMBOL doloop(WN_index(do_wn));
    if (symbol == doloop) {
      is_iv = TRUE;
    }
  }

  if (is_iv) { // it's an induction variable
    if (_lcoeff == NULL) {
      _lcoeff = CXX_NEW_ARRAY(mINT32,_nest_depth,_mem_pool);
      for (INT j=0; j<_nest_depth; j++) {
        _lcoeff[j] = 0;
      }
    }
    coeff += _lcoeff[i-1];
    if ((coeff >= (INT32_MAX-1)) || (coeff<=(INT32_MIN+1))) {
      Too_Messy = TRUE;  // Overflow
      return;
    }
    _lcoeff[i-1] = coeff;
  } else {	// it's a symbolic
    if (Lin_Symb == NULL) {
      Lin_Symb = CXX_NEW(INTSYMB_LIST,_mem_pool);
    } else {
      // check to see if it's already on the linear list
      INTSYMB_ITER iter(Lin_Symb);
      INTSYMB_NODE* prevnode = NULL; 
      for (INTSYMB_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
        if (node->Symbol == symbol) {
          coeff += node->Coeff;
          if ((coeff >= (INT32_MAX-1)) || (coeff<=(INT32_MIN+1))) {
            Too_Messy = TRUE;  // Overflow
            return;
          }
          node->Coeff = coeff;
	  if (node->Coeff == 0) { // get rid of it
	    if (node == iter.First()) {
	      CXX_DELETE(Lin_Symb->Remove_Headnode(),_mem_pool);
	    } else {
	      CXX_DELETE(Lin_Symb->Remove(prevnode,node),_mem_pool);
	    }
	  }
	  if (wn) Update_Non_Const_Loops(wn,stack);
          return;
        }
	prevnode = node; 
      }
    }
    // it's a new symbol, so add it
    Lin_Symb->Prepend(CXX_NEW(INTSYMB_NODE(symbol,coeff),_mem_pool));
    if (wn) Update_Non_Const_Loops(wn,stack);
  } 
}

// add in the non-linear terms
// given the input list, return the output list resulting from multiplying
// the code under wn by the input list
// can't pass in NULL
// can return NULL, in which case Too_Messy must be set
SUMPROD_LIST *ACCESS_VECTOR::Add_Nonlin(WN *wn, SUMPROD_LIST *list,
				DOLOOP_STACK *stack)
{
  Is_True(list,("Null input list in ACCESS_VECTOR::Add_Nonlin"));
  if (Too_Messy) return NULL;
  OPERATOR oper = WN_operator(wn);
  if (oper == OPR_ADD) {
    SUMPROD_LIST *list2 = CXX_NEW(SUMPROD_LIST(list,_mem_pool), 
						&LNO_local_pool);
    list = Add_Nonlin(WN_kid0(wn),list,stack);
    list2 = Add_Nonlin(WN_kid1(wn),list2,stack);
    if (!Too_Messy) list->Merge(list2);
    return list;
  } 
  if (oper == OPR_SUB) {
    SUMPROD_LIST *list2 = CXX_NEW(SUMPROD_LIST(list,_mem_pool), 
						&LNO_local_pool);
    list2 = Add_Nonlin(WN_kid1(wn),list2,stack);
    if (!list2->Negate_Me()) {
      Too_Messy = TRUE;
      return NULL;
    }
    list = Add_Nonlin(WN_kid0(wn),list,stack);
    if (!Too_Messy) list->Merge(list2);
    return list;
  }
  if (oper == OPR_NEG) {
    if (!list->Negate_Me()) {
      Too_Messy = TRUE;
      return NULL;
    }
    return Add_Nonlin(WN_kid0(wn),list,stack);
  }
  if (oper == OPR_MPY) {
    list = Add_Nonlin(WN_kid0(wn),list,stack);
    if (list) list = Add_Nonlin(WN_kid1(wn),list,stack);
    return list;
  }
  if (oper == OPR_INTCONST) {
    INT64 offset = WN_const_val(wn);
    SUMPROD_ITER iter(list);
    for (SUMPROD_NODE *node = iter.First(); !iter.Is_Empty();
				node = iter.Next()) {
      INT64 coeff = node->Coeff*offset;
      if ((coeff >= (INT32_MAX-1)) || (coeff<=(INT32_MIN+1))) {
        Too_Messy = TRUE;  // Overflow
        return NULL;
      }
      node->Coeff = coeff;
    }
    return list;
  }
  if (oper == OPR_LDID) {
    SYMBOL symbol(wn);
    SUMPROD_ITER iter(list);

    // check to see if it's a loop variable
    BOOL is_iv=FALSE;
    for (INT32 i=0; i<stack->Elements() && !is_iv; i++) {
      WN *wn = stack->Bottom_nth(i);
      SYMBOL doloop(WN_index(wn));
      if (symbol == doloop) {
        is_iv = TRUE;
      }
    }

    for (SUMPROD_NODE *node = iter.First(); !iter.Is_Empty();
				node = iter.Next()) {
      node->Prod_List->Append(CXX_NEW(SYMBOL_NODE(symbol,is_iv),_mem_pool));
    }

    // if it's not an iv, update Non_Const_Loops
    // we'll deal with iv's after we try to delinearize
    if (!is_iv) {
      Update_Non_Const_Loops(wn,stack);
    }
    return list;
  }
  Too_Messy = TRUE;
  return NULL;
}

// for all the loads inside the expression node wn, 
// use the def-use chains to update Non_Const_Loops
void ACCESS_VECTOR::Update_Non_Const_Loops(WN *wn, DOLOOP_STACK *stack)
{
  OPCODE opc = WN_opcode(wn);
  if (OPCODE_is_load(opc)) {
    if (OPCODE_operator(opc) != OPR_LDID) {
      _non_const_loops = stack->Elements();
      return;
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Update_Non_Const_Loops(WN_kid(wn,kidno),stack);
    }
  }

  // it's an ldid

  DEF_LIST *defs = Du_Mgr->Ud_Get_Def(wn);

  // nenad, 02/15/2000: 
  // We should also set _non_const_loops conservatively if
  // defs->Incomplete(), but that causes performance problems.
  if (!defs) {
    _non_const_loops = stack->Elements();
    return;
  }
  DEF_LIST_ITER iter(defs);

  for(DU_NODE *node=iter.First(); !iter.Is_Empty(); node=iter.Next()) {
    WN *def = node->Wn();

    // find the inner loop surrounding the def
    while (def && (WN_opcode(def) != OPC_DO_LOOP)) {
      def = LWN_Get_Parent(def);
    }
    if (def) {  // there is a do loop surrounding the def, find out which one
      def = LNO_Common_Loop(def,wn);
      if (def) {
        INT i=0;
        INT num_elements = stack->Elements();
        while ((i < num_elements) && (def != stack->Bottom_nth(i))) {
	  i++;
        }
        if (i < num_elements) {  // it varies in an ancestor loops
          _non_const_loops = MAX(_non_const_loops,i+1);
        }
      }
    }
  }
}

// for all the loads inside the expression node wn, 
// use the def-use chains to update Non_Const_Loops for every access
// vector in this array (this is used when the base or the bounds vary)
void ACCESS_ARRAY::Update_Non_Const_Loops(WN *wn, DOLOOP_STACK *stack)
{
  OPCODE opc = WN_opcode(wn);
  if (OPCODE_is_load(opc)) {
    if (OPCODE_operator(opc) != OPR_LDID) {
      for (INT32 i=0; i<_num_vec; i++) {
        Dim(i)->Max_Non_Const_Loops(stack->Elements());
      }
      return;
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Update_Non_Const_Loops(WN_kid(wn,kidno),stack);
    }
    return;
  }

  // it's an ldid
  DEF_LIST *defs = Du_Mgr->Ud_Get_Def(wn);

  // nenad, 02/15/2000: 
  // We should also set _non_const_loops conservatively if
  // defs->Incomplete(), but that causes performance problems.
  if (!defs) {
    for (INT32 i=0; i<_num_vec; i++) {
      Dim(i)->Max_Non_Const_Loops(stack->Elements());
    }
    return;
  }


  DEF_LIST_ITER iter(defs);

  INT max = 0;
  for(const DU_NODE *node=iter.First(); !iter.Is_Empty(); node=iter.Next()) {
    const WN *def = node->Wn();

    // find the inner loop surrounding the def
    while (def && (WN_opcode(def) != OPC_DO_LOOP)) {
      def = LWN_Get_Parent(def);
    }
    if (def) {  // there is a do loop surrounding the def, find out which one
      INT i=0;
      INT num_elements = stack->Elements();
      while ((i < num_elements) && (def != stack->Bottom_nth(i))) {
	i++;
      }
      if (i < num_elements) {  // it varies in an ancestor loops
        max = MAX(max,i+1);
      }
    }
  }
  if (max > 0) {
    for (INT32 i=0; i<_num_vec; i++) {
      Dim(i)->Max_Non_Const_Loops(max);
    }
  }
}



ACCESS_VECTOR *Subtract(ACCESS_VECTOR *v1, ACCESS_VECTOR *v2,
			MEM_POOL *mem_pool)
{
  Is_True(v1 && v2, ("Access vector subtraction requires non-nil operands"));

  if (v1->_nest_depth != v2->_nest_depth)
    return NULL;

  ACCESS_VECTOR *rv = CXX_NEW(ACCESS_VECTOR, mem_pool);

  rv->Too_Messy = (v1->Too_Messy || v2->Too_Messy);

  if (rv->Too_Messy)
    return rv;

  // TODO actually, we want to compute the rv and then call some function
  // to compute the actual depth.  That function has not been written yet,
  // though, so I'm doing something conservative.

  rv->_non_const_loops = MAX(v1->_non_const_loops, v2->_non_const_loops);
  rv->_nest_depth = v1->_nest_depth;
  rv->_mem_pool = mem_pool;
  rv->Const_Offset = v1->Const_Offset - v2->Const_Offset;

  rv->_lcoeff = CXX_NEW_ARRAY(mINT32, rv->_nest_depth, rv->_mem_pool);
  for (INT i=0; i < rv->_nest_depth; i++)
    rv->_lcoeff[i] = (v1->_lcoeff ? v1->_lcoeff[i] : 0) -
                     (v2->_lcoeff ? v2->_lcoeff[i] : 0);

  rv->Lin_Symb = Subtract(v1->Lin_Symb, v2->Lin_Symb, rv->_mem_pool);

  rv->Non_Lin_Symb = CXX_NEW(SUMPROD_LIST, rv->_mem_pool);
  if (v1->Non_Lin_Symb)
    rv->Non_Lin_Symb->Init(v1->Non_Lin_Symb, rv->_mem_pool);
  if (v2->Non_Lin_Symb) {
    SUMPROD_ITER iter(v2->Non_Lin_Symb);
    for (SUMPROD_NODE *node = iter.First(); !iter.Is_Empty(); 
	 node=iter.Next()) {
      // TODO again conservative, because in theory an entry on v1 and v2
      // could cancel each other.
      SUMPROD_NODE *n = CXX_NEW(SUMPROD_NODE(node,mem_pool),mem_pool);
      n->Coeff = - n->Coeff;
      rv->Non_Lin_Symb->Append(n);
    }
  }

  return rv;
}

ACCESS_VECTOR *Add(ACCESS_VECTOR *v1, ACCESS_VECTOR *v2,
			MEM_POOL *mem_pool)
{
  Is_True(v1 && v2, ("Access vector subtraction requires non-nil operands"));

  if (v1->_nest_depth != v2->_nest_depth)
    return NULL;

  ACCESS_VECTOR *rv = CXX_NEW(ACCESS_VECTOR, mem_pool);

  rv->Too_Messy = (v1->Too_Messy || v2->Too_Messy);

  if (rv->Too_Messy)
    return rv;

  // TODO actually, we want to compute the rv and then call some function
  // to compute the actual depth.  That function has not been written yet,
  // though, so I'm doing something conservative.

  rv->_non_const_loops = MAX(v1->_non_const_loops, v2->_non_const_loops);
  rv->_nest_depth = v1->_nest_depth;
  rv->_mem_pool = mem_pool;
  rv->Const_Offset = v1->Const_Offset + v2->Const_Offset;

  rv->_lcoeff = CXX_NEW_ARRAY(mINT32, rv->_nest_depth, rv->_mem_pool);
  for (INT i=0; i < rv->_nest_depth; i++)
    rv->_lcoeff[i] = (v1->_lcoeff ? v1->_lcoeff[i] : 0) +
                     (v2->_lcoeff ? v2->_lcoeff[i] : 0);

  rv->Lin_Symb = Add(v1->Lin_Symb, v2->Lin_Symb, rv->_mem_pool);

  rv->Non_Lin_Symb = CXX_NEW(SUMPROD_LIST, rv->_mem_pool);
  if (v1->Non_Lin_Symb)
    rv->Non_Lin_Symb->Init(v1->Non_Lin_Symb, rv->_mem_pool);
  if (v2->Non_Lin_Symb) {
    SUMPROD_ITER iter(v2->Non_Lin_Symb);
    for (SUMPROD_NODE *node = iter.First(); !iter.Is_Empty(); 
	 node=iter.Next()) {
      // TODO again conservative, because in theory an entry on v1 and v2
      // could cancel each other.
      SUMPROD_NODE *n = CXX_NEW(SUMPROD_NODE(node,mem_pool),mem_pool);
      rv->Non_Lin_Symb->Append(n);
    }
  }

  return rv;
}

ACCESS_VECTOR *Mul(INT c, ACCESS_VECTOR *v, MEM_POOL *mem_pool)
{
  Is_True(v, ("Access vector multiplication requires non-nil operand"));

  ACCESS_VECTOR *rv = CXX_NEW(ACCESS_VECTOR(v, mem_pool), mem_pool);

  if (rv->Too_Messy)
    return rv;

  for (INT i=0; i< rv->_nest_depth; i++)
    rv->_lcoeff[i] *= c;

  rv->Lin_Symb = Mul(c, v->Lin_Symb, rv->_mem_pool);

  if (v->Non_Lin_Symb) {
    SUMPROD_ITER iter(v->Non_Lin_Symb);
    for (SUMPROD_NODE *node = iter.First(); !iter.Is_Empty();
	 node=iter.Next()) {
      node->Coeff *= c;
    }
  }

  return rv;
}

ACCESS_VECTOR *Merge(ACCESS_VECTOR *v1, ACCESS_VECTOR *v2,
		     MEM_POOL *mem_pool)
{
  Is_True(v1 && v2, ("Access vector subtraction requires non-nil operands"));

//  if (v1->_nest_depth != v2->_nest_depth)
//   return NULL;

  ACCESS_VECTOR *rv = CXX_NEW(ACCESS_VECTOR, mem_pool);

  // Use the mininum nest_depth as the new depth
  rv->_nest_depth = MIN(v1->_nest_depth, v2->_nest_depth);
  rv->Too_Messy = (v1->Too_Messy || v2->Too_Messy);

  if (rv->Too_Messy)
    return rv;

  rv->_non_const_loops = MAX(v1->_non_const_loops, v2->_non_const_loops);
  rv->_mem_pool = mem_pool;
  rv->Const_Offset = v1->Const_Offset + v2->Const_Offset;

  rv->_lcoeff = CXX_NEW_ARRAY(mINT32, rv->_nest_depth, rv->_mem_pool);
  for (INT i=0; i < rv->_nest_depth; i++)
    rv->_lcoeff[i] = (v1->_lcoeff ? v1->_lcoeff[i] : 0) +
                     (v2->_lcoeff ? v2->_lcoeff[i] : 0);

  rv->Lin_Symb = Add(v1->Lin_Symb, v2->Lin_Symb, rv->_mem_pool);

  rv->Non_Lin_Symb = CXX_NEW(SUMPROD_LIST, rv->_mem_pool);
  if (v1->Non_Lin_Symb)
    rv->Non_Lin_Symb->Init(v1->Non_Lin_Symb, rv->_mem_pool);
  if (v2->Non_Lin_Symb) {
    SUMPROD_ITER iter(v2->Non_Lin_Symb);
    for (SUMPROD_NODE *node = iter.First(); !iter.Is_Empty(); 
	 node=iter.Next()) {
      // TODO again conservative, because in theory an entry on v1 and v2
      // could cancel each other.
      SUMPROD_NODE *n = CXX_NEW(SUMPROD_NODE(node,mem_pool),mem_pool);
      rv->Non_Lin_Symb->Append(n);
    }
  }

  return rv;
}

void ACCESS_VECTOR::Mul(INT c)
{
  if (Too_Messy) return;
  for (INT i=0; i< _nest_depth; i++) {
    if (_lcoeff[i]) {
      INT64 prod = _lcoeff[i] * c;
      if (prod < INT32_MAX) {
	_lcoeff[i] = prod;
      } else {
	Too_Messy = TRUE;
	return;
      }
    }
  }
  if (Lin_Symb) {
    INTSYMB_ITER ii(Lin_Symb);
    for (INTSYMB_NODE *in = ii.First(); !ii.Is_Empty(); in = ii.Next()) {
      if (in->Coeff == 1) {
	in->Coeff = c;
      } else {
        INT64 prod = in->Coeff * c;
        if (prod < INT32_MAX) {
	  in->Coeff = prod;
        } else {
	  Too_Messy = TRUE;
	  return;
        }
      }
    }
  }
  if (Non_Lin_Symb) {
    SUMPROD_ITER iter(Non_Lin_Symb);
    for (SUMPROD_NODE *node = iter.First(); !iter.Is_Empty();
	 node=iter.Next()) {
      if (node->Coeff == 1) {
	node->Coeff = c;
      } else {
        INT64 prod = node->Coeff * c;
        if (prod < INT32_MAX) {
	  node->Coeff = prod;
        } else {
	  Too_Messy = TRUE;
	  return;
        }
      }
    }
  }
}

void ACCESS_VECTOR::Negate_Me()
{
  if (Too_Messy)
    return;

  Const_Offset = -Const_Offset;

  if (_lcoeff) {
    for (INT i=0; i<_nest_depth; i++)
      _lcoeff[i] = -_lcoeff[i];
  }

  if (Contains_Lin_Symb()) {
    INTSYMB_ITER ii(Lin_Symb);
    for (INTSYMB_NODE *in = ii.First(); !ii.Is_Empty(); in = ii.Next())
      in->Coeff = -in->Coeff;
  }

  if (Contains_Non_Lin_Symb()) {
    SUMPROD_ITER si(Non_Lin_Symb);
    for (SUMPROD_NODE *sn = si.First(); !si.Is_Empty(); sn = si.Next())
      sn->Coeff = -sn->Coeff;
  }
}

ACCESS_VECTOR *ACCESS_VECTOR::Convert_Bound_To_Exp(MEM_POOL *pool)
{
  ACCESS_VECTOR *result = CXX_NEW(ACCESS_VECTOR(this,pool), pool);
  if (Too_Messy) return(result);

  if(_lcoeff[_nest_depth-1] > 0) { // an upper bound, negate all the 
				   // loop variables and symbols
    for (INT i=0; i<_nest_depth-1; i++) {
      result->_lcoeff[i] = -_lcoeff[i];
    }
    INTSYMB_ITER ii(result->Lin_Symb);
    for (INTSYMB_NODE *in = ii.First(); !ii.Is_Empty(); in = ii.Next())
      in->Coeff = -in->Coeff;
  
    SUMPROD_ITER si(result->Non_Lin_Symb);
    for (SUMPROD_NODE *sn = si.First(); !si.Is_Empty(); sn = si.Next())
      sn->Coeff = -sn->Coeff;
  } else { // a lower bound, just negate the constant
    result->Const_Offset = -result->Const_Offset;
  }
  result->_lcoeff[_nest_depth-1] = 0;

  return(result);
}


// How many maxs are there rooted in this tree
// Only count maxs which are either top level or children of maxs
extern INT Num_Maxs(WN *wn)
{
  if (WN_operator(wn) == OPR_MAX) {
    return(1+Num_Maxs(WN_kid(wn,0))+Num_Maxs(WN_kid(wn,1)));
  } else {
    return 0;
  }
}

// How many mins are there rooted in this tree
// Only count mins which are either top level or children of mins
extern INT Num_Mins(WN *wn)
{
  if (WN_operator(wn) == OPR_MIN) {
    return(1+Num_Mins(WN_kid(wn,0))+Num_Mins(WN_kid(wn,1)));
  } else {
    return 0;
  }
}

// How many logical ands are there rooted in this tree
// Only count LANDs which are either top level or children of LANDs
INT Num_Lands(WN *wn)
{
  if (WN_operator(wn) == OPR_LAND
      || WN_operator(wn) == OPR_CAND) {
    return(1+Num_Lands(WN_kid(wn,0))+Num_Lands(WN_kid(wn,1)));
  } else {
    return 0;
  }
}

// How many logical ors are there rooted in this tree
// Only count LIORs which are either top level or children of LIORs
INT Num_Liors(WN *wn)
{
  if (WN_operator(wn) == OPR_LIOR
      || WN_operator(wn) == OPR_CIOR) {
    return(1+Num_Liors(WN_kid(wn,0))+Num_Liors(WN_kid(wn,1)));
  } else {
    return 0;
  }
}


//=======================================================================
//
// Build an access vector according to line 'i' of 'soe',
// using the symbols in 'syms' for linear terms.  The dimension of the system
// is given in 'dim', the number of enclosing loops is 'depth'.
// Which array to use for coefficients is controlled by 'which_array':
//   which_array=0 (Work) which_array=1 (Eq) which_array=2 (Le).
// This is very ugly indeed.
//
//=======================================================================
ACCESS_VECTOR::ACCESS_VECTOR(const SYSTEM_OF_EQUATIONS *soe,
			     const INT i, const SYMBOL_LIST *syms,
			     const INT depth, const INT dim,
			     const INT non_const_loops,
			     const INT which_array,
			     BOOL is_lower_bound, MEM_POOL *pool)
{
  INT k;

  _mem_pool = pool;
  _nest_depth = depth;
  _non_const_loops = non_const_loops;

  // TODO: need to fix Delinearized_Symbol
  Too_Messy = FALSE;
  Non_Lin_Symb = NULL;
  Lin_Symb = NULL;
  Delinearized_Symbol = NULL;
  _lcoeff = CXX_NEW_ARRAY(mINT32,_nest_depth,_mem_pool);

  switch (which_array) {
  case 0:
    {
 
      if (is_lower_bound) {
	for (k = 0; k < _nest_depth; ++k) 
	  _lcoeff[k] = soe->Work(i,dim+k);
	Const_Offset = -soe->Work_Const(i);
      
	// Set the Lin_Symb list
	SYMBOL_CONST_ITER iter(syms);

	for (const SYMBOL_NODE *s = iter.First(); 
	     k+dim < soe->Num_Vars() && !iter.Is_Empty();
	     ++k, s = iter.Next()) {
	  if (soe->Work(i,k+dim)) {
	    if (Lin_Symb == NULL) Lin_Symb = CXX_NEW(INTSYMB_LIST, _mem_pool);
	    Lin_Symb->Append(CXX_NEW(INTSYMB_NODE(s->Symbol,soe->Work(i,k+dim)),_mem_pool));
	  }
	} 
      } else {
	for (k = 0; k < _nest_depth; ++k) 
	  _lcoeff[k] = -soe->Work(i,dim+k);
	Const_Offset = soe->Work_Const(i);
    
	// Set the Lin_Symb list
	SYMBOL_CONST_ITER iter(syms);
	
	for (const SYMBOL_NODE *s = iter.First(); 
	     k+dim < soe->Num_Vars() && !iter.Is_Empty();
	     ++k, s = iter.Next()) {
	  if (soe->Work(i,k+dim)) {
	    if (Lin_Symb == NULL) Lin_Symb = CXX_NEW(INTSYMB_LIST, _mem_pool);
	    Lin_Symb->Append(CXX_NEW(INTSYMB_NODE(s->Symbol,-soe->Work(i,k+dim)),_mem_pool));
	  }
	}
      }
    }
    break;

  case 1:
    {
      const IMAT &aeq = soe->Aeq();
      const INT64 *beq = soe->Beq();

      // Must be lower_bound
      for (k = 0; k < _nest_depth; ++k) 
	_lcoeff[k] = -aeq(i,dim+k);
      Const_Offset = beq[i];
    
      // Set the Lin_Symb list
      SYMBOL_CONST_ITER iter(syms);

      for (const SYMBOL_NODE *s = iter.First(); 
	   k+dim < soe->Num_Vars() && !iter.Is_Empty();
	   ++k, s = iter.Next()) {
	if (aeq(i,k+dim)) {
	  if (Lin_Symb == NULL) Lin_Symb = CXX_NEW(INTSYMB_LIST, _mem_pool);
	  Lin_Symb->Append(CXX_NEW(INTSYMB_NODE(s->Symbol,aeq(i,k+dim)),_mem_pool));
	}
      } 
    }
    break;

  case 2:
    {
      const IMAT &ale = soe->Ale();
      const INT64 *ble = soe->Ble();

      if (is_lower_bound) {
	for (k = 0; k < _nest_depth; ++k) 
	  _lcoeff[k] = ale(i,dim+k);
	Const_Offset = -ble[i];
    
	// Set the Lin_Symb list
	SYMBOL_CONST_ITER iter(syms);

	for (const SYMBOL_NODE *s = iter.First(); 
	     k+dim < soe->Num_Vars() && !iter.Is_Empty();
	     ++k, s = iter.Next()) {
	  if (ale(i,k+dim)) {
	    if (Lin_Symb == NULL) Lin_Symb = CXX_NEW(INTSYMB_LIST, _mem_pool);
	    Lin_Symb->Append(CXX_NEW(INTSYMB_NODE(s->Symbol,ale(i,k+dim)),_mem_pool));
	  }
	} 
      } else {
	for (k = 0; k < _nest_depth; ++k) 
	  _lcoeff[k] = -ale(i,dim+k);
	Const_Offset = ble[i];
    
	// Set the Lin_Symb list
	SYMBOL_CONST_ITER iter(syms);

	for (const SYMBOL_NODE *s = iter.First(); 
	     k+dim < soe->Num_Vars() && !iter.Is_Empty();
	     ++k, s = iter.Next()) {
	  if (ale(i,k+dim)) {
	    if (Lin_Symb == NULL) Lin_Symb = CXX_NEW(INTSYMB_LIST, _mem_pool);
	    Lin_Symb->Append(CXX_NEW(INTSYMB_NODE(s->Symbol,-ale(i,k+dim)),_mem_pool));
	  }
	}
      }
    }

    break;

  }

}

#if !defined(LNO) || !defined(SHARED_BUILD)
//-----------------------------------------------------------------
// initialize static variables to be used by the access vector utils,
// if invoked from outside of LNO
//-----------------------------------------------------------------
void
Initialize_Access_Vals(DU_MANAGER* du_mgr, FILE *tfile)
{
  Du_Mgr = du_mgr;
  Set_Trace_File_internal(tfile);
  MEM_POOL_Initialize(&LNO_local_pool, "Access_Vector_Pool", FALSE);
  MEM_POOL_Push (&LNO_local_pool);
}

//-----------------------------------------------------------------
// finalize variables, i.e. delete the mempool when done with the
// access utils, if invoked from outside of LNO
//-----------------------------------------------------------------
void
Finalize_Access_Vals()
{
  MEM_POOL_Pop(&LNO_local_pool);
  MEM_POOL_Delete(&LNO_local_pool);
}

#endif

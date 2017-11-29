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

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include <alloca.h>

#include "pu_info.h"
#include "opt_du.h"
#include "lego_util.h"
#include "lego_opts.h"
#include "lnoutils.h"
#include "lego_pragma.h"
#include "lwn_util.h"
#include "lego_affinity.h"
#include "lnopt_main.h"
#include "snl_utils.h"

/*======================================================================
 *
 * Local declarations.
 *
 *======================================================================*/

static void Do_Loop_Explicit_Affinity (WN* loop);
static void Do_Loop_Implicit_Affinity (WN* loop);



/***********************************************************************
 *
 * Walk the entire PU, and for each loop, annotate it with
 * an affinity lego_info if appropriate.
 *
 ***********************************************************************/

static void Whack_Do_Loops_Traverse(WN* wn_tree)
{
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree);
    Is_True(dli, ("No DO_LOOP_INFO for this loop"));
    LEGO_INFO* li = dli->Lego_Info;
    if (li)   
      Do_Loop_Explicit_Affinity(wn_tree);
    else      
      Do_Loop_Implicit_Affinity(wn_tree);
    if (Debug_Lego && dli->Lego_Info) 
      (dli->Lego_Info)->Print(stdout);
  }

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Whack_Do_Loops_Traverse(wn);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      Whack_Do_Loops_Traverse(WN_kid(wn_tree, i));
  }

  //
  // Get rid of inconsistent mappings. 
  //
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree);
    if (dli->Mp_Info && dli->Mp_Info->Nest_Index() == 0) {
      INT lego_count = 0; 
      for (INT i = 0; i < dli->Mp_Info->Nest_Total(); i++) {
	WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_tree, i + 1);
        DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn_inner);
        FmtAssert(dli_inner->Mp_Info != NULL, ("Could not find Mp_Info"));
        FmtAssert(dli_inner->Mp_Info->Nest_Index() == i, 
          ("Did not find the right do loop in the nest"));
        if (dli_inner->Lego_Info != NULL) 
          lego_count++; 
      }
      if (lego_count != 0 && lego_count != dli->Mp_Info->Nest_Total()) {
        for (INT i = 0; i < dli->Mp_Info->Nest_Total(); i++) {
	  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_tree, i + 1);
	  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn_inner);
	  if (dli_inner->Lego_Info != NULL) {
	    CXX_DELETE(dli_inner->Lego_Info, LEGO_pool);
	    dli_inner->Lego_Info = NULL; 
	  }
	}
      } 
    }
  }
}         
        
extern void Whack_Do_Loops(WN* func_nd) 
{
  Whack_Do_Loops_Traverse(func_nd);
}

/***********************************************************************
 *
 * Fill in the front_peel and back_peel of LEGO_INFO according to the
 * affinity directives to the loops
 *
 ***********************************************************************/
static void Do_Loop_Explicit_Affinity (WN* loop)
{

  DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
  Is_True(dli, ("Do_Loop_Explicit_Affinity: No DO_LOOP_INFO for this loop"));

  LEGO_INFO* li = dli->Lego_Info;
  if (!li || !li->Array() || li->Dynamic_Affinity()) return;

  SYMBOL* affinity_array = li->Array();
  ST* st = affinity_array->St();
  DISTR_ARRAY * dact = Lookup_DACT(st);
  Is_True(dact, ("Do_Loop_Explicit_Affinity: Array is not distributed"));

  DISTRIBUTE_TYPE distr_type = dact->Get_Dim(li->Dim_Num())->Distr_Type();
  if (distr_type == DISTRIBUTE_STAR) {
    DevWarn("Affinity on not distributed dimension has no effect \n");
    return;
  }
   
  INT loop_depth = dli->Depth;
  INT affinity_stride = li->Stride();
  INT affinity_offset = li->Offset();
  INT front_peel = 0;
  INT back_peel = 0;
  
  LWN_ITER* wniter = LWN_WALK_TreeIter(loop);
  while (wniter) {
    WN* wn = wniter->wn;
    wniter = LWN_WALK_TreeNext(wniter);
    OPERATOR opr = WN_operator(wn);
    
    if (((opr == OPR_ILOAD) &&
         (WN_operator(WN_kid0(wn)) == OPR_ARRAY)) ||
        ((opr == OPR_PARM) &&
         (WN_operator(WN_kid0(wn)) == OPR_ARRAY)) ||
        ((opr == OPR_ISTORE) &&
         (WN_operator(WN_kid1(wn)) == OPR_ARRAY))) {

      wn = (opr==OPR_ILOAD || opr==OPR_PARM) ? WN_kid0(wn) : WN_kid1(wn);

//      if (st != WN_st(WN_array_base(wn))) continue;

      // Test if the current array is reshaped
      ST*  cur_st = (WN_has_sym(WN_array_base(wn)) ?
                     WN_st(WN_array_base(wn)) :
                     NULL);
      DISTR_ARRAY * cur_dact = Lookup_DACT(st);
      if ((dact == NULL) || (!dact->Dinfo()->IsReshaped())) continue;
      
      ACCESS_ARRAY* aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn);
      if (aa == NULL || aa->Too_Messy) continue;
	
      // A reshaped array with good access array
      for (UINT i = 0; i< aa->Num_Vec(); ++i) {
	
        DISTRIBUTE_TYPE distr_type = cur_dact->Get_Dim(i)->Distr_Type();
        if (distr_type == DISTRIBUTE_STAR) continue;

        // Test if the distributed dimension is conforming to loop affinity
        if (!dact->DACT_Equiv(cur_dact, li->Dim_Num(), i)) continue;
	
        // Found a conforming reshaped dimension
        ACCESS_VECTOR* av = aa->Dim(i);
        if (av->Too_Messy ||
	    av->Contains_Lin_Symb() ||
            av->Contains_Non_Lin_Symb() ||
            !av->Has_Loop_Coeff() ||
            av->Loop_Coeff(loop_depth) != affinity_stride) {
          continue;
        }

        // Now, for the distributed dimension, determine the if
        // only the 'loop' has coefficient and its value is the same
        // as the li->Stride();
        BOOL is_good = TRUE;
        for (INT j = 0; j<av->Nest_Depth(); ++j) {
          if (j!=loop_depth && av->Loop_Coeff(j)!=0) {
            is_good = FALSE;
            break;
          }
        }
  
        if (!is_good) continue;

        // Determine the front_peel and back_peel
        INT new_offset = av->Const_Offset - affinity_offset;
        if (new_offset < front_peel) front_peel = new_offset;
        if (new_offset > back_peel) back_peel = new_offset;

      }
    }
  }
  
  if (distr_type == DISTRIBUTE_BLOCK) {
    if (li->Stride() >= 0) {
      li->Set_Front_Peel(-front_peel);
      li->Set_Back_Peel(back_peel);
    }
    else {
      // flip the peel if -ve stride
      li->Set_Front_Peel(back_peel);
      li->Set_Back_Peel(-front_peel);
    }
  } else {
    li->Set_Min_Offset(front_peel);
    li->Set_Max_Offset(back_peel);
  }
  
  return;

}

/****************************************************************************
 *
 * Process loop without affinity directives to determine affinity
 *
 ****************************************************************************/
static void Do_Loop_Implicit_Affinity (WN* loop)
{

  // don't determine affinity if the loop bounds are bad
  // since we cannot tile appropriately anyway.
  if (!Loop_Bounds_Simple(loop)) {
    return;
  }
  
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
  Is_True(dli, ("Do_Loop_Implicit_Affinity: No DO_LOOP_INFO for this loop"));

  Is_True(dli->Lego_Info==NULL, ("Do_Loop_Implicit_Affinity: DO_LOOP_INFO already has Lego_Info"));
  LEGO_INFO* li = CXX_NEW(LEGO_INFO(NULL),LEGO_pool);

  LEGO_AFFINITY affinity;
  INT loop_depth = dli->Depth;
  LWN_ITER* wniter = LWN_WALK_TreeIter(loop);
  while (wniter) {
    WN* wn = wniter->wn;
    wniter = LWN_WALK_TreeNext(wniter);
    OPERATOR opr = WN_operator(wn);
    
    if (((opr == OPR_ILOAD) &&
         (WN_operator(WN_kid0(wn)) == OPR_ARRAY)) ||
        ((opr == OPR_PARM) &&
         (WN_operator(WN_kid0(wn)) == OPR_ARRAY)) ||
        ((opr == OPR_ISTORE) &&
         (WN_operator(WN_kid1(wn)) == OPR_ARRAY))) {
    
      wn = (opr==OPR_ILOAD || opr==OPR_PARM) ? WN_kid0(wn) : WN_kid1(wn);

      WN *array_base = WN_array_base(wn);
	
      ST* st = (WN_has_sym(array_base) ? WN_st(array_base) : NULL);
      DISTR_ARRAY * dact = Lookup_DACT(st);
      if ((dact == NULL) || (!dact->Dinfo()->IsReshaped())) continue;
    
      ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn);
      if (aa == NULL || aa->Too_Messy) continue;
    
      // Found a reshaped array with good access array
      for (UINT i=0; i<aa->Num_Vec(); ++i) {

        DISTRIBUTE_TYPE distr_type = dact->Get_Dim(i)->Distr_Type();
        if (distr_type == DISTRIBUTE_STAR) continue;

        ACCESS_VECTOR* av = aa->Dim(i);
        if (av->Too_Messy ||
            !av->Has_Loop_Coeff() ||
            av->Loop_Coeff(loop_depth)==0 ||
	    av->Contains_Lin_Symb() ||
            av->Contains_Non_Lin_Symb()) 
          continue;

        BOOL is_good=TRUE;
        for (INT j = 0; j<av->Nest_Depth(); ++j) {
          if ((j!=loop_depth && av->Loop_Coeff(j)!=0)) {
            is_good = FALSE;
            break;
          }
        }

        // support negative stride only with block-distribution.
        if (av->Loop_Coeff(loop_depth)<0 && distr_type != DISTRIBUTE_BLOCK)
          continue;
      
        // Found a good access_vector and the dimension is distributed
        if (is_good) 
          affinity.Add_Ref(wn, distr_type, av, i, loop_depth);

      }

    }
  }
  
  affinity.Pick_Affinity(li);

  if (li->Array()==NULL) {
    CXX_DELETE(li,LEGO_pool);
  }
  else {
    if (li->Stride() < 0) {
      // -ve stride, so flip the peels
      INT32 tmp = li->Front_Peel();
      li->Set_Front_Peel (li->Back_Peel());
      li->Set_Back_Peel (tmp);
    }
    dli->Lego_Info = li;
    if (Debug_Lego) li->Print(stdout);
  }
} /* Do_Loop_Implicit_Affinity */


/****************************************************************************
 *
 * Create a new LEGO_UGS according to wn_array
 *
 ****************************************************************************/
LEGO_UGS::LEGO_UGS(WN* wn_array, DISTRIBUTE_TYPE dtype, ACCESS_VECTOR *av, INT32 dim, INT32 loop_pos):_ref_dims(LEGO_pool),_array_refs(LEGO_pool)
{

#ifdef Is_True_On
  // some error checking
  {
    FmtAssert(av, ("LEGO_UGS: Empty ACCESS_VECTOR"));
    FmtAssert(wn_array, ("LEGO_UGS: Empty array reference"));

    ACCESS_ARRAY * aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn_array);
    FmtAssert(aa, ("LEGO_UGS: Array ref has no access array"));
    ACCESS_VECTOR *av_x = aa->Dim(dim);
    FmtAssert(av_x==av, ("LEGO_UGS: Array ref dim access vector mismatch"));

    if (av->Too_Messy ||
        av->Contains_Lin_Symb() ||
        av->Contains_Non_Lin_Symb() ||
        !av->Has_Loop_Coeff() || av->Loop_Coeff(loop_pos)==0) 
      FmtAssert(FALSE, ("LEGO_UGS: Array ref dim has bad access vector"));
    
    DISTR_INFO * dinfo = da_hash->Find(WN_has_sym(WN_array_base(wn_array)) ?
                                       WN_st(WN_array_base(wn_array)) :
                                       NULL);
    DISTRIBUTE_TYPE t = dinfo->Get_Dact(0)->Get_Dim(dim)->Distr_Type();
    FmtAssert(t==dtype, ("LEGO_UGS: Array distribution type mismatch"));
  }
#endif

  _av = av;
  _dtype = dtype;
  _loop_pos = loop_pos;
  _min_offset = _max_offset = av->Const_Offset;
  _array_refs.Push(wn_array);
  _ref_dims.Push(dim);

}

/****************************************************************************
 *
 * Among all the offsets, pick the one with the highest votes
 *
 ****************************************************************************/
INT32 LEGO_UGS::Compute_Offset()
{
  if (_min_offset==_max_offset) return _min_offset;
  
  // Now, vote for the most common offset
  // It is important because other offset accesses may access romote memory.
  INT32 size = _max_offset-_min_offset+1;
  INT32 *bucket = CXX_NEW_ARRAY(INT32,size,&LNO_local_pool);
  INT32 i;

  for (i=0; i<size; ++i) {
    bucket[i] = 0;
  }
  
  for (i=0; i<_array_refs.Elements(); ++i) {
    WN* wn = _array_refs.Bottom_nth(i);
    INT32 dim = _ref_dims.Bottom_nth(i);

    ACCESS_ARRAY* aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn);
    ACCESS_VECTOR* av = aa->Dim(dim);
    bucket[av->Const_Offset - _min_offset] += 1;
  }

  INT32 pos = 0;
  INT32 count_max = bucket[pos];
  for (i=1; i<size; ++i) {
    if (bucket[i]>count_max) {
      count_max = bucket[i];
      pos=i;
    }
    else if (bucket[i]==count_max &&
             (abs(i+_min_offset) < abs(pos+_min_offset))) {
      /* This clause ensures that amongst equal vote candidates,
       * we pick the one with the lowest offset.
       */
      pos=i;
    }
  }
  
  return pos+_min_offset;
}

/****************************************************************************
 *
 * Add a wn_array to the LEGO_UGS, return FALSE if not compatible
 *
 ****************************************************************************/
BOOL LEGO_UGS::Add_Ref(WN* wn_array, 
		       DISTRIBUTE_TYPE dtype, 
		       ACCESS_VECTOR *av, 
		       INT32 dim, 
		       INT32 loop_pos)
{
  Is_True(dtype==_dtype,("LEGO_UGS::Add_Ref: Different distr_type"));
  Is_True(loop_pos==_loop_pos,("LEGO_UGS::Add_Ref: Different loop_pos"));
  if (av->Loop_Coeff(loop_pos)!=_av->Loop_Coeff(loop_pos)) return FALSE;
  
  DISTR_ARRAY *dact = Lookup_DACT(WN_has_sym(WN_array_base(wn_array)) ?
                                  WN_st(WN_array_base(wn_array)) :
                                  NULL);
  Is_True(dact,("LEGO_UGS::Add_Ref: array is not reshaped"));
  
  WN* wn_orig = _array_refs.Bottom_nth(0);
  DISTR_ARRAY *dact_orig = Lookup_DACT(WN_has_sym(WN_array_base(wn_orig)) ?
                                       WN_st(WN_array_base(wn_orig)) :
                                       NULL);
  INT dim_orig = _ref_dims.Bottom_nth(0);
  if (!dact_orig->DACT_Equiv(dact, dim_orig, dim)) return FALSE;

  // Same stride and same distribution on the related dimensions
  // See if it is already in this UGS
  for (INT i = 0; i<_array_refs.Elements(); ++i) {
    WN* wn_orig = _array_refs.Bottom_nth(i);
    if (WN_st(WN_array_base(wn_array))==WN_st(WN_array_base(wn_orig))) {

      // They are the same. See if the access_vectors are the same.
      // Because we are looking for minimizing the div and mod, not
      // the remote accesses, we don't have to look at other dimensions.
      INT dim_orig = _ref_dims.Bottom_nth(i);
      ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn_orig);
      ACCESS_VECTOR *av_orig = aa->Dim(dim_orig);
      if (av_orig->Const_Offset==av->Const_Offset) return TRUE;
    }
  }
  
  // If it gets to here, it needs to be added to the stack
  INT offset = av->Const_Offset;
  if (offset<_min_offset) _min_offset = offset;
  if (offset>_max_offset) _max_offset = offset;
  _array_refs.Push(wn_array);
  _ref_dims.Push(dim);

  return TRUE;

}

/****************************************************************************
 *
 * Add wn_array to UGS stacks
 *
 ****************************************************************************/
static void
Add_To_Lego_UGS_Stack(LEGO_UGS_STACK & ugs_stack, 
		      WN* wn_array,   
		      DISTRIBUTE_TYPE dtype, 
		      ACCESS_VECTOR* av,     
		      INT32 dim, 
		      INT32 loop_pos)
{
  for (INT i = 0; i < ugs_stack.Elements(); ++i) 
    if (ugs_stack.Bottom_nth(i)->Add_Ref(wn_array, dtype, av, dim, loop_pos))
	return;
  
  // Create a new UGS if it gets to here
  ugs_stack.Push(CXX_NEW(LEGO_UGS(wn_array, dtype, av, dim, loop_pos),LEGO_pool));
  
  return;

}

LEGO_AFFINITY::LEGO_AFFINITY():_block(LEGO_pool),_cyclic(LEGO_pool),_block_cyclic(LEGO_pool)
{
  // do nothing
}

LEGO_AFFINITY::~LEGO_AFFINITY()
{
  while (_block.Elements()) CXX_DELETE(_block.Pop(), LEGO_pool);
  while (_cyclic.Elements()) CXX_DELETE(_cyclic.Pop(), LEGO_pool);
  while (_block_cyclic.Elements()) CXX_DELETE(_block_cyclic.Pop(), LEGO_pool);
}
  
/****************************************************************************
 *
 * Add wn_array to the appropriate UGS stacks
 *
 ****************************************************************************/
void
LEGO_AFFINITY::Add_Ref(WN* wn_array,   
	       DISTRIBUTE_TYPE dtype, 
	       ACCESS_VECTOR* av,     
	       INT32 dim, 
	       INT32 loop_pos)
{
  switch (dtype) {
  case DISTRIBUTE_BLOCK:
    Add_To_Lego_UGS_Stack(_block, wn_array, dtype, av, dim, loop_pos);
    break;
  case DISTRIBUTE_CYCLIC_CONST:
    Add_To_Lego_UGS_Stack(_cyclic, wn_array, dtype, av, dim, loop_pos);
    break;
  case DISTRIBUTE_CYCLIC_EXPR:
    Add_To_Lego_UGS_Stack(_block_cyclic, wn_array, dtype, av, dim, loop_pos);
    break;
  default:
    FmtAssert(FALSE, ("LEGO_AFFINITY::Add_Ref: dimension is not reshaped"));
  }

  return;
}

/****************************************************************************
 *
 * Find the index of most occurring affinity and break ties by choosing
 * the smallest offset.
 *
 ****************************************************************************/
static INT
Vote_Affinity(LEGO_UGS_STACK & ugs_stack)
{
  if (ugs_stack.Elements()<=0) return -1;
  INT pos = 0;
  INT count = ugs_stack.Bottom_nth(0)->Get_Array_Refs().Elements();
  INT spread = ugs_stack.Bottom_nth(0)->Get_Max_Offset()-
    ugs_stack.Bottom_nth(0)->Get_Min_Offset();
  for (INT i = 1; i < ugs_stack.Elements(); ++i) {
    LEGO_UGS * ugs_cur = ugs_stack.Bottom_nth(i);
    INT new_count = ugs_cur->Get_Array_Refs().Elements();
    INT new_spread = ugs_cur->Get_Max_Offset()-ugs_cur->Get_Min_Offset();
    if (new_count>count || (new_count==count && new_spread<spread)) {
      pos = i;
      count = new_count;
      spread = new_spread;
    }
  }
  
  return pos;
}

/****************************************************************************
 *
 * (1) Pick a good affinity for the loop nest.
 * (2) Set LEGO_INFO according to the chosen affinity.
 *
 ****************************************************************************/
void
LEGO_AFFINITY::Pick_Affinity(LEGO_INFO *dli)
{
  // Two level voting:
  // Inside a distribution group, vote for the most occurring affinity.
  // Among the distribution groups, vote for the most occurring affinity.

  INT block_leader = Vote_Affinity(_block);
  INT cyclic_leader = Vote_Affinity(_cyclic);
  INT block_c_leader = Vote_Affinity(_block_cyclic);
  
  INT count = 0;
  INT spread = 0;
  LEGO_UGS * lego_ugs = NULL;
  if (block_leader>=0) {
    lego_ugs = _block.Bottom_nth(block_leader);
    count = lego_ugs->Get_Array_Refs().Elements();
    spread = lego_ugs->Get_Max_Offset()-lego_ugs->Get_Min_Offset();
  }
  if (cyclic_leader>=0) {
    LEGO_UGS * ugs_cur = _cyclic.Bottom_nth(cyclic_leader);
    INT new_count = ugs_cur->Get_Array_Refs().Elements();
    INT new_spread = ugs_cur->Get_Max_Offset()-ugs_cur->Get_Min_Offset();
    if (lego_ugs==NULL || new_count>count 
	|| (new_count==count && new_spread<spread)) {
      lego_ugs = ugs_cur;
      count = new_count;
      spread = new_spread;
    }
  }
  if (block_c_leader>=0) {
    LEGO_UGS * ugs_cur = _block_cyclic.Bottom_nth(block_c_leader);
    INT new_count = ugs_cur->Get_Array_Refs().Elements();
    INT new_spread = ugs_cur->Get_Max_Offset()-ugs_cur->Get_Min_Offset();
    if (lego_ugs==NULL || new_count>count 
	|| (new_count==count && new_spread<spread)) {
      lego_ugs = ugs_cur;
      count = new_count;
      spread = new_spread;
    }
  }
  
  // if lego_ugs is set, set the affinity for dli accordingly.
  if (lego_ugs) {
    WN * array_wn = lego_ugs->Get_Array_Refs().Bottom_nth(0);
    Is_True(array_wn,("Cannot find WN for LEGO_UGS"));

    SYMBOL *symb = CXX_NEW(SYMBOL(WN_st(WN_array_base(array_wn)),
                                  (WN_OFFSET) 0,
                                  0),
							LEGO_pool);
    INT32 offset = lego_ugs->Compute_Offset();
    INT32 front_peel = lego_ugs->Get_Min_Offset()-offset;
    INT32 back_peel = lego_ugs->Get_Max_Offset()-offset;
    INT32 stride = lego_ugs->Stride();
    INT32 dim = lego_ugs->Get_Ref_Dims().Bottom_nth(0);

    if (lego_ugs->Dis_Type()==DISTRIBUTE_BLOCK)
      dli->Init(symb, dim, stride, offset, -front_peel, back_peel);
    else {
      dli->Init(symb, dim, stride, offset, 0, 0);
      dli->Set_Min_Offset(front_peel);
      dli->Set_Max_Offset(back_peel);
    }
  }

}
  

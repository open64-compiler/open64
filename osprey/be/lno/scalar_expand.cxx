/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


// -*-C++-*-

/** $Revision: 1.8 $
*** $Date: 05/10/01 01:43:14-07:00 $
*** $Author: fchow@fluorspar.internal.keyresearch.com $
*** $Source: be/lno/SCCS/s.scalar_expand.cxx $
**/

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: be/lno/SCCS/s.scalar_expand.cxx $ $Revision: 1.8 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <ctype.h>
#include "pu_info.h"
#include "defs.h"
#include "stab.h"
#include "lnopt_main.h"
#include "scalar_expand.h"
#include "snl.h"

#include "const.h"
#include "targ_sim.h"
#include "config_targ.h"
#include "cxx_template.h"
#include "dep_graph.h"
#include "opt_du.h"
#include "cxx_memory.h"
#include "cxx_hash.h"
#include "lwn_util.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "lnoutils.h"
#include "wintrinsic.h"
#include "stab.h"
#include "strtab.h"
#include "debug.h"
#include "lno_bv.h"
#include "sxlist.h"
#include "parallel.h"
#include "soe.h"
#include "cond.h"
#include "snl_utils.h"

static WN* BND_Min_Expr(WN* wn, WN* loops[], INT nloops); 
static WN* BND_Max_Expr(WN* wn, WN* loops[], INT nloops);
ST* Find_Return_Registers(TYPE_ID type, PREG_NUM *rreg1, PREG_NUM *rreg2);

//-----------------------------------------------------------------------
// NAME: Invariant_Base 
// FUNCTION: Returns the invariant base expression for the given bound, 
//   'wn_base' assuming that the bound is truly invariant in the loop 
//   'wn_loop'.
// NOTES: In most cases, this will return 'wn_base' itself. In the case 
//     do i = 1, N 
//       ub = min(divfloor(exp))
//       do j = 1, ub  
//   it will return a pointer to "min(divfloor(exp))" rather than "ub". 
//-----------------------------------------------------------------------

static WN* Invariant_Base(WN* wn_base, 
			  WN* wn_loop) 
{
  DU_MANAGER* du = Du_Mgr; 
  DEF_LIST *def_list = du->Ud_Get_Def(wn_base); 
  DEF_LIST_ITER iter(def_list);
  const DU_NODE* node = NULL;
  INT inside_count = 0; 
  WN* inside_def = NULL; 
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* def = node->Wn();
    if (Wn_Is_Inside(def, wn_loop)) {
      inside_def = def; 
      if (++inside_count > 1) 
        FmtAssert(FALSE, ("wn_base is not really invariant wrt wn_loop")); 
    }
  }
  return inside_def != NULL ? WN_kid0(inside_def) : wn_base; 
}

//-----------------------------------------------------------------------
// NAME: SE_Index_Loops 
// FUNCTION: Set the values of 'index_loops', given that we are scalar 
//   expanding a reference with to an array with 'dimcnt' dimensions, 
//   with the set of 'dimcnt' original loops in 'loops', and 'nstrips' 
//   scalar expansion tile loops given in 'tile_loops' which have the 
//   given 'strip_sizes'. 
//-----------------------------------------------------------------------

static WN* SE_Index_Loops(WN* loops[], 
		          INT dimcnt, 
		          WN* tile_loops[],
		          INT strip_sizes[],
		          INT nstrips, 
			  WN* index_loops[])
{
  INT j = 0;
  for (INT i = 0; i < dimcnt; i++)
    index_loops[i] = nstrips > 0 && strip_sizes[i] > 0 
      ? tile_loops[j++] : loops[i];
  WN* outerloop = loops[0]; 
  if (nstrips > 0 && Get_Do_Loop_Info(tile_loops[0])->Depth 
      < Get_Do_Loop_Info(loops[0])->Depth)
    outerloop = tile_loops[0]; 
  return outerloop;
}

//-----------------------------------------------------------------------
// NAME: SE_Lower_Bound 
// FUNCTION: When scalar expanding, each dimension will have the form: 
//   "i - lb" where "lb" is the value returned by this function.  Here 
//   'index_loops' is the array of 'dimcnt' loops which index the scalar
//   expanded reference; 'outerloop' is the outermost loop in the nest of
//   loops over which we are scalar expanding, 'i' is such that 'index_
//   loops[i]' is the loop we are expanding, and 'invariant' is TRUE if 
//   we are scalar expanding over an invariant nest.  
//-----------------------------------------------------------------------

static WN* SE_Lower_Bound(WN* index_loops[], 
	                  WN* outerloop,
	                  INT i, 
	                  INT dimcnt,
			  BOOL invariant) 
{
  WN* lb = NULL; 
  DU_MANAGER* du = Du_Mgr; 
  if (invariant) { 
    WN* wn_iv = Invariant_Base(WN_kid0(WN_start(index_loops[i])), outerloop); 
    lb = LWN_Copy_Tree(wn_iv); 
    LWN_Copy_Def_Use(wn_iv, lb, du); 
  } else { 
    lb = BND_Min_Expr(WN_kid0(WN_start(index_loops[i])), index_loops, dimcnt);
  }
  return lb;
}

//-----------------------------------------------------------------------
// NAME: SE_Upper_Bound 
// FUNCTION: When scalar expanding, each dimension will have the form:
//   "i - lb" where the maximum value for "i" is "ub", the value returned
//   by this function.  Here 'index_loops' is the array of 'dimcnt' loops 
//   which index the scalar expanded reference; 'outerloop' is the outermost
//   loop in the nest of loops over which we are scalar expanding, 'i' is i
//   such that 'index_loops[i]' is the loop we are expanding, and 'invari-
//   ant' is TRUE if we are scalar expanding over an invariant nest.
//-----------------------------------------------------------------------

static WN* SE_Upper_Bound(WN* index_loops[], 
	                  WN* outerloop,
	                  INT i, 
	                  INT dimcnt,
			  BOOL invariant) 
{
  WN* ub = NULL; 
  DU_MANAGER* du = Du_Mgr; 
  if (invariant) { 
    WN* wn_iv = Invariant_Base(SNL_UBexp(WN_end(index_loops[i])), outerloop); 
    ub = LWN_Copy_Tree(wn_iv); 
    LWN_Copy_Def_Use(wn_iv, ub, du); 
  } else { 
    ub = BND_Max_Expr(SNL_UBexp(WN_end(index_loops[i])), index_loops, dimcnt);
  } 
  return ub;
}

//-----------------------------------------------------------------------
// NAME: SE_Indxs_and_Bounds 
// FUNCTION: Create some information needed to scalar expand a variable
//   with symbol 'symbol' inside the loop nest with orginal loops 'loops'.
//   The scalar expanded array has 'dimcnt' elements.  If 'invariant' is
//   TRUE, all of the loops we are expanding over are invariant with re-
//   to the nest in which they appear, otherwise we assume that they may 
//   be varying. If 'used_loops' id not NULL, we expand only over those 
//   loops for which 'used_loops[i]' is TRUE.  
//     If we are making compact scalar expansion tiles, 'tile_loops', 
//   'strip_sizes' and 'nstrips' are important, otherwise they have the 
//   values NULL, NULL, and 0.  In the former case, 'tile_loops' are the 
//   tile loops over which we are scalar expanding, and they have the 
//   sizes given in 'strip_sizes'.  There are a total of 'nstrips' tile
//   loops in this case. 
//     We compute 'bounds', 'indxs', 'sz', and 'bz', given the other 
//   parameters. The 'bounds[i]' contains the number of elements in that 
//   dimension of the scalar expanded array, while 'indxs[i]' contains 
//   the index expression for that dimension.  'sz' is the number of bytes
//   in each element, and 'bsz' is the total number of bytes for the scalar
//   expanded array.     
//-----------------------------------------------------------------------

static void SE_Indxs_and_Bounds(WN* loops[], 
				INT dimcnt, 
				SYMBOL symbol, 
				BOOL invariant, 
				BIT_VECTOR used_loops[],
				WN* tile_loops[], 
			        INT strip_sizes[], 
				INT nstrips, 
				WN* bounds[], 
			        WN* indxs[], 
				BOOL has_lcd, 
				INT* sz_addr,  
				WN** bsz_addr)
{
  DU_MANAGER* du = Du_Mgr; 
  WN** index_loops = CXX_NEW_ARRAY(WN*, dimcnt, &LNO_local_pool); 
  WN* outerloop = SE_Index_Loops(loops, dimcnt, tile_loops, strip_sizes, 
    nstrips, index_loops); 

  TYPE_ID wtype = symbol.Type;
  INT sz = (wtype == MTYPE_I1 || wtype == MTYPE_U1) ? 1 :
	   (wtype == MTYPE_I2 || wtype == MTYPE_U2) ? 2 :  
	   (wtype == MTYPE_I8 || wtype == MTYPE_U8 ||
	    wtype == MTYPE_F8 || wtype == MTYPE_C4) ? 8 :
#if defined(TARG_IA64) || defined(TARG_X8664)
	   (wtype == MTYPE_F10) ? 16 :
	   (wtype == MTYPE_C10) ? 32 :
#endif
	   (wtype == MTYPE_I4 || wtype == MTYPE_U4 ||
	    wtype == MTYPE_F4) ? 4 :
	   (wtype == MTYPE_F8) ? 8 :
           (wtype == MTYPE_F16) ? 16 :
	   (wtype == MTYPE_FQ || wtype == MTYPE_C8) ? 16 :
	   (wtype == MTYPE_CQ || wtype == MTYPE_C16 ) ? 32 : 0;
  FmtAssert(sz > 0, ("Bad type in scalar expansion"));

  TYPE_ID bsztype;
  INT i = 0;
  while (used_loops && !used_loops->Test(i)) i++;
  bsztype = Do_Wtype(loops[i]);
  for (i = i+1; i < dimcnt; i++) {
    if (!used_loops || used_loops->Test(i)) {
      bsztype = Max_Wtype(bsztype, Do_Wtype(loops[i]));
    }
  }

  WN* bsz = LWN_Make_Icon(Promote_Type(bsztype), sz);
  OPCODE opmpy = OPCODE_make_op(OPR_MPY, Promote_Type(bsztype), MTYPE_V);
  for (i = 0; i < dimcnt; i++) {
    if (!used_loops || used_loops->Test(i)) {
      INT stripsize = nstrips == 0 ? 0 : strip_sizes[i]; 
      WN* cnt = stripsize > 0 ? LWN_Make_Icon(Promote_Type(bsztype),
                                              stripsize) : NULL; 
      TYPE_ID dty = WN_desc(WN_start(index_loops[i]));
      TYPE_ID rty = Promote_Type(WN_desc(WN_start(index_loops[i])));
      OPCODE opsubty = OPCODE_make_op(OPR_SUB, rty, MTYPE_V);
      OPCODE opminty = OPCODE_make_op(OPR_MIN, rty, MTYPE_V);
      WN* lb = SE_Lower_Bound(index_loops, outerloop, i, dimcnt, invariant); 
      WN* copy_lb = NULL;  
      if (stripsize == 0) { 
        copy_lb = LWN_Copy_Tree(lb); 
        LWN_Copy_Def_Use(lb, copy_lb, du);
      }
      WN* ub = SE_Upper_Bound(index_loops, outerloop, i, dimcnt, invariant); 
      WN* one = LWN_Make_Icon(rty, 1);
      WN* rcnt = LWN_CreateExp2(opsubty, ub, LWN_CreateExp2(opsubty, lb, one));
      WN* true_cnt = cnt != NULL ? LWN_CreateExp2(opminty, cnt, rcnt) : rcnt;  
      if (has_lcd) {
        WN* wn_one = LWN_Make_Icon(rty, 1);
        WN* wn_zero = LWN_Make_Icon(rty, 0);
	OPCODE opaddty = OPCODE_make_op(OPR_ADD, rty, MTYPE_V);
	OPCODE opmaxty = OPCODE_make_op(OPR_MAX, rty, MTYPE_V);
	true_cnt = LWN_CreateExp2(opmaxty, true_cnt, wn_zero);
	true_cnt = LWN_CreateExp2(opaddty, true_cnt, wn_one); 
      } 
      bounds[i] = LWN_Copy_Tree(true_cnt);
      LWN_Copy_Def_Use(true_cnt, bounds[i], du); 
      OPCODE opldty = OPCODE_make_op(OPR_LDID, rty, dty); 
      WN* indx_ldid = LWN_CreateLdid(opldty, WN_step(loops[i])); 
      WN* indx_ldid_lb = stripsize > 0 ? 
	  LWN_CreateLdid(opldty, WN_step(index_loops[i])) : copy_lb;  
      indxs[i] = LWN_CreateExp2(opsubty, indx_ldid, indx_ldid_lb); 
      SNL_Add_Du_To_Index_Ldid(loops[i], indx_ldid, du, TRUE); 
      if (stripsize > 0) 
        SNL_Add_Du_To_Index_Ldid(index_loops[i], indx_ldid_lb, du, TRUE); 
      bsz = LWN_CreateExp2(opmpy, bsz, 
        LWN_Int_Type_Conversion(true_cnt, Promote_Type(bsztype)));
    }
  }
  bsz = LWN_Int_Type_Conversion(bsz, Pointer_type);
  *sz_addr = sz;
  *bsz_addr = bsz;
}

//-----------------------------------------------------------------------
// NAME: SE_Findxs 
// FUNCTION: Set each entry in 'findxs' to either appropriate index of the
//   final reference to the scalar expanded variable.  The formula for 
//   'findxs[i]' will depend on whether the index is over a scalar expan-
//   sion tiled loop or not.  If so, it is 'ub-ii+sz' where 'ub' is the 
//   upper bound for that dimension, 'ii' is the final value of the scalar
//   expansion tile loop, and 'sz' is the strip size of that loop.  If 
//   not, the expression is 'ub-lb' where 'ub' is the upper bound and 
//  'lb' is the lower bound for that dimension of the scalar expanded 
//  variable.  Here 'loops' is the set of 'dimcnt' original loops, and. 
//  'invariant' is TRUE if we are scalar expanding over an invariant nest.
//  'nstrips' is the number of loops we are scalar expansion tiling, and
//  if this is greater than 0, 'tile_loops' gives those loops and   
//  'strip_sizes' gives their strip sizes.  If 'used_loops' is not NULL, 
//  we are expanding only a limited number of loops, those selected are 
//  the ones for which 'used_loops[i]' is TRUE. 
//-----------------------------------------------------------------------

static void SE_Findxs(WN* loops[], 
		      INT dimcnt, 
		      BOOL invariant, 
		      BIT_VECTOR used_loops[],
		      WN* tile_loops[], 
		      INT strip_sizes[], 
		      INT nstrips, 
		      WN* findxs[]) 
{
  DU_MANAGER* du = Du_Mgr; 
  WN** index_loops = CXX_NEW_ARRAY(WN*, dimcnt, &LNO_local_pool); 
  WN* outerloop = SE_Index_Loops(loops, dimcnt, tile_loops, strip_sizes, 
    nstrips, index_loops); 
  for (INT i = 0; i < dimcnt; i++) {
    if (!used_loops || used_loops->Test(i)) {
      TYPE_ID dty = WN_desc(WN_start(index_loops[i]));
      TYPE_ID rty = Promote_Type(WN_desc(WN_start(index_loops[i])));
      OPCODE opsubty = OPCODE_make_op(OPR_SUB, rty, MTYPE_V);
      INT stripsize = nstrips == 0 ? 0 : strip_sizes[i]; 
      WN* ub = SE_Upper_Bound(index_loops, outerloop, i, dimcnt, 
        invariant); 
      OPCODE opldty = OPCODE_make_op(OPR_LDID, rty, dty); 
      WN* indx_ldid_lb = NULL; 
      WN* wn_off1 = NULL; 
      if (stripsize > 0) { 
	wn_off1 = LWN_CreateLdid(opldty, WN_step(index_loops[i]));
	WN* wn_off2 = LWN_Make_Icon(Promote_Type(Do_Wtype(index_loops[i])),
	  stripsize); 
        indx_ldid_lb = LWN_CreateExp2(opsubty, wn_off1, wn_off2);
      } else {
	WN* lb = SE_Lower_Bound(index_loops, outerloop, i, dimcnt, 
	  invariant); 
        indx_ldid_lb = lb;
      }
      findxs[i] = LWN_CreateExp2(opsubty, ub, indx_ldid_lb); 
      if (stripsize > 0) {
        SNL_Add_Du_To_Index_Ldid(index_loops[i], indx_ldid_lb, du, TRUE); 
        DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(wn_off1); 
        WN* wn_loop = Enclosing_Do_Loop(LWN_Get_Parent(outerloop));
	def_list->Set_loop_stmt(wn_loop);
      }
    }
  }
}

//-----------------------------------------------------------------------
// NAME: SE_Array 
// FUNCTION: Create an OPR_ARRAY node for the scalar expanded variable 
//   whose new symbol is 'se_ptrsym', which has 'dimcnt' dimensions, 
//   occupies 'sz' bytes of storage, whose dimensions are placed in the
//   'order' specified by this permutation vector, and whose 'bounds' 
//   and 'indxs' were computed by the 'SE_Bounds_and_Indxs'. 'used_loops'
//   is a bit vector which selects only certain loops to be expanded. 
//-----------------------------------------------------------------------

static WN* SE_Array(SYMBOL se_ptrsym,
		    INT sz, 
		    INT dimcnt, 
		    const INT order[], 
		    BIT_VECTOR* used_loops,
		    WN* bounds[],
		    WN* indxs[], 
		    BOOL has_lcd, 
		    INT lcd_count)
{
  DU_MANAGER* du = Du_Mgr; 
  INT used_dimcnt = used_loops ? used_loops->Pop_Count() : dimcnt; 
  OPCODE ldop = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);
  OPCODE op_array = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
  WN* wn_array = WN_Create(op_array, used_dimcnt+used_dimcnt+1);
  WN_element_size(wn_array) = sz;
  WN_array_base(wn_array) = WN_CreateLdid(ldop, se_ptrsym.WN_Offset(),
    se_ptrsym.St(), ST_type(se_ptrsym.St()));
  LWN_Set_Parent(WN_array_base(wn_array), wn_array);
  FmtAssert(used_loops == NULL || !has_lcd, 
    ("SE_Array: Not supporting used_loops and has_lcd at same time"));    
  INT k = 0;
  for (INT i = 0; i < dimcnt; i++) {
    if (!used_loops || used_loops->Test(i)) {
      INT j = order[i];
      WN* wn_index = NULL; 
      if (lcd_count >= 0 && j > lcd_count) { 
	wn_index = LWN_Copy_Tree(bounds[j]);
	LWN_Copy_Def_Use(bounds[j], wn_index, du);
	TYPE_ID rty = Promote_Type(WN_rtype(bounds[j]));
	OPCODE subop = OPCODE_make_op(OPR_SUB, rty, MTYPE_V);
	WN* wn_one = LWN_Make_Icon(rty, 1);
	wn_index = LWN_CreateExp2(subop, wn_index, wn_one);  
      } else { 
	wn_index = LWN_Copy_Tree(indxs[j]);
	LWN_Copy_Def_Use(indxs[j], wn_index, du);
	if (has_lcd) { 
	  TYPE_ID rty = WN_rtype(indxs[j]);
	  OPCODE addop = OPCODE_make_op(OPR_ADD, rty, MTYPE_V);
	  WN* wn_one = LWN_Make_Icon(rty, 1);
	  wn_index = LWN_CreateExp2(addop, wn_index, wn_one);  
	} 
	if (lcd_count >= 0 && j == lcd_count) { 
	  TYPE_ID rty = WN_rtype(indxs[j]);
	  OPCODE subop = OPCODE_make_op(OPR_SUB, rty, MTYPE_V);
	  WN* wn_one = LWN_Make_Icon(rty, 1);
	  wn_index = LWN_CreateExp2(subop, wn_index, wn_one);  
	} 
      } 
      WN_array_index(wn_array, k) = wn_index; 
      WN_array_dim(wn_array, k) = LWN_Copy_Tree(bounds[j]);
      LWN_Copy_Def_Use(bounds[j], WN_array_dim(wn_array, k), du);
      LWN_Set_Parent(WN_array_index(wn_array, k), wn_array);
      LWN_Set_Parent(WN_array_dim(wn_array,k), wn_array);
      k++;
    }
  }
  return wn_array; 
}

//-----------------------------------------------------------------------
// NAME: SE_Iload 
// FUNCTION: Create an ILOAD for symbol 'se_symptr' to replace the node 
//  'wn' via scalar expansion.  'wn_array' is the ARRAY node created by 
//  'SE_Array'. 'wn_newstdf' is the node where the space of the scalar 
//  expanded array is allocated. 'alias_host' is a temp which holds alias
//  information so that all of the scalar expanded nodes have common alias 
//  information.    
//-----------------------------------------------------------------------

static void SE_Iload(WN* wn,
	             WN* wn_array, 		
	             WN* wn_newstdf,
		     SYMBOL se_ptrsym,
		     WN** alias_host)
{
  DU_MANAGER* du = Du_Mgr;
  SYMBOL symbol(wn); 
  TYPE_ID wtype = symbol.Type;
#ifdef KEY // bug 7846
  OPCODE loadop = OPCODE_make_op(OPR_ILOAD, WN_rtype(wn), wtype);
#else
  OPCODE loadop = OPCODE_make_op(OPR_ILOAD, Promote_Type(wtype), wtype);
#endif
  TY_IDX wty = Be_Type_Tbl(wtype);
  TY_IDX pty = Make_Pointer_Type(Be_Type_Tbl(wtype));
  WN* load = LWN_CreateIload(loadop, 0, wty, pty, wn_array);
  WN* parent = LWN_Get_Parent(wn);
  LWN_Copy_Frequency_Tree(load,wn);
  LWN_Set_Parent(load,parent);

  // Get the alias id right
  if (*alias_host == NULL) {
    Create_unique_pointer_alias(Alias_Mgr, se_ptrsym.St(), 
      WN_array_base(wn_array), load);
    *alias_host = load;
    Copy_alias_info(Alias_Mgr, WN_array_base(wn_array), wn_newstdf);
  } else {
    Copy_alias_info(Alias_Mgr, wn_newstdf, WN_array_base(wn_array));
    Copy_alias_info(Alias_Mgr, *alias_host, load);
  }

  if (red_manager) {
    REDUCTION_TYPE rtype = red_manager->Which_Reduction(wn);
    if (rtype != RED_NONE) {
      red_manager->Add_Reduction(load, rtype);
      red_manager->Erase(wn);
    }
  }
  du->Add_Def_Use(wn_newstdf, WN_array_base(wn_array));
  for (INT k = 0; k < WN_kid_count(parent); k++)
    if (WN_kid(parent,k) == wn)
      WN_kid(parent,k) = load;
  LWN_Delete_Tree(wn);
}

//-----------------------------------------------------------------------
// NAME: SE_Istore 
// FUNCTION: Create an ISTORE for symbol 'se_symptr' to replace the node 
//  'wn' via scalar expansion.  'wn_array' is the ARRAY node created by 
//  'SE_Array'. 'wn_newstdf' is the node where the space of the scalar 
//  expanded array is allocated. 'alias_host' is a temp which holds alias
//  information so that all of the scalar expanded nodes have common alias 
//  information.    
//-----------------------------------------------------------------------

static void SE_Istore(WN* wn,
                      WN* wn_array,
                      WN* wn_newstdf,
                      SYMBOL se_ptrsym,
                      WN** alias_host)
{
  SYMBOL symbol(wn); 
  DU_MANAGER* du = Du_Mgr; 
  TYPE_ID wtype = symbol.Type;
  OPCODE storeop = OPCODE_make_op(OPR_ISTORE, MTYPE_V, wtype);
  TY_IDX pty = Make_Pointer_Type(Be_Type_Tbl(wtype));
  FmtAssert(pty, ("null ty pointer for wtype=%d", wtype));
  WN* store = LWN_CreateIstore(storeop, 0, pty, WN_kid0(wn), wn_array);

  // Get the alias id right
  if (*alias_host==NULL) {
    Create_unique_pointer_alias(Alias_Mgr, se_ptrsym.St(), 
      WN_array_base(wn_array), store);
    *alias_host = store;
    Copy_alias_info(Alias_Mgr, WN_array_base(wn_array), wn_newstdf);
  } else {
    Copy_alias_info(Alias_Mgr, wn_newstdf, WN_array_base(wn_array));
    Copy_alias_info(Alias_Mgr, *alias_host, store);
  }
  du->Add_Def_Use(wn_newstdf, WN_array_base(wn_array));

  WN_kid0(wn) = NULL;
  WN_set_kid_count(wn, 0);
  WN* block = LWN_Get_Parent(wn);
  LWN_Copy_Frequency_Tree(store, wn);
  LWN_Insert_Block_Before(block, wn, store);
  Is_True(LWN_Get_Parent(wn) == block, ("Very confused?"));
  LWN_Set_Parent(store,block);
  LWN_Copy_Linenumber(wn,store);
  LWN_Copy_Frequency_Tree(store,wn);
  LWN_Extract_From_Block(wn);
  if (red_manager) {
    REDUCTION_TYPE rtype = red_manager->Which_Reduction(wn);
    if (rtype != RED_NONE) 
      red_manager->Add_Reduction(store, rtype);
    red_manager->Erase_Node(wn);
  }
  WN_Delete(wn);
}

static WN* SE_Identity(WN* wn)
{ 
  SYMBOL symbol(wn); 
  OPCODE opldid = OPCODE_make_op(OPR_LDID, Promote_Type(symbol.Type), 
    symbol.Type); 
  OPCODE opstid = OPCODE_make_op(OPR_STID, MTYPE_V, symbol.Type); 
  WN* wn_ldid = LWN_CreateLdid(opldid, wn); 
  WN* wn_stid = LWN_CreateStid(opstid, wn, wn_ldid);  
  return wn_stid; 
} 

//-----------------------------------------------------------------------
// NAME: SE_Final_Value
// FUNCTION: Create a statement "s = s" where "s" is cloned from 'wn' and
//   insert it into its appropriate place in the 'guard_tests' for the 
//   region which is being scalar expanded.  The original loops are given 
//   in 'loops', there are 'dimcnt' of these.  Return the LDID of the
//   node in this statement; it will be scalar expanded.  The statement
//   will then be the finalization statement for this variable. 
//-----------------------------------------------------------------------

static WN* SE_Final_Value(WN* wn,
		          WN* guard_tests[],
			  WN* loops[], 
			  INT dimcnt) 
{
  DU_MANAGER* du = Du_Mgr; 
  WN* wn_stid = SE_Identity(wn); 
  WN* wn_ldid = WN_kid0(wn_stid); 
  WN* wn_loop = Enclosing_Do_Loop(wn); 
  INT i;
  for (i = 0; i < dimcnt; i++) 
    if (loops[i] == wn_loop)
      break; 
  FmtAssert(i < dimcnt, ("SE_Final_Value: Could not find SE loop")); 
  WN* wn_if = guard_tests[i]; 
  LWN_Insert_Block_After(WN_then(wn_if), NULL, wn_stid); 
  USE_LIST *use_list = du->Du_Get_Use(wn);
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next())
    du->Add_Def_Use(wn_stid, node->Wn());
  return wn_ldid; 
}

//-----------------------------------------------------------------------
// NAME: SE_Find_Def
// FUNCTION: Return the (first) definition of 'symbol' (which must be an 
//   STID) in 'wn_region'.  Return NULL if there is none. 
//-----------------------------------------------------------------------

static WN* SE_Find_Def(WN* wn_region, 
		       SYMBOL symbol)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_region); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    if (WN_operator(wn) == OPR_STID 
        && SYMBOL(wn) == symbol)
      return wn;
  }
  return NULL; 
}

//-----------------------------------------------------------------------
// NAME: SE_Safe_Depth
// FUNCTION: For the access array 'aa' which represents a bound of an SNL
//   loop of the given 'depth', return the depth of the outermost loop for
//   which none of the access_vectors are too messy nor contain non-constant
//   loops.
//-----------------------------------------------------------------------

static INT SE_Safe_Depth(ACCESS_ARRAY* aa,
                         INT depth)
{
  INT safe_depth = 0;
  for (INT dim = 0; dim < aa->Num_Vec(); dim++) {
    ACCESS_VECTOR* av = aa->Dim(dim);
    if (av->Too_Messy)
      return depth + 1;
    if (av->Non_Const_Loops() > safe_depth)
      safe_depth = av->Non_Const_Loops();
  }
  return safe_depth;
}

//-----------------------------------------------------------------------
// NAME: SNL_Is_Scalar_Expandable 
// FUNCTION: Returns an SE_STATUS value describing if the SNL 'wn_outer' to
//   which we are applying a 'permutation' of length 'nloops' can be scalar
//   expanded to enable that permutation.  Its scalar expandable variables 
//   are summarized in the 'sx_info'. 
//-----------------------------------------------------------------------

extern SE_STATUS SNL_Is_Scalar_Expandable(WN* wn_outer, 
				          INT permutation[],
                                          INT nloops, 
				          SX_INFO* sx_info,
					  BOOL full_dist)
{
  // Find the outermost loop to be permuted. 
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT kernel_depth = outer_depth; 
  INT* kernel_permutation = NULL; 
  INT kernel_nloops = 0; 
  if (permutation != NULL) {
    INT i;
    for (i = 0; i < nloops; i++)
      if (permutation[i] != i)
	break;
    INT kernel_first = i; 
    kernel_nloops = nloops - kernel_first; 
    kernel_permutation = CXX_NEW_ARRAY(INT, kernel_nloops, &LNO_local_pool); 
    for (i = kernel_first; i < nloops; i++) 
      kernel_permutation[i - kernel_first] = permutation[i]; 
    kernel_depth = outer_depth + kernel_first; 
  } 

  // Find the safe_depth. 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack); 
  INT safe_depth = Do_Loop_Depth(wn_outer);
  for (WN* wn = wn_inner; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) != OPC_DO_LOOP)
      continue;
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
    INT safe_depth_lb = SE_Safe_Depth(dli->LB, dli->Depth);
    if (safe_depth_lb > safe_depth)
      safe_depth = safe_depth_lb;
    INT safe_depth_ub = SE_Safe_Depth(dli->UB, dli->Depth);
    if (safe_depth_ub > safe_depth)
      safe_depth = safe_depth_ub;
    if (wn == wn_outer)
      break;
  }

  // If need to transform outside outermost kernel or safe depth, give up. 
  SE_STATUS return_value = SE_TRUE; 
  SX_CONST_PITER ii(&sx_info->Plist);
  INT* tkernel_permutation = !full_dist ? kernel_permutation : NULL; 
  for (const SX_PNODE* n = ii.First(); !ii.Is_Empty(); n = ii.Next()) {
    if (n->Transformable(kernel_depth, tkernel_permutation, kernel_nloops) 
        == SX_PNODE::ILLEGAL)
      return_value = SE_MAYBE; 
    if (n->Transformable(kernel_depth, tkernel_permutation, kernel_nloops) 
        == SX_PNODE::SE_REQD && safe_depth > kernel_depth)
      return SE_FALSE; 
  }

  return return_value; 
}

//-----------------------------------------------------------------------
// NAME: SNL_Bad_Scalars_Are_Distributable 
// FUNCTION: For the SNL with outermost loop 'wn_outer' consisting of 
//   'nloops' loops, to which we are applying the 'permutation' of length
//   'nloops', return the depth at which we must distribute to remove 
//   bad scalars.  Return -1 if distribution is not necessary, return 
//   Do_Loop_Depth(wn_outer) + 'nloops' if it is not possible to distri-
//   bute and remove the bad scalars.   
//-----------------------------------------------------------------------

extern INT SNL_Bad_Scalars_Are_Distributable(WN* wn_outer, 
					     INT permutation[], 
					     INT nloops, 
					     SX_INFO* sx_info, 
					     SD_INFO* sd_info)
{
  // Find the outermost loop to be permuted. 
  INT i;
  for (i = 0; i < nloops; i++)
    if (permutation[i] != i)
      break;
  INT kernel_first = i; 
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT inner_depth = outer_depth + nloops - 1; 
  INT kernel_depth = outer_depth + kernel_first; 

  // Test for distributability of bad scalars. 
  INT upper_range = sd_info->Distribution_Range(kernel_first, sx_info);
  if (upper_range == -1) 
    return -1;  
  if (upper_range >= outer_depth + nloops - 1)
    return outer_depth + nloops; 
 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  WN* wn_bad_inner = stack.Bottom_nth(upper_range + 1); 
  WN* wn_bad_outer = wn_outer; 
  if (!SNL_Is_Distributable(wn_bad_outer, wn_bad_outer, wn_bad_inner, TRUE))
    return outer_depth + nloops; 
  if (!SNL_Is_Distributable(wn_bad_outer, wn_bad_outer, wn_bad_inner, FALSE))
    return outer_depth + nloops; 
  return upper_range + 1; 
}

//-----------------------------------------------------------------------
// NAME: Is_Index_Variable 
// FUNCTION: Returns TRUE if the symbol in 'wn' is an index variable of
//   the one of the loops in array 'loops' of length 'nloops'. 
//-----------------------------------------------------------------------

static BOOL Is_Index_Variable(WN* wn, 
			      WN* loops[], 
			      INT nloops) 
{ 
  INT i;
  for (i = 0; i < nloops; i++)  
    if (SYMBOL(wn) == SYMBOL(WN_index(loops[i])))
      break; 
  return i < nloops ? TRUE : FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: BND_Lower_Bound 
// FUNCTION: Returns a lower bound for 'wn' which must be an index 
//   variable of some loop in the array 'loops[]' of length 'nloops'. 
//-----------------------------------------------------------------------

static WN* BND_Lower_Bound(WN* wn, 
		           WN* loops[], 
			   INT nloops) 
{ 
  INT i;
  for (i = 0; i < nloops; i++) 
    if (SYMBOL(wn) == SYMBOL(WN_index(loops[i])))
      break;
  WN* loop = loops[i];  
  WN* lb = WN_kid0(WN_start(loop));
  WN* wn_new = BND_Min_Expr(lb, loops, nloops); 
  return wn_new;  
} 

//-----------------------------------------------------------------------
// NAME: BND_Upper_Bound 
// FUNCTION: Returns an upper bound for 'wn' which must be an index 
//   variable of some loop in the array 'loops[]' of length 'nloops'. 
//-----------------------------------------------------------------------

static WN* BND_Upper_Bound(WN* wn, 
		           WN* loops[], 
			   INT nloops)
{
  INT i;
  for (i = 0; i < nloops; i++) 
    if (SYMBOL(wn) == SYMBOL(WN_index(loops[i])))
      break;
  WN* loop = loops[i];  
  WN* ub = SNL_UBexp(WN_end(loop));
  WN* wn_new = BND_Max_Expr(ub, loops, nloops); 
  return wn_new;  
} 

//-----------------------------------------------------------------------
// NAME: BND_Verify_Expression 
// FUNCTION: Returns TRUE is the expression 'wn' is of the form handled 
//   by the scalar expansion code below.  The array 'loops[]' of length 
//   'nloops' contains all loops whose index variables may be used in 'wn'. 
//-----------------------------------------------------------------------

static BOOL BND_Verify_Expression(WN *wn,
				  WN* loops[],  
				  INT nloops) 
{
  OPCODE wn_op = WN_opcode(wn); 
  OPERATOR wn_oper = OPCODE_operator(wn_op);
  switch (wn_oper) {
  case OPR_INTCONST: 
    return TRUE; 
  case OPR_LDID: 
    return TRUE; 
  case OPR_STID: 
    {
      for (INT i = 0; i < nloops; i++) 
        if (SYMBOL(wn) == SYMBOL(WN_index(loops[i])))
          return TRUE; 
    }
    return FALSE; 
  case OPR_ADD:
  case OPR_MPY: 
  case OPR_MIN: 
  case OPR_MAX: 
  case OPR_NEG: 
  case OPR_SUB:
  case OPR_DIV: 
    return TRUE; 
  case OPR_INTRINSIC_OP: 
    {
      INT32 intr = WN_intrinsic(wn);
      switch (intr) { 
      case INTRN_I4DIVFLOOR:
      case INTRN_U4DIVFLOOR:
      case INTRN_I8DIVFLOOR:
      case INTRN_U8DIVFLOOR:
      case INTRN_I4DIVCEIL:
      case INTRN_U4DIVCEIL:
      case INTRN_I8DIVCEIL:
      case INTRN_U8DIVCEIL:
        return TRUE; 
      default: 
        return FALSE;  
      } 
    }
  default: 
    return FALSE;  
  } 
} 

//-----------------------------------------------------------------------
// NAME: BND_Verify_Nest_Bounds 
// FUNCTION: Returns TRUE if all of the start, stop, and step expressions 
//   in the array of 'loops[]' of length 'nloops' are of the form handled
//   by the following scalar expansion code.  
//-----------------------------------------------------------------------

static BOOL BND_Verify_Nest_Bounds(WN* loops[], 
				   INT nloops) 
{ 
  for (INT i = 0; i < nloops; i++) { 
    if (!BND_Verify_Expression(WN_start(loops[i]), loops, nloops))
      return FALSE;  
    if (!BND_Verify_Expression(WN_end(loops[i]), loops, nloops))
      return FALSE;  
    if (!BND_Verify_Expression(WN_step(loops[i]), loops, nloops))
      return FALSE;  
  }
  return TRUE;  
} 

//-----------------------------------------------------------------------
// NAME: BND_Min_Expr
// FUNCTION: Returns an expression which is an lower bound of 'wn' and
//   is invariant in 'loop[0]'.  The array 'nloops[]' of length 'nloops' 
//   is a list of all loops whose index variables may be used in 'wn'. 
//-----------------------------------------------------------------------

static WN* BND_Min_Expr(WN* wn,
		       WN* loops[],  
                       INT nloops)
{ 
  OPCODE wn_op = WN_opcode(wn);  
  OPERATOR wn_oper = OPCODE_operator(wn_op); 
  switch (wn_oper) { 
  case OPR_PAREN:
    return BND_Min_Expr(WN_kid0(wn), loops, nloops);
  case OPR_INTCONST: 
    return LWN_Copy_Tree(wn, TRUE, LNO_Info_Map); 
  case OPR_LDID: 
    {
      WN* wn_new = NULL; 
      if (Is_Index_Variable(wn, loops, nloops)) { 
        wn_new = BND_Lower_Bound(wn, loops, nloops);  
      } else { 
        wn_new = LWN_Copy_Tree(wn, TRUE, LNO_Info_Map);
        LWN_Copy_Def_Use(wn, wn_new, Du_Mgr);
      } 
      return wn_new; 
    } 
  case OPR_ADD:
  case OPR_MIN: 
  case OPR_MAX: 
    {
      TYPE_ID ty_com_rtype = OPCODE_rtype(wn_op); 
      TYPE_ID ty_com_desc = OPCODE_desc(wn_op); 
      OPCODE com_opc = OPCODE_make_op(wn_oper, ty_com_rtype, ty_com_desc);  
      WN* wn_kid0 = BND_Min_Expr(WN_kid0(wn), loops, nloops);
      WN* wn_kid1 = BND_Min_Expr(WN_kid1(wn), loops, nloops);
      WN *wn_new = LWN_CreateExp2(com_opc, wn_kid0, wn_kid1); 
      return wn_new;
    }
  case OPR_MPY: 
    {
      TYPE_ID ty_mpy_rtype = OPCODE_rtype(wn_op); 
      TYPE_ID ty_mpy_desc = OPCODE_desc(wn_op); 
      OPCODE mpy_opc = OPCODE_make_op(wn_oper, ty_mpy_rtype, ty_mpy_desc);  
      OPCODE cmp_opc = OPCODE_make_op(OPR_MIN, ty_mpy_rtype, ty_mpy_desc);  
      WN* wn_mpy_kid0 = BND_Max_Expr(WN_kid0(wn), loops, nloops);
      WN* wn_mpy_kid1 = BND_Max_Expr(WN_kid1(wn), loops, nloops);
      WN* wn_new_exp0 = LWN_CreateExp2(mpy_opc, wn_mpy_kid0, wn_mpy_kid1); 
      wn_mpy_kid0 = BND_Min_Expr(WN_kid0(wn), loops, nloops);
      wn_mpy_kid1 = BND_Min_Expr(WN_kid1(wn), loops, nloops);
      WN* wn_new_exp1 = LWN_CreateExp2(mpy_opc, wn_mpy_kid0, wn_mpy_kid1); 
      WN* wn_new_one = LWN_CreateExp2(cmp_opc, wn_new_exp1, wn_new_exp0);
      wn_mpy_kid0 = BND_Max_Expr(WN_kid0(wn), loops, nloops);
      wn_mpy_kid1 = BND_Min_Expr(WN_kid1(wn), loops, nloops);
      wn_new_exp0 = LWN_CreateExp2(mpy_opc, wn_mpy_kid0, wn_mpy_kid1); 
      wn_mpy_kid0 = BND_Min_Expr(WN_kid0(wn), loops, nloops);
      wn_mpy_kid1 = BND_Max_Expr(WN_kid1(wn), loops, nloops);
      wn_new_exp1 = LWN_CreateExp2(mpy_opc, wn_mpy_kid0, wn_mpy_kid1); 
      WN* wn_new_two = LWN_CreateExp2(cmp_opc, wn_new_exp1, wn_new_exp0);
      WN *wn_new = LWN_CreateExp2(cmp_opc, wn_new_one, wn_new_two);
      return wn_new;
    }
  case OPR_NEG: 
    {
      TYPE_ID ty_neg_rtype = OPCODE_rtype(wn_op); 
      TYPE_ID ty_neg_desc = OPCODE_desc(wn_op); 
      OPCODE neg_opc = OPCODE_make_op(wn_oper, ty_neg_rtype, ty_neg_desc); 
      WN* wn_kid = BND_Max_Expr(WN_kid0(wn), loops, nloops); 
      WN *wn_new = WN_CreateExp1(neg_opc, wn_kid);
      return wn_new;  
    }
  case OPR_SUB:
  case OPR_DIV: 
    {
      TYPE_ID ty_sub_rtype = OPCODE_rtype(wn_op); 
      TYPE_ID ty_sub_desc = OPCODE_desc(wn_op); 
      OPCODE sub_opc = OPCODE_make_op(wn_oper, ty_sub_rtype, ty_sub_desc);  
      WN *wn_kid0 = BND_Min_Expr(WN_kid0(wn), loops, nloops);
      WN *wn_kid1 = BND_Max_Expr(WN_kid1(wn), loops, nloops);
      WN *wn_new = LWN_CreateExp2(sub_opc, wn_kid0, wn_kid1);
      return wn_new;
    }
  case OPR_INTRINSIC_OP: 
    {
      INT32 intr = WN_intrinsic(wn); 
      switch (intr) { 
      case INTRN_I4DIVFLOOR: 
      case INTRN_U4DIVFLOOR: 
      case INTRN_I8DIVFLOOR: 
      case INTRN_U8DIVFLOOR: 
        {
          TYPE_ID ty_div_rtype = OPCODE_rtype(wn_op);
          TYPE_ID ty_div_desc = OPCODE_desc(wn_op);
          OPCODE div_opc = OPCODE_make_op(OPR_DIV, ty_div_rtype, ty_div_desc); 
          OPCODE sub_opc = OPCODE_make_op(OPR_SUB, ty_div_rtype, ty_div_desc); 
          WN *wn_kid0 = BND_Min_Expr(WN_kid0(WN_kid0(wn)), loops, nloops); 
          WN *wn_kid1 = BND_Max_Expr(WN_kid0(WN_kid1(wn)), loops, nloops); 
          WN* wn_div = LWN_CreateExp2(div_opc, wn_kid0, wn_kid1); 
          WN* one = LWN_Make_Icon(ty_div_rtype, 1);  
          WN *wn_new = LWN_CreateExp2(sub_opc, wn_div, one); 
          return wn_new; 
        }
      case INTRN_I4DIVCEIL:
      case INTRN_U4DIVCEIL: 
      case INTRN_I8DIVCEIL: 
      case INTRN_U8DIVCEIL: 
        {
          TYPE_ID ty_div_rtype = OPCODE_rtype(wn_op);
          TYPE_ID ty_div_desc = OPCODE_desc(wn_op);
          OPCODE div_opc = OPCODE_make_op(OPR_DIV, ty_div_rtype, ty_div_desc); 
          WN *wn_kid0 = BND_Min_Expr(WN_kid0(WN_kid0(wn)), loops, nloops); 
          WN *wn_kid1 = BND_Max_Expr(WN_kid0(WN_kid1(wn)), loops, nloops); 
          WN *wn_new = LWN_CreateExp2(div_opc, wn_kid0, wn_kid1); 
          return wn_new; 
        }
      default: 
        FmtAssert(0, ("Bounds too complicated for scalar expansion."));
      } 
    }
  default: 
    FmtAssert(0, ("Bounds too complicated for scalar expansion."));  
    return NULL; 
  } 
}  
    
//-----------------------------------------------------------------------
// NAME: BND_Max_Expr
// FUNCTION: Returns an expression which is an upper bound of 'wn' and
//   is invariant in 'loop[0]'.  The array 'nloops[]' of length 'nloops' 
//   is a list of all loops whose induction variables may be used in 'wn'. 
//-----------------------------------------------------------------------

static WN* BND_Max_Expr(WN* wn, 
  		       WN* loops[], 
                       INT nloops) 
{ 
  OPCODE wn_op = WN_opcode(wn);  
  OPERATOR wn_oper = OPCODE_operator(wn_op); 
  WN* wn_new = NULL; 

  switch (wn_oper) {
  case OPR_PAREN:
    return BND_Max_Expr(WN_kid0(wn), loops, nloops);
  case OPR_INTCONST: 
    return LWN_Copy_Tree(wn, TRUE, LNO_Info_Map); 
  case OPR_LDID: 
    {
      if (Is_Index_Variable(wn, loops, nloops)) { 
        wn_new = BND_Upper_Bound(wn, loops, nloops);  
      } else { 
        wn_new = LWN_Copy_Tree(wn, TRUE, LNO_Info_Map);
        LWN_Copy_Def_Use(wn, wn_new, Du_Mgr);
      } 
    } 
    return wn_new;
  case OPR_ADD:
  case OPR_MIN: 
  case OPR_MAX: 
    {
      TYPE_ID ty_com_rtype = OPCODE_rtype(wn_op); 
      TYPE_ID ty_com_desc = OPCODE_desc(wn_op); 
      OPCODE com_opc = OPCODE_make_op(wn_oper, ty_com_rtype, ty_com_desc);  
      WN* wn_kid0 = BND_Max_Expr(WN_kid0(wn), loops, nloops);
      WN* wn_kid1 = BND_Max_Expr(WN_kid1(wn), loops, nloops);
      wn_new = LWN_CreateExp2(com_opc, wn_kid0, wn_kid1); 
    }
    return wn_new;
  case OPR_MPY: 
    {
      TYPE_ID ty_mpy_rtype = OPCODE_rtype(wn_op); 
      TYPE_ID ty_mpy_desc = OPCODE_desc(wn_op); 
      OPCODE mpy_opc = OPCODE_make_op(wn_oper, ty_mpy_rtype, ty_mpy_desc);  
      OPCODE cmp_opc = OPCODE_make_op(OPR_MAX, ty_mpy_rtype, ty_mpy_desc);  
      WN* wn_mpy_kid0 = BND_Max_Expr(WN_kid0(wn), loops, nloops);
      WN* wn_mpy_kid1 = BND_Max_Expr(WN_kid1(wn), loops, nloops);
      WN* wn_new_exp0 = LWN_CreateExp2(mpy_opc, wn_mpy_kid0, wn_mpy_kid1); 
      wn_mpy_kid0 = BND_Min_Expr(WN_kid0(wn), loops, nloops);
      wn_mpy_kid1 = BND_Min_Expr(WN_kid1(wn), loops, nloops);
      WN* wn_new_exp1 = LWN_CreateExp2(mpy_opc, wn_mpy_kid0, wn_mpy_kid1); 
      WN* wn_new_one = LWN_CreateExp2(cmp_opc, wn_new_exp1, wn_new_exp0);
      wn_mpy_kid0 = BND_Max_Expr(WN_kid0(wn), loops, nloops);
      wn_mpy_kid1 = BND_Min_Expr(WN_kid1(wn), loops, nloops);
      wn_new_exp0 = LWN_CreateExp2(mpy_opc, wn_mpy_kid0, wn_mpy_kid1); 
      wn_mpy_kid0 = BND_Min_Expr(WN_kid0(wn), loops, nloops);
      wn_mpy_kid1 = BND_Max_Expr(WN_kid1(wn), loops, nloops);
      wn_new_exp1 = LWN_CreateExp2(mpy_opc, wn_mpy_kid0, wn_mpy_kid1); 
      WN* wn_new_two = LWN_CreateExp2(cmp_opc, wn_new_exp1, wn_new_exp0);
      wn_new = LWN_CreateExp2(cmp_opc, wn_new_one, wn_new_two);
    }
    return wn_new;
  case OPR_NEG: 
    {
      TYPE_ID ty_neg_rtype = OPCODE_rtype(wn_op); 
      TYPE_ID ty_neg_desc = OPCODE_desc(wn_op); 
      OPCODE neg_opc = OPCODE_make_op(wn_oper, ty_neg_rtype, ty_neg_desc); 
      WN* wn_kid = BND_Min_Expr(WN_kid0(wn), loops, nloops); 
      wn_new = WN_CreateExp1(neg_opc, wn_kid);
    }
    return wn_new;  
  case OPR_SUB:
  case OPR_DIV: 
    {
      TYPE_ID ty_sub_rtype = OPCODE_rtype(wn_op); 
      TYPE_ID ty_sub_desc = OPCODE_desc(wn_op); 
      OPCODE sub_opc = OPCODE_make_op(wn_oper, ty_sub_rtype, ty_sub_desc);  
      WN *wn_kid0 = BND_Max_Expr(WN_kid0(wn), loops, nloops);
      WN *wn_kid1 = BND_Min_Expr(WN_kid1(wn), loops, nloops);
      WN *wn_new = LWN_CreateExp2(sub_opc, wn_kid0, wn_kid1);
      return wn_new;
    }
  case OPR_INTRINSIC_OP: 
    {
      INT32 intr = WN_intrinsic(wn); 
      switch (intr) { 
      case INTRN_I4DIVFLOOR: 
      case INTRN_U4DIVFLOOR: 
      case INTRN_I8DIVFLOOR: 
      case INTRN_U8DIVFLOOR: 
	{
          TYPE_ID ty_div_rtype = OPCODE_rtype(wn_op);
          TYPE_ID ty_div_desc = OPCODE_desc(wn_op);
          OPCODE div_opc = OPCODE_make_op(OPR_DIV, ty_div_rtype, ty_div_desc); 
          WN *wn_kid0 = BND_Max_Expr(WN_kid0(WN_kid0(wn)), loops, nloops); 
          WN *wn_kid1 = BND_Min_Expr(WN_kid0(WN_kid1(wn)), loops, nloops); 
          WN *wn_new = LWN_CreateExp2(div_opc, wn_kid0, wn_kid1); 
          return wn_new; 
        }
      case INTRN_I4DIVCEIL:
      case INTRN_U4DIVCEIL: 
      case INTRN_I8DIVCEIL: 
      case INTRN_U8DIVCEIL: 
	{
          TYPE_ID ty_div_rtype = OPCODE_rtype(wn_op);
          TYPE_ID ty_div_desc = OPCODE_desc(wn_op);
          OPCODE div_opc = OPCODE_make_op(OPR_DIV, ty_div_rtype, ty_div_desc); 
          OPCODE add_opc = OPCODE_make_op(OPR_ADD, ty_div_rtype, ty_div_desc); 
          WN *wn_kid0 = BND_Max_Expr(WN_kid0(WN_kid0(wn)), loops, nloops); 
          WN *wn_kid1 = BND_Min_Expr(WN_kid0(WN_kid1(wn)), loops, nloops); 
          WN* wn_div = LWN_CreateExp2(div_opc, wn_kid0, wn_kid1); 
          WN* one = LWN_Make_Icon(ty_div_rtype, 1);  
          WN *wn_new = LWN_CreateExp2(add_opc, wn_div, one); 
          return wn_new; 
        }
      default: 
        FmtAssert(0, ("Bounds too complicated for scalar expansion."));
      } 
  }
#ifdef KEY
  // Bug 5240 - Compiling for a 64-bit target introduces a CVT in the 
  // loop ub that should be handled here.
  case OPR_CVT:
    {
      TYPE_ID ty_cvt_rtype = OPCODE_rtype(wn_op); 
      TYPE_ID ty_cvt_desc = OPCODE_desc(wn_op); 
      OPCODE cvt_opc = OPCODE_make_op(wn_oper, ty_cvt_rtype, ty_cvt_desc); 
      WN* wn_kid = BND_Max_Expr(WN_kid0(wn), loops, nloops); 
      wn_new = WN_CreateExp1(cvt_opc, wn_kid);
      LWN_Parentize(wn_new);
    }
    return wn_new;  
#endif
  default: 
    FmtAssert(0, ("Bounds too complicated for scalar expansion."));  
    return NULL; 
  } 
}  

//-----------------------------------------------------------------------
// NAME: SE_Easy_Loop 
// FUNCTION: Returns TRUE if 'wn_outer_loop' is the outermost loop in a 
//   fully permutable invariant SNL, FALSE otherwise. 
// NOTE: This is a sufficent condition for finalization of this SNL.  
//   It actually is too restricive, and we would like to relax it later. 
//-----------------------------------------------------------------------

static BOOL SE_Easy_Loop(WN* wn_outer_loop)
{
  INT nloops = Is_Inner_SNL(wn_outer_loop); 
  if (nloops == 0)
    return FALSE;
  if (!Fully_Permutable_Permutation(wn_outer_loop, nloops))
    return FALSE; 
  if (!SNL_Is_Distributable(wn_outer_loop, nloops))
    return FALSE; 
  return TRUE; 
}
   
//-----------------------------------------------------------------------
// NAME: SE_New_Outer_Loop
// FUNCTION: Suppose we are scalar expanding 'wn_ref' and that we have
//   encountered a bad reference 'wn_scalar_ref' and that we are currently 
//   restricted to finding a scalar equivalence inside 'wn_outer_loop'. 
//   Return the outermost loop which excludes 'wn_scalar_ref' for which 
//   this loop and all innnermore loops are "safe" for finalization. 
//   Return NULL if there is no such loop. 
//-----------------------------------------------------------------------

static WN* SE_New_Outer_Loop(WN* wn_ref, 
			     WN* wn_scalar_ref,
			     WN* wn_outer_loop) 
{
  WN* wn_new_outer_loop = NULL; 
  WN* wn_common = Common_Ancestor(wn_ref, wn_scalar_ref); 
  for (WN* wn = wn_ref; wn != wn_common; wn = LWN_Get_Parent(wn)) { 
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      if (!SE_Easy_Loop(wn)) 
	break; 
      wn_new_outer_loop = wn; 
    } 
  } 
  if (wn_new_outer_loop == NULL)
    return NULL;
  if (wn_outer_loop != NULL && Do_Loop_Depth(wn_new_outer_loop)  
      <= Do_Loop_Depth(wn_outer_loop)) 
    wn_new_outer_loop = wn_outer_loop; 
  return wn_new_outer_loop; 
} 

//-----------------------------------------------------------------------
// NAME: SE_Prune_Stack_Elements
// FUNCTION: Given a (perhaps partially constructed) equivalence class 
//   'class_stack'.  Discard all elements from this stack which are 
//   outside 'wn_new_outer_loop', and return the index of the element 
//   on the reduced size 'class_stack' which corresponds to 'wn_scalar_ref'. 
//-----------------------------------------------------------------------

static INT SE_Prune_Stack_Elements(STACK<WN*>* class_stack, 
				   WN* wn_new_outer_loop, 
				   WN* wn_scalar_ref)
{
  STACK<WN*> new_stack(&LNO_local_pool); 
  INT new_i = -1; 
  INT j;
  for (j = 0; j < class_stack->Elements(); j++) {
    WN* wn = class_stack->Bottom_nth(j); 
    if (Wn_Is_Inside(wn, wn_new_outer_loop))
      new_stack.Push(wn); 
    if (wn == wn_scalar_ref)
      new_i = new_stack.Elements();  
  }
  class_stack->Clear(); 
  for (j = 0; j < new_stack.Elements(); j++) 
    class_stack->Push(new_stack.Bottom_nth(j)); 
  new_stack.Clear(); 
  return new_i - 1; 
}

//-----------------------------------------------------------------------
// NAME: Conditionally_Assigned
// FUNCTION: Returns TRUE if the 'wn_def' is a store and if it appears 
//   within an IF nested inside 'wn_eq_loop'.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Conditionally_Assigned(WN* wn_def,
				   WN* wn_eq_loop)
{
  if (!OPCODE_is_store(WN_opcode(wn_def)))
    return FALSE; 
  for (WN* wn = wn_def; wn != NULL && wn != wn_eq_loop; 
      wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_IF)
      return TRUE; 
  return FALSE; 
}  
 
//-----------------------------------------------------------------------
// NAME: Scalar_Equivalence_Class 
// FUNCTION: Returns a stack of WNs which can be reached through DEF or i
//   USE chains from 'ref'.  It is used by the scalar renaming and scalar 
//   expansion.  We only rename or expand scalars whose equivalence class
//   is entirely in the loop that we are interested. We use a depth-first  
//   search along the DU chains to collect all the references.  If 'find_
//   restrict' is TRUE, find a restricted equivalence class containing  
//   'ref', all of whose members are inside the loop 'wn_outer_loop' 
//   (the value of which we write in this function).   
//-----------------------------------------------------------------------

extern STACK<WN*>* Scalar_Equivalence_Class(WN* ref, DU_MANAGER* du, 
  MEM_POOL* pool, BOOL find_restrict, WN** wn_outer_loop)
{

  if (find_restrict)
    *wn_outer_loop = NULL; 

  // has to be LDID or STID to start gathering for scalar equ. class
  OPERATOR ref_opr=WN_operator(ref);

  if (ref_opr!=OPR_LDID && ref_opr!=OPR_STID)
    return (STACK<WN*>*)NULL;

  SYMBOL ref_symbol(ref);

  // stack used in the depth-first search 
  STACK<WN*>* class_stack = CXX_NEW(STACK<WN*>(pool), pool);

  // hash table to record when a ref has been traversed
  // allocate 128 entries initially
  HASH_TABLE<const WN*,INT> scalar_ref_table(128, pool);

  class_stack->Push(ref);
  scalar_ref_table.Enter(ref,1);
  INT i = 0;
  while (i<class_stack->Elements()) {
    WN*         scalar_ref = class_stack->Bottom_nth(i++);
    OPCODE      opc = WN_opcode(scalar_ref);
    OPERATOR    opr = OPCODE_operator(opc);
    BOOL        potential_read = FALSE;
    BOOL        potential_write = FALSE;

    switch (opr) {
     case OPR_LDID:
      potential_read = TRUE;
      break;
     case OPR_STID:
      potential_write = TRUE;
      break;
     default:
      if (!find_restrict) { 
	CXX_DELETE(class_stack, pool);
	return (STACK<WN*>*)NULL;
      } else { 
	WN* wn_new_outer_loop = SE_New_Outer_Loop(ref, scalar_ref, 
	  *wn_outer_loop); 
	if (wn_new_outer_loop == NULL) { 
	  CXX_DELETE(class_stack, pool);
	  return (STACK<WN*>*)NULL;
        } 
	if (*wn_outer_loop != wn_new_outer_loop) { 
	  *wn_outer_loop = wn_new_outer_loop; 
	  i = SE_Prune_Stack_Elements(class_stack, wn_new_outer_loop, 
	    scalar_ref);  
        } 
	continue; 
      }
    }

    // will not collect equivalence class for dedicated pregs
    if (ST_class(WN_st(scalar_ref)) == CLASS_PREG &&
        WN_offset(scalar_ref) <= Last_Dedicated_Preg_Offset) {
      if (find_restrict) 
	*wn_outer_loop = NULL; 
      CXX_DELETE(class_stack, pool);
      return (STACK<WN*>*)NULL;
    }
  
    // all symbol has to be the same as the original reference
    SYMBOL sym(scalar_ref);
    if (sym!=ref_symbol || sym.Type != ref_symbol.Type) {
      if (find_restrict) 
	*wn_outer_loop = NULL; 
      CXX_DELETE(class_stack, pool);
      return (STACK<WN*>*)NULL;
    }

    if (potential_read) {
      DEF_LIST *def_list=du->Ud_Get_Def(scalar_ref);
      if (!def_list || def_list->Incomplete()) {
	if (find_restrict) 
	  *wn_outer_loop = NULL; 
        CXX_DELETE(class_stack, pool);
        return (STACK<WN*>*)NULL;
      }
      WN* wn_new_outer_loop = NULL; 
      DEF_LIST_ITER d_iter(def_list);
      for (DU_NODE *def_node=(DU_NODE *)d_iter.First(); !d_iter.Is_Empty();
        def_node=(DU_NODE *)d_iter.Next()) {
        WN* def=def_node->Wn();
        if (scalar_ref_table.Find(def)!=1) {
          class_stack->Push(def);
          scalar_ref_table.Enter(def,1);
        }
      }
      if (red_manager) {
        REDUCTION_TYPE red_type=red_manager->Which_Reduction(scalar_ref);
        if (red_type != RED_NONE) {
          WN* parent = scalar_ref;
          while (!OPCODE_is_store(WN_opcode(parent)))
            parent=LWN_Get_Parent(parent);
          if (scalar_ref_table.Find(parent)!=1) {
            class_stack->Push(parent);
            scalar_ref_table.Enter(parent,1);
          }
        }
      }
    }

    if (potential_write) {
      USE_LIST *use_list=du->Du_Get_Use(scalar_ref);
      if (!use_list || use_list->Incomplete()) {
	if (find_restrict) 
	  *wn_outer_loop = NULL; 
        CXX_DELETE(class_stack, pool);
        return (STACK<WN*>*)NULL;
      }
      USE_LIST_ITER u_iter(use_list);
      for (DU_NODE *use_node=(DU_NODE *)u_iter.First(); !u_iter.Is_Empty();
        use_node=(DU_NODE *)u_iter.Next()) {
        WN* use=use_node->Wn();
        if (scalar_ref_table.Find(use)!=1) {
          class_stack->Push(use);
          scalar_ref_table.Enter(use,1);
        }
      }
      if (red_manager) {
        REDUCTION_TYPE red_type = red_manager->Which_Reduction(scalar_ref);
        if (red_type != RED_NONE) {
	  WN* wn_load = NULL; 
	  LWN_ITER* itr = LWN_WALK_TreeIter(WN_kid0(scalar_ref)); 
          for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
	    wn_load = itr->wn; 
	    if (OPCODE_has_sym(WN_opcode(wn_load)) 
	        && SYMBOL(wn_load) == SYMBOL(scalar_ref)    
	        && red_manager->Which_Reduction(wn_load) == red_type)
	      break; 
	  } 
	  FmtAssert(wn_load != NULL, 
            ("Scalar_Equivalence_Class: Could not find reduction load")); 
          if (scalar_ref_table.Find(wn_load) != 1) {
            class_stack->Push(wn_load);
            scalar_ref_table.Enter(wn_load, 1);
          }
        }
      }
    }
  }
  return class_stack;
}

//-----------------------------------------------------------------------
// NAME: Scalar_Equivalence_Class 
// FUNCTION: Like the above, but don't try to compute a restricted equi-
//   valence class. 
//-----------------------------------------------------------------------

extern STACK<WN*>* Scalar_Equivalence_Class(WN* ref, DU_MANAGER* du, 
  MEM_POOL* pool) 
{
  return Scalar_Equivalence_Class(ref, du, pool, FALSE, NULL); 
}

//-----------------------------------------------------------------------
// NAME: Scalar_Equivalence_Class 
// FUNCTION: Find the scalar equivalence class for 'ref' within 'wn_loop'. 
//   Assumes that only STIDs and LDIDs will be found within this loop. 
//   and that all testing has been done to ensure that this is a valid 
//   equivalence class. 
//-----------------------------------------------------------------------

extern STACK<WN*>* Scalar_Equivalence_Class(WN* ref, DU_MANAGER* du, 
  MEM_POOL* pool, WN* wn_loop)
{

  // has to be LDID or STID to start gathering for scalar equ. class
  OPERATOR ref_opr=WN_operator(ref);
  FmtAssert(ref_opr == OPR_LDID || ref_opr == OPR_STID, 
    ("Scalar_Equivalence_Class: Expected ref to be LDID or STID"));

  SYMBOL ref_symbol(ref);

  // stack used in the depth-first search 
  STACK<WN*>* class_stack = CXX_NEW(STACK<WN*>(pool), pool);

  // hash table to record when a ref has been traversed
  // allocate 128 entries initially
  HASH_TABLE<const WN*,INT> scalar_ref_table(128, pool);

  class_stack->Push(ref);
  scalar_ref_table.Enter(ref,1);
  INT i = 0;
  
  while (i < class_stack->Elements()) {
    WN* scalar_ref = class_stack->Bottom_nth(i++);

    OPCODE opc = WN_opcode(scalar_ref);
    OPERATOR opr = OPCODE_operator(opc);
    BOOL potential_read = FALSE;
    BOOL potential_write = FALSE;

    switch (opr) {
     case OPR_LDID:
      potential_read = TRUE;
      break;
     case OPR_STID:
      potential_write = TRUE;
      break;
     default:
      FmtAssert(TRUE, 
        ("Scalar_Equivalence_Class: Expected ref to be LDID or STID")); 
    } 

    if (potential_read) {
      DEF_LIST *def_list=du->Ud_Get_Def(scalar_ref);
      FmtAssert(def_list != NULL && !def_list->Incomplete(),
        ("Scalar_Equivalence_Class: Expected complete def list"));
      DEF_LIST_ITER d_iter(def_list);
      for (DU_NODE *def_node=(DU_NODE *)d_iter.First(); !d_iter.Is_Empty();
        def_node=(DU_NODE *)d_iter.Next()) {
        WN* def=def_node->Wn();
        if (scalar_ref_table.Find(def)!=1 && Wn_Is_Inside(def, wn_loop)) {
          class_stack->Push(def);
          scalar_ref_table.Enter(def,1);
        }
      }
      if (red_manager) {
        REDUCTION_TYPE red_type=red_manager->Which_Reduction(scalar_ref);
        if (red_type != RED_NONE) {
          WN* parent = scalar_ref;
          while (!OPCODE_is_store(WN_opcode(parent)))
            parent=LWN_Get_Parent(parent);
          if (scalar_ref_table.Find(parent)!=1 
	      && Wn_Is_Inside(parent, wn_loop)) {
            class_stack->Push(parent);
            scalar_ref_table.Enter(parent,1);
          }
        }
      }
    }

    if (potential_write) {
      USE_LIST *use_list=du->Du_Get_Use(scalar_ref);
      FmtAssert(use_list != NULL && !use_list->Incomplete(),
        ("Scalar_Equivalence_Class: Expected complete use list"));
      USE_LIST_ITER u_iter(use_list);
      for (DU_NODE *use_node=(DU_NODE *)u_iter.First(); !u_iter.Is_Empty();
        use_node=(DU_NODE *)u_iter.Next()) {
        WN* use=use_node->Wn();
        if (scalar_ref_table.Find(use)!=1 && Wn_Is_Inside(use, wn_loop)) {
          class_stack->Push(use);
          scalar_ref_table.Enter(use,1);
        }
      }
    }
  }
  return class_stack;
}

//-----------------------------------------------------------------------
// NAME: IF_Complement
// FUNCTION: If 'wn_node' is the THEN block of an IF, return the ELSE 
//   block.  If it is the ELSE block, return the THEN block.  Otherwise,
//   return NULL.
//-----------------------------------------------------------------------

static WN* IF_Complement(WN* wn_node)
{
  WN* wn_if = LWN_Get_Parent(wn_node); 
  if (WN_operator(wn_if) != OPR_IF)
    return NULL; 
  if (wn_node == WN_then(wn_if))
    return WN_else(wn_if);
  if (wn_node == WN_else(wn_if))
    return WN_then(wn_if);
  return NULL; 
} 

//-----------------------------------------------------------------------
// NAME: IF_Branch
// FUNCTION: Return the nearest ancestor of 'wn_node' which is the THEN 
//   or ELSE branch of an IF which is inside 'wn_def_loop', if such a 
//   branch exists.  Otherwise, return 'wn_def_loop' if 'wn_node' is 
//   inside 'wn_def_loop' and NULL otherwise. 
//-----------------------------------------------------------------------

static WN* IF_Branch(WN* wn_node,
		     WN* wn_def_loop)
{ 
  WN* wnn = NULL; 
  for (WN* wn = wn_node; wn != NULL; wnn = wn, wn = LWN_Get_Parent(wn)) {
    if (wn == wn_def_loop)
      return wn_def_loop; 
    if (WN_operator(wn) == OPR_IF 
	&& (WN_then(wn) == wnn || WN_else(wn) == wnn)) 
      return wnn;
  } 
  return NULL; 
} 

//-----------------------------------------------------------------------
// NAME: Has_Cutset 
// FUNCTION: Return TRUE if the STIDs on 'stk_equiv' form a cutset for 
//   the loop 'wn_def_loop'.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Has_Cutset(STACK<WN*>* stk_equiv,
		       WN* wn_def_loop)
{
  WN** cutset = CXX_NEW_ARRAY(WN*, stk_equiv->Elements(),
    &LNO_local_pool);
  for (INT i = 0; i < stk_equiv->Elements(); i++) { 
    WN* wn_node = stk_equiv->Bottom_nth(i);
    cutset[i] = WN_operator(wn_node) == OPR_STID
      ? IF_Branch(wn_node, wn_def_loop) : NULL;
    if (cutset[i] == wn_def_loop)
      return TRUE; 
  }
  BOOL change = TRUE; 
  while (change) { 
    change = FALSE; 
    for (INT i = 0; i < stk_equiv->Elements(); i++) {
      WN* wn_node = stk_equiv->Bottom_nth(i);
      WN* wn_cutset = cutset[i];
      if (wn_cutset == NULL)
	continue; 
      WN* wn_complement = IF_Complement(wn_cutset);
      FmtAssert(wn_complement != NULL,
	("Has_Cutset: Could not find IF complement"));
      INT j;
      for (j = 0; j < i; j++)  
        if (cutset[j] == wn_complement)
	  break; 
      if (j < i) { 
        WN* wn_new = IF_Branch(LWN_Get_Parent(cutset[i]), wn_def_loop);
        if (wn_new == wn_def_loop)
	  return TRUE; 
        for (INT k = 0; k < stk_equiv->Elements(); k++) {
	  if (cutset[k] == wn_cutset || cutset[k] == wn_complement)
            cutset[k] = wn_new;
	} 
	change = TRUE; 
      } 
    }
  }
  return FALSE;
} 

//-----------------------------------------------------------------------
// NAME: Scalar_Expandable 
// FUNCTION: Returns SE_EASY or SE_HARD if we can scalar expand 'ref' and 
//   the members of its 'equivalence_class' within the given 'loop'.  
//   Returns SE_NONE otherwise.  SE_HARD means we need to compute a final 
//   value for the scalar outside the loop, SE_EASY means we don't have to.
// NOTE: We can replace s with s(i) if and only if the loop_stmts are null
//   or point to a loop inside the given loop.
//-----------------------------------------------------------------------

extern SE_RESULT Scalar_Expandable(STACK<WN*>* stk_equiv,
  				   WN* wn_ref, 
				   WN* wn_def_loop, 
			           DU_MANAGER *du, 
				   WN* wn_outer_loop,
				   WN* wn_eq_loop) 
{
  SYMBOL sym_ref(wn_ref);

  if (!Upper_Bound_Standardize(WN_end(wn_def_loop), TRUE)) 
    return SE_NONE;
  
  if (stk_equiv == NULL)
    return SE_NONE;

  if (wn_eq_loop != NULL && !Wn_Is_Inside(wn_def_loop, wn_eq_loop)) 
    return SE_NONE; 

  if (!Wn_Is_Inside(wn_def_loop, wn_outer_loop))
    return SE_NONE; 

  if (!Has_Cutset(stk_equiv, wn_def_loop))
    return SE_NONE; 

  INT stid_count = 0; 
  BOOL found_lcd = FALSE; 
  INT inner_ldid_count = 0; 
  for (INT i = 0; i < stk_equiv->Elements(); i++) {
    WN* wn_scalar_ref = stk_equiv->Bottom_nth(i);
    OPERATOR opr = WN_operator(wn_scalar_ref);
    if (opr != OPR_LDID && opr != OPR_STID) 
      return SE_NONE;
    if (SYMBOL(wn_scalar_ref) != sym_ref) 
      return SE_NONE;
    if (opr == OPR_STID) { 
      stid_count++; 
    } else if (opr == OPR_LDID) {
      if (!Wn_Is_Inside(wn_scalar_ref, wn_outer_loop)
	  && wn_eq_loop != wn_outer_loop) {
	for (WN* wn = wn_ref; wn != NULL; wn = LWN_Get_Parent(wn)) {  
	  if (WN_opcode(wn) == OPC_DO_LOOP && !SE_Easy_Loop(wn))
	     return SE_NONE; 
	  if (wn == wn_outer_loop)
	     break;
	} 
	wn_eq_loop = wn_outer_loop; 
	i = SE_Prune_Stack_Elements(stk_equiv, wn_eq_loop, wn_scalar_ref);  
	continue; 
      } 
      WN* wn_parent = LWN_Get_Parent(wn_scalar_ref);   
      if (wn_parent != NULL && WN_operator(wn_parent)
	  == OPR_ARRAY && WN_array_base(wn_parent) == wn_scalar_ref) 
	return SE_NONE; 
      DEF_LIST* def_list = du->Ud_Get_Def(wn_scalar_ref);
      WN* wn_loop_stmt = def_list->Loop_stmt();
      WN* wn_enclosing_loop = Enclosing_Do_Loop(wn_scalar_ref); 
      DO_LOOP_INFO* dli_enclosing = NULL; 
      if (wn_enclosing_loop != NULL) { 
	dli_enclosing = Get_Do_Loop_Info(wn_enclosing_loop); 
	if (dli_enclosing->Is_Inner)
	  inner_ldid_count++; 
      } 
      if (wn_loop_stmt != NULL && (wn_loop_stmt == wn_def_loop 
	  || !Wn_Is_Inside(wn_loop_stmt, wn_def_loop))) {
	if (Conditionally_Assigned(wn_scalar_ref, wn_outer_loop))
	  return SE_NONE;
	if (dli_enclosing != NULL && dli_enclosing->Is_Inner)
	  return SE_NONE;  
	found_lcd = TRUE; 
      } 
    }
  }
  if (found_lcd && (stid_count > 1 || inner_ldid_count == 0))
    return SE_NONE; 
  return (wn_eq_loop != NULL) ? (found_lcd ? SE_HARD_LCD : SE_HARD) 
    : (found_lcd ? SE_EASY_LCD : SE_EASY);
}

//-----------------------------------------------------------------------
// NAME: Scalar_Expandable 
// FUNCTION: Returns SE_EASY or SE_HARD if we can scalar expand 'ref' and 
//   the members of its equivalence class within the given 'loop'.  
//   Returns SE_NONE otherwise.  SE_HARD means we need to compute a final 
//   value for the scalar outside the loop, SE_EASY means we don't have to.
// NOTE: We can replace s with s(i) if and only if the loop_stmts are null
//   or point to a loop inside the given loop.
//-----------------------------------------------------------------------

extern SE_RESULT Scalar_Expandable(WN* ref, WN* loop, DU_MANAGER *du)
{

  if (!Upper_Bound_Standardize(WN_end(loop), TRUE)) 
    return SE_NONE;

  WN* wn_eq_loop = NULL;  
  MEM_POOL_Push(&LNO_local_pool);
  STACK<WN*>* equivalence_class =
    Scalar_Equivalence_Class(ref, du, &LNO_local_pool, TRUE, &wn_eq_loop);
  SE_RESULT can_expand = Scalar_Expandable(equivalence_class, ref, loop, du,
    loop, wn_eq_loop);
  CXX_DELETE(equivalence_class, &LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);
  return can_expand;
}

// scalar expansion is not necessary if you have
//       do
//         x = 
//          = x
//         do
//         end do
//       end do
// and x is not used anywhere else.
// If you have
//       do
//         x = 
//         do
//         end do
//          = x
//       end do
// then you actually do need to scalar expand for tiling.  If you didn't
// the test would just be whether they were all in the same loop (first 
// do loop parent all the same).  Instead, it's as follows.  Go back to the
// first block with a do loop.  Is that do loop the same for all x, and are
// all x either above or below that do loop.  If there is no such block
// (it's directly under a do, or not under any do), then also doesn't need
// scalar expansion.

enum LOOP_RELATIONSHIP {LR_ABOVE=345, LR_BELOW, LR_DESCENDANT};

static void Get_Do_And_Above(WN* ref, WN** loopp, LOOP_RELATIONSHIP* lrp)
{
  WN*  block_kid = ref;
  WN*  block = LWN_Get_Parent(block_kid);

  while (block && WN_opcode(block) != OPC_DO_LOOP) {
    Is_True(LWN_Get_Parent(block_kid) == block, ("Bug"));

    if (WN_opcode(block) == OPC_BLOCK) {
      WN*  wn;

      // if do above or below, return that.
      for (wn = WN_prev(block_kid); wn; wn = WN_prev(wn)) {
        if (WN_opcode(wn) == OPC_DO_LOOP) {
          *loopp = wn;
          *lrp = LR_ABOVE;
          return;
        }
      }
      for (wn = WN_next(block_kid); wn; wn = WN_next(wn)) {
        if (WN_opcode(wn) == OPC_DO_LOOP) {
          *loopp = wn;
          *lrp = LR_BELOW;
          return;
        }
      }
    }
    block_kid = block;
    block = LWN_Get_Parent(block_kid);
  }

  *loopp = block;
  *lrp = LR_DESCENDANT;
}

BOOL Scalar_Expansion_Not_Necessary(WN* ref, DU_MANAGER* du)
{
  MEM_POOL_Push(&LNO_local_pool);

  SYMBOL ref_symbol(ref);
  WN* wn_eq_loop = NULL; 
  STACK<WN*>* equivalence_class =
    Scalar_Equivalence_Class(ref, du, &LNO_local_pool, TRUE, &wn_eq_loop);
  if (wn_eq_loop != NULL) {
    MEM_POOL_Pop(&LNO_local_pool);
    return FALSE; 
  } 

  WN*               loop;
  LOOP_RELATIONSHIP lr;

  Get_Do_And_Above(ref, &loop, &lr);

  while (!equivalence_class->Is_Empty()) {
    WN* scalar_ref = equivalence_class->Pop();
    WN*               loop2;
    LOOP_RELATIONSHIP lr2;

    Get_Do_And_Above(scalar_ref, &loop2, &lr2);
    if (loop != loop2 || lr != lr2) {
      MEM_POOL_Pop(&LNO_local_pool);
      return FALSE;
    }
  }

  MEM_POOL_Pop(&LNO_local_pool);
  return TRUE;
}

// --------------------------------


// used in a template, so can't be declared locally

// The only tricky issue with scalar expansion is how to get the space.
// There are three ways I can think of
//      1.  Allocate static space.  Sometimes you don't know at compile
//          time how much space you need, so not general.
//      2.  Call malloc/free.  This is slow, although if it happens inside
//          a loop, it may be possible to pull it outside the loop,
//          mallocing the max space that might be needed.  General, but
//          not fun to code, and sometimes still too slow.
//      3.  Use the stack.  We have sufficient control of the stack to
//          do this, and it's cheap, and scalar expansion uses a stack
//          discipline (LIFO).  Perfect.
// So we do (3) as a default.  We also implement (2), but without trying
// to pull the allocs out of the loop.  That makes the implementation of
// (2) useful for testing only.  Note that pulling the allocs out would
// make (2) better and would help (3) also, though (3) is good enough
// already.
//
// TODO: Stack primitives don't exist yet, so can't be implemented.

static void Mark_Calls(WN* wn)
{
  for ( ; wn; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
      if (dli) dli->Has_Calls = TRUE;
    }
  }
}

static INT unique_se_id = 0;

static TY_IDX se_type_array[MTYPE_LAST + 1]; 

extern void SE_Symbols_For_SE(SYMBOL* ptr_array, const char* pref, INT unique_id, 
  TYPE_ID mtype)
{
  char name[64];
  ST *st;

  sprintf(name, "$%s%d_%s", pref, unique_id, MTYPE_name(mtype));
  st = New_ST(CURRENT_SYMTAB);

  TY_IDX ty_base = se_type_array[mtype];  
  if (ty_base == (TY_IDX) NULL) {
    ty_base = Copy_TY(Be_Type_Tbl(mtype)); 
    se_type_array[mtype] = ty_base; 
  }
  TY_IDX ty_idx = Make_Pointer_Type (ty_base);
  if (!TY_ptr_as_array(ty_idx)) {
    char buf[32];
    sprintf (buf, "->%d", TY_IDX_index (ty_base));
    TY &ty = New_TY (ty_idx);
    TY_Init (ty, Pointer_Size, KIND_POINTER, Pointer_Mtype, Save_Str(buf));
    Set_TY_pointed (ty, ty_base);
    Set_TY_align (ty_idx, Pointer_Size);
    Set_TY_ptr_as_array (ty);
  }

  ST_Init (st,
           Save_Str(name),
           CLASS_VAR,
           SCLASS_AUTO,
           EXPORT_LOCAL,
           ty_idx);
  Set_ST_is_temp_var(st);
  *ptr_array = SYMBOL(st, 0, Pointer_type);

  Set_ST_pt_to_unique_mem(st);
  Set_ST_pt_to_compiler_generated_mem(st);
}

extern WN* Get_Expansion_Space(SYMBOL se_ptrsym, 
			       WN* bsz, 
			       const char* pref,
			       INT unique_id,
			       TYPE_ID wtype,
			       WN* allocregion,
			       WN* useregion,
			       WN* deallocregion)
{
  DU_MANAGER* du = Du_Mgr;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 

  WN*           newstdf = NULL;
  TY_IDX           pty = Make_Pointer_Type(Be_Type_Tbl(wtype));

  OPCODE        stop = OPCODE_make_op(OPR_STID, MTYPE_V, Pointer_type);
  OPCODE        ldop = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);
  OPCODE        subop = OPCODE_make_op(OPR_SUB, Pointer_type, MTYPE_V);
  OPCODE        addop = OPCODE_make_op(OPR_ADD, Pointer_type, MTYPE_V);

  BOOL          use_sp = LNO_Use_Malloc == FALSE;
  FmtAssert(!Get_Trace(TP_LNOPT, TT_LNO_SE_MALLOC),
            ("-ttLNO for malloc is obsolete: use -LNO:use_malloc"));
  SYMBOL        sp_tmp;
  WN*           sp_tmp_def;

  PREG_NUM      rreg1;
  PREG_NUM      rreg2;

  // before the region, generate
  //    malloc(bsz); newst = callreturnval
  // or
  //    tmp = get_sp(); alloca(bsz); newst = callreturnval

  if (use_sp) {
    // tmp = get_sp()
    char newstname[32];
    sprintf(newstname, "$%s%d__$stk", pref, unique_id);
    sp_tmp = Create_Preg_Symbol(newstname, Pointer_type);

    OPCODE      op = OPCODE_make_op(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V);
    WN*         call = WN_Create(op, 0);

    WN_intrinsic(call) = Pointer_Size == 8 ?
                         INTRN_U8READSTACKPOINTER :
                         INTRN_U4READSTACKPOINTER;
    ST*         rst = Find_Return_Registers(Pointer_type, &rreg1, &rreg2);
    FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));

    LWN_Copy_Linenumber(allocregion, call);
    LWN_Insert_Block_Before(LWN_Get_Parent(allocregion), allocregion, call);
    if (Block_Loop_Depth(call) >= 0) {
      if (dg && !dg->Add_Vertex(call)) {
        LNO_Erase_Dg_From_Here_In(call, dg);
      }
    }
    WN* rreg_ldid = WN_CreateLdid(ldop, rreg1, rst, pty);
    Create_alias(Alias_Mgr, rreg_ldid);
    du->Add_Def_Use(call, rreg_ldid);
    sp_tmp_def = LWN_CreateStid(stop, sp_tmp.WN_Offset(), 
                                sp_tmp.St(), pty, rreg_ldid);
    Create_alias(Alias_Mgr, sp_tmp_def);
    LWN_Copy_Linenumber(allocregion, sp_tmp_def);
    LWN_Copy_Frequency_Tree(sp_tmp_def,allocregion);
    LWN_Insert_Block_Before(LWN_Get_Parent(allocregion), allocregion,
                            sp_tmp_def);
  }

  OPCODE        icallop = OPCODE_make_op(OPR_INTRINSIC_CALL,
                                         Pointer_type, MTYPE_V);
  WN*           call = WN_Create(icallop, 1);

  ST*           rst = Find_Return_Registers(Pointer_type, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));

  if (use_sp) {
#ifdef _NEW_SYMTAB
    Set_PU_has_alloca(Get_Current_PU());
#else
    Set_SYMTAB_has_alloca(Current_Symtab);
#endif
    WN_intrinsic(call) =
      Pointer_Size == 8 ? INTRN_U8I8ALLOCA : INTRN_U4I4ALLOCA;
  }
  else {
    WN_intrinsic(call) = 
      Pointer_Size == 8 ? INTRN_U8I8MALLOC : INTRN_U4I4MALLOC;
  }
  LWN_Copy_Linenumber(allocregion, call);
  if (LNO_Use_Parm) {
    WN* tmp=WN_CreateParm(MTYPE_U8, bsz, Be_Type_Tbl(MTYPE_U8), 
      WN_PARM_BY_VALUE);
    LWN_Set_Parent(bsz, tmp);
    bsz=tmp;
  }
  WN_kid0(call) = bsz;
  LWN_Set_Parent(bsz, call);

  LWN_Copy_Frequency_Tree(call, allocregion);

  LWN_Insert_Block_Before(LWN_Get_Parent(allocregion), allocregion, call);
  if (Block_Loop_Depth(call) >= 0) {
    if (dg && !dg->Add_Vertex(call)) {
      LNO_Erase_Dg_From_Here_In(call, dg);
    }
  }
  WN* rreg_ldid = WN_CreateLdid(ldop, rreg1, rst, pty);
  Create_alias(Alias_Mgr, rreg_ldid);
  du->Add_Def_Use(call, rreg_ldid);
  newstdf = LWN_CreateStid(stop, se_ptrsym.WN_Offset(), 
		se_ptrsym.St(), pty, rreg_ldid);
  Create_local_alias(Alias_Mgr,newstdf);
  LWN_Copy_Linenumber(useregion, newstdf);

  LWN_Copy_Frequency_Tree(newstdf, allocregion);

  LWN_Insert_Block_Before(LWN_Get_Parent(allocregion), allocregion, newstdf);
  Mark_Calls(call);

  // after the region, generate 'free(newst);' or 'sp = tmp'

  icallop = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
  call = WN_Create(icallop, ((use_sp && Alloca_Dealloca_On) ? 2 : 1));
  WN* wn_callarg;
  if (use_sp) {
    WN_intrinsic(call) = Pointer_Size == 8 ?
                         INTRN_U8I8SETSTACKPOINTER :
                         INTRN_U4I4SETSTACKPOINTER;
    wn_callarg = LWN_CreateLdid(ldop, sp_tmp_def);
    du->Ud_Add_Def(wn_callarg, sp_tmp_def);
    du->Du_Add_Use(sp_tmp_def, wn_callarg);
  }
  else {
    WN_intrinsic(call) = Pointer_Size == 8 ? INTRN_U8FREE : INTRN_U4FREE;
    wn_callarg = WN_CreateLdid(ldop, se_ptrsym.WN_Offset(), 
      se_ptrsym.St(),pty);
    Create_alias(Alias_Mgr, wn_callarg);
    du->Ud_Add_Def(wn_callarg, newstdf);
    du->Du_Add_Use(newstdf, wn_callarg);
  }

  if (LNO_Use_Parm) {
    WN* tmp=WN_CreateParm(Pointer_type,wn_callarg,pty,WN_PARM_BY_VALUE);
    LWN_Set_Parent(wn_callarg, tmp);
    wn_callarg=tmp;
  }
  WN_kid0(call) = wn_callarg;
  LWN_Set_Parent(wn_callarg, call);
  LWN_Copy_Linenumber(useregion, call);

  if (use_sp && Alloca_Dealloca_On) {
    WN_kid1(call) = WN_CreateParm(Pointer_type,
                                  WN_CreateLdid(ldop,
                                                se_ptrsym.WN_Offset(),
                                                se_ptrsym.St(), pty),
                                  pty, WN_PARM_BY_VALUE);
    LWN_Parentize(call);
    du->Add_Def_Use (newstdf, WN_kid0(WN_kid1(call)));
  }

  LWN_Copy_Frequency_Tree(call, deallocregion);

  LWN_Insert_Block_After(LWN_Get_Parent(deallocregion), deallocregion, call);
  if (Block_Loop_Depth(call) >= 0) {
    if (dg && !dg->Add_Vertex(call)) {
      LNO_Erase_Dg_From_Here_In(call, dg);
    }
  }

  return newstdf;

}

//-----------------------------------------------------------------------
// NAME: SE_Assign_Lexcounts
// FUNCTION: Assign lexcounts to each of the nodes on 'deflist' and 'uselist'
//   which are either ILOADs or ISTOREs with array base 'sym' and which must 
//   be within the region from 'allocregion' to 'deallocregion' inclusive.  
//-----------------------------------------------------------------------

static void SE_Assign_Lexcounts(DYN_ARRAY<WN_REFERENCE_INFO>& deflist,
				DYN_ARRAY<WN_REFERENCE_INFO>& uselist,
				WN* allocregion,
				WN* deallocregion, 
				SYMBOL sym)
{
  INT lexcount = 0; 
  for (WN* wnn = allocregion; wnn != NULL; wnn = WN_next(wnn)) { 
    LWN_ITER* itr = LWN_WALK_TreeIter(wnn); 
    for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
      WN* wn = itr->wn; 
      if (Enclosing_Do_Loop(wn) == NULL)
	continue; 
      if (WN_operator(wn) == OPR_ILOAD 
	  && WN_operator(WN_kid0(wn)) == OPR_ARRAY
	  && OPCODE_has_sym(WN_opcode(WN_array_base(WN_kid0(wn))))
	  && SYMBOL(WN_array_base(WN_kid0(wn))) == sym) {
        INT i;
	for (i = 0; i <= uselist.Lastidx(); i++) 
	  if (uselist[i].wn == wn)
	    break;
	FmtAssert(i <= uselist.Lastidx(), 
          ("SE_Assign_Lexcounts: Could not find use"));
	FmtAssert(uselist[i].lexcount == 0, 
	  ("SE_Assign_Lexcounts: Use already assigned a lexcount")); 
	uselist[i].lexcount = ++lexcount; 
      }
      if (WN_operator(wn) == OPR_ISTORE 
	  && WN_operator(WN_kid1(wn)) == OPR_ARRAY
	  && OPCODE_has_sym(WN_opcode(WN_array_base(WN_kid1(wn))))
	  && SYMBOL(WN_array_base(WN_kid1(wn))) == sym) {
        INT i;
	for (i = 0; i <= deflist.Lastidx(); i++) 
	  if (deflist[i].wn == wn)
	    break;
	FmtAssert(i <= deflist.Lastidx(), 
          ("SE_Assign_Lexcounts: Could not find def"));
	FmtAssert(deflist[i].lexcount == 0, 
	  ("SE_Assign_Lexcounts: Def already assigned a lexcount")); 
	deflist[i].lexcount = ++lexcount; 
      }
    } 
    if (wnn == deallocregion)
      break;
  } 
  INT i;
  for (i = 0; i <= deflist.Lastidx(); i++)
    FmtAssert(deflist[i].lexcount != 0,
      ("SE_Assign_Lexcounts: Did not assign a lexcount to def"));
  for (i = 0; i <= uselist.Lastidx(); i++)
    FmtAssert(uselist[i].lexcount != 0,
      ("SE_Assign_Lexcounts: Did not assign a lexcount to use"));
}

//-----------------------------------------------------------------------
// NAME: SE_Fix_Dependence 
// FUNCTION: Update the dependences for the definitions (given in 'deflist')
//   and the uses (given in 'uselist') of the scalar expanded variable.   
// NOTE: There is room for improvement, because the SSA graph tells us some
//   information we are losing here.  Still, definitely good enough. TODO 
//   OK: calling Add_Edge calls dependence analysis, which is trivial in 
//   this case.  Silly, though, to redo dependence analysis.  Not critical,
//   probably not important, so leave in for a while. (MJW)
//-----------------------------------------------------------------------

extern void SE_Fix_Dependence(DYN_ARRAY<WN_REFERENCE_INFO>& deflist,
			      DYN_ARRAY<WN_REFERENCE_INFO>& uselist)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;  
  for (INT d = 0; d <= deflist.Lastidx(); d++) {
    DOLOOP_STACK dstack(&LNO_local_pool);
    Build_Doloop_Stack(deflist[d].wn, &dstack);

    FmtAssert(WN_operator(WN_kid1(deflist[d].wn)) == OPR_ARRAY,
	      ("Bad child of deflist[d]"));

    // def-def dependences
    for (INT dd = d; dd <= deflist.Lastidx(); dd++) {
      FmtAssert(WN_operator(WN_kid1(deflist[dd].wn)) 
	== OPR_ARRAY, ("Bad child of deflist[d]"));
      DOLOOP_STACK ddstack(&LNO_local_pool);
      Build_Doloop_Stack(deflist[dd].wn, &ddstack);
      if (!dg->Add_Edge(deflist[d].wn, &dstack, deflist[dd].wn, &ddstack,
			deflist[d].lexcount < deflist[dd].lexcount)) {
        LNO_Erase_Dg_From_Here_In(WN_kid1(deflist[d].wn), dg); 
        LNO_Erase_Dg_From_Here_In(WN_kid1(deflist[dd].wn), dg); 
      }   
    }

    // def-use dependences
    for (INT uu = 0; uu <= uselist.Lastidx(); uu++) {
      FmtAssert(WN_operator(WN_kid0(uselist[uu].wn)) 
	== OPR_ARRAY, ("Bad child of uselist[uu]"));
      DOLOOP_STACK uustack(&LNO_local_pool);
      Build_Doloop_Stack(uselist[uu].wn, &uustack);
      if (!dg->Add_Edge(deflist[d].wn, &dstack, uselist[uu].wn, &uustack,
			deflist[d].lexcount < uselist[uu].lexcount)) { 
        LNO_Erase_Dg_From_Here_In(WN_kid1(deflist[d].wn), dg); 
        LNO_Erase_Dg_From_Here_In(WN_kid0(uselist[uu].wn), dg);
      }  
    }
  }
}

//-----------------------------------------------------------------------
// NAME: SE_Wrap_Array
// FUNCTION: Create a 'wrap_index'th wrap-around OPR_ARRAY scalar-expanded 
//   variable whose new symbol is 'se_ptrsym', which has 'dimcnt' dimensions, 
//   occupies 'sz' bytes of storage, whose dimensions are placed in the 
//   'order' specified by this permutation vector, and whose 'bounds' and 
//   'indxs' were computed by 'SE_Bounds_and_Indxs'.  If 'is_load' is TRUE 
//   make the OPR_ARRAY for the ILOAD in the wrap around statement, other-
//   wise make the OPR_ARRAY for the ISTORE. 
// EXAMPLES: These wrap around statements have the following form: 
//   For 'dimcnt' == 2: 
//     s(0,n) = s 
//     s(i,0) = s(i-1,n)
//   For 'dimcnt' == 3: 
//     s(0,n,n) = s 
//     s(i,0,n) = s(i-1,n,n)
//     s(i,j,0) = s(i,j-1,n) 
//-----------------------------------------------------------------------

static WN* SE_Wrap_Array(SYMBOL se_ptrsym,
		         INT sz, 
                         INT wrap_index,
			 WN* indxs[], 
                         WN* bounds[],
			 const INT order[], 
                         INT dimcnt,
			 BOOL is_load)
{ 
  DU_MANAGER* du = Du_Mgr; 
  OPCODE ldop = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);
  OPCODE op_array = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
  WN* wn_array = WN_Create(op_array, dimcnt + dimcnt + 1);
  WN_element_size(wn_array) = sz;
  WN_array_base(wn_array) = WN_CreateLdid(ldop, se_ptrsym.WN_Offset(),
    se_ptrsym.St(), ST_type(se_ptrsym.St()));
  LWN_Set_Parent(WN_array_base(wn_array), wn_array);
  for (INT i = 0; i < dimcnt; i++) {
    INT j = order[i];
    WN* wn_index = NULL; 
    if (j < wrap_index || j == wrap_index && !is_load) { 
      wn_index = LWN_Copy_Tree(indxs[j]);
      LWN_Copy_Def_Use(indxs[j], wn_index, du);
      TYPE_ID desc = Promote_Type(WN_rtype(indxs[j]));
      OPCODE addop = OPCODE_make_op(OPR_ADD, desc, MTYPE_V);
      WN* wn_one = LWN_Make_Icon(desc, 1);
      wn_index = LWN_CreateExp2(addop, wn_index, wn_one);
    } else if (j == wrap_index && is_load) {
      wn_index = LWN_Copy_Tree(indxs[j]);
      LWN_Copy_Def_Use(indxs[j], wn_index, du);
    } else if (j == wrap_index + 1 && !is_load) { 
      TYPE_ID desc = Promote_Type(WN_rtype(indxs[j]));
      wn_index = LWN_Make_Icon(desc, 0);  
    } else { 
      wn_index = LWN_Copy_Tree(bounds[j]); 
      LWN_Copy_Def_Use(bounds[j], wn_index, du); 
      TYPE_ID desc = Promote_Type(WN_rtype(bounds[j]));
      OPCODE subop = OPCODE_make_op(OPR_SUB, desc, MTYPE_V);
      WN* wn_one = LWN_Make_Icon(desc, 1);
      wn_index = LWN_CreateExp2(subop, wn_index, wn_one);  
    }  
    WN_array_index(wn_array, i) = wn_index; 
    WN* wn_bound = LWN_Copy_Tree(bounds[j]);
    LWN_Copy_Def_Use(bounds[j], wn_bound, du);
    WN_array_dim(wn_array, i) = wn_bound;  
    LWN_Set_Parent(WN_array_index(wn_array, i), wn_array);
    LWN_Set_Parent(WN_array_dim(wn_array, i), wn_array);
  }
  return wn_array; 
} 

//-----------------------------------------------------------------------
// NAME: SE_Find_Stid
// FUNCTION: Return the outermost STID on the stack of scalar expandable 
//   references 'stk_se'.  
//-----------------------------------------------------------------------

static WN* SE_Find_Stid(STACK<WN*>* stk_se)
{ 
  WN* wn_stid = NULL; 
  for (INT i = 0; i < stk_se->Elements(); i++) {
    WN* wn = stk_se->Bottom_nth(i); 
    if (WN_operator(wn) == OPR_STID
	&& (wn_stid == NULL || Do_Depth(wn) < Do_Depth(wn_stid)))
      wn_stid = wn; 
  } 
  FmtAssert(wn_stid != NULL, 
    ("Scalar_Expand: Could not find OPR_STID def")); 
  return wn_stid;  
} 

//-----------------------------------------------------------------------
// NAME: Scalar_Expand
// FUNCTION: Expand the references in the samer equivalence class as 'wn_sym'
//   within the given 'region', given that it is allocated for the 
//   'allocregion' and deleted at the end of that 'allocregion'.  
//   The reference is defined over 'loops[]' of which there are 'dimcnt', 
//   and the 'tile_loops[]' of which there are 'nstrips'.
//   These tile loops have been introduced specifically for scalar expansion.
//   The sizes of these scalar expansion strips are given in 'strip_sizes'. 
//   The permutation order on the inner core of loops is stored in 'order'.
//   This influences what order the array indexing is generated for the 
//   scalar expanded variable.  The variable will have 'dimcnt' dimensions
//   when it is created.  
//   and scalar dependence information. 
//     If 'invariant' is TRUE, scalar expansion is being performed
//   on an invarinat nest.  If 'finalize' is TRUE, we need to
//   compute a final value for the scalar expanded variable. 
//
// ADDITIONAL NOTES: 
//   (1) nstrips <= dimcnt.  Before nstrips == dimcnt, but now some of 
//       the loops will not be scalar expansion tiled.  Now 'nstrips' 
//       loops will be scalar expansion tiled, while 'dimcnt-nstrips'
//       loops will NOT be scalar expansion tiled. 
//   (2) loops[0..dimcnt-1].  These are the actual original loops (not tile
//       loops) over which the scalar expansion will be performed. 
//   (3) tile_loops[0..nstrips-1].  These are the actual tile loops which 
//       will participate in the indexing of THIS scalar expanded array. 
//       It is a (not necessarily proper) subset of the total set of scalar 
//       expanded tiled loops for all of the scalar expanded arrays in this
//       nest.   
//   (4) stripsizes[0..dimcnt-1].  Formerly, these entries were all non-zero
//       and they all had the same constant value.  Now some of them may 
//       be 0, in which case we compute a formula for the scalar expanded 
//       variable's dimension: formula = UPPER_BOUND(ub)-LOWER_BOUND(lb)+1.  
//       When stripsize[i] > 0, we use MIN(formula, stripsize[1]) as the 
//       formula for the dimension.  
//   (5) order[0..dimcnt-1].  This should be a permutation vector. 
//   (6) When 'invariant' is TRUE we use "ub-lb+1" instead of UPPER_BOUND(ub)
//       -LOWER_BOUND(lb)+1 because we are sure that "ub-lb+1" does not 
//       contain any loop index variables. 
//   (7) If used_loops is set, only expand the scalar over the loops for
//	 which used_loops[] is set
//-----------------------------------------------------------------------

extern void Scalar_Expand(WN* allocregion,
                          WN* region,
		          WN* wn_sym, 
		          const SYMBOL& symbol,
                          WN** loops,
                          const INT* order,
		          INT dimcnt, 
		          BOOL invariant, 
		          BOOL finalize, 
			  BOOL has_lcd, 
		          WN* guard_tests[], 
		          BIT_VECTOR *used_loops,
		          WN** tile_loops,
		          INT* stripsizes, 
		          INT nstrips)
{
  DU_MANAGER* du = Du_Mgr; 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  REDUCTION_MANAGER* rm = red_manager; 

  FmtAssert(wn_sym == NULL || symbol == SYMBOL(wn_sym),
    ("Scalar_Expand: wn_sym and symbol not compatible")); 

  WN* outerloop = loops[0]; 
  if (nstrips > 0 && Get_Do_Loop_Info(tile_loops[0])->Depth 
      < Get_Do_Loop_Info(loops[0])->Depth)
    outerloop = tile_loops[0]; 
  FmtAssert(WN_opcode(allocregion) == OPC_DO_LOOP &&
            WN_opcode(region) == OPC_DO_LOOP &&
 	    allocregion == outerloop, 
            ("region and allocregion must be a do, or allocregion problem"));

  WN* wn_outer_loop = NULL; 
  STACK<WN*>* stk_se = NULL; 
  if (wn_sym != NULL) { 
    stk_se = Scalar_Equivalence_Class(wn_sym, du, &LNO_local_pool, outerloop);
    FmtAssert(stk_se != NULL, ("Scalar_Expand: Could not find equiv class"));
  } else { 
    stk_se = CXX_NEW(STACK<WN*>(&LNO_local_pool), &LNO_local_pool); 
    LWN_ITER* wniter = LWN_WALK_TreeIter(region);
    while (wniter) {
      WN* wn = wniter->wn;
      wniter = LWN_WALK_TreeNext(wniter);
      OPERATOR opr = WN_operator(wn);
      if ((opr == OPR_LDID || opr == OPR_STID) && symbol == SYMBOL(wn))
        stk_se->Push(wn); 
    } 
  }  

  // Derive index_loops[], the list of loops that will index the scalar
  // expansion array. 
  FmtAssert(nstrips <= dimcnt, 
    ("Can't have more scalar expanded tiles than dimensions.")); 
  unique_se_id++;
  if (LNO_Verbose) {
    WN* wn_outer = loops[0]; 
    fprintf(stdout, "Scalar expanding %s %s%sin loop %s at %d\n",
      symbol.Name(), has_lcd ? "(with lcd) " : "", 
      finalize ? "(with finalization) " : "", 
      WB_Whirl_Symbol(wn_outer), Srcpos_To_Line(WN_linenum(wn_outer)));
    fprintf(TFile, "Scalar expanding %s %s%sin loop %s at %d\n",
      symbol.Name(), has_lcd ? "(with lcd) " : "", 
      finalize ? "(with finalization) " : "", 
      WB_Whirl_Symbol(wn_outer), Srcpos_To_Line(WN_linenum(wn_outer)));
  }

  // Step 1: How much space do we need, in bytes?  Put that in bsz.
  // Also, while we are at it, store in bounds the number of elements
  // in that dimension.  Store in indxs the indexing expression.
  INT sz = 0; 
  WN* bsz = NULL;
  WN** bounds = CXX_NEW_ARRAY(WN*, dimcnt, &LNO_local_pool);
  WN** indxs = CXX_NEW_ARRAY(WN*, dimcnt, &LNO_local_pool);
  SE_Indxs_and_Bounds(loops, dimcnt, symbol, invariant, used_loops, 
    tile_loops, stripsizes, nstrips, bounds, indxs, has_lcd, &sz, &bsz); 
  WN** findxs = NULL; 
  
  // Step two:  Get the space.
  // Create the scalar expanded variable.
  char se[3]="se";
  SYMBOL se_ptrsym;
  SE_Symbols_For_SE(&se_ptrsym, se, unique_se_id, symbol.Type);
  Update_MP_Local_Var(se_ptrsym.St(), 0, LWN_Get_Parent(region));
  WN* deallocregion = guard_tests != NULL && guard_tests[0] != NULL
    ? guard_tests[0] : allocregion;
  WN* newstdf = Get_Expansion_Space(se_ptrsym, bsz, se, unique_se_id, 
    symbol.Type, allocregion, loops[0], deallocregion);
  Is_True(newstdf,("No memory was allocated for scalar expansion \n"));
  OPCODE ldop = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);

  // We've got a pointer, which points to an array.  We now must replace
  // every reference to the old symbol within the region.  Every reference is
  // replaced by the expression in index, where our array statement tells
  // us the appropriate bounds.

  // Note that when the walk is done, in theory all DO annotations for
  // this expanded variable are gone.

  // TODO OK: make more efficient by using SSA graph, so we don't have to
  // walk the whole region.  But the lexcount's important ... .  Perhaps
  // can use similar information from SNL code.

  MEM_POOL_Push(&LNO_local_pool);
  {
    DYN_ARRAY<WN_REFERENCE_INFO> deflist(&LNO_local_pool);
    DYN_ARRAY<WN_REFERENCE_INFO> uselist(&LNO_local_pool);

    WN* alias_host = NULL;
    INT lexcount = 0;

    if (finalize) { 
      findxs = CXX_NEW_ARRAY(WN*, dimcnt, &LNO_local_pool); 
      WN* wn = SE_Find_Stid(stk_se);  
      WN* wn_ldid = SE_Final_Value(wn, guard_tests, loops, dimcnt); 
      SE_Findxs(loops, dimcnt, invariant, used_loops, tile_loops, stripsizes, 
	nstrips, findxs); 
      WN* wn_array = SE_Array(se_ptrsym, sz, dimcnt, order, used_loops,
	bounds, findxs, has_lcd, -1);
      SE_Iload(wn_ldid, wn_array, newstdf, se_ptrsym, &alias_host);
      if (Block_Loop_Depth(wn_array) >= 0) { 
	INT idx = uselist.Newidx();
	uselist[idx].wn = LWN_Get_Parent(wn_array);
	uselist[idx].lexcount = 0; 
	if (!dg->Add_Vertex(LWN_Get_Parent(wn_array))) 
	  LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(wn_array), dg);
      } 
      DOLOOP_STACK do_stack(&LNO_local_pool);
      Build_Doloop_Stack(LWN_Get_Parent(wn_array), &do_stack);
      LNO_Build_Access(wn_array, &do_stack, &LNO_default_pool);
    }

    if (has_lcd) { 
      WN* wn_old_stid = SE_Find_Stid(stk_se);  
      WN* wn_stid = SE_Identity(wn_old_stid); 
      WN* wn_ldid = WN_kid0(wn_stid); 
      if (rm != NULL) 
        rm->Erase(wn_stid); 
      INT i;
      for (i = 0; i < stk_se->Elements(); i++) {
        WN* wn = stk_se->Bottom_nth(i);
	if (WN_operator(wn) == OPR_LDID) { 
	  DEF_LIST *def_list = du->Ud_Get_Def(wn);
	  if (def_list != NULL) { 
	    DEF_LIST_ITER iter(def_list);
	    const DU_NODE* node = NULL;
	    for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
	      WN* wn_local_stid = node->Wn(); 
	      if (!Wn_Is_Inside(wn_local_stid, outerloop))
	        du->Add_Def_Use(wn_local_stid, wn_ldid); 
	    }
	  }
        }
      } 
      LWN_Insert_Block_Before(LWN_Get_Parent(loops[0]), loops[0], wn_stid);  
      WN* wn_init_array = SE_Wrap_Array(se_ptrsym, sz, -1, indxs, bounds,  
        order, dimcnt, FALSE); 
      SE_Istore(wn_stid, wn_init_array, newstdf, se_ptrsym, &alias_host); 
      WN* wn_init_istore = LWN_Get_Parent(wn_init_array);
      DOLOOP_STACK do_init_stack(&LNO_local_pool);
      Build_Doloop_Stack(LWN_Get_Parent(wn_init_array), &do_init_stack);
      LNO_Build_Access(wn_init_array, &do_init_stack, &LNO_default_pool);
      if (Block_Loop_Depth(wn_init_array) >= 0) {
	INT idx = deflist.Newidx(); 
	deflist[idx].wn = wn_init_istore; 
	deflist[idx].lexcount = 0; 
	if (!dg->Add_Vertex(LWN_Get_Parent(wn_init_array)))
	  LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(wn_init_array), dg);
      } 
      for (i = dimcnt - 2; i >= 0; i--) {
	WN* wn_stid = SE_Identity(wn_old_stid); 
	LWN_Insert_Block_After(WN_do_body(loops[i]), NULL, wn_stid);  

        WN* wn_ldid = WN_kid0(wn_stid); 
	WN* wn_ldarray = SE_Wrap_Array(se_ptrsym, sz, i, indxs, 
	  bounds, order, dimcnt, TRUE); 
	SE_Iload(wn_ldid, wn_ldarray, newstdf, se_ptrsym, &alias_host); 
	WN* wn_wrap_iload = LWN_Get_Parent(wn_ldarray);
        if (Block_Loop_Depth(wn_ldarray) >= 0) { 
	  INT idx = uselist.Newidx();
	  uselist[idx].wn = wn_wrap_iload;  
	  uselist[idx].lexcount = 0;  
	  if (!dg->Add_Vertex(LWN_Get_Parent(wn_ldarray)))
	    LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(wn_ldarray), dg);
	}
	DOLOOP_STACK do_ldstack(&LNO_local_pool);
	Build_Doloop_Stack(LWN_Get_Parent(wn_ldarray), &do_ldstack);
	LNO_Build_Access(wn_ldarray, &do_ldstack, &LNO_default_pool);

	WN* wn_starray = SE_Wrap_Array(se_ptrsym, sz, i, indxs, 
	  bounds, order, dimcnt, FALSE); 
	SE_Istore(wn_stid, wn_starray, newstdf, se_ptrsym, &alias_host); 
        WN* wn_wrap_istore = LWN_Get_Parent(wn_starray);
        if (Block_Loop_Depth(wn_starray) >= 0) { 
	  INT idx = deflist.Newidx();
	  deflist[idx].wn = wn_wrap_istore;  
	  deflist[idx].lexcount = 0;  
	  if (!dg->Add_Vertex(LWN_Get_Parent(wn_starray)))
	    LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(wn_starray), dg);
        } 
        DOLOOP_STACK do_ststack(&LNO_local_pool);
	Build_Doloop_Stack(LWN_Get_Parent(wn_starray), &do_ststack);
	LNO_Build_Access(wn_starray, &do_ststack, &LNO_default_pool);
      } 
    }
 
    INT i;
    for (i = 0; i < stk_se->Elements(); i++) {  
      WN* wn = stk_se->Bottom_nth(i); 
      OPERATOR opr = WN_operator(wn);

      // Make the array statement.  We place an annotation on the resultant 
      // LOAD/STORE indicating that this is a scalar expansion.

      WN* wn_array = NULL; 
      if (opr == OPR_LDID) {
        DEF_LIST* def_list = du->Ud_Get_Def(wn);
	wn_array = SE_Array(se_ptrsym, sz, dimcnt, order, used_loops, 
	  bounds, indxs, has_lcd, def_list->Loop_stmt() == NULL 
	  ? -1 : Do_Depth(wn) - Do_Loop_Depth(outerloop));
	SE_Iload(wn, wn_array, newstdf, se_ptrsym, &alias_host); 
        INT idx = uselist.Newidx();
        uselist[idx].wn = LWN_Get_Parent(wn_array);
        uselist[idx].lexcount = 0; 
      } else {
	wn_array = SE_Array(se_ptrsym, sz, dimcnt, order, used_loops, 
	  bounds, indxs, has_lcd, -1);
	SE_Istore(wn, wn_array, newstdf, se_ptrsym, &alias_host); 
        INT idx = deflist.Newidx();
        deflist[idx].wn = LWN_Get_Parent(wn_array);
        deflist[idx].lexcount = 0; 
      }

      // Make an access vector for the wn_array and put it in the table,
      // and make a vertex for the graph.

      DOLOOP_STACK do_stack(&LNO_local_pool);
      Build_Doloop_Stack(LWN_Get_Parent(wn_array), &do_stack);
      LNO_Build_Access(wn_array, &do_stack, &LNO_default_pool);
      if (!dg->Add_Vertex(LWN_Get_Parent(wn_array))) 
        LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(wn_array), dg);
    }

    // Assign lexcounts and build dependence graph for the expanded scalar. 
    SE_Assign_Lexcounts(deflist, uselist, newstdf, deallocregion, 
      se_ptrsym); 
    SE_Fix_Dependence(deflist, uselist);

    // Eliminate temporaries. 
    for (i = 0; i < dimcnt; i++) {
      if (!used_loops || used_loops->Test(i)) {
        LWN_Delete_Tree(bounds[i]);
        LWN_Delete_Tree(indxs[i]);
	if (finalize) 
          LWN_Delete_Tree(findxs[i]);
      }
    }
    CXX_DELETE_ARRAY(bounds, &LNO_local_pool);
    CXX_DELETE_ARRAY(indxs, &LNO_local_pool);
    if (finalize) 
      CXX_DELETE_ARRAY(findxs, &LNO_local_pool);
  }

  MEM_POOL_Pop(&LNO_local_pool);
}

//-----------------------------------------------------------------------
// NAME: SE_Permutation_To_Order 
// FUNCTION: Given the unimodular transformation 'u' and the tiling trans-
//   formation 't', obtain the 'order' in which the subscripts of the sca-
//   lar expanded reference should be expanded.  We assume that there are
//   'nloops' in the SNL that we are expanding over, and that we actually 
//   have 'subloops' subscripts in the scalar expanded reference.  
//-----------------------------------------------------------------------

static void SE_Permutation_To_Order(const IMAT* u,
                                 const SNL_TILE_INFO* t,
                                 INT* order,
                                 INT nloops, INT subloops)
{
  if (u == NULL) {
    for (INT i = 0; i < nloops; i++)
      order[i] = i;
  }
  else {
    // does the right thing for permutations and unused rows, and the
    // rest doesn't much matter, so long as order is a permutation.

    INT seen_used[SNL_MAX_LOOPS];
    INT i;
    for (i = 0; i < nloops; i++)
      seen_used[i] = 0; // 0 unseen&unused, 1 seen, 2 used

    for (i = 0; i < nloops; i++) {
      INT j;
      for (j = 0; j < nloops; j++) {
        INT first_non_zero = -1;
        if ((*u)(i,j)) {
          if (seen_used[j] != 2) {
            seen_used[j] = 2;
            order[i] = j;
            break;
          }
          seen_used[j] = 1;
        }
      }
      if (j == nloops) {
        // anything will do, but there must have been something seen and
        // not used, saving pristene columns for something else.
        INT j;
        for (j = 0; j < nloops; j++) {
          if (seen_used[j] == 1) {
            seen_used[j] = 2;
            order[i] = j;
            break;
          }
        }
        FmtAssert(j != nloops, ("SE_Permutation_To_Order: impossible"));
      }
    }
  }

  Is_True(Is_Permutation_Vector(order, nloops),
    ("After applying permutation matrix, not a permutation vector."));

  if (t != NULL && t->Strips() > 0) {
    FmtAssert(t->Rectangular(), ("TODO OK: generate conservative order"));
    INT missing_tiles = nloops - t->Nloops();

    // lots of possible orders.  Move innermost stripped loop farthest out.
    INT mxiloop = 0;
    INT i;
    for (i = 0; i < t->Strips(); i++)
      if (mxiloop < t->Iloop(i))
        mxiloop = t->Iloop(i);
    INT mxptr = missing_tiles;
    for (i = 1+missing_tiles; i <= mxiloop+missing_tiles; i++)
      if (order[i] > order[mxptr])
        mxptr = i;
    if (mxptr != missing_tiles) {
      INT tmp = order[mxptr];
      order[mxptr] = order[missing_tiles];
      order[missing_tiles] = tmp;
    }
  }

  Is_True(Is_Permutation_Vector(order, nloops),
    ("After cache tiling, not a permutation vector."));

  // Finally, if subloops < loops, then we need a permutation in
  // [0 ... subloops-1], not [0 ... loops-1].

  INT neworder[SNL_MAX_LOOPS];
  INT k;
  for (k = 0; k < subloops; k++)
    neworder[k] = 0;
  for (k = 0; k < subloops; k++) {
    INT minindex = 0;
    for (INT kk = 1; kk < subloops; kk++) {
      if (order[minindex] > order[kk])
        minindex = kk;
    }
    neworder[minindex] = k;
    order[minindex] = nloops;
  }

  for (k = 0; k < subloops; k++)
    order[k] = neworder[k];

  Is_True(Is_Permutation_Vector(order, subloops),
    ("After subloop selection, not a permutation vector."));
}

//-----------------------------------------------------------------------
// NAME: SNL_GEN_Scalar_Expand
// FUNCTION: Perform unlimited tile size scalar expansion on a general 
//   SNL 'wn_outer' with 'nloops' loops.  The 'unimodular' matrix and 
//   the tile info 'ti' are used to guide the selection of subscripts. 
//   The 'plist' is a list of scalar expanded variables created by calling 
//   SX_INFO::Make_Sx_Info().  If 'ignore_illegal' is TRUE, skip over 
//   SX_PNODE::ILLEGAL scalars on the 'plist'.
//-----------------------------------------------------------------------

extern void SNL_GEN_Scalar_Expand(WN* wn_outer,
                                  IMAT* unimodular,
                                  SNL_TILE_INFO* ti,
                                  INT nloops,
                                  SX_PLIST* plist,
				  INT split_depth,
				  SD_PLIST* sd_plist, 
				  BOOL ignore_illegal,
				  BOOL full_dist)
{
  if (nloops == 0) 
    return; 

  DU_MANAGER* du = Du_Mgr;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  INT inner_depth = Do_Loop_Depth(wn_inner); 
  INT first_in_stack = inner_depth - nloops + 1; 
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);

  INT* permutation = Unimodular_To_Permutation(unimodular); 

  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT guard_depth = SE_Guard_Depth(wn_outer, permutation, nloops, plist, 
    split_depth, sd_plist, ignore_illegal, full_dist); 
  INT guard_loops = guard_depth - outer_depth + 1; 
  WN** guard_tests = guard_depth == -1 
    ? NULL : CXX_NEW_ARRAY(WN*, guard_loops, &LNO_local_pool); 
  SE_Guard_Tests(wn_outer, nloops, guard_tests, guard_depth); 

  SX_PITER ii(plist);
  SX_PNODE* nnext = NULL;
  INT outer = Do_Loop_Depth(wn_outer);

  INT* tpermutation = !full_dist ? permutation : NULL; 
  INT has_lcd = FALSE; 
  for (SX_PNODE* n = ii.First(); n; n = nnext) {
    nnext = ii.Next();

    SNL_DEBUG1(3, "SNL_General_Distribution() consider expanding %s",
               n->Symbol().Name());
    SX_PNODE::STATUS status;
    status = n->Transformable(outer, tpermutation, nloops);
    if (split_depth != -1 && status != SX_PNODE::ILLEGAL) {
      SD_PNODE* sdn = sd_plist->Find(n->Symbol());
      INT innermost_depth = sdn->Innermost_Depth();
      status = n->Splittable(split_depth, innermost_depth);
    }
    if (status == SX_PNODE::SE_NOT_REQD)
      continue;
    if (ignore_illegal && status == SX_PNODE::ILLEGAL)
      continue; 
    FmtAssert(status == SX_PNODE::SE_REQD,
              ("Bug: can't expand scalar %s", n->Symbol().Name()));
   
    INT order[LNO_MAX_DO_LOOP_DEPTH];
    WN* loops[LNO_MAX_DO_LOOP_DEPTH];

    for (INT lp = first_in_stack; lp <= Do_Loop_Depth(wn_inner); lp++)
      loops[lp-first_in_stack] = stack.Bottom_nth(lp);
    INT dimcnt = n->Expansion_Depth()+1-first_in_stack;
    SE_Permutation_To_Order(unimodular, ti, order, nloops, dimcnt);

    Scalar_Expand(stack.Bottom_nth(first_in_stack),
      stack.Bottom_nth(n->Expansion_Depth()), n->Wn_Symbol(), n->Symbol(), 
      loops, order, dimcnt, FALSE, n->Finalize(), n->Lcd_Depth() != -1,
      guard_tests);
    plist->Remove(n);
  }
}

//-----------------------------------------------------------------------
// NAME: SNL_GEN_Scalar_Expand 
// FUNCTION: Perform unlimited tile size scalar expansion on a general  
//   SNL 'wn_outer' with 'nloops' loops.  The 'permutation' is the final
//   permutation of the SNL loops and is used to guide the selection of  
//   subscripts. The 'plist' is a list of scalar expanded variables created 
//   by calling SX_INFO::Make_Sx_Info().  If 'ignore_illegal' is TRUE, skip 
//   over SX_PNODE::ILLEGAL scalars on the 'plist'.
//-----------------------------------------------------------------------

extern void SNL_GEN_Scalar_Expand(WN* wn_outer,
                                  INT permutation[],
                                  INT nloops,
                                  SX_PLIST* plist,
				  INT split_depth,
				  SD_PLIST* sd_plist, 
				  BOOL ignore_illegal,
				  BOOL full_dist)
{
  if (nloops == 0) 
    return; 

  SNL_TILE_INFO* ti = NULL;
  IMAT* unimodular = Permutation_To_Unimodular(permutation, nloops); 
  SNL_GEN_Scalar_Expand(wn_outer, unimodular, ti, nloops, plist, 
    split_depth, sd_plist, ignore_illegal, full_dist);
}

//-----------------------------------------------------------------------
// NAME: SE_Guard_Depth
// FUNCTION: For the SNL with outermost loop 'wn_outer', list of scalar 
//   expandable references 'plist', returns the depth of the innermost 
//   loop which we must guard to finalize scalar expandable variables. 
//   The 'split_depth' for the nest is 'split_depth'.  If 'ignore_illegal'
//   is TRUE, then ignore unexpandable scalars, otherwise assert.     
//-----------------------------------------------------------------------

extern INT SE_Guard_Depth(WN* wn_outer,
			  INT permutation[], 
			  INT nloops, 
                          SX_PLIST* plist,
                          INT split_depth,
			  SD_PLIST* sd_plist, 
                          BOOL ignore_illegal,
			  BOOL full_dist)
{
  SX_PITER ii(plist);
  SX_PNODE* sxnn = NULL;
  INT outer_depth = Do_Loop_Depth(wn_outer);
  INT guard_depth = -1;
  INT* tpermutation = !full_dist ? permutation : NULL; 
  for (SX_PNODE* sxn = ii.First(); !ii.Is_Empty(); sxn = sxnn) {
    sxnn = ii.Next();
    SNL_DEBUG1(3, "SE_Guard_Depth() consider expanding %s\n",
      sxn->Symbol().Name());
    SX_PNODE::STATUS status;
    status = sxn->Transformable(outer_depth, tpermutation, nloops);
    if (split_depth != -1 && status != SX_PNODE::ILLEGAL) {
      SD_PNODE* sdn = sd_plist->Find(sxn->Symbol());
      INT innermost_depth = sdn->Innermost_Depth();
      status = sxn->Splittable(split_depth, innermost_depth);
    }
    if (status == SX_PNODE::SE_NOT_REQD)
      continue;
    if (ignore_illegal && status == SX_PNODE::ILLEGAL)
      continue;
    FmtAssert(status == SX_PNODE::SE_REQD,
      (": can't expand scalar %s", sxn->Symbol().Name()));
    if (sxn->Finalize() && sxn->Expansion_Depth() > guard_depth)
      guard_depth = sxn->Expansion_Depth(); 
  }
  return guard_depth; 
}

//-----------------------------------------------------------------------
// NAME: SE_Guard_Tests
// FUNCTION: For the SNL with outermost loop 'wn_outer' consisting of 
//   'nloops' loops, place a nest of guard tests after 'wn_outer', one
//   for each loop up to and including the loop of depth 'guard_depth'. 
//   Return pointers to these guard tests in 'guard_tests'. 
//-----------------------------------------------------------------------

extern void SE_Guard_Tests(WN* wn_outer, 
                           INT nloops,
			   WN* guard_tests[],
			   INT guard_depth)
{
  if (guard_depth == -1) 
    return; 
  WN* wn_this_if = NULL; 
  INT outer_depth = Do_Loop_Depth(wn_outer);
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  COND_BOUNDS_INFO *cond_info =
       CXX_NEW(COND_BOUNDS_INFO(&LNO_local_pool), &LNO_local_pool);
  cond_info->Collect_Outer_Info(LWN_Get_Parent(wn_outer));
  INT i;
  for (i = outer_depth; i <= guard_depth; i++) {
    WN* wn_loop = stack.Bottom_nth(i); 
    WN* wn_test = LWN_Copy_Tree(WN_end(wn_loop), TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(WN_end(wn_loop), wn_test, Du_Mgr);
    Replace_Ldid_With_Exp_Copy(SYMBOL(WN_start(wn_loop)), wn_test,
      WN_kid0(WN_start(wn_loop)), Du_Mgr);
    if (wn_this_if != NULL
        && Redundant_Condition(cond_info, wn_test, wn_this_if)) {
      guard_tests[i - outer_depth] = wn_this_if; 
      LWN_Delete_Tree(wn_test); 
      continue;
    } 
    WN* wn_new_then = WN_CreateBlock();
    WN* wn_new_else = WN_CreateBlock();
    WN* wn_new_if = LWN_CreateIf(wn_test, wn_new_then, wn_new_else);
    IF_INFO* ii = CXX_NEW(IF_INFO(&LNO_default_pool, FALSE, FALSE),
      &LNO_default_pool);
    WN_MAP_Set(LNO_Info_Map, wn_new_if, (void *) ii);
    if (wn_this_if == NULL) {
      LWN_Insert_Block_After(LWN_Get_Parent(wn_outer), wn_outer, wn_new_if);
    } else {
      WN* wn_block = WN_then(wn_this_if);
      LWN_Insert_Block_After(wn_block, NULL, wn_new_if);
    }
    DOLOOP_STACK if_stack(&LNO_local_pool);
    Build_Doloop_Stack(wn_new_if, &if_stack);
    LNO_Build_If_Access(wn_new_if, &if_stack);
    guard_tests[i - outer_depth] = wn_new_if; 
    wn_this_if = wn_new_if;
  }
  for (i = guard_depth + 1; i < nloops; i++) 
    guard_tests[i] = NULL; 
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Scalar_Expand 
// FUNCTION: Perform unlimited tile size scalar expansion on an invariant 
//   SNL 'wn_outer' with 'nloops' loops.  The 'permutation' is the final
//   permutation of the SNL loops and is used to guide the selection of  
//   subscripts. The 'plist' is a list of scalar expanded variables created 
//   by calling SX_INFO::Make_Sx_Info().  If 'ignore_illegal' is TRUE, skip 
//   over SX_PNODE::ILLEGAL scalars on the 'plist'.
//-----------------------------------------------------------------------

extern void SNL_INV_Scalar_Expand(WN* wn_outer,
                                  INT permutation[],
                                  INT nloops,
                                  SX_PLIST* plist,
				  INT split_depth,
				  SD_PLIST* sd_plist, 
				  BOOL ignore_illegal,
				  BOOL full_dist)
{
  if (nloops == 0) 
    return; 

  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT guard_depth = SE_Guard_Depth(wn_outer, permutation, nloops, plist, 
    split_depth, sd_plist, ignore_illegal, full_dist); 
  INT guard_loops = guard_depth - outer_depth + 1; 
  WN** guard_tests = guard_depth == -1 
    ? NULL : CXX_NEW_ARRAY(WN*, guard_loops, &LNO_local_pool); 
  SE_Guard_Tests(wn_outer, nloops, guard_tests, guard_depth); 

  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  INT first_in_stack = Do_Loop_Depth(wn_inner) - nloops + 1;
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  ARRAY_DIRECTED_GRAPH16*       dg = Array_Dependence_Graph;
  DU_MANAGER*                   du = Du_Mgr;
  SX_PITER ii(plist);
  SX_PNODE* nnext = NULL;
  INT outer = Do_Loop_Depth(wn_outer);

  BOOL has_lcd = FALSE; 
  INT* tpermutation = !full_dist ? permutation : NULL; 
  for (SX_PNODE* n = ii.First(); !ii.Is_Empty(); n = nnext) {
    nnext = ii.Next();

    SNL_DEBUG1(3, "SNL_INV_Scalar_Expand() consider expanding %s\n",
               n->Symbol().Name());

    SX_PNODE::STATUS status;
    status = n->Transformable(outer, tpermutation, nloops);
    if (split_depth != -1 && status != SX_PNODE::ILLEGAL) {
      SD_PNODE* sdn = sd_plist->Find(n->Symbol()); 
      INT innermost_depth = sdn->Innermost_Depth(); 
      status = n->Splittable(split_depth, innermost_depth); 
    } 
    if (status == SX_PNODE::SE_NOT_REQD)
      continue;
    if (ignore_illegal && status == SX_PNODE::ILLEGAL)
      continue; 
    FmtAssert(status == SX_PNODE::SE_REQD,
              ("Bug: can't expand scalar %s", n->Symbol().Name()));

    // The purpose of the variable 'order' is as a mini-permutation matrix.
    // Scalar_Expand is told the future order of the loops, so that
    // it can choose a sensible stride one loop to expand into.

    WN* loops[SNL_MAX_LOOPS];
    INT order[SNL_MAX_LOOPS];

    INT lp;
    for (lp = 0; lp <= n->Expansion_Depth()-first_in_stack; lp++) {
      loops[lp] = stack.Bottom_nth(first_in_stack+lp);
      order[lp] = lp;
    }

    if (permutation) {
      for (INT i = 0; i < lp; i++) {
        INT jsmall = -1;
        for (INT j = 0; j < lp; j++) {
          BOOL ok = TRUE;
          for (INT ii = 0; ii < i; ii++)
            if (order[ii] == j)
              ok = FALSE;
          if (ok && (jsmall == -1 || permutation[jsmall] > permutation[j]))
            jsmall = j;
        }
        order[i] = jsmall;
      }
    }

    Scalar_Expand(stack.Bottom_nth(first_in_stack),
      stack.Bottom_nth(n->Expansion_Depth()), n->Wn_Symbol(), n->Symbol(), 
      loops, order, n->Expansion_Depth()+1-first_in_stack, TRUE, 
      n->Finalize(), n->Lcd_Depth() != -1, guard_tests); 
    plist->Remove(n);
  }
}

//-----------------------------------------------------------------------
// NAME: SNL_Scalar_Expand 
// FUNCTION: For the SNL with outermost loop 'wn_outer' and innermost loop
//   'wn_inner' to which we are applying the 'permutation' of length 'nloops',
//   scalar expand the required variables on the 'plist'.  If 'invariant'
//   is TRUE, use invariant scalar expansion, otherwise use general scalar 
//   expansion.  If 'ignore_illegal' is TRUE, skip over SX_PNODE::ILLEGAL 
//   scalars on the 'plist'.
//-----------------------------------------------------------------------

extern void SNL_Scalar_Expand(WN* wn_outer, 
			      WN* wn_inner, 
			      INT permutation[], 
			      INT nloops, 
			      SX_INFO* sx_info,
			      BOOL invariant,
			      BOOL ignore_illegal,
			      BOOL full_dist) 
{
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT i;
  for (i = 0; i < nloops; i++)
    if (permutation[i] != i) 
      break;
  if (i == nloops)
    return; 
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  WN* wn_se_outer = stack.Bottom_nth(outer_depth + i); 
  INT se_outer_depth = Do_Loop_Depth(wn_se_outer); 
  INT nnloops = nloops - (se_outer_depth - outer_depth);
  INT* ppermutation = CXX_NEW_ARRAY(INT, nnloops, &LNO_local_pool); 
  for (i = 0; i < nnloops; i++) 
    ppermutation[i] = permutation[nloops - nnloops + i] - (nloops - nnloops);  
  if (invariant) 
    SNL_INV_Scalar_Expand(wn_se_outer, ppermutation, nnloops, 
      &sx_info->Plist, -1, NULL, ignore_illegal, full_dist); 
  else 
    SNL_GEN_Scalar_Expand(wn_se_outer, ppermutation, nnloops, 
      &sx_info->Plist, -1, NULL, ignore_illegal, full_dist); 
}

//-----------------------------------------------------------------------
// NAME: Split_Sx_Depth
// FUNCTION: Return the depth of the outermost loop which for which we
//   must apply scalar expansion because the SNL with outermost loop 
//   'wn_outer' containing 'nloops' loops whose scalars are described by 
//   'plist' must be split into possibly three distributends: one above 
//   and below the loop at depth 'split_depth' and the main loop.
//-----------------------------------------------------------------------

extern INT Split_Sx_Depth(WN* wn_outer,
                          INT nloops,
                          SX_PLIST* plist,
                          INT split_depth)
{
  INT outer_depth = Do_Loop_Depth(wn_outer);
  if (split_depth <= outer_depth) 
    return -1; 
  INT split_sx_depth = outer_depth + nloops;
  if (plist == NULL) 
    return split_sx_depth; 
  SX_CONST_PITER ii(plist);
  for (const SX_PNODE* n = ii.First(); !ii.Is_Empty(); n = ii.Next()) {
    if (n->Outer_Se_Reqd() == n->Outer_Se_Not_Reqd())
      continue; 
    if (n->Outer_Se_Reqd() >= split_depth)
      continue;     
    if (n->Outer_Se_Reqd() < split_sx_depth)
      split_sx_depth = n->Outer_Se_Reqd(); 
  }
  return split_sx_depth;
}
     
//-----------------------------------------------------------------------
// NAME: SNL_Scalar_Expand_For_Splitting 
// FUNCTION: For the SNL with outermost loop 'wn_outer' and innermost loop 
//   'wn_inner' to which we are applying the 'permutation' of length 'nloops' 
//   scalar expand the required variables in 'plist' so that the kernel of 
//   loops from depth 'split_depth' inward can be distributed into its own
//   distributend. If 'ignore_illegal' is TRUE, skip over SX_PNODE::ILLEGAL 
//   scalars on the 'plist'. 
//
//   Returns a BOOL indicating whether a scalar expanded variable
//   had LCDs.
//-----------------------------------------------------------------------

extern void SNL_Scalar_Expand_For_Splitting(WN* wn_outer, 
				            WN* wn_inner, 
				            INT split_depth, 
				            SX_PLIST* plist, 
					    SD_PLIST* sd_plist, 
				            BOOL invariant,
					    BOOL ignore_illegal,
					    BOOL full_dist)
{
  if (split_depth <= 0) 
    return; 
  INT outer_depth = Do_Loop_Depth(wn_outer);
  INT inner_depth = Do_Loop_Depth(wn_inner); 
  INT nloops = inner_depth - outer_depth + 1;
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack);
  INT* permutation = CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool);  
  INT i;
  for (i = 0; i < nloops; i++) 
    permutation[i] = i; 
  INT souter_depth = Split_Sx_Depth(wn_outer, nloops, plist, split_depth);
  if (souter_depth == outer_depth + nloops)
    return;
  INT snloops = nloops + outer_depth - souter_depth; 
  INT* spermutation = CXX_NEW_ARRAY(INT, snloops, &LNO_local_pool);
  for (i = 0; i < snloops; i++) 
    spermutation[i] = permutation[i + nloops - snloops] - nloops + snloops; 
  Is_True(Is_Permutation_Vector(spermutation, snloops), 
    ("Bad permutation vector in SNL_Split_Scalar_Expand"));  
  WN* wn_souter = stack.Bottom_nth(outer_depth + nloops - snloops); 
  if (invariant) 
    SNL_INV_Scalar_Expand(wn_souter, spermutation, snloops, plist, 
      split_depth, sd_plist, ignore_illegal, full_dist);
  else 
    SNL_GEN_Scalar_Expand(wn_souter, spermutation, snloops, plist, 
      split_depth, sd_plist, ignore_illegal, full_dist);
}  
   

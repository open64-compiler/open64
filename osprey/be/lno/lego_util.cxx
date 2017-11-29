/*
 * Copyright (C) 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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
// ====================================================================
// ====================================================================
//
// Module: lego_util.cxx
//
// Revision history:
//  dd-mmm-95 - Original Version
//
// Description:
//    
//     Utility routines for lego.
// 
// ====================================================================
// ====================================================================

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

const static char *source_file = __FILE__;

#include <alloca.h>
#include <sys/types.h>
#include <ctype.h>
#include <limits.h>

#include "pu_info.h"
#include "lnopt_main.h"
#include "errors.h"
#include "lego_util.h"
#include "lego_pragma.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "lego_opts.h"
#include "strtab.h"
#include "const.h"
#include "targ_const.h"
#include "tile.h"
#include "targ_sim.h"

extern VAR_KIND ST_Var_Kind (ST* array_st);
extern WN* Get_Numthreads_Ldid (LEGO_INFO* li);
extern WN* Get_Runtime_Numthreads_Ldid ();
extern mBOOL Single_Loop_Coeff(ACCESS_VECTOR *av, INT64 *stride, INT64 *offset,
                               mINT32 *depth);


/***********************************************************************
 *
 * Return TRUE if ST is global'ish, FALSE otherwise
 *
 ***********************************************************************/
VAR_KIND ST_Var_Kind (ST* array_st) {
  VAR_KIND retval;

#ifdef _NEW_SYMTAB
  switch (ST_sclass(array_st)) {
    /******** locals ********/
  case SCLASS_PSTATIC:  /* Statically-allocated data with PU scope */
  case SCLASS_AUTO:     /* Local stack user variable */
    retval = var_local;
    break;
  case SCLASS_FORMAL:   /* Formal parameter */
    retval = var_formal;
    break;

    /******** globals ********/
  case SCLASS_FSTATIC:  /* Statically-allocated data with file scope,
                         * i.e. Fortran SAVE, C static */
  case SCLASS_EXTERN:   /* Unallocated C external data or text */
  case SCLASS_UGLOBAL:  /* Uninitialized C global data: provisionally
                         * allocated but allocation may be preempted
                         * by another module -- equivalent to Fortran
                         * uninitialized COMMON */
  case SCLASS_DGLOBAL:  /* Defined (initialized) C global data:
                         * allocated in this module
                         */
    retval = var_global;
    break;

  /* Fortran commmon.
   * (or possibly local, if it was an assumed size array allocated
   * using alloca).
   */
  case SCLASS_COMMON:    /* Common */
    /* must be based, don't call on the base common itself */
    FmtAssert (ST_base(array_st) != array_st,
               ("SCLASS of array (%s) is a COMMON", ST_name(array_st)));
    Is_True (ST_sclass(ST_base(array_st)) == SCLASS_COMMON,
             ("ST (%s) is BASED, but base is not a COMMON or AUTO\n",
              ST_name(array_st)));
    retval = var_common;
    break;

  /*** illegal values */
  case SCLASS_TEXT:     /* Executable code */
  case SCLASS_REG:      /* Register variable (PREG) */
  default:
    FmtAssert (FALSE, ("Unexpected SCLASS (%d) of distributed array\n",
                       ST_sclass(array_st)));
    break;
  }
  Is_True (((retval != var_global) && (retval != var_common)) ||
           (ST_level(array_st) == GLOBAL_SYMTAB),
           ("ST %s: is in Global_Symtab, but we don't think it is global\n",
            ST_name(array_st)));

#else
  switch (ST_sclass(array_st)) {
    /******** locals ********/
  case SCLASS_PSTATIC:  /* Statically-allocated data with PU scope */
  case SCLASS_AUTO:     /* Local stack user variable */
    retval = var_local;
    break;
  case SCLASS_FORMAL:   /* Formal parameter */
    retval = var_formal;
    break;

    /******** globals ********/
  case SCLASS_FSTATIC:  /* Statically-allocated data with file scope,
                         * i.e. Fortran SAVE, C static */
  case SCLASS_EXTERN:   /* Unallocated C external data or text */
  case SCLASS_UGLOBAL:  /* Uninitialized C global data: provisionally
                         * allocated but allocation may be preempted
                         * by another module -- equivalent to Fortran
                         * uninitialized COMMON */
  case SCLASS_DGLOBAL:  /* Defined (initialized) C global data:
                         * allocated in this module
                         */
    retval = var_global;
    break;

  /* Fortran commmon.
   * (or possibly local, if it was an assumed size array allocated
   * using alloca).
   */
  case SCLASS_BASED:    /* Data pointed to by another datum */
    if (ST_sclass(ST_base(array_st)) == SCLASS_COMMON)
      retval = var_common;
    else if (ST_sclass(ST_base(array_st)) == SCLASS_AUTO)
      retval = var_local;
    else 
      FmtAssert(FALSE, ("ST (%s) is BASED, but base is not a COMMON or AUTO\n",
                        ST_name(array_st)));
    break;
  case SCLASS_COMMON:   /* Fortran common block */
    FmtAssert (FALSE, ("SCLASS of array (%s) is a COMMON", ST_name(array_st)));
    break;

  /*** illegal values */
  case SCLASS_TEXT:     /* Executable code */
  case SCLASS_THREAD:   /* Fortran per-thread local data block */
  case SCLASS_REG:      /* Register variable (PREG) */
  default:
    FmtAssert (FALSE, ("Unexpected SCLASS (%d) of distributed array\n",
                       ST_sclass(array_st)));
    break;
  }
  Is_True ((retval != var_global) || (ST_is_global(array_st)),
           ("ST %s: is in Global_Symtab, but we don't think it is global\n",
            ST_name(array_st)));
#endif

  return retval;
}

/***********************************************************************
 *
 * Description: Given an ST return the ST_type it originally had.
 *      Same as ST_type, except when called on reshaped globals 
 *      whose type has been mangled.
 *
 ***********************************************************************/

extern TY_IDX Lego_Get_Original_Type (ST* st) {

  TY_IDX ty;

  if (ST_class(st) != CLASS_VAR) return ST_type(st);

#ifdef _NEW_SYMTAB
  if (ST_level(st) == GLOBAL_SYMTAB && ST_is_reshaped(st)) {
#else
  if (ST_is_global(st) && ST_is_reshaped(st)) {
#endif

    DISTR_GLOBAL_INFO* dgi = da_global->Find(st);

    if (dgi) {
      // has been seen before
      ty = dgi->Get_TY();
    }
    else {
      // seeing it for the first time

      ty = (TY_IDX) NULL;
      
      // If running IPA, and compiling a 1.I file (i.e. not symtab.G file)
      // try to find it in the pragma-block of Current_Func_Node
      // (for C/C++ it is there, not for Fortrans).
      if (FILE_INFO_ipa(File_info) && Read_Global_Data) {
        WN *pwn = WN_first(WN_func_pragmas(Current_Func_Node));
        while (pwn) {
          if (WN_operator(pwn) == OPR_PRAGMA &&
              WN_pragma(pwn) == WN_PRAGMA_TYPE_OF_RESHAPED_ARRAY &&
              WN_st(pwn) == st) {
            ty = WN_pragma_arg1(pwn);
            break;
          }
          pwn = WN_next(pwn);
        }
      }

      if (ty == (TY_IDX) NULL) {
        ty = ST_type(st);
      }
      DISTR_GLOBAL_INFO* dgi = CXX_NEW (DISTR_GLOBAL_INFO(ty),
                                        Malloc_Mem_Pool);
      da_global->Enter (st, dgi);
    }
  }
  else {
    ty = ST_type(st);
  }
  return ty;
}

/***********************************************************************
 * 
 * Given an array_st, return the appropriate type for the array.
 *
 ***********************************************************************/
extern TY_IDX Lego_Get_Array_Type (ST* st) {
  TY_IDX ty;

  ty = Lego_Get_Original_Type(st);

  if ((TY_kind(ty) == KIND_POINTER) && (ST_isFormal(st) || ST_isLocal(st))) {
    ty = TY_pointed(ty);
  }

  return ty;
}


/***********************************************************************
 * 
 * Find a unique DACT for the given array_st.  If a pragma WN* is given
 * return the DACT that corresponds to that pragma.  If no unique
 * DACT can be found, then return NULL
 *
 ***********************************************************************/
DISTR_ARRAY *Lookup_DACT(ST *array_st) {

  DISTR_INFO* dinfo = da_hash->Find (array_st);
  if (!dinfo) return NULL;
  Is_True (dinfo->Num_Dact() == 1,
           ("Array %s has %d distributions\n",
            ST_name(array_st), dinfo->Num_Dact()));
  return dinfo->Get_Dact(0);
}

/***********************************************************************
 *
 * Given a lego-info, figure out which distributed dimension
 * is the tiling for, and return an ldid of the numprocs in that
 * dimension. Called only from Lego_Tile_Loop in snl_trans.cxx.
 * 
 ***********************************************************************/
extern WN* Get_Numthreads_Ldid (LEGO_INFO* li) {
  ST *array_st = (li->Array())->St();
  Is_True (array_st, ("Lego_Info has no distributed array"));
  DISTR_INFO* dinfo = da_hash->Find(array_st);
  Is_True (dinfo, ("Array %s has no dinfo", ST_name(array_st)));
  return dinfo->Numprocs(li->Dim_Num());
}


/***********************************************************************
 *
 * Return an ldid of the runtime __mp_sug_numthreads variable.
 * 
 ***********************************************************************/
extern WN* Get_Runtime_Numthreads_Ldid () {
#ifdef KEY
  if (LNO_Num_Processors != 0)
    return LWN_Make_Icon(MTYPE_I4, LNO_Num_Processors);
#endif
  OPCODE ldid_op = OPCODE_make_op(OPR_LDID, MTYPE_I4, MTYPE_I4);
  WN* ldid_wn = WN_CreateLdid (ldid_op, 0, distr_st_entries[mp_sug_numthreads],
                               Be_Type_Tbl(MTYPE_I4));
  Create_global_alias (Alias_Mgr, distr_st_entries[mp_sug_numthreads],
                       ldid_wn, NULL);
  Du_Mgr->Add_Def_Use (Current_Func_Node, ldid_wn);
  return ldid_wn;
}

/***********************************************************************
 *
 * Return an ldid of the return value of the call to __mp_numthreads().
 * Insert generated code before prev_wn, assuming prev_wn is a kid of
 * a BLOCK node.
 * 
 ***********************************************************************/
extern WN* Get_Runtime_Numthreads_Ldid_From_Func (WN* prev_wn) {
  // generate call to __mp_numthreads(), return an LDID of return-value
  OPCODE callop = OPCODE_make_op(OPR_CALL, MTYPE_I4, MTYPE_V);
  WN* call_wn = WN_Create(callop, 0);
  WN_st_idx(call_wn) = ST_st_idx(distr_st_entries[mp_numthreads_fn]);
  Set_Runtime_Call_Side_Effects (call_wn);
  LWN_Insert_Block_Before (NULL, prev_wn, call_wn);
  // return values
  PREG_NUM rreg1, rreg2;
  ST* rst = Find_Return_Registers (MTYPE_I4, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad MTYPE_I4 return regs"));

  WN* ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID, MTYPE_I4, MTYPE_I4),
                               rreg1, rst, Be_Type_Tbl(MTYPE_I4));
  Create_alias (Alias_Mgr, ldid_wn);
  Du_Mgr->Add_Def_Use (call_wn, ldid_wn);
  return ldid_wn;
}

/***********************************************************************
 *
 * Return an ldid of the runtime __mp_cur_numthreads variable.
 * 
 ***********************************************************************/
extern WN* Get_Runtime_Cur_Numthreads_Ldid () {
  OPCODE ldid_op = OPCODE_make_op(OPR_LDID, MTYPE_I4, MTYPE_I4);
  WN* ldid_wn = WN_CreateLdid (ldid_op, 0, distr_st_entries[mp_cur_numthreads],
                               Be_Type_Tbl(MTYPE_I4));
  Create_global_alias (Alias_Mgr, distr_st_entries[mp_cur_numthreads],
                       ldid_wn, NULL);
  Du_Mgr->Add_Def_Use (Current_Func_Node, ldid_wn);
  return ldid_wn;
}


/***********************************************************************
 * 
 * Utility routines for code creation and manipulation
 *
 ***********************************************************************/

// Compute a fast div operator using the algorithm
//  d_j = (1. + pow_50)/( (double) j);
//  return ( (int) ( ((double) i)*d_j) );
static WN *Fast_32_Div(TYPE_ID rtype, WN *kid0, WN *kid1, BOOL can_speculate)
{
  WN *cvti = LWN_CreateExp1(OPCODE_make_op(OPR_CVT,MTYPE_F8,rtype),kid0);
  WN *cvtj = LWN_CreateExp1(OPCODE_make_op(OPR_CVT,MTYPE_F8,rtype),kid1);
  WN *eps = Make_Const (Host_To_Targ_Float (MTYPE_F8, 1.000000000000001));
  WN *div = LWN_CreateExp2(OPC_F8DIV,eps,cvtj);
  if (can_speculate) WN_MAP_Set(Safe_Spec_Map, div, (void*) 1);
  WN *mpy = LWN_CreateExp2(OPC_F8MPY,cvti,div);
  WN *trunc = LWN_CreateExp1(OPCODE_make_op(OPR_TRUNC,rtype,MTYPE_F8),mpy);
  return trunc;
}


// Compute a fast rem operator using the algorithm
//       d_j     = j + 0.d0
//       recip_j = (1.d0 + 2.**(-50))/d_j
//       mod     = i - int(i*recip_j)*d_j
static WN *Fast_32_Rem(TYPE_ID rtype, WN *kid0, WN *kid1, BOOL can_speculate)
{
  WN *cvtj = LWN_CreateExp1(OPCODE_make_op(OPR_CVT,MTYPE_F8,rtype),kid1);
  WN *newcvtj = LWN_Copy_Tree(cvtj,TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(cvtj, newcvtj, Du_Mgr);
  WN *eps = Make_Const (Host_To_Targ_Float (MTYPE_F8, 1.000000000000001));
  WN *div = LWN_CreateExp2(OPC_F8DIV,eps,cvtj);
  if (can_speculate) WN_MAP_Set(Safe_Spec_Map, div, (void*) 1);
  WN *cvti = LWN_CreateExp1(OPCODE_make_op(OPR_CVT,MTYPE_F8,rtype),kid0);
  WN *newcvti = LWN_Copy_Tree(cvti,TRUE,LNO_Info_Map);
  LWN_Copy_Def_Use(cvti, newcvti, Du_Mgr);
  WN *mpy = LWN_CreateExp2(OPC_F8MPY,cvti,div);
  WN *trunc = LWN_CreateExp1(OPCODE_make_op(OPR_TRUNC,rtype,MTYPE_F8),mpy);
  WN *cvt = LWN_CreateExp1(OPCODE_make_op(OPR_CVT,MTYPE_F8,rtype),trunc);
  mpy = LWN_CreateExp2(OPC_F8MPY,cvt,newcvtj);
  WN *sub = LWN_CreateExp2(OPC_F8SUB,newcvti,mpy);
  return LWN_CreateExp1(OPCODE_make_op(OPR_TRUNC,rtype,MTYPE_F8),sub);
}

WN*
AWN_Binary(OPERATOR opr,
           TYPE_ID rtype,
           WN *kid0,
           WN *kid1,
           BOOL can_speculate)
{
  if (!Get_Trace(TP_LNOPT2, TT_LEGO_DISABLE_FP_DIVMOD)) {
    if (opr == OPR_REM) {
      TYPE_ID r0type = WN_rtype(kid0);
      TYPE_ID r1type = WN_rtype(kid1);
      if (((rtype == MTYPE_I4) || (rtype == MTYPE_U4)) &&
          ((r1type == MTYPE_I4) || (r1type == MTYPE_U4)) &&
          ((r1type == MTYPE_I4) || (r1type == MTYPE_U4))) {
        return Fast_32_Rem(rtype, kid0, kid1, can_speculate);
      }
    } else if (opr == OPR_DIV) {
      TYPE_ID r0type = WN_rtype(kid0);
      TYPE_ID r1type = WN_rtype(kid1);
      if (((rtype == MTYPE_I4) || (rtype == MTYPE_U4)) &&
          ((r1type == MTYPE_I4) || (r1type == MTYPE_U4)) &&
          ((r1type == MTYPE_I4) || (r1type == MTYPE_U4))) {
        return Fast_32_Div(rtype,kid0,kid1,can_speculate);
      }
    }
  }
  OPCODE op = OPCODE_make_op(opr, rtype, MTYPE_V);
  WN* ret_wn = LWN_CreateExp2(op, kid0, kid1);
  if ((opr == OPR_REM || opr == OPR_DIV || opr == OPR_MOD) && can_speculate)
    WN_MAP_Set(Safe_Spec_Map, ret_wn, (void*) 1);
  return ret_wn;
}

 
WN*
AWN_LdidSym(SYMBOL *var)
{
   Is_True(var && ((ST_class(var->St()) == CLASS_PREG) || 
		   (ST_class(var->St()) == CLASS_VAR)),
     ("AWN_LdidSym called with SYMBOL that is not a preg or a variable\n"));

   TYPE_ID desc = var->Type;
   WN *the_ldid = WN_RLdid(Promote_Type(desc), desc, var->WN_Offset(),
                           var->St(), ST_type(var->St()));
   Create_alias(Alias_Mgr, the_ldid);
   
   return (the_ldid);
}

 
WN*
AWN_StidIntoSym(SYMBOL *var, 
                WN *val)
{
  Is_True(var && ((ST_class(var->St()) == CLASS_PREG) || 
	          (ST_class(var->St()) == CLASS_VAR)), 
    ("AWN_StidIntoSym called with SYMBOL that is not a preg or a variable\n"));

  TYPE_ID desc = var->Type;
  OPCODE op = OPCODE_make_op(OPR_STID, MTYPE_V, desc);
  WN *the_stid = LWN_CreateStid(op, var->WN_Offset(), var->St(),
				ST_type(var->St()), val);
  Create_alias(Alias_Mgr, the_stid);

  return (the_stid);
}


/*
 * Create_Positive_Divceil 
 *
 * divceil(a,b), a,b > 0 is equivalent to (a + b - 1) / b.  Assuming that
 * kid0,kid1 > 0, generate an expression tree for (kid0 + kid1 - 1) / kid1.
 * Both kid0 and kid1 are inserted into the new whirl node.
 *
 */
WN*
Create_Positive_Divceil(TYPE_ID type,
                        WN *kid0,
                        WN *kid1,
                        BOOL can_speculate)
{
   WN *kid1_copy = LWN_Copy_Tree(kid1);
   LWN_Copy_Def_Use(kid1, kid1_copy, Du_Mgr);

   WN *sub = AWN_Sub(type, kid1_copy, LWN_Make_Icon(type, 1));
   WN *add = AWN_Add(type, kid0, sub);

   WN* div;
   if (can_speculate) div = AWN_Div_Safe(type, add, kid1);
   else div = AWN_Div(type, add, kid1);
   return (div);
}


void 
Set_Array_Dim(WN *array_expr,
	      INT dim,
	      WN *bound,
	      WN *index)
{
   INT num_dim = WN_num_dim(array_expr);
   Is_True((dim >= 0) && (dim < num_dim), 
     ("Dim %d out of range 0..%d\n", dim, num_dim - 1));

   WN_array_dim(array_expr, dim) = bound;
   WN_array_index(array_expr, dim) = index;
   LWN_Set_Parent(bound, array_expr);
   LWN_Set_Parent(index, array_expr);
}


/*
 * Replace old_wn with new_wn in old_wn's parent.
 * Do not delete old_wn
 */
void 
Replace_WN(WN *old_wn,
	   WN *new_wn)
{
  WN *parent = LWN_Get_Parent(old_wn);

  Is_True (parent, ("Replace_WN: expected a parent node"));

  if (WN_opcode(parent) != OPC_BLOCK) {
    INT kidno;

    for (kidno = 0; kidno < WN_kid_count(parent); kidno++) {
      if (WN_kid(parent, kidno) == old_wn)
        break;
    }
    Is_True(kidno < WN_kid_count(parent), ("Bad parent pointer\n")); 

    WN_kid(parent, kidno) = new_wn;
  }
  else {
    WN_prev(new_wn) = WN_prev(old_wn);
    WN_next(new_wn) = WN_next(old_wn);

    if (WN_prev(new_wn)) WN_next(WN_prev(new_wn)) = new_wn;
    else WN_first(parent) = new_wn;
    if (WN_next(new_wn)) WN_prev(WN_next(new_wn)) = new_wn;
    else WN_last(parent) = new_wn;
  }
  LWN_Set_Parent(new_wn, parent);
  LWN_Set_Parent(old_wn, NULL);
}


/***********************************************************************
 *
 * Given a call WHIRL node to a libMP runtime routine, 
 * set the side-effect bits.
 *
 ***********************************************************************/
extern void Set_Runtime_Call_Side_Effects (WN* call_wn) {
  INT i;
  Is_True (call_wn && (WN_operator(call_wn) == OPR_CALL 
           || WN_operator(call_wn) == OPR_INTRINSIC_CALL),
           ("Set_Runtime_Call_Side_Effects: expected a call node\n"));
  ST* call_st = WN_st(call_wn);
  for (i=0; i<DST_MAX; i++) {
    if (distr_st_entries[i] == call_st) break;
  }
  Is_True (i<DST_MAX,
           ("Set_Runtime_Call_Side_Effects: Could not find call-st\n"));
  switch (i) {
  case HT_Push:
    /* args are basically value parameters */
    WN_Set_Call_Non_Data_Mod (call_wn);
    WN_Set_Call_Non_Data_Ref (call_wn);
    break;
  case HT_Pop:
    WN_Set_Call_Non_Data_Mod (call_wn);
    WN_Set_Call_Non_Data_Ref (call_wn);
    WN_Set_Call_Non_Parm_Mod (call_wn); /* frees dart, visible to compiler */
    WN_Set_Call_Non_Parm_Ref (call_wn);
    break;
  case HT_Top:
  case HT_Check:
    WN_Set_Call_Non_Data_Ref (call_wn);
    WN_Set_Call_Non_Parm_Ref (call_wn);
    WN_Set_Call_Parm_Ref (call_wn);
    break;
  case HT_Replace:
    WN_Set_Call_Non_Data_Mod (call_wn);
    WN_Set_Call_Non_Data_Ref (call_wn);
    WN_Set_Call_Non_Parm_Mod (call_wn);
    WN_Set_Call_Non_Parm_Ref (call_wn);
    WN_Set_Call_Parm_Ref (call_wn);
    break;
  case Initialize_Dart:
    WN_Set_Call_Parm_Mod (call_wn);
    WN_Set_Call_Parm_Ref (call_wn);
    break;
  case Allocate_Dart:
    WN_Set_Call_Non_Data_Mod (call_wn);
    WN_Set_Call_Non_Data_Ref (call_wn);
    break;
  case Alloc_Reshape:
    WN_Set_Call_Non_Data_Mod (call_wn);
    WN_Set_Call_Non_Data_Ref (call_wn);
    WN_Set_Call_Parm_Ref (call_wn);
    break;
  case Dealloc_Reshape:
    WN_Set_Call_Non_Data_Mod (call_wn);
    WN_Set_Call_Non_Data_Ref (call_wn);
    WN_Set_Call_Parm_Mod (call_wn);
    break;
  case Migrate_Array:
    WN_Set_Call_Non_Data_Mod (call_wn);
    WN_Set_Call_Non_Data_Ref (call_wn);
    WN_Set_Call_Parm_Ref (call_wn);
    break;
  case Unmigrate_Array:
    WN_Set_Call_Non_Data_Mod (call_wn);
    WN_Set_Call_Non_Data_Ref (call_wn);
    WN_Set_Call_Parm_Ref (call_wn);
    break;
  case Migrate_Pages:
    WN_Set_Call_Non_Data_Mod (call_wn);
    WN_Set_Call_Non_Data_Ref (call_wn);
    WN_Set_Call_Parm_Ref (call_wn);
    break;
  case mp_my_threadnum:
    WN_Set_Call_Non_Data_Ref (call_wn);
    break;
  case mp_numthreads_fn:
    WN_Set_Call_Non_Data_Ref (call_wn);
    break;
  case Proc_Pool_Push:
    WN_Set_Call_Non_Data_Mod (call_wn);
    WN_Set_Call_Non_Data_Ref (call_wn);
    break;
  case Proc_Pool_Pop:
    WN_Set_Call_Non_Data_Mod (call_wn);
    WN_Set_Call_Non_Data_Ref (call_wn);
    break;
  case Cyclic_Bounds:
    WN_Set_Call_Parm_Mod (call_wn);
    WN_Set_Call_Parm_Ref (call_wn);
    break;
  case Deallocate_Dart:
    WN_Set_Call_Non_Data_Mod (call_wn);
    WN_Set_Call_Non_Data_Ref (call_wn);
    WN_Set_Call_Parm_Mod (call_wn);
    WN_Set_Call_Parm_Ref (call_wn);
    break;
  case Compare_Darts:
    WN_Set_Call_Parm_Ref (call_wn);
    break;
#ifndef KEY // Pathscale does not have this
  case mp_cur_numthreads_func:
    break;
#endif
  case DST_MAX:
  default:
    FmtAssert (FALSE, ("Set_Runtime_Call_Side_Effects: switch failed\n"));
    break;
  }
}

//-----------------------------------------------------------------------
// NAME: Lego_Find_Node
// FUNCTION: Find the first node the the tree rooted at 'wn_tree' with
//   symbol 'sym' and return it.
//-----------------------------------------------------------------------

extern WN* Lego_Find_Node(SYMBOL sym,
                          WN* wn_tree)
{
  if (OPCODE_has_sym(WN_opcode(wn_tree)) && SYMBOL(wn_tree) == sym)
    return wn_tree;
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn)) {
      WN* wn_result = Lego_Find_Node(sym, wn);
      if (wn_result != NULL)
        return wn_result;
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) {
      WN* wn_result = Lego_Find_Node(sym, WN_kid(wn_tree, i));
      if (wn_result != NULL)
        return wn_result;
    }
  }
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: Lego_Find_Nodes
// FUNCTION: Find all of the nodes in the tree rooted at 'wn_tree' with the
//    symbol 'sym' iand OPERATOR type 'opr' and push their addresses on the
//    'stack'.
//-----------------------------------------------------------------------

extern void Lego_Find_Nodes(OPERATOR opr,
                            SYMBOL sym,
                            WN* wn_tree,
                            STACK<WN*>* stack)
{
  if (WN_operator(wn_tree) == opr) {
    SYMBOL newsym = SYMBOL(wn_tree);
    if (newsym == sym)
      stack->Push(wn_tree);
  }
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Lego_Find_Nodes(opr, sym, wn, stack);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Lego_Find_Nodes(opr, sym, WN_kid(wn_tree, i), stack);
  }
}

MP_INFO::MP_INFO(WN* wn_pragmas)
{
  _pid_sym0 = NULL;
  _pid_sym1 = NULL;
  _nest_layout = NULL;
  _sched_type = MP_SCHED_SIMPLE;
  _nest_index = 0;
  _nest_total = 1;
  _is_pdo = FALSE;
  _plower_disabled = FALSE;
  _sym_frozen = NULL; 
  BOOL found_schedtype = FALSE;
  for (WN* wn = WN_first(wn_pragmas); wn != NULL; wn = WN_next(wn)) {
    if (WN_opcode(wn) == OPC_PRAGMA) {
      switch (WN_pragma(wn)) {
      case WN_PRAGMA_MPSCHEDTYPE:
        found_schedtype = TRUE;
        switch (WN_pragma_arg1(wn)) {
        case WN_PRAGMA_SCHEDTYPE_UNKNOWN:
          _sched_type = MP_SCHED_UNKNOWN;
	  _plower_disabled = TRUE; 
          break;
        case WN_PRAGMA_SCHEDTYPE_SIMPLE:
          _sched_type = MP_SCHED_SIMPLE;
	  _plower_disabled = FALSE; 
          break;
        case WN_PRAGMA_SCHEDTYPE_DYNAMIC:
          _sched_type = MP_SCHED_DYNAMIC;
	  _plower_disabled = TRUE; 
          break;
        case WN_PRAGMA_SCHEDTYPE_GSS:
          _sched_type = MP_SCHED_GSS;
	  _plower_disabled = TRUE; 
          break;
        case WN_PRAGMA_SCHEDTYPE_INTERLEAVE:
          _sched_type = MP_SCHED_INTERLEAVE;
	  _plower_disabled = FALSE; 
          break;
        case WN_PRAGMA_SCHEDTYPE_RUNTIME:
          _sched_type = MP_SCHED_RUNTIME;
	  _plower_disabled = TRUE; 
          break;
        case WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED:
          _sched_type = MP_SCHED_PSEUDOLOWERED;
	  _plower_disabled = TRUE; 
          break;
        }
        break;
      case WN_PRAGMA_PDO_BEGIN:
	_is_pdo = TRUE;
	/* note fallthrough */ 
      case WN_PRAGMA_DOACROSS:
      case WN_PRAGMA_PARALLEL_DO:
        _nest_index = WN_pragma_arg1(wn);
        _nest_total = WN_pragma_arg2(wn);
        break;
      }
    }
  }
  DO_LOOP_INFO* dli = NULL;
  if (!found_schedtype && _nest_index != 0) {
    WN* wn = NULL;
    WN* wn_first = LWN_Get_Parent(wn_pragmas);
    for (wn = wn_first; wn != NULL; wn = LWN_Get_Parent(wn)) {
      if (WN_opcode(wn) == OPC_DO_LOOP) {
        dli = Get_Do_Loop_Info(wn);
        if (dli->Mp_Info != NULL && dli->Mp_Info->Nest_Index() == 0)
          break;
      }
    }
    FmtAssert(wn != NULL,
     ("Could not find mp_info on outermore nested doacross loop"));
     _sched_type = dli->Mp_Info->Sched_Type();
     _plower_disabled = dli->Mp_Info->Plower_Disabled();
  }
}

MP_INFO::MP_INFO(MP_INFO* mp_info)
{
  _pid_sym0 = mp_info->_pid_sym0;
  _pid_sym1 = mp_info->_pid_sym1;
  _nest_index = mp_info->_nest_index;
  _nest_total = mp_info->_nest_total;
  _sched_type = mp_info->_sched_type;
  _nest_index = mp_info->_nest_index;
  _nest_total = mp_info->_nest_total;
  _nest_layout = mp_info->_nest_layout;
  _is_pdo = mp_info->_is_pdo; 
  _plower_disabled = mp_info->_plower_disabled;
  _sym_frozen = mp_info->_sym_frozen; 
}

MP_INFO::MP_INFO(MP_SCHED_TYPE sched_type, 
		 BOOL is_pdo)
{
  _pid_sym0 = NULL;
  _pid_sym1 = NULL;
  _sched_type = sched_type;
  _nest_index = 0;
  _nest_total = 1;
  _nest_layout = NULL;
  _is_pdo = is_pdo; 
  _plower_disabled = FALSE;
  _sym_frozen = NULL; 
}

void MP_INFO::Print(FILE* fp)
{
  switch (_sched_type) {
  case MP_SCHED_SIMPLE:
    fprintf(fp, "  Scheduled: SIMPLE\n");
    break;
  case MP_SCHED_DYNAMIC:
    fprintf(fp, "  Scheduled: DYNAMIC\n");
    break;
  case MP_SCHED_GSS:
    fprintf(fp, "  Scheduled: GSS\n");
    break;
  case MP_SCHED_INTERLEAVE:
    fprintf(fp, "  Scheduled: INTERLEAVE\n");
    break;
  case MP_SCHED_RUNTIME:
    fprintf(fp, "  Scheduled: RUNTIME\n");
    break;
  case MP_SCHED_PSEUDOLOWERED:
    fprintf(fp, "  Scheduled: PSEUDOLOWERED\n");
    break;
  }
  if (Pid_Sym0() != NULL) {
    const char* name = ST_class(Pid_Sym0()->St()) != CLASS_PREG
      ? ST_name(Pid_Sym0()->St())
      : Pid_Sym0()->WN_Offset() > Last_Dedicated_Preg_Offset
      ? Preg_Name(Pid_Sym0()->WN_Offset()) : "DEDICATED PREG";
    fprintf(fp, " Pid_Sym0 = %s\n", name);
  }
  fprintf(stdout, "  Doacross %d out of %d\n", _nest_index,
    _nest_total);
  if (Pid_Sym1() != NULL) {
    const char* name = ST_class(Pid_Sym1()->St()) != CLASS_PREG
      ? ST_name(Pid_Sym1()->St())
      : Pid_Sym1()->WN_Offset() > Last_Dedicated_Preg_Offset
      ? Preg_Name(Pid_Sym1()->WN_Offset()) : "DEDICATED PREG";
    fprintf(fp, " Pid_Sym1 = %s\n", name);
  }
  if (Sym_Frozen() != NULL) {
    const char* name = ST_class(Sym_Frozen()->St()) != CLASS_PREG
      ? ST_name(Sym_Frozen()->St())
      : Sym_Frozen()->WN_Offset() > Last_Dedicated_Preg_Offset
      ? Preg_Name(Sym_Frozen()->WN_Offset()) : "DEDICATED PREG";
    fprintf(fp, " Sym_Frozen = %s\n", name);
  }
  if (Nest_Layout() != NULL)
    fprintf(stdout, "  layout pid = %s\n", ST_name(Nest_Layout()->St()));
  if (_is_pdo)
    fprintf(stdout, "  Loop is PDO\n"); 
  if (_plower_disabled)
    fprintf(stdout, "  Pseudo-lowering is disabled\n"); 
}

extern void Freeze_Cur_Numthreads_Func(WN* wn_loop)
{
  char Str_Buf[67];
  DU_MANAGER* du = Du_Mgr;
#ifndef KEY
  OPCODE op_call = OPCODE_make_op(OPR_CALL, MTYPE_I4, MTYPE_V);
  WN* wn_call = WN_Create(op_call, 0);
  WN_st_idx(wn_call) = ST_st_idx(distr_st_entries[mp_cur_numthreads_func]);
  Set_Runtime_Call_Side_Effects(wn_call);
  PREG_NUM rreg1, rreg2;
  ST* rst = Find_Return_Registers(MTYPE_I4, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));
#endif
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_loop));
  FmtAssert(WN_opcode(wn_region) == OPC_REGION,
    ("Freeze_Numthreads_Ldid: Could not find mp region"));
#ifndef KEY
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_region), wn_region, wn_call);
  WN* wn_ldid = WN_CreateLdid(OPCODE_make_op(OPR_LDID, MTYPE_I4, MTYPE_I4),
    rreg1, rst, Be_Type_Tbl(MTYPE_I4));
  du->Add_Def_Use(wn_call, wn_ldid);
#else // load the variable __ompc_cur_numthreads instead of doing a call
  WN* wn_ldid = WN_CreateLdid(OPCODE_make_op(OPR_LDID, MTYPE_I4, MTYPE_I4),
    0, distr_st_entries[mp_cur_numthreads], Be_Type_Tbl(MTYPE_I4));
#endif
  sprintf(Str_Buf, "$frz_cur_num_threads%d", WN_map_id(wn_loop));
  SYMBOL* sym_frozen = Create_Local_Symbol(Str_Buf, MTYPE_I4);
  WN* wn_stid = AWN_StidIntoSym(sym_frozen, wn_ldid);
  Create_local_alias(Alias_Mgr, wn_stid);
  WN_Set_Linenum(wn_stid, WN_Get_Linenum(wn_loop));
#ifndef KEY
  LWN_Insert_Block_After(LWN_Get_Parent(wn_call), wn_call, wn_stid);
#else
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_region), wn_region, wn_stid);
#endif
  DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop);
  dli_loop->Mp_Info->Set_Sym_Frozen(sym_frozen);
  Add_Pragma_To_MP_Region(wn_loop, sym_frozen->St(), 
		sym_frozen->WN_Offset(), WN_PRAGMA_SHARED);
}


extern void Freeze_Numthreads_Ldid(WN* wn_loop)
{
  DU_MANAGER* du = Du_Mgr; 
  char Str_Buf[64];
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  Is_True(LNO_Pseudo_Lower || dli->Mp_Info->Nest_Total() > 1,
    ("Freeze_Numthreads_Ldid called but plower is off"));
  WN* wn_ldid = Get_Runtime_Numthreads_Ldid();
  TYPE_ID type = WN_desc(wn_ldid);
  TYPE_ID type_trip = Promote_Type(Do_Wtype((WN *) wn_loop));
  MP_SCHED_TYPE sched_type = dli->Mp_Info->Sched_Type(); 
  FmtAssert(sched_type == MP_SCHED_SIMPLE || sched_type == MP_SCHED_INTERLEAVE,
    ("Only freezing threads for SIMPLE and INTERLEAVE scheduling")); 
  WN* wn_trip = Trip_Count(wn_loop); 
  wn_trip = LWN_Integer_Cast(wn_trip, type, type_trip);  
  if (dli->Mp_Info->Sched_Type() == MP_SCHED_INTERLEAVE) {
    WN* wn_chunk = Interleaved_Pragma_Chunksize(wn_loop, Du_Mgr); 
    if (WN_operator(wn_chunk) != OPR_INTCONST
      || WN_const_val(wn_chunk) != 1) {
      wn_chunk = LWN_Integer_Cast(wn_chunk, type, type_trip);
      wn_trip = LWN_CreateDivceil(type, wn_trip, wn_chunk);  
    } 
  } 
  sprintf(Str_Buf, "$frz_num_threads%d", WN_map_id(wn_loop));
  SYMBOL* sym_frozen = Create_Local_Symbol(Str_Buf, type);
  WN* wn_stid = AWN_StidIntoSym(sym_frozen, wn_ldid);
  Create_local_alias(Alias_Mgr, wn_stid);
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_loop));
  FmtAssert(WN_opcode(wn_region) == OPC_REGION, 
    ("Freeze_Numthreads_Ldid: Could not find mp region")); 
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_region), wn_region, wn_stid);

  WN* wn_fzldid = AWN_LdidSym(sym_frozen); 
  du->Add_Def_Use(wn_stid, wn_fzldid); 
  Copy_alias_info(Alias_Mgr, wn_stid, wn_fzldid); 
  WN* wn_min = AWN_Min(type, wn_fzldid, wn_trip);  
  sprintf(Str_Buf, "$frz_min%d", WN_map_id(wn_loop));
  SYMBOL* sym_fzmin = Create_Local_Symbol(Str_Buf, type);
  WN* wn_fzstid = AWN_StidIntoSym(sym_fzmin, wn_min);
  Create_local_alias(Alias_Mgr, wn_fzstid);
  dli->Mp_Info->Set_Sym_Frozen(sym_fzmin); 
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_loop), wn_loop, wn_fzstid);

  // Move assignment to frz_num_threads if we are versioning the mp-loop
  WN *wn;
  for (wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn))  
    if (WN_opcode(wn) == OPC_IF) 
      break; 
  if (wn == NULL || !WN_Is_If_MpVersion(wn)) 
    return;
  LWN_Extract_From_Block(wn_stid);
  LWN_Insert_Block_Before(LWN_Get_Parent(wn), wn, wn_stid); 
  WN* wn_pattern = AWN_LdidSym(sym_frozen); 
  du->Add_Def_Use(wn_stid, wn_pattern); 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (WN_operator(wn) == OPR_LDID 
	&& SYMBOL(wn) == SYMBOL(wn_ldid)) {
      Replace_Symbol(wn, SYMBOL(wn), SYMBOL(wn_stid), wn_pattern, wn); 
      du->Add_Def_Use(wn_stid, wn); 
    }
  } 
  DOLOOP_STACK dostack(&LNO_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(wn), &dostack);
  LNO_Build_Access(wn, &dostack, &LNO_default_pool);
  LWN_Delete_Tree(wn_pattern);  
}

extern WN* Get_Frozen_Numthreads_Ldid(WN* wn_loop)
{
  // WN *wn;
	// modifed by csc. 2002/11/14
  WN *wn = WN_Intconst( MTYPE_I4, 4 );
  return wn;
  for (wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO* dli_wn = Get_Do_Loop_Info(wn);
      if (dli_wn->Mp_Info != NULL && dli_wn->Mp_Info->Sym_Frozen() != NULL)
	break;
    }
  }
  FmtAssert(wn != NULL, ("Could not find frozen value for number of threads"));
  WN* wn_outer_loop = wn; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_outer_loop);
  if (dli->Mp_Info->Is_Pdo())
    if (Is_Versioned_Mp_Loop(wn_outer_loop))
      return Get_Runtime_Cur_Numthreads_Ldid();
    else
      return Get_Runtime_Cur_Numthreads_Func(wn_outer_loop);
  for (wn = wn_outer_loop; wn != NULL; wn = WN_prev(wn)) 
    if (WN_operator(wn) == OPR_STID 
      &&  SYMBOL(wn) == *(dli->Mp_Info->Sym_Frozen()))
      break;
  FmtAssert(wn != NULL, ("Could not find frozen value for number of threads"));
  WN* wn_stid = wn;
  WN* wn_ldid = AWN_LdidSym(dli->Mp_Info->Sym_Frozen());
  Copy_alias_info (Alias_Mgr, wn_stid, wn_ldid);
  Du_Mgr->Add_Def_Use(wn_stid, wn_ldid);
  return wn_ldid;
}

extern WN* Get_Runtime_Cur_Numthreads_Func(WN* wn_loop)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  FmtAssert(dli->Mp_Info->Is_Pdo(), 
    ("Get_Runtime_Cur_Numthreads_Func: Expected a PDO")); 
  DO_LOOP_INFO* dli_wn = NULL;
  WN *wn;
  for (wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      dli_wn = Get_Do_Loop_Info(wn);
      if (dli_wn->Mp_Info != NULL && dli_wn->Mp_Info->Sym_Frozen() != NULL)
	break;
    }
  }
  FmtAssert(wn != NULL, ("Could not find frozen value for number of threads"));
  FmtAssert(dli_wn->Mp_Info != NULL && dli_wn->Mp_Info->Sym_Frozen(), 
    ("Get_Runtime_Cur_Numthreads_Func: Can't find frozen symbol")); 
  WN* wn_sym_loop = wn; 
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_sym_loop)); 
  for (wn = wn_region; wn != NULL; wn = WN_prev(wn)) 
    if (WN_operator(wn) == OPR_STID 
        && SYMBOL(wn) == *(dli_wn->Mp_Info->Sym_Frozen()))
      break;
  FmtAssert(wn != NULL, ("Could not find frozen value for number of threads"));
  WN* wn_stid = wn;
  WN* wn_ldid = AWN_LdidSym(dli_wn->Mp_Info->Sym_Frozen());
  Copy_alias_info (Alias_Mgr, wn_stid, wn_ldid);
  Du_Mgr->Add_Def_Use(wn_stid, wn_ldid);
  return wn_ldid;
}


static INT Current_Highest_Lego_Mp_Tile_Key = 0; 

//-----------------------------------------------------------------------
// NAME: Get_New_Lego_Mp_Tile_Key 
// FUNCTION: Returns a unique key which can be used to index groups of 
//   loops produced by lego or MP tiling.  
//-----------------------------------------------------------------------

extern INT Get_New_Lego_Mp_Tile_Key(void)
{
  return ++Current_Highest_Lego_Mp_Tile_Key;
}


/***********************************************************************
 *
 * Allocate and return a SYMBOL* for a local variable of type mtype.
 *
 **********************************************************************/
extern SYMBOL* Create_Local_Symbol (char* name, TYPE_ID mtype) {
  ST* st         = New_ST(CURRENT_SYMTAB);
  ST_Init (st,
           Save_Str(name),
           CLASS_VAR,
           SCLASS_AUTO,
           EXPORT_LOCAL,
           Be_Type_Tbl(mtype));
  Set_ST_is_temp_var(st);
  SYMBOL* sym = CXX_NEW (SYMBOL (st, 0, mtype), &LNO_default_pool);
  return sym;
}

/***********************************************************************
 *
 * Given a wn, find the closest enclosing MP region (including wn)
 * if any. Return NULL otherwise.
 *
 **********************************************************************/
extern WN* Get_MP_Region (WN* wn) {
  while (wn) {
    if (Is_Mp_Region(wn)) return wn;
    wn = LWN_Get_Parent(wn);
  }
  return wn;
}


/**********************************************************************
*      Assert if the bounds and strides of an array are not constants.
*      Then return true if it is stride-one.
*********************************************************************/
extern BOOL Fixed_Size_Array_Is_Stride_One(ST* array_st)
{
  TY_IDX array_ty = Lego_Get_Array_Type(array_st);
  INT ndims = TY_AR_ndims(array_ty);
  for (INT i=0; i<ndims; i++) {
    FmtAssert (TY_AR_const_lbnd(array_ty, i) &&
               TY_AR_const_ubnd(array_ty, i) &&
               TY_AR_const_stride(array_ty, i),
               ("Fixed size array (%s) must have constant %s\n",
                ST_name(array_st),
                (!TY_AR_const_lbnd(array_ty,i) ? "lbnd" :
                 (!TY_AR_const_ubnd(array_ty,i) ? "ubnd" :
                  "stride"))));
  
    INT stride = TY_size(TY_AR_etype(array_ty));
    for (INT j=i+1; j<ndims; j++)
      stride = (stride * (TY_AR_ubnd_val(array_ty, j) -
                          TY_AR_lbnd_val(array_ty, j) + 1));
    if (TY_AR_stride_val(array_ty, i) != stride)
      return FALSE;
  }
  return TRUE;
}


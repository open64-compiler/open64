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


#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include <alloca.h>

#include "pu_info.h"
#include "defs.h"
#include "strtab.h"
#include "opcode.h"
#include "lego_pragma.h"
#include "lego_util.h"
#include "access_vector.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "config_targ.h"
#include "targ_const.h"
#include "const.h"
#include "dra_ec.h"             // CART_ offsets, DRA_EC_struct_ptr_ty
#include "dra_demangle.h"       // DRA_Demangle_Func


/***********************************************************************
 *
 * External declarations.
 *
 ***********************************************************************/
extern WN_MAP LNO_Info_Map;
extern MEM_POOL LNO_default_pool;

/***********************************************************************
 *
 * Private declarations.
 *
 ***********************************************************************/

static WN* Gen_Malloc_Cart      (WN* insert_wn, INT32 size, SYMBOL* sym);
static WN* Gen_Call_Array       (WN* prev_wn, WN* array_wn,
                                 ST* func_st, SYMBOL* rvsym);
static void Gen_Call_Array_Cart (WN* prev_wn, WN* array_wn, WN* cart_wn,
                                 ST* func_st);


/***********************************************************************
 *
 * Recognize all reshaped array portions being passed as a parameter,
 * and annotate them with runtime debugging information.
 *
 ***********************************************************************/
extern void EC_Array_Portion_Calls (WN* wn) {
  if (wn == NULL) return;

  if (OPCODE_is_expression(WN_opcode(wn))) return; // cannot have a call
  
  if (!OPCODE_is_call(WN_opcode(wn))) {
    if (WN_operator(wn) == OPR_BLOCK) {
      WN* kid = WN_first(wn);
      while (kid) {
        WN* next = WN_next(kid);
        EC_Array_Portion_Calls (kid);
        kid = next;
      }
    } else {
      for (INT i=0; i<WN_kid_count(wn); i++) {
        EC_Array_Portion_Calls(WN_kid(wn, i));
      }
    }
    return;
  }

  Is_True (OPCODE_is_call(WN_opcode(wn)),
           ("EC_Calls: expected a call-node\n"));

  // don't process calls to intrinsics
  if (WN_operator(wn) == OPR_INTRINSIC_CALL) return;

  // don't process calls to the runtime routines.
  if (strncmp(ST_name(WN_st(wn)), "__dsm_", 6) == 0) return;

  mINT16 num_actuals = WN_num_actuals(wn);
  for (INT i=0; i<num_actuals; i++) {
    WN* arg_wn = WN_actual(wn, i);
    Is_True (WN_operator(arg_wn) == OPR_PARM,
             ("EC_Calls: expected PARM node under a call"));
    arg_wn = WN_kid0(arg_wn);
    if (WN_operator(arg_wn) != OPR_ARRAY) continue;

    WN* array_base = WN_array_base(arg_wn);
    if (WN_operator(array_base) != OPR_LDID &&
        WN_operator(array_base) != OPR_LDA) continue;

    ST* array_st = WN_st(array_base);
    DISTR_INFO* dinfo = da_hash->Find(array_st);
    if (dinfo == NULL || dinfo->IsReshaped() == FALSE) continue;
      
    // ok - so it is a reshaped array portion
    // find real insertion point --- the closest enclosing statement.
    // Insert before insert_wn, or after prev_wn
    WN* insert_wn = wn;
    WN* prev_wn;
    while (!OPCODE_is_stmt(WN_opcode(insert_wn)))
      insert_wn = LWN_Get_Parent(insert_wn);
    char name[64];
    sprintf (name, "$cart_actual_%s_%d\n", ST_name(array_st), i);
    SYMBOL cart_sym = Create_Preg_Symbol(name, Pointer_type);
      
    TY_IDX array_ty = dinfo->Orig_TY();

    // "malloc" a cart, before insert_wn
    WN* cart_stid_wn = Gen_Malloc_Cart (insert_wn,
                                        (dinfo->Num_Dim()+3)*
                                        TY_size(Be_Type_Tbl(MTYPE_I8)),
                                        &cart_sym);
    prev_wn = cart_stid_wn;
    // now initialize values in the cart
    WN* cart_ldid_wn = AWN_LdidSym(&cart_sym);
    TY_IDX I8ptr_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8));
    Copy_alias_info (Alias_Mgr, cart_stid_wn, cart_ldid_wn);
    Du_Mgr->Add_Def_Use (cart_stid_wn, cart_ldid_wn);
    OPCODE istore_op = OPCODE_make_op(OPR_ISTORE, MTYPE_V, MTYPE_I8);
    // cart->array_base = array-base
    WN* istore_wn = LWN_CreateIstore (
      OPCODE_make_op(OPR_ISTORE,MTYPE_V,Pointer_type),
      CART_array_base,
      Make_Pointer_Type(Be_Type_Tbl(Pointer_type)),
      dinfo->Load_Distr_Array(),
      cart_ldid_wn);
    LWN_Copy_Linenumber(wn, istore_wn);
    LWN_Insert_Block_After (NULL, prev_wn, istore_wn);
    prev_wn = istore_wn;
    // cart->ndims = ndims
    cart_ldid_wn = LWN_Copy_Tree(cart_ldid_wn);
    Du_Mgr->Add_Def_Use (cart_stid_wn, cart_ldid_wn);
    istore_wn = LWN_CreateIstore (istore_op, CART_ndims, I8ptr_ty,
                                  LWN_Make_Icon(MTYPE_I8, dinfo->Num_Dim()),
                                  cart_ldid_wn);
    LWN_Copy_Linenumber(wn, istore_wn);
    LWN_Insert_Block_After (NULL, prev_wn, istore_wn);
    prev_wn = istore_wn;
    // cart->element_size = element_size
    cart_ldid_wn = LWN_Copy_Tree(cart_ldid_wn);
    Du_Mgr->Add_Def_Use (cart_stid_wn, cart_ldid_wn);
    istore_wn = LWN_CreateIstore(istore_op, CART_element_size, I8ptr_ty,
                                 LWN_Make_Icon(MTYPE_I8,
                                               TY_size(TY_AR_etype(array_ty))),
                                 cart_ldid_wn);
    LWN_Copy_Linenumber(wn, istore_wn);
    LWN_Insert_Block_After (NULL, prev_wn, istore_wn);
    prev_wn = istore_wn;
    for (INT j=0; j<dinfo->Num_Dim(); j++) {
      // cart->index[i] == index-value
      WN* index_wn = LWN_Copy_Tree (WN_array_index(arg_wn, j));
      LWN_Copy_Def_Use (WN_array_index(arg_wn, j), index_wn, Du_Mgr);
      cart_ldid_wn = LWN_Copy_Tree(cart_ldid_wn);
      Du_Mgr->Add_Def_Use (cart_stid_wn, cart_ldid_wn);
      istore_wn = LWN_CreateIstore (istore_op, CART_index+8*j,
                                    I8ptr_ty,
                                    index_wn,
                                    cart_ldid_wn);
      LWN_Copy_Linenumber(wn, istore_wn);
      LWN_Insert_Block_After (NULL, prev_wn, istore_wn);
      prev_wn = istore_wn;
    }

    // now insert cart into hash-table
    WN* array_wn = LWN_Copy_Tree (arg_wn);
    LWN_Copy_Def_Use (arg_wn, array_wn, Du_Mgr);
    // copy the access array
    ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, arg_wn);
    ACCESS_ARRAY* new_aa = CXX_NEW (ACCESS_ARRAY(aa, &LNO_default_pool),
                                    &LNO_default_pool);
    WN_MAP_Set (LNO_Info_Map, array_wn, new_aa);
    cart_ldid_wn = LWN_Copy_Tree(cart_ldid_wn);
    Du_Mgr->Add_Def_Use (cart_stid_wn, cart_ldid_wn);
    Gen_Call_Array_Cart (prev_wn, array_wn, cart_ldid_wn,
                         distr_st_entries[ECHT_Push]);

    // now remove cart from hash-table
    array_wn = LWN_Copy_Tree (arg_wn);
    LWN_Copy_Def_Use (arg_wn, array_wn, Du_Mgr);
    new_aa = CXX_NEW (ACCESS_ARRAY (aa, &LNO_default_pool),
                      &LNO_default_pool);
    WN_MAP_Set (LNO_Info_Map, array_wn, new_aa);
    Gen_Call_Array (insert_wn, array_wn,
                    distr_st_entries[ECHT_Pop], NULL);
  }
} /* EC_Calls () */

/***********************************************************************
 *
 * Generate 
 *  - sym = malloc(size) 
 *  - <insert_wn>
 *
 * and return the stid_wn of the sym for du-chains.
 *
 ***********************************************************************/
static WN* Gen_Malloc_Cart (WN* insert_wn, INT32 size, SYMBOL* sym) {
  OPCODE icallop = OPCODE_make_op(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V);
  WN* call_wn = WN_Create(icallop, 1);
  WN_intrinsic(call_wn) =
    Pointer_Size == 8 ? INTRN_U8I8MALLOC : INTRN_U4I4MALLOC;
  LWN_Copy_Linenumber(insert_wn, call_wn);
  WN* size_wn = LWN_Make_Icon (Pointer_Size == 8 ? MTYPE_I8 : MTYPE_I4,
                               size);
  WN* parm_wn = WN_CreateParm(MTYPE_U8, size_wn,
                              Be_Type_Tbl(MTYPE_U8), WN_PARM_BY_VALUE);
  LWN_Set_Parent(size_wn, parm_wn);
  WN_kid0(call_wn) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);

  LWN_Insert_Block_Before (NULL, insert_wn, call_wn);
  PREG_NUM rreg1, rreg2;
  ST* rst = Find_Return_Registers(Pointer_type, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));
      
  WN *ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                              Pointer_type,
                                              Pointer_type),
                               rreg1, rst, DRA_EC_struct_ptr_ty);
  // Be_Type_Tbl(Pointer_type));
  Create_alias (Alias_Mgr, ldid_wn); // just a register
  Du_Mgr->Add_Def_Use (call_wn, ldid_wn);
  WN* stid_wn = AWN_StidIntoSym (sym, ldid_wn);
  Create_alias (Alias_Mgr, stid_wn);
  LWN_Copy_Linenumber (insert_wn, stid_wn);
  LWN_Insert_Block_Before (NULL, insert_wn, stid_wn);

  return stid_wn;
} /* Gen_Malloc_Cart () */


/***********************************************************************
 *
 * Given an ldid in array_wn, generate a call to the given function
 * with a single argument, array_wn.
 * If rvsym is non-NULL, then store the return value of the function
 * into rvsym, and return the stid.
 * All this code is generated after prev_wn.
 *
 ***********************************************************************/
static WN* Gen_Call_Array (WN* prev_wn, WN* array_wn,
                           ST* func_st, SYMBOL* rvsym) {
  WN* call_wn = WN_Create(OPCODE_make_op(OPR_CALL,
                                         (rvsym ? Pointer_type : MTYPE_V),
                                         MTYPE_V), 1);
  WN* parm_wn = WN_CreateParm (Pointer_type, array_wn,
                               Be_Type_Tbl(Pointer_type),
                               WN_PARM_BY_VALUE);
  LWN_Set_Parent(array_wn, parm_wn);
  WN_kid(call_wn, 0) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);
  WN_st_idx(call_wn) = ST_st_idx(func_st);
  LWN_Insert_Block_After (NULL, prev_wn, call_wn);
  prev_wn = call_wn;

  if (rvsym) {
    // generate retval assignment into sym
    PREG_NUM rreg1, rreg2;
    ST* rst = Find_Return_Registers(Pointer_type, &rreg1, &rreg2);
    FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));
      
    WN *ldid_wn = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                                Pointer_type,
                                                Pointer_type),
                                 rreg1, rst, DRA_EC_struct_ptr_ty);
    // Be_Type_Tbl(Pointer_type));
    Create_alias (Alias_Mgr, ldid_wn); // just a register
    Du_Mgr->Add_Def_Use (call_wn, ldid_wn);
    WN* stid_wn = AWN_StidIntoSym (rvsym, ldid_wn);
    Create_alias (Alias_Mgr, stid_wn);
    LWN_Copy_Linenumber (prev_wn, stid_wn);
    LWN_Insert_Block_After (NULL, prev_wn, stid_wn);
    return stid_wn;
  }
  return NULL;
} /* Gen_Call_Array () */

/***********************************************************************
 *
 * Given an insertion point, and array load, and a cart load,
 * generate a call to the given func_st and insert it after prev_wn.
 *
 ***********************************************************************/
static void Gen_Call_Array_Cart (WN* prev_wn, WN* array_wn, WN* cart_wn,
                                 ST* func_st) {

  WN* call_wn = WN_Create(OPCODE_make_op(OPR_CALL, MTYPE_V, MTYPE_V), 2);
  WN* parm_wn = WN_CreateParm (Pointer_type, array_wn,
                               Be_Type_Tbl(Pointer_type),
                               WN_PARM_BY_VALUE);
  LWN_Set_Parent(array_wn, parm_wn);
  WN_kid(call_wn, 0) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);
  parm_wn = WN_CreateParm (Pointer_type, cart_wn,
                           Be_Type_Tbl(Pointer_type),
                           WN_PARM_BY_VALUE);
  LWN_Set_Parent(cart_wn, parm_wn);
  WN_kid(call_wn, 1) = parm_wn;
  LWN_Set_Parent(parm_wn, call_wn);
  WN_st_idx(call_wn) = ST_st_idx(func_st);
  LWN_Insert_Block_After (NULL, prev_wn, call_wn);
} /* Gen_Call_Array_Cart () */

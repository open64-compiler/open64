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



#include "mem_ctr.h"
#include "wn.h"
#include "stab.h"
#include "strtab.h"
#include "mtypes.h"
#include "targ_const.h"
#include "wn_util.h"
#include "config_targ.h"
#include "const.h"
#include "cxx_template.h"
#include "cxx_hash.h"

/***********************************************************************
 *
 * Exported symbols.
 *
 ***********************************************************************/

extern void MemCtr_Add (WN* pu);

/***********************************************************************
 *
 * Local declarations.
 *
 ***********************************************************************/

static INT memctr_size = 8192;  /* default at least half a page */

static ST *reg_st_global;
static ST *reg_st_local;
static ST *unreg_st_local;
static ST *reg_st_common;

static ST*  Declare_Function (const char* ty_name, const char* st_name, 
                              INT nparms, TY *ty_array[]);
static void Set_Call_Side_Effects (WN* call_wn);
static void MemCtr_Init ();
static BOOL MemCtr_Criteria_Check (ST* st);

static MEM_POOL memctr_pool;
static WN_MAP Parent_Map = WN_MAP_UNDEFINED;
#define Set_Parent(wn, p) (WN_MAP_Set(Parent_Map, wn, (void*)  p))
#define Get_Parent(wn) ((WN*) WN_MAP_Get(Parent_Map, (WN*) wn))
static void Parentize (WN* wn);

typedef STACK<WN *> STACK_OF_WN;
typedef HASH_TABLE<ST*, BOOL> STBOOL_HASH_TABLE;

static void MemCtr_Add_Global (WN* pu, ST* st,
                               STACK_OF_WN *altentry_stack);
static void MemCtr_Add_Common (WN* pu, ST* st,
                               STACK_OF_WN *altentry_stack);
static void MemCtr_Add_Local  (WN* pu, ST* st,
                               STACK_OF_WN *altentry_stack,
                               STACK_OF_WN *exit_stack);

static void Collect_Goodies (WN* pu,
                             STACK_OF_WN *exit_stack,
                             STACK_OF_WN *altentry_stack,
                             STBOOL_HASH_TABLE *used_st_hash);

extern void MemCtr_Add (WN* pu) {
  ST* st;

  STACK_OF_WN *altentry_stack = NULL;
  STACK_OF_WN *exit_stack = NULL;

  STBOOL_HASH_TABLE *used_st_hash = NULL;

  // one-time initializations
  {
    static BOOL memctr_initialized = FALSE;
    if (!memctr_initialized) {
      memctr_initialized = TRUE;
      MemCtr_Init ();
    }
  }

  MEM_POOL_Push (&memctr_pool);

  Parent_Map = WN_MAP_Create (&memctr_pool);
  Parentize (pu);

  // walk the tree, store all the STs that are referenced,
  // and collect exit points and alt-entries
  {

    // Create stack to collect the exit points in the PU
    exit_stack = CXX_NEW(STACK_OF_WN(&memctr_pool), &memctr_pool);

    // Create stack to collect the alt-entries in the PU
    if (PU_has_altentry(Get_Current_PU())) {
      altentry_stack = CXX_NEW (STACK_OF_WN(&memctr_pool), &memctr_pool);
    }

    // Create hash-table to collect referenced symbols
    used_st_hash = CXX_NEW(STBOOL_HASH_TABLE(200,&memctr_pool),&memctr_pool);

    // Walk the tree, collecting exits, altentries, and used STs
    Collect_Goodies (pu, exit_stack,
                     altentry_stack, used_st_hash);
  }



  INT i;
  FOREACH_SYMBOL(GLOBAL_SYMTAB, st, i) {
    if (ST_class(st) == CLASS_VAR &&
        used_st_hash->Find(st) &&
        !ST_is_reshaped(st)) {
      MemCtr_Add_Global (pu, st, altentry_stack);
    }
  }

  FOREACH_SYMBOL(CURRENT_SYMTAB, st, i) {
    if (ST_class(st) == CLASS_VAR && used_st_hash->Find(st)) {
      if (
          ST_base(st) != st &&
          ST_sclass(ST_base(st)) == SCLASS_COMMON) {
        if (!ST_is_reshaped(st)) {
          MemCtr_Add_Common (pu, st, altentry_stack);
        }
      }
      else {
        if (!ST_is_reshaped(st)) {
          MemCtr_Add_Local (pu, st, altentry_stack, exit_stack);
        }
      }
    }
  }

  WN_MAP_Delete (Parent_Map);
  Parent_Map = WN_MAP_UNDEFINED;
  MEM_POOL_Pop (&memctr_pool);
}

static void Parentize (WN* wn) {
  if (!OPCODE_is_leaf (WN_opcode (wn))) { 
    if (WN_opcode(wn) == OPC_BLOCK) {
      WN* kid = WN_first (wn);
      while (kid) {
        Set_Parent (kid, wn);
        Parentize (kid);
        kid = WN_next (kid);
      }
    }
    else {
      INT kidno;
      WN* kid;
      for (kidno=0; kidno<WN_kid_count(wn); kidno++) {
        kid = WN_kid (wn, kidno);
        if (kid) { 
          Set_Parent (kid, wn);
          Parentize (kid);
        }
      }
    }
  }
}

/***********************************************************************
 *
 * Given an array node, find the base ST.
 *
 ***********************************************************************/
static ST *Find_Address_Base (WN* wn) {
  FmtAssert (wn && (WN_operator(wn) == OPR_ILOAD ||
                    WN_operator(wn) == OPR_ISTORE),
             ("Find_Address_Base: expected an ILOAD/ISTORE node"));

  if (WN_operator(wn) == OPR_ILOAD)
    wn = WN_kid0(wn);
  else wn = WN_kid1(wn);

  // wn is now the address expression
  OPERATOR opr = WN_operator(wn);
  if (opr == OPR_LDID || opr == OPR_LDA) 
    return &St_Table[WN_st_idx(wn)];

  return NULL;
}

static void Collect_Goodies (WN* wn,
                             STACK_OF_WN *exit_stack,
                             STACK_OF_WN *altentry_stack,
                             STBOOL_HASH_TABLE *used_st_hash) {

  if (!wn) return;

  OPCODE opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);

  if (opr == OPR_RETURN) {
    exit_stack->Push(wn);
    return;
  }

  if (opr == OPR_INTRINSIC_CALL && (WN_intrinsic(wn) == INTRN_STOP || 
				    WN_intrinsic(wn) == INTRN_STOP_F90)) {
    exit_stack->Push (wn);
    return;
  }

  if (opr == OPR_ALTENTRY) {
    altentry_stack->Push (wn);
    return;
  }
  
  if ((opr==OPR_ILOAD && WN_operator(WN_kid0(wn))==OPR_ARRAY) ||
      (opr==OPR_ISTORE && WN_operator(WN_kid1(wn))==OPR_ARRAY))
    {
      WN* address_wn = (opr == OPR_ILOAD ? WN_kid0(wn) : WN_kid1(wn));
      if (WN_operator(address_wn) == OPR_LDID ||
          WN_operator(address_wn) == OPR_LDA) {
        
        ST *st = &St_Table[WN_st_idx(address_wn)];
        if (!used_st_hash->Find(st)) used_st_hash->Enter (st, TRUE);
      }
    }

  ST *st = &St_Table[WN_st_idx(wn)];

  if ((opr == OPR_LDA && TY_kind(ST_type(st)) == KIND_ARRAY) ||
      (opr == OPR_LDID && 
       ST_base(st) &&
       ST_sclass(ST_base(st)) == SCLASS_AUTO) ||
      (opr == OPR_LDID && TY_kind(ST_type(st)) == KIND_POINTER &&
       TY_kind(TY_pointed(ST_type(st))) == KIND_ARRAY)) {

    if (!used_st_hash->Find(st)) used_st_hash->Enter (st, TRUE);
  }
  
  // now recurse
  if (opr == OPR_BLOCK) {
    for (WN* kid = WN_first(wn); kid; kid = WN_next(kid)) {
      Collect_Goodies (kid, exit_stack, altentry_stack, used_st_hash);
    }
    return;
  }
  
  for (INT i=0; i<WN_kid_count(wn); i++) {
    Collect_Goodies (WN_kid(wn, i), exit_stack, altentry_stack, used_st_hash);
  }
}


static ST* Declare_Function (const char* ty_name, const char* st_name, 
                             INT nparms, TY_IDX ty_array[])
{
  ST* func_st;
  TY_IDX voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V), TRUE); 
  TY_IDX func_ty;
  TY& ty = New_TY(func_ty);
  PU_IDX pu_idx;
  PU& pu = New_PU (pu_idx);

  TYLIST_IDX idx = Tylist_Table.Insert(voidpty);
  for (INT i = 0; i < nparms; i++) {
    Tylist_Table.Insert(ty_array[i]);
  }
  Tylist_Table.Insert(0);
  TY_Init (ty, TY_size(voidpty), KIND_FUNCTION, MTYPE_UNKNOWN, Save_Str(ty_name));
  Set_TY_tylist (ty, idx);

  Set_TY_align (func_ty, TY_align(voidpty));
  Set_PU_prototype (pu, func_ty);

  /* Make a ST: add function to global symbol table */
  func_st = New_ST ( GLOBAL_SYMTAB );
  ST_Init(func_st, Save_Str(st_name), CLASS_FUNC, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, (TY_IDX) pu_idx);
  return func_st;
}

/***********************************************************************
 *
 * Given a call-node of one of the memory-counter registration routines,
 * update side-effect info.
 *
 ***********************************************************************/
static void Set_Call_Side_Effects (WN* call_wn) {
  WN_Set_Call_Parm_Ref (call_wn);
}

static void MemCtr_Init () {
  TY_IDX ty_array[10];

  // make address pointer to I8, shouldn't matter.
  TY_IDX string_ptr_ty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_U1), TRUE);
  TY_IDX i8_ty         = Be_Type_Tbl(MTYPE_I8);
  TY_IDX i8_ptr_ty     = Make_Pointer_Type (Be_Type_Tbl(MTYPE_I8), TRUE);
  
  // declare __memctr_register_global (address, size, name)
  {
    ty_array[0] = i8_ptr_ty;
    ty_array[1] = i8_ty;
    ty_array[2] = string_ptr_ty;
    reg_st_global = Declare_Function (".__memctr_register_global",
                                      "__memctr_register_global",
                                      3,
                                      ty_array);
  }

  // declare __memctr_register_common (address, size, common-name, name)
  {
    ty_array[0] = i8_ptr_ty;
    ty_array[1] = i8_ty;
    ty_array[2] = string_ptr_ty;
    ty_array[3] = string_ptr_ty;
    reg_st_common = Declare_Function (".__memctr_register_common",
                                      "__memctr_register_common",
                                      4,
                                      ty_array);
  }

  // declare __memctr_register_local (address, size, func-name, name)
  {
    ty_array[0] = i8_ptr_ty;
    ty_array[1] = i8_ty;
    ty_array[2] = string_ptr_ty;
    ty_array[3] = string_ptr_ty;
    reg_st_local = Declare_Function (".__memctr_register_local",
                                     "__memctr_register_local",
                                     4,
                                     ty_array);
  }

  // __memctr_unregister_local (address, size, func-name, name)
  {
    ty_array[0] = i8_ptr_ty;
    ty_array[1] = i8_ty;
    ty_array[2] = string_ptr_ty;
    ty_array[3] = string_ptr_ty;
    unreg_st_local = Declare_Function (".__memctr_unregister_local",
                                       "__memctr_unregister_local",
                                       4,
                                       ty_array);
  }

  MEM_POOL_Initialize (&memctr_pool, "memory_counter_pool", FALSE);
}

static BOOL MemCtr_Criteria_Check (ST* st) {
  if (TY_size(ST_type(st)) < memctr_size) return FALSE;
  return TRUE;
}

static BOOL Is_VLA (ST* st) {
  if (TY_kind(ST_type(st)) == KIND_POINTER &&
      TY_kind(TY_pointed(ST_type(st))) == KIND_ARRAY &&
      TY_size(TY_pointed(ST_type(st))) == 0) return TRUE;

  if (ST_base(st) &&
      ST_sclass(ST_base(st)) == SCLASS_AUTO)
    return TRUE;

  return FALSE;
}

/***********************************************************************
 *
 * Given a call-whirl tree, insert it right after the preamble in the PU,
 * and after the preamble in each alt-entry.
 *
 ***********************************************************************/
static void Insert_Call (WN* call_wn, WN* pu, STACK_OF_WN *altentry_stack) {

  WN* insert_wn = WN_first(WN_func_body(pu));
  while (insert_wn) {
    if (WN_opcode(insert_wn) == OPC_PRAGMA &&
        WN_pragma(insert_wn) == WN_PRAGMA_PREAMBLE_END)
      break;
    insert_wn = WN_next(insert_wn);
  }
  FmtAssert (insert_wn, ("Missing function preamble in function %s\n",
                         ST_name(WN_st(pu))));
  WN_INSERT_BlockAfter (WN_func_body(pu), insert_wn, call_wn);
  Set_Parent (call_wn, WN_func_body(pu));

  if (altentry_stack && altentry_stack->Elements()) {
    for (INT i=0; i<altentry_stack->Elements(); i++) {
      insert_wn = altentry_stack->Bottom_nth(i);
      while (insert_wn) {
        if (WN_opcode(insert_wn) == OPC_PRAGMA &&
            WN_pragma(insert_wn) == WN_PRAGMA_PREAMBLE_END)
          break;
        insert_wn = WN_next(insert_wn);
      }
      FmtAssert (insert_wn, ("Missing function preamble in function %s\n",
                             ST_name(WN_st(pu))));
      call_wn = WN_COPY_Tree (call_wn);
      Parentize (call_wn);
      WN_INSERT_BlockAfter (Get_Parent(insert_wn), insert_wn, call_wn);
      Set_Parent (call_wn, Get_Parent(insert_wn));
    }
  }
}   /* Insert_Call */

/***********************************************************************
 *
 * Called with the PU-wn, a global-variable st to annotate, and
 * a stack of alternate-entries in the PU.
 *
 * Generate a call upon entry to the library routine describing this
 * global array. 
 *      __memctr_register_global (address, size, name)
 *
 ***********************************************************************/
static void MemCtr_Add_Global (WN* pu, ST* st, STACK_OF_WN *altentry_stack) {
  if (MemCtr_Criteria_Check(st)) {

    // create the call
    OPCODE callop = OPCODE_make_op (OPR_CALL, MTYPE_V, MTYPE_V);
    WN* call_wn = WN_Create (callop, 3);
    WN_st_idx(call_wn) = ST_st_idx(reg_st_global);

    // parm 1: address of ST
    OPCODE lda_op = OPCODE_make_op (OPR_LDA, Pointer_type, MTYPE_V);
    TY_IDX ptr_ty = Make_Pointer_Type (ST_type(st), Is_Global_Symbol(st));
    WN* parm_wn = WN_CreateLda (lda_op, 0, ptr_ty, st);
    parm_wn = WN_CreateParm (Pointer_type,
                             parm_wn,
                             ptr_ty,
                             WN_PARM_BY_VALUE);
    WN_kid0(call_wn) = parm_wn;
    
    // parm 2: size of ST
    OPCODE const_op = OPCODE_make_op(OPR_INTCONST, MTYPE_I8, MTYPE_V);
    parm_wn = WN_CreateIntconst (const_op, TY_size(ST_type(st)));
    parm_wn = WN_CreateParm (MTYPE_I8,
                             parm_wn,
                             Be_Type_Tbl(MTYPE_I8),
                             WN_PARM_BY_VALUE);
    WN_kid1(call_wn) = parm_wn;

    // parm 3: name of ST
    char* var_name = ST_name(st);
    TCON tcon = Host_To_Targ_String (MTYPE_STRING,var_name,strlen(var_name)+1);
    TY_IDX string_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1), TRUE);
    ST *string_st = Gen_String_Sym (&tcon, Be_Type_Tbl(MTYPE_STRING), FALSE);
    parm_wn = WN_CreateLda (lda_op, 0, string_ty, string_st);
    parm_wn = WN_CreateParm (Pointer_type,
                             parm_wn,
                             Be_Type_Tbl(Pointer_type),
                             WN_PARM_BY_REFERENCE);
    WN_kid2(call_wn) = parm_wn;

    Set_Call_Side_Effects (call_wn);
    Parentize(call_wn);

    Insert_Call (call_wn, pu, altentry_stack);
  }
}

/***********************************************************************
 *
 * Called with the PU-wn, a COMMON st to annotate, and
 * a stack of alternate-entries in the PU.
 *
 * Generate a call upon entry to the library routine describing this
 * COMMON array. 
 *      __memctr_register_common (address, size, common-name, name)
 *
 ***********************************************************************/
static void MemCtr_Add_Common (WN *pu, ST *st, STACK_OF_WN *altentry_stack) {

  if (MemCtr_Criteria_Check(st)) {

    // create the call
    OPCODE callop = OPCODE_make_op (OPR_CALL, MTYPE_V, MTYPE_V);
    WN* call_wn = WN_Create (callop, 4);
    WN_st_idx(call_wn) = ST_st_idx(reg_st_common);

    // parm 1: address of ST
    OPCODE lda_op = OPCODE_make_op (OPR_LDA, Pointer_type, MTYPE_V);
    TY_IDX ptr_ty = Make_Pointer_Type (ST_type(st), Is_Global_Symbol(st));

    WN* parm_wn = WN_CreateLda (lda_op, 0, ptr_ty, st);
    parm_wn = WN_CreateParm (Pointer_type,
                             parm_wn,
                             ptr_ty,
                             WN_PARM_BY_VALUE);
    WN_kid0(call_wn) = parm_wn;
    
    // parm 2: size of ST
    OPCODE const_op = OPCODE_make_op(OPR_INTCONST, MTYPE_I8, MTYPE_V);
    parm_wn = WN_CreateIntconst (const_op, TY_size(ST_type(st)));
    parm_wn = WN_CreateParm (MTYPE_I8,
                             parm_wn,
                             Be_Type_Tbl(MTYPE_I8),
                             WN_PARM_BY_VALUE);
    WN_kid1(call_wn) = parm_wn;

    // parm 3: name of common block
    char* var_name = ST_name(ST_base(st));
    TCON tcon = Host_To_Targ_String (MTYPE_STRING,var_name,strlen(var_name)+1);
    TY_IDX string_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1), TRUE);
    ST *string_st = Gen_String_Sym (&tcon, Be_Type_Tbl(MTYPE_STRING), FALSE);
    parm_wn = WN_CreateLda (lda_op, 0, string_ty, string_st);
    parm_wn = WN_CreateParm (Pointer_type,
                             parm_wn,
                             Be_Type_Tbl(Pointer_type),
                             WN_PARM_BY_REFERENCE);
    WN_kid2(call_wn) = parm_wn;

    // parm 4: name of ST
    var_name = ST_name(st);
    tcon = Host_To_Targ_String (MTYPE_STRING,var_name,strlen(var_name)+1);
    string_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1), TRUE);
    string_st = Gen_String_Sym (&tcon, Be_Type_Tbl(MTYPE_STRING), FALSE);
    parm_wn = WN_CreateLda (lda_op, 0, string_ty, string_st);
    parm_wn = WN_CreateParm (Pointer_type,
                             parm_wn,
                             Be_Type_Tbl(Pointer_type),
                             WN_PARM_BY_REFERENCE);
    WN_kid3(call_wn) = parm_wn;

    Set_Call_Side_Effects (call_wn);
    Parentize(call_wn);

    Insert_Call (call_wn, pu, altentry_stack);
  }
}

/***********************************************************************
 *
 * Called with the PU-wn, a local st to annotate, and
 * a stack of alternate-entries in the PU.
 *
 * Generate a call upon entry to the library routine describing this
 * local array. 
 *      __memctr_register_local (address, size, func-name, name)
 *
 * and upon each return point to
 *      __memctr_unregister_local (address, size, func-name, name)
 *
 ***********************************************************************/
static void MemCtr_Add_Local  (WN* pu, ST* st,
                               STACK_OF_WN *altentry_stack,
                               STACK_OF_WN *exit_stack) {

  if (MemCtr_Criteria_Check(st) || Is_VLA(st)) {

    // create the call
    OPCODE callop = OPCODE_make_op (OPR_CALL, MTYPE_V, MTYPE_V);
    WN* call_wn = WN_Create (callop, 4);
    WN_st_idx(call_wn) = ST_st_idx(reg_st_local);

    // parm 1: address of ST
    WN *parm_wn;
    TY_IDX ptr_ty;
    OPCODE lda_op = OPCODE_make_op (OPR_LDA, Pointer_type, MTYPE_V);
    if (Is_VLA(st)) {
      OPCODE ldid_op = OPCODE_make_op (OPR_LDID, Pointer_type, Pointer_type);
      ptr_ty = ST_type(st);
      parm_wn = WN_CreateLdid (ldid_op, 0, st, ptr_ty);
    }
    else {
      ptr_ty = Make_Pointer_Type (ST_type(st), Is_Global_Symbol(st));

      parm_wn = WN_CreateLda (lda_op, 0, ptr_ty, st);
    }
    parm_wn = WN_CreateParm (Pointer_type,
                             parm_wn,
                             ptr_ty,
                             WN_PARM_BY_VALUE);
    WN_kid0(call_wn) = parm_wn;
    
    // parm 2: size of ST
    if (Is_VLA(st)) {

      // find alloca
      WN *alloca_wn = WN_first(WN_func_body(pu));
      while (alloca_wn) {
        if (WN_operator(alloca_wn) == OPR_PRAGMA &&
            WN_pragma(alloca_wn) == WN_PRAGMA_PREAMBLE_END) {
          DevWarn ("Reached end of preamble w/o finding alloca of %s\n",
                   ST_name(st));
        }
        if ((WN_operator(alloca_wn) == OPR_STID) &&
            (WN_st_idx(alloca_wn) == ST_st_idx(st)))
          break;
        alloca_wn = WN_next(alloca_wn);
      }
      FmtAssert (alloca_wn,
                 ("Unable to find alloca of %s\n", ST_name(st)));
      alloca_wn = WN_prev(alloca_wn);
      Is_True (alloca_wn &&
               WN_operator(alloca_wn) == OPR_INTRINSIC_CALL &&
               (WN_intrinsic(alloca_wn) == INTRN_U4I4ALLOCA ||
                WN_intrinsic(alloca_wn) == INTRN_U8I8ALLOCA),
               ("Expected to find an alloca for %s in preamble",
                ST_name(st)));
      parm_wn = WN_COPY_Tree (WN_kid0(WN_kid0(alloca_wn)));
      Parentize(parm_wn);
    }
    else {
      OPCODE const_op = OPCODE_make_op(OPR_INTCONST, MTYPE_I8, MTYPE_V);
      parm_wn = WN_CreateIntconst (const_op, TY_size(ST_type(st)));
    }
    parm_wn = WN_CreateParm (MTYPE_I8,
                             parm_wn,
                             Be_Type_Tbl(MTYPE_I8),
                             WN_PARM_BY_VALUE);
    WN_kid1(call_wn) = parm_wn;

    // parm 3: name of function
    char* var_name = ST_name(WN_st(pu));
    TCON tcon = Host_To_Targ_String (MTYPE_STRING,var_name,strlen(var_name)+1);
    TY_IDX string_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1), TRUE);
    ST *string_st = Gen_String_Sym (&tcon, Be_Type_Tbl(MTYPE_STRING), FALSE);
    parm_wn = WN_CreateLda (lda_op, 0, string_ty, string_st);
    parm_wn = WN_CreateParm (Pointer_type,
                             parm_wn,
                             Be_Type_Tbl(Pointer_type),
                             WN_PARM_BY_REFERENCE);
    WN_kid2(call_wn) = parm_wn;

    // parm 4: name of ST
    var_name = ST_name(st);
    tcon = Host_To_Targ_String (MTYPE_STRING,var_name,strlen(var_name)+1);
    string_ty = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1), TRUE);
    string_st = Gen_String_Sym (&tcon, Be_Type_Tbl(MTYPE_STRING), FALSE);
    parm_wn = WN_CreateLda (lda_op, 0, string_ty, string_st);
    parm_wn = WN_CreateParm (Pointer_type,
                             parm_wn,
                             Be_Type_Tbl(Pointer_type),
                             WN_PARM_BY_REFERENCE);
    WN_kid3(call_wn) = parm_wn;

    Set_Call_Side_Effects (call_wn);
    Parentize(call_wn);

    Insert_Call (call_wn, pu, altentry_stack);

    // now unregister at each exit point
    // __memctr_unregister_local (address, size, func-name, name)
    for (INT i=0; i<exit_stack->Elements(); i++) {
      WN* insert_wn = exit_stack->Bottom_nth(i);
      call_wn = WN_COPY_Tree (call_wn);
      WN_st_idx(call_wn) = ST_st_idx(unreg_st_local);
      WN_INSERT_BlockBefore (Get_Parent(insert_wn), insert_wn, call_wn);
      Set_Parent(call_wn, Get_Parent(insert_wn));
    }
  }
}

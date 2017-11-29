/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */
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

#ifndef opt_proactive_INCLUDED
#include "opt_proactive.h"
#endif
#ifndef opt_htable_INCLUDED
#include "opt_htable.h"
#endif
#ifndef opt_cfg_INCLUDED
#include "opt_cfg.h"
#endif
#ifndef config_opt_INCLUDED
#include "config_opt.h"
#endif

using std::map;
using std::set;

// For every pair of WHILR nodes in the WHIRL tree rooted
// at wn1 and wn2, check whether operators are identical.
// If the node is a constant, check whether constant value
// is identical.

static BOOL
Has_same_shape(WN * wn1, WN * wn2)
{
  if (WN_operator(wn1) != WN_operator(wn2))
    return FALSE;

  switch (WN_operator(wn1)) {
  case OPR_INTCONST:
    if (WN_const_val(wn1) != WN_const_val(wn2))
      return FALSE;

    break;

  default:
    ;
  }

  if (WN_kid_count(wn1) != WN_kid_count(wn2))
    return FALSE;

  for (int i = 0; i < WN_kid_count(wn1); i++) {
    if (!Has_same_shape(WN_kid(wn1,i), WN_kid(wn2,i)))
      return FALSE;
  }

  return TRUE;
}

// Create and return a temporary 'mtype' array of 'size'
ST *
CFG_TRANS::Tmp_array_st(MTYPE mtype, int size)
{
  TY_IDX arr_ty_idx;
  TY_IDX ele_ty_idx = MTYPE_To_TY(mtype);
  TY & ty = New_TY(arr_ty_idx);
  TY_Init (ty, size * TY_size(ele_ty_idx), KIND_ARRAY, MTYPE_UNKNOWN, Save_Str("_local_temp_array"));
  
  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb, 0, size - 1, TY_size(ele_ty_idx));
  Set_ARB_dimension(arb,1);
  Set_ARB_first_dimen(arb);
  Set_ARB_last_dimen(arb);
  
  Set_TY_arb(ty, arb);
  Set_TY_align(arr_ty_idx, TY_size(ele_ty_idx));
  Set_TY_etype(ty, ele_ty_idx);
  
  ST * st_new = New_ST(CURRENT_SYMTAB);
  ST_Init (st_new,
           Save_Str("$local_temp_array"),
           CLASS_VAR,
           SCLASS_AUTO,
           EXPORT_LOCAL,
           arr_ty_idx);
  
  Set_ST_is_temp_var(st_new);
  Set_ST_pt_to_unique_mem(st_new);
  Set_ST_pt_to_compiler_generated_mem(st_new);
  return st_new;
}

// Create a 1-dimentional load of the array reference to
// the element whose index is given in 'wn_index'.
WN *
CFG_TRANS::Create_array_load(ST * st, WN * wn_index)
{
  TY_IDX ty_idx = ST_type(*st);
  FmtAssert((TY_kind(ty_idx) == KIND_ARRAY), ("Expect an array."));
  ARB_HANDLE arb = TY_arb(ty_idx);
  FmtAssert((ARB_dimension(arb) == 1),  ("Expect 1 dimension."));

  TY_IDX element_ty_idx = TY_etype(ty_idx);
  TYPE_ID element_type_id = TY_mtype(element_ty_idx);
  UINT64 element_size = TY_size(element_ty_idx);
  UINT64 element_count = TY_size(ty_idx) / element_size;
  TY_IDX ty_ptr = Make_Pointer_Type(element_ty_idx);
  TY_IDX arr_ty_ptr = Make_Pointer_Type(ty_idx);
  OPCODE op_lda = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  WN* wn_lda = WN_CreateLda(op_lda, 0, arr_ty_ptr, st);
  WN * wn_size = WN_Intconst(element_type_id, element_count);
  OPCODE op_array = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
  WN* wn_array = WN_Create(op_array, 3);
  WN_element_size(wn_array) = element_size;
  WN_array_base(wn_array) = wn_lda;
  WN_array_index(wn_array, 0) = wn_index;
  WN_array_dim(wn_array, 0) = wn_size;

  OPCODE op_iload = OPCODE_make_op(OPR_ILOAD, element_type_id, element_type_id);
  WN * wn_iload = WN_CreateIload(op_iload, 0, element_ty_idx, ty_ptr, wn_array);
  
  Create_lda_array_alias(_cu->Alias_mgr(), wn_lda, wn_iload);
  
  if (OPERATOR_has_aux(OPR_LDA)) {
    _cu->Opt_stab()->Count_syms(wn_lda);
    AUX_ID idx = _cu->Opt_stab()->Enter_symbol(OPR_LDA, st, WN_offset(wn_lda), 
					       WN_object_ty(wn_lda), FALSE, wn_lda);
    WN_set_aux(wn_lda, idx);

    POINTS_TO * pt = _cu->Opt_stab()->Aux_stab_entry(idx)->Points_to();
    pt->Set_no_alias();
    pt->Set_expr_kind(EXPR_IS_ANY);
  }

  OPT_STAB * opt_stab = _cu->Opt_stab();
  AUX_ID tmp_preg = opt_stab->Create_preg(TY_mtype(element_ty_idx));
  ST * st_preg = opt_stab->Aux_stab_entry(tmp_preg)->St();
  WN * wn_store = WN_CreateStid(OPR_STID, MTYPE_V, element_type_id,
				opt_stab->St_ofst(tmp_preg), st_preg,
				element_ty_idx, wn_iload, 0);
  Create_alias(_cu->Alias_mgr(), wn_store);
  WN_set_aux(wn_store, tmp_preg);
  
  return wn_store;
}

// Create a 1-dimentional store of 'wn_val' to the array reference to
// the element whose index is given in 'wn_index'.
WN *
CFG_TRANS::Create_array_store(ST * st, WN * wn_index, WN * wn_val)
{
  TY_IDX ty_idx = ST_type(*st);
  TY_IDX element_ty_idx = TY_etype(ty_idx);
  TYPE_ID element_type_id = TY_mtype(element_ty_idx);
  FmtAssert((element_type_id == WN_rtype(wn_val)), ("Type of store value does not match"));
  UINT64 element_size = TY_size(element_ty_idx);
  UINT64 element_count = TY_size(ty_idx) / element_size;
  TY_IDX ty_ptr = Make_Pointer_Type(element_ty_idx);
  TY_IDX arr_ty_ptr = Make_Pointer_Type(ty_idx);
  OPCODE op_lda = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  WN* wn_lda = WN_CreateLda(op_lda, 0, arr_ty_ptr, st);
  WN * wn_size = WN_Intconst(element_type_id, element_count);
  OPCODE op_array = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
  WN* wn_array = WN_Create(op_array, 3);
  WN_element_size(wn_array) = element_size;
  WN_array_base(wn_array) = wn_lda;
  WN_array_index(wn_array, 0) = wn_index;
  WN_array_dim(wn_array, 0) = wn_size;
  OPCODE op_istore = OPCODE_make_op(OPR_ISTORE, MTYPE_V, element_type_id);
  WN * wn_istore = WN_CreateIstore(op_istore, 0, ty_ptr, wn_val, wn_array);

  Create_lda_array_alias(_cu->Alias_mgr(), wn_lda, wn_istore);

  if (OPERATOR_has_aux(OPR_LDA)) {
    _cu->Opt_stab()->Count_syms(wn_lda);
    AUX_ID idx = _cu->Opt_stab()->Enter_symbol(OPR_LDA, st, WN_offset(wn_lda), 
					       WN_object_ty(wn_lda), FALSE, wn_lda);
    WN_set_aux(wn_lda, idx);

    POINTS_TO * pt = _cu->Opt_stab()->Aux_stab_entry(idx)->Points_to();
    pt->Set_no_alias();
    pt->Set_expr_kind(EXPR_IS_ANY);
  }

  return wn_istore;
}

// Get upper bound of 'sc'.
WN *
CFG_TRANS::Get_upper_bound(SC_NODE * sc)
{
  WN * low = NULL;
  WN * high = NULL;
  WN * step = NULL;
  
  if (!sc->Get_bounds(&low, &high, &step))
    return NULL;

  WN * ub = Get_upper_bound(sc, WN_kid0(high));
  return ub;
}

// Get upper bound of 'sc' from 'wn' which is a comparison expression.
WN *
CFG_TRANS::Get_upper_bound(SC_NODE * sc, WN * wn)
{
  WN * index = Get_index_load(sc);
  OPERATOR opr = WN_operator(wn);
  WN * op1;
  WN * op2;
  STACK<WN *> * l_stk = CXX_NEW(STACK<WN *> (_pool), _pool);
  STACK<WN *> * r_stk = CXX_NEW(STACK<WN *> (_pool), _pool);
  int r_const = 0;

  // Conceptually normalize condition expressions to  "<" comparisons, 
  // collect left-hand side of "<" in "l_stk", and right-hand side of "<"
  // in "r_stk".
  switch (opr) {
  case OPR_GT:
  case OPR_GE:
    op1 = WN_kid0(wn);
    op2 = WN_kid1(wn);
    
    if (opr == OPR_GE)
      r_const = 1;

    opr = WN_operator(op1);
    if ((opr != OPR_ADD) && (opr != OPR_SUB))
      r_stk->Push(op1);
    else 
      Collect_operands(op1, r_stk, l_stk);

    opr = WN_operator(op2);
    if ((opr != OPR_ADD) && (opr != OPR_SUB))
      l_stk->Push(op2);
    else
      Collect_operands(op2, l_stk, r_stk);

    break;

  case OPR_LE:
  case OPR_LT:
    
    if (opr == OPR_LE)
      r_const = 1;

    op1 = WN_kid0(wn);
    op2 = WN_kid1(wn);
    
    opr = WN_operator(op1);
    if ((opr != OPR_ADD) && (opr != OPR_SUB))
	l_stk->Push(op1);
    else 
      Collect_operands(op1, l_stk, r_stk);

    opr = WN_operator(op2);

    if ((opr != OPR_ADD) && (opr != OPR_SUB))
      r_stk->Push(op2);
    else
      Collect_operands(op2, r_stk, l_stk);

    break;
  default:
    ;
  }

  // Check left-hand side of a "<" compare.
  int count = 0;
  WN * wn_tmp;
  while (!l_stk->Is_Empty()) {
    wn_tmp = l_stk->Pop();
    opr = WN_operator(wn_tmp);

    if (opr == OPR_INTCONST)
      r_const -= WN_const_val(wn_tmp);
    else if (WN_Simp_Compare_Trees(index, wn_tmp) == 0)
      count++;
    else {
      CXX_DELETE(l_stk, _pool);
      CXX_DELETE(r_stk,_pool);
      return NULL;
    }
  }
  
  if (count != 1) {
    CXX_DELETE(l_stk, _pool);
    CXX_DELETE(r_stk,_pool);
    return NULL;
  }

  WN * wn_last = NULL;

  while (!r_stk->Is_Empty()) {
    wn_tmp = r_stk->Pop();
    opr = WN_operator(wn_tmp);
    
    if (opr == OPR_INTCONST)
      r_const += WN_const_val(wn_tmp);
    else if (!Is_invariant(sc, wn_tmp, 0)) {
      CXX_DELETE(l_stk, _pool);
      CXX_DELETE(r_stk,_pool);
      return NULL;
    }
    else if (!wn_last) {
      wn_last = WN_COPY_Tree_With_Map(wn_tmp);
    }
    else 
      wn_last = WN_CreateExp2(OPR_ADD, MTYPE_I4, MTYPE_V, wn_last, 
			      WN_COPY_Tree_With_Map(wn_tmp));
  }
  
  wn_tmp = NULL;

  r_const--;

  if (r_const) 
    wn_tmp = WN_CreateIntconst(OPR_INTCONST, (r_const > 0) ? MTYPE_U4 : MTYPE_I4, MTYPE_V, r_const);
  
  if (!wn_last) 
    wn_last = wn_tmp;
  else if (wn_tmp)
    wn_last = WN_CreateExp2(OPR_ADD, MTYPE_I4, MTYPE_V, wn_last, wn_tmp);
  
  CXX_DELETE(l_stk, _pool);
  CXX_DELETE(r_stk,_pool);
  
  return wn_last;
}

// Query whether 'sc1' and 'sc2' have the same trip count.
BOOL
CFG_TRANS::Have_same_trip_count(SC_NODE * sc1, SC_NODE * sc2)
{
  WN * low1 = NULL;
  WN * low2 = NULL;
  WN * high1 = NULL;
  WN * high2 = NULL;
  WN * step1 = NULL;
  WN * step2 = NULL;
  
  if (sc1->Get_bounds(&low1, &high1, &step1)
      && sc2->Get_bounds(&low2, &high2, &step2)) {
    if (!Has_same_shape(low1, low2)
	|| !Has_same_shape(step1, step2))
      return FALSE;

    if (Has_same_shape(high1, high2))
      return TRUE;

    OPERATOR opr = WN_operator(high1);
    if ((opr == OPR_FALSEBR)
	&& (opr == WN_operator(high2))) {
      high1 = WN_kid0(high1);
      high2 = WN_kid0(high2);
      WN * ub1 = Get_upper_bound(sc1, high1);
      WN * ub2 = Get_upper_bound(sc2, high2);
      if (ub1 && ub2) {
	if (WN_Simp_Compare_Trees(ub1, ub2) == 0) {
	  WN_Delete(ub1);
	  WN_Delete(ub2);
	  return TRUE;
	}
      }
      if (ub1)
	WN_Delete(ub1);
      if (ub2)
	WN_Delete(ub2);
    }
  }

  return FALSE;
}

// reset/clear fields.
void
IF_MERGE_TRANS::Clear(void)
{
  CFG_TRANS::Clear();
  _action = DO_NONE;
  _pass = PASS_NONE;
  _region_id = 0;
}

// Query whether given aux_id represents a scalar non-address-taken non-virtual variable.

BOOL CFG_TRANS::Is_trackable_var(AUX_ID aux_id)
{
  OPT_STAB * op_stab = _cu->Opt_stab();

  if (aux_id && (aux_id <= op_stab->Lastidx())) {
    AUX_STAB_ENTRY * aux_entry = op_stab->Aux_stab_entry(aux_id);
    if (aux_entry && (aux_entry->Stype() == VT_NO_LDA_SCALAR)) {
      ST * st = aux_entry->St();

      if (st && (ST_class(st) == CLASS_VAR) 
	  && !ST_addr_passed(st)
	  && !ST_addr_saved(st))
	return TRUE;
    }
  }
  return FALSE;
}

// For every load in the expression tree rooted at the given wn, check whether
// it loads a scalar trackable variable.  See CFG_TRANS::Is_trackable_var.

BOOL CFG_TRANS::Is_trackable_expr(WN * wn)
{
  int i;
  OPCODE opc = WN_opcode(wn);
  if (!OPCODE_is_expression(opc))
    return FALSE;

  if ((opc == OPC_IO) || OPCODE_is_call(opc)) 
    return FALSE;

  if (OPCODE_is_load(opc)) {
    if (OPERATOR_is_scalar_load(WN_operator(wn))) {
      AUX_ID aux_id = WN_aux(wn);
      if (!Is_trackable_var(aux_id)) 
	return FALSE;
    }
    else
      return FALSE;
  }

  for ( i = 0; i < WN_kid_count(wn); i++) 
    if (!Is_trackable_expr(WN_kid(wn,i)))
      return FALSE;

  return TRUE;
}


// Query whether the values of loads in the given expression tree 
// are modified by the given SC_NODE. 'do_recursive' indicates whether to 
// recursively track the values among children of 'sc'.
// 'eval_true' indicates whether wn can be evaluated to TRUE at the entry
// of 'sc'.
// We perform a quick and simple value-number hashing to the given SC_NODE. 

BOOL CFG_TRANS::Val_mod(SC_NODE * sc, WN * wn, BOOL eval_true, BOOL do_recursive)
{
  BOOL ret_val = TRUE;

  OPT_POOL_Push(_pool, MEM_DUMP_FLAG + 1);
  Init_val_map(wn);

  if (eval_true)
    Infer_non_zero(wn, TRUE);

  SC_TYPE type = sc->Type();
  if (type == SC_THEN) {
    SC_NODE * sc_if = sc->Get_real_parent();
    WN * wn_cond = sc_if->Get_cond();
    if (wn_cond)
      Infer_non_zero(wn_cond, TRUE);
  }
  else if (type == SC_ELSE) {
    SC_NODE * sc_if = sc->Get_real_parent();
    WN * wn_cond = sc_if->Get_cond();
    if (wn_cond)
      Infer_non_zero(wn_cond, FALSE);
  }

  if (!_val_map.empty()) {
    Track_val(sc, sc->First_bb(), wn, do_recursive);
    ret_val = !Val_match(wn);
  }

  Delete_val_map();
  OPT_POOL_Pop(_pool, MEM_DUMP_FLAG + 1);
  return ret_val;
}


// Query whether the values of loads in the given expression tree 
// are modified by the given SC_NODE and its children. 
// 'eval_true' indicates whether wn can be evaluated to TRUE at the entry
// of 'sc'.

BOOL CFG_TRANS::Val_mod(SC_NODE * sc, WN * wn, BOOL eval_true)
{
  if (!Val_mod(sc, wn, eval_true, TRUE))
    return FALSE;
  
  if (!Val_mod(sc, wn, eval_true, FALSE)) {
    SC_LIST * kids = sc->Kids();
    if (kids != NULL) {
      SC_LIST_ITER sc_list_iter(kids);
      SC_NODE *tmp;
      FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
	if (Val_mod(tmp, wn, FALSE))
	  return TRUE;
      }
    }
    return FALSE;
  }

  return TRUE;
}

// Query whether the values of loads in the given expression tree match
// values hashed in _val_map.
BOOL CFG_TRANS::Val_match(WN * wn)
{
  if (OPERATOR_is_scalar_load(WN_operator(wn))) {
    AUX_ID aux_id = WN_aux(wn);
    AUX_ID val = (AUX_ID) Get_val(aux_id);
    if (val != aux_id)
      return FALSE;
  }

  for (int i = 0; i < WN_kid_count(wn); i++)
    if (!Val_match(WN_kid(wn,i)))
      return FALSE;

  return TRUE;
}

// Free storage of _val_map.
void
CFG_TRANS::Delete_val_map()
{
  _val_map.clear();

  if (_true_val)
    _true_val = NULL;
}

// Given a condition expression 'wn', infer whether there is a load in the expression 
// that has an non-zero value. 'eval_true' gives the value of 'wn' which is a TRUE or a FALSE.
void
CFG_TRANS::Infer_non_zero(WN * wn, BOOL eval_true)
{
  OPERATOR op = WN_operator(wn);
  if (((op == OPR_NE) || (op == OPR_EQ))
      && (WN_operator(WN_kid1(wn)) == OPR_INTCONST)
      && OPERATOR_is_scalar_load(WN_operator(WN_kid0(wn)))) {
    AUX_ID aux_id = WN_aux(WN_kid0(wn));
    INT64 const_val = WN_const_val(WN_kid1(wn));
    BOOL is_non_zero = FALSE;

    if (eval_true) {
      if (((op == OPR_NE) && (const_val == 0))
	  || ((op == OPR_EQ) && (const_val != 0)))
	is_non_zero = TRUE;
    }

    if (is_non_zero) {
      if (_true_val == NULL) 
	_true_val = BS_Create_Empty(_cu->Opt_stab()->Lastidx() + 1, _pool);
      
      _true_val = BS_Union1D(_true_val, aux_id, _pool);
    } 
  } 
}

// Initialize _val_map by hashing all loads in the given wn.
void
CFG_TRANS::Init_val_map(WN * wn)
{
  if (OPERATOR_is_scalar_load(WN_operator(wn))) {
    AUX_ID aux_id = WN_aux(wn);
    if (aux_id) {
      _val_map[aux_id] = aux_id;
    }
  }

  for ( int i = 0; i < WN_kid_count(wn); i++) {
    Init_val_map(WN_kid(wn, i));
  }
}

// Delete _unlink_sc.
void
CFG_TRANS::Delete_unlink_sc()
{
  while (!_unlink_sc->Is_Empty()) {
    SC_NODE * sc_tmp = _unlink_sc->Pop();
    sc_tmp->Delete();
  }
  CXX_DELETE(_unlink_sc, _pool);
  _unlink_sc = NULL;

}

// Obtain the hashed value number for the given AUX_ID.
AUX_ID
CFG_TRANS::Get_val(AUX_ID aux_id)
{
  AUX_ID val = 0;
  if (aux_id)
    val = _val_map[aux_id];
  return val;
}

// Hash aux_id to val.
void CFG_TRANS::Set_val(AUX_ID aux_id, AUX_ID val)
{
  if (aux_id) 
    _val_map[aux_id] = val;
}

// Interface to invoke alias info queries.
BOOL CFG_TRANS::Is_aliased(WN * wn1, WN * wn2)
{
  ALIAS_MANAGER * alias_mgr = _cu->Alias_mgr();
  WN * call_wn = NULL;
  WN * load_wn = NULL;
  WN * store_wn = NULL;
  TY_ITER i ;
  ST *st;
  int ii;
  int par_match_found = 0;
  int is_mod = 1;

  /* If wn1 or wn2 is ICALL we get the parameter list and compare
   * in global symbol table for the function with same
   * parameter list and check for those function if
   * function modifies the pointer.
   */
  if((WN_operator(wn1) == OPR_ICALL)||(WN_operator(wn2) == OPR_ICALL))
  {
    TY_IDX ty_idx;

    if(WN_operator(wn1) == OPR_ICALL)
      ty_idx  = WN_ty(wn1);
    else
      ty_idx  = WN_ty(wn2);

    TY &ty = Ty_Table[ty_idx];
    FOREACH_SYMBOL (GLOBAL_SYMTAB, st, ii) {
      if(ST_sym_class(st) ==  CLASS_FUNC){
        ty_idx = PU_prototype (Pu_Table[ST_pu(st)]);
        const TY& ty1 = Ty_Table [ty_idx];
        char* st_name = ST_name(st);
        if (TY_kind(ty1) == KIND_FUNCTION)
        {
          TYLIST_IDX idx1 = TY_tylist(ty1);
          TYLIST_IDX idx2 = TY_tylist(ty);
          /* Traverse the parameter list for icall and function in the
           * symbol table to check if they match
           */
          while (  (Tylist_Table[idx1] != 0) || (Tylist_Table[idx2] != 0)) {

            const TY& parm_x1 = Ty_Table[Tylist_Table[idx1]];
            const char *name1 =
                  TY_name_idx (parm_x1) == 0 ? "(anon)" : TY_name (parm_x1);
            const TY& parm_x2 = Ty_Table[Tylist_Table[idx2]];
            const char *name2 =
                  TY_name_idx (parm_x2) == 0 ? "(anon)" : TY_name (parm_x2);
            if((strcmp(name1,name2) == 0) &&
               (TY_align(Tylist_Table[idx2]) == TY_align(Tylist_Table[idx1])))
            {

              par_match_found = 1;
            }
            else{

              par_match_found  = 0;
              break;
            }
            idx1++;
            idx2++;
         }
         if( ( Tylist_Table[idx1] != 0) ||  ( Tylist_Table[idx2] != 0))
           par_match_found = 0;

         if((Tylist_Table[idx1] == 0) &&  (Tylist_Table[idx2] == 0) &&
             (par_match_found ==1))
         {
           INT mod = 0;
           INT ref = 0;
           WN  *ld_wn = NULL;
           if(WN_operator(wn1) == OPR_ICALL)
           {
             if (OPERATOR_is_scalar_load(WN_operator(wn2)))
               ld_wn = wn2;
           }
           if(WN_operator(wn2) == OPR_ICALL)
           {
             if (OPERATOR_is_scalar_load(WN_operator(wn1)))
               ld_wn = wn1;
           }
           if (ld_wn){

             ST *ld_st= NULL;
             INT mod = 0;
             INT ref = 0;
             AUX_ID load_aux = WN_aux(ld_wn);
             OPT_STAB * opt_stab = _cu->Opt_stab();
             if (load_aux && (load_aux <= opt_stab->Lastidx()))
               ld_st = opt_stab->Aux_stab_entry(load_aux)->St();
             if (Is_trackable_var(load_aux)) {
               opt_stab->check_ipa_mod_ref_info(st, ld_st, &mod, &ref);
               if (mod == 0)
               {
                  is_mod = 0;
               }
             }
             else if (ld_st && (ST_class(ld_st) == CLASS_PREG))
             {
               return FALSE;
             }
           }
         }
       }
     }
   }
   if(!is_mod) return FALSE;
  }

  if (WN_operator(wn1) == OPR_CALL)
    call_wn = wn1;
  else if (WN_operator(wn2) == OPR_CALL)
    call_wn = wn2;

  if (OPERATOR_is_scalar_load(WN_operator(wn1)))
    load_wn = wn1;
  else if (OPERATOR_is_scalar_load(WN_operator(wn2)))
    load_wn = wn2;

  if (OPERATOR_is_scalar_store(WN_operator(wn1)))
    store_wn = wn1;
  else if (OPERATOR_is_scalar_store(WN_operator(wn2)))
    store_wn = wn2;

  if (Aliased(alias_mgr, wn1, wn2) != NOT_ALIASED) {
    OPT_STAB * opt_stab = _cu->Opt_stab();

    if (call_wn && load_wn) {
      AUX_ID load_aux = WN_aux(load_wn);
      ST * load_st = NULL;

      if (load_aux && (load_aux <= opt_stab->Lastidx()))
	load_st = opt_stab->Aux_stab_entry(load_aux)->St();

      if (Is_trackable_var(load_aux)) {
	ST * call_st = WN_sym(call_wn);
	INT mod = 0;
	INT ref = 0;
      
	opt_stab->check_ipa_mod_ref_info(call_st, load_st, &mod, &ref);
	if (mod == 0)
	  return FALSE;

	if (Is_Global_Symbol(load_st)
	    && _true_val
	    && BS_MemberP(_true_val, load_aux)) {
	  INT same_entry_exit_value_or_1 = 0;	  
	  opt_stab->check_ipa_same_entry_exit_value_or_1_info(call_st, load_st,
							      &same_entry_exit_value_or_1);
	  if (same_entry_exit_value_or_1) 
	    return FALSE;
	}
      }
      else if (load_st && (ST_class(load_st) == CLASS_PREG))
	return FALSE;
    }
    else if (call_wn && store_wn) {
      AUX_ID store_aux = WN_aux(store_wn);

      if (store_aux && (store_aux <= opt_stab->Lastidx())) {
	ST * store_st = opt_stab->Aux_stab_entry(store_aux)->St();
	if (store_st && (ST_class(store_st) == CLASS_PREG)
	    && (WN_operator(WN_kid(store_wn, 0)) == OPR_INTCONST)) {
	  // Store of a constant to a preg does not alias with a call.
	  return FALSE;
	}
      }
    }

    return TRUE;
  }

  return FALSE;
}

// Query whether 'sc' contains a single killing def. 
// Input: 'stk' gives a stack of SC_NODEs that have been found to contain a single killing def.
//        'loop' gives a nesting loop to check loop invariants.
//
//  A SC_NODE contains a single killing def iif:
// (1) If the node is a SC_BLOCK, it contains a single assignment whose RHS is a loop invariant w.r.t
//     the given 'loop'.
// (2) If the node is a SC_IF, both the then-path and the else-path satisfies (1).  The inputs of
//     its if-condition are either loop index or loop invariant w.r.t the given 'loop', and the node
//     is at a control dominating point in 'loop'.
// (3) If the node is a SC_IF with an empty then-path or else-path, there must exist a killing def to 
//     the same variable on 'stk'.

BOOL CFG_TRANS::Is_kill(SC_NODE * sc, STACK<SC_NODE *> * stk, SC_NODE * loop)
{
  SC_TYPE type = sc->Type();
  SC_NODE * sc_body = loop->Find_kid_of_type(SC_LP_BODY);
  SC_NODE * sc_parent = sc->Parent();
  int count;
  WN * wn;

  if (type == SC_BLOCK) {
    count = sc->Executable_stmt_count();
    if (count != 1)
      return FALSE;
    wn = sc->First_executable_stmt();
    if (!OPERATOR_is_scalar_store(WN_operator(wn)))
      return FALSE;
    AUX_ID aux = WN_aux(wn);
    wn = WN_kid0(wn);
    if (!Is_invariant(loop, wn, 0)) {
      // Allow recursive reference to earlier killing definitions.
      if (Is_invariant(loop, wn, aux)) {
	BOOL found = FALSE;
	for (int i = 0; i < stk->Elements(); i++) {
	  SC_NODE * sc_iter = stk->Top_nth(i);
	  if (Has_dependency(sc_iter, sc)) {
	    found = TRUE;
	    break;
	  }
	}
	if (found) 
	  return TRUE;
      }
      return FALSE;
    }
    if (!Can_be_speculative(wn) || WN_has_indir_load(wn))
      return FALSE;

    return TRUE;
  }
  else if (type == SC_IF) {
    count = sc->Head()->Executable_stmt_count();
    if (count != 1)
      return FALSE;

    wn = sc->Get_cond();
    if (!wn)
      return FALSE;

    WN * index = Get_index_load(loop);
    if (!index)
      return FALSE;

    AUX_ID aux = WN_aux(index);
    wn = WN_kid0(wn);
    if (!Is_invariant(loop, wn, aux))
      return FALSE;
    
    for (int i = 0; i < 2; i ++) {
      SC_NODE * sc_tmp = (i == 0) ? sc->Find_kid_of_type(SC_THEN) :
	sc->Find_kid_of_type(SC_ELSE);
      count = sc_tmp->Executable_stmt_count();
      if (count > 1)
	return FALSE;
      else if (count == 1) {
	SC_LIST_ITER sc_list_iter;
	SC_NODE * kid;
	FOR_ALL_ELEM(kid, sc_list_iter, Init(sc_tmp->Kids())) {
	  if (!kid->Is_empty() && !Is_kill(kid, stk, loop))
	    return FALSE;
	}
      }
      else {
	BOOL found = FALSE;
	for (int i = 0; i < stk->Elements(); i++) {
	  SC_NODE * sc_iter = stk->Top_nth(i);
	  if (Has_dependency(sc_iter, sc)
	      && sc_iter->Is_ctrl_equiv(sc)) {
	    found = TRUE;
	    break;
	  }
	}
	if (!found)
	  return FALSE;
      }
    }
    count = sc->Executable_stmt_count();
    if (count > 1) 
      return TRUE;
  }

  return FALSE;
}

// Walk statements in 'sc', search for store statements that writes the value of 'load',
// collect all possible values in 'def_vals'. Return FALSE if there exists a definition
// that can not be evaluated to an interger constant.
BOOL CFG_TRANS::Get_def_vals(BB_NODE * bb, WN * load, std::set<INT64> & def_vals)
{
  WN * tmp;
  for (tmp = bb->Firststmt(); tmp != NULL; tmp = WN_next(tmp)) {
    if (!WN_is_executable(tmp))
      continue;

    if (Maybe_assigned_expr(tmp, load)) {
      if (!OPERATOR_is_scalar_store(WN_operator(tmp)))
	return FALSE;
      WN * wn_data = WN_kid0(tmp);
      OPERATOR opr = WN_operator(wn_data);
      if (opr == OPR_INTCONST) {
	INT64 val = WN_const_val(wn_data);
	def_vals.insert(val);
      }
      else if (opr == OPR_ADD) {
	WN * kid0 = WN_kid0(wn_data);
	WN * kid1 = WN_kid1(wn_data);
	if(OPERATOR_is_scalar_load(WN_operator(kid0))
	   && (WN_aux(kid0) == WN_aux(tmp))
	   && (WN_operator(kid1) == OPR_INTCONST)) {
	  INT64 val = WN_const_val(kid1);
	  std::set<INT64> new_vals;
	  std::set<INT64>::iterator set_iter;
	  for (set_iter = def_vals.begin(); set_iter != def_vals.end(); set_iter++) {
	    INT64 p_val = *set_iter;
	    INT64 n_val = p_val + val;
	    new_vals.insert(n_val);
	  }
	  for (set_iter = new_vals.begin(); set_iter != new_vals.end(); set_iter++) {
	    INT64 val = *set_iter;
	    def_vals.insert(val);
	  }
	}
	else {
	  return FALSE;
	}
      }
      else {
	return FALSE;
      }
    }
  }
  return TRUE;
}

// Walk nodes in 'sc', search for statements that writes the value of 'load',
// collect all possible values in 'def_vals'. Return FALSE if there exists a
// definition that can not be evaluated to an interger constant.
BOOL CFG_TRANS::Get_def_vals(SC_NODE * sc, WN * load, std::set<INT64> & def_vals)
{
  BB_NODE * tmp = sc->Get_bb_rep();
  if (tmp) {
    if (!Get_def_vals(tmp, load, def_vals))
      return FALSE;
  }

  BB_LIST * bb_list = sc->Get_bbs();
  if (bb_list) {
    BB_LIST_ITER bb_list_iter(bb_list);
    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      if (!Get_def_vals(tmp, load, def_vals))
	return FALSE;
    }
  }

  SC_LIST * kids = sc->Kids();
  if (kids) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE *sc_tmp;
    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init()) {
      if (!Get_def_vals(sc_tmp, load, def_vals))
	return FALSE;
    }
  }
  return TRUE;
}

// Given a WN, query whether all of its kids and itself can be speculative.

BOOL CFG_TRANS::Can_be_speculative(WN * wn)
{
  INT i;
  struct ALIAS_MANAGER * alias = _cu->Alias_mgr();
  
  for (i=0; i<WN_kid_count(wn); i++) {
    if (!Can_be_speculative(WN_kid(wn,i)))
      return FALSE;
  }

  OPERATOR op = WN_operator(wn);

  if (OPERATOR_is_store(op) || OPERATOR_is_load(op)) {
    if (WN_Is_Volatile_Mem(wn))
      return FALSE;
  }
  if ((op == OPR_ALLOCA) || (op == OPR_DEALLOCA)
      || (op == OPR_ASM_STMT)
      || (op == OPR_FORWARD_BARRIER) || (op == OPR_BACKWARD_BARRIER))
    return FALSE;
  
  if (OPCODE_is_call(WN_opcode(wn)))
    return FALSE;

  return TRUE;
}

// Given a BB_NODE, query whether all of its real statements can be speculative.
BOOL
CFG_TRANS::Can_be_speculative(BB_NODE * bb)
{
  WN * tmp;

  for (tmp = bb->Firststmt(); tmp != NULL; tmp = WN_next(tmp)) {
    if (!WN_is_executable(tmp))
      continue;
    
    if (!Can_be_speculative(tmp))
      return FALSE;
  }

  return TRUE;
}

// Given a SC_NODE, query whether all of its BB_NODEs can be speculative.
BOOL
CFG_TRANS::Can_be_speculative(SC_NODE * sc)
{
  BB_LIST_ITER bb_list_iter;
  BB_NODE * tmp;

  tmp = sc->Get_bb_rep();

  if ((tmp != NULL) && !Can_be_speculative(tmp))
    return FALSE;

  FOR_ALL_ELEM(tmp, bb_list_iter,Init(sc->Get_bbs())) {
    if (!Can_be_speculative(tmp))
      return FALSE;
  }

  SC_LIST_ITER sc_list_iter(sc->Kids());
  SC_NODE * sc_tmp;

  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init()) {
    if (!Can_be_speculative(sc_tmp))
      return FALSE;
  }
  
  return TRUE;
}

// For every load in wn, if it aliases with wn_iter,
// remove its value from _val_map

void CFG_TRANS::Remove_val(WN * wn_iter, WN * wn)
{
  OPCODE opc = WN_opcode(wn);
  
  if (OPCODE_is_load(opc)) {
    if (Is_aliased(wn_iter, wn))
      Set_val(WN_aux(wn), 0);
  }
  
  for (int i = 0; i < WN_kid_count(wn); i++) 
    Remove_val(wn_iter, WN_kid(wn,i));
}

// Track the flow of values for loads in the given WN tree by 
// evaluating all blocks in the given sc in source order. 
// 'bb_entry' gives the initial Gen block of tracked values.
// 'do_recursive' indicates whether to visit children of 'sc'.
//
// Current implementation is quick and light-weight.
// In the future, consider using existing value-numbering
// mechanism.
 
void CFG_TRANS::Track_val(SC_NODE * sc, BB_NODE * bb_entry, WN * wn, BOOL do_recursive)
{

  BB_NODE * bb = sc->Get_bb_rep();

  if (bb != NULL)
    Track_val(bb, bb_entry, wn);
  
  BB_LIST * bb_list = sc->Get_bbs();

  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;
    
    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      Track_val(tmp, bb_entry, wn);
    }
  }

  if (do_recursive) {
    SC_LIST * kids = sc->Kids();
  
    if (kids != NULL) {
      SC_LIST_ITER sc_list_iter(kids);
      SC_NODE *tmp;
      FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
	Track_val(tmp, bb_entry, wn, do_recursive);
      }
    }
  }
}

// Track the flow of values for loads in the given WN tree by
// evaluating all statements in the given bb_cur. bb_entry gives
// the initial Gen block of tracked values.

void CFG_TRANS::Track_val(BB_NODE * bb_cur, BB_NODE * bb_entry, WN * wn)
{
  STMT_ITER stmt_iter;
  WN * wn_iter;
  ALIAS_MANAGER * alias_mgr = _cu->Alias_mgr();

  FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb_cur->Firststmt(), bb_cur->Laststmt())) {
    OPCODE opc = WN_opcode(wn_iter);

    // Check kills.
    if (OPCODE_is_call(opc) || (opc == OPC_IO))
      Remove_val(wn_iter, wn);

    if (OPERATOR_is_scalar_store(WN_operator(wn_iter))) {
      WN * wn_data = WN_kid0(wn_iter);
      AUX_ID rval = (WN_has_sym(wn_data) &&
		     OPERATOR_is_load(WN_operator(wn_data))) ? Get_val(WN_aux(wn_data)) : 0;
      AUX_ID lval = Get_val(WN_aux(wn_iter));
      
      if (Is_trackable_var(WN_aux(wn_iter)) && rval
	  && bb_entry->Dominates(bb_cur)
	  && bb_cur->Postdominates(bb_entry)) {
	// For a store to a trackable variable in a block that is control-equivalent
	// to the bb_entry, transfer the RHS value (if exists) to the LHS.
	Set_val(WN_aux(wn_iter), rval);
      }
      else if (lval)
	Set_val(WN_aux(wn_iter), 0);
    }
  }
}

// Do top-down if-merging for the SC tree rooted at the given SC_NODE.
// CFG, LOOPs and SC tree are updated upon exit of this routine.

void
IF_MERGE_TRANS::Top_down_trans(SC_NODE * sc)
{
  SC_LIST * kids = sc->Kids();

  if (kids == NULL)
    return;

  CFG * cfg = _cu->Cfg();
  BOOL do_analyze_loops = cfg->Loops_valid(); 

  SC_LIST_ITER kids_iter;
  SC_NODE * tmp;
  SC_NODE * sc1 = NULL;
  SC_NODE * sc2 = NULL;
  SC_NODE * sc_new;

  // Do if-collapsing first. See IF_MERGE_TRANS::Is_uncond_cand.
  _action = DO_IFCOLLAPSE;
  sc1 = sc->First_kid_of_type(SC_IF);

  if (sc1 != NULL) {
    sc2 = sc1->Next_sibling_of_type(SC_IF);
    
    while (sc2 != NULL) {
      sc_new = Do_merge(sc1, sc2, TRUE);

      if (sc_new) {
	sc1 = sc_new;
	sc2 = sc1->Next_sibling_of_type(SC_IF);
      }
      else {
	sc1 = sc2;
	sc2 = sc1->Next_sibling_of_type(SC_IF);
      }
    }
  }

  // Do if-merging
  _action = DO_IFMERGE;
  sc1 = sc->First_kid_of_type(SC_IF);
  while (sc1 != NULL) {
    sc2 = sc1->Next_sibling_of_type(SC_IF);
    
    while (sc2 != NULL) {
      sc_new = Do_merge(sc1, sc2, TRUE);
      
      if (sc_new) {
	sc1 = sc_new;
	sc2 = sc1->Next_sibling_of_type(SC_IF);
      }
      else
	sc2 = sc2->Next_sibling_of_type(SC_IF);
    }

    sc1 = sc1->Next_sibling_of_type(SC_IF);
  }

  if (WOPT_Simplify_Bit_Op) {
    // Do if-flipping
    _action = DO_IFFLIP;
    sc1 = sc->First_kid_of_type(SC_IF);
    while (sc1 != NULL) {
      sc2 = sc1->Next_sibling_of_type(SC_IF);
    
      while (sc2 != NULL) {
	sc_new = Do_merge(sc1, sc2, TRUE);
      
	if (sc_new) {
	  sc1 = sc_new;
	  sc2 = sc1->Next_sibling_of_type(SC_IF);
	}
	else
	  sc2 = sc2->Next_sibling_of_type(SC_IF);
      }

      sc1 = sc1->Next_sibling_of_type(SC_IF);
    }
  }

  _action = DO_IFMERGE;
  FOR_ALL_ELEM(tmp, kids_iter, Init(sc->Kids())) {
    this->Top_down_trans(tmp);
  }

  if (do_analyze_loops && !cfg->Loops_valid()) 
    cfg->Analyze_loops();
}

// Lower level if-merging of two if-regions.
// Upon exit of this routine, CFG are updated.
// Loops are invalidated but not updated.
void
IF_MERGE_TRANS::Merge_CFG(SC_NODE * sc1, SC_NODE * sc2)
{
  FmtAssert(((sc1->Type() == SC_IF) && (sc2->Type() == SC_IF)), ("Expect SC_Ifs"));
  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();

  // find boundary BB_NODEs

  BB_NODE * sc1_then_end = sc1->Then_end();
  BB_NODE * sc1_else_end = sc1->Else_end();
  BB_NODE * sc1_else = sc1->Else();
  BB_NODE * sc1_merge = sc1->Merge();
  BB_NODE * sc2_then = sc2->Then();
  BB_NODE * sc2_else = sc2->Else();
  BB_NODE * sc2_then_end = sc2->Then_end();
  BB_NODE * sc2_else_end = sc2->Else_end();
  BB_NODE * sc2_merge = sc2->Merge();
  BB_NODE * sc2_head = sc2->Get_bb_rep();

  if (_trace) {
    if (Transform_count() == 0) {
      printf("\n*********** If-merge for %s(%d) ***********\n",
	     Current_PU_Name(), Current_PU_Count());
    }

    switch (_action) {
    case DO_IFFLIP:
      // From:
      // if (a & (1 << b)) 
      //   block 1;
      // a ^= (1 << b);
      // if (a & (1 << b))
      //   block 2;
      //
      // To:
      // if (a & ( 1 << b)) {
      //   block 1;
      //   a ^= ( 1 << b);
      // }
      // else {
      //   a ^= (1 << b);
      //   block 2;
      // }
      
      printf("\n\t If-flip (SC%d,SC%d)\n", 
	     sc1->Id(), sc2->Id());
      break;
    case DO_IFMERGE:
      // From:
      // if (a)
      //  block 1;
      // else
      //  block 2;
      // if (a)
      //   block 3;
      // else
      //   block 4;
      //
      // To:
      // if (a) {
      //   block 1;
      //   block 3;
      // }
      // else {
      //   block 2;
      //   block 4;
      // }
      
      printf("\n\t If-merge (SC%d,SC%d)\n", 
	     sc1->Id(), sc2->Id());
      break;
    case DO_IFCOLLAPSE:
      // From:
      // if (a) 
      //  x = const1;
      // else
      //  x = const2;
      // if (x = const1)
      //   block1;
      // else
      //   block2;
      // 
      // where const1 != const2
      //
      // To:
      // if (a) {
      //  x = const1;
      //  block1;
      // }
      // else {
      //  x= const2;
      //  block 2;
      // }
      printf("\n\t If-collapse (SC%d,SC%d)\n", 
	     sc1->Id(), sc2->Id());
      break;
    default:
      ;
    }
  }

  if (_dump) {
    fprintf(TFile, "\n Before if-merge\n");
    cfg->Print(TFile, false, (unsigned) -1) ;
  }

  sc1_then_end->Replace_succ(sc1_merge, sc2_then);

  if (cfg->Feedback())
    cfg->Feedback()->Move_edge_dest(sc1_then_end->Id(), sc1_merge->Id(), sc2_then->Id());

  sc1_else_end->Replace_succ(sc1_merge, sc2_else);

  if (cfg->Feedback())
    cfg->Feedback()->Move_edge_dest(sc1_else_end->Id(), sc1_merge->Id(), sc2_else->Id());
  
  sc2_then->Replace_pred(sc2_head, sc1_then_end);
  
  if (cfg->Feedback())
    cfg->Feedback()->Delete_edge(sc2_head->Id(), sc2_then->Id());

  sc2_else->Replace_pred(sc2_head, sc1_else_end);

  if (cfg->Feedback())
    cfg->Feedback()->Delete_edge(sc2_head->Id(), sc2_else->Id());

  sc2_then_end->Replace_succ(sc2_merge, sc1_merge);
  
  if (cfg->Feedback())
    cfg->Feedback()->Move_edge_dest(sc2_then_end->Id(), sc2_merge->Id(), sc1_merge->Id());

  sc2_else_end->Replace_succ(sc2_merge, sc1_merge);
  
  if (cfg->Feedback())
    cfg->Feedback()->Move_edge_dest(sc2_else_end->Id(), sc2_merge->Id(), sc1_merge->Id());

  sc1_merge->Replace_pred(sc1_then_end, sc2_then_end);
  sc1_merge->Replace_pred(sc1_else_end, sc2_else_end);

  sc2_head->Remove_succ(sc2_then, pool);
  sc2_head->Replace_succ(sc2_else, sc2_merge);

  sc2_merge->Remove_pred(sc2_then_end, pool);
  sc2_merge->Replace_pred(sc2_else_end, sc2_head);

  if (cfg->Feedback()) {
    cfg->Feedback()->Add_edge(sc2_head->Id(), sc2_merge->Id(), FB_EDGE_OUTGOING, 
			      cfg->Feedback()->Get_edge_freq(sc2_then_end->Id(), sc1_merge->Id())
			      + cfg->Feedback()->Get_edge_freq(sc2_else_end->Id(), sc1_merge->Id()));

  }

  Delete_branch(sc2_head);

  // Fix Prev() and Next()
  sc1_then_end->Set_next(sc2_then);
  sc2_then->Set_prev(sc1_then_end);

  sc2_then_end->Set_next(sc1_else);
  sc1_else->Set_prev(sc2_then_end);

  sc1_else_end->Set_next(sc2_else);
  sc2_else->Set_prev(sc1_else_end);
  
  sc2_else_end->Set_next(sc1_merge);
  sc1_merge->Set_prev(sc2_else_end);
  
  sc2_head->Set_next(sc2_merge);
  sc2_merge->Set_prev(sc2_head);

  cfg->Invalidate_and_update_aux_info(FALSE);
  cfg->Invalidate_loops();

}

// Merge sc2 into sc1:
// sc1 takes over sc2's kids, unlink sc2 from the SC tree.

void
IF_MERGE_TRANS::Merge_SC(SC_NODE * sc1, SC_NODE * sc2)
{
  FmtAssert((sc1->Type() == sc2->Type()), ("Expect same SC type"));
  SC_LIST * kids2 = sc2->Kids();

  // If sc2 has no kids, simply unlink it from the SC tree.
  if (kids2 == NULL) {
    sc2->Unlink();
    return;
  }
  
  // If Both sc1's last kid and sc2's first kid are SC_BLOCKs, merge BB_NODEs
  SC_NODE * last1 = sc1->Last_kid();
  SC_NODE * first2 = sc2->First_kid();

  if ((last1 != NULL) && (last1->Type() == first2->Type())
      && (last1->Type() == SC_BLOCK)) {
    BB_LIST * bbs = first2->Get_bbs();
    MEM_POOL * pool = first2->Get_pool();

    while (bbs) {
      BB_NODE * bb = bbs->Node();
      last1->Append_bbs(bb);
      bbs = bbs->Remove(bb, pool);
    }
    
    first2->Set_bbs(NULL);
    first2->Unlink();
  }

  // Merge remaining kids from sc2 into sc1.
  first2 = sc2->First_kid();

  while (first2) {
    sc1->Append_kid(first2);
    first2->Set_parent(sc1);
    sc2->Remove_kid(first2);
    first2 = sc2->First_kid();
  }
  
  sc2->Unlink();
  sc2->Delete();
}

// Attempt if-merging for the given pair of SC_NODEs.  If successful, return
// the merged SC_NODE. 'do_legal_check' indicates whether to do legality check.
SC_NODE *
IF_MERGE_TRANS::Do_merge(SC_NODE * sc1, SC_NODE * sc2, BOOL do_legal_check)
{
  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return NULL;

  if (do_legal_check && !Is_candidate(sc1, sc2, FALSE))
    return NULL;

  // Merge CFG BBs.
  Merge_CFG(sc1, sc2);

  CFG * cfg = _cu->Cfg();

  if (_dump) {
    fprintf(TFile, "\n====SC tree before if-merge====\n");
    cfg->SC_root()->Print(TFile,1);
  }
  
  // Merge SC tree.
  SC_NODE * then1 = sc1->Find_kid_of_type(SC_THEN);
  SC_NODE * else1 = sc1->Find_kid_of_type(SC_ELSE);
  SC_NODE * then2 = sc2->Find_kid_of_type(SC_THEN);
  SC_NODE * else2 = sc2->Find_kid_of_type(SC_ELSE);

  Merge_SC(then1, then2);
  Merge_SC(else1, else2);

  sc2->Convert(SC_BLOCK);
  
  // Converting sc2 into a SC_BLOCK could expose opportunities to 
  // merge SC_BLOCKs among sc2's immediate siblings.

  SC_NODE * sc_blk = sc2->Prev_sibling();
  
  if ((sc_blk == NULL) || (sc_blk->Type() != SC_BLOCK))
    sc_blk = sc2;
  
  FmtAssert((sc_blk->Kids() == NULL), ("SC_BLOCK should have no kid"));
  
  SC_NODE * next_sibling = sc_blk->Next_sibling();
  
  while ((next_sibling) && (next_sibling->Type() == SC_BLOCK)) {
    // merge next_sibling's BB_NODES into sc_blk.
    BB_LIST * bb_list = next_sibling->Get_bbs();
    SC_NODE * next = next_sibling->Next_sibling();
    MEM_POOL * pool = next_sibling->Get_pool();
    
    FmtAssert((next_sibling->Kids() == NULL), ("SC_BLOCK should have no kid"));
    
    while (bb_list) {
      BB_NODE * bb = bb_list->Node();
      bb_list = bb_list->Remove(bb, pool);
      sc_blk->Append_bbs(bb);
    }
    
    next_sibling->Unlink();
    next_sibling = next;
  }

  if (_dump) {
    fprintf(TFile, "\n====SC tree After if-merge====\n");
    cfg->SC_root()->Print(TFile,1);
  }

  FmtAssert(sc1->Is_well_behaved(), ("Not well-behaved after if-merge"));
  Inc_transform_count();
  return sc1;
}

// Query whether loads/stores in the WN tree rooted at wn_root could alias with wn1,
// where wn1 is a store/call statement.

BOOL
CFG_TRANS::Maybe_assigned_expr(WN * wn1, WN * wn_root)
{
  OPCODE opc = WN_opcode(wn_root);

  if (OPCODE_is_load(opc) || OPCODE_is_store(opc) || (opc == OPC_IO)) {
    OPCODE opc = WN_opcode(wn1);

    // OPC_IO has side effect and hidden control flow.
    if (opc == OPC_IO)
      return TRUE;

    // Disambiguate bit-reduction operations on the same object.
    WN * wn_red1 = WN_get_bit_reduction(wn1);
    WN * wn_red2 = WN_get_bit_reduction(wn_root);
    if (wn_red1 && wn_red2 && (WN_Simp_Compare_Trees(WN_kid0(wn_red1), WN_kid0(wn_red2)) == 0)) {
      if (!Maybe_assigned_expr(wn1, wn_red2)
	  && !Maybe_assigned_expr(wn_root, wn_red1))
	return FALSE;
    }
    
    ALIAS_MANAGER * alias_mgr = _cu->Alias_mgr();

    if ((OPCODE_is_store(opc) || OPCODE_is_call(opc))
	&& Is_aliased(wn1, wn_root))
      return TRUE;
  }
  if (WOPT_Simplify_Bit_Op) {
    // No alias if wn1 is a reduction of a single bit operation on an object, 
    // and wn_root is a bit operation on a different bit of the same object.
    WN * wn_bit_op = WN_get_bit_reduction(wn1);
    WN * wn_expr = wn_root;
    OPERATOR opr = WN_operator(wn_expr);

    if (OPERATOR_is_compare(opr)) {
      if (WN_operator(WN_kid(wn_expr, 0)) == OPR_INTCONST)
	wn_expr = WN_kid(wn_expr, 1);
      else if (WN_operator(WN_kid(wn_expr, 1)) == OPR_INTCONST)
	wn_expr = WN_kid(wn_expr, 0);
    }

    if (wn_bit_op && WN_is_bit_op(wn_expr)
	&& (WN_Simp_Compare_Trees(WN_kid(wn_expr,0), WN_kid(wn_bit_op, 0)) == 0)) {
      WN * wn_tmp1 = WN_kid(wn_expr, 1);

      if (WN_is_power_of_2(wn_tmp1)) {
	WN * wn_tmp2 = WN_kid(wn_bit_op, 1);

	Match_def(wn_tmp2);
	Match_def(wn_tmp1);

	if (WN_has_disjoint_val_range(wn_tmp2, wn_tmp1, _low_map, _high_map, Get_deriv_map()))
	  return FALSE;
      }
    }
  }

  for ( int i = 0; i < WN_kid_count(wn_root); i++) {
    if (Maybe_assigned_expr(wn1, WN_kid(wn_root,i)))
      return TRUE;
  }

  return FALSE;
}

// Query whether loads/stores in the WN tree rooted at wn_root could alias with
// a store/call statement in bb.

BOOL
CFG_TRANS::Maybe_assigned_expr(BB_NODE * bb, WN * wn_root)
{
  STMT_ITER stmt_iter;
  WN * wn_iter;

  FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {
    if (Maybe_assigned_expr(wn_iter, wn_root))
      return TRUE;
  }

  return FALSE;
}

// Query whether loads/stores in the WHILE tree rooted at wn_root could
// alias with a store/call statement  in the SC tree rooted at sc. 
// eval_true indicates whether wn_root is evaluated to TRUE at the entry
// of the sc.

BOOL
CFG_TRANS::Maybe_assigned_expr(SC_NODE * sc, WN * wn_root, BOOL eval_true)
{
  OPERATOR opr = WN_operator(wn_root);

  if ((opr == OPR_INTCONST) || (opr == OPR_CONST))
    return FALSE;
  
  if (Is_trackable_expr(wn_root)) {
    // Track whether values of loads in the WHILR tree rooted at wn_root
    // are modified by sc
    if (Val_mod(sc, wn_root, eval_true))
      return TRUE;
    else
      return FALSE;
  }

  BB_NODE * bb = sc->Get_bb_rep();

  if ((bb != NULL) && Maybe_assigned_expr(bb, wn_root))
    return TRUE;
  
  BB_LIST * bb_list = sc->Get_bbs();

  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;
    
    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      if (Maybe_assigned_expr(tmp, wn_root))
	return TRUE;
    }
  }

  SC_LIST * kids = sc->Kids();
  
  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE *tmp;
    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      if (Maybe_assigned_expr(tmp, wn_root, FALSE))
	return TRUE;
    }
  }

  return FALSE;
}

// Query whether loads/stores of WN trees in bb could alias with a store/call
// statement in the SC tree rooted at sc.

BOOL
CFG_TRANS::Maybe_assigned_expr(SC_NODE *sc1, BB_NODE *bb)
{
  STMT_ITER stmt_iter;
  WN * wn_iter;

  FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {
    if ((WN_operator(wn_iter) == OPR_FALSEBR)
	|| (WN_operator(wn_iter) == OPR_TRUEBR))
      wn_iter = WN_kid0(wn_iter);

    if (Maybe_assigned_expr(sc1, wn_iter, FALSE))
      return TRUE;
  }

  return FALSE;
}

// Query whether loads/stores of WN trees in bb2 could alias with a store/call
// statement in bb1.

BOOL
CFG_TRANS::Maybe_assigned_expr(BB_NODE *bb1, BB_NODE *bb2)
{
  STMT_ITER stmt_iter;
  WN * wn_iter;

  FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb2->Firststmt(), bb2->Laststmt())) {
    if (Maybe_assigned_expr(bb1, wn_iter))
      return TRUE;
  }

  return FALSE;
}

// Query whether loads/stores of WN tree in SC tree rooted at sc could alias
// with a store/call statement in bb.

BOOL
CFG_TRANS::Maybe_assigned_expr(BB_NODE * bb, SC_NODE * sc)
{
  BB_NODE * tmp = sc->Get_bb_rep();
  
  if ((tmp != NULL) && Maybe_assigned_expr(bb, tmp))
    return TRUE;

  BB_LIST * bb_list = sc->Get_bbs();

  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;
    
    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      if (Maybe_assigned_expr(bb, tmp))
	return TRUE;
    }
  }
  
  SC_LIST * kids = sc->Kids();

  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE *tmp;
    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      if (Maybe_assigned_expr(bb, tmp))
	return TRUE;
    }
  }
  
  return FALSE;
}

// Query whether loads/stores of WN trees in the SC tree rooted at sc2 could alias
// with a store/call statement in the SC tree rooted at sc1.

BOOL
CFG_TRANS::Maybe_assigned_expr(SC_NODE * sc1, SC_NODE * sc2)
{
  BB_NODE * bb = sc2->Get_bb_rep();

  if ((bb != NULL) && Maybe_assigned_expr(sc1, bb))
    return TRUE;

  BB_LIST * bb_list = sc2->Get_bbs();
  
  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;

    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      if (Maybe_assigned_expr(sc1, tmp))
	return TRUE;
    }
  }

  SC_LIST * kids = sc2->Kids();

  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE *tmp;
    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      if (Maybe_assigned_expr(sc1, tmp))
	return TRUE;
    }
  }
  
  return FALSE;
}

// Check whether sc2 can be collapsed into sc1
// Look for the pattern like below. This can be considered as a special
// case of if-merging.
// if () {
//   ......
//   x = const1;
// }
// else {
//   ......
//   x = const2;
// }
// if (x == const1) {
//   ......
// }
// else {
//   ......
// }
// where the value of const1 is not equal to the value of const2.
BOOL
IF_MERGE_TRANS::Is_if_collapse_cand(SC_NODE * sc1, SC_NODE * sc2)
{
  if (sc1->Next_sibling() != sc2)
    return FALSE;

  WN * wn;
  BB_NODE * head2 = sc2->Head();

  // Count non-label statements in head2
  STMT_ITER stmt_iter;
  int count = 0;

  FOR_ALL_ELEM (wn, stmt_iter, Init(head2->Firststmt(), head2->Laststmt())) {
    if (WN_is_executable(wn))
      count++;
  }
  
  // head2 should only contain a comparison on EQ between a I4 constant and a scalar load.
  if (count != 1)
    return FALSE;

  wn = head2->Laststmt();
  wn = WN_kid0(wn);
  if (WN_operator(wn) != OPR_EQ)
    return FALSE;

  WN * wn_const = NULL;
  WN * wn_load = NULL;

  for ( int i = 0; i < WN_kid_count(wn); i ++) {
    WN * kid = WN_kid(wn,i);
    if (WN_operator(kid) == OPR_INTCONST)
      wn_const = kid;
    else if (OPERATOR_is_scalar_load(WN_operator(kid)))
      wn_load = kid;
  }

  if (!wn_const || !wn_load)
    return FALSE;
  
  // Check whether both then-path and else-path of sc1 end with a scalar store of a
  // constant value, the store has the same symbol as wn_load, the constant value
  // on the then-path is the same as wn_const, and the constant value on the else-path
  // is different from wn_const.
  
  BB_NODE * then_end = sc1->Then_end();
  BB_NODE * else_end = sc1->Else_end();

  wn = then_end->Laststmt();
  if (!wn 
      || !OPERATOR_is_scalar_store(WN_operator(wn))
      || (WN_aux(wn) != WN_aux(wn_load))
      || (WN_operator(WN_kid0(wn)) != OPR_INTCONST)
      || (WN_const_val(WN_kid0(wn)) != WN_const_val(wn_const)))
    return FALSE;

  wn = else_end->Laststmt();
  if (!wn
      || !OPERATOR_is_scalar_store(WN_operator(wn))
      || (WN_aux(wn) != WN_aux(wn_load))
      || (WN_operator(WN_kid0(wn)) != OPR_INTCONST)
      || (WN_const_val(WN_kid0(wn)) == WN_const_val(wn_const)))
    return FALSE;

  return TRUE;
}

// Flip then-path and else-path for 'sc2', which is a SC_IF.
void
CFG_TRANS::Do_flip(SC_NODE * sc2)
{
  if (sc2->Type() != SC_IF)
    return;

  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();
  SC_NODE * sc_then = sc2->Find_kid_of_type(SC_THEN);
  SC_NODE * sc_else = sc2->First_kid_of_type(SC_ELSE);
  BB_NODE * bb_head = sc2->Head();
  BB_NODE * bb_then = sc2->Then();
  BB_NODE * bb_else = sc2->Else();
  BB_NODE * bb_then_end = sc2->Then_end();
  BB_NODE * bb_else_end = sc2->Else_end();
  BB_NODE * bb_tmp = bb_then->Prev();
  bb_tmp->Set_next(bb_else);
  bb_else->Set_prev(bb_tmp);
  bb_tmp = bb_else_end->Next();
  bb_else_end->Set_next(bb_then);
  bb_then->Set_prev(bb_else_end);
  bb_then_end->Set_next(bb_tmp);
  bb_tmp->Set_prev(bb_then_end);
  bb_tmp = bb_head->Last_succ();
  bb_head->Remove_succ(bb_tmp, pool);
  bb_head->Prepend_succ(bb_tmp, pool);
  cfg->Add_label_with_wn(bb_then);
  WN * branch_wn = bb_head->Branch_wn();
  WN_label_number(branch_wn) = bb_then->Labnam();
      
  SC_LIST * then_kids = sc_then->Kids();
  SC_LIST * else_kids = sc_else->Kids();
  sc_then->Set_kids(else_kids);
  sc_else->Set_kids(then_kids);
      
  SC_LIST_ITER sc_list_iter;
  SC_NODE * sc_tmp;

  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(else_kids)) {
    sc_tmp->Set_parent(sc_then);
  }

  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(then_kids)) {
    sc_tmp->Set_parent(sc_else);
  }
     
  cfg->Fix_info(sc2);
  cfg->Invalidate_and_update_aux_info(FALSE);
  cfg->Invalidate_loops();
  Inc_transform_count();
}

// Check whether the given pair of SC_NODEs are if-merging candidates.
// where, sc1 precedes sc2 in source order.  Do not invoke tail duplication
// transformation if do_query is TRUE.

BOOL
IF_MERGE_TRANS::Is_candidate(SC_NODE * sc1, SC_NODE * sc2, BOOL do_query)
{
  if ((sc1->Type() != SC_IF) || (sc2->Type() != SC_IF))
    return FALSE;

  // should have the same parent in the SC tree.
  if (sc1->Parent() != sc2->Parent())
    return FALSE;

  // Avoid compgoto complication.
  if (sc1->Parent()->Find_kid_of_type(SC_COMPGOTO))
    return FALSE;

  BB_NODE * rep1 = sc1->Get_bb_rep();
  BB_NODE * rep2 = sc2->Get_bb_rep();

  BB_IFINFO * info1 = rep1->Ifinfo();
  BB_IFINFO * info2 = rep2->Ifinfo();

  BB_NODE * tail1 = info1->Merge();
  BB_NODE * tail2 = info2->Merge();
  BB_NODE * head1 = rep1;
  BB_NODE * head2 = rep2;

  if (tail1->Pred()->Len() != 2)
    return FALSE;

  if (tail2->Pred()->Len() != 2)
    return FALSE;
  
  // tail1 should be control-equivalent to head2.
  if ((tail1 != head2)
      && (!tail1->Dominates(head2) || !head2->Postdominates(tail1)))
    return FALSE;

  if (!sc1->Is_well_behaved())
    return FALSE;

  if (!sc2->Is_well_behaved())
    return FALSE;

  // If merge SC is a SC_BLOCK, it should be a SESE.

  SC_NODE * next_sibling = sc1->Next_sibling();
  if (next_sibling
      && (next_sibling->Type() == SC_BLOCK)
      && !next_sibling->Is_sese())
    return FALSE;

  next_sibling = sc2->Next_sibling();
  
  if (next_sibling
      && (next_sibling->Type() == SC_BLOCK)
      && !next_sibling->Is_sese())
    return FALSE;

  // Check whether sc2 can be if-collapsed with sc1.
  // See IF_MERGE_TRANS:Is_if_collapse_cand.
  if (_action == DO_IFCOLLAPSE) {
    if (Is_if_collapse_cand(sc1, sc2)) 
      return TRUE;
    else
      return FALSE;
  }

  // Should have the same condition expression value
  WN * cond1 = head1->Laststmt();
  WN * cond2 = head2->Laststmt();  
  OPCODE op1 = WN_opcode(cond1);
  OPCODE op2 = WN_opcode(cond2);

  FmtAssert(((op1 == OPC_FALSEBR) || (op1 == OPC_TRUEBR)), ("Unexpect cond"));
  FmtAssert(((op2 == OPC_FALSEBR) || (op2 == OPC_TRUEBR)), ("Unexpect cond"));
  
  if (op1 != op2)
    return FALSE;

  WN * expr1 = WN_kid0(cond1);
  WN * expr2 = WN_kid0(cond2);

  if (WN_Simp_Compare_Trees(expr1, expr2) != 0)
    return FALSE;

  SC_LIST_ITER sc_list_iter;
  SC_NODE* tmp;
  BOOL no_alias = FALSE;

  // Use hashed result.
  SC_NODE * loop1 =  _invar_map[head1->Id()];
  SC_NODE * loop2 =  _invar_map[head2->Id()];
    
  if (loop1 && (loop1 == loop2) && 
      (loop1->Is_pred_in_tree(sc1) || (_region_id == loop1->Id())))
    no_alias = TRUE;

  Infer_val_range(sc1, sc2);

  BOOL do_flip = FALSE;
  BOOL can_skip_taildup = FALSE;
  SC_NODE * sc_flip = NULL;

  if (!no_alias) {
    if (!do_query && (_action == DO_IFFLIP)) {
      WN * wn_tmp = WN_kid0(expr1);
      if (wn_tmp) {
	BOOL result1 = FALSE;
	BOOL result2 = FALSE;
	if (Do_flip_tail_merge(sc1, wn_tmp))
	  result1 = TRUE;
	if (Do_flip_tail_merge(sc2, wn_tmp))
	  result2 = TRUE;
	if (result1 && result2)
	  can_skip_taildup = TRUE;
      }
    }

    // sc1' then-path and else-path should not modify condition expression.
    FOR_ALL_ELEM(tmp, sc_list_iter, Init(sc1->Kids())) {
      if (Maybe_assigned_expr(tmp, expr1, (tmp->Type() == SC_THEN) ? TRUE : FALSE)) {
	Delete_val_range_maps();
	return FALSE;
      }
    }

    next_sibling = sc1->Next_sibling();

    if (_action == DO_IFFLIP) {
      // Match pattern:
      // if (a & (1 << b)) {
      //   ....
      // }
      // a ^= (1 << b);
      // if (a & (1 << b)) {
      //   ......
      // }

      OPERATOR opr = WN_operator(expr1);
      if ((opr != OPR_EQ) && (opr != OPR_NE)) {
	Delete_val_range_maps();
	return FALSE;
      }
      
      WN * wn_const = WN_kid(expr1, 1);
      if ((WN_operator(wn_const) != OPR_INTCONST)
	  || (WN_const_val(wn_const) != 0)) {
	Delete_val_range_maps();
	return FALSE;
      }
      
      WN * wn_and = WN_kid(expr1, 0);

      if (!wn_and || (WN_operator(wn_and) != OPR_BAND)) {
	Delete_val_range_maps();
	return FALSE;
      }

      WN * wn1 = WN_kid(wn_and, 1);
      
      if (!WN_is_power_of_2(wn1)) {
	Delete_val_range_maps();
	return FALSE;
      }
      
      // All siblings between sc1 and sc2 (exclusive) should contain only one statement
      // that flips the condition expression.
      int stmt_cnt = 0;
      while (next_sibling && (next_sibling != sc2)) {
	int local_cnt = next_sibling->Executable_stmt_count();
	
	if (local_cnt == 0) {
	  next_sibling = next_sibling->Next_sibling();
	  continue;
	}
	else {
	  stmt_cnt += local_cnt;
	  if (stmt_cnt > 1) {
	    Delete_val_range_maps();
	    return FALSE;
	  }
	  
	  WN * wn_flip = next_sibling->First_executable_stmt();
	  
	  if (!wn_flip) {
	    Delete_val_range_maps();
	    return FALSE;
	  }

	  WN * wn_xor = WN_get_bit_reduction(wn_flip);

	  if (!wn_xor || (WN_operator(wn_xor) != OPR_BXOR)) {
	    Delete_val_range_maps();
	    return FALSE;
	  }

	  if (WN_Simp_Compare_Trees(WN_kid(wn_and, 0), WN_kid(wn_xor,0)) != 0) {
	    Delete_val_range_maps();
	    return FALSE;
	  }
	  
	  WN * wn2 = WN_kid(wn_xor, 1);

	  if (!WN_is_power_of_2(wn2)) {
	    Delete_val_range_maps();
	    return FALSE;
	  }

	  if (WN_Simp_Compare_Trees(wn1, wn2) != 0) {
	    Delete_val_range_maps();
	    return FALSE;
	  }

	  do_flip = TRUE;
	  sc_flip = next_sibling;
	}
	next_sibling = next_sibling->Next_sibling();
      }
    }
    else {
      // All siblings between sc1 and sc2 (exclusive) should not
      // modify condition expression.
      while (next_sibling && (next_sibling != sc2)) {
	if (Maybe_assigned_expr(next_sibling, expr2, FALSE)) {
	  Delete_val_range_maps();
	  return FALSE;
	}

	next_sibling = next_sibling->Next_sibling();
      }
    }

    // head2 should not modifiy condition expression.
    if (Maybe_assigned_expr(head2, expr2)) {
      Delete_val_range_maps();
      return FALSE;
    }
  }

  // sc2's then-path and else-path should have no dependency on head2
  // excluding the conditional branch.

  if (head2->Executable_stmt_count() > 1) {
    BOOL has_non_sp = !Can_be_speculative(head2);
    
    FOR_ALL_ELEM(tmp, sc_list_iter, Init(sc2->Kids())) {
      if (!no_alias && Has_dependency(tmp, head2)) {
	Delete_val_range_maps();
	return FALSE;
      }

      // Do not reorder operations that can not be speculative.
      if (has_non_sp && !Can_be_speculative(tmp)) {
	Delete_val_range_maps();
	return FALSE;
      }
    }
  }

  if (_pass == PASS_EXT) {
    Delete_val_range_maps();
    return TRUE;
  }

  if (do_flip) {
    if (can_skip_taildup) {
      Delete_val_range_maps();
      Do_flip(sc2);
      return TRUE;
    }

    // Instead of tail-duplication, we can sink the flip statement to sc2's merge block
    // if it has no dependency on sc2's then-path and else-path.
    if (!Has_dependency(sc2->First_kid(), sc_flip)
	&& !Has_dependency(sc2->Last_kid(), sc_flip)) {
      BB_NODE * bb_merge = sc2->Merge();
      BB_NODE * bb_tmp = sc_flip->First_executable_blk();
      WN * wn_flip = bb_tmp->First_executable_stmt();
      bb_tmp->Unlink_stmt(wn_flip);
      bb_merge->Prepend_stmt(wn_flip);
      Delete_val_range_maps();
      Do_flip(sc2);
      return TRUE;
    }
  }

  // For every sibling between sc1 and sc2 (exclusive), if it has 
  // dependency on sc2, and all siblings are SC_BLOCKs, do tail
  // duplication.

  BOOL has_dep = FALSE;
  BOOL do_tail_dup = TRUE;
  BOOL has_non_sp = FALSE;

  next_sibling = sc1->Next_sibling();
  SC_NODE * sc2_next_sibling = sc2->Next_sibling_of_type(SC_IF);
  int count = 0;

  // Get outermost nesting loop and nesting level.
  std::pair<SC_NODE *, int> p_ret;
  p_ret = sc1->Get_outermost_nesting_loop();
  SC_NODE * outer_loop = p_ret.first;
  int nest_level = p_ret.second;

  while (next_sibling && (next_sibling != sc2)) {
    count++;
    FOR_ALL_ELEM(tmp, sc_list_iter, Init(sc2->Kids())) {
      if (Has_dependency(next_sibling, tmp))
	has_dep = TRUE;
    }

    if (!Can_be_speculative(next_sibling))
      has_non_sp = TRUE;

    SC_TYPE type = next_sibling->Type();
    WN * cond1 = next_sibling->Get_cond();
    WN * cond2 = NULL;

    if (!next_sibling->Is_sese()) 
      do_tail_dup = FALSE;
    else if (type == SC_IF) {
      BOOL can_merge = FALSE;
      if (nest_level >= 2) {
	// check whether next_sibling can potentially be if-merged with
	// sc2's next sibling.
	if (sc2_next_sibling) {
	  cond2 = sc2_next_sibling->Get_cond();
	  if( WN_Simp_Compare_Trees(cond1, cond2) == 0) {
	    can_merge = TRUE;
	    WN * wn_tmp = WN_kid0(expr1);
	    if (wn_tmp && WN_is_bit_op(wn_tmp) && !Is_invariant(outer_loop, WN_kid1(wn_tmp), 0)) {
	      wn_tmp = WN_kid0(cond2);
	      if (WN_is_bit_op(wn_tmp) && Is_invariant(outer_loop, WN_kid1(wn_tmp), 0)) {
		can_merge = FALSE;
	      }
	    }
	  }
	}
	if (!can_merge) {
	  // check whether next_sibling can potentially be if-merged with sc1's children.
	  for (int i = 0; i < 2; i++) {
	    SC_NODE * sc_tmp = NULL;
	    if (i == 0)
	      sc_tmp = sc1->Find_kid_of_type(SC_THEN)->Find_kid_of_type(SC_IF);
	    else
	      sc_tmp = sc1->Find_kid_of_type(SC_ELSE)->Find_kid_of_type(SC_IF);

	    while (sc_tmp) {
	      cond2 = sc_tmp->Get_cond();
	      if (WN_Simp_Compare_Trees(cond1, cond2) == 0) {
		can_merge = TRUE;
		break;
	      }
	      sc_tmp = sc_tmp->Next_sibling_of_type(SC_IF);
	    }
	  }
	}
      }

      if (!can_merge)
	do_tail_dup = FALSE;  
    }
    else if (type != SC_BLOCK)
      do_tail_dup = FALSE;
    
    next_sibling = next_sibling->Next_sibling();
  }

  Delete_val_range_maps();

  if (!has_dep) {
    // Do not reorder operations that can not be speculative.
    if (has_non_sp && !Can_be_speculative(sc2))
      return FALSE;
    else if (_action == DO_IFFLIP)
      return FALSE;
    // Make sure sc1 and sc2 are control equivalent if they are not adjacent to each other.
    else if ((count > 0) && !sc1->Is_ctrl_equiv(sc2))
      return FALSE;
    else
      return TRUE;
  }

  if (!do_tail_dup)
    return FALSE;

  // Heuristic: in the global pass, avoid tail duplication unless both
  // SC_NODEs contain loop.

  if ((_pass == PASS_GLOBAL)
      && (!sc1->Has_loop() || !sc2->Has_loop())
      && ( count != 1))
    return FALSE;

  if (!do_query) {
    next_sibling = sc1->Next_sibling();
  
    while (next_sibling && (next_sibling != sc2)) {
      Do_tail_duplication(next_sibling, sc1);
      next_sibling = sc1->Next_sibling();
    }

    // Swap SC_THEN and SC_ELSE for sc2.
    if (do_flip) {
      Do_flip(sc2);
    }
  }

  return TRUE;
}

// Query whether sc1 and sc2 have dependency on each other.
BOOL
CFG_TRANS::Has_dependency(SC_NODE * sc1, SC_NODE * sc2)
{
  if (sc1->Type() == SC_IF) {
    // Recursively query TRUE path and FALSE path
    // to detect value equality on two paths separately.
    if (Has_dependency(sc1->First_kid(), sc2)
	|| Has_dependency(sc1->Last_kid(), sc2))
      return TRUE;

    // Check head of SC_IF.
    BB_NODE * head = sc1->Get_bb_rep();

    if (Maybe_assigned_expr(head, sc2)
	|| Maybe_assigned_expr(sc2, head))
      return TRUE;
  }
  else  if (Maybe_assigned_expr(sc1, sc2)
	    || Maybe_assigned_expr(sc2, sc1))
    return TRUE;

  return FALSE;
}

// Query whether SC_NODE and BB_NODE have dependency on each other.
BOOL
CFG_TRANS::Has_dependency(SC_NODE * sc, BB_NODE * bb)
{
  if (sc->Type() == SC_IF) {
    if (Has_dependency(sc->First_kid(), bb)
	|| Has_dependency(sc->Last_kid(), bb))
      return TRUE;

    // Check head of SC_IF.
    BB_NODE * head = sc->Get_bb_rep();

    if (Maybe_assigned_expr(head, bb)
	|| Maybe_assigned_expr(bb, head))
      return TRUE;
  }
  else if (Maybe_assigned_expr(sc, bb)
	   || Maybe_assigned_expr(bb, sc))
    return TRUE;

  return FALSE;
}

// Query whether bb1 and bb2 have dependency on each other.
BOOL
CFG_TRANS::Has_dependency(BB_NODE * bb1, BB_NODE * bb2)
{
  if (Maybe_assigned_expr(bb1, bb2)
      || Maybe_assigned_expr(bb2, bb1))
    return TRUE;
  
  return FALSE;
}

// Query whether bb and wn have dependency on each other.
BOOL
CFG_TRANS::Has_dependency(BB_NODE * bb, WN * wn)
{
  STMT_ITER stmt_iter;
  WN * wn_iter;

  if (!WN_is_executable(wn))
    return FALSE;

  FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {
    if (Maybe_assigned_expr(wn_iter, wn)
	|| Maybe_assigned_expr(wn, wn_iter))
      return TRUE;
  }

  return FALSE;
}

// Query whether sc and wn have dependency on each other.
BOOL
CFG_TRANS::Has_dependency(SC_NODE * sc, WN * wn)
{
  if (!WN_is_executable(wn))
    return FALSE;
  
  BB_NODE * bb = sc->Get_bb_rep();

  if ((bb != NULL) && Has_dependency(bb, wn))
    return TRUE;

  BB_LIST * bb_list = sc->Get_bbs();

  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;

    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      if (Has_dependency(tmp, wn))
	return TRUE;
    }
  }

  SC_LIST * kids = sc->Kids();

  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE *tmp;
    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      if (Has_dependency(tmp, wn))
	return TRUE;
    }
  }
  
  return FALSE;
}

// Remove branch in the given BB_NODE *.
void
CFG_TRANS::Delete_branch(BB_NODE * bb_head)
{
  WN * branch_wn = bb_head->Branch_wn();
  WN * last_stmt = bb_head->Laststmt();

  if (branch_wn && (branch_wn == last_stmt)) {
    if (bb_head->Ifinfo())
      bb_head->Set_ifinfo(NULL);

    WN * prev_stmt = WN_prev(last_stmt);

    if (prev_stmt)
      WN_next(prev_stmt) = NULL;
    WN_prev(last_stmt) = NULL;

    bb_head->Set_laststmt(prev_stmt);
	      
    if (prev_stmt == NULL)
      bb_head->Set_firststmt(NULL);

    bb_head->Set_kind(BB_GOTO);
  }
}

// Check whether wn has a side effect that could modify the program's state.
BOOL
CFG_TRANS::Has_side_effect(WN * wn)
{
  if (!Can_be_speculative(wn))
    return TRUE;

  OPERATOR opr = WN_operator(wn);

  if (!WN_is_executable(wn))
    return FALSE;

  OPT_STAB * op_stab = _cu->Opt_stab();

  if (OPERATOR_is_store(opr)) {
    // scalar stores to local non-address taken variables have no side effect.
    if (OPERATOR_is_scalar_store(opr)) {
      AUX_ID aux_id = WN_aux(wn);
      if (!aux_id)
	return TRUE;
      
      ST * st = Get_st(wn);
      if (st && (ST_sclass(st) == SCLASS_AUTO)
	  && !ST_addr_passed(st) 
	  && !ST_addr_saved(st))
	return FALSE;
    }
  }
  // scalar load has no side effect.
  else if (OPERATOR_is_scalar_load(opr))
    return FALSE;
  
  return TRUE;
}

// Hash _def_cnt_map for all statements in the given sc.
void
CFG_TRANS::Hash_def_cnt_map(BB_NODE * bb)
{
  STMT_ITER stmt_iter;
  WN * wn_iter;

  FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {
    if (OPERATOR_is_scalar_store(WN_operator(wn_iter))
	&& !Has_side_effect(wn_iter)) {
      AUX_ID aux_id = WN_aux(wn_iter);
      unsigned long def_cnt = Get_def_cnt(aux_id);
      unsigned long new_cnt = def_cnt + 1;
      _def_cnt_map[aux_id] = new_cnt;

    }
  }
}

// Hash _def_cnt_map for all statements in the given sc.
void
CFG_TRANS::Hash_def_cnt_map(SC_NODE * sc)
{
  BB_NODE * bb = sc->Get_bb_rep();

  if (bb != NULL)
    Hash_def_cnt_map(bb);

  BB_LIST * bb_list = sc->Get_bbs();

  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;

    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      Hash_def_cnt_map(tmp);
    }
  }

  SC_LIST * kids = sc->Kids();

  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE * tmp;

    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      Hash_def_cnt_map(tmp);
    }
  }
}

// Get ST * if wn is a scalar load/store.
ST *
CFG_TRANS::Get_st(WN * wn)
{
  OPERATOR opr = WN_operator(wn);

  if (OPERATOR_is_scalar_load(opr) 
      || OPERATOR_is_scalar_store(opr)) {
    AUX_ID aux_id = WN_aux(wn);
    if (aux_id) {
      OPT_STAB * opt_stab = _cu->Opt_stab();
      return opt_stab->Aux_stab_entry(aux_id)->St();
    }
  }

  return NULL;
}

// Collect classified loops for the SC tree rooted at sc.
void
PRO_LOOP_FUSION_TRANS::Collect_classified_loops(SC_NODE * sc)
{
  if ((sc->Type() == SC_LOOP) && (sc->Class_id() != 0)) {
    if (_loop_list == NULL)
      _loop_list = (SC_LIST *) CXX_NEW(SC_LIST(sc), _pool);
    else
      _loop_list->Append(sc, _pool);
  }

  SC_LIST_ITER sc_list_iter(sc->Kids());
  SC_NODE * tmp;
  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    Collect_classified_loops(tmp);
  }
}

// Check whether it is worthy to do proactive loop fusion transformation
// for the given SC_NODE. Currently we avoid loops of small trip counts.

#define TRIP_COUNT_THRESHOLD 1000

BOOL
PRO_LOOP_FUSION_TRANS::Is_worthy(SC_NODE * sc)
{
  WN * wn_start = NULL;
  WN * wn_end = NULL;
  WN * wn_step = NULL;

  if (!sc->Get_bounds(&wn_start, &wn_end, &wn_step))
    return FALSE;

  WN * wn_lower = NULL;
  WN * wn_upper = NULL;
  WN * wn_incr = NULL;
  WN * wn_kid;
  WN * wn_kid0;
  WN * wn_kid1;
  OPERATOR opr;
  OPCODE op;

  // Find trip count lower bound.
  if (WN_operator(wn_start) == OPR_STID) 
    wn_lower = WN_kid0(wn_start);

  // Find trip count upper bound.
  if ((WN_operator(wn_end) == OPR_FALSEBR)
      || (WN_operator(wn_end) == OPR_TRUEBR)) {
    wn_kid = WN_kid0(wn_end);
    opr = WN_operator(wn_kid);
    op = WN_opcode(wn_kid);

    if (((opr == OPR_EQ) || (opr == OPR_NE) 
	 || (opr == OPR_LT) || (opr == OPR_LE)
	 || (opr == OPR_GT) || (opr == OPR_GE))
	&& MTYPE_is_integral(OPCODE_desc(op))) {
      wn_kid0 = WN_kid0(wn_kid);
      wn_kid1 = WN_kid1(wn_kid);
      wn_upper = (WN_operator(wn_kid0) == OPR_LDID) ? wn_kid1 :
	((WN_operator(wn_kid1) == OPR_LDID) ? wn_kid0 : NULL);
    }
  }
  
  // Find increment.
  if (wn_step && (WN_operator(wn_step) == OPR_STID)) {
    wn_kid = WN_kid0(wn_step);
    opr = WN_operator(wn_kid);

    if ((opr == OPR_ADD) || (opr == OPR_SUB)) {
      wn_kid0 = WN_kid0(wn_kid);
      wn_kid1 = WN_kid1(wn_kid);
      wn_incr = (WN_operator(wn_kid0) == OPR_LDID) ? wn_kid1 :
	((WN_operator(wn_kid1) == OPR_LDID) ? wn_kid0 : NULL);
    }
  }

  if (wn_lower && wn_upper && wn_incr) {
    if ((WN_operator(wn_lower) == OPR_INTCONST)
	&& (WN_operator(wn_incr) == OPR_INTCONST)
	&& (WN_operator(wn_upper) == OPR_INTCONST)) {
      int trip_count = abs(WN_const_val(wn_upper) - WN_const_val(wn_lower))
	/ abs(WN_const_val(wn_incr));
      
      if (trip_count < TRIP_COUNT_THRESHOLD)
	return FALSE;
    }
  }

  return TRUE;
}

// Initialize scratch data used for the PRO_LOOP_FUSION_TRANS sub-object.
void
PRO_LOOP_FUSION_TRANS::Init()
{
  _unlink_sc = CXX_NEW(STACK<SC_NODE *>(_pool), _pool); 
}

// Delete scratch data used for the PRO_LOOP_FUSION_TRANS sub-object.
void
PRO_LOOP_FUSION_TRANS::Delete()
{
  Delete_unlink_sc();
}

// Driver for proactive loop fusion.
void
PRO_LOOP_FUSION_TRANS::Doit(SC_NODE * sc)
{
  Classify_loops(sc);
  Init();
  Top_down_trans(sc);
  Delete();
}

// Find a pair of proactive loop fusion candidates for the SC_tree rooted at sc_root.
// a non-NULL sc_begin gives the initial search point in _loop_list.
// This routine can only be called from PRO_LOOP_FUSION_TRANS::Top_down_trans.
void
PRO_LOOP_FUSION_TRANS::Find_cand
(
 SC_NODE *  sc_root, 
 SC_NODE ** cand1_ptr, 
 SC_NODE ** cand2_ptr, 
 SC_NODE *  sc_begin
)
{
  SC_NODE * cand1 = NULL;
  SC_NODE * cand2 = NULL;
  SC_NODE * tmp1;
  SC_NODE * tmp2;
  SC_NODE * tmp;
  SC_LIST * list1;
  SC_LIST * list2;

  // Find a pair of loops such that the pair (loop1 and loop2)
  // 1.1 Has the same loop class id.
  // 1.2 Is a single entry single exit
  // 1.3 LCP is sc_root.
  // 1.4 Not control equivalent or not adjacent to each 
  // 1.5 No SC_LOOP on the path from loop1 to LCP, and on the path from
  //    loop2 to LCP excluding loop1 and loop2.  (Future implementation
  //    can relax this rule if loop transformation is invoked in this
  //    transformation).
  // 
  // When such a pair is found, walk up SC tree to find a pair of
  // transformation candidates such that the pair (cand1 and cand2)
  // 2.1 Is kid of LCP.
  // 2.2 SC_TYPE is a SC_IF or a SC_LOOP.
  // 2.3 For all the sibling nodes between cand1 and cand2 EXCLUSIVE,
  //    - It must be a SC_BLOCK, a SC_IF or a SC_LOOP.
  //    - In the case of a SC_BLOCK, it should have no dependency on loop1.
  //      It can be speculative.  It should have a single-entry and single-exit.
  //      It should also have a single successor that has a single predecessor.
  //      If cand1 is a loop, cand1 should be control equivalent to the SC_BLOCK
  //      for safe code motion.  The requirement of single successor is to make
  //      sure the successor can become a merge block for a loop after the code
  //      motion or a merge block for a SC_IF after tail duplication.
  //    - In the case of a SC_IF, there must exist at least one path that does not
  //      have dependency on both loop1 and loop2. 
  //    - In the case of SC_LOOP, it should have no dependency and control equivalent
  //      to cand1, and it can be speculative.  cand1 should be a SC_LOOP.
  //
  // Limit is used to control code size bloat due to head/tail duplication.
  // 
  // 2.4 For all sibling nodes between cand1 and cand2 INCLUSIVE, 
  //     - If it is a SC_IF, it must be well-behaved; its head has no dependency
  //       on preceding siblings unless the preceding sibling is a SC_IF that can 
  //       be if-merged with this SC_IF and its merge has no dependency on succeeding
  //       siblings unless the succeeding SC_IF is a SC_IF that can be if-merged with
  //       this SC_IF.   This is a legality check for head/tail duplication.  There 
  //       should exist at most one non single-entry-singe-exit (SESE) SC_IFs since we
  //       don't duplicate non-SESE SC_IFs to avoid complexity.  If cand2 is a loop, 
  //       its merge block must have a single predecessor so that it can be tail-duplicated
  //       into the SC_IF.

  for (list1 = _loop_list; list1; list1 = list1->Next()) {
    tmp1 = list1->Node();

    SC_NODE * prev_sibling = tmp1->Prev_sibling();
    if (prev_sibling && (prev_sibling->Class_id() == tmp1->Class_id()))
      continue;

    if ((sc_begin != NULL) && (tmp1 != sc_begin))
      continue;

    if (!sc_root->Is_pred_in_tree(tmp1))
      continue;

    // Condition 1.2
    if (!tmp1->Is_sese())
      continue;

    // Condition 1.5
    if (tmp1->Num_of_loops(sc_root, TRUE, TRUE) != 0)
      continue;

    // heuristic to avoid unprofitable loops.
    if (!Is_worthy(tmp1))
      continue;

    // Condition 1.2
    if (tmp1->Is_sese()) {
      for (list2 = list1->Next(); list2; list2 = list2->Next()) {
	tmp2 = list2->Node();

	// Avoid picking candidates in a sequence of loops that are adjacent to each other.
	BOOL is_adjacent = TRUE;
	tmp = tmp1->Next_sibling();
	while (tmp && (tmp != tmp2)) {
	  if ((tmp->Type() != SC_LOOP)
	      || (tmp->Class_id() != tmp1->Class_id())) {
	    is_adjacent = FALSE;
	    break;
	  }
	  tmp = tmp->Next_sibling();
	}
	if (is_adjacent)
	  continue;

	if (!sc_root->Is_pred_in_tree(tmp2))
	  continue;

	// Condition 1.2
	if (!tmp2->Is_sese())
	  continue;

	// Condition 1.5
	if (tmp2->Num_of_loops(sc_root, TRUE, TRUE) != 0)
	  continue;

	// Condition 1.1
	if (tmp1->Class_id() == tmp2->Class_id()) {
	  SC_NODE * lcp = tmp1->Find_lcp(tmp2);

	  // Avoid compgoto complication.
	  if (lcp->Find_kid_of_type(SC_COMPGOTO))
	    continue;
	  
	  // Condition 1.3
	  if (lcp == sc_root) {
	    BB_NODE * bb1 = tmp1->First_bb();
	    BB_NODE * bb2 = tmp2->First_bb();

	    // Condition 1.4
	    if (!bb1->Dominates(bb2)
		|| !bb2->Postdominates(bb1)
		|| tmp1->Next_sibling() != tmp2) {
	      cand1 = tmp1;
	      cand2 = tmp2;
	      
	      // Condition 2.1
	      while (cand1->Parent() != lcp)
		cand1 = cand1->Parent();

	      while (cand2->Parent() != lcp)
		cand2 = cand2->Parent();

	      // Condition 2.2
	      if (!Is_cand_type(cand1->Type())
		  || !Is_cand_type(cand2->Type())) {
		cand1 = NULL;
		cand2 = NULL;
		continue;
	      }

	      // Condition 2.4, inclusive check.
	      SC_NODE * sc_tmp1 = cand1;
	      SC_NODE * sc_tmp2;
	      int non_sese_count = 0;

	      while (sc_tmp1) {
		if (sc_tmp1->Type() == SC_IF) {
		  if (!sc_tmp1->Is_well_behaved()) {
		    cand1 = NULL;
		    cand2 = NULL;
		    break;
		  }

		  if (!sc_tmp1->Is_sese()) {
		    non_sese_count++;
		    if (non_sese_count > 1) {
		      cand1 = NULL;
		      cand2 = NULL;
		      break;
		    }
		  }

		  // Check preceding siblings		  
		  BB_NODE * bb_head = sc_tmp1->Head();
		  sc_tmp2 = cand1;

		  while (sc_tmp2) {
		    if (sc_tmp2 == sc_tmp1)
		      break;
		    
		    if ((sc_tmp2->Type() == SC_IF)
			&& IF_MERGE_TRANS::Is_candidate(sc_tmp2, sc_tmp1, TRUE)) {
		    }
		    else if (Has_dependency(sc_tmp2, bb_head)) {
		      cand1 = NULL;
		      cand2 = NULL;
		      break;
		    }
		    sc_tmp2 = sc_tmp2->Next_sibling();
		  }
		  
		  if ((cand1 == NULL) || (cand2 == NULL))
		    break;

		  // Check succeeding siblings
		  sc_tmp2 = sc_tmp1->Next_sibling();
		  BB_NODE * bb_merge = sc_tmp1->Merge();

		  while (sc_tmp2) {
		    if ((sc_tmp2->Type() == SC_IF)
			&& IF_MERGE_TRANS::Is_candidate(sc_tmp1, sc_tmp2, TRUE)) {

		    }
		    else if (Has_dependency(sc_tmp2, bb_merge)) {
		      cand1 = NULL;
		      cand2 = NULL;
		      break;
		    }

		    if (sc_tmp2 == cand2)
		      break;
		    sc_tmp2 = sc_tmp2->Next_sibling();
		  }
		  
		  if ((cand1 == NULL) || (cand2 == NULL))
		    break;

		  if ((cand2->Type() == SC_LOOP)
		      && (cand2->Merge()->Pred()->Multiple_bbs())) {
		    cand1 = NULL;
		    cand2 = NULL;
		    break;
		  }
		}

		if (sc_tmp1 == cand2)
		  break;

		sc_tmp1 = sc_tmp1->Next_sibling();
	      }

	      if ((cand1 == NULL) || (cand2 == NULL))
		continue;

	      // Condition 2.3, exclusive check.
	      SC_NODE * next = cand1->Next_sibling();
	      int stmt_count = 0;  
	      int orig_stmt_count = 0;
	      
	      while (next != cand2) {
		SC_TYPE next_type = next->Type();
		
		if (next_type == SC_BLOCK) {
		  BB_NODE * next_last_bb = next->Last_bb();
		  if (!next_last_bb->Succ()
		      || next_last_bb->Succ()->Multiple_bbs()
		      || next_last_bb->Succ()->Node()->Pred()->Multiple_bbs()
		      ) {
		    cand1 = NULL;
		    cand2 = NULL;
		    break;
		  }
		  
		  if (!next->Is_sese()) {
		    cand1 = NULL;
		    cand2 = NULL;
		    break;
		  }

		  BB_LIST_ITER bb_list_iter;
		  BB_NODE * tmp;

		  if (cand1->Type() == SC_LOOP) {
		    BB_NODE * cand1_first_bb = cand1->First_bb();

		    FOR_ALL_ELEM(tmp, bb_list_iter,Init(next->Get_bbs())) {
		      if (!cand1_first_bb->Dominates(tmp)
			  || !tmp->Postdominates(cand1_first_bb)) {
			cand1 = NULL;
			cand2 = NULL;
			break;
		      }
		    }
		    if (cand1 == NULL)
		      break;
		  }
		  
		  // No dependency on loop1.
		  if (Has_dependency(tmp1, next)) {
		    cand1 = NULL;
		    cand2 = NULL;
		    break;
		  }

		  // Can be speculative
		  if (!Can_be_speculative(next)) {
		    cand1 = NULL;
		    cand2 = NULL;
		    break;
		  }
		}
		else if (next_type == SC_IF) {
		  int i;
		  BOOL find_path = FALSE;
		  for (i = 0; i <= 1; i++) {
		    SC_NODE * sc_tmp = (i == 0) ? next->First_kid() : next->Last_kid();
		    if (!Has_dependency(sc_tmp, tmp1)
			&& !Has_dependency(sc_tmp, tmp2)) {
		      find_path = TRUE;
		      break;
		    }
		  }

		  if (!find_path) {
		    cand1 = NULL;
		    cand2 = NULL;
		    break;
		  }
		  
		  // Control code size bloat due to head/tail duplication.
		  if ((WOPT_Tail_Dup_Max_Clone > 0)
		      && ((_code_bloat_count + stmt_count - orig_stmt_count)
			  >= WOPT_Tail_Dup_Max_Clone)) {
		    cand1 = NULL;
		    cand2 = NULL;
		    break;
		  }

		  // Head/tail duplication doubles statement count.
		  stmt_count *= 2;
		}
		else if (next_type == SC_LOOP) {
		  if (!Do_ext_trans()
		      || (cand1->Type() != SC_LOOP)
		      || !Can_be_speculative(next)
		      || Has_dependency(cand1, next) 
		      || !cand1->Is_ctrl_equiv(next)) {
		    cand1 = NULL;
		    cand2 = NULL;
		    break;
		  }
		}
		else {
		  cand1 = NULL;
		  cand2 = NULL;
		  break;
		}
		 
		int cur_count = next->Executable_stmt_count();
		orig_stmt_count += cur_count;
		stmt_count += cur_count;
		next = next->Next_sibling();
	      }
		
	      if (cand1 && cand2) 
		break;
	    }
	  }
	}
      }

      if (cand1 && cand2)
	break;
    }
  }

  *cand1_ptr = cand1;
  *cand2_ptr = cand2;
}

void
CFG_TRANS::Clear()
{
  _cu = NULL;
  _trace = FALSE;
  _dump = FALSE;
  _transform_count = 0;
  _pool = NULL;
  _code_bloat_count = 0;
  _true_val = NULL;
  _unlink_sc = NULL;
  _tmp_stack = NULL;
  _ext_trans = EXT_TRANS_NONE;
  _current_scope = NULL;
}

// Get COMP_UNIT *
COMP_UNIT * 
CFG_TRANS::Get_cu()
{
  return _cu;
}

// Move BB_NODEs in sc2 before the first BB_NODE in sc1, where 'sc1' and 'sc2' 
// are adjacent siblings.
// The caller of this routine should be responsible for the legality check.
void
CFG_TRANS::Do_code_motion(SC_NODE * sc1, SC_NODE * sc2)
{
  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return;

  FmtAssert((sc1->Parent() == sc2->Parent()), ("Expect sibling SC_NODEs"));
  BB_NODE * first_bb1 = sc1->First_bb();
  BB_NODE * first_bb2 = sc2->First_bb();
  CFG * cfg = _cu->Cfg();
  SC_NODE * sc1_prev = sc1->Prev_sibling();
  SC_NODE * sc2_prev = sc2->Prev_sibling();

  // Other kinds of loops not tested yet.
  if (sc1->Type() == SC_LOOP)
    FmtAssert((sc1->Loopinfo()->Is_flag_set(LOOP_PRE_DO)), ("TODO: other loops"));    

  if (_trace) {
    printf("\n\t\t Code-motion (SC%d,SC%d)\n", 
	   sc1->Id(), sc2->Id());
  }

  BB_LIST_ITER bb_list_iter;
  BB_NODE * tmp;
  SC_TYPE sc2_type = sc2->Type();

  // Fix pred/succ
  FOR_ALL_ELEM(tmp, bb_list_iter, Init(first_bb1->Pred())) {
    if (sc1->Type() == SC_LOOP)
      FmtAssert(!sc1->Contains(tmp), ("TODO: back edge"));
    tmp->Replace_succ(first_bb1, first_bb2);

    if (cfg->Feedback()) 
      cfg->Feedback()->Move_edge_dest(tmp->Id(), first_bb1->Id(), first_bb2->Id());
  }

  if (sc2_type == SC_BLOCK) {
    BB_NODE * last_bb2 = sc2->Last_bb();
    // last_bb2_succ will become new loop merge
    FmtAssert((last_bb2->Succ()->Len() == 1), ("Expect single successor"));
    BB_NODE * last_bb2_succ = last_bb2->Succ()->Node();
    
    FOR_ALL_ELEM(tmp, bb_list_iter, Init(first_bb2->Pred())) {
      if (tmp->Is_branch_to(first_bb2)) {
	WN * branch_wn = tmp->Branch_wn();
	cfg->Add_label_with_wn(last_bb2_succ);
	WN_label_number(branch_wn) = last_bb2_succ->Labnam();
      }
      
      tmp->Replace_succ(first_bb2, last_bb2_succ);
      if (cfg->Feedback())
	cfg->Feedback()->Move_edge_dest(tmp->Id(), first_bb2->Id(), last_bb2_succ->Id());
    }
    last_bb2_succ->Set_pred(first_bb2->Pred());
    
    first_bb2->Set_pred(first_bb1->Pred());
    last_bb2->Replace_succ(last_bb2_succ, first_bb1);

    if (cfg->Feedback())
      cfg->Feedback()->Move_edge_dest(last_bb2->Id(), last_bb2_succ->Id(), first_bb1->Id());

    BB_LIST * new_pred = CXX_NEW(BB_LIST(last_bb2), cfg->Mem_pool());
    first_bb1->Set_pred(new_pred);

    // Fix up prev/next 
    BB_NODE * first_bb1_prev = first_bb1->Prev();
    BB_NODE * first_bb2_prev = first_bb2->Prev();
    BB_NODE * last_bb2_next = last_bb2->Next();

    first_bb1_prev->Set_next(first_bb2);
    first_bb2->Set_prev(first_bb1_prev);
    last_bb2->Set_next(first_bb1);
    first_bb1->Set_prev(last_bb2);
  
    first_bb2_prev->Set_next(last_bb2_next);
    last_bb2_next->Set_prev(first_bb2_prev);

    if (last_bb2_succ->Prev() == last_bb2)
      last_bb2_succ->Set_prev(first_bb1_prev);

    if (sc1->Type() == SC_LOOP) {
      // Update loop info.
      BB_NODE * merge = sc1->Merge();
      FmtAssert(merge == first_bb2, ("Unexpected merge block"));
      sc1->Loopinfo()->Set_merge(last_bb2_succ);

      // fix label on loop exit.
      cfg->Add_label_with_wn(last_bb2_succ);
      BB_NODE * bb_exit = sc1->Exit();
      WN * branch_wn = bb_exit->Branch_wn();
      FmtAssert(WN_label_number(branch_wn), ("Null label"));
      WN_label_number(branch_wn) = last_bb2_succ->Labnam();
    }
  }
  else if (sc2_type == SC_LOOP) {
    BB_NODE * bb_merge2 = sc2->Merge();
    BB_NODE * bb_merge1 = sc1->Merge();

    FmtAssert(bb_merge1 == first_bb2, ("Unexpected merge."));
    
    FOR_ALL_ELEM(tmp, bb_list_iter, Init(bb_merge1->Pred())) {
      tmp->Replace_succ(bb_merge1, bb_merge2);
      if (cfg->Feedback()) 
	  cfg->Feedback()->Move_edge_dest(tmp->Id(), bb_merge1->Id(), bb_merge2->Id());
    }

    FOR_ALL_ELEM(tmp, bb_list_iter, Init(bb_merge2->Pred())) {
      tmp->Replace_succ(bb_merge2, first_bb1);
      if (cfg->Feedback())
	cfg->Feedback()->Move_edge_dest(tmp->Id(), bb_merge2->Id(), first_bb1->Id());
    }

    BB_LIST * bb_list = first_bb2->Pred();
    first_bb2->Set_pred(first_bb1->Pred());
    first_bb1->Set_pred(bb_merge2->Pred());
    bb_merge2->Set_pred(bb_list);

    BB_NODE * tmp2 = first_bb1->Prev();
    tmp = bb_merge2->Prev();
    tmp->Set_next(first_bb1);
    first_bb1->Set_prev(tmp);

    tmp = bb_merge1->Prev();
    tmp->Set_next(bb_merge2);
    bb_merge2->Set_prev(tmp);

    tmp2->Set_next(first_bb2);
    first_bb2->Set_prev(tmp2);

    // Update loop info
    sc1->Loopinfo()->Set_merge(bb_merge2);
    sc2->Loopinfo()->Set_merge(first_bb1);

    // Fix label on loop exit
    BB_NODE * bb_exit = sc1->Exit();
    WN * branch_wn = bb_exit->Branch_wn();
    FmtAssert(WN_label_number(branch_wn), ("Null label"));
    WN_label_number(branch_wn) = bb_merge2->Labnam();
  }
  else if (sc2_type == SC_IF) {
    BB_NODE * bb_merge = sc2->Merge();
    SC_TYPE type = sc1->Type();

    if ((type == SC_IF) || (type == SC_LOOP))
      FmtAssert(sc1->Merge() == first_bb2, ("Unexpected merge."));
    
    FOR_ALL_ELEM(tmp, bb_list_iter, Init(bb_merge->Pred())) {
      tmp->Replace_succ(bb_merge, first_bb1);
      if (cfg->Feedback())
	cfg->Feedback()->Move_edge_dest(tmp->Id(), bb_merge->Id(), first_bb1->Id());
    }

    FOR_ALL_ELEM(tmp, bb_list_iter, Init(first_bb2->Pred())) {
      tmp->Replace_succ(first_bb2, bb_merge);
      if (cfg->Feedback())
	cfg->Feedback()->Move_edge_dest(tmp->Id(), first_bb2->Id(), bb_merge->Id());
    }

    BB_LIST * bb_list = first_bb2->Pred();
    first_bb2->Set_pred(first_bb1->Pred());
    first_bb1->Set_pred(bb_merge->Pred());
    bb_merge->Set_pred(bb_list);

    // Fix up prev/next
    BB_NODE * tmp2 = first_bb2->Prev();
    tmp = first_bb1->Prev();
    tmp->Set_next(first_bb2);
    first_bb2->Set_prev(tmp);
    tmp = bb_merge->Prev();
    bb_merge->Set_prev(tmp2);
    tmp2->Set_next(bb_merge);
    tmp->Set_next(first_bb1);
    first_bb1->Set_prev(tmp);

    if (type == SC_LOOP)
      sc1->Loopinfo()->Set_merge(bb_merge);
    sc2->Head()->Ifinfo()->Set_merge(first_bb1);
    
    // fix label on loop exit
    if (type == SC_LOOP) {
      BB_NODE * bb_exit = sc1->Exit();
      WN * branch_wn = bb_exit->Branch_wn();
      FmtAssert(WN_label_number(branch_wn), ("Null label"));
      cfg->Add_label_with_wn(bb_merge);
      WN_label_number(branch_wn) = bb_merge->Labnam();
    }
    
  }
  else {
    FmtAssert(FALSE, ("Unimplemented code motion"));
  }
  
  SC_NODE * parent = sc1->Parent();

  // Fix label on branch to sc1.
  if (first_bb1->Labnam() != 0) {
    cfg->Add_label_with_wn(first_bb2);
    
    FOR_ALL_ELEM(tmp, bb_list_iter, Init(first_bb2->Pred())) {
      FmtAssert(!sc1->Contains(tmp), ("TODO: back edge"));

      if (tmp->Is_branch_to(first_bb1)) {
	WN * branch_wn = tmp->Branch_wn();	  
	WN_label_number(branch_wn) = first_bb2->Labnam();
	  
	if (parent->Type() != SC_IF) {
	  // If first_bb1 has a label WN and first_bb2 does not have
	  // a label WN, create one for first_bb2.	    
	  WN * wn_label = first_bb1->Firststmt();
	  if (wn_label && (WN_operator(wn_label) == OPR_LABEL)) {
	    wn_label = first_bb2->Firststmt();
	    if (!wn_label || (WN_operator(wn_label) != OPR_LABEL)) {
	      wn_label = WN_CreateLabel(0, first_bb2->Labnam(), 0, NULL);
	      cfg->Prepend_wn_in(first_bb2, wn_label);
	    }
	  }
	}
      }
    }
  }

  if (sc2_type == SC_LOOP) {
    // fix label on loop exit.
    BB_NODE * bb_exit = sc2->Exit();
    WN * branch_wn = bb_exit->Branch_wn();
    FmtAssert(WN_label_number(branch_wn), ("Null label"));
    if (first_bb1->Labnam() == 0)
      cfg->Add_label_with_wn(first_bb1);
    WN_label_number(branch_wn) = first_bb1->Labnam();
  }
  else if (sc2_type == SC_IF) {
    BB_NODE * bb_merge = sc1->Merge();
    FOR_ALL_ELEM(tmp, bb_list_iter, Init(first_bb1->Pred())) {
      if (tmp->Is_branch_to(bb_merge)) {
	WN * branch_wn = tmp->Branch_wn();
	FmtAssert(WN_label_number(branch_wn), ("Null label"));
	cfg->Add_label_with_wn(first_bb1);
	WN_label_number(branch_wn) = first_bb1->Labnam();
      }
    }
  }

  // swap sc1 and sc2 in their parent's kids.
  SC_LIST * cur_list;

  for (cur_list = parent->Kids(); cur_list; cur_list = cur_list->Next()) {
    SC_NODE * cur_node = cur_list->Node();
    if (cur_node == sc1)
      cur_list->Set_node(sc2);
    else if (cur_node == sc2)
      cur_list->Set_node(sc1);
  }

  // Fix previous sibling's merge info 
  SC_NODE * prev_sibling = sc2->Prev_sibling();

  if (prev_sibling) {
    BB_NODE * merge = prev_sibling->Merge();
    if (merge) {
      FmtAssert((merge == first_bb1), ("Unexpected merge block"));
      prev_sibling->Set_merge(first_bb2);
    }
  }

  // Fix parent info.
  Fix_parent_info(sc1, sc2);

  if (sc1_prev)
    cfg->Fix_info(sc1_prev);
  
  if (sc2_prev)
    cfg->Fix_info(sc2_prev);

  cfg->Invalidate_and_update_aux_info(FALSE);
  cfg->Invalidate_loops();
  Inc_transform_count();
}

// Insert a single-entry-single-exit region defined by (src_entry, src_exit) between
// dst_begin and dst_end.
// Before insertion, src_entry should have no predecessor, src_exit should have at most
// one successor. Also fix Prev/Next links.
void
CFG_TRANS::Insert_region
(
 BB_NODE * src_entry, 
 BB_NODE * src_exit, 
 BB_NODE * dst_begin, 
 BB_NODE * dst_end,
 MEM_POOL * pool
)
{
  FmtAssert(!src_entry->Pred(), ("Expect no predecessor"));
  FmtAssert((!src_exit->Succ() || (src_exit->Succ()->Len() == 1)), ("Expect unique successor"));

  dst_begin->Replace_succ(dst_end, src_entry);
  dst_end->Replace_pred(dst_begin, src_exit);
  src_entry->Append_pred(dst_begin, pool);


  // Next() is normally the last successor. If src_exit already has a successor, 
  // the successor must be in the same region and is the Next(). Therefore
  // we use prepend here.
  src_exit->Prepend_succ(dst_end, pool);

  BB_NODE * src_last = NULL;
  BB_NODE * bb_tmp = src_entry;
  while (bb_tmp) {
    src_last = bb_tmp;
    bb_tmp = bb_tmp->Next();
  }

  FmtAssert(src_last, ("Last BB_NODE not found"));

  // Insert into prev/next links.
  if (dst_end->Pred()->Len() == 1) {
    BB_NODE * old_prev = dst_end->Prev();
    old_prev->Set_next(src_entry);
    src_entry->Set_prev(old_prev);
    dst_end->Set_prev(src_last);
    src_last->Set_next(dst_end);
  }
  else {
    BB_NODE * old_next = dst_begin->Next();
    dst_begin->Set_next(src_entry);
    src_entry->Set_prev(dst_begin);
    src_last->Set_next(old_next);
    old_next->Set_prev(src_last);
  }
}

// Fix parent ifinfo and loop info after sc1 is head-duplicated into sc2
// or after sc2 is moved above sc1.  
void
CFG_TRANS::Fix_parent_info(SC_NODE * sc1, SC_NODE * sc2)
{
  SC_NODE * parent = sc2->Parent();
  SC_TYPE parent_type = parent->Type();

  if ((parent_type == SC_THEN) || (parent_type == SC_ELSE)) {
    BB_NODE * bb_cond = parent->Parent()->Head();
    BB_IFINFO * ifinfo = bb_cond->Ifinfo();
    
    if (parent_type == SC_THEN)
      ifinfo->Set_then(parent->First_bb());
    else
      ifinfo->Set_else(parent->First_bb());
  }
  else if (parent_type == SC_LP_BODY) {
    BB_LOOP * loop = parent->Parent()->Loopinfo();
    if (loop->Body() == sc1->First_bb()) {
      loop->Set_body(sc2->First_bb());
    }
  }
}

// Do head duplication of sc_src into sc_dst.
// Caller of this routine should take the responsiblity of legality check.
void
CFG_TRANS::Do_head_duplication(SC_NODE * sc_src, SC_NODE * sc_dst)
{
  FmtAssert((sc_dst->Type() == SC_IF), ("Expect a SC_IF"));
  FmtAssert(sc_src->Is_sese(), ("Expect a single entry single exit"));
  CFG * cfg = _cu->Cfg();

  // Other kinds of loop not tested yet.    
  SC_TYPE type = sc_src->Type();

  if (type == SC_LOOP) 
    FmtAssert((sc_src->Loopinfo()->Is_flag_set(LOOP_PRE_DO)), ("TODO: Test code motion"));  
  else if (type == SC_IF) {
    FmtAssert(sc_src->Next_sibling() == sc_dst, ("Expect adjacent nodes"));
    cfg->Insert_block_before(sc_dst);
  }

  if (_trace) {
    printf("\n\t\t Head-duplication (SC%d,SC%d)\n", 
	   sc_src->Id(), sc_dst->Id());
  }

  SC_NODE * sc_prev = sc_src->Prev_sibling();
  BB_NODE * dst_merge = sc_dst->Merge();
  BB_NODE * dst_head = sc_dst->Head();
  BB_NODE * dst_else = sc_dst->Else();
  BB_NODE * dst_then = sc_dst->Then();
  BB_IFINFO * ifinfo;
  float scale = 0.0;
  BB_LIST * bb_list;
  FB_FREQ freq;
  IDTYPE edge;

  if (cfg->Feedback()) {
    freq = cfg->Feedback()->Get_edge_freq(dst_head->Id(), dst_else->Id()) 
      / cfg->Feedback()->Get_node_freq_out(dst_head->Id()) * 1.0;
    if (freq.Known())
      scale = freq.Value();
  }
  MEM_POOL * pool = cfg->Mem_pool();
  SC_NODE * sc_merge = NULL;
  SC_NODE * sc_new = cfg->Clone_sc(sc_src, TRUE, scale, &sc_merge);
  FmtAssert(sc_new, ("NULL clone"));
 
  // Fix CFG.

  BB_NODE * new_entry;
  BB_NODE * new_exit;
  BB_NODE * old_entry;
  BB_NODE * old_exit;

  BB_LOOP * loopinfo;
  SC_NODE * sc_insert_before;
  BB_LIST_ITER bb_list_iter;
  BB_NODE * tmp;
  BB_NODE * tmp2;
  BB_NODE * src_merge;
  WN * branch_wn;
  SC_NODE * sc_then;
  SC_NODE * sc_else;
  SC_NODE * sc_tmp;

  switch (type) {
  case SC_LOOP:
    old_entry = sc_src->Head();
    src_merge = sc_src->Merge();
    old_exit = sc_src->Exit();
    new_entry = cfg->Get_cloned_bb(old_entry);
    new_exit = cfg->Get_cloned_bb(old_exit);

    // Fix label on if-branch.
    if (dst_head->Is_branch_to(dst_else)) {
      branch_wn = dst_head->Branch_wn();
      cfg->Add_label_with_wn(new_entry);
      WN_label_number(branch_wn) = new_entry->Labnam();
    }

    // Insert BB_NODEs into else-path.
    Insert_region(new_entry, new_exit, dst_head, dst_else, pool);

    if (cfg->Feedback()) {
      FB_EDGE_TYPE edge_type = cfg->Feedback()->Get_edge_type(old_exit->Id(), src_merge->Id());
      cfg->Feedback()->Move_edge_dest(dst_head->Id(), dst_else->Id(), new_entry->Id());
      cfg->Feedback()->Add_edge(new_exit->Id(), dst_else->Id(), 
				edge_type,
				cfg->Feedback()->Get_edge_freq(dst_head->Id(), new_entry->Id()));
    }
    
    // Prepend sc_new to SC_ELSE's kids.
    sc_insert_before = sc_dst->Find_kid_of_type(SC_ELSE);
    sc_insert_before->Prepend_kid(sc_new);
    sc_new->Set_parent(sc_insert_before);

    // Add merge to loopinfo.
    loopinfo = sc_new->Loopinfo();
    loopinfo->Set_merge(dst_else);

    // Fix label on loop exit
    cfg->Add_label_with_wn(dst_else);
    branch_wn = new_exit->Branch_wn();
    FmtAssert(WN_label_number(branch_wn), ("NULL label"));
    WN_label_number(branch_wn) = dst_else->Labnam();

    // Update sc_dst's ifinfo
    ifinfo = dst_head->Ifinfo();
    ifinfo->Set_else(new_entry);

    // Disconnect BB_NODEs in src_src from CFG and then insert it into then-path.
    FOR_ALL_ELEM(tmp, bb_list_iter, Init(old_entry->Pred())) {
      tmp->Replace_succ(old_entry, src_merge);

      if (tmp->Is_branch_to(old_entry)) {
	branch_wn = tmp->Branch_wn();
	FmtAssert(src_merge->Labnam(), ("Expect a non-NULL label"));
	WN_label_number(branch_wn) = src_merge->Labnam();
      }

      if (cfg->Feedback()) 
	cfg->Feedback()->Move_edge_dest(tmp->Id(), old_entry->Id(), src_merge->Id());
    }
    src_merge->Remove_pred(old_exit, pool);
    src_merge->Set_pred(old_entry->Pred());
    old_entry->Set_pred(NULL);
    old_exit->Remove_succ(src_merge, pool);

    if (cfg->Feedback()) {
       cfg->Feedback()->Move_edge_dest(dst_head->Id(), dst_then->Id(), old_entry->Id());
       cfg->Feedback()->Move_edge_dest(old_exit->Id(), src_merge->Id(), dst_then->Id());
       edge = cfg->Feedback()->Get_edge(old_exit->Id(), dst_then->Id());
       freq = cfg->Feedback()->Get_edge_freq(dst_head->Id(), old_entry->Id());
       cfg->Feedback()->Change_edge_freq(edge, freq);
     }

    tmp = src_merge->Prev();
    tmp->Set_next(NULL);
    tmp = old_entry->Prev();
    tmp->Set_next(src_merge);
    src_merge->Set_prev(tmp);
    old_entry->Set_prev(NULL);

    // Fix label on if-branch.
    if (dst_head->Is_branch_to(dst_then)) {
      branch_wn = dst_head->Branch_wn();
      cfg->Add_label_with_wn(old_entry);
      WN_label_number(branch_wn) = old_entry->Labnam();
    }

    Insert_region(old_entry, old_exit, dst_head, dst_then, pool);

    // Unlink sc_src from SC tree and prepend it to SC_THEN's kids.
    sc_src->Unlink();
    sc_insert_before = sc_dst->Find_kid_of_type(SC_THEN);
    sc_insert_before->Prepend_kid(sc_src);
    sc_src->Set_parent(sc_insert_before);

    // Update loopinfo merge
    loopinfo = sc_src->Loopinfo();
    loopinfo->Set_merge(dst_then);

    // Fix label on loop exit.
    cfg->Add_label_with_wn(dst_then);
    branch_wn = old_exit->Branch_wn();
    FmtAssert(WN_label_number(branch_wn), ("NULL label"));
    WN_label_number(branch_wn) = dst_then->Labnam();
    
    // Update sc_dst's ifinfo
    ifinfo = dst_head->Ifinfo();
    ifinfo->Set_then(old_entry);
    
    Fix_parent_info(sc_src, sc_dst);
    break;
  case SC_BLOCK:
    old_entry = sc_src->First_bb();
    old_exit = sc_src->Last_bb();
    new_entry = cfg->Get_cloned_bb(old_entry);
    new_exit = cfg->Get_cloned_bb(old_exit);

    FmtAssert((old_exit->Succ()->Len() == 1), ("Expect single successor"));
    tmp2 = old_exit->Nth_succ(0);

    FOR_ALL_ELEM(tmp, bb_list_iter, Init(old_entry->Pred())) {
      tmp->Replace_succ(old_entry, tmp2);

      if (tmp->Is_branch_to(old_entry)) {
	branch_wn = tmp->Branch_wn();
	cfg->Add_label_with_wn(dst_head);
	WN_label_number(branch_wn) = dst_head->Labnam();
      }

      if (cfg->Feedback()) 
	cfg->Feedback()->Move_edge_dest(tmp->Id(), old_entry->Id(), tmp2->Id());
    }

    if (cfg->Feedback())
      cfg->Feedback()->Delete_edge(old_exit->Id(), tmp2->Id());

    bb_list = tmp2->Pred();
    while (bb_list)
      bb_list = bb_list->Remove(bb_list->Node(), pool);

    tmp2->Set_pred(old_entry->Pred());
    
    bb_list = old_exit->Succ();
    while (bb_list)
      bb_list = bb_list->Remove(bb_list->Node(), pool);

    tmp = old_entry->Prev();
    tmp2 = old_exit->Next();
    tmp->Set_next(tmp2);
    tmp2->Set_prev(tmp);
    
    old_entry->Set_pred(NULL);
    old_exit->Set_succ(NULL);
    old_entry->Set_prev(NULL);
    old_exit->Set_next(NULL);

    if (cfg->Feedback()) {
      freq = cfg->Feedback()->Get_edge_freq(dst_head->Id(), dst_then->Id());
      cfg->Feedback()->Move_edge_dest(dst_head->Id(), dst_then->Id(), old_entry->Id());
      cfg->Feedback()->Add_edge(old_exit->Id(), dst_then->Id(), FB_EDGE_OUTGOING, freq);
      freq = cfg->Feedback()->Get_edge_freq(dst_head->Id(), dst_else->Id());
      cfg->Feedback()->Move_edge_dest(dst_head->Id(), dst_else->Id(), new_entry->Id());
      cfg->Feedback()->Add_edge(new_exit->Id(), dst_else->Id(), FB_EDGE_OUTGOING, freq);
    }

    if (dst_head->Is_branch_to(dst_else)) {
      branch_wn = dst_head->Branch_wn();
      cfg->Add_label_with_wn(new_entry);
      WN_label_number(branch_wn) = new_entry->Labnam();
    }

    Insert_region(old_entry, old_exit, dst_head, dst_then, pool);
    Insert_region(new_entry, new_exit, dst_head, dst_else, pool);
    
    sc_src->Unlink();

    sc_then = sc_dst->Find_kid_of_type(SC_THEN);
    sc_tmp = sc_then->First_kid();

    if (sc_tmp->Type() == SC_BLOCK) {
      bb_list = sc_tmp->Get_bbs();
      FOR_ALL_ELEM(tmp, bb_list_iter, Init(bb_list)) {
	sc_src->Append_bbs(tmp);
      }
      sc_tmp->Set_bbs(sc_src->Get_bbs());
      sc_src->Set_bbs(NULL);
    }
    else {
      sc_then->Prepend_kid(sc_src);
      sc_src->Set_parent(sc_then);
    }

    sc_else = sc_dst->Find_kid_of_type(SC_ELSE);
    sc_tmp = sc_else->First_kid();

    if (sc_tmp->Type() == SC_BLOCK) {
      bb_list = sc_tmp->Get_bbs();
      FOR_ALL_ELEM(tmp, bb_list_iter, Init(bb_list)) {
	sc_new->Append_bbs(tmp);
      }
      sc_tmp->Set_bbs(sc_new->Get_bbs());
      sc_new->Set_bbs(NULL);
      sc_new->Delete();
    }
    else {
      sc_else->Prepend_kid(sc_new);
      sc_new->Set_parent(sc_else);
    }

    ifinfo = dst_head->Ifinfo();
    ifinfo->Set_then(old_entry);
    ifinfo->Set_else(new_entry);

    cfg->Fix_info(sc_dst->Get_real_parent());
    break;
  case SC_IF:
    old_entry = sc_src->Head();
    old_exit = sc_src->Merge();
    new_entry = sc_new->Head();
    new_exit = sc_new->Merge();

    // Fix label on if-branch.
    if (dst_head->Is_branch_to(dst_else)) {
      branch_wn = dst_head->Branch_wn();
      cfg->Add_label_with_wn(new_entry);
      WN_label_number(branch_wn) = new_entry->Labnam();
    }
    
    // Insert BB_NODEs into else-path.
    Insert_region(new_entry, new_exit, dst_head, dst_else, pool);

    if (cfg->Feedback()) {
      FB_EDGE_TYPE edge_type = cfg->Feedback()->Get_edge_type(dst_head->Id(), dst_else->Id());
      cfg->Feedback()->Move_edge_dest(dst_head->Id(), dst_else->Id(), new_entry->Id());
      cfg->Feedback()->Add_edge(new_exit->Id(), dst_else->Id(), 
				edge_type,
				cfg->Feedback()->Get_edge_freq(dst_head->Id(), new_entry->Id()));
    }

    // Prepend sc_new and src_merge to SC_ELSE's kids.
    sc_insert_before = sc_dst->Find_kid_of_type(SC_ELSE);
    sc_insert_before->Prepend_kid(sc_merge);
    sc_merge->Set_parent(sc_insert_before);
    sc_insert_before->Prepend_kid(sc_new);
    sc_new->Set_parent(sc_insert_before);
    
    // Update sc_dst's ifinfo
    ifinfo = dst_head->Ifinfo();
    ifinfo->Set_else(new_entry);

    // Disconnect BB_NODEs in src from CFG and then insert it into the then-path.
    FOR_ALL_ELEM(tmp, bb_list_iter, Init(old_entry->Pred())) {
      tmp->Replace_succ(old_entry, dst_head);
      
      if (tmp->Is_branch_to(old_entry)) {
	branch_wn = tmp->Branch_wn();
	cfg->Add_label_with_wn(dst_head);	
	WN_label_number(branch_wn) = dst_head->Labnam();
      }

      if (cfg->Feedback())
	cfg->Feedback()->Move_edge_dest(tmp->Id(), old_entry->Id(), dst_head->Id());
    
    }
    
    dst_head->Set_pred(old_entry->Pred());
    old_entry->Set_pred(NULL);
    old_exit->Remove_succ(dst_head, pool);
    old_exit->Set_next(NULL);

    if (cfg->Feedback()) {
      FB_EDGE_TYPE edge_type = cfg->Feedback()->Get_edge_type(dst_head->Id(), dst_then->Id());
      cfg->Feedback()->Move_edge_dest(dst_head->Id(), dst_then->Id(), old_entry->Id());
      cfg->Feedback()->Move_edge_dest(old_exit->Id(), dst_head->Id(), dst_then->Id());
      edge = cfg->Feedback()->Get_edge(old_exit->Id(), dst_then->Id());
      freq = cfg->Feedback()->Get_edge_freq(dst_head->Id(), old_entry->Id());
      cfg->Feedback()->Change_edge_freq(edge, freq);
    }

    tmp = old_entry->Prev();
    tmp->Set_next(dst_head);
    dst_head->Set_prev(tmp);
    old_entry->Set_prev(NULL);
    
    // Fix label on if-branch.
    if (dst_head->Is_branch_to(dst_then)) {
      branch_wn = dst_head->Branch_wn();
      cfg->Add_label_with_wn(old_entry);
      WN_label_number(branch_wn) = old_entry->Labnam();
    }

    Insert_region(old_entry, old_exit, dst_head, dst_then, pool);

    sc_insert_before = sc_dst->Find_kid_of_type(SC_THEN);
    sc_tmp = sc_src->Next_sibling();
    sc_tmp->Unlink();
    sc_insert_before->Prepend_kid(sc_tmp);
    sc_tmp->Set_parent(sc_insert_before);
    sc_src->Unlink();    
    sc_insert_before->Prepend_kid(sc_src);
    sc_src->Set_parent(sc_insert_before);

    ifinfo = dst_head->Ifinfo();
    ifinfo->Set_then(old_entry);
    
    Fix_parent_info(sc_src, sc_dst);
    break;

  default:
    FmtAssert(FALSE, ("Unexpected SC type"));
  }

  if (sc_prev)
    cfg->Fix_info(sc_prev);
  
  cfg->Invalidate_and_update_aux_info(FALSE);
  cfg->Invalidate_loops();

  _code_bloat_count += sc_src->Executable_stmt_count();
  Inc_transform_count();
}

// Do tail duplication of sc_src into sc_dst.
// Caller of this routine should take the responsiblity of legality check.

void
CFG_TRANS::Do_tail_duplication(SC_NODE * sc_src, SC_NODE * sc_dst)
{
  FmtAssert((sc_dst->Type() == SC_IF), ("Expect a SC_IF"));
  FmtAssert(sc_src->Is_sese(), ("Expect a single entry single exit"));

  // Other kinds of loops not tested yet.
  SC_TYPE type = sc_src->Type();
  CFG * cfg = _cu->Cfg();

  if (type == SC_LOOP)
    FmtAssert((sc_src->Loopinfo()->Is_flag_set(LOOP_PRE_DO)), ("TODO: test other loops"));
  else if (type == SC_IF) {
    FmtAssert(sc_dst->Next_sibling() == sc_src, ("Expect adjacent nodes."));
    cfg->Insert_block_after(sc_src);
    cfg->Insert_block_after(sc_dst);
  }

  if (_trace) {
    printf("\n\t\t Tail-duplication (SC%d,SC%d)\n", 
	   sc_src->Id(), sc_dst->Id());
  }
  
  BB_NODE * dst_head = sc_dst->Head();
  BB_NODE * dst_merge = sc_dst->Merge();
  BB_NODE * dst_else = sc_dst->Else();
  BB_NODE * dst_else_end = sc_dst->Else_end();
  BB_NODE * dst_then_end = sc_dst->Then_end();
  float scale = 0.0;
  FB_FREQ      then_edge_freq;
  FB_FREQ      else_edge_freq;
  FB_EDGE_TYPE exit_edge_type;
  FB_FREQ freq;
  IDTYPE edge;
  SC_NODE * sc_tmp;

  if (cfg->Feedback()) {
    FB_FREQ freq = cfg->Feedback()->Get_edge_freq(dst_head->Id(), dst_else->Id()) 
      / cfg->Feedback()->Get_node_freq_out(dst_head->Id()) * 1.0;
    if (freq.Known())
      scale = freq.Value();
    then_edge_freq = cfg->Feedback()->Get_edge_freq(dst_then_end->Id(), dst_merge->Id());
    else_edge_freq = cfg->Feedback()->Get_edge_freq(dst_else_end->Id(), dst_merge->Id());

  }

  SC_NODE * sc_merge = NULL;
  SC_NODE * sc_new = cfg->Clone_sc(sc_src, TRUE, scale, &sc_merge);
  FmtAssert(sc_new, ("NULL clone"));

  // Fix CFG
  
  BB_NODE * new_entry;
  BB_NODE * new_exit;
  BB_NODE * old_entry;
  BB_NODE * old_exit;

  MEM_POOL * pool = cfg->Mem_pool();
  BB_LOOP * loopinfo;
  BB_IFINFO * ifinfo;
  SC_NODE * sc_insert_after;
  BB_LIST_ITER bb_list_iter;
  BB_NODE * tmp;
  BB_NODE * tmp2;
  BB_NODE * src_merge;
  BB_NODE * new_merge;
  BB_LIST * bb_list;
  MAP_LIST * map_lst;
  SC_NODE * sc_blk;
  WN * branch_wn;
  BB_NODE *pred;
  BB_LIST_ITER bb_iter;

  switch (type) {
  case SC_LOOP:
    old_entry = sc_src->Head();
    src_merge = sc_src->Merge();
    old_exit = sc_src->Exit();
    new_entry = cfg->Get_cloned_bb(old_entry);
    new_exit = cfg->Get_cloned_bb(old_exit);

    FmtAssert(!src_merge->Pred()->Multiple_bbs(), ("Expect single predecessor"));

    new_merge = cfg->Create_and_allocate_bb(BB_GOTO);

    if (cfg->Feedback()) {
      cfg->Feedback()->Add_node(new_merge->Id());
      exit_edge_type = cfg->Feedback()->Get_edge_type(old_exit->Id(), src_merge->Id());
    }

    // Disconnect BB_NODEs in src_src from CFG
    FOR_ALL_ELEM(tmp, bb_list_iter, Init(old_entry->Pred())) {
      tmp->Replace_succ(old_entry, src_merge);
      if (cfg->Feedback() && (old_entry != dst_merge))
	cfg->Feedback()->Move_edge_dest(tmp->Id(), old_entry->Id(), src_merge->Id());

      if (tmp->Is_branch_to(old_entry)) 
	FmtAssert(FALSE, ("TODO: fix label"));
    }
    
    if (cfg->Feedback()) {
      if (old_entry == dst_merge)
	cfg->Feedback()->Move_edge_dest(dst_else_end->Id(), dst_merge->Id(), new_entry->Id());
      cfg->Feedback()->Move_edge_dest(old_exit->Id(), src_merge->Id(), new_merge->Id());
      cfg->Feedback()->Add_edge(new_merge->Id(), src_merge->Id(), FB_EDGE_OUTGOING, then_edge_freq);

      if (!cfg->Feedback()->Edge_has_freq(dst_then_end->Id(), old_entry->Id()))
	cfg->Feedback()->Add_edge(dst_then_end->Id(), old_entry->Id(), 
				  FB_EDGE_OUTGOING, then_edge_freq);
    }

    old_exit->Replace_succ(src_merge, new_merge);
    new_merge->Append_pred(old_exit, pool);
    src_merge->Remove_pred(old_exit, pool);
    src_merge->Set_pred(old_entry->Pred());
    old_entry->Set_pred(NULL);

    // Fix ifinfo
    ifinfo = dst_head->Ifinfo();
    ifinfo->Set_merge(src_merge);

    // Fix label on loop exit
    cfg->Add_label_with_wn(new_merge);
    branch_wn = old_exit->Branch_wn();
    FmtAssert(WN_label_number(branch_wn), ("NULL label"));
    WN_label_number(branch_wn) = new_merge->Labnam();

    // Fix loop info
    loopinfo = sc_src->Loopinfo();
    loopinfo->Set_merge(new_merge);

    tmp = src_merge->Prev();
    tmp->Set_next(new_merge);
    new_merge->Set_prev(tmp);
    tmp = old_entry->Prev();
    tmp->Set_next(src_merge);
    src_merge->Set_prev(tmp);
    old_entry->Set_prev(NULL);
    
    dst_merge = src_merge;

    Insert_region(old_entry, new_merge, dst_then_end, dst_merge, pool);
      
    // UNlink sc_src from SC tree and append it to SC_THEN's kids.
    sc_src->Unlink();
    sc_insert_after = sc_dst->Find_kid_of_type(SC_THEN);
    sc_insert_after->Append_kid(sc_src);
    sc_src->Set_parent(sc_insert_after);

    sc_blk = sc_src->Prev_sibling();
    if (sc_blk)
      cfg->Fix_info(sc_blk);
    
    sc_blk = cfg->Create_sc(SC_BLOCK);
    sc_blk->Append_bbs(new_merge);
    sc_insert_after->Append_kid(sc_blk);
    sc_blk->Set_parent(sc_insert_after);
    
    // Insert new BB_NODEs into else-path
    new_merge = cfg->Create_and_allocate_bb(BB_GOTO);
    
    if (cfg->Feedback()) {
      cfg->Feedback()->Add_node(new_merge->Id());
      cfg->Feedback()->Add_edge(new_exit->Id(), new_merge->Id(), exit_edge_type, else_edge_freq);
      cfg->Feedback()->Add_edge(new_merge->Id(), dst_merge->Id(), FB_EDGE_OUTGOING, else_edge_freq);
      if (!cfg->Feedback()->Edge_has_freq(dst_else_end->Id(), new_entry->Id()))
	cfg->Feedback()->Add_edge(dst_else_end->Id(), new_entry->Id(),
				  FB_EDGE_OUTGOING, else_edge_freq);
    }

    new_exit->Prepend_succ(new_merge, pool);
    new_merge->Append_pred(new_exit, pool);
    
    tmp = new_entry;
    while (tmp) {
      tmp2 = tmp;
      tmp = tmp->Next();
    }

    tmp2->Set_next(new_merge);
    new_merge->Set_prev(tmp2);

    Insert_region(new_entry, new_merge, dst_else_end, dst_merge, pool);

    // Append src_new to SC_ELSE's kids
    sc_insert_after = sc_dst->Find_kid_of_type(SC_ELSE);
    sc_insert_after->Append_kid(sc_new);
    sc_new->Set_parent(sc_insert_after);

    sc_blk = sc_new->Prev_sibling();

    if (sc_blk)
      cfg->Fix_info(sc_blk);

    sc_blk = cfg->Create_sc(SC_BLOCK);
    sc_blk->Append_bbs(new_merge);
    sc_insert_after->Append_kid(sc_blk);
    sc_blk->Set_parent(sc_insert_after);
    
    // Add merge to loopinfo
    loopinfo = sc_new->Loopinfo();
    loopinfo->Set_merge(new_merge);

    // Fix label on loop exit
    cfg->Add_label_with_wn(new_merge);    
    branch_wn = new_exit->Branch_wn();
    FmtAssert(WN_label_number(branch_wn), ("NULL label"));
    WN_label_number(branch_wn) = new_merge->Labnam();

    break;
  case SC_BLOCK:
    old_entry = sc_src->First_bb();
    old_exit = sc_src->Last_bb();
    new_entry = sc_new->First_bb();
    new_exit = sc_new->Last_bb();

    // Disconnect BB_NODEs in sc_src from CFG.
    FmtAssert(!old_exit->Succ()->Multiple_bbs(), ("Expect singe successor"));
    tmp2 = old_exit->Succ()->Node();

    FOR_ALL_ELEM(tmp, bb_list_iter, Init(old_entry->Pred())) {
      tmp->Replace_succ(old_entry, tmp2);
      if (cfg->Feedback() && (old_entry != dst_merge)) 
	cfg->Feedback()->Move_edge_dest(tmp->Id(), old_entry->Id(), tmp2->Id());
    }

    if (cfg->Feedback())
      cfg->Feedback()->Delete_edge(old_exit->Id(), tmp2->Id());

    FOR_ALL_ELEM (pred, bb_iter, Init(tmp2->Pred())) {
      pred->Set_succ(pred->Succ()->Remove(tmp2, pool));
    }

    bb_list = tmp2->Pred();
    while (bb_list) {
      bb_list = bb_list->Remove(bb_list->Node(), pool);
    }
    tmp2->Set_pred(old_entry->Pred());
    old_entry->Set_pred(NULL);

    bb_list = old_exit->Succ();
    while (bb_list) {
      bb_list = bb_list->Remove(bb_list->Node(), pool);
    }

    old_exit->Set_succ(NULL);
    
    tmp = old_entry->Prev();
    tmp->Set_next(tmp2);
    tmp2->Set_prev(tmp);
    old_entry->Set_prev(NULL);
    old_exit->Set_next(NULL);

    if (cfg->Feedback()) {
      cfg->Feedback()->Move_edge_dest(dst_else_end->Id(), dst_merge->Id(), new_entry->Id());
      cfg->Feedback()->Move_edge_dest(dst_then_end->Id(), dst_merge->Id(), old_entry->Id());
    }
    
    if (dst_merge == old_entry) {
      ifinfo = dst_head->Ifinfo();
      ifinfo->Set_merge(tmp2);
      dst_merge = tmp2;
    }

    if (cfg->Feedback()) {
      cfg->Feedback()->Add_edge(new_exit->Id(), dst_merge->Id(),
				FB_EDGE_OUTGOING,
				cfg->Feedback()->Get_edge_freq(dst_else_end->Id(), new_entry->Id()));
      cfg->Feedback()->Add_edge(old_exit->Id(), dst_merge->Id(),
				FB_EDGE_OUTGOING,
				cfg->Feedback()->Get_edge_freq(dst_then_end->Id(), old_entry->Id()));
    }
    
    // Insert BB_NODEs into else-path
    Insert_region(new_entry, new_exit, dst_else_end, dst_merge, pool);
    
    // Append sc_new to SC_ELSE's kids
    sc_insert_after = sc_dst->Find_kid_of_type(SC_ELSE);
    sc_blk = sc_insert_after->Last_kid();

    if (sc_blk->Type() == SC_BLOCK) {
      bb_list = sc_new->Get_bbs();
      FOR_ALL_ELEM(tmp, bb_list_iter, Init(bb_list)) {
	sc_blk->Append_bbs(tmp);
      }
    }
    else {
      sc_insert_after->Append_kid(sc_new);
      sc_new->Set_parent(sc_insert_after);

      if (sc_blk)
	cfg->Fix_info(sc_blk);
    }

    // insert it into then-path.
    Insert_region(old_entry, old_exit, dst_then_end, dst_merge,pool);

    // Unlink src_src from SC tree and append it to SC_THEN's kids.
    sc_src->Unlink();
    sc_insert_after = sc_dst->Find_kid_of_type(SC_THEN);
    sc_blk = sc_insert_after->Last_kid();

    if (sc_blk->Type() == SC_BLOCK) {
      bb_list = sc_src->Get_bbs();
      FOR_ALL_ELEM(tmp, bb_list_iter, Init(bb_list)) {
	sc_blk->Append_bbs(tmp);
      }
    }
    else {
      sc_insert_after->Append_kid(sc_src);
      sc_src->Set_parent(sc_insert_after);

      if (sc_blk)
	cfg->Fix_info(sc_blk);
    }
    break;

  case SC_IF:
    old_entry = sc_src->Head();
    old_exit = sc_src->Merge();
    new_entry = sc_new->Head();
    new_exit = sc_new->Merge();

    if (dst_else_end->Is_branch_to(dst_merge)) {
      FmtAssert(FALSE, ("TODO: fix label"));
    }

    Insert_region(new_entry, new_exit, dst_else_end, dst_merge, pool);    

    if (cfg->Feedback()) {
      FB_EDGE_TYPE edge_type = cfg->Feedback()->Get_edge_type(dst_else_end->Id(), dst_merge->Id());
      cfg->Feedback()->Move_edge_dest(dst_else_end->Id(), dst_merge->Id(), new_entry->Id());
      cfg->Feedback()->Add_edge(new_exit->Id(), dst_merge->Id(), edge_type,
				cfg->Feedback()->Get_edge_freq(dst_else_end->Id(), new_entry->Id()));
    }

    // Append sc_new and src_merge to SC_ELSE's kids.
    sc_insert_after = sc_dst->Find_kid_of_type(SC_ELSE);
    sc_blk = sc_insert_after->Last_kid();

    sc_insert_after->Append_kid(sc_new);
    sc_new->Set_parent(sc_insert_after);
    sc_insert_after->Append_kid(sc_merge);
    sc_merge->Set_parent(sc_insert_after);

    if (sc_blk)
      cfg->Fix_info(sc_blk);

    // Disconnect BB_NODEs in src from CFG and then insert it into the then-path.
    sc_merge = sc_src->Next_sibling()->Next_sibling();
    tmp2 = sc_merge->First_bb();
    FOR_ALL_ELEM(tmp, bb_list_iter, Init(old_entry->Pred())) {
      tmp->Replace_succ(old_entry, tmp2);
      if (cfg->Feedback())
	cfg->Feedback()->Move_edge_dest(tmp->Id(), old_entry->Id(),tmp2->Id());
    }

    tmp2->Set_pred(old_entry->Pred());
    old_exit->Remove_succ(tmp2, pool);
    old_entry->Set_pred(NULL);
    old_exit->Set_next(NULL);
    tmp = old_entry->Prev();
    tmp->Set_next(tmp2);
    tmp2->Set_prev(tmp);
    old_entry->Set_prev(NULL);
    old_exit->Set_next(NULL);

    if (cfg->Feedback()) {
      FB_EDGE_TYPE edge_type = cfg->Feedback()->Get_edge_type(dst_then_end->Id(), dst_merge->Id());
      cfg->Feedback()->Move_edge_dest(dst_then_end->Id(), dst_merge->Id(), old_entry->Id());
      cfg->Feedback()->Move_edge_dest(old_exit->Id(), tmp2->Id(), dst_merge->Id());
      edge = cfg->Feedback()->Get_edge(old_exit->Id(), dst_merge->Id());
      freq = cfg->Feedback()->Get_edge_freq(dst_then_end->Id(), old_entry->Id());
      cfg->Feedback()->Change_edge_freq(edge, freq);
    }

    if (dst_then_end->Is_branch_to(dst_merge)) {
      FmtAssert(FALSE, ("Fix branch target."));
    }

    Insert_region(old_entry, old_exit, dst_then_end, dst_merge, pool);

    sc_insert_after = sc_dst->Find_kid_of_type(SC_THEN);
    sc_blk = sc_insert_after->Last_kid();
    sc_tmp = sc_src->Next_sibling();
    sc_src->Unlink();
    sc_insert_after->Append_kid(sc_src);
    sc_src->Set_parent(sc_insert_after);
    sc_tmp->Unlink();
    sc_insert_after->Append_kid(sc_tmp);
    sc_tmp->Set_parent(sc_insert_after);

    if (sc_blk)
      cfg->Fix_info(sc_blk);

    cfg->Fix_info(sc_insert_after);
    
    break;

  default:
    FmtAssert(FALSE, ("Unexpected SC type"));
  }
  
  cfg->Invalidate_and_update_aux_info(FALSE);
  cfg->Invalidate_loops();
  _code_bloat_count += sc_src->Executable_stmt_count();
  Inc_transform_count();
}

// Traverse siblings between sc1 and sc2, do code motion or head/tail duplication
// to bring sc1 and sc2 adjacent to each other. Return TRUE if all transformations
// during the traversal are successful.
//
// This routine can only be called from PRO_LOOP_FUSION_TRANS::Top_down_trans.
BOOL
PRO_LOOP_FUSION_TRANS::Traverse_trans(SC_NODE * sc1, SC_NODE * sc2)
{
  FmtAssert((sc1->Parent() == sc2->Parent()), ("Expect siblings"));
  SC_NODE * sc = sc1;
  BOOL ret_val = TRUE;

  if (_trace)
    printf("\n\t Traverse (SC%d,SC%d)\n", 
	   sc1->Id(), sc2->Id());

  while (sc != sc2) {
    SC_NODE * next = sc->Next_sibling();
    SC_TYPE sc_type = sc->Type();
    SC_TYPE next_type = next->Type();

    FmtAssert(((sc_type == SC_IF) || (sc_type == SC_LOOP)),
	      ("Unexpect SC type"));
    
    switch (next_type) {
    case SC_BLOCK:
      if (sc_type == SC_LOOP) {
	if (sc->Loopinfo()->Is_flag_set(LOOP_PRE_DO)) 
	  Do_code_motion(sc, next);
	else {
	  ret_val = FALSE;
	  if (_trace)
	    printf("\n\t\t  Skip non-DO-LOOP (SC%d)\n", sc->Id());
	}
      }
      else if (sc_type == SC_IF) 
	Do_tail_duplication(next, sc);
      else 
	FmtAssert(FALSE, ("Unexpect SC type"));

      break;
    case SC_IF:
      if (sc_type == SC_IF) {
	if (IF_MERGE_TRANS::Is_candidate(sc, next, TRUE))
	  Do_merge(sc, next, TRUE);
	else {
	  // FmtAssert(FALSE, ("TODO"));
	  ret_val = FALSE;
	}
      }
      else if (sc_type == SC_LOOP) {
	if (sc->Loopinfo()->Is_flag_set(LOOP_PRE_DO)) {
	  if (sc->Is_ctrl_equiv(next)
	      && !Has_dependency(sc, next) 
              && Can_be_speculative(next)) {
	    Do_code_motion(sc, next);
	  }
	  else {
	    // Do head duplication.
	    Do_head_duplication(sc, next);
	    sc = next;
	  }
	}
	else {
	  ret_val = FALSE;
	  if (_trace)
	    printf("\n\t\t  Skip non-DO-LOOP (SC%d)\n", sc->Id());
	}
      }

      break;
    case SC_LOOP:
      if (sc_type == SC_IF) {
	if (next->Loopinfo()->Is_flag_set(LOOP_PRE_DO)) {
	  Do_tail_duplication(next, sc);
	  // exhause transformation opportunities on current loop classifications.
	  Nonrecursive_trans(sc->Find_kid_of_type(SC_THEN), FALSE);
	  Nonrecursive_trans(sc->Find_kid_of_type(SC_ELSE), FALSE);
	}
	else {
	  ret_val = FALSE;
	  if (_trace)
	    printf("\n\t\t  Skip non-DO-LOOP (SC%d)\n", next->Id());
	}
      }
      else if (sc_type == SC_LOOP) {
	if (sc->Loopinfo()->Is_flag_set(LOOP_PRE_DO)
	    && next->Loopinfo()->Is_flag_set(LOOP_PRE_DO)) {
	  if  (next != sc2) {
	    Do_code_motion(sc, next);
	  }
	  else if (Do_ext_fusion() && Can_fuse(sc)) {
	    sc = Do_loop_fusion(sc, 1);
	    _loop_list = _loop_list->Remove(sc2, _pool);
	  }
	}
      }
      break;
    default:
      FmtAssert(FALSE, ("Unexpect SC type"));
    }

    if ((next == sc2) || (ret_val == FALSE))
      break;
  }

  return ret_val;
}

// Query whether the traverse transformation between the given pair
// should be delayed.
BOOL
PRO_LOOP_FUSION_TRANS::Is_delayed(SC_NODE * sc1, SC_NODE * sc2)
{
  BOOL ret_val = FALSE;

  // Case 1. Both sc1 and sc2 are SC_LOOPs, and all sibling nodes between them
  // are SC_BLOCKs.  In this scenario, we should search further for same-scenario
  // candidates and do traverse transformation on those candidates before sc1
  // and sc2 are processed.  Imposing such a delay is to reduce state transitions
  // of traverse transformation.

  if ((sc1->Type() == SC_LOOP) && (sc2->Type() == SC_LOOP)
      && (sc1->Parent() == sc2->Parent())) {
    ret_val = TRUE;
    SC_NODE * next_sibling = sc1->Next_sibling();

    while (next_sibling && (next_sibling != sc2)) {
      if (next_sibling->Type() != SC_BLOCK) {
	ret_val = FALSE;
	break;
      }
      next_sibling = next_sibling->Next_sibling();
    }
  }

  return ret_val;
}

// Do non-recursive tail-duplication transformation for candidates whose lcp is sc_root.
void
PRO_LOOP_FUSION_TRANS::Nonrecursive_trans(SC_NODE * sc_root, BOOL do_find) 
{
  if (do_find) {
    _loop_list = NULL;
    Collect_classified_loops(sc_root);
  }

  while (1) {
    if ((WOPT_Enable_Pro_Loop_Limit >= 0)
	&& (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
      break;
      
    SC_NODE * cand1 = NULL;
    SC_NODE * cand2 = NULL;
    Find_cand(sc_root, &cand1, &cand2, NULL);

    if (cand1 && cand2) {
      SC_NODE * tmp1 = cand1;
      SC_NODE * tmp2 = cand2;
      SC_NODE * last1 = NULL;
      SC_NODE * last2 = NULL;

      while (tmp1 && tmp2 && Is_delayed(tmp1, tmp2)) {
	last1 = tmp1;
	last2 = tmp2;
	Find_cand(sc_root, &tmp1, &tmp2, tmp2);
      }
      
      if (last1 && last2) {
	cand1 = last1;
	cand2 = last2;
      }
    }

    if (cand1 && cand2) {
      if (!Traverse_trans(cand1, cand2)) 
	break;
    }
    else
      break;
  }

  if (do_find) {
    while (_loop_list) {
      SC_NODE * tmp = _loop_list->Node();
      _loop_list = _loop_list->Remove(tmp, _pool);
    }
  }
}

// Top down do proactive loop fusion transformation for the SC tree rooted at the given sc_root.
void
PRO_LOOP_FUSION_TRANS::Top_down_trans(SC_NODE * sc_root)
{
  if (sc_root->Has_flag(HAS_SYMM_LOOP)) {
    int orig_transform_count = _transform_count;    

    if (!Do_ext_trans() || Do_ext_traverse()) 
      Nonrecursive_trans(sc_root, TRUE);
    
    if (Do_ext_fusion()) {
      SC_NODE * sc_loop = sc_root->First_kid_of_type(SC_LOOP);
      // Attempt loop fusion to expose if-merging opportunities.
      while (sc_loop) {
	if (Can_fuse(sc_loop)) 
	  sc_loop = Do_loop_fusion(sc_loop, 0);
	if (sc_loop)
	  sc_loop = sc_loop->Next_sibling_of_type(SC_LOOP);
      }
    }

    if (_transform_count > orig_transform_count) {
      IF_MERGE_TRANS::Top_down_trans(sc_root);
      Classify_loops(sc_root);
    }
  }

  SC_LIST_ITER sc_list_iter;
  SC_NODE * kid;
  FOR_ALL_ELEM(kid, sc_list_iter, Init(sc_root->Kids())) {
    Top_down_trans(kid);
  }
}

// Reset/clear fields
void
PRO_LOOP_FUSION_TRANS::Clear()
{
  IF_MERGE_TRANS::Clear();
  _last_class_id = 0;
  _loop_list = NULL;
  _edit_loop_class = FALSE;
}

// Reset related loop classification fields for the SC tree rooted at sc.
// Rebuild map of SC tree depth to a list of SC_LOOP nodes.
// The routine can only be invoked by PRO_LOOP_FUSION_TRANS::Classify_loops.

void
PRO_LOOP_FUSION_TRANS::Reset_loop_class(SC_NODE * sc, int cur_depth)
{
  FmtAssert(_edit_loop_class, ("Not in edit mode"));

  sc->Set_class_id(0);
  sc->Set_depth(cur_depth);
  sc->Remove_flag(HAS_SYMM_LOOP);

  if (sc->Type() == SC_LOOP) {
    SC_LIST * sc_list = _loop_depth_to_loop_map[cur_depth];

    if (!sc_list) {
      sc_list = (SC_LIST *) CXX_NEW(SC_LIST(sc), _pool);
      _loop_depth_to_loop_map[cur_depth] = sc_list;
    }
  
    sc_list = sc_list->Append(sc, _pool);
  }

  SC_LIST_ITER sc_list_iter(sc->Kids());
  SC_NODE *tmp = NULL;

  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    Reset_loop_class(tmp, cur_depth+1);
  }
}

// Mark SC_LOOPs with symmetric paths the same class id.
// The routine can only be invoked by PRO_LOOP_FUSION_TRANS::Classify_loops.

void
PRO_LOOP_FUSION_TRANS::Find_loop_class(SC_NODE * sc)
{
  FmtAssert(_edit_loop_class, ("Not in edit mode"));

  if ((sc->Type() == SC_LOOP) && (sc->Class_id() == 0)) {
    SC_LIST * sc_list = _loop_depth_to_loop_map[sc->Depth()];
    SC_LIST_ITER sc_list_iter(sc_list);
    SC_NODE *tmp = NULL;
    int new_id = New_class_id();
    sc->Set_class_id(new_id);

    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      if ((tmp->Class_id() != 0) ||  (tmp == sc))
	continue;

      if (sc->Has_symmetric_path(tmp, FALSE)) {
	tmp->Set_class_id(sc->Class_id());
	SC_NODE * lcp = sc->Find_lcp(tmp);
	lcp->Add_flag(HAS_SYMM_LOOP);
      }
    }
  }
  
  SC_LIST_ITER sc_list_iter(sc->Kids());
  SC_NODE * tmp;
  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    Find_loop_class(tmp);
  }
}

// Classify loops for the SC tree rooted at sc.
void
PRO_LOOP_FUSION_TRANS::Classify_loops(SC_NODE *sc)
{
  _edit_loop_class = TRUE;
  OPT_POOL_Push(_pool, MEM_DUMP_FLAG + 1);
  _loop_depth_to_loop_map.clear();
  Reset_loop_class(sc, 0);
  Find_loop_class(sc);
  OPT_POOL_Pop(_pool, MEM_DUMP_FLAG + 1);
  _edit_loop_class = FALSE;
}

// Reset/clear fields
void
PRO_LOOP_INTERCHANGE_TRANS::Clear()
{
  IF_MERGE_TRANS::Clear();
  _pool = NULL;
  _outer_stack = NULL;
  _inner_stack = NULL;
  _local_stack = NULL;
  _restart_stack = NULL;
  _action = DO_NONE;
}

// Query whether given SC_LOOP has a perfect loop nest, i.e.,
// - has only one child loop.
// - The child loop is the first kid.
// - All siblings of the child loop are empty blocks.
BOOL
PRO_LOOP_INTERCHANGE_TRANS::Is_perfect_loop_nest(SC_NODE * sc)
{
  if (sc->Type() == SC_LOOP) {
    SC_NODE * sc_tmp = sc->Find_kid_of_type(SC_LP_BODY);
    SC_NODE * inner_loop = sc_tmp->First_kid();

    if (inner_loop->Type() != SC_LOOP)
      return FALSE;

    sc_tmp = inner_loop->Next_sibling();
    while (sc_tmp) {
      if (!sc_tmp->Is_empty_block())
	return FALSE;
      sc_tmp = sc_tmp->Next_sibling();
    }

    return TRUE;
  }

  return FALSE;
}

// Given sc_inner of type SC_LOOP, where the sc_inner is the first loop among
// its siblings, find its buddy SC_LOOP from loops given in sc_stack that
// satisfies:
// - The LCP of sc_inner and the buddy is a SC_IF.
// - sc_inner and its buddy are on symmetric pathes. (see SC_NODE::Has_symmetric_path)
//
// Return the buddy if exists.
//
// Proactive loop fusion can be performed in lock-steps for sc_inner and its buddy.
// See PRO_LOOP_INTERCHANGE_TRANS::Do_lock_step_fusion.
SC_NODE *
CFG_TRANS::Find_fusion_buddy(SC_NODE * sc_inner, STACK<SC_NODE *> * sc_stack)
{
  if (sc_inner->Parent() == NULL)
    return NULL;

  if (sc_inner->Prev_sibling() && (sc_inner->Prev_sibling()->Type() == SC_LOOP))
    return NULL;

  if (sc_stack) {
    for (int i = 0; i < sc_stack->Elements(); i++) {
      SC_NODE * sc_cur = sc_stack->Top_nth(i);

      if (sc_cur->Prev_sibling() && (sc_cur->Prev_sibling()->Type() == SC_LOOP))
	continue;
      
      if (sc_cur != sc_inner) {
	SC_NODE * lcp = sc_inner->Find_lcp(sc_cur);
	if (lcp && (lcp->Type() == SC_IF) 
	    && sc_inner->Has_symmetric_path(sc_cur, TRUE)) {
	  return sc_cur;
	}
      }
    }
  }
  return NULL;
}

// Driver for synchronized lock-step loop fusion for a pair of buddy loops.
// See Find_fusion_buddy for definition of buddy loops.
void
PRO_LOOP_INTERCHANGE_TRANS::Do_lock_step_fusion(SC_NODE * sc1, SC_NODE * sc2)
{
  SC_NODE * sc_lcp = sc1->Find_lcp(sc2);

  if (!Is_invariant(sc1, sc_lcp->Head(), 0)
      || !Is_invariant(sc2, sc_lcp->Head(), 0))
    return;

  if (_trace)
    printf("\n\t Lock-step fusion (SC%d,SC%d)\n", sc1->Id(), sc2->Id());

  Set_region_id(sc_lcp->Id());
  
  _action = DO_NONE;
  _action |= DO_TREE_HEIGHT_RED;

  // Do canonicalization and tree height reduction in lock steps.
  Do_canon(sc_lcp, sc1, HEAD_DUP | TAIL_DUP);
  Nonrecursive_trans(sc_lcp, sc1);
  Do_canon(sc_lcp, sc2, HEAD_DUP | TAIL_DUP);
  Nonrecursive_trans(sc_lcp, sc2);
  
  SC_NODE * sc_tmp1 = sc1->Get_nesting_if(sc_lcp);
  SC_NODE * sc_tmp2 = sc2->Get_nesting_if(sc_lcp);

  // Do if-condition distribution and if-merging in lock steps.
  while (sc_tmp1 && sc_tmp2) {
    if (!Do_if_cond_dist(sc_lcp, TRUE))
      break;

    if (sc_tmp1->Next_sibling_of_type(SC_IF) == sc_tmp2) {
      Do_merge(sc_tmp1, sc_tmp2, TRUE);
      IF_MERGE_TRANS::Top_down_trans(sc_tmp1);
    }
    else if (sc_tmp2->Next_sibling_of_type(SC_IF) == sc_tmp1) {
      Do_merge(sc_tmp2, sc_tmp1, TRUE);
      IF_MERGE_TRANS::Top_down_trans(sc_tmp2);
    }
    else
      break;

    sc_lcp = sc1->Find_lcp(sc2);
    Do_canon(sc_lcp, sc1, HEAD_DUP | TAIL_DUP);
    Do_canon(sc_lcp, sc2, HEAD_DUP | TAIL_DUP);
    sc_tmp1 = sc1->Get_nesting_if(sc_lcp);
    sc_tmp2 = sc2->Get_nesting_if(sc_lcp);
    
    // Do reversed loop unswitching in lock steps.
    if (!sc_tmp1 && !sc_tmp2) {
      if (!Do_reverse_loop_unswitching(sc_lcp, sc2, NULL))
	break;

      if (!Do_reverse_loop_unswitching(sc_lcp, sc1, NULL))
	break;
    }
  }

  Set_region_id(0);
}

// Check whether the given loop nest is interchangable assuming it is a perfect loop nest.
// (See PRO_LOOP_INTERCHANGE_TRANS::Is_perfect_loop_nest)
// 
// Here we only do it for the simplest loop-nest that satisfies:
// 1. Has unique memory reference (see CFG_TRANS::Get_unique_ref).
// 2. The memory reference iterarates only on the inner loop's dimension.
// 3. Loop index for both the outer loop and the inner loop are a AUTO or a REG.
// 4. LP_START, LP_COND and LP_STEP only reference loop indexes or loop invariants w.r.t.
//   the outer loop.
//
// (1) and (2) guarantee that the loop-nest has a zero distance vector and therefore
// is fully permutable.
// TODO: code sharing with LNO for legality and profitability check.
BOOL
PRO_LOOP_INTERCHANGE_TRANS::Can_interchange(SC_NODE * outer_loop, SC_NODE * inner_loop)
{
  if (!outer_loop->Loopinfo()->Is_flag_set(LOOP_PRE_DO)
      || !inner_loop->Loopinfo()->Is_flag_set(LOOP_PRE_DO))
    return FALSE;

  WN * wn_tmp = NULL;
  WN * wn_load = Get_index_load(inner_loop);
  AUX_ID inner_aux_id = wn_load ? WN_aux(wn_load) : 0;

  // Check whether inner loop has unique memory reference.
  SC_NODE * sc_tmp = inner_loop->Find_kid_of_type(SC_LP_BODY);
  Get_unique_ref(sc_tmp, inner_loop, &wn_tmp);

  // Check whether the unique memory reference iterates only on the inner loop's dimension,
  // also check the inner loop and the outer loop's index, start, condition and step expressions
  // for loop interchange legality.
  if ((wn_tmp != NULL)
      && inner_aux_id
      && Is_invariant(outer_loop, wn_tmp, inner_aux_id)
      && Check_index(inner_loop)
      && Check_index(outer_loop)
      && Check_iteration(inner_loop, SC_LP_START, outer_loop) 
      && Check_iteration(inner_loop, SC_LP_COND, outer_loop) 
      && Check_iteration(inner_loop, SC_LP_STEP, outer_loop) 
      && Check_iteration(outer_loop, SC_LP_START, outer_loop)
      && Check_iteration(outer_loop, SC_LP_COND, outer_loop)
      && Check_iteration(outer_loop, SC_LP_STEP, outer_loop))
    return TRUE;

  return FALSE;
}

// Initialize scratch fields of PRO_LOOP_INTERCHANGE_TRANS sub-object.
void
PRO_LOOP_INTERCHANGE_TRANS::Init()
{
  _local_stack = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
  _outer_stack = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
  _inner_stack = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
  _restart_stack = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
  _unlink_sc = CXX_NEW(STACK<SC_NODE *>(_pool), _pool); 
}

// Delete scratch fields of PRO_LOOP_INTERCHANGE_TRANS sub-object.
void
PRO_LOOP_INTERCHANGE_TRANS::Delete()
{
  CXX_DELETE(_local_stack, _pool);
  _local_stack = NULL;
  CXX_DELETE(_outer_stack, _pool);
  _outer_stack = NULL;
  CXX_DELETE(_inner_stack, _pool);
  _inner_stack = NULL;
    
  while (!_restart_stack->Is_Empty())
    _restart_stack->Pop();
  CXX_DELETE(_restart_stack, _pool);
  _restart_stack = NULL;

  Delete_unlink_sc();
}

// Analyze the if-conditions in-between the outer_loop and a stack of inner loops,
// check whether the inner loops are loop fusion candidates if they were adjacent to 
// each other, but not all pairs of if-conditions are identical.
// 
// for (...) {                  // outer loop
//    if (i) {                  
//      if (a) {
//        if (b) {
//           for (...)         // inner loop 1
//           ...
//        }
//      }
//    }
//
//    if (j) {                 // different from "if(i)"
//      if (a) {               
//        if (b) {
//           for (...)         // inner loop 2
//           ...
//        }
//      }
//    }
//
// Encode the occurrence where the difference exists and return the encoded word.
std::pair<BOOL,UINT32>
PRO_LOOP_INTERCHANGE_TRANS::Can_do_misc_trans(STACK<SC_NODE *> * fusion_stack, SC_NODE * outer_loop)
{
  SC_NODE * sc_body = outer_loop->Find_kid_of_type(SC_LP_BODY);
  SC_NODE * sc_loop = fusion_stack->Top_nth(0);
  BOOL is_candidate = TRUE;
  UINT32 path_code = 0;
  CFG * cfg = _cu->Cfg();

  if (!sc_loop->Get_nesting_if(sc_body))
    return std::pair<BOOL, UINT32> (FALSE, 0);
  
  // Analyze nesting if-conditions to identify candidates.
  for (int i = 1; i < fusion_stack->Elements(); i++) {
    SC_NODE * sc_cur = fusion_stack->Top_nth(i);
    SC_NODE * lcp = sc_loop->Find_lcp(sc_cur);
    if ((lcp != sc_body) || !sc_loop->Has_same_loop_struct(sc_cur)) {
      is_candidate = FALSE;
      break;
    }
    else {
      // Currently limit it to cases that all but one if-coditions between
      // 'sc_body' and 'sc_cur' are identical.
      UINT32 cur_code = sc_loop->Encode_path(sc_cur);
      if (!path_code) {
	path_code = cur_code;
	int count = 0;
	while (cur_code) {
	  if ((cur_code & 1) != 0)
	    count++;
	  cur_code >>= 1;
	}
	if (count != 1) {
	  is_candidate = FALSE;
	  break;
	}
      }
      else if (cur_code != path_code) {
	is_candidate = FALSE;
	break;
      }
    }
  }

  return std::pair<BOOL, UINT32> (is_candidate, path_code);
}

// Process non-identical if-conditions between the outer loop and the inner loops.
// 1. Swap order with identical if-conditions so that the identical if-conditions are pushed
//    toward the outer loop and the non-identical if-conditions are pushed toward the inner loop.
// 2. If there exists only one if-condition between the outer loop and the inner loop,and the 
//    if-condition is a loop invariant w.r.t. the inner loop, sink the inner loop out of the
//    if-condition by pushing the if-condition into the inner loop.
// Refer to "PRO_LOOP_INTERCHANGE_TRANS::Can_do_misc_trans".
BOOL
PRO_LOOP_INTERCHANGE_TRANS::Process_non_identical_nodes
(STACK<SC_NODE *> * fusion_stack, SC_NODE * outer_loop, UINT32 path_code)
{
  for (int i = 0; i < fusion_stack->Elements(); i++) {
    SC_NODE * sc_cur = fusion_stack->Top_nth(i);
    SC_NODE * inner_loop = sc_cur;
    UINT32 cur_code = path_code;
    int level = 1;
    SC_NODE * sc_if = inner_loop->Get_nesting_if(1);

    if (sc_if->Get_real_parent() == outer_loop) {
      // Transformation 2.
      SC_NODE * sc_iter = inner_loop->Next_sibling();
      while (sc_iter) {
	if (Has_dependency(sc_iter, inner_loop)) {
	  return FALSE;
	}
	sc_iter = sc_iter->Next_sibling();
      }

      if (!Is_invariant(inner_loop, sc_if->Get_cond(), 0)
	  || !Do_sink_node(sc_if, inner_loop, TRUE)) {
	return FALSE;
      }
      else {
	if (_trace) {
	  printf("\n\t\t Func: %s(%d) sink loop (SC%d) out of if-condition (SC:%d)\n", 
		 Current_PU_Name(), Current_PU_Count(), inner_loop->Id(), sc_if->Id());
	}
      }

      continue;
    }
      
    while (cur_code) {
      sc_cur = sc_cur->Find_parent_of_type(SC_IF);
      if ((cur_code & 1) != 0) {
	SC_NODE * sc_then = sc_cur->Find_kid_of_type(SC_THEN);
	SC_NODE * sc_else = sc_cur->Find_kid_of_type(SC_ELSE);
	WN * wn_cond = sc_cur->Get_cond();
	if ((sc_then->Is_empty() || sc_else->Is_empty())
	    && Can_reorder_cond(wn_cond, NULL)) {
	  // Transformation 1.
	  while (level > 1) {
	    sc_cur = inner_loop->Get_nesting_if(level);
	    if (!Do_canon(sc_cur, inner_loop, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP | CHK_LEGAL)
		|| !Do_if_cond_dist(sc_cur, TRUE)) {
	      return FALSE;
	    }
	    level--;
	  }
	}
	else
	  return FALSE;
	break;
      }
      cur_code >>= 1;
      level++;
    }
  }
  return TRUE;
}

// Process identical if-conditions between the outer loop and the inner loop.
BOOL
PRO_LOOP_INTERCHANGE_TRANS::Process_identical_nodes(SC_NODE * outer_loop, SC_NODE * inner_loop)
{
  SC_NODE * sc_if = inner_loop->Get_nesting_if(1);

  if (Do_canon(outer_loop, sc_if, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP | CHK_LEGAL)) {
    SC_NODE * outer_if = sc_if->Get_nesting_if(outer_loop);
    if (outer_if) {
      SC_NODE * sc_step = outer_loop->Find_kid_of_type(SC_LP_STEP);
      SC_NODE * sc_tmp = sc_if->Get_nesting_if(outer_if);
      SC_NODE * sc_cmp = sc_tmp ? sc_tmp->Parent() : sc_if->Parent();
      BOOL is_partial_invar = TRUE;
      BOOL is_invar = TRUE;
	  
      // Check whether every identical if-condition is invariant or partial invariant.
      SC_NODE * sc_iter = sc_if->Parent();
      while (sc_iter && (sc_iter != outer_loop)) {
	if (sc_iter->Type() == SC_IF) {
	  WN * wn_cond = sc_iter->Get_cond();
	  if (Has_dependency(sc_step, wn_cond)) {
	    is_invar = FALSE;
	    is_partial_invar = FALSE;
	    break;
	  }
	  else {
	    if (is_invar && !Is_invariant(outer_loop, wn_cond, 0))
	      is_invar = FALSE;
	    
	    if (!is_invar && !Is_invariant(sc_cmp, wn_cond, 0)) {
	      is_partial_invar = FALSE;
	      break;
	    }
	  }
	}
	sc_iter = sc_iter->Parent();
      }
      
      if (is_invar || is_partial_invar) {
	// Do if-condition tree-height reduction and/or loop unswitching.
	if (!sc_tmp || Do_if_cond_tree_height_reduction(outer_if, sc_if->Parent())) {
	  sc_if = inner_loop->Get_nesting_if(outer_loop);
	  if (Do_loop_unswitching(sc_if, outer_loop, !is_invar))
	    return TRUE;
	}
      }
    }
  }
  return FALSE;
}

// Attempt code motions to bring inner loops adjacent to each other and then fuse them.
std::pair<SC_NODE *, BOOL>
PRO_LOOP_INTERCHANGE_TRANS::Do_misc_fusion(STACK<SC_NODE *> * fusion_stack, SC_NODE * outer_loop)
{
  BOOL is_candidate = TRUE;
  SC_NODE * outer_body = outer_loop->Find_kid_of_type(SC_LP_BODY);
  SC_NODE * sc_first = NULL;
  SC_NODE * sc_cur = outer_body->Find_kid_of_type(SC_LOOP);

  while (sc_cur) {
    if (fusion_stack->Contains(sc_cur)) {
      FmtAssert((sc_cur->Parent() == outer_body), ("Unexpected candidate."));
      if (!sc_first) 
	sc_first = sc_cur;
      else if (sc_first->Is_ctrl_equiv(sc_cur)) {
	// Do code motion to bring 'sc_first' and 'sc_cur' adjacent.
	while (sc_first->Next_sibling() != sc_cur) {
	  SC_NODE * sc_tmp = sc_first->Next_sibling();
	  if (Has_dependency(sc_tmp, sc_cur)) {
	    is_candidate = FALSE;
	    break;
	  }
	  else 
	    Do_code_motion(sc_first, sc_tmp);
	}
	if (sc_first->Next_sibling() == sc_cur) {
	  // Do loop fusion.
	  if (Can_fuse(sc_first))
	    sc_cur = Do_loop_fusion(sc_first, 0);
	  else {
	    is_candidate = FALSE;
	    break;
	  }
	}
      }
    }
    sc_cur = sc_cur->Next_sibling_of_type(SC_LOOP);
  }
  return std::pair<SC_NODE *, BOOL>(sc_first, is_candidate);
}

// Find killing defs in 'sc_first's previous siblings.
BOOL PRO_LOOP_INTERCHANGE_TRANS::Collect_killing_defs
(STACK<SC_NODE *> * stk_def, SC_NODE * outer_loop, SC_NODE * inner_loop)
{
  BOOL is_candidate = TRUE;
  SC_NODE * sc_first = inner_loop;
  SC_NODE * sc_tmp = sc_first->Parent()->First_kid();

  while (sc_tmp && (sc_tmp != sc_first)) {
    if (Has_dependency(sc_tmp, sc_first)) {
      if (Is_kill(sc_tmp, stk_def, outer_loop)
	  && Is_invariant(inner_loop, sc_tmp, 0)) {
	stk_def->Push(sc_tmp);
      }
      else {
	is_candidate = FALSE;
	break;
      }
    }
    sc_tmp = sc_tmp->Next_sibling();
  }

  return is_candidate;
}

// Query whether all codes inside 'sc_loop' are in SC_IFs, and
// all SC_IFs are disjoint. Currently limit it to the cases like:
// "if(x == const) {...}".
BOOL
PRO_LOOP_INTERCHANGE_TRANS::Is_disjoint(STACK<SC_NODE *> * stk_def, SC_NODE * sc_loop)
{
  SC_NODE * sc_first = sc_loop;
  BOOL is_disjoint = TRUE;
  CFG * cfg = _cu->Cfg();
  
  // Check whether all codes inside 'sc_first' are in SC_IFs, and
  // all SC_IFs are disjoint. Currently limit it to the cases like:
  // "if(x == const) {...}".
  SC_NODE * sc_body = sc_first->First_kid_of_type(SC_LP_BODY);
  SC_LIST_ITER kids_iter;
  std::set<INT64> cond_val;
  WN * wn_load = NULL;
  SC_NODE * sc_tmp;

  // Collect constants used in condition expressions.
  FOR_ALL_ELEM(sc_tmp, kids_iter, Init(sc_body->Kids())) {
    if (sc_tmp->Type() != SC_IF) {
      if (!sc_tmp->Is_empty()) {
	is_disjoint = FALSE;
	break;
      }
      continue;
    }
    if (!sc_tmp->Find_kid_of_type(SC_ELSE)->Is_empty()) {
      is_disjoint = FALSE;
      break;
    }
	      
    WN * wn_cond = sc_tmp->Get_cond();
    if (!wn_cond || (WN_operator(wn_cond) != OPR_EQ)) {
      is_disjoint = FALSE;
      break;
    }
    
    WN * kid0 = WN_kid0(wn_cond);
    WN * kid1 = WN_kid1(wn_cond);
    if ((WN_operator(kid1) != OPR_INTCONST) 
	|| !OPERATOR_is_scalar_load(WN_operator(kid0))) {
      is_disjoint = FALSE;
      break;
    }
    int val = WN_const_val(kid1);
    std::set<INT64>::iterator it = cond_val.find(val);
    if (!wn_load)
      wn_load = kid0;
    else if ((WN_aux(kid0) != WN_aux(wn_load))
	     || (it != cond_val.end())) {
      is_disjoint = FALSE;
      break;
    }

    cond_val.insert(val);
  }

  if (is_disjoint) {
    std::set<INT64> def_val;
    // Collect constant values defined by killing defs.
    for (int i = stk_def->Elements() - 1; i >= 0; i--) {
      SC_NODE * sc_iter = stk_def->Top_nth(i);
      if (!Get_def_vals(sc_iter, wn_load, def_val)) {
	is_disjoint = FALSE;
	break;
      }
    }
    // Check whether values defined by killing defs are
    // exactly the same as those used in conditional expressions.
    if (def_val != cond_val)
      is_disjoint = FALSE;
  }
  return is_disjoint;
}

// Attempt to hoist up sc_loop's next siblings.
BOOL
PRO_LOOP_INTERCHANGE_TRANS::Do_hoist_next_siblings(SC_NODE * sc_loop)
{
  SC_NODE * sc_tmp = sc_loop->Next_sibling();
  BOOL is_candidate = TRUE;
  CFG * cfg = _cu->Cfg();

  while (sc_tmp) {
    SC_NODE * sc_next = sc_tmp->Next_sibling();
    if (!sc_next) {
      if (sc_tmp->Is_empty_block())
	break;
      else {
	cfg->Insert_block_after(sc_tmp);
	sc_next = sc_tmp->Next_sibling();
      }
    }
    
    if (Has_dependency(sc_tmp, sc_loop)) {
      is_candidate = FALSE;
      break;
    }
    else {
      Do_code_motion(sc_loop, sc_tmp);
    }
    sc_tmp = sc_next;
  }
  return is_candidate;
}

// Duplicate defs in 'stk_def' and insert them before 'sc_loop'.  Return the first and 
// the last inserted nodes.
std::pair<SC_NODE *, SC_NODE *>
PRO_LOOP_INTERCHANGE_TRANS::Do_insert_defs(SC_NODE * sc_loop, STACK<SC_NODE *> * stk_def)
{
  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return std::pair<SC_NODE *, SC_NODE *> (NULL, NULL);

  CFG * cfg = _cu->Cfg();

  // Create an insertion point.
  SC_NODE * sc_ins = sc_loop->Prev_sibling();
  if (sc_ins->Type() != SC_BLOCK)
    sc_ins = cfg->Insert_block_after(sc_ins);

  BB_NODE * bb_begin = sc_ins->Last_bb();
  MEM_POOL * pool = cfg->Mem_pool();
  SC_NODE * sc_begin = NULL;
  SC_NODE * sc_end = NULL;

  // Duplicate nodes containing killing defs.
  for (int i = 0; i < stk_def->Elements(); i++) {
    SC_NODE * sc_iter = stk_def->Top_nth(i);
    SC_NODE * sc_merge = NULL;
    SC_NODE * sc_new = cfg->Clone_sc(sc_iter, TRUE, 1.0, &sc_merge);
    BB_NODE * bb_entry = sc_new->Head();
    BB_NODE * bb_exit = sc_new->Merge();
    BB_NODE * bb_end = bb_begin->Next();
    bb_begin->Replace_succ(bb_end, bb_entry);
    bb_end->Replace_pred(bb_begin, bb_exit);
    bb_entry->Append_pred(bb_begin, pool);
    bb_exit->Append_succ(bb_end, pool);
    bb_begin->Set_next(bb_entry);
    bb_entry->Set_prev(bb_begin);
    bb_exit->Set_next(bb_end);
    bb_end->Set_prev(bb_exit);
    sc_ins->Insert_after(sc_new);
    sc_new->Insert_after(sc_merge);
    if (!sc_end)
      sc_end = sc_new;
    sc_begin = sc_new;
  }
  Inc_transform_count();
  return std::pair<SC_NODE *, SC_NODE *> (sc_begin, sc_end);
}

// Push nodes in [sc_begin, sc_end] into 'inner_loop'.
void
PRO_LOOP_INTERCHANGE_TRANS::Do_push_nodes
(SC_NODE * sc_begin, SC_NODE * sc_end, SC_NODE * outer_loop,  SC_NODE * inner_loop)
{
  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return;

  SC_NODE * sc_iter = sc_end;
  SC_NODE * sc_body = inner_loop->Find_kid_of_type(SC_LP_BODY);
  BB_NODE * bb_loop = inner_loop->First_bb();

  if (_trace) {
    printf("\n\t\tFunc: %s(%d) push killing defs into loop (SC:%d)\n", 
	   Current_PU_Name(), Current_PU_Count(), inner_loop->Id());
  }
	
  // Push invariant killing defs into 'inner_loop' to enable loop interchange.	      
  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();
  while (sc_iter) {
    SC_NODE * sc_prev = sc_iter->Prev_sibling();
    if (sc_iter->Type() == SC_IF) {
      BB_NODE * bb_first = sc_iter->Head();
      BB_NODE * bb_last = sc_iter->Merge();
      SC_NODE * sc_merge = sc_iter->Next_sibling();
      SC_NODE * sc_ins = sc_body->First_kid();
      BB_NODE * bb_ins;
      BB_NODE * bb_tmp;
      BB_LIST_ITER bb_list_iter;
      
      FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_first->Pred())) {
	bb_tmp->Replace_succ(bb_first, bb_loop);
	if (bb_tmp->Is_branch_to(bb_first)) {
	  WN * branch_wn = bb_tmp->Branch_wn();
	  cfg->Add_label_with_wn(bb_loop);
	  WN_label_number(branch_wn) = bb_loop->Labnam();
	}
	if (cfg->Feedback())
	  cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_first->Id(), bb_loop->Id());
      }
      bb_loop->Set_pred(bb_first->Pred());
      bb_tmp = bb_first->Prev();
      bb_tmp->Set_next(bb_loop);
      bb_loop->Set_prev(bb_tmp);
      bb_ins = sc_body->First_bb();
      
      FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_ins->Pred())) {
	bb_tmp->Replace_succ(bb_ins, bb_first);
	if (bb_tmp->Is_branch_to(bb_ins)) {
	  WN * branch_wn = bb_tmp->Branch_wn();
	  cfg->Add_label_with_wn(bb_first);
	  WN_label_number(branch_wn) = bb_first->Labnam();
	}
	if (cfg->Feedback())
	  cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_ins->Id(), bb_first->Id());
      }
      bb_first->Set_pred(bb_ins->Pred());
      bb_ins->Set_pred(NULL);
      bb_ins->Append_pred(bb_last, pool);
      bb_last->Replace_succ(bb_loop, bb_ins);
      bb_tmp = bb_ins->Prev();
      bb_tmp->Set_next(bb_first);
      bb_first->Set_prev(bb_tmp);
      bb_ins->Set_prev(bb_last);
      bb_last->Set_next(bb_ins);
      sc_iter->Unlink();
      sc_merge->Unlink();
      sc_ins->Insert_before(sc_merge);
      sc_merge->Insert_before(sc_iter);
      cfg->Fix_info(sc_prev);
    }
    if (sc_iter == sc_begin)
      break;
    sc_iter = sc_prev;
  }
  
  cfg->Fix_info(outer_loop);
  cfg->Fix_info(inner_loop);
  cfg->Invalidate_and_update_aux_info(FALSE);    
  cfg->Invalidate_loops();
  Inc_transform_count();
}

// Swap 'sc_if' with if-conditions are nested insider it.  The transformation is limited to 
// perfectly-nested SC_IFs with either an empty then-path or an empty else-path, e.g.,
//
// From:
// if (a) {
//    if (b) {
//      if (c) {
//       ...
//      }
//    }
// }
//
// To:
// if (b) {
//    if (c) {
//      if (a) {
//        ...
//       }
//    }
// }
//
// Caller of this routine should ensure the legality.
void
PRO_LOOP_INTERCHANGE_TRANS::Do_swap_if(SC_NODE * sc_if)
{
  SC_NODE * sc_then = sc_if->First_kid();
  SC_NODE * sc_else = sc_if->Last_kid();

  if (sc_then->Is_empty() || sc_else->Is_empty()) {
    SC_NODE * sc_child = sc_else->Is_empty() ? sc_then->Find_kid_of_type(SC_IF) : 
      sc_else->Find_kid_of_type(SC_IF);

    while (sc_child) {
      SC_NODE * sc_next_child = NULL;
      SC_NODE * sc_p = sc_child->Get_real_parent();
      FmtAssert((sc_p && (sc_p->Type() == SC_IF)), ("Expect a SC_IF"));
	
      if (Do_canon(sc_child, NULL, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP | CHK_LEGAL)) {
	SC_NODE * sc_then = sc_child->First_kid();
	SC_NODE * sc_else = sc_child->Last_kid();

	for (int i = 0; i < 2; i ++) {
	  SC_NODE * sc_cur = (i == 0) ? sc_then : sc_else;
	  SC_NODE * sc_tmp = sc_cur->First_kid();
	  if ((sc_tmp == sc_cur->Last_kid())
	      && (sc_tmp->Type() == SC_BLOCK)) {
	    // Prune then-path or else-path if it contains a single block.
	    Prune_block(sc_tmp);
	  }
	  else {
	    // Attempt to negate if-conditions.
	    Do_negate(sc_tmp);
	  }
	}
	// If either the then-path or the else-path is empty, do if-condition
	// distribution and recursivly traverse down.
	if ((sc_then->Is_empty() 
	     || sc_else->Is_empty())) {  
	  sc_next_child = sc_else->Is_empty()? sc_then->Find_kid_of_type(SC_IF) :
	    sc_else->Find_kid_of_type(SC_IF);
	  Do_if_cond_dist(sc_p, FALSE);	  
	}

      }
      sc_child = sc_next_child;
    }
  }
}

// Do loop unswitching for nested if-regions whose conditional expressions are loop invariants
// in a top down order, e.g., the outermost if-condition is hoisted first.
// 
// From:
// for (..) {
//   if (invar1) {        
//       if (invar2) {
//         ...
//       }
//   }
// }
// 
// To:
//  if (invar1) {
//      if (invar2) {
//         for (...)
//      }
//  }
//
void
PRO_LOOP_INTERCHANGE_TRANS::Top_down_do_loop_unswitch(SC_NODE * sc_loop)
{
  if ((sc_loop->Type() != SC_LOOP)
      || !sc_loop->Is_sese() 
      || !sc_loop->Loopinfo()->Is_flag_set(LOOP_PRE_DO))
    return;

  SC_NODE * sc_body = sc_loop->Find_kid_of_type(SC_LP_BODY);
  SC_NODE * sc_if = NULL;
  SC_NODE * sc_first = sc_body->First_kid_of_type(SC_IF);
  SC_NODE * sc_tmp = sc_first;

  while (sc_tmp) {
    if (Is_invariant(sc_loop, sc_tmp->Head(), 0)) {
      sc_if = sc_tmp;
      break;
    }
    sc_tmp = sc_tmp->Next_sibling_of_type(SC_IF);
  }
  
  if (sc_if && sc_if->Parent()->All_kids_clonable(NULL)) {
    if (Do_canon(sc_if, NULL, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP | CHK_LEGAL)) {
      sc_tmp = Do_loop_unswitching(sc_if, sc_loop, FALSE);
      if (sc_tmp) {
	sc_tmp = sc_tmp->Find_kid_of_type(SC_THEN);
	sc_tmp = sc_tmp->Find_kid_of_type(SC_LOOP);
	if (sc_tmp)
	  Top_down_do_loop_unswitch(sc_tmp);
      }
    }
  }
  else if (sc_first && sc_first->Parent()->All_kids_clonable(NULL)) {
    SC_NODE * sc_then = sc_first->First_kid();
    SC_NODE * sc_else = sc_first->Last_kid();
    SC_NODE * sc1;
    SC_NODE * sc2;
    BOOL doit = FALSE;

    if (!Is_invariant(sc_loop, sc_first->Head(), 0)) {
      if (!sc_then->Is_empty() && !sc_else->Is_empty()) {
	// Handle cases like:
	// if (a) {            // "a" is loop variant.
	//   if (c) { ... }    // "c" is loop invariant.
	// }
	// else {
	//   if (c) { ... }    
	// }
	SC_NODE * sc1 = sc_then->First_kid_of_type(SC_IF);
	SC_NODE * sc2 = sc_else->First_kid_of_type(SC_IF);
	if (sc1 && sc2 && sc1->Parent()->All_kids_clonable(NULL)
	    && sc2->Parent()->All_kids_clonable(NULL)) {
	  WN * wn_tmp = sc1->Head()->Laststmt();
	  if (sc1->Head()->Compare_Trees(sc2->Head())
	      && Is_invariant(sc_loop, sc1->Head(), 0)
	      && Can_be_speculative(wn_tmp)
	      && !WN_has_indir_load(wn_tmp)) {
	    if (Do_canon(sc_first, NULL, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP | CHK_LEGAL)
		&& Do_canon(sc1, NULL, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP | CHK_LEGAL)
		&& Do_canon(sc2, NULL, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP | CHK_LEGAL)) {
	      if (Do_if_cond_dist(sc_first, FALSE)) {
		sc1 = sc_body->First_kid_of_type(SC_IF);
		sc2 = sc1->Next_sibling();
		FmtAssert(sc2->Type() == SC_IF, ("Expect a SC_IF"));
		sc_tmp = Do_merge(sc1, sc2, FALSE);	    
		sc1 = sc_tmp->First_kid_of_type(SC_THEN);
		sc1 = sc1->First_kid_of_type(SC_IF);
		sc2 = sc1->Next_sibling_of_type(SC_IF);
		if (sc2->First_kid()->Is_empty()
		    && sc2->Last_kid()->Is_empty()) 
		  Remove_node(sc2);
		if (sc_tmp->Parent()->All_kids_clonable(NULL)
		    && Do_canon(sc_tmp, NULL, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP | CHK_LEGAL)) {
		  doit = TRUE;
		  sc_first = sc_tmp;
		}
	      }
	    }
	  }
	}
      }
      else if (sc_else->Is_empty()
	       && (sc_first == sc_body->First_kid())) {
	// Handle cases like:
	// if (a) {            // "a" is loop variant.
	//    if (c) { ... }   // "c" is loop invariant.
	// }
	SC_NODE * sc_tmp = sc_first->Next_sibling();
	BOOL is_empty = TRUE;
	while (sc_tmp) {
	  if (!sc_tmp->Is_empty()) {
	    is_empty = FALSE;
	    break;
	  }
	  sc_tmp = sc_tmp->Next_sibling();
	}
	
	if (is_empty) {
	  sc1 = sc_then->Find_kid_of_type(SC_IF);
	  while (sc1) {
	    if (Is_invariant(sc_loop, sc1->Head(), 0))
	      break;
	    sc1 = sc1->Next_sibling_of_type(SC_IF);
	  }
	  if (sc1) {
	    WN * wn_tmp = sc1->Head()->Laststmt();
	    if (Can_be_speculative(wn_tmp)
		&& !WN_has_indir_load(wn_tmp)
		&& sc1->Parent()->All_kids_clonable(NULL)
		&& Do_canon(sc1, NULL, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP | CHK_LEGAL)
		&& Do_if_cond_dist(sc_first, FALSE)) {
	      sc1 = sc_body->First_kid_of_type(SC_IF);
	      if (sc1->Parent()->All_kids_clonable(NULL)
		  && Do_canon(sc1, NULL, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP | CHK_LEGAL)) {
		doit = TRUE;
		sc_first = sc1;
	      }
	    }
	  }
	}
      }
    }

    if (doit) {
      sc_tmp = Do_loop_unswitching(sc_first, sc_loop, FALSE);
      if (sc_tmp) {
	SC_NODE * sc_cur = sc_tmp->Find_kid_of_type(SC_THEN);
	sc_loop = sc_cur->Find_kid_of_type(SC_LOOP);
	Top_down_do_loop_unswitch(sc_loop);

	if (_trace) {
	  printf("\n\t Top-down do loop unswitch %s(%d) (Loop:%d)\n",
		 Current_PU_Name(), Current_PU_Count(), sc_loop->Id());
	}
      }
    }
  }
}

// Same as "PRO_LOOP_INTERCHANGE_TRANS::Collect_precomp_if_cond(SC_NODE *, SC_NODE *)"
BOOL
PRO_LOOP_INTERCHANGE_TRANS::Collect_precomp_if_cond(BB_NODE * bb)
{
  WN * tmp;
  for (tmp = bb->Firststmt(); tmp != NULL; tmp = WN_next(tmp)) {
    if (!WN_is_executable(tmp))
      continue;

    OPERATOR opr = WN_operator(tmp);
    if ((opr != OPR_FALSEBR) && (opr != OPR_TRUEBR)) {
      WN * wn_bit = WN_get_bit_reduction(tmp);
      if (!wn_bit || (WN_operator(wn_bit) != OPR_BXOR))
	return FALSE;

      WN * wn_kid1 = WN_kid(wn_bit, 1);
      if ((WN_operator(wn_kid1) != OPR_SHL)
	  || !WN_is_power_of_2(wn_kid1))
	return FALSE;

      WN * wn_kid0 = WN_kid0(wn_kid1);
      if ((WN_operator(wn_kid0) != OPR_INTCONST)
	  || (WN_const_val(wn_kid0) != 1))
	return FALSE;
    }
  }

  return TRUE;
}

// Walk SC tree rooted at 'sc', check whether its statements satisfy one of the following conditions.
// 1. The statement is a bit operation reduction and the operator is XOR.
// 2. The statement is a if-condition.
// If (2) is satisfied, collect its SC_NODEs in 'stk'.
BOOL
PRO_LOOP_INTERCHANGE_TRANS::Collect_precomp_if_cond(SC_NODE * sc, STACK<SC_NODE *> * stk)
{
  BB_NODE * bb_tmp = sc->Get_bb_rep();
  if (bb_tmp) {
    if (!Collect_precomp_if_cond(bb_tmp)
	|| (sc->Type() != SC_IF))
      return FALSE;

    stk->Push(sc);
  }

  BB_LIST * bb_list = sc->Get_bbs();
  if (bb_list) {
    BB_LIST_ITER bb_list_iter(bb_list);
    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init()) {
      if (!Collect_precomp_if_cond(bb_tmp))
	return FALSE;
    }
  }
  
  SC_LIST * kids = sc->Kids();

  if (kids != NULL) {
    SC_NODE * sc_tmp;
    SC_LIST_ITER sc_list_iter(kids);
    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init()) {
      if (!Collect_precomp_if_cond(sc_tmp, stk))
	return FALSE;
    }
  }

  return TRUE;
}

// Walk statements in 'bb', search for shift expressions in the format of "1 << expr",
// replace "1" with a load of 'aux_id'
void 
PRO_LOOP_INTERCHANGE_TRANS::Rewrite_shift_count(BB_NODE * bb, AUX_ID aux_id)
{
  WN * tmp;
  for (tmp = bb->Firststmt(); tmp != NULL; tmp = WN_next(tmp)) {
    if (!WN_is_executable(tmp))
      continue;
    OPERATOR opr = WN_operator(tmp);
    if ((opr != OPR_FALSEBR) && (opr != OPR_TRUEBR)) {
      WN * wn_bit = WN_get_bit_reduction(tmp);
      if (wn_bit) {
	WN * wn_kid1 = WN_kid(wn_bit, 1);
	if (WN_operator(wn_kid1) == OPR_SHL) {
	  WN * wn_kid0 = WN_kid0(wn_kid1);
	  FmtAssert(((WN_operator(wn_kid0) == OPR_INTCONST)
		     && (WN_const_val(wn_kid0) == 1)),
		    ("Unexpect statement"));
	  OPT_STAB * opt_stab = _cu->Opt_stab();
	  ST * st_preg = opt_stab->Aux_stab_entry(aux_id)->St();
	  TY_IDX ty_idx = ST_type(st_preg);
	  TYPE_ID type_id = TY_mtype(ty_idx);
	  WN * wn_load = WN_CreateLdid(OPR_LDID, type_id, type_id,
				       opt_stab->St_ofst(aux_id),
				       ST_st_idx(st_preg),
				       ty_idx, 0);
	  WN_kid0(wn_kid1) = wn_load;
	  Create_alias(_cu->Alias_mgr(), wn_load);
	  WN_set_aux(wn_load, aux_id);

	  if (_trace) {
	    printf("\n\t Rewrite precomputed if-condition %s(%d) (BB:%d)\n",
		   Current_PU_Name(), Current_PU_Count(), bb->Id());
	  }
	}
      }
    }
  }
}

// Walk SC tree rooted at 'sc', search for shift counts in the format of "1 << expr",
// replace "1" with a load of 'st'.
void
PRO_LOOP_INTERCHANGE_TRANS::Rewrite_shift_count(SC_NODE * sc, AUX_ID aux_id)
{
  BB_NODE * bb_tmp = sc->Get_bb_rep();
  if (bb_tmp) 
    Rewrite_shift_count(bb_tmp, aux_id);
  
  BB_LIST * bb_list = sc->Get_bbs();
  if (bb_list) {
    BB_LIST_ITER bb_list_iter(bb_list);
    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init()) {
      Rewrite_shift_count(bb_tmp, aux_id);
    }
  }
  
  if (sc->Kids()) {
    SC_LIST_ITER sc_list_iter(sc->Kids());
    SC_NODE * sc_tmp;
    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init()) {
      Rewrite_shift_count(sc_tmp, aux_id);
    }
  }
}

// Top down visit SC tree rooted at 'sc', look for loops with killing defs and
// if-conditions that can be precomputed.  Hoist killing defs and precomputable
// if-conditions.
void
PRO_LOOP_INTERCHANGE_TRANS::Top_down_do_precomp(SC_NODE * outer_loop, SC_NODE * sc)
{
  if (sc->Type() == SC_LOOP) {
    std::pair<INT, INT> p_val = Estimate_bounds(outer_loop, sc);
    INT upper_bound = p_val.second;
    INT lower_bound = p_val.first;
    if ((lower_bound != 0) || (upper_bound <= 0))
      return;

    // Make sure the inner loop is nested properly inside the outer_loop.
    SC_NODE * sc_tmp = sc->Get_real_parent();
    while (sc_tmp && (sc_tmp != outer_loop)) {
      if ((sc_tmp->Type() != SC_IF)
	  || (sc_tmp != sc_tmp->Parent()->First_kid()))
	return;
      sc_tmp = sc_tmp->Get_real_parent();
    }

    CFG * cfg = _cu->Cfg();
    SC_NODE * sc_body = sc->Find_kid_of_type(SC_LP_BODY);
    SC_LIST * sc_list = sc_body->Kids();
    SC_LIST_ITER sc_list_iter;
    STACK<SC_NODE *> * stk_def = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
    STACK<SC_NODE *> * stk_if = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
    STACK<SC_NODE *> * stk_precomp = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);

    BOOL do_hoist = TRUE;	
    // Find killing defs and precomputable if-conditions.
    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(sc_list)) {
      if (Is_kill(sc_tmp, stk_def, sc)) {
	stk_def->Push(sc_tmp);
      }
      else if (!Collect_precomp_if_cond(sc_tmp, stk_if)) {
	do_hoist = FALSE;
	break;
      }
    }
    
    if (do_hoist && (stk_def->Elements() > 0) && (stk_if->Elements() > 0)) {
      SC_NODE * sc_dist = NULL;
      for (int i= 0; i < stk_if->Elements(); i++) {
	SC_NODE * sc_if = stk_if->Bottom_nth(i);
	// Prune away if-conditions containing indirect loads.
	WN * wn_cond = sc_if->Get_cond();
	if (!wn_cond || WN_has_indir_load(wn_cond))
	  continue;

	if (!sc_if->Find_kid_of_type(SC_ELSE)->Is_empty())
	  continue;
	
	// Prune away elements in 'stk_if' that do not have dependency on 'stk_def',
	BOOL has_dep = FALSE;
	for (int j = 0; j < stk_def->Elements(); j++) {
	  SC_NODE * sc_def = stk_def->Bottom_nth(j);
	  if (Has_dependency(sc_def, sc_if->Head())) {
	    has_dep = TRUE;
	    break;
	  }
	}
	if (!has_dep)
	  continue;

	// Find sc_cur's ancestor that is a sibling of killing defs.
	SC_NODE * sc_cur = sc_if;
	while (sc_cur) {
	  if (sc_cur->Parent() == sc_body) {
	    break;
	  }
	  sc_cur = sc_cur->Parent();
	}
	// Find a partition point where killing defs and empty blocks are grouped together.
	SC_NODE * sc_par = NULL;
	int count = 0;

	sc_tmp = sc_cur->Prev_sibling();
	while (sc_tmp) {
	  if (stk_def->Contains(sc_tmp)) {
	    if (!sc_par)
	      sc_par = sc_tmp->Next_sibling();
	    count++;
	  }
	  else if (sc_par && !sc_tmp->Is_empty_block()) {
	    count = 0;
	    break;
	  }
	  sc_tmp = sc_tmp->Prev_sibling();
	}
	
	if (count == stk_def->Elements()) {
	  if (!sc_dist || (sc_par == sc_dist)) {
	    stk_precomp->Push(sc_if);
	    sc_dist = sc_par;
	  }
	}
      }
      
      int size = stk_precomp->Elements();
      if (size > 0) {
	// Do loop distribution at the partition point.
	cfg->Insert_block_before(sc_dist);
	sc_dist = Do_loop_dist(sc, FALSE, sc_dist);
	if (sc_dist) {
	  SC_NODE * sc_pre = sc_dist->Prev_sibling();
	  FmtAssert((sc_pre && sc_pre->Type() == SC_LOOP), ("Unexpected empty loop."));
	  // Change loop upper bound.
	  WN * wn_index = Get_index_load(sc_pre);
	  WN * wn_new = WN_CreateExp2(OPR_LE, MTYPE_I4, MTYPE_I4, 
				      WN_COPY_Tree_With_Map(wn_index),
				      WN_CreateIntconst(OPR_INTCONST, MTYPE_I4, MTYPE_V, 
							upper_bound));
	  SC_NODE * sc_end = sc_pre->Find_kid_of_type(SC_LP_COND);
	  BB_NODE * bb_end = sc_end->Last_bb();
	  WN * wn_end = bb_end->Branch_wn();
	  FmtAssert(wn_end, ("Expect a branch wn"));
	  WN_kid0(wn_end) = wn_new;
	  SC_NODE * sc_body = sc_pre->Find_kid_of_type(SC_LP_BODY);
	  SC_NODE * sc_last = sc_body->Last_kid();
	  if (!sc_last->Is_empty_block())
	    sc_last = cfg->Insert_block_after(sc_last);
	  
	  // Create a temporay array.
	  int trip_cnt = upper_bound - lower_bound + 1;
	  ST * array_st = Tmp_array_st(MTYPE_I4, trip_cnt);

	  for (int i = 0; i < size; i++) {
	    SC_NODE * sc_if = stk_precomp->Bottom_nth(i);
	    SC_NODE * sc_copy = Do_if_cond_wrap(sc_if->Head(), sc_last, TRUE);
	    FmtAssert(sc_copy, ("Expect a SC_IF"));
	    // Precompute if-conditions.	    
	    for (int j = 0; j < 2; j++) {
	      SC_NODE * sc_cur = (j == 0) ? sc_copy->Find_kid_of_type(SC_THEN) 
		: sc_copy->Find_kid_of_type(SC_ELSE);
	      sc_last = sc_cur->Last_kid();
	      bb_end = sc_last->Last_bb();
	      WN * wn_val = WN_Intconst(MTYPE_I4, (j == 0) ? 1 : 0);
	      WN * wn_st = Create_array_store(array_st, WN_COPY_Tree_With_Map(wn_index), wn_val);
	      bb_end->Prepend_stmt(wn_st);
	    }
	    // Reload precomputed values.
	    WN * wn_stid = Create_array_load(array_st, WN_COPY_Tree_With_Map(wn_index));
	    if (wn_stid) {
	      SC_NODE * sc_tmp = cfg->Insert_block_before(sc_if);
	      BB_NODE * bb_tmp = sc_tmp->Last_bb();
	      bb_tmp->Prepend_stmt(wn_stid);
	      sc_tmp = sc_if->Find_kid_of_type(SC_THEN);
	      // Rewrite shift counts.
	      AUX_ID aux_id = WN_aux(wn_stid);
	      Rewrite_shift_count(sc_tmp, aux_id);
	      // Remove if-conditions.
	      Do_if_cond_unwrap(sc_if);
	    }
	  }
	  
	  // Hoist up loop containing killing defs and the precomputed if-condition values.
	  SC_NODE * sc_next = sc_pre->Next_sibling();
	  sc_next = cfg->Insert_block_before(sc_next);
	  BB_NODE * bb_head = sc_pre->Head();
	  BB_NODE * bb_merge = sc_pre->Merge();
	  FmtAssert(bb_merge->Is_empty(), ("Expect an empty block"));

	  if (bb_merge->Succ()->Len() == 1) {
	    // Unlink nodes in [bb_head, bb_merge] and move it outside of the outer loop.
	    SC_NODE * sc_ins = outer_loop->Prev_sibling();
	    if (!sc_ins->Is_empty_block()) 
	      sc_ins = cfg->Insert_block_before(outer_loop);
	    BB_NODE * dst_end = outer_loop->First_bb();
	    BB_NODE * dst_begin = sc_ins->Last_bb();
	    BB_NODE * bb_next = bb_merge->Nth_succ(0);
	    MEM_POOL * pool = cfg->Mem_pool();

	    BB_NODE * bb_tmp;
	    BB_LIST_ITER bb_list_iter;
	    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_head->Pred())) {
	      if (bb_tmp->Is_branch_to(bb_head)) {
		WN * branch_wn = bb_tmp->Branch_wn();
		cfg->Add_label_with_wn(bb_next);
		WN_label_number(branch_wn) = bb_next->Labnam();
	      }
	      
	      bb_tmp->Replace_succ(bb_head, bb_next);
	      if (cfg->Feedback())
		cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_head->Id(), bb_next->Id());
	    }
	    
	    bb_next->Set_pred(bb_head->Pred());
	    bb_merge->Set_succ(NULL);
	    bb_head->Set_pred(NULL);
	    bb_tmp = bb_head->Prev();
	    bb_tmp->Set_next(bb_next);
	    bb_next->Set_prev(bb_tmp);
	    bb_head->Set_prev(NULL);
	    bb_merge->Set_next(NULL);
	    Insert_region(bb_head, bb_merge, dst_begin, dst_end, pool);

	    if (cfg->Feedback()) {
	      cfg->Feedback()->Move_edge_dest(dst_begin->Id(), dst_end->Id(), bb_head->Id());
	      cfg->Feedback()->Move_edge_dest(bb_merge->Id(), bb_next->Id(), dst_end->Id());
	    }
	    SC_NODE * sc_p = sc_pre->Get_real_parent();
	    sc_pre->Unlink();
	    sc_next->Unlink();
	    outer_loop->Insert_before(sc_pre);
	    outer_loop->Insert_before(sc_next);
	    cfg->Fix_info(sc_pre);
	    cfg->Fix_info(sc_p);

	    if (_trace) {
	      printf("\n\t Hoist precomputed if-condition %s(%d) (SC:%d)\n",
		     Current_PU_Name(), Current_PU_Count(), outer_loop->Id());
	    }
	  }
	}
      }
    }
    
    CXX_DELETE(stk_if, _pool);	
    CXX_DELETE(stk_def, _pool);
    CXX_DELETE(stk_precomp, _pool);
    return;
  }

  if (sc->Kids()) {
    SC_NODE * sc_tmp = sc->First_kid();
    while (sc_tmp) {
      SC_NODE * sc_next = sc_tmp->Next_sibling();
      Top_down_do_precomp(outer_loop, sc_tmp);
      sc_tmp = sc_next;
    }
  }
}

// Estimate lower and upper bound of 'inner_loop' which is nested in 'outer_loop'.
// Return a negative value if such an estimation is infeasible.
std::pair<INT, INT>
PRO_LOOP_INTERCHANGE_TRANS::Estimate_bounds(SC_NODE * outer_loop, SC_NODE * inner_loop)
{
  WN * wn_upper = Get_upper_bound(inner_loop);
  if (!wn_upper || !Is_invariant(outer_loop, wn_upper, 0))
    return std::pair<INT, INT> (-1, -1);

  SC_NODE * sc_if = inner_loop->Get_nesting_if(outer_loop);
  SC_NODE * sc_path = outer_loop->Find_kid_of_type(SC_LP_BODY);

  if (sc_if) {
    // Match pattern: "if ((x & ( 1 << y)) != 0) { ... }" and infer upper bound of "y".
    SC_NODE * sc_then = sc_if->Find_kid_of_type(SC_THEN);
    if (sc_then->Is_pred_in_tree(inner_loop)) {
      WN * wn_cond = sc_if->Get_cond();
      if (wn_cond) {
	WN * wn_kid0 = WN_kid0(wn_cond);
	WN * wn_kid1 = WN_kid1(wn_cond);
	if ((WN_operator(wn_cond) == OPR_NE)
	    && (WN_operator(wn_kid1) == OPR_INTCONST)
	    && (WN_const_val(wn_kid1) == 0)
	    && (WN_operator(wn_kid0) == OPR_BAND)) {
	  wn_kid1 = WN_kid1(wn_kid0);
	  if ((WN_operator(wn_kid1) == OPR_SHL)) {
	    if (WN_is_power_of_2(wn_kid1)) {
	      TYPE_ID mtype = WN_rtype(wn_kid1);
	      wn_kid1 = WN_get_bit_from_expr(wn_kid1);
	      if (wn_kid1) {
		std::pair<bool,int> p_val = WN_get_val(wn_kid1, Get_high_map());
		if (MTYPE_is_unsigned(mtype)) {
		  // shift count should be less than the bit size 
		  // represented by the data type.		  
		  if (!p_val.first) {
		    INT upper_bound = MTYPE_bit_size(mtype);
		    Set_hi(Get_high_map(), wn_kid1, upper_bound - 1);
		    Infer_val_range(wn_kid1, TRUE, FALSE);
		  }
		  // Obtain inner loop's upper bound value.
		  std::pair<bool,int> p_val = Clone_val(wn_upper, wn_kid1, Get_high_map());
		  if (p_val.first) {
		    int high = p_val.second;
		    SC_NODE * sc_init = inner_loop->Find_kid_of_type(SC_LP_START);
		    BB_NODE * bb_init = sc_init ? sc_init->First_bb() : NULL;
		    WN * wn_init = bb_init ? bb_init->Laststmt() : NULL;
		    WN * wn_index = Get_index_load(inner_loop);

		    // Obtain inner loop's lower bound value.
		    if (wn_init && OPERATOR_is_scalar_store(WN_operator(wn_init))
			&& wn_index && (WN_aux(wn_init) == WN_aux(wn_index))) {
		      Infer_val_range(wn_init, FALSE, TRUE);
		      p_val = WN_get_val(wn_init, Get_low_map());
		      if (p_val.first) {
			int low = p_val.second;
			WN_Delete(wn_upper);
			return std::pair<INT, INT> (low, high);
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

  WN_Delete(wn_upper);
  return std::pair<INT,INT> (-1, -1);
}

// Do miscellaneous transformations to enable loop fusion, if-merging and loop interchange for cases 
// as shown in "PRO_LOOP_INTERCHANGE_TRANS::Can_do_misc_trans".
// 'fusion_stack' gives inner loops, 'outer_loop' gives the outer loop.
// Return TRUE if re-iteration is needed.  Otherwise return FALSE.
BOOL
PRO_LOOP_INTERCHANGE_TRANS::Do_misc_trans(STACK<SC_NODE *> * fusion_stack, SC_NODE * outer_loop)
{
  CFG * cfg = _cu->Cfg();
  std::pair<BOOL,UINT32> p_val = Can_do_misc_trans(fusion_stack, outer_loop);
  BOOL is_candidate = p_val.first;

  if (is_candidate) {
    UINT32 path_code = p_val.second; 
    if (path_code > 0) {
      is_candidate = Process_non_identical_nodes(fusion_stack, outer_loop, path_code);

      if (is_candidate) {
	SC_NODE * outer_body = outer_loop->Find_kid_of_type(SC_LP_BODY);
	SC_NODE * inner_loop = fusion_stack->Top_nth(0);
	
	if (inner_loop->Parent() == outer_body) {
	  std::pair<SC_NODE *, BOOL> p_val = Do_misc_fusion(fusion_stack, outer_loop);
	  is_candidate = p_val.second;

	  if (is_candidate) {
	    SC_NODE * sc_first = p_val.first;
	    STACK<SC_NODE *> * stk_def = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
	    is_candidate = Collect_killing_defs(stk_def, outer_loop, sc_first);
	    
	    if (is_candidate) {
	      if (Is_disjoint(stk_def, sc_first)) {
		SC_NODE * sc_body = sc_first->First_kid_of_type(SC_LP_BODY);
		SC_NODE * swp_begin= NULL;
		SC_NODE * swp_end = NULL;
		SC_NODE * sc_tmp = Do_tail_merge(sc_body);
		SC_NODE * sc_prev = NULL;
		
		// Find SC_IFs that will be further swapped inwards.
		if (sc_tmp) {
		  swp_begin = sc_body->First_kid_of_type(SC_IF);
		  swp_end = sc_tmp->Prev_sibling_of_type(SC_IF);
		}
		else {
		  sc_tmp = Do_head_merge(sc_body);
		  if (sc_tmp) {
		    swp_begin = sc_tmp->Next_sibling_of_type(SC_IF);
		    swp_end = sc_body->Last_kid_of_type(SC_IF);
		    sc_prev = sc_tmp;
		  }
		}

		// Hoist up SC_NODEs succeeding the 'sc_first' to prepare for loop distribution.
		if (Do_hoist_next_siblings(sc_first)) {
		  // duplicate and insert killing defs before 'sc_first'.
		  std::pair<SC_NODE *, SC_NODE *> p_val = Do_insert_defs(sc_first, stk_def);
		  SC_NODE * sc_begin = p_val.first;
		  SC_NODE * sc_end = p_val.second;
		  if (sc_begin) {
		    // Push duplicated defs into the inner loop.
		    Do_push_nodes(sc_begin, sc_end, outer_loop, sc_first);
		  
		    // Do loop distribution.
		    if (Do_loop_dist(outer_loop, FALSE, sc_first)) {
		      outer_loop = inner_loop->Find_parent_of_type(SC_LOOP);
		      // Do loop interchange.
		      if (Do_loop_interchange(outer_loop, sc_first)) {
			if (swp_begin && swp_end) {
			  SC_NODE * sc_if = swp_begin;
			  Infer_val_range(sc_if, sc_if->Next_sibling());
			  // Push loop invariant SC_IFs inward.
			  while (sc_if) {
			    SC_NODE * sc_next;
			    if (sc_if == swp_end)
			      sc_next = NULL;
			    else
			      sc_next = sc_if->Next_sibling_of_type(SC_IF);
			    Do_swap_if(sc_if);
			    sc_if = sc_next;
			  }
			  Delete_val_range_maps();
			  Bottom_up_prune(sc_body);

			  // Do if-merging
			  IF_MERGE_TRANS::Top_down_trans(sc_body);
			
			  if (!sc_prev)
			    sc_prev = sc_end;

			  sc_if = sc_prev->Next_sibling_of_type(SC_IF);
			  Infer_val_range(sc_if, sc_if->Next_sibling());
			  Top_down_do_rev_head_merge(sc_if);
			
			  if (Is_invariant(sc_first, sc_if->Head(), 0)
			      && sc_if->Last_kid()->Is_empty()) {
			    // Do head duplications so that sc_if is the 1st kid
			    // of its parent.
			    SC_NODE * sc_else = sc_if->Find_kid_of_type(SC_ELSE);
			    sc_prev = NULL;
			    SC_NODE * sc_tmp1 = sc_if->Prev_sibling();
			    while (sc_tmp1) {
			      if (sc_tmp1 == sc_end) 
				sc_prev = sc_else->First_kid();
			      Do_head_duplication(sc_tmp1, sc_if);
			      sc_tmp1 = sc_if->Prev_sibling();
			    }
			  
			    if (Do_canon(sc_if, NULL, HEAD_DUP)) {
			      // If nodes in [~, sc_prev) have no dependency on
			      // nodes in [sc_prev, ~], prune away nodes in 
			      // [~, sc_prev).
			      sc_tmp1 = sc_prev->Prev_sibling();
			      while (sc_tmp1) {
				BOOL has_dep = FALSE;
				SC_NODE * sc_tmp2 = sc_prev;
				while (sc_tmp2) {
				  if (Has_dependency(sc_tmp1, sc_tmp2)) {
				    has_dep = TRUE;
				    break;
				  }
				  sc_tmp2 = sc_tmp2->Next_sibling();
				}
				if (!has_dep) 
				  Remove_node(sc_tmp1);
				sc_tmp1 = sc_prev->Prev_sibling();
			      }
			    
			      // Hoist invariant if-conditions out of the inner loop.
			      Top_down_do_loop_unswitch(sc_first);
			    
			      sc_body = outer_loop->Find_kid_of_type(SC_LP_BODY);
			      // Do pre-compute of if-conditions.
			      Top_down_do_precomp(outer_loop, sc_body);
			    }
			  }
			  Delete_val_range_maps();
			}
		      }
		    }
		  }
		}
	      }
	    }
	    CXX_DELETE(stk_def, _pool);
	  }
	}
	else {
	  IF_MERGE_TRANS::Top_down_trans(outer_body);
	  if (Process_identical_nodes(outer_loop, inner_loop))
	    return TRUE;
	}
      }
    }
  }   
  return FALSE;
}

// Visit the SC tree rooted at the given node in a top down order
// and invoke proactive loop interchange transformation. 
// Inner loop nests are processed before outer loop nests.
// Return TRUE if there is no need to continue the iteration.
BOOL
PRO_LOOP_INTERCHANGE_TRANS::Top_down_trans(SC_NODE * sc)
{
  SC_NODE * outer_loop = NULL;
  SC_NODE * inner_loop;

  if (sc->Type() == SC_LOOP) {
    if (!_local_stack->Is_Empty()) {
      outer_loop = _local_stack->Top();
      _outer_stack->Push(outer_loop);
      _inner_stack->Push(sc);
    }
    _local_stack->Push(sc);
  }

  SC_NODE * child = sc->First_kid();
  SC_NODE * next;
  while (child) {
    next = child->Next_sibling();
    if (Top_down_trans(child)) {
      return FALSE;
    }
    child = next;
  }
  
  BOOL do_restart = FALSE;

  if (sc->Type() == SC_LOOP) {
    FmtAssert(((_local_stack->Top() == sc)), ("Unmatched SC_LOOP stack"));
    outer_loop = sc;
    int i;

    _local_stack->Pop();

    STACK<SC_NODE *> * sc_stack = NULL;
    STACK<SC_NODE *> * buddy_stack = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
    STACK<SC_NODE *> * fusion_stack = NULL;
    int size = 0;
    
    while (!_outer_stack->Is_Empty() && (_outer_stack->Top() == sc)) {
      _outer_stack->Pop();
      inner_loop = _inner_stack->Pop();

      // Do canonicalization.
      Do_canon(outer_loop, inner_loop, SPLIT_IF_HEAD);
      
      // Find candidate loop nests.
      if (Is_candidate(outer_loop, inner_loop)) {
	if (!Do_ext_trans()) {
	  // Find loop fusion buddies and do lock-step buddy fusion first.
	  SC_NODE * sc_buddy = Find_fusion_buddy(inner_loop, buddy_stack);
	
	  if (sc_buddy) 
	    Do_lock_step_fusion(inner_loop, sc_buddy);
	  else
	    buddy_stack->Push(inner_loop);

	  if (!sc_stack) 
	    sc_stack = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);	  
	  sc_stack->Push(inner_loop);
	  size++;
	}
      }
      else if (Do_ext_trans() 
	       && Can_interchange(outer_loop, inner_loop)
	       && outer_loop->Is_sese()
	       && inner_loop->Is_sese()
	       && (inner_loop->Parent()->First_kid_of_type(SC_LOOP) == inner_loop)
	       && (inner_loop->Next_sibling_of_type(SC_LOOP) == NULL)) {
	if (!fusion_stack)
	  fusion_stack = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
	fusion_stack->Push(inner_loop);
      }
    }

    // Destruct buddy_stack.
    while (!buddy_stack->Is_Empty()) 
      buddy_stack->Pop();

    CXX_DELETE(buddy_stack, _pool);
    buddy_stack = NULL;

    SC_NODE * sc_p = outer_loop->Parent();

    // Pop restart points that are not in the same predecessor sub-tree.
    while (!_restart_stack->Is_Empty()) {
      SC_NODE * sc_tmp = _restart_stack->Top();
      if (!sc_tmp->Is_pred_in_tree(outer_loop))
	_restart_stack->Pop();
      else
	break;
    }

    if (size > 0) {
      _action = DO_NONE;
      _action |= DO_TREE_HEIGHT_RED;
      _action |= DO_IF_COND_DIS;     
      _action |= DO_LOOP_UNS;
      _action |= DO_REV_LOOP_UNS;

      for (i = 0; i < size; i++) {
	inner_loop = sc_stack->Top_nth(i);

	if (inner_loop->Parent() == NULL)
	  continue;

	// Do proactive loop interchange transformations.
	int ret_val = Nonrecursive_trans(outer_loop, inner_loop);

	// If loop unswitching happens, reiterate on the outer_loop's parent.
	if ((ret_val & DO_LOOP_UNS) != 0) {
	  do_restart = TRUE;
	  break;
	}
      }

      CXX_DELETE(sc_stack, _pool);

      if (do_restart) {
	// Save restart point
	_restart_stack->Push(sc_p);
	Top_down_trans(sc_p);
      }
    }
    else if (!_restart_stack->Is_Empty()) {
      SC_NODE * sc_bk = _restart_stack->Top();
      // Do loop distribution, loop fusion and loop interchange on restarting iterations
      // (Hence we restrict LNO transformations to loop nests processed by proactive
      // loop interchange automatons).
      // 
      // Loop distribution and loop fusion can enable loop interchange. Loop interchange 
      // can expose more opportunities for proactive loop fusion.  We try to solve 
      // phase-ordering problem here by selectively doing some LNO transformations on 
      // simplest loop nests.
      // TODO: code sharing with LNO.
      if (sc_bk->Is_pred_in_tree(outer_loop)
	  && outer_loop->Loopinfo()->Is_flag_set(LOOP_PRE_DO)) {
	SC_NODE * sc_tmp = outer_loop->Find_kid_of_type(SC_LP_BODY);
	SC_NODE * inner_loop = sc_tmp->Find_kid_of_type(SC_LOOP);

	if (inner_loop && inner_loop->Loopinfo()->Is_flag_set(LOOP_PRE_DO)) {
	  // Do loop fusion 
	  if (Can_fuse(inner_loop)) {
	    sc_tmp = Do_loop_fusion(inner_loop, 0);
	    if (sc_tmp){
	      IF_MERGE_TRANS::Top_down_trans(sc_tmp->Find_kid_of_type(SC_LP_BODY));
            }
	  }

	  // Do loop distribution
	  SC_NODE * sc_new = Do_loop_dist(outer_loop, TRUE, NULL);
	  if (sc_new) 
	    outer_loop = sc_new;

	  // Do loop interchange for a perfect loop-nest.	
	  if (Is_perfect_loop_nest(outer_loop)) {
	    sc_tmp = outer_loop->Find_kid_of_type(SC_LP_BODY);
	    inner_loop = sc_tmp->Find_kid_of_type(SC_LOOP);

	    if (Can_interchange(outer_loop, inner_loop)) 
	      Do_loop_interchange(outer_loop, inner_loop);
	  }
	}
      }
    }
    else if (fusion_stack) {
      SC_NODE * sc_prev = outer_loop->Prev_sibling();
      SC_NODE * sc_parent = outer_loop->Parent();
      BOOL do_reiter = Do_misc_trans(fusion_stack, outer_loop);
      CXX_DELETE(fusion_stack, _pool);	

      if (do_reiter) {
	SC_NODE * sc_if = sc_prev ? sc_prev->Next_sibling() : sc_parent->First_kid();
	FmtAssert((sc_if && (sc_if->Type() == SC_IF)), ("Expect a SC_IF."));
	Top_down_trans(sc_if);
      }
    }
    else if (Do_ext_trans()) {
      Top_down_do_loop_unswitch(outer_loop);
    }
  }

  return do_restart;
}

// Remove the given block from the CFG.
void
CFG_TRANS::Remove_block(BB_NODE * bb)
{
  CFG * cfg = _cu->Cfg();
  BB_NODE * bb_succ = bb->Nth_succ(0);
  BB_NODE * bb_tmp;
  BB_LIST_ITER bb_list_iter;

  FmtAssert((bb->Kind() == BB_GOTO), ("Expect a BB_GOTO"));

  if (_trace)
    printf("\n\t\t Remove block (BB:%d)\n", bb->Id());

  if (cfg->Feedback())
    cfg->Feedback()->Delete_edge(bb->Id(), bb_succ->Id());

  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb->Pred())) {
    if (bb_tmp->Is_branch_to(bb)) {
      WN * branch_wn = bb_tmp->Branch_wn();
      cfg->Add_label_with_wn(bb_succ);
      WN_label_number(branch_wn) = bb_succ->Labnam();
    }

    bb_tmp->Replace_succ(bb, bb_succ);
    
    if (cfg->Feedback())
      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb->Id(), bb_succ->Id());
  }
  
  bb_succ->Set_pred(bb->Pred());
  bb_tmp = bb->Prev();
  bb_tmp->Set_next(bb_succ);
  bb_succ->Set_prev(bb_tmp);
}

// Remove all blocks in the given SC_BLOCK from the CFG.
void
CFG_TRANS::Remove_block(SC_NODE * sc)
{
  FmtAssert((sc->Type() == SC_BLOCK), ("Expect a SC_BLOCK"));
  BB_NODE * bb_tmp;
  BB_LIST_ITER bb_list_iter;
  SC_NODE * sc_prev = sc->Prev_sibling();
  SC_NODE * sc_parent = sc->Get_real_parent();
  CFG * cfg = _cu->Cfg();

  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(sc->Get_bbs())) {
    Remove_block(bb_tmp);
  }

  sc->Unlink();
  sc->Delete();

  if (sc_prev)
    cfg->Fix_info(sc_prev);

  if (sc_parent)
    cfg->Fix_info(sc_parent);

  Inc_transform_count();
}

// Remove a SC_NODE 'sc' which is a SC_BLOCK, SC_IF or SC_LOOP.
void
CFG_TRANS::Remove_node(SC_NODE * sc)
{
  FmtAssert(sc->Is_sese(), ("Expect a SESE"));

  SC_TYPE type = sc->Type();
  CFG * cfg = _cu->Cfg();

  if (type == SC_BLOCK) {
    Remove_block(sc);
    return;
  }
  else if ((type != SC_IF) && (type != SC_LOOP))
    return;

  if (_trace) 
    printf("\n\t\t Remove node (SC:%d)\n", sc->Id());

  BB_NODE * bb_head = sc->Head();
  BB_NODE * bb_merge = sc->Merge();
  BB_NODE * bb_tmp;
  BB_LIST_ITER bb_list_iter;
  SC_NODE * sc_prev = sc->Prev_sibling();
  SC_NODE * sc_parent = sc->Get_real_parent();

  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_head->Pred())) {
    if (bb_tmp->Is_branch_to(bb_head)) {
      WN * branch_wn = bb_tmp->Branch_wn();
      cfg->Add_label_with_wn(bb_merge);
      WN_label_number(branch_wn) = bb_merge->Labnam();
    }

    bb_tmp->Replace_succ(bb_head, bb_merge);

    if (cfg->Feedback())
      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_head->Id(), bb_merge->Id());
  }

  bb_merge->Set_pred(bb_head->Pred());
  bb_tmp = bb_head->Prev();
  bb_tmp->Set_next(bb_merge);
  bb_merge->Set_prev(bb_tmp);
  
  sc->Unlink();
  sc->Delete();

  if (sc_prev)
    cfg->Fix_info(sc_prev);

  if (sc_parent)
    cfg->Fix_info(sc_parent);

  cfg->Invalidate_and_update_aux_info(FALSE);
  cfg->Invalidate_loops();
  Inc_transform_count();
}

// Remove nodes between 'sc1' and 'sc2' which are SC_LOOPs with the same parent.
void
CFG_TRANS::Remove_peel(SC_NODE * sc1, SC_NODE * sc2)
{
  if ((sc1->Type() == SC_LOOP) && (sc2->Type() == SC_LOOP)
      && (sc1->Parent() == sc2->Parent())
      && (sc1->Next_sibling() != sc2)) {
    CFG * cfg = _cu->Cfg();
    BB_NODE * merge1 = sc1->Merge();
    BB_NODE * head2 = sc2->Head();
    BB_NODE * bb_tmp;
    BB_LIST_ITER bb_list_iter;
    SC_NODE * sc_parent = sc1->Get_real_parent();
    
    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(merge1->Pred())) {
      if (bb_tmp->Is_branch_to(merge1)) {
	WN * branch_wn = bb_tmp->Branch_wn();
	cfg->Add_label_with_wn(head2);
	WN_label_number(branch_wn) = head2->Labnam();
      }

      bb_tmp->Replace_succ(merge1, head2);
      
      if (cfg->Feedback())
	cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), merge1->Id(), head2->Id());
    }

    head2->Set_pred(merge1->Pred());
    bb_tmp = merge1->Prev();
    bb_tmp->Set_next(head2);
    head2->Set_prev(bb_tmp);

    SC_NODE * sc_iter = sc1->Next_sibling();
    SC_NODE * sc_next;
    while (sc_iter && (sc_iter != sc2)) {
      sc_next = sc_iter->Next_sibling();
      sc_iter->Unlink();
      sc_iter->Delete();
      sc_iter = sc_next;
    }

    cfg->Fix_info(sc1);

    if (sc_parent)
      cfg->Fix_info(sc_parent);

    cfg->Invalidate_and_update_aux_info(FALSE);
    cfg->Invalidate_loops();
    Inc_transform_count();
  }
}

// Backward iterate statements in sc1, which is a single-BB-NODE SC_BLOCK,
// find those having no dependencies to sc_loop and sc_loop's
// consecutive siblings and create a new BB_NODE to host them.
SC_NODE *
CFG_TRANS::Split(SC_NODE * sc1, SC_NODE * sc_loop)
{
  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();
  BB_LIST * bb_list = sc1->Get_bbs();    
  
  if ((sc1->Type() == SC_BLOCK)
      && (bb_list->Len() == 1)
      && (sc1->Executable_stmt_count() > 1)) {
    BB_NODE * bb = bb_list->Node();
    WN * wn_iter = bb->Laststmt();
    WN * wn_par = NULL;
    BOOL is_real = FALSE;
    
    while (wn_iter) {
      BOOL has_dep = FALSE;

      SC_NODE * sc_tmp = sc_loop;	
      while (sc_tmp) {
	if (Has_dependency(sc_tmp, wn_iter)) {
	  has_dep = TRUE;
	  break;
	}
	sc_tmp = sc_tmp->Next_sibling();
      }
      
      if (!has_dep) {
	wn_par = wn_iter;
	wn_iter = WN_prev(wn_iter);
	if (WN_is_executable(wn_par))
	  is_real = TRUE;
      }
      else
	break;
    }

    WN * wn_last = bb->Laststmt();
    WN * wn_first = bb->Firststmt();

    if (wn_par && (wn_par != wn_first) && is_real) {
      BB_NODE * bb_new = cfg->Create_and_allocate_bb(bb->Kind());
      WN * prev_stmt = WN_prev(wn_par);
      BB_NODE * bb_tmp;

      WN_next(prev_stmt) = NULL;
      WN_prev(wn_par) = NULL;
      bb->Set_laststmt(prev_stmt);
      bb_new->Set_firststmt(wn_par);
      bb_new->Set_laststmt(wn_last);

      bb_tmp = bb->Succ()->Node();
      bb_new->Set_succ(bb->Succ());
      bb_tmp->Replace_pred(bb, bb_new);

      bb_list = CXX_NEW(BB_LIST(bb_new), pool);
      bb->Set_succ(bb_list);
      
      bb_list = CXX_NEW(BB_LIST(bb), pool);
      bb_new->Set_pred(bb_list);

      if (cfg->Feedback()) {
	cfg->Feedback()->Add_node(bb_new->Id());
	cfg->Feedback()->Move_edge_dest(bb->Id(), bb_tmp->Id(), bb_new->Id());
	FB_FREQ freq = cfg->Feedback()->Get_edge_freq(bb->Id(), bb_new->Id());
	cfg->Feedback()->Add_edge(bb_new->Id(), bb_tmp->Id(), FB_EDGE_OUTGOING, freq);
      }
      
      bb_tmp = bb->Next();
      bb_new->Set_next(bb_tmp);
      bb_tmp->Set_prev(bb_new);
      bb->Set_next(bb_new);
      bb_new->Set_prev(bb);

      SC_NODE * sc_new = cfg->Create_sc(SC_BLOCK);
      sc_new->Append_bbs(bb_new);
      sc1->Insert_after(sc_new);
      cfg->Fix_info(sc1->Get_real_parent());

      return sc1;
    }
  }

  return NULL;
}

// Add map of aux_id to WN * in _def_map.
void
CFG_TRANS::Add_def_map(AUX_ID aux_id, WN * wn)
{
  _def_map[aux_id] = wn;
}

// Add an elememt to _def_wn_map.
void
CFG_TRANS::Add_def_wn_map(WN * wn_key, WN * wn_val)
{
  OPERATOR opr = WN_operator(wn_key);
  
  if (OPERATOR_is_scalar_load(opr) || OPERATOR_is_scalar_store(opr)) {
    WN * wn_def = _def_wn_map[WN_aux(wn_key)];

    if (!wn_def) 
      _def_wn_map[WN_aux(wn_key)] = wn_val;
  }
}

// Do copy propagation for all loads in the wn.
void
CFG_TRANS::Copy_prop(WN * wn)
{
  for ( int i = 0; i < WN_kid_count(wn); i++) {
    WN * wn_kid = WN_kid(wn, i);
    OPERATOR opr = WN_operator(wn_kid);

    if (OPERATOR_is_scalar_load(opr)) {
      AUX_ID aux_id = WN_aux(wn_kid);
      if (aux_id) {
	WN * wn_val = _def_map[aux_id];
	if (wn_val) {
	  WN_kid(wn, i) = WN_COPY_Tree_With_Map(wn_val); 
	  continue;
	}
      }
    }
    
    Copy_prop(wn_kid);
  }
}

// Do copy propagation for all loads in the bb.
void
CFG_TRANS::Copy_prop(BB_NODE * bb)
{
  STMT_ITER stmt_iter;
  WN * wn_iter;
  
  FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {
    Copy_prop(wn_iter);
  }
}

// Do copy propagation for all loads in the sc.
void
CFG_TRANS::Copy_prop(SC_NODE * sc)
{
  BB_NODE * bb = sc->Get_bb_rep();
  
  if (bb != NULL)
    Copy_prop(bb);

  BB_LIST * bb_list = sc->Get_bbs();
  
  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;

    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      Copy_prop(tmp);
    }
  }
  
  SC_LIST * kids = sc->Kids();

  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE * tmp;

    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      Copy_prop(tmp);
    }
  }
}

// Partition sc_loop's siblings into two groups via code motion.
// Group 2 initially contains the sc_loop and its succeeding siblings.
// Group 1 contains the sc_loop's preceding siblings that do not have
// dependency on Group 2.
// Return the first element in Group 2.

SC_NODE *
CFG_TRANS::Do_partition(SC_NODE * sc_loop)
{
  SC_NODE * sc_begin = sc_loop;
  SC_NODE * sc1 = sc_begin->Prev_sibling();
  SC_NODE * sc2;

  while (sc1) {
    BOOL has_dep = FALSE;
    sc2 = sc_begin;

    while (sc2) {
      if (Has_dependency(sc1, sc2)) {
	has_dep = TRUE;
	break;
      }
      sc2 = sc2->Next_sibling();
    }

    SC_NODE * sc_next = sc1->Next_sibling();
    
    if (has_dep) {
      if (sc_next == sc_begin) 
	sc_begin = sc1;
      else if (!Has_dependency(sc1, sc_next)
	       && Can_be_speculative(sc_next))
	Do_code_motion(sc1, sc_next);
      else 
	sc_begin = sc1;

      sc1 = sc_begin->Prev_sibling();
    }
    else
      sc1 = sc1->Prev_sibling();
  }
  
  return sc_begin;
}

// Move all SC_BLOCKs succeeding the loop before the loop excluding the last empty block.
// Return TRUE if successful.
BOOL 
CFG_TRANS::Hoist_succ_blocks(SC_NODE * sc_loop)
{
  SC_NODE * sc_parent = sc_loop->Parent();
  SC_NODE * sc1 = sc_loop;
  SC_NODE * sc2 = sc1->Next_sibling();

  while (sc1 && sc2) {
    if ((sc1->Type() == SC_LOOP) && (sc2->Type() == SC_BLOCK)) {
      if (Has_dependency(sc1, sc2)
	  || !Can_be_speculative(sc2))
	return FALSE;

      if (sc2 == sc_parent->Last_kid()) {
	if (!sc2->Is_empty_block())
	  return FALSE;

	break;
      }
      else {
	Do_code_motion(sc1, sc2);

	sc1 = sc2->Prev_sibling();
	if ((sc1 == NULL) || (sc1->Type() != SC_LOOP)) {
	  sc1 = sc2;
	  sc2 = sc1->Next_sibling();
	}
      }
    }
    else {
      sc1 = sc2;
      sc2 = sc2->Next_sibling();
    }
  }

  return TRUE;
}

// Do canonicalization to prepare for loop distribution.
// - Rearrange order of sc_loop's siblings so that:
//   1. All SC_BLOCKs excluding the last SC_LOOP's merge appear before the SC_LOOPs.
//   2. All nodes having dependency to SC_LOOPs are adjacent to SC_LOOPs.
//
// - If possible, do copy propagation to remove the dependency of SC_LOOPs on SC_BLOCKS.
//
// Return the first SC_NODE * that has dependency on the SC_LOOPs.
SC_NODE * 
CFG_TRANS::Do_pre_dist(SC_NODE * sc_loop, SC_NODE * outer_loop)
{
  CFG * cfg = _cu->Cfg();
  SC_NODE * sc1;
  SC_NODE * sc2;
  SC_NODE * sc_parent = sc_loop->Parent();

  // All siblings of sc_loop should be a SC_LOOP or a SC_BLOCK.  
  sc1 = sc_parent->First_kid();
  while (sc1) {
    SC_TYPE sc_type = sc1->Type();
    if ((sc_type != SC_BLOCK) && (sc_type != SC_LOOP))
      return NULL;
    sc1 = sc1->Next_sibling();
  }

  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return NULL;

  // Iterate sc_loop's succeeding siblings, split empty SC_BLOCKS so that each of them
  // contains one BB_NODE *.
  sc1 = sc_loop->Next_sibling_of_type(SC_BLOCK);

  while (sc1 && (sc1->Type() == SC_BLOCK)) {
    if (sc1->Is_empty_block() 
	&& (sc1->Get_bbs()->Len() > 1)) {
      sc1 = cfg->Split(sc1);
      continue;
    }
    sc1 = sc1->Next_sibling();
  }

  // Move all SC_BLOCKs succeeding the loop above the loop excluding the last empty block.
  if (!Hoist_succ_blocks(sc_loop))
    return NULL;

  sc1 = sc_parent->Last_kid();
  if (!sc1->Is_empty_block()) 
    cfg->Insert_block_after(sc1);

  // Iterate sc_loop's preceding siblings, remove empty SC_BLOCKS, split
  // SC_BLOCKS so that each SC_BLOCK contains one BB_NODE *, split SC_BLOCKS
  // having single-BB-NODE to separate out statements having no dependencies to
  // SC_LOOPs.
  sc1 = sc_parent->First_kid();

  while (sc1 && (sc1 != sc_loop)) {
    sc2 = sc1->Next_sibling();

    if (sc1->Is_empty_block()) {
      Remove_block(sc1);
      sc1 = sc2;
    }
    else if (sc1->Get_bbs()->Len() > 1) 
      sc1 = cfg->Split(sc1);
    else {
      sc1 = Split(sc1, sc_loop);
      
      if (sc1 == NULL)
	sc1 = sc2;
    }
  }

  cfg->Fix_info(sc_loop->Get_real_parent());

  // Partition sc_loop's siblings into two groups, where the 2nd group contains
  // all SC_LOOPs and their dependent SC_BLOCKs.

  SC_NODE * sc_begin = Do_partition(sc_loop);

  // Check whether the dependent blocks only contain store statements that can be
  // copied progagated to its uses.
  // 1. Stores have no side effect.
  // 2. Stores's data are expressions of loop invariant or loop index
  //    w.r.t. to the outer_loop.
  // 3. All of stores' uses appear in the SC_LOOPs (This is TRUE if the store
  //    has a single-def).
  
  _def_map.clear();
  sc1 = sc_begin;

  while (sc1 != sc_loop) {
    BB_LIST * bb_list = sc1->Get_bbs();
    BB_NODE * bb_tmp;
    BB_LIST_ITER bb_list_iter;
    
    if (bb_list != NULL) {
      STMT_ITER stmt_iter;
      WN * wn_iter;
      WN * wn_index = NULL;
      AUX_ID loop_aux_id = 0;

      if (outer_loop) {
	WN * wn_load = Get_index_load(outer_loop);
	loop_aux_id = wn_load ? WN_aux(wn_load) : 0;
      }
      
      FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_list)) {
	FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb_tmp->Firststmt(), bb_tmp->Laststmt())) {
	  OPERATOR opr = WN_operator(wn_iter);

	  if (OPERATOR_is_store(opr)) {
	    WN * wn_data = WN_kid0(wn_iter);
	    if (Has_side_effect(wn_iter)
		|| (outer_loop && !Is_invariant(outer_loop, wn_data, loop_aux_id))
		|| (Get_def_cnt(WN_aux(wn_iter)) > 1)) {
	      sc_begin = NULL;
	      break;
	    }

	    Add_def_map(WN_aux(wn_iter), wn_data);
	  }
	  else if (WN_is_executable(wn_iter)) {
	    sc_begin = NULL;
	    break;
	  }
	}
      }

      if (sc_begin == NULL)
	break;
    }

    sc1 = sc1->Next_sibling();
  }

  // Do copy propagation if possible to make sc_loop a partition point.
  if (sc_begin && (sc_begin != sc_loop)) {
    sc1 = sc_loop;

    while (sc1) {
      Copy_prop(sc1);
      sc1 = sc1->Next_sibling();
    }

    // Re-partition after copy propagation.
    sc_begin = Do_partition(sc_loop);
  }

  Inc_transform_count();

  return sc_begin;
}

// Split head of sc_if so that it contains a single statement.
void
CFG_TRANS::Do_split_if_head(SC_NODE * sc_if)
{
  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();
  BB_NODE * bb_head = sc_if->Head();
  WN * branch_wn = bb_head->Branch_wn();
  BB_NODE * bb_new;      
  BB_NODE * bb_tmp;
  BB_LIST_ITER bb_list_iter;
  BB_IFINFO * ifinfo;
  BB_LIST * bb_list_new;
  FB_FREQ freq;
  SC_NODE * sc_new;

  if (bb_head->Executable_stmt_count() > 1) {
    WN * wn_prev = WN_prev(branch_wn);	  
    ifinfo = bb_head->Ifinfo();

    if (_trace)
      printf("\n\t\t Split if-head(SC%d)\n", sc_if->Id());

    WN_next(wn_prev) = NULL;
    WN_prev(branch_wn) = NULL;
    bb_head->Set_laststmt(wn_prev);

    bb_new = cfg->Create_and_allocate_bb(bb_head->Kind());
    bb_new->Set_firststmt(branch_wn);
    bb_new->Set_laststmt(branch_wn);

    if (cfg->Feedback())
      cfg->Feedback()->Add_node(bb_new->Id());

    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_head->Succ())) {
      bb_tmp->Replace_pred(bb_head, bb_new);

      if (cfg->Feedback()) {
	FB_EDGE_TYPE edge_type = cfg->Feedback()->Get_edge_type(bb_head->Id(), bb_tmp->Id());
	freq = cfg->Feedback()->Get_edge_freq(bb_head->Id(), bb_tmp->Id());
	cfg->Feedback()->Delete_edge(bb_head->Id(), bb_tmp->Id());
	cfg->Feedback()->Add_edge(bb_new->Id(), bb_tmp->Id(), edge_type, freq);
      }
    }
	  
    if (cfg->Feedback()) {
      freq = cfg->Feedback()->Get_node_freq_in(bb_head->Id());
      cfg->Feedback()->Add_edge(bb_head->Id(), bb_new->Id(), FB_EDGE_OUTGOING, freq);
    }

    bb_new->Set_succ(bb_head->Succ());
    bb_list_new = CXX_NEW(BB_LIST(bb_new), pool);
    bb_head->Set_succ(bb_list_new);
    bb_list_new = CXX_NEW(BB_LIST(bb_head), pool);
    bb_new->Set_pred(bb_list_new);

    bb_tmp = bb_head->Next();
    bb_tmp->Set_prev(bb_new);
    bb_new->Set_next(bb_tmp);
    bb_new->Set_prev(bb_head);
    bb_head->Set_next(bb_new);

    ifinfo->Set_cond(bb_new);
    bb_head->Set_ifinfo(NULL);
    bb_head->Set_kind(BB_GOTO);
    bb_new->Set_ifinfo(ifinfo);

    sc_if->Set_bb_rep(bb_new);
    sc_new = cfg->Create_sc(SC_BLOCK);
    sc_new->Append_bbs(bb_head);
    sc_if->Insert_before(sc_new);
    cfg->Fix_info(sc_if->Get_real_parent());
    cfg->Invalidate_and_update_aux_info(FALSE);
    cfg->Invalidate_loops();
    Inc_transform_count();
  }
}

// Do canonicalization to produce a perfectly nested SC_IFs between the inner loop
// and the outer loop, i.e., for all SC_IFs in-between:
// 1. Head contains single statement.
// 2. Head is the 1st block of its parent.
// 3. Merge is an empty block.
// Return FALSE if any action can not be honored.
// The routine can also be used to canonicalize a single SC_IF, in this case, 
// 'inner_loop' is NULL and 'outer_loop' is the input SC_IF.
BOOL
CFG_TRANS::Do_canon(SC_NODE * outer_loop, SC_NODE * inner_loop, int action)
{
  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();
  int i;

  if (_tmp_stack == NULL)
    _tmp_stack = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
  else {
    while (!_tmp_stack->Is_Empty())
      _tmp_stack->Pop();
  }

  SC_NODE * sc_cur;

  if (inner_loop) {
    sc_cur = inner_loop->Get_real_parent();
  
    while (sc_cur && (sc_cur != outer_loop)) {
      if (sc_cur->Type() == SC_IF) 
	_tmp_stack->Push(sc_cur);
      sc_cur = sc_cur->Get_real_parent();
    }
  }
  else if (outer_loop && (outer_loop->Type() == SC_IF))
    _tmp_stack->Push(outer_loop);

  int size = _tmp_stack->Elements();
  
  if (size == 0)
    return FALSE;

  // Split head of SC_IF.
  if ((action & SPLIT_IF_HEAD) != 0) {
    for (i = 0; i < size; i++) {
      SC_NODE * sc_if = _tmp_stack->Top_nth(i);
      Do_split_if_head(sc_if);
    }
  }

  // Do head duplication so that head is the first block of its parent.
  
  if ((action & HEAD_DUP) != 0) {
    for (i = 0; i < size; i++) {
      SC_NODE * sc_if = _tmp_stack->Top_nth(i);
      BB_NODE * head = sc_if->Head();
      sc_cur = sc_if->Prev_sibling();
      while (sc_cur) {
	if (((action & CHK_LEGAL) != 0)
	    && Has_dependency(sc_cur, head)) {
	  return FALSE;
	}
	
	Do_head_duplication(sc_cur, sc_if);
	sc_cur = sc_if->Prev_sibling();
      }
    }
  }

  // DO tail duplication so that the merge of a if-region is an empty block.
  if ((action & TAIL_DUP) != 0) {
    for (i = 0; i < size; i++) {
      SC_NODE * sc_if = _tmp_stack->Top_nth(i);
      SC_NODE * sc_parent = sc_if->Parent();
      SC_NODE * next_sibling;
      SC_NODE * last_sibling = sc_parent->Last_kid();
      FmtAssert((last_sibling->Type() == SC_BLOCK), ("Expect a SC_BLOCK"));
      
      if (!last_sibling->Is_empty_block()) 
	cfg->Insert_block_after(last_sibling);
      else if (last_sibling->First_bb() != last_sibling->Last_bb()) {
	BB_LIST * bb_list = last_sibling->Get_bbs();
	BB_LIST_ITER bb_list_iter(bb_list);
	BB_NODE * bb_last = last_sibling->Last_bb();
	BB_NODE * tmp;

	FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
	  if (tmp != bb_last) {
	    SC_NODE * sc_new = cfg->Create_sc(SC_BLOCK);
	    sc_new->Append_bbs(tmp);
	    last_sibling->Insert_before(sc_new);
	    cfg->Fix_info(last_sibling->Get_real_parent());
	  }
	}

	while (bb_list)
	  bb_list = bb_list->Remove(bb_list->Node(), pool);

	last_sibling->Set_bbs(NULL);
	last_sibling->Append_bbs(bb_last);
      }
      
      last_sibling = sc_parent->Last_kid();
      next_sibling = sc_if->Next_sibling();
      
      while (next_sibling && (next_sibling != last_sibling)) {
	if (!next_sibling->Is_empty_block()) {
	  Do_tail_duplication(next_sibling, sc_if);
	}
	else 
	  Remove_block(next_sibling);
	
	next_sibling = sc_if->Next_sibling();
      }
    }
  }

  // Do reversed head-merging of if-region (1) into if-region (2) and then combine if-conditions
  // having the same bodies.
  //
  // From:
  // if (a)            (1)
  //   statement;
  // if (b) {          (2)
  //    if (c) 
  //      statement;
  // }
  // else {
  //    if (d) 
  //      statement;
  // }
  // 
  // To:
  // if (b) {
  //    if (a || c) 
  //     statement;
  // }
  // else {
  //   if (a || d)
  //      statement;
  // }

  if ((action & REV_HEAD_MERGE) != 0) {
    for (i = 0; i < size; i++) {
      SC_NODE * sc_if = _tmp_stack->Top_nth(i);
      SC_NODE * sc_prev = sc_if->Prev_sibling_of_type(SC_IF);
      if (sc_prev) {
	BB_NODE * head = sc_if->Head();
	if (((action & CHK_LEGAL) != 0)
	    && Has_dependency(sc_prev, head))
	  return FALSE;

	if (!sc_prev->Last_kid()->Is_empty())
	  return FALSE;

	SC_NODE * sc1 = sc_if->First_kid()->First_kid_of_type(SC_IF);
	SC_NODE * sc2 = sc_if->Last_kid()->First_kid_of_type(SC_IF);
	WN * cond1 = NULL;
	WN * cond2 = NULL;

	if (sc1 && sc2) {
	  if (((action & CHK_LEGAL) != 0)
	      && (Has_dependency(sc_prev, sc1->Head())
		  || Has_dependency(sc_prev, sc2->Head())))
	    return FALSE;
	  // Check if-regions have same statements.
	  SC_NODE * sc_then = sc_prev->First_kid();
	  for (int j = 0; j < 2; j ++) {
	    SC_NODE * sc_tmp = (j == 0) ? sc1 : sc2;
	    SC_NODE * sc_tmp_then = sc_tmp->First_kid();
	    
	    if (!sc_tmp->Last_kid()->Is_empty())
	      return FALSE;
	    if (!sc_then->Is_same(sc_tmp_then))
	      return FALSE;
	    if (j == 0) 
	      cond1 = sc_tmp->Get_cond();
	    else 
	      cond2 = sc_tmp->Get_cond();
	  }

	  WN * cond = sc_prev->Get_cond();
	  if (!cond || !cond1 || !cond2)
	    return FALSE;

	  if (_trace) {
	    printf("\n\t\t Func: %s(%d) reverse head merge (SC%d SC:%d)\n", 
		   Current_PU_Name(), Current_PU_Count(), sc_prev->Id(), sc_if->Id());
	  }

	  // Change if-condition expressions for sc1 and sc2.
	  for (int j = 0; j < 2; j ++) {
	    SC_NODE * sc_tmp = (j == 0) ? sc1 : sc2;
	    BB_NODE * bb_head = sc_tmp->Head();
	    WN * wn_last = bb_head->Laststmt();	    
	    WN * new_prev_cond = Get_cond(sc_prev, FALSE);
	    cond = (j == 0) ? cond1 : cond2;
	    FmtAssert((new_prev_cond), ("Missing if-condition"));
	    WN * new_cond = Merge_cond(new_prev_cond, cond, OPR_CIOR);
	    WN_kid0(wn_last) = new_cond;
	  }

	  // Remove sc_prev
	  Remove_node(sc_prev);
	}
      }
    }
  }

  return TRUE;
}

// Find tail-merging opportunities in sc_parent's children and do tail-merging.
// return the tail-merging tail node.
// It is caller's responsibility for legality check.
SC_NODE * 
CFG_TRANS::Do_tail_merge(SC_NODE * sc_parent)
{
  SC_NODE * sc_iter1 = sc_parent->First_kid_of_type(SC_IF);
  SC_NODE * sc_iter2 = sc_iter1->Next_sibling_of_type(SC_IF);		
  SC_NODE * sc_tmp1 = sc_iter1->Find_kid_of_type(SC_THEN);
  sc_tmp1 = sc_tmp1->Last_non_empty_kid();		  
  SC_NODE * sc_ret = NULL;
  BOOL do_tail_merge = FALSE;
  CFG * cfg = _cu->Cfg();

  // Find tail-merging opportunities.
  if (sc_tmp1 && sc_tmp1->Is_sese()) {
    SC_TYPE type = sc_tmp1->Type();
    do_tail_merge = TRUE;	
    FmtAssert(sc_iter1->Find_kid_of_type(SC_ELSE)->Is_empty(),
	      ("Expect an empty else-path"));
    while (sc_iter2) {
      FmtAssert(sc_iter2->Find_kid_of_type(SC_ELSE)->Is_empty(),
		("Expect an empty else-path"));
      SC_NODE * sc_tmp2 = sc_iter2->Find_kid_of_type(SC_THEN);
      SC_NODE * sc_last = sc_tmp2->Last_non_empty_kid();
      sc_tmp2 = sc_tmp2->Last_kid_of_type(type);
		  
      if (!sc_tmp2 || !sc_tmp1->Is_same(sc_tmp2) || !sc_tmp2->Is_sese()) {
	do_tail_merge = FALSE;
	break;
      }

      if (sc_last != sc_tmp2) {
	Infer_val_range(sc_iter1, sc_iter2);
	SC_NODE * sc_cur = sc_tmp2->Next_sibling();
	while (sc_cur) {
	  if ((sc_cur->Type() != SC_BLOCK)
	      || Has_dependency(sc_tmp2, sc_cur)
	      || !Can_be_speculative(sc_cur)
	      || !sc_tmp2->Is_ctrl_equiv(sc_cur)) {
	    do_tail_merge = FALSE;
	    break;
	  }
	  sc_cur = sc_cur->Next_sibling();
	}
	Delete_val_range_maps();
	if (!do_tail_merge)
	  break;
	
	// do code motion so that 'sc_tmp2' becomes the last 
	// non-empty kid of its parent.
	if (!sc_last->Is_empty_block()) 
	  sc_last = cfg->Insert_block_after(sc_last);
	sc_cur = sc_tmp2->Next_sibling();
	while (sc_cur && (sc_cur != sc_last)) {
	  Do_code_motion(sc_tmp2, sc_cur);
	  sc_cur = sc_tmp2->Next_sibling();
	}
      }
      sc_iter2 = sc_iter2->Next_sibling_of_type(SC_IF);
    }

    if (do_tail_merge) {
      // Do tail-merging.
      sc_iter2 = sc_iter1;
      SC_NODE * sc_last = sc_parent->Last_kid_of_type(SC_IF);

      if (_trace) {
	printf("\n\t\t Func: %s(%d) do tail merging at SC:(%d)\n", 
	       Current_PU_Name(), Current_PU_Count(), sc_last->Id());
      }

      while (sc_iter2) {
	SC_NODE * sc_tmp = sc_iter2->Find_kid_of_type(SC_THEN);
	SC_NODE * sc_next = sc_iter2->Next_sibling_of_type(SC_IF);
	sc_tmp = sc_tmp->Last_non_empty_kid();

	if (sc_iter2 == sc_last) {
	  if (Do_sink_node(sc_iter2, sc_tmp, FALSE))
	    sc_ret = sc_tmp;
	  break;
	}
	else {
	  Remove_node(sc_tmp);
	  if (sc_iter2->First_kid()->Is_empty()
	      && sc_iter2->Last_kid()->Is_empty())
	    Remove_node(sc_iter2);
	}
		      
	sc_iter2 = sc_next;
      }
    }
  }
  
  return sc_ret;
}

// Find head-merging opportunities in sc_parent's children and do head-merging.
// It is caller's responsibility for legality check.
SC_NODE *
CFG_TRANS::Do_head_merge(SC_NODE * sc_parent)
{
  SC_NODE * sc_iter1 = sc_parent->First_kid_of_type(SC_IF);
  SC_NODE * sc_iter2 = sc_iter1->Next_sibling_of_type(SC_IF);
  SC_NODE * sc_tmp1 = sc_iter1->Find_kid_of_type(SC_THEN);
  sc_tmp1 = sc_tmp1->First_non_empty_kid(); 
  
  BOOL do_head_merge = FALSE;
  SC_NODE * sc_ret = NULL;
  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();

  // Find head-merging opportunities.
  if (sc_tmp1 && sc_tmp1->Is_sese()) {
    SC_TYPE type = sc_tmp1->Type();
    do_head_merge = TRUE;	
    FmtAssert(sc_iter1->Find_kid_of_type(SC_ELSE)->Is_empty(),
	      ("Expect an empty else-path"));

    while (sc_iter2) {
      FmtAssert(sc_iter2->Find_kid_of_type(SC_ELSE)->Is_empty(),
		("Expect an empty else-path"));
      SC_NODE * sc_tmp2 = sc_iter2->Find_kid_of_type(SC_THEN);
      SC_NODE * sc_first = sc_tmp2->First_non_empty_kid();
      sc_tmp2 = sc_tmp2->First_kid_of_type(type);
		  
      if (!sc_tmp2 || !sc_tmp1->Is_same(sc_tmp2) || !sc_tmp2->Is_sese()) {
	do_head_merge = FALSE;
	break;
      }

      if (sc_first != sc_tmp2) {
	if (!Can_be_speculative(sc_tmp2)) {
	  do_head_merge = FALSE;
	  break;
	}

	Infer_val_range(sc_iter1, sc_iter2);
	SC_NODE * sc_cur = sc_first;
	while (sc_cur && (sc_cur != sc_tmp2)) {
	  if ((sc_cur->Type() != SC_BLOCK)
	      || Has_dependency(sc_tmp2, sc_cur)
	      || !sc_cur->Is_ctrl_equiv(sc_tmp2)) {
	    do_head_merge = FALSE;
	    break;
	  }
	  sc_cur = sc_cur->Next_sibling();
	}
	Delete_val_range_maps();

	if (!do_head_merge)
	  break;
	
	// do code motion so that 'sc_tmp2' becomes the first
	// non-empty kid of its parent.
	sc_cur = sc_tmp2->Prev_sibling();
	while (sc_cur) {
	  Do_code_motion(sc_cur, sc_tmp2);
	  sc_cur = sc_tmp2->Prev_sibling();
	}
      }
      sc_iter2 = sc_iter2->Next_sibling_of_type(SC_IF);
    }

    if (do_head_merge) {
      // Do head-merging.
      sc_iter2 = sc_iter1;
      SC_NODE * sc_first = sc_parent->First_kid_of_type(SC_IF);

      if (_trace) {
	printf("\n\t\t Func: %s(%d) do head merging at SC:(%d)\n", 
	       Current_PU_Name(), Current_PU_Count(), sc_first->Id());
      }

      while (sc_iter2) {
	SC_NODE * sc_then = sc_iter2->Find_kid_of_type(SC_THEN); 
	SC_NODE * sc_next = sc_iter2->Next_sibling_of_type(SC_IF);
	SC_NODE * sc_tmp = sc_then->First_non_empty_kid();

	if (sc_iter2 == sc_first) {
	  BB_NODE * bb_head = sc_iter2->Head();
	  BB_NODE * bb_first = sc_tmp->First_bb();
	  BB_NODE * bb_last = sc_tmp->Last_bb();
	  SC_NODE * sc_merge = sc_tmp->Next_sibling();

	  sc_ret = sc_tmp;

	  if (sc_tmp->Type() == SC_IF) {
	    if (!sc_merge->Is_empty_block() 
		|| (sc_merge->Get_bbs()->Len() > 1))
	      sc_merge = cfg->Insert_block_before(sc_merge);
	    bb_last = sc_tmp->Merge();
	  }

	  // Disconnect nodes in [bb_first, bb_last] and insert them before sc_iter2.
	  BB_NODE * bb_tmp;
	  BB_NODE * bb_next = bb_last->Next();
	  BB_LIST_ITER bb_list_iter;
	  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_first->Pred())) {
	    bb_tmp->Replace_succ(bb_first, bb_next);
	    if (bb_tmp->Is_branch_to(bb_first)) {
	      WN * branch_wn = bb_tmp->Branch_wn();
	      cfg->Add_label_with_wn(bb_next);
	      WN_label_number(branch_wn) = bb_next->Labnam();
	    }
	    if (cfg->Feedback())
	      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_first->Id(), bb_next->Id());
	  }
	  
	  bb_next->Set_pred(bb_first->Pred());
	  bb_tmp = bb_first->Prev();
	  bb_next->Set_prev(bb_tmp);
	  bb_tmp->Set_next(bb_next);
	  sc_tmp->Unlink();
	  sc_merge->Unlink();
	  cfg->Fix_info(sc_iter2);
	  
	  if (bb_head->Pred()->Len() > 1)
	    cfg->Insert_block_before(sc_iter2);

	  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_head->Pred())) {
	    bb_tmp->Replace_succ(bb_head, bb_first);
	    if (bb_tmp->Is_branch_to(bb_head)) {
	      WN * branch_wn = bb_tmp->Branch_wn();
	      cfg->Add_label_with_wn(bb_first);
	      WN_label_number(branch_wn) = bb_first->Labnam();
	    }
	    if (cfg->Feedback())
	      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_head->Id(), bb_first->Id());
	  }
	  
	  bb_first->Set_pred(bb_head->Pred());
	  BB_LIST * new_pred = CXX_NEW(BB_LIST(bb_last), pool);
	  bb_head->Set_pred(new_pred);
	  bb_last->Replace_succ(bb_next, bb_head);

	  if (cfg->Feedback())
	    cfg->Feedback()->Move_edge_dest(bb_last->Id(), bb_next->Id(), bb_head->Id());
	  
	  bb_tmp = bb_head->Prev();
	  bb_first->Set_prev(bb_tmp);
	  bb_tmp->Set_next(bb_first);
	  bb_last->Set_next(bb_head);
	  bb_head->Set_prev(bb_last);
	  
	  SC_NODE * sc_prev = sc_iter2->Prev_sibling();
	  sc_iter2->Insert_before(sc_tmp);
	  sc_iter2->Insert_before(sc_merge);

	  if (sc_prev)
	    cfg->Fix_info(sc_prev);
	  cfg->Fix_info(sc_iter2->Get_real_parent());
	  
	  cfg->Invalidate_and_update_aux_info(FALSE);
	  cfg->Invalidate_loops();
	  Inc_transform_count();
	}
	else 
	  Remove_node(sc_tmp);

	if (sc_iter2->First_kid()->Is_empty()
	    && sc_iter2->Last_kid()->Is_empty())
	  Remove_node(sc_iter2);
		      
	sc_iter2 = sc_next;
      }
    }
  }
  return sc_ret;
}

// Top-down traverse SC_TREE rooted at 'sc', do reversed head-merging if possible.
void
CFG_TRANS::Top_down_do_rev_head_merge(SC_NODE * sc)
{
  if (sc->Type() == SC_IF) 
    Do_canon(sc, NULL, REV_HEAD_MERGE | CHK_LEGAL);

  SC_NODE * kid = sc->First_kid();

  while (kid) {
    Top_down_do_rev_head_merge(kid);
    kid = kid->Next_sibling();
  }
}

// Query whether statements in the given block are invariants w.r.t. the given sc.
// st_index gives loop index. If st_index is non-zero, skip stores and loads w.r.t. to 
// the loop index.
BOOL
CFG_TRANS::Is_invariant(SC_NODE * sc, BB_NODE * bb, AUX_ID st_index)
{
  STMT_ITER stmt_iter;
  WN * wn_iter;
  SC_TYPE type = sc->Type();

  // Query from hash.
  std::map<IDTYPE, SC_NODE *> & invar_map = Get_invar_map();
  SC_NODE * sc_hash;
  sc_hash = invar_map[bb->Id()];

  if (sc_hash 
      && (sc_hash == sc || sc_hash->Is_pred_in_tree(sc)))
    return TRUE;

  if ((bb->Executable_stmt_count() == 1)
      && ((type == SC_THEN) || (type == SC_ELSE))) {
    WN * wn1 = bb->Laststmt();
    BB_NODE * bb_head = sc->Parent()->Head();
    
    if (bb_head->Executable_stmt_count() == 1) {
      WN * wn2 = bb_head->Laststmt();

      if (WN_opcode(wn1) == WN_opcode(wn2)) {
	wn1 = WN_kid0(wn1);
	wn2 = WN_kid0(wn2);

	if (WN_Simp_Compare_Trees(wn1, wn2) == 0) 
	  return !Maybe_assigned_expr(sc, wn1, (type == SC_THEN) ? TRUE : FALSE);
      }
    }
  }

  BB_NODE * tmp = sc->Get_bb_rep();

  if ((tmp != NULL) && (!Is_invariant(tmp, bb, st_index)))
    return FALSE;

  BB_LIST * bb_list = sc->Get_bbs();

  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);

    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      if (!Is_invariant(tmp, bb, st_index))
	return FALSE;
    }
  }
  
  SC_LIST * kids = sc->Kids();
  
  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE *tmp;
    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      if (!Is_invariant(tmp, bb, st_index))
	return FALSE;
    }
  }

  // Hash query results.
  Hash_invar(bb, sc);

  return TRUE;
}

// Hash BB_NODE Id to SC_NODE *, where bb is an invariant w.r.t. the sc.
void
CFG_TRANS::Hash_invar(BB_NODE * bb, SC_NODE * sc)
{
  std::map<IDTYPE, SC_NODE *> & invar_map = Get_invar_map();
  SC_NODE * sc_tmp = invar_map[bb->Id()];

  if (!sc_tmp)
    invar_map[bb->Id()] = sc;
  else if ((sc_tmp->Parent() == NULL) || sc->Is_pred_in_tree(sc_tmp)) {
    invar_map[bb->Id()] = sc;
  }
}

// Query whether statements in bb2 are invariants w.r.t. to bb1.
// st_index gives loop index. If st_index is non-zero, skip stores and loads 
// w.r.t. to the loop index.
BOOL
CFG_TRANS::Is_invariant(BB_NODE * bb1, BB_NODE * bb2, AUX_ID index)
{
  STMT_ITER stmt_iter;
  WN * wn_iter;

  FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb2->Firststmt(), bb2->Laststmt())) {
    if (!Is_invariant(bb1, wn_iter, index))
      return FALSE;
  }

  return TRUE;
}

// Query whether expressions in the WHIRL tree rooted at the wn are invariants w.r.t.
// the given bb.  st_index gives loop index. If st_index is non-zero, skip stores and loads 
// w.r.t. to the loop index.
BOOL
CFG_TRANS::Is_invariant(BB_NODE * bb, WN * wn, AUX_ID st_index)
{
  if (st_index) {
    OPERATOR opr = WN_operator(wn);
    if (OPERATOR_is_scalar_load(opr)) {
      if (WN_aux(wn) == st_index)
	return TRUE;
    }
    else if (OPERATOR_is_scalar_store(opr)) {
      if (WN_aux(wn) == st_index)
	return Is_invariant(bb, WN_kid0(wn), st_index);
    }

    int kid_count = WN_kid_count(wn);

    if (kid_count == 0)
      return !Maybe_assigned_expr(bb, wn);
    else {
      for (int i = 0; i < kid_count; i++) {
	if (!Is_invariant(bb, WN_kid(wn, i), st_index))
	  return FALSE;
      }
    }

    return TRUE;
  }
  else 
    return !Maybe_assigned_expr(bb, wn);
}

// Query whether statements in the given sc are loop invariants w.r.t. the given loop.
// st_index gives loop index. If st_index is non-zero, skip stores and loads w.r.t to the
// loop index.
BOOL
CFG_TRANS::Is_invariant(SC_NODE * loop, SC_NODE * sc, AUX_ID st_index)
{
  BB_NODE * bb = sc->Get_bb_rep();

  if ((bb != NULL) && !Is_invariant(loop, bb, st_index))
    return FALSE;
  
  BB_LIST * bb_list = sc->Get_bbs();
  
  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;

    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      if (!Is_invariant(loop, tmp, st_index))
	return FALSE;
    }
  }

  SC_LIST * kids = sc->Kids();
  
  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE *tmp;
    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      if (!Is_invariant(loop, tmp, st_index))
	return FALSE;
    }
  }

  return TRUE;
}

// Query whether loads/stores in the given wn are invariants w.r.t. the given sc.
// st_index gives loop index. If st_index is non-zero, skip stores and loads w.r.t to the
// loop index.
BOOL
CFG_TRANS::Is_invariant(SC_NODE * sc, WN * wn, AUX_ID st_index)
{
  BB_NODE * bb = sc->Get_bb_rep();

  if ((bb != NULL) && !Is_invariant(bb, wn, st_index))
    return FALSE;

  BB_LIST * bb_list = sc->Get_bbs();
  
  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;

    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      if (!Is_invariant(tmp, wn, st_index))
	return FALSE;
    }
  }
  
  SC_LIST * kids = sc->Kids();
  
  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE *tmp;
    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      if (!Is_invariant(tmp, wn, st_index))
	return FALSE;
    }
  }
  
  return TRUE;
}

// Find a loop distribution candidate among sc's children that satisfies the 
// following conditions:
// - It is the first loop among sc's children.
// - It is not the first kid of sc.
// - Every preceding sibling has no dependency on it.
// - Every succeeding sibling is either a loop fusion candidate or an empty block.
//
// In addition, 'sc' should not have compgoto as its immediate child.
SC_NODE *
PRO_LOOP_INTERCHANGE_TRANS::Find_dist_cand(SC_NODE * sc)
{
  // avoid compgoto complication.
  if (sc->Find_kid_of_type(SC_COMPGOTO))
    return NULL;

  SC_NODE * sc_loop = sc->Find_kid_of_type(SC_LOOP);
  SC_NODE * sc_cur;

  if (sc_loop) {
    sc_cur = sc->First_kid();
    
    if (sc_cur == sc_loop)
      return NULL;

    while (sc_cur && (sc_cur != sc_loop)) {
      if (Has_dependency(sc_cur, sc_loop))
	return NULL;
      sc_cur = sc_cur->Next_sibling();
    }
    
    sc_cur = sc_loop->Next_sibling();
    while (sc_cur && (sc_cur->Type() == SC_LOOP)) {
      if (!sc_cur->Has_same_loop_struct(sc_loop))
	return NULL;

      sc_cur = sc_cur->Next_sibling();
    }

    while (sc_cur) {
      if (!sc_cur->Is_empty_block())
	return NULL;
      sc_cur = sc_cur->Next_sibling();
    }

    return sc_loop;
  }

  return NULL;
}

// Check sibling of sc on Rule 2 and 3 in PRO_LOOP_INTERCHANGE_TRANS::Is_candidate.
BOOL 
PRO_LOOP_INTERCHANGE_TRANS::Check_sibling(SC_NODE * sc, SC_NODE * inner_loop)
{
  SC_NODE * sc_parent = sc->Parent();
  SC_NODE * sc_cur = sc_parent->First_kid();
  BOOL check_pred = TRUE;

  if (sc == inner_loop) {
    SC_NODE * sc_dist = Find_dist_cand(sc_parent);
    if (sc_dist == sc) 
      return TRUE;
  }

  while (sc_cur) {
    if (sc_cur == sc) {
      if (sc_cur->Type() == SC_LOOP) {
	// skip succeeding consecutive loop fusion candidates.
	SC_NODE * next_sibling = sc_cur->Next_sibling();
	while (next_sibling && (next_sibling->Type() == SC_LOOP)
	       && sc_cur->Has_same_loop_struct(next_sibling)) {
	  next_sibling = next_sibling->Next_sibling();
	}
	sc_cur = next_sibling;
      }
      check_pred = FALSE;
    }

    if (sc_cur) {
      if (sc_cur != sc) {
	if ((sc_cur->Type() != SC_BLOCK) || !sc_cur->Is_sese())
	  return FALSE;
	
	if (check_pred && (sc->Type() == SC_IF)) {
	  // Check head duplication legality.
	  SC_NODE * sc_iter = inner_loop->Parent();
	  while (sc_iter) {
	    if ((sc_iter->Type() == SC_IF)
		&& Has_dependency(sc_cur, sc_iter->Head()))
	      return FALSE;

	    if (sc_iter == sc)
	      break;
	    sc_iter = sc_iter->Parent();
	  }
	}
      }

      sc_cur = sc_cur->Next_sibling();
    }
  }

  return TRUE;
}

// Query whether given loop-nest is a candidate for proactive loop interchange transformation.
// For every node on the path from the inner_loop to the outer loop (left inclusive).
// 1. Its type is SC_IF, SC_ELSE, SC_THEN, SC_LOOP or SC_LP_BODY. 
// 2. In the case of a SC_IF, it is a loop invariant w.r.t. either the outer_loop or the inner_loop;
//    it is well-behaved; Its head has a single statement.  If it is a loop variant w.r.t the outer
//    loop, its condition expression is re-orderable.  If there exists a preceding sibling, 
//    the sibling must be a SC_BLOCK which has no dependencies on all SC_IF nodes on the successor
//    part of the path (head duplication legality). If there exists a succeeding sibling,
//    the succeeding sibling must be a SC_BLOCK.
// 3. In the case of a SC_LOOP, it must be the inner loop itself and must also be the first loop
//    among its siblings.  It must be a loop distribution point, otherwise, for any preceding
//    sibling, it must be a SC_BLOCK.  For any succeeding siblings, if it is a loop, it must appear
//    in a consecutive sequence of loops that are loop fusion candidates w.r.t. to the inner loop; 
//    if it is not a loop, it must be a SC_BLOCK.
// 4. Both the outer loop and the inner loop are a do-loop.
// 5. The outer loop and the inner loop are interchangable if there was no intervening statement
//    between them.
// 6. None of its siblings and immediate child has SC_COMPGOTO type.
//
// Note that the restriction of "SC_BLOCK" is to simplify the low level transformation machinery
// (head/tail duplication, code motion etc.).  It is not a restriction of the generic proactive
// loop interchange algorithm.
BOOL 
PRO_LOOP_INTERCHANGE_TRANS::Is_candidate(SC_NODE * outer_loop, SC_NODE * inner_loop)
{
  SC_NODE * cur_node = inner_loop;
  BOOL is_cand = TRUE;

  if (inner_loop->Parent() == NULL)
    return FALSE;

  if (inner_loop->Get_real_parent() == outer_loop)
    return FALSE;

  // Rule 5.
  if (!Can_interchange(outer_loop, inner_loop))
    return FALSE;

  // Rule 4.
  if (((outer_loop->Type() == SC_LOOP) && !outer_loop->Loopinfo()->Is_flag_set(LOOP_PRE_DO))
      || !inner_loop->Loopinfo()->Is_flag_set(LOOP_PRE_DO))
    return FALSE;

  while (cur_node && (cur_node != outer_loop)) {
    SC_TYPE type = cur_node->Type();
    SC_NODE * parent_node = cur_node->Parent();
    BB_NODE * bb;
    BOOL is_invar_outer = FALSE;
    BOOL is_invar_inner = FALSE;
    WN * wn_cond = NULL;
    
    // Rule 6.
    if (parent_node->Find_kid_of_type(SC_COMPGOTO)) {
      is_cand = FALSE;
      break;
    }

    // Rule 1.
    switch (type) {
    case SC_THEN:
    case SC_ELSE:
      break;
    case SC_LOOP:
      // Rule 3.
      if ((cur_node != inner_loop) 
	  || (parent_node->Find_kid_of_type(SC_LOOP) != cur_node)
	  || !Check_sibling(cur_node, inner_loop))
	is_cand = FALSE;
      break;

    case SC_IF:
      // Rule 2.
      bb = cur_node->Get_bb_rep();
      wn_cond = WN_kid0(cur_node->Head()->Branch_wn());

      if (Is_invariant(outer_loop, bb, 0))
	is_invar_outer = TRUE;

      if (Is_invariant(inner_loop, bb, 0))
	is_invar_inner = TRUE;
      
      if ((!is_invar_outer && !is_invar_inner)
	  || (!is_invar_outer && !Can_reorder_cond(wn_cond, NULL))
	  || !cur_node->Is_well_behaved()
	  || (bb->Executable_stmt_count() > 1)
	  || !Check_sibling(cur_node, inner_loop))
	is_cand = FALSE;

      break;
    case SC_LP_BODY:
      if (cur_node->Parent() == outer_loop)
	break;

    default:
      is_cand = FALSE;
    }

    if (!is_cand)
      break;
    
    cur_node = cur_node->Parent();
  }

  return is_cand;
}

// Given a SC_IF, obtain a copy of its condition-setting WN and
// invert its opcode if do_invert is TRUE.
WN *
CFG_TRANS::Get_cond(SC_NODE * sc, BOOL do_invert)
{
  WN * old_cond = sc->Get_cond();

  if (old_cond) {
    OPCODE opc_inv = get_inverse_relop(WN_opcode(old_cond));
      
    if (opc_inv != OPCODE_UNKNOWN) {
      WN *new_cond = WN_COPY_Tree_With_Map(old_cond);
      if (do_invert) 
	WN_set_opcode(new_cond, opc_inv);
      return new_cond;
    }
  }

  return NULL;
}

// Merge two condition expression WNs using the given operator.
WN *
CFG_TRANS::Merge_cond(WN * wn1, WN * wn2, OPERATOR opr)
{
  if (wn1 == NULL)
    return wn2;
  else if (wn2 == NULL)
    return wn1;
  else {
    OPCODE op_cand = OPCODE_make_op(opr, Boolean_type, MTYPE_V);
    WN * wn = WN_CreateExp2(op_cand, wn1, wn2);
    return wn;
  }
}

// Merge two adjacent control equivalent SC_BLOCKs.
// Return merged block.
SC_NODE *
CFG_TRANS::Merge_block(SC_NODE * sc1, SC_NODE * sc2)
{
  if ((sc1->Type() == SC_BLOCK)
      && (sc2->Type() == SC_BLOCK)
      && (sc1->Next_sibling() == sc2)
      && sc1->Is_ctrl_equiv(sc2)) {
    BB_NODE * bb_first = sc2->First_bb();
    BB_LIST * bbs = sc2->Get_bbs();
    MEM_POOL * pool = sc2->Get_pool();
      
    while (bbs) {
      BB_NODE * bb = bbs->Node();
      sc1->Append_bbs(bb);
      bbs = bbs->Remove(bb, pool);
    }

    sc2->Set_bbs(NULL);
    sc2->Unlink();
    sc2->Delete();
    Inc_transform_count();
    return sc1;
  }
  return NULL;
}

// Reduce height of if-condition tree between sc1 and sc2, where sc1 is in the ancestor 
// sub-tree of sc2.
BOOL
CFG_TRANS::Do_if_cond_tree_height_reduction(SC_NODE * sc1, SC_NODE * sc2)
{
  SC_NODE * cur_node;
  SC_NODE * parent_node;
  WN * new_cond = NULL;
  WN * old_cond;
  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();

  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return FALSE;

  FmtAssert(((sc1->Type() == SC_IF) && (sc2->Type() == SC_ELSE) || (sc2->Type() == SC_THEN)),
	    ("Expect a SC_ELSE or SC_THEN"));

  // Al SC_IFs in-between sc2 and sc1 (right exclusive) should have empty merge block.
  cur_node = sc2;

  while (cur_node != sc1) {
    if ((cur_node->Type() == SC_IF)
	&& (cur_node->Merge()->Executable_stmt_count() > 0))
      return FALSE;
    cur_node = cur_node->Parent();
  }

  if (_trace)
    printf("\n\t\t If-cond tree height reduction (SC%d,SC%d)\n", 
	   sc1->Id(), sc2->Id());

  cur_node = sc2;
  parent_node = cur_node->Parent();
  
  while (cur_node != sc1) {
    SC_TYPE cur_type = cur_node->Type();
    BOOL do_inverse = FALSE;

    if (cur_type == SC_ELSE) 
      do_inverse = TRUE;

    old_cond = Get_cond(parent_node, do_inverse);
    new_cond = Merge_cond(old_cond, new_cond, OPR_CAND);
    cur_node = parent_node;
    parent_node = cur_node->Parent();
  }

  FmtAssert(new_cond, ("NULL condition"));
  
  // Remove a then-path or a else-path in a CFG (last successor is fall-through).
  // BB39 is head of sc1, BB42 is head of sc2.
  // 
  //      5                      5
  //      |                      |
  //      39                     39   
  //    /    \                  /   \
  //   41    40               41    40
  //  /  \    |             /   \    |     |
  // 62  42   |            62   43   |     42
  // |  /  \  |            |    |    |     |
  // | 51  43 |  ==>       |    44   |     51
  // | |   |  |            |    |    |     |
  // | 60  44 |            |    61   |     60
  // |  \  /  |             \   /    |     |
  // |   61   |              63      |
  // \   /    |               \      /
  //   63     |                 64
  //    \    /                \  |
  //     64                     65
  //  \  |
  //    65
  SC_NODE * sc1_p = sc1->Get_real_parent();
  SC_NODE * sc2_p = sc2->Parent();  
  SC_NODE * sc1_prev = sc1->Prev_sibling();

  FmtAssert((sc2_p->Type() == SC_IF), ("Expect a SC_IF"));
  SC_NODE * sc41 = sc2_p->Get_real_parent();
  BB_NODE * bb41 = sc41->Get_bb_rep();
  BB_NODE * bb42 = sc2_p->Get_bb_rep();
  BB_NODE * bb61 = sc2_p->Merge(); 
  BB_NODE * bb63 = sc41->Merge();
  BB_NODE * bb51 = sc2->First_bb();   
  BB_NODE * bb60 = sc2->Last_bb();    
  SC_NODE * sc2_s = sc2_p->First_kid_of_type((sc2->Type() == SC_ELSE) ? SC_THEN : SC_ELSE);
  BB_NODE * bb43 = sc2_s->First_bb(); 
  BB_NODE * bb44 = sc2_s->Last_bb(); 
  BB_NODE * bb39 = sc1->Get_bb_rep();
  BB_NODE * bb64 = sc1->Merge();
  SC_NODE * sc_tmp1;
  SC_NODE * sc_tmp2;
  BB_NODE * bb_tmp;
  WN * branch_wn;
  FB_FREQ freq;
  FB_EDGE_TYPE edge_type;
  FB_EDGE_TYPE ft_edge_type;
  FB_EDGE_TYPE br_edge_type;

  // Obtain edge types of the fall-through edge and the non-fall-through edge.
  if (cfg->Feedback()) {
    ft_edge_type = cfg->Feedback()->Get_edge_type(bb42->Id(), bb42->If_then()->Id());
    br_edge_type = cfg->Feedback()->Get_edge_type(bb42->Id(), bb42->If_else()->Id());
  }

  bb41->Replace_succ(bb42, bb43);
  BB_IFINFO * info = bb41->Ifinfo();

  if (bb41->Is_branch_to(bb42)) {
    branch_wn = bb41->Branch_wn();
    cfg->Add_label_with_wn(bb43);
    WN_label_number(branch_wn) = bb43->Labnam();
    info->Set_else(bb43);
  }
  else
    info->Set_then(bb43);
  
  bb43->Replace_pred(bb42, bb41);
  bb42->Remove_succ(bb43, pool);
  bb61->Remove_pred(bb60, pool);

  bb_tmp = bb42->Prev();
  bb_tmp->Set_next(bb43);
  bb43->Set_prev(bb_tmp);
  bb44->Set_next(bb61);
  bb61->Set_prev(bb44);
  bb51->Set_prev(bb42);
  bb42->Set_next(bb51);

  IDTYPE edge;

  if (cfg->Feedback()) {
    // Invalidate edge freq for edges connected to the removed path.
    sc_tmp1 = sc2_p;
    while (sc_tmp1 != sc1) {
      sc_tmp2 = sc_tmp1->Get_real_parent();
      edge = cfg->Feedback()->Get_edge(sc_tmp2->Get_bb_rep()->Id(),
				       sc_tmp1->Get_bb_rep()->Id());
      if (edge != IDTYPE_NULL)
	cfg->Feedback()->Change_edge_freq(edge, FB_FREQ_UNKNOWN );

      edge = cfg->Feedback()->Get_edge(sc_tmp1->Merge()->Id(),
				       sc_tmp2->Merge()->Id());
      if (edge != IDTYPE_NULL)
	cfg->Feedback()->Change_edge_freq(edge, FB_FREQ_UNKNOWN );
      sc_tmp1 = sc_tmp2;
    }

    edge_type = cfg->Feedback()->Get_edge_type(bb41->Id(), bb42->Id());
    cfg->Feedback()->Delete_edge(bb41->Id(), bb42->Id());
    freq = cfg->Feedback()->Get_edge_freq(bb42->Id(), bb43->Id());
    cfg->Feedback()->Delete_edge(bb42->Id(), bb43->Id());
    cfg->Feedback()->Add_edge(bb41->Id(), bb43->Id(), edge_type, freq);
    edge = cfg->Feedback()->Get_edge(bb42->Id(), bb51->Id());
    cfg->Feedback()->Set_edge_type(edge, ft_edge_type);

    edge = cfg->Feedback()->Get_edge(bb61->Id(), bb63->Id());
    
    if (edge != IDTYPE_NULL)
      cfg->Feedback()->Change_edge_freq(edge, freq);
  }

  // Link in removed path. Replace condition expressions in BB42 with new_cond.
  // 
  //     5                           5
  //     |                           |
  //     39                         42
  //   /    \                     /     \
  //  41     40                 39       51
  // /  \    |	              /   \      |
  // 62   43 |               41   40     60
  // |    |  |              /  \   |     |
  // |    44 |  ==>       62   43  |     |
  // |    |  |            |    |   |     |
  // |   61  |            |    44  |     |
  // \    /  |            |    |   |     |
  //   63    |            |    61  |     |
  //    \   /             \    /   |     |
  //     64                 63     |     |
  // \   |                    \   /      |
  //   65                       new      |
  //                             \      /
  //                                64
  //                             \   |
  //                                65

  BB_LIST_ITER bb_list_iter;

  // All nested if-condition should have single predecessor, otherwise there must
  // exist intervening statements that would have disqualified it from being a candidate.
  FmtAssert((bb42->Pred()->Len() == 1), ("Expect single predecessor"));

  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb39->Pred())) {
    bb_tmp->Replace_succ(bb39, bb42);
    if (cfg->Feedback()) {
      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb39->Id(), bb42->Id());
    }
  }
  
  bb42->Set_pred(bb39->Pred());
  bb42->Prepend_succ(bb39, pool);
  
  BB_LIST * bb_list_new = CXX_NEW(BB_LIST(bb42), pool);
  bb39->Set_pred(bb_list_new);
  
  BB_NODE * bb_new = cfg->Create_and_allocate_bb(bb64->Kind());

  FmtAssert(bb60->Succ()->Len() == 1, ("Expect single succ"));
  bb60->Replace_succ(bb61, bb64);

  if (cfg->Feedback()) {
    freq = cfg->Feedback()->Get_edge_freq(bb42->Id(), bb51->Id());
    cfg->Feedback()->Add_node(bb_new->Id());
    cfg->Feedback()->Add_edge(bb42->Id(), bb39->Id(), br_edge_type, FB_FREQ_UNKNOWN);
    cfg->Feedback()->Move_edge_dest(bb60->Id(), bb61->Id(), bb64->Id());
    cfg->Feedback()->Add_edge(bb_new->Id(), bb64->Id(), FB_EDGE_OUTGOING, FB_FREQ_UNKNOWN);
  }
  
  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb64->Pred())) {
    bb_tmp->Replace_succ(bb64, bb_new);
    if (cfg->Feedback()) 
      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb64->Id(), bb_new->Id());
  }

  bb_new->Set_pred(bb64->Pred());
  bb_list_new = CXX_NEW(BB_LIST(bb64), pool);
  bb_new->Set_succ(bb_list_new);

  bb_list_new = CXX_NEW(BB_LIST(bb_new), pool);
  bb64->Set_pred(bb_list_new);
  bb64->Append_pred(bb60, pool);

  // Make bb42 jumps to bb39.
  branch_wn = bb42->Branch_wn();
  cfg->Add_label_with_wn(bb39);
  WN_label_number(branch_wn) = bb39->Labnam();
  WN_kid0(branch_wn) = new_cond;

  bb_tmp = bb39->Prev();
  bb_tmp->Set_next(bb42);
  bb42->Set_prev(bb_tmp);
  bb60->Set_next(bb39);
  bb39->Set_prev(bb60);
  bb_tmp = bb64->Prev();
  bb_new->Set_prev(bb_tmp);
  bb_tmp->Set_next(bb_new);
  bb64->Set_prev(bb_new);
  bb_new->Set_next(bb64);
  
  info = bb42->Ifinfo();
  info->Set_then(bb51);
  info->Set_else(bb39);
  info->Set_merge(bb64);

  info = bb39->Ifinfo();
  info->Set_merge(bb_new);

  // Update SC tree
  // Last successor is SC_ELSE, sc76 is sc2.
  //
  //        72                     72
  //      /    \                  /  \             73
  //     73      107             75  107         /   \
  //    /  \          =>                        74   76
  //   74  76                                       / | \
  //   |  / | \
  //  75

  SC_NODE * sc73 = sc2_p;
  SC_NODE * sc74 = sc2_s;
  SC_NODE * sc76 = sc2;
  SC_NODE * sc72 = sc73->Parent();
  
  sc72->Set_kids(sc74->Kids());
  SC_LIST_ITER sc_list_iter;
  FOR_ALL_ELEM(sc_tmp1, sc_list_iter, Init(sc72->Kids())) {
    sc_tmp1->Set_parent(sc72);
  }

  //    73              73
  //   /  \            /   \
  //  74  76     =>   76   74
  //     / | \       / | \

  if (sc76->Type() == SC_ELSE) {
    sc76->Set_type(SC_THEN);
    sc74->Set_type(SC_ELSE);
    sc74->Unlink();
    sc73->Append_kid(sc74);
    sc74->Set_parent(sc73);
  }

  // 67 is sc1
  //    9                 9
  //  /   \                \
  //      66   =>          66
  //      /  \            /  \
  //     67  111         73   NEW
  //                    /   \
  //                   76    74
  //                  /|\   /  \
  //                       67  111

  SC_NODE * sc67 = sc1;
  SC_NODE * sc66 = sc67->Parent();
  SC_NODE * sc111 = sc67->Next_sibling();
  SC_NODE * sc_new = cfg->Create_sc(SC_BLOCK);

  sc111->Set_bbs(NULL);
  sc111->Append_bbs(bb_new);
  sc_new->Append_bbs(bb64);

  sc74->Set_kids(sc66->Kids());
  FOR_ALL_ELEM(sc_tmp1, sc_list_iter, Init(sc74->Kids())) {
    sc_tmp1->Set_parent(sc74);
  }

  sc66->Set_kids(NULL);
  sc66->Append_kid(sc73);
  sc66->Append_kid(sc_new);
  sc_new->Set_parent(sc66);
  sc73->Set_parent(sc66);

  cfg->Fix_info(sc1_p);
  if (sc1_prev)
    cfg->Fix_info(sc1_prev);
  
  if (cfg->Feedback()) 
    cfg->Freq_propagate(sc66);
  
  cfg->Invalidate_and_update_aux_info(FALSE);
  cfg->Invalidate_loops();
  Inc_transform_count();

  return TRUE;
}

// Invalidate invariant maps for the given BB_NODE.
void
CFG_TRANS::Invalidate_invar(BB_NODE * bb)
{
  std::map<IDTYPE, SC_NODE *> & invar_map = Get_invar_map();
  invar_map[bb->Id()] = (SC_NODE *) NULL;
}

// Invalidate invariant maps for all BB_NODEs in the given sc.
void
CFG_TRANS::Invalidate_invar(SC_NODE * sc)
{
  BB_NODE * bb = sc->Get_bb_rep();

  if (bb != NULL)
    Invalidate_invar(bb);

  BB_LIST * bb_list = sc->Get_bbs();

  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;

    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      Invalidate_invar(tmp);
    }
  }

  SC_LIST * kids = sc->Kids();

  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE * tmp;

    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      Invalidate_invar(tmp);
    }
  }
}

// Do loop unswitching.
// 'Do_partial' indicates whether to do partial loop unswitching.
// An example of partial loop unswitching is shown below, where expression 'a' is not modified in the
// then-path of the top level if-condition.
//
// From:
//  for (...) {
//    if (a) {
//      block 1;
//    }
//    else {
//      a = ...;
//      block 2;
//    }
//  }
//
// To:
//   if (a) {
//     for (...) {
//       block 1;
//     }
//   }
//   else {
//     for (...) {
//       if (a) {
//         block 1;
//       }
//       else {
//         a = ...;
//         block 2;
//       }
//     }
//   }
//  
//  Caller of this routine must canonicalize the if-condition first.
SC_NODE *
CFG_TRANS::Do_loop_unswitching(SC_NODE * sc1, SC_NODE * sc2, BOOL do_partial)
{
  FmtAssert(((sc1->Type() == SC_IF) && (sc2->Type() == SC_LOOP)), ("Unexpect SC type"));

  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();
  SC_NODE * sc_tmp = sc1->Next_sibling();
  SC_NODE * sc2_prev = sc2->Prev_sibling();

  // Only do it if the if-region and its merge are the only children
  // of the loop, and the merge is an empty block.
  if (sc1->Prev_sibling() || sc_tmp->Next_sibling()
      || !sc_tmp->Is_empty_block() || !sc2->Is_sese())
    return NULL;
  
  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return NULL;

  if (_trace)
    printf("\n\t\t Loop unswitching (SC%d,SC%d)\n", 
	   sc1->Id(), sc2->Id());

  // Create a new SC_IF and let it become the new merge for sc2.
  //
  //      \   /              \       /
  //     bb_merge    ==>     bb_head_new
  //      /  \                /     \
  //                        bb_e2   bb_e1
  //                          \     /
  //                          bb_merge
  //                          /     \
  
  BB_NODE * bb_head = sc1->Head();
  BB_NODE * bb_then = sc1->Then();
  BB_NODE * bb_else = sc1->Else();
  BB_NODE * bb_merge = sc2->Merge();
  BB_NODE * bb_head_new = NULL;

  cfg->Clone_bbs(bb_head, bb_head, &bb_head_new, &bb_head_new, TRUE, 1.0);

  BB_NODE * bb_e1 = cfg->Create_and_allocate_bb(BB_GOTO);
  BB_NODE * bb_e2 = cfg->Create_and_allocate_bb(BB_GOTO);
  SC_NODE * sc_if = cfg->Create_sc(SC_IF);
  SC_NODE * sc_then = cfg->Create_sc(SC_THEN);
  SC_NODE * sc_else = cfg->Create_sc(SC_ELSE);
  SC_NODE * sc_e1 = cfg->Create_sc(SC_BLOCK);
  SC_NODE * sc_e2 = cfg->Create_sc(SC_BLOCK);

  sc_if->Set_bb_rep(bb_head_new);
  sc_e1->Append_bbs(bb_e1);
  sc_e2->Append_bbs(bb_e2);

  BB_IFINFO * if_info = bb_head_new->Ifinfo();
  if_info->Set_merge(bb_merge);
  if_info->Set_then(bb_e1);
  if_info->Set_else(bb_e2);

  if (cfg->Feedback()) {
    cfg->Feedback()->Add_node(bb_head_new->Id());
    cfg->Feedback()->Add_node(bb_e1->Id());
    cfg->Feedback()->Add_node(bb_e2->Id());
  }

  BB_LIST_ITER bb_list_iter;
  BB_LIST * bb_list_tmp;
  BB_NODE * bb_tmp;
  WN * branch_wn;
  FB_FREQ freq;
  IDTYPE edge;
  float scale = 1.0;

  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_merge->Pred())) {
    bb_tmp->Replace_succ(bb_merge, bb_head_new);
    if (bb_tmp->Is_branch_to(bb_merge)) {
      branch_wn = bb_tmp->Branch_wn();
      cfg->Add_label_with_wn(bb_head_new);
      WN_label_number(branch_wn) = bb_head_new->Labnam();
    }

    if (cfg->Feedback()) 
      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_merge->Id(), bb_head_new->Id());
  }

  bb_head_new->Set_pred(bb_merge->Pred());
  bb_list_tmp = CXX_NEW(BB_LIST(bb_e2), pool);
  bb_head_new->Set_succ(bb_list_tmp);
  bb_head_new->Append_succ(bb_e1, pool);
  bb_list_tmp = CXX_NEW(BB_LIST(bb_head_new), pool);
  bb_e1->Set_pred(bb_list_tmp);
  bb_list_tmp = CXX_NEW(BB_LIST(bb_head_new), pool);
  bb_e2->Set_pred(bb_list_tmp);
  bb_list_tmp = CXX_NEW(BB_LIST(bb_merge), pool);
  bb_e1->Set_succ(bb_list_tmp);
  bb_list_tmp = CXX_NEW(BB_LIST(bb_merge), pool);
  bb_e2->Set_succ(bb_list_tmp);
  bb_list_tmp = CXX_NEW(BB_LIST(bb_e2), pool);
  bb_merge->Set_pred(bb_list_tmp);
  bb_merge->Append_pred(bb_e1, pool);
  
  if (cfg->Feedback()) {
    freq = cfg->Feedback()->Get_node_freq_out(bb_merge->Id());
    FB_FREQ freq1 = cfg->Feedback()->Get_edge_freq(bb_head->Id(), bb_then->Id());
    FB_FREQ freq2 = cfg->Feedback()->Get_edge_freq(bb_head->Id(), bb_else->Id());
    freq1 = freq1/(freq1 + freq2) * freq;
    FB_EDGE_TYPE edge_type = cfg->Feedback()->Get_edge_type(bb_head->Id(), bb_then->Id());
    cfg->Feedback()->Add_edge(bb_head_new->Id(), bb_e1->Id(), edge_type, freq1);
    cfg->Feedback()->Add_edge(bb_e1->Id(), bb_merge->Id(), FB_EDGE_OUTGOING, freq1);
    edge_type = cfg->Feedback()->Get_edge_type(bb_head->Id(), bb_else->Id());
    cfg->Feedback()->Add_edge(bb_head_new->Id(), bb_e2->Id(), edge_type, freq - freq1);
    cfg->Feedback()->Add_edge(bb_e2->Id(), bb_merge->Id(), FB_EDGE_OUTGOING, freq - freq1);
  }
  
  branch_wn = bb_head_new->Branch_wn();
  cfg->Add_label_with_wn(bb_e2);
  WN_label_number(branch_wn) = bb_e2->Labnam();
  
  bb_tmp = bb_merge->Prev();
  bb_tmp->Set_next(bb_head_new);
  bb_head_new->Set_prev(bb_tmp);
  bb_head_new->Set_next(bb_e1);
  bb_e1->Set_prev(bb_head_new);
  bb_e1->Set_next(bb_e2);
  bb_e2->Set_prev(bb_e1);
  bb_e2->Set_next(bb_merge);
  bb_merge->Set_prev(bb_e2);

  sc_then->Set_parent(sc_if);
  sc_else->Set_parent(sc_if);
  sc_if->Append_kid(sc_then);
  sc_if->Append_kid(sc_else);
  sc_e1->Set_parent(sc_then);
  sc_then->Append_kid(sc_e1);
  sc_e2->Set_parent(sc_else);
  sc_else->Append_kid(sc_e2);

  sc2->Loopinfo()->Set_merge(bb_head_new);
  sc2->Insert_after(sc_if);
  SC_NODE *sc_real_parent = sc2->Get_real_parent();
  cfg->Fix_info(sc_real_parent);
  cfg->Invalidate_and_update_aux_info(FALSE);
  cfg->Invalidate_loops();

  // Do head duplication of sc2 into sc_if.
  Do_head_duplication(sc2, sc_if);
  SC_NODE * sc_body;
  SC_NODE * sc_merge;
  SC_NODE * sc_next;
  SC_NODE * sc_prune;
  SC_LIST_ITER sc_list_iter;
  INT end = do_partial ? 1 : 2;

  for (int i = 0; i < end; i ++) {
    SC_NODE *sc_body1;
    BB_NODE *bb_pred;

    sc_tmp = sc_if->Find_kid_of_type((i == 0) ? SC_THEN : SC_ELSE);
    sc_tmp = sc_tmp->First_kid();
    sc_body = sc_tmp->Find_kid_of_type(SC_LP_BODY);
    sc_prune = sc_body->First_kid();
    FmtAssert((sc_prune->Type() == SC_IF), ("Expect a SC_IF"));
    sc_merge = sc_prune->Next_sibling();
    bb_head = sc_prune->Head();
    bb_merge = sc_prune->Merge();

    BB_NODE * bb_first = (i == 0) ? sc_prune->Then() : sc_prune->Else();
    BB_NODE * bb_last = (i == 0) ? sc_prune->Then_end() : sc_prune->Else_end();

    if (cfg->Feedback()) {
      freq = cfg->Feedback()->Get_node_freq_in(bb_head->Id());
      FB_FREQ freq1 = cfg->Feedback()->Get_edge_freq(bb_head->Id(), bb_first->Id());
      scale = 0;
      if ((freq > FB_FREQ_ZERO) && (freq1 > FB_FREQ_ZERO))
        scale = freq.Value() / freq1.Value();
      if (i == 0)
	cfg->Freq_scale(sc_prune->Find_kid_of_type(SC_THEN), scale);
      else
	cfg->Freq_scale(sc_prune->Find_kid_of_type(SC_ELSE), scale);
    }

    // Remove else/then path.
    sc_next = sc_prune->Find_kid_of_type((i == 0) ? SC_THEN : SC_ELSE);
    sc_else = sc_prune->Find_kid_of_type((i == 0)? SC_ELSE:SC_THEN);
    sc_prune->Unlink();
    Invalidate_invar(sc_prune);
    sc_merge->Unlink();

    sc_body->Set_kids(sc_next->Kids());
    sc_next->Set_kids(NULL);

    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(sc_body->Kids())) {
      sc_tmp->Set_parent(sc_body);
    }
    
    sc_body->Append_kid(sc_merge);
    sc_merge->Set_parent(sc_body);
    sc_prune->Delete();

    // Remove if-condition.
    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_head->Succ())) {
     if (cfg->Feedback()) 
       cfg->Feedback()->Delete_edge(bb_head->Id(), bb_tmp->Id());
    }

    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_head->Pred())) {
      bb_tmp->Replace_succ(bb_head, bb_first);

      if (bb_tmp->Is_branch_to(bb_head)) {
	branch_wn = bb_tmp->Branch_wn();
	cfg->Add_label_with_wn(bb_first);
	WN_label_number(branch_wn) = bb_first->Labnam();
      }

      if (cfg->Feedback()) 
	cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_head->Id(), bb_first->Id());
    }
  
    bb_first->Set_pred(bb_head->Pred());
  
    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_merge->Pred())) {
      if (bb_tmp != bb_last) {
	bb_merge->Remove_pred(bb_tmp, pool);

	if (cfg->Feedback()) 
	  cfg->Feedback()->Delete_edge(bb_tmp->Id(), bb_merge->Id());
	break;
      }
    }

    // remove branch in bb_last.
    if (bb_last->Is_branch_to(bb_merge)) 
      Delete_branch(bb_last);
    
    bb_tmp = bb_head->Prev();
    bb_tmp->Set_next(bb_first);
    bb_first->Set_prev(bb_tmp);
    bb_last->Set_next(bb_merge);
    bb_merge->Set_prev(bb_last);

    cfg->Fix_info(sc_body->Parent());
  }    

  if (sc2_prev){
    cfg->Fix_info(sc2_prev);
  }  
  
  cfg->Invalidate_and_update_aux_info(FALSE);
  cfg->Invalidate_loops();
  Inc_transform_count();

  return sc_if;
}

// Swap executable statements for the given pair of BB_NODE, keep labels unchanged.
void
PRO_LOOP_INTERCHANGE_TRANS::Swap_stmt(BB_NODE * bb1, BB_NODE * bb2)
{
  STMT_ITER stmt_iter;
  WN * wn_iter;
  WN * wn_insert1 = NULL;
  WN * wn_insert2 = NULL;
  
  FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb1->Firststmt(), bb1->Laststmt())) {
    if (WN_is_executable(wn_iter))
      break;
    else
      wn_insert1 = wn_iter;
  }

  FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb2->Firststmt(), bb2->Laststmt())) {
    if (WN_is_executable(wn_iter))
      break;
    else
      wn_insert2 = wn_iter;
  }

  WN * wn_begin1 = wn_insert1 ? WN_next(wn_insert1) : bb1->Firststmt();
  WN * wn_begin2 = wn_insert2 ? WN_next(wn_insert2) : bb2->Firststmt();
  WN * wn_end1 = bb1->Laststmt();
  WN * wn_end2 = bb2->Laststmt();
  WN * wn_branch1 = bb1->Branch_wn();
  WN * wn_branch2 = bb2->Branch_wn();
  INT32 label1;
  INT32 label2;

  if (wn_branch1)
    label1 = WN_label_number(wn_branch1);

  if (wn_branch2)
    label2 = WN_label_number(wn_branch2);

  if (wn_insert1) {
    WN_next(wn_insert1) = wn_begin2;
    WN_prev(wn_begin2) = wn_insert1;
    bb1->Set_laststmt(wn_end2);
  }
  else {
    bb1->Set_firststmt(wn_begin2);
    bb1->Set_laststmt(wn_end2);
  }

  if (wn_insert2) {
    WN_next(wn_insert2) = wn_begin1;
    WN_prev(wn_begin1) = wn_insert2;
    bb2->Set_laststmt(wn_end1);
  }
  else {
    bb2->Set_firststmt(wn_begin1);
    bb2->Set_laststmt(wn_end1);
  }

  wn_branch1 = bb1->Branch_wn();
  wn_branch2 = bb2->Branch_wn();

  if (wn_branch1)
    WN_label_number(wn_branch1) = label1;
  
  if (wn_branch2)
    WN_label_number(wn_branch2) = label2;
}

// Do loop interchange of the given loop nest.  Caller of this routine should check legality.
BOOL
PRO_LOOP_INTERCHANGE_TRANS::Do_loop_interchange(SC_NODE * sc_outer, SC_NODE * sc_inner)
{
  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < 3; j++) {
      SC_NODE * sc_tmp1 = NULL;
      SC_NODE * sc_tmp2 = NULL;
      BB_NODE * bb_tmp1 = NULL;
      BB_NODE * bb_tmp2 = NULL;

      switch (j) {
      case 0:
	sc_tmp1 = sc_outer->Find_kid_of_type(SC_LP_START);
	sc_tmp2 = sc_inner->Find_kid_of_type(SC_LP_START);
	break;
      case 1:
	sc_tmp1 = sc_outer->Find_kid_of_type(SC_LP_COND);
	sc_tmp2 = sc_inner->Find_kid_of_type(SC_LP_COND);
	break;
      case 2:
	sc_tmp1 = sc_outer->Find_kid_of_type(SC_LP_STEP);      
	sc_tmp2 = sc_inner->Find_kid_of_type(SC_LP_STEP);
	break;
      default:
	;
      }
      
      bb_tmp1 = sc_tmp1->Last_bb();
      bb_tmp2 = sc_tmp2->Last_bb();

      if (i == 0) {
	if ((sc_tmp1->First_bb() != bb_tmp1)
	    || (sc_tmp2->First_bb() != bb_tmp2))
	  return FALSE;

	if ((bb_tmp1->Executable_stmt_count() == 0)
	    || (bb_tmp2->Executable_stmt_count() == 0))
	  return FALSE;

	WN * wn1 = bb_tmp1->Branch_wn();
	WN * wn2 = bb_tmp2->Branch_wn();

	if (wn1 && wn2) {
	  if (WN_operator(wn1) != WN_operator(wn2))
	    return FALSE;
	}
      }
      else {
	if ((WOPT_Enable_Pro_Loop_Limit >= 0)
	    && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
	  return FALSE;
	
	if (j == 0) {
	  if (_trace)
	    printf("\n\t\t Loop interchange (SC%d,SC%d)\n", 
		   sc_outer->Id(), sc_inner->Id());
	}

	Swap_stmt(bb_tmp1, bb_tmp2);
      }
    }
  }

  CFG * cfg = _cu->Cfg();
  WN * wn_index1 = sc_outer->Index();
  WN * wn_index2 = sc_inner->Index();

  BB_LOOP * loop = sc_outer->Loopinfo();
  loop->Set_index(wn_index2);
  loop = sc_inner->Loopinfo();
  loop->Set_index(wn_index1);

  cfg->Fix_info(sc_outer);
  cfg->Fix_info(sc_inner);

  if (cfg->Feedback()) {
    SC_NODE * sc_body1 = sc_outer->Find_kid_of_type(SC_LP_BODY);
    SC_NODE * sc_body2 = sc_inner->Find_kid_of_type(SC_LP_BODY);
    BB_NODE * bb_cond1 = sc_outer->Find_kid_of_type(SC_LP_COND)->Last_bb();    
    BB_NODE * bb_body1 = sc_body1->First_bb();
    BB_NODE * bb_step1 = sc_outer->Find_kid_of_type(SC_LP_STEP)->Last_bb();    
    BB_NODE * bb_cond2 = sc_inner->Find_kid_of_type(SC_LP_COND)->Last_bb();
    BB_NODE * bb_body2 = sc_body2->First_bb();
    BB_NODE * bb_step2 = sc_inner->Find_kid_of_type(SC_LP_STEP)->Last_bb();
    FB_FREQ freq1 = cfg->Feedback()->Get_edge_freq(bb_cond1->Id(), bb_body1->Id());
    FB_FREQ freq2 = cfg->Feedback()->Get_edge_freq(bb_cond2->Id(), bb_body2->Id());
    float scale;
    IDTYPE edge;

    if ((freq1 > FB_FREQ_ZERO) && (freq2 > FB_FREQ_ZERO)) {
      scale = freq2.Value()/freq1.Value();
      cfg->Freq_scale(sc_body1, scale);
    }

    if ((freq1 > FB_FREQ_ZERO) && (freq2 > FB_FREQ_ZERO)) {
      scale = freq1.Value()/freq2.Value();
      cfg->Freq_scale(sc_body2, scale);
    }
    
    edge = cfg->Feedback()->Get_edge(bb_cond1->Id(), bb_body1->Id());
    if (edge)
      cfg->Feedback()->Change_edge_freq(edge, freq2);

    edge = cfg->Feedback()->Get_edge(bb_step1->Id(), bb_cond1->Id());
    if (edge)
      cfg->Feedback()->Change_edge_freq(edge, freq2);

    edge = cfg->Feedback()->Get_edge(bb_cond2->Id(), bb_body2->Id());
    if (edge)
      cfg->Feedback()->Change_edge_freq(edge, freq1);
    
    edge = cfg->Feedback()->Get_edge(bb_step2->Id(), bb_cond2->Id());
    if (edge)
      cfg->Feedback()->Change_edge_freq(edge, freq1);
  }

  Inc_transform_count();
  return TRUE;
}

// Do loop distributition for the given loop.
// Return the last loop after loop distribution.
// See PRO_LOOP_INTERCHANGE::Find_dist_cand for rules to find the distribution point.
// "do_interchange" indicates whether the purpose of loop distribution 
// is to enable loop interchange. "sc_dist" gives the distribution point. A NULL
//"sc_dist" will trigger the routine to find a distribution point.
SC_NODE *
PRO_LOOP_INTERCHANGE_TRANS::Do_loop_dist(SC_NODE * sc_loop, BOOL do_interchange, SC_NODE * sc_dist)
{
  FmtAssert((sc_loop->Type() == SC_LOOP), ("Expect a SC_LOOP"));

  if (!sc_loop->Is_sese()) 
    return NULL;

  SC_NODE * sc_parent = sc_loop->Find_kid_of_type(SC_LP_BODY);
  SC_NODE * inner_loop = sc_parent->Find_kid_of_type(SC_LOOP);
  SC_NODE * sc_par = sc_dist;

  if (!sc_par) {
    if (!inner_loop)
      return NULL;

    sc_par = Find_dist_cand(sc_parent);
    
    if (sc_par == NULL) {
      Do_pre_dist(inner_loop, sc_loop);
      sc_par = Find_dist_cand(sc_parent);
    }
  }

  if (sc_par == NULL)
    return NULL;

  SC_NODE * sc_tmp = sc_parent->First_kid();
  BOOL is_empty = TRUE;

  while (sc_tmp != sc_par) {
    if (!sc_tmp->Is_empty_block()) {
      is_empty = FALSE;
      break;
    }
    sc_tmp = sc_tmp->Next_sibling();
  }

  if (is_empty)
    return NULL;

  if (sc_par->Type() == SC_LOOP) {
    sc_tmp = sc_par->Next_sibling();
    while (sc_tmp) {
      if ((sc_tmp->Type() == SC_LOOP)
	  && !sc_tmp->Has_same_loop_struct(sc_par))
	return NULL;
      sc_tmp = sc_tmp->Next_sibling();
    }

    if (do_interchange) {
      if ((sc_par->Type() != SC_LOOP)
	  || !Can_interchange(sc_loop, sc_par))
	return NULL;
    }
  }

  SC_NODE * sc_last = sc_parent->Last_kid();
  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();

  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return NULL;

  if (!sc_last->Is_empty_block())
    return NULL;

  BB_NODE * bb_first = sc_par->First_bb();
  BB_NODE * bb_last = sc_last->Last_bb();

  if (bb_first->Pred()->Len() != 1)
    return NULL;

  BB_NODE * bb_pred = bb_first->Pred()->Node();

  if (bb_pred->Succ()->Len() != 1)
    return NULL;

  BB_NODE * bb_succ = bb_last->Succ()->Node();

  if (bb_succ->Pred()->Len() != 1)
    return NULL;

  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return NULL;
  
  if (_trace)
    printf("\n\t\t Loop distribution (SC%d)\n", sc_loop->Id());

  FB_EDGE_TYPE edge_type;

  if (cfg->Feedback()) 
    edge_type = cfg->Feedback()->Get_edge_type(bb_last->Id(), bb_succ->Id());
  
  // Unlink blocks in (bb_pred, bb_succ) from CFG and SC tree.
  bb_pred->Replace_succ(bb_first, bb_succ);
  bb_succ->Replace_pred(bb_last, bb_pred);

  bb_pred->Set_next(bb_succ);
  bb_succ->Set_prev(bb_pred);

  if (cfg->Feedback()) {
    cfg->Feedback()->Move_edge_dest(bb_pred->Id(), bb_first->Id(), bb_succ->Id());
    cfg->Feedback()->Delete_edge(bb_last->Id(), bb_succ->Id());
  }
  
  SC_NODE * sc_tmp1 = sc_last;
  SC_NODE * sc_tmp2;
  STACK<SC_NODE *> * sc_stack = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);

  while (sc_tmp1) {
    sc_tmp2 = sc_tmp1;
    sc_tmp1 = sc_tmp1->Prev_sibling();
    sc_stack->Push(sc_tmp2);
    sc_tmp2->Unlink();
    
    if (sc_tmp2 == sc_par)
      break;
  }

  // Clone sc_loop and insert before it.
  SC_NODE * sc_new = cfg->Clone_sc(sc_loop, TRUE, 1.0, NULL);
  BB_NODE * bb_lp_head = sc_loop->Head();
  BB_NODE * bb_lp_end = bb_lp_head->Loopend();
  BB_NODE * bb_lp_merge = sc_loop->Merge();
  BB_NODE * bb_lp_head_new = sc_new->Head();
  BB_NODE * bb_lp_end_new = bb_lp_head_new->Loopend();
  BB_NODE * bb_lp_step_new = bb_lp_head_new->Loopstep();
  BB_NODE * bb_tmp;
  BB_LIST_ITER bb_list_iter;
  BB_LIST * bb_list;

  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_lp_head->Pred())) {
    bb_tmp->Replace_succ(bb_lp_head, bb_lp_head_new);

    if (bb_tmp->Is_branch_to(bb_lp_head)) {
      WN * branch_wn = bb_tmp->Branch_wn();
      cfg->Add_label_with_wn(bb_lp_head_new);
      WN_label_number(branch_wn) = bb_lp_head_new->Labnam();
    }

    if (cfg->Feedback())
      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_lp_head->Id(), bb_lp_head_new->Id());
  }

  bb_lp_head_new->Set_pred(bb_lp_head->Pred());
  bb_lp_end_new->Prepend_succ(bb_lp_head, pool);
  WN * branch_wn = bb_lp_end_new->Branch_wn();
  cfg->Add_label_with_wn(bb_lp_head);
  WN_label_number(branch_wn) = bb_lp_head->Labnam();
  bb_list = CXX_NEW(BB_LIST(bb_lp_end_new), pool);
  bb_lp_head->Set_pred(bb_list);

  if (cfg->Feedback()) {
    FB_EDGE_TYPE edge_type = cfg->Feedback()->Get_edge_type(bb_lp_end->Id(), bb_lp_merge->Id());
    cfg->Feedback()->Add_edge(bb_lp_end_new->Id(), bb_lp_head->Id(), edge_type,
			      cfg->Feedback()->Get_node_freq_in(bb_lp_head_new->Id()));
  }
  
  bb_tmp = bb_lp_head->Prev();
  bb_tmp->Set_next(bb_lp_head_new);
  bb_lp_head_new->Set_prev(bb_tmp);
  bb_lp_step_new->Set_next(bb_lp_head);
  bb_lp_head->Set_prev(bb_lp_step_new);

  SC_NODE * sc_prev = sc_loop->Prev_sibling();
  sc_loop->Insert_before(sc_new);

  cfg->Fix_info(sc_new);
  cfg->Fix_info(sc_new->Get_real_parent());

  // Recover unlinked blocks in (bb_pred, bb_succ)
  bb_pred->Replace_succ(bb_succ, bb_first);
  bb_succ->Replace_pred(bb_pred, bb_last);

  if (cfg->Feedback()) {
    cfg->Feedback()->Move_edge_dest(bb_pred->Id(), bb_succ->Id(), bb_first->Id());
    cfg->Feedback()->Add_edge(bb_last->Id(), bb_succ->Id(), edge_type,
			      cfg->Feedback()->Get_node_freq_in(bb_first->Id()));
  }

  bb_pred->Set_next(bb_first);
  bb_succ->Set_prev(bb_last);

  sc_tmp2 = sc_parent->Last_kid();

  while (!sc_stack->Is_Empty()) {
    sc_tmp1 = sc_stack->Pop();
    sc_tmp2->Insert_after(sc_tmp1);
    sc_tmp2 = sc_tmp1;
  }

  bb_first = sc_parent->First_bb();
  bb_last = sc_par->Prev_sibling()->Last_bb();
  bb_succ = sc_par->First_bb();

  // Unlink blocks in [bb_first, bb_last] from CFG and SC tree.
  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_first->Pred())) {
    bb_tmp->Replace_succ(bb_first, bb_succ);
    if (bb_tmp->Is_branch_to(bb_first)) {
      WN * branch_wn = bb_tmp->Branch_wn();
      cfg->Add_label_with_wn(bb_succ);
      WN_label_number(branch_wn) = bb_succ->Labnam();
    }
  
    if (cfg->Feedback())
      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_first->Id(), bb_succ->Id());
  }

  bb_succ->Set_pred(bb_first->Pred());

  if (cfg->Feedback())
    cfg->Feedback()->Delete_edge(bb_last->Id(), bb_succ->Id());

  bb_tmp = bb_first->Prev();
  bb_tmp->Set_next(bb_succ);
  bb_succ->Set_prev(bb_tmp);
  
  sc_tmp1 = sc_parent->First_kid();
  while (sc_tmp1 && (sc_tmp1 != sc_par)) {
    sc_tmp2 = sc_tmp1;
    sc_tmp1 = sc_tmp1->Next_sibling();
    sc_tmp2->Unlink();
    sc_tmp2->Delete();
  }

  cfg->Fix_info(sc_loop);

  if (sc_prev)
    cfg->Fix_info(sc_prev);

  CXX_DELETE(sc_stack, _pool);

  cfg->Invalidate_and_update_aux_info(FALSE);    
  cfg->Invalidate_loops();
  Inc_transform_count();
  return sc_loop;
}

// Duplicate a if-condition bb_cond and wrap it around the given SC_NODE *,
// "is_then" tells whether the wrapped region should show up in the then-path.
// Return the duplicated SC_IF node.
SC_NODE *
CFG_TRANS::Do_if_cond_wrap( BB_NODE * bb_cond,
			    SC_NODE * sc_begin,
			    BOOL is_then)
{
  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();

  SC_NODE * sc_p = sc_begin->Get_real_parent();
  SC_NODE * sc_prev = sc_begin->Prev_sibling();
  BB_NODE * bb_new = NULL;
  cfg->Clone_bbs(bb_cond, bb_cond, &bb_new, &bb_new, TRUE, 1.0);

  BB_NODE * bb_then = bb_cond->If_then();
  BB_NODE * bb_else = bb_cond->If_else();
  BB_NODE * bb_first = sc_begin->First_bb();
  BB_NODE * bb_last;

  if (sc_begin->Type() == SC_IF) {
    SC_NODE * sc_merge = sc_begin->Next_sibling();
    if (!sc_merge->Is_empty_block())
      sc_merge = cfg->Insert_block_before(sc_merge);
    bb_last = sc_merge->First_bb();
  }
  else 
    bb_last = sc_begin->Last_bb();

  BB_NODE * bb_e = cfg->Create_and_allocate_bb(BB_GOTO);
  BB_NODE * bb_merge = cfg->Create_and_allocate_bb(BB_GOTO);
  
  SC_NODE * sc_if = cfg->Create_sc(SC_IF);
  SC_NODE * sc_then = cfg->Create_sc(SC_THEN);
  SC_NODE * sc_else = cfg->Create_sc(SC_ELSE);
  SC_NODE * sc_merge = cfg->Create_sc(SC_BLOCK);
  SC_NODE * sc_e = cfg->Create_sc(SC_BLOCK);
  SC_LIST_ITER sc_list_iter;
  SC_NODE * sc_tmp;

  sc_if->Set_bb_rep(bb_new);
  sc_merge->Append_bbs(bb_merge);
  sc_e->Append_bbs(bb_e);
  
  //           |                         |
  //          bb_first               bb_new(IF)
  //        /    \                  /        \
  //      48     47  ==>          bb_first   |
  //        \   /                 /    \     bb_e
  //          bb_last            48    47    |
  //           |                  \    /     |
  //                              bb_last    |
  //                                 \      /
  //                                 bb_merge
  //                                    |
  BB_NODE * bb_tmp;
  BB_LIST_ITER  bb_list_iter;
  BB_LIST * bb_list_tmp;
  WN * branch_wn;
  BB_IFINFO * if_info = bb_new->Ifinfo();
  IDTYPE edge;
  FB_FREQ freq;
  FB_FREQ freq_then;
  FB_FREQ freq_else;
  FB_EDGE_TYPE ft_edge_type;
  FB_EDGE_TYPE br_edge_type;
  float scale;
  
  if_info->Set_cond(bb_new);
  if_info->Set_merge(bb_merge);

  if (cfg->Feedback()) {
    ft_edge_type = cfg->Feedback()->Get_edge_type(bb_cond->Id(), bb_then->Id());
    br_edge_type = cfg->Feedback()->Get_edge_type(bb_cond->Id(), bb_else->Id());
    cfg->Feedback()->Add_node(bb_new->Id());
    cfg->Feedback()->Add_node(bb_e->Id());
    cfg->Feedback()->Add_node(bb_merge->Id());
    freq_then = cfg->Feedback()->Get_edge_freq(bb_cond->Id(), bb_then->Id());
    freq_else = cfg->Feedback()->Get_edge_freq(bb_cond->Id(), bb_else->Id());
  }

  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_first->Pred())) {
    bb_tmp->Replace_succ(bb_first, bb_new);
    if (bb_tmp->Is_branch_to(bb_first)) {
      branch_wn = bb_tmp->Branch_wn();
      cfg->Add_label_with_wn(bb_new);
      WN_label_number(branch_wn) = bb_new->Labnam();
    }

    if (cfg->Feedback()) 
      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_first->Id(), bb_new->Id());
  }

  if (cfg->Feedback()) {
    freq = cfg->Feedback()->Get_node_freq_in(bb_new->Id());
    freq_then = freq * (freq_then / (freq_then + freq_else));
    freq_else = freq - freq_then;
  }

  bb_new->Set_pred(bb_first->Pred());
  bb_list_tmp = CXX_NEW(BB_LIST(bb_e), pool);
  bb_new->Set_succ(bb_list_tmp);

  bb_list_tmp = CXX_NEW(BB_LIST(bb_e), pool);
  bb_merge->Set_pred(bb_list_tmp);

  bb_tmp = bb_first->Prev();
  bb_tmp->Set_next(bb_new);
  bb_new->Set_prev(bb_tmp);
  bb_tmp = bb_last->Next();
  bb_tmp->Set_prev(bb_merge);
  bb_merge->Set_next(bb_tmp);

  if (is_then) {
    bb_new->Append_succ(bb_first, pool);
    bb_tmp = bb_e;
    bb_merge->Append_pred(bb_last, pool);
    bb_new->Set_next(bb_first);
    bb_first->Set_prev(bb_new);
    bb_last->Set_next(bb_e);
    bb_e->Set_prev(bb_last);
    bb_e->Set_next(bb_merge);
    bb_merge->Set_prev(bb_e);
    if_info->Set_then(bb_first);
    if_info->Set_else(bb_e);

    if (cfg->Feedback()) {
      cfg->Feedback()->Add_edge(bb_new->Id(), bb_first->Id(), ft_edge_type, freq_then);
      cfg->Feedback()->Add_edge(bb_new->Id(), bb_e->Id(), br_edge_type, freq_else);
      cfg->Feedback()->Add_edge(bb_e->Id(), bb_merge->Id(), FB_EDGE_OUTGOING, freq_else);
      freq = freq_then / freq;
      scale = freq.Value();
    }
  }
  else {
    bb_new->Prepend_succ(bb_first, pool);
    bb_tmp = bb_first;
    bb_merge->Prepend_pred(bb_last, pool);
    bb_new->Set_next(bb_e);
    bb_e->Set_prev(bb_new);
    bb_first->Set_prev(bb_e);
    bb_e->Set_next(bb_first);
    bb_last->Set_next(bb_merge);
    bb_merge->Set_prev(bb_last);
    if_info->Set_then(bb_e);
    if_info->Set_else(bb_first);

    if (cfg->Feedback()) {
      cfg->Feedback()->Add_edge(bb_new->Id(), bb_first->Id(), br_edge_type, freq_else);
      cfg->Feedback()->Add_edge(bb_new->Id(), bb_e->Id(), ft_edge_type, freq_then);
      cfg->Feedback()->Add_edge(bb_e->Id(), bb_merge->Id(), FB_EDGE_OUTGOING, freq_then);
      freq = freq_else / freq;
      scale = freq.Value();
    }
  }

  branch_wn = bb_new->Branch_wn();
  cfg->Add_label_with_wn(bb_tmp);
  WN_label_number(branch_wn) = bb_tmp->Labnam();
  
  bb_list_tmp = CXX_NEW(BB_LIST(bb_new), pool);
  bb_e->Set_pred(bb_list_tmp);

  bb_list_tmp = CXX_NEW(BB_LIST(bb_new), pool);
  bb_first->Set_pred(bb_list_tmp);

  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_last->Succ())) {
    bb_tmp->Replace_pred(bb_last, bb_merge);
  }
  
  bb_merge->Set_succ(bb_last->Succ());

  if (cfg->Feedback()) {
    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_last->Succ())) {
      freq = cfg->Feedback()->Get_edge_freq(bb_last->Id(), bb_tmp->Id());
      cfg->Feedback()->Delete_edge(bb_last->Id(), bb_tmp->Id());
      cfg->Feedback()->Add_edge(bb_merge->Id(), bb_tmp->Id(), FB_EDGE_OUTGOING, freq);
    }
    
    bb_last->Set_succ(NULL);
    cfg->Freq_scale(sc_begin, scale);

    if (is_then) 
      cfg->Feedback()->Add_edge(bb_last->Id(), bb_merge->Id(), FB_EDGE_OUTGOING, freq_then);
    else 
      cfg->Feedback()->Add_edge(bb_last->Id(), bb_merge->Id(), FB_EDGE_OUTGOING, freq_else);
  }
  
  bb_list_tmp = CXX_NEW(BB_LIST(bb_merge), pool);
  bb_last->Set_succ(bb_list_tmp);

  bb_list_tmp = CXX_NEW(BB_LIST(bb_merge), pool);
  bb_e->Set_succ(bb_list_tmp);

  SC_TYPE type = sc_begin->Type();
  if (type == SC_LP_BODY) {
    //                           |
    //                         LP_BODY
    //                        /     \
    //                      SC_IF   SC_MERGE
    //     |              /       \
    //    LP_BODY ==>   SC_THEN  SC_ELSE
    //   / | \                  /  |  \
    //                         

    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(sc_begin->Kids())) {
      if (is_then) 
	sc_tmp->Set_parent(sc_then);
      else
	sc_tmp->Set_parent(sc_else);
    }

    if (is_then) {
      sc_then->Set_kids(sc_begin->Kids());
      sc_else->Append_kid(sc_e);
      sc_e->Set_parent(sc_else);
    }
    else {
      sc_else->Set_kids(sc_begin->Kids());
      sc_then->Append_kid(sc_e);
      sc_e->Set_parent(sc_then);
    }
    sc_begin->Set_kids(NULL);
    sc_begin->Append_kid(sc_if);
    sc_if->Set_parent(sc_begin);
    sc_begin->Append_kid(sc_merge);
    sc_merge->Set_parent(sc_begin);
    sc_if->Append_kid(sc_then);
    sc_if->Append_kid(sc_else);
    sc_then->Set_parent(sc_if);
    sc_else->Set_parent(sc_if);
  }
  else if (type == SC_IF) {
    SC_NODE * sc_next = sc_begin->Next_sibling();
    sc_next->Insert_before(sc_if);
    sc_p->Remove_kid(sc_begin);
    sc_p->Remove_kid(sc_next);
    sc_if->Append_kid(sc_then);
    sc_if->Append_kid(sc_else);
    sc_then->Set_parent(sc_if);
    sc_else->Set_parent(sc_if);
    if (is_then) {
      sc_then->Append_kid(sc_begin);
      sc_then->Append_kid(sc_next);
      sc_begin->Set_parent(sc_then);
      sc_next->Set_parent(sc_then);
      sc_else->Append_kid(sc_e);
      sc_e->Set_parent(sc_else);
    } 
    else {
      sc_else->Append_kid(sc_begin);
      sc_else->Append_kid(sc_next);
      sc_begin->Set_parent(sc_else);
      sc_next->Set_parent(sc_else);
      sc_then->Append_kid(sc_e);
      sc_e->Set_parent(sc_then);
    }
    sc_if->Insert_after(sc_merge);
  }
  else if (type == SC_BLOCK) {
    SC_NODE * sc_parent = sc_begin->Parent();
    sc_parent->Remove_kid(sc_begin);
    sc_if->Append_kid(sc_then);
    sc_if->Append_kid(sc_else);
    sc_then->Set_parent(sc_if);
    sc_else->Set_parent(sc_if);

    if (is_then) {
      sc_then->Append_kid(sc_begin);
      sc_begin->Set_parent(sc_then);
      sc_else->Append_kid(sc_e);
      sc_e->Set_parent(sc_else);
    }
    else {
      sc_else->Append_kid(sc_begin);
      sc_begin->Set_parent(sc_else);
      sc_then->Append_kid(sc_e);
      sc_e->Set_parent(sc_then);
    }
    if (sc_prev)
      sc_prev->Insert_after(sc_if);
    else
      sc_parent->Append_kid(sc_if);
    
    sc_if->Insert_after(sc_merge);
    cfg->Fix_info(sc_parent);
  }
  else {
    FmtAssert(FALSE, ("TODO"));
  }
  
  if (sc_prev)
    cfg->Fix_info(sc_prev);
  cfg->Fix_info(sc_p);
  
  cfg->Invalidate_and_update_aux_info(FALSE);
  cfg->Invalidate_loops();
  Inc_transform_count();

  return sc_if;
}

// Given 'sc_if', remove the if-condition so that its then-path and else-path are unconditionally
// executed.
void
CFG_TRANS::Do_if_cond_unwrap(SC_NODE * sc_if)
{
  CFG * cfg = _cu->Cfg();
  
  SC_NODE * sc_then = sc_if->Find_kid_of_type(SC_THEN);
  SC_NODE * sc_else = sc_if->Find_kid_of_type(SC_ELSE);
  SC_NODE * sc_p = sc_if->Parent();
  SC_NODE * sc_prev = sc_if->Prev_sibling();
  SC_NODE * sc_merge = sc_if->Next_sibling();

  BB_NODE * bb_head = sc_if->Head();
  BB_NODE * bb_merge = sc_if->Merge();
  BB_NODE * bb_first = NULL;
  BB_NODE * bb_last = NULL;
  BB_NODE * bb_tmp;
  BB_LIST * bb_list_new;
  BB_LIST_ITER bb_list_iter;
  MEM_POOL * pool = cfg->Mem_pool();

  for (int i = 0; i < 2; i ++) {
    SC_NODE * sc_kid;

    if (i == 0) {
      sc_kid = sc_then;
      bb_first = sc_kid->First_bb();
      bb_last = sc_kid->Last_bb();
    }
    else {
      sc_kid = sc_else;
      BB_NODE * bb_cur = sc_kid->First_bb();
      if (cfg->Feedback()) {
	FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_cur->Pred())) {
	  cfg->Feedback()->Delete_edge(bb_tmp->Id(), bb_cur->Id());
	}

	FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_last->Succ())) {
	  cfg->Feedback()->Move_edge_dest(bb_last->Id(), bb_tmp->Id(), bb_first->Id());
	}
      }

      bb_last->Remove_succs(pool);
      bb_list_new = CXX_NEW(BB_LIST(bb_cur), pool);
      bb_last->Set_succ(bb_list_new);
      bb_cur->Remove_preds(pool);
      bb_list_new = CXX_NEW(BB_LIST(bb_last), pool);
      bb_cur->Set_pred(bb_list_new);
      bb_cur->Set_prev(bb_last);
      bb_last->Set_next(bb_cur);
      bb_last = sc_kid->Last_bb();
    }

    SC_NODE * sc_tmp = sc_kid->First_kid();
    while (sc_tmp) {
      SC_NODE * sc_next = sc_tmp->Next_sibling();
      sc_tmp->Unlink();
      sc_merge->Insert_before(sc_tmp);
      sc_tmp = sc_next;
    }
  }

  sc_if->Unlink();
  sc_if->Delete();
  
  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_head->Pred())) {
    if (bb_tmp->Is_branch_to(bb_head)) {
      WN * branch_wn = bb_tmp->Branch_wn();
      cfg->Add_label_with_wn(bb_first);
      WN_label_number(branch_wn) = bb_first->Labnam();
    }

    bb_tmp->Replace_succ(bb_head, bb_first);
    if (cfg->Feedback())
      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_head->Id(), bb_first->Id());
  }

  bb_first->Remove_preds(pool);
  bb_first->Set_pred(bb_head->Pred());
  bb_tmp = bb_head->Prev();
  bb_tmp->Set_next(bb_first);
  bb_first->Set_prev(bb_tmp);

  FmtAssert(bb_last && (bb_last->Succ()->Len() == 1),
	    ("Expect a single block."));

  if (cfg->Feedback()) {
    cfg->Feedback()->Delete_node(bb_head->Id());
    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_last->Succ())) {
      cfg->Feedback()->Move_edge_dest(bb_last->Id(), bb_tmp->Id(), bb_merge->Id());
    }

    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_merge->Pred())) {
      if (bb_tmp != bb_last) {
	cfg->Feedback()->Delete_edge(bb_tmp->Id(), bb_merge->Id());
      }
    }
  }

  bb_last->Remove_succs(pool);
  bb_list_new = CXX_NEW(BB_LIST(bb_merge), pool);
  bb_last->Set_succ(bb_list_new);
  bb_merge->Remove_preds(pool);
  bb_list_new = CXX_NEW(BB_LIST(bb_last), pool);
  bb_merge->Set_pred(bb_list_new);
  bb_last->Set_next(bb_merge);
  bb_merge->Set_prev(bb_last);
  Delete_branch(bb_head);

  if (sc_prev)
    cfg->Fix_info(sc_prev);

  cfg->Fix_info(sc_p);

  cfg->Invalidate_and_update_aux_info(FALSE);
  cfg->Invalidate_loops();
  Inc_transform_count();
}

// Find whether the given sc has a unique read/write reference, i.e.,
// - Has no scalar store.
// - Scalar loads are reference to loop invariants.
// - All non-scalar loads and stores have the same address.
//
// Return the address of the non-scalar load/store in *wn_addr.
// Return TRUE if the uniqueness is verified.
BOOL
CFG_TRANS::Get_unique_ref(SC_NODE * sc, SC_NODE * sc_loop, WN ** wn_addr)
{
  BB_NODE * bb = sc->Get_bb_rep();

  if (bb != NULL) {
    if (!Get_unique_ref(bb, sc_loop, wn_addr, sc))
      return FALSE;
  }

  BB_LIST * bb_list = sc->Get_bbs();

  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;
    
    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      if (!Get_unique_ref(tmp, sc_loop, wn_addr, sc))
	return FALSE;
    }
  }

  SC_LIST * kids = sc->Kids();

  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE * tmp;
    
    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      if (tmp->Type() == SC_LOOP) {
	if (!Check_index(tmp)
	    || !Check_iteration(tmp, SC_LP_COND, sc_loop)
	    || !Check_iteration(tmp, SC_LP_STEP, sc_loop))
	  return FALSE;
	else
	  tmp = tmp->Find_kid_of_type(SC_LP_BODY);
      }
      
      if (!tmp || !Get_unique_ref(tmp, sc_loop, wn_addr))
	return FALSE;
    }
  }

  return TRUE;
}

// Query whether there exists a loop inside 'sc' whose headers can modifiy 'sc_loop' and 
// its consecutive succeeding loops' induction variables.
BOOL
CFG_TRANS::Can_mod_iv(SC_NODE * sc, SC_NODE * sc_loop)
{
  if (sc->Type() == SC_LOOP) {
    SC_NODE * sc_iter = sc_loop;
    SC_NODE * sc_init = sc->Find_kid_of_type(SC_LP_START);

    if (sc_init) {
      while (sc_iter && (sc_iter->Type() == SC_LOOP)) {
	WN * wn_load = Get_index_load(sc_iter);
	if (wn_load && Has_dependency(sc_init, wn_load))
	  return TRUE;
	sc_iter = sc_iter->Next_sibling();
      }
    }
  }
  
  SC_LIST * kids = sc->Kids();

  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE * tmp;

    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      if (Can_mod_iv(tmp, sc_loop))
	return TRUE;
    }
  }

  return FALSE;
}

// Find whether the given bb has a unique read/write reference.
// See CFG_TRANS::Get_unique_ref(SC_NODE *, SC_NODE *, WN **)
// 'sc_loc' gives the containing SC_NODE of 'bb'.
BOOL
CFG_TRANS::Get_unique_ref(BB_NODE * bb, SC_NODE * sc_loop, WN ** wn_addr, SC_NODE * sc_loc)
{
  STMT_ITER stmt_iter;
  WN * wn_iter;
  FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {
    if (!Get_unique_ref(wn_iter, sc_loop, wn_addr, sc_loc))
      return FALSE;
  }

  return TRUE;
}

// Find whether the given wn has a unique read/write reference.
// See CFG_TRANS::Get_unique_ref(SC_NODE *, SC_NODE *, WN **)
// 'sc_loc' gives the containing SC_NODE of 'wn'.
BOOL
CFG_TRANS::Get_unique_ref(WN * wn, SC_NODE * sc_loop, WN ** wn_addr, SC_NODE * sc_loc)
{
  OPERATOR opr = WN_operator(wn);

  // WN_aux(wn) may not be set correctly in the memory references
  // in the expression trees for IO statements, so we should avoid
  // traversing these statements.
  if (opr == OPR_IO) {
    if (wn_addr)
      *wn_addr = NULL;
    return FALSE;
  }
  
  if (OPERATOR_is_load(opr) || OPERATOR_is_store(opr)) {
    if (OPERATOR_is_scalar_store(opr)) {
      if (wn_addr)
	*wn_addr = NULL;
      return FALSE;
    }
    else if (OPERATOR_is_scalar_load(opr)) {
      SC_NODE * sc_iter = sc_loc;
      BOOL is_index = FALSE;
      // Check whether this is a load of a nesting loop's index.
      while (sc_iter && (sc_iter != sc_loop)) {
	if (sc_iter->Type() == SC_LOOP) {
	  WN * index = Get_index_load(sc_iter);
	  if (index && (WN_aux(index) == WN_aux(wn))) {
	    is_index = TRUE;
	    break;
	  }
	}
	sc_iter = sc_iter->Parent();
      }
      if (!is_index) {
	if (Is_invariant(sc_loop, wn, 0))
	  return TRUE;
	else {
	  if (wn_addr)
	    *wn_addr = NULL;
	  return FALSE;
	}
      }
    }
    else {
      WN * wn_tmp = OPERATOR_is_load(opr) ? WN_kid0(wn) : WN_kid1(wn);
      
      if ((*wn_addr) == NULL) 
	*wn_addr = wn_tmp;
      else if (WN_Simp_Compare_Trees(*wn_addr, wn_tmp) != 0) {
	if (wn_addr)
	  *wn_addr = NULL;
	return FALSE;
      }

      if (OPERATOR_is_load(opr))
	return TRUE;
      else {
	if (Get_unique_ref(WN_kid0(wn), sc_loop, wn_addr, sc_loc))
	  return TRUE;
	else {
	  if (wn_addr)
	    *wn_addr = NULL;
	  return FALSE;
	}
      }
    }
  }

  for (int i = 0; i < WN_kid_count(wn); i++) {
    if (!Get_unique_ref(WN_kid(wn, i), sc_loop, wn_addr, sc_loc)) {
      if (wn_addr)
	*wn_addr = NULL;
      return FALSE;
    }
  }

  return TRUE;
}

// Find a load of the given ST * in the WHIRL tree rooted at wn.
WN *
CFG_TRANS::Get_index_load(WN * wn, ST * st)
{
  OPT_STAB * opt_stab = _cu->Opt_stab();
  OPERATOR opr = WN_operator(wn);
  
  if (OPERATOR_is_scalar_load(opr)) {
    AUX_ID aux_id = WN_aux(wn);
    ST * cur_st = opt_stab->Aux_stab_entry(aux_id)->St();
      
    if (cur_st && (cur_st == st))
      return wn;
  }
  
  for ( int i = 0; i < WN_kid_count(wn); i++) {
    WN * wn_tmp = Get_index_load(WN_kid(wn,i), st);
    if (wn_tmp)
      return wn_tmp;
  }

  return NULL;
}

// Given a SC_LOOP, find a load of its index.
WN *
CFG_TRANS::Get_index_load(SC_NODE * sc)
{
  if (sc->Type() == SC_LOOP) {
    WN * wn_index = sc->Index();
    ST * st_index = WN_st(wn_index);
    SC_NODE * sc_tmp = sc->Find_kid_of_type(SC_LP_STEP);
    BB_NODE * bb = sc_tmp->First_bb();
    STMT_ITER stmt_iter;
    WN * wn_iter;

    FOR_ALL_ELEM (wn_iter, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {
      WN  * wn_tmp = Get_index_load(wn_iter, st_index);
      if (wn_tmp)
	return wn_tmp;
    }
    
    FmtAssert(FALSE, ("Load of loop index not found"));
  }

  return NULL;
}

// Do loop fusion for sc and its consecutive loops.
// If 'limit' is greater than zero, it gives the maximum number of loop fusions allowed.
// Return the fused loop.
SC_NODE * 
CFG_TRANS::Do_loop_fusion(SC_NODE * sc, int limit)
{
  FmtAssert((sc->Type() == SC_LOOP), ("Expect a SC_LOOP"));

  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return NULL;

  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();

  SC_NODE * sc_body = sc->Find_kid_of_type(SC_LP_BODY);
  BB_LIST * bb_list;
  BB_NODE * bb_tmp;
  BB_NODE * bb_tmp2;
  SC_NODE * sc_tmp = sc->Next_sibling();
  SC_NODE * sc_tmp1;
  SC_NODE * sc_tmp2;
  BB_LIST_ITER bb_list_iter;
  int count = 0;

  if (sc_body->Last_bb()) {
    _def_map.clear();
    WN *  wn_index = Get_index_load(sc);

    while (sc_tmp && (sc_tmp->Type() == SC_LOOP)) {
      if ((limit > 0) && (count >= limit))
	break;

      count++;

      BB_NODE * bb2 = sc_body->Last_bb();      
      SC_NODE * cur_sc_body = sc_tmp->Find_kid_of_type(SC_LP_BODY);
      BB_NODE * cur_bb1 = cur_sc_body->First_bb();
      BB_NODE * cur_bb2 = cur_sc_body->Last_bb();
      SC_NODE * sc_next = sc_tmp->Next_sibling();
      WN * cur_wn_index = Get_index_load(sc_tmp);

      // Replace loop indexes.
      Add_def_map(WN_aux(cur_wn_index), wn_index);
      Copy_prop(cur_sc_body);

      if (cur_bb1) {
	if (_trace) 
	  printf("\n\t\t Loop fusion (SC%d,SC%d)\n", sc->Id(), sc_tmp->Id());

	// fuse loop bodies.
	FmtAssert((bb2->Succ()->Len() == 1) && (cur_bb2->Succ()->Len() == 1)
		  && (cur_bb1->Pred()->Len() == 1), ("Expect single pred/succ"));
	bb_tmp = bb2->Succ()->Node();
	bb_tmp->Replace_pred(bb2, cur_bb2);

	if (cfg->Feedback()) {
	  cfg->Feedback()->Move_edge_dest(bb2->Id(), bb_tmp->Id(), cur_bb1->Id());
	  bb_tmp2 = cur_bb2->Succ()->Node();
	  cfg->Feedback()->Move_edge_dest(cur_bb2->Id(), bb_tmp2->Id(), bb_tmp->Id());
	}

	cur_bb2->Set_succ(bb2->Succ());
	bb_tmp = cur_bb1->Pred()->Node();
	cur_bb1->Replace_pred(bb_tmp, bb2);
	bb_list = CXX_NEW(BB_LIST(cur_bb1), pool);
	bb2->Set_succ(bb_list);

	bb_tmp = bb2->Next();
	bb2->Set_next(cur_bb1);
	cur_bb1->Set_prev(bb2);
	cur_bb2->Set_next(bb_tmp);
	bb_tmp->Set_prev(cur_bb2);

	sc_tmp1 = cur_sc_body->First_kid();
	while (sc_tmp1) {
	  sc_tmp2 = sc_tmp1->Next_sibling();
	  sc_tmp1->Unlink();
	  sc_body->Append_kid(sc_tmp1);
	  sc_tmp1->Set_parent(sc_body);
	  sc_tmp1 = sc_tmp2;
	}
      }

      // Unlink the second loop.
      BB_NODE * bb_entry =  sc_tmp->First_bb();
      BB_NODE * bb_merge = sc_tmp->Merge();
      BB_NODE * bb_exit = sc_tmp->Exit();
      
      if (cfg->Feedback())
	cfg->Feedback()->Delete_edge(bb_exit->Id(), bb_merge->Id());

      FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_entry->Pred())) {
	bb_tmp->Replace_succ(bb_entry, bb_merge);
	
	if (bb_tmp->Is_branch_to(bb_entry)) {
	  WN * branch_wn = bb_tmp->Branch_wn();
	  cfg->Add_label_with_wn(bb_merge);
	  WN_label_number(branch_wn) = bb_merge->Labnam();
	}

	if (cfg->Feedback())
	  cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_entry->Id(), bb_merge->Id());
      }
      
      bb_merge->Set_pred(bb_entry->Pred());

      bb_tmp = bb_entry->Prev();
      bb_tmp->Set_next(bb_merge);
      bb_merge->Set_prev(bb_tmp);
      sc_tmp->Unlink();
      _unlink_sc->Push(sc_tmp);
      sc_tmp = sc_next;

      cfg->Fix_info(sc);
      cfg->Fix_info(sc->Parent());
      cfg->Fix_info(sc->Get_real_parent());
    }
  }

  if (count > 0) {
    cfg->Invalidate_and_update_aux_info(FALSE);    
    cfg->Invalidate_loops();
    Inc_transform_count();
    return sc;
  }
  else
    return NULL;
}

// Compare whether wn1 in loop1 is the same as wn2 in loop2.
// Loads of loop indexes are considered to be the same.

BOOL
CFG_TRANS::Compare_trees(WN * wn1, SC_NODE * loop1, WN * wn2, SC_NODE * loop2)
{
  OPERATOR op1 = WN_operator(wn1);
  OPERATOR op2 = WN_operator(wn2);
  
  if (OPERATOR_is_scalar_load(op1)
      && OPERATOR_is_scalar_load(op2)) {
    ST * st1 = Get_st(wn1);
    ST * st2 = Get_st(wn2);
    WN * index1 = loop1->Index();
    WN * index2 = loop2->Index();

    if (st1 && st2 && (st1 == WN_st(index1)) && (st2 == WN_st(index2)))
      return TRUE;
    else
      return (WN_Simp_Compare_Trees(wn1, wn2) == 0);
  }

  if ((op1 != op2) || (WN_kid_count(wn1) != WN_kid_count(wn2)))
    return FALSE;

  if ((op1 == OPR_INTCONST)
      && (WN_const_val(wn1) != WN_const_val(wn2)))
    return FALSE;

  for (int i = 0; i < WN_kid_count(wn1); i++) {
    WN * kid1 = WN_kid(wn1, i);
    WN * kid2 = WN_kid(wn2, i);
    if (!Compare_trees(kid1, loop1, kid2, loop2))
      return FALSE;
  }

  return TRUE;
}

// Check whether given SC_LOOP's index is a AUTO or a REG.
BOOL
CFG_TRANS::Check_index(SC_NODE * sc)
{
  WN * wn_index = sc->Index();
  if (!wn_index)
    return FALSE;
  
  // Check whether loop index is a REG to avoid potential alias.
  ST * index_st = WN_st(wn_index);
  if (ST_sclass(index_st) != SCLASS_REG)
    return FALSE;
  
  return TRUE;
}

// Given a sc_loop, check whether loads/stores in the sc_loop's kid of sc_type are either loop 
// invariants w.r.t. sc_ref or loop indexes w.r.t to sc_loop.
BOOL
CFG_TRANS::Check_iteration(SC_NODE * sc_loop, SC_TYPE sc_type, SC_NODE * sc_ref)
{
  WN * wn_load = Get_index_load(sc_loop);
  AUX_ID loop_aux_id = wn_load ? WN_aux(wn_load) : 0;

  SC_LIST_ITER sc_list_iter;
  SC_NODE * sc_tmp1 = sc_loop->Find_kid_of_type(sc_type);
  SC_NODE * sc_tmp2;
  
  FOR_ALL_ELEM(sc_tmp2, sc_list_iter, Init(sc_tmp1->Kids())) {
    if (!Is_invariant(sc_ref, sc_tmp2, loop_aux_id))
      return FALSE;
  }
  
  return TRUE;
}

// Check whether sc_loop can be fused with its consecutive succeeding loops.
BOOL
CFG_TRANS::Can_fuse(SC_NODE * sc_loop)
{
  if (sc_loop->Type() != SC_LOOP)
    return FALSE;

  SC_NODE * sc_cur = sc_loop->Next_sibling();
  if (!sc_cur || (sc_cur->Type() != SC_LOOP))
    return FALSE;

  WN * wn_u = NULL;
  SC_NODE * lp_u = NULL;
  sc_cur = sc_loop;

  while (sc_cur && (sc_cur->Type() == SC_LOOP)) {
    if ((sc_cur != sc_loop) && !sc_cur->Has_same_loop_struct(sc_loop))
      return FALSE;

    if (!Check_index(sc_cur)
      || !Check_iteration(sc_cur, SC_LP_COND, sc_cur)
      || !Check_iteration(sc_cur, SC_LP_STEP, sc_cur))
      return FALSE;

    // Only do it for the simplest case where the loops only contains
    // non-scalar memory references of the same address.
    // TODO: code sharing with LNO for legality check.
    WN * wn_tmp = NULL;
    SC_NODE * sc_body = sc_cur->Find_kid_of_type(SC_LP_BODY);
    Get_unique_ref(sc_body, sc_cur, &wn_tmp);

    if (wn_tmp == NULL)
      return FALSE;

    if (Can_mod_iv(sc_body, sc_loop))
      return FALSE;
    
    if (wn_u == NULL) {
      wn_u = wn_tmp;
      lp_u = sc_cur;
    }
    else if (!Compare_trees(wn_u, lp_u, wn_tmp, sc_cur))
      return FALSE;

    sc_cur = sc_cur->Next_sibling();
  }
  
  return TRUE;
}

// Sink 'sc2' out of 'sc1' (where 'sc1' is a SC_IF, 'sc2' is a SC_LOOP or a SC_IF,
// and 'sc2' is an immedidate child of 'sc1'). If 'do_wrap' is TRUE, wrap the sinked loops
// with a if-condition that is a copy of 'sc1' and push the if-condition into the loop body.
// Caller of this routine must ensure the legality of this transformation.  Return TRUE if the
// transformation is performed.
BOOL
CFG_TRANS::Do_sink_node(SC_NODE * sc1, SC_NODE * sc2, BOOL do_wrap)
{
  FmtAssert((sc1->Type() == SC_IF) && ((sc2->Type() == SC_LOOP) || (sc2->Type() == SC_IF)),
	    ("Unexpected SC type"));
  FmtAssert((sc2->Get_real_parent() == sc1), ("Expect an immediate parent-child relationship."));

  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return FALSE;

  if (_trace)
    printf("\n\t\t Sink node out of if-region (SC%d,SC%d)\n", sc1->Id(), sc2->Id());

  CFG * cfg = _cu->Cfg();
  SC_NODE * sc_begin = sc2;

  // Make sure merges are empty blocks.
  SC_NODE * sc_merge = sc2->Next_sibling();
  if (!sc_merge->Is_empty_block())
    cfg->Insert_block_before(sc_merge);

  sc_merge = sc1->Next_sibling();
  if (!sc_merge->Is_empty_block()) 
    sc_merge = cfg->Insert_block_after(sc1);

  SC_NODE * sc_end = sc2->Next_sibling();
  SC_NODE * sc1_p = sc1->Get_real_parent();
  BOOL is_then = (sc2->Parent()->Type() == SC_THEN) ? TRUE : FALSE;

  // Insert an empty block to keep sese for sc1.
  if (!sc_end->Next_sibling())
    cfg->Insert_block_after(sc_end);

  BB_NODE * bb_head = sc1->Get_bb_rep(); 
  BB_NODE * bb_merge = sc1->Merge();
  BB_NODE * bb_merge_end = sc_merge->Last_bb();
  BB_NODE * bb_tmp1;
  BB_NODE * bb_tmp2;
  BB_NODE * bb_tmp;
  BB_NODE * bb_prev;
  BB_NODE * bb_next;
  BB_NODE * bb_44;
  BB_LIST * bb_list_tmp;
  BB_LIST_ITER bb_list_iter;
  WN * branch_wn;
  BB_IFINFO * if_info;
  MEM_POOL * pool = cfg->Mem_pool();
  IDTYPE edge;
  FB_FREQ freq;

  bb_tmp1 = sc_begin->First_bb();
  bb_tmp2 = sc_end->Last_bb();
  bb_44 = bb_tmp2->Succ()->Node();
  
  // Sink blocks in [sc_begin, sc_end] out of if-region, where sc_end is a SC_BLOCK.
  //                                        
  //                   42                     42
  //                    |                     |
  //        bb_head->  79 <--- sc1            79
  //                /       \                /  \
  //              ...       |              ...   |
  //               |        |               |    |
  //    bb_tmp1-> 51        ...       =>    44  ...  
  //               |                        \   /
  //    bb_tmp2-> 52        |                merge <- bb_merge
  //               |        |                 |
  //              44        |                ... <- bb_merge_end
  //               \        /                 |
  //      bb_merge->  merge		     51
  //                  |                       |
  //  bb_merge-end-> ...                     52
  //                  |                       |

  // ... <-> 44
  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_tmp1->Pred())) {
    if (bb_tmp->Is_branch_to(bb_tmp1)) {
      branch_wn = bb_tmp->Branch_wn();
      cfg->Add_label_with_wn(bb_44);
      WN_label_number(branch_wn) = bb_44->Labnam();
    }

    bb_tmp->Replace_succ(bb_tmp1, bb_44);

    if (cfg->Feedback())
      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb_tmp1->Id(), bb_44->Id());
  }

  bb_44->Set_pred(bb_tmp1->Pred());

  // 52<-
  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_merge_end->Succ())) {
    bb_tmp->Replace_pred(bb_merge_end, bb_tmp2);

    if (cfg->Feedback()) {
      cfg->Feedback()->Move_edge_dest(bb_merge_end->Id(), bb_tmp->Id(), bb_tmp1->Id());
      cfg->Feedback()->Move_edge_dest(bb_tmp2->Id(), bb_44->Id(), bb_tmp->Id());
      freq = cfg->Feedback()->Get_edge_freq(bb_merge_end->Id(), bb_tmp1->Id());
      edge = cfg->Feedback()->Get_edge(bb_tmp2->Id(), bb_tmp->Id());
      cfg->Feedback()->Change_edge_freq(edge, freq);
    }
  }

  // 52->
  bb_tmp2->Set_succ(bb_merge_end->Succ());

  // bb_merge_end<->51
  bb_list_tmp = CXX_NEW(BB_LIST(bb_tmp1), pool);
  bb_merge_end->Set_succ(bb_list_tmp);
  bb_list_tmp = CXX_NEW(BB_LIST(bb_merge_end), pool);
  bb_tmp1->Set_pred(bb_list_tmp);

  bb_prev = bb_tmp1->Prev();
  bb_prev->Set_next(bb_44);
  bb_tmp = bb_44->Prev();
  bb_44->Set_prev(bb_prev);
  bb_next = bb_merge_end->Next();
  bb_next->Set_prev(bb_tmp);
  bb_tmp->Set_next(bb_next);
  bb_merge_end->Set_next(bb_tmp1);
  bb_tmp1->Set_prev(bb_merge_end);

  SC_NODE * sc_insert = sc1->Next_sibling();
  SC_NODE * sc_tmp;
  SC_NODE * sc_tmp1 = sc_begin;
  SC_NODE * sc_tmp2;

  while (sc_tmp1) {
    sc_tmp2 = sc_tmp1;
    sc_tmp1 = sc_tmp1->Next_sibling();

    if (do_wrap) {
      if (sc_tmp2->Type() == SC_LOOP) {
	// Wrap loop body with cloned condition.
	sc_tmp = sc_tmp2->Find_kid_of_type(SC_LP_BODY);
	Do_if_cond_wrap(bb_head, sc_tmp, is_then);
      }
    }

    sc_tmp2->Unlink();
    sc_insert->Insert_after(sc_tmp2);
    sc_insert = sc_tmp2;
    if (sc_tmp2 == sc_end)
      break;
  }

  sc_tmp1 = sc1->Next_sibling();
  while (sc_tmp1) {
    cfg->Fix_info(sc_tmp1);
    sc_tmp1 = sc_tmp1->Next_sibling();
  }
  cfg->Fix_info(sc1_p);

  // Fix SC tree.
  cfg->Fix_info(sc1);

  if (cfg->Feedback()) {
    sc_tmp1 = sc_begin;
    while (sc_tmp1) {
      SC_TYPE type = sc_tmp1->Type();
      if (type == SC_LOOP) {
	bb_tmp1 = sc_tmp1->Head();
	sc_tmp = sc_tmp1->Find_kid_of_type(SC_LP_BODY);
	// If head does not belong to loop body, invalidate edge freq for
	// head's successors.
	if (sc_tmp && !sc_tmp->Contains(bb_tmp1)) {
	  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_tmp1->Succ())) {
	    edge = cfg->Feedback()->Get_edge(bb_tmp1->Id(), bb_tmp->Id());
	    if (edge)
	      cfg->Feedback()->Change_edge_freq(edge, FB_FREQ_UNKNOWN);
	  }
	}
	// Invalidate edge freq on loop exits.
	bb_tmp1 = sc_tmp1->Exit();
	bb_tmp2 = sc_tmp1->Merge();
	edge = cfg->Feedback()->Get_edge(bb_tmp1->Id(), bb_tmp2->Id());
	if (edge)
	  cfg->Feedback()->Change_edge_freq(edge, FB_FREQ_UNKNOWN);
      }
      else {
	if (type == SC_IF) 
	  bb_tmp1 = sc_tmp1->Head();
	else
	  bb_tmp1 = sc_tmp1->Last_bb();
	FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_tmp1->Succ())) {
	  edge = cfg->Feedback()->Get_edge(bb_tmp1->Id(), bb_tmp->Id());
	  if (edge)
	    cfg->Feedback()->Change_edge_freq(edge, FB_FREQ_UNKNOWN);
	}
      }
      cfg->Freq_propagate(sc_tmp1);

      if (sc_tmp1 == sc_end)
	break;
      sc_tmp1 = sc_tmp1->Next_sibling();
    }
  }

  bb_tmp1 = sc1->Head();
  bb_tmp2 = sc1->Merge();

  // If sc1 degenerates into an empty region, change it into a SC_BLOCK.
  if ((sc1->Then() == bb_tmp2) && (sc1->Else() == bb_tmp2)) {
    sc_tmp = sc1->Find_kid_of_type(SC_THEN);
    sc_tmp->Unlink();
    sc_tmp->Delete();
    sc_tmp = sc1->Find_kid_of_type(SC_ELSE);
    sc_tmp->Unlink();
    sc_tmp->Delete();
    sc1->Convert(SC_BLOCK);
    bb_tmp1->Set_kind(BB_GOTO);
    FmtAssert((bb_tmp1->Succ()->Len() == 2), ("Expect two successors"));
    bb_tmp1->Remove_succ(bb_tmp2, pool);
    bb_tmp2->Remove_pred(bb_tmp1, pool);
    bb_tmp1->Set_laststmt(NULL);
    bb_tmp1->Set_firststmt(NULL);
    
    if (cfg->Feedback()) {
      freq = cfg->Feedback()->Get_node_freq_in(bb_tmp1->Id());
      edge = cfg->Feedback()->Get_edge(bb_tmp1->Id(), bb_tmp2->Id());
      cfg->Feedback()->Change_edge_freq(edge, freq);
    }
  }

  cfg->Invalidate_and_update_aux_info(FALSE);    
  cfg->Invalidate_loops();
  Inc_transform_count();

  return TRUE;
}

// Do reversed loop unswitching.
BOOL
IF_MERGE_TRANS::Do_reverse_loop_unswitching
(SC_NODE * sc1, SC_NODE * sc2, SC_NODE * outer_loop)
{
  FmtAssert((sc1->Type() == SC_IF) && (sc2->Type() == SC_LOOP), ("Unexpected SC type"));
  CFG * cfg = _cu->Cfg();

  // sc1 should a SESE and the only SC_IF among its siblings.
  if (!sc1->Is_sese()
      || (sc1->Next_sibling_of_type(SC_IF) != NULL)
      || (sc1->Parent()->Find_kid_of_type(SC_IF) != sc1))
    return FALSE;

  if (!Check_index(sc2)
      || !Check_iteration(sc2, SC_LP_COND, sc2)
      || !Check_iteration(sc2, SC_LP_STEP, sc2))
    return FALSE;

  SC_NODE * sc_merge = sc1->Next_sibling();

  // sc_merge should be an empty block. Canonicalization guarantees this.
  if (!sc_merge->Is_empty_block())
    return FALSE;

  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return FALSE;
  
  if (_trace)
    printf("\n\t\t Reversed loop unswitching (SC%d,SC%d)\n", sc1->Id(), sc2->Id());
  
  SC_NODE * sc_begin = Do_pre_dist(sc2, outer_loop);
  
  if (sc_begin == NULL)
    return FALSE;

  // Catch cases of zero dependency vector to do loop fusion here.
  WN * wn_u = NULL;
  SC_NODE * lp_u = NULL;
  SC_NODE * sc_tmp1;
  SC_NODE * sc_tmp2;
  
  if (Can_fuse(sc_begin)) {
    Get_unique_ref(sc_begin->Find_kid_of_type(SC_LP_BODY), sc_begin, &wn_u);
    lp_u = sc_begin;

    // Do loop fusion if possible.
    sc_tmp1 = Do_loop_fusion(sc_begin, 0);
    if (sc_tmp1) 
      sc2 = sc_tmp1;
  }

  SC_NODE * sc_end = sc2->Next_sibling_of_type(SC_BLOCK);
  FmtAssert(sc_end && sc_end->Is_empty_block(), ("Expect an empty merge for loop"));

  sc_tmp1 = sc_merge->Next_sibling_of_type(SC_LOOP);

  if (sc_tmp1) {
    // Do not sink if there exists sinked loops and the sinked loops were not fused.    
    if (sc_tmp1->Next_sibling_of_type(SC_LOOP) != NULL)
      return FALSE;

    // Do not sink if sc2 and already-sinked loops are not loop fusion candidates.
    if (!sc_tmp1->Has_same_loop_struct(sc2))
      return FALSE;
    
    WN * wn_tmp = NULL;
    Get_unique_ref(sc_tmp1->Find_kid_of_type(SC_LP_BODY), sc_tmp1, &wn_tmp);

    if ((wn_tmp == NULL) 
	|| (wn_u == NULL)
	|| !Compare_trees(wn_u, lp_u, wn_tmp, sc_tmp1))
      return FALSE;
  }


  if (!Do_sink_node(sc1, sc2, TRUE))
    return FALSE;

  Hoist_succ_blocks(sc2);

  // Remove empty blocks between sc_merge and sc2.
  sc_tmp1 = sc_merge->Next_sibling();
  while (sc_tmp1 && (sc_tmp1 != sc2)) {
    sc_tmp2 = sc_tmp1;
    sc_tmp1 = sc_tmp1->Next_sibling();
    if (sc_tmp2->Is_empty_block())
      Remove_block(sc_tmp2);
  }

  cfg->Invalidate_and_update_aux_info(FALSE);    
  cfg->Invalidate_loops();
  Inc_transform_count();

  if (Can_fuse(sc2)) {
    sc_tmp1 = Do_loop_fusion(sc2, 0);
    if (sc_tmp1) {
      Top_down_trans(sc_tmp1->Find_kid_of_type(SC_LP_BODY));
    }
  }

  return TRUE;
}

// Prune WHIRLs in SC_BLOCKs.
// - Remove consecutive bitop operations that flip the same bit.
void
CFG_TRANS::Prune_block(SC_NODE * sc)
{
  if (sc->Type() == SC_BLOCK) {
    BB_LIST * bb_list = sc->Get_bbs();
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * bb_tmp;
    WN * wn1 = NULL;
    BB_NODE * bb1 = NULL;

    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init()) {
      WN * wn_iter = bb_tmp->Firststmt();
      WN * wn_next = NULL;

      while (wn_iter) {
	wn_next = WN_next(wn_iter);
	WN * wn_bit_op = WN_get_bit_reduction(wn_iter);
	
	if (wn_bit_op) {
	  if (!wn1) {
	    wn1 = wn_iter;
	    bb1= bb_tmp;
	  }
	  else if ((WN_operator(wn_bit_op) == OPR_BXOR)
		   && (WN_Simp_Compare_Trees(WN_kid1(wn1), WN_kid1(wn_iter)) == 0)
		   && (WN_Simp_Compare_Trees(wn_bit_op,WN_get_bit_reduction(wn1)) == 0)) {
	    bb1->Remove_stmt(wn1);
	    bb_tmp->Remove_stmt(wn_iter);
	    wn_iter = wn_next;
	    wn1 = NULL;
	    bb1 = NULL;
	  }
	  else {
	    wn1 = wn_iter;
	    bb1 = bb_tmp;
	  }
	}
	else if (WN_is_executable(wn_iter)) {
	  wn1 = NULL;
	  bb1 = NULL;
	}
	wn_iter = wn_next;
      }
    }

    if (wn1) {
      // Find next sibling which is a non-empty SC_BLOCK.
      SC_NODE * sc2 = sc->Next_sibling_of_type(SC_BLOCK);
      while (sc2 && sc2->Is_empty()) {
	sc2 = sc2->Next_sibling_of_type(SC_BLOCK);
      }

      if (sc2) {
	BB_NODE * bb2 = sc2->First_executable_blk();
	if (bb2) {
	  WN * wn2 = bb2->First_executable_stmt();
	  if (wn2 && WN_get_bit_reduction(wn2)
	      && (WN_Simp_Compare_Trees(WN_kid0(wn1), WN_kid0(wn2)) == 0)
	      && sc->Is_ctrl_equiv(sc2)) {
	    SC_NODE * sc_tmp = sc->Next_sibling();
	    BOOL has_dep = FALSE;

	    Infer_val_range(sc, sc2);
	    
	    // Check whether wn1 has dependency on siblings between sc and sc2.
	    while (sc_tmp && (sc_tmp != sc2)) {
	      if (Has_dependency(sc_tmp, wn1)) {
		has_dep = TRUE;
		break;
	      }
	      sc_tmp = sc_tmp->Next_sibling();
	    }

	    Delete_val_range_maps();

	    if (!has_dep) {
	      bb1->Unlink_stmt(wn1);
	      bb2->Unlink_stmt(wn2);
	      WN_Delete(wn1);
	      WN_Delete(wn2);
	    }
	  }
	}
      }
    }
  }
}

// Prune a SC_IF, return TRUE if pruned.
BOOL
CFG_TRANS::Prune_if(SC_NODE * sc)
{
  BOOL unlinked = FALSE;
  SC_NODE * sc_prev = sc->Prev_sibling();
  SC_NODE * sc_parent = sc->Get_real_parent();
  CFG * cfg = _cu->Cfg();
  IDTYPE id = sc->Id();

  if (sc->Type() == SC_IF) {
    SC_NODE * first_kid = sc->First_kid();
    SC_NODE * last_kid = sc->Last_kid();
    CFG * cfg = _cu->Cfg();
    SC_NODE * sc_p = sc->Parent();

    if (sc->Head()->Executable_stmt_count() == 1) {
      if (first_kid->Is_empty()
	  && last_kid->Is_empty()) {
	Remove_node(sc);
	unlinked = TRUE;
      }
      else if (sc->Is_sese()) {
	SC_NODE * sc_iter1 = first_kid->First_executable_kid();
	SC_NODE * sc_iter2 = last_kid->First_executable_kid();
	BOOL is_same = TRUE;
	
	while (sc_iter1 && sc_iter2) {
	  if (!sc_iter1->Compare_Trees(sc_iter2)) {
	    is_same = FALSE;
	    break;
	  }
	  sc_iter1 = sc_iter1->Next_executable_sibling();
	  sc_iter2 = sc_iter2->Next_executable_sibling();
	}
      
	BB_NODE * bb_prev;
	BB_NODE * bb_iter1;
	BB_NODE * bb_iter2;
	BB_NODE * bb_merge = sc->Merge();
	
	// If sc's then-path and else-path are identical, remove the condition.
	if (is_same && (sc_iter1 == NULL) && (sc_iter2 == NULL)) {
	  sc_iter1 = last_kid->First_kid();
	  sc_iter2 = last_kid->Last_kid();
	  FmtAssert((first_kid->Last_kid()->Type() == SC_BLOCK), ("Expect a SC_BLOCK."));
	  bb_prev = first_kid->Last_bb();
	  bb_iter1 = last_kid->First_bb();
	  bb_iter2 = last_kid->Last_bb();

	  Do_if_cond_unwrap(sc);
	
	  SC_NODE * tmp = sc_iter1;
	  SC_NODE * sc_end = sc_iter2->Next_sibling();
	  while (tmp && (tmp != sc_end)) {
	    SC_NODE * sc_next = tmp->Next_sibling();
	    tmp->Unlink();
	    tmp->Delete();
	    tmp = sc_next;
	  }

	  bb_prev->Set_succ(bb_iter2->Succ());
	  bb_merge->Set_pred(bb_iter1->Pred());
	  bb_iter2->Set_succ(NULL);
	  bb_iter1->Set_pred(NULL);
	  bb_prev->Set_next(bb_merge);
	  bb_merge->Set_prev(bb_prev);
	  unlinked = TRUE;
	}
      }
    }
  }

  if (unlinked) {
    if (sc_prev)
      cfg->Fix_info(sc_prev);
    cfg->Fix_info(sc_parent);
    cfg->Invalidate_and_update_aux_info(FALSE);
    cfg->Invalidate_loops();
    Inc_transform_count();

    if (_trace)
      printf("\n\t\t Prune IF (SC%d)\n", id);
  }

  return unlinked;
}

// Find pattern like
// (1) x ^= ( 1 << w)
// (2) if (x & ( 1 << w) {
//      statement 
//     }
// Change it to:
// (1) if (x & ( 1 << w)) {
// (2)    x ^= ( 1 << w)
//     }
//     else {
// (3)    x ^= ( 1 << w)
// (4)    statement
//     }
//
// In addition, if "x ^= ( 1 << w)" has no dependency on "statement",
// we can sink it to the if-region's merge block.
void
IF_MERGE_TRANS::Do_negate(SC_NODE * sc)
{
  SC_NODE * sc_next = sc->Next_sibling();
  CFG * cfg = _cu->Cfg();

  if ((sc->Type() == SC_BLOCK)
      && (sc->Executable_stmt_count() == 1)
      && sc_next
      && (sc_next->Type() == SC_IF)
      && (sc_next->Is_sese())
      && (sc_next->Head()->Executable_stmt_count() == 1)) {
    WN * wn_tmp = sc->First_executable_stmt();
    WN * wn_red = WN_get_bit_reduction(wn_tmp);
    WN * wn_cond = sc_next->Get_cond();

    if (wn_red && (WN_operator(wn_red) == OPR_BXOR)
	&& wn_cond
	&& (WN_operator(wn_cond) == OPR_NE)
	&& (WN_operator(WN_kid1(wn_cond)) == OPR_INTCONST)
	&& (WN_const_val(WN_kid1(wn_cond)) == 0)
	&& (WN_operator(WN_kid0(wn_cond)) == OPR_BAND)) {
      wn_tmp = WN_kid0(wn_cond);

      if ((WN_Simp_Compare_Trees(WN_kid0(wn_red), WN_kid0(wn_tmp)) == 0)
	  && (WN_Simp_Compare_Trees(WN_kid1(wn_red), WN_kid1(wn_tmp)) == 0)) {
	SC_NODE * sc_then = sc_next->First_kid();	  
	SC_NODE * sc_else = sc_next->Last_kid();
	
	Do_flip(sc_next);

	WN * wn_tmp = sc->First_executable_stmt();
	BB_NODE * bb_tmp = sc->First_executable_blk();
	BB_NODE * bb_merge = sc_next->Merge();

	if (_trace) {
	  printf("\n\t\t Func: %s(%d) negate if-condition (SC:%d)\n", 
		 Current_PU_Name(), Current_PU_Count(), sc_next->Id());
	}
	if (!Has_dependency(sc_then, sc)
	    && !Has_dependency(sc_else, sc)) {
	  bb_tmp->Unlink_stmt(wn_tmp);
	  bb_merge->Prepend_stmt(wn_tmp);
	  Prune_block(sc_next->Next_sibling());
	}
	else
	  Do_head_duplication(sc, sc_next);
      }
    }
  }
}

// Bottom-up prune dead codes for the SC tree rooted at 'sc'.
// Return TRUE if 'sc' is unlinked or deleted.
BOOL
CFG_TRANS::Bottom_up_prune(SC_NODE * sc)
{
  SC_NODE * tmp = sc->First_kid();
  SC_NODE * sc_next;

  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return FALSE;

  if (sc->Find_kid_of_type(SC_COMPGOTO))
    return FALSE;

  while (tmp) {
    // Merge consecutive blocks if possible.
    sc_next = tmp->Next_sibling();
    if (tmp->Type() == SC_BLOCK) {
      SC_NODE * sc_cur = tmp;
      while (sc_next && (sc_next->Type() == SC_BLOCK)) {
	sc_cur = Merge_block(sc_cur, sc_next);
	if (sc_cur) 
	  sc_next = sc_cur->Next_sibling();
	else
	  sc_next = NULL;
      }
    }

    // Remove unused label statements.
    BB_NODE * bb_rep = tmp->Get_bb_rep();
    if (bb_rep) {
      WN * wn_label = bb_rep->Firststmt();
      if (wn_label && (WN_operator(wn_label) == OPR_LABEL)) {
	BOOL is_branch_target = FALSE;
	BB_NODE * bb_tmp;
	BB_LIST_ITER bb_list_iter;
	FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_rep->Pred())) {
	  WN * wn_branch = bb_tmp->Branch_wn();
	  if (wn_branch) {
	    OPERATOR opr = WN_operator(wn_branch);
	    if (((opr != OPR_FALSEBR) && (opr != OPR_TRUEBR)
		 && (opr != OPR_GOTO))
		|| bb_tmp->Is_branch_to(bb_rep)) {
	      is_branch_target = TRUE;
	      break;
	    }
	  }
	}
	if (!is_branch_target) 
	  bb_rep->Unlink_stmt(wn_label);
      }
    }

    sc_next = tmp->Next_sibling();
    if (Bottom_up_prune(tmp))
      break;
    tmp = sc_next;
  }

  SC_TYPE type = sc->Type();
  BOOL unlinked = FALSE;
  SC_NODE * sc_prev = sc->Prev_sibling();
  SC_NODE * sc_parent = sc->Get_real_parent();
  SC_NODE * first_kid = sc->First_kid();
  SC_NODE * last_kid = sc->Last_kid();
  CFG * cfg = _cu->Cfg();
  SC_NODE * sc_p = sc->Parent();
  IDTYPE id = sc->Id();

  switch (type) {
  case SC_IF:
    unlinked = Prune_if(sc);
    break;
  case SC_BLOCK:
    Prune_block(sc);
    break;
  default:
    ;
  }

  if (unlinked) 
    Bottom_up_prune(sc_p);

  return unlinked;
}

// Check whether the expression tree rooted at the given wn has the following characteristic:
// - All loads are scalars.
// - No stores.
// - Condition sub-expressions are either CAND or COR.
// - Can be speculative.

BOOL
CFG_TRANS::Can_reorder_cond(WN * wn, WN * cond_wn)
{
  if (!Can_be_speculative(wn))
    return FALSE;

  OPERATOR opr = WN_operator(wn);
  OPERATOR cond_opr;

  if (cond_wn)
    cond_opr = WN_operator(cond_wn);

  BOOL ret_val = TRUE;

  switch (opr) {
  case OPR_BAND:
  case OPR_CAND:
    if (cond_wn) {
      if ((cond_opr != OPR_BAND) && (cond_opr != OPR_CAND))
	ret_val = FALSE;
    }
    else
      cond_wn = wn;
    break;
  case OPR_BIOR:
  case OPR_CIOR:
    if (cond_wn) {
      if ((cond_opr != OPR_BIOR) && (cond_opr != OPR_CIOR))
	ret_val = FALSE;
    }
    else
      cond_wn = wn;

    break;
  default:
    ;
  }

  if (!ret_val)
    return FALSE;

  if (OPERATOR_is_store(opr))
    return FALSE;
  else if (OPERATOR_is_load(opr) 
	   && !OPERATOR_is_scalar_load(opr))
    return FALSE;
  
  for (int i = 0; i < WN_kid_count(wn); i++) {
    if (!Can_reorder_cond( WN_kid(wn, i), cond_wn))
      return FALSE;
  }
  
  return TRUE;
}

// Distribute if-condition in sc to its children.
// 'do_legal_check' indicates whether to do legality check.
BOOL
CFG_TRANS::Do_if_cond_dist(SC_NODE * sc, BOOL do_legal_check)
{
  if ((sc->Type() != SC_IF) || (sc->Head()->Executable_stmt_count() != 1))
    return FALSE;

  SC_NODE * sc_p = sc->Get_real_parent();
  SC_NODE * sc_prev = sc->Prev_sibling();

  //
  //              4                                 4
  //              |                                 |
  //              5                                 6  
  //        /          \                         /    \
  //       /            \                      5_1    5_2
  //      42             6 ->bb_head          /  \    / \
  //    /   \          /   \                  |  8    | |
  //   39    51        8    7                 | /\    | 7
  //  /   \  |       /   \  |                e1 ...  e2 |
  //  ...    ....    .....  |  =======>       | \/    | |
  //  \   /  |       \   /  7_c               | 37    | 7_c
  //    64   60       37    |                 | |     | |
  //    \    /         \   /                  | 38    | 38'
  //      69            38->bb_merge          \ /     \ /
  //      \             /                     new_1    new_2
  //        \          /                       \      /
  //         \        /                           42
  //            65                              /    \
  //                                          5_1'   5_2'
  //                                         /  \   /  \
  //                                        39  |  51  |
  //                                       / \  |  |   |
  //                                       ... e1 ...  e2
  //                                       \ /  |  |   |
  //                                       64   |  60  |
  //                                       |    |  |   |
  //                                       69   |  69' |
  //                                        \  /   \  /
  //                                    '   new_1'   new_2'
  //                                         \      /
  //                                            65

  if (do_legal_check && !Can_reorder_cond(WN_kid0(sc->Head()->Branch_wn()), NULL))
    return FALSE;

  SC_NODE * sc_then = sc->Find_kid_of_type(SC_THEN);
  SC_NODE * sc_else = sc->Find_kid_of_type(SC_ELSE);
  SC_NODE * sc_tmp;
  SC_NODE * sc_tmp1;
  SC_NODE * sc_tmp2;
  SC_LIST_ITER sc_list_iter;

  if (do_legal_check) {
    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(sc_then->Kids())) {
      if ((sc_tmp->Type() == SC_IF) 
	  && (!Can_reorder_cond(WN_kid0(sc_tmp->Head()->Branch_wn()), NULL)
	      || (sc_tmp->Head()->Executable_stmt_count() != 1)
	      || !sc_tmp->Is_well_behaved()))
	return FALSE;
    }

    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(sc_else->Kids())) {
      if ((sc_tmp->Type() == SC_IF) 
	  && (!Can_reorder_cond(WN_kid0(sc_tmp->Head()->Branch_wn()),NULL)
	      || (sc_tmp->Head()->Executable_stmt_count() != 1)
	      || !sc_tmp->Is_well_behaved()))
	return FALSE;
    }

    // sc's head should have no dependencies on sc_then and sc_else
    if (Has_dependency(sc_then, sc->Head())
	|| Has_dependency(sc_else, sc->Head()))
      return FALSE;
  }

  // The following two cases are short-circuits for un-implemented cases.
  sc_tmp = sc_then->First_kid();
  if ((sc_tmp->Type() != SC_IF) || (sc_tmp->Next_sibling()->Next_sibling()))
    return FALSE;

  sc_tmp = sc_else->First_kid();
  if ((sc_tmp->Type() == SC_BLOCK) 
      && (!sc_tmp->Is_empty() || sc_tmp->Next_sibling()))
    return FALSE;
  
  if ((WOPT_Enable_Pro_Loop_Limit >= 0)
      && (Transform_count() >= WOPT_Enable_Pro_Loop_Limit))
    return FALSE;

  if (_trace)
    printf("\n\t\t If-cond dist (SC%d)\n", sc->Id());

  BB_NODE * bb5 = sc->Head();
  BB_NODE * bb65 = sc->Merge();
  BB_NODE * bb_else_begin;
  BB_NODE * bb_else_end;
  BB_NODE * bb_then_begin;
  BB_NODE * bb_then_end;
  BB_NODE * bb_head;
  BB_NODE * bb_merge;
  BB_NODE * bb_merge_c;
  BB_NODE * bb5_1;
  BB_NODE * bb5_2;
  BB_NODE * bb_e1;
  BB_NODE * bb_e2;
  BB_NODE * bb_tmp;
  BB_NODE * bb_new1;
  BB_NODE * bb_new2;
  BB_NODE * bb_last;
  BB_NODE * bb_next;
  BB_LIST * bb_list_new;
  BB_LIST_ITER bb_list_iter;
  WN * wn_tmp;
  BB_IFINFO * info;
  FB_FREQ freq;
  FB_FREQ freq_total;
  FB_EDGE_TYPE ft_edge_type;
  FB_EDGE_TYPE br_edge_type;

  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();

  if (cfg->Feedback()) {
    freq_total = cfg->Feedback()->Get_node_freq_out(bb5->Id());
    ft_edge_type = cfg->Feedback()->Get_edge_type(bb5->Id(), bb5->If_then()->Id());
    br_edge_type = cfg->Feedback()->Get_edge_type(bb5->Id(), bb5->If_else()->Id());
  }
  
  bb_next = NULL;
  bb_last = NULL;

  for (int i = 0; i < 2; i++) {
    sc_tmp = (i == 0) ? sc_then->First_kid() : sc_else->First_kid();
    while (sc_tmp) {
      switch (sc_tmp->Type()) {
      case SC_IF:
	bb_head = sc_tmp->Head();
	bb_merge = sc_tmp->Merge();
	bb_merge_c = NULL;
	bb_next = sc_tmp->Next_sibling()->Next_in_tree()->First_bb();
	cfg->Clone_bbs(bb_merge, bb_merge, &bb_merge_c, &bb_tmp, TRUE, 1.0);
	bb5_1 = NULL;
	bb5_2 = NULL;
	cfg->Clone_bbs(bb5, bb5, &bb5_1, &bb_tmp, TRUE, 1.0);
	cfg->Clone_bbs(bb5, bb5, &bb5_2, &bb_tmp, TRUE, 1.0);
	bb_then_begin = sc_tmp->Find_kid_of_type(SC_THEN)->First_bb();
	bb_then_end = sc_tmp->Find_kid_of_type(SC_THEN)->Last_bb();
	bb_else_begin = sc_tmp->Find_kid_of_type(SC_ELSE)->First_bb();
	bb_else_end = sc_tmp->Find_kid_of_type(SC_ELSE)->Last_bb();
	bb_new1 = cfg->Create_and_allocate_bb(bb_merge->Kind());
	bb_new2 = cfg->Create_and_allocate_bb(bb_merge->Kind());
	bb_e1 = cfg->Create_and_allocate_bb(BB_GOTO);
	bb_e2 = cfg->Create_and_allocate_bb(BB_GOTO);

	// Fix bb_head<->bb5_1, bb_head<->bb5_2,
	// Here-in-after " ->" denotes succ, "<-" denotes pred, "<->" denotes succ and pred.
	if (bb_else_begin)
	  bb_head->Replace_succ(bb_else_begin, bb5_1);
	else
	  bb_head->Replace_succ(bb_merge, bb5_1);

	bb_list_new = CXX_NEW(BB_LIST(bb_head), pool);
	bb5_1->Set_pred(bb_list_new);

	if (bb_then_begin) 
	  bb_head->Replace_succ(bb_then_begin, bb5_2);
	else
	  bb_head->Replace_succ(bb_merge, bb5_2);

	bb_list_new = CXX_NEW(BB_LIST(bb_head), pool);
	bb5_2->Set_pred(bb_list_new);

	// Fix branch label to BB5_1.
	wn_tmp = bb_head->Branch_wn();
	cfg->Add_label_with_wn(bb5_1);
	WN_label_number(wn_tmp) = bb5_1->Labnam();

	// Fix <-bb_merge, bb_then_end-> and <-bb_merge_c
	if (bb_then_end) {
	  bb_merge->Remove_pred(bb_then_end, pool);
	  bb_then_end->Replace_succ(bb_merge, bb_merge_c);
	  bb_list_new = CXX_NEW(BB_LIST(bb_then_end), pool);
	}
	else {
	  bb_merge->Remove_pred(bb_head, pool);
	  bb_list_new = CXX_NEW(BB_LIST(bb5_2), pool);
	}

	bb_merge_c->Set_pred(bb_list_new);

	// Fix bb_merge-> and bb_merge_c->
	bb_merge->Remove_succs(pool);
	bb_list_new = CXX_NEW(BB_LIST(bb_new1), pool);
	bb_merge->Set_succ(bb_list_new);
	bb_list_new = CXX_NEW(BB_LIST(bb_new2), pool);
	bb_merge_c->Set_succ(bb_list_new);

	// Fix 5_1-> and 5_2->
	bb_list_new = CXX_NEW(BB_LIST(bb_e1), pool);
	bb5_1->Set_succ(bb_list_new);
	bb_tmp = bb_else_begin ? bb_else_begin : bb_merge;
	if (i == 0)
	  bb5_1->Append_succ(bb_tmp, pool);
	else
	  bb5_1->Prepend_succ(bb_tmp, pool);

	bb_list_new = CXX_NEW(BB_LIST(bb_e2), pool);
	bb5_2->Set_succ(bb_list_new);
	bb_tmp = bb_then_begin ? bb_then_begin : bb_merge_c;
	if (i == 0)
	  bb5_2->Append_succ(bb_tmp, pool);
	else
	  bb5_2->Prepend_succ(bb_tmp, pool);

	wn_tmp = bb5_1->Branch_wn();
	if (i == 0)
	  bb_tmp = bb_e1;
	else
	  bb_tmp = (bb_else_begin ? bb_else_begin : bb_merge);

	cfg->Add_label_with_wn(bb_tmp);
	WN_label_number(wn_tmp) = bb_tmp->Labnam();

	wn_tmp = bb5_2->Branch_wn();
	if (i == 0)
	  bb_tmp = bb_e2;
	else
	  bb_tmp = (bb_then_begin ? bb_then_begin : bb_merge_c);

	cfg->Add_label_with_wn(bb_tmp);
	WN_label_number(wn_tmp) = bb_tmp->Labnam();

	// Fix <-bb_else_begin, and <-bb_then_begin
	if (bb_then_begin)
	  bb_then_begin->Replace_pred(bb_head, bb5_2);
	
	if (bb_else_begin)
	  bb_else_begin->Replace_pred(bb_head, bb5_1);

	// Fix <-new_1 and <-new_2
	bb_list_new = CXX_NEW(BB_LIST(bb_e1), pool);
	bb_new1->Set_pred(bb_list_new);
	if (i == 0)
	  bb_new1->Append_pred(bb_merge, pool);
	else
	  bb_new1->Prepend_pred(bb_merge, pool);

	bb_list_new = CXX_NEW(BB_LIST(bb_e2), pool);
	bb_new2->Set_pred(bb_list_new);
	if (i == 0)
	  bb_new2->Append_pred(bb_merge_c, pool);
	else
	  bb_new2->Prepend_pred(bb_merge_c, pool);

	// Fix <-e1->  and <-e2->
	bb_list_new = CXX_NEW(BB_LIST(bb5_1), pool);
	bb_e1->Set_pred(bb_list_new);
	bb_list_new = CXX_NEW(BB_LIST(bb_new1), pool);
	bb_e1->Set_succ(bb_list_new);
	bb_list_new = CXX_NEW(BB_LIST(bb5_2), pool);
	bb_e2->Set_pred(bb_list_new);
	bb_list_new = CXX_NEW(BB_LIST(bb_new2), pool);
	bb_e2->Set_succ(bb_list_new);

	// Fix new_1-> and new_2->
	bb_list_new = CXX_NEW(BB_LIST(bb_next), pool);
	bb_new1->Set_succ(bb_list_new);
	bb_list_new = CXX_NEW(BB_LIST(bb_next), pool);
	bb_new2->Set_succ(bb_list_new);

	if (cfg->Feedback()) {
	  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_next->Pred())) {
	    cfg->Feedback()->Delete_edge(bb_tmp->Id(), bb_next->Id());
	  }
	}
	
	bb_next->Remove_preds(pool);
	bb_list_new = CXX_NEW(BB_LIST(bb_new1), pool);
	bb_next->Set_pred(bb_list_new);
	bb_next->Append_pred(bb_new2, pool);

	// Fix 4<->6
	if (sc_tmp == sc_then->First_kid()) {
	  bb_head->Remove_preds(pool);
	  bb_head->Set_pred(bb5->Pred());
	  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb5->Pred())) {
	    if (bb_tmp->Is_branch_to(bb5)) {
	      WN * wn_branch = bb_tmp->Branch_wn();
	      WN_label_number(wn_branch) = bb_head->Labnam();
	    }
	    bb_tmp->Replace_succ(bb5, bb_head);
	    if (cfg->Feedback()) 
	      cfg->Feedback()->Move_edge_dest(bb_tmp->Id(), bb5->Id(), bb_head->Id());
	  }
	  bb_head->Set_prev(bb5->Prev());
	  bb5->Prev()->Set_next(bb_head);
	}
	
	bb_head->Set_next(bb5_2);
	bb5_2->Set_prev(bb_head);

	if (i == 1) {
	  bb5_2->Set_next(bb_e2);
	  bb_e2->Set_prev(bb5_2);
	  bb_tmp = bb_e2;
	}
	else
	  bb_tmp = bb5_2;

	if (bb_then_begin) {
	  bb_tmp->Set_next(bb_then_begin);
	  bb_then_begin->Set_prev(bb_tmp);
	  bb_then_end->Set_next(bb_merge_c);
	  bb_merge_c->Set_prev(bb_then_end);
	}
	else {
	  bb_tmp->Set_next(bb_merge_c);
	  bb_merge_c->Set_prev(bb_tmp);
	}

	if (i == 0) {
	  bb_merge_c->Set_next(bb_e2);
	  bb_e2->Set_prev(bb_merge_c);
	  bb_e2->Set_next(bb_new2);
	  bb_new2->Set_prev(bb_e2);
	}
	else {
	  bb_merge_c->Set_next(bb_new2);
	  bb_new2->Set_prev(bb_merge_c);
	}

	bb_new2->Set_next(bb5_1);
	bb5_1->Set_prev(bb_new2);

	if (i == 1) {
	  bb5_1->Set_next(bb_e1);
	  bb_e1->Set_prev(bb5_1);
	  bb_tmp = bb_e1;
	}
	else
	  bb_tmp = bb5_1;
	
	if (bb_else_begin) {
	  bb_tmp->Set_next(bb_else_begin);
	  bb_else_begin->Set_prev(bb_tmp);
	  bb_else_end->Set_next(bb_merge);
	  bb_merge->Set_prev(bb_else_end);
	}
	else {
	  bb_tmp->Set_next(bb_merge);
	  bb_merge->Set_prev(bb5_1);
	}

	if (i == 0) {
	  bb_merge->Set_next(bb_e1);
	  bb_e1->Set_prev(bb_merge);
	  bb_e1->Set_next(bb_new1);
	  bb_new1->Set_prev(bb_e1);
	}
	else {
	  bb_merge->Set_next(bb_new1);
	  bb_new1->Set_prev(bb_merge);
	}

	bb_new1->Set_next(bb_next);
	bb_next->Set_prev(bb_new1);

	info = bb_head->Ifinfo();
	info->Set_merge(bb_next);
	info->Set_then(bb5_2);
	info->Set_else(bb5_1);
	
	info = bb5_1->Ifinfo();
	info->Set_cond(bb5_1);
	info->Set_merge(bb_new1);
	bb_tmp = bb_else_begin ? bb_else_begin : bb_merge;
	info->Set_then((i == 0) ? bb_tmp : bb_e1);
	info->Set_else((i == 0) ? bb_e1 : bb_tmp);
	
	info = bb5_2->Ifinfo();
	info->Set_cond(bb5_2);
	info->Set_merge(bb_new2);
	bb_tmp = bb_then_begin ? bb_then_begin : bb_merge_c;
	info->Set_then((i == 0) ? bb_tmp : bb_e2);
	info->Set_else((i == 0) ? bb_e2 : bb_tmp);

	if (cfg->Feedback()) {
	  FB_FREQ freq1;
	  FB_FREQ freq2;
	  BB_IFINFO * ifinfo;
	  BB_NODE * bb_then;
	  BB_NODE * bb_else;

	  cfg->Feedback()->Add_node(bb_new1->Id());
	  cfg->Feedback()->Add_node(bb_new2->Id());
	  cfg->Feedback()->Add_node(bb_e1->Id());
	  cfg->Feedback()->Add_node(bb_e2->Id());
	  
	  if ((i == 0) && bb_next->Ifinfo()) {
	    ifinfo = bb_next->Ifinfo();
	    bb_then = ifinfo->Then();
	    bb_else = ifinfo->Else();
	    freq1 = cfg->Feedback()->Get_edge_freq(bb_next->Id(), bb_else->Id());
	    freq2 = cfg->Feedback()->Get_edge_freq(bb_next->Id(), bb_then->Id());
	  }
	  else if ((i == 1) && bb_last) {
	    ifinfo = bb_last->Ifinfo();
	    bb_then = ifinfo->Then();
	    bb_else = ifinfo->Else();
	    freq1 = cfg->Feedback()->Get_edge_freq(bb_else->Id(), bb_else->Ifinfo()->Then()->Id());
	    freq2 = cfg->Feedback()->Get_edge_freq(bb_then->Id(), bb_then->Ifinfo()->Then()->Id());
	  }
	  else {
	    freq1 = cfg->Feedback()->Get_node_freq_out(bb_head->Id());
	    freq2 = freq_total - freq1;
	    freq1 = freq2/2;
	    freq2 = freq1;
	  }

	  // Delete 5->bb_head
	  cfg->Feedback()->Delete_edge(bb5->Id(), bb_head->Id());
	  
	  // Add 5_1->e1, 5_2->e2, e1->new_1, e2->new_2
	  cfg->Feedback()->Add_edge(bb5_1->Id(), bb_e1->Id(), (i == 0) ? br_edge_type : ft_edge_type, freq1);
	  cfg->Feedback()->Add_edge(bb5_2->Id(), bb_e2->Id(), (i == 0) ? br_edge_type : ft_edge_type, freq2);
	  cfg->Feedback()->Add_edge(bb_e1->Id(), bb_new1->Id(), FB_EDGE_OUTGOING, freq1);
	  cfg->Feedback()->Add_edge(bb_e2->Id(), bb_new2->Id(), FB_EDGE_OUTGOING, freq2);

	  // Add 5_2->bb_then_begin, delete 6->bb_then_begin, 
	  // change bb_then_end->bb_merge to bb_then_end->bb_merge_c,
	  // Add bb_merge_c->new_2
	  if (bb_then_begin) {
	    freq = cfg->Feedback()->Get_edge_freq(bb_head->Id(), bb_then_begin->Id());
	    cfg->Feedback()->Add_edge(bb5_2->Id(), bb_then_begin->Id(), (i == 0) ? ft_edge_type : br_edge_type, freq);
	    cfg->Feedback()->Delete_edge(bb_head->Id(), bb_then_begin->Id());
	    cfg->Feedback()->Move_edge_dest(bb_then_end->Id(), bb_merge->Id(), bb_merge_c->Id());
	  }
	  else {
	    freq = cfg->Feedback()->Get_edge_freq(bb_head->Id(), bb_merge->Id());
	    cfg->Feedback()->Add_edge(bb5_2->Id(), bb_merge_c->Id(), (i == 0) ? ft_edge_type : br_edge_type, freq);
	    cfg->Feedback()->Delete_edge(bb_head->Id(), bb_merge->Id());
	  }

	  cfg->Feedback()->Add_edge(bb_merge_c->Id(), bb_new2->Id(), FB_EDGE_OUTGOING, freq);

	  // Add 6->5_2
	  cfg->Feedback()->Add_edge(bb_head->Id(), bb5_2->Id(), ft_edge_type, freq + freq2);
	  // Add new2->bb_next
	  cfg->Feedback()->Add_edge(bb_new2->Id(), bb_next->Id(), FB_EDGE_OUTGOING, freq + freq2);
	  
	  // Add 5_1->bb_else_begin, delete 6->bb_else_begin,
	  // add  bb_merge->new_1.
	  if (bb_else_begin) {
	    freq = cfg->Feedback()->Get_edge_freq(bb_head->Id(), bb_else_begin->Id());
	    cfg->Feedback()->Add_edge(bb5_1->Id(), bb_else_begin->Id(), (i == 0)? ft_edge_type : br_edge_type, freq);
	    cfg->Feedback()->Delete_edge(bb_head->Id(), bb_else_begin->Id());
	  }
	  else {
	    freq = cfg->Feedback()->Get_edge_freq(bb_head->Id(), bb_merge->Id());
	    cfg->Feedback()->Add_edge(bb5_1->Id(), bb_merge->Id(), (i == 0) ? ft_edge_type : br_edge_type, freq);
	    cfg->Feedback()->Delete_edge(bb_head->Id(), bb_merge->Id());
	  }

	  // bb_merge->bb65
	  cfg->Feedback()->Delete_edge(bb_merge->Id(), bb65->Id());
	  cfg->Feedback()->Add_edge(bb_merge->Id(), bb_new1->Id(), FB_EDGE_OUTGOING, freq);

	  // Add 6->5_1
	  cfg->Feedback()->Add_edge(bb_head->Id(), bb5_1->Id(), br_edge_type, freq + freq1);
	  // Add new_1->bb_next
	  cfg->Feedback()->Add_edge(bb_new1->Id(), bb_next->Id(), FB_EDGE_OUTGOING, freq + freq1);

	  // Delete 5->bb_head
	  cfg->Feedback()->Delete_edge(bb5->Id(), bb_head->Id());
	}
         
        // From:
	//                   SC10(THEN)
	//         /             |          \
	//      SC11(IF,BB6)    SC65(BB38)  ...
        //    /             \
        // SC12(THEN)        SC14(ELSE)            
        //   |       \     /           \
        // SC13(BB7) ...  SC15(IF,BB8) SC64(BB37)
        //
        // To:
	//                                                SC10(THEN)
        //                                   /                 x         \
        //                                  /                  x          \
        //                                 /                   x           \
        //                               SC11(IF,BB6)         SC65(BB38)   ...
        //                        /                      \      	  
        //                       /                        \
        //                      /                          \
        //             SC12(THEN) <- sc_cur                  SC14(ELSE)	  <- sc_cur
        //       /              \                           /            \
        //	/		 \                         /              \
        // SCNEW(IF,BB5_2)   SCNEW(BBnew2)                SCNEW(IF,BB5_1) SCNEW(BBnew1)
        //   /             \                               /             \
        //  /               \                             /               \
        // SCNEW(THEN)     SCNEW(ELSE)                  SCNEW(THEN)        SCNEW(ELSE)
        // /        |  \            \               /         |         \            \
        ///         |   \            \             /          |          \            \
        //SC13(BB7) ... SCNEW(BB38') SCNEW(BBe2) SC15(IF,BB8) SC64(BB37) SCNEW(BB38) SCNEW(BBe1)
        {		       
	  SC_NODE * sc11 = sc_tmp;
	  SC_NODE * sc65 = sc11->Next_sibling();
	  int j ;

	  sc_tmp = sc65->Next_sibling();
	  sc65->Unlink();
	  sc65->Delete();	  

	  for (j = 0; j < 2; j++) {
	    SC_NODE * sc_new_bb_5 = cfg->Create_sc(SC_IF);
	    SC_NODE * sc_new_bb_new = cfg->Create_sc(SC_BLOCK);
	    SC_NODE * sc_new_then = cfg->Create_sc(SC_THEN);
	    SC_NODE * sc_new_else = cfg->Create_sc(SC_ELSE);
	    SC_NODE * sc_new_bb_38 = cfg->Create_sc(SC_BLOCK);
	    SC_NODE * sc_new_bb_e = cfg->Create_sc(SC_BLOCK);
	    SC_NODE * sc_cur = (j == 0) ? sc11->Find_kid_of_type(SC_THEN) :
	      sc11->Find_kid_of_type(SC_ELSE);
	    BB_NODE * bb_5 = (j == 0) ? bb5_2 : bb5_1;
	    BB_NODE * bb_new = (j == 0) ? bb_new2 : bb_new1;
	    BB_NODE * bb_38 = (j == 0) ? bb_merge_c : bb_merge;
	    BB_NODE * bb_e = (j == 0) ? bb_e2 : bb_e1;
	    
	    sc_new_bb_5->Set_bb_rep(bb_5);
	    sc_new_bb_new->Append_bbs(bb_new);
	    sc_new_bb_38->Append_bbs(bb_38);
	    sc_new_bb_e->Append_bbs(bb_e);

	    SC_NODE * sc_kid;
	    SC_NODE * sc_ft = (i == 0) ? sc_new_then : sc_new_else;
	    SC_NODE * sc_nft = (i == 0) ? sc_new_else : sc_new_then;
	    FOR_ALL_ELEM(sc_kid, sc_list_iter, Init(sc_cur->Kids())) {
	      sc_kid->Set_parent(sc_ft);
	    }
	    sc_ft->Set_kids(sc_cur->Kids());
	    sc_ft->Append_kid(sc_new_bb_38);
	    sc_new_bb_38->Set_parent(sc_ft);
	    sc_nft->Append_kid(sc_new_bb_e);
	    sc_new_bb_e->Set_parent(sc_nft);
	    sc_new_bb_5->Append_kid(sc_new_then);
	    sc_new_then->Set_parent(sc_new_bb_5);
	    sc_new_bb_5->Append_kid(sc_new_else);
	    sc_new_else->Set_parent(sc_new_bb_5);
	    sc_cur->Set_kids(NULL);
	    sc_cur->Append_kid(sc_new_bb_5);
	    sc_new_bb_5->Set_parent(sc_cur);
	    sc_cur->Append_kid(sc_new_bb_new);
	    sc_new_bb_new->Set_parent(sc_cur);
	  }
	}
	
	bb_last = bb_head;
	break;
      case SC_BLOCK:
	FmtAssert(((i == 1) && (sc_tmp->Is_empty())), ("Unexpected block."));
	bb_last = sc_tmp->Last_bb();
	FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb65->Pred())) {
	  if (bb_tmp != bb_last) {
	    bb65->Remove_pred(bb_tmp, pool);
	    if (cfg->Feedback())
	      cfg->Feedback()->Delete_edge(bb_tmp->Id(), bb65->Id());
	  }
	}
	sc_tmp = sc_tmp->Next_sibling();
	break;
      default:
	FmtAssert(FALSE, ("TODO"));
	sc_tmp = sc_tmp->Next_sibling();
      }
    }
  }

  // From:
  //                         SC8
  //                   /            \
  //         SC9(IF, BB5)            SC112(BB65)
  //      /               \
  // 	 SC10(THEN)       SC66(ELSE)
  //     |                |
  //     SC11(IF,bb6)     SC73(IF,BB42)
  //
  // To:
  //                        SC8
  //            /            |            \ 				
  //        SC11(IF,bb6)  SC73(IF,BB42)  SC112(BB65)

  SC_NODE * sc9 = sc;
  SC_NODE * sc8 = sc9->Parent();
  SC_NODE * sc10 = sc9->Find_kid_of_type(SC_THEN);
  SC_NODE * sc66 = sc9->Find_kid_of_type(SC_ELSE);
  SC_NODE * sc_insert = sc9->Prev_sibling();

  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(sc66->Kids())) {
    sc10->Append_kid(sc_tmp);
  }

  sc_tmp1 = sc9->Next_sibling();
  while (sc_tmp1) {
    sc_tmp2 = sc_tmp1;
    sc10->Append_kid(sc_tmp2);
    sc_tmp1 = sc_tmp2->Next_sibling();
    sc_tmp2->Unlink();
  }

  sc9->Unlink();
  _unlink_sc->Push(sc9);
  
  if (sc_insert == NULL)
    sc8->Set_kids(sc10->Kids());

  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(sc10->Kids())) {
    if (sc_insert) {
      sc_insert->Insert_after(sc_tmp);
      sc_insert = sc_tmp;
    }
    else
      sc_tmp->Set_parent(sc8);      
  }

  sc66->Set_kids(NULL);
  sc10->Set_kids(NULL);  

  cfg->Fix_info(sc_p);

  if (sc_prev)
    cfg->Fix_info(sc_prev);

  cfg->Invalidate_and_update_aux_info(FALSE);
  cfg->Invalidate_loops();
  Inc_transform_count();

  return TRUE;
}

// Automation of proactive loop interchange for the given loop-nest.
int
PRO_LOOP_INTERCHANGE_TRANS::Nonrecursive_trans
(SC_NODE * outer_loop, SC_NODE * inner_loop)
{
  BOOL success = TRUE;
  int ret_val = 0;

  SC_NODE * cur_node = inner_loop;
  SC_NODE * parent_node = cur_node->Get_real_parent();
  SC_NODE * last_node;
  SC_NODE * sc_tmp;

  while (parent_node != outer_loop) {
    BOOL is_invariant = FALSE;

    last_node = cur_node;

    // Find the outermost if-condition that is loop invariant w.r.t. the outer loop.
    while ((parent_node != outer_loop) && Is_invariant(outer_loop, parent_node->Head(), 0)) {
      FmtAssert((parent_node->Type()  == SC_IF), ("Expect a SC_IF"));
      cur_node = parent_node;
      parent_node = cur_node->Get_real_parent();
      is_invariant = TRUE;
    }
    
    // Find the outermost if-condition that is NOT loop invariant w.r.t. the outer loop.
    if (!is_invariant) {
      while ((parent_node != outer_loop) && !Is_invariant(outer_loop, parent_node->Head(), 0)) {
	FmtAssert((parent_node->Type()  == SC_IF), ("Expect a SC_IF"));
	cur_node = parent_node;
	parent_node = cur_node->Get_real_parent();
      }
    }

    if (is_invariant) {
      if (cur_node != last_node->Get_real_parent()) {
	if ((_action & DO_TREE_HEIGHT_RED) != 0) {
	  Do_canon(outer_loop, inner_loop, HEAD_DUP | TAIL_DUP);
	  
	  // Reduce height of if-condition tree to 1.
	  if (Do_if_cond_tree_height_reduction(cur_node, last_node->Parent())) {
	    cur_node = inner_loop;
	    sc_tmp = last_node->Get_real_parent();
	    FmtAssert((sc_tmp->Type() == SC_IF), ("Expect a SC_IF"));
	    Hash_invar(sc_tmp->Head(), outer_loop);

	    if ((ret_val & DO_TREE_HEIGHT_RED) == 0)
	      ret_val |= DO_TREE_HEIGHT_RED;
	  }
	  else
	    success = FALSE;
	}
      }
      else if (parent_node == outer_loop) {
	if (outer_loop->Type() == SC_LOOP) {
	  if ((_action & DO_LOOP_UNS) != 0) {
	    Do_canon(outer_loop, inner_loop, HEAD_DUP | TAIL_DUP);
	    SC_NODE * sc_p = outer_loop->Parent();

	    // Do loop unswitching w.r.t. the outer loop and terminate.
	    if (Do_loop_unswitching(cur_node, outer_loop, FALSE)) {
	      IF_MERGE_TRANS::Top_down_trans(sc_p);	
	      
	      if ((ret_val & DO_LOOP_UNS) == 0)
		ret_val |= DO_LOOP_UNS;
	      break;
	    }
	    else
	      success = FALSE;
	  }
	}
      }
    }
    else if ((last_node->Type() == SC_IF)
	     && Is_invariant(outer_loop, last_node->Head(), 0)) {
      if ((_action & DO_IF_COND_DIS) != 0) {
	Do_canon(outer_loop, inner_loop, HEAD_DUP | TAIL_DUP);

	if (Do_if_cond_dist(last_node->Get_real_parent(), TRUE)) {
	  IF_MERGE_TRANS::Top_down_trans(outer_loop);
	  cur_node = inner_loop;

	  if ((ret_val & DO_IF_COND_DIS) == 0)
	    ret_val |= DO_IF_COND_DIS;
	}
	else
	  success = FALSE;
      }
    }
    else if (parent_node == outer_loop) {
      if ((_action & DO_REV_LOOP_UNS) != 0) {
	Do_canon(outer_loop, inner_loop, HEAD_DUP | TAIL_DUP);
	
	// Do a reversed loop unswitching for the inner loop.
	if (Do_reverse_loop_unswitching(inner_loop->Get_real_parent(), inner_loop, outer_loop)) {
	  cur_node = inner_loop;
	  
	  if ((ret_val & DO_REV_LOOP_UNS) == 0)
	    ret_val |= DO_REV_LOOP_UNS;
	}
	else
	  success = FALSE;
      }
    }

    if (!success)
      break;

    parent_node = cur_node->Get_real_parent();
  }

  return ret_val;
}


// Driver for proactive loop interchange.
void
PRO_LOOP_INTERCHANGE_TRANS::Doit(SC_NODE * sc)
{
  Init();
  Top_down_trans(sc);
  Delete();
}

// Reset/clear fields.
void
PRO_LOOP_TRANS::Clear(void)
{
  PRO_LOOP_FUSION_TRANS::Clear();
  PRO_LOOP_INTERCHANGE_TRANS::Clear();
  PRO_LOOP_EXT_TRANS::Clear();
}

// Driver to invoke extended proactive loop optimizations.
// Without this extension, proactive loop optimizations are limited to
// SC_NODEs on symmetric paths (see SC_NODE::Has_symmetric_path for
// definition).  The extension allows merging of if-regions from non-symmetric
// paths by first transforming non-symmetric paths into symmetric path 
// (PRO_LOOP_EXT_TRANS::Normalize) then invoke if-merging, proactive loop fusion,
// and head/tail duplication to bring if-regions under the same parent.  The
// extension allows aggressive if-mergings and exposes dead code removal
// opportunities that follow the if-merging.
void
PRO_LOOP_TRANS::Do_ext_trans(SC_NODE * sc_root)
{
  PRO_LOOP_EXT_TRANS::Init();
  SC_NODE * sc_lcp = Normalize(sc_root);
  PRO_LOOP_EXT_TRANS::Delete();
  SC_NODE * sc_iter;

  if (sc_lcp) {
    IF_MERGE_TRANS::Top_down_trans(sc_lcp);
    sc_iter = sc_lcp;
    Set_ext_trans(EXT_TRANS_TRAVERSE | EXT_TRANS_FUSION);    
  }
  else {
    sc_iter = sc_root;
    Set_ext_trans(EXT_TRANS_FUSION);
  }
  
  int count = _transform_count;
  PRO_LOOP_FUSION_TRANS::Doit(sc_iter);

  if (_transform_count > count) {
    PRO_LOOP_EXT_TRANS::Top_down_trans(sc_iter);
    PRO_LOOP_INTERCHANGE_TRANS::Doit(sc_iter);
  }
}

// Obtain hash key for 'wn'.
UINT32
PRO_LOOP_EXT_TRANS::Get_key(WN * wn)
{
  UINT32 val = 0;
  OPERATOR opr = WN_operator(wn);

  switch (opr) {
  case OPR_INTCONST:
    return WN_const_val(wn);
  case OPR_LDID:
    return WN_aux(wn);
  default:
    ;
  }

  for (int i = 0; i < WN_kid_count(wn); i++) {
    WN * kid = WN_kid(wn, i);
    val = (val << 4) + Get_key(kid);
  }

  val += (val << 4) + (UINT32) opr;
  return val;
}

// Hash if-compare expressions in 'wn_cond'.
// Return hashed value in '*val'.
void
PRO_LOOP_EXT_TRANS::Get_val(WN * wn_cond, IF_CMP_VAL * val)
{
  OPERATOR opr = WN_operator(wn_cond);
  WN * op1;
  WN * op2;
  IF_CMP_VAL val1, val2;
  UINT32 key;
  STACK<WN *> * stk;

  IF_CMP_VAL ret_val =  _wn_to_val_num_map[wn_cond];
  if (ret_val) {
    *val = ((*val) << MAX_IF_CMP_BITS) + ret_val;
    return;
  }
  
  switch (opr) {
  case OPR_EQ:
  case OPR_NE:
  case OPR_BAND:
    key = Get_key(wn_cond);
    stk = _key_to_wn_hash[key % IF_CMP_HASH_SIZE];
    if (stk) {
      for (int i = 0; i < stk->Elements(); i++) {
	WN * wn_iter = stk->Top_nth(i);
	if ((wn_cond == wn_iter) || (WN_Simp_Compare_Trees(wn_cond, wn_iter) == 0)) {
	  ret_val = _wn_to_val_num_map[wn_iter];
	  break;
	}
      }
    }
     
    if (!ret_val && (_next_valnum < MAX_VAL_NUM)) {
      ret_val = _next_valnum;
      _wn_to_val_num_map[wn_cond] =  _next_valnum;
      _next_valnum++;
      if (!stk) 
	stk = CXX_NEW(STACK<WN *>(_pool), _pool);
      stk->Push(wn_cond);
      _key_to_wn_hash[key % IF_CMP_HASH_SIZE] = stk;
    }

    if (ret_val) 
      *val = ((*val) << MAX_IF_CMP_BITS) + ret_val;
    else
      *val = 0;
    break;

  case OPR_CAND:
    op1 = WN_kid0(wn_cond);
    op2 = WN_kid1(wn_cond);
    Get_val(op2, val);
    if (*val == 0)
      break;
    Get_val(op1, val);
    break;
  default:
    *val = 0;
  }
}

// Get inverted conditional WHIRL for 'sc_if' which is a SC_IF.
WN *
PRO_LOOP_EXT_TRANS::Get_inverted_cond(SC_NODE * sc_if)
{
  WN * wn_invert = NULL;
  
  if (sc_if->Type() == SC_IF) {
    WN * wn_cond = sc_if->Get_cond();
    if (wn_cond) {
      wn_invert = _wn_to_wn_map[wn_cond];
      if (!wn_invert) {
	wn_invert = Get_cond(sc_if, TRUE);
	if (wn_invert) {
	  _wn_to_wn_map[wn_cond] = wn_invert;
	  _wn_list->Push(wn_invert);
	}
      }
    }
  }
  return wn_invert;
}

// Hash values of loops' nesting if-conditions.
void 
PRO_LOOP_EXT_TRANS::Hash_if_conds(SC_NODE * sc)
{
  // Skip loops that are not flagged LOOP_PRE_DO since the 
  // the CFG transformation infrastructure does not support
  // these loops at this moment.
  if ((sc->Type() == SC_LOOP) 
      && sc->Loopinfo()->Is_flag_set(LOOP_PRE_DO)) {
    sc->Set_next(NULL);
    std::pair<SC_NODE *, bool> p_ret = sc->Get_nesting_if();
    SC_NODE * sc_if = p_ret.first;
    BOOL is_else = p_ret.second;
    
    IF_CMP_VAL val = 0;
    int level = 0;

    while (sc_if) {
      if (level == MAX_IF_CMP_LEVEL) {
	val = 0;
	break;
      }
      WN * wn_cond = NULL;

      if (is_else) 
	wn_cond = Get_inverted_cond(sc_if);
      else
	wn_cond = sc_if->Get_cond();

      if (wn_cond)
	Get_val(wn_cond, &val);
      
      if (val == 0) 
	break;
      
      p_ret = sc_if->Get_nesting_if();
      sc_if = p_ret.first;
      is_else = p_ret.second;
      level++;
    }
    
    if (val) {
      SC_NODE * sc_rep =  _val_to_sc_map[val];
      // Check whether duplicating 'sc' into its sibling SC_IF can make 
      // 'sc' belong to a group of loops having the same nesting if-conditions.
      if (!sc_rep) {
	// Find next SC_IF, skip empty blocks in-between.
	SC_NODE * next_if = sc->Next_sibling_of_type(SC_IF);
	if (next_if && !Has_dependency(sc, next_if->Head())) {
	  SC_NODE * sc_iter = sc->Next_sibling();
	  while (sc_iter && (sc_iter != next_if)) {
	    if (!sc_iter->Is_empty_block()) {
	      next_if = NULL;
	      break;
	    }
	    sc_iter = sc_iter->Next_sibling();
	  }

	  if (next_if) {
	    WN * wn_cond = Get_cond(next_if, FALSE);  
	    if (wn_cond) {
	      IF_CMP_VAL next_val = 0;
	      Get_val(wn_cond, &next_val);
	      if (next_val) {
		for (int i2 = 1; i2 <= level; i2++) {
		  next_val = (next_val << MAX_IF_CMP_BITS);
		}
		next_val += val;
		SC_NODE * sc_tmp =  _val_to_sc_map[next_val];
		if (sc_tmp) {
		  // Remove empty blocks between 'sc' and 'next_if'
		  SC_NODE * sc1 = sc->Next_sibling();
		  SC_NODE * sc2;
		  while (sc1 && (sc1 != next_if)) {
		    sc2 = sc1->Next_sibling();
		    if (sc1->Is_empty_block()) 
		      Remove_block(sc1);
		    sc1 = sc2;
		  }
		  
		  // Do head duplication.
		  Do_head_duplication(sc, next_if);
		  sc_rep = sc_tmp;
		  FmtAssert((sc == next_if->Find_kid_of_type(SC_THEN)->First_kid()),
			    ("Unexpect loop after head duplication."));

		}
	      }
	    }
	  }
	}
      }

      if (!sc_rep) {
	_val_to_sc_map[val] = sc;
	STACK<IF_CMP_VAL> * stk = _if_cmp_vals[level-1];
	if (!stk)
	  stk = CXX_NEW(STACK<IF_CMP_VAL>(_pool), _pool);
	stk->Push(val);
	_if_cmp_vals[level-1] = stk;
      }
      else {
	SC_NODE * sc_last = sc_rep->Last();
	sc_last->Set_next(sc);
      }
    }
  }

  if (sc->Type() != SC_LOOP) {
    // Only do it for the outermost loop at this time.
    SC_LIST_ITER kids_iter;
    SC_NODE * tmp;
    FOR_ALL_ELEM(tmp, kids_iter, Init(sc->Kids())) {
      Hash_if_conds(tmp);
    }
  }
}

// Initialize scratch data used for the PRO_LOOP_EXT_TRANS sub-object.
void
PRO_LOOP_EXT_TRANS::Init()
{
  _next_valnum = 1;
  _wn_list = CXX_NEW(STACK<WN *>(_pool), _pool);

  for (int i = 0; i < IF_CMP_HASH_SIZE; i++)
    _key_to_wn_hash[i] = NULL;

  for (int i = 0; i < MAX_IF_CMP_LEVEL; i++)
    _if_cmp_vals[i] = NULL;

  _unlink_sc = CXX_NEW(STACK<SC_NODE *>(_pool), _pool); 
}

// Clear fields.
void
PRO_LOOP_EXT_TRANS::Clear()
{
  for (int i = 0; i < IF_CMP_HASH_SIZE; i++) 
    _key_to_wn_hash[i] = NULL;

  for (int i = 0; i < MAX_IF_CMP_LEVEL; i++) 
    _if_cmp_vals[i] = NULL;
  
  _wn_list = NULL;
  
}

// Delete sratch data used for the PRO_LOOP_EXT_TRANS sub-object.
void
PRO_LOOP_EXT_TRANS::Delete()
{
  for (int i = 0; i < IF_CMP_HASH_SIZE; i++) {
    STACK<WN *> * stk = _key_to_wn_hash[i];
    if (stk)
      CXX_DELETE(stk,_pool);
    _key_to_wn_hash[i] = NULL;
  }
  for (int i = 0; i < MAX_IF_CMP_LEVEL; i++) {
    STACK<IF_CMP_VAL> * stk = _if_cmp_vals[i];
    if (stk)
      CXX_DELETE(stk,_pool);
    _if_cmp_vals[i] = NULL;
  }

  if (_wn_list) {
    while (!_wn_list->Is_Empty()) {
      WN * wn = _wn_list->Pop();
      WN_Delete(wn);
    }
    CXX_DELETE(_wn_list, _pool);
  }
  _wn_list = NULL;
  Delete_unlink_sc();
}

// Remove adjacent loops in the link threaded by the 'next' field.
void
PRO_LOOP_EXT_TRANS::Remove_adjacent_loops(SC_NODE * sc)
{
  SC_NODE * sc_last = sc;

  while (sc_last) {
    SC_NODE * sc_iter = sc_last;
    SC_NODE * sc_next = sc_iter->Next();
  
    while (sc_next && (sc_next == sc_iter->Next_sibling())) {
      sc_iter = sc_next;
      sc_next = sc_next->Next();
    }
    if (sc_last != sc_iter) {
      sc_last->Set_next(sc_next);
      sc_last = sc_next;
    }
    else
      sc_last = sc_iter->Next();
  }
}

// Iterate nodes in the list threaded by the 'next' field of 'sc', check whether there
// exists a pair of nodes whose outermost-nesting if-conditions are adjacent to each other.
// of 'sc'.
BOOL
PRO_LOOP_EXT_TRANS::Has_adjacent_if(SC_NODE * sc)
{
  std::pair<SC_NODE *, int> p_ret;
  while (sc) {
    SC_NODE * sc_next = sc->Next();
    p_ret = sc->Get_outermost_nesting_if();
    SC_NODE * sc_outer1 = p_ret.first;
  
    if (sc_next) {
      p_ret = sc_next->Get_outermost_nesting_if();
      SC_NODE * sc_outer2 = p_ret.first;
      if (sc_outer1->Next_sibling() == sc_outer2)
	return TRUE;
    }
    sc = sc_next;
  }

  return FALSE;
}

// Iterate nodes in the list threaded by the 'next' field of 'sc', check whether
// their nesting if-conditions have the same level, if yes, also return the level.
std::pair<bool, int>
PRO_LOOP_EXT_TRANS::Has_same_nesting_level(SC_NODE * sc)
{
  int level1 ;
  std::pair<SC_NODE *, int> p_ret = sc->Get_outermost_nesting_if();
  level1 = p_ret.second;

  SC_NODE * sc_next = sc->Next();
  while (sc_next) {
    int level2;
    p_ret = sc_next->Get_outermost_nesting_if();
    level2 = p_ret.second;

    if (level1 != level2) {
      return std::pair<bool, int> (FALSE, 0);
    }
    sc_next = sc_next->Next();
  }
  
  return std::pair<bool, int>(TRUE, level1);
}

// Given 'va', get number of nesting levels it represents.
int
PRO_LOOP_EXT_TRANS::Get_level_from_val(IF_CMP_VAL val)
{
  int level = 0;

  while (val) {
    val = val >> MAX_IF_CMP_BITS;
    level++;
  }

  return level;
}

// Decode 'action' to find the portion of if-condition tree for the tree-height-reduction.
void
PRO_LOOP_EXT_TRANS::Decode_action(UINT64 action, int * p_outer, int * p_inner)
{
  int level = 1;
  int l_outer = 0;
  int l_inner = 0;
  
  while (action) {
    if (action & 1) {
      if (!l_outer)
	l_outer = level;
      l_inner = level;
    }
    else {
      if (l_outer) {
	// Abort if multiple portions need the transformation.
	l_outer = 0;
	l_inner = 0;
	break;
      }
    }
    action >>=  1;
    level++;
  }

  *p_outer = l_outer;
  *p_inner = l_inner;
}

// Find a pair of candidates among the child nodes of 'sc' to invoke
// extended transformations.  Such candidates satisfy the following criteria:
// 1. The type is either a SC_IF or a SC_LOOP.
// 2. The pair are separated by a SC_IF with an empty else-path or an empty if-path
//    and the SC_IF is a SESE.
// 3. The pair are if-merging candidates or loops with same trip counts 
// 'sc' should not have any immediate child having SC_COMPGOTO type.
// If found, return the SC_IF node between the candidates.

SC_NODE *
PRO_LOOP_EXT_TRANS::Find_cand(SC_NODE * sc, SC_NODE ** cand1, SC_NODE ** cand2)
{
  *cand1 = NULL;
  *cand2 = NULL;

  if (sc->Find_kid_of_type(SC_COMPGOTO))
    return NULL;

  CFG * cfg = Get_cu()->Cfg();
  SC_LIST_ITER kids_iter;
  SC_NODE * tmp;
  
  FOR_ALL_ELEM(tmp, kids_iter, Init(sc->Kids())) {
    SC_TYPE type = tmp->Type();

    if ((type != SC_IF) 
	|| (!tmp->First_kid()->Is_empty()
	    && !tmp->Last_kid()->Is_empty())
	|| !tmp->Is_sese())
      continue;

    SC_NODE * prev = tmp->Prev_sibling();
    while (prev && prev->Is_empty_block()) {
      prev = prev->Prev_sibling();
    }

    SC_NODE * next = tmp->Next_sibling();
    while (next && next->Is_empty_block()) {
      next = next->Next_sibling();
    }

    if (!prev || !next || (prev->Type() != next->Type())
	|| !prev->Is_sese() || !next->Is_sese())
      continue;

    IF_MERGE_PASS s_pass = Get_pass();
    switch (next->Type()) {
    case SC_IF:
      Set_pass(PASS_EXT);
      if (IF_MERGE_TRANS::Is_candidate(prev, next, TRUE)) {
	*cand1 = prev;
	*cand2 = next;
	Set_pass(s_pass);
	return tmp;
      }
      else {
	WN * cond1 = prev->Get_cond(); 
	WN * cond2 = next->Get_cond();
	if ((WN_operator(cond1) == WN_operator(cond2))
	    && (WN_kid_count(cond1) == 2)
	    && (WN_Simp_Compare_Trees(WN_kid1(cond1), WN_kid1(cond2)) == 0)
	    && ((WN_operator(cond1) == OPR_NE) || (WN_operator(cond1) == OPR_EQ))
	    && (WN_operator(WN_kid1(cond1)) == OPR_INTCONST)
	    && (WN_const_val(WN_kid1(cond1)) == 0)) {
	  cond1 = WN_kid0(cond1);
	  cond2 = WN_kid0(cond2);
	  // match patterns like: "(a bitop (1 << cnt)) == 0" or
	  // "(a bitop (1 << cnt)) != 0", 
	  // where 'a' is a 4-byte, one of 'cond1' and 'cond2' is a 8 byte, and 
	  // the other one is a 4-byte.
	  if ((WN_operator(cond1) == WN_operator(cond2))
	      && WN_is_bit_op(cond1)) {
	    SC_NODE * sc_l = NULL;
	    SC_NODE * sc_s = NULL;

	    if (MTYPE_byte_size(WN_rtype(cond1)) == 8)
	      sc_l = prev;
	    else if (MTYPE_byte_size(WN_rtype(cond2)) == 8)
	      sc_l = next;

	    if (MTYPE_byte_size(WN_rtype(cond1)) == 4)
	      sc_s = prev;
	    else if (MTYPE_byte_size(WN_rtype(cond2)) == 4)
	      sc_s = next;

	    if (sc_l && sc_s) {
	      WN * op1 = WN_kid0(cond1);
	      WN * op2 = WN_kid0(cond2);
	      op1 = (WN_operator(op1) == OPR_CVT) ? WN_kid0(op1) : op1;
	      op2 = (WN_operator(op2) == OPR_CVT) ? WN_kid0(op2) : op2;
	      if ((WN_Simp_Compare_Trees(op1, op2) == 0)
		  && (MTYPE_byte_size(WN_desc(op1)) == 4)) {
		op1 = WN_kid1(cond1);
		op2 = WN_kid1(cond2);
		if (WN_is_power_of_2(op1) && WN_is_power_of_2(op2)
		    && (WN_operator(op1) == OPR_SHL)) {
		  op1 = WN_get_bit_from_expr(op1);
		  op2 = WN_get_bit_from_expr(op2);
		  // Create a new SC_IF with comparision expression "if (cnt < 32)"
		  // and wrap it around 'prev' and 'next'.
		  if (op1 && op2 && (WN_Simp_Compare_Trees(op1, op2) == 0)
		      && !Has_dependency(prev, op1)
		      && !Has_dependency(next, op2)) {
		    for (int i = 0; i < 2; i++) {
		      // Create a comparison expression 'if (cnt < 32)'.
		      WN * wn_tmp = WN_COPY_Tree_With_Map((i == 0) ? op1 : op2);
		      wn_tmp = WN_CreateExp2(OPR_LT, MTYPE_I4, MTYPE_I4, wn_tmp,
					     WN_CreateIntconst(OPR_INTCONST, MTYPE_I4, MTYPE_V, 32));
		      // Create a CFG block to host 'wn_tmp'.
		      BB_NODE * bb_new = NULL;
		      SC_NODE * sc_cur = ( i == 0) ? prev : next;
		      cfg->Clone_bbs(sc_cur->Head(), sc_cur->Head(), &bb_new, &bb_new, TRUE, 1.0);
		      WN * wn_branch = bb_new->Laststmt();
		      WN_kid0(wn_branch) = wn_tmp;
		      // Insert a new if-region before 'prev'.
		      SC_NODE * sc_tmp = cfg->Insert_if_before(sc_cur, bb_new);
		      Do_tail_duplication(sc_cur, sc_tmp);
		      if (i == 0)
			*cand1 = sc_tmp;
		      else
			*cand2 = sc_tmp;

		      if (sc_cur == sc_l) {
			sc_tmp = sc_tmp->Find_kid_of_type(SC_THEN);
			sc_tmp = sc_tmp->Find_kid_of_type(SC_IF);
			FmtAssert(sc_tmp, ("Expect a SC_IF"));
			WN * branch_l = sc_tmp->Head()->Branch_wn();
			WN * branch_s = sc_s->Head()->Branch_wn();
			WN_kid0(branch_l) = WN_COPY_Tree_With_Map(WN_kid0(branch_s));
		      }
		    }
		    Set_pass(s_pass);
		    return tmp;
		  }
		}
	      }
	    }
	  }
	}
      }

      Set_pass(s_pass);
      break;
    case  SC_LOOP:
      if (Check_index(prev) && Check_index(next)
	  && Check_iteration(prev, SC_LP_START, prev)
	  && Check_iteration(prev, SC_LP_COND, prev)
	  && Check_iteration(prev, SC_LP_STEP, prev)
	  && Check_iteration(next, SC_LP_START, next)
	  && Check_iteration(next, SC_LP_COND, next)
	  && Check_iteration(next, SC_LP_STEP, next)
	  && Have_same_trip_count(prev, next)) {
	*cand1 = prev;
	*cand2 = next;
	return tmp;
      }
      break;
    default:
      ;
    }
  }

  return NULL;
}

// Given two adjacent loops 'sc1' and 'sc2' who have the same trip count, 
// shift iteration space of 'sc1' so that its loop index at the last iteration 
// is the same as the loop index of 'sc2' at its first iteration, peel the
// last iteration of 'sc1' and the first iteration of 'sc2', do if-merging
// and dead code removal on peeled iterations.  If peeded iterations can be
// optimized away, dido the peeling, if-merging and dead code removal
// on the remaining iterations until both loops are completely removed.
// Do nothing if it is impossible to remove the loops.
void
PRO_LOOP_EXT_TRANS::Shift_peel_and_remove(SC_NODE * sc1, SC_NODE * sc2)
{
  WN * upper = Get_upper_bound(sc1);
  if (!upper) 
    return;

  SC_NODE * sc_body1 = sc1->Find_kid_of_type(SC_LP_BODY);
  SC_NODE * sc_body2 = sc2->Find_kid_of_type(SC_LP_BODY);
  SC_NODE * sc_kid = sc_body1->First_kid();

  // limit it to the case that the loop body contains a single if-region.
  if (!sc_kid || (sc_kid->Type() != SC_IF)
      || (sc_kid->Next_sibling() != sc_body1->Last_kid())
      || !sc_kid->Next_sibling()->Is_empty_block())
    return;

  sc_kid = sc_body2->First_kid();

  if (!sc_kid || (sc_kid->Type() != SC_IF)
      || (sc_kid->Next_sibling() != sc_body2->Last_kid())
      || !sc_kid->Next_sibling()->Is_empty_block())
    return;

  // Find insertion point for the peeled iteration.
  sc_kid = sc1->Next_sibling();
  if (!sc_kid->Is_empty_block()
      || (sc_kid != sc2->Prev_sibling()))
    return;

  BB_NODE * bb_from = sc_kid->Last_bb();
  BB_NODE * bb_to = sc2->First_bb();

  WN * index2 = Get_index_load(sc2);
  WN * wn_new = WN_CreateExp2(OPR_SUB, MTYPE_I4, MTYPE_V, upper, index2);

  // copy SC_IF in 'sc1' and insert it between 'bb_from' and 'bb_to'.
  CFG * cfg = _cu->Cfg();
  MEM_POOL * pool = cfg->Mem_pool();
  sc_kid = sc_body1->First_kid();
  SC_NODE * sc_merge = NULL;
  SC_NODE * sc_new = cfg->Clone_sc(sc_kid, TRUE, 1.0, &sc_merge);
  BB_NODE * new_entry = sc_new->Head();
  BB_NODE * new_exit = sc_new->Merge();
  SC_NODE * sc_if1 = sc_new;

  Insert_region(new_entry, new_exit, bb_from, bb_to, pool);
  sc2->Insert_before(sc_new);
  sc2->Insert_before(sc_merge);

  bb_from = new_exit;
  sc_kid = sc_body2->First_kid();
  sc_merge = NULL;
  sc_new = cfg->Clone_sc(sc_kid, TRUE, 1.0, &sc_merge);
  SC_NODE * sc_if2 = sc_new;
  new_entry = sc_new->Head();
  new_exit = sc_new->Merge();

  Insert_region(new_entry, new_exit, bb_from, bb_to, pool);
  sc2->Insert_before(sc_new);
  sc2->Insert_before(sc_merge);

  // Replace loop index of sc_if1.
  Replace_wn(sc_if1, Get_index_load(sc1), wn_new);

  // If-merging
  sc_new = Do_merge(sc_if1, sc_if2, TRUE);
  if (sc_new) {
    _current_scope = sc2;
    IF_MERGE_TRANS::Top_down_trans(sc_new->First_kid());
    IF_MERGE_TRANS::Top_down_trans(sc_new->Last_kid());
    _current_scope = NULL;
  }
}

// Do top-down extended transformations for the SC tree rooted at the given SC_NODE.
// CFG, LOOPs and SC tree are updated upon exit of this routine.
void
PRO_LOOP_EXT_TRANS::Top_down_trans(SC_NODE * sc)
{
  SC_LIST_ITER kids_iter;
  SC_NODE * tmp;
  SC_NODE * cand1 = NULL;
  SC_NODE * cand2 = NULL;
  SC_TYPE type = sc->Type();

  // Avoid COMPGOTO complication.
  if (sc->Find_kid_of_type(SC_COMPGOTO))
    return;

  if (type == SC_LP_BODY) {
    SC_NODE * sc_if = Find_cand(sc, &cand1, &cand2);
    SC_NODE * sc_no_alias = sc_if;
    if (sc_if) {
      // Check whether sc_if's head has dependency on its previous siblings.
      SC_NODE * sc_prev = sc_if->Prev_sibling();
      while (sc_prev && sc_no_alias) {
	if (sc_prev->Type() == SC_LOOP)
	  _current_scope = sc_prev;
	Infer_val_range(sc_if, sc_if->Next_sibling());	
	if (Has_dependency(sc_prev, sc_if->Head())) {
	  sc_no_alias = NULL;
	}
	if (sc_prev->Type() == SC_LOOP)
	  _current_scope = NULL;
	Delete_val_range_maps();	
	sc_prev = sc_prev->Prev_sibling();
      }
    }
    
    while (sc_if) {
      if (!cand1->Clonable(FALSE) || !cand2->Clonable(FALSE))
	return;

      if ((sc_if != sc_no_alias) 
	  && Has_dependency(cand1, sc_if))
	return;
      
      SC_NODE * sc_iter = cand1->Next_sibling();
      while (sc_iter && (sc_iter != cand2)) {
	SC_NODE * sc_next = sc_iter->Next_sibling();
	if (sc_iter->Is_empty_block())
	  Remove_block(sc_iter);
	sc_iter = sc_next;
      }

      // If cand1 and cand2 are loops, find the empty then-path or
      // the empty else-path.
      sc_iter = NULL;
      if ((cand1->Type() == SC_LOOP) 
	  && (cand2->Type() == SC_LOOP)) {
	if (sc_if->First_kid()->Is_empty())
	  sc_iter = sc_if->First_kid();
	else if (sc_if->Last_kid()->Is_empty())
	  sc_iter = sc_if->Last_kid();
      }

      Do_head_duplication(cand1, sc_if);
      Do_tail_duplication(cand2, sc_if);

      IF_MERGE_TRANS::Top_down_trans(sc_if->First_kid());
      IF_MERGE_TRANS::Top_down_trans(sc_if->Last_kid());

      if (sc_iter) {
	SC_NODE * sc1 = sc_iter->Find_kid_of_type(SC_LOOP);
	SC_NODE * sc2 = sc1->Next_sibling_of_type(SC_LOOP);
	if (sc1 && sc2 && !sc1->Has_same_loop_struct(sc2)) {
	  Shift_peel_and_remove(sc1, sc2);
	  Bottom_up_prune(sc_if);
	  // If there are only empty blocks left betweeen 'sc1' and 'sc2'.
	  // remove 'sc1' and 'sc2'. Otherwise remove nodes between 'sc1' 
	  // and 'sc2'.
	  BOOL remove_loops = TRUE;
	  BOOL remove_peels = FALSE;
	  SC_NODE * sc_iter = sc1->Next_sibling();
	  while (sc_iter && (sc_iter != sc2)) {
	    if (!sc_iter->Is_empty_block()) {
	      remove_peels = TRUE;
	    }
	    sc_iter = sc_iter->Next_sibling();
	  }

	  if (remove_loops) {
	    Remove_node(sc1);
	    Remove_node(sc2);
	  }
	  else if (remove_peels) 
	    Remove_peel(sc1, sc2);
	}
      }

      Bottom_up_prune(sc_if);
      sc_if = Find_cand(sc, &cand1, &cand2);
    }
  }
  else if (type == SC_IF) {
    if (Bottom_up_prune(sc))
      return;
  }

  SC_NODE * sc_iter = sc->First_kid();
  while (sc_iter) {
    SC_NODE * sc_next = sc_iter->Next_sibling();
    this->Top_down_trans(sc_iter);
    sc_iter = sc_next;
  }
}

// Do normalization in lock-step for nesting if-conditions of 'sc1' and 'sc2'.
// 'action' gives the portion of if-condition tree to apply tree-height-reduction.
void
PRO_LOOP_EXT_TRANS::Do_lock_step_normalize(SC_NODE * sc1, SC_NODE * sc2, UINT64 action)
{
  SC_NODE * sc_lcp = sc1->Find_lcp(sc2);
  SC_NODE * sc_parent = sc_lcp->Parent();

  if (!Is_invariant(sc1, sc_lcp->Head(), 0)
      || !Is_invariant(sc2, sc_lcp->Head(), 0))
    return;

  if (_trace)
    printf("\n\t Lock-step normalization (SC%d,SC%d)\n", sc1->Id(), sc2->Id());

  int l_outer = 0;
  int l_inner = 0;
  Decode_action(action, &l_outer, &l_inner);
  
  if (l_outer && l_inner) {
    // Find the beginning and ending SC_IFs for the tree-height-reduction.
    SC_NODE * if1_inner = sc1->Get_node_at_dist(sc_lcp, l_inner);
    SC_NODE * if2_inner = sc2->Get_node_at_dist(sc_lcp, l_inner);

    SC_NODE * if1_outer = sc1->Get_node_at_dist(sc_lcp, l_outer);
    if (if1_outer)
      if1_outer = if1_outer->Parent();

    SC_NODE * if2_outer = sc2->Get_node_at_dist(sc_lcp, l_outer);
    if (if2_outer)
      if2_outer = if2_outer->Parent();
    
    FmtAssert(if1_inner && if1_outer, ("If-cond candidate not found."));

    // Do tree-height reduction.
    if (!Do_if_cond_tree_height_reduction(if1_outer, if1_inner))
      return;
    if (!Do_if_cond_tree_height_reduction(if2_outer, if2_inner))
      return;
  }

  // Do recursive if-cond distribution and if-merging.
  while (sc_lcp) {
    SC_NODE * sc_tmp1 = sc1->Get_nesting_if(sc_lcp);
    SC_NODE * sc_tmp2 = sc2->Get_nesting_if(sc_lcp);    

    if (!sc_tmp1 && !sc_tmp2) {
      if (!Do_reverse_loop_unswitching(sc_lcp, sc2, NULL))
	break;
      Do_reverse_loop_unswitching(sc_lcp, sc1, NULL);
      break;
    }
    
    if (!Do_if_cond_dist(sc_lcp, TRUE))
      return;

    if (sc_tmp1->Next_sibling_of_type(SC_IF) == sc_tmp2) {
      sc_tmp1 = Do_merge(sc_tmp1, sc_tmp2, FALSE);
      IF_MERGE_TRANS::Top_down_trans(sc_tmp1);
      Do_canon(sc_tmp1, sc1, HEAD_DUP | TAIL_DUP);
      Do_canon(sc_tmp1, sc2, HEAD_DUP | TAIL_DUP);
      sc_lcp = sc1->Find_lcp(sc2);
      FmtAssert(sc_lcp->Type() == SC_IF, ("Unexpected SC type"));
    }
  }
}

// Do normalization for nesting if-conditions of 'sc'.
// 'action' gives the portion of if-condition tree to apply tree-height-reduction.
void
PRO_LOOP_EXT_TRANS::Do_normalize(SC_NODE * sc, UINT64 action)
{
  std::pair<SC_NODE *, int> p_ret = sc->Get_outermost_nesting_if();
  SC_NODE * sc_if = p_ret.first;
  if (!Is_invariant(sc, sc_if->Head(), 0))
    return;

  if (_trace)
    printf("\n\t Normalization (SC%d)\n", sc->Id());

  int l_outer = 0;
  int l_inner = 0;
  Decode_action(action, &l_outer, &l_inner);

  if (l_outer && l_inner) {
    // Find the beginning and ending SC_IFs for the tree-height-reduction.
    SC_NODE * if_inner = sc->Get_node_at_dist(sc_if, l_inner);
    SC_NODE * if_outer = sc->Get_node_at_dist(sc_if, l_outer);
    if (if_outer)
      if_outer = if_outer->Parent();
    
    // Do tree-height reduction.
    if (!Do_if_cond_tree_height_reduction(if_outer, if_inner))
      return;
  }

  // Do recursive if-cond distribution and if-merging.
  int dist = 1;
  while (sc_if) {
    SC_NODE * sc_tmp = sc->Get_nesting_if(sc_if);

    if (!sc_tmp) {
      Do_reverse_loop_unswitching(sc_if, sc, NULL);
      break;
    }

    if (!Do_if_cond_dist(sc_if, TRUE))
      return;

    p_ret = sc->Get_outermost_nesting_if();
    sc_if = p_ret.first;
    Do_canon(sc_if, sc, HEAD_DUP | TAIL_DUP);

    sc_if = sc->Get_node_at_dist(sc_if, dist);
    if (sc_if)
      sc_if = sc_if->Parent();
    dist++;
  }
}

// In the nesting if-condition of 'sc', identify the portion of tree that was tree-height reduced.
UINT64
PRO_LOOP_EXT_TRANS::Encode_tree_height_reduction(SC_NODE * sc)
{
  std::pair<SC_NODE *, bool> p_ret = sc->Get_nesting_if();
  SC_NODE * sc_iter = p_ret.first;
  UINT64 word = 0;
  while (sc_iter) {
    IF_CMP_VAL val = 0;
    Get_val(sc_iter->Get_cond(), &val);
    int shift_cnt = 0;
    while (val) {
      val = val >> MAX_IF_CMP_BITS;
      shift_cnt++;
    }
    if (shift_cnt == 1)
      word = (word << 1);
    else {
      while (shift_cnt > 0) {
	word = (word << 1) + 1;
	shift_cnt--;
      }
    }
    p_ret = sc_iter->Get_nesting_if();
    sc_iter = p_ret.first;
  }
  return word;
}

// Query whether every SC_If node between 'inner' and 'outer' satisfies the following condition:
// 1. It is well-behaved.  
// 2. If there exists a preceding sibling, the sibling must be a SC_BLOCK or a SC_LOOP which has
//    no dependencies on all SC_IF nodes on the successor part of the path (head duplication
//    legality).  If there exists a succeeding sibling, the succeeding sibling must be a SC_BLOCK
//    or a SC_LOOP.
// 3. None of its sibings and immediate child has SC_COMPGOTO type.
BOOL
PRO_LOOP_EXT_TRANS::Is_candidate(SC_NODE * outer, SC_NODE * inner)
{
  std::pair<SC_NODE *, bool> p_ret = inner->Get_nesting_if();
  SC_NODE * sc_n1 = p_ret.first;
  SC_NODE * sc_iter = sc_n1;

  if (sc_iter == outer)
    return FALSE;
  
  if (outer->Find_kid_of_type(SC_COMPGOTO))
    return FALSE;
  
  while (sc_iter && (sc_iter != outer)) {
    if (!sc_iter->Is_well_behaved())
      return FALSE;

    if (sc_iter->Find_kid_of_type(SC_COMPGOTO))
      return FALSE;

    SC_NODE * sc_tmp = sc_iter->Next_sibling();
    while (sc_tmp) {
      if ((sc_tmp->Type() != SC_BLOCK)
	  && (sc_tmp->Type() != SC_LOOP))
	return FALSE;
      sc_tmp = sc_tmp->Next_sibling();
    }

    sc_tmp = sc_iter->Prev_sibling();
    while (sc_tmp) {
      if ((sc_tmp->Type() != SC_BLOCK)
	  && (sc_tmp->Type() != SC_LOOP))
	return FALSE;

      SC_NODE * sc_cur = sc_n1;
      while (sc_cur) {
	if (Has_dependency(sc_tmp, sc_cur->Head()))
	  return FALSE;
	
	if (sc_cur == sc_iter)
	  break;

	p_ret = sc_cur->Get_nesting_if();
	sc_cur = p_ret.first;
      }
      
      sc_tmp = sc_tmp->Prev_sibling();
    }
    
    p_ret = sc_iter->Get_nesting_if();
    sc_iter = p_ret.first;
  }

  return TRUE;
}

// Normalize if-conditions for the tree rooted at 'sc'.
// Return the LCP of normalized trees.
SC_NODE * 
PRO_LOOP_EXT_TRANS::Normalize(SC_NODE * sc)
{
  Hash_if_conds(sc);

  STACK<SC_NODE *> * freeze_list = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
  STACK<SC_NODE *> * if_list = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
  STACK<SC_NODE *> * loop_list = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
  STACK<IF_CMP_VAL> * val_list = CXX_NEW(STACK<IF_CMP_VAL>(_pool), _pool);
  STACK<UINT64> * action_list= CXX_NEW(STACK<UINT64>(_pool), _pool);

  // Collect unsymmetric if-condition trees that are candidates for normalization transformations.
  for (int i = MAX_IF_CMP_LEVEL - 1; i >= 0; i--) {
    STACK<IF_CMP_VAL> * stk = _if_cmp_vals[i];
    if (stk) {
      for (int j = 0; j < stk->Elements(); j++) {
	IF_CMP_VAL val = stk->Top_nth(j);
	SC_NODE * sc = _val_to_sc_map[val];
	int v_level = Get_level_from_val(val);

	if (v_level != (i + 1)) {
	  // Collect loops whose level of nesting if-conditions is the same level as 'v_level'
	  Remove_adjacent_loops(sc);
	  SC_NODE * sc_iter = sc->Next();
	  UINT64 word = Encode_tree_height_reduction(sc);
	  std::pair<SC_NODE *, int> p_level;
	  while (sc_iter) {
	    int i_level = 0;
	    p_level = sc_iter->Get_outermost_nesting_if();
	    SC_NODE * sc_if = p_level.first;
	    i_level = p_level.second;
	    if (i_level == v_level) {
	      if_list->Push(sc_if);
	      loop_list->Push(sc_iter);
	      action_list->Push(word);
	      val_list->Push(val);
	    }
	    sc_iter = sc_iter->Next();
	  }
	  
	  // Limit it to if-condition trees without compound if-compare expressions like CAND.	  
	  continue;
	}

	if (sc && !freeze_list->Contains(sc)) {
	  // Remove adjacent loops from the list linked by the 'next' field.
	  // Then check whether remaining nodes's outermost-nesting IFs are adjacent
	  // to each other.
	  Remove_adjacent_loops(sc);
	  if (!Has_adjacent_if(sc)) {
	    // Check whether removing one level of if-condition will uncover loop
	    // fusion candidates.
	    IF_CMP_VAL next_val = (val >> MAX_IF_CMP_BITS);
	    if (next_val) {
	      SC_NODE * cand = _val_to_sc_map[next_val];

	      // Limit it to the case that all nodes in the list linked by the 'next'
	      // field of 'cand' has the same nesting level.
	      if (cand) {
		std::pair<bool, int> p_level = Has_same_nesting_level(cand);
		if (p_level.first) {
		  int level = p_level.second;
		  std::pair<SC_NODE *, int> p_ret = cand->Get_outermost_nesting_if();
		  SC_NODE * outermost = p_ret.first;
		  while (sc) {
		    p_ret = sc->Get_outermost_nesting_if();
		    SC_NODE * sc_if = p_ret.first;
		    if (sc_if->Parent() == outermost->Parent()) {
		      if_list->Push(sc_if);
		      loop_list->Push(sc);
		      // Construct an action word to encode the portion of if-condition tree
		      // that needs a reduction in tree-height.
		      UINT64 word = 0;
		      if (i != level) {
			word = Encode_tree_height_reduction(cand);
		      }
		      word |= ( 1 << MAX_IF_CMP_LEVEL);
		      action_list->Push(word);
		      val_list->Push(next_val);
		      // Freeze 'cand' for transformations.
		      if (!freeze_list->Contains(cand))
			freeze_list->Push(cand);
		    }
		    sc = sc->Next();
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }

  while (!freeze_list->Is_Empty())
    freeze_list->Pop();
  CXX_DELETE(freeze_list, _pool);

  // Find buddy loops and collect work lists for the normalization.
  STACK<SC_NODE *> * sc_work_list = CXX_NEW(STACK<SC_NODE *>(_pool), _pool);
  STACK<UINT64> * action_work_list = CXX_NEW(STACK<UINT64>(_pool), _pool);
  
  while (!if_list->Is_Empty()) {
    SC_NODE * sc_if = if_list->Pop();
    SC_NODE * sc_loop = loop_list->Pop();
    UINT64 action = action_list->Pop();
    IF_CMP_VAL val = val_list->Pop();
    SC_NODE * sc_buddy = NULL;
    
    if (sc_work_list->Contains(sc_if))
      continue;

    BOOL do_dist = FALSE;
    BOOL do_tree_height = FALSE;

    if ((action >> MAX_IF_CMP_LEVEL) != 0)
      do_dist = TRUE;
    if ((action & IF_CMP_ACTION_MASK) != 0)
      do_tree_height = TRUE;

    if (!do_tree_height)
      continue;

    if (do_dist) {
      // Find buddy loop.
      for (int i = 0; i < loop_list->Elements(); i++) {
	SC_NODE * sc_iter = loop_list->Top_nth(i);
	IF_CMP_VAL val_iter = val_list->Top_nth(i);
	if ((sc_loop->Find_lcp(sc_iter) != sc_if)
	    || (val != val_iter))
	  continue;
	sc_buddy = sc_iter;
	break;
      }

      if (!sc_buddy) {
	// Either then-path or else-path must be empty.
	if (!sc_if->First_kid()->Is_empty()
	    && !sc_if->Last_kid()->Is_empty())
	  continue;
      }
    }

    sc_if->Set_next(sc_loop);
    sc_loop->Set_next(sc_buddy);
    if (sc_buddy)
      sc_buddy->Set_next(NULL);
    sc_work_list->Push(sc_if);
    action_work_list->Push(action);
  }

  CXX_DELETE(if_list, _pool);
  CXX_DELETE(loop_list, _pool);
  CXX_DELETE(action_list, _pool);
  CXX_DELETE(val_list, _pool);
  
  // Do if-condition normalizations.
  int count= 0;
  SC_NODE * sc_lcp = NULL;
  
  while (!sc_work_list->Is_Empty()) {
    SC_NODE * sc_if = sc_work_list->Pop();
    UINT64 action = action_work_list->Pop();
    SC_NODE * sc_loop = sc_if->Next();
    SC_NODE * sc_buddy = sc_loop->Next();

    Do_split_if_head(sc_if);

    if (!Is_candidate(sc_if, sc_loop))
      continue;

    if (!sc_lcp)
      sc_lcp = sc_if->Get_real_parent();
    else
      sc_lcp = sc_lcp->Find_lcp(sc_if);
    
    BOOL do_dist = FALSE;
    if ((action >> MAX_IF_CMP_LEVEL) != 0)
      do_dist = TRUE;
    
    action &= IF_CMP_ACTION_MASK;
    if (do_dist) {
      Do_canon(sc_if, sc_loop, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP);
      if (sc_buddy) {
	Do_canon(sc_if, sc_buddy, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP);
	Do_lock_step_normalize(sc_loop, sc_buddy, action);
      }
      else {
	Do_normalize(sc_loop, action);
      }
    }
    else {
      int l_outer = 0;
      int l_inner = 0;
      Decode_action(action, &l_outer, &l_inner);

      if (l_outer && l_inner) {
	FmtAssert(l_outer == 1, ("Unexpected outer nesting level")); 
	Do_canon(sc_if, sc_loop, SPLIT_IF_HEAD | HEAD_DUP | TAIL_DUP);	
	SC_NODE * sc_tmp = Do_if_cond_wrap(sc_if->Head(), sc_if, TRUE);
	SC_NODE * sc_inner = sc_loop->Get_node_at_dist(sc_if, l_inner - 1);
	Do_if_cond_tree_height_reduction(sc_if, sc_inner);
	Do_if_cond_unwrap(sc_tmp);
      }
    }
  }
  
  CXX_DELETE(sc_work_list, _pool);
  CXX_DELETE(action_work_list, _pool);

  return sc_lcp;
}

// Delete maps for value ranges.
void
CFG_TRANS::Delete_val_range_maps()
{
  _low_map.clear();
  _high_map.clear();

}

// Get a WHIRL that represents an integer constant of the given value
// from _const_wn_map.
WN *
CFG_TRANS::Get_const_wn(INT64 val)
{
  WN * wn =   _const_wn_map[val];
  
  if (!wn) {
    wn = WN_CreateIntconst(OPR_INTCONST, MTYPE_I8, MTYPE_V, val);
    _const_wn_map[val] = wn;
  }

  return wn;
}

// Set the lower bound for 'wn_key', if 'wn_key' already has a lower bound,
// tighten it if possible.
void
CFG_TRANS::Set_lo(std::map<WN *, WN *> & map, WN * wn_key, int val)
{
  WN * wn_tmp =  map[wn_key];
  int val_tmp;

  if (wn_tmp) {
    std::pair<bool,int> p_val = WN_get_val(wn_key, map);
    val_tmp = p_val.second;
    if (p_val.first
	&& (val_tmp > val))
      return;
  }

  Set_map(map, wn_key, Get_const_wn(val));
}

// Set the upper bound for 'wn_key', if 'wn_key' already has an upper bound,
// tighten it if possible.
void
CFG_TRANS::Set_hi(std::map<WN *, WN *> & map, WN * wn_key, int val)
{
  WN * wn_tmp = map[wn_key];
  int val_tmp;

  if (wn_tmp) {
    std::pair<bool,int> p_val = WN_get_val(wn_key, map);
    val_tmp = p_val.second;
    if (p_val.first
	&& (val_tmp < val))
      return;
  }

  Set_map(map, wn_key, Get_const_wn(val));
}

// Set up map of wn_key to wn_val.  Update _def_wn_map is necessary.
void
CFG_TRANS::Set_map(std::map<WN *, WN *> &map, WN * wn_key, WN * wn_val)
{
  map[wn_key] = wn_val;
  Add_def_wn_map(wn_key, wn_val);
}

// Infer value range for the given WN *.  set_high and set_low indicates
// whether to set low boundary or high boundary respectively.
void
CFG_TRANS::Infer_val_range(WN * wn, BOOL set_high, BOOL set_low)
{
  OPERATOR opr = WN_operator(wn);

  if (OPERATOR_is_scalar_store(opr)) {
    WN * wn_data = WN_kid(wn, 0);

    if (set_low)
      Set_map(_low_map, wn, wn_data);

    if (set_high)
      Set_map(_high_map, wn, wn_data);
  }
  else {
    WN * op1;
    WN * op2;
    WN * wn_tmp;
    int val;
    int val2;
    std::pair<bool,int> p_val;

    switch (opr) {
    case OPR_GT:
      op1 = WN_kid(wn, 0);
      op2 = WN_kid(wn, 1);
      p_val = WN_get_val(op2, _low_map);
      val = p_val.second;

      if (p_val.first) {
	wn_tmp = Get_const_wn(val + 1);
	Set_map(_low_map, op1, wn_tmp);
	Infer_val_range(op1, TRUE, TRUE);
      }

      break;

    case OPR_GE:
      op1 = WN_kid(wn, 0);
      op2 = WN_kid(wn, 1);
      p_val = WN_get_val(op2, _low_map);
      val = p_val.second;

      if (p_val.first) {
	Set_map(_low_map, op1, op2);
	Infer_val_range(op1, TRUE, TRUE);
      }
      break;

    case OPR_LT:
      op1 = WN_kid(wn, 0);
      op2 = WN_kid(wn, 1);
      p_val = WN_get_val(op1, _low_map);
      val = p_val.second;
      
      if (p_val.first) {
	wn_tmp = Get_const_wn(val + 1);
	Set_map(_low_map, op2, wn_tmp);
	Infer_val_range(op2, TRUE, TRUE);
      }

      break;
      
    case OPR_LE:
      op1 = WN_kid(wn, 0);
      op2 = WN_kid(wn, 1);
      p_val = WN_get_val(op1, _low_map);
      val = p_val.second;

      if (p_val.first) {
	Set_map(_low_map, op2, op1);
	Infer_val_range(op2, TRUE, TRUE);
      }
      break;

    case OPR_MPY:
      op1 = WN_kid(wn, 0);
      op2 = WN_kid(wn, 1);

      if (WN_operator(op2) == OPR_INTCONST) {
	INT64 r_val = WN_const_val(op2);

	wn_tmp =  _low_map[wn];
	if (wn_tmp) {
	  p_val = WN_get_val(wn_tmp, _low_map);
	  val = p_val.second;	    
	  if (p_val.first) {
	    if (r_val > 0) {
	      INT64 new_val = val/ r_val;
	      Set_lo(_low_map, op1, new_val);
	      Infer_val_range(op1, TRUE, TRUE);
	    }
	  }
	}
	
	wn_tmp =  _high_map[wn];
	if (wn_tmp) {
	  p_val =  WN_get_val(wn_tmp,  _high_map);
	  val = p_val.second;
	  if (p_val.first) {
	    if (r_val > 0) {
	      INT64 new_val = val / r_val;
	      Set_hi(_high_map, op1, new_val);
	      Infer_val_range(op1, TRUE, TRUE);
	    }
	  }
	}
      }

      break;

    case OPR_ADD:
      op1 = WN_kid(wn, 0);
      op2 = WN_kid(wn, 1);

      if (WN_operator(op2) == OPR_INTCONST) {
	wn_tmp =  _low_map[wn];
	if (wn_tmp) {
	  p_val = WN_get_val(wn_tmp, _low_map);
	  val = p_val.second;	    
	  if (p_val.first) {
	    INT64 new_val = -1 * WN_const_val(op2) + val;
	    wn_tmp = Get_const_wn(new_val);
	    Set_map(_low_map, op1, wn_tmp);
	    Infer_val_range(op1, TRUE, TRUE);
	  }
	}

	wn_tmp = _high_map[wn];
	if (wn_tmp) {
	  p_val =  WN_get_val(wn_tmp,  _high_map);
	  val = p_val.second;
	  if (p_val.first) {
	    INT64 new_val = -1 * WN_const_val(op2) + val;
	    wn_tmp = Get_const_wn(new_val);
	    Set_map(_high_map, op1, wn_tmp);
	    Infer_val_range(op1, TRUE, TRUE);
	  }
	}
      }

      break;

    case OPR_SUB:

      op1 = WN_kid(wn, 0);
      op2 = WN_kid(wn, 1);

      wn_tmp = _low_map[wn];

      if (wn_tmp) {
	p_val = WN_get_val(wn_tmp,  _low_map);
	val = p_val.second;
	if (p_val.first) {
	  wn_tmp = _low_map[op2];
	    
	  if (wn_tmp) {
	    p_val = WN_get_val(op2, _low_map);
	    val2 = p_val.second;
	    if  (p_val.first) {
	      INT64 new_val = val2 + val;
	      wn_tmp = Get_const_wn(new_val);
	      Set_map(_low_map, op1, wn_tmp);
	      Infer_val_range(op1, TRUE, TRUE);
	    }
	  }
	}
      }

      wn_tmp = _high_map[wn];

      if (wn_tmp) {
	p_val = WN_get_val(wn_tmp,  _high_map);
	val = p_val.second;
	if (p_val.first) {
	  wn_tmp =  _high_map[op2];
	  
	  if (wn_tmp) {
	    p_val = WN_get_val(op2,  _high_map);
	    val2 = p_val.second;
	    if (p_val.first) {
	      INT64 new_val = val2 + val;
	      wn_tmp = Get_const_wn(new_val);
	      Set_map(_high_map, op1, wn_tmp);
	      Infer_val_range(op1, TRUE, TRUE);
	    }
	  }
	}
      }

      break;

    default:
      ;
    }
  }
}

// Walk WHIRL tree rooted at 'wn', find shift count expressions and infer value ranges
// for variables appearing in shift count expression.  'sc_loop' gives the nesting loop
// of 'wn'.
void
CFG_TRANS::Infer_shift_count_val(WN  * wn, SC_NODE * sc_loop)
{
  OPERATOR opr = WN_operator(wn);
  if (opr == OPR_SHL) {
    WN * wn_tmp = WN_kid1(wn);
    OPERATOR opr = WN_operator(wn_tmp);

    if ((opr == OPR_ADD) || (opr == OPR_SUB)) {
      WN * op1 = WN_kid0(wn_tmp);
      WN * op2 = WN_kid1(wn_tmp);
      int val;
      WN * wn_tmp;
      if (WN_operator(op2) == OPR_INTCONST) {
	val = WN_const_val(op2);	
	if (opr == OPR_ADD)
	  val = val * -1;
	if (Is_invariant(sc_loop, op1, 0)) {
	  opr = WN_operator(op1);
	  switch (opr) {
	  case OPR_LDID:
	    Set_lo(_low_map, op1, val);
	    break;
	  case OPR_MPY:
	    wn_tmp = WN_kid1(op1);
	    if (WN_operator(wn_tmp) == OPR_INTCONST) {
	      if ((val > 0)
		  && (val < WN_const_val(wn_tmp))) {
		// From pattern: c1 * x >= c2, where c1 > 0, c2 > 0 and c1 > c2,
		// we can infer that x >= 1.
		wn_tmp = WN_kid0(op1);
		if (WN_operator(wn_tmp) == OPR_LDID)
		  Set_lo(_low_map, wn_tmp, 1);
	      }
	      else if ((val < 0)
		       && ((-1 * val) < WN_const_val(wn_tmp))) {
		// From pattern: c1 * x + c2 >= 0, where c1 > c2 > 0,
		// we can infer that x >= 0.
		wn_tmp = WN_kid0(op1);
		if (WN_operator(wn_tmp) == OPR_LDID)
		  Set_lo(_low_map, wn_tmp, 0);
	      }
	    }
	    break;
	  default:
	    ;
	  }
	}
      }
    }
    else if (OPERATOR_is_scalar_load(opr)
	     && Is_invariant(sc_loop, wn_tmp, 0)) {
      Set_lo(_low_map, wn_tmp, 0);
    }
  }
  
  for (int i = 0; i < WN_kid_count(wn); i++)
    Infer_shift_count_val(WN_kid(wn,i), sc_loop);
}

// Infer value ranges from loop bounds.
void
CFG_TRANS::Infer_lp_bound_val(SC_NODE * sc_lcp)
{
  if ((sc_lcp->Type() == SC_LOOP) 
      && sc_lcp->Loopinfo()->Is_flag_set(LOOP_PRE_DO)) {
    SC_NODE * sc_body = sc_lcp->Find_kid_of_type(SC_LP_BODY);
    SC_NODE * sc_cond = sc_lcp->Find_kid_of_type(SC_LP_COND);
    SC_NODE * sc_init = sc_lcp->Find_kid_of_type(SC_LP_START);
    SC_NODE * sc_step = sc_lcp->Find_kid_of_type(SC_LP_STEP);
    BB_NODE * bb_body = sc_body->First_bb();
    BB_NODE * bb_cond = sc_cond->First_bb();
    BB_NODE * bb_init = sc_init->First_bb();
    BB_NODE * bb_step = sc_step->First_bb();

    if (bb_init && bb_cond && bb_body && bb_step
	&& !bb_cond->Is_branch_to(bb_body)) {
      BOOL set_high = FALSE;
      BOOL set_low = FALSE;
      WN * wn_init = bb_init->Laststmt();
      WN * wn_step = bb_step->Laststmt();
      wn_step = WN_prev(wn_step);
      WN * wn_add = wn_step ? WN_kid(wn_step, 0) : NULL;
      WN * wn_load = wn_add ? WN_kid(wn_add, 0) : NULL;
	  
      if (wn_step && OPERATOR_is_scalar_store(WN_operator(wn_step))
	  && wn_init && OPERATOR_is_scalar_store(WN_operator(wn_init))
	  && (WN_aux(wn_init) == WN_aux(wn_step))
	  && wn_load && OPERATOR_is_scalar_load(WN_operator(wn_load))
	  && (WN_aux(wn_load) == WN_aux(wn_step))
	  && (WN_operator(wn_add) == OPR_ADD)) {
	std::pair<bool, int> p_val = WN_get_val(WN_kid(wn_add,1),_low_map);
	int val = p_val.second;

	if (p_val.first) {
	  if (val > 0)
	    set_low = TRUE;
	  else
	    set_high = TRUE;
	}
      }
	  
      if (set_high || set_low) {
	if (Is_invariant(sc_body, bb_init, 0)) 
	  Infer_val_range(wn_init, set_high, set_low);
	    
	WN * wn_cond = bb_cond->Laststmt();

	if (wn_cond && (WN_operator(wn_cond) == OPR_FALSEBR)
	    && Is_invariant(sc_body, bb_cond, 0)) {
	  wn_cond = WN_kid(wn_cond, 0);
	  Match_def(wn_cond);
	  Infer_val_range(wn_cond, TRUE, TRUE);
	  
	  WN * wn_lhs = WN_kid0(wn_cond);
	  if (OPERATOR_is_scalar_load(WN_operator(wn_lhs)))
	    _deriv_wn_map[WN_aux(wn_lhs)] = wn_cond;
	}
      }
    }
  }
}

// Given a pair of SC_NODE *, infer value ranges for variables appearing in nesting loops'
// initialization blocks and condition-testing blocks.
void
CFG_TRANS::Infer_val_range(SC_NODE * sc1, SC_NODE * sc2)
{
  SC_NODE * sc_lcp = sc1->Find_lcp(sc2);

  if (Do_ext_trans()) {
    _low_map.clear();
    _high_map.clear();
    _def_wn_map.clear();
    _deriv_wn_map.clear();
    SC_NODE * nesting_lp = NULL;

    while (sc_lcp) {
      SC_TYPE type = sc_lcp->Type();
      if ((type == SC_LOOP) 
	  && sc_lcp->Loopinfo()->Is_flag_set(LOOP_PRE_DO)) {
	Infer_lp_bound_val(sc_lcp);

	if (Do_ext_trans()) {
	  // Infer value ranges from the loop entry.
	  SC_NODE * sc_body = sc_lcp->Find_kid_of_type(SC_LP_BODY);
	  SC_NODE * sc_tmp = sc_body->First_kid();
	  BB_NODE * bb_body = sc_body->First_bb();

	  if (!sc_tmp->Is_pred_in_tree(sc1)
	      && !sc_tmp->Is_pred_in_tree(sc2)) {
	    WN * tmp;
	    for (tmp = bb_body->Firststmt(); tmp != NULL; tmp = WN_next(tmp)) {
	      Infer_shift_count_val(tmp, sc_lcp);
	    }
	  }
	}

	if (_current_scope && (_current_scope != sc_lcp) 
	    && sc_lcp->Is_pred_in_tree(_current_scope)) {
	  Infer_lp_bound_val(_current_scope);
	}
	nesting_lp = sc_lcp;
      }
      sc_lcp = sc_lcp->Parent();
    }

    if (nesting_lp && (sc1->Type() == SC_IF)) {
      WN * wn_cond = sc1->Get_cond();
      Infer_shift_count_val(wn_cond, nesting_lp);
    }
  }
}

// For scalar loads in the given WHIRL tree, match definitions and
// set up value range maps.
void
CFG_TRANS::Match_def(WN * wn)
{
  if (OPERATOR_is_scalar_load(WN_operator(wn))) {
    WN * wn_def =  _def_wn_map[WN_aux(wn)];

    if (wn_def && (wn_def != wn)) {
      _low_map[wn] = wn_def;
      _high_map[wn] = wn_def;
    }
  }
  
  for (int i = 0; i < WN_kid_count(wn); i++)
    Match_def(WN_kid(wn,i));
}

// Walk WHIRL trees rooted at 'wn', find a scalar load that matches 'id'.
WN *
CFG_TRANS::Get_wn_by_aux_id(AUX_ID aux_id, WN * wn)
{
  OPERATOR opr = WN_operator(wn);
  FmtAssert(opr != OPR_BLOCK, ("Illegal input WHILR"));

  if (OPERATOR_is_scalar_load(opr)
      && (WN_aux(wn) == aux_id))
    return wn;

  for (int i = 0; i < WN_kid_count(wn); i++) {
    WN * wn_ret = Get_wn_by_aux_id(aux_id, WN_kid(wn, i));
    if (wn_ret)
      return wn_ret;
  }

  return NULL;
}

// Given an integral WHIRL 'wn1', query whether it is evaluatable and get the evaluated value.
// 'map' gives a WHIRL-to-WHIRL map that maps a WHIRL to another WHIRL containing the same value.
// For scalar loads in 'wn1', clone the values from nodes in 'wn2'.
std::pair<bool,int> 
CFG_TRANS::Clone_val(WN * wn1, WN * wn2, std::map<WN *, WN *> & map)
{
  OPERATOR opr = WN_operator(wn1);
  INT val, val1, val2;

  if (opr == OPR_INTCONST) {
    val = WN_const_val(wn1);
    return std::pair<bool,int>(TRUE, val);
  }
  else if (OPERATOR_is_scalar_load(opr)) {
    WN * wn_tmp = Get_wn_by_aux_id(WN_aux(wn1), wn2);
    if (wn_tmp)
      return WN_get_val(wn_tmp, map);
    else
      return std::pair<bool,int>(FALSE, 0);
  }
   
  std::pair<bool,int> pair1;
  std::pair<bool,int> pair2;

  switch (opr) {
  case OPR_ADD:
    pair1 = Clone_val(WN_kid(wn1, 0), wn2, map);
    pair2 = Clone_val(WN_kid(wn1, 1), wn2, map);
    val1 = pair1.second;
    val2 = pair2.second;

    if (pair1.first && pair2.first) {
      val = val1 + val2;
      return std::pair<bool,int>(TRUE, val);
    }
    break;
  default:
    break;
  }
  
  return std::pair<bool,int>(FALSE,0);
}

// Simplify "ADD" and "SUB" expressions in 'wn'.
WN * 
CFG_TRANS::Simplify_wn(WN * wn)
{
  OPERATOR opr = WN_operator(wn);
  if ((opr == OPR_ADD) || (opr == OPR_SUB)) {
    STACK<WN *> * l_stk = CXX_NEW(STACK<WN *> (_pool), _pool);
    STACK<WN *> * r_stk = CXX_NEW(STACK<WN *> (_pool), _pool);

    Collect_operands(wn, l_stk, r_stk);
    WN * wn_tmp;
    int const_val = 0;
    OPERATOR opr;

    while (!r_stk->Is_Empty()) {
      wn_tmp = r_stk->Pop();
      opr = WN_operator(wn_tmp);
      if (opr == OPR_INTCONST)
	const_val -= WN_const_val(wn_tmp);
      else {
	// Find a matching element in l_stk.
	BOOL found = FALSE;
	for (int i = 0; i < l_stk->Elements(); i++) {
	  WN * wn2 = l_stk->Top_nth(i);
	  if (wn2 && (WN_Simp_Compare_Trees(wn_tmp, wn2) == 0)) {
	    l_stk->Settop_nth(NULL, i);
	    found = TRUE;
	    break;
	  }
	}
	if (!found) {
	  CXX_DELETE(l_stk, _pool);
	  CXX_DELETE(r_stk, _pool);
	  return NULL;
	}
      }
    }

    WN * wn_last = NULL;
    while (!l_stk->Is_Empty()) {
      wn_tmp = l_stk->Pop();
      if (wn_tmp) {
	opr = WN_operator(wn_tmp);
	if (opr == OPR_INTCONST)
	  const_val += WN_const_val(wn_tmp);
	else if (wn_last == NULL) {
	  wn_last = wn_tmp;
	}
	else 
	  wn_last = WN_CreateExp2(OPR_ADD, MTYPE_I4, MTYPE_V, wn_last, wn_tmp);
      }
    }

    if (wn_last && const_val) {
      wn_tmp = WN_CreateIntconst(OPR_INTCONST, (const_val > 0) ? MTYPE_U4 : MTYPE_I4, 
				 MTYPE_V, const_val);
      wn_last = WN_CreateExp2(OPR_ADD, MTYPE_I4, MTYPE_V, wn_last, wn_tmp);
    }

    CXX_DELETE(l_stk, _pool);
    CXX_DELETE(r_stk, _pool);
    return wn_last;
  }

  return NULL;
}

// Find WHIRLs in 'wn' that matches 'wn_old' and replace it with 'wn_new'.
// Return TRUE if replacement happens.
BOOL
CFG_TRANS::Replace_wn(WN  * wn, WN * wn_old, WN * wn_new)
{
  BOOL ret_val = FALSE;
  for ( int i = 0; i < WN_kid_count(wn); i++) {
    WN * wn_kid = WN_kid(wn, i);
    if (WN_Simp_Compare_Trees(wn_kid, wn_old) == 0) {
      WN_kid(wn, i) = wn_new;
      ret_val = TRUE;
    }
    else {
      if (Replace_wn(wn_kid, wn_old, wn_new)) {
	ret_val = TRUE;
	wn_kid = Simplify_wn(wn_kid);
	if (wn_kid)
	  WN_kid(wn, i) = wn_kid;
      }
    }
  }
  return ret_val;
}

// Find WHIRLs in 'bb' that matches 'wn_old' and replace it with 'wn_new'.
void
CFG_TRANS::Replace_wn(BB_NODE * bb, WN * wn_old, WN * wn_new)
{
  WN * wn;
  STMT_ITER stmt_iter;
  FOR_ALL_ELEM (wn, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {
    Replace_wn(wn, wn_old, wn_new);
  }
}

// Find WHIRLs in 'sc' that matches 'wn_old' and replace it with 'wn_new'.
void
CFG_TRANS::Replace_wn(SC_NODE * sc, WN * wn_old, WN * wn_new)
{
  BB_NODE * bb = sc->Get_bb_rep();

  if (bb)
    Replace_wn(bb, wn_old, wn_new);

  BB_LIST * bb_list = sc->Get_bbs();

  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    BB_NODE * tmp;
    
    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      Replace_wn(tmp, wn_old, wn_new);
    }
  }

  SC_LIST * kids = sc->Kids();

  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE *tmp;

    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      Replace_wn(tmp, wn_old, wn_new);
    }
  }
}

// Given a SC_IF node 'sc' and a bit-operation expression 'wn', if both the then-path and
// the else-path end with a SC_BLOCK whose statements are bit reductions on the same object,
// and there exists only one statement that alters the same bit as 'wn', sink that statement to 
// the merge block.  
BOOL CFG_TRANS::Do_flip_tail_merge(SC_NODE * sc, WN * wn)
{
  FmtAssert(sc->Type() == SC_IF, ("Expect a SC_IF."));
  if (!sc->Is_well_behaved())
    return FALSE;

  if (!WN_is_bit_op(wn))
    return FALSE;

  SC_NODE * tmp;
  for (int i = 0; i < 2; i++) {
    tmp = (i == 0) ? sc->First_kid() : sc->Last_kid();
    SC_NODE * first_kid = tmp->First_kid();
    SC_NODE * last_kid = tmp->Last_kid();

    if (!first_kid->Is_ctrl_equiv(last_kid))
      return FALSE;
    
    if (last_kid->Type() != SC_BLOCK)
      return FALSE;
  }

  BB_NODE * bb_merge = sc->Merge();
  if (!bb_merge->Is_postdom(sc))
    return FALSE;
  
  WN * wn_flip1 = NULL;
  WN * wn_flip2 = NULL;
  BB_NODE * bb1 = NULL;
  BB_NODE * bb2 = NULL;

  for (int i = 0; i < 2; i++) {
    tmp = (i == 0) ? sc->First_kid() : sc->Last_kid();
    tmp = tmp->Last_kid();
    
    BB_LIST_ITER bb_list_iter(tmp->Get_bbs());
    BB_NODE * bb_iter;
  
    FOR_ALL_ELEM(bb_iter, bb_list_iter, Init()) {
      WN * wn_iter;
      STMT_ITER stmt_iter;
      FOR_ALL_ELEM(wn_iter, stmt_iter, Init(bb_iter->Firststmt(), bb_iter->Laststmt())) {
	if (!WN_is_executable(wn_iter))
	  continue;
	WN * wn_tmp = WN_get_bit_reduction(wn_iter);
	if (!wn_tmp || (WN_Simp_Compare_Trees(WN_kid0(wn_tmp), WN_kid0(wn)) != 0)) {
	  return FALSE;
	}
	if (Maybe_assigned_expr(wn_iter, wn)) {
	  if (i == 0) {
	    if (!wn_flip1) {
	      wn_flip1 = wn_iter;
	      bb1 = bb_iter;
	    }
	    else
	      return FALSE;
	  }
	  else {
	    if (!wn_flip2) {
	      wn_flip2 = wn_iter;
	      bb2 = bb_iter;
	    }
	    else
	      return FALSE;
	  }
	}
      }
    }
  }

  if (wn_flip1 && wn_flip2
      && (WN_Simp_Compare_Trees(WN_kid0(wn_flip1), WN_kid0(wn_flip2)) == 0)) {
    bb1->Unlink_stmt(wn_flip1);
    bb2->Unlink_stmt(wn_flip2);
    WN_Delete(wn_flip2);
    bb_merge->Prepend_stmt(wn_flip1);
    return TRUE;
  }
  return FALSE;
}

void IF_MERGE_TRANS::Normalize(SC_NODE *sc)
{
    SC_NODE *sc1;
    SC_LIST_ITER kids_iter;
    SC_NODE * tmp;
    SC_LIST_ITER sc_list_iter;
    BOOL has_dep = FALSE;

    sc1 = sc->First_kid_of_type(SC_IF);
    while (sc1 != NULL) {

      BB_NODE * bb_head = sc1->Head();
      FOR_ALL_ELEM(tmp, sc_list_iter, Init(sc1->Kids())) {
      if (Has_dependency(tmp, bb_head)) {
       has_dep = TRUE;
       break;
      }
      else
        has_dep = FALSE;
    }
    if(has_dep) 
      Do_split_if_head(sc1);

    sc1 = sc1->Next_sibling_of_type(SC_IF);
   }
   FOR_ALL_ELEM(tmp, kids_iter, Init(sc->Kids())) {
    this->Normalize(tmp);
  }
}

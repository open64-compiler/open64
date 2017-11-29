/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

// ====================================================================
// ====================================================================
//
// Copyright (C) 2007, University of Delaware, Hewlett-Packard Company,
// All Rights Reserved.
//
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
//
// ====================================================================
#include "defs.h"			// INT32, INT64
#include "config.h"			// Alias_Pointer_Parms
#include "opt_defs.h"
#include "tracing.h"			// trace flags
#include "config_opt.h"

#include "erglob.h"
#include "opt_cfg.h"  // for EXITBB_ITER 
#include "opt_sym.h"
#include "opt_misc.h"

#include <stack>
#include "opt_du.h"
#include "bb_node_set.h"

extern void Rename_CODEMAP(COMP_UNIT *);

static void
Analyze_pu_noreturn_attr (OPT_STAB* stab, PU* pu, ST* pu_st) {
 
  EXITBB_ITER iter(stab->Cfg());
  FOR_ALL_ITEM (iter, Init()) {
    BB_NODE* exit_bb = iter.Cur_exit_bb();
    WN *wn = exit_bb->Laststmt();
    if (wn && 
        (WN_operator (wn) == OPR_RETURN ||
         WN_operator (wn) == OPR_RETURN_VAL)) {
      wn = WN_prev (wn);
    }
    if (!wn || WN_operator (wn) != OPR_CALL) return;

    PU& pu_ent = Pu_Table[ST_pu(WN_st(wn))];
    if (!PU_has_attr_noreturn (pu_ent)) {
      return;
    }
  }
 
  // Now, all exit blocks are ended with call having 
  // __attribute__((noreturn), this function itself 
  // satisify the "noreturn" semantic 
  Set_PU_has_attr_noreturn (*pu);
 
}

static void
Analyze_nothrow_attr (OPT_STAB* opt_stab, ST* pu_st) {
  
  PU& pu = Pu_Table[ST_pu(pu_st)];
  if (PU_nothrow (pu)) {
    return;
  }

  // NOTE: We should not check language type and give up if it is 
  //  not C++. Here is an example, a() call b() which call c(), a() 
  //  and c() are C++ src, and b() is C code. We cannot blindly assume 
  //  b() will not throw exception.
  //

  BOOL may_throw = FALSE;

  CFG_ITER iter (opt_stab->Cfg ());
  BB_NODE *bb;
  FOR_ALL_NODE (bb, iter, Init()) {
    if (!bb->Hascall ())
      continue;

    // Examine WN statements
    //
    if (bb->Firststmt ()) {
      // HINT: calls are not necessarily at the end of the block depending on 
      //   CFG::Calls_break().
      //
      WN* wn;
      STMT_ITER iter;
      FOR_ALL_ELEM (wn, iter, Init (bb->Firststmt(), bb->Laststmt())) {
        OPERATOR opr = WN_operator (wn);  
        if (!OPERATOR_is_call (opr)) {
          continue;
        }

        if (opr == OPR_CALL) {
          ST* st = WN_st (wn);
          PU& pu = Pu_Table [ST_pu(*st)];
          if (!PU_nothrow (pu)) {
            may_throw = TRUE;
            break;
          }
        } else {
          // not able to handle indirect calls, virtual functions calls
          may_throw = TRUE;
          break;
        }
      } // end of FOR_ALL_ELEM 
    }

    if (may_throw)
      break;

    // Examine STMTREPs
    //
    if (bb->First_stmtrep ()) {
      STMTREP_ITER stmt_iter(bb->Stmtlist());
      STMTREP *stmt;
      FOR_ALL_NODE (stmt, stmt_iter, Init()) {
        OPERATOR opr = stmt->Opr ();
        if (!OPERATOR_is_call (opr)) {
            continue;
        }

        if (opr == OPR_CALL) {
          ST* st = stmt->St ();
          PU& pu = Pu_Table [ST_pu(*st)];
          if (!PU_nothrow (pu)) {
            may_throw = TRUE;
            break;
          }
        } else {
          // not able to handle indirect calls, virtual functions calls
          may_throw = TRUE;
          break;
        }
      } // end of FOR_ALL_NODE 
    }
    
    if (may_throw) { break; }
  }

  if (!may_throw) {
    Set_PU_nothrow (pu);
  }
}

// Analyze_pu_attr() conducts following things
//
//   - reveal _attribute_ semantics
//   - points-to summary 
// 
void
Analyze_pu_attr (OPT_STAB* opt_stab, ST* pu_st) {

  WN* pu_tree = opt_stab->Pu ();
  if (WN_opcode(pu_tree)!=OPC_FUNC_ENTRY)  {
    // not applicable, give up. 
    return;
  }

  PU& pu_ent = Pu_Table[ST_pu(pu_st)];

  // analyze __attribute__((noreturn)
  if (WOPT_Enable_Noreturn_Attr_Opt && !PU_has_attr_noreturn (pu_ent)) {
    Analyze_pu_noreturn_attr (opt_stab, &pu_ent, pu_st);
  }

  if (WOPT_Enable_Pt_Summary) {
    SET_OPT_PHASE("Points-to Summary");
    opt_stab->Summarize_points_to ();
  }

  // analyze if PU throw exception
  Analyze_nothrow_attr (opt_stab, pu_st);
}

// Check if the array node is in a 'form' suitable to be a candidate
// for redundant mem clear removal
// - should be a single dimensional array
// - the index should be just the induction variable with zero offset
// - the base addr is a PREG or VAR (v, with offset and field == 0)
//   or a expression of the form (v + offset, where offset is a const and > 0)
// If valid, returns the base addr and offset
static BOOL valid_array_addr(WN *array_wn, WN *ind, INT bytenum,
                             WN *& base_addr, INT& offset)
{
  Is_True(WN_operator(array_wn) == OPR_ARRAY, ("Expecting an ARRAY"));

  // deal with 1-dim array  only
  if (WN_num_dim(array_wn) != 1)
    return FALSE; 

  // the size should equal
  if (WN_element_size(array_wn) != bytenum)
    return FALSE; 

  // Check index
  WN *idx = WN_kid2(array_wn);
  if (! (WN_operator(idx) == OPR_LDID && WN_offset(ind) == 0 &&
         WN_st(idx) == WN_st(ind)) )
    return FALSE;

  // Check base
  WN *addr = WN_kid0(array_wn);
  if (! ( ( WN_operator(addr) == OPR_LDID &&
            (ST_class(WN_st(addr)) == CLASS_PREG ||
             (WN_offset(addr) == 0 && WN_field_id(addr) == 0)
            ) 
          ) ||
          ( WN_operator(addr) == OPR_ADD &&
            WN_operator(WN_kid0(addr)) == OPR_LDID &&
            (ST_class(WN_st(WN_kid0(addr))) == CLASS_PREG ||
             (WN_offset(WN_kid0(addr)) == 0 && 
              WN_field_id(WN_kid0(addr)) == 0)
            ) &&
            WN_operator(WN_kid1(addr)) == OPR_INTCONST &&
            WN_const_val(WN_kid1(addr)) > 0
          )
        )
     )
    return FALSE;

  if (WN_operator(addr) == OPR_LDID)
  {
    base_addr = addr;
    offset = 0;
  } 
  else
  {
    base_addr = WN_kid0(addr);
    offset = WN_const_val(WN_kid1(addr));
  }
    
  return TRUE;
}

// Check is the whirl statment 'copy' is a STID fed by a LDID
// The STID and LDID must be from either a PREG or VAR with zero offset
// and field ids
static BOOL isCopy(WN *copy)
{
  if (WN_operator(copy) != OPR_STID)
    return FALSE;
  if (WN_operator(WN_kid0(copy)) != OPR_LDID)
    return FALSE;
  if (ST_class(WN_st(copy)) != CLASS_PREG &&
      (WN_offset(copy) != 0 || WN_field_id(copy) != 0))
    return FALSE;
  if (ST_class(WN_st(WN_kid0(copy))) != CLASS_PREG &&
      (WN_offset(WN_kid0(copy)) != 0 || WN_field_id(WN_kid0(copy)) != 0))
    return FALSE;
  return TRUE;
}

// Starting from the def node, until we reach the statement following
// the do_loop, check if there is use of the target of the def node
static BOOL check_if_def_reaches_memset(WN *def, WN *base_addr, WN *do_loop,
                                        ALIAS_MANAGER *alias_mgr,
                                        BOOL& use_after_do_loop)
{
  FmtAssert(isCopy(def), ("def not a copy!\n"));
  use_after_do_loop = FALSE;
  ST *st = WN_st(def);
  // Ignore anything that is not a var and has its address taken
  if (ST_sym_class(*st) != CLASS_VAR ||
      ST_addr_passed(*st)  || ST_addr_saved(*st))
    return FALSE;

  WN *stmt = WN_next(def);
  while (stmt != WN_next(do_loop)) 
  {
    for (WN_ITER *wni = WN_WALK_TreeIter(stmt);
         wni; wni = WN_WALK_TreeNext(wni)) 
    {
      WN *wn = WN_ITER_wn(wni);
      // Since it is VAR with address not taken, we only look for calls
      // or LDIDs that use the VAR
      // If there is a may use at a call
      // check if it is one of some 'nice' libc calls
      // or if the call does not return
      // else abort
      if (WN_operator(wn) == OPR_CALL &&
          Aliased_with_region(alias_mgr, def, wn, READ_AND_WRITE)) 
      {
        ST *call_st = WN_st(wn);
        if (call_st && 
            (!strcmp(ST_name(call_st), "calloc") ||
             !strcmp(ST_name(call_st), "abort") ||
             !strcmp(ST_name(call_st), "exit") ||
             !strcmp(ST_name(call_st), "fprintf")))
          continue;
        if (WN_Call_Never_Return(wn))
          continue;
        return FALSE;
      } 
      else if (WN_operator(wn) == OPR_LDID && WN_st(def) == WN_st(wn)) 
      {
        // If found an LDID that uses the var, ensure that it is the
        // base_addr of the array in the do_loop, else abort
        if (wn != base_addr)
          return FALSE;
        else
          return TRUE;
      }
    }
    stmt = WN_next(stmt);
  }
  use_after_do_loop = TRUE;
  return FALSE;
}

// Given a tree, initialize its parent pointers.
// Override what was there, if anything.
// Do not update parent pointer of the root node 'wn'.
static void wn_parentize(WN* wn, WN_MAP& parent_map) 
{
  if (!OPCODE_is_leaf(WN_opcode(wn))) 
  {
    if (WN_opcode(wn) == OPC_BLOCK) 
    {
      WN *kid = WN_first(wn);
      while (kid) 
      {
        WN_MAP_Set(parent_map, kid, wn);
        wn_parentize(kid, parent_map);
        kid = WN_next(kid);
      }
    } 
    else 
    {
      for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) 
      {
        WN *kid = WN_kid(wn, kidno);
        if (kid) 
        {
          WN_MAP_Set(parent_map, kid, wn);
          wn_parentize(kid, parent_map);
        }
      }
    }
  }
}

// Optimization to eliminate redundant clear of memory following a calloc of
// the same space.
// Checks if there are no writes into the space allocated by the calloc
// in which case the subsequent clear is no longer redundant
void remove_redundant_mem_clears(WN *func,
                                 ALIAS_MANAGER *alias_mgr,
                                 DU_MANAGER *du_mgr)
{
  if (!WOPT_Enable_Mem_Clear_Remove)
    return;

  MEM_POOL mpool;
  MEM_POOL_Initialize(&mpool, "parent wn", FALSE);
  MEM_POOL_Push(&mpool);

  WN_MAP parent_map = WN_MAP_Create(&mpool);
  BOOL parentized = FALSE;

  // Search for the do_loop that zero out a memory location
  for (WN_ITER *wni = WN_WALK_StmtIter(WN_func_body(func));
       wni; wni = WN_WALK_StmtNext(wni)) 
  {
    WN *wn = WN_ITER_wn(wni);

    if (WN_opcode(wn) != OPC_DO_LOOP)
      continue;
    WN *do_loop_wn = wn;
    WN *body       = WN_do_body(do_loop_wn);
    WN *loop_info  = WN_do_loop_info(do_loop_wn);
    if (loop_info == NULL)
      continue;

    // Check for unit stride
    WN *loop_indvar = WN_loop_induction(loop_info);
    if (!loop_indvar)
      continue;
    BOOL is_incr;
    WN *loop_stride = WN_LOOP_Increment(do_loop_wn, &is_incr);
    if (!loop_stride || is_incr == FALSE ||
        !WN_operator_is(loop_stride, OPR_INTCONST) ||
        WN_const_val(loop_stride) != 1)
      continue;

    OPCODE ub_compare;
    WN *upper_bound = WN_LOOP_UpperBound(do_loop_wn, &ub_compare, TRUE);
    if (!upper_bound)
      continue;

    WN *trip_count = WN_LOOP_TripCount(do_loop_wn, TRUE);
    if (!trip_count)
      continue;

    WN *lower_bound = WN_LOOP_LowerBound(do_loop_wn);
    if (!lower_bound)
      continue;

    // If not starting from constant 0, abort
    if (!(WN_operator_is(lower_bound, OPR_INTCONST) &&
          WN_const_val(lower_bound) == 0))
      continue;

    Is_True(WN_opcode(loop_indvar), ("expected a non-NULL loop induction"));

    WN *base_addr = NULL;
    INT offset_val = 0;
    // Try to find the mem clear within the loop body
    // Check if only ISTORE of the constant 0 is in the loop body
    INT istore_num = 0;
    BOOL do_opt = TRUE;
    for (WN *stmt = WN_first(body); stmt; stmt = WN_next(stmt)) 
    {
      if (WN_operator(stmt) == OPR_LABEL)
        continue;
      if (!(WN_operator(stmt) == OPR_ISTORE &&
            WN_field_id(stmt) == 0 && WN_offset(stmt) == 0)) 
      {
        do_opt = FALSE;
        break;
      }

      // no transformation if more than one ISTORE
      if (++istore_num > 1) 
      {
        do_opt = FALSE;
        break;
      }

      // Currently deal with only non-float types
      if (MTYPE_is_float(WN_desc(stmt))) 
      {
        do_opt = FALSE;
        break;
      }

      WN *load_value_wn = WN_kid0(stmt);
      WN *store_addr_wn = WN_kid1(stmt);
      INT mbyte_stmt = MTYPE_byte_size(OPCODE_desc(WN_opcode(stmt)));

      // If not assigned a constant 0, abort
      if (!(WN_operator_is(load_value_wn, OPR_INTCONST) &&
            WN_const_val(load_value_wn) == 0)) 
      {
        do_opt = FALSE;
        break;
      }

      // The address of the ISTORE has to be a ARRAY with some
      // restrictions (see valid_array_addr)
      if (! (WN_operator(store_addr_wn) == OPR_ARRAY &&
             valid_array_addr(store_addr_wn, loop_indvar, mbyte_stmt,
                              base_addr, offset_val)) ) 
      {
        do_opt = FALSE;
        break;
      }
    }

    if (!do_opt || istore_num == 0)
      continue;

    FmtAssert(WN_operator(base_addr) == OPR_LDID && offset_val >= 0,
               ("Expecting LDID with const offset\n"));

    // Search backwards starting from the do_loop, until we reach a calloc
    // and check if the return value of the calloc reaches the base addr
    // of the array being cleared.
    // Need to ensure that any writes to the calloced area does not
    // overlap with the area being cleared in the do_loop.

    WN *prev_wn = WN_prev(do_loop_wn);
    for (; prev_wn; prev_wn = WN_prev(prev_wn)) 
    {
      if (!(WN_operator(prev_wn) == OPR_CALL &&
            !strcmp(ST_name(WN_st(prev_wn)), "calloc")))
        continue;

      if (!parentized) 
      {
        wn_parentize(func, parent_map);
        parentized = TRUE;
      }

      // The return value of the calloc is assumed to be the next statement
      WN *def_wn = WN_next(prev_wn);
      if (!(WN_operator(def_wn) == OPR_STID &&
            Is_Return_Preg(WN_offset(WN_kid0(def_wn))) && isCopy(def_wn)))
        continue;

      // Follow the uses of the return value of the calloc
      // If the value is being copied over to another variable or preg
      // follow the uses of this new copy target, until we reach the 
      // final use in the do_loop. We do not care about any use after
      // the do_loop.
      
      BOOL found_memset = FALSE;
      BOOL do_opt = TRUE;
      // Stack to keep track of any copies we see while traversing the
      // du chain.
      std::stack<WN*> def_stack;

      while (do_opt && def_wn) 
      {
        USE_LIST *use_list = du_mgr->Du_Get_Use(def_wn);
        ST *st = WN_st(def_wn);
        // If its a var with address taken, ignore
        if (ST_sym_class(*st) == CLASS_VAR &&
            (ST_addr_passed(*st) || ST_addr_saved(*st))) 
        {
          do_opt = FALSE;
          break;
        }

        // In presence of may defs, the use list can be incomplete
        // If so, do a brute force search for all uses
        // starting from the statement following the define 
        // until we hit the do_loop
        if (!use_list || use_list->Incomplete()) 
        {
          BOOL use_after_do_loop = FALSE;
          if (!check_if_def_reaches_memset(def_wn, base_addr, do_loop_wn,
                                           alias_mgr, use_after_do_loop)) 
          {
            // If the use is before the do_loop, abort
            if (!use_after_do_loop)
              do_opt = FALSE;
          }
          else 
          {
            // Any use/def beyond the use within the do_loop
            // can be ignored since we are checking all instructions
            // between def_wn and this use
            found_memset = TRUE;
          }
          // Track next define
          if (!def_stack.empty()) 
          {
            def_wn = def_stack.top();
            def_stack.pop();
          } 
          else
            def_wn = NULL;
          continue;
        }
        USE_LIST_ITER uiter(use_list);
        for (DU_NODE *use_node = uiter.First(); !uiter.Is_Empty();
             use_node = uiter.Next()) 
        {
          WN *use = use_node->Wn();
          if (WN_operator(use) == OPR_RETURN)
            continue;
          if (WN_operator(use) != OPR_LDID) 
          {
             do_opt = FALSE;
             break;
          } 
          else if (use == base_addr) 
          {
            found_memset = TRUE;
            continue;
          }
          WN *useParent = (WN *)WN_MAP_Get(parent_map, use);
          // If the use's parent is a copy, save the target of the copy
          // so as to start tracking its use the next time
          if (isCopy(useParent)) 
          {
            def_stack.push(useParent);
          } 
          else if (WN_operator(useParent) == OPR_ISTORE &&
                     WN_kid1(useParent) == use &&
                     (ST_class(WN_st(use)) == CLASS_PREG  ||
                      (WN_offset(use) == 0 && WN_field_id(use) == 0))) 
          {
              // Make sure we are not overwriting the area we will
              // be clearing later
              if (WN_offset(useParent) +
                  MTYPE_byte_size(WN_desc(useParent)) - 1 >= offset_val) 
              {
                do_opt = FALSE;
                break;
              }
          } 
          else if (!OPERATOR_is_boolean(WN_operator(useParent))) 
          {
            // Construct not handled
            do_opt = FALSE;
            break;
          }
        }
        // Track next define
        if (!def_stack.empty()) 
        {
          def_wn = def_stack.top();
          def_stack.pop();
        } 
        else
          def_wn = NULL;
      }
      if (found_memset && do_opt) 
      {
        // remove the loop
        WN *parent_block = (WN *)WN_MAP_Get(parent_map, do_loop_wn);
        WN_DELETE_FromBlock(parent_block, do_loop_wn);
        WN_ITER_wn(wni) = NULL;
        break;
      }
    }
  }

  WN_MAP_Delete(parent_map);
  MEM_POOL_Pop(&mpool);
  MEM_POOL_Delete(&mpool);
}

//====================================================================
// UselessStoreElimination is to remove useless store inside the
// innermost loop. Here is the example:
// The code is as following:
//
// for (i=0; i< N;i++) {
//   c++;    
//   if (c == 10000) c = 1;        <-- wrap around of c
//   for (j=0; j< M;j++) a[j] = 0; <-- useless store
//   d = c;
//   a[x] = c;
//   if (a[y] != d) printf(stdout,"%d\n",y);
// }
// free(a);
//
// The only use of a[y] is compared with d (copy of c). When comparing, 
// a[y] can be two values 0[!c] or c. In every iteration, c is 
// monotonically increased. So, even if the a[j] is not cleared
// in each iteration, the semantics of (a[y] != d) does not change.
// Since the previous iterations' values are all !c, which have the same 
// semantics as 0 when comparing with c. 
// However, for the first iteration, some of a[] may have value of c, thus a[] 
// should be cleared. Another case that a[] needs be cleared is when c is
// wrapped around.  Then there are already values existed in a[] from 
// the previous round. Thus we need to clear a[].  
// In the above example, we can guard the j loop by the condition  
// "if (i==0 || c==1)".
//
// To elminate the useless store loop, we need to analyze the code as:
// 1) find the innermost loop j that has only one istore stmt ==> clear_st 
// 2) find its enclosing loop i and a is freed after loop i before any use
// 3) traverse the loop i in reverse post order to check all the defs and
//    uses of a[] using alias query
// 3.1) record the first def's (after clear_st) rhs ==> c 
// 3.2) record the copy of c ==> d 
// 3.3) check the following defs' rhs is the same as c or d and the 
//      following uses of a[] is in eq/ne and its rhs is the same as
//      c or d
// 3.4) check there is no other def of c or d in between
// 4) traverse the loop i in reverse post order to find the defs of c
// 4.1) the reaching def of c should be a monotical increment or 
//      a store of const
// 4.2) if def is increment stmt only, then guard the loop with
//      "if (i==0)"
// 4.3) if def is increase and a store of const, then guard the loop
//      with "if (i== 0 || c == const)"
//===================================================================

UselessStoreElimination::UselessStoreElimination(COMP_UNIT* comp_unit)
{
    _comp_unit = comp_unit;
    _cfg = comp_unit->Cfg();
    _opt_stab = comp_unit->Opt_stab();
    _alias_rule = _opt_stab->Rule();
    _htable = comp_unit->Htable();
    _opt_count = 0;

    _tracing = Get_Trace(TP_WOPT2, ULSE_TRACE_FLAG);
}

void
UselessStoreElimination::Perform_Useless_Store_Elimination()
{

    BB_LOOP* loop_list = _cfg->Analyze_loops();
    if (loop_list == NULL) return;

    BB_LOOP_ITER loop_iter(loop_list);
    BB_LOOP* loop;

    // loop_list contains all the outermost loop
    FOR_ALL_NODE (loop, loop_iter, Init()) {
        Traverse_Loops(loop);
    }

    if (_opt_count > 0) {
        
        if (_tracing)
            fprintf(stdout, "[ULSE] %d useless store loops are removed\n", _opt_count);

        // After changing cfg, invalidate loops and other rpo... data 
        // structures and rename the codemap
        _cfg->Invalidate_loops();
        _cfg->Invalidate_and_update_aux_info(TRUE);
        _cfg->Analyze_loops();

        Rename_CODEMAP(_comp_unit);
    }
}


// traverse each loop, check the innermost loop is candidate 
// or not and then perform transformation
void
UselessStoreElimination::Traverse_Loops(BB_LOOP* loop)
{
    if (loop->Child())
    {
        BB_LOOP_ITER loop_iter(loop->Child());
        BB_LOOP *child;
        FOR_ALL_NODE(child, loop_iter, Init()) {
            Traverse_Loops(child);
        }
    }
    else
    {
        Candidate_Clear();

        // check the inner most loop
        // this loop should have a parent loop
        BB_LOOP * parent = loop->Parent();
        if (parent == NULL) return;

        // 1) Is the innermost loop applicable?
        // only has one istore stmt a[j] = 0
        STMTREP * st_stmt = Is_Applicable_Innerloop(loop);
        if (st_stmt == NULL) return;

        POINTS_TO * pt = st_stmt->Lhs()->Points_to(_opt_stab);
        if (pt == NULL) return;

        if (_tracing)
        {
            fprintf(stdout, "[ULSE] =========have applicable loop===========\n");
            loop->Print(stdout);
        }
                        
        // 2) is a freed after the parent loop before any uses?
        if (!Is_Freed_in_BB(parent->Merge(), st_stmt->Lhs()))
        {
            if (_tracing)
                fprintf(stdout, "[ULSE] rejected: because of no free after loop\n");
            return;
        }    

        // 3) check all the defs and uses of a[]
        // we need two passes here, because as in the above example
        // "d = c" is before "a[x] = c". We need to find "a[x]=c" in the
        // first pass and then in the second pass, record the copy of c
        RPOBB_ITER cfg_iter(_cfg);
        BB_NODE * bb;
        FOR_ALL_ELEM(bb, cfg_iter, Init())
        {
           if (!Check_Uses_Defs_in_Loop(parent, st_stmt, bb, pt, TRUE))
           {
                if (_tracing)
                    fprintf(stdout, "[ULSE] rejected: could not find a legal def\n");
                return;
           }     

           if (_candidate.def_defbb != NULL) break;
        }
        if (_candidate.def_defbb == NULL) 
        {
            if (_tracing)
                fprintf(stdout, "[ULSE] rejected: could not find a legal def\n");
            return;
        }
        
        // In the second pass, keep track of defs and uses of a[], 
        // the def and uses's rhs should be the same as c or its copy
        // and there is no other def to c or d between def and use
        FOR_ALL_ELEM(bb, cfg_iter, Init())
        {
            if (!Check_Uses_Defs_in_Loop(parent, st_stmt, bb, pt, FALSE))
            {
                if (_tracing)
                    fprintf(stdout, "[ULSE] rejected: defs/uses have different rhs\n");
                return;
            }    
        }

        // 4) check the def of c: they should be a incr [and const]
        pt = _candidate.def_stmt->Rhs()->Points_to(_opt_stab);
        if (pt == NULL) return;
        FOR_ALL_ELEM(bb, cfg_iter, Init())
        {
            if (Have_Increment_Def(parent, bb, pt)) break;
        }
        if (!_candidate.def_inc)
        {   
            if (_tracing)
                fprintf(stdout, "[ULSE] rejected: there is no monotically increased def\n"); 
            return;
        }    

        // now add condition to the loop
        if (!Perform_transform(loop)) return;

        _opt_count ++;
    }

}

// check whether this innermost loop is a candidate
// only one istore stmt in the loop
// if yes, return istore stmt, otherwise return NULL
STMTREP *
UselessStoreElimination::Is_Applicable_Innerloop(BB_LOOP* loop)
{
    STMTREP * ret_stmt = NULL;

    if (!loop->Parent()->Merge()) return NULL;

    int inst_count = 0;
    BB_NODE_SET_ITER bb_iter;
    BB_NODE* bb;
    FOR_ALL_ELEM(bb, bb_iter, Init(loop->True_body_set())) {
        if (bb == loop->End()
            || bb == loop->Step()
            || bb == loop->Loopback()
            || bb == loop->Tail()) continue;

        STMTREP_ITER stmt_iter (bb->Stmtlist());
        STMTREP* stmt;
        // the loop has one stmt, which is a "istore x 0"
        FOR_ALL_NODE (stmt, stmt_iter, Init()) {

            if (stmt->Opr() == OPR_LABEL
                || stmt->Op() == OPC_PRAGMA
                || stmt->Op() == OPC_XPRAGMA)
                continue;

            inst_count ++;
            if (stmt->Opr() == OPR_ISTORE
                && stmt->Rhs()->Kind() == CK_CONST
                && stmt->Rhs()->Const_val() == 0) {
                    ret_stmt = stmt;
                    _candidate.clear_bb = bb;
            }

            if (inst_count >= 2)
                return NULL;
        }
    }
    return ret_stmt;
}
    
// check whether the coderep cr is aliased with pt
// or has a child coderep that is aliased with pt
BOOL
UselessStoreElimination::Aliased_with_CR(CODEREP * cr, POINTS_TO* pt)
{
    if (cr->Kind() == CK_OP)
    {
        for (INT i = 0; i < cr->Kid_count(); i++)
        {
            if (Aliased_with_CR(cr->Get_opnd(i), pt))
                return TRUE;
        }
    }
    if (cr->Kind() == CK_IVAR)
    {
        POINTS_TO * pt1 = cr->Points_to(_opt_stab);
        if(pt1 && _alias_rule->Aliased_Memop(pt1, pt))
        {
            return TRUE;
        }
    }
    return FALSE;
}

BOOL
UselessStoreElimination::Aliased_with_base(CODEREP * cr1, POINTS_TO * pt)
{
    if (cr1->Kind() == CK_IVAR &&
        cr1->Ilod_base() &&
        cr1->Ilod_base()->Points_to(_opt_stab) != NULL &&
        _alias_rule->Aliased_Memop(cr1->Ilod_base()->Points_to(_opt_stab), pt))
        return TRUE;
                
    return FALSE;
}

BOOL
UselessStoreElimination::Is_Freed_in_BB(BB_NODE * bb, CODEREP * cr)
{
    POINTS_TO * pt = cr->Points_to(_opt_stab);
    
    Is_True(pt!= NULL, ("should find points_to set"));

    STMTREP_ITER stmt_iter (bb->Stmtlist());
    STMTREP * stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init())
    {
        // a free call before any uses
        if (stmt->Opr() == OPR_CALL)
        {
            if (!strcmp(ST_name(stmt->St()),"free") &&
                Aliased_with_base(stmt->Rhs()->Get_opnd(0), pt)) 
            {
                return TRUE;
            }

            if (Call_Can_be_Ignore(stmt, pt))
                continue;

            return FALSE;    

        }
        // should have no other uses
        if (stmt->Lhs() && Aliased_with_CR(stmt->Lhs(), pt))
        {
            return FALSE;
        }
        if (stmt->Rhs() && Aliased_with_CR(stmt->Rhs(), pt))
        {
            return FALSE;
        }
    }

    if (bb->Succ() == NULL ||
        bb->Succ()->Multiple_bbs()) 
        return FALSE;

    return Is_Freed_in_BB(bb->Succ()->Node(), cr);
}


// check whether this is a known_effect call that does not
// mod the user globals 
BOOL
UselessStoreElimination::Call_Can_be_Ignore(STMTREP * call, POINTS_TO *  pt)
{
    Is_True(OPERATOR_is_call(call->Opr()), ("Should be a call"));

    if (call->Opr() == OPR_CALL &&
        (!strcmp(ST_name(call->St()), "free") ||
         !strcmp(ST_name(call->St()), "malloc")))
         return TRUE;
    
    // intrinsic call should have no alias with pt
    if (call->Opr() == OPR_INTRINSIC_CALL)
    {
        if (Aliased_with_CR(call->Rhs(), pt)) {
            return FALSE;
        }    
        
        return TRUE;
    }
   
    if (_tracing)
       fprintf(stdout, "[ULSE] rejected: have unexpected calls in the loop\n");
       
    return FALSE;    
}


// keep track of all the def and use of a[] in the loop
// if first pass (first = TRUE), return TRUE when the first def of "a[]=c" 
// (after "a[]=0") is found;
// return FALSE when any unwanted def/use of a[] is found
// if second pass, record the copy of c (d=c), check whether 1) all the 
// def of a[] has the same rhs as c or d and 2) all the uses of a[] is in ne/eq and 
// compared with c or d 3) there is no other def to c or d
BOOL
UselessStoreElimination::Check_Uses_Defs_in_Loop(
    BB_LOOP* loop, STMTREP* st_stmt, BB_NODE * bb,
    POINTS_TO * pt, BOOL first)
{
    if (!loop->True_body_set()->MemberP(bb))
        return TRUE;

    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init())
    {
        if (stmt == st_stmt) continue;

        if (OPERATOR_is_call(stmt->Opr()))
        {
            // skip these "known" calls
            if (Call_Can_be_Ignore(stmt, pt)) continue;
            
            return FALSE;
        }
        // the first pass to find the first def of a[]
        if (first)
        {
            if ((stmt->Lhs() && !Check_First_Def_Coderep(stmt, bb, stmt->Lhs(), pt, TRUE)) ||
                (stmt->Rhs() && !Check_First_Def_Coderep(stmt, bb, stmt->Rhs(), pt, FALSE)))
                return FALSE;

            if (_candidate.def_defbb != NULL) return TRUE;
        }
        // the second pass to check all the defs and uses of a[]
        else
        {
            if ((stmt->Lhs() && !Check_Uses_Defs_Coderep(stmt, stmt->Lhs(), pt, TRUE)) ||
                (stmt->Rhs() && !Check_Uses_Defs_Coderep(stmt, stmt->Rhs(), pt, FALSE)))
                return FALSE;
        }

    }

    return TRUE;
}


// check this cr is the def of a[], left=TRUE means it comes from lhs of a stmt
BOOL
UselessStoreElimination::Check_First_Def_Coderep(
    STMTREP* stmt, BB_NODE *bb, CODEREP * cr, POINTS_TO * pt, BOOL left)
{
    if (cr->Kind() == CK_OP)
    {
        for (INT i = 0; i < cr->Kid_count(); i++)
        {
            if (!Check_First_Def_Coderep(stmt, bb, cr->Get_opnd(i), pt, left))
                return FALSE;
        }
    }

    if (cr->Kind() == CK_IVAR)
    {
        POINTS_TO * pt1 = cr->Points_to(_opt_stab);
        if(pt1 && _alias_rule->Aliased_Memop(pt1, pt))
        {
            // the use should be the left side of istore
            if ((stmt->Opr() == OPR_ISTORE) && left)
            {
                // the first def should be after the clear store
                // that is, the first def's bb should postdominates
                // the block where the clear store is
                if (Is_Def_Candidate(stmt->Rhs()) &&
                    bb->Postdominates(_candidate.clear_bb))
                {
                    _candidate.def = stmt->Rhs();
                    _candidate.def_defbb = stmt->Rhs()->Defbb();
                    _candidate.def_stmt = stmt->Rhs()->Defstmt();
                    return TRUE;
                }
            }

            Candidate_Clear();
            return FALSE;
        }
    }

    return TRUE;
}

// check wthether this cr is the def or use of a[]
// left=TRUE means it comes from the lhs of a stmt
// if it is the def, then its rhs should be same as c or its copy
// if it is the use, it should be in eq/ne and it is compared with c or its copy
// there is no other def to c or d
BOOL
UselessStoreElimination::Check_Uses_Defs_Coderep(
    STMTREP* stmt, CODEREP * cr, POINTS_TO * pt, BOOL left)
{
    if (cr->Kind() == CK_OP)
    {
        for (INT i = 0; i < cr->Kid_count(); i++)
        {
            if (!Check_Uses_Defs_Coderep(stmt, cr->Get_opnd(i), pt, left))
                return FALSE;
        }
    }

    if (cr->Kind() == CK_IVAR)
    {
        POINTS_TO * pt1 = cr->Points_to(_opt_stab);
        if(pt1 && _alias_rule->Aliased_Memop(pt1, pt))
        {
            // keep track of the defs of a[]: its rhs should be c or d
            if ((stmt->Opr() == OPR_ISTORE) && left)
            {
                if (!Same_as_first_def(stmt->Rhs()))
                    return FALSE;
            }
            // keep track of uses of a[]: in eq/ne and its rhs should be c or d
            else if ((stmt->Opr() == OPR_FALSEBR || stmt->Opr() == OPR_TRUEBR)
                 && (stmt->Rhs()->Opr() == OPR_EQ || stmt->Rhs()->Opr() == OPR_NE))
            {
                if (stmt->Rhs()->Opnd(0) == cr &&
                    !Same_as_first_def(stmt->Rhs()->Opnd(1)))
                {
                    return FALSE;
                }
                else if (stmt->Rhs()->Opnd(1) == cr &&
                    !Same_as_first_def(stmt->Rhs()->Opnd(0)))
                {
                    return FALSE;
                }
            }
            // no other uses of a[]
            else
                return FALSE;
        }
    }
    // keep track of the copy of c and whether there is redefinition of c 
    // or its copy
    if (cr->Kind() == CK_VAR)
    {
        // keep track of the copy of the def
        // here only the first copy is kept since it is enough for astar case
        // ideally, all the copies should be kept for more relaxed opportunity 
        if (stmt->Opr() == OPR_STID &&
            !left &&
            _candidate.def_copy == NULL &&
            cr->Aux_id() == _candidate.def->Aux_id()) 
        {
            _candidate.def_copy = stmt->Lhs();
        }
        else
        {
            // having the other def for def or def_copy?
            if (stmt != _candidate.def_stmt &&
                stmt->Opr() == OPR_STID &&
                left &&
                (cr->Aux_id() == _candidate.def->Aux_id() ||
                 (_candidate.def_copy != NULL &&
                  cr->Aux_id() == _candidate.def_copy->Aux_id())))
                return FALSE;
        }

    }
    
    return TRUE;
}

BOOL
UselessStoreElimination::Is_Def_Candidate(CODEREP * def)
{
    if (def->Kind() != CK_VAR) return FALSE;

    // the address of this def is not taken
    // we just need to keep track of the def of this def
    if (_opt_stab->Addr_saved(def->Aux_id()) ||
        _opt_stab->Addr_passed(def->Aux_id()))
        return FALSE;

    // def's def can be found    
    if (!def->Defstmt() || 
        def->Is_flag_set(CF_DEF_BY_PHI) || 
        def->Is_flag_set(CF_DEF_BY_CHI))
        return FALSE;
        
    return TRUE;
}

BOOL
UselessStoreElimination::Same_as_first_def(CODEREP* cr)
{
    // because there is no redef beteen def and use
    // so we just use aux_id to compare they are the same
    if (cr->Aux_id() == _candidate.def->Aux_id() ||
        (_candidate.def_copy != NULL &&
         cr->Aux_id() == _candidate.def_copy->Aux_id()))
        return TRUE;

    return FALSE;
}


// check whether the def of c include a increment (or a wrap around)
BOOL
UselessStoreElimination::Have_Increment_Def(
    BB_LOOP * loop, BB_NODE* bb, POINTS_TO * st_pt)
{
    if (!loop->True_body_set()->MemberP(bb))
        return FALSE;

    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init())
    {
        // find the def stmt, end the collection
        if (stmt == _candidate.def_stmt) return TRUE;

        if (OPERATOR_is_call(stmt->Opr()))
        {
            // skip these "known effect" calls
            if (Call_Can_be_Ignore(stmt, st_pt)) continue;
            
            return FALSE;

        }
        if (stmt->Opr() == OPR_ISTORE)
        {
            POINTS_TO * pt = stmt->Lhs()->Points_to(_opt_stab);
            if (pt && _alias_rule->Aliased_Memop(pt, st_pt))
            {
                INT const_val;
                INT ret = Istore_Inc_Const_Other(stmt, &const_val);
                switch (ret)
                {
                case Inc_Store:
                    _candidate.def_inc = TRUE;
                    break;
                case Const_Store:
                    // currently only allow one store const def
                    if (!_candidate.def_const)
                    {
                        _candidate.def_const = TRUE;
                        _candidate.def_const_val = const_val;
                        break;
                    }
                    // if more than one store const, go to the default case
                default:
                    Candidate_Clear();
                    return FALSE;
                }
           }
       }
    }

    return FALSE;

}

// return Inc_Store, if the store is an increment
// rturn Const_Store, if the store is a istore const
// return Other_Store, if the store is others
UselessStoreElimination::Store_Type
UselessStoreElimination::Istore_Inc_Const_Other(STMTREP * stmt, INT * const_val)
{
    Is_True(stmt->Opr() == OPR_ISTORE, ("something wrong"));

    if (stmt->Rhs()->Kind() == CK_CONST)
    {
        *const_val = stmt->Rhs()->Const_val();
        return Const_Store;
    }

    if (stmt->Rhs()->Kind() == CK_OP &&
        stmt->Rhs()->Opr() == OPR_ADD &&
        stmt->Rhs()->Get_opnd(1)->Kind() == CK_CONST &&
        stmt->Rhs()->Get_opnd(1)->Const_val() > 0 &&
        stmt->Rhs()->Get_opnd(0)->Ilod_base()->Aux_id() == stmt->Lhs()->Istr_base()->Aux_id())
    {
        return Inc_Store;
    }

    return Other_Store;
}


// transform the loop: add conditions to the loop
// the first condition is whether this is the first iteration of i ("if (i==0)")
// if there is a wrap around case, add another condition "OR (def == wrap_around_const)"
BOOL
UselessStoreElimination::Perform_transform(BB_LOOP *loop)
{
    // generate the conditions to guard the loop
    // 1) first condition is whether the first iteration of i loop
    CODEREP *iv_init, *iv, *iv_comp;
    AUX_ID iv_id;
    STMTREP_ITER stmt_iter(loop->Parent()->Preheader()->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init())
    {
        if (stmt->Opr() == OPR_STID)
        {   
            // get the iv's initial val
            iv_init = stmt->Rhs();
            iv_id = stmt->Lhs()->Aux_id();
            break;
        }    
    }
    STMTREP_ITER stmt_iter1(loop->Parent()->End()->Stmtlist());
    MTYPE iv_type;
    FOR_ALL_NODE(stmt, stmt_iter1, Init())
    {
        if (stmt->Opr() == OPR_FALSEBR)
        {
            // get iv of loop i
            if (stmt->Rhs()->Get_opnd(0)->Aux_id() == iv_id)
                iv = stmt->Rhs()->Get_opnd(0);
            else if (stmt->Rhs()->Get_opnd(1)->Aux_id() == iv_id)
                iv = stmt->Rhs()->Get_opnd(1);
            else
                return FALSE;
            iv_type = iv->Dtyp();
            break;
        }
    }
    
    // generate the comparison (iv == iv_init)
    iv_comp = Alloc_stack_cr(2+IVAR_EXTRA_NODE_CNT);
    iv_comp->Init_op(OPCODE_make_op(OPR_EQ, iv_type, iv_type), 2);
    iv_comp->Set_opnd(0,iv);
    iv_comp->Set_opnd(1, iv_init);
    iv_comp = _htable->Hash_Op(iv_comp);

    // generate the falsebr statement
    INT32 label = loop->Merge()->Labnam();
    STMTREP* br = CXX_NEW(STMTREP(OPC_FALSEBR), _cfg->Mem_pool());
    br->Set_rhs(iv_comp);
    br->Set_label_number(label);

    if (_tracing)
    {
        fprintf(stdout, "[ULSE] loop transformed: the following condition is added to the loop\n");
        iv_comp->Print(4,stdout);
    }
    // insert bb containing br before loop's preheader and 
    // connect it to loop's merge block
    BB_NODE * new_bb = _cfg->ULSE_insert_bb_and_merge(
                        br, loop->Preheader(), loop->Merge());

    
    // 2) generate another condition for the wrap around case
    // the condition is "def == wrap_around_const"
    // the two conditions are OR relation
    if (_candidate.def_const)
    {
        CODEREP * cond, *ldid, *ldconst;
        MTYPE def_type = _candidate.def->Dtyp();
        ldconst = Alloc_stack_cr(0);
        ldconst->Init_const(OPCODE_make_op(OPR_INTCONST, def_type, MTYPE_V),
                    _candidate.def_const_val);
        ldconst = _htable->Hash_Const(ldconst);
        cond = Alloc_stack_cr(2+IVAR_EXTRA_NODE_CNT);
        cond->Init_op(OPCODE_make_op(OPR_EQ, def_type, def_type), 2);
        cond->Set_opnd(0, ldconst);
        cond->Set_opnd(1, _candidate.def_stmt->Rhs());
        cond = _htable->Hash_Op(cond);
    
        label = loop->Preheader()->Labnam();
        br = CXX_NEW(STMTREP(OPC_TRUEBR), _cfg->Mem_pool());
        br->Set_rhs(cond);
        br->Set_label_number(label);
        
        if (_tracing)
        {
            fprintf(stdout, "OR\n");
            cond->Print(4, stdout);
        }
        // insert a block containing br before the previous condition block
        // and connect it to loop's preheader
        _cfg->ULSE_insert_bb_and_merge(br, new_bb, loop->Preheader());
        
    }

    return TRUE;
}

//===================================================================
// This function is to generate a condition block before before_bb
// and connect to merge_bb
//            before_bb's preds
//                  |
//                  V
//            condition block
//              /         \
//             |           V
//             |         before_bb
//             |           |
//             |           V
//              \         ....
//               \         |
//                V        V
//                 merge-bb
//                    |
//                    V
//====================================================================
BB_NODE *
CFG::ULSE_insert_bb_and_merge(STMTREP * stmt, 
    BB_NODE * before_bb, BB_NODE * merge_bb)
{
    BB_NODE* cond_bb = Create_and_allocate_bb (BB_LOGIF);
    cond_bb->Append_stmtrep(stmt);

    before_bb->Insert_Before(cond_bb);
    if (Feedback())
    {
        Feedback()->Add_node(cond_bb->Id());
    }
    
    BB_NODE* pred;
    BB_LIST_ITER pred_iter;
    FB_FREQ edge_freq = 0;
    FOR_ALL_ELEM (pred, pred_iter, Init(before_bb->Pred()))
    {
        pred->Replace_succ (before_bb, cond_bb);
        if (Feedback())
        {
            edge_freq = edge_freq +
                Feedback()->Get_edge_freq(pred->Id(), before_bb->Id());
            Feedback()->Move_edge_dest(pred->Id(), before_bb->Id(), cond_bb->Id());    
        }        
    }

    cond_bb->Set_pred(before_bb->Pred());
    before_bb->Set_pred(NULL);
    Connect_predsucc(cond_bb, before_bb);
    Connect_predsucc(cond_bb, merge_bb); 

    if (Feedback())
    {
        Feedback()->Add_edge(cond_bb->Id(), before_bb->Id(),
                            FB_EDGE_BRANCH_NOT_TAKEN, 0.5 * edge_freq);
        Feedback()->Add_edge(cond_bb->Id(), merge_bb->Id(),
                            FB_EDGE_BRANCH_TAKEN, 0.5 * edge_freq);
    }

    return cond_bb;

}


/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Copyright (C) 2007, University of Delaware, Hewlett-Packard Company, 
//  All Rights Reserved.
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

#include <vector>
#include <algorithm>
#include <stack>
using namespace std;

#include "tracing.h"
#include "id_map.h"
using idmap::ID_MAP;

#include "be_util.h"
#include "bb_node_set.h"
#include "glob.h"
#include "opt_alias_rule.h"
#include "opt_main.h"  // for COMP_UNIT
#include "opt_lmv.h"
#include "opt_ivr.h"

const INT LMV_HEURISTIC::_low_trip_count = 40; 
const INT LMV_HEURISTIC::_max_access_vectors = 6;
const INT LMV_HEURISTIC::_max_write_vectors = 3;
const INT LMV_HEURISTIC::_min_write_vectors = 1;

const float _dup_loop_freq_ratio = 0.1f;

extern void Rename_CODEMAP(COMP_UNIT *);

//====================================================================
//====================================================================
// 
//             Implementation of class LMV_HEURISTIC 
//
//====================================================================
//====================================================================

// Attempts to determine the benefit of applying loop multiversioning
// given a group of memory access ranges.  The assumption being that
// by asserting non-alias between all pairs, the performance of the
// loop in question will be improved significantly and overcome the
// cost of the runtime checks necessary to assert non-alias.
// return FALSE iff no such pairs are found.
//
// At present, the heuristic is targeted to specific code sequences,
// however more aggressive experimentation is possible with the
// -WOPT:loop_multiver_aggr=on option.  The multiversioning transformation
// has been well testing under that option, so as to allow this to
// serve as a test bed for identifying future opportunities.
//
// Currently LMV focuses on alias, however one may also wish to multiversion
// around a number of different attributed, e.g. loop trip count and tailor
// code generation appropriately for those versions.
//
// The better heuristics that is planed to be experimented soon are:
// 
//   - Examine in what degree a critical length in loop in shorten
//     with assumed no-alias
//   
//   - Examine how many redundant expressions will be eliminated with
//     assumed no-alias.
// 
//   - Evaluate the profitability that will be obtained by assuming the
//     some quantities (say, trip-counter) to be loop invariant. 
//
typedef struct {
  INT ptr_id;
  INT weight;
} PTR_INFO ;

struct less_weight {
  bool operator() (PTR_INFO info1,  PTR_INFO info2) 
     { return info1.weight < info2.weight; }
};

BOOL
LMV_HEURISTIC::Apply(MEM_GROUP_VECT &groups)
{
  if (WOPT_Enable_Loop_Multiver_Aggressive)
    return TRUE;

  INT n_vect = 0;
  INT n_write_vect = 0;
  INT n_read = 0;
  INT n_write = 0;
  for (MEM_GROUP_VECT_CITER iter = groups.begin();
        iter != groups.end(); iter++) {
    MEM_GROUP *grp = *iter;
    if (grp->Write())
      n_write_vect++;
    n_vect++;

    MEM_ACCESS_VECT& v = grp->Mem_accesses();
    for (MEM_ACCESS_VECT_ITER iter = v.begin ();
        iter != v.end (); iter++) {
      MEM_ACCESS *ma = (*iter);
      if (ma->Is_read())
        n_read++;
      else if (ma->Is_write())
        n_write++;
    }
  }

  if (n_write_vect < Min_write_vectors())
    return FALSE;

  if (n_write_vect == Max_write_vectors())
    return TRUE;

  return FALSE;
}

//====================================================================
//====================================================================
// 
//             Implementation of class LOOP_MULTIVER
//
//====================================================================
//====================================================================

LOOP_MULTIVER::LOOP_MULTIVER (COMP_UNIT* comp_unit):
  MEM_POOL_Constructor (&_mp, "loop multiversioning", TRUE), 
  _candidates (&_mp)
{
  _comp_unit = comp_unit;
  _htable = comp_unit->Htable();
  _opt_stab = comp_unit->Opt_stab();
  _cfg = _opt_stab->Cfg();
  _tracing = Get_Trace (TP_WOPT2, LOOP_MULTVER_FLAG);
  _blk_num = _stmt_num = _expr_num = _ld_num = _st_num = 0;
  _alias_rule = _opt_stab->Rule ();
  _local_mp = MEM_local_pool_ptr;
 
  _visited_cr = BS_Create_Empty (1024/*init size*/, &_mp);

}

#define MAX_BB_NUM    (32)
#define MAX_STMT_NUM  (128)
#define MAX_EXPR_NUM  (400)

// check to see whether the statistic data collected by Pass_initial_screen()
// exceed the threshold.
//
BOOL
LOOP_MULTIVER::Exceed_threshold (void) {
  return _blk_num > MAX_BB_NUM || _stmt_num > MAX_STMT_NUM || _expr_num > MAX_EXPR_NUM; 
}

// return the "latency" of given <cr>, the latency is imprecise and 
// the unit for the "latency" is not specified -- it is not in machine 
// cycles as traditional "latencies" are. We just need a number of 
// each operator so that the "weight" between operators can be 
// differentiated.
//
INT
LOOP_MULTIVER::Estimate_latency (const CODEREP* cr, BOOL lhs) {
  
  switch (cr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return 1;
  
  case CK_VAR:
    {
    ST* st = _opt_stab->Aux_stab_entry (cr->Aux_id())->St ();
    if (ST_class(st) == CLASS_PREG) 
      return 1;
    }
    return lhs ? 2 : 4;

  case CK_IVAR:
    return lhs ? 4 : 8; 

  case CK_DELETED:
    return 0;

  case CK_OP:
    break;

  default:
    Is_True (FALSE, ("Unexpected CODEREP kind"));
  }

  // Certainly need refined. 
  INT lat = 0;
  switch (cr->Opr()) {
  case OPR_MOD: 
  case OPR_DIV:  lat = 32;  break;
  case OPR_DIVREM : lat = 40; break;
  case OPR_MPY:
  case OPR_FLOOR:
  case OPR_CEIL:
     return 40; 
  case OPR_MLOAD:
  case OPR_MSTORE:
     return 12;
  }

  return 2;
}

//////////////////////////////////////////////////////////////////////////////
//
// Evaluate_stmt ()
//
//   Helper function of Pass_initial_screen(). It does the following things for
// Pass_initial_screen():
//    - collect the number of expression, load/store in given stmt.
//    - check to see whether the given statement as well as its expr disable 
//      loop-multiversioning -- e.g it is not possible to perform loop-multiver
//      if there is a call in current loop, and that call has side-effect. 
// 
// Evaluate_cr_rec()
//   Helper function of Evaluate_stmt(). Count the number of expr and their 
// weight recursively. 
//
//////////////////////////////////////////////////////////////////////////////
//
void
LOOP_MULTIVER::Evaluate_cr_rec (const CODEREP* cr, BOOL is_read) {

  if (!BS_MemberP (_visited_cr, cr->Coderep_id())) {
    _visited_cr = BS_Union1D (_visited_cr, cr->Coderep_id(), &_mp);
  } else {
    // do not visit a CODEREP twice 
    return;
  }

  switch (cr->Kind()) {
  case CK_LDA: _expr_num ++; break;
  case CK_CONST: break;
  case CK_RCONST: 
    { 
      TYPE_ID dty = cr->Dtyp(); 
      if (MTYPE_is_float(dty) || MTYPE_is_complex(dty)) {
        _expr_num ++;
      }
    }
    break;

  case CK_VAR:
    {
      ST* st = _opt_stab->Aux_stab_entry (cr->Aux_id())->St ();
      if (ST_class(st) != CLASS_PREG) {
        _expr_num ++;
        if (is_read) _ld_num ++ ; else _st_num ++; 
      }
    }
    break;

  case CK_IVAR:
    {
      if (is_read) _ld_num ++ ; else _st_num ++; 
      _expr_num ++;
      Evaluate_cr_rec (is_read ? cr->Ilod_base() : cr->Istr_base(), TRUE);
    }
    break;
     
  case CK_OP:
    _expr_num ++;
    for (INT i = 0; i < cr->Kid_count() ; i++) {
      Evaluate_cr_rec (cr->Opnd(i), TRUE);
    }
    break;

  case CK_DELETED:
    break;
  }
}

BOOL
LOOP_MULTIVER::Evaluate_stmt (const STMTREP* stmt) {
  switch (stmt->Opr()) {
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
  case OPC_BACKWARD_BARRIER:
  case OPC_FORWARD_BARRIER:
    return FALSE;

  default:
    if (OPCODE_is_black_box (stmt->Op())) {
      return FALSE;
    }
  }
 
  if (stmt->Rhs()) Evaluate_cr_rec (stmt->Rhs(), TRUE);
  if (stmt->Lhs()) Evaluate_cr_rec (stmt->Lhs(), FALSE);
  ++ _stmt_num;

  return TRUE;
}

///////////////////////////////////////////////////////////////////////////
//
// Helper function of Gen_test_cond(), it returns the CODEREP
// holding value "<ptr1> + val".
//
////////////////////////////////////////////////////////////////////////////
//
CODEREP*
LOOP_MULTIVER::Gen_add_expr (CODEREP* ptr, CODEREP *val) {

  CODEREP* tmp_cr = Alloc_stack_cr(2/*at most 2 kids*/+IVAR_EXTRA_NODE_CNT);

  OPCODE opcode = (MTYPE_byte_size(ptr->Dtyp()) == 8) ? OPC_U8ADD : OPC_U4ADD;
  tmp_cr->Init_op(opcode, 2);
  tmp_cr->Set_opnd(0, ptr);
  tmp_cr->Set_opnd(1, val);

  ptr->IncUsecnt();
  val->IncUsecnt();
  return _htable->Hash_Op (tmp_cr);
}

////////////////////////////////////////////////////////////////////////////
//
// Helper function of Gen_test_cond(), it returns the CODEREP
// holding value "<ptr>+ofst".
//
////////////////////////////////////////////////////////////////////////////
//
CODEREP*
LOOP_MULTIVER::Gen_add_expr (CODEREP* ptr, INT ofst) {

  if (ofst == 0) return ptr;

  CODEREP* tmp_cr = Alloc_stack_cr(2/*at most 2 kids*/+IVAR_EXTRA_NODE_CNT);
  CODEREP* ofst_cr; 

  tmp_cr->Init_const (ofst > 0 ? MTYPE_U4 : MTYPE_I4, (INT64)ofst);
  ofst_cr = _htable->Hash_Const (tmp_cr); 
   
  return Gen_add_expr(ptr,ofst_cr);
}

////////////////////////////////////////////////////////////////////////////
//
// Helper function of Gen_test_cond(), it returns the CODEREP
// that contains the result of disambiguating <ar1> and <ar2>
//
////////////////////////////////////////////////////////////////////////////
//
CODEREP*
LOOP_MULTIVER::Gen_range_expr (CODEREP *ptr1, const ADDR_LINEAR_EXPR_RANGE &ar1,
                               CODEREP *ptr2, const ADDR_LINEAR_EXPR_RANGE &ar2)
{
  CODEREP *ptr1_high, *ptr2_high;
  CODEREP *ptr1_low  = Gen_add_expr (ptr1, ar1.low.Const_val());
  CODEREP *ptr2_low  = Gen_add_expr (ptr2, ar2.low.Const_val());
  if (ar1.high.Is_const())
    ptr1_high = Gen_add_expr (ptr1, ar1.high.Const_val());
  else
  {
    CODEREP *ptr1_tmp = Gen_add_expr(ar1.high.cr(),ar1.high.Const_part());
    ptr1_high = Gen_add_expr(ptr1,ptr1_tmp);
  }
  if (ar2.high.Is_const())
    ptr2_high = Gen_add_expr (ptr2, ar2.high.Const_val());
  else
  {
    CODEREP *ptr2_tmp = Gen_add_expr(ar2.high.cr(),ar2.high.Const_part());
    ptr2_high = Gen_add_expr(ptr2,ptr2_tmp);
  }

  // generate the expr : (p1h < p2l || p2h < p1l)
  CODEREP* tmp_cr = Alloc_stack_cr(2/*2 kids */);
  BOOL use_8_bytes = (MTYPE_byte_size(ptr1_low->Dtyp()) == 8);

  CODEREP* cr_1; // generate "p1h < p2l"
  tmp_cr->Init_op (use_8_bytes ? OPC_I4U8LT : OPC_I4U4LT, 2);
  tmp_cr->Set_opnd (0, ptr1_high);
  tmp_cr->Set_opnd (1, ptr2_low);
  cr_1 = _htable->Hash_Op (tmp_cr);

  CODEREP* cr_2; // generate "p2h < p1l"
  tmp_cr->Init_op (use_8_bytes ? OPC_I4U8LT : OPC_I4U4LT, 2);
  tmp_cr->Set_opnd (0, ptr2_high);
  tmp_cr->Set_opnd (1, ptr1_low);
  cr_2 = _htable->Hash_Op (tmp_cr);

  tmp_cr->Init_op (OPC_I4CIOR, 2);
  tmp_cr->Set_opnd (0, cr_1);
  tmp_cr->Set_opnd (1, cr_2);

  CODEREP* cr = _htable->Hash_Op (tmp_cr);
  return cr;
}

CODEREP *
LOOP_MULTIVER::Gen_range_and_expr(CODEREP *range1, CODEREP *range2)
{
  Is_True(range1 != NULL || range2 != NULL,("Expected a non-NULL coderep"));
  if (range1 == NULL)
    return range2;
  else if (range2 == NULL)
    return range1;

  CODEREP *tmp_cr = Alloc_stack_cr(2/* 2 kids */);
  tmp_cr->Init_op(OPC_I4CAND, 2);
  tmp_cr->Set_opnd(0,range1);
  tmp_cr->Set_opnd(1,range2);

  CODEREP *cr = _htable->Hash_Op(tmp_cr);
  return cr;
}

////////////////////////////////////////////////////////////////////////////
//
// Generate a expression which is going to be evaluated at runtime. If 
// the evaluation turns out to be true, it means the alias does not happen. 
// In that case, the faster verion should be executed. 
// 
// We set the original loop be the the "faster" verion. 
//
////////////////////////////////////////////////////////////////////////////
//
CODEREP*
LOOP_MULTIVER::Gen_test_cond (LMV_CANDIDATE* cand) {

  if (_tracing)
    fprintf(TFile,"Mem Groups for candidate at BB%d\n",
        cand->Loop()->Header()->Id());
  // Do these checkes need to be moved earlier, i.e. during
  // the heuristic check when computing memory ranges?
  const MEM_GROUP_VECT &mem_groups = cand->Mem_groups();
  for (MEM_GROUP_VECT_CITER val_iter = mem_groups.begin();
      val_iter != mem_groups.end(); val_iter++) {
    MEM_GROUP *grp = *val_iter;

    if (_tracing)
      grp->Print(TFile);

    const MEM_RANGE &mr = *grp->Mem_range();
    const ADDR_LINEAR_EXPR_RANGE &r = mr.Access_range();
    // TODO: Handle non-constant lower bound
    Is_True(r.low.Is_const(),
        ("cannot handle non-const lower bound for now"));

    // TODO: Handle this case
    if ( mr.Base_is_symbol())
      return NULL;

    if (mr.Base_ptr()->Kind() != MA_PTR_PREG &&
        mr.Base_ptr()->Kind() != MA_PTR_SYM)
      return NULL;

    // TODO: Handle non-loop-invariant cases
    CODEREP* ptr = mr.Base_ptr()->Coderep();
    if (!cand->Loop()->Invariant_cr(ptr))
      return NULL;

  }
  if (_tracing)
    fprintf(TFile,"End Mem Groups\n");

  // Map of access ranges containing a write that have all ready
  // been checked against all other writes.  This prevents redundant
  // runtime checks amongst the write access ranges.
  // Essentially, this is a visited set.  Is there a cheaper way?
  ID_MAP<INT,MEM_GROUP*> write_checked(32,0,&_mp,FALSE);
  write_checked.Init();
  CODEREP *prev_range_chk = NULL;

  for (MEM_GROUP_VECT_CITER iter = mem_groups.begin();
      iter != mem_groups.end(); iter++) {
    MEM_GROUP *grp1 = *iter;
    if (grp1->Write()){
      for (MEM_GROUP_VECT_CITER iter2 = mem_groups.begin();
          iter2 != mem_groups.end(); iter2++) {
        MEM_GROUP *grp2 = *iter2;
        if (!_alias_rule->Aliased_Memop
            ((grp1->Mem_accesses())[0]->Points_to(_opt_stab),
             (grp2->Mem_accesses())[0]->Points_to(_opt_stab),
             (TY_IDX)0, (TY_IDX)0))
          continue;

        if(grp1 != grp2 && write_checked.Lookup(grp2) == 0) {
          const MEM_RANGE& r1 = *grp1->Mem_range();
          const MEM_RANGE& r2 = *grp2->Mem_range();

          const ADDR_LINEAR_EXPR_RANGE ar1 = r1.Access_range();
          const ADDR_LINEAR_EXPR_RANGE ar2 = r2.Access_range();

          // TODO: Handle non-constant range
          Is_True (ar1.low.Is_const () && ar2.low.Is_const(),
              ("currently cannot handle non-constant lower bound"));

          MA_POINTER *p1, *p2;
          p1 = r1.Base_ptr ();
          p2 = r2.Base_ptr ();

          CODEREP* ptr1 = p1->Coderep();
          CODEREP* ptr2 = p2->Coderep();
          CODEREP *range_chk = Gen_range_expr(ptr1,ar1,ptr2,ar2);
          prev_range_chk = Gen_range_and_expr(prev_range_chk,range_chk);
        }
      }
      write_checked.Insert(grp1,1);
    }
  }
  Is_True(prev_range_chk,("Non-NULL range check CODEREP tree expected"));
  if (_tracing) {
    fprintf (TFile, "The precondition predicate (kid of true-branch) is:\n");
    prev_range_chk->Print (0, TFile);
  }
  return prev_range_chk;
}

//////////////////////////////////////////////////////////////////////////////////
//
// Not_applicable() take a glance of the loop in quesition to see whether it is 
// legal for multiversioning. No cost issue is taken into accout in this function.
//
//////////////////////////////////////////////////////////////////////////////////
//
BOOL 
LOOP_MULTIVER::Not_applicable (BB_LOOP* loop) {
  // Currenly, applicable only to inner most loop
  if (loop->Child()) {
    if (_tracing) {
      fprintf (TFile, "Not innermost loop, give up\n");
    }
    return TRUE;
  }

  // don't try to outsmart OpenMP pragma 
  if (loop->Is_flag_set(LOOP_IS_MP) || loop->Is_flag_set(LOOP_IS_PDO)) {
    if (_tracing) {
      fprintf (TFile, "Don't try to outsmart OpenMP pragma\n");
    }
    return TRUE;
  }

  // Check to see whether CFG agree
  if (!_cfg->LMV_eligible_for_multiversioning(loop, _tracing)) 
    return TRUE; 

  return FALSE;
}

//////////////////////////////////////////////////////////////////////////////////
//
//  Take a closer look to screen out the ineligible loop 
//
//////////////////////////////////////////////////////////////////////////////////
//
BOOL 
LOOP_MULTIVER::Pass_initial_screen (const BB_LOOP* loop) {

  // do not do multiversion for loop with small trip count 
  CODEREP* iv;
  if (_agg_mode && (iv = loop->Iv()) && iv->Kind() == CK_CONST && 
     iv->Const_val () < LMV_HEURISTIC::Low_trip_count_threshold ()) {
    if (_tracing)
      fprintf(TFile, "The trip count is %d smaller than the threshold %d\n",
          (INT)iv->Const_val(), (INT)LMV_HEURISTIC::Low_trip_count_threshold ());
    return FALSE;
  }

  BB_NODE_SET_ITER iter;
  BB_NODE* blk;
 
  _visited_cr = BS_ClearD (_visited_cr);

  // Take a glance of each block to see whether the loop in question is 
  // appropriate for multiversioning.
  //
  _blk_num = 0;
  BB_NODE_SET* loop_body = loop->True_body_set(); 
  FOR_ALL_ELEM (blk, iter, Init(loop_body)) {
    if (blk->Hascall()) {
      if (_tracing) {
        fprintf (TFile, 
	         "give up due to call in block %d\n", 
                 blk->Id());   
      }
      return FALSE;
    }

    _blk_num ++;
    // too many blocks
    if (_blk_num > MAX_BB_NUM) {
      if (_tracing) {
        fprintf (TFile, "block number exceed threshold %d\n", MAX_BB_NUM);
      }
      return FALSE;  
    }

    if (!blk->Clonable (TRUE/*allow-loop-cloning*/)) {
      if (_tracing) {
        fprintf (TFile, "block %d is not clonable\n", blk->Id());
      }
      return FALSE;
    }

    if (blk->Hascall()) {
      // Currently, we cannot handle loop with call. 
      if (_tracing) {
        fprintf (TFile, "give up due to call\n");
      }
      return FALSE;
    }

    switch (blk->Kind()) {
    case BB_VARGOTO:
    case BB_IO:
    case BB_SUMMARY:
      if (_tracing) {
        fprintf (TFile, "Don't know how to deal with BB:%d of kind %d",
	                 blk->Id(), blk->Kind());
      }
      return FALSE;

    case BB_ENTRY:
    case BB_EXIT:
    case BB_REGIONSTART:
    case BB_REGIONEXIT:
      Is_True (FALSE, ("BB:%d of kind %d should excluded by BB_NODE::Clonable",
                       blk->Id(), blk->Kind()));
      return FALSE;
    }
  }

  // Take a closer look of each block by examining each statement to see 
  // whether it is appropriate for multiversioning 
  //
  _stmt_num = _expr_num = _ld_num = _st_num = 0;
  FOR_ALL_ELEM (blk, iter, Init(loop->True_body_set())) {
   
    STMTREP_CONST_ITER stmt_iter (blk->Stmtlist());
    const STMTREP* stmt;
    INT wrap_cnt = 0;
    FOR_ALL_NODE (stmt, stmt_iter, Init()) {
      if (!Evaluate_stmt (stmt)) {
         if (_tracing) {
	   fprintf (TFile, 
	        "unable to conduction loop multiversioning due to this statement:\n");
	   stmt->Print(TFile);
	 }
      }

      if (wrap_cnt ++ > 128) {
        // wrap_cnt is used to prevent Exceed_threshold() from 
        // being called too frequently.
        wrap_cnt = 0;
        if (Exceed_threshold ()) {
	  if (_tracing) {
	    fprintf (TFile, "give up because the size exceed threshold");
	    fprintf (TFile, 
	             "block:%d, statement:%d, expression:%d, load:%d, store:%d\n",
		     _blk_num, _stmt_num, _expr_num, _ld_num, _st_num);
	  }
          return FALSE;
        }
      }
    } // end of FOR_ALL_NODE 

    if (Exceed_threshold ()) {
      if (_tracing) {
	fprintf (TFile, "give up because the size exceed threshold");
	fprintf (TFile, 
	         "block:%d, statement:%d, expression:%d, load:%d, store:%d\n",
		 _blk_num, _stmt_num, _expr_num, _ld_num, _st_num);
      }
      return FALSE;
    }
  } // end of FOR_ALL_ELEM

  return TRUE;
}

///////////////////////////////////////////////////////////////////////////
//
// Figure out whether <loop> is a candidate. If yes, put this candidate 
// into <_candidates> vector. 
//
// Since there may be several candidates in a single PU, performing 
// multiversioning upon all these candidates may not be realistic. If that 
// happen, only few candidates with top priority are select for the 
// final code generation. Therefore, the multiversioning code generation 
// should not be conducted right after a single candidate is identified. It 
// is done when all candidates are identified and collected in <_candidates> 
// vector. 
//
////////////////////////////////////////////////////////////////////////////
//
void
LOOP_MULTIVER::Identify_candidate (BB_LOOP* loop) {


  MEM_POOL_Popper lmp (_local_mp);

  IVR ivr(_comp_unit,FALSE);
  LMV_LOOP_INFO* loopinfo =
      CXX_NEW(LMV_LOOP_INFO(loop, &_mp,ivr,_tracing), &_mp);
  
  MEM_ACCESS_ANALYZER* maa = 
    CXX_NEW (MEM_ACCESS_ANALYZER(_opt_stab, loopinfo, &_mp, _tracing), &_mp);
  maa->Analyze_mem_access ();

  MEM_GROUP_VECT mem_groups(&_mp);
  if (!maa->Assemble_aliased_mem_groups(_alias_rule,mem_groups))
    return;

  LMV_HEURISTIC heur(&_mp, this, loopinfo, _opt_stab, maa,
                    _alias_rule, _tracing);
  if (heur.Apply(mem_groups)) {
    LMV_CANDIDATE* cand = CXX_NEW (LMV_CANDIDATE(&_mp), &_mp);
    cand->Set_mem_access_analyzer (maa);
    cand->Set_mem_groups(mem_groups);
    cand->Set_loop (loop);
    _candidates.push_back (cand);
  }
}

//////////////////////////////////////////////////////////////////////////
//
//   Identify all candidates in current PU. All candidates are 
// collected by <_candidates> vector. 
// 
//////////////////////////////////////////////////////////////////////////
//
void
LOOP_MULTIVER::Identify_candidates (void) {

  BB_LOOP* loop_list = _cfg->Analyze_loops();

  BB_LOOP_ITER loop_iter(loop_list);
  BB_LOOP* loop;

  // go through all innermost loops 
  stack<BB_LOOP*> loop_nest;
  loop_nest.push(loop_list);
  while (!loop_nest.empty()) {
    BB_LOOP *cur_loop = loop_nest.top();
    loop_nest.pop();

    // Visit all nodes at this nesting level
    BB_LOOP_ITER loop_nest_iter(cur_loop);
    FOR_ALL_NODE(loop,loop_nest_iter,Init()) {
      if (loop->Child())
        loop_nest.push(loop->Child());

      if (_tracing) {
        fprintf(TFile,"Examining Loop with header BB%d\n",
            loop->Header()->Id());
      }
      if (Not_applicable(loop) || !Pass_initial_screen (loop)) {
        // The loop is not eligible for multiversioning
        continue;
      }

      Identify_candidate (loop);
    }
  }

  if (_tracing) {
    fprintf (TFile, "End Loop Multiversioning\n%s\n", DBar);
  }
}

////////////////////////////////////////////////////////////////////////
//
// Sort the candidates in the descending order of priority. 
//
////////////////////////////////////////////////////////////////////////
//
void
LOOP_MULTIVER::Sort_candidates (void) {
  // not yet implemented 
}

void
LOOP_MULTIVER::Annotate_alias_group_helper 
  (const MEM_ACCESS_VECT& memops, LMV_ALIAS_GROUP alias_grp) {

  MEMOP_ANNOT_CR_SR_MGR* annot_mgr = _opt_stab->Cr_sr_annot_mgr();
  MEMOP_ANNOT_ITEM annot;
  annot.Set_LMV_alias_group (alias_grp);

  for (MEM_ACCESS_VECT_CITER iter = memops.begin ();
       iter != memops.end(); iter++) {
    const MEM_ACCESS* access = *iter;
    Is_True (access->Is_read() || access->Is_write(), 
             ("neither read nor write"));
    CODEREP* cr = access->Coderep ();
    POINTS_TO* pt = cr->Points_to (_opt_stab);
    pt->Set_LMV_alias_group (alias_grp);

    if (access->Is_write()) {
      annot_mgr->Add_annot (access->Stmtrep(), annot);
    }
  }
}

void
LOOP_MULTIVER::Annotate_alias_group (LMV_CANDIDATE* cand)
{
  INT grp_num = 1;
  const MEM_GROUP_VECT &mem_groups = cand->Mem_groups();
  for (MEM_GROUP_VECT_CITER iter = mem_groups.begin();
      iter != mem_groups.end(); iter++) {
    const MEM_ACCESS_VECT &memops = (*iter)->Mem_accesses();
    LMV_ALIAS_GROUP alias_grp =
        Gen_LMV_alias_group(cand->Loop()->Header()->Id(),grp_num++);
    Annotate_alias_group_helper(memops,alias_grp);
  }

  // Now that we have attached alias groups to the memory
  // references in the original loop, flag the loop to indicate same.
  // We do this by setting a flag on the LOOP_INFO.  If none exists
  // we will manufacture one to be consumed if this loop is raised
  // to a do loop.
  BB_LOOP *loop = cand->Loop();
  BB_NODE *body = loop->Body();
  WN *loop_info = body->Label_loop_info();
  if (loop_info)
    WN_Set_Multiversion_Alias(loop_info);
  else {
    loop_info = WN_CreateLoopInfo(NULL,loop->Wn_trip_count(),
        0,loop->Depth(),0);
    WN_Set_Multiversion_Alias(loop_info);
    body->Set_label_loop_info(loop_info);
  }
}

/////////////////////////////////////////////////////////////////////////
//
//   As its name suggests, this function is to perform transoformation 
// on given candidate. 
//
/////////////////////////////////////////////////////////////////////////
//
BOOL
LOOP_MULTIVER::Perform_transformation (LMV_CANDIDATE* cand) {
  
  CODEREP* precond = Gen_test_cond (cand);
  if (!precond) {
    // it is possible because currently we cannot handle some cases.
    return FALSE;
  }

  // We feel more comfortable to have code generation done by CFG in that 
  // code generation entails lot of knowledge of CFG's internal, trick, 
  // assumption and lots of other good and bad things.
  //
  // LMV_CFG_ADAPTOR is employed to serve as a "adaptor" between CFG and 
  // LOOP_MULTIVER.
  //
  LMV_CFG_ADAPTOR adaptor(&_mp, _cfg, _tracing, cand->Loop(), precond);
  _cfg->LMV_clone_loop (&adaptor);

  Annotate_alias_group (cand);
  return TRUE;
}

///////////////////////////////////////////////////////////////////////////
//
//        The driver of the loop multiversioning
//
///////////////////////////////////////////////////////////////////////////
//
void
LOOP_MULTIVER::Perform_loop_multiversioning (void)
{
  if (_tracing) {
    fprintf (TFile, 
             "Begin Loop Multiversioning for PU:%d %s\n%s\n", 
	     Current_PU_Count(),
	     ST_name(Get_Current_PU_ST ()), DBar);

    fprintf (TFile, "The CFG before loop multiversioning\n");
    _opt_stab->Cfg()->Print (TFile, TRUE, (IDTYPE)-1);
    fprintf (TFile, "The end of CFG before loop multiversioning\n\n");
  }
  
  Identify_candidates ();
  if (_candidates.size () == 0) return;

  for (LMV_CAND_VECT_ITER iter = _candidates.begin ();
       iter != _candidates.end(); iter++) {
    LMV_CANDIDATE* cand = *iter; 
    BOOL xform = Perform_transformation (cand);

    if (xform && _tracing)
      fprintf(TFile,"Multiversioned loop file: %s, line %d\n",
          Orig_Src_File_Name,
          Srcpos_To_Line(WN_Get_Linenum(cand->Loop()->Orig_wn())));
  }

  // reconstruct the CFG 
  _cfg->Invalidate_loops();
  _cfg->Invalidate_and_update_aux_info(TRUE);
  _cfg->Analyze_loops();

  if (_tracing) {
    fprintf (TFile, "\n%s\t\tAfter loop multiversioning\n%s", DBar, DBar);
    _cfg->Print (TFile, TRUE, (IDTYPE)-1);
    fprintf (TFile, 
             "End Loop Multiversioning for PU:%d %s\n%s\n", 
	     Current_PU_Count(),
	     ST_name(Get_Current_PU_ST ()), DBar);
  }

  Rename_CODEMAP(_comp_unit);
}


//====================================================================
//====================================================================
//
//   Implementation of LMV_CFG_ADAPTOR 
//
//====================================================================
//====================================================================
//
void
LMV_CFG_ADAPTOR::Get_all_src_bb (std::list<BB_NODE*>& bb_list) const {

    bb_list.clear ();

    const std::map<IDTYPE, IDTYPE>& map = _old_to_new_blk;
    for (std::map<IDTYPE, IDTYPE>::const_iterator iter = map.begin (),
         iter_end = map.end (); iter != iter_end; iter++) {
        
        IDTYPE src_id = (*iter).first;
        Is_True (src_id != 0 && (*iter).second != 0, ("Invalid map entry"));

        bb_list.push_back (_cfg->Get_bb (src_id));
    }
}


//====================================================================
//====================================================================
//
//   From this points on define some CFG class functions which will be 
// called to generate the loop multiversioning code.
//
//====================================================================
//====================================================================
//


// Check to see whether the loop in question is appropriate for multiversioning.
//
BOOL
CFG::LMV_eligible_for_multiversioning (const BB_LOOP* loop, BOOL trace) {

  // Multiple preheaders renders code generation complicated.
  //
  INT count = 0;
  BB_LIST_ITER pred_iter;
  BB_NODE* pred;
  FOR_ALL_ELEM (pred, pred_iter, Init(loop->Header()->Pred())) {
    if (!Is_backedge (pred, loop->Header())) {
      count++;
    }
  }
  if (count != 1) {
    if (trace) { 
      fprintf (TFile, "No unique preheader\n"); 
    }
    return FALSE; 
  }

  // Make sure preheader and merge block are set properly  
  if (loop->Preheader() == NULL || loop->Merge() == NULL) {
    if (trace) { 
      fprintf (TFile, 
        "No preheader or merge blocks or they are not set properly \n"); 
    };
    return FALSE;
  }
  
  // Make sure the blocks related to this loop are permuted in this order along the 
  // prev/next link: preheader, loop body blocks, merge block. And there should be no
  // "hole" between them, meaning those blocks are adjacent to each other. This 
  // werid requirement is to make code-emit happy. Code-emit makes some werid assumptions 
  // with CFG.
  //
  BB_NODE* phdr = loop->Preheader();
  BB_NODE* next_blk = phdr->Next(); 
  BB_NODE_SET* loop_body = loop->True_body_set ();
  BOOL there_is_hole = FALSE;

  if (phdr->Next() != loop->Header()) {
    there_is_hole = TRUE;
  }

  INT cnt = 0;
  do {
    if (loop_body->MemberP(next_blk)) {
      cnt ++; 
      next_blk = next_blk->Next();    
    } else {
      if (next_blk != loop->Merge()) {
        there_is_hole = TRUE;
      }
      break;
    }
  } while (next_blk);
  
  // It is possible there is a hole in the loop body, for example: 
  //   for (....) { if (cond) break; } 
  // the then-clause of the if-statement is not belong to the loop body, 
  // but it is interposed between loop body blocks.
  //
  // TODO: add the empty block mentioned above into loop body to 
  //   enable the multiversioning 
  //
  if (there_is_hole || cnt != loop_body->Size()) {
    if (trace) {
      fprintf (TFile, "The loop body blocks are not adjacent along prev/next link");
    }
    return FALSE;
  }

  // Make sure all merge-block's predecessors are blocks of loop body
  //
  FOR_ALL_ELEM (pred, pred_iter, Init(loop->Merge()->Pred())) {
    if (!loop_body->MemberP (pred)) {
      if (trace) {
        fprintf (TFile, 
	  "One of merge-blk's predecessor BB:%d is not in loop body", pred->Id());
      }
      return FALSE;
    }
  }

  // When a loop is multiversioned, both loop body and preheader are cloned.
  // We need to copy preheader because emiter mades some assumptions of the 
  // preheader. For instance, it assumes the init statement of the IV is the 
  // last one of the preheader. If emiter cannot find the init statement 
  // there, it simply gives up converting a while-loop into a do-loop.
  //
  if (!phdr->Clonable (TRUE)) {
    if (trace) {
      fprintf (TFile, "Preheader BB:%d is not clonable\n",phdr->Id ());
    }
    return FALSE;
  }

  // If there is a break from loop, the successor should be the merge block or 
  // the break is done by a explicit goto statement.
  // 
  BB_NODE* blk;
  BB_NODE_SET_ITER iter;
  FOR_ALL_ELEM (blk, iter, Init(loop_body)) {

    BB_NODE* succ;
    BB_LIST_ITER succ_iter;
    FOR_ALL_ELEM (succ, succ_iter, Init(blk->Succ())) {

      if (!loop_body->MemberP (succ) && succ != loop->Merge()) {
        STMTREP* br = blk->Branch_stmtrep();
        if (!br || br->Op () != OPC_GOTO) {
	  if (trace) {
	    fprintf (TFile, 
	       "No explicit goto statment between loop body block BB:%d and BB:%d\n",
	       blk->Id(), succ->Id());
	  }
	  return FALSE;
	}
      }

    }// end of for all succ

  }
  
  return TRUE;
}


///////////////////////////////////////////////////////////////////////////////////
//
// Clone block <src> coming from source loop: statements and other stuff are copied, 
// map between them are established,....
//
//////////////////////////////////////////////////////////////////////////////////
//
BB_NODE* 
CFG::LMV_clone_block (const BB_NODE* src, LMV_CFG_ADAPTOR* adaptor) {

  BB_NODE* clone = Create_and_allocate_bb (src->Kind()); 
  Clone_bb (src->Id(), clone->Id(), FALSE); 
  clone->Set_loopdepth (src->Loopdepth());
  clone->Set_rid_id (src->Rid_id());
  clone->Set_rid(src->Rid());
  clone->Set_layout_id(src->layout_Id());
  clone->Set_flag (src->Flag());
  // When cloning a 'DO' loop we lower the loop back to a while loop
  // because we cannot manufacture (yet) all the state contained in
  // the BB_LOOP structure for a 'DO' loop.  The loop should be
  // raised again during "emit".
  if ( src->Kind() == BB_DOEND )
    clone->Set_kind(BB_WHILEEND);
  else if ( src->Kind() == BB_DOSTEP )
    clone->Set_kind(BB_GOTO);

  // DCE requires non-null PHI-list to transfer dead phi functions
  // from one block to another.
  clone->Set_phi_list (CXX_NEW(PHI_LIST(clone), Mem_pool()));
  clone->Set_linenum (src->Linenum());
  clone->Set_freq (src->Freq());

  // Set a map between original block and duplicated block
  adaptor->Map_cloned_bb (src, clone);

  if (src->Labnam() != 0) {
    clone->Add_label (this);
    adaptor->Map_cloned_label (src->Labnam(), clone->Labnam());
    BB_NODE* t = const_cast<BB_NODE*>(src);
    if (t->Label_stmtrep ()) {
      clone->Add_label_stmtrep (Mem_pool ());
    }
  }

  return clone;
}

// Create a block like <model>, the kind of the new block is specified.
//
BB_NODE*
CFG::LMV_create_alike_block (BB_KIND kind, BB_NODE* model) {

  BB_NODE* new_bb = Create_and_allocate_bb (kind);
  new_bb->Set_loopdepth (model->Loopdepth());
  new_bb->Set_rid_id (model->Rid_id());
  new_bb->Set_rid(model->Rid());
  new_bb->Set_layout_id(model->layout_Id());
  new_bb->Set_flag (model->Flag());

  // DCE requires non-null PHI-list to transfer dead phi functions
  // from one block to another.
  new_bb->Set_phi_list (CXX_NEW(PHI_LIST(new_bb), Mem_pool()));
  new_bb->Set_linenum (model->Linenum());
  new_bb->Set_freq (model->Freq());

  return new_bb;
}

////////////////////////////////////////////////////////////////////////////
//
//  Establish the Pred/Succ relationship for cloend loop
//
////////////////////////////////////////////////////////////////////////////
//
void
CFG::LMV_clone_pred_succ_relationship (LMV_CFG_ADAPTOR* adaptor) {
  
  BB_LOOP* src_loop = adaptor->Src_loop();

  BB_NODE* src_blk;
  BB_NODE_SET_ITER bb_iter;   
  FOR_ALL_ELEM (src_blk, bb_iter, Init(src_loop->True_body_set ())) {

    BB_NODE* clone = adaptor->Get_cloned_bb (src_blk);

    BB_LIST_ITER pred_iter;
    BB_NODE* pred;
    FOR_ALL_ELEM (pred, pred_iter, Init(src_blk->Pred())) {
      BB_NODE* t = adaptor->Get_cloned_bb (pred);
      if (t) {
        Connect_predsucc (t, clone);
      } else {
        Is_True (pred == adaptor->Src_loop()->Preheader(),
	         ("Block BB:%d is not cloned", pred->Id()));
      }
    }
  }

  // special handling of header and preheader
  Connect_predsucc (adaptor->Cloned_loop_preheader (), 
                    adaptor->Cloned_loop_header ());

  // special handling of merge block
  BB_NODE* new_merge = adaptor->Cloned_loop_merge ();

  BB_LIST_ITER pred_iter;
  BB_NODE* pred;
  FOR_ALL_ELEM (pred, pred_iter, Init(src_loop->Merge()->Pred())) {
    BB_NODE* cloned_pred = adaptor->Get_cloned_bb (pred);
    Connect_predsucc (cloned_pred, new_merge);
  }
}

//////////////////////////////////////////////////////////////////////////////
//
// This function is supposed to do:
//   - clone all blocks comprising loop body, and 
//   - chain all cloned blocks together via a single prev/next list. The 
//     order of the cloned block in the list is also copied from source loop. 
//
//////////////////////////////////////////////////////////////////////////////
//     
void
CFG::LMV_clone_loop_body (LMV_CFG_ADAPTOR* adaptor) {

  // Clone the body by traversing the prev/next list from header
  // all the way down to the merge block. This traversal style enables us:
  //  - to check whether header is the first block in the prev/next list.
  //  - whether there is a "hole" in the loop body in the prev/next list
  //  - ease the work of cloning the prev/next relationship.
  //
  BB_NODE* src_blk = adaptor->Src_loop()->Header(); 
  BB_NODE* clone_blk;
  BB_NODE* prev_clone_blk = NULL;
  BB_NODE_SET* src_loop_body = adaptor->Src_loop()->True_body_set();
  INT clone_cnt = 0;
  do {
    clone_blk = LMV_clone_block (src_blk, adaptor);
    src_blk = src_blk->Next();
    ++ clone_cnt;
    if (prev_clone_blk) {
      // Chain them up by prev/next link
      prev_clone_blk->Set_next (clone_blk);
      clone_blk->Set_prev (prev_clone_blk);
    }
    prev_clone_blk = clone_blk;
  } while (src_blk && src_loop_body->MemberP (src_blk));

  // If this assertion should be trigged, check out 
  // CFG::LMV_eligible_for_multiversioning() to see why this case 
  // was not prevented earlier.
  //
  Is_True (clone_cnt == src_loop_body->Size(), 
           ("Some blocks in the source loop body are not cloned which suggest "
	    "there is a hole in the loop body blocks in the prev/next link"));

  adaptor->Set_pred_next_lst_header (adaptor->Cloned_loop_header());
  adaptor->Set_pred_next_lst_tail (clone_blk);

  if (!adaptor->Trace()) {
    return;
  } else {
    fprintf (TFile, 
       "The map of loop body blocks are (in format <src,dup>):\n");  
    BB_NODE* t;
    BB_NODE_SET_ITER iter;
    FOR_ALL_ELEM (t, iter, Init(src_loop_body)) {
      fprintf (TFile, "<%d,%d>,", t->Id(), adaptor->Get_cloned_bb(t)->Id());
    }

    fprintf (TFile, "\nPrev/next of src loop: ");
    for (t = adaptor->Src_loop()->Header(); 
         t && src_loop_body->MemberP(t); t = t->Next()) {
      fprintf (TFile, "%d,", t->Id());
    }
    
    fprintf (TFile, "\nPrev/next of cloned loop: ");
    for (t = adaptor->Pred_next_lst_header (); t; t = t->Next()) {
      fprintf (TFile, "%d,", t->Id());
    }
    fprintf (TFile, "\n");
  }
}

/////////////////////////////////////////////////////////////////////////////
//
// Apparently, the labels cannot be copied, they should be created anew in 
// the cloned blocks. We call those labels as "internal lables" to 
// differenticate them from other labels outside the loop. When a branch is 
// cloned, the label of it is blindly copied too. This is not correct if the
// the label is "internal label" of source loop. This function is going to 
// correct those labels by changing them into their corresponding "internal 
// labels" of cloned loop.
//
//////////////////////////////////////////////////////////////////////////////
//
void
CFG::LMV_update_internal_labels (LMV_CFG_ADAPTOR* adaptor) {
  
  BB_LOOP* src_loop = adaptor->Src_loop();
  BB_NODE_SET_ITER bbs_iter;   
  BB_NODE* bb;
  FOR_ALL_ELEM(bb, bbs_iter, Init(src_loop->True_body_set())) {
    STMTREP* br = bb->Branch_stmtrep();
    if (!br || !OPERATOR_has_label (br->Opr())) {
      // HINT: <br> could be a call.
      continue;
    }

    INT lab = br->Label_number();
    if (lab == 0) continue;

    if (INT new_lab = adaptor->Get_cloned_label (lab)) {
      STMTREP* new_br = adaptor->Get_cloned_bb (bb)->Branch_stmtrep(); 
      new_br->Set_label_number (new_lab); 
    }
  }

#ifdef Is_True_On
  // Verify: For every labeld block of src loop body, its cloned 
  //   counterpart should be labeled too, and the labels are mapped
  //   properly.
  FOR_ALL_ELEM(bb, bbs_iter, Init(src_loop->True_body_set())) {
    INT lab = bb->Labnam();
    if (lab) {
      INT l = adaptor->Get_cloned_bb (bb)->Labnam ();
      Is_True (l && l == adaptor->Get_cloned_label (lab), 
         ("Lab %d is either not cloned or not mapped properly", lab));
    }
  }
#endif // Is_True_On

  // Update the lab associated with the branches of precessors of new-merge blk.
  //
  BB_LIST_ITER bb_iter;   
  FOR_ALL_ELEM (bb, bb_iter, Init(src_loop->Merge()->Pred())) {
    // If this assertion should be triggered, check out
    // CFG::LMV_eligible_for_multiversioning to see why it fail
    // to screen out this loop.
    //
    Is_True (src_loop->True_body_set()->MemberP (bb),
       ("All predecessors are supposed to be the block of loop body"));

    BB_NODE* clone_pred = adaptor->Get_cloned_bb (bb);
    STMTREP* br = clone_pred->Branch_stmtrep();
    if (br && OPERATOR_has_label (br->Opr()) && 
        br->Label_number() == src_loop->Merge()->Labnam()) {
      INT t = adaptor->Cloned_loop_merge()->Labnam();
      Is_True (t != 0, 
          ("Label of new merge block BB:%d of source loop BB:%d is not "
	   "set properly", 
	   adaptor->Cloned_loop_merge()->Id(),
	   src_loop->Header()->Id()));
      br->Set_label_number (t);
    }
  }
}

BB_LOOP*
CFG::LMV_clone_BB_LOOP (LMV_CFG_ADAPTOR* adaptor, BB_LOOP* model) {

    BB_LOOP* clone = CXX_NEW (BB_LOOP (NULL,
                                 adaptor->Get_cloned_bb (model->Start ()),
  					             adaptor->Get_cloned_bb (model->End ()),
  					             adaptor->Get_cloned_bb (model->Body ()),
  					             adaptor->Get_cloned_bb (model->Step ()),
  					             adaptor->Get_cloned_bb (model->Merge ())),
  				               Mem_pool());
  
    clone->Set_flag (model->Flags());
  
    // set flag "well-formed"
    //
    clone->Reset_well_formed ();
    if (model->Well_formed ()) { clone->Set_well_formed (); }

    // set flag "has-entry-guard"
    //
    clone->Reset_has_entry_guard();
    if (model->Has_entry_guard()) { clone->Set_has_entry_guard(); }

    // set flag "Valid_doloop"
    clone->Reset_valid_doloop ();
    if (model->Valid_doloop ()) { clone->Set_valid_doloop (); }
      
    clone->Set_test_at_entry (model->Test_at_entry ());
    clone->Set_test_at_exit (model->Test_at_exit ());
    clone->Set_exit_early (model->Exit_early ());

    if (model->Promoted_do()) { clone->Set_promoted_do(); }

    clone->Set_header_pred_count (model->Header_pred_count ());

    if (WN* idx = model->Index ()) {
        clone->Set_index (WN_COPY_Tree_With_Map (idx));
    }

    clone->Set_orig_wn (model->Orig_wn());

    BB_NODE* prehdr = (prehdr = model->Preheader ()) ? 
                      adaptor->Get_cloned_bb (prehdr) : NULL; 
    clone->Set_preheader (prehdr);

    BB_NODE* header = (header = model->Header()) ? 
                      adaptor->Get_cloned_bb (header) : NULL; 
    clone->Set_header (header);

    BB_NODE* tail = (tail = model->Tail ()) ? 
                     adaptor->Get_cloned_bb (tail) : NULL;
    clone->Set_tail (tail);
  
    return clone;
}

/////////////////////////////////////////////////////////////////////////////
//
//  Create BB_LOOPs for the cloned loop nest and return the BB_LOOP corresponding
// the outmost src loop.
// 
/////////////////////////////////////////////////////////////////////////////
//
BB_LOOP*
CFG::LMV_clone_BB_LOOPs (LMV_CFG_ADAPTOR* adaptor) {

  // step 1: clone BB_LOOPs for the cloned loop nest. 
  //
  std::list<BB_NODE*> src_blks;
  adaptor->Get_all_src_bb (src_blks);

  typedef std::map<BB_LOOP*, BB_LOOP*> BB_LOOP_MAP;
  BB_LOOP_MAP bb_loop_map;

  for (std::list<BB_NODE*>::iterator iter = src_blks.begin (), 
       iter_end = src_blks.end ();
       iter != iter_end; iter++) {

    BB_NODE* src_blk = *iter;
    if (BB_LOOP* loop = src_blk->Loop ()) {
      BB_LOOP* cloned_loop = bb_loop_map[loop];
      if (!cloned_loop) {
        bb_loop_map[loop] = cloned_loop = LMV_clone_BB_LOOP (adaptor, loop);
      }
      adaptor->Get_cloned_bb (src_blk)->Set_loop (cloned_loop); 
    }
  }

  // step 2: Fix cloned BB_LOOPs; clone BB_LOOP hierarchy 
  //
  for (BB_LOOP_MAP::iterator iter = bb_loop_map.begin (),
       iter_end = bb_loop_map.end (); 
       iter != iter_end; iter++) {

    BB_LOOP* src = (*iter).first;
    BB_LOOP* dest = (*iter).second;

    // HINT: loop-merge block of outermost look is likely not cloned, 
    //    they are shared with src outmost loop.
    //
    if (!dest->Merge () && src->Merge ()) {
      dest->Set_merge (src->Merge ());
    }

    if (BB_LOOP* parent = src->Parent ()) {
      BB_LOOP_MAP::iterator iter = bb_loop_map.find (parent); 
      dest->Set_parent (iter != bb_loop_map.end () ? (*iter).second : parent);
    }

    if (BB_LOOP* child = src->Child ()) {
      BB_LOOP_MAP::iterator iter = bb_loop_map.find (child); 
      dest->Set_child (iter != bb_loop_map.end () ? (*iter).second : child);
    }

    if (BB_LOOP* sibling = src->Next ()) {
      BB_LOOP_MAP::iterator iter = bb_loop_map.find (sibling); 
      // if <iter> is null, it means the <src> is the outmost loop we are 
      // cloning, set the sibling to NULL. The laber CFG::Analyze_loops() will
      // reestablish the prev/next link.
      //
      dest->Set_Next (iter != bb_loop_map.end () ? (*iter).second : NULL);
    }
  }

  return bb_loop_map[adaptor->Src_loop()];
}

void
CFG::LMV_clone_BB_IFINFO (LMV_CFG_ADAPTOR* adaptor) {
  
  BB_NODE* bb;
  BB_NODE_SET_ITER iter;
  FOR_ALL_ELEM (bb, iter, Init(adaptor->Src_loop()->True_body_set())) {

    if (bb->Kind() != BB_LOGIF) { continue ; }

    BB_IFINFO* ifinfo = bb->Ifinfo();
    if (!ifinfo) { continue; }

    BB_NODE *cond_blk, *then_blk, *else_blk, *merge_blk;
    cond_blk = adaptor->Get_cloned_bb (ifinfo->Cond());
    then_blk = adaptor->Get_cloned_bb (ifinfo->Then());
    else_blk = adaptor->Get_cloned_bb (ifinfo->Else());
    merge_blk = adaptor->Get_cloned_bb (ifinfo->Merge());

    Is_True (cond_blk && then_blk && else_blk && merge_blk, 
            ("Fail to construct BB_IFINFO"));

    BB_IFINFO* cloned_ifinfo = CXX_NEW (BB_IFINFO(ifinfo->Thenloc(),
                                                  ifinfo->Elseloc(),
                                                  cond_blk,
                                                  then_blk, 
                                                  else_blk,
                                                  merge_blk), 
                                          Mem_pool());
    adaptor->Get_cloned_bb (bb)->Set_ifinfo (cloned_ifinfo);
  }
}

/////////////////////////////////////////////////////////////////////////
//
// This function is to generate the preconditioning statement/block 
// and connect it with the related blocks. The final control flow graph
// is depicted bellow. 
//
//                         original-preheader's preds
//                                |  |
//                                V  V
//                         preconditional block (*)
//                         /         \
//                        /           V
//             preheader-of      original-preheader
//             duplicated-loop         | 
//                   |                 |
//                   V                 V
//             dup-loop-body     src-loop-body
//                   |                 |
//                   V                 V
//              merge-blk          new-merge-blk (*)
//                        \          /
//                         \        /
//                          V      V
//                     org-merge-block-of-src-loop
//                          
//  Three new blocks are created in this function, see the asterisk marked 
// block in the above graph. There are 
//     - preconditioning block, and 
//     - empty block serving as merge block for source loop. 
// 
/////////////////////////////////////////////////////////////////////////
//
void
CFG::LMV_gen_precondioning_stuff (LMV_CFG_ADAPTOR* adaptor) {
  
  BB_LOOP* src_loop = adaptor->Src_loop();

  // step 1: Create a empty block serving as new merge block
  // 
  BB_NODE* orig_merge = src_loop->Merge();
  BB_NODE* new_merge = Create_and_allocate_bb (BB_GOTO);

  orig_merge->Insert_Before (new_merge);

  BB_LIST_ITER pred_iter;
  BB_NODE* t;
  FOR_ALL_ELEM (t, pred_iter, Init(orig_merge->Pred())) {
    t->Replace_succ (orig_merge, new_merge);
    if (OPT_FEEDBACK* fb = Feedback ()) {
      fb->Add_node (new_merge->Id ());
      fb->Move_edge_dest (t->Id(), orig_merge->Id(), new_merge->Id());
    }
  }

  new_merge->Set_pred (orig_merge->Pred());
  orig_merge->Set_pred (NULL);
  Connect_predsucc (new_merge, orig_merge);
  if (OPT_FEEDBACK* fb = Feedback ()) {
    fb->Add_edge (new_merge->Id(), orig_merge->Id(), 
                  FB_EDGE_OUTGOING,
                  fb->Get_node_freq_in (new_merge->Id()));
  }

  // update the labels of branches of merge block's predecessors.
  //
  INT lab = orig_merge->Labnam();
  if (lab) {
    new_merge->Add_label (this);
    INT new_lab = new_merge->Labnam();
    FOR_ALL_ELEM (t, pred_iter, Init(new_merge->Pred())) {
      STMTREP* br = t->Branch_stmtrep();
      if (br && OPERATOR_has_label (br->Opr()) && 
          br->Label_number () == lab) {
        br->Set_label_number (new_lab);
      }
    }
  }

  // original merge block of the src loop become the merge block 
  // of the preconditioning statement
  orig_merge->Set_ifmerge (); 

  // step 2: create and insert preconditioning 
  //
  BB_NODE* orig_phdr = src_loop->Preheader();
  BB_NODE* precond = LMV_create_alike_block(BB_LOGIF,orig_phdr);
  precond->Set_flag(0);
  adaptor->Set_precond_blk (precond);

  // splice into pred/next list and permute the precond right before 
  // original preheader.
  orig_phdr->Insert_Before (precond);

  // interpose <precond> between <orig_phdr> and its preds  
  BB_NODE* pred;
  FOR_ALL_ELEM (pred, pred_iter, Init(orig_phdr->Pred())) {
    pred->Replace_succ (orig_phdr, precond); 
    if (OPT_FEEDBACK* fb = Feedback ()) {
      fb->Add_node (precond->Id ());  
      fb->Move_edge_dest (pred->Id(), orig_phdr->Id(), precond->Id());
    }

    // If the preheader is the then/else of a lowered IF/THEN/ELSE
    // construct then we must update the BB_IFINFO on the predecessor
    if (pred->Kind()==BB_LOGIF) {
      if (pred->If_then() == orig_phdr)
        pred->Ifinfo()->Set_then(precond);
      else if (pred->If_else() == orig_phdr)
        pred->Ifinfo()->Set_else(precond);
    }
  }
  precond->Set_pred (orig_phdr->Pred ());
  orig_phdr->Set_pred (NULL);
  Connect_predsucc (precond, orig_phdr);

  // Likewise if the preheader is the merge point of a lowered IF/THEN/ELSE
  // use idom to find if parent is not safe.
  // Case can be multientry, entry directly jump to header BB. 
  // Iterate all BBs and if bb's if_merge is header, change to precond.
  for (IDTYPE id = _first_bb_id+1; id <= _last_bb_id; id++) {
    BB_NODE *bb = _bb_vec[id];
    if (bb && bb->Kind() == BB_LOGIF && bb->If_merge() == orig_phdr) {
      bb->Ifinfo()->Set_merge(precond);
    }
  }

  // If the original preheader is the body of the parent loop we need
  // to make the precond block the new body
  BB_LOOP *parent_loop = src_loop->Parent();
  if (parent_loop && parent_loop->Body() == orig_phdr)
    parent_loop->Set_body(precond);

  // Append the preconditioning branch 
  
  // Why use FALESBR: 
  //   - the predicate evaluated to true when alias does not present
  //   - the original loop is set to be the "faster" version
  //
  STMTREP* br = CXX_NEW(STMTREP(OPC_FALSEBR), Mem_pool());
  br->Set_rhs (adaptor->Predicate());
  t = adaptor->Cloned_loop_preheader();
  if (t->Labnam() == 0) { t->Add_label (this); }
  br->Set_label_number (t->Labnam());
  precond->Append_stmtrep (br);

  // When CFG lower the if-statment, it try to to permute the then-clause
  // before else-clause in the prev/next list, which entails negating 
  // the branch. In order to compliant with this rule, we treat the source 
  // loop and cloned as then-clause and else-clause respectively.  And 
  // construe the preconditioning branch as the negation of the "original"  
  // branch.
  //
  BB_IFINFO* ifinfo = 
      CXX_NEW (BB_IFINFO(0, 0, 
                         precond,      // conditional block
		         orig_phdr,     // then clause
                         adaptor->Cloned_loop_preheader(), // else clause 
		         orig_merge),  // merge block of if-stmt
               Mem_pool());
  precond->Set_ifinfo (ifinfo);                                      

  //connect the precond and the preheader of cloned loop
  Connect_predsucc (precond, adaptor->Cloned_loop_preheader());

  // Update the BB_LOOP of src loop
  src_loop->Set_merge (new_merge);
  
  // step 3: connect the merge block of the cloned loop and the 
  //  original merge block of src loop.
  Connect_predsucc (adaptor->Cloned_loop_merge(), orig_merge);

  // step 4: Splice the blocks of cloned loop into the prev/next list in the 
  //   order: condition, then-clause, else clause, merge. 
  //  
  // NOTE: This order should not be changed since some phase (say, emit) assume
  //  the blocks of the if-statement are in this order.
  //
  // What we need to do is to insert the cloned loop (together with preheader and 
  // merge block) between:
  //   - new merge block of source loop, 
  //   - original merge block of source loop, this block is now serving as 
  //     merge block of the if-statement.
  //
  new_merge->Set_next (adaptor->Cloned_loop_preheader ());
  adaptor->Cloned_loop_preheader()->Set_prev (new_merge);
  orig_merge->Set_prev (adaptor->Cloned_loop_merge ());
  adaptor->Cloned_loop_merge ()->Set_next (orig_merge);
   
  // step 5: update merge block of cloned BB_LOOP
  // 
  if (adaptor->Cloned_loop ()) {
    adaptor->Cloned_loop ()->Set_merge (adaptor->Cloned_loop_merge ());
  }

  if (adaptor->Trace()) {
    fprintf (TFile, "Preconditioning block is BB:%d\n", precond->Id());
    fprintf (TFile, "New merge for source loop is BB:%d\n", new_merge->Id());
  }
}

void
CFG::LMV_clone_frequency (LMV_CFG_ADAPTOR* adaptor) {

  OPT_FEEDBACK* fb = Feedback ();
  if (fb == NULL) return;
   
  BB_LOOP* src_loop = adaptor->Src_loop ();
  std::list<BB_NODE*> all_src_blk;
  std::map<BB_NODE*, BB_NODE*> clone_map;

  // step 1: collect all src blocks
  //
  {
    BB_NODE_SET* body = src_loop->True_body_set(); 
    BB_NODE* blk;
    BB_NODE_SET_ITER iter;
    FOR_ALL_ELEM (blk, iter, Init (body)) {
      all_src_blk.push_back (blk);
      clone_map[blk] = adaptor->Get_cloned_bb (blk);
    }

    blk = src_loop->Preheader ();
    all_src_blk.push_back (blk);
    clone_map[blk] = adaptor->Cloned_loop_preheader ();

    blk = src_loop->Merge ();
    all_src_blk.push_back (blk);
    clone_map[blk] = adaptor->Cloned_loop_merge ();
  }

  // step 2: allocate a feedback block for each newly created 
  //
  for (std::map<BB_NODE*, BB_NODE*>::iterator iter = clone_map.begin (), 
       iter_e = clone_map.end (); iter != iter_e; iter++) {
    BB_NODE* blk = (*iter).second;  
    fb->Add_node (blk->Id());
  }

  // step 3: clone edge frequency.
  //
  for (std::list<BB_NODE*>::iterator iter = all_src_blk.begin (), 
       iter_e = all_src_blk.end (); iter != iter_e; iter++) {

    BB_NODE* blk = *iter;    
    BB_NODE* cloned = clone_map[blk];
    BB_NODE* succ;
    BB_NODE* cloned_succ;
    BB_LIST_ITER succ_iter;
	FOR_ALL_ELEM (succ, succ_iter, Init (blk->Succ())) {
      cloned_succ = clone_map[succ];
      if (!cloned_succ) {
        Is_True (blk == src_loop->Merge(), ("internal inconsistency"));
        cloned_succ = succ;
      }

      if (fb->Edge_has_freq (blk->Id(), succ->Id())) {
        fb->Clone_edge (blk->Id(), succ->Id(), cloned->Id(), 
                        cloned_succ->Id(), 0.5f);
      }
    }
  }

  // step 4: set precondition outgoing edge freq
  //
  {
    FB_FREQ freq = fb->Get_edge_freq (src_loop->Preheader()->Id(), 
                                      src_loop->Header()->Id ());
    BB_NODE* precond = adaptor->Precond_blk();
    BB_NODE* succ = precond->Nth_succ(0);
    IDTYPE src_id, dst_id;
    src_id = precond->Id();
    dst_id = succ->Id();

    fb->Delete_edge (src_id, dst_id);
    fb->Add_edge (src_id, dst_id, 
                  precond->Next () != succ ? 
                  FB_EDGE_BRANCH_TAKEN : FB_EDGE_BRANCH_NOT_TAKEN, freq);

    succ = precond->Nth_succ(1);
    dst_id = succ->Id();
    fb->Delete_edge (src_id, dst_id);
    fb->Add_edge (src_id, dst_id, 
                  precond->Next () != succ ? 
                  FB_EDGE_BRANCH_TAKEN : FB_EDGE_BRANCH_NOT_TAKEN, freq);
  }
}

// I don't remember all reasons for not reusing CFG::Clone_bbs(). I recall 
// that CFG::Clone_bbs() is bit awkward as it requires that the source BBs must
// be linked with next/prev field. This requirement may not be satisfied 
// in our case.
//
void
CFG::LMV_clone_loop (LMV_CFG_ADAPTOR* adaptor) {
 
  if (adaptor->Trace()) {
    fprintf (TFile, 
       "Duplicating loop (BB:%d)\nthe map between original and new blocks" 
       " are (in format <orig,new>):\n", adaptor->Src_loop()->Header()->Id());
  }

  BB_LOOP* src_loop = adaptor->Src_loop ();

  // step 1. clone the loop body and chain them together via prev/next link.
  //
  LMV_clone_loop_body (adaptor);

  // step 2. Clone the preheader
  BB_NODE *new_phdr = LMV_clone_block(src_loop->Preheader(),adaptor);
  // The loop preheader is lowered to a GOTO. If the loop is a DO loop
  // all of the DO* blocks will be lowered.  This is done because we cannot
  // properly manufacture a new BB_LOOP for a DO loop.
  new_phdr->Set_kind(BB_GOTO);
  adaptor->Set_cloned_loop_preheader (new_phdr);

  // prepend to the prev/next list
  BB_NODE* t = adaptor->Pred_next_lst_header ();
  t->Insert_Before (new_phdr);
  adaptor->Set_pred_next_lst_header (new_phdr);

  // step 3: Create an empty block serving as merge block for the cloned loop. 
  //
  BB_NODE* new_merge = LMV_create_alike_block (BB_GOTO, src_loop->Merge());
  new_merge->Set_flag (0);
  adaptor->Set_cloned_loop_merge (new_merge);
  if (src_loop->Merge()->Labnam()) {
    new_merge->Add_label(this);
    adaptor->Map_cloned_label(src_loop->Merge()->Labnam(),new_merge->Labnam());
  }

  // append to the prev/next list
  t = adaptor->Pred_next_lst_tail ();
  t->Insert_After (new_merge);
  adaptor->Set_pred_next_lst_tail (new_merge);

  if (adaptor->Trace()) {
    fprintf (TFile, 
             "Preheader and merge block of cloned loop is BB:%d and BB:%d\n",
             new_phdr->Id(), new_merge->Id());
  }

  // step 4: Clone the pred/succ relationship.
  //
  LMV_clone_pred_succ_relationship (adaptor);

  // step 5: Update the internal labels
  //
  LMV_update_internal_labels (adaptor);

  // step 6: Generate a BB_LOOP structure for the cloned loop.
  //
  BB_LOOP* dup_BB_LOOP = LMV_clone_BB_LOOPs (adaptor);
  adaptor->Set_cloned_loop (dup_BB_LOOP);

  // step 7: update BB_IFINFO associated with block of kind BB_LOGIF 
  LMV_clone_BB_IFINFO (adaptor);

  // step 8: Generate preconditiong block and splic then/else clause in the CFG.
  //
  LMV_gen_precondioning_stuff (adaptor);

  // step 9: copy freq feedback
  if (Feedback ()) {
    LMV_clone_frequency (adaptor);
  }
}

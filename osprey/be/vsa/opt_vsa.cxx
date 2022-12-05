//-*-c++-*-

/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

// =============================================================================
// =============================================================================
//
// Module: opt_vsa.cxx
//
// =============================================================================
//
// Description:
//
// Vulnerability Static Analysis
//
// =============================================================================
// =============================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <stdarg.h>
#include "defs.h"
#include "vsa_defs.h"
#include "errors.h"
#include "erglob.h"
#include "glob.h"        // for Cur_PU_Name
#include "mempool.h"
#include "tracing.h"        /* for TFile */
#include "stab.h"
#include "irbdata.h"
#include "cxx_memory.h"
#include "be_symtab.h"
#include "const.h"
#include "erbe.h"

#include "opt_defs.h"
#include "opt_config.h"
#include "opt_cfg.h"
#include "opt_dbg.h"
#include "opt_dna.h"
#include "opt_estr.h"
#include "opt_etable.h"
#include "opt_main.h"
#include "bb_node_set.h"
#include "opt_util.h"
#include "opt_vsa_util.h"
#include "opt_mu_chi.h"
#include "opt_ssa.h"
#include "opt_sym.h"
#include "opt_alias_rule.h"
#include "opt_cvtl_rule.h"
#include "opt_lftr2.h"
#include "opt_addr_util.h"
#include "config_vsa.h"
#include "report.h"    // for vsa report
#include "opt_vsa.h"
#include "opt_vsa_eh.h"
#include "opt_vra.h"
#include "class_hierarchy.h"
#include "symtab_access_global.h"
#include <stack>
#include "opt_vsa_rbc.h"
#include "opt_vsa_jni.h"
#include "opt_vsa_report.h"
#include "opt_vsa_checker.h"
#include "opt_vsa_graph.h"
#include "pro_core.h"
#include "builtin_rule_defs.h"
#include "intrn_info.h"

// =============================================================================
//
// SCC:Dfs Tarjan's SCC algorithm which creates the supporting data structure
//         for multiple phi results with circular dependency
//
// =============================================================================
void
SCC::Dfs(CODEREP *x, VSA *vsa, hash_set<IDTYPE> &visited_bb)
{
  AUX_STAB_ENTRY *sym;
  ST             *st;

  Is_True(x != NULL, ("SCC:Dfs: null var"));
  // skip these two VSYMs
  if (x->Aux_id() == vsa->Opt_stab()->Default_vsym() ||
      x->Aux_id() == vsa->Opt_stab()->Return_vsym()) 
    return ;
  
  sym = vsa->Opt_stab()->Aux_stab_entry(x->Aux_id());
  st = sym->St();

  if ( x->Is_flag_set(CF_IS_ZERO_VERSION) ) 
    return ;

  if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI)) )
    return ;

  if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI)) ) {
    PHI_NODE *phi = x->Defphi();
    if (visited_bb.find(phi->Bb()->Id()) == visited_bb.end())
      visited_bb.insert(phi->Bb()->Id());
    else {
      INT id = Lookup(x);
      if ((id >= 0) && Onstack(id)) {
        for (INT i = Id()-1; i > id; --i) {
          Set_low(i, (Low(id) < Low(i))? Low(id) : Low(i));
          Inc_scc_ecnt();
        }
      }
      return;
    }
    
    Push(x);
    // check all its operands since it has not been checked
    PHI_OPND_ITER phi_opnd_iter(phi);
    CODEREP *opnd;
    FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
      Dfs(opnd, vsa, visited_bb);
    }
    return ;
  }
}

// =============================================================================
//
// SCC::Defer determines if the attribute from the path leads to by x should be
//      deferred to the SCC::Update.
//
// =============================================================================
BOOL
SCC::Defer(CODEREP *x) const
{
  INT i = Lookup(x);
  if (i < 0)     
    return FALSE;// not found in scc groups
  if ( Low(i) == i && i+1 < Id() && Low(i+1) == i)
    return TRUE; // first element of scc and the scc has multiple components
  if ( Low(i) != i && Low(Low(i)) == Low(i) )
    return TRUE; // not 1st element, then it's low_link must point to itself
  return FALSE;
}

// =============================================================================
//
// SCC::Update the vardef attributes of a SCC by unifying them
//
// =============================================================================
void
SCC::Update(void) const
{
  if (Id() == 0) return;  // there is nothing to be done
  Is_Trace(Tracing(),
           (TFile, "%sPerform unification on vardef for\n", SBar));
  Is_Trace_cmd(Tracing(), Print(TFile));
  for (INT i=0; i < Id(); ++i) {

    if (Low(i) == i) {  // low_link candidate
      // unify the attribute by merging into the low_link
      CODEREP *low_link = Ids(i);
      CODEREP *peer;

      for (INT j = i+1; (j < Id()) && (Low(j) == i); ++j) {
        peer = Ids(j);
        if (low_link->Value_def() && peer->Value_def()) {
          low_link->Set_value_def();
          if (low_link->Value_invalid_addr() && peer->Value_invalid_addr())
            low_link->Set_value_invalid_addr();
          else
            low_link->Set_value_maydangling();
        } else if (low_link->Value_not_def() && peer->Value_not_def())
          low_link->Set_value_not_def();
        else
          low_link->Set_value_maydef();
        if (peer->Value_malloc())
          low_link->Set_value_malloc();
      } // unify loop

      // populate same attribute set from low_link to the rest of scc peers
      for (INT j = i+1; (j < Id()) && (Low(j) == i); ++j) {
        peer = Ids(j);
        peer->Set_value_flags(low_link->Value_flags());
      } // populate loop

    } // low_link candidate
  }
  Is_Trace(Tracing(),
           (TFile, "%sAfter unification on vardef for\n", SBar));
  Is_Trace_cmd(Tracing(), Print(TFile));
}

// =============================================================================
//
// SCC::Print the content of SCC, Strongly Connected Phinode for a CODEREP
//
// =============================================================================
void
SCC::Print(FILE *fp) const
{
  fprintf(fp, "%sSCC with %d entries:%s", SBar, Id(), (Id()==0)?"\n":" ");
  if (Id() != 0) Ids(0)->Print(0, fp);
  
  for (INT i=0; i < Id(); ++i) {
    fprintf(fp, "#%d:\t Ids: cr%d\tLow: %d\tVardef:%s %s %s %s %s\n",
            i, Ids(i)->Coderep_id(), Low(i),
            (Ids(i)->Value_def())? "def:Y":" ",
            (Ids(i)->Value_maydef())? "maydef:Y":" ",
            (Ids(i)->Value_not_def())? "not_def:Y":" ",
            (Ids(i)->Value_maydangling())? "maydangling:Y":" ",
            (Ids(i)->Value_invalid_addr())? "invalid_addr:Y":" ");
  }
}

// =============================================================================
//
//  Bottom-up traversal of CODEREP nodes in a statement to create occurrence list
//  for VSA
//
// =============================================================================
void
ETABLE::VSA_bottom_up_stmt(STMTREP *stmt)
{
  const OPERATOR stmt_opr = OPCODE_operator(stmt->Op()); 

  Is_Trace(Tracing(),
          (TFile, "----- stmt: %s -----\n", OPCODE_name(stmt->Op())));
  Is_Trace_cmd(Tracing(),stmt->Print(TFile));
  
  stmt->Set_stmt_id(Cfg()->Get_stmt_id());

  // for each statement see if they have a rhs and lhs and traverse
  // any expressions there
  CODEREP *rhs = stmt->Rhs();
  CODEREP *lhs = stmt->Lhs();

  if (OPCODE_is_fake(stmt->Op())) {
    for (INT32 i = 0; i < rhs->Kid_count(); i++) {
      New_temp_id();
      VSA_bottom_up_cr(stmt, i, rhs->Opnd(i), FALSE, 0, rhs, i);
    }
  } else if (rhs != NULL) {
    New_temp_id();
    VSA_bottom_up_cr(stmt, 0, rhs, FALSE, 0, lhs, 0);
  }
  if (lhs != NULL) {
    Is_Trace(Tracing(),(TFile,"Lhs\n"));
    New_temp_id();
    VSA_bottom_up_cr(stmt, 1, lhs, OPCODE_is_store(stmt->Op()), 0, NULL, 0);
  }
}

// =============================================================================
//
//  Bottom-up traversal of CODEREP nodes, assumes CODEREP is not NULL
//
// =============================================================================
void    
ETABLE::VSA_bottom_up_cr(STMTREP *stmt, INT stmt_kid_num, CODEREP *cr,
                         BOOL is_store, UINT depth, CODEREP *parent, INT whichkid)
{
  Is_True(cr != NULL,("ETABLE::VSA_bottom_up_cr, null CODEREP"));

  Is_Trace(Tracing(),(TFile, "----- cr -----\n"));
  Is_Trace_cmd(Tracing(),cr->Print(2,TFile));

  switch (cr->Kind()) {

    //
    // three constant
  case CK_CONST:
#if 0
    if ( VSA_do_valuerange())
      Append_real_occurrence(cr, stmt, stmt_kid_num, depth, FALSE);
#endif
    break;
  case CK_RCONST: 
#if 0
    if ( VSA_do_valuerange() ) // 
      Append_real_occurrence(cr, stmt, stmt_kid_num, depth, FALSE);
#endif
    break;
  case CK_LDA:    
#if 0
    if ( VSA_do_iloadlda() && ! cr->Is_flag_set(CF_LDA_LABEL) )
      Append_real_occurrence(cr, stmt, stmt_kid_num, depth, FALSE);
#endif
    break;

  case CK_VAR:            // variable terminal rvi candidates

    if ( !cr->Is_var_volatile() && 
         // screen out MLDID
         !Opt_stab()->Aux_stab_entry(cr->Aux_id())->No_register())
      // && ST_class( Opt_stab()->St(cr->Aux_id()) ) != CLASS_PREG )  VSA needs to track all LDID
    {
      Insert_real_occurrence(cr, stmt, stmt_kid_num, depth, is_store, TRUE);
    }
    break;

  case CK_IVAR:        // non-terminal

#ifdef Is_True_On
    if (cr->Ilod_base() != NULL && cr->Istr_base() != NULL && 
        cr->Ilod_base() != cr->Istr_base())
      Warn_todo("CODEREP ilod_base != istr_base.");
    if (cr->Opr() == OPR_ILOADX)
      Warn_todo("ETABLE::Bottom_up_cr: Indexed load.");
#endif

    // Skip the DUMMY OPR_PARM
    if (cr->Opr() == OPR_PARM && (cr->Offset() & WN_PARM_DUMMY)) 
      break;

    VSA_bottom_up_cr(stmt, stmt_kid_num, 
                     is_store ? cr->Istr_base() : cr->Ilod_base(),
                     FALSE, ( depth + 1 ), cr, 0);
    if ( cr->Opr() == OPR_MLOAD ) {
      VSA_bottom_up_cr(stmt, stmt_kid_num, 
                       cr->Mload_size() ? 
                       cr->Mload_size() : cr->Mstore_size(),
                       FALSE, ( depth + 1 ), cr, 1);
    }
    else if (cr->Opr() == OPR_ILOADX) {
      VSA_bottom_up_cr(stmt, stmt_kid_num, cr->Index(),
                       FALSE, ( depth + 1 ), cr, 1);
    }
    break;

  case CK_OP:                // non-terminal
    {
      // if (cr->Opr() == OPR_ARRAY) todo: array bound checking

      //  A CR has its temp_id field initialized to -1, and might be
      //  set to the temp_id of statement processed. If
      //  ETABLE::New_temp_id() guarantees not to repeatedly use an
      //  id, CRs reached here the first time will have an id
      //  different from the Cur_temp_id.
      //  
      //   cr->Temp_id() != Cur_temp_id()               -->  first visit 
      //   cr->Temp_id() == Cur_temp_id() && !Is_lcse   -->  second visit
      //   cr->Temp_id() == Cur_temp_id() && Is_lcse    -->  third+ visit
      //  
      if (cr->Temp_id() == Cur_temp_id())
        if (cr->Is_lcse() && (cr->Max_depth() >= depth ||
                              cr->Max_depth() == 255))
          break;   // return because this CR has been visited twice
        else
          cr->Set_is_lcse();  // this is the second time
      else {
        cr->Set_temp_id(Cur_temp_id());  // this is the first time
        cr->Reset_is_lcse();
        cr->Set_max_depth(depth);
      }

      // remember maximum depth 
      if (cr->Max_depth() < depth)        
        cr->Set_max_depth( ( depth <= 255 ) ? depth : 255 );
        
      for (INT32 i=0; i<cr->Kid_count(); i++)        { 
        if (cr->Opr() == OPR_INTRINSIC_OP && cr->Intrinsic() == INTRN_EXPECT &&
            i == 1)
          continue;
        VSA_bottom_up_cr(stmt, stmt_kid_num, cr->Opnd(i), FALSE, (depth+1), cr, i);
      }
      break;
    }
  default:                // illegal kind
    Is_True(0,("ETABLE::Bottom_up_cr(), unexpected kind 0x%x",cr->Kind()));
    break;
  }
}


// =============================================================================
// Collect the defphi node into the EXP_OCCURS and set its BB in phi_list
// If any of the PHI_NODE operand is defined by phi, also collect them
// recursively
// =============================================================================
void
EXP_WORKLST::VSA_collect_defphi_recursive(PHI_NODE    *phi_node,
                                          BB_NODE_SET &phi_list,
                                          BOOL         tracing,
                                          ETABLE      *etable)
{
  Is_True(phi_node, ("Collect_defphi_recursive: Null PHI_NODE parameter"));

  if (phi_node->Live()) {
    MEM_POOL *per_exp_pool = etable->Per_expr_pool();
    BB_NODE  *bb = phi_node->Bb();
    if (!phi_list.MemberP(bb->Dom_dfs_id())) {
      Is_Trace(tracing,
               (TFile, "------ Enter Phi-node (by variable): %d ------\n",
                bb->Id()));
      Is_Trace_cmd(tracing, phi_node->Print(bb->Phi_list()->In_degree(),TFile));
      phi_list.Union1D(bb->Dom_dfs_id());
      
      // append phi_node to the phi_occurs list
      Is_Trace(etable->Tracing(),
               (TFile, "------ Generate EXP_PHI: %d ------\n", bb->Id()));
      EXP_PHI *new_phi = CXX_NEW(EXP_PHI(E_num(), 
                                         bb->Phi_list()->In_degree(), 
                                         bb, per_exp_pool), per_exp_pool);
      EXP_OCCURS *new_occ = etable->Append_phi_occurrence(Exp(), new_phi, this);

      new_occ->Set_occurrence(phi_node->RESULT()); // new_occ is appended to the phi_occurs list contains phi result
      etable->Set_exp_phi_bb(bb, new_occ);

      // Enter the CR_OCC_MAP for this new_occ with phi result
      etable->Vsa()->Enter_cr_occ_map(phi_node->RESULT(), new_occ);

      // scan phi_node's operands and build the phi-predecessor list
      BB_LIST_ITER bb_iter;
      BB_NODE     *bb_phi;
      CODEREP     *cr;
      PHI_NODE    *defphi;
      INT          i = 0;
      FOR_ALL_ELEM (bb_phi, bb_iter, Init(bb->Pred())) {

        cr = phi_node->OPND(i);
        Is_Trace(etable->Tracing(),
                 (TFile,"------ Generate Phi-pred occurrence %d ------\n", 
                  bb_phi->Id()));
        EXP_OCCURS *phi_pred = etable->Append_phi_pred_occurrence(Exp(), bb_phi, this);
        phi_pred->Set_occurrence(cr);      // reconnect the use-def relation with htable
        phi_pred->Set_phi_opnd_idx(i);     // or use def_occur's opnd, once it is setup in latter phase
        new_phi->Set_pred(i, phi_pred);

        // recursively collect the defphi for operand that is defined by phi
        if (cr) {
          if (cr->Is_flag_set(CF_DEF_BY_PHI) && 
              ((defphi = cr->Defphi() ) != NULL)) {
            VSA_collect_defphi_recursive(defphi, phi_list, tracing, etable);
          }
        }
        i++;
      } // for each phi operands
    } // first visit for the phi_node
  } // if phi_node live
}

// =============================================================================
//
// VSA worklist creation Step 2
// Collect exp's phi.
// Build expression phi list and phi's predecessor list, both in DPO
//
// =============================================================================
BOOL
EXP_WORKLST::VSA_collect_phi_occurrences(ETABLE *etable)
{
  FmtAssert(etable,("EXP_WORKLST::VSA_collect_phi_occurrences: Etable is NULL"));

  Is_Trace(etable->Tracing(),
           (TFile, "====== ETABLE::Insert_phi_occurrences ======\n"));
  Is_Trace(etable->Tracing(),
           (TFile, "The real occurrence list is:\n"));
  Is_Trace_cmd(etable->Tracing(),Real_occurs().Print(TFile));

  // short circuit the process if there is only one real occurrence with
  // no phi function at all.
  if (Real_occurs().Head() == Real_occurs().Tail() &&
      !Real_occurs().Head()->Mult_real() &&
      Real_occurs().Head()->Bb()->Dom_frontier()->EmptyP()) return FALSE;

  BB_NODE_SET        &phi_list = etable->Phi_work_set();
  EXP_OCCURS         *exp_occ;
  EXP_OCCURS_ITER     exp_occ_iter;
  MEM_POOL           *local_pool = etable->Etable_local_pool();

  Is_True(Exp()->Kind() == CK_VAR, ("EXP_WORKLST::VSA_colect_phi_occurrences does not handle the Kind()"));
  OPT_POOL_Push(local_pool, -1);
  {
    phi_list.ClearD();

    FOR_ALL_NODE (exp_occ, exp_occ_iter, Init(Real_occurs().Head())) {
      PHI_NODE *phi_node;
      CODEREP  *cr = exp_occ->Occurrence();
      Is_True(cr,("EXP_WORKLST::VSA_collect_phi_occurrences: real occurrence with null CODEREP"));
      if (cr == NULL)
        continue;
      if ((cr->Kind()==CK_IVAR) && (cr->Opr()==OPR_PARM)) cr = cr->Ilod_base();
      Is_True(cr,("EXP_WORKLST::VSA_collect_phi_occurrences: real occurrence with null CODEREP as kid"));    

      if (cr->Is_flag_set(CF_DEF_BY_PHI) && 
          ((phi_node = cr->Defphi()) != NULL)) {
        VSA_collect_defphi_recursive(phi_node, phi_list,  etable->Tracing(), etable);
      }  // collect phi_node
    } // all real occurrences
  }
  OPT_POOL_Pop(local_pool, -1);

  return TRUE;
}

// =============================================================================
//
// EXP_OCCURS::Trace_add_ref - to generate trace for the mapping from cr to
// the actual expression occurrence.
//
// =============================================================================
void
EXP_OCCURS::Trace_add_ref(ETABLE *etable)
{
  CODEREP    *cr = this->Occurrence();
  EXP_OCCURS *def = etable->Vsa()->Cr_2_EXPOCC(cr);
  BOOL        t = etable->Tracing();

  if (def != NULL) {
    Is_Trace(t, (TFile, "====== Enter DEF-USE ======\n====== DEF is :\n"));
    Is_Trace_cmd(t, def->Print(TFile, FALSE));
    Is_Trace(t, (TFile, "====== USE is:\n"));
    Is_Trace_cmd(t, this->Print(TFile, FALSE));

    MEM_POOL   *pool = etable->Etable_local_pool();
    def->Add_ref(this, pool);
  }
  else { // TODO: handle CHI defined CR 
    // Is_True(cr->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI)), ("EXP_WORKLST::Compute_modref_info: Phi opnd isn't defined yet"));
    Is_Trace(t, (TFile, "====== Enter DEF-USE FAILED ======\n"));
    Is_Trace(t, (TFile, "====== USE w/o DEF is:\n"));
    Is_Trace_cmd(t, this->Print(TFile, FALSE));
    Is_Trace(t, (TFile, "====== End DEF-USE FAILURE ======\n"));
  }
}

// =============================================================================
//
//  EXP_WORKLST::COmpute_modref_info - generate the DU chain out of the U-D
//
// =============================================================================
void
EXP_WORKLST::Compute_modref_info(ETABLE *etable)
{
  { // processing real_occurs list
    EXP_OCCURS_ITER  exp_occ_iter;
    EXP_OCCURS      *exp_occ;

    // For each phi node for this expression
    FOR_ALL_NODE(exp_occ, exp_occ_iter, Init(Real_occurs().Head())) {

      if (! exp_occ->Occurs_as_lvalue()) { // skip if exp_occ itself is a def node
        exp_occ->Trace_add_ref(etable);
      }
    }
  } // processing real_occurs list

  { // processing phi_occurs list
    EXP_OCCURS_ITER  phi_occ_iter;
    EXP_OCCURS      *phi_occ;

    // For each phi node for this expression
    FOR_ALL_NODE(phi_occ, phi_occ_iter, Init(Phi_occurs().Head())) {

      EXP_PHI *phi = phi_occ->Exp_phi();

      for (INT i = 0; i < phi->Opnd_count(); ++i) {
        EXP_OCCURS *opnd = phi->Pred(i);  // opnd occurrence is setup in Pred() for VSA 
        opnd->Trace_add_ref(etable);
      }
    }
  } // processing phi_occurs list
}

// =============================================================================
//
// perform the VSA iterations
//
// =============================================================================
void
ETABLE::VSA_perform_analysis_iterations(VSA *vsa)
{
  _vsa = vsa;
  // Even they are useless, the structure should be around to
  // share the code
  _str_red = CXX_NEW(STR_RED(Cfg(), Htable(), _etable_pool, _tracing),
                     _etable_pool);
  _lftr = CXX_NEW(LFTR(this, Htable(), Cfg(), LFTR_HASH_SIZE), _etable_pool);

  {
    BB_NODE *bb = Cfg()->Entry_bb();
    if (bb != Cfg()->Fake_entry_bb()) {
      STMTREP *entry_chi = bb->Stmtlist()->Head();
      Is_True(entry_chi->Opr() == OPR_OPT_CHI, ("cannot find entry chi."));
      Set_entry_chi(entry_chi);
    }
  }

  // Phase numbers for various steps:
  INT32 Iterator_phase      = 0;
  INT32 Phi_collection_phase= 0;
  INT32 Compute_modref_phase= 0;

  // Initialize Dpo_vec before Dpo_Bb(dpo_id) in EXP_WORKLST::Insert_exp_phi
  Cfg()->Dpo_vec();

  Cfg()->Reset_stmt_id();

  SET_OPT_PHASE("VSA ITERATIONS: Build initial occurrence lists");

  // To get memory allocation tracing, use -ta25
  if (Get_Trace(TKIND_ALLOC, TP_WOPT1)) {
    MEM_Tracing_Enable();
  }

  Init_worklst(); // collect all real load occurances
  INT first_rank_e_num = _cur_e_num;
  INT bb_cnt = 0, edge_cnt = 0;

#ifdef Is_True_On
  CFG_ITER cfg_iter;
  BB_NODE *bb;
  FOR_ALL_ELEM(bb, cfg_iter, Init(Cfg())) {
    bb_cnt++;
    edge_cnt += bb->Succ()->Len();
  }
#endif

  EXP_WORKLST *cur_worklst;
  INT32 cur_worklst_idx = 0;
  EXP_WORKLST_ITER2 worklst_iter(Exp_worklst(), Urgent_worklst());

  FOR_ALL_NODE(cur_worklst, worklst_iter, Init()) {

    cur_worklst_idx++;
    if (WOPT_Enable_VSA_Limit != -1 &&
        cur_worklst_idx/*cur_worklst->E_num()*/ > WOPT_Enable_VSA_Limit) {
      DevWarn("LPRE: skip LPRE for variable with v_num > %d\n",
              WOPT_Enable_VSA_Limit);
      break;
    }
    
    OPT_POOL_Push(Per_expr_pool(), -1);

    Is_Trace(Tracing(),
             (TFile, "processing %dth expression\n", cur_worklst_idx));
    Is_Trace_cmd(Tracing(),cur_worklst->Exp()->Print(0,TFile));
    Is_Trace_cmd(Tracing(),cur_worklst->Print(TFile));


    // do stuff for each expression
    Per_worklst_cleanup(cur_worklst);

    // process each occurence
    // Step 2 may cause bypass of the remaining passes
    SET_OPT_REPEAT_PHASE(Phi_collection_phase, "VSA: Var phi collection");
    if (cur_worklst->Real_occurs().Head() != NULL &&
        cur_worklst->VSA_collect_phi_occurrences(this)) {
      // step 3: compute modref
      SET_OPT_REPEAT_PHASE(Compute_modref_phase, "VSA: Compute modref info");
      cur_worklst->Compute_modref_info(this);

      // step 4: Compute value range

      // step 5: Analyze dereference null pointer

      // step 6: Analyze out of bound memory access

    } // main activities for cur_worklst

    cur_worklst->Remove_occurs(this);
    OPT_POOL_Pop(Per_expr_pool(), -1);

    if (WOPT_Enable_Verify >= 4) {
      Is_True(_comp_unit->Verify_CODEMAP(), ("CODEMAP corrupted."));
      _comp_unit->Verify_version();
    }
  }
  
  CXX_DELETE(_str_red,_etable_pool);
  CXX_DELETE(_lftr,_etable_pool);
}

// =============================================================================
// PATH_SELECTED::Print
//   print the selected rna (callsite) and phi (path) in PATH_SELECTED
// =============================================================================
void
PATH_SELECTED::Print(FILE* fp) const
{
  fprintf(fp, "RNA selected:");
  {
    U32_SET::const_iterator it = _rna_selected.begin();
    for ( ; it != _rna_selected.end(); ++it ) {
      if (it != _rna_selected.begin())
        fprintf(fp, ",");
      fprintf(fp, " %d", *it);
    }
  }
  fprintf(fp, "\n");

  fprintf(fp, "PHI selected:");
  {
    PATH_MAP::const_iterator it = _phi_selected.begin();
    for ( ; it != _phi_selected.end(); ++it ) {
      fprintf(fp, " DNA[%d]:", it->first);
      const U64_SET& paths = it->second;
      U64_SET::const_iterator p = paths.begin();
      for ( ; p != paths.end(); ++p ) {
        if (p != paths.begin())
          fprintf(fp, ",");
        uint64_t path = *p;
        fprintf(fp, " %ld->%d", path >> 32, (uint32_t)path);
      }
      fprintf(fp, "\n");
    }
  }
  fprintf(fp, "\n");
}

// =============================================================================
//
// Print function for tracing SRCPOS_TREENODE and SRCPOS_HANDLE
//
// =============================================================================
void
SRCPOS_TREENODE::Print(INT indent, FILE *fp) const
{
  INT i;
  for (i = 0; i < indent; ++ i)
    fprintf(fp, " ");
  fprintf(fp, "> ");
  for (i = 0; i < _data.size(); ++ i) {
    SRCPOS spos = _data[i].Transpos();
    UINT file_idx = _data[i].File_idx();
    fprintf(fp, "%d:%d:%d", SRCPOS_filenum(spos), SRCPOS_linenum(spos), _data[i].Info());
    INLCXT* inlcxt = _data[i].Inlcxt();
    if(inlcxt)
    {
      fprintf(fp, "(");
      for(INLCXT* cxt = inlcxt; cxt ; cxt = cxt->Parent())
      {
        SRCPOS cxt_spos = SRCPOS_NODE::Transpos(cxt->Inlcxt_line_num(), file_idx);
        fprintf(fp, "%d:%d,", SRCPOS_filenum(cxt_spos), SRCPOS_linenum(cxt_spos));
      }
      fprintf(fp, ") ");
    }
    else
      fprintf(fp, " ");
  }
  fprintf(fp, "\n");
  for (i = 0; i < _children.size(); ++ i)
    _children[i]->Print(indent+1, fp);
}

// =============================================================================
// SRCPOS_PATH
// =============================================================================
PATH_POSSIBLE_RES
SRCPOS_PATH::Is_path_possible(PHI_NODE* phi, INT opnd) const
{
  if (!_cu->Vra())
    return PATH_REACHABLE;
  Is_True_Ret(_cu == g_comp_unit, ("context is wrong"), PATH_NOT_REACHABLE);

  INT i;
  for (i = _path.size() - 1; i >= 0; --i) {
    if (_path[i].second == NODE_IS_CU) {
      Is_True(_path[i].first == (BB_NODE*)_cu, ("comp_unit mismatch"));
      break;
    }
  }

  if (i < 0)
    i = 0;

  if (_path[i].second == NODE_IS_CU)
    ++i;

  Is_True(i < _path.size(), ("no bb found"));
  if (i >= _path.size())
    return PATH_REACHABLE;

  BOOL maybe = FALSE;
  for ( ; i < _path.size(); ++i) {
    if (_path[i].second == NODE_IS_PHI)
      continue;
    Is_True(_path[i].second == NODE_IS_BB, ("not bb"));
    if (_path[i].second != NODE_IS_BB)
      continue;
    BB_NODE* bb = _path[i].first;
    VRA_RESULT ret = _cu->Vra()->Is_path_possible(bb, phi, opnd);
    if (ret == VA_NO)
      return PATH_NOT_REACHABLE;
    else if (ret == VA_POSSIBLE)
      maybe = TRUE;
  }
  return maybe ? PATH_MAY_REACHABLE : PATH_REACHABLE;
}

PATH_POSSIBLE_RES
SRCPOS_PATH::Is_def_reachable(BB_NODE *use_bb, STMTREP *def_sr) const
{
  Is_True_Ret(use_bb, ("null use bb"), PATH_NOT_REACHABLE);
  Is_True_Ret(def_sr, ("null def_sr"), PATH_NOT_REACHABLE);
  Is_True_Ret(_cu == g_comp_unit, ("context is wrong"), PATH_NOT_REACHABLE);
  BB_NODE *def_bb = def_sr->Bb();
  if (use_bb == def_bb)
    return PATH_REACHABLE;
  if (!_cu->Vra())
    return PATH_REACHABLE;
  // get control dependency between use_bb and chi bb
  std::vector< std::pair<BB_NODE*, BB_NODE*> > cds;
  VRA *vra = _cu->Vra();
  vra->Collect_control_dependencies(use_bb, def_bb, cds);
  // find out filter used in comparison on CDs
  std::vector< std::pair<BB_NODE*, BB_NODE*> >::iterator it;
  for (it = cds.begin(); it != cds.end(); ++it) {
    CODEREP* cond = vra->Get_cond_expr(it->first, it->second);
    if (cond == NULL)
      continue;
    hash_set<CODEREP*> vars;
    if (vra->Analyze_coderep_vars(cond, vars) == false || vars.empty())
      continue;
    hash_set<CODEREP*>::iterator vit;
    for (vit = vars.begin(); vit != vars.end(); ++vit) {
      CODEREP *cond_var = *vit;
      // continue cond_var UD to find the real def
      while (cond_var && cond_var->Kind() == CK_VAR &&
             !cond_var->Is_flag_set(CF_DEF_BY_CHI) &&
             !cond_var->Is_flag_set(CF_DEF_BY_PHI)) {
        STMTREP *cond_defsr = cond_var->Defstmt();
        if (cond_defsr) {
          cond_var = cond_defsr->Rhs();
        } else {
          break;
        }
      }
      // if condition is also defined by def_sr, need cross function
      // condition evaluation, which is not supported for now, mark
      // the path as maybe
      if (cond_var &&
          cond_var->Is_flag_set(CF_DEF_BY_CHI) &&
          cond_var->Defstmt() == def_sr) {
        return PATH_MAY_REACHABLE;
      }
    }
  }
  return PATH_REACHABLE;
}

// =============================================================================
//
// SRCPOS_HANDLE constructor
// @parm dna: function where the srcpos_handle is constructed
// @parm pool: mempool where this srcpos_handle can malloc memory
//
// =============================================================================
SRCPOS_HANDLE::SRCPOS_HANDLE(DNA_NODE* dna, MEM_POOL *pool) :
  _root_stmt(NULL), _root_x(NULL), _context(dna), _cur_idx(0), _children_count(0),
  _complexity(0), _orig_puname(NULL), _orig_stname(NULL), _forward(FALSE),
  _message(NULL), _msgid(NULL), _prev_bb(NULL), _prev_dna(NULL), _mem_pool(pool) {
  OPT_POOL_Push(pool, VSA_DUMP_FLAG);
  _root = _cur_node = CXX_NEW(SRCPOS_TREENODE(NULL, dna, pool), pool);
  _key_spos = CXX_NEW(KEY_SRCPOS_VEC(mempool_allocator<KEY_SRCPOS>(pool)), pool);
  _graph = VSA_Value_Graph ? CXX_NEW(VALUE_GRAPH(pool), pool) : NULL;
  _path = CXX_NEW(SRCPOS_PATH(dna->Comp_unit(), pool), pool);
  Set_orig_puname(dna->Fname());
}

// =============================================================================
//
// SRCPOS_HANDLE constructor
// @parm vsa: NULL(default), if set orig stname will be set by U-D
// @parm origin: NULL(default), if set, stpath from orign will be added
//               For ex: AOB orign is the checking cr, vul_cr is the base of orign,
//               append orign path info from stpath
//
// =============================================================================
SRCPOS_HANDLE::SRCPOS_HANDLE(CODEREP *vul_cr, STMTREP *stmt, DNA_NODE* dna, 
                             MEM_POOL *pool, VSA *vsa, CODEREP *orign) :
  _root_stmt(stmt), _root_x(vul_cr), _context(dna), _cur_idx(0), _children_count(0),
  _complexity(0), _orig_puname(NULL), _orig_stname(NULL), _forward(FALSE),
  _message(NULL), _msgid(NULL), _prev_bb(stmt->Bb()), _prev_dna(dna), _mem_pool(pool) {
  CONTEXT_SWITCH context(dna);
  OPT_POOL_Push(pool, VSA_DUMP_FLAG);
  _root = _cur_node = CXX_NEW(SRCPOS_TREENODE(NULL, dna, pool), pool);
  _key_spos = CXX_NEW(KEY_SRCPOS_VEC(mempool_allocator<KEY_SRCPOS>(pool)), pool);
  _graph = VSA_Value_Graph ? CXX_NEW(VALUE_GRAPH(pool), pool) : NULL;
  _path = CXX_NEW(SRCPOS_PATH(dna->Comp_unit(), pool), pool);
  _path->Add_bb(stmt->Bb());
  Append_data(stmt, dna, PATHINFO_VUL_SPOT);
  // Append stmt rhs's stpath to find back the path before copy propagation
  if (stmt->Opr() == OPR_STID && stmt->Rhs() != vul_cr && stmt->Rhs() != orign)
    Append_stpath(stmt, stmt->Rhs(), dna, FALSE);
  if (orign)
    Append_stpath(stmt, orign, dna, FALSE);
  if (vul_cr != orign)
    Append_stpath(stmt, vul_cr, dna, TRUE);
  // set original st name
  if (vsa != 0) {
    Set_orig_stname(Find_orig_stname(vul_cr, stmt, dna));
  }
  // set original pu name. if pu is inlined, get name from inline context
  if (stmt != NULL && stmt->Bb()->Inlinecxt() != NULL)
    Set_orig_puname(ST_name(stmt->Bb()->Inlinecxt()->Inlcxt_call_st()));
  else if (dna != NULL)
    Set_orig_puname(dna->Fname());
}

// =============================================================================
//
// SRCPOS_HANDLE::Clone(void)
//   return a clone of current SRCPOS_HANDLE
//
// =============================================================================
SRCPOS_HANDLE*
SRCPOS_HANDLE::Clone(MEM_POOL *pool)
{
  if(pool == NULL) {
    pool = this->Mem_pool();
  }
  SRCPOS_HANDLE *ret = CXX_NEW(SRCPOS_HANDLE(_context, pool), pool);
  SRCPOS_TR_ITER srcpos_tr_iter(this);
  SRCPOS_NODE last;
  while(!srcpos_tr_iter.Is_empty()) {
    SRCPOS_NODE node = srcpos_tr_iter.Next();
    if (node.Info() == PATHINFO_NONE)
      continue;
    if (!node.equal(last))
      ret->Append_data(node);
    last = node;
  }
  ret->_root_x = _root_x;
  ret->_key_spos->reserve(_key_spos->size());
  ret->_key_spos->assign(_key_spos->begin(), _key_spos->end());
  ret->_graph = NULL;
  ret->_path = _path->Clone(pool);
  if(_orig_stname) {
    INT stname_len = strlen(_orig_stname);
    char *orig_stname = (char*)MEM_POOL_Alloc(pool, stname_len + 1);
    memcpy(orig_stname, _orig_stname, stname_len);
    orig_stname[stname_len] = '\0';
    ret->Set_orig_stname(orig_stname);
  }
  ret->Set_orig_puname(_orig_puname);
  ret->_complexity = _complexity;
  if (_message) {
    ret->Set_msgid(_message);
  }
  ret->_msgid = _msgid;
  return ret;
}

BOOL
SRCPOS_HANDLE::Is_temp_var(const char *name)
{
  if(name) {
    if(name[0] == '.') {
      return TRUE;
    } else if(PU_java_lang(Get_Current_PU()) && !strncmp(name, "_CD_", 4)) {
      // java constant data(start with "_CD_")
      return TRUE;
    }
  }
  return FALSE;
}

BOOL
SRCPOS_HANDLE::Is_temp_var(ST_IDX st_idx)
{
  return Is_temp_var(ST_name(st_idx)) || ST_is_class_const_data(&St_Table[st_idx]);
}

const char *
SRCPOS_HANDLE::Find_orig_stname(CODEREP *cr, STMTREP *stmt, DNA_NODE *dna, BOOL forced, BOOL follow_ud) const
{
  if(!cr)
    return NULL;
  if (! forced) {
    if(Orig_stname() != NULL && !Is_temp_var(Orig_stname())) {
      return _orig_stname;
    }
  }

  return Find_cr_stname(cr, stmt, dna, follow_ud, TRUE); // TRUE for trying to generate stname for op like add
}

const char *
SRCPOS_HANDLE::Find_cr_stname(STRING_BUFFER *buf, CODEREP *cr, STMTREP *stmt, DNA_NODE *dna,
                              BOOL follow_ud, BOOL gen_cr, BOOL hex)
{
  if(IPSA_insession() && dna != NULL && stmt != NULL) {
    STPATH *stpath = dna->Get_stpath(stmt, cr);
    if(stpath && !Is_temp_var(stpath->St_name()) && !stmt->Bb()->Inlinecxt()) {
      const char* name = stpath->St_name();
      buf->Append(name);
      return buf->To_string();
    }
  }

  OPT_STAB* opt_stab = dna->Comp_unit()->Opt_stab();
  if (cr->Kind() == CK_VAR) {
    AUX_STAB_ENTRY *sym = opt_stab->Aux_stab_entry(cr->Aux_id());
    char *st_name = Vsa_sym_name(sym);
    const char *new_name = NULL;
    if(Is_temp_var(st_name)) {
      if(ST_is_class_const_data(sym->St())) {
        new_name = Get_class_const_value(sym->St(), cr->Offset()/Pointer_Size);
        // use orignal st_name if null const value
        new_name ? buf->Append(new_name)
                 : buf->Append_stname(st_name);
        return buf->To_string();
      }
      if(cr->Is_flag_set(CF_DEF_BY_CHI)) {
        if(cr->Def_at_entry()) {
          switch (ST_sclass(sym->St())) {
            case SCLASS_FORMAL:
            case SCLASS_FORMAL_REF:
            // TODO: get from parameter name
              break;
            case SCLASS_REG:
            // TODO: add an assertion, should be get from stpath
              break;
            case SCLASS_UGLOBAL:
            case SCLASS_DGLOBAL:
            case SCLASS_FSTATIC:
            case SCLASS_AUTO:
              break;
            default:
              break;
          }
        } else {
          STMTREP *def_stmt = cr->Defstmt();
          CODEREP *rhs = def_stmt->Rhs();
          return Find_cr_stname(buf, rhs, def_stmt, dna, follow_ud, gen_cr, hex);
        }
      } else if(cr->Is_flag_set(CF_DEF_BY_PHI)) {
        // TODO
        // Is_True(false, ("TODO: temp name need resolve"));
        //buf->Append(st_name);
        return NULL;
      } else if(follow_ud && cr->Defstmt()) {
        STMTREP *def_stmt = cr->Defstmt();
        CODEREP *rhs = def_stmt->Rhs();
        new_name = Find_cr_stname(buf, rhs, def_stmt, dna, TRUE, gen_cr, hex);
      }
    } else {
      buf->Append(st_name);
    }
    if(cr->Field_id() > 0) {
      new_name = Gen_fld_stname(buf, NULL, cr->Lod_ty(), cr->Field_id());
    }
    return buf->To_string();
  }
  else if (cr->Kind() == CK_LDA) {
    char *st_name = NULL;
    ST *lda_st = cr->Lda_base_st();
    if (ST_class(lda_st) == CLASS_CONST &&
        TCON_ty(ST_tcon_val(lda_st)) == MTYPE_STR) {
      st_name = Index_to_char_array(TCON_str_idx(ST_tcon_val(lda_st)));
      buf->Append(st_name, TRUE);  // str may contains invalid character
    }
    else {
      AUX_STAB_ENTRY *sym = opt_stab->Aux_stab_entry(cr->Lda_aux_id());
      st_name = Vsa_sym_name(sym);
      buf->Append_stname(st_name);
    }
    return buf->To_string();
  }
  else if(cr->Kind() == CK_IVAR) {
    if (cr->Opr() == OPR_PARM) {
      return Find_cr_stname(buf, cr->Ilod_base(), stmt, dna, follow_ud, gen_cr, hex);
    }
    CODEREP *base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
    Is_True(base, ("base is null for Ivar"));
    VSA_ADDRESS_INFO info;
    dna->Comp_unit()->Analyze_address_info(stmt, cr, &info,
                                           cr->Ilod_base() == NULL, FALSE);
    CODEREP *base_cr = info.Base();
    if (base_cr == NULL) {
      // no base?
      if (PU_src_lang(Get_Current_PU()) & (PU_C_LANG | PU_CXX_LANG))
        buf->Append('*');
      buf->Append('(');
      Find_cr_stname(buf, base, stmt, dna, TRUE, gen_cr, hex);
      if (cr->Offset() > 0)
        buf->Format("+%d", cr->Offset());
      buf->Append(')');
      return buf->To_string();
    }
    // workaround for invalid non-struct ilod_ty with field_id != 0
    TY_IDX object_ty = TY_kind(cr->Ilod_ty()) == KIND_STRUCT ? cr->object_ty()
                                                             : cr->Ilod_ty();
    TY_IDX ilod_addr_ty = cr->Opr() != OPR_MLOAD ? cr->Ilod_base_ty()
                                                 : Make_Pointer_Type(cr->Ilod_ty());
    TY_IDX base_addr_ty = base_cr->Kind() == CK_LDA ? base_cr->Lda_ty()
                                                    : base_cr->object_ty();
    if (base_cr->Kind() == CK_VAR &&
        opt_stab->Aux_stab_entry(base_cr->Aux_id())->Is_preg())
      base_addr_ty = ilod_addr_ty;
    Is_True(TY_kind(base_addr_ty) == KIND_POINTER, ("base addr is not pointer type"));
    Is_True(TY_kind(ilod_addr_ty) == KIND_POINTER, ("ilod addr is not pointer type"));
    UINT fld_id = cr->I_field_id();
    INT64 offset = cr->Offset();
    BOOL var_ofst = (info.Pos_index().size() > 0 || info.Neg_index().size() > 0 ||
                     info.Pos_offset().size() > 0 || info.Neg_offset().size() > 0);
    if (fld_id == 0 && offset == 0 && !var_ofst)
      buf->Append('*');
    // handle base
    Find_cr_stname(buf, base_cr, stmt, dna, TRUE, gen_cr, hex);
    // check if load from struct pointer
    TY_IDX ilod_ty = ilod_addr_ty;
    if (TY_kind(ilod_addr_ty) == KIND_POINTER)
      ilod_ty = TY_pointed(ilod_addr_ty);
    if (Is_Structure_Type(ilod_ty) &&
        (fld_id > 0 || offset > 0)) {
      if (fld_id == 0)
        fld_id = Get_field_id_from_offset(ilod_ty, offset, object_ty);
      if (fld_id > 0) {
        Gen_fld_stname(buf, NULL, ilod_addr_ty, fld_id);
        offset = 0;
      }
    }
    else {
      TY_IDX base_ty = base_addr_ty;
      if (TY_kind(base_addr_ty) == KIND_POINTER)
        base_ty = TY_pointed(base_addr_ty);
      if (Is_Structure_Type(base_ty) &&
          (fld_id > 0 || cr->Offset() > 0)) {
        if (fld_id == 0)
          fld_id = Get_field_id_from_offset(base_ty, offset, object_ty);
        if (fld_id > 0) {
          Gen_fld_stname(buf, NULL, base_addr_ty, fld_id);
          offset = 0;
        }
      }
    }

    INT size = TY_size(object_ty);
    if (size == 0)
      size = 1;  // adjust size to avoid DBZ
    if (var_ofst || offset != 0) {
      buf->Append('[');
      BOOL add_prefix = FALSE;
      STPATH *stpath;
      if (dna && (stpath = dna->Get_stpath(stmt, base)) != NULL) {
        Find_cr_stname(buf, base, stmt, dna, TRUE, gen_cr, hex);
      }
      else {
        INT i;
        for (i = 0; i < info.Pos_index().size(); ++i) {
          Is_True(info.Pos_index()[i].second &&
                  info.Pos_index()[i].second->Kind() == CK_CONST,
                  ("invalid scale cr"));
          if (add_prefix)
            buf->Append('+');
          else
            add_prefix = TRUE;
          Find_cr_stname(buf, info.Pos_index()[i].first, stmt, dna, TRUE, gen_cr, hex);
          INT scale = 1;
          if (info.Pos_index()[i].second &&
              info.Pos_index()[i].second->Kind() == CK_CONST)
            scale = info.Pos_index()[i].second->Const_val() / size;
          if (scale > 1)
            buf->Format("*%d", scale);
        }
        for (i = 0; i < info.Pos_offset().size(); ++i) {
          if (add_prefix)
            buf->Append('+');
          else
            add_prefix = TRUE;
          Find_cr_stname(buf, info.Pos_offset()[i], stmt, dna, TRUE, gen_cr, hex);
        }
      }
      if (fld_id == 0 && offset != 0) {
        if (add_prefix && offset > 0)
          buf->Append('+');
        buf->Format("%d", offset/size);
      }
      buf->Append(']');
    }
    return buf->To_string();
  } else if(cr->Kind() == CK_OP && stmt != NULL) {
    OPERATOR opr = stmt->Opr();
    if((opr == OPR_INTRINSIC_CALL) &&
       (stmt->Rhs()->Intrinsic() == INTRN_CHECK_CAST) &&
       (cr->Kid_count() > 1) &&
       (cr->Opnd(1)->Kind() == CK_OP) &&
       (cr->Opnd(1)->Opr() == OPR_PARM)) {
      CODEREP *tc_cr = cr->Opnd(1)->Ilod_base();
      return Find_cr_stname(buf, tc_cr, stmt, dna, follow_ud, gen_cr, hex);
    } else if(dna &&
              opr == OPR_CALL &&
              OPERATOR_has_sym(opr) &&
              Vsa_check_alloc_heap(ST_name(stmt->St()))) {
      if(IPSA_insession()) {
        // check st name from call stpath first
        STPATH *stpath = dna->Get_stpath(stmt, NULL);
        if(stpath && !Is_temp_var(stpath->St_idx())) {
          const char* st_name = ST_name(stpath->St_idx());
          buf->Append(st_name);
          return buf->To_string();
        }
      }
      CODEREP *ret_cr = dna->Comp_unit()->Find_return_value(stmt);
      STMTREP *nxtstmt = NULL;
      STMTREP *defstmt = NULL;
      if (ret_cr != NULL) {
        nxtstmt = stmt->Next();
        if (nxtstmt != NULL && nxtstmt->Opr() == OPR_STID &&
            nxtstmt->Rhs() == ret_cr) {
          ret_cr = nxtstmt->Lhs();
          defstmt = nxtstmt;
          nxtstmt = nxtstmt->Next();
          if(nxtstmt != NULL && 
             (nxtstmt->Opr() == OPR_STID || nxtstmt->Opr() == OPR_ISTORE) &&
             nxtstmt->Rhs() == ret_cr) {
            ret_cr = nxtstmt->Lhs();
            defstmt = nxtstmt;
          }
        }
      }
      Is_True(ret_cr && defstmt, ("return cr/stmt is NULL"));
      if (ret_cr == NULL || defstmt == NULL)
        return NULL;
      const char *ret_str = Find_cr_stname(buf, ret_cr, defstmt, dna, FALSE, gen_cr, hex);
      if (ret_str == NULL && gen_cr) {
        return Gen_cr_stname(buf, cr, stmt, dna, follow_ud, hex);
      } else {
        return ret_str;
      }
    } else if (cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL) {
      return Find_cr_stname(buf, cr->Opnd(0), stmt, dna, follow_ud, gen_cr, hex);
    } else if (gen_cr) {
      return Gen_cr_stname(buf, cr, stmt, dna, follow_ud, hex);
    } else {
      return NULL;
    }
  } else if (cr->Kind() == CK_OP && gen_cr) {
    return Gen_cr_stname(buf, cr, stmt, dna, follow_ud, hex);
  } else if (cr->Kind() == CK_CONST) {
    if (!PU_java_lang(Get_Current_PU()) || cr->Const_val() != 0)
      buf->Format(hex ? "0x%x" : "%ld", cr->Const_val());
    else
      buf->Append("null(0)");
    return buf->To_string();
  } else if (cr->Has_const_fval()) {
    buf->Format("%0.1lf", cr->Const_fval());
    return buf->To_string();
  }
  return NULL;
}

// =============================================================================
//
// SRCPOS_HANDLE::Gen_cr_stname(STRING_BUFFER *buf, CODEREP *cr, STMTREP *stmt,
//                              DNA_NODE *dna, BOOL hex)
//   generate stname for cr which is CK_OP
//
// =============================================================================
const char*
SRCPOS_HANDLE::Gen_cr_stname(STRING_BUFFER *buf, CODEREP *cr, STMTREP *stmt,
                             DNA_NODE *dna, BOOL follow_ud, BOOL hex)
{
  Is_True(cr != NULL && cr->Kind() == CK_OP, ("cr is not op"));
  // cr is too complicated, give up
  if (Count_cr_kids(cr) > 12)
    return NULL;

  OPERATOR opr = cr->Opr();
  INT  kid_count = cr->Kid_count();
  char opr_sign = '\0';

  switch (cr->Opr()) {
  case OPR_CALL:
    // v2csf doesn't allow spaces in variable name. so simply generate
    // retval_of_call or retval_of_icall
    if (stmt != NULL && stmt->Opr() == OPR_CALL) {
      buf->Append("retval_of_call");
    }
    else {
      buf->Append("retval_of_icall");
    }
    return buf->To_string();
  case OPR_ICALL:
  {
    // generate name for obj.func()
    const char *obj_name = Find_cr_stname(buf, cr->Opnd(0),
                                          stmt, dna, follow_ud, FALSE, hex);
    if(obj_name != NULL) {
      if(PU_java_lang(Get_Current_PU())) {
        buf->Append(".");
      } else {
        buf->Append("->");
      }
      VSA *vsa = dna->Comp_unit()->Vsa();
      RNA_NODE *rna = dna->Get_callsite_rna(stmt);
      RNA_CALLEE_ITER iter(vsa->Ipsa(), rna);
      if(!iter.Is_end()) {
        DNA_NODE *first_callee = iter.Current();
        if(first_callee) {
          const char *fun_name = first_callee->Fname();
          STRING_BUFFER sbuf(strlen(fun_name + 1));
          const char *fun_sig = CLASS_HIERARCHY::Extract_fun_sig(fun_name, &sbuf);
          if(fun_sig) {
            buf->Append(fun_sig);
            return buf->To_string();
          }
        }
      }
      buf->Append("$call$");
    }
    return buf->To_string();
  }
  case OPR_INTRINSIC_CALL:
    return buf->To_string();
  case OPR_CVT:
  case OPR_CVTL:
    return Find_cr_stname(buf, cr->Opnd(0), stmt, dna, follow_ud, TRUE, hex);

  case OPR_NEG:
    opr_sign = '-';
    break;
  case OPR_ADD:
    opr_sign = '+';
    break;
  case OPR_SUB:
    opr_sign = '-';
    break;
  case OPR_MPY:
    opr_sign = '*';
    break;
  case OPR_DIV:
    opr_sign = '/';
    break;

  default:
    if (kid_count > 2) {
      buf->Append(OPERATOR_name(cr->Opr()) + 4);
      return buf->To_string();
    }
  }
  // only output opr if no kid
  if (kid_count == 0) {
    buf->Append(OPERATOR_name(cr->Opr()) + 4);
    return buf->To_string();
  }
  // output opr opnd0 for 1 kid
  if (kid_count == 1) {
    if (opr_sign != '\0')
      buf->Append(opr_sign);
    else
      buf->Append(OPERATOR_name(cr->Opr()) + 4);
  }

  BOOL paren = cr->Opnd(0)->Kind() == CK_OP &&
               cr->Opnd(0)->Opr() != OPR_CVT;
  if (paren)
    buf->Append('(');
  Find_cr_stname(buf, cr->Opnd(0), stmt, dna, follow_ud, TRUE, hex);
  if (paren)
    buf->Append(')');

  if (kid_count == 1)
    return buf->To_string();

  // output opnd0 opr opnd1 for 2 kid
  if (opr_sign != '\0')
    buf->Append(opr_sign);
  else
    buf->Append(OPERATOR_name(cr->Opr()) + 4);

  paren = cr->Opnd(1)->Kind() == CK_OP &&
          cr->Opnd(1)->Opr() != OPR_CVT;
  if (paren)
    buf->Append('(');
  Find_cr_stname(buf, cr->Opnd(1), stmt, dna, follow_ud, TRUE, hex);
  if (paren)
    buf->Append(')');
  return buf->To_string();
}

// =============================================================================
//
// SRCPOS_HANDLE::Set_orig_stname
//   set orig_stname according to the variable name from cr
//
// =============================================================================
void
SRCPOS_HANDLE::Set_orig_stname(DNA_NODE* dna, CODEREP* cr)
{
  Set_orig_stname(Find_orig_stname(cr, NULL, dna));
}


const char *
SRCPOS_HANDLE::Gen_fld_stname(STRING_BUFFER *buf, const char *st_name,
                              TY_IDX st_ty, UINT16 field_id)
{
  if (st_name != NULL && *st_name != '\0')
    buf->Append(st_name);
  if(field_id > 0 && TY_kind(st_ty) != KIND_SCALAR) {
    const char *dot = ".";
    if(TY_kind(st_ty) == KIND_POINTER) {
      st_ty = TY_pointed(st_ty);
      if (PU_src_lang(Get_Current_PU()) & (PU_C_LANG | PU_CXX_LANG))
        dot = "->";
    }
    if(TY_kind(st_ty) != KIND_STRUCT) {
      return buf->To_string();
    }
    buf->Append(dot);
    INT avail = buf->Avail();
    // Get_composed_fld_name() requires at least 5 bytes for "...$"
    if (avail < 5) {
      buf->Adjust_current(avail - 5);
      buf->Append("...$");
      return buf->To_string();
    }
    char *fld_name = Get_composed_fld_name(st_ty, field_id,
                                           buf->Current(), avail);
    if (fld_name == NULL)
      buf->Append("$fld_err$");
    else
      buf->Adjust_current();  // place _current to right location
  }
  return buf->To_string();
}

// =============================================================================
// SRCPOS_HANDLE::Set_key_srcpos
// =============================================================================
void
SRCPOS_HANDLE::Set_key_srcpos(DNA_NODE* dna, STMTREP* stmt, CODEREP *cr)
{
  Is_True(dna && stmt, ("invalid func or stmt"));
  STPATH *stpath;
  UINT index;
  // check cr stpath
  if ((cr != NULL &&
       (stpath = dna->Get_stpath(stmt, cr)) != NULL &&
       (index = stpath->Path_size()) > 0) ||
      (stmt->Opr() == OPR_STID &&
       (stpath = dna->Get_stpath(stmt, stmt->Rhs())) != NULL &&
       (index = stpath->Path_size()) > 0)) {
    SRCPOS_NODE path = stpath->Path(index - 1);
    SRCPOS spos = path.Spos();
    if (spos == 0)
      spos = stmt->Linenum();
    Is_True(spos != 0, ("srcpos is 0"));
    const char *name = stpath->St_name();
    _key_spos->push_back(KEY_SRCPOS(dna, path.Inlcxt(), spos, name));
    return;
  }
  // normal step
  if (stmt->Linenum() != 0) {
    const char *name = cr ? Find_cr_stname(cr, stmt, dna, FALSE, FALSE, FALSE)
                          : "";
    Set_key_srcpos(dna, stmt->Bb(), stmt->Linenum(), name);
  }
}

// =============================================================================
// SRCPOS_HANDLE::Append_control_dependency
// =============================================================================
void
SRCPOS_HANDLE::Append_control_dependency(BB_NODE* succ, BB_NODE* pred,
                                         DNA_NODE* dna, BOOL add_data)
{
  COMP_UNIT* cu = dna->Comp_unit();
  if(!cu->Vra()) {
    return;
  }
  std::vector< std::pair<BB_NODE*, BB_NODE*> > cds;
  if(Is_forward()) {
    cu->Vra()->Collect_control_dependencies(pred, succ, cds);
  } else {
    cu->Vra()->Collect_control_dependencies(succ, pred, cds);
  }
  std::vector< std::pair<BB_NODE*, BB_NODE*> >::iterator it;
  SRCPOS spos = 0;
  for (it = cds.begin(); it != cds.end(); ++it) {
    STMTREP* stmt = it->first->Last_stmtrep();
    if (stmt == NULL ||
        (stmt->Opr() != OPR_TRUEBR &&
         stmt->Opr() != OPR_FALSEBR &&
         stmt->Opr() != OPR_COMPGOTO))
      continue;

    if (!Find_control_dependency(cu, it->first, it->second))
      _cur_node->Append_control_dependency(dna->Comp_unit(), it->first, it->second);

    // only add control dependency for phi result
    if (add_data == FALSE)
      continue;

    // ignore other conditions on the same line
    if (spos == stmt->Linenum())
      continue;
    spos = stmt->Linenum();

    // TODO: check BB_kind and insert different PATHINFO, especially for loop
    BOOL then_taken = it->first->Next() == it->second;
    BOOL cond_taken = (stmt->Opr() == OPR_TRUEBR && then_taken) ||
                      (stmt->Opr() == OPR_FALSEBR && !then_taken);
    PATH_INFO then_info =  then_taken ? PATHINFO_THEN_TAKEN : PATHINFO_ELSE_TAKEN;
    PATH_INFO cond_info =  cond_taken ? PATHINFO_COND_FALSE : PATHINFO_COND_TRUE;
    if(Is_forward()) {
      // add cond evaluates to true/false
      _cur_node->Append_data(stmt, cu->Dna(), cond_info);
      // add then/else block
      _cur_node->Append_data(it->second, cu->Dna(), then_info);
    } else {
      // add then/else block
      _cur_node->Append_data(it->second, cu->Dna(), then_info);
      // add cond evaluates to true/false
      _cur_node->Append_data(stmt, cu->Dna(), cond_info);
    }
  }
}

// =============================================================================
//
// SRCPOS_HANDLE::Append_data
//   (SRCPOS spos, BB_NODE* bb, DNA_NODE* dna, PATH_INFO info)
//     append <spos, dna, info> into current SRCPOS_NODE
//   (ST* st, BB_NODE* bb, DNA_NODE* dna, PATH_INFO info)
//     append <spos, dna, info> into current SRCPOS_NODE
//   (BB_NODE* bb, DNA_NODE* dna, PATH_INFO info)
//     append <bb, dna, info> into current SRCPOS_NODE
//   (STMTREP* stmt, DNA_NODE* dna, PATH_INO info)
//     append <stmt, dna, info> into current SRCPOS_NODE
// =============================================================================
void
SRCPOS_HANDLE::Append_data(SRCPOS spos, BB_NODE* bb, DNA_NODE* dna, INT32 info)
{
  // add control dependency
  if (_prev_bb != NULL && bb != NULL && _prev_bb != bb && _prev_dna == dna)
    Append_control_dependency(_prev_bb, bb, dna, TRUE);
  if (_prev_bb != bb && bb != NULL)
    _prev_bb = bb;
  if (_prev_dna != dna)
    _prev_dna = dna;
  // add data
  _cur_node->Append_data(spos, dna, info);
}

void
SRCPOS_HANDLE::Append_data(ST* st, BB_NODE* bb, DNA_NODE* dna, INT32 info)
{
  // add control dependency
  if (_prev_bb != NULL && bb != NULL && _prev_bb != bb && _prev_dna == dna)
    Append_control_dependency(_prev_bb, bb, dna, TRUE);
  if (_prev_bb != bb && bb != NULL)
    _prev_bb = bb;
  if (_prev_dna != dna)
    _prev_dna = dna;
  // add data
  _cur_node->Append_data(st, dna, info);
}

void
SRCPOS_HANDLE::Append_data(BB_NODE* bb, DNA_NODE* dna, INT32 info)
{
  // only add control dependency for phi -> use of phi result
  // don't add control dependency for phi pred -> phi
  if (_prev_bb != NULL && bb != NULL && _prev_bb != bb && _prev_dna == dna)
    Append_control_dependency(_prev_bb, bb, dna, FALSE);
  if (_prev_bb != bb && bb != NULL)
    _prev_bb = bb;
  if (_prev_dna != dna)
    _prev_dna = dna;
  // add data
  _cur_node->Append_data(bb, dna, info);
}

void
SRCPOS_HANDLE::Append_data(STMTREP* stmt, DNA_NODE* dna, INT32 info, BOOL add_data)
{
  // add control dependency
  BB_NODE* bb = stmt->Bb();
  if (_prev_bb != NULL && _prev_bb != bb && _prev_dna == dna)
    Append_control_dependency(_prev_bb, bb, dna, add_data);
  if (_prev_bb != bb)
    _prev_bb = bb;
  if (_prev_dna != dna)
    _prev_dna = dna;
  // add data
  _cur_node->Append_data(stmt, dna, info);
}

// =============================================================================
//
// SRCPOS_HANDLE::Append_stpath
//   if there is stpath annotated on cr, append it
//   if set_orig_st is TRUE, SRCPOS_HANDLE's _orig_stname is set from stpath
//
// =============================================================================
void
SRCPOS_HANDLE::Append_stpath(STMTREP *stmt, CODEREP *cr, DNA_NODE* dna, BOOL force_set_orig_st)
{
  if(IPSA_insession() && dna != NULL) {
    CODEREP* var = cr;
    STPATH *stpath = dna->Get_stpath(stmt, var);
    if (stpath == NULL) {
      stpath = dna->Search_stpath(stmt, var);
      // set to FALSE because stpath is not for var, but var's parents
      force_set_orig_st = FALSE;
    }
    #if 0
    if (stpath == NULL && var && var->Kind() == CK_IVAR) {
      var = cr->Ilod_base();
      stpath = dna->Get_stpath(stmt, var);
      while (stpath == NULL && var && var->Kind() == CK_OP &&
             (var->Opr() == OPR_CVT || var->Opr() == OPR_CVTL)) {
        var = var->Opnd(0);
        stpath = dna->Get_stpath(stmt, var);
      }
    }
    #endif
    if(stpath) {
      if (force_set_orig_st) {
        ST_IDX stidx = stpath->St_idx();
        Is_True(stidx != ST_IDX_ZERO,
                ("invalid st_idx in stpath"));
        if(stidx != ST_IDX_ZERO) {
          if(ST_class(stidx) == CLASS_VAR) 
            _orig_stname = Gen_fld_stname(ST_name(stidx), ST_type(stidx), stpath->Field_id());
          else {
            _orig_stname = ST_name(stidx);
          }
        }
      }
      for(int i =0; i < stpath->Path_size(); i++) {
        _cur_node->Append_data(stpath->Path(i));
      }
    }
  }
}

// =============================================================================
//
// =============================================================================
void
SRCPOS_HANDLE::Compose_path_selected(IPSA* ipsa, PATH_SELECTED* path) const
{
  Is_True(ipsa != NULL, ("Need ipsa object"));
  Is_True(path != NULL, ("Need path object"));

  SRCPOS_TR_ITER srcpos_tr_iter(this);
  SRCPOS_NODE last_vspos;
  RNA_NODE* rna;
  while (! srcpos_tr_iter.Is_empty()) {
    SRCPOS_NODE vspos = srcpos_tr_iter.Next();
    if (vspos.Info() == PATHINFO_NONE)
      continue;
    switch (vspos.Info()) {
    case PATHINFO_PHI:
      Is_True(last_vspos.Info() == PATHINFO_BRANCH, ("last vspos is not phi"));
      Is_True(last_vspos.Dna() == vspos.Dna(), ("last vspos dna mismatch"));
      path->Add_phi(vspos.Dna()->Dna_idx(), vspos.Bb()->Id(), last_vspos.Bb()->Id());
      break;
    case PATHINFO_DNA_CALLRETURN:
    case PATHINFO_DNA_CALLSITE:
      rna = vspos.Dna()->Get_callsite_rna(vspos.Stmt());
      Is_True(rna != NULL, ("failed to get rna. wrong dna pointer used?"));
      if (rna != NULL)
        path->Add_rna(rna->Rna_idx());
      break;
    }
    last_vspos = vspos;
  }
}

// =============================================================================
//
//  This function creates a list of paths to the SRCPOS_VECTOR
//  This function relies on the information
//  captured with the srcpos_h.
//
// =============================================================================

void
SRCPOS_HANDLE::Compose_path_vector(SRCPOSINFO_VECTOR *sposinfo_vector) const
{
  SRCPOS_TR_ITER srcpos_tr_iter(this);
  INLCXT* left_cxt = NULL;
  UINT left_idx = 0;
  std::stack< pair<SRCPOS, INLCXT*> > stack;
  hash_set<IDTYPE> dna_set;
  UINT num_nodes = 0;
  BOOL lang_c_cxx = 0;
  BOOL lang_java = 0;
  SRCPOS_NODE prev_vspos;

  while (! srcpos_tr_iter.Is_empty()) {
    SRCPOS_NODE vspos = srcpos_tr_iter.Next();
    if (vspos.Info() == PATHINFO_NONE)
      continue;
    ++ num_nodes;
    DNA_NODE *dna = vspos.Dna();
    if (dna) {
      dna_set.insert(dna->Dna_idx());
      Is_True((PU_src_lang(*dna->Pu()) >> 6) == 0,
              ("New language added?"));
      if (lang_java == FALSE &&
          (PU_src_lang(*dna->Pu()) & PU_JAVA_LANG))
        lang_java = TRUE;
      else if (lang_c_cxx == FALSE)
        lang_c_cxx = TRUE;
    }

    INLCXT* right_cxt  = vspos.Inlcxt();
    UINT file_idx = vspos.File_idx();
    if (left_idx != file_idx) {
      // switched to another file, reset left_cxt
      left_idx = file_idx;
      left_cxt = NULL;
    }

    // check if  left_cxt is deeper or left_cxt == right_cxt
    INLCXT* lcxt = left_cxt;
    while(lcxt && *lcxt != right_cxt)
      lcxt = lcxt->Parent();
    INLCXT* rcxt = NULL;
    if(lcxt == NULL) {
      // check if  right_cxt is deeper
      rcxt = right_cxt;
      while(rcxt && *rcxt != left_cxt)
        rcxt = rcxt->Parent();
    }
    
    // if right_cxt is not deeper, either
    // 1. lcxt != NULL and rcxt == NULL , left_cxt is deeper or left_cxt == right_cxt
    // 2. lcxt == NULL and rcxt == NULL , left_cxt and right_cxt in different inline context
    if( rcxt == NULL ) {
      for(INLCXT* cxt = left_cxt; cxt && *cxt != lcxt; cxt =cxt->Parent()) {
        SRCPOS spos = SRCPOS_NODE::Transpos(cxt->Inlcxt_line_num(), file_idx);
        // instead of PATHINFO_INL_END, emitting a PATHINFO_DNA_CALLRETURN
        if (spos != 0) {
          // spos is for callsite, try to get fname from ctx's Parent or vspos's DNA
          const char* fname;
          fname = cxt->Parent() ? ST_name(file_idx, cxt->Parent()->Inlcxt_call_st())
                                : vspos.Dna()->Fname();
          sposinfo_vector->push_back(SRCPOSINFO(spos,
                                                PATHINFO_DNA_CALLRETURN,
                                                NULL,
                                                fname));
        }
      }
    }
    // if left_cxt is not deeper, either
    // 1. rcxt != NULL and lcxt == NUL, right_cxt is deeper
    // 2. rcxt == NULL and lcxt == NULL, left_cxt and right_cxt in different inline context
    if( lcxt == NULL ) {
      for(INLCXT* cxt = right_cxt; cxt && *cxt != rcxt; cxt = cxt->Parent()) {
        stack.push(make_pair(SRCPOS_NODE::Transpos(cxt->Inlcxt_line_num(), file_idx),
                             cxt));
      }
      while(!stack.empty()) {
        SRCPOS  spos = stack.top().first;
        INLCXT* cxt  = stack.top().second;
        // instead of PATHINFO_INL_BEGIN, emitting a PATHINFO_DNA_CALLSITE
        // when this is the first line in sposinfo_vector, don't add this callsite
        // because the "real" first line is in the inline function body
        ST* call_st = St_ptr(vspos.Dna()->File_idx(), cxt->Inlcxt_call_st());
        if (spos != 0 &&
            (sposinfo_vector->size() > 0 || vspos.Transpos() == ST_Srcpos(*call_st))) {
          // spos is for callsite, try to get fname from ctx's Parent or vspos's DNA
          const char* fname;
          fname = cxt->Parent() ? ST_name(file_idx, cxt->Parent()->Inlcxt_call_st())
                                : vspos.Dna()->Fname();
          sposinfo_vector->push_back(SRCPOSINFO(spos,
                                                PATHINFO_DNA_CALLSITE,
                                                NULL,
                                                fname));
        }
        stack.pop();
      }
    }
    left_cxt = right_cxt;
    SRCPOS spos = vspos.Transpos();
    if (spos != 0 && vspos.Info() != PATHINFO_PHI && vspos.Info() != PATHINFO_DUMMY) {
      if (vspos.equal(prev_vspos))
        continue;
      prev_vspos = vspos;
      const char *vname = vspos.Vname();
      if (vname == NULL &&
          (vspos.Info() == PATHINFO_VUL_SPOT || vspos.Info() == PATHINFO_ST_DECLARE))
        vname = Orig_stname();
      sposinfo_vector->push_back(SRCPOSINFO(spos, vspos.Info(), vname, vspos.Fname()));
    }
  }

  // set the complexity = num_nodes + num_dnas + 2 * (nul_of_languages)
  _complexity = num_nodes + dna_set.size() + 2 * (lang_c_cxx + lang_java);
}

// =============================================================================
//
//  This function creates a list of paths in the string returned to the caller,
//  caller needs to free the string.  This function relies on the information
//  captured with the srcpos_h.
//
// =============================================================================
char *
SRCPOS_HANDLE::Compose_path_string(void) const
{
  INT headcnt = VSA_Path_Head_Max();
  INT tailcnt = VSA_Path_Tail_Max();
  INT uselen = (headcnt + tailcnt) * 30;
  INT buflen = uselen + 80;
  char  *errmsg_buf = (char *)malloc(buflen);
  Is_True ((errmsg_buf != 0), ("malloc error - return null\n"));
  
  errmsg_buf[0] = '[';
  INT totalmsg_sz = 1;

  SRCPOSINFO_VECTOR vector;
  Compose_path_vector(&vector);
  BOOL truncated = vector.size() > (headcnt + tailcnt) ? TRUE : FALSE;
  if (VSA_Alert_Huge_Path && vector.size() > headcnt + tailcnt)
    FmtAssert(FALSE, ("SRCPOS_HANDLE::Compose_path_string Path Count Exceed Limit"));

  SRCPOSINFO_VECTOR::iterator it;
  INT cnt = 1;
  for(it = vector.begin(); it != vector.end(); it++) {
    SRCPOS spos = it->Spos();
    INT info = it->Msg_id();
    if (info == PATHINFO_COPY_PROP) info = PATHINFO_COPY;  // reset copy_prop to avoid remaster
    if (info == PATHINFO_VUL_SPOT_SO) info = PATHINFO_VUL_SPOT;// reset SPOT_SO to SPOT
    totalmsg_sz += snprintf(&errmsg_buf[totalmsg_sz], buflen - totalmsg_sz,
                            "%d:%d:%d:%d,", SRCPOS_filenum(spos),
                            SRCPOS_linenum(spos), SRCPOS_column(spos), info);
    // We hack around buffer overflow by assuming per "context" size < 100. If > 100, we should crap out
    // the right way to do this is to either spit out one at a time, or, form the context output first,
    // copy to buffer if within limit. Otherwise, truncate.
    if (totalmsg_sz >= uselen)
      break;
    if (!VSA_Alert_Huge_Path && truncated == TRUE) {
      if (cnt >= headcnt) {
        totalmsg_sz += snprintf(&errmsg_buf[totalmsg_sz], buflen - totalmsg_sz,
                                "-1:-1:-1:-1,");
        std::advance(it, vector.size() - headcnt - tailcnt);
        truncated = FALSE;
      }
      ++ cnt;
    }
  }

  if(errmsg_buf[totalmsg_sz - 1] == ',')
    -- totalmsg_sz;  // overwrite the last comma

  errmsg_buf[totalmsg_sz] = ']';
  errmsg_buf[totalmsg_sz + 1] = '\0';
  
  return errmsg_buf; // caller need to free this
}

// =============================================================================
// SRCPOS_HANDLE::Write_path_json
// NOT USED ANY MORE. Refer new code in opt_vsa_report.h/cxx
// =============================================================================
void
SRCPOS_HANDLE::Write_path_json(FILE *fp, SRCPOS master) const
{
  INT count = 0;
  INT max_count = VSA_Json_Path_Max();
  SRCPOS spos = 0;
  SRCPOSINFO_VECTOR vector;
  
  Compose_path_vector(&vector);
  for(SRCPOSINFO_VECTOR::iterator it = vector.begin(); it != vector.end(); it++) {
    INT32 msg = it->Msg_id();
    spos = it->Spos();
    INT32 linenum = Srcpos_To_Line(spos);
    INT32 filenum = SRCPOS_filenum(spos);
    INT32 colnum  = SRCPOS_column(spos);
    if (count > 0)
      fprintf(fp, ",\n");
    ++ count;
    fprintf(fp, "    { \"fileId\":\"%d\", \"lineNo\":%d, \"columnNo\":%d, \"message\":\"%d\", "
                "\"variableName\":\"\", \"functionName\":\"\" }",
            filenum, linenum, colnum, msg);
    if (count > max_count)
      break;
  }
  
  if (count == 0 || spos != master) {
    if (count > 0)
      fprintf(fp, ",\n");
    fprintf(fp, "    { \"fileId\":\"%d\", \"lineNo\":%d, \"columnNo\":%d, \"message\":\"%d\", "
                "\"variableName\":\"\", \"functionName\":\"\" }",
                SRCPOS_filenum(master), Srcpos_To_Line(master), SRCPOS_column(master),
                PATHINFO_VUL_SPOT);
  }
}

const char*
SRCPOS_HANDLE::Path_to_key(char* buf, INT len, const char* var,
                           const char* rname, const char* cname, IDTYPE& keyid) const
{
  IPSA *ipsamgr = NULL;
  STRING_BUFFER sbf(buf, len);
  // append variable name
  if (var && *var != '\0') {
    sbf.Append(var);
    sbf.Append('@');
  }
  // append rule wich shouldn't be empty
  sbf.Append(rname);
  if (cname && *cname) {
    sbf.Append('@');
    sbf.Append(cname);
  }

  // append key srcpos
  char fname[1024];
  if (_key_spos->size() == 0) {
    // Set_key_srcpos() not called, pickup the first data in root treenode
    // to generate the key
    SRCPOS_NODE first = First_spos();
    Is_True(first.Dna() != NULL, ("srcpos node dna is NULL"));
    DNA_NODE *dna = first.Dna();
    if (dna != NULL) ipsamgr = dna->Comp_unit()->Vsa()->Ipsa();
    SRCPOS spos = first.Transpos();
    Is_True(dna != NULL && spos != 0, ("invalid dna or srcpos"));
    fname[0] = '\0';
    Get_Global_File_Name(SRCPOS_filenum(spos), fname, sizeof(fname));
    sbf.Format("@%s:%d", fname, SRCPOS_linenum(spos));

    // add last srcpos
    SRCPOS_NODE last = Last_spos();
    spos = last.Transpos();
    if (!last.equal(first) && spos != 0) {
      Is_True(last.Dna() != NULL, ("srcpos node dna is NULL"));
      fname[0] = '\0';
      Get_Global_File_Name(SRCPOS_filenum(spos), fname, sizeof(fname));
      sbf.Format("@%s:%d", fname, SRCPOS_linenum(spos));
    }
  }
  else {
    Is_True(_key_spos->size() == 1 || _key_spos->size() == 2,
            ("bad key spos size"));
    KEY_SRCPOS_VEC::const_iterator end = _key_spos->end();
    char *ptr = buf;
    DNA_NODE *dna = NULL;
    SRCPOS kpos = 0;
    for (KEY_SRCPOS_VEC::const_iterator it = _key_spos->begin();
         it != end; ++it) {
      Is_True(it->_dna != NULL && it->_spos != 0, ("invalid dna or srcpos"));
      //doesn't skip same linenum in key so that source/sink can be paired correctly
      //if (it->_dna == dna && it->_spos == kpos)
      //  continue;
      dna = it->_dna;
      if (ipsamgr == NULL && dna != NULL)
        ipsamgr = dna->Comp_unit()->Vsa()->Ipsa();
      kpos = it->_spos;
      SRCPOS spos = SRCPOS_NODE::Transpos(kpos, dna->File_idx());
      // old implementation, use function name and delta from function entry
#if 0
      INLCXT   *cxt = it->_inlcxt;
      const char *fname;
      SRCPOS entry;
      if (cxt == NULL) {
        entry = WN_Get_Linenum(dna->Comp_unit()->Input_tree());
        fname = dna->Fname();
      }
      else if (cxt == INVALID_INLCXT) {
        entry = 0;
        fname = dna->Fname();
      }
      else {
        ST* st = St_ptr(dna->File_idx(), cxt->Inlcxt_call_st());
        entry = ST_Srcpos(*st);
        fname = Str_ptr(dna->File_idx(), ST_name_idx(st));
      }
      sbf.Format("@%s@%x", fname,
                 SRCPOS_linenum(spos) - SRCPOS_linenum(entry));
#endif
      // new implementation, use file name and line number
      fname[0] = '\0';
      Get_Global_File_Name(SRCPOS_filenum(spos), fname, sizeof(fname));
      sbf.Format("@%s:%d", fname, SRCPOS_linenum(spos));
    }
  }
  if (ipsamgr != NULL & sbf.To_string() != NULL)
    keyid = ipsamgr->Put_isugrp_id(sbf.To_string());
    
  return sbf.To_string();
}

// =============================================================================
// SRCPOS_HANDLE::Set_msgid
// =============================================================================
void
SRCPOS_HANDLE::Set_msgid(const char* msgid)
{
  INT len = strlen(msgid);
  Is_True(msgid != NULL && len > 1 && len < 12 &&
          msgid[0] != '$' && msgid[1] != '{' && msgid[len-1] != '}',
          ("invalid msg id"));
  _msgid = (char*)MEM_POOL_Alloc(_mem_pool, len + 4);
  snprintf(_msgid, len+4, "${%s}", msgid);
}

// =============================================================================
// SRCPOS_HANDLE::Add_message
// =============================================================================
void
SRCPOS_HANDLE::Add_message(const char* fmt, ...)
{
  char buf[512];
  va_list ap;
  va_start(ap, fmt);
  INT size = vsnprintf(buf, sizeof(buf), fmt, ap);
  va_end(ap);
  _message = (char*)MEM_POOL_Alloc(_mem_pool, size+1);
  if (size >= sizeof(buf)) {
    va_start(ap, fmt);
    vsnprintf(_message, size+1, fmt, ap);
    va_end(ap);
  }
  else {
    memcpy(_message, buf, size+1);
  }
}

INT
SRCPOS_HANDLE::Push_value_graph()
{
  if (!VSA_Value_Graph)
    return 0;
  Is_True_Ret(_graph != NULL, ("no value graph"), 0);
  return _graph->Push_mark();
}

void
SRCPOS_HANDLE::Pop_value_graph(INT level)
{
  if (!VSA_Value_Graph)
    return;
  Is_True_Ret(_graph != NULL, ("no value graph"));
  _graph->Pop_mark(level);
}

BOOL
SRCPOS_HANDLE::Eval_value_graph()
{
  if (!VSA_Value_Graph)
    return TRUE;
  Is_True_Ret(_graph != NULL, ("no value graph"), TRUE);
  BOOL maybe = FALSE;
  OP_RESULT res = _graph->Eval_graph(maybe);
  if (maybe) {
    Set_flag(SRCPOS_FLAG_MAYBE);
  }
  return (res != OP_CONFLICT) ? TRUE : FALSE;
}

BOOL
SRCPOS_HANDLE::Add_assign(VSA* lvsa, CODEREP *lhs, VSA *rvsa, CODEREP *rhs)
{
  if (!VSA_Value_Graph)
    return TRUE;
  Is_True_Ret(_graph != NULL, ("no value graph"), TRUE);
  OP_RESULT res = _graph->Add_assign(lvsa, lhs, rvsa, rhs);
  // TODO: PROVEN or VIOLATION?
  return (res != OP_CONFLICT) ? TRUE : FALSE;
}

BOOL
SRCPOS_HANDLE::Add_assign(VSA* lvsa, VSYM_OBJ_REP *lhs, VSA *rvsa, VSYM_OBJ_REP *rhs)
{
  if (!VSA_Value_Graph)
    return TRUE;
  Is_True_Ret(_graph != NULL, ("no value graph"), TRUE);
  OP_RESULT res = _graph->Add_assign(lvsa, lhs, rvsa, rhs);
  // TODO: PROVEN or VIOLATION?
  return (res != OP_CONFLICT) ? TRUE : FALSE;
}

BOOL
SRCPOS_HANDLE::Add_control_dependency(DNA_NODE *dna, BB_NODE *pred, BB_NODE *succ)
{
  if (!VSA_Value_Graph)
    return TRUE;
  Is_True_Ret(_graph != NULL, ("no value graph"), TRUE);
  Is_True(pred->Dominates(succ), ("pred does not dom succ"));
  OP_RESULT res = _graph->Add_control_dependency(dna, pred, succ);
  // TODO: PROVEN or VIOLATION?
  return (res != OP_CONFLICT) ? TRUE : FALSE;
}

BOOL
SRCPOS_HANDLE::Add_phi_opnd(DNA_NODE *dna, PHI_NODE *phi, INT32 opnd_idx)
{
  if (!VSA_Value_Graph) {
    return TRUE;
  }
  Is_True_Ret(_graph != NULL, ("no value graph"), TRUE);
  BOOL maybe = FALSE;
  OP_RESULT res = _graph->Add_phi_opnd(dna, phi, opnd_idx, maybe);
  if (maybe) {
    Set_flag(SRCPOS_FLAG_MAYBE);
  }
  return (res != OP_CONFLICT) ? TRUE : FALSE;
}

void
SRCPOS_HANDLE::Print(FILE *fp)  const
{
  _root->Print(0, fp);
}

// =============================================================================
//
// Print function for tracing Heap Object lifecycle
//
// =============================================================================
void
HO_LIST::Print(FILE *fp) 
{
  HEAP_OBJ    *heap_obj;
  HO_LIST_ITER ho_list_iter;
  FOR_ALL_ELEM (heap_obj, ho_list_iter, Init(this)) {
    heap_obj->Print(fp);
    HEAP_OBJ_REP* hor = heap_obj->Entry_chi();
    while (hor != NULL) {
      fprintf(fp, "  ");
      hor->Print_detail(fp);
      hor = hor->Next();
    }
  }
}

// =============================================================================
//
// HO_LIST::Count returns the number of elements in this HO_LIST
//
// =============================================================================
INT
HO_LIST::Count(void)
{
  HEAP_OBJ    *heap_obj;
  HO_LIST_ITER ho_list_iter;
  INT count = 0;
  FOR_ALL_ELEM (heap_obj, ho_list_iter, Init(this)) {
    ++count;
  }
  return count;
}

// =============================================================================
//
// HO_LIST::Find returns the heap_obj that has the symid
//
// =============================================================================
HEAP_OBJ*
HO_LIST::Find(IDTYPE symid, BOOL is_lda)
{
  HEAP_OBJ    *heap_obj;
  HO_LIST_ITER ho_list_iter;
  FOR_ALL_ELEM (heap_obj, ho_list_iter, Init(this)) {
    if (heap_obj->Sym_id() == symid) {
      heap_obj = heap_obj->Get_based_on();
      BOOL is_ho_lda = (heap_obj->Kind() == RSC_KIND_LDA);
      if (is_lda == is_ho_lda)
        return heap_obj;
    }
  }
  return NULL;
}

// =============================================================================
//
// HO_LIST::Find returns the heap_obj that has the sym
//
// =============================================================================
HEAP_OBJ*
HO_LIST::Find(VSA *vsa, AUX_STAB_ENTRY *sym, BOOL is_lda)
{
  HEAP_OBJ    *heap_obj;
  HO_LIST_ITER ho_list_iter;
  FOR_ALL_ELEM (heap_obj, ho_list_iter, Init(this)) {
    IDTYPE auxid = heap_obj->Sym_id();
    AUX_STAB_ENTRY *s = vsa->Opt_stab()->Aux_stab_entry(auxid);
    if (s->Base() == sym->Base()) {
      heap_obj = heap_obj->Get_based_on();
      BOOL is_ho_lda = (heap_obj->Kind() == RSC_KIND_LDA);
      if (is_lda == is_ho_lda)
        return heap_obj;
    }
  }
  return NULL;
}

// =============================================================================
//
// Print function for tracing Heap Object lifecycle
//
// =============================================================================
void
HOR_LIST::Print(FILE *fp) 
{
  HEAP_OBJ_REP *heap_obj_rep;
  HOR_LIST_ITER hor_list_iter;
  fprintf(fp, "<ULIST>{ ");
  FOR_ALL_ELEM (heap_obj_rep, hor_list_iter, Init(this)) {
    heap_obj_rep->Print(fp);
    fprintf(fp, " ");
  }
  fprintf(fp, "}\n");
}

// =============================================================================
//
// HO_LIST::Count returns the number of elements in this HO_LIST
//
// =============================================================================
INT
HOR_LIST::Count(void)
{
  HEAP_OBJ_REP *heap_obj_rep;
  HOR_LIST_ITER hor_list_iter;
  INT count = 0;
  FOR_ALL_ELEM (heap_obj_rep, hor_list_iter, Init(this)) {
    ++count;
  }
  return count;
}

// =============================================================================
//
// HOR_LIST::Find returns the heap_obj_rep that has the hor
//
// =============================================================================
HEAP_OBJ_REP*
HOR_LIST::Find(HEAP_OBJ_REP *hor) 
{
  HEAP_OBJ_REP *heap_obj_rep;
  HOR_LIST_ITER hor_list_iter;
  HEAP_OBJ* ho = hor->Heap_obj();
  FOR_ALL_ELEM (heap_obj_rep, hor_list_iter, Init(this)) {
    if (heap_obj_rep->Heap_obj() == ho) {
      return heap_obj_rep;
    }
  }
  return NULL;
}

// =============================================================================
//
// Print function for tracing Vsym Object lifecycle
//
// =============================================================================
void
VO_LIST::Print(const VSA *vsa, FILE *fp)
{
  VSYM_OBJ    *vsym_obj;
  VO_LIST_ITER vo_list_iter;
  FOR_ALL_ELEM (vsym_obj, vo_list_iter, Init(this)) {
    vsa->Print_vo(vsym_obj, fp);
    VSYM_OBJ_REP* vor = vsym_obj->Entry_chi();
    while (vor != NULL) {
      fprintf(fp, "  ");
      vor->Print_detail(fp);
      vor = vor->Next();
    }
    fprintf(fp, "\n");
  }
}

// =============================================================================
//
// vO_LIST::Count returns the number of elements in this VO_LIST
//
// =============================================================================
INT
VO_LIST::Count(void)
{
  VSYM_OBJ    *vsym_obj;
  VO_LIST_ITER vo_list_iter;
  INT count = 0;
  FOR_ALL_ELEM (vsym_obj, vo_list_iter, Init(this)) {
    ++count;
  }
  return count;
}

// =============================================================================
//
// VO_LIST::Find returns the vsym_obj that has the same heap_obj_rep && fld_id
//
// =============================================================================
VSYM_OBJ*
VO_LIST::Find(HEAP_OBJ_REP *hor, VSYM_FLD_REP *vfr)
{
  VSYM_OBJ    *vsym_obj;
  VO_LIST_ITER vo_list_iter;
  FOR_ALL_ELEM (vsym_obj, vo_list_iter, Init(this)) {
    if ((vsym_obj->Base_hor() == hor) && vsym_obj->Fld_rep().Find(vfr)) {
      return vsym_obj;
    }
  }
  return NULL;
}

// =============================================================================
// FIELD_OBJ_REP::Print
// =============================================================================
void
FIELD_OBJ_REP::Print(FILE* fp) const
{
  HEAP_OBJ_REP* fobj = Hor();
  fprintf(fp, "ho%dv%d:", fobj->Heap_obj()->Id(), fobj->Version());
  Fld_rep()->Print(fp);
}

// =============================================================================
// VSYM_TRACKER::Print
// =============================================================================
void
VSYM_TRACKER::Print(FILE *fp) const
{
  fprintf(fp, ("Vsym tracker:"));
  int i = 1;
  for (VSYM_VECTOR::const_reverse_iterator it = _vsym_info.rbegin();
       it != _vsym_info.rend(); ++ it) {
    fprintf(fp, "[L%d]", i++);
    (*it)->Print(fp);
    fprintf(fp, " ");
  }
  fprintf(fp, "\n");
}



// =============================================================================
//
// Merge content into 'this' from 'from'
//
// =============================================================================
void
VSA::Merge(HEAP_OBJ *to, HEAP_OBJ *from)
{
  if(to == from) return;

  BB_NODE     *bbx;
  BB_LIST_ITER bb_list_iter;
  FOR_ALL_ELEM (bbx, bb_list_iter, Init(to->Def_bbs())) {
    from->Prepend_def_bbs(bbx, Mem_pool());
  }
}

// =============================================================================
//
// Convert hor to use the heap_obj as its heap_obj
//
// =============================================================================
void
VSA::Merge(HEAP_OBJ *heap_obj, HEAP_OBJ_REP *hor)
{
  Merge(heap_obj, hor->Heap_obj());
  // hor->Set_heap_obj(heap_obj);
}

// =============================================================================
//
// Replace CK_VAR or CK_IVAR with their constant initialized value.
// x is  CK_VAR st  or
//       CK_IVAR CK_LDA st
// and var_aux_id is corr opt_stab entry for the st.
//
// =============================================================================
CODEREP *
VSA::Udt_const_init_scalar(CODEREP *x, AUX_ID var_aux_id)
{
#ifdef Is_True_On
  if (x->Kind() == CK_VAR) {
    Is_True(var_aux_id == x->Aux_id(),
            ("VSA::Udt_const_init_scalar: wrong aux_id."));
  } else if (x->Kind() == CK_IVAR) {
    Is_True(x->Offset() == 0, 
            ("VSA::Udt_const_init_scalar: wrong aux_id."));
  } else {
    Is_True(FALSE, ("VSA::Udt_const_init_scalar: wrong tree."));
  }
#endif

  AUX_STAB_ENTRY *psym = Opt_stab()->Aux_stab_entry(var_aux_id);

  // ignore vector type so far
  if ( MTYPE_is_vector(x->Dsctyp()) ||
       (psym->St() && MTYPE_is_vector(ST_mtype(psym->St()))) ) {
    Warn_todo("Udt_const_init_scalar: handle vector type.");
    return NULL;
  }

  // is this variable a constant initialized scalar?
  BOOL const_initialized = 
    psym->Is_flag_const_init() &&
#if defined(TARG_NVISA)
    !psym->Is_volatile();
#else
    !psym->Is_volatile() &&
    psym->St_ofst() == 0;  // a limitation for matching INITV or TCON
                           // should make it smarter to look into block of INITV.
#endif

  if (const_initialized) {
    TCON init_tcon;
    ST *st = psym->St();
    if (ST_is_const_initialized_scalar(st, psym->St_ofst(), init_tcon))
      {
        // if the ST's initialized value can be represented with a TCON
        // first convert the init value to the type of the variable
        if ( x->Dsctyp() != TCON_ty(init_tcon) ) {
          // init_tcon = Targ_Conv( x->Dsctyp(), init_tcon );
          // Do not use Targ_Conv.  Consider when the INITO is a
          // structure type and x is a member of the structure.
          Warn_todo("Udt_const_init_scalar: should copy the bits instead of targ_conv.");
          return NULL;
        }
        // then convert it to the type of the result
        if ( Get_Trace(TP_WOPT2, VSA_DUMP_FLAG)) {
          fprintf(TFile, "Udt_const_init_scalar:  replacing LDID/ILOAD-LDA aux %d with TCON\n",
                  var_aux_id);
        }
        if ( x->Dtyp() != TCON_ty(init_tcon) )
          init_tcon = Targ_Conv(x->Dtyp(), init_tcon);
        TCON_IDX tcon_idx = Enter_tcon(init_tcon);
        return Htable()->Add_tcon(tcon_idx);
      }
    else {
      if (INITV_IDX initv = ST_is_const_and_has_initv(st)) {
        // if the ST's initialized value is a INITV (e.g. SYMOFF)
        if (MTYPE_size_min(x->Dsctyp()) == MTYPE_size_min(Pointer_type) &&
            MTYPE_size_min(x->Dtyp()) == MTYPE_size_min(Pointer_type) &&
            INITV_kind(Initv_Table[initv]) == INITVKIND_SYMOFF &&
            ST_class(&St_Table[INITV_st(Initv_Table[initv])]) == CLASS_VAR) {
          AUX_ID aux_id =
            Opt_stab()->Find_sym_with_st_and_ofst(&St_Table[INITV_st(Initv_Table[initv])],
                                                  INITV_ofst(Initv_Table[initv]));
          Is_True(aux_id != ILLEGAL_AUX_ID, ("VSA::Udt_const_init_scalar: can't find opt_stab entry."));
          if (Get_Trace(TP_WOPT2, VSA_DUMP_FLAG)) {
            fprintf(TFile, "Udt_const_init_scalar:  replacing LDID/ILOAD-LDA aux %d with LDA %d\n", 
                    var_aux_id,  aux_id);
          }
          if (aux_id == ILLEGAL_AUX_ID)
            return NULL;
          AUX_STAB_ENTRY *psym2 = Opt_stab()->Aux_stab_entry(aux_id);
          CODEREP  *cr = Alloc_stack_cr(0);
          TY_IDX ptr_ty = Make_Pointer_Type(ST_type(psym2->St()));
          cr->Init_lda(Pointer_type,
                       aux_id,
                       psym2->St_ofst(),     // redundant info
                       ptr_ty,               // redundant info
                       psym2->St());         // redundant info
          return Htable()->Rehash(cr, FALSE);
        }
      }
    }
  }
  return NULL;
}


#define INVALID_SRCPOS (-1)

// =============================================================================
//
//  This is a helper function to find linenum for a BB
//  we lookup linenum from the following 2 positions in 
//  1. the linenum of the BB
//  2. the first stmt linenum in this BB if exist
//  3. the linenum of fall-through succ
//  4. the last stmt linenum in pred 
//
// =============================================================================
SRCPOS
VSA::Get_bb_linenum(BB_NODE *bb)
{
  SRCPOS linenum = bb->Linenum();
  if (linenum == 0 && bb->First_stmtrep()) {
    linenum = bb->First_stmtrep()->Linenum();
  }

  if (linenum == 0) {
    // found in fall-through succ
    if(bb->Next() && CFG::Fall_through(bb, bb->Next()))
      linenum = bb->Next()->Linenum();
  }

  if (linenum == 0 ) {
    // found in fall-through pred
    if(bb->Prev() && CFG::Fall_through(bb->Prev(), bb)
       && bb->Prev()->Last_stmtrep())
      linenum = bb->Prev()->Last_stmtrep()->Linenum();
  }
  return linenum;
}

#if 0
// =============================================================================
//
//  INLCXT::Rootlvl_inl_line_num - returns the line number where callee makes
//  the call
//
// =============================================================================
INT64 
INLCXT::Rootlvl_inl_line_num(void) const
{
  INT64 retval;
  INLCXT *parent_context = this->Parent();
  INLCXT *top_level_context;
  if (parent_context == NULL) { // single level inline
    retval = this->Inlcxt_line_num();
  } else { // multiple level inline
    while (parent_context != NULL) { // need to back track to the top level call instance context
      top_level_context = parent_context;
      parent_context = parent_context->Parent();
    }
    retval = top_level_context->Inlcxt_line_num();
  } // inline level check
  return retval;
}
#endif

// =============================================================================
//
//  VS_FLD_KIND VSA::Get_vfr_kind(CODEREP *cr) const
//     VS_FLD_KIND is set ANY by default & conditionally set to ZERO in LDA
//
// =============================================================================
VS_FLD_KIND
VSA::Get_vfr_kind(CODEREP *cr) const
{
  VS_FLD_KIND ret = FLD_K_ANY;
  if (cr != NULL && cr->Kind() == CK_LDA) {
    ST *st = cr->Lda_base_st();
    Is_True_Ret(st, ("null lda base st"), ret);
    TY_IDX ty = ST_type(st);
    if (TY_kind(ty) == KIND_SCALAR) {
      ret = FLD_K_ID;
    }
  }
  return ret;
}


// =============================================================================
// BOOL VSA::ST_initialized_zero(ST* st)
// Check if the global st is initialized to zero
// =============================================================================
BOOL
VSA::ST_initialized_zero(ST* st, INT ofst)
{
  if (ST_init_value_zero(st))
    return TRUE;
  UINT32 file_idx = Dna()->File_idx();
  ST_IDX st_idx = ST_st_idx(st);
  switch (ST_sclass(st)) {
  case SCLASS_EXTERN:
    if (!IPSA_insession() || Dna()->File_idx() == 0)
      return FALSE;  // ignore this case
    // fall through
  case SCLASS_COMMON:
    if (!IPSA_insession() || Dna()->File_idx() == 0)
      return TRUE;
    WHIRL_FILE_MANAGER::Get()->Resolve(file_idx, st_idx, file_idx, st_idx);
    st = St_ptr(file_idx, st_idx);
    // fall through
  case SCLASS_PSTATIC:
  case SCLASS_FSTATIC:
    if (!ST_is_initialized(st))
      return TRUE;
    // fall through
  case SCLASS_DGLOBAL:
    if (ST_init_value_zero(st))
      return TRUE;
    {
      const INITO* inito = ST_inito(file_idx, st_idx);
      return Is_initv_zero(file_idx, inito->val, ofst);
    }
    break;
  case SCLASS_UGLOBAL:
    return TRUE;
  default:
    Is_True(FALSE, ("wrong st sclass"));
    break;
  }
  return FALSE;
}

// =============================================================================
//
//  INLCXT::Print - Print the inline context
//
// =============================================================================
void 
INLCXT::Print(FILE* fp) const
{
  const INLCXT*  cxt = this;
  while(cxt) {
    fprintf(fp, "%d:%d,", SRCPOS_filenum(_inlcxt_line_num), SRCPOS_linenum(_inlcxt_line_num));
    cxt = cxt->Parent();
  }
}

char *
VSA::Sym_name(IDTYPE id) const
{
  // TODO: We need to expand this routine to handle struct's field access.
  //       Good reference: osprey/be/whirl2c/wn2c.cxx/WN2C_ldid, line 6085
  AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(id);
  return Vsa_sym_name(sym);
}

BOOL
VSA::Is_legit_error(IDTYPE id) const
{
  AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(id);
  ST   *st = sym->St();

  // additional filter
  switch (ST_sclass(st)) {
  case SCLASS_AUTO:
    return TRUE;

  case SCLASS_FORMAL:
  case SCLASS_FORMAL_REF:
    // VSA is turning on IPA level info, info for testgen
    return TRUE;
    break; 

  case SCLASS_FSTATIC:
  case SCLASS_UGLOBAL:
  case SCLASS_DGLOBAL:
    break; 

  default: // examine common/com/symtab_defs.h for other SCLASS
    break;
  }

  if (sym->Is_preg()) return TRUE;

  return FALSE;
}


#if 0
// To be removed later
// =============================================================================
//
//  VSA:Classify_npd_error - nullpointer deref error reporting 
//
// =============================================================================
void
VSA::Classify_npd_error(CODEREP *x, STMTREP *stmt)
{
  VAL_RANGE_RESULT rr;
  BOOL is_vra = FALSE;
  if (x->Kind() == CK_CONST && x->Const_val() == 0) {
    SRCPOS_HANDLE srcpos_h(x, stmt, Comp_unit()->Dna(), Loc_pool());
    const char* name = srcpos_h.Orig_stname();
    if (name != NULL) {
      Report_vsa_error(x, name, NPD, EC_VSA_Nullptr_deref, &srcpos_h);
    }
    else {
      Is_Trace(Tracing(),
               (TFile, ("VSA-NPD: No symbol for NPD on constant.\n")));
      Report_vsa_error(x, (AUX_ID)0, NPD, EC_VSA_Npd_noname_deref, &srcpos_h);
    }
  }
  else if ((rr = Check_expr_zero(x, stmt->Bb(), is_vra)) != VAL_INR) {
    SRCPOS_HANDLE srcpos_h(x, stmt, Comp_unit()->Dna(), Loc_pool());
    if (x->Kind() == CK_VAR) {
      hash_set<IDTYPE> visited_bb;
      Classify_npd_error(x, stmt->Bb(), &srcpos_h, rr, is_vra, visited_bb);
    }
    else {
      const char* name = srcpos_h.Orig_stname();
      if (name != NULL) {
        Report_vsa_error(x, name, NPD, EC_VSA_Nullptr_deref, &srcpos_h);
      }
      else {
        Is_Trace(Tracing(),
                 (TFile, ("VSA-NPD: No symbol for NPD on constant.\n")));
        Report_vsa_error(x, (AUX_ID)0, NPD, EC_VSA_Npd_noname_deref, &srcpos_h);
      }
    }
  }
}

void
VSA::Classify_npd_error(CODEREP *x, BB_NODE *curbb, SRCPOS_HANDLE *srcpos_h,
                        VAL_RANGE_RESULT rr, BOOL vra, hash_set<IDTYPE> &visited_bb)
{

  Is_True(x != NULL, ("VSA::Classify_npd_error: null var"));
  // skip these two VSYMs
  if (x->Aux_id() == Opt_stab()->Default_vsym() ||
      x->Aux_id() == Opt_stab()->Return_vsym()) 
    return;
  
  if ( x->Is_flag_set(CF_IS_ZERO_VERSION) ) {
    return;
  }
  AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(x->Aux_id());
  ST   *st = sym->St();
  char *output_var_name = Sym_name(x->Aux_id());
  UINT32 anat = NPD;

  // if x is def by phi, check if all phi's opnd is zero and
  // has a possible path to the use
  if (Comp_unit()->Vra() && x->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE* phi = x->Defphi();
    BOOL is_npd = FALSE;
    for (INT i = 0; i < phi->Size(); ++i) {
      CODEREP* opnd = phi->OPND(i);
      BOOL is_vra = FALSE;
      // check if opnd can be zero
      if (Check_var_zero(opnd, curbb, is_vra) != VAL_INR) {
        // check if there is a path from opnd def to use
        VRA_RESULT ret = Comp_unit()->Vra()->Is_path_possible(curbb, phi, i);
        if (ret != VA_NO) {
          is_npd = TRUE;
          break;
        }
      }
    }
    if (is_npd == FALSE)
      return;
  }

  if (vra && !x->Value_invalid_addr() && !x->Value_maydangling()) {
    // the result comes from value range analysis so x may not be zreo
    // report error directly
    if (rr != VAL_OOR)
      return;
    Report_vsa_error(x, x->Aux_id(), anat, EC_VSA_Nullptr_deref , srcpos_h);
    return;
  }

  if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI)) || x->Value_invalid_addr()) {
    // test for entry_chi -> VSA UIV
    if (x->Def_at_entry() && st !=NULL /* vsym does not have st entry e.g. return vsym */) {
      BB_NODE* defbb = cr->Defstmt() ? cr->Defstmt()->Bb() : Cfg()->Entry_bb();
      srcpos_h->Append_data(st, defbb, Comp_unit()->Dna(), PATHINFO_ST_DECLARE);

      switch (ST_sclass(st)) {
      case SCLASS_AUTO:
        //Append_cd_linenum(x, curbb, srcpos_h);
        Report_vsa_error(x, x->Aux_id(), anat, EC_VSA_Nullptr_deref, srcpos_h);
        break;
      case SCLASS_FORMAL:
      case SCLASS_FORMAL_REF:
        if (IPSA_insession()) {
          // call the following function for all its caller
          Check_callers_argument_for_uiv(x, srcpos_h, FOR_ISTOR_BASE);
        }
        break;
      case SCLASS_FSTATIC:
      case SCLASS_DGLOBAL:
      case SCLASS_COMMON:
        if (!ST_initialized_zero(st, x->Offset))
          break;
        if (Comp_unit()->Dna()->Report_data_npd() && VSA_Global_Var_Zero && !vra)
          Report_vsa_error(x, x->Aux_id(), anat, EC_VSA_Nullptr_may_deref, srcpos_h);
        break;
      case SCLASS_UGLOBAL:
      case SCLASS_EXTERN:
        // Need cross-file IPA and whole-program mode to detect static/global/extern/etc
        // (will handle specific init later (e.g. legit mem map I/O)
        if (!ST_initialized_zero(st, x->Offset))
          break;
        if (Comp_unit()->Dna()->Report_data_npd() && VSA_Global_Var_Zero && !vra)
          Report_vsa_error(x, x->Aux_id(), anat, EC_VSA_Nullptr_may_deref, srcpos_h);        
        break;

      default: // examine common/com/symtab_defs.h for other SCLASS
        // we assume static etc are initialized to 0
        // (will handle specific init later (e.g. legit mem map I/O)
        if (0) {
          // Need to turn on when we have cross file driven by linker
          Report_vsa_error(x, (AUX_ID)0, anat, EC_VSA_Nullptr_may_deref, srcpos_h);
        }
        break;

      }
      return;
    } // end of entry_chi

    STMTREP *defstmt = x->Defstmt();
    if (defstmt) {
      if (x->Value_invalid_addr()) {
        srcpos_h->Append_data(defstmt, x, Comp_unit()->Dna(), PATHINFO_COPY);

        Report_vsa_error(x, x->Aux_id(), anat, EC_VSA_Nullptr_may_deref, srcpos_h);        
        return;
      }
    
      if (!IPSA_insession() && ! Load_from_subset_memory_area(defstmt, x)) {
        // we also check other cases that is not defined by call
        Report_vsa_error(x, x->Aux_id(), anat, EC_VSA_Nullptr_deref, srcpos_h);
      }
      else if (!IPSA_insession()) {
        // if not entry_chi, conservatively assumes they are defined.
      } else {
        // in ipsa mode,
        if (OPERATOR_is_call(defstmt->Opr())) {
          // defined by call, check the rna info if the variable is modified in callee
          // if it is passed to callee with LDA operation.  Set value def if modified,
          // otherwise, inherit the state of the chi operand by calling Vsa_var.
          RNA_NODE *rna = Sr_2_rna(defstmt);
          if (rna != NULL) {
            IDTYPE arg_seq = rna->Get_arg_with_lda(x->Aux_id());
            if (arg_seq != INVALID_VAR_IDX &&
                ! rna->Is_set_arg_flag(arg_seq,REF_ISTORE)) {
              CHI_NODE *defchi = x->Defchi();
              srcpos_h->Append_data(defstmt, PATHINFO_CALL_CHI);
              Classify_npd_error(defchi->OPND(), curbb, srcpos_h, rr, FALSE, visited_bb);
            }
          }
        }
      }
      return;
    } // defstmt != NULL
  }

  if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI)) ) {
    PHI_NODE     *phi = x->Defphi();
    
    if (visited_bb.find(phi->Bb()->Id()) != visited_bb.end())
      return;
    visited_bb.insert(phi->Bb()->Id());

    if ( x->Value_checked() && (x->Value_invalid_addr() || x->Value_maydangling())) {

      // check all its operands since it has not been checked
      // we need to fork the srcpos_tree, if there is more than on operands is
      // maydef or not_def
      PHI_OPND_ITER phi_opnd_iter(phi);
      CODEREP      *opnd;
      INT           def_cnt = 0;

      FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
        if (opnd->Value_def())
          def_cnt++;
      }

      srcpos_h->Path()->Push_mark(phi);
      if (def_cnt > 1) {
        // fork the srcpos_h by creating children
        srcpos_h->Append_data(phi->Bb(), Comp_unit()->Dna(), PATHINFO_PHI);
        SRCPOS_TREENODE *cur_treenode = srcpos_h->Cur_node();
        cur_treenode->Add_children(def_cnt);
        IDTYPE i = 0;
        BB_NODE       *bb_pred;
        BB_LIST_ITER   bb_iter;
        INT32          which_pred = 0;

        FOR_ALL_ELEM(bb_pred, bb_iter, Init(phi->Bb()->Pred())) {
          opnd = phi->OPND(which_pred);
          ++which_pred;
          if ((opnd->Value_invalid_addr() || opnd->Value_maydangling())) { // append only the no_def or maydef operand
            srcpos_h->Set_cur_node(cur_treenode, i);
            
            srcpos_h->Append_data(bb_pred, Dna(), PATHINFO_BRANCH);
            srcpos_h->Path()->Add_bb(bb_pred);
            Classify_npd_error(opnd, bb_pred, srcpos_h, rr, FALSE, visited_bb);
            ++i;
          }
          srcpos_h->Path()->Pop_mark(phi, FALSE);
        }
      } else {
        BB_NODE       *bb_pred1;
        BB_LIST_ITER   bb_iter1;
        INT32          which_pred1 = 0;

        FOR_ALL_ELEM(bb_pred1, bb_iter1, Init(phi->Bb()->Pred())) {
          opnd = phi->OPND(which_pred1);
          ++which_pred1;
          if ((opnd->Value_invalid_addr() || opnd->Value_maydangling())) { // append only the no_def or maydef operand
            srcpos_h->Append_data(bb_pred1, Dna(), PATHINFO_BRANCH);
            Classify_npd_error(opnd, bb_pred1, srcpos_h, rr, FALSE, visited_bb);
          }
          srcpos_h->Path()->Pop_mark(phi, FALSE);
        }
      } // handling for forking or not
      srcpos_h->Path()->Pop_mark(phi, TRUE);
    } // x->Value_maydef() is TRUE
  }

  return;   
}
#endif

#if 0
// To be removed later
// =============================================================================
//
//  VSA:Report_vsa_error - unitialized reference error reporting 
//
// =============================================================================
void
VSA::Report_vsa_error(CODEREP *x, UINT32 anat, INT ecode, SRCPOS_HANDLE *srcpos_h)
{
  AUX_ID auxid = 0;
  if (!noname)
    auxid = x->Aux_id();
    Report_vsa_error(auxid, x, bb, anat, ecode, noname, srcpos_h);
  return;
}
#endif

void
VSA::Setup_sr_rna_map(DNA_NODE *dna)
{
  Is_True(dna == Comp_unit()->Dna(), ("dna mismatch"));
  for (INT i = VAR_INIT_ID; i < dna->Call_list()->size(); ++i) {
    RNA_NODE *rna = (*dna->Call_list())[i];
    Is_True(rna && rna->Callstmt(), ("rna or callstmt invalid"));
    Enter_sr_rna_map(rna->Callstmt(), rna);
  }
}

void
VSA::Print_sr_2_rna_map(FILE *fp)
{
  RNANODE_MAP_ITER iter(_sr_2_rna_map);
  for (; !iter.Is_end(); iter.Next()) {
    fprintf(fp, "key : sr%d, node : rna%d\n", iter.Key(), iter.Node()->Rna_idx());
  }
}

BOOL
VSA::Report_vsa_error(CODEREP *x, AUX_ID auxid, UINT32 anat, ISSUE_CERTAINTY ic, SRCPOS_HANDLE *srcpos_h) const
{
  const char *output_var_name = NULL;
  const char* orig_name = srcpos_h->Orig_stname();
  if(orig_name != NULL)
    output_var_name = orig_name;
  else if (auxid != 0)
    output_var_name = Sym_name(auxid);
  else if (x == NULL)
    output_var_name = "";
  return Report_vsa_error(x, output_var_name, anat, ic, srcpos_h);
}

BOOL
VSA::Report_vsa_error(CODEREP *x, const char *output_var_name, UINT32 anat, ISSUE_CERTAINTY ic, SRCPOS_HANDLE *srcpos_h) const
{
  char  ilod_name_buf[ISKEY_MAX_KEY_LEN];
  if(!output_var_name || output_var_name[0] == '.') {
    const char* orig_name = srcpos_h->Orig_stname();
    if(orig_name != NULL)
      output_var_name = orig_name;
#if 0
    else if(x){
      if (x->Kind() == CK_VAR)
        output_var_name = Sym_name(x->Aux_id());
      else if (x->Kind() == CK_LDA)
        output_var_name = Sym_name(x->Lda_aux_id());
      else if (x->Kind() == CK_IVAR && x->Opr() == OPR_ILOAD) {
        CODEREP *ilodbase = x->Ilod_base() ? x->Ilod_base() : x->Istr_base();
        if (ilodbase->Kind() != CK_VAR)
          ilodbase = Find_base_pointer_load(ilodbase);
        Is_True(ilodbase != NULL,
                ("Not find ilodbase"));
        if (ilodbase && ilodbase->Kind() == CK_VAR)
          snprintf(ilod_name_buf, ISKEY_MAX_KEY_LEN, "*(%s+%d)",
                  Sym_name(ilodbase->Aux_id()), x->Offset());
        else if (ilodbase && ilodbase->Kind() == CK_LDA)
          snprintf(ilod_name_buf, ISKEY_MAX_KEY_LEN, "*(&%s+%d)",
                  Sym_name(ilodbase->Lda_aux_id()), x->Offset());
        else
          snprintf(ilod_name_buf, ISKEY_MAX_KEY_LEN, "*($wrong$)");
        output_var_name = ilod_name_buf;
      }
    }
#endif
  }

  // do not warn on variables in system libraries
  if (output_var_name != NULL &&
      output_var_name[0] == '_' &&
      output_var_name[1] == '_')
    return FALSE;
  
  if (VSA_Warn_On_This == FALSE &&
      output_var_name != NULL &&
      (strcmp(output_var_name, ("this")) == 0 ||
       strncmp(output_var_name, ".material", 9) == 0 ||
       strcmp(output_var_name, (".cxx.bind.")) == 0 ||
       strncmp(output_var_name, ".init", 5) == 0))
    return FALSE;

  if(!output_var_name || 
     (output_var_name &&  output_var_name[0] == '.')) {
    output_var_name = srcpos_h->Find_cr_stname(x, NULL, Dna(), TRUE, TRUE, anat == NPD);
    if (output_var_name == NULL)
      output_var_name = "$nosymbol";
  }

  if (anat != DBF && anat != MSF && Vsa_check_sym_ignore(output_var_name))
    return FALSE;

  // ignore UIV/NPD without variable name
  if ((anat == UIV || anat == NPD) &&
      (output_var_name == NULL || output_var_name[0] == '$'))
    return FALSE;

  const char * ana_name = Get_builtin_rule_code(anat);
  UINT32 fix_cost = Get_builtin_rule_fix_cost(anat);
  return Report_vsa_error(x, output_var_name, ana_name, fix_cost, ic, srcpos_h);
}

BOOL
VSA::Report_vsa_error(CODEREP *x, const char *output_var_name, const char *ana_name, INT32 fix_cost,
                      ISSUE_CERTAINTY ic, SRCPOS_HANDLE *srcpos_h) const
{
  BOOL is_interproc = srcpos_h->Context() != NULL;
  SRCPOS spos = srcpos_h->Root_linenum();
  if (spos == 0) {
    Is_Trace(Tracing(), (TFile, "#### HIT SRCPOS == 0, place a bp at "));
    Is_Trace(Tracing(), (TFile, "%s:%d to debug this issue ####\n", __FILE__, __LINE__));
    return FALSE;
  }

  if (srcpos_h->Eval_value_graph() == FALSE) {
    Is_Trace(Tracing(), (TFile, "--ignore this issue: value graph evals to false.\n"));
    return FALSE;
  }

  SRCPOS entry_pos = WN_Get_Linenum(Comp_unit()->Input_tree());
  char key_buf[ISKEY_MAX_KEY_LEN];
  IDTYPE keyid;
  const char* key = srcpos_h->Path_to_key(key_buf, sizeof(key_buf),
                                          output_var_name, ana_name, NULL, keyid);
  if (ic == IC_DEFINITELY && srcpos_h->Is_flag_set(SRCPOS_FLAG_MAYBE))
    ic = IC_MAYBE;
  const char *output_pu_name = srcpos_h->Orig_puname();
  if (output_pu_name == NULL)
    output_pu_name = Cur_pu_name();
  VSA_ISSUE issue("BUILTIN", ana_name, key, keyid,
                  output_var_name, output_pu_name,
                  spos, ic, srcpos_h);
  issue.Set_fix_cost(fix_cost);
  Vsa_error_print(&issue);
  return TRUE;
}

BOOL
VSA::Report_xsca_error(CODEREP *x, const char *output_var_name, const char *ana_name,
                      ISSUE_CERTAINTY ic, SRCPOS_HANDLE *srcpos_h) const
{
  SRCPOS spos = srcpos_h->Root_linenum();
  if (spos == 0) {
    Is_Trace(Tracing(), (TFile, "#### HIT SRCPOS == 0, place a bp at "));
    Is_Trace(Tracing(), (TFile, "%s:%d to debug this issue ####\n", __FILE__, __LINE__));
    return FALSE;
  }
  SRCPOS entry_pos = WN_Get_Linenum(Comp_unit()->Input_tree());
  char key_buf[ISKEY_MAX_KEY_LEN];
  IDTYPE keyid;
  const char* key = srcpos_h->Path_to_key(key_buf, sizeof(key_buf),
                                          output_var_name, ana_name, NULL, keyid);
  const char *output_pu_name = srcpos_h->Orig_puname();
  if (output_pu_name == NULL)
    output_pu_name = Cur_pu_name();
  if (ic == IC_DEFINITELY && srcpos_h->Is_flag_set(SRCPOS_FLAG_MAYBE))
    ic = IC_MAYBE;
  VSA_ISSUE issue("SML", ana_name, key, keyid,
                  output_var_name, output_pu_name,
                  spos, ic, srcpos_h);
  issue.Set_fix_cost(1);
  Vsa_error_print(&issue);
  return TRUE;
}

/*
void
VSA::Classify_npd_error_name(char *output_var_name, SRCPOS_HANDLE *srcpos_h, VAL_RANGE_RESULT rr)
{
  INT ecode;
  char *output_pu_name = Cur_pu_name();

  if ((output_var_name != NULL) && strcmp(output_var_name, "this") == 0) {
    // disable report of "this" pointer for now - TODO link this with proper var in RNA
    return;
  }
  
  switch (rr) {
  case VAL_OOR:
    ecode = EC_VSA_Nullptr_deref;
    break;

  case VAL_May_OOR:
    ecode = EC_VSA_Nullptr_may_deref;
    break;
    
  case VAL_UNK_R: // TODO - what to report?
    // fall through for now
  default: // this includes in range results
    // nothing need to be done
    return;
  }

  SRCPOS spos = 0;
  Vsa_error_print(output_var_name, Cur_pu_name(), "NPD", spos, ecode, "");
  return;
}
*/

void
VSA::Classify_asm_npd_error(STMTREP *stmt, BB_NODE *curbb)
{
  Is_True(stmt->Opr() == OPR_ASM_STMT, ("not asm stmt"));
  CODEREP* rhs = stmt->Rhs();
  Is_True(rhs != NULL && rhs->Opr() == OPR_ASM_STMT, ("not stmt op"));

  INT i;
  for (i = 0; i < rhs->Kid_count(); ++i) {
    CODEREP* opnd = rhs->Opnd(i);
    Is_True(opnd->Opr() == OPR_ASM_INPUT, ("not asm input"));
    ST_IDX st = opnd->Asm_constraint();
    Is_True(st != ST_IDX_ZERO && ST_name(st) != NULL, ("no constraint string"));

    CODEREP *x = Udt_cr(opnd->Opnd(0), curbb, stmt);
    if (x == NULL)
      continue;

    const char* cstr = ST_name(st);
    if (*cstr == 'p') {
      // we have an address opnd
      SRCPOS_HANDLE srcpos_h(opnd->Opnd(0), stmt, Comp_unit()->Dna(), Loc_pool());
      hash_set<IDTYPE> visited_bb;
      if (IPSA_insession())
        Ipsa()->Begin_trav_counter();
      CALL_STACK cs;
      Classify_vul_error(x, curbb, stmt, cs,
                         &srcpos_h, (ILODSTORBASE)(FOR_ILOD_BASE | FOR_NPD), visited_bb);
      Is_True(cs.empty(), ("call stack is not empty"));
      if (IPSA_insession())
        Ipsa()->End_trav_counter();
    }
    // TODO: write to a buffer?
  }
}


// =============================================================================
//
//  VSA::Place_ho_phi_node places phi node for heap_object
//
// =============================================================================
void
VSA::Place_ho_phi_node(void)
{
  PHILIST_MAP  *bb_ro_philist = Bb_ho_philist();
  HEAP_OBJ     *heap_obj;
  HO_LIST      *heap_obj_list = Heap_obj_list();
  HO_LIST_ITER  ho_list_iter;
  HEAP_OBJ_REP *hor;
  Place_ro_phi_node(bb_ro_philist, heap_obj, heap_obj_list, &ho_list_iter, hor);
}

// =============================================================================
//
//  VSA::Place_vo_phi_node places phi node for vsym_object
//
// =============================================================================
void
VSA::Place_vo_phi_node(void)
{
  PHILIST_MAP  *bb_ro_philist = Bb_vo_philist();
  VSYM_OBJ     *vsym_obj;
  VO_LIST      *vsym_obj_list = Vsym_obj_list();
  VO_LIST_ITER  vo_list_iter;
  VSYM_OBJ_REP *vor;
  Place_ro_phi_node(bb_ro_philist, vsym_obj, vsym_obj_list, &vo_list_iter, vor);
}

HEAP_OBJ_REP*
VSA::Find_real_phi_def(CODEREP *x, hash_set<IDTYPE> &visited_bb)
{
  HEAP_OBJ_REP *heap_obj_rep;
  if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI))) {
    PHI_NODE     *phi = x->Defphi();

    if (visited_bb.find(phi->Bb()->Id()) != visited_bb.end())
      return NULL;
    visited_bb.insert(phi->Bb()->Id());


    PHI_OPND_ITER phi_opnd_iter(phi);
    CODEREP      *opnd;
    FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
      if (opnd->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI)))
        heap_obj_rep = Find_real_phi_def(opnd, visited_bb);
      else
        heap_obj_rep = Cr_2_heap_obj(opnd);
      if (heap_obj_rep == NULL) continue;
      if (heap_obj_rep == Null_hor()) continue;  // it is constant Zero, continue search

      Is_Trace(Tracing(), (TFile, "VSA::Update_pending_ref: found successfully\n"));
      return heap_obj_rep;
    }
  } else if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI))) {
    return Find_real_phi_def(x->Defchi()->OPND(), visited_bb);
  }
  return NULL;
}


// =============================================================================
//
//  VSA::Update_reaching_def, to update any pointer assignment that could reach
//       the pointer freed by the free function call
//
//  VSA::Update_pending_ref to update the _pending_ref list, these are paremeter
//       of free calls.  Due to BB traversal sequence, we bound to come across
//       such nodes.
//
// =============================================================================
void
VSA::Update_reaching_def(CODEREP *cr, HEAP_OBJ_REP *hor, MEM_POOL *def_bbs_pool, hash_set<IDTYPE> &visited_bb)
{
  if (cr->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI))) {
    PHI_NODE *phi = cr->Defphi();
    if (visited_bb.find(phi->Bb()->Id()) != visited_bb.end())
      return;
    visited_bb.insert(phi->Bb()->Id());

    BB_NODE *curbb = phi->Bb();
    PHI_OPND_ITER phi_opnd_iter(phi);
    CODEREP *opnd;
    FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
      Update_reaching_def(opnd, hor, def_bbs_pool, visited_bb); 
    }
  }
  else if (cr->Is_flag_set((CR_FLAG)(CF_IS_ZERO_VERSION)) ||
           cr->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI))) {
    ; // do nothing
  }
  else {
    // defined by a regular assignment statement
    STMTREP *defstmt = cr->Get_defstmt();
    if (defstmt->Opr() == OPR_STID &&
        !Cr_2_heap_obj(defstmt->Lhs()) // TODO: this is a hack to remove assertion. need improve. 
        ) {
      HEAP_OBJ_REP *new_hor = Allocate_heap_obj(hor->Heap_obj(), NULL);
      hor->Heap_obj()->Prepend_def_bbs(defstmt->Bb(), def_bbs_pool);
      new_hor->Set_attr(ROR_DEF_BY_AUTO);
      Enter_cr_heap_obj_map(defstmt->Lhs(), new_hor);
      Is_Trace(Tracing(),
               (TFile, "@@@@@@@@ VSA::Update_reaching_def add new hor for STID\n"));
    }
  }
}

void
VSA::Update_pending_ref(MEM_POOL *def_bbs_pool)
{
  STMTREP *call_stmt;
  CODEREP *actual_arg = NULL;
  STMTREP_LIST_ITER stmtrep_list_iter;

  FOR_ALL_ELEM (call_stmt, stmtrep_list_iter, Init(_pending_ref)) {
    HEAP_OBJ_REP *heap_obj_rep = NULL;

    // CF_IS_ZERO_VERSION: WOPT creates chi on malloc/free/etc and those chi are removed
    // by VSA. That causes actual_arg become zero version.
    // Fix in handling malloc/free not removing the chi isn't right because they are not
    // the right def stmt for this cr. This issue should be fixed at beginning of WOPT by
    // not creating mu/chi for malloc/free/etc
    actual_arg = Find_actual_arg(call_stmt);

    // If we couldn't find actual_arg, then continue to check the next item.
    if (actual_arg == NULL ||
        actual_arg->Is_flag_set(CF_IS_ZERO_VERSION) ||
        (actual_arg->Kind() != CK_LDA && actual_arg->Kind() != CK_VAR)) // ignore IVAR/OP so far
      continue;

    if (actual_arg) {
      hash_set<IDTYPE> visited_bb;
      heap_obj_rep = Find_real_phi_def(actual_arg, visited_bb);
    
      Is_Trace(Tracing(), (TFile, "VSA::Update_pending_ref for:\n"));
      Is_Trace_cmd(Tracing(), actual_arg->Print(9, TFile));
    }

    if (heap_obj_rep == NULL) {
      // It's a program error, if actual_arg is local variable and its address
      // is not escaped.  Otherwise, we will make the warning tunable.
      Is_Trace(Tracing(), (TFile, "VSA::Update_pending_ref: could not find heap_obj\n"));
      Is_Trace_cmd(Tracing(), Heap_obj_list()->Print(TFile));
      BOOL is_lda = actual_arg->Kind() == CK_LDA &&
                    TY_kind(ST_type(actual_arg->Lda_base_st())) != KIND_ARRAY;
      AUX_ID aux = actual_arg->Kind() == CK_LDA ? actual_arg->Lda_aux_id() :
                                                  actual_arg->Aux_id();
#if 0
      HEAP_OBJ *ho_w_matching_auxid = Find(aux, is_lda);
#else // 11/02/2019
      HEAP_OBJ *ho_w_matching_auxid = NULL;
      if (is_lda || actual_arg->Is_flag_set((CR_FLAG)CF_DEF_BY_PHI) ||
          !actual_arg->Is_flag_set((CR_FLAG)CF_DEF_BY_CHI))
        ho_w_matching_auxid = Find(aux, is_lda);
      else {
        STMTREP *defstmt = actual_arg->Defstmt();
        // if actual_arg is not defined by entry chi
        if (defstmt->Opr() != OPR_OPT_CHI) {
          ho_w_matching_auxid = Find(aux, is_lda);
        }
      }
#endif
      if (ho_w_matching_auxid != NULL)
         heap_obj_rep = Allocate_heap_obj(ho_w_matching_auxid, NULL);
      else {
        // freeing a memory location that is not allocated inside the PU
        // we will report error when we classify_dbf_error
        BB_NODE* defbb = actual_arg->Kind() == CK_LDA || actual_arg->Is_var_volatile()
                           ? call_stmt->Bb() : actual_arg->Defbb();
        Is_True(defbb,("VSA::Update_pending_ref, actual_arg does not have defstmt"));
        heap_obj_rep = Allocate_heap_obj(actual_arg, defbb, def_bbs_pool);
        heap_obj_rep->Set_srcpos_node(call_stmt,
                                      Comp_unit()->Dna(),
                                      PATHINFO_PARM);
        Is_True(heap_obj_rep->Heap_obj()->Sym_id() == aux, ("aux id mismatch"));
        ST *st = Opt_stab()->Aux_stab_entry(aux)->St();
        switch (ST_sclass(st)) {
        case SCLASS_FORMAL:
        case SCLASS_FORMAL_REF:
          heap_obj_rep->Set_attr(ROR_DEF_BY_IPARM);
          break;
        case SCLASS_FSTATIC:
        case SCLASS_UGLOBAL:
        case SCLASS_DGLOBAL:
        case SCLASS_COMMON:
        case SCLASS_EXTERN:
          heap_obj_rep->Set_asgn_attr(ROR_ASGN_TO_GLOBAL);
          break;
        case SCLASS_AUTO:
        {
          heap_obj_rep->Set_attr(ROR_DEF_BY_AUTO);
          if (actual_arg->Kind() != CK_LDA &&
              !actual_arg->Is_var_volatile()) {
            hash_set<IDTYPE> visited_bb;
            Update_reaching_def(actual_arg, heap_obj_rep, def_bbs_pool, visited_bb);
          }
        }
          break;
        default:
          break;
        }
      }
    }

    HEAP_OBJ_REP *new_heap_obj_rep = Clone_heap_obj(heap_obj_rep, call_stmt->Bb(), _mem_pool);
    new_heap_obj_rep->Set_attr(ROR_DEF_BY_FREE);
    new_heap_obj_rep->Set_stmt_def(call_stmt, Comp_unit()->Dna());
    // cannot find the linenum, defer to rename phase to update

    // enter the new_heap_obj_rep to hor_refmap, correct it in VSA::Rename_call
    Enter_cr_heap_obj_refmap(/*parm_node*/call_stmt->Rhs(), new_heap_obj_rep);

    // treat free as a new definition of the existing heap_obj_rep
    if (!Callee_returns_new_heap_memory(call_stmt))  // exclude realloc
      Enter_cr_heap_obj_map(call_stmt->Rhs(), new_heap_obj_rep); // annotate Rhs new ho
    Is_Trace_cmd(Tracing(), heap_obj_rep->Heap_obj()->Print(TFile));
  }
}

// =============================================================================
//
//   Generate exit_mu list for heap objects
//
// =============================================================================
#if 0
void
VSA::Generate_ho_exit_mu(BB_NODE *bb)
{
  BB_LIST_ITER  bb_iter;
  BB_NODE      *dom_bb;
  BB_LIST_ITER  dom_bb_iter;

  //  Iterate through each statement
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {

    OPERATOR opr = stmt->Opr();

    if (opr == OPR_RETURN || opr == OPR_RETURN_VAL) {
      HO_LIST_ITER  ho_list_iter;
      HEAP_OBJ     *heap_obj;

      FOR_ALL_NODE(heap_obj, ho_list_iter, Init(Heap_obj_list())) {
      }
    }

  }
  FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Generate_ho_exit_mu(dom_bb);  /* child */
  }
}
#endif

// =============================================================================
//
//   Rename heap object on chi list
//
// =============================================================================
void
VSA::Rename_hochi(STMTREP *stmt, BOOL fwd)
{
  OPERATOR opr = stmt->Opr();
  CHI_NODE* cnode;
  CHI_LIST_ITER chi_iter;
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    if (!cnode->Live())
      continue;
    HEAP_OBJ_REP *hor = Cr_2_heap_obj(cnode->RESULT());

    if (hor == NULL && opr != OPR_OPT_CHI) {
      if (OPERATOR_is_call(opr) &&
          Opt_stab()->Aux_stab_entry(cnode->RESULT()->Aux_id())->Is_return_preg())
        continue;

      hor = Cr_2_heap_obj(cnode->OPND());
      if (hor) {
        hor = hor->Heap_obj()->Top_of_stack();
        Enter_cr_heap_obj_map(cnode->OPND(), hor);
        Enter_cr_heap_obj_map(cnode->RESULT(), hor);
      }
    }

    if (hor != NULL && opr == OPR_OPT_CHI) {
      if (fwd) {
        hor->Gen_name(stmt);
        Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Rename_hochi: "));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in the stmt:\n"));
        Is_Trace_cmd(Tracing(), stmt->Print(TFile));
      }
      else {
        if (hor->Heap_obj()->Top_match_sr(stmt))
          hor->Heap_obj()->Pop();
      }
    }
  }
}

// =============================================================================
//
//   Rename heap object reference
//
// =============================================================================
void
VSA::Rename_horef(CODEREP *cr, STMTREP *enclosing_stmt)
{
  switch (cr->Kind()) {
  case CK_CONST:
    return;
  case CK_VAR:
    {
      // Vsa_stmt has associated a hor to this LDID node
      HEAP_OBJ_REP *heap_obj_rep = Cr_2_heap_obj(cr);
      HEAP_OBJ_REP *cur_hor = NULL;
      if (heap_obj_rep != NULL) { // rhs points to a heap_obj
        // reset the the current version
        cur_hor = heap_obj_rep->Heap_obj()->Top_of_stack();
      }
      // dealing with pointers that has not shown in malloc/free 11/02/2019
#if 0
      else {
        STMTREP *defstmt = cr->Defstmt();
        if (cr->Is_flag_set((CR_FLAG)CF_DEF_BY_PHI) ||
            (cr->Is_flag_set((CR_FLAG)CF_DEF_BY_CHI) &&
             defstmt->Opr() != OPR_OPT_CHI)) {
          AUX_ID sym = cr->Aux_id();
          HEAP_OBJ *ho_w_matching_auxid = Find(sym, FALSE);
          if (ho_w_matching_auxid) {
            cur_hor = ho_w_matching_auxid->Top_of_stack();
          }
        }
      }
#endif
      if (cur_hor != NULL) {
        Enter_cr_heap_obj_map(cr, cur_hor);
        Copy_tor_from_hor_to_hor(cur_hor, heap_obj_rep);
        Enter_fsm_mu_ror(enclosing_stmt, cur_hor);
        Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Rename_horef: "));
        Is_Trace_cmd(Tracing(), cur_hor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in the following CR\n"));
        Is_Trace_cmd(Tracing(), cr->Print(0, TFile));
      }
    }
    break;
  case CK_IVAR:
    {
      CODEREP* base;
      if (cr->Opr() == OPR_PARM) {
        base = cr->Ilod_base();
      }
      else {
        base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
      }
      Rename_horef(base, enclosing_stmt);
    }
    break;
  case CK_OP:
    if (cr->Opr() == OPR_CVT)
      Rename_horef(cr->Opnd(0), enclosing_stmt);
    else
      for (INT32 i=0; i<cr->Kid_count(); i++)        {
        Rename_horef(cr->Opnd(i), enclosing_stmt);
      }
    break;
  default:
    ;
  }
}

// =============================================================================
//
//   Find_1st_istore of a VSYM_OBJ_REP node
//
// =============================================================================
STMTREP*
VSYM_OBJ_REP::Find_1st_istore(hash_set<IDTYPE> &visited_bb) const
{
  switch (Attr()) {
  case ROR_DEF_BY_ISTORE:
    return Stmt_def();
  case ROR_DEF_BY_PHI:
  {
    PHI_NODE *pnode = Phi_def();
    BB_NODE  *curbb = pnode->Bb();
    if (visited_bb.find(curbb->Id()) != visited_bb.end())
      return NULL;
    visited_bb.insert(curbb->Id());

    STMTREP  *retval = NULL;
    INT       i;
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), (TFile, "VSYM_OBJ_REP::Find_1st_istore for: "));
    Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), Print(TFile));
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), (TFile, " <- phi("));
    for (i = 0; i < pnode->Size(); ++i) {
      if (i != 0)
        Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), (TFile, ", "));
      VSYM_OBJ_REP* opnd = (VSYM_OBJ_REP *)pnode->OPND(i);
      Is_Trace_cmd(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), opnd->Print(TFile));

      STMTREP *tmp = opnd->Find_1st_istore(visited_bb);
      if (tmp && retval == NULL) retval = tmp;
    }
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG), (TFile, ")\n"));
    return retval;
  }
  break;
  case ROR_DEF_BY_CHI:
  default:
    return NULL;
  }
}

// =============================================================================
//
//   Rename vsym object reference
//
// =============================================================================
void
VSA::Rename_voref(CODEREP *cr)
{
  switch (cr->Kind()) {
  case CK_CONST:
  case CK_VAR:
    return;
  case CK_IVAR:
    {

      if (cr->Opr() == OPR_PARM) {
        Rename_voref(cr->Ilod_base());
      }
      else if (cr->Opr() == OPR_ILOAD || cr->Opr() == OPR_MLOAD) {
        CODEREP *base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base(); 
        Rename_voref(base);
        // rename vor for ILOAD
        VSYM_OBJ_REP *vor = Cr_2_vor(cr);
        if (vor == NULL) { // *(*p) won't get vor in Create_refn_vsym, we make it upper
          HEAP_OBJ_REP *base_hor = Find_cr_heap_obj(base);
          HEAP_OBJ_REP *hor = (base_hor)? base_hor->Heap_obj()->Top_of_stack():NULL;
          if (hor) {
            UINT ifldid = cr->I_field_id();
            VSYM_FLD_REP vfr(FLD_K_ID, ifldid, Cr_ofst(cr));
            VSYM_OBJ *vsym_obj = Find(hor, &vfr);
            if (vsym_obj) vor = vsym_obj->Top_of_stack();
            if (vor) {
              Enter_cr_vor_map(cr, vor);
              // TODO: make up the phi insertion for this new ILOAD created vsym_obj ?
              Is_Trace_cmd(Tracing(), cr->Print(0, TFile));
              Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ VSA::Rename_voref ILOAD, create "));
              Is_Trace_cmd(Tracing(), vor->Print(TFile));
              Is_Trace(Tracing(), (TFile, " based on "));
              Is_Trace_cmd(Tracing(), hor->Print(TFile));
              Is_Trace(Tracing(), (TFile, "\n"));
            }
          }
        }
        if (vor) {
          VSYM_OBJ_REP *cur_vor = vor->Vsym_obj()->Top_of_stack();
          if (vor != cur_vor) {
            Enter_cr_vor_map(cr, cur_vor);
          }
          Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ VSA::Rename_voref: "));
          Is_Trace_cmd(Tracing(), cur_vor->Print(TFile));
          Is_Trace(Tracing(), (TFile, "\n"));
          Is_Trace_cmd(Tracing(), cr->Print(7, TFile));

          STMTREP *defstmt = (cur_vor->Attr() == ROR_DEF_BY_ISTORE)? cur_vor->Stmt_def():NULL;
          HEAP_OBJ_REP *heap_obj_rep = (defstmt)? Cr_2_heap_obj(defstmt->Lhs()):NULL;

          if (heap_obj_rep != NULL) {
            Enter_cr_heap_obj_map(cr, heap_obj_rep);
            Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ VSA::Rename_voref ILOAD with HOR: "));
            Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
            Is_Trace(Tracing(), (TFile, "\n"));
          }
        }
      }
    }
    break;
  case CK_OP:
    if (cr->Opr() == OPR_CVT)
      Rename_voref(cr->Opnd(0));
    else
      for (INT32 i=0; i<cr->Kid_count(); i++)        {
        Rename_voref(cr->Opnd(i));
      }
    break;
  default:
    ;
  }
}

// =============================================================================
// Check if the phi result and opnds are identical
// =============================================================================
BOOL
HEAP_OBJ_REP::Is_phi_identical() const
{
  PHI_NODE* phi = Phi_def();
  Is_True(phi != NULL, ("null phi"));
  HEAP_OBJ_REP* first = (HEAP_OBJ_REP*)phi->OPND(0);
  for (INT i = 1; i < phi->Size(); ++i) {
    HEAP_OBJ_REP* opnd = (HEAP_OBJ_REP*)phi->OPND(i);
    if (opnd != first)
      return FALSE;
  }
  return TRUE;
}

// =============================================================================
//
//   Generate a new SSA name for a heap_obj
//
// =============================================================================
IDTYPE
HEAP_OBJ_REP::Gen_name(STMTREP* sr)
{
  Is_True(this != NULL, ("HEAP_OBJ_REP:Gen_name - NULL this"));
  IDTYPE i = Heap_obj()->Gen_version();
  this->Set_version(i);
  this->Heap_obj()->Push(this, sr);
  return i;
}

// =============================================================================
// HEAP_OBJ_REP::Find_fld(UINT fld)
// =============================================================================
FIELD_OBJ_REP*
HEAP_OBJ_REP::Find_fld(const VSYM_FLD_REP* vfr) const
{
  FIELD_OBJ_REP* fl = Flist();
  while (fl != NULL && !vfr->Match(fl->Fld_rep()))
    fl = fl->Next();
  return fl;
}

// =============================================================================
//
//   Generate a new SSA name for a vsym_obj
//
// =============================================================================
IDTYPE
VSYM_OBJ_REP::Gen_name(STMTREP* sr)
{
  Is_True(this != NULL, ("VSYM_OBJ_REP:Gen_name - NULL this"));
  IDTYPE i = Vsym_obj()->Gen_version();
  this->Set_version(i);
  this->Vsym_obj()->Push(this, sr);
  return i;
}

// =============================================================================
//
//   Generate a heap_obj_rep for the result of a phi function.
//
// =============================================================================
HEAP_OBJ_REP*
VSA::Gen_phi(PHI_NODE *phi, HEAP_OBJ *heap_obj)
{
  Is_True(heap_obj != NULL, ("VSA::Gen_phi: could not find the heap_obj by ID"));
  HEAP_OBJ_REP *phi_result = Allocate_heap_obj(heap_obj, NULL);
  phi_result->Set_attr(ROR_DEF_BY_PHI);
  phi_result->Set_phi_def(phi);
  // do not call IDTYPE version = phi_result->Gen_name();
  Is_Trace(Tracing(), (TFile, "VSA::Gen_phi: heapID:%d, VerID:%d\n", heap_obj->Id(), phi_result->Version()));
  
  phi->Set_result((CODEREP *)phi_result);  // hack to keep the result

  return phi_result; 
}

// =============================================================================
//
//   Generate a vsym_obj_rep for the result of a phi function.
//
// =============================================================================
VSYM_OBJ_REP*
VSA::Gen_phi(PHI_NODE *phi, VSYM_OBJ *vsym_obj)
{
  Is_True(vsym_obj != NULL, ("VSA::Gen_phi: could not find the vsym_obj by ID"));
  VSYM_OBJ_REP *phi_result = Allocate_vsym_obj(vsym_obj);
  phi_result->Set_attr(ROR_DEF_BY_PHI);
  phi_result->Set_phi_def(phi);
  // do not call IDTYPE version = phi_result->Gen_name();
  Is_Trace(Tracing(), (TFile, "VSA::Gen_phi: vsymID:%d, VerID:%d\n", vsym_obj->Id(), phi_result->Version()));
  
  phi->Set_result((CODEREP *)phi_result);  // hack to keep the result

  return phi_result; 
}

// =============================================================================
//
//   Rename heap objects
//
// =============================================================================
void
VSA::Rename(BB_NODE *bb)
{
  PHI_LIST_ITER phi_iter;
  PHI_NODE     *phi;
  BB_LIST_ITER  bb_iter;
  INT32         pos;
  BB_NODE      *succ, *dom_bb;
  BB_LIST_ITER  dom_bb_iter;

  if (Tracing())
    fprintf( TFile, "---- VSA::Rename Heap Object (start) for BB%d\n", bb->Id() );

  //  Iterate through each phi-node 
  FOR_ALL_ELEM (phi, phi_iter, Init(Bb_ho_philist(bb))) {
    HEAP_OBJ_REP *hor = (HEAP_OBJ_REP*)phi->RESULT();
    Is_True(hor != NULL, ("VSA::Rename phi gen name - phi node not setup"));
    if (hor->Version_not_set()) {
      IDTYPE version = hor->Gen_name(NULL);
      Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Rename phi gen name: "));      
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(), (TFile, "\n"));      
    }
    else
      Is_True(FALSE, ("VSA::Rename phi gen name - not handled condition"));
  }

  if (VSA_Hor_Unification)
    Perform_hor_unification(bb);

  //  Iterate through each statement
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {

    OPERATOR opr = stmt->Opr();

    // Process mu_list for the statement

    // Process heap obj in calls
    if ( OPERATOR_is_call(opr) ) 
      Rename_call(stmt);
    // Process LHS of STID, which is treated as reference to heap_obj
    else if (stmt->Rhs()) {
      Rename_horef(stmt->Rhs(), stmt);
      switch (opr) {
      case OPR_STID:
        {
          // Vsa_stmt has associated a hor to the lhs of the statement
          HEAP_OBJ_REP *heap_obj_rep = Cr_2_heap_obj(stmt->Lhs());
          if (heap_obj_rep != NULL) { // lhs points to a heap_obj
            AUX_STAB_ENTRY *aux =_opt_stab->Aux_stab_entry(stmt->Lhs()->Aux_id());
            STMTREP* next = stmt->Next();
            HEAP_OBJ_REP *cur_hor = heap_obj_rep->Heap_obj()->Top_of_stack();
            Enter_fsm_mu_ror(stmt, cur_hor);
            if ((aux->Is_preg() && Is_Return_Preg(aux->St_ofst()) &&
                  next != NULL && next->Opr() == OPR_RETURN) && heap_obj_rep->Asgn_attr() == ROR_ASGN_TO_RETREG ||
                (aux->Is_global() && heap_obj_rep->Asgn_attr() == ROR_ASGN_TO_GLOBAL)) {
              IDTYPE version = heap_obj_rep->Gen_name(stmt);
              heap_obj_rep->Set_prev_ver(cur_hor);
              heap_obj_rep->Set_srcpos_node(stmt, Dna(), PATHINFO_FREE);
              Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Rename STID-COPY: "));
              Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
              Is_Trace(Tracing(), (TFile, "\n"));
            } else if (heap_obj_rep->Attr() == ROR_DEF_BY_AUTO || heap_obj_rep->Attr() == ROR_DEF_BY_NULL) {
              HEAP_OBJ_REP *r_hor = Cr_2_heap_obj(stmt->Rhs());
              if (!r_hor || r_hor != cur_hor) {
                IDTYPE version = heap_obj_rep->Gen_name(stmt);
                if (heap_obj_rep->Attr() != ROR_DEF_BY_NULL)
                  heap_obj_rep->Set_prev_ver(cur_hor);
                heap_obj_rep->Set_srcpos_node(stmt, Dna(), PATHINFO_FREE);
                Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Rename STID-COPY: "));
                Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
                Is_Trace(Tracing(), (TFile, "\n"));
              }
            } else {
              // reset the the current version
              if (stmt->Lhs()->Value_escaped() &&
                  cur_hor->Attr() == ROR_DEF_BY_ALLOC &&
                  cur_hor->Heap_obj()->Kind() == RSC_KIND_ALLOC &&
                  !cur_hor->Escaped()) {
                // if lhs escaped, create a copy and mark it escaped
                HEAP_OBJ_REP *new_hor = Clone_heap_obj(cur_hor, bb, _mem_pool);
                new_hor->Set_attr(ROR_DEF_BY_COPY);
                new_hor->Set_stmt_def(stmt, Comp_unit()->Dna());
                new_hor->Set_escaped(TRUE);
                new_hor->Gen_name(stmt);
                Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Rename STID %d: ",
                                            stmt->Stmtrep_id()));
                Is_Trace_cmd(Tracing(), new_hor->Print(TFile));
                Is_Trace(Tracing(), (TFile, " <- COPY "));
                Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
                Is_Trace(Tracing(), (TFile, "\n"));
                Enter_cr_heap_obj_map(stmt->Lhs(), new_hor);
              }
              else {
                Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Rename STID: "));
                Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
                Is_Trace(Tracing(), (TFile, "\n"));
                Enter_cr_heap_obj_map(stmt->Lhs(), cur_hor);
              }
            }
#if 0
            if (VSA_Hor_Unification)
              Update_ulist_w_ho_rename(Cr_2_heap_obj(stmt->Lhs()), cur_hor, stmt, ROR_DEF_BY_COPY);
#endif
          }
          else if (Cr_2_heap_obj(stmt->Lhs()) == NULL) {
            HEAP_OBJ *ho_w_matching_auxid = Find(stmt->Lhs()->Aux_id(), FALSE);
            if (ho_w_matching_auxid != NULL) {
              HEAP_OBJ_REP *hor = ho_w_matching_auxid->Top_of_stack();
              STMTREP* defstmt = Find_ptr_defstmt(stmt->Rhs());
              if (defstmt != NULL) {
                // we have lhs = rhs; rhs doesn't have heap_obj and lhs has heap obj somewhere
                // lhs is defined by chi, we need a new version on lhs
                hor = Clone_heap_obj(hor, bb, _mem_pool);
                hor->Set_attr(ROR_DEF_BY_CHI);
                hor->Set_stmt_def(defstmt, Comp_unit()->Dna());
                hor->Gen_name(stmt);
              }
              Enter_cr_heap_obj_map(stmt->Lhs(), hor);
              hor->Heap_obj()->Push(hor, stmt);
              Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Rename: for missing heap_obj, set to entry chi\n"));
              Is_Trace_cmd(Tracing(), hor->Print(TFile));
              Is_Trace(Tracing(), (TFile, "\n"));
            }
            else {
              // check if rhs has hor and rhs hor asgn flag according to STMT lhs
              // consider cases 1:
              //  if (...) p = malloc(...); // a
              //  else p = malloc(...);     // b
              //  return p;                 // no MSF on a and b
              // consider case 2:
              //  q = malloc(...);          // a
              //  if (...) p = q;
              //  return p;                 // a MSF on a if p = q doesn't take
              CODEREP* rhs = stmt->Rhs();
              HEAP_OBJ_REP *rhs_hor = Cr_2_heap_obj(rhs);
              rhs_hor = rhs_hor ? rhs_hor->Heap_obj()->Top_of_stack() : NULL;
              if (rhs_hor != NULL &&
                  rhs_hor->Asgn_attr() == ROR_ASGN_TO_NONE &&
                  rhs_hor->Attr() == ROR_DEF_BY_PHI &&
                  rhs->Kind() == CK_VAR &&
                  rhs->Is_flag_set(CF_DEF_BY_PHI) &&
                  rhs->Defphi()->Bb() == rhs_hor->Phi_def()->Bb()) {
                // set ASGN flag for case above
                AUX_STAB_ENTRY *aux =_opt_stab->Aux_stab_entry(stmt->Lhs()->Aux_id());
                ROR_ATTR attr = ROR_ASGN_TO_NONE;
                if (aux->Is_global())
                  attr = ROR_ASGN_TO_GLOBAL;
                else if (stmt->Next() != NULL && stmt->Next()->Opr() == OPR_RETURN &&
                    aux->Is_preg() && Is_Return_Preg(aux->St_ofst()))
                  attr = ROR_ASGN_TO_RETREG;
                if (attr != ROR_ASGN_TO_NONE) {
                  if (rhs_hor->Ulist() != NULL) {
                    HOR_LIST_ITER hor_list_iter;
                    HEAP_OBJ_REP *cur_hor;
                    FOR_ALL_NODE(cur_hor, hor_list_iter, Init(rhs_hor->Ulist())) {
                      if (cur_hor->Attr() == ROR_DEF_BY_PHI &&
                          cur_hor->Phi_def()->Bb() == rhs_hor->Phi_def()->Bb())
                        rhs_hor->Set_asgn_attr(attr);
                    }
                  }
                  else
                    rhs_hor->Set_asgn_attr(attr);
                }
              }
            }
          }
        }
        break;
      case OPR_ISTORE:
        Rename_horef(stmt->Lhs(), stmt);
        break;
      default:
        break;
      }
    }
    // Process CHI list here
    Rename_hochi(stmt, TRUE);

    // clone heap_obj_rep when it escapes and push stack 
    // Check_heap_obj_escaped(bb, stmt, TRUE);

    // track heap object rename stack top element at exit BB
    if (opr == OPR_RETURN || opr == OPR_RETURN_VAL)
      Generate_ho_exit_mu(bb);
  }

  FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
    pos = succ->Pred()->Pos(bb);
    FOR_ALL_ELEM (phi, phi_iter, Init(Bb_ho_philist(succ))) {
      HEAP_OBJ_REP *hor = (HEAP_OBJ_REP *) phi->RESULT();
      Is_True(pos != -1,
              ("VSA::Rename, Corrupted CFG, BB%d's successor BB%d does not point back to it\n",
               bb->Id(), succ->Id()));
      phi->Set_opnd(pos, (CODEREP *) (hor->Heap_obj()->Top_of_stack())); // hack

      HOR_LIST     *hor_list = hor->Ulist();
      if (hor_list != NULL) {
        HOR_NODE *cur_hor = hor_list->Head();
        HOR_NODE *prev = NULL;
        while (cur_hor != NULL) {
          HOR_NODE *tmp = cur_hor;
          Is_True(tmp->Hor() != NULL, ("null hor"));
          HEAP_OBJ_REP *tmp_hor = tmp->Hor();
          cur_hor = cur_hor->Next();
          HEAP_OBJ_REP *cur_top = tmp_hor->Heap_obj()->Top_of_stack();
          if (tmp_hor->Heap_obj() != hor->Heap_obj() ||
              tmp_hor == cur_top || hor_list->Find(cur_top) != NULL) {
            prev = tmp;
          }
          else {
            HOR_NODE *cur_top_node = CXX_NEW(HOR_NODE(cur_top), Mem_pool());
            hor_list->Append(cur_top_node, tmp);
            if (prev != NULL)  // do not remove the first one because it's phi result
              hor_list->Remove(prev, tmp);
            prev = cur_top_node;
            Is_Trace(Tracing(), (TFile, "HOR Unification: "));
            Is_Trace_cmd(Tracing(), tmp_hor->Print(TFile));
            Is_Trace(Tracing(), (TFile, " replaced by "));
            Is_Trace_cmd(Tracing(), cur_top->Print(TFile));
            Is_Trace(Tracing(), (TFile, " in Rename from BB%d to BB%d pos=%d\n",
                                        bb->Id(), succ->Id(), pos));
          }
        }
      }
    }
  }

  FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Rename(dom_bb);  /* child */
  }

  if (Tracing())
    fprintf( TFile, "---- VSA::Rename Heap Object (Reverse) for BB%d\n", bb->Id() );
  //  The statements are processed in reverse order when poping
  //  rename stack.
  //
  // iterate through each statement in this bb
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {

    OPERATOR opr = stmt->Opr();

    // verify heap_obj_rep and pop stack
    Check_heap_obj_escaped(bb, stmt, FALSE);

    // process CHI list

    // Process heap obj in calls
    if (OPERATOR_is_call(opr)) {
      HEAPSTATE heapstate;
      if ((heapstate = Callee_returns_new_heap_memory(stmt)) ||
          Callee_frees_heap_memory(stmt) != NULL) {
        HEAP_OBJ_REP *hor;
        if(Callee_frees_heap_memory(stmt) != NULL) {
          hor = Cr_2_heap_obj_ref(stmt->Rhs());
          if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt)) {
            if (VSA_Hor_Unification)
              Update_ulist_w_free_rename_rev(hor, stmt);
            if (hor->Heap_obj()->Top_match_sr(stmt))
              hor->Heap_obj()->Pop();
          }
        }
        if(heapstate) {
          hor = Cr_2_heap_obj(stmt->Rhs());
          if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt)) {
            hor->Heap_obj()->Pop();
          }
        }
      }
      else if (opr == OPR_CALL) {
        // for RSC obj
        DNA_NODE *dna = Comp_unit()->Dna();
        RNA_NODE *rna = dna->Get_callsite_rna(stmt);
        DNA_NODE *callee = Ipsa()->Get_dna(rna->Uniq_callee());
        if (callee) {
          if (callee->Is_set_rbc_flag(DNA_RBC_MODEL)) {
            HEAP_OBJ_REP *hor = Cr_2_heap_obj(stmt->Rhs());
            if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt)) {
              hor->Heap_obj()->Pop();
            }
          }
        }
      }
      else if (opr == OPR_ICALL) {
        // for RSC obj
        DNA_NODE *dna = Comp_unit()->Dna();
        RNA_NODE *rna = dna->Get_callsite_rna(stmt);
        const CALLEE_VECTOR& callee_list = rna->Callee_list();
        for (CALLEE_VECTOR::const_iterator iter = callee_list.begin(); iter != callee_list.end(); iter++) {
          DNA_NODE *callee = Ipsa()->Get_dna(iter->Callee());
          if (callee) {
            if (callee->Is_set_rbc_flag(DNA_RBC_MODEL)) {
              HEAP_OBJ_REP *hor = Cr_2_heap_obj(stmt->Rhs());
              if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt)) {
                hor->Heap_obj()->Pop();
              }
            }
          }
        }
      }
      // return hor
      CODEREP* call_return_store = Comp_unit()->Find_return_value(stmt);
      if (call_return_store != NULL && call_return_store->Kind() == CK_VAR &&
          TY_kind(call_return_store->Lod_ty()) == KIND_POINTER) {
        HEAP_OBJ_REP* hor = Cr_2_heap_obj(call_return_store);
        if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt))
          hor->Heap_obj()->Pop();
      }
    }
    else if (opr == OPR_STID) {
      HEAP_OBJ_REP *hor = Cr_2_heap_obj(stmt->Lhs());
      if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt)) {
#if 0
        if (VSA_Hor_Unification)
          Update_ulist_w_ho_rename_rev(hor, stmt);
#endif
        hor->Heap_obj()->Pop();
      }
    }
    else if (opr == OPR_ISTORE) {
    }
    else if (opr == OPR_OPT_CHI) {
      Rename_hochi(stmt, FALSE);
    }
  }

  FOR_ALL_ELEM (phi, phi_iter, Init(Bb_ho_philist(bb))) {
    HEAP_OBJ_REP *hor = (HEAP_OBJ_REP *) phi->RESULT();
    Is_True(hor == hor->Heap_obj()->Top_of_stack(), ("stack mismatch"));
    hor->Heap_obj()->Pop();
  }
  if (Tracing())
    fprintf( TFile, "---- VSA::Rename Heap Object (end) for BB%d\n", bb->Id() );
}

// =============================================================================
//
//   Rename vsym objects - similar to VSA::Rename which is for heap_obj_rep,
//     this procedure perform rename algorithm on vsym_obj_rep.  However, we
//     piggy back the heap_obj_rep rename on top of the vor that is just renamed
//     especially those referenced inside free call
//
// =============================================================================
void
VSA::Rename_vsym(BB_NODE *bb, MEM_POOL *def_bbs_pool)
{
  PHI_LIST_ITER phi_iter;
  PHI_NODE     *phi;
  BB_LIST_ITER  bb_iter;
  INT32         pos;
  BB_NODE      *succ, *dom_bb;
  BB_LIST_ITER  dom_bb_iter;
  CHI_LIST_ITER chi_iter;
  CHI_NODE *cnode;

  if (Tracing())
    fprintf( TFile, "@@@@@@@@@@ VSA::Rename Vsym Object for BB%d\n", bb->Id() );

  //  Iterate through each phi-node of vsym_obj
  FOR_ALL_ELEM (phi, phi_iter, Init(Bb_vo_philist(bb))) {
    VSYM_OBJ_REP *vor = (VSYM_OBJ_REP*)phi->RESULT();
    Is_True(vor != NULL, ("VSA::Rename phi gen name - phi node not setup"));
    if (vor->Version_not_set()) {
      IDTYPE version = vor->Gen_name(NULL);
      vor->Set_phi_def(phi);
      Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ BB%d VSA::Rename_vsym phi gen name: ", bb->Id()));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, "\n"));      
    }
    else
      Is_True(FALSE, ("VSA::Rename_vsym phi gen name - not handled condition"));
  }
  //  Iterate through each phi-node of heap_obj
  FOR_ALL_ELEM (phi, phi_iter, Init(Bb_ho_philist(bb))) {
    HEAP_OBJ_REP *hor = (HEAP_OBJ_REP*)phi->RESULT();
    Is_True(hor != NULL, ("VSA::Rename_vsym - phi node not setup"));
    hor->Heap_obj()->Push(hor, NULL);
  }

  //  Iterate through each statement
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {

    OPERATOR opr = stmt->Opr();

    if (opr == OPR_OPT_CHI)
      Rename_entry_vsym_chi(stmt, def_bbs_pool, TRUE);

    // Process mu_list for the statement
    MU_LIST_ITER mu_iter;
    MU_NODE *mnode;
    FOR_ALL_NODE( mnode, mu_iter, Init(Stmt_vor_mu(stmt))) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      VSYM_OBJ_REP *vor = cvor->first;
      VSYM_OBJ_REP *cur_vor = vor->Vsym_obj()->Top_of_stack();
      if (vor != cur_vor) {
        cvor->first = cur_vor;
        Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d:BB%d VSA::Rename_vsym MU: ", Srcpos_To_Line(stmt->Linenum()), bb->Id()));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, "\n"));
        Is_Trace_cmd(Tracing(), stmt->Print(TFile));
      }
    }

    // Process heap obj in calls
    if ( OPERATOR_is_call(opr) ) {
      Rename_vocall(stmt, def_bbs_pool);
    }
    else if (stmt->Rhs()) {
      // Process LHS of STID, which is treated as reference to heap_obj
      Rename_voref(stmt->Rhs());
      switch (opr) {
      case OPR_STID:
        {
          // Vsa_stmt has associated a hor to the lhs of the statement
          HEAP_OBJ_REP *heap_obj_rep = Cr_2_heap_obj(stmt->Lhs());
          if (heap_obj_rep != NULL) { // lhs points to a heap_obj
            if (heap_obj_rep->Attr() == ROR_DEF_BY_AUTO) {
              heap_obj_rep->Heap_obj()->Push(heap_obj_rep, stmt);
            }
          }
          VSYM_OBJ_REP* vor = Cr_2_vor(stmt->Lhs());
          if (vor != NULL) {
            // reset the the current version
            IDTYPE version = vor->Gen_name(stmt);
            Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d:BB%d VSA::Rename_vsym STID: ", Srcpos_To_Line(stmt->Linenum()), bb->Id()));
            Is_Trace_cmd(Tracing(), vor->Print(TFile));
            Is_Trace(Tracing(), (TFile, "\n"));
            Is_Trace_cmd(Tracing(), stmt->Print(TFile));
            if (heap_obj_rep != NULL) {
              Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d:BB%d VSA::Rename_vsym STID with HOR: ",
                                   Srcpos_To_Line(stmt->Linenum()), bb->Id()));
              Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
              Is_Trace(Tracing(), (TFile, "\n"));
              HEAP_OBJ_REP *rhor = Cr_2_heap_obj(stmt->Rhs());
              if (rhor)
                vor->Set_hor(rhor);
              else
                vor->Set_hor(heap_obj_rep);
            }
          }
        }
        break;
      case OPR_ISTORE:
      case OPR_MSTORE:
        {
          CODEREP *cr = stmt->Lhs();
          CODEREP *base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
          Rename_voref(base);

          // Create_refn_vsym has associated a vor to the lhs of the statement
          VSYM_OBJ_REP *vor = Cr_2_vor(stmt->Lhs());
          if (vor == NULL) {
            // the ISTORE failed in the Create_refn_vsym, probably due to *(*p)=malloc()
            HEAP_OBJ_REP *base_hor = Find_cr_heap_obj(base);
            HEAP_OBJ_REP *hor = (base_hor)? base_hor->Heap_obj()->Top_of_stack():NULL;
            INT32 linenum = Srcpos_To_Line(stmt->Linenum());
            if (hor) {
              UINT ifldid = cr->I_field_id();
              vor = Allocate_vsym_obj(bb, hor, ifldid, Cr_ofst(cr), def_bbs_pool);
              // TODO: make up the phi insertion for this new ISTORE created vsym_obj
              vor->Set_srcpos_node(stmt, Comp_unit()->Dna(), PATHINFO_ISTORE);
              vor->Set_attr(ROR_DEF_BY_ISTORE);
              vor->Set_stmt_def(stmt, Comp_unit()->Dna());
              Enter_cr_vor_map(cr, vor);
              Is_Trace_cmd(Tracing(), cr->Print(0, TFile));
              Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d VSA::Rename_vsym ISTORE, create ", linenum));
              Is_Trace_cmd(Tracing(), vor->Print(TFile));
              Is_Trace(Tracing(), (TFile, " based on "));
              Is_Trace_cmd(Tracing(), hor->Print(TFile));
              Is_Trace(Tracing(), (TFile, "\n"));
            }
          }
          if (vor != NULL) { // lhs points to a vsym_obj
            // reset the the current version
            IDTYPE version = vor->Gen_name(stmt);
            Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d:BB%d VSA::Rename_vsym ISTORE: ", Srcpos_To_Line(stmt->Linenum()), bb->Id()));
            Is_Trace_cmd(Tracing(), vor->Print(TFile));
            Is_Trace(Tracing(), (TFile, "\n"));     
            Is_Trace_cmd(Tracing(), stmt->Print(TFile));
            HEAP_OBJ_REP *heap_obj_rep = Cr_2_heap_obj(stmt->Rhs());
            if (heap_obj_rep != NULL) {
              Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d:BB%d VSA::Rename_vsym ISTORE with HOR: ",
                                   Srcpos_To_Line(stmt->Linenum()), bb->Id()));
              Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
              Is_Trace(Tracing(), (TFile, "\n"));     
              vor->Set_hor(heap_obj_rep);
            }
          }
        }
        break;
      default:
        break;
      }
    }

    // Process CHI list here
    FOR_ALL_NODE( cnode, chi_iter, Init(Stmt_vor_chi(stmt))) {
      CVOR *cvor = (CVOR*)cnode->OPND();
      // set the operand to the current version of vo
      VSYM_OBJ_REP *vor = cvor->first;
      VSYM_OBJ_REP *cur_vor = vor->Vsym_obj()->Top_of_stack();
      if (vor != cur_vor) {
        cvor->first = cur_vor;
      }
      // call Gen_name to generate a new version for the result
      cvor = (CVOR*)cnode->RESULT();
      vor = cvor->first;
      IDTYPE version = vor->Gen_name(stmt);
      vor->Set_stmt_def(stmt, Comp_unit()->Dna());

      Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d VSA::Rename_vsym CHI: ", Srcpos_To_Line(stmt->Linenum())));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, "\n"));     
    }

    // clone heap_obj_rep when it escapes and push stack
    Check_heap_obj_escaped(bb, stmt, TRUE);

    // rename HOR annotated on return stmt
    if (stmt->Opr() == OPR_RETURN || stmt->Opr() == OPR_RETURN_VAL) {
      Create_return_vsym_mu(stmt, def_bbs_pool);
    }
  }

  FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
    pos = succ->Pred()->Pos(bb);
    FOR_ALL_ELEM (phi, phi_iter, Init(Bb_vo_philist(succ))) {
      VSYM_OBJ_REP *vor = (VSYM_OBJ_REP *) phi->RESULT();
      Is_True(pos != -1,
              ("VSA::Rename, Corrupted CFG, BB%d's successor BB%d does not point back to it\n",
               bb->Id(), succ->Id()));
      phi->Set_opnd(pos, (CODEREP *) (vor->Vsym_obj()->Top_of_stack())); // hack
    }
  }

  FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Rename_vsym(dom_bb, def_bbs_pool);  /* child */
  }

  //  The statements are processed in reverse order when poping
  //  rename stack.
  //
  // iterate through each statement in this bb
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {

    OPERATOR opr = stmt->Opr();

    // verify heap_obj_rep and pop stack
    Check_heap_obj_escaped(bb, stmt, FALSE);

    // process CHI list
    FOR_ALL_NODE( cnode, chi_iter, Init(Stmt_vor_chi(stmt))) {
      CVOR *cvor = (CVOR*)cnode->OPND();
      // set the operand to the current version of vo
      VSYM_OBJ_REP *vor = cvor->first;
      if(vor->Vsym_obj()->Top_match_sr(stmt))
        vor->Vsym_obj()->Pop();
    }

    // Process heap obj in calls
    if ( OPERATOR_is_call(opr)) {
      HEAPSTATE heapstate;
      if ((heapstate = Callee_returns_new_heap_memory(stmt)) ||
          Callee_frees_heap_memory(stmt) != NULL) {
        HEAP_OBJ_REP *hor;
        if(heapstate) {
          hor = Cr_2_heap_obj(stmt->Rhs());
          if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt)) {
            hor->Heap_obj()->Pop();
          }
          VSYM_OBJ_REP* vor = Cr_2_vor(stmt->Rhs());
          if (vor != NULL && vor->Vsym_obj()->Top_match_sr(stmt)) {
            vor->Vsym_obj()->Pop();
          }
        }
        if(Callee_frees_heap_memory(stmt) != NULL) {
          hor = Cr_2_heap_obj_ref(stmt->Rhs());
          if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt)) {
            if (VSA_Hor_Unification)
              Update_ulist_w_free_rename_rev(hor, stmt);
            if (hor->Heap_obj()->Top_match_sr(stmt))
              hor->Heap_obj()->Pop();
          }
        }
      }
      else if (opr == OPR_CALL) {
#if 0
        // for Long to do the right thing
        // for RSC obj
        DNA_NODE *dna = Comp_unit()->Dna();
        RNA_NODE *rna = dna->Get_callsite_rna(stmt);
        DNA_NODE *callee = Ipsa()->Get_dna(rna->Callee_idx());
        if (callee && callee->Is_rbc_set(DNA_RBC_MODEL)) {
          HEAP_OBJ_REP *hor = Cr_2_heap_obj(stmt->Rhs());
          if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt)) {
            hor->Heap_obj()->Pop();
          }
        }
#endif
        // Probably not the right place for this piece of code. ICALL isn't handled here
      }
      // return hor and vsym
      CODEREP* call_return_store = Comp_unit()->Find_return_value(stmt);
      if (call_return_store != NULL && call_return_store->Kind() == CK_VAR &&
          TY_kind(call_return_store->Lod_ty()) == KIND_POINTER) {
        HEAP_OBJ_REP* hor = Cr_2_heap_obj(call_return_store);
        if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt)) {
          hor->Heap_obj()->Pop();
        }
      }
    }
    else {
      switch (opr) {
      case OPR_ISTORE:
      case OPR_MSTORE:
      {
        VSYM_OBJ_REP *vor = Cr_2_vor(stmt->Lhs());
        if (vor != NULL &&
            vor->Vsym_obj()->Top_match_sr(stmt)) { // rhs points to a vsym_obj
          // reset the the current version
          vor->Vsym_obj()->Pop();
        }
      }
      break;
      case OPR_STID:
      {
        VSYM_OBJ_REP* vor = Cr_2_vor(stmt->Lhs());
        if (vor != NULL &&
            vor->Vsym_obj()->Top_match_sr(stmt)) {
          // reset the the current version
          vor->Vsym_obj()->Pop();
        }
        HEAP_OBJ_REP *hor = Cr_2_heap_obj(stmt->Lhs());
        if (hor != NULL &&
            hor->Attr() == ROR_DEF_BY_AUTO &&
            hor->Heap_obj()->Top_match_sr(stmt)) {
          hor->Heap_obj()->Pop();
        }
      }
      break;
      case OPR_OPT_CHI:
      {
        Rename_entry_vsym_chi(stmt, def_bbs_pool, FALSE);
      }
      break;
      default:
      break;
      }
    }
  }

  FOR_ALL_ELEM (phi, phi_iter, Init(Bb_ho_philist(bb))) {
    HEAP_OBJ_REP *hor = (HEAP_OBJ_REP *) phi->RESULT();
    Is_True(hor == hor->Heap_obj()->Top_of_stack(), ("hor stack mismatch"));
    hor->Heap_obj()->Pop();
  }
  FOR_ALL_ELEM (phi, phi_iter, Init(Bb_vo_philist(bb))) {
    VSYM_OBJ_REP *vor = (VSYM_OBJ_REP *) phi->RESULT();
    Is_True(vor == vor->Vsym_obj()->Top_of_stack(), ("vor stack mismatch"));
    vor->Vsym_obj()->Pop();
  }
}

// =============================================================================
//
//   Verify Heap object stack
//
// =============================================================================
void
VSA::Verify_heap_obj_stack(void)
{
  HEAP_OBJ *ho;
  HO_LIST_ITER ho_iter;
  
  FOR_ALL_ELEM (ho, ho_iter, Init(_heap_obj_list)) {
    Is_True(ho->Stack()->Elements() == 1,
            ("Heap obj stack should only contain Entry_chi. id %d , Stack size %d.", ho->Id(), ho->Stack()->Elements()));
  }
}

// =============================================================================
//
//   Verify Vsym object stack
//
// =============================================================================
void
VSA::Verify_vsym_obj_stack(void)
{
  VSYM_OBJ *vo;
  VO_LIST_ITER vo_iter;
  
  FOR_ALL_ELEM (vo, vo_iter, Init(_vsym_obj_list)) {
    Is_True(vo->Stack()->Elements() == 1,
            ("Vsym obj stack should only contain Entry_chi. id %d , Stack size %d.", vo->Id(), vo->Stack()->Elements()));
  }
}


// =============================================================================
//
//   Create_refn_vsym traverse coderep, identify and create refined_vsym
//
// =============================================================================
HEAP_OBJ_REP *
VSA::Find_cr_heap_obj(CODEREP *cr)
{
  HEAP_OBJ_REP *ret;
  switch (cr->Kind()) {
  case CK_LDA:
  case CK_VAR: {
    ret = Cr_2_heap_obj(cr);
#if 0
    if (ret == NULL) {
      // the base is not initialized by malloc call
      HEAP_OBJ *ho_w_matching_auxid = Find(cr->Aux_id());
      if (ho_w_matching_auxid != NULL)
        ret = Allocate_heap_obj(ho_w_matching_auxid, NULL);
    }
#endif
    return ret;
  }
  case CK_IVAR: {
    ret = Cr_2_heap_obj(cr);
    if (ret == NULL) {
      VSYM_OBJ_REP *vor = Cr_2_vor(cr);
      if (vor)
        ret = vor->Hor();
    }
    // if ret is NULL, it's due to the ISTORE statement for this ILOAD is different ILOAD
    // caused by the alias effect of ISTORE statement
    return ret;
  }
  case CK_OP:
    cr = Find_ilod_base(cr);
    return cr ? Find_cr_heap_obj(cr) : NULL;
  default:
    return NULL;
  }
}

// =============================================================================
// Create_entrychi_hor
// create HEAP_OBJ and HEAP_OBJ_REP for parameter on entry chi
// =============================================================================
void
VSA::Create_entrychi_hor(CODEREP *cr, STMTREP *stmt, MEM_POOL *pool)
{
  Is_True(cr != NULL && cr->Kind() == CK_VAR, ("cr is NULL"));
  Is_True(Cr_2_heap_obj(cr) == NULL, ("hor already created"));
  Is_True(stmt->Opr() == OPR_OPT_CHI &&
          WN_operator(stmt->Bb()->Entrywn()) == OPR_FUNC_ENTRY, ("not entry"));
  HEAP_OBJ_REP* hor = Allocate_heap_obj(cr, stmt->Bb(), pool);
  hor->Heap_obj()->Set_kind(RSC_KIND_IPARM);
  Is_True(hor->Heap_obj()->Sym_id() == cr->Aux_id(), ("aux id mismatch"));
  Enter_cr_heap_obj_map(cr, hor);
}

// =============================================================================
//
//   Get offset from coderep, ignore preg offset
//
// =============================================================================
AUX_ID
VSA::Cr_aux_id(CODEREP *cr) const
{
  return cr->Kind() == CK_LDA ? cr->Lda_aux_id() :
           cr->Kind() == CK_VAR ? cr->Aux_id() :
             (cr->Kind() == CK_IVAR && cr->Ivar_mu_node()) ?
               cr->Ivar_mu_node()->Aux_id() : 0;
}

// =============================================================================
//
//   Get VSYM_FLD_REP from coderep
//
// =============================================================================
VSYM_FLD_REP
VSA::Cr_vfr(CODEREP *cr) const
{
  if (cr->Kind() == CK_OP) {
    // TODO: improve for OP like ptr +/- const
    return VSYM_FLD_REP(FLD_K_ANY, 0, 0);
  }

  Is_True(cr->Kind() == CK_LDA || cr->Kind() == CK_VAR || cr->Kind() == CK_IVAR,
          ("not VAR or IVAR"));
  if (cr->Kind() == CK_LDA || cr->Kind() == CK_VAR)
    return VSYM_FLD_REP(FLD_K_ID, Cr_fldid(cr), Cr_ofst(cr));
  UINT fld_id = 0;
  mINT32 ofst = 0;
  if (cr->Opr() != OPR_PARM) {
    fld_id = cr->I_field_id();
    ofst = cr->Offset();
  }

  CODEREP *base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
  if (base->Kind() == CK_VAR || base->Kind() == CK_IVAR)
    return VSYM_FLD_REP(FLD_K_ID, fld_id, ofst);
  if (base->Kind() == CK_LDA)
    return VSYM_FLD_REP(FLD_K_ID, fld_id + base->Afield_id(),
                                  ofst + base->Offset());
  if (base->Kind() == CK_CONST)
    return VSYM_FLD_REP(FLD_K_ID, fld_id,
                        ofst + base->Const_val());
  if (base->Kind() == CK_RCONST)
    return VSYM_FLD_REP(FLD_K_ID, fld_id,
                        ofst + (INT)base->Const_fval());

  Is_True(cr->Opr() == OPR_MLOAD || base->Kind() == CK_OP,
          ("LDA-ILOAD not folded?"));
  // TODO: fix up field/offset by checking types
  // VSA_ADDRESS_INFO info;
  // Comp_unit()->Analyze_pointer_info(NULL, base, &info, FALSE);
  // UINT fld_id = cr->I_field_id();
  // INT32 fld_ofst = cr->Offset() + info.Fix_ofst();
  // Fixup_field_id(info.Base()->object_ty(), fld_id, fld_ofst);
  base = Find_ilod_base(base);
  if (base != NULL && base->Kind() == CK_IVAR) {
    // special handling for recursive data structure
    HEAP_OBJ_REP *hor = Cr_2_heap_obj(base);
    VSYM_OBJ_REP *vor = Cr_2_vor(base);
    if (hor == vor->Vsym_obj()->Base_hor()) {
      fld_id += base->I_field_id();
      ofst += base->Offset();
    }
  }
  return VSYM_FLD_REP(FLD_K_ANY, fld_id, 0);
}

// =============================================================================
//
//   Get VSYM_FLD_REP which may be accessed via the base
//
// =============================================================================
VSYM_FLD_REP
VSA::Base_vfr(CODEREP *cr) const
{
  TY_IDX obj_ty;
  UINT   fld_id;
  INT32  fld_ofst;
  BOOL   force_any = FALSE;

  if (cr->Kind() == CK_CONST || cr->Kind() == CK_RCONST) {
    Is_True(FALSE, ("CONST/RCONST"));
    return VSYM_FLD_REP(FLD_K_ID, 0, 0);
  }

  if (cr->Kind() == CK_OP) {
    VSA_ADDRESS_INFO info;
    if (Comp_unit()->Analyze_pointer_info(NULL, cr, &info, FALSE) == FALSE)
      return VSYM_FLD_REP(FLD_K_ANY, fld_id, fld_ofst);
    cr = info.Base();
    fld_ofst = info.Fix_ofst();
    force_any = !info.Const_ofst();
  }
  else {
    fld_ofst = cr->Offset();
  }

  Is_True(cr->Kind() == CK_LDA ||
          cr->Kind() == CK_VAR ||
          cr->Kind() == CK_IVAR, ("bad base cr kind"));

  switch (cr->Kind()) {
  case CK_LDA:
    obj_ty = cr->Lda_ty();
    fld_id = cr->Afield_id();
    break;

  case CK_VAR:
    obj_ty = cr->Lod_ty();
    fld_id = cr->Field_id();
    break;

  case CK_IVAR:
    obj_ty = cr->Ilod_ty();
    fld_id = cr->I_field_id();
    break;

  case CK_OP:
  case CK_CONST:
  case CK_RCONST:
  default:
    Is_True(FALSE, ("CONST/RCONST or wrong kind"));
    return VSYM_FLD_REP(FLD_K_ID, 0, 0);
  }

  // TODO: fixup field id/offset for OP
  // if (force_any)
  //   Fixup_field_id(obj_ty, fld_id, fld_ofst);

  // handle field id
  if (fld_id > 0) {
    Is_True(TY_kind(obj_ty) == KIND_STRUCT, ("not struct"));
    UINT cur_field_id = 0;
    FLD_HANDLE fld = FLD_get_to_field(obj_ty, fld_id, cur_field_id);
    Is_True (!fld.Is_Null(), ("Invalid field id"));
    obj_ty = FLD_type(fld);
  }

  // return ANY or ID depends oon obj_ty is scalar or aggregate
  return (force_any ||
          TY_kind(obj_ty) == KIND_STRUCT ||
          TY_kind(obj_ty) == KIND_ARRAY)
           ? VSYM_FLD_REP(FLD_K_ANY, fld_id, fld_ofst)
           : VSYM_FLD_REP(FLD_K_ID, fld_id, fld_ofst);
}

// =============================================================================
//
//   Get fieldid from coderep
//
// =============================================================================
IDTYPE
VSA::Cr_fldid(CODEREP *cr) const
{
  return cr->Kind() == CK_VAR ? cr->Field_id() :
           cr->Kind() == CK_IVAR ? cr->I_field_id() :
             cr->Kind() == CK_LDA ? cr->Afield_id() : 0;
}

// =============================================================================
//
//   Get offset from coderep, ignore preg offset
//
// =============================================================================
mINT32
VSA::Cr_ofst(CODEREP *cr) const
{
  mINT32 ofst = cr->Offset();
  if(cr->Kind() == CK_VAR &&
     Opt_stab()->Aux_stab_entry(cr->Aux_id())->Is_preg()) {
    ofst = 0;
  }
  return ofst;
}

// =============================================================================
//
//   Create refined vsym in mu list of a function.
//   Note: we do not need this for ISTORE since we have lumped aliased pointers
//         into one heap_obj and take care of it at the statement level
//
// =============================================================================
void
VSA::Create_refn_vsym_mu(CODEREP *cr, STMTREP *stmt, HEAP_OBJ_REP *hor,
                         IDTYPE fldid, mINT32 ofst, MEM_POOL *pool, VS_FLD_KIND fld_k)
{
  if (hor == Null_hor())
    return;
  BB_NODE *bb = stmt->Bb();
  VSYM_OBJ_REP *vor = Allocate_vsym_obj(bb, hor, fldid, ofst, pool, fld_k);
  MU_LIST *mu_list = Stmt_vor_mu(stmt);
  MU_NODE *mnode = Create_stmt_vsym_mu(cr, stmt, vor);
  mu_list->Append(mnode);
}

// =============================================================================
// Create_stmt_vsym_chi
// =============================================================================
MU_NODE*
VSA::Create_stmt_vsym_mu(CODEREP *cr, STMTREP *stmt, VSYM_OBJ_REP *vor)
{
  Is_True(!Is_special_vor(vor), ("no chi for special vor"));
  Is_True(cr->Kind() != CK_IVAR || cr->Opr() != OPR_PARM, ("hit PARM"));
  MU_NODE *mnode = CXX_NEW(MU_NODE, Mem_pool());
  CVOR     *cvor = CXX_NEW(CVOR(vor, cr), Mem_pool());
  mnode->Set_OPND((CODEREP*) cvor, FALSE);

  VSA_STATS_inc(vo_mu);
  Is_Trace(Tracing(), (TFile, "Create_stmt_vsym_mu creates "));
  Is_Trace_cmd(Tracing(), vor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " based on "));
  Is_Trace_cmd(Tracing(), vor->Vsym_obj()->Base_hor()->Heap_obj()->Print(TFile));
  Is_Trace(Tracing(), (TFile, "\n"));
  return mnode;
}

// =============================================================================
// Append_stmt_vsym_chi
// =============================================================================
CHI_NODE*
VSA::Create_stmt_vsym_chi(CODEREP *cr, STMTREP *stmt, VSYM_OBJ_REP *vor, MEM_POOL *def_bbs_pool)
{
  Is_True(!Is_special_vor(vor), ("no chi for special vor"));
  Is_True(cr->Kind() != CK_IVAR || cr->Opr() != OPR_PARM, ("hit PARM"));
  CHI_NODE *cnode = CXX_NEW(CHI_NODE, Mem_pool());
  CVOR     *cvor = CXX_NEW(CVOR(vor, cr), Mem_pool());
  cnode->Set_OPND((CODEREP*) cvor, FALSE);
  vor = Clone_vsym_obj(vor, stmt->Bb(), def_bbs_pool);
  vor->Set_attr(ROR_DEF_BY_CHI);
  vor->Set_stmt_def(stmt, Comp_unit()->Dna());
  cvor = CXX_NEW(CVOR(vor, cr), Mem_pool());
  cnode->Set_RESULT((CODEREP*) cvor);
  cnode->Set_live(TRUE);

  VSA_STATS_inc(vo_chi);
  Is_Trace(Tracing(), (TFile, "Create_stmt_vsym_chi creates "));
  Is_Trace_cmd(Tracing(), vor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " based on "));
  Is_Trace_cmd(Tracing(), vor->Vsym_obj()->Base_hor()->Heap_obj()->Print(TFile));
  Is_Trace(Tracing(), (TFile, "\n"));
  return cnode;
}

// =============================================================================
// Rename_entry_vsym_chi
// =============================================================================
void
VSA::Rename_entry_vsym_chi(STMTREP *stmt, MEM_POOL *pool, BOOL fwd)
{
  Is_True(stmt->Opr() == OPR_OPT_CHI, ("not entry chi stmt"));
  CHI_LIST *chi_list = Stmt_vor_chi(stmt);
  Is_True(chi_list != NULL, ("no chi list for entry chi"));

  CHI_NODE* cnode;
  CHI_LIST_ITER chi_iter;
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    if (!cnode->Live())
      continue;
    HEAP_OBJ_REP *hor = Cr_2_heap_obj(cnode->RESULT());
    if (hor != NULL) {
      if (fwd) {
        hor->Gen_name(stmt);
        Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Rename_hochi: "));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in the stmt:\n"));
        Is_Trace_cmd(Tracing(), stmt->Print(TFile));
      }
      else {
        if (hor->Heap_obj()->Top_match_sr(stmt))
          hor->Heap_obj()->Pop();
      }
    }
  }

  if (!fwd)
    return;

  VSYM_OBJ* vobj;
  VO_LIST_ITER vo_iter;
  FOR_ALL_ELEM(vobj, vo_iter, Init(Vsym_obj_list())) {
    HEAP_OBJ* ho = vobj->Base_hor()->Heap_obj();
    if (ho->Kind() != RSC_KIND_IPARM)
      continue;
    if (ho->Sym_id() == ILLEGAL_AUX_ID)
      continue;
    CODEREP* cr = Comp_unit()->Dna()->Find_param_cr(ho->Sym_id());
    if (cr == NULL) {
      // not a iparam, check if it's global var, or struct param
      AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(ho->Sym_id());
      if (aux_entry->St() == NULL ||
          ST_sclass(aux_entry->St()) == SCLASS_AUTO ||
          ST_sclass(aux_entry->St()) == SCLASS_REG)
        continue;
      if (ST_sclass(aux_entry->St()) == SCLASS_FORMAL ||
          ST_sclass(aux_entry->St()) == SCLASS_FORMAL_REF) {
        // struct param
        Is_True(ST_mtype(aux_entry->St()) == MTYPE_M, ("should be struct type"));
        cr = Comp_unit()->Dna()->Find_st_param(ST_st_idx(aux_entry->St()));
      }
      else {
        // global var
        CHI_NODE *cnode;
        CHI_LIST_ITER chi_iter;
        FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
          if (cnode->Live() && cnode->Aux_id() == ho->Sym_id()) {
            cr = cnode->RESULT();
            break;
          }
        }
      }
      if (cr == NULL)
        continue;
    }
    HEAP_OBJ_REP* hor = ho->Top_of_stack();
    // update with the new hor which is def by OPT_CHI
    //vobj->Set_base_hor(hor);
    VSYM_OBJ_REP* vor = Find_vor_chi_vor(stmt, cr, vobj->Fld_rep_ptr());
    if (vor != NULL) {
      // not set when the vor is created in Create_refn_vsym
      vor->Set_stmt_def(stmt, Comp_unit()->Dna());
      continue;
    }
    vor = Allocate_vsym_obj(vobj);
    vor->Set_attr(ROR_DEF_BY_CHI);
    CHI_NODE* cnode = Create_stmt_vsym_chi(cr, stmt, vor, pool);
    chi_list->Append(cnode);
  }
  Is_Trace_cmd(Tracing(), Print_sr(stmt, TFile));
}

// =============================================================================
// Add_field_obj
// =============================================================================
template<typename T> void
VSA::Add_field_heap_obj(HEAP_OBJ_REP *hor, hash_map<uintptr_t, T>& ho_map, const T& value)
{
  FIELD_OBJ_REP* fl = hor->Flist();
  while (fl != NULL) {
    HEAP_OBJ_REP* fobj = fl->Hor();
    HEAP_OBJ_REP* ftop = fobj->Heap_obj()->Top_of_stack();
    if (fobj != ftop)
      fl->Set_hor(ftop);
    uintptr_t key = (uintptr_t)ftop; // fobj->Heap_obj();
    if (ho_map.find(key) == ho_map.end()) {
      ho_map.insert(make_pair(key, value));     // add ho to ho_map
      Add_field_heap_obj(ftop, ho_map, value);  // add fields recursively
    }
    fl = fl->Next();
  }
}

// =============================================================================
// Create_stmt_vsym_mu_chi
// =============================================================================
void
VSA::Create_stmt_vsym_mu_chi(CODEREP *cr, STMTREP* stmt, BOOL chi, BOOL mu, MEM_POOL *pool)
{
  CODEREP* base = Find_ilod_base(cr);
  if (base != NULL) {
    HEAP_OBJ_REP *base_hor = Find_cr_heap_obj(base);
    HEAP_OBJ_REP *hor = (base_hor)? base_hor->Heap_obj()->Top_of_stack():NULL;
    if (hor) {
      VSYM_FLD_REP vfr(FLD_K_ID, Cr_fldid(base), Cr_ofst(base));
      VSYM_OBJ *vsym_obj = Find(hor, &vfr);
      VSYM_OBJ_REP* vor = NULL;
      if (vsym_obj)
        vor = vsym_obj->Top_of_stack();
      if (vor) {
        if (mu && !Find_vor_mu_cr(stmt, vor)) {
          MU_LIST *mu_list = Stmt_vor_mu(stmt);
          Is_True(mu_list != NULL, ("no vo mu list for call"));
          MU_NODE* mnode = Create_stmt_vsym_mu(base, stmt, vor);
          mu_list->Append(mnode);
        }
        if (chi && !Find_vor_chi_cr(stmt, vor)) {
          CHI_LIST *chi_list = Stmt_vor_chi(stmt);
          Is_True(chi_list != NULL, ("no vo chi list for call"));
          CHI_NODE* cnode = Create_stmt_vsym_chi(base, stmt, vor, pool);
          chi_list->Append(cnode);
        }
      }
    }
  }
}

// =============================================================================
// Create_call_vsym_mu_chi
// =============================================================================
void
VSA::Create_call_vsym_mu_chi(STMTREP *call_stmt, MEM_POOL *def_bbs_pool)
{
  BOOL create_chi = TRUE;
  BOOL intrn_not_pure = FALSE;
  INTRINSIC intrn = Get_call_intrinsic(call_stmt);
  if (intrn != INTRINSIC_NONE) {
    if (INTRN_is_pure(intrn) ||
        INTRN_has_no_side_effects(intrn))
      create_chi = FALSE;
    else
      intrn_not_pure = TRUE;
  }
  else if (call_stmt->Opr() == OPR_CALL) {
    const PU& pu = Pu_Table[ST_pu(call_stmt->St())];
    if (PU_is_pure(pu) ||
        PU_no_side_effects(pu) ||
        PU_has_attr_pure(pu))
      create_chi = FALSE;
  }

  // create vsym mu/chi for actuals and global vars on mu/chi list
  hash_map<uintptr_t, std::pair<CODEREP*, CODEREP*> > ho_map;
  HEAP_OBJ_REP *hor = NULL;

  for (INT actual = 0; actual < call_stmt->Rhs()->Kid_count(); ++ actual) {
    if (call_stmt->Opr() == OPR_ICALL && actual == call_stmt->Rhs()->Kid_count() - 1)
      break;
    Is_True(call_stmt->Rhs()->Opnd(actual)->Kind() == CK_IVAR &&
            call_stmt->Rhs()->Opnd(actual)->Opr() == OPR_PARM, ("wrong param cr"));
    CODEREP* opnd = call_stmt->Rhs()->Opnd(actual)->Ilod_base();
    if (opnd->Kind() != CK_LDA && TY_kind(opnd->object_ty()) != KIND_POINTER)
      continue;
    hor = Cr_2_heap_obj(opnd);
    if (hor != NULL) {
      hor = hor->Heap_obj()->Top_of_stack();
      // assume intrn that is not pure only has side effect on first parameter
      CODEREP* res = (intrn_not_pure && actual > 0) ? NULL : opnd;
      ho_map.insert(std::make_pair((uintptr_t)hor/*->Heap_obj()*/,   // insert may fail
                                   std::make_pair(opnd, res)));
      Add_field_heap_obj(hor, ho_map, std::make_pair(opnd, res));
    }
  }

  CHI_LIST *var_chi_list = call_stmt->Chi_list();
  if (var_chi_list != NULL && !var_chi_list->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(var_chi_list)) {
      if (!cnode->Live())
        continue;
      if (cnode->Aux_id() == Opt_stab()->Default_vsym() ||
          cnode->Aux_id() == Opt_stab()->Return_vsym())
        continue;
      AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(cnode->Aux_id());
      ST* st = aux_entry->St();
      // do not propagate hor for return preg, different return preg cr with same
      // aux id are actully differnt things
      if (st == NULL || aux_entry->Is_return_preg())
        continue;
      if ((ST_sclass(st) == SCLASS_AUTO ||
           ST_sclass(st) == SCLASS_FORMAL ||
           ST_sclass(st) == SCLASS_FORMAL_REF) &&
          !ST_addr_passed(st))
        continue;
      hor = Cr_2_heap_obj(cnode->OPND());
      if (hor == NULL)
        hor = Cr_2_heap_obj(cnode->RESULT());
      if (hor != NULL) {
        hor = hor->Heap_obj()->Top_of_stack();
        ho_map.insert(std::make_pair((uintptr_t)hor/*->Heap_obj()*/,
                                     std::make_pair(cnode->OPND(), cnode->RESULT())));
        Add_field_heap_obj(hor, ho_map, std::make_pair(cnode->OPND(), cnode->RESULT()));
      }
    }
  }

  MU_LIST *var_mu_list = call_stmt->Mu_list();
  if (var_mu_list != NULL && !var_mu_list->Is_Empty()) {
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(var_mu_list)) {
      if (mnode->Aux_id() == Opt_stab()->Default_vsym() ||
          mnode->Aux_id() == Opt_stab()->Return_vsym())
        continue;
      AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(mnode->Aux_id());
      ST* st = aux_entry->St();
      if (st == NULL)
        continue;
      if ((ST_sclass(st) == SCLASS_AUTO ||
           ST_sclass(st) == SCLASS_FORMAL ||
           ST_sclass(st) == SCLASS_FORMAL_REF) &&
          !ST_addr_passed(st))
        continue;
      hor = Cr_2_heap_obj(mnode->OPND());
      if (hor != NULL) {
        hor = hor->Heap_obj()->Top_of_stack();
        ho_map.insert(std::make_pair((uintptr_t)hor/*->Heap_obj()*/, // insert may fail
                                     std::make_pair(mnode->OPND(), (CODEREP*)NULL)));
        Add_field_heap_obj(hor, ho_map, std::make_pair(mnode->OPND(), (CODEREP*)NULL));
      }
    }
  }

  // create vsym chi/mu for hor referenced in this call
  MU_LIST *mu_list = Stmt_vor_mu(call_stmt);
  Is_True(mu_list != NULL, ("no vo mu list for call"));
  CHI_LIST *chi_list = Stmt_vor_chi(call_stmt);
  Is_True(chi_list != NULL, ("no vo chi list for call"));
  VSYM_OBJ* vobj;
  VO_LIST_ITER vo_iter;
  FOR_ALL_ELEM(vobj, vo_iter, Init(Vsym_obj_list())) {
    hor = vobj->Base_hor();
    hash_map<uintptr_t, std::pair<CODEREP*, CODEREP*> >::iterator iter = ho_map.find((uintptr_t)hor);
    if (iter != ho_map.end()) {

      VSYM_OBJ_REP *vor = vobj->Top_of_stack();
      CODEREP* opnd = iter->second.first;
      MU_NODE* mnode = Create_stmt_vsym_mu(opnd, call_stmt, vor);
      mu_list->Append(mnode);
      CODEREP* res = iter->second.second;
      if (create_chi && res != NULL) {
        VSYM_OBJ_REP *found_vor = Find_vor_chi_vor(call_stmt, res, vobj->Fld_rep_ptr());
        if (!found_vor) {
          CHI_NODE* cnode = Create_stmt_vsym_chi(res, call_stmt, vor, def_bbs_pool);
          chi_list->Append(cnode);
        }
      }
    }
  }

  // create vsym chi for return value
  CODEREP* call_return_store = Comp_unit()->Find_return_value(call_stmt);
  if (call_return_store != NULL && call_return_store->Kind() == CK_VAR &&
      TY_kind(call_return_store->Lod_ty()) == KIND_POINTER) {
    hor = Cr_2_heap_obj(call_return_store);
    if (hor != NULL && hor->Attr() == ROR_DEF_BY_CHI &&
        hor->Stmt_def() == call_stmt) {
      hor->Heap_obj()->Push(hor, call_stmt);
      if (ho_map.find((uintptr_t)hor/*->Heap_obj()*/) == ho_map.end()) {
        // inserted chi node for this ho
        CHI_LIST *chi_list = Stmt_vor_chi(call_stmt);
        Is_True(chi_list != NULL, ("no vo chi list for call"));
        VSYM_OBJ* vobj;
        VO_LIST_ITER vo_iter;
        FOR_ALL_ELEM(vobj, vo_iter, Init(Vsym_obj_list())) {
          if (vobj->Base_hor()->Heap_obj() == hor->Heap_obj()) {
            VSYM_OBJ_REP *vor = vobj->Top_of_stack();
            CHI_NODE* cnode = Create_stmt_vsym_chi(call_return_store, call_stmt,
                                                   vor, def_bbs_pool);
            chi_list->Append(cnode);
          }
        }
      }
    } else if (hor != NULL && hor->Attr() == ROR_DEF_BY_CHI) {
      DNA_NODE *dna = Comp_unit()->Dna();
      RNA_NODE *rna = dna->Get_callsite_rna(call_stmt);
      for (INT32 i = 0; i < call_stmt->Rhs()->Kid_count(); i++) {
        if (call_stmt->Opr() == OPR_ICALL && i == call_stmt->Rhs()->Kid_count() - 1)
          break;
        CODEREP *opcr = call_stmt->Rhs()->Opnd(i);
        HEAP_OBJ_REP *parm_hor = Cr_2_heap_obj(opcr->Ilod_base());
        if (parm_hor && parm_hor->Heap_obj() == hor->Heap_obj()) {
          IDTYPE param = rna->Get_arg_with_cr(opcr->Ilod_base());
          if (param != INVALID_VAR_IDX && rna->Is_set_arg_flag(param, REF_ISTORE)) {
            hor->Heap_obj()->Push(hor, call_stmt);
            // inserted chi node for this ho, even though ho had in map, add all related vsym to chi list
            CHI_LIST *chi_list = Stmt_vor_chi(call_stmt);
            Is_True(chi_list != NULL, ("no vo chi list for call"));
            VSYM_OBJ* vobj;
            VO_LIST_ITER vo_iter;
            FOR_ALL_ELEM(vobj, vo_iter, Init(Vsym_obj_list())) {
              if (vobj->Base_hor()->Heap_obj() == hor->Heap_obj()) {
                VSYM_OBJ_REP *vor = vobj->Top_of_stack();
                CHI_NODE* cnode = Create_stmt_vsym_chi(call_return_store, call_stmt,
                                                      vor, def_bbs_pool);
                chi_list->Append(cnode);
              }
            }
          }
        }
      }
    }
  }
}

// =============================================================================
// Create_return_vsym_mu
// =============================================================================
void
VSA::Create_return_vsym_mu(STMTREP *stmt, MEM_POOL *pool)
{
  Is_True(stmt->Opr() == OPR_RETURN || stmt->Opr() == OPR_RETURN_VAL,
          ("return stmt expected"));
  HOR_ARRAY* array = _ret_2_hor_array_map.Lookup(stmt->Bb()->Id());
  if (array != NULL && array->size() > 0) {
    for (INT i = 0; i < array->size(); ++i) {
      HEAP_OBJ_REP *hor = (*array)[i];
      HEAP_OBJ_REP *tos = hor->Heap_obj()->Top_of_stack();
      if (hor->Heap_obj()->Kind() == RSC_KIND_ALLOC &&
          !hor->Escaped() && tos->Escaped()) {
        // escaped via vsym
        hor->Set_escaped(TRUE);
        Is_Trace(Tracing(), (TFile, "HOR-ESC on return "));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " for escaped TOS "));
        Is_Trace_cmd(Tracing(), tos->Print(TFile));
        Is_Trace(Tracing(), (TFile, "\n"));
      }
      if (hor->Attr() != ROR_DEF_BY_FREE &&
          tos->Attr() == ROR_DEF_BY_FREE)
        // free via vsym
        (*array)[i] = tos;
    }
  }

  // find ho referenced by return mu and return value
  hash_map<uintptr_t, CODEREP*> ho_map;

  // ho referenced on return value
  STMTREP* prev = stmt->Prev();
  if (prev != NULL && OPERATOR_is_scalar_store(prev->Opr()) &&
      Opt_stab()->Aux_stab_entry(prev->Lhs()->Aux_id())->Is_dedicated_preg()) {
    HEAP_OBJ_REP* hor = Cr_2_heap_obj(prev->Lhs());
    if (hor != NULL) {
      hor = hor->Heap_obj()->Top_of_stack();
      ho_map.insert(std::make_pair((uintptr_t)hor->Heap_obj(), prev->Lhs()));
      Add_field_heap_obj(hor, ho_map, prev->Lhs());
    }
  }

  // ho referenced on return mu
  MU_LIST *var_mu_list = stmt->Mu_list();
  if (var_mu_list != NULL && !var_mu_list->Is_Empty()) {
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(var_mu_list)) {
      if (mnode->Aux_id() == Opt_stab()->Default_vsym() ||
          mnode->Aux_id() == Opt_stab()->Return_vsym())
        continue;
      AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(mnode->Aux_id());
      if (aux_entry->St() == NULL ||
          ST_sclass(aux_entry->St()) == SCLASS_REG ||
          ST_sclass(aux_entry->St()) == SCLASS_AUTO)
        continue;
      if ((ST_sclass(aux_entry->St()) == SCLASS_FORMAL ||
           ST_sclass(aux_entry->St()) == SCLASS_FORMAL_REF) &&
          TY_kind(aux_entry->Ty()) != KIND_POINTER)
        continue;
      HEAP_OBJ_REP* hor = Cr_2_heap_obj(mnode->OPND());
      if (hor != NULL) {
        hor = hor->Heap_obj()->Top_of_stack();
        ho_map.insert(std::make_pair((uintptr_t)hor->Heap_obj(), mnode->OPND()));
        Add_field_heap_obj(hor, ho_map, mnode->OPND());
      }
    }
  }

  // ho referenced on return bb
  for (INT i = 0; i < array->size(); ++i) {
    HEAP_OBJ_REP *hor = (*array)[i];
    HEAP_OBJ* ho = hor->Heap_obj();
    if ((ho->Kind() != RSC_KIND_IPARM &&
         ho->Kind() != RSC_KIND_ALLOC) ||
        ho->Sym_id() == ILLEGAL_AUX_ID)
      continue;
    AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(ho->Sym_id());
    if (aux_entry->St() == NULL ||
        TY_kind(aux_entry->Ty()) != KIND_POINTER ||
        ST_sclass(aux_entry->St()) == SCLASS_REG ||
        ST_sclass(aux_entry->St()) == SCLASS_AUTO)
      continue;
    if (ho_map.find((uintptr_t)ho) != ho_map.end())
      continue;
    HEAP_OBJ_REP* tos = ho->Top_of_stack();
    if (ST_sclass(aux_entry->St()) == SCLASS_FORMAL) {
      CODEREP* parm = Comp_unit()->Dna()->Find_st_param(ST_st_idx(aux_entry->St()));
      if (parm != NULL)
        ho_map.insert(std::make_pair((uintptr_t)ho, parm));
    }
    else if (tos->Attr() == ROR_DEF_BY_CHI && tos->Stmt_def() != NULL &&
        tos->Stmt_def()->Opr() == OPR_OPT_CHI) {
      CHI_NODE* chi = tos->Stmt_def()->Chi_list()->Search_chi_node(ho->Sym_id());
      Is_True(chi != NULL, ("not find chi node"));
      if (chi != NULL)
        ho_map.insert(std::make_pair((uintptr_t)ho, chi->RESULT()));
    }
  }

  // create vsym mu for all ho referenced by return
  MU_LIST *mu_list = Stmt_vor_mu(stmt);
  Is_True(mu_list != NULL, ("no vo mu list for call"));

  MU_LIST_ITER mu_iter;
  MU_NODE *mnode;
  FOR_ALL_NODE( mnode, mu_iter, Init(mu_list)) {
    CVOR *cvor = (CVOR*)mnode->OPND();
    VSYM_OBJ_REP *vor = cvor->first;
    CODEREP *cr = cvor->second;
    // add vsym mu ho to map, so that all vsym based on the ho canbe added
    // to return vor mu list
    ho_map.insert(std::make_pair((uintptr_t)vor->Vsym_obj()->Base_hor()->Heap_obj(), cr));
  }

  VSYM_OBJ* vobj;
  VO_LIST_ITER vo_iter;
  FOR_ALL_ELEM(vobj, vo_iter, Init(Vsym_obj_list())) {
    HEAP_OBJ* ho = vobj->Base_hor()->Heap_obj();
    hash_map<uintptr_t, CODEREP*>::iterator it = ho_map.find((uintptr_t)ho);
    VSYM_OBJ_REP *vor = vobj->Top_of_stack();
    if (it != ho_map.end() && !Find_vor_mu_cr(stmt, vor)) {
      CODEREP* opnd = it->second;
      MU_NODE* mnode = Create_stmt_vsym_mu(opnd, stmt, vor);
      mu_list->Append(mnode);
    }
  }
}

// =============================================================================
//
//   Create refined vsym in chi list of a function.
//   Note: we do not need this for ISTORE since we have lumped aliased pointers
//         into one heap_obj and take care of it at the statement level
//
// =============================================================================
void
VSA::Create_refn_vsym_chi(CODEREP *cr, STMTREP *stmt, HEAP_OBJ_REP *hor,
                          IDTYPE fldid, mINT32 ofst, MEM_POOL *pool, VS_FLD_KIND fld_k)
{
  if (hor == Null_hor())
    return;
  BB_NODE  *bb = stmt->Bb();

  HEAP_OBJ_REP *param_hor = Cr_2_heap_obj(cr);
  // Is_True(param_hor == NULL, ("Create_refn_vsym_chi has already called"));

  // allow same cr chi created twice(jni need vsym chi created on same heap obj)
  // if (param_hor != NULL) return;

  if (stmt->Opr() == OPR_OPT_CHI) {
    hor->Heap_obj()->Set_kind(RSC_KIND_IPARM);
    Is_True(hor->Heap_obj()->Sym_id() == cr->Aux_id(), ("aux id mismatch"));
  }
  if (param_hor == NULL)
    Enter_cr_heap_obj_map(cr, hor);
  VSYM_OBJ_REP *vor = Allocate_vsym_obj(bb, hor, fldid, ofst, pool, fld_k);
  vor->Set_attr(ROR_DEF_BY_CHI);
  CHI_LIST *chi_list = Stmt_vor_chi(stmt);
  CHI_NODE *cnode = Create_stmt_vsym_chi(cr, stmt, vor, pool);
  chi_list->Append(cnode);
#if 0
  CHI_NODE *cnode = CXX_NEW(CHI_NODE, Mem_pool());
  CVOR     *cvor = CXX_NEW(CVOR(vor, cr), Mem_pool());

#if 0
  printf("memory content in cvor->first after ctor : 0x%llx\n", cvor->first);
  printf("memory content in cvor->second after ctor : 0x%llx\n", cvor->second);
#endif
  cnode->Set_OPND((CODEREP*) cvor, FALSE);
#if 0
  printf("memory content cvor->first after cnode Set_OPND : 0x%llx\n", cvor->first);
  printf("memory content cvor->second after cnode Set_OPND : 0x%llx\n", cvor->second);
#endif
  // the chi node will hold a new vor/cvor
  vor = Clone_vsym_obj(vor, bb, pool);
  vor->Set_attr(ROR_DEF_BY_CHI);
  vor->Set_stmt_def(stmt, Comp_unit()->Dna());
  cvor = CXX_NEW(CVOR(vor, cr), Mem_pool());
  cnode->Set_RESULT((CODEREP*) cvor);
  chi_list->Append(cnode);

  Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d VSA::Create_refn_vsym_chi create ",
                       Srcpos_To_Line(stmt->Linenum())));
  Is_Trace_cmd(Tracing(), vor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " based on "));
  Is_Trace_cmd(Tracing(), hor->Print(TFile));
  Is_Trace(Tracing(), (TFile, "\n"));
#endif
}


// =============================================================================
//
//   Create_refn_vsym traverse coderep, identify and create refined_vsym
//
// =============================================================================
void
VSA::Create_refn_vsym(CODEREP *cr, STMTREP *stmt, MEM_POOL *def_bbs_pool, BOOL handle_call)
{
  CODEREP* base;
  HEAP_OBJ_REP *base_hor;
  BB_NODE *bb = stmt->Bb();

  switch (cr->Kind()) {
  case CK_IVAR:

    if (cr->Opr() == OPR_PARM) {
      base = cr->Ilod_base();
    }
    else {
      base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
    }

    Create_refn_vsym(base, stmt, def_bbs_pool);

    // refined vsym handling
    base_hor = Find_cr_heap_obj(base);

    if (base_hor) {
      //no rename here, Top_of_stack invalid???
      base_hor = base_hor->Heap_obj()->Top_of_stack();

      Is_Trace_cmd(Tracing(), cr->Print(0, TFile));

      UINT ifldid = cr->I_field_id();
      mINT32 ofst = Cr_ofst(cr);

      if (cr->Opr() == OPR_PARM && OPERATOR_is_call(stmt->Opr())) {
        // discard handle_call, always create refn vsym after heap object rename is done.
        //if (! handle_call) break;
        DNA_NODE *caller = Comp_unit()->Dna();
        RNA_NODE *rna = caller->Get_callsite_rna(stmt);
        IDTYPE    param = rna->Get_arg_with_cr(cr->Ilod_base());
        if(param != INVALID_VAR_IDX) {
          VS_FLD_KIND fld_kind = FLD_K_ID;
          if (rna->Is_set_arg_flag(param, REF_BASE)) {
            VS_FLD_KIND kind = Find_fld_name(rna, ifldid);
            fld_kind = kind == FLD_INVALID ? fld_kind : kind;
          }
          // create MU for OPR_PARM that references a pointer that has hor.
          // we use a different object that contains a pair (vor, param)
          // and stash inside the mu-list { mu-node,...} and attach to the
          // call statement.
          BOOL      parm_ref = rna->Is_set_arg_flag(param, REF_ILOAD);
          if (parm_ref) {
            Create_refn_vsym_mu(cr->Ilod_base(), stmt, base_hor, ifldid, ofst, def_bbs_pool, fld_kind);
          }

          // create chi_list
          BOOL      parm_mod = rna->Is_set_arg_flag(param, REF_ISTORE);
          if (parm_mod) {
            Create_refn_vsym_chi(cr->Ilod_base(), stmt, base_hor, ifldid, ofst, def_bbs_pool, fld_kind);
          }

          // For LDA paramm find it's non-lda HEAP_OBJ and create vsym based on it
          if (base->Kind() == CK_LDA &&
              TY_kind(ST_type(base->Lda_base_st())) != KIND_ARRAY) {
            HEAP_OBJ* ldid_ho = Find(base->Lda_aux_id(), FALSE);
            if (ldid_ho != NULL && ldid_ho != base_hor->Heap_obj()) {
              HEAP_OBJ_REP* ldid_hor = ldid_ho->Top_of_stack();
              MU_NODE* mu = stmt->Mu_list() ? stmt->Mu_list()->Search_mu_node(base->Lda_aux_id())
                                            : NULL;
              CODEREP* ldid = mu ? mu->OPND() : base;
              if (parm_ref)
                Create_refn_vsym_mu(ldid, stmt, ldid_hor, 0, 0, def_bbs_pool, FLD_K_ID);
              if (parm_mod)
                Create_refn_vsym_chi(ldid, stmt, ldid_hor, 0, 0, def_bbs_pool, FLD_K_ID);
            }
          }
        } else {
          Is_Trace(Tracing(), (TFile, "cr%d is not in rna arg list\n", 
                                      cr->Ilod_base()->Coderep_id()));
        }
      } else {
        //ILOAD is a reference, should not trigger phi insertion, unless passed
        // as the argument to the free()
        VSYM_OBJ_REP *vor;
        if(Is_ivar_need_vsym(cr, stmt)) {
          if (OPERATOR_is_call(stmt->Opr()) && Callee_frees_heap_memory(stmt) != NULL)
            vor = Allocate_vsym_obj(bb, base_hor, ifldid, ofst, def_bbs_pool);
          else
            vor = Allocate_vsym_obj(NULL, base_hor, ifldid, ofst, def_bbs_pool);
          if (Cr_2_vor(cr) == NULL) {
            Enter_cr_vor_map(cr, vor);
          }
          else {
            MU_LIST* mu_list = Stmt_vor_mu(stmt);
            if (mu_list != NULL) {
              MU_NODE* mnode = Create_stmt_vsym_mu(cr, stmt, vor);
              mu_list->Append(mnode);
            }
          }
          Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d VSA::Create_refn_vsym ILOAD, create ",
                              (INT32)stmt->Linenum()));
          Is_Trace_cmd(Tracing(), vor->Print(TFile));
          Is_Trace(Tracing(), (TFile, " based on "));
          Is_Trace_cmd(Tracing(), base_hor->Print(TFile));
          Is_Trace(Tracing(), (TFile, "\n"));
        }
      }
    }
    break;

  case CK_OP:
    if (cr->Opr() == OPR_CVT)
      Create_refn_vsym(cr->Opnd(0), stmt, def_bbs_pool, handle_call);
    else {
      for (INT32 i=0; i<cr->Kid_count(); i++)        {
        Create_refn_vsym(cr->Opnd(i), stmt, def_bbs_pool, handle_call);
      }
    }
    break;
  default:
    break;
  }
}


// =============================================================================
//
//   Create_refn_vsym creates refined vsym based on the heap_obj_rep after
//         heap_obj has completed rename.  It uses the stack to keep track of
//         current version of heap_obj.
//
//         It intends to cover ILOAD/ISTORE and pointers passed to callee such
//         that we build the complete SSA against ILOAD/ISTORE.
//
// =============================================================================
void
VSA::Create_refn_vsym(BB_NODE *bb, MEM_POOL *def_bbs_pool)
{
  PHI_LIST_ITER phi_iter;
  PHI_NODE     *phi;
  BB_LIST_ITER  bb_iter;
  INT32         pos;
  BB_NODE      *succ, *dom_bb;
  BB_LIST_ITER  dom_bb_iter;
  HEAP_OBJ_REP *hor;
  MU_LIST_ITER  mu_iter;
  MU_NODE      *mnode;
  MU_LIST      *mu_list;
  CHI_LIST_ITER chi_iter;
  CHI_NODE     *cnode;
  CHI_LIST     *chi_list;

  // enable only if -ALIAS:restricted=on till we handle the forward analysis
  // if (!  Alias_Pointer_Restricted) return;

  if (Tracing())
    fprintf( TFile, "---- VSA::Create refined vsym for BB%d\n", bb->Id() );

  //  Iterate through each phi-node to push the heap_obj_rep onto stack
  FOR_ALL_ELEM (phi, phi_iter, Init(Bb_ho_philist(bb))) {
    hor = (HEAP_OBJ_REP*)phi->RESULT();
    if (hor)
      hor->Heap_obj()->Push(hor, NULL);
  }

  //  Iterate through each statement
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {

    // Nothing to process? mu_list for the statement
    mu_list = Stmt_vor_mu(stmt);
    FOR_ALL_NODE( mnode, mu_iter, Init(mu_list)) {
      // TODO: when we iterate the VOR creation due to *p = malloc();
      CVOR *cvor = (CVOR*)mnode->OPND();
      VSYM_OBJ_REP *vor = cvor->first;
    }

    OPERATOR opr = stmt->Opr();

    // Process Rhs() of all statement

    if (stmt->Rhs())
      Create_refn_vsym(stmt->Rhs(), stmt, def_bbs_pool);

    CODEREP *cr;
    CODEREP *base;

    // Process heap obj in calls
    if ( OPERATOR_is_call(opr) ) {
      HEAPSTATE heapstate;
      if ((heapstate = Callee_returns_new_heap_memory(stmt)) ||
          Callee_frees_heap_memory(stmt) != NULL) {
        hor = Cr_2_heap_obj(stmt->Rhs());

        // for realloc
        HEAP_OBJ_REP* hor_ref = Cr_2_heap_obj_ref(stmt->Rhs());
        if (hor_ref != NULL && hor_ref != Null_hor() &&
            (hor == NULL || hor_ref->Heap_obj() != hor->Heap_obj())){
          hor_ref->Heap_obj()->Push(hor_ref, stmt);
        }
        if (hor != NULL && hor != Null_hor()) {
          hor->Heap_obj()->Push(hor, stmt);
          if (VSA_Vsym_Memcall() /* && Opt_stab()->Addr_saved(hor->Heap_obj()->Sym_id()) */) {
            // need this vor for UIV on heap objects
            VSYM_OBJ_REP *vor = Allocate_vsym_obj(bb, hor, 0, 0, def_bbs_pool);
            vor->Set_srcpos_node(stmt, Comp_unit()->Dna(), PATHINFO_COPY);
            vor->Set_attr(ROR_DEF_BY_CHI);
            vor->Set_stmt_def(stmt, Comp_unit()->Dna());
            vor->Set_hor(hor);
            Enter_cr_vor_map(stmt->Rhs(), vor);
            Is_Trace_cmd(Tracing(), stmt->Rhs()->Print(0, TFile));
            Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d VSA::Create_refn_vsym CALL, create ",
                                 Srcpos_To_Line(stmt->Linenum())));
            Is_Trace_cmd(Tracing(), vor->Print(TFile));
            Is_Trace(Tracing(), (TFile, " based on "));
            Is_Trace_cmd(Tracing(), hor->Print(TFile));
            Is_Trace(Tracing(), (TFile, "\n"));
          }
        }
      }
      else {
        // check return value if it's a pointer
        CODEREP* call_return_store = Comp_unit()->Find_return_value(stmt);
        if (call_return_store != NULL &&
            call_return_store->Kind() == CK_VAR &&
            TY_kind(call_return_store->Lod_ty()) == KIND_POINTER) {
          HEAP_OBJ_REP* hor = Cr_2_heap_obj(call_return_store);
          if (hor != NULL && hor->Attr() == ROR_DEF_BY_CHI &&
              hor->Stmt_def() == stmt)
            hor->Heap_obj()->Push(hor, stmt);
        }
      }

      // strlen
      INTRINSIC intrn = Get_call_intrinsic(stmt);
      if (intrn == INTRN_STRLEN) {
        CODEREP* str = stmt->Rhs()->Opnd(0)->Ilod_base();
        Create_stmt_vsym_mu_chi(str, stmt, FALSE, TRUE, def_bbs_pool);
      }
      // memcpy/strcpy
      else if (intrn == INTRN_MEMCPY || intrn == INTRN_STRCPY) {
        // dest
        CODEREP* dest = stmt->Rhs()->Opnd(0)->Ilod_base();
        Create_stmt_vsym_mu_chi(dest, stmt, TRUE, FALSE, def_bbs_pool);
        // src
        CODEREP* src = stmt->Rhs()->Opnd(1)->Ilod_base();
        Create_stmt_vsym_mu_chi(src, stmt, FALSE, TRUE, def_bbs_pool);
      }

    } else if (opr == OPR_STID) {
      cr = stmt->Lhs();
      hor = Cr_2_heap_obj(cr);
      VSYM_OBJ_REP *vor = Cr_2_vor(cr);
      if (hor != NULL && hor->Is_entry_chi())
        hor->Heap_obj()->Push(hor, stmt);
      if (hor != NULL && vor != NULL &&
          Opt_stab()->Addr_saved(cr->Aux_id())) {
        UINT ifldid = cr->Field_id();
        vor = Allocate_vsym_obj(bb, hor, ifldid, Cr_ofst(cr), def_bbs_pool);
        vor->Set_srcpos_node(stmt, Comp_unit()->Dna(), PATHINFO_COPY);
        vor->Set_attr(ROR_DEF_BY_COPY);
        vor->Set_stmt_def(stmt, Comp_unit()->Dna());
        if (Cr_2_heap_obj(stmt->Rhs()))
          vor->Set_hor(Cr_2_heap_obj(stmt->Rhs()));
        else
          vor->Set_hor(hor);
        Enter_cr_vor_map(cr, vor);
        Is_Trace_cmd(Tracing(), cr->Print(0, TFile));
        Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d VSA::Create_refn_vsym STID, create ",
                                    Srcpos_To_Line(stmt->Linenum())));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " based on "));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(), (TFile, "\n"));
      }
    } else if (opr == OPR_ISTORE) {
      hor = Cr_2_heap_obj(stmt->Lhs());
      if (hor != NULL && hor->Is_entry_chi())
        hor->Heap_obj()->Push(hor, stmt);

      cr = stmt->Lhs();
      base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
      Create_refn_vsym(base, stmt, def_bbs_pool);

      // refined vsym handling
      HEAP_OBJ_REP *base_hor = Find_cr_heap_obj(base);
      // the base_hor might have already changed if the base pointer is freed
      base_hor = (base_hor)?base_hor->Heap_obj()->Top_of_stack() : NULL;
      INT32 linenum = Srcpos_To_Line(stmt->Linenum());
      if (base_hor) {
        UINT ifldid = cr->I_field_id();
        VSYM_OBJ_REP *vor = Allocate_vsym_obj(bb, base_hor, ifldid, Cr_ofst(cr), def_bbs_pool);
        vor->Set_srcpos_node(stmt, Comp_unit()->Dna(), PATHINFO_ISTORE);
        vor->Set_attr(ROR_DEF_BY_ISTORE);
        vor->Set_stmt_def(stmt, Comp_unit()->Dna());
        vor->Set_hor(hor);
        Enter_cr_vor_map(cr, vor);
        Is_Trace_cmd(Tracing(), cr->Print(0, TFile));
        Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d VSA::Create_refn_vsym ISTORE, create ", linenum));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " based on "));
        Is_Trace_cmd(Tracing(), base_hor->Print(TFile));
        Is_Trace(Tracing(), (TFile, "\n"));
      }
      else {
        Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d VSA::Create_refn_vsym ISTORE, fail to create vsym for\n", linenum));
        Is_Trace_cmd(Tracing(), cr->Print(0, TFile));
      }

      CODEREP* lhs_base = Find_ilod_base(stmt->Lhs()->Istr_base());
      HEAP_OBJ_REP* lhs_hor;
      if (lhs_base != NULL && (lhs_hor = Cr_2_heap_obj(lhs_base)) != NULL) {
        CODEREP* rhs_base = Find_ilod_base(stmt->Rhs());
        HEAP_OBJ_REP* rhs_hor;
        if (rhs_base != NULL && (rhs_hor = Cr_2_heap_obj(rhs_base)) != NULL) {
          FIELD_OBJ_REP* fl = CXX_NEW(FIELD_OBJ_REP(rhs_hor, FLD_K_ID,
                                                    stmt->Lhs()->I_field_id(),
                                                    stmt->Lhs()->Offset()), _mem_pool);
          lhs_hor->Prepend_fl(fl);
        }
      }
    }
    else if (stmt->Opr() == OPR_OPT_CHI) {
      FOR_ALL_NODE (cnode, chi_iter, Init(stmt->Chi_list())) {
        if (!cnode->Live())
          continue;
        HEAP_OBJ_REP *hor = Cr_2_heap_obj(cnode->RESULT());
        // not create vor for entry chi here
        if (hor != NULL)
          hor->Heap_obj()->Push(hor, stmt);
      }
    }

    // Process CHI list here
    chi_list = Stmt_vor_chi(stmt);
    FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
      CVOR *cvor = (CVOR*)cnode->RESULT();
      VSYM_OBJ_REP *vor = cvor->first;

      hor = vor->Vsym_obj()->Base_hor();
      hor->Heap_obj()->Push(hor, stmt);
    }
  }

  FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
    
  }

  FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Create_refn_vsym(dom_bb, def_bbs_pool);  /* child */
  }

  //  The statements are processed in reverse order when poping
  //  rename stack.
  //
  // iterate through each statement in this bb
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {

    OPERATOR opr = stmt->Opr();

    // process CHI list
    chi_list = Stmt_vor_chi(stmt);
    FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
      CVOR *cvor = (CVOR*)cnode->RESULT();
      VSYM_OBJ_REP *vor = cvor->first;

      hor = vor->Vsym_obj()->Base_hor();
      if(hor->Heap_obj()->Top_match_sr(stmt))
        hor->Heap_obj()->Pop();
    }

    // Process heap obj in calls
    if ( OPERATOR_is_call(opr)) {
      // check malloc/free
      if (Callee_returns_new_heap_memory(stmt) ||
          Callee_frees_heap_memory(stmt) != NULL) {
        HEAP_OBJ_REP *hor = Cr_2_heap_obj(stmt->Rhs());
        if (hor != NULL &&
            hor->Heap_obj()->Top_match_sr(stmt)) {
          hor->Heap_obj()->Pop();
        }

        hor = Cr_2_heap_obj_ref(stmt->Rhs());
        if (hor != NULL &&
            hor->Heap_obj()->Top_match_sr(stmt)) {
          hor->Heap_obj()->Pop();
        }
      }
      else {
        // check return value if it's a pointer
        CODEREP* call_return_store = Comp_unit()->Find_return_value(stmt);
        if (call_return_store != NULL &&
            call_return_store->Kind() == CK_VAR &&
            TY_kind(call_return_store->Lod_ty()) == KIND_POINTER) {
          HEAP_OBJ_REP* hor = Cr_2_heap_obj(call_return_store);
          if (hor != NULL &&
              hor->Heap_obj()->Top_match_sr(stmt)) {
            hor->Heap_obj()->Pop();
          }
        }
      }
    } else if (opr == OPR_STID) {
      HEAP_OBJ_REP *hor = Cr_2_heap_obj(stmt->Lhs());
      if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt))
        hor->Heap_obj()->Pop();
    } else if (opr == OPR_ISTORE) {
      HEAP_OBJ_REP *hor = Cr_2_heap_obj(stmt->Lhs());
      if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt))
        hor->Heap_obj()->Pop();
    }
    else if (opr == OPR_OPT_CHI) {
      FOR_ALL_NODE (cnode, chi_iter, Init(stmt->Chi_list())) {
        if (!cnode->Live())
          continue;
        HEAP_OBJ_REP *hor = Cr_2_heap_obj(cnode->RESULT());
        if (hor != NULL && hor->Heap_obj()->Top_match_sr(stmt))
          hor->Heap_obj()->Pop();
      }
    }
  }

  FOR_ALL_ELEM (phi, phi_iter, Init(Bb_ho_philist(bb))) {
    HEAP_OBJ_REP *hor = (HEAP_OBJ_REP *) phi->RESULT();
    hor->Heap_obj()->Pop();
  }
}

// =============================================================================
// CODEREP *Find_vor_mu_cr(STMTREP *stmt, VSYM_OBJ_REP *vor)
// =============================================================================
CODEREP*
VSA::Find_vor_mu_cr(STMTREP *stmt, VSYM_OBJ_REP *vor) const
{
  MU_LIST *mu_list = Stmt_vor_mu(stmt);
  if (mu_list && !mu_list->Is_Empty()) {
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      if (cvor->first == vor)
        return cvor->second;
    }
  }
  return NULL;
}

// =============================================================================
// VSYM_OBJ_REP *Find_vor_mu_vor(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP* vfr)
// =============================================================================
VSYM_OBJ_REP*
VSA::Find_vor_mu_vor(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP* vfr) const
{
  Is_True(vfr, ("null vfr"));
  MU_LIST *mu_list = Stmt_vor_mu(stmt);
  if (vfr && mu_list && !mu_list->Is_Empty()) {
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      if (cvor->second == cr && 
          Vsym_match(stmt, cr, vfr, cvor->first->Vsym_obj()->Fld_rep_ptr()))
        return cvor->first;
    }
  }
  return NULL;
}

// =============================================================================
// VSYM_OBJ_REP *Find_vor_mu_vor(STMTREP *stmt, CODEREP *cr, IDTYPE fldid, mINT32 ofst)
// =============================================================================
VSYM_OBJ_REP *
VSA::Find_vor_mu_vor(STMTREP *stmt, CODEREP *cr, IDTYPE fldid, mINT32 ofst) const
{
  VSYM_FLD_REP vfr(FLD_K_ID, fldid, ofst);
  return Find_vor_mu_vor(stmt, cr, &vfr);
}

// =============================================================================
// VSYM_OBJ_REP *Find_hor_mu_vor(STMTREP *stmt, HEAP_OBJ_REP* hor, VSYM_FLD_REP* vfr)
// =============================================================================
VSYM_OBJ_REP*
VSA::Find_hor_mu_vor(STMTREP *stmt, HEAP_OBJ_REP* hor, VSYM_FLD_REP* vfr, CODEREP *cr) const
{
  Is_True_Ret(vfr, ("null vsym fld rep"), NULL);
  MU_NODE *mnode;
  MU_LIST_ITER mu_iter;
  MU_LIST *mu_list = Stmt_vor_mu(stmt);
  HEAP_OBJ* hobj = hor->Heap_obj();
  if (mu_list && !mu_list->Is_Empty()) {
    FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      if (cvor->first->Vsym_obj()->Base_hor()->Heap_obj() == hobj && 
          Vsym_match(stmt, cr, vfr, cvor->first->Vsym_obj()->Fld_rep_ptr())) {
        return cvor->first;
      }
    }
  }
  FOR_ALL_NODE(mnode, mu_iter, Init(stmt->Mu_list())) {
    CODEREP *mu_cr = mnode->OPND();
    VSYM_OBJ_REP *mu_vor = Cr_2_vor(mu_cr);
    if (mu_vor && mu_vor->Vsym_obj()->Base_hor()->Heap_obj() == hobj &&
        mu_vor->Vsym_obj()->Fld_rep_ptr()->Match(vfr)) {
      return mu_vor;
    }
  }
  return NULL;
}

// =============================================================================
// VSYM_OBJ_REP *Find_hor_mu_vor_any(STMTREP *stmt, HEAP_OBJ_REP* hor,
//                                   VSYM_FLD_REP* vfr, CODEREP *cr, BOOL match_any)
// Match candidate vor mu with following order:
// 1. exact match with given vfr
// 2. any_fld(FLD_K_ANY, vfr->Fld_id_name(), vfr->Ofst());
// 3. any_zero_fld(FLD_K_ANY, 0, 0);
// =============================================================================
VSYM_OBJ_REP*
VSA::Find_hor_mu_vor_any(STMTREP *stmt, HEAP_OBJ_REP* hor, VSYM_FLD_REP* vfr,
                         CODEREP *cr, BOOL match_any) const
{
  Is_True_Ret(vfr, ("null vsym fld rep"), NULL);
  MU_NODE *mnode;
  MU_LIST_ITER mu_iter;
  MU_LIST *mu_list = Stmt_vor_mu(stmt);
  HEAP_OBJ* hobj = hor->Heap_obj();
  VSYM_OBJ_REP *any_vor = NULL;
  VSYM_OBJ_REP *any_zero_vor = NULL;
  VSYM_FLD_REP any_fld(FLD_K_ANY, vfr->Fld_id_name(), vfr->Ofst());
  VSYM_FLD_REP any_zero_fld(FLD_K_ANY, 0, 0);

  if (mu_list && !mu_list->Is_Empty()) {
    FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      if (cvor->first->Vsym_obj()->Base_hor()->Heap_obj() == hobj) {
        if (Vsym_match(stmt, cr, vfr, cvor->first->Vsym_obj()->Fld_rep_ptr())) {
          return cvor->first;
        }
        if (match_any) {
          if (any_vor == NULL &&
              Vsym_match(stmt, cr, &any_fld, cvor->first->Vsym_obj()->Fld_rep_ptr())) {
            any_vor = cvor->first;
          } else if (any_zero_vor == NULL && any_vor == NULL &&
                     Vsym_match(stmt, cr, &any_zero_fld, cvor->first->Vsym_obj()->Fld_rep_ptr())) {
            any_zero_vor = cvor->first;
          }
        }
      }
    }
  }
  if (match_any) {
    return any_vor ? any_vor : any_zero_vor;
  }
  return NULL;
}

// =============================================================================
// VSYM_OBJ_REP *Find_hor_chi_vor_any(STMTREP *stmt, HEAP_OBJ_REP* hor,
//                                    VSYM_FLD_REP* vfr, CODEREP *cr, BOOL match_any)
// Match candidate vor chi with following order:
// 1. exact match with given vfr
// 2. any_fld(FLD_K_ANY, vfr->Fld_id_name(), vfr->Ofst());
// 3. any_zero_fld(FLD_K_ANY, 0, 0);
// =============================================================================
VSYM_OBJ_REP*
VSA::Find_hor_chi_vor_any(STMTREP *stmt, HEAP_OBJ_REP* hor, VSYM_FLD_REP* vfr,
                          CODEREP *cr, BOOL match_any) const
{
  Is_True_Ret(vfr, ("null vsym fld rep"), NULL);
  CHI_NODE *cnode;
  CHI_LIST_ITER chi_iter;
  CHI_LIST *chi_list = Stmt_vor_chi(stmt);
  HEAP_OBJ* hobj = hor->Heap_obj();
  VSYM_OBJ_REP *any_vor = NULL;
  VSYM_OBJ_REP *any_zero_vor = NULL;
  VSYM_FLD_REP any_fld(FLD_K_ANY, vfr->Fld_id_name(), vfr->Ofst());
  VSYM_FLD_REP any_zero_fld(FLD_K_ANY, 0, 0);

  if (chi_list && !chi_list->Is_Empty()) {
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      CVOR *cvor = (CVOR*)cnode->OPND();
      if (cvor->first->Vsym_obj()->Base_hor()->Heap_obj() == hobj) {
        if (Vsym_match(stmt, cr, vfr, cvor->first->Vsym_obj()->Fld_rep_ptr())) {
          return cvor->first;
        }
        if (match_any) {
          if (any_vor == NULL &&
              Vsym_match(stmt, cr, &any_fld, cvor->first->Vsym_obj()->Fld_rep_ptr())) {
            any_vor = cvor->first;
          } else if (any_zero_vor == NULL && any_vor == NULL &&
                     Vsym_match(stmt, cr, &any_zero_fld, cvor->first->Vsym_obj()->Fld_rep_ptr())) {
            any_zero_vor = cvor->first;
          }
        }
      }
    }
  }
  if (match_any) {
    return any_vor ? any_vor : any_zero_vor;
  }
  return NULL;
}

// =============================================================================
// BOOL Vsym_match(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP *vfr1, VSYM_FLD_REP *vfr2)
//
// =============================================================================
BOOL
VSA::Vsym_match(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP *vfr1, VSYM_FLD_REP *vfr2) const
{
  Is_True(vfr1 && vfr2, ("empty vfr1 or vfr2"));
  if(vfr1 && vfr2) {
    VS_MTATCH_KIND mk = vfr2->Match(vfr1);
    if(mk == VS_EXACT_MATCH) {
      return TRUE;
    } else if(mk == VS_MAY_MATCH) {
      if(Eval_vsym_match(stmt, cr, vfr1, vfr2)) {
        return TRUE;
      }
    }
  }
  return FALSE;
}


// =============================================================================
// BOOL Eval_vsym_match(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP *vfr1, VSYM_FLD_REP *vfr2
//
// 1. vfr1 (fld name) -> vfr2 (id, cr) (C->Java):
//    vfr1 cand list: <class1, fld1, id1 >,  <class2, fld2, id2>
//    find cr type, compare with cand class name, see if matching id
// 2. vfr1 (fld id) -> vfr2(fld name, cr) (Java->C)
//    vfr2 cand list: <class1, fld1, id1 >,  <class2, fld2, id2>
//    as we have no cr on vfr1, unable to figure out vfr1's class type, so return
//    not match (may need to stop checking to avoid flase postitive/false negative)
// =============================================================================
BOOL
VSA::Eval_vsym_match(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP *vfr1, VSYM_FLD_REP *vfr2) const
{
  if(!cr) {
    return FALSE;
  }
  VSYM_FLD_REP *vfr_id = vfr2->Is_uniq_id() ? vfr2 : NULL;
  VSYM_FLD_REP *vfr_nm = vfr1->Is_uniq_id() ? NULL : vfr1;

  // only handle situation 1: vfr2 is fld_id, vfr1 is fld name
  if(!vfr_id || !vfr_nm) {
    return FALSE;
  }

  DEF_TYS def_types;
  Find_cr_def_types(stmt, cr, def_types);
  for(int i=0; i< def_types.size(); i++)
  {
    TY_IDX ty_idx = def_types[i].second;
    DNA_NODE *def_dna = def_types[i].first;
    char *cls_name = TY_name(def_dna->File_idx(), ty_idx);
    // all def_types should match
    VFR_CANDS *cands = Vfr_2_cand_map(vfr_nm);
    Is_True(cands, ("no candidates found"));
    if(!cands) {
      return FALSE;
    }
    for(int j=0; j<cands->size(); j++) {
      VFR_CAND *cand = (*cands)[j];
      if(!cand->Match(cls_name, vfr_id->Fld_id())) {
        return FALSE;
      }
    }
  }
  return TRUE;
}

// =============================================================================
// MU_NODE *Find_vor_mu(STMTREP *stmt, UINT32 file_idx, ST_IDX st, IDTYPE fld)
// =============================================================================
MU_NODE*
VSA::Find_vor_mu(STMTREP *stmt, UINT32 file_idx, ST_IDX st, VSYM_FLD_REP* vfr) const
{
  Is_True(vfr, ("null vsym fld rep"));
  MU_LIST *mu_list = Stmt_vor_mu(stmt);
  if (vfr && mu_list && !mu_list->Is_Empty()) {
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      CODEREP* cr = Find_ilod_base(cvor->second);
      if (cr == NULL || cr->Kind() != CK_VAR)
        continue;
      AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(cr->Aux_id());
      if (aux_entry->St() != NULL && ST_st_idx(aux_entry->St()) == st &&
          Vsym_match(stmt, cvor->second, vfr, cvor->first->Vsym_obj()->Fld_rep_ptr()))
        return mnode;
    }
  }
  return NULL;
}

// =============================================================================
// MU_NODE *Find_vor_mu(STMTREP *stmt, VSYM_OBJ *vo)
// =============================================================================
MU_NODE*
VSA::Find_vor_mu(STMTREP *stmt, VSYM_OBJ *vo) const
{
  MU_LIST *mu_list = Stmt_vor_mu(stmt);
  if (mu_list && !mu_list->Is_Empty()) {
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      if (cvor->first->Vsym_obj() == vo)
        return mnode;
    }
  }
  return NULL;
}

// =============================================================================
// CODEREP *Find_vor_chi_cr(STMTREP *stmt, VSYM_OBJ_REP *vor)
// =============================================================================
CODEREP*
VSA::Find_vor_chi_cr(STMTREP *stmt, VSYM_OBJ_REP *vor) const
{
  CHI_LIST *chi_list = Stmt_vor_chi(stmt);
  if (chi_list && !chi_list->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      CVOR *cres = (CVOR*)cnode->RESULT();
      CVOR *copnd = (CVOR*)cnode->OPND();
      Is_True(cres->second == copnd->second, ("cvor cr mismatch"));
      if (cres->first == vor || copnd->first == vor)
        return cres->second;
    }
  }
  return NULL;
}

// =============================================================================
// HEAP_OBJ_REP *Find_stmt_cur_hor(STMTREP *stmt, HEAP_OBJ *ho)
// =============================================================================
HEAP_OBJ_REP*
VSA::Find_stmt_cur_hor(STMTREP *stmt, HEAP_OBJ *ho) const
{
  MU_LIST *mu_list = Stmt_hor_mu(stmt);
  if (mu_list && !mu_list->Is_Empty()) {
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
      CHOR *chor = (CHOR*)mnode->OPND();
      HEAP_OBJ_REP* hor = chor->first;
      if (hor->Heap_obj() == ho)
        return hor;
    }
  }
  CHI_LIST *chi_list = Stmt_hor_chi(stmt);
  if (chi_list && !chi_list->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      CHOR *cvor = (CHOR*)cnode->OPND();
      HEAP_OBJ_REP* hor = cvor->first;
      if (hor->Heap_obj() == ho)
        return hor;
    }
  }
  return NULL;
}

// =============================================================================
// VSYM_OBJ_REP *Find_stmt_cur_vor(STMTREP *stmt, VSYM_OBJ *vo)
// =============================================================================
VSYM_OBJ_REP*
VSA::Find_stmt_cur_vor(STMTREP *stmt, VSYM_OBJ *vo) const
{
  MU_LIST *mu_list = Stmt_vor_mu(stmt);
  if (mu_list && !mu_list->Is_Empty()) {
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(mu_list)) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      VSYM_OBJ_REP* vor = cvor->first;
      if (vor->Vsym_obj() == vo)
        return vor;
    }
  }
  CHI_LIST *chi_list = Stmt_vor_chi(stmt);
  if (chi_list && !chi_list->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      CVOR *cvor = (CVOR*)cnode->OPND();
      VSYM_OBJ_REP* vor = cvor->first;
      if (vor->Vsym_obj() == vo)
        return vor;
    }
  }
  return NULL;
}

// =============================================================================
// CODEREP *Find_stmt_chi_var(STMTREP *stmt, HEAP_OBJ *ho)
// =============================================================================
CODEREP*
VSA::Find_stmt_chi_var(STMTREP *stmt, HEAP_OBJ *ho) const
{
  CHI_LIST *chi_list = stmt->Chi_list();
  if (chi_list == NULL)
    return NULL;
  CHI_NODE* cnode;
  CHI_LIST_ITER chi_iter;
  FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
    if (!cnode->Live())
      continue;
    HEAP_OBJ_REP* hor = Cr_2_heap_obj(cnode->RESULT());
    if (hor != NULL && hor->Heap_obj() == ho)
      return cnode->RESULT();
  }

  chi_list = Stmt_vor_chi(stmt);
  if (chi_list == NULL)
    return NULL;
  FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
    CVOR *cvor = (CVOR*)cnode->RESULT();
    HEAP_OBJ_REP* hor = cvor->first->Vsym_obj()->Base_hor();
    if (hor->Heap_obj() == ho)
      return cvor->second;
  }
  return NULL;
}

// =============================================================================
// VSYM_OBJ_REP *Find_vor_chi_vor(STMTREP *stmt, CODEREP *cr, IDTYPE fld)
// =============================================================================
VSYM_OBJ_REP*
VSA::Find_vor_chi_vor(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP* vfr) const
{
  CHI_NODE *cnode = Find_vor_chi(stmt, cr, vfr);
  return cnode ? ((CVOR*)cnode->RESULT())->first : NULL;
}

// =============================================================================
// CHI_NODE *Find_vor_chi_vor(STMTREP *stmt, CODEREP *cr, IDTYPE fld)
// =============================================================================
CHI_NODE*
VSA::Find_vor_chi(STMTREP *stmt, CODEREP *cr, VSYM_FLD_REP* vfr) const
{
  Is_True(vfr, ("null vsym fld rep"));
  CHI_LIST *chi_list = Stmt_vor_chi(stmt);
  if (vfr && chi_list && !chi_list->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      CVOR *cvor = (CVOR*)cnode->RESULT();
      if (cvor->second == cr && 
          Vsym_match(stmt, cr, vfr, cvor->first->Vsym_obj()->Fld_rep_ptr()))
        return cnode;
    }
  }
  return NULL;
}

// =============================================================================
// VSYM_OBJ_REP *Find_hor_chi_vor(STMTREP *stmt, HEAP_OBJ_REP* hor, VSYM_FLD_REP* vfr)
// =============================================================================
VSYM_OBJ_REP*
VSA::Find_hor_chi_vor(STMTREP *stmt, HEAP_OBJ_REP* hor, VSYM_FLD_REP* vfr, CODEREP *cr) const
{
  Is_True(vfr, ("null vsym fld rep"));
  CHI_LIST *chi_list = Stmt_vor_chi(stmt);
  if (vfr && chi_list && !chi_list->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    HEAP_OBJ* hobj = hor->Heap_obj();
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      CVOR *cvor = (CVOR*)cnode->RESULT();
      if (cvor->first->Vsym_obj()->Base_hor()->Heap_obj() == hobj &&
          Vsym_match(stmt, cr, vfr, cvor->first->Vsym_obj()->Fld_rep_ptr())) {
        return cvor->first;
      }
    }
  }
  return NULL;
}

// =============================================================================
// CHI_NODE *Find_vor_chi(STMTREP *stmt, UINT32 file_idx, ST_IDX st, IDTYPE fld)
// =============================================================================
CHI_NODE*
VSA::Find_vor_chi(STMTREP *stmt, UINT32 file_idx, ST_IDX st, VSYM_FLD_REP* vfr) const
{
  Is_True(vfr, ("null vsym fld rep"));
  CHI_LIST *chi_list = Stmt_vor_chi(stmt);
  if (vfr && chi_list && !chi_list->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      CVOR *cvor = (CVOR*)cnode->OPND();
      CODEREP* cr = Find_ilod_base(cvor->second);
      if (cr == NULL || cr->Kind() != CK_VAR)
        continue;
      AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(cr->Aux_id());
      if (aux_entry->St() != NULL && ST_st_idx(aux_entry->St()) == st &&
          Vsym_match(stmt, cvor->second, vfr, cvor->first->Vsym_obj()->Fld_rep_ptr()))
        return cnode;
    }
  }
  return NULL;
}

// =============================================================================
// CHI_NODE *Find_vor_chi(STMTREP *stmt, VSYM_OBJ *vo)
// =============================================================================
CHI_NODE*
VSA::Find_vor_chi(STMTREP *stmt, VSYM_OBJ *vo) const
{
  CHI_LIST *chi_list = Stmt_vor_chi(stmt);
  if (chi_list && !chi_list->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      CVOR *cres = (CVOR*)cnode->RESULT();
      Is_True(cres->first->Vsym_obj() ==
              ((CVOR*)cnode->OPND())->first->Vsym_obj(), ("vo mismatch"));
      if (cres->first->Vsym_obj() == vo)
        return cnode;
    }
  }
  return NULL;
}

// =============================================================================
// CVOR *Find_vor_chi_opnd(STMTREP *stmt, VSYM_OBJ_REP *vor)
// =============================================================================
CVOR*
VSA::Find_vor_chi_opnd(STMTREP *stmt, VSYM_OBJ_REP *vor) const
{
  CHI_LIST *chi_list = Stmt_vor_chi(stmt);
  if (chi_list && !chi_list->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      CVOR *cvor = (CVOR*)cnode->RESULT();
      if (cvor->first == vor)
        return (CVOR*)cnode->OPND();
    }
  }
  return NULL;
}

// =============================================================================
// MU_NODE *Find_stmt_var_mu(STMTREP *stmt, UINT32 file_idx,
//                           ST_IDX st_idx, VSYM_FLD_REP* fld)
// =============================================================================
MU_NODE*
VSA::Find_stmt_var_mu(STMTREP *stmt, UINT32 file_idx,
                      ST_IDX st_idx, VSYM_FLD_REP* fld) const
{
  WHIRL_FILE_MANAGER* mgr = NULL;
  UINT32 my_file_idx = Comp_unit()->Dna()->File_idx();
  if (my_file_idx != file_idx) {
    mgr = WHIRL_FILE_MANAGER::Get();
    Is_True(mgr != NULL, ("not in xfa mode"));
  }
  MU_NODE* mnode;
  MU_LIST_ITER mu_iter;
  INT fld_id = (fld != NULL && fld->Is_uniq_id()) ? fld->Fld_id() : 0;
  FOR_ALL_NODE(mnode, mu_iter, Init(stmt->Mu_list())) {
    AUX_STAB_ENTRY* sym = Opt_stab()->Aux_stab_entry(mnode->Aux_id());
    if (!sym->Is_global())
      continue;

    if (sym->Field_id() != fld_id)
      continue;

    UINT32 res_file_idx = my_file_idx;
    ST_IDX res_st_idx = ST_st_idx(sym->St());
    if (mgr != NULL)
      mgr->Resolve(my_file_idx, res_st_idx, res_file_idx, res_st_idx);

    if (file_idx == res_file_idx && st_idx == res_st_idx)
      return mnode;
  }
  return NULL;
}

// =============================================================================
// MU_NODE *Find_stmt_var_mu(STMTREP *stmt, UINT32 file_idx,
//                           ST_IDX st_idx, VSYM_FLD_REP* fld)
// =============================================================================
MU_NODE*
VSA::Find_stmt_var_mu(STMTREP *stmt, ST* st, VSYM_FLD_REP* fld) const
{
  Is_True_Ret(st, ("null st"), NULL);
  TY_IDX ty = ST_type(st);
  BOOL is_ptr = (TY_kind(ty) == KIND_ARRAY || TY_kind(ty) == KIND_POINTER);
  BOOL is_struct = TY_kind(ty) == KIND_STRUCT;
  MU_NODE* mnode;
  MU_LIST_ITER mu_iter;
  FOR_ALL_NODE(mnode, mu_iter, Init(stmt->Mu_list())) {
    AUX_STAB_ENTRY* sym = Opt_stab()->Aux_stab_entry(mnode->Aux_id());
    if (sym->St() == st) {
      IDTYPE fld_id = is_struct ? sym->Field_id() :
                        (is_ptr && sym->Byte_size() != 0) ?
                          sym->Base_byte_ofst()/sym->Byte_size() : 0;
      VSYM_FLD_REP tmp(FLD_K_ID, fld_id, sym->Base_byte_ofst());
      if (fld->Match(&tmp))
        return mnode;
    }
  }
  return NULL;
}

// =============================================================================
// void Update_stmt_var_mu(STMTREP *stmt, MU_NODE *mu, AUX_ID aux)
// =============================================================================
void
VSA::Update_stmt_var_mu(STMTREP *stmt, MU_NODE *mu, AUX_ID aux)
{
  Is_True(mu != NULL && mu->Aux_id() == aux, ("wrong mu"));
  CODEREP* opnd = Comp_unit()->Opt_stab()->Top_coderep(aux);
  Is_True(opnd != NULL, ("top coderep is NULL"));
  mu->Set_OPND(opnd);
  Is_Trace(Tracing(),
           (TFile, "Update_stmt_var_mu: update cr=%d:sym%dv%d\n",
                   opnd->Coderep_id(), aux, opnd->Version()));
}

// =============================================================================
// void Update_stmt_var_chi(CHI_NODE *chi, AUX_ID aux)
// =============================================================================
void
VSA::Update_stmt_var_chi(STMTREP* stmt, CHI_NODE *chi, AUX_ID aux)
{
  AUX_STAB_ENTRY* entry = Opt_stab()->Aux_stab_entry(aux);
  Is_True(entry && entry->St(), ("failed to find aux entry"));
  TY_IDX ty = ST_type(entry->St());
  TYPE_ID mty = entry->Mtype();
  TYPE_ID rty = Mtype_comparison(mty);

  CODEMAP* htable = Comp_unit()->Htable();

  CODEREP* opnd = NULL;
  if (stmt->Opr() == OPR_OPT_CHI) {
    CODEREP* zero_cr = entry->Zero_cr();
    if (zero_cr == NULL) {
      zero_cr = _htable->Add_def(aux, 0, NULL, rty, mty,
                                 0, ty, 0, TRUE);
      zero_cr->Set_flag(CF_MADEUP_TYPE);
      zero_cr->Set_flag(CF_IS_ZERO_VERSION);
      entry->Set_zero_cr(zero_cr);
    }
    opnd = zero_cr;
  }
  else {
    opnd = Comp_unit()->Opt_stab()->Top_coderep(aux);
    Is_True(opnd != NULL, ("top coderep is NULL"));
  }
  chi->Set_OPND(opnd);

  CODEREP* res = htable->Add_def(aux, -1, stmt, Mtype_comparison(mty), mty,
                                 0, ty, 0, TRUE);
  res->Set_flag(CF_DEF_BY_CHI);
  res->Set_defchi(chi);
  res->Set_defstmt(stmt);
  chi->Set_RESULT(res);

  Opt_stab()->Push_coderep(aux, res);
  Is_Trace(Tracing(),
           (TFile, "Update_stmt_var_chi: update-push %s:%d cr%d:sym%dv%d=cr%d:sym%dv%d\n",
                   OPERATOR_name(stmt->Opr()) + 4, stmt->Stmtrep_id(),
                   res->Coderep_id(), aux, res->Version(),
                   opnd->Coderep_id(), aux, opnd->Version()));
}

// =============================================================================
// void VSA::Update_bb_var_phi(BB_NODE *bb, PHI_NODE *phi, AUX_ID aux)
// =============================================================================
void
VSA::Update_bb_var_phi(BB_NODE *bb, PHI_NODE *phi, AUX_ID aux)
{
  AUX_STAB_ENTRY* entry = Opt_stab()->Aux_stab_entry(aux);
  Is_True(entry && entry->St(), ("failed to find aux entry"));
  if (entry == NULL || entry->St() == NULL)
    return;
  TY_IDX ty = ST_type(entry->St());
  TYPE_ID mty = TY_mtype(ty);
  CODEMAP* htable = Comp_unit()->Htable();
  CODEREP* res = htable->Add_def(aux, -1, NULL,
                                 Mtype_comparison(mty), mty,
                                 0, ty, 0, TRUE);
  res->Set_flag(CF_DEF_BY_PHI);
  res->Set_defphi(phi);
  phi->Set_result(res);
  phi->Set_res_is_cr();
  Opt_stab()->Push_coderep(aux, res);
  Is_Trace(Tracing(),
           (TFile, "Update_bb_var_phi: update-push BB%d cr%d:sym%dv%d\n",
                   bb->Id(), res->Coderep_id(), aux, res->Version()));
}

// =============================================================================
// void VSA::Update_stmt_var_mu(STMTREP *stmt, const AUX_DEF_MAP& to_add, const AUX_DEF_MAP& to_upd)
// =============================================================================
void
VSA::Update_stmt_var_mu(STMTREP *stmt, const AUX_DEF_MAP* to_add, const AUX_DEF_MAP* to_upd, const AUX_SET *aux_set)
{
  Is_True(to_add != NULL && to_upd != NULL, ("to_add or to_upd is null"));
  MU_LIST* mu_list = stmt->Mu_list();
  Is_True(mu_list != NULL, ("mu list is null"));

  MU_NODE *mu, *prev = NULL;
  MU_LIST_ITER mu_iter;
  mu_iter.Init(mu_list);
  for (mu = mu_iter.First(); ! mu_iter.Is_Empty(); /* empty */) {
    Is_True(to_add->find(mu->Aux_id()) == to_add->end(), ("found new aux in mu"));
    AUX_ID aux = mu->Aux_id();
    AUX_STAB_ENTRY* entry = Opt_stab()->Aux_stab_entry(aux);
    if (!entry->Is_global()) {
      // do nothing
    }
    else if (to_upd->find(aux) != to_upd->end()) {
      Update_stmt_var_mu(stmt, mu, aux);
    }
    else if (aux_set != NULL && aux_set->find(aux) == aux_set->end()) {
      MU_NODE *tmp = mu;
      mu = mu_iter.Next();
      mu_list->Remove(prev, tmp);
      continue;
    }
    prev = mu;
    mu = mu_iter.Next();
  }

  for (AUX_DEF_MAP::const_iterator it = to_add->begin();
       it != to_add->end(); ++ it) {
    AUX_ID aux = it->first;
    if (Comp_unit()->Opt_stab()->NULL_coderep(aux) &&
        stmt->Opr() != OPR_OPT_CHI) {
      Is_True(Cfg()->Fake_entry_bb() != NULL, ("no fake entry bb"));
      continue;
    }
    MU_NODE* mu = mu_list->New_mu_node(aux, Comp_unit()->Mem_pool());
    Update_stmt_var_mu(stmt, mu, aux);
  }
}

// =============================================================================
// void VSA::Update_stmt_var_chi(STMTREP *stmt, const AUX_DEF_MAP& to_add, const AUX_DEF_MAP& to_upd)
// =============================================================================
void
VSA::Update_stmt_var_chi(STMTREP *stmt, const AUX_DEF_MAP *to_add,
                         const AUX_DEF_MAP *to_upd, const AUX_SET *aux_set)
{
  Is_True(to_add != NULL && to_upd != NULL, ("to_add or to_upd is null"));
  CHI_LIST* chi_list = stmt->Chi_list();
  Is_True(chi_list != NULL, ("chi list is null"));

  CHI_NODE *chi, *prev = NULL;
  CHI_LIST_ITER chi_iter;
  chi_iter.Init(chi_list);
  for (chi = chi_iter.First(); ! chi_iter.Is_Empty(); /* empty */ ) {
    Is_True(to_add->find(chi->Aux_id()) == to_add->end(), ("found new aux in chi"));
    AUX_ID aux = chi->Aux_id();
    if (to_upd->find(aux) != to_upd->end()) {
      if (chi->Live() && !chi->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION)) {
        CODEREP* res = chi->RESULT();
        Opt_stab()->Push_coderep(aux, res);
        Is_Trace(Tracing(),
                 (TFile, "Update_stmt_var_chi: reuse-push %s:%d cr%d:sym%dv%d\n",
                         OPERATOR_name(stmt->Opr()) + 4, stmt->Stmtrep_id(),
                         res->Coderep_id(), aux, res->Version()));
      }
      else {
        chi->Set_live(TRUE);
        Update_stmt_var_chi(stmt, chi, aux);
      }
    }
    else if (!chi->Live() ||
             !Opt_stab()->Aux_stab_entry(aux)->Is_global()) {
      // do nothing
    }
    else if (!chi->OPND()->Is_flag_set(CF_IS_ZERO_VERSION) &&  // can leave zero ver without def
             aux_set != NULL && aux_set->find(aux) == aux_set->end()) {
      CODEREP* res = chi->RESULT();
      CODEREP* opnd = chi->OPND();
      Is_Trace(Tracing(),
               (TFile, "Update_stmt_var_chi: remove chi %s:%d cr%d:sym%dv%d=cr%d:sym%dv%d\n",
                       OPERATOR_name(stmt->Opr()) + 4, stmt->Stmtrep_id(),
                       res->Coderep_id(), aux, res->Version(),
                       opnd->Coderep_id(), aux, opnd->Version()));
      CHI_NODE *tmp = chi;
      chi = chi_iter.Next();
      chi_list->Remove(prev, tmp);
      continue;
    }
    prev = chi;
    chi = chi_iter.Next();
  }

  for (AUX_DEF_MAP::const_iterator it = to_add->begin();
       it != to_add->end(); ++ it) {
    AUX_ID aux = it->first;
    if (Comp_unit()->Opt_stab()->NULL_coderep(aux) &&
        stmt->Opr() != OPR_OPT_CHI) {
      Is_True(Cfg()->Fake_entry_bb() != NULL, ("fake_entry is null"));
      continue;
    }
    CHI_NODE* chi = chi_list->New_chi_node(aux, Comp_unit()->Mem_pool());
    Is_True(chi != NULL, ("failed to create chi"));
    chi->Set_live(TRUE);
    Update_stmt_var_chi(stmt, chi, aux);
  }
}

// =============================================================================
// void VSA::Update_bb_var_phi(BB_NODE *bb, const AUX_DEF_MAP& to_add, const AUX_DEF_MAP& to_upd)
// =============================================================================
void
VSA::Update_bb_var_phi(BB_NODE *bb, const AUX_DEF_MAP *to_add, const AUX_DEF_MAP *to_upd)
{
  Is_True(to_add != NULL && to_upd != NULL, ("to_add or to_upd is null"));
  PHI_LIST* phi_list = bb->Phi_list();
  if (phi_list == NULL || phi_list->Is_Empty())
    return;

  PHI_NODE* phi;
  PHI_LIST_ITER phi_iter;
  FOR_ALL_NODE (phi, phi_iter, Init(phi_list)) {
    AUX_ID aux = phi->Aux_id();
    if (to_add->find(aux) != to_add->end() ||
        to_upd->find(aux) != to_upd->end()) {
      if (phi->Live() && !phi->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION)) {
        CODEREP* res = phi->RESULT();
        Opt_stab()->Push_coderep(aux, res);
        Is_Trace(Tracing(),
                 (TFile, "Update_bb_var_phi: reuse-push BB%d cr%d:sym%dv%d\n",
                         bb->Id(), res->Coderep_id(), aux, res->Version()));
        continue;
      }

      phi->Set_live();
      Update_bb_var_phi(bb, phi, aux);
    }
  }
}

// =============================================================================
// void VSA::Update_bb_succ_var_phi(BB_NODE *bb, const AUX_DEF_MAP& to_add, const AUX_DEF_MAP& to_upd)
// =============================================================================
void
VSA::Update_bb_succ_var_phi(BB_NODE *bb, const AUX_DEF_MAP *to_add, const AUX_DEF_MAP *to_upd)
{
  Is_True(to_add != NULL && to_upd != NULL, ("to_add or to_upd is NULL"));
  BB_NODE *succ;
  BB_LIST_ITER bb_iter;
  FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
    INT32 pos = succ->Pred()->Pos(bb);
    PHI_NODE *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_ELEM (phi, phi_iter, Init(succ->Phi_list())) {
      AUX_ID aux = phi->Aux_id();
      if (to_add->find(aux) != to_add->end() ||
          to_upd->find(aux) != to_upd->end()) {
        CODEREP* opnd = Comp_unit()->Opt_stab()->Top_coderep(aux);
        Is_True(opnd != NULL, ("top coderep is NULL"));
        phi->Set_opnd(pos, opnd);
      }
    }
  }
}

// =============================================================================
// CHI_NODE *Find_stmt_var_chi(STMTREP *stmt, UINT32 file_idx,
//                             ST_IDX st_idx, VSYM_FLD_REP* fld)
// =============================================================================
CHI_NODE*
VSA::Find_stmt_var_chi(STMTREP *stmt, UINT32 file_idx,
                       ST_IDX st_idx, VSYM_FLD_REP* fld) const
{
  WHIRL_FILE_MANAGER* mgr = NULL;
  UINT32 my_file_idx = Comp_unit()->Dna()->File_idx();
  if (my_file_idx != file_idx) {
    mgr = WHIRL_FILE_MANAGER::Get();
    Is_True(mgr != NULL, ("not in xfa mode"));
  }
  CHI_NODE* cnode;
  CHI_LIST_ITER chi_iter;
  INT fld_id = (fld != NULL && fld->Is_uniq_id()) ? fld->Fld_id() : 0;
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    if (!cnode->Live())
      continue;

    AUX_STAB_ENTRY* aux = Opt_stab()->Aux_stab_entry(cnode->Aux_id());
    if (!aux->Is_global())
      continue;

    if (aux->Field_id() != fld_id)
      continue;

    UINT32 res_file_idx = my_file_idx;
    ST_IDX res_st_idx = ST_st_idx(aux->St());
    if (mgr != NULL)
      mgr->Resolve(my_file_idx, res_st_idx, res_file_idx, res_st_idx);

    if (file_idx == res_file_idx && st_idx == res_st_idx)
      return cnode;
  }
  return NULL;
}

// =============================================================================
// CHI_NODE *Find_stmt_var_chi(STMTREP *stmt, ST* st, VSYM_FLD_REP* fld)
// =============================================================================
CHI_NODE*
VSA::Find_stmt_var_chi(STMTREP *stmt, ST* st, VSYM_FLD_REP* fld) const
{
  Is_True_Ret(st, ("null st"), NULL);
  TY_IDX ty = ST_type(st);
  BOOL is_ptr = (TY_kind(ty) == KIND_ARRAY || TY_kind(ty) == KIND_POINTER);
  BOOL is_struct = TY_kind(ty) == KIND_STRUCT;
  CHI_NODE* cnode;
  CHI_LIST_ITER chi_iter;
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    if (!cnode->Live())
      continue;

    AUX_STAB_ENTRY* sym = Opt_stab()->Aux_stab_entry(cnode->Aux_id());
    if (sym->St() == st) {
      IDTYPE fld_id = is_struct ? sym->Field_id() :
                        (is_ptr && sym->Byte_size() != 0) ?
                          sym->Base_byte_ofst()/sym->Byte_size() : 0;
      VSYM_FLD_REP tmp(FLD_K_ID, fld_id, sym->Base_byte_ofst());
      if (fld->Match(&tmp))
        return cnode;
    }
  }
  return NULL;
}

// =============================================================================
//
// CHI_NODE *Find_stmt_var_chi(STMTREP *stmt, CODEREP* opnd)
//
// =============================================================================
CHI_NODE*
VSA::Find_stmt_var_chi(STMTREP *stmt, CODEREP *opnd) const
{
  CHI_NODE* cnode;
  CHI_LIST_ITER chi_iter;
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    if (!cnode->Live())
      continue;

    AUX_STAB_ENTRY* aux = Opt_stab()->Aux_stab_entry(cnode->Aux_id());
    if (!aux->Is_global())
      continue;

    if (cnode->OPND() == opnd)
      return cnode;
  }
  return NULL;
}

// =============================================================================
// STMTREP* Get_entry_chi_stmt() const
// =============================================================================
STMTREP*
VSA::Get_entry_chi_stmt() const
{
  if (Cfg()->Fake_entry_bb() != NULL) {
    BB_NODE *bb;
    BB_LIST_ITER bb_iter;
    FOR_ALL_ELEM (bb, bb_iter, Init(Cfg()->Fake_entry_bb()->Succ())) {
      if (bb->Kind() == BB_ENTRY) {
        STMTREP* entry_chi = bb->First_stmtrep();
        Is_True(entry_chi != NULL && entry_chi->Opr() == OPR_OPT_CHI,
                ("bad entry chi stmt"));
        return entry_chi;
      }
    }
    Is_True(FALSE, ("not find entry"));
    return NULL;
  }
  else {
    BB_NODE *bb = Cfg()->Entry_bb();
    Is_True(bb->Kind() == BB_ENTRY, ("wrong entry bb kind"));
    STMTREP *entry_chi = bb->First_stmtrep();
    Is_True(entry_chi != NULL && entry_chi->Opr() == OPR_OPT_CHI,
            ("bad entry chi stmt"));
    return entry_chi;
  }
}

// =============================================================================
// void Collect_stmt_chi_globals(STMTREP* stmt, FST_AUX_MAP* globals)
// iteratr stmt chi list and add map from <file, st> to AUX for globals
// =============================================================================
void
VSA::Collect_stmt_chi_globals(UINT32 file_idx, STMTREP* stmt, FST_AUX_MAP* globals)
{
  Is_True(globals != NULL, ("globals is null"));
  UINT32 me = Comp_unit()->Dna()->File_idx();
  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
  if (me != file_idx) {
    Is_True(mgr != NULL, ("not in xfa mode"));
  }
  // make sure stmt has chi
  Is_True(stmt->Chi_list() != NULL, ("no chi list"));
  CHI_NODE* chi;
  CHI_LIST_ITER chi_iter;
  FOR_ALL_NODE(chi, chi_iter, Init(stmt->Chi_list())) {
    if (!chi->Live())
      continue;
    AUX_STAB_ENTRY* aux = Opt_stab()->Aux_stab_entry(chi->Aux_id());
    if (!aux->Is_global())
      continue;
    if (aux->Field_id() != 0 || aux->St_ofst() != 0)
      continue;
    UINT32 def_file = file_idx;
    ST_IDX def_st = ST_st_idx(aux->St());
    if (mgr != NULL && ST_sclass(aux->St()) == SCLASS_EXTERN)
      mgr->Resolve(def_file, def_st, def_file, def_st);
    ST* st = St_ptr(def_file, def_st);
    if (ST_class(st) != CLASS_VAR)
      continue;  // ignore non-var symbols
    if (ST_is_const_initialized(st))
      continue;
    FILE_ST_IDX fst = File_st_idx(def_file, def_st);
    AUX_ID& value = (*globals)[fst];
    if (value == 0 || (aux->Is_real_var() && ! Opt_stab()->Is_real_var(value))) {
      value = chi->Aux_id();
    }
  }
}

// =============================================================================
// void Build_local_fst_map(UINT32 file_idx, FST_FST_MAP& map)
// map global variable definition <file, st> to local <file, st>
// =============================================================================
void
VSA::Build_local_fst_map(FST_FST_MAP* map)
{
  Is_True(map != NULL, ("fst map is null"));
  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
  if (mgr == NULL)
    return;  // no need to build map for single input file

  UINT32 me = Comp_unit()->Dna()->File_idx();
  ST* st;
  int i;
  FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {
    if (ST_class(st) != CLASS_VAR)
      continue;
    if (ST_sclass(st) != SCLASS_EXTERN)
      continue;
    UINT32 ref_file = me;
    UINT32 ref_st = ST_st_idx(st);
    UINT32 def_file = ref_file;
    UINT32 def_st = ref_st;
    mgr->Resolve(ref_file, ref_st, def_file, def_st);
    map->insert(std::make_pair(File_st_idx(def_file, def_st),
                               File_st_idx(ref_file, ref_st)));
  }
}

// =============================================================================
// UINT32 Build_local_aux_map(ST_AUX_MAP* map)
// setup mapping from <file, st> to AUX for externs referenced in function
// =============================================================================
void
VSA::Build_local_aux_map(FST_AUX_MAP* map)
{
  Is_True(map != NULL, ("aux map is null"));
  UINT32 me = Comp_unit()->Dna()->File_idx();
  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();

  // search aux table
  AUX_ID i;
  AUX_STAB_ITER stab_iter(Comp_unit()->Opt_stab());
  FOR_ALL_NODE(i, stab_iter, Init()) {
    AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(i);
    if (!sym->Is_global())
      continue;

    if (sym->St_ofst() != 0)
      continue;

    UINT32 def_file = me;
    ST_IDX def_st = ST_st_idx(sym->St());
    if (mgr)
      mgr->Resolve(def_file, def_st, def_file, def_st);
    FILE_ST_IDX fst = File_st_idx(def_file, def_st);
    AUX_ID& value = (*map)[fst];
    if (value == 0 || (sym->Is_real_var() && ! Opt_stab()->Is_real_var(value)))
      value = i;
  }
  Is_True(i == Comp_unit()->Opt_stab()->Lastidx() + 1,
          ("wrong i=%d or lastidx=%d", i, Comp_unit()->Opt_stab()->Lastidx()));
}

// =============================================================================
// void Place_global_phi(const AUX_DEF_MAP& to_add, const AUX_DEF_MAP& to_upd,
//                       UINT32 adjust, MEM_POOL* pool)
// =============================================================================
void
VSA::Place_global_phi(const AUX_DEF_MAP* to_add, const AUX_DEF_MAP* to_upd, MEM_POOL* pool)
{
  Is_True(to_add != NULL && to_upd != NULL, ("to_add or to_upd is NULL"));
  BS_ELT bbs = Cfg()->Total_bb_count();
  MEM_POOL bbset_pool;

  OPT_POOL_Initialize(&bbset_pool, "SSA bb set pool", FALSE, SSA_DUMP_FLAG);
  OPT_POOL_Push(&bbset_pool, SSA_DUMP_FLAG);
  BB_NODE_SET inserted(bbs, Cfg(), &bbset_pool, BBNS_EMPTY);
  BB_NODE_SET everonlist(bbs, Cfg(), &bbset_pool, BBNS_EMPTY);
  BB_LIST_CONTAINER worklist;

  // an approximate algorithm
  BB_NODE_SET_ITER bns_iter;
  BB_NODE *bbx, *bby;
  BB_LIST_ITER bb_list_iter;

  FOR_ALL_ELEM (bbx, bb_list_iter, Init(_call_bb_list)) {
    everonlist.Union1D(bbx);
    worklist.Append(bbx, &bbset_pool);
  }

  while (bbx = worklist.Remove_head(&bbset_pool)) {
    FOR_ALL_ELEM (bby, bns_iter, Init(bbx->Dom_frontier())) {
      if (inserted.MemberP(bby) == FALSE) {
        // create phi
        PHI_LIST* phi_list = bby->Phi_list();
        for (AUX_DEF_MAP::const_iterator it = to_add->begin();
             it != to_add->end(); ++ it) {
          AUX_ID aux = it->first;
          Is_True(phi_list->Search_phi_node(aux) == NULL, ("phi already placed"));
          PHI_NODE *phi = phi_list->New_phi_node(aux, Mem_pool(), bby);
          AUX_STAB_ENTRY *entry = Opt_stab()->Aux_stab_entry(aux);
          Is_True(entry != NULL && entry->St() != NULL, ("aux entry invalid"));
          if (entry == NULL || entry->St() == NULL)
            continue;
          TY_IDX ty = ST_type(entry->St());
          TYPE_ID mty = TY_mtype(ty);
          CODEMAP* htable = Comp_unit()->Htable();
          CODEREP* res = htable->Add_def(aux, -1, NULL,
                                         Mtype_comparison(mty), mty,
                                         0, ty, 0, TRUE);
          res->Set_flag(CF_DEF_BY_PHI);
          res->Set_defphi(phi);
          phi->Set_result(res);
        }
        // add to inserted
        inserted.Union1D(bby);
        if (everonlist.MemberP(bby) == FALSE) {
          everonlist.Union1D(bby);
          worklist.Append(bby, &bbset_pool);
        }
      }
    }
  }

#if 0
  // a precise algorithm
  for (AUX_DEF_MAP::const_iterator it = to_add->begin();
       it != to_add->end(); ++ it) {
    inserted.ClearD();
    everonlist.ClearD();
    worklist.Clear();

    AUX_ID aux = it->first;
    BB_LIST* defs = it->second;
    Is_True(defs != NULL, ("def bbs is NULL"));

    BB_NODE_SET_ITER bns_iter;
    BB_NODE *bbx, *bby;
    BB_LIST_ITER bb_list_iter;
    FOR_ALL_ELEM (bbx, bb_list_iter, Init(defs)) {
      if (everonlist.MemberP(bbx) == FALSE) {
        everonlist.Union1D(bbx);
        worklist.Append(bbx, &bbset_pool);
      }
    }

    while (bbx = worklist.Remove_head(&bbset_pool)) {
      FOR_ALL_ELEM (bby, bns_iter, Init(bbx->Dom_frontier())) {
        if (inserted.MemberP(bby) == FALSE) {
          // New phi
          PHI_LIST* phi_list = bby->Phi_list();
          Is_True(phi_list->Search_phi_node(aux) == NULL, ("phi already placed"));
          PHI_NODE *phi = phi_list->New_phi_node(aux, Mem_pool(), bby);
          AUX_STAB_ENTRY *entry = Opt_stab()->Aux_stab_entry(aux);
          Is_True(entry != NULL && entry->St() != NULL, ("aux entry invalid"));
          TY_IDX ty = ST_type(entry->St());
          TYPE_ID mty = TY_mtype(ty);
          CODEMAP* htable = Comp_unit()->Htable();
          CODEREP* res = htable->Add_def(aux, -1, NULL,
                                         Mtype_comparison(mty), mty,
                                         0, ty, 0, TRUE);
          res->Set_flag(CF_DEF_BY_PHI);
          res->Set_defphi(phi);
          phi->Set_result(res);
          inserted.Union1D(bby);
          if (everonlist.MemberP(bby) == FALSE) {
            everonlist.Union1D(bby);
            worklist.Append(bby, &bbset_pool);
          }
        }
      }
    }
  }
#endif

  OPT_POOL_Pop(&bbset_pool, SSA_DUMP_FLAG);
  OPT_POOL_Delete(&bbset_pool, SSA_DUMP_FLAG);
}

// =============================================================================
// ST_IDX Find_or_create_local_st(FST_FST_MAP& fst_map, UINT32 file_idx, ST_IDX st_idx,
//                                BOOL& create)
//   create is in/out:
//     in  - TRUE: create if not found
//     out - TRUE: st is not found and created just now
// =============================================================================
ST*
VSA::Find_or_create_local_st(FST_FST_MAP* fst_map, UINT32 file_idx, ST_IDX st_idx, BOOL& create)
{
  UINT32 me = Comp_unit()->Dna()->File_idx();
  if (file_idx == me) {
    create = FALSE;
    return ST_ptr(st_idx);
  }

  Is_True(fst_map != NULL, ("fst map is null"));
  FILE_ST_IDX fst = File_st_idx(file_idx, st_idx);
  FST_FST_MAP::iterator it = fst_map->find(fst);
  if (it != fst_map->end()) {
    FILE_ST_IDX local = it->second;
    Is_True(FST_file_idx(local) == me, ("wrong fst_map"));
    return ST_ptr(FST_st_idx(local));
  }

  if (!create)
    return NULL;

  // find or create new TY
  ST* ref_st = St_ptr(file_idx, st_idx);
  Is_True(ST_class(ref_st) == CLASS_VAR, ("not var?"));
  TY* ref_ty = Ty_ptr(file_idx, ST_type(ref_st));
  TY_IDX ty = (TY_IDX)0;
  switch (TY_kind(*ref_ty)) {
  case KIND_SCALAR:
    ty = MTYPE_To_TY(TY_mtype(*ref_ty));
    break;
  case KIND_STRUCT:
    {
      TY& new_ty = New_TY(ty);
      TY_Init(new_ty, 0, KIND_STRUCT, MTYPE_M,
              Save_Str(TY_name(file_idx, ref_ty)));
      Set_TY_is_incomplete(new_ty);
    }
    break;
  case KIND_ARRAY:
  case KIND_POINTER:
    ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V));
    Set_TY_is_incomplete(ty);
    break;
  case KIND_FUNCTION:
  case KIND_VOID:
  default:
    Is_True(FALSE, ("bad kind"));
    break;
  }

  // create new PU and extern st
  ST* st = New_ST(GLOBAL_SYMTAB);
  ST_Init(st, Save_Str(ST_name(file_idx, ref_st)), ST_class(ref_st),
          SCLASS_EXTERN, EXPORT_PREEMPTIBLE, ty);

  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
  Is_True(mgr != NULL, ("not in xfa mode"));
  mgr->Add_symbol(me, ST_st_idx(st), file_idx, st_idx);
  fst_map->insert(std::make_pair(fst, File_st_idx(me, ST_st_idx(st))));
  return st;
}

// =============================================================================
// AUX_ID Find_local_aux(FST_AUX_MAP& aux_map, FST_FST_MAP& fst_map,
//                       UINT32 file_idx, ST_IDX st_idx, BOOL& create)
//   FST_AUX_MAP& aux_map: global <file_idx, st_idx> to AUX
//   FST_FST_MAP& fst_map: global <file_idx, st_idx> to local <file_idx, st_idx>
//   create is in/out:
//     in  - TRUE: create if not found
//     out - TRUE: st is not found and created just now
// =============================================================================
AUX_ID
VSA::Find_or_create_local_aux(FST_AUX_MAP* aux_map, FST_FST_MAP* fst_map,
                              UINT32 file_idx, ST_IDX st_idx, BOOL& create)
{
  BOOL create_st = create;
  ST* st = Find_or_create_local_st(fst_map, file_idx, st_idx, create_st);
  if (st == NULL && !create)
    return ILLEGAL_AUX_ID;
  Is_True(st != NULL, ("failed to create st"));

  Is_True(aux_map != NULL, ("aux map is null"));
  FILE_ST_IDX fst = File_st_idx(file_idx, st_idx);
  FST_AUX_MAP::iterator it = aux_map->find(fst);
  if (it != aux_map->end()) {
    create = FALSE;
    return it->second;
  }

  WN wn;
  BZERO (&wn, sizeof(WN));
  WN_set_operator(&wn, OPR_LDID);
  TY_IDX ty = ST_type(st);
  TYPE_ID mty = TY_kind(ty) == KIND_ARRAY ? Pointer_type : TY_mtype(ty);
  WN_set_desc(&wn, mty);
  WN_set_rtype(&wn, Mtype_comparison(mty));
  WN_st_idx(&wn) = ST_st_idx(st);
  WN_set_ty(&wn, ty);
  INT32 lastidx = Opt_stab()->Lastidx();
  Opt_stab()->Set_aux_sym_cnt(lastidx);
  Opt_stab()->Count_syms(&wn);
  INT32 sym_cnt = Opt_stab()->Get_aux_sym_cnt();
  if (sym_cnt != lastidx) {
    Is_True(sym_cnt == lastidx + 1, ("wrong sym cnt returned"));
    Opt_stab()->Set_aux_stab_idx(lastidx + 1);
  }
  AUX_ID aux = Opt_stab()->Enter_symbol(OPR_LDID, st, 0, ty, FALSE, &wn);
  if (aux != lastidx + 1) {
    create = FALSE;
    if (sym_cnt == lastidx + 1)
      Opt_stab()->Shrink_aux_stab(1); // remove the entry added just now
  }
  aux_map->insert(std::make_pair(fst, aux));
  return aux;
}

// ====================================================================
// VSA::Initialize_ignore_cr_set
// ====================================================================
void
VSA::Initialize_ignore_cr_set(MEM_POOL* mp)
{
  Is_True(_ignore_cr_set == NULL, ("_ignore_cr_set already initialized"));

  _ignore_cr_set = CXX_NEW(CODEREP_SET(16, hash_coderep(), std::equal_to<CODEREP *>(), mp), mp);
}

// =============================================================================
// BOOL VSA::Get_alloc_path
// =============================================================================
BOOL
VSA::Get_alloc_path(SRCPOS_NODES *nodes, HEAP_OBJ_REP* hor, CODEREP* cr, hash_set<IDTYPE> &visited_bb)
{
  while(cr->Kind() == CK_OP && (cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL))
    cr = cr->Opnd(0);
  if(cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = cr->Defphi();
    if (visited_bb.find(phi->Bb()->Id()) != visited_bb.end())
      return FALSE;
    visited_bb.insert(phi->Bb()->Id());

    for(int i=0; i < phi->Size(); i++) {
      // only find the first one
      if(Get_alloc_path(nodes, hor, phi->OPND(i), visited_bb))
        return TRUE;
    }
  }
  else if(cr->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP* defstmt = cr->Get_defstmt();
    if(Callee_returns_new_heap_memory(defstmt)) {
      HEAP_OBJ_REP *call_hor = Cr_2_heap_obj(defstmt->Rhs());
      if(call_hor == hor) {
        SRCPOS_NODE node(defstmt, Comp_unit()->Dna(), PATHINFO_COPY);
        if(nodes->size() > 0 && !((*nodes)[nodes->size()-1].equal(node)))
          nodes->push_back(node);
        return TRUE;
      } else
        return FALSE;
    }
    else if (hor->Attr() == ROR_DEF_BY_CHI && defstmt->Opr() == OPR_OPT_CHI)
      return TRUE;
    else
      return FALSE;
  }
  else {
    STMTREP *defstmt = cr->Get_defstmt();
    if(defstmt && OPERATOR_is_store(defstmt->Opr())) {
      if(Get_alloc_path(nodes, hor, defstmt->Rhs(), visited_bb)) {
        nodes->push_back(SRCPOS_NODE(defstmt, Comp_unit()->Dna(), PATHINFO_COPY));
        return TRUE;
      }
    }
  }
  return FALSE;
}


#ifdef Is_True_On
// =============================================================================
//
//  Print_srcpos_nodes:print SRCPOS_NODES, for debug use only
//
// =============================================================================

void
VSA::Print_srcpos_nodes(SRCPOS_NODES &nodes, FILE* fp)
{
  for(SRCPOS_NODES::iterator it = nodes.begin(); it != nodes.end(); it++) {
    SRCPOS spos = it->Spos();
    fprintf(fp, "%d:%d:%d:%p, ",
                it->File_idx(), SRCPOS_filenum(spos), SRCPOS_linenum(spos), it->Inlcxt());
  }
  fprintf(fp, "\n");
}
#endif

// =============================================================================
//
//  VSA::Compose_rvsa_path compose the path for RVSA report
//
// =============================================================================
void
VSA::Compose_rvsa_path(SRCPOS_HANDLE *srcpos_h, CODEREP* cr)
{
  if(cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = cr->Defphi();
    PHI_OPND_ITER iter(phi);
    CODEREP  *opnd;
    SRCPOS_TREENODE *cur_treenode = srcpos_h->Cur_node();
    cur_treenode->Add_children(phi->Size());
    srcpos_h->Path()->Push_mark(phi);
    IDTYPE  i = 0;
    FOR_ALL_ELEM(opnd, iter, Init()) {
      srcpos_h->Set_cur_node(cur_treenode, i);
      Compose_rvsa_path(srcpos_h, opnd);
      i++;
      srcpos_h->Path()->Pop_mark(phi, FALSE);
    }
    srcpos_h->Path()->Pop_mark(phi, TRUE);
  } else {
    STMTREP *defstmt = cr->Defstmt();
    srcpos_h->Append_data(defstmt, Comp_unit()->Dna(), PATHINFO_COPY);
  }
}
// =============================================================================
//
//  VSA::Compose_dbf_path compose the path for DBF error
//
// =============================================================================

void
VSA::Compose_dbf_path(SRCPOS_HANDLE *srcpos_h, HEAP_OBJ_REP* hor, STMTREP* call )
{
  Is_True(hor->Attr() == ROR_DEF_BY_FREE ||
          hor->Attr() == ROR_DEF_BY_DANGLE, ("Wrong HEAP_OBJ_REP attr"));
  Is_True(OPERATOR_is_call(call->Opr()),  ("Wrong STMTREP Opr"));
  srcpos_h->Append_data(call, Comp_unit()->Dna(), PATHINFO_FREE);
  
  // 1. compose the SRCPOS_NODE vector for hor

  if(IPSA_insession()) {

    HEAP_OBJ_REP* pho = hor;
    HEAP_OBJ_REP* def_hor = NULL;
    while(pho) {
      if(pho->Attr() == ROR_DEF_BY_CHI
         || pho->Attr() == ROR_DEF_BY_ALLOC
         || pho->Attr() == ROR_DEF_BY_IPARM
         || pho->Attr() == ROR_DEF_BY_AUTO) {
        def_hor = pho;
        break;
      }
      pho = pho->Prev_ver();
    }
    
    if(def_hor) {
      
      SRCPOS_NODES nodes, nodes1, nodes2;
      SRCPOS_NODE last_node;
#if 0
      // do not expand stpath because it messes up the path
      STPATH *stpath = Comp_unit()->Dna()->Get_stpath(call,
                                              call->Rhs()->Opnd(0)->Ilod_base());
      if(stpath) {
        Is_True(stpath->St_idx() != ST_IDX_ZERO,
                ("invalid st_idx in stpath"));
        srcpos_h->Set_orig_stname(ST_name(stpath->St_idx()));
        for(int i = 0; i < stpath->Path_size(); i++) {
          if(!last_node.equal(stpath->Path(i))) {
            nodes1.push_back(stpath->Path(i));
            last_node = stpath->Path(i);
          }
        }
      }
#endif
      hash_set<IDTYPE> visited_bb;
      if(Get_alloc_path(&nodes, def_hor, call->Rhs()->Opnd(0)->Ilod_base(), visited_bb)) {
        for(int i = nodes.size() - 1; i >= 0 ; i --) {
          if(!last_node.equal(nodes[i])) {
            nodes1.push_back(nodes[i]);
             last_node = nodes[i];
          }
        }
      }
      /*
      for(int i =0; i < nodes1.size(); i++)
        srcpos_h->Append_data(nodes1[i]);
      */
      pho = hor;
      while(pho) {
        if(pho->Attr() == ROR_DEF_BY_FREE || pho->Attr() == ROR_DEF_BY_DANGLE) {
          nodes.clear();
          nodes2.clear();
          last_node.Clean();
          SRCPOS_NODE spos_node = pho->Srcpos_node();
          nodes2.push_back(spos_node);
          srcpos_h->Set_key_srcpos(spos_node.Dna(), spos_node.Stmt(), NULL);
#if 0
          // do not expand stpath because it messes up the path
          stpath = Comp_unit()->Dna()->Get_stpath(pho->Stmt_def(),
                                          pho->Stmt_def()->Rhs()->Opnd(0)->Ilod_base());
          if(stpath) {
            for(int i = 0; i < stpath->Path_size(); i++) {
              if(!last_node.equal(stpath->Path(i))) {
                last_node=stpath->Path(i);
                 nodes2.push_back(last_node);
              }
            }
          }
#endif
          visited_bb.clear();
          if(Get_alloc_path(&nodes, def_hor, pho->Stmt_def()->Rhs()->Opnd(0)->Ilod_base(), visited_bb)){
            for(int i = nodes.size() -1; i >= 0; i--) {
              if(!last_node.equal(nodes[i])) {
                nodes2.push_back(nodes[i]);
                last_node = nodes[i];
              }
            }
          }
          
          // merge nodes2 with nodes1
          int i1 = nodes1.size() - 1;
          int i2 = nodes2.size() - 1;
          while(i1 >= 0 && i2 >= 0 && nodes1[i1].equal(nodes2[i2])) {
            i1--;
            i2--;
          }
          // insert nodes2 from 0 to i2 to node2 after i1
          if(i2 >= 0) {
            int size1 = nodes1.size();
            nodes1.resize(size1 + i2 +1);
            // move [i1+1 , size1) to end for nodes1
            for(int i = size1 -1, j = nodes1.size() -1 ; i >= i1+1; i--, j--) 
              nodes1[j] = nodes1[i];
            // copy [0 , i2] from nodes2 to node1

            for(int i = i1+1, j=0; j <= i2; i++, j++)
              nodes1[i] = nodes2[j];
          }

          // srcpos_h->Append_data(pho->Srcpos_node());
          // for(int i = 0; i < nodes2.size() ; i++ )
          //   srcpos_h->Append_data(nodes2[i]);
        }
        pho = pho->Prev_ver();
      }
      for(int i =0; i < nodes1.size(); i++)
        srcpos_h->Append_data(nodes1[i]);
    }
  }
}

// =============================================================================
//
// VSA::Classify_vul_error, to find if the cr is 0 or not.
//   This overloaded function process CODEREP* cr, which is the 1st parameter.
//
//   kind == FOR_DIVISOR: report DBZ if cr is 0. Make sure cr is the second opnd
//                        of DIV/MOD/REM/DIVREM
//   kind == FOR_ILOD_BASE/FOR_ISTOR_BASE: report NPD if cr is 0. Make sure cr is
//                        in the addr expr of ILOAD/ISTORE/etc
//
// =============================================================================
void
VSA::Classify_vul_error(CODEREP* cr, BB_NODE* cur_bb, STMTREP* stmt,
                        CALL_STACK& cs, SRCPOS_HANDLE* sp_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited_bb)
{
  Is_True(cr != NULL, ("cr is NULL"));
  Is_True(sp_h != NULL, ("srcpos handle is NULL"));

  // if trun on new npd checker, ignore npd check for this checker
  if ((VSA_New_Npd_Checker() || PU_java_lang(Get_Current_PU())) && ((kind & FOR_ILOD_BASE) || (kind & FOR_ISTOR_BASE) || (kind & FOR_NPD))) {
    return;
  }

  if (cr->Kind() == CK_LDA)
    return;  // No NPD/DBZ on LDA

  // append stpath if current cr is propagated
  //sp_h->Append_stpath(stmt, cr, Comp_unit()->Dna(), FALSE);

  if (cr->Kind() == CK_CONST ||
      cr->Kind() == CK_RCONST ) {
    Is_True(stmt != NULL, ("const coderep without stmt"));
    // ignore vsym base *(*(0)) because it should be reported earlier when check *(0).
    if ((kind & FOR_VSYM_BASE) == FOR_VSYM_BASE)
      return;

    if ((cr->Kind() == CK_CONST && cr->Const_val() == 0) ||
        (cr->Has_const_fval() && cr->Const_fval() == 0.0 )) {
      //sp_h->Append_stpath(stmt, cr, Comp_unit()->Dna(), FALSE);
      if (Is_path_possible(stmt, cs, sp_h)) {
        // this stmt causes cr to be 0. treat it as the "key" srcpos for this issue
        sp_h->Set_key_srcpos(Dna(), stmt, cr);
        Report_vul_error(cr, sp_h, kind, IC_DEFINITELY);
        sp_h->Remove_last_key_srcpos();
      }
    }
    return;
  }

  // handle CK_OP
  if (cr->Kind() == CK_OP && Is_valid_lookup_virt_op(cr)) {
    // A lookup virt op, use the real value as cr
    cr = Get_lookup_virt_original_target_info(cr);
  } else if (cr->Kind() == CK_OP) {
    if ((kind & FOR_ILOD_BASE) != 0 || (kind & FOR_ISTOR_BASE) != 0) {
      // assume alloca is safe
      if (cr->Opr() == OPR_ALLOCA)
        return;
      // ignore BAND so far
      if (cr->Opr() == OPR_BAND)
        return;
      // analyze the base
      CODEREP* base = Comp_unit()->Analyze_base_info(stmt, cr, FALSE);
      if (base != NULL) {
        if (base->Kind() == CK_LDA)
          return; // No NPD/DBZ on LDA
        if (sp_h->Orig_stname() == NULL &&
            sp_h->Root_x()->Kind() != CK_VAR &&
            base->Kind() == CK_VAR) {
          sp_h->Set_orig_stname(Sym_name(base->Aux_id()));
        }
        sp_h->Append_stpath(stmt, base, Comp_unit()->Dna(), FALSE);
        Classify_vul_error(base, cur_bb, stmt, cs, sp_h, kind, visited_bb);
      }
      else {
        //sp_h->Append_stpath(stmt, cr, Comp_unit()->Dna(), FALSE);
        if (sp_h->Orig_stname() == NULL)
          sp_h->Set_orig_stname(sp_h->Find_cr_stname(cr, stmt, Comp_unit()->Dna(), TRUE, TRUE, TRUE));
        // no base pointer found in this stmt, treat it as the "key" srcpos for this issue
        sp_h->Set_key_srcpos(Dna(), stmt, cr);
        Report_vul_error(cr, sp_h, kind, IC_MAYBE);
        sp_h->Remove_last_key_srcpos();
      }
    }
    else if ((kind & FOR_DIVISOR) != 0) {
      if(cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL) {
        Classify_vul_error(cr->Opnd(0), cur_bb, stmt, cs, sp_h, kind, visited_bb);
      } else if(cr->Opr() == OPR_MPY) {
        Classify_vul_error(cr->Opnd(0), cur_bb, stmt, cs, sp_h, kind, visited_bb);
        Classify_vul_error(cr->Opnd(1), cur_bb, stmt, cs, sp_h, kind, visited_bb);
      } else if(cr->Opr() == OPR_BAND || cr->Opr() == OPR_BXOR) {
        sp_h->Set_flag(SRCPOS_FLAG_MAYBE);
        Classify_vul_error(cr->Opnd(0), cur_bb, stmt, cs, sp_h, kind, visited_bb);
        Classify_vul_error(cr->Opnd(1), cur_bb, stmt, cs, sp_h, kind, visited_bb);
      } else if(cr->Opr() == OPR_DIV || cr->Opr() == OPR_MOD ||
                cr->Opr() == OPR_REM || cr->Opr() == OPR_DIVREM) {
        Classify_vul_error(cr->Opnd(0), cur_bb, stmt, cs, sp_h, kind, visited_bb);
      }
    }
    return;
  }

  Is_True(cr->Kind() == CK_VAR || cr->Kind() == CK_IVAR,
          ("Only VAR/IVAR is allowed here"));
  if (cr->Kind() != CK_VAR && cr->Kind() != CK_IVAR)
    return;

  // check value range for VAR at first
  // TODO: srcpos_handle for value range
  BOOL is_vra = FALSE;
  VAL_RANGE_RESULT rr = Check_expr_zero(cr, cur_bb, is_vra);

  if (rr == VAL_INR && is_vra == TRUE) {
    // value range considers it's not zero
    return;
  }

  ST *st = NULL;
  if (cr->Kind() == CK_VAR) {
    AUX_ID aux = cr->Aux_id();
    AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(aux);
    if (sym->St() != NULL)
      st = sym->St();

    if (rr != VAL_INR && is_vra == TRUE &&
        !cr->Is_flag_set(CF_DEF_BY_PHI) &&  // not report PHI here for more precise path handled later
        !cr->Value_invalid_addr() && !cr->Value_maydangling()) {
      //sp_h->Append_stpath(stmt, cr, Comp_unit()->Dna(), FALSE);
      if (rr == VAL_OOR || (rr == VAL_May_OOR && VSA_Vra_Mayzero)) {
        // TODO: need VRA to tell which condition makes this OOR
        SRCPOS spos = Get_bb_first_linenum(cur_bb);
        const char *name = sp_h->Find_cr_stname(cr, stmt, Dna());
        sp_h->Set_key_srcpos(Dna(), cur_bb, spos, name);
        Report_vul_error(cr, sp_h, kind, (rr == VAL_May_OOR) ? IC_MAYBE : IC_DEFINITELY);
        sp_h->Remove_last_key_srcpos();
        return;
      }
    }
  }

  //if (cr->Kind() == CK_VAR &&
  //     cr->Value_checked() &&
  //    !cr->Value_invalid_addr() && !cr->Value_maydangling())
  //  return;

  if (cr->Kind() == CK_VAR &&
      cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = cr->Defphi();
    Is_True(phi != NULL, ("invalid def phi"));
    BB_NODE *phi_bb = phi->Bb();

    if (visited_bb.find(phi_bb->Id()) != visited_bb.end())
      return;
    visited_bb.insert(phi_bb->Id());

    INT zero_cnt = 0;
    INT32 pred_idx = 0;
    BOOL is_vra;
    CODEREP *opnd;
    PHI_OPND_ITER phi_iter(phi);
    vector<BOOL> check_opnd(phi->Size(), FALSE);
    FOR_ALL_ELEM(opnd, phi_iter, Init()) {
      //if (opnd->Value_invalid_addr() || opnd->Value_maydangling())
      if (Check_var_zero(opnd, phi_bb->Nth_pred(pred_idx), is_vra) != VAL_INR &&
          sp_h->Path()->Is_path_possible(phi, pred_idx) != PATH_NOT_REACHABLE) {
        check_opnd[pred_idx] = TRUE;
        ++ zero_cnt;
      }
      ++ pred_idx;
    }

    if (zero_cnt == 0)
      return;

    sp_h->Append_data(phi_bb,
                      Comp_unit()->Dna(),
                      PATHINFO_PHI);
    sp_h->Add_children(zero_cnt);
    SRCPOS_TREENODE *cur_node = sp_h->Cur_node();
    sp_h->Path()->Push_mark(phi);

    BB_NODE* pred;
    BB_LIST_ITER bb_iter;
    INT32 zero_pred_idx = 0;
    pred_idx = 0;
    FOR_ALL_ELEM(pred, bb_iter, Init(phi_bb->Pred())) {
      opnd = phi->OPND(pred_idx);
      if (check_opnd[pred_idx] == TRUE) {
        sp_h->Set_cur_node(cur_node, zero_pred_idx);
        sp_h->Set_flag(SRCPOS_FLAG_MAYBE);
        sp_h->Append_data(pred,
                          Comp_unit()->Dna(),
                          PATHINFO_BRANCH);
        sp_h->Path()->Add_bb(pred);
        Classify_vul_error(opnd, pred, NULL, cs, sp_h, kind, visited_bb);
        ++ zero_pred_idx;
      }
      ++ pred_idx;
      sp_h->Path()->Pop_mark(phi, FALSE);
    }
    sp_h->Path()->Pop_mark(phi, TRUE);
    return;
  }

  // CK_VAR defined by ENTRY_CHI
  if (cr->Kind() == CK_VAR && 
      cr->Is_flag_set(CF_DEF_BY_CHI)) {
    if (cr->Def_at_entry() && st != NULL) {
      STMTREP *defstmt = cr->Defstmt();
      Is_True(defstmt->Bb()->Kind() == BB_ENTRY,
              ("def bb is not entry"));
      if (defstmt->Bb()->Labnam() > 0) {
        // EH handler
        WN* entrywn = defstmt->Bb()->Entrywn();
        Is_True(entrywn != NULL &&
                WN_operator(entrywn) == OPR_LABEL,
                ("invalid entry wn"));
        Is_True(WN_Label_Is_Handler_Begin(entrywn),
                ("entry wn is not handler"));
        Check_eh_paths_for_vul(cr, cur_bb, defstmt, cs, sp_h, kind, visited_bb);
        return;
      }
      // real function entry
      SRCPOS st_pos = ST_Srcpos(*st);
      if (st_pos == 0)
        st_pos = Cfg()->Entry_spos();
      sp_h->Append_data(st, defstmt->Bb(), Comp_unit()->Dna(), PATHINFO_ST_DECLARE);
      switch (ST_sclass(st)) {
      case SCLASS_AUTO:
        // also report a NPD for UIV dereference
        if ((kind & FOR_ILOD_BASE) != 0 || (kind & FOR_ISTOR_BASE) != 0) {
          // treat the declaration of the st as the "key" srcpos for this issue
          const char *name = sp_h->Find_cr_stname(cr, stmt, Dna());
          sp_h->Set_key_srcpos(Dna(), NULL, st_pos, name);
          Report_vul_error(cr, sp_h, kind, IC_DEFINITELY);
          sp_h->Remove_last_key_srcpos();
        }
        break;
      case SCLASS_FORMAL:
      case SCLASS_FORMAL_REF:
      case SCLASS_REG:
        if (IPSA_insession() && !Comp_unit()->Dna()->Is_root()) {
          Check_callers_argument_for_uiv(cr, cs, sp_h, kind);
        }
        else if (VSA_Warn_On_Param) {
          // treat the declaration of the st as the "key" srcpos for this issue
          const char *name = sp_h->Find_cr_stname(cr, stmt, Dna());
          sp_h->Set_key_srcpos(Dna(), NULL, st_pos, name);
          Report_vul_error(cr, sp_h, kind, IC_MAYBE);
          sp_h->Remove_last_key_srcpos();
        }
        break;
      case SCLASS_PSTATIC:
        // TODO: pstatic? seems need value range
        break;
      case SCLASS_COMMON:
      case SCLASS_FSTATIC:
      case SCLASS_UGLOBAL:
      case SCLASS_DGLOBAL:
      case SCLASS_EXTERN:
        // TODO: side-effect model for global variable
        if (IPSA_insession() && !Comp_unit()->Dna()->Is_root()) {
          Check_callers_global_for_uiv(ST_st_idx(st), cs, sp_h, kind);
        }
        else if (VSA_Global_Var_Zero &&
                 ST_initialized_zero(st, cr->Offset()) &&
                 ((kind & FOR_ILOD_BASE) != 0 ||
                  (kind & FOR_ISTOR_BASE) != 0)) {
          if (!PU_java_lang(Get_Current_PU())) {
            // treat the declaration of the st as the "key" srcpos for this issue
            const char *name = sp_h->Find_cr_stname(cr, stmt, Dna());
            sp_h->Set_key_srcpos(Dna(), NULL, st_pos, name);
            Report_vul_error(cr, sp_h, kind, IC_MAYBE);
            sp_h->Remove_last_key_srcpos();
          }
        }
        break;
      }
    }
  }

  if (cr->Kind() == CK_IVAR) {
    // ignore MU node -- too much false positives
    //MU_NODE* mnode = cr->Ivar_mu_node();
    //if (mnode) {
    //  Classify_vul_error(mnode->OPND(), cur_bb, stmt, sp_h, kind, visited_bb);
    //}

    VSYM_OBJ_REP *vor = Cr_2_vor(cr);
    if (vor != NULL) {
      Classify_vul_error(cr, cur_bb, stmt, vor, cs, sp_h, kind, visited_bb, FALSE);
    }
    return;
  }

  // CK_VAR defined by STMT or CHI
  STMTREP* def = cr->Get_defstmt();
  if (def != NULL) {
    if (def->Opr() == OPR_INTRINSIC_CALL) {
      Check_intrn_call_for_vul(cr, cur_bb, def, cs, sp_h, kind, visited_bb);
      return;
    }
    else if (OPERATOR_is_call(def->Opr())) {
      RNA_NODE *rna = Sr_2_rna(def);
      if (rna != NULL) {
        // check callee, either return value or output parameter
        //Is_True(rna->Uniq_callee() != INVALID_RNA_PU_IDX,
        //        ("invalid callee idx"));
        if (cr->Kind() == CK_VAR) {

          IDTYPE which_arg;
          if (Callee_may_taint_arg(def, cr, &which_arg )) {
            sp_h->Append_data(def, Comp_unit()->Dna(), PATHINFO_DNA_CALLRETURN);
            // treat the def stmt as the "key" srcpos for this issue
            sp_h->Set_key_srcpos(Dna(), def, cr);
            Report_vul_error(cr, sp_h, kind, IC_MAYBE);
            sp_h->Remove_last_key_srcpos();
            return;
          }

          Is_True(cr->Is_flag_set(CF_DEF_BY_CHI) && cr->Defchi(),
                  ("not def by chi"));
          CHI_NODE *chi = cr->Defchi();
          AUX_STAB_ENTRY* aux = _opt_stab->Aux_stab_entry(cr->Aux_id());
          if (aux->Is_return_preg() ||
              aux->Is_global() ||
              rna->Get_arg(chi->OPND(), this).first != INVALID_VAR_IDX) {
            sp_h->Append_data(def, Comp_unit()->Dna(), PATHINFO_DNA_CALLRETURN);
            cs.push(rna);
            INT size = cs.size();
            Check_callee_return_value_for_xfa(cr, cs, sp_h, kind);
            Is_True(cs.size() == size && cs.top() == rna,
                    ("call stack corrupted"));
            cs.pop();
            return;
          }
          else {
            // skip this call
             Classify_vul_error(chi->OPND(), cur_bb, def, cs, sp_h, kind, visited_bb);
          }
        }
        // TODO: IVAR
      }
      if (def->Callee_returns_new_heap_memory()) {
        HEAP_OBJ_REP *hor = Cr_2_heap_obj(def->Rhs());
        if (hor) { 
          // this function returns heap_obj_rep which is maydef
          // however, report immediately will result in false positive
          // must check if the reference is guarded with null pointer check
          if (Dna()->Report_malloc_maynpd()) {
            // treat the stmt as the "key" srcpos for this issue
            sp_h->Set_key_srcpos(Dna(), def, cr);
            Report_vul_error(cr, sp_h, kind, IC_MAYBE);
            sp_h->Remove_last_key_srcpos();
          }
        }
      }
    }
    else if (def->Opr() == OPR_OPT_CHI) {
      CHI_NODE* chi = cr->Defchi();
      Is_True(chi != NULL, ("def chi is NULL"));
      sp_h->Append_stpath(def, chi->OPND(), Comp_unit()->Dna(), FALSE);
      Classify_vul_error(chi->OPND(), cur_bb, def, cs, sp_h, kind, visited_bb);
    }
    else if (!cr->Is_flag_set(CF_DEF_BY_CHI)) {
      Is_True(def->Rhs() != NULL, ("def rhs stmt is NULL"));
      // do not append data if def is stid to preg. for example:
      // Line 1:  a = b;
      // convert to:
      // Line 1:  preg = b;
      // Line 1:  a = preg;
      // Line 1 should only be added once
      if ((st != NULL && ST_class(st) == CLASS_VAR) ||
          def->Rhs()->Kind() == CK_CONST)
        sp_h->Append_data(def, Comp_unit()->Dna(), PATHINFO_COPY);
      sp_h->Append_stpath(def, def->Rhs(), Comp_unit()->Dna(), FALSE);
      Classify_vul_error(def->Rhs(), cur_bb, def, cs, sp_h, kind, visited_bb);
    }
  }
}

// =============================================================================
//
// VSA::Classify_vul_error, to find if the x is 0 or not.
//   kind == FOR_DIVISOR: report DBZ if x is 0. Make sure x is the second opnd
//                        of DIV/MOD/REM/DIVREM
//   kind == FOR_ILOD_BASE/FOR_ISTOR_BASE: report NPD if x is 0. Make sure x is
//                        in the addr expr of ILOAD/ISTORE/etc
//
// =============================================================================
void
VSA::Classify_vul_error(CODEREP* cr, BB_NODE* cur_bb, STMTREP* stmt, VSYM_OBJ_REP* vor,
                        CALL_STACK& cs, SRCPOS_HANDLE* sp_h, ILODSTORBASE kind,
                        hash_set<IDTYPE> &visited_bb, BOOL back_edge)
{
  switch (vor->Attr()) {
  case ROR_DEF_BY_CHI:
    {
      STMTREP *def_stmt = vor->Stmt_def();
      if (vor->Is_entry_chi() ||
          (def_stmt && def_stmt->Opr() == OPR_OPT_CHI)) {
        CODEREP* ilod_base = Find_base_pointer_load(cr);
        if(ilod_base == NULL)
          break;
        Check_callers_argument_for_uiv(ilod_base, cs, sp_h, (ILODSTORBASE)(kind | FOR_VSYM_BASE));
      }
      else {
        Is_True(def_stmt, ("vsym object has no define statement"));
        if(OPERATOR_is_call(def_stmt->Opr())) {
          CODEREP* ilod_base = Find_base_pointer_load(cr);
          if (ilod_base == NULL || ilod_base->Kind() != CK_VAR)
            break;
          RNA_NODE *rna = Sr_2_rna(def_stmt);
          if (rna == NULL)
            break;
          cs.push(rna);
          INT size = cs.size();
          Check_callee_return_value_for_xfa(ilod_base, cs, sp_h, (ILODSTORBASE)(kind | FOR_VSYM_BASE));
          Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
          cs.pop();
        }
        else if (def_stmt->Rhs()) {
          // EH handling here if def_stmt is label
          Classify_vul_error(def_stmt->Rhs(), def_stmt->Bb(), def_stmt, cs, sp_h, (ILODSTORBASE)(kind), visited_bb);
        }
      }
    }
    // TODO: other cases
    break;
  case ROR_DEF_BY_PHI:
  case ROR_DEF_BY_HORPHI:
    {
      // visit guard
      PHI_NODE  *phi = vor->Phi_def();
      BB_NODE *phibb = phi->Bb();
      if (visited_bb.find(phibb->Id()) != visited_bb.end())
        return;
      visited_bb.insert(phibb->Id());

      SRCPOS_TREENODE* cur_node = sp_h->Add_children(phi->Size());
      sp_h->Path()->Push_mark(phi);

      //PHI_OPND_ITER phi_opnd_iter(phi);
      BB_NODE      *bb_pred;
      BB_LIST_ITER  bb_iter;
      INT32         opnd_idx = 0;
      //FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
      FOR_ALL_ELEM(bb_pred, bb_iter, Init(phibb->Pred())) {
        if (sp_h->Path()->Is_path_possible(phi, opnd_idx) != PATH_NOT_REACHABLE) {
          VSYM_OBJ_REP *vor_opnd = (VSYM_OBJ_REP*) phi->OPND(opnd_idx);
          sp_h->Set_cur_node(cur_node, opnd_idx);
          sp_h->Append_data(bb_pred, Dna(), PATHINFO_BRANCH);
          sp_h->Path()->Add_bb(bb_pred);
          Classify_vul_error(cr, bb_pred, stmt, vor_opnd, cs, sp_h, kind, visited_bb,
                             back_edge || phibb->Dominates(bb_pred));
          sp_h->Path()->Pop_mark(phi, FALSE);
        }
        ++ opnd_idx;
      }
      sp_h->Path()->Pop_mark(phi, TRUE);
    }
    break;
  case ROR_DEF_BY_COPY:
  case ROR_DEF_BY_ISTORE:
    {
      STMTREP* def = vor->Stmt_def();
      if (def == NULL)
        break;
      if (back_edge &&
          def->Opr() == OPR_ISTORE &&
          cr->Opr() == OPR_ILOAD &&
          def->Lhs()->Istr_base() == cr->Ilod_base())
        break;
      Classify_vul_error(def->Rhs(), cur_bb, def, cs, sp_h, kind, visited_bb);
    }
    break;
  case ROR_DEF_BY_NONE:
    //Is_True(FALSE, ("def by none?"));
    break;
  case ROR_DEF_BY_NULL:
    // TODO: report NPD/DBZ here?
    break;
  default:
    Is_True(FALSE, ("TODO: handle vor def by: %d", vor->Attr()));
    break;
  }
}

// =============================================================================
//
// VSA::Report_vul_error: report error according to ILODSTORBASE kind and VAL_RANGE_RESULT
//
// =============================================================================
BOOL
VSA::Report_vul_error(CODEREP* x, SRCPOS_HANDLE* sp_h, ILODSTORBASE kind, ISSUE_CERTAINTY ic)
{
  // not report M NPD/DBZ if x is param and VSA_Warn_Param is off
  if (!VSA_Warn_On_Param && ic != IC_DEFINITELY &&
      x && x->Kind() == CK_VAR && Dna()->Is_param(x))
    return FALSE;

  AUX_ID aux = 0;
  if (x != NULL && x->Kind() == CK_VAR)
    aux = x->Aux_id();
  else if (sp_h->Root_x()->Kind() == CK_VAR) {
    if (sp_h->Context() == Comp_unit()->Dna()) 
      aux = sp_h->Root_x()->Aux_id();
    else if (sp_h->Orig_stname() == NULL && sp_h->Context() != Comp_unit()->Dna()) {
      DNA_NODE* dna = sp_h->Context();
      CONTEXT_SWITCH context(dna);
      AUX_ID tmp = sp_h->Root_x()->Aux_id();
      sp_h->Set_orig_stname(dna->Comp_unit()->Vsa()->Sym_name(tmp));
    }
  }

  if (VSA_Issue_Certainty_Maybe)
    ic = IC_MAYBE;

  if ((kind & FOR_DIVISOR) != 0) {
    sp_h->Set_msgid("DBZ.1");
    return Report_vsa_error(x, aux, DBZ, ic, sp_h);
  }
  else if ((kind & FOR_ILOD_BASE) != 0 || (kind & FOR_ISTOR_BASE) != 0) {
    sp_h->Set_msgid("NPD.1");
    return Report_vsa_error(x, aux, NPD, ic, sp_h);
  } else if ((kind & FOR_UIV) != 0) {
    sp_h->Set_msgid("UIV.1");
    return Report_vsa_error(x, aux, UIV, ic, sp_h);
  } else if ((kind & FOR_AOB) != 0) {
    sp_h->Set_msgid("AOB.1");
    return Report_vsa_error(x, aux, AOB, ic, sp_h);
  }
  else {
    Is_True(FALSE, ("unknown kind: %x", kind));
    return FALSE;
  }
}

// =============================================================================
// Find_hor_def
// =============================================================================
HEAP_OBJ_REP*
VSA::Find_or_create_hor(CODEREP *x, BB_NODE* bb, MEM_POOL *def_bbs_pool)
{
  Is_True(x->Kind() == CK_VAR, ("not CK_VAR"));
  Is_True(Cr_2_heap_obj(x) == NULL, ("x already has a hor"));

  // do not create ho for volatile variable
  if (x->Is_var_volatile())
    return NULL;

  if (x->Is_flag_set(CF_DEF_BY_PHI)) {
    // not handle phi result here. it will be handled in Vsa_bb
#if 0
    PHI_NODE *phi = x->Defphi();
    hor->Set_attr(ROR_DEF_BY_PHI);
    hor->Set_phi_def(phi);
    hor->Set_srcpos_node(phi->Bb(), Comp_unit()->Dna(), PATHINFO_PHI);
#endif
    return NULL;
  }

  HEAP_OBJ *ho_w_matching_auxid = Find(x->Aux_id(), FALSE);
  if (ho_w_matching_auxid != NULL) {
    HEAP_OBJ_REP* hor = Allocate_heap_obj(ho_w_matching_auxid, NULL);
    ho_w_matching_auxid->Prepend_def_bbs(bb, def_bbs_pool);
    return hor;
  }

  if (x->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP* stmt = x->Defstmt();
    Is_True(stmt != NULL, ("defstmt is NULL"));
    OPERATOR opr = stmt->Opr();
    HEAP_OBJ_REP* hor;
    if (opr == OPR_OPT_CHI) {
      hor = Allocate_heap_obj(x, bb, def_bbs_pool);
      hor->Heap_obj()->Set_kind(RSC_KIND_IPARM);
      hor->Set_attr(ROR_DEF_BY_CHI);
      hor->Set_srcpos_node(stmt, Comp_unit()->Dna(), PATHINFO_CHI);
    }
    else if (OPERATOR_is_call(opr)) {
      hor = Allocate_heap_obj(x, bb, def_bbs_pool);
      hor->Set_attr(ROR_DEF_BY_CHI);
      hor->Set_srcpos_node(stmt, Comp_unit()->Dna(), PATHINFO_CALL_CHI);
    }
    else if (OPERATOR_is_store(opr)) {
      hor = Allocate_heap_obj(x, bb, def_bbs_pool);
      hor->Set_attr(ROR_DEF_BY_ISTORE);
      hor->Set_srcpos_node(stmt, Comp_unit()->Dna(), PATHINFO_COPY);
    }
    else if (opr == OPR_ASM_STMT || opr == OPR_BACKWARD_BARRIER) {
      return NULL;  // assume these operators do not impacr HOR
    }
    else {
      Is_True(FALSE, ("TODO: handle opr %s", OPERATOR_name(opr) + 4));
      return NULL;
    }
    Is_True(hor->Heap_obj()->Sym_id() == x->Aux_id(), ("aux id mismatch"));
    hor->Set_stmt_def(stmt, Comp_unit()->Dna());
    return hor;
  }

  STMTREP* stmt = x->Defstmt();
  CODEREP* base = stmt->Rhs();  //bug: Find_base_pointer_load(stmt->Rhs());
  if (base != NULL && base->Kind() == CK_VAR) {
    // TODO: LDA/IVAR??
    HEAP_OBJ_REP *hor = Cr_2_heap_obj(base);
    if (hor == NULL) {
      hor = Find_or_create_hor(base, stmt->Bb(), def_bbs_pool);
      if (hor != NULL)
        Enter_cr_heap_obj_map(base, hor); // annotate hor to base
    }
    return hor;
  }
  else {
    return NULL;
  }
}

// =============================================================================
//
//  VSA::Classify_uiv_error that focus on refined vsym UIV error
//
// =============================================================================
void
VSA::Classify_uiv_error(CODEREP *ilod, BB_NODE* cur_bb, STMTREP *stmt, VSYM_OBJ_REP *vor,
                        CALL_STACK& cs, SRCPOS_HANDLE *srcpos_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited_bb)
{
  VSYM_TRACKER tracker;
  CODEREP     *cr = ilod;
  if ( tracker.Init(this, &cr, stmt,  Loc_pool()) != CS_VSYM_UD )
    return;

  switch (vor->Attr()) {
  case ROR_DEF_BY_CHI:
  {
    STMTREP *sr = vor->Stmt_def();
    if (vor->Is_entry_chi() || (sr && sr->Opr() == OPR_OPT_CHI)) {
      Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Line%d VSA::Found refined vsym UIV: ",
                           Srcpos_To_Line(stmt->Linenum())));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, "\n"));
      Is_Trace_cmd(Tracing(), ilod->Print(2, TFile));
      Is_Trace_cmd(Tracing(), stmt->Print(TFile));
      // WORK IN PROGRESS
      // need to check caller if the memory has been initialized by caller,
      // it is definite uninitialized if we can prove it, otherwise, maybe
      // in either case, we need to synthesize (* base) for error report
      CODEREP* ilod_base = Find_base_pointer_load(ilod->Ilod_base());
      if (ilod_base == NULL)
        break;

      VSA_ADDRESS_INFO info;
      if (Comp_unit()->Analyze_address_info(stmt, ilod, &info, FALSE, FALSE) == FALSE)
        break;

      HEAP_OBJ_REP *base_hor = vor->Vsym_obj()->Base_hor();
      if (base_hor->Heap_obj()->Kind() == RSC_KIND_LDA &&
          (info.Base() && info.Base()->Kind() == CK_VAR &&
           info.Base()->Defstmt() &&
           info.Base()->Defstmt()->Opr() == OPR_STID &&
           info.Base()->Defstmt()->Rhs()->Kind() == CK_OP &&
           info.Base()->Defstmt()->Rhs()->Opr() == OPR_ALLOCA)) {
        // try hard to eliminate any IVAR which has its base created by alloca
        STMTREP* def = info.Base()->Defstmt();
        // treat the def stmt as the "key" srcpos for this issue
        srcpos_h->Set_key_srcpos(Dna(), def, ilod);
        srcpos_h->Set_msgid("UIV.1");
        Report_vsa_error(ilod, (const char *)NULL, UIV, IC_DEFINITELY, srcpos_h);
        srcpos_h->Remove_last_key_srcpos();
      }
      else if (ilod_base->Kind() == CK_VAR &&
               Comp_unit()->Dna()->Is_root() && Dna()->Is_param(ilod_base)) {
        if (VSA_Param_Uiv) {
          // treat the declaration of the st as the "key" srcpos for this issue
          const char *name = srcpos_h->Find_cr_stname(cr, sr, Dna());
          srcpos_h->Set_key_srcpos(Dna(), NULL, WN_Get_Linenum(Comp_unit()->Input_tree()), name);
          srcpos_h->Set_msgid("UIV.1");
          Report_vsa_error(ilod, ilod_base->Aux_id(), UIV, IC_MAYBE, srcpos_h);
          srcpos_h->Remove_last_key_srcpos();
        }
      }
      else {
        Is_True(ilod_base->Kind() == CK_VAR ||
                ilod_base->Kind() == CK_LDA,
                ("unexpected ilod base kind"));
        if (ilod_base->Kind() != CK_VAR && ilod_base->Kind() != CK_LDA)
          break;
        AUX_ID aux = ilod_base->Kind() == CK_VAR ? ilod_base->Aux_id()
                                                 : ilod_base->Lda_aux_id();
        AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(aux);
        Is_True(sym->St() != NULL, ("no st found"));
        if (sym->St() == NULL)
          break;

        switch (ST_sclass(sym->St())) {
        case SCLASS_AUTO:
          break;  // should report error?
        case SCLASS_FORMAL:
        case SCLASS_FORMAL_REF:
        case SCLASS_REG:
          if (IPSA_insession())
            Check_callers_argument_for_uiv(ilod_base, cs, srcpos_h, FOR_ILOD_REF, &tracker);
          break;
        case SCLASS_PSTATIC:
          break;  // should report error?
        case SCLASS_COMMON:
        case SCLASS_FSTATIC:
        case SCLASS_UGLOBAL:
        case SCLASS_DGLOBAL:
        case SCLASS_EXTERN:
          if (IPSA_insession() && !Comp_unit()->Dna()->Is_root())
            Check_callers_global_for_uiv(ST_st_idx(sym->St()),
                                         cs, srcpos_h, FOR_ILOD_REF);
          break;
        }
      }
    }
    else if (sr->Opr() == OPR_CALL &&
             Callee_returns_new_heap_memory(sr) &&
             Is_alloc_not_initialized(sr)) {
      srcpos_h->Append_data(sr, Comp_unit()->Dna(), PATHINFO_COPY);
      // treat the call stmt as the "key" srcpos for this issue
      srcpos_h->Set_key_srcpos(Dna(), sr, ilod);
      srcpos_h->Set_msgid("UIV.1");
      Report_vsa_error(ilod, (char*)NULL, UIV, IC_DEFINITELY, srcpos_h);
      srcpos_h->Remove_last_key_srcpos();
    }
  }
  break;
  case ROR_DEF_BY_PHI:
    {
      // prd if any of the phi operand is entry chi
      PHI_NODE  *phi = vor->Phi_def();
      BB_NODE *phibb = phi->Bb();
      if (visited_bb.find(phibb->Id()) != visited_bb.end())
        return;
      visited_bb.insert(phibb->Id());

      SRCPOS_TREENODE* cur_node = srcpos_h->Add_children(phi->Size());
      srcpos_h->Path()->Push_mark(phi);

      INT32         opnd_idx = 0;
      BB_NODE      *bb_pred;
      BB_LIST_ITER  bb_iter;
      FOR_ALL_ELEM(bb_pred, bb_iter, Init(phibb->Pred())) {
        if (srcpos_h->Path()->Is_path_possible(phi, opnd_idx) != PATH_NOT_REACHABLE) {
          srcpos_h->Set_cur_node(cur_node, opnd_idx);
          srcpos_h->Append_data(bb_pred, Dna(), PATHINFO_BRANCH);
          srcpos_h->Path()->Add_bb(bb_pred);
          VSYM_OBJ_REP *vor_opnd = (VSYM_OBJ_REP*) phi->OPND(opnd_idx);
          Classify_uiv_error(ilod, bb_pred, stmt, vor_opnd, cs, srcpos_h, kind, visited_bb);
          srcpos_h->Path()->Pop_mark(phi, FALSE);
        }
        ++ opnd_idx;
      }
      srcpos_h->Path()->Pop_mark(phi, TRUE);
    }
    break;
  case ROR_DEF_BY_COPY:
    {
      STMTREP* stmt = vor->Stmt_def();
      if (stmt != NULL && stmt->Opr() == OPR_STID &&
          stmt->Rhs()->Kind() == CK_OP && stmt->Rhs()->Opr() == OPR_ALLOCA) {
        srcpos_h->Append_data(stmt, Comp_unit()->Dna(), PATHINFO_COPY);
        // treat the alloca stmt as the "key" srcpos for this issue
        srcpos_h->Set_key_srcpos(Dna(), stmt, NULL);
        srcpos_h->Set_msgid("UIV.1");
        Report_vsa_error(stmt->Lhs(), stmt->Lhs()->Aux_id(), UIV, IC_DEFINITELY, srcpos_h);
        srcpos_h->Remove_last_key_srcpos();
      }
    }
    break;
  case ROR_DEF_BY_ISTORE:
    break;
  default:
    break;
  }
}


// =============================================================================
//
//  VSA::Classify_dbf_error the final phase of DBF Error analysis
//
// =============================================================================
void
VSA::Classify_dbf_error(CODEREP *arg, STMTREP *stmt, HEAP_OBJ_REP *hor, hash_set<IDTYPE>& visited)
{
  if (VSA_New_Heap_Checker())
    return;  // do nothing if new heap checker is on

  switch (hor->Attr()) {
  case ROR_DEF_BY_FREE:
  case ROR_DEF_BY_DANGLE:
  {
    Is_Trace(Tracing(), (TFile, "VSA ERROR:Free after Free: heapID:%d, VerID:%d in the following stmt\n",
                         hor->Heap_obj()->Id(), hor->Version()));
    Is_Trace_cmd(Tracing(), stmt->Print(TFile));
    // don't pass arg to avoid stpath of arg messes up the whole path
    SRCPOS_HANDLE srcpos_h(NULL /*arg*/, stmt, _cu->Dna(), Loc_pool());
    Compose_dbf_path(&srcpos_h, hor, stmt);
    // treat the free stmt as the "key" srcpos for this issue
    srcpos_h.Set_key_srcpos(Dna(), stmt, arg);
    srcpos_h.Set_msgid("DBF.1");
    Report_vsa_error(arg, (char*)NULL, DBF, IC_DEFINITELY, &srcpos_h);
  }
  break;
  case ROR_DEF_BY_PHI:
  {
    // prd if any of the phi operand is entry chi
    PHI_NODE     *phi = hor->Phi_def();
    if (visited.find(phi->Bb()->Id()) != visited.end())
      return;
    visited.insert(phi->Bb()->Id());

    PHI_OPND_ITER phi_opnd_iter(phi);
    CODEREP      *opnd;
    BOOL          all_opnd_defined = TRUE;
    INT32         opnd_idx = 0;
    FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
      if (Is_path_possible(stmt->Bb(), phi, opnd_idx)) {
        HEAP_OBJ_REP *hor_opnd = (HEAP_OBJ_REP*) opnd;
        Classify_dbf_error(arg, stmt, hor_opnd, visited);
      }
      ++ opnd_idx;
    }
  }
  break;
  case ROR_DEF_BY_COPY:
  {
    HEAP_OBJ_REP *prev = hor->Prev_ver();
    if (prev == NULL) // TODO: not set HOR created by unknown ptr to COPY
      break;
    Is_True(prev != NULL, ("prev ver for COPY is NULL"));
    Classify_dbf_error(arg, stmt, prev, visited);
  }
  break;
  case ROR_DEF_BY_CHI:
  {
    // if IPSA() in session, we do the following:
    if (! IPSA_insession()) break;

    // if the pointer passed into free() is returned from a function call
    // we go into the function call and examine the return value
    STMTREP *defstmt = hor->Defstmt();
    if (defstmt && OPERATOR_is_call(defstmt->Opr())) {
    }

    // if the pointer is passed in from a call through parameter,
    // we trave to the caller
    if (defstmt && defstmt->Opr() == OPR_OPT_CHI) {
    }
  }
  break;
  default:
    break;
  }
}

// =============================================================================
//
//  VSA::Classify_dbf_error the final phase of DBF Error analysis
//
// =============================================================================
void
VSA::Classify_dbf_error(BB_NODE *bb)
{
  //  Iterate through each statement
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP     *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {

    OPERATOR opr = stmt->Opr();
    if ( OPERATOR_is_call(opr)) { // Process heap obj in calls
      CODEREP *actual_arg;
      BOOL rsc_free = FALSE;
      HEAP_OBJ_REP *rhor = Cr_2_heap_obj(stmt->Rhs());
      if (rhor != NULL)
        if (rhor->Attr() == ROR_DEF_BY_FREE || rhor->Attr() == ROR_DEF_BY_DANGLE)
          rsc_free = TRUE;
      BOOL maybe_free = FALSE;
      if (( actual_arg = Callee_frees_heap_memory(stmt, &maybe_free))!= NULL || rsc_free ) {


        if (rhor != NULL && actual_arg == NULL)
          actual_arg = stmt->Rhs()->Find_actual_arg();
        if (actual_arg == NULL ||
            (actual_arg->Kind() != CK_LDA &&  // added for user defined free
             actual_arg->Kind() != CK_VAR &&
             actual_arg->Kind() != CK_IVAR)) continue;
        // BOOL     is_legit = Is_legit_error(actual_arg->Aux_id());
        INT      ecode;

        // Vsa_stmt has already created the hor
        HEAP_OBJ_REP *hor = Cr_2_heap_obj_ref(stmt->Rhs()/*vcall_node*/);
        if (hor == NULL && actual_arg->Kind() == CK_IVAR && actual_arg->Opr() == OPR_ILOAD) {
          // This might result from the VSYM_OBJ_REP is defined by PHI
          VSYM_OBJ_REP *vor = Cr_2_vor(actual_arg);
          if (vor) {
            // Rename_voref for Rhs() did not handle vor that is defined by PHI
            // The logic to handle free() cannot be done in Rename_vocall
#if 0
            hash_set<IDTYPE> visited_bb;
            STMTREP *first_def = vor->Find_1st_istore(visited_bb);
            hor = (first_def)? Cr_2_heap_obj(first_def->Lhs()) :NULL;
#endif
            hor = vor->Vsym_obj()->Base_hor();
            // hor eliminates the false positive but not enough to detect the case that
            // malloc has failed.
          }
        }
        if (hor == NULL && VSA_Udr()) {
          if (!VSA_New_HVA) {
            Is_Trace(Tracing(), (TFile, "VSA ERROR: free a pointer that is not or partially defined\n"));
            Is_Trace_cmd(Tracing(), stmt->Print(TFile));
            SRCPOS_HANDLE srcpos_h(actual_arg, stmt, Comp_unit()->Dna(), Loc_pool(), this);
            if (actual_arg->Kind() == CK_IVAR)
              srcpos_h.Append_stpath(stmt, actual_arg->Ilod_base(), Dna(), FALSE);
            CODEREP* base = Find_base_pointer_load(actual_arg);
            // if pointer from parameter, set issue to maybe
            ISSUE_CERTAINTY maybe = (maybe_free ||
                                    (base && base->Kind() == CK_VAR &&
                                      Dna()->Is_param(base))) ? IC_MAYBE : IC_DEFINITELY;
            // treat the free stmt as the "key" srcpos for this issue
            srcpos_h.Set_key_srcpos(Dna(), stmt, actual_arg);
            srcpos_h.Set_msgid("UDR.1");
            Report_vsa_error(actual_arg, (char*)NULL, UDR, maybe, &srcpos_h);
          }
        }
        else if (VSA_Udr() &&
                 hor->Is_entry_chi() &&
                 IPSA_insession() && Comp_unit()->Dna()->Is_root()) {
          Is_Trace(Tracing(), (TFile, "VSA ERROR: free a pointer that is defined at function entry\n"));
          Is_Trace_cmd(Tracing(), stmt->Print(TFile));
          SRCPOS_HANDLE srcpos_h(actual_arg, stmt, Comp_unit()->Dna(), Loc_pool(), this);
          // treat the free stmt as the "key" srcpos for this issue
          const char *name = srcpos_h.Find_cr_stname(actual_arg, stmt, Dna());
          srcpos_h.Set_key_srcpos(Dna(), NULL, WN_Get_Linenum(Comp_unit()->Input_tree()), name);
          srcpos_h.Set_msgid("UDR.1");
          Report_vsa_error(actual_arg, (char*)NULL, UDR, IC_MAYBE, &srcpos_h);
        }
        else {
          hash_set<IDTYPE> visited;
          Classify_dbf_error(actual_arg, stmt, hor, visited);
        }
      } // check in free(ptr_var)
    }
    else if (opr == OPR_RETURN || opr == OPR_RETURN_VAL) {

      // Process HOR, heap obj rep, in the (HO version's exit_mu)
      // the following is semantics based on C/C++; Java to be analyzed
      Is_Trace(Tracing(), (TFile, "VSA checking Alloc w/o Free in the following stmt\n"));
      Is_Trace_cmd(Tracing(), stmt->Print(TFile));

      // go through ret bb's HOR_ARRAY and check missing free
      if (VSA_Msf() && !PU_java_lang(Get_Current_PU()))
        Classify_msf_error(bb);
    }

    // check Use-After-Free in stmt Lhs() & Rhs()
    if (VSA_Uaf() && !PU_java_lang(Get_Current_PU()))
      Classify_uaf_error(bb, stmt);
  }
  BB_NODE      *dom_bb;
  BB_LIST_ITER  dom_bb_iter;
  FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Classify_dbf_error(dom_bb);  /* child */
  }
}

// =============================================================================
//
//  VSA::Perform_heap_analysis reports partial redundant free errors
//
// =============================================================================
void
VSA::Perform_heap_analysis(CFG *cfg, MEM_POOL *def_bbs_pool)
{
  BB_NODE *bb;
  CFG_ITER cfg_iter;

  // Two prep work done in VSA::Vsa_bb call, which forces the phase ordering 
  // 1. heap_obj creation and its instances associate with malloc/free/assignment
  // 2. Collect def_bbs in heap_obj, for ho phi placement

  // Update deferred heap object reference
  Is_Trace(Tracing(),
           (TFile, "%sVSA::Perform_heap_analysis: Update Pending Ref\n%s", SBar, SBar) );
  Update_pending_ref(def_bbs_pool);

  if (Count_ho_list() != 0) {
    // For every BBs, setup the Phi_list.
    FOR_ALL_ELEM (bb, cfg_iter, Init(cfg))
      Enter_bb_ho_philist(bb, CXX_NEW( PHI_LIST(bb), Mem_pool() ));

    // Placement of heap_obj phi functions
    Is_Trace(Tracing(),
             (TFile, "%sVSA::Perform_heap_analysis: Place ho PHI\n%s", SBar, SBar) );
    Place_ho_phi_node();

    // Verify HO & VO stack before rename
    Verify_heap_obj_stack();

    // Rename heap_obj into SSA versions
    Is_Trace(Tracing(),
             (TFile, "%sVSA::Perform_heap_analysis: Rename Heap Object\n%s", SBar, SBar) );
    Rename(cfg->Entry_bb());

    // Verify heap object stack after rename
    Is_Trace(Tracing(), ( TFile, "%sVSA::Perform_heap_analysis: Verify Heap Object Stack\n%s", SBar, SBar));
    Verify_heap_obj_stack();
    
    Is_Trace(Tracing(),
             (TFile, "%sAfter VSA::Perform_heap_analysis: Dump Heap object list\n%s", SBar, SBar) );
    Is_Trace_cmd(Tracing(), Print_ho_list(TFile));

    // dump IR with hor
    Is_Trace_cmd(Tracing(), Print_hor(TFile));
  }
}

// =============================================================================
//
//  VSA::Perform_vsym_analysis reports partial redundant free errors
//
// =============================================================================
void
VSA::Perform_vsym_analysis(CFG *cfg, MEM_POOL *def_bbs_pool)
{
  // enable only if -ALIAS:restricted=on till we handle the forward analysis
  // if (!  Alias_Pointer_Restricted) return;

  BB_NODE *bb;
  CFG_ITER cfg_iter;

  // The prep work done in VSA::Create_refn_vsym, which forces the phase ordering 
  // 1. vsym_obj creation and its instances associate with heap_obj_rep
  // 2. Collect def_bbs in vsym_obj, for vo phi placement

  if (Count_vo_list() != 0) {
    // For every BBs, setup the Phi_list.
    FOR_ALL_ELEM (bb, cfg_iter, Init(cfg))
      Enter_bb_vo_philist(bb, CXX_NEW( PHI_LIST(bb), Mem_pool() ));
    // Placement of heap_obj phi functions

    Is_Trace(Tracing(),
             (TFile, "%sVSA::Perform_vsym_analysis: Place vo PHI\n%s", SBar, SBar) );
    Place_vo_phi_node();

    // Verify vsym object stack before rename
    Verify_heap_obj_stack();
    Verify_vsym_obj_stack();

    // Rename heap_obj into SSA versions
    Is_Trace(Tracing(),
             (TFile, "%sVSA::Perform_vsym_analysis: Rename Vsym Object\n%s", SBar, SBar) );
    Rename_vsym(cfg->Entry_bb(), def_bbs_pool);

    // Verify vsym object stack after rename
    Is_Trace(Tracing(), ( TFile, "%sVSA::Perform_vsym_analysis: Verify Vsym Object Stack\n%s", SBar, SBar));
    Verify_heap_obj_stack();
    Verify_vsym_obj_stack();

#if 1
    Is_Trace(Tracing(),
             (TFile, "%sAfter VSA::Perform_vsym_analysis: Dump Heap object list\n%s", SBar, SBar) );
    Is_Trace_cmd(Tracing(), Print_ho_list(TFile));

    // dump IR with hor
    Is_Trace_cmd(Tracing(), Ipsa()->Print_fld_name_map(TFile));
    Is_Trace_cmd(Tracing(), Print_hor(TFile));
#endif
  }
}

// =============================================================================
// VSA::Check_eh_paths(CODEREP *x, STMTREP *opt_chi, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind)
//
// =============================================================================
void
VSA::Check_eh_paths_for_vul(CODEREP *x, BB_NODE* curbb, STMTREP *opt_chi,
                            CALL_STACK& cs, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited)
{
  Is_True(x->Kind() == CK_VAR,
          ("x is not CK_VAR"));

  if (Comp_unit()->EH_table() == NULL)
    return;

  if (visited.find(opt_chi->Bb()->Id()) != visited.end())
    return;
  visited.insert(opt_chi->Bb()->Id());

  // overwrite curbb with sp_h->Path()->Origin_bb() to get full EH filtering control flow
  curbb = sp_h->Path()->Origin_bb();

  // filter == -1: catch all, or clean-up
  INT filter = -1;
  {
    std::vector< std::pair<BB_NODE*, BB_NODE*> > cds;
    Comp_unit()->Vra()->Collect_control_dependencies(curbb, opt_chi->Bb(), cds);
    std::vector< std::pair<BB_NODE*, BB_NODE*> >::iterator it;
    for (it = cds.begin(); it != cds.end(); ++it) {
      STMTREP* stmt = it->first->Last_stmtrep();
      if (stmt == NULL)
        continue;
      if (stmt->Rhs()->Kind() != CK_OP ||
          (stmt->Rhs()->Opr() != OPR_EQ &&
           stmt->Rhs()->Opr() != OPR_NE))
        continue;
      if (stmt->Rhs()->Opnd(0)->Kind() != CK_VAR ||
          stmt->Rhs()->Opnd(1)->Kind() != CK_CONST)
        continue;
      AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(stmt->Rhs()->Opnd(0)->Aux_id());
      ST* st = sym->St();
      if (ST_one_per_pu(st) && strcmp(ST_name(st), "__Exc_Filter__") == 0) {
        filter = stmt->Rhs()->Opnd(1)->Const_val();
        break;
      }
    }
  }

  std::vector<EH_PATH*> paths;
  EH_TABLE* eh_table = Comp_unit()->EH_table();
  EH_PATH_ITER iter(eh_table, opt_chi);
  while (!iter.Is_empty()) {
    EH_PATH* path = iter.Next();
    // get throw stmt
    STMTREP* stmt = path->Throw_stmt();
    STR_IDX eh_type = Comp_unit()->Dna()->Get_eh_throw_type(stmt);
    if (eh_type != 0) {
      if (eh_table->Is_eh_type_match(Comp_unit()->Dna()->File_idx(), eh_type, filter) == TRUE)
        paths.push_back(path);
    }
    else {
      // eh_type = 0 here, this is not a direct 'throw' stmt with a valid object
      // maybe this stmt is 'throw null' or
      // a call stmt to a function that may throw some exception.
      RNA_NODE* rna = Sr_2_rna(stmt);
      if (rna != NULL && rna->Uniq_callee() != INVALID_RNA_PU_IDX) {
        // If the stmt is a call (Not direct 'throw') which may throw exceptions
        DNA_NODE *callee = Ipsa()->Get_dna(rna->Uniq_callee());
        if (filter >= 0) {
          FS_PAIR eh_type = eh_table->Type_info(filter);
          if (callee->EH_table()->Throw_eh_type(eh_type.first, eh_type.second) == TRUE)
            paths.push_back(path);
        }
        else {
          EH_TYPE_VECTOR* tv = callee->EH_table()->Type_list();
          if (VSA_EH_Spec_Default == 1 && tv->size() == 0 &&
              ! PU_nothrow(Pu_Table[callee->Pu_idx()])) {
            paths.push_back(path);
            break;
          }
          EH_TYPE_VECTOR::iterator it;
          for (it = tv->begin(); it != tv->end(); ++it) {
            if (eh_table->Get_type_filter(it->first, it->second) == filter) {
              paths.push_back(path);
              break;
            }
          }
        }
      }
      else if (filter == -1) {
        // this is an external call and catch all, so add to path
        paths.push_back(path);
      }
    }
  }

  if (paths.size() == 0)
    return;

  // TODO: find out the right srcpos for catch
  sp_h->Append_data(opt_chi, Comp_unit()->Dna(), PATHINFO_EH_CATCH);
  SRCPOS_TREENODE *cur_treenode = sp_h->Cur_node();
  cur_treenode->Add_children(paths.size());
  sp_h->Path()->Push_mark(Comp_unit());

  INT32 i = 0;
  std::vector<EH_PATH*>::iterator it;
  for (it = paths.begin(); it != paths.end(); ++it) {
    EH_PATH* path = *it;
    STMTREP* call = path->Throw_stmt();
    CODEREP* opnd = NULL;
    // search chi list
    CHI_LIST_ITER chi_iter;
    CHI_NODE *cnode;
    FOR_ALL_NODE (cnode, chi_iter, Init(call->Chi_list())) {
      if (!cnode->Live())
        continue;
      if (x->Aux_id() == cnode->Aux_id()) {
        opnd = cnode->RESULT();
        break;
      }
    }

    if (opnd == NULL) {
      MU_LIST_ITER mu_iter;
      MU_NODE *mnode;
      FOR_ALL_NODE (mnode, mu_iter, Init(call->Mu_list())) {
        if (x->Aux_id() == mnode->Aux_id()) {
          opnd = mnode->OPND();
          break;
        }
      }
    }

    if (opnd == NULL) {
      // variable is defined and used in handler locally
      continue;
    }

    sp_h->Set_cur_node(cur_treenode, i);
    sp_h->Append_data(path->Throw_stmt(), Comp_unit()->Dna(), PATHINFO_EH_THROW);
    sp_h->Path()->Add_bb(call->Bb());
    if ((kind & FOR_UIV) != 0)
      Classify_uiv_error(opnd, call->Bb(), cs, sp_h, kind, visited);
    else if ((kind & (FOR_DIVISOR | FOR_NPD)) != 0)
      Classify_vul_error(opnd, call->Bb(), call, cs, sp_h, kind, visited);
    sp_h->Path()->Pop_mark(Comp_unit(), FALSE);
  }
  sp_h->Path()->Pop_mark(Comp_unit(), TRUE);
}

// =============================================================================
// VSA::Check_intrn_call_for_vul(CODEREP *x, BB_NODE* cur_bb, STMTREP* stmt, SRCPOS_HANDLE* sp_h,
//                               ILODSTORBASE kind, hash_set<IDTYPE> &visited_bb);
// =============================================================================
void
VSA::Check_intrn_call_for_vul(CODEREP *x, BB_NODE* cur_bb, STMTREP* stmt,
                              CALL_STACK& cs, SRCPOS_HANDLE* sp_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited)
{
  Is_True(stmt->Opr() == OPR_INTRINSIC_CALL,
          ("stmt is not intrinsic"));
  CODEREP* tc_cr = NULL;
  ILODSTORBASE tc_kind = FOR_NONSPECIFIC;

  INTRINSIC intrn = stmt->Rhs()->Intrinsic();
  CODEREP* rhs = stmt->Rhs();
  if (intrn == INTRN_CHECK_CAST) {
    Is_True(rhs->Kid_count() == 2,
            ("invalid kid count for CheckCast"));
    Is_True(rhs->Opnd(1)->Kind() == CK_IVAR &&
            rhs->Opnd(1)->Opr() == OPR_PARM,
            ("kid 1 is not OPR_PRAM"));
    tc_cr = rhs->Opnd(1)->Ilod_base();
    const char* stname = sp_h->Orig_stname();
    BOOL set_stname = (stname == NULL) || (stname[0] == '.');
    sp_h->Append_stpath(stmt, tc_cr, Comp_unit()->Dna(), set_stname);
    tc_kind = kind;
  }

  if (tc_cr != NULL) {
    if ((kind & FOR_UIV) != 0 && tc_cr->Kind() == CK_VAR)
      Classify_uiv_error(tc_cr, cur_bb, cs, sp_h, tc_kind, visited);
    else if ((kind & (FOR_DIVISOR | FOR_NPD)) != 0)
      Classify_vul_error(tc_cr, cur_bb, stmt, cs, sp_h, tc_kind, visited);
  }
}

// =============================================================================
//
// VSA::Check_callers_argument_for_uiv transfer control to DNA_NODE
//
// =============================================================================
void
VSA::Check_callers_argument_for_uiv(CODEREP *x, CALL_STACK& cs,
                                    SRCPOS_HANDLE *srcpos_h, ILODSTORBASE kind,
                                    VSYM_TRACKER *tracker)
{
  if (VSA_Compiletime_Triage() & 0x2) return;
  Is_True(IPSA_insession(),
          ("VSA::Check_callers_argument_for_uiv get called w/o -xa option"));
  DNA_NODE *my_dna = Comp_unit()->Dna();

  // Can't setup visit counter before the recursion on DNA-RNA graph here
  // This should be setup before the traversal starts
  //Ipsa()->New_trav_counter();
  //srcpos_h->Set_context(my_dna);
  my_dna->Check_callers_argument_for_uiv(Ipsa(), x, cs, srcpos_h, kind, tracker);
}

// =============================================================================
//
// VSA::Check_callers_global_for_uiv transfer control to DNA_NODE
//
// =============================================================================
void
VSA::Check_callers_global_for_uiv(ST_IDX st,
                                  CALL_STACK& cs, SRCPOS_HANDLE *srcpos_h, ILODSTORBASE kind)
{
  if (VSA_Compiletime_Triage() & 0x1) return;
  Is_True(IPSA_insession(),
          ("VSA::Check_callers_global_for_uiv get called w/o -xa option"));
  DNA_NODE *my_dna = Comp_unit()->Dna();
    
  UINT32 file_idx = my_dna->File_idx();
  if (file_idx != 0 &&
      ST_sclass(st) == SCLASS_EXTERN) {
    WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
    if (mgr != NULL)
      mgr->Resolve(file_idx, st, file_idx, st);
  }

  my_dna->Check_callers_global_for_uiv(Ipsa(), file_idx, st, cs, srcpos_h, kind);
}

// =============================================================================
//
// VSA::Check_callee_return_value_for_xfa transfer control to DNA_NODE
//
// =============================================================================
void
VSA::Check_callee_return_value_for_xfa(CODEREP *x,
                                       CALL_STACK& cs, SRCPOS_HANDLE *srcpos_h, ILODSTORBASE kind)
{
  Is_True(!cs.empty(), ("no call stack found"));
  RNA_NODE* rna = cs.top();
  Is_True(IPSA_insession(),
          ("VSA::Check_callee_return_value_for_xfa get called w/o -xa option"));
  Is_True(x->Kind() == CK_VAR,
          ("Only var is allowed"));

  // Can't setup visit counter before the recursion on DNA-RNA graph here
  // This should be setup before the traversal starts
  //Ipsa()->New_trav_counter();
  //srcpos_h->Set_context(Comp_unit()->Dna());

  if(Ipsa()->Is_jni_call(rna)) {
    return;
  }

  srcpos_h->Path()->Push_mark(Comp_unit());
  // in caller's context
  for(CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
      it != rna->Callee_list().end(); it++) {
    DNA_NODE* callee = Ipsa()->Get_dna(it->Callee());
    Is_True(callee != NULL, ("bad callee"));
    if (callee == NULL)
      continue;
    // skip RBC model and assert nodes.
    if(callee->Non_functional())
      continue;
    else {
      callee->Check_callee_side_effects_for_xfa(Ipsa(), x, cs, srcpos_h, kind);
    }
  }
  srcpos_h->Path()->Pop_mark(Comp_unit(), TRUE);
}

// =============================================================================
//
// VSA::Check_callee_return_value_for_xfa transfer control to DNA_NODE
//
// =============================================================================
void
VSA::Check_callee_istore_for_uiv(BB_NODE *curbb, IDTYPE arg, CODEREP *chi_opnd, CALL_STACK &cs,
                                 SRCPOS_HANDLE *sp_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited)
{
  Is_True(!cs.empty(), ("no call stack found"));
  RNA_NODE* rna = cs.top();
  Is_True(IPSA_insession(),
          ("VSA::Check_callee_istore_for_uiv get called w/o -xa option"));

  SRCPOS_TREENODE* cur_node = sp_h->Add_children(rna->Callee_list().size());
  sp_h->Path()->Push_mark(Comp_unit());

  INT i = 0;
  for (CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
       it != rna->Callee_list().end(); it++) {
    DNA_NODE* callee = Ipsa()->Get_dna(it->Callee());
    Is_True(callee != NULL, ("bad callee"));
    if (callee == NULL)
      continue;
    // ignore RBC model and assert dna
    if(callee->Non_functional())
      continue;
    sp_h->Set_cur_node(cur_node, i++);
    sp_h->Append_data(rna->Callstmt(), Comp_unit()->Dna(), PATHINFO_DNA_CALLRETURN);
    // switch to callee contexta
    CONTEXT_SWITCH ctx(callee);
    callee->Check_istore_for_uiv(Ipsa(), curbb, arg, chi_opnd, cs,
                                 sp_h, kind, visited);
    sp_h->Path()->Pop_mark(Comp_unit(), FALSE);
  }
  sp_h->Path()->Pop_mark(Comp_unit(), TRUE);
}

// =============================================================================
//
// VSA::Load_from_subset_memory_area checks the LDID x loads from subset of 
// the memory area that is stored into by the defstmt.
// This interface is called if x is defined by the chi of the defstmt.
//
// =============================================================================
BOOL
VSA::Load_from_subset_memory_area(STMTREP *defstmt, CODEREP *x)
{
  switch (defstmt->Opr()) {
  case OPR_STID:
    {
      IDTYPE stid_auxid = defstmt->Lhs()->Aux_id();
      IDTYPE ldid_auxid = x->Aux_id();
      if (stid_auxid == ldid_auxid && defstmt->Lhs()->Offset() == x->Offset())
        return TRUE;
      else
        return FALSE;
    }
    break;
  case OPR_ISTORE:
  case OPR_ISTBITS:
    {
      // will need to handle field sensitivity
    }
    break;
  case OPR_MSTORE:
    {
      // will need to handle field sensitivity
      CODEREP *base = defstmt->Lhs()->Istr_base();
      CODEREP *num_bytes = (CODEREP *)defstmt->Lhs()->Mstore_size();
    }
    break;
  default:
    break;
  }
  // default to alias behavior, since we failed to prove otherwise
  return TRUE;
}


// =============================================================================
//
//  VSA::Propagate_vardef - return the root of the var
//
// =============================================================================
CODEREP *
VSA::Propagate_vardef(CODEREP *x, SCC *scc, hash_set<IDTYPE> &visited_bb)
{
  AUX_STAB_ENTRY *sym;
  ST             *st;

  Is_True(x != NULL, ("VSA::Propagate_vardef: null var"));
  Is_True(scc != NULL, ("VSA::Propagate_vardef: null scc"));
  
  // skip these two VSYMs
  if (x->Aux_id() == Opt_stab()->Default_vsym() ||
      x->Aux_id() == Opt_stab()->Return_vsym()) 
    return x;
  
  sym = Opt_stab()->Aux_stab_entry(x->Aux_id());
  st = sym->St();

  if ( x->Is_flag_set(CF_IS_ZERO_VERSION) ) {
    x->Set_value_def();  // zero version means alias gave up, assume the worst
    return x;
  }

  // is this variable a constant initialized scalar?
  CODEREP *retv = Udt_const_init_scalar(x, x->Aux_id());
  if (retv) {
    retv->Set_value_def();
    return retv;
  }

  if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI)) ) {
    // test for entry_chi -> VSA UIV
    if (x->Def_at_entry() && st !=NULL /* vsym does not have st entry e.g. return vsym */) {
      switch (ST_sclass(st)) {
      case SCLASS_AUTO:
        x->Set_value_not_def();
        break;
      case SCLASS_FORMAL:
      case SCLASS_FORMAL_REF:
      case SCLASS_REG:
        x->Set_value_maydef();
        break;
      case SCLASS_FSTATIC:
        if (! x->Value_checked()) {
          x->Set_value_def();
          x->Set_value_invalid_addr();// the value is initialized to ZERO by default
        }
        break;
      case SCLASS_UGLOBAL:
      case SCLASS_DGLOBAL:
        if (! x->Value_checked()) {
          x->Set_value_maydef(); 
        }
        break;
      default: // examine common/com/symtab_defs.h for other SCLASS
        x->Set_value_maydef();
        break;
      }
      return x;
    } // end of entry_chi
    STMTREP *defstmt = x->Defstmt();
    if (!IPSA_insession() && ! Load_from_subset_memory_area(defstmt, x)) {
      // we also check other cases that is not defined by call
      x->Set_value_not_def();
    } else {
      // if not entry_chi, conservatively assumes they are defined.
      x->Set_value_def();
    }
    return x;
  }

  if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI)) ) {
    if (x->Value_checked() && (x->Value_def() || x->Value_maydef()))
      return x;

    // check all its operands since it has not been checked
    PHI_NODE *phi = x->Defphi();
    if (visited_bb.find(phi->Bb()->Id()) != visited_bb.end()) {
      x->Set_value_checked();
      return x;
    } else
      visited_bb.insert(phi->Bb()->Id());

    PHI_OPND_ITER phi_opnd_iter(phi);
    CODEREP *opnd;
    BOOL alldef = TRUE;
    BOOL has_malloc = x->Value_malloc();
    INT  invalid_addr_cnt = 0;
    INT  maydangling_ref_cnt = 0;
    INT  unknown_value_cnt = 0;
    FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
      CODEREP *cr = Propagate_vardef(opnd, scc, visited_bb);
      if (!scc->Defer(opnd)) {
        if (! cr->Value_def()) {
          alldef = FALSE;
          x->Set_value_maydef();
        }
        if (cr->Value_invalid_addr())
          invalid_addr_cnt++;
        else if (cr->Value_maydangling())
          maydangling_ref_cnt++;
        else
          unknown_value_cnt++;
        if (cr->Value_malloc() && has_malloc == FALSE)
          has_malloc = TRUE;
      }
    }
    if (alldef)  // todo handle this reset
      x->Set_value_def();

    if (has_malloc)
      x->Set_value_malloc();
 
    if (invalid_addr_cnt || maydangling_ref_cnt) {
      if ((invalid_addr_cnt+maydangling_ref_cnt+unknown_value_cnt) == invalid_addr_cnt) {
        x->Set_value_invalid_addr();
      } else {
        x->Set_value_maydangling();
      }
    }

    return x;
  }

  STMTREP *stmt = x->Defstmt();
  Is_True(stmt != NULL || x->Is_var_volatile(), ("VSA::Udt_var: null stmt"));

  // is this variable has been assigned with constant zero
  if (stmt != NULL &&
      ((stmt->Rhs()->Kind() == CK_CONST &&
        stmt->Rhs()->Const_val() == 0) ||
       (stmt->Rhs()->Has_const_fval() &&
        stmt->Rhs()->Const_fval() == 0.0)))
    x->Set_value_invalid_addr();
  if (! x->Value_checked()) {
    x->Set_value_def();
  }

  return x;
}


// =============================================================================
//
//  VSA::Append_cd_linenum appends the last statement of cd bbs that lead to curbb
//
// =============================================================================
void
VSA::Append_cd_linenum(CODEREP *x, BB_NODE *curbb, SRCPOS_HANDLE *srcpos_h, hash_set<IDTYPE> &visited_bb)
{
  // when a reference of UIV in a deeply nested loop, the additional linenum
  // helps convey the message

  if (curbb->Kind() == BB_ENTRY) {
    AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(x->Aux_id());
    ST   *st = sym->St();
    srcpos_h->Append_data(st, curbb, Dna(), PATHINFO_ST_DECLARE);
    return;
  }

  if (visited_bb.find(curbb->Id()) != visited_bb.end())
    return;
  visited_bb.insert(curbb->Id());

  BB_NODE *rcfgbb;
  BB_NODE_SET_ITER rcfg_iter;
  FOR_ALL_ELEM( rcfgbb, rcfg_iter, Init( curbb->Rcfg_dom_frontier() ) ) {
    srcpos_h->Append_data(rcfgbb, Dna(), PATHINFO_CD_BB);
    Append_cd_linenum(x, rcfgbb, srcpos_h, visited_bb);
  }
}

// =============================================================================
//
//  VSA::Classify_uiv_error for variables
//
// =============================================================================
void
VSA::Classify_uiv_error(CODEREP *x, BB_NODE *curbb,
                        CALL_STACK& cs, SRCPOS_HANDLE *srcpos_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited_bb)
{

  Is_True(x != NULL, ("VSA::Classify_uiv_error: null var"));
  // skip these two VSYMs
  if (x->Aux_id() == Opt_stab()->Default_vsym() ||
      x->Aux_id() == Opt_stab()->Return_vsym()) 
    return;
  
  if ( x->Is_flag_set(CF_IS_ZERO_VERSION) ) {
    return;
  }

  AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(x->Aux_id());
  ST   *st = sym->St();

  if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI)) ) {
    STMTREP *defstmt = x->Defstmt();
    // test for entry_chi -> VSA UIV
    if (x->Def_at_entry() && st !=NULL /* vsym does not have st entry e.g. return vsym */) {
      Is_True(defstmt->Bb()->Kind() == BB_ENTRY,
              ("def bb is not entry"));
      if (defstmt->Bb()->Labnam() > 0) {
        // eh handler, continue traversing from call stmt
        WN* entrywn = defstmt->Bb()->Entrywn();
        Is_True(entrywn != NULL &&
                WN_operator(entrywn) == OPR_LABEL,
                ("invalid entry wn"));
        Is_True(WN_Label_Is_Handler_Begin(entrywn),
                ("entry wn is not handler"));
        Check_eh_paths_for_vul(x, curbb, defstmt, cs, srcpos_h, kind, visited_bb);
        return;
      }
      SRCPOS st_pos = ST_Srcpos(*st);
      if(st_pos == 0) {
        // set the st_pos to function entry
        st_pos = Cfg()->Entry_spos();
      }

      srcpos_h->Append_data(st, defstmt->Bb(), Dna(), PATHINFO_ST_DECLARE);
          
      switch (ST_sclass(st)) {
      case SCLASS_AUTO:

        // ignore _temp_.Mreturn., which is generated by compiler for struct return value
        if (st != NULL && strncmp(ST_name(st), "_temp_.Mreturn.", 15) == 0)
          break;
        // ignore .init, which is generated by compiler for struct initialization
        if (st != NULL && strncmp(ST_name(st), ".init", 5) == 0)
          break;

        // Append_cd_linenum(x, curbb, srcpos_h);
#if 0
        Report_uiv_error(x, srcpos_h);
#else
        // treat the declaration of the st as the "key" srcpos for this issue
        srcpos_h->Set_key_srcpos(Dna(), NULL, st_pos,
                                 srcpos_h->Find_cr_stname(x, defstmt, Dna()));
        srcpos_h->Set_msgid("UIV.1");
        Report_vsa_error(x, x->Aux_id(), UIV, IC_DEFINITELY, srcpos_h);
        srcpos_h->Remove_last_key_srcpos();
#endif
        break;
      case SCLASS_FORMAL:
      case SCLASS_FORMAL_REF:
      case SCLASS_REG:              // PREG in entry_chi is likely an input parameter
        if (IPSA_insession()) {
          // call the following function for all its caller
          Check_callers_argument_for_uiv(x, cs, srcpos_h, kind);
        }
        break;
      case SCLASS_FSTATIC:
      case SCLASS_UGLOBAL:
      case SCLASS_DGLOBAL:
        break;
      default: // examine common/com/symtab_defs.h for other SCLASS
        break;
      }
      return;
    } // end of entry_chi

    if (!IPSA_insession() && ! Load_from_subset_memory_area(defstmt, x)) {
      // we also check other cases that is not defined by call
#if 0    
      Report_uiv_error(x, srcpos_h);
#else
      // treat the def stmt as the "key" srcpos for this issue
      srcpos_h->Set_key_srcpos(Dna(), defstmt, x);
      srcpos_h->Set_msgid("UIV.1");
      Report_vsa_error(x, x->Aux_id(), UIV, IC_DEFINITELY, srcpos_h);
      srcpos_h->Remove_last_key_srcpos();
#endif
    } else if (!IPSA_insession()) {
      // if not entry_chi, conservatively assumes they are defined.
    } else {
      // in ipsa mode,
      if (OPERATOR_is_call(defstmt->Opr())) {
        // defined by call, check the rna info if the variable is modified in callee
        // if it is passed to callee with LDA operation.  Set value def if modified,
        // otherwise, inherit the state of the chi operand by calling Vsa_var.
        RNA_NODE *rna = Sr_2_rna(defstmt);
        if (rna != NULL) {
          IDTYPE arg_seq = rna->Get_arg_with_lda(x->Aux_id());
          if (arg_seq != INVALID_VAR_IDX) {
            CHI_NODE *defchi = x->Defchi();
            // chi opnd already defined, no UIV
            if (defchi->OPND()->Value_def())
              return;
            // no ISTORE in callee, continue with chi opnd
            if (!rna->Is_set_arg_flag(arg_seq,REF_ISTORE)) {
              if (rna->Callee_cnt() == 0) {
                // extern call with LDA, set result to 'M'
                srcpos_h->Set_flag(SRCPOS_FLAG_MAYBE);
              }
              srcpos_h->Append_data(defstmt, Dna(), PATHINFO_CALL_CHI);
              Classify_uiv_error(defchi->OPND(), curbb, cs, srcpos_h, kind, visited_bb);
              return;
            }
            // check istores in callee for each return statement
            cs.push(rna);
            INT size = cs.size();
            Check_callee_istore_for_uiv(curbb, arg_seq, defchi->OPND(), cs, srcpos_h, kind, visited_bb);
            Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
            cs.pop();
            return;
          }
        }
        else if (defstmt->Opr() == OPR_INTRINSIC_CALL) {
          Check_intrn_call_for_vul(x, curbb, defstmt, cs, srcpos_h, kind, visited_bb);
        }
      }
    }
    return;
  }
  else if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI)) ) {
    PHI_NODE *phi = x->Defphi();
    if (visited_bb.find(phi->Bb()->Id()) != visited_bb.end())
      return;
    visited_bb.insert(phi->Bb()->Id());

    if ( x->Value_checked() && x->Value_maydef()) {

      // check all its operands since it has not been checked
      // we need to fork the srcpos_tree, if there is more than on operands is
      // maydef or not_def
      PHI_OPND_ITER phi_opnd_iter(phi);
      CODEREP      *opnd;
      INT           not_def_cnt = 0;
      INT           which_pred = 0;
      vector<BOOL>  check_opnd(phi->Size(), FALSE);

      FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
        if (!opnd->Value_def() && 
            (srcpos_h->Path()->Is_path_possible(phi, which_pred) != PATH_NOT_REACHABLE)) {
          check_opnd[which_pred] = TRUE;
          not_def_cnt++;
        }
        ++which_pred;
      }

      if (not_def_cnt > 0) {
        // fork the srcpos_h by creating children
        srcpos_h->Append_data(phi->Bb(), Dna(), PATHINFO_PHI);
        srcpos_h->Add_children(not_def_cnt);
        SRCPOS_TREENODE *cur_treenode = srcpos_h->Cur_node();
        srcpos_h->Path()->Push_mark(phi);
        IDTYPE           i = 0;
        BB_NODE         *bb_pred;
        BB_LIST_ITER     bb_iter;
        INT32            which_pred = 0;

        FOR_ALL_ELEM(bb_pred, bb_iter, Init(phi->Bb()->Pred())) {
          opnd = phi->OPND(which_pred);
          if (check_opnd[which_pred] == TRUE) {
            srcpos_h->Set_cur_node(cur_treenode, i);
            // TODO: update with pred's linenum
            srcpos_h->Append_data(bb_pred, Dna(), PATHINFO_BRANCH);
            srcpos_h->Path()->Add_bb(bb_pred);
            Classify_uiv_error(opnd, bb_pred, cs, srcpos_h, kind, visited_bb);
            ++i;
          }
          ++which_pred;
          srcpos_h->Path()->Pop_mark(phi, FALSE);
        }
        srcpos_h->Path()->Pop_mark(phi, TRUE);
      }
    } // x->Value_maydef() is TRUE
    else if(x->Value_def() && VSA_Rvsa) {
      Compose_rvsa_path(srcpos_h, x);
      srcpos_h->Set_to_root();
      Report_rvsa_info(x, x->Aux_id(), UIV, IC_DEFINITELY, srcpos_h);
    }
    return;
  }
    

  // after mainopt lowering, input parameter is no longer in entry_chi
  if (st && IPSA_insession()) {
    IDTYPE maybe_param = Dna()->Is_param(x);
    if (maybe_param != INVALID_VAR_IDX) {
      // TODO: for Iload/Istore, we need additional check before checking callers_argument
      Check_callers_argument_for_uiv(x, cs, srcpos_h, kind);
    }
    else if(VSA_Rvsa) {  // for RVSA
    Compose_rvsa_path(srcpos_h, x);
    srcpos_h->Set_to_root();
    Report_rvsa_info(x, x->Aux_id(), UIV, IC_DEFINITELY, srcpos_h);
    }
  }
  return;
}

BOOL
VSA::Report_rvsa_info(CODEREP* x, AUX_ID auxid, UINT32 anat, ISSUE_CERTAINTY ic, SRCPOS_HANDLE *srcpos_h) const
{
  SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
  if(cur_node->Get_child_cnt() > 0) {
    for(int i=0; i < cur_node->Get_child_cnt(); i++) {
      srcpos_h->Set_cur_node(cur_node, i);
      Report_rvsa_info(x, auxid, anat, ic, srcpos_h);
    }
  }
  else
    Report_vsa_error(x, auxid, anat, ic, srcpos_h);
  return TRUE;
}

// =============================================================================
//
//  VSA::Report_aob_if_no_size: report AOB if doesn't find the buffer size
//
// =============================================================================
void
VSA::Report_aob_if_no_size(CODEREP *x, STMTREP *stmt)
{
  Is_True_Ret(x && stmt, ("invalid cr or stmt"));
  CODEREP *base = Find_base_pointer_load(x);
  if (!base || base->Kind() != CK_VAR)
    return;
  AUX_STAB_ENTRY* aux = _opt_stab->Aux_stab_entry(base->Aux_id());
  Is_True_Ret(aux, ("not find the symbol"));
  BOOL report_parm = (VSA_Warn_On_Param && Dna()->Is_param(base));
  BOOL report_global = (VSA_Warn_On_Global && aux->Is_global());
  BOOL report_retval = (VSA_Warn_On_Retval && aux->Is_return_preg());
  if (report_parm || report_global || report_retval) {
    ST* st = aux->St();
    Is_True(st != NULL, ("null st"));
    SRCPOS_HANDLE srcpos_h(base, stmt, Dna(), Loc_pool(), this);
    STMTREP* def = base->Defstmt();
    if (report_retval) {
      if (def)
        srcpos_h.Append_data(def, Dna(), PATHINFO_DNA_CALLRETURN);
    }
    else {
      ST *st = aux->St();
      if (st && ST_Srcpos(*st))
        srcpos_h.Append_data(st, def ? def->Bb() : Cfg()->Entry_bb(),
                             Dna(), PATHINFO_ST_DECLARE);
    }
    srcpos_h.Set_msgid("AOB.1");
    Report_vsa_error(base, (char*)NULL, AOB, IC_MAYBE, &srcpos_h);
  }
}

// =============================================================================
//
//  VSA::Classify_asm_uiv_error for variables
//
// =============================================================================
void
VSA::Classify_asm_uiv_error(STMTREP *stmt, BB_NODE *curbb)
{
  Is_True(stmt->Opr() == OPR_ASM_STMT, ("not asm stmt"));
  CODEREP* rhs = stmt->Rhs();
  Is_True(rhs != NULL && rhs->Opr() == OPR_ASM_STMT, ("not stmt op"));

  INT i;
  for (i = 0; i < rhs->Kid_count(); ++i) {
    CODEREP* opnd = rhs->Opnd(i);
    Is_True(opnd->Opr() == OPR_ASM_INPUT, ("not asm input"));
    ST_IDX st = opnd->Asm_constraint();
    Is_True(st != ST_IDX_ZERO && ST_name(st) != NULL, ("no constraint string"));
    if (st == ST_IDX_ZERO || ST_name(st) == NULL)
      continue;

    opnd = opnd->Opnd(0);
    const char* cstr = ST_name(st);
    if (*cstr != '=' && opnd->Kind() == CK_LDA) {
      // here is a read on the LDA opnd
      AUX_ID aux = opnd->Lda_aux_id();
      MU_NODE *mu;
      MU_LIST_ITER mu_iter;
      FOR_ALL_NODE(mu, mu_iter, Init(stmt->Mu_list())) {
        if ( !mu->Is_Valid() || mu->Aux_id() != aux)
          continue;
        Classify_uiv_error(mu->OPND(), curbb, stmt, FOR_UIV);
      }
    }
    else {
      Classify_uiv_error(opnd, curbb, stmt, FOR_UIV);
    }
  }
}

// =============================================================================
//  VSA::Mark_var_escaped(CODEREP* x, hash_set<IDTYPE> &visited)
// =============================================================================
void
VSA::Mark_var_escaped(CODEREP* x, hash_set<IDTYPE> &visited)
{
  return;  // disable escaped logic
  Is_True(x->Kind() == CK_VAR, ("not var"));
  if (x->Value_escaped() ||
      TY_kind(x->object_ty()) != KIND_POINTER) // only for pointer
    return;
  Is_Trace(Tracing(),
           (TFile, "set sym%dv%d cr%d escaped\n",
                   x->Aux_id(), x->Version(), x->Coderep_id()));
  x->Set_value_escaped();
  if (x->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE* phi = x->Defphi();
    Is_True(phi != NULL, ("null phi node"));
    if (visited.find(phi->Bb()->Id()) == visited.end()) {
      visited.insert(phi->Bb()->Id());
      for (INT i = 0; i < phi->Size(); ++ i)
        Mark_var_escaped(phi->OPND(i), visited);
    }
  }
}

// =============================================================================
//
//  VSA::Vsa_var - 
//
// =============================================================================
CODEREP *
VSA::Vsa_var(CODEREP *x)
{
  return x;
}

// =============================================================================
//  VSA::Udt_ivar - return the root of its value
// =============================================================================
CODEREP *
VSA::Udt_ivar(CODEREP *x)
{

  STMTREP *stmt = x->Ivar_defstmt();
  if (stmt == NULL) return NULL;

  CODEREP *expr = stmt->Rhs();

  return expr;
}


// =============================================================================
//  Vsa::Vsa_ivar - return the root of its value
// =============================================================================
CODEREP *
VSA::Vsa_ivar(CODEREP *x)
{

  STMTREP *stmt = x->Ivar_defstmt();
  if (stmt == NULL) return NULL;

  CODEREP *expr = stmt->Rhs();

  return expr;
}


// =============================================================================
// Process_identity_assignment - cr is a CK_VAR node being referenced; check if
// it is defined at an identity assignment; if so, return the rhs version of
// the identity assignment; else return NULL.  
// =============================================================================
//
//  DONT CHANGE THIS FUNCTION ---
//
//    Any extra conditions must be in Is_identity_assignment_removeable()
//
static CODEREP *
Process_identity_assignment(CODEREP *cr)
{
  STMTREP *dstmt;
  if (cr->Is_flag_set(CF_DEF_BY_PHI)) 
    return NULL;
  if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
    dstmt = cr->Defstmt();
    if (dstmt != NULL && 
        dstmt->Is_identity_assignment_removable() &&
        VSA_Enable_DCE)
      return cr->Defchi()->OPND();
  } else {
    if ((dstmt = cr->Defstmt()) != NULL && 
        dstmt->Is_identity_assignment_removable() &&
        VSA_Enable_DCE)
      return dstmt->Rhs();
  }
  return NULL;  
}

#if 0
// To be removed later
// =============================================================================
//
// Npd_expr - helper function to handle pointer arithmatic beneath ILOAD or ISTORE
//
// =============================================================================
BOOL
VSA::Npd_ptr_arith(CODEREP *expr, STMTREP *stmt, BB_NODE *bb, SRCPOS_HANDLE *srcpos_h)
{
  Is_True(expr, ("expr must not be null"));
  
  if ((expr->Kind() == CK_OP) &&
      ((expr->Opr() == OPR_ADD) || (expr->Opr() == OPR_SUB))) {
    if ((expr->Opnd(0)->Kind() == CK_VAR) &&
        (TY_kind(expr->Opnd(0)->Lod_ty()) == KIND_POINTER)) {
      // we assume pointer arithmatic will have possible bad derefercnec
      Is_Trace(Tracing(), (TFile, "Check NPD of Iload/Istore-ptr:: "));
      Is_Trace_cmd(Tracing(), expr->Print(2, TFile));
      Is_Trace(Tracing(), (TFile, "\n"));
      AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(expr->Opnd(0)->Aux_id());
      ST   *st = sym->St();
      if (st && (ST_sclass(st) == SCLASS_FORMAL)) {
        // we ignore formals for now until RNA/DNA and -xa is on
        return FALSE;
      }
      

      srcpos_h->Append_stpath(stmt, expr->Opnd(0), Dna(), TRUE);
      hash_set<IDTYPE> visited_bb;
      Classify_npd_error(expr->Opnd(0), bb, srcpos_h, VAL_May_OOR, FALSE, visited_bb);
      return TRUE;
    }
  }
  return FALSE;
}
#endif

// =============================================================================
//
//  VSA::Udt_cr - scan the expression - cr
//
// =============================================================================
CODEREP *
VSA::Udt_cr(CODEREP *x, BB_NODE *curbb, STMTREP *stmt)
{
  CODEREP *expr;

  switch (x->Kind()) {
  case CK_LDA: 
  case CK_CONST: 
  case CK_RCONST:
    return NULL;
    
  case CK_VAR: 
    {
      return Udt_var(x, curbb, INVALID_SRCPOS);
    }
  case CK_IVAR:
    {
      expr = Udt_ivar(x);
      if (expr)
        return expr;
      expr = Udt_cr(x->Ilod_base(), curbb, stmt);

      if (expr == NULL)
        expr = x->Ilod_base();

      if (expr->Kind() == CK_LDA && x->Offset() == 0) {
        CODEREP *retv = Udt_const_init_scalar(x, expr->Lda_aux_id());
        // TODO: should return here???
        if (retv)
          return retv;
      }

      BOOL skip_npd_check = FALSE;
      if(stmt->Opr() == OPR_ICALL && PU_java_lang(Get_Current_PU())) {
        // java icall last parm is for vptr, skip it
        // for null object, the check should be handled by CHECK_CAST
        if(x == stmt->Rhs()->Opnd(stmt->Rhs()->Kid_count()-1) &&
          expr->Kind() == CK_IVAR &&
          expr->I_field_id() == 1) {
          skip_npd_check = TRUE;
        }
      }

      // skip npd check for fixed safe address
      if (expr->Kind() == CK_CONST &&
          x->Opr() != OPR_PARM &&
          Is_const_addr_safe(x->Offset() + expr->Const_val()))
        skip_npd_check = TRUE;

      // parm could be beneath an ILOAD, screen out first
      if (VSA_Npd() && !PU_java_lang(Get_Current_PU()) && (x->Opr() != OPR_PARM) && !skip_npd_check) {
        // TODO: some prop work below is still needed
        SRCPOS_HANDLE srcpos_h(expr, stmt, _cu->Dna(), Loc_pool(), this);
        if (IPSA_insession())
          Ipsa()->Begin_trav_counter();
        hash_set<IDTYPE> visited_bb;
        CALL_STACK cs;
        Classify_vul_error(expr, curbb, stmt,
                           cs, &srcpos_h, (ILODSTORBASE)(FOR_ILOD_BASE | FOR_NPD), visited_bb);
        Is_True(cs.empty(), ("call stack is not empty"));
        if (IPSA_insession())
          Ipsa()->End_trav_counter();
        break;
#if 0
// To be removed later
        VAL_RANGE_RESULT rr;
        BOOL is_vra = FALSE;
        if (srcpos_h == NULL) {
          srcpos_h = CXX_NEW(SRCPOS_HANDLE(x, stmt, Dna(), Loc_pool()), Loc_pool());
        }
        else {
          srcpos_h->Append_data(stmt, x, Dna(), PATHINFO_COPY);
        }
        if (Npd_ptr_arith(x, stmt, curbb, srcpos_h))
          break;
        
        CODEREP *ilod_base = (expr != NULL) ? expr : x->Ilod_base();
        if ((ilod_base->Kind() == CK_OP) && (ilod_base->Opr() == OPR_ARRAY)) {
          // the array express could be CK_OP or CK_VAR
          CODEREP *ex = expr;
          switch (expr->Opnd(0)->Kind()) {
          case CK_VAR:
            ex = expr->Opnd(0);
            break;
          case CK_OP:
            ex = expr->Opnd(0);
            if (ex->Opnd(0)->Kind() == CK_VAR) {
              ex = ex->Opnd(0);
              break;
            }
            
            // fall through
            // we don't handle for now till UIV result is cached in symtab
          default:
            Is_Trace(Tracing(), (TFile, "non VAR/OP inside ARRAY of NPD"));
            return x;

          }
          // check for array out of bound
          Is_Trace(Tracing(), (TFile, "Check NPD of Iload_array:: "));
          Is_Trace_cmd(Tracing(), expr->Print(2, TFile));
          Is_Trace(Tracing(), (TFile, "\n"));

          rr = Check_val_range(ilod_base, curbb);
          if (rr != VAL_INR) {
            hash_set<IDTYPE> visited_bb;
            Classify_npd_error(ex, curbb, srcpos_h, rr, FALSE, visited_bb);
          }
          return x;
        }
        
#if 0 // SC - in case we need to worry about structs that is not LDID/MMILOAD
        if ((ilod_base->Kind() == CK_VAR) && (TY_kind(x->Ilod_ty()))) {
          if (TY_kind(x->Ilod_ty()) == KIND_STRUCT) {
            printf("Struct here\n");
          }
        }
#endif
        
        if (ilod_base->Kind() == CK_LDA && x->Offset() == 0) {
          CODEREP *retv = Udt_const_init_scalar(x, ilod_base->Lda_aux_id());
          if (retv) {
            // VSA check the value and report error
            return retv;
          }
        }
        else if (ilod_base->Kind() == CK_CONST) {
          if (x->Ilod_base()->Const_val() == 0) {
            // should call Check_val_range
            // should update srcpos of ilod_base -SC
            // FmtAssert(FALSE, ("in ILOD BASE zero CONST"));
            ST_IDX st = IPSA_insession() ? Dna()->Get_node_st(stmt, ilod_base) : ST_IDX_ZERO;
            if (st != ST_IDX_ZERO) {
              Report_vsa_error(x, ST_name(st), NPD, EC_VSA_Nullptr_deref, srcpos_h);
            }
            else {
              Is_Trace(Tracing(),
                       (TFile, ("VSA-NPD: No symbol for NPD on constant.\n")));
              Report_vsa_error(x, (AUX_ID)0, NPD, EC_VSA_Npd_noname_deref, srcpos_h);
            }
            // Classify_npd_error_name(NULL, srcpos_h, VAL_OOR);
            return NULL;
          }
        }
        else {
          if (expr != NULL) {
            if (!Npd_ptr_arith(expr, stmt, curbb, srcpos_h)) {
              Is_Trace(Tracing(), (TFile, "Check NPD of Iload "));
              Is_Trace_cmd(Tracing(), expr->Print(2, TFile));
              Is_Trace_cmd(Tracing(), expr->Print(2, TFile));
              Is_Trace(Tracing(), (TFile, "\n"));
              if (expr->Kind() == CK_IVAR) {
                if ((expr->Ilod_base()->Kind() == CK_IVAR) ||
                    ((expr->Ilod_base()->Kind() == CK_OP) &&
                     (expr->Ilod_base()->Opr() == OPR_ARRAY))) {
                  // we assume array under iload can be covered in AOB
                  return expr;
                }
                else if (expr->Ilod_base()->Kind() == CK_VAR) {
                  rr = Check_var_zero(expr->Ilod_base(), curbb, is_vra);
                  if (rr != VAL_INR) {
                    // in ILOD_BASE VAR
                    FmtAssert(TRUE, ("in ILOD BASE VAR"));
                    hash_set<IDTYPE> visited_bb;
                    Classify_npd_error(expr->Ilod_base(), curbb, srcpos_h, rr, is_vra, visited_bb);
                    End_visit_counter();
                    return NULL;
                  }
                }

              }

              if ((expr->Kind() == CK_OP) && (expr->Opr() == OPR_BAND)) {
                // ignore for now
              }
              CODEREP *x = expr;
              if ((expr->Kind() == CK_IVAR) && (expr->Ilod_base()->Kind() == CK_VAR))
                x = expr->Ilod_base();
              else
                x = expr;
              rr = Check_expr_zero(x, curbb, is_vra);
              if (rr != VAL_INR) {
                // FmtAssert(TRUE, ("indirect ILOD BASE VAR"));
                hash_set<IDTYPE> visited_bb;
                Classify_npd_error(x, curbb, srcpos_h, rr, is_vra, visited_bb);
                End_visit_counter();
              }
            }
          }
          return NULL;
        }
#endif
      } // end NPD check
        
      CODEREP *expr2;
      if (x->Opr() == OPR_MLOAD)
        expr2 = Udt_cr(x->Mload_size(), curbb, stmt);
      else if (x->Opr() == OPR_ILOADX)
        expr2 = Udt_cr(x->Index(), curbb, stmt);
      else
        expr2 = NULL;

      //if (expr || expr2) {
      //  return expr;
      //}

      if (x->Istr_base() != NULL && x->Istr_base() != x->Ilod_base()) {
        // undo the vulnerability static analysisn of Istr_base()
        x->Istr_base()->DecUsecnt_rec();
        x->Ilod_base()->IncUsecnt();
        x->Set_istr_base(x->Ilod_base());
      }
      return NULL;
    }
  case CK_OP:
    // expr is intentionally dead
    for  (INT32 i = 0; i < x->Kid_count(); i++) {
      expr = Udt_cr(x->Opnd(i), curbb, stmt);
    }

    // check division-by-zero here
    if (VSA_Dbz &&
        (x->Opr() == OPR_DIV || x->Opr() == OPR_MOD ||
         x->Opr() == OPR_REM || x->Opr() == OPR_DIVREM)) {
      Is_True(x->Kid_count() == 2,
              ("illegal div expr"));
      // pick up the divisor from the CODEREP x
      expr = x->Opnd(1);

      SRCPOS_HANDLE sp_h(expr, stmt, _cu->Dna(), Loc_pool(), this, x);
      if (IPSA_insession())
        Ipsa()->Begin_trav_counter();

      hash_set<IDTYPE> visited_bb;
      CALL_STACK cs;
      Classify_vul_error(expr, curbb, stmt, cs, &sp_h, FOR_DIVISOR, visited_bb);
      Is_True(cs.empty(), ("call stack is not empty"));
      if (IPSA_insession())
        Ipsa()->End_trav_counter();
    }

    // x->Set_flag(CF_C_P_PROCESSED);
    return x;
  }
  return NULL;
}


// =============================================================================
//
//  VSA::Udt_var - return the root of the var
//
// =============================================================================
CODEREP *
VSA::Udt_var(CODEREP *x, BB_NODE *curbb, SRCPOS spos)
{
  AUX_STAB_ENTRY *sym;
  ST             *st;

  Is_True(x != NULL, ("VSA::Udt_var: null var"));
  // skip these two VSYMs
  if (x->Aux_id() == Opt_stab()->Default_vsym() ||
      x->Aux_id() == Opt_stab()->Return_vsym()) 
    return x;
  
  sym = Opt_stab()->Aux_stab_entry(x->Aux_id());
  st = sym->St();

  if ( x->Is_flag_set(CF_IS_ZERO_VERSION) ) {
    x->Set_value_def();  // zero version means alias gave up, assume the worst
    return x;
  }

  // is this variable a constant initialized scalar?
  CODEREP *retv = Udt_const_init_scalar(x, x->Aux_id());
  if (retv) {
    retv->Set_value_def();  return retv;
  }

  if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI)) ) {
    // test for entry_chi -> VSA UIV
    if (x->Def_at_entry() && st !=NULL /* vsym does not have st entry e.g. return vsym */) {
      switch (ST_sclass(st)) {
      case SCLASS_AUTO:
        x->Set_value_not_def();
        // Classify_uiv_error(x, curbb, spos);
        break;
      case SCLASS_FORMAL:
      case SCLASS_FORMAL_REF:
        x->Set_value_maydef();
#if 0
        if (IPSA_insession()) {
          // call the following function for all its caller
                 Check_callers_argument_for_uiv(x, curbb, spos);
        }
#endif
        break;
      case SCLASS_FSTATIC:
        if (! x->Value_checked()) {
          x->Set_value_def();
          x->Set_value_invalid_addr();// the value is initialized to ZERO by default
        }
        break;
      case SCLASS_UGLOBAL:
      case SCLASS_DGLOBAL:
        if (! x->Value_checked()) {
          x->Set_value_maydef(); 
        }
        break;
      default: // examine common/com/symtab_defs.h for other SCLASS
        x->Set_value_maydef();
        break;
      }
      return x;
    } // end of entry_chi

    STMTREP *defstmt = x->Defstmt();
    if (!IPSA_insession() && ! Load_from_subset_memory_area(defstmt, x)) {
      // we also check other cases that is not defined by call
      x->Set_value_not_def();
      // Classify_uiv_error(x, curbb, spos);
    } else if (!IPSA_insession()) {
      // if not entry_chi, conservatively assumes they are defined.
      x->Set_value_def();
    } else {
      // in ipsa mode,
      if (OPERATOR_is_call(defstmt->Opr())) {
        // defined by call, check the rna info if the variable is modified in callee
        // if it is passed to callee with LDA operation.  Set value def if modified,
        // otherwise, inherit the state of the chi operand by calling Vsa_var.
        RNA_NODE *rna = Sr_2_rna(defstmt);
        if (rna != NULL) {
          IDTYPE arg_seq = rna->Get_arg_with_lda(x->Aux_id());
          if (arg_seq != INVALID_VAR_IDX &&
              ! rna->Is_set_arg_flag(arg_seq,REF_ISTORE)) {
            CHI_NODE *defchi = x->Defchi();
            x = Udt_var(defchi->OPND(), curbb, spos);
          }
        }
      }
      else
        x->Set_value_def();
    }
    return x;
  }

  if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI)) ) {

    if ( x->Value_checked()) {
      if ( x->Value_maydef())
        // Classify_uiv_error(x, curbb, spos);
      return x;
    }

    // x's value could be pending from Udt_bb
    // if that is the case, we can find out
    // CODEREP *top_cr = Opt_stab()->Top_coderep(x->Aux_id());
    if (/*top_cr == x &&*/ spos == INVALID_SRCPOS) {
      // will forego the version in the recursion
      // x->Set_value_def();  udt_cr phase should not do this
      return x;
    }
    // check all its operands since it has not been checked
    PHI_NODE *phi = x->Defphi();
    PHI_OPND_ITER phi_opnd_iter(phi);
    CODEREP *opnd;
    BOOL alldef = TRUE;
    FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
      CODEREP *cr = Udt_var(opnd, curbb, INVALID_SRCPOS);
#if 0
      if (! cr->Value_def()) {
        alldef = FALSE;
        x->Set_value_maydef();
      }
#endif
    }
#if 0
    if (alldef) // todo, pending list 
      x->Set_value_def();
#endif
    return x;

  }

  Is_True(x->Defstmt() != NULL || x->Is_var_volatile(), ("VSA::Udt_var: null stmt"));
  // x->Set_value_def();  already done in Propagate_vardef
  return x;
}


// =============================================================================
//
//  VSA::Propagate_vardef - process the expression - x
//
// =============================================================================
CODEREP *
VSA::Propagate_vardef(CODEREP *x, BB_NODE *curbb, STMTREP *stmt, UINT32 *flags)
{
  CODEREP *expr;
  
  switch (x->Kind()) {
  case CK_LDA: 
  case CK_RCONST:
    return NULL;
  case CK_CONST:
    if (flags != NULL)
      *flags |= (ISVAR_VALUE_DEF | ISVAR_VALUE_INVALID_ADDR);
    return x;
  case CK_VAR: 
    {
      CODEREP *id_cr = Process_identity_assignment(x);
      if (id_cr) {
        if (id_cr->Is_flag_set(CF_IS_ZERO_VERSION)) {
          Htable()->Fix_zero_version(x->Defchi(), x->Defstmt());
          id_cr = x->Defchi()->OPND();
        }
        if (id_cr->Dsctyp() == MTYPE_UNKNOWN ||
            id_cr->Is_flag_set(CF_MADEUP_TYPE)) {
          id_cr->Set_dtyp(x->Dtyp());
          id_cr->Set_dsctyp(x->Dsctyp());
          id_cr->Set_lod_ty(x->Lod_ty());
          id_cr->Set_field_id(x->Field_id());
          if (x->Bit_field_valid()) {
            id_cr->Set_bit_field_valid();
          }
          id_cr->Set_sign_extension_flag();
          id_cr->Reset_flag(CF_MADEUP_TYPE);
        }

        expr = Propagate_vardef(id_cr, curbb, stmt, NULL);
        if (flags != NULL && expr != NULL)
          *flags |= expr->Value_flags();
      }
      else {
#if 1
        IDTYPE auxid = x->Aux_id();
        AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(auxid);
        if (sym->Is_return_preg() &&
            !x->Is_flag_set(CF_DEF_BY_PHI) &&
            !x->Is_var_volatile() &&
            x->Defstmt()->Opr() != OPR_OPT_CHI )
          x->Set_value_maydangling();
#endif
        SCC scc(x, this, Loc_pool());
        
        hash_set<IDTYPE> visited_bb;
        expr = Propagate_vardef(x, &scc, visited_bb);
        scc.Update();
        if (flags != NULL && expr != NULL)
          *flags |= expr->Value_flags();
      }
      Is_True(expr == NULL ||
              (expr->Kind() == CK_VAR &&
               !expr->Is_flag_set(CF_IS_ZERO_VERSION)), ("bad expr"));
      if (VSA_Prop_Vardef && expr != NULL && !expr->Is_var_volatile()) {
        CODEREP *top = Opt_stab()->Top_coderep(expr->Aux_id());
        if (top != NULL && top != expr &&
            top == Opt_stab()->Top_coderep(top->Aux_id())) {
          //Is_True(top->Kind() == CK_VAR && top->Aux_id() != expr->Aux_id(),
          //        ("wrong top coderep"));
          Is_True(!top->Is_flag_set(CF_DEF_BY_PHI),
                  ("wrong top flag"));
          Is_True(top->Is_flag_set(CF_DEF_BY_CHI) ||
                  (OPERATOR_is_scalar_store(top->Defstmt()->Opr()) &&
                   top->Defstmt()->Rhs() == expr),
                  ("wrong top defstmt"));
          expr = top;
        }
      }
      return expr;
    }
  case CK_IVAR:
    {
      MU_NODE *mnode = x->Ivar_mu_node();
      if (mnode) {
        // ILOAD's mu node could be caused by it's base is a LDA node
        CODEREP *m_cr = Process_identity_assignment(mnode->OPND());
        if (m_cr)
          mnode->Set_OPND(m_cr);
        {
          SCC scc(mnode->OPND(), this, Loc_pool());
          
          hash_set<IDTYPE> visited_bb;
          m_cr = Propagate_vardef(mnode->OPND(), &scc, visited_bb);
          if (VSA_Prop_Vardef && m_cr != NULL && m_cr != mnode->OPND()) {
            Is_Trace(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                     (TFile, "PROP VARDEF: replace opnd of mu(%d) on cr%d on sr%d line %d:\n",
                      m_cr->Aux_id(), mnode->OPND()->Coderep_id(),
                      stmt->Stmtrep_id(), Srcpos_To_Line(stmt->Linenum())));
            Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                         stmt->Print(TFile));
            Is_Trace(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                     (TFile, " with cr%d:\n", m_cr->Coderep_id()));
            Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                         m_cr->Print(2, TFile));
            mnode->Set_OPND(m_cr);
          }
          scc.Update();
        }
      }

      expr = Udt_ivar(x);
      if (expr &&
          expr->Kind() != CK_IVAR &&
          expr->Kind() != CK_OP) {
        // VSA this ivar has a defstmt, let's check if it is a constant
        // keep track of cases like *p=2
        // only propagate CONST, LDA and VAR so far
        return expr;
      }

      INT64 ofst = x->Offset();
      expr = Propagate_vardef(x->Ilod_base(), curbb, stmt, NULL);
      CODEREP *expr2;
      if (x->Opr() == OPR_MLOAD)
        expr2 = Propagate_vardef(x->Mload_size(), curbb, stmt, NULL);
      else if (x->Opr() == OPR_ILOADX)
        expr2 = Propagate_vardef(x->Index(), curbb, stmt, NULL);
      else
        expr2 = NULL;

      if (VSA_Prop_Vardef &&
          ((expr && expr != x->Ilod_base()) ||
           (expr2 && x->Opr() == OPR_MLOAD && expr2 != x->Mload_size()) ||
           (expr2 && x->Opr() == OPR_ILOADX && expr2 != x->Index()))) {
        CODEREP *cr = Alloc_stack_cr(x->Extra_ptrs_used());
        cr->Copy(*x);
        if (expr && expr != x->Ilod_base()) {
          cr->Set_offset(ofst);
          cr->Set_ilod_base(expr);
        }
        cr->Set_istr_base(NULL);
        cr->Set_usecnt(0);
        if (expr2) {
          if (x->Opr() == OPR_MLOAD && expr2 != x->Mload_size())
            cr->Set_mload_size(expr2);
          else if (x->Opr() == OPR_ILOADX && expr2 != x->Index())
            cr->Set_index(expr2);
        }
        cr->Set_ivar_occ(x->Ivar_occ());
        x->DecUsecnt();
        if (x->Opr() == OPR_MLOAD || x->Opr() == OPR_ILOADX) {
          expr = Htable()->Rehash(cr);
        }
        else {
          FOLD ftmp;
          expr = ftmp.Fold_Expr(cr);
          if (expr == NULL) {
            expr = Htable()->Rehash(cr);
            if (expr->Kind()==CK_OP)
              expr->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
          }
        }
        return expr;
      }

      if (x->Istr_base() != NULL && x->Istr_base() != x->Ilod_base()) {
        // undo the vulnerability static analysisn of Istr_base()
        x->Set_istr_base(x->Ilod_base());
      }
      return NULL;
    }
  case CK_OP:
    {
      CODEREP *new_opnd[x->Kid_count()];
      BOOL need_rehash = FALSE;
      const OPERATOR opr = x->Opr();

      for  (INT32 i = 0; i < x->Kid_count(); i++) {
        CODEREP *opnd = x->Opnd(i);
        expr = Propagate_vardef(opnd, curbb, stmt, flags);
        if (VSA_Prop_Vardef) {
          if (expr == NULL) {
            new_opnd[i] = opnd;
          }
          else {
            new_opnd[i] = expr;
            if (need_rehash == FALSE && expr != opnd)
              need_rehash = TRUE;
          }
        }
      }

      if (need_rehash) {
        CODEREP *new_x = Alloc_stack_cr(x->Extra_ptrs_used());
        new_x->Copy(*x);
        new_x->Set_usecnt(0);
        for (INT32 i = 0; i < x->Kid_count(); i++) {
          new_x->Set_opnd(i, new_opnd[i]);
        }
        FOLD ftmp;
        x = ftmp.Fold_Expr(new_x);
        if (x == NULL) {
          x = Htable()->Rehash(new_x);
          x->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
        }
      }
      return x;
    }
  }
  return NULL;
}

// =============================================================================
//
//  VSA::Classify_uiv_error - scan the expression - x, carry the state kind for
//       error reporting purpose
//
// =============================================================================
void
VSA::Classify_uiv_error(CODEREP *x, BB_NODE *curbb, STMTREP *stmt, ILODSTORBASE kind,  STPATH* stpath)
{
  // turn off uiv by default for java lang
  if (!(VSA_Uiv() && !PU_java_lang(Get_Current_PU())))
    return;

  switch (x->Kind()) {
  case CK_LDA: 
  case CK_CONST: 
  case CK_RCONST:
    return;
  case CK_VAR:
  {
      // ignore UIV for function call from stl clang front end for now
      if (OPERATOR_is_call(stmt->Opr()) && OPERATOR_has_sym(stmt->Opr()) &&
          Vsa_check_regex_sym_ignore(ST_name(stmt->St())))
        return;

      SRCPOS_HANDLE srcpos_h(x, stmt, Dna(), Loc_pool(), this);

      CODEREP *id_cr = Process_identity_assignment(x);

      if (id_cr) {
        if (id_cr->Is_flag_set(CF_IS_ZERO_VERSION)) {
          Htable()->Fix_zero_version(x->Defchi(), x->Defstmt());
          id_cr = x->Defchi()->OPND();
        }
        if (id_cr->Dsctyp() == MTYPE_UNKNOWN ||
            id_cr->Is_flag_set(CF_MADEUP_TYPE)) {
          id_cr->Set_dtyp(x->Dtyp());
          id_cr->Set_dsctyp(x->Dsctyp());
          id_cr->Set_lod_ty(x->Lod_ty());
          id_cr->Set_field_id(x->Field_id());
          if (x->Bit_field_valid()) {
            id_cr->Set_bit_field_valid();
          }
          id_cr->Set_sign_extension_flag();
          id_cr->Reset_flag(CF_MADEUP_TYPE);
        }

        Classify_uiv_error(id_cr, curbb, stmt, kind, stpath); // recurse
      }
      if (IPSA_insession())
        Ipsa()->Begin_trav_counter();
      hash_set<IDTYPE> visited_bb;
      CALL_STACK cs;
      Classify_uiv_error(x, curbb, cs, &srcpos_h, kind, visited_bb);
      Is_True(cs.empty(), ("call stack is not empty"));
      if (IPSA_insession())
        Ipsa()->End_trav_counter();
      break;
    }
  case CK_IVAR:
    {
      if (x->Opr() == OPR_PARM) {
        Classify_uiv_error(x->Ilod_base(), curbb, stmt, FOR_PARM_BASE, stpath);
        return;
      }
      MU_NODE *mnode = x->Ivar_mu_node();
      if (mnode) {
        CODEREP *cr = mnode->OPND();
        if (cr->Aux_id() != Opt_stab()->Default_vsym() &&
            cr->Aux_id() != Opt_stab()->Return_vsym()) {
        
          // ILOAD's mu node could be caused by it's base is a LDA node
          SRCPOS_HANDLE srcpos_h(cr, stmt, Dna(), Loc_pool());

          if (IPSA_insession())
            Ipsa()->Begin_trav_counter();
          hash_set<IDTYPE> visited_bb;
          CALL_STACK cs;
          Classify_uiv_error(cr, curbb, cs, &srcpos_h, FOR_UIV, visited_bb);
          Is_True(cs.empty(), ("call stack is not empty"));
          if (IPSA_insession())
            Ipsa()->End_trav_counter();
        }
      }

      Classify_uiv_error(x->Ilod_base(), curbb, stmt, FOR_ILOD_BASE, stpath);
      
      if (x->Opr() == OPR_MLOAD)
        Classify_uiv_error(x->Mload_size(), curbb, stmt, FOR_UIV, stpath);
      else if (x->Opr() == OPR_ILOADX)
        Classify_uiv_error(x->Index(), curbb, stmt, FOR_UIV, stpath);
      else
        ;
      // check refined vsym for additional UIV
      VSYM_OBJ_REP *vor = Cr_2_vor(x);
      if (vor && IPSA_insession()) {
        // WORK IN PROGRESS
        SRCPOS_HANDLE srcpos_h(x, stmt, Dna(), Loc_pool());

        if (IPSA_insession())
          Ipsa()->Begin_trav_counter();

        hash_set<IDTYPE> visited_bb;
        CALL_STACK cs;
        Classify_uiv_error(x, curbb, stmt, vor, cs, &srcpos_h, FOR_UIV, visited_bb);
        Is_True(cs.empty(), ("call stack is not empty"));
        if (IPSA_insession())
          Ipsa()->End_trav_counter();

      } // there is any refined vsym
      break;
    }
  case CK_OP:
    {
      for  (INT32 i = 0; i < x->Kid_count(); i++) {
        Classify_uiv_error(x->Opnd(i), curbb, stmt, kind, stpath);
      }
    }
    break;
  }
  return;
}

// =============================================================================
//  VSA::Vsa_cr - scan the expression - cr
// =============================================================================
CODEREP *
VSA::Vsa_cr(CODEREP *x, STMTREP *sr, MEM_POOL *def_bbs_pool, BOOL in_array)
{
  CODEREP *expr;
  switch (x->Kind()) {
  case CK_LDA:
    return Create_lda_rscobj(x, sr);
  case CK_CONST: 
  case CK_RCONST:
    return NULL;
  case CK_VAR: 
    return Vsa_var(x);
  case CK_IVAR:
    {
      if (x->Opr() == OPR_PARM)
        return Vsa_cr(x->Ilod_base(), sr, def_bbs_pool, in_array);

      MU_NODE *mnode = x->Ivar_mu_node();
      if (mnode) {
        // ILOAD's mu node could be caused by it's base is a LDA node
      }

      CODEREP *expr2;
      expr = Vsa_ivar(x);
      if (expr) {
        // VSA this ivar has a defstmt, let's check if it is a constant
        // keep track of cases like *p=2
        return expr;
      }

      expr = Vsa_cr(x->Ilod_base(), sr, def_bbs_pool, in_array);
      CODEREP *ilod_base = (expr != NULL) ? expr : x->Ilod_base();
      if (ilod_base->Kind() == CK_LDA){
        // x->Offset() == 0 or != 0, we should do something here
      }
      else {
        CODEREP* cr = Find_base_pointer_load(ilod_base);
        if (cr != NULL && cr->Kind() == CK_VAR &&
            !cr->Is_var_volatile() &&
            Cr_2_heap_obj(cr) == NULL) {
          // TODO: LDA/IVAR??
#if 0
          HEAP_OBJ *ho_w_matching_auxid = Find(cr->Aux_id());
          if (ho_w_matching_auxid == NULL) {
            new_hor = Allocate_heap_obj(curbb, def_bbs_pool);
            //new_hor->Set_attr(ROR_DEF_BY_ISTORE);
            //new_hor->Set_stmt_def(stmt, Dna());
            //new_hor->Set_srcpos_node(stmt, Dna(), PATHINFO_ALLOC);
            if (Find_hor_def(cr, new_hor)) {
              Enter_cr_heap_obj_map(cr, new_hor);
              Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Vsa_cr ILOAD: an unknown heap_obj_rep assigned to a known pointer\n"));
            }
          }
          else {
            new_hor = Allocate_heap_obj(ho_w_matching_auxid, NULL);
            ho_w_matching_auxid->Prepend_def_bbs(curbb, def_bbs_pool);
            Enter_cr_heap_obj_map(cr, new_hor);
            Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Vsa_cr ILOAD: an unknown heap_obj_rep assigned to a known pointer\n"));
          }
#endif
          HEAP_OBJ_REP *new_hor = Find_or_create_hor(cr, sr->Bb(), def_bbs_pool);
          if (new_hor != NULL) {
            Enter_cr_heap_obj_map(cr, new_hor);
            Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Vsa_cr ILOAD: an unknown heap_obj_rep assigned to a known pointer\n"));
          }
        }
      }

      if (x->Opr() == OPR_MLOAD)
        expr2 = Vsa_cr(x->Mload_size(), sr, def_bbs_pool, in_array);
      else if (x->Opr() == OPR_ILOADX)
        expr2 = Vsa_cr(x->Index(), sr, def_bbs_pool, in_array);
      else
        expr2 = NULL;

      //if (expr || expr2) {
      //  return expr;
      //}
      if (x->Istr_base() != NULL && x->Istr_base() != x->Ilod_base()) {
        // undo the vulnerability static analysisn of Istr_base()
      }
      // OPR_PARM, OPR_MLOAD, OPR_MMILOAD will reach here
      // Is_True(FALSE, ("VSA::Vsa_cr: unknown state"));
      return NULL;
    }
  case CK_OP:

    const OPERATOR opr = x->Opr();

    if (opr == OPR_ALLOCA) {
      // treat it the same way as malloc, except, use RSC_KIND_LDA
      Create_alloca_rscobj(x, sr);
    } else if (opr != OPR_ADD && opr != OPR_SUB && opr != OPR_NEG && opr != OPR_MPY) {
      for  (INT32 i = 0; i < x->Kid_count(); i++) {
        // can do this in this loop because this is the only place
        // where opr can be array
        if ( opr == OPR_ARRAY ) {
          // only concerned about the index expressions, not the base
          if ( i > 0 )
            in_array = TRUE;
        }
        expr = Vsa_cr(x->Opnd(i), sr, def_bbs_pool, in_array);
      }
    } else {
      for  (INT32 i = 0; i < x->Kid_count(); i++) {
        expr = Vsa_cr(x->Opnd(i), sr, def_bbs_pool, in_array);
      }
    }

    // x->Set_flag(CF_C_P_PROCESSED);
    return x;
  }
  return NULL;
}

// =============================================================================
//
// VSA::Callee_returns_new_heap_memory, a wrapper in VSA class such that the
//      caller does not need to pass in Ipsa(), minimal interface change from
//      the STMTREP::Callee_returns_new_heap_memory
//
// =============================================================================
HEAPSTATE
VSA::Callee_returns_new_heap_memory(STMTREP *call, BOOL checkhor) const
{
  if (call->Opr() != OPR_CALL && call->Opr() != OPR_INTRINSIC_CALL) return HS_NONE;
  if (call->Callee_returns_new_heap_memory()) {
    return HS_GOOD;
  }
  else if (IPSA_insession()) {
    return Ipsa()->Callee_allocate_heap_memory(Dna(), call, checkhor);
  }
  return HS_NONE;
}

// =============================================================================
// VSA::Vsa_malloc_size
// return size for memory malloc functions
// =============================================================================
CODEREP*
VSA::Vsa_malloc_size(STMTREP* call, CODEREP *ret_cr, COMP_UNIT *cu, HEAPSIZE *hsz )
{
  if (call->St() != NULL && strcmp(ST_name(call->St()), "calloc") == 0) {
    CODEREP* nmemb = call->Rhs()->Opnd(0)->Ilod_base();
    CODEREP* esize = call->Rhs()->Opnd(1)->Ilod_base();
    return Htable()->Add_bin_node_and_fold(OPCODE_make_op(OPR_MPY, nmemb->Dtyp(), MTYPE_V),
                                           nmemb, esize, call->Bb());
  }
  if (call->Callee_returns_new_heap_memory())
    return Vsa_malloc_size_opnd(call, ret_cr, cu);
  else if (IPSA_insession()) {
    INT64 size = Ipsa()->Callee_returns_heapobj_size(Dna(), call);
    return (size > 0) ? Htable()->Add_const(MTYPE_I8, size) : NULL;
  } else {
    return NULL;
  }
}


// =============================================================================
//
//  STMTREP::Callee_returns_new_heap_memory - returns TRUE if it is a call to
//  malloc and its equivalence.
//
// =============================================================================
BOOL
STMTREP::Callee_returns_new_heap_memory(void) const
{
  if (this->Call_flags() & WN_CALL_DOES_MEM_ALLOC) // need additional heap filter
    return TRUE;

  if (Opr() == OPR_CALL && OPERATOR_has_sym(Opr())) {
    if (WOPT_Enable_Disambiguate_Heap_Obj && PU_has_attr_malloc (Pu_Table[ST_pu(St())]) ||
        Vsa_check_alloc_heap(ST_name(St())))
      return TRUE;
  }
  else if (Opr() == OPR_INTRINSIC_CALL) {
    INTRINSIC intrinsic = Rhs()->Intrinsic();
    if ((intrinsic == INTRN_U4I4MALLOC) ||
        (intrinsic == INTRN_U8I8MALLOC)) {
      return TRUE;
    }
    if ((PU_src_lang(Get_Current_PU()) & PU_JAVA_LANG)) {
      if (intrinsic == INTRN_ALLOC_OBJ)
        return TRUE;
      if (intrinsic == INTRN_NEW_PRIM_ARR || intrinsic == INTRN_NEW_OBJ_ARR)
        return TRUE;
      // TODO:
      // _Jv_NewMultiArray
    }
  }
  return FALSE;
}

// =============================================================================
//
//  VSA::Callee_frees_heap_memory - returns TRUE if this is call to free and
//       its equivalence.
//       Besides the STMTREP level check, we also check DNA analysis result
//       such that we handle user defined free function.
//       It returns the argument that is passed into free call. If return NULL,
//       the call statement does not free heap memory.
//
// =============================================================================
CODEREP *
VSA::Callee_frees_heap_memory(STMTREP *call, BOOL *maybe) const
{

  if (call->Opr() != OPR_CALL) return NULL;
  if (call->Callee_frees_heap_memory()) {
    if (maybe) *maybe = FALSE;
    //return call->Rhs()->Find_actual_arg();
    return Vsa_free_ptr_opnd(call);
  }

  if (IPSA_insession()) 
    return Ipsa()->Callee_frees_heap_memory(Dna(), call, maybe);
  else
    return NULL;

}

// =============================================================================
//
//  STMTREP::Callee_frees_heap_memory - returns TRUE if this is call to free and
//  its equivalence.
//
// =============================================================================
BOOL
STMTREP::Callee_frees_heap_memory(void) const
{
  if (Call_flags() & WN_CALL_DOES_MEM_FREE) // need additional heap filter
    return TRUE;

  if (Opr() == OPR_CALL && OPERATOR_has_sym(Opr())) {

    if (Vsa_check_free_heap(ST_name(St())))
      return TRUE;
    else
      return FALSE;

  } else if (Opr() == OPR_INTRINSIC_CALL) {
    INTRINSIC intrinsic = Rhs()->Intrinsic();
    if ((intrinsic == INTRN_U4FREE) ||
        (intrinsic == INTRN_U8FREE)) {
      return TRUE;
    }
  }

  return FALSE;
}


// =============================================================================
//
//  STMTREP::Callee_may_taint_parm - returns TRUE if it is a call that might
//  taint one of the arguments that passed down the call.
//  Return True if there is tainted argument.  The value for which_arg set to 0
//  if this function is vararg.
//
// =============================================================================
BOOL
VSA::Callee_may_taint_arg(STMTREP *call, CODEREP *cr, IDTYPE *which_arg) const
{
  if (call->Opr() == OPR_CALL && OPERATOR_has_sym(call->Opr())) {
    if (VSA_Enable_Taint_Model()) {
      TAINTMODEL tm = Vsa_check_func_arg_tainted(ST_name(call->St()), which_arg);
      switch (tm) {
      case TM_ARG_RET: return TRUE;
      case TM_NONE: return FALSE;
      case TM_ARG_REG:
      case TM_ARG_VAR:
      case TM_ARG_VSYM:
        // go into argument list to find the actual arg that is tainted
        *which_arg = 0;
        CODEREP *x = call->Rhs();
        Is_True(x->Opr() == OPR_CALL, ("Call is not OPR_CALL"));
        for (INT32 i=0; i < x->Kid_count(); i++) {
          CODEREP *parm_node = x->Opnd(i);
          CODEREP *parm = parm_node->Ilod_base();
          if ((TY_kind(parm_node->Ilod_ty()) == KIND_POINTER ||
               TY_kind(parm->object_ty()) == KIND_POINTER) &&
              Is_same_symbol(cr, parm)) {
            *which_arg = i;
            break;
          }
        }
        if (*which_arg != 0)
          return TRUE;
        else
          return FALSE;
      }
    }
  }
  return FALSE;
}

CODEREP *
VSA::Callee_create_new_thread(STMTREP *call)
{
  CODEREP *thread_fn = Is_new_thread_created(call);
  if (thread_fn) {
    return thread_fn;
  } else {
    // get from rbc model: with "pthread_create" tag
    // pthread_create(pthread_t, const pthread_attr_t *,
    //                void *(*start_routine)(void *), void *arg)
    // tagobj layout: ret/parm1/parm2/parm3/parm4
    // function pointer at parm3
    RNA_NODE *rna = Sr_2_rna(call);
    thread_fn = Ipsa()->Rbc()->Get_tag_obj(this, "pthread_create", rna, 3);
    return thread_fn;
  }
  return NULL;
}

CODEREP *
VSA::Callee_register_signal_handler(STMTREP *call)
{
  CODEREP *sig_handler = Is_register_signal_handler(call);
  if (sig_handler) {
    return sig_handler;
  } else {
    // get from rbc model: with "signal" tag
    // sighandler_t signal(int signum, sighandler_t handler)
    // tagobj layout: ret/parm1/parm2/parm3/parm4
    // function pointer at parm2
    RNA_NODE *rna = Sr_2_rna(call);
    sig_handler = Ipsa()->Rbc()->Get_tag_obj(this, "signal", rna, 2);
    return sig_handler;
  }
  return NULL;
}

// =============================================================================
//
//  VSA::Is_stmt_dominates - check if sr1 dominates sr2
//
// =============================================================================
BOOL
VSA::Is_stmt_dominates(STMTREP *sr1, STMTREP *sr2)
{
  if(sr1 == NULL || sr2 == NULL)
    return FALSE;
  if(sr1 == sr2)
    return TRUE;

  BB_NODE *bb1 = sr1->Bb();
  BB_NODE *bb2 = sr2->Bb();
  if(bb1 == bb2) {
    // same bb, check stmt order
    STMTREP *next = sr1->Next();
    while(next != NULL) {
      if(next == sr2) {
        return TRUE;
      }
      next = next->Next();
    }
    return FALSE;
  } else {
    return bb1->Dominates(bb2);
  }
  return FALSE;
}

// =============================================================================
//
//  VSA::Is_stmt_reaches - check if sr1 reaches sr2
//
// =============================================================================
BOOL
VSA::Is_stmt_reaches(STMTREP *sr1, STMTREP *sr2)
{
  CONTEXT_SWITCH ctx(Dna());
  if(sr1 == NULL || sr2 == NULL || sr1 == sr2) {
    return FALSE;
  }

  BB_NODE *bb1 = sr1->Bb();
  BB_NODE *bb2 = sr2->Bb();
  if(bb1 == bb2) {
    // same bb, check stmt order
    STMTREP *next = sr1->Next();
    while(next != NULL) {
      if(next == sr2) {
        return TRUE;
      }
      next = next->Next();
    }
    return FALSE;
  } else {
    vector<UINT32> visited;
    visited.resize(Cfg()->Total_bb_count());
    VRA *vra = Comp_unit()->Vra();
    if(vra == NULL) {
      return FALSE;
    }
    return vra->Cfg_has_path(bb1, bb2, visited, 1);
  }
  return FALSE;
}

// =============================================================================
//
//  VSA::Find_nth_arg - recursively find the nth LDID or CONST node of a function 
//  argument.
//
// =============================================================================
CODEREP *
CODEREP::Find_nth_arg(INT32 n)
{
  CODEREP *retv = NULL;
  switch (Opr()) {
  case OPR_CALL:
    if (Kid_count() > n) {
      CODEREP *opnd = Opnd(n);
      Is_True(opnd->Opr() == OPR_PARM, ("CODEREP::Find_nth_arg : Ill formed call"));
      if (opnd->Opr() == OPR_PARM) {
        retv = opnd->Ilod_base();
        if (retv->Kind() == CK_OP && (retv->Opr() == OPR_CVT || retv->Opr() == OPR_CVTL))
        retv = retv->Opnd(0);
      }
      else
        retv = NULL;
    }
    else {
      return NULL;
    }
    break;
  case OPR_ICALL:
  default:
    return NULL;
  }
  Is_True(retv->Kind() == CK_VAR ||
          retv->Kind() == CK_CONST ||
          retv->Kind() == CK_OP ||
          retv->Kind() == CK_LDA, 
          ("CODEREP::Find_nth_arg returns expression that is not VAR nor CONST"));
  return retv;
}


// =============================================================================
//
//  VSA::Find_actual_arg - recursively find the LDID node of a function argument
//  , currently only handles one argument, which is good enough for DBF error
//
// =============================================================================
CODEREP *
CODEREP::Find_actual_arg(void)
{
  switch (Kind()) {
  case CK_CONST:
    return this;
  case CK_VAR:
    return this;

  case CK_IVAR:
    if (Opr() == OPR_PARM) {
      CODEREP *retv = Ilod_base()->Find_actual_arg();
      return retv;
    }
    else
      return this;  // let caller to worry about

  case CK_OP:
    if (Kid_count() > 0)
      return Opnd(0)->Find_actual_arg();
    else
      return NULL;

  default:
    ;
  }
  return this;
}

// =============================================================================
// VSA::Find_preg_type Find the type for preg from parameter or return value
// =============================================================================
TY_IDX
VSA::Find_preg_type(CODEREP *cr, hash_set<IDTYPE>& visited) const
{
  Is_True(cr->Kind() == CK_VAR &&
          Opt_stab()->Aux_stab_entry(cr->Aux_id())->Is_preg(),
          ("not a preg"));
  if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = cr->Defphi();
    if (visited.find(phi->Bb()->Id()) == visited.end()) {
      visited.insert(phi->Bb()->Id());
      PHI_OPND_ITER phi_opnd_iter(phi);
      CODEREP *opnd;
      FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
        TY_IDX ret = Find_preg_type(opnd, visited);
        if (ret != TY_IDX_ZERO) {
          return ret;
        }
      }
    }
    return TY_IDX_ZERO;
  }
  else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP *def = cr->Defstmt();
    if (def->Opr() == OPR_OPT_CHI) {
      TY_IDX fty = ST_pu_type(Dna()->St());
      Is_True(TY_kind(fty) == KIND_FUNCTION, ("not function type"));
      TYLIST_IDX tylist = TY_parms(fty);
      IDTYPE parm = Dna()->Is_param(cr);
      TY_IDX parm_ty = TY_IDX_ZERO;
      for (INT i = 0; i <= parm; ++i) {
        parm_ty = TYLIST_ty(tylist);
        if (parm_ty == TY_IDX_ZERO)
          break;
        ++tylist;
      }
      return parm_ty;
    }
    else if (cr == Comp_unit()->Find_return_value(def)) {
      TY_IDX fty = def->Opr() == OPR_CALL ? ST_type(def->St())
                                          : def->Ty();
      Is_True(TY_kind(fty) == KIND_FUNCTION, ("not function type"));
      return TY_ret_type(fty);
    }
    else {
      // No idea
      return TY_IDX_ZERO;
    }
  }
  else {
    CODEREP *rhs = cr->Defstmt()->Rhs();
    if (rhs->Kind() == CK_VAR) {
      if (Opt_stab()->Aux_stab_entry(rhs->Aux_id())->Is_preg())
        return Find_preg_type(rhs, visited);
    }
    if (rhs->Kind() == CK_VAR || rhs->Kind() == CK_IVAR)
      return rhs->object_ty();
    if (rhs->Kind() == CK_LDA)
      return rhs->Lda_ty();
    // TODO: OP/etc
    return TY_IDX_ZERO;
  }
}

// =============================================================================
//
// VSA::Find_actual_arg given STMTREP as the parameter, customized for
//      Update_pending_ref function, where we handles only free calls
//
// =============================================================================
CODEREP*
VSA::Find_actual_arg(STMTREP *call_stmt) const
{
  CODEREP* arg;
  if (IPSA_insession()) {
    if (call_stmt->Opr() == OPR_CALL && OPERATOR_has_sym(call_stmt->Opr())) {
      if (Vsa_check_free_heap(ST_name(call_stmt->St()))) {
        //return call_stmt->Rhs()->Find_actual_arg();
        arg = Vsa_free_ptr_opnd(call_stmt);
      }
      else {
        // HANDLE IPSA ENABLED USER DEFINED FREE HERE SUCH THAT THE SECOND ARGUMENT
        // COULD BE RETURNED AS WELL. THIS MEANS WE NEED TO HAVE A EQUIVALENT OF
        // MU_LIST AND CHI_LIST FOR FUNCTION CALLS ON HEAP OBJECT RELATED TRACKING
        arg = Ipsa()->Callee_frees_heap_memory(Dna(), call_stmt);
      }
    }
    else
      return NULL;
  }
  else {
    arg = call_stmt->Rhs();
  }
  return arg->Find_actual_arg();
}


// =============================================================================
// COMP_UNIT::Is_stmt_in_loop
// Check if stmt is in loop and can be executed many times
// =============================================================================
BOOL
COMP_UNIT::Is_stmt_in_loop(STMTREP *sr)
{
  BB_NODE *bb = sr->Bb();
  while (bb != NULL) {
    if (bb->Loop() != NULL)
      return TRUE;
    bb = bb->Idom();
  }

  return Dna()->Is_called_in_loop();
}

// =============================================================================
//
//  COMP_UNIT::Find_return_value, service routine for Handle_call, more check?
//
// =============================================================================
CODEREP*
COMP_UNIT::Find_return_value(STMTREP *call_stmt)
{
  CHI_LIST_ITER chi_iter;
  CHI_NODE *cnode;
  CHI_LIST *chi_list = call_stmt->Chi_list();
  CODEREP* return_cr = NULL;
#ifdef Is_True_On
  int number = 0;
#endif
  FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
    if (cnode->Live()) {
      CODEREP* cr = cnode->RESULT();
      AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(cr->Aux_id());
      if(sym->Is_return_preg() &&
         cr->Usecnt()  > 0)
      {
        return_cr = cr;
#ifdef Is_True_On
        number++;
#else
        break;
#endif
      }
    }
  }

  // Is_True(number <= 1, ("found more than 1 used return cr"));
  // MCALL would pass the return value in two PREGs.  Struct return is
  // irrelevant to side effect processing.
#ifdef Is_True_On
  if (number > 1) return NULL;
#endif

  return return_cr;
}

// =============================================================================
//
//  COMP_UNIT::Find_return_value with the same symbol as the one in lda_node
//
// =============================================================================
CODEREP*
COMP_UNIT::Find_return_value(STMTREP *call_stmt, CODEREP *lda_node)
{
  Is_True(lda_node->Kind() == CK_LDA,
          ("VSA::Find_return_value (STMTREP*, CODEREP*) expects LDA node\n") );

  CHI_LIST_ITER chi_iter;
  CHI_NODE *cnode;
  CHI_LIST *chi_list = call_stmt->Chi_list();
  CODEREP* return_cr = NULL;
  AUX_STAB_ENTRY *lda_sym = Opt_stab()->Aux_stab_entry(lda_node->Lda_aux_id());
  AUX_STAB_ENTRY *sym;

  FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
    if (cnode->Live()) {
      sym = Opt_stab()->Aux_stab_entry(cnode->Aux_id());
      if (sym->St() == lda_sym->St()) {
        // TODO TODO TODO, should check the offset and size of the aux_entry
        // USE the enclosing relation of the aux_entry is the alternative
        return_cr = cnode->RESULT();
        return return_cr;
      }
    }
  }

  MU_LIST_ITER mu_iter;
  MU_NODE *mnode;
  FOR_ALL_NODE( mnode, mu_iter, Init(call_stmt->Mu_list())) {
    sym = Opt_stab()->Aux_stab_entry(mnode->Aux_id());
    if (sym->St() == lda_sym->St()) {
      return_cr = mnode->OPND();
      return return_cr;
    }
  }
  return return_cr;
}

// =============================================================================
//  COMP_UNIT::Get_end_srcpos
//  return the srcpos of function end
// =============================================================================
SRCPOS
COMP_UNIT::Get_end_srcpos(void)
{
  // check end srcpos
  SRCPOS spos = End_srcpos();
  if (spos)
    return spos;

  // check last bb
  BB_NODE *bb = Cfg()->Last_bb();
  while (bb) {
    if (bb->Last_stmtrep()) {
      spos = bb->Last_stmtrep()->Linenum();
      if (spos)
        break;
    }
    bb = bb->Prev();
  }

  Is_True(spos, ("fail to get end spos"));
  return spos;
}

// =============================================================================
//
//  COMP_UNIT::Handle_call_sideffect: prune the mu / chi list of a call if could
//
// =============================================================================
void
VSA::Handle_call_sideffect(STMTREP *call_stmt)
{
  OPERATOR opr = call_stmt->Opr();
  BOOL no_sideffect = FALSE;
  if (opr == OPR_CALL && OPERATOR_has_sym(opr)) {
    no_sideffect = Vsa_check_no_sideffect(ST_name(call_stmt->St()));
  } else if (opr == OPR_INTRINSIC_CALL) {
    no_sideffect = Vsa_check_no_sideffect(INTRINSIC_name(call_stmt->Rhs()->Intrinsic()));
  }

  if (!no_sideffect)
    return;

  // remove chi list entry except the return register
  CHI_LIST_ITER chi_iter;
  CHI_NODE *cnode;
  CHI_LIST *chi_list = call_stmt->Chi_list();

  FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
    if (cnode->Live()) {
      CODEREP* cr = cnode->RESULT();
      IDTYPE   auxid = cr->Aux_id();
      if (auxid == Opt_stab()->Return_vsym()) continue;
      if (auxid == Opt_stab()->Default_vsym()) continue;
      AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(auxid);
      if(sym->Is_return_preg() && cr->Usecnt() > 0) continue;

      cnode->Set_live(FALSE);
      Is_Trace(Tracing(), (TFile, "VSA::Handle_call_sideffect: set_not_live sym%d\n",
                           cr->Aux_id()));
    }
  }
}

// =============================================================================
//
//  VSA::Handle_call performs two main functionalities:
//       1. creates heap_obj notion for  malloc/free related analysis
//       2. construct RNA structure, callsite speecific context, for IPSA
//
// =============================================================================
void
VSA::Handle_call(STMTREP *call_stmt, BB_NODE *bb, MEM_POOL *pool)
{
  OPERATOR opr = call_stmt->Opr();

  Is_True(opr == OPR_CALL ||
          opr == OPR_ICALL ||
          opr == OPR_INTRINSIC_CALL,
          ("VSA::Handle_call: Can handle only calls"));

  // since this procedure only intended for malloc related work, we do not
  // deal with OPR_ICALL, nordo nested PU now.  Will do when we need to.
  // if (opr == OPR_ICALL) {
  //   _call_bb_list = _call_bb_list->Prepend(bb, pool);
  //   return;
  // }

  // for RSC obj
  DNA_NODE *dna = Dna();
  RNA_NODE *rna = Sr_2_rna(call_stmt);
  Is_True(rna != NULL, ("Sr_2_rna returns NULL"));
  if (rna && rna->Is_flag_set(RBC_SE_MODEL)) {
    Ipsa()->Rbc()->Eval__mvsa_model(dna, rna, pool);
  }
  // end for RSC obj

  if (!Callee_frees_heap_memory(call_stmt) &&
      !Callee_returns_new_heap_memory(call_stmt)) {
    _call_bb_list = _call_bb_list->Prepend(bb, pool);
  }

  Handle_call_sideffect(call_stmt);

  CODEREP      *actual_arg;
  HEAP_OBJ_REP *heap_obj_rep = NULL;

  BOOL maybe = FALSE;
  if ((actual_arg = Callee_frees_heap_memory(call_stmt, &maybe)) != NULL) {
    if (!(actual_arg->Kind() == CK_VAR && Dna()->Is_aux_global(actual_arg->Aux_id()))) {
      actual_arg = call_stmt->Rhs()->Find_actual_arg();
      BOOL   is_ptr;
      IDTYPE_SET visited_set;
      IDTYPE arg_param = Dna()->Find_param_references(actual_arg, &is_ptr, IN_NONE, visited_set);
      if (actual_arg->Kind() == CK_VAR && arg_param != INVALID_VAR_IDX) {
        if (maybe == FALSE)
          maybe = bb->Rcfg_dom_frontier() != NULL && !bb->Rcfg_dom_frontier()->EmptyP();
        // ignore DBF for function call from stl clang front end for now
        if (!Vsa_check_regex_sym_ignore(ST_name(call_stmt->St())))
          Dna()->Set_deallocate(maybe);
        Dna()->Set_parm_flag(arg_param, REF_FREED);
      }
    }

    Is_Trace(Tracing(), (TFile, "VSA::Handle call: processing heap obj\n"));
    Is_Trace_cmd(Tracing(),call_stmt->Print(TFile));

    if (actual_arg->Kind() == CK_LDA) {
#if 0 // move this section of logic to Rename_call to avoid zero version constraint
      CODEREP *related_store = Comp_unit()->Find_return_value(call_stmt, actual_arg);
      heap_obj_rep = (related_store)? Cr_2_heap_obj(related_store) : NULL;
      if (heap_obj_rep)
        Enter_cr_heap_obj_map(call_stmt->Rhs()/*parm_node*/, heap_obj_rep);
#endif
    }
    else if (actual_arg->Kind() == CK_VAR) {
      Is_True(actual_arg->Kind() == CK_VAR,
              ("VSA:Handle_call: argument for free is not a variable"));
      heap_obj_rep = Cr_2_heap_obj(actual_arg);
      if (heap_obj_rep == NULL) {
        // stash away this call for later to resolve
        Is_Trace(Tracing(), (TFile, "VSA::Handle_call, Prepend ref for free call :\n"));
        Is_Trace_cmd(Tracing(), call_stmt->Rhs()->Print(8, TFile));
        Prepend_ref(call_stmt);
      }
    }
    if (heap_obj_rep != NULL) {
      HEAP_OBJ_REP *new_heap_obj_rep = Clone_heap_obj(heap_obj_rep, bb, pool);
      new_heap_obj_rep->Set_attr(ROR_DEF_BY_FREE);
      new_heap_obj_rep->Set_srcpos_node(call_stmt, Dna(), PATHINFO_PARM);
      new_heap_obj_rep->Set_stmt_def(call_stmt, Dna());

      // enter the new_heap_obj_rep to hor_map, correct it in VSA::Rename_call
      Enter_cr_heap_obj_refmap(call_stmt->Rhs()/*parm_node*/, new_heap_obj_rep);
      if (actual_arg->Kind() == CK_VAR && Dna()->Is_aux_global(actual_arg->Aux_id())) {
        // printf("callee frees heap memory through global variable\n");
        // update the heap_obj in the chi list's global
        // no need to sync with the rename since new_heap_obj_rep will be updated
        CHI_NODE *cnode = Find_stmt_var_chi(call_stmt, actual_arg);
        CODEREP  *chi_result = (cnode)? cnode->RESULT(): NULL;
        if (chi_result)
          Enter_cr_heap_obj_map(chi_result, new_heap_obj_rep);
      }

      // treat free as a new definition of the existing heap_obj_rep
      if (!Callee_returns_new_heap_memory(call_stmt))  // exclude realloc
        Enter_cr_heap_obj_map(call_stmt->Rhs(), new_heap_obj_rep); // annotate Rhs new ho
      Is_Trace_cmd(Tracing(), heap_obj_rep->Heap_obj()->Print(TFile));
    }
  }

  CODEREP *call_return_store;
  if (Callee_returns_new_heap_memory(call_stmt)) {
    call_return_store = Comp_unit()->Find_return_value(call_stmt);
    // TODO: need to call dna()->Set_flag(DNA_MEMORY_ALLOCATION); if pointer return
    
    Is_Trace(Tracing(), (TFile, "VSA::Handle call: processing heap obj\n"));
    Is_Trace_cmd(Tracing(), call_stmt->Print(TFile));

    if (call_return_store) {
      Is_Trace(Tracing(), (TFile, "call return value is stored in:  "));
      Is_Trace_cmd(Tracing(), call_return_store->Print(0, TFile));

      heap_obj_rep = Allocate_heap_obj(call_return_store, bb, pool);
      heap_obj_rep->Set_attr(ROR_DEF_BY_ALLOC);
      heap_obj_rep->Set_stmt_def(call_stmt, Dna());
      CODEREP* size = Vsa_malloc_size(call_stmt, call_return_store, Comp_unit());
      if (size != NULL) {
        heap_obj_rep->Heap_obj()->Set_byte_size(size);
      }
      heap_obj_rep->Set_srcpos_node(call_stmt,
                                    Dna(),
                                    PATHINFO_ALLOC);
      Enter_cr_heap_obj_map(call_stmt->Rhs(), heap_obj_rep); // for rename
      Enter_cr_heap_obj_map(call_return_store, heap_obj_rep); 
    }
    else if( VSA_Msf() && !PU_java_lang(Get_Current_PU())) {
      ST_IDX st = IPSA_insession() ? Dna()->Get_node_st(call_stmt, NULL) : ST_IDX_ZERO;
      if (Tracing() && st == ST_IDX_ZERO) {
        fprintf(TFile, ("VSA-MISF: No return value for malloc. memory allocated may leak"));
      }
      SRCPOS_HANDLE srcpos_h(NULL, call_stmt, Dna(), Loc_pool());
      const char *name = (st != ST_IDX_ZERO) ? ST_name(st) : "";
      // treat the call stmt as the "key" srcpos for this issue
      srcpos_h.Set_key_srcpos(Dna(), call_stmt, NULL);
      srcpos_h.Set_msgid("MSF.1");
      BOOL ret = Report_vsa_error(NULL, name, MSF, IC_DEFINITELY, &srcpos_h);
      if (ret && VSA_Xsca) {
        Report_xsca_error(NULL, name, "MSR_22_1", IC_DEFINITELY, &srcpos_h);
      }
    }
  }

  Is_True(rna, ("VSA::Handle_call call site rna is NULL, dna : %s, callsite : sr%d\n", Dna()->Fname(), call_stmt->Stmtrep_id()));

  if (IPSA_insession() && !Dna()->Non_functional()) {
    RNA_NODE *rna  = Ipsa()->Eval_callsite_arg_list(Dna(), call_stmt);

    if (rna != NULL) {
      // the callee could also allocate a memory space and assign to the param
      // need to create a new heap_obj_rep for the result
      // do not call Create_refn_vsym to create vo inside Handle_call because
      // ho renaming isn't done yet
      //Create_refn_vsym(call_stmt->Rhs(), call_stmt, pool, TRUE);

      IDTYPE oparam = INVALID_VAR_IDX;
      INT64  adjuster = 0;
      CODEREP  *retv = Ipsa()->Eval_callee_side_effect_return_value(rna, &oparam, &adjuster);
      // add a check for Callee_returns_new_heap_memory temporarily
      // we may need detailed model for this kind of callee:
      // void* foo(void* p, int x) {
      //    if (x) return p;
      //    else return malloc(x+1);
      // }
      // return value may be merge of new alloc hor and old hor on p.
      if (retv != NULL &&
          oparam <= rna->Arg_cnt() &&
          !Callee_returns_new_heap_memory(call_stmt) &&
          (heap_obj_rep = Cr_2_heap_obj(retv)) != NULL) {

        // if oparam == INVALID_VAR_IDX, its value return through preg
        // otherwise, it is assigned by *(arg#oparam)
        if (oparam == INVALID_VAR_IDX)
          call_return_store = Comp_unit()->Find_return_value(call_stmt);
        else {
          call_return_store = rna->Get_arg(oparam);
          if (rna->Is_set_arg_flag(oparam, ARG_LDA) && call_return_store->Kind() == CK_LDA) {
            // we need the chi node result, if it is LDA node
            call_return_store = Comp_unit()->Find_return_value(call_stmt, call_return_store);
            if (call_return_store) {
              // create chi entry with pass through semantics arg needs to be opr_parm node
              // Create_refn_vsym_chi(arg, call_stmt, heap_obj_rep, 0, pool);
            }
          }
          else if (call_return_store->Kind() == CK_VAR) {
            // this is passing by value and there is no side effect
            call_return_store = NULL;
          }
        }
        Is_Trace(Tracing(),
                 (TFile, "VSA::Handle call: processing heap obj for call\n"));
        Is_Trace_cmd(Tracing(), call_stmt->Print(TFile));

        if (call_return_store &&
            ! call_return_store->Is_flag_set(CF_IS_ZERO_VERSION) &&
            call_return_store->Kind() == CK_VAR) {
          // Equivalent to STID handling
          Is_Trace(Tracing(), (TFile, "call return value is stored in:  "));
          Is_Trace_cmd(Tracing(), call_return_store->Print(0, TFile));
          HEAP_OBJ *heap_obj = Find(call_return_store->Aux_id(), FALSE);
          Enter_cr_heap_obj_map(call_return_store, heap_obj_rep, TRUE);
          if (heap_obj != NULL && heap_obj != heap_obj_rep->Heap_obj()) {
            if (heap_obj != heap_obj_rep->Heap_obj()->Get_based_on())
              heap_obj->Set_based_on(heap_obj_rep->Heap_obj());
            Merge(heap_obj, heap_obj_rep->Heap_obj());
          }
        }
      }
      else {
        call_return_store = Comp_unit()->Find_return_value(call_stmt);
        if (call_return_store != NULL && call_return_store->Kind() == CK_VAR &&
            TY_kind(call_return_store->Lod_ty()) == KIND_POINTER &&
            Cr_2_heap_obj(call_return_store) == NULL) {
          HEAP_OBJ_REP* hor = Find_or_create_hor(call_return_store, bb, pool);
          if (hor != NULL) {
            Enter_cr_heap_obj_map(call_return_store, hor);
            Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Handle_call CALL-return: an unknown heap_obj_rep assigned to a known pointer\n"));
          }
        }
      }
    }
  }
}


BOOL
VSA::Is_ivar_need_vsym(CODEREP *cr, STMTREP *stmt)
{
  // do not generate vsym vor virtual call vptr or interface call's two iload/tranformed ldid
  Is_True(cr->Kind() == CK_IVAR, ("not ivar"));
  IDTYPE ifldid = cr->I_field_id();
  CODEREP *func_ptr = stmt->Rhs();
  // Checking the following pattern of STMT
  //>  LDID U8 U8 ... <----- Loading the class instace base ptr
  //> U8PARM
  //> ...
  //> XXPARM
  //> ...
  //>   ... <----- Loading the V-Table's Function Ptr Entry
  //>  U8PARM
  //>   LDA <----- UTF_constant_function_name
  //>  U8PARM
  //> U8INTRINSIC_OP 1667
  //>I4ICALL
  if (stmt->Opr() == OPR_ICALL &&
      stmt->Rhs()->Opr() == OPR_ICALL &&
      stmt->Rhs()->Kid_count() > 0 &&
      Is_valid_lookup_virt_op(stmt->Rhs()->Opnd(stmt->Rhs()->Kid_count() - 1))) {

    // Get the last parameter, and extract LOOKUP_VIRT_FUNC's first param.
    func_ptr = Get_lookup_virt_original_target_info(stmt->
                    Rhs()->Opnd(stmt->Rhs()->Kid_count() - 1));

    // If virtual call, check for matching the function ptr. (a possible iload) 
    if (Is_stmt_virtual_call(stmt) &&
        func_ptr->Kind() == CK_IVAR &&
        (cr == func_ptr ||
         cr == func_ptr->Ilod_base()))
      return FALSE;

    // Check for interface call, if the cr matches the
    // iload on the icall's first param or its load_base.
    if (Is_lookup_virt_op_interface_call(func_ptr) &&
        (stmt->Rhs()->Opnd(0)->Ilod_base() == cr ||
         stmt->Rhs()->Opnd(0)->Ilod_base()->Kind() == CK_IVAR &&
         stmt->Rhs()->Opnd(0)->Ilod_base()->Ilod_base() == cr))
      return FALSE;

    return TRUE;
  }

  // In case of C/C++ non-LOOKUP_VIRT_FUNC function pointer.
  if (stmt->Opr() == OPR_ICALL &&
      stmt->Call_flags() & WN_CALL_IS_VIRTUAL &&
      ifldid == 1 &&
      stmt->Rhs()->Opnd(stmt->Rhs()->Kid_count()- 1)->Kind() == CK_IVAR &&
      stmt->Rhs()->Opnd(stmt->Rhs()->Kid_count()- 1)->Ilod_base() == cr) {
    return FALSE;
  } else if(stmt->Opr() == OPR_INTRINSIC_CALL &&
            stmt->Rhs()->Intrinsic() == INTRN_LOOKUP_IF &&
            ifldid == 1 &&
            (cr == stmt->Rhs()->Opnd(0)->Ilod_base() ||
             cr == stmt->Rhs()->Opnd(0)->Ilod_base()->Ilod_base())) {
    return FALSE;
  }
  return TRUE;
}


// =============================================================================
//
//  VSA::Rename_call rename heap_obj notion for malloc/free related analysis
//
// =============================================================================
void
VSA::Rename_call(STMTREP *call_stmt)
{
  OPERATOR opr = call_stmt->Opr();

  Is_True(opr == OPR_CALL ||
          opr == OPR_ICALL ||
          opr == OPR_INTRINSIC_CALL,
          ("VSA::Rename_call: Can handle only calls"));

  // since this procedure only intended for malloc related work, we do not
  // deal with OPR_ICALL, nor do nested PU now.  Will do when we need to.
  Rename_horef(call_stmt->Rhs(), call_stmt);  // rename the argument list, includes free

  if (opr == OPR_ICALL) return;

  CODEREP      *actual_arg;
  HEAP_OBJ_REP *heap_obj_rep = NULL;

  DNA_NODE *dna = Dna();
  RNA_NODE *rna = dna->Get_callsite_rna(call_stmt);
  DNA_NODE *callee = Ipsa()->Get_dna(rna->Uniq_callee());
  CODEREP *rhs = call_stmt->Rhs();

  if ((actual_arg = Callee_frees_heap_memory(call_stmt)) != NULL) {
    if (actual_arg->Kind() != CK_VAR)
      actual_arg = call_stmt->Rhs()->Find_actual_arg();

    if (actual_arg->Kind() == CK_LDA) {
      CODEREP *related_store = Comp_unit()->Find_return_value(call_stmt, actual_arg);
      heap_obj_rep = (related_store)? Cr_2_heap_obj(related_store) : NULL;
      if (heap_obj_rep != NULL) {
        if (heap_obj_rep != Null_hor() &&
            heap_obj_rep->Heap_obj()->Kind() != RSC_KIND_LDA) {
          HEAP_OBJ_REP *new_heap_obj_rep = Clone_heap_obj(heap_obj_rep,
                                                          call_stmt->Bb(), Mem_pool());
          new_heap_obj_rep->Set_attr(ROR_DEF_BY_FREE);
          new_heap_obj_rep->Set_srcpos_node(call_stmt, Dna(), PATHINFO_PARM);
          new_heap_obj_rep->Set_stmt_def(call_stmt, Dna());
          heap_obj_rep = new_heap_obj_rep;
        }
        Enter_cr_heap_obj_map(call_stmt->Rhs(), heap_obj_rep);
      }
    }
    else if (actual_arg->Kind() == CK_VAR) {
      heap_obj_rep = Cr_2_heap_obj_ref(call_stmt->Rhs());  // to generate new version of ho
      // Is_True(heap_obj_rep != NULL, ("VSA::Rename free: got NULL heap_obj_rep for argument"));
    }
    if (heap_obj_rep != NULL && heap_obj_rep != Null_hor()) {
      // if heap object is not defined for this identifier to be freed
      // it could be undefined or partially undefined.  Classify_dbf_error
      // will take care of the error report
      HEAP_OBJ_REP *cur_hor = heap_obj_rep->Heap_obj()->Top_of_stack();
      Enter_cr_heap_obj_refmap(call_stmt->Rhs(), cur_hor);

      if (heap_obj_rep->Heap_obj()->Kind() != RSC_KIND_LDA) {
        // if no HOR annotated on actual_arg, annotate current hor on it
        if (actual_arg->Kind() == CK_VAR && Cr_2_heap_obj(actual_arg) == NULL)
          Enter_cr_heap_obj_map(actual_arg, cur_hor);

        IDTYPE version = heap_obj_rep->Gen_name(call_stmt);
        heap_obj_rep->Set_prev_ver(cur_hor);      // update due to pending free
        heap_obj_rep->Set_srcpos_node(call_stmt, Dna(), PATHINFO_FREE);
        Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Rename free: "));
        Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in the following stmt\n"));
        Is_Trace_cmd(Tracing(), call_stmt->Print(TFile));
        if (VSA_Hor_Unification)
          Update_ulist_w_free_rename(heap_obj_rep, cur_hor, call_stmt);
      }
      else { // we do not rename if the heap_obj_rep for &variable
        if (!Callee_returns_new_heap_memory(call_stmt))  // exclude realloc
          Enter_cr_heap_obj_map(call_stmt->Rhs(), cur_hor);
      }
    }
  }
  // for RSC obj
  else if (opr == OPR_CALL && OPERATOR_has_sym(opr)) {
    if (callee)
      if (callee->Is_set_rbc_flag(DNA_RBC_MODEL)) {
        // try to rename rsc delete first
        heap_obj_rep = Cr_2_heap_obj_ref(rhs);
        if (heap_obj_rep != NULL) {
          HEAP_OBJ_REP *cur_hor = heap_obj_rep->Heap_obj()->Top_of_stack();
          Enter_cr_heap_obj_refmap(rhs, cur_hor);
          IDTYPE version = heap_obj_rep->Gen_name(call_stmt);
          heap_obj_rep->Set_prev_ver(cur_hor);
          heap_obj_rep->Set_srcpos_node(call_stmt, Dna(), PATHINFO_FREE);
          Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ VSA::Rename rsc_delete: "));
          Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
          Is_Trace(Tracing(), (TFile, " in the following stmt\n"));
          Is_Trace_cmd(Tracing(), call_stmt->Print(TFile));
          if (VSA_Hor_Unification)
            Update_ulist_w_free_rename(heap_obj_rep, cur_hor, call_stmt);
        }
      }
  }
  else if (opr == OPR_ICALL) {
    // OPR_ICALL is skipped at the beginning, add codes here for future use
    const CALLEE_VECTOR& callee_list = rna->Callee_list();
    for (CALLEE_VECTOR::const_iterator iter = callee_list.begin(); iter != callee_list.end(); iter++) {
      callee = Ipsa()->Get_dna(iter->Callee());
      if (callee)
        if (callee->Is_set_rbc_flag(DNA_RBC_MODEL)) {
          // try to rename rsc delete first
          heap_obj_rep = Cr_2_heap_obj_ref(rhs);
          if (heap_obj_rep != NULL) {
            HEAP_OBJ_REP *cur_hor = heap_obj_rep->Heap_obj()->Top_of_stack();
            Enter_cr_heap_obj_refmap(rhs, cur_hor);
            IDTYPE version = heap_obj_rep->Gen_name(call_stmt);
            heap_obj_rep->Set_prev_ver(cur_hor);
            heap_obj_rep->Set_srcpos_node(call_stmt, Dna(), PATHINFO_FREE);
            Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ VSA::Rename rsc_delete: "));
            Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
            Is_Trace(Tracing(), (TFile, " in the following stmt\n"));
            Is_Trace_cmd(Tracing(), call_stmt->Print(TFile));
            if (VSA_Hor_Unification)
              Update_ulist_w_free_rename(heap_obj_rep, cur_hor, call_stmt);
          }
        }
    }
  }
  // end for RSC obj
  HEAPSTATE heapstate;
  if ((heapstate = Callee_returns_new_heap_memory(call_stmt, TRUE))) {
    heap_obj_rep = Cr_2_heap_obj(call_stmt->Rhs());

    if (heap_obj_rep != NULL) { // see comment in callee_frees case
      IDTYPE version = heap_obj_rep->Gen_name(call_stmt);
      Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Rename malloc: "));
      Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
      Is_Trace(Tracing(), (TFile, "in the following stmt\n"));
      Is_Trace_cmd(Tracing(), call_stmt->Print(TFile));
      if (heapstate == HS_DANGLE) {
        // the callee returns a dangle pointer
        heap_obj_rep->Set_attr(ROR_DEF_BY_DANGLE);
      }
    }
  }
  // for RSC obj
  else if (opr == OPR_CALL && OPERATOR_has_sym(opr)) {
    if (callee)
      if (callee->Is_set_rbc_flag(DNA_RBC_MODEL)) {
        heap_obj_rep = Cr_2_heap_obj(rhs);
        if (heap_obj_rep != NULL) {
          IDTYPE version = heap_obj_rep->Gen_name(call_stmt);
          Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ VSA::Rename rsc_add: "));
          Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
          Is_Trace(Tracing(), (TFile, "in the following stmt\n"));
          Is_Trace_cmd(Tracing(), call_stmt->Print(TFile));
        }
      }
  }
  else if (opr == OPR_ICALL) {
    // OPR_ICALL is skipped at the beginning, add codes here for future use
    const CALLEE_VECTOR& callee_list = rna->Callee_list();
    for (CALLEE_VECTOR::const_iterator iter = callee_list.begin(); iter != callee_list.end(); iter++) {
      callee = Ipsa()->Get_dna(iter->Callee());
      if (callee)
        if (callee->Is_set_rbc_flag(DNA_RBC_MODEL)) {
          heap_obj_rep = Cr_2_heap_obj(rhs);
          if (heap_obj_rep != NULL) {
            IDTYPE version = heap_obj_rep->Gen_name(call_stmt);
            Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ VSA::Rename rsc_add: "));
            Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
            Is_Trace(Tracing(), (TFile, "in the following stmt\n"));
            Is_Trace_cmd(Tracing(), call_stmt->Print(TFile));
          }
        }
    }
  }
  // end for RSC obj

  CODEREP* call_return_store = Comp_unit()->Find_return_value(call_stmt);
  if (call_return_store != NULL && call_return_store->Kind() == CK_VAR &&
      TY_kind(call_return_store->Lod_ty()) == KIND_POINTER) {
    HEAP_OBJ_REP* hor = Cr_2_heap_obj(call_return_store);
    if (hor != NULL && hor->Attr() == ROR_DEF_BY_CHI &&
        hor->Stmt_def() == call_stmt) {
      IDTYPE version = hor->Gen_name(call_stmt);
      Is_Trace(Tracing(),
              (TFile, "@@@@@@@@@@ VSA::Rename_call gen ver %d for return value: ", version));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(), (TFile, "in call\n"));
      Is_Trace_cmd(Tracing(), call_stmt->Print(TFile));
    }
  }
}

// =============================================================================
//
//  VSA::Rename_vocall rename heap_obj notion for malloc/free related analysis
//    for instances that is carried by vsym_obj_rep
//
// =============================================================================
void
VSA::Rename_vocall(STMTREP *call_stmt, MEM_POOL* def_bbs_pool)
{
  OPERATOR opr = call_stmt->Opr();

  Is_True(opr == OPR_CALL ||
          opr == OPR_ICALL ||
          opr == OPR_INTRINSIC_CALL,
          ("VSA::Rename_call: Can handle only calls"));

  // since this pvrocedure only intended for malloc related work, we do not
  // deal with OPR_ICALL, nordo nested PU now.  Will do when we need to.
  Rename_voref(call_stmt->Rhs());  // rename the argument list, includes free

  //if (opr == OPR_ICALL) return;

  CODEREP      *actual_arg;
  HEAP_OBJ_REP *heap_obj_rep;

  if ((actual_arg = Callee_frees_heap_memory(call_stmt)) != NULL) {

    if (actual_arg->Kind() == CK_VAR) {
      heap_obj_rep = Cr_2_heap_obj(call_stmt->Rhs());  // fetch the latest version
      if (heap_obj_rep != NULL) {
        heap_obj_rep->Heap_obj()->Push(heap_obj_rep, call_stmt);
      }
    }
    else if (actual_arg->Kind() == CK_IVAR && actual_arg->Opr() == OPR_ILOAD) {
      VSYM_OBJ_REP *vor = Cr_2_vor(actual_arg);
      heap_obj_rep = Cr_2_heap_obj(actual_arg);
      if (heap_obj_rep == NULL && vor)
        heap_obj_rep = vor->Hor();
      if (heap_obj_rep != NULL) {
        // if heap object is not defined for this identifier to be freed
        // it could be undefined or partially undefined.  Classify_dbf_error
        // will take care of the error report
        HEAP_OBJ_REP *cur_hor = heap_obj_rep->Heap_obj()->Top_of_stack();
        Enter_cr_heap_obj_refmap(call_stmt->Rhs(), cur_hor);
        if (vor && vor->Hor() != cur_hor) 
          vor->Set_hor(cur_hor);

        // create a new heap_obj_rep, since ILOAD has been skipped for this process earlier
        HEAP_OBJ_REP *new_heap_obj_rep = Clone_heap_obj(heap_obj_rep, call_stmt->Bb(), Mem_pool());
        new_heap_obj_rep->Set_attr(ROR_DEF_BY_FREE);
        new_heap_obj_rep->Set_srcpos_node(call_stmt,
                                          Dna(),
                                          PATHINFO_PARM);
        new_heap_obj_rep->Set_stmt_def(call_stmt, Dna());

        IDTYPE version = new_heap_obj_rep->Gen_name(call_stmt);
        new_heap_obj_rep->Set_prev_ver(cur_hor);      // update due to pending free
        new_heap_obj_rep->Set_srcpos_node(call_stmt,
                                          Dna(),
                                          PATHINFO_FREE);
        // realloc does malloc and free but the map only allow 1 hor
        // replace malloc hor with free hor for this case
        BOOL replace = Callee_returns_new_heap_memory(call_stmt) ? TRUE : FALSE;
        Enter_cr_heap_obj_map(call_stmt->Rhs(), new_heap_obj_rep, replace);
        Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ VSA::Rename_vocall free: "));
        Is_Trace_cmd(Tracing(), new_heap_obj_rep->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in the following stmt\n"));
        Is_Trace_cmd(Tracing(), call_stmt->Print(TFile));
        if (VSA_Hor_Unification)
          Update_ulist_w_free_rename(new_heap_obj_rep, cur_hor, call_stmt);
      }
    }
  }

  if (Callee_returns_new_heap_memory(call_stmt)) {
    heap_obj_rep = Cr_2_heap_obj(call_stmt->Rhs());
    if (heap_obj_rep)
      heap_obj_rep->Heap_obj()->Push(heap_obj_rep, call_stmt);
    if (VSA_Vsym_Memcall()) {
      VSYM_OBJ_REP* vor = Cr_2_vor(call_stmt->Rhs());
      if (vor != NULL) {
        if (heap_obj_rep)
          vor->Set_hor(heap_obj_rep);
        IDTYPE version = vor->Gen_name(call_stmt);
        Is_Trace(Tracing(), (TFile, "VSA_Vsym_Memcall: rename vor: "));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " with hor: "));
        Is_Trace_cmd(Tracing(), heap_obj_rep->Print(TFile));
        Is_Trace(Tracing(), (TFile, " on stmt:\n"));
        Is_Trace_cmd(Tracing(), call_stmt->Print(TFile));
      }
    }
  }

  DNA_NODE *dna = Comp_unit()->Dna();
  RNA_NODE *rna = dna->Get_callsite_rna(call_stmt);

  // Do not create vor for jni calls, they will be created
  // in model jni phase
  if(Ipsa()->Is_jni_call(rna)) {
    return;
  }
  if (opr == OPR_CALL && OPERATOR_has_sym(opr) &&
      Vsa_check_no_sideffect(ST_name(call_stmt->St()))) {
    return;
  }
  // for ICALL with uniq callee
  DNA_NODE *uniq_callee = Ipsa()->Get_dna(rna->Uniq_callee());
  if(uniq_callee && Vsa_check_no_sideffect(uniq_callee->Fname())) {
    return;
  }

  // end for RSC obj
  Create_call_vsym_mu_chi(call_stmt, def_bbs_pool);
}


// =============================================================================
//
// =============================================================================
VAL_RANGE_RESULT
VSA::Check_val_range(CODEREP* cr, BB_NODE* bb)
{
  Is_True((cr->Opr() == OPR_ARRAY), ("Operand must be array"));

  if (cr->Opnd(0)->Kind() == CK_VAR) {
    AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(cr->Opnd(0)->Aux_id());
    ST *st = sym->St();
    if (st && (ST_sclass(st) == SCLASS_FORMAL)) {
      // we ignore formals for now until RNA/DNA and -xa is on
      return VAL_INR;
    }
  }
  if (cr->Opnd(1)->Kind() == CK_CONST) {
    if (cr->Opnd(2)->Kind() == CK_CONST) {
      // printf("v1 %x, v2 %x\n", cr->Opnd(1)->Const_val(),cr->Opnd(2)->Const_val());
      
      if ((cr->Opnd(2)->Const_val()) < (cr->Opnd(1)->Const_val()))
        return VAL_INR;
      else
        return VAL_OOR;
    }
    else {
      // TODO: evaluate a const
      return VAL_May_OOR;
    }
  }
  Is_True(0, ("Unexpected Opnd (non-const) inside Array"));
  return VAL_INR;
}


// =============================================================================
//
// =============================================================================
VAL_RANGE_RESULT
VSA::Check_val_range(CODEREP* cr, BB_NODE* bb, INT32 u, INT32 l)
{
  if ((cr->Kind() == CK_VAR)) {
    AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(cr->Aux_id());
    ST *st = sym->St();
#if 0
    if (st && ST_sclass(st) == SCLASS_FORMAL) {
      // we ignore formals for now until RNA/DNA and -xa is on
      return VAL_INR;
    }
#endif
    if (cr->Value_not_def()) {
      // until val range analysis is powerful enough, we give up for now
      return (cr->Kind() == CK_OP) ? VAL_INR : VAL_OOR;
    }
    if (cr->Value_maydef()) {
      // until val range analysis is powerful enough, we give up for now
      return (cr->Kind() == CK_OP) ? VAL_INR : VAL_May_OOR;
    }
    if (cr->Value_invalid_addr())
      return VAL_OOR;  // we are dealing with ILOAD/ISTORE here
    else if (cr->Value_maydangling())
      return VAL_May_OOR;
    // we don't have flag for float, set it to May_OOR
    return MTYPE_is_float(cr->Dtyp()) ? VAL_May_OOR : VAL_INR;
  }
  else {
    if (cr->Kind() == CK_OP) {
      // until val range analysis is powerful enough, we give up for now
      return VAL_INR;
    }
    if (cr->Kind() == CK_LDA) {
      // until val range analysis is powerful enough, we give up for now
      return VAL_INR;
    }
      
    if (cr->Kind() == CK_IVAR && cr->Ivar_defstmt()) {
      if (cr->Ivar_defstmt()->Opr() == OPR_OPT_CHI) {
        if (cr->Value_invalid_addr()) {
            return VAL_OOR;
        }
        return VAL_May_OOR;
      }
      else {
        if (cr->Value_invalid_addr())
          return VAL_OOR;
        // TODO: handle *p on RHS where p could be 0-/+k where k is small
        else if (cr->Value_maydangling())
          return VAL_May_OOR;
        return VAL_INR; // need to check symbol scope, value (0, ...), ...
      }
    }
    else {
      // we should see entry chi here, it is undefine
      return VAL_May_OOR;
    }
        
  }
}

// =============================================================================
// VSA::Check_var_zero
// Check if the value of cr in bb can be zero
// =============================================================================
VAL_RANGE_RESULT
VSA::Check_var_zero(CODEREP* cr, BB_NODE* bb, BOOL& is_vra)
{
  Is_True(cr->Kind() == CK_VAR,
          ("unexpected cr kind %d", cr->Kind()));
  VRA* vra = Comp_unit()->Vra();
  // if VRA is unavailable, fall back to VSA::Check_val_range()
  if (vra == NULL) {
    is_vra = FALSE;
    return Check_val_range(cr, bb, 0, 0);
  }
  // check if cr in bb is zero by VRA
  VRA_RESULT ret = vra->Is_var_zero(cr, bb);
  switch (ret) {
  case VA_NO:       is_vra = TRUE;  return VAL_INR;
  case VA_YES:      is_vra = TRUE;  return VAL_OOR;
  case VA_POSSIBLE: is_vra = TRUE;  return VAL_May_OOR;
  // if VRA failed to check the value, fall back to VSA::Check_val_range()
  default:          is_vra = FALSE; return Check_val_range(cr, bb, 0, 0);
  }
}

// =============================================================================
// VSA::Check_expr_zero
// Check if the value of expr in bb can be zero
// =============================================================================
VAL_RANGE_RESULT
VSA::Check_expr_zero(CODEREP* cr, BB_NODE* bb, BOOL& is_vra)
{
  VRA* vra = Comp_unit()->Vra();
  VAL_RANGE_RESULT rr = VAL_UNK_R;
  // if VRA is unavailable, fall back to VSA::Check_val_range()
  if (vra == NULL) {
    is_vra = FALSE;
    rr = Check_val_range(cr, bb, 0, 0);
  }
  else {
    // check if cr in bb is zero by VRA
    VRA_RESULT ret = vra->Is_expr_zero(cr, bb);
    switch (ret) {
    case VA_NO:       is_vra = TRUE;  rr = VAL_INR; break;
    case VA_YES:      is_vra = TRUE;  rr = VAL_OOR; break;
    case VA_POSSIBLE: is_vra = TRUE;  rr = VAL_May_OOR; break;
      // if VRA failed to check the value, fall back to VSA::Check_val_range()
    default:          is_vra = FALSE; rr = Check_val_range(cr, bb, 0, 0); break;
    }
  }

  // check value range with user annotation
  // find value range annotated on coderep
  VALUE_RANGE_VEC *vr_vec = Cr_2_vr(cr->Coderep_id());
  if (vr_vec == NULL) {
    // TODO: may need to deal with propogation
    // annotation on a, analyse on c
    // a = b
    // b = c
  }
  if (vr_vec != NULL) {
    // value range annotation found, re-calculate
    for (INT idx = 0; idx < vr_vec->size(); idx++) {
      VALUE_RANGE *vr = (*vr_vec)[idx];
      if (vr != NULL) {
        is_vra = TRUE;
        if (vr->Not_equal(0)) {
          rr = VAL_INR;
        }
        else {
          rr = VAL_OOR;
        }
        if (rr != VAL_INR)
          break;
      }
    }
  }
  return rr;
}

// =============================================================================
// VSA::Check_expr_in_range
// Check if expr at bb is in range of [lb, lb + size)
// =============================================================================
VAL_RANGE_RESULT
VSA::Check_expr_in_range(CODEREP *expr, BB_NODE* bb, CODEREP *lb, CODEREP *size)
{
  VRA* vra = Comp_unit()->Vra();
  // if VRA is unavailable, fall back to VSA::Check_val_range()
  if (vra == NULL)
    return VAL_UNK_R;
  CODEREP* ub = vra->New_cr(OPCODE_make_op(OPR_ADD, lb->Dtyp(), MTYPE_V),
                            lb, size, TRUE);
  PATH_SELECTED path;
  VRA_RESULT ret = vra->Is_expr_in_range(expr, bb, lb, ub, path);
  switch (ret) {
  case VA_YES:      return VAL_INR;
  case VA_NO:       return VAL_OOR;
  case VA_POSSIBLE: return VAL_May_OOR;
  default:          return VAL_UNK_R;
  }
}

// =============================================================================
// VSA::Check_expr_out_range
// Check if expr at bb is out range of [lb, lb + size)
// =============================================================================
VAL_RANGE_RESULT
VSA::Check_expr_out_range(CODEREP *expr, BB_NODE* bb, CODEREP *lb, CODEREP *size)
{
  VRA* vra = Comp_unit()->Vra();
  // if VRA is unavailable, fall back to VSA::Check_val_range()
  if (vra == NULL)
    return VAL_UNK_R;
  CODEREP* ub = vra->New_cr(OPCODE_make_op(OPR_ADD, lb->Dtyp(), MTYPE_V),
                            lb, size, TRUE);
  PATH_SELECTED path;
  VRA_RESULT ret = vra->Is_expr_out_range(expr, bb, lb, ub, path);
  switch (ret) {
  case VA_NO:       return VAL_INR;
  case VA_YES:      return VAL_OOR;
  case VA_POSSIBLE: return VAL_May_OOR;
  default:          return VAL_UNK_R;
  }
}

// =============================================================================
// VSA::Is_path_possible
// Check if there is a possible path from phi's predecessor indicated by opnd
// to use_bb.
// =============================================================================
BOOL
VSA::Is_path_possible(BB_NODE* use_bb, PHI_NODE* phi, INT opnd) const
{
  VRA* vra = Comp_unit()->Vra();
  if (vra == NULL)
    return TRUE;   // assume there is a path from phi(opnd) to use_bb
  VRA_RESULT ret = vra->Is_path_possible(use_bb, phi, opnd);
  return ret == VA_NO ? FALSE : TRUE;
}

// =============================================================================
// VSA::Is_path_possible_from_caller
// Check if the value used on expr matches the def of the value in caller
// =============================================================================
BOOL
VSA::Is_path_possible_from_caller(RNA_NODE* rna, IDTYPE param, CODEREP* expr,
                                  CALL_STACK& cs, VSYM_TRACKER* tracker, const PATH_SELECTED& path) const
{
  if (param > rna->Arg_cnt())
    return TRUE;
  Is_True(expr->Kind() == CK_OP && expr->Kid_count() == 2, ("bad expr"));
  CODEREP* actual = rna->Get_arg(param);
  STMTREP* callstmt = rna->Callstmt();
  DNA_NODE* caller = Ipsa()->Get_dna(rna->Caller_idx());
  Is_True(caller == Comp_unit()->Dna(), ("wrong vsa used"));

  if (tracker != NULL && tracker->Size() == 1) {
    CODEREP* vsym_cr = NULL;
    VSYM_FLD_REP* fld = tracker->Fld_rep();
    if (actual->Kind() == CK_LDA) {
      // TODO: call Find_ilod_base(actual) early and adjust field id
      // to get the correct field id
      MU_NODE* mu = Find_stmt_var_mu(callstmt, actual->Lda_base_st(), fld);
      if (mu != NULL)
        vsym_cr = mu->OPND();
    }

    if (vsym_cr == NULL) {
      CODEREP* x = Find_ilod_base(actual);
      if (x == NULL)
        return TRUE;

      HEAP_OBJ_REP* hor = Cr_2_heap_obj(x);
      if (hor == NULL)
        return TRUE;

      VSYM_OBJ_REP* vor = Find_hor_mu_vor(callstmt, hor, fld, x);
      if (vor == NULL)
        return TRUE;

      // TODO: follow vsym U-D
      if (vor->Attr() == ROR_DEF_BY_ISTORE) {
        STMTREP* sr = vor->Stmt_def();
        Is_True(sr != NULL &&
                (sr->Opr() == OPR_ISTORE || sr->Opr() == OPR_MSTORE), ("bad istore"));
        vsym_cr = sr->Rhs();
      }
    }

    if (vsym_cr == NULL)
      return TRUE;
    actual = vsym_cr;
  }

  return Is_path_possible(actual, callstmt->Bb(), expr, cs, NULL, path);
}


// =============================================================================
// VSA::Is_path_possible_local
// Check if the value satisfied with comparison expression locally
// =============================================================================
BOOL
VSA::Is_path_possible_local(CODEREP* actual, CODEREP* cmp, BB_NODE* bb,
                            const PATH_SELECTED& path) const
{
  VRA* vra = Comp_unit()->Vra();
  CODEREP* opnd1 = cmp->Opnd(1);
  Is_True(opnd1->Kind() == CK_CONST || opnd1->Kind() == CK_RCONST, ("bad opnd1"));
#if 0
  if (opnd1->Kind() == CK_RCONST) {
    double fval;
    MTYPE  fty;
    {
      CONTEXT_SWITCH callee_ctx(Dna());
      fval = opnd1->Const_fval(Htable());
      fty = opnd1->Dtyp();
    }
    // create a new RCONST CR in caller's context
    TCON tc = Host_To_Targ_Float(fty, fval);
    CODEREP* cr = Alloc_stack_cr(0);
    ST* st = New_Const_Sym(Enter_tcon(tc), MTYPE_To_TY(fty));
    cr->Init_rconst(fty, st);
    opnd1 = caller->Comp_unit()->Htable()->Hash_Rconst(cr);
  }
#endif
  VRA_RESULT res = VA_UNKNOWN;
  IDTYPE bb_id = bb->Id();
  switch (cmp->Opr()) {
  case OPR_EQ:
    res = vra->Expr_cmp_val<OPR_EQ>(actual, bb_id, opnd1, path);
    break;
  case OPR_NE:
    res = vra->Expr_cmp_val<OPR_NE>(actual, bb_id, opnd1, path);
    break;
  case OPR_GT:
    res = vra->Expr_cmp_val<OPR_GT>(actual, bb_id, opnd1, path);
    break;
  case OPR_GE:
    res = vra->Expr_cmp_val<OPR_GE>(actual, bb_id, opnd1, path);
    break;
  case OPR_LT:
    res = vra->Expr_cmp_val<OPR_LT>(actual, bb_id, opnd1, path);
    break;
  case OPR_LE:
    res = vra->Expr_cmp_val<OPR_LE>(actual, bb_id, opnd1, path);
    break;
  default:
    return TRUE;
  }
  return res == VA_NO ? FALSE : TRUE;
}

// =============================================================================
// VSA::Is_path_possible_from_caller
// Check call stack to get caller and check value for cd in caller
// =============================================================================
BOOL
VSA::Is_path_possible_from_caller(IDTYPE param, CODEREP* expr, CALL_STACK& cs,
                                  VSYM_TRACKER* tracker, const PATH_SELECTED& path) const
{
  if (cs.empty()) {
    // Traverse callby list
    for (INT i = VAR_INIT_ID; i < Dna()->Clby_list()->size(); i++) {
      RNA_NODE* rna = (*Dna()->Clby_list())[i];
      if (rna->Is_back_edge())
        continue; // TODO. ignore back edge for recursion
      DNA_NODE* caller = Ipsa()->Get_dna(rna->Caller_idx());
      if (caller == NULL || caller->Non_functional())
        continue;
      VSA *vsa = caller->Comp_unit()->Vsa();
      CONTEXT_SWITCH context(caller);
      if (vsa->Is_path_possible_from_caller(rna, param, expr, cs, tracker, path) == TRUE)
        return TRUE;
    }
    return FALSE;
  }
  else {
    RNA_NODE* rna = cs.top();
    if (rna->Is_back_edge())
      return FALSE; // TODO. ignore back edge for recursion
    DNA_NODE* caller = Ipsa()->Get_dna(rna->Caller_idx());
    VSA *vsa = caller->Comp_unit()->Vsa();
    CONTEXT_SWITCH context(caller);
    cs.pop();
    INT size = cs.size();
    BOOL ret = vsa->Is_path_possible_from_caller(rna, param, expr, cs, tracker, path);
    Is_True(cs.size() == size, ("call stack corrupted"));
    cs.push(rna);
    return ret;
  }
}

// =============================================================================
// VSA::Is_path_possible_from_callee
// Check rna to get callee and check value for cd in callee
// =============================================================================
BOOL
VSA::Is_path_possible_from_callee(RNA_NODE* rna, IDTYPE param, CODEREP* expr, CALL_STACK& cs,
                                  VSYM_TRACKER* tracker, const PATH_SELECTED& path) const
{
  if (rna->Is_back_edge() && cs.size() > 0)
    return FALSE; // TODO. It is better to check if rna is already in cs

  if (param > 0)  // TODO. handle the output parameter later
    return TRUE;

  BOOL ret = TRUE;
  cs.push(rna);
  INT size = cs.size();
  for (CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
       it != rna->Callee_list().end(); it++) {
    DNA_NODE* callee = Ipsa()->Get_dna(it->Callee());
    if (callee == NULL ||
        callee->Non_functional() ||
        callee->Retv_list()->size() == 0)
      continue;

    ret = callee->Is_path_possible_in_callee(Ipsa(), rna, param, expr,
                                             cs, tracker, path);
    if (ret == TRUE)
      break;
  }
  Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
  cs.pop();

  return ret;
}

// =============================================================================
// VSA::Is_path_possible_var
// Check if there is a possible path by checking if the value of var (CK_VAR)
// satistified with compare expression on given path
// =============================================================================
BOOL
VSA::Is_path_possible_var(CODEREP* var, BB_NODE* bb, CODEREP* cmp_expr, CALL_STACK& cs, VSYM_TRACKER* tracker,
                          const PATH_SELECTED& path) const
{
  Is_True(var->Kind() == CK_VAR, ("not var"));
  if (var->Is_flag_set(CF_IS_ZERO_VERSION))
    return TRUE;
  if (var->Is_flag_set(CF_DEF_BY_PHI)) {
#if 0
    // TODO: handle phi and avoid recursion
    PHI_NODE* phi = var->Defphi();
    BB_LIST_ITER bb_iter;
    BB_NODE* pred;
    INT      i = 0;
    IDTYPE   dna_idx = Comp_unit()->Dna()->Dna_idx();
    IDTYPE   phi_bb_idx = phi->Bb()->Id();
    FOR_ALL_ELEM (pred, bb_iter, Init(phi->Bb()->Pred())) {
      if (path.Selected(dna_idx, pred->Id(), phi_bb_idx)) {
        return Is_path_possible(phi->OPND(i), pred, cmp_expr,
                                cs, tracker, path);
      }
      ++ i;
    }
#endif
    // assume TRUE
    return TRUE;
  }
  else if (var->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP* def = var->Defstmt();
    if (OPERATOR_is_call(def->Opr())) {
      RNA_NODE *rna = Sr_2_rna(def);
      if (rna == NULL)
        return TRUE;
      AUX_STAB_ENTRY* aux = Opt_stab()->Aux_stab_entry(var->Aux_id());
      if (aux->Is_return_preg()) {
        return Is_path_possible_from_callee(rna, 0, cmp_expr, cs, tracker, path);
      }
      else if (aux->St() != NULL) {
        CHI_NODE* chi = var->Defchi();
        pair<IDTYPE, BOOL> arg = rna->Get_arg(chi->OPND(), this);
        if (arg.first != INVALID_VAR_IDX && arg.second)
          return Is_path_possible_from_callee(rna, arg.first, cmp_expr, cs, tracker, path);
      }
      // assume TRUE
      return TRUE;
    }
    else if (def->Opr() == OPR_OPT_CHI) {
      IDTYPE param = Dna()->Is_param(var);
      if (param != INVALID_VAR_IDX && !Dna()->Is_root())
        return Is_path_possible_from_caller(param, cmp_expr, cs, tracker, path);
    }
    // assume true
    return TRUE;
  }
  else {
    STMTREP* def = var->Defstmt();
    if (def != NULL) {
      CODEREP* rhs = def->Rhs();
      return Is_path_possible(rhs, def->Bb(), cmp_expr, cs, tracker, path);
    }
    // assume true
    return TRUE;
  }
}

// =============================================================================
// VSA::Is_path_possible_ivar
// Check if there is a possible path by checking if the value of ivar (CK_IVAR)
// satistified with compare expression on given path
// =============================================================================
BOOL
VSA::Is_path_possible_ivar(CODEREP* var, BB_NODE* bb, CODEREP* cmp_expr, CALL_STACK& cs, VSYM_TRACKER* tracker,
                          const PATH_SELECTED& path) const
{
  Is_True(var->Kind() == CK_IVAR, ("not ivar"));

  if (tracker != NULL)
    return TRUE;    // TODO: multiple levels dereference

  VSYM_OBJ_REP* vor = Cr_2_vor(var);
  if (vor == NULL)
    return TRUE;    // no vor, give up

  switch (vor->Attr()) {
  case ROR_DEF_BY_CHI:
    {
      STMTREP *def = vor->Stmt_def();
      if (def != NULL && def->Opr() == OPR_OPT_CHI) {
        CODEREP* x = Find_base_pointer_load(var);
        IDTYPE param;
        if (x == NULL || x->Kind() != CK_VAR ||
            Dna()->Is_root() ||
            (param = Dna()->Is_param(x)) == INVALID_VAR_IDX)
          return TRUE;

        VSYM_TRACKER trc;
        if (trc.Init(this, &var, def, Loc_pool()) != CS_VSYM_UD)
          return TRUE;

        return Is_path_possible_from_caller(param, cmp_expr, cs, &trc, path);
      }
      else if (def != NULL && OPERATOR_is_call(def->Opr())) {
        // TODO: find out parm and call Is_path_possible_from_callee
      }
      // assume TRUE
      return TRUE;
    }
  case ROR_DEF_BY_ISTORE:
    {
      STMTREP *def = vor->Stmt_def();
      if (def != NULL && def->Opr() == OPR_ISTORE)
        return Is_path_possible(def->Rhs(), def->Bb(), cmp_expr, cs, tracker, path);
      // assume TRUE
      return TRUE;
    }
  default:
    return TRUE;    // TODO: other cases, COPY?
  }
}

// =============================================================================
// VSA::Is_path_possible
// Check if there is a possible path by checking if the value of var satistified
// with compare expression on given path
// =============================================================================
BOOL
VSA::Is_path_possible(CODEREP* var, BB_NODE* bb, CODEREP* cmp_expr, CALL_STACK& cs, VSYM_TRACKER* tracker,
                      const PATH_SELECTED& path) const
{
  VRA* vra = Comp_unit()->Vra();
  switch (var->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    //if (!path.Selected(Comp_unit()->Dna()->Dna_idx(), bb->Id()))
    //  return FALSE;
    if (Is_path_possible_local(var, cmp_expr, bb, path) == FALSE)
      return FALSE;
#if 0
    // TODO: handle path passibility from entry to current bb
    // TODO: avoid recursion and reduce compile time
    if (vra != NULL)
    {
      // check if there is a path upper side
      std::vector< std::pair<BB_NODE*, BB_NODE*> > cds;
      vra->Collect_control_dependencies(bb, Cfg()->Entry_bb(), cds);
      for (std::vector< std::pair<BB_NODE*, BB_NODE*> >::iterator it = cds.begin();
           it != cds.end(); ++ it) {
        if (Is_path_possible(it->first, it->second, cs, path) == FALSE)
          return FALSE;
      }
    }
#endif
    return TRUE;
  case CK_VAR:
    if (Is_path_possible_local(var, cmp_expr, bb, path) == FALSE)
      return FALSE;
    return Is_path_possible_var(var, bb, cmp_expr, cs, tracker, path);
  case CK_IVAR:
    if (Is_path_possible_local(var, cmp_expr, bb, path) == FALSE)
      return FALSE;
    return Is_path_possible_ivar(var, bb, cmp_expr, cs, tracker, path);
  case CK_OP:
    return TRUE; // TODO
  defaule:
    Is_True(FALSE, ("wrong cr kind"));
    return TRUE;
  }
  return TRUE;
}

// =============================================================================
// VSA::Is_path_possible
// Check if there is a possible path by checking if the value used in control
// dependency can be matched
// =============================================================================
BOOL
VSA::Is_path_possible(BB_NODE* pred, BB_NODE* succ, CALL_STACK& cs, const PATH_SELECTED& path) const
{
  VRA* vra = Comp_unit()->Vra();
  Is_True(vra != NULL, ("vra is off"));

  STMTREP* sr = pred->Last_stmtrep();
  if (sr == NULL || sr->Rhs() == NULL)
    return TRUE;
  if (sr->Opr() != OPR_TRUEBR &&
      sr->Opr() != OPR_FALSEBR &&
      sr->Opr() != OPR_COMPGOTO)
    return TRUE;  // TODO: handle AGOTO?

  CODEREP* cond = sr->Rhs();
  if (sr->Opr() == OPR_COMPGOTO) {
    Is_True(pred->Kind() == BB_VARGOTO, ("not vargoto"));
    if (pred->Switchdefault() == succ)
      return TRUE;  // TODO: handle default?
    if (cond->Kind() == CK_OP &&
        (cond->Opr() == OPR_CVT || cond->Opr() == OPR_CVTL ||
         cond->Opr() == OPR_TRUNC))
      cond = cond->Opnd(0);
    INT32 i;
    for (i = 0; i < pred->Switchentries(); ++i) {
      if (pred->Switchcase(i) == succ) {
        cond = vra->New_cmp_cr(OPR_EQ, cond, (INT64)i);
        break;
      }
    }
    Is_True(i != pred->Switchentries(), ("not find succ in switchcases"));
  }
  else if (cond->Kind() == CK_LDA ||
           cond->Kind() == CK_VAR ||
           cond->Kind() == CK_IVAR ||
           (cond->Kind() == CK_OP &&
            cond->Opr() != OPR_EQ && cond->Opr() != OPR_NE &&
            cond->Opr() != OPR_GE && cond->Opr() != OPR_GT &&
            cond->Opr() != OPR_LE && cond->Opr() != OPR_LT)) {
    // convert if (a) into if (a != 0)
    cond = vra->New_cmp_cr(OPR_NE, cond, (INT64)0);
  }

  hash_set<CODEREP*> crs;
  if (vra->Analyze_coderep_vars(cond, crs) == FALSE)
    return TRUE;
  // only need to check the first so far because xfa only support x cmp cst.
  if (crs.begin() == crs.end())
    return TRUE;
  CODEREP* var = *crs.begin();

  if (vra->Is_bb_reachable(var, succ) == FALSE)
    return FALSE;

  CODEREP* canon_expr = NULL;
  if (vra->Canonicalize_coderep(cond, var, canon_expr) == FALSE)
    return TRUE;

  Is_True(canon_expr != NULL, ("canon expr is NULL"));
  if ((sr->Opr() == OPR_FALSEBR && pred->Next() != succ) ||
      (sr->Opr() == OPR_TRUEBR && pred->Next() == succ))
    canon_expr = vra->Complement_cr(canon_expr);

  Is_True(canon_expr != NULL && canon_expr->Kind() == CK_OP &&
          (canon_expr->Opnd(0) == var ||
           (canon_expr->Opnd(0)->Kind() == CK_OP &&
            canon_expr->Opnd(0)->Opr() == OPR_ABS)),
          ("invalud canon expr"));
  if (canon_expr == NULL || canon_expr->Kind() != CK_OP ||
      canon_expr->Kid_count() != 2 ||
      (canon_expr->Opnd(1)->Kind() != CK_CONST &&
       canon_expr->Opnd(1)->Kind() != CK_RCONST))
    return TRUE;

#if 0
  if (var->Kind() == CK_IVAR) {
    VSYM_OBJ_REP* vor = Cr_2_vor(var);
    if (vor == NULL)
      return TRUE;

    // TODO: follow vsym U-D
    if (vor->Attr() != ROR_DEF_BY_CHI ||
        vor->Stmt_def() == NULL ||
        vor->Stmt_def()->Opr() != OPR_OPT_CHI)
      return TRUE;

    CODEREP* x = Find_base_pointer_load(var);
    IDTYPE param;
    if (x == NULL || x->Kind() != CK_VAR ||
        Dna()->Is_root() ||
        (param = Dna()->Is_param(x)) == INVALID_VAR_IDX)
      return TRUE;

    VSYM_TRACKER tracker;
    if (tracker.Init(this, &var, NULL, Loc_pool()) != CS_VSYM_UD)
      return TRUE;

    return Is_path_possible_from_caller(param, canon_expr, cs, &tracker, path);
  }

  if (var->Kind() != CK_VAR)  // No cross function value range for IVAR
    return TRUE;

  IDTYPE param = Dna()->Is_param(var);
  if (param != INVALID_VAR_IDX && !Dna()->Is_root()) {
    if (Is_path_possible_from_caller(param, canon_expr, cs, NULL, path) == FALSE)
      return FALSE;
  }
  return TRUE;
#endif

  return Is_path_possible(var, sr->Bb(), canon_expr, cs, NULL, path);
}

// =============================================================================
// VSA::Is_path_possible
// Check if there is a possible path initial frame in call stack to this stmt
// =============================================================================
BOOL
VSA::Is_path_possible(STMTREP* sr, CALL_STACK& cs, const SRCPOS_HANDLE* sp_h) const
{
  VRA* vra = Comp_unit()->Vra();
  // if VRA is unavailable, assume path available
  if (vra == NULL)
    return TRUE;

  PATH_SELECTED path;
  sp_h->Compose_path_selected(Ipsa(), &path);

  // step 1: check if the path from vul spot to here possible
  SRCPOS_CD_ITER iter(sp_h);
  while (!iter.Is_empty()) {
    SRCPOS_CTRLDEP cd = iter.Next();
    CONTEXT_SWITCH ctx(cd.Dna());
    if (cd.Vsa()->Is_path_possible(cd.Pred(), cd.Succ(), cs, path) == FALSE)
      return FALSE;
  }

  // step 2: check if there is a path upper side
  std::vector< std::pair<BB_NODE*, BB_NODE*> > cds;
  vra->Collect_control_dependencies(sr->Bb(), Cfg()->Entry_bb(), cds);
  for (std::vector< std::pair<BB_NODE*, BB_NODE*> >::iterator it = cds.begin();
       it != cds.end(); ++ it) {
    if (Is_path_possible(it->first, it->second, cs, path) == FALSE)
      return FALSE;
  }

  return TRUE;
}

// =============================================================================
// VSA::Get_heapobj_length
// Get the length of the heap_obj
// =============================================================================
CODEREP*
VSA::Get_heapobj_length(HEAP_OBJ_REP* hor, BB_NODE* bb, hash_set<IDTYPE>& visited, CALL_STACK& cs, EVAL_ACTION act) const
{
  if (hor == NULL)
    return NULL;
  switch (hor->Attr()) {
  case ROR_DEF_BY_ALLOCA:
  case ROR_DEF_BY_ALLOC:
    return hor->Heap_obj()->Byte_size();
  case ROR_DEF_BY_COPY:
    return Get_heapobj_length(hor->Prev_ver(), bb, visited, cs, act);
  case ROR_DEF_BY_PHI:
  case ROR_DEF_BY_VARPHI:
  case ROR_DEF_BY_VORPHI:
    {
      PHI_NODE     *phi = hor->Phi_def();
      if (visited.find(phi->Bb()->Id()) != visited.end())
        return NULL;
      visited.insert(phi->Bb()->Id());
      PHI_OPND_ITER phi_opnd_iter(phi);
      CODEREP      *opnd;
      INT32         opnd_idx = 0;
      INT64         sz = 0;
      FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
        if (Is_path_possible(bb, phi, opnd_idx)) {
          HEAP_OBJ_REP *hor_opnd = (HEAP_OBJ_REP*) opnd;
          CODEREP* sz_cr = Get_heapobj_length(hor_opnd, bb, visited, cs, act);
          if (sz_cr != NULL && sz_cr->Kind() == CK_CONST) {
            INT64 nsz = sz_cr->Const_val();
            if (sz == 0 ||
                (act == MIN_VALUE && nsz < sz) ||
                (act == MAX_VALUE && nsz > sz))
              sz = nsz;
          }
        }
        ++ opnd_idx;
      }
      return sz == 0 ? NULL : Htable()->Add_const(MTYPE_I8, sz);
    }
  case ROR_DEF_BY_LDA:
  case ROR_DEF_BY_NULL:
  case ROR_DEF_BY_IPARM:
  case ROR_DEF_BY_CHI:
    // return NULL so far and let Get_object_length() to handle this
    return NULL;
  default:
    return NULL;
  }
}

// =============================================================================
// VSA::Get_object_length
// Get the length of the object represented by cr.
// For symbol, return value is the size of the symbol
// For malloc/alloca, return value is the size of the block
// =============================================================================
std::pair<CODEREP*, RNA_NODE*>
VSA::Get_object_length(CODEREP* cr, STMTREP* sr, hash_set<IDTYPE>& visited, CALL_STACK& cs, EVAL_ACTION act) const
{
  if (cr->Kind() == CK_CONST && cr->Const_val() == 0)
    return make_pair(Htable()->Add_const(MTYPE_I8, 0), (RNA_NODE*)NULL);

  // special case for &(p->field) where field is array
  if (cr->Kind() == CK_OP && cr->Opr() == OPR_ADD &&
      cr->Opnd(1)->Kind() == CK_CONST) {
    TY_IDX ty = cr->Opnd(0)->object_ty();
    TY_IDX obj_ty = TY_IDX_ZERO;
    if ((ty != TY_IDX_ZERO) &&
        (TY_kind(ty) == KIND_POINTER) &&
        (obj_ty = TY_pointed(ty)) != TY_IDX_ZERO &&
        (TY_kind(obj_ty) == KIND_STRUCT)) {
      UINT fld_id = 0;
      FLD_HANDLE fld = FLD_get_from_offset(obj_ty, cr->Opnd(1)->Const_val(), fld_id);
      if (!fld.Is_Null() &&
          TY_kind(FLD_type(fld)) == KIND_ARRAY) {
        CODEREP *size_cr = Comp_unit()->Vra()->New_cr(MTYPE_I8, TY_size(FLD_type(fld)));
        return make_pair(size_cr, (RNA_NODE*)NULL);
      }
    }
  }

  Is_True(sr != NULL, ("sr is NULL"));
  BB_NODE *bb = sr->Bb();
  CODEREP* size_cr = NULL;
  HEAP_OBJ_REP* ho = NULL;
  ho = Cr_2_heap_obj(cr);
  if (ho != NULL)
    size_cr = Get_heapobj_length(ho, bb, visited, cs, act);

  // if this is a ILOAD field with pointer type, return ho size directly
  if (size_cr != NULL && cr->Kind() == CK_IVAR && TY_kind(cr->object_ty()) == KIND_POINTER)
    return make_pair(size_cr, (RNA_NODE*)NULL);

  VSA_ADDRESS_INFO info;
  BOOL ret = Comp_unit()->Analyze_pointer_info(sr, cr, &info, TRUE);
  if (ret == FALSE) {
    return make_pair(size_cr, (RNA_NODE*)NULL);
  }

  INT64 fix_ofst = info.Fix_ofst();
  CODEREP* base = info.Base();
  RNA_NODE *rna = NULL;
  INT64 size = 0;
  if (info.Kind() == PTR_ST) {
    if (base->Afield_id() != 0) {
      TY_IDX st_ty = ST_type(base->Lda_base_st());
      Is_True(TY_kind(st_ty) == KIND_STRUCT || TY_kind(st_ty) == KIND_ARRAY, ("not struct/array type"));
      if (TY_kind(st_ty) == KIND_STRUCT) {
        UINT field_id = 0;
        FLD_HANDLE fld = FLD_get_to_field(st_ty, base->Afield_id(), field_id);
        Is_True(!fld.Is_Null(), ("field id out of bound"));
        size = TY_size(FLD_type(fld));
        fix_ofst -= FLD_ofst(fld);
      }
      else if (TY_kind(st_ty) == KIND_ARRAY) {
        size = TY_size(st_ty);
      }
    }
    if (size == 0) {
      // assertions above, assume whole ST
      size = ST_size(base->Lda_base_st());
    }
  }
  else if (base != NULL) {
    HEAP_OBJ_REP* ho = Cr_2_heap_obj(base);
    if (ho != NULL && size_cr == NULL)
      size_cr = Get_heapobj_length(ho, bb, visited, cs, act);
    if (size_cr == NULL &&
        (info.Kind() == PTR_FORMAL || info.Kind() == PTR_RETURN)) {
      pair<UINT, RNA_NODE*> sz_tmp = Get_object_length_xfa(base, visited, cs, act);
      size = sz_tmp.first;
      rna = sz_tmp.second;
    }
  }

  if (size_cr != NULL && size_cr->Kind() == CK_CONST)
    size = size_cr->Const_val();

  if (size == 0) {
    VRA* vra = Comp_unit()->Vra();
    if (size_cr != NULL && fix_ofst != 0 && vra != NULL) {
      size_cr = vra->New_cr(OPCODE_make_op(OPR_ADD, size_cr->Dtyp(), MTYPE_V),
                            size_cr,
                            vra->New_cr(MTYPE_I8, -fix_ofst),
                            TRUE);
    }
    return make_pair(size_cr, (RNA_NODE*)NULL);
  }

  if (fix_ofst > 0) {
    size -= fix_ofst;
  }
  if (info.Pos_offset().size() == 1 && info.Neg_offset().size() == 0 &&
      Comp_unit()->Vra()) {
    INT64 min, max;
    CODEREP* ofst = info.Pos_offset().front();
    if ((Comp_unit()->Vra()->Get_bounds(ofst, bb, min, max) & VA_UB) == VA_UB) {
      size -= max;
    }
  }
  if (info.Pos_index().size() == 1 && info.Neg_index().size() == 0 &&
      Comp_unit()->Vra()) {
    INT64 min, max;
    CODEREP* index = info.Pos_index().front().first;
    CODEREP* scale = info.Pos_index().front().second;
    Is_True(index != NULL && scale != NULL && scale->Kind() == CK_CONST,
            ("index or scale is wrong"));
    if ((Comp_unit()->Vra()->Get_bounds(index, bb, min, max) & VA_UB) == VA_UB) {
      max *= scale->Const_val();
      size -= max;
    }
  }
  return make_pair(Htable()->Add_const(MTYPE_I8, size), rna);
}

// =============================================================================
// VSA::Get_object_length_xfa
// Get the minimal length of the object represented by param cr/return cr from
// its caller or callee
// =============================================================================
std::pair<UINT, RNA_NODE*>
VSA::Get_object_length_xfa(CODEREP* cr, hash_set<IDTYPE>& visited, CALL_STACK& cs, EVAL_ACTION act) const
{
  Is_True(cr->Kind() == CK_VAR, ("cr is not var"));
  STMTREP* sr = cr->Defstmt();
  if (sr == NULL)  // volatile, zero-version, etc
    return pair<UINT, RNA_NODE*>(0, NULL);
  IDTYPE param;
  CODEREP* retv;
  RNA_NODE* sz_rna = NULL;
  UINT sz = 0;
  if (sr->Opr() == OPR_OPT_CHI &&
      (param = Dna()->Is_param(cr)) != INVALID_VAR_IDX) {
    RNA_NODE* call_site;
    if (cs.empty()) {
      call_site = NULL;
    }
    else {
      call_site = cs.top();
      cs.pop();
    }
    for (INT i = VAR_INIT_ID; i < Dna()->Clby_list()->size(); i++) {
      RNA_NODE* rna = (*Dna()->Clby_list())[i];
      if (param > rna->Arg_cnt())
        continue;
      if (call_site != NULL && call_site != rna)
        continue;
      DNA_NODE *caller = Ipsa()->Get_dna(rna->Caller_idx());
      if (caller == NULL || caller->Non_functional())
        continue;
      if (Ipsa()->Traved(rna))
        continue;
      Ipsa()->Set_trav(rna);
      STMTREP* call_stmt = rna->Callstmt();
      CODEREP* x = rna->Get_arg(param);
      CONTEXT_SWITCH context(caller);
      hash_set<IDTYPE> visited2;
      cs.push(rna);
      INT size = cs.size();
      VSA *caller_vsa = caller->Comp_unit()->Vsa();
      pair<CODEREP*, RNA_NODE*> ret;
      ret = caller_vsa->Get_object_length(x, call_stmt, visited2, cs, act);

      if (ret.first != NULL && ret.first->Kind() == CK_CONST) {
        INT64 nsz = ret.first->Const_val();
        if (sz == 0 ||
            (act == MIN_VALUE && nsz < sz) ||
            (act == MAX_VALUE && nsz > sz)) {
          sz = nsz;
          sz_rna = ret.second ? ret.second : rna;
        }
      }
      Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
      cs.pop();
      if (call_site != NULL)
        break;
    }
    if (call_site != NULL)
      cs.push(call_site);
  }
  else if (OPERATOR_is_call(sr->Opr()) &&
           (retv = Comp_unit()->Find_return_value(sr)) == cr) {
    RNA_NODE *rna = Sr_2_rna(sr);
    if (rna != NULL && !Ipsa()->Traved(rna)) {
      Ipsa()->Set_trav(rna);
      cs.push(rna);
      INT size = cs.size();
      for(CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
          it != rna->Callee_list().end(); it++) {
        DNA_NODE* callee = Ipsa()->Get_dna(it->Callee());
        Is_True(callee != NULL, ("bad callee"));
        if (callee == NULL)
          continue;
        if (callee->Non_functional())
          continue;
        if (callee->Retv_list()->size() == 0)
          continue;
        CONTEXT_SWITCH context(callee);
        hash_set<IDTYPE> visited2;
        VSA* callee_vsa = callee->Comp_unit()->Vsa();
        for (INT i = VAR_INIT_ID; i < callee->Retv_list()->size(); ++i) {
          PDV_NODE* pdv = (*callee->Retv_list())[i];
          if (pdv->Oparam() == 0 && pdv->Stmt()->Rhs() != NULL) {
            std::pair<CODEREP*, RNA_NODE*> ret;
            ret = callee_vsa->Get_object_length(pdv->Stmt()->Rhs(), pdv->Stmt(),
                                                visited2, cs, act);
            if (ret.first != NULL && ret.first->Kind() == CK_CONST) {
              INT64 nsz = ret.first->Const_val();
              if (sz == 0 ||
                  (act == MIN_VALUE && nsz < sz) ||
                  (act == MAX_VALUE && nsz > sz)) {
                sz = nsz;
                sz_rna = ret.second ? ret.second : rna;
              }
            }
          }
        }
      }
      Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
      cs.pop();
    }
  }
  return make_pair(sz, sz_rna);
}

// =============================================================================
// VSA::Get_string_length
// Get the length of the string represented by cr.
// Find the last stid/istore to the object with value 0 and return the offset
// in the stid/istore
// =============================================================================
std::pair<CODEREP*, RNA_NODE*>
VSA::Get_string_length(CODEREP* cr, STMTREP* sr, hash_set<IDTYPE>& visited,
                       CALL_STACK& cs, VSYM_TRACKER* tracker, EVAL_ACTION act) const
{
  CODEREP *ret = NULL;
  UINT ofst = 0;
  RNA_NODE *rna = NULL;

  if (cr->Kind() == CK_LDA) {
    // const string?
    ST* fmt_st = cr->Lda_base_st();
    if (ST_class(fmt_st) == CLASS_CONST &&
        TCON_ty(ST_tcon_val(fmt_st)) == MTYPE_STR) {
      const char * str = Index_to_char_array(TCON_str_idx(ST_tcon_val(fmt_st)));
      return make_pair(Htable()->Add_const(MTYPE_I8, strlen(str)), (RNA_NODE*)NULL);
    }
    // check if there is x[n] = '\0';
    CODEREP* var = NULL;
    AUX_ID lda_aux = cr->Lda_aux_id();
    AUX_ID aux = lda_aux;
    do {
      if (sr->Mu_list()) {
        MU_NODE* mu = sr->Mu_list()->Search_mu_node(aux);
        if (mu != NULL && !mu->OPND()->Is_flag_set(CF_IS_ZERO_VERSION))
          var = mu->OPND();
      }
      if (var == NULL && sr->Chi_list()) {
        CHI_NODE* chi = sr->Chi_list()->Search_chi_node(aux);
        if (chi != NULL && chi->Live() && !chi->OPND()->Is_flag_set(CF_IS_ZERO_VERSION))
          var = chi->OPND();
      }
      if (var != NULL) {
        if (var->Is_flag_set(CF_DEF_BY_PHI)) {
          // TODO: refine DEF_BY_PHI
        }
        else if (var->Is_flag_set(CF_DEF_BY_CHI)) {
          // TODO: refine DEF_BY_CHI
        }
        else {
          STMTREP* def = var->Defstmt();
          if (def != NULL &&
              def->Opr() == OPR_STID &&
              def->Rhs() != NULL &&
              def->Rhs()->Kind() == CK_CONST &&
              def->Rhs()->Const_val() == 0 &&
              (ofst == 0 || def->Lhs()->Offset() < ofst)) {
            ofst = def->Lhs()->Offset();
          }
        }
      }
      aux = Opt_stab()->St_group(aux);
    } while (aux != ILLEGAL_AUX_ID && aux != lda_aux);
    // fall through to check MSTORE/MEMCPY/etc
  }
  if (ofst == 0) {
    HEAP_OBJ_REP* hor = Cr_2_heap_obj(cr);
    VSYM_FLD_REP fld(FLD_K_ID, cr->Field_id(), Cr_ofst(cr));
    VSYM_FLD_REP* p_fld = (tracker != NULL && !tracker->Empty()) ? tracker->Fld_rep()
                                                                 : &fld;
    VSYM_OBJ_REP* vor = NULL;
    if (hor != NULL &&
        (vor = Find_hor_mu_vor(sr, hor, p_fld, cr)) != NULL) {
      if (vor->Attr() == ROR_DEF_BY_ISTORE) {
        STMTREP* def = vor->Stmt_def();
        if (def->Opr() == OPR_ISTORE &&
            def->Rhs() &&
            def->Rhs()->Kind() == CK_CONST &&
            def->Rhs()->Const_val() == 0) {
          if (ofst == 0 ||
              (act == MIN_VALUE && def->Lhs()->Offset() < ofst) ||
              (act == MAX_VALUE && def->Lhs()->Offset() > ofst))
            ofst = def->Lhs()->Offset();
        }
        else if (def->Opr() == OPR_MSTORE) {
          CODEREP* rhs = def->Rhs();
          if (rhs->Kind() == CK_IVAR &&
              rhs->Opr() == OPR_MLOAD) {
            std::pair<CODEREP*, RNA_NODE*> tmp;
            tmp = Get_string_length(rhs->Ilod_base(), def, visited, cs, tracker, act);
            if (tmp.first != NULL)
              return tmp;
            else
              return make_pair(rhs->Mload_size(), (RNA_NODE*)NULL);
          }
          else if (rhs->Kind() == CK_CONST) {
            if (rhs->Const_val() == 0) {
              STMTREP* prev = def->Prev();
              CODEREP* prev_sz = NULL;
              UINT rhs_ofst = 0;
              RNA_NODE *tmp_rna = NULL;
              if (prev != NULL && prev->Opr() == OPR_MSTORE &&
                  prev->Rhs()->Kind() == CK_IVAR && prev->Rhs()->Opr() == OPR_MLOAD &&
                  prev->Lhs()->Istr_base() == def->Lhs()->Istr_base()) {
                std::pair<CODEREP*, RNA_NODE*> tmp;
                tmp = Get_string_length(prev->Rhs()->Ilod_base(), prev, visited, cs, tracker, act);
                if (tmp.first != NULL && tmp.first->Kind() == CK_CONST) {
                  rhs_ofst = tmp.first->Const_val();
                  tmp_rna = tmp.second;
                }
              }
              if (rhs_ofst == 0)
                rhs_ofst = def->Lhs()->Offset();
              if (ofst == 0 ||
                  (act == MIN_VALUE && rhs_ofst < ofst) ||
                  (act == MAX_VALUE && rhs_ofst > ofst)) {
                ofst = rhs_ofst;
                rna = tmp_rna;
              }
            }
            else {
            }
          }
          else if (rhs->Kind() == CK_LDA) {
            return Get_string_length(def->Rhs(), def, visited, cs, tracker, act);
          }
        }
      }
    }
    else if (cr->Kind() == CK_VAR) {
      if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
        // TODO: def by phi
        return Get_string_length_from_object(cr, sr, visited, cs, act);
      }
      else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
        std::pair<UINT, RNA_NODE*> tmp;
        tmp = Get_string_length_xfa(cr, visited, cs, tracker, act);
        ofst = tmp.first;
        rna = tmp.second;
      }
      else {
        STMTREP* def = cr->Defstmt();
        return def ? Get_string_length(def->Rhs(), def, visited, cs, tracker, act)
                   : std::pair<CODEREP*, RNA_NODE*>(NULL, NULL);
      }
    }
    else if (cr->Kind() == CK_IVAR) {
      if (tracker->Init(this, &cr, sr, Loc_pool()) != CS_VSYM_UD)
        return Get_string_length_from_object(cr, sr, visited, cs, act);
      return Get_string_length(cr->Ilod_base(), sr, visited, cs, tracker, act);
    }
  }
  // by default, return object length
  if (ofst > 0)
    return make_pair(Htable()->Add_const(MTYPE_I8, ofst), rna);
  else
    return Get_string_length_from_object(cr, sr, visited, cs, act);
}

// =============================================================================
// VSA::Get_string_length_xfa
// Get the minimal length of the string represented by param cr/return cr from
// its caller or callee
// =============================================================================
std::pair<UINT, RNA_NODE*>
VSA::Get_string_length_xfa(CODEREP* cr, hash_set<IDTYPE>& visited,
                           CALL_STACK& cs, VSYM_TRACKER* tracker, EVAL_ACTION act) const
{
  Is_True(cr->Kind() == CK_VAR, ("cr is not var"));
  STMTREP* sr = cr->Defstmt();
  IDTYPE param;
  CODEREP* retv;
  RNA_NODE* sz_rna = NULL;
  UINT sz = 0;
  if (sr->Opr() == OPR_OPT_CHI &&
      (param = Dna()->Is_param(cr)) != INVALID_VAR_IDX) {
    RNA_NODE* call_site;
    if (cs.empty()) {
      call_site = NULL;
    }
    else {
      call_site = cs.top();
      cs.pop();
    }
    for (INT i = VAR_INIT_ID; i < Dna()->Clby_list()->size(); i++) {
      RNA_NODE* rna = (*Dna()->Clby_list())[i];
      if (param > rna->Arg_cnt())
        continue;
      if (call_site != NULL && call_site != rna)
        continue;
      DNA_NODE *caller = Ipsa()->Get_dna(rna->Caller_idx());
      if (caller == NULL || caller->Non_functional())
        continue;
      if (Ipsa()->Traved(rna))
        continue;
      Ipsa()->Set_trav(rna);
      STMTREP* call_stmt = rna->Callstmt();
      CODEREP* x = rna->Get_arg(param);
      CONTEXT_SWITCH context(caller);
      VSA *caller_vsa = caller->Comp_unit()->Vsa();
      VSYM_OBJ_REP* vor = NULL;
      if (tracker != NULL && !tracker->Empty()) {
        if (x->Kind() == CK_LDA) {
          if (tracker->Size() != 1)
            continue;
          MU_NODE* mu = caller_vsa->Find_stmt_var_mu(call_stmt, x->Lda_base_st(), tracker->Fld_rep());
          if (mu == NULL)
            continue;
          x = mu->OPND();
        }
        else {
          HEAP_OBJ_REP* hor = caller_vsa->Cr_2_heap_obj(x);
          VSYM_FLD_REP* vfr = tracker->Fld_rep();
          if (hor != NULL &&
              (vor = caller_vsa->Find_hor_mu_vor(call_stmt, hor, vfr, x)) != NULL) {
            vor = tracker->Compress_old(caller_vsa, hor, vfr, vor, call_stmt, x);
          }
        }
      }
      hash_set<IDTYPE> visited2;
      cs.push(rna);
      INT size = cs.size();
      std::pair<CODEREP*, RNA_NODE*> ret;
      ret = caller_vsa->Get_string_length(x, call_stmt, visited2, cs, tracker, act);
      if (ret.first != NULL && ret.first->Kind() == CK_CONST) {
        INT64 nsz = ret.first->Const_val();
        if (sz == 0 ||
            (act == MIN_VALUE && nsz < sz) ||
            (act == MAX_VALUE && nsz > sz)) {
          sz = nsz;
          sz_rna = ret.second ? ret.second : rna;
        }
      }
      Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
      cs.pop();
      if (call_site != NULL)
        break;
    }
    if (call_site != NULL)
      cs.push(call_site);
  }
  else if (OPERATOR_is_call(sr->Opr()) &&
           (retv = Comp_unit()->Find_return_value(sr)) == cr) {
    RNA_NODE *rna = Sr_2_rna(sr);
    if (rna != NULL && !Ipsa()->Traved(rna)) {
      Ipsa()->Set_trav(rna);
      cs.push(rna);
      INT size = cs.size();
      for(CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
          it != rna->Callee_list().end(); it++) {
        DNA_NODE* callee = Ipsa()->Get_dna(it->Callee());
        Is_True(callee != NULL, ("bad callee"));
        if (callee == NULL)
          continue;
        if (callee->Non_functional())
          continue;
        if (callee->Retv_list()->size() == 0)
          continue;
        CONTEXT_SWITCH context(callee);
        hash_set<IDTYPE> visited2;
        VSA* callee_vsa = callee->Comp_unit()->Vsa();
        for (INT i = VAR_INIT_ID; i < callee->Retv_list()->size(); ++i) {
          PDV_NODE* pdv = (*callee->Retv_list())[i];
          if (pdv->Oparam() == 0 && pdv->Stmt()->Rhs() != NULL) {
            std::pair<CODEREP*, RNA_NODE*> ret;
            ret = callee_vsa->Get_string_length(pdv->Stmt()->Rhs(), pdv->Stmt(),
                                                         visited2, cs, tracker, act);
            if (ret.first != NULL && ret.first->Kind() == CK_CONST) {
              INT64 nsz = ret.first->Const_val();
              if (sz == 0 ||
                  (act == MIN_VALUE && nsz < sz) ||
                  (act == MAX_VALUE && nsz > sz)) {
                sz = nsz;
                sz_rna = ret.second ? ret.second : rna;
              }
            }
          }
        }
      }
      Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
      cs.pop();
    }
  }
  return make_pair(sz, sz_rna);
}

// =============================================================================
// VSA::Get_string_length_from_object
// Get the string length from object length for strlen(object)
// strlen(obj) = sizeof(obj) - 1 considering the last element is '\0'
// =============================================================================
std::pair<CODEREP*, RNA_NODE*>
VSA::Get_string_length_from_object(CODEREP* cr, STMTREP* sr, hash_set<IDTYPE>& visited,
                                   CALL_STACK& cs, EVAL_ACTION act) const
{
  std::pair<CODEREP*, RNA_NODE*> sz = Get_object_length(cr, sr, visited, cs, act);
  CODEREP *size_cr = sz.first;
  if (size_cr) {
    VRA* vra = Comp_unit()->Vra();
    TYPE_ID mtype = size_cr->Dtyp();
    if (size_cr->Kind() == CK_CONST) {
       INT64 val = size_cr->Const_val();
       if (val <= 0) {
         return sz;
       }
       size_cr = vra->New_cr(mtype, val - 1);
    }
    else {
      size_cr = vra->New_cr(OPCODE_make_op(OPR_SUB, mtype, MTYPE_V),
                            size_cr,
                            vra->New_cr(mtype, 1),
                            TRUE);
    }
    return make_pair(size_cr, sz.second);
  }
  return sz;
}

// =============================================================================
// VSA::Eval_size_info
// Evaluate size expression and handle intrinsic call like strlen/memcpy/etc
// =============================================================================
std::pair<CODEREP*, RNA_NODE*>
VSA::Eval_size_info(CODEREP* cr, STMTREP* sr, hash_set<IDTYPE>& visited, CALL_STACK& cs, EVAL_ACTION act) const
{
  CODEREP* ret = NULL;
  RNA_NODE* sz_rna = NULL;
  switch (cr->Kind()) {
  case CK_LDA:
  case CK_RCONST:
    return make_pair(cr, (RNA_NODE*)NULL);
  case CK_CONST:
    return make_pair(cr, (RNA_NODE*)NULL);
  case CK_VAR:
    {
      if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
        // TODO: def by phi
      }
      else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
        // TODO: param or return value
        STMTREP* def = cr->Defstmt();
        INTRINSIC intrn = OPERATOR_is_call(def->Opr()) ? Get_call_intrinsic(def) : INTRINSIC_NONE;
        BOOL is_wcslen = (def->Opr() == OPR_CALL && strcmp(ST_name(def->St()), "wcslen") == 0);
        if (intrn == INTRN_STRLEN || is_wcslen) {
          if ((ret = def->Const_val()) == NULL) {
            VSYM_TRACKER tracker;
            std::pair<CODEREP*, RNA_NODE*> tmp;
            tmp = Get_string_length(def->Rhs()->Opnd(0)->Ilod_base(), def, visited, cs, &tracker, act);
            ret = tmp.first;
            if (tmp.first != NULL && is_wcslen) {
              // adjust return value for wcslen. Get_string_length() returns offset in bytes
              TY_IDX str_ty = def->Rhs()->Opnd(0)->Ilod_ty();
              Is_True(TY_kind(str_ty) == KIND_POINTER, ("not pointer"));
              if (TY_kind(str_ty) == KIND_POINTER) {
                INT sz = TY_size(TY_pointed(str_ty));
                Is_True(sz == 2 || sz == 4, ("wchar_t is not short or int?"));
                ret = Comp_unit()->Vra()->New_div_cr(ret, sz);
              }
            }
            if (ret) {
              sz_rna = tmp.second;
              def->Set_const_val(ret); // cache the value
            }
          }
          return make_pair(ret ? ret : cr, sz_rna);
        }
        if (def->Opr() == OPR_OPT_CHI || OPERATOR_is_call(def->Opr())) {
          std::pair<UINT, RNA_NODE*> tmp;
          tmp = Eval_size_info_xfa(cr, NULL, visited, cs, act);
          if (tmp.first > 0)
            return make_pair(Htable()->Add_const(MTYPE_I8, tmp.first), tmp.second);
        }
      }
      else {
        STMTREP* def = cr->Defstmt();
        Is_True(def != NULL || cr->Is_var_volatile(), ("null defstmt"));
        if (def)
          return Eval_size_info(def->Rhs(), def, visited, cs, act);
      }
    }
    return make_pair(cr, (RNA_NODE*)NULL);
  case CK_IVAR:
    {
      VSYM_OBJ_REP* vor = Cr_2_vor(cr);
      if (vor != NULL)
        return Eval_size_info(cr, vor, sr, visited, cs, act);
    }
    return make_pair(cr, (RNA_NODE*)NULL);
  case CK_OP:
    {
      if (cr->Kid_count() == 1) {
        return Eval_size_info(cr->Opnd(0), sr, visited, cs, act);
      }
      else if (cr->Kid_count() == 2) {
        std::pair<CODEREP*, RNA_NODE*> tmp0, tmp1;
        tmp0 = Eval_size_info(cr->Opnd(0), sr, visited, cs, act);
        tmp1 = Eval_size_info(cr->Opnd(1), sr, visited, cs, act);
        if (tmp0.first == NULL ||
            tmp1.first == NULL ||
            tmp0.second != tmp1.second)
          return std::pair<CODEREP*, RNA_NODE*>(cr, (RNA_NODE*)NULL);
        CODEREP *op0 = tmp0.first;
        CODEREP *op1 = tmp1.first;
        if (op0->Kind() == CK_CONST && op1->Kind() == CK_CONST) {
          INT64 val;
          INT64 v0 = op0->Const_val();
          INT64 v1 = op1->Const_val();
          switch (cr->Opr()) {
          case OPR_ADD:  val = v0 + v1; break;
          case OPR_SUB:  val = v0 - v1; break;
          case OPR_MPY:  val = v0 * v1; break;
          case OPR_DIV:  val = v0 / v1; break;
          case OPR_REM:  val = v0 % v1; break;
          default:
            Is_Trace(Tracing(), (TFile, "TODO: handle %s\n",
                                        OPERATOR_name(cr->Opr()) + 4));
            return make_pair(cr, (RNA_NODE*)NULL);
          }
          return make_pair(Htable()->Add_const(MTYPE_I8, val), tmp0.second);
        }
      }
    }
    return make_pair(cr, (RNA_NODE*)NULL);
  default:
    Is_True(FALSE, ("unknown cr kind"));
    return make_pair(cr, (RNA_NODE*)NULL);
  }
}

// =============================================================================
// VSA::Eval_size_info
// Evaluate size expression with vsym info
// =============================================================================
std::pair<CODEREP*, RNA_NODE*>
VSA::Eval_size_info(CODEREP* cr, VSYM_OBJ_REP* vor, STMTREP* sr,
                    hash_set<IDTYPE>& visited, CALL_STACK& cs, EVAL_ACTION act) const
{
  if (vor == NULL)
    return make_pair(cr, (RNA_NODE*)NULL);
  switch (vor->Attr()) {
  case ROR_DEF_BY_CHI:
  case ROR_DEF_BY_PHI:
  case ROR_DEF_BY_COPY:
    // TODO later
    break;
  case ROR_DEF_BY_ISTORE:
    {
      STMTREP* def = vor->Stmt_def();
      if (def == NULL)
        break;
      return Eval_size_info(def->Rhs(), def, visited, cs, act);
    }
    break;
  default:
    break;
  }
  return make_pair(cr, (RNA_NODE*)NULL);
}

// =============================================================================
// VSA::Eval_size_info_xfa
// Evaluate size expression cross functions
// =============================================================================
std::pair<UINT, RNA_NODE*>
VSA::Eval_size_info_xfa(CODEREP* cr, VSYM_OBJ_REP* vor, hash_set<IDTYPE>& visited,
                        CALL_STACK& cs, EVAL_ACTION act) const
{
  Is_True(cr->Kind() == CK_VAR, ("cr is not var"));
  STMTREP* sr = cr->Defstmt();
  IDTYPE param;
  CODEREP* retv;
  RNA_NODE* sz_rna = NULL;
  UINT sz = 0;
  if (sr->Opr() == OPR_OPT_CHI &&
      (param = Dna()->Is_param(cr)) != INVALID_VAR_IDX) {
    RNA_NODE* call_site;
    if (cs.empty()) {
      call_site = NULL;
    }
    else {
      call_site = cs.top();
      cs.pop();
    }
    for (INT i = VAR_INIT_ID; i < Dna()->Clby_list()->size(); i++) {
      RNA_NODE* rna = (*Dna()->Clby_list())[i];
      if (param > rna->Arg_cnt())
        continue;
      if (call_site != NULL && call_site != rna)
        continue;
      DNA_NODE *caller = Ipsa()->Get_dna(rna->Caller_idx());
      if (caller == NULL || caller->Non_functional())
        continue;
      if (Ipsa()->Traved(rna))
        continue;
      Ipsa()->Set_trav(rna);
      STMTREP* call_stmt = rna->Callstmt();
      CODEREP* x = rna->Get_arg(param);
      CONTEXT_SWITCH context(caller);
      hash_set<IDTYPE> visited2;
      cs.push(rna);
      INT size = cs.size();
      VSA *caller_vsa = caller->Comp_unit()->Vsa();
      if (vor != NULL)
        vor = NULL; // vor = blahblah
      std::pair<CODEREP*, RNA_NODE*> ret;
      ret = caller_vsa->Eval_size_info(x, vor, call_stmt, visited2, cs, act);
      if (ret.first != NULL && ret.first->Kind() == CK_CONST) {
        INT64 nsz = ret.first->Const_val();
        if (sz == 0 ||
            (act == MIN_VALUE && nsz < sz) ||
            (act == MAX_VALUE && nsz > sz)) {
          sz = nsz;
          sz_rna = ret.second ? ret.second : rna;
        }
      }
      Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
      cs.pop();
      if (call_site != NULL)
        break;
    }
    if (call_site != NULL)
      cs.push(call_site);
  }
  else if (OPERATOR_is_call(sr->Opr()) &&
           (retv = Comp_unit()->Find_return_value(sr)) == cr) {
    RNA_NODE *rna = Sr_2_rna(sr);
    if (rna != NULL && !Ipsa()->Traved(rna)) {
      Ipsa()->Set_trav(rna);
      cs.push(rna);
      INT size = cs.size();
      for (CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
           it != rna->Callee_list().end(); it++) {
        DNA_NODE* callee = Ipsa()->Get_dna(it->Callee());
        Is_True(callee != NULL, ("bad callee"));
        if (callee == NULL)
          continue;
        if (callee->Non_functional())
          continue;
        if (callee->Retv_list()->size() == 0)
          continue;
        CONTEXT_SWITCH context(callee);
        hash_set<IDTYPE> visited2;
        VSA* callee_vsa = callee->Comp_unit()->Vsa();
        for (INT i = VAR_INIT_ID; i < callee->Retv_list()->size(); ++i) {
          PDV_NODE* pdv = (*callee->Retv_list())[i];
          if (pdv->Oparam() == 0 && pdv->Stmt()->Rhs() != NULL) {
            if (vor != NULL)
              vor = NULL; //vor = blah;
            std::pair<CODEREP*, RNA_NODE*> ret;
            ret = callee_vsa->Eval_size_info(pdv->Stmt()->Rhs(), vor, pdv->Stmt(),
                                                      visited2, cs, act);
            if (ret.first != NULL && ret.first->Kind() == CK_CONST) {
              INT64 nsz = ret.first->Const_val();
              if (sz == 0 ||
                  (act == MIN_VALUE && nsz < sz) ||
                  (act == MAX_VALUE && nsz > sz)) {
                sz = nsz;
                sz_rna = ret.second ? ret.second : rna;
              }
            }
          }
        }
      }
      Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
      cs.pop();
    }
  }
  return make_pair(sz, sz_rna);
}

// =============================================================================
// VSA::Is_same_symbol(CODEREP* cr1, CODEREP* cr2) const
// return TRUE if two crs operate on the same symbol
// =============================================================================
BOOL
VSA::Is_same_symbol(CODEREP* cr1, CODEREP* cr2) const
{
  if (cr1 == cr2)
    return TRUE;

  // check cr1
  AUX_ID aux1 = ILLEGAL_AUX_ID;
  ST*    st1  = NULL;
  if (cr1->Kind() == CK_VAR) {
    aux1 = cr1->Aux_id();
    st1 = _opt_stab->Aux_stab_entry(aux1)->St();
  }
  else if (cr1->Kind() == CK_LDA) {
    aux1 = cr1->Lda_aux_id();
    st1 = cr1->Lda_base_st();
  }
  else
    return FALSE;

  // check cr2
  AUX_ID aux2 = ILLEGAL_AUX_ID;
  ST*    st2  = NULL;
  if (cr2->Kind() == CK_VAR) {
    aux2 = cr2->Aux_id();
    st2 = _opt_stab->Aux_stab_entry(aux2)->St();
  }
  else if (cr2->Kind() == CK_LDA) {
    aux2 = cr2->Lda_aux_id();
    st2 = cr2->Lda_base_st();
  }
  else
    return FALSE;

  if (aux1 == aux2 ||
      (st1 != NULL && st1 == st2))
    return TRUE;
  else
    return FALSE;
}

// =============================================================================
// VSA::Is_strlen_of(CODEREP* str, CODEREP* len)
// return TRUE if the len = strlen(str) or
//                    len = wcslen(str) * sizeof(wchar_t)
// =============================================================================
BOOL
VSA::Is_strlen_of(CODEREP* str, CODEREP* len, BOOL is_wchar) const
{
  if (is_wchar == FALSE &&
      len->Kind() == CK_OP && len->Opr() == OPR_MPY &&
      len->Opnd(0)->Kind() == CK_VAR &&
      len->Opnd(1)->Kind() == CK_CONST &&
      (len->Opnd(1)->Const_val() == 2 ||
       len->Opnd(1)->Const_val() == 4)) {
    is_wchar = TRUE;
    len = len->Opnd(0);
  }
  else if (len->Kind() != CK_VAR) {
    return FALSE;
  }

  Is_True(len->Kind() == CK_VAR, ("bad kind"));
  if (len->Is_flag_set(CF_DEF_BY_PHI)) {
    return FALSE;  // TODO: check phi
  }

  STMTREP *def = len->Defstmt();
  if (def == NULL)
    return FALSE;

  if (len->Is_flag_set(CF_DEF_BY_CHI)) {
    if (!OPERATOR_is_call(def->Opr()))
      return FALSE; // TODO: check caller?
    if (is_wchar &&
        def->Opr() == OPR_CALL &&
        strcmp(ST_name(def->St()), "wcslen") == 0)
      return TRUE;
    if (!is_wchar &&
        Get_call_intrinsic(def) == INTRN_STRLEN)
      return TRUE;
    // TODO: check callee?
  }
  else {
    STMTREP *def = len->Defstmt();
    if (def != NULL)
      return Is_strlen_of(str, def->Rhs(), is_wchar);
  }
  return FALSE;
}

// =============================================================================
// VSA::Is_var_tainted(CODEREP* cr)
// return TRUE if the VAR is tainted
// =============================================================================
BOOL
VSA::Is_var_tainted(CODEREP* cr) const
{
  Is_True(cr->Kind() == CK_VAR, ("not var"));
  if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
    // TODO: later
  }
  else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP* def = cr->Defstmt();
    if (def->Opr() == OPR_OPT_CHI) {
      // TODO: later check caller???
    }
    else if (OPERATOR_is_call(def->Opr())) {
      IDTYPE which_arg = 0;
      // TODO: later check callee???
      return Callee_may_taint_arg(def, cr, &which_arg);
    }
  }
  else {
    STMTREP* def = cr->Defstmt();
    if (def != NULL)
      return Is_value_tainted(def->Rhs());
  }
  return FALSE;
}

// =============================================================================
// VSA::Is_vor_tainted(CODEREP* cr, VSYM_OBJ_REP* vor)
// return TRUE if the VSYM_OBJ_REP is tainted
// =============================================================================
BOOL
VSA::Is_vor_tainted(CODEREP* cr, VSYM_OBJ_REP* vor) const
{
  Is_True(cr != NULL && vor != NULL, ("bad cr or vor"));
  switch (vor->Attr()) {
  case ROR_DEF_BY_ISTORE:
    {
      STMTREP *def = vor->Stmt_def();
      if (def != NULL)
        return Is_value_tainted(def->Rhs());
    }
    return FALSE;
  case ROR_DEF_BY_CHI:
    // TODO
    return FALSE;
  default:
    return FALSE;
  }
}

// =============================================================================
// VSA::Is_value_tainted(CODEREP* cr)
// return TRUE if the value represented by the cr is tainted
// =============================================================================
BOOL
VSA::Is_value_tainted(CODEREP* cr) const
{
  switch (cr->Kind()) {
  case CK_CONST:
  case CK_RCONST:
  case CK_LDA:
    return FALSE;
  case CK_VAR:
    return Is_var_tainted(cr);
  case CK_IVAR:
    {
      VSYM_OBJ_REP *vor = Cr_2_vor(cr);
      if (vor != NULL && Is_vor_tainted(cr, vor))
        return TRUE;
      // if base's tainted, the ivar is tainted
      return Is_value_tainted(cr->Ilod_base());
    }
  case CK_OP:
    for (INT i = 0; i < cr->Kid_count(); ++i) {
      if (Is_value_tainted(cr->Opnd(i)))
        return TRUE;
    }
    return FALSE;
  default:
    return FALSE;
  }
}

// =============================================================================
// VSA::Find_def_cr_rec(CODEREP* cr, DNA_NODE* dna, const PATH_SELECTED& paths,
//                      CODEREP*& ret, IDTYPE& bb, hash_set<IDTYPE>& visited)
// Find def of cr in given dna and path selected
// return TRUE if the cr is found. cr and bb are returned by output param
// =============================================================================
BOOL
VSA::Find_def_cr_rec(CODEREP* cr, DNA_NODE* dna, const PATH_SELECTED& paths,
                     CODEREP*& ret, DNA_NODE*& def_dna, IDTYPE& bb, hash_set<IDTYPE>& visited) const
{
  switch (cr->Kind()) {
  case CK_LDA:
  case CK_RCONST:
  case CK_CONST:
    return TRUE;

  case CK_VAR:
    if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
      // TODO: output param or return value
      STMTREP* def = cr->Defstmt();
      IDTYPE param;
      if (def->Opr() == OPR_OPT_CHI &&
          Dna()->Parm_list()->size() > 1 &&
          (param = Dna()->Is_param(cr)) != INVALID_VAR_IDX) {
        for (INT i = VAR_INIT_ID; i < Dna()->Clby_list()->size(); i++) {
          RNA_NODE* rna = (*Dna()->Clby_list())[i];
          if (param > rna->Arg_cnt())
            continue;
          DNA_NODE *caller = Ipsa()->Get_dna(rna->Caller_idx());
          if (caller == NULL || caller->Non_functional())
            continue;
          if (!paths.Selected(rna->Rna_idx()))
            continue;
          if (visited.find(rna->Rna_idx()) != visited.end())
            continue;
          visited.insert(rna->Rna_idx());
          CODEREP* x = rna->Get_arg(param);
          bb = rna->Callstmt()->Bb()->Id();
          ret = x;
          def_dna = caller;
          if (caller == dna) {
            return TRUE;
          }
          CONTEXT_SWITCH context(caller);
          return caller->Comp_unit()->Vsa()->Find_def_cr_rec(x, dna, paths,
                                                             ret, def_dna, bb, visited);
        }
      }
      if (def->Opr() == OPR_INTRINSIC_CALL) {
        std::pair<CODEREP*, RNA_NODE*> tmp;
        CALL_STACK cs;
        tmp = Eval_size_info(cr, def, visited, cs, MAX_VALUE);
        ret = tmp.first;
        if (ret != cr)
          return TRUE;
        return FALSE;
      }
    }
    else if (!cr->Is_flag_set(CF_DEF_BY_PHI) &&
             !cr->Is_flag_set(CF_IS_ZERO_VERSION) &&
             !cr->Is_var_volatile()) {
      STMTREP* def = cr->Defstmt();
      Is_True(def != NULL, ("null defstmt"));
      if (def)
        return Find_def_cr_rec(def->Rhs(), dna, paths, ret, def_dna, bb, visited);
    }
    // TODO: def-by-phi
    return FALSE;

  case CK_IVAR:      // TODO: IVAR
    return FALSE;
  case CK_OP:        // TODO: OP
    if ((cr->Opr() == OPR_ADD || cr->Opr() == OPR_SUB) &&
        (cr->Opnd(0)->Kind() == CK_CONST || cr->Opnd(1)->Kind() == CK_CONST)) {
      CODEREP* cst = cr->Opnd(0)->Kind() == CK_CONST ? cr->Opnd(0) : cr->Opnd(1);
      CODEREP* var = cr->Opnd(0)->Kind() == CK_CONST ? cr->Opnd(1) : cr->Opnd(0);
      if (Find_def_cr_rec(var, dna, paths, ret, def_dna, bb, visited)) {
        FOLD_CONTEXT fctx(def_dna->Comp_unit()->Htable());
        ret = def_dna->Comp_unit()->Vra()->New_cr(OPCODE_make_op(cr->Opr(), MTYPE_I8, MTYPE_V),
                                         ret, cst, TRUE);
        return TRUE;
      }
    }
    else if (cr->Opr() == OPR_CVT)
      return Find_def_cr_rec(cr->Opnd(0), dna, paths, ret, def_dna, bb, visited);
    return FALSE;
  default:
    Is_True(FALSE, ("unknown cr kind"));
    return FALSE;
  }
}

// =============================================================================
// VSA::Check_mload_oob
// Check if the mload out-of-bound:
// if cr is a struct field, check if mload size smaller than field size.
// otherwise check if mload size smaller than object size.
// =============================================================================
void
VSA::Check_mload_oob(CODEREP* cr, STMTREP* sr)
{
  Is_True(cr->Kind() == CK_IVAR && cr->Opr() == OPR_MLOAD, ("cr is not mload"));
  CODEREP *len_orig = (cr == sr->Lhs()) ? cr->Mstore_size() : cr->Mload_size();
  CODEREP *base = (cr == sr->Lhs()) ? cr->Istr_base()   : cr->Ilod_base();
  CODEREP *base_sz = NULL;
  VRA_RESULT res = VA_NO;

  // eval len at first
  std::pair<CODEREP*, RNA_NODE*> len_ret, base_ret;
  len_ret = Eval_size_info(len_orig, sr, MAX_VALUE);
  CODEREP *len = len_ret.first;

  // check for type info
  if (cr->I_field_id() > 0) {
    TY_IDX ty = cr->Ilod_ty();
    Is_True(TY_kind(ty) == KIND_POINTER &&
            TY_kind(TY_pointed(ty)) == KIND_STRUCT, ("not pointer to struct"));
    ty = TY_pointed(ty);
    UINT field_id = 0;
    FLD_HANDLE fld = FLD_get_to_field(ty, cr->I_field_id(), field_id);
    UINT32 size = TY_size(FLD_type(fld));
    base_sz = Htable()->Add_const(MTYPE_I8, size);
    PATH_SELECTED path;
    res = (base_sz->Kind() != CK_CONST && base_sz->Kind() != CK_RCONST)
            ? Comp_unit()->Vra()->Compare_cr<OPR_LT>(base_sz, sr->Bb(), len, path)
            : Comp_unit()->Vra()->Compare_cr<OPR_GT>(len, sr->Bb(), base_sz, path);
  }

  // check for heap object
  if (res != VA_YES && res != VA_POSSIBLE) {
    base_ret = Get_object_length(base, sr, MIN_VALUE);
    base_sz = base_ret.first;
    if (base_sz != NULL && Comp_unit()->Vra()) {
      PATH_SELECTED path;
      res = (base_sz->Kind() != CK_CONST && base_sz->Kind() != CK_RCONST)
              ? Comp_unit()->Vra()->Compare_cr<OPR_LT>(base_sz, sr->Bb(), len, path)
              : Comp_unit()->Vra()->Compare_cr<OPR_GT>(len, sr->Bb(), base_sz, path);
    }
  }

  BOOL is_tainted = FALSE;
  if (res == VA_UNKNOWN && cr != sr->Lhs()) { // only check rhs for tainted AOB
    CODEREP* len_ub = NULL;
    if (Comp_unit()->Vra()->Get_ub(len_orig, sr->Bb(), len_ub) == FALSE) {
      // len doesn't have a upper bound, check if len is tainted or treat as 'tainted'
      CODEREP *base_base = Find_ilod_base(base);
      if (base_base == NULL)
        base_base = base;
      CODEREP *len_base = len->Kind() == CK_IVAR
                            ? Find_ilod_base(len->Ilod_base()) : len;
      if (len_base == NULL)
        len_base = len;
      is_tainted = Is_value_tainted(len) ||
                   (VSA_Src_Tainted && Is_same_symbol(base_base, len_base));
    }
  }

  if ((len_ret.second == base_ret.second ||
       len_ret.second == NULL || base_ret.second == NULL) &&
      (res == VA_YES || res == VA_POSSIBLE || is_tainted)) {
    SRCPOS_HANDLE srcpos_h(base, sr, Dna(), Loc_pool(), this);
#if 0
    const char* dest_desc = srcpos_h.Find_cr_stname(base, sr, Dna());
    if (dest_desc == NULL) dest_desc = "<destination>";
    const char* dest_sz_desc = base_sz ? srcpos_h.Find_cr_stname(base_sz, sr, Dna())
                                       : "<unknown>";;
    if (dest_sz_desc == NULL) dest_sz_desc = "<dest length>";
    const char* copy_sz_desc = srcpos_h.Find_cr_stname(len, sr, Dna());
    if (copy_sz_desc == NULL) copy_sz_desc = "<copy length>";
    srcpos_h.Add_message("memcpy() copies %s%s bytes to %s which is %s bytes",
                         copy_sz_desc, is_tainted ? " (tainted)" : "",
                         dest_desc, dest_sz_desc);
#endif
    RNA_NODE* sz_rna = base_ret.second ? base_ret.second : len_ret.second;
    if (sz_rna) {
      srcpos_h.Append_data(sz_rna->Callstmt(),
                           Ipsa()->Get_dna(sz_rna->Caller_idx()),
                           PATHINFO_DNA_CALLSITE);
    }
    srcpos_h.Set_msgid("AOB.1");
    Report_vsa_error(base, (char*)NULL, AOB,
                     res == VA_YES ? IC_DEFINITELY : IC_MAYBE, &srcpos_h);
  }
}

// =============================================================================
//
//  VSA::Udt_stmt - VSA analysis at statement level, currently include the UIV
//  analysis and preparation phase for DBF analysis
//
// =============================================================================
void
VSA::Udt_stmt(STMTREP *stmt, BB_NODE *bb, MEM_POOL *def_bbs_pool)
{
  if (!(VSA_Npd() && VSA_Ral()))
    return;

  CODEREP *x = NULL;
  if (stmt->Op() == OPC_XPRAGMA && // should move the inline body check here
      WN_pragma(stmt->Orig_wn()) == WN_PRAGMA_COPYIN_BOUND)
    return;

  if (VSA_Npd() && !PU_java_lang(Get_Current_PU()) && stmt->Op() == OPC_ASM_STMT) {
    Classify_asm_npd_error(stmt, bb);
    return;
  }

  if (stmt->Rhs()) {
    if (!stmt->Is_identity_asgn())  // suppress warning
      x = Udt_cr(stmt->Rhs(), bb, stmt);
  }

  switch (stmt->Opr()) { 
  case OPR_STID:
    {
      //stmt->Lhs()->Set_value_def();
      //if (stmt->Rhs()->Kind() == CK_CONST && stmt->Rhs()->Const_val() == 0) {
      //stmt->Lhs()->Set_value_invalid_addr();
      //}
    }
    break;
  case OPR_ICALL:
    {
      if (!(VSA_Npd() && !PU_java_lang(Get_Current_PU())))
        break;
      // check if icall target is NULL
      CODEREP *rhs = stmt->Rhs();
      Is_True(rhs->Kind() == CK_OP && rhs->Kid_count() > 0,
              ("invalid icall stmtrep"));
      x = rhs->Opnd(rhs->Kid_count() - 1);
      // prepare srcpos info
      SRCPOS_HANDLE srcpos_h(x, stmt, _cu->Dna(), Loc_pool());

      if (IPSA_insession())
        Ipsa()->Begin_trav_counter();
      hash_set<IDTYPE> visited_bb;
      CALL_STACK cs;
      Classify_vul_error(x, bb, stmt,
                         cs, &srcpos_h, (ILODSTORBASE)(FOR_ILOD_BASE | FOR_NPD), visited_bb);
      Is_True(cs.empty(), ("call stack is not empty"));
      if (IPSA_insession())
        Ipsa()->End_trav_counter();
    }
    break;
  case OPR_CALL:
  {
    if(!(VSA_Npd() && !PU_java_lang(Get_Current_PU())))
      break;
    RNA_NODE * rna = _cu->Dna()->Get_callsite_rna(stmt);
    // if rna has functional callee, just break
    if (rna->Is_flag_set(RNA_HAS_FUNCTIONAL))
      break;
    //if(!Ipsa()->Is_jni_call(rna))
    //  break;
    for (int i=VAR_INIT_ID ; i< rna->Arg_list()->size(); i++) {
      VAR_NODE *arg_node = rna->Arg_list()->at(i);
      CODEREP *x = arg_node->Cr();
      // if param is ILOAD or ISTORED in callee and x is an CK_OP,
      // which means the pointer is adjusted and the check of NULL
      // in callee doesn't work.
      if(arg_node->Flags() & (REF_ILOAD | REF_ISTORE) &&
         ((x->Kind() == CK_OP &&
           (x->Opr() == OPR_ADD || x->Opr() == OPR_SUB)) ||
          x->Kind() == CK_CONST ||
          x->Kind() == CK_VAR ||
          x->Kind() == CK_IVAR)) {
        SRCPOS_HANDLE srcpos_h(x, stmt, _cu->Dna(), Loc_pool());
        if (IPSA_insession())
          Ipsa()->Begin_trav_counter();
        hash_set<IDTYPE> visited_bb;
        CALL_STACK cs;
        Classify_vul_error(x, bb, stmt,
                           cs, &srcpos_h, (ILODSTORBASE)(FOR_ILOD_BASE | FOR_NPD), visited_bb);
        Is_True(cs.empty(), ("call stack is not empty"));
        if (IPSA_insession())
          Ipsa()->End_trav_counter();
      }
    }
    break;
  }
  case OPR_ISTORE:
  case OPR_ISTBITS:
    if (!(VSA_Npd() && !PU_java_lang(Get_Current_PU())))
      break;
    {
      x = Udt_cr(stmt->Lhs()->Istr_base(), bb, stmt);
      if (x == NULL)
        x = stmt->Lhs()->Istr_base();

      SRCPOS_HANDLE srcpos_h(x, stmt, Dna(), Loc_pool(), this);

      // TODO: some prop work is still needed
      hash_set<IDTYPE> visited_bb;
      if (IPSA_insession())
        Ipsa()->Begin_trav_counter();
      CALL_STACK cs;
      Classify_vul_error(x, bb, stmt,
                         cs, &srcpos_h, (ILODSTORBASE)(FOR_ISTOR_BASE | FOR_NPD), visited_bb);
      Is_True(cs.empty(), ("call stack is not empty"));
      if (IPSA_insession())
        Ipsa()->End_trav_counter();

#if 0
// To be removed later
      if (x != NULL && (x->Kind() == CK_VAR)) {
        // non CK_var, leave that to AOB to handle
        
        Is_Trace(Tracing(), (TFile, "Check NPD of Istore:: "));
        Is_Trace_cmd(Tracing(), x->Print(2, TFile));
        Is_Trace(Tracing(), (TFile, "\n"));
        
        if ((x->Kind() == CK_VAR) && (TY_kind(x->Lod_ty() == KIND_POINTER))) {
          VAL_RANGE_RESULT rr = VAL_INR;
          if (x->Value_invalid_addr())
            rr = VAL_OOR;
          else if (x->Value_maydef() || !(x->Value_checked()))
            rr = VAL_May_OOR;
          if (rr != VAL_INR) {          
            Classify_npd_error(x, bb, srcpos_h, VAL_May_OOR, FALSE);
          }
        }

        else if (!Npd_ptr_arith(x, stmt, bb, srcpos_h)) {
          // TODO Array or Iload beneath ISTORE may be too much for now to speculate
          if (!(((x->Kind() == CK_OP) && (x->Opr() == OPR_ARRAY)) ||
                (x->Kind() == CK_IVAR))) {
            VAL_RANGE_RESULT rr;
            BOOL is_vra = FALSE;
            rr = Check_expr_zero(x, bb, is_vra);
            if (rr != VAL_INR) {
              if (srcpos_h == NULL) {
                srcpos_h = CXX_NEW(SRCPOS_HANDLE(x, stmt, Dna(), Loc_pool()), Loc_pool());
              }
              else {
                srcpos_h->Append_data(stmt, x, Dna(), PATHINFO_COPY);
              }
              Classify_npd_error(x, bb, srcpos_h, rr, is_vra);
            }          
          }
        }
      }
      else {
        if (stmt->Lhs()->Istr_base()->Kind() == CK_CONST) {
          if (stmt->Lhs()->Value_invalid_addr())
            // NO handle to symbol name here
            // Classify_npd_error(stmt->Lhs()->Istr_base(), bb, stmt->Linenum(), VAL_OOR);
            Is_Trace(Tracing(), (TFile, "NPD of Istore of const (0 likely) %d\n", 0));
        }
      }
#endif
    }  
    break;
  
 case OPR_MSTORE:
    {
      FmtAssert(TRUE, (" MSTORE not handled in Udt_cr"));
    }
    break;
  default:
    break;
  }
}

// =============================================================================
//
//  VSA::Propagate_vardef - VSA analysis at statement level
//
// =============================================================================
void
VSA::Propagate_vardef(STMTREP *stmt, BB_NODE *bb)
{
  CODEREP *x, *lhs;
  if (stmt->Op() == OPC_XPRAGMA && // should move the inline body check here
      WN_pragma(stmt->Orig_wn()) == WN_PRAGMA_COPYIN_BOUND)
    return;

  UINT32 flags = 0;
  if (stmt->Rhs()) {
    x = Propagate_vardef(stmt->Rhs(), bb, stmt, &flags);
    if (VSA_Prop_Vardef && x != NULL && x != stmt->Rhs()) {
      Is_Trace(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
               (TFile, "PROP VARDEF: replace rhs of sr%d at line %d:\n",
                       stmt->Stmtrep_id(), Srcpos_To_Line(stmt->Linenum())));
      Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                   stmt->Print(TFile));
      Is_Trace(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
               (TFile, " with cr%d:\n", x->Coderep_id()));
      Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                   x->Print(2, TFile));
      x->IncUsecnt();
      stmt->Set_rhs(x);
    }
    if (x == NULL)
      x = stmt->Rhs();
  }

  switch (stmt->Opr()) { 
  case OPR_STID:
    {
      if (x) {
        switch (x->Kind()) {
        case CK_CONST:
          stmt->Lhs()->Set_value_def();
          if (Is_invalid_address(x->Const_val())) {
            stmt->Lhs()->Set_value_invalid_addr();
          }
          break;
        case CK_VAR: 
          stmt->Lhs()->Set_value_flags(x->Value_flags());
#if 0
          if (TY_kind(x->object_ty()) == KIND_POINTER) {
            AUX_STAB_ENTRY *aux = _opt_stab->Aux_stab_entry(stmt->Lhs()->Aux_id());
            STMTREP* next = stmt->Next();
            // assign to return value or global var
            if ((aux->Is_preg() && Is_Return_Preg(aux->St_ofst()) &&
                 next != NULL && next->Opr() == OPR_RETURN) ||
                (aux->Is_global())) {
              stmt->Lhs()->Set_value_escaped();
              hash_set<IDTYPE> visited;
              Mark_var_escaped(x, visited);
            }
          }
#endif
          if (VSA_Prop_Vardef && !x->Is_var_volatile()) {
            Is_True(x == stmt->Rhs(), ("TODO: x != rhs"));
            Is_Trace(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                     (TFile, "PROP VARDEF: push cr%d sym%dv%d for sr%d at line %d:\n",
                             x->Coderep_id(), x->Aux_id(), x->Version(),
                             stmt->Stmtrep_id(), Srcpos_To_Line(stmt->Linenum())));
            Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                         stmt->Print(TFile));
            Opt_stab()->Push_coderep(x->Aux_id(), stmt->Lhs());
          }
          break;
        case CK_OP:
          // copy rhs's flags to lhs
          stmt->Lhs()->Set_value_flags(flags);
          break;
        default:
          stmt->Lhs()->Set_value_def();
          break;
        } // switch
      } // if (x)
    }
    break;
  case OPR_ISTORE:
  case OPR_ISTBITS:
    lhs = Propagate_vardef(stmt->Lhs()->Istr_base(), bb, stmt, NULL);
    if (lhs != NULL && lhs->Kind() == CK_VAR &&
        x != NULL && x->Kind() == CK_VAR &&
        TY_kind(x->object_ty()) == KIND_POINTER) {
      AUX_STAB_ENTRY *aux = _opt_stab->Aux_stab_entry(lhs->Aux_id());
      // assign to output param
      if (aux->St() && ST_sclass(aux->St()) == SCLASS_FORMAL) {
        hash_set<IDTYPE> visited;
        Mark_var_escaped(x, visited);
      }
    }
    if (VSA_Prop_Vardef && lhs != NULL && lhs != stmt->Lhs()->Istr_base()) {
      Is_Trace(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
               (TFile, "PROP VARDEF: replace istr base cr%d of sr%d at line %d:\n",
                       stmt->Lhs()->Istr_base()->Coderep_id(),
                       stmt->Stmtrep_id(), Srcpos_To_Line(stmt->Linenum())));
      Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                   stmt->Print(TFile));
      Is_Trace(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
               (TFile, " with cr%d:\n", lhs->Coderep_id()));
      Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                   lhs->Print(2, TFile));
      stmt->Lhs()->Set_istr_base(lhs);
    }
    break;
    
  case OPR_MSTORE:
    {
      x = Propagate_vardef(stmt->Lhs()->Istr_base(), bb, stmt, NULL);
      if (VSA_Prop_Vardef && x != NULL && x != stmt->Lhs()->Istr_base()) {
        Is_Trace(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                 (TFile, "PROP VARDEF: replace istr base cr%d of sr%d at line %d:\n",
                         stmt->Lhs()->Istr_base()->Coderep_id(),
                         stmt->Stmtrep_id(), Srcpos_To_Line(stmt->Linenum())));
        Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                     stmt->Print(TFile));
        Is_Trace(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                 (TFile, " with cr%d:\n", x->Coderep_id()));
        Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                     x->Print(2, TFile));
        stmt->Lhs()->Set_istr_base(x);
      }
      CODEREP *num_bytes = (CODEREP *)stmt->Lhs()->Mstore_size();
      x = Propagate_vardef(num_bytes, bb, stmt, NULL);
      if (VSA_Prop_Vardef && x != NULL && x != num_bytes) {
        Is_Trace(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                 (TFile, "PROP VARDEF: replace mstore size cr%d of sr%d at line %d:\n",
                         stmt->Lhs()->Mstore_size()->Coderep_id(),
                         stmt->Stmtrep_id(), Srcpos_To_Line(stmt->Linenum())));
        Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                     stmt->Print(TFile));
        Is_Trace(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                 (TFile, " with cr%d:\n", x->Coderep_id()));
        Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                     x->Print(2, TFile));
        stmt->Lhs()->Set_mstore_size(x);
      }
    }
    break;
  default:
    if (OPERATOR_is_call(stmt->Opr())) {
    }
    break;
  }
}


// =============================================================================
//
//  VSA::Classify_uiv_error - Report value related errors at statement level
//
// =============================================================================
void
VSA::Classify_uiv_error(STMTREP *stmt, BB_NODE *bb)
{

  if (!(VSA_Uiv() && !PU_java_lang(Get_Current_PU())))
    return;
  
  if (stmt->Op() == OPC_XPRAGMA && // should move the inline body check here
      WN_pragma(stmt->Orig_wn()) == WN_PRAGMA_COPYIN_BOUND)
    return;

  if (stmt->Op() == OPC_ASM_STMT) {
    Classify_asm_uiv_error(stmt, bb);
    return;
  }

  MU_NODE *mnode;
  MU_LIST_ITER mu_iter;
  FOR_ALL_NODE( mnode, mu_iter, Init(Stmt_vor_mu(stmt))) {
    CVOR *cvor = (CVOR*)mnode->OPND();
    VSYM_OBJ_REP *vor = cvor->first;

    Is_True(vor, ("CVOR contains a null vor"));
    if (vor->Is_entry_chi() && IPSA_insession() &&
        OPERATOR_is_call(stmt->Opr())) {
      CODEREP  *cr  = cvor->second;
      RNA_NODE *rna = Sr_2_rna(stmt);
      if (rna == NULL)
        continue;
      IDTYPE param;
      if(cr->Kind() == CK_OP && rna->Is_flag_set(RBC_SE_MODEL)) {
        // jni generate mu for OP kind, guard with jni only?
        continue;
      } else {
        param = rna->Get_arg_with_cr(cr);
      }
      if (param >= VAR_INIT_ID) {
        UINT32    arg_flags = rna->Get_arg_flags(param);
        if ((arg_flags & REF_ILOAD) == REF_ILOAD) {
          // This is a maybe UIV, might be worthwhile report
          // WORK IN PROGRESS
        }
      }
    }
  }

  if (stmt->Rhs()) {
    if (! stmt->Is_identity_asgn() &&
        (IPSA_insession() && ! Dna()->Is_param_copyin(stmt) ) )
      Classify_uiv_error(stmt->Rhs(), bb, stmt, FOR_UIV);
  }

  switch (stmt->Opr()) { 
  case OPR_STID:
    break;
  case OPR_ISTORE:
  case OPR_ISTBITS:
    Classify_uiv_error(stmt->Lhs()->Istr_base(), bb, stmt, FOR_ISTOR_BASE);
    break;
    
  case OPR_MSTORE:
    {
      Classify_uiv_error(stmt->Lhs()->Istr_base(), bb, stmt, FOR_ISTOR_BASE);
      CODEREP *num_bytes = (CODEREP *)stmt->Lhs()->Mstore_size();
      Classify_uiv_error(num_bytes, bb, stmt, FOR_UIV);
    }
    break;
  default:    break;
  }
}

// =============================================================================
//
//  VSA::Vsa_stmt - do RNA process and DBF prep work at statement level
//
// =============================================================================
void
VSA::Vsa_stmt(STMTREP *stmt, BB_NODE *bb, const AUX_DEF_MAP* to_add,
              const AUX_DEF_MAP* to_upd, const AUX_SET_MAP* aux_set, MEM_POOL *def_bbs_pool)
{
  CODEREP *x = NULL;
  if (stmt->Rhs()) {
    x = Vsa_cr(stmt->Rhs(), stmt, def_bbs_pool, FALSE/*in_array*/);
  }

  MU_LIST  *mu_list;
  CHI_LIST *chi_list;

  switch (stmt->Opr()) {
  case OPR_STID:
    {
      AUX_ID aux = stmt->Lhs()->Aux_id();
      if ((to_add != NULL && to_add->find(aux) != to_add->end()) ||
          (to_upd != NULL && to_upd->find(aux) != to_upd->end())) {
        CODEREP* lhs = stmt->Lhs();
        Opt_stab()->Push_coderep(aux, lhs);
        Is_Trace(Tracing(),
                 (TFile, "Vsa_stmt: stid-push STID:%d cr%d:sym%dv%d\n",
                         stmt->Stmtrep_id(), lhs->Coderep_id(), aux, lhs->Version()));
      }

      if (x != NULL) {
        HEAP_OBJ_REP *heap_obj_rep = Cr_2_heap_obj(stmt->Rhs());

        // rhs points to a heap_obj; we make lhs to point-to the same heap_obj
        // our algorithm creates heap_obj from memory allocation call and our
        // IR has lowered to have dedicate register carrying the return value,
        // as the result, when we perform heap_obj allocation, there's no visibilty
        // to the` lhs varible.  Therefore, we will merge heap_obj here.
        if (heap_obj_rep != NULL) {
          AUX_STAB_ENTRY *aux =_opt_stab->Aux_stab_entry(stmt->Lhs()->Aux_id());
          STMTREP* next = stmt->Next();

          // assign to return value or global var
          if ((aux->Is_preg() && Is_Return_Preg(aux->St_ofst()) &&
               next != NULL && next->Opr() == OPR_RETURN) ||
              (aux->Is_global())) {
            HEAP_OBJ_REP *new_hor = Clone_heap_obj(heap_obj_rep, bb, _mem_pool);
            new_hor->Set_attr(ROR_DEF_BY_COPY);
            new_hor->Set_stmt_def(stmt, Comp_unit()->Dna());
            if (aux->Is_global())
              new_hor->Set_asgn_attr(ROR_ASGN_TO_GLOBAL);
            else
              new_hor->Set_asgn_attr(ROR_ASGN_TO_RETREG);
            heap_obj_rep = new_hor;  // force new_hor to be in heap_obj_map
          }

          // lhs copy from rhs, replace old lhs hor with new rhs hor
          if (heap_obj_rep->Attr() != ROR_DEF_BY_LDA)
            Enter_cr_heap_obj_map(stmt->Lhs(), heap_obj_rep, TRUE);

          // check if Lhs has already assigned with heap_obj in the procedure
          // example:
          //           p = malloc();
          //                              we have two STID after malloc call
          //           q = malloc();
          //           p = q;             We need a merge for this
          // at the same time, the object pointed to by p is a memory leak
          // due to the copy propagation, we have a mess to deal with in -xa
          // TODO TODO TODO: more work to be done!
          HEAP_OBJ *heap_obj = Find(stmt->Lhs()->Aux_id(), FALSE);
          if (heap_obj != NULL) {
            IDTYPE          rhs_id = heap_obj_rep->Heap_obj()->Sym_id();
#if 0
            Is_True(rhs_id != INVALID_ID,
                    ("VSA::Vsa_stmt, the heap_obj of rhs has invalid_id"));
#endif
            if (rhs_id != INVALID_ID) {
              AUX_STAB_ENTRY *rhs_sym = Opt_stab()->Aux_stab_entry(rhs_id);
              STMTREP *prev_stmt = stmt->Prev();
              CODEREP *prevstmt_rhs =
                (prev_stmt && prev_stmt->Rhs()) ? prev_stmt->Rhs() : NULL;
              if (rhs_sym->Is_preg() && prevstmt_rhs &&
                  prevstmt_rhs->Kind() == CK_VAR &&
                  heap_obj_rep->Heap_obj()->Byte_size() == heap_obj->Byte_size()) {
                // this is for p = malloc(); that has already had another p = malloc();
                // We convert the heap_obj of the rhs to use the heap_obj of lhs
                // only replace HO when size is the same
                if (heap_obj->Get_based_on() != heap_obj_rep->Heap_obj())
                  heap_obj_rep->Heap_obj()->Set_based_on(heap_obj);
                heap_obj_rep->Set_heap_obj(heap_obj);
                heap_obj->Prepend_def_bbs(bb, def_bbs_pool);
              }
              else { // the rhs heap_obj has sym_id, the "p = q;" is occurring
                // the following enables the redirection from p to q for Find()
                Merge(heap_obj, heap_obj_rep->Heap_obj());
                if (heap_obj_rep->Heap_obj()->Get_based_on() != heap_obj)
                  heap_obj->Set_based_on(heap_obj_rep->Heap_obj());
              }
            }
            else { // rhs_id == INVALID_ID, this could be caused by the rhs is a preg
              Merge(heap_obj, heap_obj_rep->Heap_obj());
            }
          } // END of (heap_obj != NULL); Lhs() has already had been intialized before
          else {
            IDTYPE id = stmt->Lhs()->Aux_id();
            BOOL   lhs_is_ptr = Ty_Table[stmt->Lhs()->Lod_ty()].kind == KIND_POINTER;
            AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(id);
            if (! sym->Is_dedicated_preg()) {
              heap_obj_rep->Heap_obj()->Prepend_def_bbs(bb, def_bbs_pool);
              if (heap_obj_rep->Heap_obj()->Kind() != RSC_KIND_LDA) {
                // preserve the sym_id of original variable for above two cases
                heap_obj_rep->Heap_obj()->Set_ho_cr(stmt->Lhs());
              } else {
                // to avoid a new unused ho created in Create_aggregate_heapobj
                Enter_cr_heap_obj_map(stmt->Lhs(), heap_obj_rep);
                // check the chi list if this STID is an assignment to a struct field
                // or an array subscript.
                Create_aggregate_heapobj(stmt);
              }
            }
          }
          Is_Trace(Tracing(), (TFile, "VSA::Vsa_stmt: assigning heap obj to its Lhs\n"));
          Is_Trace_cmd(Tracing(), stmt->Lhs()->Print(9, TFile));
          Is_Trace_cmd(Tracing(), heap_obj_rep->Heap_obj()->Print(TFile));
        } // the rhs carries a heap_obj_rep
        else if (Cr_2_heap_obj(stmt->Lhs()) == NULL) {
          // this could happen if the lhs is a pointer that is associated with a heap_obj
          // but the rhs is a pointer that is potentially defined outside the current pu
          HEAP_OBJ *ho_w_matching_auxid = Find(stmt->Lhs()->Aux_id(), FALSE);
          if (ho_w_matching_auxid != NULL) {
            HEAP_OBJ_REP *new_hor = Allocate_heap_obj(ho_w_matching_auxid, NULL);
            ho_w_matching_auxid->Prepend_def_bbs(bb, def_bbs_pool);
            Enter_cr_heap_obj_map(stmt->Lhs(), new_hor);

            Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Vsa_stmt STID: an unknown heap_obj_rep assigned to a known pointer\n"));
          }
        }
      }
      else { // this section takes care of p = NULL;
        BOOL lhs_is_ptr = Ty_Table[stmt->Lhs()->Lod_ty()].kind == KIND_POINTER ||
                          Find(stmt->Lhs()->Aux_id(), FALSE) != NULL;
        CODEREP *x = stmt->Rhs();
        if (lhs_is_ptr && x) {
          if (x->Kind() == CK_CONST && x->Const_val() == 0) {
            Enter_cr_heap_obj_map(stmt->Lhs(), Null_hor());
          }
          else if (VSA_Model_Lda() != 0 && x->Kind() == CK_IVAR) {
            // This is potentially an n-dimensional array load
            HEAP_OBJ_REP* hor;
            HEAP_OBJ     *ho_w_matching_auxid = Find(stmt->Lhs()->Aux_id(), FALSE);
            if (ho_w_matching_auxid != NULL) {
              hor = Allocate_heap_obj(ho_w_matching_auxid, NULL);
              ho_w_matching_auxid->Prepend_def_bbs(bb, def_bbs_pool);
            }
            else {
              hor = Allocate_heap_obj(stmt->Lhs(), bb, def_bbs_pool);
              Is_True(hor->Heap_obj()->Sym_id() == stmt->Lhs()->Aux_id(), ("aux id mismatch"));
            }
            // This IVAR does not have an hor associate with it yet
            if (Cr_2_heap_obj(x)) {
              Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Vsa_stmt STID: a heap_obj_rep associated with the ILOD already\n"));
              Is_Trace_cmd(Tracing(), Cr_2_heap_obj(x)->Print(TFile));
              Is_Trace(Tracing(), (TFile, "\n"));
            }
            else {
              //Enter_cr_heap_obj_map(x, hor);
            }
            Enter_cr_heap_obj_map(stmt->Lhs(), hor);
            hor->Set_attr(ROR_DEF_BY_AUTO); // match the logic in update_reaching def
          }
        }
      }
    }
    break;
  case OPR_ISTORE:
    {
      if (x != NULL) {
        HEAP_OBJ_REP *heap_obj_rep = Cr_2_heap_obj(stmt->Rhs());

        if (heap_obj_rep != NULL) {
          Enter_cr_heap_obj_map(stmt->Lhs(), heap_obj_rep); // lhs copy from rhs
          // check if Lhs has already assigned with heap_obj in the procedure
          // example:
          //           *p = malloc();
          //           *q = malloc();
          //           *p = *q;             We need a merge for this
          // at the same time, the object pointed to by *p is a memory leak
          // due to the copy propagation, we have a mess to deal with in -xa
#if 0     // cannot handle sym_id of heap_obj yet, comment out with if 0
          HEAP_OBJ *heap_obj = Find(stmt->Lhs()->Aux_id());
          if (heap_obj != NULL) {
            IDTYPE          rhs_id = heap_obj_rep->Heap_obj()->Sym_id();
            Is_True(rhs_id != INVALID_ID,
                    ("VSA::Vsa_stmt, the heap_obj of rhs has invalid_id"));

            if (rhs_id != INVALID_ID) {
              AUX_STAB_ENTRY *rhs_sym = Opt_stab()->Aux_stab_entry(rhs_id);
              STMTREP *prev_stmt = stmt->Prev();
              CODEREP *prevstmt_rhs =
                (prev_stmt && prev_stmt->Rhs()) ? prev_stmt->Rhs() : NULL;
              if (rhs_sym->Is_preg() && prevstmt_rhs &&
                  prevstmt_rhs->Kind() == CK_VAR) {
                // this is for p = malloc(); that has already had another p = malloc();
                // We convert the heap_obj of the rhs to use the heap_obj of lhs
                heap_obj_rep->Heap_obj()->Set_based_on(heap_obj);
                heap_obj_rep->Set_heap_obj(heap_obj);
                heap_obj->Prepend_def_bbs(bb, def_bbs_pool);
              }
              else { // the rhs heap_obj has sym_id, the "p = q;" is occurring
                // the following enables the redirection from p to q for Find()
                Merge(heap_obj, heap_obj_rep->Heap_obj());
                heap_obj->Set_based_on(heap_obj_rep->Heap_obj());
              }
            }
            else { // rhs_id == INVALID_ID, this should not happen, FmtAssert here?
              Merge(heap_obj, heap_obj_rep->Heap_obj());
            }
          } // END of (heap_obj != NULL); Lhs() has already had been intialized before
          else {
            IDTYPE id = stmt->Lhs()->Aux_id();
            AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(id);
            if (! sym->Is_dedicated_preg()) {
              Is_True(heap_obj_rep->Heap_obj()->Sym_id() == id, ("aux id mismatch"));
              heap_obj_rep->Heap_obj()->Prepend_def_bbs(bb, def_bbs_pool);
            }
          }
#endif
          heap_obj_rep->Heap_obj()->Prepend_def_bbs(bb, def_bbs_pool);
          Is_Trace(Tracing(), (TFile, "VSA::Vsa_stmt: assigning heap obj to its ISTORE Lhs\n"));
          Is_Trace_cmd(Tracing(), stmt->Lhs()->Print(9, TFile));
          Is_Trace_cmd(Tracing(), heap_obj_rep->Heap_obj()->Print(TFile));
        } // the rhs carries a heap_obj_rep

        // always assign hor for lhs istore base
        // - this could happen if the lhs is a pointer that is associated with a heap_obj
        // - but the rhs is a pointer that is potentially defined outside the current pu
        CODEREP* cr = Find_base_pointer_load(stmt->Lhs()->Istr_base());
        if (cr != NULL && cr->Kind() == CK_VAR) {
          // istore to parameter, set ROR_ASGN_TO_OPARM
          if (heap_obj_rep != NULL && Dna()->Is_param(cr))
            heap_obj_rep->Set_asgn_attr(ROR_ASGN_TO_OPARM);

          if (Cr_2_heap_obj(cr) == NULL) {
            // TODO: LDA/IVAR??
#if 0
            HEAP_OBJ_REP *new_hor;
            HEAP_OBJ *ho_w_matching_auxid = Find(cr->Aux_id());
            if (ho_w_matching_auxid == NULL) {
              new_hor = Allocate_heap_obj(bb, def_bbs_pool);
              //new_hor->Set_attr(ROR_DEF_BY_ISTORE);
              //new_hor->Set_stmt_def(stmt, Dna());
              //new_hor->Set_srcpos_node(stmt, Dna(), PATHINFO_ALLOC);
              Find_hor_def(cr, new_hor);
            }
            else {
              new_hor = Allocate_heap_obj(ho_w_matching_auxid, NULL);
              ho_w_matching_auxid->Prepend_def_bbs(bb, def_bbs_pool);
            }
#endif
            HEAP_OBJ_REP *new_hor = Find_or_create_hor(cr, bb, def_bbs_pool);
            if (new_hor != NULL) {
              Enter_cr_heap_obj_map(cr, new_hor);
              Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Vsa_stmt ISTORE: an unknown heap_obj_rep assigned to a known pointer\n"));
            }
          }
        }
      }
      else { // this section takes care of *p = NULL;
#if 0
        CODEREP *x = stmt->Rhs();
        if (x->Kind() == CK_CONST && x->Const_val() == 0) {
          HEAP_OBJ_REP* null_hor = Get_null_hor(bb, def_bbs_pool);
          Enter_cr_heap_obj_map(stmt->Lhs(), null_hor);
        }
#endif
        CODEREP* cr = Find_base_pointer_load(stmt->Lhs()->Istr_base());
        if (cr != NULL && cr->Kind() == CK_VAR &&
            Cr_2_heap_obj(cr) == NULL) {
          HEAP_OBJ_REP *new_hor = Find_or_create_hor(cr, bb, def_bbs_pool);
          if (new_hor != NULL) {
            Enter_cr_heap_obj_map(cr, new_hor);
            Is_Trace(Tracing(), (TFile, "@@@@@@@@ VSA::Vsa_stmt ISTORE: an unknown heap_obj_rep assigned to a known pointer\n"));
          }
        }
      }
    }
    break;
  case OPR_ISTBITS:
    x = Vsa_cr(stmt->Lhs()->Istr_base(), stmt, def_bbs_pool, FALSE/*in_array*/);
    // todo: some chi list process
    break;
  case OPR_MSTORE:
    {
      x = Vsa_cr(stmt->Lhs()->Istr_base(), stmt, def_bbs_pool, FALSE/*in_array*/ );
      CODEREP *num_bytes = (CODEREP *)stmt->Lhs()->Mstore_size();
      x = Vsa_cr(num_bytes, stmt, def_bbs_pool, FALSE/*in_array*/);
      // todo: some chi list process
    }
    break;
  case OPR_RETURN:
  case OPR_RETURN_VAL:
    if ((to_add != NULL && to_add->size() > 0) ||
        (to_upd != NULL && to_upd->size() > 0))
      Update_stmt_var_mu(stmt, to_add, to_upd, NULL);

    mu_list = Stmt_vor_mu(stmt);
    Is_True(mu_list == NULL, ("Create_refn_return_mu_vsym called more than once\n"));
    if (mu_list == NULL) {
      mu_list = CXX_NEW(MU_LIST, Mem_pool());
      Enter_stmt_vor_mu_map(stmt, mu_list);
    }
    //Dna()->Create_refn_4_exitmu(Comp_unit(), stmt, def_bbs_pool);
    break;
  case OPR_OPT_CHI:
    if ((to_add != NULL && to_add->size() > 0) ||
        (to_upd != NULL && to_upd->size() > 0))
      Update_stmt_var_chi(stmt, to_add, to_upd, NULL);

    chi_list = Stmt_vor_chi(stmt);
    Is_True(chi_list == NULL, ("Create_refn_entry_chi_vsym called more than once\n"));
    if (chi_list == NULL) {
      chi_list = CXX_NEW(CHI_LIST, Mem_pool());
      Enter_stmt_vor_chi_map(stmt, chi_list);
    }
    if (WN_operator(bb->Entrywn()) == OPR_FUNC_ENTRY)
      Dna()->Create_refn_4_entrychi(Comp_unit(), stmt, def_bbs_pool);
    break;
  default:
    {
      if (OPERATOR_is_call(stmt->Opr())) {

        mu_list = Stmt_vor_mu(stmt);
        Is_True(mu_list == NULL, ("Create_refn_vsym called more than once\n"));
        if (mu_list == NULL) {
          mu_list = CXX_NEW(MU_LIST, Mem_pool());
          Enter_stmt_vor_mu_map(stmt, mu_list);
        }

        chi_list = Stmt_vor_chi(stmt);
        Is_True(chi_list == NULL, ("Create_refn_vsym called more than once\n"));
        if (chi_list == NULL) {
          chi_list = CXX_NEW(CHI_LIST, Mem_pool());
          Enter_stmt_vor_chi_map(stmt, chi_list);
        }

        AUX_SET* call_aux = NULL;
        if (aux_set != NULL) {
          AUX_SET_MAP::const_iterator it = aux_set->find(stmt->Stmtrep_id());
          if (it != aux_set->end())
            call_aux = it->second;
        }
        if ((to_add != NULL && to_add->size() > 0) ||
            (to_upd != NULL && to_upd->size() > 0))
          Update_stmt_var_mu(stmt, to_add, to_upd, call_aux);

        Handle_call(stmt, bb, def_bbs_pool);

        if (stmt->Opr() != OPR_CALL && stmt->Opr() != OPR_ICALL)
          break;
        if ((to_add != NULL && to_add->size() > 0) ||
            (to_upd != NULL && to_upd->size() > 0))
          Update_stmt_var_chi(stmt, to_add, to_upd, call_aux);
      }
    }
    break;
  }
}

// =============================================================================
//
//  Scan_npd_error - do VSA by Use-Def Chain Traveral according to pre-order traversal
//  of the dominator tree so that all variables' defs are seen before any use.
//  Maintaining the stack is to deal with zero versioning.
//
// =============================================================================
void
VSA::Scan_npd_error(BB_NODE *bb, MEM_POOL *def_bbs_pool)
{
  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) { 
    Udt_stmt(stmt, bb, def_bbs_pool);
  }

  // do vulnerability static analysisn for its dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Scan_npd_error(dom_bb, def_bbs_pool);
}

// =============================================================================
//
//  Propagate_vardef - Mark all variable load if it is def, Maydef, value zero
//  by Use-Def Chain Traveral according to pre-order traversal of the dominator
//  tree 
//
// =============================================================================
void
VSA::Propagate_vardef(BB_NODE *bb)
{
  CODEREP *cr;
  PHI_NODE *phi; 
  PHI_LIST_ITER phi_iter;

  //  iterate through each phi-node 
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    if (phi->Live()) {
      CODEREP *phi_result = phi->RESULT();
      Opt_stab()->Push_coderep(phi->Aux_id(), phi_result);

      if ( phi_result->Value_checked() ) continue;

      SCC scc(phi_result, this, Loc_pool());
      
      hash_set<IDTYPE> visited_bb;
      PHI_OPND_ITER phi_opnd_iter(phi);
      CODEREP *opnd;
      BOOL alldef = TRUE;
      INT  invalid_addr_cnt = 0;
      INT  maydangling_ref_cnt = 0;
      INT  unknown_value_cnt = 0;
      FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
        CODEREP *cr = Propagate_vardef(opnd, &scc, visited_bb);
        if (!scc.Defer(opnd)) {
          if (! cr->Value_def()) {
            alldef = FALSE;
            phi_result->Set_value_maydef();
          }

          if (cr->Value_invalid_addr())
            invalid_addr_cnt++;
          else if (cr->Value_maydangling())
            maydangling_ref_cnt++;
          else
            unknown_value_cnt++;
        }
      }
      if (alldef) // todo: handel this reset
        phi_result->Set_value_def();

      if (invalid_addr_cnt || maydangling_ref_cnt) {
        if ((invalid_addr_cnt+maydangling_ref_cnt+unknown_value_cnt) == invalid_addr_cnt) {
          phi_result->Set_value_invalid_addr();
        } else {
          phi_result->Set_value_maydangling();
        }
      }
      scc.Update();

    }
    else Opt_stab()->Push_coderep(phi->Aux_id(), NULL);
  }

  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {

    // check if we are starting inline body, push the inline context, or pop if inline body end
    // WN_PRAGMA_ID pragma = (WN_PRAGMA_ID)WN_pragma(stmt->Orig_wn());
    // if (pragma == WN_PRAGMA_INLINE_BODY_START) ... copy-in handling
    // else if (pragma == WN_PRAGMA_INLINE_BODY_END) ... how to handle copy-out? especially multi-exit call
    // 
    Propagate_vardef(stmt, bb);
    if (OPERATOR_is_scalar_store (stmt->Opr())) {
      CODEREP *lhs = stmt->Lhs();
      Opt_stab()->Push_coderep(lhs->Aux_id(), lhs);
    }

    if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = stmt->Chi_list();
      FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
        if (cnode->Live()) {
          Opt_stab()->Push_coderep(cnode->Aux_id(), cnode->RESULT());
        }
        else Opt_stab()->Push_coderep(cnode->Aux_id(), NULL);
      }
    }

    if (OPERATOR_is_scalar_store (stmt->Opr()) &&
        Opt_stab()->Aux_stab_entry(stmt->Lhs()->Aux_id())->Is_dedicated_preg())
      Set_past_ret_reg_def();
    else if (stmt->Opr() == OPR_RETURN || 
             stmt->Opr() == OPR_RETURN_VAL ||
             stmt->Opr() == OPR_REGION ||
               stmt->Opr() == OPR_GOTO_OUTER_BLOCK)
      Reset_past_ret_reg_def();
  }

  // do vulnerability static analysisn for its dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Propagate_vardef(dom_bb);

  // iterate through each statement in this bb
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
    if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = stmt->Chi_list();
      FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
        Opt_stab()->Pop_coderep(cnode->Aux_id());
      }
    } 
    if (OPERATOR_is_scalar_store (stmt->Opr())) {
      Opt_stab()->Pop_coderep(stmt->Lhs()->Aux_id());
      if (VSA_Prop_Vardef && stmt->Rhs()->Kind() == CK_VAR &&
          !stmt->Rhs()->Is_var_volatile()) {
        Is_True(Opt_stab()->Top_coderep(stmt->Rhs()->Aux_id()) == stmt->Lhs(),
                ("top coderep mismatch"));
        Is_Trace(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                 (TFile, "PROP VARDEF: pop cr%d sym%dv%d for sr%d at line %d:\n",
                         Opt_stab()->Top_coderep(stmt->Rhs()->Aux_id())->Coderep_id(),
                         Opt_stab()->Top_coderep(stmt->Rhs()->Aux_id())->Aux_id(),
                         Opt_stab()->Top_coderep(stmt->Rhs()->Aux_id())->Version(),
                         stmt->Stmtrep_id(), Srcpos_To_Line(stmt->Linenum())));
        Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VARPROP_TRACE_FLAG),
                     stmt->Print(TFile));
        Opt_stab()->Pop_coderep(stmt->Rhs()->Aux_id());
      }
    }
  }

  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    Opt_stab()->Pop_coderep(phi->Aux_id());
  }
}

// =============================================================================
//
//  Classify_uiv_error - Report value related errors including uiv and npd, by
//  Use-Def Chain Traveral according to pre-order traversal of the CFG dominator
//  tree. Since this is just for error reporting, we do not need maintain stack.
//
// =============================================================================
void
VSA::Scan_uiv_error(BB_NODE *bb)
{
  if (!(VSA_Uiv() && !PU_java_lang(Get_Current_PU())))
    return;
  
  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {

    // check if we are starting inline body, push the inline context, or pop if inline body end
    // WN_PRAGMA_ID pragma = (WN_PRAGMA_ID)WN_pragma(stmt->Orig_wn());
    // if (pragma == WN_PRAGMA_INLINE_BODY_START) ... copy-in handling
    // else if (pragma == WN_PRAGMA_INLINE_BODY_END) ... how to handle copy-out? especially multi-exit call

    Classify_uiv_error(stmt, bb);
  }

  // do the same for its dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Scan_uiv_error(dom_bb);
}

// =============================================================================
// BB_has_real_stmtrep
// Check if there are "real" stmtrep in BB or BB's unique successor
// =============================================================================
BOOL
VSA::BB_has_real_stmtrep(BB_NODE *bb) const
{
  do {
    if (!bb->Is_empty())
      return TRUE;
    if (bb->Succ() == NULL)
      return FALSE;
    if (bb->Succ()->Multiple_bbs())
      return FALSE;
    bb = bb->Succ()->Node();
    if (bb->Pred() && bb->Pred()->Multiple_bbs())
      return FALSE;
  } while (bb);
  return FALSE;
}

// =============================================================================
// Is_bb_loop_inverted
// Check if two adjcent DDs are from loop inversion
// =============================================================================
BOOL
VSA::Is_bb_loop_inverted(BB_NODE *pred, BB_NODE *succ) const
{
  // check if pred is LOGIF and succ is DOHEAD
  if (pred->Kind() != BB_LOGIF || succ->Kind() != BB_DOHEAD)
    return FALSE;
  // check if pred has two succs
  if (!pred->Succ() || !pred->Succ()->Next() ||
      pred->Succ()->Next()->Next())
    return FALSE;
  // check if succ is one of pred's succ
  if (pred->Succ()->Node() != succ &&
      pred->Succ()->Next()->Node() != succ)
    return FALSE;
  // check if succ has EVAL whose kid is the same in pred's last stmt
  STMTREP *eval = succ->Last_stmtrep();
  if (eval == NULL || eval->Opr() != OPR_EVAL)
    return FALSE;
  STMTREP *cmp = pred->Last_stmtrep();
  if (cmp == NULL ||
      (cmp->Opr() != OPR_TRUEBR && cmp->Opr() != OPR_FALSEBR))
    return FALSE;
  return cmp->Rhs()->Contains(eval->Rhs());
}

// =============================================================================
// Can_swap_condition
// Check if two bb can be swapped, which is lowered from
// if (a && b) {         if (b && a) {
//   do something   ==>    do something
// }                     }
// if (a || b) {         if (b || a) {
//   do something   ==>    do something
// }                     }
// =============================================================================
BOOL
VSA::Can_swap_condition(BB_NODE *pred, BB_NODE *succ) const
{
  Is_True_Ret(pred && pred->Succ() && succ && succ->Pred(),
              ("wrong bb"), FALSE);
  if (pred->Kind() != BB_LOGIF || succ->Kind() != BB_LOGIF)
    return FALSE;
  // succ has extra stmtrep in BB, can't swap
  if (succ->First_stmtrep() != succ->Last_stmtrep())
    return FALSE;

  // handle if ifinfo is NULL
  if (pred->Ifinfo() == NULL || succ->Ifinfo() == NULL) {
    // make sure pred/succ only have 2 succ
    if (pred->Succ()->Next() == NULL ||
        pred->Succ()->Next()->Next() != NULL ||
        succ->Succ()->Next() == NULL ||
        succ->Succ()->Next()->Next() != NULL)
      return FALSE;
    BB_NODE *else_blk = pred->Succ()->Node();
    if (else_blk == succ)
      else_blk = pred->Succ()->Next()->Node();
    // succ and pred must share one succ
    if (succ->Succ()->Node() == else_blk ||
        succ->Succ()->Next()->Node() == else_blk)
      return TRUE;
    return FALSE;
  }

  // another succ of pred must be empty
  BB_IFINFO *pred_info = pred->Ifinfo();
  if (pred_info->Then() != succ && pred_info->Else() != succ)
    return FALSE;
  BB_NODE *else_blk = pred_info->Else();
  if (else_blk == succ)
    else_blk = pred_info->Then();
  if (BB_has_real_stmtrep(else_blk))
    return FALSE;
  // one if succ's succ must be empty
  BB_IFINFO *succ_info = succ->Ifinfo();
  if (succ_info->Then() && succ_info->Else() &&
      BB_has_real_stmtrep(succ_info->Then()) &&
      BB_has_real_stmtrep(succ_info->Else()))
    return FALSE;
  BB_NODE *succ_merge = succ_info->Merge();
  // no merge block but one of succ is pred's succ
  if (succ_merge == NULL &&
      (succ_info->Then() == else_blk ||
       succ_info->Else() == else_blk))
    return TRUE;
  // succ merge must be empty
  if (succ_merge && BB_has_real_stmtrep(succ_merge))
    return FALSE;
  // succ merge must be pred merge's pred
  if (succ_merge->Succ() &&
      succ_merge->Succ()->Node() == pred_info->Merge())
    return TRUE;
  // otherwise return FALSE
  return FALSE;
}

// =============================================================================
// Check_redundant_condition
// Check if conditions from entry to BB is redundant
// =============================================================================
void
VSA::Check_redundant_condition(BB_NODE *bb, CODEREP *cond,
                               SRCPOS_HANDLE* sp_h, hash_set<IDTYPE>& visited)
{
  BB_NODE* cd;
  BB_NODE_SET_ITER cd_iter;
  Is_True(sp_h->Root_stmt() && sp_h->Root_stmt()->Bb(), ("no root stmt"));
  BB_NODE* root_bb = sp_h->Root_stmt()->Bb();
  FOR_ALL_ELEM(cd, cd_iter, Init(bb->Rcfg_dom_frontier())) {
    Is_True(cd->Succ() != NULL,
            ("cd bb does not have successors"));
    std::pair<CODEREP*, STMTREP*> cd_cond = Comp_unit()->Vra()->Get_bb_cond_rec(cd);
    if (cd_cond.first == NULL)
      continue;
    Is_True(cd_cond.second && cd_cond.second->Bb(), ("invalid cd cond"));
    BB_NODE* cd_bb = cd_cond.second->Bb();
    if (!cd_bb->Dominates(bb) || visited.find(cd_bb->Id()) != visited.end())
      continue;
    visited.insert(cd_bb->Id());
    VRA_RESULT res1 = Comp_unit()->Vra()->Compare_cr(cond, cd_cond.first,
                                                     root_bb->Id(), cd_bb->Id());
    VRA_RESULT res2 = Comp_unit()->Vra()->Compare_cr(cd_cond.first, cond,
                                                     cd_bb->Id(), root_bb->Id());

    Is_Trace(Tracing(),
             (TFile, "[RCD] Check condition from BB%d (BB%d) to BB%d (BB%d):\n",
                     root_bb->Id(), bb->Id(), cd_bb->Id(), cd->Id()));
    Is_Trace(Tracing(), (TFile, "From condition:\n"));
    Is_Trace_cmd(Tracing(), cond->Print(TFile));
    Is_Trace(Tracing(), (TFile, "To condition:\n"));
    Is_Trace_cmd(Tracing(), cd_cond.first->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, "Compare result: res1=%d res2=%d, swapable=%d\n",
                     res1, res2, Can_swap_condition(cd_bb, root_bb)));
    // case1: res1 == res2 == VA_NO
    //   no intersection between two comparions, for example:
    //   if (a > 5) { if (a < 4) { ... } }
    // case 2: res1 == VA_YES, res2 == VA_YES or VA_POSSIBLE
    //   cd cond is superset of rootx cond, for example:
    //   if (a > 5) { if (a > 4) { ... } }
    // case 3: res2 == VA_YES, res1 == VA_YES or VA_POSSIBLE
    //   rootx cond is superset of cd cond. for example:
    //   if (a > 4) { if (a > 5) { ... } }
    // for case 3, need to make sure two branch can be swapped
    if ((res1 == VA_NO && res2 == VA_NO) ||
        (res1 == VA_YES &&
         (res2 == VA_POSSIBLE || res2 == VA_YES)) ||
        (res2 == VA_YES &&
         (res1 == VA_POSSIBLE || res1 == VA_YES) &&
         Can_swap_condition(cd_bb, root_bb))) {
      // set a mark to keep previous data in srcpos handler
      UINT32 mark = sp_h->Water_mark();
      sp_h->Append_data(cd_cond.second, Dna(), PATHINFO_CD_BB, FALSE);
      Report_vsa_error(cond, NULL, "RCD", 2, IC_DEFINITELY, sp_h);
      // remove all entries added just now
      sp_h->Set_water_mark(mark);
    }
    //Check_redundant_condition(cd_bb, cond, sp_h, visited);
  }
}

// =============================================================================
// Is_var_overflow_candidate
//   check if cr is candidate for integer overflow and also compose the path
// =============================================================================
BOOL
VSA::Is_var_overflow_candidate(CODEREP *cr, SRCPOS_HANDLE *sp_h, hash_set<IDTYPE>& visited) const
{
  Is_True(cr && cr->Kind() == CK_VAR, ("not var"));
  if (cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
      cr->Is_var_volatile())
    return TRUE;  // ignore whole expr

  if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = cr->Defphi();
    if (visited.find(phi->Bb()->Id()) != visited.end())
      return TRUE;
    visited.insert(phi->Bb()->Id());

    // don't append path for phi
    IDTYPE i = 0;
    CODEREP *opnd;
    PHI_OPND_ITER opnd_iter(phi);
    FOR_ALL_ELEM (opnd, opnd_iter, Init()) {
      if (Is_var_overflow_candidate(opnd, sp_h, visited) == TRUE)
        return TRUE;
    }
    return FALSE;
  }
  else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP *def = cr->Defstmt();
    Is_True(def != NULL, ("null def stmt"));
    if (def->Opr() == OPR_OPT_CHI ||
        OPERATOR_is_call(def->Opr())) {
      sp_h->Append_data(def, Dna(), PATHINFO_CHI);
      return TRUE;
    }
  }
  else {
    STMTREP *def = cr->Defstmt();
    Is_True(def != NULL, ("null def stmt"));
    CODEREP *rhs = def->Rhs();
    if (rhs != NULL) {
      // for IVAR, check it's ilod base
      while (rhs->Kind() == CK_IVAR) {
        rhs = rhs->Ilod_base();
        Is_True(rhs != NULL, ("bad ilod base"));
      }
      if (rhs->Kind() == CK_VAR) {
        sp_h->Append_data(def, Dna(), PATHINFO_COPY);
        return Is_var_overflow_candidate(rhs, sp_h, visited);
      }
    }
  }
  return FALSE;
}

// =============================================================================
// Is_overflow_checked
//   check if overflow is checked in control dependency between def and use
// TODO:
//   this function is problematic. I don't have a good idea about how to check
// if overflow is checked in program
// =============================================================================
BOOL
VSA::Is_overflow_checked(const hash_set<CODEREP*> &crs,
                         BB_NODE *def, BB_NODE *use, hash_set<IDTYPE>& visited) const
{
  if (use == def || use->Postdominates(def))
    return FALSE;

  BB_NODE *cd;
  BB_NODE_SET_ITER cd_iter;
  FOR_ALL_ELEM (cd, cd_iter, Init(use->Rcfg_dom_frontier())) {
    if (visited.find(cd->Id()) != visited.end())
      return TRUE;
    visited.insert(cd->Id());

    STMTREP *br = cd->Branch_stmtrep();
    if (br == NULL || br->Rhs() == NULL)
      continue;
    hash_set<CODEREP*>::const_iterator it;
    for (it = crs.begin(); it != crs.end(); ++it) {
      if (br->Rhs()->Contains(*it))
        continue;
      if (!Is_overflow_checked(crs, def, cd, visited))
        return FALSE;
    }
  }

  return TRUE;
}

// =============================================================================
// Scan_integer_overflow
//     check if result of tainted data arithmetic result used without overflow
// check
// =============================================================================
BOOL
VSA::Scan_integer_overflow(UINT32 tag, CODEREP *arith, CODEREP *cr, STMTREP *sr)
{
  Is_True(tag > 0 && cr != NULL && sr != NULL,
          ("bad tag, cr or sr"));

  if (sr->Linenum() == 0)
    return FALSE;

  switch (cr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return FALSE;
  case CK_IVAR:
    if (cr->Opr() == OPR_PARM) {
      Scan_integer_overflow(tag, arith, cr->Ilod_base(), sr);
    }
    // TODO: check ivar tainted
    return FALSE;
  case CK_OP:
    if (arith == NULL &&
        (cr->Opr() == OPR_ADD || cr->Opr() == OPR_SUB ||
         cr->Opr() == OPR_MPY || cr->Opr() == OPR_DIV)) {
      arith = cr;
    }
    for (INT i = 0; i < cr->Kid_count(); ++i) {
      if (Scan_integer_overflow(tag, arith, cr->Opnd(i), sr))
        return TRUE;
    }
    return FALSE;
  case CK_VAR:
    break;
  default:
    Is_True(FALSE, ("bad cr kind"));
    return FALSE;
  }

  Is_True(cr->Kind() == CK_VAR, ("not var"));

  // no ADD/SUB/MPY/DIV in the expr
  if (arith == NULL)
    return FALSE;

  // INT32-C only cares signed integer overflow
  // refine this for other INTxx rules
  if (!MTYPE_is_signed(cr->Dtyp()))
    return FALSE;

  // no zero version or volatile
  if (cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
      cr->Is_var_volatile())
    return TRUE;  // ignore whole expr

  Is_True(cr->Aux_id() != Opt_stab()->Default_vsym() &&
          cr->Aux_id() != Opt_stab()->Return_vsym(), ("def or ret vsym in real occ"));

  // check tainted
  TAG_OBJ_REP *tor = NULL;
  if (VSA_Enable_TAG_OLD) {
    TAGOKIND tag_kind;
    TOR_LIST_OLD *tor_list = Find_tor_list_from_cr(sr, cr, tag_kind);
    tor = tor_list ? tor_list->Find(tag) : NULL;
  } else {
    TOR_LIST *tor_list = Find_tor_list_from_cr(sr, cr, TRUE);
    tor = tor_list ? tor_list->Find(tag) : NULL;
  }
  if (tor == NULL)
    return FALSE;

  BB_NODE *def_bb = cr->Is_flag_set(CF_DEF_BY_PHI)
                      ? cr->Defphi()->Bb()
                      : cr->Defstmt()->Bb();
  // check if CD is check integer overflow
  if (def_bb != NULL && !sr->Bb()->Postdominates(def_bb)) {
    hash_set<IDTYPE> visited;
    hash_set<CODEREP*> crs;
    Comp_unit()->Vra()->Analyze_coderep_vars(arith, crs);
    if (crs.size() > 1 &&
        Is_overflow_checked(crs, sr->Bb(), def_bb, visited) == TRUE)
      return TRUE;
  }

  INT64 min = -INT64_MAX, max = INT64_MAX;
  BOOL is_bounded = Comp_unit()->Vra()->Get_bounds(cr, sr->Bb(), min, max);
  if (min != -INT64_MAX && max != INT64_MAX)
    return FALSE;

  AUX_STAB_ENTRY* aux = Opt_stab()->Aux_stab_entry(cr->Aux_id());
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "RBC INT32-C: cr%d sym%dv%d %s in sr%d %s LINE %d: [%lld, %lld].\n",
                   cr->Coderep_id(), cr->Aux_id(), cr->Version(),
                   aux->St() ? ST_name(aux->St()) : "-noname-",
                   sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4,
                   Srcpos_To_Line(sr->Linenum()),
                   min, max));

  SRCPOS_HANDLE srcpos_h(cr, sr, Dna(), Loc_pool(), this);
  hash_set<IDTYPE> visited;
  BOOL is_cand = Is_var_overflow_candidate(cr, &srcpos_h, visited);
  if (is_cand) {
    Rbc()->Report_rbc_error(this, sr, "INT32-C", IC_MAYBE, &srcpos_h, CERT_C_RULESET);
  }
  return is_cand;
}

// =============================================================================
// Scan_pointer_for_misra
//     MSR_18_2: Subtraction between pointers shall only be applied to pointers
// that address elements of the same array.
//     MSR_18_3: The relational operators >, >=, < and <= shall not be applied
// to objects of pointer type except where they point into the same project.
// =============================================================================
BOOL
VSA::Scan_pointer_for_misra(CODEREP *cr, STMTREP *sr)
{
  if (!VSA_Xsca)
    return FALSE;

  if (sr->Linenum() == 0)
    return FALSE;

  switch (cr->Kind()) {
  case CK_VAR:
  {
    AUX_STAB_ENTRY *aux = Comp_unit()->Opt_stab()->Aux_stab_entry(cr->Aux_id());
    if (TY_kind(cr->object_ty()) == KIND_POINTER ||
        (aux->Is_preg() && cr->Dtyp() == Pointer_Mtype)) {
      return TRUE;
    }
    return FALSE;
  }
  case CK_CONST:
  case CK_RCONST:
    return FALSE;
  case CK_IVAR:
    if (cr->Opr() == OPR_PARM) {
      if (Scan_pointer_for_misra(cr->Ilod_base(), sr))
        return TRUE;
    }
    else if (cr->Opr() == OPR_ILOAD || cr->Opr() == OPR_MLOAD) {
      CODEREP *base = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
      if (Scan_pointer_for_misra(base, sr))
        return TRUE;
      if (cr->Opr() == OPR_MLOAD) {
        if (Scan_pointer_for_misra(cr->Mload_size(), sr))
          return TRUE;
      }
    }
    return FALSE;
  case CK_OP:
    if (cr->Opr() == OPR_SUB ||
        cr->Opr() == OPR_GT || cr->Opr() == OPR_GE ||
        cr->Opr() == OPR_LT || cr->Opr() == OPR_LE) {
      CODEREP *opnd0 = cr->Opnd(0);
      CODEREP *opnd1 = cr->Opnd(1);
      if (Scan_pointer_for_misra(opnd0, sr) && Scan_pointer_for_misra(opnd1, sr)) {
        VSA_ADDRESS_INFO info0;
        Comp_unit()->Analyze_pointer_info(NULL, opnd0, &info0, FALSE);
        VSA_ADDRESS_INFO info1;
        Comp_unit()->Analyze_pointer_info(NULL, opnd1, &info1, FALSE);
        if (info0.Base() != info1.Base() &&
            (info0.Base() == NULL || info1.Base() == NULL ||
             Cr_2_heap_obj(info0.Base()) != Cr_2_heap_obj(info1.Base()))) {
          SRCPOS_HANDLE srcpos_h(cr, sr, Dna(), Loc_pool(), this);
          if (cr->Opr() == OPR_SUB) {
            Report_xsca_error(cr, (char*)NULL, "MSR_18_2", IC_DEFINITELY, &srcpos_h);
          } else {
            Report_xsca_error(cr, (char*)NULL, "MSR_18_3", IC_DEFINITELY, &srcpos_h);
          }
        }
      }
    } else {
      for (INT i = 0; i < cr->Kid_count(); ++i) {
        if (Scan_pointer_for_misra(cr->Opnd(i), sr))
          return TRUE;
      }
    }
    return FALSE;
  case CK_LDA:
    return TRUE;
  default:
    Is_True(FALSE, ("bad cr kind"));
    return FALSE;
  }
}

// =============================================================================
// Scan_abs_for_misra
//     MSR_D_4_11:  The validity of values passed to library functions
// shall be checked.
// This function is only checked for abs.
// =============================================================================
void
VSA::Scan_abs_for_misra(CODEREP *cr, STMTREP *sr)
{
  if (!VSA_Xsca)
    return;

  if (sr->Linenum() == 0)
    return;

  switch (cr->Kind()) {
  case CK_VAR:
  case CK_CONST:
  case CK_RCONST:
  case CK_LDA:
    break;
  case CK_IVAR:
    if (cr->Opr() == OPR_PARM) {
      Scan_abs_for_misra(cr->Ilod_base(), sr);
    }
    break;
  case CK_OP:
    if (cr->Opr() == OPR_ABS) {
      BOOL is_checked = false;
      CODEREP *opnd = cr->Opnd(0);
      BB_NODE *bb = sr->Bb();
      BB_NODE* cd;
      BB_NODE_SET_ITER cb_iter;
      FOR_ALL_ELEM(cd, cb_iter, Init(bb->Rcfg_dom_frontier())) {
        Is_True(cd->Succ() != NULL,
                ("cd bb does not have successors"));
        Is_True(cd->Succ()->Multiple_bbs(),
                ("succ of cc should be multiple bbs"));
        STMTREP *last_stmt = cd->Last_stmtrep();
        if (last_stmt != NULL) {
          OPERATOR opr = last_stmt->Opr();
          if (opr == OPR_TRUEBR || opr == OPR_FALSEBR) {
            if (Check_var_value(last_stmt, opnd, bb, INT_MIN)) {
              is_checked = true;
            }
            break;
          }
        }
      }
      if (!is_checked) {
        SRCPOS_HANDLE srcpos_h(cr, sr, Dna(), Loc_pool(), this);
        Report_xsca_error(cr, (char*)NULL, "MSR_D_4_11", IC_DEFINITELY, &srcpos_h);
      }
    } else {
      for (INT i = 0; i < cr->Kid_count(); ++i) {
        Scan_abs_for_misra(cr->Opnd(i), sr);
      }
    }
    break;
  default:
    Is_True(FALSE, ("VSA::Scan_abs_for_misra: bad cr kind"));
    break;
  }
}

BOOL
VSA::Check_var_value(STMTREP *sr, CODEREP *cr, BB_NODE *bb, INT64 lower_bound)
{
  OPERATOR opr = sr->Opr();
  Is_True((opr == OPR_TRUEBR || opr == OPR_FALSEBR),
          ("wrong operator for check_var_value"));
  CODEREP *cmp = sr->Rhs();
  if (cmp->Kind() == CK_OP && OPERATOR_is_compare(cmp->Opr())) {
    CODEREP *rhs = cmp->Opnd(1);
    CODEREP *lhs = cmp->Opnd(0);
    CODEREP *var = NULL;
    CODEREP *value = NULL;
    if (rhs->Kind() == CK_CONST) {
      value = rhs;
      var = lhs;
    } else if (lhs->Kind() == CK_CONST) {
      value = lhs;
      var = rhs;
    }
    if (cr->Kind() == CK_OP && (cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL))
      cr = cr->Opnd(0);
    if (var == cr && value->Kind() == CK_CONST) {
      INT64 val = value->Const_val();
      if ((opr == OPR_TRUEBR && sr->Label_number() == bb->Labnam()) ||
          (opr == OPR_FALSEBR && sr->Label_number() != bb->Labnam())) {
        if (cmp->Opr() == OPR_GT && val >= lower_bound) {
          return TRUE;
        } else if ((cmp->Opr() == OPR_GE || cmp->Opr() == OPR_EQ) &&
                   val > lower_bound) {
          return TRUE;
        }
        if (var->Kind() == CK_VAR && TY_mtype(var->Lod_ty()) == MTYPE_I4 &&
            cmp->Opr() == OPR_NE && val == lower_bound) {
          return TRUE;
        }
      }
    }
  }
  return FALSE;
}

// =============================================================================
//
//  Scan_rule_based_error - create a plug-in mechanism for rule-based check.
//       Such plug-in system provides a set of predefined interface to interact
//       with VSA system.  This enables community to develop new rules without
//       independent of the VSA core team involvement.
//
// =============================================================================
void
VSA::Scan_rule_based_error(BB_NODE *bb)
{
  if (!VSA_Rbc())
    return;

  if (bb == Cfg()->Entry_bb()) {
    if (Dna()->Is_set_rbc_flag(DNA_RBC_ASSERT_DNA)) {
      Ipsa()->Rbc()->Eval__mvsa_assert(Dna(), NULL, RBC_SE_ASSERT_DNA);
    }
  }
  // check redundant condition for this bb
  if (VSA_Rcd && Comp_unit()->Vra() && BB_has_real_stmtrep(bb)) {
    std::pair<CODEREP*, STMTREP*> cond = Comp_unit()->Vra()->Get_bb_cond(bb);
    // ignore redundant condition caused by loop-inversion
    if (cond.first && !Is_bb_loop_inverted(cond.second->Bb(), bb)) {
      SRCPOS_HANDLE sp_h(cond.first, cond.second, Dna(), Loc_pool());
      hash_set<IDTYPE> visited;
      visited.insert(cond.second->Bb()->Id());
      Check_redundant_condition(bb, cond.first, &sp_h, visited);
    }
  }

  const char *tag_name = VSA_Tainted_Int32C ? "tainted" : "int32-c";
  TAG_BASE *tag_base = Rbc()->Find_tag_base((char *) tag_name);
  UINT32 tag_id = tag_base ? tag_base->Id() : 0;

  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {

    if (tag_id > 0 && stmt->Rhs()) {
      Scan_integer_overflow(tag_id, NULL, stmt->Rhs(), stmt);
    }

    if (stmt->Rhs()) {
      Scan_pointer_for_misra(stmt->Rhs(), stmt);
      Scan_abs_for_misra(stmt->Rhs(), stmt);
    }

    OPERATOR opr = stmt->Opr();
    if (opr == OPR_MSTORE) {
      Scan_pointer_for_misra(stmt->Lhs()->Istr_base(), stmt);
      CODEREP *num_bytes = (CODEREP *)stmt->Lhs()->Mstore_size();
      Scan_pointer_for_misra(num_bytes, stmt);

      Check_mload_oob(stmt->Lhs(), stmt);
      if (stmt->Rhs()->Kind() == CK_IVAR && stmt->Rhs()->Opr() == OPR_MLOAD)
        Check_mload_oob(stmt->Rhs(), stmt);
      if (VSA_Xsca) {
        if (num_bytes != NULL && num_bytes->Kind() == CK_CONST) {
          CODEREP *tgt = stmt->Lhs();
          CODEREP *src = stmt->Rhs();
          UINT64 size = num_bytes->Const_val();
          if (Rbc()->Is_memory_overlap(tgt, size, src, stmt, Dna())) {
            SRCPOS_HANDLE sp_h(tgt, stmt, Dna(), Loc_pool(), this);
            Rbc()->Report_xsca_error(this, stmt->Linenum(), "MSR_19_1", IC_DEFINITELY, &sp_h);
          }
          if (!Rbc()->Is_memory_big_enough(tgt, sizeof(char), size, stmt, Dna())) {
            // some of the calls like memmove, memcpy are translated to
            // MSTORE here, we'd better report "AOB" instead of "MSR_21_18"
            // because we are not sure if it was a call.
            SRCPOS_HANDLE sp_h(tgt, stmt, Dna(), Loc_pool(), this);
            // Rbc()->Report_xsca_error(this, stmt->Linenum(), "MSR_21_18", &sp_h);
            Report_vsa_error(tgt, (char*)NULL, AOB, IC_DEFINITELY, &sp_h);
          }
        }
      }
    }
    else if (opr == OPR_STID) {
      if (VSA_Xsca) {
        // process lhs, check if it is a struct/union field
        // get field type, field id & field offset
        CODEREP *tgt = stmt->Lhs();
        TY_IDX tgt_ty = TY_IDX_ZERO;
        TY_IDX tgt_fld_ty = TY_IDX_ZERO;
        UINT tgt_fld_id = 0;
        UINT64 tgt_fld_ofst = 0;
        if (tgt->Kind() == CK_OP &&
            (tgt->Opr() == OPR_CVT || tgt->Opr() == OPR_TRUNC)) {
          tgt = tgt->Opnd(0);
        }
        if (tgt->Kind() == CK_VAR) {
          tgt_ty = tgt->Lod_ty();
          tgt_fld_id = tgt->Field_id();
        }
        if (tgt_ty == TY_IDX_ZERO || !Is_Structure_Type(tgt_ty)) {
          continue;
        }
        if (tgt_fld_id > 0) {
          UINT curr_id = 0;
          FLD_HANDLE tgt_fld_handle = FLD_get_to_field(tgt_ty, tgt_fld_id, curr_id);
          if (!tgt_fld_handle.Is_Null()) {
            tgt_fld_ty = FLD_type(tgt_fld_handle);
            tgt_fld_ofst = FLD_ofst(tgt_fld_handle);
          }
        }
        if (tgt_fld_ty == TY_IDX_ZERO) {
          continue;
        }
        // process rhs, check if it is a struct/union field
        // get field type, field id & field offset
        CODEREP *src = stmt->Rhs();
        TY_IDX src_ty = TY_IDX_ZERO;
        TY_IDX src_fld_ty = TY_IDX_ZERO;
        UINT src_fld_id = 0;
        UINT64 src_fld_ofst = 0;
        if (src->Kind() == CK_OP &&
            (src->Opr() == OPR_CVT || src->Opr() == OPR_TRUNC)) {
          src = src->Opnd(0);
        }
        if (src->Kind() == CK_VAR) {
          src_ty = src->Lod_ty();
          src_fld_id = src->Field_id();
        }
        if (src_ty == TY_IDX_ZERO || !Is_Structure_Type(src_ty)) {
          continue;
        }
        if (src_fld_id > 0) {
          UINT curr_id = 0;
          FLD_HANDLE src_fld_handle = FLD_get_to_field(src_ty, src_fld_id, curr_id);
          if (!src_fld_handle.Is_Null()) {
            src_fld_ty = FLD_type(src_fld_handle);
            src_fld_ofst = FLD_ofst(src_fld_handle);
          }
        }
        if (src_fld_ty == TY_IDX_ZERO) {
          continue;
        }
        // analyze, check if same offset of a same symbol is assigned with
        // value of different type
        ST *tgt_st = Opt_stab()->Aux_stab_entry(tgt->Aux_id())->St();
        ST *src_st = Opt_stab()->Aux_stab_entry(src->Aux_id())->St();
        if (tgt_st == src_st) {
          if (tgt_fld_ofst == src_fld_ofst && tgt_fld_ty != src_fld_ty) {
            SRCPOS_HANDLE sp_h(stmt->Lhs(), stmt, Dna(), Loc_pool(), this);
            Rbc()->Report_xsca_error(this, stmt->Linenum(), "MSR_19_1", IC_DEFINITELY, &sp_h);
          }
        }
      }
    }
    else if (opr == OPR_ISTBITS || opr == OPR_ISTORE) {
      Scan_pointer_for_misra(stmt->Lhs()->Istr_base(), stmt);
    }
    else if ( OPERATOR_is_call(opr)) { // Process RBC

      DNA_NODE *dna = Dna();
      RNA_NODE *rna = dna->Get_callsite_rna(stmt);

      INTRINSIC intrn = Get_call_intrinsic(stmt);
      if ((intrn == INTRN_MEMCPY || intrn == INTRN_STRNCPY) && VSA_Aob) {
        // temporary hack for AliOS release
        std::pair<CODEREP*, RNA_NODE*> arg1_ret, arg2_ret, arg3_ret;
        CODEREP *arg1 = rna->Get_arg(1);  // destination
        arg1_ret = Get_object_length(arg1, stmt, MIN_VALUE);
        CODEREP *arg1_sz = arg1_ret.first;
        CODEREP *arg2 = rna->Get_arg(2);  // source
        arg2_ret = Get_object_length(arg2, stmt, MIN_VALUE);
        CODEREP *arg2_sz = arg2_ret.first;
        arg3_ret = Eval_size_info(rna->Get_arg(3), stmt, MAX_VALUE);  // num_bytes
        CODEREP *arg3 = arg3_ret.first;
        if (intrn == INTRN_MEMCPY &&
            (VSA_Src_Tainted || arg2_sz != NULL) &&
            (arg2_ret.second == arg3_ret.second ||
             arg2_ret.second == NULL || arg3_ret.second == NULL) &&
            Comp_unit()->Vra() &&
            !Is_strlen_of(arg2, rna->Get_arg(3), FALSE)) {
          PATH_SELECTED path;
          VRA_RESULT res = VA_UNKNOWN;
          if (arg2_sz != NULL)
            res = (arg2_sz->Kind() != CK_CONST && arg2_sz->Kind() != CK_RCONST)
                             ? Comp_unit()->Vra()->Compare_cr<OPR_LT>(arg2_sz, bb, arg3, path)
                             : Comp_unit()->Vra()->Compare_cr<OPR_GT>(arg3, bb, arg2_sz, path);
          BOOL is_tainted = FALSE;
          CODEREP* arg3_ub = NULL;
          if (res == VA_UNKNOWN) {
            if (Comp_unit()->Vra()->Get_ub(rna->Get_arg(3), bb, arg3_ub) == FALSE) {
              // arg3 doesn't have a upper bound, check if arg3 is tainted or treat as 'tainted'
              CODEREP *arg2_base = Find_ilod_base(arg2);
              if (arg2_base == NULL)
                arg2_base = arg2;
              CODEREP *arg3_base = arg3->Kind() == CK_IVAR
                                     ? Find_ilod_base(arg3->Ilod_base()) : arg3;
              if (arg3_base == NULL)
                arg3_base = arg3;
              is_tainted = Is_value_tainted(arg3) ||
                           (VSA_Src_Tainted && Is_same_symbol(arg2_base, arg3_base));
            }
          }
          // assert: arg2_sz >= copy_limit
          if (res == VA_YES || res == VA_POSSIBLE || is_tainted) {
            // printf("RBC POC: memcpy overflow error\n");
            SRCPOS_HANDLE srcpos_h(arg2, stmt, Dna(), Loc_pool(), this);
#if 0
            const char* func_desc = "memcpy";
            const char* dest_desc = srcpos_h.Find_cr_stname(arg2, stmt, Dna());
            if (dest_desc == NULL) dest_desc = "<destination>";
            const char* dest_sz_desc = arg2_sz ? srcpos_h.Find_cr_stname(arg2_sz, stmt, Dna())
                                               : "<unknown>";
            if (dest_sz_desc == NULL) dest_sz_desc = "<dest length>";
            const char* copy_sz_desc = srcpos_h.Find_cr_stname(arg3, stmt, Dna());
            if (copy_sz_desc == NULL) copy_sz_desc = "<copy length>";
            srcpos_h.Add_message("%s() reads %s%s bytes to %s which is %s bytes",
                                 func_desc,
                                 copy_sz_desc, is_tainted ? " (tainted)" : "",
                                 dest_desc, dest_sz_desc);
#endif
            RNA_NODE* sz_rna = arg2_ret.second ? arg2_ret.second : arg3_ret.second;
            if (sz_rna) {
              srcpos_h.Append_data(sz_rna->Callstmt(),
                                   Ipsa()->Get_dna(sz_rna->Caller_idx()),
                                   PATHINFO_DNA_CALLSITE);
            }
            srcpos_h.Set_msgid("AOB.1");
            // report AOB instead of non-standard __MVSA_MEMCPY_OVERFLOW
            //Report_vsa_error(stmt, __MVSA_MEMCPY_OVERFLOW, res == VA_POSSIBLE, &srcpos_h);
            Report_vsa_error(arg2, (char*)NULL, AOB,
                             res == VA_YES ? IC_DEFINITELY : IC_MAYBE, &srcpos_h);
          }
        }

        // report a warning if arg2_sz isn't found
        if (arg2_sz == NULL) {
          Report_aob_if_no_size(arg2, stmt);
        }

        if (arg1_sz != NULL &&
            (arg1_ret.second == arg3_ret.second ||
             arg1_ret.second == NULL || arg3_ret.second == NULL) &&
            Comp_unit()->Vra()) {
          PATH_SELECTED path;
          VRA_RESULT res = (arg1_sz->Kind() != CK_CONST && arg1_sz->Kind() != CK_RCONST)
                             ? Comp_unit()->Vra()->Compare_cr<OPR_LT>(arg1_sz, bb, arg3, path)
                             : Comp_unit()->Vra()->Compare_cr<OPR_GT>(arg3, bb, arg1_sz, path);
          // assert: arg1_sz >= copy_limit
          if (res == VA_YES || res == VA_POSSIBLE) {
            // printf("RBC POC: memcpy overflow error\n");
            SRCPOS_HANDLE srcpos_h(arg1, stmt, Dna(), Loc_pool(), this);
#if 0
            const char* func_desc = intrn == INTRN_MEMCPY ? "memcpy" : "strncpy";
            const char* dest_desc = srcpos_h.Find_cr_stname(arg1, stmt, Dna());
            if (dest_desc == NULL) dest_desc = "<destination>";
            const char* dest_sz_desc = srcpos_h.Find_cr_stname(arg1_sz, stmt, Dna());
            if (dest_sz_desc == NULL) dest_sz_desc = "<dest length>";
            const char* copy_sz_desc = srcpos_h.Find_cr_stname(arg3, stmt, Dna());
            if (copy_sz_desc == NULL) copy_sz_desc = "<copy length>";
            srcpos_h.Add_message("%s() writes %s bytes to %s which is %s bytes",
                                 func_desc, copy_sz_desc, dest_desc, dest_sz_desc);
#endif
            RNA_NODE* sz_rna = arg1_ret.second ? arg1_ret.second : arg3_ret.second;
            if (sz_rna) {
              srcpos_h.Append_data(sz_rna->Callstmt(),
                                   Ipsa()->Get_dna(sz_rna->Caller_idx()),
                                   PATHINFO_DNA_CALLSITE);
            }
            srcpos_h.Set_msgid("AOB.1");
            // report AOB instead of non-standard __MVSA_MEMCPY_OVERFLOW
            //Report_vsa_error(stmt, __MVSA_MEMCPY_OVERFLOW, res == VA_POSSIBLE, &srcpos_h);
            Report_vsa_error(arg1, (char*)NULL, AOB,
                             res == VA_YES ? IC_DEFINITELY : IC_MAYBE, &srcpos_h);
          }
        }

        // report a warning if arg1_sz isn't found
        if (arg1_sz == NULL) {
          Report_aob_if_no_size(arg1, stmt);
        }

      }
      else if (intrn == INTRN_STRCPY && (VSA_Builtin_CertC || VSA_Aob)) {
        std::pair<CODEREP*, RNA_NODE*> arg1_ret, arg2_ret;
        arg1_ret = Get_object_length(rna->Get_arg(1), stmt, MIN_VALUE);
        CODEREP *arg1 = arg1_ret.first;
        arg2_ret = Get_string_length(rna->Get_arg(2), stmt, MAX_VALUE);
        CODEREP *arg2 = arg2_ret.first;
        if (arg1 != NULL && arg2 != NULL &&
            (arg1_ret.second == arg2_ret.second ||
             arg1_ret.second == NULL || arg2_ret.second == NULL) &&
            Comp_unit()->Vra() && VSA_Builtin_CertC) {
          PATH_SELECTED path;
          VRA_RESULT res = (arg1->Kind() != CK_CONST && arg1->Kind() != CK_RCONST)
                             ? Comp_unit()->Vra()->Compare_cr<OPR_LT>(arg1, bb, arg2, path)
                             : Comp_unit()->Vra()->Compare_cr<OPR_GT>(arg2, bb, arg1, path);
          if (res == VA_YES || res == VA_POSSIBLE) {
            SRCPOS_HANDLE srcpos_h(rna->Get_arg(1), stmt, Comp_unit()->Dna(), Loc_pool(), this);
#if 0
            const char* dest_desc = srcpos_h.Find_cr_stname(rna->Get_arg(1), stmt, Dna());
            if (dest_desc == NULL) dest_desc = "<destination>";
            const char* dest_sz_desc = srcpos_h.Find_cr_stname(arg1, stmt, Dna());
            if (dest_sz_desc == NULL) dest_sz_desc = "<dest length>";
            const char* src_desc = srcpos_h.Find_cr_stname(rna->Get_arg(2), stmt, Dna());
            if (src_desc == NULL) src_desc = "<source>";
            const char* src_sz_desc = srcpos_h.Find_cr_stname(arg2, stmt, Dna());
            if (src_sz_desc == NULL) src_sz_desc = "<src length>";
            srcpos_h.Add_message("strcpy() copies %s which is %s bytes to %s which is %s bytes",
                                 src_desc, src_sz_desc, dest_desc, dest_sz_desc);
#endif
            RNA_NODE* sz_rna = arg1_ret.second ? arg1_ret.second : arg2_ret.second;
            if (sz_rna) {
              srcpos_h.Append_data(sz_rna->Callstmt(),
                                   Ipsa()->Get_dna(sz_rna->Caller_idx()),
                                   PATHINFO_DNA_CALLSITE);
            }
            srcpos_h.Set_msgid("STR31C.1");
            //Report_vsa_error(stmt, __MVSA_STRCPY_OVERFLOW, res == VA_POSSIBLE, &srcpos_h);
            Rbc()->Report_rbc_error(this, stmt, "STR31-C", res == VA_POSSIBLE, &srcpos_h, CERT_C_RULESET);
          }
        }

        // report a warning if arg1_sz isn't found
        if (arg1 == NULL) {
          Report_aob_if_no_size(rna->Get_arg(1), stmt);
        }

      }
      else if (intrn == INTRN_MEMSET && (VSA_Builtin_CertC || VSA_Aob)) {
        CODEREP *arg1 = rna->Get_arg(1);
        std::pair<CODEREP*, RNA_NODE*> arg1_ret;
        arg1_ret = Get_object_length(arg1, stmt, MIN_VALUE);
        CODEREP *arg1_sz = arg1_ret.first;
        if (arg1_sz != NULL && Comp_unit()->Vra() && VSA_Builtin_CertC) {
          CODEREP *arg3 = rna->Get_arg(3);
          PATH_SELECTED path;
          VRA_RESULT res = (arg1_sz->Kind() != CK_CONST && arg1_sz->Kind() != CK_RCONST)
                             ? Comp_unit()->Vra()->Compare_cr<OPR_LT>(arg1_sz, bb, arg3, path)
                             : Comp_unit()->Vra()->Compare_cr<OPR_GT>(arg3, bb, arg1_sz, path);
          if (res == VA_YES || res == VA_POSSIBLE) {
            SRCPOS_HANDLE srcpos_h(arg1, stmt, Comp_unit()->Dna(), Loc_pool(), this);
#if 0
            const char* set_sz_desc = srcpos_h.Find_cr_stname(arg3, stmt, Dna());
            if (set_sz_desc == NULL) set_sz_desc = "<set length>";
            const char* dest_desc = srcpos_h.Find_cr_stname(arg1, stmt, Dna());
            if (dest_desc == NULL) dest_desc = "<destination>";
            const char* dest_sz_desc = srcpos_h.Find_cr_stname(arg1_sz, stmt, Dna());
            if (dest_sz_desc == NULL) dest_sz_desc = "<dest length>";
            srcpos_h.Add_message("memset() sets %s bytes on %s which is %s bytes",
                                 set_sz_desc, dest_desc, dest_sz_desc);
#endif
            if (arg1_ret.second) {
              srcpos_h.Append_data(arg1_ret.second->Callstmt(),
                                   Ipsa()->Get_dna(arg1_ret.second->Caller_idx()),
                                   PATHINFO_DNA_CALLSITE);
            }
            srcpos_h.Set_msgid("ARR38C.1");
            Rbc()->Report_rbc_error(this, stmt, "ARR38-C", res == VA_POSSIBLE, &srcpos_h);
          }
        }

        // report a warning if arg1_sz isn't found
        if (arg1_sz == NULL) {
          Report_aob_if_no_size(arg1, stmt);
        }
      }

      if (IPSA_insession() && rna->Is_flag_set(RBC_SE_ASSERT)) {
        Ipsa()->Rbc()->Eval__mvsa_assert(dna, rna, RBC_SE_ASSERT);
      }

      if (opr == OPR_CALL && OPERATOR_has_sym(opr)) {
        // RBC POC: hard-coded rules for printf/scanf
        const char* caller_name = ST_name(stmt->St());
        Is_True(caller_name != NULL, ("invalud call stmt without name"));
        if (strcmp(caller_name, "printf") == 0)
          Check_printf(dna, rna, -1, -1, 1, 2);
        else if (strcmp(caller_name, "fprintf") == 0)
          Check_printf(dna, rna, -1, -1, 2, 3);
        else if (strcmp(caller_name, "dprintf") == 0)
          Check_printf(dna, rna, -1, -1, 2, 3);
        else if (strcmp(caller_name, "sprintf") == 0)
          Check_printf(dna, rna, 1, -1, 2, 3);
        else if (strcmp(caller_name, "snprintf") == 0)
          Check_printf(dna, rna, 1, 2, 3, 4);
        else if (strcmp(caller_name, "asprintf") == 0)
          Check_printf(dna, rna, -1, -1, 2, 3);
        else if (strcmp(caller_name, "scanf") == 0)
          Check_scanf(dna, rna, 1, 2);
        else if (strcmp(caller_name, "fscanf") == 0)
          Check_scanf(dna, rna, 2, 3);
        else if (strcmp(caller_name, "sscanf") == 0)
          Check_scanf(dna, rna, 2, 3);
      }
    } else if (opr == OPR_RETURN) {
      if (!(VSA_Ral() && !PU_java_lang(Get_Current_PU())))
        continue;
      STMTREP *ret_stmt = stmt->Prev();
      if (ret_stmt == NULL)
        continue;
      Is_Trace(Tracing(), (TFile, "Check RAL from Return: "));
      Is_Trace_cmd(Tracing(), ret_stmt->Print(TFile));

      CODEREP *retv = ret_stmt->Rhs();
      if (retv != NULL) {
        AUX_ID auxid;
        if (retv->Kind() == CK_VAR) {  // for LDA check, need go one extra step
          STMTREP *defstmt = retv->Defstmt();
          CODEREP *retval = (defstmt)? defstmt->Rhs() : NULL;
          HEAP_OBJ_REP *hor = (retval)? Cr_2_heap_obj(retval) : NULL;
          if (hor && (hor->Heap_obj()->Kind() == RSC_KIND_LDA)) {
            auxid = ret_stmt->Rhs()->Aux_id();
            SRCPOS_HANDLE srcpos_h(retv, stmt, Dna(), Loc_pool());
            srcpos_h.Set_msgid("RAL.1");
            Report_vsa_error(retv, auxid, RAL, IC_DEFINITELY, &srcpos_h);
          }
        }
        else if (retv->Kind() == CK_LDA) {
          if (ST_sclass(retv->Lda_base_st()) == SCLASS_AUTO) {
            auxid = retv->Lda_aux_id();
            SRCPOS_HANDLE srcpos_h(retv, stmt, Dna(), Loc_pool());
            srcpos_h.Set_msgid("RAL.1");
            BOOL ret = Report_vsa_error(retv, auxid, RAL, IC_DEFINITELY, &srcpos_h);
            if (ret && VSA_Xsca) {
              const char* var_name = srcpos_h.Orig_stname() ?
                                     srcpos_h.Orig_stname() : auxid ? Sym_name(auxid) : "";
              Report_xsca_error(retv, var_name, "MSR_18_6", IC_DEFINITELY, &srcpos_h);
            }
          }
        }
      }
    }
  }

  // do the same for its dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Scan_rule_based_error(dom_bb);
}

// =============================================================================
//
//  Vsa_bb - do RNA process and DBF prep work according to pre-order traversal of
//           the dominator tree
//
// =============================================================================
void
VSA::Vsa_bb(BB_NODE *bb, const AUX_DEF_MAP* to_add, const AUX_DEF_MAP* to_upd,
            const AUX_SET_MAP* aux_set, MEM_POOL *def_bbs_pool)
{
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "%s VSA::Vsa_bb forward processing BB=%d\n", SBar, bb->Id()));

  // placing phi for new globals
  Is_True(VSA_Global_Muchi == FALSE || (to_add != NULL && to_upd != NULL),
          ("wrong to_add or to_upd"));
  if (VSA_Global_Muchi && (to_add->size() > 0 || to_upd->size())) {
    Update_bb_var_phi(bb, to_add, to_upd);
  }

  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    Vsa_stmt(stmt, bb, to_add, to_upd, aux_set, def_bbs_pool);
  }

  // placing phi for new globals
  if (VSA_Global_Muchi && (to_add->size() > 0 || to_upd->size() > 0)) {
    Update_bb_succ_var_phi(bb, to_add, to_upd);
  }

  // do the same for its dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Vsa_bb(dom_bb, to_add, to_upd, aux_set, def_bbs_pool);

  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "%s VSA::Vsa_bb backward processing BB=%d\n", SBar, bb->Id()));

  if (VSA_Global_Muchi && (to_add->size() > 0 || to_upd->size() > 0)) {
    FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
      if (stmt->Opr() == OPR_OPT_CHI ||
          ((stmt->Opr() == OPR_CALL || stmt->Opr() == OPR_ICALL))) {
        CHI_NODE* chi;
        CHI_LIST_ITER chi_iter;
        FOR_ALL_NODE (chi, chi_iter, Init(stmt->Chi_list())) {
          if (!chi->Live())
            continue;
          AUX_ID aux = chi->Aux_id();
          if (to_add->find(aux) != to_add->end() ||
              to_upd->find(aux) != to_upd->end()) {
            CODEREP* res = Opt_stab()->Pop_coderep(aux);
            Is_Trace(Tracing(),
                     (TFile, "Vsa_bb: stmt-chi-pop %s:%d cr%d:sym%dv%d\n",
                             OPERATOR_name(stmt->Opr()) + 4, stmt->Stmtrep_id(),
                             res->Coderep_id(), aux, res->Version()));
            Is_True(res == chi->RESULT(), ("coderep stack mismatch"));
          }
        }
      }
      else if (stmt->Opr() == OPR_STID) {
        AUX_ID aux = stmt->Lhs()->Aux_id();
        if (to_add->find(aux) != to_add->end() ||
            to_upd->find(aux) != to_upd->end()) {
          CODEREP* lhs = Opt_stab()->Pop_coderep(aux);
          Is_Trace(Tracing(),
                   (TFile, "Vsa_bb: stid-lhs-pop STID:%d cr%d:sym%dv%d\n",
                           stmt->Stmtrep_id(), lhs->Coderep_id(), aux, lhs->Version()));
          Is_True(lhs == stmt->Lhs(), ("coderep stack mismatch"));
        }
      }
    }

    PHI_LIST* phi_list = bb->Phi_list();
    if (phi_list != NULL && !phi_list->Is_Empty()) {
      PHI_NODE* phi;
      PHI_LIST_ITER phi_iter;
      FOR_ALL_NODE (phi, phi_iter, Init(phi_list)) {
        if (!phi->Live())
          continue;
        AUX_ID aux = phi->Aux_id();
        if (to_add->find(aux) != to_add->end() ||
            to_upd->find(aux) != to_upd->end()) {
          CODEREP* res = Opt_stab()->Pop_coderep(aux);
          Is_Trace(Tracing(),
                   (TFile, "Vsa_bb: phi-res-pop BB%d cr%d:sym%dv%d\n",
                           phi->Bb()->Id(), res->Coderep_id(), aux, res->Version()));
          Is_True(res == phi->RESULT(), ("coderep stack mismatch"));
        }
      }
    }
  }
}

// =============================================================================
//
// Dump_call_path:
//     append call path & print out the full path when reach root
//
// =============================================================================
void
VSA::Dump_call_path(DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h, hash_set<IDTYPE> &visited)
{
  RNODE_VECTOR *clby_list = dna->Clby_list();
  INT idx = 0;
  INT parent_idx = srcpos_h->Cur_idx();
  srcpos_h->Add_children(clby_list->size());
  SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
  for (INT i = VAR_INIT_ID; i < clby_list->size(); i++) {
    RNA_NODE *caller_rna = (*clby_list)[i];
    if (caller_rna == NULL)
      continue;
    if (visited.find(caller_rna->Rna_idx()) != visited.end())
      continue;
    visited.insert(caller_rna->Rna_idx());
    DNA_NODE *caller = Ipsa()->Get_dna(caller_rna->Caller_idx());
    if (caller == NULL)
      continue;
    if (caller->Non_functional())
      continue;
    STMTREP *call_stmt = caller_rna->Callstmt();
    srcpos_h->Set_cur_node(cur_node, idx);
    srcpos_h->Append_data(call_stmt, caller, PATHINFO_DNA_CALLSITE);
    idx++;
    Dump_call_path(caller, srcpos_h, visited);
  }
  if (dna->Is_root_entry()) {
    srcpos_h->Append_data(dna->St(), NULL, dna, PATHINFO_ST_DECLARE);
    Rbc()->Report_rbc_error(this, ST_Srcpos(*dna->St()), "SAP", FALSE, srcpos_h);
  }
  srcpos_h->Reset_cur_node(cur_node, parent_idx);
}

// =============================================================================
//
// Find_global_use_bb:
//     find global var used position & return an srcpos_handle points to
//
// =============================================================================
SRCPOS_HANDLE*
VSA::Find_global_use_bb(BB_NODE *bb, CODEREP *cr)
{
  SRCPOS_HANDLE *ret = NULL;
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    BOOL is_call = OPERATOR_is_call(stmt->Opr());
    CODEREP *rhs = stmt->Rhs();
    if (rhs != NULL) {
      if (Is_same_symbol(rhs, cr)) {
        ST *st = Rbc()->Get_cr_st(this, cr);
        ret = CXX_NEW(SRCPOS_HANDLE(Dna(), Loc_pool()), Loc_pool());
        ret->Append_data(st, NULL, Dna(), PATHINFO_ST_DECLARE);
        ret->Set_orig_stname(ST_name(st));
        ret->Append_data(stmt, Dna(), PATHINFO_VUL_SPOT);
        return ret;
      }
      if (rhs->Kind() == CK_OP) {
        for (INT i = 0; i < rhs->Kid_count(); i++) {
          CODEREP *opnd = rhs->Opnd(i);
          if (is_call && opnd->Kind() == CK_IVAR)
            opnd = opnd->Ilod_base();
          if (Is_same_symbol(opnd, cr)) {
            ST *st = Rbc()->Get_cr_st(this, cr);
            ret = CXX_NEW(SRCPOS_HANDLE(Dna(), Loc_pool()), Loc_pool());
            ret->Append_data(st, NULL, Dna(), PATHINFO_ST_DECLARE);
            ret->Set_orig_stname(ST_name(st));
            ret->Append_data(stmt, Dna(), PATHINFO_VUL_SPOT);
            return ret;
          }
          else if (opnd->Kind() == CK_OP) {
            for (INT j = 0; j < opnd->Kid_count(); j++) {
              CODEREP *nested = opnd->Opnd(j);
              if (Is_same_symbol(nested, cr)) {
                ST *st = Rbc()->Get_cr_st(this, cr);
                ret = CXX_NEW(SRCPOS_HANDLE(Dna(), Loc_pool()), Loc_pool());
                ret->Append_data(st, NULL, Dna(), PATHINFO_ST_DECLARE);
                ret->Set_orig_stname(ST_name(st));
                ret->Append_data(stmt, Dna(), PATHINFO_VUL_SPOT);
                return ret;
              }
            }
          }
        }
      }
    }
  }

  // do the same for its dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    ret = Find_global_use_bb(dom_bb, cr);
    if (ret != NULL)
      break;
  }
  return ret;
}

// =============================================================================
//
// Dump_global_access_path:
//     find global var used and print out the call path to root
//
// =============================================================================
void
VSA::Dump_global_access_path(void)
{
  DNA_NODE *dna = Dna();
  INT i;
  // global used within
  VNODE_VECTOR *glist = dna->Glob_list();
  for (i = VAR_INIT_ID; i < glist->size(); i++) {
    VAR_NODE *vnd = (*glist)[i];
    if (vnd != NULL) {
      CODEREP *cr = vnd->Cr();
      SRCPOS_HANDLE *srcpos_h = Find_global_use_bb(Cfg()->Entry_bb(), cr);
      if (srcpos_h != NULL) {
        hash_set<IDTYPE> visited;
        Dump_call_path(dna, srcpos_h, visited);
      }
    }
  }
  // global set within
  PNODE_VECTOR *grlist = dna->Rgvl_list();
  for (i = PDV_INIT_ID; i < grlist->size(); i++) {
    PDV_NODE *pdv = (*grlist)[i];
    if ((pdv != NULL) &&
        (pdv->Kind() & BY_GLOBALVAR)) {
      CODEREP *cr = pdv->Stmt()->Lhs();
      ST *st = Rbc()->Get_cr_st(this, cr);
      STMTREP *stmt = pdv->Stmt();
      SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(dna, Loc_pool()), Loc_pool());
      srcpos_h->Append_data(st, NULL, dna, PATHINFO_ST_DECLARE);
      srcpos_h->Set_orig_stname(ST_name(st));
      srcpos_h->Append_data(stmt, dna, PATHINFO_VUL_SPOT);
      hash_set<IDTYPE> visited;
      Dump_call_path(dna, srcpos_h, visited);
    }
  }
}

// =============================================================================
//
//  Perform__hor_unification - when multiple heap_obj_rep get copied onto one
//           pointer, we create Unified_list for these heap_obj_rep and apply
//           same operation as we perform Rename on heap_obj_rep
//  Note: this phase takes place before the VSA::Rename phase for unification
//        to take effect
//
// =============================================================================
void
VSA::Perform_hor_unification(BB_NODE *bb)
{
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "%s VSA::Perform_hor_unification, processing BB=%d\n", SBar, bb->Id()));

  CODEREP *cr;
  PHI_NODE *phi; 
  PHI_LIST_ITER phi_iter;

  //  iterate through each phi-node 
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    if (phi->Live()) {
      CODEREP      *phi_result = phi->RESULT();
      if (phi_result->Is_var_volatile()) continue;
      if (phi_result->Is_var_nodef()) continue;

      PHI_OPND_ITER phi_opnd_iter(phi);
      CODEREP      *opnd;
      BOOL          to_unify = FALSE;
      HEAP_OBJ_REP *last_hor = NULL;
      HEAP_OBJ_REP *cur_hor  = NULL;
      HEAP_OBJ_REP *phi_result_cur_hor = NULL;

      // iterate through each phi-operand to see if they carry different heap_obj
      FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
        cur_hor = Cr_2_heap_obj(opnd);
 
        // Check if unification is needed
        if (cur_hor && last_hor && last_hor->Heap_obj() != cur_hor->Heap_obj() && cur_hor != Null_hor()) {
          to_unify = TRUE;
        }

        if (cur_hor && last_hor == NULL && cur_hor != Null_hor()) {
          // All phi_result acquires a heap_obj_rep if it it's operand has one
          phi_result_cur_hor = cur_hor->Heap_obj()->Top_of_stack();
          Enter_cr_heap_obj_map(phi_result, phi_result_cur_hor, TRUE);

          last_hor = cur_hor;
        }
      }

      if (to_unify && phi_result_cur_hor) {
        HOR_LIST *ulist = CXX_NEW(HOR_LIST, _mem_pool);

        // append phi_result_cur_hor to ulist
        ulist->Append(CXX_NEW(HOR_NODE(phi_result_cur_hor), Mem_pool()));
        phi_result_cur_hor->Set_ulist(ulist);

        // go through the phi operand list to create the unified_list
        FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
          // get the hor of its operand, and append it's latest version into the ulist
          // at the same time, attach the ulist to each element of the elist
          if (opnd == phi_result)
            continue;

          cur_hor = Cr_2_heap_obj(opnd);
          if (cur_hor == NULL)
            continue;

          HEAP_OBJ_REP *latest_hor = cur_hor->Heap_obj()->Top_of_stack();
          if (latest_hor->Attr() == ROR_DEF_BY_PHI &&
              latest_hor->Phi_def()->Bb() == bb) {
            // replace phi result with cur_hor
            latest_hor = cur_hor;
          }
#if 0
          Is_True(cur_hor != latest_hor,
                ("VSA::Perform_hor_unification Operand's hor does not have a new version"));
#endif
          // union and find 
          if (ulist->Find(latest_hor) == NULL) {
            ulist->Append(CXX_NEW(HOR_NODE(latest_hor), Mem_pool()));
            latest_hor->Set_ulist(ulist);
          }
        }
        Is_Trace(Tracing(), (TFile, "HOR Unification for phi: "));
        Is_Trace_cmd(Tracing(), phi->Print(TFile));
        Is_Trace_cmd(Tracing(), ulist->Print(TFile));
      } // to_unify
    } // if phi->Live()
  } // for each phi
}

// =============================================================================
//
//  Update_ulist_w_ho_rename
//           While rename the heap_obj_rep, we also generate new version for 
//           the rest of the hor in the ulist, excluding the parameter passed in
//
//  Update_ulist_w_ho_rename_rev
//           Pop the rename stack to compensate the rename stack push occurred
//           in the original update
//
// =============================================================================
void
VSA::Update_ulist_w_ho_rename(HEAP_OBJ_REP *ho, HEAP_OBJ_REP *prev, STMTREP *stmt, ROR_ATTR attr)
{
  HOR_LIST     *hor_list = prev->Ulist();
  if (hor_list == NULL) return;  // there is no unified list for ho

  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "%s VSA::Update_ulist_w_ho_rename\n", SBar));

  HOR_LIST     *new_ulist = CXX_NEW(HOR_LIST, _mem_pool);
  HOR_LIST_ITER hor_list_iter;
  HEAP_OBJ_REP *cur_hor;

  FOR_ALL_NODE(cur_hor, hor_list_iter, Init(hor_list)) {
    Is_True(cur_hor != cur_hor->Next(), ("VSA::Update_ulist_w_ho_rename has circle"));
    HEAP_OBJ_REP *new_heap_obj_rep = NULL;
    // ho is the new hor after free call while prev is the previous hor before free()
    if (cur_hor == prev) {
      new_heap_obj_rep = ho;
    }
    // The ULIST for phi result contains the phi_result and its operand
    // The ULIST generated for free() should not replicate the original ULIST's
    // member if it contains the same Heap_obj()
    else if (cur_hor->Heap_obj() != prev->Heap_obj()) {
      new_heap_obj_rep = Clone_heap_obj(cur_hor, stmt->Bb(), _mem_pool);
      new_heap_obj_rep->Set_attr(attr);
      new_heap_obj_rep->Set_srcpos_node(stmt,
                                        Dna(),
                                        PATHINFO_PARM);
      new_heap_obj_rep->Set_stmt_def(stmt, Dna());

      IDTYPE version = new_heap_obj_rep->Gen_name(stmt);
      Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
               (TFile, "Update_ulist_w_ho_rename: push hor "));
      Is_Trace_cmd(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
                   new_heap_obj_rep->Print(TFile));
      Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
               (TFile, " in the ulist of "));
      Is_Trace_cmd(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
                   ho->Print(TFile));
      Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
               (TFile, "\n"));
      new_heap_obj_rep->Set_prev_ver(cur_hor);      // update due to pending free
    }
    if (new_heap_obj_rep) {
      new_ulist->Append(CXX_NEW(HOR_NODE(new_heap_obj_rep), Mem_pool()));
      new_heap_obj_rep->Set_ulist(new_ulist);
    }
  }

  Is_Trace_cmd(Tracing(), new_ulist->Print(TFile));
}

void
VSA::Update_ulist_w_ho_rename_rev(HEAP_OBJ_REP *hor, STMTREP *stmt)
{
  HOR_LIST     *hor_list = hor->Ulist();
  if (hor_list == NULL) {
    Is_True(hor->Heap_obj()->Top_match_sr(stmt),
            ("wrong hor on top of stack"));
    hor->Heap_obj()->Pop();
    return;
  }
  
  HOR_LIST_ITER hor_list_iter;
  HEAP_OBJ_REP *cur_hor;

  FOR_ALL_NODE(cur_hor, hor_list_iter, Init(hor_list)) {
    if (cur_hor->Heap_obj()->Top_match_sr(stmt))
      cur_hor->Heap_obj()->Pop();
  }
}


// =============================================================================
//
// Hor_same_ulist: return TURE if b is in a's ulist
//
// =============================================================================
BOOL
VSA::Hor_same_ulist(HEAP_OBJ_REP *a, HEAP_OBJ_REP *b) const
{
  if (a == NULL || b == NULL) return TRUE;
  if (a->Heap_obj() == b->Heap_obj()) return TRUE;

  HOR_LIST *hor_list = a->Ulist();
  HOR_LIST_ITER hor_list_iter;
  HEAP_OBJ_REP *cur_hor;

  FOR_ALL_NODE(cur_hor, hor_list_iter, Init(hor_list)) {
    if (cur_hor->Heap_obj() == b->Heap_obj())
      return TRUE;
  }
  return FALSE;
}


// =============================================================================
// VSA::Find_icall_defs
// =============================================================================
BOOL
VSA::Find_icall_defs(CODEREP* cr, BB_NODE* bb,
                     CALL_STACK& cs, ICALL_TARGET_VECTOR &defset, hash_set<IDTYPE> &visited)
{
  BOOL return_val = FALSE;
  if (cr->Is_flag_set(CF_IS_ZERO_VERSION)) {
    return FALSE;
  }
  else if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = cr->Defphi();
    if(visited.find(phi->Bb()->Id()) == visited.end()) {
      visited.insert(phi->Bb()->Id());
      PHI_OPND_ITER phi_opnd_iter(phi);
      CODEREP *opnd;
      FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
        // opnd->Print();
        return_val = Find_icall_defs(opnd, bb, cs, defset, visited);
        if(!return_val)
          return FALSE;
      }
    }
  }
  else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
    DNA_NODE* dna = Dna();
    IDTYPE param_id = dna->Is_param(cr);
    if(param_id != INVALID_VAR_IDX) {
      if(dna->Clby_list()->size() <= VAR_INIT_ID) {
        return FALSE;
      }
      RNA_NODE* call_site;
      return_val = TRUE;
      if (cs.empty()) {
        call_site = NULL;
      }
      else {
        call_site = cs.top();
        cs.pop();
      }
      for(INT i = VAR_INIT_ID; i < dna->Clby_list()->size(); i++) {
        RNA_NODE *rna = (*dna->Clby_list())[i];
        if (param_id >= rna->Arg_cnt())
          continue;
        if (call_site != NULL && call_site != rna)
          continue;
        DNA_NODE *caller = Ipsa()->Get_dna(rna->Caller_idx());
        if (caller == NULL || caller->Non_functional())
          continue;
        if (Ipsa()->Traved(rna))
          continue;
        Ipsa()->Set_trav(rna);

        STMTREP* call_stmt = rna->Callstmt();
        CODEREP* x = rna->Get_arg(param_id);
        CONTEXT_SWITCH context(caller);
        hash_set<IDTYPE> visited_bb2;
        cs.push(rna);
        INT size = cs.size();
        VSA *caller_vsa = caller->Comp_unit()->Vsa();
        return_val &= caller_vsa->Find_icall_defs(x, call_stmt->Bb(),
                                                  cs, defset, visited_bb2);
        Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
        cs.pop();

        if (call_site != NULL)
          break;
      }
      if (call_site != NULL)
        cs.push(call_site);
    }
    else {
      STMTREP *defstmt = cr->Get_defstmt();
      if (defstmt && OPERATOR_is_call(defstmt->Opr())) {
        RNA_NODE *rna = Dna()->Get_callsite_rna(defstmt);
        if(rna != NULL && rna->Uniq_callee() != INVALID_RNA_PU_IDX &&
           !Ipsa()->Traved(rna)) {
          Ipsa()->Set_trav(rna);
          cs.push(rna);
          INT size = cs.size();
          // new-rna TODO: iterate all possible callee
          if(cr->Kind() == CK_VAR) {
            DNA_NODE *callee = Ipsa()->Get_dna(rna->Uniq_callee());
            CONTEXT_SWITCH context(callee);
            vector<STMTREP*> stmts;
            callee->Collect_callee_return_value_for_xfa(Ipsa(), cr, rna, stmts);
            callee->Collect_callee_global_value_for_xfa(Ipsa(), cr, rna, stmts);
            VSA* callee_vsa = callee->Comp_unit()->Vsa();
            if(stmts.size() > 0) {
              return_val = TRUE;
              for(vector<STMTREP*>::const_iterator it = stmts.begin(); it != stmts.end(); it++) {
                STMTREP* stmt = *it;
                hash_set<IDTYPE> visited_bb2;
                return_val &= callee_vsa->Find_icall_defs(stmt->Rhs(), stmt->Bb(),
                                                          cs, defset, visited_bb2);
              }
            }
          }
          Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
          cs.pop();
        }
      }
    }
    // TODO:: del with function side effect
  }
  else {
    switch(cr->Kind()) {
    case CK_VAR:
    {
      if (cr->Is_var_volatile())
        break;
      STMTREP* stmt = cr->Get_defstmt();
      if(stmt->Opr() == OPR_STID)
        return Find_icall_defs(stmt->Rhs(), bb, cs, defset, visited);
      break;
    }
    case CK_IVAR:
    {
      VSYM_OBJ_REP* vor = Comp_unit()->Vsa()->Cr_2_vor(cr);
      if (vor != NULL) {
        return Find_icall_defs(cr, bb, vor, cs, defset, visited);
      }
      break;
    }
    case CK_LDA:
    {
      AUX_ID auxid = cr->Lda_aux_id();
      ST* func_st = Comp_unit()->Opt_stab()->Aux_stab_entry(auxid)->St();
      if(TY_kind(ST_type(func_st)) == KIND_FUNCTION) {
        // TODO:: need to consider multiple files
        ICALL_TARGET_VECTOR::const_iterator it = defset.begin();
        INT file_idx = Dna()->File_idx();
        for(;it != defset.end(); it++ ){
          if(it->first == file_idx && it->second == ST_st_idx(*func_st))
            break;
        }
        if( it == defset.end())
          defset.push_back(std::make_pair(file_idx, ST_st_idx(*func_st)));
        return TRUE;
      } else
        return FALSE;
      break;
    }
    case CK_CONST:
    case CK_OP:
      break;
    default:
      break;
    } // switch
  }
  return return_val;
}

BOOL
VSA::Find_icall_defs(CODEREP* cr, BB_NODE* bb, VSYM_OBJ_REP* vor,
                     CALL_STACK& cs, ICALL_TARGET_VECTOR &defset, hash_set<IDTYPE> &visited)
{
  switch(vor->Attr()) {
  case ROR_DEF_BY_CHI:
    {
      STMTREP *def_stmt = vor->Stmt_def();
      Is_True(vor->Is_entry_chi() || def_stmt, ("vsym object has no define statement"));
      if (vor->Is_entry_chi() ||
          (def_stmt && def_stmt->Opr() == OPR_OPT_CHI)) {
        if (def_stmt == NULL)  // TODO: this is a bug of vor creation/renaming
          break;
        CODEREP* ilod_base = Find_vor_chi_cr(def_stmt, vor);
        Is_True(ilod_base != NULL, ("not find vor chi cr"));
        if (ilod_base == NULL ||
            MTYPE_size_min(ilod_base->Dsctyp()) < MTYPE_size_min(Pointer_type) ||
            MTYPE_size_min(ilod_base->Dtyp()) < MTYPE_size_min(Pointer_type))
          break;
        return Find_callers_argument_for_icall_defs(ilod_base, vor->Vsym_obj()->Fld_rep_ptr(),
                                                    FOR_VSYM_BASE, cs, defset);
      }
      else if (def_stmt) {
        if (OPERATOR_is_call(def_stmt->Opr())) {
          CODEREP* ilod_base = Find_vor_chi_cr(def_stmt, vor);
          Is_True(ilod_base != NULL /*&& ilod_base->Opr() == OPR_PARM*/, ("not find vor chi cr"));
          if (ilod_base == NULL || ilod_base->Kind() != CK_VAR)
            break;
          RNA_NODE *rna = Sr_2_rna(def_stmt);
          if (rna == NULL)
            return FALSE;

          cs.push(rna);
          INT size = cs.size();
          BOOL ret = Find_callees_side_effects_for_icall_defs(ilod_base/*->Ilod_base()*/, vor->Vsym_obj()->Fld_rep_ptr(),
                                                              FOR_VSYM_BASE, cs, defset);
          Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
          cs.pop();
          return ret;
        }
        else if (def_stmt->Rhs()) {
          return Find_icall_defs(def_stmt->Rhs(), def_stmt->Bb(), cs, defset, visited);
        }
      }
    }
    break;
  case ROR_DEF_BY_PHI:
  case ROR_DEF_BY_HORPHI:
    {
      // visit guard
      PHI_NODE  *phi = vor->Phi_def();
      BB_NODE *curbb = phi->Bb();
      if (visited.find(curbb->Id()) != visited.end())
        return FALSE;
      visited.insert(curbb->Id());
      BOOL ret = TRUE;
      PHI_OPND_ITER phi_opnd_iter(phi);
      CODEREP      *opnd;
      INT32         opnd_idx = 0;
      FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
        if (!Is_path_possible(bb, phi, opnd_idx))
          continue;
        VSYM_OBJ_REP *vor_opnd = (VSYM_OBJ_REP*) opnd;
        ret &= Find_icall_defs(cr, bb, vor_opnd, cs, defset, visited);
        ++ opnd_idx;
      }
      return ret;
    }
    break;
  case ROR_DEF_BY_ISTORE:
  case ROR_DEF_BY_COPY:
    {
      STMTREP* def = vor->Stmt_def();
      if (def == NULL)
        break;
      return Find_icall_defs(def->Rhs(), bb, cs, defset, visited);
    }
    break;
  case ROR_DEF_BY_NONE:
    //Is_True(FALSE, ("def by none?"));
    break;
  case ROR_DEF_BY_NULL:
    // TODO: refine with iterative HO/VO phase
    break;
  default:
    Is_True(FALSE, ("TODO: handle vor def by: %d", vor->Attr()));
    break;
  }
  return FALSE;
}

BOOL
VSA::Find_callers_argument_for_icall_defs(CODEREP *base, VSYM_FLD_REP* vfr, ILODSTORBASE kind, CALL_STACK& cs, ICALL_TARGET_VECTOR& defset)
{
  Is_True(base->Kind() == CK_VAR, ("TODO: not LDID"));

  AUX_STAB_ENTRY* aux = Opt_stab()->Aux_stab_entry(base->Aux_id());
  if (aux->St() == NULL ||
      ST_sclass(aux->St()) == SCLASS_AUTO)
    return FALSE;

  //this is formal, assume it can be generic type with pointer size, no need to be pointer or ptr to struct
  Is_True(MTYPE_size_min(base->Dsctyp()) == MTYPE_size_min(Pointer_type) &&
          MTYPE_size_min(base->Dtyp()) == MTYPE_size_min(Pointer_type), ("type size mismatch"));
  //Is_True(TY_kind(base->Lod_ty()) == KIND_POINTER &&
  //        TY_kind(TY_pointed(base->Lod_ty())) == KIND_STRUCT, ("not pointer to struct param"));
  RNA_NODE* call_site;
  if (cs.empty()) {
    call_site = NULL;
  }
  else {
    call_site = cs.top();
    cs.pop();
  }

  BOOL ret = FALSE;
  DNA_NODE* dna = Dna();
  IDTYPE arg = dna->Is_param(base);
  UINT32 file_idx = dna->File_idx();
  UINT32 st_idx = INVALID_VAR_IDX;
  if (arg == INVALID_VAR_IDX) {
    Is_True(aux->St() != NULL &&
            ST_sclass(aux->St()) != SCLASS_FORMAL &&
            ST_sclass(aux->St()) != SCLASS_FORMAL_REF, ("not global var"));
    st_idx = ST_st_idx(aux->St());
    if (ST_sclass(aux->St()) == SCLASS_EXTERN) {
      WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
      if (mgr != NULL)
        mgr->Resolve(file_idx, st_idx, file_idx, st_idx);
    }
  }

  for (INT i = VAR_INIT_ID; i < dna->Clby_list()->size(); ++i) {
    RNA_NODE* rna = (*dna->Clby_list())[i];
    if (call_site != NULL && call_site != rna)
      continue;
    DNA_NODE *caller = Ipsa()->Get_dna(rna->Caller_idx());
    if (caller == NULL || caller->Non_functional())
      continue;
    if (Ipsa()->Traved(rna))
      continue;
    Ipsa()->Set_trav(rna);

    {
      CONTEXT_SWITCH context(caller);
      VSA* caller_vsa = caller->Comp_unit()->Vsa();
      CODEREP* cr;
      VSYM_OBJ_REP* vor;
      if (arg != INVALID_VAR_IDX && arg <= rna->Arg_cnt()) {
        cr = rna->Get_arg(arg);
        Is_True(cr != NULL, ("not find arg %d in %s -> %s",
                             arg, dna->Fname(), caller->Fname()));
        if (cr)
          vor = caller_vsa->Find_vor_mu_vor(rna->Callstmt(), cr, vfr);
      }
      else {
        MU_NODE *mnode = caller_vsa->Find_vor_mu(rna->Callstmt(), file_idx, st_idx, vfr);
        if (mnode == NULL)
          continue;
        Is_True(mnode != NULL, ("not find global var %s in %s -> %s",
                                ST_name(file_idx, st_idx), dna->Fname(), caller->Fname()));
        CVOR* cvor = (CVOR*)mnode->OPND();
        cr = cvor->second;
        vor = cvor->first;
      }
      if (vor != NULL) {
        cs.push(rna);
        INT size = cs.size();
        hash_set<IDTYPE> visited;
        ret = caller_vsa->Find_icall_defs(cr, rna->Callstmt()->Bb(), vor,
                                          cs, defset, visited);
        Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
        cs.pop();
        if (call_site != NULL)
          break;
      }
    }
  }

  if (call_site != NULL)
    cs.push(call_site);
  return ret;
}

BOOL
VSA::Find_callees_side_effects_for_icall_defs(CODEREP *x, VSYM_FLD_REP* vfr, ILODSTORBASE kind, CALL_STACK& cs, ICALL_TARGET_VECTOR &defset)
{
  Is_True(!cs.empty(), ("no call stack found"));
  RNA_NODE* rna = cs.top();
  // 'this' is caller's VSA
  Is_True(x->Kind() == CK_VAR,
          ("Only var is allowed"));

  // TODO
  if (rna->Uniq_callee() == INVALID_RNA_PU_IDX)
    return FALSE;

  // skip the rna if it has already visited
  if (Ipsa()->Traved(rna))
    return FALSE;
  Ipsa()->Set_trav(rna);

  DNA_NODE* callee = Ipsa()->Get_dna(rna->Uniq_callee());
  if (callee->Non_functional())
    return FALSE;

  AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(x->Aux_id());
  if (aux_entry->St() != NULL &&
      ST_sclass(aux_entry->St()) != SCLASS_REG &&
      ST_sclass(aux_entry->St()) != SCLASS_AUTO &&
      ST_sclass(aux_entry->St()) != SCLASS_FORMAL &&
      ST_sclass(aux_entry->St()) != SCLASS_FORMAL_REF) {
    // global var
    UINT32 file_idx = Dna()->File_idx();
    ST_IDX st_idx = ST_st_idx(aux_entry->St());
    if (ST_sclass(aux_entry->St()) == SCLASS_EXTERN) {
      WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
      if (mgr != NULL)
        mgr->Resolve(file_idx, st_idx, file_idx, st_idx);
    }

    CONTEXT_SWITCH context(callee);
    VSA* callee_vsa = callee->Comp_unit()->Vsa();

    STMTR_VECTOR::const_iterator it;
    BOOL ret = TRUE;
    for (it = callee->Rets_list()->begin(); it != callee->Rets_list()->end(); ++ it) {
      STMTREP* stmt = *it;
      MU_NODE* mnode = callee_vsa->Find_vor_mu(stmt, file_idx, st_idx, vfr);
      if (mnode != NULL) {
        CVOR* cvor = (CVOR*)mnode->OPND();
        Is_True(cvor->first != NULL && cvor->second != NULL, ("invalid cvor"));
        if (cvor->first != NULL && cvor->second != NULL) {
          hash_set<IDTYPE> visited;
          ret &= callee_vsa->Find_icall_defs(cvor->second, stmt->Bb(), cvor->first, cs, defset, visited);
        }
      }
    }
    return ret;
  }
  else {
    std::vector<STMTREP*> stmts;
    stmts.reserve(callee->Retv_list()->size() + callee->Rgvl_list()->size() / 4);
    callee->Collect_callee_return_value_for_xfa(Ipsa(), x, rna, stmts);

    if (stmts.size() == 0)
      return FALSE;

    CONTEXT_SWITCH context(callee);
    BOOL ret = TRUE;
    for (INT i = 0; i < stmts.size(); ++i) {
      STMTREP* stmt = stmts[i];
      Is_True(stmt->Opr() == OPR_STID, ("TODO: not stid"));
      STMTREP* ret_stmt = stmt->Next();
      Is_True(ret_stmt != NULL &&
              (ret_stmt->Opr() == OPR_RETURN || ret_stmt->Opr() == OPR_RETURN_VAL),
              ("stmt next is not return"));
      if (ret_stmt == NULL)
        continue;
      VSA* callee_vsa = callee->Comp_unit()->Vsa();
      VSYM_OBJ_REP* vor = callee_vsa->Find_vor_mu_vor(ret_stmt, stmt->Lhs(), vfr);
      if (vor == NULL)
        continue;
      Is_True(vor != NULL, ("not find vor"));
      hash_set<IDTYPE> visited;
      ret &= callee_vsa->Find_icall_defs(stmt->Lhs(), ret_stmt->Bb(), vor, cs, defset, visited);
    }
    return ret;
  }
}

BOOL
VSA::Find_vfunc(CODEREP* cr, BB_NODE* bb, INT32 ofst, ICALL_TARGET_VECTOR& vfunc, hash_set<IDTYPE>& visited, CALL_STACK& cs)
{
  Is_Trace(Tracing(), (TFile, "Find vfunc for call %s, cr %d\n", Dna()->Fname(), cr->Coderep_id()));
  if (cr->Kind() != CK_VAR) {
    // TODO: handle thunk for C++, which may adjust this by fixed offset or offset from vtable
    Find_vfunc_from_type(cr->object_ty(), ofst, vfunc);
    return FALSE;
  }
  if (cr->Kind () == CK_VAR) {
    ST* st = Comp_unit()->Opt_stab()->St (cr->Aux_id());
    // for extern globals like java.system.stdout, stderr, get from type
    if (st && ST_sclass(st) == SCLASS_EXTERN && Vsa_check_var_no_sideffect(ST_name(st))) {
      return Find_vfunc_from_type(cr->object_ty(), ofst, vfunc);
    }
  }

  if (cr->Is_flag_set(CF_IS_ZERO_VERSION)) {
    Find_vfunc_from_type(cr->object_ty(), ofst, vfunc);
    return FALSE;
  }

  BOOL ret = TRUE;
  if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = cr->Defphi();
    if (visited.find(phi->Bb()->Id()) != visited.end())
      return TRUE;
    visited.insert(phi->Bb()->Id());
    PHI_OPND_ITER phi_opnd_iter(phi);
    CODEREP *opnd;
    INT i = 0;
    FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
      if (Is_path_possible(bb, phi, i)) {
        ret &= Find_vfunc(opnd, bb, ofst, vfunc, visited, cs);
      }
      ++ i;
    }
    return ret;
  }
  else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP *defstmt = cr->Get_defstmt();
    if (defstmt->Opr() == OPR_OPT_CHI) {
      // TODO: EH handler here
      DNA_NODE* dna = Dna();
      IDTYPE param_id = dna->Is_param(cr);
      if (param_id != INVALID_VAR_IDX) {
        // input param
        // ATTENTION: the call-by list may be imcomplete
        if(dna->Clby_list()->size() <= 1) {
          return FALSE;
        }
        RNA_NODE *call_site;
        if(cs.empty()) {
          call_site = NULL;
        } else {
          call_site = cs.top();
          cs.pop();
        }
        for (INT i = VAR_INIT_ID; i < dna->Clby_list()->size(); i++) {
          RNA_NODE *rna = (*dna->Clby_list())[i];
          if (param_id > rna->Arg_cnt())
            continue;
          if (call_site != NULL && call_site != rna)
            continue;
          DNA_NODE *caller = Ipsa()->Get_dna(rna->Caller_idx());
          if (caller == NULL || caller->Non_functional())
            continue;
          if (Ipsa()->Traved(rna))
            continue;
          Ipsa()->Set_trav(rna);
          STMTREP* call_stmt = rna->Callstmt();
          CODEREP* x = rna->Get_arg(param_id);
          Is_True(x != NULL, ("not find the actual, wrong caller?"));
          if (x == NULL)
            continue;
          cs.push(rna);
          INT size = cs.size();
          CONTEXT_SWITCH context(caller);
          hash_set<IDTYPE> visited2;
          Is_Trace(Tracing(), (TFile, "Define by caller: %s\n", caller->Fname()));
          ret &= caller->Comp_unit()->Vsa()->Find_vfunc(x, call_stmt->Bb(), ofst, vfunc, visited2, cs);

          Is_Trace(Tracing(), (TFile, "End define by caller:%s->%s\n", caller->Fname(), Dna()->Fname()));
          Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
          cs.pop();
          if(call_site != NULL)
            break;
        }
        if(call_site != NULL)
          cs.push(call_site);
        return ret;
      }
      else {
        // TODO: check global var
        // TODO: ATTENTION: the call-by list may be imcomplete
      }
    }
    else if (OPERATOR_is_call(defstmt->Opr())) {
      // object comes from obj creation
      if (Find_vfunc_from_creation(defstmt, cr, ofst, vfunc))
        return TRUE;

      // java type casting routine: _Jv_CheckCast (Find the class constant)
      if(PU_java_lang(Get_Current_PU()) &&
        defstmt->Opr() == OPR_INTRINSIC_CALL &&
        defstmt->Rhs()->Intrinsic() == INTRN_CHECK_CAST) {
        CODEREP *lda = defstmt->Rhs()->Opnd(0)->Ilod_base();
        // Check cast first param would be LDA if we could locate the param's class symbol
        // If we cannot determine its class symbol, the param points to a UTF-8 String,
        // with gcj class symbol name.

        // Often, this includes cases like:
        // array of array ... of (primitive types or classes) is present or
        // array of primitive types is present.
        // See MangleTool.java -> mangleForClassConstant
        // e.g.
        // float[][] would be "[[f",
        // java.lang.Object -> "java.lang.Object"
        // java.lang.Exception[] -> "[Ljava.lang.Exception"
        if (lda->Kind() != CK_LDA)
          return FALSE;

        // FIXME: TODO: Find the class symbol from the UTF-8 class signature string (dyn-link) or
        // FIXME: TODO: calculate V-Table's entries in case of arrays

        // LDA points to a symbol table of the class (type) to be passed,
        // Resolve the vfunc from the V-Table there.
        ST *st = lda->Lda_base_st();
        if(st && ST_is_class_symbol(st)) {
          return Find_vfunc_from_type(ST_vtable_ty_idx(st), ofst, vfunc);
        }
      }

      // return value or side effect for a call
      // TODO: handle multiple ICALL targets
      RNA_NODE *rna = Dna()->Get_callsite_rna(defstmt);
      ret = FALSE;

      if (rna != NULL && !Ipsa()->Traved(rna) &&
          rna->Uniq_callee() != INVALID_RNA_PU_IDX) {
        Ipsa()->Set_trav(rna);
        cs.push(rna);
        INT size = cs.size();
        if(cr->Kind() == CK_VAR) {
          DNA_NODE *callee = Ipsa()->Get_dna(rna->Uniq_callee());
          vector<STMTREP*> stmts;
          CONTEXT_SWITCH context(callee);
          callee->Collect_callee_return_value_for_xfa(Ipsa(), cr, rna, stmts);
          callee->Collect_callee_global_value_for_xfa(Ipsa(), cr, rna, stmts);
          if(stmts.size() > 0) {
            vector<STMTREP*>::const_iterator it;
            ret = TRUE;
            for (it = stmts.begin(); it != stmts.end(); ++it) {
              STMTREP* stmt = *it;
              hash_set<IDTYPE> visited2;
              Is_Trace(Tracing(), (TFile, "By callee\n"));
              ret &= callee->Comp_unit()->Vsa()->Find_vfunc(stmt->Rhs(), stmt->Bb(), ofst, vfunc, visited2, cs);
              Is_Trace(Tracing(), (TFile, "\nEnd By callee: %s->%s\n", callee->Fname(), Dna()->Fname()));
            }
          }
        }
        Is_True(cs.size() == size && cs.top() == rna, ("call stack corrupted"));
        cs.pop();
      }
      if (ret && rna->Is_resolved())
        return TRUE;
      else
        return FALSE;
    }
  }
  else {
    STMTREP* def = cr->Get_defstmt();
    if (def && def->Opr() == OPR_STID) {
      return Find_vfunc(def->Rhs(), bb, ofst, vfunc, visited, cs);
    }
    else {
      // TODO: ivar?
      Find_vfunc_from_type(cr->object_ty(), ofst, vfunc);
      return FALSE;
    }
  }
  return FALSE;
}

BOOL
VSA::Find_vfunc_from_creation(STMTREP* call, CODEREP* obj, INT32 ofst, ICALL_TARGET_VECTOR& vfunc)
{
  if (call->Callee_returns_new_heap_memory())
    return FALSE;

  char *ty_name = NULL;
  if ((PU_src_lang(Get_Current_PU()) & PU_CXX_LANG)) {
    CODEREP *call_return_store = Comp_unit()->Find_return_value(call);
    STMTREP *next_stmt = NULL;
    if(call_return_store) {
      next_stmt = call->Next();
      if(next_stmt != NULL && next_stmt->Opr() == OPR_STID &&
         next_stmt->Rhs() == call_return_store) {
        CODEREP *lhs = next_stmt->Lhs();
        ID_ST_MAP *vtbl_map = Dna()->Vtbl_map();
        if(vtbl_map->find(lhs->Coderep_id()) != vtbl_map->end()) {
          ST_IDX& vtbl_st = (*vtbl_map)[lhs->Coderep_id()];
          Is_True(ST_is_vtable(&St_Table[vtbl_st]), ("find vfun: st is not vtable"));
          if (!ST_is_vtable(&St_Table[vtbl_st]))
            return FALSE;
          TY_IDX class_ty = ST_vtable_ty_idx(vtbl_st);
          ty_name = TY_name(class_ty);
        }
        else {
          Is_Trace(Tracing(), (TFile, ("C++ failed to find vfunc")));
          return FALSE;
        }
      }
    }
    // TODO, search the next stmt for ISTORE to object at ofst 0 or constructor
  }
  else if ((PU_src_lang(Get_Current_PU()) & PU_JAVA_LANG)) {
    // get from class ST
    if (call->Opr() == OPR_INTRINSIC_CALL &&
        call->Rhs()->Intrinsic() == INTRN_ALLOC_OBJ) {
      Is_True(call->Rhs()->Kid_count() == 1,
              ("invalid parms for _Jv_AllocObjectNoFinalizer"));
      Is_True(call->Rhs()->Opnd(0)->Opr() == OPR_PARM,
              ("param 0 is not PARM"));
      CODEREP* cls_lda = call->Rhs()->Opnd(0)->Ilod_base();
      Is_True(cls_lda->Kind() == CK_LDA,
              ("param 0 is not LDA"));
      ST* cls_st = cls_lda->Lda_base_st();
      Is_True(strcmp(TY_name(ST_type(cls_st)), "java.lang.Class") == 0,
              ("param 0 is not java.lang.Class"));
      Is_True(ST_is_class_symbol(cls_st),
              ("param 0 is not a class symbol"));
      if (!ST_is_class_symbol(cls_st))
        return FALSE;
      ty_name = TY_name(ST_vtable_ty_idx(cls_st));
    }
  }

  if(ty_name) {
    CLASS_HIERARCHY* hierarchy = Ipsa()->Glob_cha();
    Is_True(hierarchy != NULL, ("Class hierarchy not built"));
    VIRFUNC_INFO* vi = hierarchy->Get_vtable_entry(ty_name, ofst, 0);
    if (vi != NULL) {
      ST *fun_st = St_ptr(vi->_file_idx, vi->_fun_st);
      Is_True(ST_class(fun_st) == CLASS_FUNC, 
              ("Find vfunc from creation: found virtual fun st is not a function symbol"));
      vfunc.push_back(std::make_pair(vi->_file_idx, vi->_fun_st));
      Is_Trace(Tracing(), (TFile, "Find vfunc from creation: %s+%d", ty_name, ofst));
      return TRUE;
    }
  }
  return FALSE;
}

BOOL
VSA::Find_vfunc_from_type(TY_IDX ty, INT32 ofst, ICALL_TARGET_VECTOR& vfunc)
{
  TY_IDX ty_pointed = ty;
  if(TY_kind(ty) == KIND_POINTER)
    ty_pointed = TY_pointed(ty);
  if (TY_kind(ty_pointed) != KIND_STRUCT)
    return FALSE;

  CLASS_HIERARCHY* hierarchy = Ipsa()->Glob_cha();
  Is_True(hierarchy != NULL,
          ("Class hierarchy not built"));

  VIRFUNC_INFO_VEC* vec = hierarchy->Find_candidate_functions(TY_name(ty_pointed), ofst, TRUE);
  if (vec == NULL || vec->size() == 0)
    return FALSE;

  if (VSA_Devirt_Aggr == FALSE && vec->size() > VSA_Devirt_Aggr_Threshold)
    return FALSE;

  VIRFUNC_INFO_VEC::iterator it;
  for (it = vec->begin(); it != vec->end(); ++it) {
    VIRFUNC_INFO* func = *it;
    Is_True(func != NULL, ("null vfunc entry in class hierarchy"));
    ST *fun_st = St_ptr(func->_file_idx, func->_fun_st);
    Is_True(ST_class(fun_st) == CLASS_FUNC, 
            ("Find vfun: found virtual fun st is not a function symbol"));
    vfunc.push_back(std::make_pair(func->_file_idx, func->_fun_st));
  }
  Is_Trace(Tracing(), (TFile, "Find vfunc from type %s+%d\n", TY_name(ty_pointed), ofst));
  return TRUE;
}

// =============================================================================
// VSA::Vor_access_whole_ho()
// check if cr/vor with ANY FLD_REP is in a loop and access whole base heap obj
// return value: -1 = unknown, 0 = not whole ho, 1 = whole ho
// =============================================================================
INT
VSA::Vor_access_whole_ho(STMTREP *sr, CODEREP *cr, VSYM_OBJ_REP *vor)
{
  Is_True(sr != NULL, ("bad sr"));
  Is_True(cr && cr->Kind() == CK_IVAR && cr->Opr() != OPR_PARM, ("bad cr"));
  Is_True(vor != NULL && vor->Vsym_obj()->Fld_rep().Is_any(), ("bad vor"));

  VSA_ACCESS_INFO info;
  if (Comp_unit()->Analyze_access_info(sr, cr, &info) == FALSE) {
    return -1;
  }

  CODEREP *sz_cr = vor->Vsym_obj()->Base_hor()->Heap_obj()->Byte_size();
  if (sz_cr == NULL) {
    return -1;
  }

  if (sz_cr->Kind() == CK_CONST) {
    INT64 lb_val = info.Lower_bound_value();
    if (lb_val == INT64_MAX)
      return -1;
    INT64 ub_val = info.Upper_bound_value();
    if (ub_val == INT64_MAX)
      return -1;
    INT64 scale_val = info.Scale_value();
    if (scale_val == INT64_MAX)
      return -1;
    lb_val = lb_val * scale_val - info.Fix_ofst();
    ub_val = ub_val * scale_val - info.Fix_ofst();
    if (lb_val <= 0 && ub_val >= sz_cr->Const_val()) {
      return info.Is_store() && info.Exit_early() ? 0 : 1;;
    }
    return 0;
  }
  // TODO: variable length ho
  return -1;
}


// =============================================================================
// VSA::VSA
// =============================================================================
VSA::VSA( COMP_UNIT *cu, IPSA *ipsa, MEM_POOL* gpool, MEM_POOL* lpool):
  _cu(cu), _opt_stab(cu->Opt_stab()), _htable(cu->Htable()), _cfg(cu->Cfg()),
  _ipsa_mgr(ipsa), _mem_pool(gpool), _loc_pool(lpool), _filt_handler(gpool),
  _cr_2_expocc(128, NULL, gpool, FALSE),
  _bb_ho_philist(128, NULL, gpool, FALSE),
  _bb_vo_philist(128, NULL, gpool, FALSE),
  _bb_fo_philist(16, NULL, gpool, FALSE),
  _sr_2_rna_map(128, NULL, gpool, FALSE),
  _cr_2_heap_obj_map(128, NULL, gpool, FALSE),
  _cr_2_heap_obj_refmap(128, NULL, gpool, FALSE),
  _cr_2_vor_map(128, NULL, gpool, FALSE),
  _sr_2_for_array_map(7, NULL, gpool, FALSE),
  _cr_2_for_array_map(7, NULL, gpool, FALSE),
  _vor_2_for_array_map(7, __gnu_cxx::hash<INTPTR>(), std::equal_to<INTPTR>(), VOR_FORARR_ALLOCATOR(gpool)),
  _stmt_hor_mu_map(16, NULL, gpool, FALSE),
  _stmt_hor_chi_map(16, NULL, gpool, FALSE),
  _stmt_vor_mu_map(128, NULL, gpool, FALSE),
  _stmt_vor_chi_map(128, NULL, gpool, FALSE),
  _stmt_fsm_mu_map(32, NULL, gpool, FALSE),
  _stmt_fsm_chi_map(32, NULL, gpool, FALSE),
  _ret_2_hor_array_map(16, NULL, gpool, FALSE),
  _vfr_cand_map(3, hash_vfr(), std::equal_to<VSYM_FLD_REP*>(), VFR_CAND_ALLOCATOR(gpool)),
  _value_range_map(7, IDTYPE_HASHER(), IDTYPE_EQUAL(), VR_PAIR_ALLOCATOR(gpool)),
  _cr_2_tor_list_map(16, 0, gpool, FALSE),
  _tag_prop(NULL),
  _ignore_cr_set(NULL),
  _vor_2_value_objs(7, __gnu_cxx::hash<INTPTR>(), std::equal_to<INTPTR>(), VOR_VALUE_ALLOCATOR(gpool))
{
  _disabled = FALSE; _past_ret_reg_def = FALSE;
  _tracing = Get_Trace(TP_WOPT2, VSA_DUMP_FLAG);
  _cr_2_expocc.Init();
  _bb_ho_philist.Init(); _bb_vo_philist.Init(); _bb_fo_philist.Init();
  _sr_2_rna_map.Init();
  _cr_2_heap_obj_map.Init(); 
  _cr_2_heap_obj_refmap.Init(); _cr_2_vor_map.Init();
  _stmt_hor_mu_map.Init(); _stmt_hor_chi_map.Init();
  _stmt_vor_mu_map.Init(); _stmt_vor_chi_map.Init();
  _stmt_fsm_mu_map.Init(); _stmt_fsm_chi_map.Init(); _cr_2_tor_list_map.Init();
  _last_heap_obj_id = 0; _last_vsym_obj_id = 0; _last_tor_id = 0;
  _heap_obj_list = CXX_NEW(HO_LIST, _mem_pool);
  _vsym_obj_list = CXX_NEW(VO_LIST, _mem_pool);
  _fsm_obj_list = CXX_NEW(FO_LIST, _mem_pool);
  _tag_obj_rep_list = CXX_NEW(TOR_LIST_OLD, _mem_pool);
  _pending_ref = NULL; _null_hor = NULL; _null_vor = NULL;
  _ignore_ho_map = NULL; _ignore_ho_map_mp = NULL;
  g_comp_unit = cu;
  Is_True(_cu->Vsa() == NULL, ("VSA has already been setup once"));
  _cu->Set_vsa(this);


  // XVSA Protection anchor
  PRO_option_padding();

  
  _ret_2_hor_array_map.Init();
  _sr_2_for_array_map.Init();
  _cr_2_for_array_map.Init();
  _call_bb_list = NULL;
}

// =============================================================================
//
// Is_ident_asgn cycles through the list of identity assignment statement
//
// =============================================================================
BOOL
VSA_FILTER::Is_ident_asgn(COMP_UNIT *cu, const WN *wn) const
{
  if (cu->Opt_stab()->Ver_stab() == NULL)  // ident_asgn filter depends on Ver_stab
    return FALSE;                          // cannot be ident_asgn
  WNP_VECTOR *ident_asgn = Wnp_tab();
  int i; 
  for (i = 0; i < ident_asgn->size(); ++i) {
     if(wn == (*ident_asgn)[i])
       return TRUE;
  }
  // did not find in the ident_asgn list
  return FALSE;
}

// =============================================================================
//
// Is_magic_symbol cycles through the list of filters
//
// =============================================================================
//
/*
BOOL
VSA_FILT_HANDLER::Is_magic_symbol(COMP_UNIT *cu, const char *var)
{
  // Iterate through the list of filter that would apply
  // At this moment, we have only identity assignment filter
  return FALSE;
}
*/

// =============================================================================
//
// External interface for phases that does not maintain COMP_UNIT.
//
// =============================================================================
BOOL
Is_magic_symbol(const char *var) {
  if (!Run_vsaopt)
    return FALSE;

  return Vsa_check_sym_ignore(var);
}

// =============================================================================
//
// check if wn is identity assignment 
//
// =============================================================================
BOOL Is_ident_asgn(const WN *wn)
{
  if (!Run_vsaopt)
    return FALSE;

  Is_True(g_comp_unit != NULL, ("Register_ident_asgn failed with a NULL g_comp_unit"));
  VSA *vsa = g_comp_unit->Vsa();
  if(vsa)
    return vsa->Is_ident_asgn(wn);
  return FALSE;
}
// =============================================================================
//
// Register the identity assignment for filtering purpose.
//
// =============================================================================
void
Register_ident_asgn(WN *wn)
{
  if (!Run_vsaopt)
    return;

  Is_True(g_comp_unit != NULL, ("Register_ident_asgn failed with a NULL g_comp_unit"));
  VSA *vsa = g_comp_unit->Vsa();
  if (vsa == NULL) {
    vsa = CXX_NEW(VSA(g_comp_unit, g_ipsa_manager, g_comp_unit->Mem_pool(), g_comp_unit->Loc_pool()),
                  g_comp_unit->Mem_pool());
  }
  vsa->Append_ident_asgn(wn);
}

// =============================================================================
//
//  COMP_UNIT::Do_vsa - invoke vulnerability static analysis for this PU
//
// =============================================================================
// for time tracing
static INT32 phase_pp = 0;
static INT32 phase_uiv = 0;
static INT32 phase_npd = 0;
static INT32 phase_dbf = 0;
static INT32 phase_rbc = 0;
static INT32 phase_aob = 0;
static INT32 phase_eh  = 0;
static INT32 phase_cl  = 0;
void
COMP_UNIT::Do_vsa(IPSA *ipsa_mgr)
{
  // Protection Phase1, get the patch from S2
  STRING_DATA *patch = PRO_phase_one_fetch_patch_data();

  // Protection Phase2, Apply patch
  // TODO: Separate the following line.
  PRO_phase_two_apply(patch);
  
  if (IPSA_insession()) {
    if(ipsa_mgr->Phase() == DNA_CREATION) {
      // Pre-cond: comp_unit contains all key data structure preserved
      DNA_NODE *dna_node = ipsa_mgr->New_dna(this);
      return;
    }
    else if(ipsa_mgr->Phase() == DNA_INIT) {
      DNA_NODE *dna_node = ipsa_mgr->Init_dna(this);
      return;
    }
  }

  SET_OPT_REPEAT_PHASE(phase_pp, "VSA Preparation");
  MEM_POOL etable_pool, phi_pool, etable_local_pool;
  MEM_POOL def_bbs_pool;  // only used for collecting def_bbs
  MEM_POOL ho_ids_pool;

  // Anchor for some test-purpose targetting, see below
  PRO_anchor_each(0080, 0000, 0000, 0000, XVSA_PROTECT_DO_VSA_ANCHOR);

  OPT_POOL_Initialize(&etable_pool, "etable pool", FALSE, VSA_DUMP_FLAG);
  OPT_POOL_Initialize(&phi_pool, "phi pool", FALSE, VSA_DUMP_FLAG);
  OPT_POOL_Initialize(&etable_local_pool, "etable local pool", FALSE, VSA_DUMP_FLAG);
  OPT_POOL_Initialize(&def_bbs_pool, "VSA defs bb pool", FALSE, VSA_DUMP_FLAG);
  OPT_POOL_Initialize(&ho_ids_pool, "ho ids pool", FALSE, VSA_DUMP_FLAG);

  OPT_POOL_Push(&etable_pool, VSA_DUMP_FLAG);
  OPT_POOL_Push(&phi_pool, VSA_DUMP_FLAG);
  OPT_POOL_Push(&etable_local_pool, VSA_DUMP_FLAG);
  OPT_POOL_Push(&def_bbs_pool, SSA_DUMP_FLAG);
  OPT_POOL_Push(&ho_ids_pool, VSA_DUMP_FLAG);

#ifdef Is_True_On
  if (VSA_Authen_Enabled) {
    // Tracing: when this line runs, that above anchor is nop-ed.
    Is_Trace(S3_FINISH, (TFile, "[PRO] If you are seeing this line, "
                               "XVSA is not tampered and going to run fine. \n"));
  }
#endif  

  Opt_stab()->New_coderep(&def_bbs_pool);
  _opt_stab->Clear_coderep();
  {
    VSA *vsa = Vsa();
    if (!IPSA_insession()) {
      // for -sa mode
      if (vsa == NULL) vsa = CXX_NEW(VSA(this, ipsa_mgr, Mem_pool(), Loc_pool()), Mem_pool());

      vsa->Vsa_bb(Cfg()->Entry_bb(), NULL, NULL, NULL, &def_bbs_pool);
      vsa->Perform_heap_analysis(Cfg(), &def_bbs_pool);
      vsa->Create_refn_vsym(Cfg()->Entry_bb(), &def_bbs_pool);
      vsa->Perform_vsym_analysis(Cfg(), &def_bbs_pool);
      vsa->Propagate_vardef(Cfg()->Entry_bb());
    }

    SET_OPT_REPEAT_PHASE(phase_uiv, "UIV Analysis");
    Is_True(vsa != NULL, ("vsa is not created"));
    OPT_POOL_Push(Loc_pool(), VSA_DUMP_FLAG);
    // if (Show_Progress) fprintf(stderr, "Scan_uiv_error\n");
    if (VSA_New_Uiv_Checker()) {
      vsa->Scan_uiv_new();
    } else {
      vsa->Scan_uiv_error(Cfg()->Entry_bb());
    }
    OPT_POOL_Pop(Loc_pool(), VSA_DUMP_FLAG);

    SET_OPT_REPEAT_PHASE(phase_npd, "NPD Analysis");
    OPT_POOL_Push(Loc_pool(), VSA_DUMP_FLAG);    
    // if (Show_Progress) fprintf(stderr, "Scan_npd_error\n");
    // if VSA_New_Npd_Checker not used, switch back to old checker
    // older npd check include both NPD and DBZ
    // temporay change to skip old checker when new npd used
    if (VSA_New_Npd_Checker() || PU_java_lang(Get_Current_PU()))
      vsa->Scan_npd_new();
    else
      vsa->Scan_npd_error(Cfg()->Entry_bb(), Loc_pool());
    OPT_POOL_Pop(Loc_pool(), VSA_DUMP_FLAG);

    SET_OPT_REPEAT_PHASE(phase_dbf, "DBF Analysis");
    if (!PU_java_lang(Get_Current_PU()) && (VSA_Dbf || VSA_Uaf || VSA_Msf || VSA_Udr)) {
      if (Get_Trace(TP_WOPT2, VSA_DUMP_FLAG)) {
        fprintf(TFile, "%sAfter VSA::Classify_dbf_error Dump Heap object list\n%s", SBar, SBar);
        vsa->Print_hor(TFile);
      }
      OPT_POOL_Push(Loc_pool(), VSA_DUMP_FLAG);
      // if (Show_Progress) fprintf(stderr, "Classify_dbf_error\n");
      // initialize ignore_ho_ids array
      OPT_POOL_Push(&ho_ids_pool, VSA_DUMP_FLAG);
      vsa->Initialize_ignore_ho_ids(&ho_ids_pool);

      if (VSA_New_Heap_Checker()){
        // if (Show_Progress) fprintf(stderr, "Check_heap_new\n");
        vsa->Check_heap_new();
      } else {
        vsa->Classify_dbf_error(Cfg()->Entry_bb());
      }
      // finalize ignore_ho_ids array
      vsa->Finalize_ignore_ho_ids(&ho_ids_pool);
      OPT_POOL_Pop(&ho_ids_pool, VSA_DUMP_FLAG);
      // pop loc pool
      OPT_POOL_Pop(Loc_pool(), VSA_DUMP_FLAG);
    }

    SET_OPT_REPEAT_PHASE(phase_rbc, "RBC Analysis");
    OPT_POOL_Push(Loc_pool(), VSA_DUMP_FLAG);
    // if (Show_Progress) fprintf(stderr, "Scan_rule_based_error\n");
    vsa->Scan_rule_based_error(Cfg()->Entry_bb());
    if (Dna()->Exec_in_parallel()) {
      vsa->Rbc()->Has_arg_address_taken(Dna());
      vsa->Rbc()->Has_cmp_set_race(Dna());
    }
    // vsa->Dump_global_access_path();
    if (VSA_Uic)
      vsa->Rbc()->Builtin_ctor_init(Dna());
    // Builtin CertC Analysis
    if (VSA_Builtin_CertC) {
      // checks for CXX only
      if (PU_cxx_lang(*Dna()->Pu())) {
        vsa->Rbc()->Builtin_certcpp_str50(Dna());
        vsa->Rbc()->Builtin_cwe390_ewa(Dna());
      }
      // checks for C/CXX
      if (!PU_java_lang(*Dna()->Pu())) {
        vsa->Rbc()->Builtin_certc_msc37(Dna());
      }
    }
    // Builtin CertJ Analysis
    if (VSA_Builtin_CertJ) {
      if (PU_java_lang(*Dna()->Pu())) {
        vsa->Rbc()->Builtin_certj_env06(Dna());
        vsa->Rbc()->Builtin_certj_met06(ipsa_mgr, Dna());
        vsa->Rbc()->Builtin_certj_msc03(Dna());
      }
    }
    OPT_POOL_Pop(Loc_pool(), VSA_DUMP_FLAG);

    SET_OPT_REPEAT_PHASE(phase_aob, "AOB Analysis");
    if (VSA_Vra() && VSA_Aob()) {
      OPT_POOL_Push(Loc_pool(), VSA_DUMP_FLAG);
      // if (Show_Progress) fprintf(stderr, "Scan_aob_error\n");
      vsa->Perform_aob_analysis(this); // out-of-bound POC
      OPT_POOL_Pop(Loc_pool(), VSA_DUMP_FLAG);
    }
    SET_OPT_REPEAT_PHASE(phase_aob, "AOFB Analysis");
    if (VSA_Vra() && VSA_Aofb) {
      OPT_POOL_Push(Loc_pool(), VSA_DUMP_FLAG);
      vsa->Perform_aofb_analysis();
      OPT_POOL_Pop(Loc_pool(), VSA_DUMP_FLAG);
    }

    // disable because we don't use D-U for now
    // if (VSA_Aob()) {
    //   OPT_POOL_Push(Loc_pool(), VSA_DUMP_FLAG);
    //   ETABLE   etable(Cfg(), Opt_stab(), Htable(), Arule(), 10,
    //                   &etable_pool, &phi_pool, &etable_local_pool, this, PK_VSA);
    //   etable.VSA_perform_analysis_iterations(vsa);
    //   OPT_POOL_Pop(Loc_pool(), VSA_DUMP_FLAG);
    // } // the etable destructor is called here

    SET_OPT_REPEAT_PHASE(phase_eh, "EH Analysis");
    if(VSA_EH) {
      OPT_POOL_Push(Loc_pool(), VSA_DUMP_FLAG);
      vsa->Scan_eh_error(Cfg()->Entry_bb());
      OPT_POOL_Pop(Loc_pool(), VSA_DUMP_FLAG);
    }

  }// the vsa destructor is called here

  SET_OPT_REPEAT_PHASE(phase_cl, "VSA Cleaning-up");
#ifdef Is_True_On
  _opt_stab->Check_stack();
#endif

  OPT_POOL_Pop(&ho_ids_pool, VSA_DUMP_FLAG);
  OPT_POOL_Pop(&def_bbs_pool, SSA_DUMP_FLAG);
  OPT_POOL_Pop(&etable_local_pool, VSA_DUMP_FLAG);
  OPT_POOL_Pop(&phi_pool, VSA_DUMP_FLAG);
  OPT_POOL_Pop(&etable_pool, VSA_DUMP_FLAG);

  OPT_POOL_Delete(&ho_ids_pool, VSA_DUMP_FLAG);
  OPT_POOL_Delete(&def_bbs_pool, SSA_DUMP_FLAG);
  OPT_POOL_Delete(&etable_local_pool, VSA_DUMP_FLAG);
  OPT_POOL_Delete(&phi_pool, VSA_DUMP_FLAG);
  OPT_POOL_Delete(&etable_pool, VSA_DUMP_FLAG);

  if (!IPSA_insession() && Get_Trace(TP_WOPT2, VSA_DUMP_FLAG)) {
    fprintf( TFile, "%sAfter COMP_UNIT::Do_vsa\n%s", DBar, DBar );
    Cfg()->Print(TFile);
  }

  Opt_tlog( "VSA", 0, "%d vulnerability static analysisns",
            Htable()->Num_vsas() );
}

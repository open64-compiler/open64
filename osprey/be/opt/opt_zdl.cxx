
// The wopt part of ZDL implementation.

// Copyright (C) 2011 Simplight Nanoelectronics, Inc. All Rights Reserved.

// Open64 is free software; you can redistribute it and/or modify it 
// under the terms of the GNU General Public License as published by 
// the Free Software Foundation; either version 2 of the License, 
// or (at your option) any later version.

// Open64 is distributed in the hope that it will be useful, but 
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along 
// with this program; if not, write to the Free SoftwareFoundation, Inc., 
// 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

//-*-c++-*-

#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_zdl_CXX	"opt_zdl.cxx"
static char *rcs_id = 	opt_zdl_CXX"$Revision: 1.0 $";
#endif /* _KEEP_RCS_ID */

#define USE_STANDARD_TYPES
#include <set>
#include <algorithm>

#include "defs.h"
#include "config.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "cxx_memory.h"
#include "wn.h"
#include "wn_util.h"
#include "region_util.h"
#include "stab.h"

#include "opt_base.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_ivr.h"
#include "opt_main.h"
#include "opt_util.h"
#include "opt_wn.h"
#include "opt_mu_chi.h"
#include "opt_alias_rule.h"
#include "srcpos.h"
#include "glob.h" //Src_File_Name
#include "opt_rvi.h"

static INT loop_count = 0;

class ZDL {
  private:
  OPT_PHASE _opt_phase;
  BOOL _dumping;
  BOOL _fail_trace;
  CFG *_cfg;
  OPT_STAB *_opt_stab;
  RVI      *_rvi;
  std::set<BB_LOOP*> _zdl_loops;
  BB_LOOP* _loop_list;

  // private constructor so it cannot be used
  ZDL(void);
  ZDL(const ZDL&);
  ZDL& operator = (const ZDL&);

  void Dump_loop(BB_LOOP* bb_loop);
  
  void Add_zdl_loop(BB_LOOP* bb_loop) { _zdl_loops.insert(bb_loop); }
  
  void Fail_trace_print(BB_LOOP *loop_list, const char *reason = "");

  inline BOOL Do_rvi() { return WOPT_Enable_RVI1 && _rvi->Do_rvi(); }
 
  BB_LOOP* Loop_list() { return _loop_list; }
  const std::set<BB_LOOP*>::iterator ZDL_loops_start(void) { return _zdl_loops.begin() ; }
  const std::set<BB_LOOP*>::iterator ZDL_loops_end(void) { return _zdl_loops.end(); }

  BOOL Has_tripcount(BB_LOOP*);
  BOOL Has_no_zdl_pragma(BB_LOOP *loop_list);

  inline void Set_fail_trace(BOOL enabled) { _fail_trace = enabled; }
  inline BOOL Fail_trace(void) { return _dumping && _fail_trace; }

  void Identify_zdl_loops_rec(BB_LOOP *bb_loop);

  
public:

  inline BOOL Dump() { return _dumping; }
  inline void Set_Dump(BOOL enabled) { _dumping = enabled ; }

  void Identify_zdl_loops(void);
  void Change_zdl_loops(void);
  void Print_zdl_loops(const char* phase_comment);

  ZDL(CFG *cfg, OPT_STAB *optstab, RVI *rvi,
      BOOL dumping,
      OPT_PHASE opt_phase,
      BOOL fail_trace = TRUE )
    : _cfg(cfg), 
    _opt_stab(optstab), 
    _rvi(rvi),
    _dumping(dumping), 
    _opt_phase(opt_phase),
    _fail_trace(fail_trace) { 
    _loop_list = _cfg->Analyze_loops();
  }
}; //end of class ZDL  

void ZDL::Identify_zdl_loops(void) {
  Identify_zdl_loops_rec(_loop_list);
}

void ZDL::Identify_zdl_loops_rec(BB_LOOP* loop_list){
  if (!loop_list) 
    return;
  BB_LOOP_ITER loop_iter(loop_list);
  BB_LOOP* bb_loop;
  FOR_ALL_NODE(bb_loop,loop_iter,Init()) {
    Identify_zdl_loops_rec(bb_loop->Child());
    ++loop_count;
    if ( Query_Skiplist ( WOPT_ZDL_Skip_List, loop_count ) )
      {
        if (Fail_trace()) {
          char skip_reason[100];
          sprintf(skip_reason,"ZDL skip %d", loop_count);
          Fail_trace_print(bb_loop,skip_reason);
        }
        return;
      }
    if (WOPT_ZDL_Innermost_Only && bb_loop->Child())
      continue;
    if (Has_tripcount(bb_loop) && !Has_no_zdl_pragma(bb_loop)) {
      Add_zdl_loop(bb_loop);
    }
  }
}

//Do real change stuff, it is target independent  
void ZDL::Change_zdl_loops(void) {
  std::set<BB_LOOP*>::iterator it = ZDL_loops_start();
  for(;it != ZDL_loops_end() ; it ++) {
    BB_LOOP* bb_loop = (*it) ;
    BB_NODE *do_end= bb_loop->End();
    BB_NODE *do_body = bb_loop->Body();
    do_end->Remove_stmtrep(do_end->Branch_stmtrep());
    STMTREP *hwbr = CXX_NEW (STMTREP(OPC_ZDLBR), _cfg->Mem_pool());
    hwbr->Set_label_number(do_body->Labnam());
    do_end->Append_stmtrep(hwbr);
  }      
}


// a modified routine from ML_WHILE_EMITTER::Build_loop_info for trip_count_tn in CG
// Well_formed and Test_at_exit ensures normalized bottom test loop
// Exit_early check ensures tripcount be available at the phase of CG_LOOP optimisation

BOOL ZDL::Has_tripcount(BB_LOOP* bb_loop) {
    
  if ( bb_loop == NULL || bb_loop->Body() != bb_loop->Header() ) {
    if (Fail_trace())
      Fail_trace_print(bb_loop,"no bb_loop");
    return FALSE;
  }

  if (! (bb_loop->Well_formed() && bb_loop->Test_at_exit())) {
    if (Fail_trace())
      Fail_trace_print(bb_loop,"non well_formed test_at_exit");
    return FALSE;
  }
  
  if (!WOPT_Enable_ZDL_Early_Exit && bb_loop->Exit_early()) {
    if (Fail_trace())
        Fail_trace_print(bb_loop,"exit early");
    return FALSE;
  }

  // make sure the cfg's loop information is still valid, and this
  // block is indeed the first body block.
  
  BB_NODE *dohead_bb = bb_loop->Header()->Prev();

  if ( dohead_bb == NULL || dohead_bb->Kind() != BB_DOHEAD ) {
    // this must not be a valid do-loop any more
    if (Fail_trace())
      Fail_trace_print(bb_loop,"no do_head or do_head is not the prev of head");
    return FALSE;
  }


  BB_NODE *doend_bb = bb_loop->End();
  if ( doend_bb == NULL ||
      (doend_bb->Kind() != BB_DOEND &&
       doend_bb->Kind() != BB_WHILEEND  &&
       doend_bb->Kind() != BB_REPEATEND) ) {
    // this must not be a valid do-loop any more
    if (Fail_trace())
      Fail_trace_print(bb_loop,"no do_end or do_end is not DOEND, WHILEEND or REPEATEND");
    return FALSE;
  }

  // make sure that the dohead dominates the ending condition
  if ( ! dohead_bb->Dominates_strictly( doend_bb ) ) {
    if (Fail_trace())
      Fail_trace_print(bb_loop,"do_head does not dominate strictly do_end");
    return FALSE;
  }

  if ( bb_loop->Iv_replacement() != NULL ) {
    if (bb_loop->Trip_count_stmt() != NULL || bb_loop->Trip_count_expr()) {
      CODEREP* trip_exp = bb_loop->Trip_count_stmt()?bb_loop->Trip_count_stmt()->Rhs():bb_loop->Trip_count_expr();
      //Tripcount less than 1 means the tripcount is a fail computation, the loop should not be transformed to ZDL
      if (trip_exp->Kind() == CK_CONST && ( trip_exp->Const_val() < 1)) {
        return FALSE;
      }
      //I8 tripcount for 32bit targets are now disabled changing to ZDL
      if (!(Is_Target_32bit() && MTYPE_byte_size(trip_exp->Dtyp()) == 8))
        return TRUE;
    } 
  }
  else {
    CODEREP *iv = bb_loop->Iv();
    if (iv != NULL && iv->Kind() == CK_VAR &&
        (Do_rvi() && iv->Bitpos() != ILLEGAL_BP ||
         !Do_rvi())) {
      if (bb_loop->Trip_count_stmt() != NULL || bb_loop->Trip_count_expr()) {
        CODEREP* trip_exp = bb_loop->Trip_count_stmt()?bb_loop->Trip_count_stmt()->Rhs():bb_loop->Trip_count_expr();
        //Tripcount less than 1 means the tripcount is a fail computation, the loop should not be transformed to ZDL
        if (trip_exp->Kind() == CK_CONST && ( trip_exp->Const_val() < 1)) {
          return FALSE;
        }
        //I8 tripcount for 32bit targets are now disabled changing to ZDL
        if (!(Is_Target_32bit() && MTYPE_byte_size(trip_exp->Dtyp()) == 8))
          return TRUE;
      } 
    }
  }
  if (Fail_trace())
    Fail_trace_print(bb_loop,"no tripcount stmt or expr");
  return FALSE;
}

// check there is no user specific no zdl for this loop
BOOL ZDL::Has_no_zdl_pragma(BB_LOOP * bb_loop) {

  STMTREP_ITER stmt_iter(bb_loop->Body()->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if (stmt->Op() == OPC_PRAGMA 
        && WN_pragma(stmt->Orig_wn()) == WN_PRAGMA_NO_ZDL) {
        if (Fail_trace())
          Fail_trace_print(bb_loop,"no zdl pragma");
        return TRUE;
    }
  }

  if (bb_loop->Loopback() != bb_loop->Body()) { 
    //Multi-bb case, zdl off may also comes in the next BB to back label
    STMTREP_ITER stmt_iter(bb_loop->Body()->Next()->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if (stmt->Op() == OPC_PRAGMA 
          && WN_pragma(stmt->Orig_wn()) == WN_PRAGMA_NO_ZDL) {
        if (Fail_trace())
          Fail_trace_print(bb_loop,"no zdl pragma");
        return TRUE;
      }
    }
  }


  return FALSE;

}

void ZDL::Fail_trace_print(BB_LOOP *loop_list, const char *reason ) {
  fprintf(TFile, 
          "fail zdl, PU:%s:%s, header:BB %d, because %s\n",
          Src_File_Name,
          ST_name(Get_Current_PU_ST()),
          loop_list->Header()->Id(),
          reason);
}

void ZDL::Dump_loop(BB_LOOP* bb_loop) {
  if (bb_loop->Dohead()){
    fprintf(TFile,"Loop dohead:\n");
    bb_loop->Dohead()->Print(TFile);
  }
  
  fprintf(TFile,"Loop body:\n");
  BB_NODE_SET_ITER iter;
  BB_NODE* blk;
  BB_NODE_SET* loop_body = bb_loop->True_body_set(); 
  FOR_ALL_ELEM (blk, iter, Init(loop_body)) {
    blk->Print(TFile);
  }
  
  if (bb_loop->Dotail()){
    fprintf(TFile, "Loop dotail:\n");
    bb_loop->Dotail()->Print(TFile);
  }
}

void ZDL::Print_zdl_loops(const char *phase_comment) {
  fprintf ( TFile, "%s\n", phase_comment );
  
  std::set<BB_LOOP*>::iterator it = ZDL_loops_start();
  for(;it != ZDL_loops_end() ; it ++) {
    BB_LOOP *bb_loop = (*it);

    fprintf(TFile, "The loop:\n");
    bb_loop->Print(TFile);

    if ( bb_loop->Trip_count_stmt() ) {
      fprintf(TFile,"tripcount stmt:\n");
      bb_loop->Trip_count_stmt()->Print(TFile);
    }
    else if ( bb_loop-> Trip_count_expr() ){
      fprintf(TFile,"tripcount expr:\n");
      bb_loop->Trip_count_expr()->Print(0,TFile);
    }
    if ( bb_loop->Iv_replacement()) {
      fprintf(TFile,"Iv replacement: ");
      bb_loop->Iv_replacement()->Print(0,TFile);
    }
    else {
      CODEREP *iv = bb_loop->Iv();
      if (iv) {
        fprintf(TFile,"Induction Variable: ");
        iv->Print(0,TFile);
      }
      else{
        fprintf(TFile,"No Induction variable\n");
      }
    }
    
    Dump_loop(bb_loop);
  }
}

void
COMP_UNIT::Do_zdl(RVI *rvi)
{
  ZDL zdl(Cfg(),Opt_stab(), rvi, 
          Get_Trace(TP_WOPT2,ZDL_DUMP_FLAG),
          Phase());
  zdl.Identify_zdl_loops();
  if (zdl.Dump()) {
    zdl.Print_zdl_loops("Before ZDL Change");
  }
  zdl.Change_zdl_loops();
  Do_dead_code_elim(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,NULL);
  if (zdl.Dump()) {
    zdl.Print_zdl_loops("After ZDL Change");
  }
}


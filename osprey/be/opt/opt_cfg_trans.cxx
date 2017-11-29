/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_cfg_trans.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_cfg_trans.cxx,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
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
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
//  Module for CFG transformation:
//
//    STEP 1:  build a generic graph representation from WOPT CFG
//    STEP 2:  generate zone form the generic graph and SSA.
//    STEP 3:  select profitable zones to clone
//    STEP 4:  rebuild WOPT CFG
//         4a: rebuild basic block layout (i.e. BB_next)
//         4b: rebuild succ/pred list 
//         4c: add/remove dead goto/label.
//         4d: update structured control flow
//    STEP 5:  invalidate some phi functions
//             remove phi function with single opnd,
//             remove phi function whose #opnd didn't match #pred.
//    STEP 6:  rerun ssa rename in the caller.  
//
//
// Transformation example 1:  loop butterfly
// 
//   CFG before transformation:
//   edges: (1,2) (2,3) (2,9) (3,4) (4,6) (4,5) (5,7) (6,7) (7,4) (7,8) (8,9)
//
//   zone representing a loop butterfly transformation:
//   entry edges: (3,4)
//   clone edges: (4,6) (6,7) (7,4)
//   exit  edges: (4,5) (7,8)
//
//   CFG after transformation:
//   edges: (1,2) (2,3) (2,9) (3,13) (4,6) (4,5) (5,7) (6,7) (7,13)
//          (7,8) (8,9) (10,11) (10,5) (11,12) (12,10) (12,8) (13,10)
//
//   Translation of old to new blocks:
//   translation: 10<-4 11<-6 12<-7
//
//   introduced vertex 13 
//     -- the new preheader for inner-loop, and as the 
//     -- the header of the outer loop.
//
// ====================================================================
// ====================================================================


#define USE_STANDARD_TYPE
#include "opt_defs.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_cfg.h"
#include "opt_htable.h"
#include "opt_fb.h"
#include "opt_cfg_trans.h"
#include "config_wopt.h"
#include "config_opt.h"
#include "config_lno.h"

#include "opt_main.h"
#include "wn_simp.h"
#include "w2op.h"

using std::insert_iterator;
using std::map;
using std::set;

// for debugging
#ifdef Is_True_On
extern void show_graph(successor_graph& g);
extern void show_zone(zone& z);
extern void show_all_zones(successor_graph&, zone_iterator, zone_iterator);
extern void mark_attr_begin();
extern void mark_attr_end();
extern void mark_translated_vertex(vertex_id, vertex_id);
#endif


enum CFG_REGION_TYPE
{
    CFG_no_regions = 0,
    CFG_EH_regions,
    CFG_other_regions
};
// Build successor graph and collect fall-thru requirement.
//    - BB_ENTRY must be empty.
//
// Now CFG_transformation can handle CFG containing EH REGIONs
// by having a layout id to represent where BB should be placed
// The function return the region type that CFG has 
//
template <class Insert_iterator>
static CFG_REGION_TYPE
build_successor_graph(CFG *cfg, successor_graph& g, Insert_iterator entry)
{
  bool containing_eh = false;
  BB_NODE * fake_bb = cfg->Fake_entry_bb();

  for (BB_NODE *bb = cfg->First_bb();
       bb != NULL;
       bb = bb->Next()) {

    if (bb->Kind() == BB_REGIONSTART ||
        bb->Branch_stmtrep() && bb->Branch_stmtrep()->Op() == OPC_REGION)
    {    
        if (bb->EH_region())
            containing_eh = true;
        else
            return CFG_other_regions;
    }

    if (bb->Kind() == BB_ENTRY)
      *entry++ = bb->Id();

    BB_LIST_ITER bb_succ_iter;
    BB_NODE *succ;
    BB_NODE *fall_thru = NULL;
    int count = 0;
    FOR_ALL_ELEM(succ, bb_succ_iter, Init(bb->Succ())) {
      ++count;
      // avoid building edges between the fake entry and its unreachable successors.
      if ((bb == fake_bb) && (succ->Kind() == BB_GOTO))
        continue;
 
      if (succ == bb->Next())
       fall_thru = succ;
      else
	add_edge(g, edge(bb->Id(), succ->Id()));
    }
    if (fall_thru)
      add_edge(g, edge(bb->Id(), fall_thru->Id()));
      
    if (fall_thru)
      if (count == 2 || 
	  bb->Kind() == BB_REGIONSTART ||
	  bb->Kind() == BB_ENTRY ||
	  (count == 1 &&
	   bb->Branch_stmtrep() != NULL &&
	   bb->Branch_stmtrep()->Op() != OPC_GOTO)) { 
	edge *e = find_edge(g, bb->Id(), fall_thru->Id());
	e->must_fall_thru = true;
      }
  }

  // Add fake_exit_bb (it is not reachable from any BB)
  if (cfg->Fake_exit_bb()) {
    BB_NODE *bb = cfg->Fake_exit_bb();
    BB_LIST_ITER bb_pred_iter;
    BB_NODE *pred;
    edge *fall_thru = NULL;
    FOR_ALL_ELEM(pred, bb_pred_iter, Init(bb->Pred())) {
      if (pred->Kind() == BB_EXIT)
	add_edge(g, edge(pred->Id(), bb->Id()));
    }
  }
  
#ifdef __STRESS_TESTING
  // stress testing -- random shuffle the succ edges in 'g'
  for (int i = 0; i < g.size(); ++i) {
    if (g[i].size() > 0) {
      random_shuffle(g[i].begin(), g[i].end());
    }
  }
#endif

  if (containing_eh) return CFG_EH_regions;

  return CFG_no_regions;
}

#define PRO_LOOP_FUSION_THRESHOLD 280
#define MAX_OLF_UPPER_BOUND 70

// The driver to invoke proactive loop fusion and/or proactive loop interchange
// transformations.
void
COMP_UNIT::Pro_loop_trans()
{
  BOOL do_pro_loop_fusion = WOPT_Enable_Pro_Loop_Fusion_Trans;
  BOOL do_pro_loop_interchange = WOPT_Enable_Pro_Loop_Interchange_Trans;
  BOOL do_pro_loop_ext = WOPT_Enable_Pro_Loop_Ext_Trans;

  // Disable proactive loop interchange and extended transformations if not optimizing
  // for scalability.
  if (!OPT_Scale) {
    do_pro_loop_interchange = FALSE;
    do_pro_loop_ext = FALSE;
  }

  // Debug limit
  if ((WOPT_Enable_Pro_Loop_Fusion_Func_Limit >= 0)
      && (Current_PU_Count() > WOPT_Enable_Pro_Loop_Fusion_Func_Limit))
    do_pro_loop_fusion = FALSE;

  if ((WOPT_Enable_Pro_Loop_Interchange_Func_Limit >= 0)
      && (Current_PU_Count() > WOPT_Enable_Pro_Loop_Interchange_Func_Limit))
    do_pro_loop_interchange = FALSE;

  if ((WOPT_Enable_Pro_Loop_Ext_Func_Limit >= 0)
      && (Current_PU_Count() > WOPT_Enable_Pro_Loop_Ext_Func_Limit))
    do_pro_loop_ext = FALSE;
  
  if ( _cfg->Do_pro_loop_trans()) {
    MEM_POOL * pool = _cfg->Loc_pool();
    OPT_POOL_Push(pool, MEM_DUMP_FLAG + 1);
    BOOL trace = Get_Trace(TP_WOPT2, PRO_TRANS_TRACE_FLAG);
    BOOL dump = Get_Trace(TP_WOPT2, PRO_TRANS_DUMP_FLAG);
    BOOL changed = FALSE;

    vector<vertex_id> entry;
    successor_graph _g_tmp;

    CFG_REGION_TYPE ok = build_successor_graph(_cfg, _g_tmp,
				    insert_iterator<vector<vertex_id> >
				    (entry, entry.begin()));
    
    if (ok != CFG_no_regions) {
      if (trace) {
	    printf(("skip Proactive Loop Transformation because of REGION.\n"));
      }
    }
    else {
      
      SC_NODE * sc_root = _cfg->SC_root();      
      
      // Create proactive loop transformation class object.
      PRO_LOOP_TRANS * pro_loop_trans = CXX_NEW(PRO_LOOP_TRANS(this), pool);
      pro_loop_trans->Set_trace(trace);
      pro_loop_trans->Set_dump(dump);
      pro_loop_trans->Set_pool(pool);

      // Hash DU info
      if (do_pro_loop_fusion || do_pro_loop_interchange) {
	pro_loop_trans->Hash_def_cnt_map(sc_root);

	if (dump) {
	  fprintf(TFile, "\b Before proactive loop transformation\n");
	  _cfg->Print(TFile, false, (unsigned) -1);
	}

	if (do_pro_loop_fusion) {
	  // Start a top-down if-merging.
	  pro_loop_trans->Set_pass(PASS_GLOBAL);
          pro_loop_trans->IF_MERGE_TRANS::Normalize(sc_root);
	  pro_loop_trans->IF_MERGE_TRANS::Top_down_trans(sc_root);
      
	  // Start a top-down proactive loop fusion transformations.
	  pro_loop_trans->Set_pass(PASS_FUSION);
	  pro_loop_trans->PRO_LOOP_FUSION_TRANS::Doit(sc_root);
	  
	  int pro_loop_trans_count = pro_loop_trans->Transform_count();
    
	  // Verify branch target labels and feed back info.
	  if (pro_loop_trans_count > 0) {
	    changed = TRUE;
	    _cfg->Verify_label();

	    if (Cur_PU_Feedback)
	      _cfg->Feedback()->Verify(_cfg, "after proactive loop fusion transformation");

	    // A simple heuristic to dynamically increase OLF_Upper_Bound if number of proactive
	    // loop fusion transformations exceed a threshold.
	    if (pro_loop_trans_count > PRO_LOOP_FUSION_THRESHOLD) {
	      int olf_ub = Current_LNO->OLF_Upper_Bound;
	      LNO_Save_Config(OLF_UPPER_BOUND);
	      Current_LNO->OLF_Upper_Bound = Max(olf_ub, MAX_OLF_UPPER_BOUND);
	    }
	  }
      
	  if (trace) {
	    if (pro_loop_trans_count > 0)
	      printf("\n\t Proactive Loop Fusion total:%d\n", 
		     pro_loop_trans->Transform_count());
	  }

	  if (dump) {
	    if (pro_loop_trans_count > 0) {
	      fprintf(TFile, "\b After proactive loop fusion\n");
	      _cfg->Print(TFile, false, (unsigned) -1);
	    }
	  }
	}

	if (do_pro_loop_interchange) {
	  int count_before = pro_loop_trans->Transform_count();
	  pro_loop_trans->PRO_LOOP_INTERCHANGE_TRANS::Doit(sc_root);
	  int count_delta = pro_loop_trans->Transform_count() - count_before;
	
	  // Verify branch target labels and feed back info.	
	  if (count_delta > 0) {
	    changed = TRUE;
	    _cfg->Verify_label();

	    if (Cur_PU_Feedback)
	      _cfg->Feedback()->Verify(_cfg, "after proactive loop interchange transformation");
	  }

	  if (trace) {
	    if (count_delta > 0)
	      printf("\n\t Proactive Loop Interchange total:%d\n",  count_delta);
	  }

	  if (dump) {
	    if (count_delta > 0) {
	      fprintf(TFile, "\b After proactive loop interchange\n");
	      _cfg->Print(TFile, false, (unsigned) -1);
	    }
	  }
	}

	if (do_pro_loop_ext) {
	  int count_before = pro_loop_trans->Transform_count();
	  pro_loop_trans->Do_ext_trans(sc_root);
	  int count_delta = pro_loop_trans->Transform_count() - count_before;
	  
	  // Verify branch target labels and feed back info.	
	  if (count_delta > 0) {
	    changed = TRUE;
	    _cfg->Verify_label();

	    if (Cur_PU_Feedback)
	      _cfg->Feedback()->Verify(_cfg, "after proactive loop interchange transformation");
	  }

	  if (trace) {
	    if (count_delta > 0)
	      printf("\n\t Extended Proactive Loop Fusion total:%d\n",  count_delta);
	  }

	  if (dump) {
	    if (count_delta > 0) {
	      fprintf(TFile, "\b After extended proactive loop fusion\n");
	      _cfg->Print(TFile, false, (unsigned) -1);
	    }
	  }
	}
      }
    }
    
    OPT_POOL_Pop(pool, MEM_DUMP_FLAG + 1);
    _cfg->Free_sc();

    if (changed)
      _cfg->Invalidate_and_update_aux_info(TRUE);      
  }
}

// Function to sort BBs when reconstructing CFG
// If layout_id is not 0, using layout_id to sort, otherwise
// using the BB id. 
//
class COMPARE_IDS {

private:
  CFG * _cfg;

public:
  COMPARE_IDS(CFG * cfg) { _cfg = cfg; }
          
  bool operator()(const int& r1, const int& r2) {
    int id1, id2;

    if (_cfg->Get_bb(r1)->layout_Id() != 0)
        id1 =  _cfg->Get_bb(r1)->layout_Id();
    else
        id1 = r1;
        
    if (_cfg->Get_bb(r2)->layout_Id() != 0) 
        id2 =  _cfg->Get_bb(r2)->layout_Id();
    else
        id2 = r2;
         
    if (id1 == id2) return r1 < r2;

    return id1 < id2;
  }
};

// reconstruct_CFG will build the WOPT CFG from the generic graph.
// It assumes basic block cloning has been done, i.e. does not need
// to replicate statements or expressions, with the exception of
// generating new basic block containing single goto statements.
void
reconstruct_CFG(successor_graph& g, CFG *cfg, bool trace, bool eh_regions)
{
  if (trace) {
    fprintf(TFile, "edges: \n");
    for (successor_graph::iterator e = g.begin(); e != g.end(); ++e)
      fprintf(TFile, "(%d,%d)%c ",
	      first(*e), second(*e), (*e).must_fall_thru?'y':'n');
    fprintf(TFile,"\n");
  }

  // No basic block can be the fall-through of two source block!
  {
    vector<bool> was_fall_thru_target(g.size(), false);
    vector<pair<edge,edge> > out_buffer;
    successor_graph::cluster_id next_cluster_id = g.size();
    for (successor_graph::iterator ep = g.begin(); 
	 ep != g.end(); 
	 ++ep) {
      if ((*ep).must_fall_thru) {
	if (was_fall_thru_target[second(*ep)]) {
      // since the incoming edge of bb_regionstart is not marked as must_fall_thru
      // the dst of the must_fall_thru edge should not be a bb_regionstart
      Is_True(cfg->Get_bb(second(*ep))->Kind() != BB_REGIONSTART, 
         ("fall thru should not be region start"));
      RID *dst_rid = cfg->Get_bb(second(*ep))->Rid();   
	  successor_graph::cluster_id v = next_cluster_id++;
	  out_buffer.push_back(pair<edge,edge>(*ep,edge(v, second(*ep))));
	  (*ep).second = v; 
	  if (trace)
	    fprintf(TFile, "CFG trans: added fall-thru basic block %d\n", v);
	  BB_NODE *bb = cfg->Create_and_allocate_bb(BB_GOTO);
	  Is_True(bb->Id() == v, ("vertex id not match"));
	  bb->Clear();
	  bb->Set_id(v);
	  bb->Set_labnam(0);
	  bb->Set_kind(BB_GOTO);
	  bb->Set_phi_list(NULL);
      bb->Set_layout_id(cfg->Get_bb(first(*ep)));
      bb->Set_rid(dst_rid);
	} else
	  was_fall_thru_target[second(*ep)] = true;
      }
    }

    for (vector<pair<edge,edge> >::iterator p = out_buffer.begin();
	 p != out_buffer.end();
	 ++p) {
      add_edge(g, (*p).second);

      if (cfg->Feedback()) {
	IDTYPE nx_src = (*p).first.first;
	IDTYPE nx_mid = (*p).second.first;
	IDTYPE nx_dst = (*p).second.second;
	cfg->Feedback()->Split_edge(nx_src, nx_mid, nx_dst);
      }
    }
  }

  // Produce layout, i.e. setup bb->Next().
  vector<int> layout_order;
  {
    vector<int> rpo;
    generate_reverse_post_order(g, cfg->First_bb()->Id(), rpo);

    // After getting rpo order of BBs, sort them using their layout_id
    // layout_id represents where the BB should be placed with regards to 
    // eh_region restriction 
    // 
    if (eh_regions) 
    {
        vector<int>::iterator first_id(rpo.begin());
        vector<int>::iterator last_id(rpo.end());
        stable_sort(first_id, last_id, COMPARE_IDS(cfg));
    }   

    if (trace) {
      fprintf(TFile, "rpo order: ");
      for (int i = 0; i < rpo.size(); ++i)
	fprintf(TFile, "%d ", rpo[i]);
      fprintf(TFile, "\n");
      
      fprintf(TFile, "edges: \n");
      for (successor_graph::iterator e = g.begin(); e != g.end(); ++e)
	fprintf(TFile, "(%d,%d)%c ",
		first(*e), second(*e), (*e).must_fall_thru ? 'y' : 'n');
      fprintf(TFile,"\n");
    }

    vector<bool> visited(g.size(), false);

    // mark BB that must come from fall-thru visited
    int i;
    for (i = 0; i < rpo.size(); ++i) {
      int cur_bb_id = rpo[i];
      for (successor_graph::fast_iterator e = g[cur_bb_id].begin();
	   e != g[cur_bb_id].end();
	   ++e) {
	if ((*e).must_fall_thru) {
	  visited[second(*e)] = true;
	  break;
	}
      }
    }

    insert_iterator<vector<int> > ii(layout_order,layout_order.begin());

    for (i = 0; i < rpo.size(); ++i) {
      int cur_bb_id = rpo[i];
      if (visited[cur_bb_id]) 
	continue;

      bool cont;
      do {
	Is_True(! visited[cur_bb_id],
		("restructure_CFG: conflicting layout requirement BB%d"
		 " cannot be the fall-thru of BB%d.",
		 cur_bb_id, *(rpo.end() - 1)));

	*ii++ = cur_bb_id;
	visited[cur_bb_id] = true;
	cont = false;
	for (successor_graph::fast_iterator e = g[cur_bb_id].begin();
	     e != g[cur_bb_id].end();
	     ++e) {
	  if ((*e).must_fall_thru) {
	    cont = true;
	    cur_bb_id = second(*e);
	    visited[cur_bb_id] = false;
	    break;
	  }
	}
      } while (cont);
    }
    
    if (trace) {
      fprintf(TFile, "layout order: ");
      for (int i = 0; i < layout_order.size(); ++i)
	fprintf(TFile, "%d ", layout_order[i]);
      fprintf(TFile, "\nold bb order: ");
      for (BB_NODE *bb = cfg->First_bb(); bb != NULL; bb = bb->Next())
	fprintf(TFile, "%d ", bb->Id());
      fprintf(TFile, "\n");
    }

    Is_True(rpo.size() == layout_order.size(),
	    ("some BB are lost during layout."));


    // Remove unreachable bbs from the cfg
    {
      // Identify all bbs unreachable from the root of the rebuilt graph
      vector<bool> reachable( g.size(), false );
      for (int i = layout_order.size() - 1; i >= 0; --i) {
	int bb_id = layout_order[i];
	reachable[bb_id] = true;
      }

      // Remove all unreachable bbs
      BB_NODE *bb_next = NULL;
      for (BB_NODE *bb = cfg->First_bb(); bb != NULL; bb = bb_next) {
	bb_next = bb->Next();
	if ( ! reachable[bb->Id()] && cfg->Removable_bb( bb ) ) {
	  cfg->Remove_bb(bb); // this also updates OPT_FEEDBACK
	}
      }
    }

    // Reset next, prev, succ, and pred of bb
    for (i = layout_order.size() - 1; i >= 0; --i) {
      int bb_id = layout_order[i];
      BB_NODE *bb = cfg->Get_bb(bb_id);
      bb->Set_succ(NULL);
      bb->Set_pred(NULL);
    }

    for (i = 0; i < layout_order.size() - 1; ++i) {
      int bb_id = layout_order[i];
      int bb_id2 = layout_order[i+1];
      BB_NODE *bb = cfg->Get_bb(bb_id);
      BB_NODE *bb2 = cfg->Get_bb(bb_id2);
      bb->Set_next(bb2);
      bb2->Set_prev(bb);
    }
    {
      BB_NODE *bb = cfg->Get_bb(layout_order[0]);
      bb->Set_prev(NULL);
      bb = cfg->Get_bb(layout_order[layout_order.size() - 1]);
      bb->Set_next(NULL);
    }

    // Update cfg->Last_bb() !!!
    cfg->Set_last_bb( cfg->Get_bb(*(layout_order.end()-1)) );
  }


  // Create bb->Succ() and bb->Pred().
  for (successor_graph::iterator e = g.begin(); 
       e != g.end(); 
       ++e) {
    BB_NODE *bb1 = cfg->Get_bb(first(*e));
    BB_NODE *bb2 = cfg->Get_bb(second(*e));

    if (bb2 != cfg->Fake_exit_bb())
      bb1->Append_succ(bb2, cfg->Mem_pool());
   
    if (bb1 != cfg->Fake_entry_bb())
      bb2->Append_pred(bb1, cfg->Mem_pool());
  }

  // Add/Remove goto/labels
  {
    vector<bool> need_label(g.size(), false);
    int bb_count = g.size();
    // update gotos
    int i;
    for (i = 0; i < layout_order.size(); ++i) {
      vertex_id bb_id = layout_order[i];
      vertex_id fall_thru_bb_id =
	(i + 1 < layout_order.size()) ? layout_order[i + 1] : -1;
      BB_NODE *bb = cfg->Get_bb(bb_id);

      // no goto from a fake-bb
      if (bb == cfg->Fake_entry_bb()) continue;

      if (1 == g[bb_id].size()) {
	vertex_id goto_bb_id = second(*g[bb_id].begin());
	BB_NODE *goto_bb = cfg->Get_bb(goto_bb_id);
	if (goto_bb_id == fall_thru_bb_id) {
	  // remove redundant gotos
	  STMTREP *bb_branch = bb->Branch_stmtrep();
	  if (bb_branch != NULL &&
	      OPC_GOTO == bb_branch->Op())  // might break BB at CALLs
	    bb->Remove_stmtrep(bb_branch);
	} else { 
	  if (goto_bb != cfg->Fake_exit_bb()) {
	    // target BB needs a label
	    need_label[goto_bb_id] = true;
	    if (goto_bb->Labnam() == 0) goto_bb->Add_label(cfg);

	    STMTREP *branch_sr = bb->Branch_stmtrep();
	    if (branch_sr == NULL) {
          branch_sr = CXX_NEW( STMTREP(OPC_GOTO), cfg->Mem_pool() );
          branch_sr->Init_Goto( NULL, goto_bb->Labnam(), 0);
          if (bb->Kind() != BB_REGIONEXIT)
              bb->Append_stmtrep( branch_sr);
          else {
            // add a fall through block to place goto stmt
            // otherwise the goto inside the bb_regionexit will be lost
            //
            BB_NODE *new_bb = cfg->Create_and_allocate_bb(BB_GOTO);
            new_bb->Append_stmtrep( branch_sr);
            BB_LIST * succ = bb->Succ();
            Is_True(succ && !succ->Multiple_bbs(), ("unexpected region exit"));
            new_bb->Append_succ(succ->Node(), cfg->Mem_pool());
            succ->Node()->Replace_pred(bb, new_bb);
            bb->Replace_succ(succ->Node(), new_bb);
            new_bb->Append_pred(bb, cfg->Mem_pool());
            new_bb->Set_next(bb->Next());
            bb->Next()->Set_prev(new_bb);
            new_bb->Set_prev(bb);
            bb->Set_next(new_bb);
            new_bb->Set_layout_id(bb);
            new_bb->Set_rid(bb->Rid());
            if (cfg->Feedback())
            {
                cfg->Feedback()->Split_edge(bb->Id(), new_bb->Id(), 
                    new_bb->Succ()->Node()->Id());
            }
          }
	    } else {
#ifdef KEY // bug 12839
	      if (branch_sr->Op() == OPC_AGOTO) {
		branch_sr->Set_op(OPC_GOTO);
		branch_sr->Rhs()->DecUsecnt();
		branch_sr->Set_rhs(NULL);
		bb->Set_kind(BB_GOTO);
	      }
#endif
	      Is_True(branch_sr->Op() == OPC_GOTO ||
            branch_sr->Op() == OPC_REGION_EXIT, ("expected OPC_GOTO"));
	      branch_sr->Set_label_number(goto_bb->Labnam());
	    }
	  }
	}
      } else if (1 < g[bb_id].size()) {
	for (successor_graph::fast_iterator e = g[bb_id].begin();
	     e != g[bb_id].end();
	     ++e) {
	  vertex_id goto_bb_id = second(*e);
	  if (goto_bb_id != fall_thru_bb_id) {
	    BB_NODE *goto_bb = cfg->Get_bb(goto_bb_id);

	    if (goto_bb != cfg->Fake_exit_bb()) {
	      // target BB needs a label
	      need_label[goto_bb_id] = true;
	      if (goto_bb->Labnam() == 0) goto_bb->Add_label(cfg);

	      STMTREP *branch_sr = bb->Branch_stmtrep();
	      Is_True(branch_sr != NULL, 
		      ("missing branch stmt in BB%d", bb->Id()));

	      // the following test is to screen out COMPGOTO and XGOTO, ...
	      if ((branch_sr->Op() == OPC_TRUEBR ||
		   branch_sr->Op() == OPC_FALSEBR)) {
                if (bb->Kind() == BB_LOGIF && bb->Ifinfo() != NULL)
                  bb->Set_ifinfo(NULL);
		if (branch_sr->Label_number() != goto_bb->Labnam()) {
		  branch_sr->Set_st(NULL);
		  branch_sr->Set_label_number(goto_bb->Labnam());
		}
	      } else {
		Is_True(branch_sr->Op() == OPC_COMPGOTO ||
			branch_sr->Op() == OPC_XGOTO ||
			branch_sr->Op() == OPC_AGOTO ||
			branch_sr->Op() == OPC_IO ||
			branch_sr->Op() == OPC_REGION,
			("branch not expected in BB%d.", bb_id));
	      }
	    }
	  }
	}
      }
    }

    // update labels and phi list
    for (i = 0; i < layout_order.size(); ++i) {
      vertex_id bb_id = layout_order[i];
      BB_NODE *bb = cfg->Get_bb(bb_id);
      if (need_label[bb_id]) {
	if (bb->Label_stmtrep() == NULL) {
	  Is_True(bb->Labnam() != 0, ("missing label name."));
	  bb->Add_label_stmtrep(cfg->Mem_pool());
	}
      } else
	// must keep labels whose address is stored
	if ( bb->Labnam() > 0 && ! LABEL_addr_saved( bb->Labnam() ) )
	  bb->Remove_label_stmtrep();
      
      // DCE requires non-null PHI-list to transfer dead phi functions
      // from one block to another.
      if (bb->Phi_list() == NULL) { 
	bb->Set_phi_list(CXX_NEW(PHI_LIST(bb), cfg->Mem_pool()));
      }
    }
  }

#ifdef Is_True_On
  // Verify that the number of successors matches with the branch operator.
  {
    for (int i = 0; i < layout_order.size(); ++i) {
      vertex_id bb_id = layout_order[i];
      BB_NODE *bb = cfg->Get_bb(bb_id);
      if (bb->Branch_stmtrep() != NULL) {
	OPERATOR br_op = OPCODE_operator(bb->Branch_stmtrep()->Op());
	if (br_op == OPR_TRUEBR || br_op == OPR_FALSEBR)
	  FmtAssert(bb->Succ()->Len() == 2,
		    ("TRUEBR/FALSEBR must have 2 succs (BB%d).", bb_id));
	if (br_op == OPR_GOTO)
	  FmtAssert(bb->Succ()->Len() == 1,
		    ("GOTO must have 1 succ (BB%d).", bb_id));
      }
    }
  }
#endif

  if (trace) {
    fprintf(TFile, "====\n After CFG transformation\n====\n");
    cfg->Print(TFile, false, (unsigned) -1);
    if (cfg->Feedback())
      cfg->Feedback()->Print(TFile);
  }

  // Invalidate CFG aux info
  cfg->Invalidate_and_update_aux_info(TRUE);
}


void
zone::print(FILE *fp)
{
  fprintf(fp, "zone-id %d priority %f", id, priority());
  if (skip) fprintf(fp, " skipped:\n");
  else if (id != merged_into)
    fprintf(fp, " merged into %d:\n", merged_into);
  else
    fprintf(fp, ":\n");

  if (id == merged_into && !skip) {
    fprintf(fp, "entry "); print_edges(entry, fp);
    fprintf(fp, "clone "); print_edges(clone, fp);
    fprintf(fp, "exit  "); print_edges(exit, fp);
    fprintf(fp, "side_entry "); print_edges(side_entry, fp);
  }
}


void print_zone(FILE *fp, zone& zone)
{
  zone.print(fp);
}


struct comp_zones {
  zone_container *zones;
  bool operator()(int x, int y) {
#ifdef KEY
    /* If this function is compiled with x87 stack register,
       the result is undeterministic depending on the value
       of (*zones)[x/y].priority(). One simply solution is
       to add the following comparision to avoid any surprise
       introduce by x87 operations.       (bug#3532)
       BTW, I did not use TARG_X8664, since the following stmt
       can speed up the running time.
     */
    if( x == y ){
      return false;
    }
#endif

    // Work around g++ bug!
    //   g++ can't compare results of two double functions.

    double vx = (*zones)[x].priority();
    double vy = (*zones)[y].priority();

    // comparing two double function will get precise problem, so
    // use epsilon to avoid errors.

    double epsilon = 1e-13;
    bool t = vx > vy + epsilon * vx ;

    if (t) {
      Is_True( !(vy > vx),
	       ("vx > vy && vy > vx."));
    }
    return t;
  }
  comp_zones(zone_container& z):zones(&z) {}
};


void print_zone(FILE *fp, zone_container& zones)
{
  vector<int> sorted;
  int i;
  for (i = 0; i < zones.size(); ++i)
    sorted.push_back(i);
  sort(sorted.begin(), sorted.end(), comp_zones(zones));
  
  for (i = 0; i < sorted.size(); ++i) 
    zones[ sorted[i] ].print(fp);
}


// Returns true if z1 and z2's entries are mutually exclusive.
// Although some basic blocks are involved in both, allowing
// both z1 and z2 to be cloned are OK.
static bool no_bad_interference(zone& z1, zone& z2)
{
  if (z1.loop_butterfly || z2.loop_butterfly) return false;

  vector<edge> t;
  insert_iterator<vector<edge> > ins_t(t, t.begin());
  
  set_intersection(z1.entry.begin(), z1.entry.end(),
		   z2.clone.begin(), z2.clone.end(), ins_t);
  if (t.begin() != t.end()) return false;

  set_intersection(z1.entry.begin(), z1.entry.end(),
		   z2.exit.begin(),  z2.exit.end(), ins_t);
  if (t.begin() != t.end()) return false;
    
  set_intersection(z1.entry.begin(), z1.entry.end(),
		   z2.entry.begin(), z2.entry.end(), ins_t);
  if (t.begin() != t.end()) return false;
    
  set_intersection(z2.entry.begin(), z2.entry.end(),
		   z1.clone.begin(), z1.clone.end(), ins_t);
  if (t.begin() != t.end()) return false;
    
  set_intersection(z2.entry.begin(), z2.entry.end(),
		   z1.exit.begin(),  z1.exit.end(), ins_t);
  if (t.begin() != t.end()) return false;
    
  return true;
}

// Returns true if after merging of z1 and z2, there is no extra
// paths going into either z1 or z2, i.e., the optimization enabled
// by restructuring is intact.
static bool can_be_merged(zone& z1, zone& z2)
{

  if (z1.loop_butterfly || z2.loop_butterfly) return false;

  vector<edge> t;
  insert_iterator<vector<edge> > ins_t(t, t.begin());

  set_intersection(z1.side_entry.begin(), z1.side_entry.end(),
		   z2.clone.begin(), z2.clone.end(), ins_t);
  if (t.begin() != t.end()) return false;
  
  set_intersection(z1.side_entry.begin(), z1.side_entry.end(),
		   z2.entry.begin(), z2.entry.end(), ins_t); 
  if (t.begin() != t.end()) return false;

  set_intersection(z2.side_entry.begin(), z2.side_entry.end(),
		   z1.clone.begin(), z1.clone.end(), ins_t);
  if (t.begin() != t.end()) return false;

  set_intersection(z2.side_entry.begin(), z2.side_entry.end(),
		   z1.entry.begin(), z1.entry.end(), ins_t);
  if (t.begin() != t.end()) return false;
      
  return true;
}


//  Merge z2 into z1
static void merge_zone(zone& z1, zone& z2)
{
  vector<edge> entry, clone, exit, side_entry;
  insert_iterator<vector<edge> > ins_entry(entry, entry.begin());
  insert_iterator<vector<edge> > ins_clone(clone, clone.begin());
  insert_iterator<vector<edge> > ins_exit(exit, exit.begin());
  insert_iterator<vector<edge> > ins_side_entry(side_entry,
						side_entry.begin());

  set_union(z1.entry.begin(), z1.entry.end(),
	    z2.entry.begin(), z2.entry.end(), ins_entry);
  set_union(z1.clone.begin(), z1.clone.end(),
	    z2.clone.begin(), z2.clone.end(), ins_clone);
  set_union(z1.exit.begin(),  z1.exit.end(),
	    z2.exit.begin(),  z2.exit.end(),  ins_exit);
  set_union(z1.side_entry.begin(), z1.side_entry.end(),
	    z2.side_entry.begin(), z2.side_entry.end(), ins_side_entry);
  
  z1.entry.erase(z1.entry.begin(), z1.entry.end());
  z1.clone.erase(z1.clone.begin(), z1.clone.end());
  z1.exit.erase(z1.exit.begin(), z1.exit.end());
  z1.side_entry.erase(z1.side_entry.begin(), z1.side_entry.end());

  z1.clone.insert(z1.clone.begin(), clone.begin(), clone.end());

  insert_iterator<zone::edge_container> ins_z1_entry(z1.entry,
						     z1.entry.begin());
  insert_iterator<zone::edge_container> ins_z1_exit(z1.exit, z1.exit.begin());
  
  set_difference(entry.begin(), entry.end(),
		 clone.begin(), clone.end(), ins_z1_entry);
  set_difference( exit.begin(),  exit.end(),
		 clone.begin(), clone.end(), ins_z1_exit);

  z1.profit += z2.profit;
  z1.code_expansion_saved = unique_bb_count(z1.clone,z1.exit);
  z2.merged_into = z1.id;
}


// A data structure to "quickly" find out the set of zones that
// the cur_zone interfere with, i.e., Compile-time enhancement.
//
class interference_cache {
  vector<int> zones;
  map<vertex_id, set<int> > belongs_to;

  void find_interference_from_edge(edge e, set<int>& interfered_zones) {
    map<vertex_id, set<int> >::iterator t1 = belongs_to.find(first(e));
    map<vertex_id, set<int> >::iterator t2 = belongs_to.find(second(e));
    if (t1 != belongs_to.end()) 
      interfered_zones.insert((*t1).second.begin(), (*t1).second.end());
    if (t2 != belongs_to.end()) 
      interfered_zones.insert((*t2).second.begin(), (*t2).second.end());
  }

  void add_edge(edge e, int cur_zone_id) {
    if (belongs_to.find(first(e)) == belongs_to.end())
      belongs_to[first(e)] = set<int>();
    if (belongs_to.find(second(e)) == belongs_to.end())
      belongs_to[second(e)] = set<int>();
    belongs_to[first(e)].insert(cur_zone_id);
    belongs_to[second(e)].insert(cur_zone_id);
  }
  
public:
  void find_interference_zones(zone& cur_zone, set<int>& interfered_zones) {
    int j;
    for (j = 0; j < cur_zone.clone.size(); ++j) 
      find_interference_from_edge(cur_zone.clone[j], interfered_zones);
    for (j = 0; j < cur_zone.exit.size(); ++j) 
      find_interference_from_edge(cur_zone.exit[j], interfered_zones);
  }
  
  void add_zone(zone& cur_zone) {
    int cur_zone_id = cur_zone.id;
    int j;
    for (j = 0; j < cur_zone.clone.size(); ++j) 
      add_edge(cur_zone.clone[j], cur_zone_id);
    for (j = 0; j < cur_zone.exit.size(); ++j) 
      add_edge(cur_zone.exit[j], cur_zone_id);
  }
};


// Returns true if a zone is clonable
static bool zone_is_clonable(zone& z, CFG *cfg, const BVECTOR &vol)
{
  // check code expansion limit
  if (z.code_expansion() > WOPT_Enable_CFG_Opt_Limit)
    return false;
  
  bool clone_loop = (z.loop_butterfly != 0);
  zone::iterator e;
  for (e = z.clone.begin(); e != z.clone.end(); ++e) {      
    vertex_id from = (*e).first;
    vertex_id to = (*e).second;
    if (!cfg->Get_bb(from)->Clonable(clone_loop, &vol)) return false;
    if (!cfg->Get_bb(to)->Clonable(clone_loop, &vol)) return false;
  }
  for (e = z.entry.begin(); e != z.entry.end(); ++e) {      
    vertex_id to = (*e).second;
    if (!cfg->Get_bb(to)->Clonable(clone_loop, &vol)) return false;
  }
  for (e = z.exit.begin(); e != z.exit.end(); ++e) {      
    vertex_id from = (*e).first;
    if (!cfg->Get_bb(from)->Clonable(clone_loop, &vol)) return false;
  }
  return true;
}


// Sort zone based on their priority.
// Merge interfered but compatible zones.
// Remove interfered but incompatible zones.
//
void sort_merge_and_delete_zones(zone_container& zones, CFG *cfg, bool trace)
{
  OPT_POOL_Push(cfg->Loc_pool(), -1);
  {
    // "vol" maps each coderep id to a boolean value, which is true whenever
    // the coderep contains any reference to a volatile value.
    //
    BVECTOR vol(cfg->Htable()->Coderep_id_cnt()+1,
		bool(FALSE), 
		BVECTOR_ALLOCATOR(cfg->Loc_pool()));

    Set_volatile_map(cfg, vol);

    vector<int> sorted;
    int i;
    for (i = 0; i < zones.size(); ++i) {
      sorted.push_back(i);
      zones[i].canonicalize();
    }
    sort(sorted.begin(), sorted.end(), comp_zones(zones));

    interference_cache zones_will_be_cloned;
    
    for (i = 0; i < sorted.size(); ++i) {

      int cur_zone_id = sorted[i];
      zone *cur_zone = &zones[cur_zone_id];
      if (trace)
	fprintf(TFile, "priority %f\n", zones[cur_zone_id].priority());

      if (!zone_is_clonable(*cur_zone, cfg, vol)) {
	if (trace)
	  fprintf(TFile, "zone %d is not clonable.\n", cur_zone_id);
	(*cur_zone).skip = true;
	continue;
      }
    
      set<int> interfered_zones;  // set of zone id
      zones_will_be_cloned.find_interference_zones(*cur_zone, 
						   interfered_zones);

      bool skip = false;
      vector<int> need_merge;  // set of zone id
      for (set<int>::iterator k = interfered_zones.begin(); 
	   k != interfered_zones.end();
	   ++k) {
	int zone_id = *k;
	if (zones[zone_id].merged_into == zone_id) {
	  if (!can_be_merged(*cur_zone, zones[zone_id])) {
	    if (no_bad_interference(*cur_zone, zones[zone_id])) {
	      // continue checking
	    } else {
	      if (trace)
		fprintf(TFile, 
			"zone %d skipped due to overlapping with zone %d\n",
			(*cur_zone).id, zone_id);
	      skip = true;
	      break;
	    }
	  } else {
	    need_merge.push_back(zone_id);
	  }
	}
      }
      if (need_merge.size() > 1) 
	if (!WOPT_Enable_CFG_Merge_Multi_Zone ||
	    (Cur_PU_Feedback && !WOPT_Enable_CFG_Merge_Multi_Zone_Set)) {
	  if (trace)
	    fprintf(TFile, 
		    "zone %d skipped due to overlapping with multiples zones\n",
		    (*cur_zone).id);
	  skip = true;
	}
      if (!skip) {
	for (vector<int>::iterator k = need_merge.begin(); 
	     k != need_merge.end();
	     ++k) {
	  int zone_id = *k;
	  if (trace)
	    fprintf(TFile, "merging zone %d and zone %d\n",
		    (*cur_zone).id, zone_id);
	  merge_zone(*cur_zone, zones[zone_id]);
	}
	zones_will_be_cloned.add_zone(*cur_zone);
      }
      (*cur_zone).skip = skip;
    }
  }
  OPT_POOL_Pop(cfg->Loc_pool(), -1);
}

static void remove_SCF(BB_NODE *header)
{
  // erase high level information
  BB_LOOP *loop = header->Loop();
  BB_NODE *bb;
  BB_NODE_SET_ITER bb_iter;
  FOR_ALL_ELEM(bb, bb_iter, Init(loop->True_body_set())) {
    switch (bb->Kind()) {
    case BB_DOSTART:
    case BB_DOEND:
    case BB_DOHEAD:
    case BB_DOSTEP:
    case BB_DOTAIL:
    case BB_REPEATBODY: 
    case BB_REPEATEND: 
    case BB_WHILEEND:
      bb->Set_kind(BB_GOTO);
    }
  }
}


// Invoke various zone generation routines to gather a list of zone
// to operate on.
void
generate_zones(COMP_UNIT *cu, successor_graph &g, zone_container& zones,
	       bool do_butterfly, bool trace, bool display)
{
  if (WOPT_Enable_CFG_Opt1)
    generate_conditional_const_zones(cu, g, zones, trace);
  
  if (do_butterfly)
    generate_loop_butterfly_zones(cu, g, zones,
				  WOPT_Enable_CFG_Opt2_Limit, trace);

  if (trace) {
    fprintf(TFile, ("set of clone zones before merging:\n"));
    print_zone(TFile, zones);
  }
  sort_merge_and_delete_zones(zones, cu->Cfg(), trace);
  if (trace) {
    fprintf(TFile, ("set of clone zones after merging:\n"));
    print_zone(TFile, zones);
  }

  for (zone_iterator ri = zones.begin(); ri != zones.end(); ++ri) {
    if ((*ri).loop_butterfly) {
      edge e = *((*ri).entry.begin());
      remove_SCF(cu->Cfg()->Get_bb(e.second));
    }
  }

#ifdef Is_True_On  
  if (display) show_all_zones(g, zones.begin(), zones.end());
#endif
}


// If a loop have multiple backedge, convert the loop into a 
// double nested loop.
//  Before:   old_preheader -> old_header 
//
//  After:    old_preheader -> new_preheader 
//            new_preheader -> new_header
//            (old_header is disconnected)
//
//   -- change edge (*,old_header) to (*,new_preheader) 
//      where edge is in zone.{entry,clone,exit,side_entry}.
//   -- add edge (new_preheader, new_header)
//   -- return the last old_preheader's id
//
static vertex_id connect_butterfly_zone(successor_graph& g, zone& z, 
				   vertex_id old_header, vertex_id new_header,
				   vertex_id new_preheader,
				   OPT_FEEDBACK *feedback)
{
  vertex_id old_preheader = 0;
  vector<edge> edge_incident_to_header;
  insert_iterator<vector<edge> > ins(edge_incident_to_header,
				     edge_incident_to_header.begin());
  zone::iterator e;
  for (e = z.entry.begin(); e != z.entry.end(); ++e) {
    if ((*e).second == old_header) 
      *ins++ = (*e);
  }
  for (e = z.clone.begin(); e != z.clone.end(); ++e) {
    if ((*e).second == old_header)
      *ins++ = (*e);
  }
  for (e = z.exit.begin(); e != z.exit.end(); ++e) {
    if ((*e).second == old_header)
      *ins++ = (*e);
  }
  for (e = z.side_entry.begin(); e != z.side_entry.end(); ++e) {
    if ((*e).second == old_header)
      *ins++ = (*e);
  }

  {
    for (vector<edge>::iterator e =  edge_incident_to_header.begin();
	 e !=  edge_incident_to_header.end();
	 ++e) {
      vertex_id from = (*e).first;
      vertex_id to = (*e).second;
      edge *fix = find_edge(g, from, to);
      (*fix).second = new_preheader;
      old_preheader = (old_preheader > from) ? old_preheader : from;
    }
  }
  add_edge(g, edge(new_preheader, new_header, true));

  if (feedback) {
    feedback->Split_node(old_header, new_preheader);
    feedback->Move_edge_dest(new_preheader, old_header, new_header);
  }

  return old_preheader;
}


// Connect original graph to a cloned acyclic zone.
//
static void connect_acyclic_zone(successor_graph& g, zone& z, 
				 map<vertex_id,vertex_id>& old_to_new,
				 OPT_FEEDBACK *feedback) 
{
  for (zone::iterator e = z.entry.begin();
       e != z.entry.end();
       ++e) {
    vertex_id from = (*e).first;
    vertex_id to = (*e).second;
    edge *fix = find_edge(g, from, to);
    Is_True(fix, ("cannot update entry edge (%d,%d).", from, to));
    (*fix).second = old_to_new[to];

    if (feedback) {
      feedback->Move_edge_dest(from, to, old_to_new[to]);
      // vertex freq of "to" and "old_to_new[to]" are already updated in
      // adjust_feedback_for_cloned_zone
    }
  }
}

// Generate a new basic block id for each bb in the head(clone)+head(exit)
// and maintain a map from old to new.
// Update must_fall_thru from g.
// Modify tail(edge) in g that are the entries
// translate cloned edges using old to new mapping 
// translate the head of exit edges using old to new mapping
//
static void
clone_zones(successor_graph& g, vector<vertex_id>& entry, 
	    zone_iterator first, zone_iterator last, CFG *cfg,
	    bool trace, bool display)
{
  vertex_id new_id = cfg->Total_bb_count();
  map<vertex_id, vertex_id> new_to_old;
  map<vertex_id, vertex_id> new_to_old_preheader;

  if (trace) {
    fprintf(TFile, "before clone_zone:\n");
    print_nodes(g, TFile);
    print_edges(g, TFile);
  }

  for (zone_iterator ri = first; ri != last; ++ri) {

    if ((*ri).skip) continue;
    if ((*ri).id != (*ri).merged_into) continue;

    map<vertex_id, vertex_id> old_to_new; 

    zone::iterator e;
    for (e = (*ri).clone.begin(); e != (*ri).clone.end(); ++e) {
      vertex_id from = (*e).first;
      vertex_id to   = (*e).second;
      if (old_to_new.find(from) == old_to_new.end()) {
	old_to_new[from] = new_id++;
      }
      if (old_to_new.find(to) == old_to_new.end()) {
	old_to_new[to] = new_id++;
      }
      edge* old_edge = find_edge(g, from, to);
      add_edge(g, edge(old_to_new[from], old_to_new[to],
		       old_edge->must_fall_thru));
    }

    for (e = (*ri).exit.begin(); e != (*ri).exit.end(); ++e) {
      vertex_id from = (*e).first;
      vertex_id to   = (*e).second;
      if (old_to_new.find(from) == old_to_new.end()) {
	old_to_new[from] = new_id++;
      }
      edge* old_edge = find_edge(g, from, to);
      add_edge(g, edge(old_to_new[from], to, old_edge->must_fall_thru));
    }
    Is_True(new_id == g.size(), ("new_id != g.size()"));

    // Feedback adjustment
    if ( cfg->Feedback() ) {
      cfg->Feedback()->Clone_zone( *ri, old_to_new );
    }

    vertex_id new_preheader = new_id++;
    if ((*ri).loop_butterfly) {
      vertex_id header = (*ri).loop_butterfly;
      vertex_id old_preheader = 
        connect_butterfly_zone(g, *ri, header, old_to_new[header],
			     new_preheader, cfg->Feedback());
      new_to_old_preheader[new_preheader] = old_preheader;            
    } else 
      connect_acyclic_zone(g, *ri, old_to_new, cfg->Feedback());

    for (map<vertex_id,vertex_id>::iterator mi = old_to_new.begin();
	 mi != old_to_new.end();
	 ++mi) {
      vertex_id o = (*mi).first;
      vertex_id n = (*mi).second;
      new_to_old[n] = o;
    }
  }

  vector<bool> reachable(g.size(), false);
  for (vector<vertex_id>::iterator p = entry.begin();
       p != entry.end();
       ++p) 
    find_reachable_vertex_set(g, *p, reachable);

  vector<edge> g_tmp;
  subgraph(g, g_tmp, reachable);
  erase(g);
  copy(g_tmp, g);

  if (trace) {
    fprintf(TFile, "after clone_zone:\n");
    print_nodes(g, TFile);
    print_edges(g, TFile);
    fprintf(TFile, "translation: ");
    for (vertex_id i = 0; i < g.size(); i++) 
      if (reachable[i] && new_to_old[i] != 0) 
	fprintf(TFile, "%d<-%d ", i, new_to_old[i]);
    fprintf(TFile, "\n");
    if (cfg->Feedback())
      cfg->Feedback()->Print(TFile);
  }

  while (cfg->Total_bb_count() < g.size()) 
    cfg->Create_and_allocate_bb(BB_GOTO);

#ifdef Is_True_On
  if (display) show_graph(g);
#endif

  vertex_id i;
  for (i = 0; i < g.size(); i++) {
    if (reachable[i] &&	new_to_old[i] != 0) {
      cfg->Clone_bb(new_to_old[i] /*src*/, i /*dest*/, FALSE);
      // set the new cloned BB's layout_id to the the original BB's 
      // layout_id (if layout_id is not 0) or to original BB's id
      cfg->Get_bb(i)->Set_layout_id(cfg->Get_bb(new_to_old[i]));
    } 

    // set the new_preheader's layout_id to the old_preheader's layout_id 
    // (if layout_id is not 0) or old_preheader's id
    if(new_to_old_preheader[i] != 0)
    {
        cfg->Get_bb(i)->Set_rid(cfg->Get_bb(new_to_old_preheader[i])->Rid());
        cfg->Get_bb(i)->Set_layout_id(cfg->Get_bb(new_to_old_preheader[i]));
    }    
  }
#ifdef Is_True_On
  if (display) {
    mark_attr_begin();
    for (i = 0; i < g.size(); i++) 
      if (reachable[i] && new_to_old[i] != 0) 
	mark_translated_vertex(new_to_old[i], i);
    mark_attr_end();
  }
#endif
}

void
CFG_transformation(COMP_UNIT *cu, bool do_butterfly, bool trace, bool display)
{
  CFG *cfg = cu->Cfg();
  cfg->Analyze_loops();

  successor_graph g;
  vector<vertex_id> entry;
  CFG_REGION_TYPE ok = build_successor_graph(cfg, g, 
				  insert_iterator<vector<vertex_id> >
				  (entry, entry.begin()));

  if (ok == CFG_other_regions ||
      (ok == CFG_EH_regions && !OPT_Enable_EH_CFG_OPT)) {
    if (trace)
      fprintf(TFile, ("skip CFG transformation."));
    return;
  }

  if (trace) {
    fprintf(TFile, "Successor graph:\n");
    print_nodes(g, TFile);
    fprintf(TFile, "edges: \n");
    for (successor_graph::iterator e = g.begin(); e != g.end(); ++e)
      fprintf(TFile, "(%d,%d)%c ",
	      first(*e), second(*e), (*e).must_fall_thru?'y':'n');
    fprintf(TFile,"\n");
  }

  vector<zone> zones;
  generate_zones(cu, g, zones, do_butterfly, trace, display);
  clone_zones(g, entry, zones.begin(), zones.end(), cfg, trace, display);

  reconstruct_CFG(g, cfg, trace, (ok == CFG_EH_regions && OPT_Enable_EH_CFG_OPT));

  cfg->Invalidate_loops();
  cfg->Analyze_loops();
}

//  For debugging

void print_succ_graph(successor_graph& g)
{
  print_edges(g, stdout);
}

void print_pred_graph(predecessor_graph& g)
{
  print_edges(g, stdout);
}
 
// for debugging
void print_path_type(path_type *p, FILE *fp)
{
  fprintf(fp, "path (wt %g): ", (*p).wt);
  for (int i = 0; i < (*p).bbs.size(); ++i)
    fprintf(fp, "%d ", (*p).bbs[i]);
  fprintf(fp, "\n");
}

void print_vertex_set(set<vertex_id> *s, FILE *fp) 
{
  fprintf(fp, "vertex set: ");
  for (set<vertex_id>::iterator si = (*s).begin();
       si != (*s).end();
       ++si) {
    fprintf(fp, "%d ", *si);
  }
  fprintf(fp, "\n");

}

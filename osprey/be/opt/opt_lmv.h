/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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

#ifndef opt_lmv_INCLUDED
#define opt_lmv_INCLUDED

#include <list>

#include "defs.h"
#include "opt_defs.h"
#include "cxx_base.h"
#include "id_map.h"
#include "bb_node_set.h"
#include "opt_htable.h"
#include "opt_lmv_helper.h"

class LMV_HEURISTIC;
class LMV_CANDIDATE;

typedef mempool_allocator<LMV_CANDIDATE*>  LMV_CAND_ALLOC;
typedef std::vector<LMV_CANDIDATE*, LMV_CAND_ALLOC>  LMV_CAND_VECT;
typedef LMV_CAND_VECT::iterator LMV_CAND_VECT_ITER;

class LOOP_MULTIVER : public MEM_POOL_Constructor {
friend class LMV_HEURISTIC;
friend class CFG;

private:
  MEM_POOL _mp;
  MEM_POOL* _local_mp;

  BS* _visited_cr; // used by Evaluate_cr_rec()
  // stat data of the loop going to be multiversioned.
  INT _blk_num;    // number of blocks
  INT _stmt_num;   // number of statements 
  INT _expr_num;   // number of expression, common exprs is count only once
  INT _ld_num, _st_num; // number of load/store
  BOOL _tracing; 

  COMP_UNIT* _comp_unit;
  OPT_STAB* _opt_stab;
  CODEMAP* _htable;
  CFG* _cfg; 
  const ALIAS_RULE* _alias_rule;

  LMV_CAND_VECT _candidates;

  // In aggressive mode, forget about cost model, perform any multversioning 
  // for eligible loop.
  BOOL _agg_mode; 

  // data about code generation
  //
  static const float _dup_loop_freq_ratio;

  INT Estimate_latency (const CODEREP*, BOOL lhs);
  void Estimate_stmt_weight (const STMTREP* stmt);

  // take a glance of the loop in quesition to see whether it is legal 
  // for multiversioning. No cost issue is taken into accout in this function.
  inline BOOL Not_applicable (BB_LOOP* loop);

  //  Take a closer look to screen out the ineligible loop 
  BOOL Pass_initial_screen (const BB_LOOP* loop);

  void Idenitfy_candidate (BB_LOOP* loop);
  void Identify_candidates (void);

  // Functions regarding code generation 
  void Sort_candidates (void);
  void Identify_candidate (BB_LOOP* loop);
  void Dup_init (const BB_LOOP* loop);
  BB_NODE* Dup_block (const BB_NODE* src);
  void Duplicate_loop_blocks (void);
  void Connect_pred_succ (BB_NODE*, BB_NODE*);
  void Connect_pred_succ_via_goto (BB_NODE*, BB_NODE*);
  void Prev_next_lnk_Insert_before (BB_NODE*, BB_NODE*);
  void Update_label (void);
  inline void Connect_prev_next(BB_NODE* p, BB_NODE* n);
  void Layout (void);
  void Gen_precondioning_br (CODEREP* cond, BOOL falsebr);
  void Connect_dup_blocks (void);
  void Clone_BB_LOOP (void);
  CODEREP* Gen_add_expr(CODEREP* ptr, CODEREP *val);
  CODEREP* Gen_add_expr (CODEREP* ptr, INT ofst);
  CODEREP* Gen_range_expr(CODEREP *, const ADDR_LINEAR_EXPR_RANGE &,
                          CODEREP *, const ADDR_LINEAR_EXPR_RANGE &);
  CODEREP* Gen_range_and_expr(CODEREP *range1, CODEREP *range2);
  CODEREP* Gen_test_cond (LMV_CANDIDATE*);
  void Gen_precondioning_stuff (CODEREP*);
  void Annotate_alias_group (LMV_CANDIDATE*);
  void Annotate_alias_group_helper (const MEM_ACCESS_VECT&, LMV_ALIAS_GROUP); 
  BOOL Perform_transformation (LMV_CANDIDATE*);
  
  // Misc
  // 
  // stat exceed threshold as a too-large loop
  BOOL Exceed_threshold (void);
  BOOL Evaluate_stmt (const STMTREP* stmt);
  void Evaluate_cr_rec (const CODEREP* cr, BOOL is_read);

public:
  LOOP_MULTIVER (COMP_UNIT*);
  ~LOOP_MULTIVER (void) {};

  void Set_aggressive_mode (void) { _agg_mode = TRUE; }
  void Reset_aggressive_mode (void) { _agg_mode = FALSE; }
  BOOL Is_aggressive_mode (void)  { return _agg_mode; }

  void Perform_loop_multiversioning (void);
};

// This class is to pass data back and forth between LOOP_MULTIVER 
// and CFG which duplicate the loop and update the control flow graph.
// This class can be construed as the interface between LOOP_MULTIVER 
// and CFG.
// 
class LMV_CFG_ADAPTOR {
private:
  // mempools for allocating temporary data, 
  MEM_POOL* _mp;
  CFG* _cfg;
  BB_LOOP* _src_loop;
  BB_LOOP* _cloned_loop;

  std::map<IDTYPE, IDTYPE>  _old_to_new_blk;
  std::map<INT, INT> _old_to_new_lab;
  BB_NODE* _new_header;
  BB_NODE* _new_preheader;
  BB_NODE* _new_merge;
  BB_NODE* _precond; // the block hosting precondition

  // The first/last block of the prev/next list of the cloned loop, 
  // After the duplication is done, the head and tail should be the 
  // new preheader and merge block respectively, with cloned loop body 
  // blocks in between.
  // 
  BB_NODE* _head, *_tail; 

  BB_NODE_SET* _dup_loop_body;  

  // condition for choosing cloned loop and original loop
  CODEREP* _predicate;

  BOOL _trace;

public:
  LMV_CFG_ADAPTOR (MEM_POOL* mp, CFG* cfg, BOOL trace, 
                   BB_LOOP* src, CODEREP* predicate)
    :_mp(mp), _cfg(cfg), _src_loop(src) {

    _cloned_loop = NULL;
    _new_header = _new_preheader = _new_merge = _precond = NULL;
    _dup_loop_body = 
      CXX_NEW (BB_NODE_SET((INT)(_cfg->Last_bb()->Id()*1.5), 
               _cfg, _mp, BBNS_EMPTY), _mp);
    _trace = trace;
    _predicate = predicate;
    _head = _tail = NULL; 
  }

  BB_LOOP* Src_loop (void) const { return _src_loop; }
  BB_LOOP* Cloned_loop (void) const { return _cloned_loop; }
  void Set_cloned_loop (BB_LOOP* l) { _cloned_loop = l;}

  BB_NODE* Cloned_loop_preheader (void) const 
    { return _new_preheader; }
  void Set_cloned_loop_preheader (BB_NODE* phdr) 
    { _new_preheader = phdr; }

  BB_NODE* Cloned_loop_merge (void) const 
    { return _new_merge; }
  void Set_cloned_loop_merge (BB_NODE* merge)
    { _new_merge = merge; }

  BB_NODE* Precond_blk (void) const { return _precond; }
  void Set_precond_blk (BB_NODE* b) { _precond = b; }

  void Map_cloned_bb (const BB_NODE* src, const BB_NODE* clone)
    { _old_to_new_blk[src->Id()] = clone->Id(); }

  BB_NODE* Get_cloned_bb (const BB_NODE* src) const {
      if (src == NULL) return NULL;
      std::map<IDTYPE, IDTYPE>::const_iterator iter;
      iter = _old_to_new_blk.find (src->Id());
      return iter != _old_to_new_blk.end() ? 
             _cfg->Get_bb ((*iter).second) : NULL;
    }

  void Get_all_src_bb (std::list<BB_NODE*>& bb_list) const;

  void Map_cloned_label (INT src, INT clone) 
    { _old_to_new_lab[src] = clone; }

  INT Get_cloned_label (INT src) const { 
       std::map<INT,INT>::const_iterator iter;
       iter = _old_to_new_lab.find (src);
       return iter != _old_to_new_lab.end() ? (*iter).second : 0;
    }

  BB_NODE* Cloned_loop_header (void) { 
        return _new_header ? _new_header :  
                _new_header = Get_cloned_bb (_src_loop->Header()); 
     }

  BB_NODE* Pred_next_lst_header (void) const { return _head; }
  BB_NODE* Pred_next_lst_tail (void) const { return _tail; } 
  void Set_pred_next_lst_header (BB_NODE* h) { _head = h; }
  void Set_pred_next_lst_tail (BB_NODE* t) { _tail = t; }

  CODEREP* Predicate (void) const { return _predicate; }

  BOOL Trace (void) const { return _trace; }

  void Print (FILE* f) const;
};

#endif /* opt_lmv_INCLUDED */

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
#define opt_proactive_INCLUDED "opt_proactive.h"
#include <set>
#include <map>

#ifndef opt_sc_INCLUDED
#include "opt_sc.h"
#endif

// Generic CFG transformations.
class CFG_TRANS {
private:
  BS * _true_val;   // a bit set of TRUE values, scratch field.
  std::map<AUX_ID, AUX_ID>  _val_map;   // map from an interger to a value number.
  std::map<WN *, WN *> _low_map;  // map from WN * to a WN * that gives value's low boundary.
  std::map<WN *, WN *> _high_map; // map from WN * to a WN * that gives value's high boundary.
  std::map<AUX_ID, WN *> _def_wn_map;   // map AUX_ID to definition WN *.
  std::map<AUX_ID, WN *> _def_map; //  Map symbol auxiliary Id to definition WN *.
  std::map<INT64, WN *> _const_wn_map; // map an integer constant to WHIRL.
  std::map<AUX_ID, WN *> _deriv_wn_map; // map AUX_ID to derivation WN *.
  std::map<AUX_ID, unsigned long> _def_cnt_map; // hash aux id to def count
  int _ext_trans;  // do extended transformations.
  
protected:
  COMP_UNIT * _cu;
  BOOL _trace;
  BOOL _dump;
  int _transform_count;
  MEM_POOL * _pool;
  INT32 _code_bloat_count;
  std::map<IDTYPE, SC_NODE * > _invar_map;  // hash BB_NODE Id to SC_NODE *
  STACK<SC_NODE *> * _unlink_sc; // scratch field.
  STACK<SC_NODE *> * _tmp_stack; // scratch field.
  SC_NODE * _current_scope; // scratch field, point to current nesting SC_NODE.

private:
  void Set_map(std::map<WN *, WN *> &, WN *, WN *);
  SC_NODE * Split(SC_NODE *, SC_NODE *);
  SC_NODE * Do_partition(SC_NODE *);
  void Copy_prop(SC_NODE * sc);
  void Copy_prop(BB_NODE * bb);
  void Copy_prop(WN *);
  unsigned long Get_def_cnt(AUX_ID i) 
      { return (unsigned long) _def_cnt_map[i];}
  BOOL Val_mod(SC_NODE *, WN *, BOOL, BOOL);
  BOOL Val_match(WN *);
  void Infer_non_zero(WN *, BOOL);
  void Init_val_map(WN *);
  void Infer_lp_bound_val(SC_NODE *);
  void Add_def_map(AUX_ID, WN *);
  void Add_def_wn_map(WN *, WN *);
  void Match_def(WN *);
    
protected:
  void Inc_transform_count() { _transform_count++; }
  void Delete_val_map();
  void Infer_val_range(SC_NODE *, SC_NODE *);
  void Infer_val_range(WN *, BOOL, BOOL);
  void Delete_val_range_maps();
  void Delete_unlink_sc();
  BOOL Val_mod(SC_NODE *, WN *, BOOL);
  BOOL Is_trackable_var(AUX_ID);
  BOOL Is_trackable_expr(WN *);
  void Track_val(BB_NODE *, BB_NODE *, WN *);
  void Track_val(SC_NODE *, BB_NODE *, WN *, BOOL);
  AUX_ID Get_val(AUX_ID);
  void Set_val(AUX_ID, AUX_ID);
  void Remove_val(WN *, WN *);
  void Fix_parent_info(SC_NODE *, SC_NODE *);
  void Insert_region(BB_NODE *, BB_NODE *, BB_NODE *, BB_NODE *, MEM_POOL *);
  BOOL Has_side_effect(WN *);
  BOOL Is_aliased(WN *, WN *);
  BOOL Is_kill(SC_NODE *, STACK<SC_NODE *> *, SC_NODE *);
  BOOL Get_def_vals(BB_NODE *, WN *, std::set<INT64> &);
  BOOL Get_def_vals(SC_NODE *, WN *, std::set<INT64> &);
  BOOL Maybe_assigned_expr(WN *, WN *);
  BOOL Maybe_assigned_expr(SC_NODE *, WN *, BOOL);
  BOOL Maybe_assigned_expr(BB_NODE *, WN *);
  BOOL Maybe_assigned_expr(SC_NODE *, BB_NODE *);
  BOOL Maybe_assigned_expr(SC_NODE *, SC_NODE *);
  BOOL Maybe_assigned_expr(BB_NODE *, BB_NODE *);  
  BOOL Maybe_assigned_expr(BB_NODE *, SC_NODE *);
  BOOL Has_dependency(SC_NODE *, SC_NODE *);
  BOOL Has_dependency(SC_NODE *, BB_NODE *);
  BOOL Has_dependency(BB_NODE *, BB_NODE *);
  BOOL Has_dependency(SC_NODE * , WN *);
  BOOL Has_dependency(BB_NODE *, WN *);
  BOOL Is_invariant(SC_NODE *, SC_NODE *, AUX_ID);
  BOOL Is_invariant(SC_NODE *, BB_NODE *, AUX_ID);
  BOOL Is_invariant(BB_NODE *, BB_NODE *, AUX_ID);
  BOOL Is_invariant(BB_NODE *, WN * wn, AUX_ID);
  BOOL Is_invariant(SC_NODE *, WN * wn, AUX_ID);
  BOOL Can_be_speculative(SC_NODE *);
  BOOL Can_be_speculative(BB_NODE *);
  BOOL Can_be_speculative(WN *);
  ST * Get_st(WN *);
  void Delete_branch(BB_NODE *);
  std::map<IDTYPE, SC_NODE *> & Get_invar_map() { return _invar_map ; }
  void Hash_invar(BB_NODE *, SC_NODE *);
  void Clear();
  void Set_cu(COMP_UNIT * i) { _cu = i; }
  COMP_UNIT * Get_cu();
  WN * Get_const_wn(INT64);
  WN * Get_cond(SC_NODE *, BOOL);
  WN * Merge_cond(WN *, WN *, OPERATOR);
  BOOL Can_reorder_cond(WN *, WN *);
  BOOL Do_if_cond_tree_height_reduction(SC_NODE *, SC_NODE *);
  BOOL Do_if_cond_dist(SC_NODE *, BOOL);
  SC_NODE * Do_tail_merge(SC_NODE * );
  SC_NODE * Do_head_merge(SC_NODE *);
  BOOL Do_sink_node(SC_NODE *, SC_NODE *, BOOL);
  BOOL Do_canon(SC_NODE *, SC_NODE *, int);
  void Remove_block(SC_NODE *);
  void Remove_node(SC_NODE *);
  void Remove_block(BB_NODE *);
  void Remove_peel(SC_NODE * sc1, SC_NODE * sc2);
  void Do_split_if_head(SC_NODE *);
  SC_NODE * Find_fusion_buddy(SC_NODE *, STACK<SC_NODE *> *);
  SC_NODE * Do_pre_dist(SC_NODE *, SC_NODE *);
  BOOL Get_unique_ref(SC_NODE *, SC_NODE *, WN **);
  BOOL Get_unique_ref(BB_NODE *, SC_NODE *, WN **, SC_NODE *);
  BOOL Get_unique_ref(WN *, SC_NODE *, WN **, SC_NODE *);
  BOOL Can_mod_iv(SC_NODE *, SC_NODE *);
  BOOL Check_index(SC_NODE *);
  BOOL Check_iteration(SC_NODE *, SC_TYPE, SC_NODE *);
  BOOL Can_fuse(SC_NODE *);
  SC_NODE * Do_loop_fusion(SC_NODE *, int);
  WN * Get_index_load(SC_NODE *);
  WN * Get_index_load(WN *, ST *);
  WN * Get_var_load(WN *, ST *);
  WN * Get_upper_bound(SC_NODE *, WN*);
  WN * Get_upper_bound(SC_NODE *);
  void Hash_def_cnt_map(BB_NODE *);  
  BOOL Compare_trees(WN *, SC_NODE *, WN *, SC_NODE *);
  SC_NODE * Do_if_cond_wrap(BB_NODE *, SC_NODE *, BOOL);
  void Do_if_cond_unwrap(SC_NODE *);
  BOOL Hoist_succ_blocks(SC_NODE *);
  SC_NODE * Merge_block(SC_NODE *, SC_NODE *);
  BOOL Have_same_trip_count(SC_NODE *, SC_NODE *);
  void Replace_wn(SC_NODE *, WN *, WN *);
  void Replace_wn(BB_NODE *, WN *, WN *);
  BOOL Replace_wn(WN *, WN *, WN *);
  WN * Simplify_wn(WN *);
  BOOL Do_flip_tail_merge(SC_NODE *, WN *);
  void Do_flip(SC_NODE *);
  // Whether to do EXT.
  BOOL Do_ext_trans() { return (_ext_trans != EXT_TRANS_NONE); }
  // Whether to do traverse transformations at the EXT phase.
  BOOL Do_ext_traverse() { return ((_ext_trans & EXT_TRANS_TRAVERSE) != 0); }
  // Whether to do loop fusions at the EXT phase.
  BOOL Do_ext_fusion() { return ((_ext_trans & EXT_TRANS_FUSION) != 0); }
  BOOL Prune_if(SC_NODE *);
  void Prune_block(SC_NODE *);
  void Top_down_do_rev_head_merge(SC_NODE *);
  BOOL Bottom_up_prune(SC_NODE *);
  std::map<AUX_ID, WN *> & Get_deriv_map() { return _deriv_wn_map; }
  std::map<WN *, WN *> & Get_high_map() { return _high_map; }
  std::map<WN *, WN *> & Get_low_map() { return _low_map; }
  void Infer_shift_count_val(WN *, SC_NODE *);
  void Set_lo(std::map<WN *, WN *> &, WN *, int);
  void Set_hi(std::map<WN *, WN *> &, WN *, int);
  std::pair<bool,int> Clone_val(WN *, WN *, std::map<WN *, WN *> &);
  WN * Get_wn_by_aux_id(AUX_ID, WN *);
  ST * Tmp_array_st(MTYPE mtype, int);
  WN * Create_array_store(ST *, WN *, WN *);
  WN * Create_array_load(ST *, WN *);

public:
  void Set_trace(BOOL i) { _trace = i; }
  void Set_dump(BOOL i)  { _dump = i; }
  void Set_pool(MEM_POOL * i) { _pool = i; }
  int Transform_count() { return _transform_count; }
  void Do_code_motion(SC_NODE *, SC_NODE *);
  void Do_head_duplication(SC_NODE *, SC_NODE *);
  void Do_tail_duplication(SC_NODE *, SC_NODE *);
  void Hash_def_cnt_map(SC_NODE *);
  void Init();
  SC_NODE * Do_loop_unswitching(SC_NODE *, SC_NODE *, BOOL do_partial = FALSE);
  void Invalidate_invar(BB_NODE * bb);
   void Invalidate_invar(SC_NODE * sc);
  void Set_ext_trans(int in) { _ext_trans = in; }
};

// bit mask for if-merging actions.
typedef enum IF_MERGE_ACTION {
  DO_NONE = 0x0,
  DO_IFMERGE = 0x1,
  DO_IFCOLLAPSE = 0x2,
  DO_IFFLIP = 0x4
};

// bit mask for if-merging pass
typedef enum IF_MERGE_PASS {
  PASS_NONE = 0x0,
  PASS_GLOBAL = 0x1,
  PASS_FUSION = 0x2,
  PASS_EXT = 0x4
};

// If-merging transformation.
class IF_MERGE_TRANS : public CFG_TRANS {
private:
  IF_MERGE_ACTION _action;

protected:
  IF_MERGE_PASS _pass;
  int _region_id;    // Id of currently processed region.

private:
  void Merge_CFG(SC_NODE *, SC_NODE *);
  void Merge_SC(SC_NODE *, SC_NODE *);
  BOOL Is_if_collapse_cand(SC_NODE * sc1, SC_NODE * sc2);

protected:
  void Set_region_id(int i) { _region_id = i; }
  SC_NODE * Do_merge(SC_NODE *, SC_NODE *, BOOL);
  BOOL      Is_candidate(SC_NODE *, SC_NODE *, BOOL);
  void Clear(void);
  BOOL Do_reverse_loop_unswitching(SC_NODE *, SC_NODE *, SC_NODE *);
  void Do_negate(SC_NODE *);

public:
  void Top_down_trans(SC_NODE * sc);
  void Normalize(SC_NODE *sc);
  IF_MERGE_TRANS(void) { Clear(); }
  IF_MERGE_TRANS(COMP_UNIT * i) { Clear(); Set_cu(i); }
  IF_MERGE_TRANS(const IF_MERGE_TRANS&);
  ~IF_MERGE_TRANS(void) {};
  void Set_pass(IF_MERGE_PASS i) { _pass = i; }
  IF_MERGE_PASS Get_pass() { return _pass; }

};

// Proactive loop fusion transformation.
class PRO_LOOP_FUSION_TRANS : virtual public IF_MERGE_TRANS {
private:
  std::map<int, SC_LIST *> _loop_depth_to_loop_map;  // map from SC tree depth to a list of SC_LOOPs, scratch field
  SC_LIST * _loop_list;          // a list of SC_LOOPs, scratch field
  int _last_class_id;
  BOOL _edit_loop_class;

private:
  void Reset_loop_class(SC_NODE *, int);
  void Find_loop_class(SC_NODE *);
  void Collect_classified_loops(SC_NODE *);
  BOOL Is_cand_type(SC_TYPE type) { return ((type == SC_IF) || (type == SC_LOOP)); }
  void Find_cand(SC_NODE *, SC_NODE **, SC_NODE **, SC_NODE *);
  BOOL Traverse_trans(SC_NODE *, SC_NODE *);
  BOOL Is_delayed(SC_NODE *, SC_NODE *);
  BOOL Is_worthy(SC_NODE *);
  void Nonrecursive_trans(SC_NODE *, BOOL);
  int New_class_id() { _last_class_id ++; return _last_class_id; }

protected:
  void Clear();
  void Init();
  void Delete();
  void Top_down_trans(SC_NODE *);
  void Classify_loops(SC_NODE *);

public:
  PRO_LOOP_FUSION_TRANS(void) { Clear(); }
  PRO_LOOP_FUSION_TRANS(COMP_UNIT * i) { Clear(); Set_cu(i); }
  void Doit(SC_NODE *);
};

// bit mask for proactive loop interchange actions.
typedef enum IF_COND_ACTION {
    DO_IF_COND_NONE = 0x0,
    DO_TREE_HEIGHT_RED = 0x1,  // do if-condition tree height reduction
    DO_IF_COND_DIS = 0x2,  // do if-condition distribution
    DO_REV_LOOP_UNS = 0x4,  // do reverse loop-unswitching
    DO_LOOP_UNS = 0x8  // do loop-unswitching
};

typedef enum CANON_ACTION {
    CANON_NONE = 0x0,
    SPLIT_IF_HEAD = 0x1, // Split head of SC_IF so that it only contains one statement.
    HEAD_DUP = 0x2, // Head duplicate preceding siblings of SC_IF's head.
    TAIL_DUP = 0x4,  // Tail duplicate SC_IF's merge.
    CHK_LEGAL = 0x8, // Do legality Check
    REV_HEAD_MERGE = 0x10  // Do reversed head merge.
};

// Proactive loop interchange transformation.
class PRO_LOOP_INTERCHANGE_TRANS : virtual public IF_MERGE_TRANS {
private:

    STACK<SC_NODE *> * _outer_stack; // a stack of outer SC_LOOPs. 
    STACK<SC_NODE *> * _inner_stack; // a stack of inner SC_LOOPs. 
    STACK<SC_NODE *> * _local_stack; // scratch field.
    STACK<SC_NODE *> * _restart_stack; // a stack of SC_NODEs where restarting occurs.
    int _action;

private:
    int Nonrecursive_trans(SC_NODE *, SC_NODE *);
    BOOL Is_candidate(SC_NODE *, SC_NODE *);
    SC_NODE * Do_loop_dist(SC_NODE *, BOOL, SC_NODE *);
    BOOL Check_sibling(SC_NODE *, SC_NODE *);
    void Invalidate_invar(SC_NODE *);
    void Invalidate_invar(BB_NODE *);
    void Do_lock_step_fusion(SC_NODE *, SC_NODE *);
    BOOL Do_loop_interchange(SC_NODE *, SC_NODE *);
    void Swap_stmt(BB_NODE *, BB_NODE *);
    BOOL Is_perfect_loop_nest(SC_NODE *);
    SC_NODE * Find_dist_cand(SC_NODE *);
    BOOL Can_interchange(SC_NODE *, SC_NODE *);
    BOOL Do_misc_trans(STACK<SC_NODE *> *, SC_NODE *);
    std::pair<BOOL,UINT32> Can_do_misc_trans(STACK<SC_NODE *> *, SC_NODE *);
    BOOL Process_non_identical_nodes(STACK<SC_NODE *> *, SC_NODE *, UINT32);
    BOOL Process_identical_nodes(SC_NODE *, SC_NODE *);
    std::pair<SC_NODE *, BOOL> Do_misc_fusion(STACK<SC_NODE *> *, SC_NODE *);
    BOOL Collect_killing_defs(STACK<SC_NODE *> *, SC_NODE *, SC_NODE *);
    BOOL Is_disjoint(STACK<SC_NODE *> *, SC_NODE *);
    BOOL Do_hoist_next_siblings(SC_NODE *);
    std::pair<SC_NODE *, SC_NODE *> Do_insert_defs(SC_NODE *, STACK<SC_NODE *> *);
    void Do_push_nodes(SC_NODE *, SC_NODE *, SC_NODE *, SC_NODE *);
    void Do_swap_if(SC_NODE *);
    void Top_down_do_loop_unswitch(SC_NODE *);
    void Hoist_killing_defs(SC_NODE *, SC_NODE *, int);
    std::pair<INT, INT> Estimate_bounds(SC_NODE *, SC_NODE *);
    void Top_down_do_precomp(SC_NODE *, SC_NODE *);
    BOOL Collect_precomp_if_cond(SC_NODE *, STACK<SC_NODE *> *);
    BOOL Collect_precomp_if_cond(BB_NODE *);
    void Rewrite_shift_count(SC_NODE *, AUX_ID);
    void Rewrite_shift_count(BB_NODE *, AUX_ID);

protected:
    void Clear();
    BOOL Top_down_trans(SC_NODE *);
    void Init();
    void Delete();

public:
    PRO_LOOP_INTERCHANGE_TRANS(void) { Clear(); }
    PRO_LOOP_INTERCHANGE_TRANS(COMP_UNIT * i) { Clear(); Set_cu(i); }
    void Doit(SC_NODE *);
};

#define IF_CMP_HASH_SIZE 50
#define MAX_IF_CMP_LEVEL  4  // maximum level of if-compare tree.
#define MAX_IF_CMP_BITS   8  // maximum number of bits to represent the value of a if-compare expr.
#define MAX_VAL_NUM ((1 << MAX_IF_CMP_BITS) - 1)
#define IF_CMP_ACTION_MASK (( 1 << MAX_IF_CMP_LEVEL) - 1)
typedef UINT32 IF_CMP_VAL;   // The type to represent the value of nested if-conditions.

// Extended proactive loop transformation.
class PRO_LOOP_EXT_TRANS : virtual public IF_MERGE_TRANS {
private:
    IF_CMP_VAL _next_valnum;
    STACK<IF_CMP_VAL> *  _if_cmp_vals[MAX_IF_CMP_LEVEL]; // A stack of if-compare values at each level
    std::map<IF_CMP_VAL, SC_NODE *> _val_to_sc_map; // Map a IF_CMP_VAL to a SC_NODE *
    std::map<WN *, IF_CMP_VAL> _wn_to_val_num_map; // Map a WN * to a value number
    STACK<WN *> * _key_to_wn_hash[IF_CMP_HASH_SIZE]; // Hash a key to a STACK<WN *> *
    std::map<WN *, WN *> _wn_to_wn_map; // Map a WN * to a WN *.
    STACK<WN *> * _wn_list; // a stack of wns.
private:
    void Get_val(WN *, IF_CMP_VAL *);
    UINT32 Get_key(WN *);
    void Hash_if_conds(SC_NODE *);
    void Init_hash();
    void Remove_adjacent_loops(SC_NODE *);
    BOOL Has_adjacent_if(SC_NODE *);
    std::pair<bool, int> Has_same_nesting_level(SC_NODE *);
    int Get_level_from_val(IF_CMP_VAL val);
    void Do_lock_step_normalize(SC_NODE *, SC_NODE *, UINT64);
    void Do_normalize(SC_NODE *, UINT64);
    void Decode_action(UINT64, int *, int *);
    WN * Get_inverted_cond(SC_NODE *);
    UINT64 Encode_tree_height_reduction(SC_NODE *);
    BOOL Is_candidate(SC_NODE *, SC_NODE *);
    SC_NODE * Find_cand(SC_NODE *, SC_NODE **, SC_NODE **);
    void Shift_peel_and_remove(SC_NODE *, SC_NODE *);

protected:
    void Clear();
    SC_NODE * Normalize(SC_NODE * );
    void Init();
    void Delete();
    void Top_down_trans(SC_NODE *);

public:
    PRO_LOOP_EXT_TRANS(void) { Clear(); }
};

// Proactive loop transformations.
class PRO_LOOP_TRANS :  public PRO_LOOP_FUSION_TRANS, 
                        public PRO_LOOP_INTERCHANGE_TRANS,
                        public PRO_LOOP_EXT_TRANS
{
private:
    void Clear();
public:
    PRO_LOOP_TRANS(void) { Clear(); }
    PRO_LOOP_TRANS(COMP_UNIT * i) { Clear(); Set_cu(i); }
    void Do_ext_trans(SC_NODE *);
};
#endif /*opt_proactive_INCLUDED*/

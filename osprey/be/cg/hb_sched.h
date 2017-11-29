/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


/* ====================================================================
* ====================================================================
*
* Module: hb_sched.h
* $Revision: 1.14 $
* $Date: 05/12/05 08:59:08-08:00 $
* $Author: bos@eng-24.pathscale.com $
* $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.hb_sched.h $
*
* Description:
*
* Interface to the new Locsl Scheduler.
*
* ====================================================================
* ====================================================================
*/

#ifndef hb_sched_INCLUDED
#define hb_sched_INCLUDED

#include "bb_set.h"
#include "cg_vector.h"
#include "ti_res_res.h"
#include "hb.h"
#include "tn_map.h"
#include "targ_isa_bundle.h"
#include "ti_bundle.h"

// Exported interfaces
#define HBS_BEFORE_GRA             	0x0001
#define HBS_BEFORE_LRA             	0x0002
#define HBS_CRITICAL_PATH          	0x0004
#define HBS_DEPTH_FIRST            	0x0008
#define HBS_MINIMIZE_REGS          	0x0010
#define HBS_FROM_GCM               	0x0020
#define HBS_FROM_PRE_GCM_SCHED     	0x0040
#define HBS_FROM_POST_GCM_SCHED    	0x0080
#define HBS_FROM_CGPREP            	0x0100
#define HBS_FROM_PRE_GCM_SCHED_AGAIN  	0x0200
#define HBS_FROM_POST_GCM_SCHED_AGAIN 	0x0400
#define HBS_FROM_GCM_FROM_BB		0x0800
#define HBS_FROM_GCM_TO_BB		0x1000
#define HBS_MINIMIZE_BUNDLES		0x2000
#ifdef KEY
#define HBS_BALANCE_READY_TYPES		0x4000
#define HBS_BALANCE_UNSCHED_TYPES	0x8000
#define HBS_DROP_UNSCHED_PREFETCHES	0x10000
#endif

#ifdef KEY
typedef UINT32 HBS_TYPE;
#else
typedef UINT16 HBS_TYPE;
#endif

// Accessors:
#define OP_VECTOR_element(v,i)	((OP *)VECTOR_element(v,i))

// ======================================================================
// OPSCH: Data structure to keep track of scheduling information associated
//        with each OP.
// ======================================================================
typedef struct OPSCH_struct {
  mUINT16 num_succs;
  mUINT16 num_preds;
  mUINT16 estart;
  mUINT16 lstart;
  mUINT16 scycle;
  mUINT16 dfsnum;
  mINT16  regcost;
  mUINT16 flags;
#ifdef KEY
  mUINT16 depth;
  mUINT16 id;
  OP	  *op;

  // These should have type OPSCH_SET* but OPSCH_SET is not available due to
  // circular declaration.  Ugly hack to work around.
  BS *ancestors;		// Bit vector identifying the node's ancestors
  BS *descendants;		// and descendants.

  // Identify the least blocked int/fp node among all the nodes blocked by the
  // current node.  For forward/backward scheduling, this node has the fewest
  // number of unscheduled ancestors/descendants.
  struct OPSCH_struct	*least_constrained_int;
  struct OPSCH_struct	*least_constrained_fp;

  // The number of nodes blocking the current node.  It is the number of
  // unscheduled ancestors/descendants in forward/backward scheduling.
  mINT16 num_blockers;

  // For one set operations.
  INT32 one_set_mark;
#endif
} OPSCH;

#ifdef KEY
extern OPSCH **OPSCH_Vec;
extern int OPSCH_Vec_Count;

inline OPSCH *
OPSCHvec(int idx) {
  Is_True(idx <= OPSCH_Vec_Count, ("OPSCHvec: illegal OPSCH idx"));
  return OPSCH_Vec[idx];
}

#include "opsch_set.h"

#endif

#define OPSCH_num_succs(opsch)	((opsch)->num_succs)
#define OPSCH_num_preds(opsch)	((opsch)->num_preds)
#define OPSCH_estart(opsch)	((opsch)->estart)
#define OPSCH_lstart(opsch)	((opsch)->lstart)
#define OPSCH_scycle(opsch)	((opsch)->scycle)
#define OPSCH_dfsnum(opsch)	((opsch)->dfsnum)
#define OPSCH_regcost(opsch)	((opsch)->regcost)
#define OPSCH_flags(opsch)	((opsch)->flags)

#ifdef KEY
#define OPSCH_depth(opsch)			((opsch)->depth)
#define OPSCH_id(opsch)				((opsch)->id)
#define OPSCH_op(opsch)				((opsch)->op)
#define OPSCH_ancestors(opsch)			((opsch)->ancestors)
#define OPSCH_descendants(opsch)		((opsch)->descendants)
#define OPSCH_least_constrained_int(opsch)	((opsch)->least_constrained_int)
#define OPSCH_least_constrained_fp(opsch)	((opsch)->least_constrained_fp)
#define OPSCH_num_blockers(opsch)		((opsch)->num_blockers)
#define OPSCH_one_set_mark(opsch)		((opsch)->one_set_mark)
#endif

//
// Define all the flags used to associate information with OPs in the 
// local scheduler. 

// OP has been scheduled.
#define OPSCH_SCHEDULED		0x0001
// OP has been visited. This is a temporary flag and needs to be reset 
// before use.
#define OPSCH_VISITED		0x0002 

// The following two flags identify addiu/daddiu OPs and load/store OPs
// respectively, that can be switched. 
#define OPSCH_ADDIU		0x0004
#define OPSCH_LDST		0x0008
// op defines an operand of an xfer-op. This is used to schedule such 
// instructions as far away as possible from the xfer-op (for T5).
#define OPSCH_DEF_XFER_OPND	0x0010

// The following two flags identify store OPs and load OPs than can be
// interchanged.
#define OPSCH_ST               0x0020
#define OPSCH_LD               0x0040

#define OPSCH_ADDIU_LDST_PAIR	 (OPSCH_ADDIU | OPSCH_LDST)
#define OPSCH_ST_LD_PAIR         (OPSCH_ST | OPSCH_LD)

#define OPSCH_scheduled(opsch)	   (OPSCH_flags(opsch) & OPSCH_SCHEDULED)
#define Set_OPSCH_scheduled(opsch) (OPSCH_flags(opsch) |= OPSCH_SCHEDULED)
#define OPSCH_visited(opsch)	   (OPSCH_flags(opsch) & OPSCH_VISITED)
#define Set_OPSCH_visited(opsch)   (OPSCH_flags(opsch) |= OPSCH_VISITED)
#define Reset_OPSCH_visited(opsch) (OPSCH_flags(opsch) &= ~OPSCH_VISITED)
#define OPSCH_addiu(opsch)	   (OPSCH_flags(opsch) & OPSCH_ADDIU)
#define Set_OPSCH_addiu(opsch)     (OPSCH_flags(opsch) |= OPSCH_ADDIU)
#define OPSCH_ldst(opsch)	   (OPSCH_flags(opsch) & OPSCH_LDST)
#define Set_OPSCH_ldst(opsch)      (OPSCH_flags(opsch) |= OPSCH_LDST)
#define OPSCH_def_xfer_opnd(opsch) (OPSCH_flags(opsch) & OPSCH_DEF_XFER_OPND)
#define Set_OPSCH_def_xfer_opnd(opsch) (OPSCH_flags(opsch) |= OPSCH_DEF_XFER_OPND)
#define OPSCH_st(opsch)	           (OPSCH_flags(opsch) & OPSCH_ST)
#define Set_OPSCH_st(opsch)        (OPSCH_flags(opsch) |= OPSCH_ST)
#define OPSCH_ld(opsch)	           (OPSCH_flags(opsch) & OPSCH_LD)
#define Set_OPSCH_ld(opsch)        (OPSCH_flags(opsch) |= OPSCH_LD)

// BB_OP_MAP to store information for each OP. Each entry
// is a pointer to an OPSCH structure. 

inline OPSCH* 
OP_opsch(OP *op, BB_MAP value_map) {
  BB_OP_MAP op_map = (BB_OP_MAP) BB_MAP_Get(value_map, OP_bb(op));
  return ((OPSCH *) BB_OP_MAP_Get(op_map, op));
}

// ======================================================================
// REG_ENTRY: Data structure to keep track of register variables (defined,
//            used,.. etc). The scheduling phase before register allocation 
//            uses the info to minimize register usages.
// ======================================================================
typedef union {
  void *ptr;
  struct {
    mINT16 def_count;
    mBOOL reg_assigned;
  }s;
} REG_ENTRY;

#define REG_ENTRY_ptr(re)		(re.ptr)
#define REG_ENTRY_def_count(re)		(re.s.def_count)
#define REG_ENTRY_reg_assigned(re)	(re.s.reg_assigned)

// ======================================================================
// BBSCH: Data structure to keep track of all properties pertaining to a
//        block (eg. schedule length, block parallelism, number of real ops,
//	  local register cost, global register costs, etc..)
// ======================================================================

/* Information about the schedule for a basic block. */
typedef struct {
  mINT16 schedule_length;
  mINT16 block_parallelism;
  mINT16 num_real_ops;
  mINT16 num_align_ops;
  mINT8  *local_regcost;
  INT8  global_regcost;
  INT    bb_start_pc;
  mUINT16 flags;
} BBSCH;

#define BBSCH_schedule_length(s)	((s)->schedule_length)
#define BBSCH_block_parallelism(s)	((s)->block_parallelism)
#define BBSCH_num_real_ops(s)           ((s)->num_real_ops)
#define BBSCH_num_align_ops(s)          ((s)->num_align_ops)
#define BBSCH_local_regcost(s)          ((s)->local_regcost)
#define BBSCH_global_regcost(s)         ((s)->global_regcost)
#define BBSCH_bb_start_pc(s)            ((s)->bb_start_pc)
#define BBSCH_flags(s)			((s)->flags)

#define Set_BB_num_align_nops(bbsch, value) ((bbsch)->num_align_ops = (value))
#define Set_BB_local_regcost(bbsch, value) ((bbsch)->local_regcost = (value))
#define Set_BB_global_regcost(bbsch, value) ((bbsch)->global_regcost = (value))
#define Set_BB_num_real_ops(bbsch, value) ((bbsch)->num_real_ops = (value))
#define Set_BB_start_pc(bbsch, value)     ((bbsch)->bb_start_pc = (value))
#define Set_BB_flags(bbsch, value)        ((bbsch)->flags = (value))

extern BOOL Trace_HB;

// ====================================================================
//
// Priority_Selector
//
// This class is responsible for describing the base configuration of
// priority selection of <ops> within a basic block. Each individual
// hueristic can provide its own iterator functions which decide the
// order of picking the instructions.
//
// ====================================================================

class HB_Schedule;

class Priority_Selector {
protected:
  BB* _curbb;
  std::list<BB*> _curbb_list;
  OP *_best_op;
#ifdef KEY
  OP* _last_sched_op;
#endif
  HB_Schedule *_cur_sched;
  MEM_POOL *_pool; 
  HBS_TYPE _hbs_type;

  virtual BOOL Is_OP_Better (OP *cur_op, OP *best_op);
public:
  // Constructor/destructor:
  Priority_Selector(std::list<BB*> bblist, HB_Schedule *sched, HBS_TYPE type, MEM_POOL *pool) 
    { _curbb_list = bblist; _cur_sched = sched; _hbs_type = type; _pool = pool;
#ifdef KEY
    _last_sched_op = NULL;
#endif // KEY
    }
  Priority_Selector(BB* bb, HB_Schedule *sched, HBS_TYPE type, MEM_POOL *pool) 
    { _curbb = bb; _cur_sched = sched; _hbs_type = type; _pool = pool;
#ifdef KEY
    _last_sched_op = NULL;
#endif // KEY    
    }
  ~Priority_Selector() {}

  // Tracing:
  void Print (FILE *f, const char* str) const;
  void Trace (const char* str) const;

  // Member access:
  std::list<BB*>& BB_List(void) { return _curbb_list;}

  // copy constructor:
  void operator=(const Priority_Selector& p) { 
    _curbb = p._curbb; 
    _curbb_list = p._curbb_list; 
    _best_op = p._best_op;
#ifdef KEY
    _last_sched_op = p._last_sched_op;
#endif
  }

  // Iterator functions:
  BOOL operator==(const OP*) const;
  BOOL operator!=(const OP*) const;
  virtual void* Get_Next_Element(HB_Schedule *sched);

  // utility functions:
  void Add_Element_Sorted(VECTOR vector, void* element, VECTOR_ELEMENT_COMPARE comp_func);
#ifdef KEY
  int Sched_OP_With_Preallocated_TN(OP *op);
#endif

  // Extraneous functions:
  OP* Select_OP_For_Delay_Slot(OP*);
  void Build_Ready_Vector (BB*, BOOL is_fwd);
  void Build_Ready_Vector (std::list<BB*>, BOOL is_fwd);
  virtual inline BOOL Is_Fwd_Schedule() { return FALSE; }
};

class List_Based_Bkwd : public Priority_Selector {
public:

  // Constructor/destructor:
  List_Based_Bkwd(BB* bb, HB_Schedule *sched, HBS_TYPE type, MEM_POOL *pool);
  List_Based_Bkwd(std::list<BB*> bblist, HB_Schedule *sched, HBS_TYPE type, MEM_POOL *pool);
  ~List_Based_Bkwd() {}
 
  // Iterator functions:
  BOOL operator==(const OP*) const;
  BOOL operator!=(const OP*) const;
  inline BOOL Is_Fwd_Schedule() { return FALSE; }
  void* Get_Next_Element(HB_Schedule *cur_sched)
    { return Priority_Selector::Get_Next_Element(cur_sched); }

  // Extraneous functions:
};

class List_Based_Fwd : public Priority_Selector {
protected:
  BOOL Is_OP_Better (OP *cur_op, OP *best_op);
public:

  // Constructor/destructor:
  List_Based_Fwd(BB* bb, HB_Schedule *sched, HBS_TYPE type, MEM_POOL *pool);
  List_Based_Fwd(std::list<BB*> bblist, HB_Schedule *sched, HBS_TYPE type, MEM_POOL *pool);
  ~List_Based_Fwd() {}
 
  // Iterator functions:
  BOOL operator==(const OP*) const;
  BOOL operator!=(const OP*) const;
  inline BOOL Is_Fwd_Schedule() { return TRUE; }
  void* Get_Next_Element(HB_Schedule *cur_sched);

  // Extraneous functions:
};

// ====================================================================
//
// Cycle_Selector:
//
// This class provides the base configuration in picking the 
// appropriate cycle times once the <op> has been picked by the
// <Priority_Selector> to be scheduled. The individual heuristics are
// responsible for providing their own separate iterator functions.
//
// ====================================================================
class Cycle_Selector {
protected:
  OP *_cur_op;
  INT _etime;
  INT _ltime;
  INT _cur_time;
public:

  // Constructor/destructor:
  Cycle_Selector() {}
  ~Cycle_Selector(void) {}

  void virtual Init(OP *op, INT etime, INT ltime) 
     { _cur_op = op; _etime = etime; _ltime = ltime; }

  // Iterator functions:

  // Extraneous functions:
  virtual INT  Get_Cycle() { return _cur_time; }
  virtual INT Next_Cycle() { return -1; }
  virtual INT Bound() { return 0; }
};


class Bkwd_Cycle_Sel : public Cycle_Selector {
public:

  Bkwd_Cycle_Sel() {}
  ~Bkwd_Cycle_Sel() {}

  void Init(OP *op, INT etime, INT ltime) 
     { _cur_op = op; _etime = etime; _ltime = ltime; _cur_time = ltime;}

  // Iterator functions:

  // Extraneous functions:
  INT  Get_Cycle()	{ return _cur_time; }
  INT  Next_Cycle()	{ return --_cur_time; }
  INT Bound() 		{ return 0; }
};

class Fwd_Cycle_Sel : public Cycle_Selector {
public:

  Fwd_Cycle_Sel() {}
  ~Fwd_Cycle_Sel(void) {}

  void Init(OP *op, INT etime, INT ltime) 
     { _cur_op = op; _etime = etime; _ltime = ltime; _cur_time = etime;}

  // Iterator functions:

  // Extraneous functions:
  INT  Get_Cycle()	{ return _cur_time; }
  INT  Next_Cycle()	{ return ++_cur_time; }
  INT Bound() 		{ return -1; }
};

// ====================================================================
//
// HB_Schedule:
//
// This class provides the infra-structure required to schedule a 
// block/region. It interacts with the above two descriptors
// (Priority_Selector and Cycle_Selector) in
// achieving the desired order. 
//
// ====================================================================
class HB_Schedule {
friend class Cycle_Selector;
friend class Priority_Selector;
protected:
  BB_MAP            _hb_map;
private:
  HBS_TYPE          _hbs_type;
  MEM_POOL          _hb_pool;
  MEM_POOL          _hb_map_pool;
  BB*               _prolog_bb;
  BB*               _epilog_bb;
  VECTOR            _ready_vector;
  VECTOR            _sched_vector;
  INT32 	    _Cur_Regs_Avail[ISA_REGISTER_CLASS_MAX+1];
  BB*               _prolog_mbb;
  BB*               _epilog_mbb;
  hTN_MAP           _regs_map;
  TI_RES_RES*       _rr_tab;
  TI_BUNDLE         *_bundle;
  INT32		    _max_sched;
#ifdef KEY
  OPSCH_SET	    *_scheduled_opschs;
  INT32		    _ready_count;
  INT32		    _unsched_count;
  INT32		    _ready_fp_percentage;
  INT32		    _unsched_fp_percentage;
  INT32		    _one_set_counter;
#endif

  // private functions:
  BOOL Avoid_Processing_HB(std::list<BB*>);
  void Invoke_Pre_HBS_Phase (BB*);
  void Invoke_Post_HBS_Phase (BB*);
  void Invoke_Pre_HBB_Phase (std::list<BB*>);
  void Invoke_Post_HBB_Phase (std::list<BB*>);
  INT  Calculate_Etime (OP*);
  INT  Calculate_Ltime (OP*);
  void Set_Resource_Usage (OP*);

  // Check if there are any resource conflicts in scheduling 'op' at
  // 'cycle'. Returns TRUE if all required resources can be reserved.
  inline BOOL Check_Resource_Usage(OP *op, INT cycle) {
    return TI_RES_RES_Resources_Available(_rr_tab, OP_code(op), cycle);
  }

  void Compute_BBSCH (BB*, BBSCH*);
  BOOL Can_Schedule_Op (OP *cur_op, INT cur_time);
  void Initialize (); 
#ifdef KEY
  void Schedule_Block (BB*, BBSCH*, int scheduling_algorithm);
#else
  void Schedule_Block (BB*, BBSCH*); 
#endif
  void Schedule_Blocks (std::list<BB*>&); 
  void Put_Sched_Vector_Into_BB (BB*, BBSCH*, BOOL);
  void Put_Sched_Vector_Into_HB (std::list<BB*>&);
  void Add_OP_To_Sched_Vector (OP*, BOOL);
  void Adjust_Ldst_Offsets (void);
#ifdef KEY
  void Adjust_Ldst_Offsets (BOOL is_fwd);
  void Update_Least_Constrained (OPSCH *, BOOL);
  void DFS_Update_Least_Constrained (OPSCH *, BOOL);
  void Drop_Remaining_Prefetches (BB *);

  // One set operations.
  void Clear_One_Set(void)		{ _one_set_counter++; }
  BOOL One_Set_MemberP(OPSCH *opsch)
    { return OPSCH_one_set_mark(opsch) == _one_set_counter; }
  void One_Set_Union1(OPSCH *opsch)
    { OPSCH_one_set_mark(opsch) = _one_set_counter; }
  void One_Set_Difference1(OPSCH *opsch)
    { OPSCH_one_set_mark(opsch) = _one_set_counter - 1; }
#endif
  void Init_Register_Map (BB*);
  void Init_RFlag_Table (std::list<BB*>&, BOOL);
  void Update_Regs_For_OP (OP*);

public:

  // Constructor/Destructors:

  // HB_Schedule can schedule either a single block or a hyperblock.
  // HBS_TYPE specifies the context in which the scheduler is being invoked.
  // BBSCH* is used to return info about the schedule generated.
  // The <regs_avail> parameter (if not NULL) gives an estimate of the
  // number of registers available in each register class.

  HB_Schedule ();
  ~HB_Schedule();

  //  Set members
  void Set_hbs_type(HBS_TYPE type) { _hbs_type = type; }
  void Set_hbs_before_gra(void) { _hbs_type = (HBS_TYPE) (_hbs_type | HBS_BEFORE_GRA); }
  void Set_hbs_before_lra(void) { _hbs_type = (HBS_TYPE) (_hbs_type | HBS_BEFORE_LRA); }
  void Set_hbs_critical_path(void) { _hbs_type = (HBS_TYPE) (_hbs_type | HBS_CRITICAL_PATH); }
  void Set_hbs_depth_first(void) { _hbs_type = (HBS_TYPE) (_hbs_type | HBS_DEPTH_FIRST); }
  void Set_hbs_minimize_regs(void) { _hbs_type = (HBS_TYPE) (_hbs_type | HBS_MINIMIZE_REGS); }
  void Set_hbs_from_gcm(void) { _hbs_type = (HBS_TYPE) (_hbs_type | HBS_FROM_GCM); }
  void Set_hbs_from_pre_gcm_sched(void) { _hbs_type = (HBS_TYPE) (_hbs_type | HBS_FROM_PRE_GCM_SCHED); }   
  void Set_hbs_from_post_gcm_sched(void) { _hbs_type = (HBS_TYPE) (_hbs_type | HBS_FROM_POST_GCM_SCHED); }   
  void Set_hbs_minimize_bundles(void) { _hbs_type = (HBS_TYPE) (_hbs_type | HBS_MINIMIZE_BUNDLES); }
#ifdef KEY
  void Set_hbs_balance_ready_types(void) { _hbs_type = (HBS_TYPE) (_hbs_type | HBS_BALANCE_READY_TYPES); }
  void Set_hbs_balance_unsched_types(void) { _hbs_type = (HBS_TYPE) (_hbs_type | HBS_BALANCE_UNSCHED_TYPES); }
  void Set_hbs_drop_unsched_prefetches(void) { _hbs_type = (HBS_TYPE) (_hbs_type | HBS_DROP_UNSCHED_PREFETCHES); }
#endif

  // Member access functions:
  HBS_TYPE type(void)        const { return _hbs_type; }
  BOOL HBS_Before_GRA(void)  const { return _hbs_type & HBS_BEFORE_GRA; }
  BOOL HBS_Before_LRA(void)  const { return _hbs_type & HBS_BEFORE_LRA; }
  BOOL HBS_Critical_Path(void) const { return _hbs_type & HBS_CRITICAL_PATH; }
  BOOL HBS_Depth_First(void) const { return _hbs_type & HBS_DEPTH_FIRST; }
  BOOL HBS_Minimize_Regs(void) const { return _hbs_type & HBS_MINIMIZE_REGS; }
  BOOL HBS_Minimize_Bundles(void) const { return _hbs_type & HBS_MINIMIZE_BUNDLES; }
#ifdef KEY
  BOOL HBS_Balance_Ready_Types(void) const { return _hbs_type & HBS_BALANCE_READY_TYPES; }
  BOOL HBS_Balance_Unsched_Types(void) const { return _hbs_type & HBS_BALANCE_UNSCHED_TYPES; }
  BOOL HBS_Drop_Unsched_Prefetches(void) const { return _hbs_type & HBS_DROP_UNSCHED_PREFETCHES; }
#endif
  BOOL HBS_From_GCM(void) const { return _hbs_type & HBS_FROM_GCM; }
  BOOL HBS_From_CGPREP(void) const { return _hbs_type & HBS_FROM_CGPREP; }
  BOOL HBS_From_Pre_GCM_Sched(void) const { return _hbs_type & HBS_FROM_PRE_GCM_SCHED; }
  BOOL HBS_From_Post_GCM_Sched(void) const { return _hbs_type & HBS_FROM_POST_GCM_SCHED; }
  VECTOR ready_vector(void) { return _ready_vector; }
  VECTOR sched_vector(void) { return _sched_vector; }
  BB_MAP hb_map(void) { return _hb_map; }
  TI_BUNDLE* bundle(void)    { return _bundle; }
  hTN_MAP regs_map(void) { return _regs_map; }
  TI_RES_RES* rr_tab(void) { return _rr_tab; }
#ifdef KEY
  void Update_Schedule_Parameters(void);
  INT32 Ready_Vector_Fp_Count(void);

  INT32 Ready_Count(void)		    { return _ready_count; }
  INT32 Ready_Fp_Percentage(void)	    { return _ready_fp_percentage; }
  INT32 Unsched_Count(void)		    { return _unsched_count; }
  INT32 Unsched_Fp_Percentage(void)	    { return _unsched_fp_percentage; }
#endif

  // Exported functions:
  void Init (BB*, HBS_TYPE, INT32, BBSCH*, mINT8*);
  void Init (std::list<BB*>, HBS_TYPE, mINT8*);
  INT Find_Schedule_Cycle(OP*, BOOL);
  void Estimate_Reg_Cost_For_OP (OP*);
#ifdef KEY
  void Schedule_BB (BB*, BBSCH*, int scheduling_algorithm = -1);
#else
  void Schedule_BB (BB*, BBSCH*);   
#endif
  void Schedule_HB (std::list<BB*>);   
};


// Other Exported utility routines

extern INT Memory_OP_Offset_Opndnum (OP *op);
extern INT Memory_OP_Base_Opndnum (OP *op);
extern BOOL Reschedule_BB(BB*);
extern BOOL Can_Schedule_HB(std::list<BB*> hb_blocks);
extern BOOL Is_Ldst_Addiu_Pair (OPSCH*, OPSCH*, OP*, OP*);
extern void Fixup_Ldst_Offset(OP*, INT64, INT64, HBS_TYPE);
extern void Compute_OPSCH(BB *bb, BB_MAP value_map, MEM_POOL *pool,
			  BOOL compute_bitsets = FALSE, BOOL is_fwd = FALSE);
extern void Compute_OPSCHs(std::list<BB*> bblist, BB_MAP value_map,
			   MEM_POOL *pool, BOOL compute_bitsets = FALSE,
			   BOOL is_fwd = FALSE);

// Trace Utility routines
extern void Print_BB_For_HB (std::list<BB*> bblist);

#endif /* hb_sched_INCLUDED */

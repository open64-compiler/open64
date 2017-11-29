/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

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


//-*-c++-*-
//                     Machine Modeling
//                     ----------------
//
// Description:
//
//     These are routines used by lno to model the scheduling of inner loops.
//     LNO uses this model to select which loop to make innermost and to
//     select unrolling factors.
//
//
//
// Exported types and functions:
//
//	LOOP_MODEL
//
//		What loop should be inner?  How much should we register
//		block each loop.
//
//	    INT Num_Fp_Regs() const
//
//		Estimate of how many fp registers it would take for the
//		inner loop.  If exceeds Target_FPRs, answer is nothing more
//		precise than just "too many".
//
//	    INT Num_Tlb() const
//
//		Estimate of how many tlb entries required (conservatively 
//		set to number of array bases)
//
//	    INT Block_Number(INT i) const
//
//		How much should we register block loop number i.
//		The i refers to the loop *before* reordering.
//		Note: 1 for none.
//
//          INT Iloop(INT i) const
//          INT Iloop()
//          INT Stripsz(INT i) const
//          INT* Stripsz()
//          INT& Nstrips()
//          INT Stripdepth() const
//          INT Striplevel(INT i) const
//          INT* Striplevel() const
//
//              The description of cache blocking.  After reordering.
//              To transform
//                      do i
//                        do j
//              into
//                      do j'
//                        do i
//                          do j by B
//              we note that the first strip is the j loop: Iloop(0) => 1.
//              Stripsz(0) => B.  There is one strip Nstrips() => 1.  And
//              Finally, the stripdepth is the depth of the loop before which
//              we put all the strips, in this case Stripdepth() = 0
//              (the i loop).  Note all entries for Iloop had better be at
//              least the stripdepth.  Also note that for multi-level blocking,
//              it does make sense that Iloop(i) might return the same loop
//              for different strips, although stripsz for the first would
//              presumably be larger than that for the second.  Striplevel
//              is the cache/memory hierarcy level we are blocking for.
//
//              There is no blocking if Nstrips() == 0.  Note that Iloop()[i]
//              is equivalent to Iloop(i), and likewise for Stripsz().
//
//	    INT New_Order(INT i) const
//
//		The permutation to apply.  New_Order(0) is which loop
//		is made to be the outermost loop.
//
//	    INT Inner_Loop() const
//
//		Which loop should be the inner loop
//
//	    double Num_Cycles() const
//
//		How many cycles does one iteration take.  An answer of
//		-1.0 implies that we could not make an estimate.
//
//	    LOOP_MODEL(WN *wn, BOOL *can_be_inner, BOOL *can_be_unrolled,
//		INT outermost_can_be_tiled, 
//		ARRAY_DIRECTED_GRAPH16 *array_graph,
//		class SX_INFO *pi, 
//		INT SNL_Depth, HASH_TABLE<WN *,BIT_VECTOR *> *invar_table)
//
//		Model the inner loop given by wn.  can_be_inner[i] is
//		TRUE iff it is legal to make loop 'i' the inner loop.
//		can_be_unrolled[i] is TRUE iff it is legal to register
//		block loop 'i'.  outermost_can_be_tiled says that loop i
//		can participate in a tile and so can all the loops inside.
//		SNL_Depth is the number of loops
//  		were are considering in the transformation.
//
//		invar_table if non-null maps expressions to bit vectors representing whether
//		or not expressions are invariant in some loop, invariant expressions,
//		whether inner or outer will be moved out, so they don't take computational
//		resources
//
//
//	    LOOP_MODEL()
//		used in conjunction with next call
//
//	    void MODEL(WN *wn, BOOL *can_be_inner, BOOL *can_be_unrolled,
//		INT outermost_can_be_tiled, 
//		ARRAY_DIRECTED_GRAPH16 *array_graph,
//		class SX_INFO *pi, 
//		INT SNL_Depth, HASH_TABLE<WN *,BIT_VECTOR *> *invar_table)
//
//		Model an uninited LOOP_MODEL
//
//	    ~LOOP_MODEL
//
//		This must be called as this structure uses the malloc 
//		MEM_POOL
//
//	REGISTER_MODEL
//
//		How many registers will a set of statements require.
//		Note that this class isn't used by LOOP_MODEL.  LOOP_MODEL
//		effectively inline computes the functionality given here.
//		This class is provided so that users outside of model
//		can estimate register usage.
//
//	    REGISTER_MODEL(MEM_POOL *pool) 
//
//		Create a new model.  Use pool for computation.
//
//	    void Add_Statement(WN *wn)
//
//		Add a statement to the system.
//
//	    WN *Statement(INT i)
//
//		Return statement i
//
//	    INT Num_Statements()
//
//	    ~REGISTER_MODEL()
//
//	ARRAY_REF
//
//		A stack of lists of array references.  Each element
//		in the stack corresponds to a list of all the
//		references for one base array. 
//
//	    void Calc_Regs_And_Refs(INT *num_regs, INT *num_refs,
//			INT *num_variant_stores, INT *num_invariant_stores) 
//
//		How many registers and references are needed by this
//		stack.  This just looks at _is_cse, _is_dup and _is_invar, 
//		and _has_dup_loads.  It doesn't calculate these bits.
//		The number of references is the number of non-invariants
//		on the stack (counting cses that are both loaded and stored
//		as two).  The number of registers is the number of invariants
//		plus the number of cses.
//		Also sets *num_variants_stores and *num_invariant_stores to
//		the number of stores that are loop variant and invariant
//		respectively
//
//	    INT Conflict_Refs(BOOL *can_be_unrolled, INT num_loops)
//
//		How many references are of maximal dimensionality (>= 2) and
//		are invariant in some outer unrollable loop
//
//	    INT Num_Bad() const
//
//		How many bad references?  These are LOAD/STORE of non-arrays
//		or too_messys.  These references are not on any list.  We 
//		increment the counter instead.
//
//	    INT Num_Refs() const
//
//		How many refs are on the list
//
//	    void Remove_Cse(INT inner, INT max_dist, INT step)
//
//		Remove all duplicates or CSEs given that inner is the inner
//		loop.  Two references are a cse if they are duplicates except 
//		with a distance < max_dist in the inner loop.
//		Mark remaining copy as a cse
//		Set _has_store and has_load of remaining copy to union of
//		fields over all members of the equivalence class
//		Step is the step size of the inner loop.  Zero implies not 
//	        constant.
//
//	    void Remove_Invariants(INT loop_no)
//
//		Remove all array references that would be invariant
//		if loop number loop_no (counting from the outermost)
//		was made innermost.
//
//	    void Mark_Invariants(INT loop_no)
//
//		Mark all array references that would be invariant
//		if loop number loop_no (counting from the outermost)
//		was made innermost.
//
//	    INT Num_Invariants(INT loop_no)
//
//		How many array references are invariant
//
//	    void Unroll(INT loop_no, INT num_copies)
//
//		Mimic the effect of unrolling loop "loop_no" 
//		"num_copies" times.  For example, given a reference
//		a[i][j], if we unroll 'i' three times, we replace the
//		reference with the three references a[3i][j], a[3i+1][j]
//		and a[3i+2][j] (we multiply by three since we're not chaging
//		the step size).  If a reference is invariant with respect
//		to loop_no, we mark it as a duplicate and do "not" make
//		loop_no copies.
//
//	    ARRAY_REF(WN *wn, INT SNL_Depth, MEM_POOL *pool,
//		HASH_TABLE<WN *,BIT_VECTOR *> *invar_table)
//
//		Build the reference stack
//		Wn points to all the code in the loop.
//		SNL_Depth is the number of loops in the SNL we are modelling. 
//
//	    ARRAY_REF(MEM_POOL *pool)
//
//		Create a new ARRAY_REF
//
//	    void Add_References(WN *wn, INT SNL_Depth)
//
//		Add all the references under wn to the stack.
//		SNL_Depth is the number of loops in the SNL we are modelling. 
//		This routine may be called incrementally
//
//	    ARRAY_REF(ARRAY_REF *orig, MEM_POOL *pool)
//
//		Copy the stack
//
//	    void Print(FILE *fp);
//
//	    void Push(ARRAY_REF_LIST *arl)
//
//		Add a new list to the stack
//
//	    ARRAY_REF_LIST *Array_Ref_List(INT i)
//
//		Return the i'th list (0 is bottom)
//
//	    INT Elements() const
//
//		How many elements on the stack.
//
//	    ~ARRAY_REF()
//
//
//	ARRAY_REF_LIST
//
//		A list of array references.  To avoid walking the code
//		multiple times, we collect all the array references in
//		the loop.
//
//	    ARRAY_REF_LIST(MEM_POOL *pool, SYMBOL *base_array)
//
//		Create a new list. 
//		base_array is the base_array of all the references on the
//		list
//
//	    void Calc_Regs_And_Refs(INT *num_regs, INT *num_refs 
//			INT *num_variant_stores, INT *num_invariant_stores) 
//
//		How many registers and references are needed by this
//		stack.  This just looks at _is_cse, _is_dup and _is_invar, 
//		and _has_dup_loads.  It doesn't calculate these bits.
//		The number of references is the number of non-invariants
//		on the stack (counting cses that are both loaded and stored
//		as two).  The number of registers is the number of invariants
//		plus the number of cses.
//		Also sets *num_variants_stores and *num_invariant_stores to
//		the number of stores that are loop variant and invariant
//		respectively
//
//	    void Remove_Cse(INT inner, INT max_dist, INT step)
//
//		Remove all duplicates or CSEs given that inner is the inner
//		loop.  Two references are a cse if they are duplicates except 
//		with a distance < max_dist in the inner loop.
//		Mark remaining copy as a cse
//		Step is the step size of the inner loop.  Zero implies not 
//	        constant.
//
//
//	    void Remove_Invariants(INT loop_no)
//
//		Remove all array references that would be invariant
//		if loop number loop_no (counting from the outermost)
//		was made innermost.
//
//	    void Unroll(INT loop_no, INT num_copies)
//
//		Mimic the effect of unrolling loop "loop_no" 
//		"num_copies" times.  For example, given a reference
//		a[i][j], if we unroll 'i' three times, we replace the
//		reference with the three references a[3i][j], a[3i+1][j]
//		and a[3i+2][j] (we multiply by three since we're not chaging
//		the step size).  If a reference is invariant with respect
//		to loop_no, we mark it as a duplicate and do "not" make
//		loop_no copies.
//
//	    void Mark_Invariants(INT loop_no)
//
//		Mark all array references that would be invariant
//		if loop number loop_no (counting from the outermost)
//		was made innermost.
//
//	    INT Num_Invariants(INT loop_no)
//
//		How many unique invariant array references are there
//
//	    INT Num_Refs() const
//
//		How many refs are on the list
//
//
//	    ARRAY_REF_LIST(ARRAY_REF_LIST *orig, MEM_POOL *pool)
//
//		Copy the stack
//
// 	    void Print(FILE *fp);
//
//
//	     SYMBOL *Base_Array
//
//		To which base array do all these references refer.
//
//	     BOOL Is_Scalar_Expanded() const
//
//		Is the array really a scalar expanded array
//
//	ARRAY_REF_NODE
//
//		One element of an ARRAY_REF_LIST
//
//	    ACCESS_ARRAY *Array 
//
//		The access array.  This may be the same array as in the original
//		code.  So, it should never be deleted.
//
//	    ARRAY_REF_NODE(ACCESS_ARRAY *array, BOOL is_store, 
//						mUINT16 lex_number)
//
//	    ARRAY_REF_NODE(ARRAY_REF_NODE *in) 
//
//		Create a copy of in, creating a new access array
//
// 	    void Print(FILE *fp);
//
//	ARRAY_REF_ITER
//
//
//	SYMBOL_TREE
//
//		A binary tree of symbols.  We use this to count the number
//		of unique scalars that are not stored.
//
//	    SYMBOL_TREE(MEM_POOL *pool)
//
//	    void Enter(SYMBOL *symbol, BOOL is_store, INT weight)
//
//		If this symbol isn't in the tree, enter it and set
//		its weight.  Either way, if is_store is TRUE, set the
//		node's is_store
//
//	    void Enter_Scalar_Refs(WN *wn, ARRAY_REF *ar, 
//			SX_INFO *pi,
//			BOOL can_be_inner, INT outer,
//			INT *num_scalar_refs)
//
//		Enter into the tree all the unqiue scalar refs 
//		rooted at wn.  Don't enter scalars that might be
//		scalar expanded.  Put them on the ARRAY_REF instead
//		outer is the outermost loop than can be inner
//		Increment num_scalar_refs by the number of scalar refs in the 
//		tree.  
//
//	    INT Num_Unstored() const 
//
//		Return the sum of the weights of all nodes that have never
//		been stored
//


/** $Revision: 1.8 $
*** $Date: 05/05/04 09:52:28-07:00 $
*** $Author: gautam@eng-27.pathscale.com $
*** $Source: ../../be/lno/SCCS/s.model.h $
**/

#ifndef MD_RCS_ID
#define MD_RCS_ID
#ifdef _KEEP_RCS_ID
static char *model_rcs_id = "$Source: ../../be/lno/SCCS/s.model.h $ $Revision: 1.8 $";
#endif /* _KEEP_RCS_ID */
#endif

#define USE_STANDARD_TYPES
#include <bstring.h>
#include "defs.h"
#ifndef access_vector_INCLUDED
#include "access_vector.h"
#endif
#ifndef cxx_graph_INCLUDED
#include "cxx_graph.h"
#endif
#ifndef graph_template_INCLUDED
#include "graph_template.h"
#endif
#ifndef lnoutils_INCLUDED
#include "lnoutils.h"
#endif
#ifndef dep_INCLUDED
#include "dep.h"
#endif
#ifndef lno_scc_INCLUDED
#include "lno_scc.h"
#endif
#ifndef lnopt_main_INCLUDED
#include "lnopt_main.h"
#endif
#ifndef snl_nest_INCLUDED
#include "snl_nest.h"
#endif
#include <sys/types.h>
#include "sxlist.h"
#ifndef _lno_bv_INCLUDED
#include "lno_bv.h"
#endif


#ifndef MODEL_DELCARE
#define MODEL_DELCARE

#if defined(TARG_MIPS) || defined(TARG_LOONGSON)
#define	Target_INTRs	32
#endif
#ifdef TARG_PPC32
#define	Target_INTRs	32
#endif
#ifdef TARG_IA64
#define	Target_INTRs	128
#endif
#ifdef TARG_IA32
#define	Target_INTRs	8
#endif
#ifdef TARG_X8664
#define	Target_INTRs	16
#endif

typedef HASH_TABLE<WN*,INT> WN2INT;

// full declarations are in "ti_res_count.h"
struct ti_res_count;
typedef struct ti_res_count TI_RES_COUNT;

// Returns TRUE if 'wn_ref' is reference which will be modelled as a 
// bad array, FALSE otherwise. 

extern BOOL Is_Bad_Array(WN* wn_ref, INT nloops); 
#ifdef TARG_X8664
extern BOOL Is_Vectorization_Beneficial(WN* loop);

// bug 5880
extern BOOL Is_Aggressive_Vintr_Loop(WN *loop);
#endif

class LOOP_MODEL {
  INT *_block_number_inner;
  INT *_iloop_inner;
  INT *_stripsz_inner;
  INT *_striplevel_inner;
  INT _nstrips_inner;
  INT _stripdepth_inner;
  INT *_new_order_inner;
  INT _inner_loop_inner;
  INT _num_fp_array_refs;  // how many fp array refs in the loop pre-unrolling
  INT _num_fp_scalar_refs; // how many fp scalar refs in the loop pre-unrolling
  INT _num_int_array_refs;  // how many int array refs in the loop pre-unrolling
  INT _num_int_scalar_refs; // how many int scalar refs in the loop pre-unrolling
  BOOL *_can_be_inner;
  INT _num_loops;
  double _num_cycles_inner;
  INT _num_fp_regs_inner;
  INT _num_fp_refs_inner;
  INT _num_int_regs_inner;
  INT _num_int_refs_inner;
  INT _unroll_prod_inner;
  INT64 *_est_num_iterations;  // how many iterations in each loop
  INT *_required_unroll;       // input: >0 -> must unroll by that amount
  INT *_required_blocksize;    // index as [MHD_MAX_LEVELS*depth + mhd_level]
  INT *_required_permutation;  // -1 means no restriction

  INT *_block_number;  // result, register blocking
  INT *_new_order; // result, cache blocking
  INT *_iloop;         // result, cache blocking
  INT *_stripsz;       // result, cache blocking
  INT *_striplevel;    // result, cache blocking
  INT _nstrips;        // result, cache blocking
  INT _stripdepth;     // result, cache blocking
  INT _inner_loop;     // results
  double _num_cycles;     // results
  INT _num_fp_regs;    // results
  INT _num_fp_refs;    // results
  INT _num_int_regs;    // results
  INT _num_int_refs;    // results
  INT _num_tlb;    // results
  INT _unroll_prod;    // results

  INT _num_evaluations;

  static INT _model_no;
  mBOOL _blocking_disabled;

  static TI_RES_COUNT *OP_Resources(WN *wn, double *num_fp_instr,
			HASH_TABLE<WN *,BIT_VECTOR *> *invar_table);
  static double OP_Cycles(class REGISTER_MODEL *rmodel, 
                          double *num_fp_instr,
                          HASH_TABLE<WN *,BIT_VECTOR *> *invar_table,
                          MEM_POOL *pool);
  static INT OP_Resources_R(WN *wn,TI_RES_COUNT *resource_count,
     double *num_fp_instr,HASH_TABLE<WN *,BIT_VECTOR *> *invar_table);
  static INT FP_Cycles_Madd(WN *wn,TI_RES_COUNT *resource_count,
     double *num_fp_instr, HASH_TABLE<WN *,BIT_VECTOR *> *invar_table);
  static INT OP_Cycles_Cvt(OPCODE opcode,TI_RES_COUNT *resource_count,
     double *num_fp_instr);
  static INT FP_Cycles_Intrinsic(WN *wn,TI_RES_COUNT *resource_count,
     double *num_fp_instr);
  INT Unique_Unstored_Fp_Scalar_Refs(WN *wn, class ARRAY_REF *ar,
					SX_INFO *pi);
  INT Unique_Unstored_Int_Scalar_Refs(WN *wn, class ARRAY_REF *ar,
					SX_INFO *pi);
  void Try_Inner(BOOL *can_be_unrolled, INT outermost_can_be_tiled,
		 INT inner, INT num_loops);
  void Try_Unroll(BOOL *can_be_unrolled, INT inner, INT num_loops,
		INT *unroll_factors,INT l, INT unroll_product,
		BOOL *can_reg_allocate, BOOL *try_further_unroll,
		class ARRAY_REF *arl);
  void Evaluate(INT inner, INT num_loops, INT *unroll_factors,
	INT unroll_product, BOOL *can_reg_allocate, BOOL *try_further_unroll, 
	ARRAY_REF *arl, BOOL *can_be_unrolled);
  void Model_Results_Analysis(INT inner, INT num_loops,INT outermost_can_be_tiled,
		double machine_cycles, double cache_cycles, double overhead_cycles);

  class ARRAY_REF *_arl; // list of all array references
  class LAT_DIRECTED_GRAPH16 *_lat_graph;  // graph of latencies
  WN *_wn;

  // estimates of components
  TI_RES_COUNT *_OP_resource_count;
  double *_loop_rcycles_unroll_by;
  double _LOOP_INIT_issue;
  double _OP_issue;
  double _issue_rate;
  double _latency_cycles;  // bound due to latency 
  INT _base_fp_regs;  // how many regs needed to keep pipeline full
  INT _base_int_regs;  // how many int regs needed in each inner loop
  INT _scalar_fp_regs;
  INT _scalar_int_regs;
  double _num_mem_units;

  enum MODEL_LIMIT {MODEL_LIMIT_UNSET, MODEL_LIMIT_IDEAL, MODEL_LIMIT_RES,
			MODEL_LIMIT_LAT};
  MODEL_LIMIT _model_limit;

  // Try to move the vectorizable outer loop into the innermost position
  //
  void Shift_Vectorizable_Innermost (void);

public:
  friend class REGISTER_MODEL;
  INT Num_Fp_Regs() const {return _num_fp_regs;}
  INT Num_Fp_Refs() const {return _num_fp_refs;}
  INT Num_Int_Regs() const {return _num_int_regs;}
  INT Num_Int_Refs() const {return _num_int_refs;}
  INT Num_TLB() const {return _num_tlb;}
  static INT Model_No() {return _model_no;}
  INT Block_Number(INT i) const { return _block_number[i];}
  INT Iloop(INT i) const {
    Is_True(_iloop[i] >= _stripdepth, ("Bad cache blocking info in model"));
    return _iloop[i];
  }
  INT *Iloop() { return _iloop;}
  INT Stripsz(INT i) const { return _stripsz[i];}
  INT *Stripsz() const { return _stripsz;}
  INT Striplevel(INT i) const { return _striplevel[i];}
  INT *Striplevel() const { return _striplevel;}
  INT& Nstrips() { return _nstrips;}
  INT Stripdepth() const { return _stripdepth;}
  INT New_Order(INT i) const { return _new_order[i];}
  INT Inner_Loop() const { return _inner_loop; }
  double Num_Cycles() const { return _num_cycles; }
  LOOP_MODEL() {;};
  LOOP_MODEL(WN *wn, BOOL *can_be_inner, BOOL *can_be_unrolled,
             INT outermost_can_be_tiled, 
             class ARRAY_DIRECTED_GRAPH16 *array_graph,
             class SX_INFO *pi, INT SNL_Depth,
	     HASH_TABLE<WN *,BIT_VECTOR *> *invar_table) {
    Model(wn,can_be_inner,can_be_unrolled,outermost_can_be_tiled,array_graph,
		pi,SNL_Depth,invar_table);
  }
  void Model(WN *wn, BOOL *can_be_inner, BOOL *can_be_unrolled,
             INT outermost_can_be_tiled, 
             class ARRAY_DIRECTED_GRAPH16 *array_graph,
             class SX_INFO *pi, INT SNL_Depth,
	     HASH_TABLE<WN *,BIT_VECTOR *> *invar_table); 
  ~LOOP_MODEL();
};

typedef STACK<WN *> STATEMENT_STACK;

class REGISTER_MODEL {
  MEM_POOL *_pool;
  STATEMENT_STACK *_statement_stack;
  double Count_Op();
  double Count_Op(WN *wn);
public:
  REGISTER_MODEL() {
    _pool = NULL;
    _statement_stack = NULL;
  }
  WN *Statement(INT i) {
    return _statement_stack->Bottom_nth(i);
  }
  INT Num_Statements() {
    return _statement_stack->Elements();
  }
  REGISTER_MODEL(MEM_POOL *pool) {
    _pool = pool;
    _statement_stack = CXX_NEW(STATEMENT_STACK(_pool),_pool);
  }
  void Init(MEM_POOL *pool) {
    _pool = pool;
    _statement_stack = CXX_NEW(STATEMENT_STACK(_pool),_pool);

  }
  void Add_Statement(WN *wn) {
    _statement_stack->Push(wn);
  }
  void Calculate_Register_Usage(
      WN *inner, INT *fp_regs_used_out, INT *int_regs_used_out, INT *tlb_out);
  void Evaluate(WN *inner, 
                WN2INT *se_needed, 
                HASH_TABLE<WN *,BIT_VECTOR *> *invar_table,
                double *loop_cycles, 
                INT *fp_regs_used_out, 
                INT *int_regs_used_out, 
                INT *tlb_out);
  ~REGISTER_MODEL() { CXX_DELETE(_statement_stack,_pool); };
};


class ARRAY_REF_NODE: public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS( ARRAY_REF_NODE);
  mUINT16 _lex_number;	// lexical ordering
  mUINT16 _unroll_copy[LNO_MAX_DO_LOOP_DEPTH];  // for each loop,
			// which unroll copy will this ref be in
  mUINT16 _first_ref_store;  // is the lexically first ref a store
  mBOOL _is_invariant; // is this invariant in the current loop of interest
  mBOOL _is_cse;      // is this a cse of another reference
  mINT16 _max_inner_offset;  // the maximum offset of a dimension refering
			     // to the inner loop variable (only meaningfull if 
			     // is_cse is true)
  mINT16 _min_inner_offset;  // the minimum offset of a dimension refering
			     // to the inner loop variable (only meaningfull if 
			     // is_cse is true)
  mBOOL _is_dup; // is this a duplicate 
  mBOOL _has_dup_loads;  // is this a duplicate with more than one load
  // is this a store or a load, since merge cses into one node we need two bits
  mBOOL _has_store;      // is this or one of its duplicates/cse a store
  mBOOL _has_load;      // is this or one of its duplicates/cse a load
  INT   _element_size;
public:
  ACCESS_ARRAY *Array;
  WN           *Wn;
  ARRAY_REF_NODE(ACCESS_ARRAY *array, WN* wn, BOOL is_store, INT element_size,
	mUINT16 lex_number) { 
    Array = array;
    Wn = wn;
    _lex_number = lex_number;
    for (INT i=0; i<array->Dim(0)->Nest_Depth(); i++) {
      _unroll_copy[i] = 0;
    }
    _is_invariant=FALSE;
    _is_cse=FALSE;
    _is_dup=FALSE;
    _has_dup_loads=FALSE;
    _max_inner_offset = INT16_MIN;
    _min_inner_offset = INT16_MAX;
    _has_store = is_store;
    _first_ref_store = is_store;
    _has_load = !is_store;
    _element_size = element_size;
  }
  ARRAY_REF_NODE(ARRAY_REF_NODE *in, MEM_POOL *pool) { 
    Array = CXX_NEW(ACCESS_ARRAY(in->Array,pool),pool); 
    Wn = in->Wn;
    _lex_number = in->_lex_number;
    INT bound = Array->Dim(0)->Nest_Depth();
    for (INT i=0; i<bound; i++) {
      _unroll_copy[i] = in->_unroll_copy[i];
    }
    _is_invariant = in->_is_invariant;
    _is_cse = in->_is_cse;
    _max_inner_offset = in->_max_inner_offset;
    _min_inner_offset = in->_min_inner_offset;
    _is_dup = in->_is_dup;
    _has_dup_loads = in->_has_dup_loads;
    _first_ref_store = in->_first_ref_store;
    _has_store = in->_has_store;
    _has_load = in->_has_load;
    _element_size = in->_element_size;
  }
  INT Element_Size() const {return _element_size;}
  void Print(FILE *fp) const;
  BOOL Lexically_Before(ARRAY_REF_NODE *node2);
  BOOL Has_Store() const {return _has_store;}
};

class ARRAY_REF_LIST: public SLIST {
  DECLARE_SLIST_CLASS( ARRAY_REF_LIST, ARRAY_REF_NODE )
  MEM_POOL *_pool;
  mBOOL _is_scalar_expanded;
public:
  ARRAY_REF_LIST(MEM_POOL *pool, SYMBOL *base_array) :
    _pool(pool), Base_Array(base_array) { _is_scalar_expanded = FALSE; }
  ARRAY_REF_LIST(ARRAY_REF_LIST *orig, MEM_POOL *pool); 
  void Calc_Regs_And_Refs(
	INT *num_fp_regs, INT *num_fp_refs,
	INT *num_fp_variant_stores, INT *num_fp_invariant_stores,
	INT *num_int_regs, INT *num_int_refs,
	INT *num_int_variant_stores, INT *num_int_invariant_stores) ;
  INT Conflict_Refs(INT max_dim, BOOL *can_be_unrolled, INT num_loops);
  void Remove_Cse(INT inner, INT max_dist, INT step);
  void Remove_Invariants(INT loop_no);
  void Mark_Invariants(INT loop_no);
  void Unroll(INT loop_no, INT num_copies);
  INT Num_Invariants(INT loop_no);
  BOOL Is_Scalar_Expanded() const { return _is_scalar_expanded;}
  INT Num_Fp_Refs() const;
  INT Num_Int_Refs() const;
  SYMBOL *Base_Array;
  void Print(FILE *fp) const;
  ~ARRAY_REF_LIST();
};

class ARRAY_REF_ITER: public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS( ARRAY_REF_ITER, ARRAY_REF_NODE ,ARRAY_REF_LIST)
public:
  ~ARRAY_REF_ITER() {}
};

class ARRAY_REF_CONST_ITER: public SLIST_ITER {
  DECLARE_SLIST_CONST_ITER_CLASS( ARRAY_REF_CONST_ITER, ARRAY_REF_NODE ,ARRAY_REF_LIST)
public:
  ~ARRAY_REF_CONST_ITER() {}
};

typedef STACK<ARRAY_REF_LIST *> ARRAY_REF_STACK;
typedef STACK<DO_LOOP_INFO *> DLI_STACK;
class ARRAY_REF
{
public:
  void Calc_Regs_And_Refs(
	INT *num_fp_regs, INT *num_fp_refs,
	INT *num_fp_variant_stores, INT *num_fp_invariant_stores,
	INT *num_int_regs, INT *num_int_refs,
	INT *num_int_variant_stores, INT *num_int_invariant_stores) ;
  INT Conflict_Refs(BOOL *can_be_unrolled, INT num_loops);
  void Remove_Cse(INT inner, INT max_dist, INT step);
  void Remove_Invariants(INT loop_no);
  void Unroll(INT loop_no, INT num_copies);
  void Mark_Invariants(INT loop_no);
  INT Num_Invariants(INT loop_no);
  ARRAY_REF(WN *wn, INT SNL_Depth, MEM_POOL *pool,
	HASH_TABLE<WN *,BIT_VECTOR *> *invar_table) : _stack(pool) {
    _pool = pool;
    _num_bad_fp = 0;
    _num_bad_int = 0;
    _lex_number = 0;
    Build(wn, SNL_Depth,invar_table);
  }
  INT Num_Fp_Refs() const;
  INT Num_Int_Refs() const;
  ARRAY_REF(MEM_POOL *pool) : _stack(pool) {
    _pool = pool;
    _num_bad_fp = 0;
    _num_bad_int = 0;
  }
  void Add_References(WN *wn, INT SNL_Depth,HASH_TABLE<WN *,BIT_VECTOR *> *invar_table) {
    Build(wn, SNL_Depth,invar_table);
  }

  void Enter_Scalar_Expand(WN *wn,SX_PNODE *pnode, BOOL *can_be_inner,
				INT num_loops);
  void Enter_Scalar_Expand(BIT_VECTOR *bv, WN *wn);
  void Enter_Innermost_Scalar_Expand(WN *wn);
  ARRAY_REF(ARRAY_REF *orig, MEM_POOL *pool);
  void Push(ARRAY_REF_LIST *arl) { _stack.Push(arl); }
  INT Num_Fp_Bad() const { return _num_bad_fp; };
  INT Num_Int_Bad() const { return _num_bad_int; };
  INT Num_Bad() const { return _num_bad_int+_num_bad_fp; };
  ARRAY_REF_LIST *Array_Ref_List(INT i) { return _stack.Bottom_nth(i); }
  const ARRAY_REF_LIST *Array_Ref_List(INT i) const { return _stack.Bottom_nth(i); }
  INT Elements() const { return _stack.Elements(); }
  void Print(FILE *fp) const;
  ~ARRAY_REF() {};
private:
  ARRAY_REF_STACK _stack;
  MEM_POOL *_pool;
  INT _num_bad_fp;
  INT _num_bad_int;
  void Build(WN *wn, INT SNL_Depth,HASH_TABLE<WN *,BIT_VECTOR *> *invar_table);
  void Build_Rec(WN *wn, DLI_STACK *dli_stack, INT SNL_Depth,
			HASH_TABLE<WN *,BIT_VECTOR *> *invar_table);
  void Build_Array(WN *wn, BOOL is_store, DLI_STACK *dli_stack, INT SNL_Depth);
  mUINT16 _lex_number;
};


// A graph to model latency

class LAT_VERTEX16: public VERTEX16 {
  WN *_wn;
public:
  LAT_VERTEX16(WN *wn) {_wn = wn;};
  friend class LAT_DIRECTED_GRAPH16;
};

class LAT_EDGE16 : public EDGE16 {
public:
  UINT16 Latency;
  UINT16 _num_unused_dim;
  friend class LAT_DIRECTED_GRAPH16;
  DEPV *Depv; // a null pointer implies an "all equals" dependence
	      // we use it to save space for non-loads/stores
};

class LAT_DIRECTED_GRAPH16 :
	public DIRECTED_GRAPH16<LAT_EDGE16,LAT_VERTEX16> {
  MEM_POOL *_pool;  // the pool for the Depvs
  mUINT8 _num_dim; // how many dimensions in each DEPV
  mUINT8 _num_bad;  // how many bad enclosing do loops
  HASH_TABLE<VINDEX16,VINDEX16> 
	_hash_table; // maps vertices in array graph to vertices in this
  ARRAY_DIRECTED_GRAPH16 *_array_graph;
public:
  LAT_DIRECTED_GRAPH16(mUINT16 num_v, mUINT16 num_e, 
        mUINT8 num_dim, mUINT8 num_bad, MEM_POOL *pool,
	ARRAY_DIRECTED_GRAPH16 *array_graph) :
	DIRECTED_GRAPH16<LAT_EDGE16,LAT_VERTEX16>(num_v,num_e),
	_hash_table(200,pool) {
    _num_dim = num_dim;
    _num_bad = num_bad;
    _array_graph = array_graph;
    _pool = pool;
    Lat_scc_vertex_map = NULL;
    Scc_lat_vertex_map = NULL;
  }
  ~LAT_DIRECTED_GRAPH16() {
    if (Scc_lat_vertex_map) CXX_DELETE_ARRAY(Scc_lat_vertex_map,_pool);
  }
  VINDEX16 *Lat_scc_vertex_map;  // map vertices in this to v's in scc_graph
  VINDEX16 *Scc_lat_vertex_map;  // map vertices in this to v's in scc_graph
  void Set_Scc_Graph(SCC_DIRECTED_GRAPH16 *scc_graph, INT inner);
  double Max_Cycle(INT inner,double lower_bound);
  BOOL Is_Valid(INT inner, EINDEX16 e);
  void Print(FILE *fp);
  EINDEX16 Add_Edge(VINDEX16 from, VINDEX16 to, DEPV *depv, INT latency,
			UINT16 num_unused_dim) {
    EINDEX16 result =
      DIRECTED_GRAPH16<LAT_EDGE16,LAT_VERTEX16>::Add_Edge(from,to);
    if (result != 0) {
      _e[result].Depv = depv;
      _e[result].Latency = latency;
      _e[result]._num_unused_dim = num_unused_dim;
    }
    return result;
  }
  INT Latency(EINDEX16 edge) {
    return(_e[edge].Latency);
  }
  DEPV *Depv(EINDEX16 edge) {
    return(_e[edge].Depv);
  }
  UINT16 Num_Unused_Dim(EINDEX16 edge) {
    return(_e[edge]._num_unused_dim);
  }
  VINDEX16 Add_Vertex(WN *wn) {
    VINDEX16 result =
      DIRECTED_GRAPH16<LAT_EDGE16,LAT_VERTEX16>::Add_Vertex();
    _v[result]._wn = wn;
    return result;
  }
  WN *Wn(VINDEX16 v) {
    return _v[v]._wn;
  }
  void Remove_Edge(EINDEX16 e) {
    DIRECTED_GRAPH16<LAT_EDGE16,LAT_VERTEX16>::Delete_Edge(e);
  }
  void Delete_Edge(EINDEX16 e) {
    CXX_DELETE_ARRAY(_e[e].Depv,_pool);
    DIRECTED_GRAPH16<LAT_EDGE16,LAT_VERTEX16>::Delete_Edge(e);
  }
  void Map_Vertex(VINDEX16 oldv, VINDEX16 newv) {
    _hash_table.Enter(oldv, newv);
  }
  INT Add_Flow_Edges();
  INT Add_Vertices_Op_Edges(WN *wn, HASH_TABLE<WN *,BIT_VECTOR *> *invar_table);
  mUINT8 Num_Dim() { return(_num_dim); }
  mUINT8 Num_Bad() { return(_num_bad); }
private:
  INT Add_Vertices_Op_Edges_Rec(VINDEX16 store,WN *wn, INT latency,
			HASH_TABLE<WN *,BIT_VECTOR *> *invar_table);
  INT FP_Latency_Madd(VINDEX16 store,WN *wn, INT latency,
			HASH_TABLE<WN *,BIT_VECTOR *> *invar_table);
  INT FP_Latency_Cvt(OPCODE opcode);
  INT FP_Latency_Intrinsic(WN *wn);
};

// Cost table, stores sets of maximal paths between i and j
// this is Floyd's algorithm as taken from the SWP as taken from Lam
class COST {
public:
  UINT16 Distance;
  UINT16 Latency;
};

class COST_V {
  UINT16 _length;
  UINT16 _alloc_length;
  COST *_costs;
public:
  COST_V();
  void Init() { _length = 0; }
  COST *Costs() { return _costs;};
  UINT16 Length() { return _length; }
  void Set_Length(UINT16 length) { _length = length; }
  void Push(UINT16 latency, UINT16 distance, MEM_POOL *pool);
};

class COST_TABLE {
  UINT16 _maxn;  // the amount of data (used if _n gets smaller)
  UINT16 _n;  // number of vertices
  COST_V *_data;
  MEM_POOL *_pool;
  double _min_ii;
public:
  COST_TABLE(UINT16 n, MEM_POOL *pool);
  void Realloc(UINT16 n);
  double Init(INT inner, LAT_DIRECTED_GRAPH16 *graph, 
	SCC_DIRECTED_GRAPH16 *scc_graph, INT scc_id, INT *scc_pos);
  void Print(FILE *fp);
  COST_V *Cost_V(UINT16 v1, UINT16 v2) { return &_data[_n*v1+v2];};
  double Solve(double init_min_ii);
private:
  void Push(UINT16 v1, UINT16 v2, UINT16 latency, UINT16 distance) {
    _data[_n*v1+v2].Push(latency,distance,_pool);
  }
  void Add_Maximal_Costs(COST_V *cvij, COST_V *cvik, COST_V *cvkj);
  BOOL Is_Max_Cost(INT dist, INT latency, COST_V *cv, INT offset);
  void Update_Min_II(COST_V *cv1, COST_V *cv2);
};


// data structures used for register pressure

class SYMBOL_TREE_NODE
{
  SYMBOL_TREE_NODE *_left;
  SYMBOL_TREE_NODE *_right;
  SYMBOL _symbol;
  BOOL _is_store;
  INT  _weight;
public:
  SYMBOL_TREE_NODE(SYMBOL symbol, BOOL is_store, INT weight) {
    _symbol = symbol;
    _is_store = is_store;
    _left = _right = NULL;
    _weight = weight;
  }
  INT Num_Fp_Unstored() const;
  INT Num_Int_Unstored() const;
  INT Symbol_Compare(SYMBOL *s) { // is this <,= or > s
    if (_symbol.ST_Base() < s->ST_Base()) return(-1);
    else if (_symbol.ST_Base() > s->ST_Base()) return(1);
    else if (_symbol.ST_Offset() < s->ST_Offset()) return(-1);
    else if (_symbol.ST_Offset() > s->ST_Offset()) return(1);
    else if (_symbol.WN_Offset() < s->WN_Offset()) return(-1);
    else if (_symbol.WN_Offset() > s->WN_Offset()) return(1);
    else return(0);
  }
  void Enter(SYMBOL *symbol, MEM_POOL *pool, BOOL Is_Store, INT weight);  

};

// binary tree of SYMBOL
class SYMBOL_TREE
{
  SYMBOL_TREE_NODE *_symbol_node;
  BOOL _is_floating_point;
  MEM_POOL *_pool;
  SYMBOL _innermost_loop_var_symb;
public:
  SYMBOL_TREE(BOOL is_floating_point, MEM_POOL *pool){
    _is_floating_point= is_floating_point;
    _symbol_node = NULL; _pool = pool; };

  void Initialize_Innermost_Loop_Var_Symbol(WN* wn);
  BOOL Integer_Ref_Needs_Reg(WN* wn);
  void Enter(SYMBOL *symbol, BOOL is_store, INT weight) { 
    if (!_symbol_node) {
      _symbol_node = CXX_NEW(SYMBOL_TREE_NODE(*symbol,is_store,weight),_pool);
    } else {
      _symbol_node->Enter(symbol,_pool,is_store,weight); 
    }
  };
  void Enter_Scalar_Refs(WN *wn, INT *num_scalar_refs,
			 WN2INT* se_needed=NULL, ARRAY_REF *ar=NULL);
  void Enter_Scalar_Refs(WN *wn, ARRAY_REF *ar, SX_INFO *pi,
  			BOOL *can_be_inner, INT num_loops, INT outer,
			INT *num_scalar_refs);
  INT Num_Fp_Unstored() const { 
    if (_symbol_node) return _symbol_node->Num_Fp_Unstored(); 
    return(0);
  }
  INT Num_Int_Unstored() const { 
    if (_symbol_node) return _symbol_node->Num_Int_Unstored(); 
    return(0);
  }
};
    


#endif  // MODEL_DELCARE


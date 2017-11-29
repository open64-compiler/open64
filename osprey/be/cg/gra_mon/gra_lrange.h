/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

//  $Revision: 1.16 $
//  $Date: 05/12/05 08:59:10-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lrange.h $ 
// Description: GRA Live Ranges & Coloring Lists
  
  
#ifndef GRA_LRANGE_INCLUDED
#define GRA_LRANGE_INCLUDED
#ifndef GRA_LRANGE_RCS_ID  
#define GRA_LRANGE_RCS_ID
#ifdef _KEEP_RCS_ID
static char *gra_lrange_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_lrange.h $ $Revision: 1.16 $";
#endif
#endif

#include <limits.h>
#include "defs.h"
#include "mempool.h"
#include "errors.h"
#include "tn.h"
#include "tn_map.h"
#include "bb.h"
#include "bb_set.h"
#include "register.h"

#include "gra_bb.h"
#include "gra_pref.h"
#include "gra_bb_list.h"

class INTERFERE_DEREF;
typedef INTERFERE_DEREF *INTERFERE;
class LRANGE_SET_SUBUNIVERSE;
class LRANGE_SUBUNIVERSE;
class GRA_REGION;
#ifdef KEY
class LRANGE_BOUNDARY_BB;
#endif

//      There are three distinct types of LRANGEs:
//
//          complement      -- A global live range from the part of the
//                             flow graph which has not been previously
//                             allocated with instruction level
//                             interference.  We'll allocate these with BB
//                             level interference, allowing splitting and
//                             spilling.
//
//          local           -- Represents the local scheduler's request
//                             for one register in a block.  There will be
//                             one for each unit of local girth in each
//                             block.
//
//          region          -- Represents a live range inside a region
//                             that has been allocated before as an
//                             existence proof using instruction level
//                             interference.  We'll allocate these with
//                             fine grained interference not allowing
//                             splitting or spilling.
typedef enum {
  LRANGE_TYPE_COMPLEMENT = 0,
  LRANGE_TYPE_REGION     = 1,
  LRANGE_TYPE_LOCAL      = 2
} LRANGE_TYPE;

enum LR_FLAG {
  LRANGE_FLAGS_listed    = 0x1,   	// On the coloring list
//              Used to freeze the _Neighbors_Left counts of LRANGEs after
//              they are listed so the counts will be useful during splitting
//              at which point, they give the number of colored neighbors of
//              the coloring candidate.
  LRANGE_FLAGS_allocated = 0x2,   	// Register has been allocated, so it is
					// valid to call the Reg() function
  LRANGE_FLAGS_has_wired_register = 0x4,// Must have a wired reg; we must
					// allocate that reg to it
  LRANGE_FLAGS_region_invariant = 0x8,  // Not (re)defined in region
//              We treat REGION live ranges that are invariant in their
//              regions specially by preallocating a register to before
//              simplification, thus reducing the complexity of that algorithm
//              while losing nothing.  This function tests <lrange> to
//              ditermine whether it has received this special treatment.
  LRANGE_FLAGS_spans_a_call = 0x10, 	// Live across a call (so cannot be
					// allocated to a caller-saved register)
  LRANGE_FLAGS_avoid_ra  = 0x20,  	// Might be the operand of a call, so
					// cannot be allocated to ra
  LRANGE_FLAGS_must_allocate = 0x40, 	// Give it a maximum priority.
//              It isn't clear that this makes sense except for
//              locals.  But assuming that the trace off is between having GRA
//              spill and LRA spill a global, I think we'll always prefer GRA
//              to do the spilling since it may be able to service many blocks
//              with a single spill/restore pair, whereas LRA will have to
//              spill/restore for each block.  So it seems useful to set this
//              property for enough locals in each block to prevent LRA from
//              spilling.
  LRANGE_FLAGS_spilled   = 0x80,  	// keep track of if it has been spilled
  LRANGE_FLAGS_split_listed = 0x100,    // Already entered on the list of
                                   	// LRANGEs with split spill or restores
					// (so it won't be added a second time)
  LRANGE_FLAGS_spans_infreq_call = 0x200, // Live across infrequently 
					  // executed call; will use a caller-
					  // saved reg and spill it at the
					  // call site if it is profitable to
					  // do so
  LRANGE_FLAGS_spans_rot_reg_clob = 0x400, // Live across rotating reg clobber
					   // relevant to its class (cannot be 
					   // assigned rotating reg)
  LRANGE_FLAGS_spans_a_setjmp = 0x800,  // Live across a setjmp
  LRANGE_FLAGS_tn_is_save_reg = 0x1000, // its TN marked TN_is_save_reg
  LRANGE_FLAGS_cannot_split =   0x2000, // meaning evident
  LRANGE_FLAGS_no_appearance =  0x4000, // should not be assigned register
#ifdef TARG_X8664
  LRANGE_FLAGS_spans_savexmms = 0x8000,   // spans the savexmms pseudo-op
  LRANGE_FLAGS_spans_x87_OP =   0x10000,  // spans x87 OP
  LRANGE_FLAGS_spans_mmx_OP =   0x20000,  // spans MMX OP
#elif defined(TARG_SL) //minor_reg_alloc
/* add a flag to indicate if the liverange spans different regions, here region means 
  * different rid in basic block. For minor thread, bb in parallel region has different rid
  * with bb in non pallel region. 
  */ 
  LRANGE_FLAGS_spans_multiregions = 0x8000, 
#endif
};

// These represent a value that can be given a register by GRA.  They
// are actually the only things that GRA does allocate to registers.
class LRANGE {
friend class LRANGE_MGR;
friend class LRANGE_GLUE_REF_GBB_ITER;
friend class LRANGE_CLIST;
friend class LRANGE_CLIST_ITER;
friend class LRANGE_LOCAL_LIST_ITER;
friend class LRANGE_SPLIT_LIST_ITER;
friend class LRANGE_LIVE_GBB_ITER;
friend class LRANGE_BB_LOCAL_LIST_ITER;
friend class LRANGE_LUNIT_ITER;
private:
  INT32               id;		// for implementing LRANGE_SETs
  INT32               neighbors_left;
  INT32               priority_queue_index; // its index in the priority queue;
					// used by coloring to implement fast 
					// removal from its ready/not-ready 
					// queues
  LRANGE*             clist_next;       // Internal coloring list link
  float               priority;		// store what Calculate_Priority compute
  INT32               mark;             // Implements the One_Set
  GRA_PREF*           pref;
  float               pref_priority;    // Priority of allocating the same
                                        // register to this live range as to
					// other members of its preference class
  ISA_REGISTER_CLASS  rc:8;
  mREGISTER           reg;		// the register allocated
  mREGISTER           orig_reg;         // Original register before coloring
                                        //   if there was one (eg for regions)
  LRANGE_TYPE         type:8;
  LR_FLAG             flags:24;
  union {
    struct lrange_complement_specific {
      TN*       tn;             // Corresponding to the LRANGE
      INTERFERE neighbors;      // Inteference graph neighbors
      LUNIT*    first_lunit;    // Head of _LUNIT list
      BB_SET*   live_bb_set;	// to maintain the set of BBs in the range;
//                Used to support iteration over the BBs in the live range 
//		  and test for interference between to complement LRANGEs.  
//		  So long as the associated BBs are accurate, these things will 
//		  also be accurate.
      GTN_SET*  global_pref_set;
//		  Maintain set of global TNs that this TN is preferenced to.
//		  We have verified that there is no real conflict between the
//		  TNs on the list and the TN represented by this LRANGE.
      TN*       original_tn;	// original GTN (before any spilling) from which
				// this lrange was created
      LRANGE*   next_split_list_lrange;
#ifdef KEY
      BB_SET*   internal_bb_set; // to maintain the set of internal BBs in the
				 // range; it is same as live_bb_set but without
				 // the boundary bbs
      LRANGE_BOUNDARY_BB*	boundary_bbs;	// List of boundary bbs.
#endif
    } c;
    struct lrange_region_specific {
      TN*       tn;             // Corresponding to the LRANGE
      INTERFERE neighbors;      // Inteference graph neighbors
      GRA_REGION* region;	// the region that owns it
      GRA_BB_LIST* complement_bbs;      // with glue references
    } r;
    struct lrange_local_specific {
      GRA_BB *gbb;		// block to which it is local
      LRANGE *next_bb_local_lrange;	// internally linked BB_Local_List
    } l;
  }                     u;
public:
  LRANGE(void) {}
  ~LRANGE(void) {}

  // access functions
  TN* Original_TN(void) 	{ DevAssert(type == LRANGE_TYPE_COMPLEMENT,
				   ("_Original_TN of a non-complement LRANGE"));
				  return u.c.original_tn; }
  TN* Tn(void) 			{ DevAssert(!(type == LRANGE_TYPE_LOCAL),
					("No TN for local LRANGEs."));
			  	  return u.c.tn; }
  INTERFERE Neighbors(void)	{ return u.c.neighbors; }
  INT32 Neighbors_Left(void) 	{ return neighbors_left; }
  INT32 Neighbors_Left_Increment(void) { return ++neighbors_left; }
  INT32 Neighbors_Left_Decrement(void) { return --neighbors_left; }
  float Priority(void)		{ return priority; }
  void Priority_Set(float p) 	{ priority = p; }
  ISA_REGISTER_CLASS Rc(void)	{ return rc; }
  REGISTER Reg(void) {DevAssert(Allocated() || Has_Wired_Register(),
		("LRANGE::Register called for unallocated non-wired LRANGE"));
		      return reg; }
  LRANGE_TYPE Type(void)	{ return type; }
  INT32 Id(void)		{ return id; }
  void Id_Set(INT32 i)		{ id = i; }
  BB_SET* Live_BB_Set(void)	{ return u.c.live_bb_set; }
  void Clear_Live_BBs(void)	{ BB_SET_ClearD(u.c.live_bb_set); }
  GTN_SET* Global_Pref_Set(void){ return u.c.global_pref_set; }
  void Clear_Global_Pref_Set(void) { BB_SET_ClearD(u.c.global_pref_set); }
  void First_Lunit_Reset(void)	{ u.c.first_lunit = NULL; }
  GRA_PREF *Pref(void)		{ return pref; }
  GRA_BB *Gbb(void)		{ return u.l.gbb; }
  GRA_REGION *Region(void)	{ return u.r.region; }
  float Pref_Priority(void)	{ return pref_priority; }
  REGISTER Orig_Reg(void)	{ return orig_reg; }
  INT32 Priority_Queue_Index(void) { return priority_queue_index; }
  void Priority_Queue_Index_Set(INT32 index) { priority_queue_index = index; }
#ifdef KEY
  BB_SET* Internal_BB_Set(void)	{ return u.c.internal_bb_set; }
  void Clear_Internal_BBs(void)	{ BB_SET_ClearD(u.c.internal_bb_set); }

  LRANGE_BOUNDARY_BB *Boundary_BBs(void)	{ return u.c.boundary_bbs; }
  void *Set_Boundary_BBs(LRANGE_BOUNDARY_BB *x)	{ u.c.boundary_bbs = x; }
  LRANGE_BOUNDARY_BB* Get_Boundary_Bb(BB *target);
  LRANGE_BOUNDARY_BB* Remove_Boundary_Bb(BB *target);
#endif

  // flags
  BOOL Listed(void)		{ return flags & LRANGE_FLAGS_listed; }
  void Listed_Set(void)		{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_listed);}
  BOOL Allocated(void)		{ return flags & LRANGE_FLAGS_allocated; }
  void Allocated_Reset(void)    { flags = (LR_FLAG)(flags&~LRANGE_FLAGS_allocated); }

  BOOL Spans_A_Call(void)	{ return flags & LRANGE_FLAGS_spans_a_call; }
  void Spans_A_Call_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_spans_a_call); }
  void Spans_A_Call_Reset(void)	{ flags = (LR_FLAG)(flags&~LRANGE_FLAGS_spans_a_call); }
  BOOL Spans_Infreq_Call(void)	{ return flags & LRANGE_FLAGS_spans_infreq_call; }
  void Spans_Infreq_Call_Set(void) { flags = (LR_FLAG)(flags|LRANGE_FLAGS_spans_infreq_call); }
  void Spans_Infreq_Call_Reset(void){ flags = (LR_FLAG)(flags&~LRANGE_FLAGS_spans_infreq_call);}
  BOOL Split_Listed(void)	{ return flags & LRANGE_FLAGS_split_listed; }
  void Split_Listed_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_split_listed); }
  BOOL Spans_Rot_Reg_Clob(void)	{ return flags & LRANGE_FLAGS_spans_rot_reg_clob; }
  void Spans_Rot_Reg_Clob_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_spans_rot_reg_clob); }
  void Spans_Rot_Reg_Clob_Reset(void)	{ flags = (LR_FLAG)(flags&~LRANGE_FLAGS_spans_rot_reg_clob); }
  BOOL Spans_A_Setjmp(void)	{ return flags & LRANGE_FLAGS_spans_a_setjmp; }
  void Spans_A_Setjmp_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_spans_a_setjmp); }
  void Spans_A_Setjmp_Reset(void) { flags = (LR_FLAG)(flags&~LRANGE_FLAGS_spans_a_setjmp); }
  BOOL Avoid_RA(void)		{ return flags & LRANGE_FLAGS_avoid_ra; }
  void Avoid_RA_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_avoid_ra); }
  BOOL Spilled(void)		{ return flags & LRANGE_FLAGS_spilled; }
  void Spilled_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_spilled); }
  BOOL Must_Allocate(void)	{ return flags & LRANGE_FLAGS_must_allocate; }
  void Must_Allocate_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_must_allocate); }
  BOOL Has_Wired_Register(void)	{ return flags & LRANGE_FLAGS_has_wired_register; }
  BOOL Region_Invariant(void)	{ return flags & LRANGE_FLAGS_region_invariant; }
  BOOL Tn_Is_Save_Reg(void)	{ return flags & LRANGE_FLAGS_tn_is_save_reg; }
  void Tn_Is_Save_Reg_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_tn_is_save_reg); }
  BOOL Cannot_Split(void)	{ return flags & LRANGE_FLAGS_cannot_split; }
  void Cannot_Split_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_cannot_split); }
  BOOL No_Appearance(void)	{ return flags & LRANGE_FLAGS_no_appearance; }
  void No_Appearance_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_no_appearance); }
#ifdef TARG_X8664
  BOOL Spans_Savexmms(void)	{ return flags & LRANGE_FLAGS_spans_savexmms; }
  void Spans_Savexmms_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_spans_savexmms); }
  BOOL Spans_x87_OP(void)	{ return flags & LRANGE_FLAGS_spans_x87_OP; }
  void Spans_x87_OP_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_spans_x87_OP); }
  void Spans_x87_OP_Reset(void)	{ flags = (LR_FLAG)(flags&~LRANGE_FLAGS_spans_x87_OP); }
  BOOL Spans_mmx_OP(void)	{ return flags & LRANGE_FLAGS_spans_mmx_OP; }
  void Spans_mmx_OP_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_spans_mmx_OP); }
  void Spans_mmx_OP_Reset(void)	{ flags = (LR_FLAG)(flags&~LRANGE_FLAGS_spans_mmx_OP); }
#endif


#ifdef TARG_SL //minor_reg_alloc
  BOOL Spans_Multiregions(void)	{ return flags & LRANGE_FLAGS_spans_multiregions; }
  void Spans_Multiregions_Set(void)	{ flags = (LR_FLAG)(flags|LRANGE_FLAGS_spans_multiregions); }
#endif 

  void Wire_Register(REGISTER r){ flags = (LR_FLAG)(flags|LRANGE_FLAGS_has_wired_register);
				  reg = r; }
//              Give <lrange> a wired register <reg>.
  void Preallocated_Region_Invariant(REGISTER r) {
				  flags = (LR_FLAG)(flags|LRANGE_FLAGS_region_invariant);
				  flags = (LR_FLAG)(flags|LRANGE_FLAGS_allocated);
				  reg = r; }
//              Preallocate <reg> to <lrange>, throughout <region>.  Set its
//              _Preallocated_Region_Invariant and _Allocated flags and its
//              Register field.  This is a LOW LEVEL function meant to be
//              CALLED BY GRA_REGION which is primarily responsible for the
//              allocation.

  // other inlined member functions
  BOOL Has_Preference(void)	{ return pref != NULL; }
  void Neighbors_Left_Initialize(void) { neighbors_left = Neighbor_Count(); }
    // Neithbors_Left_Initialize is used during both coloring and 
    // simplification.  During simplification it is used to implement the
    // Chaitin/Briggs algorithm.  During coloring, it is
    // used when splitting in order to determine if new Briggs
    // points are being created.
  LRANGE *BB_Local_List_Push(LRANGE *new_elt) // "this" is last head of list
				{ new_elt->u.l.next_bb_local_lrange = this;
				  return new_elt; }
  LRANGE *Split_List_Push(LRANGE *new_elt) // "this" is last head of list
				{ new_elt->u.c.next_split_list_lrange = this;
				  return new_elt; }
//              Add <new_elt> to the LRANGE_Split_List list beginning with
//              <head> and return the new head.

  // non-inlined member functions
  INT32 Candidate_Reg_Count(void);
  void Add_Live_BB(GRA_BB *gbb);
  void Remove_Live_BB(GRA_BB *gbb);
  BOOL Contains_BB(GRA_BB *gbb);
  void Add_Global_Pref(TN *tn);
  void Remove_Global_Pref(TN *tn);
  BOOL Check_Global_Pref(TN *tn);
  void Initialize_Region_Inteference(GRA_REGION* region);
  void Add_LUNIT( LUNIT* lunit );
  void Add_Lunit(  LUNIT* lunit );
  REGISTER_SET Allowed_Registers(GRA_REGION* region);
#ifdef KEY
  REGISTER_SET Reclaimable_Registers(GRA_REGION* region);
#endif
  BOOL Interferes( LRANGE* lr1 );
  void Region_Interference( LRANGE* lrange1,
			    GRA_REGION* region );
  void Remove_Neighbor(LRANGE* neighbor, GRA_REGION* region );
  void Allocate_Register( REGISTER reg, BOOL reclaim = FALSE );
  INT32 Neighbor_Count(void);
  void Calculate_Priority(void);
  BOOL Find_LUNIT_For_GBB( const GRA_BB* gbb, LUNIT** lunitp );
  void Preference_Copy(LRANGE* lrange1, GRA_BB* gbb );
  void Recompute_Preference(void);
  char* Format( char* buff );
#ifdef KEY
  void Add_Internal_BB(GRA_BB *gbb);
  void Remove_Internal_BB(GRA_BB *gbb);
  BOOL Contains_Internal_BB(GRA_BB *gbb);
  void Add_Boundary_BB(GRA_BB *gbb);
  void Boundary_BBs_Push(LRANGE_BOUNDARY_BB *x);
  void Update_Boundary_BBs(void);
#endif
};

#ifdef TARG_IA64
//Inserted by ORC for Experiments.
struct BUFFERED_LRANGE {
    LRANGE *lrange;
    INT abi_property;
    ISA_REGISTER_CLASS reg_class;
    INT lunits_number;
    float density;
    BUFFERED_LRANGE *next,*prev;
};
#endif

// manages the allocation and usage of all LRANGE nodes
class LRANGE_MGR {
  INT32    one_set_counter;
  LRANGE*  interference_creation_lrange;
  TN_MAP   tn_map;

public:
  LRANGE_MGR(void) {}
  ~LRANGE_MGR(void) {}

  // inlined member functions
  void Clear_One_Set(void)	{ ++one_set_counter; }
  BOOL One_Set_MemberP(const LRANGE *lrange) 
				{ return lrange->mark == one_set_counter; }
  void One_Set_Union1(LRANGE *lrange) { lrange->mark = one_set_counter; }
  void One_Set_Difference1(LRANGE *lrange) { lrange->mark = one_set_counter-1; }
//              These functions support a simple single set of LRANGEs
//              with a very cheap implementation of a limited number of
//              useful operations.  Notice there is exactly one of these
//              sets (hence the name) in existence at a time and each
//              LRANGE is either in or out.

  void Add_GBB_With_Glue_Reference(LRANGE *lrange, GRA_BB *gbb)
    { lrange->u.r.complement_bbs = GRA_BB_LIST_Push(gbb, 
					lrange->u.r.complement_bbs, GRA_pool);}
//              <lrange> is a REGION LRANGE and <gbb> is a block outside the
//              region that contains a preferencing copy into or out of
//              <lrange>.  <gbb> is recorded on the list of such blocks
//              associated with <lrange>.

  LRANGE* Get( const TN* tn ) 
    { return TN_is_register(tn) ? (LRANGE*) TN_MAP_Get(tn_map,tn) : NULL; }
//              Return the LRANGE corresponding to <tn> if there is one or
//              NULL if there isn't.

  // non-inlined member functions
  void Initialize(void);
  void Finalize(void);
  LRANGE* Create( LRANGE_TYPE type, ISA_REGISTER_CLASS rc, size_t size );
  LRANGE* Create_Complement( TN* tn );
  LRANGE* Create_Local( GRA_BB* gbb, ISA_REGISTER_CLASS cl );
  LRANGE* Create_Region( TN* tn, GRA_REGION* region );
  LRANGE* Create_Duplicate( LRANGE* lrange );
  void Begin_Complement_Interference(LRANGE *lrange);
  void Complement_Interference( LRANGE* neighbor );
  void End_Complement_Interference( void );
};

// Used to iterate over the the GRA_BB associated with a region
// LRANGE by calls to LRANGE_Add_GBB_With_Glue_Reference.
// Supports _Done, _Curretn, _Step functions and:
class LRANGE_GLUE_REF_GBB_ITER {
  GRA_BB_LIST *rest;
public:
  LRANGE_GLUE_REF_GBB_ITER(void) {}
  ~LRANGE_GLUE_REF_GBB_ITER(void) {}

  void Init(LRANGE *lrange)	{ rest = lrange->u.r.complement_bbs; }
  BOOL Done(void)		{ return rest == NULL; }
  GRA_BB *Current(void)		{ return GRA_BB_LIST_first(rest); }
  void Step(void)		{ rest = GRA_BB_LIST_rest(rest); }
};

//      LRANGE_CLIST -- Coloring lists
//
//          This is the type of a coloring list used to keep the LRANGEs
//          in each region in order for coloring.  It also must support
//          splitting which entails being able to move a lrange to a point
//          later in the list.  These are just headed internally linked
//          lists.  Supports:
//
//              void LRANGE_CLIST_Append( LRANGE_CLIST* clist0,
//                                        LRANGE_CLIST* clist1 )
//                  Make both <clist1> and <clist2> be the list
//                  c00,..,c0n,c10,...,c1m.  Only one of these two
//                  LRANGE_CLISTs can continue to be valid after this
//                  operation.  Choose one and discard the other.
class LRANGE_CLIST {
friend class LRANGE_CLIST_ITER;
  LRANGE *first;
  LRANGE *last;
public:
  LRANGE_CLIST(void) {}
  ~LRANGE_CLIST(void) {}

  void Initialize(void)		{ first = last = NULL; }
//                  Make <clist> empty.
  void Push(LRANGE *lrange)	{ lrange->clist_next = first;
				  first = lrange;
				  if (last == NULL) last = lrange; }
//                  Add <lrange> to the front of <clist>.

  void Append(LRANGE_CLIST *clist1);
  void Print_Clist(void);
};

// Looping through coloring lists
class LRANGE_CLIST_ITER {
  LRANGE_CLIST *clist;
  LRANGE *prev;
  LRANGE sentinal;
public:
  LRANGE_CLIST_ITER(void) {}
  ~LRANGE_CLIST_ITER(void) {}

  void Init(LRANGE_CLIST *cl) 	{ clist = cl;
				  prev = &sentinal;
				  sentinal.clist_next = cl->first; }
  BOOL Done(void)		{ return prev->clist_next == NULL; }
  LRANGE *Current(void)		{ return prev->clist_next; }
  void Step(void)		{ prev = prev->clist_next; }
  void Init_Following(LRANGE_CLIST_ITER *iter1) { *this = *iter1;
				  if (!Done()) Step();	}
//                      Used to set up an iteration starting with the next
//                      LRANGE after the _Current element of <iter>.  After
//                      this:
//                          _Step(iter);
//                          _Current(new_iter) == _Current(iter)
//                      This is used during spliting.  We need to search the
//                      coloring list forward looking for the proper insertion
//                      point for the deferred part of the split LRANGE.

  void Replace_Current(LRANGE *lrange);
  void Splice(LRANGE *lrange);
#ifdef KEY
  void Push(LRANGE *lrange);
//                      Insert <lrange> before the current element of <iter>.
#endif
};

// looping over an internally linked BB_local_List
class LRANGE_BB_LOCAL_LIST_ITER {
  LRANGE *current;
public:
  LRANGE_BB_LOCAL_LIST_ITER(void) {}
  ~LRANGE_BB_LOCAL_LIST_ITER(void) {}

  void Init(LRANGE *head)	{ current = head; }
  BOOL Done(void)		{ return current == NULL; }
  LRANGE *Current(void)		{ return current; }
  void Step(void)		{ current = current->u.l.next_bb_local_lrange; }
};

// Use to iterate over the elements of the internally linked
// LRANGE_Split_List.  Supports standard iterator functions and:
class LRANGE_SPLIT_LIST_ITER {
  LRANGE *current;
public:
  LRANGE_SPLIT_LIST_ITER(void) {}
  ~LRANGE_SPLIT_LIST_ITER(void) {}

  void Init(LRANGE *head)	{ current = head; }
  BOOL Done(void)		{ return current == NULL; }
  LRANGE *Current(void)		{ return current; }
  void Step(void)		{ current =current->u.c.next_split_list_lrange;}
};

// looping over the LUNITs for a complement live range
class LRANGE_LUNIT_ITER {
  LUNIT *current;
  LUNIT *next;
public:
  LRANGE_LUNIT_ITER(void) {}
  ~LRANGE_LUNIT_ITER(void) {}

  void Init(LRANGE *lrange) {
    Is_True(lrange->Type() == LRANGE_TYPE_COMPLEMENT,
            ("Iterating over the LUNITs of a non-COMPLEMENT type"));
    current = lrange->u.c.first_lunit;
    if (current != NULL)
      next = current->lrange_list_next;
  }
  BOOL Done(void)               { return current == NULL; }
  LUNIT* Current(void)          { return current; }
  void Step(void)               { current = next;
                                  if (next != NULL)
                                    next = next->lrange_list_next; }
};

// looping over the GRA_BBs in a complement live range
class LRANGE_LIVE_GBB_ITER {
  BB_SET *set;
  BB *current;
public:
  LRANGE_LIVE_GBB_ITER(void) {}
  ~LRANGE_LIVE_GBB_ITER(void) {}

  void Init(LRANGE *lrange) {
    Is_True(lrange->Type() == LRANGE_TYPE_COMPLEMENT,
            ("Iterating over the live_gbbs of a non-complement LRANGE"));
    set = lrange->u.c.live_bb_set;
    current = BB_SET_Choose(set);
  }
  BOOL Done(void)               { return current == BB_SET_CHOOSE_FAILURE; }
  GRA_BB *Current(void);
  void Step(void)               { current = BB_SET_Choose_Next(set, current); }
};

#ifdef KEY
class LRANGE_BOUNDARY_BB {
  LRANGE_BOUNDARY_BB*	next;
  LRANGE*		lrange;
  GRA_BB*		gbb;
  mUINT16		start_index;
  mUINT16		end_index;
    // start_index and end_index give the OP index in the BB where the lrange
    // starts and ends, respectively.  The indices can be <, >, or = relative
    // to each other, depending on the live-in and live-out properties of the
    // lrange.  Index 0 means no such OP.
    //
    // There are 4 cases:
    //   live-in	(live-in only)
    //		end_index>0
    //   live-out	(live-out only)
    //		start_index>0
    //   contained	(live range fully contained in BB)
    //		start_index>0, end_index>0
    //   empty		(not live anywhere in BB)
    //		start_index=0, end_index=0
    // Because the BB is a boundary BB, the live range cannot be pass-thru
    // (live-in, live-out, continuous) or disjoint (live-in, live-out, not
    // continuous).  If the live range has gaps in the BB, we combine the live
    // range segments and model it as either live-in, live-out, or contained.
public:
  LRANGE_BOUNDARY_BB(void)	{ start_index = end_index = 0; }
  ~LRANGE_BOUNDARY_BB(void)	{}

  LRANGE *Lrange(void)		{ return lrange; }
  void Lrange_Set(LRANGE *lr)	{ lrange = lr; }
  GRA_BB *Gbb(void)		{ return gbb; }

  void Init(GRA_BB *gbb, LRANGE *lrange);
  LRANGE_BOUNDARY_BB* Next()	{ return next; }
  void Next_Set(LRANGE_BOUNDARY_BB *x)	{ next = x; }
  BOOL Interfere(LRANGE_BOUNDARY_BB *boundary_bb1);
  BOOL Interfere(BOOL other_live_in, BOOL other_live_out,
		 mUINT16 other_start_index, mUINT16 other_end_index);
  BOOL Interfere(GRA_BB *gbb, ISA_REGISTER_CLASS rc, REGISTER reg);
  mUINT16 Start_Index(void)	{ return start_index; }
  mUINT16 End_Index(void)	{ return end_index; }

  BOOL Is_Live_In(void);
  BOOL Is_Live_Out(void);
};
#endif

// these functions are used by the generated lrange_set package
extern INT32 LRANGE_INT( LRANGE* lr );
extern LRANGE* INT_LRANGE( INT32 i );
extern LRANGE* INT_LRANGE_Sub(INT i, LRANGE_SET_SUBUNIVERSE* subuniverse);
inline INT32
LRANGE_Universe_ID_S( LRANGE* lrange, LRANGE_SET_SUBUNIVERSE* sub ) {
	  return lrange->Id(); }

extern LRANGE_MGR lrange_mgr;

#ifdef KEY
extern REGISTER_SET Global_Preferenced_Regs(LRANGE* lrange, GRA_BB* gbb);
#endif
#endif

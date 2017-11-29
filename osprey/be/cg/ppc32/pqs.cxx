/*

  Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
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


/*================================================================

This file implements the predicate query system

Key assumptions and requirements:

1) the PQS data structures are built once, and maintained as needed. 

2) Any predicate TN which is defined more than once is subject to the following two 
restrictions:
  A) All definitions are in the same basic block.
  B) There is no use of the TN as a qualifying predicate in between the first definition
     and the last definition. This restriction allows the PQS to not have to keep around instance
     numbers for each use of a predicate register, and simplifies things considerably. 


================================================================*/


#ifndef PQSTEST
#include "defs.h"
#include "pqs_defs.h"
#include "pqs_cg.h"
#include "pqs_cg_ia64.h"
#include "mempool.h"
#include "mempool_allocator.h"
#include "tracing.h"

#else
#include "pqstest.h"
#include "pqs_defs.h"
#include "pqsstubs.h"

#endif

#include "pqs.h"

#ifdef PQS_USE_MEMPOOLS
// Mempool variable
MEM_POOL PQS_mem_pool;
static BOOL pqs_mempool_init=FALSE;

// Memory initialization
void PQS_Init_Memory(void)
{
  if (pqs_mempool_init) return;
  pqs_mempool_init = TRUE;
  MEM_POOL_Initialize(&PQS_mem_pool,"PQS_mem_pool",FALSE);
  MEM_POOL_Push(&PQS_mem_pool);
}
#endif


// Values used internally for implementation of setting inquiry routines
enum truth_val {
   TRUTH_UNKNOWN=0,
   TRUTH_MAY_SET_TRUE=1,
   TRUTH_MAY_SET_FALSE=2,
   TRUTH_ALWAYS_SET_TRUE=4,
   TRUTH_ALWAYS_SET_FALSE=8,
   TRUTH_NEVER_SET_TRUE=16,
   TRUTH_NEVER_SET_FALSE=32,
   TRUTH_QUAL_PRED_TRUE=64
};

//================================================================

template<> void PQS_TN_SET::Print(FILE *f,BOOL newline) 
{
  PQS_TN_SET_TYPE::iterator p;

  fprintf(f,"{ ");
  p = _set.begin();
  while (p != _set.end()) {
    fprintf(f,"%d ",TN_number(*p));
    ++p;
  }
  fprintf(f,"}");
  if (newline) fprintf(f,"\n");
}

template<> void PQS_TNI_SET::Print(FILE *f, BOOL newline) 
{
  PQS_TNI_SET::set_iterator_type p;
  
  fprintf(f,"{ ");
  p = _set.begin();
  while (p != _set.end()) {
    fprintf(f,"<%d,TN%d> ",PQS_TNI_IDX(*p),TN_number(PQS_TNI_TN(*p)));
    ++p;
  }
  fprintf(f,"}");
  if (newline) fprintf(f,"\n");
}

/*****************************************************************

static inline BOOL
PQS_ITYPE_sets_unconditionally(PQS_ITYPE itype) 

Returns true if the instruction type always sets its P reg outputs.

****************************************************************/

static inline BOOL
PQS_ITYPE_sets_unconditionally(PQS_ITYPE itype) 
{
   return (itype == PQS_ITYPE_UNC || itype == PQS_ITYPE_DIVSQRT);
}


/*================================================================
  Check a TN set for all valid queriable entries
  returns TRUE if any of the elements is marked no_query.
  ==================================================================
*/
static BOOL check_for_unqueriable(const PQS_TN_SET &t)
{
   BOOL result = FALSE;
   PQS_TN_SET::set_iterator_type p;
   
   for (p = t.begin(); !result && p != t.end(); p++) {
      result |= PQS_TN_no_query(*p);
   }
   return (result);
}


/*================================================================

Infrastructure routines, not inplemented in
the header file since they are only called here.

================================================================
*/

inline PQS_NODE_IDX
PQS_MANAGER::New_pqs_idx(PQS_ITYPE itype, PQS_OP inst) {
   PQS_NODE new_node(itype,inst);
   _data.push_back(new_node);
   return (_data.size() - 1);
}


// Set up the contents of a PQS_NODE
void
PQS_NODE::Init()
{
   _inst = NULL;
   _itype = PQS_ITYPE_INVALID;
   in_pred1 = in_pred2 = PQS_IDX_NONE;
   qual_pred = PQS_IDX_NONE;
   qual_tn = NULL;
   out_pred1 = out_pred2 = NULL;
   other_instruction = PQS_IDX_NONE;
   flags = 0;
   _marker1 = 0;
   _marker2 = 0;
}

// Adds a use of a TN/idx pair to a node.
void
PQS_NODE::add_use(PQS_TN tn, PQS_NODE_IDX idx) {
   if (tn == out_pred1) {
      use1.push_back(idx);
   } else if (tn == out_pred2) {
      use2.push_back(idx);
   } else {
      FmtAssert(0,("PQS_NODE::add_use: no uses of TN"));
   }
}


/*****************************************************************

inline BOOL
PQS_MANAGER::may_set_TRUE(PQS_NODE_IDX tni, PQS_TN tn)
PQS_MANAGER::may_set_FALSE(PQS_NODE_IDX tni, PQS_TN tn)
PQS_MANAGER::never_set_TRUE(PQS_NODE_IDX tni, PQS_TN tn)
PQS_MANAGER::never_set_FALSE(PQS_NODE_IDX tni, PQS_TN tn)
PQS_MANAGER::always_set_TRUE(PQS_NODE_IDX tni, PQS_TN tn)
PQS_MANAGER::always_set_FALSE(PQS_NODE_IDX tni, PQS_TN tn)

PQS_MANAGER::may_set_TRUE(INT32 truth)
PQS_MANAGER::may_set_FALSE(INT32 truth)
PQS_MANAGER::never_set_TRUE(INT32 truth)
PQS_MANAGER::never_set_FALSE(INT32 truth)
PQS_MANAGER::always_set_TRUE(INT32 truth)
PQS_MANAGER::always_set_FALSE(INT32 truth)
PQS_MANAGER::qual_always_true(INT32 truth)

Queries about what a predicate setting instruction might do to a TN
The "always" should be understood to mean "subject to a TRUE qualifying predicate."

****************************************************************/

BOOL
PQS_MANAGER::may_set_TRUE(PQS_NODE_IDX tni, PQS_TN tn)
{
   INT32 truth = get_truth_info(tni,tn);
   return (truth & TRUTH_MAY_SET_TRUE) != 0;
}

BOOL
PQS_MANAGER::may_set_FALSE(PQS_NODE_IDX tni, PQS_TN tn)
{
   INT32 truth = get_truth_info(tni,tn);
   return (truth & TRUTH_MAY_SET_FALSE) != 0;
}

BOOL
PQS_MANAGER::never_set_TRUE(PQS_NODE_IDX tni, PQS_TN tn)
{
   INT32 truth = get_truth_info(tni,tn);
   return (truth & TRUTH_NEVER_SET_TRUE) != 0;
}

BOOL
PQS_MANAGER::never_set_FALSE(PQS_NODE_IDX tni, PQS_TN tn)
{
   INT32 truth = get_truth_info(tni,tn);
   return (truth & TRUTH_NEVER_SET_FALSE) != 0;
}

BOOL
PQS_MANAGER::always_set_TRUE(PQS_NODE_IDX tni, PQS_TN tn)
{
   INT32 truth = get_truth_info(tni,tn);
   return (truth & TRUTH_ALWAYS_SET_TRUE) != 0;
}

BOOL
PQS_MANAGER::always_set_FALSE(PQS_NODE_IDX tni, PQS_TN tn)
{
   INT32 truth = get_truth_info(tni,tn);
   return (truth & TRUTH_ALWAYS_SET_FALSE) != 0;
}

BOOL
PQS_MANAGER::may_set_TRUE(INT32 truth)
{
   return (truth & TRUTH_MAY_SET_TRUE) != 0;
}

BOOL
PQS_MANAGER::may_set_FALSE(INT32 truth)
{
   return (truth & TRUTH_MAY_SET_FALSE) != 0;
}

BOOL
PQS_MANAGER::never_set_TRUE(INT32 truth)
{
   return (truth & TRUTH_NEVER_SET_TRUE) != 0;
}

BOOL
PQS_MANAGER::never_set_FALSE(INT32 truth)
{
   return (truth & TRUTH_NEVER_SET_FALSE) != 0;
}

BOOL
PQS_MANAGER::always_set_TRUE(INT32 truth)
{
   return (truth & TRUTH_ALWAYS_SET_TRUE) != 0;
}

BOOL
PQS_MANAGER::always_set_FALSE(INT32 truth)
{
   return (truth & TRUTH_ALWAYS_SET_FALSE) != 0;
}

BOOL
PQS_MANAGER::qual_always_true(INT32 truth)
{
   return (truth & TRUTH_QUAL_PRED_TRUE) != 0;
}

/****************************************************************/

#define SET_RESULT12(t1,t2,t3,t4,t5,t6) \
       if (one_or_two == 1) { \
	  if (always_false) { \
	     result |= (t1); \
	  } else if (always_true) {\
	     result |= (t2);\
	  } else {\
	     result |= (t3);\
	  }\
       } else {\
	  if (always_false) {\
	     result |= (t4);\
	  } else if (always_true) {\
	     result |= (t5);\
	  } else {\
	     result |= (t6);\
	  }\
       }

#define SET_RESULT(t1,t2,t3) \
	  if (always_false) { \
	     result |= (t1); \
	  } else if (always_true) {\
	     result |= (t2);\
	  } else {\
	     result |= (t3);\
	  }


/*****************************************************************

INT32
PQS_MANAGER::get_truth_info(PQS_NODE_IDX tni, PQS_TN tn)

Return a truth value indicating what the instruction may do. 
Tn must be set by the node tni.

****************************************************************/
INT32
PQS_MANAGER::get_truth_info(PQS_NODE_IDX tni, PQS_TN tn)
{
   INT32 result = TRUTH_UNKNOWN;
   BOOL always_true = PQS_NODE_condition_true(tni);
   BOOL always_false = PQS_NODE_condition_false(tni);
   PQS_ITYPE itype = PQS_NODE_get_itype(tni);
   INT32  one_or_two = PQS_NODE_get_1_2(tni,tn);
   BOOL qual_is_true = (PQS_NODE_get_qual_pred(tni) == PQS_IDX_TRUE);
   if (qual_is_true) result |= TRUTH_QUAL_PRED_TRUE;

   switch (itype) {
    case PQS_ITYPE_UNC:
       if (!qual_is_true) result |= TRUTH_MAY_SET_FALSE;
       // Fall through
    case PQS_ITYPE_NORM:
       SET_RESULT12(TRUTH_NEVER_SET_TRUE|TRUTH_MAY_SET_FALSE|TRUTH_ALWAYS_SET_FALSE,
		    TRUTH_NEVER_SET_FALSE|TRUTH_MAY_SET_TRUE|TRUTH_ALWAYS_SET_TRUE,
		    TRUTH_MAY_SET_TRUE|TRUTH_MAY_SET_FALSE,
		    TRUTH_NEVER_SET_FALSE|TRUTH_MAY_SET_TRUE|TRUTH_ALWAYS_SET_TRUE,
		    TRUTH_NEVER_SET_TRUE|TRUTH_MAY_SET_FALSE|TRUTH_ALWAYS_SET_FALSE,
		    TRUTH_MAY_SET_TRUE|TRUTH_MAY_SET_FALSE);
       break;

    case PQS_ITYPE_AND:
       result |= TRUTH_NEVER_SET_TRUE;
       SET_RESULT(TRUTH_MAY_SET_FALSE|TRUTH_ALWAYS_SET_FALSE,
		  TRUTH_NEVER_SET_FALSE,
		  TRUTH_MAY_SET_FALSE);
       break;

    case PQS_ITYPE_ANDCM:
       result |= TRUTH_NEVER_SET_TRUE;
       SET_RESULT(TRUTH_NEVER_SET_FALSE,
		  TRUTH_MAY_SET_FALSE|TRUTH_ALWAYS_SET_FALSE,
		  TRUTH_MAY_SET_FALSE);
       break;


    case PQS_ITYPE_OR:
       result |= TRUTH_NEVER_SET_FALSE;
       SET_RESULT(TRUTH_NEVER_SET_TRUE,
		  TRUTH_MAY_SET_TRUE|TRUTH_ALWAYS_SET_TRUE,
		  TRUTH_MAY_SET_TRUE);
       break;
       
    case PQS_ITYPE_ORCM:
       result |= TRUTH_NEVER_SET_FALSE;
       SET_RESULT(TRUTH_MAY_SET_TRUE|TRUTH_ALWAYS_SET_TRUE,
		  TRUTH_NEVER_SET_TRUE,
		  TRUTH_MAY_SET_TRUE);
       break;
       
    case PQS_ITYPE_ANDORCM:
       SET_RESULT12(TRUTH_NEVER_SET_TRUE|TRUTH_MAY_SET_FALSE|TRUTH_ALWAYS_SET_FALSE,
		  TRUTH_NEVER_SET_TRUE|TRUTH_NEVER_SET_FALSE,
		  TRUTH_NEVER_SET_TRUE|TRUTH_MAY_SET_FALSE,
		  TRUTH_NEVER_SET_FALSE|TRUTH_MAY_SET_TRUE|TRUTH_ALWAYS_SET_TRUE,
		  TRUTH_NEVER_SET_FALSE|TRUTH_NEVER_SET_TRUE,
		  TRUTH_NEVER_SET_FALSE|TRUTH_MAY_SET_TRUE);
       break;

    case PQS_ITYPE_ORANDCM:
       SET_RESULT12(TRUTH_NEVER_SET_FALSE|TRUTH_NEVER_SET_TRUE,
		  TRUTH_NEVER_SET_FALSE|TRUTH_MAY_SET_TRUE|TRUTH_ALWAYS_SET_TRUE,
		  TRUTH_NEVER_SET_FALSE|TRUTH_MAY_SET_TRUE,
		  TRUTH_NEVER_SET_TRUE|TRUTH_NEVER_SET_FALSE,
		  TRUTH_NEVER_SET_TRUE|TRUTH_MAY_SET_FALSE|TRUTH_ALWAYS_SET_FALSE,
		  TRUTH_NEVER_SET_TRUE|TRUTH_MAY_SET_FALSE);
       break;

    case PQS_ITYPE_DIVSQRT:
       result |= TRUTH_MAY_SET_FALSE | TRUTH_MAY_SET_TRUE;
       break;

    default:
       // Should never get here
       FmtAssert(0,("Can't test truth value of ITYPE %d\n",itype));
   }       

   // Fixup special handling of false
   if (result | TRUTH_MAY_SET_FALSE) result &= ~TRUTH_NEVER_SET_FALSE;
   return (result);
}



/*****************************************************************

void PQS_MANAGER::PQS_Mark_TN_Parents_TRUE(TN *tn) 
void PQS_MANAGER::PQS_Mark_TN_Parents_TRUE(PQS_NODE_IDX tni, TN *tn)

Walk up the PQS tree, marking all ancestors of tn. The second form is the
one used internally. This will only mark nodes which have the potential
for setting things TRUE. For example, the qualifying predicate of an 
AND is never walked, since the node can never set something TRUE.

****************************************************************/

void 
PQS_MANAGER::PQS_Mark_TN_Parents_TRUE(PQS_NODE_IDX tni, PQS_TN tn)
{
   PQS_TN qual_tn;
   PQS_NODE_IDX qual_idx;
   PQS_NODE_IDX up_idx;

   /* Early exit */
   if (!PQS_Is_Real_Idx(tni)) return;

   /* Set the marker on the current index */
   PQS_NODE_Mark(tni,tn);
   
   /* Walk the qualifying predicate */
   if (may_set_TRUE(tni,tn)) {
      qual_idx = PQS_NODE_get_qual_pred(tni);
      qual_tn  = PQS_NODE_get_qual_tn(tni);
      PQS_Mark_TN_Parents_TRUE(qual_idx,qual_tn);
   }

   /* Walk the previous instances if necessary */
   up_idx = PQS_NODE_get_up_idx(tni,tn);
   PQS_Mark_TN_Parents_TRUE(up_idx,tn);
}

// Alternative entry
void 
PQS_MANAGER::PQS_Mark_TN_Parents_TRUE(PQS_TN tn)
{
   PQS_Mark_TN_Parents_TRUE(PQS_TN_get_last_definition(tn),tn);
}


/*****************************************************************

void
PQS_MANAGER::PQS_Add_Instruction (PQS_OP instruction)

Add an instruction to the current PQS_MANAGER. Instructions must be added in 
flow sequence. 

 To add an instruction to a PQS block

 1) Allocate the new PQS_NODE, and point it at the instruction. Point the 
    instruction at the PQS_NODE. 

 2) Get the TN_MAP for the qualifying predicate. Find the instruction setting this instance (the last)
    and set the qualifying PQS_NODE.

 3) For each of the TN's defined,
    A) allocate the PQS_TN_MAP if necessary
    B) add the previous instance (if present) as the input to the PQS_NODE (when required)
    C) Point the PQS_NODE at the TN


****************************************************************/


PQS_NODE_IDX
PQS_MANAGER::PQS_Add_Instruction (PQS_OP inst)
{
   PQS_ITYPE itype;
   PQS_NODE_IDX pqsnode = PQS_IDX_NONE;
   PQS_NODE_IDX last_definition;
   PQS_TN qual;
   PQS_TN p1,p2;
   PQS_NODE_IDX p1_last,p2_last;
   PQS_NODE_FLAGS flags;
   
   // Check out the instruction to see which class it is. 
   itype = PQS_classify_instruction(inst,qual,p1,p2,flags);
   if (qual) PQS_TN_set_used_as_qual_pred(qual);


   if (itype == PQS_ITYPE_NOPREDICATES) {
      PQS_OP_set_pqs_idx(inst,PQS_IDX_NONE);
      return pqsnode;
   }
   
   // Point the node at the instruction and vice versa
   pqsnode = New_pqs_idx(itype,inst);
   PQS_NODE_set_flags(pqsnode,flags);
   PQS_OP_set_pqs_idx(inst,pqsnode);

   // Set the qualifying predicate information
   if (qual == PQS_TN_P0) {
      PQS_NODE_set_qual_pred(pqsnode,PQS_IDX_TRUE);
      // A Normal with a true predicate behaves like an UNC
      if (itype == PQS_ITYPE_NORM) {
	 itype = PQS_ITYPE_UNC;
	 PQS_NODE_set_itype(pqsnode,itype);
      }
   } else {
      last_definition = PQS_TN_get_last_definition(qual);
      PQS_NODE_set_qual_pred(pqsnode,last_definition);
   }
   PQS_NODE_set_qual_tn(pqsnode,qual);
   
   // Set up the predicate pointers
   if (p1 && (p1 != PQS_TN_P0)) {
      PQS_NODE_set_out_pred1(pqsnode,p1);
      p1_last = PQS_TN_get_last_definition(p1);
      PQS_TN_set_last_definition(p1,pqsnode);
      
      if (PQS_ITYPE_sets_unconditionally(itype)) {
	 // These two set unconditionally
	 PQS_NODE_set_in_pred1(pqsnode,PQS_IDX_NONE);
      } else {
	 PQS_NODE_set_in_pred1(pqsnode,p1_last);
	 PQS_NODE_add_use(p1_last,p1,pqsnode);
      }
      // Set the NO_QUERY flag if the predicate has already been used a s qualifying predicate
      if (PQS_TN_used_as_qual_pred(p1)) {
	 PQS_TN_set_no_query(p1);
      }
   }
   
   if (p2 && (p2 != PQS_TN_P0)) {
      PQS_NODE_set_out_pred2(pqsnode,p2);
      p2_last = PQS_TN_get_last_definition(p2);
      PQS_TN_set_last_definition(p2,pqsnode);
      
      if (PQS_ITYPE_sets_unconditionally(itype)) {
	 PQS_NODE_set_in_pred2(pqsnode,PQS_IDX_NONE);
      } else {
	 PQS_NODE_set_in_pred2(pqsnode,p2_last);
	 PQS_NODE_add_use(p2_last,p2,pqsnode);
      }
      // Set the NO_QUERY flag if the predicate has already been used a s qualifying predicate
      if (PQS_TN_used_as_qual_pred(p2)) {
	 PQS_TN_set_no_query(p2);
      }
   }
   
   return pqsnode;
}


//
// Utility routine, returns posibility of two TNs never being true together
//
PQS_TRUTH
PQS_MANAGER::never_true_together(PQS_TN t1, PQS_TN t2, PQS_NODE_IDX tni)
{

   PQS_TRUTH tval = PQS_TRUTH_UNKNOWN;

   // Trivial case
   if (t1 == t2) return PQS_TRUTH_NEVER;
   
   // Check to make sure these two are set by this tni
   if (!((t1 == PQS_NODE_get_out_pred1(tni) &&
	    t2 == PQS_NODE_get_out_pred2(tni)) ||
	   (t2 == PQS_NODE_get_out_pred1(tni) &&
	    t1 == PQS_NODE_get_out_pred2(tni)))) {
     printf("bad tni %d\n",tni);
   }

   /* Check for getting set under an unconditional FALSE */
   if ((PQS_NODE_get_qual_pred(tni) == PQS_IDX_TRUE) &&
       (always_set_FALSE(tni,t1) ||
	always_set_FALSE(tni,t2))) {
      return (PQS_TRUTH_ALWAYS);
   }

   switch (PQS_NODE_get_itype(tni)) {
    case PQS_ITYPE_UNC:
       tval = PQS_TRUTH_ALWAYS;
       break;
       
    case PQS_ITYPE_NORM:
    case PQS_ITYPE_ORANDCM:
    case PQS_ITYPE_ANDORCM:
       // Can't rule it out, since this may set them complementarily
       tval = PQS_TRUTH_POSSIBLE;
       break;

    case PQS_ITYPE_OR:
       if (PQS_NODE_condition_false(tni)) {
	  tval = PQS_TRUTH_POSSIBLE;
       } else {
	  tval = PQS_TRUTH_NEVER;
       }

    case PQS_ITYPE_ORCM:
       if (PQS_NODE_condition_true(tni)) {
	  tval = PQS_TRUTH_POSSIBLE;
       } else {
	  tval = PQS_TRUTH_NEVER;
       }
       
    case PQS_ITYPE_AND:
    case PQS_ITYPE_ANDCM:
       // May set both to zero
       tval = PQS_TRUTH_POSSIBLE;
       break;

    default:
       FmtAssert(0,("Can't test truth value of this ITYPE\n"));
       break;
   }
   return tval;
}



BOOL
PQS_MANAGER::PQS_is_disjoint_helper(PQS_NODE_IDX tni2, PQS_TN tn2)
{
   PQS_TN tnm;
   PQS_TRUTH is_disjoint;
   PQS_NODE_IDX up_idx, qual_idx;
   PQS_TN qual_tn;
   BOOL up_value;
   BOOL qual_value;
   BOOL val_1,val_2, need_upwalk;
   BOOL result;

   // Go until we get a match on an ancestor of tni2, or until tni2 is 
   // an invalid

   if (!PQS_Is_Real_Idx(tni2)) return FALSE; // If we get here, we have not been able to prove it
   
   if (Is_Marked(tni2)) {
      // We have a collision with an ancestor of tni1
      val_1 = TRUE;
      val_2 = TRUE;
      need_upwalk = FALSE;
      if (Is_Marked1(tni2)) {
	 tnm = PQS_NODE_get_out_pred1(tni2);
	 is_disjoint = never_true_together(tnm,tn2,tni2);
	 if (is_disjoint == PQS_TRUTH_ALWAYS) {
	    val_1 = TRUE;
	 } else if (is_disjoint == PQS_TRUTH_NEVER) {
	    val_1 = FALSE;
	 } else if (is_disjoint == PQS_TRUTH_POSSIBLE) {
	    need_upwalk = TRUE;
	 }
      }
      if (Is_Marked2(tni2)) {
	 tnm = PQS_NODE_get_out_pred2(tni2);
	 is_disjoint = never_true_together(tnm,tn2,tni2);
	 if (is_disjoint == PQS_TRUTH_ALWAYS) {
	    val_2 = TRUE;
	 } else if (is_disjoint == PQS_TRUTH_NEVER) {
	    val_2 = FALSE;
	 } else  if (is_disjoint == PQS_TRUTH_POSSIBLE) {
	    need_upwalk = TRUE;
	 }
      }
      if (need_upwalk) {
	 // Harder case; the two may be disjoint, but we can only determine this
	 // by walking upward. 
	 up_value = PQS_is_disjoint_helper(PQS_NODE_get_up_idx(tni2,tn2),tn2);
      } else {
	 up_value = TRUE;
      }
      result = (up_value && val_1 && val_2);

   } else {
      // This node is not marked, so we need to check that 
      // 1) The qualifying predicate is disjoint, and
      // 2) The previous instance is disjoint
      up_idx = PQS_NODE_get_up_idx(tni2,tn2);
      if (PQS_Is_Real_Idx(up_idx)) {
	 up_value = PQS_is_disjoint_helper(up_idx,tn2);
      } else {
	 up_value = TRUE; // No previous instance
      }
      if (may_set_TRUE(tni2,tn2)) {
	 qual_idx = PQS_NODE_get_qual_pred(tni2);
	 qual_tn = PQS_NODE_get_qual_tn(tni2);
	 qual_value = PQS_is_disjoint_helper(qual_idx,qual_tn);
      } else {
	 qual_value = TRUE;
      }
      
      result = (qual_value && up_value);
   }
   return (result);
}


/*****************************************************************

BOOL PQS_is_disjoint (TN * tn1, TN * tn2)
BOOL PQS_is_disjoint (PQS_NODE_IDX tni1, PQS_NODE_IDX tni2, TN *tn1, TN *tn2)

Return TRUE if it is never possible for tn1 and tn2 to be true at the same
time.  The first form is the main inqiry form, and uses the last set
instance of the TN as the inquiry point. The second form is used internally
to indicate the start point for the inquiry.


****************************************************************/

BOOL
PQS_MANAGER::PQS_is_disjoint (PQS_NODE_IDX tni1, PQS_NODE_IDX tni2, PQS_TN tn1, PQS_TN tn2)
{
  // A couple of quick exits
  if (tn1 == tn2) return (FALSE);

  BOOL r=PQS_is_disjoint_h(tni1,tni2,tn1,tn2);
  if (PQS_Tracing && (tni1 != PQS_IDX_TRUE || tni2 != PQS_IDX_TRUE)) {
    fprintf(TFile,"<PQS> Is_Disjoint(TN%d[%d],TN%d[%d]) = %d\n",TN_number(tn1),tni1,
	    TN_number(tn2),tni2,r);
  }
  
  return r;
}


BOOL
PQS_MANAGER::PQS_is_disjoint_h (PQS_NODE_IDX tni1, PQS_NODE_IDX tni2, PQS_TN tn1, PQS_TN tn2)
{
   PQS_TRUTH same_tnis;

   // Degenerate cases
   if (tni1 == PQS_IDX_FALSE || tni2 == PQS_IDX_FALSE) {
      return (TRUE);
   } else if (tni1 == PQS_IDX_TRUE || tni2 == PQS_IDX_TRUE) {
      return (FALSE);
   }

   // Simple case, tni1 and tni2 are the same
   if (tni1 == tni2) {
      same_tnis = never_true_together(tn1,tn2,tni1);
      if (same_tnis == PQS_TRUTH_ALWAYS) {
	 return TRUE;
      } else if (same_tnis == PQS_TRUTH_NEVER) {
	 return FALSE;
      }
   }

   // Either we missed the simple case or the tni's are not the same.

   // Mark all ancestors of tn1
   Update_Marker();
   PQS_Mark_TN_Parents_TRUE(tni1,tn1);

   return PQS_is_disjoint_helper(tni2,tn2);

}


// ================ Public published entries ================

BOOL
PQS_MANAGER::PQS_is_disjoint(PQS_TN tn1, PQS_TN tn2)
{
   if (PQS_TN_no_query(tn1) || PQS_TN_no_query(tn2)) {
      return FALSE;
   }
   
   return PQS_is_disjoint(PQS_TN_get_last_definition(tn1),
			  PQS_TN_get_last_definition(tn2),
			  PQS_TN_get_tn_to_use(tn1),
			  PQS_TN_get_tn_to_use(tn2));
}

BOOL
PQS_MANAGER::PQS_is_disjoint(PQS_TN_SET &tns1, PQS_TN_SET &tns2)
{
   PQS_TN_SET_TYPE &tnset1 = tns1._set;
   PQS_TN_SET_TYPE &tnset2 = tns2._set;
   PQS_TN_SET_TYPE::iterator p,q;
   BOOL result = TRUE;

   if (check_for_unqueriable(tns1) || check_for_unqueriable(tns2)) {
      return (FALSE);
   }

   // Iterate overall pairs checking for disjoint-ness
   for (p = tnset1.begin(); result && (p != tnset1.end()); ++p) {
      for (q = tnset2.begin(); result && (q != tnset2.end()); ++q) {
	 result != PQS_is_disjoint(PQS_TN_get_last_definition(*p),
				   PQS_TN_get_last_definition(*q),
				   *p,*q);
      }
   }
   return (result);
}

/*****************************************************************

BOOL PQS_is_subset_of (PQS_TN tn1, PQS_TN tn2)
BOOL PQS_is_subset_of (PQS_TN tn1, PQS_TN_SET &tns2)
BOOL PQS_is_subset_of (PQS_TN_SET &tns1, PQS_TN_SET &tns2)

Return TRUE if it is always the case that if TN1 is true, then TN2 must also be true. 
For the set forms, return TRUE if any element in the first set is true, than at least one
element of the second set must be true.

****************************************************************/

BOOL
PQS_MANAGER::PQS_is_subset_of (PQS_NODE_IDX tni1, PQS_TN tn1, PQS_NODE_IDX tni2, PQS_TN tn2)
{
   BOOL qual_val, prev_val;
   INT32 truth;
   BOOL needs_upwalk;
   
   // Stopping cases
   if (tni2 == PQS_IDX_TRUE) return (TRUE);
   if (tni2 == PQS_IDX_FALSE) return (FALSE);
   if (tni1 == tni2) {
      if (tn1 == tn2) {
	 return (TRUE);
      } else {
	 return (FALSE);
      }
   } else if (!PQS_Is_Real_Idx(tni1)) {
      return (FALSE);
   }
   
   truth = get_truth_info(tni1,tn1);
   if (may_set_TRUE(truth)) {
      qual_val = PQS_is_subset_of(PQS_NODE_get_qual_pred(tni1),
				  PQS_NODE_get_qual_tn(tni1),tni2,tn2);
   } else {
      qual_val = TRUE;
   }

   // Don't needs to check the previous value if this node is guaranteed
   // to set something, which means it's an unconditional setter, or the
   // qual pred is TRUE and the node is an ALWAYS setter. 
   needs_upwalk = TRUE;
   if (PQS_ITYPE_sets_unconditionally(PQS_NODE_get_itype(tni1)) ||
       (qual_always_true(truth) &&
	(always_set_TRUE(truth) || always_set_FALSE(truth)))) {
      needs_upwalk = FALSE;
   }
   
   if (needs_upwalk) {
      prev_val = PQS_is_subset_of(PQS_NODE_get_up_idx(tni1,tn1),
				  tn1,tni2,tn2);
   } else {
      prev_val = TRUE;
   }

   return (prev_val && qual_val);
}


BOOL
PQS_MANAGER::PQS_is_subset_of (PQS_NODE_IDX tni1, PQS_TN tn1, PQS_TN_SET &tns2)
{
   BOOL qual_val, prev_val, needs_upwalk;
   INT32 truth;
   
   // Stopping cases
   if (tns2.Is_Subset(PQS_TN_P0)) return (TRUE);
   if (tns2.Is_Subset(tn1))       return (TRUE);
   if (!PQS_Is_Real_Idx(tni1))    return (FALSE);

   truth = get_truth_info(tni1,tn1);
   if (may_set_TRUE(truth)) {
      qual_val = PQS_is_subset_of(PQS_NODE_get_qual_pred(tni1),
				  PQS_NODE_get_qual_tn(tni1),tns2);
   } else {
      qual_val = TRUE;
   }
   
   // Don't needs to check the previous value if this node is guaranteed
   // to set something, which means it's an unconditional setter, or the
   // qual pred is TRUE and the node is an ALWAYS setter. 
   needs_upwalk = TRUE;
   if (PQS_ITYPE_sets_unconditionally(PQS_NODE_get_itype(tni1)) ||
       (qual_always_true(truth) &&
	(always_set_TRUE(truth) || always_set_FALSE(truth)))) {
      needs_upwalk = FALSE;
   }
   
   if (needs_upwalk) {
      prev_val = PQS_is_subset_of(PQS_NODE_get_up_idx(tni1,tn1),
				  tn1,tns2);
   } else {
      prev_val = TRUE;
   }

   return (prev_val && qual_val);
}

// ================ Public published entries ================

BOOL
PQS_MANAGER::PQS_is_subset_of(PQS_TN tn1, PQS_TN tn2)
{
  BOOL result;
  if (PQS_Tracing) {
    fprintf(TFile,"<PQS> Is_Subset_Of(TN%d,TN%d) = ",TN_number(tn1),TN_number(tn2));
  }
  
  if (PQS_TN_no_query(tn1) || PQS_TN_no_query(tn2)) {
    if (PQS_Tracing) {
      fprintf(TFile,"0\n");
    }
    return FALSE;
  }
  result = PQS_is_subset_of(PQS_TN_get_last_definition(tn1),
			    PQS_TN_get_tn_to_use(tn1),
			    PQS_TN_get_last_definition(tn2),
			    PQS_TN_get_tn_to_use(tn2));
  if (PQS_Tracing) {
    fprintf(TFile,"%d\n",result);
  }
  return result;
}

BOOL
PQS_MANAGER::PQS_is_subset_of(PQS_TN tn1, PQS_TN_SET &tns2)
{
   PQS_TN_SET tns_simp;
   BOOL result;

   if (PQS_Tracing) {
     fprintf(TFile,"<PQS> Is_Subset_Of(TN%d,",TN_number(tn1));
     tns2.Print(TFile,FALSE);
     fprintf(TFile,") = ");
   }

   if (PQS_TN_no_query(tn1) || check_for_unqueriable(tns2)) {
     if (PQS_Tracing) {
       fprintf(TFile,"0\n");
     }
     return (FALSE);
   }

   tns_simp = Simplify_TN_Set(tns2);
   result = PQS_is_subset_of(PQS_TN_get_last_definition(tn1),
			     PQS_TN_get_tn_to_use(tn1),
			     tns_simp);

   if (PQS_Tracing) {
     fprintf(TFile,"%d\n",result);
   }
   return result;
}


BOOL
PQS_MANAGER::PQS_is_subset_of(PQS_TN_SET &tns1, PQS_TN_SET &tns2)
{
   PQS_TN tn1;
   BOOL result = TRUE;
   PQS_TN_SET_TYPE &tnset = tns1._set;
   PQS_TN_SET_TYPE::iterator p;
   PQS_TN_SET tns_simp;

   if (PQS_Tracing) {
     fprintf(TFile,"<PQS> Is_Subset_Of(");
     tns1.Print(TFile,FALSE);
     fprintf(TFile,",");
     tns2.Print(TFile,FALSE);
     fprintf(TFile,") = ");
   }
     
   if (check_for_unqueriable(tns1) || check_for_unqueriable(tns2)) {
     if (PQS_Tracing) {
       fprintf(TFile,"0\n");
     }
     return (FALSE);
   }

   tns_simp = Simplify_TN_Set(tns2);

   p = tnset.begin();
   while (result && (p != tnset.end())) {
      tn1 = *p;
      result &= PQS_is_subset_of(PQS_TN_get_last_definition(tn1),
				 PQS_TN_get_tn_to_use(tn1),
				 tns_simp);
      ++p;
   }

   if (PQS_Tracing) {
     fprintf(TFile,"%d\n",result);
   }
   return result;
}


/*****************************************************************

Utility routine to simplify sets of TNs, etc. What it does it try 
to replace complementary pairs with the qualifying predicate. For example, 
If we had (pq) px,py = UNC..., and the set contains px and py, replace the two 
with pq. 

PQS_TN_SET
PQS_MANAGER::Simplify_TN_Set (PQS_TN_SET &tni)

 ****************************************************************
*/

PQS_TN_SET
PQS_MANAGER::Simplify_TN_Set (const PQS_TN_SET &tns)
{
   // First, generate a TNI set
   PQS_TN_SET::set_iterator_type p;
   PQS_TNI_SET::set_iterator_type q;
   PQS_TN tn;
   PQS_NODE_IDX tni;
   PQS_TNI_SET tnis;
   PQS_TN_SET result;
   
   for (p = tns.begin(); p != tns.end(); ++p) {
      tnis.Insert(PQS_TNI(PQS_TN_get_last_definition(*p),PQS_TN_get_tn_to_use(*p)));
   }
   
   Simplify_TNI_Set(tnis);

   // check for the TRUE predicate
   if (tnis.Is_Subset(PQS_TNI(PQS_IDX_TRUE,PQS_TN_P0))) {
      result.Insert(PQS_TN_P0);
   } else {

      // Convert back to a TN set, removing all elements which are not the last definition 
      for (q = tnis.begin(); q != tnis.end(); ++q) {
	 tn = PQS_TNI_TN(*q);
	 tni = PQS_TNI_IDX(*q);
	 if (tn) {
	    if (tni == PQS_TN_get_last_definition(tn)) {
	       result.Insert(tn); 
	    }
	 }
      }
   }
   
   // Add back the original set, just in case
   // result = result.Union(result,tns);
   return (result);
}


/*****************************************************************

Utility routine to simplify sets of TNs, etc. What it does it try 
to replace complementary pairs with the qualifying predicate. For example, 
If we had (pq) px,py = UNC..., and the set contains px and py, replace the two 
with pq. 

PQS_TNI_SET
PQS_MANAGER::Simplify_TNI_Set (PQS_TNI_SET &tni)

 ****************************************************************
*/

void
PQS_MANAGER::Simplify_TNI_Set (PQS_TNI_SET &result)
{
   PQS_TNI_SET::set_iterator_type p,p_end;
   BOOL any_combining;
   PQS_TNI_SET_TYPE &r_set = result._set;
   // Look for combinable pairs
   do {
      any_combining = FALSE;
      for (p = r_set.begin(); p != r_set.end(); ++p) {
	 any_combining = Simplify_In_Set(PQS_TNI_IDX(*p),PQS_TNI_TN(*p),result);
	 if (any_combining) break;
      }
   } while (any_combining);

   return;
}


// Helper routine for above

BOOL 
PQS_MANAGER::Simplify_In_Set(PQS_NODE_IDX tni, PQS_TN tn, PQS_TNI_SET &tnis)
{
   BOOL something_changed=FALSE;
   INT32 truth;

   // Simple things, if the TN is null or the tni is nothing, remove it from the set
   if (tn == NULL || tni == PQS_IDX_INVALID) {
      tnis.Clear(PQS_TNI(tni,tn));   
      return (TRUE);
   } else if (tni == PQS_IDX_NONE) {
      return (FALSE);
   } else if (tni == PQS_IDX_TRUE) {
     return (FALSE);
   }

   PQS_ITYPE itype=PQS_NODE_get_itype(tni);

   // If the TN can never be set TRUE, remove it
   truth = get_truth_info(tni,tn);
   if (never_set_TRUE(truth)) {
      // If it can never be true, remove it
      something_changed = TRUE;
      tnis.Clear(PQS_TNI(tni,tn));
   } else if (never_set_FALSE(truth) && !PQS_ITYPE_sets_unconditionally(itype)) {
      // Never sets a false, value so safe to add in the previous value
      PQS_TNI prev(PQS_NODE_get_up_idx(tni,tn),tn);
      if (!tnis.Is_Subset(prev)) {
	 something_changed = TRUE;
	 tnis.Insert(PQS_TNI(PQS_NODE_get_up_idx(tni,tn),tn));
      }
   } else if (always_set_TRUE(truth)) {
      // This always sets the predicate if the qualifying predicate is true,
      // so add the qual predicate to the set
      something_changed = TRUE;
      tnis.Insert(PQS_TNI(PQS_NODE_get_qual_pred(tni),PQS_NODE_get_qual_tn(tni)));
      tnis.Clear(PQS_TNI(tni,tn));
   }
   
   
   if (itype==PQS_ITYPE_UNC || itype==PQS_ITYPE_NORM) {
     PQS_TN other = PQS_NODE_get_other_tn(tni,tn);
     if (other && tnis.Is_Subset(PQS_TNI(tni,other))) {
	 // Both are in the set, remove them, insert the qualifying predicate, 
       something_changed = TRUE;
       tnis.Clear(PQS_TNI(tni,other));
       tnis.Clear(PQS_TNI(tni,tn));
       tnis.Insert(PQS_TNI(PQS_NODE_get_qual_pred(tni),PQS_NODE_get_qual_tn(tni)));
       // If both are in the set, then it's safe to add in both previous
       // values.
       if (itype == PQS_ITYPE_NORM) {
	 tnis.Insert(PQS_TNI(PQS_NODE_get_up_idx(tni,tn),tn));
	 tnis.Insert(PQS_TNI(PQS_NODE_get_up_idx(tni,other),other));
       }
     }
   }
   
   if (tnis.Is_Subset(PQS_TNI(PQS_IDX_TRUE,PQS_TN_P0))) {
      // Delete everything but this
      // Don't set something_changed, or we will get an infinite loop
      tnis.Clear();
      tnis.Insert(PQS_TNI(PQS_IDX_TRUE,PQS_TN_P0));
   }

   return something_changed;
}




//================================================================
//================================================================

// Dumping routines
static char * itype_name(PQS_ITYPE p)
{
   char *r;
   switch (p) {
    case PQS_ITYPE_INVALID: r = " PQS_ITYPE_INVALID"; break;
    case PQS_ITYPE_NOPREDICATES: r = " PQS_ITYPE_NOPREDICATES"; break;
    case PQS_ITYPE_NORM: r = " PQS_ITYPE_NORM"; break;
    case PQS_ITYPE_UNC: r = " PQS_ITYPE_UNC"; break;
    case PQS_ITYPE_OR: r = " PQS_ITYPE_OR"; break;
    case PQS_ITYPE_AND: r = " PQS_ITYPE_AND"; break;
    case PQS_ITYPE_ORANDCM: r = " PQS_ITYPE_ORANDCM"; break;
    case PQS_ITYPE_ORCM: r = " PQS_ITYPE_ORCM"; break;
    case PQS_ITYPE_ANDCM: r = " PQS_ITYPE_ANDCM"; break;
    case PQS_ITYPE_ANDORCM: r = " PQS_ITYPE_ANDORCM"; break;
    case PQS_ITYPE_DIVSQRT: r = " PQS_ITYPE_DIVSQRT"; break;
    case PQS_ITYPE_LAST: r = " PQS_ITYPE_LAST"; break;
   }
   return r;
}


void PQS_MANAGER::Print_all(FILE *f)
{
   PQS_NODE_IDX i;
   fprintf(f,"PQS data dump =========================================\n");
   for (i=1; i < _data.size(); ++i) {
      Print_idx(i,f);
   }
   fprintf(f,"=======================================================\n");
}


void PQS_MANAGER::Print_idx(PQS_NODE_IDX p, FILE *f)
{
   fprintf(f,"Node %5d: ",p);
   _data[p].Print(f);
}

static char * get_nq_flag(TN *tn)
{
  if (tn && PQS_TN_no_query(tn)) return ("*");
  return ("");
}

void PQS_NODE::Print(FILE *f)
{
   TN_NUM n1,n2,nq;
   n1 = out_pred1 ? TN_number(out_pred1) : -1;
   n2 = out_pred2 ? TN_number(out_pred2) : -1;
   nq = qual_tn   ? TN_number(qual_tn) : -1;
   fprintf(f,"  (%d[%d]%s) %d[%d]%s, %d[%d]%s = %s [0x%p] ",
	   nq,qual_pred,get_nq_flag(qual_tn),
	   n1,in_pred1,get_nq_flag(out_pred1),
	   n2,in_pred2,get_nq_flag(out_pred2),
	   itype_name(_itype),_inst);
   if (flags & PQS_FLAG_CONDITION_TRUE) fprintf(f,"TRUE");
   if (flags & PQS_FLAG_CONDITION_FALSE) fprintf(f,"FALSE");
   fprintf(f,"\n");
}



void dump_tn_set(PQS_TN_SET &tn)
{
   tn.Print();
}

void dump_tni_set(PQS_TNI_SET &tn)
{
   tn.Print();
}

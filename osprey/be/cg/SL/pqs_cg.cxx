/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#include "defs.h"

#include "pqs_cg.h"
#include "pqs_cg_ia64.h"
#include "pqs.h"
#include "tracing.h"

// Disable for MIPS because it has no predicate register
BOOL PQS_disabled=TRUE;

// Tracing variable
BOOL PQS_Tracing;

// Give the maps a place to live
TN_MAP PQS_tn_map;
OP_MAP PQS_op_map;

static void
get_top_info(TOP x, PQS_ITYPE &itype, PQS_RELOPTYPE &relop)
{
   relop = PQS_RELOPTYPE_OTHER;
   itype = PQS_ITYPE_INVALID;

   return;
}

/****************************************************************


  PQS_ITYPE 
  PQS_classify_instruction (PQS_OP inst, PQS_TN &qual, PQS_TN &p1, PQS_TN &p2, PQS_NODE_FLAGS &flags)
  
  Classify an instruction, returning (if present) the type of the instruction
  and setting qual, p1 and p2 to the qualifying predicate, the first predicate output
  and the second predicate output.

  flags is set if it is easily determined that a condition is always true or always false.

  WARNING: the result index numbers are hard-wired

****************************************************************/
PQS_ITYPE 
PQS_classify_instruction (PQS_OP inst, PQS_TN &qual, PQS_TN &p1, PQS_TN &p2, PQS_NODE_FLAGS &flags)
{
   PQS_ITYPE result;
   PQS_RELOPTYPE relop;
   TN *op1,*op2;
   TOP topcode;
   flags = 0;
   qual = NULL;
   p1 = NULL;
   p2 = NULL;
   
   topcode = OP_code(inst);
   get_top_info(topcode, result, relop);
   
   if (OP_has_predicate(inst)) {
      qual = OP_opnd(inst,OP_PREDICATE_OPND);
   }
   
   // Detect some very simple (but important) special cases
   if (relop == PQS_RELOPTYPE_EQ) {
      op1 = OP_opnd(inst,1);
      op2 = OP_opnd(inst,2);
      if (op1 == op2) {
	 flags = PQS_FLAG_CONDITION_TRUE;
      }
   }

   if (relop == PQS_RELOPTYPE_NE) {
      op1 = OP_opnd(inst,1);
      op2 = OP_opnd(inst,2);
      if (op1 == op2) {
	 flags = PQS_FLAG_CONDITION_FALSE;
      }
   }

   // Get the predicate results for the producer
   if (result != PQS_ITYPE_NOPREDICATES) {
      // Warning: this is hardwired 
      if (result == PQS_ITYPE_DIVSQRT) {
	 p1 = OP_result(inst,1);
      } else {
	 p1 = OP_result(inst,0);
	 p2 = OP_result(inst,1);
      }
   }

   return (result);
}

/*================================================================

Initialization and termination routines

See pqs_cg.h for descriptions.

*/


// This is the primary instance of the PQS used by the CG "global" interface
static PQS_MANAGER *pqsm=NULL;

// Allocate the map for a TN if it hasn't already been allocated
static PQS_TN_MAP_TYPE *
get_tn_map(const TN *t)
{
   PQS_TN_MAP_TYPE *m = (PQS_TN_MAP_TYPE *) TN_MAP_Get(pqsm->PQS_tn_map,t);
   if (m == NULL) {
      m = CXX_NEW(PQS_TN_MAP_TYPE,&PQS_mem_pool);
      TN_MAP_Set(pqsm->PQS_tn_map,(TN *) t,m);
   }
   return (m);
}


/* Part of the constructor to initialize TN0 and maps */
// Needs to be set up this way becuase we need valid PQS_MANAGER before we can use
// get_tn_map to set up the stuff for P0
void PQS_MANAGER::Init_TN_OP_Info(void)
{
   PQS_TN_MAP_TYPE *p0_map;
   PQS_TN_P0 = True_TN;
   PQS_tn_map = TN_MAP_Create();
   PQS_op_map = OP_MAP32_Create();
   p0_map = CXX_NEW(PQS_TN_MAP_TYPE,&PQS_mem_pool);
   TN_MAP_Set(PQS_tn_map,PQS_TN_P0,p0_map);
   p0_map->last_def = PQS_IDX_TRUE;
}


//
// Destructor, 
// Just delete the maps; the advantage of MEMPOOLS is that we don't need to worry 
// about freeing up the space.
// The default destructors for all the STL members take care of cleaning up the 
// memory (which is probably also in a mempool) for the various lists and vectors
// used in the PQS
//
PQS_MANAGER::~PQS_MANAGER()
{
  TN_MAP_Delete(PQS_tn_map);
  OP_MAP_Delete(PQS_op_map);
}


// Initialize the PQS for a set of BB's 
// 
void
PQSCG_init(BB * first_bb)
{
   BB *bb;
   OP *op;

   if (pqsm || PQS_disabled) return;
   // Initialize the pqs manager
   PQS_Init_Memory();
   MEM_POOL_Push(&PQS_mem_pool);
   pqsm = CXX_NEW(PQS_MANAGER,&MEM_phase_pool);
   PQS_tn_map = pqsm->PQS_tn_map;
   PQS_op_map = pqsm->PQS_op_map;

   // Add all the instructions
   FOR_ALL_BBLIST_ITEMS(first_bb,bb) {
      FOR_ALL_BB_OPs_FWD(bb,op) {
	 pqsm->PQS_Add_Instruction(op);
      }
   }

   // Trace
   if (Get_Trace(TP_PQS, 1)) {
     PQS_Tracing = TRUE;
   } else {
     PQS_Tracing = FALSE;
   }

   if (Get_Trace (TKIND_IR,TP_PQS)) {
     pqsm->Print_all(TFile);
   }

}


void
PQSCG_term(void)
{
   if (pqsm) {
      CXX_DELETE(pqsm,&MEM_phase_pool);
      MEM_POOL_Pop(&PQS_mem_pool);
      pqsm = NULL;
   }
}

void
PQSCG_reinit(BB * first_bb)
{
  PQSCG_term();
  PQSCG_init(first_bb);
}

BOOL 
PQSCG_pqs_valid(void)
{
  return pqsm != NULL;
}


void
PQSCG_add_instruction(PQS_OP op)
{
  pqsm->PQS_Add_Instruction(op);
}

//
// In practice, I don't think we ever generate a case for which this is 
// not going to return TRUE. If we eventually get clever with the OR and AND forms
// of the predicate generators, this will change.
//

BOOL 
PQSCG_sets_results_if_qual_true(PQS_OP op)
{
  PQS_ITYPE itype;
  PQS_TN qual, p1, p2;
  PQS_NODE_FLAGS flags;
  BOOL result=FALSE;
  
  itype = PQS_classify_instruction (op, qual, p1, p2, flags);
  switch (itype) {
   case PQS_ITYPE_NOPREDICATES:
   case PQS_ITYPE_NORM:
   case PQS_ITYPE_UNC:
   case PQS_ITYPE_DIVSQRT:
     result = TRUE;
     break;

   case PQS_ITYPE_OR:
   case PQS_ITYPE_ORANDCM:
   case PQS_ITYPE_ANDCM:
     result = (flags & PQS_FLAG_CONDITION_TRUE) != 0;
     break;

   case PQS_ITYPE_ORCM:
   case PQS_ITYPE_AND:
   case PQS_ITYPE_ANDORCM:
     result = (flags & PQS_FLAG_CONDITION_FALSE) != 0;
     break;
  }

  return (result);
}

void
PQSCG_copy_tn_map(PQS_TN tn_out, PQS_TN tn_in)
{
  PQS_TN_MAP_TYPE *m_in=get_tn_map(tn_in);
  PQS_TN_MAP_TYPE *m_out=get_tn_map(tn_out);
  
  // Copy the data
  *m_out = *m_in;
  // Set the tn_to_use field
  if (m_out->tn_to_use == NULL) {
    m_out->tn_to_use = tn_in;
  }
}


// Get the mempool used by PQS
MEM_POOL * PQSCG_get_mempool(void)
{
  return &PQS_mem_pool;
}


/*================================================================
  The interface routines which use the "current" PQS_MANAGER set up by PQS_Init()
  ================================================================
*/

#define MAKE_SURE_PQSM_INTIALIZED Is_True(pqsm,("Predicate query system not yet intialized"))

BOOL PQSCG_is_disjoint(PQS_TN tn1, PQS_TN tn2){
   if (!pqsm) return (FALSE);
   return pqsm->PQS_is_disjoint(tn1,tn2);
}

BOOL PQSCG_is_disjoint(PQS_TN_SET &tns1, PQS_TN_SET &tns2)
{
   if (!pqsm) return (FALSE);
   return pqsm->PQS_is_disjoint(tns1,tns2);
}

BOOL PQSCG_is_subset_of (PQS_TN tn1, PQS_TN tn2)
{
   if ((tn1 == tn2) || tn2 == True_TN) return (TRUE);
   if (!pqsm) return (FALSE);
   return pqsm->PQS_is_subset_of (tn1, tn2);
}

BOOL PQSCG_is_subset_of (PQS_TN tn1, PQS_TN_SET &tns2)
{
   if (tns2.Is_Subset(True_TN) || tns2.Is_Subset(tn1)) return (TRUE);
   if (!pqsm) return (FALSE);
   return pqsm->PQS_is_subset_of (tn1, tns2);
}

BOOL PQSCG_is_subset_of (PQS_TN_SET &tns1, PQS_TN_SET &tns2)
{
   if (tns2.Is_Subset(True_TN) || tns2.Is_Subset(tns1)) return (TRUE);
   if (!pqsm) return (FALSE);
   return pqsm->PQS_is_subset_of (tns1, tns2);
}


/*================================================================================
  ================================================================================
*/

// Routines to deal with the maps
PQS_NODE_IDX PQS_TN_get_last_definition(const TN *t)
{
   PQS_TN_MAP_TYPE *m = (PQS_TN_MAP_TYPE *) TN_MAP_Get(pqsm->PQS_tn_map,t);
   if (m) return m->last_def;
   return (PQS_IDX_NONE);
}
   
PQS_NODE_IDX PQS_TN_used_as_qual_pred(const TN *t)
{
   PQS_TN_MAP_TYPE *m =  (PQS_TN_MAP_TYPE *) TN_MAP_Get(pqsm->PQS_tn_map,t);
   if (m) return m->used_as_qual_pred;
   return (FALSE);
}

PQS_NODE_IDX PQS_TN_no_query(const TN *t)
{
   PQS_TN_MAP_TYPE *m =  (PQS_TN_MAP_TYPE *) TN_MAP_Get(pqsm->PQS_tn_map,t);
   if (m) return m->no_query;
   return (TRUE);
}



void PQS_TN_set_last_definition(const TN *t, PQS_NODE_IDX p)
{
   PQS_TN_MAP_TYPE *m=get_tn_map(t);
   m->last_def = p;
}

void PQS_TN_set_used_as_qual_pred(const TN *t)
{
   PQS_TN_MAP_TYPE *m=get_tn_map(t);
   m->used_as_qual_pred = TRUE;
}

void PQS_TN_set_no_query(const TN *t)
{
   PQS_TN_MAP_TYPE *m=get_tn_map(t);
   m->no_query = TRUE;
}


PQS_TN
PQS_TN_get_tn_to_use(const TN *t)
{
  PQS_TN_MAP_TYPE *m=get_tn_map(t);
  if (m->tn_to_use != NULL) {
    return m->tn_to_use;
  } else {
    return (PQS_TN) t;
  }
}

void
PQS_TN_set_tn_to_use(const TN *t, const TN *to_use)
{
  PQS_TN_MAP_TYPE *m=get_tn_map(t);
  m->tn_to_use = (PQS_TN) to_use;
}

void
PQS_OP_set_pqs_idx(OP *op, PQS_NODE_IDX p)
{
   OP_MAP32_Set(pqsm->PQS_op_map,op,p);
}


PQS_NODE_IDX
PQS_OP_get_pqs_idx(OP *op)
{
   return OP_MAP32_Get(PQS_op_map,op);
}



// Debugging routines

void dump_idx(PQS_NODE_IDX idx)
{
   pqsm->Print_idx(idx);
}

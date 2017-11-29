/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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


#include "defs.h"

#include "pqs_cg.h"
#include "pqs_cg_ia64.h"
#include "pqs.h"
#include "tracing.h"

// Disable the PQS (for debugging purposes)
BOOL PQS_disabled=FALSE;

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

   switch (x) {
    case TOP_cmp4_eq_or_andcm: 
    case TOP_cmp4_ge_or_andcm: 
    case TOP_cmp4_gt_or_andcm: 
    case TOP_cmp4_i_eq_or_andcm: 
    case TOP_cmp4_i_ne_or_andcm: 
    case TOP_cmp4_le_or_andcm: 
    case TOP_cmp4_lt_or_andcm: 
    case TOP_cmp4_ne_or_andcm: 
    case TOP_cmp4_z1_ge_or_andcm: 
    case TOP_cmp4_z1_gt_or_andcm: 
    case TOP_cmp4_z1_le_or_andcm: 
    case TOP_cmp4_z1_lt_or_andcm: 
    case TOP_cmp4_z2_ge_or_andcm: 
    case TOP_cmp4_z2_gt_or_andcm: 
    case TOP_cmp4_z2_le_or_andcm: 
    case TOP_cmp4_z2_lt_or_andcm: 
    case TOP_cmp_eq_or_andcm: 
    case TOP_cmp_ge_or_andcm: 
    case TOP_cmp_gt_or_andcm: 
    case TOP_cmp_i_eq_or_andcm: 
    case TOP_cmp_i_ne_or_andcm: 
    case TOP_cmp_le_or_andcm: 
    case TOP_cmp_lt_or_andcm: 
    case TOP_cmp_ne_or_andcm: 
    case TOP_cmp_z1_ge_or_andcm: 
    case TOP_cmp_z1_gt_or_andcm: 
    case TOP_cmp_z1_le_or_andcm: 
    case TOP_cmp_z1_lt_or_andcm: 
    case TOP_cmp_z2_ge_or_andcm: 
    case TOP_cmp_z2_gt_or_andcm: 
    case TOP_cmp_z2_le_or_andcm: 
    case TOP_cmp_z2_lt_or_andcm: 
    case TOP_tbit_nz_or_andcm: 
    case TOP_tbit_z_or_andcm: 
    case TOP_tnat_nz_or_andcm: 
    case TOP_tnat_z_or_andcm: 
       itype = PQS_ITYPE_ORANDCM;
       break;

    case TOP_cmp4_eq_and_orcm: 
    case TOP_cmp4_ge_and_orcm: 
    case TOP_cmp4_gt_and_orcm: 
    case TOP_cmp4_i_eq_and_orcm: 
    case TOP_cmp4_i_ne_and_orcm: 
    case TOP_cmp4_le_and_orcm: 
    case TOP_cmp4_lt_and_orcm: 
    case TOP_cmp4_ne_and_orcm: 
    case TOP_cmp4_z1_ge_and_orcm: 
    case TOP_cmp4_z1_gt_and_orcm: 
    case TOP_cmp4_z1_le_and_orcm: 
    case TOP_cmp4_z1_lt_and_orcm: 
    case TOP_cmp4_z2_ge_and_orcm: 
    case TOP_cmp4_z2_gt_and_orcm: 
    case TOP_cmp4_z2_le_and_orcm: 
    case TOP_cmp4_z2_lt_and_orcm: 
    case TOP_cmp_eq_and_orcm: 
    case TOP_cmp_ge_and_orcm: 
    case TOP_cmp_gt_and_orcm: 
    case TOP_cmp_i_eq_and_orcm: 
    case TOP_cmp_i_ne_and_orcm: 
    case TOP_cmp_le_and_orcm: 
    case TOP_cmp_lt_and_orcm: 
    case TOP_cmp_ne_and_orcm: 
    case TOP_cmp_z1_ge_and_orcm: 
    case TOP_cmp_z1_gt_and_orcm: 
    case TOP_cmp_z1_le_and_orcm: 
    case TOP_cmp_z1_lt_and_orcm: 
    case TOP_cmp_z2_ge_and_orcm: 
    case TOP_cmp_z2_gt_and_orcm: 
    case TOP_cmp_z2_le_and_orcm: 
    case TOP_cmp_z2_lt_and_orcm: 
       itype = PQS_ITYPE_ORANDCM;
       break;

    case TOP_cmp4_eq_andcm: 
    case TOP_cmp4_ge_andcm: 
    case TOP_cmp4_gt_andcm: 
    case TOP_cmp4_i_eq_andcm: 
    case TOP_cmp4_i_ne_andcm: 
    case TOP_cmp4_le_andcm: 
    case TOP_cmp4_lt_andcm: 
    case TOP_cmp4_ne_andcm: 
    case TOP_cmp4_z1_ge_andcm: 
    case TOP_cmp4_z1_gt_andcm: 
    case TOP_cmp4_z1_le_andcm: 
    case TOP_cmp4_z1_lt_andcm: 
    case TOP_cmp4_z2_ge_andcm: 
    case TOP_cmp4_z2_gt_andcm: 
    case TOP_cmp4_z2_le_andcm: 
    case TOP_cmp4_z2_lt_andcm: 
    case TOP_cmp_eq_andcm: 
    case TOP_cmp_ge_andcm: 
    case TOP_cmp_gt_andcm: 
    case TOP_cmp_i_eq_andcm: 
    case TOP_cmp_i_ne_andcm: 
    case TOP_cmp_le_andcm: 
    case TOP_cmp_lt_andcm: 
    case TOP_cmp_ne_andcm: 
    case TOP_cmp_z1_ge_andcm: 
    case TOP_cmp_z1_gt_andcm: 
    case TOP_cmp_z1_le_andcm: 
    case TOP_cmp_z1_lt_andcm: 
    case TOP_cmp_z2_ge_andcm: 
    case TOP_cmp_z2_gt_andcm: 
    case TOP_cmp_z2_le_andcm: 
    case TOP_cmp_z2_lt_andcm: 
       itype = PQS_ITYPE_ANDCM;
       break;

    case TOP_cmp4_eq_and: 
    case TOP_cmp4_ge_and: 
    case TOP_cmp4_gt_and: 
    case TOP_cmp4_i_eq_and: 
    case TOP_cmp4_i_ne_and: 
    case TOP_cmp4_le_and: 
    case TOP_cmp4_lt_and: 
    case TOP_cmp4_ne_and: 
    case TOP_cmp4_z1_ge_and: 
    case TOP_cmp4_z1_gt_and: 
    case TOP_cmp4_z1_le_and: 
    case TOP_cmp4_z1_lt_and: 
    case TOP_cmp4_z2_ge_and: 
    case TOP_cmp4_z2_gt_and: 
    case TOP_cmp4_z2_le_and: 
    case TOP_cmp4_z2_lt_and: 
    case TOP_cmp_eq_and: 
    case TOP_cmp_ge_and: 
    case TOP_cmp_gt_and: 
    case TOP_cmp_i_eq_and: 
    case TOP_cmp_i_ne_and: 
    case TOP_cmp_le_and: 
    case TOP_cmp_lt_and: 
    case TOP_cmp_ne_and: 
    case TOP_cmp_z1_ge_and: 
    case TOP_cmp_z1_gt_and: 
    case TOP_cmp_z1_le_and: 
    case TOP_cmp_z1_lt_and: 
    case TOP_cmp_z2_ge_and: 
    case TOP_cmp_z2_gt_and: 
    case TOP_cmp_z2_le_and: 
    case TOP_cmp_z2_lt_and: 
    case TOP_tbit_nz_and: 
    case TOP_tbit_z_and: 
    case TOP_tnat_nz_and: 
    case TOP_tnat_z_and: 
       itype = PQS_ITYPE_AND;
       break;

    case TOP_cmp4_eq_orcm: 
    case TOP_cmp4_ge_orcm: 
    case TOP_cmp4_gt_orcm: 
    case TOP_cmp4_i_eq_orcm: 
    case TOP_cmp4_i_ne_orcm: 
    case TOP_cmp4_le_orcm: 
    case TOP_cmp4_lt_orcm: 
    case TOP_cmp4_ne_orcm: 
    case TOP_cmp4_z1_ge_orcm: 
    case TOP_cmp4_z1_gt_orcm: 
    case TOP_cmp4_z1_le_orcm: 
    case TOP_cmp4_z1_lt_orcm: 
    case TOP_cmp4_z2_ge_orcm: 
    case TOP_cmp4_z2_gt_orcm: 
    case TOP_cmp4_z2_le_orcm: 
    case TOP_cmp4_z2_lt_orcm: 
    case TOP_cmp_eq_orcm: 
    case TOP_cmp_ge_orcm: 
    case TOP_cmp_gt_orcm: 
    case TOP_cmp_i_eq_orcm: 
    case TOP_cmp_i_ne_orcm: 
    case TOP_cmp_le_orcm: 
    case TOP_cmp_lt_orcm: 
    case TOP_cmp_ne_orcm: 
    case TOP_cmp_z1_ge_orcm: 
    case TOP_cmp_z1_gt_orcm: 
    case TOP_cmp_z1_le_orcm: 
    case TOP_cmp_z1_lt_orcm: 
    case TOP_cmp_z2_ge_orcm: 
    case TOP_cmp_z2_gt_orcm: 
    case TOP_cmp_z2_le_orcm: 
    case TOP_cmp_z2_lt_orcm: 
       itype = PQS_ITYPE_ORCM;
       break;


    case TOP_cmp4_eq_or: 
    case TOP_cmp4_ge_or: 
    case TOP_cmp4_gt_or: 
    case TOP_cmp4_i_eq_or: 
    case TOP_cmp4_i_ne_or: 
    case TOP_cmp4_le_or: 
    case TOP_cmp4_lt_or: 
    case TOP_cmp4_ne_or: 
    case TOP_cmp4_z1_ge_or: 
    case TOP_cmp4_z1_gt_or: 
    case TOP_cmp4_z1_le_or: 
    case TOP_cmp4_z1_lt_or: 
    case TOP_cmp4_z2_ge_or: 
    case TOP_cmp4_z2_gt_or: 
    case TOP_cmp4_z2_le_or: 
    case TOP_cmp4_z2_lt_or: 
    case TOP_cmp_eq_or: 
    case TOP_cmp_ge_or: 
    case TOP_cmp_gt_or: 
    case TOP_cmp_i_eq_or: 
    case TOP_cmp_i_ne_or: 
    case TOP_cmp_le_or: 
    case TOP_cmp_lt_or: 
    case TOP_cmp_ne_or: 
    case TOP_cmp_z1_ge_or: 
    case TOP_cmp_z1_gt_or: 
    case TOP_cmp_z1_le_or: 
    case TOP_cmp_z1_lt_or: 
    case TOP_cmp_z2_ge_or: 
    case TOP_cmp_z2_gt_or: 
    case TOP_cmp_z2_le_or: 
    case TOP_cmp_z2_lt_or: 
    case TOP_tbit_nz_or: 
    case TOP_tbit_z_or: 
    case TOP_tnat_nz_or: 
    case TOP_tnat_z_or: 
       itype = PQS_ITYPE_OR;
       break;

    case TOP_fcmp_ord_unc: 
    case TOP_fcmp_unord_unc: 
    case TOP_cmp4_eq_unc: 
    case TOP_cmp4_ge_unc: 
    case TOP_cmp4_geu_unc: 
    case TOP_cmp4_gt_unc: 
    case TOP_cmp4_gtu_unc: 
    case TOP_cmp4_i_eq_unc: 
    case TOP_cmp4_i_ge_unc: 
    case TOP_cmp4_i_geu_unc: 
    case TOP_cmp4_i_gt_unc: 
    case TOP_cmp4_i_gtu_unc: 
    case TOP_cmp4_i_le_unc: 
    case TOP_cmp4_i_leu_unc: 
    case TOP_cmp4_i_lt_unc: 
    case TOP_cmp4_i_ltu_unc: 
    case TOP_cmp4_i_ne_unc: 
    case TOP_cmp4_le_unc: 
    case TOP_cmp4_leu_unc: 
    case TOP_cmp4_lt_unc: 
    case TOP_cmp4_ltu_unc: 
    case TOP_cmp4_ne_unc: 
    case TOP_cmp_eq_unc: 
    case TOP_cmp_ge_unc: 
    case TOP_cmp_geu_unc: 
    case TOP_cmp_gt_unc: 
    case TOP_cmp_gtu_unc: 
    case TOP_cmp_i_eq_unc: 
    case TOP_cmp_i_ge_unc: 
    case TOP_cmp_i_geu_unc: 
    case TOP_cmp_i_gt_unc: 
    case TOP_cmp_i_gtu_unc: 
    case TOP_cmp_i_le_unc: 
    case TOP_cmp_i_leu_unc: 
    case TOP_cmp_i_lt_unc: 
    case TOP_cmp_i_ltu_unc: 
    case TOP_cmp_i_ne_unc: 
    case TOP_cmp_le_unc: 
    case TOP_cmp_leu_unc: 
    case TOP_cmp_lt_unc: 
    case TOP_cmp_ltu_unc: 
    case TOP_cmp_ne_unc: 
    case TOP_fclass_m_unc: 
    case TOP_fclass_nm_unc: 
    case TOP_fcmp_eq_unc: 
    case TOP_fcmp_ge_unc: 
    case TOP_fcmp_gt_unc: 
    case TOP_fcmp_le_unc: 
    case TOP_fcmp_lt_unc: 
    case TOP_fcmp_neq_unc: 
    case TOP_fcmp_nge_unc: 
    case TOP_fcmp_ngt_unc: 
    case TOP_fcmp_nle_unc: 
    case TOP_fcmp_nlt_unc: 
    case TOP_tbit_nz_unc: 
    case TOP_tbit_z_unc: 
    case TOP_tnat_nz_unc: 
    case TOP_tnat_z_unc: 
       itype = PQS_ITYPE_UNC;
       break;

    case TOP_fcmp_ord: 
    case TOP_fcmp_unord: 
    case TOP_cmp4_eq: 
    case TOP_cmp4_ge: 
    case TOP_cmp4_geu: 
    case TOP_cmp4_gt: 
    case TOP_cmp4_gtu: 
    case TOP_cmp4_i_eq: 
    case TOP_cmp4_i_ge: 
    case TOP_cmp4_i_geu: 
    case TOP_cmp4_i_gt: 
    case TOP_cmp4_i_gtu: 
    case TOP_cmp4_i_le: 
    case TOP_cmp4_i_leu: 
    case TOP_cmp4_i_lt: 
    case TOP_cmp4_i_ltu: 
    case TOP_cmp4_i_ne: 
    case TOP_cmp4_le: 
    case TOP_cmp4_leu: 
    case TOP_cmp4_lt: 
    case TOP_cmp4_ltu: 
    case TOP_cmp4_ne: 
    case TOP_cmp_eq: 
    case TOP_cmp_ge: 
    case TOP_cmp_geu: 
    case TOP_cmp_gt: 
    case TOP_cmp_gtu: 
    case TOP_cmp_i_eq: 
    case TOP_cmp_i_ge: 
    case TOP_cmp_i_geu: 
    case TOP_cmp_i_gt: 
    case TOP_cmp_i_gtu: 
    case TOP_cmp_i_le: 
    case TOP_cmp_i_leu: 
    case TOP_cmp_i_lt: 
    case TOP_cmp_i_ltu: 
    case TOP_cmp_i_ne: 
    case TOP_cmp_le: 
    case TOP_cmp_leu: 
    case TOP_cmp_lt: 
    case TOP_cmp_ltu: 
    case TOP_cmp_ne: 
    case TOP_fclass_m: 
    case TOP_fclass_nm: 
    case TOP_fcmp_eq: 
    case TOP_fcmp_ge: 
    case TOP_fcmp_gt: 
    case TOP_fcmp_le: 
    case TOP_fcmp_lt: 
    case TOP_fcmp_neq: 
    case TOP_fcmp_nge: 
    case TOP_fcmp_ngt: 
    case TOP_fcmp_nle: 
    case TOP_fcmp_nlt: 
    case TOP_tbit_nz: 
    case TOP_tbit_z: 
    case TOP_tnat_nz: 
    case TOP_tnat_z: 
       itype = PQS_ITYPE_NORM;
       break;

    case TOP_frcpa: 
    case TOP_frsqrta: 
       itype = PQS_ITYPE_DIVSQRT;
       break;

    default: 
       itype = PQS_ITYPE_NOPREDICATES;
       break;
   }

   
   switch (x) {
    case TOP_fcmp_neq: 
    case TOP_fcmp_neq_unc: 
    case TOP_cmp4_i_ne: 
    case TOP_cmp4_i_ne_and: 
    case TOP_cmp4_i_ne_and_orcm: 
    case TOP_cmp4_i_ne_andcm: 
    case TOP_cmp4_i_ne_or: 
    case TOP_cmp4_i_ne_or_andcm: 
    case TOP_cmp4_i_ne_orcm: 
    case TOP_cmp4_i_ne_unc: 
    case TOP_cmp4_ne: 
    case TOP_cmp4_ne_and: 
    case TOP_cmp4_ne_and_orcm: 
    case TOP_cmp4_ne_andcm: 
    case TOP_cmp4_ne_or: 
    case TOP_cmp4_ne_or_andcm: 
    case TOP_cmp4_ne_orcm: 
    case TOP_cmp4_ne_unc: 
    case TOP_cmp_i_ne: 
    case TOP_cmp_i_ne_and: 
    case TOP_cmp_i_ne_and_orcm: 
    case TOP_cmp_i_ne_andcm: 
    case TOP_cmp_i_ne_or: 
    case TOP_cmp_i_ne_or_andcm: 
    case TOP_cmp_i_ne_orcm: 
    case TOP_cmp_i_ne_unc: 
    case TOP_cmp_ne: 
    case TOP_cmp_ne_and: 
    case TOP_cmp_ne_and_orcm: 
    case TOP_cmp_ne_andcm: 
    case TOP_cmp_ne_or: 
    case TOP_cmp_ne_or_andcm: 
    case TOP_cmp_ne_orcm: 
    case TOP_cmp_ne_unc: 
       relop = PQS_RELOPTYPE_NE;
       break;

    case TOP_cmp4_eq: 
    case TOP_cmp4_eq_and: 
    case TOP_cmp4_eq_and_orcm: 
    case TOP_cmp4_eq_andcm: 
    case TOP_cmp4_eq_or: 
    case TOP_cmp4_eq_or_andcm: 
    case TOP_cmp4_eq_orcm: 
    case TOP_cmp4_eq_unc: 
    case TOP_cmp4_i_eq: 
    case TOP_cmp4_i_eq_and: 
    case TOP_cmp4_i_eq_and_orcm: 
    case TOP_cmp4_i_eq_andcm: 
    case TOP_cmp4_i_eq_or: 
    case TOP_cmp4_i_eq_or_andcm: 
    case TOP_cmp4_i_eq_orcm: 
    case TOP_cmp4_i_eq_unc: 
    case TOP_cmp_eq: 
    case TOP_cmp_eq_and: 
    case TOP_cmp_eq_and_orcm: 
    case TOP_cmp_eq_andcm: 
    case TOP_cmp_eq_or: 
    case TOP_cmp_eq_or_andcm: 
    case TOP_cmp_eq_orcm: 
    case TOP_cmp_eq_unc: 
    case TOP_cmp_i_eq: 
    case TOP_cmp_i_eq_and: 
    case TOP_cmp_i_eq_and_orcm: 
    case TOP_cmp_i_eq_andcm: 
    case TOP_cmp_i_eq_or: 
    case TOP_cmp_i_eq_or_andcm: 
    case TOP_cmp_i_eq_orcm: 
    case TOP_cmp_i_eq_unc: 
    case TOP_fcmp_eq: 
    case TOP_fcmp_eq_unc: 
       relop = PQS_RELOPTYPE_EQ;
       break;

   }
   
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

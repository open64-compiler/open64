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

//  Create Basic GRA Data Structures
/////////////////////////////////////
//
//  Description:
//
//      Traverse the flow grap building GRA's basic data structures:
//
//          1. The GRA_REGIONs that represent the separately preallocated
//             parts of the PU and the complement.
//
//          2. The GRA_BBs that encapsulate the BBs
//
//          3. The LRANGEs that represent register candidates
//
//          4. The LUNITs that represent a BB within a particular LRANGE
//
/////////////////////////////////////

//  $Revision: 1.13 $
//  $Date: 05/12/05 08:59:10-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_create.cxx $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_create.cxx $

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_create.cxx $ $Revision: 1.13 $";
#endif

#include "defs.h"
#include "mempool.h"
#include "cg.h"
#include "cgir.h"
#include "cgtarget.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "op.h"
#include "whirl2ops.h" 		// to get definition of TN_To_PREG(tn)
#include "dominate.h"
#include "findloops.h"
#include "cg_flags.h"
#include "cg_region.h"
#include "gra_bb.h"
#include "gra_lrange.h"
#include "gra_lunit.h"
#include "gra_region.h"
#include "gra_loop.h"
#include "gra_trace.h"
#include "gra_cflow.h"
#include "gra_pref.h"
#include "register.h"
#ifdef TARG_SL2 //para_region_mgr
#include "gra_para_region.h"
#endif 
#include "tracing.h"

#define FREE(ptr) MEM_POOL_FREE(Malloc_Mem_Pool,ptr)

INT GRA_non_preference_tn_id = -1;
static BOOL gbb_needs_rename;   // Some local renaming required for a GBB
                                // due to a cgprep failure.  DevWarn and
                                // rename for robustness.

#ifdef TARG_IA64
BOOL fat_self_recursive = FALSE;
extern BOOL gra_self_recursive;
extern char *Cur_PU_Name;
BOOL OP_maybe_unc_cmp(OP* op)
{
                  TOP op_code = OP_code(op);
                  switch (op_code) {
                  case  TOP_cmp_ne:
                  case  TOP_cmp_ge:
                  case  TOP_cmp_le:
                  case  TOP_cmp_lt:
                  case  TOP_cmp_ltu:
                  case  TOP_cmp_leu:
                  case  TOP_cmp_gt:
                  case  TOP_cmp_gtu:
                  case  TOP_cmp_geu:
                  case  TOP_cmp_i_ne:
                  case  TOP_cmp_i_ge:
                  case  TOP_cmp_i_le:
                  case  TOP_cmp_i_lt:
                  case  TOP_cmp_i_ltu:
                  case  TOP_cmp_i_leu:
                  case  TOP_cmp_i_gt:
                  case  TOP_cmp_i_gtu:
                  case  TOP_cmp_i_geu:
                  case  TOP_cmp4_ne:
                  case  TOP_cmp4_ge:
                  case  TOP_cmp4_le:
                  case  TOP_cmp4_lt:
                  case  TOP_cmp4_ltu:
                  case  TOP_cmp4_leu:
                  case  TOP_cmp4_gt:
                  case  TOP_cmp4_gtu:
                  case  TOP_cmp4_geu:
                  case  TOP_cmp4_i_ne:
                  case  TOP_cmp4_i_ge:
                  case  TOP_cmp4_i_le:
                  case  TOP_cmp4_i_gt:
                  case  TOP_cmp4_i_lt:
                  case  TOP_cmp4_i_ltu:
                  case  TOP_cmp4_i_leu:
                  case  TOP_cmp4_i_gtu:
                  case  TOP_cmp4_i_geu:

                        {  return TRUE;
                        }
                  default: return FALSE;
                  }

}
OP* Search_Pair_OP(OP *op,OP* def_op1,TN* res_tn,TN* predicated_tn)
{
    for (OP* find_op=BB_first_op(OP_bb(op));find_op;find_op=OP_next(find_op)){
        if (OP_cond_def(find_op)) {
           TN *pn = OP_opnd(find_op, OP_PREDICATE_OPND);
           DEF_KIND kind ;
           OP *def_op2 = TN_Reaching_Value_At_Op(pn,find_op,&kind,TRUE);
           if ((pn!=predicated_tn)&&(def_op2==def_op1)&&(OP_results(find_op)==1)&&(OP_result(find_op,0)==res_tn)) {
               return find_op;
           }
         }
    }
    return NULL;
}
OP* Check_Disjoint_Predicate_Guarded_Def (OP* xop)
{
   if (OP_cond_def(xop)||(OP_results(xop)==1)) {
       TOP opcode=OP_code(xop);
       TN *pn = OP_opnd(xop, OP_PREDICATE_OPND);
       if (pn==True_TN) return FALSE;
       DEF_KIND kind ;
       OP *def_op = TN_Reaching_Value_At_Op(pn,xop,&kind,TRUE);
       BOOL Yes=FALSE;
       if ((def_op)&&(OP_bb(def_op)==OP_bb(xop))) {
          if((OP_cmp_unc(def_op))||(OP_maybe_unc_cmp(def_op))) {
             OP* find_op=Search_Pair_OP(xop,def_op,OP_result(xop,0),pn);
             if (find_op) {
                 return find_op;
              }
           }
       }
    }
   return NULL;
}
class OP_OF_ONLY_DEF
{
   typedef struct OP_IDX {
       INT idx;
       OP_IDX *next;
   }OP_IDX_TYPE;
   OP_IDX_TYPE *op_idx_array;
   MEM_POOL *own_mem;
   public:
        INT *op_idx ;
        OP_OF_ONLY_DEF(MEM_POOL *pool) {
              own_mem=pool;
              op_idx_array = NULL;
        }
        OP_IDX_TYPE* Create_Array_Member (void) {
              OP_IDX_TYPE *new_op_idx_array ; 
              new_op_idx_array = TYPE_MEM_POOL_ALLOC(OP_IDX_TYPE,own_mem);
              new_op_idx_array->idx=0 ;
              new_op_idx_array->next =NULL;
              return new_op_idx_array;
         }
         void Add (OP* op) {
             OP_IDX_TYPE *head= op_idx_array;
             if (head == NULL) {
                 OP_IDX_TYPE *s_op_idx_array=Create_Array_Member();
                 s_op_idx_array->idx = OP_map_idx(op);
                 head=s_op_idx_array;
                 op_idx_array=s_op_idx_array;
             }else {
                while (head->next!=NULL) {
                   if (head->idx == OP_map_idx(op))
                      return;
                   head = head->next;
                }
                if (head->idx == OP_map_idx(op))
                   return;
                OP_IDX_TYPE *s_op_idx_array=Create_Array_Member();
                s_op_idx_array->idx = OP_map_idx(op);
                head->next = s_op_idx_array;
             }
         }
         BOOL FIND_OP(OP* find_op) {
              OP* op;
              OP_IDX_TYPE *head= op_idx_array;
              head= op_idx_array;
              while (head !=NULL) {
                 if (OP_map_idx(find_op)==head->idx){
                    return TRUE;
                 }else 
                    head = head->next;
              }
              return FALSE;
         }
         
         ~OP_OF_ONLY_DEF(void){
         }
         void Set_OPS_OF_ONLY_DEF(GRA_BB* gbb)
         {
            GRA_BB_OP_FORWARD_ITER iter;
            INT op_count;
            for (iter.Init(gbb),op_count=1;!iter.Done();iter.Step(),op_count++) 
            {

                OP* xop = iter.Current();
                if(OP_cond_def(xop)){
                   OP* another_op =Check_Disjoint_Predicate_Guarded_Def(xop);
                   if (another_op) {
                      Add(xop);
                      Add(another_op);
                   }
                } 
            }
            return;
         }

};
#endif // TARG_IA64

static void
Identify_Region_Boundries(void)
/////////////////////////////////////
//  flag blocks that will be used to control the splitting
//   algorithm with regions as with loops.
/////////////////////////////////////
{
  GRA_REGION_PREV_ALLOC_ITER iter;

  for (iter.Init(); ! iter.Done(); iter.Step()) {
    GRA_REGION *region = iter.Current();
    RID *rid = region->Rid();
    GRA_REGION_GBB_ITER gbb_iter;

    //
    // no need to deal with SWP regions in this manner.  they're
    // dealt with by the loop splitting rules.
    //
    if (RID_type(rid) == RID_TYPE_swp) {
      continue;
    }

    for (gbb_iter.Init(region); ! gbb_iter.Done(); gbb_iter.Step()) {
      GRA_BB* gbb = gbb_iter.Current();
      GRA_BB_FLOW_NEIGHBOR_ITER bb_iter;

      // flag entry "prolog" blocks for regions
      for (bb_iter.Preds_Init(gbb); ! bb_iter.Done(); bb_iter.Step()) {
	GRA_BB* pred = bb_iter.Current();
	if (BB_rid(pred->Bb()) == Current_Rid) {
	  pred->Region_Prolog_Set();
	}
      }

      // flag exit "epilog" blocks for regions
      for (bb_iter.Succs_Init(gbb); ! bb_iter.Done(); bb_iter.Step()) {
	GRA_BB* succ = bb_iter.Current();
	if (BB_rid(succ->Bb()) == Current_Rid) {
	  succ->Region_Epilog_Set();
	}
      }
    }
  }
}

/////////////////////////////////////
static void
Create_GRA_BB_Call_Spill_Block(BB *bb)
/////////////////////////////////////
//
// add block when control flow would not otherwise
// allow shrink wrapping around the call block.  if
// there is not a successor of the same frequency of
// the call block, then we need a spill block.
//
/////////////////////////////////////
{
  BB *succ = BB_Unique_Successor(bb);

  // 
  // no successor in situations such as call to fortran stop
  //
  if (succ == NULL || BB_freq(bb) == BB_freq(succ)) {
    return;
  }
  
  // 
  // we're going to be a bit paranoid here.  if the function
  // result (if any) was used, then it should have been used
  // in the successor block and that block should have had the
  // same frequency (since they have a unique predecessor/
  // successor relationship.  check for upwardly exposed uses
  // of the result registers just the same.
  //

  if (!BB_call(succ)) {
   /* Check bit vectors to deduce that the results of the previous call are
      used in the successor block.

      Note: this check is not going to work if the successor block also has a
      call and the output register set overlaps with the input register set.  
      The the successor block may use the same register - with different values -
      for input to the new call. In this case, it looks like there is a use of
      the TN, but that use may just be as an input argument for the next call.
   */

    for (TN *tn = GTN_SET_Choose(BB_live_use(succ));
         tn != GTN_SET_CHOOSE_FAILURE;
         tn = GTN_SET_Choose_Next(BB_live_use(succ), tn)) {
      REGISTER_SET func_val =
        REGISTER_CLASS_function_value(TN_register_class(tn));
      if (TN_is_register(tn) &&
          (TN_register(tn) != REGISTER_UNDEFINED) &&
          GTN_SET_MemberP(BB_live_in(succ), tn) &&
          REGISTER_SET_MemberP(func_val, TN_register(tn))) {
        return;
      }
    }

  } else if (OP_has_predicate(BB_last_op(bb)) &&
             OP_has_predicate(BB_last_op(succ)) &&
             !TN_is_const_reg(OP_opnd(BB_last_op(bb), OP_PREDICATE_OPND)) &&
             !TNs_Are_Equivalent(OP_opnd(BB_last_op(bb), OP_PREDICATE_OPND),
                                 OP_opnd(BB_last_op(succ), OP_PREDICATE_OPND))) {
   /* We really don't know what is going on here - so insert the block! */
    goto block_needed;
  } else {
   /* Scan the OPs in the block, looking for a use of the result registers
      for the previous call. */

    for (TN *tn = GTN_SET_Choose(BB_live_use(succ));
         tn != GTN_SET_CHOOSE_FAILURE;
         tn = GTN_SET_Choose_Next(BB_live_use(succ), tn)) {
      OP *op;
      REGISTER_SET func_val =
        REGISTER_CLASS_function_value(TN_register_class(tn));

      if (TN_is_register(tn) &&
          (TN_register(tn) != REGISTER_UNDEFINED) &&
          GTN_SET_MemberP(BB_live_in(succ), tn) &&
          REGISTER_SET_MemberP(func_val, TN_register(tn))) {

        FOR_ALL_BB_OPs(bb, op) {
          for (INT j = 0; j < OP_opnds(op); j++) {
            TN *cktn = OP_opnd(op, j);
            if (TN_is_register(cktn) &&
                TNs_Are_Equivalent(tn, cktn)) {
             /* There is a use of the result TN before it is redefined.
                Assume that ALL result TNs are consumed and no spill
                block is needed. */
              return;
            }
          }
          for (INT k = 0; k < OP_results(op); k++) {
            TN *cktn = OP_result(op, k);
            if (TN_is_register(cktn) &&
                TNs_Are_Equivalent(tn, cktn)) {
             /* There is no use of the result TN. Be safe and allocate
                a block to hold any spills that may be needed. */
              goto block_needed;
            }
          }
        }
      }
    }

  }

block_needed:

  //
  // okay, we need a block.
  //
  GRA_Add_Call_Spill_Block(bb, succ);
}

/////////////////////////////////////
static void
Create_Spill_Blocks(void)
/////////////////////////////////////
//
// add spill blocks where needed.
//
/////////////////////////////////////
{
  BB *bb;

  for ( bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ) {
    //
    // add spill block following call if needed to allow
    // shrink wrapping around the call.
    //
    if (BB_call(bb)) {
      Create_GRA_BB_Call_Spill_Block(bb);
    }
  }
}

/////////////////////////////////////
// Create GRA_LOOPs from CG's loop-finding code.
// Call Create_Spill_Blocks() to add spill blocks where needed.
// Visit all the BBs and create GRA_BBs, GRA_REGIONs (prev_alloc or complement).
// Set the needs_a_register GTN_SET in each GRA_BB.
// Allocate the LRANGE_SUBUNIVERSEs for each GRA_REGION.
// Call Identify_Region_Boundries() to identify region boundaries.
static void
Create_GRA_BBs_And_Regions(void)
{
  BB *bb;

  GRA_Init_Trace_Memory();
  //
  // find all loops, and set up maps such that the gbb's may map to
  // the GRA_LOOP structure representing the loop, if any, that contains
  // the bb.
  //
  if (GRA_loop_splitting) { // are we doing loop-directed live range splitting?
    Calculate_Dominators();
    gra_loop_mgr.Find_Loops();
  }
  GRA_Trace_Memory("After Find Loops");

  Create_Spill_Blocks();

  GRA_Init_Trace_Memory();
  for ( bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ) {
    GRA_REGION* region = gra_region_mgr.Get(BB_rid(bb));
    GRA_BB*     gbb    = gbb_mgr.Create(bb,region);
    region->Add_GBB(gbb);
    gra_loop_mgr.Set_GBB_Loop(gbb);

#ifdef TARG_SL //minor_reg_mgr
    RID * rid = BB_rid(bb);
    GRA_PARA_REGION* para_region = gra_para_region_mgr.Get(rid);
    if(rid && RID_TYPE_minor(rid)) {
      gra_para_region_mgr.Add_Rid_Into_Minor_Vector(rid);
    }	
    if(para_region) 
      para_region->Add_BB(bb);
#endif


    if (!GRA_use_old_conflict) {
      GTN_SET*    needs_a_register = GTN_SET_Create(GTN_UNIVERSE_size,
						    GRA_pool);
      needs_a_register = GTN_SET_CopyD(needs_a_register,BB_live_in(gbb->Bb()),
				       GRA_pool);
      needs_a_register = GTN_SET_IntersectionD(needs_a_register,
					       BB_defreach_in(gbb->Bb()));
      needs_a_register =
	GTN_SET_UnionD_Intersection(needs_a_register,BB_live_out(gbb->Bb()),
				    BB_defreach_out(gbb->Bb()),
				    GRA_pool);
      gbb->Needs_A_Register_Set(needs_a_register);
    }
  }

  //
  // allocate the subuniverses for the lranges now that we know the
  // total count of regions.
  //
  GRA_REGION_PREV_ALLOC_ITER iter;
  for (iter.Init(); ! iter.Done(); iter.Step()) {
    GRA_REGION *region = iter.Current();
    region->Alloc_Subuniverses();
  }

  GRA_Trace_Memory("After Create_GRA_BBs_And_Regions()");

  Identify_Region_Boundries();
  GRA_Trace_Memory_Counts();
}

/////////////////////////////////////
inline void
Region_TN_Reference( TN* tn, GRA_REGION* region )
/////////////////////////////////////
//
//  We have found a reference to 'tn' in 'region' Make a live range for it if
//  none already made.
//
/////////////////////////////////////
{
  if ( TN_Is_Allocatable(tn) && lrange_mgr.Get(tn) == NULL ) {
    if (Get_Trace(TP_SWPIPE, 4)) {
      fprintf(TFile, "Region_TN_Reference:  TN%d - %s\n",
	      TN_number(tn),
	      REGISTER_name(TN_register_class(tn), TN_register(tn)));
    }
    lrange_mgr.Create_Region(tn,region);
  }
}

/////////////////////////////////////
static void
Scan_Region_BB_For_Referenced_TNs( GRA_BB* gbb )
/////////////////////////////////////
//
//  Scan the OPs in 'gbb', a GRA_BB in a preallocated region, making LRANGEs
//  for all the referenced TNs.  Notice
//  the difference between this and Scan_Complement_BB_For_Referenced_TNs.  We
//  are doing this before we even create the complement LRANGEs so that we can
//  tell whether a given TN referenced in the complement actually belongs to
//  one of the regions.  If a TN is referenced both in a region and the
//  complement, The references in the complement should all be copies that we
//  try to eliminate through preferencing.
//
/////////////////////////////////////
{
  INT i;
  GRA_BB_OP_FORWARD_ITER iter;
  GRA_REGION *region = gbb->Region();

  lrange_mgr.Clear_One_Set();

  if (BB_rotating_kernel(gbb->Bb())) {
    TN *ded_tns[ISA_REGISTER_CLASS_MAX + 1][REGISTER_MAX + 1];
    ISA_REGISTER_CLASS rc;
    FOR_ALL_ISA_REGISTER_CLASS(rc) {
      for (REGISTER r = 0; r <= REGISTER_MAX; r++)
	ded_tns[rc][r] = NULL;
    }
    BB *bb = gbb->Bb();
    ANNOTATION *annot = ANNOT_Get(BB_annotations(bb), ANNOT_ROTATING_KERNEL);
    ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(annot);
      
    INT i;
    for (i = 0; i < ROTATING_KERNEL_INFO_copyin(info).size(); i++) {
      TN *tn = ROTATING_KERNEL_INFO_copyin(info)[i];
      ded_tns[TN_register_class(tn)][TN_register(tn)] = tn;
      Region_TN_Reference(tn,region);
    }
    for (i = 0; i < ROTATING_KERNEL_INFO_copyout(info).size(); i++) {
      TN *tn = ROTATING_KERNEL_INFO_copyout(info)[i];
      ded_tns[TN_register_class(tn)][TN_register(tn)] = tn;
      Region_TN_Reference(tn,region);
    }
#ifdef TARG_IA64
    for (i = 0; i < ROTATING_KERNEL_INFO_localdef(info).size(); i++) {
      TN *tn = ROTATING_KERNEL_INFO_localdef(info)[i];
      ded_tns[TN_register_class(tn)][TN_register(tn)] = tn;
      Region_TN_Reference(tn,region);
    }
#endif

    OP *op;
    FOR_ALL_BB_OPs(bb, op) {
      for (INT j = 0; j < OP_opnds(op); j++) {
	TN *tn = OP_opnd(op, j);
	if (TN_is_register(tn) && 
	    !TN_is_dedicated(tn) &&
	    REGISTER_Is_Rotating(TN_register_class(tn), TN_register(tn))) {
	  ded_tns[TN_register_class(tn)][TN_register(tn)] = tn;
	  Region_TN_Reference(tn,region);
	}
      }
      for (INT k = 0; k < OP_results(op); k++) {
	TN *tn = OP_result(op, k);
	if (TN_is_register(tn) && 
	    !TN_is_dedicated(tn) &&
	    REGISTER_Is_Rotating(TN_register_class(tn), TN_register(tn))) {
	  ded_tns[TN_register_class(tn)][TN_register(tn)] = tn;
	  Region_TN_Reference(tn,region);
	}
      }
    }
    
    FOR_ALL_ISA_REGISTER_CLASS(rc) {
#ifdef HAS_ROTATING_REGISTERS
      if (REGISTER_Has_Rotating_Registers(rc)) {
	INT size = REGISTER_bit_size(rc, REGISTER_First_Rotating_Registers(rc)) / 8;
	REGISTER last_reg = REGISTER_Last_Rotating_Registers(rc);
	if (rc == ISA_REGISTER_CLASS_integer) {
	  last_reg = REGISTER_First_Rotating_Registers(rc) +
	    REGISTER_Number_Stacked_Rotating(rc) - 1;
	}
	for (REGISTER r = REGISTER_First_Rotating_Registers(rc); 
	     r <= last_reg;
	     r++) {
	  TN *tn = ded_tns[rc][r];
	  if (tn == NULL) {
	    tn = Gen_Register_TN(rc, size);
	    Set_TN_register(tn, r);
	  }
	  Region_TN_Reference(tn,region);
	}
      }
#endif
      // non-rotating registers must be mentioned in the copyin/copyout code.
    }
  } else {
    for (iter.Init(gbb); ! iter.Done(); iter.Step()) {
      OP* op = iter.Current();

      for ( i = OP_results(op) - 1; i >= 0; --i ) {
	Region_TN_Reference(OP_result(op,i),region);
      }
      
      for ( i = OP_opnds(op) - 1; i >= 0; --i ) {
	if ( TN_Is_Allocatable(OP_opnd(op,i)) )
	  Region_TN_Reference(OP_opnd(op,i),region);
      }
    }
  }
}

#ifdef TARG_IA64
/////////////////////////////////////
 static void
 Create_Global_Dedicated_TN_LRANGEs(void)
/////////////////////////////////////
//
//  GRA assumes live range of dedicated TN is local one. This assumption
//  is not always true. The definitions of dedicated TN and their uses may
//  be live across multiple blocks. It may be expensive to build global live
//  ranges of dedicated TN and construct interference graph for them. On the
//  other hand, the change to make that happen may be huge. This solution
//  solve the problem: We divide the live range of dedicated TN <t> into
//  two parts:
//     1) the set of basic blocks in which <t> has real occurrrences.
//     2) the set of basic blocks in which <t> does not have real occurrence.
//
//  As mentioned above, the live-range snippet in part-1) is currently handled
//  as local wired live range. The coloring of live range snippet in part-1) is
//  correct.
//
//  What we concerned about is that register bound to <t> will be used to
//  color <t>'s neighbors in part-2).  Therefore, we should remove the register
//  associated with <t> from the available register set of blocks in part-2) to
//  prevent that from happening. This function serves this purpose.
//
//  The net result of this function is equivalent to construting a global live
//  range for dedicated TN and let him join the coloring process, hence the name.
//
/////////////////////////////////////////////
{

  MEM_POOL_Popper mp(&MEM_local_nz_pool);
  
  GTN_SET* ded_gtns = GTN_SET_Create_Empty (Last_Dedicated_TN, mp.Pool());
  
  // loop over all dedicated TNs
  for (TN_NUM i = Last_Dedicated_TN; i > 0; i--) {
    TN* tn = TNvec(i);
    // ignore local dedicated TN
    if (!TN_is_global_reg(tn)) continue;
    
    // ignore TN bound to unallocatable register
    // ignore TN bound to unallocatable register
    REGISTER_SET alloc = REGISTER_CLASS_allocatable(TN_register_class (tn));
    if (!REGISTER_SET_MemberP(alloc, TN_register(tn))) continue;
    
    ded_gtns = GTN_SET_Union1D (ded_gtns, tn, mp.Pool());
  }
  
  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    GRA_BB* gbb = NULL;
    GTN_SET* live_use = BB_live_use(bb);
    GTN_SET* def_reach_in = BB_defreach_in(bb);
    
    for (TN* tn = GTN_SET_Intersection_Choose(ded_gtns, BB_live_in(bb));
	 tn != GTN_SET_CHOOSE_FAILURE;
	 tn = GTN_SET_Intersection_Choose_Next(ded_gtns, BB_live_in(bb), tn)) {
      if (GTN_SET_MemberP (def_reach_in, tn) &&
	  !GTN_SET_MemberP (live_use, tn)) {
	// dedicated TN has no occurrence in this block but it is live
        // through this block.
        gbb = gbb ? gbb : gbb_mgr.Get(bb);
	gbb->Make_Register_Used(TN_register_class(tn), TN_register(tn));
      }
    }
  }/* end of outer for-loop */
}
#endif

/////////////////////////////////////
static void
Create_LRANGEs(void)
/////////////////////////////////////
//
//  Make all the LRANGEs.
//
/////////////////////////////////////
{
  GRA_REGION_PREV_ALLOC_ITER region_iter;
  GRA_REGION_GBB_ITER gbb_iter;
  GTN_UNIVERSE_ITER gtn_iter;
  ISA_REGISTER_CLASS rc;

  GRA_Init_Trace_Memory();

  // Create LRANGES with references in the regions first.  This must be done
  // first so these can be associated with the regions and so we can handle
  // the preference-creating copies around the borders of the regions correctly.

  for (region_iter.Init(); ! region_iter.Done(); region_iter.Step()) {
    GRA_REGION* region = region_iter.Current();

    for (gbb_iter.Init(region); !gbb_iter.Done(); gbb_iter.Step()) {
      GRA_BB* gbb = gbb_iter.Current();

      // TODO: We could combine this scan with one to create the
      // interference...

      Scan_Region_BB_For_Referenced_TNs(gbb);

      // find all border (i.e. prolog/epilog) blocks.  its kind of a hack
      // to put it here, but we won't need it once we have marked prolog
      // and epilog blocks for all loops.  at least doing it it this routine
      // saves another walk over the blocks in the PU
      if (GRA_loop_splitting) {
	gbb->Check_Loop_Border();
      }
    }
  }

  // Create local LRANGEs for the complement blocks.

  for (gbb_iter.Init(gra_region_mgr.Complement_Region()); ! gbb_iter.Done();
       gbb_iter.Step()) {
    GRA_BB* gbb = gbb_iter.Current();

    // see comments before call above
    if (GRA_loop_splitting) {
      gbb->Check_Loop_Border();
    }

    FOR_ALL_ISA_REGISTER_CLASS( rc ) {
      if ( gbb->Register_Girth(rc) > 0 )
        gbb->Create_Local_LRANGEs(rc,gbb->Register_Girth(rc));
    }
  }

  // All the other global TNs have corresponding complement live ranges:

  for ( GTN_UNIVERSE_ITER_Init(&gtn_iter);
        ! GTN_UNIVERSE_ITER_Done(&gtn_iter);
        GTN_UNIVERSE_ITER_Step(&gtn_iter)
  ) {
    TN* tn = GTN_UNIVERSE_ITER_Current(&gtn_iter);

    if ( TN_Is_Allocatable(tn) && ! lrange_mgr.Get(tn) && TN_is_global_reg(tn))
      lrange_mgr.Create_Complement(tn);
  }
#ifdef TARG_IA64 
  Create_Global_Dedicated_TN_LRANGEs();
#endif
  GRA_Trace_Memory("After Create_LRANGEs()");
}

/////////////////////////////////////
static void
Set_Call_Flag(LRANGE* lrange, BB* bb)
/////////////////////////////////////
//
//  Set the appropriate call flag for the live range.  If the call
//  block is infrequently executed, and the lrange spans no high
//  frequency calls, then we want the spans_infreq_call flag set.
//  This is so that we might allocate it to a caller saved register
//  (splitting around infrequent call blocks cheaper than saving
//  and restoring the callee saved register).
//
/////////////////////////////////////
{
  if (!lrange->Spans_A_Call()) {
    //
    // Infrequent call splitting won't work without the code to choose
    // the best split.  Don't do it if we've turned that off.
    //
    if ((BB_freq(bb) > GRA_call_split_freq) || !GRA_choose_best_split) {
      lrange->Spans_A_Call_Set();
      lrange->Spans_Infreq_Call_Reset();
    } else {
      lrange->Spans_Infreq_Call_Set();
    }
  }
}

/////////////////////////////////////
static void
Create_Live_BB_Sets(void)
/////////////////////////////////////
//
//  Set up the live-bb sets for each LRANGE.  Here is a troublesome N**2 loop.
//
/////////////////////////////////////
{
  BB *bb;
  TN *tn;

  GRA_Init_Trace_Memory();

#ifdef KEY
  GTN_SET *incoming_GTNs;
  GTN_SET *outgoing_GTNs;
  if (GRA_optimize_boundary) {
    // Those GTNs that are live_in and defreach_in.
    incoming_GTNs = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
    // Those GTNs that are live_out and defreach_out.
    outgoing_GTNs = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
  }
#endif

  for ( bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ) {
    GRA_BB* gbb = gbb_mgr.Get(bb);

#ifdef KEY
    if (GRA_optimize_boundary) {
      /*	TODO:  Define GTN_SET_IntersectionR in gtn_set.h.
      GTN_SET_IntersectionR(incoming_GTNs, BB_live_in(bb), BB_defreach_in(bb));
      GTN_SET_IntersectionR(outgoing_GTNs, BB_live_out(bb), BB_defreach_out(bb));
      */
      BS_IntersectionR(incoming_GTNs, BB_live_in(bb), BB_defreach_in(bb));
      BS_IntersectionR(outgoing_GTNs, BB_live_out(bb), BB_defreach_out(bb));
    }
#endif

    for ( tn = GTN_SET_Intersection_Choose(BB_live_in(bb),
                                           BB_defreach_in(bb));
          tn != GTN_SET_CHOOSE_FAILURE;
          tn = GTN_SET_Intersection_Choose_Next(BB_live_in(bb),
                                                BB_defreach_in(bb),
                                                tn)
    ) {
      if ( TN_Is_Allocatable(tn) ) {
	lrange_mgr.Get(tn)->Add_Live_BB(gbb);
#ifdef KEY
	if (GRA_optimize_boundary) {
	  // The lrange enters the BB.  If the lrange also exits the BB, then
	  // the BB is an internal BB, else the BB is a boundary BB.  (Ignore
	  // gaps inside the BB for now.)
	  // TODO:  Implement gaps.
	  if (GTN_SET_MemberP(outgoing_GTNs, tn))
	    lrange_mgr.Get(tn)->Add_Internal_BB(gbb);
	  else
	    lrange_mgr.Get(tn)->Add_Boundary_BB(gbb);
	}
#endif
      }
    }

    for ( tn = GTN_SET_Intersection_Choose(BB_live_out(bb),
                                           BB_defreach_out(bb));
          tn != GTN_SET_CHOOSE_FAILURE;
          tn = GTN_SET_Intersection_Choose_Next(BB_live_out(bb),
                                                BB_defreach_out(bb),
                                                tn)
    ) {
      if ( TN_Is_Allocatable(tn) ) {
        LRANGE* lrange = lrange_mgr.Get(tn);

        lrange->Add_Live_BB(gbb);

#ifdef KEY
	if (GRA_optimize_boundary) {
	  // The lrange exits the BB.  If the lrange also enters the BB, then
	  // the BB is an internal BB, else the BB is a boundary BB.  (Ignore
	  // gaps inside the BB for now.)
	  if (GTN_SET_MemberP(incoming_GTNs, tn))
	    lrange->Add_Internal_BB(gbb);
	  else
	    lrange->Add_Boundary_BB(gbb);
	}
#endif

        if ( BB_call(gbb->Bb()) ) {
          if (    lrange->Type() == LRANGE_TYPE_COMPLEMENT
               && gbb->Region() == gra_region_mgr.Complement_Region()
          ) {
	    Set_Call_Flag(lrange, gbb->Bb());
          }
          else if (    lrange->Type() == LRANGE_TYPE_REGION
                    && lrange->Region() == gbb->Region()
          ) {
	    Set_Call_Flag(lrange, gbb->Bb());
          }

	  if (gbb->Setjmp())
	    lrange->Spans_A_Setjmp_Set();
	}

	if (BB_mod_rotating_registers(gbb->Bb()))
	  lrange->Spans_Rot_Reg_Clob_Set();
	else if (BB_mod_pred_rotating_registers(gbb->Bb()) &&
		 Is_Predicate_REGISTER_CLASS(lrange->Rc()))
	  lrange->Spans_Rot_Reg_Clob_Set();

#ifdef TARG_X8664
	if (gbb->Savexmms() && lrange->Rc() == ISA_REGISTER_CLASS_float)
	  lrange->Spans_Savexmms_Set();
	if (gbb->x87_OP())
	  lrange->Spans_x87_OP_Set();
	if (gbb->mmx_OP())
	  lrange->Spans_mmx_OP_Set();
#endif
      }
    }
  }
  GRA_Trace_Memory("After Create_Live_BB_Sets()");
}

// For each ISA_REGISTER_CLASS, a vector indexed by register number that takes 
// us to the most recently created wired LRANGE.  If it wasn't for the current
// block, we know we haven't created one for the current block yet.  Since we
// scan each block only once to both create the wired LRANGEs and do the
// preferencing, this works.
LRANGE** wired_lranges[ISA_REGISTER_CLASS_MAX + 1];

/////////////////////////////////////
static void
Initialize_Wired_LRANGEs(void)
/////////////////////////////////////
//
//  Setup the wired LRANGE mapping.
//
/////////////////////////////////////
{
  ISA_REGISTER_CLASS rc;

  FOR_ALL_ISA_REGISTER_CLASS( rc ) {
    LRANGE** vec =
      TYPE_MEM_POOL_ALLOC_N(LRANGE*,&MEM_local_nz_pool, REGISTER_MAX + 1);
    bzero(vec,(REGISTER_MAX + 1) * sizeof(LRANGE*));
    wired_lranges[rc] = vec;
  }
}

#ifdef HAS_STACKED_REGISTERS
#define REGISTER_IS_ALLOCATABLE(rc,reg)	\
	(REGISTER_allocatable(rc, reg)	\
	|| (REGISTER_Is_Stacked(rc, reg) \
	   && REGISTER_Is_Allocatable_Stacked_Register(rc,reg)))
#else
#define REGISTER_IS_ALLOCATABLE(rc,reg)	REGISTER_allocatable(rc, reg)
#endif

/////////////////////////////////////
static void
Wired_TN_Reference( GRA_BB* gbb, ISA_REGISTER_CLASS rc, REGISTER reg ,
		   LRANGE_LIST **wired_locals)
/////////////////////////////////////
//
//  Possibly Generate a local LRANGE for a wired TN reference to <rc> <reg> in
//  the given <gbb>.
//
/////////////////////////////////////
{
  if ( REGISTER_IS_ALLOCATABLE(rc, reg) ) {
    LRANGE* lrange = wired_lranges[rc][reg];

    if ( lrange == NULL || lrange->Gbb() != gbb ) {
      wired_lranges[rc][reg] = gbb->Create_Wired_LRANGE(rc,reg);
      wired_locals[rc] = LRANGE_LIST_Push(wired_lranges[rc][reg],
					  wired_locals[rc],
					  &MEM_local_nz_pool);
    }
  }
  // Else not allocatable
}

/////////////////////////////////////
static BOOL
Get_Possibly_Wired_Reference( TN* tn, LRANGE** lrange )
/////////////////////////////////////
//
//  Find the LRANGE that represents <tn> which might be a dedicated TN.
//  Return TRUE to indicate that a LRANGE was found, with the lrange returned
//  by reference in <lrange>.  <gbb> is the current GRA_BB and is passed in to
//  allow some extra validity checking.
//
/////////////////////////////////////
{
  if ( TN_is_dedicated(tn) ) {
    REGISTER            reg = TN_register(tn);
    ISA_REGISTER_CLASS  rc  = TN_register_class(tn);

    if ( ! REGISTER_IS_ALLOCATABLE(rc, reg))
      return FALSE;     // Not allocatable

    *lrange = wired_lranges[TN_register_class(tn)][TN_register(tn)];
    return TRUE;
  }
  else if ( ! TN_Is_Allocatable(tn) )
    return FALSE;
  else {
    *lrange = lrange_mgr.Get(tn);
    return *lrange != NULL;
  }
}

/////////////////////////////////////
static BOOL
Complement_TN_Reference( OP* op, TN* tn, GRA_BB* gbb, LUNIT** lunitp,
			LRANGE_LIST **wired_locals )
/////////////////////////////////////
//
//  Record a reference to <tn> in <gbb> by making a LUNIT if TN has a
//  complement LRANGE and the LUNIT hasn't already been created.
//  Increment the reference count of the created or preexisting LUINIT.
//
//  If <tn> is an allocatable global TN that belongs to the complement, return
//  TRUE with the associated LUNIT in returned by reference in <lunitp>.
//
/////////////////////////////////////
{
  if ( TN_is_dedicated(tn) ) {
    Wired_TN_Reference(gbb,TN_register_class(tn),TN_register(tn),wired_locals);
    return FALSE;
  }
  else if ( ! TN_Is_Allocatable(tn) )
    return FALSE;
  else if ( TN_is_global_reg(tn) ) {
    LRANGE* lrange = lrange_mgr.Get(tn);

    if (!(GTN_SET_Intersection_MemberP(BB_live_in(gbb->Bb()),
				       BB_defreach_in(gbb->Bb()), tn)
	  || GTN_SET_Intersection_MemberP(BB_live_out(gbb->Bb()),
					  BB_defreach_out(gbb->Bb()), tn))) {
      // The following warning should be OK now because we do a renaming
      // pass just before GRA which should remove such GTNs.
      //
      // Hack for now to turn off devwarns if HB formation enabled.  Getting
      // *way* too many of these.
      //
      if (!OP_dummy(op) && !HB_formation) {
	DevWarn("GRA given Local reference to global TN%d[PREG%d] in BB%d. "
		"Will rename.", TN_number(tn), TN_To_PREG(tn),BB_id(gbb->Bb()));
      }
      gbb_needs_rename = TRUE;
      return FALSE;
    }
    else if ( lrange->Type() != LRANGE_TYPE_COMPLEMENT )
      return FALSE;
    else if ( lrange_mgr.One_Set_MemberP(lrange) )
      (void) lrange->Find_LUNIT_For_GBB(gbb,lunitp);
    else {
      *lunitp = LUNIT_Create(lrange,gbb);
      (*lunitp)->True_Reference_Set();
      lrange_mgr.One_Set_Union1(lrange);
    }
    return TRUE;
  }

  return FALSE;
}

/////////////////////////////////////
static void
Complement_Copy( OP* op, GRA_BB *gbb, std::list<GRA_PREF_CAND*>& pref_list )
/////////////////////////////////////
//
//  <op> is a copy OP in the complement.  It implies a preference if it
//  copies between LRANGEs of different types.  Note the preference if
//  so. <gbb> is the block in which the copy occurs.
//
/////////////////////////////////////
{
  TN* result        = OP_result(op,0);
  TN* opnd          = CGTARG_Copy_Operand_TN(op);
  LRANGE* result_lr = NULL;
  LRANGE* opnd_lr   = NULL;

  if (!GRA_preference_all) {
    return;
  }

  // Used for GRA preference triaging.
  if (TN_number(result) == GRA_non_preference_tn_id) return;

  BOOL res_found = Get_Possibly_Wired_Reference(result,&result_lr);
  BOOL op_found = Get_Possibly_Wired_Reference(opnd,&opnd_lr);
  if ( res_found && op_found && result_lr->Type() != opnd_lr->Type()
      ) {
    if ((result_lr->Type() == LRANGE_TYPE_REGION ||
	opnd_lr->Type() == LRANGE_TYPE_REGION) &&
	GRA_preference_glue) {
      //
      // Still assume no conflicts in glue code copies since we split
      // entry/exit glue blocks and insert copies as needed.
      //
      result_lr->Preference_Copy(opnd_lr,gbb);

    } else if (GRA_preference_dedicated &&
	       (TN_is_dedicated(result) || TN_is_dedicated(opnd))) {
      GRA_PREF_CAND* cand = gra_pref_mgr.CAND_Create(result, opnd,
						 &MEM_local_nz_pool);
      pref_list.push_front(cand);
      GRA_Trace_Possible_Preference_Copy(result_lr, opnd_lr, gbb);
    }
  } else {
    //
    // if the copy is to or from a region lrange, but the source/destination
    // is local, we still want to add this to the list of blocks with glue
    // references.
    //
    if (result_lr != NULL && result_lr->Type() == LRANGE_TYPE_REGION) {
      lrange_mgr.Add_GBB_With_Glue_Reference(result_lr,gbb);
    } else if (opnd_lr != NULL &&
	       opnd_lr->Type() == LRANGE_TYPE_REGION) {
      lrange_mgr.Add_GBB_With_Glue_Reference(opnd_lr,gbb);
    } else if (GRA_preference_globals &&
	       result_lr != NULL &&
	       result_lr->Type() == LRANGE_TYPE_COMPLEMENT &&
	       opnd_lr != NULL &&
	       opnd_lr->Type() == LRANGE_TYPE_COMPLEMENT) {
      //
      // set up TN for possible global preferencing
      //
      LUNIT *op_lunit;
      LUNIT *result_lunit;
      if (opnd_lr->Find_LUNIT_For_GBB(gbb, &op_lunit) &&
	  result_lr->Find_LUNIT_For_GBB(gbb, &result_lunit)) {
	GRA_PREF_CAND* cand = gra_pref_mgr.CAND_Create(result, opnd,
						   &MEM_local_nz_pool);
	pref_list.push_front(cand);
	GRA_Trace_Possible_Preference_Copy(result_lr, opnd_lr, gbb);
      }
    }
  }
}

/////////////////////////////////////
static void
Avoid_RA_In_Call_Argument( OP* op )
/////////////////////////////////////
//
//  Awful hack to avoid the horrible return address register for the operand
//  of a call.
//
/////////////////////////////////////
{
  INT i;

  if( RA_TN == NULL )
    return;

  for ( i = OP_opnds(op) - 1; i >= 0; --i ) {
    LRANGE *lrange = lrange_mgr.Get(OP_opnd(op,i));

    if (    lrange != NULL
         && lrange->Type() == LRANGE_TYPE_COMPLEMENT
         && lrange->Rc() == TN_register_class(RA_TN)
    ) {
      lrange->Avoid_RA_Set();
    }
  }
}

/////////////////////////////////////
/*ARGSUSED1*/
static BOOL
Region_TN( TN* tn, GRA_BB* gbb )
/////////////////////////////////////
//
//  Is the LRANGE corresponding to a reference to <tn> in <gbb> a REGION
//  LRANGE?
//
/////////////////////////////////////
{
  LRANGE* lrange = lrange_mgr.Get(tn);

  return lrange != NULL && lrange->Type() == LRANGE_TYPE_REGION;
}

/////////////////////////////////////
static BOOL
Region_LRANGEs_Referenced( OP* op, GRA_BB* gbb, TN** region_tn )
/////////////////////////////////////
//
//  Are any of <op>'s operands REGION LRANGEs?.  If a region LRANGE is found,
//  return it by reference in <region_tn>.
//
/////////////////////////////////////
{
  INT32 i;

  for ( i = OP_opnds(op) - 1; i >= 0; --i ) {
    if ( Region_TN(OP_opnd(op,i),gbb) ) {
      *region_tn = OP_opnd(op,i); 
     return TRUE;
    }
  }

  for ( i = OP_results(op) - 1; i >= 0; --i ) {
    if ( Region_TN(OP_result(op,i),gbb) ) {
      *region_tn = OP_result(op,i);
      return TRUE;
    }
  }

  return FALSE;
}

/////////////////////////////////////
static BOOL
Needs_Rename( TN* tn, GRA_BB* gbb, TN_MAP map, TN** local_rename )
/////////////////////////////////////
//
//  Should we rename <tn> references in <gbb>?  If so, return the new TN to
//  use for the rename by reference in <local_rename>.  <map> is created and
//  deleted by the client, but is used to keep track of the renames for this
//  Needs_Rename and shouldn't be directly used outside of it.
//
/////////////////////////////////////
{
  if ( ! TN_Is_Allocatable(tn) )
    return FALSE;
  else if ( ! TN_is_global_reg(tn) )
    return FALSE;
  else if (GTN_SET_Intersection_MemberP(BB_live_in(gbb->Bb()),
					BB_defreach_in(gbb->Bb()), tn)
  ) {
    return FALSE;
  }
  else if (GTN_SET_Intersection_MemberP(BB_live_out(gbb->Bb()),
					BB_defreach_out(gbb->Bb()), tn))
    return FALSE;
  else {
    if ( TN_MAP_Get(map,tn) != NULL ) {
      *local_rename = (TN*) TN_MAP_Get(map,tn);
      return TRUE;
    }
    else {
      *local_rename = Dup_TN(tn);
      TN_MAP_Set(map,tn,(void*) *local_rename);
      return TRUE;
    }
  }
}

/////////////////////////////////////
static void
Local_Rename( GRA_BB* gbb )
/////////////////////////////////////
//
//  There are some "global" TNs referenced ing <gbb> that are neither live in
//  nor out of <gbb>.  Rename the references with locals.
//
/////////////////////////////////////
{
  INT i;
  GRA_BB_OP_FORWARD_ITER iter;
  TN_MAP map = TN_MAP_Create();

  for (iter.Init(gbb); ! iter.Done(); iter.Step()) {
    OP*  xop = iter.Current();
    TN* local_rename;

    for ( i = OP_results(xop) - 1; i >= 0; --i ) {
      if ( Needs_Rename(OP_result(xop,i),gbb,map,&local_rename) )
	Set_OP_result(xop, i, local_rename);
    }

    for ( i = OP_opnds(xop) - 1; i >= 0; --i ) {
      if ( Needs_Rename(OP_opnd(xop,i),gbb,map,&local_rename) )
        Set_OP_opnd(xop, i, local_rename);
    }
  }

  TN_MAP_Delete(map);
}
/////////////////////////////////////
static void
List_Preferenced_Wired_Locals( GRA_BB *gbb, LRANGE_LIST **wired_locals )
/////////////////////////////////////
//
//  place wired local lranges that are preferenced to a global lrange
//  in the list of lranges for the block.  those that are not preferenced
//  will get their registers immediately placed in the used list for the
//  block, but will not be in any global (or local) lranges conflict graph.
//  this is okay, because the wired live ranges get their colors immediately
//  anyway.  thus, we save the expense of having them in the conflict graph
//  at no performance penalty (since there is no preferencing benefit to
//  having them in the graph).
//
/////////////////////////////////////
{
  LRANGE_LIST *l, *tmp;
  LRANGE *lrange;
  ISA_REGISTER_CLASS rc;

  FOR_ALL_ISA_REGISTER_CLASS( rc ) {
    for ( l = wired_locals[rc]; l != NULL; l = LRANGE_LIST_rest(l) ) {
      lrange = LRANGE_LIST_first(l);
      if (!lrange->Has_Preference()) {
	tmp = gbb->Unpreferenced_Wired_LRANGEs(rc);
	tmp = LRANGE_LIST_Push(lrange, tmp, GRA_pool);
	gbb->Unpreferenced_Wired_LRANGEs_Set(rc, tmp);
      } else {
	gbb->Incr_Local_Lrange_Count(rc);
	gbb->Local_Lranges_Set(rc, 
			  gbb->Local_Lranges(rc)->BB_Local_List_Push(lrange));
      }      
    }
  } 
}

/////////////////////////////////////
static BOOL
OP_Refs_Home(OP* op, TN* home_tn)
/////////////////////////////////////
//
//  Determine if the given op references the home location (op assumed
//  to be a memory reference and home_tn to be homeable).
//
/////////////////////////////////////
{
  WN* wn = Get_WN_From_Memory_OP(op);
  if (wn != NULL) {
    if (Aliased(Alias_Manager, TN_home(home_tn), wn) == SAME_LOCATION) {
      return TRUE;
    } 
  }
  return FALSE;
}

/////////////////////////////////////
static BOOL
Store_To_Home(OP* op, TN* op_tn)
/////////////////////////////////////
//
//  Determine if this op is storing to the given tn's
//  home location.
//
/////////////////////////////////////
{
  if (TN_is_gra_homeable(op_tn) && OP_store(op) &&
      op_tn == OP_opnd(op,TOP_Find_Operand_Use(OP_code(op), OU_storeval)) &&
      OP_Refs_Home(op, op_tn)) {
    return TRUE;
  }
  return FALSE;
}

/////////////////////////////////////
static BOOL
Load_From_Home(OP* op, TN* op_tn)
/////////////////////////////////////
//
//  Determine if this op is loading from the given tn's
//  home location.
//
/////////////////////////////////////
{
  if (TN_is_gra_homeable(op_tn) && OP_load(op) && op_tn == OP_result(op, 0) &&
      OP_Refs_Home(op, op_tn)) {
      return TRUE;
  }
  return FALSE;
}

#ifdef TARG_IA64
extern INIT_USE_ONLY_GTN *GTN_USE_ONLY;
extern void Build_GTN_In_List (TN *tn,BB *bb);
#endif

/////////////////////////////////////
static void
Scan_Complement_BB_For_Referenced_TNs( GRA_BB* gbb )
/////////////////////////////////////
//
//  Walk the OPs in <gbb> to discover references to complement LRANGEs.
//  Discover preferences.
//
/////////////////////////////////////
{
  INT i, op_count;
  GRA_BB_OP_FORWARD_ITER iter;
  LUNIT* lunit;
  LRANGE_LIST *wired_locals[ISA_REGISTER_CLASS_MAX+1];
  hTN_MAP live_data;
  std::list<GRA_PREF_CAND*> pref_list;
  bzero(&wired_locals, (ISA_REGISTER_CLASS_MAX+1) * sizeof(LRANGE_LIST*));

  MEM_POOL_Push(&MEM_local_nz_pool);
  live_data = hTN_MAP_Create(&MEM_local_nz_pool);
  gbb_needs_rename = FALSE;

  lrange_mgr.Clear_One_Set();
  Initialize_Wired_LRANGEs();
#ifdef TARG_IA64
  OP_OF_ONLY_DEF Op_Of_Only_Def(&MEM_local_nz_pool);
  Op_Of_Only_Def.Set_OPS_OF_ONLY_DEF(gbb);
#endif

  for (iter.Init(gbb), op_count=1; ! iter.Done(); iter.Step(), op_count++ ) {
    OP*  xop = iter.Current();

    for ( i = OP_opnds(xop) - 1; i >= 0; --i ) {
      TN *op_tn = OP_opnd(xop, i);
      if (! TN_is_register(op_tn))
	continue;
      GRA_PREF_LIVE* gpl = (GRA_PREF_LIVE*) hTN_MAP_Get(live_data, op_tn);
      if (!gpl) {
	gpl = gra_pref_mgr.LIVE_Create(&MEM_local_nz_pool);
	hTN_MAP_Set(live_data, op_tn, gpl);
      }
      if (Complement_TN_Reference(xop, op_tn, gbb, &lunit, wired_locals)) {
#ifdef TARG_IA64
        lunit->Has_Use_Set();
#endif
        if (!lunit->Has_Def()) {
	  lunit->Has_Exposed_Use_Set();
	  gpl->Exposed_Use_Set(TRUE);
	}
	if (Store_To_Home(xop, op_tn)) {
	  lunit->Syncs_With_Home_Set();
	}
      } else if (!gpl->Num_Defs()) {
	gpl->Exposed_Use_Set(TRUE);
      }
#ifdef TARG_IA64
      Build_GTN_In_List(op_tn,gbb->Bb());
#endif
    }

    for ( i = OP_results(xop) - 1; i >= 0; --i ) {
      TN* res_tn = OP_result(xop,i);
      if (! TN_is_register(res_tn))
	continue;
      GRA_PREF_LIVE* gpl = (GRA_PREF_LIVE*) hTN_MAP_Get(live_data, res_tn);
      if (!gpl) {
	gpl = gra_pref_mgr.LIVE_Create(&MEM_local_nz_pool);
	hTN_MAP_Set(live_data, res_tn, gpl);
      }

#ifdef TARG_IA64
      if (OP_cond_def(xop) && !Op_Of_Only_Def.FIND_OP(xop)) {
	// there is a hidden use
	if (Complement_TN_Reference(xop, res_tn, gbb, &lunit, wired_locals)) {
	    lunit->Has_Use_Set();
	    if (!lunit->Has_Def()) {
	      lunit->Has_Exposed_Use_Set();
	      gpl->Exposed_Use_Set(TRUE);
	    }
	  } else if (!gpl->Num_Defs()) {
	    gpl->Exposed_Use_Set(TRUE);
	  }
      } 
#else
      if (OP_cond_def(xop)) { // there is a hidden use
        if (Complement_TN_Reference(xop, res_tn, gbb, &lunit, wired_locals)) {
          if (!lunit->Has_Def()) {
	    lunit->Has_Exposed_Use_Set();
	    gpl->Exposed_Use_Set(TRUE);
	  }
        } else if (!gpl->Num_Defs()) {
	  gpl->Exposed_Use_Set(TRUE);
        }
      }
#endif // TARG_IA64

      gpl->Num_Defs_Set(gpl->Num_Defs() + 1);
      gpl->Last_Def_Set(op_count);

      if ( Complement_TN_Reference(xop, res_tn,gbb,&lunit, wired_locals)) {
	if (lunit) {
	  //
	  // Complement reference.  Want flag set if last def in the
	  // block is from the home location.
	  //
	  lunit->Has_Def_Set();
	  if (Load_From_Home(xop, res_tn) && !OP_cond_def(xop)) {
	    lunit->Def_From_Home_Set();
	  } else {
	    lunit->Def_From_Home_Reset();
	    lunit->Syncs_With_Home_Reset();
	  }
	}
      }
    }

#ifdef KEY
    // Treat clobbered registers as though they are result registers.
    // Bug 4579.
    if (OP_code(xop) == TOP_asm) {
      ASM_OP_ANNOT *asm_info = (ASM_OP_ANNOT *) OP_MAP_Get(OP_Asm_Map, xop);
      if (asm_info) {
	ISA_REGISTER_CLASS cl;
	FOR_ALL_ISA_REGISTER_CLASS(cl) {
	  REGISTER_SET clobbered_regs = ASM_OP_clobber_set(asm_info)[cl];
	  for (REGISTER reg = REGISTER_SET_Choose(clobbered_regs);
	       reg != REGISTER_UNDEFINED;
	       reg = REGISTER_SET_Choose_Next(clobbered_regs, reg)) {
	    Wired_TN_Reference(gbb, cl, reg, wired_locals);

	    TN *tn = Build_Dedicated_TN(cl, reg, 0);
	    GRA_PREF_LIVE* gpl = (GRA_PREF_LIVE*)hTN_MAP_Get(live_data, tn);
	    if (!gpl) {
	      gpl = gra_pref_mgr.LIVE_Create(&MEM_local_nz_pool);
	      hTN_MAP_Set(live_data, tn, gpl);
	    }
	    gpl->Num_Defs_Set(gpl->Num_Defs() + 1);
	    gpl->Last_Def_Set(op_count);
	  }
	} 
      }
    }
#endif

    if ( CGTARG_Is_Preference_Copy(xop) )
      Complement_Copy(xop, gbb, pref_list);
    else if ( ! OP_glue(xop) ) {
      TN* region_tn;
      DevAssert(! Region_LRANGEs_Referenced(xop,gbb,&region_tn),
                ("Illegal region reference to TN%d in BB%d",
                 TN_number(region_tn), BB_id(gbb->Bb())));
    }

    if ( OP_call(xop) )
      Avoid_RA_In_Call_Argument(xop);
  }

  if ( gbb_needs_rename )
    Local_Rename(gbb);

  //
  // find preferenced globals, and check if there is a conflict between
  // the two tn's in the block.
  //
  std::list<GRA_PREF_CAND*>::iterator gpci;
  for (gpci = pref_list.begin(); gpci != pref_list.end(); gpci++) {
    GRA_PREF_CAND* gpc = *gpci;
    TN* tn_dest = gpc->Dest();
    TN* tn_src = gpc->Source();
    LRANGE *lr_dest;
    LRANGE *lr_src;
    LUNIT* lunit = NULL;
    BOOL comp_pref;

    (void) Get_Possibly_Wired_Reference(tn_dest, &lr_dest);
    (void) Get_Possibly_Wired_Reference(tn_src, &lr_src);

    if (!TN_is_dedicated(tn_dest) && !TN_is_dedicated(tn_src)) {
      (void) lr_dest->Find_LUNIT_For_GBB(gbb, &lunit);
      comp_pref = TRUE;
    } else {
      comp_pref = FALSE;
    }

    GRA_PREF_LIVE* gpl_dest = (GRA_PREF_LIVE*) hTN_MAP_Get(live_data, tn_dest);
    GRA_PREF_LIVE* gpl_src = (GRA_PREF_LIVE*) hTN_MAP_Get(live_data, tn_src);

#ifdef KEY
    // Do the same tests as below to see if a preferencing copy is allowed, but
    // do it for dedicated registers.  Bug 4579.
    BOOL allow_copy = TRUE;

    // Disallow copy if src is a dedicated register that is redefined later.
    if (TN_register(tn_src) != REGISTER_UNDEFINED) {
      TN *tn_dedicated_src = Build_Dedicated_TN(TN_register_class(tn_src),
						TN_register(tn_src), 0);
      GRA_PREF_LIVE* gpl_dedicated_src =
	(GRA_PREF_LIVE*) hTN_MAP_Get(live_data, tn_dedicated_src);
      if (gpl_dedicated_src &&
	  gpl_dedicated_src->Last_Def() > gpl_dest->Last_Def())
	allow_copy = FALSE;
    }

    // Disallow copy if dest is a dedicated register with an exposed use or has
    // more than one definition in the block.
    if (TN_register(tn_dest) != REGISTER_UNDEFINED) {
      TN *tn_dedicated_dest = Build_Dedicated_TN(TN_register_class(tn_dest),
						 TN_register(tn_dest), 0);
      GRA_PREF_LIVE* gpl_dedicated_dest =
	(GRA_PREF_LIVE*) hTN_MAP_Get(live_data, tn_dedicated_dest);
      if (gpl_dedicated_dest &&
	  (gpl_dedicated_dest->Exposed_Use() ||
	   gpl_dedicated_dest->Num_Defs() > 1))
	allow_copy = FALSE;
    }
#endif

    //
    // If the source of the preferencing copy is redefined after
    // later in the block, or the destination has an exposed use or
    // there is more than one definition of it in the block, then
    // we are conservatively assuming that there is a conflict.
    //
    if (gpl_src->Last_Def() < gpl_dest->Last_Def() &&
	!gpl_dest->Exposed_Use() && gpl_dest->Num_Defs() == 1
#ifdef KEY
	&& allow_copy
#endif
	) {

      //
      // Complement to complement preference.
      //
      if (comp_pref) {
	TN *source_tn = lr_src->Tn();
	TN *dest_tn = lr_dest->Tn();
	lr_src->Add_Global_Pref(tn_dest);
	lr_dest->Add_Global_Pref(tn_src);
	lunit->Global_Pref_Set(lr_src);
      } else {
	//
	// Complement to dedicated preference.
	//
	lr_dest->Preference_Copy(lr_src, gbb);
      }
    } else {
      GRA_Trace_Preference_Conflict(lr_dest, lr_src, gbb);
    }
  }
	
  List_Preferenced_Wired_Locals(gbb, wired_locals);

  MEM_POOL_Pop(&MEM_local_nz_pool);
}

/////////////////////////////////////
static void
Create_LUNITs(void)
/////////////////////////////////////
//
//  Make LUNITs for the complement LRANGEs.
//
/////////////////////////////////////
{
  GRA_REGION_GBB_ITER gbb_iter;
  GRA_Init_Trace_Memory();

  MEM_POOL_Push(&MEM_local_nz_pool);
  Initialize_Wired_LRANGEs();

  for (gbb_iter.Init(gra_region_mgr.Complement_Region()); ! gbb_iter.Done();
       gbb_iter.Step() ) {
    GRA_BB* gbb = gbb_iter.Current();
    Scan_Complement_BB_For_Referenced_TNs(gbb);
  }
  MEM_POOL_Pop(&MEM_local_nz_pool);
  GRA_Trace_Memory("After Create_LUNITs()");
}

/////////////////////////////////////
static BOOL
Check_Global_Preference(LRANGE* lrange1, LRANGE* lrange2)
/////////////////////////////////////
//
//  Check to see if two global tn's have the potential to be
//  preferenced.  Check each lunit, and if one of the tn's is
//  defined in a block while the other one is live, and this is
//  not the block in which the preferencing copy occurs, then
//  they conflict and cannot be preferenced.  otherwise, add 
//  them to each other's preference list.
//
/////////////////////////////////////
{
  LRANGE_LUNIT_ITER iter;

  //
  // check for possible preference.
  //
  if (!lrange1->Check_Global_Pref(lrange2->Tn())) {
    return FALSE;
  }

  //
  // TODO: need an early exit test as this will get called with both lranges
  // and we need only do the tests once.
  //
  for (iter.Init(lrange1); ! iter.Done(); iter.Step()) {
    LUNIT *lunit = iter.Current();

    if (lunit->Has_Def() && lunit->Global_Pref() != lrange2 &&
	lrange2->Contains_BB(lunit->Gbb())) {
      LUNIT *lunit2;
      if (!lrange2->Find_LUNIT_For_GBB(lunit->Gbb(), &lunit2) ||
	  lunit2->Global_Pref() != lrange1) {
	GRA_Trace_Global_Preference_Failure(lrange1,lrange2,lunit->Gbb());
	lrange1->Remove_Global_Pref(lrange2->Tn());
	lrange2->Remove_Global_Pref(lrange1->Tn());
	return FALSE;
      }
    }
  }

  for (iter.Init(lrange2); ! iter.Done(); iter.Step()) {
    LUNIT *lunit = iter.Current();

    if (lunit->Has_Def() && lunit->Global_Pref() != lrange1 &&
	lrange1->Contains_BB(lunit->Gbb())) {
      LUNIT *lunit1;
      if (!lrange1->Find_LUNIT_For_GBB(lunit->Gbb(), &lunit1) ||
	  lunit1->Global_Pref() != lrange2) {
	GRA_Trace_Global_Preference_Failure(lrange1,lrange2,lunit->Gbb());
	lrange1->Remove_Global_Pref(lrange2->Tn());
	lrange2->Remove_Global_Pref(lrange1->Tn());
	return FALSE;
      }
    }
  }

  //
  // no conflict.  add preference copy.  must do it for each lunit that
  // contains a preferencing copy between the two so that the priority
  // of the copy is set correctly.
  //
  for (iter.Init(lrange1); ! iter.Done(); iter.Step()) {
    LUNIT *lunit = iter.Current();
    if (lunit->Global_Pref() == lrange2) {
      lrange1->Preference_Copy(lrange2, lunit->Gbb());
    }
  }

  for (iter.Init(lrange2); ! iter.Done(); iter.Step()) {
    LUNIT *lunit = iter.Current();
    if (lunit->Global_Pref() == lrange1) {
      lrange1->Preference_Copy(lrange2, lunit->Gbb());
    }
  }

  GRA_Trace_Global_Preference_Success(lrange1, lrange2);
  return TRUE;
}
    
/////////////////////////////////////
static void
Build_Complement_Interference_Graph(void)
/////////////////////////////////////
//
//  Build the coarse grained interference graph for the complement region.
//
//  This is an improvement to the algorithm for building interferences.  The
//  basic idea to improve on the classical interference building loop nest:
//  
//  for each lr0 in universe of live ranges
//      for each lr1 in universe of live ranges
//          if ( intersects(blocks(lr0),blocks(lr1)) && lr0 != lr1 )
//              Make_Interferece(lr1,lr0)
//  
//  What's wrong with this loop?  Of course it's N**2 in the number of live
//  ranges, but that's life in the big city.
//  
//  Actually there are several things wrong with this approach.  Firstly,
//  it's really LiveRangeCount**2 * (BlockCount/32).  This is because the
//  intersects test has to loop over the bit set words that represent the
//  blocks for the two live ranges.  This can actually get to be a largish
//  loop in some of the pathological cases (quad artithmetic, mostly, which
//  makes large numbers of blocks.)  Also, many live ranges contain few
//  blocks, yet this algorithm fails to take advantage of this fact and
//  instead check each pair of live ranges for interference.
//  
//  The improvement is to calculate the set of interfering live ranges by
//  walking the blocks in each live range.  The worst case performance is
//  LiveRangeCount * LiveRangeCount/32  * BlockCount, but its average case
//  performance is much better because of the large number of live ranges
//  that only contain just a few blocks.  So the new loop is like:
//  
//  for each lr0 in universe of live ranges {
//      interfering_lranges = 0
//      for each bb in blocks(lr0)
//          interfering_lranges += lranges_needing_a_register(bb)
//  
//      for each lr1 in interfering_lranges
//          Make_Interferece(lr1,lr0)
//  }
/////////////////////////////////////
{
  ISA_REGISTER_CLASS           rc;
  TN*                          tn;
  GTN_SET*                     interferences;
  GRA_REGION_RC_NL_LRANGE_ITER iter0;
  GRA_REGION_RC_NL_LRANGE_ITER iter1;
  GRA_REGION_GBB_ITER          gbb_iter;
  GRA_REGION *region = gra_region_mgr.Complement_Region();

  //
  // choose best looping structure for compile time.  under flag control
  // for the moment.
  //
  if (GRA_use_old_conflict) {
    FOR_ALL_ISA_REGISTER_CLASS( rc ) {

      for (iter0.Init(region,rc); ! iter0.Done(); iter0.Step()) {
	LRANGE* lrange0 = iter0.Current();

	lrange_mgr.Begin_Complement_Interference(lrange0);

        for (iter1.Init(region,rc); ! iter1.Done(); iter1.Step()) {
	  LRANGE* lrange1 = iter1.Current();

	  if ( lrange0->Interferes(lrange1) )
	    lrange_mgr.Complement_Interference(lrange1);
	}

	lrange_mgr.End_Complement_Interference();
      }
    }
  } else {
    MEM_POOL_Push(&MEM_local_nz_pool);
    interferences = GTN_SET_Create(GTN_UNIVERSE_size,&MEM_local_nz_pool);
    FOR_ALL_ISA_REGISTER_CLASS( rc ) {

      for (iter0.Init(region,rc); ! iter0.Done(); iter0.Step()) {
	LRANGE* lrange0 = iter0.Current();
	LRANGE_LIVE_GBB_ITER live_gbb_iter;
	
	GTN_SET_ClearD(interferences);

	for (live_gbb_iter.Init(lrange0); ! live_gbb_iter.Done(); live_gbb_iter.Step()) {
	  GRA_BB *live_gbb = live_gbb_iter.Current();

	  interferences = GTN_SET_UnionD(interferences,
					 live_gbb->Needs_A_Register(),
					 GRA_pool);
	}
	
	lrange_mgr.Begin_Complement_Interference(lrange0);

	for ( tn = GTN_SET_Choose(interferences);
	     tn != GTN_SET_CHOOSE_FAILURE;
	     tn = GTN_SET_Choose_Next(interferences,tn)
	     ) {
	  if ( TN_Is_Allocatable(tn) && TN_register_class(tn) == rc ) {
	    LRANGE* ilrange = lrange_mgr.Get(tn);
	    if ( ilrange->Type() == LRANGE_TYPE_COMPLEMENT ) {
	      if (!Check_Global_Preference(lrange0, ilrange)) {
		lrange_mgr.Complement_Interference(ilrange);
	      }
	    }
	  }
	}

	lrange_mgr.End_Complement_Interference();
      }
    }
    MEM_POOL_Pop(&MEM_local_nz_pool);
  }

  //  Also create interferences for the locals in each block with the globals
  //  that pass through the block.  Since all the locals in the block have
  //  essentially the same interferences with globals, this is a property of
  //  the block not of the lranges that pass through it.
  for (gbb_iter.Init(region); ! gbb_iter.Done(); gbb_iter.Step()) {
    GRA_BB* gbb = gbb_iter.Current();

    gbb->Create_Global_Interferences();
  }

}

/////////////////////////////////////
static void
Add_To_Live_Set( LRANGE_SET** live_lrange_sets, GRA_REGION* region,
                                                LRANGE*     lrange )
/////////////////////////////////////
//
//  Add <lrange> to the appropriate set in <live_lrange_sets> which are
//  LRANGE_SETs over LRANGEs in <region>'s subuniverses.
//
//  Make <lrange> interfere with all the other members of the appropriate set
//  in <live_lrange_sets>.
//
/////////////////////////////////////
{
  LRANGE* lrange1;
  ISA_REGISTER_CLASS  rc  = lrange->Rc();
  LRANGE_SUBUNIVERSE* sub = region->Subuniverse(rc);
  LRANGE_SET*         set = live_lrange_sets[rc];

  if ( lrange->Region() != region )
    return;

  for ( lrange1 = LRANGE_SET_ChooseS(set,sub);
        lrange1 != LRANGE_SET_CHOOSE_FAILURE;
        lrange1 = LRANGE_SET_Choose_NextS(set,lrange1,sub)
  ) {
    lrange->Region_Interference(lrange1,region);
  }

  live_lrange_sets[rc] =
    LRANGE_SET_Union1DS(set,lrange,&MEM_local_nz_pool,sub);
}

/////////////////////////////////////
static void
Remove_From_Live_Set( LRANGE_SET** live_lrange_sets, GRA_REGION* region,
                                                     LRANGE*     lrange )
/////////////////////////////////////
//
//  Remove <lrange> from the appropriate set in <live_lrange_sets> which are
//  LRANGE_SETs over LRANGEs in <region>'s subuniverses.
//
/////////////////////////////////////
{
  ISA_REGISTER_CLASS  rc  = lrange->Rc();
  LRANGE_SUBUNIVERSE* sub = region->Subuniverse(rc);
  LRANGE_SET*         set = live_lrange_sets[rc];

  if ( lrange->Region() != region )
    return;

  if ( ! LRANGE_SET_MemberPS(set,lrange,sub) ) {

    // We have a definition of something not in the live set.  At the very
    // least , this means that we have a dead definition which might be a
    // performance problem.  In any case, we'd better make sure that this
    // LRANGE interferes with any of the live ranges that cross its
    // definition.  I suppose we could decide to give it no register at all if
    // we knew it was going to be elminated later.  But it won't be, so we
    // need to be careful.

    DevWarn("GRA def without use "
            "in previously allocated region: TN%d",
            TN_number(lrange->Tn()));
    Add_To_Live_Set(live_lrange_sets,region,lrange);
  }

  live_lrange_sets[rc] = LRANGE_SET_Difference1DS(set,lrange,sub);
}

/////////////////////////////////////
static void
Make_Fine_Grained_Interferences( GRA_BB* gbb )
/////////////////////////////////////
//
//  Create interferences between the global TNs referenced in <gbb> using the
//  fine grained interference rule.
//
/////////////////////////////////////
{
  ISA_REGISTER_CLASS rc;
  GRA_BB_LIVE_OUT_LRANGE_ITER live_out_iter;
  GRA_BB_OP_BACKWARD_ITER op_iter;
  INT i;
  LRANGE_SET** live_lrange_sets;
  GRA_REGION* region = gbb->Region();

  MEM_POOL_Push(&MEM_local_nz_pool);

  //  Set up a set per register class to keep track of the live LRANGEs as we
  //  step backwards through the OPs in the block.
  //
  live_lrange_sets =
    TYPE_MEM_POOL_ALLOC_N(LRANGE_SET*,&MEM_local_nz_pool,
                                      ISA_REGISTER_CLASS_MAX + 1);

  FOR_ALL_ISA_REGISTER_CLASS( rc ) {
    LRANGE_SUBUNIVERSE* sub = region->Subuniverse(rc);
    live_lrange_sets[rc] =
      LRANGE_SET_Create_Empty(sub->Count(), &MEM_local_nz_pool);
  }

  //  Initially all the live outs that belong to the region are in the live
  //  sets.
  //
  for (live_out_iter.Init(gbb); ! live_out_iter.Done(); live_out_iter.Step()) {
    LRANGE* lrange = live_out_iter.Current();

    if (    lrange->Type() == LRANGE_TYPE_REGION
         && lrange->Region() == region
    ) {
      Add_To_Live_Set(live_lrange_sets,region,lrange);
    }
  }

  //  Step backwards through the ops, removing the result of each op from the
  //  appropriate live set and adding the operands (making interferences to
  //  the members of the live set as we add.)
  //
  for (op_iter.Init(gbb); ! op_iter.Done(); op_iter.Step()) {
    OP* op = op_iter.Current();
    BOOL remove_after = FALSE;

    for ( i = OP_results(op) - 1; i >= 0; --i ) {
      if ( TN_Is_Allocatable(OP_result(op,i)) )
	if (OP_uniq_res(op)) {
	  remove_after = TRUE;
	} else {
	  Remove_From_Live_Set(live_lrange_sets,region,
			       lrange_mgr.Get(OP_result(op,i)));
	}
    }

    for ( i = OP_opnds(op) - 1; i >= 0; --i ) {
      if ( TN_Is_Allocatable(OP_opnd(op,i)) ) {
        Add_To_Live_Set(live_lrange_sets,region,lrange_mgr.Get(OP_opnd(op,i)));
      }
    }

    if (remove_after) {
      for ( i = OP_results(op) - 1; i >= 0; --i ) {
	Remove_From_Live_Set(live_lrange_sets,region,
			     lrange_mgr.Get(OP_result(op,i)));
      }
    }
  }

  MEM_POOL_Pop(&MEM_local_nz_pool);
}


static GRA_REGION* targ_dependent_region; // The region for which target
                                          //  dependent interference is
                                          //  currently being created.

/////////////////////////////////////
static void
Target_Dependent_Region_Interference( void* lrange1_void, void* lrange2_void )
/////////////////////////////////////
//
//  Call back function for target dependent interference interface.
//
/////////////////////////////////////
{
  LRANGE* lrange1 = (LRANGE*) lrange1_void;
  LRANGE* lrange2 = (LRANGE*) lrange2_void;

  DevAssert(lrange1->Type() == LRANGE_TYPE_REGION,("Not a region LRANGE"));
  DevAssert(lrange2->Type() == LRANGE_TYPE_REGION,("Not a region LRANGE"));
  DevAssert(lrange1->Region() == targ_dependent_region,("Wrong region"));
  DevAssert(lrange2->Region() == targ_dependent_region,("Wrong region"));

  if ( lrange1->Rc() == lrange2->Rc() )
    lrange1->Region_Interference(lrange2,targ_dependent_region);
}

/////////////////////////////////////
static void
Make_Target_Dependent_Region_Interferences( GRA_REGION* region )
/////////////////////////////////////
//
//  The current target needs for us to obey some additional interference rules
//  above the classical ones.  See be/cg/cgtarget.h.
//
/////////////////////////////////////
{
  GRA_REGION_GBB_ITER gbb_iter;
  GRA_BB_OP_FORWARD_ITER op_iter;
  INT32 first_bb_cycle;

  targ_dependent_region = region;

  CGTARG_Interference_Initialize(region->Cycle_Count(), region->Is_Loop(),
                                 Target_Dependent_Region_Interference);

  first_bb_cycle = 0;
  for (gbb_iter.Init(region); ! gbb_iter.Done(); gbb_iter.Step()) {
    GRA_BB* gbb = gbb_iter.Current();

    for (op_iter.Init(gbb); ! op_iter.Done(); op_iter.Step()) {
      INT i;
      OP* op = op_iter.Current();

      for ( i = 0; i < OP_results(op); ++i ) {
	TN *rtn = OP_result(op,i);
        if ( TN_Is_Allocatable(rtn) )
	  CGTARG_Result_Live_Range(lrange_mgr.Get(rtn),op,first_bb_cycle);
      }
    }
    first_bb_cycle += OP_scycle(BB_last_op(gbb->Bb())) + 1;
  }

  first_bb_cycle = 0;
  for (gbb_iter.Init(region); ! gbb_iter.Done(); gbb_iter.Step()) {
    GRA_BB* gbb = gbb_iter.Current();

    for (op_iter.Init(gbb); ! op_iter.Done(); op_iter.Step()) {
      INT i;
      OP* op = op_iter.Current();

      for ( i = 0; i < OP_opnds(op); ++i ) {
        TN* otn = OP_opnd(op,i);

        if ( TN_Is_Allocatable(otn) )
          CGTARG_Operand_Live_Range(lrange_mgr.Get(otn),i,op,first_bb_cycle);
      }
    }
    first_bb_cycle += OP_scycle(BB_last_op(gbb->Bb())) + 1;
  }

  CGTARG_Interference_Finalize();
}

/////////////////////////////////////
static void
Build_Region_Interference_Graph( GRA_REGION* region )
/////////////////////////////////////
//
//  Build the fine grained interference graph for the previous allocated
//  <region>.
//
/////////////////////////////////////
{
  GRA_REGION_GBB_ITER gbb_iter;
  GRA_REGION_RC_NL_LRANGE_ITER lrange_iter;
  ISA_REGISTER_CLASS rc;

  // All its LRANGEs need to have their interference initialized first.
  //
  FOR_ALL_ISA_REGISTER_CLASS( rc ) {
    for (lrange_iter.Init(region,rc); ! lrange_iter.Done(); lrange_iter.Step()){
      LRANGE* lrange = lrange_iter.Current();

      lrange->Initialize_Region_Inteference(region);
    }
  }

  //  Now walk each block in the region generating interference:
  //
  for (gbb_iter.Init(region); ! gbb_iter.Done(); gbb_iter.Step()) {
    GRA_BB* gbb = gbb_iter.Current();

    Make_Fine_Grained_Interferences(gbb);
  }

  if ( CGTARG_Interference_Required() )
    Make_Target_Dependent_Region_Interferences(region);
}

/////////////////////////////////////
static void
Create_Interference_Graph(void)
/////////////////////////////////////
//  No interface description.
/////////////////////////////////////
{
  GRA_REGION_PREV_ALLOC_ITER iter;

  GRA_Init_Trace_Memory();
  Build_Complement_Interference_Graph();
  GRA_Trace_Memory("After Build_Complement_Interference_Graph()");

  GRA_Init_Trace_Memory();
  for (iter.Init(); ! iter.Done(); iter.Step()) {
    GRA_REGION* region = iter.Current();

    if ( ! region->Prev_Alloc_By_GRA() )
      Build_Region_Interference_Graph(region);
  }
  GRA_Trace_Memory("After Build_Region_Interference_Graph()");
}

#ifdef TARG_IA64
//=============================================================
//
// Compute the fatest point value of every function,for those
// functions too fat and self recursive,we can make some special
// optimization heuristics.
//                          -- ORC  
//
//============================================================
static void
Compute_GRA_Fat_Point(void) {
    
  ISA_REGISTER_CLASS           rc;
  TN*                          tn;
  //GTN_SET*                     interferences;
  GRA_REGION_RC_NL_LRANGE_ITER iter0;
  GRA_REGION_RC_NL_LRANGE_ITER iter1;
  GRA_REGION_GBB_ITER          gbb_iter;
  GRA_REGION *region = gra_region_mgr.Complement_Region();
  MEM_POOL_Push(&MEM_local_nz_pool);
  //interferences = GTN_SET_Create(GTN_UNIVERSE_size,&MEM_local_nz_pool);
  typedef mempool_allocator<INT>                 INT_ALLOC;
  typedef std::vector<INT,INT_ALLOC>                  INT_VECTOR;
  INT_VECTOR fats(PU_BB_Count+2, (INT32)0,INT_ALLOC(&MEM_local_nz_pool));
  FOR_ALL_ISA_REGISTER_CLASS( rc ) { //Perhaps only int register is needed to be computed.

      for (iter0.Init(region,rc); ! iter0.Done(); iter0.Step()) {
	LRANGE* lrange0 = iter0.Current();
	LRANGE_LIVE_GBB_ITER live_gbb_iter;
	
	//GTN_SET_ClearD(interferences);

	for (live_gbb_iter.Init(lrange0); ! live_gbb_iter.Done(); live_gbb_iter.Step()) {
	  GRA_BB *live_gbb = live_gbb_iter.Current();
          //How to new a int vector here?
	  fats[(live_gbb->Bb())->id] += 1;
	}
      }	
   }
   INT32 fatest_point = 0; 
   INT32 fat_bb_id   = 0;
   for (INT32 i = 0;i < PU_BB_Count;i++) {
       if (fats[i] > fatest_point) {
           fatest_point = fats[i];
           fat_bb_id = i;
       }
   }    
   
   if ((gra_self_recursive)&&(fatest_point > 100)) {
       fat_self_recursive = TRUE;
       DevWarn("The fatest point %d is greater than 80 and also self recursive!\n",fatest_point);
   }    
   MEM_POOL_Pop(&MEM_local_nz_pool);
 
}
#endif // TARG_IA64

#ifdef TARG_SL //minor_reg_alloc
/* this function is used to mark flag for lrange which spans multi regions and 
  * this flags is used to update exclude set for each parallel body in minor mode
  */
void 
Mark_Lrange_For_Minor_Thread()
{
  ISA_REGISTER_CLASS rc;
  GRA_REGION_RC_NL_LRANGE_ITER iter0;
  GRA_REGION_GBB_ITER          gbb_iter;
  GRA_REGION *region = gra_region_mgr.Complement_Region();

  // FOR_ALL_ISA_REGISTER_CLASS( rc ) {
  rc = ISA_REGISTER_CLASS_integer;
  for (iter0.Init(region,rc); ! iter0.Done(); iter0.Step()) {
    LRANGE* lrange0 = iter0.Current();
    LRANGE_LIVE_GBB_ITER live_gbb_iter;
    LRANGE_LUNIT_ITER lunit_iter; 
    /*  for now we think the rid_count greater than 2 means the lrange spans multi-region, 
     *  there only two parallel regions active at same time in minor mode
     */ 
    vector<INT> met_rid;
    for (lunit_iter.Init(lrange0); !lunit_iter.Done(); lunit_iter.Step()) 
    {
      LUNIT* lunit = lunit_iter.Current();
      GRA_BB *live_gbb = lunit->Gbb();
      BB* bb = live_gbb->Bb();
      if(BB_rid(bb) && RID_TYPE_minor(BB_rid(bb))) {  // NULL for func_entry region 
        if(find(met_rid.begin(), met_rid.end(), RID_id(BB_rid(bb))) == met_rid.end()) {
          met_rid.push_back(RID_id(BB_rid(bb)));
        }		
      }
    }
    if(met_rid.size() > 1 && met_rid.size() < 3) 
    {
      lrange0->Spans_Multiregions_Set();
    }
    met_rid.clear();
  }
  //}
}
#endif 

/////////////////////////////////////
void
GRA_Create(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  Create_GRA_BBs_And_Regions();
  Create_LRANGEs();
  Create_Live_BB_Sets();
  Create_LUNITs();
  Create_Interference_Graph();
#ifdef TARG_IA64
  Compute_GRA_Fat_Point();
#endif
#ifdef TARG_SL //minor_reg_alloc
  Mark_Lrange_For_Minor_Thread();
#endif

}


/////////////////////////////////////
void
GRA_Delete(void)
{
}

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

#ifdef TARG_SL2

#include "bb.h"
#include "be_util.h"
#include "cg.h"
#include "lra.h"
#include "targ_isa_registers.h"
#include "cg_spill.h"
#include "tracing.h"
#include "../be/be_isr.h"

static BOOL trace_ipisr_parents; //-Wb,-ttisr:0x01, caller-saved regs used by parents
static BOOL trace_ipisr_child;   //-Wb,-ttisr:0x02, caller-saved regs used by this PU
static BOOL trace_ipisr_spill;   //-Wb,-ttisr:0x04, caller-saved regs spilled by this PU

/* ====================================================================
 * IPISR_Prolog_save_code - generate save code for caller-saved regs
 * ==================================================================== */
static void
IPISR_Prolog_save_code(BB *entrybb)
{
  ISA_REGISTER_CLASS rc;
  REGISTER reg;
  INT32 cur_id = Current_PU_Count();

  if (trace_ipisr_spill) {
    fprintf(TFile, "\tregisters spilled by this PU:\n");
  }

  FOR_ALL_ISA_REGISTER_CLASS(rc) {
    REGISTER_SET need_save = REGISTER_SET_Difference(caller_saved_regs_used[rc],
                                isr_cg[cur_id].Regset(rc));

    if (trace_ipisr_spill && !REGISTER_SET_EmptyP(need_save)) {
      fprintf(TFile, "\t  class %d: ", rc);
      REGISTER_SET_Print(need_save, TFile);
      fprintf(TFile, "\n");
    }

    FOR_ALL_REGISTER_SET_members(need_save, reg) {
      TN* tn = Build_Dedicated_TN(rc, reg, 0);
      ST *mem_loc = CGSPILL_Get_TN_Spill_Location (tn, CGSPILL_LCL);
      OPS ops = OPS_EMPTY;
      // Generate the spill ops
      CGSPILL_Store_To_Memory (tn, mem_loc, &ops, CGSPILL_LRA, entrybb);

      // Allocate registers for any temps used in spill sequence
      Assign_Temp_Regs (&ops, entrybb);

      // Insert the ops in the op list for the current BB
      CGSPILL_Prepend_Ops (entrybb, &ops);
    }

    isr_cg[cur_id].Regset_Union(rc, need_save);
  }

}

/* ====================================================================
 * IPISR_Epilog_restore_code - generate restore code for caller-saved regs
 * ==================================================================== */
static void
IPISR_Epilog_restore_code(BB *entrybb)
{
  ISA_REGISTER_CLASS rc;
  REGISTER reg;
  INT32 cur_id = Current_PU_Count();

  FOR_ALL_ISA_REGISTER_CLASS(rc) {
    REGISTER_SET need_save = REGISTER_SET_Difference(caller_saved_regs_used[rc],
                                isr_cg[cur_id].Regset(rc));
    FOR_ALL_REGISTER_SET_members(need_save, reg) {
      TN* tn = Build_Dedicated_TN(rc, reg, 0);
      ST *mem_loc = CGSPILL_Get_TN_Spill_Location (tn, CGSPILL_LCL);
      OPS ops = OPS_EMPTY;
     // Generate the spill ops
      CGSPILL_Load_From_Memory (tn, mem_loc, &ops, CGSPILL_LRA, entrybb);

      // Allocate registers for any temps used in spill sequence
      Assign_Temp_Regs (&ops, entrybb);

      // Insert the ops in the op list for the current BB
      CGSPILL_Prepend_Ops (entrybb, &ops);
    }
  }
}

/* ====================================================================
 * Insert code at entry and exit to save/restore caller-saved registers
 * that were not saved by the parents in the call graph.
 * ===================================================================*/
static void
IPISR_Spill_Caller_Saved_Regs (void)
{
  BB_LIST *elist;

  for ( elist = Entry_BB_Head; elist; elist = BB_LIST_rest(elist) ) {
    IPISR_Prolog_save_code (BB_LIST_first(elist));
  }

  for ( elist = Exit_BB_Head; elist; elist = BB_LIST_rest(elist) ) {
    IPISR_Epilog_restore_code (BB_LIST_first(elist));
  }
}

extern void
IPISR_Insert_Spills()
{
  if (Get_Trace(TP_IPISR, 0xffffffff))
    fprintf(TFile, "\n<ipisr> start trace for %s\n", ST_name(Get_Current_PU_ST()));

  trace_ipisr_parents = Get_Trace(TP_IPISR, 0x1);
  trace_ipisr_child   = Get_Trace(TP_IPISR, 0x2);
  trace_ipisr_spill   = Get_Trace(TP_IPISR, 0x4);

  // Scan all ops to collect used caller-saved registers
  BB* bb;
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    OP* op;
    INT i;
    FOR_ALL_BB_OPs_FWD (bb, op) {
      TN *tn;
      REGISTER reg;
      ISA_REGISTER_CLASS rc;
      // Scan the results
      for (i = 0; i < OP_results(op); i++) {
        tn = OP_result(op, i);
        if (TN_is_register(tn)) {
          reg = TN_register(tn);
          rc = TN_register_class(tn);
          if (REGISTER_SET_MemberP(REGISTER_CLASS_caller_saves(rc), reg)) {
            caller_saved_regs_used[rc] =
               REGISTER_SET_Union1(caller_saved_regs_used[rc], reg);
          }
        }
      }

      // Scan the operands
      for (i = 0; i < OP_opnds(op); i++) {
        tn = OP_opnd(op, i);
        if (TN_is_register(tn)) {
          reg = TN_register(tn);
          rc = TN_register_class(tn);
          if (REGISTER_SET_MemberP(REGISTER_CLASS_caller_saves(rc), reg)) {
            caller_saved_regs_used[rc] =
               REGISTER_SET_Union1(caller_saved_regs_used[rc], reg);
          }
        }

        if (OP_call(op)) {
          //if (OP_icall(op)) { // Meet an indirect call
          //}

          if (TN_is_symbol(tn)) {
            ST *var = TN_var(tn);
            if (ST_class(var) == CLASS_FUNC
              && ST_sclass(var) == SCLASS_EXTERN) // Meet an external call
            {

            }
          }
        } // if
      } // ~for all opnds

    }
  } // ~for all bb

  if (trace_ipisr_parents) {
    fprintf(TFile, "\tregisters already saved by parents:\n");
    INT32 cur_id = Current_PU_Count();
    ISA_REGISTER_CLASS rc;
    FOR_ALL_ISA_REGISTER_CLASS(rc) {
      if (!REGISTER_SET_EmptyP(isr_cg[cur_id].Regset(rc))) { // for clear output
        fprintf(TFile, "\t  class %d: ", rc);
        REGISTER_SET_Print(isr_cg[cur_id].Regset(rc), TFile);
        fprintf(TFile, "\n");
      }
    }
  }

  if (trace_ipisr_child) {
    fprintf(TFile, "\tregisters used by this PU:\n");
    ISA_REGISTER_CLASS rc;
    FOR_ALL_ISA_REGISTER_CLASS(rc) {
      if (!REGISTER_SET_EmptyP(caller_saved_regs_used[rc])) {
        fprintf(TFile, "\t  class %d: ", rc);
        REGISTER_SET_Print(caller_saved_regs_used[rc], TFile);
        fprintf(TFile, "\n");
      }
    }
  }

  // Spill the caller-saved regs
  IPISR_Spill_Caller_Saved_Regs();
}

#endif

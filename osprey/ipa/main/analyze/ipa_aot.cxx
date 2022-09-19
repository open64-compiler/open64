/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

//-*-c++-*-
// =============================================================================
// =============================================================================
//
// Module: ipa_ptn.cxx
//
// =============================================================================
//
// Description:
//
// Partition for native and uwasm binary
//
// =============================================================================
// =============================================================================

#include "aot_mgr.h"
#include "ipa_cg.h"
#include "ip_graph_trav.h"
#include "timing.h"
#include "ipo_defs.h"
#include <cmplrs/host.h>    // for typedef string
#include "ld_ipa_option.h"  // for read ipa options

template<> void
AOT_COMP_DRIVER::Initialize(IPA_AOT_MGR *mgr)
{
  // hard code the settings
  Set_keep(ld_ipa_opt[LD_IPA_KEEP_TEMPS].flag);
  Set_workdir(tmpdir);
  Set_uwm_driver("xvsa");
  Set_nat_driver("xvsa_nat");
  Set_linker("u_sld");
  Set_output(outfilename);
  Add_nat_file("1.I");
  Add_uwm_file("2.I");
  Add_uwm_option("-m32 -TARG:abi=n32 -c -noxa -TENV:ipa_ident=1607482502 -TENV:read_global_data=symtab.G -Gspace 0  -Wb,-OPT:procedure_reorder=on -pic1  -Wb,-CG:enable_feedback=off -Wb,-CG:direct_acc_mem=1");
  Add_nat_option("-kp -noxa -TENV:ipa_ident=1607482502 -TENV:read_global_data=symtab.G -Gspace 0  -Wb,-OPT:procedure_reorder=on -pic1  -Wb,-CG:enable_feedback=off -fPIC -shared -m32 -openmp");
  Add_symtab_option("-m32 -TARG:abi=n32 -noxa -o symtab.uwm  -TENV:emit_global_data=symtab.G -Gspace 0 -Wb,-OPT:procedure_reorder=on -pic1  -Wb,-CG:enable_feedback=off -TENV:object_name=_out_uwm");
  if (Keep()) {
    Add_uwm_option("-kp");
    Add_nat_option("-kp");
  }
  if (ld_ipa_opt[LD_IPA_SHOW].flag) {
    Add_uwm_option("-sw");
    Add_nat_option("-sw");
  }
  // append uwasm isolation option
  static char uwasm_iso_buf[32];
  snprintf(uwasm_iso_buf, 32, "-TENV:uwasm_iso=%.1d", Uwasm_Isolation);
  Add_nat_option(uwasm_iso_buf);

  FmtAssert(Verify(), ("AOT_COMP_DRIVER: Verify Error"));
}

void
IPA_AOT_EXECUTOR::Mark_native_for_cycle()
{
  // TODO:
}

BOOL
IPA_AOT_EXECUTOR::Do_ptn(IPA_FUNC_VEC *nat_funcs, IPA_FUNC_VEC *uwm_funcs)
{
  // Step 1: Find the cycle nodes in the call grap, if one of the node in 
  // cycle is native, mark all node in the cycle native
  Is_Trace(Get_Trace(TP_AOT, TT_AOT_PTN), (TFile, "AOT_PTN: Propagate native for cycle nodes:\n"));
  Mark_native_for_cycle();

  // Step 2: iterate call graph, if node is native, mark its child as native
  Is_Trace(Get_Trace(TP_AOT, TT_AOT_PTN), (TFile, "AOT_PTN: Propagate native to callee:\n"));
  IPA_NODE_ITER cg_iter(Get_call_graph(), PREORDER);
  for(cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
    IPA_NODE* node = cg_iter.Current();
    if(node) {
      const IP_PROC_INFO* proc_info = IP_FILE_HDR_proc_info (node->File_Header());
      IPA_STATE_TYPE state = IP_PROC_INFO_state(proc_info[node->Proc_Info_Index ()]);
      if(state != IPA_DELETED && state != IPA_WRITTEN) {
        ST *fun_st = node->Func_ST();
        if(ST_is_native(fun_st)) {
          nat_funcs->push_back(node);
          IPA_SUCC_ITER succ_iter(node);
          for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) {
            IPA_NODE *callee = Get_call_graph()->Callee(succ_iter.Current_Edge());
            if (callee && !ST_is_native(callee->Func_ST())) {
              Set_ST_is_native(callee->Func_ST());
              Is_Trace(Get_Trace(TP_AOT, TT_AOT_PTN),
                (TFile, "  Propagate native from %s -> %s\n", Fname(node), Fname(callee)));
            }
          }
        } else {
          uwm_funcs->push_back(node);
        }
      }
    }
  }
}

void
IPA_AOT_EXECUTOR::Adjust_preg_num(IPA_NODE *node)
{
  IPA_NODE_CONTEXT context (node);
  WN *pu_tree = node->Whirl_Tree();
  Adjust_tree_preg_num(pu_tree);
}

BOOL
IPA_AOT_EXECUTOR::Do_flush()
{
  IP_flush_output();
  return TRUE;
}

const char* 
IPA_AOT_EXECUTOR::Fname(IPA_NODE *node)
{
  return ST_name(node->Func_ST());
}

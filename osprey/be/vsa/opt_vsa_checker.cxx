//-*-c++-*-

/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

// =============================================================================
// =============================================================================
//
// Module: opt_vsa_checker.cxx
//
// =============================================================================
//
// Description:
//
// implementations for functions declared in opt_vsa_checker.h
//
// =============================================================================
// =============================================================================


#include "defs.h"
#include "errors.h"
#include "tracing.h"    /* for TFile */

#include "opt_defs.h"
#include "opt_config.h"
#include "opt_cfg.h"
#include "opt_bb.h"
#include "opt_vsa.h"
#include "opt_dna.h"
#include "opt_vsa_util.h"
#include "opt_vsa_checker.h"
#include "class_hierarchy.h"
#include "opt_vsa_rbc.h"

// =============================================================================
// VPTR_TRACKER::Can_function_taken(CODEREP *cr, const char *cr_cls,
//                                  const char *cls, const char *func)
//  Check if func can be taken with given qthis' in cr
// =============================================================================
BOOL
VPTR_TRACKER::Can_function_taken(CODEREP *cr, const char *cr_cls, const char *cls, const char *func)
{
  Is_Trace(Tracing(),
           (TFile, "VPTR: CHECK %s: --> %s\n", cr->Print_str(TRUE), func));
  Is_Trace(Tracing(),
           (TFile, "    : ----VPTR STACK DUMP----\n"));
  Is_Trace_cmd(Tracing(), Dump_top(TFile));
  Is_Trace(Tracing(),
           (TFile, "    : ====VPTR STACK DUMP END====\n"));
  const char *top_cls = cr_cls;
  if (top_cls == NULL) {
    if (Stack().empty()) {
      Is_Trace(Tracing(),
               (TFile, "    : empty stack. YES.\n"));
      return TRUE;
    }
    const VPTR_FRAME& frame = Top();
    top_cls = frame.Find_class(cr);
    if (top_cls == NULL) {
      Is_Trace(Tracing(),
              (TFile, "    : not find top class. YES.\n"));
      return TRUE;
    }
  }
  CLASS_INFO *cls_info = _ch->Get_class_info(cls);
  Is_True_Ret(cls_info, ("fail to get class info for %s", cls), TRUE);
  INT32 ofst = cls_info->Get_vtable_ofst(func);
  if (ofst == INVALID_VTABLE_OFFSET) {
    Is_Trace(Tracing(),
             (TFile, "    : not find ofst for %s in %s. YES.\n", func, cls));
    return TRUE;
  }
  if (ofst == cls_info->Get_vtable_ofst(func)) {
    Is_Trace(Tracing(),
             (TFile, "    : find same method in %s ofst %d. YES\n", top_cls, ofst));
    return TRUE;
  }
  return FALSE;
}

void
VPTR_TRACKER::Push(DNA_NODE *dna, CODEREP *cr, const char *cls_name)
{
  Is_Trace(Tracing(),
           (TFile, "VPTR: PUSH %s: %s --> %s\n",
                   cr->Print_str(TRUE), dna->Fname(), cls_name));
  _vptr_stack.push_back(VPTR_FRAME(dna, cr, cls_name));
}

void
VPTR_TRACKER::Pop(DNA_NODE *dna)
{
  Is_Trace(Tracing(),
           (TFile, "VPTR: POP %s\n", dna->Fname()));
  Is_True(!_vptr_stack.empty(), ("vptr stack is empty"));
  Is_True(_vptr_stack.back().Dna() == dna, ("dna mismatch"));
  _vptr_stack.pop_back();
}

// =============================================================================
// VPTR_TRACKER::VPTR_INFO::Dump()
// VPTR_TRACKER::VPTR_FRAME::Dump()
// VPTR_TRACKER::Dump()
// =============================================================================
void
VPTR_TRACKER::VPTR_INFO::Dump(FILE *fp) const
{
  fprintf(fp, "  * %s --> %s\n",
              Cr()->Print_str(TRUE), Cls_name());
}

void
VPTR_TRACKER::VPTR_FRAME::Dump(FILE *fp) const
{
  fprintf(fp, " %s\n", Dna()->Fname());
  INFO_ARRAY::const_iterator end = Array().end();
  for (INFO_ARRAY::const_iterator it = Array().begin();
       it != end; ++it) {
    it->Dump(fp);
  }
}

void
VPTR_TRACKER::Dump_top(FILE *fp) const
{
  if (!Stack().empty())
    Stack().back().Dump(fp);
}

void
VPTR_TRACKER::Dump(FILE *fp) const
{
  VPTR_STACK::const_reverse_iterator end = Stack().rend();
  for (VPTR_STACK::const_reverse_iterator it = Stack().rbegin();
       it != end; ++it) {
    it->Dump(fp);
  }
}

// =============================================================================
// CONTAINER_UD_HELPER
// CONTAINER_UD_HELPER::Eval_container_get
// =============================================================================
DEF_OBJS *
CONTAINER_UD_HELPER::Eval_container_get(RNA_NODE *rna, CHECK_OBJ &obj,
                                        TRAV_CONTEXT *ctx, MEM_POOL *pool)
{
  DEF_OBJS *value_objs = NULL;
  RBC_OP ref_ops[] = RBC_OP_CONTAINER_GET_REF;
  BOOL is_get_ref = rna->Is_container_op() &&
                    ctx->Ipsa()->Rna_has_rbc_ops(rna, ref_ops,
                                                 RBC_OPS_CNT(ref_ops));
  if (obj.Is_var() && is_get_ref)
    return value_objs;

  if (obj.Is_vsym()) {
    // TODO: compress vsym
    if (is_get_ref) {
      ctx->Tracker()->Pop();  // pop 1 level vsym for get_ref
    }
#if 0
    VSYM_OBJ_REP *vor = obj.Vor();
    CODEREP *cr = vor->Vsym_obj()->Base_hor()->Heap_obj()->Ho_cr();
    ctx->Tracker()->Pop();
    while (cr->Kind() == CK_IVAR) {
      VSYM_OBJ_REP *tmp = ctx->Vsa()->Cr_2_vor(cr);
      if (tmp == NULL) {
        return EVAL_RET_INVALID;
      }
      ctx->Tracker()->Push(tmp->Vsym_obj()->Fld_rep_ptr());
      cr = Find_ilod_base(cr->Ilod_base());
    }
  // TODO end
#endif
    // get from vor map first, if not null return the value objs directly
    VSYM_OBJ_REP *vor = obj.Vor();
    value_objs = ctx->Vsa()->Vor_2_value_objs(vor);
    if (value_objs) {
      return value_objs;
    }
  }

  RBC_BASE *rbc = ctx->Vsa()->Rbc();
  EVAL_RET ret = rbc->Eval__mvsa_container_op(ctx->Dna(), rna, pool, NULL, FALSE);
  if (ret.first == LIST_PTR || ret.first == MAP_PTR) {
    value_objs = (DEF_OBJS *)ret.second;
  }
  return value_objs;
}


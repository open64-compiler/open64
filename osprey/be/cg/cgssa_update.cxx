// ====================================================================
//
// Copyright (C) 2011, Hewlett-Packard Development Company, L.P.
// All Rights Reserved.
//
// Open64 is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// Open64 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
// MA  02110-1301, USA.
//
// ====================================================================
//
// Module: cgssa_update.cxx
//
// Description:
//   Implementation for CGSSA_UPDATER
//   
// ====================================================================

#include <stdarg.h>
#include "cgssa_update.h"

using namespace CFG_UTIL;
using namespace CGSSA_NAME;

VER_ID
CGSSA_UPDATER::SSA_Gen_Register_TN(ISA_REGISTER_CLASS rclass, INT size) {
  TN* new_tn = Gen_Register_TN(rclass, size);
  return Ssa()->SSA_Gen_TN(new_tn);

}

VER_ID
CGSSA_UPDATER::SSA_Gen_Typed_Register_TN(TYPE_ID mtype, INT size) {
  TN* new_tn = Gen_Typed_Register_TN(mtype, size);
  return Ssa()->SSA_Gen_TN(new_tn);
}

VER_ID
CGSSA_UPDATER::SSA_Gen_Unique_Literal_TN(INT64 ivalue, INT size) {
  TN* new_tn = Gen_Unique_Literal_TN(ivalue, size);
  return Ssa()->SSA_Gen_TN(new_tn);
}

VER_ID
CGSSA_UPDATER::SSA_Gen_Symbol_TN(ST *st, INT64 offset, INT32 relocs) {
  TN* new_tn = Gen_Symbol_TN(st, offset, relocs);
  return Ssa()->SSA_Gen_TN(new_tn);
}

void
CGSSA_UPDATER::SSA_Set_OP_opnd(OP* op, INT opndnum, VER_ID new_ver_id) {

  Ssa()->SSA_Replace_OP_opnd(op, opndnum, new_ver_id);

  // real IR change
  Set_OP_opnd(op, opndnum, Ssa()->Get_Version_TN(new_ver_id));
}

void
CGSSA_UPDATER::SSA_OP_Change_To_Noop(OP * op) {

  // update the SSA information
  for (int i = 0; i < OP_opnds(op); ++i) {
    Ssa()->SSA_Remove_OP_opnd(op, i);
  }
  for (int i =0 ; i < OP_results(op); ++i) {
    Ssa()->SSA_Remove_OP_result(op, i);
  }

  Ssa()->Delete_Ref_ID_map((INTPTR) op);

  // IR change
  OP_Change_To_Noop(op);
}

// SSA_Mk_OP(opr, def_ver,... opnd_ver,...);
OP*
CGSSA_UPDATER::SSA_Mk_OP(TOP opr, ...) {

  va_list ap;
  OP * op;
  INT results, opnds, i;
  TN** results_tn;
  TN** opnds_tn;
  VER_ID* results_ver;
  VER_ID* opnds_ver;

  results = TOP_fixed_results(opr);
  opnds = TOP_fixed_opnds(opr);

  results_tn = (TN **) malloc(results * sizeof(TN *));
  results_ver = (VER_ID *) malloc(results * sizeof(VER_ID));
  opnds_tn = (TN **) malloc(opnds * sizeof(TN *));
  opnds_ver = (VER_ID *) malloc(opnds* sizeof(VER_ID));

  va_start(ap, opr);
  for (i = 0; i< results; ++i) {
    VER_ID result_ver = va_arg(ap, VER_ID);
    results_ver[i] = result_ver;
    results_tn[i] = Ssa()->Get_Version_TN(result_ver);
  }

  for (i = 0; i< opnds; ++i) {
    VER_ID opnd_ver = va_arg(ap, VER_ID);
    opnds_ver[i] = opnd_ver;
    opnds_tn[i] = Ssa()->Get_Version_TN(opnd_ver);
    
  }
  va_end(ap);

  // call the Mk_VarOP
  op = Mk_VarOP(opr, results, opnds, results_tn, opnds_tn);

  // update the SSA information 
  for (i = 0; i < results; ++i) {
    Ssa()->SSA_New_OP_result(op, i, results_ver[i]);
  }
  for (i = 0; i < opnds; ++i) {
    Ssa()->SSA_New_OP_opnd(op, i, opnds_ver[i]);
  }

  return op;
}

// replace the old_op with the new_op
void
CGSSA_UPDATER::SSA_Replace_OP(OP * old_op, OP* new_op) {

  Cfg()->Insert_after(old_op, new_op);
  SSA_OP_Change_To_Noop(old_op);
}

void
CGSSA_UPDATER::Delete_Phi(PHI_NODE* phi, CGSSA_DEL_USES del_uses) {
  if (del_uses==CGSSA_DELETE_USES)
    Ssa()->Delete_Uses(phi);

  BB_NODE* node = Cfg()->Get_BB(phi->BB());
  Ssa()->Delete_a_Phi_from_BB(node->Get_id(), phi);

  Ssa()->Delete_Result_Ref_ID(phi);
  VER_ID ver_id = Ssa()->Result_Ver(phi);
  Ssa()->Delete_Ver(ver_id);

  Ssa()->Delete_Ref_ID_map((INTPTR)phi);
}

void
CGSSA_UPDATER::Delete_OP(OP* op, CGSSA_DEL_USES del_uses) {
  if (del_uses==CGSSA_DELETE_USES)
    Ssa()->Delete_Uses(op);

  // delete results
  for (int i = 0; i < OP_results(op); ++i ) {
    VER_ID ver_id = Ssa()->Result_Ver(op, i);
    Ssa()->Delete_Ver(ver_id);
    Ssa()->Delete_Result_Ref_ID(op, i);
  }

  Cfg()->Remove_stmt(op); // BB_Remove_Op is called from here
  Ssa()->Delete_Ref_ID_map((INTPTR) op);
}

void
CGSSA_UPDATER::Delete_Stmt(VER_ID ver_id) {
  VERSION_KIND ver_kind = Ssa()->Def_kind(ver_id);
  Is_True(ver_kind != VERSION_UNKNOWN && ver_kind != VERSION_CHI_DEF,
    ("Invalid version kind"));

  if (ver_kind == VERSION_PHI_DEF) {
    PHI_NODE* phi = Ssa()->Get_Phi_Def(ver_id);
    // Dead PHIs are not deleted to help the 'leave SSA' phase to detect
    // live range overlaps.
//     Delete_Phi(phi);
    return;
  }

  // ver_kind should be VERSION_OCC_DEF, here.
  OP* op = Ssa()->Get_Occ_Def(ver_id);
  Delete_OP(op);
}


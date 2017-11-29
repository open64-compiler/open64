/*

  Copyright (C) 2010, Hewlett-Packard Development Company, L.P. All Rights Reserved.

  Open64 is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  Open64 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
  MA  02110-1301, USA.

*/

//====================================================================
//
// Module: wssa_wn.cxx
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Implementation for WN related routines in WHIRL SSA
//
// SEE ALSO:
//  be/com/wssa_wn.h
//
//====================================================================

#include "wssa_wn.h"
#include "wn.h"

namespace WSSA {

//===================================================================
// WSSA WN utilities
//   WN_has_node: return TRUE if the oper can have the kind of nodes
//   WN_has_phi: return TRUE if the oper can have phi node
//   WN_has_chi: return TRUE if the oper can have chi node
//   WN_has_ver: return TRUE if the st for the oper can have the version
//   WN_use_ver: return TRUE if the oper is LDID or LDBITS
//   WN_def_ver: return TRUE if the oper is STID or STBITS
//===================================================================

BOOL
WN_has_node(const WN* wn, WSSA_NODE_KIND nkind) {
  switch (nkind) {
    case WSSA_PHI:
      return WN_has_phi(wn);
    case WSSA_CHI:
      return WN_has_chi(wn);
    case WSSA_MU:
      return WN_has_mu (wn);
    case WSSA_OCC:
      return WN_has_ver(wn);
    default:
      Is_True(FALSE, ("Invalid nkind"));
      return FALSE;
  }
}

BOOL
WN_has_phi(const WN* wn) {
  OPERATOR opr = WN_operator(wn);
  switch (opr) {
    case OPR_IF:
    case OPR_DO_LOOP:
    case OPR_WHILE_DO:
    case OPR_DO_WHILE:
    case OPR_LABEL:
      return TRUE;
    default:
      return FALSE;
  }
}

BOOL
WN_has_chi(const WN* wn) {
  OPERATOR opr = WN_operator(wn);
  switch (opr) {
    case OPR_FUNC_ENTRY:  // entry chi
    case OPR_ALTENTRY:    // entry chi
    case OPR_ISTORE:
    case OPR_ISTOREX:
    case OPR_ISTBITS:
    case OPR_MSTORE:
    case OPR_STID:
    case OPR_STBITS:
    case OPR_CALL:
    case OPR_ICALL:
    case OPR_INTRINSIC_CALL:
    case OPR_IO:
    case OPR_FORWARD_BARRIER:
    case OPR_BACKWARD_BARRIER:
    case OPR_DEALLOCA:
    case OPR_OPT_CHI:  // entry chi
    case OPR_REGION:   // black-box region
    case OPR_ASM_STMT:
      return TRUE;
    case OPR_LABEL:
      if (WN_Label_Is_Handler_Begin(wn))  // entry chi
        return TRUE;
      else
        return FALSE;
    default:
      return FALSE;
  }
}

BOOL
WN_has_mu(const WN* wn) {
  OPERATOR opr = WN_operator(wn);
  switch (opr) {
    case OPR_ASM_STMT:
    case OPR_ILOAD:
    case OPR_ILDBITS:
    case OPR_ILOADX:
    case OPR_MLOAD:
    case OPR_CALL:
    case OPR_ICALL:
    case OPR_INTRINSIC_CALL:
    case OPR_IO:
    case OPR_RETURN:
    case OPR_RETURN_VAL:
    case OPR_FORWARD_BARRIER:
    case OPR_BACKWARD_BARRIER:
    case OPR_REGION: // black-box region
    case OPR_REGION_EXIT:
    case OPR_PARM:
      return TRUE;
    default:
      return FALSE;
  }
}

BOOL
WN_has_ver(const WN* wn) {
  OPERATOR opr = WN_operator(wn);
  switch (opr) {
    case OPR_LDID:
    case OPR_LDBITS:
    case OPR_STID:
    case OPR_STBITS:
      return TRUE;
    default:
      return FALSE;
  }
}

BOOL
WN_def_ver(const WN* wn) {
  OPERATOR opr = WN_operator(wn);
  switch (opr) {
    case OPR_STID:
    case OPR_STBITS:
      return TRUE;
    default:
      return FALSE;
  }
}

BOOL
WN_use_ver(const WN* wn) {
  OPERATOR opr = WN_operator(wn);
  switch (opr) {
    case OPR_LDID:
    case OPR_LDBITS:
      return TRUE;
    default:
      return FALSE;
  }
}

BOOL
WN_is_volatile(const WN* wn) {
  if (WN_ty(wn) != 0 && TY_is_volatile(WN_ty(wn)))
    return TRUE;
  else if (WN_has_sym(wn) &&
           WN_st(wn) != NULL &&
           ST_class(WN_st(wn)) == CLASS_VAR &&
           TY_is_volatile(ST_type(WN_st(wn))))
    return TRUE;
  else
    return FALSE;
}

BOOL
Tree_is_volatile(const WN* tree) {
  OPCODE opc = WN_opcode(tree);
  Is_True(OPCODE_is_stmt(opc) || OPCODE_is_expression(opc),
          ("opcode %s is not stmt or expr", OPCODE_name(opc)));

  if (WN_is_volatile(tree)) {
    return TRUE;
  }
  for (int i=0; i < WN_kid_count(tree); ++i) {
    if (Tree_is_volatile(WN_kid(tree, i)))
      return TRUE;
  }
  return FALSE;
}

} /* namespace WSSA */


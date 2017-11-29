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

//=====================================================================
//
// Module: wssa_du.cxx
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Implementation of DU manager based on WHIRL SSA
//
// Exported classes:
//  WSSA::WSSA_DU_MANAGER
//
// SEE ALSO:
//  be/com/wssa_du.h
//
//====================================================================

#include "wssa_du.h"
#include "wssa_core.h"
#include "wssa_mgr.h"
#include "wssa_utils.h"
#include "wssa_wn.h"
#include "wn.h"
#include "symtab.h"
#include "pu_info.h"

namespace WSSA {

//====================================================================
// USE_NODE
//====================================================================

void
USE_NODE::Print(FILE* fp, INT indent) const {
  Put_indent(fp, indent);
  fprintf(fp, "used by %s:%p\n", WSSA_node_name(_use_type), _use_wn);
}

void
USE_NODE::Verify() const {
}

//====================================================================
// DU MANAGER
//====================================================================

WSSA_DU_MANAGER::WSSA_DU_MANAGER(WHIRL_SSA_MANAGER* mgr, const WN* root)
  : _wssa_mgr(mgr), _root(root) {
}

void
WSSA_DU_MANAGER::Build() {
  Is_True(_root != NULL, ("root wn is NULL"));
  Is_True(_wssa_mgr != NULL && _wssa_mgr->Is_stat_OK(), ("WHIRL SSA info is unavailable"));
  Build_rec(_root);
}

void
WSSA_DU_MANAGER::Build_rec(const WN* wn) {
  Is_True(wn != NULL, ("Null wn"));
  Is_True(_wssa_mgr != NULL && _wssa_mgr->Is_stat_OK(), ("WHIRL SSA info is unavailable"));

  OPERATOR opr = WN_operator(wn);
  // ignore LOOP_INFO
  if (opr == OPR_LOOP_INFO) {
    return;
  }

  if (_wssa_mgr->WN_has_phi(wn)) {
    for (WHIRL_SSA_MANAGER::const_phi_iterator it = _wssa_mgr->WN_phi_begin(wn);
         it != _wssa_mgr->WN_phi_end(wn);
         ++it) {
      for (int i=0; i<it->Opnd_count(); i++) {
        Add_use(it->Get_opnd(i), WSSA_PHI, wn);
      }
    }
  }

  if (_wssa_mgr->WN_has_chi(wn)) {
    for (WHIRL_SSA_MANAGER::const_chi_iterator it = _wssa_mgr->WN_chi_begin(wn);
         it != _wssa_mgr->WN_chi_end(wn);
         ++it) {
      Add_use(it->Get_opnd(0), WSSA_CHI, wn);
    }
  }

  if (_wssa_mgr->WN_has_mu(wn)) {
    for (WHIRL_SSA_MANAGER::const_mu_iterator it = _wssa_mgr->WN_mu_begin(wn);
         it != _wssa_mgr->WN_mu_end(wn);
         ++it) {
      Add_use(it->Get_opnd(0), WSSA_MU, wn);
    }
  }

  switch(opr) {
    case OPR_BLOCK:
      for (WN* item = WN_first(wn); item != NULL; item = WN_next(item)) {
        Build_rec(item);
      }
      break;
    case OPR_REGION:
      Build_rec(WN_region_body(wn));
      break;

    case OPR_LDID:
    case OPR_LDBITS:
      Add_use(_wssa_mgr->Get_wn_ver(wn), WSSA_OCC, wn);
      break;
    default:
      for (int i=0; i<WN_kid_count(wn); i++) {
        Build_rec(WN_kid(wn, i));
      }
  }
}


//====================================================================
// iterators to traverse the D-U chain
//====================================================================
WSSA_DU_ITERATOR
WSSA_DU_MANAGER::WN_du_begin(WN* wn) const {
  return WSSA_DU_ITERATOR(this, wn);
}

WSSA_DU_ITERATOR
WSSA_DU_MANAGER::WN_du_end(WN* wn) const {
  return WSSA_DU_ITERATOR(this, NULL);
}

//====================================================================
// Print and verify methods
//====================================================================
void
WSSA_DU_MANAGER::Print(FILE* fp) const {
  fprintf(fp, "DU chain dump:");
  for (USE_MAP::const_iterator vit = _use_map.begin();
       vit != _use_map.end();
       ++vit) {
    VER_IDX ver = (VER_IDX)vit->first;
    fprintf(fp, " Ver:%d, first_use:%d\n", ver, vit->second);
    for (const_iterator uit = begin(ver);
         uit != end(ver);
         ++uit) {
      uit->Print(fp, 2);
    }
  }
}

void
WSSA_DU_MANAGER::Verify() const {
}

} /* namespace WSSA */


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
// Module: wssa_update.h
//
// Revision history:
//  Dec-6 - Original Version
//
// Description:
//  Interface for WHIRL SSA UPDATER
//
// Exported classes:
//  WSSA::WSSA_UPDATER
//
// SEE ALSO:
//  be/com/wssa_mgr.h  (WHIRL_SSA_MANAGER)
//  be/com/wn_cfg.h    (WHIRL CFG)
//
//====================================================================

#ifndef wssa_update_INCLUDED
#define wssa_update_INCLUDED

#include "defs.h"
#include "wssa_defs.h"
#include "wn_cfg.h"
#include "wssa_mgr.h"
#include "wssa_du.h"
#include <ext/hash_map>
using __gnu_cxx::hash_map;

namespace WSSA {

typedef hash_map<UINT32 /* WST_IDX */, 
                 pair<VER_IDX /* old ver */, VER_IDX /* new ver */> > WST_DEF_MAP;

//===================================================================
// WSSA_UPDATER
//   interface to update WHIRL SSA
//===================================================================
class WSSA_UPDATER {
public:
  typedef CFG_UTIL::WN_CFG::BB_NODE BB_NODE;

private:
  BOOL _trace;
  BOOL _update_cfg;
  BOOL _update_ssa;
  BOOL _update_du;
  WN* _root;
  CFG_UTIL::WN_CFG* _cfg;
  WHIRL_SSA_MANAGER* _ssa;
  WSSA_DU_MANAGER*   _du;

public:
  WSSA_UPDATER(WN* root, CFG_UTIL::WN_CFG* cfg, 
               WHIRL_SSA_MANAGER* ssa, BOOL trace = FALSE)
    : _root(root), _ssa(ssa), _cfg(cfg), _trace(trace),
      _update_cfg(FALSE), _update_ssa(FALSE), _update_du(FALSE) {
    _du = NULL;  // TODO: update DU
  } 

public:
  void Enable_update_cfg()  { _update_cfg = TRUE;  }
  void Disable_update_cfg() { _update_cfg = FALSE; }
  void Enable_update_ssa()  { _update_ssa = TRUE;  }
  void Disable_update_ssa() { _update_ssa = FALSE; }
  void Enable_update_du()   { _update_du  = TRUE;  }
  void Disable_update_du()  { _update_du  = FALSE; }

public:
  // adjust phi operands order according to CFG
  // only needed for WOPT emitter since the CFG and SSA are built separately
  void Adjust_phi_opnds();
  void Rename_all_preg();
  void Rename_all_wst();

private:
  BOOL wn_is_branch(WN* stmt);
  BOOL wn_change_cfg(WN* stmt);
  BOOL wn_change_ssa(WN* stmt);

private:
  // manage the WST_DEF_MAP
  void update_def_map(WST_DEF_MAP& def, VER_IDX new_ver);
  VER_IDX get_last_def(WST_DEF_MAP& def, WST_IDX wst_idx);
  VER_IDX get_prev_def(WST_DEF_MAP& def, WST_IDX wst_idx);

  // rename all the versions in the def info
  void ssa_rename_mu(WN* wn, WST_DEF_MAP& def);
  void ssa_rename_chi(WN* wn, WST_DEF_MAP& def);
  void ssa_rename_rhs(WN* rhs, WST_DEF_MAP& def);
  void ssa_rename_stmt(WN* stmt, WST_DEF_MAP& def);
  // rename the old versions to the new versions
  BOOL ssa_rename_tree(WN* wn, VER_IDX old_ver, VER_IDX new_ver);
  BOOL ssa_rename_cur_bb(BB_NODE* bb, WN* start, VER_IDX old_ver, VER_IDX new_ver);
  void ssa_rename_dom_bb(BB_NODE* bb, VER_IDX old_ver, VER_IDX new_ver);
  void ssa_rename_wst(BB_NODE* bb, WN* start, VER_IDX old_ver, VER_IDX new_ver);
  // add or remove statement
  void ssa_add_stmt(WN* stmt);
  void ssa_remove_stmt(WN* stmt);

  // internal methods to update DU
  void du_add_stmt(WN* stmt);
  void du_remove_stmt(WN* stmt);

  // internal methods to update CFG
  BB_NODE* cfg_find_succ(WN* stmt);
  void cfg_insert_before(WN* before, WN* stmt);
  void cfg_insert_after(WN* after, WN* stmt);
  void cfg_remove_stmt(WN* stmt);

public:
  // copy tree and ssa information
  WN* Copy_tree(WN* tree);
  WN* Copy_tree_with_map(WN* tree);

  // insert statement and update whirl, cfg and ssa
  void Insert_before(WN* block, WN* before, WN* stmt);
  void Insert_after(WN* block, WN* after, WN* stmt);

  // extract statement(s) from block, update whirl, cfg and ssa
  WN*  Extract_stmt(WN* block, WN* stmt);
  WN*  Extract_list(WN* block, WN* first, WN* last);

  // delete statement(s) from block, update whirl, cfg and ssa
  void Delete_stmt(WN* block, WN* stmt);
  void Delete_list(WN* block, WN* first, WN* last);

public:
  // rename inside bb and keep the versions of live out the same
  void Rename_BB(BB_NODE* bb);
  void Rename_BB_new_preg(BB_NODE* bb);

  // delete all statements in bb and bb itself
  void Delete_BB(BB_NODE* bb);
};

}; /* namespace WSSA */

#endif /* wssa_update_INCLUDED */


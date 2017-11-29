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
// Module: wssa_utils.h
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Implementation for utilities used by dump/verify in WHIRL SSA
//
// Exported functions:
//  WSSA::Get_wst_name
//  WSSA::Get_ver_wst
//  WSSA::Get_ver_num
//
// SEE ALSO:
//  be/com/wssa_mgr.h (WHIRL_SSA_MANAGER)
//
//====================================================================

#include "wssa_utils.h"
#include "wssa_mgr.h"
#include "pu_info.h"
#include "ir_reader.h"

namespace WSSA {

void
Put_indent(FILE* fp, int indent) {
  fprintf(fp, "%*s", indent, "");
}

void
Print_wst(FILE* fp, WST_IDX idx) {
  const WHIRL_SSA_MANAGER* mgr = PU_Info_ssa_ptr(Current_PU_Info);
  const WST_Symbol_Entry& sym = mgr->Get_wst(idx);
  if (sym.Sym_type() == WST_PREG)
    fprintf(fp, "%s:%d", mgr->WST_name(idx), sym.Preg_num());
  else
    fprintf(fp, "%s", mgr->WST_name(idx));
}

void
Print_ver(FILE* fp, VER_IDX idx) {
  const WHIRL_SSA_MANAGER* mgr = PU_Info_ssa_ptr(Current_PU_Info);
  WST_IDX wst_idx = mgr->Get_ver_wst(idx);
  UINT32 ver_num = mgr->Get_ver_num(idx);
  const WST_Symbol_Entry& sym = mgr->Get_wst(wst_idx);
  if (sym.Sym_type() == WST_PREG)
    fprintf(fp, "%s:%dv%d", 
                mgr->WST_name(wst_idx), sym.Preg_num(), ver_num);
  else
    fprintf(fp, "%sv%d", mgr->WST_name(wst_idx), ver_num);
}

const char* Get_wst_name(WST_IDX idx) {
  const WHIRL_SSA_MANAGER* mgr = PU_Info_ssa_ptr(Current_PU_Info);
  return mgr->WST_name(idx);
}

WST_IDX Get_ver_wst(VER_IDX ver) {
  const WHIRL_SSA_MANAGER* mgr = PU_Info_ssa_ptr(Current_PU_Info);
  return mgr->Get_ver_wst(ver);
}

UINT32 Get_ver_num(VER_IDX ver) {
  const WHIRL_SSA_MANAGER* mgr = PU_Info_ssa_ptr(Current_PU_Info);
  return mgr->Get_ver_num(ver);
}

UINT32 Get_wst_max_ver(WST_IDX idx) {
  const WHIRL_SSA_MANAGER* mgr = PU_Info_ssa_ptr(Current_PU_Info);
  return mgr->Get_max_ver(idx);
}

void dump_wn_ssa(WN* wn) {
  WHIRL_SSA_MANAGER* mgr = PU_Info_ssa_ptr(Current_PU_Info);
  MGR_STAT mgr_stat;
  if (mgr) {
    mgr_stat = mgr->Stat();
    if (mgr_stat != STAT_OK) {
      fprintf(stdout, "Warning: WHIRL SSA information is unavailable\n");
    }
    else {
      mgr->Set_stat(STAT_DUMP); // dump SSA info
    }
  }

  dump_wn(wn);

  if (mgr)
    mgr->Set_stat(mgr_stat);
}


void dump_tree_ssa(WN* wn) {
  WHIRL_SSA_MANAGER* mgr = PU_Info_ssa_ptr(Current_PU_Info);
  MGR_STAT mgr_stat;
  if (mgr) {
    mgr_stat = mgr->Stat();
    if (mgr_stat != STAT_OK) {
      fprintf(stdout, "Warning: WHIRL SSA information may not be correct\n");
    }
    mgr->Set_stat(STAT_DUMP); // dump SSA info
  }

  dump_tree(wn);

  if (mgr)
    mgr->Set_stat(mgr_stat);
}

void fdump_wn_ssa(FILE* f, WN* wn) {
  WHIRL_SSA_MANAGER* mgr = PU_Info_ssa_ptr(Current_PU_Info);
  MGR_STAT mgr_stat;
  if (mgr) {
    mgr_stat = mgr->Stat();
    if (mgr_stat != STAT_OK) {
      fprintf(f, "Warning: WHIRL SSA information may not be correct\n");
    }
    mgr->Set_stat(STAT_DUMP); // dump SSA info
  }

  fdump_wn(f, wn);

  if (mgr)
    mgr->Set_stat(mgr_stat);
}

void fdump_tree_ssa(FILE* f, WN* wn) {
  WHIRL_SSA_MANAGER* mgr = PU_Info_ssa_ptr(Current_PU_Info);
  MGR_STAT mgr_stat;
  if (mgr) {
    mgr_stat = mgr->Stat();
    if (mgr_stat != STAT_OK) {
      fprintf(f, "Warning: WHIRL SSA information is unavailable\n");
    }
    else {
      mgr->Set_stat(STAT_DUMP); // dump SSA info
    }
  }

  fdump_tree(f, wn);

  if (mgr)
    mgr->Set_stat(mgr_stat);
}

} /* namespace WSSA */


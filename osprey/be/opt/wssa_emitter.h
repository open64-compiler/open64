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
// Module: wssa_emitter.h
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Emit WHIRL SSA from HSSA
//
//====================================================================

#ifndef wssa_emitter_INCLUDED
#define wssa_emitter_INCLUDED

#include "wssa_mgr.h"
#include "wn.h"
#include "symtab.h"
#include "opt_bb.h"
#include "opt_ssa.h"
#include "opt_emit.h"
#include <ext/hash_map>
using __gnu_cxx::hash_map;

extern BOOL OPT_Enable_WHIRL_SSA;
class CHI_NODE;
class MU_NODE;

class WHIRL_SSA_EMITTER {
private:
  WSSA::WHIRL_SSA_MANAGER* _wssa_mgr;      
  WN_MAP                   _wn_cr_map;
  OPT_STAB*                _opt_stab;
  WN*                      _wssa_wn;
  BOOL                     _trace;
  BOOL                     _trace_time;
  INT32                    _vsym_idx;  // count vsym index in opt_stab,
                                       // used as vsym WSSA_ST suffix

  // mapping coderep to ver_idx in WSSA 
  hash_map<INTPTR, WSSA::VER_IDX> _cr_to_ver;
  WSSA::VER_IDX Get_cr_ver(CODEREP* cr) const;

  // generate versions
  WSSA::VER_IDX New_use_ver(CODEREP* cr, WN* use_wn);
  WSSA::VER_IDX New_def_ver(CODEREP* cr, WN* def_wn, WSSA::WSSA_NODE_KIND def_kind);
  void Update_def_ver(WSSA::VER_IDX, WN* def_wn, WSSA::WSSA_NODE_KIND def_kind);

  // check if the def of cr is live
  BOOL Is_cr_def_live(const CODEREP* cr) const;
  WSSA::VER_IDX Combine_res_opnd_ver(CODEREP* res, CODEREP* opnd);

  // copy a MU/CHI/PHI_NODE to WN
  void WSSA_Copy_MU_Node(MU_NODE*, WN*);
  void WSSA_Copy_CHI_Node(CHI_NODE*, WN*);
  void WSSA_Copy_PHI_Node(PHI_NODE*, WN*);

  // copy MU/CHI node recursively
  void WSSA_Build_MU_CHI_Version_Expr(WN*);
  void WSSA_Build_MU_CHI_Version_Stmt(WN*);

  // for each WSSA_ST has the same base wn_st index
  // allocate new index 0,1,2 for it
  static INT32 Alloca_Idx(hash_map<INT32, INT32> &st_field_idx_map, INT32 wn_st_idx);

  // convert HSSA vsym type to WSSA vsym type
  static WSSA::WSSA_VSYM_TYPE Vsym_type_from_aux(OPT_VAR_TYPE vt);

public:
  WHIRL_SSA_EMITTER(WSSA::WHIRL_SSA_MANAGER*, OPT_STAB*, WN_MAP);
  ~WHIRL_SSA_EMITTER();

  void Set_trace(BOOL trace)         { _trace = trace;      }
  BOOL Get_trace()                   { return _trace;       }
  void Set_trace_time(BOOL trace)    { _trace_time = trace; }
  BOOL Get_trace_time()              { return _trace_time;  }

public:
  // update symbol table for WSSA
  void WSSA_Convert_OPT_Symbol();
  
  // Map MU/CHI/Version onto WN
  void WSSA_Build_MU_CHI_Version(WN*);

  // Copy Phi node from BB to WN node
  void WSSA_Copy_PHI(BB_NODE*, WN*);
  void WSSA_Copy_Fallthrough_PHI(BB_NODE*, STMT_CONTAINER*);
  WN*  WSSA_Copy_Equivalent_CHI(STMTREP*);
  void WSSA_Set_Ver(WN*, WSSA::VER_IDX);
};

#endif  /* wssa_emitter_INCLUDED */

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
// Module: cgssa_update.h
// 
// Description:
//    Definitions for classes that are used in incrementally updating SSA
//  
// Exported Classes:
//    CGSSA_UPDATER
//
// ====================================================================

#ifndef cgssa_update_INCLUDED
#define cgssa_update_INCLUDED

#include "cgssa_core.h"

using namespace CFG_UTIL;

namespace CGSSA_NAME {

class CGSSA_UPDATER {
private:
  typedef CFG_UTIL::CG_CFG::BB_NODE BB_NODE;
  CGSSA*            _ssa;
  CG_CFG* _cfg;

public:
  CGSSA_UPDATER(CGSSA* ssa, CG_CFG* cfg) 
    : _ssa(ssa), _cfg(cfg) {
    Is_True(_ssa != NULL, ("ssa is NULL"));
    Is_True(_cfg != NULL, ("cfg is NULL"));
  }

  CGSSA* Ssa() { return _ssa; }
  CG_CFG* Cfg() { return _cfg; }

  // Interface functions for SSA updater
  // These functions are counter part of IR update functions
  // with SSA information update.
  VER_ID SSA_Gen_Register_TN(ISA_REGISTER_CLASS rclass, INT size);
  VER_ID SSA_Gen_Typed_Register_TN(TYPE_ID mtype, INT size);
  VER_ID SSA_Gen_Unique_Literal_TN(INT64 ivalue, INT size);
  VER_ID SSA_Gen_Symbol_TN(ST *st, INT64 offset, INT32 relocs);

  void SSA_Set_OP_opnd(OP* op, INT opndnum, VER_ID new_ver_id);
  void SSA_OP_Change_To_Noop(OP *op);
  OP*  SSA_Mk_OP(TOP opr, ...);
  void SSA_Replace_OP(OP* old_op, OP* new_op);

  void Delete_Phi(PHI_NODE* phi, CGSSA_DEL_USES del_uses=CGSSA_DELETE_USES);
  void Delete_OP(OP* op, CGSSA_DEL_USES del_uses=CGSSA_DELETE_USES);
  void Delete_Stmt(VER_ID);

}; /* CGSSA_UPDATER */

} /* namespace CGSSA_NAME */
#endif /* cgssa_update_INCLUDED */


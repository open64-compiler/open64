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
//  Interface for utilities used by dump/verify in WHIRL SSA
//
// Exported functions:
//
// SEE ALSO:
//  be/com/wssa_mgr.h (WHIRL_SSA_MANAGER)
//
//====================================================================

#ifndef wssa_utils_INCLUDED
#define wssa_utils_INCLUDED

#include "defs.h"
#include <stdio.h>
#include <sys/time.h>
#include <vector>
#include "wssa_defs.h"
#include "tracing.h"

#define TT_WSSA_CALL_TRACE        0x00000001 // trace call chain in wssa
#define TT_WSSA_MGR               0x00000010 // trace wssa manager
#define TT_WSSA_EMT               0x00000020 // trace wssa emitter internal
#define TT_WSSA_VERIFIER          0x00000040 // trace wssa verifier
#define TT_WSSA_WHIRL2SRC         0x00000080 // trace wssa whirl2c/whirl2f
#define TT_WSSA_UPDATER           0x00000100 // trace wssa updater
#define TT_WSSA_SIMPLIFIER        0x00000200 // trace wssa simplifier
#define TT_WSSA_LNO               0x00010000 // trace wssa info usage/update in LNO
#define TT_WSSA_EMT_INOUT         0x00020000 // trace code rep/WSSA before/after WSSA Emitter

namespace WSSA {

// utility routines for Print/Verify, etc
// in order to remove the dependency on WHRIL_SSA_MANAGER in Print/Verify routines
void Put_indent(FILE* fp, int indent);
void Print_wst(FILE* fp, WST_IDX idx);
void Print_ver(FILE* fp, VER_IDX idx);
const char* Get_wst_name(WST_IDX idx);
WST_IDX Get_ver_wst(VER_IDX idx);
UINT32 Get_ver_num(VER_IDX idx);
UINT32 Get_wst_max_ver(WST_IDX idx);

// utility routines for dump WN/tree
void dump_wn_ssa(WN* wn);
void dump_tree_ssa(WN* wn);
void fdump_wn_ssa(FILE* f, WN* wn);
void fdump_tree_ssa(FILE* f, WN* wn);

};

#endif /* wssa_utils_INCLUDED */


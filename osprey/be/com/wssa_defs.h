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
// Module: wssa_defs.h
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Basic data type, constant definitions for WHIRL SSA
//
//====================================================================

#ifndef wssa_defs_INCLUDED
#define wssa_defs_INCLUDED

#include "defs.h"

typedef UINT32 WSSA_WN_IDX; // map_cat + wn_map_id
class WN;                   // forward decl of WN
class ST;                   // forward decl of ST

#define INVALID_IDX     ((UINT32)-1)
#define INVALID_VER     ((UINT32)-1)
#define ST_INVALID      0
#define WSSA_ZERO_VER   0

/* internal types */
namespace WSSA {

// wssa sym table index, using enum to be distinguished from ST_IDX
enum WST_IDX {
  WST_IDX_ZERO = 0,
  WST_INVALID = INVALID_IDX
};

// wssa ver table index, using enum to be distinguished from UINT32
enum VER_IDX {
  VER_IDX_ZERO = 0,
  VER_INVALID = INVALID_IDX
};

// wssa symbol types
enum WSSA_SYM_TYPE {
  WST_UNKNOWN,         /* undefined type */
  WST_WHIRL,           /* WHIRL ST */
  WST_PREG,            /* PREG */
  WST_FIELD,           /* FIELDS in struct variable */
  WST_VSYM,            /* virtual symbols */
};

// return the name of sym type
extern const char* WSSA_sym_type_name(INT stype);

// virtual symbol types
enum WSSA_VSYM_TYPE {
  WVT_UNKNOWN,         /* undefined type */
  WVT_NO_LDA_SCALAR,  /* scalar */
  WVT_LDA_SCALAR,      /* lda-based scalar vsym */
  WVT_LDA_VSYM,        /* lda-based non-scalar vsym */
  WVT_UNIQUE_VSYM,     /* unique vsym */
  WVT_SPECIAL_VSYM,    /* special vsym, def_vsym and ret_vsym */
};

// return the name of vsym type
extern const char* WSSA_vsym_type_name(INT vsym);

// symtable related defines
typedef UINT32 FIELD_INFO_IDX; /* index to field_info */
typedef UINT32 VSYM_INFO_IDX;  /* index to vsym_info */

// three kinds of SSA nodes: phi, chi, mu
enum WSSA_NODE_KIND {
  WSSA_UNKNOWN,  /* Unknown */
  WSSA_OCC,      /* Real occurence in WN */
  WSSA_PHI,      /* phi node */
  WSSA_CHI,      /* chi node, may-def */
  WSSA_MU,       /* mu node, may-use */
};

// return the name of SSA nodes
extern const char* WSSA_node_name(INT kind);

// WSSA list related defines
typedef UINT32 WSSA_NODE_IDX;

// forward decl for MANAGER
class WHIRL_SSA_MANAGER;    // forward decl of WHIRL_SSA_MANAGER

} /* namespace WSSA */

#endif /* wssa_defs_INCLUDED */


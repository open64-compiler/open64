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
// Module: wssa_defs.cxx
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Misc routines for WHIRL SSA
//
// Exported functions:
//  WSSA_sym_type_name
//  WSSA_vsym_type_name
//  WSSA_node_name
//
// SEE ALSO:
//  be/com/wssa_defs.h
//
//====================================================================

#include "wssa_defs.h"

namespace WSSA {

// return the name of sym type
const char* WSSA_sym_type_name(INT stype) {
  switch(stype) {
    case WST_WHIRL: return "whirl";
    case WST_PREG:  return "preg";
    case WST_FIELD: return "field";
    case WST_VSYM:  return "vsym";
    default: return "unknown sym type";
  }
}

// return the name of the vsym type
const char* WSSA_vsym_type_name(INT vsym) {
  switch(vsym) {
    case WVT_LDA_SCALAR:   return "lda scalar";
    case WVT_LDA_VSYM:     return "lda non-scalar";
    case WVT_UNIQUE_VSYM:  return "uniq vsym";
    case WVT_SPECIAL_VSYM: return "spec vsym";
    default: return "unknown vsym type";
  }
}

// return the name of the given SSA node kind
const char* WSSA_node_name(INT kind) {
  switch( kind ) {
    case WSSA_OCC: return "wn";
    case WSSA_PHI: return "phi";
    case WSSA_CHI: return "chi";
    case WSSA_MU:  return "mu";
    default: return "unknown ssa node";
  }
}

} /* namespace WSSA */

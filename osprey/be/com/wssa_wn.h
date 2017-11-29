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
// Module: wssa_wn.h
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Interface for WN related functions in WHIRL SSA
//
//====================================================================

#ifndef wssa_wn_INCLUDED
#define wssa_wn_INCLUDED

#include "wssa_defs.h"
class WN;
class ST;

namespace WSSA {

// return TRUE if the node with given kind can attach to the WN
BOOL WN_has_node(const WN* wn, WSSA_NODE_KIND nkind);

// return TRUE if the PHI_NODE can attach to the WN
BOOL WN_has_phi(const WN* wn);

// return TRUE if the CHI_NODE can attach to the WN
// refer WN_has_chi() in  opt/opt_wn.cxx
BOOL WN_has_chi(const WN* wn);

// return TRUE if the MU_NODE can attach to the WN
// refer WN_has_mu() in opt/opt_wn.cxx
BOOL WN_has_mu (const WN* wn);

// return TRUE if the version can attach to the WN
BOOL WN_has_ver(const WN* wn);

// return TRUE if the version can be defined by the WN (STID/STBITS)
BOOL WN_def_ver(const WN* wn);

// return TRUE if the version can be used by the WN (LDID/LDBITS)
BOOL WN_use_ver(const WN* wn);

// return TRUE if WN or ST used in WN node is volatile
BOOL WN_is_volatile(const WN* wn);

// return TRUE if WN or ST used in WN tree is volatile
// tree must be statement or expression
BOOL Tree_is_volatile(const WN* tree);

} /* namespace WSSA */

#endif  /* wssa_wn_INCLUDED */

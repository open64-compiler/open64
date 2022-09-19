/*
 *  Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#ifndef uwasm_nat_util_INCLUDED
#define uwasm_nat_util_INCLUDED

/* ====================================================================
 *
 * Module: uwasm_nat_util.h
 *
 * Description:
 *
 * Utilities for uwasm native compilation, focus on:
 * instrumentation for data isolation
 *
 * ====================================================================
 */

#include "defs.h"
#include "srcpos.h"

class WN;
class ST;

// UWASM Isolation kind
enum UWASN_ISO_KIND {
  UWASM_ISO_NONE,    // No isolation
  UWASN_ISO_WRAP,    // Wrap-around address
  UWASM_ISO_ASSERT,  // Assert if addr out of range
};

// UWASM native memory isolation related
extern ST *Uwasm_addr_mask_st();
extern ST *Uwasm_addr_base_st();
extern ST *Uwasm_ofst_mask_st();

// conditions to check if address should be guarded
BOOL Uwasm_check_addr(WN *wn);

// Instru in VHO phase
void Uwasm_instru_load_in_vho(WN *block, WN *load, SRCPOS spos);
void Uwasm_instru_store_in_vho(WN *block, WN *store, SRCPOS spos);

// Helper function to lower assert
WN * WN_lower_assert(WN *block, WN *tree, INT64 actions);
// Instru in Lower-To-CG phase
void Uwasm_instru_load_before_cg(WN *block, WN *load, SRCPOS spos);
void Uwasm_instru_store_before_cg(WN *block, WN *store, SRCPOS spos);

#endif /* uwasm_nat_util_INCLUDED */

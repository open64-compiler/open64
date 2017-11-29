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


#include <alloca.h>
#include <stdio.h>
#include <limits.h>
#include <assert.h>

#include "ti_si.h"
#include "ti_errors.h"
#include "targ_isa_bundle.h"
#include "ti_bundle.h"


/* ====================================================================
 *
 *  TI_BUNDLE_Has_Property
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_BUNDLE_Has_Property( 
  TI_BUNDLE *bundle,
  ISA_EXEC_UNIT_PROPERTY property,
  INT *error
)
{
  INT i;

  *error = TI_RC_OKAY;

  if (bundle == NULL) {
    sprintf(TI_errmsg, "ISA_BUNDLE_INFO is empty");
    *error = TI_RC_ERROR;
    return FALSE;
  }

  for (i = 0; i < TI_BUNDLE_slot_count(bundle); i++) {
    if (TI_BUNDLE_exec_property(bundle,i) & property) 
      return TRUE;
  }
  
  return FALSE;
}
    
/* ====================================================================
 *
 *  TI_BUNDLE_Is_Full
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_BUNDLE_Is_Full( 
  TI_BUNDLE *bundle,
  INT *error
)
{
  INT i;

  *error = TI_RC_OKAY;

  if (bundle == NULL) {
    sprintf(TI_errmsg, "ISA_BUNDLE_INFO is empty");
    *error = TI_RC_ERROR;
    return FALSE;
  }

  for (i = 0; i < TI_BUNDLE_slot_count(bundle); i++) {
    if (!TI_BUNDLE_slot_filled(bundle, i)) 
      return FALSE;
  }
  
  return TRUE;
}

/* ====================================================================
 *
 *  TI_BUNDLE_Is_Empty
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_BUNDLE_Is_Empty( 
  TI_BUNDLE *bundle,
  INT *error
)
{
  INT i;

  *error = TI_RC_OKAY;

  if (bundle == NULL) {
    sprintf(TI_errmsg, "ISA_BUNDLE_INFO is empty");
    *error = TI_RC_ERROR;
    return FALSE;
  }

  for (i = 0; i < TI_BUNDLE_slot_count(bundle); i++) {
    if (TI_BUNDLE_slot_filled(bundle, i)) 
      return FALSE;
  }
  
  return TRUE;
}

/* ====================================================================
 *
 *  TI_BUNDLE_Return_Template
 *
 *  See interface description
 *
 * ====================================================================
 */
INT TI_BUNDLE_Return_Template( 
  TI_BUNDLE *bundle
)
{
  return TI_BUNDLE_pack_code(bundle);
}

/* ====================================================================
 *
 *  TI_BUNDLE_Clear
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_BUNDLE_Clear( 
  TI_BUNDLE *bundle
)
{
  INT i;
  for (i = 0; i < ISA_MAX_SLOTS; i++) {
    TI_BUNDLE_slot_filled(bundle, i) =  0;
    TI_BUNDLE_exec_property(bundle, i) = 0;
    TI_BUNDLE_stop_bit(bundle, i) = 0; 
  }

  Set_TI_BUNDLE_pack_code(bundle, 0);
  Set_TI_BUNDLE_stop_mask(bundle, 0x0);
  Set_TI_BUNDLE_slot_mask(bundle, 0x0);
  Set_TI_BUNDLE_slot_count(bundle, ISA_MAX_SLOTS);

}

/* ====================================================================
 *
 *  TI_BUNDLE_Slot_Available
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_BUNDLE_Slot_Available(
  TI_BUNDLE  *bundle,
  ISA_EXEC_UNIT_PROPERTY property,
  INT        slot
)
{
  INT i, j;
  ISA_EXEC_UNIT_PROPERTY slot_prop;
  BOOL match;

  /* if <slot> is exceeds the bundle slot capacity, return FALSE */
  if (slot >= ISA_MAX_SLOTS) return FALSE;
  
  /* Loop through the bundles unless either a match is found or withdrawn. 
     TODO: Access it through hierarchical (or state) tables */

/* an instruction is compatible with a template slot if its EXEC_UNIT_PROPERTY
   is a superset of the slot's EXEC_UNIT_PROPERTY.  As a special case, consider
   instructions with an EXEC_UNIT_PROPERTY of 0 to always match */

#define SLOTS_COMPATIBLE(_prop, _bundle_index, _slot_index) \
 ((_prop == 0) || \
  ((_prop & ISA_EXEC_Slot_Prop(_bundle_index, _slot_index)) == \
   ISA_EXEC_Slot_Prop(_bundle_index, _slot_index)))
  
  /* Loop through the bundle and slot info. */  
  for (i = 0; i < ISA_MAX_BUNDLES; ++i) {
    match = TRUE;
    for (j = 0; j < ISA_MAX_SLOTS; ++j) {
      slot_prop = (j == slot) ? property : TI_BUNDLE_exec_property(bundle, j);
      if ((j == slot) || TI_BUNDLE_slot_filled(bundle, j) ||
	  TI_BUNDLE_stop_bit(bundle, j)) {
	  if (!SLOTS_COMPATIBLE(slot_prop, i, j) ||
		  (TI_BUNDLE_stop_bit(bundle, j) != ISA_EXEC_Stop(i, j))) {
	    match = FALSE;
	    break;
	}
      }
    }
    if (match) {
      Set_TI_BUNDLE_pack_code(bundle, i);
      return TRUE;
    }
  }
  return FALSE;
}

/* ====================================================================
 *
 *  TI_BUNDLE_Stop_Bit_Available
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_BUNDLE_Stop_Bit_Available(
  TI_BUNDLE  *bundle,
  INT        slot
)
{
  INT i, j;
  ISA_EXEC_UNIT_PROPERTY slot_prop;
  BOOL stop_bit_value;
  BOOL match;

  /* if <slot> is exceeds the bundle slot capacity, return FALSE */
  if (slot >= ISA_MAX_SLOTS) return FALSE;
  if (slot == (ISA_MAX_SLOTS - 1))
      return TRUE;

  /* Loop through the bundle and slot info. */  
  for (i = 0; i < ISA_MAX_BUNDLES; ++i) {
    match = TRUE;
    for (j = 0; j < ISA_MAX_SLOTS; ++j) {
      stop_bit_value = (j == slot) ? TRUE : TI_BUNDLE_stop_bit(bundle, j);
      if (ISA_EXEC_Stop(i, j) != stop_bit_value) {
	  /* always look at stop bits */
	  match = FALSE;
	  break;
      } else if (TI_BUNDLE_slot_filled(bundle, j)) {
	  /* only look at exec props if slot is filled */
	  slot_prop = TI_BUNDLE_exec_property(bundle, j);
	  if (!SLOTS_COMPATIBLE(slot_prop, i, j)) {
	      match = FALSE;
	      break;
	  }
      }
    }
    if (match) {
      Set_TI_BUNDLE_pack_code(bundle, i);
      return TRUE;
    }
  }
  return FALSE;
}

/* ====================================================================
 *
 *  TI_BUNDLE_Reserve_Slot
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_BUNDLE_Reserve_Slot(
  TI_BUNDLE  *bundle,
  INT slot,
  ISA_EXEC_UNIT_PROPERTY property
)
{

  TI_BUNDLE_slot_filled(bundle, slot) = TRUE;
  Set_TI_BUNDLE_exec_property(bundle, slot, property);
  TI_BUNDLE_slot_mask(bundle) |= property << 
    (ISA_TAG_SHIFT * (ISA_MAX_SLOTS - slot - 1));
}

/* ====================================================================
 *
 *  TI_BUNDLE_Reserve_Stop_Bit
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_BUNDLE_Reserve_Stop_Bit(
  TI_BUNDLE  *bundle,
  INT slot
)
{
  if (slot < 0 || slot > ISA_MAX_SLOTS) {
    fprintf(stderr,"TI_BUNDLE_Reserve_Stop_Bit: slot value not legal value \n");
    assert(FALSE);
  }

  Set_TI_BUNDLE_stop_bit(bundle, slot, TRUE);
}

/* ====================================================================
 *
 *  TI_BUNDLE_Unreserve_Stop_Bit
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_BUNDLE_Unreserve_Stop_Bit(
  TI_BUNDLE  *bundle,
  INT slot
)
{
  if (slot < 0 || slot > ISA_MAX_SLOTS) {
    fprintf(stderr,"TI_BUNDLE_Reserve_Stop_Bit: slot value not legal value \n");
    assert(FALSE);
  }

  Set_TI_BUNDLE_stop_bit(bundle, slot, FALSE);
}

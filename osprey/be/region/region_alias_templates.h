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


//-*-c++-*-
//============================================================================
//
// Module: region_alias_templates.h
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:58-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/region/SCCS/s.region_alias_templates.h $
//
// Revision history:
//  22-SEP-97 dahl - Original Version
//
//============================================================================

#include "region_util.h"	// POINTS_TO_SET
#include "opt_alias_mgr.h"	// ALIAS_MANAGER
#include "opt_alias_rule.h"	// ALIAS_RULE
#include "opt_points_to.h"	// BASE_IS_FIXED

// =======================================================================
// Boundary set search comparison functions for REGION_search_set
// can call search two ways:

// used to see if a PT is aliased with the boundary set
struct comp_aliased {
 private:
  const POINTS_TO *_item;
  const ALIAS_RULE *_ar;
 public:
  comp_aliased(const POINTS_TO *item, const ALIAS_RULE *ar) :
  	_item(item), _ar(ar) { }
  BOOL operator()(const POINTS_TO *x) const {
    // copy POINTS_TOs to local memory
    POINTS_TO x_copy, item_copy;
    x_copy.Copy_fully(x);
    item_copy.Copy_fully(_item);
    // remove const attribute from each
    x_copy.Reset_const();
    item_copy.Reset_const();
    // call Aliased_Memop
    Is_True(_ar != NULL, ("comp_aliased template, alias rule is NULL"));
    return _ar->Aliased_Memop(&item_copy, &x_copy);
  }
};

// used to check for duplicates in the boundary set, calls Meet to merge
// overlaps with the list
struct comp_same_pt {
 private:
  const POINTS_TO *_item;
  const ALIAS_RULE *_ar;
 public:
  comp_same_pt(const POINTS_TO *item, const ALIAS_RULE *ar) :
  	_item(item), _ar(ar) { }
  // may modify parameter
  BOOL operator()(POINTS_TO *x) {
    if (_ar) {
      struct comp_aliased comp(_item, _ar);
      if (!comp(x))
	return FALSE;
    }
    if (x->Similar(_item)) { // checks base, offset size
      x->Meet(_item, NULL);
      return TRUE;
    } else
      return FALSE;
  }
};

template <class COMP>
POINTS_TO *REGION_search_set(POINTS_TO_SET *pset, COMP comp)
{
  // don't check pset for NULL, it can be empty
  for (POINTS_TO_SET *ptmp=pset; ptmp; ptmp=ptmp->Next) {
    if (comp(ptmp->Pt))
      return ptmp->Pt;
  }
  return NULL;
}

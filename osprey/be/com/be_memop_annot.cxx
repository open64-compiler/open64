//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Copyright (C) 2007, University of Delaware, Hewlett-Packard Company, 
//  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// ====================================================================

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */

#include "defs.h"
#include "config.h"
#include "errors.h"

#include <vector>
using namespace std;

#include "wn.h"
#include "wn_map.h"
#include "cxx_memory.h"
#include "tracing.h"

#include "be_memop_annot.h"

WN_MEMOP_ANNOT_MGR* WN_MEMOP_ANNOT_MGR::_unique_instance = NULL;
MEMOP_ANNOT_MGR* MEMOP_ANNOT_MGR::active_mgr = NULL; 
	
BOOL
MEMOP_ANNOT_ITEM::operator == (const MEMOP_ANNOT_ITEM& that) const {
  BOOL equ = FALSE;
  if (Kind() == that.Kind()) {
    switch (Kind()) {
    case MEM_ANNOT_INVALID: break;
    case MEM_ANNOT_MALLOC_ID:
      equ = (Malloc_id () == that.Malloc_id());
      break;
    case MEM_ANNOT_LMV_ALIAS_GROUP:
      equ = (LMV_alias_group () == that.LMV_alias_group ());
      break;

    default:
      FmtAssert (FALSE, ("Unknown kind %d", Kind()));
    }
  }
  return equ;
}

///////////////////////////////////////////////////////////////////////////
//
//           Implementation of MEMOP_ANNOT
//
///////////////////////////////////////////////////////////////////////////
//

// Lookup the annotation item of given kind.
//
BOOL
MEMOP_ANNOT::Lookup (MEM_ANNOT_KIND kind, MEMOP_ANNOT_ITEM& res) const {

  if (_kinds & kind) {
    for (MEMOP_ANNOT_ITEM_CITER iter = _vect.begin (); 
         iter != _vect.end (); iter++) {
      if ((*iter).Kind() == kind) {
	res = *iter;
	return TRUE;
      }
    }
    Is_True (FALSE, ("Fail to find the annotation of kind %d", kind));
  }

  return FALSE;
}

void
MEMOP_ANNOT::Remove (MEM_ANNOT_KIND kind) {

  if (_kinds & kind) {
    for (MEMOP_ANNOT_ITEM_ITER iter = _vect.begin (); 
         iter != _vect.end (); iter++) {
      if ((*iter).Kind() == kind) {
        *iter = _vect.back ();
        _vect.pop_back ();
        Remove_kind (kind); 
        return;
      }
    }
    Is_True (FALSE, ("Fail to kind the annotation of kind %d", kind));
  }
}

void
MEMOP_ANNOT::Replace_or_add (const MEMOP_ANNOT_ITEM& annot) {

  if (annot.Kind() == MEM_ANNOT_INVALID && 
      annot.Kind() > MEM_ANNOT_LAST_KIND) {
    DevWarn ("Add a invalid item") ;
    return;
  }

  MEM_ANNOT_KIND kind = annot.Kind ();
  if (_kinds & kind) {
    for (MEMOP_ANNOT_ITEM_ITER iter = _vect.begin (); 
         iter != _vect.end (); iter++) {
      if ((*iter).Kind() == kind) {
	*iter = annot;
      }
      return;
    }
    Is_True (FALSE, ("Fail to kind the annotation of kind %d", kind));
  }
  _vect.push_back (annot);
  _kinds = MEM_ANNOT_KIND(UINT(_kinds) | UINT(annot.Kind()));
}

void
MEMOP_ANNOT::Meet (const MEMOP_ANNOT_ITEM* that) {

  MEMOP_ANNOT_ITEM match; 
  BOOL find_match = Lookup (that->Kind(), match) && match == (*that);
  Remove_all ();

  if (find_match) {
    _vect.push_back (match);
    _kinds = that->Kind();
  } 
}
 
///////////////////////////////////////////////////////////////////////////
//
// Meet the two annotation sets. This function is useful when propagating 
// data flow information at phi node. The net result is to get rid of 
// all <this>'s annotations that does not show up in <that>.
//
///////////////////////////////////////////////////////////////////////////
//
void 
MEMOP_ANNOT::Meet (const MEMOP_ANNOT* that) {
  
  BOOL change = FALSE;

  for (MEMOP_ANNOT_ITEM_ITER iter = _vect.begin (); 
       iter != _vect.end (); iter++) {
    MEMOP_ANNOT_ITEM& item = *iter;
    MEMOP_ANNOT_ITEM item2;
    if (!Lookup (item.Kind(), item2) || item == item2) {
      // Do not erase and advance iterator, it seems has bug. 
      item.Set_kind (MEM_ANNOT_INVALID); 
      change = TRUE;
    }
  }

  if (change) {
    MEMOP_ANNOT_ITEM_VECT t;
    Clear_kinds ();

    for (MEMOP_ANNOT_ITEM_ITER iter = _vect.begin (); iter != _vect.end (); iter++) {

      if ((*iter).Kind () != MEM_ANNOT_INVALID) {
        t.push_back (*iter); 
        Add_kind ((*iter).Kind());
      }
    }
    _vect = t;
  }
}

void 
MEMOP_ANNOT::Union (const MEMOP_ANNOT* that) {
  
  if (that == NULL) return;  
  const MEMOP_ANNOT_ITEM_VECT* vect = &that->_vect; 
  for (MEMOP_ANNOT_ITEM_CITER iter = vect->begin (); 
       iter != vect->end (); iter++) {

    const MEMOP_ANNOT_ITEM& item = *iter;
    MEMOP_ANNOT_ITEM item2;
    if (!Lookup (item.Kind(), item2)) {
      _vect.push_back (item2);
      Add_kind (item2.Kind());
    } else if (item != item2) {
      DevWarn ("memory annotations of kind %d does not agree", item2.Kind());
    };
  }
}

///////////////////////////////////////////////////////////////////////////
//
//   Implementation of MEMOP_ANNOT_MGR
//
///////////////////////////////////////////////////////////////////////////
//
MEMOP_ANNOT_MGR::MEMOP_ANNOT_MGR (MEM_POOL* mp): 
  _mp(mp), _all_annot(mp), _recycled_annot (mp) {

  _all_annot.push_back (NULL);
  _last_id = 0;

  // This initial capacity is large enough for most cases 
  //
  _all_annot.reserve (512 * sizeof (MEMOP_ANNOT*));
  _recycled_annot.reserve (512 * sizeof(MEMOP_ANNOT*));
}

void
MEMOP_ANNOT_MGR::Init (void) {
  
  for (MOA_VECT_ITER iter = _all_annot.begin (); 
       iter != _all_annot.end (); iter++) {
    if (*iter != NULL)
      _recycled_annot.push_back (*iter);
  }
  _all_annot.clear ();
  _all_annot.push_back (NULL);
  _last_id = 0;
}

MEMOP_ANNOT*
MEMOP_ANNOT_MGR::Alloc_annot (void) {
  MEMOP_ANNOT* t; 
  if (_recycled_annot.empty()) {
    t = CXX_NEW (MEMOP_ANNOT(_mp, ++_last_id), _mp);
  } else {
    t = _recycled_annot.back ();
    _recycled_annot.pop_back (); 
    t->Recycle(++_last_id);
  }
  _all_annot.push_back (t);
  Is_True (_all_annot[_last_id] == t && t->Id() == _last_id, 
           ("Inconsistent status"));

  return t;
}

////////////////////////////////////////////////////////////////////////////
//
//  Implementation of WN_MEMOP_ANNOT_MGR 
//
////////////////////////////////////////////////////////////////////////////
void
WN_MEMOP_ANNOT_MGR::Init (void) {

  MEMOP_ANNOT_MGR::Init (); 
  if (_wn_map != WN_MAP_UNDEFINED) { WN_MAP_Delete (_wn_map);}
  _wn_map = WN_MAP_Create (_mp);
}

WN_MEMOP_ANNOT_MGR::WN_MEMOP_ANNOT_MGR (MEM_POOL* mp): 
  MEMOP_ANNOT_MGR(mp) {

  Is_True (_unique_instance == NULL, 
           ("WN_MEMOP_ANNOT_MGR disallow more than one instance"));

  _unique_instance = this;
  _wn_map = WN_MAP_UNDEFINED;
}

WN_MEMOP_ANNOT_MGR::~WN_MEMOP_ANNOT_MGR (void) {
  _unique_instance = NULL; 
  if (_wn_map != WN_MAP_UNDEFINED) { WN_MAP_Delete (_wn_map);}
}

////////////////////////////////////////////////////////////////////////////
//
//   Implementation of PT_MEM_ANNOT 
//
////////////////////////////////////////////////////////////////////////////
//
void
PT_MEM_ANNOT::Remove_annot (MEM_ANNOT_KIND kind) {
  if (_flags == USE_INLINE_ITEM) {
    if (kind == _inlined_annot.Kind()) {
      _flags = (PT_MEM_ANNOT_SELECTOR)0;
    }
    return;
  } else if (_flags == USE_ANNOT_PTR) {
    _annot_ptr->Remove (kind);
  }
}

void
PT_MEM_ANNOT::Replace_or_add_annot (const MEMOP_ANNOT_ITEM& item) {

  if (item.Kind() == MEM_ANNOT_INVALID) return; 

  // if the value is is 0, might as well delete it.
  if (item.Is_invalid()) {
    Remove_annot (item.Kind()); 
    return;
  }

  if (!_flags || 
      _flags == USE_INLINE_ITEM && item.Kind() == _inlined_annot.Kind()) {
    // by far the most common case.
    _inlined_annot = item;
    _flags = USE_INLINE_ITEM; 
    return;
  }
  
  if (_flags == USE_ANNOT_PTR) {
    // less frequent case
    _annot_ptr->Replace_or_add (item);
    return;
  }

  Is_True (_flags == USE_INLINE_ITEM, ("Incorrect selector")); 

  // rare case
  MEMOP_ANNOT_MGR* p = MEMOP_ANNOT_MGR::Active_mgr ();
  if (p) {
    _annot_ptr  = p->Alloc_annot_v ();
    _annot_ptr->Replace_or_add (item);
    _annot_ptr->Replace_or_add (_inlined_annot);
    _flags = USE_ANNOT_PTR;
  } else {
    DevWarn ("fail to add annotations because no active annotation manager");
  }
}

void
PT_MEM_ANNOT::Replace_or_add_annots (MEMOP_ANNOT* annots) {

  // Make sure annots is allocated by current active annotation manager
  Is_True (MEMOP_ANNOT_MGR::Active_mgr()->Alloc_by_this_class (annots), 
    ("Annotation should be allocated by current active annotation manager"));
   
  if (_flags == USE_INLINE_ITEM) {
    if (!_inlined_annot.Is_invalid()) {
      annots->Replace_or_add (_inlined_annot);
    }
  } else {
    Is_True (!_flags || _flags == USE_ANNOT_PTR, ("Incorrect flags"));   
    if (_flags == USE_ANNOT_PTR)
      annots->Union (_annot_ptr);
  }

  _flags = USE_ANNOT_PTR;
  _annot_ptr = annots;
}

void 
PT_MEM_ANNOT::Set_malloc_id (UINT64 id) {
  if (id != INVALID_ITEM_VALUE) {
    MEMOP_ANNOT_ITEM t;
    t.Set_malloc_id (id);
    Replace_or_add_annot (t);
  } else {
    Remove_annot (MEM_ANNOT_MALLOC_ID);
  }
}

void
PT_MEM_ANNOT::Set_LMV_alias_group (LMV_ALIAS_GROUP grp_id) {

  if (grp_id != INVALID_ITEM_VALUE) {
    MEMOP_ANNOT_ITEM t;
    t.Set_LMV_alias_group (grp_id);
    Replace_or_add_annot (t);
  } else {
    Remove_annot (MEM_ANNOT_LMV_ALIAS_GROUP);
  }
}

BOOL
PT_MEM_ANNOT::Lookup (MEM_ANNOT_KIND kind, MEMOP_ANNOT_ITEM& t) const {
  if (!_flags) return FALSE;
  if (_flags == USE_INLINE_ITEM) { 
     t = _inlined_annot; 
     return !t.Is_invalid () && t.Kind() == kind;
  } else {
    Is_True (_flags == USE_ANNOT_PTR, ("Incorrect flags"));
    return _annot_ptr->Lookup (kind, t);
  }
}

void
PT_MEM_ANNOT::Meet (const PT_MEM_ANNOT& that) {
  if (!Has_annotation ()) return;

  if (!that.Has_annotation ()) {
    Invalidate (); return;
  }

  if (Item_is_inlined ()) {

    const MEMOP_ANNOT_ITEM& i1 = Get_inlined_item(); 
    if (that.Item_is_inlined()) {
      const MEMOP_ANNOT_ITEM& i2 = that.Get_inlined_item();
      if (i1 != i2) {
        Invalidate ();
      }
    } else {
      MEMOP_ANNOT_ITEM t;
      if (!that.Get_annots_ptr()->Lookup (i1.Kind(), t) || i1 != t) {
        Invalidate ();
      }
    }
  } else {
    if (that.Item_is_inlined())
      Get_annots_ptr()->Meet (&that.Get_inlined_item());
    else {
      Get_annots_ptr()->Meet (that.Get_annots_ptr());   
    }
  }
}

////////////////////////////////////////////////////////////////////////////
//
//   All dump functions go here
//
////////////////////////////////////////////////////////////////////////////
//
void
MEMOP_ANNOT_ITEM::Print (FILE* f) const {
  switch (Kind()) {
  case MEM_ANNOT_INVALID:
    fprintf (f, "Invalid"); 
    break;

  case MEM_ANNOT_MALLOC_ID:
    fprintf (f, "malloc:%#llx", (unsigned long long)Malloc_id ());
    break;

  case MEM_ANNOT_LMV_ALIAS_GROUP:
    fprintf (f, "alias-grp:%#llx", (unsigned long long)LMV_alias_group ());
    break; 

  default:
    FmtAssert (FALSE, ("Invalid kind %d", Kind()));
  };
}

void
MEMOP_ANNOT::Print (FILE* f) const {
 
  fprintf (f, "id:%d,", _id);

  for (MEMOP_ANNOT_ITEM_CITER iter = _vect.begin (); iter != _vect.end(); iter++) {
    const MEMOP_ANNOT_ITEM& item = *iter; 
    (*iter).Print (f);
    fprintf (TFile, ",");
  }

  // no new-line
}

void
PT_MEM_ANNOT::Print (FILE* f, BOOL verbose) const {
  if (_flags) {
    if (Item_is_inlined ()) {
      _inlined_annot.Print (f);
    } else if (!verbose) {
      fprintf (f, "annot:%d", _annot_ptr->Id());
    } else {
      _annot_ptr->Print (f);
    }
  }
}

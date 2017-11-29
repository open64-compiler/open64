//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_union_find.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_union_find.h,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
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
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// The template pair
//   U_F_REP<ELEMENT_TYPE>  and  U_F_ELEMENT<ELEMENT_TYPE>
// efficiently implements sets with two operators: find representative
// and set union.
//
// U_F_ELEMENT<ELEMENT_TYPE> is the type of an element of a set.
// Each set is assigned an object of type U_F_REP<ELEMENT_TYPE>
// which acts as a representation for that set.  Also, each
// representative U_F_REP<ELEMENT_TYPE> holds a pointer to an object
// of type ELEMENT_TYPE.
//
// ELEMENT_TYPE *
// U_F_REP<ELEMENT_TYPE>::Representative(void) const;
//   returns a pointer to its associated object of type ELEMENT_TYPE.
//
// U_F_REP<ELEMENT_TYPE> *
// U_F_REP<ELEMENT_TYPE>::Union(U_F_REP<ELEMENT_TYPE> &that);
//   merges this set and that set, and returns a pointer to the
//   resulting union set representative.  (The representative of a
//   union of sets is chosen to be one of the representatives of the
//   sets being merged.
//
// U_F_REP<ELEMENT_TYPE> *
// U_F_ELEMENT<ELEMENT_TYPE>::Find(void);
//   returns a pointer to the representative of the set containing
//   this element.
//
// Implementation:
//
// The U_F_ELEMENT objects in each set are arranged in a tree.  Each
// U_F_ELEMENT has a parent U_F_ELEMENT, except for the root, which has
// a pointer to the representative U_F_REP object for that set.
//
// ====================================================================
// ====================================================================


#ifndef opt_union_find_INCLUDED
#define opt_union_find_INCLUDED "opt_union_find.h"

#include "defs.h"


// ====================================================================


template <class ELEMENT_TYPE> class U_F_ELEMENT;

template <class ELEMENT_TYPE>
class U_F_REP {
  friend class U_F_ELEMENT<ELEMENT_TYPE>;
protected:

  // _height is an estimate of the depth of the tree of U_F_ELEMENT
  //   class objects.  It is used as a heuristic during set union
  //   operations to minimize the depth of the resulting set.
  // _representative points to the ELEMENT_TYPE object representative
  //   for this set.

  UINT          _height;
  ELEMENT_TYPE *_representative;

public:
  U_F_REP(void) : _height(0), _representative(NULL) { }

  void          Set_representative(ELEMENT_TYPE *representative)
    { _representative = representative; }

  void          Set_height(UINT height)
    { _height = height; }

  ELEMENT_TYPE *Representative(void) const
    {
      Is_True(_representative == NULL ||
	      _representative->Null_parent(),
	      ("UNION_FIND: Set representative must not have parent"));
      return _representative;
    }

  U_F_REP<ELEMENT_TYPE> *Union(U_F_REP<ELEMENT_TYPE> &);
};


// ====================================================================


template <class ELEMENT_TYPE>
class U_F_ELEMENT {
private:

  // _parent is the parent node of this U_F_ELEMENT in this set tree.
  // _set_rep is the representative of this set iff this U_F_ELEMENT
  //   is the root of its set tree (iff _parent == NULL).

  U_F_ELEMENT           *_parent;
  U_F_REP<ELEMENT_TYPE> *_set_rep;

public:
  U_F_ELEMENT(void) : _parent(NULL), _set_rep(NULL) { }

  void Put_in_set(U_F_REP<ELEMENT_TYPE> *set)
    {
      FmtAssert(_parent == NULL &&
		_set_rep == NULL,
		("U_F_ELEMENT::Put_in_set: Element already "
		 "belongs somewhere"));
      if (set->Representative() == NULL) {
	// We're the first element of the set.
	set->Set_representative((ELEMENT_TYPE *) this);
	_set_rep = set;
      }
      else {
	_parent = set->Representative();
      }
    }

  void Set_parent(U_F_ELEMENT *parent) { _parent = parent; }

  BOOL Null_parent(void) const { return _parent == NULL; }

#if Is_True_On
  void Clear_set_rep(void) { _set_rep = NULL; }
#endif

  U_F_REP<ELEMENT_TYPE> *Find(void);
};


// ====================================================================


// here starts definition of templatized routines

template <class ELEMENT_TYPE> U_F_REP<ELEMENT_TYPE> *
U_F_REP<ELEMENT_TYPE>::Union(U_F_REP<ELEMENT_TYPE> &that)
{
  if (Representative() != that.Representative()) {
    U_F_REP<ELEMENT_TYPE> *parent, *child;

    if (_height < that._height) {
      parent = &that;
      child  =  this;
    }
    else {
      parent =  this;
      child  = &that;
    }

    if (_height == that._height) {
      ++(parent->_height);
    }

#if Is_True_On
    // For debugging, make sure we can see that the child's
    // representative is no longer the root of a tree.
    child->Representative()->Clear_set_rep();
#endif

    child->Representative()->Set_parent(parent->Representative());

    Set_representative(parent->Representative());
    Set_height(parent->_height);
    // We really should free the memory associated with *child here.
    return parent;
  }
  else {
    FmtAssert(this == &that,
	      ("U_F_REP: Different sets must have different "
	       "representatives"));
    return this;
  }
}


template <class ELEMENT_TYPE> U_F_REP<ELEMENT_TYPE> *
U_F_ELEMENT<ELEMENT_TYPE>::Find(void)
{
  if (_parent == NULL) {
    Is_True(_set_rep == NULL ||
	    _set_rep->Representative() == this,
	    ("UNION_FIND::Find: Inconsistent representative"));
    return _set_rep;
  }
  else {
    Is_True(_parent != this,
	    ("UNION_FIND: Endless find loop"));
    U_F_REP<ELEMENT_TYPE> *retval = _parent->Find();
    Is_True(retval != NULL,
	    ("UNION_FIND: Elements in no set may not be connected."));
    _parent = retval->Representative();
    return retval;
  }
}

// ====================================================================

#endif

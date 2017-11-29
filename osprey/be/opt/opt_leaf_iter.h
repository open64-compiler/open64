//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_leaf_iter.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_leaf_iter.h,v $
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
// ====================================================================


#ifndef opt_leaf_iter_INCLUDED
#define opt_leaf_iter_INCLUDED "opt_leaf_iter.h"

#include "cxx_template.h"
#include "opt_htable.h"

template <class NODE_TYPE>
void Expand_onto_stack(STACK<NODE_TYPE *> &, NODE_TYPE *);

void Expand_onto_stack(STACK<CODEREP *> &s, CODEREP *const cr);

void Expand_onto_stack(STACK<WN *> &s, WN *const wn);

template <class NODE_TYPE>
class LEAF_ITER {
private:
  STACK<NODE_TYPE *>  s;
  NODE_TYPE          *initial_node;  // For rewinding to support First().
  NODE_TYPE          *last;
  const BOOL          _tracing;

public:
  LEAF_ITER(MEM_POOL *const pool, const BOOL tracing) :
    _tracing(tracing), s(pool)
      { }

  LEAF_ITER(NODE_TYPE *node, MEM_POOL *pool, BOOL tracing) :
    _tracing(tracing), s(pool)
      { Init(node); }

  ~LEAF_ITER(void) { }

  void Init(NODE_TYPE *node)
    { initial_node = node; last = NULL; }

  BOOL Is_Empty(void) const
    {
      // Can't use (Elements() == 0) as a test because silly loop
      // iteration macros use Is_Empty as a loop termination
      // condition, rather than check whether Next() returned
      // NULL. For this reason, Is_Empty should really be called
      // Was_Empty_Last_Time().
      return (last == NULL);
    }

  NODE_TYPE *Next(void)
    {
      if (s.Elements() > 0) {
	NODE_TYPE *cur = s.Pop();
	last = cur;
	return cur;
      }
      else {
	last = NULL;
	return NULL;
      }
    }

  NODE_TYPE *First(void) { s.Clear();
			   Expand_onto_stack(s, initial_node);
			   return Next();	}

  NODE_TYPE *Cur(void) const    { return last; }
};

#endif

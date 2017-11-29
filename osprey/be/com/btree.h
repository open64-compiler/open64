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


// -*-C++-*-

/**
*** This file contains a template for BINARY TREES
*** This must be instantiated with a BINARY_NODE
*** that has the operators <, == and = defined
***
***  template <class BINARY_NODE> class BINARY_TREE
*** 
***   	    BINARY_TREE(MEM_POOL *pool)
***
***		Create a new binary tree.
***
***	    void Enter(BINARY_NODE NODE)
***
***		Enter a copy of NODE into the tree.  Does nothing if
***		it's already in the tree.
***
***	    BOOL *Find(BINARY_NODE element) const
***
***		Is element in the tree?
***
***	    ~BINARY_TREE() 
***
**/

#ifndef bin_tree_INCLUDED
#define bin_tree_INCLUDED 

static const char *bin_tree_rcs_id  = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.btree.h $ $Revision: 1.2 $";


#ifndef defs_INCLUDED
#include "defs.h"
#endif
#ifndef ERRORS_INCLUDED
#include "errors.h"
#endif
#ifndef CXX_MEMORY_INCLUDED
#include "cxx_memory.h"
#endif


template <class BINARY_NODE> 
class BINARY_TREE_NODE {
  BINARY_TREE_NODE<BINARY_NODE> *_left; 
  BINARY_TREE_NODE<BINARY_NODE> *_right; 
  BINARY_NODE   _data;
public:
  // create a new element
  BINARY_TREE_NODE(BINARY_NODE data) {
    _data = data;
    _left = _right = NULL;
  }; 
  BINARY_TREE_NODE<BINARY_NODE>* Enter(BINARY_NODE node, MEM_POOL *pool);
  BINARY_TREE_NODE<BINARY_NODE>* Find(BINARY_NODE node) const;
  ~BINARY_TREE_NODE() {
    if (_left) CXX_DELETE(_left,Default_Mem_Pool);
    if (_right) CXX_DELETE(_right,Default_Mem_Pool);
  };
  BINARY_NODE* Get_Data() { return &_data; }
  const BINARY_NODE* Get_Data() const { return &_data; }
};

template <class BINARY_NODE>
class BINARY_TREE {
  BINARY_TREE_NODE<BINARY_NODE> *_binary_tree_node;
  MEM_POOL *_pool;
public:
  BINARY_TREE(MEM_POOL *pool) { _pool = pool; _binary_tree_node = NULL; }
  BINARY_TREE_NODE<BINARY_NODE>* Enter(BINARY_NODE node) {
    typedef BINARY_TREE_NODE<BINARY_NODE> THIS_BINARY_TREE_NODE;
    if (!_binary_tree_node) {
      _binary_tree_node = CXX_NEW(THIS_BINARY_TREE_NODE(node),_pool);
      return(_binary_tree_node);
    }  else {
      return(_binary_tree_node->Enter(node,_pool));
    }
  }
  BINARY_TREE_NODE<BINARY_NODE>* Find(BINARY_NODE element) const { 
    if (!_binary_tree_node) {
      return(FALSE);
    } else {
      return(_binary_tree_node->Find(element));
   }
  }
  ~BINARY_TREE() { 
    MEM_POOL_Set_Default(_pool);
    if (_binary_tree_node) CXX_DELETE(_binary_tree_node,Default_Mem_Pool);
  }
};


#ifdef __GNUC__
// Implementation stuff included here because g++
// (rightly) doesn't do implicit .cxx file inclusion.

template <class BINARY_NODE>
BINARY_TREE_NODE<BINARY_NODE>* BINARY_TREE_NODE<BINARY_NODE>::
  Enter(BINARY_NODE node, MEM_POOL *pool) 
{
  //typedef BINARY_TREE_NODE<BINARY_NODE> THIS_NODE;
  //typedef BINARY_TREE_NODE<BINARY_NODE> *THIS_NODEP;
  //THIS_NODEP nodep = this;
  BINARY_TREE_NODE<BINARY_NODE> *nodep = this;
  BOOL found = FALSE;
  while (!found) {
    if (nodep->_data == node) {
      found = TRUE;
    } else if (node < nodep->_data) {
      if (!nodep->_left) {
	nodep->_left = CXX_NEW(BINARY_TREE_NODE<BINARY_NODE>(node),pool);
	found = TRUE;
      }
      nodep = nodep->_left;
    } else {
      if (!nodep->_right) {
	nodep->_right = CXX_NEW(BINARY_TREE_NODE<BINARY_NODE>(node),pool);
	found = TRUE;
      }
      nodep = nodep->_right;
    }
  }
  return nodep;
}
   

template <class BINARY_NODE>
BINARY_TREE_NODE<BINARY_NODE>* BINARY_TREE_NODE<BINARY_NODE>::
  Find(BINARY_NODE node) const
{
  //typedef BINARY_TREE_NODE<BINARY_NODE> *THIS_NODEP;
  BINARY_TREE_NODE<BINARY_NODE> *nodep = (BINARY_TREE_NODE<BINARY_NODE> *)this;
  while (1) {
    if (nodep->_data == node) {
      return nodep;
    } else if (node < nodep->_data) {
      if (nodep->_left) {
	nodep = nodep->_left;
      } else {
	return NULL;
      }
    } else {
      if (nodep->_right) {
	nodep = nodep->_right;
      } else {
	return NULL;
      }
    }
  }
}

#endif

#endif 

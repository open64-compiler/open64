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

/** $Revision: 1.2 $
*** $Date: 02/11/07 23:41:34-00:00 $
*** $Author: fchow@keyresearch.com $
*** $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.btree.cxx $
**/

#ifndef __GNUC__
// Implementation stuff included here because g++
// (rightly) doesn't do implicit .cxx file inclusion.

#include <sys/types.h>
#include "stdlib.h"
#include "btree.h"


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

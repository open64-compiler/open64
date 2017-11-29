/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include <sys/types.h>
#include <stdio.h>
#include "cxx_memory.h"
#include "errors.h"
#include "cxx_queue.h"

#ifndef __GNUC__
// Implementation stuff excluded here because g++
// (rightly) doesn't do implicit .cxx file inclusion.

template <class ITEM_TYPE>
QUEUE<ITEM_TYPE>::QUEUE (MEM_POOL *pool)
{
  _pool = pool;
  _length = 0;
  _first = _last = NULL;
}

template <class ITEM_TYPE>
void
QUEUE<ITEM_TYPE>::Add_Tail_Q(ITEM_TYPE item)
{
  QUEUE_NODE<ITEM_TYPE> *node;

  node = CXX_NEW (QUEUE_NODE<ITEM_TYPE>(item), _pool);
  if (0 == _length) 
    _first = _last = node;
  else {
    _last->Qnode_Next(node);
    _last = node;
  }
  _length++;
  return;
}

template<class ITEM_TYPE>
ITEM_TYPE
QUEUE<ITEM_TYPE>::Get_Q ()
{
  ITEM_TYPE item;
  QUEUE_NODE<ITEM_TYPE> *node;

  if (0 == _length) 
    return (ITEM_TYPE) 0;
  node = _first;
  item = node->Qnode_Item();
  /* Remove node from queue */
  _first = node->Qnode_Next();
  _length--;
  if (0 == _length) 
    _last = NULL;
  return item;
}

template<class ITEM_TYPE>
ITEM_TYPE
QUEUE<ITEM_TYPE>::Get_Tail_Q ()
{
  ITEM_TYPE item;
  QUEUE_NODE<ITEM_TYPE> *node;
  INT32 count, i;

  if (0 == _length)
    return (ITEM_TYPE) 0;
  else if (1 == _length) 
    return this->Get_Q();
  else {
    count = _length - 2;
    node = _first;
    for (i = 0; i < count; i++) {
      node = node->Qnode_Next();
    }
    assert (node->Qnode_Next() == _last);
    item = _last->Qnode_Item();
    node->Qnode_Next(NULL);
    _length--;
    _last = node;
    return item;
  }
}

template<class ITEM_TYPE>
INT32
QUEUE<ITEM_TYPE>::Index(ITEM_TYPE item, BOOL Insert_If_Absent)
{
  INT32 ret_val = 0;
  QUEUE_NODE<ITEM_TYPE> *node = _first;
  while (NULL != node) {
    if (node->Qnode_Item() == item)
      return ret_val;
    node = node->Qnode_Next();
    ret_val++;
  }
  FmtAssert (ret_val == _length, ("Inconsistency in queue index function"));
  if (Insert_If_Absent) {
    this->Add_Tail_Q (item);
    return ret_val;
  }
  else
    return -1;
}

#endif 

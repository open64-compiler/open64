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
// ====================================================================
// ====================================================================
//
// Module: cxx_base.cxx
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:34-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.cxx_base.cxx $
//
// Revision history:
//  18-SEP-94 shin - Original Version
//  30-SEP-94 dkchen - Added doubly linked list
//
// Description:
//
// ====================================================================
// ====================================================================
//

#include "defs.h"
#include "errors.h"
#include "erglob.h"	// for error messages

#include "cxx_base.h"

SLIST_NODE*
SLIST_NODE::Remove(SLIST_NODE *prev)
{
  Is_True(this, ("SLIST_NODE::Remove(): this is NULL"));

  if (prev != NULL) 
    prev->_next = _next;

  _next = NULL;
  return this;
}

INT32
SLIST_NODE::Len(void) const
{
  INT i;
  const SLIST_NODE *p;
  for (i = 0, p = this; p != NULL; i++, p = p->Next());
  return i;
}

INT
SLIST_NODE::Pos(SLIST_NODE *nd) const
{
  const SLIST_NODE *cur = this;
  INT pos = 0;
  while (cur) {
    if (nd == cur) 
      return pos;
    cur = cur->Next();
    pos++;
  }
  return -1;
}

SLIST::SLIST( SLIST_NODE *list)
{
  Is_True(this, ("SLIST::SLIST(): this is NULL"));

  _head = _tail = list;
  while (_tail && _tail->_next != NULL)
    _tail = _tail->_next;
}  

void
SLIST::Init( SLIST_NODE *list)
{
  Is_True(this, ("SLIST::Init(): this is NULL"));

  _head = _tail = list;
  while (_tail && _tail->_next != NULL)
    _tail = _tail->_next;
}  

BOOL
SLIST::Append( SLIST_NODE *nd, SLIST_NODE *od)
{
  Is_True(this, ("SLIST::Append(): this is NULL"));

  if (nd == NULL) return FALSE;

  if ( od == NULL && _head == NULL) {
    _head = _tail = nd;
    return TRUE;
  }

  // append nd after od
  if ( _tail == od ) {
    od->Insert_After(nd);
    _tail = nd;
    return TRUE;
  }
  // make sure od is in the list
  for (SLIST_NODE* tmp = _head; tmp != NULL; tmp = tmp->_next ) {
    if (tmp == od) {
      od->Insert_After(nd);
      return TRUE;
    }
  }

  return FALSE;
}

BOOL
SLIST::Prepend( SLIST_NODE *nd, SLIST_NODE *od )
{
  Is_True(this, ("SLIST::Prepend(): this is NULL"));

  if (nd == NULL) return FALSE;

  if (od == NULL && _head == NULL)
    _head = _tail = nd;

  // insert nd before od
  if ( _head == od ) {
    _head = od->Insert_Before(nd);
    return TRUE;
  }
  // make sure od is in the list
  for (SLIST_NODE* tmp = _head; tmp->_next != NULL; tmp = tmp->_next ) {
    if (tmp->_next == od) {
      tmp->_next = od->Insert_Before(nd);
      return TRUE;
    }
  }
  return FALSE;
}

void
SLIST::Append_List(SLIST *new_list)
{
  Is_True(this, ("SLIST::Append_List(): this is NULL"));

  // merge new_list into this
  SLIST_NODE* tmp = new_list->Head();
  SLIST_NODE* tmp2 = new_list->Tail();

  if (_head == NULL) {
    _head = tmp;
    _tail = tmp2;
  }
  else if (tmp2 != NULL) {
    _tail->_next = tmp;
    _tail = tmp2;
  }
}

void
SLIST::Prepend_List(SLIST *new_list)
{
  Is_True(this, ("SLIST::Prepend_List(): this is NULL"));

  // Prepend new_list into at the "_head" of the list
  SLIST_NODE* tmp = new_list->Head();
  SLIST_NODE* tmp2 = new_list->Tail();

  if (_head == NULL) {
    _head = tmp;
    _tail = tmp2;
  }
  else if (tmp2 != NULL) {
    tmp2->_next = _head;
    _head = tmp;
  }
}

SLIST_NODE*
SLIST::Remove_Headnode(void)
{
  Is_True(this, ("SLIST::Remove_Headnode() called with NULL this"));

  if (_head == NULL)
    return NULL;

  SLIST_NODE *old_head = _head;
  _head = _head->_next;
  if (_head == NULL)
    _tail = NULL;

  old_head->_next = NULL;
  return old_head;
}

SLIST_NODE*
SLIST::Remove(SLIST_NODE *prev, SLIST_NODE *cur)
{
  Is_True(this, ("SLIST::Remove() called with NULL this"));

  if (prev == NULL) {
    Is_True(cur == _head, ("cur is not head, but prev is null"));
    return Remove_Headnode();
  }
  else {
    Is_True(prev->_next == cur, ("prev does not point to cur"));
    prev->_next = cur->_next;
    if (prev->_next == NULL)
      _tail = prev;
  }

  cur->_next = NULL;
  return cur;
}

void
SLIST::Remove_node(SLIST_NODE *slist_node)
{
  Is_True(this, ("SLIST::Remove_node() called with NULL this"));
  SLIST_NODE *prev = NULL;

  // found previous node
  SLIST_NODE* tmp;
  for (tmp = Head(); tmp != NULL; tmp = tmp->_next) {
    if (tmp == slist_node) break;
    prev = tmp;
  }
  Is_True(tmp == slist_node, ("SLIST::Remove_node() cannot delete node from SLIST."));

  if (prev != NULL)
    prev->Set_Next(slist_node->Next());

  if (slist_node == Head())
    Set_Head(slist_node->Next());

  if (slist_node == Tail())
    Set_Tail(prev);

  slist_node->Set_Next(NULL);
}

INT32
SLIST::Len(void) const
{
  Is_True(this, ("SLIST::Len(): this is NULL"));

  SLIST_ITER lst_iter(_head);
  return lst_iter.Len();
}


SLIST_NODE *
SLIST_ITER::Nth(INT n)
{
  Is_True(this, ("SLIST_Iter::Nth(): this is NULL"));

  Len();

  if (n >= _len) return NULL;
  
  if (n < _idx) First();
  for (; n != _idx; Next());
  return _cur;
}

INT32
SLIST_ITER::Len(void)
{
  Is_True(this, ("SLIST_Iter::Len(): this is NULL"));

  if (_len == -1) { // initialize the field
    const SLIST_NODE *tmp;
    for (tmp = _head, _len = 0; tmp != NULL; tmp = tmp->Next())
      _len++;
  }
  return _len;
}

CHAIN_NODE*
CHAIN_NODE::Insert_After(CHAIN_NODE *nd)
{
  Is_True(this, ("this is NULL"));

  if (nd != NULL) {
    if (_next != NULL)
      _next->_prev=nd;
    nd->_next = _next;
    _next=nd;
    nd->_prev=this;
  }
  return nd;
}

CHAIN_NODE*
CHAIN_NODE::Insert_Before(CHAIN_NODE *nd)
{
  Is_True(this, ("this is NULL"));

  if (nd != NULL) {
    if (_prev != NULL)
      _prev->_next=nd;
    nd->_prev = _prev;
    _prev=nd;
    nd->_next=this;
  }
  return nd;
}

CHAIN_NODE*
CHAIN_NODE::Remove(void)
{
  Is_True(this, ("this is NULL"));

  if (_prev != NULL) 
    _prev->_next = _next;
  if (_next != NULL) 
    _next->_prev = _prev;

  _next = NULL;
  _prev = NULL;

  return this;
}

void
CHAIN::Append( CHAIN_NODE *nd )
{
  Is_True(this, ("this is NULL"));

  if (_head == NULL)
    _head = _tail = nd;
  else
    _tail = _tail->Insert_After(nd);
}

void
CHAIN::Prepend( CHAIN_NODE *nd )
{
  Is_True(this, ("this is NULL"));

  if (_head == NULL)
    _head = _tail = nd;
  else {
    _head = _head->Insert_Before(nd);
  }
}

void
CHAIN::Insert_After(CHAIN_NODE *nd, CHAIN_NODE *after_nd)
{ 
  Is_True(this, ("this is NULL"));

  if (_head == NULL)
    _head = _tail = nd;
  else if (after_nd == _tail)
    _tail = _tail->Insert_After(nd);
  else
    after_nd->Insert_After(nd);
}

void
CHAIN::Insert_Before(CHAIN_NODE *nd, CHAIN_NODE *before_nd)
{
  Is_True(this, ("this is NULL"));

  if (_head == NULL)
    _head = _tail = nd;
  else if (before_nd == _head)
    _head = _head->Insert_Before(nd);
  else
    before_nd->Insert_Before(nd);
}

BOOL
CHAIN::Is_Member( CHAIN_NODE *nd ) const
{
  Is_True(this, ("this is NULL"));

  if (nd == NULL)
    return FALSE;
  
  for (CHAIN_NODE* tmp = _head; tmp != NULL; tmp = tmp->_next ) 
    if (tmp == nd)
      return TRUE;

  return FALSE;
}

void
CHAIN::Append_List(CHAIN *new_list)
{
  Is_True(this, ("this is NULL"));

  if (new_list == NULL)
    return;

  if (_head == NULL) {
    Init(new_list);
    return;
  }

  _tail->_next = new_list->_head;
  _tail->_next->_prev = _tail;

  _tail = new_list->_tail;
}

void
CHAIN::Prepend_List(CHAIN *new_list)
{
  Is_True(this, ("this is NULL"));

  if (new_list == NULL)
    return;

  if (_head == NULL) {
    Init(new_list);
    return;
  }

  _head->_prev = new_list->_tail;
  _head->_prev->_next = _head;

  _head = new_list->_head;

}

// remove the element from the chain.  No check is made to see if it
// is in the chain.
void
CHAIN::Remove( CHAIN_NODE *nd )
{
  Is_True(this, ("this is NULL"));

  if (nd == NULL)
    return;

  if (nd == _head)
    _head = _head->_next;

  if (nd == _tail)
    _tail = _tail->_prev;

  nd->Remove();
}

CHAIN_NODE*
CHAIN::Remove_Head(void)
{
  Is_True(this, ("this is NULL"));

  CHAIN_NODE* nd;

  if (_head == NULL)
    nd = NULL;
  else if (_head == _tail) {
    nd = _head->Remove();
    _head = _tail = NULL;
  }
  else {
    _head = _head->_next;
    nd = _head->_prev->Remove();
    _head->_prev = NULL;
  }
  return nd;
}

CHAIN_NODE*
CHAIN::Remove_Tail(void)
{
  Is_True(this, ("this is NULL"));

  CHAIN_NODE* nd;

  if (_tail == NULL)
    nd = NULL;
  else if (_head == _tail) {
    nd = _head->Remove();
    _head=_tail=NULL;
  }
  else {
    _tail = _tail->_prev;
    nd = _tail->_next->Remove();
    _tail->_next=NULL;
  }
  return nd;
}

INT32
CHAIN::Len(void) const
{
  Is_True(this, ("this is NULL"));
  CHAIN_ITER list_iter((CHAIN*)this);
  return list_iter.Len();
}

CHAIN_NODE *
CHAIN_ITER::First(void)
{
  Is_True(this, ("this is NULL"));
  _cur=this->_list->Head();
  _idx = 0;

  return _cur;
}

CHAIN_NODE *
CHAIN_ITER::Last(void)
{
  Is_True(this, ("this is NULL"));
  _idx = Len();
  _cur=this->_list->Tail();

  return _cur;
}

CHAIN_NODE *
CHAIN_ITER::Next(void)
{
  Is_True(this, ("this is NULL"));
  if (_cur != NULL) {
    _cur = _cur->Next();
    _idx++;
  }
  return _cur;
}

CHAIN_NODE *
CHAIN_ITER::Prev(void)
{
  Is_True(this, ("this is NULL"));
  if (_cur != NULL) {
    _cur = _cur->Prev();
    _idx--;
  }
  return _cur;
}

CHAIN_NODE *
CHAIN_ITER::Nth(INT n)
{
  Is_True(this, ("this is NULL"));

  Len();

  if (n >= _len) return NULL;

  _cur=_list->Head();
  _idx=0;
  while (_idx !=n){
    _cur=_cur->_next;
    _idx++;
  }
  return _cur;
}

CHAIN_NODE *
CHAIN_ITER::Last_Nth(INT n)
{
  Is_True(this, ("this is NULL"));

  Len();

  if (n >= _len) return NULL;

  _cur=_list->Tail();
  _idx=_len-1;
  while (_idx != _len-1-n){
    _cur=_cur->_prev;
    _idx--;
  }
  return _cur;
}

INT32
CHAIN_ITER::Len(void)
{
  Is_True(this, ("this is NULL"));

  if (_len == -1) { // initialize the field
    _len=0;
    _cur=_list->Head();
    while (_cur){
      _cur=_cur->_next;
      _len++;
    }
  }
  return _len;
}

// ==================================================================
// ==================================================================
//
// Circularly Linked Lists
//
// ==================================================================
// ==================================================================

// ==================================================================
// CLIST_NODE::Find_Next just searches the circularly-linked list for
// the node whose '_next' field points to 'this'
// ==================================================================

CLIST_NODE *
CLIST_NODE::Find_Next( void ) const
{
  for ( const CLIST_NODE *cn = this; cn != NULL; cn = cn->Next() ) {
    if ( cn->Next() == this )
      return (CLIST_NODE *)cn;	// need to cast a const * to non-const
  }

  ErrMsg( EC_Unimplemented, "CLIST_NODE::Find_Next: invalid list" );
  return NULL;
}

// ==================================================================
// CLIST_NODE::Insert_Before
//
// Inserts a node "nd" before "this" node.  Please NOTE that this 
// method is inefficient time-wise because of the need to search for 
// the node that has "this" as its "next" node.
// ==================================================================

CLIST_NODE *
CLIST_NODE::Insert_Before( CLIST_NODE *nd )
{
  if ( this == NULL ) {
    nd->Set_Next(nd);	// circ list contains this single node
    return nd;
  }
  else {
    // need to go through the list to find which one points to 'this'
    CLIST_NODE *next_is_this = Find_Next();

    nd->Set_Next(this);
    next_is_this->Set_Next(nd);
    return nd;
  }
}

// ==================================================================
// CLIST_NODE::Remove
//
// Removes "this" node from the list, returning it
// ==================================================================

CLIST_NODE*
CLIST_NODE::Remove( CLIST_NODE *prev )
{
  if ( prev != NULL )
    prev->Set_Next(this->Next());
  _next = NULL;
  return this;
}


// ==================================================================
// CLIST_NODE::Len
//
// Returns the number of elements in the list
// ==================================================================

INT32
CLIST_NODE::Len( void ) const
{
  if ( this == NULL ) {
    return 0;
  }
  else {
    INT32 len = 1;
    for ( const CLIST_NODE *cn = this->Next(); 
	  cn != this; 
	  cn = cn->Next() )
    {
      len++;
    }

    return len;
  }
}

// ==================================================================
// CLIST::Init
//
// Initialize a CLIST by finding the head/tail of the given list
// ==================================================================

void
CLIST::Init( CLIST_NODE *list )
{
  if ( this == NULL )
    return;

  _head = list;
  for ( _tail = list; 
	_tail != NULL && _tail->Next() != _head; 
	_tail = _tail->Next() )
    ; // do nothing, except loop until we find the head

  FmtAssert( _tail != NULL || list == NULL,
    ("CLIST::Init: Invalid list") );
}

// ==================================================================
// CLIST::Append
//
// Appends node "nd" to the list so it is at the "end" of the list, and
// therefore precedes the head
// ==================================================================

void
CLIST::Append( CLIST_NODE *nd )
{
  if ( this == NULL || nd == NULL ) 
    return;

  if (_head == NULL) {
    Init( nd );
  }
  else {
    _tail->Insert_After(nd);
    _tail = _tail->Next();
  }
}

// ==================================================================
// CLIST::Append
//
// Appends node "nd" after the node "od" in the list
// ==================================================================

BOOL
CLIST::Append( CLIST_NODE *nd, CLIST_NODE *od)
{
  if (this == NULL) return FALSE;
  if (nd == NULL) return FALSE;

  if ( od == NULL && _head == NULL) {
    _head = _tail = nd;
    return TRUE;
  }

  // append nd after od
  if ( _tail == od ) {
    od->Insert_After(nd);
    _tail = nd;
    return TRUE;
  }

  // make sure od is in the list
  for ( CLIST_NODE* tmp = _tail->Next(); 
	tmp != NULL && tmp != _tail; 
	tmp = tmp->Next() )
  {
    if (tmp == od) {
      od->Insert_After(nd);
      return TRUE;
    }
  }

  return FALSE;
}

// ==================================================================
// CLIST::Prepend
//
// Inserts node "nd" at the "_head" of the list
// ==================================================================

void
CLIST::Prepend( CLIST_NODE *nd )
{
  if (this == NULL) return;
  if (nd == NULL) return;

  // insert nd to beginning of list
  if (_head == NULL)
    _head = _tail = nd;
  else {
    _tail->Insert_After(nd);
    _head = _tail->Next();
  }
}

// ==================================================================
// CLIST::Prepend
//
// Inserts node "nd" before the node "od" in the list
// ==================================================================

BOOL
CLIST::Prepend( CLIST_NODE *nd, CLIST_NODE *od )
{
  if (this == NULL) return FALSE;
  if (nd == NULL) return FALSE;

  if ( od == NULL && _head == NULL) {
    _head = _tail = nd;
    return TRUE;
  }

  // prepend nd before od
  if ( _head == od ) {
    _tail->Insert_After(nd);	// can insert after tail which is
    _head = nd;			//   the same as before head
    return TRUE;
  }

  // make sure od is in the list
  CLIST_NODE *precedes = _head;
  for ( CLIST_NODE* tmp = _head->Next(); 
	tmp != NULL && tmp != _head; 
	tmp = tmp->Next(), precedes = precedes->Next() )
  {
    if (tmp == od) {
      precedes->Insert_After(nd);	// insert after one that
      return TRUE;			// precedes it
    }
  }

  return FALSE;
}

// ==================================================================
// CLIST::Append_List
//
// Appends the "new_list" at the "_tail" of the list
// ==================================================================

void
CLIST::Append_List(CLIST *new_list)
{
  if (this == NULL) return;

  // merge new_list into this
  CLIST_NODE* nlh = new_list->Head();
  CLIST_NODE* nlt = new_list->Tail();

  if ( nlh == NULL ) {
    return;		// nothing to append
  }
  else if (_head == NULL) {
    _head = nlh;	// the new list is the only list
    _tail = nlt;
  }
  else {
    _tail->Set_Next(nlh);	// need to append to existing list
    nlt->Set_Next(_head);
    _tail = nlt;
  }
}

// ==================================================================
// CLIST::Prepend_List
//
// Prepends the "new_list" at the "_head" of the list
// ==================================================================

void
CLIST::Prepend_List(CLIST *new_list)
{
  if (this == NULL) return;

  // Prepend new_list into at the "_head" of the list
  CLIST_NODE* nlh = new_list->Head();
  CLIST_NODE* nlt = new_list->Tail();

  if ( nlh == NULL ) {
    return;		// nothing to prepend
  }
  else if (_head == NULL) {
    _head = nlh;	// the new list is the only list
    _tail = nlt;
  }
  else {
    nlt->Set_Next(_head);	// need to prepend to existing list
    _tail->Set_Next(nlh);
    _head = nlh;
  }
}

// ==================================================================
// CLIST::Remove_Headnode
//
// Removes the head of the list, if any, returning it
// ==================================================================

CLIST_NODE*
CLIST::Remove_Headnode(void)
{
  CLIST_NODE* rv;

  if (this == NULL)
    return NULL;

  rv = _head;
  if (_head != NULL) {
    if ( _head == _tail ) {
      _head = _tail = NULL;
    }
    else {
      _head = _head->Next();
      _tail->Set_Next(_head);
    }
  }
  rv->_next = NULL;
  return rv;
}

// ==================================================================
// CLIST::Len
//
// Returns the number of items in the list
// ==================================================================

INT32
CLIST::Len(void) const
{
  if (this == NULL) {
    return 0;
  }
  else {
    return _head->Len();
  }
}

// ==================================================================
// CLIST_ITER::First
//
// Gets the first element "_head" and set "_cur" pointer
// ==================================================================

CLIST_NODE * 
CLIST_ITER::First(void)
{
  if (this == NULL) return NULL;

  _cur = _head;
  _saw_head = FALSE;

  return _cur;
}

// ==================================================================
// CLIST_ITER::Next
//
// Gets the next element and bumps the current pointer
// ==================================================================

CLIST_NODE * 
CLIST_ITER::Next(void)
{
  if (this == NULL) return NULL;

  if ( _cur != NULL )
    _cur = _cur->Next();

  _saw_head = TRUE;

  return _cur;
}

// ==================================================================
// CLIST_ITER::Len
//
// Gets the number of elements in the list
// ==================================================================

INT32
CLIST_ITER::Len(void) const
{
  if (this == NULL || _head == NULL) 
    return 0;

  return _head->Len();
}


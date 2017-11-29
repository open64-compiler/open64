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
/* ====================================================================
 * ====================================================================
 *
 * Module: cxx_base.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:35-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.cxx_base.h $
 *
 * Revision history:
 *  18-SEP-94 shin - Original Version
 *  30-SEP-94 dkchen - Added doubly linked list
 *
 * Description:
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef cxx_base_INCLUDED
#define cxx_base_INCLUDED

#include "errors.h"

#ifdef _KEEP_RCS_ID
static char *cxx_basercs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.cxx_base.h $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

// NOTE:  FOLLOW THESE CODING CONVENTIONS:
//
//        All classes defined in this file are base classes.  Please
//        follow these conventions:
//
//        To avoid undesirable effect of default assignment operator
//        and default copy constructor, please create a "private"
//        operator= and copy constructor for all classes.

// Singly Linked List
//
//	Here's how to make and use a singly linked list.  Suppose you would
//	like to use a list containing pairs of integers.  You would do this:
//
//	class II_NODE : public SLIST_NODE {
//        DECLARE_SLIST_NODE_CLASS(II_NODE);
//       public:
//	   INT int1;
//	   INT int2;
//	   II_NODE(INT i1, INT i2) : int1(i1), int2(i2) {}
//	   ~II_NODE() {}
//       };
//
//	class II_LIST : public SLIST {
//	  DECLARE_SLIST_CLASS(II_LIST, II_NODE);
//	 public:
//	   // --> YOU MUST SUPPLY THIS, AND YOU MUST SUPPLY IT YOURSELF <--
//         ~II_LIST();
//	};
//
//	class II_ITER : public SLIST_ITER {
//	  DECLARE_SLIST_ITER_CLASS(II_ITER, II_NODE, II_LIST);
//	};
//
//	class II_CONST_ITER : public SLIST_ITER {
//	  DECLARE_SLIST_CONST_ITER_CLASS(II_CONST_ITER, II_NODE, II_LIST);
//	};
//
// Doing that supplies you with the following public functions
// and classes.  Here I'm using II_NODE, etc as an example ...
// obviously you will have different names for your classes.  The
// member functions retain their names.
//
//
//
// II_NODE: defines the basic element of singly linked list.  It
//             contains the pointer to the next node.
//
//	Exported Functions:
//
//          II_NODE(void)
//
//		Construct a singly linked list node, initialize next
//		field to NULL. This does not initialize any of your
//		fields!!! 
//
//	    II_NODE		*Next()
//	    const II_NODE	*Next() const
//
//	        The item on the list following this one
//
//	    void Insert_After(II_NODE *nd)
//
//              Inserts a node "nd" after "this" node.
//
//          II_NODE* Insert_Before(II_NODE *nd)
//
//              Inserts a node "nd" before "this" node.
//		WARNING: does not fix the previous pointer's pointer to this,
//		so must be used with extreme caution!!
//
//	    II_NODE*	Remove(II_NODE *prev)
//
//		Removes the "this" node from the list, returning it.  Note
//		that the list's head and tail pointers are not updated,
//		so this must be used with extreme caution.  TODO Michael
//		Wolf says get rid of this function, or at least change its
//		name so that people don't use it accidentally.
//
//
//	    void	Set_Next(II_NODE *)
//
//	       Reset's the next pointer.  Be careful.
//
//	    INT		Len() const
//
//	       How many items are on the list from this out, including this.
//
//	    INT		Pos(II_NODE *) const
//
//             Returns the position of the parameter in the linked list.
//             0 is the first position, and so on.
//             Returns -1 if the parameter doesn't belong to the list.
//
//  II_LIST is the container class is that holds a list of II_NODES.
//  It maintains the head and the tail of the singly linked list.
//
//       Exported Functions:
//
//         II_LIST()
//         II_LIST(II_NODE*)
//
//             Construct a an empty list or a list containing the one element.
//
//		NOTE THAT NO DESTRUCTORS ARE SUPPLIED.  IN ORDER TO DELETE
//		DATA, YOU MUST SUPPLY YOUR OWN!  For example, you might want
//		have the II_LIST contain a memory pool from which all the
//		II_NODES are allocated, and then the destructor would iterate
//		down the list deleting nodes.  Another possibility is to store
//		a memory pool with each node.  Since there are so many choices,
//		a destructor cannot be supplied here.
//
//         void Append( II_NODE *nd )
//         BOOL SLIST::Append( SLIST_NODE *nd, SLIST_NODE *od)
//
//             Appends node "nd" to the end of the list, or after od.
//             Returns TRUE if the append is successful.
//
//         void Prepend( II_NODE *nd )
//         BOOL Prepend( II_NODE *nd, II_NODE *od )
//
//              Inserts node "nd" at the head of the list, or
//		before the node "od" in the list.  Returns TRUE if the
//              insertion is successful.
//
//         void Append_List(II_LIST *new_list)
//
//             Appends the "new_list" to the end of this one.
//
//         void Prepend_List(II_LIST *new_list)
//
//             Prepends the "new_list" at the front of this one.
//
//         II_NODE* Remove_Headnode(void)
//
//             Removes the current head node from this list, returning it.
//
//         II_NODE* Remove(II_NODE *prev, II_NODE *cur)
//
//             Removes cur from the list, returning it.  prev must point to
//		cur.  If prev is NULL, this has same effect as
//		Remove_Headnode() -- and cur must be the head.  _head and _tail
//		of the list are updated appropriately.
//
//         II_NODE *Head(void)
//         const II_NODE *Head(void) const
//
//             Returns the first node of the list
//
//         II_NODE *Tail(void)
//         const II_NODE *Tail(void) const
//
//             Returns the last node of the list
//
//         BOOL Is_Empty(void) const
//
//             Returns TRUE if head node is NULL, or FALSE otherwise
//
//         INT32 Len(void) const
//
//             Returns the length of the list in the container
//
//         INT Pos(SLIST_NODE *) const
//        
//             Returns the position of the parameter in the linked list.
//             0 is the first position, and so on.
//             Returns -1 if the parameter doesn't belong to the list.
//
//
// II_ITER: the iterator class is typically used to iterate through
//             a singly linked list that is based on the base class
//             SLIST_NODE, to perform certain operation.  Iteration does not
//             change the list content.
//
// II_CONST_ITER: the same as iter, but returns constant pointers to nodes,
//	and is initialized with constant pointers.  The interfaces for the
//	two are identical, except for those differences.  [const] is used
//	to signify where II_ITER does not have a const but II_CONST_ITER does.
//
//       Exported Functions:
//
//         II_ITER(void)
//         II_CONST_ITER(void)
//
//             Construct a singly linked list iter but don't associate
//             it with a list.
//
//         II_ITER(II_NODE *nd)
//         II_CONST_ITER(const II_NODE *nd)
//
//             Construct so that when First is called, it will return nd and
//	       continue from there.
//
//         II_ITER(SLIST *sl)
//         II_CONST_ITER(const SLIST *sl)
//
//             Construct so that when First is called, it will return the
//	       head of the list and continue from there.
//
//         ~II_ITER()
//         ~II_CONST_ITER()
//
//             Destruct the iterator.
//
//         void Init([const] SLIST_NODE *nd)
//         void Init([const] SLIST_NODE *nd)
//
//		Reinitialize.
//
//         void Set([const] SLIST_NODE *nd)
//
//		Sets the current to nd, len/idx to -1.
//
//         void SLIST_ITER::Clear(void)
//
//             Set head/tail to NULL, len/idx to -1.
//
//         [const] II_NODE* Nth(INT n)
//
//             Gets the n-th element in the list
//
//         [const] II_NODE* First(void)
//
//             Gets the first element "_head" and set "_cur" pointer
//
//         [const] II_NODE* Next(void)
//
//             Gets the _next element and bump the "_cur" pointer
//
//         [const] II_NODE* Peek_Next(void)
//
//             Gets the _next element in the list, doesn't change the iterator
//
//         [const] II_NODE* Head(void)
//
//             Gets the "_head" in the list, doesn't change the iterator
//
//         [const] II_NODE* Cur(void)
//
//             Gets the "_cur" element in the list
//
//         BOOL Is_Empty(void)
//
//             Returns TRUE is the current element, "_cur", a NULL pointer.
//
// Example usage of iterator:
// II_ITER  foo_iter(list);
// for (II_NODE *n=foo_iter.First(); !foo_iter.Is_Empty(); n=foo_iter.Next()) {
//   // do whatever
// }
//
// Substitute CHAIN for SLIST and you get a doubly linked list, with the
// following additional functionality:
//
// CC_NODE: defines the basic element of doubly linked list, with the
//          additional functions than II_NODE(SLIST):
//
//	Exported Functions:
//
//          CC_NODE*    Prev(void)
//
//              Gets the _prev element of this one
//
//          void        Set_Prev(CC_NODE *)
//
//              Resets the _prev pointer.  Be careful.
//
// CC_LIST is the container class that holds a chain of CC_NODES.
// It maintains the head and tail of the doubly linked list.  In
// addition to what II_LIST provide, here are the extra:
//
//	Exported Functions:
//
//          void        Insert_After(CC_NODE *nd, CC_NODE *after_nd)
//
//              Insert the node 'nd' after the node 'after_nd'
//
//          void        Insert_Before(CC_NODE *nd, CC_NODE *before_nd)
//
//              Insert the node 'nd' before the node 'before_nd'
//
//          void        Remove(CC_NODE *nd)
//
//              Remove the node 'nd' from the chain.  NOTE: different
//              interface from the II_LIST.
//
// CC_ITER: the iterator class is typically used to iterate through
//             a doubly linked list that is based on the base class
//             CHAIN_NODE, to perform certain operation.  Iteration does not
//             change the list content.
//          Becaue the nature of chain, the chain iterator allows use
//          to traverse the list backward by using the set of
//          functions: (Last(), Is_Empty_Reverse(), Prev())
//
//          The additional function compare with II_ITER(SLIST_ITER):
//
//	Exported Functions:
//
//          CC_NODE*    Last(void)
//
//              Gets the last element of this list and set the index=len
//
//          CC_NODE*    Prev(void)
//
//              Gets the previous element of this list and decrment
//              the current pointer.
//
//          CC_NODE*    Last_Nth(INT n)
//
//              Gets the last nth element of this list and set the
//              pointer/index accordingly.
//
//          BOOL        Is_Empty_Reverse(void)
//
//              The mirror function for traversing the list backward.
//

class SLIST_NODE {
friend class SLIST;
friend class SLIST_ITER;
private:
  SLIST_NODE *_next;   // point to the next node in the singly linked list

  SLIST_NODE& operator= (const SLIST_NODE& sl);
  SLIST_NODE(const SLIST_NODE&);

protected:
  SLIST_NODE(void)                       { _next = NULL; }
  ~SLIST_NODE(void)                      {}

  void        Insert_After(SLIST_NODE *nd)  { nd->_next = _next; _next = nd; }
  SLIST_NODE *Insert_Before(SLIST_NODE *nd) { nd->_next = this; return nd; }
  SLIST_NODE *Remove(SLIST_NODE *prev);
  void        Set_Next(SLIST_NODE *n)       { _next = n; }
  INT32       Len(void) const;
  INT         Pos(SLIST_NODE *) const;
public:
  SLIST_NODE *Next(void) const              { return _next;}
};

class SLIST {
private:
  SLIST_NODE *_head;
  SLIST_NODE *_tail;

  SLIST& operator= (const SLIST& sl);
  SLIST(const SLIST&);

protected:
  SLIST(void)                  { _head = _tail = NULL; }
public:
  SLIST(SLIST_NODE *list);
  ~SLIST(void)                 {}

  void Set_Head(SLIST_NODE *h)      { _head = h; }
  void Set_Tail(SLIST_NODE *t)      { _tail = t; }

  void        Init(SLIST_NODE *list);   // same as constructor
  void        Init_Head(SLIST_NODE *list)      { _head = list; _tail = NULL; }
  void        Clear(void)                      { _head = _tail = NULL; }

  BOOL        Append( SLIST_NODE *nd, SLIST_NODE *od);
  BOOL        Prepend( SLIST_NODE *nd, SLIST_NODE *od );
  void        Append_List(SLIST *new_list);
  void        Prepend_List(SLIST *new_list);
  SLIST_NODE *Remove_Headnode(void);
  SLIST_NODE *Remove(SLIST_NODE *prev, SLIST_NODE *cur);
  void        Remove_node(SLIST_NODE *slist_node);

  SLIST_NODE		*Head(void)            { return _head; }
  const SLIST_NODE	*Head(void) const      { return _head; }
  SLIST_NODE		*Tail(void)            { return _tail; }
  const SLIST_NODE	*Tail(void) const      { return _tail; }
  BOOL        Is_Empty(void) const             { return _head == NULL; }
  INT32       Len(void) const;
  INT         Pos(SLIST_NODE *nd) const        { return _head->Pos(nd); }

  void Append( SLIST_NODE *nd )
    {
      if (nd == NULL) return;
      if (_head == NULL)
	_head = _tail = nd;
      else {
	_tail->Insert_After(nd);
	_tail = _tail->Next();
      }
    }
  
  void Prepend( SLIST_NODE *nd )
    {
      if (nd == NULL) return;
      // insert nd to beginning of list
      if (_head == NULL)
	_head = _tail = nd;
      else {
	_head = _head->Insert_Before(nd);
      }
    }

};

class SLIST_ITER {
private:
  SLIST_NODE  *_head;
  SLIST_NODE  *_cur;
  mINT16       _len;
  mINT16       _idx;          // simulate indexing, default to -1

  SLIST_ITER& operator= (const SLIST_ITER& sl);
  SLIST_ITER(const SLIST_ITER&);

protected:
  SLIST_ITER(void) { _head = NULL; _cur = NULL; _len = -1; _idx = -1;}
  void Set_Cur(SLIST_NODE* cur) { _cur = cur;}
  void Set_Idx(mINT16 idx) {_idx = idx;}

public:
  SLIST_ITER(SLIST_NODE *nd)
                   { _head = nd; _cur = _head; _len = -1; _idx = -1; }
  SLIST_ITER(SLIST *sl)
		   { _head=sl->Head(); _cur=_head; _len=-1; _idx=-1; }
  ~SLIST_ITER(void) {}

  void         Init(SLIST_NODE *nd) { _head = nd; _cur = _head;}
  void         Init(SLIST *sl)      { _head = (sl ? sl->Head() : NULL);
							_cur = _head;}
  void         Init(void)           { _head = NULL; _cur = _head;}
  void         Clear(void) { _head = NULL; _cur = NULL; _len = -1; _idx = -1;}
  void         Set(SLIST_NODE *nd)  {_cur = nd; _idx = -1;}
  SLIST_NODE  *First(void) {  // get the first element, and reset the pointer
    if (this == NULL)
      return NULL;
    if (_head)
      _cur = _head;
    else
      _cur = NULL;
    _idx = 0;
    return _cur;
  }
  SLIST_NODE  *Next(void) {   // get the next element, and bump the pointer.
    if (this == NULL)
      return NULL;
    if (_cur != NULL) {
      _cur = _cur->Next();
      _idx++;
    }
    return _cur;
  }
  SLIST_NODE  *Nth(INT n);// get the nth element

  SLIST_NODE  *Peek_Next(void) const {return _cur->Next();} 
  SLIST_NODE  *Head(void) const		{return _head;}
  SLIST_NODE  *Cur(void) const		{return _cur;}
  INT          Idx(void) const		{return _idx;}
  INT32        Len(void);		// get the length of the list
  BOOL         Is_Empty(void) const	{ return _cur == NULL; }
};

// DECLARE_SLIST_NODE_CLASS: a macro to define the body of a derived
//                           class "NAME_LIST" of SLIST_NODE
//                   Example:
//                           class NAME_LIST : public SLIST_NODE {
//                           DECLARE_SLIST_NODE_CLASS( NAME_LIST )
//                           ~NAME_LIST(void);
//                           };
//                   Note:   Create your own destructor!

#define DECLARE_SLIST_NODE_CLASS( NAME_NODE )			      \
								      \
public:								      \
  NAME_NODE *Next(void) {					      \
    return (NAME_NODE *) SLIST_NODE::Next();                          \
  }								      \
  const NAME_NODE *Next(void) const {				      \
    return (NAME_NODE *) SLIST_NODE::Next();                          \
  }								      \
  void Insert_After(NAME_NODE *nd) { 				      \
    SLIST_NODE::Insert_After(nd);  				      \
  }								      \
  void Insert_Before(NAME_NODE *nd) {				      \
    SLIST_NODE::Insert_Before(nd); 				      \
  }								      \
  NAME_NODE *Remove(NAME_NODE *prev) { 				      \
    return (NAME_NODE*) SLIST_NODE::Remove(prev);	     	      \
  }								      \
  void Set_Next(NAME_NODE *nd) { 				      \
    SLIST_NODE::Set_Next(nd); 					      \
  }								      \
  INT32  Len(void) const    { return SLIST_NODE::Len(); }             \
  INT    Pos(NAME_NODE *od) { return SLIST_NODE::Pos(od); }           \

// DECLARE_SLIST_CLASS: a macro to define the body of a derived
//                      class "NAME_LIST" of SLIST.
//              Example:
//                      class NAME_LIST : public SLIST {
//                      DECLARE_SLIST_CLASS( NAME_LIST, NODE_T )
//                      };

#define DECLARE_SLIST_CLASS( NAME_LIST, NAME_NODE)		 	\
public:			                                                \
  typedef NAME_NODE CONTAINER_NODE; 				        \
  NAME_LIST(NAME_NODE *nd) { SLIST::Init(nd); } 			\
  NAME_LIST() : SLIST() {}			 			\
  void Append(NAME_NODE *nd) { SLIST::Append(nd); }			\
  BOOL Append(NAME_NODE *nd, NAME_NODE *od) { 				\
    return SLIST::Append(nd, od);					\
  }									\
  void Prepend(NAME_NODE *nd) { SLIST::Prepend(nd); }			\
  BOOL Prepend(NAME_NODE *nd, NAME_NODE *od) { 				\
    return SLIST::Prepend(nd, od);					\
  }									\
  void Append_List(NAME_LIST *nl) { SLIST::Append_List(nl); }		\
  void Prepend_List(NAME_LIST *nl) { SLIST::Prepend_List(nl); }		\
  NAME_NODE *Remove_Headnode(void) {					\
    return (NAME_NODE*) SLIST::Remove_Headnode();			\
  }	  								\
  NAME_NODE *Remove(NAME_NODE *prev, NAME_NODE *cur) {			\
    return (NAME_NODE*) SLIST::Remove(prev, cur);			\
  }	  								\
  NAME_NODE *Head(void) { return (NAME_NODE *) SLIST::Head(); }		\
  const NAME_NODE *Head(void) const 					\
		{ return (const NAME_NODE *) SLIST::Head(); }		\
  NAME_NODE *Tail(void) { return (NAME_NODE *) SLIST::Tail(); }		\
  const NAME_NODE *Tail(void) const					\
		{ return (const NAME_NODE *) SLIST::Tail(); }		\
  BOOL       Is_Empty(void) const { return SLIST::Is_Empty(); }		\
  INT32      Len(void) const { return SLIST::Len(); }			\
  INT        Pos(NAME_NODE *od) { return SLIST::Pos(od); }              \


// DECLARE_SLIST_ITER_CLASS: a macro to define the body of a derived
//                           class "NAME_LIST" of SLIST_ITER
//                   Example:
//                           class NAME_LIST : public SLIST_ITER {
//                           DECLARE_SLIST_ITER_CLASS( NAME_LIST, NODE_T )
//                           };

#define DECLARE_SLIST_ITER_CLASS( NAME_ITER, NAME_NODE, NAME_LIST)	\
public:									\
  NAME_ITER(NAME_NODE *nd) { SLIST_ITER::Init(nd); } 			\
  NAME_ITER(NAME_LIST *nl) { SLIST_ITER::Init(nl); }     		\
  NAME_ITER(void)          { SLIST_ITER::Init();   }                    \
  void Init(NAME_NODE *nd) { SLIST_ITER::Init(nd); } 			\
  void Init(NAME_LIST *nl) { SLIST_ITER::Init(nl); }                    \
  void Set(NAME_NODE *nd) { SLIST_ITER::Set(nd); } 			\
  NAME_NODE *First(void) { return (NAME_NODE *) SLIST_ITER::First(); }	\
  NAME_NODE *Next(void) { return (NAME_NODE *) SLIST_ITER::Next(); }	\
  NAME_NODE *Nth(INT n) { return (NAME_NODE *) SLIST_ITER::Nth(n); }	\
  NAME_NODE *Peek_Next(void) { return (NAME_NODE *) SLIST_ITER::Peek_Next(); }\
  NAME_NODE *Head(void) { return (NAME_NODE *) SLIST_ITER::Head(); }	\
  NAME_NODE *Cur(void) { return (NAME_NODE *) SLIST_ITER::Cur(); }	\
  BOOL Is_Empty(void) { return SLIST_ITER::Is_Empty(); }   		\

#define DECLARE_SLIST_CONST_ITER_CLASS( NAME_ITER, NAME_NODE, NAME_LIST) \
public:									\
  NAME_ITER(const NAME_NODE *nd) { SLIST_ITER::Init((NAME_NODE*)nd); } \
  NAME_ITER(const NAME_LIST *nl) { SLIST_ITER::Init((NAME_LIST*)nl); } \
  NAME_ITER(void)          { SLIST_ITER::Init();   }                    \
  void Init(NAME_NODE *nd) { SLIST_ITER::Init(nd); }			\
  void Init(NAME_LIST *nl) { SLIST_ITER::Init(nl); }			\
  void Set(NAME_NODE *nd) { SLIST_ITER::Set(nd); } 			\
  const NAME_NODE *First(void) { return (NAME_NODE *) SLIST_ITER::First(); } \
  const NAME_NODE *Next(void) { return (NAME_NODE *) SLIST_ITER::Next(); }   \
  const NAME_NODE *Nth(INT n) { return (NAME_NODE *) SLIST_ITER::Nth(n); }   \
  const NAME_NODE *Peek_Next(void) { return (NAME_NODE *) SLIST_ITER::Peek_Next(); }   \
  const NAME_NODE *Head(void) { return (NAME_NODE *) SLIST_ITER::Head(); }   \
  const NAME_NODE *Cur(void) { return (NAME_NODE *) SLIST_ITER::Cur(); }     \
  BOOL Is_Empty(void) { return SLIST_ITER::Is_Empty(); }   		     \


// CHAIN_NODE: defines the basic element of doubly linked list.  It
//             contains the pointer to the prev and the next node.
//
//       Exported Functions:
//
//         CHAIN_NODE::CHAIN_NODE(void)
//
//             Construct a doubly linked list node, 
//             initialize next field to NULL.
//
//         CHAIN_NODE::~CHAIN_NODE(void)
//
//             Destruct a doubly linked list node, 
//             initialize next field to NULL.
//
//         CHAIN_NODE* CHAIN_NODE::Insert_After(CHAIN_NODE *nd)
//
//             Inserts a node "nd" after "this" node, returns "nd".
//
//         CHAIN_NODE* CHAIN_NODE::Insert_Before(CHAIN_NODE *nd)
//
//             Inserts a node "nd" before "this" node, returns "nd".
//
//         CHAIN_NODE* CHAIN_NODE::Remove(void)
//
//             Removes the "this" node from the list, returning it.
//
//         CHAIN_NODE* CHAIN_NODE::Next(void)
//
//             Returns the "next" node
//
//         CHAIN_NODE* CHAIN_NODE::Next(void)
//
//             Returns the "prev" node
//
//         void CHAIN_NODE::Set_Next(CHAIN_NODE *nd)
//
//             Sets the "next" field of "this" node.
//
//         void CHAIN_NODE::Set_Prev(CHAIN_NODE *nd)
//
//             Sets the "prev" field of "this" node.
//

class CHAIN_NODE {
friend class CHAIN;
friend class CHAIN_ITER;
private:
  CHAIN_NODE *_next;   // point to the next node in the doubly linked list
  CHAIN_NODE *_prev;   // point to the prev node in the doubly linked list

  CHAIN_NODE& operator= (const CHAIN_NODE& sl);
  CHAIN_NODE(const CHAIN_NODE&);

protected:
  CHAIN_NODE(void)                       { _next = _prev = NULL; }
  ~CHAIN_NODE(void)                      {}

  CHAIN_NODE *Insert_After(CHAIN_NODE *nd);
  CHAIN_NODE *Insert_Before(CHAIN_NODE *nd);
  CHAIN_NODE *Remove(void);

  CHAIN_NODE *Next(void)                    { return _next;}
  const CHAIN_NODE *Next(void) const        { return _next;}
  CHAIN_NODE *Prev(void)                    { return _prev;}
  const CHAIN_NODE *Prev(void) const        { return _prev;}
  void        Set_Next(CHAIN_NODE *n)       { _next = n; }
  void        Set_Prev(CHAIN_NODE *n)       { _prev = n; }
};

// CHAIN     : the container class is typically used to construct a
//             doubly linked list that is based on the base class,
//             CHAIN_NODE.  It maintains the head and the tail of the
//             doubly linked list for fast update.
//
//       Exported Functions:
//
//         CHAIN::CHAIN(void)
//
//             Construct a doubly linked list container, initialize head and
//             tail to NULL
//
//         CHAIN::CHAIN(CHAIN_NODE *nd)
//
//             Construct a doubly linked list container, initialize head and
//             tail to nd.
//
//         CHAIN::~CHAIN(void)
//
//             Destruct a doubly linked list container, reset head and tail
//             to NULL.
//
//         void CHAIN::Init(CHAIN_NODE *node)
//
//             Initializes head and tail with node
//
//         void CHAIN::Init(CHAIN *list)
//
//             Initializes head and tail to those of the "list"
//
//         void CHAIN::Clear(void)
//
//             Clear the CHAIN content, ie set head and tail to NULL.
//
//         void CHAIN::Append( CHAIN_NODE *nd )
//
//             Appends node "nd" to the "tail" of the list
//
//         void CHAIN::Prepend( CHAIN_NODE *nd )
//
//             Inserts node "nd" at the "head" of the list
//
//         void CHAIN::Insert_After( CHAIN_NODE *nd, CHAIN_NODE *after_nd )
//
//             Inserts node "nd" after the "after_nd" node.
//
//         void CHAIN::Insert_before( CHAIN_NODE *nd, CHAIN_NODE *before_nd )
//
//             Inserts node "nd" before the "before_nd" node.
//
//         BOOL CHAIN::Is_Member( CHAIN_NODE *nd ) const
//
//             Returns TRUE if "nd" is in the current list, or FALSE otherwise
//
//         void CHAIN::Append_List(CHAIN *new_list)
//
//             Appends the "new_list" at the "tail" of the list
//
//         void CHAIN::Prepend_List(CHAIN *new_list)
//
//             Prepends the "new_list" at the "head" of the list
//
//         void CHAIN::Remove(CHAIN_NODE *nd)
//
//             Remove the node from the chain
//
//         CHAIN_NODE* CHAIN::Remove_Head(void)
//
//             Remove the current head node from the chain, and return it
//
//         CHAIN_NODE* CHAIN::Remove_Tail(void)
//
//             Removes the current tail node from the chain, returning it
//
//         CHAIN_NODE * CHAIN::Head(void)
//         const CHAIN_NODE * CHAIN::Head(void) const
//
//             Returns the "head" node of the list
//
//         CHAIN_NODE * CHAIN::Tail(void)
//         const CHAIN_NODE * CHAIN::Tail(void) const
//
//             Returns the "tail" node of the list
//
//         BOOL CHAIN::Is_Empty(void) const
//
//             Returns TRUE if head node is NULL, or FALSE otherwise
//
//         INT32 CHAIN::Len(void) const
//
//             Returns the length of the list in the container
//

class CHAIN {
private:
  CHAIN_NODE *_head;
  CHAIN_NODE *_tail;

  CHAIN& operator= (const CHAIN& sl);
  CHAIN(const CHAIN&);

protected:
  CHAIN(void)                  { _head = _tail = NULL; }

public:
  CHAIN(CHAIN_NODE *nd)        { _head = _tail = nd; }
  ~CHAIN(void)                 {}

                                        // same as constructor
  void        Init(void)                { _head = _tail = NULL; }
  void        Init(CHAIN_NODE *nd)      { _head = _tail = nd; }
  void        Init(CHAIN *list)         
			       { _head = list->Head(); _tail = list->Tail();}
  void        Clear(void)               { _head = _tail = NULL; }

  void        Append( CHAIN_NODE *nd );
  void        Prepend( CHAIN_NODE *nd );
  void        Insert_After(CHAIN_NODE *nd, CHAIN_NODE *after_nd);
  void        Insert_Before(CHAIN_NODE *nd, CHAIN_NODE *before_nd);
  BOOL        Is_Member(CHAIN_NODE *nd) const;

  void        Append_List(CHAIN *new_list);
  void        Prepend_List(CHAIN *new_list);
  void        Remove(CHAIN_NODE *);
  CHAIN_NODE *Remove_Head(void);
  CHAIN_NODE *Remove_Tail(void);

  CHAIN_NODE       *Head(void)                 { return _head; }
  const CHAIN_NODE *Head(void) const           { return _head; }
  CHAIN_NODE       *Tail(void)                       { return _tail; }
  const CHAIN_NODE *Tail(void) const                 { return _tail; }
  BOOL        Is_Empty(void) const             { return _head == NULL; }
  INT32       Len(void) const;

};

// CHAIN_ITER: the iterator class is typically used to iterate through
//             a doubly linked list that is based on the base class
//             CHAIN_NODE, to perform certain operation.  It does not
//             change the list content.
//
//       Exported Functions:
//
//         CHAIN_ITER::CHAIN_ITER(void)
//
//             Construct a doubly linked list iter.  Initialize list/cur to
//             NULL and len/idx to -1.
//
//         CHAIN_ITER::Set_Cur(CHAIN_NODE *cur)
//
//             Sets the _cur node to the specified 'cur' node.  This allow
//             caller to visit part of the chain starting from 'cur' node.
//             NOTE: Since this function currently does not set the _idx,
//                   please refrain from using it for indexing access.
//
//         CHAIN_ITER::CHAIN_ITER(CHAIN *sl)
//
//             Construct a doubly linked list iter.  It takes a doubly
//             linked list container and use it to initialize this
//             iterator's list pointer.  It also set cur node to NULL
//             and len/idx to -1.
//
//         CHAIN_ITER::~CHAIN_ITER(void)
//
//             Destruct a doubly linked list iter.
//
//         void CHAIN_ITER::Init(CHAIN *sl)
//
//             Uses the doubly linked list container to initialize the
//             list of this iterator.
//
//         void CHAIN_ITER::Clear(void)
//
//             Set list/tail to NULL, len/idx to -1.
//
//         CHAIN_NODE* CHAIN_ITER::First(void)
//
//             Gets the first element and set "cur" pointer
//
//         CHAIN_NODE* CHAIN_ITER::Last(void)
//
//             Gets the last element and set "cur" pointer
//
//         CHAIN_NODE* CHAIN_ITER::Next(void)
//
//             Gets the next element and increment the "cur" pointer
//
//         CHAIN_NODE* CHAIN_ITER::Prev(void)
//
//             Gets the next element and decrement the "cur" pointer
//
//         CHAIN_NODE* CHAIN_ITER::Nth(INT n)
//
//             Gets the n-th element in the list
//             Note: Head()==Nth(0)
//
//         CHAIN_NODE* CHAIN_ITER::Last_Nth(INT n)
//
//             Gets the last n-th element in the list
//             Note: Tail()==Last_Nth(0)
//
//         CHAIN *CHAIN_ITER::List(void)
//
//             Gets the pointer to the container list
//
//         CHAIN_NODE* CHAIN_ITER::Peek_Next(void)
//
//             Gets the next element in the list
//
//         CHAIN_NODE* CHAIN_ITER::Cur(void)
//
//             Gets the "cur" element in the list
//
//         INT CHAIN_ITER::Idx(void)
//
//             Gets the current index "idx"
//
//         INT32 CHAIN_ITER::Len(void)
//
//             Gets the length "len" of the list
//
//         BOOL CHAIN_ITER::Is_Empty(void)
//
//             Returns TRUE if the current element, "cur", is a NULL pointer.
//
//         BOOL CHAIN_ITER::Is_Empty_Reverse(void)
//
//             Returns TRUE if the current element, "cur", is a NULL pointer.
//
// Example:
// CHAIN_ITER  foo_iter( list );
//
// for (foo_iter.First(); !foo_iter.Is_Empty(); foo_iter.Next()) {
//   // do whatever
// }
//

class CHAIN_ITER {
private:
  CHAIN_NODE  *_cur;
  CHAIN       *_list;
  mINT16       _len;
  mINT16       _idx;          // simulate indexing, default to -1

  CHAIN_ITER& operator= (const CHAIN_ITER& sl);
  CHAIN_ITER(const CHAIN_ITER&);

protected:
  CHAIN_ITER(void)   { _list = NULL; _cur = NULL; _len = -1; _idx = -1;}
  void Set_Cur(CHAIN_NODE *cur) { _cur = cur; }

public:
  CHAIN_ITER(CHAIN *sl)
		   { _list = sl; _cur=_list->Head(); 
					_len = -1; _idx = -1; }
  ~CHAIN_ITER(void) {}

  void         Init(CHAIN *sl) { _list = sl; _len=_idx=-1; _cur=_list->Head();}
  void         Clear(void) { _list = NULL; _cur = NULL; _len = -1; _idx = -1;}
  CHAIN_NODE  *First(void);  // get the first element, and reset the index.
  CHAIN_NODE  *Last(void);   // get the last element, and set index=len.
  CHAIN_NODE  *Next(void);   // get the next element, and incr. the pointer.
  CHAIN_NODE  *Prev(void);   // get the prev element, and decr. the pointer.
  CHAIN_NODE  *Nth(INT n);   // get the nth element
  CHAIN_NODE  *Last_Nth(INT n);// get the last nth element

  CHAIN       *List(void)            {return _list;} 
  CHAIN_NODE  *Peek_Next(void)       {return _cur->Next();} 
  CHAIN_NODE  *Cur(void)             {return _cur;}
  INT          Idx(void)             {return _idx;}
  INT32        Len(void);            // get the length of the list
  BOOL         Is_Empty(void)        { return _cur == NULL; }
  BOOL         Is_Empty_Reverse(void){ return _cur == NULL; }
};

// DECLARE_CHAIN_NODE_CLASS: a macro to define the body of a derived
//                           class "NAME_LIST" of CHAIN_NODE
//                   Example:
//                           class NAME_LIST : public CHAIN_NODE {
//                           DECLARE_CHAIN_NODE_CLASS( NAME_LIST )
//                           ~NAME_LIST(void);
//                           };
//                   Note:   Create your own destructor!

#define DECLARE_CHAIN_NODE_CLASS( NAME_NODE )				\
public:									\
  NAME_NODE *Next(void) 						\
		{return (NAME_NODE *)CHAIN_NODE::Next();}		\
  NAME_NODE *Prev(void)							\
		{return (NAME_NODE *)CHAIN_NODE::Prev();}		\
  const NAME_NODE *Next(void) const 					\
		{return (const NAME_NODE *)CHAIN_NODE::Next();}		\
  const NAME_NODE *Prev(void) const 					\
		{return (const NAME_NODE *)CHAIN_NODE::Prev();}		\
  NAME_NODE *Insert_Before(NAME_NODE *nd) {				\
    return (NAME_NODE*) CHAIN_NODE::Insert_Before(nd);                  \
  }                                                                     \
  NAME_NODE *Insert_After(NAME_NODE *nd) {				\
    return (NAME_NODE*) CHAIN_NODE::Insert_After(nd);                   \
  }                                                                     \
  NAME_NODE *Remove() {							\
    return (NAME_NODE*) CHAIN_NODE::Remove();                           \
  }                                                                     \

// DECLARE_CHAIN_CLASS: a macro to define the body of a derived
//                      class "NAME_LIST" of CHAIN.
//              Example:
//                      class NAME_LIST : public CHAIN {
//                      DECLARE_CHAIN_CLASS( NAME_LIST, NAME_NODE )
//                      };

#define DECLARE_CHAIN_CLASS( NAME_LIST, NAME_NODE)		 	\
public:									\
  NAME_LIST(NAME_NODE *nd) { CHAIN::Init((CHAIN_NODE*)nd); }		\
  NAME_LIST(void)          { CHAIN::Init();   } 			\
  void		Clear() {CHAIN::Clear();}				\
  void		Append(NAME_NODE* nd) {CHAIN::Append((CHAIN_NODE*)nd);}	\
  void		Prepend(NAME_NODE* nd) {CHAIN::Prepend((CHAIN_NODE*)nd);}\
  void		Insert_After(NAME_NODE* a, NAME_NODE* b)		\
                 {CHAIN::Insert_After((CHAIN_NODE*)a,(CHAIN_NODE*)b);}	\
  void		Insert_Before(NAME_NODE* a, NAME_NODE* b)		\
                 {CHAIN::Insert_Before((CHAIN_NODE*)a,(CHAIN_NODE*)b);}	\
  void		Append_List(NAME_LIST* l) {CHAIN::Append_List(l);}	\
  void		Prepend_List(NAME_LIST* l) {CHAIN::Prepend_List(l);}	\
                                                                        \
  BOOL       Is_Member(NAME_NODE *nd) const { return CHAIN::Is_Member((CHAIN_NODE*)nd); }  \
  NAME_NODE *Head(void) { return (NAME_NODE *) CHAIN::Head(); }		\
  const NAME_NODE *Head(void) const					\
		{ return (const NAME_NODE *) CHAIN::Head(); }   	\
  NAME_NODE *Tail(void) { return (NAME_NODE *) CHAIN::Tail(); }		\
  const NAME_NODE *Tail(void) const					\
		{ return (const NAME_NODE *) CHAIN::Tail(); }   	\
  void       Remove(NAME_NODE *nd) {					\
    CHAIN::Remove((CHAIN_NODE*)nd);					\
  }									\
  NAME_NODE  *Remove_Head(void) {					\
    return (NAME_NODE*) CHAIN::Remove_Head();                           \
  }                                                                     \
  NAME_NODE  *Remove_Tail(void) {					\
    return (NAME_NODE*) CHAIN::Remove_Tail();				\
  }                                                                     \
  BOOL       Is_Empty(void) const { return CHAIN::Is_Empty(); }		\
  BOOL       Is_Empty_Reverse(void) const { return CHAIN::Is_Empty(); }	\
  INT32      Len(void) const { return CHAIN::Len(); }			\


// DECLARE_CHAIN_ITER_CLASS: a macro to define the body of a derived
//                           class "NAME_LIST" of CHAIN_ITER
//                   Example:
//                           class NAME_LIST : public CHAIN_ITER {
//                           DECLARE_CHAIN_ITER_CLASS( NAME_LIST, NODE_T )
//                           };

#define DECLARE_CHAIN_ITER_CLASS( NAME_ITER, NAME_NODE, NAME_LIST)	\
public:									\
  NAME_ITER(NAME_LIST *nl) { CHAIN_ITER::Init(nl); }     		\
  NAME_NODE *First(void) { return (NAME_NODE *) CHAIN_ITER::First(); }	\
  NAME_NODE *Last(void) { return (NAME_NODE *) CHAIN_ITER::Last(); }	\
  NAME_NODE *Next(void) { return (NAME_NODE *) CHAIN_ITER::Next(); }	\
  NAME_NODE *Prev(void) { return (NAME_NODE *) CHAIN_ITER::Prev(); }	\
  NAME_NODE *List(void) { return (NAME_NODE *) CHAIN_ITER::List(); }	\
  NAME_NODE *Nth(INT n) { return (NAME_NODE *) CHAIN_ITER::Nth(n); }	\
  NAME_NODE *Last_Nth(INT n)                                            \
		    { return (NAME_NODE *) CHAIN_ITER::Last_Nth(n); }	\
  NAME_NODE *Peek_Next(void) { return (NAME_NODE *) CHAIN_ITER::Peek_Next(); }\
  NAME_NODE *Head(void) { return (NAME_NODE *) CHAIN_ITER::List(); }	\
  NAME_NODE *Cur(void) { return (NAME_NODE *) CHAIN_ITER::Cur(); }	\
  BOOL Is_Empty(void) { return CHAIN_ITER::Is_Empty(); }   	        \
  BOOL Is_Empty_Reverse(void) { return CHAIN_ITER::Is_Empty(); } 	\


#define DECLARE_CHAIN_CONST_ITER_CLASS( NAME_ITER, NAME_NODE, NAME_LIST) \
public:									\
  NAME_ITER(const NAME_LIST *nl)					\
	      { CHAIN_ITER::Init((NAME_LIST *)nl); }			\
  const NAME_NODE *First(void) 						\
              { return (const NAME_NODE *) CHAIN_ITER::First(); }	\
  const NAME_NODE *Last(void) 						\
              { return (const NAME_NODE *) CHAIN_ITER::Last(); }	\
  const NAME_NODE *Next(void) 						\
              { return (const NAME_NODE *) CHAIN_ITER::Next(); }	\
  const NAME_NODE *Prev(void) 						\
              { return (const NAME_NODE *) CHAIN_ITER::Prev(); }	\
  const NAME_NODE *List(void) 						\
              { return (const NAME_NODE *) CHAIN_ITER::List(); }	\
  const NAME_NODE *Nth(INT n)						\
              { return (const NAME_NODE *) CHAIN_ITER::Nth(n); }	\
  const NAME_NODE *Last_Nth(INT n)                                      \
              { return (const NAME_NODE *) CHAIN_ITER::Last_Nth(n); }	\
  const NAME_NODE *Peek_Next(void) 					\
              { return (const NAME_NODE *) CHAIN_ITER::Peek_Next(); }	\
  const NAME_NODE *Head(void)						\
              { return (const NAME_NODE *) CHAIN_ITER::List(); }	\
  const NAME_NODE *Cur(void) 						\
              { return (const NAME_NODE *) CHAIN_ITER::Cur(); }		\
  BOOL Is_Empty(void) { return CHAIN_ITER::Is_Empty(); }		\
  BOOL Is_Empty_Reverse(void) { return CHAIN_ITER::Is_Empty(); }	\



// ==================================================================
// Circularly Linked List
//
// CLIST_NODE: defines the basic element of circularly linked list.  It
//             contains the pointer to the next node.
//
//       Exported Functions:
//
//         CLIST_NODE::CLIST_NODE(void)
//
//             Construct a circular list node, initialize next field 
//		to NULL.
//
//         CLIST_NODE::~CLIST_NODE(void)
//
//             Destruct a circular list node, initialize next field to 
//		NULL.
//
//         void CLIST_NODE::Insert_After(CLIST_NODE *nd)
//
//             Inserts a node "nd" after "this" node.
//
//         CLIST_NODE* CLIST_NODE::Insert_Before(CLIST_NODE *nd)
//
//		Inserts a node "nd" before "this" node.  Please NOTE 
//		that this method is inefficient time-wise because of 
//		the need to search for the node that has "this" as its 
//		"next" node.
//
//         CLIST_NODE* CLIST_NODE::Remove(CLIST_NODE *prev)
//
//             Removes the "this" node from the list, returning it
//
//         CLIST_NODE* CLIST_NODE::Next(void)
//
//             Returns the "_next" node
//
//         void CLIST_NODE::Set_Next(CLIST_NODE *n)
//
//             Sets the "_next" node of "this" node.
//
//	   INT32 CLIST_NODE::Len(void) const
//
//	 	Returns the number of elements in the list
//

class CLIST_NODE {
friend class CLIST;
friend class CLIST_ITER;
private:
  CLIST_NODE *_next;   // point to the next node in the circularly linked list

  CLIST_NODE& operator= (const CLIST_NODE& cl);
  CLIST_NODE(const CLIST_NODE&);

  // access function only used by non-private methods
  CLIST_NODE *Find_Next( void ) const;

protected:
  CLIST_NODE(void)                       { _next = this; }
  ~CLIST_NODE(void)                      { _next = NULL; }

  void        Insert_After(CLIST_NODE *nd)  { nd->_next = _next; _next = nd; }
  CLIST_NODE *Insert_Before(CLIST_NODE *nd);
  CLIST_NODE *Remove(CLIST_NODE *prev);

  CLIST_NODE *Next(void) const		{ return _next;}
  void        Set_Next(CLIST_NODE *n)	{ _next = n; }
  INT32       Len(void) const;
};

// CLIST     : the container class is typically used to construct a
//		circularly linked list that is based on the base class,
//		CLIST_NODE.  It maintains the head of the circularly linked 
//		list for fast update.
//
//       Exported Functions:
//
//         CLIST::CLIST(void)
//
//             Construct a circular list container, initialize head to NULL
//
//         CLIST::CLIST(CLIST_NODE *list)
//
//             Construct a circular list container, initialize head with list.
//
//         CLIST::~CLIST(void)
//
//             Destruct a circular list container, reset head to NULL.
//
//         void CLIST::Init(CLIST_NODE *list)
//
//             Initializes head with list
//
//         void CLIST::Clear(void)
//
//             Clear the CLIST content, ie set head to NULL.
//
//         void CLIST::Append( CLIST_NODE *nd )
//
//             Appends node "nd" to the list so it is at the "end" of
//		the list, and therefore precedes the head
//
//         BOOL CLIST::Append( CLIST_NODE *nd, CLIST_NODE *od)
//
//             Appends node "nd" after the node "od" in the list
//
//         void CLIST::Prepend( CLIST_NODE *nd )
//
//             Inserts node "nd" at the "_head" of the list
//
//         BOOL CLIST::Prepend( CLIST_NODE *nd, CLIST_NODE *od )
//
//             Inserts node "nd" before the node "od" in the list
//
//         void CLIST::Append_List(CLIST *new_list)
//
//             Appends the "new_list" so it precedes the head
//
//         void CLIST::Prepend_List(CLIST *new_list)
//
//             Prepends the "new_list" at the "_head" of the list
//
//         CLIST_NODE * CLIST::Remove_Headnode(void)
//
//             Removes the current head node
//
//         CLIST_NODE * CLIST::Head(void)
//
//             Returns the "_head" node of the list
//
//         BOOL CLIST::Is_Empty(void) const
//
//             Returns TRUE if head node is NULL, or FALSE otherwise
//
//         INT32 CLIST::Len(void) const
//
//             Returns the length of the list in the container
//
//

class CLIST {
private:
  CLIST_NODE *_head;
  CLIST_NODE *_tail;

  CLIST& operator = (const CLIST& cl);
  CLIST(const CLIST&);

protected:
  CLIST(void)			{ _head = _tail = NULL; }

public:
  void        Init(CLIST_NODE *list);
  CLIST(CLIST_NODE *list)		{ Init(list); }
  ~CLIST(void)				{ _head = _tail = NULL; }

  void        Clear(void)               { _head = _tail = NULL; }

  void        Append( CLIST_NODE *nd );
  BOOL        Append( CLIST_NODE *nd, CLIST_NODE *od);
  void        Prepend( CLIST_NODE *nd );
  BOOL        Prepend( CLIST_NODE *nd, CLIST_NODE *od );

  void        Append_List(CLIST *new_list);
  void        Prepend_List(CLIST *new_list);
  CLIST_NODE *Remove_Headnode(void);

  CLIST_NODE *Head(void) const                 { return _head; }
  CLIST_NODE *Tail(void) const                 { return _tail; }
  BOOL        Is_Empty(void) const             { return _head == NULL; }
  INT32       Len(void) const;

};

// CLIST_ITER: the iterator class is typically used to iterate through
//             a circularly linked list that is based on the base class
//             CLIST_NODE, to perform certain operation.  It does not
//             change the list content.
//
//       Exported Functions:
//
//         CLIST_ITER::CLIST_ITER(void)
//
//             Construct a circular list iter.  Initialize head/cur to NULL
//
//         CLIST_ITER::CLIST_ITER(CLIST_NODE *nd)
//
//             Construct a circular list iter.  Initialize head with nd,
//             cur with NULL
//
//         CLIST_ITER::CLIST_ITER(CLIST *cl)
//
//             Construct a circular list iter.  It takes a circular list
//             container and use its head to initialize this
//             iterator's head pointer.  It also set cur node to NULL
//
//         CLIST_ITER::~CLIST_ITER(void)
//
//             Destruct a circular list iter.  It sets head to NULL
//
//         void CLIST_ITER::Init(CLIST_NODE *nd)
//
//             Initializes "_head" with node "nd"
//
//         void CLIST_ITER::Init(CLIST *cl)
//
//             Uses the circular list container's head to initialize the
//             head node of this iterator.
//
//         void CLIST_ITER::Clear(void)
//
//             Set head to NULL
//
//         void CLIST_ITER:: Set(CLIST_NODE *nd)
//
//             Sets the current node "_cur" with node "nd"
//
//         CLIST_NODE* CLIST_ITER::First(void)
//
//             Gets the first element "_head" and set "_cur" pointer
//
//         CLIST_NODE* CLIST_ITER::Next(void)
//
//             Gets the _next element and bump the "_cur" pointer
//
//         CLIST_NODE* CLIST_ITER::Peek_Next(void)
//
//             Gets the _next element in the list
//
//         CLIST_NODE* CLIST_ITER::Head(void)
//
//             Gets the "_head" in the list
//
//         CLIST_NODE* CLIST_ITER::Cur(void)
//
//             Gets the "_cur" element in the list
//
//         INT32 CLIST_ITER::Len(void)
//
//             Gets the length of the list
//
//         BOOL CLIST_ITER::Is_Empty(void)
//
//             Returns TRUE if the current element, "_cur", is a NULL pointer.
//
// Example:
// CLIST_ITER  foo_iter( list );
//
// for (foo_iter.First(); !foo_iter.Is_empty(); foo_iter.Next()) {
//   // do whatever
// }
//

class CLIST_ITER {
private:
  CLIST_NODE *_head;
  CLIST_NODE *_cur;
  BOOL        _saw_head;

  CLIST_ITER& operator= (const CLIST_ITER& cl);
  CLIST_ITER(const CLIST_ITER&);

protected:
  CLIST_ITER(void)		{ _head = NULL; 
				  _cur = NULL; 
				  _saw_head = FALSE;
				}

public:
  CLIST_ITER(CLIST_NODE *nd)	{ _head = nd;
				  _cur = _head; 
				  _saw_head = FALSE;
				}
  CLIST_ITER(CLIST *cl)		{ _head = cl->Head(); 
				  _cur = _head; 
				  _saw_head = FALSE;
				}
  ~CLIST_ITER(void)		{ _head = NULL; 
				  _cur = NULL;
				  _saw_head = FALSE;
				}

  void         Init(CLIST_NODE *nd) 
				{ _head = nd; 
				  _cur = _head;
				  _saw_head = FALSE;
				}
  void         Init(CLIST *cl)  { _head = cl->Head();
				  _cur = _head;
				  _saw_head = FALSE;
				}
  void	       Init(void)	{ _head = NULL; 
				  _cur = NULL;
				  _saw_head = FALSE;
				}

  void         Clear(void) { Init(); }
  void         Set(CLIST_NODE *nd)  {_cur = nd; }
  CLIST_NODE  *First(void);  // get the first element, and reset the pointer
  CLIST_NODE  *Next(void);   // get the next element, and bump the pointer.

  CLIST_NODE  *Peek_Next(void) const {return _cur->Next();} 
  CLIST_NODE  *Head(void) const   {return _head;}
  CLIST_NODE  *Cur(void)  const   {return _cur;}
  BOOL         Saw_Head(void) const {return _saw_head;}
  INT32        Len(void) const;    // get the length of the list

  // for circular list, if we've already seen the head, we're done
  BOOL         Is_Empty(void){ return ( _cur == NULL	||
					(_cur == _head && _saw_head) );
			     }
};

// DECLARE_CLIST_NODE_CLASS: a macro to define the body of a derived
//                           class "NAME_LIST" of CLIST_NODE
//                   Example:
//                           class NAME_LIST : public CLIST_NODE {
//                           DECLARE_CLIST_NODE_CLASS( NAME_LIST )
//                           ~NAME_LIST(void);
//                           };
//                   Note:   Create your own destructor!

#define DECLARE_CLIST_NODE_CLASS( NAME_LIST )			      \
								      \
public:								      \
  NAME_LIST *Next(void) {return (NAME_LIST *) CLIST_NODE::Next();}    \
  void Insert_After(NAME_LIST *nd) { 			      	      \
    CLIST_NODE::Insert_After(nd);				      \
  }							              \
  void Insert_Before(NAME_LIST *nd) {			              \
    CLIST_NODE::Insert_Before(nd); 			              \
  }							              \
  NAME_NODE* Remove(NAME_LIST *prev) { 	              	 	      \
    return (NAME_NODE*)CLIST_NODE::Remove(prev);		      \
  }								      \
  void Set_Next(NAME_LIST *nd) {				      \
    CLIST_NODE::Set_Next(nd);					      \
  }								      \
  INT32 Len(void)  { return CLIST_NODE::Len(); }		      \


// DECLARE_CLIST_CLASS: a macro to define the body of a derived
//                      class "NAME_LIST" of CLIST.
//              Example:
//                      class NAME_LIST : public CLIST {
//                      DECLARE_CLIST_CLASS( NAME_LIST, NODE_T )
//                      };

#define DECLARE_CLIST_CLASS( NAME_LIST, NAME_NODE)		 	\
public:									\
  NAME_LIST(NAME_NODE *nd) { CLIST::Init(nd); } 			\
  void Append(NAME_NODE *nd) { CLIST::Append(nd); }			\
  void Append(NAME_NODE *nd, NAME_NODE *od) { 				\
    CLIST::Append(nd, od);						\
  }									\
  void Prepend(NAME_NODE *nd) { CLIST::Prepend(nd); }			\
  void Prepend(NAME_NODE *nd, NAME_NODE *od) { 				\
    CLIST::Prepend(nd, od);						\
  }									\
  void Append_List(NAME_LIST *nl) { CLIST::Append_List(nl); }		\
  void Prepend_List(NAME_LIST *nl) { CLIST::Prepend_List(nl); }		\
  NAME_NODE *Remove_Headnode(void) {					\
    return (NAME_NODE*)CLIST::Remove_Headnode();			\
  }									\
  NAME_NODE *Head(void) { return (NAME_NODE *) CLIST::Head(); }		\
  NAME_NODE *Tail(void) { return (NAME_NODE *) CLIST::Tail(); }		\
  BOOL       Is_Empty(void) const { return CLIST::Is_Empty(); }		\
  INT32      Len(void) const { return CLIST::Len(); }			\



// DECLARE_CLIST_ITER_CLASS: a macro to define the body of a derived
//                           class "NAME_LIST" of CLIST_ITER
//                   Example:
//                           class NAME_LIST : public CLIST_ITER {
//                           DECLARE_CLIST_ITER_CLASS( NAME_LIST, NODE_T )
//                           };

#define DECLARE_CLIST_ITER_CLASS( NAME_ITER, NAME_NODE, NAME_LIST)	\
public:									\
  NAME_ITER(NAME_NODE *nd) { CLIST_ITER::Init(nd); } 			\
  NAME_ITER(NAME_LIST *nl) { CLIST_ITER::Init(nl); }     		\
  NAME_ITER(void)          { CLIST_ITER::Init();   }                    \
  void Init(NAME_NODE *nd) { CLIST_ITER::Init(nd); } 			\
  void Init(NAME_LIST *nl) { CLIST_ITER::Init(nl); }                    \
  void Set(NAME_NODE *nd) { CLIST_ITER::Set(nd); } 			\
  NAME_NODE *First(void) { return (NAME_NODE *) CLIST_ITER::First(); }	\
  NAME_NODE *Next(void) { return (NAME_NODE *) CLIST_ITER::Next(); }	\
  NAME_NODE *Peek_Next(void){ return (NAME_NODE *) CLIST_ITER::Peek_Next(); }\
  NAME_NODE *Head(void) { return (NAME_NODE *) CLIST_ITER::Head(); }	\
  NAME_NODE *Cur(void) { return (NAME_NODE *) CLIST_ITER::Cur(); }	\
  BOOL       Saw_Head(void) { return CLIST_ITER::Saw_Head(); }		\



#endif  // cxx_base_INCLUDED


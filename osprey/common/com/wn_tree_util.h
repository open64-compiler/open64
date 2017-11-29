/* -*- c++ -*-
 *
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


//-*-c++-*-
// INTERFACE: wn_tree_util.h
#ifndef wn_tree_util_INCLUDED
#define wn_tree_util_INCLUDED "wn_tree_util.h"
// ====================================================================
// ====================================================================
//
// Module: wn_tree_util.h
// $Revision: 
// $Date: 
// $Author: 
// $Source: 
//
// Revision history:
//  14-Aug-97 - Original Version 
//
// Description:
//
// ======================= WN tree walk routines =======================
//  - wn_tree_util.h defines interface to tree walk functions.
//
//  - defines a tree iterator class WN_TREE_ITER which satisfies 
//    the requirements of an STL FORWARD ITERATOR 
//
//  - defines a tree container WN_TREE_CONTAINER which satisfies
//    the requirements of an STL FORWARD CONTAINER
//
//  - NOTE : We prefer that you use the WN_TREE_CONTAINER 
//    over the WN_TREE_ITERATOR since the interface/algorithms are cleaner
//    
// Benefits of using the tree container:
// (A) clean interface
// (B) Ability to use STL algorithms which operate on Fwd Containers
// (C) efficient: it doesn't "push" unless it has to, 
//                it uses kid_index to efficiently access next kid
//                it pre-allocates for the vector (fewer reallocs)
// (D) generic :  it allows parameterized tree traversal: PRE/POST-ORDER
//                it allows us to skip siblings of nodes
//                it allows us to abort tree walk in middle (use stl find_if)
//
// Qn: WHEN is it BEST to USE the tree container defined here?
// Ans: 
//  (A) If you traverse but not modify a whirl tree
//  - eg If you are traversing whirl trees computing summary information
//  - eg If you are searching for whirl patterns in a whirl tree
//  (B) If you modify only the children of the node you are currently visiting
//  - eg Replace ICALL by CALL
// ====================================================================
// included files
// ======================================================================
#include <utility>                     // pair template
#include <algorithm>                   // which includes algobase which has
                                       // the function "distance"
#include <vector>                      // represent stack as vector of pairs
#include <functional>                  // for != etc
#include "defs.h"                      // INT etc
#include "wn.h"                        // whirl node
#include "cxx_memory.h"                // for CXX_NEW etc
// ======================================================================
// TYPEDEFS, DEFINES, FORWARD_DEFINITIONS and EXTERN interfaces
// ======================================================================

// typedef TRAV_ORDER needs to be exposed to the user
// IN_ORDER doesn't make sense for whirl trees (they are not binary trees)

enum TRAV_ORDER {
  PRE_ORDER=0,
  POST_ORDER=1
};

// ======================================================================
// 
// This file contains the routines necessary for whirl tree operations
// a. tree walk: WN_WALK_TREE
// b. tree copy: WN_COPY_TREE
// .... for later: WN_INSERT, WN_DELETE, .....

// Exported classes
// 

// WN_TREE_ITER: tree iterator helper class
//     This class has data members 
//     - WN *_wn  (since the WN_TREE_ITER container OWNS its elements we keep
//                 pointers to whirl nodes inside the container)
//     -  _parent is a stack of pairs
//     - kid_index part of pair is used for determining "next" for non-block 
//     - WN* part of the pair is the parent wn
//     - template value parameter, TRAV_ORDER represents PRE/POSI_ORDER
//     - Note: IN_ORDER traversal doesn't make much sense since whirl
//             trees are not binary (or for that matter n-ary for fixed n)

// member functions
//     WN_TREE_next, WN_TREE_next_skip, etc, etc
//

// ======================================================================

// class WN_TREE_ITER: parameterized by how you traverse tree
// a. PRE_ORDER
// b. POST_ORDER
//    the default value (16) chosen for stack size indicates the 
//    "expected depth" of the whirl tree: 
//   TRAVERSAL ORDER examples
//   eg if the tree is:
//                   1
//                2     3
//              4  5   6
//                7 8
//
// (a) its depth is 4 (the Depth function returns the actual tree depth)
// (b)* the pre-order traversal order is: 1 2 4 5 7 8 3 6
//    * when traversing 5 the stack will have [<1,0> <2,1>
//    * when traversing 7 the stack will have [<1,0> <2,1> <5,0>
//    * when traversing 7, "next" returns 8 and "next_skip" returns 3
// (c)* the post-order traversal order is: 4 7 8 5 2 6 3 1
//    * when traversing 5 the stack will have [<1,0> <2,1>
//    * when traversing 7 the stack will have [<1,0> <2,1> <5,0>
// (d)* the in-order traversal doesn't make sense for non-binary trees


// ======================================================================
// Base class shared by both traversal order interators
// We need this to get partial specialization work
// ======================================================================

template <typename WHIRL>
class WN_TREE_ITER_base
{

  // WN_TREE_ITER contains a stack of pairs where
  // each stack element is a pair consisting of a whirl node and a
  // kid_index (specifying which kid the whirl node is (of its parent)
  // kid_index is used only for non-block parent and is -1 if parent is block 


  // The next typedefs are for
  // STL forward iterator requirements (see STL/iterator.h)

public:

  typedef WN_TREE_ITER_base<WHIRL>  self;
  typedef std::forward_iterator_tag iterator_category;
  typedef WHIRL                     value_type;
  typedef ptrdiff_t                 difference_type;
  typedef value_type*               pointer;
  typedef const value_type&         const_reference;
  typedef value_type&               reference;
  typedef std::vector<std::pair<WHIRL,INT32> > WN_STACK;

protected:

  WHIRL			       _wn;     // whirl node to iterate over;
  WN_STACK		       _parent; // whirl node's internal stack


public:
  // access functions  

  WHIRL Wn(void)                        const      {return _wn;}
  const WN_STACK& Parent(void)		const      {return _parent;}

  // set functions

  void Set_wn(WHIRL wn2)                           { _wn = wn2;}
  void Set_parent(const WN_STACK& parent2)	   {_parent = parent2;}

protected:
  // Utility functions 

  INT  Get_kid_index(void) const {  return (_parent.back()).second;}          
  void Set_kid_index(INT i)      {  (_parent.back()).second = i;}    
  INT  Inc_kid_index(void)       {  return (++((_parent.back()).second));}

  // Push() for stack
  // push _wn onto _parent stack
  // _wn becomes its next descendent 
  // NOTE: you only push a node if it has descendents
  // Thus, it is incorrect to call this fn with leaf nodes or empty blocks.

  void Push () {
    Is_True (_wn != NULL, ("Bad tree node"));
    Is_True (((WN_operator (_wn) == OPR_BLOCK) && WN_first (_wn)) ||
	     !OPCODE_is_leaf (WN_opcode (_wn)),
	     ("Push only applies to nodes with descendents"));;
  
    if (WN_operator (_wn) == OPR_BLOCK) {
      _parent.push_back (std::make_pair (_wn, -1));
      Set_wn (WN_first (_wn));
    } else {
      _parent.push_back (std::make_pair (_wn, 0));
      Set_wn (WN_kid0 (_wn));
    } 
  }					    

public:

  // pop stack, and update parent with parent_wn
  void Pop () {
    Is_True(! _parent.empty(),("Cannot pop empty stack"));
    Set_wn (Get_parent_wn ());
    _parent.pop_back ();
  }
  
  WHIRL Get_parent_wn(void) const { 
    return _parent.empty () ? NULL : (_parent.back()).first;
  }

  inline INT Depth()             { return 1+ _parent.size();} // Depth() >= 1

  // Constructors

  // (a) For PRE_ORDER traversal,  the stack is empty upon startup whereas
  //     for POST_ORDER traversal, the stack has all nodes upto leftmost leaf
  // (b) A tree node is pushed on the stack ONLY if it has children

  // (c) INVARIANT relationship between _wn and _parent_stack:
  //     parent_wn =  1st(top(_parent))     kid_index = 2nd(top(_parent))
  //     1. If (parent_wn) is a BLOCK 
  //          then _wn is "a" kid of parent_wn;
  //     2. else /* parent_wn has kids */
  //          then _wn is WN_Kid(parent_wn,kid_index)

  // used for creating a "null" iterator via conversion from 0
  // we can thus write (tree_iter != 0)

  WN_TREE_ITER_base(): _wn(NULL)    {}

  // use the constructor  when you need to specify default stack size

  WN_TREE_ITER_base (WHIRL wn2) : _wn(wn2) {}

  // operators to fulfill forward Iterator requirements

  // Equality is just a test of equality on WN *
  // equality is only defined on two tree-iterators which have SAME TRAV ORDER
  // equality satisfies (it1 == it2) <==> (&*it1 == &*it2)

  friend bool operator==(const self &x, const self & y) {
    return x.Wn() == y.Wn();
  }
  friend bool operator!=(const self &x, const self & y) {
    return x.Wn() != y.Wn();
  }
  
  reference   operator*()     { return *_wn; }

  // replace the current node by another
  void Replace (WHIRL new_wn) {
    WHIRL parent = Get_parent_wn ();
    Is_True (parent != NULL, ("can't replace a node without a parent"));
    if (WN_operator (parent) == OPR_BLOCK) {
      Is_True (OPERATOR_has_next_prev (WN_operator (new_wn)),
	       ("Invalid opcode passed to TREE_ITER::Replace ()"));
      if (WN_first (parent) == _wn) {
	WN_first (parent) = new_wn;
	WN_prev (new_wn) = NULL;
	WN_next (new_wn) = WN_next (_wn);
	if (WN_next (_wn) == NULL)
	  WN_last (parent) = new_wn;
	else
	  WN_prev (WN_next (_wn)) = new_wn;
      } else if (WN_last (parent) == _wn) {
	WN_last (parent) = new_wn;
	WN_next (new_wn) = NULL;
	WN_prev (new_wn) = WN_prev (_wn);
	if (WN_prev (_wn) == NULL)
	  WN_first (parent) = new_wn;
	else
	  WN_next (WN_prev (_wn)) = new_wn;
      } else {
	WN_prev (new_wn) = WN_prev (_wn);
	WN_next (WN_prev (_wn)) = new_wn;
	WN_next (new_wn) = WN_next (_wn);
	WN_prev (WN_next (_wn)) = new_wn;
      }
    } else {
      WN_kid (parent, Get_kid_index ()) = new_wn;
    }
    _wn = new_wn;
  }


}; // class WN_TREE_ITER_base


// ======================================================================

// dummy template to parameterize the traversal order.
// only the specialized versions defined below are used.
template <TRAV_ORDER order, class WHIRL = WN*>
class WN_TREE_ITER : public WN_TREE_ITER_base<WHIRL>
{
}; // WN_TREE_ITER


// ======================================================================
// preorder traversal iterator
// ======================================================================
template <typename WHIRL>
class WN_TREE_ITER<PRE_ORDER, WHIRL> : public WN_TREE_ITER_base<WHIRL>
{
public:
  WN_TREE_ITER () : WN_TREE_ITER_base<WHIRL> () {}
  
  WN_TREE_ITER (WHIRL wn) : WN_TREE_ITER_base<WHIRL> (wn) {}

  // tree related functions

  // Unwind starts from a node and keeps popping tree nodes off stack 
  // until you've reached the "next" node of the leaf node. 
  // Unwind starts from leaf except in "print" 
  void Unwind();


  void WN_TREE_next ();			// next for preorder

  // WN_TREE_next_skip return the next sibling, ie 
  // a. "abort" the tree walk of any children
  // b. Go to the next (right) sibling if it exists
  //    else go to the next (right) sibling of parent's, etc..
  void WN_TREE_next_skip () {	    
    Unwind ();
  }

  // skip to the next sibling of the nth parent.
  // i.e., when depth == 1, skip to the next sibling of the parent
  // when depth == 2, skip to the next sibling of the grandparent, etc.
  void Skip (UINT depth = 0) {
    while (depth > 0 && !this->_parent.empty ()) {
      this->Pop ();
      --depth;
    }
    WN_TREE_next_skip ();
  }

  // delete the current node
  void Delete () {
    WHIRL parent = this->Get_parent_wn ();
    Is_True (parent, ("cannot delete nodes without a parent"));
    Is_True (WN_operator (parent) == OPR_BLOCK,
	     ("can only delete nodes under a OPR_BLOCK"));
    WHIRL _next = WN_next (this->_wn);
    WHIRL _prev = WN_prev (this->_wn);
    if (WN_first (parent) == this->_wn) {
      WN_first (parent) = _next;
      if (_next == NULL)
	WN_last (parent) = NULL;
      else
	WN_prev (_next) = _prev;
    } else if (WN_last (parent) == this->_wn) {
      WN_last (parent) = _prev;
      if (_prev == NULL)
	WN_first (parent) = NULL;
      else
	WN_next (_prev) = _next;
    } else {
      WN_prev (_next) = _prev;
      WN_next (_prev) = _next;
    }
    this->_wn = _next;
    if (this->_wn == NULL) {
      this->Pop ();
      Skip (0);
    }
  }

  // Insert a node before the current node, after insertion, current node
  // points to the new node
  void Insert (WHIRL node) {
    WHIRL parent = this->Get_parent_wn ();
    Is_True (parent, ("cannot insert to nodes without a parent"));
    Is_True (WN_operator (parent) == OPR_BLOCK,
	     ("can only insert before nodes under a OPR_BLOCK"));
    WHIRL _prev = WN_prev (this->_wn);
    WN_prev (node) = _prev;
    if (_prev == NULL)
      WN_first (parent) = node;
    else
      WN_next (_prev) = node;
    WN_next (node) = this->_wn;
    WN_prev (this->_wn) = node;
    this->_wn = node;
  }

  // tree related "iterators"
  
  typedef WN_TREE_ITER<PRE_ORDER, WHIRL>  self;

  self &      operator++()    { WN_TREE_next(); return *this;} // pre
  self        operator++(INT) { self tmp = *this; WN_TREE_next(); return tmp;}

}; // WN_TREE_ITER<PRE_ORDER, WHIRL>


// Unwind keeps popping parent_stack untill you reach a parent_whirl_node 
// for which the _wn (child) is NOT the "last" (rightmost) child. Unwind 
// sets _wn to the "next" child of this parent_whirl_node
// unwind the "stack" until we reach the "next" or the END
template <class WHIRL>
void
WN_TREE_ITER<PRE_ORDER, WHIRL>::Unwind() {

  // unwind "unwinds the stack"
  BOOL done = FALSE;
  while (!done) {
    WHIRL wn = this->Wn();
    Is_True(wn != 0,("Bad tree node"));

    WHIRL parent_wn = this->Get_parent_wn();
    if (parent_wn == NULL) {
      // reached end of unwind
      this->Set_wn(NULL);
      return;
    }
    
    if (WN_operator(parent_wn) == OPR_BLOCK) {
      if (WN_next(wn)) {
        this->Set_wn(WN_next(wn));
        done = TRUE;
      }
      else // all stmts in a block processed ==> go back up
        this->Pop(); // Pop(parent_wn) + MORE WORK NEEDED
    } 
    else { // parent is NON_BLOCK ie increment kid_count to get next sibling
      INT indx = this->Get_kid_index();
      if ((0 <= indx) && (indx < WN_kid_count(parent_wn) - 1)) {
        this->Set_wn(WN_kid(parent_wn,this->Inc_kid_index()));
        done = TRUE;
      }
      else {
        this->Pop(); // Pop(parent_wn) + MORE WORK NEEDED
      }
    } // else parent is NON_BLOCK
  } // while (!done)
}  // Unwind


// Specialization of WN_TREE_next() for PRE_ORDER

template <class WHIRL>
void
WN_TREE_ITER<PRE_ORDER, WHIRL>::WN_TREE_next ()
{
  Is_True(this->_wn != 0, ("Bad tree node"));
  if (WN_operator(this->_wn) == OPR_BLOCK) {
    if (WN_first(this->_wn)) // go down
      this->Push();  
    else              // go back up
      Unwind(); // Pop(parent_wn) + MORE WORK NEEDED

  } // if (OPCODE_OPERATOR(WN_opcode(wn)) == OPR_BLOCK)
  else  // not a block ==> look at kid_count to determine where to go
    if ((WN_kid_count(this->_wn) != 0) && (WN_kid0(this->_wn)))
      this->Push(); // go down
    else  // go sideways (if parent is BLOCK) or UP otherwise
      Unwind(); // Pop(parent_wn) + MORE WORK NEEDED
}


// ======================================================================
// postorder traversal iterator
// ======================================================================
template <class WHIRL>
class WN_TREE_ITER<POST_ORDER, WHIRL> : public WN_TREE_ITER_base<WHIRL>
{
public:
  // tree related functions

  // Wind keeps pushing a tree node on stack and going down its leftmost
  // child until you reach a "leaf"
  void Wind();


  void WN_TREE_next ();			// next for preorder

  // tree related "iterators"
  
  typedef WN_TREE_ITER<POST_ORDER, WHIRL>  self;

  self &      operator++()    { WN_TREE_next(); return *this;} // pre
  self        operator++(INT) { self tmp = *this; WN_TREE_next(); return tmp;}

  // constructors
  
  WN_TREE_ITER () : WN_TREE_ITER_base<WHIRL> () {}
  
  WN_TREE_ITER (WHIRL wn) : WN_TREE_ITER_base<WHIRL> (wn) {

    Wind ();				// Wind initializes stack and _wn
					// to the "first" element based on
					// the traversal order 
  }

}; // WN_TREE_ITER<POST_ORDER, WHIRL>


// Wind takes a whirl node and keeps pushing it and going down its leftmost
// child until it gets to a leaf node.
// Wind is mainly for determining the FIRST node to be traversed.
// (a) We've got to go traversing DOWN a whirl tree; 
//     wind the "stack" until we reach a LEAF (empty BLOCK or leaf_node)
template <class WHIRL>
void
WN_TREE_ITER<POST_ORDER, WHIRL>::Wind ()
{
  Is_True(this->_wn != 0,("Bad tree node"));
  BOOL done = FALSE;
  while (!done) {

    if (WN_operator(this->_wn) == OPR_BLOCK) {
      if (WN_first(this->_wn)) 
	this->Push();
      else // leaf block 
	done = TRUE;
    } else  // parent is NON_BLOCK ie increment kid_count to get next sibling
      if ((WN_kid_count(this->_wn) == 0) || (!WN_kid0(this->_wn)))
	// leaf node
	done = TRUE;
      else 
	this->Push();
  } // while (!done)
} // Wind


    
// Specialization of WN_TREE_next() for POST_ORDER

template <class WHIRL>
void
WN_TREE_ITER<POST_ORDER, WHIRL>::WN_TREE_next ()
{
  Is_True(this->_wn != 0, ("Bad tree node"));
  
  WHIRL parent_wn = this->Get_parent_wn();
  if (parent_wn == NULL) {
    // reached end of recursion
    this->Set_wn(NULL);
    return;
  }
    
  if (WN_operator(parent_wn) == OPR_BLOCK) {
    if (WN_next(this->_wn)) { // go sideways
      this->Set_wn(WN_next(this->_wn));
      Wind();
    }
    else              // go back up
      this->Pop(); // Pop(parent_wn) 
  } // if (OPCODE_OPERATOR(WN_opcode(wn)) == OPR_BLOCK)
  else { // not a block ==> look at kid_count to determine where to go
    INT indx = this->Get_kid_index();
    if ((0 <= indx) && (indx < WN_kid_count(parent_wn) - 1)) {
      this->Set_wn(WN_kid(parent_wn, this->Inc_kid_index()));
      Wind();
    }
    else 
      this->Pop(); // Pop(parent_wn) 
  } // else 
}

// shorthand for commonly used iterator
typedef WN_TREE_ITER<PRE_ORDER, WN*> TREE_ITER;
typedef WN_TREE_ITER<PRE_ORDER, const WN*> CONST_TREE_ITER;

// ======================================================================

// WN_TREE_CONTAINER: is a model of a forward container
// class WN_TREE_CONTAINER: parameterized by how you traverse tree
// a. PRE_ORDER
// b. POST_ORDER

template <TRAV_ORDER order = PRE_ORDER>
class WN_TREE_CONTAINER {
public:
  typedef WN                                   value_type;
  typedef WN_TREE_ITER<order, WN*>	       iterator;
  typedef WN_TREE_ITER<order, const WN*>       const_iterator;
  typedef value_type *                         pointer;
  typedef value_type &                         reference;
  typedef const value_type &                   const_reference;
  typedef size_t                               size_type;
  typedef ptrdiff_t                            difference_type;

  typedef WN_TREE_CONTAINER<order>             self;
protected:
  iterator _start;
  iterator _finish;
  WN *     _root;

public:
  iterator       begin()          { return _start;}
  const_iterator begin() const    { return _start;}
  iterator       end()            { return _finish;}
  const_iterator end()   const    { return _finish;}
  bool           empty() const    { return begin() == end(); }

  WN *           Root()  const    { return _root;}

  // operators to fulfill Fwd Container requirements defined below

  // (1) equality on container is elementwise equality (linear time) on
  // containers with the SAME traversal order
  // (2) WARNING: equality is linear time, don't use it unless you have to
  // (3) equality on container should not be invoked unless
  // you want WHIRL_NODE* equality to mean WN_equiv as is done for now
  // (4) if you can come up with a reasonable (better than WN_equiv) definition
  // of equality on WN*, please replace the defn below and let tk know
  // (5) I believe the operator== is not meaningful and will remove it 
  // unless anyone who uses the tree container convinces me that operator== 
  // can be reasonably defined form WN_TREE_CONTAINER

  friend bool operator==(const self &x, const self & y) {
    typedef typename WN_TREE_CONTAINER<order >::const_iterator
    container_node;
    container_node xptr = x.begin();
    container_node yptr = y.begin();
    while (xptr.Wn() && yptr.Wn() &&  WN_Equiv(xptr.Wn(),yptr.Wn())) {
      ++xptr;
      ++yptr;
    }
    return ((xptr.Wn() == 0) && (yptr.Wn() == 0));
  }

  // WARNING: size is linear time, don't use it unless you have to
  size_type size()          const { return std::distance(begin(), end());}
  size_type      max_size() const { return size();} // fix_sized container

  // constructors
  WN_TREE_CONTAINER() : _start(0), _finish (0), _root(NULL) {}
  WN_TREE_CONTAINER(WN *w) :
    _start (iterator(w)), _finish (iterator()), _root (w) {
    Is_True(w!=0,("Bad Tree Root"));
  }
  WN_TREE_CONTAINER(const WN* w) :
    _start (const_iterator(w)), _finish (const_iterator()), _root (w) {
    Is_True(w!=0,("Bad Tree Root"));
  }
                                       
}; // class WN_TREE_CONTAINER

// ======================================================================
// convenient macros for the "last" tree_iterator
// ======================================================================
// this is a WN_TREE_ITER<..> with _wn == NULL
// Note: this is the ONLY WN_TREE_ITER object whose _wn is NULL
//     : ++ (or WN_TREE_next) is NOT defined on this object

#define LAST_PRE_ORDER_ITER (WN_TREE_ITER<PRE_ORDER, WN*> ())
#define LAST_POST_ORDER_ITER (WN_TREE_ITER<POST_ORDER, WN*> ())
#define LAST_PRE_ORDER_CONST_ITER (WN_TREE_ITER<PRE_ORDER, const WN*> ())
#define LAST_POST_ORDER_CONST_ITER (WN_TREE_ITER<POST_ORDER, const WN*> ())

// ======================================================================
// TREE_WALK: external interface
// ======================================================================

// WN_TREE_walk interface:
//  - walk the whirl tree "wn"
//  - apply "op" to each whirl node in the tree
//  - "trav_order" specifies order of tree traversal (defaults to PRE_ORDER)
//  - use parent map OR stack to iterate over tree nodes (defaults to stack)
//

// ======================================================================
// PRE-ORDER TRAVERSAL : root, left-subtree, right-subtree
// POST-ORDER TRAVERSAL: left-subtree, right-subtree, root
// ======================================================================



template <class OPERATION, class WHIRL, TRAV_ORDER trav_order>
inline OPERATION&
WN_TREE_walk (WHIRL wn, OPERATION& op,
	      const WN_TREE_ITER<trav_order, WHIRL>& last_iter)
{
  // legality check
  Is_True(wn != 0, ("Bad tree node"));
  // create tree iterator
  // use stack of default size
  WN_TREE_ITER<trav_order, WHIRL> tree_iter(wn);
  while (tree_iter!=last_iter) {
    // apply op and go to next node
    op(tree_iter.Wn());    
    ++tree_iter;
  }
  return op;
}
  

// One for PRE_ORDER
template <class OPERATION, class WHIRL>
OPERATION&
WN_TREE_walk_pre_order (WHIRL wn, OPERATION& op) {
  return WN_TREE_walk(wn, op, WN_TREE_ITER<PRE_ORDER, WHIRL> ());
}

// One for post_order
template <class OPERATION, class WHIRL>
OPERATION& WN_TREE_walk_post_order(WHIRL wn, OPERATION& op) {
  return WN_TREE_walk(wn, op, WN_TREE_ITER<POST_ORDER, WHIRL> ());
}


// ======================================================================
// Function object examples for use with tree traversal
// ======================================================================

// function object example: print the whirl opcode
struct WN_OPCODE_print {
  void  operator()(WN* wn) {
    printf("%s\n",OPCODE_name(WN_opcode(wn)));
  }
};

// function object example: count the number of whirl nodes
struct WN_count {
  INT num_nodes;
  WN_count() : num_nodes(0){}
  void  operator()(WN* ) {
    ++num_nodes;
  }
};

// ======================================================================
//  Some examples of usage of the TREE_ITERATOR/TREE_CONTAINER
// ======================================================================
// All examples are illustrated using BOTH the iterator and containers
// The container incarnations have cleaner interface/algorithms and 
// hence should be preferred over the iterator incarnations
// (A1)
// WN_TREE_walk_pre_order (wn, WN_OPCODE_print()); PRE_ORDER TRAV print opcode
// WN_TREE_walk_post_order(wn, WN_OPCODE_print()); POST_ORDER TRAV print opcode
// (A2) The above examples using a tree container
//  WN_TREE_CONTAINER<PRE_ORDER>  wcpre(wn);
//  WN_TREE_CONTAINER<POST_ORDER> wcpost(wn);
//  WN_TREE_CONTAINER<PRE_ORDER> ::iterator wipre;
//  WN_TREE_CONTAINER<POST_ORDER>::iterator wipost;
// for (wipre=wcpre.begin(); wipre != wcpre.end(); ++wipre)
//    WN_OPCODE_print(wipre.Wn());
// for (wipost=wcpost.begin(); wipost != wcpost.end(); ++wipost)
//    WN_OPCODE_print(wipost.Wn());

// (B1)
// next statement prints the number of nodes in the whirl tree
// printf("Number of tree nodes = %d\n",
//       WN_TREE_walk_pre_order (wn, WN_count()).num_nodes)
// (B2) The above example using a tree container
//  WN_count countpre;
//  WN_count countpost;
//    
//  for (wipre = wcpre.begin();  wipre !=  wcpre.end(); ++wipre)  
//    countpre(wipre.Wn());
//  for (wipost = wcpost.begin(); wipost != wcpost.end(); ++wipost)
//    countpost(wipost.Wn());
//  Assert(countpre.num_nodes == countpost.num_nodes)

// ======================================================================
// Some function objects that can be useful
// (C1) count the number of STIDs in the whirl tree
// struct WN_count_STID {
//  INT num_nodes;
//  WN_count_STID() : num_nodes(0){}
//  void  operator()(WN* wn) {
//    if (OPCODE_operator(WN_OPCODE(wn)) ==OPR_STID)
//            ++num_nodes;
//  }
// };
// 
// printf("Number of STID nodes = %d\n",
//         WN_TREE_walk_pre_order (wn, WN_count_STID()).num_nodes)
// 
// (C2) The above example using a tree container
//  WN_count_STID countstid;
//  for (wipre = wcpre.begin();  wipre !=  wcpre.end(); ++wipre)  
//    countstid(wipre.Wn());
// printf("Number of STID nodes = %d\n",countstid.num_nodes)
// (D1) Does a whirl tree have EH regions?
// 
// struct WN_EH_region_check {
//  BOOL found;
//  WN_EH_region_check() : found(FALSE){}
//  void  operator()(WN* wn) {
//        found = found || ((WN_operator(wn) == OPR_REGION) 
//                          && WN_region_is_EH(wn))
//  }
// };
// Use:
// BOOL WN_HAS_EH_REGION (WN* wn) { 
//      return WN_TREE_walk_pre_order (wn, WN_EH_region_check()).found;
// }
//
//  if (WN_HAS_EH_REGION (wn))
//    printf("wn has EH REGIONS\n");
//  else
//    printf("wn doesn't have EH REGIONS\n");    

// ======================================================================
// Using STL algorithms with the tree iterator
// ======================================================================
// Note: operator* on WN_TREE_ITER<..> returns a WN  (not a WN*)
// Hence the predicates used in the algorithms (eg find_if) should
// take a WN as an argument.
//
// - First the predicate definition
// bool is_stid(WN w) { return (WN_operator(&w) == OPR_STID);}
//
// - Next, its usage 
//  printf("First stid subtree in the tree\n");
//  WN_TREE_CONTAINER<PRE_ORDER>::iterator it = 
//    find_if(wcpre.begin(), wcpre.end(),is_stid);
//  if (it.Wn())
//    WN_TREE_dump_tree(it.Wn());


#endif // wn_tree_util_INCLUDED


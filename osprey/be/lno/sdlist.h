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
*** Description:
***
***	This file contains a class SD_INFO and related classes SD_CONST_PITER,
***	SD_PITER, SD_PLIST, and SD_PNODE which are used to build up objects 
***	of the SD_INFO class.  The structure of this class is similar to the
***	SX_INFO (see sxlist.h and sxlist.cxx for the code). 
***
***	The SD_INFO holds a linked list of information about scalars in an 
***	SNL.  Of particular interest are the non-expandable scalars, which 
***	we may distribute out of the nest.  Each scalar in the SNL is re-
***	presented by a single SD_PNODE on the Plist.  
***
***	class SD_PNODE
***
***      PRIVATE MEMBERS:
***
***	  SYMBOL _symbol
***
***	    The symbol of the scalar being described. 
***
***	  mINT8 _innermost_depth
***
***	    The innermost loop levels at which this scalar is defined in 
***	    the SNL.  	
***
***	  HASH_TABLE<WN*,INT> _in_closure;
***
***	    A hash table to store the nodes which can reached by each 
***	    definition of a scalar in the SNL. 
***
***	 PUBLIC MEMBERS: 
***
***	  SD_PNODE(const SYMBOL& symbol, 
***	           INT innermost_depth, 
***	           INT above)
***
***	    Construct a SD_PNODE with the given 'symbol' and 
***	    'innermost_depth' and Is_Above() value of 'above'. 
***
***	  ~SD_PNODE() 
***
***	    Null destructor for this class. 
***
***	  Symbol(), Innermost_Depth(), Set_Innermost_Depth() 
***
***	    Public versions of _symbol and _innermost_depth and a function
***	    to set the value of _innermost_depth. 
***
***	  Add_Closure(WN* wn)
*** 
***	    Add 'wn' to the closure of nodes reached by definitions of 
***	    this scalar in the SNL.
***
***	  In_Closure(WN* wn)
***
***	    Return TRUE if 'wn' is already determined to be in the closure
***	    of nodes reached by definitions of this scalar in the SNL.  
***
***	  Print(FILE* fp)
***	
***	    Print function for object of type SD_PNODE. 
***
***	class SD_INFO
***
***	 PRIVATE MEMBERS: 
***
***	  _max_inner_depth
***
***	    The depth of the innermost loop in the SNL
***
***	  Is_Worst_Case
***
***	    Return TRUE if the node 'sd_ref' has worst case behavior, i.e.
***	    the '_innermost_depth' is the depth of the innermost loop in the 
***	    SNL.
***
***       Update
***
***	    Add 'wn' to the closure list for 'sd_ref'.
***
***	  Set_Worst_Case
***
***	    Indicate that the node 'sd_ref' has worst case behavior, i.e.
***	    the '_innermost_depth' is the depth of the innermost loop in the 
***	    SNL.
***
***	  Push_Memory_Nodes
***
***	    If necessary, push all of the memory reference nodes in the
***	    statement for 'wn_orig' onto the stack 'st_closure', in antici-
***	    pation of entering them into the closure hash table 'sd_ref'. 
***	    Return TRUE if we should continue computing the closure, FALSE 
***	    if there is no reason to continue.
***
***	  Closure_Ldid
***
***	    Enter the node of OPR_LDID 'wn_ldid' into the closure hash
***	    table for 'sd_ref', and push other nodes within the immediately
***	    closure of 'wn_ldid' onto the stack 'st_closure'.  Return TRUE
***	    if it makes sense to continue computing the closure of 'sd_ref', 
***	    return FALSE otherwise.
***
***	  Closure_Stid
***
***	    Enter the node of OPR_STID 'wn_stid' into the closure hash
***	    table for 'sd_ref', and push other nodes within the immediately
***	    closure of 'wn_stid' onto the stack 'st_closure'.  Return TRUE
***	    if it makes sense to continue computing the closure of 'sd_ref',
***	    return FALSE otherwise.
***
***	  Closure_ILoad
***
***	    Enter the node of OPR_STID 'wn_iload' into the closure hash
***	    table for 'sd_ref', and push other nodes within the immediately
***	    closure of 'wn_iload' onto the stack 'st_closure'.  Return TRUE
***	    if it makes sense to continue computing the closure of 'sd_ref',
***	    FALSE otherwise.
***
***	  Closure_IStore
***
***	    Enter the node of OPR_STID 'wn_istore' into the closure hash
***	    table for 'sd_ref', and push other nodes within the immediately
***	    closure of 'wn_istore' onto the stack 'st_closure'.  Return TRUE 
***	    if it makes sense to continue computing the closure of 'sd_ref', 
***	    return FALSE otherwise.
***
***	  Closure
***
***	    Compute the depth of the closure of 'wn_ref' and place it in
***	    the _innermost_depth field of this node's SD_PNODE.
***
***	  void Handle_Def(WN* wn_def)
***
***	    Add information relevant to the OPR_STID node 'wn_def' to 
***	    the Plist of the SD_INFO. 
***
***	 PUBLIC MEMBERS: 
***
***	  SD_PLIST Plist
***
***	    A list of SD_NODEs, one for each of the scalars in the SNL
***
***	  SD_INFO(MEM_POOL* pool)
***
***	    Create an empty SD_INFO from the memory pool 'pool'. 
***
***	  ~SD_INFO()
***
***	    Null destructor for this class.
***
***	  SD_PNODE* Find(const SYMBOL& sym)
***	  const SD_PNODE* Find(const SYMBOL& sym) const
***
***	    Returns a pointer to the SD_PNODE on the Plist corresponding 
***	    to the symbol 'sym'.  Returns NULL if there is no node on the 
***	    Plist corresponding to this 'sym'. 
***	
***	  void Enter(const SYMBOL& symbol, 
***	             INT innermost_depth,
***		     BOOL above)
***
***	    Enter a node on the Plist of the SD_INFO which has a symbol 
***	    'sym', innermost depth 'innermost_depth', and Is_Above() value
***	    'above'.
***
***	  void Create(const SYMBOL& sym, WN* wn)
***
***	    Create a node for the symbol 'sym' with innermost depth and
***	    above values calculated from 'wn', if one does not already exist. 
***	    If there is already such a node, update it with the innermost depth
***	    and above value corresponding to 'wn'.
***
***	  void Remove(SD_PNODE* sdn) 
***
***	    Remove the node 'sdn' from the Plist of the SD_INFO. 
***
***	  void Print(FILE* fp) const
***
***	    Print function for objects of class SD_INFO.
***
***	  void Make_Sd_Info(WN* wn_outer, INT nloops)
***
***	    Construct an SD_INFO for the SNL with outermost loop  
***	    'wn_outer' containing 'nloops' loops. 
***
*** 	  INT Distribution_Range(INT depth, 
***	                         SX_INFO* sx_info) 
***
***	    For the SNL whose outermost transformable depth is 'depth'
***	    and whose expandable scalars are summarized in 'sx_info', 
***	    return the innermost depth of loops containing unexpandable 
***	    scalars. 
***
**/

#ifndef sdlist_INCLUDED
#define sdlist_INCLUDED "sdlist.h"

#ifndef cxx_hash_INCLUDED
#include "cxx_hash.h"
#endif 

#define SD_HASH_SIZE 512 

class SD_PNODE : public CHAIN_NODE {
 private: 
  SYMBOL _symbol; 
  mINT8 _innermost_depth;
  BOOL _above;   
  HASH_TABLE<WN*,INT> _in_closure; 
 public: 
  SD_PNODE(const SYMBOL& symbol, INT innermost_depth, BOOL above); 
  ~SD_PNODE() {} 
  const SYMBOL& Symbol() const {return _symbol;}
  mINT8 Innermost_Depth() const {return _innermost_depth;}
  void Set_Innermost_Depth(INT v) {_innermost_depth = v;}
  BOOL Above() {return _above;}
  void Set_Above(BOOL v) {_above = v;}
  void Add_Closure(WN* wn) {_in_closure.Enter(wn, 1);} 
  BOOL In_Closure(WN* wn) {return _in_closure.Find(wn);} 
  void Print(FILE* fp) const; 
};

class SD_PLIST: public CHAIN {
  DECLARE_CHAIN_CLASS(SD_PLIST, SD_PNODE);
 private:
  MEM_POOL* _pool;
 public:
  SD_PLIST(MEM_POOL* pool) : _pool(pool), CHAIN() {}
  SD_PNODE* Find(const SYMBOL& sym);
  const SD_PNODE* Find(const SYMBOL& sym) const;
  void Print(FILE *fp) const;
  MEM_POOL* Pool() {return _pool;}
  ~SD_PLIST();
};

class SD_PITER: public CHAIN_ITER {
  DECLARE_CHAIN_ITER_CLASS(SD_PITER, SD_PNODE, SD_PLIST)
 public:
  ~SD_PITER() {}
};

class SD_CONST_PITER: public CHAIN_ITER {
  DECLARE_CHAIN_CONST_ITER_CLASS(SD_CONST_PITER, SD_PNODE, SD_PLIST)
 public:
  ~SD_CONST_PITER() {}
};

class SD_INFO {
 private: 
  WN* _wn_outer; 
  mINT8 _max_inner_depth; 
  BOOL Is_Worst_Case(SD_PNODE* sd_ref) 
    {return sd_ref->Innermost_Depth() == _max_inner_depth;}
  BOOL Update(SD_PNODE* sd_ref, WN* wn);
  void Set_Worst_Case(SD_PNODE* sd_ref);
  BOOL Push_Memory_Nodes(WN* wn_orig, SD_PNODE* sd_ref, STACK<WN*>* st_closure);
  BOOL Register_Ldid(WN* wn_ldid, SD_PNODE* sd_ref); 
  BOOL Register_Stid(WN* wn_stid, SD_PNODE* sd_ref); 
  BOOL Register_ILoad(WN* wn_iload, SD_PNODE* sd_ref); 
  BOOL Register_IStore(WN* wn_istore, SD_PNODE* sd_ref); 
  BOOL Closure_Ldid(WN* wn_ldid, SD_PNODE* sd_ref, STACK<WN*>* st_closure); 
  BOOL Closure_Stid(WN* wn_stid, SD_PNODE* sd_ref, STACK<WN*>* st_closure); 
  BOOL Closure_ILoad(WN* wn_iload, SD_PNODE* sd_ref, STACK<WN*>* st_closure); 
  BOOL Closure_IStore(WN* wn_istore, SD_PNODE* sd_ref, STACK<WN*>* st_closure); 
  void Closure(WN* wn_ref);
  void Handle_Def(WN* wn); 
 public: 
  SD_PLIST Plist; 
  SD_INFO(MEM_POOL* pool) : Plist(pool) {}
  ~SD_INFO() {}
  SD_PNODE* Find(const SYMBOL& sym);
  const SD_PNODE* Find(const SYMBOL& sym) const;
  void Enter(const SYMBOL& symbol, INT innermost_depth, BOOL above); 
  void Create(const SYMBOL& symbol, WN* wn); 
  void Remove(SD_PNODE* sdn);
  void Print(FILE* fp) const;
  void Make_Sd_Info(WN* wn_outer, INT nloops);
  INT Distribution_Range(INT depth, SX_INFO* sx_info); 
};

#endif /* sdlist_INCLUDED */

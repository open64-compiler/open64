/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_base.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_base.h,v $
//
// Revision history:
//  8-SEP-94 shin - Original Version
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
// This file contains a class and several macros used widely throughout
// the optimizer:
//
// --> THe class MAP, which provides a hash-table based mapping from
//     one type to another.
//
//     Currently, MAP is only used in opt_cfg.h to map labels to BBs.
//     Perhaps it should be replaced by STL hash_map.
//
// --> Macros for iterators including:
//     FOR_ALL_ELEM and FOR_ALL_ELEM_REVERSE and FOR_ALL_ELEM_EXCEPT
//     FOR_ALL_NODE and FOR_ALL_NODE_REVERSE and FOR_ALL_NODE_EXCEPT
//     FOR_ALL_ITEM
//
// ====================================================================
// ====================================================================


#ifndef opt_base_INCLUDED
#define opt_base_INCLUDED	"opt_base.h"
#ifdef _KEEP_RCS_ID
static char *opt_basercs_id = 	opt_base_INCLUDED"$Revision: 1.8 $";
#endif /* _KEEP_RCS_ID */

#include "cxx_base.h"
#include "cxx_template.h"


// ====================================================================
//
// Classes MAP (and MAP_LIST) provide a hash-table based mapping from
// one type to another.
//
// ====================================================================


typedef void * POINTER;

class MAP_LIST : public SLIST_NODE {
private:
  POINTER key;
  POINTER val;

          MAP_LIST(void);
          MAP_LIST(const MAP_LIST&);
          MAP_LIST& operator = (const MAP_LIST&);
public:
          MAP_LIST(POINTER k, POINTER v) { Set_Next(NULL); key = k; val = v; }
         ~MAP_LIST(void);

  POINTER Key(void)          { return key;}
  void    Set_key(POINTER k) { key = k; }
  POINTER Val(void)          { return val;}
  void    Set_val(POINTER v) { val = v; }
};

typedef MAP_LIST *MAP_LIST_P;

class MAP_LIST_CONTAINER : public SLIST {
  DECLARE_SLIST_CLASS( MAP_LIST_CONTAINER, MAP_LIST )
};
class MAP_LIST_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS( MAP_LIST_ITER, MAP_LIST, MAP_LIST_CONTAINER )
};

class MAP {
private:
  MEM_POOL   *mem_pool;
  mUINT32     size;
  MAP_LIST_P *hash_vec;

              MAP(void);
              MAP(const MAP&);
              MAP& operator = (const MAP&);

public:
              MAP(mUINT32 hash_size, MEM_POOL *pool);
             ~MAP(void);

  void        Alloc_hash_vec(void);
  void        Free_hash_vec(void);

  mUINT32     Hash(POINTER k);
  MAP_LIST   *Find_map_list(POINTER k);
  void        Add_map(POINTER k, POINTER v);
  void        Override_add_map(POINTER k, POINTER v);
  POINTER     Get_val(POINTER k);
  mUINT32     Size() { return size; };
  MAP_LIST *  Get_bucket(UINT32 i) { return hash_vec[i]; };
#ifdef Is_True_On
  void	      Dump_map(FILE *fp=stderr);
#endif
};


// ====================================================================
//
// MACROS for iterators includes:
//   FOR_ALL_ELEM and FOR_ALL_ELEM_REVERSE and FOR_ALL_ELEM_EXCEPT
//   FOR_ALL_NODE and FOR_ALL_NODE_REVERSE and FOR_ALL_NODE_EXCEPT
//   FOR_ALL_ITEM
//
// ====================================================================


//  MACROS for iterators
//
//  FOR_ALL_ELEM(var, iter, init)
//      Iterates through a list of elements that may be stored in a
//      linked list or an array.  It returns the content of the list.
//
//      Parameters:
//
//          var:  the variable to hold the current node in the list
//          iter: the iterator that contains the list iteration info
//          init: the function that initialize the iterator.
//
//      Example:
//
//          The following two lines iterates through the successor
//          list of a basic block "bb":
//
//          BB_NODE *tmp;
//          BB_LIST_ITER bb_succ_iter();
//          FOR_ALL_ELEM (tmp, bb_succ_iter, Init(bb->Succ())) {
//
//
//          expands to:
//
//          BB_LIST_ITER bb_succ_iter( bb->Succ() );
//          //for each successor of bb
//          for ( tmp = bb_succ_iter.First_bb();
//	        ! bb_succ_iter.Is_Empty();
//	        tmp = bb_succ_iter.Next_bb() )
//
//      Note:
//
//          Since the iterator is designed to be used as brower, please
//          make certain the code in the loop does not change the
//          structure of the list.  

#define FOR_ALL_ELEM(var, iter, init)      \
      iter.init;                           \
      for (var = iter.First_elem();        \
	   !iter.Is_Empty();               \
	   var = iter.Next_elem())

#define FOR_ALL_ELEM_REVERSE(var, iter, init)      \
      iter.init;                                   \
      for (var = iter.Last_elem();                 \
	   !iter.Is_Empty_Reverse();               \
	   var = iter.Prev_elem())

//  FOR_ALL_ELEM_EXCEPT(var, iter, init, except)
//      Iterates through a list of elements that may be stored in a
//      linked list or an array, and skips the "except" element.  
//	It returns the content of the list.

#define FOR_ALL_ELEM_EXCEPT(var, iter, init, except)      \
	  FOR_ALL_ELEM(var,iter,init) if ( var != except )

//  FOR_ALL_NODE(var, iter, init)
//      Iterates through a list of elements that may be stored in a
//      linked list or an array.  It returns the linked list nodes if
//      it is a linked list, or it returns the index if the list is
//      represented as a dynamic array.
//
//      Parameters:
//
//          var:  the variable to hold the current node or index
//          iter: the iterator that contains the list iteration info
//          init: the function that initialize the iterator.
     
#define FOR_ALL_NODE(var, iter, init)      \
      iter.init;                           \
      for (var = iter.First();             \
	   !iter.Is_Empty();               \
	   var = iter.Next())

#define FOR_ALL_NODE_REVERSE(var, iter, init)      \
      iter.init;                                   \
      for (var = iter.Last();                      \
	   !iter.Is_Empty_Reverse();               \
	   var = iter.Prev())

//  FOR_ALL_NODE_EXCEPT(var, iter, init, except)
//      Iterates through a list of elements that may be stored in a
//      linked list or an array, and skips the "except" node.  It 
//	returns the linked list nodes if it is a linked list, or it 
//	returns the index if the list is represented as a dynamic array.

#define FOR_ALL_NODE_EXCEPT(var, iter, init, except)      \
	  FOR_ALL_NODE(var,iter,init) if ( var != except )

     
//  FOR_ALL_ITEM(iter, init)
//      Iterates through a list of elements that may be stored in a
//      linked list or an array.  It does not return anything.
//	The pointer to the current node or the index is internal to
//      the iterator. Member functions need to be declared to access
//	the current node or index.  The difference between FOR_ALL_ITEM
//	and FOR_ALL_NODE is that FOR_ALL_ITEM does not require a variable
//	to be supplied, which duplicates a similar variable within the
//	iterator.
//
//      Parameters:
//
//          iter: the iterator that contains the list iteration info
//          init: the function that initialize the iterator.

#define FOR_ALL_ITEM(iter, init)     \
      iter.init;                     \
      for (iter.First();             \
	   !iter.Is_Empty();         \
	   iter.Next())


#endif  // opt_base_INCLUDED


// ====================================================================

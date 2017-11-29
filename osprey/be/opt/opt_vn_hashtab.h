//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_vn_hashtab.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_vn_hashtab.h,v $
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
// INTERFACE DESCRIPTION:
//
//    This defines a class of object (VN_HASHTAB) used by value 
//    numbering (VN) for mapping VN expressions and memory locations 
//    to value numbers.  The interface exported here is a low-level
//    interface for use by the value numbering algorithms, and much
//    of the book-keeping is left to the client.
//
//    Most algorithms for value-numbering uses two hash-tables for
//    handling expressions versus memory locations.  We use one
//    unified hash-table to hold both kinds of information, since 
//    they both map a form of expression (lvalue vs rvalue) to a 
//    value-number.
//
//    We do not export any iterator over a VN_HASHTAB, since instead 
//    we adhere to the philosophy that any iterative algorithm over 
//    the VN_HASHKEYs should be provided by means of a member function
//    of the VN_HASHTAB class.
//
//    The exported VN_HASHTAB methods and datatypes are as follows:
//
//      EXPR_MAPPING: The kind of value returned by a "lookup_or_insert"
//          where "first()" is the VN_EXPR::PTR and "second()" is the
//          VN_VALNUM.
//
//      VN_HASHTAB: The constructor of the hash-table, where the only
//          parameter is the size of the hash-table.  The hash-table is
//          an STL hash_map, with collision resolution by chaining, where
//          buckets in the chains and the hash-table itself is allocated
//          using the default STL allocator.  TODO: Use specialized mempool
//          allocator.
//
//      lookup_or_insert: Given a VN_EXPR and a unique suggested VN_VALNUM,
//          the expression is looked up in the hash-table.  If found, the
//          existing mapping is returned, and the client can free() the
//          VN_EXPR and reuse the VN_VALNUM passed in as parameters.  If not
//          found, the given VN_EXPR will be entered into the hash_map, where
//          it will map to the given VN_VALNUM; in this case the returned
//          EXPR_MAPPING will map the given VN_EXPR to the given VN_VALNUM,
//          and the VN_EXPR reference must not be memory-reclaimed until
//          the mapping is removed from the hash-table.
//
//      clear: We intend to support both a single-pass and an iterative 
//          value-numbering algorithm.  The iterative value numbering
//          algorithm (e.g. Taylor Simpson's RPO algorithm) may require
//          a fast method of clearing the hash-table between iterations.
//          Currently, we use the STL default mechanism of clearing the
//          hash-table, which involves walking through all the buckets and
//          freeing them up (i.e. putting them on a free-list).  Currently,
//          the destructor for a VN_HASHKEY is a no-op, and the client must
//          keep track of allocated VN_EXPR objects and free them as 
//          appropriate.
//
//          TODO:  Use mempool allocator + see "possible improvements" below.
//
//
// IMPLEMENTATION DETAILS:
//
//    We implement the hashtable using the STL hash_map implementation,
//    where we use a mempool allocator.  Significant characteristics of 
//    this implementation are as follows:
//
//       * The allocator uses a free-list, which means clearing up
//         the hashtable and then reconstructing it again will end
//         up reusing bucket-elements allocated in a previous 
//         construction of the hash-table.
//
//       * Clearing the hashtable does involve walking over all the
//         buckets and reclaiming the memory (putting them into a free
//         list).  We could potentially do better than that (see below).
//
//       * We leave the management of VN_EXPR entries to the client of 
//         the VN_HASHTAB.  This includes deallocation, allocation, etc.
//         The destructor for the VN_HASHKEY is a noop.  The client should
//         maintain a list of the VN_EXPR entries in the hash-table,
//         and then walk throught it and clear it as appropriate.  One
//         reason for doing it this way is that we may have more than one
//         kind of mapping to a VN_EXPR (e.g. it may be referenced both 
//         from the hash-table and from a coderep_id-->VN_EXPR mapping).
//
//
// POSSIBLE IMPROVEMENTS:
//
//    The current implementation uses the STL "hash_map", and as such
//    is fairly simple.  A slightly more complicated implementation,
//    where we allocate hash-table buckets in a segmented array (allocates
//    a chunk of buckets at a time) and have the hash-table be an array of
//    indices into this segmented array, will allow a much more efficient
//    "clear()" implementation:
//
//        1) Memzero the hash-table (array of indices), assuming a zero
//           index indicates an empty hash-table entry.
//
//        2) Set the next available index in the segmented array to be
//           index 1.
//
//    This involves defining our own hash-bucket structure ("next" is an
//    index into segmented array), and a reimplementation of the hash-table
//    algorithms we now take from STL.  This is quite a simple job, albeit
//    using STL is even simpler and safer.  We only ought to undertake this
//    performance improvement if the STL traversal through every hash-bucket
//    every time "clear()" is called turns out to be a significant overhead
//    in the iterative value numbering algorithms.
//
//
// SEE ALSO:
//
//    opt_vn_expr.h : A value numbering expression object.  Also defines
//                    the VN_VALNUM type and associated values.
//
//    opt_vn.h      : Top level interface to the value-numbering 
//                    algorithms, and associated resultant maps.
//
// ====================================================================
// ====================================================================


#ifndef opt_vn_hashtab_INCLUDED
#define opt_vn_hashtab_INCLUDED "opt_vn_hashtab.h"

#include <ext/hash_map>
#include "mempool_allocator.h"
#include "opt_vn_expr.h"

#ifdef __STL_USE_NAMESPACES
using std::hash_map;
#endif

// Function object for determining equality between hash table keys.
//
struct VN_KEY_EQ
{
  bool operator()(const VN_EXPR::PTR &k1, const VN_EXPR::PTR &k2) const
  {
    return k1->is_equal_to(k2);
  }
};


// Function object for determining hash-value of hash table keys.
//
struct VN_KEY_HASHVAL
{
  size_t operator()(const VN_EXPR::PTR &k1) const
  {
    return k1->hash();
  }
};


class VN_HASHTAB
{
private:
   
   typedef pair<const VN_EXPR::PTR, VN_VALNUM>  HTABLE_VALUE_TYPE;
   typedef mempool_allocator<HTABLE_VALUE_TYPE> HTABLE_ALLOCATOR;
   typedef hash_map <VN_EXPR::PTR, 
      VN_VALNUM, VN_KEY_HASHVAL, VN_KEY_EQ, HTABLE_ALLOCATOR> HTABLE;
   
   HTABLE _tab;
   
public:

   typedef HTABLE::value_type EXPR_MAPPING;
   
   VN_HASHTAB(UINT32 minimum_size = 1000, MEM_POOL *mpool = Malloc_Mem_Pool): 
      _tab(minimum_size, 
	   VN_KEY_HASHVAL(), 
	   VN_KEY_EQ(), 
	   HTABLE_ALLOCATOR(mpool))
   {}
   
   EXPR_MAPPING lookup_or_insert(VN_EXPR::PTR expr, VN_VALNUM new_vn)
   {
      return (*_tab.insert(EXPR_MAPPING(expr, new_vn)).first);
   }
   
   void clear() 
   {
      _tab.clear();
   }
   
}; // VN_HASHTAB


#endif // opt_vn_hashtab_INCLUDED

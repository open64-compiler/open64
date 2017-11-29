//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: bb_node_set.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/bb_node_set.h,v $
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
//  This package implements sets of BB_NODE*'s.  These can be
//  manipulated with a fairly complete repertoire of set functions.
//  The sets are represented with bit strings for efficiency of both
//  space and run time.  The vector that represents these sets is
//  grown as necessary to accommodate the results of the various
//  operations.  In spite of this, the client of the package retains
//  control over storage allocation by providing memory allocation
//  pools (MEM_POOLs).  The representations are never automatically
//  trimmed, so that the representation of any given bit set will be
//  large enough to hold the element with the greatest number that it
//  ever held.
//
//
//
//  Destructive Operations Conventions
//  ==================================
//
//
//  The BB_NODE_SET class provides methods that work either on the
//  given instance, or can create and return new instances of a
//  set.  Those that work through side-effects on the given instance
//  are considered "destructive" operations, and all such methods end
//  with the letter D.  These methods still return the given instance
//  so expressions may be built up.  For now, destructive operations
//  only expand storage, and never shrink it.
//
//
//
//  Storage Allocation
//  ==================
//
//
//  The client of this package has full control over storage
//  allocation for BB_NODE_SET's.  This is achieved by passing storage
//  allocation pools (MEM_POOLs) to the functions that may need to use
//  them.
//
//
//
//  Types
//  =====
//
//
//  CLASS BB_NODE_SET
//
//  TYPE BB_NODE*
//
//      This is the type of an element of a BB_NODE_SET.
//
//
//
//  Constructors, Clearing, and Freeing
//  ===============================
//
//
//  BB_NODE_SET( 
//    BS_ELT size,
//    const CFG *cfgraph, 
//    MEM_POOL *pool,
//    BBNS_ACTION action 
//  )
//
//	Creates an instance of a set, that can hold any BB_NODE with
//	an Id() in the range 0..size-1.  'size' must be non-negative.
//	The 'cfgraph' provides the mechanism to get from an integral
//	value back to a BB_NODE *.
//	The memory pool is where the memory for the bitset will be
//	allocated.
//	'action' is one of:
//	  BBNS_EMPTY	: Clear the set on creation
//	  BBNS_UNIVERSE	: Fill the set on creation
//	  BBNS_DONT_CARE: Leave the set uninitialized (caller should make
//			  sure he will initialize it
//
//
//  BB_NODE_SET( 
//    BS_ELT low, 
//    BS_ELT high, 
//    const CFG *cfgraph, 
//    MEM_POOL *pool 
//  )
//
//	Creates an instance of a set with members low..high
//
//
//  BB_NODE_SET( 
//    const BB_NODE *bb, 
//    const CFG *cfgraph, 
//    MEM_POOL *pool
//  )
//
//	Creates an instance of a set with the sole member being 'bb'
//
//
//  BB_NODE_SET *ClearD()
//
//      Destructive clear operation.  After this 'set' will be empty.
//      However, this does not change the allocated size of the set
//      (it will still be able to contain the same members that it
//      could before it was cleared without expansion.)
//
//
//  BB_NODE_SET *RangeD(
//    BS_ELT      low,
//    BS_ELT      high,
//  )
//
//      Returns a set whose members are the BBs with Id() values of the
//      numbers 'low' ... 'high'.  Both 'low' and the size of the
//      range must be nonnegative or an error is caused.  I.e., 'low'
//      >= 0 and ('high' - 'low' + 1) >= 0.  Note that 'high' may be
//      -1 if 'low' is 0.
//
//
//  BB_NODE_SET *SingletonD(
//    BB_NODE* element,
//  )
//
//      Returns a set with 'element' as its sole member.
//
//
//  BB_NODE_SET *UniverseD(
//    BS_ELT    size,
//  )
//
//      Returns a set containing the BBs with Id() values of the numbers
//      0...'size' - 1.  'Size' must be nonnegative or an error is
//      caused.
//
//
//
//  Copying
//  =======
//
//
//  BB_NODE_SET *Copy(
//    MEM_POOL *pool
//  )
//
//	Creates a new instance of the given set, but allocated from
//	a different memory pool.
//
//  BB_NODE_SET *CopyD(
//    BB_NODE_SET   *set2,
//  )
//
//      The given instance's set becomes an exact copy of set2.
//
//
//  Set Operations
//  ==============
//
//
//  BB_NODE* Choose()
//
//      Returns some element of 'set', if 'set' is nonempty.  Else
//      returns BB_NODE_SET_CHOOSE_FAILURE.  In fact, this is defined so
//      that it always returns the least element of the set.
//
//
//  BB_NODE* Choose_Next(
//    BB_NODE* x
//  )
//
//      Returns the "next" element of 'set', starting after 'x', if
//	'set' is nonempty.  Else returns BB_NODE_SET_CHOOSE_FAILURE.
//	This is used for looping over the elements of a set.
//
//
//  BB_NODE* Choose_Range(
//    BS_ELT    low,
//    BS_ELT    high
//  )
//
//      Returns some element of 'set' whose Id() value is in the
//      range low..high if there is one.  Else returns
//      BB_NODE_SET_CHOOSE_FAILURE.  Both 'low' and the size of the 
//	range must be nonnegative or an error is caused.  
//	I.e., 'low' >= 0 and ('high' - 'low' + 1) >= 0.  Note that 
//	'high' may be -1 if 'low' is 0.  As with the Choose function, 
//	always returns the least element of the set that's in range.
//
//
//  BB_NODE_SET *Difference(
//    BB_NODE_SET   *set2,
//    MEM_POOL *pool
//  )
//
//      Creates a new instance and returns the given set with any
//	elements from set2 removed.
//
//  BB_NODE_SET *DifferenceD(
//    BB_NODE_SET *set2
//  )
//
//      Returns the given set with any elements from set2 removed.
//
//
//  BB_NODE_SET *Difference1(
//    BB_NODE* x,
//    MEM_POOL   *pool
//  )
//
//	Creates a new instance and returns the given set with element
//	x removed.
//
//  BB_NODE_SET *Difference1D(
//    BB_NODE* x
//  )
//
//      Removes the element x from the set.
//
//
//  BB_NODE_SET *Intersection(
//    BB_NODE_SET   *set2,
//    MEM_POOL *pool
//  )
//
//      Creates a new instance, allocated from pool, and returns the 
//	intersection with 'set2'.
//
//  BB_NODE_SET *IntersectionD(
//    BB_NODE_SET *set2
//  )
//
//      Intersects with set2.
//
//
//  BB_NODE_SET *Union(
//    BB_NODE_SET   *set2,
//    MEM_POOL *pool
//  )
//      Creates a new instance, allocated from pool, and returns the 
//	union with 'set2'.
//
//  BB_NODE_SET *UnionD(
//    BB_NODE_SET   *set2,
//  )
//
//      Unions with set2.
//
//
//  BB_NODE_SET *Union1(
//    BB_NODE* x,
//    MEM_POOL   *pool
//  )
//
//	Creates a new instance, allocated from pool, and returns the
//	set with element x added.
//
//  BB_NODE_SET *Union1D(
//    BB_NODE* x,
//  )
//
//      Adds element x to the set.
//
//  BB_NODE_SET *Union1D(
//    IDTYPE* id,
//  )
//
//      Adds BB_NODE with 'id' (say Id() or Dom_dfs_id()) to the set.
//
//  size_t BB_NODE_SET Size()
//
//	Returns the cardinality (population count) of the set.
//
//  size_t BB_NODE_SET Alloc_size()
//
//      Returns the number of bytes used to allocate the set.
//
//
//  Testing Sets
//  ============
//
//
//  BOOL ContainsP(
//    BB_NODE_SET *set2
//  )
//
//      Returns TRUE iff every element of 'set2' is in the set.  Else
//      FALSE.
//
//
//  BOOL EmptyP()
//
//      Returns TRUE iff the is empty.  Else FALSE.
//
//
//  BOOL EqualP(
//    BB_NODE_SET *set2
//  )
//
//      Returns TRUE iff the set has exactly the same members as
//      'set2'.  Else FALSE.
//
//
//  BOOL IntersectsP(
//    BB_NODE_SET *set2
//  )
//
//      Returns TRUE iff the set and 'set2' have at least one member in
//      common.  Else FALSE.
//
//
//  BOOL MemberP(
//    BB_NODE* x
//  )
//
//  BOOL MemberP(
//    IDTYPE id
//  )
//
//      Returns TRUE iff 'x' or 'id' is a member of the set.  Else FALSE.
//
//
//  Looping Over Members
//  ====================
//
//
//  This is done using Choose and Choose_Next.  Here is an example:
//
//	BB_NODE* x;
//	BB_NODE_SET *set;
//
//	for ( x = set->Choose();
//	      x != BB_NODE_SET_CHOOSE_FAILURE;
//            x = set->Choose_Next( x ) ) 
//	{
//        x is a member of the set
//      }
//
//
//  Handling Intersections Effeciently
//  ===================================
//
//
//  Often, once wants to perform operations on the intersection of two
//  sets without having to create the intersection.  This can be thought
//  of as an "operation with a mask".  Anyway, a set of functions is
//  provided to support this:
//
//
//  BOOL Intersection_MemberP(
//    BB_NODE_SET     *set2,
//    BB_NODE* x
//  )
//
//      Returns TRUE iff 'x' is a member of the intersection of the set
//	and 'set2' .  Else FALSE.
//
//
//  BB_NODE* Intersection_Choose(
//    BB_NODE_SET *set2
//  )
//
//      Returns some element of of the intersection of the set and 
//	'set2', if the intersection is nonempty.  Else returns
//      BB_NODE_SET_CHOOSE_FAILURE.  In fact, this is defined so that 
//	it always returns the least element of the set.
//
//
//  BB_NODE* Intersection_Choose_Next(
//    BB_NODE_SET     *set2,
//    BB_NODE* x
//  )
//
//      Returns the "next" element of the intersection of the set and
//      'set2', starting after 'x', if there is such a member.  Else
//      returns BB_NODE_SET_CHOOSE_FAILURE.  This is very useful for 
//	looping over the elements of a set intersection.
//
//
//  Printing Sets
//  =============
//
//
//  void Print(
//    FILE   *f
//  )
//
//      Prints the set on 'f'.  The type FILE is as defined in stdio.h.
//
// ====================================================================
// ====================================================================

#ifndef bb_node_set_INCLUDED
#define bb_node_set_INCLUDED         "bb_node_set.h"
#ifdef _KEEP_RCS_ID
static char *bb_node_set_rcs_id =    bb_node_set_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

#include "mempool.h"	// our clients should probably include this
#include "bitset.h"	// our clients do not need to know about it
#include "opt_bb.h"
#include "opt_cfg.h"
#include "erglob.h"

#define BB_NODE_SET_CHOOSE_FAILURE ((BB_NODE*)BS_CHOOSE_FAILURE)

// action to use in constructor to decide how the bitset should be
// initialized
typedef enum {
  BBNS_EMPTY,		// clear set when creating
  BBNS_UNIVERSE,	// fill set when creating
  BBNS_DONT_CARE,	// leave set uninitialized
} BBNS_ACTION;

class BB_NODE_SET {
private:
  BS *bs;		// the actual bitset
  const CFG *cfg;	// control-flow graph to get index => bb_node
  MEM_POOL *mempool;	// memory pool to use as default

  // constructor to override default (never used for this class)
  BB_NODE_SET(void)		{}
  BB_NODE_SET(const BB_NODE_SET&);
  BB_NODE_SET& operator = (const BB_NODE_SET&);

  // constructor to create set with no BS
  BB_NODE_SET( const CFG *cfgraph, MEM_POOL *pool )
		{ mempool = pool;
		  cfg = cfgraph;
		}

  // function to convert from cfg,int to bb_node
  BB_NODE *id2bb( BS_ELT id ) const
		{ return id != BS_CHOOSE_FAILURE ?
			cfg->Get_bb(id) :
			BB_NODE_SET_CHOOSE_FAILURE;
		}

public:
  // constructor to create empty set with given size
  BB_NODE_SET( BS_ELT size, const CFG *cfgraph, MEM_POOL *pool,
	       BBNS_ACTION action )
		{ mempool = pool;
		  cfg = cfgraph;
		  switch ( action ) {
		    case BBNS_EMPTY:
		      bs = BS_Create_Empty( size, pool );
		      break;
		    case BBNS_UNIVERSE:
		      bs = BS_Universe( size, pool );
		      break;
		    case BBNS_DONT_CARE:
		      bs = BS_Create( size, pool );
		      break;
		    default:
		      ErrMsg( EC_Misc_Int, "BB_NODE_SET: action",
			      (INT) action );
		      break;
		  }
		}

  // constructor to create set with elements low..high set
  BB_NODE_SET( BS_ELT low, BS_ELT high, 
	       const CFG *cfgraph, MEM_POOL *pool )
		{ mempool = pool;
		  cfg = cfgraph;
		  bs = BS_Range( low, high, pool );
		}

  // constructor to create set with single element bb included
  BB_NODE_SET( const BB_NODE *bb, const CFG *cfgraph, MEM_POOL *pool )
		{ mempool = pool;
 		  cfg = cfgraph;
		  bs = BS_Singleton( bb->Id(), pool );
		}

 // destructor
 ~BB_NODE_SET(void)		{}

  BS          *Bs(void) const   { return bs; }

  const CFG   *Cfg(void) const  { return cfg; }

  BB_NODE_SET *ClearD( void )
		{ bs = BS_ClearD(bs);
		  return this; 
		}

  BB_NODE_SET *RangeD( BS_ELT low, BS_ELT high )
		{ bs = BS_RangeD( bs, low, high, mempool );
		  return this;
		}

  BB_NODE_SET *SingletonD( const BB_NODE *bb )
		{ bs = BS_SingletonD( bs, bb->Id(), mempool );
		  return this;
		}

  BB_NODE_SET *UniverseD( BS_ELT size )
		{ bs = BS_UniverseD( bs, size, mempool );
		  return this;
		}

  BB_NODE_SET *Copy( MEM_POOL *pool )
		{ BB_NODE_SET *new1 = CXX_NEW(BB_NODE_SET(cfg,pool),pool);
		  new1->bs = BS_Copy( bs, pool );
		  return new1;
		}

  BB_NODE_SET *CopyD( const BB_NODE_SET *bbns )
		{ bs = BS_CopyD( bs, bbns->bs, mempool );
		  return this;
		}


// To be obsolete.   Replace by iterators.

  BB_NODE *Choose( void ) const
		{ return id2bb(BS_Choose(bs)); }

  BB_NODE *Intersection_Choose( const BB_NODE_SET *bbns ) const
		{ return id2bb(BS_Intersection_Choose(bs,bbns->bs)); }

  BB_NODE *Choose_Next( const BB_NODE *bb ) const
		{ return id2bb(BS_Choose_Next(bs,bb->Id())); }

  BB_NODE *Intersection_Choose_Next( const BB_NODE_SET *bbns,
				     const BB_NODE *bb ) const
		{ return id2bb(
		  BS_Intersection_Choose_Next(bs,bbns->bs,bb->Id()) );
		}

  BB_NODE *Choose_Range( BS_ELT low, BS_ELT high ) const
		{ return id2bb(BS_Choose_Range(bs,low,high)); }

//

  BB_NODE_SET *Difference( const BB_NODE_SET *bbns, MEM_POOL *pool )
		{ BB_NODE_SET *new1 = CXX_NEW(BB_NODE_SET(cfg,pool),pool);
		  new1->bs = BS_Difference( bs, bbns->bs, pool );
		  return new1;
		}

  BB_NODE_SET *DifferenceD( const BB_NODE_SET *bbns )
		{ bs = BS_DifferenceD( bs, bbns->bs );
		  return this;
		}

  BB_NODE_SET *Difference1( const BB_NODE *bb, MEM_POOL *pool )
		{ BB_NODE_SET *new1 = CXX_NEW(BB_NODE_SET(cfg,pool),pool);
		  new1->bs = BS_Difference1( bs, bb->Id(), pool );
		  return new1;
		}

  BB_NODE_SET *Difference1D( const BB_NODE *bb )
		{ bs = BS_Difference1D( bs, bb->Id() );
		  return this;
		}

  BB_NODE_SET *Intersection( const BB_NODE_SET *bbns, MEM_POOL *pool )
		{ BB_NODE_SET *new1 = CXX_NEW(BB_NODE_SET(cfg,pool),pool);
		  new1->bs = BS_Intersection( bs, bbns->bs, pool );
		  return new1;
		}

  BB_NODE_SET *IntersectionD( const BB_NODE_SET *bbns )
		{ bs = BS_IntersectionD( bs, bbns->bs );
		  return this;
		}

  BS_ELT Size( void ) const
		{ return BS_Size(bs); }

  BS_ELT Alloc_size( void ) const
		{ return BS_Alloc_Size(bs); }

  BB_NODE_SET *Union( const BB_NODE_SET *bbns, MEM_POOL *pool )
		{ BB_NODE_SET *new1 = CXX_NEW(BB_NODE_SET(cfg,pool),pool);
		  new1->bs = BS_Union( bs, bbns->bs, pool );
		  return new1;
		}

  BB_NODE_SET *UnionD( const BB_NODE_SET *bbns )
		{ bs = BS_UnionD( bs, bbns->bs, mempool );
		  return this;
		}

  BB_NODE_SET *Union1( const BB_NODE *bb, MEM_POOL *pool )
		{ BB_NODE_SET *new1 = CXX_NEW(BB_NODE_SET(cfg,pool),pool);
		  new1->bs = BS_Union1( bs, bb->Id(), pool );
		  return new1;
		}

  BB_NODE_SET *Union1D( const BB_NODE *bb )
		{ bs = BS_Union1D( bs, bb->Id(), mempool );
		  return this;
		}

  BB_NODE_SET *Union1D( const IDTYPE id )
		{ bs = BS_Union1D( bs, id, mempool );
		  return this;
		}

  BOOL ContainsP( const BB_NODE_SET *bbns ) const
		{ return BS_ContainsP( bs, bbns->bs ); }

  BOOL EmptyP( void ) const
		{ return BS_EmptyP( bs ); }

  BOOL EqualP( const BB_NODE_SET *bbns ) const
		{ return BS_EqualP( bs, bbns->bs ); }

  BOOL IntersectsP( const BB_NODE_SET *bbns ) const
		{ return BS_IntersectsP( bs, bbns->bs ); }

  BOOL MemberP( const BB_NODE *bb )  const
		{ return BS_MemberP( bs, bb->Id() ); }

  BOOL MemberP( const IDTYPE id )  const
		{ return BS_MemberP( bs, id ); }

  BOOL Intersection_MemberP( const BB_NODE_SET *bbns, const BB_NODE *bb) const
		{ return BS_Intersection_MemberP(bs,bbns->bs,bb->Id());}

  void Print( FILE *fp=stderr ) const
		{ fprintf(fp, "%lld ", (INT64)BS_Alloc_Size(bs)); BS_Print( bs, fp ); }

}; // end BB_NODE_SET 


class BB_NODE_SET_ITER {
private:
  BS *bs;                // pointer to the bs
  BS_ELT cur;
  const CFG    *cfg;
  BB_NODE_SET_ITER(const BB_NODE_SET_ITER&);
  BB_NODE_SET_ITER& operator = (const BB_NODE_SET_ITER&);

public:
           BB_NODE_SET_ITER(void)	{ bs = NULL; }
           ~BB_NODE_SET_ITER(void)	{}
  void     Init(BB_NODE_SET *bns)	{ bs = bns->Bs(); cfg = bns->Cfg(); }

  BB_NODE *Id2bb(const BS_ELT id) const 
    { return id != BS_CHOOSE_FAILURE ? cfg->Get_bb(id) : NULL; }

  BS_ELT   First(void)            { return cur = BS_Choose(bs); }
  BS_ELT   Next(void)             { return cur = BS_Choose_Next(bs, cur); }
  BOOL     Is_Empty(void) const   { return (cur == BS_CHOOSE_FAILURE); }
  BB_NODE *First_elem(void)       { return Id2bb(First()); }
  BB_NODE *Next_elem(void)        { return Id2bb(Next()); }
};


#endif // bb_node_set_INCLUDED

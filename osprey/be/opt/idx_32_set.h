//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: idx_32_set.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/idx_32_set.h,v $
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
//  This package implements sets of IDX_32's.  These can be
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
//  The IDX_32_SET class provides methods that work either on the
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
//  allocation for IDX_32_SET's.  This is achieved by passing storage
//  allocation pools (MEM_POOLs) to the functions that may need to use
//  them.
//
//
//
//  Types
//  =====
//
//
//  CLASS IDX_32_SET
//
//  TYPE IDX_32
//
//      This is the type of an element of a IDX_32_SET.
//
//
//
//  Constructors, Clearing, and Freeing
//  ===============================
//
//
//  IDX_32_SET( 
//    BS_ELT size,
//    MEM_POOL *pool,
//    OPTS_ACTION action 
//  )
//
//	Creates an instance of a set, that can hold any IDX_32 with
//	a value in the range 0..size-1.  'size' must be non-negative.
//	The memory pool is where the memory for the bitset will be
//	allocated.
//	'action' is one of:
//	  OPTS_FALSE	: Clear the set on creation
//	  OPTS_TRUE	: Fill the set on creation
//	  OPTS_DONT_CARE: Leave the set uninitialized (caller should make
//			  sure he will initialize it
//
//
//  IDX_32_SET( 
//    BS_ELT low, 
//    BS_ELT high, 
//    MEM_POOL *pool 
//  )
//
//	Creates an instance of a set with members low..high
//
//
//  IDX_32_SET( 
//    const IDX_32 elt, 
//    MEM_POOL *pool
//  )
//
//	Creates an instance of a set with the sole member being 'elt'
//
//
//  IDX_32_SET *ClearD()
//
//      Destructive clear operation.  After this 'set' will be empty.
//      However, this does not change the allocated size of the set
//      (it will still be able to contain the same members that it
//      could before it was cleared without expansion.)
//
//
//  IDX_32_SET *ResizeD( BS_ELT new_size )
//
//      Destructive resize operation.  After this 'set' will be large
//      enough to hold elt new_size - 1.
//
//
//  IDX_32_SET *RangeD(
//    BS_ELT      low,
//    BS_ELT      high,
//  )
//
//      Returns a set whose members are the BBs with value values of the
//      numbers 'low' ... 'high'.  Both 'low' and the size of the
//      range must be nonnegative or an error is caused.  I.e., 'low'
//      >= 0 and ('high' - 'low' + 1) >= 0.  Note that 'high' may be
//      -1 if 'low' is 0.
//
//
//  IDX_32_SET *SingletonD(
//    IDX_32 element,
//  )
//
//      Returns a set with 'element' as its sole member.
//
//
//  IDX_32_SET *UniverseD(
//    BS_ELT    size,
//  )
//
//      Returns a set containing the BBs with values of the numbers
//      0...'size' - 1.  'Size' must be nonnegative or an error is
//      caused.
//
//
//
//  Copying
//  =======
//
//
//  IDX_32_SET *Copy(
//    MEM_POOL *pool
//  )
//
//	Creates a new instance of the given set, but allocated from
//	a different memory pool.
//
//  IDX_32_SET *CopyD(
//    IDX_32_SET   *set2,
//  )
//
//      The given instance's set becomes an exact copy of set2.
//
//
//  Set Operations
//  ==============
//
//
//  IDX_32 Choose()
//
//      Returns some element of 'set', if 'set' is nonempty.  Else
//      returns IDX_32_SET_CHOOSE_FAILURE.  In fact, this is defined so
//      that it always returns the least element of the set.
//
//
//  IDX_32 Choose_Next(
//    IDX_32 x
//  )
//
//      Returns the "next" element of 'set', starting after 'x', if
//	'set' is nonempty.  Else returns IDX_32_SET_CHOOSE_FAILURE.
//	This is used for looping over the elements of a set.
//
//
//  IDX_32 Choose_Range(
//    BS_ELT    low,
//    BS_ELT    high
//  )
//
//      Returns some element of 'set' whose value is in the
//      range low..high if there is one.  Else returns
//      IDX_32_SET_CHOOSE_FAILURE.  Both 'low' and the size of the 
//	range must be nonnegative or an error is caused.  
//	I.e., 'low' >= 0 and ('high' - 'low' + 1) >= 0.  Note that 
//	'high' may be -1 if 'low' is 0.  As with the Choose function, 
//	always returns the least element of the set that's in range.
//
//
//  IDX_32_SET *Difference(
//    IDX_32_SET   *set2,
//    MEM_POOL *pool
//  )
//
//      Creates a new instance and returns the given set with any
//	elements from set2 removed.
//
//  IDX_32_SET *DifferenceD(
//    IDX_32_SET *set2
//  )
//
//      Returns the given set with any elements from set2 removed.
//
//
//  IDX_32_SET *Difference1(
//    IDX_32 x,
//    MEM_POOL   *pool
//  )
//
//	Creates a new instance and returns the given set with element
//	x removed.
//
//  IDX_32_SET *Difference1D(
//    IDX_32 x
//  )
//
//      Removes the element x from the set.
//
//
//  IDX_32_SET *Intersection(
//    IDX_32_SET   *set2,
//    MEM_POOL *pool
//  )
//
//      Creates a new instance, allocated from pool, and returns the 
//	intersection with 'set2'.
//
//  IDX_32_SET *IntersectionD(
//    IDX_32_SET *set2
//  )
//
//      Intersects with set2.
//
//
//  IDX_32_SET *IntersectionR(
//    IDX_32_SET *set1,
//    IDX_32_SET *set2
//  )
//
//      'this' = set1 Intersect set2.
//
//
//  IDX_32_SET *Union(
//    IDX_32_SET   *set2,
//    MEM_POOL *pool
//  )
//      Creates a new instance, allocated from pool, and returns the 
//	union with 'set2'.
//
//  IDX_32_SET *UnionD(
//    IDX_32_SET   *set2,
//  )
//
//      Unions with set2.
//
//
//  IDX_32_SET *Union1(
//    IDX_32 x,
//    MEM_POOL   *pool
//  )
//
//	Creates a new instance, allocated from pool, and returns the
//	set with element x added.
//
//  IDX_32_SET *Union1D(
//    IDX_32 x 
//  )
//
//      Adds element x to the set.
//
//  IDX_32_SET *Bs_2_3_Or_1_Or_D(
//    IDX_32_SET *set1,
//    IDX_32_SET *set2 )
//
//	'this' is unioned with (set1 OR set2)
//	'this' = 'this' + (set1 + set2)
//
//
//  IDX_32_SET *Bs_2_1_Minus_3_Or_R(
//    IDX_32_SET *set1,
//    IDX_32_SET *set2,
//    IDX_32_SET *set3 )
//
//      'this' is set to (((NOT set1) AND set2) OR set3)
//
//
//  IDX_32_SET *Bs_3_2_Minus_1_Or_D(
//    IDX_32_SET *set1,
//    IDX_32_SET *set2 )
//
//      'this' is set to (((NOT set1) AND set2) OR this)
//
//
//  IDX_32_SET *Bs_2_1_Minus_3_Or_4_And_5_And_6_And_R( 
//    IDX_32_SET *set1,
//    IDX_32_SET *set2,
//    IDX_32_SET *set3,
//    IDX_32_SET *set4,
//    IDX_32_SET *set5,
//    IDX_32_SET *set6 )
//
//      'this' is set to ((((NOT set1) AND set2) OR set3) AND set4 AND
//			  set5 AND set6)
//
//  IDX_32_SET *Bs_3_2_Minus_4_Or_1_Or_D( 
//    IDX_32_SET *set1,
//    IDX_32_SET *set2,
//    IDX_32_SET *set3 )
//
//	'this' is unioned with (((NOT set1) AND set2) OR set3)
//	'this' = 'this' + (set2 - set1) + set3
//
//
//  IDX_32_SET *Bs_3_2_Minus_4_Or_5_Or_1_Or_D( 
//    IDX_32_SET *set1,
//    IDX_32_SET *set2,
//    IDX_32_SET *set3,
//    IDX_32_SET *set4 )
//
//	'this' is unioned with ((((NOT set1) AND set2) OR set3) OR set4)
//	'this' = 'this' + (set2 - set1) + set3 + set4
//
//
//  IDX_32_SET *Pp1( 
//    IDX_32_SET *set1,
//    IDX_32_SET *set2,
//    IDX_32_SET *set3,
//    IDX_32_SET *set4 )
//
//      'this' is set to ((((NOT set1) AND set2) OR set3) AND set4)
//
//
//  IDX_32_SET *Bs_1_Not_2_Or_3_Minus_4_And_R( 
//    IDX_32_SET *set1,
//    IDX_32_SET *set2,
//    IDX_32_SET *set3,
//    IDX_32_SET *set4 )
//
//      'this' is set to ((NOT set1) OR set2) AND (NOT set3) AND set4
//
//
//  IDX_32_SET *Bs_1_2_Or_3_And_R( 
//    IDX_32_SET *set1,
//    IDX_32_SET *set2,
//    IDX_32_SET *set3 )
//
//      'this' is set to ((set1 OR set2) AND set3)
//
//
//  IDX_32_SET *Pp2( 
//    IDX_32_SET *set1,
//    IDX_32_SET *set2 )
//
//      'this' is set to ((set1 OR set2) AND this)
//
//  IDX_32_SET *Bs_3_Not_4_Or_2_And_1_Or_D( 
//    IDX_32_SET *set1,
//    IDX_32_SET *set2,
//    IDX_32_SET *set3 )
//
//      'this' is set to (this OR set1 AND ((not set2) OR set3))
//
//  IDX_32_SET *Bs_4_3_Minus_2_Not_Or_1_And_D( 
//    IDX_32_SET *set1,
//    IDX_32_SET *set2,
//    IDX_32_SET *set3 )
//
//      'this' is set to (this AND ((not set1) OR (not set2) AND set3))
//
//  IDX_32_SET *Bs_2_3_Minus_1_Or_D( 
//    IDX_32_SET *set1,
//    IDX_32_SET *set2 )
//
//      'this' is set to (this OR set1 AND (not set2))
//
//  IDX_32_SET *Bs_2_3_Minus_4_Minus_1_Or_D( 
//    IDX_32_SET *set1,
//    IDX_32_SET *set2,
//    IDX_32_SET *set3 )
//
//      'this' is set to (this OR (set1 AND (not set2) AND (not set3)))
//
//  size_t IDX_32_SET_Size()
//
//	Returns the cardinality (population count) of the set.
//
//
//  Testing Sets
//  ============
//
//
//  BOOL ContainsP(
//    IDX_32_SET *set2
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
//    IDX_32_SET *set2
//  )
//
//      Returns TRUE iff the set has exactly the same members as
//      'set2'.  Else FALSE.
//
//
//  BOOL IntersectsP(
//    IDX_32_SET *set2
//  )
//
//      Returns TRUE iff the set and 'set2' have at least one member in
//      common.  Else FALSE.
//
//
//  BOOL MemberP(
//   IDX_32 x
//  )
//
//      Returns TRUE iff 'x' is a member of the set.  Else FALSE.
//
//
//  Looping Over Members
//  ====================
//
//
//  This is done using Choose and Choose_Next.  Here is an example:
//
//	IDX_32 x;
//	IDX_32_SET *set;
//
//	for ( x = set->Choose();
//	      x != IDX_32_SET_CHOOSE_FAILURE;
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
//    IDX_32_SET     *set2,
//    IDX_32 x
//  )
//
//      Returns TRUE iff 'x' is a member of the intersection of the set
//	and 'set2' .  Else FALSE.
//
//
//  IDX_32 Intersection_Choose(
//    IDX_32_SET *set2
//  )
//
//      Returns some element of of the intersection of the set and 
//	'set2', if the intersection is nonempty.  Else returns
//      IDX_32_SET_CHOOSE_FAILURE.  In fact, this is defined so that 
//	it always returns the least element of the set.
//
//
//  IDX_32 Intersection_Choose_Next(
//    IDX_32_SET     *set2,
//    IDX_32 x
//  )
//
//      Returns the "next" element of the intersection of the set and
//      'set2', starting after 'x', if there is such a member.  Else
//      returns IDX_32_SET_CHOOSE_FAILURE.  This is very useful for 
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

#ifndef idx_32_set_INCLUDED
#define idx_32_set_INCLUDED      "idx_32_set.h"
#ifdef _KEEP_RCS_ID
static char *idx_32_set_rcs_id = idx_32_set_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

#include "mempool.h"	// our clients should probably include this
#include "bitset.h"	// our clients do not need to know about it
#include "errors.h"     // for ErrMsg() ...
#include "erglob.h"     // for EC_Misc_Int ...

#define IDX_32_SET_CHOOSE_FAILURE ((IDX_32)BS_CHOOSE_FAILURE)

// action to use in constructor to decide how the bitset should be
// initialized
#ifndef OPTS_ACTION_DEF
#define OPTS_ACTION_DEF
enum OPTS_ACTION {
  OPTS_FALSE,		// clear set when creating
  OPTS_TRUE,		// fill set when creating
  OPTS_DONT_CARE,	// leave set uninitialized
};
#endif // OPTS_ACTION_DEF

class IDX_32_SET {
private:
  BS 	    *_bs;	// the actual bitset
  MEM_POOL  *_mempool;	// memory pool to use as default

  // constructor to override default (never used for this class)
  IDX_32_SET(void)		{}
  IDX_32_SET(const IDX_32_SET&);
  IDX_32_SET& operator = (const IDX_32_SET&);

  // constructor to create set with no BS
  IDX_32_SET( MEM_POOL *pool )
		{
		  _mempool = pool;
		}

  // function to convert from bit position into IDX_32
  IDX_32 id2elt( BS_ELT id ) const
		{ return (IDX_32)id; }

public:
  // constructor to create empty set with given size
  IDX_32_SET( BS_ELT size, MEM_POOL *pool, OPTS_ACTION action )
		{ 
		  _mempool = pool;
		  switch ( action ) {
		    case OPTS_FALSE:
		      _bs = BS_Create_Empty( size, pool );
		      break;
		    case OPTS_TRUE:
		      _bs = BS_Universe( size, pool );
		      break;
		    case OPTS_DONT_CARE:
		      _bs = BS_Create( size, pool );
		      break;
		    default:
		      ErrMsg( EC_Misc_Int, "IDX_32_SET: action",
			      (INT) action );
		      break;
		  }
		}

  // constructor to create set with elements low..high set
  IDX_32_SET( BS_ELT low, BS_ELT high, MEM_POOL *pool )
		{ 
		  _mempool = pool;
		  _bs = BS_Range( low, high, pool );
		}

  // constructor to create set with single element elt included
  IDX_32_SET( const IDX_32 elt, MEM_POOL *pool)
		{ 
		  _mempool = pool;
		  _bs = BS_Singleton( elt, pool );
		}

 // destructor
 ~IDX_32_SET(void)		{}

  BS          *Bs(void) const   { return _bs; }

  IDX_32_SET *ClearD( void )
		{ _bs = BS_ClearD(_bs);
		  return this; 
		}

  IDX_32_SET *ResizeD( BS_ELT new_size )
		{ _bs = BS_ResizeD( _bs, new_size, _mempool );
		  return this;
		}

  IDX_32_SET *RangeD( BS_ELT low, BS_ELT high )
		{ _bs = BS_RangeD( _bs, low, high, _mempool );
		  return this;
		}

  IDX_32_SET *SingletonD( const IDX_32 elt )
		{ _bs = BS_SingletonD( _bs, elt, _mempool );
		  return this;
		}

  IDX_32_SET *UniverseD( BS_ELT size )
		{ _bs = BS_UniverseD( _bs, size, _mempool );
		  return this;
		}

  IDX_32_SET *Copy( MEM_POOL *pool )
		{ IDX_32_SET *new1 = CXX_NEW(IDX_32_SET(pool),pool);
		  new1->_bs = BS_Copy( _bs, pool );
		  return new1;
		}

  IDX_32_SET *CopyD( const IDX_32_SET *set1 )
		{ _bs = BS_CopyD( _bs, set1->_bs, _mempool );
		  return this;
		}


// To be obsolete.   Replace by iterators.

  IDX_32 Choose( void ) const
		{ return id2elt(BS_Choose(_bs)); }

  IDX_32 Intersection_Choose( const IDX_32_SET *set1 ) const
		{ return id2elt(BS_Intersection_Choose(_bs,set1->_bs)); }

  IDX_32 Choose_Next( const IDX_32 elt ) const
		{ return id2elt(BS_Choose_Next(_bs,elt)); }

  IDX_32 Intersection_Choose_Next( const IDX_32_SET *set1,
				     const IDX_32 elt ) const
		{ return id2elt(
		  BS_Intersection_Choose_Next(_bs,set1->_bs,elt) );
		}

  IDX_32 Choose_Range( BS_ELT low, BS_ELT high ) const
		{ return id2elt(BS_Choose_Range(_bs,low,high)); }

//

  IDX_32_SET *Difference( const IDX_32_SET *set1, MEM_POOL *pool )
		{ IDX_32_SET *new1 = CXX_NEW(IDX_32_SET(pool),pool);
		  new1->_bs = BS_Difference( _bs, set1->_bs, pool );
		  return new1;
		}

  IDX_32_SET *DifferenceD( const IDX_32_SET *set1 )
		{ _bs = BS_DifferenceD( _bs, set1->_bs );
		  return this;
		}

  IDX_32_SET *Difference1( const IDX_32 elt, MEM_POOL *pool )
		{ IDX_32_SET *new1 = CXX_NEW(IDX_32_SET(pool),pool);
		  new1->_bs = BS_Difference1( _bs, elt, pool );
		  return new1;
		}

  IDX_32_SET *Difference1D( const IDX_32 elt )
		{ _bs = BS_Difference1D( _bs, elt );
		  return this;
		}


  IDX_32_SET *Intersection( const IDX_32_SET *set1, MEM_POOL *pool )
		{ IDX_32_SET *new1 = CXX_NEW(IDX_32_SET(pool),pool);
		  new1->_bs = BS_Intersection( _bs, set1->_bs, pool );
		  return new1;
		}

  IDX_32_SET *IntersectionD( const IDX_32_SET *set1 )
		{ _bs = BS_IntersectionD( _bs, set1->_bs );
		  return this;
		}

  IDX_32_SET *IntersectionR(  const IDX_32_SET *set1, 
			      const IDX_32_SET *set2 )
		{ _bs = BS_IntersectionR( _bs, set1->_bs, set2->_bs );
		  return this;
		}

  BS_ELT Size( void ) const
		{ return BS_Size(_bs); }

  IDX_32_SET *Union( const IDX_32_SET *set1, MEM_POOL *pool )
		{ IDX_32_SET *new1 = CXX_NEW(IDX_32_SET(pool),pool);
		  new1->_bs = BS_Union( _bs, set1->_bs, pool );
		  return new1;
		}

  IDX_32_SET *UnionD( const IDX_32_SET *set1 )
		{ _bs = BS_UnionD( _bs, set1->_bs, _mempool );
		  return this;
		}

  IDX_32_SET *Union1( const IDX_32 elt, MEM_POOL *pool )
		{ IDX_32_SET *new1 = CXX_NEW(IDX_32_SET(pool),pool);
		  new1->_bs = BS_Union1( _bs, elt, pool );
		  return new1;
		}

  IDX_32_SET *Union1D( const IDX_32 elt )
		{ _bs = BS_Union1D( _bs, elt, _mempool );
		  return this;
		}

  IDX_32_SET *Bs_2_3_Or_1_Or_D(
    const IDX_32_SET *set1,
    const IDX_32_SET *set2 )
		{ _bs = BS_2_3_Or_1_Or_D(this->_bs, set1->_bs,set2->_bs,
				      _mempool);
		  return this;
		}

  IDX_32_SET *Bs_2_1_Minus_3_Or_R(
    const IDX_32_SET *set1,
    const IDX_32_SET *set2,
    const IDX_32_SET *set3 )
		{ _bs = BS_2_1_Minus_3_Or_R(this->_bs,
					 set1->_bs,set2->_bs,set3->_bs,
					 _mempool);
		  return this;
		}

  IDX_32_SET *Bs_3_2_Minus_1_Or_D(
    const IDX_32_SET *set1,
    const IDX_32_SET *set2 )
		{ _bs = BS_3_2_Minus_1_Or_D(this->_bs,
					 set1->_bs,set2->_bs,
					 _mempool);
		  return this;
		}

  IDX_32_SET *Bs_2_1_Minus_3_Or_4_And_5_And_6_And_R(
    const IDX_32_SET *set1,
    const IDX_32_SET *set2,
    const IDX_32_SET *set3,
    const IDX_32_SET *set4,
    const IDX_32_SET *set5,
    const IDX_32_SET *set6 )
		{ _bs = BS_2_1_Minus_3_Or_4_And_5_And_6_And_R( this->_bs,
			set1->_bs, set2->_bs, set3->_bs, 
			set4->_bs, set5->_bs, set6->_bs, _mempool );
		  return this;
		}

  IDX_32_SET *Bs_3_2_Minus_4_Or_1_Or_D(
    const IDX_32_SET *set1,
    const IDX_32_SET *set2,
    const IDX_32_SET *set3 )
		{ _bs = BS_3_2_Minus_4_Or_1_Or_D( this->_bs,
			set1->_bs, set2->_bs, set3->_bs, _mempool );
		  return this;
		}

  IDX_32_SET *Bs_3_2_Minus_4_Or_5_Or_1_Or_D(
    const IDX_32_SET *set1,
    const IDX_32_SET *set2,
    const IDX_32_SET *set3,
    const IDX_32_SET *set4 )
		{ _bs = BS_3_2_Minus_4_Or_5_Or_1_Or_D( this->_bs,
			set1->_bs, set2->_bs, set3->_bs, set4->_bs, 
			_mempool );
		  return this;
		}

  IDX_32_SET *Pp1(
    const IDX_32_SET *set1,
    const IDX_32_SET *set2,
    const IDX_32_SET *set3,
    const IDX_32_SET *set4 )
		{ _bs = BS_2_1_Minus_3_Or_4_And_R( this->_bs,
			set1->_bs, set2->_bs, set3->_bs, 
			set4->_bs, _mempool );
		  return this;
		}

  IDX_32_SET *Bs_1_Not_2_Or_3_Minus_4_And_R( 
    const IDX_32_SET *set1,
    const IDX_32_SET *set2,
    const IDX_32_SET *set3,
    const IDX_32_SET *set4 )
		{ _bs = BS_1_Not_2_Or_3_Minus_4_And_R( this->_bs,
			  set1->_bs, set2->_bs, set3->_bs, set4->_bs,
			  _mempool );
		  return this;
		}

  IDX_32_SET *Bs_1_2_Or_3_And_R(
    const IDX_32_SET *set1,
    const IDX_32_SET *set2,
    const IDX_32_SET *set3 )
		{ _bs = BS_1_2_Or_3_And_R(this->_bs,
			set1->_bs,set2->_bs,set3->_bs, _mempool);
		  return this;
		}

  IDX_32_SET *Pp2(
    const IDX_32_SET *set1,
    const IDX_32_SET *set2 )
		{ _bs = BS_1_2_Or_3_And_R(this->_bs,
			set1->_bs,set2->_bs,this->_bs, _mempool);
		  return this;
		}

  IDX_32_SET *Bs_3_Not_4_Or_2_And_1_Or_D( 
    const IDX_32_SET *set1,
    const IDX_32_SET *set2,
    const IDX_32_SET *set3 )
		{ _bs = BS_3_Not_4_Or_2_And_1_Or_D(this->_bs,
			set1->_bs,set2->_bs,set3->_bs, _mempool);
		  return this;
		}

  IDX_32_SET *Bs_4_3_Minus_2_Not_Or_1_And_D(
    const IDX_32_SET *set1,
    const IDX_32_SET *set2,
    const IDX_32_SET *set3 )
		{ _bs = BS_4_3_Minus_2_Not_Or_1_And_D(this->_bs,
			set1->_bs,set2->_bs,set3->_bs, _mempool);
		  return this;
		}

  IDX_32_SET *Bs_2_3_Minus_1_Or_D( 
    const IDX_32_SET *set1,
    const IDX_32_SET *set2 )
		{ _bs = BS_2_3_Minus_1_Or_D(this->_bs,
			set1->_bs,set2->_bs, _mempool);
		  return this;
		}

  IDX_32_SET *Bs_2_3_Minus_4_Minus_1_Or_D( 
    const IDX_32_SET *set1,
    const IDX_32_SET *set2,
    const IDX_32_SET *set3 )
		{ _bs = BS_2_3_Minus_4_Minus_1_Or_D(this->_bs,
			set1->_bs,set2->_bs,set3->_bs, _mempool);
		  return this;
		}

  BOOL ContainsP( const IDX_32_SET *set1 ) const
		{ return BS_ContainsP( _bs, set1->_bs ); }

  BOOL EmptyP( void ) const
		{ return BS_EmptyP( _bs ); }

  BOOL EqualP( const IDX_32_SET *set1 ) const
		{ return BS_EqualP( _bs, set1->_bs ); }

  BOOL IntersectsP( const IDX_32_SET *set1 ) const
		{ return BS_IntersectsP( _bs, set1->_bs ); }

  BOOL MemberP( const IDX_32 elt )  const
		{ return BS_MemberP( _bs, elt ); }

  BOOL Intersection_MemberP( const IDX_32_SET *set1, const IDX_32 elt) const
		{ return BS_Intersection_MemberP(_bs,set1->_bs,elt);}

  void Print( FILE *fp=stderr ) const
		{ fprintf(fp, "%lld ", (INT64)BS_Alloc_Size(_bs)); BS_Print( _bs, fp ); }

}; // end IDX_32_SET 


class IDX_32_SET_ITER {
private:
  BS *_bs;                // pointer to the bs
  BS_ELT cur;
  IDX_32_SET_ITER(const IDX_32_SET_ITER&);
  IDX_32_SET_ITER& operator = (const IDX_32_SET_ITER&);

public:
           IDX_32_SET_ITER(void)	{ _bs = NULL; }
           ~IDX_32_SET_ITER(void)	{}
  void     Init(IDX_32_SET *bns)	{ _bs = bns->Bs(); }

  BS_ELT   First(void)            { return cur = BS_Choose(_bs); }
  BS_ELT   Next(void)             { return cur = BS_Choose_Next(_bs, cur); }
  BOOL     Is_Empty(void) const   { return (cur == BS_CHOOSE_FAILURE); }
  IDX_32   First_elem(void)       { return First(); }
  IDX_32   Next_elem(void)        { return Next(); }
};


#endif // idx_32_set_INCLUDED

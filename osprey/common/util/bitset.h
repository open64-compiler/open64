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


#ifndef bitset_INCLUDED
#define bitset_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: bitset.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:17:57 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/bitset.h,v $
 *
 * Revision history:
 *  05-01-93 - Original Version
 *
 * Description:
 *
 *      Bitset interface.  See below for details.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *xxx_rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/bitset.h,v $ $Revision: 1.1 $";
#endif /* _KEEP_RCS_ID */

/* This package implements sets of nonnegative INTs.  These can be
 * manipulated with a fairly complete repertoire of set functions.
 * The sets are represented with bit strings for efficiency of both
 * space and run time.  The vector that represents these sets is grown
 * as necessary to accommodate the results of the various operations.
 * In spite of this, the client of the package retains control over
 * storage allocation by providing memory allocation pools
 * (MEM_POOLs).  The representations are never automatically trimmed,
 * so that the representation of any given bit set will be large
 * enough to hold the largest number that it ever held.
 *
 *
 *
 * Destructive Operations Conventions
 * ==================================
 *
 *
 * This package contains a number of functions that implement
 * destructive operations.  The purpose of these operations is
 * efficiency both of the operations themselves and of memory usage.
 * Some of the destructive operations may still need to expand the
 * storage allocated to the set.  When this happens, the set may need
 * to be copied.  Thus you can not count on the side effects of these
 * operations, only on the correctness of their returned values.
 * Functions in this package that can have a destructive effect on a
 * one of their arguments always have a name that ends with the letter
 * D.  Only the first argument of such functions is ever destructively
 * modified.  All destructive operations return a pointer to the set.
 * In the normal case this will be the same as their first argument,
 * but sometimes the set will have to be copied in order to perform
 * the operation.  So the client should not rely on the side effects
 * of destructive operations.  Instead, one should use their returned
 * values.  For now, destructive operations only expand storage, and
 * never shrink it.
 *
 *
 * Replacement Operations Conventions
 * ==================================
 *
 *
 * This package implements a number of "replacement" operations.
 * These are very similar to the "destructive" versions, but also take
 * a pointer to the result, which is not used in the calculation.
 * These all end with the letter R. 
 *
 *
 * Storage Allocation
 * ==================
 *
 *
 * The client of this package has full control over storage allocation
 * for BS's.  This is achieved by passing storage allocation pools
 * (MEM_POOLs) to the functions that may need to use them.  All
 * storage allocated for a set is "flat", which is to say that from
 * the point of view of storage allocation, each BS may be seen as a
 * single array containing no pointers to additional storage.  This
 * allows the client to free this storage directly if its allocation
 * pool supports freeing (See the discussion under BS_Create below).
 *
 *
 *
 * Types
 * =====
 *
 *
 * TYPE struct bs BS
 *
 *     This is the client visible type of a BS.  It has no client
 *     visible fields.
 *
 *
 * TYPE BS_ELT
 *
 *      This is the type of an element of a BS.  It is a numeric type
 *      with the range 0..INT_MAX-1
 *
 *
 * Creation, Clearing, and Freeing
 * ===============================
 *
 *
 * BS *BS_Create( BS_ELT size, MEM_POOL *pool )
 *
 *     Creates and returns a new BS capable of holding (without
 *     expansion) any set of integers in the range 0 through size - 1.
 *     'Size' must be nonnegative or an error is caused.  The newly
 *     created BS is uninitialized, and may contain any of the
 *     possible sets.  'Pool' is used to allocate the space for the
 *     set.  Storage for the space is "flat", that is the set is
 *     allocated as a single array and contains no pointers to any
 *     additionally allocated memory.  The client is thus free to free
 *     the BS directly (if 'pool' supports this.)  (the allocated size
 *     may be obtained from BS_Alloc_Size).
 *
 *
 * size_t BS_Size_Alloc_Size( BS_ELT size )
 *
 *     Returns the number of bytes which would be required to allocate
 *     a set of 'size' size, i.e., the minimum number of bytes required
 *     to hold a set containing the elements 0 through size - 1.
 *
 *
 * size_t BS_Alloc_Size( BS *set )
 *
 *     Returns the number of bytes used to allocate the 'set'.
 *
 *
 * BS *BS_ClearD( BS *set )
 *
 *     Destructive clear operation.  After this 'set' will be empty.
 *     However, this does not change the allocated size of the set (it
 *     will still be able to contain the same members that it could
 *     before it was cleared without expansion.)
 *
 *
 * BS *BS_Create_Empty( BS_ELT size, MEM_POOL *pool )
 *
 *     Create an empty set large enough to hold the element 'size' - 1
 *     without expansion.  Equivalent to
 *
 *         BS_ClearD( BS_Create( size ), pool )
 *
 *
 * BS *BS_ResizeD( BS* set, BS_ELT new_size, MEM_POOL *pool )
 *
 *     Resize the set to be large enough to hold element 'new_size' - 1
 *     without expansion.  Similar to creating an empty set of the
 *     new_size, and then copying the set to it.
 *
 *
 * BS *BS_Range( BS_ELT low, BS_ELT high, MEM_POOL *pool )
 * BS *BS_RangeD( BS* set, BS_ELT low, BS_ELT high, MEM_POOL *pool )
 *
 *     Returns a set whose members are 'low' ... 'high'.  Both 'low'
 *     and the size of the range must be nonnegative or an error is
 *     caused.  I.e., 'low' >= 0 and ('high' - 'low' + 1) >= 0.  Note
 *     that 'high' may be -1 if 'low' is 0.
 *
 *
 * BS *BS_Singleton( BS_ELT element, MEM_POOL *pool )
 * BS *BS_SingletonD( BS *set, BS_ELT element, MEM_POOL *pool )
 *
 *     Returns a set with 'element' as its sole member.
 *
 *
 * BS *BS_Universe( BS_ELT size, MEM_POOL *pool )
 * BS *BS_UniverseD( BS *set, BS_ELT size, MEM_POOL *pool )
 *
 *     Returns a set containing 0...'size' - 1.  'Size' must be
 *     nonnegative or an error is caused.
 *
 *
 *
 * Copying
 * =======
 *
 *
 * BS *BS_Copy( BS *set, MEM_POOL *pool )
 * BS *BS_CopyD( BS *set1, BS *set2, MEM_POOL *pool )
 *
 *     Returns an exact copy of set.  Note that for BS_CopyD, if storage is
 *     allocated it will be the same as the storage actually allocated to
 *     set2, regardless of the current size of set2.  Thus the following
 *     sequence:
 *
 *         BS *set1, set2;
 *         set1 = BS_Create( 32, pool );
 *         set2 = BS_Create( 1024, pool );
 *         set2 = BS_ClearD( set2 );
 *         set1 = BS_CopyD( set1, set2, pool );
 *
 *     will result in set1 being grown to be large enough to contain
 *     the integer 1023, even though it will be empty.
 *
 *
 * Set Operations
 * ==============
 *
 *
 * BS_ELT BS_Choose( BS *set )
 *
 *     Returns some element of 'set', if 'set' is nonempty.  Else rerturns
 *     BS_CHOOSE_FAILURE.  In fact, this is defined so that it always
 *     returns the least element of the set.
 *
 *
 * BS_ELT BS_Intersection_Choose( BS *set1, BS *set2 )
 *
 *     Returns some element of the intersection of 'set1' and 'set2', if
 *     this intersection is nonempty.  Else rerturns BS_CHOOSE_FAILURE.
 *     In fact, this is defined so that it always returns the least
 *     element of the set.
 *
 *
 * BS_ELT BS_Choose_Range( BS *set, BS_ELT low, BS_ELT high )
 *
 *     Returns some element of 'set' intersection {low...high} if
 *     there is one.  Else returns BS_CHOOSE_FAILURE.  Both 'low' and
 *     the size of the range must be nonnegative or an error is
 *     caused.  I.e., 'low' >= 0 and ('high' - 'low' + 1) >= 0.  Note
 *     that 'high' may be -1 if 'low' is 0.  As with the _Choose
 *     function, always returns the last element of the set that's in
 *     range.
 *
 *
 * CONST BS_ELT BS_CHOOSE_FAILURE
 *
 *     A special value that BS_Choose and BS_Choose_Range return when they
 *     are unable to choose an element.
 *
 *
 * BS *BS_Difference( BS *set1, BS *set2, MEM_POOL *pool )
 * BS *BS_DifferenceD( BS *set1, BS *set2 )
 *
 *     Returns { x : member( x, 'set1' ) & ~ member( x, 'set2' ) }.
 *
 *
 * BS *BS_Difference1( BS *set, BS_ELT x, MEM_POOL *pool )
 * BS *BS_Difference1D( BS *set, BS_ELT x )
 *
 *     Returns { y : member( y , set ) & ~ ( y = x ) }.  X must be
 *     nonnegative or an error is caused.
 *
 *
 * BS *BS_Intersection( BS *set1, BS *set2, MEM_POOL *pool )
 * BS *BS_IntersectionD( BS *set1, BS *set2 )
 * BS *BS_IntersectionR( BS *result, BS *set1, BS *set2 )
 *
 *     Returns the intersection of 'set1' and 'set2'.
 *
 *
 * BS_ELT BS_Size( BS *set )
 *
 *     Returns the cardinality of 'set'.
 *
 *
 * BS *BS_Union( BS *set1, BS *set2, MEM_POOL *pool )
 * BS *BS_UnionD( BS *set1, BS *set2, MEM_POOL *pool )
 * BS *BS_UnionR( BS *result, BS *set1, BS *set2, MEM_POOL *pool )
 *
 *     Returns the union of set1 and set2.
 *
 *
 * BS *BS_Union1( BS *set, BS_ELT x, MEM_POOL *pool )
 * BS *BS_Union1D( BS *set, BS_ELT x, MEM_POOL *pool )
 *
 *     Returns set union { x }.  X must be nonnegative or an error is caused.
 *
 *
 * BS* BS_UnionD_Intersection( BS* set1, BS* set2, BS* set3, MEM_POOL* pool )
 *
 *      Destructively unions into set1 the intersection of set2 and set3,
 *      growing it if necessary in the given pool.
 *      
 *
 * BS *BS_2_1_Minus_3_Or_R(BS *result,BS *set1,BS *set2,BS *set3, MEM_POOL *pool)
 *
 *	Returns result = ~set1 & set2 | set3
 *
 *
 * BS *BS_3_2_Minus_1_Or_D(BS *set1,BS *set2,BS *set3, MEM_POOL *pool)
 *
 *	Returns set1 = ~set2 & set3 | set1
 *
 *
 * BS *BS_3_2_Minus_4_Or_1_Or_D(BS *set1,BS *set2,BS *set3, BS *set4, 
			    MEM_POOL *pool)
 *
 *	Returns set1 |= (~set2 & set3) | set4
 *
 *
 * BS *BS_3_2_Minus_4_Or_5_Or_1_Or_D(BS *set1,BS *set2,BS *set3, 
			       BS *set4, BS *set5, MEM_POOL *pool)
 *
 *	Returns set1 |= (~set2 & set3) | set4 | set5
 *
 *
 * BS *BS_2_1_Minus_3_Or_4_And_5_And_6_And_R( BS *result, BS *set1, BS *set2, BS *set3,
 *						 BS *set4, BS *set5, BS *set6,
 *				     MEM_POOL *pool)
 *
 *	Returns result = (~set1 & set2 | set3) & set4 & set5 & set6
 *
 *
 * BS *BS_2_1_Minus_3_Or_4_And_R( BS *result, BS *set1, BS *set2, BS *set3,
 *                                       BS *set4,
 *				     MEM_POOL *pool)
 *
 *	Returns result = (~set1 & set2 | set3) & set4
 *
 *
 * BS *BS_1_Not_2_Or_3_Minus_4_And_R( BS *result, 
 *				BS *set1, BS *set2, BS *set3, BS *set4,
 *				MEM_POOL *pool )
 *
 *	Returns result = ((~set1) | set2) & (~set3) & set4
 *
 *
 * BS *BS_2_3_Or_1_Or_D(BS *set1,BS *set2,BS *set3, MEM_POOL *pool)
 *
 *	Returns set1 |= (set2 | set3)
 *
 *
 * BS *BS_1_2_Or_3_And_R(BS *result,BS *set1,BS *set2,BS *set3, MEM_POOL *pool)
 *
 *	Returns result = (set1 | set2) & set3
 *
 *
 * BS *BS_2_3_And_1_Or_D(BS *set1,BS *set2,BS *set3, MEM_POOL *pool)
 *
 *	Returns set1 = set1 | (set2 & set3)
 *
 *
 * BS *BS_3_Not_4_Or_2_And_1_Or_D(BS *set1,BS *set2,BS *set3,BS *set4, MEM_POOL *pool)
 *
 *	Returns set1 = set1 | (set2 * (~set3 | set4))
 *
 *
 * BS *BS_4_3_Minus_2_Not_Or_1_And_D(BS *set1,BS *set2,BS *set3,BS *set4, MEM_POOL *pool)
 *
 *	Returns set1 = set1 * (~set2 | (set4 * ~set3))
 *
 *
 * BS *BS_2_3_Minus_1_Or_D(BS *set1,BS *set2,BS *set3, MEM_POOL *pool)
 *
 *	Returns set1 = set1 | (set2 * ~set3)
 *
 *
 * BS *BS_2_3_Minus_4_Minus_1_Or_D(BS *set1,BS *set2,BS *set3,BS *set4, MEM_POOL *pool)
 *
 *	Returns set1 = set1 | (set2 * ~set3 * ~set4)
 *
 *
 *
 * Testing Sets
 * ============
 *
 *
 * BOOL BS_ContainsP( BS *set1, BS *set2 )
 *
 *     Returns TRUE iff every element of 'set2' is in 'set1'.  Else FALSE.
 *
 *
 * BOOL BS_EmptyP( BS *set )
 *
 *     Returns TRUE iff 'set' is empty.  Else FALSE.
 *
 *
 * BOOL BS_EqualP( BS *set1, BS *set2 )
 *
 *     Returns TRUE iff 'set1' has exactly the same members as 'set2'.
 *     Else FALSE.
 *
 *
 * BOOL BS_IntersectsP( BS *set1, BS *set2 )
 *
 *     Returns TRUE iff 'set1' and 'set2' have at least one member in
 *     common.  Else FALSE.
 *
 *
 * BOOL BS_MemberP( BS *set, BS_ELT x )
 *
 *     Returns TRUE iff 'x' is a member of 'set'.  Else FALSE.  'X'
 *     must be nonnegative or an error is caused.
 *
 * BOOL BS_Intersection_MemberP( BS *set1, BS *set2, BS_ELT x )
 *
 *     Returns TRUE iff 'x' is a member of the intersection of 'set1' and
 *     'set2'.
 *
 *
 * Looping Over Members
 * ====================
 *
 * BS_ELT BS_Choose_Next( BS *set, BS_ELT elt )
 *
 *     Returns the next element of 'set' after elt. If there is no such
 *     element, returns BS_CHOOSE_FAILURE.  This is useful for looping
 *     over the elements of a set.
 *
 *     Here's an example of how this is used:
 *
 *	  BS_ELT elt;
 *	  for ( elt = BS_Choose( set );
 *		elt != BS_CHOOSE_FAILURE;
 *		elt = BS_Choose_Next( set, elt ) )
 *	  {
 *	    elt is an element of the set
 *	  }
 *
 * Looping over Members of an Intersection
 * =======================================
 *
 * BS_ELT BS_Intersection_Choose_Next(  BS *set1, BS *set2, BS_ELT x )
 *
 *      Returns the first member after 'x' of the intersection of 'set1'
 *      and 'set2' or BS_CHOOSE_FAILURE if the intersection is empty.
 *
 * Printing Sets
 * =============
 *
 *
 * void BS_Print( BS *set, FILE *f )
 *
 *     Prints 'set' on 'f'.  The type FILE is as defined in stdio.h.
 */

#ifndef defs_INCLUDED
#include "defs.h"
#endif /* defs_INCLUDED */

typedef struct bs BS;
typedef INT32 BS_ELT;

struct bv;


/* Machine dependent definitions that will need to be changed for 64
 * bit implementation:
 */

/* The basic word storage unit of a BS (MUST BE UNSIGNED)
 */
#if defined(_MIPS_ISA) && _MIPS_ISA >= 3
typedef mUINT64 BS_WORD;
#define LOG2_BITS_PER_BS_WORD 6
#else
typedef mUINT32 BS_WORD;
#define LOG2_BITS_PER_BS_WORD 5
#endif

typedef mUINT8  BS_BYTE;

#define BITS_PER_BS_WORD    (sizeof(BS_WORD) * 8)
#define BYTES_PER_BS_WORD   (sizeof(BS_WORD))

/*
 *  bs_PBPW(x)      Product of (x * Bits Per Word)
 *  bs_QBPW(x)      Quotient of ( x / Bits Per Word)
 *  bs_RBPW(x)      Remainder of ( x / Bits Per Word)
 *  bs_PBPB(x)      Product of (x * Bits Per Byte)
 *  bs_QBPB(x)      Quotient of (x / Bits Per Byte)
 *  bs_RBPB(x)      Remainder of ( x / Bits Per Byte)
 *  bs_PBytesPW(x)  Product of (x * Bytes Per Word)
 */
#define bs_PBPW(x)  ((BS_ELT) ((x) << LOG2_BITS_PER_BS_WORD))
#define bs_QBPW(x)  ((BS_ELT) ((x) >> LOG2_BITS_PER_BS_WORD))
#define bs_RBPW(x)  ((BS_ELT) ((x) & ((1 << LOG2_BITS_PER_BS_WORD) - 1)))
#define bs_PBPB(x)  ((BS_ELT) ((x) << 3))
#define bs_QBPB(x)  ((BS_ELT) ((x) >> 3))
#define bs_RBPB(x)  ((BS_ELT) ((x) & 0x7))
#define bs_PBytesPW(x)  ((BS_ELT) ((x) * BYTES_PER_BS_WORD))

/* bs_ZEROS     a word of zeros
 * bs_ONES      a word of ones
 * bs_ONE       just 1
 */
#define bs_ZEROS    ((BS_WORD) 0)
#define bs_ONES     ((BS_WORD) ~0)
#define bs_ONE      ((BS_WORD) 1)

/* End of machine dependent part.
 */


/* Representation of a bit set is a pointer to a sequence of the
 * words. The first of these words gives the length of the set in
 * total number of words available to hold members (does not include
 * the first word which holds the length.)
 *
 * The sets are essentially BYTE addressed, with word addressing used
 * only to improve the effeciency of operations that must look at the
 * whole set.  This has two advantages (on machines with effecient
 * byte addressing such as MIPS).  Firstly, it is an endian
 * independent representation.  Secondly it allows us to implement
 * very effecient Find_First_One and Population_Count operations at
 * the byte level as each only requires a table of 256 entries.
 *
 * x is an element of set if
 *
 *      1. x < BITS_PER_BS_WORD * BS_word_count(x), and
 *      2. BS_byte(set,x / 8) & (1 << x & 0x7) != bs_ZEROS
 */

/* Basic access to BS's:
 *
 * BS_word_count(set)       Number of words allocated to hold elements
 *                          of set
 * BS_word(set,i)           i'th word of set
 * BS_byte(set,i)           i'th byte of set
 */
#define BS_word_count(x) (*((BS_WORD *) x))
#define BS_word(x,i)     (*(((BS_WORD *) x) + (i) + 1))
#define BS_byte(x,i)     (*(((unsigned char *) x) + (i) + sizeof(BS_WORD)))


/* Throw away a bit of address space to get a sane out of range
 * element.  This already doesn't matter and in the 64 bit environment
 * it really won't.
 */
#define BS_MIN_ELT ((BS_ELT) 0)
#define BS_MAX_ELT ((BS_ELT) UINT32_MAX)
#define BS_CHOOSE_FAILURE ((BS_ELT) -1)

extern BS *BS_Create( BS_ELT size, MEM_POOL *pool );
extern size_t BS_Size_Alloc_Size( BS_ELT size );
extern size_t BS_Alloc_Size( BS *set );
extern BS *BS_ClearD( BS *set );
extern BS *BS_Create_Empty( BS_ELT size, MEM_POOL *pool );
extern BS *BS_ResizeD( BS* set, BS_ELT new_size, MEM_POOL *pool );
extern BS *BS_Range( BS_ELT low, BS_ELT high, MEM_POOL *pool );
extern BS *BS_RangeD( BS* set, BS_ELT low, BS_ELT high, MEM_POOL *pool );
extern BS *BS_Singleton( BS_ELT element, MEM_POOL *pool );
extern BS *BS_SingletonD( BS *set, BS_ELT element, MEM_POOL *pool );
extern BS *BS_Universe( BS_ELT size, MEM_POOL *pool );
extern BS *BS_UniverseD( BS *set, BS_ELT size, MEM_POOL *pool );
extern BS *BS_Copy( BS *set, MEM_POOL *pool );
extern BS *BS_CopyD( BS *set1, BS *set2, MEM_POOL *pool );
extern BS_ELT BS_Choose( const BS *bs );
extern BS_ELT BS_Intersection_Choose( BS *set1, BS *set2 );
extern BS_ELT BS_Choose_Range( BS *set, BS_ELT low, BS_ELT high );
extern BS *BS_Difference( BS *set1, BS *set2, MEM_POOL *pool );
extern BS *BS_DifferenceD( BS *set1, BS *set2 );
extern BS *BS_Difference1( BS *set, BS_ELT x, MEM_POOL *pool );
extern BS *BS_Difference1D( BS *set, BS_ELT x );
extern BS *BS_Intersection( BS *set1, BS *set2, MEM_POOL *pool );
extern BS *BS_IntersectionD( BS *set1, BS *set2 );
extern BS *BS_IntersectionR(BS* result, const BS *set1, const BS *set2);
extern BS_ELT BS_Size( BS *set );
extern BS *BS_Union( BS *set1, BS *set2, MEM_POOL *pool );
extern BS *BS_UnionD( BS *set1, BS *set2, MEM_POOL *pool );
extern BS *BS_UnionR( BS *result, BS *set1, BS *set2, MEM_POOL *pool );
extern BS *BS_Union1( BS *set, BS_ELT x, MEM_POOL *pool );
extern BS* BS_UnionD_Intersection( BS* set1, BS* set2, BS* set3,
                                   MEM_POOL *pool );
extern BS *BS_Union1D( BS *set, BS_ELT x, MEM_POOL *pool );

extern BS *BS_2_1_Minus_3_Or_R( BS *result, const BS *set1, const BS *set2,
			const BS *set3, MEM_POOL *pool );
extern BS *BS_3_2_Minus_1_Or_D( BS *set1, const BS *set2,
			const BS *set3, MEM_POOL *pool );
extern BS *BS_2_3_Or_1_Or_D( BS *set1, const BS *set2, const BS *set3,
			  MEM_POOL *pool );
extern BS *BS_1_2_Or_3_And_R( BS *result, const BS *set1, const BS *set2,
			const BS *set3, MEM_POOL *pool );
extern BS *BS_2_3_And_1_Or_D(  BS *set1, const BS *set2,
			const BS *set3, MEM_POOL *pool );
extern BS *BS_1_Not_2_Or_3_Minus_4_And_R( BS *result, const BS *set1,
			const BS *set2, const BS *set3, const BS *set4,
			MEM_POOL *pool );

extern BS *BS_2_1_Minus_3_Or_4_And_5_And_6_And_R( BS *result, const BS *set1,
			const BS *set2, const BS *set3, const BS *set4,
			const BS *set5, const BS *set6, MEM_POOL *pool);
extern BS *BS_2_1_Minus_3_Or_4_And_R( BS *result, const BS *set1,
			const BS *set2, const BS *set3, const BS *set4,
			MEM_POOL *pool);
extern BS *BS_3_2_Minus_4_Or_1_Or_D( BS *set1, const BS *set2,
                        const BS *set3, const BS *set4, MEM_POOL *pool);
extern BS *BS_3_2_Minus_4_Or_5_Or_1_Or_D( BS *set1, const BS *set2, 
			const BS *set3, const BS *set4, const BS *set5,
			MEM_POOL *pool );
extern BS *BS_3_Not_4_Or_2_And_1_Or_D( BS *result, const BS *set1,
                        const BS *set2, const BS *set3, MEM_POOL *pool);
extern BS *BS_4_3_Minus_2_Not_Or_1_And_D(BS *result, const BS *set1,
                        const BS *set2, const BS *set3, MEM_POOL *pool);
extern BS *BS_2_3_Minus_1_Or_D(BS *result, const BS *set1,
                        const BS *set2, MEM_POOL *pool);
extern BS *BS_2_3_Minus_4_Minus_1_Or_D(BS *result, const BS *set1,
                        const BS *set2, const BS *set3, MEM_POOL *pool);
extern BOOL BS_ContainsP( BS *set1, BS *set2 );
extern BOOL BS_EmptyP( BS *set );
extern BOOL BS_EqualP( BS *set1, BS *set2 );
extern BOOL BS_IntersectsP( BS *set1, BS *set2 );
extern BOOL BS_MemberP( BS *set, BS_ELT x );
extern BOOL BS_Intersection_MemberP( BS *set1, BS *set2, BS_ELT x );
extern BS_ELT BS_Choose_Next( const BS *set, BS_ELT elt);
extern BS_ELT BS_Intersection_Choose_Next(BS *set1, BS *set2, BS_ELT elt);

/* FBS -- fixed-size bitset implementation.
    Fixed size bitsets are faster because no implicit reallocation
    is required.   For debugging, a slower implementation that
    verifies the range of x is in bitset.c.  
*/
extern BOOL FBS_MemberP_Validate(BS *set, BS_ELT x);
extern void FBS_Union1D_Validate(BS *set, BS_ELT x);

#ifdef Is_True_On
#define FBS_MemberP(set, x)  FBS_MemberP_Validate(set, x)
#define FBS_Union1D(set, x)  FBS_Union1D_Validate(set, x)
#else
#define FBS_MemberP(set, x)  (BS_byte(set,bs_QBPB(x)) & (bs_ONE << bs_RBPB(x)))
#define FBS_Union1D(set, x)  (BS_byte(set,bs_QBPB(x)) |= bs_ONE << bs_RBPB(x))
#endif

extern void BS_Print( BS *set, FILE *f );
#pragma mips_frequency_hint NEVER BS_Print

#ifdef __cplusplus
}
#endif
#endif /* bitset_INCLUDED */


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
***	This file includes all the snl_*.h files -- only only ever
***	includes snl.h and never any of the more specific files.
***	The descriptions of the classes and functions exported to the
***	general world are discussed here.  Descriptions of snl code
***	provided for snl code in other files may be described in the
***	correct .h if I'm in the mood, but everything that the outside
***	world (non snl_* files) might want to use needs to be described
***	here.
***
*** Reserved Prefix:
***
***	SNL
***
*** Exported Values:
***
***	SNL_MAX_LOOPS
***
***	    The maximum number of loops allowed in a simply nested loop.
***	    Thus arrays of things of this size can be used
***	    statically and automatically, e.g. loop[SNL_MAX_LOOPS] can be
***	    declared.  If a simply nested loop nest is deeper, only the
***	    innermost SNL_MAX_LOOPS will be optimized.
***	    In practice, eight is more than will ever be necessary.
***         For now, just use LNO_MAX_DO_LOOP_DEPTH, but supposedly things
***         should work if a lesser number is used.  However, serious
***         debugging has not been done.  So test with SNL_MAX_LOOPS of 2
***         and see how things work.  But no known bugs.
***
*** Exported Types:
***
***	SNL_REGION
***
***	    Contains pointers to two WNs in the same block.  This is
***	    primarily for debugging purposes: a transformation fctn
***	    returns the first and last statements on a block that it
***	    touched.
***
***		WN* First	ptr to the first WN in the region
***		WN* Last	ptr to the first WN in the region
***		SNL_REGION::SNL_REGION(WN* first, WN* last)
***		SNL_REGION::SNL_REGION() // set First and Last to NULL
***		BOOL operator ==()       // two components the same
***		BOOL operator !=()       // not equal
***
***	SNL_TILE_INFO
***
***	    This type is used to specify which tiling transformation to
***	    apply.
***
***	    Exported Functions:
***
***	    SNL_TILE_INFO(
***	        WN**		loop,
***	        const IMAT&	L,
***		const INT*      striplevel,
***	        MEM_POOL*	pool
***	    );
***
***	      Use the matrix L to indicate which tiling to perform.  (See the
***	      frame document on cache tiling.)  This is very cool, allowing
***	      for tile shapes they said were impossible in a unimodular
***	      framework (e.g. diagonal/anti-diagonal).  BUT THIS FUNCTION IS
***	      UNIMPLEMENTED.  Trivial to implement once a smith-normal-form
***	      finder is implemented.  Anyway, note that the colums of L
***	      describe tile edges in the iteration space, so very easy to use.
***	      loop is an array loop[0] through loop[L.Rows()-1] of do loops
***	      (outer to inner) that L is to be applied to.  Pool indicates
***	      where to allocate temporary storage for this data structure.
***           Striplevel tells for what level we are blocking, one entry
***           per col of L (i.e. the number of strips).
***
***	    SNL_TILE_INFO(
***		INT		nloops,
***		INT		strips,
***		const INT*	iloop,
***		const INT*	stripsz,
***             const INT*      striplevel,
***		MEM_POOL*	pool
***	    );
***
***	      nloops: The number of loops in the nest.
***	      strips: The number of loops that are to be stripped for tiling
***		(very often: nloops-1).
***	      iloop: Indicates how the order of the tiled loops corresponds
***		to the order of the inner loops. iloop[0] == 1 indicates
***		that the outermost tiled loop (0) is the stripped part of
***		the second to the outermost loop (1) within the tile.
***		For example, if we transform DO i DO j DO k into
***		DO jj DO kk DO i DO j DO k, then strip=2, iloop[0]=1,
***		and iloop[1]=2.
***	      stripsz: The size of each strip.  An integer constant.
***           striplevel: The memory hierarcy level this strip is for, e.g.
***             1 for the L1 cache.
***	      pool: where to allocate temporary storage for this data structure
***	      Rectangular() will return true.
***
***
***	    ~SNL_TILE_INFO()
***
***	      destruct one of these babies.
***
***	    const IMAT& L() const
***
***	      For the constructed transformation, what is L?  
***	      The columns of L are the tile boundaries in the iteration
***	      space, ordered from outermost to innermost tile execution.
***
***	    const IMAT& KHT() const
***
***                               T
***	      Returns the matrix k H  for the constructed transformation.
***	      (See the tiling document, or Ancourt and Irigoin: Scanning
***	      Polyhedra with DO Loops.)
***
***	    INT K() const()
***
***	      Returns k for the constructed transformation.
***
***	    MEM_POOL* Pool() const
***
***	      Where this class gets its temporary storage.
***
***	    BOOL Rectangular() const
***
***	      True if rectangular tiling.
***
***	    INT Iloop(int i) const
***
***	      See the description of iloop for the constructor.  This is only
***	      interesting if Rectangular() is true.
***
***	    INT Stripsz(int i) const
***
***	      See the description of stripsz for the constructor.  This is only
***	      interesting if Rectangular() is true.
***
***	    INT Striplevel(int i) const
***
***	      See the description of striplevel for the constructor.
***
***	    INT Nloops()
***	    INT Strips()
***
***	      The number of loops in the nest and the number of strips.
***	      Often Strips() is Nloops() - 1.
***
***	SNL_NEST_INFO
***
***	    Information about loop bounds, distributability, invariantness
***	    of bounds.  The user of this class will typically pass this
***	    this data to the transformation routines, but will also call
***	    the member functions below to determine which transformations
***	    can be applied to this nest.
***
***	    SNL_NEST_INFO::SNL_NEST_INFO(
***	        WN* outer,
***	        INT nloops,
***	        MEM_POOL* pool,
***		BOOL inner_only
***	    )
***
***	    Given the outer loop and nloops, construct information
***	    to answer questions supplied in the member functions below.
***	    Non-stride one loops are not optimized.  Access vectors must
***	    be valid upon entry to this code and are not altered by this
***	    function.  This function must not be called with nloops larger
***         then SNL_MAX_LOOPS.  Period.
***
***	    ~SNL_FIND_NEST_INFO()
***
***		Destruct one of these.
***
***	    DOLOOP_STACK&	Dostack()
***	    const DOLOOP_STACK&	Dostack() const
***
***		The loop stack, at least to the depth of the innermost in
***		the SNL.
***
***	    INT	       	Depth_Inner() const
***
***		The depth of the innermost loop.
***		Dostack().Bottom_nth(Depth_Inner()) is the innermost WN.
***
***	    INT		Nloops() const
***	    INT		Nloops_General() const
***	    INT		Nloops_Invariant() const
***	    INT&	Nloops_Invariant()
***
***		Number of loops in the nest.  These are relative to the
***		innermost loop, so that the depth of the outermost loop
***		for an invariant transformation would be Depth_Inner() -
***		Nloops_Invariant() + 1, and likewise for Nloops_General()
***		which is the maximum number of loops that may participate
***		in a general transformation.  In order to participate in
***		a general transformation, each loop must go at least once.
***
***	    INT Num_Bad() const
***
***		How many outermost loops are marked bad.
***
***	    void Print(FILE*) const;
***
***		Dump information to a file (human readable).
***
***	    MEM_POOL* Pool() const
***
***		The memory pool used in this data structure.
***
***	    BOOL Above_Is_Distributable() const
***	    BOOL Below_Is_Distributable() const
***
***		For general transformations only, whether the imperfect code
***		above/below the loops are distributable.  TRUE for a perfect
***		nest.  It's guaranteed that at most one will be FALSE.
***
*** Exported Functions:
***
***	See the files for descriptions. 
**/

/** $Revision$
*** $Date$
*** $Author$
*** $Source$
**/

#ifndef snl_INCLUDED
#define snl_INCLUDED "snl.h"

#ifdef _KEEP_RCS_ID
static char *snl_rcs_id = snl_INCLUDED "$Revision$";
#endif /* _KEEP_RCS_ID */

#ifndef defs_INCLUDED
#include "defs.h"
#endif
#ifndef stab_INCLUDED
#include "stab.h"
#endif
#ifndef CXX_BASE_INCLUDED
#include "cxx_base.h"
#endif
#ifndef cxx_hash_INCLUDED
#include "cxx_hash.h"
#endif
#ifndef lnopt_main_INCLUDED
#include "lnopt_main.h"
#endif
#ifndef dep_INCLUDED
#include "dep.h"
#endif
#ifndef reduc_INCLUDED
#include "reduc.h"
#endif

class ARRAY_DIRECTED_GRAPH16;

#ifndef snl_utils_INCLUDED
#include "snl_utils.h"
#endif
#ifndef snl_nest_INCLUDED
#include "snl_nest.h"
#endif
#ifndef snl_trans_INCLUDED
#include "snl_trans.h"
#endif
#ifndef snl_gen_INCLUDED
#include "snl_gen.h"
#endif
#ifndef snl_inv_INCLUDED
#include "snl_inv.h"
#endif
#ifndef snl_deps_INCLUDED
#include "snl_deps.h"
#endif
#ifndef snl_dist_INCLUDED
#include "snl_dist.h"
#endif
#ifndef snl_test_INCLUDED
#include "snl_test.h"
#endif

#endif

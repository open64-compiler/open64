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


// This is a -*- C++ -*- presentation

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <math.h>
#include <sys/types.h>
#include <limits.h>
#include "pu_info.h"
#include "lnoutils.h"
#include "lnopt_main.h"
#include "stdlib.h"
#include "fiz_fuse.h"
#include "ara.h"
#include "cross_snl.h"
#include "snl_utils.h"
#include "cross_cache.h"


#define SMALL_CONST 5
#define ESTIMATED_CONST 100
#define ABS(x) (((x) < 0) ? -(x) : (x))

// -------------------------------------------------------------------------------------
/*
 * Return the difference hi - lo. Approximate to
 * a constant if cannot find the difference exactly
 *
 * Return -1 if you cannot find the difference
 */

INT Get_Range(ACCESS_VECTOR *lo, ACCESS_VECTOR *hi)
{
  if (hi == NULL) 
    return 1;

  // first check if everything is the same excet for the constant
  BOOL indices_same = TRUE;

  for (INT i = 0; i < lo->Nest_Depth(); ++i) {
    if (lo->Loop_Coeff(i) != hi->Loop_Coeff(i)) {
      indices_same = FALSE;
      break;
    }
  }

  if (indices_same == FALSE) {
    /* the size obviously varies - lets return -1 */
    return -1;
  }

  // now check if the symbolic constants are the same
  if (lo->Lin_Symb != NULL && hi->Lin_Symb != NULL) {
    if ( *(lo->Lin_Symb) == *(hi->Lin_Symb)) {
      return hi->Const_Offset - lo->Const_Offset + 1;
    } else {
      // symbolic constants are different
      return ESTIMATED_CONST;
    }
  } else {

    if (lo->Lin_Symb == NULL && hi->Lin_Symb == NULL) {
      return hi->Const_Offset - lo->Const_Offset;
    } else {
      return ESTIMATED_CONST + (hi->Const_Offset - lo->Const_Offset) + 1;
    }
  }

  return -1;
    
}




    
/*
 * Compare two access vectors
 * return TRUE if they are the similar
 *        if dimensions contain unprojected loop variables they must be equal
 *        otherwise they can vary by a "small constant"
 *          
 * return FALSE otherwise
 */
BOOL Is_Similar(ACCESS_VECTOR *acv1, ACCESS_VECTOR *acv2)
{
  BOOL is_unprojected = FALSE;

  // first look to see if there are unprojected dimensions
  // these dimensions have better be the same
  INT j;
  for (j = 0; j < acv1->Nest_Depth(); ++j) {
    if (acv1->Loop_Coeff(j) != 0) {
      is_unprojected = TRUE;
      if (j >= acv2->Nest_Depth()) {
	return FALSE; // not enough loops around the other region
      }

      // the j'th loop is assumed common to both since it has not been projected away
      if (acv1->Loop_Coeff(j) != acv2->Loop_Coeff(j)) {
	return FALSE;
      }

      // they might have symbolic constants. These must be equal too
      if (acv1->Lin_Symb != NULL) {
	if (acv2->Lin_Symb != NULL) {
	  if (!(*(acv1->Lin_Symb) == *(acv2->Lin_Symb))) {
	    return FALSE;
	  }
	} else {
	  return FALSE;
	}
      } else {
	if (acv2->Lin_Symb != NULL) {
	  return FALSE;
	}
      }

      // make sure the constants are the same as well
      if (acv1->Const_Offset != acv2->Const_Offset) {
	return FALSE;
      }
    }
  }

  // check the other vector
  for (j = 0; j < acv2->Nest_Depth(); ++j) {
    if (acv2->Loop_Coeff(j) != 0) {
      is_unprojected = TRUE;

      if (j >= acv1->Nest_Depth()) {
	return FALSE; // not enough loops around the other region
      }

      // the j'th loop is assumed common to both since it has not been projected away
      if (acv1->Loop_Coeff(j) != acv2->Loop_Coeff(j)) {
	return FALSE;
      }

      // they might have symbolic constants. These must be equal too
      if (acv2->Lin_Symb != NULL) {
	if (acv1->Lin_Symb != NULL) {
	  if (!(*(acv2->Lin_Symb) == *(acv1->Lin_Symb))) {
	    return FALSE;
	  }
	} else {
	  return FALSE;
	}
      } else {
	if (acv1->Lin_Symb != NULL) {
	  return FALSE;
	}
      }

      // make sure the constants are the same as well
      if (acv2->Const_Offset != acv1->Const_Offset) {
	return FALSE;
      }
    }
  }

  // the other dimensions can differ by a small constant
  if (! is_unprojected) {
    if (acv1->Lin_Symb != NULL) {
      if (acv2->Lin_Symb != NULL) {
	if (!(*(acv1->Lin_Symb) == *(acv2->Lin_Symb))) {
	  return FALSE;
	}
      } else {
	return FALSE;
      }
    } else {
      if (acv1->Lin_Symb != NULL) {
	return FALSE;
      }
    }
    
    if (ABS(acv1->Const_Offset - acv2->Const_Offset) > SMALL_CONST) {
      return FALSE;
    }
  }

  return TRUE;
}
    
/*
 * Compare two access vectors
 * return TRUE if they are the same
 *        if there are unprojected loop variables the indices must match
 *        constants (symbolic and otherwise) must match
 * return FALSE otherwise
 */
BOOL Is_Equal(ACCESS_VECTOR *acv1, ACCESS_VECTOR *acv2)
{

  // check the unprojected variables
  INT j;
  for (j = 0; j < acv1->Nest_Depth(); ++j) {
    if (acv1->Loop_Coeff(j) != 0) {
      if (j >= acv2->Nest_Depth()) {
	return FALSE; // not enough loops around the other region
      }

      // the j'th loop is assumed common to both since it has not been projected away
      if (acv1->Loop_Coeff(j) != acv2->Loop_Coeff(j)) {
	return FALSE;
      }
    }
  }

  // check the other vector
  for (j = 0; j < acv2->Nest_Depth(); ++j) {
    if (acv2->Loop_Coeff(j) != 0) {

      if (j >= acv1->Nest_Depth()) {
	return FALSE; // not enough loops around the other region
      }

      // the j'th loop is assumed common to both since it has not been projected away
      if (acv1->Loop_Coeff(j) != acv2->Loop_Coeff(j)) {
	return FALSE;
      }

    }
  }

  // check the constants
  if (acv2->Lin_Symb != NULL) {
    if (acv1->Lin_Symb != NULL) {
      if (!(*(acv2->Lin_Symb) == *(acv1->Lin_Symb))) {
	return FALSE;
      }
    } else {
      return FALSE;
    }
  } else {
    if (acv1->Lin_Symb != NULL) {
      return FALSE;
    }
  }
  
  // make sure the constants are the same as well
  if (acv2->Const_Offset != acv1->Const_Offset) {
    return FALSE;
  }

  return TRUE;
}


/*
 * Are_Independent_Regions : is the intersection NULL?
 *
 * c1, c2 : cache regions to be intersected
 * ara_info : the ARA_INFO of the innermost surrounding loops
 */
BOOL Are_Independent_Regions(CACHE_REGION *c1, CACHE_REGION *c2, ARA_LOOP_INFO *ara_info)
{
  if (c1->Is_Messy() || c2->Is_Messy())
    return FALSE;
  if (c1->Get_Ref()->Array() != c2->Get_Ref()->Array()) {
    return  TRUE; // different arrays
  }

  REGION *r1 = c1->Get_Region();
  REGION *r2 = c2->Get_Region();

  REGION *r =  Region_Intersect(*r1, *r2, *ara_info);

  if (r != NULL) {
    CXX_DELETE(r, &ARA_memory_pool);
    return FALSE;
  } else {
    return TRUE;
  }
}

/*
 * Is_Subset_Region : is region1 a subset of region2 ?
 * c1, c2 : cache_regions
 * ara_info : the ARA_INFO of the innermost surrounding loops
 */

BOOL Is_Subset_Region(CACHE_REGION *c1, CACHE_REGION *c2, ARA_LOOP_INFO *ara_info)
{
  if (c1->Is_Messy() || c2->Is_Messy())
    return FALSE;
  if (c1->Get_Ref()->Array() != c2->Get_Ref()->Array()) {
    return  FALSE; // different arrays
  }

  REGION *r1 = c1->Get_Region();
  REGION *r2 = c2->Get_Region();

  INT res =  Region_Compare(*r1, *r2, *ara_info);
  if (res == 1 || res == 3) {
    return TRUE;
  }

  return FALSE;
}

/*
 * Compare_Cache_Regions : compare the regions c1, c2
 * c1, c2 : cache_regions
 * _ara_info :  the ARA_INFO of the innermost surrounding loop
 * 
 * returns  1 if c1 < c2
 * returns  2 if c1 == c2
 * return   3 if c1 > c2
 * returns -1 if they are not comparable
 */

INT Compare_Cache_Regions(CACHE_REGION *c1, CACHE_REGION *c2, ARA_LOOP_INFO *ara_info)
{
  if (c1->Is_Messy() || c2->Is_Messy())
    return -1;
  if (c1->Get_Ref()->Array() != c2->Get_Ref()->Array()) {
    return  -1; // different arrays
  }

  REGION *r1 = c1->Get_Region();
  REGION *r2 = c2->Get_Region();

  INT res =  Region_Compare(*r1, *r2, *ara_info);
  if (res == 1 || res == 3 || res == 2) {
    return res;
  }

  return -1;
}


/*
 * Are_Similar_Dimensions : are the indicated diemnsions of c1 and c2 similar?
 *                          dimensions are similar if they vary only by a small constant
 *
 * c1, c2       : cache reions
 * dist         : array of dimensions to check
 * ndist        : number of entries in dist
 */
BOOL Are_Similar_Dimensions(CACHE_REGION *c1, CACHE_REGION *c2, INT32* dist, INT ndist)
{
  if (c1->Is_Messy() || c2->Is_Messy())
    return FALSE;

  if (c1->Get_Ref()->Array() != c2->Get_Ref()->Array()) {
    return FALSE; // different arrays
  }

  REGION *r1 = c1->Get_Region();
  REGION *r2 = c2->Get_Region();

  /* make sure the regions are similar. This means the following -
   * eveything has to be the same except for integer constants
   * we might need to check that these constants are small -
   * ie say less than 2 but for now we will ignore this
   */

  // make sure they have the same dimensions
  if (r1->Num_Dim() != r2->Num_Dim())
    return FALSE;

  for (INT i = 0; i < ndist; ++i) {
    AXLE_NODE *ax1 = &r1->Dim(dist[i]);
    AXLE_NODE *ax2 = &r2->Dim(dist[i]);

    // if a dimension has an unprojected loop variable then it
    // had better be the same in the other region
    CON_PAIR *lo1 = ax1->lo;
    CON_PAIR *lo2 = ax2->lo;

    if (lo1->_coeff != NULL || lo2->_coeff != NULL) {
      // TODO : worry about coupled dimensions
      return FALSE;
    }

   
    // check if the acess vectors of lo are similar
    if (!Is_Similar(lo1->_ac_v, lo2->_ac_v)) {
      return FALSE;
    }

    // check if the access vectors of hi are similar
    if (ax2->up != NULL && ax1->up != NULL) {
      if (ax2->up->_coeff != NULL ||  ax1->up->_coeff != NULL) {
	return FALSE;
      }

      if (!Is_Similar(ax2->up->_ac_v, ax1->up->_ac_v)) {
	return FALSE;
      }
    } else if (!(ax2->up == NULL && ax1->up == NULL)) {
      return FALSE;
    }
  }
  return TRUE;
}

/*
 * Are_Similar_Regions : do the two regions mostly match
 *             Two regiosn mostly match if diemnsions
 *             vary by utmost a small constant
 *
 */
BOOL Are_Similar_Regions(CACHE_REGION *c1, CACHE_REGION *c2)
{

  if (c1->Is_Messy() || c2->Is_Messy())
    return FALSE;

  if (c1->Get_Ref()->Array() != c2->Get_Ref()->Array()) {
    return FALSE; // different arrays
  }

  REGION *r1 = c1->Get_Region();
  REGION *r2 = c2->Get_Region();

  /* make sure the regions are similar. This means the following -
   * eveything has to be the same except for integer constants
   * we might need to check that these constants are small -
   * ie say less than 2 but for now we will ignore this
   */

  // make sure they have the same dimensions
  if (r1->Num_Dim() != r2->Num_Dim())
    return FALSE;

  int ndims = r1->Num_Dim();
  for (INT i = 0; i < ndims; ++i) {
    AXLE_NODE *ax1 = &r1->Dim(i);
    AXLE_NODE *ax2 = &r2->Dim(i);

    // if a dimension has an unprojected loop variable then it
    // had better be the same in the other region
    CON_PAIR *lo1 = ax1->lo;
    CON_PAIR *lo2 = ax2->lo;

    if (lo1->_coeff != NULL || lo2->_coeff != NULL) {
      // TODO : worry about coupled dimensions
      return FALSE;
    }

   
    // check if the acess vectors of lo are similar
    if (!Is_Similar(lo1->_ac_v, lo2->_ac_v)) {
      return FALSE;
    }

    // check if the access vectors of hi are similar
    if (ax2->up != NULL && ax1->up != NULL) {
      if (ax2->up->_coeff != NULL ||  ax1->up->_coeff != NULL) {
	return FALSE;
      }

      if (!Is_Similar(ax2->up->_ac_v, ax1->up->_ac_v)) {
	return FALSE;
      }
    } else if (!(ax2->up == NULL && ax1->up == NULL)) {
      return FALSE;
    }
  }
  return TRUE;
}


/*
 * Merge_regions : Merge the cache regions into a single region
 *                 if they are capable of being unioned together
 *
 * c1, c2    : cache_regions
 * ara_info  : ARA_INFO of the surrounding innermost loop
 */
CACHE_REGION *Merge_Regions(CACHE_REGION *c1, CACHE_REGION *c2, ARA_LOOP_INFO *ara_info )
{
  if (c1->Is_Messy() || c2->Is_Messy())
    return NULL;

  if (c1->Get_Ref()->Array() != c2->Get_Ref()->Array()) {
    return NULL; // different arrays
  }

  if (c1->Type() != c2->Type()) {
    return NULL;
  }

  if (c1->Type() == EXCLUSIVE || c1->Type() == REPLICATED) {
    // just check if we can union them together
    REGION *newregion = Region_Union(*(c1->Get_Region()), *(c2->Get_Region()), *ara_info);
    if (newregion == NULL) {
      return NULL;
    } else {
      // make a copy
      ARA_REF *newref = CXX_NEW (ARA_REF(*c1->Get_Ref()), &LNO_local_pool);

      // need a better way of emptying the list */
      while (!newref->Image().Is_Empty()) {
	REGION * tmp = newref->Image().Remove_Headnode();
      }
      newref->Image().Append(newregion);


      // substitute this array ref for the old one
      CACHE_REGION *newcr = CXX_NEW(CACHE_REGION(c1, newref), &LNO_local_pool);
      return newcr;
    }
  } else {
    /* distributed - the distributed dimensions must match */
    if (c1->N_Dist() != c2->N_Dist()) {
      return NULL;
    }

    INT* dist1 = c1->Dist();
    INT* dist2 = c2->Dist();

    // make sure all the distributed dimensions match 
    INT i;
    for (i = 0; i < c1->N_Dist(); ++i) {
      INT dim = dist1[i];
      BOOL found = FALSE;

      for (INT j = 0; j < c2->N_Dist(); ++j) {
	if (dist2[j] == dim) {
	  found = TRUE;
	  break;
	}
      }

      if (!found) {
	return NULL;
      }
    }

    // the regions are distributed along the same directions
    // make sure the bounds for these dimensions match
    REGION *r1 = c1->Get_Region();
    REGION *r2 = c2->Get_Region();

    for (i = 0; i < c1->N_Dist(); ++i) {
      AXLE_NODE *ax1 = &r1->Dim(dist1[i]);
      AXLE_NODE *ax2 = &r2->Dim(dist1[i]);
      CON_PAIR *lo1 = ax1->lo;
      CON_PAIR *lo2 = ax2->lo;
      CON_PAIR *hi1 = ax1->up;
      CON_PAIR *hi2 = ax2->up;

      if (lo1->_coeff != NULL || lo2->_coeff != NULL 
	  || hi1->_coeff != NULL || hi2->_coeff != NULL) {
	// TODO : worry about coupled dimensions
	return NULL;
      }

      if (!Is_Equal(lo1->_ac_v, lo2->_ac_v)) {
	return NULL;
      }

      /* check the upper bounds */
      if (hi1 != NULL || hi2 != NULL) {
	if (hi1 == NULL || hi2 == NULL) {
	  return NULL;
	}

	if (!Is_Equal(hi1->_ac_v, hi2->_ac_v)) {
	  return NULL;
	}
      }
    }

    // all the distributed dimensions are the same - check if we can
    // union these two regions

    REGION *newregion = Region_Union(*r1, *r2, *ara_info);
    if (newregion == NULL) {
      return NULL;
    } else {
      // make a copy
      ARA_REF *newref = CXX_NEW (ARA_REF(*c1->Get_Ref()), &LNO_local_pool);

      // need a better way of emptying the list */
      while (!newref->Image().Is_Empty()) {
	REGION * tmp = newref->Image().Remove_Headnode();
      }
      newref->Image().Append(newregion);

      // substitute this array ref for the old one
      CACHE_REGION *newcr = CXX_NEW(CACHE_REGION(c1, newref), &LNO_local_pool);
      return newcr;
    }

  }

  return NULL;

}


// -------------------------------------------------------------------------------
// Build a cache region given a reference and the SNL towhich it belongs and
// which loop is parallelized.
//
// parallel_loop == 0 => serial
// any other value implies that loop (counting 1 = outermost) is parallel
// Note that in order to determine regions, the order of the loops
// does not matter
// ref : the reference ro be converted in cache region
// asi : info about the surrounding SNL
// parallel_loop : which loop is parallel

CACHE_REGION::CACHE_REGION(ARA_REF_INFO *ref, ARRAY_SNL_INFO *asi, UINT32 parallel_loop)
{
  _dist = NULL;
  _offsets = NULL;
  _ranges = NULL;

  // Lets take care of the easy cases first 
  if (parallel_loop == 0) { 
    /* not a parallel loop */
    if (ref->Is_Messy()) {
      _is_messy = TRUE;
      return;
    } else {
      _is_messy = FALSE;

      _type = EXCLUSIVE;
      _reg  = ref->Get_Proj_Ref();
      _dims = ref->Dim();
      _ndist = 0;
      return;
    }
  } else {
    if (ref->Is_Messy()) {
      _is_messy = TRUE;
      return;
    }

    _reg = ref->Get_Proj_Ref();
    _dims = ref->Dim();
    _is_messy = FALSE;

    /* Find out which loop is parallel and how it causes array distribution */
    WN* snl_root = asi->Get_SNL_Root();
    WN* wn_parallel = (parallel_loop != 1) ? SNL_Get_Inner_Snl_Loop(snl_root, parallel_loop) :
      snl_root;
    ARA_REF *orig_ref = ref->Get_Ref();

    /* find out which dimensions of the reference carry the parallel loop :
     * initially we shall only handle uncoupled dimensions with the coefficient
     * of the parallel loop being 1 or -1 and every other lop variable being 0
     */
    REGION_ITER iter(&orig_ref->Image());
    REGION *r = iter.First();               // only one region per ARA_REF
    

    // store the distribution information
    _dist = CXX_NEW_ARRAY(INT32, _dims, &LNO_local_pool);
    _offsets = CXX_NEW_ARRAY(INT32, _dims, &LNO_local_pool);
    _ranges = CXX_NEW_ARRAY(INT32, _dims, &LNO_local_pool);
    _ndist = 0;

    for (INT i = 0; i < _dims; ++i) {
      AXLE_NODE *a = &r->Dim(i);
      CON_PAIR *lo = a->lo;
      CON_PAIR *up = a->up;
      INT step = a->step;
      
      if (lo->_coeff != NULL || (up != NULL && (up->_coeff != NULL || ABS(step) != 1))) {
	_is_messy = TRUE;
	return;
	// TODO : we need to take care of coupled dimensions and non-unit strides
      }

      INT loop_start_depth = Do_Loop_Depth(snl_root);
      INT loop_var = loop_start_depth + parallel_loop - 1;
      ACCESS_VECTOR *acv = lo->_ac_v;

      if (acv->Loop_Coeff(loop_var) == 0) 
	continue; 

      // Only integer constants allowed in this reference
      if (acv->Contains_Lin_Symb() || acv->Contains_Non_Lin_Symb()) {
	_is_messy = TRUE;
	return;
      }
	  
      // Check if any other loop var is also non-zero - we will label this messy
      // but we can ignore loops outside the SNL by treating them as constants
      for (INT j = 0; j < asi->Get_Depth(); ++j) {

	if (j == parallel_loop - 1) 
	  continue;

	if (acv->Loop_Coeff(j+loop_start_depth) != 0) {
	  _is_messy = TRUE;
	  return;
	}
      }

      

      // Now we know that this dimension has only this loop variable indexing it
      _dist[_ndist] = (acv->Loop_Coeff(loop_var) > 0) ? i : -i;
      _offsets[_ndist] = acv->Const_Offset;

      if (up == NULL) {
	_ranges[_ndist] = 0;
      } else {
	if (up->_ac_v->Contains_Lin_Symb() || up->_ac_v->Contains_Non_Lin_Symb()) {
	  _is_messy = TRUE;
	  return;
	}

	// make sure this echoes lo
	for (INT k = 0; k <= acv->Nest_Depth(); ++k) {
	  if (acv->Loop_Coeff(k) != up->_ac_v->Loop_Coeff(k)) {
	    _is_messy = TRUE;
	    return;
	  }
	}

	// store the offset as range
	_ranges[_ndist] = up->_ac_v->Const_Offset;
      }
	
      _ndist++;
    }

    if (_ndist == 0) {
      /* the array is independent of this loop */
      _type = REPLICATED;
    } else if (_ndist == 1) {
      _type = DISTRIBUTED;
    } else if (_ndist > 1) {
      // TODO : take care of coupled dimensions
      _is_messy = TRUE;
      return;
    } 

    return;
  }
}

// initialize with known values
CACHE_REGION::CACHE_REGION(CACHE_REGION *cr)
{
  _type = cr->_type;
  _reg  = cr->_reg;
  _dims = cr->_dims;
  if (cr->_ndist > 0) {
    _dist    = CXX_NEW_ARRAY(INT32, cr->_ndist, &LNO_local_pool);
    _ranges   = CXX_NEW_ARRAY(INT32, cr->_ndist, &LNO_local_pool);
    _offsets = CXX_NEW_ARRAY(INT32, cr->_ndist, &LNO_local_pool);
    for (INT i = 0; i < _ndist; ++i) {
      _dist[i] = cr->_dist[i];
      _ranges[i] = cr->_ranges[i];
      _offsets[i] = cr->_offsets[i];
    }
  } else {
    _dist = NULL;
    _ranges = NULL;
    _offsets = NULL;
  }

  _ndist = cr->_ndist;
  _is_messy = cr->_is_messy;
}

CACHE_REGION::CACHE_REGION(CACHE_REGION *cr, ARA_REF *ref)
{
  _type = cr->_type;
  _reg  = ref;
  _dims = cr->_dims;
  if (cr->_ndist > 0) {
    _dist    = CXX_NEW_ARRAY(INT32, cr->_ndist, &LNO_local_pool);
    _ranges   = CXX_NEW_ARRAY(INT32, cr->_ndist, &LNO_local_pool);
    _offsets = CXX_NEW_ARRAY(INT32, cr->_ndist, &LNO_local_pool);
    for (INT i = 0; i < cr->_ndist; ++i) {
      _dist[i] = cr->_dist[i];
      _ranges[i] = cr->_ranges[i];
      _offsets[i] = cr->_offsets[i];
    }
  } else {
    _dist = NULL;
    _ranges = NULL;
    _offsets = NULL;
  }

  _ndist = cr->_ndist;
  _is_messy = cr->_is_messy;
}


/*
 * Region_Size : Determine the size of a cache region
 *
 * Return -1 if unable to find the size of hte region
 */
INT32 CACHE_REGION::Region_Size(void)
{
  if (_is_messy)
    return -1;

  // not messy - we will approximate it as follows
  // all symbolic variables will be replaced by 100
  // all ranges will be multipled togther

  REGION *r1 = Get_Region();
  INT32 size = 1;

  for (INT i = 0; i < r1->Num_Dim(); ++i) {
    AXLE_NODE *ax = &r1->Dim(i);
    CON_PAIR *lo = ax->lo;
    CON_PAIR *hi = ax->up;

    // don't handle coupled dimensions yet
    if (lo->_coeff != NULL || (hi != NULL && hi->_coeff != NULL)) {
      return -1;
    }

    if (hi != NULL) {
      // we have a range here - approximate it
      INT range = Get_Range(lo->_ac_v, hi->_ac_v);

      if (range == -1)
	return -1;
      else
	size = range * size;
    }
  }

  return size;
}


// Intersect_region : return the size of the intersection
//                    with the current cache_region
// c        : cache region to be intersected with the cache contents
// ara_info : ARA_INFO of the innermost loop surrounding the sibling SNLs
//
INT32 CACHE_REGION::Intersect_Region(CACHE_REGION *c, ARA_LOOP_INFO *ara_info)
{
  if (this->Is_Messy() || c->Is_Messy()) {
    return -1;
  }

  if (this->Get_Ref()->Array() != c->Get_Ref()->Array()) {
    return -1; // different arrays
  }

  REGION *r1 = this->Get_Region();
  REGION *r2 = c->Get_Region();
  REGION *r =  Region_Intersect(*r1, *r2, *ara_info);

  if (r == NULL) {
    return 0;
  } else {
    INT32 size = 1;

    for (INT i = 0; i < r->Num_Dim(); ++i) {
      AXLE_NODE *ax = &r->Dim(i);
      CON_PAIR *lo = ax->lo;
      CON_PAIR *hi = ax->up;

      // don't handle coupled dimensions yet
      if (lo->_coeff != NULL || (hi != NULL && hi->_coeff != NULL)) {
	return -1;
      }

      if (hi != NULL) {
	// we have a range here - approximate it
	INT range = Get_Range(lo->_ac_v, hi->_ac_v);

	if (range == -1)
	  return -1;
	else
	  size = range * size;
      }
    }

    return size;
  }
}

void CACHE_REGION::Print(FILE *file)
{
  fprintf(file, "Type : ");
  switch(_type) {
  case DISTRIBUTED : fprintf(file, "DISTRIBUTED\n"); break;
  case REPLICATED : fprintf(file, "REPLICATED\n"); break;
  case EXCLUSIVE : fprintf(file, "EXCLUSIVE\n"); break;
  default : fprintf(file, "UNKNOWN\n"); break;
  }

  fprintf(file, "Messy : %s\n", (_is_messy) ? "TRUE" : "FALSE");

  fprintf(file, "Region : ");
  _reg->Print(file);

  if (_type == DISTRIBUTED) {
    fprintf(file, "NDIST : %d\n", _ndist);
    for (INT i = 0; i < _ndist; ++i) {
      fprintf(file, "dim = %d offset = %d range = %d\n", _dist[i], _offsets[i], _ranges[i]);
    }
  }
}

// ------------------------------------------------------------------------    
CACHE_CONTENTS::CACHE_CONTENTS(CACHE_TYPE type, UINT64 size, INT32 nprocs, 
			       ARA_LOOP_INFO *ara_info ) :
_reg_list()
{
  _type = type;
  _size = size;
  _nprocs = nprocs;
  _ara_info = ara_info;
}

/*
 * Add_Region : Add the cache region to the cache contents
 *
 * c : cache region
 * atype : type of the region (read or write)
 */

void CACHE_CONTENTS::Add_Region(CACHE_REGION *c, ACCESS_TYPE atype)
{
  switch (c->Type()) {
  case DISTRIBUTED : {
    Add_Region_Distributed(c, atype);
    break;
  }
  case REPLICATED : {
    Add_Region_Replicated(c, atype);
    break;
  }
  case EXCLUSIVE : {
    Add_Region_Exclusive(c, atype);
    break;
  }
  default : {
    FmtAssert(0, ("Uknown cache region type"));
  }
  }
}

/*
 * Add the contents of a distributed region of type atype to the cache contents
 * We have the following guidelines to keep in mind
 * if (the region does not exist before) 
 *    add it as distributed
 * else if (the region covers already existing region)
 *    if (atype is read-only)
 *        add the region
 *    else if (atype is a write)
 *        remove existing region and add this
 * else if (partial cover)
 *    don't know what to do - as an approximation just add it and let both
 *    regions remain
 *
 * 
 */
void CACHE_CONTENTS::Add_Region_Distributed(CACHE_REGION *c, ACCESS_TYPE atype)
{
  BOOL add_region = TRUE;

  CACHE_REGION_ITER iter(&_reg_list);
  CACHE_REGION * prev = NULL;
  CACHE_REGION * curr = iter.First();

  while (!iter.Is_Empty()) {
    if (!Are_Independent_Regions(c, curr, _ara_info)) {
      if (Are_Similar_Regions(c, curr)) {
	if (atype != CACHE_READ_ONLY) {
	  CXX_DELETE(_reg_list.Remove(prev,curr), &LNO_local_pool);
	  prev = NULL;
	  iter.Init(&_reg_list);
	  curr = iter.First();
	}
      } else {
	/* don't know what to do here - we will let both remain 
	 * We have said that similar means there is considerable overlap 
	 * We shall assume that the negation of that is that there is not
	 * much similarity and can be treated as a different region
	 */
      }
    }
    prev = curr;
    curr = iter.Next();
  }
    
  _reg_list.Append(c); 
}


void CACHE_CONTENTS::Add_Region_Replicated(CACHE_REGION *c, ACCESS_TYPE atype)
{
  BOOL add_region = TRUE;

  CACHE_REGION_ITER iter(&_reg_list);
  CACHE_REGION * prev = NULL;
  CACHE_REGION * curr = iter.First();

  while (!iter.Is_Empty()) {
    if (!Are_Independent_Regions(c, curr, _ara_info)) {
      if (Are_Similar_Regions(c, curr)) {
	if (atype != CACHE_READ_ONLY) {
	  CXX_DELETE(_reg_list.Remove(prev,curr), &LNO_local_pool);
	  prev = NULL;
	  iter.Init(&_reg_list);
	  curr = iter.First();
	}
      } else {
	/* don't know what to do here - we will let both remain 
	 * We have said that similar means there is considerable overlap 
	 * We shall assume that th negation of that is that there is not
	 * much similarity and can be treated as a different region
	 */
      }
    }
    prev = curr;
    curr = iter.Next();
  }
    
  _reg_list.Append(c); 
}


void CACHE_CONTENTS::Add_Region_Exclusive(CACHE_REGION *c, ACCESS_TYPE atype)
{
  BOOL add_region = TRUE;

  CACHE_REGION_ITER iter(&_reg_list);
  CACHE_REGION * prev = NULL;
  CACHE_REGION * curr = iter.First();

  while (!iter.Is_Empty()) {
    if (!Are_Independent_Regions(c, curr, _ara_info)) {
      if (Are_Similar_Regions(c, curr)) {
	if (atype != CACHE_READ_ONLY) {
	  CXX_DELETE(_reg_list.Remove(prev,curr), &LNO_local_pool);
	  prev = NULL;
	  iter.Init(&_reg_list);
	  curr = iter.First();
	}
      } else {
	/* don't know what to do here - we will let both remain 
	 * We have said that similar means there is considerable overlap 
	 * We shall assume that th negation of that is that there is not
	 * much similarity and can be treated as a different region
	 */
      }
    }
    prev = curr;
    curr = iter.Next();
  }
    
  _reg_list.Append(c); 
}
    

/*
 * Compact cache : go through the cache finding opportunities to merge
 */
void CACHE_CONTENTS::Compact_Cache(void)
{
  CACHE_REGION_ITER iter(&_reg_list);
  CACHE_REGION_LIST *crl = CXX_NEW(CACHE_REGION_LIST, &LNO_local_pool);

  while (!_reg_list.Is_Empty()) {
    CACHE_REGION * curr = _reg_list.Remove_Headnode();
  
    CACHE_REGION_ITER piter(crl);
    CACHE_REGION *prev = NULL;
    CACHE_REGION *pcur = piter.First();
    CACHE_REGION * add_reg = curr;

    while (!piter.Is_Empty()) {
	
      if (add_reg->Get_Ref()->Array() != pcur->Get_Ref()->Array() ||
	  pcur->Type() != add_reg->Type()) {
	prev = pcur;
	pcur = piter.Next();
	continue;
      }

      // check if we can merge the two
      CACHE_REGION *newcurr = Merge_Regions(add_reg, pcur, _ara_info);
      if (newcurr != NULL) {
	crl->Remove(prev, pcur);
	add_reg = newcurr;
	
	piter.Init(&_reg_list);
	pcur = piter.First();
	prev = NULL;
	break;
      } else {
	prev = pcur;
	pcur = piter.Next();
      }
    }

    crl->Append(add_reg);
  }

  _reg_list.Append_List(crl);
  
}


// Regions_Distributed_Similarly : are the two cache regions
// distributed alon the same dimensions and array regions
// more or less match
// returns 0 on success
// return s-1 if they do not match

INT32 Regions_Distributed_Similarly(CACHE_REGION *c1, CACHE_REGION *c2)
{
  if (c1->N_Dist() != c2->N_Dist()) {
    return -1;
  }

  INT32 *dims1 = c1->Dist();
  INT32 *dims2 = c2->Dist();
  BOOL found = FALSE;
  for (INT i = 0; i < c1->N_Dist(); ++i) {
    INT32 d = dims1[i];
    found = FALSE;

    for (INT j = 0; j < c2->N_Dist(); ++j) {
      if (dims1[j] == dims2[i]) {
	found = TRUE;
	break;
      }
    }
    if (!found)
      break;
  }

  if (!found)
    return -1;
  
  if (Are_Similar_Dimensions(c1, c2, dims1, c1->N_Dist())) {
    return 0;
  } else {
    return -1;
  }
}

/* Regions_Distributed_Orthogonally : check if the regions are distributed orthogonally
 *                                    they do not share a common parallel dimension
 * c1, c2 are the cache regions
 */

BOOL Regions_Distributed_Orthogonally(CACHE_REGION *c1, CACHE_REGION *c2)
{
  FmtAssert(c1->N_Dist() == c2->N_Dist(), ("Dimensions dont match"));

  INT32 *dims1 = c1->Dist();
  INT32 *dims2 = c2->Dist();
  BOOL found = FALSE;
  for (INT i = 0; i < c1->N_Dist(); ++i) {
    INT32 d = dims1[i];
    found = FALSE;

    for (INT j = 0; j < c2->N_Dist(); ++j) {
      if (dims1[j] == dims2[i]) {
	found = TRUE;
	break;
      }
    }
    if (found)
      break;
  }

  if (found)
    return FALSE;
  else
    return TRUE;
}

// Intersect_Region : Returns the size of the region that intersects with
//                    the cache contents
//
// returns -1 if it was not able to find the intersection
INT32 CACHE_CONTENTS::Intersect_Region(CACHE_REGION *c)
{
  
  CACHE_REGION_ITER iter(&_reg_list);
  INT csize = c->Region_Size();
  INT size = 0;

  if (csize == -1) {
    return -1;
  }

  for (CACHE_REGION * curr = iter.First(); !iter.Is_Empty(); curr = iter.Next()) {
    if (!Are_Independent_Regions(c, curr, _ara_info)) {
      INT comparision = Compare_Cache_Regions(c, curr, _ara_info);
      if (comparision == 1 || comparision == 3) {

	// c is a subset of curr
	if (c->Type() == curr->Type()) {
	  if (c->Type() != DISTRIBUTED) {
	    size = MAX(size, csize);
	    continue;
	  } else {
	    // both of them are distributed and one of them is a subset
	    // of the other - check if they are distributed similarly
	    INT32 offset = Regions_Distributed_Similarly(c, curr);
	    if (offset >= 0) {
	      size = MAX(size, csize / _nprocs - offset);
	      continue;
	    } else if (Regions_Distributed_Orthogonally(c, curr)) {
		size = MAX(size, csize / (_nprocs * _nprocs));
	      continue;
	    }
	  }
	} else {
	  // they are of different types 
	  if (c->Type() == EXCLUSIVE) {
	    if (curr->Type() == REPLICATED) {
	      size = MAX(size, csize);
	      continue;
	    } else if (curr->Type() == DISTRIBUTED) {
	      size = MAX(size, csize / _nprocs);
	      continue;
	    } 
	  } else if (c->Type() == REPLICATED) {
	    if (curr->Type() == EXCLUSIVE) {
	      size = MAX(size, 0);
	    } else if (curr->Type() == DISTRIBUTED) {
	      if (Are_Similar_Dimensions(c, curr, curr->Dist(), curr->N_Dist()) >= 0) {
		size = MAX(size, csize / _nprocs);
		continue;
	      }
	    }
	  } else if (c->Type() == DISTRIBUTED) {
	    if (curr->Type() == EXCLUSIVE) {
	      size = MAX(size, 0);
	      continue;
	    } else if (curr->Type() == REPLICATED) {
		size = MAX(size, csize / _nprocs);
		continue;
	    }
	  }
	}
      } else if (comparision == 2) {
	// curr is a subset of c
	INT isize = c->Region_Size();

	if (c->Type() == curr->Type()) {
	  if (c->Type() != DISTRIBUTED) {
	    size = MAX(size, isize);
	    continue;
	  } else {
	    // both of them are distributed and one of them is a subset
	    // of the other - check if they are distributed similarly
	    INT32 offset = Regions_Distributed_Similarly(curr, c);
	    if (offset >= 0) {
	      size = MAX(size, isize / _nprocs - offset);
	      continue;
	    } else if (Regions_Distributed_Orthogonally(curr, c)) {
	      if (Are_Similar_Dimensions(curr, c, c->Dist(), c->N_Dist()) >= 0) {
		size = MAX(size, isize / (_nprocs * _nprocs));
		continue;
	      }
	    }
	  }
	} else {

	  // they are of different types 
	  if (c->Type() == EXCLUSIVE) {
	    if (curr->Type() == REPLICATED) {
	      size = MAX(size, isize);
	      continue;
	    } else if (curr->Type() == DISTRIBUTED) {
	      size = MAX(size, isize / _nprocs);
	      continue;
	    } 
	  } else if (c->Type() == REPLICATED) {
	    if (curr->Type() == EXCLUSIVE) {
	      size = MAX(size, 0);
	      continue;
	    } else if (curr->Type() == DISTRIBUTED) {
	      size = MAX(size, isize / _nprocs);
	      continue;
	    }
	  } else if (c->Type() == DISTRIBUTED) {
	    if (curr->Type() == EXCLUSIVE) {
	      size = MAX(size, 0);
	      continue;
	    } else if (curr->Type() == REPLICATED) {
	      if (Are_Similar_Dimensions(c, curr, c->Dist(), c->N_Dist()) >= 0) {
		size = MAX(size, isize / _nprocs);
		continue;
	      }
	    }
	  }
	}
      }

      if (Are_Similar_Regions(c, curr)) {

	/* now there is considerable over_lap - get the intersection */
	if (c->Type() == curr->Type()) {
	  if (c->Type()  != DISTRIBUTED) {
	    INT isize = c->Intersect_Region(curr, _ara_info);
	    size = MAX(size, isize);
	  } else {
	    // both the regions are distributed
	    INT32 offset = Regions_Distributed_Similarly(curr, c);
	    if (offset >= 0) {
	      size = MAX(size, csize / _nprocs - offset);
	      continue;
	    } else if (Regions_Distributed_Orthogonally(curr, c)) {
	      size = MAX(size, csize / (_nprocs * _nprocs));
	      continue;
	    }
	  }
	} else {
	  if (c->Type() == EXCLUSIVE) {
	    if (curr->Type() == REPLICATED) {
	      INT isize = c->Intersect_Region(curr, _ara_info);
	      size = MAX(size, isize);
	      continue;
	    } else if (curr->Type() == DISTRIBUTED) {
	      if (Are_Similar_Dimensions(c, curr, curr->Dist(), curr->N_Dist())) {
		size = MAX(size, csize / _nprocs);
		continue;
	      } 
	    }
	  } else if (c->Type() == REPLICATED) {
	    if (curr->Type() == EXCLUSIVE) {
	      size = MAX(size, 0);
	      continue;
	    } else if (curr->Type() == DISTRIBUTED) {
	      size = MAX(size, csize / _nprocs);
	      continue;
	    }
	  } else if (c->Type() == DISTRIBUTED) {
	    if (curr->Type() == EXCLUSIVE) {
	      if (_nprocs >= 1) {
		size = MAX(size, 0);
		continue;
	      }
	      continue;
	    } else if (curr->Type() == REPLICATED) {
	      if (_nprocs >= 1) {
		size = MAX(size, csize * (_nprocs - 1)  / _nprocs);
		continue;
	      } else {
		size = MAX(size, csize);
		continue;
	      }
	    }
	  }
	}
      }
    }
  }

  if (c->Type() == DISTRIBUTED) {
    return size * _nprocs;
  } else {
    return size;
  }
}


void CACHE_CONTENTS::Print(FILE *file)
{
  fprintf(file, "_ara_info = %p\n", _ara_info);
  fprintf(file, "CACHE CONTENTS :");
  CACHE_REGION_ITER iter(&_reg_list);
  INT i = 0;

  for (CACHE_REGION *curr = iter.First(); !iter.Is_Empty(); curr = iter.Next()) {
    fprintf(file, "\n%d : ", ++i);
    curr->Print(file);
  }
  fprintf(file, "\n-*-\n");
}

		




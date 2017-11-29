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


#ifndef dvector_INCLUDED
#define dvector_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif


/**                     Dependence Vectors
***                     -------------------
***
*** Description:
***
***     This is the basic data structure used to represent loop dependences.
***     A dependence vector, DEPV,  is an array of dependences, one for each 
***     enclosing do loop.  Each dependence, DEP, is a mixed distance direction:
***     i.e. either a constant integer or a union of directions.
***	DEP is implemented as a 16 bit quantity.  The leftmost bit tells whether
***     the dependence is a constant distance dependence.  The next 3 bits
***     give the direction.  The rightmost 12 bits give the distance is this
***     is a constant distance dependence and a bound on the distance otherwise.
***
***
***
***
*** Exported types and functions:
***
***     DIRECTION
***
***	        An enumeration containing the following values 
***             {DIR_POS,DIR_NEG,DIR_EQ,DIR_POSNEG,DIR_POSEQ,
***		 DIR_NEGEQ,DIR_STAR}
***
***         void DIRECTION_Print(DIRECTION direction,FILE *fp)
***
***		Print a direction
***
***     DEP              	
***
***		A single direction/distance.  The following functions/fields 
***             are defined for distances.
***
***         BOOL DEP_IsDistance(DEP Dep)
***
***		Is this dependence a constant distance?
***
***	    INT32  DEP_Distance(DEP Dep)
***
***		The distance of the dependence.  
***		This is only defined for cases when IsDistance returns true.  
***		On overflow, this just sets the direction.
***		Note that this is a function and cannot be a lhs.
***
***         UINT32 DEP_DistanceBound(DEP Dep)
***
***              This gives a bound on the distance.  
***		 In other words, this gives a guarantee that abs(dist) >= bound
***		 On overflow, the bound is set to some edge value.
***
***         DIRECTION DEP_Direction(DEP Dep)
***
***		The direction of the dependence.  This is defined even if
***		the dependence has a distance.
***
***	    DEP DEP_SetDistance(INT32 distance)
***
***		Return a dependence that has a distance of distance
***		If distance doesn't fit into 16 bits, set the distance
***		to be non-constant, but the bound to be max
***
***	    DEP DEP_SetDirection(DIRECTION direction)
***
***		Return a dependence that has a DIRECTION direction.
***		This automatically sets the dependence to NOT have a 
***		distance (unless the direction is = in which case
***		the distance is 0).
***
***	    DEP DEP_UnionDirection(DEP dep,DIRECTION direction)
***
***		Return a dependence that has a direction that is the
***		union of dep's direction and direction.
***		This automatically sets the dependence to NOT have a 
***		distance.
***
***	    DEP DEP_Negate(DEP dep)
***
***		Negate a dependence
***
***    void DEP_Lex_Pos_Decompose(DEP dep,MEM_POOL *pool,DEP **pos, 
***	DEP **neg, BOOL keep_pos_equals,BOOL keep_neg_equals) 
***
***		Compute the lexicographically positive decomposition
***		of dep and of -dep.  Set *pos to lexpos(this).
***		Set *neg to lexpos(-this).  If (0) is part of the original
***		it will be placed in pos iff keep_pos_equals==TRUE,
***		it will be place in neg iff keep_neg_equals==TRUE
***		*pos,*neg will be set to NULL if there is no pos/neg dependence
***
***     DEP DEP_Lex_Pos_Compose(DEP *pos, DEP *neg, BOOL *pos_has_eq,
***		BOOL *neg_has_eq)
***
***		Reverse the above process.  Pos/Neg = NULL implies there
***		are no pos/neg component (Although one must be non-NULL)
***		Also set pos_has_eq/neg_has_eq
***		to true iff the original positive/negative contained the
***		equal direction.  
***
***         void DEP_Print(const DEP dep,FILE *fp)
***
***             Print a dependence.
***
***         void DEP_PrintBound(const DEP dep,FILE *fp)
***
***             Print the bound of the dependence.
***             
***             
***
***     DEPV
***
***		A vector of dependences.
***
***	    DEP DEPV_Dep(DEPV *depv, UINT8 i)
***		
***		The i'th dimension (the first dimension is 0).  
***		Can be either a LHS or a RHS
***
***	    DEPV *DEPV_Create(MEM_POOL *mem_pool,UINT8 num_dim)
***
***		Create a new, uninitialized dependence vector 
***
***	    DEPV *DEPV_CreateStar(MEM_POOL *mem_pool,UINT8 num_dim)
***
***		Create a new dependence vector with each term set to star
***
***	    DEPV *DEPV_CreateEqual(MEM_POOL *mem_pool,UINT8 num_dim)
***
***		Create a new dependence vector with each term set to equal
***
***         DEPV *DEPV_Copy(MEM_POOL *mem_pool, DEPV *depv, UINT8 num_dim)
***
***             Create a new dependence vector equal to depv
***
***
***	    DEPV *DEPV_Free(MEM_POOL *mem_pool,DEPV *depv)
***
***		Free the storage for the dependence vector
***	
***         void DEPV_Print(const DEPV *depv,FILE *fp, UINT8 num_dim)
***
***             Print a dependence vector.
***
***         void DEPV_PrintBound(const DEPV *depv,FILE *fp, UINT8 num_dim)
***
***             Print a dependence vector, printing the bounds of each 
***		dimension rather than the direction
***
***	    
***/


/** $Revision: 1.2 $
*** $Date: 02/11/07 23:41:35-00:00 $
*** $Author: fchow@keyresearch.com $
*** $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.dvector.h $
**/

#ifndef ND_RCS_ID
#define ND_RCS_ID
#ifdef _KEEP_RCS_ID
static char *dvector_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.dvector.h $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */
#endif

#include "defs.h"
#include "errors.h"
#include "mempool.h"


typedef enum {
  DIR_POS=1,
  DIR_NEG=2,
  DIR_EQ=4,
  DIR_POSNEG=3,
  DIR_POSEQ=5,
  DIR_NEGEQ=6,
  DIR_STAR=7
} DIRECTION;

/* DEP is implemented as a 16 bit quantity */
/* Bit 15 is on if the distance is constant */
/* Bits 12-14  give the direction */
/* Bits 0-11 give the distance + offset (offset=2048) */
typedef mINT16  DEP;

#define DISTANCE_FLAG_PRIVATE 0x8000
#define DISTANCE_MASK_PRIVATE 0x0FFF
#define DISTANCE_OFFSET_PRIVATE 2048
#define DIRECTION_MASK_PRIVATE 0x7000
#define MAX_BOUND_PRIVATE 4095
#define MAX_DISTANCE_PRIVATE 2047
#define MIN_DISTANCE_PRIVATE -2048

extern DEP DEP_SetDistance(INT32 distance);
extern void DEP_Lex_Pos_Decompose(DEP dep,MEM_POOL *pool,DEP **pos, 
    DEP **neg, BOOL keep_pos_equals,BOOL keep_neg_equals) ;

extern DEP DEP_Lex_Pos_Compose(DEP *pos, DEP *neg, BOOL *pos_has_eq,
	BOOL *neg_has_eq);

inline BOOL DEP_IsDistance(DEP dep)
{
  return(dep & DISTANCE_FLAG_PRIVATE);
}

inline INT32 DEP_Distance(DEP dep)
{
  Is_True(DEP_IsDistance(dep),("DEP_Distance called on non-constant dist."));
  return(((INT32) (dep & DISTANCE_MASK_PRIVATE)) - DISTANCE_OFFSET_PRIVATE);
}

inline UINT32 DEP_DistanceBound(DEP dep)
{
  return(((UINT32) (dep & DISTANCE_MASK_PRIVATE)) - DISTANCE_OFFSET_PRIVATE);
}

inline DIRECTION DEP_Direction(DEP dep)
{
  return((DIRECTION) ((dep & DIRECTION_MASK_PRIVATE) >> 12)); 
}

inline DEP DEP_MAKE_DIST_CONST_PRIVATE(DEP dep)
{
  return (dep | DISTANCE_FLAG_PRIVATE);
}

inline DEP DEP_MAKE_DIST_NON_CONST_PRIVATE(DEP dep)
{
  return( dep & (~DISTANCE_FLAG_PRIVATE));
}

/* set the direction bits, don't fiddle with anything else */
inline DEP DEP_SET_DIR_PRIVATE(DEP dep,DIRECTION direction)
{
  dep &= (~DIRECTION_MASK_PRIVATE);
  dep  |= ((mINT16) direction) << 12;
  return (dep);
}

/* set the distance bits, don't fiddle with anything else */
/* assumes distance is withing bounds */
inline DEP DEP_SET_DIST_PRIVATE(DEP dep,INT32 distance)
{
  dep &= (~DISTANCE_MASK_PRIVATE);
  dep = (mINT16) (dep | (distance + DISTANCE_OFFSET_PRIVATE));
  return (dep);
}



/* DEPV */

/* DEPV is implemented as an array of DEP, which is assumed to be mINT16 */
/* Element 0..num_dim give the individual deps */
typedef DEP DEPV;  


#define DEPV_Dep(depv,i) depv[i]


inline void DEPV_Free(MEM_POOL *mem_pool,DEPV *depv)
{
  MEM_POOL_FREE(mem_pool,depv);
}

extern DEP DEP_SetDirection(DIRECTION direction);
extern DEP DEP_UnionDirection(DEP dep,DIRECTION direction);
extern DEP DEP_Negate(DEP dep);
extern void  DIRECTION_Print(DIRECTION dir,FILE *fp);
extern void  DEP_Print(const DEP dep,FILE *fp);
extern void  DEP_PrintBound(const DEP dep,FILE *fp);
extern DEPV *DEPV_Create(MEM_POOL *mem_pool,UINT8 num_dim);
extern DEPV *DEPV_CreateStar(MEM_POOL *mem_pool,UINT8 num_dim);
extern DEPV *DEPV_CreateEqual(MEM_POOL *mem_pool,UINT8 num_dim);
extern DEPV *DEPV_Copy(MEM_POOL *mem_pool, DEPV *depv, UINT8 num_dim);
extern void DEPV_Print(const DEPV *depv,FILE *fp, UINT8 num_dim);
extern void DEPV_PrintBound(const DEPV *depv,FILE *fp, UINT8 num_dim);

#ifdef __cplusplus
}
#endif

#endif /* dvector_INCLUDED */




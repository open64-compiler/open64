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



/* ====================================================================
 * ====================================================================
 *
 * Module: dvector.c  
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:35-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.dvector.cxx $
 *
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Base Routines for tree nodes in WHIRL
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
#endif

#ifndef dvector_INCLUDED
#include "dvector.h"
#endif


/**                     Dependence Vectors
***                     -------------------
***
*** Description:
***
***     This is the basic data structure used to represent loop dependences.
***     A dependence vector is an array of dependences, one for each 
***     enclosing do loop.  Each dependence is a mixed distance direction:
***     i.e. either a constant integer or a union of directions.
***
***
***
***
*** See dvector.h for exported routines
***	    
***/



extern DEP DEP_SetDirection(DIRECTION direction)
{
  DEP dep=0;

  if (direction == DIR_EQ) {
    return(DEP_SetDistance(0));
  } else {
    dep = DEP_MAKE_DIST_NON_CONST_PRIVATE(dep);
    dep = DEP_SET_DIR_PRIVATE(dep,direction);

    /* set the bound */
    if (direction == DIR_POS) {
      dep = DEP_SET_DIST_PRIVATE(dep,1); /* the bound */
    } else if (direction == DIR_NEG) {
      dep = DEP_SET_DIST_PRIVATE(dep,1); /* the bound */
    } else {
      dep = DEP_SET_DIST_PRIVATE(dep,0); /* a bound of 0 = no bound */
    }
  }
  return(dep);
}

extern DEP DEP_UnionDirection(DEP dep,DIRECTION direction)
{
  if (DEP_Direction(dep) == direction) {
    if (direction != DIR_EQ) {
      dep = DEP_MAKE_DIST_NON_CONST_PRIVATE(dep);
      if (direction == DIR_POS) {
        dep = DEP_SET_DIST_PRIVATE(dep,1); /* the bound */
      } else if (direction == DIR_NEG) {
        dep = DEP_SET_DIST_PRIVATE(dep,1); /* the bound */
      } else {
        dep = DEP_SET_DIST_PRIVATE(dep,0); /* a bound of 0 = no bound */
      }
    }
    return (dep);
  }
  dep = DEP_MAKE_DIST_NON_CONST_PRIVATE(dep);
  dep |= (((mINT16) direction) << 12);
  dep = DEP_SET_DIST_PRIVATE(dep,0); /* a 0 bound is equivalent to no bound */
  return (dep);
}

extern DEP DEP_Negate(DEP dep)
{
  DEP tmp=0;

  /* Is it a constant distance? */
  if (DEP_IsDistance(dep)) return DEP_SetDistance(-DEP_Distance(dep));

  tmp = DEP_MAKE_DIST_NON_CONST_PRIVATE(tmp);
  if (DEP_Direction(dep) == DIR_POS) {
    tmp = DEP_SET_DIR_PRIVATE(tmp,DIR_NEG);
    tmp = DEP_SET_DIST_PRIVATE(tmp,1); /* the bound */
  } else if (DEP_Direction(dep) == DIR_NEG) {
    tmp = DEP_SET_DIR_PRIVATE(tmp,DIR_POS);
    tmp = DEP_SET_DIST_PRIVATE(tmp,1); /* the bound */
  } else if (DEP_Direction(dep) == DIR_POSEQ) {
    tmp = DEP_SET_DIR_PRIVATE(tmp,DIR_NEGEQ);
    tmp = DEP_SET_DIST_PRIVATE(tmp,0); /* the bound */
  } else if (DEP_Direction(dep) == DIR_NEGEQ) {
    tmp = DEP_SET_DIR_PRIVATE(tmp,DIR_POSEQ);
    tmp = DEP_SET_DIST_PRIVATE(tmp,0); /* the bound */
  } else tmp = dep;

  return(tmp);
}


extern DEP DEP_SetDistance(INT32 distance)
{
  DEP dep=0;

  if (distance > MAX_DISTANCE_PRIVATE) {
     dep = DEP_MAKE_DIST_NON_CONST_PRIVATE(dep);
     distance = MAX_BOUND_PRIVATE;
  } else if (distance < MIN_DISTANCE_PRIVATE) {
     dep = DEP_MAKE_DIST_NON_CONST_PRIVATE(dep);
     distance = MAX_BOUND_PRIVATE;
  } else {
     dep = DEP_MAKE_DIST_CONST_PRIVATE(dep);
  }

  if (distance > 0) {
    dep = DEP_SET_DIR_PRIVATE(dep,DIR_POS);
  } else if (distance < 0) {
    dep = DEP_SET_DIR_PRIVATE(dep,DIR_NEG);
  } else {
    dep = DEP_SET_DIR_PRIVATE(dep,DIR_EQ);
  }
  dep = DEP_SET_DIST_PRIVATE(dep,distance); 
  return(dep);
}


extern void  DIRECTION_Print(DIRECTION dir,FILE *fp)
{
  switch (dir) {
    case DIR_POS: fprintf(fp," + "); break;
    case DIR_NEG: fprintf(fp," - "); break;
    case DIR_EQ: fprintf(fp," = "); break;
    case DIR_POSNEG: fprintf(fp," +- "); break;
    case DIR_POSEQ: fprintf(fp," += "); break;
    case DIR_NEGEQ: fprintf(fp," -= "); break;
    case DIR_STAR: fprintf(fp," * "); break;
    default: Is_True(0,("Illegal direction in DIRECTION_Print"));
  };
}

extern void  DEP_Print(const DEP dep,FILE *fp)
{
  if (DEP_IsDistance(dep)) fprintf(fp," %d ",DEP_Distance(dep));
  else DIRECTION_Print(DEP_Direction(dep),fp);
}

extern void  DEP_PrintBound(const DEP dep,FILE *fp)
{
  fprintf(fp," %d ",(INT32) DEP_DistanceBound(dep));
}


extern DEPV *DEPV_Create(MEM_POOL *mem_pool,UINT8 num_dim)
{
  DEPV *depv;

  depv = TYPE_MEM_POOL_ALLOC_N(DEP,mem_pool,num_dim);
  return(depv);
}


extern DEPV *DEPV_CreateStar(MEM_POOL *mem_pool,UINT8 num_dim)
{
  DEPV *depv;
  INT i;

  depv = TYPE_MEM_POOL_ALLOC_N(DEP,mem_pool,num_dim);
  for (i=0; i< num_dim; i++) {
    depv[i] = DEP_SetDirection(DIR_STAR);
  }
  return(depv);
}

extern DEPV *DEPV_CreateEqual(MEM_POOL *mem_pool,UINT8 num_dim)
{
  DEPV *depv;
  INT i;

  depv = TYPE_MEM_POOL_ALLOC_N(DEP,mem_pool,num_dim);
  for (i=0; i< num_dim; i++) {
    depv[i] = DEP_SetDirection(DIR_EQ);
  }
  return(depv);
}

extern DEPV *DEPV_Copy(MEM_POOL *mem_pool, DEPV *depv, UINT8 num_dim)
{
  DEPV *result;
  INT i;

  result = TYPE_MEM_POOL_ALLOC_N(DEP,mem_pool,num_dim);
  for (i=0; i<num_dim; i++) {
    result[i] = depv[i];
  }
  return(result);
}



extern void DEPV_Print(const DEPV *depv,FILE *fp, UINT8 num_dim)
{
  INT i;

  fprintf(fp," (  ");
  for (i=0; i<num_dim; i++) {
    DEP_Print(DEPV_Dep(depv,i),fp);
  }
  fprintf(fp," ) ");
}

extern void DEPV_PrintBound(const DEPV *depv,FILE *fp, UINT8 num_dim)
{
  INT i;

  fprintf(fp," (  ");
  for (i=0; i<num_dim; i++) {
    DEP_PrintBound(DEPV_Dep(depv,i),fp);
  }
  fprintf(fp," ) ");
}


void DEP_Lex_Pos_Decompose(DEP dep,MEM_POOL *pool, DEP **pos, 
	DEP **neg, BOOL keep_pos_equals, BOOL keep_neg_equals) 
{
  if (DEP_Direction(dep) == DIR_POS) {
    *pos = TYPE_MEM_POOL_ALLOC(DEP,pool);
    *neg = NULL;
    **pos = dep;
  } else if (DEP_Direction(dep) == DIR_NEG) {
    *neg = TYPE_MEM_POOL_ALLOC(DEP,pool);
    *pos = NULL;
    **neg = DEP_Negate(dep);
  } else if (DEP_Direction(dep) == DIR_POSNEG) {
    *pos = TYPE_MEM_POOL_ALLOC(DEP,pool);
    *neg = TYPE_MEM_POOL_ALLOC(DEP,pool);
    **pos = **neg = DEP_SetDirection(DIR_POS);
  } else if (DEP_Direction(dep) == DIR_POSEQ) {
    *pos = TYPE_MEM_POOL_ALLOC(DEP,pool);
    *neg = NULL;
    if (keep_pos_equals) {
      **pos = dep;
    } else {
      **pos = DEP_SetDirection(DIR_POS);
    }
    if (keep_neg_equals) {
      *neg = TYPE_MEM_POOL_ALLOC(DEP,pool);
      **neg = DEP_SetDirection(DIR_EQ);
    }
  } else if (DEP_Direction(dep) == DIR_NEGEQ) {
    *neg = TYPE_MEM_POOL_ALLOC(DEP,pool);
    *pos = NULL;
    if (keep_neg_equals) {
      **neg = DEP_SetDirection(DIR_POSEQ);
    } else {
      **neg = DEP_SetDirection(DIR_POS);
    }
    if (keep_pos_equals) {
      *pos = TYPE_MEM_POOL_ALLOC(DEP,pool);
      **pos = DEP_SetDirection(DIR_EQ);
    }
  } else if (DEP_Direction(dep) == DIR_STAR) {
    *neg = TYPE_MEM_POOL_ALLOC(DEP,pool);
    *pos = TYPE_MEM_POOL_ALLOC(DEP,pool);
    if (keep_pos_equals) {
      **pos = DEP_SetDirection(DIR_POSEQ);
    } else {
      **pos = DEP_SetDirection(DIR_POS);
    }
    if (keep_neg_equals) {
      **neg = DEP_SetDirection(DIR_POSEQ);
    } else {
      **neg = DEP_SetDirection(DIR_POS);
    }
  } else {	
    /* a DIR_EQ */
    *pos = *neg = NULL;
    if (keep_pos_equals) {
      *pos = TYPE_MEM_POOL_ALLOC(DEP,pool);
      **pos = DEP_SetDirection(DIR_EQ);
    }
    if (keep_neg_equals) {
      *neg = TYPE_MEM_POOL_ALLOC(DEP,pool);
      **neg = DEP_SetDirection(DIR_EQ);
    }
  }
}

/* reverse a decomposition */
DEP DEP_Lex_Pos_Compose(DEP *pos, DEP *neg, BOOL *pos_has_eq, BOOL *neg_has_eq)
{
  Is_True(pos || neg,("Two null deps in DEP_Lex_Pos_Compose"));

  *pos_has_eq = FALSE;
  *neg_has_eq = FALSE;
  if (pos) { 
    if ((DEP_Direction(*pos) == DIR_POSEQ)||(DEP_Direction(*pos) == DIR_EQ)) {
      *pos_has_eq = TRUE;
    } 
  }

  if (neg) {
    if ((DEP_Direction(*neg) == DIR_POSEQ)||(DEP_Direction(*neg) == DIR_EQ)) {
      *neg_has_eq = TRUE;
    } 
  }

  if (!neg) return (*pos);
  if (!pos) return (DEP_Negate(*neg));
  return(DEP_UnionDirection(DEP_Negate(*neg),DEP_Direction(*pos)));
}
    
  


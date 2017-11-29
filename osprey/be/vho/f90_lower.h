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



#ifndef f90_lower_INCLUDED
#define f90_lower_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* This is the main external interface to the F90 lowerer 
 *
 *  WN * F90_Lower (WN *pu);
 *
 * Lowers the program unit in *pu. Returns a lowered program unit
 * (most likely, it will be the same pointer passed in, with a lot of changed subtrees,
 * but not necessarily).
 *
 */
extern WN * F90_Lower (PU_Info* pu_info, WN *pu);


/* Some enumerated types needed by the lowerer; these don't need to be 
   generally visible, but they are needed in two files */
#ifdef F90_LOWER_INTERNAL

/*
 * COPY_FLAG indicates what sort of temp copies need to be done for 
 * this statement.
 */
typedef enum {
   COPY_NONE=0,
   COPY_LEFT=1,
   COPY_RIGHT=2,
   COPY_BOTH=3  /* Must be COPY_LEFT & COPY_RIGHT */
} COPY_FLAG_T;

/*
 * DIR_FLAG indicates which direction a DO loop should be run. This is also used for dependence
 * analysis.
 */
typedef enum {
   DIR_DONTCARE=0,
   DIR_POSITIVE=1,
   DIR_NEGATIVE=2,
   DIR_ZERO=3,
   DIR_UNKNOWN=4
} DIR_FLAG;

typedef enum {
   DEP_UNKNOWN = 0,
   DEP_INDEPENDENT = 1,
   DEP_IDENTICAL = 2,
   DEP_REMOVABLE = 3
} DEP_SUMMARY;

/* This is the interface to the c++ routine for doing triplet dependence
 * analysis. It's in c++ becuase it makes use of LNO's SYSTEM_OF_EQUATIONS
 * class
 */
extern DIR_FLAG F90_Lower_Analyze_Triplet (INT64 l, INT64 s1, INT64 s2, 
					   INT64 e, BOOL e_present, MEM_POOL *mp);

#endif

#ifdef __cplusplus
}
#endif
#endif /* wn_lower_INCLUDED */

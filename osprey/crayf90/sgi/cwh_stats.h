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
 * Module: cwh_stats.h
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: prototypes for entry points into cwh_stats.c - the 
 *              counter functions & printing.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_STATS_INCLUDED
#define CWH_STATS_INCLUDED

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

/* 
 * counter defines for types - measure number of times New_TY is called
 * mostly, though other actions are counted too.
 *
 * Note c_POINTER and c_F90_NOPTR require Make_Pointer_Type 
 * in stab_util.c to be modified... ie: add
 *
 *  BUMP_TY_COUNTER(c_POINTER);
 *  at New_TY()
 *
 * and
 *
 *   if (!TY_is_f90_pointer(t)) { 
 *     ....
 *   } else 
 *       BUMP_TY_COUNTER(c_F90_NOPTR);
*/


enum type_counter { 
  c_TY_ARRAY = 0,
  c_TY_POINTER = 1,
  c_TY_f90_POINTER = 2,
  c_TY_PROC = 3 ,
  c_TY_MISC = 4,
  c_TY_STRUCT = 5,
  c_TY_COPY = 6,
  c_TY_UNIQ_POINTER = 7,
  c_TY_DTYPE =  8,
  c_TY_MATCH_ARRAY =  9,
  c_TY_MATCH_DOPE =  10,
  c_TY_MATCH =  11,
  c_TY_CLEARED = 12,
  c_TY_REUSED = 13,
  c_TY_F90_NOPTR = 14,
  c_TY_LAST  = 15 
};

extern int cwh_stat_ty_c[c_TY_LAST];


#ifdef _DEBUG 
#define BUMP_TY_COUNTER(i) cwh_stat_ty_c[i] ++ ; 
#else
#define BUMP_TY_COUNTER(i) 
#endif



#endif /* CWH_STATS_INCLUDED */

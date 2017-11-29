/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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



#ifndef f90_utils_INCLUDED
#define f90_utils_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif
   
   
#define MAX_NDIM 7 /* Max number of dimensions allowed */
   
extern BOOL F90_Is_Transformational(INT32 intrinsic);
extern BOOL F90_Is_Char_Intrinsic(INT32 intr);
extern INT F90_Get_Dim(WN *dim_wn);
extern BOOL F90_Size_Walk(WN *expr, INT *ndim, WN **sizes);
extern INT  F90_Rank_Walk(WN *expr);
extern WN * F90_Wrap_ARREXP(WN *expr);
extern WN * F90_wrap_cvtl(WN * wn, TYPE_ID ty);

#ifdef __cplusplus
}
#endif
#endif /* f90_utils_INCLUDED */






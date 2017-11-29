/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

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


/**
***		Intrinsics, Intrinsic Types, and Intrinsic Flags
***		------------------------------------------------
***
*** Description:
***
***	This interface describes all the intrinsic names, operators,
***	types associated with intrinsics, and properties associated with
***	intrinsics.
***
*** Reserved Prefixes:
***
***	INTRN		for INTRINSIC members only.
***
*** Exported types:
***
***	INTRINSIC
***
***	    An enumerated type.  The members are a partial set of all language
***	    defined intrinsics.  Those language intrinsics not included in this
***	    enumerated type are already present in WHIRL as OPC nodes.  There
***	    are usually two separate flavors of each fortran intrinsic - one
***	    named INTRN_XXX and one named INTRN_XXXe.  The former name
***	    represents the version called directly by generated code and usually
***	    has call by value semantics.  These intrinsics might eventually be
***	    turned into calls, be inlined or have direct code generated for
***	    them.  The INTRN_XXXe version is always an external routine with
***	    call by reference semantics.  It is needed to support passing an
***	    intrinsic function itself to another subprogram.
***
***	    All INTRINSICs are prefixed with INTRN.
***
*** Exported data:
***
***	    none
***
**/

#ifndef wintrinsic_INCLUDED
#define wintrinsic_INCLUDED "wintrinsic.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {

  INTRINSIC_INVALID = -1,
  INTRINSIC_NONE = 0    ,

/* All intrinsic definations are moved to intrn_entry.def */
#define NEED_INTRN_ID
# include "intrn_entry.def"
#undef  NEED_INTRN_ID

  INTRINSIC_LAST        ,
  INTRINSIC_FIRST = 1   ,

#if defined(TARG_SL)
  INTRN_SL_INTRN_BGN = INTRN_VBUF_OFFSET,
  INTRN_SL2_BEGIN = INTRN_C2_MVGR_R2G,
  INTRN_SL_INTRN_END = INTRN_VBUF_ABSOLUTE,
  INTRN_SL2_END = INTRN_VBUF_ABSOLUTE,
  INTRN_C3_INTRINSIC_BEGIN = INTRN_CVT64_HIGH, 
  INTRN_C3_INTRINSIC_END = INTRN_COPY_HI,
#endif

} INTRINSIC;

#ifdef __cplusplus
}
#endif

#endif

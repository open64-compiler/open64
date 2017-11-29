/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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


//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: profile_type.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $ 
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/profile_type.h,v $
//
// Revision history:
//  1-Nov-2002 - Original Version (by s.x.yang)
//
// Description:
// ===========
//     Definition of profiling type and phase. 
//
// ====================================================================
// ====================================================================

#ifndef profile_type_INCLUDED
#define profile_type_INCLUDED

#ifndef INT32_MAX
    # define INT32_MAX  2147483647
#endif

// When to instrument?  Correlates to Instrumentation_Phase_Num
enum PROFILE_PHASE
{
  PROFILE_PHASE_NONE            = -1,
  PROFILE_PHASE_BEFORE_VHO	= 0,
  PROFILE_PHASE_IPA_CUTOFF	= 0,	// phases less than or equal to
					// IPA_CUTOFF will not be
					// instrumented when the input file 
					// is an ipa-generated file.
  PROFILE_PHASE_BEFORE_LNO	= 1,
  PROFILE_PHASE_BEFORE_WOPT	= 2,
  PROFILE_PHASE_BEFORE_CG	= 3,
  PROFILE_PHASE_BEFORE_REGION	= 4,
  PROFILE_PHASE_LAST		= 5,
  PROFILE_PHASE_MAX             = INT32_MAX  // Fb_Hdr size must be 0 mod 64
};


// What instrument? Correlates to Profile_Type
enum PROFILE_TYPE
{
  WHIRL_PROFILE     = 1,
  CG_EDGE_PROFILE   = 2,
  CG_VALUE_PROFILE  = 4,
  CG_STRIDE_PROFILE  = 8,
  PROFILE_TYPE_LAST = 16,
  PROFILE_TYPE_MAX  = INT32_MAX
};

#endif /*profile_type_INCLUDED*/

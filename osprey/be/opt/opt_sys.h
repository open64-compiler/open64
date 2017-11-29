//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_sys.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_sys.h,v $
//
// Revision history:
//  09-JAN-94 shin - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
//   Defines all OS dependent function interface
//
// ====================================================================
// ====================================================================


#ifndef opt_sys_INCLUDED
#define opt_sys_INCLUDED "opt_sys.h"
#ifdef _KEEP_RCS_ID
static char *opt_sysrcs_id = opt_sys_INCLUDED"$ $Revision$";
#endif /* _KEEP_RCS_ID */

//  Return the CPU time in millisecond.
#if defined(BUILD_OS_DARWIN)
/* Darwin/BSD has only 100 ticks per second, so we need to use FP */
#define CLOCK_IN_MS() ((clock_t) (clock() / (CLOCKS_PER_SEC / 1000.0)))
#else /* defined(BUILD_OS_DARWIN) */
#define CLOCK_IN_MS() (clock() / (CLOCKS_PER_SEC / 1000))
#endif /* defined(BUILD_OS_DARWIN) */

#endif

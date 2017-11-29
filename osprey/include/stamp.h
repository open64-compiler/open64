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


/* $Header: /home/bos/bk/kpro64-pending/include/stamp.h 1.8 04/12/21 14:57:36-08:00 bos@eng-25.internal.keyresearch.com $ */

#ifndef __STAMP_H__
#define __STAMP_H__

#ifdef __cplusplus
extern "C" {
#endif

#if defined(TARG_NVISA)
/* NVISA:  ptx version */
#include "pathscale_defs.h"
#define	MS_STAMP 1
#define	LS_STAMP 2
#define INCLUDE_STAMP "1.2"

#elif defined(TARG_SL)
#include "pathscale_defs.h"
#define	MS_STAMP 1
#define	LS_STAMP 0
#define INCLUDE_STAMP "1.0"
#elif defined(__linux)
#include "pathscale_defs.h"

#ifdef PSC_TO_OPEN64
#ifndef OPEN64_MAJOR_VERSION
#error OPEN64_MAJOR_VERSION not defined - check include path for pathscale_defs.h
#endif

#define	MS_STAMP OPEN64_MAJOR_VERSION_NUM
#define	LS_STAMP OPEN64_MINOR_VERSION_NUM
#define INCLUDE_STAMP OPEN64_FULL_VERSION
#endif /* PSC_TO_OPEN64 */

#else
#define	MS_STAMP 7
#define	LS_STAMP 40
#define INCLUDE_STAMP "7.40"
#endif

#ifdef __cplusplus
}
#endif

#endif  /* __STAMP_H__ */

/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
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


#ifdef TARG_SL
#include "gnu/SL/config.h"
#else 
#ifdef TARG_MIPS
#include "gnu/MIPS/config.h"
#endif /* TARG_MIPS */
#endif
#ifdef TARG_IA64
#include "gnu/ia64/config.h"
#endif /* TARG_IA64 */
#if defined(TARG_IA32) || defined(TARG_X8664) || defined(TARG_NVISA)
#include "gnu/i386/config.h"
#endif /* TARG_IA32 */
#if defined TARG_PPC32
#include "gnu/rs6000/config.h"
#endif // TARG_PPC32
#ifdef TARG_LOONGSON
#include "gnu/loongson/config.h"
#endif


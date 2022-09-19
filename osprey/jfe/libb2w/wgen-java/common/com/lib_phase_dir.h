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

/* 
 * PHASEPATH:    Location for *NON* general purpose, non GNU phases(e.g be).
 * GNUPHASEPATH: Location for *NON* general purpose, GNU phases(e.g collect2)
 *
 * BINPATH:      Location for general purpose phases (e.g m4) 
 * ALTBINPAHT:   Another location for general purpose phases (e.g m4 of NUE)
 *
 * LIBPATH:      Location for compiler's specific lib.
 * ALTLIBPATH:   Specify the location where native lib reside.
 */

#ifndef lib_phase_dir_INCLUDED
#define lib_phase_dir_INCLUDED

#include <stamp.h>

#include "pathscale_defs.h"

#if defined(linux) && (defined(TARG_IA64) || defined(TARG_X8664))
    #if CROSS_COMPILATION && defined(TARG_IA64)
	    #define NAMEPREFIX	""
	    #define INTERPOSE   "/ia64-open64-linux"
    #else
	    #define NAMEPREFIX	""
	    #define INTERPOSE   ""
    #endif 

    #define BINPATH		INTERPOSE "/bin"

    #ifdef CROSS_COMPILATION
        #define ALTBINPATH  INTERPOSE "/altbin"
    #else
        #define ALTBINPATH  BINPATH
    #endif 

    #if defined(TARG_IA64)
    #define LIBPATH	INTERPOSE "/lib/gcc-lib/ia64-open64-linux/" OPEN64_FULL_VERSION
    #else
    #define LIBPATH     INTERPOSE "/lib/gcc-lib/x86_64-open64-linux/" OPEN64_FULL_VERSION
    #endif

    #define ALTLIBPATH	"/usr" INTERPOSE "/lib"

    #define PHASEPATH	    LIBPATH
    #define GNUPHASEPATH    LIBPATH

#elif defined(linux) && defined(TARG_IA32)

    #define NAMEPREFIX	 ""
    #define BINPATH		 "/bin"
    #define ALTBINPATH   BINPATH
    #define LIBPATH		 "/lib"
    #define ALTLIBPATH	 LIBPATH
    #define PHASEPATH	 "/ia32-sgi-linux/bin"
    #define GNUPHASEPATH "/lib"

#else
#define NAMEPREFIX	""
#ifdef PSC_TO_OPEN64
#define BINPATH		OPEN64_INSTALL_PREFIX "/bin"
#define ALTBINPATH	BINPATH
#define LIBPATH		OPEN64_INSTALL_PREFIX "/lib"
#define ALTLIBPATH	LIBPATH
#define PHASEPATH	OPEN64_INSTALL_PREFIX "/lib"
#define GNUPHASEPATH	OPEN64_INSTALL_PREFIX "/lib/gcc-lib/" OPEN64_TARGET "/" OPEN64_GCC_VERSION
#endif
#endif

#endif /* lib_phase_dir_INCLUDED */

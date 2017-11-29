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

/* location of internal gcc binaries, relative to installation root */
#define INTERNAL_GCC_BIN "open64-gcc-4.2.0/bin"

#if defined(VENDOR_OSP)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

#elif defined(TARG_PPC32)
#define NAMEPREFIX	 ""
#define BINPATH		"/bin"
#define ALTBINPATH	BINPATH
#define LIBPATH		"/lib/gcc-lib/ppc32-open64-linux/" OPEN64_FULL_VERSION
#define ALTLIBPATH	LIBPATH
#define PHASEPATH	"/lib/gcc-lib/ppc32-open64-linux/" OPEN64_FULL_VERSION
#define GNUPHASEPATH	"/lib/gcc-lib/ppc32-open64-linux/" OPEN64_FULL_VERSION

#elif defined(TARG_LOONGSON)
    #ifndef CROSS_COMPILATION
       #define CROSS_COMPILATION
    #endif
    #define NAMEPREFIX 	 "mips64el-n32-linux-"
    #define INTERPOSE	 	 OPEN64_TARGET
    #define BINPATH     	 "/"INTERPOSE "/bin"
    #define ALTBINPATH  	 BINPATH
    #define LIBPATH      	 "/"INTERPOSE"/lib"
    #define ALTLIBPATH  	 LIBPATH
    #define PHASEPATH    	 "/"INTERPOSE"/lib"
    #define GNUPHASEPATH	 PHASEPATH
	
	
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

#elif defined(VENDOR_SL)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    #ifndef CROSS_COMPILATION
    #if defined(TARG_MIPS) || defined(TARG_X8664_SOCC)
    #define CROSS_COMPILATION
    #endif
    #endif

    #define OPEN64_VERSION OPEN64_CMPLR_NAME_PREFIX VERSION
    #define OPEN64_GCC_VERSION "3.2"
    #define OPEN64_INSTALL_PREFIX "/"


#if defined(linux) 
    #ifdef CROSS_COMPILATION
        #define NAMEPREFIX	""
	#define INTERPOSE   OPEN64_TARGET
        #define BINPATH		"/usr/" INTERPOSE "/bin"
        #define ALTBINPATH  "/usr/" INTERPOSE "/altbin"
    #else
	#define NAMEPREFIX	""
	#define INTERPOSE   ""
        #define BINPATH		"/usr/" INTERPOSE "/bin"
        #define ALTBINPATH  BINPATH
    #endif 

    #if !defined(TARG_SL)
        #define LIBPATH	"/usr/" INTERPOSE "/lib/gcc-lib/" OPEN64_TARGET VERSION
    #else
        #define LIBPATH	"/usr/" INTERPOSE "/lib"
    #endif

    #define ALTLIBPATH	"/usr/" INTERPOSE "/lib"

    #ifdef TARG_IA64
      #define PHASEPATH       LIBPATH
    #else
      #define PHASEPATH	    OPEN64_PHASE_PATH
    #endif
      #define GNUPHASEPATH	LIBPATH

#else
    #define NAMEPREFIX	""
    #define BINPATH		"/usr/bin"
    #define ALTBINPATH   BINPATH
    #define LIBPATH		"/usr/lib"
    #define ALTLIBPATH	LIBPATH
    #define PHASEPATH	"/usr/lib32/cmplrs"
    #define GNUPHASEPATH	PHASEPATH
#endif
#endif /* defined(VENDOR_XXX) */
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#undef OPEN64_TARGET

#ifdef TARG_IA64
    #define OPEN64_TARGET "ia64-orc-linux"
    #define VERSION "2.1"
    #define OPEN64_PHASE_PATH ""
    #define OPEN64_CMPLR_NAME_PREFIX "or"
    #define OPEN64_TARGET_NAME "ia64"  // for lang_defs.c
#elif defined(TARG_X8664)
    #ifdef BUILD_ARCH_IA32  // fix bug 469
        #define OPEN64_TARGET "x86_32-linux" 
    #else 
        #define OPEN64_TARGET "x86_64-linux" 
    #endif 
    #define VERSION ""
    #define OPEN64_PHASE_PATH "x8664-linux/bin"
    #define OPEN64_CMPLR_NAME_PREFIX "open"
    #define OPEN64_TARGET_NAME "x8664"
#elif defined(TARG_IA32)
    #define OPEN64_TARGET ""
    #define VERSION ""
    #define OPEN64_PHASE_PATH "ia32-sgi-linux/bin"
    #define OPEN64_CMPLR_NAME_PREFIX "sgi"
    #define OPEN64_TARGET_NAME "ia32"
#elif defined(TARG_X8664_SOCC)
    #define OPEN64_TARGET "socc-linux/bin"
    #define VERSION ""
    #define OPEN64_PHASE_PATH "usr/socc-linux/bin"
    #define OPEN64_CMPLR_NAME_PREFIX "so"
    #define OPEN64_TARGET_NAME "so"
#elif defined(TARG_SL)
    #define OPEN64_TARGET ""
    #define VERSION ""
    #define OPEN64_PHASE_PATH "/usr/bin"
    #define OPEN64_CMPLR_NAME_PREFIX ""
    #define OPEN64_TARGET_NAME "sl"
#elif defined(TARG_MIPS)
    #define OPEN64_TARGET "mips-linux"
    #define VERSION ""
    #define OPEN64_PHASE_PATH "/usr/mips-linux/bin"
    #define OPEN64_CMPLR_NAME_PREFIX "mips"
    #define OPEN64_TARGET_NAME "mips"
#elif defined(TARG_PPC32)
    #define OPEN64_TARGET "ppc32-linux"
    #define VERSION ""
    #define OPEN64_PHASE_PATH "bin"
    #define OPEN64_CMPLR_NAME_PREFIX ""
    #define OPEN64_TARGET_NAME "ppc32"
#elif defined(TARG_LOONGSON)
    #define OPEN64_TARGET "loongson-linux"
    #define VERSION ""
    #define OPEN64_PHASE_PATH ""
    #define OPEN64_NAME_PREFIX "loong"
    #define OPEN64_CMPLR_NAME_PREFIX "loong"
    #define OPEN64_TARGET_NAME "loongson"
#endif // TARG_IA64

#endif /* lib_phase_dir_INCLUDED */

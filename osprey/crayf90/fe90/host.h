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



/* USMID:  "\n@(#)5.0_pl/headers/host.h	5.5	10/14/99 12:53:57\n" */

/* This module is for host specific information.  All uses of _HOST32 and */
/* _HOST64 and any other host defines should only be referenced in this   */
/* module.                                                                */


# if defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)
   typedef	long double		ldouble;
# elif defined(_HOST_OS_SOLARIS) || (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX))
   typedef	double			ldouble;
# else
   typedef	long double		ldouble;
# endif


# if defined(_HOST32) && defined(_TARGET64)
  typedef       long long               long_type;
  typedef       double                  float_type;
# else
#   if defined(_HOST_OS_MAX)
      typedef   double                  float_type;
#   else
      typedef   float                   float_type;
#   endif
  typedef       long                    long_type;
# endif

/* Define the host machines 64 bit integer type */

# if defined(_HOST32)
  typedef       long long               long64;
# else
  typedef       long                    long64;
# endif


typedef unsigned int                    boolean;
typedef unsigned int                    Uint;
typedef unsigned long                   Ulong;


# include <stdlib.h>            /* Needed for getenv.      */
# include <ctype.h>
# include <stdio.h>
# include <string.h>
# include <limits.h>
# include <unistd.h>

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX) || (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
# include <math.h>
# include <sys/param.h>

# if !(defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)) && defined(_DEBUG)
# include <malloc.h>             /* Needed for malloc_check */
# endif

# endif


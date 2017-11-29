/* 
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 *
 * File modified October 3, 2003 by PathScale, Inc. to update Open64 C/C++ 
 * front-ends to GNU 3.3.1 release.
 */

/* HOST_WIDE_INT definitions for the GNU compiler.
   Copyright (C) 1998, 2002 Free Software Foundation, Inc.

   This file is part of GCC.

   Provide definitions for macros which depend on HOST_BITS_PER_INT
   and HOST_BITS_PER_LONG.  */

#ifndef GCC_HWINT_H
#define GCC_HWINT_H

/* This describes the machine the compiler is hosted on.  */
#define HOST_BITS_PER_CHAR  CHAR_BIT
#define HOST_BITS_PER_SHORT (CHAR_BIT * SIZEOF_SHORT)
#define HOST_BITS_PER_INT   (CHAR_BIT * SIZEOF_INT)
#define HOST_BITS_PER_LONG  (CHAR_BIT * SIZEOF_LONG)

#ifdef HAVE_LONG_LONG
# define HOST_BITS_PER_LONGLONG (CHAR_BIT * SIZEOF_LONG_LONG)
#else
#ifdef HAVE___INT64
# define HOST_BITS_PER_LONGLONG (CHAR_BIT * SIZEOF___INT64)
#else
/* If we're here and we're GCC, assume this is stage 2+ of a bootstrap
   and 'long long' has the width of the *target*'s long long.  */
#ifndef SGI_MONGOOSE
# if GCC_VERSION > 3000
#  define HOST_BITS_PER_LONGLONG LONG_LONG_TYPE_SIZE
# endif /* gcc */
#endif
#endif /* no long long */
#endif

/* Find the largest host integer type and set its size and type.  */

/* Use long long on the host if the target has a wider long type than
   the host.  */

#if ! defined HOST_BITS_PER_WIDE_INT \
    && defined HOST_BITS_PER_LONGLONG \
    && (HOST_BITS_PER_LONGLONG > HOST_BITS_PER_LONG) \
    && (defined (LONG_LONG_MAX) || defined (LONGLONG_MAX) \
        || defined (LLONG_MAX) || defined (__GNUC__))

# ifdef MAX_LONG_TYPE_SIZE
#  if MAX_LONG_TYPE_SIZE > HOST_BITS_PER_LONG
#   define HOST_BITS_PER_WIDE_INT HOST_BITS_PER_LONGLONG
#   define HOST_WIDE_INT long long
#  endif
# else
#  if LONG_TYPE_SIZE > HOST_BITS_PER_LONG
#   define HOST_BITS_PER_WIDE_INT HOST_BITS_PER_LONGLONG
#   define HOST_WIDE_INT long long
#  endif
# endif

#endif

#ifndef HOST_BITS_PER_WIDE_INT

# if HOST_BITS_PER_LONG > HOST_BITS_PER_INT
#  define HOST_BITS_PER_WIDE_INT HOST_BITS_PER_LONG
#  define HOST_WIDE_INT long
# else
#  define HOST_BITS_PER_WIDE_INT HOST_BITS_PER_INT
#  define HOST_WIDE_INT int
# endif

#endif /* ! HOST_BITS_PER_WIDE_INT */

/* Provide defaults for the way to print a HOST_WIDE_INT
   in various manners.  */

#ifndef HOST_WIDE_INT_PRINT_DEC
# if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
#  define HOST_WIDE_INT_PRINT_DEC "%d"
#  define HOST_WIDE_INT_PRINT_DEC_C "%d"
#  define HOST_WIDE_INT_PRINT_DEC_SPACE "% *d"
# else
#  if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
#   define HOST_WIDE_INT_PRINT_DEC "%ld"
#   define HOST_WIDE_INT_PRINT_DEC_C "%ldL"
#   define HOST_WIDE_INT_PRINT_DEC_SPACE "% *ld"
#  else
#   define HOST_WIDE_INT_PRINT_DEC "%lld"
#   define HOST_WIDE_INT_PRINT_DEC_C "%lldLL"
#   define HOST_WIDE_INT_PRINT_DEC_SPACE "% *lld"
#  endif
# endif
#endif /* ! HOST_WIDE_INT_PRINT_DEC */

#ifndef HOST_WIDE_INT_PRINT_UNSIGNED
# if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
#  define HOST_WIDE_INT_PRINT_UNSIGNED "%u"
#  define HOST_WIDE_INT_PRINT_UNSIGNED_SPACE "% *u"
# else
#  if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
#   define HOST_WIDE_INT_PRINT_UNSIGNED "%lu"
#   define HOST_WIDE_INT_PRINT_UNSIGNED_SPACE "% *lu"
#  else
#   define HOST_WIDE_INT_PRINT_UNSIGNED "%llu"
#   define HOST_WIDE_INT_PRINT_UNSIGNED_SPACE "% *llu"
#  endif
# endif
#endif /* ! HOST_WIDE_INT_PRINT_UNSIGNED */

#ifndef HOST_WIDE_INT_PRINT_HEX
# if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
#  define HOST_WIDE_INT_PRINT_HEX "0x%x"
# else
#  if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
#   define HOST_WIDE_INT_PRINT_HEX "0x%lx"
#  else
#   define HOST_WIDE_INT_PRINT_HEX "0x%llx"
#  endif
# endif
#endif /* ! HOST_WIDE_INT_PRINT_HEX */

#ifndef HOST_WIDE_INT_PRINT_DOUBLE_HEX
# if HOST_BITS_PER_WIDE_INT == 64
#  if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
#   define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%x%016x"
#  else
#   if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
#    define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%lx%016lx"
#   else
#    define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%llx%016llx"
#   endif
#  endif
# else
#  if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
#   define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%x%08x"
#  else
#   if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
#    define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%lx%08lx"
#   else
#    define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%llx%08llx"
#   endif
#  endif
# endif
#endif /* ! HOST_WIDE_INT_PRINT_DOUBLE_HEX */

/* Find HOST_WIDEST_INT and set its bit size, type and print macros.
   It will be the largest integer mode supported by the host which may
   (or may not) be larger than HOST_WIDE_INT.  */

#ifndef HOST_WIDEST_INT
#if defined HOST_BITS_PER_LONGLONG \
    && HOST_BITS_PER_LONGLONG > HOST_BITS_PER_LONG
#   define HOST_BITS_PER_WIDEST_INT HOST_BITS_PER_LONGLONG
#   define HOST_WIDEST_INT long long
#   define HOST_WIDEST_INT_PRINT_DEC "%lld"
#   define HOST_WIDEST_INT_PRINT_DEC_SPACE "% *lld"
#   define HOST_WIDEST_INT_PRINT_UNSIGNED "%llu"
#   define HOST_WIDEST_INT_PRINT_UNSIGNED_SPACE "% *llu"
#   define HOST_WIDEST_INT_PRINT_HEX "0x%llx"
#  else
#   define HOST_BITS_PER_WIDEST_INT HOST_BITS_PER_LONG
#   define HOST_WIDEST_INT long
#   define HOST_WIDEST_INT_PRINT_DEC "%ld"
#   define HOST_WIDEST_INT_PRINT_DEC_SPACE "% *ld"
#   define HOST_WIDEST_INT_PRINT_UNSIGNED "%lu"
#   define HOST_WIDEST_INT_PRINT_UNSIGNED_SPACE "% *lu"
#   define HOST_WIDEST_INT_PRINT_HEX "0x%lx"
# endif /* long long wider than long */
#endif /* ! HOST_WIDEST_INT */

#endif /* ! GCC_HWINT_H */

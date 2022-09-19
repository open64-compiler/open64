/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/* Function declarations for libiberty.

   Copyright 2001, 2002 Free Software Foundation, Inc.
   
   Note - certain prototypes declared in this header file are for
   functions whoes implementation copyright does not belong to the
   FSF.  Those prototypes are present in this file for reference
   purposes only and their presence in this file should not construed
   as an indication of ownership by the FSF of the implementation of
   those functions in any way or form whatsoever.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
   
   Written by Cygnus Support, 1994.

   The libiberty library provides a number of functions which are
   missing on some operating systems.  We do not declare those here,
   to avoid conflicts with the system header files on operating
   systems that do support those functions.  In this file we only
   declare those functions which are specific to libiberty.  */

#ifndef LIBIBERTY_H
#define LIBIBERTY_H

#ifdef __cplusplus
extern "C" {
#endif

#include "ansidecl.h"

/* Get a definition for size_t.  */
#include <stddef.h>
/* Get a definition for va_list.  */
#include <stdarg.h>

#include <stdio.h>

/* If the OS supports it, ensure that the supplied stream is setup to
   avoid any multi-threaded locking.  Otherwise leave the FILE pointer
   unchanged.  If the stream is NULL do nothing.  */

extern void unlock_stream (FILE *);

/* If the OS supports it, ensure that the standard I/O streams, stdin,
   stdout and stderr are setup to avoid any multi-threaded locking.
   Otherwise do nothing.  */

extern void unlock_std_streams (void);

/* Open and return a FILE pointer.  If the OS supports it, ensure that
   the stream is setup to avoid any multi-threaded locking.  Otherwise
   return the FILE pointer unchanged.  */

extern FILE *fopen_unlocked (const char *, const char *);
extern FILE *fdopen_unlocked (int, const char *);
extern FILE *freopen_unlocked (const char *, const char *, FILE *);

/* Build an argument vector from a string.  Allocates memory using
   malloc.  Use freeargv to free the vector.  */


/* Build an argument vector from a string.  Allocates memory using
   malloc.  Use freeargv to free the vector.  */

extern char **buildargv PARAMS ((const char *)) ATTRIBUTE_MALLOC;

/* Free a vector returned by buildargv.  */

extern void freeargv PARAMS ((char **));

/* Duplicate an argument vector. Allocates memory using malloc.  Use
   freeargv to free the vector.  */

extern char **dupargv PARAMS ((char **)) ATTRIBUTE_MALLOC;


/* Return the last component of a path name.  Note that we can't use a
   prototype here because the parameter is declared inconsistently
   across different systems, sometimes as "char *" and sometimes as
   "const char *" */
#define HAVE_DECL_BASENAME 1
/* HAVE_DECL_* is a three-state macro: undefined, 0 or 1.  If it is
   undefined, we haven't run the autoconf check so provide the
   declaration without arguments.  If it is 0, we checked and failed
   to find the declaration so provide a fully prototyped one.  If it
   is 1, we found it so don't provide any declaration at all.  */
#if !HAVE_DECL_BASENAME
#if defined (__GNU_LIBRARY__ ) || defined (__linux__) \
 || defined (__FreeBSD__) || defined (__OpenBSD__) || defined (__NetBSD__) \
 || defined (__CYGWIN__) || defined (__CYGWIN32__) || defined (__MINGW32__) \
 || defined (__DragonFly__) || defined (HAVE_DECL_BASENAME)
extern char *basename (const char *) ATTRIBUTE_RETURNS_NONNULL ATTRIBUTE_NONNULL(1);
#else
/* Do not allow basename to be used if there is no prototype seen.  We
   either need to use the above prototype or have one from
   autoconf which would result in HAVE_DECL_BASENAME being set.  */
#define basename basename_cannot_be_used_without_a_prototype
#endif
#endif

/* A well-defined basename () that is always compiled in.  */

extern const char *lbasename PARAMS ((const char *));

/* Concatenate an arbitrary number of strings.  You must pass NULL as
   the last argument of this function, to terminate the list of
   strings.  Allocates memory using xmalloc.  */

extern char *concat PARAMS ((const char *, ...)) ATTRIBUTE_MALLOC;

/* Concatenate an arbitrary number of strings.  You must pass NULL as
   the last argument of this function, to terminate the list of
   strings.  Allocates memory using xmalloc.  The first argument is
   not one of the strings to be concatenated, but if not NULL is a
   pointer to be freed after the new string is created, similar to the
   way xrealloc works.  */

extern char *reconcat PARAMS ((char *, const char *, ...)) ATTRIBUTE_MALLOC;

/* Determine the length of concatenating an arbitrary number of
   strings.  You must pass NULL as the last argument of this function,
   to terminate the list of strings.  */

extern unsigned long concat_length PARAMS ((const char *, ...));

/* Concatenate an arbitrary number of strings into a SUPPLIED area of
   memory.  You must pass NULL as the last argument of this function,
   to terminate the list of strings.  The supplied memory is assumed
   to be large enough.  */

extern char *concat_copy PARAMS ((char *, const char *, ...));

/* Concatenate an arbitrary number of strings into a GLOBAL area of
   memory.  You must pass NULL as the last argument of this function,
   to terminate the list of strings.  The supplied memory is assumed
   to be large enough.  */

extern char *concat_copy2 PARAMS ((const char *, ...));

/* This is the global area used by concat_copy2.  */

extern char *libiberty_concat_ptr;

/* Concatenate an arbitrary number of strings.  You must pass NULL as
   the last argument of this function, to terminate the list of
   strings.  Allocates memory using alloca.  The arguments are
   evaluated twice!  */
#define ACONCAT(ACONCAT_PARAMS) \
  (libiberty_concat_ptr = alloca (concat_length ACONCAT_PARAMS + 1), \
   concat_copy2 ACONCAT_PARAMS)

/* Check whether two file descriptors refer to the same file.  */

extern int fdmatch PARAMS ((int fd1, int fd2));

/* Get the working directory.  The result is cached, so don't call
   chdir() between calls to getpwd().  */

extern char * getpwd PARAMS ((void));

/* Get the amount of time the process has run, in microseconds.  */

extern long get_run_time PARAMS ((void));

/* Generate a relocated path to some installation directory.  Allocates
   return value using malloc.  */

extern char *make_relative_prefix PARAMS ((const char *, const char *,
					   const char *));

/* Choose a temporary directory to use for scratch files.  */

extern char *choose_temp_base PARAMS ((void)) ATTRIBUTE_MALLOC;

/* Return a temporary file name or NULL if unable to create one.  */

extern char *make_temp_file PARAMS ((const char *)) ATTRIBUTE_MALLOC;

/* Allocate memory filled with spaces.  Allocates using malloc.  */

extern const char *spaces PARAMS ((int count));

/* Return the maximum error number for which strerror will return a
   string.  */

extern int errno_max PARAMS ((void));

/* Return the name of an errno value (e.g., strerrno (EINVAL) returns
   "EINVAL").  */

extern const char *strerrno PARAMS ((int));

/* Given the name of an errno value, return the value.  */

extern int strtoerrno PARAMS ((const char *));

/* ANSI's strerror(), but more robust.  */

extern char *xstrerror PARAMS ((int));

/* Return the maximum signal number for which strsignal will return a
   string.  */

extern int signo_max PARAMS ((void));

/* Return a signal message string for a signal number
   (e.g., strsignal (SIGHUP) returns something like "Hangup").  */
/* This is commented out as it can conflict with one in system headers.
   We still document its existence though.  */

/*extern const char *strsignal PARAMS ((int));*/

/* Return the name of a signal number (e.g., strsigno (SIGHUP) returns
   "SIGHUP").  */

extern const char *strsigno PARAMS ((int));

/* Given the name of a signal, return its number.  */

extern int strtosigno PARAMS ((const char *));

/* Register a function to be run by xexit.  Returns 0 on success.  */

extern int xatexit PARAMS ((void (*fn) (void)));

/* Exit, calling all the functions registered with xatexit.  */

extern void xexit PARAMS ((int status)) ATTRIBUTE_NORETURN;

/* Set the program name used by xmalloc.  */

extern void xmalloc_set_program_name PARAMS ((const char *));

/* Report an allocation failure.  */
extern void xmalloc_failed PARAMS ((size_t)) ATTRIBUTE_NORETURN;

/* Allocate memory without fail.  If malloc fails, this will print a
   message to stderr (using the name set by xmalloc_set_program_name,
   if any) and then call xexit.  */

extern PTR xmalloc PARAMS ((size_t)) ATTRIBUTE_MALLOC;

/* Reallocate memory without fail.  This works like xmalloc.  Note,
   realloc type functions are not suitable for attribute malloc since
   they may return the same address across multiple calls. */

extern PTR xrealloc PARAMS ((PTR, size_t));

/* Allocate memory without fail and set it to zero.  This works like
   xmalloc.  */

extern PTR xcalloc PARAMS ((size_t, size_t)) ATTRIBUTE_MALLOC;

/* Copy a string into a memory buffer without fail.  */

extern char *xstrdup PARAMS ((const char *)) ATTRIBUTE_MALLOC;

/* Copy an existing memory buffer to a new memory buffer without fail.  */

extern PTR xmemdup PARAMS ((const PTR, size_t, size_t)) ATTRIBUTE_MALLOC;

/* Physical memory routines.  Return values are in BYTES.  */
extern double physmem_total PARAMS ((void));
extern double physmem_available PARAMS ((void));

/* Compute the 32-bit CRC of a block of memory.  */
extern unsigned int xcrc32 (const unsigned char *, int, unsigned int);

/* These macros provide a K&R/C89/C++-friendly way of allocating structures
   with nice encapsulation.  The XDELETE*() macros are technically
   superfluous, but provided here for symmetry.  Using them consistently
   makes it easier to update client code to use different allocators such
   as new/delete and new[]/delete[].  */

/* Scalar allocators.  */

#define XALLOCA(T)		((T *) alloca (sizeof (T)))
#define XNEW(T)			((T *) xmalloc (sizeof (T)))
#define XCNEW(T)		((T *) xcalloc (1, sizeof (T)))
#define XDUP(T, P)		((T *) xmemdup ((P), sizeof (T), sizeof (T)))
#define XDELETE(P)		free ((void*) (P))

/* Array allocators.  */

#define XALLOCAVEC(T, N)	((T *) alloca (sizeof (T) * (N)))
#define XNEWVEC(T, N)		((T *) xmalloc (sizeof (T) * (N)))
#define XCNEWVEC(T, N)		((T *) xcalloc ((N), sizeof (T)))
#define XDUPVEC(T, P, N)	((T *) xmemdup ((P), sizeof (T) * (N), sizeof (T) * (N)))
#define XRESIZEVEC(T, P, N)	((T *) xrealloc ((void *) (P), sizeof (T) * (N)))
#define XDELETEVEC(P)		free ((void*) (P))

/* Allocators for variable-sized structures and raw buffers.  */

#define XALLOCAVAR(T, S)	((T *) alloca ((S)))
#define XNEWVAR(T, S)		((T *) xmalloc ((S)))
#define XCNEWVAR(T, S)		((T *) xcalloc (1, (S)))
#define XDUPVAR(T, P, S1, S2)	((T *) xmemdup ((P), (S1), (S2)))
#define XRESIZEVAR(T, P, S)	((T *) xrealloc ((P), (S)))

/* Type-safe obstack allocator.  */

#define XOBNEW(O, T)		((T *) obstack_alloc ((O), sizeof (T)))
#define XOBNEWVEC(O, T, N)	((T *) obstack_alloc ((O), sizeof (T) * (N)))
#define XOBNEWVAR(O, T, S)	((T *) obstack_alloc ((O), (S)))
#define XOBFINISH(O, T)         ((T) obstack_finish ((O)))

/* hex character manipulation routines */

#define _hex_array_size 256
#define _hex_bad	99
extern const char _hex_value[_hex_array_size];
extern void hex_init PARAMS ((void));
#define hex_p(c)	(hex_value (c) != _hex_bad)
/* If you change this, note well: Some code relies on side effects in
   the argument being performed exactly once.  */
#define hex_value(c)	(_hex_value[(unsigned char) (c)])

/* Definitions used by the pexecute routine.  */

#define PEXECUTE_FIRST   1
#define PEXECUTE_LAST    2
#define PEXECUTE_ONE     (PEXECUTE_FIRST + PEXECUTE_LAST)
#define PEXECUTE_SEARCH  4
#define PEXECUTE_VERBOSE 8

/* Execute a program.  */

extern int pexecute PARAMS ((const char *, char * const *, const char *,
			    const char *, char **, char **, int));

/* Wait for pexecute to finish.  */

extern int pwait PARAMS ((int, int *, int));

#if !HAVE_DECL_ASPRINTF
/* Like sprintf but provides a pointer to malloc'd storage, which must
   be freed by the caller.  */

extern int asprintf PARAMS ((char **, const char *, ...)) ATTRIBUTE_PRINTF_2;
#endif

#if !HAVE_DECL_VASPRINTF
/* Like vsprintf but provides a pointer to malloc'd storage, which
   must be freed by the caller.  */

extern int vasprintf PARAMS ((char **, const char *, va_list))
  ATTRIBUTE_PRINTF(2,0);
#endif

#define ARRAY_SIZE(a) (sizeof (a) / sizeof ((a)[0]))

/* Drastically simplified alloca configurator.  If we're using GCC,
   we use __builtin_alloca; otherwise we use the C alloca.  The C
   alloca is always available.  You can override GCC by defining
   USE_C_ALLOCA yourself.  The canonical autoconf macro C_ALLOCA is
   also set/unset as it is often used to indicate whether code needs
   to call alloca(0).  */
extern PTR C_alloca PARAMS ((size_t)) ATTRIBUTE_MALLOC;
#undef alloca
#if GCC_VERSION >= 2000 && !defined USE_C_ALLOCA
# define alloca(x) __builtin_alloca(x)
# undef C_ALLOCA
# define ASTRDUP(X) \
  (__extension__ ({ const char *const libiberty_optr = (X); \
   const unsigned long libiberty_len = strlen (libiberty_optr) + 1; \
   char *const libiberty_nptr = alloca (libiberty_len); \
   (char *) memcpy (libiberty_nptr, libiberty_optr, libiberty_len); }))
#else
# define alloca(x) C_alloca(x)
# undef USE_C_ALLOCA
# define USE_C_ALLOCA 1
# undef C_ALLOCA
# define C_ALLOCA 1
extern const char *libiberty_optr;
extern char *libiberty_nptr;
extern unsigned long libiberty_len;
# define ASTRDUP(X) \
  (libiberty_optr = (X), \
   libiberty_len = strlen (libiberty_optr) + 1, \
   libiberty_nptr = alloca (libiberty_len), \
   (char *) memcpy (libiberty_nptr, libiberty_optr, libiberty_len))
#endif

#ifdef __cplusplus
}
#endif


#endif /* ! defined (LIBIBERTY_H) */

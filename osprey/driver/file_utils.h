/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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


#include "basic.h"

/* drops path prefix in string; result points inside old string */
extern char *drop_path (char *s);

/* drops the last component of the path, leaving only the directory */
extern char *directory_path (char *s);

/* append path separator and component to directory */
extern char *concat_path (char *d, char *f);

/* check whether file exists */
extern boolean file_exists (char *path);

/* check whether file exists and has any executable bits set */
extern boolean is_executable (char *path);

/* check whether is a directory */
extern boolean is_directory (char *path);

/* check whether directory is writable */
extern boolean directory_is_writable (char *path);

/* get current working directory */
extern char *get_cwd (void);

/* save the program name in case get_executable_dir needs it */
extern void file_utils_set_program_name(char *name);

/* get the directory containing the executable */
extern char *get_executable_dir (void);

/* Copy content of file to stdout. */
extern void dump_file_to_stdout (char *filename);

/* Platform-specific macros for checking filenames */
#if defined(__CYGWIN__) || defined(_WIN32)
#define is_dir_separator(x)      ({ char ds = (x); ds == '/' || ds == '\\'; })
#define is_absolute_file_name(x) ({ const char *fn = (x); \
                                    is_dir_separator(fn[0]) || fn[1] == ':'; })
#else /* !__CYGWIN__ && !_WIN32 */
#define is_dir_separator(x)      ((x) == '/')
#define is_absolute_file_name(x) (is_dir_separator((x)[0]))
#endif /* !__CYGWIN__ && !_WIN32 */

#ifdef __MINGW32__
#define realpath(N,R) _fullpath((R),(N),_MAX_PATH)
#endif


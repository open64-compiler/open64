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


#ifndef file_util_INCLUDED
#define file_util_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif



#ifdef _KEEP_RCS_ID
static char *file_util_rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/file_util.h,v $ $Revision: 1.1 $";
#endif /* _KEEP_RCS_ID */

/* stdio.h (included in defs.h) defines the following:
 *	extern FILE *fopen ( char *filename, char *access );
 *	extern INT  fclose ( FILE *stream );
 *	extern void fflush ( FILE *stream );
 *	extern void fputs  ( char *string, FILE *stream );
 */

#include <sys/stat.h>           /* For fstat() */
#include <unistd.h>               /* For unlink() */

/* ====================================================================
 *
 * Muse File Utility Prototypes
 *
 * ====================================================================
 */

/* Determine whether a name is associated with a non-directory file: */
extern BOOL Is_File ( const char *fname );

/* Determine whether two streams are associated with the same file: */
extern BOOL Same_File (FILE *file1, FILE *file2);

/* Determine whether a filename has a given extension: */
extern BOOL Has_Extension (
  char *name,	/* The filename to check */
  char *ext	/* The extension to look for */
);

/* Search for a file with a specific extension.  First look for the
 * name given; if not found, and the name does not already have the
 * given extension, look for the name with the extension appended.
 * The name string passed must have enough space to append the
 * extension, and if that is the form found, it will be appended on
 * return.
 */
extern BOOL Find_File (
  char *name,	/* The initial filename to search for */
  char *ext	/* The default extension to try */
);

/* Replace the given file name's extension with another extension and
 * return a new filename string.  The given extension should include
 * the period if desired (a period in the original name will be
 * eliminated).
 */
extern char *New_Extension (
  const char *name,	/* The root file name, possibly with extension */
  const char *ext	/* The new extension */
);

extern char *Remove_Extension (
  char *name    /* The original file name */
);

/* Make a temporary file name from a temporary directory name, a file
 * name prefix, and the process ID:
 */
extern char *Make_Temp_File (
  char *tmp,	/* Temporary directory pathname to use */
  char *prefix	/* Prefix for file name */
);

/* Make a full path name from a base file name: */
extern char *Full_Path_Name (
  char *base,	/* Base file name (may be full pathname) */
  char *path,	/* String to receive pathname */
  INT pathlen	/* Length of path -- exceeding is a fatal error */
);

/* Return the last component of the pathname specified in 'pname'.
 * Note that we return a pointer to a portion of the input string.
 * Therefore, if the caller wants to modify the returned value, the
 * caller must first make a copy.
 */
extern char *Last_Pathname_Component ( char *pname );

/* Make an absolute path name from the file name,
 * which means no .. or . or // in the path. */
extern char *Make_Absolute_Path (char *filename);

/* return getcwd or $PWD or . */
extern char * Get_Current_Working_Directory (void);

#ifdef __cplusplus
}
#endif
#endif /* file_util_INCLUDED */

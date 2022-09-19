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



static char *source_file = __FILE__;
static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/util/file_util.c,v $ $Revision: 1.1.1.1 $";

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/param.h>
#include <stdlib.h>
#include <stdio.h>
#include <cmplrs/rcodes.h>

#include "defs.h"
#include "file_util.h"

/* ====================================================================
 *
 * Is_File
 *
 * Determine whether a pathname represents an existing regular file.
 *
 * ====================================================================
 */

BOOL
Is_File ( const char *fname )
{
    struct stat desc;		    /* File status descriptor */

    if (fname == NULL)
	return FALSE;
  
    if ( stat ( fname, &desc ) != 0 )
	return FALSE;		    /* existing? */
    return ( (desc.st_mode & S_IFREG) != 0 ); /* regular file? */
}


/* ====================================================================
 *
 * Same_File
 *
 * Determine whether two streams are associated with the same file.
 * Returns FALSE (not the same) if there are any problems obtaining
 * status.
 *
 * ====================================================================
 */

BOOL
Same_File ( file1, file2 )
  FILE *file1, *file2;
{
  struct stat d1, d2;	/* Stream status descriptors */

  if ( file1 == NULL || file2 == NULL ) return FALSE;
  if ( fstat ( fileno(file1), &d1 ) == -1 ) return FALSE;
  if ( fstat ( fileno(file2), &d2 ) == -1 ) return FALSE;
  return ( d1.st_ino == d2.st_ino ) && ( d1.st_dev == d2.st_dev );
}

#ifndef MONGOOSE_BE
/* ====================================================================
 *
 * Find_File
 *
 * Search for a file with a specific extension.  First look for the
 * name given; if not found, and the name does not already have the
 * given extension, look for the name with the extension appended.
 * The name string passed must have enough space to append the
 * extension, and if that is the form found, it will be appended on
 * return.
 *
 * ====================================================================
 */

BOOL
Find_File ( name, ext )
  char *name;	/* The initial filename to search for */
  char *ext;	/* The default extension to try */
{
  INT16 len;

  /* Check for the initial filename: */
  if ( Is_File(name) ) return TRUE;

  /* Try appending the default extension: */
  if ( ! Has_Extension ( name, ext ) ) {
    len = strlen(name);
    strcat (name, ext);
    if ( Is_File(name) ) return TRUE;
    name[len] = 0;	/* Restore the original file name */
  }

  /* Nothing worked */
  return FALSE;
}

/* ====================================================================
 *
 * Has_Extension
 *
 * Determine whether a filename has a given extension.
 *
 * ====================================================================
 */

BOOL
Has_Extension ( name, ext )
  char *name;	/* The filename to check */
  char *ext;	/* The extension to look for */
{
  INT16 nlen = strlen(name);
  INT16 elen = strlen(ext);

  /* If ext is longer than name, no chance: */
  if ( elen > nlen ) return FALSE;

  /* Otherwise compare the tail of name to ext: */
  return ( strcmp ( &name[nlen-elen], ext ) == 0 );
}
#endif /* MONGOOSE_BE */


/* ====================================================================
 *
 * New_Extension
 *
 * Replace the given file name's extension with another extension and
 * return a new filename string.  The given extension should include
 * the period if desired (a period in the original name will be
 * eliminated).
 *
 * ====================================================================
 */

char *
New_Extension ( const char *name, const char *ext )
{
  char *new;
  INT16 len, i;

  /* Allocate a new name string: */
  len = strlen(name);
  new = (char *) malloc ( len + strlen(ext) + 1 );
  strcpy ( new, name );
  for ( i=len-1; i>=0; i-- ) {
    if ( new[i] == '/' ) break;	/* Don't touch directory prefixes */
    if ( new[i] == '.' ) {
      new[i] = 0;
      break;
    }
  }
  strcat ( new, ext );
  return new;
}

/* ====================================================================
 *
 * Remove_Extension
 *
 * Remove the extension from a given file name. The period in the
 * original file name is also removed. Assumes that the directory
 * prefixes have already been removed.
 *
 * ====================================================================
 */

char *
Remove_Extension ( name )
  char *name; /* The file name with extension */
{
  char *new;
  INT16 len, i;
  /* Allocate a new name string: */
  len = strlen(name);
  new = (char *) malloc ( len );
  strcpy ( new, name );
  for ( i=len-1; i>=0; i-- ) {
    if ( new[i] == '.' ) {
      new[i] = 0;
      break;
    }
  }
  return new;
}


#ifndef MONGOOSE_BE
/* ====================================================================
 *
 * Make_Temp_File
 *
 * Make a temporary file name from a temporary directory name, a file
 * name prefix, and the process ID.
 *
 * ====================================================================
 */

char *
Make_Temp_File ( tmp, prefix )
  char *tmp;	/* Temporary directory pathname to use */
  char *prefix;	/* Prefix for file name */
{
  INT16 len = strlen(tmp);
  char *name;	/* Result temporary file pathname */

  name = (char *) malloc (len + 20);
  if ( len > 0 ) {
    strcpy ( name, tmp );
    name[len++] = '/';
  }
  strcpy ( &name[len], prefix );
  len += strlen(prefix);
  sprintf ( &name[len], "%d", getpid() );
  return name;
}

/* current working directory */
static char *cwd = NULL;
static INT cwd_size;

char *
Get_Current_Working_Directory (void)
{
  char *cwd;
  cwd = getcwd((char *) NULL, MAXPATHLEN);
  if (cwd == NULL) {
    cwd = getenv("PWD");
	if (cwd == NULL) {
		/* can't get path */
		cwd = ".";
        }
  }
  return cwd;
}

/* ====================================================================
 *
 * Full_Path_Name
 *
 * Make a full path name from a base file name.
 *
 * ====================================================================
 */

char *
Full_Path_Name ( base, path, pathlen )
  char *base;	/* Base file name (may be full pathname) */
  char *path;	/* String to receive pathname */
  INT pathlen;	/* Length of path -- exceeding is a fatal error */
{
  INT baselen;

  /* If the base name is NULL, just give up: */
  if ( base == NULL ) return NULL;
  baselen = strlen (base);
  if ( baselen > pathlen ) exit(RC_SYSTEM_ERROR);

  /* If the base name is a full path, just copy it and return: */
  if (base[0] == '/') {
    strcpy (path, base);
    return NULL;
  }

  /* Otherwise prepend the current working directory name: */
  if ( cwd == NULL ) {
	cwd = Get_Current_Working_Directory();
  	cwd_size = strlen (cwd);
  }
  if ( baselen + cwd_size > pathlen ) exit(RC_SYSTEM_ERROR);
  strcpy (path, cwd);
  strcat (path, "/");
  strcat (path, base);
  return path;
}
#endif /* MONGOOSE_BE */

/* ====================================================================
 *
 * Last_Pathname_Component
 *
 * Return the last component of the pathname specified in 'pname'.
 * I.e., if the input is "/da/db/dc/f.x", return a pointer to "f.x".
 * If there are no slashes in the input, just return the input.
 *
 * Note that we return a pointer to a portion of the input string.
 * Therefore, if our caller wants to modify the returned value, the
 * caller must first make a copy.
 *
 * ====================================================================
 */

char *
Last_Pathname_Component ( char *pname )
{
  char *cp = pname + strlen(pname);

  while (cp != pname) {
    if (*cp == '/') return cp+1;
    --cp;
  }
  if (*cp == '/') return cp+1;
  return cp;

} /*end: Last_Pathname_Component */


#ifndef MONGOOSE_BE
/* Eliminates "//", "/.", and "/.." from the path, to the extent that it
 * is possible.
 * "//"      -> "/"
 * "/./"     -> "/"
 * "a/b/../" -> "a/"
 */
static char *
normalize_path(char * path)
{
  char * inp = path, *outp = path, *tmp;

  while (inp != NULL && *inp != '\0') {
    if (inp[0] == '/') {
      if (inp[1] == '/')
	inp+= 1;
      else if (inp[1] == '.' && inp[2] == '/')
	inp += 2;
      else if (inp[1] == '.' && inp[2] == '.' && inp[3] == '/') {
	/* Skip up one level up output path */
	for (tmp = outp-1;
	     tmp >= path && *tmp != '/';
	     tmp -= 1);
        /* Check that we skipped one level up, but not past ".." */
        if (tmp >= path && tmp[0] == '/' && 
	  (tmp[1] != '.' || tmp[2] != '.'))
	  outp = tmp;
	else {
	  *outp++ = '/';
	  *outp++ = '.';
	  *outp++ = '.';
	}
	inp+= 3;
      } /* if */
      else
	*outp++ = *inp++;
    } /* if */
    else
      *outp++ = *inp++;
  } /* while */
  
  *outp = '\0';
  return path;
}

#define is_absolute_file_name(file_name)	((file_name)[0] == '/')


/* Make an absolute path name from the file name,
 * which means no .. or . or // in the path. */
extern char *
Make_Absolute_Path (char *filename)
{
   char  *normalized;
   INT64 cwd_length;

   if ( cwd == NULL ) {
	cwd = Get_Current_Working_Directory();
   	cwd_size = strlen (cwd);
   }
   normalized = (char *)malloc(cwd_size+strlen(filename)+2);
   if (normalized == NULL) {
      perror("malloc");
      exit(RC_SYSTEM_ERROR);
   }
   if (is_absolute_file_name(filename))
      (void)strcpy(normalized, filename);
   else
   {
      cwd_length = cwd_size;
      strcpy(normalized, cwd);
      if (cwd[cwd_length - 1] != '/')
      {
	 normalized[cwd_length++] = '/';
	 normalized[cwd_length] = '\0';
      }
      (void)strcpy(&normalized[cwd_length], filename);
   }
   (void)normalize_path(normalized);
   return normalized;
}
#endif /* MONGOOSE_BE */

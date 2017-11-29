/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>
#include "errors.h"
#include "string_utils.h"
#include "file_utils.h"

#if defined(_WIN32)
#define DIR_SEPARATOR '\\'
#define DIR_SEPARATOR_STR "\\"
#else
#define DIR_SEPARATOR '/'
#define DIR_SEPARATOR_STR "/"
#endif

static char *saved_orig_program_name;

/* drops path prefix in string */
char *
drop_path (char *s)
{
	char *tail = NULL;
	char *t;
	for (t = s; *t; t++) {
		if (is_dir_separator (*t))
			tail = t;
	}

	if (tail == NULL) {
		return s;	/* no path prefix */
	} else {
		tail++;		/* skip the slash */
		return tail;	/* points inside s, not new string! */
	}
}

/* drops the last component of the path, leaving only the directory */
char *
directory_path (char *s)
{
        char *t, *tail = NULL;
	char path[MAXPATHLEN];
	for (t = s; *t; t++) {
		if (is_dir_separator(*t))
			tail = t;
	}
	if (tail == NULL) {
		return NULL;
	} else {
		return substring_copy (s, 0, tail-s);
	}
}

char *
concat_path (char *d, char *f)
{
	if ((d == NULL) || (strlen (d) == 0) || 
            (strcmp(d, DIR_SEPARATOR_STR) == 0)) {
		/* Directory is root, don't return //f */
		return concat_strings (DIR_SEPARATOR_STR, f);
	} else if ((f == NULL) || (strlen (f) == 0)) {
		/* file is null, return directory portion only */
		return d;
	} else {
		char *path = (char *) malloc(strlen(d) + strlen(f) + 2);
		strcpy (path, d);
		strcat (path, DIR_SEPARATOR_STR);
		strcat (path, f);
		return path;
	}
}

boolean
file_exists (char *path)
{
	int st;
	struct stat sbuf;
	st = stat(path, &sbuf);
	if (st == -1 && (errno == ENOENT || errno == ENOTDIR))
		return FALSE;
	else
		return TRUE;
}

boolean
is_executable (char *path)
{
	int st;
	struct stat sbuf;
	st = stat(path, &sbuf);
	if (st == -1 && (errno == ENOENT || errno == ENOTDIR))
		return FALSE;
	else
                /* this check is not completely accurate */
#if defined(_WIN32)
		/* only have user permissions */
		return (sbuf.st_mode & (S_IXUSR)) != 0;
#else
		return (sbuf.st_mode & (S_IXUSR|S_IXGRP|S_IXOTH)) != 0;
#endif
}

boolean
is_directory (char *path)
{
        /* check if . file exists */
        buffer_t buf;
	if (*path == NIL) return FALSE;		/* empty path */
        strcpy(buf, path);
        strcat(buf, DIR_SEPARATOR_STR);
        strcat(buf, ".");
        if (file_exists(buf)) 
		return TRUE;
        else 
		return FALSE;
}

/* check if directory is writable */
boolean
directory_is_writable (char *path)
{
	FILE *f;
	char *s;
	int fd;
#if defined(_WIN32)
	s = concat_strings(path, "\\ctm.XXXXXX");
	/* mingw doesn't have mkstemp */
	s = mktemp(s);
	f = fopen(s, "w");
#else
	s = concat_strings(path, "/ctm.XXXXXX");
	fd = mkstemp(s);
	if (fd == -1)
		return FALSE;
	f = fdopen(fd, "w");
#endif
	if (f == NULL) {
		return FALSE;
	} else {
		fclose (f);
		unlink(s);
		return TRUE;
	}
}

char *
get_cwd (void)
{
	char *cwd = getcwd((char *) NULL, MAXPATHLEN);
	if (cwd == NULL) {
		cwd = getenv("PWD");
		if (cwd == NULL) {
			/* can't get path */
			cwd = ".";
		}
	}
	return string_copy(cwd);
}

void file_utils_set_program_name(char *name)
{
        saved_orig_program_name = name;
}

// Get program path from PATH variable.
static char *
get_executable_dir_from_path(char *name)
{
  if (is_dir_separator (name[0]))
    return name;
#if defined(_WIN32)
  else if (name[1] == ':' && is_dir_separator(name[2]))
    return name;
#endif
  else {
    /* relative path */
    char *path = getenv("PATH");
    if (path != NULL) {
      char *p = string_copy(path);
      char *tmp;
      char *dir;
#if defined(_WIN32)
#if defined(__MINGW32__)
      /* mingw uses semicolon as separator */
      while ((dir = strtok(p, ";")) != NULL) {
#else
      /* strtok_r doesn't exist */
      while ((dir = strtok(p, ":")) != NULL) {
#endif /* __MINGW32__ */
#else
      while ((dir = strtok_r(p, ":", &tmp)) != NULL) {
#endif /* _WIN32 */
	if (is_directory(dir)) {
	  char filename[MAXPATHLEN];
	  snprintf(filename, MAXPATHLEN, "%s%s%s", dir, DIR_SEPARATOR_STR, name);
	  if (is_executable(filename)) {
	    return string_copy(filename);
	  }
#if defined(_WIN32)
	  else {
	     /* if not found, then try again with or without .exe suffix,
	      * depending on whether the original filename had one; 
	      * e.g. if invoked opencc when opencc.exe is full name.
	      */
	      char *suffix = get_suffix(filename);
	      if (suffix != NULL && strcmp(suffix, "exe") == 0) {
	        /* has suffix so try without suffix */
	        *(suffix-1) = NIL;
	      }
	      else {
	        /* add exe suffix */
	        strcat(filename, ".exe");
	      }
	      if (is_executable(filename)) {
	          return string_copy(filename);
	      }
	  }
#endif
	}
        p = NULL;
      }
    }
  }
  return name;
}

char *
get_executable_dir (void)
{
	char path[MAXPATHLEN];
	int rval = 0;
	int i;

#if defined(linux)
	/* Look in this special place for a link to the executable. This
	   only works on Linux, but it is benign if we try it elsewhere. */
	rval = readlink ("/proc/self/exe", path, sizeof(path));
#endif
	if (rval <= 0) {
		// If can't read /proc/self/exe, get program path from PATH
		// variable.
		char *p = get_executable_dir_from_path(saved_orig_program_name);
		strncpy(path, p, sizeof(path));
		rval = strlen(path);
	} else {
		path[rval] = '\0';	// readlink doesn't append NULL
	}
	if (rval > 0) {
		for (i=rval-1; i >= 0; i--) {
			if (is_dir_separator(path[i])) break;
		}
		if (i > 0) {
			/* Overwrite the trailing slash, giving the directory
			   portion of the path. */
			path[i] = '\0';      
		} else if (i == 0) {
			/* Directory is the root */
		        strcpy (path, DIR_SEPARATOR_STR);
		}
		if (is_directory (path)) {
			/* Verify that it is a directory */
			return string_copy (path);
		}
	}

	/* TBD: try to extract the name from argv0 */

	/* Can't get anything reasonable. */
	return NULL;
}

void
dump_file_to_stdout(char *filename)
{
  const int buf_size = 1024;
  char buf[buf_size];
  FILE *f;
  int n;

  if (filename == NULL || !file_exists(filename))
    internal_error("file does not exist");
  
  f = fopen(filename, "r");
  if (f == NULL)
    internal_error("cannot open file for read");

  // Copy the content of file to stdout.
  while ((n = fread(buf, 1, buf_size, f)) > 0) {
    write(1, buf, n);
  }
  fclose(f);
}

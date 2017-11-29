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


#ifndef __MAKE_DEPEND_H__
#define __MAKE_DEPEND_H__

#ifdef __cplusplus
extern "C" {
#endif

/*
 * An opaque handle on the structure used to update a single target's make
 * dependencies rule.  Each target built in a directory has one rule in an
 * automatically-generated make-dependencies file, usually named Makedepend
 * and sincluded by the directory's makefile.  Each rule relates the target
 * to all source files that it #includes and all libraries/runtimes that it
 * links against.
 */
typedef struct mdrule *MDhandle;

/*
 * Call MDopen(toolname, filename, target, errorfunc) to open a make rule
 * for target in filename.  Target may be null if the calling tool does not
 * yet know its output file's name.  Errorfunc is called for malloc failure
 * and system call errors.  Toolname is used to distinguish link-editor and
 * include-file dependencies for the same target (e.g., a command made using
 * a null-suffix rule).
 *
 * MDupdate(handle, dependency) adds the named dependency to the right part
 * of target's rule.  Dependency must be in persistent store.
 *
 * MDclose(handle, target) updates the dependencies file named by MDopen's
 * filename argument.  If a null target was passed to MDopen, a non-null one
 * must be passed to MDclose.  MDclose locks filename, updates target's rule
 * in it or adds a new rule if target had none, and unlocks filename.  Lines
 * in filename not matching '^target *:' are left alone.
 */
extern MDhandle	MDopen(const char *, const char *, const char *, 
		       void (*)(const char *, ...));
extern void	MDupdate(MDhandle, char *);
extern void	MDclose(MDhandle, char *);

#ifdef __cplusplus
}
#endif

#endif /* __MAKE_DEPEND_H__ */

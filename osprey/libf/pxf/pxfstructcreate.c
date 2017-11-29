/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


static char USMID[] = "@(#) libf/pxf/pxfstructcreate.c	92.1	09/15/99 10:42:00";
#include <fortran.h>
#include <malloc.h>
#include <errno.h>
#include <liberrno.h>
#include <string.h>
#include "pxfstruct.h"
#include "pxfstructtable.h"
#include "table.h"

#ifndef _UNICOS
#include <stddef.h>
#endif

extern char *_fc_acopy(_fcd f);

#define NUMCONST sizeof(strtble)/sizeof(struct strtble)

#define FREE(_A_,_B_) free(((_A_)pxfhand.pxfstructptr)->_B_);

/*
 * Description:
 *	PXFSTRUCTCREATE creates a copy of the requested structure
 *	and returns a "handle" for it. The "handle" can be used to
 *	reference this copy of the structure.
 * Standard:
 *	Section 8.3.1 of Posix 1003.9-1992
 * Parameters:
 *	structname (input)  name of the structure
 *	jhandle	   (output) handle for the structure created
 *	ierror	   (output) error code
 */

#ifdef _UNICOS
void
PXFSTRUCTCREATE(
#else
void
_PXFSTRUCTCREATE(
#endif
	  _fcd structname, _f_int *jhandle, _f_int *ierror)
{
	char *name;
	int i;
	_f_int jhandle_temp;
	void *structp;

	name = _fc_acopy(structname);

	if (name == NULL) {
		*ierror = ENOMEM;
		return;
	}


	for (i = 0; i < NUMCONST; i++) {
		if (strcmp(name,strtble[i].str) == 0) {

		        /* Using calloc since it zeroes all of the allocated memory.
			 * This is need for some routines that use structures with
			 * dynamically allocated memory.
			 */
			structp = calloc(1,strtble[i].size);
			if (structp==NULL){
				*ierror = ENOHANDLE;
			} else {
				jhandle_temp =
				  _pxfhandle_table_add(&_pxfhandle_table,
						       structp,
						       strtble[i].type);
				if (jhandle_temp < 0) {
				  *ierror = ENOHANDLE;
				} else {
				  *ierror = 0;
				  *jhandle = jhandle_temp;
				}
			}
			free(name);
			return;
		}
	}
	free(name);
	*ierror = ENONAME;
}


#ifndef _UNICOS
void
pxfstructcreate_(
	  char *structname,
	  _f_int *jhandle,
	  _f_int *ierror,
	  _f_int structnamelen)
{
  _PXFSTRUCTCREATE(_cptofcd(structname, structnamelen),
		   jhandle, ierror);
}

#endif


/*
 * Description:
 *	PXFSTRUCTFREE deletes the copy of the structure referenced by
 *	jhandle.
 * Parameters:
 *	jhandle	   (input) handle for the structure to be deleted
 *	ierror	   (output) error code
 *
 */

#ifdef _UNICOS
void
PXFSTRUCTFREE(
#else
void
_PXFSTRUCTFREE(
#endif
	      _f_int *jhandle,
	      _f_int *ierror)
{
  int i;
  struct pxfhandle pxfhand;

  pxfhand = _pxfhandle_table_lookup(&_pxfhandle_table, *jhandle);
  if (pxfhand.pxfstructptr == NULL) {
    *ierror = EBADHANDLE;
    return;
  }

  switch (pxfhand.pxftype) {
  case PXF_FLOCK:
  case PXF_UTIMBUF:
  case PXF_UNAMBUF:
  case PXF_STATBUF:
  case PXF_TMSBUF:
  case PXF_DIRENT:
  case PXF_SIGSET:
  case PXF_SIGACTION:
    
    break;

  case PXF_GROUP:
    FREE(struct group *,gr_name)
    FREE(struct group *,gr_passwd)
    if (((struct group *)pxfhand.pxfstructptr)->gr_mem != NULL) {
      for(i=0;((struct group *)pxfhand.pxfstructptr)->gr_mem[i] != NULL; i++) {
	FREE(struct group *,gr_mem[i])
      }
    }
    break;
  case PXF_PASSWD:
    FREE(struct passwd *,pw_name)
    FREE(struct passwd *,pw_dir)
    FREE(struct passwd *,pw_shell)
/* components not supported by POSIX 1003.9-1992, but supported by target OSes */
    FREE(struct passwd *,pw_passwd)
#ifndef	_LITTLE_ENDIAN
    FREE(struct passwd *,pw_age)
    FREE(struct passwd *,pw_comment)
#endif	/* not _LITTLE_ENDIAN */
    FREE(struct passwd *,pw_gecos)
    break;

  case PXF_TERMIOS:
    FREE(struct termios *,c_cc)
    break;
  }

  _pxfhandle_table_remove(&_pxfhandle_table, *jhandle);
  *ierror = 0;
  return;
}

#ifndef _UNICOS
void
pxfstructfree_(
	       _f_int *jhandle,
	       _f_int *ierror)
{
  _PXFSTRUCTFREE(jhandle, ierror);
}

#endif



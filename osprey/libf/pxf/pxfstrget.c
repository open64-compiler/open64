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


#pragma ident "@(#) libf/pxf/pxfstrget.c	92.2	09/15/99 10:41:12"


#include <fortran.h>
#include <malloc.h>
#include <errno.h>
#include <liberrno.h>
#include <string.h>
#include <sys/utsname.h>
#include <grp.h>
#include <pwd.h>
#include <dirent.h>
#include "pxfstruct.h"
#include "table.h"

#define SETIFMATCH(_A_,_B_,_C_) if (strcmp(_A_,#_C_) == 0) {\
		*ilen = strlen(((struct _B_ *)(pxfhand.pxfstructptr))->_C_);\
		if (_c2fcpy(((struct _B_ *)(pxfhand.pxfstructptr))->_C_,value) == -1) {\
			*ierror = ETRUNC;\
			strncpy(_fcdtocp(value),((struct _B_ *)(pxfhand.pxfstructptr))->_C_,_fcdlen(value));\
		}\
		}

#define GETIFMATCH(_A_,_B_,_C_) if (strcmp(_A_,#_C_) == 0) {\
		if (*ilen == 0) {\
			((struct _B_ *)(pxfhand.pxfstructptr))->_C_ = _fc_acopy(value);\
		}\
		else\
			strncpy(((struct _B_ *)(pxfhand.pxfstructptr))->_C_, _fcdtocp(value), *ilen);\
		}

extern char *_fc_acopy(_fcd f);

/*
 * Description:
 *      PXFSTRGET allows access to character values stored in individual
 *      components of the structure.
 * Standard:
 *      Section 8.3.2 of Posix 1003.9-1992
 * Parameters:
 *      jhandle (input)  handle for structure.
 *      compnam (input)  Component name.
 *      value  (output) Value of component described by jhandle and
 *                       compnam.
 *      ilen    (output) Length of value.
 *      ierror  (output) Error code.
 */

#ifdef _UNICOS
void
PXFSTRGET(
#else
void
_PXFSTRGET(
#endif
	_f_int *jhandle,
	_fcd compnam,
	_fcd value,
	_f_int *ilen,
	_f_int *ierror)
{
	char	*component;
	struct	pxfhandle pxfhand;

	*ierror	= 0;
	component = _fc_acopy(compnam);
	if (component == NULL) {
		*ierror	= ENOMEM;
		return;
	}

        pxfhand = _pxfhandle_table_lookup(&_pxfhandle_table, *jhandle);
        if (pxfhand.pxfstructptr == NULL) {
          *ierror = EBADHANDLE;
          return;
        }

	switch (pxfhand.pxftype){
		case PXF_UNAMBUF:
			SETIFMATCH(component,utsname,sysname)
			else SETIFMATCH(component,utsname,nodename)
			else SETIFMATCH(component,utsname,release)
			else SETIFMATCH(component,utsname,version)
			else SETIFMATCH(component,utsname,machine)
			else {
				*ierror = ENONAME;
			}
			break;
	        case PXF_GROUP:
		        SETIFMATCH(component,group,gr_name)
			else {
			        *ierror = ENONAME;
			}
                        break;
		case PXF_PASSWD:
			SETIFMATCH(component,passwd,pw_name)
			else SETIFMATCH(component,passwd,pw_dir)
			else SETIFMATCH(component,passwd,pw_shell)
	/* additional field supported by UNICOS, Solaris, and IRIX but not mentioned
	 * in the Posix 1003.9-1992. */
			else SETIFMATCH(component,passwd,pw_passwd)
#ifndef	_LITTLE_ENDIAN
			else SETIFMATCH(component,passwd,pw_age)
			else SETIFMATCH(component,passwd,pw_comment)
#endif	/* not _LITTLE_ENDIAN */
			else SETIFMATCH(component,passwd,pw_gecos)
			else {
			        *ierror = ENONAME;
			}
			break;
	        case PXF_DIRENT:
		        SETIFMATCH(component,dirent,d_name)
			else {
			        *ierror = ENONAME;
			}
                        break;
		default:
			*ierror = ENONAME;
			break;
	}
	free(component);
}

#ifndef _UNICOS
void
pxfstrget_(
	   _f_int *jhandle,
	   char *compnam,
	   char *value,
	   _f_int *ilen,
	   _f_int *ierror,
	   _f_int compnamlen,
	   _f_int valuelen
)
{
  _PXFSTRGET( jhandle, _cptofcd(compnam, compnamlen),
	      _cptofcd(value, valuelen), ilen, ierror);
}
#endif

/*
 * Description:
 *      PXFSTRSET provides a way to set values stored in individual
 *      components of the structure.
 * Standard:
 *      Section 8.3.2 of Posix 1003.9-1992
 * Parameters:
 *      jhandle (input)  handle for structure.
 *      compnam (input)  Component name.
 *      value  (input)  New value of component described by jhandle and
 *                       compnam.
 *      ilen    (input)  Length of value.
 *      ierror  (output) Error code.
 */

#ifdef _UNICOS
void
PXFSTRSET(
#else
void
_PXFSTRSET(
#endif
	_f_int *jhandle,
	_fcd compnam,
	_fcd value,
	_f_int *ilen,
	_f_int *ierror)
{
	char	*component;
	struct	pxfhandle pxfhand;

	*ierror	= 0;
	component	= _fc_acopy(compnam);
	if (component == NULL) {
		*ierror	= ENOMEM;
		return;
	}

        pxfhand = _pxfhandle_table_lookup(&_pxfhandle_table, *jhandle);
        if (pxfhand.pxfstructptr == NULL) {
          *ierror = EBADHANDLE;
          return;
        }

	switch (pxfhand.pxftype){
		default:
			*ierror = ENONAME;
			break;
	}
	free(component);
}

#ifndef _UNICOS
void
pxfstrset_(
	  _f_int *jhandle,
	  char *compnam,
	  char *value,
	  _f_int *ilen,
	  _f_int *ierror,
	  _f_int compnamlen,
	  _f_int valuelen
)
{
  _PXFSTRSET( jhandle, _cptofcd(compnam, compnamlen),
	      _cptofcd(value, valuelen), ilen, ierror);
}
#endif

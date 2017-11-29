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


#pragma ident "@(#) libf/pxf/pxfget.c	92.1	09/15/99 10:40:48"
#include <fortran.h>
#include <malloc.h>
#include <errno.h>
#include <utime.h>
#include <string.h>
#include <liberrno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <grp.h>
#include <pwd.h>
#include "pxfstruct.h"
#include "table.h"
#include <sys/signal.h>
#include <sys/termios.h>

#define SETIFMATCH(_A_,_B_,_C_) if (strcmp(_A_,#_C_) == 0) {\
		*value = ((struct _B_ *)(pxfhand.pxfstructptr))->_C_;\
		}
#define GETIFMATCH(_A_,_B_,_C_) if (strcmp(_A_,#_C_) == 0) {\
		((struct _B_ *)(pxfhand.pxfstructptr))->_C_ = *value;\
		}

/*
 * Description:
 *	PXFINTGET allows access to values stored in individual components
 *	of the structure.
 * Standard:
 *	Section 8.3.2 of Posix 1003.9-1992
 * Parameters:
 *	jhandle (input)	 handle for structure.
 *	compnam (input)	 Component name.
 *	value	(output) Value of component described by jhandle and
 *			 compnam.
 *	ierror	(output) Error code.
 */
#ifdef _UNICOS
void
PXFINTGET(
#else
void
_PXFINTGET(
#endif
	   _f_int *jhandle,
	   _fcd compnam,
	   _f_int *value,
	   _f_int *ierror)
{
	char	*component;
	struct pxfhandle pxfhand;
	char	**gr_mem;
	int	i;

	*ierror = 0;
	component = _fc_acopy(compnam);
	if (component == NULL) {
	         *ierror = ENOMEM;
		return;
	}

	pxfhand = _pxfhandle_table_lookup(&_pxfhandle_table, *jhandle);
	if (pxfhand.pxfstructptr == NULL) {
	  *ierror = EBADHANDLE;
	  return;
	}

	switch (pxfhand.pxftype){

	   case PXF_FLOCK:

	      SETIFMATCH(component,flock,l_type)
	      else SETIFMATCH(component,flock,l_whence)
	      else SETIFMATCH(component,flock,l_start)
	      else SETIFMATCH(component,flock,l_len)
#ifndef	_LITTLE_ENDIAN
	      else SETIFMATCH(component,flock,l_sysid)
#endif
	      else SETIFMATCH(component,flock,l_pid)
	      else {
	         *ierror = ENONAME;
	      }
	      break;
	   case PXF_UTIMBUF:
	      SETIFMATCH(component,utimbuf,actime)
	      else SETIFMATCH(component,utimbuf,modtime)
	      else {
	         *ierror = ENONAME;
	      }
	      break;
	   case PXF_STATBUF:
	      SETIFMATCH(component,stat,st_dev)
	      else SETIFMATCH(component,stat,st_ino)
	      else SETIFMATCH(component,stat,st_mode)
	      else SETIFMATCH(component,stat,st_nlink)
	      else SETIFMATCH(component,stat,st_uid)
	      else SETIFMATCH(component,stat,st_gid)
	      else SETIFMATCH(component,stat,st_rdev)
	      else SETIFMATCH(component,stat,st_size)
	      else SETIFMATCH(component,stat,st_atime)
	      else SETIFMATCH(component,stat,st_mtime)
	      else SETIFMATCH(component,stat,st_ctime)
	      else SETIFMATCH(component,stat,st_blocks)
	      else SETIFMATCH(component,stat,st_blksize)
#ifdef	_UNICOS
	      else SETIFMATCH(component,stat,st_acid)
	      else SETIFMATCH(component,stat,st_count)
	      else SETIFMATCH(component,stat,st_msize)
	      else SETIFMATCH(component,stat,st_oblksize)
	      else SETIFMATCH(component,stat,st_allocf)
	      else SETIFMATCH(component,stat,st_cblks)
	      else SETIFMATCH(component,stat,st_cbits)
	      else SETIFMATCH(component,stat,st_dm_mode)
	      else SETIFMATCH(component,stat,st_dm_port)
	      else SETIFMATCH(component,stat,st_dm_state)
	      else SETIFMATCH(component,stat,st_dm_status)
	      else SETIFMATCH(component,stat,st_dm_mid)
	      else SETIFMATCH(component,stat,st_dm_key)
	      else SETIFMATCH(component,stat,st_hasacl)
	      else SETIFMATCH(component,stat,st_hascomps)
	      else SETIFMATCH(component,stat,st_slevel)
	      else SETIFMATCH(component,stat,st_secflg)
	      else SETIFMATCH(component,stat,st_intcls)
	      else SETIFMATCH(component,stat,st_intcat)
	      else SETIFMATCH(component,stat,st_site)
	      else SETIFMATCH(component,stat,st_nindir)
#endif	/* _UNICOS */
	      else {
	         *ierror = ENONAME;
	      }
	      break;
	   case PXF_TMSBUF:
	      SETIFMATCH(component,tms,tms_utime)
	      else SETIFMATCH(component,tms,tms_stime)
	      else SETIFMATCH(component,tms,tms_cutime)
	      else SETIFMATCH(component,tms,tms_cstime)
	      else {
		 *ierror = ENONAME;
	      }
              break;
	   case PXF_GROUP:
	      SETIFMATCH(component,group,gr_gid)
	      else if (strcmp(component,"gr_nmem\0") == 0) {
		/* find length of null terminated array */
	        gr_mem = ((struct group *)(pxfhand.pxfstructptr))->gr_mem;
		for(i = 0; gr_mem[i] != NULL; i++);
		*value = i;
	      } else {
                   *ierror = ENONAME;
              }
	      break;
	   case PXF_PASSWD:
	      SETIFMATCH(component,passwd,pw_uid)
	      else SETIFMATCH(component,passwd,pw_gid)
	      else {
		 *ierror = ENONAME;
	      }
	      break;
	   case PXF_SIGACTION:
	      SETIFMATCH(component,sigaction,sa_flags)
	      else {
		 *ierror = ENONAME;
	      }
	      break;
	   case PXF_TERMIOS:
	      SETIFMATCH(component,termios,c_iflag)
	      else SETIFMATCH(component,termios,c_oflag)
	      else SETIFMATCH(component,termios,c_cflag)
	      else SETIFMATCH(component,termios,c_lflag)
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
pxfintget_(
	   _f_int *jhandle,
	   char *compnam,
	   _f_int *value,
	   _f_int *ierror,
	   _f_int compnamlen)
{
  _PXFINTGET( jhandle, _cptofcd(compnam,compnamlen),
	      value, ierror);
}
#endif
	   
/*
 * Description:
 *	PXFINTSET provides a way to set values stored in individual
 *	components of the structure.
 * Standard:
 *	Section 8.3.2 of Posix 1003.9-1992
 * Parameters:
 *	jhandle (input)	 handle for structure.
 *	compnam (input)	 Component name.
 *	value	(input)  New value of component described by jhandle and
 *			 compnam.
 *	ierror	(output) Error code.
 */
#ifdef _UNICOS
void
PXFINTSET(
#else
void
_PXFINTSET(
#endif
	   _f_int *jhandle,
	   _fcd compnam,
	   _f_int *value,
	   _f_int *ierror)
{
	char *component;
	struct pxfhandle pxfhand;

	*ierror = 0;
	component = _fc_acopy(compnam);
	if (component == NULL) {
	         *ierror = ENOMEM;
		return;
	}

	pxfhand = _pxfhandle_table_lookup(&_pxfhandle_table, *jhandle);
	if (pxfhand.pxfstructptr == NULL) {
	  *ierror = EBADHANDLE;
	  return;
	}

	switch (pxfhand.pxftype)	{
	   case PXF_FLOCK:

	      GETIFMATCH(component,flock,l_type)
	      else GETIFMATCH(component,flock,l_whence)
	      else GETIFMATCH(component,flock,l_start)
	      else GETIFMATCH(component,flock,l_len)
#ifndef	_LITTLE_ENDIAN
	      else GETIFMATCH(component,flock,l_sysid)
#endif
	      else GETIFMATCH(component,flock,l_pid)
	      else {
	         *ierror = ENONAME;
	      }
	      break;
	   case PXF_UTIMBUF:
	      GETIFMATCH(component,utimbuf,actime)
	      else GETIFMATCH(component,utimbuf,modtime)
	      else {
	         *ierror = ENONAME;
	      }
	      break;
	   case PXF_SIGACTION:
	      GETIFMATCH(component,sigaction,sa_flags)
	      else {
		 *ierror = ENONAME;
	      }
	      break;
	   case PXF_TERMIOS:
	      GETIFMATCH(component,termios,c_iflag)
	      else GETIFMATCH(component,termios,c_oflag)
	      else GETIFMATCH(component,termios,c_cflag)
	      else GETIFMATCH(component,termios,c_lflag)
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
pxfintset_(
	   _f_int *jhandle,
	   char *compnam,
	   _f_int *value,
	   _f_int *ierror,
	   _f_int compnamlen)
{
  _PXFINTSET( jhandle, _cptofcd(compnam,compnamlen),
	      value, ierror);
}

void
pxfintset64_(
	   _f_int8 *jhandle,
	   char *compnam,
	   _f_int8 *value,
	   _f_int8 *ierror,
	   _f_int compnamlen)
{
	_f_int	jhandledef;
	_f_int	valuedef;
	_f_int	ierrordef;
	jhandledef	= *jhandle;
	valuedef	= *value;
	ierrordef	= *ierror;
	_PXFINTSET( &jhandledef, _cptofcd(compnam,compnamlen),
	      &valuedef, &ierrordef);
}

void
pxfintset32_(
	   _f_int4 *jhandle,
	   char *compnam,
	   _f_int4 *value,
	   _f_int4 *ierror,
	   _f_int compnamlen)
{
	_PXFINTSET( jhandle, _cptofcd(compnam,compnamlen),
	      value, ierror);
}
#endif

#ifdef	_CRAYMPP
void
_PXFINTSET32(
	   _f_int4 *jhandle,
	   _fcd compnam,
	   _f_int4 *value,
	   _f_int4 *ierror)
{
	_f_int	jhandle8;
	_f_int	value8;
	_f_int	ierror8;
	jhandle8	= *jhandle;
	value8	= *value;
	ierror8	= *ierror;
	_PXFINTSET( &jhandle8, compnam, &value8, &ierror8);
}
#endif

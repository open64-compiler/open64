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


#pragma ident "@(#) libf/pxf/pxfestrget.c	92.1	06/29/99 11:36:06"


#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <malloc.h>
#include <string.h>
#include <grp.h>
#include "pxfstruct.h"
#include "table.h"

/* includes bounds checking */
#define GETIFMATCH(_A_,_B_,_C_) if (strcmp(_A_,#_C_) == 0) {\
                 for(i=0; i <= cindex && ((char **)((struct _B_ *)(pxfhand.pxfstructptr))->_C_)[i] != NULL; i++);\
		 i--;\
		 if(cindex <= i && ((char **)((struct _B_ *)(pxfhand.pxfstructptr))->_C_)[i] != NULL) {\
                   *ILEN = strlen(((char **)((struct _B_ *)(pxfhand.pxfstructptr))->_C_)[cindex]);\
		   if (_c2fcpy(((struct _B_ *)(pxfhand.pxfstructptr))->_C_[cindex],VALUE) == -1) {\
		        *IERROR = ETRUNC;\
			strncpy(_fcdtocp(VALUE),((struct _B_ *)(pxfhand.pxfstructptr))->_C_[cindex],_fcdlen(VALUE));\
		   }\
		 } else {\
		   *IERROR = EINVAL;\
		 }\
}	

extern char *_fc_acopy(_fcd f);

/*
 *  PXFESTRGET  -- access single character string in an array
 *  (section 8.3.2 of Posix 1003.9-1992)
 *
 *  Call from Fortran:
 *
 *     SUBROUTINE PXFESTRGET(JHANDLE,COMPNAM,INDEX,VALUE,ILEN,IERROR)
 *     INTEGER JHANDLE,INDEX,ILEN,IERROR
 *     CHARACTER*(*) COMPNAM, VALUE
 *
 *  Where:
 *
 *  JHANDLE   is an input handle variable created with
 *            PXFSTRUCTCREATE.
 *
 *  COMPNAM   is an input character variable or array element
 *            containing the desired structure component name.
 *
 *  INDEX     is an input integer variable for the desired
 *            index in the array.
 *
 *  VALUE     is an output character variable or array element
 *            that will contain the string referenced by compnam,
 *            index, and jhandle.
 *
 *  ILEN      is an output integer variable for the length of the
 *            returned character string.
 *
 *  IERROR    is an output integer variable that will contain
 *            the status:
 *
 *	           zero    - PXFESTRGET was successful.
 *
 *	           nonzero - PXFESTRGET was not successful.
 *
 *            PXFESTRGET may return any of the following error
 *            values:
 *
 *
 *             ENONAME If component name is not defined for
 *                     this structure.
 *
 *             ETRUNC  If the declared length of the character
 *                     argument is insufficient to contain the
 *                     string to be returned.
 *
 *             ENOMEM  If there is insufficent memory to create
 *                     data structures needed by the routine.
 *
 *             EINVAL  If INDEX is out of range.
 *
 *             EBADHANDLE  If JHANDLE is invalid.
 *
 */


#ifdef _UNICOS
void
PXFESTRGET(
#else
void
_PXFESTRGET(
#endif
	    _f_int *JHANDLE,
	    _fcd COMPNAM,
	    _f_int *INDEX,
	    _fcd VALUE,
	    _f_int *ILEN,
	    _f_int *IERROR
)
{
  char *component;
  struct pxfhandle pxfhand;
  int cindex, i;

  cindex = *INDEX - 1;
  *IERROR = 0;

  /* make a c-style string equivalent of the FCD */
  component = _fc_acopy(COMPNAM);
  if (component == NULL) {
    *IERROR = ENOMEM;
    return;
  }

  pxfhand = _pxfhandle_table_lookup(&_pxfhandle_table, *JHANDLE);
  if (pxfhand.pxfstructptr == NULL) {
    *IERROR = EBADHANDLE;
    return;
  }

  switch (pxfhand.pxftype) {
    case PXF_GROUP:
      GETIFMATCH(component,group,gr_mem)
      else {
	*IERROR = ENONAME;
      }
      break;
    default:
      *IERROR = ENONAME;
      break;
  }
  free(component);
}

#ifndef _UNICOS
void
pxfestrget_(
	    _f_int *JHANDLE,
	    char *COMPNAM,
	    _f_int *INDEX,
	    char *VALUE,
	    _f_int *ILEN,
	    _f_int *IERROR,
	    _f_int compnamlen,
	    _f_int valuelen
)
{
  _PXFESTRGET( JHANDLE, _cptofcd(COMPNAM, compnamlen),
	       INDEX, _cptofcd(VALUE, valuelen),
	       ILEN, IERROR);
}
#endif



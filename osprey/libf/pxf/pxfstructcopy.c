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


#pragma ident "@(#) libf/pxf/pxfstructcopy.c	92.2	09/15/99 10:41:12"


#include <liberrno.h>
#include <errno.h>
#include <fortran.h>
#include <string.h>
#include <malloc.h>
#include "pxfstruct.h"
#include "pxfstructtable.h"
#include "table.h"

#ifndef _UNICOS
#include <stddef.h>
#endif

extern char *_fc_acopy(_fcd f);

#define NUMCONST sizeof(strtble)/sizeof(struct strtble)

#define COPY_STRING(_DEST_, _SRC_, _ERR_)\
       if (_SRC_ != NULL) {\
	    temp = (char *)malloc(sizeof(char)*(strlen(_SRC_)+1));\
	    if (temp != NULL) {\
	      (void)strcpy(temp,_SRC_);\
	      _DEST_ = temp;\
	      _ERR_ = 0;\
	    } else {\
	      _ERR_ = 1;\
	    }\
       }

#define COPY_STRING_STRUCT(_DEST_, _SRC_, _STRUCTTYPE_, _COMP_, _ERR_) \
       COPY_STRING(((struct _STRUCTTYPE_ *)_DEST_)->_COMP_, ((struct _STRUCTTYPE_ *)_SRC_)->_COMP_, _ERR_)

#define FREE(_A_,_B_,_C_) free(((struct _A_ *)_B_)->_C_);


/*
 *  PXFSTRUCTCOPY  -- structure copy
 *  (section 8.3.4 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFSTRUCTCOPY(STRUCTNAME,JHANDLE1,JHANDLE2,IERROR)
 *     INTEGER JHANDLE1,JHANDLE2,IERROR
 *     CHAARACTER*(*) STRUCTNAME
 *
 *  Where:
 *
 *  STRUCTNAME is an input character variable or array element
 *             containing the structure name corresponding to the
 *             two structure handles' type.
 *
 *  JHANDLE1   is an input structure handle variable to be copied.
 *
 *  JHANDLE2   is an output structure handle variable that will contain
 *             a copy of JHANDLE1's structure after PXFSTRUCTCOPY's
 *             successful execution.
 *
 *  IERROR     is an output integer variable that will contain
 *             the status:
 *
 *              zero    - PXFSTRUCTCOPY was successful.
 *
 *              nonzero - PXFSTRUCTCOPY was not successful.
 *
 *             PXFSTRUCTCOPY may return any of the following error
 *             values:
 *
 *              ENONAME If STRUCTNAME is an invalid structure name, or
 *                      if STRUCTNAME does not match the JHANDLE1 and
 *                      JHANDLE2's structure type.
 *
 *              ENOMEM  If memory is unavailable to create data
 *                      structures needed to copy a component.
 *
 *              EBADHANDLE  If JHANDLE1 or JHANDLE2 is invalid.
 */

#ifdef _UNICOS
void
PXFSTRUCTCOPY(
#else
void
_PXFSTRUCTCOPY(
#endif
	       _fcd STRUCTNAME,
	       _f_int *JHANDLE1,
	       _f_int *JHANDLE2,
	       _f_int *IERROR
)
{
  char *cstructname, *temp;
  int i, found, error, num_mem;
  struct pxfhandle pxfhand1, pxfhand2;
  void *pxfstructptr1, *pxfstructptr2;
  void *tempstructptr;

  pxfhand1 = _pxfhandle_table_lookup(&_pxfhandle_table, *JHANDLE1);
  pxfhand2 = _pxfhandle_table_lookup(&_pxfhandle_table, *JHANDLE2);
  if (pxfhand1.pxfstructptr == NULL || pxfhand2.pxfstructptr == NULL) {
    *IERROR = EBADHANDLE;
    return;
  }
  pxfstructptr1 = pxfhand1.pxfstructptr;
  pxfstructptr2 = pxfhand2.pxfstructptr;

  /* copy STRUCTNAME fcd to c-style string */
  cstructname = _fc_acopy(STRUCTNAME);
  if (cstructname == NULL) {
    *IERROR = ENOMEM;
    return;
  }

  /* check for valid STRUCTNAME */
  for (i = 0, found = 0; i < NUMCONST && !found; i++) {
    if (strcmp(cstructname,strtble[i].str) == 0) {
      found = 1;
    }
  }

  free(cstructname);

  if (found == 0 ||
      (pxfhand1.pxftype != pxfhand2.pxftype) ||
      (pxfhand1.pxftype != strtble[--i].type)) {
    *IERROR = ENONAME;
    return;
  }


  switch (pxfhand1.pxftype) {

  case PXF_FLOCK:
  case PXF_UTIMBUF:
  case PXF_UNAMBUF:
  case PXF_STATBUF:
  case PXF_TMSBUF:
    (void)memcpy(pxfstructptr2,pxfstructptr1,strtble[i].size);
    break;

  case PXF_GROUP:
    /* allocate temporary structure used until the copy has completed sucessfully */
    if ((tempstructptr = calloc(1,strtble[i].size)) != NULL) {

      /* copy structure components from pxfstructptr1 to tempstructptr */
      COPY_STRING_STRUCT(tempstructptr,pxfstructptr1,group,gr_name,error)
      if (!error) {
	COPY_STRING_STRUCT(tempstructptr,pxfstructptr1,group,gr_passwd,error)
	if (!error) {
	  ((struct group *)tempstructptr)->gr_gid = ((struct group *)pxfstructptr1)->gr_gid;
	  for(num_mem=0; ((struct group *)pxfstructptr1)->gr_mem[num_mem] != NULL; num_mem++);;
	  if ((((struct group *)tempstructptr)->gr_mem
	       = (char **)calloc(num_mem+1,sizeof(char *))) != NULL) {
	    for(i=0; i < num_mem && !error; i++) {
	      COPY_STRING(((struct group *)tempstructptr)->gr_mem[i],((struct group *)pxfstructptr1)->gr_mem[i],error)
	    }
	    if (!error) {
	      /* copy sucessful. now free the old struct */
	      FREE(group,pxfstructptr2,gr_name)
	      FREE(group,pxfstructptr2,gr_passwd)
	      if (((struct group *)pxfstructptr2)->gr_mem != NULL) {
		for(i=0; ((struct group *)pxfstructptr2)->gr_mem[i] != NULL; i++) {
		  free(((struct group *)pxfstructptr2)->gr_mem[i]);
		}
		FREE(group,pxfstructptr2,gr_mem)
	      }

	      /* replace the pxfstructptr2 struct with the temporary struct. */
	      _pxfhandle_table_replace(&_pxfhandle_table, *JHANDLE2,
				       tempstructptr, pxfhand2.pxftype);
	      break;
	    }

	    /* error copying gr_mem elements */
	    for(i--; i >= 0; i--) {
	      free(((struct group *)tempstructptr)->gr_mem[i]);
	    }
	    FREE(group,tempstructptr,gr_mem)
	  }
	}
	FREE(group,tempstructptr,gr_name)
      }
      *IERROR = ENOMEM;
      FREE(group,tempstructptr,gr_passwd)
      free(tempstructptr);
    } else {
      *IERROR = ENOMEM;
    }
    return;

  case PXF_PASSWD:
    /* allocate temporary structure used until the copy has completed sucessfully */
    if ((tempstructptr = calloc(1,strtble[i].size)) != NULL) {

      /* copy structure components */
      COPY_STRING_STRUCT(tempstructptr,pxfstructptr1,passwd,pw_name,error)
      if (!error) {
	COPY_STRING_STRUCT(tempstructptr,pxfstructptr1,passwd,pw_passwd,error)
	if (!error) {
	  ((struct passwd *)tempstructptr)->pw_uid = ((struct passwd *)pxfstructptr1)->pw_uid;
	  ((struct passwd *)tempstructptr)->pw_gid = ((struct passwd *)pxfstructptr1)->pw_gid;
#ifndef	_LITTLE_ENDIAN
	  COPY_STRING_STRUCT(tempstructptr,pxfstructptr1,passwd,pw_age,error)
	  if (!error) {
	    COPY_STRING_STRUCT(tempstructptr,pxfstructptr1,passwd,pw_comment,error)
	    if (!error) {
#endif	/* not _LITTLE_ENDIAN */
	      COPY_STRING_STRUCT(tempstructptr,pxfstructptr1,passwd,pw_gecos,error)
	      if (!error) {
		COPY_STRING_STRUCT(tempstructptr,pxfstructptr1,passwd,pw_dir,error)
		if (!error) {
		  COPY_STRING_STRUCT(tempstructptr,pxfstructptr1,passwd,pw_shell,error)
		  if (!error) {
		    /* no errors so free all of the pxfstructptr2's strings */
		    FREE(passwd,pxfstructptr2,pw_name)
		    FREE(passwd,pxfstructptr2,pw_passwd)
#ifndef	_LITTLE_ENDIAN
		    FREE(passwd,pxfstructptr2,pw_age)
		    FREE(passwd,pxfstructptr2,pw_comment)
#endif	/* not _LITTLE_ENDIAN */
		    FREE(passwd,pxfstructptr2,pw_gecos)
		    FREE(passwd,pxfstructptr2,pw_dir)
		    FREE(passwd,pxfstructptr2,pw_shell)

		    /* replace pxfstructptr2 with tempstructptr */
		      _pxfhandle_table_replace(&_pxfhandle_table, *JHANDLE2,
					       tempstructptr,
					       pxfhand2.pxftype);
		    break;
		  }
		  FREE(passwd,tempstructptr,pw_shell)
		}
		FREE(passwd,tempstructptr,pw_dir)
	      }
	      FREE(passwd,tempstructptr,pw_gecos)
#ifndef	_LITTLE_ENDIAN
	    }
	    FREE(passwd,tempstructptr,pw_comment)
	  }
	  FREE(passwd,tempstructptr,pw_age)
#endif	/* not _LITTLE_ENDIAN */
	}
	FREE(passwd,tempstructptr,pw_passwd)
      }

      *IERROR = ENOMEM;
      FREE(passwd,tempstructptr,pw_name)
      free(tempstructptr);
    } else {
      *IERROR = ENOMEM;
    }
    return;

  default:
    *IERROR = ENONAME;
    return;

  }
  *IERROR = 0;
}


#ifndef _UNICOS
void
pxfstructcopy_(
	       char *STRUCTNAME,
	       _f_int *JHANDLE1,
	       _f_int *JHANDLE2,
	       _f_int *IERROR,
	       _f_int structnamelen
)
{
  _PXFSTRUCTCOPY(_cptofcd(STRUCTNAME,structnamelen),
		 JHANDLE1, JHANDLE2, IERROR);
}
#endif




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


#pragma ident "@(#) libf/pxf/pxfgetgrnam.c	92.1	06/29/99 11:36:06"


#include <fortran.h>
#include <errno.h>
#include <liberrno.h>
#include <malloc.h>
#include <grp.h>
#include <string.h>
#include "pxfstruct.h"
#include "table.h"

#ifndef _UNICOS
#include <stddef.h>
#endif

extern char *_fc_acopy(_fcd f);

/*
 *  PXFGETGRNAM  -- get group information using group name
 *  (section 9.2.1 of Posix 1003.9-1992)
 *
 *  Fortran:
 *   SUBROUTINE PXFGETGRNAM(NAME, ILEN, JGROUP, IERROR)
 *   CHARACTER*(*) NAME
 *   INTEGER ILEN, JGROUP, IERROR
 *
 *  Where:
 *
 *  NAME   is an input character character variable or array element contain
 *         the group name for which group information is requested.
 *
 *  ILEN   is an input integer variable containing the length of NAME.
 *
 *  JGROUP is an output handle of type 'group' created
 *         with PXFSTRUCTCREATE.
 *
 *  IERROR is an output integer variable that will contain
 *          the status:
 *
 *           zero    - group information was retrieved.
 *
 *           nonzero - PXFGETGRNAM was not successful.
 *
 *          PXFGETGRNAM may return any of the following error
 *          values:
 *
 *           ENOENT If NAME contains an non-existant group
 *                  name.
 *
 *           ENOMEM If memory needed by PXFGETGRNAM could not
 *                  be allocated.
 *
 *           EINVAL If ILEN < 0 or ILEN > LEN(NAME).
 *
 */

#ifdef _UNICOS
void
PXFGETGRNAM(
#else
void
_PXFGETGRNAM(
#endif
	     _fcd NAME,
	     _f_int *ILEN,
	     _f_int *JGROUP,
	     _f_int *IERROR
)
{
  int count, i, j;
  struct group *groupsrc,   /* points to the structure returned from the call to getgrnam */
    *grouporiginal,         /* points to the structure created by PXFSTRUCTCREATE */
    grouptemp;              /* temporary group struct to be used until all memory
                               allocations are successful. Then the struct is assigned
			       to the PXFSTRUCTCREATE created group struct. */
  char *cname,
    **membersrc,
    **memberdest;
  struct pxfhandle pxfhand;

  pxfhand = _pxfhandle_table_lookup(&_pxfhandle_table, *JGROUP);
  if (pxfhand.pxfstructptr == NULL || pxfhand.pxftype != PXF_GROUP) {
    *IERROR = EBADHANDLE;
    return;
  }
  grouporiginal = (struct group *)pxfhand.pxfstructptr;

  /* check for invalid range error on ILEN. */
  if (*ILEN < 0 || *ILEN > _fcdlen(NAME)) {
    *IERROR = EINVAL;
  } else {

    if (*ILEN == 0) {
      /*
       * If length is zero, user wants trailing blanks stripped.
       * Otherwise, malloc memory and copy the string adding a
       * NULL terminator.
       */
      cname = _fc_acopy(NAME);
      if (cname == NULL) {
	*IERROR = ENOMEM;
	return;
      }

    } else {
      cname = (char *) malloc (*ILEN + 1);
      if (cname != NULL) {
        (void) memcpy(cname, _fcdtocp(NAME), *ILEN);
        cname[*ILEN] ='\0';
      } else {
        *IERROR = ENOMEM;
        return;
      }
    }

    /* make system call to getgrnam */
    if ((groupsrc = getgrnam(cname)) != NULL) {

      free(cname);

      /* copy the structure's components since it's static */
      /* component: gr_name (group name) */
      grouptemp.gr_name =
	(char *) malloc((strlen(groupsrc->gr_name)+1)*sizeof(char));
      if (grouptemp.gr_name == NULL) {
	*IERROR = ENOMEM;
	return;
      }
      (void) strcpy(grouptemp.gr_name, groupsrc->gr_name);

      /* component: gr_passwd (group password) */
      /* Note: Password component currently not supported by Posix 1003.9-1992,
       * but all target OSes support this field, so copy for any future support. */
      grouptemp.gr_passwd =
	(char *) malloc((strlen(groupsrc->gr_passwd)+1)*sizeof(char));
      if (grouptemp.gr_passwd == NULL) {
	*IERROR = ENOMEM;
	free(grouptemp.gr_name);
	return;
      }
      (void) strcpy(grouptemp.gr_passwd, groupsrc->gr_passwd);
      
      /* component: gr_gid (group id) */
      grouptemp.gr_gid = groupsrc->gr_gid;

      /* component: gr_mem (group members) */
      membersrc = groupsrc->gr_mem;
      for(count = 0; membersrc[count] != NULL; count++);
      /* allocate vector of character string pointers for user names.
       * NOTE: Using calloc so the entries are initially NULL. This is done
       * so PXFSTRUCTFREE knows which entries need to be free'd. */
      memberdest = (char **) calloc(count+1,sizeof(char *));
      if (memberdest == NULL) {
	*IERROR = ENOMEM;
	free(grouptemp.gr_name);
	free(grouptemp.gr_passwd);
	return;
      }
      for(i = 0; i < count; i++) {
	memberdest[i] = (char *) malloc((strlen(membersrc[i])+1)*sizeof(char));
	if (memberdest[i] == NULL) {
	  *IERROR = ENOMEM;
	  for(j = 0; j < i; j++) free(memberdest[j]);
	  free(grouptemp.gr_name);
	  free(grouptemp.gr_passwd);
	  free(memberdest);
	  return;
	}
	(void) strcpy(memberdest[i], membersrc[i]);
      }
      memberdest[i]= NULL;
      grouptemp.gr_mem = memberdest;  
    } else {
      *IERROR = ENOENT;
      free(cname);
      return;
    }
  }

  /* free any memory pointed to by the user's structure and copy grouptemp */
  free(grouporiginal->gr_name);
  free(grouporiginal->gr_passwd);
  if (grouporiginal->gr_mem != NULL) {
    for(i=0; grouporiginal->gr_mem[i] != NULL; i++) {
      free(grouporiginal->gr_mem[i]);
    }
    free(grouporiginal->gr_mem);
  }
  *grouporiginal = grouptemp;
}


#ifndef _UNICOS
void
pxfgetgrnam_(
	     char *NAME,
	     _f_int *ILEN,
	     _f_int *JGROUP,
	     _f_int *IERROR,
	     _f_int namelen
)
{
  _PXFGETGRNAM( _cptofcd(NAME, namelen), ILEN,
		JGROUP, IERROR);
}
#endif








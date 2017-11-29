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


static char USMID[] = "@(#) libcif/cifbinread.c	30.8	07/26/96 07:19:13";


/*
 *	_Cif_binread reads the variable portions of a binary CIF record based
 *	on the record type.
 */

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#include <memory.h>
#include <stdio.h>
#include <assert.h>

#include "cif_int.h"

extern void free(void *ptr);

/* --------------------------------------------------------------------------
 * read_strlist reads in an array of strings.
 * --------------------------------------------------------------------------
 */
static int
read_strlist (cifd, ns, sp, fd)
int cifd;					/* CIF descriptor */
int ns;						/* number of strings in array */
char ***sp;					/* address of pointer to array of string pointers */
FILE *fd;
{
	register int i;
	register char **cp;				/* pointer to array of string pointrs */
	register int mode;				/* memory management mode */
	short slen[100];					/* array of string lengths */

	if (fread ((char *)slen, sizeof(short), ns, fd) != ns) IO_ERROR;
	mode = _Cif_filetbl[cifd].mode;
	cp = *sp = (char **)_Cif_space[mode] (ns * sizeof(char *), cifd);
	if (cp == NULL)
		return (CIF_NOMEM);
	for (i = 0; i < ns; i++) {
		cp[i] = _Cif_space[mode] (slen[i]+1, cifd);
		if (cp[i] == NULL)
			return (CIF_NOMEM);
		if (fread (cp[i], sizeof(char), slen[i], fd) != slen[i]) IO_ERROR;
		(cp[i])[slen[i]] = '\0';
	}
	return (0);
}

/* --------------------------------------------------------------------------
 * _Cif_binread
 * --------------------------------------------------------------------------
 */
int
_Cif_binread
#ifdef __STDC__
(int cifd, int rtype, struct Cif_generic *cr, FILE *fd)
#else
(cifd, rtype, cr, fd)
int cifd;
int rtype;
struct Cif_generic *cr;
FILE *fd;
#endif
{

	register int i, n;
	register char *cp;
	register long *lp;
	register int mode;
	unsigned char c;

	mode = _Cif_filetbl[cifd].mode;

	switch (rtype) {

	case CIF_CALLSITE:
		if ((n = CIFCS(cr)->nargs) > 0) {
			lp= CIFCS(cr)->argids= (long *)_Cif_space[mode] (sizeof(long)*n, cifd);
			if (lp == NULL)
				return (CIF_NOMEM);
			if (fread ((char *)lp, sizeof(long), n, fd) != n) IO_ERROR;
		}
		break;

	case CIF_CIFHDR:
		CIFHDR(cr)->canpos = _Cif_filetbl[cifd].seek;
		CIFHDR(cr)->form = BINARY_CIF_FORMAT;
		break;

	case CIF_COMBLK:
		if (_Cif_filetbl[cifd].return_version == 1) {

			n = CIFCB1(cr)->nlen;
			cp = CIFCB1(cr)->name = _Cif_space[mode] (n+1, cifd);
			if (cp == NULL)
			  	return (CIF_NOMEM);
			if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
			cp[n] = '\0';
		}
		else { /* version 2 */

		  	n = CIFCB(cr)->nlen;
			cp = CIFCB(cr)->name = _Cif_space[mode] (n+1, cifd);
			if (cp == NULL)
			  	return (CIF_NOMEM);
			if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
			cp[n] = '\0';
	        }
		break;

	case CIF_CONST:
		n = CIFCON(cr)->nlen;
		cp = CIFCON(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return(CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		n = CIFCON(cr)->vlen;
		cp = CIFCON(cr)->value = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return(CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';

		if (CIFCON(cr)->origform) {
		  n = CIFCON(cr)->olen;
		  cp = CIFCON(cr)->oform = _Cif_space[mode] (n+1, cifd);

		  if (cp == NULL)
		    return(CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';
		}


		break;

	case CIF_ENTRY:
		if ((n = CIFENTRY(cr)->nargs) > 0) {
			lp = CIFENTRY(cr)->argids = (long *) _Cif_space[mode]
			                                         (sizeof(long)*n, cifd);
			if (lp == NULL)
				return (CIF_NOMEM);
			if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		}
		n = CIFENTRY(cr)->nlen;
		cp = CIFENTRY(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		break;

	case CIF_FILE:
		if (_Cif_filetbl[cifd].return_version < 3) {
		  int onlen = CIFFILE(cr)->onlen;
			/* do NOT move this line down!
			   On 32-bit architectures like sparc
			   CIFFILE(cr)->onlen and CIFFILE1(cr)->name
			   occupy the same space, so this MUST be done first */

		  n = CIFFILE1(cr)->nlen;
		  cp = CIFFILE1(cr)->name = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		      return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';


		  if (_Cif_filetbl[cifd].version == 3) {

		    /* Read and discard the original file name which exists in
		     * in a V3 CIF but not in V1 or V2 */

		    char *tmp;
		    tmp = _Cif_space[mode] (onlen +1, cifd);
		    if (tmp == NULL)
			return (CIF_NOMEM);
		    if (fread (tmp, sizeof(char), onlen, fd) != onlen) IO_ERROR;
		  }
		}
		else { /* Version 3 CIF */

		  n = CIFFILE(cr)->nlen;
		  cp = CIFFILE(cr)->name = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		      return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';

		  if (_Cif_filetbl[cifd].version == 3) {
		    n = CIFFILE(cr)->onlen;
		    if (n > 0) {
		      cp = CIFFILE(cr)->oname = _Cif_space[mode] (n+1, cifd);
		      if (cp == NULL)
			  return (CIF_NOMEM);
		      if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		      cp[n] = '\0';
		    }
		  }
		  else {
		    CIFFILE(cr)->oname = (char *) NULL;
		  }
		}
		break;

	case CIF_LABEL:
		n = CIFLABEL(cr)->nlen;
		cp = CIFLABEL(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		break;

	case CIF_ORIG_CMD:
		n = CIFOCMD(cr)->nlen;
		cp = CIFOCMD(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		break;

	case CIF_CDIR:
		n = CIFCDIR(cr)->nids;
		lp = CIFCDIR(cr)->ids = (long *)_Cif_space[mode] (sizeof(long)*n, cifd);
		if (lp == NULL)
			return (CIF_NOMEM);
		if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		break;

	case CIF_CDIR_DOSHARED:
		n = CIFCDIRDO(cr)->nids;
		lp = CIFCDIRDO(cr)->ids = (long *)_Cif_space[mode] (sizeof(long)*n, cifd);
		if (lp == NULL)
			return (CIF_NOMEM);
		if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		break;

        case CIF_GEOMETRY:
		if ((n = CIFGEOM(cr)->ndims) > 0) {
			CIFGEOM(cr)->dim =
				(struct Cif_geometry_dim *)_Cif_space[mode](sizeof(struct Cif_geometry_dim)*n, cifd);
			if (CIFGEOM(cr)->dim == NULL)
				return (CIF_NOMEM);
			if (fread ((char *)CIFGEOM(cr)->dim, GEOM_SSIZE, n, fd) != n) IO_ERROR;
		}
		n = CIFGEOM(cr)->nlen;
		if (n > 0) {
		  	cp = CIFGEOM(cr)->name = _Cif_space[mode] (n+1, cifd);
			if (cp == NULL)
			  	return (CIF_NOMEM);
			if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
			cp[n] = '\0';
		}
		break;

	case CIF_MESSAGE:

		if (_Cif_filetbl[cifd].return_version < 3) {

		  if (_Cif_filetbl[cifd].version == 3) {

		    /* Read and discard the scoping unit name which exists
		     * in a V3 CIF but not in V1 or V2 */

		    char *tmp;
		    n = CIFMSG(cr)->nlen;
		    tmp = _Cif_space[mode] (n +1, cifd);
		    if (tmp == NULL)
			return (CIF_NOMEM);
		    if (fread (tmp, sizeof(char), n, fd) != n) IO_ERROR;
		  }

		  if (CIFMSG1(cr)->nargs > 0) {
		    if ((i= read_strlist (cifd,
					  (int) CIFMSG1(cr)->nargs,
					  &CIFMSG1(cr)->args, fd)) < 0)
			return (i);
		  }
		}
		else { /* Version 3 CIF */
		  if (_Cif_filetbl[cifd].version == 3) {
		    n = CIFMSG(cr)->nlen;
		    if (n > 0) {
		      cp = CIFMSG(cr)->name = _Cif_space[mode] (n+1, cifd);
		      if (cp == NULL)
			  return (CIF_NOMEM);
		      if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		      cp[n] = '\0';
		    }
		  }
		  else {
		    CIFMSG(cr)->name = (char *) NULL;
		  }
		  if (CIFMSG(cr)->nargs > 0) {
		    if ((i= read_strlist (cifd,
					  (int) CIFMSG(cr)->nargs,
					  &CIFMSG(cr)->args, fd)) < 0)
			return (i);
		  }
		}

		break;

	case CIF_MISC_OPTS:

		if (_Cif_filetbl[cifd].return_version == 1) {

		  if ((n = CIFMO1(cr)->nmsgs) > 0) {
		    lp= CIFMO1(cr)->msgno= (long *) _Cif_space[mode] (sizeof(long)*n, cifd);
		    if (lp == NULL)
		      return (CIF_NOMEM);
		    if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		  }
		  if (CIFMO1(cr)->ncdirs > 0) {
		    if ((i= read_strlist (cifd, (int) CIFMO1(cr)->ncdirs, &CIFMO1(cr)->cdirs, fd)) < 0)
		      return (i);
		  }
		  n = CIFMO1(cr)->onlen;
		  cp = CIFMO1(cr)->objname = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		    return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';
		  n = CIFMO1(cr)->cnlen;
		  cp = CIFMO1(cr)->calname = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		    return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';
		  n = CIFMO1(cr)->inlen;
		  cp = CIFMO1(cr)->inname = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		    return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';
		}
		else {  /* a v2 cif */

		  if ((n = CIFMO(cr)->nmsgs) > 0) {
		    lp= CIFMO(cr)->msgno= (long *) _Cif_space[mode] (sizeof(long)*n, cifd);
		    if (lp == NULL)
		      return (CIF_NOMEM);
		    if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		  }
		  if (CIFMO(cr)->ncdirs > 0) {
		    if ((i= read_strlist (cifd, (int) CIFMO(cr)->ncdirs, &CIFMO(cr)->cdirs, fd)) < 0)
		      return (i);
		  }
		  n = CIFMO(cr)->onlen;
		  cp = CIFMO(cr)->objname = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		    return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';
		  n = CIFMO(cr)->cnlen;
		  cp = CIFMO(cr)->calname = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		    return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';
		  n = CIFMO(cr)->inlen;
		  cp = CIFMO(cr)->inname = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		    return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';
		  n = CIFMO(cr)->llen;
		  cp = CIFMO(cr)->lname = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		    return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';

		  if (CIFMO(cr)->numincs > 0) {
		    if ((i= read_strlist (cifd, (int) CIFMO(cr)->numincs, &CIFMO(cr)->incdirs, fd)) < 0)
		      return (i);
		  }

		}

		break;
			
	case CIF_NAMELIST:
		n = CIFNL(cr)->nlen;
		cp = CIFNL(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		n = CIFNL(cr)->nids;
		lp = CIFNL(cr)->ids = (long *)_Cif_space[mode] (sizeof(long)*n, cifd);
		if (lp == NULL)
			return (CIF_NOMEM);
		if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		break;

	case CIF_ND_MSG:
		if (CIFNMSG(cr)->nargs > 0) {
			if ((i = read_strlist (cifd, (int) CIFNMSG(cr)->nargs, &CIFNMSG(cr)->args, fd))
				< 0) return (i);
		}
		break;

	case CIF_OBJECT:
		if (_Cif_filetbl[cifd].return_version == 1) {

		if ((n = CIFOBJ1(cr)->nlen) > 0) {
			cp = CIFOBJ1(cr)->name = _Cif_space[mode] (n+1, cifd);
			if (cp == NULL)
				return (CIF_NOMEM);
			if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
			cp[n] = '\0';
		}
		if ((n = CIFOBJ1(cr)->ndims) > 0) {
			CIFOBJ1(cr)->dim =
				(struct Cif_dim *)_Cif_space[mode] (sizeof(struct Cif_dim)*n, cifd);
			if (CIFOBJ1(cr)->dim == NULL)
				return (CIF_NOMEM);
			if (fread ((char *)CIFOBJ1(cr)->dim, DIM_SSIZE, n, fd) != n) IO_ERROR;
		}
	      }
		else {
		if ((n = CIFOBJ(cr)->nlen) > 0) {
			cp = CIFOBJ(cr)->name = _Cif_space[mode] (n+1, cifd);
			if (cp == NULL)
				return (CIF_NOMEM);
			if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
			cp[n] = '\0';
		}
		if ((n = CIFOBJ(cr)->ndims) > 0) {
			CIFOBJ(cr)->dim =
				(struct Cif_dim *)_Cif_space[mode] (sizeof(struct Cif_dim)*n, cifd);
			if (CIFOBJ(cr)->dim == NULL)
				return (CIF_NOMEM);
			if (fread ((char *)CIFOBJ(cr)->dim, DIM_SSIZE, n, fd) != n) IO_ERROR;
		}
	      }
		break;

	case CIF_UNIT:
		n = CIFUNIT(cr)->nlen;
		cp = CIFUNIT(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		break;

	case CIF_ENDUNIT:
		n = CIFENDU(cr)->nlen;
		cp = CIFENDU(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		break;

	case CIF_USAGE:
		if (_Cif_filetbl[cifd].return_version == 1) {

		  n = CIFUSAGE1(cr)->nuses;
		  CIFUSAGE1(cr)->use =
		    (struct Cif_use *) _Cif_space[mode] (sizeof(struct Cif_use_1)*n, cifd);
		  if (CIFUSAGE1(cr)->use == NULL)
		    return (CIF_NOMEM);
		  if (fread ((char *)CIFUSAGE1(cr)->use, sizeof(struct Cif_use_1), n, fd)
		      != n) IO_ERROR;

		}
		else { /* a v2 cif */

		  n = CIFUSAGE(cr)->nuses;
		  CIFUSAGE(cr)->use =
		    (struct Cif_use *) _Cif_space[mode] (sizeof(struct Cif_use)*n, cifd);
		  if (CIFUSAGE(cr)->use == NULL)
		    return (CIF_NOMEM);
		  if (fread ((char *)CIFUSAGE(cr)->use, sizeof(struct Cif_use), n, fd)
		      != n) IO_ERROR;

		  if (CIFUSAGE(cr)->nmembs > 0) {

		    n = CIFUSAGE(cr)->nmembs;
		    CIFUSAGE(cr)->membs = (long *) _Cif_space[mode] (sizeof(long)*n, cifd);
		    if (CIFUSAGE(cr)->membs == NULL)
		      return (CIF_NOMEM);
		    if (fread ((char *)CIFUSAGE(cr)->membs, sizeof(long), n, fd)
			!= n) IO_ERROR;
		  }
		}
		break;

	case CIF_FILEDIR:
	{
		struct Cif_unittbl *ut;

		n = CIFFDIR(cr)->nunits;
		ut = CIFFDIR(cr)->ut = (struct Cif_unittbl *) _Cif_space[mode]
		                                     (sizeof(struct Cif_unittbl)*n, cifd);
		if (ut == NULL)
			return (CIF_NOMEM);
		(void) memset((char *)ut, 0, n * sizeof(struct Cif_unittbl));
		for (i = 0; i < n; i++, ut++) {
			if (fread ((char *)ut, UNITTBL_SSIZE, 1, fd) != 1) IO_ERROR;
			cp = ut->name = _Cif_space[mode] (ut->nlen+1, cifd);
			if (cp == NULL)
				return (CIF_NOMEM);
			if (fread (cp, sizeof(char), ut->nlen, fd) != ut->nlen) IO_ERROR;
			cp[ut->nlen] = '\0';
		}
		break;
	}

	case CIF_UNITDIR:
	{
		struct Cif_urectbl ur[CIF_MAXRECORD];
		struct Cif_urectbl *urp;

		n = sizeof(struct Cif_urectbl) * CIF_MAXRECORD;
		urp = CIFUDIR(cr)->ur = (struct Cif_urectbl *) _Cif_space[mode] (n, cifd);
		if (urp == NULL)
			return(CIF_NOMEM);
		(void) memset ((char *)urp, '\0', n);
		n = CIFUDIR(cr)->nsections;
		if (fread ((char *)ur, sizeof(struct Cif_urectbl), n, fd) != n)
			IO_ERROR;
		for (i = 0; i < n; i++) {
		  urp[ur[i].rectype] = ur[i];

		  /* mask out those records that this cif version (eg v1) doesn't know about */

		  if (_Cif_shortsize[ur[i].rectype][_Cif_filetbl[cifd].return_version] == 0)
		    urp[ur[i].rectype].nrecords = 0;
		}

		/* Only returns the correct number of entries for the version requested */
		if (_Cif_filetbl[cifd].return_version == 1)
		  CIFUDIR(cr)->nsections = CIF_MAXRECORD_1;
		else
		  CIFUDIR(cr)->nsections = CIF_MAXRECORD;

		break;
	}

	case CIF_C_TAG:
	{
		struct Cif_tmod *tm;

		n = CIFCTAG(cr)->nmods;
		tm = CIFCTAG(cr)->mods =
			(struct Cif_tmod *) _Cif_space[mode] (sizeof(struct Cif_tmod)*n, cifd);
		if (tm == NULL)
			return (CIF_NOMEM);
		for (i = 0; i < n; i++)
			if (fread ((char *)&(tm[i]), TMOD_SSIZE, 1, fd) != 1) IO_ERROR;
		n = CIFCTAG(cr)-> nmems;
		CIFCTAG(cr)->memids = (long *) _Cif_space[mode] (sizeof(long)*n, cifd);
		if (CIFCTAG(cr)->memids == NULL)
			return (CIF_NOMEM);
		if (fread ((char *)CIFCTAG(cr)->memids, sizeof(long), n, fd) != n) 
			IO_ERROR;
		n = CIFCTAG(cr)->nlen;
		CIFCTAG(cr)->name = _Cif_space[mode](n+1, cifd);
		if (CIFCTAG(cr)->name == NULL)
			return (CIF_NOMEM);
		if (fread ((char *)CIFCTAG(cr)->name, sizeof(char), n, fd) != n)
			IO_ERROR;
		(CIFCTAG(cr)->name)[n] = '\0';
		break;
	}

	case CIF_C_OPTS:
		n = CIFCOPTS(cr)->nlen;
		CIFCOPTS(cr)->name = _Cif_space[mode](n+1, cifd);
		if (CIFCOPTS(cr)->name == NULL)
			return (CIF_NOMEM);
		if (fread ((char *)CIFCOPTS(cr)->name, sizeof(char), n, fd) != n)
			IO_ERROR;
		(CIFCOPTS(cr)->name)[n] = '\0';
		if (CIFCOPTS(cr)->nincs > 0) {
			if ((i = read_strlist (cifd, (int) CIFCOPTS(cr)->nincs, &CIFCOPTS(cr)->incs, fd))
				< 0) return (i);
		}
		if (CIFCOPTS(cr)->ndefs > 0) {
			if ((i = read_strlist (cifd, (int) CIFCOPTS(cr)->ndefs, &CIFCOPTS(cr)->defs, fd))
				< 0) return (i);
		}
		if (CIFCOPTS(cr)->nudefs > 0) {
			if ((i = read_strlist (cifd, (int) CIFCOPTS(cr)->nudefs, &CIFCOPTS(cr)->udefs, fd))
				< 0) return (i);
		}
		break;

	case CIF_C_MESSAGE:
		if (_Cif_filetbl[cifd].return_version == 1) {
		  if (CIFCMSG1(cr)->nargs > 0) {
		    if ((i = read_strlist (cifd, (int) CIFCMSG(cr)->nargs, &CIFCMSG(cr)->args, fd)) < 0)
		      return (i);
		  }
		}
		else {
		  if (CIFCMSG(cr)->nargs > 0) {
		    if ((i = read_strlist (cifd, (int) CIFCMSG(cr)->nargs, &CIFCMSG(cr)->args, fd)) < 0)
		      return (i);
		  }
		}
		break;

	case CIF_C_CONST:
		n = CIFCCON(cr)->vlen;
		CIFCCON(cr)->value = _Cif_space[mode](n+1, cifd);
		if (CIFCCON(cr)->value == NULL)
			return (CIF_NOMEM);
		if (fread ((char *)CIFCCON(cr)->value, sizeof(char), n, fd) != n)
			IO_ERROR;
		(CIFCCON(cr)->value)[n] = '\0';
		break;

	case CIF_C_ENTRY:
	{
		struct Cif_tmod *tm;

		if (_Cif_filetbl[cifd].return_version == 1) {

		  n = CIFCENTRY1(cr)->nmods;
		  tm = CIFCENTRY1(cr)->mods =
		    (struct Cif_tmod *) _Cif_space[mode] (sizeof(struct Cif_tmod)*n, cifd);
		  if (tm == NULL)
		    return (CIF_NOMEM);
		  for (i = 0; i < n; i++)
		    if (fread ((char *)&(tm[i]), TMOD_SSIZE, 1, fd) != 1) IO_ERROR;

		  n = CIFCENTRY1(cr)->nargs;
		  CIFCENTRY1(cr)->argids = (long *) _Cif_space[mode](sizeof(long)*n, cifd);
		  if (CIFCENTRY1(cr)->argids == NULL)
		    return (CIF_NOMEM);
		  if (fread ((char *)CIFCENTRY1(cr)->argids, sizeof(long), n, fd) != n)
		    IO_ERROR;

		  n = CIFCENTRY1(cr)->nlen;
		  CIFCENTRY1(cr)->name = _Cif_space[mode](n+1, cifd);
		  if (CIFCENTRY1(cr)->name == NULL)
		    return (CIF_NOMEM);
		  if (fread ((char *)CIFCENTRY1(cr)->name, sizeof(char), n, fd) != n)
		    IO_ERROR;
		  (CIFCENTRY1(cr)->name)[n] = '\0';
		}
		else {

		  n = CIFCENTRY(cr)->nmods;
		  tm = CIFCENTRY(cr)->mods =
		    (struct Cif_tmod *) _Cif_space[mode] (sizeof(struct Cif_tmod)*n, cifd);
		  if (tm == NULL)
		    return (CIF_NOMEM);
		  for (i = 0; i < n; i++)
		    if (fread ((char *)&(tm[i]), TMOD_SSIZE, 1, fd) != 1) IO_ERROR;

		  n = CIFCENTRY(cr)->nargs;
		  CIFCENTRY(cr)->argids = (long *) _Cif_space[mode](sizeof(long)*n, cifd);
		  if (CIFCENTRY(cr)->argids == NULL)
		    return (CIF_NOMEM);
		  if (fread ((char *)CIFCENTRY(cr)->argids, sizeof(long), n, fd) != n)
		    IO_ERROR;

		  n = CIFCENTRY(cr)->nlen;
		  CIFCENTRY(cr)->name = _Cif_space[mode](n+1, cifd);
		  if (CIFCENTRY(cr)->name == NULL)
		    return (CIF_NOMEM);
		  if (fread ((char *)CIFCENTRY(cr)->name, sizeof(char), n, fd) != n)
		    IO_ERROR;
		  (CIFCENTRY(cr)->name)[n] = '\0';
		}

		break;
	}

	case CIF_C_OBJECT:
	{
	  	struct Cif_tmod *tm;

		n = CIFCOBJ(cr)->nmods;
		tm = CIFCOBJ(cr)->mods =
		  	(struct Cif_tmod *) _Cif_space[mode] (sizeof(struct Cif_tmod)*n, cifd);
		if (tm == NULL)
		  	return (CIF_NOMEM);
		for (i = 0; i < n; i++)
		  	if (fread ((char *)&(tm[i]), TMOD_SSIZE, 1, fd) != 1) IO_ERROR;
		if ((n = CIFCOBJ(cr)->nlen) > 0) {
		  	CIFCOBJ(cr)->name = _Cif_space[mode](n+1, cifd);
			if (CIFCOBJ(cr)->name == NULL)
			  	return (CIF_NOMEM);
			if (fread ((char *)CIFCOBJ(cr)->name, sizeof(char), n, fd) != n)
		    		IO_ERROR;
			(CIFCOBJ(cr)->name)[n] = '\0';
		}

		break;
	}

	case CIF_C_LINT_DIRECTIVE:
	{
		if ((n = CIFCLDIR(cr)->nlen) > 0) {
			CIFCLDIR(cr)->name = _Cif_space[mode](n+1, cifd);
			if (CIFCLDIR(cr)->name == NULL)
				return (CIF_NOMEM);
			if (fread ((char *)CIFCLDIR(cr)->name, sizeof(char), n, fd) != n)
				IO_ERROR;
			(CIFCLDIR(cr)->name)[n] = '\0';
		}

		break;
	}

	case CIF_C_MACRO_DEF:
	{
		if ((n = CIFCMDEF(cr)->nlen) > 0) {
			CIFCMDEF(cr)->name = _Cif_space[mode](n+1, cifd);
			if (CIFCMDEF(cr)->name == NULL)
				return (CIF_NOMEM);
			if (fread ((char *)CIFCMDEF(cr)->name, sizeof(char), n, fd) != n)
				IO_ERROR;
			(CIFCMDEF(cr)->name)[n] = '\0';
		}

		break;
	}

	case CIF_C_ENTRY_END:
	{
		if ((n = CIFCEEND(cr)->nlen) > 0) {
			CIFCEEND(cr)->name = _Cif_space[mode](n+1, cifd);
			if (CIFCEEND(cr)->name == NULL)
				return (CIF_NOMEM);
			if (fread ((char *)CIFCEEND(cr)->name, sizeof(char), n, fd) != n)
				IO_ERROR;
			(CIFCEEND(cr)->name)[n] = '\0';
		}

		break;
	}

#ifndef CRAY2
	case CIF_F90_CALLSITE:
		if ((n = CIFF90CS(cr)->nargs) > 0) {
			lp= CIFF90CS(cr)->argids= (long *)_Cif_space[mode] (sizeof(long)*n, cifd);
			if (lp == NULL)
				return (CIF_NOMEM);
			if (fread ((char *)lp, sizeof(long), n, fd) != n) IO_ERROR;

			lp= (long *) (CIFF90CS(cr)->nmembs=
				      (int *)_Cif_space[mode] (sizeof(int)*n, cifd));
			if (lp == NULL)
				return (CIF_NOMEM);
			if (fread ((char *)lp, sizeof(int), n, fd) != n) IO_ERROR;
			lp= (long *) (CIFF90CS(cr)->membs=
				      (long **)_Cif_space[mode] (sizeof(long *)*n, cifd));
			if (lp == NULL)
			    return (CIF_NOMEM);

			for (i = 0; i < (int) CIFF90CS(cr)->nargs; i++) {
			    if (CIFF90CS(cr)->nmembs[i] > 0) {
				lp= (long *) (CIFF90CS(cr)->membs[i]=
					      (long *)_Cif_space[mode] (sizeof(long)*CIFF90CS(cr)->nmembs[i], cifd));
				if (lp == NULL)
				    return (CIF_NOMEM);
				if (fread ((char *)lp, sizeof(long), CIFF90CS(cr)->nmembs[i], fd) != CIFF90CS(cr)->nmembs[i]) IO_ERROR;
			    }
			}

			if (CIFF90CS(cr)->rank == 1) {

			    lp= (long *) (CIFF90CS(cr)->ranks = (int *)_Cif_space[mode] (sizeof(int)*n, cifd));
			    if (lp == NULL)
				return (CIF_NOMEM);
			    if (fread ((char *)lp, sizeof(int), n, fd) != n) IO_ERROR;
			}


		    }
		break;

	case CIF_F90_SCOPE_INFO:
		if ((n = CIFF90SI(cr)->numalts) > 0) {
			lp= CIFF90SI(cr)->entryids= (long *)_Cif_space[mode] (sizeof(long)*n, cifd);
			if (lp == NULL)
				return (CIF_NOMEM);
			if (fread ((char *)lp, sizeof(long), n, fd) != n) IO_ERROR;
		}
		break;

	case CIF_F90_COMBLK:
		n = CIFF90CB(cr)->nlen;
		cp = CIFF90CB(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		break;

	case CIF_F90_CONST:
		n = CIFF90CON(cr)->vlen;
		cp = CIFF90CON(cr)->value = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return(CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		break;

	case CIF_F90_ENTRY:

		if ((n = CIFF90ENTRY(cr)->nargs) > 0) {
			lp = CIFF90ENTRY(cr)->argids = (long *) _Cif_space[mode]
			  					(sizeof(long)*n, cifd);
			if (lp == NULL)
				return (CIF_NOMEM);
			if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		}
		n = CIFF90ENTRY(cr)->nlen;
		cp = CIFF90ENTRY(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		break;

	case CIF_F90_DERIVED_TYPE:
		if (_Cif_filetbl[cifd].return_version == 2) {

		  n = CIFF90DTYPE2(cr)->nlen;
		  cp = CIFF90DTYPE2(cr)->name = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		      return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';

		  if ((n = CIFF90DTYPE2(cr)->nmembs) > 0) {
		    lp = CIFF90DTYPE2(cr)->memids = (long *) _Cif_space[mode]
			(sizeof(long)*n, cifd);
		    if (lp == NULL)
		    	return (CIF_NOMEM);
		    if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		  }
		}
		else {
		  n = CIFF90DTYPE(cr)->nlen;
		  cp = CIFF90DTYPE(cr)->name = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		      return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';

		  if ((n = CIFF90DTYPE(cr)->nmembs) > 0) {
		    lp = CIFF90DTYPE(cr)->memids = (long *) _Cif_space[mode]
			(sizeof(long)*n, cifd);
		    if (lp == NULL)
		    	return (CIF_NOMEM);
		    if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		  }
		}
		break;

	case CIF_F90_LABEL:
		n = CIFF90LABEL(cr)->nlen;
		cp = CIFF90LABEL(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		break;

	case CIF_F90_NAMELIST:
		n = CIFF90NL(cr)->nlen;
		cp = CIFF90NL(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		n = CIFF90NL(cr)->nids;
		lp = CIFF90NL(cr)->ids = (long *)_Cif_space[mode] (sizeof(long)*n, cifd);
		if (lp == NULL)
			return (CIF_NOMEM);
		if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		break;

	case CIF_F90_OBJECT:
		if ((n = CIFF90OBJ(cr)->nlen) > 0) {
			cp = CIFF90OBJ(cr)->name = _Cif_space[mode] (n+1, cifd);
			if (cp == NULL)
				return (CIF_NOMEM);
			if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
			cp[n] = '\0';
		}
		if ((n = CIFF90OBJ(cr)->ndims) > 0 &&
		    CIFF90OBJ(cr)->atype != CIF_AT_DEFERRED) { /* deferred arrays have all
								* dimensions assumed to be ':'
								*/
			CIFF90OBJ(cr)->dim =
				(struct Cif_dim *)_Cif_space[mode] (sizeof(struct Cif_dim)*n, cifd);
			if (CIFF90OBJ(cr)->dim == NULL)
				return (CIF_NOMEM);
			if (fread ((char *)CIFF90OBJ(cr)->dim, DIM_SSIZE, n, fd) != n) IO_ERROR;
		}
		break;


   case CIF_F90_MISC_OPTS:
		n = CIFF90MO(cr)->ciflen;
		cp = CIFF90MO(cr)->cifname = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';

		if (CIFF90MO(cr)->nPdirs > 0) {
			if ((i= read_strlist (cifd, (int) CIFF90MO(cr)->nPdirs, &CIFF90MO(cr)->Pdirs, fd)) < 0)
				return (i);
		}
		if (CIFF90MO(cr)->npdirs > 0) {
			if ((i= read_strlist (cifd, (int) CIFF90MO(cr)->npdirs, &CIFF90MO(cr)->pdirs, fd)) < 0)
				return (i);
		}
		if ((n = CIFF90MO(cr)->nmsgs) > 0) {
			lp= CIFF90MO(cr)->msgno= (long *) _Cif_space[mode] (sizeof(long)*n, cifd);
			if (lp == NULL)
				return (CIF_NOMEM);
			if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		}
		if (CIFF90MO(cr)->ncdirs > 0) {
			if ((i= read_strlist (cifd, (int) CIFF90MO(cr)->ncdirs, &CIFF90MO(cr)->cdirs, fd)) < 0)
				return (i);
		}
		n = CIFF90MO(cr)->onlen;
		cp = CIFF90MO(cr)->objname = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		n = CIFF90MO(cr)->cnlen;
		cp = CIFF90MO(cr)->calname = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		n = CIFF90MO(cr)->inlen;
		cp = CIFF90MO(cr)->inname = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';
		break;

	case CIF_F90_OPT_OPTS:

		if ((n = CIFF90OPTOPT(cr)->noptlevels) > 0) {
			CIFF90OPTOPT(cr)->lopts =
				(struct Cif_f90_level_opts *)_Cif_space[mode] (sizeof(struct Cif_f90_level_opts)*n, cifd);
			if (CIFF90OPTOPT(cr)->lopts == NULL)
				return (CIF_NOMEM);
			if (CIFF90OPTOPT(cr)->newdef == 1) {
			    if (fread ((char *)CIFF90OPTOPT(cr)->lopts, OPTS_SSIZE, n, fd) != n) IO_ERROR;
			}
			else {
			    if (fread ((char *)CIFF90OPTOPT(cr)->lopts, 4, n, fd) != n) IO_ERROR;
			}
		}
		break;

     	case CIF_F90_INT_BLOCK:
		if (_Cif_filetbl[cifd].return_version == 2) {

		  n = CIFF90IB2(cr)->nlen;
		  cp = CIFF90IB2(cr)->name = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		      return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';

		  if ((n = CIFF90IB2(cr)->numints) > 0) {
		    lp= CIFF90IB2(cr)->procids= (long *) _Cif_space[mode] (sizeof(long)*n, cifd);
		    if (lp == NULL)
			return (CIF_NOMEM);
		    if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		  }
		}
		else { /* Version 3 or beyond */

		  n = CIFF90IB(cr)->nlen;
		  cp = CIFF90IB(cr)->name = _Cif_space[mode] (n+1, cifd);
		  if (cp == NULL)
		      return (CIF_NOMEM);
		  if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		  cp[n] = '\0';

		  if ((n = CIFF90IB(cr)->numints) > 0) {
		    lp= CIFF90IB(cr)->procids= (long *) _Cif_space[mode] (sizeof(long)*n, cifd);
		    if (lp == NULL)
			return (CIF_NOMEM);
		    if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		  }
		}
		break;

     	case CIF_F90_RENAME:

		n = CIFF90RN(cr)->nlen;
		cp = CIFF90RN(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';

		n = CIFF90RN(cr)->orignlen;
		cp = CIFF90RN(cr)->origname = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n) IO_ERROR;
		cp[n] = '\0';

		if ((n = CIFF90RN(cr)->nlocalids) > 0) {
			lp= CIFF90RN(cr)->localid = (long *) _Cif_space[mode] (sizeof(long)*n, cifd);
			if (lp == NULL)
				return (CIF_NOMEM);
			if (fread (lp, sizeof(long), n, fd) != n) IO_ERROR;
		}
		break;

	case CIF_CC_TYPE:
		n = CIFCCTYPE(cr)->nlen;
		cp = CIFCCTYPE(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n)
		    IO_ERROR;
		cp[n] = '\0';

		if ( (n = CIFCCTYPE(cr)->nmem) > 0 ) {
		    cp = _Cif_space[ mode ]( n * sizeof( int ), cifd );
		    if ( cp == NULL )
			return( CIF_NOMEM );
		    CIFCCTYPE(cr)->mem = (int *) cp;
		    if ( (int) fread( cp, sizeof( int ), n, fd ) < n )
			IO_ERROR;
		}
		break;

	case CIF_CC_ENTRY:
		n = CIFCCENT(cr)->nlen;
		cp = CIFCCENT(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n)
		    IO_ERROR;
		cp[n] = '\0';

		n = CIFCCENT(cr)->elen;
		cp = CIFCCENT(cr)->ename = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n)
		    IO_ERROR;
		cp[n] = '\0';

		if ( (n = CIFCCENT(cr)->nparam) > 0 ) {
		    cp = _Cif_space[ mode ]( n * sizeof( int ), cifd );
		    if ( cp == NULL )
			return( CIF_NOMEM );
		    CIFCCENT(cr)->param = (int *) cp;
		    if ( (int) fread( cp, sizeof( int ), n, fd ) < n )
			IO_ERROR;
		}
		break;

	case CIF_CC_OBJ:
		n = CIFCCOBJ(cr)->nlen;
		cp = CIFCCOBJ(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n)
		    IO_ERROR;
		cp[n] = '\0';
		break;

	case CIF_CC_ENUM:
		n = CIFCCENUM(cr)->nlen;
		cp = CIFCCENUM(cr)->name = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n)
		    IO_ERROR;
		cp[n] = '\0';
		n = CIFCCENUM(cr)->vlen;
		cp = CIFCCENUM(cr)->value = _Cif_space[mode] (n+1, cifd);
		if (cp == NULL)
			return (CIF_NOMEM);
		if (fread (cp, sizeof(char), n, fd) != n)
		    IO_ERROR;
		cp[n] = '\0';
		break;

	case CIF_CC_EXPR:
		if ( (n = CIFCCEXPR(cr)->noper) > 0 ) {
		    cp = _Cif_space[ mode ]( n * sizeof( int ), cifd );
		    if ( cp == NULL )
			return( CIF_NOMEM );
		    CIFCCEXPR(cr)->oper = (int *) cp;
		    if ( (int) fread( cp, sizeof( int ), n, fd ) < n )
			IO_ERROR;
		}
		break;

	case CIF_BE_NODE:
		if ( _Cif_filetbl[cifd].return_version == 2 ) {
		    if ( (n = CIFBENODE2(cr)->nsuccs) > 0 ) {
			cp = _Cif_space[ mode ]( n * sizeof( int ), cifd );
			if ( cp == NULL )
			    return( CIF_NOMEM );
			CIFBENODE2(cr)->succs = (int *) cp;
			if ( (int) fread( cp, sizeof( int ), n, fd ) < n )
			    IO_ERROR;
		    }
		    if ( (n = CIFBENODE2(cr)->nlines) > 0 ) {
			if ( _Cif_filetbl[cifd].version == 3 ) {
			    /* Read and discard file ids 
			     * which exist in a V3 CIF but not in V2 */
			    cp = _Cif_space[ mode ]( n * sizeof( int ), cifd );
			    if ( cp == NULL )
				return( CIF_NOMEM );
			    if ( (int) fread( cp, sizeof( int ), n, fd ) < n )
				IO_ERROR;
			    free( cp );
			}
			cp = _Cif_space[ mode ]( n * sizeof( int ), cifd );
			if ( cp == NULL )
			    return( CIF_NOMEM );
			CIFBENODE2(cr)->lines = (int *) cp;
			if ( (int) fread( cp, sizeof( int ), n, fd ) < n )
			    IO_ERROR;
		    }
		    if ( (int) fread( &c, 1, 1, fd ) < 1 )
			IO_ERROR;
		    n = c;
		    cp = _Cif_space[ mode ]( n+1, cifd );
			if ( cp == NULL )
			    return( CIF_NOMEM );
		    CIFBENODE2(cr)->label = cp;
		    if ( (int) fread( cp, 1, n, fd ) < n )
			IO_ERROR;
		    cp[ n ] = 0;
		}
		else { /* Version 3 or beyond */
		    if ( (n = CIFBENODE(cr)->nsuccs) > 0 ) {
			cp = _Cif_space[ mode ]( n * sizeof( int ), cifd );
			if ( cp == NULL )
			    return( CIF_NOMEM );
			CIFBENODE(cr)->succs = (int *) cp;
			if ( (int) fread( cp, sizeof( int ), n, fd ) < n )
			    IO_ERROR;
		    }
		    if ( (n = CIFBENODE(cr)->nlines) > 0 ) {
			cp = _Cif_space[ mode ]( n * sizeof( int ), cifd );
			if ( cp == NULL )
			    return( CIF_NOMEM );
			CIFBENODE(cr)->fid = (int *) cp;
			if ( _Cif_filetbl[cifd].version == 2 ) {
			    for ( i = 0; i < n; i++ )
				CIFBENODE(cr)->fid[ i ] = 0;
			} else {
			    if ( (int) fread( cp, sizeof( int ), n, fd ) < n )
				IO_ERROR;
			}
			cp = _Cif_space[ mode ]( n * sizeof( int ), cifd );
			if ( cp == NULL )
			    return( CIF_NOMEM );
			CIFBENODE(cr)->lines = (int *) cp;
			if ( (int) fread( cp, sizeof( int ), n, fd ) < n )
			    IO_ERROR;
		    }
		    if ( (int) fread( &c, 1, 1, fd ) < 1 )
			IO_ERROR;
		    n = c;
		    assert( n >= 0 && n < 255 );
		    cp = _Cif_space[ mode ]( n+1, cifd );
			if ( cp == NULL )
			    return( CIF_NOMEM );
		    CIFBENODE(cr)->label = cp;
		    if ( (int) fread( cp, 1, n, fd ) < n )
			IO_ERROR;
		    cp[ n ] = 0;
		}
		break;

	case CIF_BE_FID:
		if ( (n = CIFBEFID(cr)->nfid) > 0 ) {
		    cp = _Cif_space[ mode ]( n * sizeof( int ), cifd );
		    if ( cp == NULL )
			return( CIF_NOMEM );
		    CIFBEFID(cr)->fid = (int *) cp;
		    if ( (int) fread( cp, sizeof( int ), n, fd ) < n )
			IO_ERROR;
		}
		break;

#endif


	case CIF_MACH_CHAR:
		if (_Cif_filetbl[cifd].lang == CIF_LG_F77) {
		  if (_Cif_filetbl[cifd].return_version == 1) {

		    /*
		     * We have the correct bits, but some of them could
		     * not be set in v1, so mask those out
		     */
#ifdef CRAY2
		    /*
		     * On a Cray2, TAILGT was 0x01 in v1, it is now 0x02
		     * It is the only valid cray2 value for a v1 cif
		     */
		    if (CIFMC1(cr)->valmask | CIF_MC_TAILGT_1)  {
		      CIFMC1(cr)->valmask = CIF_MC_TAILGT;
		    }
		    else {
		      CIFMC1(cr)->valmask = 0;
		    }
#else  /* Non-Cray2 have more bits to mask out */
		    CIFMC1(cr)->valmask = CIFMC1(cr)->valmask & CIF_MC_MASK;
#endif /* CRAY2 */

		  } /* else, v2, no change required to value mask */
	        } /* else, f90 or C, no change required to the value mask */

		break;

	default:
		break;

	}

	return (0);

}

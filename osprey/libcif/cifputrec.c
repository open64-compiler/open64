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


static char USMID[] = "@(#) libcif/cifputrec.c	30.6	07/26/96 07:19:13";


/* -------------------------------------------------------------------------
 * Cif_Putrecord writes out the provided record to a CIF file in binary form.
 * It writes out the record type value alone then writes out the record
 * structure, followed by any variable length information.  Shortened forms
 * of some of the structures are used to avoid writing out pointers that
 * are used only in memory.
 *
 * Tabs are set up to be read with tab spacing = 3
 * --------------------------------------------------------------------------
 */

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "cif_int.h"

static FILE *fd;							/* file desciptor of cif file */


/* --- write_strlist outputs a list of strings --- */
static int write_strlist (sp, ns)
char **sp;					/* pointer to array of string pointers */
int ns;						/* number of strings to write */
{
	int i;
	short slen[100];		/* array of string lengths */

	if (ns > 0) {
		for (i = 0; i < ns; i++)
			slen[i] = strlen (sp[i]);
		if (fwrite ((char *)slen, sizeof(short), i, fd) != i)
			return (CIF_SYSERR);
		for (i = 0; i < ns; i++) {
			if (fwrite ( sp[i], sizeof(char), slen[i], fd) != slen[i])
				return (CIF_SYSERR);
		}
	}
	return (0);
}

/* --- write_unitdir compacts and writes a CIF_UNITDIR record --- */
static int write_unitdir (cr)
struct Cif_generic *cr;
{
	int i, j;
	struct Cif_unitdir ut;
	struct Cif_urectbl ur[CIF_MAXRECORD];
	struct Cif_urectbl *urp;
 
	ut.rectype = CIF_UNITDIR;
	ut.maxsid = CIFUDIR(cr)->maxsid;
	urp = CIFUDIR(cr)->ur;
	for (i = j = 0; i < (int) CIFUDIR(cr)->nsections; i++) {
		if (urp[i].nrecords > 0)
			ur[j++] = urp[i];
	}
	ut.nsections = j;
	if (fwrite (&ut, UNITDIR_SSIZE, 1, fd) != 1) return (CIF_SYSERR);
	if (fwrite (ur, URECTBL_SSIZE, j, fd) != j) return (CIF_SYSERR);
	return (0);

}

/* --------------------------------------------------------------------------
 * Cif_Putrecord
 * --------------------------------------------------------------------------
 */

int Cif_Putrecord
#ifdef __STDC__
(int cifd, struct Cif_generic *cr)
#else
(cifd, cr)
int cifd;									/* CIF file descriptor */
struct Cif_generic *cr;					/* pointer to CIF structure */
#endif
{

	int i, j, n;
	int rtype;
	char *cp;
	unsigned char c;

	if (cifd < 0 || cifd >= CIF_FT_SIZE || _Cif_filetbl[cifd].form == NOT_A_CIF)
		return (CIF_NOTOPEN);
	else if (_Cif_filetbl[cifd].optype == 'r')
		return (CIF_BADREQ);

	fd = _Cif_filetbl[cifd].fd;
	rtype = cr->rectype;

	if (rtype < 1 || rtype > CIF_MAXRECORD || _Cif_structsize == 0)
		return (CIF_BADFORM);

	/* If CIF_UNITDIR, process separately because the unit record table must be
	 * compressed.
	 */

	if (rtype == CIF_UNITDIR)
		return (write_unitdir (cr));

	/* Write out the record type, then (possibly short) record structure itself
	 * minus the record type then write out auxiliary information for each
	 * record type that needs it.
	 */

	if (fwrite((char *)cr, sizeof(char), 1, fd) != 1) return (CIF_SYSERR);
	cp = (char *)cr;

	if (fwrite(++cp, _Cif_shortsize[rtype][_Cif_filetbl[cifd].version]-1, 1, fd) != 1)
		return (CIF_SYSERR);

	switch (rtype) {

	case CIF_CALLSITE:
		if ((i = CIFCS(cr)->nargs) > 0) {
			if (fwrite((char *)CIFCS(cr)->argids, sizeof(long), i, fd) != i)
				return (CIF_SYSERR);
		}
		break;

	case CIF_COMBLK:
		/* Check which structures we should be using, v1 or v2 */

		if (_Cif_filetbl[cifd].version == 1) {

			if (fwrite (CIFCB1(cr)->name, sizeof(char), CIFCB1(cr)->nlen, fd) !=
			    	CIFCB1(cr)->nlen) return (CIF_SYSERR);

	        }
		else { /* use v2 cif records */

			if (fwrite (CIFCB(cr)->name, sizeof(char), CIFCB(cr)->nlen, fd) !=
			    	CIFCB(cr)->nlen) return (CIF_SYSERR);

		}
		break;

	case CIF_CONST:
		/* Check which structures we should be using, v1 or v2 */

		if (_Cif_filetbl[cifd].version == 1) {

		  if (fwrite (CIFCON1(cr)->name, sizeof(char), CIFCON1(cr)->nlen, fd) !=
		      CIFCON1(cr)->nlen) return (CIF_SYSERR);
		  if (fwrite (CIFCON1(cr)->value, sizeof(char), CIFCON1(cr)->vlen, fd) !=
		      CIFCON1(cr)->vlen) return (CIF_SYSERR);

		}
		else { /* v2 cif */

		  if (fwrite (CIFCON(cr)->name, sizeof(char), CIFCON(cr)->nlen, fd) !=
		      CIFCON(cr)->nlen) return (CIF_SYSERR);
		  if (fwrite (CIFCON(cr)->value, sizeof(char), CIFCON(cr)->vlen, fd) !=
		      CIFCON(cr)->vlen) return (CIF_SYSERR);

		  if (CIFCON(cr)->origform)
		    if (fwrite (CIFCON(cr)->oform, sizeof(char), CIFCON(cr)->olen, fd) !=
			CIFCON(cr)->olen) return (CIF_SYSERR);

		}
		break;

	case CIF_ENTRY:
		if ((i = CIFENTRY(cr)->nargs) > 0) {
			if (fwrite (CIFENTRY(cr)->argids, sizeof(long), i, fd) != i)
				return (CIF_SYSERR);
		}
		if (fwrite (CIFENTRY(cr)->name, sizeof(char), CIFENTRY(cr)->nlen, fd)
			!= CIFENTRY(cr)->nlen) return (CIF_SYSERR);
		break;

	case CIF_FILE:
		if (_Cif_filetbl[cifd].version == 3) {
		  if (fwrite (CIFFILE(cr)->name, sizeof(char), CIFFILE(cr)->nlen, fd)
		      != CIFFILE(cr)->nlen) return (CIF_SYSERR);

		  if (fwrite (CIFFILE(cr)->oname, sizeof(char), CIFFILE(cr)->onlen, fd)
		      != CIFFILE(cr)->onlen) return (CIF_SYSERR);
		}
		else {
		  if (fwrite (CIFFILE1(cr)->name, sizeof(char), CIFFILE1(cr)->nlen, fd)
		      != CIFFILE1(cr)->nlen) return (CIF_SYSERR);
		}
		break;

	case CIF_LABEL:
		if (fwrite (CIFLABEL(cr)->name, sizeof(char), CIFLABEL(cr)->nlen, fd)
			!= CIFLABEL(cr)->nlen) return (CIF_SYSERR);
		break;

	case CIF_ORIG_CMD:
		if (fwrite (CIFOCMD(cr)->name, sizeof(char), CIFOCMD(cr)->nlen, fd)
			!= CIFOCMD(cr)->nlen) return (CIF_SYSERR);
		break;

	case CIF_MESSAGE:
		if (_Cif_filetbl[cifd].version == 3) {
		  if (fwrite (CIFMSG(cr)->name, sizeof(char), CIFMSG(cr)->nlen, fd)
		      != CIFMSG(cr)->nlen) return (CIF_SYSERR);
		  if ((i = write_strlist (CIFMSG(cr)->args, (int) CIFMSG(cr)->nargs)) < 0)
		      return (i);
		}
		else {
		  if ((i = write_strlist (CIFMSG1(cr)->args, (int) CIFMSG1(cr)->nargs)) < 0)
		      return (i);
		}
		break;

   case CIF_MISC_OPTS:
		/* Check which structures we should be using, v1 or v2 */

		if (_Cif_filetbl[cifd].version == 1) {

		  if ((i = CIFMO1(cr)->nmsgs) > 0) {
		    if (fwrite (CIFMO1(cr)->msgno, sizeof(long), i, fd) != i)
		      return (CIF_SYSERR);
		  }
		  if ((i = write_strlist (CIFMO1(cr)->cdirs, (int) CIFMO1(cr)->ncdirs)) < 0)
		    return (i);
		  if ((i = CIFMO1(cr)->onlen) > 0) {
		    if (fwrite (CIFMO1(cr)->objname, sizeof(char), i, fd) != i)
		      return (CIF_SYSERR);
		  }
		  if ((i = CIFMO1(cr)->cnlen) > 0) {
		    if (fwrite (CIFMO1(cr)->calname, sizeof(char), i, fd) != i)
		      return (CIF_SYSERR);
		  }
		  if ((i = CIFMO1(cr)->inlen) > 0) {
		    if (fwrite (CIFMO1(cr)->inname, sizeof(char), i, fd) != i)
		      return (CIF_SYSERR);
		  }
		}
		else { /* v2 cif structures */

		  if ((i = CIFMO(cr)->nmsgs) > 0) {
		    if (fwrite (CIFMO(cr)->msgno, sizeof(long), i, fd) != i)
		      return (CIF_SYSERR);
		  }
		  if ((i = write_strlist (CIFMO(cr)->cdirs, (int) CIFMO(cr)->ncdirs)) < 0)
		    return (i);
		  if ((i = CIFMO(cr)->onlen) > 0) {
		    if (fwrite (CIFMO(cr)->objname, sizeof(char), i, fd) != i)
		      return (CIF_SYSERR);
		  }
		  if ((i = CIFMO(cr)->cnlen) > 0) {
		    if (fwrite (CIFMO(cr)->calname, sizeof(char), i, fd) != i)
		      return (CIF_SYSERR);
		  }
		  if ((i = CIFMO(cr)->inlen) > 0) {
		    if (fwrite (CIFMO(cr)->inname, sizeof(char), i, fd) != i)
		      return (CIF_SYSERR);
		  }
		  if ((i = CIFMO(cr)->llen) > 0) {
		    if (fwrite (CIFMO(cr)->lname, sizeof(char), i, fd) != i)
		      return (CIF_SYSERR);
		  }

		  if ((i = write_strlist (CIFMO(cr)->incdirs, (int) CIFMO(cr)->numincs)) < 0)
		    return (i);
		}
		break;

	case CIF_NAMELIST:
		if (fwrite (CIFNL(cr)->name, sizeof(char), CIFNL(cr)->nlen, fd) !=
			CIFNL(cr)->nlen) return (CIF_SYSERR);
		if (fwrite((char *)CIFNL(cr)->ids, sizeof(long), CIFNL(cr)->nids, fd)
			!= CIFNL(cr)->nids) return (CIF_SYSERR);
		break;

	case CIF_ND_MSG:
		if ((i = write_strlist (CIFNMSG(cr)->args, (int) CIFNMSG(cr)->nargs)) < 0)
			return (i);
		break;

	case CIF_OBJECT:
		/* Check which structures we should be using, v1 or v2 */

		if (_Cif_filetbl[cifd].version == 1) {

		  	if (CIFOBJ1(cr)->name != NULL) {
			  	if (fwrite (CIFOBJ1(cr)->name, sizeof(char), CIFOBJ1(cr)->nlen, fd)
				    != CIFOBJ1(cr)->nlen) return (CIF_SYSERR);
			}
			if ((i = CIFOBJ1(cr)->ndims) > 0) {
			  	if (fwrite(CIFOBJ1(cr)->dim, DIM_SSIZE, i, fd) != i)
				  	return (CIF_SYSERR);
			}
		}
		else { /* use v2 cif records */

		  	if (CIFOBJ(cr)->name != NULL) {
			  	if (fwrite (CIFOBJ(cr)->name, sizeof(char), CIFOBJ(cr)->nlen, fd)
				    != CIFOBJ(cr)->nlen) return (CIF_SYSERR);
			}
			if ((i = CIFOBJ(cr)->ndims) > 0) {
			  	if (fwrite(CIFOBJ(cr)->dim, DIM_SSIZE, i, fd) != i)
				  	return (CIF_SYSERR);
			}

		}
		break;

	case CIF_CDIR:
		if (fwrite((char *)CIFCDIR(cr)->ids, sizeof(long), CIFCDIR(cr)->nids, fd)
			!= CIFCDIR(cr)->nids) return (CIF_SYSERR);
		break;

	case CIF_CDIR_DOSHARED:
		if (fwrite((char *)CIFCDIRDO(cr)->ids, sizeof(long), CIFCDIRDO(cr)->nids, fd)
			!= CIFCDIRDO(cr)->nids) return (CIF_SYSERR);
		break;

        case CIF_GEOMETRY:
		if ((i = CIFGEOM(cr)->ndims) > 0) {
			if (fwrite(CIFGEOM(cr)->dim, GEOM_SSIZE, i, fd) != i)
				return (CIF_SYSERR);
		}
		if (CIFGEOM(cr)->name != NULL) {
		  	if (fwrite (CIFGEOM(cr)->name, sizeof(char), CIFGEOM(cr)->nlen, fd)
				!= CIFGEOM(cr)->nlen) return (CIF_SYSERR);
		}
		break;

	case CIF_UNIT:
		if (fwrite (CIFUNIT(cr)->name, sizeof(char), CIFUNIT(cr)->nlen, fd)
			!= CIFUNIT(cr)->nlen) return (CIF_SYSERR);
		break;

	case CIF_ENDUNIT:
		if (fwrite (CIFENDU(cr)->name, sizeof(char), CIFENDU(cr)->nlen, fd)
			!= CIFENDU(cr)->nlen) return (CIF_SYSERR);
		break;

	case CIF_USAGE:
		/* Check which structures we should be using, v1 or v2 */

		if (_Cif_filetbl[cifd].version == 1) {

		  if (fwrite (CIFUSAGE1(cr)->use, sizeof(struct Cif_use),
			      CIFUSAGE1(cr)->nuses, fd) != CIFUSAGE1(cr)->nuses)
		    return (CIF_SYSERR);

		}
		else { /* writing a v2 cif */

		  if (fwrite (CIFUSAGE(cr)->use, sizeof(struct Cif_use),
			      CIFUSAGE(cr)->nuses, fd) != CIFUSAGE(cr)->nuses)
		    return (CIF_SYSERR);

		  if (CIFUSAGE(cr)->nmembs > 0) {
		    if (fwrite (CIFUSAGE(cr)->membs, sizeof(long),
				CIFUSAGE(cr)->nmembs, fd) != CIFUSAGE(cr)->nmembs)
		      return (CIF_SYSERR);
		  }
		}
		break;

	case CIF_FILEDIR:
	{
		struct Cif_unittbl *ut;

		ut = CIFFDIR(cr)->ut;
		for (i= 0; i < (int) CIFFDIR(cr)->nunits; i++, ut++) {
			if (fwrite (ut, UNITTBL_SSIZE, 1, fd) != 1) return (CIF_SYSERR);
			if (fwrite (ut->name, sizeof(char), ut->nlen, fd) != ut->nlen)
				return (CIF_SYSERR);
		}
		break;
	}
			
	case CIF_C_TAG:

		for (i = 0; i < (int) CIFCTAG(cr)->nmods; i++) {
			  if (fwrite (&(CIFCTAG(cr)->mods[i]), TMOD_SSIZE, 1, fd) != 1)
			    	return (CIF_SYSERR);
			}

		i = CIFCTAG(cr)->nmems;
		if (fwrite (CIFCTAG(cr)->memids, sizeof(long), i, fd) != i)
		  	return(CIF_SYSERR);
		if (fwrite (CIFCTAG(cr)->name, sizeof(char), CIFCTAG(cr)->nlen, fd) !=
		    CIFCTAG(cr)->nlen) return (CIF_SYSERR);

		break;

	case CIF_C_OPTS:
		if (fwrite (CIFCOPTS(cr)->name, sizeof(char), CIFCOPTS(cr)->nlen, fd) !=
			CIFCOPTS(cr)->nlen) return (CIF_SYSERR);
		if ((i = write_strlist (CIFCOPTS(cr)->incs, (int) CIFCOPTS(cr)->nincs)) < 0)
			return (i);
		if ((i = write_strlist (CIFCOPTS(cr)->defs, (int) CIFCOPTS(cr)->ndefs)) < 0)
			return (i);
		if ((i = write_strlist (CIFCOPTS(cr)->udefs, (int) CIFCOPTS(cr)->nudefs)) < 0)
			return (i);
		break;

	case CIF_C_MESSAGE:

		/* Check which structures we should be using, v1 or v2 */

		if (_Cif_filetbl[cifd].version == 1) {

		  if ((i = write_strlist (CIFCMSG1(cr)->args, (int) CIFCMSG1(cr)->nargs)) < 0)
		    	return (i);

		}
		else {  /* writing a v2 cif */

		  if ((i = write_strlist (CIFCMSG(cr)->args, (int) CIFCMSG(cr)->nargs)) < 0)
		    	return (i);
		}

		break;
	
	case CIF_C_CONST:
		if (fwrite (CIFCCON(cr)->value, sizeof(char), CIFCCON(cr)->vlen, fd)
			!= CIFCCON(cr)->vlen) return (CIF_SYSERR);
		break;

	case CIF_C_ENTRY:

		/* Check which structures we should be using, v1 or v2 */

		if (_Cif_filetbl[cifd].version == 1) {

		  for (i = 0; i < (int) CIFCENTRY1(cr)->nmods; i++) {
		    	if (fwrite (&(CIFCENTRY1(cr)->mods[i]), TMOD_SSIZE, 1, fd) != 1)
			  	return (CIF_SYSERR);
		      }
		  if (fwrite (CIFCENTRY1(cr)->argids, sizeof(long), CIFCENTRY1(cr)->nargs,
			      fd) != CIFCENTRY1(cr)->nargs) return (CIF_SYSERR);
		  if (fwrite (CIFCENTRY1(cr)->name, sizeof(char), CIFCENTRY1(cr)->nlen, fd)
		      != CIFCENTRY1(cr)->nlen) return (CIF_SYSERR);

		}
		else { /* writing a v2 cif */

		  for (i = 0; i < (int) CIFCENTRY(cr)->nmods; i++) {
		    	if (fwrite (&(CIFCENTRY(cr)->mods[i]), TMOD_SSIZE, 1, fd) != 1)
			  	return (CIF_SYSERR);
		      }
		  if (fwrite (CIFCENTRY(cr)->argids, sizeof(long), CIFCENTRY(cr)->nargs,
			      fd) != CIFCENTRY(cr)->nargs) return (CIF_SYSERR);
		  if (fwrite (CIFCENTRY(cr)->name, sizeof(char), CIFCENTRY(cr)->nlen, fd)
		      != CIFCENTRY(cr)->nlen) return (CIF_SYSERR);
		}

		break;

	case CIF_C_OBJECT:
		for (i = 0; i < (int) CIFCOBJ(cr)->nmods; i++) {
			if (fwrite (&(CIFCOBJ(cr)->mods[i]), TMOD_SSIZE, 1, fd) != 1)
				 return (CIF_SYSERR);
		}
		if (CIFCOBJ(cr)->nlen > 0) {
			if (fwrite (CIFCOBJ(cr)->name, sizeof(char), CIFCOBJ(cr)->nlen, fd)
				!= CIFCOBJ(cr)->nlen) return (CIF_SYSERR);
		}
		break;

	case CIF_C_LINT_DIRECTIVE:

		if (CIFCLDIR(cr)->nlen > 0) {
			if (fwrite (CIFCLDIR(cr)->name, sizeof(char), CIFCLDIR(cr)->nlen, fd)
				!= CIFCLDIR(cr)->nlen) return (CIF_SYSERR);
		}

		break;

	case CIF_C_MACRO_DEF:

		if (CIFCMDEF(cr)->nlen > 0) {
			if (fwrite (CIFCMDEF(cr)->name, sizeof(char), CIFCMDEF(cr)->nlen, fd)
				!= CIFCMDEF(cr)->nlen) return (CIF_SYSERR);
		}

		break;

	case CIF_C_ENTRY_END:

		if (CIFCEEND(cr)->nlen > 0) {
			if (fwrite (CIFCEEND(cr)->name, sizeof(char), CIFCEEND(cr)->nlen, fd)
				!= CIFCEEND(cr)->nlen) return (CIF_SYSERR);
		}

		break;

	case CIF_BE_NODE:
		if (_Cif_filetbl[cifd].version == 2) {

		    if ( (n = CIFBENODE2(cr)->nsuccs) > 0 ) {
			if ( (int) fwrite( (char *)CIFBENODE2(cr)->succs, sizeof( int ), n, fd ) < n )
			    return( CIF_SYSERR );
		    }
		    if ( (n = CIFBENODE2(cr)->nlines) > 0 ) {
			if ( (int) fwrite( (char *)CIFBENODE2(cr)->lines, sizeof( int ), n, fd ) < n )
			    return( CIF_SYSERR );
		    }
		    cp = CIFBENODE2(cr)->label;
		    n = strlen( cp );
		    assert( n < 255 );
		    c = n;
		    if ( (int) fwrite( &c, 1, 1, fd ) < 1 )
			return( CIF_SYSERR );
		    if ( (int) fwrite( cp, 1, n, fd ) < n )
			return( CIF_SYSERR );

		} else {	/* version >= 3 */

		    if ( (n = CIFBENODE(cr)->nsuccs) > 0 ) {
			if ( (int) fwrite( (char *)CIFBENODE(cr)->succs, sizeof( int ), n, fd ) < n )
			    return( CIF_SYSERR );
		    }
		    if ( (n = CIFBENODE(cr)->nlines) > 0 ) {
			if ( (int) fwrite( (char *)CIFBENODE(cr)->fid, sizeof( int ), n, fd ) < n )
			    return( CIF_SYSERR );
			if ( (int) fwrite( (char *)CIFBENODE(cr)->lines, sizeof( int ), n, fd ) < n )
			    return( CIF_SYSERR );
		    }
		    cp = CIFBENODE(cr)->label;
		    n = strlen( cp );
		    assert( n < 255 );
		    c = n;
		    if ( (int) fwrite( &c, 1, 1, fd ) < 1 )
			return( CIF_SYSERR );
		    if ( (int) fwrite( cp, 1, n, fd ) < n )
			return( CIF_SYSERR );
		}
		break;

	case CIF_BE_FID:
		if ( (n = CIFBEFID(cr)->nfid) > 0 ) {
		    if ( (int) fwrite( (char *)CIFBEFID(cr)->fid, sizeof( int ), n, fd ) < n )
			return( CIF_SYSERR );
		}
		break;

	case CIF_F90_CALLSITE:
		if ((i = CIFF90CS(cr)->nargs) > 0) {
			if (fwrite((char *)CIFF90CS(cr)->argids, sizeof(long), i, fd) != i)
			    return (CIF_SYSERR);
			if (fwrite((char *)CIFF90CS(cr)->nmembs, sizeof(int), i, fd) != i)
			    return (CIF_SYSERR);

			for (j = 0; j < (int) CIFF90CS(cr)->nargs; j++) {
			    if (CIFF90CS(cr)->nmembs[j] > 0)
				if (fwrite((char *)CIFF90CS(cr)->membs[j], sizeof(long), CIFF90CS(cr)->nmembs[j], fd) != CIFF90CS(cr)->nmembs[j])
				    return (CIF_SYSERR);
			}

			if (CIFF90CS(cr)->rank == 1) {

			    if (fwrite((char *)CIFF90CS(cr)->ranks, sizeof(int), i, fd) != i)
				return (CIF_SYSERR);

			}



		    }
		break;

	case CIF_F90_SCOPE_INFO:
		if ((i = CIFF90SI(cr)->numalts) > 0) {
			if (fwrite((char *)CIFF90SI(cr)->entryids, sizeof(long), i, fd) != i)
				return (CIF_SYSERR);
		}
		break;

	case CIF_F90_COMBLK:
		if (fwrite (CIFF90CB(cr)->name, sizeof(char), CIFF90CB(cr)->nlen, fd) !=
			CIFF90CB(cr)->nlen) return (CIF_SYSERR);
		break;


	case CIF_F90_CONST:
		if (fwrite (CIFF90CON(cr)->value, sizeof(char), CIFF90CON(cr)->vlen, fd) !=
			CIFF90CON(cr)->vlen) return (CIF_SYSERR);

		break;

	case CIF_F90_ENTRY:

		if ((i = CIFF90ENTRY(cr)->nargs) > 0) {
			if (fwrite (CIFF90ENTRY(cr)->argids, sizeof(long), i, fd) != i)
				return (CIF_SYSERR);
		}
		if (fwrite (CIFF90ENTRY(cr)->name, sizeof(char), CIFF90ENTRY(cr)->nlen, fd)
			!= CIFF90ENTRY(cr)->nlen) return (CIF_SYSERR);

		break;




	case CIF_F90_DERIVED_TYPE:
		/* Check which structures we should be using, v2 or v3 */

		if (_Cif_filetbl[cifd].version == 2) {

		  if (fwrite (CIFF90DTYPE2(cr)->name, sizeof(char), CIFF90DTYPE2(cr)->nlen, fd)
		      != CIFF90DTYPE2(cr)->nlen) return (CIF_SYSERR);
		  if ((i = CIFF90DTYPE2(cr)->nmembs) > 0) {
		    if (fwrite (CIFF90DTYPE2(cr)->memids, sizeof(long), i, fd) != i)
			return (CIF_SYSERR);
		  }
		}
		else {	/* Version 3 CIF */

		  if (fwrite (CIFF90DTYPE(cr)->name, sizeof(char), CIFF90DTYPE(cr)->nlen, fd)
		      != CIFF90DTYPE(cr)->nlen) return (CIF_SYSERR);
		  if ((i = CIFF90DTYPE(cr)->nmembs) > 0) {
		    if (fwrite (CIFF90DTYPE(cr)->memids, sizeof(long), i, fd) != i)
			return (CIF_SYSERR);
		  }
		}

		break;

	case CIF_F90_LABEL:
		if (fwrite (CIFF90LABEL(cr)->name, sizeof(char), CIFF90LABEL(cr)->nlen, fd)
			!= CIFF90LABEL(cr)->nlen) return (CIF_SYSERR);
		break;

	case CIF_F90_NAMELIST:

		if (fwrite (CIFF90NL(cr)->name, sizeof(char), CIFF90NL(cr)->nlen, fd) !=
			CIFF90NL(cr)->nlen) return (CIF_SYSERR);

		if (fwrite((char *)CIFF90NL(cr)->ids, sizeof(long), CIFF90NL(cr)->nids, fd)
			!= CIFF90NL(cr)->nids) return (CIF_SYSERR);

		break;

	case CIF_F90_OBJECT:
		if (CIFF90OBJ(cr)->name != NULL) {
			if (fwrite (CIFF90OBJ(cr)->name, sizeof(char), CIFF90OBJ(cr)->nlen, fd)
				!= CIFF90OBJ(cr)->nlen) return (CIF_SYSERR);
		}
		if ((i = CIFF90OBJ(cr)->ndims) > 0 &&
		    CIFF90OBJ(cr)->atype != CIF_AT_DEFERRED) { /* deferred arrays have all
								* dimensions assumed to be ':'
								*/
			if (fwrite(CIFF90OBJ(cr)->dim, DIM_SSIZE, i, fd) != i)
				return (CIF_SYSERR);
		}
		break;


   case CIF_F90_MISC_OPTS:

		if ((i = CIFF90MO(cr)->ciflen) > 0) {
			if (fwrite (CIFF90MO(cr)->cifname, sizeof(char), i, fd) != i)
				return (CIF_SYSERR);
		}

		if ((i = write_strlist (CIFF90MO(cr)->Pdirs, (int) CIFF90MO(cr)->nPdirs)) < 0)
			return (i);

		if ((i = write_strlist (CIFF90MO(cr)->pdirs, (int) CIFF90MO(cr)->npdirs)) < 0)
			return (i);
		if ((i = CIFF90MO(cr)->nmsgs) > 0) {
			if (fwrite (CIFF90MO(cr)->msgno, sizeof(long), i, fd) != i)
				return (CIF_SYSERR);
		}
		if ((i = write_strlist (CIFF90MO(cr)->cdirs, (int) CIFF90MO(cr)->ncdirs)) < 0)
			return (i);
		if ((i = CIFF90MO(cr)->onlen) > 0) {
			if (fwrite (CIFF90MO(cr)->objname, sizeof(char), i, fd) != i)
				return (CIF_SYSERR);
		}
		if ((i = CIFF90MO(cr)->cnlen) > 0) {
			if (fwrite (CIFF90MO(cr)->calname, sizeof(char), i, fd) != i)
				return (CIF_SYSERR);
		}
		if ((i = CIFF90MO(cr)->inlen) > 0) {
			if (fwrite (CIFF90MO(cr)->inname, sizeof(char), i, fd) != i)
				return (CIF_SYSERR);
		}
		break;


	case CIF_F90_OPT_OPTS:

		if ((i = CIFF90OPTOPT(cr)->noptlevels) > 0) {
			if (fwrite (CIFF90OPTOPT(cr)->lopts, OPTS_SSIZE, i, fd) != i)
				return (CIF_SYSERR);
		}
		break;

     	case CIF_F90_INT_BLOCK:
		/* Check which structures we should be using, v2 or v3 */

		if (_Cif_filetbl[cifd].version == 2) {

		  if ((i = CIFF90IB2(cr)->nlen) > 0) {
		    if (fwrite (CIFF90IB2(cr)->name, sizeof(char), i, fd) != i)
			return (CIF_SYSERR);
		  }
		  if ((i = CIFF90IB2(cr)->numints) > 0) {
		    if (fwrite (CIFF90IB2(cr)->procids, sizeof(long), i, fd) != i)
			return (CIF_SYSERR);
		  }
		}
		else {  /* Version 3 CIF */

		  if ((i = CIFF90IB(cr)->nlen) > 0) {
		    if (fwrite (CIFF90IB(cr)->name, sizeof(char), i, fd) != i)
			return (CIF_SYSERR);
		  }
		  if ((i = CIFF90IB(cr)->numints) > 0) {
		    if (fwrite (CIFF90IB(cr)->procids, sizeof(long), i, fd) != i)
			return (CIF_SYSERR);
		  }
		}
		break;

     	case CIF_F90_RENAME:

		if ((i = CIFF90RN(cr)->nlen) > 0) {
			if (fwrite (CIFF90RN(cr)->name, sizeof(char), i, fd) != i)
				return (CIF_SYSERR);
		}
		if ((i = CIFF90RN(cr)->orignlen) > 0) {
			if (fwrite (CIFF90RN(cr)->origname, sizeof(char), i, fd) != i)
				return (CIF_SYSERR);
		}

		if ((i = CIFF90RN(cr)->nlocalids) > 0) {
		  	if (fwrite (CIFF90RN(cr)->localid, sizeof(long), i, fd) != i)
			  	return (CIF_SYSERR);
		}
		break;

	case CIF_CC_TYPE:
		if ((i = CIFCCTYPE(cr)->nlen) > 0) {
			if (fwrite (CIFCCTYPE(cr)->name, sizeof(char), i, fd) != i)
				return (CIF_SYSERR);
		}
		if ( (n = CIFCCTYPE(cr)->nmem) > 0 ) {
		    if ( (int) fwrite( (char *)CIFCCTYPE(cr)->mem, sizeof( int ), n, fd ) < n )
			return( CIF_SYSERR );
		}
		break;

	case CIF_CC_ENTRY:
		if ((i = CIFCCENT(cr)->nlen) > 0) {
			if (fwrite (CIFCCENT(cr)->name, sizeof(char), i, fd) != i)
				return (CIF_SYSERR);
		}
		if ((i = CIFCCENT(cr)->elen) > 0) {
			if (fwrite (CIFCCENT(cr)->ename, sizeof(char), i, fd) != i)
				return (CIF_SYSERR);
		}
		if ( (n = CIFCCENT(cr)->nparam) > 0 ) {
		    if ( (int) fwrite( (char *)CIFCCENT(cr)->param, sizeof( int ), n, fd ) < n )
			return( CIF_SYSERR );
		}
		break;

	case CIF_CC_OBJ:
		if ((i = CIFCCOBJ(cr)->nlen) > 0) {
			if (fwrite (CIFCCOBJ(cr)->name, sizeof(char), i, fd) != i)
				return (CIF_SYSERR);
		}
		break;

	case CIF_CC_ENUM:
		if ((i = CIFCCENUM(cr)->nlen) > 0) {
			if (fwrite (CIFCCENUM(cr)->name, sizeof(char), i, fd) != i)
				return (CIF_SYSERR);
		}
		if ((i = CIFCCENUM(cr)->vlen) > 0) {
			if (fwrite (CIFCCENUM(cr)->value, sizeof(char), i, fd) != i)
				return (CIF_SYSERR);
		}
		break;

	case CIF_CC_EXPR:
		if ( (n = CIFCCEXPR(cr)->noper) > 0 ) {
		    if ( (int) fwrite( (char *)CIFCCEXPR(cr)->oper, sizeof( int ), n, fd ) < n )
			return( CIF_SYSERR );
		}
		break;

	default:
		break;

	}
	return (0);
}

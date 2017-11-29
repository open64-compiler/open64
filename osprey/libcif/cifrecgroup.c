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


static char USMID[] = "@(#) libcif/cifrecgroup.c	30.5	06/27/97 14:34:02";


/*
 *	Cif_Recgroup retrieves all the records of a particular type that belong
 *	to a single unit.
 */

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#include <memory.h>
#include <stdio.h>
#include <stdlib.h>

#include "cif_int.h"

int
Cif_Recgroup
#ifdef __STDC__
(int cifd, struct Cif_unitdir *udir, int rtype, struct Cif_generic **rptr)
#else
(cifd, udir, rtype, rptr)
int cifd;
struct Cif_unitdir *udir;
int rtype;
struct Cif_generic **rptr;
#endif
{

	register int i, mode, n;
	struct Cif_urectbl *urp;
	struct Cif_generic *cr;
	struct Cif_generic *tmp_rec;
	FILE *fd;
	struct Cif_filedir *fdir;
	int status;
	int nrecords;
	long pos;

	if (cifd < 0 || cifd > CIF_FT_SIZE || _Cif_filetbl[cifd].form == NOT_A_CIF)
		return (CIF_NOTOPEN);
	else if (_Cif_filetbl[cifd].optype == 'w' ||
	         _Cif_filetbl[cifd].form == ASCII_CIF ||
	         _Cif_filetbl[cifd].seek == NO ||
	         rtype < 0 ||
	         rtype > ((_Cif_filetbl[cifd].return_version == 1) ?
			  	CIF_MAXRECORD_1 : CIF_MAXRECORD) ||
	         _Cif_structsize[rtype][_Cif_filetbl[cifd].return_version] == 0)
		return (CIF_BADREQ);

	/* Position file to start of record group and read in as many records
	 * as are present.
	 */

	fd = _Cif_filetbl[cifd].fd;

	/* if udir == NULL, we don't want to use the unit dir record, but want to
	   read records global to the file */

	if (udir == (struct Cif_unitdir *) NULL) {

		/*
		 * If memory management mode isn't set, then set to FIXED.  If the mode is
		 * FIXED, reset the amount of buffer used.
		 */

	  	mode = _Cif_filetbl[cifd].mode;
		if (mode == CIF_MEM_DEFAULT) {
		  	if ((status = Cif_Memmode (cifd, CIF_MEM_FIXED)) != 0)
			  	return (status);
			mode = _Cif_filetbl[cifd].mode;
		      }
		if (mode == CIF_MEM_FIXED)
		  	_Cif_memarea[_Cif_filetbl[cifd].fme].mused = 0;


		switch (rtype) {

		case CIF_CIFHDR : {

		  /* start from the beginning of the file, first record is the cif_cifhdr */

		    (void) Cif_Setpos(cifd, CIF_FIRST_RECORD);
		  
		  status =  Cif_Getrecord(cifd, rptr);
		  if (status < 0)
		    return(status);
		  else
		    return(1);  /* one record returned */

		}

		case CIF_FILEDIR : {
		  status =  Cif_Getfiledir(cifd, (struct Cif_filedir **) rptr);
		  if (status < 0)
		    return(status);
		  else
		    return(1);  /* one record returned */

		}

		case CIF_SRCFILE : {

		  /* source file comes after the filedir */

		  status =  Cif_Getfiledir(cifd, &fdir);
		  if (status < 0)
		    return(status);

		  status =  Cif_Getrecord(cifd, rptr);
		  if (status < 0)
		    return(status);
		  else
		    return(1);  /* one record returned */
		}

		case CIF_FILE : {

		  /* find out how many files there will be from the filedir */

		  status =  Cif_Getfiledir(cifd, &fdir);
		  if (status < 0)
		    return(status);

		  nrecords = CIFFDIR(fdir)->nfiles;

		  if (nrecords > 0) {
			n = nrecords *
			  _Cif_structsize[rtype][_Cif_filetbl[cifd].return_version];

			cr = *rptr = (struct Cif_generic *) _Cif_space[mode] (n, cifd);
			if (cr == NULL)
			  	return (CIF_NOMEM);
			(void) memset ((char *)cr, '\0', n);

			do {  /* look for first CIF_FILE */
			  pos = Cif_Getpos(cifd);  /* note start of record postion */

			  if ((status = Cif_Getrecord(cifd, &tmp_rec)) < 0)
			    return(status);

			} while (status != CIF_FILE);

			(void) Cif_Setpos(cifd, pos); /* go back to where the required records
						  started */

			for (i = 0; i < nrecords; i++) {
			    	if (fread ((char *)cr,
					   _Cif_shortsize[rtype][_Cif_filetbl[cifd].version],
					   1, fd) != 1) IO_ERROR;

				if ((n = _Cif_binread (cifd, rtype, cr, fd)) < 0)
				  	return (n);
				cr = (struct Cif_generic *)((char *)cr +
					_Cif_structsize[rtype][_Cif_filetbl[cifd].return_version]);

			      }
		      }

		  return(nrecords);
		}

		case CIF_INCLUDE : {

		  /* find out how many files there will be from the filedir */

		  status =  Cif_Getfiledir(cifd, &fdir);
		  if (status < 0)
		    return(status);

		  nrecords = CIFFDIR(fdir)->nincs;

		  if (nrecords > 0) {
		    	n = nrecords *
			  _Cif_structsize[rtype][_Cif_filetbl[cifd].return_version];

			cr = *rptr = (struct Cif_generic *) _Cif_space[mode] (n, cifd);
			if (cr == NULL)
			  	return (CIF_NOMEM);
			(void) memset ((char *)cr, '\0', n);

			do {  /* look for first CIF_INCLUDE */
			  pos = Cif_Getpos(cifd);  /* note start of record postion */

			  if ((status = Cif_Getrecord(cifd, &tmp_rec)) < 0)
			    return(status);

			} while (status != CIF_INCLUDE &&
				 status != CIF_UNIT);

			(void) Cif_Setpos(cifd, pos); /* go back to where the required records
						  started */

			for (i = 0; i < nrecords; i++) {
			    	if (fread ((char *)cr,
					   _Cif_shortsize[rtype][_Cif_filetbl[cifd].version],
					   1, fd) != 1) IO_ERROR;

				if ((n = _Cif_binread (cifd, rtype, cr, fd)) < 0)
				  	return (n);
				cr = (struct Cif_generic *)((char *)cr +
					_Cif_structsize[rtype][_Cif_filetbl[cifd].return_version]);

			      }
		      }

		  return(nrecords);
		}

		case CIF_UNITDIR : {

		  status =  Cif_Getfiledir(cifd, &fdir);
		  if (status < 0)
		    return(status);

		  status = Cif_Getunitdir(cifd, fdir->ut, (struct Cif_unitdir **) rptr );
		  if (status < 0)
		    	return(status);
		  else
		    return(1);  /* unitdir returned */
		}

		case CIF_SRC_POS:
		case CIF_MESSAGE : {

		    /* start from the beginning of the file and read all of
		     * the cif_messages, until we hit the first unit record
		     */
		    (void) Cif_Setpos(cifd, CIF_FIRST_RECORD);

		    do {  /* look for first record */
			pos = Cif_Getpos(cifd);  /* note start of record postion */
			if ((status = Cif_Getrecord(cifd, &tmp_rec)) < 0)
			    return(status);
		    } while (status != rtype &&
			     status != CIF_UNIT);

		    if (status != rtype) {
			return(0);  /* record was not found */
		    }

		    (void) Cif_Setpos(cifd, pos); /* go back to where the required records
						     started */
		    nrecords = -1;
		    do {  /* Count the messages */
/*			pos = Cif_Getpos(cifd);   note start of record postion */
			if ((status = Cif_Getrecord(cifd, &tmp_rec)) < 0)
			    return(status);

			nrecords++;

		    } while (status == rtype);

		    /* Allocate memory for the messages */

		    if (nrecords > 0) {
		    	n = nrecords *
			    _Cif_structsize[rtype][_Cif_filetbl[cifd].return_version];

			cr = *rptr = (struct Cif_generic *) _Cif_space[mode] (n, cifd);
			if (cr == NULL)
			    return (CIF_NOMEM);
			(void) memset ((char *)cr, '\0', n);

			(void) Cif_Setpos(cifd, pos); /* go back to where the required records
							 started */

			for (i = 0; i < nrecords; i++) {
			    if (fread ((char *)cr,
				       _Cif_shortsize[rtype][_Cif_filetbl[cifd].version],
				       1, fd) != 1) IO_ERROR;

			    if ((n = _Cif_binread (cifd, rtype, cr, fd)) < 0)
				return (n);
			    cr = (struct Cif_generic *)((char *)cr +
							_Cif_structsize[rtype][_Cif_filetbl[cifd].return_version]);

			}
		    }

		    return(nrecords);
		}


		case CIF_MACH_CHAR :
		case CIF_ND_MSG :
		case CIF_EDOPTS :
		case CIF_MISC_OPTS :
		case CIF_OPT_OPTS: {
		  /* start from the beginning of the file, first record is the cif_cifhdr */

		    (void) Cif_Setpos(cifd, CIF_FIRST_RECORD);

		  do {
		    status =  Cif_Getrecord(cifd, rptr);

		  }
		  while (status != rtype && status > 0);

		  if (status == rtype)
		    return(1);  /* record was found */
		  else
		    if (status != CIF_EOF)
		      return(status);  /* something went wrong */
		    else
		      return(0);  /* record was not found */

		}

		default:
		  return (CIF_BADREQ);  /* all other records should be accessed via
					   the unitrecord */
		}



	}


	else {  /* read from the unitdir to find out how many records there are,
		   and where they begin */

		mode = _Cif_filetbl[cifd].mode;  /* note. we don't have to check the mode
						    to see if it is the default mode as
						    to get here we must have issued at least
						    one getrecord to get the unitdir, which
						    would have checked the mode for us */
		urp = udir->ur;
		if ((nrecords = urp[rtype].nrecords) > 0) {
			if (fseek (fd, (long)urp[rtype].recpos, 0)) return (CIF_SYSERR);

			/* obtain sufficient storage for the records to be
			   returned, which may be different from what we are about
			   to read from the disk */

			n = /* urp[rtype]. */ nrecords *
			  _Cif_structsize[rtype][_Cif_filetbl[cifd].return_version];

			cr = *rptr = (struct Cif_generic *) _Cif_space[mode] (n, cifd);

			if (cr == NULL)
			  	return (CIF_NOMEM);
			(void) memset ((char *)cr, '\0', n);

			/* cif version being read is the same as that to be returned */

			if (_Cif_filetbl[cifd].version ==
			    _Cif_filetbl[cifd].return_version) {

			  for (i = 0; i < /* urp[rtype]. */ nrecords; i++) {

			    	if (fread ((char *)cr,
					   _Cif_shortsize[rtype][_Cif_filetbl[cifd].version],
					   1, fd) != 1) IO_ERROR;

				if ((n = _Cif_binread (cifd, rtype, cr, fd)) < 0)
				  	return (n);
				cr = (struct Cif_generic *)((char *)cr +
					_Cif_structsize[rtype][_Cif_filetbl[cifd].version]);
			      }
			}
			else {
			  /* cif version on disk is different to that requested, which means
			     that we have to map the binary records accordingly, v1 <-> v2 */

			  /* create a temporary, static buffer in which to read data into
			     just in case the cif on disk is larger than the cif record
			     to be returned (eg cif v2 on disk; application wants a v1) */

			  if (_cif_map_buffer == (struct Cif_generic *) NULL)
			    _cif_map_buffer = (struct Cif_generic *) malloc(CIF_MAX_SSIZE);


			  for (i = 0; i < (int) urp[rtype].nrecords; i++) {
			    	if (fread ((char *)_cif_map_buffer,
					   _Cif_shortsize[rtype][_Cif_filetbl[cifd].version],
					   1, fd) != 1) IO_ERROR;

				(void) _Cif_binary_map_version(rtype, _cif_map_buffer, cr);

				if ((n = _Cif_binread (cifd, rtype, cr, fd)) < 0)
				  	return (n);
				cr = (struct Cif_generic *)((char *)cr +
					_Cif_structsize[rtype][_Cif_filetbl[cifd].return_version]);
			      }
			}

		      }


		/*
		 * The cif_callsite record has a link field which points at the
		 * next callsite matching this entry point
		 */

		if (rtype == CIF_CALLSITE) {

		  struct Cif_callsite *cs = (struct Cif_callsite *) *rptr;
		  int i;

		  for (i = 0; i < nrecords - 1; i++) {
		    if (cs[i].entryid == cs[i+1].entryid)
		      cs[i].link = &cs[i+1];
		  }

		}
	    }
	return (nrecords);
    }

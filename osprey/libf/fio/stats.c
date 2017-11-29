/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/stats.c	92.1	06/21/99 10:37:55"

/*
 *	stats.c		Fortran I/O statistics support routines.  This
 *			code is common between the CRAY Y-MP and the 
 *			CRAY-2.  Any changes to one copy should also be
 *			made in the other.
 */
#if	!defined(_CRAYMPP) && defined(_UNICOS)
#define _STATS_ENABLED
#endif

#ifdef	_STATS_ENABLED
extern int _pstatfd; 		/* file descriptor of stat pipe */
#endif
  
#include <errno.h>
#if !defined(_ABSOFT)
#include <malloc.h>
#else
#include <stdlib.h>
#endif
#include <string.h>
#include <unistd.h>
#include "fio.h"

/*
 * _ft_stopen	- Initialize unit fields associated with statistics gathering.
 *		  Allocate a close packet. Send Fortran open packet to procstat.
 *		  This should be the last routine called during open processing
 *		  to ensure that an open packet is sent only when an open 
 *		  succeeds.
 *
 * Return value:  0 on normal exit, -1 with errno set on error.
 */
int _ft_stopen(
	unit	*cup,	/* unit pointer */
	char	*atstr)	/* assign attributes associated with the file or unit */
{
#ifndef	_STATS_ENABLED
				/* No procstat support for MPP or SPARC yet */
	return(0);		/* normal return */
#else
	unum_t		unum;
	int		fd;		/* system file descriptor number */
	int		pktsiz;
	int		limit;
	int		len_name;
	int		len_attr;
	char		*cp;
	char		*file_name;
	union stat_ntry	*sp;
	union stat_ntry	open_packet;

	if (cup == NULL)
		return(0);

	fd	= cup->usysfd;

	if (STDIN_FILENO <= fd && fd <= STDERR_FILENO)
		return(0);	/* no Fortran stats for standard files for now*/

	file_name	= (cup->ufnm != NULL ? cup->ufnm : "(unnamed)");
	
	/*
	 * Initialize the open packet.
	 *
	 * Notice that pktsiz includes the null terminator for the filename
	 * since the size of the fname[1] field ensures room for an extra byte.
	 */
	len_name	= strlen(file_name);
	len_attr	= (atstr == NULL) ? 0 : strlen(atstr);
	pktsiz	= MIN(MAXPACK,
		     sizeof(open_packet.fopn_pkt) +
		     len_name + 1 + len_attr + 1);
	open_packet.fopn_pkt.h.pktsiz	= pktsiz;
	open_packet.fopn_pkt.h.pid	= getpid();
	open_packet.fopn_pkt.h.typ	= FOPEN_PACK;
	open_packet.fopn_pkt.h.tstmp	= _rtc();
	open_packet.fopn_pkt.h.rsv0	= 0;
	open_packet.fopn_pkt.h.rsv1	= 0;
	open_packet.fopn_pkt.h.rsv2	= 0;
	open_packet.fopn_pkt.h.rsv3	= 0;
	open_packet.fopn_pkt.unitnum	= cup->uid;
	open_packet.fopn_pkt.iotype	= IO_TYPE(cup);
	open_packet.fopn_pkt.fstruct	= _deduce_fstruct(cup->ufs,
					(struct fdinfo*)cup->ufp.fdc,cup->ufmt);
	open_packet.fopn_pkt.sys_fd	= fd;
	open_packet.fopn_pkt.smode	= 0;
	open_packet.fopn_pkt.recl	= cup->urecl;
	open_packet.fopn_pkt.rsv1	= 0;
	open_packet.fopn_pkt.rsv2	= 0;
	open_packet.fopn_pkt.rsv3	= 0;

	cp	= &open_packet.fopn_pkt.strings[0];
	limit	= pktsiz - (cp - (char*)&open_packet);

	if (len_name > limit - 1)
		len_name	= limit - 1;
	if (len_name >= 0) {	/* truncate name to max length of packet */
		(void)memcpy(cp, file_name, len_name);	/* copy file name */
		cp[len_name]	= '\0';
	}
	
	cp    += len_name + 1;		/* skip over name & null terminator */
	limit -= len_name + 1;

	if (len_attr > limit - 1)
		len_attr	= limit - 1;
	if (len_attr >= 0) {		/* don't copy of no room or no attrs */
		(void)memcpy(cp, atstr, len_attr);	/* copy assign attrs */
		cp[len_attr]	= '\0';
	}

	/* 
	 * Allocate and zero the close packet.
	 */
	if ((sp = (union stat_ntry *)malloc(sizeof(union stat_ntry))) == NULL) {
		errno	= FENOMEMY;
		return(-1);
	}
	cup->ftstat	= sp;
	memset((char*)sp, 0, sizeof(*(cup->ftstat)));

	/*
	 * Now send open packet.  The open has succeeded.
	 */
	(void)write(	_pstatfd,
			(char*)&open_packet,
			open_packet.fopn_pkt.h.pktsiz);

	return(0);		/* normal return */
#endif
}
/*
 * _ft_stclose	- Send a close packet at the end of close processing for
 *		  a particular unit.  Then deallocate the close packet.
 *
 * Return value:  0 on normal exit, -1 with errno set otherwise.
 */
int _ft_stclose(unit *cup)
{
#ifndef	_STATS_ENABLED
				/* No procstat support for MPP or SPARC yet */
	return(0);		/* normal return */
#else
	int		pktsiz;
	char		*file_name;
	union stat_ntry *sp;

	if (cup->ftstat == NULL)
		return(0);

	file_name	= (cup->ufnm != NULL ? cup->ufnm : "(unnamed)");

	/*
	 * Fill in the close packet.
	 */
	sp			= cup->ftstat;
	pktsiz			= sizeof(sp->fcls_pkt);
	sp->fcls_pkt.h.pktsiz	= pktsiz;
	sp->fcls_pkt.h.pid	= getpid();
	sp->fcls_pkt.h.typ	= FCLOSE_PACK;
	sp->fcls_pkt.h.tstmp	= _rtc();
	sp->fcls_pkt.h.rsv0	= 0;
	sp->fcls_pkt.h.rsv1	= 0;
	sp->fcls_pkt.h.rsv2	= 0;
	sp->fcls_pkt.h.rsv3	= 0;
	sp->fcls_pkt.unitnum	= cup->uid;
	sp->fcls_pkt.iotype	= IO_TYPE(cup);
        sp->fcls_pkt.sys_fd	= cup->usysfd;
	sp->fcls_pkt.smode	= 0;
	sp->fcls_pkt.rsv1	= 0;
	sp->fcls_pkt.rsv2	= 0;
	sp->fcls_pkt.rsv3	= 0;
	sp->fcls_pkt.rsv4	= 0;
	sp->fcls_pkt.rsv5	= 0;
	sp->fcls_pkt.rsv6	= 0;
	sp->fcls_pkt.rsv7	= 0;

	/*
	 * fname gets the first 15 characters of the file name.  The 
	 * complete file name can be found in the open packet.
	 */
	(void) strncpy(&sp->fcls_pkt.fname[0], file_name, 15);
	sp->fcls_pkt.fname[15]	= '\0';

	/*
	 * Now send the close packet, and then deallocate it.
	 */
	(void)write(_pstatfd, (char*)sp, sp->fcls_pkt.h.pktsiz);
	free(sp);
	cup->ftstat	= NULL;
	
	return(0);
#endif
}

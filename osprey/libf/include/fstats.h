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


/* USMID @(#) libf/include/fstats.h	92.0	10/08/98 14:30:10 */


#ifndef __FSTATS_H_
#define __FSTATS_H_
  
/*
 *	fstats.h	definitions used for Fortran I/O statistics 
 *			gathering.
 */

#if	defined(_CRAYMPP) || !defined(_UNICOS)

#define FORTSTATS	0
#define FSTATS_POST(_U,_IO,_FIOSP) ;

#else	

#define FORTSTATS	(_PSFLAGS(_PS_F_MASK))
					/* Indicate whether Fortran I/O 
					 * statistics are to be gathered.  If 
					 * this flag definition is altered, 
					 * S@PSFORT in asdef (YMP only) must be
					 * altered as well. */

/*
 *	FSTATS_POST
 *
 *		Record statistics which have been gathered during
 *		a particular Fortran I/O statement.  
 *
 *		_U 	- pointer to unit. 
 *		_IO	- I/O statement code orTF_WRITE or TF_READ
 *		_FIOSP	- FIOSPTR for 
 *
 */

#define FSTATS_POST(_U,_IO,_FIOSP){\
	union stat_ntry *ft;\
\
	if (FORTSTATS && (_U) != NULL && (ft = ((unit *)_U)->ftstat) != NULL){\
		/*\
		 * Gather statistics if unit is open and is associated with\
		 * a disk file.\
	 	 */\
		long  init_rt;	/* real time clock value at start of I/O */\
		long  rt;\
		init_rt = _FIOSP->f_rtbgn;\
		rt  	= _rtc() - init_rt;\
\
		if ((_IO) & TF_WRITE) \
			POST_IT(write)\
		else if ((_IO) & TF_READ)\
			POST_IT(read)\
		else {\
			switch (_IO) {\
\
			case T_BUFOUT:		POST_IT(bufout)		break;\
			case T_BUFIN:		POST_IT(bufin)		break;\
			case T_REWIND:		POST_IT(rewind)		break;\
			case T_BACKSPACE:	POST_IT(backspace)	break;\
			case T_ENDFILE:		POST_IT(endfile)	break;\
			case T_LENGTH:		POST_IT(length)		break;\
			case T_UNIT:		POST_IT(unit)		break;\
			case T_FLUSH:		POST_IT(flush)		break;\
			case T_GETPOS:		POST_IT(getpos)		break;\
			case T_SETPOS:		POST_IT(setpos)		break;\
			case T_CLOSE:		POST_IT(close)		break;\
			default:\
				break;	/* no stats gathered for other types */\
			}\
		}\
        }\
}

#define POST_IT(STMT) { \
	ft->fcls_pkt.STMT.num    += 1;\
	ft->fcls_pkt.STMT.rtc_tm += rt;\
}
#endif	/* defined(_CRAYMPP) || !defined(_UNICOS) */
#endif	/* _FSTATS_H_ */

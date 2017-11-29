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


#pragma ident "@(#) libu/ffio/trcprint.c	92.1	06/29/99 13:16:47"

#include <stdio.h>
#include <ffio.h>
#include "trcio.h"

#ifdef	_CRAY1
#pragma _CRI taskcommon  _trc_lyr_nest
#endif
int _trc_lyr_nest;

#define	MAX_NEST	20
static char blanks[MAX_NEST+1] = "                    "; /* MAX_NEST blanks */

#define MIN(a,b) ((a)<(b)?(a):(b))

static char *name_tab[] =
		{
		"???",
		"ffopen",
		"ffread",
		"ffreada",
		"ffreadc",
		"ffwrite",
		"ffwritea",
		"ffwritec",
		"ffclose",
		"ffflush",
		"ffweof",
		"ffweod",
		"ffseek",
		"ffbksp",
		"ffpos",
		"ffalgn",
		"fffcntl",
		"fflistio",
		};

/*
 * trace printing stuff
 */

_trc_enter(fio, opcd)
struct fdinfo *fio;
int opcd;
	{
	int i;
	char *buf, *op;
	struct trace_f *trc_info;

	op = name_tab[opcd];
	trc_info = (struct trace_f *)fio->lyr_info;
	buf = &trc_info->prtbuf[trc_info->bufptr];
	i = sprintf(buf, "%2d %*.sTRCE: %s ",
			_trc_lyr_nest, MIN(_trc_lyr_nest,MAX_NEST), blanks, op);
	_trc_lyr_nest++;
        trc_info->bufptr += i;
	return(0);
	}


/*
 * _trc_simple_exit is used if we don't have return status
 */
_trc_simple_exit(fio)
struct fdinfo *fio;
	{
	int i;
	char *buf;
	struct trace_f *trc_info;

	trc_info = (struct trace_f *)fio->lyr_info;

	buf = &trc_info->prtbuf[trc_info->bufptr];
	if (buf[-1] != '\n') {
		*buf++ = '\n';
		trc_info->bufptr++;
	}

	_trc_lyr_nest--;

        write(trc_info->trcfd, trc_info->prtbuf, trc_info->bufptr);
	trc_info->bufptr = 0;
	return(0);
	}

_trc_exit(fio, ret, stat)
struct fdinfo *fio;
int ret;
struct ffsw *stat;
	{
	int i;
	char *buf;
	struct trace_f *trc_info;

	trc_info = (struct trace_f *)fio->lyr_info;

	buf = &trc_info->prtbuf[trc_info->bufptr];
	if (buf[-1] != '\n') {
		*buf++ = '\n';
		trc_info->bufptr++;
	}

	_trc_lyr_nest--;
	i = sprintf(buf, "%2d %*.sTRCX:	ret=%d, stat=%d, err=%d\n",
			_trc_lyr_nest, MIN(_trc_lyr_nest,MAX_NEST), blanks,
			ret, stat->sw_stat, stat->sw_error);

        write(trc_info->trcfd, trc_info->prtbuf, trc_info->bufptr + i);
	trc_info->bufptr = 0;
	return(0);
	}
/*
 * Print info for read/write words
 */
_trc_pr_rww(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp;
bitptr bufptr;
struct ffsw *stat;
int *ubc;
	{
	_trc_info(fio, "(fd /* %d */, &memc[%d], %d, &statw[%d], ",
		fio, BPTR2CP(bufptr), nbytes, (long *)stat);

	PFULP(fio, fulp);
	_trc_info(fio, ", & conubc[%d]);\n", *ubc);
	}

/*
 * Print info for read/write chars
 */
_trc_pr_rwc(fio, bufptr, nbytes, stat, fulp)
struct fdinfo *fio;
int nbytes, fulp;
bitptr bufptr;
struct ffsw *stat;
	{
	_trc_info(fio, "(fd /* %d */", fio);
	_trc_info(fio, "&memc[%d], ",BPTR2CP(bufptr));
	_trc_info(fio, "%d, ",nbytes);
	_trc_info(fio, "&statw[%d], ", (long *)stat);

	PFULP(fio, fulp);
	_trc_info(fio, ")\n",0);
	}


/*
 * Print info for calls with 2 parameters
 */
_trc_pr_2p(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	_trc_info(fio, "(fd /* %d */", fio);
	_trc_info(fio, "&statw[%d])\n", stat);
	}

/*
 * Print statistics
 */
_trc_stats(trc_info)
struct trace_f *trc_info;
	{
	int i;
	char *buf;

	buf = &trc_info->prtbuf[trc_info->bufptr];
	i = sprintf(buf, "STATS:\n");
        trc_info->bufptr += i;
	buf += i;
	i = sprintf(buf, "\tforward sequential: %d, \n", trc_info->forw);
        trc_info->bufptr += i;
	buf += i;
	i = sprintf(buf, "\tbackward sequential: %d\n", trc_info->backw);
        trc_info->bufptr += i;
	buf += i;
	i = sprintf(buf, "\tunknown: %d\n", trc_info->unknown);
        trc_info->bufptr += i;
	buf += i;
	i = sprintf(buf,"\tread: %d\twrite:%d\treada:%d\twritea:%d\tlseek:%d\n",
		trc_info->reads,
		trc_info->writes,
		trc_info->readas,
		trc_info->writeas,
		trc_info->lseeks
		);
        trc_info->bufptr += i;
	buf += i;
	return(0);
	}

_trc_info(fio, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
struct fdinfo *fio;
char *str;
int arg1, arg2, arg3, arg4, arg5, arg6, arg7;
	{
	int i;
	char *buf;
	struct trace_f *trc_info;

	trc_info = (struct trace_f *)fio->lyr_info;
	buf = &trc_info->prtbuf[trc_info->bufptr];
	i = sprintf(buf, str, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
	trc_info->bufptr += i;
	return(0);
	}

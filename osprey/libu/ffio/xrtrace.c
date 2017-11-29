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


#pragma ident "@(#) libu/ffio/xrtrace.c	92.1	06/29/99 13:16:47"

#include <stdio.h>
#include <errno.h>
#include <ffio.h>
#include <cray/mtlock.h>

#if DEBUG
static char *_get_rname();
#endif

/*
 * xrtrace()
 *	debug the calls through the chain and provide for debug hooks.
 */

#if __STDC__ && _RELEASE >= 3
#pragma _CRI duplicate _xrtrace as $xrtrace
#else
#error no pragma duplicate available
#endif

#define MSG	&msgbuf[strlen(msgbuf)]

_xrtrace(fio, routine, str, aparm0, aparm1, aparm2, aparm3, aparm4,
aparm5, aparm6, aparm7, aparm8)
struct fdinfo *fio;
int (* routine)();
char *str;
long aparm0;	/* arglist word 1 */
long aparm1;	/* arglist word 2 */
long aparm2;	/* ... */
long aparm3;	
long aparm4;
long aparm5;
long aparm6;
long aparm7;
long aparm8;
	{
#define MAXPARM 9
	int ret,narg, i;
	long ap[MAXPARM];
	char *estr, *getenv(), *rname;
	char msgbuf[256];

	msgbuf[0] = '\0';
	rname = str;
	if (__fdctrace_enable < 0)
		{
		if ((estr = getenv("FDCTRACE")) == NULL)
			__fdctrace_enable = 0;
		else
			{
			__fdctrace_enable = atol(estr);
			}
		}

	if (fio->magic != MAGIC_ID) abort();

	NUMARG(narg);

	switch(narg-3)	/* note the fall-throughs */
		{
		case 9:	ap[8] = aparm8;
		case 8:	ap[7] = aparm7;
		case 7:	ap[6] = aparm6;
		case 6:	ap[5] = aparm5;
		case 5:	ap[4] = aparm4;
		case 4:	ap[3] = aparm3;
		case 3:	ap[2] = aparm2;
		case 2:	ap[1] = aparm1;
		case 1:	ap[0] = aparm0;	
		}

	if (FDCTRACE(TRACE_CALLS))
		{
#if _CRAY1 && DEBUG
		rname = _get_rname(routine, str);
#endif
		sprintf(MSG,"_xrcall: { %s (%x", rname, aparm0);
/*
 *		Print out the arguments.  All of them, and only those really passed.
 */
		for (i = 1; i < narg-3 ; i++)
			{
			/* only read/write has 6 params */
			if ((narg-3) == 6)
				{
				if (i == 1) /* arg 2 */
					{
					bitptr val;

					SET_BPTR(val, ap[i]);
					sprintf(MSG, ", wa=%x bo=%x",
						BPTR2WP(val),BPBITOFF(val));
					}
				else if (i == 5) /* arg 6 */
					sprintf(MSG, ", ubc=%x@%x",
						*(long *)ap[i], ap[i]);
				else
					sprintf(MSG, ", %x", ap[i]);
				}
			else
				sprintf(MSG, ", %x", ap[i]);
			}
		sprintf(MSG,"); fio@%x\n",fio);
		_xrt_putstr(msgbuf);
		}
/*
 *	Call the routine with the correct number of arguments
 */
	switch(narg-3)
		{
		case 1:	ret = routine(ap[0]);
			break;
		case 2:	ret = routine(ap[0],ap[1]);
			break;
		case 3:	ret = routine(ap[0],ap[1],ap[2]);
			break;
		case 4:	ret = routine(ap[0],ap[1],ap[2],ap[3]);
			break;
		case 5:	ret = routine(ap[0],ap[1],ap[2],ap[3],ap[4]);
			break;
		case 6:	ret = routine(ap[0],ap[1],ap[2],ap[3],ap[4],ap[5]);
			break;
		case 7:	ret = routine(ap[0],ap[1],ap[2],ap[3],ap[4],ap[5],ap[6]);
			break;
		case 8:	ret = routine(ap[0],ap[1],ap[2],ap[3],ap[4],ap[5],ap[6],ap[7]);
			break;
		case 9:	ret = routine(ap[0],ap[1],ap[2],ap[3],ap[4],ap[5],ap[6],ap[7],ap[8]);
			break;
		default: sprintf(MSG,
				"xrtrace: internal error, params=%d.\n",
				narg-3);
			abort();
			break;
		}
	if (FDCTRACE(TRACE_CALLS))
		{
		msgbuf[0] = '\0';
		if ((narg-3) == 6)
			{
			struct ffsw *sp;
			sp = (struct ffsw *)ap[3];
			sprintf(MSG,
				"_xrrtrn: \t\t%s, ret=%x, ubc=%x@%x, stat=%x,%x,%x,%x }\n",
				rname, ret, *(long *)ap[5], ap[5],sp->sw_flag,
				sp->sw_error, sp->sw_count, sp->sw_stat);
			}
		else
			sprintf(MSG,
				"_xrrtrn: \t\t%s, ret=%x, errno=%x }\n",
				rname, ret, errno);
		_xrt_putstr(msgbuf);
		}
	
	return (ret);
	}

_xrt_putf(str,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13)
char *str;
int v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13;
	{
	char msgbuf[256];
	sprintf(msgbuf,str,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13);
	_xrt_putstr(msgbuf);
	}

_xrt_putstr(str)
char *str;
	{
	if (MULTI_ON)
		{
		fflush(stdout);
		write(1, str, strlen(str));
		}
	else
		fputs(str, stdout);
	}

#if _CRAY1 && DEBUG

/*
 *	This routine cracks the instructions in the routine pointed
 *	to by the routine function pointer.  It tries to find the
 *	name of the routine in the TNB and returns it.
 *	ostr is a default return value if the process goes wrong.
 */
static char *
_get_rname(routine, ostr)
int (* routine)();
char *ostr;
	{
	long *lim, *lp, lword, *sbreak();
	int opcode, ctr, tmp;
	long wa, bitlen, bitoff, nlen;
	char *str;

#define MAXINS	4	/* only allow lookahead in the instructions */
			/* in the calling */
			/* sequence of the first four instructions. */

	str = ostr;
/* UGLY !!! get the name of the routine to be called !!! */
	bitoff = ((long)routine & 3)*16;/* extract bit offset in word */
					/* from parcel address */
	wa = (long)routine >> 2;	/* word address of code */
	ctr = 0;
	opcode = 0;
	while ((opcode < 010 || opcode > 022) && ctr < MAXINS)
		{
		ctr++;
		lword = 0;
#if ! _ADDR32	/* XMP */
		bitlen = 32;			/* len of instruction */
		tmp = 32;
		/* get instruction ! */
		MOVBITZ(wa, &bitoff, &bitlen, &lword, &tmp);
		opcode = lword >> 25;
		lword = lword & 0xffffff; /* extract address */
#else	/* YMP */
		bitlen = 48;			/* len of instruction */
		tmp = 16;
		/* get instruction ! */
		MOVBITZ(wa, &bitoff, &bitlen, &lword, &tmp);
		opcode = lword >> (25 + 16);
		lword = (((lword >> 16) & 0xffff) | (lword << 16)) & 0xffffffff;
#endif
		bitoff += 16;	/* ONLY handle one INSRTMAC load */
		}
	lim = sbreak(0);
/* only if we found an 'A0 exp' */
	lp = (long *)lword; /* 24 bits, on X.  32 on Y. masked above */
	if ((opcode >= 010 && opcode <= 022) && lim > lp)
		{
		lp = (long *)lp;	/* get word pointed to by address */
					/* field of instruction */
		if (*lp>>32 == 0) /* check out TNB 32bits of ZERO */
			{
			nlen = *lp & 0xffff;
			str = (char *)((int)lp - (nlen-1)/8-1);
			}
		else
			{
			printf("OLD TRBK!  \n");
			nlen = 7;
			}
		}
/* UGLY !!! get the name of the routine to be called !!! */

	return(str);
	}
#endif

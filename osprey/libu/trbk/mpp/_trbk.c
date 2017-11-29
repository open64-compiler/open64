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


#pragma ident "@(#) libu/trbk/mpp/_trbk.c	92.1	07/01/99 13:49:30"
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <cray/mtlock.h>
#include <cray/signal.h>
#include <cray/stk.h>
#ifdef	_UNICOS_MAX
typedef struct sigctx ctx_t;
#else
#include <ucontext.h>
typedef struct ucontext ctx_t;
#endif

#define	DEFDEPTH	25	/* Default traceback depth */

int	_trbkdpth = DEFDEPTH;	/* Default traceback depth */

static DECL_LOCK(threadlock)	/* lock words for routine */
static DECL_MPP_LOCK(pelock)

extern	struct DSIB	*_get_fp();

static char	*abtmsg	= " Traceback aborted; possible stack corruption.\n";
static char	*bgnmsg	= "\n Beginning of Traceback (PE %d):\n";
static char	*endmsg	= " End of Traceback.\n";
static char	*termsg	= " Traceback terminated; maximum depth exceeded.\n";

/*
 *	_trbk(filed, sigsav)
 *
 *	Writes a list of all subprograms active in the current calling
 *	tree.  Input is an optional file descriptor to which is list is
 *	to be written and an optional frame pointer denoting the point
 *	at which to start the traceback.  _trbk() accepts the following
 *	(optional) parameters:
 *
 *	filed	File descriptor
 *	sigsav	Address of signal save area
 *
 *	If the file descriptor is not specified, then the traceback
 *	report is written to stderr.
 *
 *	If the signal save area is not specified, then traceback begins
 *	at the routine which called _trbk().
 *
 *	The maximum depth of the traceback is defined in the memory
 *	location: _trbkdpth.  The default depth is 25.
 *
 *	The return status of _trbk() is one of the following:
 *
 *	+1	_trbk() terminated normally after tracing back through
 *		the startup routine.
 *
 *	 0	_trbk() terminated prematurely after the maximum
 *		traceback depth was exceeded.
 *
 *	-1	_trbk() terminated abnormally, possibly due to
 *		corruption of the stack.
 */

int
_trbk(
	int			fd,	/* File to write traceback information */
	struct sigarea_lib	*sa	/* Address of signal save area */
)
{
	register short	i, j;	/* Temporaries */
	register int	stat;	/* Return status */
	char	*lastmsg;	/* Pointer to last message */
	char	*init;		/* Initial word of message */
	char	buffer[200];	/* Traceback line buffer */
	struct DSIB	d1, d2;	/* Dummy frame packages */
	struct DSIB	*fp;	/* Frame pointer */

	if (_numargs() < 1)
		fd	= fileno(stderr);

	if (_numargs() < 2) {
		init	= "Called from";
		fp	= _get_fp();
	}
	else {
		register short	needle;	/* Needle-level flag */
		register int	pc;	/* Address of interrupt */
		ctx_t		*ctx;	/* Pointer to register save area */
		union whatever {
			int		temp;
			struct ci_word	ci;
		}	w;

		init	= "Interrupt at";
		ctx	= (ctx_t *) sa->context;

		w.temp	= 0;
		d2.ssib	= (struct SSIB *) NULL;
		d2.ssp	= (void *) NULL;

#ifdef	_UNICOS_MAX
		needle	= ((((long) ctx->fp) & 07) == 1);
		d2.fp	= (void *) (ctx->fp & ~07);
		pc	= ctx->pc;
#else
		needle	= ((ctx->uc_mcontext.gregs[15] & 07) == 1);
		d2.fp	= (void *) (ctx->uc_mcontext.gregs[15] & ~07);

		/* UNICOS/mk saves the program counter in register 31 */

		pc	= ctx->uc_mcontext.gregs[31];
#endif
		pc	= pc - 4;	/* Back up one instruction */
		d2.ra	= (void *) pc;	/* Interrupt address */
#ifdef	_CRAYT3D
		d2.ci	= w.ci;
#else
		d2.pfp	= (void *) NULL;
		d2.ciwi	= w.temp;
#endif

		fp	= &d2 + 1;

		if (needle) {

			d1.ssib	= (struct SSIB *) NULL;
			d1.ssp	= (void *) NULL;
			d1.fp	= d2.fp;
			d2.fp	= &d1 + 1;
#ifdef	_UNICOS_MAX
			d1.ra	= (void *) ctx->ra;
			w.temp	= ctx->ci;
#else
			d1.ra	= (void *) ctx->uc_mcontext.gregs[26];
			w.temp	= ctx->uc_mcontext.gregs[25];
#endif
#ifdef	_CRAYT3D
			d1.ci	= w.ci;
#else
			d1.pfp	= (void *) NULL;
			d1.ciwi	= w.temp;
#endif
		}

		fp	= &d2 + 1;
	}

	fp	= fp - 1;
	lastmsg	= endmsg;
	stat	= 1;

	if (_trbkdpth < 0 || _trbkdpth >= 1000)	/* Sanity check */
		_trbkdpth	= DEFDEPTH;

	j	= _trbkdpth;

	(void) sprintf(buffer, bgnmsg, _my_pe());

	MEM_LOCK(&threadlock)
	MPP_LOCK(&pelock)

	(void) write(fd, buffer, strlen(buffer));

	for (i = 0; i < j; i++) {
		register long	raddr;		/* Address of call site */
#ifndef	_CRAYT3D
		register int	index;		/* Index to ci word */
#endif
		register int	len;		/* Length of traceback line */
		register int	lineno;		/* Line number of call site */
		register char	*name;		/* Pointer to routine name */

		name	= "???";	/* Assume unknown routine */

#ifdef	_CRAYT3D
		lineno	= fp->ci.lineno;
#else
		index	= fp->ciwi;
		lineno	= 0;		/* Assume line number is unknown */
#endif

		raddr	= (long) fp->ra;	/* Return (interrupt) address */
		raddr	= raddr - 4;		/* Back up one instruction */
		fp	= (struct DSIB *) fp->fp;

		if (fp == (struct DSIB *) NULL)
			goto done;

		fp	= fp - 1;

		/* Perform some minimal sanity checks */

		if ((long) fp->ssib < 0 || raddr <= 0 || (long) fp < 0) {
			lastmsg	= abtmsg;
			stat	= -1;
			goto done;
		}

		if (fp->ssib != (struct SSIB *) NULL) {	/* If we have an SSIB */

			if (fp->ssib->SSIB_ver != SSIB_VERSION) {
				lastmsg	= abtmsg;
				stat	= -1;
				goto done;
			}

			name	= (char *) fp->ssib + fp->ssib->SSIB_len;

#ifndef	_CRAYT3D
			if (index != 0)		/* If we have a ci word */
				lineno	= ((struct ci_word *) (fp->ssib))[index].lineno;
#endif

		}

		if (lineno == 0)
			len	= sprintf(buffer,
					"  %s address 0x%x in routine '%s'.\n",
					init, raddr, name);
		else
			len	= sprintf(buffer,
					"  %s line %d (address 0x%x) in routine '%s'.\n",
					init, lineno, raddr, name);

		(void) write(fd, buffer, len);

		init	= "Called from";	/* First word of subsequent messages */
	}

	stat	= 0;
	lastmsg	= termsg;

done:
	(void) write(fd, lastmsg, strlen(lastmsg));

	MPP_UNLOCK(&pelock)
	MEM_UNLOCK(&threadlock)

	return(stat);
}

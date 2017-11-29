/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include "cmplrs/host.h"

void
__argmnt( int64 *__vcnt, int32 *chararg1, int32 *nargs )
{
    /* 
    The last 10-bit of __vcnt contains the number of argument passed, not
    counting the string lengths.  The string lengths 'lcnt' are passed
    by:  __vcnt = nargs + (lcnt << 10)
    */
    *nargs = (int) (*chararg1 ? (*__vcnt&1023) : ((*__vcnt&1023) + (*__vcnt>>10)));
}

void
__getadr( int64 *__vcnt, int32 *n, long *iaddr)
{
    int acnt = *__vcnt&1023;
    int lcnt = *__vcnt>>10;
    /*
    The frontend needs to call this with the address of __vaddr as the
    first parameter so this routine can find the following argument
    addresses passed on the parameter list
    */
    if (*n < 1 || *n > acnt + lcnt) {
	*iaddr = 0;
	return;
    }

    /* 
    Note that for 64-bit environment iaddr must always correspond to
    an INTEGER*8 argument.  However, the actual character length parameters
    at the end of the list are passed as INTEGER*4 always so we need to
    distinguish the two cases here
    */
    /* Gettting the address of the actual arguments */
    if (*n <= acnt) {
	    /* 
	    For new 32-bit ABI needs the 4-byte adjustment so that
	    the 4-byte address can be obtained from the 8-byte 
	    argument slot
	    */
	*iaddr = *(long *) ((char *) __vcnt + ((*n) * 8 +
	    (_MIPS_SIM == _MIPS_SIM_NABI32 ? 4 : 0)));
	return;
    }
    /* Getting the length of the character arguments.  Since these are
    ** always 32-bit long regardless of the pointer size we need to
    ** force it to be of type int32 */
    *(int32 *) iaddr = *(int32 *) ((char *) __vcnt + ((*n) * 8 +
	    (_MIPS_SIM == _MIPS_SIM_NABI32 || _MIPS_SZPTR == 64 ? 4 : 0)));
}


void
__getcln( int64 *__vcnt, int32 *n, int32 *iclen)
{
    int acnt = *__vcnt&1023;
    int lcnt = *__vcnt>>10;
    /*
        'n' is the Nth character argument is the list and is not the Nth 
	argument in general.
    */
    if (*n < 1 || *n > lcnt) {
	*iclen = 0;
	return;
    }

    /* Getting the 4-byte length of the character arguments */
    *iclen = *(int32 *) ((char *) __vcnt 
	     + (4 + (acnt+*n) * 8
	     + (_MIPS_SIM == _MIPS_SIM_NABI32 || _MIPS_SZPTR == 64 ? 4 : 0)));
}


int32
__nullok( int64 *__vcnt, int32 *n, long *iaddr)
{
    int acnt = *__vcnt&1023;
    int lcnt = *__vcnt>>10;
    /*
    The frontend needs to call this with the address of __vaddr as the
    first parameter so this routine can find the following argument
    addresses passed on the parameter list
    */
    if (*n < 1 || *n > acnt) {
	return(*iaddr != 0);
    }

    /* 
    Note that for 64-bit environment iaddr must always correspond to
    an INTEGER*8 argument.  However, the actual character length parameters
    at the end of the list are passed as INTEGER*4 always so we need to
    distinguish the two cases here
    */
    /* Gettting the address of the actual arguments */
    if (*n <= acnt) {
	    /* 
	    For new 32-bit ABI needs the 4-byte adjustment so that
	    the 4-byte address can be obtained from the 8-byte argument
	    slot
	    */
	return(*iaddr != *(long *) ((char *) __vcnt + 
		(*n * 8 + 
	        (_MIPS_SIM == _MIPS_SIM_NABI32 ? 4 : 0))));
    }
    /* Getting the length of the character arguments */
    return(*(int32 *)iaddr != *(int32 *) ((char *) __vcnt + (*n * 8+ 
	    (_MIPS_SIM == _MIPS_SIM_NABI32 || _MIPS_SZPTR == 64 ? 4 : 0))));
}


void 
__xetarg( int64 *__vcnt, int32 *n, int32 *len, void *iarg)
{
    long iaddr;
    __getadr( __vcnt, n, &iaddr );
    memcpy( iarg, (char *) iaddr, *len );
}

void
__retour( int64 *__vcnt, int32 *nargs, ... )
{
    va_list ap;
    char *cval;
    long iaddr;
    int32 len, n;

    va_start(ap, __vcnt);
    nargs = va_arg( ap, int32 * );
    n = 0;
    while ((*nargs)--) {
	n++;
	if ((len = *(va_arg( ap, int32 *))) <= 0)
	   goto badarg;
	__getadr( __vcnt, &n, &iaddr );
	if ((cval = va_arg( ap, char * )) <= 0)
	    goto badarg;
	memcpy( (char *) iaddr, cval, len );
    }
    goto endsub;
badarg:
    fprintf( stderr, "Warning: Wrong arguments in RETOUR\n" );
endsub:
    va_end(ap);
}


/*	Everything after this point is used to support the objects produced
**	by fcom for the old 32-bit support.
*/

#include "comargs.h"
int32 comargs__ [COMARGSZ];


static int32 nlev = -1;
static int32 **comptr = NULL;
static int32 comptr_size = 0;

extern void check_vararg_error(void);
extern void s_abort(void);
#ifdef FTN90_IO
#include "fio.h"
#else
extern void f77fatal (int32, char *);
#endif

int32 nullok_( argno, adr )
    int32 *argno;
    int32 *adr;
{
    check_vararg_error();
    return( *adr != *( comptr[nlev] + *argno ) );
}



void getadr_(int32 *argno, int32 *argadr )
{
    check_vararg_error();
    *argadr = *( comptr[nlev] + *argno );
}



void argmnt_(int32 *narg )
{
    check_vararg_error();
    *narg = *comptr[nlev];
}


void xetarg_(int32 *argno, int32 *len, char *buff )
{
    check_vararg_error();
    memcpy( buff, (void *) *(comptr[nlev] + *argno), *len );
}


void set_varg_()
{
    int n;
    if (++nlev >= comptr_size)
    {
	comptr_size += 20;
	if (comptr_size == 20)
	    comptr = (int32 **) calloc( comptr_size, sizeof(int32) );
	else
	    comptr = (int32 **) realloc( comptr, comptr_size * sizeof(int32) );
    }
    n = comargs__[0]*8 + 4;	/* make sure that char lengths are copied */
    if (!(comptr[nlev] = (int32 *) malloc( n )))
#ifdef FTN90_IO
	_ferr( NULL, FENOMEMY );
#else
	f77fatal( 113, "vararg" );
#endif
    memcpy( comptr[nlev], comargs__, n );
}


void free_varg_()
{
    check_vararg_error();
    free(comptr[nlev--]);
}


void check_vararg_error(void)
{
    if (nlev < 0)
    {
	fprintf( stderr, "Compiler error in vararg\n" );
	s_abort();
    }
}


void retour_(int32 *arg1,  ... )
{
    va_list ap;
    char *cval;
    int32 *ip, *ival;
    int32 nargs, len, n;
    double *dp, *dval;

    va_start(ap, arg1);
    nargs = *arg1;
    n = 0;
    while (nargs--)
    {
	n++;
	if ((len = *(va_arg( ap, int32 *))) <= 0)
	    goto badarg;
	if (len == 4)
	{
	    ip = (int32 *) *(comptr[nlev] + n);
	    if (!(ival = va_arg( ap, int32 * )))
		goto badarg;
	    *ip = *ival;
	}
	else if (len == 8)
	{
	    dp = (double *) *(comptr[nlev] + n);
	    if (!(dval = va_arg( ap, double * )))
		goto badarg;
	    *dp = *dval;
	}
	else
	{
	    ip = (int32 *) *(comptr[nlev] + n);
	    if (!(cval = va_arg( ap, char * )))
		goto badarg;
	    memcpy( ip, cval, len );
	}
    }
    goto endsub;
badarg:
    fprintf( stderr, "Warning: Wrong arguments in RETOUR\n" );
endsub:
    va_end(ap);
}


void reset_comargs__()
{
    comargs__[0] = 0;
}

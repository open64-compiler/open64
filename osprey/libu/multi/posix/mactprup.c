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


#pragma ident "@(#) libu/multi/posix/mactprup.c	92.1	06/25/99 14:35:10"

#include "macdefs.h"
#include <stdio.h>
#include <stdlib.h>
#include <thread.h>

#if defined(ASSEMBLY)
#   undef ASSEMBLY
#endif
#include "mactdefs.h"


/*
 * Shared semaphore for all the routines in this module.
 */
static int              prup_sem = 0;


/*========================================
 * ISELFSCH_
 *
 * Do "ivar = (jvar = ivar) + 1", single-threaded.
 */
int
iselfsch_(ivar)
int                    *ivar;
{
    int                 jvar;

    _mact_psem(&prup_sem);                                              /*CRIT*/
    {                                                                   /*CRIT*/
        jvar   = *ivar;                                                 /*CRIT*/
        *ivar += 1;                                                     /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&prup_sem);                                              /*CRIT*/

    return(jvar);
}


/*========================================
 * ISELFADD_
 *
 * Do "ivar = (jvar = ivar) + ivalue", single-threaded.
 */
int
iselfadd_(ivar, ivalue)
int                    *ivar;
int                    *ivalue;
{
    int                 jvar;

    _mact_psem(&prup_sem);                                              /*CRIT*/
    {                                                                   /*CRIT*/
        jvar   = *ivar;                                                 /*CRIT*/
        *ivar += *ivalue;                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&prup_sem);                                              /*CRIT*/

    return(jvar);
}


/*========================================
 * ICRITADD_
 *
 * Do "ivar = ivar + ivalue", single-threaded.
 */
void
icritadd_(ivar, ivalue)
int                    *ivar;
int                    *ivalue;
{

    _mact_psem(&prup_sem);                                              /*CRIT*/
    {                                                                   /*CRIT*/
        *ivar += *ivalue;                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&prup_sem);                                              /*CRIT*/

    return;
}


/*========================================
 * ISELFMUL_
 *
 * Do "ivar = (jvar = ivar) * ivalue", single-threaded.
 */
int
iselfmul_(ivar, ivalue)
int                    *ivar;
int                    *ivalue;
{
    int                 jvar;

    _mact_psem(&prup_sem);                                              /*CRIT*/
    {                                                                   /*CRIT*/
        jvar   = *ivar;                                                 /*CRIT*/
        *ivar *= *ivalue;                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&prup_sem);                                              /*CRIT*/

    return(jvar);
}


/*========================================
 * ICRITMUL_
 *
 * Do "ivar = ivar * ivalue", single-threaded.
 */
void
icritmul_(ivar, ivalue)
int                    *ivar;
int                    *ivalue;
{

    _mact_psem(&prup_sem);                                              /*CRIT*/
    {                                                                   /*CRIT*/
        *ivar *= *ivalue;                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&prup_sem);                                              /*CRIT*/

    return;
}


/*========================================
 * XSELFADD_
 *
 * Do "xvar = (var = xvar) + xvalue", single-threaded.
 */
float
xselfadd_(xvar, xvalue)
float                  *xvar;
float                  *xvalue;
{
    float               var;

    _mact_psem(&prup_sem);                                              /*CRIT*/
    {                                                                   /*CRIT*/
        var    = *xvar;                                                 /*CRIT*/
        *xvar += *xvalue;                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&prup_sem);                                              /*CRIT*/

    return(var);
}


/*========================================
 * XCRITADD_
 *
 * Do "xvar = xvar + xvalue", single-threaded.
 */
void
xcritadd_(xvar, xvalue)
float                  *xvar;
float                  *xvalue;
{

    _mact_psem(&prup_sem);                                              /*CRIT*/
    {                                                                   /*CRIT*/
        *xvar += *xvalue;                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&prup_sem);                                              /*CRIT*/

    return;
}


/*========================================
 * XSELFMUL_
 *
 * Do "xvar = (var = xvar) * xvalue", single-threaded.
 */
float
xselfmul_(xvar, xvalue)
float                  *xvar;
float                  *xvalue;
{
    float               var;

    _mact_psem(&prup_sem);                                              /*CRIT*/
    {                                                                   /*CRIT*/
        var    = *xvar;                                                 /*CRIT*/
        *xvar *= *xvalue;                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&prup_sem);                                              /*CRIT*/

    return(var);
}


/*========================================
 * XCRITMUL_
 *
 * Do "xvar = xvar * xvalue", single-threaded.
 */
void
xcritmul_(xvar, xvalue)
float                  *xvar;
float                  *xvalue;
{

    _mact_psem(&prup_sem);                                              /*CRIT*/
    {                                                                   /*CRIT*/
        *xvar *= *xvalue;                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&prup_sem);                                              /*CRIT*/

    return;
}

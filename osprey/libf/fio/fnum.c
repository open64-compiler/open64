/*
Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.

Unpublished -- rights reserved under the copyright laws of the United
States. USE OF A COPYRIGHT NOTICE DOES NOT IMPLY PUBLICATION OR
DISCLOSURE. THIS SOFTWARE CONTAINS CONFIDENTIAL INFORMATION AND TRADE
SECRETS OF PATHSCALE, INC. USE, DISCLOSURE, OR REPRODUCTION IS
PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN PERMISSION OF PATHSCALE,
INC.

U.S. Government Restricted Rights:
The Software is a "commercial item," as that term is defined at 48
C.F.R. 2.101 (OCT 1995), consisting of "commercial computer software"
and "commercial computer software documentation," as such terms are used
in 48 C.F.R. 12.212 (SEPT 1995).  Consistent with 48 C.F.R. 12.212 and
48 C.F.R. 227-7202-1 through 227-7202-4 (JUNE 1995), all U.S. Government
End Users acquire the Software with only those rights set forth in the
accompanying license agreement. PathScale, Inc. 2071 Stierlin Court, Suite 200;
Mountain View CA 94043.

*/



#pragma ident "@(#) libf/fio/fnum.c	92.2	08/02/99 10:38:18"
 

#include <stdio.h>
#include <foreign.h>
#include <errno.h>
#include <liberrno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "fio.h"

#ifdef KEY /* Bug 1683 */
/* Don't pollute the Fortran namespace */

int pathf90_fnum(int *u)

#else

extern int fnum_(int *u);

int fnum_(int *u)

#endif /* KEY Bug 1683 */
{
        int             n, retval;
        unum_t          unum; 
        unit            *cup;
        struct stat     buf;
        struct fiostate cfs;
        int     stat;
        register int    errf;           /* ERR processing flag  */

        unum    = *u;
        retval  = -1;

        if (unum != 0 && unum != 5 && unum != 6)
          return retval;

        STMT_BEGIN(unum, 0, T_INQU, NULL, &cfs, cup);
                                                                                                                                                             
        if (cup == NULL && !GOOD_UNUM(unum))
                _ferr(&cfs, FEIVUNIT, unum);    /* invalid unit number */
                                                                                                                                                             
        if (cup == NULL) 
                 cup     = _imp_open(    &cfs,
                                         SEQ,
                                         FMT,
                                         unum,
                                         errf,
                                         &stat);

        retval      = fileno ( cup->ufp.std );
        STMT_END(cup, T_INQU, NULL, &cfs);      /* unlock the unit */
        return retval;
}


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


#pragma ident "@(#) libu/multi/posix/macttune.c	92.1	06/25/99 14:35:10"

#include "macdefs.h"
#include <stdio.h>
#include <stdlib.h>
#include <thread.h>

#if defined(ASSEMBLY)
#   undef ASSEMBLY
#endif
#include "mactdefs.h"


/*========================================
 * _MACT_VARARG_TSKTUNE
 *
 * Really do a TSKTUNE().  The tsktune_() entry point is in
 * mactutil.s, so we can handle the variable number of args.
 */
void
_mact_vararg_tsktune(argc, argv)
unsigned int            argc;           /* Number of arguments           */
void                   *argv[];         /* Vector of arguments           */
{
    unsigned int       *argv_lens;      /* Vector of string lengths      */
    unsigned int        value;          /* Value associated with keyword */

    /*
     * The argument list is of the form:
     *   kw1, val1, kw2, val2, ..., kwN, valN, kwlen1, kwlen2, ..., kwlenN
     * The argc count includes all of these, so for example if the user
     * called us with "TSKTUNE('STACKSZ', 1000)", we will get argc == 3
     * and argv pointing to "'STACKSZ', 1000, 7".  The argv_lens pointer
     * is used to get to the string lengths.
     */
    argc /= 3;
    argv_lens = (unsigned int *) &argv[2 * argc];
    for ( ; argc > 0; argc--, argv += 2, argv_lens += 1) {
        if (*argv_lens == 6) {
            if      (strncmp(((char *) *argv), "MAXCPU", 6) == 0) {
                /*
                 * This has no effect in the SPARC v1.0 release.
                 */
                0;
            }
            else if (strncmp(((char *) *argv), "SAMPLE", 6) == 0) {
                /*
                 * This has no effect in the SPARC v1.0 release.
                 */
                0;
            }
            else {
                fprintf(stderr, "Unrecognized scheduler parameter name\n");
                abort();
            }
        }
        else if (*argv_lens == 7) {
            if      (strncmp(((char *) *argv), "STACKSZ", 7) == 0) {
                /*
                 * Store the new macrotask stack size.  Thr_min_stack()
                 * is the minimum.  STACKSZ gives the size in words.
                 */
                value =   *(((unsigned int **) argv)[1])
                        * sizeof(unsigned int);

                _mact_psem(&_mact_stack_size_sem);                      /*CRIT*/
                {                                                       /*CRIT*/
                    if ((_mact_stack_size = thr_min_stack()) < value) { /*CRIT*/
                        _mact_stack_size = value;                       /*CRIT*/
                    }                                                   /*CRIT*/
                }                                                       /*CRIT*/
                _mact_vsem(&_mact_stack_size_sem);                      /*CRIT*/
            }
            else if (strncmp(((char *) *argv), "STACKIN", 7) == 0) {
                /*
                 * This has no effect in the SPARC v1.0 release.
                 */
                0;
            }
            else {
                fprintf(stderr, "Unrecognized scheduler parameter name\n");
                abort();
            }
        }
        else if (*argv_lens == 8) {
            if      (strncmp(((char *) *argv), "DBACTIVE", 8) == 0) {
                /*
                 * This has no effect in the SPARC v1.0 release.
                 */
                0;
            }
            else if (strncmp(((char *) *argv), "DBRELEAS", 8) == 0) {
                /*
                 * This has no effect in the SPARC v1.0 release.
                 */
                0;
            }
            else if (strncmp(((char *) *argv), "HOLDTIME", 8) == 0) {
                /*
                 * This has no effect in the SPARC v1.0 release.
                 */
                0;
            }
            else if (strncmp(((char *) *argv), "PRIORITY", 8) == 0) {
                /*
                 * This has no effect in the SPARC v1.0 release.
                 */
                0;
            }
            else if (strncmp(((char *) *argv), "STACKINW", 8) == 0) {
                /*
                 * This has no effect in the SPARC v1.0 release.
                 */
                0;
            }
            else if (strncmp(((char *) *argv), "STACKSZW", 8) == 0) {
                /*
                 * Store the new macrotask stack size.  Thr_min_stack()
                 * is the minimum.  STACKSZW gives the size in words.
                 */
                value =   *(((unsigned int **) argv)[1])
                        * sizeof(unsigned int);

                _mact_psem(&_mact_stack_size_sem);                      /*CRIT*/
                {                                                       /*CRIT*/
                    if ((_mact_stack_size = thr_min_stack()) < value) { /*CRIT*/
                        _mact_stack_size = value;                       /*CRIT*/
                    }                                                   /*CRIT*/
                }                                                       /*CRIT*/
                _mact_vsem(&_mact_stack_size_sem);                      /*CRIT*/
            }
            else {
                fprintf(stderr, "Unrecognized scheduler parameter name\n");
                abort();
            }
        }
    }

    /*
     * Done.
     */
    return;
}

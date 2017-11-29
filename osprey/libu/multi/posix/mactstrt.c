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


#pragma ident "@(#) libu/multi/posix/mactstrt.c	92.1	06/25/99 14:35:10"

#include "macdefs.h"
#include <stdio.h>
#include <stdlib.h>
#include <thread.h>

#if defined(ASSEMBLY)
#   undef ASSEMBLY
#endif
#include "mactdefs.h"


#if defined(DEBUG)
    /*========================================
     * MT_DUMP
     *
     * Dump data structures, for debugging purposes.
     *
     */
    void mt_dump_(void)
    {
        static char    *state_names[] =
                        {
                            "Nonexistent",
                            "Ready",
                            "Running",
                            "Lock_Wait",
                            "Event_Wait",
                            "Bar_Wait",
                            "Task_Wait",
                            "Complete",
                        };
        volatile mact_task_t
                       *mt;             /* Ptr to new macrotask descriptor  */
        int             i, lcpu, hash_entries;

        printf("========================================================\n");
        printf("MT INTERNAL DATA STRUCTURE DUMP\n");

        printf("\n");
        printf("_MACT_HASH\n");
        printf("==========\n");
        for (i = 0; i < MTHASH_SIZE; i++) {
            hash_entries = 0;
            for (mt = _mact_hash[i]; mt != NULL; mt = mt->mt_link) {
                hash_entries++;
            }
            if (hash_entries == 0) {
                continue;
            }
            printf("  _mact_hash[%2d], %d entr%s\n", i,
                   hash_entries, ((hash_entries == 1) ? "y" : "ies")
                  );

            for (mt = _mact_hash[i]; mt != NULL; mt = mt->mt_link) {
                printf("    %u:\n",                   mt               );
                printf("      mt_mtid        %10u\n", mt->mt_mtid      );
                printf("      mt_state     %12s\n",
                       state_names[(int) mt->mt_state]
                      );
                printf("      mt_stid        %10u ",  mt->mt_stid      );
                printf(" (%s hash chain)\n",
                       (  ((mt->mt_stid & (MTHASH_SIZE - 1)) == i)
                        ? "correct"
                        : "INCORRECT"
                       )
                      );
                printf("      mt_utid        %10u\n", mt->mt_utid      );
                printf("      mt_wait_obj    %10u\n", mt->mt_wait_obj  );
                printf("      mt_wait_link   %10u\n", mt->mt_wait_link );
                printf("      mt_wait_count  %10u\n", mt->mt_wait_count);
                printf("      mt_subr        %10u\n", mt->mt_subr      );
                printf("      mt_argc        %10u\n", mt->mt_argc      );
                printf("      mt_argv        %10u\n", mt->mt_argv      );
                printf("      mt_tcom        %10u\n", mt->mt_tcom      );
                printf("      mt_link        %10u\n", mt->mt_link      );
                printf("      mt_sem         %10u\n", mt->mt_sem       );
            }
        }

        printf("\n");
        printf("_MACT_FREE\n");
        printf("==========\n");
        printf("  mt_free ");
        for (mt = _mact_free; mt != NULL; mt = mt->mt_link) {
            printf("%10u\n           ", mt);
        }
        printf("%10u\n", mt);

        printf("========================================================\n");

        fflush(stdout);
    }
#endif


/*========================================
 * _MACT_VARARG_TSKSTART
 *
 * Really do a TSKSTART().  The tskstart_() entry point is in
 * mactutil.s, so we can handle the optional user arguments.
 */
void
_mact_vararg_tskstart(tca, subr, argc, argv)
unsigned int            tca[];          /* Task Control Array               */
void                  (*subr)();        /* Subroutine to macrotask          */
unsigned int            argc;           /* Number of arguments to subr()    */
void                   *argv[];         /* Vector of arguments for subr()   */
{
    volatile mact_task_t
                       *mt;             /* Ptr to my macrotask descriptor   */
    unsigned int        argv_in_use;    /* New task still using argv[]?     */

    /*
     * Maybe initialize the macrotask stack size.
     */
    if (_mact_stack_size == 0) {
        _mact_psem(&_mact_stack_size_sem);                              /*CRIT*/
        {                                                               /*CRIT*/
            if (_mact_stack_size == 0) {                                /*CRIT*/
                char           *p = getenv("MP_STACKSZW");              /*CRIT*/
                size_t          mp_stackszb;                            /*CRIT*/
                                                                        /*CRIT*/
                if (   p != NULL                                        /*CRIT*/
                    && sscanf(p, "%i", &mp_stackszb) == 1               /*CRIT*/
                   ) {                                                  /*CRIT*/
                    mp_stackszb <<= 2;      /* Words --> bytes */       /*CRIT*/
                }                                                       /*CRIT*/
                else {                                                  /*CRIT*/
                    mp_stackszb = 0x100000; /* 1 mbyte default */       /*CRIT*/
                }                                                       /*CRIT*/
                                                                        /*CRIT*/
                /*                                         */           /*CRIT*/
                /* Adjust for _start_mact()'s stack frame. */           /*CRIT*/
                /*                                         */           /*CRIT*/
                mp_stackszb += 324;                                     /*CRIT*/
                                                                        /*CRIT*/
                if (  (_mact_stack_size = thr_min_stack())              /*CRIT*/
                    < mp_stackszb                                       /*CRIT*/
                   ) {                                                  /*CRIT*/
                    _mact_stack_size = mp_stackszb;                     /*CRIT*/
                }                                                       /*CRIT*/
            }                                                           /*CRIT*/
        }                                                               /*CRIT*/
        _mact_vsem(&_mact_stack_size_sem);                              /*CRIT*/
    }

    /*
     * Account for the new macrotask.  We have to do the increment
     * here, rather than in _mact_add(), to prevent erroneously
     * reporting deadlock if the task that called us is the only
     * runnable one, and it goes on to wait on a lock (for example)
     * before the task we just created gets around to incrementing
     * the runnable task counter.  In addition, we have to do the
     * increment before we create the new task, so that we don't
     * end up with a similar situation if the new task goes off
     * and waits on a lock before we account for it.
     */
    INC_RUNNABLE_MACT_COUNT(1);

    /*
     * Get a macrotask descriptor and fill in most of it.
     * Put the address of our "argv is in use" flag in
     * mt_wait_obj; _mact_start_macrotask() will fill in
     * the rest of the descriptor and will clear the flag
     * when it's done.
     */
    argv_in_use       = 1;

    mt                = _mact_allocate();
    mt->mt_state      = TS_Ready;
    mt->mt_stid       = 0;
    mt->mt_utid       = (tca[0] < 3) ? 0 : tca[2];
    mt->mt_wait_obj   = (void *) &argv_in_use;
    mt->mt_wait_link  = NULL;
    mt->mt_wait_count = 0;
    mt->mt_subr       = subr;
    mt->mt_argc       = argc;
    mt->mt_argv       = argv;
    mt->mt_sem        = 0;

    /*
     * Start the macrotask.  The start-up code will take care of
     * adding the new descriptor to the proper hash list.  Wait
     * for the "argv in use" flag to clear, signifying that the
     * start-up code is done with the argv vector, before we
     * return and potentially trash that vector's memory (it's
     * in the frame owned by the original caller of TSKSTART()).
     */
    if (   thr_create(NULL, _mact_stack_size,
                      ((void *(*)(void *)) _mact_start_macrotask),
                      ((void *) mt),
                      (THR_DETACHED | THR_BOUND), NULL
                     )
        != 0
       ) {
        perror("Could not create thread for new macrotask");
        abort();
    }

    while (argv_in_use)
        ;

    /*
     * Done.  Put the address of the macrotask descriptor into
     * the TCA's second word, for future reference.
     */
    tca[1] = (unsigned int) mt;
    return;
}

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


#pragma ident "@(#) libu/multi/posix/mactbar.c	92.1	06/25/99 14:35:10"

#include "macdefs.h"
#include <stdio.h>
#include <stdlib.h>
#include <thread.h>

#if defined(ASSEMBLY)
#   undef ASSEMBLY
#endif
#include "mactdefs.h"


/*========================================
 * BARASGN_
 *
 * Assign a barrier.
 */
void
barasgn_(name, value)
void                   *name;           /* Where to put the barrier address */
unsigned int           *value;          /* Number of tasks needed to open   */
{
    mact_barrier_t     *new_barrier;    /* The new barrier                  */

    /*
     * Get the new barrier and initialize it.
     */
    if (   (new_barrier = (mact_barrier_t *) malloc(sizeof(mact_barrier_t)))
        == NULL
       ) {
        /*
         * Couldn't allocate any.  Halt the program.
         */
        perror("Could not get memory for new barrier in BARASGN");
    }

    new_barrier->mb_ocount = *value;
    new_barrier->mb_ncount = *value;
    new_barrier->mb_qhead  = NULL;
    new_barrier->mb_qtail  = NULL;
    new_barrier->mb_sem    = 0;

    /*
     * Give it to our caller, and we're done.
     */
    *((mact_barrier_t **) name) = new_barrier;

    return;
}


/*========================================
 * BARSYNC_
 *
 * Wait for a barrier to open.
 */
void
barsync_(name)
mact_barrier_t        **name;           /* Address of the barrier address   */
{
    mact_barrier_t     *barrier = *name;/* The barrier address              */
    volatile mact_task_t
                       *mt,             /* Ptr to some macrotask descriptor */
                       *my_mt;          /* Ptr to my macrotask descriptor   */
    unsigned int        release_count   /* Number of tasks we'll release    */
                        = 0;
    unsigned int        was_open;       /* Did we open the barrier?         */

    _mact_psem(&(barrier->mb_sem));                                     /*CRIT*/
    {                                                                   /*CRIT*/
        if (barrier->mb_ncount == 1) {                                  /*CRIT*/
            /*                                      */                  /*CRIT*/
            /* We opened it.  Let everybody go, and */                  /*CRIT*/
            /* close it again.                      */                  /*CRIT*/
            /*                                      */                  /*CRIT*/
            for (mt = barrier->mb_qhead;                                /*CRIT*/
                 mt != NULL;                                            /*CRIT*/
                 mt = mt->mt_wait_link) {                               /*CRIT*/
                release_count++;                                        /*CRIT*/
            }                                                           /*CRIT*/
            INC_RUNNABLE_MACT_COUNT(release_count);                     /*CRIT*/
            for (mt = barrier->mb_qhead;                                /*CRIT*/
                 mt != NULL;                                            /*CRIT*/
                 mt = mt->mt_wait_link) {                               /*CRIT*/
                mt->mt_state = TS_Running;                              /*CRIT*/
            }                                                           /*CRIT*/
            barrier->mb_qhead  = NULL;                                  /*CRIT*/
            barrier->mb_qtail  = NULL;                                  /*CRIT*/
            barrier->mb_ncount = barrier->mb_ocount;                    /*CRIT*/
            was_open = 1;                                               /*CRIT*/
        }                                                               /*CRIT*/
        else {                                                          /*CRIT*/
            /*                                                 */       /*CRIT*/
            /* It's not open yet; get on the tail of the queue */       /*CRIT*/
            /* and we'll wait for it.                          */       /*CRIT*/
            /*                                                 */       /*CRIT*/
            my_mt = _mact_find_self();                                  /*CRIT*/
            my_mt->mt_wait_obj  = (volatile void *) barrier;            /*CRIT*/
            my_mt->mt_wait_link = NULL;                                 /*CRIT*/
            if (barrier->mb_qhead == NULL) {                            /*CRIT*/
                barrier->mb_qhead = barrier->mb_qtail = my_mt;          /*CRIT*/
            }                                                           /*CRIT*/
            else {                                                      /*CRIT*/
                barrier->mb_qtail->mt_wait_link = my_mt;                /*CRIT*/
                barrier->mb_qtail               = my_mt;                /*CRIT*/
            }                                                           /*CRIT*/
            barrier->mb_ncount--;                                       /*CRIT*/
            my_mt->mt_state = TS_Bar_Wait;                              /*CRIT*/
            was_open = 0;                                               /*CRIT*/
        }                                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&(barrier->mb_sem));                                     /*CRIT*/

    /*
     * If the barrier was open, then we're done.
     */
    if (was_open)
        return;

    /*
     * Otherwise, wait for someone else to open it.  When
     * they do so, our state changes back to "running".
     */
    DEC_RUNNABLE_MACT_COUNT();
    _mact_wait_for_me_to_be_running(my_mt);

    /*
     * Done.
     */
    return;
}


/*========================================
 * BARREL_
 *
 * Release (deassign) a barrier.
 */
void
barrel_(name)
mact_barrier_t        **name;           /* Address of the barrier address   */
{

    if ((*name)->mb_ncount != (*name)->mb_ocount) {
        /*
         * Tasks are waiting on it; you shouldn't be releasing it.
         */
        fprintf(stderr, "BARREL called with tasks waiting for barrier\n");
        abort();
    }

    /*
     * Release the space.
     */
    free((void *) *name);
    *name = NULL;

    return;
}

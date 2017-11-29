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


#pragma ident "@(#) libu/multi/posix/mactcdl.c	92.1	06/25/99 14:35:10"

#include "macdefs.h"
#include <stdio.h>
#include <stdlib.h>
#include <thread.h>

#if defined(ASSEMBLY)
#   undef ASSEMBLY
#endif
#include "mactdefs.h"


/*========================================
 * get_more_mt_descriptors
 *
 * Get more macrotasking descriptors and add them to the global
 * lists.
 *
 * NOTE: This routine MUST be called with the macrotasking
 *       descriptor free list semaphore (_mact_free_sem) held!
 */
static void get_more_mt_descriptors(void);

static void
get_more_mt_descriptors()
{
    mact_task_t        *new_mts;        /* new group of macrotasking descs */
    mact_task_t        *curr_mt;        /* a macrotasking desc in the above */

#   define MTS_TO_GET   10

    /*
     * Get some more descriptors, and leave them on the free list.
     * Currently, we grab 10.  If we mark them all as free and chain
     * the existing list to the end of them, then change the global
     * main list and free pointers to point to them, then we don't
     * have to worry about hurting anyone traversing the main list,
     * even though it's not locked (the free list is, of course).
     */
    if (   (new_mts = (mact_task_t *) malloc(MTS_TO_GET * sizeof(mact_task_t)))
        == NULL
       ) {
        /*
         * Couldn't allocate any.  Halt the program.
         */
        perror("Could not get memory for new macrotask descriptor");
        abort();
    }

    for (curr_mt = &new_mts[0]; curr_mt < &new_mts[MTS_TO_GET - 1]; curr_mt++) {
        curr_mt->mt_state = TS_Nonexistent;
        curr_mt->mt_tcom  = NULL;
        curr_mt->mt_link  = curr_mt + 1;
    }

    curr_mt->mt_link = _mact_free;
    _mact_free       = new_mts;
    _mact_tasks      = new_mts;

    /*
     * Update our counters.
     */
    _mact_total_count += MTS_TO_GET;
    _mact_free_count  += MTS_TO_GET;

    /*
     * Done.
     */
    return;

#   undef MTS_TO_GET
}


/*========================================
 * _MACT_ALLOCATE
 *
 * Provide a new macrotask descriptor.
 */
volatile mact_task_t *
_mact_allocate()
{
    volatile mact_task_t
                       *mt;             /* Ptr to the macrotask descriptor */
    static unsigned int next_mtid = 2;  /* Next unique macrotask ID        */

    /*
     * Get a free descriptor.
     */
    _mact_psem(&_mact_free_sem);                                        /*CRIT*/
    {                                                                   /*CRIT*/
        if (_mact_free == NULL) {                                       /*CRIT*/
            get_more_mt_descriptors();                                  /*CRIT*/
        }                                                               /*CRIT*/
        mt         = _mact_free;                                        /*CRIT*/
        _mact_free = _mact_free->mt_link;                               /*CRIT*/
        _mact_free_count--;                                             /*CRIT*/
                                                                        /*CRIT*/
        mt->mt_mtid  = next_mtid++;                                     /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&_mact_free_sem);                                        /*CRIT*/

    /*
     * Done.
     */
    return(mt);
}


/*========================================
 * _MACT_ADD
 *
 * Add this macrotask to the running list.
 */
void
_mact_add(mt)
volatile mact_task_t   *mt;             /* Ptr to the macrotask descriptor */
{

    /*
     * Put the descriptor on the appropriate hash list.
     */
    _mact_psem(&_mact_free_sem);                                        /*CRIT*/
    {                                                                   /*CRIT*/
        mt->mt_link          = MT_HASH(mt->mt_stid);                    /*CRIT*/
        MT_HASH(mt->mt_stid) = mt;                                      /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&_mact_free_sem);                                        /*CRIT*/

    /*
     * Done.
     */
    return;
}


/*========================================
 * _MACT_DELETE
 *
 * Remove a task from the _mact_hash[] data structure, and put it on
 * the free list.
 *
 * NOTES
 *   - There is no protection against failing to find the given
 *     descriptor in the hash list.
 */
void
_mact_delete(volatile mact_task_t *my_mt)
{
    volatile mact_task_t
                       *mt;             /* Ptr to waiter's macrotask desc */
    unsigned int        release_count   /* Number of tasks we'll release  */
                        = 0;
    thread_t            id              /* Task's Solaris thread ID       */
                        = my_mt->mt_stid;
    volatile mact_task_t
                       *prev_mt;        /* Previous descriptor            */

    /*
     * I am complete now.
     */
    my_mt->mt_state = TS_Complete;

    /*
     * Let all the tasks waiting for me continue.
     */
    _mact_psem(&(my_mt->mt_sem));                                       /*CRIT*/
    {                                                                   /*CRIT*/
        for (mt = my_mt->mt_waiters;                                    /*CRIT*/
             mt != NULL;                                                /*CRIT*/
             mt = mt->mt_wait_link                                      /*CRIT*/
            ) {                                                         /*CRIT*/
            release_count++;                                            /*CRIT*/
        }                                                               /*CRIT*/
        INC_RUNNABLE_MACT_COUNT(release_count);                         /*CRIT*/
        for (mt = my_mt->mt_waiters;                                    /*CRIT*/
             mt != NULL;                                                /*CRIT*/
             mt = mt->mt_wait_link                                      /*CRIT*/
            ) {                                                         /*CRIT*/
            mt->mt_state = TS_Running;                                  /*CRIT*/
            release_count++;                                            /*CRIT*/
        }                                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&(my_mt->mt_sem));                                       /*CRIT*/

    /*
     * We now have one fewer runnable macrotask.
     */
    DEC_RUNNABLE_MACT_COUNT();

    /*
     * Put my descriptor on the free list; I'm done with it.
     */
    _mact_psem(&_mact_free_sem);                                        /*CRIT*/
    {                                                                   /*CRIT*/
        if ((prev_mt = MT_HASH(id)) == my_mt) {                         /*CRIT*/
            MT_HASH(id) = my_mt->mt_link;                               /*CRIT*/
        }                                                               /*CRIT*/
        else {                                                          /*CRIT*/
            while (prev_mt->mt_link != my_mt) {                         /*CRIT*/
                prev_mt = prev_mt->mt_link;                             /*CRIT*/
            }                                                           /*CRIT*/
            prev_mt->mt_link = prev_mt->mt_link->mt_link;               /*CRIT*/
        }                                                               /*CRIT*/
                                                                        /*CRIT*/
        my_mt->mt_state = TS_Nonexistent;                               /*CRIT*/
        my_mt->mt_stid  = 0;                                            /*CRIT*/
        my_mt->mt_link  = _mact_free;                                   /*CRIT*/
        _mact_free      = my_mt;                                        /*CRIT*/
        _mact_free_count++;                                             /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&_mact_free_sem);                                        /*CRIT*/

    return;
}


/*========================================
 * _MACT_FIND_SELF
 *
 * Find my macrotask.
 *
 */
volatile mact_task_t *
_mact_find_self(void)
{
    thread_t            stid            /* My Solaris thread ID   */
                        = thr_self();
    volatile mact_task_t
                       *mt;             /* Ptr to some descriptor */

    /*
     * Find the macrotask running on my thread.
     */
    for (mt = MT_HASH(stid);
         mt != NULL && mt->mt_stid != stid;
         mt = mt->mt_link
        )
        ;

    if (mt == NULL) {
        /*
         * I am hitherto unknown -- create me.
         */
        mt          = _mact_allocate();
        mt->mt_stid = stid;
        mt->mt_utid = 0;
        _mact_add(mt);
    }

    return(mt);
}

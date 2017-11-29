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


#pragma ident "@(#) libu/multi/posix/mactevnt.c	92.1	06/25/99 14:35:10"

#include "macdefs.h"
#include <stdio.h>
#include <stdlib.h>
#include <thread.h>

#if defined(ASSEMBLY)
#   undef ASSEMBLY
#endif
#include "mactdefs.h"


/*========================================
 * _MACT_VARARG_EVASGN
 *
 * Do an EVASGN().  The evasgn_() entry point is in mactutil.s,
 * so we can handle the optional user arguments.
 */
void
_mact_vararg_evasgn(name, value)
void                   *name;           /* Where to put the event address */
void                   *value;          /* Expected initial value         */
{
    mact_event_t       *new_event;      /* The new event                  */

    /*
     * Has this event already been assigned?
     */
    if (   value != NULL
        && *((unsigned int *) value) != *((unsigned int *) name)
       ) {
        return;
    }

    /*
     * Get the new event and initialize it.
     */
    if ((new_event = (mact_event_t *) malloc(sizeof(mact_event_t))) == NULL) {
        /*
         * Couldn't allocate any.  Halt the program.
         */
        perror("Could not get memory for new event in EVASGN");
    }

    new_event->me_state  = ES_Cleared;
    new_event->me_qhead  = NULL;
    new_event->me_qtail  = NULL;
    new_event->me_wcount = 0;
    new_event->me_sem    = 0;

    /*
     * Give it to our caller, and we're done.
     */
    *((mact_event_t **) name) = new_event;

    return;
}


/*========================================
 * EVWAIT_
 *
 * Wait for an event to be posted.
 */
void
evwait_(name)
mact_event_t          **name;           /* Address of the event address   */
{
    mact_event_t       *event = *name;  /* The event address              */
    volatile mact_task_t
                       *my_mt;          /* Ptr to my macrotask descriptor */
    unsigned int        have_it;        /* Do we have the event?          */

    _mact_psem(&(event->me_sem));                                       /*CRIT*/
    {                                                                   /*CRIT*/
        if (event->me_state == ES_Posted) {                             /*CRIT*/
            /*                        */                                /*CRIT*/
            /* It's posted; continue. */                                /*CRIT*/
            /*                        */                                /*CRIT*/
            have_it = 1;                                                /*CRIT*/
        }                                                               /*CRIT*/
        else {                                                          /*CRIT*/
            /*                                                */        /*CRIT*/
            /* It's cleared; get on the tail of the queue and */        /*CRIT*/
            /* we'll wait for it.                             */        /*CRIT*/
            /*                                                */        /*CRIT*/
            my_mt = _mact_find_self();                                  /*CRIT*/
            my_mt->mt_wait_obj  = (volatile void *) event;              /*CRIT*/
            my_mt->mt_wait_link = NULL;                                 /*CRIT*/
            if (event->me_qhead == NULL) {                              /*CRIT*/
                event->me_qhead = event->me_qtail = my_mt;              /*CRIT*/
            }                                                           /*CRIT*/
            else {                                                      /*CRIT*/
                event->me_qtail->mt_wait_link = my_mt;                  /*CRIT*/
                event->me_qtail               = my_mt;                  /*CRIT*/
            }                                                           /*CRIT*/
            event->me_wcount++;                                         /*CRIT*/
            my_mt->mt_state = TS_Event_Wait;                            /*CRIT*/
            have_it = 0;                                                /*CRIT*/
        }                                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&(event->me_sem));                                       /*CRIT*/

    /*
     * If the event was posted, then we're done.
     */
    if (have_it)
        return;

    /*
     * Otherwise, wait for someone to post it.  When they do so,
     * they'll change our state back to "running".
     */
    DEC_RUNNABLE_MACT_COUNT();
    _mact_wait_for_me_to_be_running(my_mt);

    /*
     * Done.
     */
    return;
}


/*========================================
 * EVPOST_
 *
 * Post an event.
 */
void
evpost_(name)
mact_event_t          **name;           /* Address of the event address   */
{
    mact_event_t       *event = *name;  /* The event address              */
    volatile mact_task_t
                       *mt;             /* Ptr to waiter's macrotask desc */
    unsigned int        release_count   /* Number of tasks we'll release  */
                        = 0;

    _mact_psem(&(event->me_sem));                                       /*CRIT*/
    {                                                                   /*CRIT*/
        /*                                            */                /*CRIT*/
        /* Post the event, and let any tasks waiting  */                /*CRIT*/
        /* for it proceed.                            */                /*CRIT*/
        /*                                            */                /*CRIT*/
        event->me_state = ES_Posted;                                    /*CRIT*/
        for (mt = event->me_qhead; mt != NULL; mt = mt->mt_wait_link) { /*CRIT*/
            release_count++;                                            /*CRIT*/
        }                                                               /*CRIT*/
        INC_RUNNABLE_MACT_COUNT(release_count);                         /*CRIT*/
        for (mt = event->me_qhead; mt != NULL; mt = mt->mt_wait_link) { /*CRIT*/
            mt->mt_state = TS_Running;                                  /*CRIT*/
        }                                                               /*CRIT*/
        event->me_qhead = NULL;                                         /*CRIT*/
        event->me_qtail = NULL;                                         /*CRIT*/
        event->me_wcount = 0;                                           /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&(event->me_sem));                                       /*CRIT*/

    /*
     * Done.
     */
    return;
}


/*========================================
 * EVCLEAR_
 *
 * Clear an event.
 */
void
evclear_(name)
mact_event_t          **name;           /* Address of the event address   */
{
    mact_event_t       *event = *name;  /* The event address              */

    _mact_psem(&(event->me_sem));                                       /*CRIT*/
    {                                                                   /*CRIT*/
        event->me_state = ES_Cleared;                                   /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&(event->me_sem));                                       /*CRIT*/

    /*
     * Done.
     */
    return;
}


/*========================================
 * EVTEST_
 *
 * Test an event.
 */
int
evtest_(name)
mact_event_t          **name;           /* Address of the event address   */
{

    return((*name)->me_state == ES_Posted);
}


/*========================================
 * EVREL_
 *
 * Release (deassign) an event.
 */
void
evrel_(name)
mact_event_t          **name;           /* Address of the event address   */
{

    if ((*name)->me_state == ES_Cleared && (*name)->me_qhead != NULL) {
        /*
         * Tasks are waiting on it; you shouldn't be releasing it.
         */
        fprintf(stderr, "EVREL called with tasks waiting for event\n");
        abort();
    }

    /*
     * Release the space.
     */
    free((void *) *name);
    *name = NULL;

    return;
}

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


#pragma ident "@(#) libu/multi/posix/mactlock.c	92.1	06/25/99 14:35:10"

#include "macdefs.h"
#include <stdio.h>
#include <stdlib.h>
#include <thread.h>

#if defined(ASSEMBLY)
#   undef ASSEMBLY
#endif
#include "mactdefs.h"


/*========================================
 * _MACT_VARARG_LOCKASGN
 *
 * Do a LOCKASGN().  The lockasgn_() entry point is in
 * mactutil.s, so we can handle the optional user arguments.
 */
void
_mact_vararg_lockasgn(name, value)
void                   *name;           /* Where to put the lock address */
void                   *value;          /* Expected initial value        */
{
    mact_lock_t        *new_lock;       /* The new lock                  */

    /*
     * Has this lock already been assigned?
     */
    if (   value != NULL
        && *((unsigned int *) value) != *((unsigned int *) name)
       ) {
        return;
    }

    /*
     * Get the new lock and initialize it.
     */
    if ((new_lock = (mact_lock_t *) malloc(sizeof(mact_lock_t))) == NULL) {
        /*
         * Couldn't allocate any.  Halt the program.
         */
        perror("Could not get memory for new lock in LOCKASGN");
    }

    new_lock->ml_state  = LS_Unlocked;
    new_lock->ml_qhead  = NULL;
    new_lock->ml_qtail  = NULL;
    new_lock->ml_wcount = 0;
    new_lock->ml_sem    = 0;

    /*
     * Give it to our caller, and we're done.
     */
    *((mact_lock_t **) name) = new_lock;

    return;
}


/*========================================
 * LOCKON_
 *
 * Lock a lock.
 */
void
lockon_(name)
mact_lock_t           **name;           /* Address of the lock address    */
{
    mact_lock_t        *lock = *name;   /* The lock address               */
    volatile mact_task_t
                       *my_mt;          /* Ptr to my macrotask descriptor */
    unsigned int        have_it;        /* Do we have the lock?           */

    _mact_psem(&(lock->ml_sem));                                        /*CRIT*/
    {                                                                   /*CRIT*/
        if (lock->ml_state == LS_Unlocked) {                            /*CRIT*/
            /*                              */                          /*CRIT*/
            /* It's unlocked; just take it. */                          /*CRIT*/
            /*                              */                          /*CRIT*/
            lock->ml_state = LS_Locked;                                 /*CRIT*/
            have_it = 1;                                                /*CRIT*/
        }                                                               /*CRIT*/
        else {                                                          /*CRIT*/
            /*                                               */         /*CRIT*/
            /* It's locked; get on the tail of the queue and */         /*CRIT*/
            /* we'll wait for it.                            */         /*CRIT*/
            /*                                               */         /*CRIT*/
            my_mt = _mact_find_self();                                  /*CRIT*/
            my_mt->mt_wait_obj  = (volatile void *) lock;               /*CRIT*/
            my_mt->mt_wait_link = NULL;                                 /*CRIT*/
            if (lock->ml_qhead == NULL) {                               /*CRIT*/
                lock->ml_qhead = lock->ml_qtail = my_mt;                /*CRIT*/
            }                                                           /*CRIT*/
            else {                                                      /*CRIT*/
                lock->ml_qtail->mt_wait_link = my_mt;                   /*CRIT*/
                lock->ml_qtail               = my_mt;                   /*CRIT*/
            }                                                           /*CRIT*/
            lock->ml_wcount++;                                          /*CRIT*/
            my_mt->mt_state = TS_Lock_Wait;                             /*CRIT*/
            have_it = 0;                                                /*CRIT*/
        }                                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&(lock->ml_sem));                                        /*CRIT*/

    /*
     * If we got the lock, then we're done.
     */
    if (have_it)
        return;

    /*
     * Otherwise, wait for someone to give it to us.  When they
     * do so, they'll change our state back to "running".
     */
    DEC_RUNNABLE_MACT_COUNT();
    _mact_wait_for_me_to_be_running(my_mt);

    /*
     * Done.
     */
    return;
}


/*========================================
 * LOCKOFF_
 *
 * Unlock a lock.
 */
void
lockoff_(name)
mact_lock_t           **name;           /* Address of the lock address    */
{
    mact_lock_t        *lock = *name;   /* The lock address               */
    volatile mact_task_t
                       *mt;             /* Ptr to waiter's macrotask desc */

    _mact_psem(&(lock->ml_sem));                                        /*CRIT*/
    {                                                                   /*CRIT*/
        if (lock->ml_qhead == NULL) {                                   /*CRIT*/
            /*                                 */                       /*CRIT*/
            /* Nobody waiting; just unlock it. */                       /*CRIT*/
            /*                                 */                       /*CRIT*/
            lock->ml_state = LS_Unlocked;                               /*CRIT*/
        }                                                               /*CRIT*/
        else {                                                          /*CRIT*/
            /*                                            */            /*CRIT*/
            /* Somebody is waiting; let them have it, and */            /*CRIT*/
            /* leave it locked.                           */            /*CRIT*/
            /*                                            */            /*CRIT*/
            mt = lock->ml_qhead;                                        /*CRIT*/
            if ((lock->ml_qhead = mt->mt_wait_link) == NULL) {          /*CRIT*/
                lock->ml_qtail = NULL;                                  /*CRIT*/
            }                                                           /*CRIT*/
            lock->ml_wcount--;                                          /*CRIT*/
            INC_RUNNABLE_MACT_COUNT(1);                                 /*CRIT*/
            mt->mt_state = TS_Running;                                  /*CRIT*/
        }                                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&(lock->ml_sem));                                        /*CRIT*/

    /*
     * Done.
     */
    return;
}


/*========================================
 * LOCKTEST_
 *
 * Test a lock, and lock it if it's unlocked.
 */
int
locktest_(name)
mact_lock_t           **name;           /* Address of the lock address    */
{
    mact_lock_t        *lock = *name;   /* The lock address               */
    unsigned int        have_it;        /* Do we have the lock?           */

    _mact_psem(&(lock->ml_sem));                                        /*CRIT*/
    {                                                                   /*CRIT*/
        if (lock->ml_state == LS_Unlocked) {                            /*CRIT*/
            /*                         */                               /*CRIT*/
            /* It's unlocked; take it. */                               /*CRIT*/
            /*                         */                               /*CRIT*/
            lock->ml_state = LS_Locked;                                 /*CRIT*/
            have_it = 1;                                                /*CRIT*/
        }                                                               /*CRIT*/
        else {                                                          /*CRIT*/
            /*                                */                        /*CRIT*/
            /* It's locked; we can't have it. */                        /*CRIT*/
            /*                                */                        /*CRIT*/
            have_it = 0;                                                /*CRIT*/
        }                                                               /*CRIT*/
    }                                                                   /*CRIT*/
    _mact_vsem(&(lock->ml_sem));                                        /*CRIT*/

    /*
     * Tell our caller whether or not it was already locked.
     */
    return(!have_it);
}


/*========================================
 * LOCKREL_
 *
 * Release (deassign) a lock.
 */
void
lockrel_(name)
mact_lock_t           **name;           /* Address of the lock address    */
{

    if ((*name)->ml_state == LS_Locked) {
        /*
         * It's locked; you shouldn't be releasing it.
         */
        fprintf(stderr, "LOCKREL called with lock set\n");
        abort();
    }

    /*
     * Release the space.
     */
    free((void *) *name);
    *name = NULL;

    return;
}

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


/* USMID @(#) libu/multi/posix/mactdefs.h	92.0	10/08/98 14:57:41 */

/*
 * Use these definitions for the all source files.
 *
 * NOTE: The two sides (assembly and C) of this #if MUST stay in
 *        synchronization!
 */

/*
 * Number of entries in the Solaris threads hash table.
 */
#define MTHASH_SIZE     256


#if defined(ASSEMBLY)

    !
    ! Use these definitions for the assembly language source files.
    !
    ! NOTE: The two sides (assembly and C) of this #if MUST stay in
    !       synchronization!

    .seg "data"

    !
    ! Stack frame offsets and sizes.  The SFO_ARGS_? sizes are
    ! double aligned.
    !
    SFO_WSAVE   = 0                     ! 16 word window save area
    SFO_SU_RETP = SFO_WSAVE   + (16 * 4)! Struct/union return pointer
    SFO_ARGS    = SFO_SU_RETP + 4       ! Start of outgoing arguments
    SFO_ARGS_1  = SFO_ARGS    + 4       ! Enough space for 1 outgoing arg
    SFO_ARGS_3  = SFO_ARGS_1  + 8       ! Enough space for 3 outgoing args
    SFO_ARGS_5  = SFO_ARGS_3  + 8       ! Enough space for 5 outgoing args

    _TASKVER    = 2                     ! Table layout version # (same as Cray)

    !
    ! Address of the static $TASKCOM.  The storage is in
    ! mactstcm.s.
    !
    .global _taskcom

    !
    ! Task states.
    !
    TS_NONEXISTENT = 0                  ! Descriptor is free
    TS_READY       = TS_NONEXISTENT + 1 ! Ready to run (presently unused)
    TS_RUNNING     = TS_READY       + 1 ! Running
    TS_LOCK_WAIT   = TS_READY       + 1 ! Waiting for a lock
    TS_EVENT_WAIT  = TS_LOCK_WAIT   + 1 ! Waiting for an event
    TS_BAR_WAIT    = TS_EVENT_WAIT  + 1 ! Waiting for a barrier
    TS_TASK_WAIT   = TS_BAR_WAIT    + 1 ! Waiting for a task to complete
    TS_COMPLETE    = TS_TASK_WAIT   + 1 ! Done

    !
    ! Per-task information.
    !
    MT_MTID       = 0                   ! Unique macrotask ID
    MT_STATE      = MT_MTID       + 4   ! Task state
    MT_STID       = MT_STATE      + 4   ! Solaris thread id
    MT_UTID       = MT_STID       + 4   ! User task ID (word 3 of TCA)
    MT_WAIT_OBJ   = MT_UTID       + 4   ! Object being waited for
    MT_WAIT_LINK  = MT_WAIT_OBJ   + 4   ! Link in wait chain
    MT_WAITERS    = MT_WAIT_LINK  + 4   ! List of tasks TSKWAITing for me
    MT_WAIT_COUNT = MT_WAITERS    + 4   ! Number of tasks TSKWAITing for me
    MT_SUBR       = MT_WAIT_COUNT + 4   ! The macrotasked subroutine
    MT_ARGC       = MT_SUBR       + 4   ! Number of args for macrotask
    MT_ARGV       = MT_ARGC       + 4   ! Vector of args for macrotask
    MT_TCOM       = MT_ARGV       + 4   ! Task common description table
    MT_LINK       = MT_TCOM       + 4   ! Link in free or hash chain
    MT_SEM        = MT_LINK       + 4   ! Semaphore for this descriptor
                                        ! Structure size, padded to size of
                                        !   SuperDragon external cache line
       MTP        = (64 - ((MT_SEM + 4) % 64))
    MT_SIZE       = MT_SEM        + 4 + MTP

    .global tskstart_
    .global _mact_vararg_tskstart
    .global _mact_start_macrotask
    .global _mact_add
    .global _mact_delete
    .global _mact_psem
    .global _mact_vsem

    .global lockasgn_
    .global _mact_vararg_lockasgn

    .global evasgn_
    .global _mact_vararg_evasgn

    .global tsktune_
    .global _mact_vararg_tsktune

#else

    /*
     * Use these definitions for the C language source files.
     *
     * NOTE: The two sides (assembly and C) of this #if MUST stay in
     *       synchronization!
     */

    /*====================
     * TASK COMMON
     */

#   define _TASKVER             2   /* Table layout version # (same as Cray) */

    /*
     * Address of the static $TASKCOM.  The storage is in
     * mactstcm.s.
     */
    extern void                *_taskcom;


    /*====================
     * TASKS
     */

    /*
     * Task states.
     */
    typedef enum {
        TS_Nonexistent,                 /* Descriptor is free               */
        TS_Ready,                       /* Ready to run (presently unused)  */
        TS_Running,                     /* Running                          */
        TS_Lock_Wait,                   /* Waiting for a lock               */
        TS_Event_Wait,                  /* Waiting for an event             */
        TS_Bar_Wait,                    /* Waiting for a barrier            */
        TS_Task_Wait,                   /* Waiting for a task to complete   */
        TS_Complete,                    /* Done                             */
    } mact_task_state_t;

    /*
     * Per-task information.  If you need to change this, be sure
     * to update the computation of mt_argv's size.  This member
     * takes enough space to pad the whole structure to a multiple
     * of the SuperDragon external cache line size.  If there are
     * more args than will fit, then the space is malloc()ed, and
     * a pointer to it is stored in mt_argv[0].
     */
    typedef struct MACT_Task_t {
        unsigned int    mt_mtid;        /* Unique macrotask ID               */
        mact_task_state_t
                        mt_state;       /* Task state                        */
        thread_t        mt_stid;        /* Solaris thread ID                 */
        unsigned int    mt_utid;        /* User task ID (word 3 of TCA)      */
        volatile void  *mt_wait_obj;    /* Object being waited for           */
        volatile void  *mt_wait_link;   /* Link in wait chain                */
        volatile void  *mt_waiters;     /* List of tasks TSKWAITing for me   */
        unsigned int    mt_wait_count;  /* Number of tasks TSKWAITing for me */
        void          (*mt_subr)();     /* The macrotasked subroutine        */
        unsigned int    mt_argc;        /* Number of args for macrotask      */
        void          **mt_argv;        /* Vector of args for macrotask      */
        void           *mt_tcom;        /* Task common description table     */
        volatile struct MACT_Task_t
                       *mt_link;        /* Link in free or hash chain        */
        int             mt_sem;         /* Semaphore for this descriptor     */
        char            mt_pad          /* Pad to the SuperDragon external   */
                        [  64           /*   cache line size.                */
                         - (  (  sizeof(unsigned int)
                               + sizeof(mact_task_state_t)
                               + sizeof(thread_t)
                               + sizeof(unsigned int)
                               + sizeof(volatile void *)
                               + sizeof(volatile void *)
                               + sizeof(unsigned int)
                               + sizeof(void (*)())
                               + sizeof(unsigned int)
                               + sizeof(void **)
                               + sizeof(void *)
                               + sizeof(volatile struct MACT_Task_t *)
                               + sizeof(int)
                              )
                            % 64
                           )
                        ];
    } mact_task_t;

    extern volatile mact_task_t
                       *_mact_tasks;
    extern volatile mact_task_t
                       *_mact_free;
    extern volatile int _mact_free_sem;
    extern volatile mact_task_t
                       *_mact_hash[MTHASH_SIZE];
    extern volatile int _mact_hash_sem;

#   define MT_HASH(id) _mact_hash[(id) & (MTHASH_SIZE - 1)]

    /*
     * Total number of macrotask descriptors we have, and the number
     * of free ones.
     */
    extern volatile unsigned int
                        _mact_total_count;
    extern volatile unsigned int
                        _mact_free_count;

    /*
     * Total number of runnable macrotasks there are, a semaphore
     * to protect access, and macros for updating the counter.
     */
    extern volatile unsigned int
                        _mact_runnable_tasks;
    extern volatile int _mact_runnable_tasks_sem;

#   define INC_RUNNABLE_MACT_COUNT(num)                                 \
    do                                                                  \
    {                                                                   \
        _mact_psem(&_mact_runnable_tasks_sem);                 /*CRIT*/ \
        _mact_runnable_tasks += (num);                         /*CRIT*/ \
        _mact_vsem(&_mact_runnable_tasks_sem);                 /*CRIT*/ \
    } while (0)

#   define DEC_RUNNABLE_MACT_COUNT()                                    \
    do                                                                  \
    {                                                                   \
        _mact_psem(&_mact_runnable_tasks_sem);                 /*CRIT*/ \
        _mact_runnable_tasks--;                                /*CRIT*/ \
        _mact_vsem(&_mact_runnable_tasks_sem);                 /*CRIT*/ \
    } while (0)

#   define CHECK_FOR_MACT_DEADLOCK()                                    \
    do                                                                  \
    {                                                                   \
        if (_mact_runnable_tasks == 0)                                  \
        {                                                               \
             tsklist_();                                                \
             fflush(stdout);                                            \
             fprintf(stderr,                                            \
                     "Deadlock - All user tasks waiting for %s\n",      \
                     "locks, events, or tasks"                          \
                    );                                                  \
             abort();                                                   \
        }                                                               \
    } while (0)


    /*====================
     * LOCKS
     */

    /*
     * Lock states.
     */
    typedef enum {
        LS_Locked,
        LS_Unlocked
    } mact_lock_state_t;

    /*
     * Per-lock information.
     */
    typedef struct MACT_Lock_t {
        mact_lock_state_t
                        ml_state;       /* Lock state                       */
        volatile mact_task_t
                       *ml_qhead,       /* Head of queue of waiters         */
                       *ml_qtail;       /* Tail of queue of waiters         */
        unsigned int    ml_wcount;      /* Number of tasks waiting          */
        int             ml_sem;         /* Semaphore for this descriptor    */
    } mact_lock_t;


    /*====================
     * EVENTS
     */

    /*
     * Event states.
     */
    typedef enum {
        ES_Posted,
        ES_Cleared
    } mact_event_state_t;

    /*
     * Per-event information.
     */
    typedef struct MACT_Event_t {
        mact_event_state_t
                        me_state;       /* Event state                      */
        volatile mact_task_t
                       *me_qhead,       /* Head of queue of waiters         */
                       *me_qtail;       /* Tail of queue of waiters         */
        unsigned int    me_wcount;      /* Number of tasks waiting          */
        int             me_sem;         /* Semaphore for this descriptor    */
    } mact_event_t;


    /*====================
     * BARRIERS
     */

    /*
     * Per-barrier information.
     */
    typedef struct MACT_Barrier_t {
        unsigned int    mb_ocount;      /* Number of tasks needed to open   */
        unsigned int    mb_ncount;      /* Number of tasks not yet synced   */
        volatile mact_task_t
                       *mb_qhead,       /* Head of queue of waiters         */
                       *mb_qtail;       /* Tail of queue of waiters         */
        int             mb_sem;         /* Semaphore for this descriptor    */
    } mact_barrier_t;


    /*====================
     * STACKS
     */

    /*
     * Macrotask stack sizes, and the associated semaphore.
     */
    extern volatile size_t
                        _mact_stack_size;
    extern volatile int _mact_stack_size_sem;


    /*====================
     * FUNCTIONS
     */

    /*
     * Available functions.
     */
    extern void         _mact_vararg_tskstart_(unsigned int [], void (*)(),
                                               unsigned int, void *[]
                                              );
    extern void         _mact_start_macrotask(volatile mact_task_t *);

    extern volatile mact_task_t
                       *_mact_allocate(void);
    extern void         _mact_add(volatile mact_task_t *);
    extern void         _mact_delete(volatile mact_task_t *);
    extern void         _mact_psem(volatile int *);
    extern void         _mact_vsem(volatile int *);
    extern volatile mact_task_t
                       *_mact_find_self(void);
    extern void         _mact_wait_for_me_to_be_running(volatile mact_task_t *);

    extern void         _mact_vararg_lockasgn_(void *, void*);

    extern void         _mact_vararg_evasgn_(void *, void*);

    extern void         _mact_vararg_tsktune_(unsigned int, void*);

    /*
     * PSL provides this, which is used by the task common support
     * code to tell whether the caller is an autotask or macrotask,
     * and if an autotask, where the task common descriptor should
     * be put.
     */
    extern void         _psl_mact_tcom_desc_addr(void ***, int *);

#endif

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/* USMID @(#) clibinc/cray/mtlock.h	92.2	07/26/99 12:57:34 */


/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

#ifndef _CRAY_MTLOCK_H
#define _CRAY_MTLOCK_H

/*
 *	Include section.
 */

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#include <cray/libuni.h>
#endif

#if	defined(__mips)
#include <mutex.h>
#elif	defined(_SOLARIS)
#include <synch.h>
#elif   defined(KEY) /* Bug 6003 */
#include <pthread.h>
#elif   defined(_LITTLE_ENDIAN) && !defined(__sv2)
/* do not include pthread.h yet */
#endif


/*
 *	Typedef section.
 */

/*
 *	The plock_t typedef should be used for pthread-locking.
 *	The mpplock_t typedef should be used for PE-locking.
 */
#if	defined(__mips) || (defined(_LITTLE_ENDIAN) && defined(__sv2))
typedef unsigned long	plock_t;
#elif	defined(_SOLARIS)
typedef mutex_t	plock_t;
#elif   defined(KEY) /* Bug 6003 */
typedef pthread_mutex_t plock_t;
#elif	defined(_LITTLE_ENDIAN) && !defined(__sv2)
/* do not use typedef pthread_mutex_t	plock_t yet */
typedef unsigned long	plock_t;
#else
typedef volatile long	plock_t;
#endif
typedef long mpplock_t;

/*
 *	Macros section.
 */

/*
 * DECL_LOCK(name) declares a pthread-lock word with name 'name'.
 * DECL_MPP_LOCK(name) declares a PE-lock word with name 'name'.
 * EXTERN_LOCK(name) is used in modules which reference a pthread-lock.
 * EXTERN_MPP_LOCK(name) is used in modules which reference a PE-lock.
 */

#define DECL_LOCK(name)		plock_t name;	/* static initialized to 0 */
#define DECL_MPP_LOCK(name)	mpplock_t name;	/* static initialized to 0 */
#define EXTERN_LOCK(name)	extern plock_t name;
#define EXTERN_MPP_LOCK(name)	extern mpplock_t name;

/*
 * MULTI_ON tests for the existence of multi-tasking in a program.
 */

#if	defined(_CRAY1) || defined(_LIB_UMK)
#define	MULTI_ON		($MULTION == 0)
#elif	defined(__mips)
#define	MULTI_ON		1
#elif	defined(_SOLARIS)
#define	MULTI_ON		1
#else
#define	MULTI_ON		0
#endif

/*
 * MEM_LOCK and MEM_UNLOCK are general library locking macros.  They
 * take one argument:
 *
 *	lock - the address of the word used as a software lock.
 *
 * These macros are no-ops if multitasking is not being used by the program.
 */
#if	defined(_CRAY1) || defined(_LIB_UMK)
#define MEM_LOCK(lock)		{ if (MULTI_ON) _inline_mtlock(lock); }
#define MEM_UNLOCK(lock)	{ if (MULTI_ON) _inline_mtunlock(lock); }

#elif	defined(__mips) || (defined(_LITTLE_ENDIAN) && defined(__sv2))
#define MEM_LOCK(lock)		{ while (__lock_test_and_set((lock),1L)); }
#define MEM_UNLOCK(lock)	{ *(lock) = 0; }

#elif	defined(_SOLARIS)
#define MEM_LOCK(lock)		mutex_lock(lock);
#define MEM_UNLOCK(lock)	mutex_unlock(lock);

#elif defined(KEY) /* Bug 6003 */

/* If we're not linking with -lpthread, then weak symbol pthread_mutex_lock
 * will be null, and we'll skip these operations, which are unnecessary in
 * the absence of threading. */
#pragma weak pthread_mutex_init
#pragma weak pthread_mutex_lock
#pragma weak pthread_mutex_unlock
#define MEM_LOCK_INIT		PTHREAD_MUTEX_INITIALIZER
#define MEM_LOCK(lock)          { if (pthread_mutex_lock) pthread_mutex_lock(lock); }
#define MEM_UNLOCK(lock)        { if (pthread_mutex_unlock) pthread_mutex_unlock(lock); }

#elif   defined(_LITTLE_ENDIAN) && !defined(__sv2)
/* do not use the following yet */
/* define MEM_LOCK(lock)          pthread_mutex_lock(lock) */
/* define MEM_UNLOCK(lock)        pthread_mutex_unlock(lock) */
#define MEM_LOCK(lock)
#define MEM_UNLOCK(lock)

#else
#define MEM_LOCK(lock)
#define MEM_UNLOCK(lock)

#endif

/*
 * LMEM_LOCK and LMEM_UNLOCK are general library locking macros.  They
 * take one argument:
 *
 *	lock - the address of the word used as a software lock.
 *
 * These macros are identical to MEM_LOCK and MEM_UNLOCK, except that
 * they do not check MULTI_ON.
 */
#if	defined(_CRAY1) || defined(_LIB_UMK)
#define LMEM_LOCK(lock)		{  _inline_mtlock(lock); }
#define LMEM_UNLOCK(lock)	{  _inline_mtunlock(lock); }

#else

#define LMEM_LOCK(lock)		MEM_LOCK(lock)
#define LMEM_UNLOCK(lock)	MEM_UNLOCK(lock)

#endif

/*
 * MPP_LOCK and MPP_UNLOCK are general library locking macros for
 * the MPP.  They take one argument:
 *
 *	lock - the address of the word used as a software lock.
 *
 * NOTE: the DECL_MPP_LOCK macro should be used to declare a lock word.
 */

#ifdef _CRAYMPP
#define MPP_LOCK(lock)		{ if (_num_pes() > 1) _shmem_set_lock(lock);   }
#define MPP_UNLOCK(lock)	{ if (_num_pes() > 1) _shmem_clear_lock(lock); }
#else
#define MPP_LOCK(lock)
#define MPP_UNLOCK(lock)
#endif

/*
 * LIB_LOCK and LIB_UNLOCK are currently used by libc for the
 * multi-tasking version of the library.  They take one argument:
 *
 *	index - index into the table of library locks
 *
 * NOTE:  This macro is only active if the MULTIP macro is set.
 */

#ifdef	MULTIP
#define LIB_LOCK(index)		MEM_LOCK(&__lib_locks[index])
#define LIB_UNLOCK(index)	MEM_UNLOCK(&__lib_locks[index])
#else
#define LIB_LOCK(index)
#define LIB_UNLOCK(index)
#endif

/*
 * This section has the indexes used by the LIB_LOCK macro.
 */

#define ENVIRON_LOCK		0	/* Environment lock	*/
#define TRACEBACK_LOCK		1	/* Traceback lock	*/
#define MESSAGE_LOCK		2	/* Message system lock	*/
#define LIB_LOCK_MAX		64	/* Size of lock array	*/

/*
 *	Externals section.
 */

#ifdef  MULTIP
extern plock_t __lib_locks[];	/* array of library locks */
#pragma common __lib_locks
#endif

#if	defined(_CRAY1) || defined(_LIB_UMK)

#pragma _CRI soft $MULTION
extern $MULTION(void);	/* must be a parcel reference */

#pragma _CRI soft _lockon
extern int _lockon(plock_t *);

#pragma _CRI soft _lockoff
extern int _lockoff(plock_t *);

#endif

/*
 *	Inline functions section.
 */

#define SIGNBIT	(1<<63)

/*
 * _inline_mtlock is functionally equivalent to _lockon (CX/CEA systems)
 * or _mtlock (CRAY-2 systems)..
 *
 * _inline_mtunlock is functionally equivalent to _lockoff (CX/CEA systems)
 * or _mtunlock (CRAY-2 systems).
 *
 * Because both are inline functions, they perform better than the other locking
 * functions when there is no lock contention.  The existence of lock 
 * contention is detected when:
 *
 *	1) _inline_mtlock detects a locked lock.  
 *  or	2) (CX/CEA systems) _inline_mtunlock detects a queue of tasks waiting
 *	   for this lock.
 *
 * When lock contention is detected the lower level lock management routines
 * are called.
 */
#if	defined(_CRAY1) || defined(_LIB_UMK)

extern void _semts(int);
extern void _semclr(int);
extern void _cmr(void);

#define TSKLK	2

#pragma _CRI inline _inline_mtlock
static void
_inline_mtlock(plock_t *lck)
{
	_semts(TSKLK);
/*
 *	Since lck's type has the "volatile" qualifier, the load of *lck will
 *	bypass cache on CRAY TS.
 */
	if (*lck != 0) {	/* Word is locked; put _lockon to work */
		_semclr(TSKLK);
		_lockon(lck);
	}
	else {			/* Lock word is unlocked */
		*lck = SIGNBIT;

#if	defined(_CRAY1) && defined(_ADDR64)	/* CRAY TS */
		/*
		 * Clear the data cache in preparation for the critical
		 * region we are about to enter.
		 */
		_Pragma("_CRI suppress");
#endif
		/* 
		 * Ensure that *lck is written to memory before letting
		 * another task read it.
		 */
		_cmr();
		_semclr(TSKLK);
	}
}

#pragma _CRI inline _inline_mtunlock
static void
_inline_mtunlock(plock_t *lck)
{
	_semts(TSKLK);
/*
 *	Since lck's type has the "volatile" qualifier, the load of *lck will
 *	bypass cache on CRAY TS.
 */
	if ((*lck & ~SIGNBIT) != 0) {
		/*
		 * Other tasks are waiting on the lock.  Let _lockoff take
		 * care of the details.
		 */
		_semclr(TSKLK);
		_lockoff(lck);
	}
	else {
		/*
		 * Assure that any previous memory stores issued by this CPU 
		 * are complete before  another CPU may obtain the lock.  
		 * Also, the lock word must be updated before release of the 
		 * semaphore because another task may be trying to queue 
		 * itself on this lock's waiting list.  The _cmr() takes 
		 * care of both.
		 */
		*lck = 0;
		_cmr();
		_semclr(TSKLK);
	}
}

#endif	/* _CRAY1 || _LIB_UMK */

#undef SIGNBIT			/* get rid of SIGNBIT definition */

/*
 *	KLUDGE - WORKAROUND - KLUDGE - WORKAROUND - KLUDGE -WORKAROUND
 *
 *	This is a workaround for two C compiler bugs:
 *
 *		SPR 80220  (fixed in SCC_M 4.0.2.5)
 *		SPR 703732 (opened in March '97)
 *
 *	Any module which includes cray/mtlock.h directly or indirectly and
 *	does not contain any functions must invoke this macro to ensure that
 *	a function exists in the C module.  This seems to successfully
 *	avoid the bug in the C compiler whereby a shared array is having
 *	a shared array be incorrectly set up.
 */
#ifdef	_CRAYT3D
#define SHARED_CC_BUG_WORKAROUND(name) void name(void) {}
#else
#define SHARED_CC_BUG_WORKAROUND(name) 
#endif

#endif	/* !_CRAY_MTLOCK_H */

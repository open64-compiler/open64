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


#pragma ident "@(#) libu/ffio/globutil.c	92.1	06/29/99 13:16:47"

#include <time.h>
#include <time.h>
#include <assert.h>
#ifdef	__mips
#include <sys/types.h>
#include <sys/mman.h>
#endif
#include "globio.h"

/****************************************************************************
 *
 * Constants
 *
 ***************************************************************************/
/*
 * Define constants from mpi.h if needed.
 */
#if	defined(__mips) && !defined(MPI_H_INCLUDED)
#define MPI_SUCCESS             0
#define MPI_MAX_ERROR_STRING    256
#define MPI_INT                 3
#endif

/****************************************************************************
 *
 * Data
 *
 ***************************************************************************/

glio_group_t _glio_curgroup;

/****************************************************************************
 *
 * External References
 *
 ***************************************************************************/

void *_sma_global_heap_alloc(size_t n);
void _sma_global_heap_free(void *p);

/****************************************************************************
 *
 * Forward References
 *
 ***************************************************************************/

void ckMPIerr(int value);

/****************************************************************************
 *
 * Functions
 *
 ***************************************************************************/

/*
 * _glio_delay
 *
 * Delay for nano nanoseconds.   This is useful in polling loops.
 */
void
_glio_delay(int nano)
{

#ifdef	__mips
	struct timespec ts;

	ts.tv_sec	= 0;
	ts.tv_nsec	= nano;
	nanosleep(&ts, NULL);
#else
	float	hertz;
	long	ticks, start;
	hertz	= IRTC_RATE();

	ticks	= hertz * (float)nano / 1000000000.0;

	start	= _rtc();
	/* if the clock goes backwards, stop spinning */
	while (_rtc() < (start+ticks) && _rtc() >= start) 
		continue;
#endif
	return;
}

/*
 * _glio_group_init
 *
 * Initialize a group based on the type of program we're called from.
 */
void
_glio_group_init(glio_group_t *gg)
{
	int	groupsz, myrank;

	if (gg->grtype != GR_DEFAULT) 
		return;

	switch (_glio_progtype()) {
		case GR_SHMEM:
			gg->grtype	= GR_SHMEM;
			gg->groupsz	= _num_pes();
			gg->myrank	= _my_pe();
			gg->u.shmem.group	= SHMEM_GROUP_WORLD;
			break;

		case GR_MPI:
			gg->grtype	= GR_MPI;

			ckMPIerr( MPI_Comm_size(MPI_COMM_WORLD, &groupsz) );
			gg->groupsz	= groupsz;

			ckMPIerr( MPI_Comm_rank(MPI_COMM_WORLD, &myrank) );
			gg->myrank	= myrank;

			gg->u.MPI.comm	= MPI_COMM_WORLD;
			break;

		default:
			gg->grtype	= GR_ONE;
			gg->groupsz	= 1;
			gg->myrank	= 0;
			break;
	}
}

#ifdef	__mips
/*
 * _glio_arena_create (IRIX version)
 *
 * Allocate the symmetric shared memory arena.
 * Allocate a shared memory arena.   The arena contains memory in sections,
 * with one section per process and all sections being of equal size.
 * "Symmetric" memory regions are allocated from these sections in a
 * manner analagous to shmalloc().  A table of offsets is kept by every
 * process to permit access to another process's copy of any data object
 * allocated in this arena.
 *
 * The arena sets aside enough memory at initialization time to satisfy
 * all allocation requests thereafter.
 *
 * Algorithm
 * ---------
 *
 *	1) Process with rank 0 opens a scratch file and initializes
 *	   it to the proper size.
 *	2) All processes mmap the file into their address space.
 */

glio_arena_t *
_glio_arena_create(
	glio_group_t	*gg,		/* group of processes to share access */
	size_t		asize)		/* arena size (bytes per process) */
{
	char	*fname, *fncpy;
	int	fd;
	int	groupsz;
	int	namelen;
	int	myrank;
	size_t	arena_size;
	glio_arena_t	*arp;
	shmem_group_t	shg;
	void	*aret;
	MPI_Comm	comm;
	MPI_Status	mpistatus;

	groupsz	= gg->groupsz;
	myrank	= gg->myrank;

	arp	= malloc(sizeof(*arp));
	if (arp == NULL) {
                fprintf(stderr,"%s:\n\
    _glio_arena_create(a) could not allocate a memory object of size %lld bytes\n",
			GLOBERRMSG, (long long)sizeof(*arp));
		abort();
	}
	bzero(arp, sizeof(*arp));

	arp->grp	= *gg;		/* copy it */
	gg		= &arp->grp;	/* point to the new copy */

	/* 
	 * Process with rank 0 finds a unique new file name to use as
	 * a memory mapped file.
	 */
	if (myrank == 0) {

		fname	= NULL;

		do {
			if (fname != NULL)
				free(fname);
			fname	= tempnam(NULL, "glio_arena");
			assert(fname != NULL);
			fd	= open(fname, O_CREAT | O_EXCL | O_RDWR, 0700);
		} while (fd == -1 && errno == EEXIST);
	}
	

	/*
	 * Trivial groups of size 1 can be handled trivially.
	 */
	if (groupsz == 1)
		goto past_file_name_send;

	_glio_barrier(arp);

	/*
	 * Initialization
	 */
	switch (gg->grtype) {
	    case GR_SHMEM:
		if ( _shmem_group_inquire != NULL) {
			_shmem_group_inquire(arp->grp.u.shmem.group, &shg);
		} else {
		    /* Special case for pre-release versions of MPT 1.2 */
		    static int	*world_plist;
		    static int	*world_racom;

		    /* if pre-release version of MPT 1.2 is used, then all */
		    /* PEs are in the group */
		    assert (groupsz == _num_pes());

                    if (world_plist == NULL) {
			register short	ipe;

                        world_plist	= malloc(_num_pes() * sizeof(int));
			if (world_plist == NULL) {
                		fprintf(stderr,"%s:\n\
    _glio_arena_create(b) could not allocate a memory object of size %lld bytes\n",
					GLOBERRMSG, (long long)(_num_pes() *
					sizeof(int)));
				abort();
			}

			world_racom	= shmalloc(SHMEM_GROUP_COM_SIZE *
					sizeof(int));
                        assert(world_racom != NULL);
			bzero(world_racom, 10*sizeof(int));

                        for (ipe = 0; ipe < _num_pes(); ipe++)
                                world_plist[ipe]	= ipe;
                    }

                    shg.groupsz	= _num_pes();
                    shg.myrank	= _my_pe();
		    shg.plist	= world_plist;
		    shg.racom	= world_racom;

		}
		break;

	    case GR_MPI:
		comm	= arp->grp.u.MPI.comm;
		break;

	    default:
		break;
	}

	/*
	 * Process 0 now must communicate the file name to all other 
	 * processes in the group.
	 */

	switch (gg->grtype) {
	    case GR_SHMEM:
		if (myrank == 0) {
			void	*vp;

			fncpy	= _sma_global_heap_alloc(strlen(fname)+1);
			assert(fncpy != NULL);
			strcpy(fncpy, fname); 

			vp	= fncpy;
			
			/* racom[1] gets string length */
			shg.racom[1]	= strlen(fname);

			/* racom[2]  and racom[3] get the pointer */
			/* to the string.			    */
			memcpy(&shg.racom[2], &vp, sizeof(vp));
		}

		_glio_barrier(arp);
			
		/* 
		 * Other processes now get the file name.
		 */
		if (myrank != 0) {
			void	*vp;

			namelen	= _shmem_int_g( &shg.racom[1], shg.plist[0]);
			assert(namelen > 0);

			/* get pointer to the string */
			_shmem_getmem(&vp, &shg.racom[2], sizeof(vp),
				shg.plist[0]);

			fname	= malloc(namelen + 1);
			if (fname == NULL) {
                		fprintf(stderr,"%s:\n\
    _glio_arena_create(c) could not allocate a memory object of size %lld bytes\n",
				GLOBERRMSG, (long long)(namelen + 1));
				abort();
			}

			/* copy the string */
			_shmem_getmem(fname, vp, namelen, shg.plist[0]);
			fname[namelen]	= '\0';
		}

		_glio_barrier(arp);

		if (myrank == 0) {
			_sma_global_heap_free(fncpy);
		}

		break;

	    case GR_MPI:
		if (myrank == 0) {
			register int	rank;

			namelen	= strlen(fname);

			for (rank = 1; rank < groupsz; rank++) {
                		ckMPIerr( MPI_Send(&namelen, 1, MPI_INT, 
					rank, 1, comm) );
			}

			for (rank = 1; rank < groupsz; rank++) {
                		ckMPIerr( MPI_Send(fname, namelen, MPI_CHAR, 
					rank, 2, comm) );
			}
		} else {
                	ckMPIerr( MPI_Recv(&namelen, 1, MPI_INT, 
					0, 1, comm, &mpistatus) );

			fname	= malloc(namelen + 1);
			if (fname == NULL) {
                		fprintf(stderr,"%s:\n\
    _glio_arena_create(d) could not allocate a memory object of size %lld bytes\n",
				GLOBERRMSG, (long long)(namelen + 1));
				abort();
			}

                	ckMPIerr( MPI_Recv(fname, namelen, MPI_CHAR, 
					0, 2, comm, &mpistatus) );

			fname[namelen]	= '\0';
		}

		break;

	    default:
		assert(0);
	}


	_glio_barrier(arp);

	/* 
	 * Non-rank-0 processes now open the file.
	 */
	if (myrank != 0) {
		fd	= open(fname, O_RDWR, 0700);
		if (fd == -1) {
			fprintf(stderr, "%s:\n\
Global I/O failed to open mapped file.  Errno is %d\n",
				GLOBERRMSG, errno);
			abort();
		}
	}

	_glio_barrier(arp);

past_file_name_send:
	/*
 	 * All processes have opened the file, so rank 0 may now unlink it.
	 */
	if (myrank == 0) {
		unlink(fname);
	}

	_glio_barrier(arp);

	/*
	 * After the barrier process 0 may initialize the mapped
	 * file and unlink it because we know that all processes in the
	 * group have now opened this file.
	 */
	arena_size	= groupsz * CEILING(asize, 1024);

	if (myrank == 0) {
		ssize_t	wret;

		wret	= pwrite(fd, " ", 1, arena_size - 1);
		assert(wret != -1L);
	}

	_glio_barrier(arp);

	/*
	 * A barrier assures us that the file has been initialized
	 * to the right size.   Now map the file into every process'
	 * address space.
	 */

	aret	= mmap64(NULL, arena_size, PROT_READ | PROT_WRITE, MAP_SHARED,
                        fd, 0);
	if (aret == MAP_FAILED) {
                fprintf(stderr,"%s:\n\
    Cannot map internal file %s\n\
    for shared memory arena.  Error = %d\n",
			GLOBERRMSG, fname, errno);
		abort();
	}

	free(fname);

	arp->asize	= arena_size / gg->groupsz;
	arp->ashm	= aret;
	arp->mybase	= (char *)aret + gg->myrank * arp->asize;
	arp->freespace	= arp->mybase;

	return(arp);
} 
#endif /* __mips */

#ifdef	_CRAY
/*
 * _glio_arena_create (T3E version)
 *
 * Simply allocate the needed memory on the local heap.
 */
glio_arena_t *
_glio_arena_create(
	glio_group_t *gg,		/* group of processes to share access */
	size_t	asize)			/* arena size (bytes per process) */
{
	int	rank;
	int	groupsz, myrank;
	glio_arena_t *arp;
	struct base_pe *bpelst;
	shmem_group_t shg;
	MPI_Comm comm;
	MPI_Status mpistatus;
	struct {
		long addr;
		long pe;
	} message;

	groupsz	= gg->groupsz;
	myrank	= gg->myrank;
	arp	= malloc(sizeof(*arp));

	if (arp == NULL) {
                fprintf(stderr,"%s:\n\
    _glio_arena_create(A) could not allocate a memory object of size %lld bytes\n",
			GLOBERRMSG, (long long)sizeof(*arp));
		abort();
	}

	bzero(arp, sizeof(*arp));

	arp->grp	= *gg;		/* copy it */
	gg		= &arp->grp;	/* point to the new copy */
	arp->mybase	= malloc(asize);

	if (arp->mybase == NULL) {
                fprintf(stderr,"%s:\n\
    _glio_arena_create(B) could not allocate a memory object of size %lld bytes\n",
			GLOBERRMSG, (long long)asize);
		abort();
	}

	bzero(arp->mybase, asize);
	arp->freespace	= arp->mybase;
	bpelst		= malloc(groupsz * sizeof(bpelst[0]));

	if (bpelst == NULL) {
                fprintf(stderr,"%s:\n\
    _glio_arena_create(C) could not allocate a memory object of size %lld bytes\n",
			GLOBERRMSG, (long long)(groupsz * sizeof(bpelst[0])));
		abort();
	}

	arp->peshm	= bpelst;

	/*
	 * Now all processes share their addresses.
	 */
	switch (gg->grtype) {
	    case GR_MPI:

		comm	= arp->grp.u.MPI.comm;
		/*
		 * Each process sends its address to every other process.
		 */

		for (rank = 0; rank < groupsz; rank++) {	/* send loop */

			message.addr	= (long)arp->mybase;
			message.pe	= _my_pe();
                	ckMPIerr( MPI_Send(&message, 2, MPI_LONG, rank, 0, comm));
		}

		for (rank = 0; rank < groupsz; rank++) {	/* recv loop */

                	ckMPIerr( MPI_Recv(&message, 2 , MPI_LONG, 
					rank, 0, comm, &mpistatus) );
			bpelst[rank].pe		= message.pe;
			bpelst[rank].base	= (void *) message.addr;
		}
		break;


	    case GR_SHMEM:

		/*
		 * We iterate once for each process.  Each time through
		 * the loop, a different process stores its address to 
		 * racom[1], we do a barrier sync, and then all PEs
		 * get the address from racom[1].  
		 */
		_shmem_group_inquire(arp->grp.u.shmem.group, &shg);

		for (rank = 0; rank < groupsz; rank++) {

			bpelst[rank].pe	= shg.plist[rank];

			if (myrank == rank) {
			    _shmem_putmem( &shg.racom[1], &arp->mybase,
				sizeof(bpelst[0].base), shg.plist[0]);
			}

			_sma_group_barrier(arp->grp.u.shmem.group);	

			_shmem_getmem( &bpelst[rank].base, &shg.racom[1],
				sizeof(bpelst[0].base), shg.plist[0]);

			_sma_group_barrier(arp->grp.u.shmem.group);	

		}
		break;

	    default:
		assert(0);
	}
	return(arp);
} 
#endif /* _CRAY */

/*
 * _glio_arena_destroy
 *
 * Deallocate an arena.
 */
void
_glio_arena_destroy(glio_arena_t *arp)
{
#ifdef	__mips

	munmap(arp->ashm, arp->asize * arp->grp.groupsz);

#else	/* _CRAY */

	if (arp->peshm != NULL)
		free (arp->peshm);
#endif
	free(arp);

	return;
}

/*
 * _glio_barrier
 *
 * Performs a barrier synchronization on all processes associated with
 * a global file.
 */
void
_glio_barrier(glio_arena_t *arp)
{
	if (arp->grp.groupsz == 1)
		return;

	switch (arp->grp.grtype) {
		case GR_SHMEM:	
			if (arp->grp.groupsz == _num_pes()) {
				_shmem_barrier_all();
			}
			else {
				_sma_group_barrier(arp->grp.u.shmem.group);	
			}
			break;

		case GR_MPI:
			MPI_Barrier(arp->grp.u.MPI.comm);	
			break;

		default:	
			tracebk();
			assert(0);	/* not implemented */
	}
	return;
}

/*
 * _glio_rtc_rate
 *
 * Returns the hertz rate for the _rtc() timer.
 */
long long
_glio_rtc_rate(void)
{
#ifdef	__mips
	extern double	_nowrap_cycles_per_sec;

        if (_nowrap_cycles_per_sec == 0)
                _init_hw_clock();

        return((long long) _nowrap_cycles_per_sec);
#else
	return(IRTC_RATE());
#endif
}

/*
 * _glio_progtype
 *
 * Determine if this is a SHMEM or MPI program.
 */
enum progtype 
_glio_progtype()
{
#ifdef	__mips
	int	flag;

	if (MPI_Init != NULL) {

		flag	= 0;
		MPI_Initialized(&flag);
		if (flag)
			return(GR_MPI);
		else
			return(GR_ONE);

	} else if (start_pes != NULL) {
		return(GR_SHMEM);

	}


	return((enum progtype) 0);

#elif	defined(_CRAYT3E)

	return(GR_SHMEM);
#endif
}

/*
 * More emulated SHMEM functions follow.
 */
void
_glio_shmem_getmem(glio_arena_t *arp, void *trg, void *src, size_t bytes,
	int rank)
{
#ifdef	_CRAYT3E
	_shmem_getmem(trg, REMPTR(arp, src, rank), bytes, REMPE(rank));
#elif	defined(__mips)
	memcpy(trg, REMPTR(arp, src, rank), bytes);
#endif
	return;
}

void
_glio_shmem_putmem(glio_arena_t *arp, void *trg, void *src, size_t bytes,
	int rank)
{
#ifdef	_CRAYT3E
	_shmem_putmem(REMPTR(arp, trg, rank), src, bytes, REMPE(rank));
#elif	defined(__mips)
	memcpy(REMPTR(arp, trg, rank), src, bytes);
#endif
	return;
}

void
_glio_shmem_get64(glio_arena_t *arp, void *trg, void *src, size_t len,
        int rank)
{
#ifdef	_CRAYT3E
	_shmem_get64(trg, REMPTR(arp, src, rank), len, REMPE(rank));
#elif	defined(__mips)
	memcpy(trg, REMPTR(arp, src, rank), len*64/8);
#endif
	return;
}

void
_glio_shmem_put64(glio_arena_t *arp, void *trg, void *src, size_t len,
        int rank)
{
#ifdef	_CRAYT3E
	_shmem_put64(REMPTR(arp, trg, rank), src, len, REMPE(rank));
#elif	defined(__mips)
	memcpy(REMPTR(arp, trg, rank), src, len*64/8);
#endif
	return;
}

void
_glio_shmem_get32(glio_arena_t *arp, void *trg, void *src, size_t len,
        int rank)
{
#ifdef	_CRAYT3E
        _shmem_get32(trg, REMPTR(arp, src, rank), len, REMPE(rank));
#elif	defined(__mips)
        memcpy(trg, REMPTR(arp, src, rank), len*32/8);
#endif
	return;
}

void
_glio_shmem_put32(glio_arena_t *arp, void *trg, void *src, size_t len,
        int rank)
{
#ifdef	_CRAYT3E
        _shmem_put32(REMPTR(arp, trg, rank), src, len, REMPE(rank));
#elif	defined(__mips)
        memcpy(REMPTR(arp, trg, rank), src, len*32/8);
#endif
	return;
}

void
_glio_shmem_int_get(glio_arena_t *arp, int *trg, int *src, size_t len,
        int rank)
{
#ifdef	_CRAYT3E
	_shmem_int_get(trg, REMPTR(arp, src, rank), len, REMPE(rank));
#elif	defined(__mips)
	memcpy(trg, REMPTR(arp, src, rank), len*sizeof(*trg));
#endif
	return;
}

void
_glio_shmem_int_put(glio_arena_t *arp, int *trg, int *src, size_t len,
        int rank)
{
#ifdef	_CRAYT3E
        _shmem_int_put(REMPTR(arp, trg, rank), src, len, REMPE(rank));
#elif	defined(__mips)
        memcpy(REMPTR(arp, trg, rank), src, len*sizeof(*trg));
#endif
	return;
}

void
_glio_shmem_long_get(glio_arena_t *arp, long *trg, long *src, size_t len,
        int rank)
{
#ifdef	_CRAYT3E
        _shmem_long_get(trg, REMPTR(arp, src, rank), len, REMPE(rank));
#elif	defined(__mips)
        memcpy(trg, REMPTR(arp, src, rank), len*sizeof(*trg));
#endif
	return;
}

void
_glio_shmem_long_put(glio_arena_t *arp, long *trg, long *src, size_t len,
        int rank)
{
#ifdef	_CRAYT3E
        _shmem_long_put(REMPTR(arp, trg, rank), src, len, REMPE(rank));
#elif	defined(__mips)
        memcpy(REMPTR(arp, trg, rank), src, len*sizeof(*trg));
#endif
	return;
}

void
_glio_shmem_int_p(glio_arena_t *arp, int *trg, int val, int rank)
{
#ifdef	_CRAYT3E
	_shmem_int_p(REMPTR(arp, trg, rank), val, REMPE(rank));
#elif	defined(__mips)
	*(int*)(REMPTR(arp, trg, rank))	= val;
#endif
	return;
}

void
_glio_shmem_long_p(glio_arena_t *arp, long *trg, long val, int rank)
{
#ifdef	_CRAYT3E
        _shmem_long_p(REMPTR(arp, trg, rank), val, REMPE(rank));
#elif	defined(__mips)
        *(long*)(REMPTR(arp, trg, rank))	= val;
#endif
	return;
}

void
_glio_set_lock(glio_arena_t *arp, long *lock)
{
#ifdef	_CRAY
	while (_shmem_long_swap(REMPTR(arp, lock, 0), 1L, REMPE(0)) == 1L)
			continue;
#elif	defined(__mips)
	while (__lock_test_and_set((long*)REMPTR(arp, lock, 0), 1L) == 1L)
			sginap(1);
#endif
	return;
}

void 
_glio_shmem_wait(long *addr, long value)
{
#ifdef	_CRAY
	_shmem_wait(addr, value);
#else
	while (*(volatile long *)addr == value)
		_glio_delay(1000);
#endif
	return;
}

void
_glio_clear_lock(glio_arena_t *arp, long *lock)
{
	_glio_shmem_quiet();
	_glio_shmem_long_p(arp, lock, 0, 0);
	return;
}

/*
 *	ALLOCALIGN is the desired alignment of the allocated memory.
 *	On T3E systems we must align on a 64 byte boundary for I/O
 *	using O_WELLFORMED to work.
 */
#ifdef	_CRAYT3E
#define ALLOCALIGN 64		/* byte size of an Scache line */
#else
#define ALLOCALIGN (sizeof(long long))
#endif

/*
 *	MAXPAD is the largest number of additional bytes ever allocated for
 *	alignment purposes.
 */
#define MAXPAD 		(STRMPAD + ALLOCALIGN - 1)

/*
 *	_glio_malloc, _glio_strdup
 *
 *	Wrappers around malloc/strdup.  An extra STRMPAD 
 *	bytes are allocated at the front of the request for T3E streams 
 *	coherency padding.    The memory allocated is kept in a list for
 *	automatic deallocation at file close time.
 */
void *
_glio_malloc(struct glob_f *glob_info, size_t size)
{
	void	*ptr;

	ptr	= malloc(MAXPAD + size);

	if (ptr == NULL) {
                fprintf(stderr,"%s:\n\
    _glio_malloc() could not allocate a memory object of size %lld bytes\n",
			GLOBERRMSG, (long long)size);
		abort();
	}

	assert(glob_info->memlistsz < MAXAL);
	glob_info->memlist[glob_info->memlistsz++]	= ptr;
	ptr	= _glio_padmem(ptr);

	return(ptr);
}

char *
_glio_strdup(struct glob_f *glob_info, const char *str)
{
	int	size;
	char	*c;

	size	= strlen(str) + 1 + MAXPAD;
	c	= _glio_malloc(glob_info, size);
        if (c != NULL) 
		strcpy(c, str);

	return(c);
}

/*
 * _glio_padmem
 *
 * Take a pointer and pad it to memory boundary which satisfies:
 *		1) 8 byte alignment
 *		2) additional pad needed on T3E systems for stream safety
 */

void 
*_glio_padmem(void *optr)
{
	void	*nptr;

	nptr	= (char *) optr + MAXPAD;
	nptr	= (char *) ((long long)nptr / ALLOCALIGN * ALLOCALIGN);

	return (nptr);
}

/*
 * ckMPIerr
 *
 * Checks the return value of MPI functions.
 */
void
ckMPIerr(int value)
{
        char	errstr[MPI_MAX_ERROR_STRING];
        int	errstrlen;

        if (value != MPI_SUCCESS) {
                MPI_Error_string(value, errstr, &errstrlen);
                fprintf(stderr,"%s:\n\
    glio_group_MPI received MPI error %d:\n\
    %s.\n", GLOBERRMSG, value, errstr);
                abort();
        }
	return;
}

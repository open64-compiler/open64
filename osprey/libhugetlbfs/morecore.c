/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * libhugetlbfs - Easy use of Linux hugepages
 * Copyright (C) 2005-2006 David Gibson & Adam Litke, IBM Corporation.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <dlfcn.h>
#include <string.h>
#include <fcntl.h>
#include <sys/uio.h>

#include "hugetlbfs.h"

#include "libhugetlbfs_internal.h"

static int heap_fd;
static int shrink_ok;		/* default = 0; no shrink */
static int zero_fd;
static long blocksize;

#ifndef OPEN64_MOD
static void *heapbase;
static void *heaptop;
static long mapsize;
#endif

#ifdef OPEN64_MOD
#define MAX_TOP_PAD (1024*1024)
void *heapbase;
void *heaptop;
long mapsize;
unsigned long newbrk = 0;

HUGEPAGE_STYPE hugepage_m_stype = SIZE_2M;   /* type of huge page to match */
HUGEPAGE_STYPE hugepage_elf_stype = SIZE_2M; /* type of huge page to map segments */
HUGEPAGE_STYPE hugepage_heap_stype = SIZE_2M; /* type of huge page to map heap */
HUGEPAGE_STYPE hugepage_s_stype = SIZE_2M; /* type of huge page supported by the system */

long hugepages_heap_limit;
long hugepages_seg_total;
int heap_starts_in_bd;
static long hugepages_avail;
#endif

static long hugetlbfs_next_addr(long addr)
{
#if defined(__powerpc64__)
	return ALIGN(addr, 1L << SLICE_HIGH_SHIFT);
#elif defined(__powerpc__)
	return ALIGN(addr, 1L << SLICE_LOW_SHIFT);
#elif defined(__ia64__)
	if (addr < (1UL << SLICE_HIGH_SHIFT))
		return ALIGN(addr, 1UL << SLICE_HIGH_SHIFT);
	else
		return ALIGN(addr, gethugepagesize());
#else
	return ALIGN(addr, gethugepagesize());
#endif
}

/*
 * Our plan is to ask for pages 'roughly' at the BASE.  We expect and
 * require the kernel to offer us sequential pages from wherever it
 * first gave us a page.  If it does not do so, we return the page and
 * pretend there are none this covers us for the case where another
 * map is in the way.  This is required because 'morecore' must have
 * 'sbrk' semantics, ie. return sequential, contigious memory blocks.
 * Luckily, if it does not do so and we error out malloc will happily
 * go back to small pages and use mmap to get them.  Hurrah.
 */
#define IOV_LEN	64
#ifdef OPEN64_MOD
/* Took out (hugepage_heap_stype == hugepage_elf_stype)
 * From IN_UNIFY_PAGE, since we will always be initially allocating
 * memory from the rest of a 1GB page used to map BD.
 */
#define IN_UNIFY_PAGE  ((hugepage_elf_stype == SIZE_1G) && \
                        (newbrk > 0) && \
                        ((unsigned long) heaptop <= newbrk))
#endif
static void *hugetlbfs_morecore(ptrdiff_t increment)
{
	unsigned long offset;
	int i;
	struct iovec iov[IOV_LEN];
	int ret;
	void *p;
	long delta;

#ifdef OPEN64_MOD
        long p_delta = 0;
        DEBUG("brk=0x%lx\n",(unsigned long) sbrk(0));
#endif

	DEBUG("hugetlbfs_morecore(%ld) = ...\n", (long)increment);

	/*
	 * how much to grow the heap by =
	 * 	(size of heap) + malloc request - mmap'd space
	 */
	delta = (heaptop-heapbase) + increment - mapsize;

	DEBUG("heapbase = %p, heaptop = %p, mapsize = %lx, delta=%ld\n",
	      heapbase, heaptop, mapsize, delta);

#ifdef OPEN64_MOD
        if (IN_UNIFY_PAGE) {
            if (delta < 0) {
                p = heaptop;
                heaptop = heaptop + increment;
		DEBUG("... = %p\n", p);
                return p;
            }
            else {
                /* transition point from BD page */
                p_delta = increment - delta;
                heaptop = heapbase = (void *) newbrk;
                mapsize = 0;
                DEBUG("in transition p_delta = %ld, heaptop = %p\n",
                      p_delta, heaptop);
            }
        }
#endif

	/* align to multiple of hugepagesize. */
	delta = ALIGN(delta, blocksize);

	if (delta > 0) {
                /* growing the heap */
                DEBUG("Attempting to map %ld bytes\n", delta);

#ifdef OPEN64_MOD
                if ((long long) delta + mapsize > (long long) hugepages_heap_limit * blocksize) {
                    DEBUG("size %ld exceeds huge page limit %ld\n",
                          delta + mapsize, hugepages_heap_limit);
                    __morecore = &__default_morecore;
                    DEBUG("brk=0x%lx\n",(unsigned long) sbrk(0));
                    return NULL;
                }
#endif

		/* map in (extend) more of the file at the end of our last map */
#ifdef OPEN64_MOD
                if (sbrk(delta) == (void *)-1l){
			WARNING("New heap segment map at %p failed: %s\n",
	                	heapbase+mapsize, strerror(errno));
	 		return NULL;
		}
		p = mmap(heapbase + mapsize, delta, PROT_READ|PROT_WRITE,
			 MAP_PRIVATE|MAP_FIXED, heap_fd, mapsize);
#else
                p = mmap(heapbase + mapsize, delta, PROT_READ|PROT_WRITE,
                         MAP_PRIVATE, heap_fd, mapsize);
#endif
		if (p == MAP_FAILED) {
			WARNING("New heap segment map at %p failed: %s\n",
				heapbase+mapsize, strerror(errno));
			return NULL;
		}

		/* if this is the first map */
		if (! mapsize) {
			if (heapbase && (heapbase != p)) {
				WARNING("Heap originates at %p instead of %p\n",
					p, heapbase);
				if (__hugetlbfs_debug)
					dump_proc_pid_maps();
			}
			/* then setup the heap variables */
			heapbase = heaptop = p;
		} else if (p != (heapbase + mapsize)) {
			/* Couldn't get the mapping where we wanted */
			munmap(p, delta);
			WARNING("New heap segment mapped at %p instead of %p\n",
			      p, heapbase + mapsize);
			if (__hugetlbfs_debug)
				dump_proc_pid_maps();
			return NULL;
		}

		/* The NUMA users of libhugetlbfs' malloc feature are
		 * expected to use the numactl program to specify an
		 * appropriate policy for hugepage allocation */

		/*
		 * Use readv(2) to instantiate the hugepages.  If we
		 * can't get all that were requested, release the entire
		 * mapping and return NULL.  Glibc malloc will then fall back
		 * to using mmap of base pages.
		 *
		 * If we instead returned a hugepage mapping with insufficient
		 * hugepages, the VM system would kill the process when the
		 * process tried to access the missing memory.
		 */

		if (__hugetlbfs_prefault) {
			for (offset = 0; offset < delta; ) {
				for (i = 0; i < IOV_LEN && offset < delta; i++) {
					iov[i].iov_base = p + offset;
					iov[i].iov_len = 1;
					offset += blocksize;
				}
				ret = readv(zero_fd, iov, i);
				if (ret != i) {
					DEBUG("Got %d of %d requested; err=%d\n", ret,
							i, ret < 0 ? errno : 0);
					WARNING("Failed to reserve %ld huge pages "
							"for heap\n", delta/blocksize);
					munmap(p, delta);
					return NULL;
				}
			}
		}

		/* we now have mmap'd further */
		mapsize += delta;
	} else if (delta < 0) {
		/* shrinking the heap */

		if (!shrink_ok) {
			/* shouldn't ever get here */
			WARNING("Heap shrinking is turned off\n");
			return NULL;
		}

		if (!mapsize) {
			WARNING("Can't shrink empty heap!\n");
			return NULL;
		}

		/*
		 * If we are forced to change the heapaddr from the
		 * original brk() value we have violated brk semantics
		 * (which we are not supposed to do).  This shouldn't
		 * pose a problem until glibc tries to trim the heap to an
		 * address lower than what we aligned heapaddr to.  At that
		 * point the alignment "gap" causes heap corruption.
		 * So we don't allow the heap to shrink below heapbase.
		 */
		if (mapsize + delta < 0) {  /* remember: delta is negative */
			WARNING("Unable to shrink heap below %p\n", heapbase);
			/* unmap just what is currently mapped */
			delta = -mapsize;
			/* we need heaptop + increment == heapbase, so: */
			increment = heapbase - heaptop;
		}
		DEBUG("Attempting to unmap %ld bytes @ %p\n", -delta,
			heapbase + mapsize + delta);
		ret = munmap(heapbase + mapsize + delta, -delta);
		if (ret) {
			WARNING("Unmapping failed while shrinking heap: "
				"%s\n", strerror(errno));
		} else {

			/*
			 * Now shrink the hugetlbfs file.
			 */
			mapsize += delta;
			ret = ftruncate(heap_fd, mapsize);
			if (ret) {
				WARNING("Could not truncate hugetlbfs file to "
					"shrink heap: %s\n", strerror(errno));
			}
		}

	}

	/* heap is continuous */

	p = heaptop;

#ifdef OPEN64_MOD
        p -= p_delta;
        heaptop -= p_delta;
#endif

	/* and we now have added this much more space to the heap */
	heaptop = heaptop + increment;
	DEBUG("... = %p\n", p);

	return p;
}

void __hugetlbfs_setup_morecore(void)
{
	char *env, *ep;
	unsigned long heapaddr;
#ifdef OPEN64_MOD
        unsigned long curbrk = (unsigned long) sbrk(0);
	long top_pad;
#endif
	env = getenv("HUGETLB_MORECORE");

#ifndef OPEN64_MOD
	if (! env)
		return;
#endif
	if (env && strcasecmp(env, "no") == 0) {
		DEBUG("HUGETLB_MORECORE=%s, not setting up morecore\n",
								env);
		return;
	}

	/*
	 * We have been seeing some unexpected behavior from malloc when
	 * heap shrinking is enabled, so heap shrinking is disabled by
	 * default.
	 *
	 * If malloc has been called successfully before setup_morecore,
	 * glibc will notice a gap between the previous top-of-heap and
	 * the new top-of-heap when it calls hugetlbfs_morecore.  It treats
	 * this as a "foreign sbrk."  Unfortunately, the "foreign sbrk"
	 * handling code will then immediately try to free the memory
	 * allocated by hugetlbfs_morecore!
	 *
	 * This behavior has been reported to the ptmalloc2 maintainer,
	 * along with a patch to correct the behavior.
	 */
	env = getenv("HUGETLB_MORECORE_SHRINK");
	if (env && strcasecmp(env, "yes") == 0)
		shrink_ok = 1;

	blocksize = gethugepagesize();
	if (blocksize <= 0) {
		if (errno == ENOSYS)
			ERROR("Hugepages unavailable\n");
		else if (errno == EOVERFLOW)
			ERROR("Hugepage size too large\n");
		else
			ERROR("Hugepage size (%s)\n", strerror(errno));
		return;
	}

	heap_fd = hugetlbfs_unlinked_fd();
	if (heap_fd < 0) {
		ERROR("Couldn't open hugetlbfs file for morecore\n");
		return;
	}

	env = getenv("HUGETLB_MORECORE_HEAPBASE");
	if (env
#ifdef OPEN64_MOD
            && ! heap_starts_in_bd
#endif
            ) {
		heapaddr = strtoul(env, &ep, 16);
		if (*ep != '\0') {
			ERROR("Can't parse HUGETLB_MORECORE_HEAPBASE: %s\n",
			      env);
			return;
		}
	}
#ifndef OPEN64_MOD
	else {
		heapaddr = (unsigned long)sbrk(0);
		heapaddr = hugetlbfs_next_addr(heapaddr);
	}
#else	
        else if (! heap_starts_in_bd) {
            if ((heapbase > 0) && (hugepage_heap_stype == hugepage_elf_stype)) {
                heapaddr = (unsigned long) heapbase;
                mapsize = (unsigned long) sbrk(0) - heapaddr;
            }
            else {
                heapaddr = (unsigned long)sbrk(0);
                heapaddr = hugetlbfs_next_addr(heapaddr);
            }
        }
#endif

#ifdef OPEN64_MOD
        if (! heap_starts_in_bd && heapaddr > curbrk)
            sbrk(heapaddr - curbrk);
#endif
	zero_fd = open("/dev/zero", O_RDONLY);
#ifdef OPEN64_MOD
	if (! heap_starts_in_bd) {
#endif
	    DEBUG("setup_morecore(): heapaddr = 0x%lx\n", heapaddr);

	    heaptop = heapbase = (void *)heapaddr;
	    __morecore = &hugetlbfs_morecore;
#ifdef OPEN64_MOD
	}
#endif

	/* Set some allocator options more appropriate for hugepages */

#ifndef OPEN64_MOD
        if (shrink_ok)
            mallopt(M_TRIM_THRESHOLD, blocksize / 2);
        else
            mallopt(M_TRIM_THRESHOLD, -1);

        mallopt(M_TOP_PAD, blocksize / 2);
#else
	top_pad = blocksize / 2;
        if (top_pad > MAX_TOP_PAD)
		top_pad = MAX_TOP_PAD;
        if (shrink_ok)
            mallopt(M_TRIM_THRESHOLD, top_pad);
        else
            mallopt(M_TRIM_THRESHOLD, -1);

        mallopt(M_TOP_PAD, top_pad);
#endif
        /* we always want to use our morecore, not ordinary mmap().
         * This doesn't appear to prohibit malloc() from falling back
         * to mmap() if we run out of hugepages. */
        
        mallopt(M_MMAP_MAX, 0);
}

#ifdef OPEN64_MOD
/*  Customize heap allocation, interface to the compiler.
 *
 *  Environment variable overrides command line option in setting limit of huge pages to use
 *  for the heap. 
 *
 *    Bit mask of input attr:
 *    bit 0: unused
 *    bit 1: unused 
 *    bit 2: unused 
 *    bit 3: heap 2M page
 *    bit 4: heap 1G page
 *    bit 5: unused 
 *    bit 6: unused
 */

#define MALLOPT_MASK 0x7
#define HEAP_2M_MASK 0x8
#define HEAP_1G_MASK 0x10

void  __setup_hugepage(int l_limit, int attr)
{
    if (attr & HEAP_1G_MASK)
        hugepage_heap_stype = SIZE_1G;
    else if (attr & HEAP_2M_MASK)
        hugepage_heap_stype = SIZE_2M;

    DEBUG("%s page for heap from input\n", hugepage_stype_name[hugepage_heap_stype]);

    if (hugepage_heap_stype > hugepage_s_stype) {
        hugepage_heap_stype = hugepage_s_stype;
        DEBUG("Downgrade to %s page for heap\n", hugepage_stype_name[hugepage_heap_stype]);
    }

    hugepage_m_stype = hugepage_heap_stype;

    hugepages_avail = hugetlbfs_num_pages();

    hugepages_heap_limit = hugepages_avail;

    if (hugepages_avail > 0) {
        char *env;
        
        env = getenv("HUGETLB_LIMIT");
        
        if ( env ) {
            long n = atol(env);
            if( (n >= 0) && (n < hugepages_avail) ) 
                hugepages_heap_limit = n;
        }
        else if ((l_limit >= 0) && (l_limit < hugepages_avail))
            hugepages_heap_limit = l_limit;        

        if (hugepage_heap_stype == hugepage_elf_stype)
            hugepages_heap_limit -= hugepages_seg_total;

        if (hugepages_heap_limit < 0)
            hugepages_heap_limit = 0;

        DEBUG("Limit %ld huge pages for heap.\n", hugepages_heap_limit);

        if (hugepages_heap_limit > 0)
            __hugetlbfs_setup_morecore();
    }
}

/* Set up to allow heap allocation for
 * the rest of a HUGE page mapped BD section.
 */

void __hugetlbfs_setup_bd_morecore(void)
{
        long top_pad;

	blocksize = gethugepagesize();
        heap_starts_in_bd = 1;
	__morecore = &hugetlbfs_morecore;

        mallopt(M_TRIM_THRESHOLD, -1);
	top_pad = blocksize / 2;
        if (top_pad > MAX_TOP_PAD)
		top_pad = MAX_TOP_PAD;
	mallopt(M_TOP_PAD, top_pad);
	mallopt(M_MMAP_MAX, 0);
}
#endif

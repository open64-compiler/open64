/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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



#include <sys/types.h>
#include <malloc.h>
#include <bstring.h>
#include "defs.h"
#include "mempool.h"
#include "tracing.h"
#include "erglob.h"

#ifdef KEY
#ifndef NO_VALGRIND
#include <memcheck.h>

/*
 * Check to see if we have an old version of Valgrind without mempool
 * support.  Disable Valgrind support if this is the case.
 */

#ifndef VALGRIND_CREATE_MEMPOOL
#define NO_VALGRIND
#endif

#endif /* NO_VALGRIND */
#endif /* KEY */

#ifdef NO_VALGRIND
#define REDZONE_SIZE 0
#else
static int redzone_size = 0;
#define REDZONE_SIZE redzone_size
#endif /* NO_VALGRIND */

/* The value of the following macro must be distinct from TRUE and FALSE
 * as defined in defs.h. The macro is used only in the case that
 * PURIFY_MEMPOOLS is switched on (ON, ON-TRACE, or ON-TRACE-X).
 */
#define MEM_POOL_INIT_IN_PROGRESS (-1)

/* -----------------------------------------------------------------
 * Compile time parameters
 * ----------------------------------------------------------------- */
/* BLOCK_SIZE - This is the size of blocks for small object
 */
#define BLOCK_SIZE 0x2000

/* MIN_LARGE_BLOCK_SIZE - objects larger than this size goes to the large
   block list
   */
#define MIN_LARGE_BLOCK_SIZE 0x800

/* if ZAP_ON_FREE is defined, we fill freed memory with garbage
 */
#ifdef Is_True_On
#define ZAP_ON_FREE (1)
#endif


/* -----------------------------------------------------------------
 * Exported variables
 * -----------------------------------------------------------------
 */

/* These pools zero newly-allocated memory.
 */
MEM_POOL MEM_local_pool;
MEM_POOL MEM_src_pool;
MEM_POOL MEM_pu_pool;
MEM_POOL MEM_phase_pool;

MEM_POOL *MEM_local_pool_ptr = &MEM_local_pool;
MEM_POOL *MEM_src_pool_ptr = &MEM_src_pool;
MEM_POOL *MEM_pu_pool_ptr = &MEM_pu_pool;
MEM_POOL *MEM_phase_pool_ptr = &MEM_phase_pool;

/* These pools don't zero newly-allocated memory, and hence allocation
 * is faster than the zeroed pools above.  These should be used whenever
 * appropriate.
 */
MEM_POOL MEM_local_nz_pool;
MEM_POOL MEM_src_nz_pool;
MEM_POOL MEM_pu_nz_pool;
MEM_POOL MEM_phase_nz_pool;

MEM_POOL *MEM_local_nz_pool_ptr = &MEM_local_nz_pool;
MEM_POOL *MEM_src_nz_pool_ptr = &MEM_src_nz_pool;
MEM_POOL *MEM_pu_nz_pool_ptr = &MEM_pu_nz_pool;
MEM_POOL *MEM_phase_nz_pool_ptr = &MEM_phase_nz_pool;


/* -----------------------------------------------------------------
 * Data structures
 * -----------------------------------------------------------------
 */

/* MEM_BLOCK - this is really the header for a memory block allocated
 * by this package.  Space after the final field is used for user
 * allocations.
 */
struct mem_block {
  size_t     avail;         /* Number of bytes still available in the
                             * block
                             */
  MEM_PTR    ptr;           /* Points at the next byte to be allocated.
                             */
  MEM_BLOCK *rest;          /* Points at the rest of the rest of the
                             * blocks in an internally linked list.
                             */
};

#define MEM_BLOCK_avail(x)      ((x)->avail)
#define MEM_BLOCK_ptr(x)        ((x)->ptr)
#define MEM_BLOCK_rest(x)       ((x)->rest)

#define MEM_BLOCK_first_ptr(x)                                          \
    (MEM_PTR) (((char *) (x)) + PAD_TO_ALIGN(sizeof(MEM_BLOCK)))

/* MEM_LARGE_BLOCK - this is really the header for a large memory block
   (blocks larger than BLOCK_SIZE) allocated by this package.  Space
   after the final field (after rounding up for alignment) is for user's
   storage
 */
typedef struct mem_large_block MEM_LARGE_BLOCK;
struct mem_large_block {
  MEM_LARGE_BLOCK *next;		/* doubly-linked list */
  MEM_LARGE_BLOCK *prev;
  MEM_POOL_BLOCKS *base;		/* points back to the head of list */
  MEM_PTR ptr;				/* points to the user memory block */
};

#define MEM_LARGE_BLOCK_next(x)		((x)->next)
#define MEM_LARGE_BLOCK_prev(x)		((x)->prev)
#define MEM_LARGE_BLOCK_base(x)		((x)->base)
#define MEM_LARGE_BLOCK_ptr(x)		((x)->ptr)
#define MEM_LARGE_BLOCK_OVERHEAD	(PAD_TO_ALIGN(sizeof(MEM_LARGE_BLOCK)))

/* When we free a large block we must also erase fields that identify it
 * as a valid large block.  Subsequent to being freed as a large block,
 * the memory may be malloc'ed as a "small" block for some other mempool.
 * If this "small" block is not initialized (and neither is the 
 * MEM_LARGE_BLOCK_OVERHEADT preceeding it), then when we call MEMPOOL_FREE
 * on this "small" block it may be interpreted as a large block and
 * its successor blocks (freed and possibly reallocated) may be added
 * to the large block list of this other mempool.  This can cause all kinds
 * of havoc and is *very* hard to track down.  The MEM_LARGE_BLOCK_zap()
 * is intended to avoid this kind of scenario (a real bug we encountered)!
 */
static void
MEM_LARGE_BLOCK_zap(MEM_LARGE_BLOCK *block)
{
   MEM_LARGE_BLOCK_base(block) = NULL;
   MEM_LARGE_BLOCK_ptr(block) = NULL;
}
   
static void
MEM_LARGE_BLOCK_free(MEM_LARGE_BLOCK *block)
{
   MEM_LARGE_BLOCK_zap(block);
   free(block);
}

static MEM_LARGE_BLOCK * 
MEM_LARGE_BLOCK_realloc(MEM_LARGE_BLOCK *old_block, INT64 new_size)
{
   MEM_POOL_BLOCKS * const base = MEM_LARGE_BLOCK_base(old_block);
   MEM_PTR           const ptr  = MEM_LARGE_BLOCK_ptr(old_block);
   MEM_LARGE_BLOCK *       new_block;

   MEM_LARGE_BLOCK_zap(old_block); /* In case old_block != new_block */
   new_block = (MEM_LARGE_BLOCK *)realloc(old_block, new_size);
   if (new_block != NULL)
   {
      MEM_LARGE_BLOCK_base(new_block) = base;
      MEM_LARGE_BLOCK_ptr(new_block) = ptr;
   }
   return new_block;
}
   

/* INT_LIST - Kind of a drag, a list of integers used to implement a
 * stack of current space allocated in the MEM_STATs.
 */
typedef struct int_list INT_LIST;

struct int_list {
  INT32     first;      /* First element
                         */
  INT_LIST *rest;       /* All the rest
                         */
};

#define INT_LIST_first(x) ((x)->first)
#define INT_LIST_rest(x)  ((x)->rest)

static INT_LIST *free_int_lists;    /* Free list of INT_LISTs.
                                     */

/* MEM_STAT - Per call-site information.  Used to keep track of how
 * the clients of this package use it.
 */

struct mem_stat {
  const char *file;                   /* File called from
                                 */
  INT32 line;                   /* Line called from
                                 */
  INT32 total;                  /* Total memory allocated from
                                 * this site.
                                 */
  INT32 current;                /* Memory currently allocated from
                                 * this site
                                 */
  INT32 max_t;                  /* Maximum memory ever in use and
                                 * allocated from this site (maximum
                                 * value of current.)
                                 */
  size_t max_s;                 /* Maximum memory ever allocated
                                 * from this site in a single call to
                                 * malloc
                                 */
  INT32 last;                   /* Last amount of memory allocated
                                 * from this site
                                 */
  INT32 last_grew;              /* Count of times the amount of
                                 * memory grew for one call to the
                                 * next
                                 */
  INT32 last_shrank;            /* Count of times the amount of
                                 * memory shrank from once call to the
                                 * next
                                 */
  INT32 count;                  /* How many calls from this callsite
                                 */
  MEM_STAT *hash_list_rest;     /* Used to keep the list of
                                 * elements in the hash bucket
                                 */
  MEM_STAT *pool_list_rest;     /* Used to keep the list of
                                 * elements associates with each pool
                                 */
  INT_LIST  *saved_current;     /* Stack of previous values of
                                 * current, pushed when the pool is
                                 * pushed.
                                 */
  MEM_POOL *pool;		/* Pool the allocation is from.
				 */
};

#define MEM_STAT_file(x)              ((x)->file)
#define MEM_STAT_line(x)              ((x)->line)
#define MEM_STAT_total(x)             ((x)->total)
#define MEM_STAT_current(x)           ((x)->current)
#define MEM_STAT_max_t(x)             ((x)->max_t)
#define MEM_STAT_max_s(x)             ((x)->max_s)
#define MEM_STAT_last(x)              ((x)->last)
#define MEM_STAT_last_grew(x)         ((x)->last_grew)
#define MEM_STAT_last_shrank(x)       ((x)->last_shrank)
#define MEM_STAT_count(x)             ((x)->count)
#define MEM_STAT_hash_list_rest(x)    ((x)->hash_list_rest)
#define MEM_STAT_pool_list_rest(x)    ((x)->pool_list_rest)
#define MEM_STAT_saved_current(x)     ((x)->saved_current)
#define MEM_STAT_pool(x)	      ((x)->pool)


/* MEM_POOL_BLOCKS - This represents a particular level of allocation
 * for the pool.  As the pool is Push'ed and Pop'ed (Mark'ed and
 * Free'ed in old parlance) we'll push and pop a list of these,
 * free'ing any used blocks.
 */
struct mem_pool_blocks {
  MEM_BLOCK *block;			/* list of memory blocks */

  MEM_LARGE_BLOCK *large_block;		/* list of large memory blocks */

  MEM_BLOCK *base_block;		/* points to the block when this is
					   pushed */

  MEM_PTR *base_ptr;			/* MEM_BLOCK_ptr when this is pushed */

  size_t base_avail;			/* MEM_BLOCK_avail when this is
					   pushed */

  MEM_POOL_BLOCKS *rest;		/* When active, used to keep
                                         * stack of allocation levels.
                                         * When inactive, used to keep
                                         * free list of these object.
                                         */
};

#define MEM_POOL_BLOCKS_block(x) ((x)->block)
#define MEM_POOL_BLOCKS_large_block(x) ((x)->large_block)
#define MEM_POOL_BLOCKS_base_block(x) ((x)->base_block)
#define MEM_POOL_BLOCKS_base_ptr(x) ((x)->base_ptr)
#define MEM_POOL_BLOCKS_base_avail(x) ((x)->base_avail)
#define MEM_POOL_BLOCKS_rest(x) ((x)->rest)

/***********************************************************************
 * The purify option:
 * ------------------
 * Based on a tt flag you can choose that your mem-pools actually
 * do a malloc/free. Push, Pop, Alloc, Free, Realloc, Freeze, Unfreeze, 
 * continue to work as before for the user, but their implementation 
 * does real malloc/free for each piece of storage. So purify should be
 * a lot more useful.
 *
 * Unless this flag is given, memory pools should behave exactly as
 * before, except that each memory pool will have a pointer (8 bytes)
 * of wasted storage.
 *
 * Implementation:
 * ---------------
 * 1. We need to keep track of all the memory allocated, so that when the
 *    memory pool is popped we can actually do the free.
 * 2. We need to do (1) for each push-level for a memory pool, so that we
 *    only pop the current push level.
 * 
 * We therefore have 
 * (a) a stack of pointers of the memory allocated, and
 * (b) a stack of stacks(a), one for each push-level.
 *
 * The implementation of these stack of stacks is as follows:
 * Each memory pool has an extra pointer that points to a 
 * struct mem_pure_stack (see below). This struct corresponds to the
 * most recent push-level. It contains a pointer to the struct for the
 * previous push-level, and it contains a pointer to the last memory
 * allocated in this push-level.
 *
 * The memory allocated for a particular push-level is strung together
 * by allocating an extra 8 bytes of storage, and using the double-word 
 * before the pointer to store the pointer to the next piece of memory 
 * allocated.
 *
 ***********************************************************************/

/* MEM_PURE_STACKS - This struct stores 
 *      (a) a pointer to the latest allocation in this push-level
 *      (b) a pointer to the corresponding struct from the previous push level.
 */
struct mem_pure_stack {
  MEM_PTR               last_alloc;     /* latest alloc'd storage */
  MEM_PURE_STACK        *prev_stack;    /* pointer to push-stack of 
                                         * previous push level.
                                         */
};

#define MEM_PURE_STACK_last_alloc(x) ((x)->last_alloc)
#define MEM_PURE_STACK_prev_stack(x) ((x)->prev_stack)
#define MEM_POOL_last_alloc(x)                          \
    MEM_PURE_STACK_last_alloc(MEM_POOL_pure_stack(x))
#define MEM_POOL_prev_stack(x)                          \
    MEM_PURE_STACK_prev_stack(MEM_POOL_pure_stack(x))
BOOL purify_pools = FALSE;
static BOOL purify_pools_trace = FALSE;
static BOOL purify_pools_trace_x = FALSE;

#define MAGIC_NUM 0xdecf

/* Accessing POOL fields.  This is defined here, as these fields are
 * all private.
 */
#define MEM_POOL_name(x)            ((x)->name)
#define MEM_POOL_blocks(x)          ((x)->blocks)
#define MEM_POOL_bz(x)		    ((x)->bz)
#define MEM_POOL_rest(x)            ((x)->rest)
#define MEM_POOL_pure_stack(x)      ((x)->pure_stack)
#define MEM_POOL_frozen(x)          ((x)->frozen)
#define MEM_POOL_magic_num(x)       ((x)->magic_num)
#define MEM_POOL_alloc_site_list(x) ((x)->alloc_site_list)

#define MEM_POOL_block(x)                                               \
        MEM_POOL_BLOCKS_block(MEM_POOL_blocks(x))
#define MEM_POOL_large_block(x)						\
	MEM_POOL_BLOCKS_large_block(MEM_POOL_blocks(x))  

/* -----------------------------------------------------------------
 * Local variables
 * -----------------------------------------------------------------
 */
static MEM_POOL_BLOCKS *free_mem_pool_blocks_list;
				    /* Free list of MEM_PPOOL_BLOCKS
                                     */

static MEM_POOL_BLOCKS  overhead_blocks;
                                    /* Used to initialize the overhead
                                     * pool below.
                                     */
static MEM_POOL mem_overhead_pool = /* Used to allocate stuff for this
                                     * package
                                     */
{
    "memory overhead",
    &overhead_blocks,
    NULL,
    NULL,
    TRUE,
    FALSE,
    MAGIC_NUM,
    NULL
};

static MEM_POOL *The_Default_Mem_Pool;

/* PAD_TO_ALIGN - Pad up a given size to double word alignment.
 */
#define PAD_TO_ALIGN(size) (((size) + 7) & (~0U << 3))


/* Implementation of memory statististics tracking mechanism
 */
static BOOL mem_tracing_enabled = FALSE;
#ifdef Is_True_On
static MEM_POOL *initialized_pools =/* List of pools, used for memory
                                     * statistics reporting.
                                     */
    &mem_overhead_pool;
#endif

#define N_BUCKETS 503

static MEM_STAT *call_site_hash_tab[N_BUCKETS];


/* ====================================================================
 *
 *  Hash
 *
 *  Compute a hash key for a alloc callsite.
 *
 *  'line'      - line number in file of callsite
 *  'file'      - filename containing callsite
 *
 * ====================================================================
 */

static UINT32
Hash(
  INT32 line,
  const char *file
)
{
  const char *p;
  UINT32 result = line;

  /* Somebody once told me this was a good idea.  It probably spreads
   * the bits around OK.
   */
  for ( p = file; *p; ++p )
    result = ((result << 1) ^ (result >> 1) ^ *p) + *p;

  return result % N_BUCKETS;
}

/* ====================================================================
 *
 *  Hash_Get
 *
 *  Find the MEM_STAT for a callsite if there is one.  Return NULL,
 *  otherwise.
 *
 *  'hn'    - hash number (as returned by Hash).
 *  'line'  - line number of callsite
 *  'file'  - filename with callsite
 *
 * ====================================================================
 */

static MEM_STAT*
Hash_Get(
  UINT32 hn,
  MEM_POOL *pool,
  INT32  line,
  const char  *file
)
{
  MEM_STAT *as;

  for ( as = call_site_hash_tab[hn];
        as != NULL;
        as = MEM_STAT_hash_list_rest(as)
  ) {
    if (    MEM_STAT_line(as) == line
         && strcmp(MEM_STAT_file(as),file) == 0
	 && MEM_STAT_pool(as) == pool
    ) {
      return as;
    }
  }

  return NULL;
}

/* ====================================================================
 *
 *  Site_Account_Alloc
 *
 *  Record a memory (re)allocation event.
 *
 *  'pool'      - In which MEM_POOL
 *  'old_size'  - If a realloc, memory that was allocated, else 0.
 *  'new_size'  - Amount of memory allocated.
 *  'line'      - Line in file of the callsite.
 *  'file'      - Name of the file containing the callsite.
 *
 * ====================================================================
 */

static void
Site_Account_Alloc(
  MEM_POOL *pool,
  size_t    old_size,
  size_t    new_size
  MEM_STAT_ARGS(line,file)
)
{
  INT32       size = new_size - old_size;
  UINT32      hn   = Hash(line,file);
  MEM_STAT   *ms   = Hash_Get(hn,pool,line,file);

  if ( ms == NULL ) {
    ms = (MEM_STAT *) calloc(1,sizeof(MEM_STAT));

    MEM_STAT_pool(ms) = pool;
    MEM_STAT_line(ms) = line;
    MEM_STAT_file(ms) = file;
    MEM_STAT_hash_list_rest(ms) = call_site_hash_tab[hn];
    call_site_hash_tab[hn] = ms;
    MEM_STAT_pool_list_rest(ms) = MEM_POOL_alloc_site_list(pool);
    MEM_POOL_alloc_site_list(pool) = ms;
  }

  MEM_STAT_current(ms) += size;
  MEM_STAT_total(ms) += size;
  if ( size > MEM_STAT_last(ms) )
    ++MEM_STAT_last_grew(ms);
  else if ( size < MEM_STAT_last(ms) )
    ++MEM_STAT_last_shrank(ms);
  MEM_STAT_max_t(ms) = Max(MEM_STAT_max_t(ms),MEM_STAT_current(ms));
  MEM_STAT_max_s(ms) = Max(MEM_STAT_max_s(ms),size);
  MEM_STAT_last(ms) = size;
  ++MEM_STAT_count(ms);
}

/* ====================================================================
 *
 *  Site_Account_Pop
 *
 *  Record a memory pool poping event (which frees its memory.)
 *
 *  'pool'  - is the pool being poped.
 *  'line'  - is the line number of the callsite
 *  'file'  - is the filename with the callsite
 *
 * ====================================================================
 */

static void
Site_Account_Pop(
  MEM_POOL *pool
  MEM_STAT_ARGS(line,file)
)
{
  MEM_STAT *ms;

  for ( ms = MEM_POOL_alloc_site_list(pool);
        ms != NULL;
        ms = MEM_STAT_pool_list_rest(ms)
  ) {
    INT_LIST *tmp = MEM_STAT_saved_current(ms);

    if ( tmp == NULL ) {
      /* Wasn't on the list at the time of the push, hence current
       * will now be 0
       */
      MEM_STAT_current(ms) = 0;
    }
    else {
      MEM_STAT_current(ms) = INT_LIST_first(tmp);
      MEM_STAT_saved_current(ms) = INT_LIST_rest(tmp);
      INT_LIST_rest(tmp) = free_int_lists;
      free_int_lists = tmp;
    }
  }
}

/* ====================================================================
 *
 *  Site_Account_Push
 *
 *  Record a memory pool pushing event.
 *
 *  'pool'  - is the pool being pushed.
 *
 * ====================================================================
 */

static void
Site_Account_Push(
  MEM_POOL *pool
  MEM_STAT_ARGS(line,file)
)
{
  MEM_STAT *ms;

  for ( ms = MEM_POOL_alloc_site_list(pool);
        ms != NULL;
        ms = MEM_STAT_pool_list_rest(ms)
  ) {
    INT_LIST *il;

    if ( free_int_lists == NULL )
      il = TYPE_MEM_POOL_ALLOC(INT_LIST,&mem_overhead_pool);
    else {
      il = free_int_lists;

      free_int_lists = INT_LIST_rest(il);
    }

    INT_LIST_rest(il) = MEM_STAT_saved_current(ms);
    MEM_STAT_saved_current(ms) = il;
    INT_LIST_first(il) = MEM_STAT_current(ms);
  }
}

/* ====================================================================
 *
 *  Field_Size
 *
 *  How wide a field to print an integer 'i'?
 *
 * ====================================================================
 */

static INT32
Field_Size(
  INT32 i
)
{
  char buff[100];

  /* Could certainly be more winning, but who cares.  This will only
   * be compiled for collecting memory stats.
   */
  sprintf(buff,"%d",i);
  return strlen(buff);
}

/* ====================================================================
 *
 *  MEM_STAT_Sort
 *
 *  Qsort comparison function for ordering MEM_STATs in descending
 *  order of the maximum amount of space they ever had outstanding.
 *
 *  'as1p'  - pointers to MEM_STAT *s
 *  'as2p'
 *
 * ====================================================================
 */

typedef INT (*QSORT_FUNC) (const void *, const void *);

static INT
MEM_STAT_Sort(
    MEM_STAT **as1p,
    MEM_STAT **as2p
)
{
  MEM_STAT *as1 = *as1p;
  MEM_STAT *as2 = *as2p;

  return MEM_STAT_max_t(as2) - MEM_STAT_max_t(as1);
}

/* ====================================================================
 *
 *  MEM_STAT_In_List
 *
 *  Determine if the pool is already in the list.
 *
 * ====================================================================
 */

static BOOL
MEM_STAT_In_List(
    MEM_POOL *list,
    MEM_POOL *pool
)
{
  for ( ;
        list != NULL;
        list = MEM_POOL_rest(list)
  ) {
    if ( list == pool )
      return ( TRUE );
  }

  return ( FALSE );
}

/* ====================================================================
 *
 *  MEM_POOL_Report
 *
 *  Print statistics for the given MEM_POOL.
 *
 *  'pool'          - The pool to report on.
 *  'used_total'    - The total current amount of memory allocated.
 *
 * ====================================================================
 */

INT32
MEM_POOL_Report(
  MEM_POOL   *pool,
  INT32       used_total
)
{
  MEM_STAT  *as;
  MEM_STAT **as_vec;
  INT32 i;
  INT32 total_current = 0;
  INT32 total_allocated = 0;
  INT32 max_allocated = 0;
  INT32 current_fs = 3;
  INT32 total_fs = 3;
  INT32 max_t_fs = 4;
  INT32 max_s_fs = 4;
  INT32 count_fs = 5;
  INT32 last_grew_fs = 4;
  INT32 last_shrank_fs = 6;
  INT32 site_count = 0;

  Is_True (MEM_POOL_magic_num(pool) == MAGIC_NUM,
           ("Report from un-initialized MEM_POOL %s\n", MEM_POOL_name(pool)));

  fprintf(TFile,"----- %s callsites\n",MEM_POOL_name(pool));

  /* Prepass to count records, figure out size of fields.
   */
  for ( as = MEM_POOL_alloc_site_list(pool);
        as != NULL;
        as = MEM_STAT_pool_list_rest(as)
  ) {
    current_fs     = Max(current_fs,Field_Size(MEM_STAT_current(as)));
    total_fs       = Max(total_fs,Field_Size(MEM_STAT_total(as)));
    max_t_fs       = Max(max_t_fs,Field_Size(MEM_STAT_max_t(as)));
    max_s_fs       = Max(max_s_fs,Field_Size(MEM_STAT_max_s(as)));
    count_fs       = Max(count_fs,Field_Size(MEM_STAT_count(as)));
    last_grew_fs   = Max(last_grew_fs,
                         Field_Size(MEM_STAT_last_grew(as)));
    last_shrank_fs = Max(last_shrank_fs,
                         Field_Size(MEM_STAT_last_shrank(as)));

    ++site_count;
  }

  /* Now sort by maximum allocated memory:
   */
  MEM_POOL_Push(&mem_overhead_pool);
  as_vec = TYPE_MEM_POOL_ALLOC_N(MEM_STAT *,&mem_overhead_pool,
                                            site_count);

  for ( as = MEM_POOL_alloc_site_list(pool), i = 0;
        as != NULL;
        as = MEM_STAT_pool_list_rest(as), ++i
  ) {
    as_vec[i] = as;
  }

  qsort((void*)as_vec,site_count,
                      sizeof(MEM_STAT*),
                      (QSORT_FUNC) MEM_STAT_Sort);

  /* Print the column headers.
   */
  fprintf(TFile,"%*s %*s %*s %*s %*s %*s %*s Site\n",
                  max_t_fs,
                  "maxt",
                  current_fs,
                  "cur",
                  total_fs,
                  "tot",
                  max_s_fs,
                  "maxs",
                  count_fs,
                  "count",
                  last_grew_fs,
                  "grew",
                  last_shrank_fs,
                  "shrank");

  /* And the records
   */
  for ( i = 0; i < site_count; ++i ) {
    as = as_vec[i];

    fprintf(TFile,"%*d %*d %*d %*d %*d %*d %*d %s %d\n",
                  max_t_fs,
                  (INT)MEM_STAT_max_t(as),
                  current_fs,
                  (INT)MEM_STAT_current(as),
                  total_fs,
                  (INT)MEM_STAT_total(as),
                  max_s_fs,
                  (INT)MEM_STAT_max_s(as),
                  count_fs,
                  MEM_STAT_count(as),
                  last_grew_fs,
                  MEM_STAT_last_grew(as),
                  last_shrank_fs,
                  MEM_STAT_last_shrank(as),
                  MEM_STAT_file(as),
                  MEM_STAT_line(as));
    total_current += MEM_STAT_current(as);
    total_allocated += MEM_STAT_total(as);
    max_allocated += MEM_STAT_max_t(as);
  }

  MEM_POOL_Pop(&mem_overhead_pool);

  fprintf(TFile,"++++ Allocated for %s pool: total=%d, max=%d, current=%d (%d%%used)\n",
                MEM_POOL_name(pool),
		total_allocated,
		max_allocated,
                total_current,
                (INT)  (100.0 * (  ((double) total_current)
                                 / ((double) used_total))));
  return total_allocated;
}

/* ====================================================================
 *
 *  MEM_Trace
 *
 *  Show detailed info about where the memory is being used.
 *
 * ====================================================================
 */

void
MEM_Trace(void)
{
#ifdef Is_True_On
/* need linux malloc or -lmalloc on irix to use mallinfo */
#if defined(linux) || defined(MEM_STATS)
  MEM_POOL *pool;
  struct    mallinfo mi = mallinfo();
  INT32     used_total = mi.usmblks + mi.uordblks;
  INT32	    total_allocated = 0;

  fprintf(TFile,"arena    %10d\n",mi.arena);
  fprintf(TFile,"ordblks  %10d\n",mi.ordblks);
  fprintf(TFile,"smblks   %10d\n",mi.smblks);
  fprintf(TFile,"hblkhd   %10d\n",mi.hblkhd);
  fprintf(TFile,"hblks    %10d\n",mi.hblks);
  fprintf(TFile,"usmblks  %10d\n",mi.usmblks);
  fprintf(TFile,"fsmblks  %10d\n",mi.fsmblks);
  fprintf(TFile,"uordblks %10d\n",mi.uordblks);
  fprintf(TFile,"fordblks %10d\n",mi.fordblks);
  fprintf(TFile,"keepcost %10d\n",mi.keepcost);

  for ( pool = initialized_pools;
        pool != NULL;
        pool = MEM_POOL_rest(pool)
  ) {
    total_allocated += MEM_POOL_Report(pool,used_total);
  }
  fprintf(TFile,"++++ Total Allocated = %d\n",total_allocated);
#else
  fprintf(TFile,
          "MEM_Trace: Not available; compiler not compiled with MEM_STATS\n");
#endif
#else
  fprintf(TFile,
          "MEM_Trace: Not available; compiler not compiled with Is_True_On\n");
#endif  
}


/* ====================================================================
 *
 *  Trace_Memory_Allocation
 *
 *  Do a memory trace if request via command-line switch.
 *
 * ====================================================================
 */
void
Trace_Memory_Allocation (
  const INT phase,	/* Phase after which we're printing */
  const char *const pname )	/* Print name for phase	*/
{
  if ( Get_Trace ( TKIND_ALLOC, phase ) ) {
    fprintf ( TFile,
              "\n%s%s\tMemory allocation information after %s\n%s%s\n",
	      DBar, DBar, pname, DBar, DBar );
    MEM_Trace ();
  }
}

/* ====================================================================
 *
 *  MEM_Tracing_Enable
 *
 *  Turn on statistics gathering.
 *
 * ====================================================================
 */

void
MEM_Tracing_Enable(void)
{
#ifdef Is_True_On
  mem_tracing_enabled = TRUE;
#endif
}



#if Is_True_On
char *special_address = NULL;
char *special_address_owner = "NOBODY";
#endif

/* ====================================================================
 *
 *  Allocate_Block
 *
 *  Allocate a new block to a pool.
 *
 *  'pool'      Allocate a new block to this pool
 *
 * ====================================================================
 */

static MEM_PTR
Allocate_Block (MEM_POOL *pool)
{
  MEM_BLOCK *block = (MEM_BLOCK *)
    malloc (BLOCK_SIZE + PAD_TO_ALIGN(sizeof(MEM_BLOCK)));

  if (block == NULL)
    ErrMsg (EC_No_Mem, "Allocate_Block");

  if ( MEM_POOL_bz(pool) )
    bzero (block, BLOCK_SIZE + PAD_TO_ALIGN(sizeof(MEM_BLOCK)));
  
#ifdef ZAP_ON_FREE
  else
    memset(((char *) block), 0xa5,
	   BLOCK_SIZE + PAD_TO_ALIGN(sizeof(MEM_BLOCK)));
#endif

  MEM_BLOCK_avail(block) = BLOCK_SIZE;
  MEM_BLOCK_ptr(block) = MEM_BLOCK_first_ptr(block);
  MEM_BLOCK_rest(block) = MEM_POOL_block(pool);
  MEM_POOL_block(pool) = block;

#if Is_True_On
  if (special_address >= ((char *) MEM_BLOCK_ptr(block)) &&
      special_address < (((char *) MEM_BLOCK_ptr(block)) +
			 MEM_BLOCK_avail(block))) {
    fprintf(TFile, "Pool %s given %llu bytes from 0x%p to 0x%p\n",
	    MEM_POOL_name(pool), (UINT64)MEM_BLOCK_avail(block),
	    (char *) MEM_BLOCK_ptr(block),
	    ((char *) MEM_BLOCK_ptr(block)) + MEM_BLOCK_avail(block));
    special_address_owner = MEM_POOL_name(pool);
  }
#endif

#ifdef KEY
#ifndef NO_VALGRIND
  /* Tell Valgrind that this memory is not (yet) valid. */
  VALGRIND_MAKE_NOACCESS(MEM_BLOCK_first_ptr(block), BLOCK_SIZE);
#endif /* NO_VALGRIND */
#endif /* KEY */

  return block;
}

/* ====================================================================
 *
 *  Allocate_Large_Block
 *
 *  Allocate a new large block to a pool.
 *
 *  'pool'      Allocate a new block to this pool
 *  'size'	size of the new block
 *
 * ====================================================================
 */

static MEM_PTR
Allocate_Large_Block (MEM_POOL *pool, INT32 size)
{
  MEM_LARGE_BLOCK *block;
  size += MEM_LARGE_BLOCK_OVERHEAD;
  block = (MEM_LARGE_BLOCK *) malloc (size);

  if (block == NULL)
    ErrMsg (EC_No_Mem, "Allocate_Large_Block");

  if ( MEM_POOL_bz(pool) ) {
    bzero (block, size);
  }
  
#ifdef ZAP_ON_FREE
  else
    memset(((char *) block), 0xa5, size);
#endif

  MEM_LARGE_BLOCK_ptr(block) = (MEM_PTR)
    (((char *)block) + MEM_LARGE_BLOCK_OVERHEAD);
  MEM_LARGE_BLOCK_base(block) = MEM_POOL_blocks(pool);
  MEM_LARGE_BLOCK_next(block) = MEM_POOL_large_block(pool);
  MEM_LARGE_BLOCK_prev(block) = NULL;
  if (MEM_LARGE_BLOCK_next(block) != NULL)
    MEM_LARGE_BLOCK_prev(MEM_LARGE_BLOCK_next(block)) = block;
  MEM_POOL_large_block(pool) = block;

  return MEM_LARGE_BLOCK_ptr(block);
}

/* ====================================================================
 *
 *  Raw_Allocate
 *
 *  Allocate memory from a pool having determined which block size to
 *  use.
 *
 *  pool        - the pool to allocate from
 *  size        - How much to allocate.  Already rouneded up to double
 *                word alignment
 *
 * ====================================================================
 */

static MEM_PTR
Raw_Allocate(
  MEM_POOL *pool,
  INT32     size
)
{
  MEM_PTR *result;

  if (size <= MIN_LARGE_BLOCK_SIZE) {
    MEM_BLOCK *b = MEM_POOL_block(pool);

    if (b == NULL || MEM_BLOCK_avail(b) < size) {
      b = Allocate_Block (pool);
    }

    result = MEM_BLOCK_ptr(b) + REDZONE_SIZE;
    MEM_BLOCK_ptr(b) = (MEM_PTR) (((char*) MEM_BLOCK_ptr(b)) + size + (REDZONE_SIZE*2));
    MEM_BLOCK_avail(b) -= size + (REDZONE_SIZE*2);

#ifdef KEY
#ifndef NO_VALGRIND
    /* Tell Valgrind that we've allocated this piece of memory.  */
    VALGRIND_MEMPOOL_ALLOC(MEM_POOL_blocks(pool), result, size);
#endif /* NO_VALGRIND */
#endif /* KEY */

    return result;
  } else
    return Allocate_Large_Block (pool, size);
}

/* ====================================================================
 *
 *  MEM_POOL_Allocate
 *
 *  Allocate memory out of a pool.
 *
 *  'pool'  - The pool to allocate from.
 *  'size'  - How much to allocate.
 *
 * ====================================================================
 */

MEM_PTR
MEM_POOL_Alloc_P
(
  MEM_POOL *pool,
  size_t    size
  MEM_STAT_ARGS(line,file)
)
{

  if (pool == Default_Mem_Pool) pool = The_Default_Mem_Pool;
  if (pool == Malloc_Mem_Pool) {
      MEM_PTR p = malloc(size);
      if (p == NULL)
	  ErrMsg (EC_No_Mem, "MEM_POOL_Alloc");
      return p;
  }

  Is_True (MEM_POOL_magic_num(pool) == MAGIC_NUM,
           ("Alloc from un-initialized MEM_POOL %s\n", MEM_POOL_name(pool)));

#ifdef JUST_USE_MALLOC
  return calloc(1,size);
#endif

  if (purify_pools) {
    MEM_PTR ret_val;

    /* Check to see that this pool was pushed before this allocation.
     */
    if (!MEM_POOL_blocks(pool)) {
      DevWarn("Allocation from %s before MEM_POOL_Push(%s)",
	      MEM_POOL_name(pool), MEM_POOL_name(pool));
      MEM_POOL_blocks(pool) = (MEM_POOL_BLOCKS *) TRUE;
    }

    ret_val = calloc(1,size+8);
    Is_True (ret_val, ("MEM_POOL_Alloc: calloc returned NULL"));
    Is_True (MEM_POOL_pure_stack(pool), ("MEM_POOL_Alloc %s: missing stack",
                                         MEM_POOL_name(pool)));
    *(MEM_PTR*)ret_val = MEM_POOL_last_alloc(pool);
    MEM_POOL_last_alloc(pool) = ret_val;
    if (purify_pools_trace)
      printf ("pool %s, alloc 0x%p, size %llu, (0x%p - 0x%p)\n",
	      MEM_POOL_name(pool), (char *)ret_val+8, (UINT64)size,
	      (char *)ret_val+8, (char *)ret_val+size);
    return ((MEM_PTR) ((size_t)ret_val+8));
  }

  Is_True(MEM_POOL_blocks(pool) != NULL,
            ("Alloc with uninitialized MEM_POOL"));
#ifdef Is_True_On
  if ( mem_tracing_enabled )
    Site_Account_Alloc(pool,0,size,line,file);
#endif

  /* Round size to double word so any double will be correctly aligned
   */
  size = PAD_TO_ALIGN(size);

  return Raw_Allocate(pool,size);
}


/* ====================================================================
 *
 *  MEM_POOL_Realloc
 *
 *  See interface description
 *
 * ====================================================================
 */

MEM_PTR
MEM_POOL_Realloc_P
(
  MEM_POOL *pool,
  MEM_PTR   old_block,
  size_t    old_size,
  size_t    new_size
  MEM_STAT_ARGS(line,file)
)
{
  MEM_PTR    result;

  if (pool == Default_Mem_Pool) pool = The_Default_Mem_Pool;
  if (pool == Malloc_Mem_Pool) {
      MEM_PTR p = realloc(old_block,new_size);
      if (p == NULL)
	   ErrMsg (EC_No_Mem, "MEM_POOL_Realloc");
      return p;
  }

  Is_True (MEM_POOL_magic_num(pool) == MAGIC_NUM,
           ("Realloc from un-initialized MEM_POOL %s\n", MEM_POOL_name(pool)));

#ifdef JUST_USE_MALLOC
  result = realloc(old_block,new_size);
#ifdef KEY
  if (purify_pools_trace)
    if (result != old_block)
      printf ("pool %s, freed block 0x%p\n", MEM_POOL_name(pool), old_block);
#endif
  if ( old_size < new_size )
    bzero((char*)result + old_size,new_size - old_size);
  return result;
#endif

  if (purify_pools) {
    MEM_PTR ret_val = NULL;
    BOOL foundit = FALSE;

    if (!MEM_POOL_blocks(pool)) {
      DevWarn("Realloc from %s before MEM_POOL_Push(%s)",
	      MEM_POOL_name(pool), MEM_POOL_name(pool));
      MEM_POOL_blocks(pool) = (MEM_POOL_BLOCKS *) TRUE;
    }

    /* Find the thing that points to this location, but if orig non-null */
    if (old_block) {
      /* find the previous entry and remove it */
      MEM_PURE_STACK* tmp_stack = MEM_POOL_pure_stack(pool);
      MEM_PTR cur = (MEM_PTR) ((size_t) old_block - 8);
      Is_True (tmp_stack, ("MEM_POOL_Realloc %s: missing stack",
                           MEM_POOL_name(pool)));
      while (tmp_stack) {
        MEM_PTR tmp = MEM_PURE_STACK_last_alloc(tmp_stack);
        MEM_PTR prev = NULL;

        while (tmp && (tmp != cur)) {
          prev = tmp;
          tmp = *((MEM_PTR*) tmp);
        }
        if (tmp) {
          foundit = TRUE;
          /* splice it out */
          if (prev) *(MEM_PTR*) prev = *(MEM_PTR*) tmp;
          else MEM_PURE_STACK_last_alloc(tmp_stack) = *(MEM_PTR*) tmp;
          break;
        }
        tmp_stack = MEM_PURE_STACK_prev_stack(tmp_stack);
      }
    }
    if (old_block && !foundit) {
      DevWarn ("Realloc without a previous alloc, pool %s, 0x%p",
               MEM_POOL_name(pool), old_block);
      /* Do a malloc and copy */
      ret_val = (MEM_PTR) malloc (new_size+8);
      bcopy(old_block, (MEM_PTR) (((size_t) ret_val)+8), old_size);
    }
    else {
      /* either found it or old_block was null, so do a real realloc */
	ret_val = (MEM_PTR)
	    realloc((MEM_PTR) (old_block ? (size_t) old_block-8 : 0),
			       new_size+8);
#ifdef KEY
	if (purify_pools_trace)
	  if (ret_val != old_block && old_block != 0)
	    printf ("pool %s, freed block 0x%p\n", MEM_POOL_name(pool), old_block);
#endif
    }
    if (new_size > 0) {
      FmtAssert (ret_val, ("oops - realloc returned NULL, pool %s\n",
                           MEM_POOL_name(pool)));
      *(MEM_PTR*) ret_val = MEM_POOL_last_alloc(pool);
      MEM_POOL_last_alloc(pool) = ret_val;
      ret_val = (MEM_PTR) ((size_t) ret_val + 8);
      if ( old_size < new_size )
        bzero((char*)ret_val + old_size,new_size - old_size);
    }
    if (purify_pools_trace)
      printf ("pool %s, realloc 0x%p, new size %llu, (0x%p - 0x%p)\n",
	      MEM_POOL_name(pool), ret_val, (UINT64)new_size,
	      ret_val, (char *)ret_val + new_size - 8);
    return ret_val;
  }

  Is_True(MEM_POOL_blocks(pool) != NULL,
            ("Alloc with uninitialized MEM_POOL"));
#ifdef Is_True_On
  if ( mem_tracing_enabled )
    Site_Account_Alloc(pool,old_size,new_size,line,file);
#endif

  /* Round size to double word so any double will be correctly aligned
   */
  old_size = PAD_TO_ALIGN(old_size);
  new_size = PAD_TO_ALIGN(new_size);

  /* It might already be just the right size.
   */
  if ( new_size == old_size )
    return old_block;

#if 0
#ifdef Is_True_On
  if (new_size < old_size)
    DevWarn ("MEMORY: shrinking an object in (%s) from %d to %d bytes",
	     MEM_POOL_name(pool), old_size, new_size);
  else if (new_size < old_size * 1.5 && old_size > 256)
    DevWarn ("MEMORY: small grow from %d to %d bytes (mempool: %s)",
	     old_size, new_size, MEM_POOL_name(pool));
#endif
#endif

  if (old_size <= MIN_LARGE_BLOCK_SIZE) {
    if (new_size < old_size)
      return old_block;
    else {
      result = Raw_Allocate (pool, new_size);
      bcopy (old_block, result, old_size);
      return result;
    }
  } else {
    MEM_LARGE_BLOCK *large_block = (MEM_LARGE_BLOCK *)
      (((char *) old_block) - MEM_LARGE_BLOCK_OVERHEAD);
    if (MEM_LARGE_BLOCK_ptr(large_block) == (MEM_PTR) old_block &&
	MEM_LARGE_BLOCK_base(large_block) == MEM_POOL_blocks(pool)) {
      /* this is a valid large block that we can reallocate */
      if (new_size <= MIN_LARGE_BLOCK_SIZE) {
	result = Raw_Allocate (pool, new_size);
	bcopy (old_block, result, new_size);
	MEM_POOL_FREE (pool, old_block);
	return result;
      } else {
	MEM_LARGE_BLOCK *p = 
	   (MEM_LARGE_BLOCK *)(((char *)old_block) - MEM_LARGE_BLOCK_OVERHEAD);

	large_block = 
	   MEM_LARGE_BLOCK_realloc(p, new_size + MEM_LARGE_BLOCK_OVERHEAD);
#ifdef KEY
	if (purify_pools_trace)
	  if (p != large_block && p != 0)
	    printf ("pool %s, freed block 0x%p\n", MEM_POOL_name(pool), p);
#endif

	if (large_block == NULL)
	  ErrMsg (EC_No_Mem, "MEM_POOL_Realloc");
	MEM_LARGE_BLOCK_ptr(large_block) = (MEM_PTR)
	  (((char *)large_block) + MEM_LARGE_BLOCK_OVERHEAD);
	if (MEM_POOL_bz(pool)) {
	  bzero (((char *) MEM_LARGE_BLOCK_ptr(large_block)) + old_size,
		 new_size - old_size);
	}
	p = MEM_LARGE_BLOCK_prev(large_block);
	if (p == NULL)
	  MEM_POOL_large_block(pool) = large_block;
	else
	  MEM_LARGE_BLOCK_next(p) = large_block;
	p = MEM_LARGE_BLOCK_next(large_block);
	if (p)
	  MEM_LARGE_BLOCK_prev(p) = large_block;
	return MEM_LARGE_BLOCK_ptr(large_block);
      }
    } else {
      result = Raw_Allocate (pool, new_size);
      if (new_size > old_size)
	bcopy (old_block, result, old_size);
      else
	bcopy (old_block, result, new_size);
      return result;
    }
  }
}

/* ====================================================================
 *
 *  MEM_POOL_Push
 *
 *  Push a MEM_POOL to a new allocation level.  Memory can be
 *  No memory can be allocated in a pool until it's pushed the first
 *  time.
 *
 *  'pool'  - memory pool to push
 *
 * ====================================================================
 */

void
MEM_POOL_Push_P
(
  MEM_POOL *pool
  MEM_STAT_ARGS(line,file)
)
{
  MEM_POOL_BLOCKS *pb;

  Is_True (MEM_POOL_magic_num(pool) == MAGIC_NUM,
           ("Push before Initialize in MEM_POOL %s\n", MEM_POOL_name(pool)));

  FmtAssert(MEM_POOL_frozen(pool) == FALSE, ("Pushing a frozen pool - %s.",
                                             MEM_POOL_name(pool)));
  if (pool == Default_Mem_Pool) pool = The_Default_Mem_Pool;
  if (pool == Malloc_Mem_Pool) return;
#ifdef JUST_USE_MALLOC
    return;
#endif

  if (purify_pools) {
    MEM_PURE_STACK* tmp = (MEM_PURE_STACK*) malloc(sizeof(MEM_PURE_STACK));
    Is_True (tmp, ("MEM_POOL_Push %s: malloc stack returned NULL",
                   MEM_POOL_name(pool)));
    MEM_PURE_STACK_last_alloc(tmp) = NULL;
    MEM_PURE_STACK_prev_stack(tmp) = MEM_POOL_pure_stack(pool);
    MEM_POOL_pure_stack(pool) = tmp;
    if (purify_pools_trace_x) {
      if (MEM_POOL_blocks(pool) == (MEM_POOL_BLOCKS *) MEM_POOL_INIT_IN_PROGRESS) {
	(void) printf("MEM_POOL_Push %s 0x%p<-- free push (called by M_P_Initialize)\n",
		      MEM_POOL_name(pool), pool);
      }
      else {
	(void) printf ("MEM_POOL_Push %s 0x%p\n", MEM_POOL_name(pool), pool);
      }
    }

    /* Co-opt the blocks field to note whether we have kosher
     * push-before-alloc behavior.
     */
    MEM_POOL_blocks(pool) = (MEM_POOL_BLOCKS *) TRUE;
    return;
  }

#ifdef Is_True_On
  if ( mem_tracing_enabled )
    Site_Account_Push(pool,line,file);
#endif

  if ( free_mem_pool_blocks_list != NULL ) {
    /* Can take from free list.
     */
    pb = free_mem_pool_blocks_list;
    free_mem_pool_blocks_list = MEM_POOL_BLOCKS_rest(pb);
#ifdef KEY
#ifndef NO_VALGRIND
    VALGRIND_MAKE_READABLE(pb, sizeof(MEM_POOL_BLOCKS));
    VALGRIND_MAKE_WRITABLE(pb, sizeof(MEM_POOL_BLOCKS));
#endif /* NO_VALGRIND */
#endif /* KEY */
  } else {
     /* Need to allocate a new one.
      */
    pb = TYPE_MEM_POOL_ALLOC(MEM_POOL_BLOCKS,&mem_overhead_pool);
  }

  MEM_POOL_BLOCKS_rest(pb) = MEM_POOL_blocks(pool);
  MEM_POOL_BLOCKS_large_block(pb) = NULL;
  if (MEM_POOL_BLOCKS_rest(pb) == NULL) {
    MEM_POOL_BLOCKS_block(pb) = NULL;
    MEM_POOL_BLOCKS_base_block(pb) = NULL;
    MEM_POOL_BLOCKS_base_ptr(pb) = NULL;
    MEM_POOL_BLOCKS_base_avail(pb) = 0;
  } else {
    MEM_POOL_BLOCKS *p = MEM_POOL_BLOCKS_rest(pb);
    MEM_POOL_BLOCKS_block(pb) = MEM_POOL_BLOCKS_block(p);
    MEM_POOL_BLOCKS_base_block(pb) = MEM_POOL_BLOCKS_block(p);
    if (MEM_POOL_BLOCKS_block(p)) {
      MEM_POOL_BLOCKS_base_ptr(pb) = MEM_BLOCK_ptr(MEM_POOL_BLOCKS_block(p));
      MEM_POOL_BLOCKS_base_avail(pb) =
	MEM_BLOCK_avail(MEM_POOL_BLOCKS_block(p));
    } else {
      MEM_POOL_BLOCKS_base_ptr(pb) = NULL;
      MEM_POOL_BLOCKS_base_avail(pb) = 0;
    }
  }
  MEM_POOL_blocks(pool) = pb;

#ifdef KEY
#ifndef NO_VALGRIND
  VALGRIND_CREATE_MEMPOOL(pb, REDZONE_SIZE, MEM_POOL_bz(pool));
#endif /* NO_VALGRIND */
#endif /* KEY */
}

/* ====================================================================
 *
 * MEM_POOL_Push_Freeze
 *
 * Check that the pool is not already frozen. If not, push a pool
 * to a new allocation level and then freeze it from subsequent push/pop
 * operations until the pool is explicitly unfrozen.
 *
 * 'pool' - is the pool to push-and-freeze
 *
 * ====================================================================
 */

void
MEM_POOL_Push_Freeze_P
(
  MEM_POOL *pool
  MEM_STAT_ARGS(line,file)
)
{
  Is_True (MEM_POOL_magic_num(pool) == MAGIC_NUM,
           ("Push_Freeze before Initialize in MEM_POOL %s\n", MEM_POOL_name(pool)));
  FmtAssert (MEM_POOL_frozen(pool) == FALSE,
             ("Cannot Push_Freeze a frozen pool - %s.", MEM_POOL_name(pool)));
  if (purify_pools_trace_x)
    printf ("MEM_POOL_Push_Freeze %s -- \n", MEM_POOL_name(pool));
  MEM_POOL_Push_P (pool, line, file);
  MEM_POOL_frozen(pool) = TRUE;
}


/* ====================================================================
 *
 *  MEM_POOL_Pop
 *
 *  Pop the state of the given MEM_POOL, freeing any memory allocated
 *  since the matching call to MEM_POOL_Push.
 *
 *  'pool'  - is the pool to pop.
 *
 * ====================================================================
 */

void
MEM_POOL_Pop_P
(
  MEM_POOL *pool
  MEM_STAT_ARGS(line,file)
)
{
  MEM_BLOCK *bp, *next_bp;
  MEM_LARGE_BLOCK *lbp, *next_lbp;
  MEM_POOL_BLOCKS *bsp;

  Is_True (MEM_POOL_magic_num(pool) == MAGIC_NUM,
           ("Pop before Initialize in MEM_POOL %s\n", MEM_POOL_name(pool)));

  FmtAssert(MEM_POOL_frozen(pool) == FALSE, ("Popping a frozen pool - %s.",
                                             MEM_POOL_name(pool)));
  if (pool == Default_Mem_Pool) pool = The_Default_Mem_Pool;
  if (pool == Malloc_Mem_Pool) return;

#ifdef JUST_USE_MALLOC
    return;
#endif
  if (purify_pools) {
    MEM_PURE_STACK *tmp_stack;
    MEM_PTR tmp = NULL; 
    MEM_PTR next = NULL;
    Is_True (MEM_POOL_pure_stack(pool),
	     ("Pop, but no push stack on %s", MEM_POOL_name(pool)));
    if (purify_pools_trace_x)
      printf ("MEM_POOL_Pop %s 0x%p\n", MEM_POOL_name(pool), pool);
    tmp = MEM_POOL_last_alloc(pool);
    while (tmp) {
      next = (*(MEM_PTR*) tmp);
      if (purify_pools_trace) printf ("pool %s, pop-free 0x%p\n",
                                      MEM_POOL_name(pool), (char *)tmp+8);
      free (tmp);
      tmp = next;
    }
    tmp_stack = MEM_POOL_prev_stack(pool);
    free (MEM_POOL_pure_stack(pool));
    MEM_POOL_pure_stack(pool) = tmp_stack;
    return;
  }

  bsp = MEM_POOL_blocks(pool);

  FmtAssert(MEM_POOL_blocks(pool),("Freeing an uninitialized pool."));

#ifdef Is_True_On
  if ( mem_tracing_enabled )
    Site_Account_Pop(pool,line,file);
#endif

  for (bp = MEM_POOL_BLOCKS_block(bsp); bp; bp = next_bp) {
    next_bp = MEM_BLOCK_rest(bp);

#if Is_True_On
    if (special_address >= (char *) MEM_BLOCK_first_ptr(bp) &&
	special_address < ((char *) MEM_BLOCK_ptr(bp) +
			   MEM_BLOCK_avail(bp))) {
      fprintf(TFile, "Pool %s freed %llu bytes from 0x%p to 0x%p\n",
	      MEM_POOL_name(pool),
	      (UINT64)(MEM_BLOCK_avail(bp) +
	       (char *) MEM_BLOCK_ptr(bp) -
	       (char *) MEM_BLOCK_first_ptr(bp)),
	      (char *) MEM_BLOCK_first_ptr(bp),
	      (char *) MEM_BLOCK_ptr(bp) + MEM_BLOCK_avail(bp));
      special_address_owner = "NOBODY";
    }
#endif

    if (bp == MEM_POOL_BLOCKS_base_block(bsp)) {
      MEM_BLOCK_ptr(bp) = MEM_POOL_BLOCKS_base_ptr(bsp);
      MEM_BLOCK_avail(bp) = MEM_POOL_BLOCKS_base_avail(bsp);
      if (MEM_POOL_bz(pool)) {
#ifdef KEY
#ifndef NO_VALGRIND
	VALGRIND_MAKE_WRITABLE(MEM_BLOCK_ptr(bp), MEM_BLOCK_avail(bp));
#endif /* NO_VALGRIND */
#endif /* KEY */
	bzero (MEM_BLOCK_ptr(bp), MEM_BLOCK_avail(bp));
#ifdef KEY
#ifndef NO_VALGRIND
	VALGRIND_MAKE_NOACCESS(MEM_BLOCK_ptr(bp), MEM_BLOCK_avail(bp));
#endif /* NO_VALGRIND */
#endif /* KEY */
      }
      break;
    }
    free (bp);
  }

  for (lbp = MEM_POOL_BLOCKS_large_block(bsp); lbp; lbp = next_lbp) {
    next_lbp = MEM_LARGE_BLOCK_next(lbp);
    MEM_LARGE_BLOCK_free(lbp);
  }

  /* Avoid poping the final blocks from a pool.  This allow a usage
   * like:
   *    MEM_POOL_Alloc
   *    ...
   *    MEM_POOL_Pop
   * without the initial push.
   */
  if ( MEM_POOL_BLOCKS_rest(bsp) != NULL ) {
    MEM_POOL_blocks(pool) = MEM_POOL_BLOCKS_rest(bsp);
    MEM_POOL_BLOCKS_rest(bsp) = free_mem_pool_blocks_list;
    free_mem_pool_blocks_list = bsp;
#ifdef KEY
#ifndef NO_VALGRIND
    /* Tell Valgrind everything in here has been freed.  */
    VALGRIND_DESTROY_MEMPOOL(bsp);
#endif /* NO_VALGRIND */
#endif /* KEY */
  } else {
#ifdef KEY
#ifndef NO_VALGRIND
    VALGRIND_MAKE_WRITABLE(bsp, sizeof(MEM_POOL_BLOCKS));
#endif /* NO_VALGRIND */
#endif /* KEY */
    bzero (bsp, sizeof(MEM_POOL_BLOCKS));
#ifdef KEY
#ifndef NO_VALGRIND
    VALGRIND_MAKE_NOACCESS(bsp, sizeof(MEM_POOL_BLOCKS));
    /* Tell Valgrind everything in here has been freed, but then put it
     * back as empty.  */
    VALGRIND_DESTROY_MEMPOOL(bsp);
    VALGRIND_CREATE_MEMPOOL(bsp, REDZONE_SIZE, MEM_POOL_bz(pool));
#endif /* NO_VALGRIND */
#endif /* KEY */
  }
}


/* ====================================================================
 *
 *  MEM_POOL_Pop_Unfreeze
 *
 *  Check that the pool is frozen.
 *  Then unfreeze the memory pool, and pop.
 *
 *  'pool'  - is the pool to pop.
 *
 * ====================================================================
 */

void
MEM_POOL_Pop_Unfreeze_P
(
  MEM_POOL *pool
  MEM_STAT_ARGS(line,file)
)
{
  Is_True (MEM_POOL_magic_num(pool) == MAGIC_NUM,
           ("Pop_Unfreeze before Initialize in MEM_POOL %s\n", MEM_POOL_name(pool)));
  FmtAssert (MEM_POOL_frozen(pool) == TRUE,
             ("Cannot Pop_Unfreeze a non-frozen pool - %s.",
              MEM_POOL_name(pool)));
  MEM_POOL_frozen(pool) = FALSE;
  if (purify_pools_trace_x)
    printf ("MEM_POOL_Pop_Unfreeze %s -- \n", MEM_POOL_name(pool));
  MEM_POOL_Pop_P (pool, line, file);
}

void MEM_POOL_Set_Default(MEM_POOL *pool)
{
  The_Default_Mem_Pool = pool;
}

void MEM_POOL_FREE(MEM_POOL *pool, void *data)
{
  MEM_LARGE_BLOCK *large_block;

  if (data == NULL)
    return;
  
  if (pool == Default_Mem_Pool) pool = The_Default_Mem_Pool;
  if (pool == Malloc_Mem_Pool) {
    free(data);
    return;
  }

  Is_True (MEM_POOL_magic_num(pool) == MAGIC_NUM,
           ("Free into un-initialized MEM_POOL %s\n", MEM_POOL_name(pool)));

#ifdef JUST_USE_MALLOC
  return;
#endif
  if (data && purify_pools) {
    BOOL foundit = FALSE;
    /* find the entry and remove it */
    MEM_PURE_STACK* tmp_stack = MEM_POOL_pure_stack(pool);
    MEM_PTR cur = (MEM_PTR) ((size_t) data - 8);
    Is_True (tmp_stack, ("MEM_POOL_Realloc %s: missing stack",
                         MEM_POOL_name(pool)));
    while (tmp_stack) {
      MEM_PTR tmp = MEM_PURE_STACK_last_alloc(tmp_stack);
      MEM_PTR prev = NULL;

      while (tmp && (tmp != cur)) {
        prev = tmp;
        tmp = *((MEM_PTR*) tmp);
      }
      if (tmp) {
        foundit = TRUE;
        /* splice it out */
        if (prev) *(MEM_PTR*) prev = *(MEM_PTR*) tmp;
        else MEM_PURE_STACK_last_alloc(tmp_stack) = *(MEM_PTR*) tmp;
        break;
      }
      tmp_stack = MEM_PURE_STACK_prev_stack(tmp_stack);
    }
    if (purify_pools_trace)
      printf ("pool %s, free 0x%p\n", MEM_POOL_name(pool), data);
    if (!foundit) {
      /* free anyway, so that it shows up as a purify error */
      free (cur);
      FmtAssert(FALSE,("MEM_POOL_FREE: pool %s, could not find pointer 0x%p\n",
		       MEM_POOL_name(pool), data));
    }
    free (cur);
    return;
  }

  large_block = (MEM_LARGE_BLOCK *)
    (((char *) data) - MEM_LARGE_BLOCK_OVERHEAD);
  if (MEM_LARGE_BLOCK_ptr(large_block) == (MEM_PTR) data) {
    MEM_LARGE_BLOCK *prev;
    MEM_LARGE_BLOCK *next;
    if (MEM_LARGE_BLOCK_base(large_block) != MEM_POOL_blocks(pool))
      return;

    prev = MEM_LARGE_BLOCK_prev(large_block);
    next = MEM_LARGE_BLOCK_next(large_block);
    if (prev == NULL)
      MEM_POOL_large_block(pool) = next;
    else
      MEM_LARGE_BLOCK_next(prev) = next;

    if (next)
      MEM_LARGE_BLOCK_prev(next) = prev;

    MEM_LARGE_BLOCK_free(large_block);
  }
    
}

#ifdef Is_True_On
static void
trace_initialized_pool (char *msg, char *pname)
{
    MEM_POOL **listp = &initialized_pools;
    printf("<%s %s> initialized_pools: ", msg, pname);
    while (*listp != NULL) {
	printf(", %s", MEM_POOL_name(*listp));
	listp = &MEM_POOL_rest(*listp);
    }
    printf("\n");
}
#endif

void MEM_POOL_Delete(MEM_POOL *pool)
{
  MEM_POOL_BLOCKS *bsp;

  if (pool == Default_Mem_Pool) pool = The_Default_Mem_Pool;
  if (pool == Malloc_Mem_Pool) return;

#ifdef Is_True_On
  /*
   * Remove from initialized_pools list so we don't get bogus stats.
   * To enable stats for deleted pools, need to keep a stats data
   * structure separate from the pools themselves.  I think we want to
   * accumulate stats for pools of the same name that are created
   * within a given run.
   */
  if ( mem_tracing_enabled ) {
    MEM_POOL **listp = &initialized_pools;
    /* trace_initialized_pool ("delete", MEM_POOL_name(pool)); */
    if (*listp == pool) {
	initialized_pools = MEM_POOL_rest(pool);
    }
    else {
	while (*listp && MEM_POOL_rest(*listp)) {
		if (MEM_POOL_rest(*listp) == pool) break;
		listp = &MEM_POOL_rest(*listp);
	}
	if (*listp && MEM_POOL_rest(*listp)) {
		// found it
		MEM_POOL_rest(*listp) = MEM_POOL_rest(pool);
	}
    	else {
		DevWarn("didn't find pool %s in initialized_pools", MEM_POOL_name(pool));
    	}
    }
    Is_True(!MEM_STAT_In_List( initialized_pools, pool ),
	    ("Pool still in initialized list"));
  }
#endif

  Is_True (MEM_POOL_magic_num(pool) == MAGIC_NUM,
           ("Deleting a pool that has not been initialized: %s\n",
            MEM_POOL_name(pool)));

  if (purify_pools) {
    /* let's be nice and deallocate all storage */
#ifndef TODO_REMOVE_FREE_PUSH
    if (!MEM_POOL_pure_stack(pool)) {
      DevWarn("During MEM_POOL_Delete: Too many pops on %s.",
	      MEM_POOL_name(pool));
    }
    else {
      MEM_POOL_Pop(pool);
#endif
      if (MEM_POOL_pure_stack(pool)) {
	DevWarn("During MEM_POOL_Delete: Too few pops on %s.",
		MEM_POOL_name(pool));
	while (MEM_POOL_pure_stack(pool))
	  MEM_POOL_Pop(pool);
      }
#ifndef TODO_REMOVE_FREE_PUSH
    }
#endif
    if (purify_pools_trace_x) 
      printf ("MEM_POOL_Delete %s 0x%p\n", MEM_POOL_name(pool), pool);
    MEM_POOL_magic_num(pool) = 0;
    return;
  }

  /* Pop everything to make old blocks available for re-use.
   * Must treat last block specially since MEM_POOL_Pop doesn't
   * make last block available for reuse by other pools.
   */
  while (MEM_POOL_BLOCKS_rest(MEM_POOL_blocks(pool)) != NULL)
    MEM_POOL_Pop(pool);
  MEM_POOL_Pop(pool);
  bsp = MEM_POOL_blocks(pool);
  MEM_POOL_BLOCKS_rest(bsp) = free_mem_pool_blocks_list;
  free_mem_pool_blocks_list = bsp;

  bzero (pool, sizeof(MEM_POOL));
  MEM_POOL_magic_num(pool) = 0;
}


/* ====================================================================
 *
 *  MEM_POOL_Initialize
 *
 *  Initialize a new MEM_POOL.  Must be done before it can be pushed.
 *
 *  'pool'  - is the pool to initialize
 *  'name'  - a name to associate with the pool for debugging purposes
 *            (NOT copied.)
 *  'bzero' - if true, memory allocate from the pool will always be
 *            zeroed, otherwise no guarantee.
 *
 * ====================================================================
 */

void
MEM_POOL_Initialize_P
(
  MEM_POOL     *pool,
  char         *name,
  BOOL          bzero
  MEM_STAT_ARGS(line,file)
)
{
#ifdef KEY
#ifndef NO_VALGRIND
  static BOOL mem_overhead_pool_initialized = FALSE;
  if(RUNNING_ON_VALGRIND && (mem_overhead_pool_initialized == FALSE)) {
    mem_overhead_pool_initialized = TRUE;
    redzone_size = 8;
    VALGRIND_CREATE_MEMPOOL(MEM_POOL_blocks(&mem_overhead_pool), 
                            REDZONE_SIZE, MEM_POOL_bz(&mem_overhead_pool));
  }
#endif /* NO_VALGRIND */
#endif /* KEY */

  if (pool == Default_Mem_Pool) pool = The_Default_Mem_Pool;
  if (pool == Malloc_Mem_Pool) return;
  MEM_POOL_name(pool) = name;
  MEM_POOL_bz(pool) = bzero;
  MEM_POOL_blocks(pool) = NULL;
  MEM_POOL_frozen(pool) = FALSE;
  MEM_POOL_pure_stack(pool) = NULL;
  
  /* Don't allow duplicate initializations */
  Is_True (MEM_POOL_magic_num(pool) != MAGIC_NUM,
           ("Initialization of an already initialized pool: %s\n",
            MEM_POOL_name(pool)));
  MEM_POOL_magic_num(pool) = MAGIC_NUM;

  if (purify_pools_trace_x)
    printf ("MEM_POOL_Initialize %s 0x%p\n", MEM_POOL_name(pool), pool);
  
#ifdef Is_True_On
  MEM_POOL_alloc_site_list(pool) = NULL;
  if ( mem_tracing_enabled ) {
    if ( ! MEM_STAT_In_List( initialized_pools, pool ) ) {
        /* trace_initialized_pool ("initialize", MEM_POOL_name(pool)); */
	MEM_POOL_rest(pool) = initialized_pools;
	initialized_pools = pool;
    }
  }
#endif

  if (purify_pools) {
    MEM_POOL_blocks(pool) = (MEM_POOL_BLOCKS *) MEM_POOL_INIT_IN_PROGRESS;
  }

#ifndef TODO_REMOVE_FREE_PUSH
  MEM_POOL_Push(pool);
#endif

  if (purify_pools) {
    /* Mark the pool as unpushed, even though we still do a push for
     * the moment.
     */
    MEM_POOL_blocks(pool) = NULL;
  }
}

/* ====================================================================
 *
 *  MEM_Initialize
 *
 *  Initialize the memory package.  Actually, just initialize the
 *  backward compatible pools.
 *
 * ====================================================================
 */

void
MEM_Initialize(void)
{
  char* ppools = getenv ("PURIFY_MEMPOOLS");
  if (ppools) {
    if (((ppools[0] == 'O') || (ppools[0] == 'o')) &&
        ((ppools[1] == 'N') || (ppools[1] == 'n'))) {
      purify_pools = TRUE;
      if ((ppools[2] == '-') &&
          ((ppools[3] == 'T') || (ppools[3] == 't')) &&
          ((ppools[4] == 'R') || (ppools[4] == 'r')) &&
          ((ppools[5] == 'A') || (ppools[5] == 'a')) &&
          ((ppools[6] == 'C') || (ppools[6] == 'c')) &&
          ((ppools[7] == 'E') || (ppools[7] == 'e'))) {
        purify_pools_trace = TRUE;
        if ((ppools[8] == '-') &&
            ((ppools[9] == 'X') || (ppools[9] == 'x'))) {
          purify_pools_trace_x = TRUE;
	  if ((ppools[10] == '-') && 
	    ((ppools[11] == 'O') || (ppools[11] == 'o')) &&
	    ((ppools[12] == 'N') || (ppools[12] == 'n')) &&
	    ((ppools[13] == 'L') || (ppools[13] == 'l')) &&
	    ((ppools[14] == 'Y') || (ppools[14] == 'y'))) {
	    purify_pools_trace = FALSE; 
            DevWarn("Using purify memory pools, limited extended tracing ###");
	  } else 
            DevWarn ("Using purify memory pools, with extended tracing ###");
        }
        else 
          DevWarn ("Using purify memory pools, with tracing ###");
      }
      else DevWarn ("Using purify memory pools ###");
    }
    else if (((ppools[0] == 'O') || (ppools[0] == 'o')) &&
             ((ppools[1] == 'F') || (ppools[1] == 'f')) &&
             ((ppools[2] == 'F') || (ppools[2] == 'f'))) {
      purify_pools = FALSE;
    }
    else DevWarn ("PURIFY_MEMPOOLS set to garbage, using regular pools");
  }

  MEM_POOL_Initialize(&MEM_local_pool,"Local",TRUE);
  MEM_POOL_Initialize(&MEM_src_pool,"Source",TRUE);
  MEM_POOL_Initialize(&MEM_pu_pool,"Program unit",TRUE);
  MEM_POOL_Initialize(&MEM_phase_pool,"Phase",TRUE);

  MEM_POOL_Push(&MEM_local_pool);
  MEM_POOL_Push(&MEM_src_pool);
  MEM_POOL_Push(&MEM_pu_pool);
  MEM_POOL_Push(&MEM_phase_pool);

  MEM_POOL_Initialize(&MEM_local_nz_pool,"Local (nz)",FALSE);
  MEM_POOL_Initialize(&MEM_src_nz_pool,"Source (nz)",FALSE);
  MEM_POOL_Initialize(&MEM_pu_nz_pool,"Program unit (nz)",FALSE);
  MEM_POOL_Initialize(&MEM_phase_nz_pool,"Phase (nz)",FALSE);

  MEM_POOL_Push(&MEM_local_nz_pool);
  MEM_POOL_Push(&MEM_src_nz_pool);
  MEM_POOL_Push(&MEM_pu_nz_pool);
  MEM_POOL_Push(&MEM_phase_nz_pool);

}

/* Two non pool related things kept here for historical reasons.
 */

/* ====================================================================
 *
 * MEM_PTR Realloc_Clear ( ptr, new_size, old_size )
 *
 * This routine is used to grow space that was initially allocated via
 * a call to the Unix routine 'malloc()'.
 *
 * This routine takes as input a pointer that was previously allocated
 * by, for example, 'malloc()', the new size (in bytes) to be allocated,
 * and old size (in bytes) that were allocated by the previous call to
 * 'malloc()'.  It calls 'realloc()' to grow the space pointed to by
 * the input 'ptr', then clears the new space.  Note that we need to
 * know the old size so we can determine which portion of the returned
 * space must be cleared (the contents of the original space must be
 * unchanged).
 *
 * Note that we fatal-error-out here if we cannot allocate enough
 * space.  That is *probably* what is desired, but I can imagine a
 * situation where our caller can do a graceful recovery if there is not
 * enough memory.  If that ever becomes needed, we should just return
 * NULL here to allow our caller that flexibility.  On the other hand,
 * by fatal-erroring-out here, we relieve our caller of the burden of
 * checking if memory was successfully allocated -- thus making our
 * caller less cluttered / easier to understand.
 *
 * ====================================================================
 */

MEM_PTR
Realloc_Clear ( MEM_PTR ptr, INT32 new_size, INT32 old_size )
{
  MEM_PTR result = (MEM_PTR) realloc ( ptr, new_size );
#ifdef KEY
  if (purify_pools_trace)
    if (result != ptr && ptr != 0)
      printf ("pool UNKNOWN, freed 0x%p (size %d)\n", ptr, old_size);
#endif

  if ( result == NULL )
    ErrMsg ( EC_No_Mem, "Realloc_Clear" );

  /* must clear the new portion of the allocated space */
  if ( new_size > old_size ) {
    MEM_PTR start_of_new = (MEM_PTR) ( ((char *) result) + old_size );
    INT32 num_added_bytes = new_size - old_size;
    bzero ( start_of_new, num_added_bytes );
  }

  return result;
}

/* ====================================================================
 *
 * MEM_PTR Re_Calloc ( ptr, new_nelem, elsize, old_nelem )
 *
 * This routine is used to grow space that was initially allocated via
 * a call to the Unix routine 'calloc()'.  'calloc()' is passed a number
 * of elements and an element size, and returns a pointer to *cleared*
 * memory for that many elements of that size:
 *
 *      MEM_PTR calloc ( UINT nelem, UINT elsize )
 *
 * This routine takes as input a pointer that was previously allocated
 * by 'calloc()', the same element size, and both the new and old number
 * of elements.  It calls 'realloc()' indirectly, via 'clear_realloc()',
 * to grow the space pointed to by the input 'ptr', clearing all the new
 * space.  Note that we need to know the old number of elements so we
 * can determine which portion of the returned space must be cleared
 * (the contents of the original space must be unchanged).
 *
 * The major functionality of this routine is provided by the support
 * routine 'clear_realloc()' -- we could easily only provide that
 * interface, but this gives a calloc()-style interface to the user who
 * likes to use 'calloc()'.
 *
 * ====================================================================
 */

MEM_PTR
Re_Calloc ( MEM_PTR ptr, INT32 new_nelem, INT32 elsize, INT32 old_nelem )
{
  return Realloc_Clear ( ptr, new_nelem * elsize, old_nelem * elsize );
}

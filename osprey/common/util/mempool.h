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


/* ====================================================================
 * ====================================================================
 *
 * Module: mempool.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:17:56 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/mempool.h,v $
 *
 * Revision history:
 *  19-Sep-89 - Original Version
 *  24-Jan-91 - Copied for TP/Muse
 *  28-May-91 - Integrated Josie routines
 *  20-Sep-91 - Added TYPE_X_MALLOC and TYPE_ALLOC macros
 *  05-Apr-93 - Rewritten from scratch to allow statistics, better
 *              handling of large blocks.
 *
 * Description:
 *
 * This package provides a different interface from the traditional C
 * malloc/free memory allocation, and is MUCH more effecient for a
 * particular style of memory allocation common in the compiler.  In
 * this style, individual memory blocks are not freed one at a time.
 * Rather all the outstanding memory in a particular memory pool is
 * freed as a whole.  Doing this allow us to replace large numbers of
 * calls to alloc/free with only a few calls which allocate and
 * deallocate large blocks.
 *
 * Each MEM_POOL has a current allocation level.  MEM_POOLs may be
 * Push'ed onto a new allocation level or Pop'ed back to their
 * previous allocation level.  When a MEM_POOL is pop'ed, all the
 * memory allocated from it since it was pushed to its current level
 * is freed.
 *
 * Compiling all the compiler sources with -DIs_True_On and MEM_Trace_Enable
 * enables collection of memory allocation statistics.  With these it is
 * possible to see a lot of information about the callsites where
 * memory is allocated.  In particular, it is possible to see how much
 * memory allocated from each callsite is outstanding.
 *
 * Before you begin
 * ================
 *
 *      void MEM_Initialize()
 *
 *          Initialize the memory pool package.
 *
 * Global Variables
 * ================
 *
 *	MEM_POOL *Malloc_Mem_Pool
 *
 *	    If this MEM_POOL is passed as an argument to an alloc or free
 *	    routine, use malloc instead of the mempools.  
 *	    Note that it causes the use of malloc and not calloc.  Memory
 *	    from this pool will not be automatically zeroed.
 *
 *      MEM_POOL *Default_Mem_Pool
 *
 *	    Use the default MEM_POOL  
 *
 *
 * Operations defined on MEM_POOLS
 * ===============================
 *
 *	void MEM_POOL_Set_Default(MEM_POOL *pool)
 *
 *	    All subsequent uses of Default_Mem_Pool will refer to pool
 *
 *      void MEM_POOL_Initialize(
 *          MEM_POOL *pool,
 *          char     *name,
 *          BOOL      bz
 *      )
 *
 *          Must be called on a pool before it used for the other
 *          operations.
 *
 *              pool    - is a pointer to the MEM_POOL to initialize.
 *              name    - is a name to use when printing information
 *                        about it
 *              bz      - If TRUE, memory allocated from the pool will
 *                        always be zero'ed
 *
 *          After a MEM_POOL has been initialized, it is important
 *          that the structure it points to at no point go out of
 *	    scope, so MEM_POOLs initialized this way should generally
 *	    statically allocated.
 *
 *
 *      BOOL MEM_POOL_Zeroed(MEM_POOL *pool)
 *
 *	    Returns TRUE iff <pool> zeroes newly allocated memory
 *	    (i.e., if <bz> argument was TRUE when pool was initialized).
 *
 *
 *      void MEM_POOL_Push(
 *          MEM_POOL *pool
 *      )
 *
 *          Check that the pool is not frozen. If not, 
 *          push a pool to a new allocation level.
 *
 *              pool    - is a pointer to the MEM_POOL to push
 *
 *      void MEM_POOL_Push_Freeze(
 *          MEM_POOL *pool
 *      )
 *      
 *          Same as MEM_POOL_Push, except that after being pushed, 
 *          the pool is frozen for subsequent push/pop operations
 *          until the pool is unfrozen.
 *
 *              pool    - is a pointer to the MEM_POOL to push
 *
 *      void MEM_POOL_Pop(
 *          MEM_POOL *pool
 *      )
 *
 *          Check that the pool is not frozen. If not, then
 *          pop a MEM_POOL to a previous allcation level.  All memory
 *          allocated since the matching call to MEM_POOL_Push is
 *          freed.  As a special case, unmatched pop's just free ALL
 *          outstanding storage in t
 *
 *              pool    - is a pointer to the MEM_POOL to pop
 *
 *      void MEM_POOL_Pop_Unfreeze(
 *          MEM_POOL *pool
 *      )
 *
 *          Same as MEM_POOL_Pop, except that the memory pool is first
 *          unfrozen. Must be called on a frozen memory pool.
 *
 *              pool    - is a pointer to the MEM_POOL to pop
 *
 *
 *      MEM_PTR MEM_POOL_Alloc(
 *          MEM_POOL *pool,
 *          size_t    size
 *      )
 *
 *          Allocate memory from a pool.  Always double word aligned.
 *
 *              pool    - is a pointer to the MEM_POOL for allocation
 *              size    - gives the number of bytes to allocate.
 *
 *          This is a low level usage and is discouraged in favor of
 *          the TYPE_ forms below.
 *
 *	    If pool == Malloc_Mem_Pool this just calls the system malloc
 *
 *
 *      MEM_PTR MEM_POOL_Realloc(
 *          MEM_POOL *pool,
 *          MEM_PTR   old_block,
 *          size_t    old_size,
 *          size_t    new_size
 *      )
 *
 *          Reallocate memory from a pool.
 *
 *              pool      - is a pointer to the MEM_POOL for allocation
 *              old_block - is a value previously returned from
 *                          MEM_POOL_Alloc, or MEM_POOL_Realloc.
 *                          (Very important.)  It must have been
 *                          (re)allocated from the given pool. (Very
 *                          important.)
 *              old_size  - is the size used to (re)allocate old_block
 *              new_size  - is the size new allocation size.
 *
 *          Returns a block of memory 'new_size' bytes long.  The
 *          first 'old_size' byte of the returned memory contain the
 *          same values as in the 'old_block'.
 *
 *          Implementation notes: There are some cases in which this
 *          is more effecient than just allocating a new block and
 *          copying in the firrt old_size bytes.  In particular, if
 *          'old_block' was the last block of memory allocated from
 *          'pool' the reallocation will often be 'in place'.  Even if
 *          it isn't in place, the memory for 'old_block' will be made
 *          available for future by the pool package.  If it
 *          especially important to get this effect for some reason,
 *          you should consider using a dedicated pool to hold the
 *          vector that you plan to realloc.
 *
 *	    If pool == Malloc_Mem_Pool this just calls the system realloc
 *
 *
 *      void MEM_POOL_FREE(
 *          MEM_POOL *pool,
 *	    void *data
 *      )
 *
 *	    Free the data from a MEM_POOL.  If pool == Malloc_Mem_Pool,
 *	    this calls the UNIX free.  Otherwise this is a no-op.
 *
 *
 *	void MEM_POOL_Delete(MEM_POOL *pool)
 *
 * 	    Free all the memory associated with <pool>.  Currently,
 *	    deleted pools are not reported by MEM_Trace (but this can
 *	    be changed).  Note that <*pool> itself is not freed.  A
 *	    deleted pool cannot be used unless it is first initialized
 *	    again with MEM_POOL_Initialize.
 *
 *
 *  Typed allocation macros
 *  =======================
 *
 *  Use of these forms is STRONGLY encouraged as they will allow us to
 *  collect information by types as will as by callsites.  They are
 *  also terser and less error prone than bare calls to alloc with
 *  sizeof and a cast.
 *
 *      type * TYPE_MEM_POOL_ALLOC(
 *          type,
 *          MEM_POOL *pool
 *      )
 *
 *      type * TYPE_MEM_POOL_ALLOC_N(
 *          type,
 *          MEM_POOL *pool,
 *          size_t    count
 *      )
 *
 *      type * TYPE_MEM_POOL_REALLOC_N(
 *          type,
 *          MEM_POOL *pool,
 *          type     *old_block,
 *          size_t    old_count,
 *          size_t    new_count
 *      )
 *
 *          Allocate one or more objects of a particular type from a
 *          pool and return a typed pointer.  The plain form allocates
 *          a single object.  The _N for allocates a vector of such
 *          objects.
 *
 *              type        - is the name of the type to allocate.  Enough
 *                            memory is allocated to hold one (plain form)
 *                            or n (_N form) for these.
 *              pool        - is a pointer to the MEM_POOL to use,
 *              count       - How many objects (_N form only).
 *
 *	    If pool == Malloc_Mem_Pool this calls the system malloc/realloc
 *
 *
 *      type * TYPE_MEM_POOL_REALLOC_N(
 *          type,
 *          MEM_POOL *pool,
 *          type     *old_block,
 *          size_t    old_count,
 *          size_t    new_count
 *      )
 *
 *          Reallocate a vector of one or more objects.  Both the old
 *          and new vectors must be in the same pool.
 *
 *              type        - is the name of the type to allocate.  Enough
 *                            memory is allocated to hold one (plain form)
 *                            or n (_N form) for these.
 *              pool        - is a pointer to the MEM_POOL to use,
 *              old_block   - is the block of memory to reallocate.
 *                            It must have been (re)allocated from the
 *                            given 'pool'
 *              old_count   - is the number of elements allocated in
 *                            'old_block'.  It is VERY IMPORTANT that
 *                            this be accurate.
 *              new_count   - How many objects
 *
 *
 *  Tracing memory allocation
 *  =========================
 *
 *  When Is_True_On is enabled, we also support:
 *
 *      void MEM_Tracing_Enable(void)
 *
 *          Tell the memory package to collect alloc call site
 *          statistics.
 *
 *      void MEM_Trace(void)
 *
 *          Produces a report on the TFile at to memory allocation.
 *          This report starts with the low-level malloc information
 *          (see malloc(3)) provided by the -lmalloc package.
 *          Then for each pool, a list MEM_POOL_Alloc callsites is
 *          given, sorted by the maximum amount of outstanding memory
 *          at any time for each callsite.  Here is a key to the per
 *          callsite information:
 *
 *              maxt    - Maximum outstanding memory at any one time
 *              curr    - Current memory outstanding
 *              tot     - Total memory allocated from this callsite
 *              maxs    - Largest memory block allocated from this
 *                        callsite
 *              count   - Count of times callsite was executed
 *              grew    - Count of times more memory was allocated
 *                        from this callsite than last time it was
 *                        executed.
 *              shrank  - Count of times less memory was allocated
 *                        from this callsite than last time it was
 *                        executed.
 *
 *	void Trace_Memory_Allocation(
 *	    const INT phase,
 *	    const char *const pname
 *	)
 *
 *	    Produce a memory allocation trace (via MEM_Trace) when
 *	    the trace switch -ta<m> is specified on the compile line.
 *
 *		phase - Phase after which we're printing
 *		pname - Print name for phase 
 *
 * Related Utilities
 * =================
 *
 * While not strictly MEM_POOL related, the following utilities are
 * are provided:
 *
 *      type * TYPE_ALLOCA(
 *          type
 *      )
 *
 *      type * TYPE_ALLOCA_N(
 *          type,
 *          size_t    count
 *      )
 *
 *          Allocate one or more objects of a particular type using
 *	    alloca and return a typed pointer.  The plain form allocates
 *          a single object.  The _N for allocates a vector of such
 *          objects.
 *
 *              type        - is the name of the type to allocate.  Enough
 *                            memory is allocated to hold one (plain form)
 *                            or n (_N form) for these.
 *              count       - How many objects (_N form only).
 *
 * Backward Compatibility
 * ======================
 *
 * For the sake of backward compatibility, this package also supports
 * an older interface.  It's direct usage in new code is discouraged.
 * It deals with allocations from four predefined pools which are
 * never directly accessed, but are accessed via function calls.
 *
 * The four predefined pools are:
 *
 *      L       - Pushed and popped to get local utility storage
 *      Pu      - Pushed and popped to get storage that can be freed
 *                up after we are done with each program unit
 *      Src     - Pushed and popped to get storage that can be freed
 *                up after we are done with each source file.
 *      P       - Pushed and popped to get utiity storage that can be
 *                freed up when we are done with the current "phase".
 *
 * All four pools support:
 *
 *      MEM_PTR X_Alloc(
 *          size_t size
 *      )
 *
 *      TYPE_X_ALLOC(
 *          type
 *      )
 *
 *      TYPE_X_ALLOC_N(
 *          type,
 *          INT32 n
 *      )
 *
 * (Where X is one of the predefined pools.)
 *
 * Additionally, the Local pool supports:
 *
 *      void L_Save(void)
 *
 *          Pushes the L pool.
 *
 *      void L_Free(void)
 *
 *          Pops the L pool.
 *
 * ====================================================================
 * ==================================================================== */





#ifndef mempool_INCLUDED
#define mempool_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif



#include "defs.h"

#define TYPE_ALLOCA(type) ((type *)alloca(sizeof(type)))
#define TYPE_ALLOCA_N(type,n) ((type *)alloca(sizeof(type) * (n)))


#define Malloc_Mem_Pool  (MEM_POOL *) 1
#define Default_Mem_Pool  (MEM_POOL *) 0

/* MEM_BLOCK - Internals private.
 */
typedef struct mem_block MEM_BLOCK;

/* MEM_POOL_BLOCKS - Internals private.
 */
typedef struct mem_pool_blocks MEM_POOL_BLOCKS;

/* MEM_STAT - Internals private.
 */
typedef struct mem_stat MEM_STAT;

/* MEM_PURE_STACK - Internals private.
 */
typedef struct mem_pure_stack MEM_PURE_STACK;

/* MEM_POOL - An object out of which to allocate and free memory.
 * See below for currently supported MEM_POOL operations.
 *
 * Internals private, but documented here, as the
 * declaration must be public so its size will be known to clients.
 */
typedef struct mem_pool MEM_POOL;
struct mem_pool {
  const char      *name;            /* Name of the pool for
                                     * debugging only.
                                     */
  MEM_POOL_BLOCKS *blocks;          /* Current top of allocation
                                     * stack.  This changes when the
                                     * pool is pushed or popped.
                                     */
  MEM_POOL        *rest;            /* Used to keep a link of all
                                     * initialized pools.
                                     */
  MEM_PURE_STACK *pure_stack;       /* Stack of pointers, one for each push 
                                     * level, each pointer pointing to first 
                                     * allocated piece in that push-level.
                                     */
  mBOOL             bz;             /* To allocate zero'd memory
                                     * or not out of this pool.
                                     */
  mBOOL           frozen;           /* If frozen, then subsequent Push/Pop
                                     * operations are disabled until the
                                     * mem_pool is unfrozen.
                                     * Default unfrozen.
                                     */
  mUINT16       magic_num;          /* Set to a a magic number when
                                     * the MEM_POOL is initialized, and
                                     * reset to zero when the pool is deleted.
                                     * Used to check that the pool is
                                     * initialized before a Push/Alloc.
                                     */
  MEM_STAT        *alloc_site_list; /* Used to report statistics
                                     * about call sites that have
                                     * outstanding allocation in the
                                     * pool.
                                     */
};

#define MEM_STAT_ARGS(line,file) ,INT32 line, const char *file

extern void
MEM_Initialize(void);

#define MEM_POOL_Zeroed(pool) ((pool)->bz)

/* These pools zero newly-allocated memory.
 */
extern MEM_POOL MEM_local_pool;
extern MEM_POOL MEM_src_pool;
extern MEM_POOL MEM_pu_pool;
extern MEM_POOL MEM_phase_pool;

extern MEM_POOL *MEM_local_pool_ptr;
extern MEM_POOL *MEM_src_pool_ptr;
extern MEM_POOL *MEM_pu_pool_ptr;
extern MEM_POOL *MEM_phase_pool_ptr;

/* These pools don't zero newly-allocated memory, and hence allocation
 * is faster than the zeroed pools above.  These should be used whenever
 * appropriate.
 */
extern MEM_POOL MEM_local_nz_pool;
extern MEM_POOL MEM_src_nz_pool;
extern MEM_POOL MEM_pu_nz_pool;
extern MEM_POOL MEM_phase_nz_pool;

extern MEM_POOL *MEM_local_nz_pool_ptr;
extern MEM_POOL *MEM_src_nz_pool_ptr;
extern MEM_POOL *MEM_pu_nz_pool_ptr;
extern MEM_POOL *MEM_phase_nz_pool_ptr;

extern void MEM_Trace(void);

extern void 
Trace_Memory_Allocation(
  INT phase,
  const char *pname
);

extern void MEM_Tracing_Enable(void);

extern MEM_PTR
MEM_POOL_Alloc_P
(
  MEM_POOL *mempool,
  size_t    size
  MEM_STAT_ARGS(line,file)
);

extern MEM_PTR
MEM_POOL_Realloc_P
(
  MEM_POOL *mempool,
  MEM_PTR   old_block,
  size_t    old_size,
  size_t    new_size
  MEM_STAT_ARGS(line,file)
);

extern void
MEM_POOL_Push_P
(
  MEM_POOL *pool
  MEM_STAT_ARGS(line,file)
);

extern void
MEM_POOL_Push_Freeze_P
(
  MEM_POOL *pool
  MEM_STAT_ARGS(line,file)
);

extern void
MEM_POOL_Pop_P
(
  MEM_POOL *pool
  MEM_STAT_ARGS(line,file)
);

extern void
MEM_POOL_Pop_Unfreeze_P
(
  MEM_POOL *pool
  MEM_STAT_ARGS(line,file)
);

extern void MEM_POOL_Set_Default(MEM_POOL *pool);
extern void MEM_POOL_FREE(MEM_POOL *pool, void *data);
extern void MEM_POOL_Delete(MEM_POOL *pool);

extern void
MEM_POOL_Initialize_P
(
  MEM_POOL     *pool,
  const char   *name,
  BOOL          bz
  MEM_STAT_ARGS(line,file)
);

#ifdef Is_True_On
#define MEM_POOL_Alloc(pool,size)                                       \
  MEM_POOL_Alloc_P(pool,size,__LINE__,__FILE__)

#define MEM_POOL_Realloc(pool,o,os,ns)                                  \
  MEM_POOL_Realloc_P((pool),(o),(os),(ns),__LINE__,__FILE__)

#define MEM_POOL_Push(pool)                                             \
  MEM_POOL_Push_P(pool,__LINE__,__FILE__)

#define MEM_POOL_Push_Freeze(pool)                                      \
  MEM_POOL_Push_Freeze_P(pool,__LINE__,__FILE__)

#define MEM_POOL_Pop(pool)                                              \
  MEM_POOL_Pop_P(pool,__LINE__,__FILE__)

#define MEM_POOL_Pop_Unfreeze(pool)                                     \
  MEM_POOL_Pop_Unfreeze_P(pool,__LINE__,__FILE__)

#define MEM_POOL_Initialize(pool,name,bz)                               \
  MEM_POOL_Initialize_P(pool,name,bz,__LINE__,__FILE__)
#else
#define MEM_POOL_Alloc(pool,size)                                       \
  MEM_POOL_Alloc_P(pool,size,0,NULL)

#define MEM_POOL_Realloc(pool,o,os,ns)                                  \
  MEM_POOL_Realloc_P((pool),(o),(os),(ns),0,NULL)

#define MEM_POOL_Push(pool)                                             \
  MEM_POOL_Push_P(pool,0,NULL)

#define MEM_POOL_Push_Freeze(pool)                                      \
  MEM_POOL_Push_Freeze_P(pool,0,NULL)

#define MEM_POOL_Pop(pool)                                              \
  MEM_POOL_Pop_P(pool,0,NULL)

#define MEM_POOL_Pop_Unfreeze(pool)                                     \
  MEM_POOL_Pop_Unfreeze_P(pool,0,NULL)

#define MEM_POOL_Initialize(pool,name,bz)                               \
  MEM_POOL_Initialize_P(pool,name,bz,0,NULL)
#endif

#define TYPE_MEM_POOL_ALLOC(type,pool)                                  \
    ((type *) MEM_POOL_Alloc(pool,sizeof(type)))

#define TYPE_MEM_POOL_ALLOC_N(type,pool,n)                              \
    ((type *) MEM_POOL_Alloc(pool,sizeof(type) * (n)))

#define TYPE_MEM_POOL_REALLOC_N(type,pool,old_block,old_n,new_n)        \
    ((type *) MEM_POOL_Realloc(pool,((MEM_PTR) old_block),              \
                                    (sizeof(type) * (old_n)),           \
                                    (sizeof(type) * (new_n))))


/* Backward compatibility
 */
#define L_Save()     MEM_POOL_Push(MEM_local_pool_ptr)
#define L_Alloc(x)   MEM_POOL_Alloc(MEM_local_pool_ptr,(x))
#define L_Free()     MEM_POOL_Pop(MEM_local_pool_ptr)

#define P_Alloc(x)   MEM_POOL_Alloc(MEM_phase_pool_ptr,(x))
#define Src_Alloc(x) MEM_POOL_Alloc(MEM_src_pool_ptr,(x))
#define Pu_Alloc(x)  MEM_POOL_Alloc(MEM_pu_pool_ptr,(x))

#define TYPE_L_ALLOC(type)       ((type*)L_Alloc(sizeof(type)))
#define TYPE_L_ALLOC_N(type,n)   ((type*)L_Alloc(sizeof(type)*(n)))
#define TYPE_P_ALLOC(type)       ((type*)P_Alloc(sizeof(type)))
#define TYPE_P_ALLOC_N(type,n)   ((type*)P_Alloc(sizeof(type)*(n)))
#define TYPE_PU_ALLOC(type)      ((type*)Pu_Alloc(sizeof(type)))
#define TYPE_PU_ALLOC_N(type,n)  ((type*)Pu_Alloc(sizeof(type)*(n)))
#define TYPE_SRC_ALLOC(type)     ((type*)Src_Alloc(sizeof(type)))
#define TYPE_SRC_ALLOC_N(type,n) ((type*)Src_Alloc(sizeof(type)*(n)))

/* Routines to grow space (based on realloc), clearing the new part of
 * the grown area while leaving the (copied) original contents
 * unmodified.
 */
MEM_PTR Realloc_Clear ( /* realloc and clear new space */
  MEM_PTR ptr,          /* Currently allocated memory */
  INT32 new_size,       /* New size desired */
  INT32 old_size        /* Old size (leave this much unchanged) */
);
MEM_PTR Re_Calloc (     /* Realloc and clear -- calloc-style call */
  MEM_PTR ptr,          /* Currently allocated memory */
  INT32 new_nelem,      /* New element count desired */
  INT32 elsize,         /* Element size */
  INT32 old_nelem       /* Old element count (leave unchanged) */
);


#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

/* Class for automatically pushing and popping a mempool.  This is useful
 * for automatically doing a MEM_POOL_Pop() whenever we return from a
 * function, and for guaranteeing that all objects allocated from a mempool
 * are deallocated before the mempool is popped.
 *
 * Use the class within a function body, like this:
 *
 *  RETURN_TYPE my_func(...)
 *  {
 *      // mempool must have already been initialized; typically it's a
 *      // file static or global variable.  You MUST declare popper before
 *      // you start allocating storage from mempool.
 *    MEM_POOL_Popper popper(&mempool); // constructor does MEM_POOL_Push()
 *
 *      // Temporary that's no longer needed after we return from my_func().
 *      // Its declaration MUST come after popper.
 *    DYN_ARRAY<WN *> array_of_wn(&mempool);
 *      // Other temporaries allocated from mempool.
 *
 *    ... // code that allocates elements for array_of_wn and other temps.
 *
 *    if (some_condition)
 *        // Temporaries are automatically destructed and mempool is popped,
 *        // in the right order, despite early return from function.
 *      return val1;
 *
 *      // Temporaries are automatically destructed and mempool is popped,
 *      // in the right order.
 *    return val2;
 *  }
 *
 * The reason the MEM_POOL_Popper destructor call (hence also the
 * MEM_POOL_Pop()) is guaranteed to follow the destruction of objects
 * allocated from the mempool is that C++ guarantees that auto objects are
 * destructed in the reverse order of their appearance in the block (see
 * Stroustrup, "C++ Programming Language" 3rd ed., p. 245).
 */

class MEM_POOL_Popper {
  MEM_POOL *pool;
public:
  MEM_POOL_Popper(MEM_POOL *_pool) : pool(_pool) { MEM_POOL_Push(pool); }
  ~MEM_POOL_Popper() { MEM_POOL_Pop(pool); }

  MEM_POOL* Pool () const { return pool; }
};

/* Similar to MEM_POOL_Popper, CXX_MEM_POOL is used for initializing and
 * automatically deleting a MEM_POOL within a local scope.
 */

class CXX_MEM_POOL {
  MEM_POOL mem_pool;

public:
  MEM_POOL *operator()() { return &mem_pool; }

  CXX_MEM_POOL(const char *name, BOOL do_bzero) {
    mem_pool.magic_num = 0;		/* force it to be uninitialized */
    MEM_POOL_Initialize(&mem_pool, name, do_bzero);
    MEM_POOL_Push(&mem_pool);
  };
  ~CXX_MEM_POOL() {
    MEM_POOL_Pop(&mem_pool);
    MEM_POOL_Delete(&mem_pool);
  }
};

/* MEM_POOL_Constructor:  to be phased out.
   This is used for those cases where the MEM_POOL needs to be file-level
   static or global.
*/

class MEM_POOL_Constructor
{
private:
   MEM_POOL *pool;

public:

    MEM_POOL_Constructor (MEM_POOL* p, const char* name, BOOL zero) : pool (p) {
	MEM_POOL_Initialize (pool, name, zero);
	MEM_POOL_Push (pool);
    }

    ~MEM_POOL_Constructor () {
	MEM_POOL_Pop (pool);
	MEM_POOL_Delete (pool);
    }

    MEM_POOL* Pool () const { return pool; };
};
#endif /* #ifdef __cplusplus */

#endif /* mempool_INCLUDED */

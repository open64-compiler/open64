/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* =======================================================================
 * =======================================================================
 *
 *  Module: op_map.c
 *  $Revision: 1.6 $
 *  $Date: 05/12/05 08:59:08-08:00 $
 *  $Author: bos@eng-24.pathscale.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.op_map.cxx $
 *
 *  Revision comments:
 *
 *  3-Apr-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Implementation of OP_MAPs.  See op_map.h for interface description.
 *
 * =======================================================================
 * =======================================================================
 */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static const char *source_file = __FILE__;
static const char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.op_map.cxx $ $Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "errors.h"
#include "mempool.h"
#include "cgir.h"

#ifdef DEBUG
#define inline static
#endif


/* =====================================================================
 *		     OP_MAP_Idx_Op Implementation
 * =====================================================================
 *
 * Note: Currently tuned to have a "working set" of one BB.  Working
 *       set can be extended in a variety of ways.  A BB_MAP of OP *
 *	 vectors could be kept, with the entries instantiated on-demand
 *       (giving an arbitrarily large working set, but this is probably
 *	 overkill).  A small array/list/tree of BBs and associated OP *
 *	 vectors probably represents the best space/time tradeoff.
 *
 * =====================================================================
 */

static BB *idx_op_bb;
static OP **idx_ops;
static UINT16 idx_ops_length;
static MEM_POOL idx_op_pool;


static void init_idx_ops(void)
{
  idx_op_bb = NULL;
  MEM_POOL_Initialize(&idx_op_pool, "OP_MAP_Idx_Op_pool", TRUE);
  MEM_POOL_Push(&idx_op_pool);
}


static void finish_idx_ops(void)
{
  MEM_POOL_Pop(&idx_op_pool);
  MEM_POOL_Delete(&idx_op_pool);
}


static void create_idx_ops(BB *bb)
/* ---------------------------------------------------------------------
 * Create a new idx_ops vector for <bb>.
 * ---------------------------------------------------------------------
 */
{
  UINT16 next_idx = BB_next_op_map_idx(bb);
  OP *op;

  /* Deallocate old vector (if any) */
  MEM_POOL_Pop(&idx_op_pool);
  MEM_POOL_Push(&idx_op_pool);

  /* Allocate new vector */
  idx_ops = TYPE_MEM_POOL_ALLOC_N(OP *, &idx_op_pool, next_idx);
  idx_ops_length = next_idx;
  idx_op_bb = bb;

  /* Fill it in */
  for (op = BB_first_op(bb); op; op = OP_next(op)) {
    Is_True(OP_map_idx(op) < next_idx,
	    ("OP_map_idx out of range"));
    idx_ops[OP_map_idx(op)] = op;
  }
}


OP *OP_MAP_Idx_Op(BB *bb, UINT16 idx)
{

  if (bb != idx_op_bb) {
    /* This is a different <bb> than that used in the last call,
     * so create a new idx_ops vector.
     */
    create_idx_ops(bb);
  }

  if (idx >= idx_ops_length) {
    if (idx < BB_next_op_map_idx(bb)) {
      /* <idx> is out of vector range, but may be legit, so
       * create new vector.
       */
      create_idx_ops(bb);
    } else {
      /* <idx> is bogus */
      return NULL;
    }
  }

  Is_True(idx_ops[idx] == NULL || OP_map_idx(idx_ops[idx]) == idx,
	  ("OP_map_idx disagrees with OP_MAP_Idx_Op"));
  return idx_ops[idx];
}

/* =====================================================================
 *			     HASH TABLES
 *
 * This HASH_TABLE is a collection of VALUEs (see below) keyed by OP *.
 * These HASH_TABLEs are designed to be frequently created and deleted;
 * both creation and deletion are generally constant-time operations,
 * though when no table is available for reuse creation takes time
 * linear in the number of hash buckets.
 * =====================================================================
 */

typedef union value {
  /*
   * These are the types that can be held in the table.  We're not
   * wasting space for the 32-bit quantities since we allocate in
   * doubleword-sized chunks and HASH_ENTRYs are an even number of
   * words this way.
   */
  mINT32 i32;
  void *ptr;
  mINT64 i64;
} VALUE;


typedef struct hash_entry {
  const OP *key;
    /* matching OP for this entry */
  struct hash_entry *next;
    /* next entry in hash bucket */
  VALUE val;
    /* entry's value */
} HASH_ENTRY;

#define MAX_BUCKETS_LOG2 32		/* max hash tbl len = 2^32 buckets */

typedef struct hash_table {
  UINT8 length_log2;
    /* number of buckets log 2 */
  HASH_ENTRY **buckets;
    /* array of <2 ^ length_log2> hash buckets */
  mUINT32 gen;
  mUINT32 *bucket_gens;
    /* generation counts: a bucket is valid iff gen == bucket_gens[hash] */
  struct hash_table *next_free;
} HASH_TABLE;

/* free_tables[n] holds a list of free hash tables of length 2^n */
static HASH_TABLE *free_tables[MAX_BUCKETS_LOG2+1];
#define next_free_table(tbl) ((tbl)->next_free)

static HASH_ENTRY *free_entries;		/* list of free entries */
#define next_free_entry(entry) ((entry)->next)


static void init_hash_tables(void)
{
  BZERO(free_tables, sizeof(free_tables));
  free_entries = NULL;
}


static HASH_TABLE *hash_table_create(UINT8 length_log2)
/* ---------------------------------------------------------------------
 * Create and return a new hash table with 2^length_log2 buckets.
 * ---------------------------------------------------------------------
 */
{
  HASH_TABLE *tbl;

  Is_True(length_log2 <= MAX_BUCKETS_LOG2,
	  ("can't make hash table with 2^%d buckets", length_log2));

  /* If we have to allocate a new table, this takes time
   * O(2^length_log2).  Otherwise it takes constant time.
   */

  tbl = free_tables[length_log2];
  if (tbl) {
    free_tables[length_log2] = next_free_table(tbl);
    tbl->gen += 1;
    if (tbl->gen == 0) {
      DevWarn("(Performance) OP_MAP generation overflow - zeroing");
      BZERO(tbl->bucket_gens, sizeof(mUINT32) * (1 << length_log2));
      tbl->gen = 1;
    }
  } else {
    tbl = TYPE_MEM_POOL_ALLOC(HASH_TABLE, &MEM_phase_nz_pool);
    tbl->length_log2 = length_log2;
    tbl->buckets = TYPE_MEM_POOL_ALLOC_N(HASH_ENTRY *, &MEM_phase_nz_pool,
					 1 << length_log2);
    tbl->gen = 1;

    /* Note that this is allocated in a zeroed MEM_POOL so all bucket_gens
     * are originally zero.
     */
    tbl->bucket_gens = TYPE_MEM_POOL_ALLOC_N(mUINT32, &MEM_phase_pool,
					     1 << length_log2);
  }
  return tbl;
}


static void hash_table_delete(HASH_TABLE *tbl)
/* ---------------------------------------------------------------------
 * Make hash table <tbl> available for reuse.
 * ---------------------------------------------------------------------
 */
{
  /* Takes constant time */
  next_free_table(tbl) = free_tables[tbl->length_log2];
  free_tables[tbl->length_log2] = tbl;
}


static HASH_ENTRY *hash_entry_create(void)
/* ---------------------------------------------------------------------
 * Return a new, uninitialized hash table entry, either from the free
 * list or from the phase pool.
 * ---------------------------------------------------------------------
 */
{
  HASH_ENTRY *entry = free_entries;
  if (entry) {
    free_entries = next_free_entry(free_entries);
  } else {
    entry = TYPE_MEM_POOL_ALLOC(HASH_ENTRY, &MEM_phase_nz_pool);
  }
  return entry;
}


static void hash_entries_delete(HASH_ENTRY *entries)
/* ---------------------------------------------------------------------
 * Free <entries> up so they can be reused later.
 * ---------------------------------------------------------------------
 */
{
  HASH_ENTRY *last = entries;
  while (last->next) last = last->next;
  next_free_entry(last) = free_entries;
  free_entries = entries;
}
  

/* ---------------------------------------------------------------------
 * The hash function: First divide the key (an OP *) by 8 (since we
 * know OPs are at least doubleword aligned), then do a mod by the
 * table length.  Can do this without divides since length (and 8)
 * are powers of two.
 * ---------------------------------------------------------------------
 */
#define hash(key, len_log2) (((INTPTR)key >> 3) & ((1 << len_log2) - 1))


inline HASH_ENTRY *hash_table_entry(HASH_TABLE *tbl, const OP *key, BOOL create)
/* ---------------------------------------------------------------------
 * Lookup the hash entry for <key> in <tbl> and return it if it's
 * found.  Otherwise, return NULL if <create> is FALSE, or return
 * a new entry with the given key if <create> is TRUE.
 * --------------------------------------------------------------------- */
{
  UINT32 idx = hash(key, tbl->length_log2);

  if (tbl->bucket_gens[idx] == tbl->gen) {
    /*
     * Bucket is valid, so search for entry.
     */
    HASH_ENTRY *entry = tbl->buckets[idx];
    Is_True(entry, ("initial bucket entry is NULL"));
    while (entry && entry->key != key)
      entry = entry->next;

    if (entry == NULL && create) {
      /*
       * Add a new entry to this bucket.
       */
      entry = hash_entry_create();
      entry->next = tbl->buckets[idx];
      tbl->buckets[idx] = entry;
      entry->key = key;
    }

    return entry;
  }

  if (create) {
    if (tbl->bucket_gens[idx] > 0) {
      /*
       * Bucket is old: free all but first entry, then return first.
       */
      HASH_ENTRY *rest = tbl->buckets[idx]->next;
      if (rest) hash_entries_delete(rest);
    } else {
      /*
       * Bucket is new.  Create its initial entry.
       */
      tbl->buckets[idx] = hash_entry_create();
    }
    tbl->buckets[idx]->key = key;
    tbl->buckets[idx]->next = NULL;
    tbl->bucket_gens[idx] = tbl->gen;
    return tbl->buckets[idx];
  }

  return NULL;
}

/* =====================================================================
 *			       OP_MAPs
 *
 * Global maps are implemented as HASH_TABLEs.
 * =====================================================================
 */

struct op_map {
  OP_MAP_KIND kind;			/* for error checking only */
  HASH_TABLE *tbl;
  struct op_map *next_free;
};

OP_MAP free_maps;
#define next_free_map(map) ((map)->next_free)


void OP_MAP_Init(void)
{
  init_hash_tables();
  init_idx_ops();
  free_maps = NULL;
}


void OP_MAP_Finish(void)
{
  finish_idx_ops();
}


OP_MAP _OP_MAP_Create(OP_MAP_KIND kind)
/* -----------------------------------------------------------------------
 * Return a new global map of the given <kind>.
 * -----------------------------------------------------------------------
 */
{
  OP_MAP result;

  /* Get an OP_MAP descriptor */
  if (free_maps) {
    result = free_maps;
    free_maps = next_free_map(free_maps);
    Is_True(result->kind == OP_MAP_DELETED,
	    ("map from free list not marked OP_MAP_DELETED"));
  } else {
    /* Allocate a new one */
    result = TYPE_MEM_POOL_ALLOC(struct op_map, &MEM_phase_nz_pool);
  }

  result->kind = kind;
  /* TODO: Choose table size less arbitrarily. */
  result->tbl = hash_table_create(9);

  return result;
}


void OP_MAP_Delete(OP_MAP map)
{
  /* Delete hash table */
  hash_table_delete(map->tbl);

  /* Put <map> on front of free list and mark deleted (for debug).
   */
  map->kind = OP_MAP_DELETED;
  next_free_map(map) = free_maps;
  free_maps = map;
}

#ifdef TARG_IA64
BOOL OP_MAP_Is_Delete(OP_MAP map)
{
   return map->kind == OP_MAP_DELETED;
}
#endif

void OP_MAP_Set(OP_MAP map, OP *op, void *value)
{
  HASH_ENTRY *entry = hash_table_entry(map->tbl, op, TRUE);
  Is_True(map->kind != OP_MAP_DELETED, ("accessing deleted OP_MAP"));
  Is_True(map->kind == OP_MAP_PTR, ("OP_MAP is of wrong kind"));
  entry->val.ptr = value;
}


void OP_MAP32_Set(OP_MAP map, OP *op, INT32 value)
{
  HASH_ENTRY *entry = hash_table_entry(map->tbl, op, TRUE);
  Is_True(map->kind != OP_MAP_DELETED, ("accessing deleted OP_MAP"));
  Is_True(map->kind == OP_MAP_I32, ("OP_MAP is of wrong kind"));
  entry->val.i32 = value;
}


void OP_MAP64_Set(OP_MAP map, OP *op, INT64 value)
{
  HASH_ENTRY *entry = hash_table_entry(map->tbl, op, TRUE);
  Is_True(map->kind != OP_MAP_DELETED, ("accessing deleted OP_MAP"));
  Is_True(map->kind == OP_MAP_I64, ("OP_MAP is of wrong kind"));
  entry->val.i64 = value;
}


void *OP_MAP_Get(OP_MAP map, const OP *op)
{
  HASH_ENTRY *entry = hash_table_entry(map->tbl, op, FALSE);
  Is_True(map->kind != OP_MAP_DELETED, ("accessing deleted OP_MAP"));
  Is_True(map->kind == OP_MAP_PTR, ("OP_MAP is of wrong kind"));
  return entry ? entry->val.ptr : NULL;
}


INT32 OP_MAP32_Get(OP_MAP map, const OP *op)
{
  HASH_ENTRY *entry = hash_table_entry(map->tbl, op, FALSE);
  Is_True(map->kind != OP_MAP_DELETED, ("accessing deleted OP_MAP"));
  Is_True(map->kind == OP_MAP_I32, ("OP_MAP is of wrong kind"));
  return entry ? entry->val.i32 : 0;
}


INT64 OP_MAP64_Get(OP_MAP map, const OP *op)
{
  HASH_ENTRY *entry = hash_table_entry(map->tbl, op, FALSE);
  Is_True(map->kind != OP_MAP_DELETED, ("accessing deleted OP_MAP"));
  Is_True(map->kind == OP_MAP_I64, ("OP_MAP is of wrong kind"));
  return entry ? entry->val.i64 : 0;
}

/* =====================================================================
 *			     BB_OP_MAPs
 *
 * Local maps are implemented using a dynamnically sized array, index
 * by OP_map_idx. The fast path for setting and getting map entries
 * is handled by inline functions in op_map.h. Here we handle the
 * unusual situations.
 * =====================================================================
 */

BB_OP_MAP BB_OP_MAP_Create_Kind(BB *bb, MEM_POOL *pool, OP_MAP_KIND kind)
/* -----------------------------------------------------------------------
 * Return a new local map of the given <kind>.
 * -----------------------------------------------------------------------
 */
{
  INT32 size;
  BB_OP_MAP new_map = TYPE_MEM_POOL_ALLOC(struct bb_op_map, pool);
#ifdef TARG_IA64
  INT32 nelem = BB_next_op_map_idx(bb) + 20;
#else
  INT32 nelem = BB_next_op_map_idx(bb);
#endif

  new_map->bb = bb;
  new_map->kind = kind;
  new_map->pool = pool;
  new_map->nelem = nelem;

  switch (kind) {
  case OP_MAP_I32:
    size = nelem * sizeof(INT32);
    break;
  case OP_MAP_I64:
    size = nelem * sizeof(INT64);
    break;
  default:
    FmtAssert(FALSE, ("unexpected BB_OP_MAP kind"));
    /*NOTREACHED*/
  case OP_MAP_PTR:
    size = nelem * sizeof(void *);
    break;
  }

  new_map->themap.ptr = (void **) MEM_POOL_Alloc(pool, size);
  if (!MEM_POOL_Zeroed(pool)) BZERO (new_map->themap.ptr, size);

  return new_map;
}


void BB_OP_MAP_Extend_Map(BB_OP_MAP map, OP *op)
/* -----------------------------------------------------------------------
 * Extend local map to include the specified OP.
 * -----------------------------------------------------------------------
 */
{
  INT32 idx = OP_map_idx(op);
  enum { INCR_PERCENT = 10 }; /* Percentage extra to add to extend size */

  FmtAssert(OP_bb(op) == map->bb, ("OP is not in BB_OP_MAP BB"));

  if (idx >= map->nelem) {
    INT32 elem_size;
    INT32 new_nelem = (BB_next_op_map_idx(map->bb) * (100 + INCR_PERCENT)) / 100;
    INT32 old_nelem = map->nelem;
    new_nelem = MAX(old_nelem * 2, new_nelem);
    FmtAssert(new_nelem > idx, ("OP map index is out of range"));

    switch (map->kind) {
    case OP_MAP_I32:
      elem_size = sizeof(INT32);
      break;
    case OP_MAP_I64:
      elem_size = sizeof(INT64);
      break;
    case OP_MAP_PTR:
      elem_size = sizeof(void *);
      break;
    }

    map->themap.ptr = (void **) MEM_POOL_Realloc(map->pool,
				    map->themap.ptr,
				    old_nelem * elem_size,
				    new_nelem * elem_size);
    if (!MEM_POOL_Zeroed(map->pool)) {
      BZERO((char *)map->themap.ptr + (old_nelem * elem_size),
	    elem_size * (new_nelem - old_nelem));
    }

    map->nelem = new_nelem;
  }
}

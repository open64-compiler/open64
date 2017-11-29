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


//-*-c++-*-
/* =======================================================================
 * =======================================================================
 *
 *  Module: tn_map.cxx
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:27-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.tn_map.cxx $
 *
 *  Description:
 *  ============
 *
 *  Implementation of TN_MAPs.  See tn_map.h for interface description.
 *
 * =======================================================================
 * =======================================================================
 */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "defs.h"
#include "errors.h"
#include "mempool.h"
#include "erglob.h"
#include "tn.h"
#include "tn_map.h"


typedef struct {
  void  *value;		/* information associated with the TN */
  INT32 generation;	/* generation number of the value */
} TN_MAP_INFO;

struct tn_map {
  INT32 current_gen;	/* the current generation count */
  INT32 table_size;	/* the size of the table array */
  TN_MAP_INFO *table;	/* table containing map information */
  struct tn_map *next;	/* pointer to next TN_MAP */
};

#define TN_MAP_current_gen(map)	((map)->current_gen)
#define TN_MAP_table_size(map)	((map)->table_size)
#define TN_MAP_table(map)	((map)->table)
#define TN_MAP_next(map)	((map)->next)
#define TN_MAP_value(map,tn)	((map)->table[TN_number(tn)].value)
#define TN_MAP_generation(map,tn) ((map)->table[TN_number(tn)].generation)

/* List of MAPs that have been allocated for this PU and then deleted */
static TN_MAP Free_TN_MAPs;

/* Initialize TN_MAP package for PU. */
void
TN_MAP_Init (void)
{
  Free_TN_MAPs = NULL;
}


TN_MAP
TN_MAP_Create (void)
{
  TN_MAP new_map;

  if (Free_TN_MAPs != NULL) {
    /* check if we have a free map available */
    new_map = Free_TN_MAPs;
    Free_TN_MAPs = TN_MAP_next(new_map);
  }
  else {
    /* allocate a new map */
    INT table_size = Last_TN + MAX ((Last_TN / 5), 100);
    new_map = TYPE_PU_ALLOC (struct tn_map);
    TN_MAP_current_gen(new_map) = 0;
    TN_MAP_table_size(new_map) = table_size;
    TN_MAP_table(new_map) = TYPE_PU_ALLOC_N (TN_MAP_INFO, table_size+1);
  }
  /* increment the generation number */
  TN_MAP_current_gen(new_map)++;

  return new_map;
}


void
TN_MAP_Set ( TN_MAP map, TN *tn, void *value)
{
  if (TN_number(tn) > TN_MAP_table_size(map)) {
    /* grow the TN_MAP. */
    INT new_size = MAX (Last_TN + MAX ((Last_TN / 5), 100),
			2 * TN_MAP_table_size(map));
    TN_MAP_table(map) = TYPE_MEM_POOL_REALLOC_N (
			    TN_MAP_INFO, &MEM_pu_pool, TN_MAP_table(map), 
			    TN_MAP_table_size(map)+1, new_size+1);
    TN_MAP_table_size(map) = new_size;
  }
  TN_MAP_value(map,tn) = value;
  TN_MAP_generation(map,tn) = TN_MAP_current_gen(map);
}

void *
TN_MAP_Get ( TN_MAP map, const TN *tn)
{
  void *retval;

  if (TN_number(tn) <= TN_MAP_table_size(map) &&
      TN_MAP_current_gen(map) == TN_MAP_generation(map,tn))
  {
    retval = TN_MAP_value(map,tn);
  }
  else {
    retval = NULL;
  }
  return retval;
}

void
TN_MAP_Delete (TN_MAP map)
{
  /* add map to the Free_TN_MAPs list */
  TN_MAP_next(map) = Free_TN_MAPs;
  Free_TN_MAPs = map;
}



// Linked lists of TN* and values, used as entries in the hash array

typedef struct hash_entry {
  TN *tn;
  void *value;
  struct hash_entry *next;
} HASH_ENTRY;

typedef struct hash_entry32 {
  TN *tn;
  INT32 value;
  struct hash_entry32 *next;
} HASH_ENTRY32;

typedef struct hash_entry64 {
  TN *tn;
  INT64 value;
  struct hash_entry64 *next;
} HASH_ENTRY64;

#define HASH_ENTRY_tn(entry)	((entry)->tn)
#define HASH_ENTRY_value(entry)	((entry)->value)
#define HASH_ENTRY_next(entry)	((entry)->next)


// Arrays of sise TN_MAP_HASH look up the hashed TN values

struct htn_map {
  MEM_POOL *map_pool;
  HASH_ENTRY   *map_entry[TN_MAP_HASH];
};

struct htn_map32 {
  MEM_POOL *map_pool;
  HASH_ENTRY32 *map_entry[TN_MAP_HASH];
};

struct htn_map64 {
  MEM_POOL *map_pool;
  HASH_ENTRY64 *map_entry[TN_MAP_HASH];
};

#ifdef TARG_IA64
//float hTN_MAP struct
typedef struct hash_entryf {
  TN *tn;
  float value;
  struct hash_entryf *next;
} HASH_ENTRYf;

struct htn_mapf {
  MEM_POOL *map_pool;
  HASH_ENTRYf *map_entry[TN_MAP_HASH];
};
#endif

#define hTN_MAP_pool(map)          ((map)->map_pool)
#define hTN_MAP_entry(map, index)  ((map)->map_entry[index])


// htN_MAP*_Create

hTN_MAP
hTN_MAP_Create(MEM_POOL *pool)
{
  hTN_MAP new_map;

  new_map = TYPE_MEM_POOL_ALLOC(struct htn_map, pool);
  BZERO(new_map, sizeof(struct htn_map));
  hTN_MAP_pool(new_map) = pool;
  return new_map;
}

hTN_MAP32
hTN_MAP32_Create(MEM_POOL *pool)
{
  hTN_MAP32 new_map;

  new_map = TYPE_MEM_POOL_ALLOC(struct htn_map32, pool);
  BZERO(new_map, sizeof(struct htn_map32));
  hTN_MAP_pool(new_map) = pool;
  return new_map;
}

hTN_MAP64
hTN_MAP64_Create(MEM_POOL *pool)
{
  hTN_MAP64 new_map;

  new_map = TYPE_MEM_POOL_ALLOC(struct htn_map64, pool);
  BZERO(new_map, sizeof(struct htn_map64));
  hTN_MAP_pool(new_map) = pool;
  return new_map;
}


// htN_MAP*_Set

void 
hTN_MAP_Set(hTN_MAP map, TN *tn, void *value)
{
  INT index = TN_number(tn) & (TN_MAP_HASH - 1);
  HASH_ENTRY *map_entry = hTN_MAP_entry(map, index);

  while (map_entry != NULL) {
    if (HASH_ENTRY_tn(map_entry) == tn) break;
    map_entry = HASH_ENTRY_next(map_entry);
  }
  if (map_entry == NULL) {
    map_entry = TYPE_MEM_POOL_ALLOC(HASH_ENTRY, hTN_MAP_pool(map));
    HASH_ENTRY_next(map_entry) = hTN_MAP_entry(map, index);
    hTN_MAP_entry(map, index) = map_entry;
    HASH_ENTRY_tn(map_entry) = tn;
  }
  HASH_ENTRY_value(map_entry) = value;
}

void 
hTN_MAP32_Set(hTN_MAP32 map, TN *tn, INT32 value)
{
  INT index = TN_number(tn) & (TN_MAP_HASH - 1);
  HASH_ENTRY32 *map_entry = hTN_MAP_entry(map, index);

  while (map_entry != NULL) {
    if (HASH_ENTRY_tn(map_entry) == tn) break;
    map_entry = HASH_ENTRY_next(map_entry);
  }
  if (map_entry == NULL) {
    map_entry = TYPE_MEM_POOL_ALLOC(HASH_ENTRY32, hTN_MAP_pool(map));
    HASH_ENTRY_next(map_entry) = hTN_MAP_entry(map, index);
    hTN_MAP_entry(map, index) = map_entry;
    HASH_ENTRY_tn(map_entry) = tn;
  }
  HASH_ENTRY_value(map_entry) = value;
}

void 
hTN_MAP64_Set(hTN_MAP64 map, TN *tn, INT64 value)
{
  INT index = TN_number(tn) & (TN_MAP_HASH - 1);
  HASH_ENTRY64 *map_entry = hTN_MAP_entry(map, index);

  while (map_entry != NULL) {
    if (HASH_ENTRY_tn(map_entry) == tn) break;
    map_entry = HASH_ENTRY_next(map_entry);
  }
  if (map_entry == NULL) {
    map_entry = TYPE_MEM_POOL_ALLOC(HASH_ENTRY64, hTN_MAP_pool(map));
    HASH_ENTRY_next(map_entry) = hTN_MAP_entry(map, index);
    hTN_MAP_entry(map, index) = map_entry;
    HASH_ENTRY_tn(map_entry) = tn;
  }
  HASH_ENTRY_value(map_entry) = value;
}


// htN_MAP*_Get

void *
hTN_MAP_Get(hTN_MAP map, TN *tn)
{
  INT index = TN_number(tn) & (TN_MAP_HASH - 1);
  HASH_ENTRY *map_entry = hTN_MAP_entry(map, index);

  while (map_entry != NULL) {
    if (HASH_ENTRY_tn(map_entry) == tn) 
      return HASH_ENTRY_value(map_entry);
    map_entry = HASH_ENTRY_next(map_entry);
  }
  return NULL;
}

INT32
hTN_MAP32_Get(hTN_MAP32 map, TN *tn)
{
  INT index = TN_number(tn) & (TN_MAP_HASH - 1);
  HASH_ENTRY32 *map_entry = hTN_MAP_entry(map, index);

  while (map_entry != NULL) {
    if (HASH_ENTRY_tn(map_entry) == tn) 
      return HASH_ENTRY_value(map_entry);
    map_entry = HASH_ENTRY_next(map_entry);
  }
  return 0;
}

INT64
hTN_MAP64_Get(hTN_MAP64 map, TN *tn)
{
  INT index = TN_number(tn) & (TN_MAP_HASH - 1);
  HASH_ENTRY64 *map_entry = hTN_MAP_entry(map, index);

  while (map_entry != NULL) {
    if (HASH_ENTRY_tn(map_entry) == tn) 
      return HASH_ENTRY_value(map_entry);
    map_entry = HASH_ENTRY_next(map_entry);
  }
  return 0;
}


// htN_MAP*_Get_And_Set

void *
hTN_MAP_Get_And_Set(hTN_MAP map, TN *tn, void *value)
{
  INT index = TN_number(tn) & (TN_MAP_HASH - 1);
  HASH_ENTRY *map_entry = hTN_MAP_entry(map, index);

  while (map_entry != NULL) {
    if (HASH_ENTRY_tn(map_entry) == tn) break;
    map_entry = HASH_ENTRY_next(map_entry);
  }
  void *answer = NULL;
  if (map_entry == NULL) {
    map_entry = TYPE_MEM_POOL_ALLOC(HASH_ENTRY, hTN_MAP_pool(map));
    HASH_ENTRY_next(map_entry) = hTN_MAP_entry(map, index);
    hTN_MAP_entry(map, index) = map_entry;
    HASH_ENTRY_tn(map_entry) = tn;
  } else
    answer = HASH_ENTRY_value(map_entry);
  HASH_ENTRY_value(map_entry) = value;
  return answer;
}

INT32
hTN_MAP32_Get_And_Set(hTN_MAP32 map, TN *tn, INT32 value)
{
  INT index = TN_number(tn) & (TN_MAP_HASH - 1);
  HASH_ENTRY32 *map_entry = hTN_MAP_entry(map, index);

  while (map_entry != NULL) {
    if (HASH_ENTRY_tn(map_entry) == tn) break;
    map_entry = HASH_ENTRY_next(map_entry);
  }
  INT32 answer = 0;
  if (map_entry == NULL) {
    map_entry = TYPE_MEM_POOL_ALLOC(HASH_ENTRY32, hTN_MAP_pool(map));
    HASH_ENTRY_next(map_entry) = hTN_MAP_entry(map, index);
    hTN_MAP_entry(map, index) = map_entry;
    HASH_ENTRY_tn(map_entry) = tn;
  } else
    answer = HASH_ENTRY_value(map_entry);
  HASH_ENTRY_value(map_entry) = value;
  return answer;
}

INT64
hTN_MAP64_Get_And_Set(hTN_MAP64 map, TN *tn, INT64 value)
{
  INT index = TN_number(tn) & (TN_MAP_HASH - 1);
  HASH_ENTRY64 *map_entry = hTN_MAP_entry(map, index);

  while (map_entry != NULL) {
    if (HASH_ENTRY_tn(map_entry) == tn) break;
    map_entry = HASH_ENTRY_next(map_entry);
  }
  INT64 answer = 0;
  if (map_entry == NULL) {
    map_entry = TYPE_MEM_POOL_ALLOC(HASH_ENTRY64, hTN_MAP_pool(map));
    HASH_ENTRY_next(map_entry) = hTN_MAP_entry(map, index);
    hTN_MAP_entry(map, index) = map_entry;
    HASH_ENTRY_tn(map_entry) = tn;
  } else
    answer = HASH_ENTRY_value(map_entry);
  HASH_ENTRY_value(map_entry) = value;
  return answer;
}

#ifdef TARG_IA64
//float
hTN_MAPf
hTN_MAPf_Create(MEM_POOL *pool)
{
  hTN_MAPf new_map;

  new_map = TYPE_MEM_POOL_ALLOC(struct htn_mapf, pool);
  BZERO(new_map, sizeof(struct htn_mapf));
  hTN_MAP_pool(new_map) = pool;
  return new_map;
}

void 
hTN_MAPf_Set(hTN_MAPf map, TN *tn, float value)
{
  INT index = TN_number(tn) & (TN_MAP_HASH - 1);
  HASH_ENTRYf *map_entry = hTN_MAP_entry(map, index);

  while (map_entry != NULL) {
    if (HASH_ENTRY_tn(map_entry) == tn) break;
    map_entry = HASH_ENTRY_next(map_entry);
  }
  if (map_entry == NULL) {
    map_entry = TYPE_MEM_POOL_ALLOC(HASH_ENTRYf, hTN_MAP_pool(map));
    HASH_ENTRY_next(map_entry) = hTN_MAP_entry(map, index);
    hTN_MAP_entry(map, index) = map_entry;
    HASH_ENTRY_tn(map_entry) = tn;
  }
  HASH_ENTRY_value(map_entry) = value;
}

float
hTN_MAPf_Get(hTN_MAPf map, TN *tn)
{
  INT index = TN_number(tn) & (TN_MAP_HASH - 1);
  HASH_ENTRYf *map_entry = hTN_MAP_entry(map, index);

  while (map_entry != NULL) {
    if (HASH_ENTRY_tn(map_entry) == tn) 
      return HASH_ENTRY_value(map_entry);
    map_entry = HASH_ENTRY_next(map_entry);
  }
  return 0.0;
}
#endif


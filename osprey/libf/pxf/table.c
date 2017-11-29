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


#pragma ident "@(#) libf/pxf/table.c	92.2	10/29/99 21:41:49"

#include <malloc.h>
#include <string.h>
#include <liberrno.h>
#include <errno.h>
#include "table.h"

/*
 * PXF table manager functions (used by PXF routines)
 *
 * These functions maintain lookup tables for structs. The _table_*
 * functions maintain lookup table for regular structs, while
 * the _pxfhandle_table_* functions maintain lookup tables for
 * handles used. The _table_* are used by the pxf*dir routines.
 * Any routine which takes a handle as an input or output argument
 * uses the _pxfhandle_table_* family of functions.
 *
 * Both families of table functions do not reclaim entries after
 * the information has been removed. Therefore, the table can only
 * grow larger with each *_add.
 *
 * Mutex locking is provided to protect the table datastructures from
 * during execution in a multi-threaded program.
 *
 * These functions assume one-based indicies, like FORTRAN arrays.
 * Internally, zero-based indicies are used, but the indicies passed in
 * and returned from these functions are one-based.
 *
 */

#define GROW_SIZE 10
#define NEW_SIZE(n) (n + GROW_SIZE)
#define ONE_INDEX(zero_index) (zero_index + 1) /* computes the one-based
						  next index */
#define ZERO_INDEX(one_index) (one_index - 1) /* computes the zero-based
						 next index */

/* global tables used by the pxf routines */
pxfhandle_table_t _pxfhandle_table;
table_t _pxfdir_table;

/*
 * _table_add
 *
 * adds an entry to a table
 *
 */
_f_int
_table_add(table_t *table,
	   void *entry)
{
  _f_int return_val;
  void **temp;
  
  MEM_LOCK(&table->mutex)
    
  /* check table size, and enlarge the entry table,
     if needed */
  if (table->size <= table->next_index &&
      (temp =
       (void **) realloc(table->entries,
			 sizeof(void *)*NEW_SIZE(table->size)))
      == NULL) {
    return_val = -1;
  } else {
    return_val = ONE_INDEX(table->next_index);
    if (table->size <= table->next_index) {
      table->size = NEW_SIZE(table->size);
      table->entries = temp;
    }
    table->entries[table->next_index++] = entry;
  }
  
  MEM_UNLOCK(&table->mutex)
  return(return_val);
}


/*
 * _table_remove
 *
 * removes an entry from a table
 *
 */
int
_table_remove(table_t *table,
	      _f_int entry_num)
{
  int return_val;
  int entry_num_zero = ZERO_INDEX(entry_num);

  MEM_LOCK(&table->mutex)

  if (entry_num_zero < table->next_index &&
      entry_num_zero >= 0) {
    free(table->entries[entry_num_zero]);
    table->entries[entry_num_zero] = NULL;
    return_val = 1;
  } else {
    return_val = 0;
  }
    
  MEM_UNLOCK(&table->mutex)
  return(return_val);
}


/*
 * _table_lookup
 *
 * looks up an entry number in a table
 *
 */
void *
_table_lookup(table_t *table,
	      _f_int entry_num)
{
  void *return_val;
  int entry_num_zero = ZERO_INDEX(entry_num);

  MEM_LOCK(&table->mutex)

  if (entry_num_zero < table->size &&
      entry_num_zero >= 0) {
    return_val = table->entries[entry_num_zero];
  } else {
    return_val = NULL;
  }

  MEM_UNLOCK(&table->mutex)
  return(return_val);
}



/* pxfstruct functions */

/*
 * _pxfhandle_table_add
 *
 * adds an entry to a pxfhandle table
 *
 */
_f_int
_pxfhandle_table_add(pxfhandle_table_t *table,
		     void *entryp,
		     int entry_type)
{
  _f_int return_val;
  struct pxfhandle *temp;

  MEM_LOCK(&table->mutex)

  /* check table size, and enlarge the entry table,
     if needed*/
  if (table->size <= table->next_index &&
      (temp =
       (struct pxfhandle *)realloc(table->entries,
				   sizeof(struct pxfhandle)*
				   NEW_SIZE(table->size)))
      == NULL) {
    return_val = -1;
  } else {
    return_val = ONE_INDEX(table->next_index);
    if (table->size <= table->next_index) {
      table->size = NEW_SIZE(table->size);
      table->entries = temp;
    }
    table->entries[table->next_index].pxfstructptr = entryp;
    table->entries[table->next_index++].pxftype = entry_type;
  }

  MEM_UNLOCK(&table->mutex)
  return(return_val);
}


/*
 * _pxfhandle_table_remove
 *
 * removes an entry from a pxfhandle table
 *
 */
int
_pxfhandle_table_remove(pxfhandle_table_t *table,
			_f_int entry_num)
{
  int return_val;
  int entry_num_zero = ZERO_INDEX(entry_num);

  MEM_LOCK(&table->mutex)

  if (entry_num_zero < table->next_index &&
      entry_num_zero >= 0) {
    free(table->entries[entry_num_zero].pxfstructptr);
    /* zero out the entry */
    (void)memset(&(table->entries[entry_num_zero]), 0,
		 sizeof(struct pxfhandle));
    return_val = 1;
  } else {
    return_val = 0;
  }

  MEM_UNLOCK(&table->mutex)
  return(return_val);
}


/*
 * _pxfhandle_table_replace
 *
 * replaces an entry with an entry of the same type in a pxfhandle table
 *
 */
int
_pxfhandle_table_replace(pxfhandle_table_t *table,
			 _f_int entry_num,
			 void *new_entry,
			 int entry_type)
{
  int return_val;
  int entry_num_zero = ZERO_INDEX(entry_num);

  MEM_LOCK(&table->mutex)
  
  /* remove the entry */
  if (entry_num_zero < table->next_index &&
      entry_num_zero >= 0 &&
      table->entries[entry_num_zero].pxftype == entry_type) {
    return_val = 1;
    free(table->entries[entry_num_zero].pxfstructptr);
    table->entries[entry_num_zero].pxfstructptr = new_entry;
  } else {
    return_val = 0;
  }

  MEM_UNLOCK(&table->mutex)
  return(return_val);
}


/*
 * _pxfhandle_table_lookup
 *
 * looks up an entry in a pxfhandle table
 *
 */
struct pxfhandle
_pxfhandle_table_lookup(pxfhandle_table_t *table,
			_f_int entry_num)
{
  struct pxfhandle return_val;
  int entry_num_zero = ZERO_INDEX(entry_num);

  MEM_LOCK(&table->mutex)

  if (entry_num_zero < table->size &&
      entry_num_zero >= 0) {
    return_val = table->entries[entry_num_zero];
  } else {
    (void)memset(&return_val, 0, sizeof(struct pxfhandle));
  }

  MEM_UNLOCK(&table->mutex)
  return(return_val);
}







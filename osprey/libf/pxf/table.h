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


/* USMID @(#) libf/pxf/table.h	92.0	10/08/98 14:30:10 */

#include <fortran.h>
#include <cray/mtlock.h>
#include "pxfstruct.h"

typedef struct table {
  _f_int next_index;
  _f_int size;
  DECL_LOCK(mutex)
  void **entries;
} table_t;

typedef struct pxfhandle_table {
  _f_int next_index;
  _f_int size;
  DECL_LOCK(mutex)
  struct pxfhandle *entries;
} pxfhandle_table_t;

/* global variables for tables */
extern pxfhandle_table_t _pxfhandle_table;
extern table_t _pxfdir_table;

/* functions for maintaining pointer tables */
extern _f_int _table_add(table_t *, void *);
extern int _table_remove(table_t *, _f_int);
extern void *_table_lookup(table_t *, _f_int);

/* functions for maintaining handle tables */
extern _f_int _pxfhandle_table_add(pxfhandle_table_t *, void *, int);
extern int _pxfhandle_table_remove(pxfhandle_table_t *, _f_int);
extern int _pxfhandle_table_replace(pxfhandle_table_t *, _f_int, void *, int);
extern struct pxfhandle _pxfhandle_table_lookup(pxfhandle_table_t *, _f_int);

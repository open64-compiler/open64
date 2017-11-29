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
 * Module: cwh_mkdepend.h
 * $Revision$
 * $Date$
 * $Author$ 
 * $Source$
 *
 * Revision history:
 *  09-26-95 - Original Version
 *
 * Description:
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_MKDEPEND_INCLUDED
#define CWH_MKDEPEND_INCLUDED


   
extern char * mdupdate_file;
extern char * mdtarget_file;


/* Generic routines for manipulating tables */
typedef struct {
   INT32 current_idx;  /* Last used index in table */
   INT32 current_size; /* How big is the current table */
   void **ptr;         /* pointer to the data */
} table_s, *table_p;

extern INT32 cwh_next_table_entry( table_p t);
extern void  cwh_add_to_module_files_table(char *name);
extern void  cwh_mkdepend_add_name(INT32 idx, char * name) ;
extern void  cwh_write_makedepend(void) ;

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

extern void  cwh_add_to_used_files_table(char * name, INT duplicate) ;

#ifdef __cplusplus
}
#endif /* __cplusplus */




#define TABLE_SIZE(x) (x.current_size)
#define TABLE_CURRENT_IDX(x) (x.current_idx)
#define TABLE_PTR(x) (x.ptr)
#define TABLE_IDX(x,i) ((x).ptr[i])
#define SET_TABLE_IDX(x,i,val) (x).ptr[(i)] = (void *) (val)
#define TABLE_TOP(x,ty) ((ty *) ((x).ptr[(x).current_idx]))
#define TABLE_INCREMENT 16
#define INIT_TABLE {-1,0,NULL}

#endif /* CWH_MKDEPEND_INCLUDED */


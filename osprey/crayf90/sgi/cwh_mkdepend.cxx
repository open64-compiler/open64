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


/* ====================================================================
 * ====================================================================
 *
 * Module: cwh_pdgcs
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:57:32-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: 
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: contains administrative routines invoked from the Cray 
 *              FE at initialization and termination of the 
 *              compiler and around each program unit. PDGCS_* entry 
 *              points plus fei_* routines which deal with procedure
 *              initialization and cleanup.
 * 
 * ====================================================================
 * ====================================================================
 */

static const char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/crayf90/sgi/SCCS/s.cwh_mkdepend.cxx $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include <ctype.h>

/* sgi includes */

#include "defs.h"
#include "glob.h"
#include "strtab.h"
#include <sys/types.h>
#include "tracing.h"
#include "cmplrs/make_depend.h"
#include <stdarg.h>
#include "erfe90.h"

#include "i_cvrt.h"

/* conversion includes */

#include  "sgi_cmd_line.h"
#include  "cwh_mkdepend.h"


char * mdupdate_file = NULL;
char * mdtarget_file = NULL;

/* Utility routines for tables */
extern INT32
cwh_next_table_entry( table_p t)
{
   if (t->current_size == 0) {
      t->ptr = (void**) malloc(sizeof(void *) * TABLE_INCREMENT);
      bzero(t->ptr,sizeof (void *) * TABLE_INCREMENT);
      t->current_size = TABLE_INCREMENT;
   } 

   t->current_idx += 1;
   if (t->current_idx >= t->current_size) {
      /* reallocate the table */
      t->current_size += TABLE_INCREMENT;
      t->ptr = (void **) realloc (t->ptr, sizeof (void *) * t->current_size);
      bzero(t->ptr+(t->current_size-TABLE_INCREMENT),sizeof (void *) * TABLE_INCREMENT);
   }
   return (t->current_idx);
}

static table_s name_table = INIT_TABLE;
static table_s used_files_table = INIT_TABLE;
static table_s module_files_table = INIT_TABLE;

INT32
fei_next_name(INT32 num)
{
   /* Look at the name table. If the last pointer is NULL, return it, otherwise
    * get the next one.
    */
   if (TABLE_SIZE(name_table) == 0) {
      return (cwh_next_table_entry(&name_table));
   }
   if (TABLE_TOP(name_table,char *) == NULL) {
      return (TABLE_CURRENT_IDX(name_table));
   } else {
      return (cwh_next_table_entry(&name_table));
   }
}


/* Add a name to the name table */
extern void
cwh_mkdepend_add_name(INT32 idx, char * name)
{
   SET_TABLE_IDX(name_table, idx, strdup(name));
}


/* Add to the used_files_table */

extern void
cwh_add_to_used_files_table(char * name, INT duplicate)
{
   INT i;
   char *t;

   /* add it to the used files table */
   i = cwh_next_table_entry(&used_files_table);
   if (duplicate) {
      t = strdup(name);
   } else {
      t = name;
   }
   SET_TABLE_IDX(used_files_table,i,t);
}

/* Add to the modules generated table, appending .mod */
extern void
cwh_add_to_module_files_table(char *name)
{
   INT i,len;
   char * fname;
   len = strlen(name);
   fname = (char *) malloc(len + 4);
   /* Uppercase the string */
   for (i = 0; i < len - 1; i++) {
      fname[i] = islower(name[i]) ? toupper(name[i]) : name[i];
   }
   fname[len-1]='.';
   fname[len]='m';
   fname[len+1]='o';
   fname[len+2]='d';
   fname[len+3]='\0';
   i = cwh_next_table_entry(&module_files_table);
   SET_TABLE_IDX(module_files_table,i,fname);
}


/*===============================================
 *
 * fei_add_use_path
 *
 * Get the name of all MODULEs used.
 *
 *===============================================
 */ 
void
fei_add_use_path(INTPTR st_idx,INT32 path_idx,INT32 module_idx)
{
   INT i;
   
   char *path_name;
   
   path_name = (char *) TABLE_IDX(name_table,path_idx);
   cwh_add_to_used_files_table(path_name,FALSE);
}


static void makedepend_error(const char *fmt, ...)
{
    static char makedepend_errbuf[400];
    va_list ap;

    va_start(ap,fmt);
    vsprintf(makedepend_errbuf, fmt, ap);
    va_end(ap);
    ErrMsg(EC_Makedepend_Error,makedepend_errbuf);
}

      
extern void
cwh_write_makedepend(void)
{
   INT i;
   MDhandle h;

   if (mdupdate_file == NULL || mdtarget_file == NULL) return;

   h = MDopen("mfef90",mdupdate_file,mdtarget_file,makedepend_error);

   for (i=0; i <= TABLE_CURRENT_IDX(used_files_table) ; i++) {
      MDupdate(h,(char *)TABLE_IDX(used_files_table,i));
   }
   MDclose(h,NULL);

   for (i=0; i <= TABLE_CURRENT_IDX(module_files_table) ; i++) {
      h = MDopen("mfef90",mdupdate_file,(char *)TABLE_IDX(module_files_table,i),NULL);
      MDupdate(h,mdtarget_file);
      MDclose(h,NULL);
   }
}

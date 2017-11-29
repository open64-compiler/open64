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
 * Module: printsrc.c
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:38-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.printsrc.cxx $
 *
 * Revision history:
 *  10-june-93 - Original Version
 *
 * Description: print the given source line alongside the current dump
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
static const char rcs_id[] = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.printsrc.cxx $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include <stdio.h>
#include "defs.h"
#include "erglob.h"
#include "tracing.h"
#include "dwarf_DST.h"
#include "dwarf_DST_mem.h"
#include "printsrc.h"

typedef struct {
  char *filename;
  INT incl_index;
  FILE *fileptr;
  INT32 foffset;
  INT32 max_line_printed;
  INT32 lastline;
} file_info;

static file_info *file_table;
static char **incl_table;
static INT cur_file_index = 0;
static BOOL no_source;

static void Gen_File_Table(void)
{
  INT count;
  DST_IDX idx;
  DST_INCLUDE_DIR *incl;
  DST_FILE_NAME *file;
  char *name;
  INT incl_table_size;
  INT file_table_size;
  INT new_size;

  no_source = Get_Trace(TKIND_INFO, TINFO_SOURCE) != 0;
  if (no_source) return;

  incl_table_size = 0;
  incl_table = NULL;
  file_table_size = 0;
  file_table = NULL;
  count = 1;
  for (idx = DST_get_include_dirs (); 
       !DST_IS_NULL(idx); 
       idx = DST_INCLUDE_DIR_next(incl))
  {
    incl = DST_DIR_IDX_TO_PTR (idx);
    name = DST_STR_IDX_TO_PTR (DST_INCLUDE_DIR_path(incl));
    if (count >= incl_table_size) {
      new_size = count + 10;
      if (incl_table == NULL)
        incl_table = (char **) malloc (new_size * sizeof (char *));
      else 
	incl_table = (char **) realloc (incl_table, new_size * sizeof (char *));
      if (incl_table == NULL) ErrMsg (EC_No_Mem, "Gen_File_Table");
      incl_table_size = new_size;
    }
    incl_table[count] = name;
    count++;
  }

  count = 1;
  for (idx = DST_get_file_names (); 
       !DST_IS_NULL(idx); 
       idx = DST_FILE_NAME_next(file))
  {
    file = DST_FILE_IDX_TO_PTR (idx);
    if (DST_IS_NULL(DST_FILE_NAME_name(file))) {
      name = const_cast<char*>("NULLNAME");
    }
    else {
      name = DST_STR_IDX_TO_PTR (DST_FILE_NAME_name(file));
    }
    if (count >= file_table_size) {
      new_size = count + 10;
      if (file_table == NULL)
        file_table = (file_info *) malloc (new_size * sizeof (file_info));
      else 
	file_table = (file_info *) realloc (file_table, 
					    new_size * sizeof (file_info));
      if (file_table == NULL) ErrMsg (EC_No_Mem, "Gen_File_Table");
      file_table_size = new_size;
    }
    file_table[count].filename = name;
    file_table[count].incl_index = DST_FILE_NAME_dir(file);
    file_table[count].fileptr = NULL;
    file_table[count].foffset = 0;
    file_table[count].max_line_printed = 0;
    file_table[count].lastline = 0;
    count++;
  }
}


/* ====================================================================
 *
 *  Print_Src_Line
 *
 *  Prints out the given line from the source file.  If the current line
 *  is more than 20 away from the last line printed, it will skip some
 *  lines and print the 20 lines leading up to the current line.  If the
 *  current line is smaller, it will go back and print only the current
 *  line.
 *
 * ==================================================================== */
void Print_Src_Line(SRCPOS srcpos, FILE *f)
{
  char buf[1024];
  file_info *cur_file;
  FILE *fileptr;
  INT32 line;
  static BOOL initialized = FALSE;
  INT32 filenum = SRCPOS_filenum(srcpos);

  if (filenum == 0) return;

  if (!initialized) {
    Gen_File_Table();
    initialized = TRUE;
  }

  if (no_source) return;

  cur_file = &file_table[filenum];
  if (filenum != cur_file_index) {
    if (cur_file_index != 0) {

      /* close the previous file, saving it's current position.
       */
      file_info *prev_file = &file_table[cur_file_index];
      prev_file->foffset = ftell(prev_file->fileptr);
      fclose (prev_file->fileptr);
      prev_file->fileptr = NULL;
    }
    cur_file_index = filenum;
    cur_file = &file_table[cur_file_index];
    /* open the new file. */
    if (cur_file->incl_index == 0)
    sprintf (buf, "%s", cur_file->filename);
    else
    sprintf (buf, "%s/%s", incl_table[cur_file->incl_index], 
			   cur_file->filename);
    cur_file->fileptr = fopen (buf, "r");
    if (cur_file->fileptr == NULL) {
      cur_file_index = 0;	/* indicate invalid cur_file */
      return;
    }

    /* If we've opened this file before, return to its last position.
     */
    fseek(cur_file->fileptr, cur_file->foffset, SEEK_SET);
  }
  fileptr = cur_file->fileptr;

  line = SRCPOS_linenum(srcpos);
  if (line == cur_file->lastline || fileptr == NULL) return;

  if (line < cur_file->lastline) {	/* need to back up */
    fseek(fileptr, 0, SEEK_SET);	/* reset to beginning of file */
    cur_file->lastline = 0;
  }

  for (;
       cur_file->lastline < MAX(MIN(line-1, cur_file->max_line_printed-1), line - 20); 
       cur_file->lastline++)
  {
    fgets(buf, sizeof(buf), fileptr);
  }

  for (; cur_file->lastline < line; cur_file->lastline++) {
    if (fgets(buf, sizeof(buf), fileptr) != NULL) {
      fprintf(f, "|||||[%d]%s", cur_file->lastline+1, buf);
    }
  }

  if (line > cur_file->max_line_printed) cur_file->max_line_printed = line;
}

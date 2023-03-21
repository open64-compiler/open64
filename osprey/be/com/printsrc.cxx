/*
 *  Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
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
#include "whirl_file_mgr.h"   // WHIRL_FILE_MANAGER

struct file_info {
  char *filename;
  INT incl_index;
  FILE *fileptr;
  INT32 foffset;
  INT32 max_line_printed;
  INT32 lastline;
};

static struct FILE_INFO_CONTEXT  Default_fi_ctx;
static struct FILE_INFO_CONTEXT *file_info_ctx = &Default_fi_ctx;
static BOOL no_source;

static void Gen_File_Table(void)
{
  INT count;
  DST_IDX idx;
  DST_INCLUDE_DIR *incl;
  DST_FILE_NAME *file;
  char *name;
  char **incl_table;
  struct file_info *file_table;
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
  file_info_ctx->_file_table = file_table;
  file_info_ctx->_file_info_count = count;
  file_info_ctx->_incl_table = incl_table;
  file_info_ctx->_cur_file_index = 0;
  file_info_ctx->_initialized = TRUE;
}

void Set_file_info_context(const FILE_INFO_CONTEXT* ctx)
{
  file_info_ctx = (FILE_INFO_CONTEXT *)ctx;
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
  INT32 filenum = SRCPOS_filenum(srcpos);

  if (filenum == 0) return;

  if (!file_info_ctx->_initialized) {
    Gen_File_Table();
  }

  if (no_source) return;

  struct file_info *file_table = file_info_ctx->_file_table;
  char **incl_table = file_info_ctx->_incl_table;
  cur_file = &file_table[filenum];
  if (filenum != file_info_ctx->_cur_file_index) {
    if (file_info_ctx->_cur_file_index != 0) {

      /* close the previous file, saving it's current position.
       */
      file_info *prev_file = &file_table[file_info_ctx->_cur_file_index];
      prev_file->foffset = ftell(prev_file->fileptr);
      fclose (prev_file->fileptr);
      prev_file->fileptr = NULL;
    }
    file_info_ctx->_cur_file_index = filenum;
    cur_file = &file_table[file_info_ctx->_cur_file_index];
    /* open the new file. */
    if (cur_file->incl_index == 0)
    sprintf (buf, "%s", cur_file->filename);
    else
    sprintf (buf, "%s/%s", incl_table[cur_file->incl_index], 
			   cur_file->filename);
    cur_file->fileptr = fopen (buf, "r");
    if (cur_file->fileptr == NULL) {
      file_info_ctx->_cur_file_index = 0;	/* indicate invalid cur_file */
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

void
Get_Local_Srcpos_Filename(SRCPOS spos, const char **fname, const char **dirname)
{
  if (SRCPOS_filenum(spos) == 0) {
    *fname = NULL;
    *dirname = NULL;
  }
  else {
    if (!file_info_ctx->_initialized) {
      Gen_File_Table();
    }
    Is_True(file_info_ctx->_file_info_count > 1, ("empty file info table"));
    if(file_info_ctx->_file_info_count > 1) {
      file_info *cur_file = &file_info_ctx->_file_table[SRCPOS_filenum(spos)];
      *fname = cur_file->filename;
      *dirname = file_info_ctx->_incl_table[cur_file->incl_index];
    } else {
      *fname = NULL;
      *dirname = NULL;
    }
  }
}

const char*
Get_Local_File_Full_Path(INT filenum, char* buf, INT buf_len)
{
  if (!file_info_ctx->_initialized) {
    Gen_File_Table();
  }

  if (filenum <= 0 || filenum >= file_info_ctx->_file_info_count)
    return NULL;

  struct file_info *file_table = file_info_ctx->_file_table;
  char **incl_table = file_info_ctx->_incl_table;
  file_info *fi = &file_table[filenum];
  const char* dir_name = incl_table[fi->incl_index];
  char sep = dir_name[1] == ':' && dir_name[2] == '\\' ? '\\' : '/';
  size_t ret = snprintf(buf, buf_len, "%s%c%s", dir_name, sep, fi->filename);
  if (ret < buf_len)
    return buf;
  else
    return NULL;
}

const char*
Get_Local_File_Name(INT filenum, char* buf, INT buf_len)
{
  if (!file_info_ctx->_initialized) {
    Gen_File_Table();
  }

  if (filenum <= 0 || filenum >= file_info_ctx->_file_info_count)
    return NULL;

  struct file_info *file_table = file_info_ctx->_file_table;
  file_info *fi = &file_table[filenum];
  size_t ret = snprintf(buf, buf_len, "%s", fi->filename);
  if (ret < buf_len)
    return buf;
  else
    return NULL;
}

INT
Get_Local_File_Count()
{
  if (!file_info_ctx->_initialized) {
    Gen_File_Table();
  }
  return file_info_ctx->_file_info_count;
}

INT
Get_Global_File_Number(INT file_index, INT filenum)
{
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
  return mgr != NULL ? mgr->Get_file(file_index).Get_global_file_num(filenum) :
                       filenum;
}

const char*
Get_Global_File_Full_Path(INT id, char* buf, INT buf_len)
{
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
  if (mgr != NULL) {
    const char* fname = mgr->Get_file_name(id);
    if (fname != NULL && buf != NULL)
      strncpy(buf, fname, buf_len);
    return buf;
  }
  else {
    return Get_Local_File_Full_Path(id, buf, buf_len);
  }
}

const char*
Get_Global_File_Name(INT id, char* buf, INT buf_len)
{
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
  if (mgr != NULL) {
    const char* fname = mgr->Get_file_name(id);
    Is_True_Ret(fname, ("can't find file name"), NULL);
    const char* tmp = strrchr(fname, '/');  // try '/' for Linux/Mac
    if (tmp == NULL)
      tmp = strrchr(fname, '\\');           // try '\\' for Windows
    if (tmp == NULL)  // not found, set to fname
      tmp = fname;
    else
      ++ tmp;         // skip '/' or '\\'
    if (tmp != NULL && buf != NULL)
      strncpy(buf, tmp, buf_len);
    return buf;
  }
  else {
    return Get_Local_File_Name(id, buf, buf_len);
  }
}

INT
Get_Global_File_Count()
{
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
  return mgr != NULL ? mgr->Get_file_table_size() :
                       Get_Local_File_Count();
}

void
Get_Srcpos_Filename(SRCPOS spos, const char **fname, const char **dirname)
{
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
  if (mgr == NULL) {
    Get_Local_Srcpos_Filename(spos, fname, dirname);
  }
  else {
    UINT filenum = SRCPOS_filenum(spos);
    const char* ptr = mgr->Get_file_name(filenum);
    *dirname = ptr;
    while (*ptr != '\0') {
      if ((*ptr == '/') || (*ptr == '\\'))
        *fname = ptr + 1;
      ++ptr;
    }
  }
}

void
Print_Local_File_Name(INT filenum, FILE* f)
{
  if (!file_info_ctx->_initialized) {
    fprintf(f, "not initialized\n");
    return;
  }

  if (filenum <= 0 || filenum >= file_info_ctx->_file_info_count) {
    fprintf(f, "filenum %d out of bound %d\n",
               filenum, file_info_ctx->_file_info_count);
    return;
  }

  struct file_info *file_table = file_info_ctx->_file_table;
  file_info *fi = &file_table[filenum];
  fprintf(f, "%s\n", fi->filename);
}

void
Print_Global_File_Name(INT file_index, INT filenum, FILE *f)
{
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
  if (mgr == NULL) {
    fprintf(f, "not xfa, call Print_Local_File_Name instead\n");
    return;
  }
  INT id =  mgr->Get_file(file_index).Get_global_file_num(filenum);
  fprintf(f, "%s\n", mgr->Get_file_name(id));
}


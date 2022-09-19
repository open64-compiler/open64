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
 * Module: gdar.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/gdar.cxx,v $
 *
 * Revision history:
 *  09-Jun-97 - Original Version
 *
 * Description:
 *
 * This file contains the GDAR routines.  The GDAR facility is an
 * SGI-internal capability for experimenting with the marking of
 * "Global Data And Routines".  :-)
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _NEW_SYMTAB
// TODO_NEW_SYMTAB: figure out what needs to be done
#else

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <search.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <elf.h>
#include <sys/elf_whirl.h>

#include "defs.h"
#include "glob.h"
#include "stab.h"
#include "pu_info.h"
#include "file_util.h"
#include "gdar.h"


/*  Global Defines  */

#define GLOBAL 0
#define STATIC 1

#define FUNCTION 0
#define DATA     1

#define DELETE   -9
#define NOCHANGE  0
#define PREEMPT   1
#define HIDDEN    2
#define INTERNAL  3
#define GPREL     4

#define NOOPTION 0
#define TRACE    1
#define LIST     2

#define OFF 0
#define ON  1

typedef enum {
  tok_GLOBAL,
  tok_STATIC,
  tok_FUNCTION,
  tok_DATA,
  tok_DEFAULT,
  tok_DELETE,
  tok_NOCHANGE,
  tok_PREEMPT,
  tok_HIDDEN,
  tok_INTERNAL,
  tok_GPREL,
  tok_OPTIONS,
  tok_TRACE,
  tok_LIST,
  tok_OFF,
  tok_ON,
  tok_name,
  tok_eof
} TOKENS;


/*  Global Data  */

static FILE *gdar_file;

static char  *file_name;
static INT32  file_name_len;

static char  line_buf[256];
static char *line_ptr;

static INT32 file_size;
static INT32 string_size;
static INT32 hash_size;

static struct stat stat_buf;

static INT32 global_flag   = GLOBAL;
static INT32 function_flag = FUNCTION;
static INT32 type_flag     = NOCHANGE;
static INT32 option_flag   = NOOPTION;
static INT32 trace_flag    = OFF;

static INT32 global_func_default = NOCHANGE;
static INT32 global_data_default = NOCHANGE;
static INT32 static_func_default = NOCHANGE;

static char *string_table;
static char *string_ptr;
static char *token_ptr;


/*  Token processing routine  */

static INT32 get_token(void) {

  char  *token_start;
  INT32  token_size;
  BOOL   char_found;
  BOOL   colon_seen;

  token_scan:

  char_found = FALSE;
  colon_seen = FALSE;

  while (!char_found) {
    if (*line_ptr == 0) {
      if (fgets (&line_buf[0], 256, gdar_file) == NULL)
	return tok_eof;
      line_ptr = &line_buf[0];
    } else if ((*line_ptr == ' ') || (*line_ptr == '\t') || (*line_ptr == '\n'))
      line_ptr++;
    else if (*line_ptr == '!')
      *line_ptr = 0;
    else
      char_found = TRUE;
  }

  token_start = line_ptr++;
  while ((*line_ptr != 0) && (*line_ptr != ' ') && (*line_ptr != '\t') &&
	 (*line_ptr != '!') && (*line_ptr != '\n')) {
    if (*line_ptr == ':')
      colon_seen = TRUE;
    line_ptr++;
  }
  token_size = line_ptr - token_start;

  if (trace_flag == ON)
    printf ("-- GDAR token: %.*s\n", token_size, token_start);

  switch (token_size) {

    case 2:
      if (strncmp (token_start, "ON", 2) == 0)
	return tok_ON;
      break;

    case 3:
      if (strncmp (token_start, "OFF", 3) == 0)
	return tok_OFF;
      break;

    case 4:
      if (strncmp (token_start, "DATA", 4) == 0)
	return tok_DATA;
      else if (strncmp (token_start, "LIST", 4) == 0)
	return tok_LIST;
      break;

    case 5:
      if (strncmp (token_start, "GPREL", 5) == 0)
	return tok_GPREL;
      else if (strncmp (token_start, "TRACE", 5) == 0)
	return tok_TRACE;
      break;

    case 6:
      if (strncmp (token_start, "DELETE", 6) == 0)
	return tok_DELETE;
      else if (strncmp (token_start, "GLOBAL", 6) == 0)
	return tok_GLOBAL;
      else if (strncmp (token_start, "HIDDEN", 6) == 0)
	return tok_HIDDEN;
      else if (strncmp (token_start, "STATIC", 6) == 0)
	return tok_STATIC;
      break;

    case 7:
      if (strncmp (token_start, "DEFAULT", 7) == 0)
	return tok_DEFAULT;
      else if (strncmp (token_start, "OPTIONS", 7) == 0)
	return tok_OPTIONS;
      else if (strncmp (token_start, "PREEMPT", 7) == 0)
	return tok_PREEMPT;
      break;

    case 8:
      if (strncmp (token_start, "FUNCTION", 8) == 0)
	return tok_FUNCTION;
      else if (strncmp (token_start, "INTERNAL", 8) == 0)
	return tok_INTERNAL;
      else if (strncmp (token_start, "NOCHANGE", 8) == 0)
	return tok_NOCHANGE;
      break;
  }

  if (token_size && (*token_start == '"')) {
    token_start++;
    token_size--;
  }
  if (token_size && (*(line_ptr - 1) == '"')) {
    token_size--;
  }
  if (token_size == 0)
    goto token_scan;

  if (colon_seen) {
    if ((token_size > file_name_len) &&
	(strncmp (token_start, file_name, file_name_len) == 0) &&
	(*(token_start + file_name_len) == ':')) {
      token_start += file_name_len + 1;
      token_size -= file_name_len + 1;
      if (token_size == 0)
	goto token_scan;
    } else
      goto token_scan;
  }

  token_ptr = string_ptr++;
  if (global_flag == GLOBAL)
    if (function_flag == FUNCTION)
      *token_ptr = 'F';
    else
      *token_ptr = 'D';
  else
    if (function_flag == FUNCTION)
      *token_ptr = 'f';
    else
      *token_ptr = 'd';
  (void) strncpy (string_ptr, token_start, token_size);
  string_ptr += token_size;
  *(string_ptr++) = 0;

  return tok_name;

}



/*  Main routine  */

void Process_GDAR (char *gdar_filename, SYMTAB *global_symtab,
		   struct pu_info **global_pu)
{
  ST              *st;
  char             symbol[256];
  ENTRY            item;
  ENTRY           *found_item;
  struct pu_info **prev_pu_ptr;
  struct pu_info  *curr_pu;
  struct pu_info  *next_pu;
  BOOL             delete_pu;

  /*  Sanity checks  */

  if ((gdar_filename == NULL) || (global_symtab == NULL))
    return;


  /*  Initialization  */

  file_name = Remove_Extension (Last_Pathname_Component (Irb_File_Name));
  file_name_len = strlen(file_name);

  if (stat (gdar_filename, &stat_buf) != 0) {
    return;
  }
  file_size = stat_buf.st_size;

  if ((gdar_file = fopen (gdar_filename, "r")) == NULL) {
    return;
  }
  line_buf[0] = 0;
  line_ptr = &line_buf[0];

  hash_size = file_size / 6;
  if (hcreate (hash_size) == 0) {
    fclose (gdar_file);
    return;
  }

  string_size = file_size;
  if ((string_ptr = string_table = (char *) malloc(string_size)) == NULL) {
    (void) hdestroy ();
    fclose (gdar_file);
    return;
  }


  /*  Read and process GDAR file  */

  for (;;) {

    switch (get_token()) {

      case tok_GLOBAL:
	global_flag = GLOBAL;
	break;

      case tok_STATIC:
	global_flag = STATIC;
	break;

      case tok_FUNCTION:
	function_flag = FUNCTION;
	break;

      case tok_DATA:
	function_flag = DATA;
	break;

      case tok_DEFAULT:
	if (global_flag == GLOBAL)
	  if (function_flag == FUNCTION)
	    global_func_default = type_flag;
	  else
	    global_data_default = type_flag;
	else
	  if (function_flag == FUNCTION)
	    static_func_default = type_flag;
	break;

      case tok_DELETE:
	type_flag = DELETE;
	break;

      case tok_NOCHANGE:
	type_flag = NOCHANGE;
	break;

      case tok_PREEMPT:
	type_flag = PREEMPT;
	break;

      case tok_HIDDEN:
	type_flag = HIDDEN;
	break;

      case tok_INTERNAL:
	type_flag = INTERNAL;
	break;

      case tok_GPREL:
	type_flag = GPREL;
	break;

      case tok_OPTIONS:
	option_flag = NOOPTION;
	break;

      case tok_TRACE:
	option_flag = TRACE;
	break;

      case tok_LIST:
	option_flag = LIST;
	break;

      case tok_OFF:
	if (option_flag == TRACE)
	  trace_flag = OFF;
	break;

      case tok_ON:
	if (option_flag == TRACE)
	  trace_flag = ON;
	break;

      case tok_name:
	item.key = token_ptr;
	item.data = (void *) type_flag;
	(void) hsearch (item, ENTER);
	break;

      case tok_eof:
	goto done;

    }

  }

  done:

  fclose (gdar_file);


  /*  Process all symbols in the global symbol table  */

  for (st = SYMTAB_symbols(global_symtab); st != NULL; st = ST_next(st)) {

    if (trace_flag == ON)
      printf ("-- GDAR processing %s\n", ST_name(st));

    if ((ST_symclass(st) == CLASS_FUNC) && (ST_export(st) != EXPORT_LOCAL) &&
	((ST_sclass(st) == SCLASS_EXTERN) || (ST_sclass(st) == SCLASS_TEXT))) {
      symbol[0] = 'F';
      (void) strcpy (&symbol[1], ST_name(st));
      item.key = &symbol[0];
      found_item = hsearch (item, FIND);
      if (found_item) {
	if ((INT32) found_item->data == INTERNAL) {
	  Set_ST_export (st, EXPORT_INTERNAL);
	} else if ((INT32) found_item->data == HIDDEN) {
	  Set_ST_export (st, EXPORT_HIDDEN);
	} else if ((INT32) found_item->data == PREEMPT) {
	  Set_ST_export (st, EXPORT_PREEMPTIBLE);
	}
      } else {
	if (global_func_default == INTERNAL) {
	  Set_ST_export (st, EXPORT_INTERNAL);
	} else if (global_func_default == HIDDEN) {
	  Set_ST_export (st, EXPORT_HIDDEN);
	} else if (global_func_default == PREEMPT) {
	  Set_ST_export (st, EXPORT_PREEMPTIBLE);
	}
      }
    } else if ((ST_symclass(st) == CLASS_VAR) &&
	       (ST_export(st) != EXPORT_LOCAL) &&
	       ((ST_sclass(st) == SCLASS_EXTERN) ||
		(ST_sclass(st) == SCLASS_DGLOBAL) ||
		(ST_sclass(st) == SCLASS_UGLOBAL))) {
      symbol[0] = 'D';
      (void) strcpy (&symbol[1], ST_name(st));
      item.key = &symbol[0];
      found_item = hsearch (item, FIND);
      if (found_item) {
	if ((INT32) found_item->data == GPREL) {
	  Set_ST_gprel (st);
	  Set_ST_export (st, EXPORT_INTERNAL);
	} else if ((INT32) found_item->data == INTERNAL) {
	  Set_ST_export (st, EXPORT_INTERNAL);
	} else if ((INT32) found_item->data == HIDDEN) {
	  Set_ST_export (st, EXPORT_HIDDEN);
	} else if ((INT32) found_item->data == PREEMPT) {
	  Set_ST_export (st, EXPORT_PREEMPTIBLE);
	}
      } else {
	if (global_data_default == GPREL) {
	  Set_ST_gprel (st);
	  Set_ST_export (st, EXPORT_INTERNAL);
	} else if (global_data_default == INTERNAL) {
	  Set_ST_export (st, EXPORT_INTERNAL);
	} else if (global_data_default == HIDDEN) {
	  Set_ST_export (st, EXPORT_HIDDEN);
	} else if (global_data_default == PREEMPT) {
	  Set_ST_export (st, EXPORT_PREEMPTIBLE);
	}
      }
    } else if ((ST_symclass(st) == CLASS_FUNC) &&
	       (ST_export(st) == EXPORT_LOCAL) &&
	       (ST_sclass(st) == SCLASS_TEXT)) {
      symbol[0] = 'f';
      (void) strcpy (&symbol[1], ST_name(st));
      item.key = &symbol[0];
      found_item = hsearch (item, FIND);
      if (found_item) {
	if ((INT32) found_item->data == INTERNAL) {
	  Set_ST_pu_no_gp_prolog (st);
	} else if ((INT32) found_item->data == HIDDEN) {
	  Reset_ST_pu_no_gp_prolog (st);
	}
      } else {
	if (static_func_default == INTERNAL) {
	  Set_ST_pu_no_gp_prolog (st);
	} else if (static_func_default == HIDDEN) {
	  Reset_ST_pu_no_gp_prolog (st);
	}
      }
    } else if ((ST_symclass(st) == CLASS_VAR) &&
	       (ST_export(st) == EXPORT_LOCAL) &&
	       ((ST_sclass(st) == SCLASS_FSTATIC) ||
		(ST_sclass(st) == SCLASS_PSTATIC))) {
      symbol[0] = 'd';
      (void) strcpy (&symbol[1], ST_name(st));
      item.key = &symbol[0];
      found_item = hsearch (item, FIND);
      if (found_item) {
	if ((INT32) found_item->data == GPREL) {
	  Set_ST_gprel (st);
	}
       }
    }

  }


  /*  Process all pu's in the file  */

  if (*global_pu) {

    prev_pu_ptr = global_pu;
    curr_pu = *global_pu;
    next_pu = curr_pu->next;

    while (curr_pu) {

      st = PU_Info_proc_sym(curr_pu);
      delete_pu = FALSE;

      if (trace_flag == ON)
	printf ("-- GDAR pu %s\n", ST_name(st));

      if ((ST_symclass(st) == CLASS_FUNC) &&
	  (ST_export(st) != EXPORT_LOCAL) &&
	  (ST_sclass(st) == SCLASS_TEXT)) {
	symbol[0] = 'F';
	(void) strcpy (&symbol[1], ST_name(st));
	item.key = &symbol[0];
	found_item = hsearch (item, FIND);
	if (found_item) {
	  if ((INT32) found_item->data == DELETE)
	    delete_pu = TRUE;
	} else {
	  if (global_func_default == DELETE)
	    delete_pu = TRUE;
	}
      } else if ((ST_symclass(st) == CLASS_FUNC) &&
		 (ST_export(st) == EXPORT_LOCAL) &&
		 (ST_sclass(st) == SCLASS_TEXT)) {
	symbol[0] = 'f';
	(void) strcpy (&symbol[1], ST_name(st));
	item.key = &symbol[0];
	found_item = hsearch (item, FIND);
	if (found_item) {
	  if ((INT32) found_item->data == DELETE)
	    delete_pu = TRUE;
	} else {
	  if (static_func_default == DELETE)
	    delete_pu = TRUE;
	}
      }

      if (delete_pu) {
	Set_ST_export (st, EXPORT_INTERNAL);
	Set_ST_sclass (st, SCLASS_EXTERN);
	*prev_pu_ptr = next_pu;
      } else
	prev_pu_ptr = &(curr_pu->next);

      curr_pu = next_pu;
      if (curr_pu)
	next_pu = curr_pu->next;

    }

  }


  /*  Termination  */

  (void) hdestroy ();
  free (string_table);

  return;

}
#endif /* _NEW_SYMTAB */

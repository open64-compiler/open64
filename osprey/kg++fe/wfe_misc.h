/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
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


/* translate gnu decl trees to whirl */

#ifndef wfe_util_INCLUDED
#define wfe_util_INCLUDED

/* add some typedefs to avoid including C++ specifi items */
#ifndef __cplusplus
typedef int INT;
typedef signed int INT32;
typedef unsigned long long UINT64;
typedef int BOOL;
typedef struct wn WN;
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern int  WFE_Keep_Zero_Length_Structs;

extern void WFE_Init (INT argc, char **arrgv, char **envp);
extern void WFE_Finish (void);
extern void WFE_File_Init (INT argc, char **argv);
extern void WFE_File_Finish (void);
extern void WFE_Check_Errors (int *error_count, int *warning_count, BOOL *need_inliner);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

#include "wn.h"
#include "srcpos.h"

typedef enum {
  wfe_stmk_unknown,
  wfe_stmk_func_entry,
  wfe_stmk_func_pragmas,
  wfe_stmk_func_body,
  wfe_stmk_region_pragmas,
#ifdef KEY
  wfe_stmk_region_body,
  wfe_stmk_region_exits,
  wfe_stmk_call_region_pragmas,	// not used currently
  wfe_stmk_call_region_body,
  wfe_stmk_call_region_exits,	// not used currently
#endif // KEY
  wfe_stmk_scope,
  wfe_stmk_if_cond,
  wfe_stmk_if_then,
  wfe_stmk_if_else,
  wfe_stmk_while_cond,
  wfe_stmk_while_body,
  wfe_stmk_dowhile_cond,
  wfe_stmk_dowhile_body,
  wfe_stmk_for_cond,
  wfe_stmk_for_body,
  wfe_stmk_switch,
  wfe_stmk_comma,
  wfe_stmk_rcomma,
  wfe_stmk_temp_cleanup,
#ifdef KEY
  wfe_stmk_dummy,	// does not generate code
#endif // KEY
  wfe_stmk_last
} WFE_STMT_KIND;

extern void WFE_Stmt_Push (WN* wn, WFE_STMT_KIND kind, SRCPOS srcpos);
extern WN*  WFE_Stmt_Top (void);
extern void WFE_Stmt_Append (WN* wn, SRCPOS srcpos);
extern WN*  WFE_Stmt_Last (void);
extern WN*  WFE_Stmt_Pop (WFE_STMT_KIND kind);

#ifdef KEY
extern void WFE_Stmt_Prepend_Last (WN* wn, SRCPOS srcpos);
#endif


#ifdef KEY
extern void WFE_Guard_Var_Push (void);
extern tree WFE_Guard_Var_Pop (void);
extern tree WFE_Get_Guard_Var (void);
extern bool Check_For_Call_Region (void);
extern bool Did_Not_Terminate_Region;
extern WN * WFE_Find_Stmt_In_Stack (WFE_STMT_KIND);
#endif

extern UINT current_file;
extern int lineno;

extern int wfe_invoke_inliner;

// get the srcpos info from the global variable lineno
inline SRCPOS
Get_Srcpos (void)
{
  SRCPOS s;
  SRCPOS_clear(s);
  SRCPOS_filenum(s) = current_file;
  SRCPOS_linenum(s) = lineno;
  return s;
}

#endif

#endif

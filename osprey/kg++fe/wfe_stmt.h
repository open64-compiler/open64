/* 
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 *
 * File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
 * front-ends to GNU 3.2.2 release.
 */

/* WFE == WHIRL Front End */
/* translate gnu stmt trees to whirl */

#ifndef wn_stmt_INCLUDED
#define wn_stmt_INCLUDED

extern void WFE_Stmt_Init (void);
extern LABEL_IDX WFE_Get_LABEL (tree label, int def);
extern void WFE_Check_Undefined_Labels(void);
extern void Mark_Scopes_And_Labels (tree);
#ifndef KEY
extern void Push_Temp_Cleanup (tree, bool);
#endif
extern void Do_Temp_Cleanups (tree);
extern bool Is_Cleanup_Only (void);
extern void Do_Handlers (void);
extern void Do_EH_Cleanups (void);
extern void Call_Throw();
extern void WFE_Expand_If (tree);
#ifdef KEY
extern void Push_Temp_Cleanup (tree, bool, bool=0);
extern void Do_EH_Tables (void);
extern void Call_Terminate();
extern LABEL_IDX lookup_cleanups(INITV_IDX&);
extern bool WFE_has_copy_constructor (tree type);
extern bool Set_Current_Scope_Has_Alloca (INT &);
extern void Set_Current_Scope_Alloca_St (ST *, int);
extern void Add_Current_Scope_Alloca_St (ST *, int);
extern ST_IDX exc_ptr;
extern int make_symbols_weak;
extern bool try_block_seen;
extern bool in_cleanup;
extern void Push_Top_Level_Scope (tree);
extern void Pop_Top_Level_Scope (void);
#endif // KEY
extern INT Current_Handler_Count();
extern void Add_Handler_Info (WN * call_wn, INT i, INT num_handlers);

extern
#ifdef __cplusplus
"C"
#endif
void WFE_Expand_Stmt (tree stmt, WN* target_wn = NULL);

#ifdef KEY
extern LABEL_IDX WFE_unusable_label_idx;
extern LABEL_IDX WFE_last_label_idx;
#endif

#endif

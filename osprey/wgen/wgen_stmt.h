/*
 * Copyright (C) 2007 PathScale, LLC.  All Rights Reserved.
 */
/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

////////////////////////////////////////////////////////////////////////////////
//
// Copyright 2006 PathScale, Inc. All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
//
// Further, this software is distributed without any warranty that it is
// free of the rightful claim of any third person regarding infringement 
// or the like.  Any license provided herein, whether implied or 
// otherwise, applies only to this software file.  Patent licenses, if 
// any, provided herein do not apply to combinations of this program with 
// other software, or any other product whatsoever.  
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write the Free Software Foundation, Inc., 59
// Temple Place - Suite 330, Boston MA 02111-1307, USA.
////////////////////////////////////////////////////////////////////////////////

/* WGEN == WHIRL Front End */
/* translate gnu stmt trees to whirl */

#ifndef wgen_stmt_INCLUDED
#define wgen_stmt_INCLUDED

extern void WGEN_Stmt_Init (void);

extern LABEL_IDX WGEN_Get_LABEL (gs_t label, int def);
extern void WGEN_Check_Undefined_Labels(void);
extern void Mark_Scopes_And_Labels (gs_t);
#ifndef KEY
extern void Push_Temp_Cleanup (gs_t, bool);
#endif
extern void Do_Temp_Cleanups (gs_t);
extern void Do_Handlers (INT =0);
extern void Call_Throw();
#ifdef KEY
extern void Push_Temp_Cleanup (gs_t, bool, bool=0);
extern void Do_EH_Tables (void);
extern void Call_Terminate();
extern LABEL_IDX lookup_cleanups(INITV_IDX&);
extern bool WGEN_has_copy_constructor (gs_t type);
extern bool Set_Current_Scope_Has_Alloca (INT &);
extern void Set_Current_Scope_Alloca_St (ST *, int);
extern void Add_Current_Scope_Alloca_St (ST *, int);
extern ST_IDX exc_ptr;
extern int make_symbols_weak;
extern bool try_block_seen;
extern bool in_cleanup;
extern void WGEN_Expand_Pragma (gs_t);
extern void Register_Cleanup (gs_t);
extern void Unregister_Cleanup (void);
extern ST_IDX Get_exception_pointer_symbol (void);
extern ST_IDX Get_exception_filter_symbol (void);
#ifdef FE_GNU_4_2_0
extern void WGEN_maybe_do_eh_cleanups (void);
#endif
#endif // KEY
extern INT Current_Handler_Count();
extern void Add_Handler_Info (WN * call_wn, INT i, INT num_handlers);

extern
#ifdef __cplusplus
"C"
#endif
void WGEN_Expand_Stmt (gs_t stmt, WN* target_wn = NULL);

extern void WGEN_Expand_Label (gs_t label);

#ifdef KEY
extern LABEL_IDX WGEN_unusable_label_idx;
extern LABEL_IDX WGEN_last_label_idx;
#endif

#endif

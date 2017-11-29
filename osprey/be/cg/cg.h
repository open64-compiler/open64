/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


#ifndef	cg_INCLUDED
#define	cg_INCLUDED



#include "opt_alias_interface.h"
#include "region_util.h"

#include "wn.h"
#include "dwarf_DST_mem.h"
#include "symtab.h"
#if defined(TARG_SL) || defined(TARG_MIPS)
#include "register.h"
#endif

/* type stubs - can't include appropriate files since
 * this file is included by be/com/driver.c ...
 */
struct bb;

extern BOOL PU_Has_Calls;
extern BOOL PU_References_GP;
#ifdef TARG_IA64
extern BOOL GRA_optimize_restore_pr;
extern BOOL GRA_optimize_restore_b0_ar_pfs;
extern BOOL GRA_optimize_restore_ar_lc;
extern BOOL EBO_data_spec;
extern BOOL edge_done;
#endif
#ifdef KEY
extern BOOL PU_Has_Exc_Handler;
extern BOOL PU_Has_Nonlocal_Goto_Target;
extern BOOL CG_file_scope_asm_seen;
#endif

#ifdef TARG_X8664
struct tn;
extern BOOL PU_has_local_dynamic_tls;         // for local-dynamic tls
extern struct tn*  Local_Dynamic_TLS_Base;    // return value for __get_tls_addr
extern BOOL PU_References_GOT; // for -m32 -fpic
extern BOOL PU_has_avx128; // cause emit of vzeroupper
extern BOOL PU_has_builtin_apply_args; // __builtin_apply_args
extern BOOL PU_has_builtin_apply; // __builtin_apply
#endif

extern BOOL CG_PU_Has_Feedback;

#if defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS)
extern BOOL RGN_Formed;
#endif

#ifdef TARG_LOONGSON
extern BOOL edge_done;

typedef struct {
  char *name;
  struct bb *dominator;
}Info_Global_Name;

extern Info_Global_Name Global_Name_Executed[];
extern int iter_executed;
#endif

/* WOPT alias manager */
extern struct ALIAS_MANAGER *Alias_Manager;

#ifdef __cplusplus
extern "C" {
#endif

/* Generate code for the given REGION or PU
   The results are returned in a WHIRL statement-list
   attached to a BLOCK. */
extern WN *CG_Generate_Code (
  WN            *rwn,           /* The REGION to compile */
  struct ALIAS_MANAGER *am,	/* WOPT alias manager */
  DST_IDX	pu_dst,		/* dst_idx for pu, (NULL for region) */
  BOOL		region		/* processing PU or region */
);

extern void CG_PU_Initialize( WN *wn );
extern void CG_PU_Finalize( void );

extern void CG_Change_Elf_Symbol_To_Undefined (ST* st);

/* initialization for statics to keep the ST for locals generated
   when creating quad glue code for regions */
extern void Init_gen_quad_preg(void);

#ifdef __cplusplus
}
#endif

/* Print the IR for a program unit after a phase, if enabled: */
extern void Trace_IR (
  INT phase,		/* Phase after which to print */
  const char *pname,	/* Print name of phase */
#ifdef TARG_IA64
  struct bb *bb,	/* BB to print, or NULL */
  BOOL after = TRUE
#else
  struct bb *bb		/* BB to print, or NULL */
#endif
);

/* Print IR, ST, TNs for a program unit after a phase, if enabled: */
extern void Check_for_Dump (
  INT phase,	/* Phase after which to print */
  struct bb *bb	/* BB to print, or NULL */
);

/* Overloaded version of standard Get_Trace with additional bb
 * parameter. If bb parameter is non-NULL and BBs were explicitly 
 * enabled for tracing, return FALSE unless the given BB is in the 
 * enabled list.
 */
extern BOOL Get_Trace ( INT func, INT arg, struct bb *bb );

/* memory pools for allocations local to processing a region */
extern MEM_POOL MEM_local_region_pool;
extern MEM_POOL MEM_local_region_nz_pool;

extern RID *Current_Rid;

#if defined(TARG_SL) || defined(TARG_MIPS)
extern void CG_Dump_Cur_Region();
extern REGISTER_SET caller_saved_regs_used[ISA_REGISTER_CLASS_MAX+1];
extern void IPISR_Insert_Spills();
#endif
#if defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS)
/* indicate whether region is already been formed. */
extern BOOL RGN_Formed;
#endif

#endif /* cg_INCLUDED */

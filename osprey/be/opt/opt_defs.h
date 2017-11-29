/*
 * Copyright (C) 2008-2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_defs.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_defs.h,v $
//
// Revision history:
//  30-AUG-94 - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
//   Exported data types:
//
//     IDTYPE:mUINT32 - to define a unique identifier, eg id number in
//                      the basic block node, BB_NODE.
//
//     IDX_16:mINT16  - to define a variable for array subscript.
//
//     IDX_32:mINT32  - to define a variable for array subscript.
//
//     IDX_64:mINT64  - to define a variable for array subscript.
//
// ====================================================================
// ====================================================================


#ifndef opt_defs_INCLUDED
#define opt_defs_INCLUDED       "opt_defs.h"
#ifdef _KEEP_RCS_ID
static char *opt_defsrcs_id =	opt_defs_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

#ifndef defs_INCLUDED
#include "defs.h"
#endif
#ifndef mtypes_INCLUDED
#include "mtypes.h"
#endif  
#ifndef tracing_INCLUDED
#include "tracing.h"
#endif
#ifndef symtab_INCLUDED
#include "symtab.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef TYPE_ID MTYPE;

#define MTYPE_IS_INTEGER(ty) (MTYPE_type_class(ty) & MTYPE_CLASS_INTEGER)

/* Common definitions */
#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifndef BOOL
#define BOOL INT
#endif

#ifndef ILLEGAL_BP
#define ILLEGAL_BP (-1)
#endif

typedef mINT16  IDX_16;
typedef mINT32  IDX_32;
typedef mINT64  IDX_64;

typedef IDTYPE  AUX_ID;
typedef IDTYPE  VER_ID;
typedef IDTYPE  EXP_ID;

enum STMT_ID {
  INVALID_STMT_ID = -1
};

#ifdef Is_True_On
#define Is_Trace(Cond, Parmlist) { if (Cond) fprintf Parmlist ; }
#define Is_Trace_cmd(Cond, Cmd) { if (Cond) Cmd ; }
#else
#define Is_Trace(Cond, Parmlist) ((void) 1)
#define Is_Trace_cmd(Cond, Cmd)  ((void) 1)
#endif

#ifdef __cplusplus
// action to use in constructor to decide how the bitset should be
// initialized
#ifndef OPTS_ACTION_DEF
#define OPTS_ACTION_DEF
enum OPTS_ACTION {
  OPTS_FALSE,		// clear set when creating
  OPTS_TRUE,		// fill set when creating
  OPTS_DONT_CARE	// leave set uninitialized
};
#endif /* OPTS_ACTION_DEF */

static inline BOOL
Is_fortran(void)
{
  Is_True((PU_src_lang(Get_Current_PU()) &
	   (PU_src_lang(Get_Current_PU()) - 1)) == 0,
	  ("Mixed-language inlining not allowed; "
	   "need to know original PU language"));
  return (PU_src_lang(Get_Current_PU()) == PU_F77_LANG ||
	  PU_src_lang(Get_Current_PU()) == PU_F90_LANG);
}
#define IS_FORTRAN Is_fortran()

#endif /* __cplusplus */

/*
 * Tracing flags:  -ttPHASE-NUMBER:FLAG
 */

/* TP_ALIAS (phase-number) */
/* Flags for tracing the Nystrom alias analyzer */
#define NYSTROM_SOLVER_FLAG       0x00000001 /* trace constraint graph solver */
#define NYSTROM_CG_PRE_FLAG       0x00000002 /* text dump of constraint graph after build */
#define NYSTROM_CG_POST_FLAG      0x00000004 /* text dump of constraint graph after solve */
#define NYSTROM_CG_VCG_FLAG       0x00000008 /* dump vcg before/after solve */
#define NYSTROM_QUERY_TRACE_FLAG  0x00000010 /* emit trace of all "aliased()" queries */
#define NYSTROM_CG_OPT_FLAG       0x00000020 /* emit trace of optimized nodes */
#define NYSTROM_MEMORY_TRACE_FLAG 0x00000040 /* collect emit memory tracing */
#define NYSTROM_CG_BE_MAP_FLAG    0x00000080 /* CG clone and map updates 
                                                during IPA for BE */
#define NYSTROM_CG_BUILD_FLAG     0x00000100 /* CG build trace */
#define NYSTROM_SUMMARY_FLAG      0x00000200 /* IPA-BE summary trace */
#define NYSTROM_ALIAS_TAG_FLAG    0x00000400 /* be alias tag after 
                                                Transfer_alias_tag_to_occ_and_aux */
#define NYSTROM_LW_SOLVER_FLAG    0x00000800 /* light weight trace constraint graph solver */
#define NYSTROM_INLINE_FLAG       0x00001000 /* inline trace*/

/* TP_GLOBOPT (phase-number) */
#define DOM_DUMP_FLAG	     0x0001 /* print dominator tree */
#define SSA_DUMP_FLAG	     0x0002 /* trace construction of phi functions */
#define CFG_VERF_FLAG        0x0004 /* verify control flow graph */ 
#define CFG_DUMP_FLAG        0x0008 /* dump control flow graph */ 
#define DSE_DUMP_FLAG        0x0010 /* trace dead-store elim (preopt) */
#define PROP_DUMP_FLAG       0x0020 /* dump copy propagation's output */
#define FOLD_DUMP_FLAG	     0x0040 /* trace simple folding to constants */
#define DCE_DUMP_FLAG        0x0080 /* dump dead-code elim (on stmtrep) */
#define IVR_DUMP_FLAG        0x0100 /* dump/trace IV recognition */
#define DU_DUMP_FLAG         0x0200 /* dump/trace D-U chain construction */
#define EMIT_DUMP_FLAG       0x0400 /* dump after converting back into WHIRL*/
#define ALIAS_DUMP_FLAG      0x0800 /* trace alias analysis */
#define ITAB_DUMP_FLAG	     0x1000 /* itable dump flag */
#define DFEQN_DUMP_FLAG	     0x2000 /* data-flow equations dump flag */
#define CR_DUMP_FLAG         0x4000 /* dump CODEMAP, STMTREP and CODEREP */
#define DFLOC_DUMP_FLAG	     0x8000 /* data-flow equations dump flag */
#define RVI_TRACE_FLAG      0x10000 /* reg-variable identification trace  */
#define MAIN_EMIT_DUMP_FLAG 0x20000 /* main-emitter dump flag */
#define INDUCTION_DUMP_FLAG 0x40000 /* mainopt find induction trace flag */
#define OPT_LOWER_FLAG      0x80000 /* dump lowered tree in pre/main opt*/
#define LFTR_FLAG          0x100000 /* linear function test replacement */
#define IVE_TRACE_FLAG     0x200000 /* induction var elim trace */
#define LPRE_DUMP_FLAG     0x400000 /* trace SSA-LPRE */
#define LDX_DUMP_FLAG      0x800000 /* Index load optimization */
#define ALIAS_TRACE_FLAG  0x1000000 /* trace alias analysis for CG */
#define EXC_TRACE_FLAG    0x2000000 /* trace C++ exception handling */
#define TAIL_TRACE_FLAG   0x4000000 /* trace tail recursion */
#define EPRE_DUMP_FLAG    0x8000000 /* trace SSA-EPRE */
#define SPRE_DUMP_FLAG   0x10000000 /* trace SSA-SPRE */
#define MEM_DUMP_FLAG    0x20000000 /* mem_pool trace */
#define STATISTICS_FLAG  0x40000000 /* trace statistics */
#define ENABLE_STAT      0x80000000 /* enable statistics */
/* TP_WOPT2 (phase-number) */
#define SECOND_RENAME_FLAG      0x2 /* second CODEMAP renaming */
#define BOOL_SIMP_FLAG          0x4 /* boolean expr simplification */
#define FB_PRE_FLAG             0x8 /* trace feedback PRE */
#define CFG_OPT_FLAG           0x10 /* trace CFG optimzation */
#define WOVP_DUMP_FLAG         0x20 /* dump after write-once variable promotion*/
#define PT_SUMMARY_FLAG        0x40 /* trace the points-to summary */    
#define LOOP_MULTVER_FLAG      0x80 /* Loop multiversioning */ 
#define LOOP_NORM_FLAG   0x00000100 /* Trace Loop Normalization */
#define LCLSC_TRACE_FLAG 0x00000200 /* trace stack shrink optimization */
#define PRO_TRANS_TRACE_FLAG 0x00000400 /* trace proactive loop fusion transformation */
#define PRO_TRANS_DUMP_FLAG  0x00000800 /* dump proactive loop fusion transformation */
#define ULSE_TRACE_FLAG      0x00001000 /* trace the useless store remove transformation */
#define REASSO_DUMP_FLAG_DEBUG	     0x2000 /* reassociation before and after dump flag */
#define REASSO_DUMP_FLAG	     0x4000 /* trace into reassociation flag */
#define ZDL_DUMP_FLAG                0x8000 /* dump the zdl gen */


/* Flags associated with value numbering scheme and the 
 * WOPT_Enable_Value_Numbering option.
 */
enum VALUE_NUMBERING_FLAG
{VNFRE_OFF = 0,
 VNFRE_AFTER_EPRE = 1,            /* Default */
 VNFRE_BEFORE_AND_AFTER_EPRE = 2,
 VNFRE_SINGLE_PASS_AFTER_EPRE = 3,
 VNFRE_SINGLE_PASS_BEFORE_AND_AFTER_EPRE = 4,
 VNFRE_NO_OF_FLAGS = 5
};


/* Optimizer mem_pool Push and Pop */
#ifdef Is_True_On
#define OPT_POOL_Initialize(pool, str, bool, debug) \
  { if (Get_Trace(TP_GLOBOPT, MEM_DUMP_FLAG)) \
      fprintf(TFile,"0x%x, 0x%p, OPT_POOL_Initialize(%s)\n",debug,pool,str); \
    MEM_POOL_Initialize(pool, str, bool); \
  }
#define OPT_POOL_Alloc(pool, size, debug) \
  ((Get_Trace(TP_GLOBOPT, MEM_DUMP_FLAG)) ? \
    (fprintf(TFile,"0x%x, 0x%p, OPT_POOL_Alloc()\n",debug,pool), \
    MEM_POOL_Alloc(pool, size)) : MEM_POOL_Alloc(pool, size))
#define OPT_POOL_Realloc(pool, p1, p2, p3, debug) \
  ((Get_Trace(TP_GLOBOPT, MEM_DUMP_FLAG)) ? \
    (fprintf(TFile,"0x%x, 0x%p, OPT_POOL_Realloc()\n",debug,pool), \
    MEM_POOL_Realloc(pool, p1, p2, p3)) : MEM_POOL_Realloc(pool, p1, p2, p3))
#define TYPE_OPT_POOL_ALLOC(obj, pool, debug) \
  ((Get_Trace(TP_GLOBOPT, MEM_DUMP_FLAG)) ? \
    (fprintf(TFile,"0x%x, 0x%p, TYPE_OPT_POOL_ALLOC()\n",debug,pool), \
    TYPE_MEM_POOL_ALLOC(obj, pool)) : TYPE_MEM_POOL_ALLOC(obj, pool))
#define TYPE_OPT_POOL_ALLOC_N(n, pool, num, debug) \
  ((Get_Trace(TP_GLOBOPT, MEM_DUMP_FLAG)) ? \
    (fprintf(TFile,"0x%x, 0x%p, TYPE_OPT_POOL_ALLOC_N()\n",debug,pool), \
    TYPE_MEM_POOL_ALLOC_N(n, pool, num)) : TYPE_MEM_POOL_ALLOC_N(n, pool, num))
#define OPT_POOL_Delete(pool, debug) \
  { if (Get_Trace(TP_GLOBOPT, MEM_DUMP_FLAG)) \
      fprintf(TFile,"0x%x, 0x%p, OPT_POOL_Delete()\n",debug,pool); \
    MEM_POOL_Delete(pool); \
  }
#define OPT_POOL_FREE(pool, item, debug) \
  { if (Get_Trace(TP_GLOBOPT, MEM_DUMP_FLAG)) \
      fprintf(TFile,"0x%x, 0x%p, OPT_POOL_FREE()\n",debug,pool); \
    MEM_POOL_FREE(pool, item); \
  }
#define OPT_POOL_Push(pool, debug) \
  { if (Get_Trace(TP_GLOBOPT, MEM_DUMP_FLAG)) \
      fprintf(TFile,"0x%x, 0x%p, OPT_POOL_Push()\n",debug,pool); \
    MEM_POOL_Push(pool); \
  }
#define OPT_POOL_Pop(pool, debug) \
  { if (Get_Trace(TP_GLOBOPT, MEM_DUMP_FLAG)) \
      fprintf(TFile,"0x%x, 0x%p, OPT_POOL_Pop()\n",debug,pool); \
    MEM_POOL_Pop(pool); \
  }
#else
#define OPT_POOL_Initialize(pool, str, bool, debug) \
    MEM_POOL_Initialize(pool, str, bool);
#define OPT_POOL_Alloc(pool, size, debug) \
    MEM_POOL_Alloc(pool, size);
#define OPT_POOL_Realloc(pool, p1, p2, p3, debug) \
    MEM_POOL_Realloc(pool, p1, p2, p3);
#define TYPE_OPT_POOL_ALLOC(obj, pool, debug) \
    TYPE_MEM_POOL_ALLOC(obj, pool);
#define TYPE_OPT_POOL_ALLOC_N(n, pool, num, debug) \
    TYPE_MEM_POOL_ALLOC_N(n, pool, num);
#define OPT_POOL_Delete(pool, debug) \
    MEM_POOL_Delete(pool);
#define OPT_POOL_FREE(pool, item, debug) \
    MEM_POOL_FREE(pool, item);
#define OPT_POOL_Push(pool, debug) \
    MEM_POOL_Push(pool);
#define OPT_POOL_Pop(pool, debug) \
    MEM_POOL_Pop(pool);
#endif


#ifdef __cplusplus
}
#endif

#endif /* opt_defs_INCLUDED */

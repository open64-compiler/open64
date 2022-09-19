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


#ifndef pragmas_INCLUDED
#define pragmas_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: wn_pragmas.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/wn_pragmas.h,v $
 *
 * Description:
 *
 * This header file is for support of WHIRL's OPC_PRAGMA. In OPC_PRAGMA,
 * the offset field stores the enumerated pragma id; the ST pointer field,
 * if non-null, points to the symbol table field to give the symbol 
 * associated with the pragma. The constval field, which overlaps with
 * kid[0] and kid[1], can be used to store additional values, depending
 * on the pragma.
 *
 * Pragmas what affect only the front-ends are not represented in WHIRL.
 * Pragmas that deal with properties of symbol table objects also do not
 * need to be represented as WHIRL pragmas, and should be entered into the
 * symbol table as attributes for the objects.  Only pragmas associated with
 * program text need to be represented in WHIRL's OPC_PRAGMA. Some of these
 * pragmas can actually be encoded in WHIRL nodes (e.g. the flags in the
 * call instructions), and also do not need to be represented by OPC_PRAGMA.
 *
 * Pragmas in general are generated due to one of the following:
 * 1. #pragmas in the source program;
 * 2. c$ directives in fortran source code;
 * 3. cc/f77 command line options;
 * 4. compiler to pass information to later compilation phases.
 *
 * Pragmas have scope.  Since WHIRL is organized and compiled on a per-PU 
 * basis, the scope of WHIRL pragmas is at most the current PU.  The scope
 * of a WHIRL pragma can be one of the following:
 *
 * 1. PU scope: These pragmas must appear right after the OPC_BLOCK underneath
 * the OPC_FUNC_ENTRY.  This allows a component to learn about the pragmas
 * without scanning the entire PU.
 *
 * 2. WN node scope: These pragmas take advantage of the hierarchical nature of
 * structured control flow WHIRL nodes in defining its scope.  A pragma with
 * WN scope must be specified before a WHIRL statement node, and it only affects
 * that node.  If the node is an SCF node, it affects all the code underneath
 * it. More than one of such pragmas can appear before a statement node.
 * (The reason they must be specified before the WN node, and not within
 * it, is to allow phases to learn about the pragmas without having to look
 * inside the node.)
 *
 * 3. Point scope: These pragmas do not really have scope.  They are for
 * placing a mark at a specific point of the code.  They are not associated
 * with any other WHIRL nodes.
 *
 * 4. Enclosed scope: For these pragmas, the scope begins at the "on" pragma,
 * and ends at the "off" pragma.  The matching on/off pragmas must belong
 * to the same OPC_BLOCK. These pragmas can have either statement or
 * basic block granularity.  For basic block granularity, the scope begins
 * at the basic block containing the "on" pragma, and ends at the basic block
 * containing the "off" pragma.  The same on/off pragmas can be perfectly 
 * nested one inside another.
 *
 * 5. Pragma-specific scope: These pragmas have unique scope rules according
 * to the definition of the pragmas.
 *
 * Note that because we require the OPC_PRAGMAs to be positioned at specific
 * places, this may require the front-ends to do extra work in processing
 * pragmas in the source code and generating the OPC_PRAGMAs at the right
 * place.  But we feel this simplifies the backends' processing of pragmas
 * by making it unnecessary for them to search for the pragmas.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *pragmas_rcs_id = "$Source: /home/bos/bk/kpro64-pending/common/com/SCCS/s.wn_pragmas.h $ $Revision: 1.8 $";
#endif /* _KEEP_RCS_ID */

/* Pragma IDs: */
 /* make sure you update WN_pragmas[] table in wn_pragmas.c */
typedef enum {
  WN_PRAGMA_UNDEFINED,		/* pragma 0 is undefined to make sure	*/
				/* the front-ends send a valid pragma	*/

  WN_PRAGMA_INLINE_BODY_START,	/* mark start of an inlined function body */
  WN_PRAGMA_INLINE_BODY_END,	/* mark end of an inlined function body	*/
  WN_PRAGMA_INLINE_DEPTH,
  WN_PRAGMA_INLINE_LOOPLEVEL,   /* TODO: remove at next WHIRL change */

  WN_PRAGMA_AGGRESSIVE_INNER_LOOP_FISSION,
  WN_PRAGMA_FISSION,		/* fission the surrounding l loops here	*/
  WN_PRAGMA_FISSIONABLE,	/* fission the surrounding l loops here	*/
  WN_PRAGMA_FUSE,		/* fuse the next n loops for l levels	*/
  WN_PRAGMA_FUSEABLE,		/* fuse the next n loops for l levels	*/
  WN_PRAGMA_NO_FISSION,		/* do not fission the next n loops	*/
  WN_PRAGMA_NO_FUSION,		/* do not fuse the next n loops		*/
  WN_PRAGMA_INTERCHANGE,	/* interchange the surrounding loops
				 * based on the loop indices specified.
				 */

  WN_PRAGMA_NO_INTERCHANGE,	/* do not interchange loops		*/
  WN_PRAGMA_BLOCKING_SIZE,	/* specify sizes for blocking		*/
  WN_PRAGMA_NO_BLOCKING,	/* do not block loop			*/
  WN_PRAGMA_UNROLL,		/* unroll loop n times			*/
  WN_PRAGMA_BLOCKABLE,		/* block loops as specified by indices	*/
  WN_PRAGMA_PREFETCH,		/* specify prefetch for each cache level*/
  WN_PRAGMA_PREFETCH_MANUAL,	/* specify handling of manual prefetches*/
  WN_PRAGMA_PREFETCH_REF,	/* generate prefetch node for array ref */
  WN_PRAGMA_PREFETCH_REF_DISABLE,
				/* disable specified array prefetches   */
  WN_PRAGMA_IVDEP,		/* force mem ref indep			*/

  WN_PRAGMA_OPTIONS,		/* specifies compilation options	*/
  WN_PRAGMA_OPAQUE,		/* cannot move code across region boundary */

  WN_PRAGMA_MIPS_FREQUENCY_HINT,/* provide hints regarding execution	*/
  WN_PRAGMA_DISTRIBUTE,
  WN_PRAGMA_REDISTRIBUTE,
  WN_PRAGMA_DISTRIBUTE_RESHAPE,
  WN_PRAGMA_DYNAMIC,

  WN_PRAGMA_ACCESSED_ID,	/* an assumed access to a variable   */

  /* mpc pragmas */


  WN_PRAGMA_PFOR_BEGIN,
  WN_PRAGMA_ENTER_GATE,
  WN_PRAGMA_EXIT_GATE,

  /* fortran 77 mp pragmas */

  WN_PRAGMA_BARRIER,
  WN_PRAGMA_CHUNKSIZE,
  WN_PRAGMA_COPYIN,
  WN_PRAGMA_CRITICAL_SECTION_BEGIN,
  WN_PRAGMA_CRITICAL_SECTION_END,
  WN_PRAGMA_DOACROSS,
  WN_PRAGMA_IF,
  WN_PRAGMA_LASTLOCAL,
  WN_PRAGMA_LOCAL,
  WN_PRAGMA_MPSCHEDTYPE,
  WN_PRAGMA_ORDERED,
  WN_PRAGMA_PARALLEL_BEGIN,
  WN_PRAGMA_PARALLEL_END,	/* TODO: remove at next WHIRL change */
  WN_PRAGMA_PARALLEL_DO,
  WN_PRAGMA_PDO_BEGIN,
  WN_PRAGMA_PDO_END,            /* TODO: remove at next WHIRL change */
  WN_PRAGMA_PSECTION_BEGIN,
  WN_PRAGMA_PSECTION_END,       /* TODO: remove at next WHIRL change */
  WN_PRAGMA_REDUCTION,
  WN_PRAGMA_SECTION,
  WN_PRAGMA_SHARED,
  WN_PRAGMA_SINGLE_PROCESS_BEGIN,
  WN_PRAGMA_SINGLE_PROCESS_END,
  WN_PRAGMA_ITERATE_VAR,
  WN_PRAGMA_ITERATE_INIT,
  WN_PRAGMA_ITERATE_COUNT,
  WN_PRAGMA_ITERATE_STEP,
  WN_PRAGMA_AFFINITY,
  WN_PRAGMA_DATA_AFFINITY,
  WN_PRAGMA_THREAD_AFFINITY,
  WN_PRAGMA_NUMTHREADS,
  WN_PRAGMA_NOWAIT,
  WN_PRAGMA_PAGE_PLACE,
#if 0
#define WN_PRAGMA_PAGE_ALLOCATE WN_PRAGMA_PAGE_PLACE	/* to be removed */
#endif
  WN_PRAGMA_ONTO,
  WN_PRAGMA_LASTTHREAD, 

  /* cray directives */

  WN_PRAGMA_NORECURRENCE,
  WN_PRAGMA_NEXT_SCALAR,

  /* purple pragmas */

  WN_PRAGMA_PURPLE_CONDITIONAL,
  WN_PRAGMA_PURPLE_UNCONDITIONAL,

  /* wopt pragmas */
  
  WN_PRAGMA_WOPT_FINISHED_OPT,

  /* KAP pragmas */

  WN_PRAGMA_KAP_ARCLIMIT,
  WN_PRAGMA_KAP_CONCURRENTIZE,
  WN_PRAGMA_KAP_INLINE_FILE,
  WN_PRAGMA_KAP_INLINE_PU,
  WN_PRAGMA_KAP_LIMIT,
  WN_PRAGMA_KAP_MINCONCURRENT,
  WN_PRAGMA_KAP_NOCONCURRENTIZE,
  WN_PRAGMA_KAP_NOINLINE_FILE,
  WN_PRAGMA_KAP_NOINLINE_PU,
  WN_PRAGMA_KAP_OPTIMIZE,
  WN_PRAGMA_KAP_ROUNDOFF,
  WN_PRAGMA_KAP_SCALAR_OPTIMIZE,
  WN_PRAGMA_KAP_CTHRESHOLD,
  WN_PRAGMA_KAP_EACH_INVARIANT_IF_GROWTH,
  WN_PRAGMA_KAP_MAX_INVARIANT_IF_GROWTH,
  WN_PRAGMA_KAP_STORAGE_ORDER,

  WN_PRAGMA_KAP_ASSERT_BOUNDS_VIOLATIONS,
  WN_PRAGMA_KAP_ASSERT_NOBOUNDS_VIOLATIONS,
  WN_PRAGMA_KAP_ASSERT_CONCURRENT_CALL,
  WN_PRAGMA_KAP_ASSERT_DO,
  WN_PRAGMA_KAP_ASSERT_DOPREFER,
  WN_PRAGMA_KAP_ASSERT_EQUIVALENCE_HAZARD,
  WN_PRAGMA_KAP_ASSERT_NOEQUIVALENCE_HAZARD,
  WN_PRAGMA_KAP_ASSERT_LAST_VALUE_NEEDED,
  WN_PRAGMA_KAP_ASSERT_NOLAST_VALUE_NEEDED,
  WN_PRAGMA_KAP_ASSERT_PERMUTATION,
  WN_PRAGMA_KAP_ASSERT_NORECURRENCE,
  WN_PRAGMA_KAP_ASSERT_RELATION,
  WN_PRAGMA_KAP_ASSERT_NOSYNC,
  WN_PRAGMA_KAP_ASSERT_TEMPORARIES_FOR_CONSTANT_ARGUMENTS,
  WN_PRAGMA_KAP_ASSERT_NOTEMPORARIES_FOR_CONSTANT_ARGUMENTS,
  WN_PRAGMA_KAP_ASSERT_ARGUMENT_ALIASING,
  WN_PRAGMA_KAP_ASSERT_BENIGN,
  WN_PRAGMA_KAP_ASSERT_DEPENDENCE,
  WN_PRAGMA_KAP_ASSERT_FREQUENCY,
  WN_PRAGMA_KAP_ASSERT_IGNORE_ANY_DEPENDENCE,
  WN_PRAGMA_KAP_ASSERT_IGNORE_ASSUMED_DEPENDENCE,
  WN_PRAGMA_KAP_ASSERT_NO_ARGUMENT_ALIASING,
  WN_PRAGMA_KAP_ASSERT_NO_CONCURRENT_CALL,
  WN_PRAGMA_KAP_ASSERT_NO_INTERCHANGE,
  WN_PRAGMA_KAP_ASSERT_USE_COMPRESS,
  WN_PRAGMA_KAP_ASSERT_USE_EXPAND,
  WN_PRAGMA_KAP_ASSERT_USE_CONTROLLED_STORE,
  WN_PRAGMA_KAP_ASSERT_USE_GATHER,
  WN_PRAGMA_KAP_ASSERT_USE_SCATTER,
  WN_PRAGMA_KAP_OPTIONS,
 
  WN_PRAGMA_PREAMBLE_END,

  /* Pragmas used to hide code from whirl2f (-flist) and whirl2c (-clist)
   */
  WN_PRAGMA_FLIST_SKIP_BEGIN,
  WN_PRAGMA_FLIST_SKIP_END,
  WN_PRAGMA_CLIST_SKIP_BEGIN,
  WN_PRAGMA_CLIST_SKIP_END,

  /* lego */
  WN_PRAGMA_FILL,
  WN_PRAGMA_ALIGN,

  /* mp */
  WN_PRAGMA_INDEPENDENT_BEGIN,
  WN_PRAGMA_INDEPENDENT_END,

  /* inline (move up to group with WN_PRAGMA_KAP_INLINE_FILE et al. ) */
  WN_PRAGMA_KAP_OPTION_INLINE,		/* from KAP C*$* OPTION inline= */
  WN_PRAGMA_KAP_OPTION_NOINLINE,

/* CRAY pragmas */

  /* vectorization directives */

  WN_PRAGMA_CRI_IVDEP,
  WN_PRAGMA_CRI_NOVECTOR,
  WN_PRAGMA_CRI_NOVSEARCH,
  WN_PRAGMA_CRI_PREFERVECTOR,
  WN_PRAGMA_CRI_SHORTLOOP,

  /* tasking directives */

  WN_PRAGMA_CRI_CASE,
  WN_PRAGMA_CRI_ENDCASE,
  WN_PRAGMA_CRI_COMMON,
  WN_PRAGMA_CRI_GUARD,
  WN_PRAGMA_CRI_ENDGUARD,
  WN_PRAGMA_CRI_ENDLOOP,
  WN_PRAGMA_CRI_PARALLEL,
  WN_PRAGMA_CRI_ENDPARALLEL,
  WN_PRAGMA_CRI_PREFERTASK,
  WN_PRAGMA_CRI_TASKCOMMON,
  WN_PRAGMA_CRI_TASKLOOP,
  WN_PRAGMA_CRI_SHARED,
  WN_PRAGMA_CRI_PRIVATE,
  WN_PRAGMA_CRI_VALUE,
  WN_PRAGMA_CRI_DEFAULTS,
  WN_PRAGMA_CRI_MAXCPUS,
  WN_PRAGMA_CRI_SAVELAST,
  WN_PRAGMA_CRI_CHUNKSIZE,
  WN_PRAGMA_CRI_NUMCHUNKS,

  WN_PRAGMA_CRI_TASK,
  WN_PRAGMA_CRI_NOTASK,
  WN_PRAGMA_CRI_ALIGN,		/* align instructions */
  WN_PRAGMA_CRI_BL,		/* bottomloading */
  WN_PRAGMA_CRI_CNCALL,		/* concurrent call */

  WN_PRAGMA_MPNUM, 
  WN_PRAGMA_COPYIN_BOUND,       /* variable used to store parameter in
                                 * array bound variable/expression.
                                 */
  WN_PRAGMA_SYNC_DOACROSS, 	/* associate an MP region with true doacross */
				/* i.e., doacross with explicit sync */
  WN_PRAGMA_DEFAULT,
  WN_PRAGMA_FIRSTPRIVATE,
  WN_PRAGMA_MASTER_BEGIN,
  WN_PRAGMA_ORDERED_BEGIN,
  WN_PRAGMA_ORDERED_END,
  WN_PRAGMA_ATOMIC,
  WN_PRAGMA_ORDERED_LOWER_BOUND, /* Internal XPRAGMA, store loop lower bnd */
  WN_PRAGMA_ORDERED_STRIDE,      /* Internal XPRAGMA, store loop stride */
  WN_PRAGMA_END_MARKER,
  WN_PRAGMA_PARALLEL_SECTIONS,  /* !$OMP parallel sections */

  WN_PRAGMA_START_STMT_CLUMP,    /* start/end of body of code that must be placed */
  WN_PRAGMA_END_STMT_CLUMP,	 /* within a single region. ie: region processing */
                                 /* can't split it. F90 IO for now                */

  WN_PRAGMA_TYPE_OF_RESHAPED_ARRAY, /* ty_idx of distribute_reshaped globals */

  /* "asm" support */
  WN_PRAGMA_ASM_CONSTRAINT,	/* constraint string for an ASM operand */
  WN_PRAGMA_ASM_CLOBBER,        /* clobber string for an ASM statement */

#ifdef KEY
  WN_PRAGMA_FORALL,
#endif
  WN_PRAGMA_COPYPRIVATE, /* by jhs, 02/7/22 */
  WN_PRAGMA_PARALLEL_WORKSHARE, /* by jhs, 04/3/10 */
  WN_PRAGMA_PWORKSHARE_BEGIN, /* by jhs, 2004/3/10 */
  WN_PRAGMA_PWORKSHARE_END, /* by jhs, 2004/3/10 */
  WN_PRAGMA_THREADPRIVATE, /* by jhs, 02.9.18 */
  MAX_WN_PRAGMA			/* last one in enum			*/
} WN_PRAGMA_ID;

/* Pragma scopes: */
typedef enum {
  WN_PRAGMA_SCOPE_UNKNOWN,
  WN_PRAGMA_SCOPE_PU,		/* Affects entire current program unit */
  WN_PRAGMA_SCOPE_WN,		/* Affects next whirl statement node */
  WN_PRAGMA_SCOPE_POINT,	/* Affects this point of the code */

  /* matching on/off pragmas must belong to the same block */
  WN_PRAGMA_SCOPE_ON,		/* Start of affected scope */
  WN_PRAGMA_SCOPE_OFF,		/* End of affected scope */

  WN_PRAGMA_SCOPE_SPECIAL,	/* pragma-specific rule for scope */

  MAX_SCOPE_PRAGMA		/* last one in enum */
} WN_PRAGMA_SCOPE;

/* schedtypes (for WN_PRAGMA_MPSCHEDTYPE) */
typedef enum {
  WN_PRAGMA_SCHEDTYPE_UNKNOWN,
  WN_PRAGMA_SCHEDTYPE_RUNTIME,
  WN_PRAGMA_SCHEDTYPE_SIMPLE,
  WN_PRAGMA_SCHEDTYPE_INTERLEAVE,
  WN_PRAGMA_SCHEDTYPE_DYNAMIC,
  WN_PRAGMA_SCHEDTYPE_GSS,
  WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED, 
  MAX_PRAGMA_SCHEDTYPE
} WN_PRAGMA_SCHEDTYPE_KIND;

/* Possible values for the default clause */
typedef enum {
  WN_PRAGMA_DEFAULT_UNKNOWN,
  WN_PRAGMA_DEFAULT_NONE,
  WN_PRAGMA_DEFAULT_SHARED,
  WN_PRAGMA_DEFAULT_PRIVATE,
  MAX_PRAGMA_DEFAULT
} WN_PRAGMA_DEFAULT_KIND;

/* which components are affected by a pragma */
typedef enum {
  PUSER_NULL	= 0x0,
  PUSER_IPA	= 0x1,
  PUSER_LNO	= 0x2,
  PUSER_WOPT	= 0x4,
  PUSER_CG	= 0x8,
  PUSER_MP	= 0x10,
  PUSER_NONE	= 0x20,
  PUSER_PURPLE  = 0x40,
  PUSER_W2C     = 0x80,
  PUSER_W2F     = 0x100,
  PUSER_REGION  = 0x200
} WN_PRAGMA_USERS;

/* flags for ACCESSED_ID pragma */
typedef enum {
  ACCESSED_LOAD     = 0x1,
  ACCESSED_STORE    = 0x2,
  ACCESSED_ILOAD    = 0x4,
  ACCESSED_ISTORE   = 0x8
} WN_PRAGMA_ACCESSED_FLAGS;

/* flags for SHARED pragma */
typedef enum {
  SHARED_DEADIN     = 0x1,
  SHARED_DEADOUT    = 0x2
} WN_PRAGMA_SHARED_FLAGS;

/* enumeration for mips_frequency_hint */
typedef enum {
  FREQUENCY_HINT_NEVER    = 0x1,
  FREQUENCY_HINT_INIT     = 0x2,
  FREQUENCY_HINT_FREQUENT = 0x3
} MIPS_FREQUENCY_HINT;

/* enumeration for distribute/redistribute/distribute_reshape type */
typedef enum {
  DISTRIBUTE_STAR         = 0x1,
  DISTRIBUTE_BLOCK        = 0x2,
  DISTRIBUTE_CYCLIC_EXPR  = 0x3,
  DISTRIBUTE_CYCLIC_CONST = 0x4 
} DISTRIBUTE_TYPE;


/* enumeration for assert_do/assert_doprefer */
typedef enum {
  ASSERT_DO_NONE	   = 0x0,  /* not used, just used to mark errors */
  ASSERT_DO_SERIAL         = 0x1,
  ASSERT_DO_CONCURRENT     = 0x2,
  ASSERT_DO_VECTOR         = 0x3
} ASSERT_DO_TYPE;

/* enumeration for assert_dependence */
typedef enum {
  ASSERT_DEPENDENCE_SAFE       = 0x1,
  ASSERT_DEPENDENCE_UNSAFE     = 0x2
} ASSERT_DEPENDENCE_TYPE;

/* non-integer values for assert_frequency */
enum {
  ASSERT_FREQUENCY_ERROR	= -3,
  ASSERT_FREQUENCY_RARELY	= -2,
  ASSERT_FREQUENCY_USUALLY	= -1
};

/* ====================================================================
 *
 * WN_PRAGMA_DESC:  Pragma descriptors
 *
 * These describe the characteristics of the WHIRL pragmas
 *
 * ====================================================================
 */

/* Pragma descriptors: */
typedef struct wn_pragma_desc {
  WN_PRAGMA_USERS users;	/* The components that use this pragma */
  WN_PRAGMA_SCOPE scope;	/* Pragma scope */
  char	*name;			/* Pragma name */
} WN_PRAGMA_DESC;

extern WN_PRAGMA_DESC WN_pragmas[];

/* I suspect this is not needed anymore, so set it to NULL
 * for now for testing.
 */
#define IS_AFFINITY_XPRAGMA(wn) (FALSE)


#ifdef __cplusplus
}
#endif
#endif /* pragmas_INCLUDED */

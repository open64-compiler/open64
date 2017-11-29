/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

#include <stdint.h>
#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop

/* Header of wn_mp_dg.cxx
*  csc.
*/
#include <sys/types.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */

#define USE_STANDARD_TYPES          /* override unwanted defines in "defs.h" */

#include <bstring.h>
#include "wn.h"
#include "wn_util.h"
#include "erglob.h"
#include "errors.h"
#include "strtab.h"                 /* for strtab */
#include "symtab.h"                 /* for symtab */
#include "irbdata.h"                /* for inito */
#include "dwarf_DST_mem.h"          /* for DST */
#include "pu_info.h"
#ifdef __MINGW32__
#include <WINDOWS.h>
#endif /* __MINGW32__ */
#include "ir_bwrite.h"
#include "ir_reader.h"
#include "ir_bcom.h"
#include "region_util.h"            /* for RID */
#include "dep_graph.h"
#include "cxx_hash.h"
#include "wn_mp.h"        /* for wn_mp_dg.cxx's extern functions */

/* wn_mp_dg.cxx header end.
*  csc.
*/

#include <string.h>

#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif /* ! defined(BUILD_OS_DARWIN) */
#include "alloca.h"
#include "cxx_template.h"
#include "defs.h"
#include "glob.h"
#include "errors.h"
#include "erglob.h"
#include "erbe.h"
#include "tracing.h"
#include "strtab.h"

#include "symtab.h"

#include "wn.h"
#include "wn_util.h"
#include "wn_simp.h"
#include "stblock.h"
#include "data_layout.h"
#include "targ_sim.h"
#include "targ_const.h"
#include "config_targ.h"
#include "config_asm.h"
#include "const.h"
#include "ttype.h"
#include "wn_pragmas.h"
#include "wn_lower.h"
#include "region_util.h"
#include "wutil.h"
#include "wn_map.h"
#include "pu_info.h"
#include "dwarf_DST.h"
#include "dwarf_DST_producer.h"
#include "dwarf_DST_mem.h"
#include "config.h"
#include "standardize.h"
#include "irbdata.h"
#include "omp_lower.h"
#include "privatize_common.h"
#include "cxx_hash.h"
#include "wn_mp.h"
#include "mempool.h"
#include "parmodel.h"	// for NOMINAL_PROCS
#include "fb_info.h"
#include "fb_whirl.h"
#include "be_symtab.h"
#ifdef KEY
#include "wn_lower.h"
#include "config_opt.h"
#endif
#include "alias_analyzer.h"

/*
MP lowerer cleanup TODO by DRK:

1.  Convert all the tables (LABEL, VAR, etc.) to BOUNDSCHECKED_VECTOR's,
put BOUNDSCHECKED_VECTOR into cxx_template.h

2.  Define class for entering/exiting a PU's scope such that global
vars CURRENT_SYMTAB, Current_pu, Current_Map_Tab, and Current_PU_Info
are managed automatically (stack discipline seems appropriate)

3.  New global utility function for defining function prototype TY's

4.  Delete "unused function"'s when debugging is done

5.  Make sure we use ST * and TY * instead of IDX's where possible

6.  MP_Reduction_Combine_Cycles() should use targ_info or something for
machine cycle time and runtime costs.
*/


#define WN_Compare_Trees(x,y)	(WN_Simp_Compare_Trees(x,y))

static DST_INFO_IDX  nested_dst;
#ifdef KEY
static void
Transfer_Maps ( WN_MAP_TAB * parent, WN_MAP_TAB * child, WN * tree,
                RID * root_rid );
#endif
/*
* Add csc debug support
*/
void csc_printf(const char *what, INT line)
{
     printf("csc: %s at wn_mp.cxx: %d .\n", what, line );
}

#define csc_debug(x) csc_printf(x,__LINE__)

inline WN_OFFSET WN_offsetx ( WN *wn )
{
  OPERATOR opr;
  opr = WN_operator(wn);
  if ((opr == OPR_PRAGMA) || (opr == OPR_XPRAGMA)) {
    return (WN_pragma_arg1(wn));
  } else {
    return (WN_offset(wn));
  }
}

inline void WN_set_offsetx ( WN *wn, WN_OFFSET ofst )
{
  OPERATOR opr;
  opr = WN_operator(wn);
  if ((opr == OPR_PRAGMA) || (opr == OPR_XPRAGMA)) {
    WN_pragma_arg1(wn) = ofst;
  } else {
    WN_offset(wn) = ofst;
  }
}

static inline TYPE_ID Promote_Type(TYPE_ID mtype)
{
  switch (mtype) {
    case MTYPE_I1 : case MTYPE_I2: return(MTYPE_I4);
    case MTYPE_U1 : case MTYPE_U2: return(MTYPE_U4);
    default: return mtype;
  }
}

static void
my_Get_Return_Pregs(PREG_NUM *rreg1, PREG_NUM *rreg2, mTYPE_ID type,
                    const char *file, INT line)
{
  if (WHIRL_Return_Info_On) {
    RETURN_INFO return_info = Get_Return_Info(Be_Type_Tbl(type),
                                              Use_Simulated);
    if (RETURN_INFO_count(return_info) <= 2) {
      *rreg1 = RETURN_INFO_preg(return_info, 0);
      *rreg2 = RETURN_INFO_preg(return_info, 1);
    } else
      Fail_FmtAssertion("file %s, line %d: more than 2 return registers",
                        file, line);

  } else
    Get_Return_Pregs(type, MTYPE_UNKNOWN, rreg1, rreg2);

  FmtAssert(*rreg1 != 0 && *rreg2 == 0, ("bad return pregs"));
} // my_Get_Return_Pregs()

#define GET_RETURN_PREGS(rreg1, rreg2, type) \
  my_Get_Return_Pregs(&rreg1, &rreg2, type, __FILE__, __LINE__)

typedef enum {

  MPRUNTIME_NONE = 0,
  MPRUNTIME_FIRST = 1,

    MPR_SETUP                = 1,
    MPR_CLEANUP              = 2,

    MPR_GETLOCK              = 3,
    MPR_UNLOCK               = 4,
    MPR_BARRIER              = 5,
    MPR_SETLOCK              = 6,
    MPR_UNSETLOCK            = 7,

    MPR_COPYIN               = 8,

    MPR_PARALLEL_DO_32       = 9,
    MPR_PARALLEL_DO_64       = 10,
    MPR_PARALLEL_REGION      = 11,

    MPR_BEGIN_PDO_32         = 12,
    MPR_BEGIN_PDO_64         = 13,
    MPR_NEXT_ITERS_32        = 14,
    MPR_NEXT_ITERS_64        = 15,
    MPR_END_PDO              = 16,

    MPR_BEGIN_SINGLE_PROCESS = 17,
    MPR_END_SINGLE_PROCESS   = 18,

    MPR_ENTER_GATE           = 19,
    MPR_EXIT_GATE            = 20,

    MPR_BEGIN_INDEPENDENT    = 21,
    MPR_END_INDEPENDENT      = 22,

    MPR_MY_THREADNUM         = 23,

  MPR_OMP_PARALLEL_REGION    = 24,
  MPR_OMP_BEGIN_SPR          = 25,      /* serialized parallel region */
  MPR_OMP_END_SPR            = 26,
  MPR_OMP_PARALLEL_DO_32     = 27,
  MPR_OMP_PARALLEL_DO_64     = 28,
  
  MPR_OMP_BEGIN_PDO_64         = 29,
  MPR_OMP_NEXT_ITERS_64        = 30,
  MPR_OMP_END_PDO              = 31,

  MPR_OMP_BEGIN_SINGLE_PROCESS = 32,
  MPR_OMP_END_SINGLE_PROCESS   = 33,
  MPR_OMP_BARRIER_OLD          = 34, /* conflict with new API */

  MPR_OMP_PDO_ORDERED_BEGIN    = 35,
  MPR_OMP_PDO_ORDERED_END      = 36,
  MPR_OMP_ORDERED_BEGIN_OLD    = 37, /* conflict with new API */
  MPR_OMP_ORDERED_END_OLD      = 38, /* conflict with new API */

  MPR_OMP_COPYIN               = 39,
  MPR_OMP_NONPOD_COPYIN        = 40,
  MPR_OMP_NONPOD_ARRAY_COPYIN  = 41,

  // Begin KMPC RTL calls.
  MPR_OMP_IN_PARALLEL		  = 42,	/* test if in parallel region */
  MPR_OMP_CAN_FORK		  = 43,	/* test fork, to be eliminated*/
  MPR_OMP_SET_NUM_THREADS	  = 44,	/* for threadnum setting, not used*/
  MPR_OMP_INIT_RTL		  = 45, /* obsoleted */
  MPR_OMP_FINI_RTL		  = 46, /* obsoleted */
  
  MPR_OMP_SERIALIZED_PARALLEL     = 47, /* para-region serialized, can delete */
  MPR_OMP_END_SERIALIZED_PARALLEL = 48, /* Can be deleted */
  MPR_OMP_GET_THREAD_NUM	  = 49,
  MPR_OMP_GET_NUM_THREADS	  = 50, 

  MPR_OMP_FORK			  = 51,

  MPR_OMP_STATIC_INIT_4		 = 52,
  MPR_OMP_STATIC_INIT_8	 	 = 53,
  MPR_OMP_STATIC_FINI	  	 = 54, /* Can be deleted */

  MPR_OMP_SCHEDULER_INIT_4	  = 55,
  MPR_OMP_SCHEDULER_INIT_8	  = 56,
  MPR_OMP_SCHEDULER_NEXT_4	  = 57,
  MPR_OMP_SCHEDULER_NEXT_8	  = 58,
  
  MPR_OMP_SINGLE		  = 59,
  MPR_OMP_END_SINGLE		  = 60,

  MPR_OMP_MASTER		  = 61,
  MPR_OMP_END_MASTER		  = 62,
  
  MPR_OMP_BARRIER		  = 63,

  MPR_OMP_CRITICAL		= 64,
  MPR_OMP_END_CRITICAL		= 65,
  MPR_OMP_ORDERED		= 66,
  MPR_OMP_END_ORDERED		= 67,

  MPR_OMP_FLUSH			= 68,	/* Not really needed? to be deleted*/
#ifdef KEY
  MPR_OMP_GET_THDPRV            = 69,
  MPR_OMP_COPYIN_THDPRV         = 70,
  MPR_OMP_COPYPRIVATE           = 71,

  MPRUNTIME_LAST = MPR_OMP_COPYPRIVATE
#else
  MPRUNTIME_LAST = MPR_OMP_FLUSH
#endif

} MPRUNTIME;


  // Schedule type
typedef enum {
  OMP_SCHED_UNKNOWN			= 0,
  OMP_SCHED_STATIC			= 1,
  OMP_SCHED_STATIC_EVEN			= 2,
  OMP_SCHED_DYNAMIC			= 3,
  OMP_SCHED_GUIDED			= 4,
  OMP_SCHED_RUNTIME			= 5,
    // Ordered schedule type
  OMP_SCHED_ORDERED_STATIC		= 33,
  OMP_SCHED_ORDERED_STATIC_EVEN		= 34,
  OMP_SCHED_ORDERED_DYNAMIC		= 35,
  OMP_SCHED_ORDERED_GUIDED		= 36,
  OMP_SCHED_ORDERED_RUNTIME		= 37,
 
  OMP_SCHED_NORMAL_FIRST  = OMP_SCHED_STATIC,
  OMP_SCHED_NORMAL_LAST   = OMP_SCHED_RUNTIME,
  OMP_SCHED_ORDERED_FIRST = OMP_SCHED_ORDERED_STATIC,
  OMP_SCHED_ORDERED_LAST  = OMP_SCHED_ORDERED_RUNTIME
} SCHEDULE_TYPE;

  // kinds of nested functions generated for parallel constructs
typedef enum {
  PAR_FUNC_NONE = 0,
  PAR_FUNC_DO32,
  PAR_FUNC_DO64,
  PAR_FUNC_REGION,
  PAR_FUNC_LAST = PAR_FUNC_REGION
} PAR_FUNC_TYPE;

typedef enum {
  VAR_NONE             = 0,
  VAR_LASTLOCAL        = 1,
  VAR_LOCAL            = 2,
    /* FIRSTPRIVATE implies LOCAL for all purposes in MP lowering */
  VAR_FIRSTPRIVATE     = 3,
  VAR_REDUCTION_SCALAR = 4,
  VAR_REDUCTION_ARRAY  = 5,
	  /* It seems VAR_REDUCTION_ARRAY is used for other 
		 * usage in original implementation,
		 * so I define a new one for OpenMP array reduction.
		 */
  VAR_REDUCTION_ARRAY_OMP  = 6
} VAR_TYPE;

typedef struct {
  VAR_TYPE   vtype;
  TYPE_ID    mtype;
  BOOL       has_offset;
  BOOL       is_static_array;
  BOOL       is_dynamic_array;
    // TRUE iff (vtype == VAR_LASTLOCAL) and variable is also FIRSTPRIVATE
  BOOL       is_last_and_firstprivate;
  BOOL       is_non_pod;  // C++ object or array of objects
                          // "pod" stands for "plain old data"
  TY_IDX     ty;
  WN        *vtree;
  WN        *vtreex;
  ST        *orig_st;
  WN_OFFSET  orig_offset;
  ST        *new_st;
  WN_OFFSET  new_offset;
  OPERATOR   reduction_opr; /* specified in REDUCTION pragma */
  OPCODE     reduction_opc; /* computed from reduction_opr and its operands */
} VAR_TABLE;

typedef enum {
  PCLASS_UNKNOWN        = 0,
  PCLASS_DEADIN_DEADOUT = 1,
  PCLASS_COPYIN_DEADOUT = 2,
  PCLASS_DEADIN_COPYOUT = 3,
  PCLASS_COPYIN_COPYOUT = 4,
  PCLASS_LIVEIN_DEADOUT = 5,
  PCLASS_DEADIN_LIVEOUT = 6,
  PCLASS_LIVEIN_LIVEOUT = 7 
} PREG_CLASS;

typedef struct {
  PREG_CLASS  pclass;
  TYPE_ID     type;
  BOOL        preamble_store;
  BOOL        lastlocal_list;
  BOOL        local_list;
  BOOL        reduction_list;
  BOOL        shared_list;
  INT32       shared_flags;
  ST          *temp;
} PREG_INFO;

typedef LABEL_IDX LABEL_INFO_TABLE;

typedef ST * SHARED_TABLE;

typedef INT32 MPID_TABLE;

typedef HASH_TABLE<WN *, BOOL> WN_TO_BOOL_HASH;
static const mINT32 NUM_HASH_ELEMENTS = 1021;

/*
Template for simple fixed-size vector with bounds-checking (if compiled for
debugging).  Why did we have to re-invent the wheel here?  Because neither
the old classes in cxx_template.cxx nor the new STL classes have precisely
this functionality.
*/

template<class T>
class BOUNDSCHECKED_VECTOR {
  T *array;
  mUINT32 size;
  MEM_POOL *mempool;
  typedef BOUNDSCHECKED_VECTOR<T> *PV;
  BOOL zero_is_invalid; // index 0 is invalid (for e.g. PREG_IDX)
  PV *ppv;  // external pointer to "this"; invalidate upon destruction
public:
  BOUNDSCHECKED_VECTOR(MEM_POOL *_mempool, mUINT32 _size,
                       BOOL _zero_is_invalid = FALSE, PV *_ppv = NULL)
    : mempool(_mempool), size(_size), zero_is_invalid(_zero_is_invalid),
      ppv(_ppv) {
      array = CXX_NEW_ARRAY(T, size, mempool);
      if (ppv)
        *ppv = this;
  }
  T &operator[](mINT32 idx) {
#ifdef Is_True_On
    if (idx < 0)
      Fail_FmtAssertion("BOUNDSCHECKED_VECTOR::operator[] : negative index "
                        "%d", idx);
    if (idx == 0 && zero_is_invalid)
      Fail_FmtAssertion("BOUNDSCHECKED_VECTOR::operator[] : invalid zero "
                        "index");
    if (idx >= size)
      Fail_FmtAssertion("BOUNDSCHECKED_VECTOR::operator[] : out-of-bounds "
                        "index %d (array size is %d)", idx, size);
#endif
    return array[idx];
  }
    // so we can say: pv->at(idx)
  T &at(mINT32 idx) { return (*this)[idx]; }
  mUINT32 Size() const { return size; }
  ~BOUNDSCHECKED_VECTOR() {
    CXX_DELETE_ARRAY(array, mempool);
    if (ppv)
      *ppv = NULL;
  }
};

static BOOL first_call = TRUE;  // first call to wn_lower()
static MEM_POOL mp_lower_pool;  // for lower_mp() temporaries

static WN *stmt_block;		/* Original statement nodes */
static WN *serial_stmt_block;	/* Serial statement nodes */
static WN *cont_nodes;		/* Statement nodes after mp code */
static WN *do_node;		/* Do loop node for parallel do */
static WN *replace_block;	/* Replacement nodes to be returned */
static WN *ntrip_calc;		/* Ntrip calculation code */
static WN *livein_block;	/* Livein code for mp call */
static WN *alloca_block;	/* Alloca code for mp routine */
static WN *copyin_block;	/* Copyin code for mp call */
static WN *copyout_block;	/* Copyout code for mp call */
static WN *firstprivate_block;  /* FIRSTPRIVATE code for mp call */
static WN *liveout_block;	/* Liveout code for mp call */
static WN *do_prefix;		/* Prefix code for do loop */
static WN *do_suffix;   /* Suffix code for do loop */
static WN *if_preamble_block;	/* MP if preamble block */
static WN *if_postamble_block;	/* MP if postamble block */
static WN *do_preamble_block;	/* Do preamble block */
static INT64 line_number;	/* Line number of parallel do/region */

static ST *parallel_proc;	/* Extracted parallel process */
static ST *local_start;   /* Parallel Do local start, obsolete */
static ST *local_ntrip;   /* Parallel Do local ntrip, obsolete */
static ST *thread_info;   /* Parallel Do thread info, obsolete */
static ST *local_upper = NULL;   /* Parallel Do local upper bound */
static ST *local_lower = NULL;   /* Parallel Do local lower bound */
static ST *local_stride = NULL;  /* Parallel Do local stride for next chunk */
static ST *last_iter;      /* Is local execute last iteration? */
static ST *local_limit;   /* Parallel Do local limit */
static ST *limit_st;      /* Temp var to store do_limit. can be preg. */
static WN_OFFSET limit_ofst;
static ST *local_gtid;		/* Microtask local gtid */
static ST *local_btid;		/* Microtask local btid */
static WN *base_node;		  /* Parallel do base */
static WN *limit_node = NULL;		/* Parallel do limit */
static WN *ntrip_node;		/* Parallel do trip count */
static WN *stride_node;		/* Parallel do stride */
static WN *parallel_func;	/* Parallel do function */
static FEEDBACK *parallel_pu_fb;  /* Feedback for parallel function */
static WN *reference_block;	/* Parallel funciton reference block */
static INT32 func_level;	/* Parallel function stab level */
static ST *do_index_st;		/* User do index variable ST */
static TYPE_ID do_index_type;	/* User do index variable type */
static BOOL fast_doacross;	/* Flag if doacross meets fastpath requirement*/

static INT32 copyin_count;	/* Count of copyins */
static INT32 local_count;	/* Count of lastlocals, locals, firstprivates
				   & reductions */
static INT32 reduction_count;	/* Count of reductions */
static INT32 shared_count;	/* Count of shareds, lastlocals & reductions */
static WN *affinity_nodes;	/* Points to (optional) affinity nodes */
static WN *affinity_d_nodes;	/* Points to (optional) affinity data nodes */
static WN *affinity_t_nodes;	/* Points to (optional) affinity thread nodes */
static WN *chunk_node;		/* Points to (optional) chunk node */
static WN *copyin_nodes;	/* Points to (optional) copyin nodes */
static WN *copyin_nodes_end;	/* Points to (optional) copyin nodes end */
static WN *if_node;		/* Points to (optional) if node */
static WN *lastlocal_nodes;	/* Points to (optional) lastlocal nodes */
static WN *lastthread_node;	/* Points to (optional) lastthread node */
static WN *local_nodes;		/* Points to (optional) local nodes */
static WN *firstprivate_nodes;	/* Points to (optional) firstprivate nodes */
                                // frontend-generated finalization code for
                                // non-POD lastlocals
static WN *non_pod_finalization_nodes;
                                // firstprivate and lastprivate appear on same
                                // non-POD ST
static BOOL non_pod_first_and_lastprivate;
static WN *mpnum_node;		/* Points to (optional) mpnum node */
static WN *mpsched_node;	/* Points to (optional) mpsched node */
static WN *numthreads_node;	/* Points to (optional) numthreads node */
static WN *ordered_node;	/* Points to (optional) ordered node */
static WN *do_order_lb = NULL; /* store lb and stride of an ordered parallel */
static WN *do_order_stride = NULL; /* loop, passed to omp-pdo-ordered calls */
static WN *reduction_nodes;	/* Points to (optional) reduction nodes */
static WN *shared_nodes;	/* Points to (optional) shared nodes */

static INT32 num_constructs;	   /* Number of parallel constructs */
static INT32 nested_local_count;   /* Nested count of lastlocals, locals, &
				      firstprivates */
static INT32 nested_reduction_count; /* Nested count of reductions */
static WN *nested_affinity_nodes;  /* Points to (nested) affinity nodes */
static WN *nested_affinity_d_nodes;/* Points to (nested) affin. data nodes */
static WN *nested_affinity_t_nodes;/* Points to (nested) affin. thread nodes */
static WN *nested_chunk_node;	   /* Points to (nested) chunk node */
static WN *nested_lastlocal_nodes; /* Points to (nested) lastlocal nodes */
static WN *nested_lastthread_node; /* Points to (nested) lastthread node */
static WN *nested_local_nodes;	   /* Points to (nested) local nodes */
static WN *nested_firstprivate_nodes; /* Points to (nested) firstprivate
					 nodes */
static WN *nested_mpsched_node;	   /* Points to (nested) mpsched node */
static WN *nested_nowait_node;	   /* Points to (nested) nowait node */
static WN *nested_ordered_node;	   /* Points to (nested) ordered node */
static WN *nested_do_order_lb = NULL; /* store lb and stride of an ordered */
static WN *nested_do_order_stride = NULL; /* parallel loop */
static WN *nested_reduction_nodes; /* Points to (nested) reduction nodes */
static WN *nested_shared_nodes;	   /* Points to (nested) shared nodes */

static VAR_TABLE *var_table;	    /* Table of variable substitutions */
static VAR_TABLE *nested_var_table; /* Table of nested variable substitutions */

typedef BOUNDSCHECKED_VECTOR<PREG_INFO> PREG_INFO_TABLE;
PREG_INFO_TABLE *preg_info_table;   /* Table of preg information */

static LABEL_INFO_TABLE *label_info_table;    /* Mapping from parent to
                                                 nested PU labels */
static SHARED_TABLE *shared_table;  /* Table of shared ST's */
static MPID_TABLE *mpid_table;	    /* Table of assigned mpid's */

static INT32 mpid_size = 0;	    /* Size of mpid_table */

  // Generic types for nested parallel functions
static TY_IDX mpdo32_ty = TY_IDX_ZERO;
static TY_IDX mpdo64_ty = TY_IDX_ZERO;
static TY_IDX mpregion_ty = TY_IDX_ZERO;
  // Generic type for parallel runtime routines
static TY_IDX mpruntime_ty = TY_IDX_ZERO;

static ST_IDX last_pu_proc_sym = ST_IDX_ZERO; // last PU we compiled
static INT32 do_id = 0;			// Unique do number within PU
static INT32 region_id = 0;		// Unique region number within PU
static INT32 lock_id = 0;		// Unique lock number within PU
static WN *pu_chunk_node = NULL;        // (optional) PU chunk node
static WN *pu_mpsched_node = NULL;      // (optional) PU mpsched node

  // ST's for compiler-generated temporaries
static ST *mpbase_st = NULL;		/* ST for local iteration base */
static ST *mptrips_st = NULL;		/* ST for local iteration trips */
static ST *mpflags_st = NULL;		/* ST for local iteration flags */

static BOOL pu_has_eh = FALSE;		/* Parent PU contains EH region */

static SYMTAB_IDX psymtab;	/* Parent symbol table */
static SYMTAB_IDX csymtab;	/* Child symbol table */
static PU_Info *ppuinfo;	/* Parent PU info structure */
static WN_MAP_TAB *pmaptab;	/* Parent map table */
static WN_MAP_TAB *cmaptab;	/* Child map table */

static BOOL pu_has_alloca;	/* PU contains alloca */
static BOOL pu_has_region;	/* PU contains region */

static BOOL inside_versioning_if; /* root node of tree being lowered is the
                                     MP_IF test for LNO versioning */

  // TRUE if MP region we're currently processing has the compiler-
  // generated flag set on its first pragma (the one that identifies it
  // as a PARALLEL_REGION, PDO, etc.), FALSE otherwise
static BOOL comp_gen_construct;

  // What kind of construct we're lowering.  We don't distinguish among
  // most of the simpler types. Note that mpt is set according to the
  // outermost construct by lower_mp (say, to MPP_PARALLEL_REGION), and
  // when we reach an inner construct (say, MPP_PDO) we save the old value
  // of mpt, set it to something appropriate for the inner construct until
  // we're done processing that construct, then restore the old value of mpt.
static MP_process_type mpt;

  // Type and ST for intel openmp runtime library calls
  // csc.
static ST * gtid_st = NULL;		  /* ST for global thread number */
static TY_IDX lock_ty_idx = TY_IDX_ZERO;  /* Type index for lock */
static ST *old_gtid_st = NULL;

  // To add unnamed_lock_st for unnamed lock and critical_lock_not_init (lg)
static ST *unnamed_lock_st = NULL;         /* ST for unnamed lock */
static BOOL critical_lock_not_init = TRUE; /* for uninitialized critical lock */

/*  This table contains the external names of all MP runtime routines.  */

static const char *mpr_names [MPRUNTIME_LAST + 1] = {
  "",				/* MPRUNTIME_NONE */
  "__mp_setup",			/* MPR_SETUP */
  "__mp_cleanup",		/* MPR_CLEANUP */
  "__mp_getlock",		/* MPR_GETLOCK */
  "__mp_unlock",		/* MPR_UNLOCK */
  "__mp_barrier",		/* MPR_BARRIER */
  "mp_setlock",			/* MPR_SETLOCK */
  "mp_unsetlock",		/* MPR_UNSETLOCK */
  "__mp_copyin",		/* MPR_COPYIN */
  "__mp_parallel_do",		/* MPR_PARALLEL_DO_32 */
  "__mp_parallel_do_64",	/* MPR_PARALLEL_DO_64 */
  "__mp_region",		/* MPR_PARALLEL_REGION */
  "__mp_begin_pdo",		/* MPR_BEGIN_PDO_32 */
  "__mp_begin_pdo_64",		/* MPR_BEGIN_PDO_64 */
  "__mp_next_iters",		/* MPR_NEXT_ITERS_32 */
  "__mp_next_iters_64",		/* MPR_NEXT_ITERS_64 */
  "__mp_end_pdo",		/* MPR_END_PDO */
  "__mp_begin_single_process",	/* MPR_BEGIN_SINGLE_PROCESS */
  "__mp_end_single_process",	/* MPR_END_SINGLE_PROCESS */
  "__mp_enter_gate_new",	/* MPR_ENTER_GATE */
  "__mp_exit_gate_new",		/* MPR_EXIT_GATE */
  "__mp_begin_single_process",	/* MPR_BEGIN_INDEPENDENT */
  NULL,                     	/* MPR_END_INDEPENDENT */
  "mp_my_threadnum",            /* MPR_MY_THREADNUM */
  "__omp_region",               /* MPR_OMP_PARALLEL_REGION */
  "__omp_begin_spr",            /* MPR_OMP_BEGIN_SPR */
  "__omp_end_spr",              /* MPR_OMP_END_SPR */
  "__omp_parallel_do",          /* MPR_OMP_PARALLEL_DO_32 */
  "__omp_parallel_do_64",       /* MPR_OMP_PARALLEL_DO_64 */
  "__omp_begin_pdo_64",		/* MPR_OMP_BEGIN_PDO_64 */
  "__omp_next_iters_64",	/* MPR_OMP_NEXT_ITERS_64 */
  "__omp_end_pdo",		/* MPR_OMP_END_PDO */
  "__omp_begin_single_process",	/* MPR_OMP_BEGIN_SINGLE_PROCESS */
  "__omp_end_single_process",	/* MPR_OMP_END_SINGLE_PROCESS */
  "__omp_barrier",		/* MPR_OMP_BARRIER_OLD */
  "__omp_pdo_ordered_begin",	/* MPR_OMP_PDO_ORDERED_BEGIN */
  "__omp_pdo_ordered_end",	/* MPR_OMP_PDO_ORDERED_END */
  "__omp_begin_ordered",	/* MPR_OMP_ORDERED_BEGIN_OLD */
  "__omp_end_ordered",	        /* MPR_OMP_ORDERED_END_OLD */
  "__omp_copyin",		/* MPR_OMP_COPYIN */
  "__omp_nonpod_copyin",	/* MPR_OMP_COPYIN */
  "__omp_nonpod_array_copyin",	/* MPR_OMP_COPYIN */
  "__ompc_in_parallel",		/* ORC-OpenMP API: MPR_OMP_IN_PARALLEL */
  "__ompc_can_fork",		/* MPR_OMP_CAN_FORK */
  "__ompc_set_num_threads", 	/* MPR_OMP_SET_NUM_THREADS */
  "__ompc_init_rtl",		/* MPR_OMP_INIT_RTL */
  "__ompc_fini_rtl",		/* MPR_OMP_FINI_RTL */
  "__ompc_serialized_parallel", /* MPR_OMP_SERIALIZED_PARALLEL */
  "__ompc_end_serialized_parallel", /* MPR_OMP_END_SERIALIZED_PARALLEL */
  "__ompc_get_local_thread_num",   	/* MPR_OMP_GET_THREAD_NUM */
  "__ompc_get_num_threads",  	/* MPR_OMP_GET_NUM_THREADS */
  "__ompc_fork",			/* MPR_OMP_FORK */
  "__ompc_static_init_4",   	/* MPR_OMP_STATIC_INIT_4 */
  "__ompc_static_init_8",   	/* MPR_OMP_STATIC_INIT_8 */
  "__ompc_static_fini",     	/* MPR_OMP_STATIC_FINI */
  "__ompc_scheduler_init_4",     /* MPR_OMP_SCHEDULER_INIT_4 */
  "__ompc_scheduler_init_8",     /* MPR_OMP_SCHEDULER_INIT_8 */
  "__ompc_schedule_next_4",     /* MPR_OMP_SCHEDULER_NEXT_4 */
  "__ompc_schedule_next_8",     /* MPR_OMP_SCHEDULER_NEXT_8 */  
  "__ompc_single",              /* MPR_OMP_SINGLE */
  "__ompc_end_single",          /* MPR_OMP_END_SINGLE */
  "__ompc_master",              /* MPR_OMP_MASTER */
  "__ompc_end_master",          /* MPR_OMP_END_MASTER */
  "__ompc_barrier",             /* MPR_OMP_BARRIER */
  "__ompc_critical",            /* MPR_OMP_CRITICAL */
  "__ompc_end_critical",        /* MPR_OMP_END_CRITICAL */
  "__ompc_ordered",             /* MPR_OMP_ORDERED */
  "__ompc_end_ordered",         /* MPR_OMP_ORDERED */
  "__ompc_flush",               /* MPR_OMP_FLUSH  */
#ifdef KEY
  "__ompc_get_thdprv",          /* MPR_OMP_GET_THDPRV */
  "__ompc_copyin_thdprv",       /* MPR_OMP_COPYIN_THDPRV */
  "__ompc_copyprivate",         /* MPR_OMP_COPYPRIVATE */
#endif
};


/*  This table contains ST_IDX entries entries for each of the MP
    runtime routines.  These entries allow efficient sharing of all
    calls to a particular runtime routine. */

static ST_IDX mpr_sts [MPRUNTIME_LAST + 1] = {
  ST_IDX_ZERO,	 /* MPRUNTIME_NONE */
  ST_IDX_ZERO,	 /* MPR_SETUP */
  ST_IDX_ZERO,	 /* MPR_CLEANUP */
  ST_IDX_ZERO,	 /* MPR_GETLOCK */
  ST_IDX_ZERO,	 /* MPR_UNLOCK */
  ST_IDX_ZERO,	 /* MPR_BARRIER */
  ST_IDX_ZERO,	 /* MPR_SETLOCK */
  ST_IDX_ZERO,	 /* MPR_UNSETLOCK */
  ST_IDX_ZERO,	 /* MPR_COPYIN */
  ST_IDX_ZERO,	 /* MPR_PARALLEL_DO_32 */
  ST_IDX_ZERO,	 /* MPR_PARALLEL_DO_64 */
  ST_IDX_ZERO,	 /* MPR_PARALLEL_REGION */
  ST_IDX_ZERO,	 /* MPR_BEGIN_PDO_32 */
  ST_IDX_ZERO,	 /* MPR_BEGIN_PDO_64 */
  ST_IDX_ZERO,	 /* MPR_NEXT_ITERS_32 */
  ST_IDX_ZERO,	 /* MPR_NEXT_ITERS_64 */
  ST_IDX_ZERO,	 /* MPR_END_PDO */
  ST_IDX_ZERO,	 /* MPR_BEGIN_SINGLE_PROCESS */
  ST_IDX_ZERO,	 /* MPR_END_SINGLE_PROCESS */
  ST_IDX_ZERO,	 /* MPR_ENTER_GATE */
  ST_IDX_ZERO,	 /* MPR_EXIT_GATE */
  ST_IDX_ZERO,	 /* MPR_BEGIN_INDEPENDENT */
  ST_IDX_ZERO,	 /* MPR_END_INDEPENDENT */
  ST_IDX_ZERO,	 /* MPR_MY_THREADNUM */
  ST_IDX_ZERO,	 /* MPR_OMP_PARALLEL_REGION */
  ST_IDX_ZERO,	 /* MPR_OMP_BEGIN_SPR */
  ST_IDX_ZERO,	 /* MPR_OMP_END_SPR */
  ST_IDX_ZERO,	 /* MPR_OMP_PARALLEL_DO_32 */
  ST_IDX_ZERO,	 /* MPR_OMP_PARALLEL_DO_64 */
  ST_IDX_ZERO,	 /* MPR_OMP_BEGIN_PDO_64 */
  ST_IDX_ZERO,	 /* MPR_OMP_NEXT_ITERS_64 */
  ST_IDX_ZERO,	 /* MPR_OMP_END_PDO */
  ST_IDX_ZERO,	 /* MPR_OMP_BEGIN_SINGLE_PROCESS */
  ST_IDX_ZERO,	 /* MPR_OMP_END_SINGLE_PROCESS */
  ST_IDX_ZERO,	 /* MPR_OMP_BARRIER_OLD */
  ST_IDX_ZERO,	 /* MPR_OMP_PDO_ORDERED_BEGIN */
  ST_IDX_ZERO,	 /* MPR_OMP_PDO_ORDERED_END */
  ST_IDX_ZERO,	 /* MPR_OMP_ORDERED_BEGIN_OLD */
  ST_IDX_ZERO,	 /* MPR_OMP_ORDERED_END_OLD */
  ST_IDX_ZERO,	 /* MPR_OMP_COPYIN */
  ST_IDX_ZERO,	 /* MPR_OMP_NONPOD_COPYIN */
  ST_IDX_ZERO,	 /* MPR_OMP_NONPOD_ARRAY_COPYIN */
  ST_IDX_ZERO,	 /* MPR_OMP_IN_PARALLEL */
  ST_IDX_ZERO,   /* MPR_OMP_CAN_FORK */
  ST_IDX_ZERO,   /* MPR_OMP_SET_NUM_THREADS */
  ST_IDX_ZERO,   /* MPR_OMP_INIT_RTL */
  ST_IDX_ZERO,   /* MPR_OMP_FINI_RTL */
  ST_IDX_ZERO,   /* MPR_OMP_SERIALIZED_PARALLEL */
  ST_IDX_ZERO,   /* MPR_OMP_END_SERIALIZED_PARALLEL */
  ST_IDX_ZERO,   /* MPR_OMP_GET_THREAD_NUM */
  ST_IDX_ZERO,   /* MPR_OMP_GET_NUM_THREADS */
  ST_IDX_ZERO,   /* MPR_OMP_FORK */
  ST_IDX_ZERO,   /* MPR_OMP_STATIC_INIT_4 */
  ST_IDX_ZERO,   /* MPR_OMP_STATIC_INIT_8 */
  ST_IDX_ZERO,   /* MPR_OMP_STATIC_FINI */
  ST_IDX_ZERO,   /* MPR_OMP_SCHEDULER_INIT_4 */
  ST_IDX_ZERO,   /* MPR_OMP_SCHEDULER_INIT_8 */
  ST_IDX_ZERO,   /* MPR_OMP_SCHEDULER_NEXT_4 */
  ST_IDX_ZERO,   /* MPR_OMP_SCHEDULER_NEXT_8 */
  ST_IDX_ZERO,   /* MPR_OMP_SINGLE */
  ST_IDX_ZERO,   /* MPR_OMP_END_SINGLE */
  ST_IDX_ZERO,   /* MPR_OMP_MASTER */
  ST_IDX_ZERO,   /* MPR_OMP_END_MASTER */
  ST_IDX_ZERO,   /* MPR_OMP_BARRIER */
  ST_IDX_ZERO,   /* MPR_OMP_CRITICAL */
  ST_IDX_ZERO,   /* MPR_OMP_END_CRITICAL */
  ST_IDX_ZERO,   /* MPR_OMP_ORDERED */
  ST_IDX_ZERO,   /* MPR_OMP_END_ORDERED */
  ST_IDX_ZERO,   /* MPR_OMP_FLUSH */
#ifdef KEY
  ST_IDX_ZERO,   /* MPR_OMP_GET_THDPRV */
  ST_IDX_ZERO,   /* MPR_OMP_COPYIN_THDPRV */
  ST_IDX_ZERO,   /* MPR_OMP_COPYPRIVATE */
#endif
};

#define MPSP_STATUS_PREG_NAME "mpsp_status"
#define IS_MASTER_PREG_NAME  "mp_is_master"


// Verify that tree contains no MP pragmas or IF's
void Verify_No_MP(WN *tree)
{
  WN_ITER *wni = WN_WALK_TreeIter(tree);

  for ( ; wni; wni = WN_WALK_TreeNext(wni)) {
    WN *wn = WN_ITER_wn(wni);
    OPERATOR opr = WN_operator(wn);

    if ((opr == OPR_PRAGMA || opr == OPR_XPRAGMA) &&
        WN_pragmas[WN_pragma(wn)].users & PUSER_MP)
      Fail_FmtAssertion("Verify_MP_Lowered: unlowered MP pragma %d, "
          "node %#lx, tree %#lx", WN_pragma(wn), (unsigned long) wn,
          (unsigned long) tree);

    if (opr == OPR_IF && WN_Is_If_MpVersion(wn))
      Fail_FmtAssertion("Verify_MP_Lowered: unlowered MP IF, node %#lx, "
          "tree %#lx", (unsigned long) wn, (unsigned long) tree);

    BOOL first_and_last;
    if (Is_Nonpod_Finalization_IF(wn, &first_and_last))
      Fail_FmtAssertion("Verify_MP_Lowered: unlowered non-POD finalization "
                        "IF, node %#lx, tree %#lx",
			(unsigned long) wn, (unsigned long) tree);
  }
}


/* Forward function declarations. */

static WN * Gen_MP_Load_Store ( ST * from_st, WN_OFFSET from_offset,
				ST * to_st,   WN_OFFSET to_offset,
				BOOL is_dynamic );

static BOOL Is_NameLock_ST(ST *st);

ST_IDX Make_MPRuntime_ST ( MPRUNTIME rop );

#define GET_MPRUNTIME_ST(x) (mpr_sts[x] == ST_IDX_ZERO ? \
                             Make_MPRuntime_ST(x) : mpr_sts[x])


/*
Utility class for temporarily changing the line_number global variable.
When an object of this class is created, line_number is set to the value
given in the constructor; when the object is destroyed, line_number is
restored to the value it had before the object was created.
*/

class Linenum_Pusher {
  INT64 old_line_number;
public:
  Linenum_Pusher(INT64 new_line_number) {
    if (new_line_number <= 0)
      Fail_FmtAssertion(
        "Linenum_Pusher::Linenum_Pusher() : invalid line number %lld",
        new_line_number);
    old_line_number = line_number;
    line_number = new_line_number;
//    DevWarn("setting line_number to %ld", new_line_number);
  }
  ~Linenum_Pusher() { line_number = old_line_number; }
};

/*
A parameter of Gtid is needed by most function calls to RTL, so the 
calls to get global thread number is needed right after __omp_begin.
The use of gtid is not only meaningful for nested OMP regions. For every
working thread, the gtid is passed by RTL as a parameter at the entrance
of thread function.
So, currently, all gtid out of threads' scope is set to 0, for currently
nested parallelism is not supported. This should be fixed in the future.
now, use local_gtid instead of gtid_st, for the reason of unification of
Transform_Do.
*/
static ST *
Create_Gtid_ST( void )
{
  if( local_gtid != NULL)
	  return local_gtid;
      // the usage of gtid is not so frequently, so create it as a global var
  local_gtid = New_ST(CURRENT_SYMTAB);
  ST_Init(local_gtid, Save_Str("__ompv_gtid_s1"), CLASS_VAR, SCLASS_AUTO, 
                   EXPORT_LOCAL, MTYPE_To_TY(MTYPE_I4));
  return local_gtid;
}

/*
Create code to get gtid
*/
static WN *
Get_Gtid(ST * gtid)
{
     // if gtid == NULL, then set gtid = 0, else load the var gtid
     // Some RT calls do require a true gtid rather than 0. e.g., barrier.
  if( gtid == NULL)
  {
     return  WN_CreateIntconst( OPC_I4INTCONST, 0); 
  }
  else
  {
     return  WN_Ldid( MTYPE_I4, 0, gtid, ST_type( gtid ));
  }
}

/*
The lock type of Intel RTL is an I4 array of size 8. But where to put it?
Does every named lock need a different lock? Remained to be tested.
*/
#if defined(TARG_MIPS) && !defined(TARG_SL)
BOOL Is_Target_32bit () 
{ 
  return (Target_ABI == ABI_N32); 
}
#endif

static void 
Create_Lock_Type()
{
    if( lock_ty_idx != TY_IDX_ZERO )
      return;
#ifdef KEY
#ifdef TARG_X8664
    if (Is_Target_32bit())
      lock_ty_idx = MTYPE_To_TY(MTYPE_I4);
    else
#endif
      lock_ty_idx = MTYPE_To_TY(MTYPE_I8);
#else
       // define lock_ty_idx as an I4 array of size 8
    TY& ty = New_TY(lock_ty_idx);
    TY_Init(ty, 8*TY_size(MTYPE_To_TY(MTYPE_I4)), KIND_ARRAY,
                   MTYPE_UNKNOWN, Save_Str("NAME_LOCK_TY"));
    Set_TY_etype(ty, Be_Type_Tbl(MTYPE_I4));

    ARB_HANDLE arb = New_ARB();
    ARB_Init(arb, 0, 7, 1);
    Set_ARB_dimension(arb,1);
    Set_ARB_first_dimen(arb);
    Set_ARB_last_dimen(arb);
    Set_ARB_const_lbnd(arb);
    Set_ARB_lbnd_val(arb, 0);
    Set_ARB_const_ubnd(arb);
    Set_ARB_ubnd_val(arb, 7);
    Set_ARB_const_stride(arb);
    Set_ARB_stride_val(arb, 1);

    Set_TY_arb(ty, arb);
    Set_TY_align(lock_ty_idx, 8*TY_size(MTYPE_To_TY(MTYPE_I4)));
#endif 
}


/*
Generate RT calls to judge if current threads in a parallel region, 
replace original calls to intrisic function mp_in_parallel_region.
*/
static WN *
Gen_In_Parallel(void)
{
  WN *wn = WN_Create(OPC_I4CALL, 0);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_IN_PARALLEL);
  
  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;
  
  return wn;
}

static UINT64
Get_Offset_From_Const_Array(WN* array)
{
  // should be one-dimension array with constant index.
  WN* index = WN_array_index(array,0);
  FmtAssert(WN_operator(index) == OPR_INTCONST,("expect a const index"));
  UINT64 offset = WN_element_size(array) * WN_const_val(index);
  return offset;
}

// Return a non-structure field from offset, if there's multiple field
// with the same offset, return the first.
// This routine can return empty fld handler.
static FLD_HANDLE 
Get_FLD_From_Offset_r(const TY_IDX ty_idx, const UINT64 offset, UINT* field_id)
{
  Is_True(Is_Structure_Type(ty_idx), ("need to be a structure type"));

  UINT64 cur_offset = 0;

  FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty_idx));
  do {
    if ( field_id != NULL )
      (*field_id) ++;
    FLD_HANDLE fld(fld_iter);       

    // we assume that we will not see bit-fields here.
    cur_offset = FLD_ofst(fld);

    if (cur_offset == offset)
    {
      // check type
      TY_IDX cur_fld_idx = FLD_type(fld);
      if (!Is_Structure_Type(cur_fld_idx))
        return fld;
    }

    TY_IDX cur_fld_idx = FLD_type(fld);
    if (TY_kind(cur_fld_idx) == KIND_STRUCT &&
        TY_fld(cur_fld_idx) != FLD_HANDLE())
    {
      // it's possible that the new_offset becomes negative
      // because of unions. 
      INT64 new_offset = offset - cur_offset;
      if (new_offset < 0) return FLD_HANDLE();
      FLD_HANDLE fld1 = Get_FLD_From_Offset_r(cur_fld_idx, new_offset, field_id);
      if (!fld1.Is_Null()) return fld1;
    }

  } while (!FLD_last_field(fld_iter++));

  return FLD_HANDLE();
}

// Return a non-structure field from offset, if there's multiple field
// with the same offset, return the first.
// This routine will assert if it cannot find a valid field.
static FLD_HANDLE 
Get_FLD_From_Offset(const TY_IDX ty_idx, const UINT64 offset, UINT *field_id= NULL)
{
  if (field_id != NULL)
	  *field_id= 0;
  FLD_HANDLE fld = Get_FLD_From_Offset_r(ty_idx, offset, field_id);
  FmtAssert(!fld.Is_Null(),("cannot find field from offset"));
  return fld;
}

/*
Generate RT calls to judge if OK to fork. This call should be invocated 
before real fork calls. Original calls to __omp_region do both the judge
and fork job.
*/
static WN *
Gen_Can_Fork(void)
{
  WN *wn = WN_Create(OPC_I4CALL, 0);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_CAN_FORK);

  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;

  return wn;
}

/*
Generate RT calls to set thread number, obsoleted, function
Move to fork.
*/
static WN *
Gen_Set_Num_Threads( ST * gtid, WN *nThreads)
{
  WN *wn = WN_Create(OPC_VCALL, 2);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_SET_NUM_THREADS);

  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;

  WN_kid(wn, 0) = WN_CreateParm(MTYPE_I4, Get_Gtid( gtid ),
			Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE);
  WN_kid(wn, 1) = WN_CreateParm(MTYPE_I4, nThreads, Be_Type_Tbl(MTYPE_I4), 
                        WN_PARM_BY_VALUE);

  return wn;
}

/*
Generate RT calls to fork working threads. All fork jobs is done in
the same way. Original implementation uses different strategy
for forking of Parallel region and Parallel DO.
*/
static WN *
Gen_Fork (ST *proc, WN *nThreads) 
{
  WN * wn;
  WN * wnx;
  wn = WN_Create(OPC_VCALL, 3 );
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_FORK);

  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Non_Parm_Ref(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;

  if (nThreads != NULL)
  {
	  /* num_threads subclause exist for parallel */
	  WN_kid(wn, 0) = WN_CreateParm(MTYPE_I4, nThreads, 
			  Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE);
  }
  else
  {
	  /* should be set to 0 */
	  WN_kid(wn, 0) = WN_CreateParm(MTYPE_I4, WN_Intconst (MTYPE_I4, 0),
			  Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE);
  }

  wnx = WN_Lda( Pointer_type, 0, proc);
  WN_kid(wn, 1) = WN_CreateParm(Pointer_type, wnx, 
                       WN_ty(wnx), WN_PARM_BY_REFERENCE);
  WN *link = WN_LdidPreg( Pointer_type, Frame_Pointer_Preg_Offset);
  WN_kid(wn, 2) = WN_CreateParm(Pointer_type, link, WN_ty(link),
                     WN_PARM_BY_REFERENCE);

  return wn;
}

/*
Generate RT calls to initialize RTL. This call should be invocated at the 
beginning of the program. For Profiling only. obsoleted.
*/
static WN *
Gen_Init_RTL ()
{
  WN *wn = WN_Create(OPC_VCALL, 0);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_INIT_RTL);

  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
    //maybe it's not a proper linenum setting
  WN_linenum(wn) = line_number;

  return wn;
}
 
/*
Generate RT calls to finalize RTL. This call should be invocated at the end
of the program. For Profiling only. obsoleted.
*/
static WN *
Gen_Fini_RTL ()
{
  WN *wn = WN_Create(OPC_VCALL, 0);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_FINI_RTL);

  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
    //maybe it's not a proper linenum setting
  WN_linenum(wn) = line_number;

  return wn;
}

/*
Generate RT calls to get thread no.
*/
static WN *
Gen_Get_Thread_Num ()
{
  WN *wn = WN_Create(OPC_I4CALL, 0);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_GET_THREAD_NUM);

  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;

  return wn;
}

/*
 * Fill the gtid variable with proper thread no.
 * Ideally, this gtid should be in TLS. csc.
 */
 
static WN *
Gen_Store_Gtid ()
{
  WN *block = WN_CreateBlock( );
  if( local_gtid == NULL )
  {
    Create_Gtid_ST( );
  }
  WN *wn = Gen_Get_Thread_Num();
  WN_INSERT_BlockLast( block, wn );
  PREG_NUM   rreg1, rreg2;
  GET_RETURN_PREGS(rreg1, rreg2, MTYPE_I4);
  wn = WN_Stid( MTYPE_I4, 0, local_gtid, ST_type( local_gtid ),
             WN_LdidPreg ( MTYPE_I4, rreg1 ));
  WN_linenum(wn) = line_number;
  WN_INSERT_BlockLast( block, wn );
  
  return block;
}

/*
* The usable version of Set number of threads. obsoleted.
*/
static WN *
Set_Thread_Num( WN *nThreads )
{
  WN *block = WN_CreateBlock();
  WN_INSERT_BlockLast( block, Gen_Store_Gtid());
  WN_INSERT_BlockLast( block, 
      Gen_Set_Num_Threads( local_gtid, nThreads ));

  return block;
}

/*
Generate RT calls to get number of total threads.
*/
static WN *
Gen_Get_Num_Threads()
{
  WN *wn = WN_Create(OPC_I4CALL, 0);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_GET_NUM_THREADS);

  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;

  return wn;
}

/*
Generate RT calls to serialize parallel region
KEY: gtid is not used, the function call does not take any argument

The OMP_SERIALIZED library calls are made to inform the runtime library
that we are "inside" a parallel region although we are executing the 
optimized serial version of the parallel region. Do this if we are really
"inside" the parallel region and not if "if (false)" prohibits parallel
execution (bug 5467).
*/

#if defined(KEY) && defined(Is_True_On)
// debug variable
static BOOL serialized_parallel_in_cond = FALSE;
#endif // KEY && Is_True_On

#ifdef KEY
static WN * serial_test;
#endif // KEY

static WN *
Gen_Serialized_Parallel (ST *gtid)
{
#ifdef KEY
  WN *wn = WN_Create(OPC_VCALL, 0);
#else
  WN *wn = WN_Create(OPC_VCALL, 1);
#endif
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_SERIALIZED_PARALLEL);
  
  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;

#ifdef KEY
  serial_test = NULL;

  if (if_node) // if_node is an XPRAGMA
  {
#ifdef Is_True_On
    Is_True (!serialized_parallel_in_cond, 
             ("Did we forget to end our previous serialized_parallel?"));

    serialized_parallel_in_cond = TRUE;
#endif // Is_True_On

    // clone if-compare tree
    WN * if_test = WN_COPY_Tree (WN_kid0 (if_node));

    // temporary symbol to store if-compare test
    ST * cmp = Gen_Temp_Symbol (MTYPE_TO_TY_array[WN_rtype (if_test)],
                                  "test_serial");
    
    WN * blk = WN_CreateBlock ();

    // Store compare result
    WN_INSERT_BlockLast (blk, WN_Stid (WN_rtype (if_test),
                                       0,
				       cmp,
				       ST_type (cmp),
				       if_test));

    // Load from temp var
    serial_test = WN_Ldid (WN_rtype (if_test), 0, cmp, ST_type (cmp));

    if_test = WN_COPY_Tree (serial_test);
    WN * if_then = WN_CreateBlock ();
    WN * if_else = WN_CreateBlock ();

    // Insert call node in then-part
    WN_INSERT_BlockLast (if_then, wn);

    WN * if_wn = WN_CreateIf (if_test, if_then, if_else);
    WN_linenum (if_wn) = line_number;

    WN_INSERT_BlockLast (blk, if_wn);
    return blk;
  }
#endif // KEY

#ifndef KEY
  WN_kid(wn, 0) = WN_CreateParm(MTYPE_I4, Get_Gtid( gtid ),
                     Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE);
#endif // !KEY

  return wn;
}
  
/*
Generate RT calls to end serialize parallel region
KEY: gtid is not used, the function call does not take any argument
See comments before Gen_Serialized_Parallel.
*/
static WN *
Gen_End_Serialized_Parallel( ST *gtid )
{
#ifdef KEY
  WN *wn = WN_Create(OPC_VCALL, 0);
#else
  WN *wn = WN_Create(OPC_VCALL, 1);
#endif
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_END_SERIALIZED_PARALLEL);
  
  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;

#ifdef KEY
  if (if_node) // if_node is an XPRAGMA
  {
#ifdef Is_True_On
    Is_True (serialized_parallel_in_cond, 
             ("Did we forget to start a serialized_parallel?"));

    serialized_parallel_in_cond = FALSE;
#endif // Is_True_On

    FmtAssert (serial_test, ("NULL compare statement"));

    WN * if_test = WN_COPY_Tree (serial_test);
    WN_Delete (serial_test);
    serial_test = NULL;

    WN * if_then = WN_CreateBlock ();
    WN * if_else = WN_CreateBlock ();

    WN_INSERT_BlockLast (if_then, wn);

    WN * if_wn = WN_CreateIf (if_test, if_then, if_else);
    WN_linenum (if_wn) = line_number;
    return if_wn;
  }
#endif // KEY

#ifndef KEY
  WN_kid(wn, 0) = WN_CreateParm(MTYPE_I4, Get_Gtid( gtid ),
                     Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE); 
#endif // !KEY

  return wn;
}

static WN *Gen_Master( ST *gtid );
static void Create_Preg_or_Temp ( TYPE_ID mtype, char *name, ST **st,
                                  WN_OFFSET *ofst );
static WN * Gen_MP_Load ( ST * st, WN_OFFSET offset, BOOL scalar_only = FALSE );
static WN * Gen_Barrier ( ST* gtid );

/*
* Generate RT calls to start critical section
*/
static WN *
Gen_Critical (ST *gtid, ST *lck)
{
  WN *wn = WN_Create(OPC_VCALL, 2);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_CRITICAL);
  
  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Parm_Ref(wn);
	WN_Set_Call_Parm_Mod(wn);
  WN_linenum(wn) = line_number;
  
  WN_kid(wn, 0) = WN_CreateParm( MTYPE_I4, Get_Gtid( gtid ),
                     Be_Type_Tbl( MTYPE_I4 ), WN_PARM_BY_VALUE ); 

  WN_kid(wn, 1) = WN_CreateParm( Pointer_type, 
                     WN_Lda( Pointer_type, 0, lck ),
                     Be_Type_Tbl( Pointer_type ), WN_PARM_BY_REFERENCE );

  WN *return_wn = NULL;

#ifndef KEY
  if( critical_lock_not_init == TRUE )
  {
    return_wn = WN_CreateBlock( );
    
    //initial lock = 0;

    WN *wn_master = Gen_Master( gtid);
    WN_linenum( wn_master ) = line_number;
    WN_INSERT_BlockLast( return_wn, wn_master );
    ST *return_st;
    WN_OFFSET return_ofst;
    PREG_NUM rreg1, rreg2;
    Create_Preg_or_Temp ( MTYPE_I4, "is_master", &return_st, &return_ofst );
    GET_RETURN_PREGS(rreg1, rreg2, MTYPE_I4);
    WN *wn_temp = WN_Stid (MTYPE_I4, return_ofst, return_st, ST_type(return_st),
                           WN_LdidPreg ( MTYPE_I4, rreg1 ));
    WN_linenum(wn_temp) = line_number;
    WN_INSERT_BlockLast( return_wn, wn_temp );
    
    //Create_IF    
    WN *test = WN_EQ(MTYPE_I4,
                     Gen_MP_Load( return_st, return_ofst ),
                     WN_CreateIntconst ( OPC_I4INTCONST, 1 ));
    WN *if_wn = WN_CreateIf(test,WN_CreateBlock(),WN_CreateBlock());
    WN_linenum( if_wn ) = line_number;

    //Init lock to 0
    //The following statement maybe wrong.
    //It does cause problem in compiling.
    Create_Lock_Type( );
    TY_IDX ptr_lck = Make_Pointer_Type( lock_ty_idx, FALSE );
    WN *lck_addr = WN_Lda( Pointer_type, 0, lck );

    WN *init_wn = WN_CreateMstore( 0, ptr_lck, 
                        WN_Intconst( MTYPE_I4, 0 ),
                        lck_addr,
                        WN_Intconst( MTYPE_I4, 32 ));

    WN_linenum( init_wn ) = line_number;
    WN_INSERT_BlockLast( WN_then( if_wn ), init_wn );

    WN_INSERT_BlockLast( return_wn, if_wn );
		WN_INSERT_BlockLast( return_wn, Gen_Barrier( gtid )); 
    WN_INSERT_BlockLast( return_wn, wn );
  }
  else
#endif
    return_wn = wn;

  return return_wn;
}

/*
* Generate RT calls to end critical section
*/
static WN *
Gen_End_Critical (ST *gtid, ST *lck)
{
  WN *wn = WN_Create(OPC_VCALL, 2);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_END_CRITICAL);
  
  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;
  
  WN_kid(wn, 0) = WN_CreateParm(MTYPE_I4, Get_Gtid( gtid ),
                     Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE); 
  WN_kid(wn, 1) = WN_CreateParm( Pointer_type, 
                     WN_Lda( Pointer_type, 0, lck ),
                     Be_Type_Tbl( Pointer_type ), WN_PARM_BY_REFERENCE );

  return wn;
}

/*
* Generate RT calls to start ordered
*/
static WN *
Gen_Ordered( ST *gtid )
{
  WN *wn = WN_Create(OPC_VCALL, 1);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_ORDERED);
  
  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;
  
  WN_kid(wn, 0) = WN_CreateParm(MTYPE_I4, Get_Gtid( gtid ),
                     Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE); 
  
  return wn;
}

/*
* Generate RT calls to end ordered
*/
static WN *
Gen_End_Ordered( ST *gtid )
{
  WN *wn = WN_Create(OPC_VCALL, 1);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_END_ORDERED);
  
  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;
  
  WN_kid(wn, 0) = WN_CreateParm(MTYPE_I4, Get_Gtid( gtid ),
                     Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE); 
  
  return wn;
}

/*
* Generate RT calls to begin master
*/
static WN *
Gen_Master (ST *gtid)
{
  WN *wn = WN_Create(OPC_I4CALL, 1);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_MASTER);
  
  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;
  
  WN_kid(wn, 0) = WN_CreateParm(MTYPE_I4, Get_Gtid( gtid ),
                     Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE); 
  
  return wn;
}

/*
* Generate RT calls to begin master
*/
static WN *
Gen_End_Master (ST *gtid)
{
  WN *wn = WN_Create(OPC_VCALL, 1);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_END_MASTER);
  
  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Non_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;
  
  WN_kid(wn, 0) = WN_CreateParm(MTYPE_I4, Get_Gtid( gtid ),
                     Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE); 
  
  return wn;
}

/*
* Generate a begin single RTL call.
* The first argument is only for compatibility usage.
* Remove it later.
* */
static WN * 
Gen_Single (WN * constructnum, 
            ST * gtid,
            BOOL is_omp)
{
  WN *wn;

#ifndef KEY
  if (is_omp) 
#else
  if (TRUE) 
#endif
  {
    wn = WN_Create ( OPC_I4CALL, 1 );
    WN_st_idx(wn) = GET_MPRUNTIME_ST ( MPR_OMP_SINGLE );

    WN_kid(wn, 0) = WN_CreateParm(MTYPE_I4, Get_Gtid( gtid ),
                     Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE); 
  } else {
    wn = WN_Create ( OPC_I4CALL, 1 );
    WN_st_idx(wn) = GET_MPRUNTIME_ST ( MPR_BEGIN_SINGLE_PROCESS );

    WN_kid(wn, 0) = WN_CreateParm (MTYPE_I4, constructnum, 
		    Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE);
  }
  WN_Set_Call_Non_Data_Mod ( wn );
  WN_Set_Call_Non_Data_Ref ( wn );
  WN_Set_Call_Non_Parm_Mod ( wn );
  WN_Set_Call_Non_Parm_Ref ( wn );
  WN_Set_Call_Parm_Ref ( wn );
  WN_linenum(wn) = line_number;

  return ( wn );
}

/*
* End_Single. not totally compliant with original one.
* I don't know the right way for Intel's RTL to handle nowait,
* Is it also to ignore the end calls? csc.
*/

static WN *
Gen_End_Single (WN * constructnum, 
                ST * gtid,
                BOOL is_omp, BOOL nowait)
{
  Is_True( is_omp || !nowait, ("no need for END_SINGLE_PROCESS runtime call"));

  WN *return_wn = WN_CreateBlock( );
  WN *wn;

#ifndef KEY
  if (is_omp) 
#else
  if (TRUE)
#endif
  {
    wn = WN_Create ( OPC_VCALL, 1 );
    WN_st_idx(wn) = GET_MPRUNTIME_ST ( MPR_OMP_END_SINGLE );

    WN_kid(wn, 0) = WN_CreateParm(MTYPE_I4, Get_Gtid( gtid ),
                     Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE); 
  } else {
    wn = WN_Create ( OPC_VCALL, 1 );
    WN_st_idx(wn) = GET_MPRUNTIME_ST ( MPR_END_SINGLE_PROCESS );
    WN_kid0(wn) = WN_CreateParm (MTYPE_I4, constructnum, Be_Type_Tbl(MTYPE_I4),
                                 WN_PARM_BY_VALUE);
  }
  WN_Set_Call_Non_Data_Mod ( wn );
  WN_Set_Call_Non_Data_Ref ( wn );
  WN_Set_Call_Non_Parm_Mod ( wn );
  WN_Set_Call_Non_Parm_Ref ( wn );
  WN_Set_Call_Parm_Ref ( wn );
  WN_linenum(wn) = line_number;

  WN_INSERT_BlockLast( return_wn, wn );

  if( is_omp && !nowait )
  {
    // I don't know whether the end_single call implies a barrier,
    // to ensure the correctness, I insert one here.
    // csc.
    WN_INSERT_BlockLast( return_wn, Gen_Barrier( gtid ));
  }

  return ( return_wn );
}

/*
* Flush thread modified data. envision it to the global scope.
* Currently, the f90fe and cfe translate FLUSH directives directly
* into VINTRINSIC_CALL<SYNCHRONIZE>, and the cg smooth this call
* away. So the OMP programs with FLUSH will not cause compilation
* error anyway, but the standard-required behavior may be biased,
* and for that, I've not observed and verified. So this part is
* remained untouched. A few more test is needed.
* TODO: rewrite the whole FLUSH framework. maybe we can simply
* translate the intrinsic call into the RTL one, but for the ZERO-
* ARG form of FLUSH, the FE should be adapted to a proper generation
* of the call. 2002.12.20
* csc.
* currently, all RTL calls to flush are generated for other constructs
*/
static WN *
Gen_Flush( ST *flush_var, WN_OFFSET flush_offset)
{
  WN *wn = WN_Create(OPC_VCALL, 1);
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_FLUSH);
  
  WN_Set_Call_Non_Data_Mod(wn);
  WN_Set_Call_Non_Data_Ref(wn);
  WN_Set_Call_Parm_Mod(wn);
  WN_Set_Call_Parm_Ref(wn);
  WN_linenum(wn) = line_number;
  
  WN_kid(wn, 0) = WN_CreateParm( Pointer_type, 
                     WN_Lda( Pointer_type, flush_offset, flush_var ),
                     Be_Type_Tbl( Pointer_type ), WN_PARM_BY_REFERENCE );
	// TODO: finish the flush call.
  return wn;
}
#ifdef KEY
static WN* 
Get_First_Stmt_in_Block(WN *block)
{
  WN *first = WN_first(block);
  OPERATOR opr = WN_operator( first );
  if ( opr == OPR_ALTENTRY ){
    while (opr != OPR_PRAGMA ||
           WN_pragma( first ) != WN_PRAGMA_PREAMBLE_END)
      first = WN_next(first);
    opr = WN_operator(first);
  }
  if (opr == OPR_PRAGMA &&
      WN_pragma( first ) == WN_PRAGMA_PREAMBLE_END){
    first = WN_next(first);
    opr = WN_operator(first);
  }
  if (opr == OPR_PRAGMA &&
      WN_pragma( first ) == WN_PRAGMA_START_STMT_CLUMP)
    first = WN_next(first);

  return first;
}
static ST*
Extract_Gtid_ST(WN *block)
{
  WN *wn = WN_first(block);
  while (wn){
    OPERATOR opr = WN_operator(wn);
    if (opr == OPR_STID){
      if (strcmp( ST_name(WN_st(wn)), "__ompv_temp_gtid") == 0)
        return WN_st(wn);
    }
    wn = WN_next(wn);
  }
  return NULL;
}

void 
Gen_Threadpriv_Func(WN* prags, WN* block, BOOL prepend)
{
  WN *blck = prags;
  WN *keep_prags; 
  WN *thrprv_assign;
  WN *wn;
  ST *gtid_st = NULL;
  BOOL need_thread_num = FALSE;

  BOOL target_32bit = Is_Target_32bit();

  const OPCODE uint_opc = (Pointer_Size == 4) ? OPC_U4INTCONST :
                         (Pointer_Size == 8) ? OPC_U8INTCONST : OPCODE_UNKNOWN;
  const mTYPE_ID uint_mtype = (uint_opc == OPC_U4INTCONST) ? MTYPE_U4 : MTYPE_U8;

  prags = WN_first(prags);

  while (prags) { 
    keep_prags = WN_next(prags);
    if (WN_opcode(prags) != OPC_PRAGMA ||
        WN_pragma(prags) != WN_PRAGMA_THREADPRIVATE) {
      prags = keep_prags;
      continue;
    }
    WN *stmt = WN_first(block);
    BOOL match = FALSE;
    while (WN_opcode(stmt) == OPC_VCALL &&
           WN_kid_count(stmt) == 4){
      if (WN_operator(WN_kid0(WN_kid2(stmt))) == OPR_LDA &&
          WN_st(WN_kid0(WN_kid2(stmt))) == WN_st(prags)) {
        match = TRUE;
        break;
      }
      stmt = WN_next(stmt);
    }
    if (match){
      prags = keep_prags;
      continue;
    }

    wn = WN_Create(OPC_VCALL, 4);
    WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_GET_THDPRV);
  
    WN_Set_Call_Non_Data_Mod(wn);
    WN_Set_Call_Non_Data_Ref(wn);
    WN_Set_Call_Parm_Mod(wn);
    WN_Set_Call_Parm_Ref(wn);
    WN_linenum(wn) = line_number;

    if (ST_is_not_used(ST_ptr(WN_pragma_arg2(prags))))
      Clear_ST_is_not_used(ST_ptr(WN_pragma_arg2(prags)));
    WN_kid(wn, 0) = WN_CreateParm( Pointer_type, 
                     WN_Lda( Pointer_type, 0, ST_ptr(WN_pragma_arg2(prags)) ),
                     Be_Type_Tbl( Pointer_type ), WN_PARM_BY_REFERENCE );

    WN_kid(wn, 1) = WN_CreateParm(MTYPE_I8, 
                     WN_Intconst (MTYPE_I8, ST_size(ST_ptr(WN_st_idx(prags)))),
                     Be_Type_Tbl(MTYPE_I8), WN_PARM_BY_VALUE); 

    if (ST_is_not_used(WN_st(prags)))
      Clear_ST_is_not_used(WN_st(prags));
    WN_kid(wn, 2) = WN_CreateParm( Pointer_type, 
                     WN_Lda( Pointer_type, 0, ST_ptr(WN_st_idx(prags)) ),
                     Be_Type_Tbl( Pointer_type ), WN_PARM_BY_REFERENCE );

//    if (prepend != TRUE)
//      gtid_st = Extract_Gtid_ST(block);

    if (!gtid_st){
      gtid_st = New_ST(CURRENT_SYMTAB);
      ST_Init(gtid_st, Save_Str("__ompv_gtid_s1"), CLASS_VAR, SCLASS_AUTO, 
                   EXPORT_LOCAL, MTYPE_To_TY(MTYPE_I4));
      need_thread_num = TRUE;
    }

    WN_kid(wn, 3) = WN_CreateParm( uint_mtype , 
                     Gen_MP_Load(gtid_st, 0),
                     Be_Type_Tbl( uint_mtype ), WN_PARM_BY_REFERENCE );

    WN *thrprv_block = WN_CreateBlock ();
    WN_INSERT_BlockLast(thrprv_block, wn);

    WN_OFFSET return_ofst = 0;

    ST *local_st = ST_ptr(WN_pragma_arg1(prags));
    ST *global_st = ST_ptr(WN_pragma_arg2(prags));
    TYPE_ID type = TY_mtype(TY_pointed(ST_type(global_st)));
    OPCODE load_opc = OPCODE_make_op(OPR_ILOAD,Pointer_type, Pointer_type);

    WN *test = WN_CIOR(WN_EQ(MTYPE_I4,
                             Gen_MP_Load( ST_ptr(WN_pragma_arg2(prags)), return_ofst ),
                             WN_CreateIntconst (target_32bit?OPC_U4INTCONST:OPC_U8INTCONST, 0)),
                       WN_EQ(MTYPE_I4,
                             WN_CreateIload(load_opc, 0,
                                            Be_Type_Tbl(Pointer_type),
                                            Make_Pointer_Type(Be_Type_Tbl(type),FALSE),
                                            WN_Add (uint_mtype,
                                                    WN_Ldid(Pointer_type, 0,
                                                            global_st,
                                                            ST_type(global_st)),
                                                    WN_Mpy (uint_mtype,
                                                            WN_Ldid(MTYPE_I4, 0,
                                                                    gtid_st,
                                                                    ST_type(gtid_st)),
                                                            WN_CreateIntconst (uint_opc,
                                                                               target_32bit?4:8)
                                                            )
                                                    )
                                             ),
                             WN_CreateIntconst (target_32bit?OPC_U4INTCONST:OPC_U8INTCONST, 0)
                             )
                       );
    WN *thrprv_test = WN_CreateIf( test, thrprv_block, WN_CreateBlock());



    thrprv_assign = WN_Stid(Pointer_type, 0, 
                            ST_ptr(WN_pragma_arg1(prags)), 
                            ST_type(local_st), 
                            WN_CreateIload(load_opc, 0,
                                           Be_Type_Tbl(Pointer_type),
                                           Make_Pointer_Type(Be_Type_Tbl(type),FALSE),
                                           WN_Add (uint_mtype, 
                                                   WN_Ldid(Pointer_type, 0, 
                                                           global_st, 
                                                           ST_type(global_st)),
                                                   WN_Mpy (uint_mtype,
                                                           WN_Ldid(MTYPE_I4, 0,
                                                                   gtid_st,
                                                                   ST_type(gtid_st)),
                                                           WN_CreateIntconst (uint_opc, 
                                                                              target_32bit?4:8)))));
                                     
    if (prepend != TRUE) {
      WN_INSERT_BlockLast(block, thrprv_test);
      WN_INSERT_BlockLast(block, thrprv_assign);
      wn = NULL;
    }
    else{
      wn = Get_First_Stmt_in_Block(block);
      WN_INSERT_BlockBefore(block, wn, thrprv_test);
      WN_INSERT_BlockBefore(block, wn, thrprv_assign);
    }
    WN_DELETE_FromBlock(blck, prags);

    prags = keep_prags;
  }
  if (need_thread_num){
    if (prepend == TRUE){
      if (!wn || !WN_prev(wn) || !WN_prev(WN_prev(wn)))
        Fail_FmtAssertion("cannot find a place to insert threadprivate");
      wn = WN_prev(WN_prev(wn));
    }
    else{
      wn = Get_First_Stmt_in_Block(block);
      wn = WN_next(WN_next(wn));
    }
    WN_INSERT_BlockBefore( block, wn,  Gen_Get_Thread_Num() );
    PREG_NUM   rreg1, rreg2;
    GET_RETURN_PREGS(rreg1, rreg2, MTYPE_I4);
    WN* wnx = WN_Stid( MTYPE_I4, 0, gtid_st, ST_type( gtid_st ),
             WN_LdidPreg ( MTYPE_I4, rreg1 ));
    WN_INSERT_BlockBefore( block, wn, wnx );
  }
    
}
#endif

/*
* When switching scope, use this call to save some global vars.
* When the scope is switched back, use Pop_Some_Globals to restore them.
* csc.
*/
static void 
Push_Some_Globals( )
{

  old_gtid_st = local_gtid;

}

/*
* Restore globals.
* csc.
*/
static void 
Pop_Some_Globals( )
{

	// TODO: when enable true nested-parallelism, 
	// a stack style pop/push should be implemented.
  local_gtid = old_gtid_st;
  old_gtid_st = NULL;

}

/*
* Initial PU-wise Globals. need to be called in LowerMP_PU_Init.
* csc.
*/
static void 
Init_PU_Globals( )
{

  local_gtid = NULL;
}

/*
Create a DST entry for a local variable in either the parent subprogram or
the nested subprogram.
*/

static void 
Add_DST_variable ( ST *st, DST_INFO_IDX parent_dst, 
			       INT64 line_number, DST_INFO_IDX type_idx )
{
  DST_INFO      *info;
  DST_INFO_IDX  dst, child_idx;
  DST_ATTR_IDX attr_idx;
  DST_ASSOC_INFO *assoc;
  DST_BASETYPE *attr;
  USRCPOS       srcpos;
  INT32         typesize;
  static DST_INFO_IDX int32_idx = {DST_INVALID_BLOCK_IDX, DST_INVALID_BYTE_IDX};
  static DST_INFO_IDX int64_idx = {DST_INVALID_BLOCK_IDX, DST_INVALID_BYTE_IDX};
  DST_INFO_IDX  int_idx;
  DST_IDX	cmp;
  char         *name;

  /* don't do anything if without -g option */
  if (Debug_Level == 0)
    return;

  if (DST_IS_NULL( type_idx )) {
    /* For variables which do not exist in the original program, there
    ** are no type information for them.  We need to search in the DST
    ** tree for the corresponding type entry
    */
    if (TY_kind(ST_type(st)) == KIND_POINTER) {
      typesize = TY_size(TY_pointed(ST_type(st)));
    } else {
      typesize = TY_size(ST_type(st));
    }

    const char *int_name1, *int_name2, *int_name3;
    DST_INFO_IDX *int_idx_p;

    switch (typesize) {
    case 4:
      int_name1 = "int"; int_name2 = "INTEGER*4"; int_name3 = "INTEGER_4";
      int_idx_p = &int32_idx;
      break;
    case 8:
#ifndef KEY
      int_name1 = "long long"; 
#else
      // Bug 7287 - C and C++ programs have "long long int" and 
      // "long long unsigned int" as basetype, but no "long long".
      int_name1 = "long long int";
#endif
      int_name2 = "INTEGER*8"; int_name3 = "INTEGER_8";
      int_idx_p = &int64_idx;
      break;
    default:
      Fail_FmtAssertion("can't handle typesize == %d", (INT) typesize);
    }

    if (DST_IS_NULL(*int_idx_p) ) {
      cmp = DST_get_compile_unit();
      info = DST_INFO_IDX_TO_PTR( cmp );
      attr_idx = DST_INFO_attributes( info );
      child_idx = DST_COMPILE_UNIT_first_child(
                  DST_ATTR_IDX_TO_PTR(attr_idx, DST_COMPILE_UNIT));
      while (!DST_IS_NULL(child_idx)) {
        info = DST_INFO_IDX_TO_PTR( child_idx );
        if (DST_INFO_tag( info ) == DW_TAG_base_type) {
          attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(info), DST_BASETYPE);
          name = DST_STR_IDX_TO_PTR( DST_FORMAL_PARAMETER_name(attr));
#ifdef KEY //bug 11848: name can not be null
         if(name == NULL)
            Is_True(0, ("Base type should have a name in a DST entry"));
          else
#endif
          if (!strcmp(name, int_name1) || !strcmp(name, int_name2) ||
              !strcmp(name, int_name3)) {
            *int_idx_p = child_idx;
            break;
          }
        }
        child_idx = DST_INFO_sibling(DST_INFO_IDX_TO_PTR(child_idx));
      }
      if (DST_IS_NULL(child_idx)) {
          // type not emitted by frontend, so we have to insert it
        *int_idx_p = DST_mk_basetype(int_name1, DW_ATE_signed, typesize);
        (void) DST_append_child(parent_dst, *int_idx_p);
      }
    }
    int_idx = *int_idx_p;

  }

  USRCPOS_srcpos(srcpos) = line_number;
  if (ST_sclass(st) == SCLASS_FORMAL_REF) {
    dst = DST_mk_formal_parameter( srcpos,
			ST_name( st ),
			int_idx,	/* type DST_IDX */
			ST_st_idx(st),	/* symbol */
			DST_INVALID_IDX,
			DST_INVALID_IDX,
			FALSE,
			FALSE,
			FALSE,   // is_artificial
			FALSE ); // is_declaration_only
    DST_SET_deref( DST_INFO_flag( DST_INFO_IDX_TO_PTR (dst) ) );
  } else
    dst = DST_mk_variable( srcpos,
			ST_name( st ),
			int_idx, 	/* type DST_IDX */
			0 /* offset */,
			ST_st_idx(st),	/* symbol */
			DST_INVALID_IDX,
			FALSE,		/* memory allocated */
			TRUE, 		/* parameter has sc_auto */
			FALSE,		/* sc_extern || sc_unspecified */
			FALSE);		/* is_artificial */

  (void)DST_append_child( parent_dst, dst );
}


extern DST_IDX 
Find_DST_From_ST ( ST *st, PU_Info *pu_info )
{
  DST_INFO_IDX	dst, child_idx;
  DST_INFO      *info;
  INT32		level, index;
  DST_ASSOC_INFO *assoc;
  DST_DW_tag	tag;
  DST_flag      flag;
  DST_ATTR_IDX	iattr;

  level = ST_level(st);
  index = ST_index(st);
  dst = PU_Info_pu_dst( pu_info );
  /* Go through the list of children in the parent subprogram to find the
  ** one that points to this ST entry
  */
  info = DST_INFO_IDX_TO_PTR( dst );
  iattr = DST_INFO_attributes( info );
  child_idx = DST_SUBPROGRAM_def_first_child(
		DST_ATTR_IDX_TO_PTR(iattr, DST_SUBPROGRAM));
  while (!DST_IS_NULL(child_idx)) {
    info = DST_INFO_IDX_TO_PTR( child_idx );
    tag = DST_INFO_tag(info);
    flag = DST_INFO_flag(info);
    iattr = DST_INFO_attributes(info);
    level = ST_level(st);
    index = ST_index(st);
    switch (tag) 
    {
      case DW_TAG_formal_parameter:
        assoc = &DST_FORMAL_PARAMETER_st(
		DST_ATTR_IDX_TO_PTR(iattr, DST_FORMAL_PARAMETER));
	break;
      case DW_TAG_variable:
        assoc = &DST_VARIABLE_def_st(
		DST_ATTR_IDX_TO_PTR(iattr, DST_VARIABLE));
	break;
      default:
	goto next;
    }
    if (pDST_ASSOC_INFO_st_level(assoc) == level 
	&& pDST_ASSOC_INFO_st_index(assoc) == index) {
        // PV 644324: F90 arrays with dope vectors have DST entries for
        // compiler-generated variables for array bounds that match in
        // level and index but have no name: search for entry with valid name
      DST_INFO_IDX name_idx;

      if (tag == DW_TAG_formal_parameter) {
        name_idx = DST_FORMAL_PARAMETER_name(DST_ATTR_IDX_TO_PTR(iattr,
                                                DST_FORMAL_PARAMETER));
      } else if (tag == DW_TAG_variable) {
        DST_VARIABLE *vattr = DST_ATTR_IDX_TO_PTR(iattr, DST_VARIABLE);

        name_idx = DST_IS_comm(flag) ? DST_VARIABLE_comm_name(vattr) :
                                       DST_VARIABLE_def_name(vattr);
      } else
        Fail_FmtAssertion("impossible tag == %d\n", (INT) tag);

      if (DST_IS_NULL(name_idx))
        goto next;

      return child_idx;
    }
next:
    child_idx = DST_INFO_sibling(DST_INFO_IDX_TO_PTR(child_idx));
  }
  return( DST_INVALID_IDX );
}


extern void 
Create_New_DST ( DST_INFO_IDX dst, ST *st , BOOL append_to_nested )
{
  DST_INFO      *info;
  DST_INFO_IDX  new_dst, type_idx;
  DST_ASSOC_INFO *assoc;
  DST_DW_tag	tag;
  DST_flag	flag;
  DST_ATTR_IDX	iattr;
  DST_VARIABLE  *vattr;
  DST_FORMAL_PARAMETER *fattr;
  USRCPOS       srcpos;
  char		*name;

  USRCPOS_srcpos(srcpos) = 0LL;
  info = DST_INFO_IDX_TO_PTR( dst );
  tag = DST_INFO_tag(info);
  iattr = DST_INFO_attributes(info);
  flag = DST_INFO_flag(info);
  type_idx = DST_INVALID_IDX;
  switch (tag) 
  {
    case DW_TAG_formal_parameter:
      fattr = DST_ATTR_IDX_TO_PTR(iattr, DST_FORMAL_PARAMETER);
      type_idx = DST_FORMAL_PARAMETER_type( fattr );
      name = DST_STR_IDX_TO_PTR( DST_FORMAL_PARAMETER_name( fattr ) );
      break;
    case DW_TAG_variable:
      if (DST_IS_comm(flag)) {
	vattr = DST_ATTR_IDX_TO_PTR(iattr, DST_VARIABLE);
	type_idx = DST_VARIABLE_comm_type( vattr );
	name = DST_STR_IDX_TO_PTR( DST_VARIABLE_comm_name( vattr ) );
      }
      else {
        vattr = DST_ATTR_IDX_TO_PTR(iattr, DST_VARIABLE);
        type_idx = DST_VARIABLE_def_type( vattr );
	name = DST_STR_IDX_TO_PTR( DST_VARIABLE_def_name( vattr ) );
      }
      break;
    default:
	Fail_FmtAssertion( "Unimplemented local MP variable kind" );
  }
  new_dst = DST_mk_variable( srcpos,
			name,
			type_idx, 	/* type DST_IDX */
			0 /* offset */,
			ST_st_idx(st),	/* symbol */
			DST_INVALID_IDX,
			FALSE,		/* memory allocated */
			TRUE, 		/* parameter has sc_auto */
			FALSE,		/* sc_extern || sc_unspecified */
			FALSE);		/* is_artificial */

  if (append_to_nested)
    (void)DST_append_child( nested_dst, new_dst );
  info = DST_INFO_IDX_TO_PTR( new_dst );
}


static void 
Create_Func_DST ( char * st_name )
{
  DST_INFO_IDX	dst = PU_Info_pu_dst( Current_PU_Info );
  DST_INFO	*info = DST_INFO_IDX_TO_PTR(dst);
  DST_ASSOC_INFO *assoc;
  USRCPOS       srcpos;

  USRCPOS_srcpos(srcpos) = line_number;
  nested_dst =  DST_mk_subprogram( srcpos,
			st_name,
			DST_INVALID_IDX,	/* return type */
			DST_INVALID_IDX,	/* for weak symbols */
			ST_st_idx(parallel_proc),
			DW_INL_not_inlined,
			DW_VIRTUALITY_none,
			0,
			FALSE,			/* declaration */
			FALSE,			/* prototype */
#ifdef KEY
                        FALSE,                  // is_artificial
#endif
			FALSE			/* external */
			);
  (void)DST_append_child( dst, nested_dst );
}


/*  Compare two PRAGMA nodes or XPRAGMA trees for equality.  */

static BOOL 
Identical_Pragmas ( WN * wn1, WN * wn2 )
{
  INT32 i;

  if ((WN_operator(wn1) != WN_operator(wn2)) ||
      (WN_pragma(wn1) != WN_pragma(wn2)) ||
      (WN_st(wn1) != WN_st(wn2)) ||
      (WN_pragma_flags(wn1) != WN_pragma_flags(wn2)) ||
      ((WN_operator(wn1) == OPR_PRAGMA) &&
       (WN_pragma_arg1(wn1) != WN_pragma_arg1(wn2))) || 
       (WN_pragma_arg2(wn1) != WN_pragma_arg2(wn2)) ||
      (WN_kid_count(wn1) != WN_kid_count(wn2)))
    return (FALSE);

  for (i = 0; i < WN_kid_count(wn1); i++)
    if (WN_Compare_Trees(WN_kid(wn1, i), WN_kid(wn2, i)) != 0)
      return (FALSE);

  return (TRUE);
}

/*
Because sometimes we may merely need a temp var, not a preg, e.g. the var
to accept a value as the function's formal parameter.
So, this function just allocate a temp var in current scope.
*/
static void 
Create_Temp( TYPE_ID mtype, const char *name, ST **st )
{
  ST *new_st;
  new_st = New_ST (CURRENT_SYMTAB);
  ST_Init (new_st,
           Save_Str2 ( "__ompv_temp_", name ),
           CLASS_VAR,
           SCLASS_AUTO,
           EXPORT_LOCAL,
           MTYPE_To_TY (mtype));
  Set_ST_is_temp_var ( new_st );
  *st = new_st;
}

#ifdef KEY
// If old_st_idx has already been localized, return the new
// st_idx. Else, create a new local symbol and return its st_idx.
static ST_IDX Localize_Symbol (ST_IDX old_st_idx, VAR_TABLE * v)
{
  for ( ; v->orig_st; v++) {
    if (ST_st_idx(*v->orig_st) == old_st_idx &&
        v->vtype == VAR_LOCAL) {
      return ST_st_idx(*v->new_st);
    }
  }

  ST_IDX new_st_idx = 0;

  ST * new_st = New_ST (CURRENT_SYMTAB);
  ST * old_st = &St_Table[old_st_idx];
  ST_Init (new_st,
           Save_Str (ST_name(old_st_idx)),
           ST_class(old_st),
           ST_sclass(old_st),
           ST_export(old_st),
           ST_type(old_st));
  new_st->flags = old_st->flags;
  new_st->flags_ext = old_st->flags_ext;

  return ST_st_idx(new_st);
}

// For C++ exception handling, the PU holds different exception
// handling-related information, generated by the front-end.
// old_inito is expected to hold the start of the info for the
// serial function.
// Clone the information (INITOs, INITVs, and local STs), and
// localize where necessary.
//
// TODO: The parallel function being created may not need all
// the typeinfos from the serial function, but currently we will
// have all of them in the typeinfo table for the parallel function.
static INITO_IDX
Process_PU_Exc_Info ( INITO_IDX old_inito, VAR_TABLE * vtab )
{
  INITO_IDX new_inito;
  ST *old_initst, *new_initst;
  INITV_IDX old_initv, new_initv, pinito, parent, prev;
  STACK<INITV_IDX> old_stack(Malloc_Mem_Pool), new_stack(Malloc_Mem_Pool);

  old_initst = INITO_st(old_inito);
  old_initv  = INITO_val(old_inito);

  // Table name (__EH_INFO_PER_PU__)
  new_initst = New_ST (CURRENT_SYMTAB);
  ST_Init (new_initst,
           Save_Str ( ST_name(old_initst) ),
           ST_class(old_initst),
           ST_sclass(old_initst),
           ST_export(old_initst),
           ST_type(old_initst));

  new_initst->flags = old_initst->flags;
  new_initst->flags_ext = old_initst->flags_ext;

  Set_ST_is_not_used(*old_initst);

  new_inito = New_INITO ( new_initst );

  pinito = new_inito;
  parent = 0;
  prev = 0;

  // __Exc_Ptr__, __Exc_Filter__
  for (INT i = 0; i < 2; i++)
  {
    (void) Initv_Table.New_entry(new_initv);
    INITV& new_initv_ref = Initv_Table[new_initv];
    INITV& old_initv_ref = Initv_Table[old_initv];
    if (pinito) {
      Set_INITO_val(pinito, new_initv);
      pinito = 0;
    } else if (prev) {
      Set_INITV_next(prev, new_initv);
    }
    INITVKIND k = INITV_kind(old_initv);
    Is_True (k == INITVKIND_VAL,
             ("Unexpected initv kind during PU exception processing"));
    INITO_IDX inito = 0;
    ST_IDX st = TCON_uval(INITV_tc_val(old_initv_ref));
    if (st)
      st = Localize_Symbol (st, vtab);
    INITV_Set_VAL(new_initv_ref, Enter_tcon (Host_To_Targ (MTYPE_U4, st)),
                  INITV_repeat2(old_initv_ref));
    old_initv = INITV_next(old_initv);
    prev = new_initv;
  }

  INITO_IDX old_inito_array[2] = {0, 0};
  INITO_IDX new_inito_array[2] = {0, 0};

  // __TYPEINFO_TABLE__, __EH_SPEC_TABLE__
  for (INT i = 0; i < 2; i++)
  {
    (void) Initv_Table.New_entry(new_initv);
    INITV& new_initv_ref = Initv_Table[new_initv];
    INITV& old_initv_ref = Initv_Table[old_initv];
    if (prev)
      Set_INITV_next(prev, new_initv);
    INITVKIND k = INITV_kind(old_initv);
    Is_True (k == INITVKIND_VAL,
             ("Unexpected initv kind during typeinfo/EH-spec processing"));

    INITO_IDX inito = 0;
    ST_IDX st = TCON_uval(INITV_tc_val(old_initv_ref));
    if (st)
    {
      inito = (INITO_IDX) st;
      st = Localize_Symbol (INITO_st_idx(Inito_Table[inito]), vtab);
      INITO_IDX new_inito = New_INITO (st, INITO_val(inito));

      INITV_Set_VAL(new_initv_ref, Enter_tcon (Host_To_Targ (MTYPE_U4,
                                                             new_inito)),
                    INITV_repeat2(old_initv_ref));
      old_inito_array[i] = inito;
      new_inito_array[i] = new_inito;
    }
    else
      INITV_Set_VAL(new_initv_ref, Enter_tcon (Host_To_Targ (MTYPE_U4, st)),
                    INITV_repeat2(old_initv_ref));
    old_initv = INITV_next(old_initv);
    prev = new_initv;
  }

  for (INT i = 0; i < 2; i++) {

    // i == 0 : info in typeinfo table
    // i == 1 : info in exception-specification table
    old_initv = old_inito_array[i] ? INITO_val(old_inito_array[i]) : 0;
    pinito = new_inito_array[i];
    parent = prev = 0;

    while ( old_initv ) {

      (void) Initv_Table.New_entry(new_initv);
      INITV& new_initv_ref = Initv_Table[new_initv];
      INITV& old_initv_ref = Initv_Table[old_initv];

      if (pinito) {
        Set_INITO_val(pinito, new_initv);
        pinito = 0;
      } else if (parent) {
        Set_INITV_blk(parent, new_initv);
        parent = 0;
      } else if (prev) {
        Set_INITV_next(prev, new_initv);
      }

      INITVKIND k = INITV_kind(old_initv);
      switch ( k ) {

      case INITVKIND_ZERO:
        INITV_Set_ZERO(new_initv_ref, INITV_mtype(old_initv_ref),
                       INITV_repeat2(old_initv_ref));
        old_initv = INITV_next(old_initv);
        prev = new_initv;
        break;

      case INITVKIND_ONE:
        INITV_Set_ONE(new_initv_ref, INITV_mtype(old_initv_ref),
                      INITV_repeat2(old_initv_ref));
        old_initv = INITV_next(old_initv);
        prev = new_initv;
        break;

      case INITVKIND_VAL:
      {
        ST_IDX st = TCON_uval(INITV_tc_val(old_initv_ref));
        INITV_Set_VAL(new_initv_ref, Enter_tcon (Host_To_Targ (MTYPE_U4, st)),
                      INITV_repeat2(old_initv_ref));
        old_initv = INITV_next(old_initv);
        prev = new_initv;
      }
      break;

      case INITVKIND_BLOCK:
        INITV_Set_BLOCK(new_initv_ref, INITV_repeat1(old_initv_ref), 0);
        old_stack.Push(old_initv);
        new_stack.Push(new_initv);
        old_initv = INITV_blk(old_initv);
        parent = new_initv;
        prev = 0;
        break;

      default:
        Fail_FmtAssertion ( "unexpected INITV kind %d", (INT) k );
      }

      while (!old_initv && old_stack.Elements() > 0) {
        old_initv = INITV_next(old_stack.Pop());
        prev = new_stack.Pop();
      }
    }
  }

  return new_inito;
}
#endif

static WN * Gen_MP_Store ( ST * st, WN_OFFSET offset, WN * value, 
			   BOOL scalar_only = FALSE );

/*
Create MicroTask for Working threads.  This includes creating the following:
the corresponding nested symbol table; entries for the TY, PU, and ST
tables; debugging information; PU_Info object; and Whirl tree.
Current_PU_Info is set to point to the new nested function, and the
parallel function's symtab becomes CURRENT_SYMTAB.
*/

static void 
Create_MicroTask ( PAR_FUNC_TYPE func_type )
{
  BOOL is_do32 = FALSE, is_do64 = FALSE, is_region = FALSE;
  switch (func_type) {  // validate input, set type flag
  case PAR_FUNC_DO32:
    is_do32 = TRUE;
    break;
  case PAR_FUNC_DO64:
    is_do64 = TRUE;
    break;
  case PAR_FUNC_REGION:
    is_region = TRUE;
    break;
  default:
    Fail_FmtAssertion("invalid parallel function type %d", (INT) func_type);
    break;
  }

    // should be merged up after done. Currently reserved for Debug
  const char *construct_type_str = is_region ? "ompregion" : "ompdo";
  char temp_str[64];


  // get function prototype

  TY_IDX &func_ty_idx = mpregion_ty;

  if  (func_ty_idx == TY_IDX_ZERO) {
    // create new type for function, and type for pointer to function

    TY& ty = New_TY(func_ty_idx);
    sprintf(temp_str, ".%s", construct_type_str);
    TY_Init(ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, Save_Str(temp_str));
    Set_TY_align(func_ty_idx, 1);

    TYLIST_IDX parm_idx;
    TYLIST& parm_list = New_TYLIST(parm_idx);
    Set_TY_tylist(ty, parm_idx);
    Set_TYLIST_type(parm_list, Be_Type_Tbl(MTYPE_V));  // return type

      // Two basic parameters
    Set_TYLIST_type(New_TYLIST(parm_idx), // gtid
                      Be_Type_Tbl(Pointer_type));
    Set_TYLIST_type(New_TYLIST(parm_idx), // btid
                      Be_Type_Tbl(Pointer_type));

    Set_TYLIST_type(New_TYLIST(parm_idx), TY_IDX_ZERO); // end of parm list

      // now create a type for a pointer to this function
    TY_IDX ptr_ty_idx;
    TY &ptr_ty = New_TY(ptr_ty_idx);
    sprintf(temp_str, ".%s_ptr", construct_type_str);
    TY_Init(ptr_ty, Pointer_Size, KIND_POINTER, Pointer_Mtype,
            Save_Str(temp_str));
    Set_TY_pointed(ptr_ty, func_ty_idx);
  }


  // generate new name for nested function

  INT32 mp_region_num;    // MP region number within parent PU
  INT32 mp_construct_num; // construct number within MP region

  if (mpnum_node)
    mp_region_num = WN_pragma_arg1(mpnum_node);
  else
      // should PAR regions and PAR DO's be numbered separately? -- DRK
    mp_region_num = ++(is_region ? region_id : do_id);
  mp_construct_num = mpid_table[mp_region_num]++;

  const char *old_st_name = ST_name(PU_Info_proc_sym(Current_PU_Info));
  char *st_name = (char *) alloca(strlen(old_st_name) + 32);
  if (mp_construct_num == 0)
    sprintf ( st_name, "__%s_%s%d", construct_type_str, old_st_name,
	      mp_region_num );
  else
    sprintf ( st_name, "__%s_%s%d.%d", construct_type_str, old_st_name,
	      mp_region_num, mp_construct_num );


  // create new PU and ST for nested function

  PU_IDX pu_idx;
  PU& pu = New_PU(pu_idx);
  PU_Init(pu, func_ty_idx, CURRENT_SYMTAB);

/*
Many questions of DRK's about flags:

is_pure and no_side_effects shouldn't be set due to output ref. parms?
does no_delete matter?
have no idea: needs_fill_align_lowering, needs_t9, put_in_elf_section,
  has_return_address, has_inlines, calls_{set,long}jmp, namelist
has_very_high_whirl and mp_needs_lno should have been handled already
is inheriting pu_recursive OK?
*/

  Set_PU_no_inline(pu);
  Set_PU_is_nested_func(pu);
  Set_PU_mp(pu);
#ifdef KEY
  Set_PU_mp_lower_generated(pu);
#endif // KEY
    // child PU inherits language flags from parent
  if (PU_c_lang(Current_PU_Info_pu()))
    Set_PU_c_lang(pu);
  if (PU_cxx_lang(Current_PU_Info_pu()))
    Set_PU_cxx_lang(pu);
  if (PU_f77_lang(Current_PU_Info_pu()))
    Set_PU_f77_lang(pu);
  if (PU_f90_lang(Current_PU_Info_pu()))
    Set_PU_f90_lang(pu);
  if (PU_java_lang(Current_PU_Info_pu()))
    Set_PU_java_lang(pu);

  Set_FILE_INFO_has_mp(File_info);  // is this true after MP lowerer?--DRK

  parallel_proc = New_ST(GLOBAL_SYMTAB);
  ST_Init(parallel_proc,
          Save_Str (st_name),
          CLASS_FUNC,
          SCLASS_TEXT,
          EXPORT_LOCAL,
          pu_idx);
  Set_ST_addr_passed(parallel_proc);

  Allocate_Object ( parallel_proc );


  // create nested symbol table for parallel function

  New_Scope(CURRENT_SYMTAB + 1,
            Malloc_Mem_Pool,  // find something more appropriate--DRK
            TRUE);
  csymtab = CURRENT_SYMTAB;
  func_level = CURRENT_SYMTAB;
  Scope_tab[csymtab].st = parallel_proc;

  Set_PU_lexical_level(pu, CURRENT_SYMTAB);

  Create_Func_DST ( st_name );


  // pre-allocate in child as many pregs as there are in the parent

  for (UINT32 i = 1; i < PREG_Table_Size(psymtab); i++) {
    PREG_IDX preg_idx;
    PREG &preg = New_PREG(csymtab, preg_idx);
      // share name with corresponding parent preg
    Set_PREG_name_idx(preg,
      PREG_name_idx((*Scope_tab[psymtab].preg_tab)[preg_idx]));
  }

    // create ST's for parameters

  ST *arg_gtid = New_ST( CURRENT_SYMTAB );
  ST_Init (arg_gtid,
             Save_Str ( "__ompv_gtid_a" ),
             CLASS_VAR,
             SCLASS_FORMAL,
             EXPORT_LOCAL,
             Be_Type_Tbl(MTYPE_I4));
  Set_ST_is_value_parm( arg_gtid );

  ST *arg_slink = New_ST( CURRENT_SYMTAB );
  ST_Init( arg_slink,
           Save_Str( "__ompv_slink_a" ),
           CLASS_VAR,
           SCLASS_FORMAL,
           EXPORT_LOCAL,
           Be_Type_Tbl( Pointer_type ));
  Set_ST_is_value_parm( arg_slink );

    // TODO: other procedure specific arguments should
    // be handled here.

  // create WHIRL tree for nested function

  parallel_func = WN_CreateBlock ( );
  reference_block = WN_CreateBlock ( );
#ifdef KEY
  WN *thread_priv_prag = WN_first(WN_func_pragmas(PU_Info_tree_ptr(Current_PU_Info)));
  if (thread_priv_prag) {
    while (thread_priv_prag) { 
      if (WN_opcode(thread_priv_prag) == OPC_PRAGMA &&
          WN_pragma(thread_priv_prag) == WN_PRAGMA_THREADPRIVATE) {
        WN_INSERT_BlockLast ( reference_block,
			WN_CreatePragma ( WN_PRAGMA_THREADPRIVATE,
                                          WN_st_idx(thread_priv_prag),
			                  WN_pragma_arg1(thread_priv_prag), 
                                          WN_pragma_arg2(thread_priv_prag) ));
      }
      thread_priv_prag = WN_next(thread_priv_prag);
    }
  }
#endif
  // Currently, don't pass data via arguments.
  WN *func_entry = WN_CreateEntry ( 2, parallel_proc,
                                    parallel_func, WN_CreateBlock ( ),
				    reference_block );

  WN_kid0(func_entry) = WN_CreateIdname ( 0, arg_gtid );
  WN_kid1(func_entry) = WN_CreateIdname ( 0, arg_slink );
     // TODO: variable arguments list should be added here.

  WN_linenum(func_entry) = line_number;

  // The arg_slink contains slink for caller. should put into a 
  // implied temp var.
  ST *slink = Gen_Temp_Symbol (MTYPE_To_TY(Pointer_type), "__slink_sym");

  Is_True( slink != NULL, ("The slink should not be NULL"));
  WN *wn_store_slink = Gen_MP_Store( slink, 0,
           WN_Ldid( Pointer_type, 0, arg_slink, ST_type( arg_slink ), 0 ));
  WN_linenum( wn_store_slink ) = line_number;
  WN_INSERT_BlockLast( parallel_func, wn_store_slink );

  // to unify the process of Transform_Do and provide the 
  // possibility to promote gtid to preg, we stroe the value
  // of gtid in another temp vars.
  // The gtid can be eniminated, since the symbol of arguments are also 
  // local storage.
  Create_Temp( MTYPE_I4, "gtid", &local_gtid );
  WN *wn_store_gtid = Gen_MP_Store( local_gtid, 0,
      WN_Ldid( MTYPE_I4, 0, arg_gtid, ST_type(arg_gtid), 0 ));
//      WN_IloadLdid( MTYPE_I4, 0, Be_Type_Tbl( MTYPE_I4 ), arg_gtid, 0 ));
  WN_linenum( wn_store_gtid ) = line_number;
  WN_INSERT_BlockLast( parallel_func, wn_store_gtid );    
  // create PU_Info for nested function


  PU_Info *parallel_pu = TYPE_MEM_POOL_ALLOC ( PU_Info, Malloc_Mem_Pool );
  PU_Info_init ( parallel_pu );
  Set_PU_Info_tree_ptr (parallel_pu, func_entry );

  PU_Info_proc_sym(parallel_pu) = ST_st_idx(parallel_proc);
  PU_Info_maptab(parallel_pu) = cmaptab = WN_MAP_TAB_Create(MEM_pu_pool_ptr);
  PU_Info_pu_dst(parallel_pu) = nested_dst;
  Set_PU_Info_state(parallel_pu, WT_SYMTAB, Subsect_InMem);
  Set_PU_Info_state(parallel_pu, WT_TREE, Subsect_InMem);
  Set_PU_Info_state(parallel_pu, WT_PROC_SYM, Subsect_InMem);
  Set_PU_Info_flags(parallel_pu, PU_IS_COMPILER_GENERATED);

  // don't copy nystrom points to analysis, alias_tag map
  // mp function's points to analysis will be analyzed locally.
  AliasAnalyzer *aa = AliasAnalyzer::aliasAnalyzer();
  if (aa) {
    // Current_Map_Tab is update to PU_Info_maptab(parallel_pu) in PU_Info_maptab
    Is_True(PU_Info_maptab(parallel_pu) == Current_Map_Tab,
        ("parallel_pu's PU's maptab isn't parallel_pu\n"));
    Current_Map_Tab = pmaptab;
    WN_MAP_Set_dont_copy(aa->aliasTagMap(), TRUE);
    WN_MAP_Set_dont_copy(WN_MAP_ALIAS_CGNODE, TRUE);
    Current_Map_Tab = PU_Info_maptab(parallel_pu);
  }
  else {
    Current_Map_Tab = pmaptab;
    WN_MAP_Set_dont_copy(WN_MAP_ALIAS_CGNODE, TRUE);
    Current_Map_Tab = PU_Info_maptab(parallel_pu);
  }

    // use hack to save csymtab using parallel_pu, so we can restore it
    // later when we lower parallel_pu; this is necessary because the
    // new symtab routines can't maintain more than one chain of symtabs
    // in memory at one time, and we lower the parent PU all the way to
    // CG before we lower any of the nested MP PUs
        // Save_Local_Symtab expects this
  Set_PU_Info_symtab_ptr(parallel_pu, NULL);
  Save_Local_Symtab(csymtab, parallel_pu);

  Is_True(PU_Info_state(parallel_pu, WT_FEEDBACK) == Subsect_Missing,
          ("there should be no feedback for parallel_pu"));
  if (Cur_PU_Feedback) {
#ifdef KEY
    parallel_pu_fb = CXX_NEW(FEEDBACK(func_entry,
                                      MEM_pu_nz_pool_ptr,
                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
                                      cmaptab),
                             MEM_pu_nz_pool_ptr);
#else
    parallel_pu_fb = CXX_NEW(FEEDBACK(func_entry,
                                      MEM_pu_nz_pool_ptr,
				      1, 1, 1, 1, 1, 1,
				      cmaptab),
		             MEM_pu_nz_pool_ptr);
#endif
    Set_PU_Info_state(parallel_pu, WT_FEEDBACK, Subsect_InMem);
    Set_PU_Info_feedback_ptr(parallel_pu, parallel_pu_fb);
        // Note that unlike every other kind of map, the FEEDBACK map for
        // the child PU is read and written by the MP lowerer. Therefore
        // we copy over all the relevant values here, and don't transfer
        // the parent FEEDBACK map at the end of MP lowerering.
    FB_Transfer(Cur_PU_Feedback, parallel_pu_fb, stmt_block);
  }

  RID *root_rid = RID_Create ( 0, 0, func_entry );
  RID_type(root_rid) = RID_TYPE_func_entry;
  Set_PU_Info_regions_ptr ( parallel_pu, root_rid );
  Is_True(PU_Info_regions_ptr(parallel_pu) != NULL,
	 ("Create_MicroTask, NULL root RID"));

  PU_Info *tpu = PU_Info_child(Current_PU_Info);

    // add parallel_pu after last child MP PU_Info item in parent's list
  if (tpu && PU_Info_state(tpu, WT_SYMTAB) == Subsect_InMem &&
      PU_mp(PU_Info_pu(tpu)) ) {
    PU_Info *npu;

    while ((npu = PU_Info_next(tpu)) &&
	   PU_Info_state(npu, WT_SYMTAB) == Subsect_InMem &&
	   PU_mp(PU_Info_pu(npu)) )
      tpu = npu;

    PU_Info_next(tpu) = parallel_pu;
    PU_Info_next(parallel_pu) = npu;
  } else {
    PU_Info_child(Current_PU_Info) = parallel_pu;
    PU_Info_next(parallel_pu) = tpu;
  }


  // change some global state; need to clean this up--DRK

  Current_PU_Info = parallel_pu;
  Current_pu = &Current_PU_Info_pu();
  Current_Map_Tab = pmaptab;

  Add_DST_variable ( arg_gtid, nested_dst, line_number, DST_INVALID_IDX );
  Add_DST_variable ( arg_slink, nested_dst, line_number, DST_INVALID_IDX );

}

/*  Create either a preg or a temp depending on presence of C++ exception
    handling.  */

static void 
Create_Preg_or_Temp ( TYPE_ID mtype, const char *name, ST **st,
				  WN_OFFSET *ofst )
{
  ST *new_st;

  if (!pu_has_eh) {
    *st = MTYPE_To_PREG ( mtype );
    *ofst = Create_Preg (mtype, name);
  } else {
    new_st = New_ST (CURRENT_SYMTAB);
    ST_Init (new_st,
             Save_Str2 ( "__ompv_temp_", name ),
             CLASS_VAR,
             SCLASS_AUTO,
             EXPORT_LOCAL,
             MTYPE_To_TY (mtype));
    Set_ST_is_temp_var ( new_st );
    *st = new_st;
    *ofst = 0;
  }
}


/*
If tree is the test for whether a thread is the MASTER, return TRUE, else
return FALSE.

The test for being the MASTER consists of checking whether a PREG called
thread_num is 0.  thread_num is set by code preceding the "if" to be the
result of calling omp_get_thread_num().
*/

static BOOL 
Is_Master_Test(WN *tree)
{
  if (WN_operator(tree) != OPR_EQ)
    return FALSE;

  WN *wn_ldid;

  if (WN_operator(WN_kid(tree, 0)) == OPR_LDID)
    wn_ldid = WN_kid(tree, 0);
  else if (WN_operator(WN_kid(tree, 1)) == OPR_LDID)
    wn_ldid = WN_kid(tree, 1);
  else
    return FALSE;

  WN *wn_intconst = (wn_ldid == WN_kid(tree, 0)) ? WN_kid(tree, 1) :
    WN_kid(tree, 0);

  if (WN_operator(wn_intconst) != OPR_INTCONST)
    return FALSE;

  if (WN_const_val(wn_intconst) != 1 ||
#ifdef KEY // bug 6282
      WN_class(wn_ldid) != CLASS_PREG ||
#endif
      Preg_Is_Dedicated(WN_offset(wn_ldid)) ||
      strcmp(IS_MASTER_PREG_NAME, Preg_Name(WN_offset(wn_ldid))) != 0)
    return FALSE;

  return TRUE;
}


/*
If tree is the test for whether a thread should enter a SINGLE section,
return TRUE, else return FALSE.

The test for entering a SINGLE consists of checking whether a PREG called
MPSP_STATUS_PREG_NAME is a non-0 value.  The PREG is set by code preceding
the "if" to be the result of calling omp_begin_single_process() or
mp_begin_single_process().
*/

static BOOL 
Is_Single_Test(WN *tree)
{
  if (WN_operator(tree) != OPR_LDID ||
#ifdef KEY // bug 5118
      WN_class(tree) != CLASS_PREG ||
#endif
      Preg_Is_Dedicated(WN_offset(tree)) ||
      strcmp(MPSP_STATUS_PREG_NAME, Preg_Name(WN_offset(tree))) != 0)

    return FALSE;

  return TRUE;
}


/*
If the root of tree is a SINGLE, MASTER, CRITICAL, or ATOMIC construct (and
therefore guards all the WNs below it), enter all nodes guarded by tree
into guarded_set; otherwise, do nothing.

Only recursive calls should use optional parameter tree_is_guarded.
*/

static void
Enter_Guarded_WNs(WN_TO_BOOL_HASH *guarded_set, WN *tree,
                  BOOL tree_is_guarded = FALSE) // only used for recursion
{
  if (tree_is_guarded) {
      // enter entire tree recursively
    guarded_set->Enter(tree, TRUE);

    if (!OPCODE_is_leaf(WN_opcode(tree))) {
      if (WN_opcode(tree) == OPC_BLOCK) {
        for (WN *kid = WN_first(tree); kid; kid = WN_next(kid))
          Enter_Guarded_WNs(guarded_set, kid, TRUE);
      } else {
        for (INT kidno = 0; kidno < WN_kid_count(tree); kidno++) {
          WN *kid = WN_kid(tree, kidno);
          if (kid)
            Enter_Guarded_WNs(guarded_set, kid, TRUE);
        }
      }
    }

    return;
  }

/*
The lowered WHIRL for the guarding MP constructs is as follows:

  MASTER:
    "then" block of "if" passes Is_Master_Test()
  SINGLE:
    "then" block of "if" passes Is_Single_Test()
  named CRITICAL:
    everything between matching OPC_VCALL(MPR_GETLOCK) and
      OPC_VCALL(MPR_UNLOCK) is guarded
  unnamed CRITICAL:
    everything between OPC_VCALL(MPR_SETLOCK) and OPC_VCALL(MPR_UNSETLOCK)
      is guarded
  ATOMIC:
    Lowered in 3 different ways.  One is as a CRITICAL section (handled as
    above).  The other two are as intrinsic calls, in which case we get
    help from a routine in the OMP Prelowerer.  Only the LDA for the scalar
    variable or array base being atomically updated needs to be guarded.
*/

  if (WN_operator(tree) == OPR_INTRINSIC_CALL) {
    WN *lda = Get_ATOMIC_Update_LDA(tree);
    if (lda) {
      guarded_set->Enter(lda, TRUE);
      return;
    }
  }

  OPCODE opc = WN_opcode(tree);
  ST_IDX end_st, end_name_st = ST_IDX_ZERO;

  switch (opc) {
  case OPC_IF:
    if (Is_Master_Test(WN_if_test(tree)) ||
        Is_Single_Test(WN_if_test(tree)) )
      Enter_Guarded_WNs(guarded_set, WN_then(tree), TRUE);
    return;
  case OPC_VCALL:
/* To modify (1)MPR_GETLOCK ---> MPR_CRITICAL 
             (2)MPR_UNLOCK ---> MPR_END_CRITICAL 
             (3)MPR_SETLOCK ---> MPR_CRITICAL
             (4)MPR_UNSETLOCK ---> MPR_END_CRITICAL
        By lg !
*/ 
/*
    if (WN_st_idx(tree) == GET_MPRUNTIME_ST(MPR_GETLOCK)) { // named CRITICAL
      end_st = GET_MPRUNTIME_ST(MPR_UNLOCK);
        // call has one PARM child that's an LDA of the lock var.
      end_name_st = WN_st_idx(WN_kid0(WN_kid0(tree)));
    } else if (WN_st_idx(tree) == GET_MPRUNTIME_ST(MPR_SETLOCK)) {
        // unnamed CRITICAL
      end_st = GET_MPRUNTIME_ST(MPR_UNSETLOCK);
*/
    if (WN_st_idx(tree) == GET_MPRUNTIME_ST(MPR_OMP_CRITICAL)) { 
    // named CRITICAL and unnamed CRITICAL
      end_st = GET_MPRUNTIME_ST(MPR_OMP_END_CRITICAL);
        // call has one PARM child that's an LDA of the lock var.
      end_name_st = WN_st_idx(WN_kid0(WN_kid0(tree)));
    } else
      return;
    break;
  default:
    return; // not a guarding construct
  }

  Is_True(opc == OPC_VCALL,
          ("should be looking for call that marks end of guarded code"));

    // add all nodes in CRITICAL section to guarded_set, warning if we
    // can't find the matching CRITICAL END
  DYN_ARRAY<WN *> nodes_in_critsect(Malloc_Mem_Pool);

  for (WN *guarded_wn = WN_next(tree); guarded_wn;
       guarded_wn = WN_next(guarded_wn)) {
    if (WN_opcode(guarded_wn) == OPC_VCALL &&
        WN_st_idx(guarded_wn) == end_st &&
        (!end_name_st || WN_st_idx(WN_kid0(WN_kid0(guarded_wn))) ==
	                           end_name_st) )
      break;  // found matching CRITICAL END
    nodes_in_critsect.AddElement(guarded_wn);
  }
//  if (!guarded_wn)
//    DevWarn("Enter_Guarded_WNs() did not find matching CRITICAL END");

  for (INT i = 0; i <= nodes_in_critsect.Lastidx(); i++)
    Enter_Guarded_WNs(guarded_set, nodes_in_critsect[i], TRUE);
}


/*  Walk tree to locate all uplevel references and build list of them.
    guarded_set is the set of WNs guarded by SINGLE or MASTER constructs;
    guared stores to non-SHARED STs do not elicit a warning message. */

static void 
Gather_Uplevel_References ( WN * block, INT32 level, WN * parent,
					WN * grandparent, WN * tree,
					WN_TO_BOOL_HASH *guarded_set )
{
  WN *wn;
  WN *node;
  INT32 i;
  ST *st;
  OPCODE op;
  OPERATOR opr;

  BOOL a_pointer;
  WN_PRAGMA_ACCESSED_FLAGS flags;

  Is_True(level >= 2, ("impossible symtab level == %d", level));

  if (tree) {

    Enter_Guarded_WNs(guarded_set, tree);

    op = WN_opcode(tree);
    opr = WN_operator(tree);

    if (op == OPC_LOOP_INFO)
      return;

    if (op == OPC_BLOCK)
      for (node = WN_first(tree); node; node = WN_next(node))
	Gather_Uplevel_References ( block, level, tree, parent, node,
	                            guarded_set );
    else
      for (i = 0; i < WN_kid_count(tree); i++)
	Gather_Uplevel_References ( block, level, tree, parent,
				    WN_kid(tree, i), guarded_set );

    if (OPCODE_has_sym(op) && (st = WN_st(tree)) != NULL &&
        ST_level(st) < level && ST_class(st) == CLASS_VAR) {

      a_pointer = (TY_kind(ST_type(st)) == KIND_POINTER);

      if (opr == OPR_LDID && !a_pointer)
         flags = ACCESSED_LOAD;
      else if (opr == OPR_STID)
         flags = ACCESSED_STORE;
      else if ((opr == OPR_LDA) && !a_pointer)
        if (WN_operator(parent) == OPR_ILOAD)
           flags = ACCESSED_LOAD;
        else if (WN_operator(parent) == OPR_ISTORE)
           flags = ACCESSED_STORE;
        else if (WN_operator(parent) == OPR_ARRAY)
	        if (WN_operator(grandparent) == OPR_ILOAD)
             flags = ACCESSED_LOAD;
          else if (WN_operator(grandparent) == OPR_ISTORE)
             flags = ACCESSED_STORE;
          else
            flags = (WN_PRAGMA_ACCESSED_FLAGS) (ACCESSED_LOAD |
                     ACCESSED_STORE |
                     ACCESSED_ILOAD |
                     ACCESSED_ISTORE);
        else
           flags = (WN_PRAGMA_ACCESSED_FLAGS) (ACCESSED_LOAD |
					      ACCESSED_STORE |
					      ACCESSED_ILOAD |
					      ACCESSED_ISTORE);
      else if ((opr == OPR_LDID) && a_pointer)
	      if (WN_operator(parent) == OPR_ILOAD)
          flags = (WN_PRAGMA_ACCESSED_FLAGS) (ACCESSED_LOAD |
					      ACCESSED_ILOAD);
        else if (WN_operator(parent) == OPR_ISTORE)
          flags = (WN_PRAGMA_ACCESSED_FLAGS) (ACCESSED_LOAD |
					      ACCESSED_ISTORE);
        else if (WN_operator(parent) == OPR_ARRAY)
          if (WN_operator(grandparent) == OPR_ILOAD)
             flags = (WN_PRAGMA_ACCESSED_FLAGS) (ACCESSED_LOAD |
                      ACCESSED_ILOAD);
          else if (WN_operator(grandparent) == OPR_ISTORE)
             flags = (WN_PRAGMA_ACCESSED_FLAGS) (ACCESSED_LOAD |
                      ACCESSED_ISTORE);
        else
             flags = (WN_PRAGMA_ACCESSED_FLAGS) (ACCESSED_LOAD |
                      ACCESSED_STORE |
                      ACCESSED_ILOAD |
                      ACCESSED_ISTORE);
      else
        flags = (WN_PRAGMA_ACCESSED_FLAGS) (ACCESSED_LOAD |
            ACCESSED_STORE |
            ACCESSED_ILOAD |
            ACCESSED_ISTORE);
    else
        flags = (WN_PRAGMA_ACCESSED_FLAGS) (ACCESSED_LOAD |
					    ACCESSED_STORE |
					    ACCESSED_ILOAD |
					    ACCESSED_ISTORE);

      wn = WN_first(block);
      while (wn && (WN_st(wn) < st))
          wn = WN_next(wn);
      if (wn && (WN_st(wn) == st))
         WN_pragma_arg2(wn) |= flags;
      else {
        const ST_SCLASS sclass = ST_sclass(st);

        if (sclass == SCLASS_AUTO || sclass == SCLASS_FORMAL_REF ||
            sclass == SCLASS_FORMAL || sclass == SCLASS_PSTATIC)
          Set_ST_has_nested_ref ( st );

	WN_INSERT_BlockBefore ( block, wn,
				WN_CreatePragma ( WN_PRAGMA_ACCESSED_ID, st, 0,
						  flags ));
      }

      if ((flags & ACCESSED_STORE) && (TY_kind(ST_type(st)) == KIND_SCALAR)) {
	for (i = 0; (i < shared_count) && (st != shared_table[i]); i++) { }
	if (i == shared_count) {
	  ST *split_blk, *common_blk = ST_Source_COMMON_Block(st, &split_blk);
	  BOOL is_threadprivate_common =
	    (ST_is_thread_private(st)) ||
	    (split_blk && ST_is_thread_private(split_blk)) ||
	    (common_blk && ST_is_thread_private(common_blk));

          if (!Is_NameLock_ST(st) &&
              !is_threadprivate_common &&
              !guarded_set->Find(tree) &&
#ifdef KEY
              strncmp(ST_name(st), "__thdprv", 8) &&
#endif
	            !comp_gen_construct) {
#ifdef KEY
            USRCPOS srcpos;
            USRCPOS_srcpos(srcpos) = WN_Get_Linenum(tree);
            Set_Error_Line(USRCPOS_linenum(srcpos));
#endif
            ErrMsg ( EC_MPLOWER_shared_store, st );
          }
	  shared_table[shared_count++] = st;
	}
      }

    }

    if (((op == OPC_U4INTRINSIC_CALL) &&
	 (WN_intrinsic(tree) == INTRN_U4I4ALLOCA)) ||
	((op == OPC_U8INTRINSIC_CALL) &&
	 (WN_intrinsic(tree) == INTRN_U8I8ALLOCA))) {
      Is_True(!Alloca_Dealloca_On,
              ("Alloca_Dealloca_On yet found INTRN_ALLOCA"));
      pu_has_alloca = TRUE;
    }

    if (opr == OPR_ALLOCA) {
      Is_True(Alloca_Dealloca_On,
              ("found OPR_ALLOCA yet not Alloca_Dealloca_On"));
      pu_has_alloca = TRUE;
    }

    if (op == OPC_REGION)
      pu_has_region = TRUE;

  }
}


/*  Walk tree gathering information about all interesting preg usage.  */

static void 
Walk_and_Info_Pregs ( WN * tree )
{
  WN *node;
  INT32 i;
  OPCODE op;

  if (tree) {

    op = WN_opcode(tree);
    if (op == OPC_BLOCK)
      for (node = WN_first(tree); node; node = WN_next(node))
	Walk_and_Info_Pregs ( node );
    else
      for (i = 0; i < WN_kid_count(tree); i++)
	Walk_and_Info_Pregs ( WN_kid(tree, i) );

    if (OPCODE_has_sym(op) && OPCODE_has_offset(op) && WN_st(tree) &&
#ifdef KEY // bug 11914: if OPC_PRAGMA, WN_offsetx will not return correct pnum
        op != OPC_PRAGMA &&
#endif
        (ST_class(WN_st(tree)) == CLASS_PREG) &&
        !Preg_Is_Dedicated(WN_offsetx(tree))) {
      PREG_IDX pnum = Get_Preg_Idx(WN_offsetx(tree));

      PREG_INFO *preg = &preg_info_table->at(pnum);

      if (preg->type == MTYPE_UNKNOWN)
	preg->type = ST_btype(WN_st(tree));
    }
  }
}


/*  Walk tree replacing livein/out parent pregs with temps.  */

static void
Walk_and_Replace_Pregs ( WN * tree )
{
  WN *node;
  INT32 i;
  OPCODE op;
  PREG_INFO *preg;

  if (tree) {

    op = WN_opcode(tree);
    if (op == OPC_BLOCK)
      for (node = WN_first(tree); node; node = WN_next(node))
	Walk_and_Replace_Pregs ( node );
    else
      for (i = 0; i < WN_kid_count(tree); i++)
	Walk_and_Replace_Pregs ( WN_kid(tree, i) );

    if (OPCODE_has_sym(op) && OPCODE_has_offset(op) && WN_st(tree) &&
	(ST_class(WN_st(tree)) == CLASS_PREG) &&
	!Preg_Is_Dedicated(WN_offsetx(tree)) ) {
      preg = &preg_info_table->at(Get_Preg_Idx(WN_offsetx(tree)));

      if (preg->temp && (preg->pclass != PCLASS_COPYIN_DEADOUT)) {
        WN_st_idx(tree) = ST_st_idx(preg->temp);
        WN_set_offsetx(tree, 0);
      }

    }
  }
}


/*
Translate a label in sequential code to an equivalent label in MP code. If
the equivalent MP label doesn't exist then create it.  For orphaned
constructs, just return the input label--no translation needed, since we
don't copy the MP code or move it into a nested scope.
*/

// scrutinize all uses of St_Table and Label_Table to make sure they
// refer to the right symtab level -- DRK

static LABEL_IDX 
Translate_Label ( LABEL_IDX plabel_idx )
{
  if (plabel_idx == LABEL_IDX_ZERO)
    return LABEL_IDX_ZERO;

  if (mpt == MPP_ORPHANED_SINGLE || mpt == MPP_ORPHANED_PDO)
    return plabel_idx;

  LABEL_IDX clabel_idx = label_info_table[LABEL_IDX_index(plabel_idx)];

  if (clabel_idx == LABEL_IDX_ZERO) {
      // remove when symtab-setting stuff gets cleaned up--DRK
    Is_True(csymtab != SYMTAB_IDX_ZERO, ("child symtab not created yet"));
    Is_True(psymtab != SYMTAB_IDX_ZERO, ("psymtab not set yet"));
    Is_True(psymtab < csymtab,
            ("fishy-looking psymtab %d and/or csymtab %d",
	     (INT) psymtab, (INT) csymtab));
    Is_True(CURRENT_SYMTAB == csymtab, ("CURRENT_SYMTAB != csymtab"));
    char* Cur_PU_Name = ST_name(PU_Info_proc_sym(Current_PU_Info));
    INT strsize = strlen(User_Label_Number_Format) + 64 + strlen(Cur_PU_Name);
    char* labelname = (char*) calloc(strsize, 1);
    sprintf ( labelname, User_Label_Number_Format, (INT) csymtab,
	      (INT) plabel_idx, ST_name(Get_Current_PU_ST()) );

    LABEL &clabel = New_LABEL(csymtab, clabel_idx);
    LABEL_Init(clabel, Save_Str(labelname),
               LABEL_kind((*Scope_tab[psymtab].label_tab)[LABEL_IDX_index(plabel_idx)]));
    label_info_table[LABEL_IDX_index(plabel_idx)] = clabel_idx;
  }
  
  return clabel_idx;
}

/* Return TRUE if the symbol st is of type KIND_ARRAY or KIND_ARRAY*
 * From be/lno/ipa_lno_read.cxx, for ARRAY_REDUCTION. csc.
 */
static BOOL
Is_Kind_Array(const ST* st)
{
	TY_IDX ty_idx = ST_type(st);
	if (TY_kind(ty_idx) == KIND_POINTER)
		ty_idx = TY_pointed(ty_idx);
	return TY_kind(ty_idx) == KIND_ARRAY;
}

/*
Return TRUE if upper and lower bounds of each dimension of arr_ty are
defined, FALSE otherwise. arr_ty must be an array type.
*/

static BOOL 
TY_All_Bounds_Defined(TY_IDX arr_ty)
{
  Is_True(TY_kind(arr_ty) == KIND_ARRAY, ("not an array type"));

  INT i;
  for (i = 0; i < TY_AR_ndims(arr_ty); i++) {
    if (!TY_AR_const_lbnd(arr_ty, i) &&
        TY_AR_lbnd_var(arr_ty, i) == 0)
      return FALSE; // lower bound not defined
    if (!TY_AR_const_ubnd(arr_ty, i) &&
        TY_AR_ubnd_var(arr_ty, i) == 0)
      return FALSE; // upper bound not defined
  }

  return TRUE;
} // TY_All_Bounds_Defined()


/*  Calculate size of a dynamic array (in bytes).  */

static WN * 
Calculate_Array_Size ( ST * st, TY_IDX ty )
{
  INT32 i;
  WN   *wn;
  WN   *lower = NULL;
  WN   *upper = NULL;
  const OPCODE int_opc = (Pointer_Size == 4) ? OPC_I4INTCONST :
                         (Pointer_Size == 8) ? OPC_I8INTCONST : OPCODE_UNKNOWN;
  const mTYPE_ID int_mtype = (int_opc == OPC_I4INTCONST) ? MTYPE_I4 : MTYPE_I8;

  Is_True(int_opc != OPCODE_UNKNOWN, ("invalid Pointer_Size"));

  wn = WN_CreateIntconst ( int_opc, TY_size(TY_AR_etype(ty)) );
  for (i = 0; i < TY_AR_ndims(ty); i++) {
    if (TY_AR_const_lbnd(ty, i))
      lower = WN_CreateIntconst ( int_opc, TY_AR_lbnd_val(ty, i) );
    else {
      if (TY_AR_lbnd_var(ty,i)) {
        lower = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                TY_mtype(ST_type(TY_AR_lbnd_var(ty,i))),
                                TY_mtype(ST_type(TY_AR_lbnd_var(ty,i)))),
                               0, TY_AR_lbnd_var(ty,i),
                               ST_type(TY_AR_lbnd_var(ty,i)));
      }
      else lower = NULL;
    }
    if (TY_AR_const_ubnd(ty, i))
      upper = WN_CreateIntconst ( int_opc, TY_AR_ubnd_val(ty, i) );
    else {
      if (TY_AR_ubnd_var(ty,i)) {
        upper = WN_CreateLdid (OPCODE_make_op(OPR_LDID,
                                 TY_mtype(ST_type(TY_AR_ubnd_var(ty,i))),
                                 TY_mtype(ST_type(TY_AR_ubnd_var(ty,i)))),
                               0, TY_AR_ubnd_var(ty,i),
                               ST_type(TY_AR_ubnd_var(ty,i)));
      }
      else upper = NULL;
    }
    if (upper && lower) {
      wn = WN_Mpy ( int_mtype, wn, WN_Add ( int_mtype,
					    WN_Sub ( int_mtype, upper, lower ),
					    WN_CreateIntconst ( int_opc,
								1 )));
    } else {
      ErrMsg ( EC_MPLOWER_local_nosize, st );
      WN_DELETE_Tree (wn);
      wn = WN_CreateIntconst ( int_opc, 0 );
      break;
    }
  }

  return (wn);
}

// Return a Whirl BLOCK for allocating a VLA auto variable using ALLOCA.
static WN *
Gen_Auto_Alloca(ST *st, WN_OFFSET offset, TY_IDX ty, WN *size)
{
  WN *block = WN_CreateBlock(), *vla_stid_kid;

  if (Alloca_Dealloca_On) {
      // use ALLOCA operator
    vla_stid_kid = WN_CreateAlloca(size);

  } else {
      // Call ALLOCA intrinsic
    WN *intrin_call = WN_Create(
      (Pointer_Size == 4) ? OPC_U4INTRINSIC_CALL : OPC_U8INTRINSIC_CALL, 1);
    WN_intrinsic(intrin_call) = (Pointer_Size == 4) ? INTRN_U4I4ALLOCA :
                                INTRN_U8I8ALLOCA;
    WN_Set_Call_Non_Data_Mod(intrin_call);
    WN_Set_Call_Non_Data_Ref(intrin_call);
    WN_Set_Call_Non_Parm_Mod(intrin_call);
    WN_Set_Call_Non_Parm_Ref(intrin_call);
    WN_Set_Call_Parm_Ref(intrin_call);
    WN_linenum(intrin_call) = line_number;
    WN_kid(intrin_call, 0) = WN_CreateParm(
      (Pointer_Size == 4) ? MTYPE_I4 : MTYPE_I8, size,
      Be_Type_Tbl((Pointer_Size == 4) ? MTYPE_I4 : MTYPE_I8),
      WN_PARM_BY_VALUE);

    WN_INSERT_BlockLast(block, intrin_call);

      // Load returned value
    PREG_NUM rreg1, rreg2;

    GET_RETURN_PREGS(rreg1, rreg2, Pointer_type);
    vla_stid_kid = WN_LdidPreg(Pointer_type, rreg1);
  }

  WN *vla_stid = WN_Stid(Pointer_type, offset, st, ty, vla_stid_kid);
  WN_linenum(vla_stid) = line_number;
  WN_INSERT_BlockLast(block, vla_stid);

  return block;
}


// A VLA that is scoped within a parallel construct
// will have its ALLOCA generated by the front end,
// and it doesn't need a new ALLOCA when localized.

static vector<ST*> inner_scope_vla;

static void 
Gather_Inner_Scope_Vlas(WN *wn)
{
  if (WN_operator(wn) == OPR_STID && WN_operator(WN_kid0(wn)) == OPR_ALLOCA) {
    inner_scope_vla.push_back(WN_st(wn));    
  }
  else if (WN_operator(wn) == OPR_BLOCK) {
    for (WN *kid = WN_first(wn); kid; kid = WN_next(kid)) {
      Gather_Inner_Scope_Vlas(kid);
    }
  }
  else {
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
      Gather_Inner_Scope_Vlas(WN_kid(wn, kidno));
    }
  }
}

static BOOL
Vla_Needs_Alloca(ST *st)
{
  vector<ST*>::iterator i;
  for (i = inner_scope_vla.begin(); i != inner_scope_vla.end(); i++) {
    if (*i == st) {
      return FALSE;
    }
  }
  return TRUE;
} 

#ifdef KEY
// Keep track of localized copies of threadprivate variables.
class Localized_thdprv_var
{
  ST_IDX var; // local ST for a threadprivate variable
  ST_IDX localized; // localized copy of 'var'

  public:
  Localized_thdprv_var (ST_IDX v, ST_IDX l) : var (v), localized (l) {}
  ST_IDX Thdprv_ptr (void) const { return var; }
  ST_IDX Thdprv_local (void) const { return localized; }
};

// This vector is cleared at the end of MP processing for a PU.
std::vector<Localized_thdprv_var> localized_var_vect;
#endif // KEY

/*
Make a local symbol having the same type as the original symbol.  The
parameters firstprivate_blockp and alloca_blockp are the same as in
Create_Local_Variables().  Input parameter prev_def points to a prior entry
for the symbol (NULL if there's none): in particular, if vtype is
VAR_FIRSTPRIVATE, there may have been a prior VAR_LASTLOCAL entry (if the
variable is both FIRSTPRIVATE and LASTPRIVATE) or a prior VAR_LOCAL entry
(if old_st was marked SHARED_DEADOUT).
*/

static void 
Localize_Variable ( VAR_TABLE *v, VAR_TYPE vtype, OPERATOR opr,
                    WN *vtree, ST *old_st, WN_OFFSET old_offset,
                    WN **firstprivate_blockp, WN **alloca_blockp,
                    VAR_TABLE *prev_def )
{
  ST   *sym;
  TY_IDX ty;
  char *localname;
  DST_INFO_IDX dst;
  const BOOL orphaned =
    (mpt == MPP_ORPHANED_SINGLE || mpt == MPP_ORPHANED_PDO);

  Is_True(!prev_def || vtype == VAR_FIRSTPRIVATE,
          ("prev_def only valid for VAR_FIRSTPRIVATE"));
  Is_True(!prev_def || prev_def->vtype == VAR_LASTLOCAL ||
          prev_def->vtype == VAR_LOCAL,
          ("invalid vtype for prev_def"));

    // disallow privatizing things that are EQUIVALENCEd: PV 707883
  switch (vtype) {
  case VAR_LASTLOCAL:
  case VAR_LOCAL:
  case VAR_FIRSTPRIVATE:
    if (Has_Base_Block(old_st) && ST_is_equivalenced(old_st)) {
        // if the MP region in which the privatization pragma appears
        // was compiler-generated, it must have come from LNO (-pfa), in
	// which case alias analysis has determined that privatization is
	// actually OK: PV 574997
      if (comp_gen_construct) {
//        DevWarn("allowing privatization of EQUIVALENCEd %s", ST_name(old_st));
        break;
      }
// Bug 3795
#ifndef KEY
      ErrMsg(EC_MPLOWER_priv_equiv, old_st);
#endif
      return;
    }
    break;
  default:
    break;
  }

  v->vtype	      = vtype;
  v->is_static_array  = ((ST_sclass(old_st) == SCLASS_FORMAL) &&
			 ((vtype == VAR_LOCAL) || (vtype == VAR_LASTLOCAL) ||
			  (vtype == VAR_FIRSTPRIVATE)) &&
			 (TY_kind(ST_type(old_st)) == KIND_POINTER) &&
			 (TY_kind(TY_pointed(ST_type(old_st))) == KIND_ARRAY) &&
			 (TY_size(TY_pointed(ST_type(old_st))) != 0));
  v->is_dynamic_array = (((vtype == VAR_LOCAL) || (vtype == VAR_LASTLOCAL) ||
                          (vtype == VAR_REDUCTION_ARRAY_OMP) ||
			  (vtype == VAR_FIRSTPRIVATE)) &&
			 (TY_kind(ST_type(old_st)) == KIND_POINTER) &&
                         (ST_keep_name_w2f(old_st) || 
                          PU_src_lang(Get_Current_PU()) == PU_F77_LANG ||
                          PU_src_lang(Get_Current_PU()) == PU_F90_LANG) &&
			 (TY_kind(TY_pointed(ST_type(old_st))) == KIND_ARRAY) &&
			 (TY_size(TY_pointed(ST_type(old_st))) == 0) &&
			 TY_All_Bounds_Defined(TY_pointed(
			                         ST_type(old_st)) ) );
       v->is_non_pod = ( ST_class(old_st) != CLASS_PREG &&
                        ( TY_kind(ST_type(old_st)) == KIND_STRUCT &&
                        TY_is_non_pod(ST_type(old_st)) ) ||
                        ( TY_kind(ST_type(old_st)) == KIND_ARRAY &&
                        TY_kind(TY_etype(ST_type(old_st))) == KIND_STRUCT &&
			                  TY_is_non_pod(TY_etype(ST_type(old_st)))));
  v->vtree	      = vtree;
  v->vtreex	      = NULL;
  v->orig_st	      = old_st;
  v->orig_offset      = old_offset;

#ifdef Is_True_On
  if (v->is_non_pod)
    switch (mpt) {
    case MPP_SINGLE:
    case MPP_ORPHANED_SINGLE:
    case MPP_PDO:
    case MPP_ORPHANED_PDO:
    case MPP_PARALLEL_REGION:
      break;
    default:
      Fail_FmtAssertion("non-POD object invalidly privatized on construct "
                        "with mpt == %d", (INT) mpt);
    }
#endif

  if (prev_def) {
    v->mtype = prev_def->mtype;
    v->has_offset = prev_def->has_offset;
    v->ty = prev_def->ty;
    v->vtree = prev_def->vtree ? WN_COPY_Tree(prev_def->vtree) : NULL;
    v->vtreex = prev_def->vtreex ? WN_COPY_Tree(prev_def->vtreex) : NULL;
    v->new_st = prev_def->new_st;
    v->new_offset = prev_def->new_offset;
    if (prev_def->vtype == VAR_LASTLOCAL)
      prev_def->is_last_and_firstprivate = TRUE;

  } else if (ST_class(old_st) != CLASS_PREG) {

    ty = ST_type(old_st);

    TY_KIND kind = TY_kind(ty);
    while ( kind == KIND_POINTER || kind == KIND_STRUCT || kind == KIND_ARRAY)
    {
      if (kind == KIND_POINTER)
      {
        if (v->is_static_array || vtype == VAR_REDUCTION_ARRAY)
          ty = TY_pointed(ty);
        else
          break;
      }else if (kind == KIND_STRUCT)
      {
        if (vtype == VAR_REDUCTION_SCALAR)
           ty = FLD_type(Get_FLD_From_Offset(ty, v->orig_offset));
        else if (vtype == VAR_REDUCTION_ARRAY)
        {
          WN* array = v->vtree;
          if (WN_element_size(array)<0)
          {
            // fortran dope vector: we will iteratively find the element type.
            // Maybe we should check the dimsion.
            ty = FLD_type(TY_fld(ty));
          }
          else
          {
            // must be from pointer promption
            // we might have some problems for the union as multiple fields
            // share the same offset. But this info is lost and there's no way
            // to recover. (we need to change the reduction pragma)
            UINT64 offset = Get_Offset_From_Const_Array(array);
            FLD_HANDLE fld = Get_FLD_From_Offset(ty, offset);

            ty = FLD_type(fld);
          }
        } else
         break;
      }else if (kind == KIND_ARRAY)
      {
        if (vtype == VAR_REDUCTION_ARRAY)
           ty = TY_etype(ty);
        else
          break;
      }
      kind = TY_kind(ty);
    }
  
    localname = (char *) alloca(strlen(ST_name(old_st)) + 32);
      // if already localized, append "x" to localization prefix
    if (strncmp(ST_name(old_st), "__mplocal_", 10) == 0)
      sprintf ( localname, "__mplocalx_%s", &ST_name(old_st)[10] );
    else if (strncmp(ST_name(old_st), "__mplocalfe_", 12) == 0)
      sprintf ( localname, "__mplocalfex_%s", &ST_name(old_st)[12] );
    else if (strncmp(ST_name(old_st), "__mptemp_", 9) == 0)
      sprintf ( localname, "__mptempx_%s", &ST_name(old_st)[9] );
    else
      sprintf ( localname, "__mplocal_%s", ST_name(old_st) );

    if (v->is_non_pod && orphaned) {
        // Within orphaned worksharing constructs, non-POD objects are
        // privatized already by the frontend, so we don't create a new
        // symbol for such objects.
      sym = NULL;

    } else {
      sym = NULL;
#ifdef KEY
    INT16 i;
    ST *symbol;
    WN *thread_priv_prag = NULL;
    WN *matched_pragma = NULL;
    if (reference_block) {
      thread_priv_prag = WN_first(reference_block);
      if (thread_priv_prag) {
        while (thread_priv_prag) { 
          if (WN_opcode(thread_priv_prag) == OPC_PRAGMA &&
              WN_pragma(thread_priv_prag) == WN_PRAGMA_THREADPRIVATE) {
            if (WN_pragma_arg1(thread_priv_prag) == ST_st_idx(old_st)){
              matched_pragma = thread_priv_prag;
              break;
            }
          }
          thread_priv_prag = WN_next(thread_priv_prag);
        }
// Bug 4178
        thread_priv_prag = WN_first(reference_block);
        while (!matched_pragma && thread_priv_prag) {
          if (WN_opcode(thread_priv_prag) == OPC_PRAGMA &&
              WN_pragma(thread_priv_prag) == WN_PRAGMA_THREADPRIVATE){ 
            FOREACH_SYMBOL (CURRENT_SYMTAB, symbol, i) {
              if (symbol == ST_ptr(WN_pragma_arg1(thread_priv_prag)) &&
                  strcmp(localname, ST_name(symbol)) == 0) {
                sym = symbol;
                break;
              }
            }
            if (!sym)
              break;
          }
          thread_priv_prag = WN_next(thread_priv_prag);
        }
      }
    }
    // TODO: Remove the above fix for bug 4178
    // Ensure all occurrences of a local thdprv pointer gets the same
    // local copy. Needed for multiple orphan DO regions, when each
    // of them accesses the same threadprivate variable.
    if (!sym)
      for (INT it = 0; it < localized_var_vect.size(); it++)
      {
        if (localized_var_vect[it].Thdprv_ptr() == ST_st_idx (old_st))
        {
          sym = ST_ptr (localized_var_vect[it].Thdprv_local());
          break;
        }
      }
#endif
        // Create privatized version of symbol.
     if (!sym) {
       sym = New_ST (CURRENT_SYMTAB);
       ST_Init (sym,
     	       Save_Str (localname),
       	       CLASS_VAR,             // SCLASS_AUTO requires CLASS_VAR
               SCLASS_AUTO,
               EXPORT_LOCAL,
               ty);

       if (ST_addr_saved(old_st))
          Set_ST_addr_saved(sym);
       if (ST_addr_passed(old_st))
          Set_ST_addr_passed(sym);
     }
#ifdef KEY
     { // Assumption: The local pragma for a local thdprv ST must be present
       // in the function pragmas
       WN *prag = WN_first(WN_func_pragmas(PU_Info_tree_ptr(Current_PU_Info)));
       while (prag)
       {          
         if (WN_opcode (prag) == OPC_PRAGMA &&
             WN_pragma (prag) == WN_PRAGMA_THREADPRIVATE &&
             WN_pragma_arg1 (prag) == ST_st_idx (old_st))
         {
           Localized_thdprv_var v (WN_pragma_arg1 (prag), ST_st_idx (sym));

           localized_var_vect.push_back (v);
           WN_pragma_arg1 (prag) = ST_st_idx (sym);
	   break;
         }
         prag = WN_next (prag);
       }
     }
     if (matched_pragma) 
	WN_pragma_arg1(matched_pragma) = ST_st_idx(sym);
#endif

      // Don't do the following; it depends on a back-end-specific table
      // that doesn't exist until we're compiling the nested MP
      // procedure, and addr_used_locally is recomputed by the optimizer
      // in any case.
      // if (BE_ST_addr_used_locally(old_st)) Set_BE_ST_addr_used_locally(sym);

      if (v->is_dynamic_array) {
        Set_ST_pt_to_unique_mem(sym);
        Set_ST_pt_to_compiler_generated_mem(sym);
      }

      if (Debug_Level > 0) {
          // search in the DST tree of the parent subprogram for the
          // DST entry corresponding to this symbol
#ifndef KEY
        dst = Find_DST_From_ST( old_st, ppuinfo );
#else // bug 5473
	if (ppuinfo) dst = Find_DST_From_ST( old_st, ppuinfo );
	else dst = DST_INVALID_IDX;
#endif
          // Only create a new DST entry for a valid symbol, not any
          // of the predef stuff.  If we're localizing for an orphaned
	  // construct, there's no nested function and associated symbol
	  // table or DST.
        if (!DST_IS_NULL(dst))
          Create_New_DST( dst, sym, !orphaned );
      }
    } // if (orphaned)

    if (TY_kind(ty) == KIND_ARRAY) {
      v->mtype = TY_mtype(TY_etype(ty));
      v->has_offset = (vtype != VAR_LASTLOCAL) && (vtype != VAR_LOCAL) &&
		      (vtype != VAR_FIRSTPRIVATE) && (vtype != VAR_REDUCTION_ARRAY_OMP);
    } else if (TY_kind(ty) == KIND_STRUCT) {
      v->mtype = TY_mtype(ty);
      v->has_offset = FALSE;
    } else if (v->is_dynamic_array) {
      v->mtype = TY_mtype(TY_AR_etype(TY_pointed(ty)));
      v->has_offset = FALSE;
    } else {
      v->mtype = TY_mtype(ty);
      v->has_offset = TRUE;
    }
    v->ty         = ty;
    v->new_st     = sym;
    v->new_offset = 0;

    if ((v->is_static_array) && (vtree == NULL)) {
      v->vtree = WN_RLdid ( Promote_Type(Pointer_type), Pointer_type,
			    old_offset, old_st, ST_type(old_st) );
      v->vtreex = WN_Create ( (Pointer_Size == 4) ? OPC_U4ARRAY : OPC_U8ARRAY,
			      3 );
      WN_element_size(v->vtreex) = Pointer_Size;
      WN_kid0(v->vtreex) = WN_RLdid ( Promote_Type(Pointer_type), Pointer_type,
				      old_offset, old_st, ST_type(old_st) );
      WN_kid1(v->vtreex) = WN_Intconst ( MTYPE_I4, 0 );
      WN_kid2(v->vtreex) = WN_Intconst ( MTYPE_I4, 0 );
    }

    if (v->is_dynamic_array && 
        Vla_Needs_Alloca(v->orig_st) &&
        !v->is_non_pod) { // fecc allocates non-POD VLAs
      if (*alloca_blockp == NULL)
	    *alloca_blockp = WN_CreateBlock ( );
      WN_INSERT_BlockLast(*alloca_blockp,
        Gen_Auto_Alloca(v->new_st, v->new_offset, v->ty,
                        Calculate_Array_Size(old_st, TY_pointed(ty))));
    }

  } else {  // ST_class(old_st) == CLASS_PREG
    v->mtype      = ST_btype(old_st);
    v->has_offset = TRUE;
    v->ty         = ST_type(old_st);
    v->new_st     = old_st;
    v->new_offset = old_offset;

  }

  if (vtype == VAR_FIRSTPRIVATE && firstprivate_blockp &&
      !v->is_non_pod) {   // fecc generates constructor calls
    if (*firstprivate_blockp == NULL)
      *firstprivate_blockp = WN_CreateBlock ( );
    WN_INSERT_BlockLast(*firstprivate_blockp,
      Gen_MP_Load_Store(v->orig_st, v->orig_offset,
                        v->new_st, v->new_offset,
                        v->is_dynamic_array));
  }

    /* determine the opcode for reductions */
  if (vtype == VAR_REDUCTION_SCALAR || 
		vtype == VAR_REDUCTION_ARRAY  ||
		vtype == VAR_REDUCTION_ARRAY_OMP) {
    TYPE_ID rtype, dtype = MTYPE_V;

   /* Operator is explicit in OMP, or inferred by OMP_Prelower for MP.
   If it's still unknown at this point, it couldn't be inferred
	 because the operator wasn't used in the lexical scope of the
	 reduction clause, so it's an error. */
    if (opr == OPERATOR_UNKNOWN) {
      char *redvar_name = (char *) alloca(strlen(ST_name(v->orig_st)) + 32);

      sprintf(redvar_name, "%s %s",
	      vtree ? "element of array" : "variable",
              ST_name(v->orig_st));
      ErrMsg(EC_MPLOWER_red_not_found, redvar_name);
    }

      // Promote 1 and 2 byte types to 4 bytes
    if (v->mtype == MTYPE_I1 || v->mtype == MTYPE_I2)
      rtype = MTYPE_I4;
    else if (v->mtype == MTYPE_U1 || v->mtype == MTYPE_U2)
      rtype = MTYPE_U4;
    else
      rtype = v->mtype;

    switch (opr) {
    case OPR_LAND:
    case OPR_LIOR:
      if (v->mtype == MTYPE_I8) {
          // truncate result on LOGICAL*8 to 4 bytes: PV 602935
        rtype = MTYPE_I4;
      }
      break;

    case OPR_EQ:
    case OPR_NE:
      if (rtype == MTYPE_I8) {
        dtype = MTYPE_I8;
          // truncate .EQV./.NEQV. result on LOGICAL*8: PV 659567
	      rtype = MTYPE_I4;
      } else {
        dtype = rtype;
      }
      break;

    case OPR_CAND:
    case OPR_CIOR:
        // for && and ||, the rtype must be MTYPE_I4: PV 677602
      rtype = MTYPE_I4;
      break;

    default:
      break;
    }

    v->reduction_opr = opr;
    v->reduction_opc = OPCODE_make_op(opr, rtype, dtype);
  } else {
    v->reduction_opr = OPERATOR_UNKNOWN;
    v->reduction_opc = OPCODE_UNKNOWN;
  }
}


/*
Verify that the given PRAGMA or XPRAGMA tree doesn't reference any
non-dedicated pregs.
*/

static void 
Verify_No_Pregs_In_Tree(WN *tree)
{
  WN_ITER *it = WN_WALK_TreeIter(tree);

  while (it) {
    WN *wn = it->wn;

    OPERATOR opr = WN_operator(wn);

    if (OPERATOR_has_sym(opr) && OPERATOR_has_offset(opr) && WN_st(wn) &&
        ST_class(WN_st(wn)) == CLASS_PREG &&
        !Preg_Is_Dedicated(WN_offsetx(wn)))
      Fail_FmtAssertion("invalid preg reference in worksharing scope pragma");

    it = WN_WALK_TreeNext(it);
  }
}
#ifdef KEY
/* Generate a procedure body to copy the data */

static ST*
Create_Copyfunc(ST *struct_st)
{
  SYMTAB_IDX cp_psymtab = CURRENT_SYMTAB;
  PU_Info *cp_ppuinfo = Current_PU_Info;
  WN_MAP_TAB *cp_pmaptab = Current_Map_Tab;
  WN_MAP_TAB *cp_cmaptab = cmaptab;	
  ST *cp_parallel_proc = parallel_proc, *swap_proc;
  SYMTAB_IDX cp_csymtab;
  INT32 cp_func_level;

  const char *construct_type_str = "cp_thunk";
  TY_IDX &func_ty_idx = mpregion_ty;
  char temp_str[64];

  if  (func_ty_idx == TY_IDX_ZERO) {
    // create new type for function, and type for pointer to function

    TY& ty = New_TY(func_ty_idx);
    sprintf(temp_str, ".%s", construct_type_str);
    TY_Init(ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, Save_Str(temp_str));
    Set_TY_align(func_ty_idx, 1);

    TYLIST_IDX parm_idx;
    TYLIST& parm_list = New_TYLIST(parm_idx);
    Set_TY_tylist(ty, parm_idx);
    Set_TYLIST_type(parm_list, Be_Type_Tbl(MTYPE_V));  // return type

      // Two basic parameters
    Set_TYLIST_type(New_TYLIST(parm_idx), // src
                      Be_Type_Tbl(Pointer_type));
    Set_TYLIST_type(New_TYLIST(parm_idx), // dst
                      Be_Type_Tbl(Pointer_type));

    Set_TYLIST_type(New_TYLIST(parm_idx), TY_IDX_ZERO); // end of parm list

      // now create a type for a pointer to this function
    TY_IDX ptr_ty_idx;
    TY &ptr_ty = New_TY(ptr_ty_idx);
    sprintf(temp_str, ".%s_ptr", construct_type_str);
    TY_Init(ptr_ty, Pointer_Size, KIND_POINTER, Pointer_Mtype,
            Save_Str(temp_str));
    Set_TY_pointed(ptr_ty, func_ty_idx);
  }

  PU_IDX pu_idx;
  PU& pu = New_PU(pu_idx);
  PU_Init(pu, func_ty_idx, CURRENT_SYMTAB);

  Set_PU_no_inline(pu);
  Set_PU_is_nested_func(pu);
  Set_PU_mp(pu);
  if (PU_c_lang(Current_PU_Info_pu()))
    Set_PU_c_lang(pu);
  if (PU_cxx_lang(Current_PU_Info_pu()))
    Set_PU_cxx_lang(pu);
  if (PU_f77_lang(Current_PU_Info_pu()))
    Set_PU_f77_lang(pu);
  if (PU_f90_lang(Current_PU_Info_pu()))
    Set_PU_f90_lang(pu);
  if (PU_java_lang(Current_PU_Info_pu()))
    Set_PU_java_lang(pu);

  Set_FILE_INFO_has_mp(File_info);

  char *st_name = (char *) alloca(strlen(construct_type_str) + 32);
  sprintf ( st_name, "__%s_%d", construct_type_str, ST_st_idx(struct_st));

  parallel_proc = New_ST(GLOBAL_SYMTAB);
  ST_Init(parallel_proc,
          Save_Str (st_name),
          CLASS_FUNC,
          SCLASS_TEXT,
          EXPORT_LOCAL,
          pu_idx);
  Set_ST_addr_passed(parallel_proc);

  Allocate_Object ( parallel_proc );

  New_Scope(CURRENT_SYMTAB + 1,
            Malloc_Mem_Pool,  
            TRUE);
  cp_csymtab = CURRENT_SYMTAB;
  cp_func_level = CURRENT_SYMTAB;
  Scope_tab[cp_csymtab].st = parallel_proc;

  Set_PU_lexical_level(pu, CURRENT_SYMTAB);
  Create_Func_DST ( st_name );

  for (UINT32 i = 1; i < PREG_Table_Size(cp_psymtab); i++) {
    PREG_IDX preg_idx;
    PREG &preg = New_PREG(cp_csymtab, preg_idx);
      // share name with corresponding parent preg
    Set_PREG_name_idx(preg,
      PREG_name_idx((*Scope_tab[cp_psymtab].preg_tab)[preg_idx]));
  }
  ST *arg_src = New_ST( CURRENT_SYMTAB );
  ST_Init (arg_src,
             Save_Str ( "__ompv_src_a" ),
             CLASS_VAR,
             SCLASS_FORMAL,
             EXPORT_LOCAL,
             Be_Type_Tbl( Pointer_type ));
  Set_ST_is_value_parm( arg_src );
                                                                                                                                                             
  ST *arg_dst = New_ST( CURRENT_SYMTAB );
  ST_Init( arg_dst,
           Save_Str( "__ompv_dst_a" ),
           CLASS_VAR,
           SCLASS_FORMAL,
           EXPORT_LOCAL,
           Be_Type_Tbl( Pointer_type ));
  Set_ST_is_value_parm( arg_dst );

  WN *func_body = WN_CreateBlock();
  WN *func_entry = WN_CreateEntry(2, parallel_proc,
                                 func_body,
                                 WN_CreateBlock(),
                                 WN_CreateBlock());

  WN_linenum(func_entry) = line_number;
  WN_kid0(func_entry) = WN_CreateIdname ( 0, arg_src );
  WN_kid1(func_entry) = WN_CreateIdname ( 0, arg_dst );
  WN_linenum(func_entry) = line_number;

  FLD_HANDLE  fld;
  fld = TY_flist(Ty_Table[ST_type(struct_st)]);
  WN *value, *addr, *istore;
  TYPE_ID type;
  for (; !fld.Is_Null (); fld = FLD_next(fld)){
    TY& ty = Ty_Table [FLD_type (fld)];
    TY *pty = &ty;
    TY_IDX pty_idx = TY_pointed(*pty);
    OPCODE addr_load_opc = OPCODE_make_op(OPR_ILOAD, Pointer_type, Pointer_type);
    if (TY_kind(pty_idx) != KIND_ARRAY){
      type = TY_mtype(pty_idx);
      OPCODE load_opc = OPCODE_make_op(OPR_ILOAD,type,type);
      value = WN_CreateIload(load_opc, 0,
                        Be_Type_Tbl(type),
                        Make_Pointer_Type(Be_Type_Tbl(type),FALSE),
                        WN_CreateIload(addr_load_opc, FLD_ofst(fld),
                                       Be_Type_Tbl(Pointer_type),
                                       Make_Pointer_Type(Be_Type_Tbl(Pointer_type),FALSE),
                                       WN_Ldid(Pointer_type, 0, arg_src, ST_type(arg_src))));
      istore = WN_CreateIstore(OPCODE_make_op(OPR_ISTORE,MTYPE_V,type),
                        0,
                        Make_Pointer_Type(Be_Type_Tbl(type),FALSE),
                        value,
                        WN_CreateIload(addr_load_opc, FLD_ofst(fld),
                                       Be_Type_Tbl(Pointer_type),
                                       Make_Pointer_Type(Be_Type_Tbl(Pointer_type),FALSE),
                                       WN_Ldid(Pointer_type, 0, arg_dst, ST_type(arg_dst))));
      WN_INSERT_BlockLast(func_body, istore);
    }
    else{
// Bug 3822
      WN *call = Transform_To_Memcpy(WN_CreateIload(addr_load_opc, FLD_ofst(fld),
                                                    Be_Type_Tbl(Pointer_type),
                                                    Make_Pointer_Type(Be_Type_Tbl(Pointer_type),FALSE),
                                                    WN_Ldid(Pointer_type, 0, arg_dst, ST_type(arg_dst))),
                                     WN_CreateIload(addr_load_opc, FLD_ofst(fld),
                                                    Be_Type_Tbl(Pointer_type),
                                                    Make_Pointer_Type(Be_Type_Tbl(Pointer_type),FALSE),
                                                    WN_Ldid(Pointer_type, 0, arg_src, ST_type(arg_src))),
                                     0, 
                                     Make_Pointer_Type(Be_Type_Tbl(Pointer_type),FALSE), 
                                     Make_Pointer_Type(Be_Type_Tbl(Pointer_type),FALSE), 
                                     WN_CreateIntconst (OPC_U4INTCONST,TY_size(pty_idx)));
      WN_INSERT_BlockLast(func_body, call);
    }
  }
  WN *wn = WN_CreateReturn ( );
  WN_linenum(wn) = line_number;
  WN_INSERT_BlockLast ( func_body, wn );

  PU_Info *copy_pu = TYPE_MEM_POOL_ALLOC ( PU_Info, Malloc_Mem_Pool );
  PU_Info_init ( copy_pu );
  Set_PU_Info_tree_ptr (copy_pu, func_entry );

  PU_Info_proc_sym(copy_pu) = ST_st_idx(parallel_proc);
  PU_Info_maptab(copy_pu) = cp_cmaptab = WN_MAP_TAB_Create(MEM_pu_pool_ptr);
  PU_Info_pu_dst(copy_pu) = nested_dst;
  Set_PU_Info_state(copy_pu, WT_SYMTAB, Subsect_InMem);
  Set_PU_Info_state(copy_pu, WT_TREE, Subsect_InMem);
  Set_PU_Info_state(copy_pu, WT_PROC_SYM, Subsect_InMem);
  Set_PU_Info_flags(copy_pu, PU_IS_COMPILER_GENERATED);

  Set_PU_Info_symtab_ptr(copy_pu, NULL);
  Save_Local_Symtab(cp_csymtab, copy_pu);

  if (Cur_PU_Feedback) {
    parallel_pu_fb = CXX_NEW(FEEDBACK(func_entry,
                                      MEM_pu_nz_pool_ptr,
                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
                                      cp_cmaptab),
                             MEM_pu_nz_pool_ptr);
    Set_PU_Info_state(copy_pu, WT_FEEDBACK, Subsect_InMem);
    Set_PU_Info_feedback_ptr(copy_pu, parallel_pu_fb);
    FB_Transfer(Cur_PU_Feedback, parallel_pu_fb, stmt_block); 
  }

  RID *root_rid = RID_Create ( 0, 0, func_entry );
  RID_type(root_rid) = RID_TYPE_func_entry;
  Set_PU_Info_regions_ptr ( copy_pu, root_rid );
  Is_True(PU_Info_regions_ptr(copy_pu) != NULL,
         ("Create_Copythunk, NULL root RID"));

  PU_Info *tpu = PU_Info_child(Current_PU_Info);

    // add parallel_pu after last child MP PU_Info item in parent's list
  if (tpu && PU_Info_state(tpu, WT_SYMTAB) == Subsect_InMem &&
      PU_mp(PU_Info_pu(tpu)) ) {
    PU_Info *npu;

    while ((npu = PU_Info_next(tpu)) &&
	   PU_Info_state(npu, WT_SYMTAB) == Subsect_InMem &&
	   PU_mp(PU_Info_pu(npu)) )
      tpu = npu;

    PU_Info_next(tpu) = copy_pu;
    PU_Info_next(copy_pu) = npu;
  } else {
    PU_Info_child(Current_PU_Info) = copy_pu;
    PU_Info_next(copy_pu) = tpu;
  }


  // change some global state; need to clean this up--DRK

  Current_PU_Info = cp_ppuinfo;
  Current_pu = &Current_PU_Info_pu();
  Current_Map_Tab = cp_pmaptab;
  Transfer_Maps ( cp_pmaptab, cp_cmaptab, func_body,
		  PU_Info_regions_ptr(Current_PU_Info) );

  Current_Map_Tab = cp_cmaptab;
  // We don't need to update dependence graph since this function is totally new.
  //MP_Fix_Dependence_Graph ( cp_ppuinfo, Current_PU_Info, func_body ); 
  Set_PU_Info_depgraph_ptr(copy_pu,NULL);
  Current_Map_Tab = cp_pmaptab;

  CURRENT_SYMTAB = cp_psymtab;
  Add_DST_variable ( arg_src, nested_dst, line_number, DST_INVALID_IDX );
  Add_DST_variable ( arg_dst, nested_dst, line_number, DST_INVALID_IDX );

  swap_proc = parallel_proc;
  parallel_proc = cp_parallel_proc;
  return swap_proc;
}

/*  Generate a copyprivate call.  Generate a procedure to copy the data. 
    Also, generate a structure so that the compiler can pass the address 
    of each copyprivate variable into RTL. */

static void
Gen_MP_Copyprivate(WN *copyprivates, WN **copyprivate_blockp,
                   WN *ldid_lock)
{
  WN *wnx;
  FLD_HANDLE fld;
  FLD *next;
  ST *st, *fld_st;

#ifndef TARG_NVISA
  BOOL target_32bit = Is_Target_32bit();
#else
  BOOL target_32bit = FALSE;
#endif

  /* Create a struct type */
  INT32 count = 0;
  for (wnx = copyprivates; wnx; wnx = WN_next(wnx),count++);

  TY_IDX struct_ty_idx;
  TY& struct_ty = New_TY(struct_ty_idx);
  TY_Init(struct_ty, target_32bit ? 4*count : 8*count, 
          KIND_STRUCT, MTYPE_M, STR_IDX_ZERO);
  Set_TY_align(struct_ty_idx, MTYPE_align_req(Pointer_type));

  INT32 offset = 0;
  for (wnx = copyprivates; wnx; wnx = WN_next(wnx)){
    fld_st = WN_st(wnx);
    fld = New_FLD ();
    if (wnx == copyprivates) 
      Set_TY_fld(struct_ty, fld);
    FLD_Init (fld, Save_Str(ST_name(fld_st)), 
              Make_Pointer_Type (TY_IDX(ST_type(fld_st))), offset);
    offset += target_32bit ? 4 : 8;
  }
  Set_FLD_last_field (fld);

  char st_name[32];
  sprintf ( st_name, "__mp_cpprv_%d", ST_st_idx(fld_st) );
  st = New_ST(CURRENT_SYMTAB);
  ST_Init (st,
           Save_Str(st_name),
           CLASS_VAR,
           SCLASS_AUTO,
           EXPORT_LOCAL,
           struct_ty_idx);
  Set_ST_base (st, st);

  WN *wn;
  /* Generate a series of store stmt to get the address of each
     copyprivate variable */
  fld = TY_flist(Ty_Table[ST_type(st)]);

  for (wnx = copyprivates; wnx; wnx = WN_next(wnx), fld = FLD_next(fld))
  {
    TY_IDX ty = FLD_type(fld);
    wn = WN_Stid ( TY_mtype(ty), FLD_ofst(fld), st, 
                   ty, WN_Lda (Pointer_type,0,WN_st(wnx)) );
#ifdef KEY // bug 8764
    Set_ST_addr_saved(WN_st(wnx));
#endif
    WN_linenum(wn) = line_number;
    WN_INSERT_BlockLast(*copyprivate_blockp, wn);
  }

  /* Generate a procedure call for copyprivate */
  wn = WN_Create ( OPC_VCALL, 3 );
  WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_COPYPRIVATE);

  WN_Set_Call_Non_Data_Mod ( wn );
  WN_Set_Call_Non_Data_Ref ( wn );
  WN_Set_Call_Non_Parm_Mod ( wn );
  WN_Set_Call_Non_Parm_Ref ( wn );
  WN_Set_Call_Parm_Mod ( wn );
  WN_Set_Call_Parm_Ref ( wn );
  WN_linenum(wn) = line_number;
  WN_kid0(wn) = WN_CreateParm ( MTYPE_I4,
                                ldid_lock,
                                Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE );

  WN_kid1(wn) = WN_CreateParm (Pointer_type,
                               WN_Lda(Pointer_type, 0, st),
                               Make_Pointer_Type ( ST_type(st),FALSE ),
                               WN_PARM_BY_REFERENCE );
  /* Create a procedure */

  Push_Some_Globals( );
  ST *copyproc = Create_Copyfunc(st);
  Pop_Some_Globals( );

  WN_kid2(wn) = WN_CreateParm (Pointer_type,
                               WN_Lda(Pointer_type, 0, copyproc),
                               Make_Pointer_Type ( ST_pu_type(copyproc),FALSE ),
                               WN_PARM_BY_REFERENCE );

  WN_INSERT_BlockLast(*copyprivate_blockp, wn);
}
#endif
/*  Walk the reduction, last_local, local, and firstprivate lists and add the
    contents to the VAR_TABLE table.  If firstprivate_blockp is non-NULL,
    then (*firstprivate_blockp) must be NULL, and the code to initialize
    values of FIRSTPRIVATE variables is returned in (*firstprivate_blockp);
    if the value of (*firstprivate_blockp) is NULL upon return, no such
    code was generated.  Accumulate code to allocate dynamic arrays in
    (*alloca_blockp).  */

static void 
Create_Local_Variables ( VAR_TABLE * vtab, WN * reductions,
				     WN * lastlocals, WN * locals,
				     WN * firstprivates, 
				     WN **firstprivate_blockp,
				     WN * lastthread,
				     WN ** alloca_blockp )
{
  WN        *l;
  OPERATOR   opr;
  VAR_TABLE *v = vtab;
  BOOL is_non_combined_worksharing = FALSE;

  switch (mpt) {
  case MPP_SINGLE:
  case MPP_ORPHANED_SINGLE:
  case MPP_PDO:
  case MPP_ORPHANED_PDO:
#ifdef KEY /* Bug 4828 */
  case MPP_WORKSHARE:
  case MPP_ORPHANED_WORKSHARE:
#endif
    is_non_combined_worksharing = TRUE;
    break;
  case MPP_PARALLEL_DO:
  case MPP_PARALLEL_REGION:
#ifdef KEY
  case MPP_ORPHAN:
#endif
    break;
  default:
    Fail_FmtAssertion("illegal construct, mpt == %d", (INT) mpt);
  }

#ifdef Is_True_On
  if (is_non_combined_worksharing) {
      /* PV 656616 : In a worksharing construct, pregs should never appear
         in a reduction, lastlocal, or firstprivate pragma. In the non-
         orphaned case, they should have been scoped shared on the
	 parallel region and replaced by temps during Process_Preg_Temps().
	 In the orphaned case, it's a reprivatization error. */
    WN *ll[] = { reductions, lastlocals, firstprivates, NULL };
    INT i;

    for (i = 0; ll[i]; i++)
      for (l = ll[i]; l; l = WN_next(l))
        Verify_No_Pregs_In_Tree(l);
  }
#endif

  /* Do reductions */
  for (l = reductions; l; l = WN_next(l)) {
    if (WN_opcode(l) == OPC_PRAGMA) {
	VAR_TYPE reduction_var_type = VAR_REDUCTION_SCALAR;
	if( WN_pragma_omp(l) && WN_st(l) && 
			Is_Kind_Array(WN_st(l)) == TRUE)
//	  ( TY_kind( ST_type( WN_st( l ))) == KIND_ARRAY ))
		reduction_var_type = VAR_REDUCTION_ARRAY_OMP;

	Localize_Variable( v, reduction_var_type,
                   (OPERATOR) WN_pragma_arg2(l), NULL, WN_st(l),
                    WN_offsetx(l), firstprivate_blockp, alloca_blockp,
                    NULL );
        v++;
    } else {
      opr = WN_operator(WN_kid0(l));
      if ((opr == OPR_LDA) || (opr == OPR_LDID)) {
	      Localize_Variable ( v, VAR_REDUCTION_SCALAR,
			    (OPERATOR) WN_pragma_arg2(l), NULL,
			    WN_st(WN_kid0(l)), WN_offsetx(WN_kid0(l)),
			    firstprivate_blockp, alloca_blockp, NULL );
	      v++;
      } else if (opr == OPR_ARRAY) {
	     Localize_Variable ( v, VAR_REDUCTION_ARRAY,
	                         (OPERATOR) WN_pragma_arg2(l),
                                 WN_COPY_Tree ( WN_kid0(l) ),
			         WN_st(WN_kid0(WN_kid0(l))),
			         WN_offsetx(WN_kid0(WN_kid0(l))),
			         firstprivate_blockp, alloca_blockp, NULL );
	     v++;
#ifdef KEY // bug 9112 : handle the extra offset specified by an ADD
      } else if (opr == OPR_ADD && 
      		 WN_operator(WN_kid0(WN_kid0(l))) == OPR_ARRAY) {
	     INT ofst = WN_const_val(WN_kid1(WN_kid0(l)));
	     Localize_Variable ( v, VAR_REDUCTION_ARRAY,
	                         (OPERATOR) WN_pragma_arg2(l),
                                 WN_COPY_Tree ( WN_kid0(WN_kid0(l)) ),
			         WN_st(WN_kid0(WN_kid0(WN_kid0(l)))),
			         WN_offsetx(WN_kid0(WN_kid0(WN_kid0(l))))+ofst,
			         firstprivate_blockp, alloca_blockp, NULL );
	     v++;
#endif
      } else {
        Fail_FmtAssertion ( "invalid reduction directive" );
      }
    }
  }

  /* Do lastlocals */
  for (l = lastlocals; l; l = WN_next(l)) {
    if (WN_pragma_arg2(l) & SHARED_DEADOUT)
      Localize_Variable ( v, VAR_LOCAL, OPERATOR_UNKNOWN, NULL, WN_st(l),
                          WN_offsetx(l), firstprivate_blockp,
			  alloca_blockp, NULL );
    else
      Localize_Variable ( v, VAR_LASTLOCAL, OPERATOR_UNKNOWN, NULL,
                          WN_st(l), WN_offsetx(l), firstprivate_blockp,
			  alloca_blockp, NULL );
    v++;
  }
   
  /* Do locals */
  for (l = locals; l; l = WN_next(l)) {
    Localize_Variable ( v, VAR_LOCAL, OPERATOR_UNKNOWN, NULL, WN_st(l),
                        WN_offsetx(l), firstprivate_blockp, alloca_blockp,
			NULL );
    if (lastthread && (v->orig_st == WN_st(lastthread)) &&
	(v->orig_offset == WN_offsetx(lastthread))) {
      WN_st_idx(lastthread) = ST_st_idx(v->new_st);
      WN_set_offsetx(lastthread, v->new_offset);
    }
    v++;
  }

  /* Do firstprivates */
  for (l = firstprivates; l; l = WN_next(l)) {
      // Search for LASTLOCAL entry for l in vtab.
    VAR_TABLE *llv = vtab;

    for ( ; llv->orig_st; llv++)
      if (WN_st(l) == llv->orig_st &&
          (!llv->has_offset || WN_offsetx(l) == llv->orig_offset)) {
          // VAR_LOCAL can happen for SHARED_DEADOUT
        Is_True(llv->vtype == VAR_LASTLOCAL || llv->vtype == VAR_LOCAL,
                ("impossible combination of variable types"));
	break;
      }

    if (!llv->orig_st)
      llv = NULL;

    Localize_Variable ( v, VAR_FIRSTPRIVATE, OPERATOR_UNKNOWN, NULL,
                        WN_st(l), WN_offsetx(l), firstprivate_blockp,
			alloca_blockp, llv );
    if (lastthread && (v->orig_st == WN_st(lastthread)) &&
	(v->orig_offset == WN_offsetx(lastthread))) {
      WN_st_idx(lastthread) = ST_st_idx(v->new_st);
      WN_set_offsetx(lastthread, v->new_offset);
    }
    v++;
  }

}


/*  Determine the final (accumulating) reduction op based on the primary
    reduction op.  This is necessary because OPR_SUB and OPR_DIV must be
    handled specially.  While the individual threads perform a SUB/DIV the
    final reduction op must be ADD/MUL.  */

static OPCODE 
Make_Final_Reduction_Op ( OPCODE op )
{
  OPERATOR opr = OPCODE_operator(op);

  if (opr == OPR_SUB)
      return (OPCODE_make_op ( OPR_ADD, OPCODE_rtype(op), MTYPE_V ));
  else if (opr == OPR_DIV)
      return (OPCODE_make_op ( OPR_MPY, OPCODE_rtype(op), MTYPE_V ));
  else
      return (op);
}


/*
Set ST of *new_initv to localized version of ST of *old_initv (or just
ST of *old_initv, if it's not localized).
*/

static void
Localize_INITVKIND_SYMOFF(INITV_IDX new_idx, INITV_IDX old_idx, VAR_TABLE *v )
{
  Is_True(new_idx !=0, ("null new_idx"));
  Is_True(old_idx !=0, ("null old_idx"));

  INITV &new_initv = Initv_Table[new_idx];
  INITV &old_initv = Initv_Table[old_idx];

  BOOL same_initv = new_idx == old_idx;
  INITV_IDX orig_next;
  if (same_initv)
    orig_next = INITV_next(old_idx);

  Is_True(INITV_kind(new_initv) == INITVKIND_SYMOFF,
          ("wrong kind of new_initv"));
  Is_True(INITV_kind(old_initv) == INITVKIND_SYMOFF,
          ("wrong kind of old_initv"));

  INITV_Set_SYMOFF(new_initv, INITV_repeat1(old_initv), INITV_st(old_initv),
                   INITV_ofst(old_initv));
  if (same_initv)
    Set_INITV_next(new_idx, orig_next);

  for ( ; v->orig_st; v++) {
    if (ST_st_idx(*v->orig_st) == INITV_st(old_initv) &&
        (!v->has_offset || (v->orig_offset == INITV_ofst(old_initv))) ) {
      INITV_Set_SYMOFF(new_initv, INITV_repeat1(new_initv),
          ST_st_idx(*v->new_st),
	  v->has_offset ? v->new_offset : INITV_ofst(new_initv));
      if (same_initv)
        Set_INITV_next(new_idx, orig_next);
      break;
    }
  }
}


/*
Apply Localize_INITVKIND_SYMOFF() to each INITVKIND_SYMOFF in an INITO.
*/

static void
Localize_All_INITVKIND_SYMOFFs(INITO_IDX obj_idx, VAR_TABLE *vtab )
{
  Is_True(obj_idx !=0, ("null obj_idx"));

  STACK<INITV_IDX> initv_stack(Malloc_Mem_Pool);
  INITV_IDX val_idx = INITO_val(obj_idx);

  while (val_idx) {
    INITVKIND k = INITV_kind(val_idx);
    switch (k) {
      case INITVKIND_SYMOFF:
        Localize_INITVKIND_SYMOFF(val_idx, val_idx, vtab);
	val_idx = INITV_next(val_idx);
	break;

      case INITVKIND_ZERO:
      case INITVKIND_ONE:
      case INITVKIND_VAL:
      case INITVKIND_PAD:
      case INITVKIND_SYMDIFF:   // do we need to localize these next 3?
      case INITVKIND_SYMDIFF16:
      case INITVKIND_LABEL:
           val_idx = INITV_next(val_idx);
           break;

      case INITVKIND_BLOCK:
        initv_stack.Push(val_idx);
        val_idx = INITV_blk(val_idx);
        break;

      default:
	      Fail_FmtAssertion ( "unknown INITV kind %d", (INT) k );
    }

    while (!val_idx && initv_stack.Elements() > 0) {
      val_idx = INITV_next(initv_stack.Pop());
    }
  }
}


  // functor class
struct Localize_Nested_PU_Exception_Region {
  INITO_IDX old_inito;
  VAR_TABLE *vtab;
  SYMTAB_IDX level;
  Localize_Nested_PU_Exception_Region(INITO_IDX _old_inito, VAR_TABLE *_vtab,
    SYMTAB_IDX _level) : old_inito(_old_inito), vtab(_vtab), level(_level) { }
  BOOL operator() (UINT32 nested_raw_idx, const INITO *) const {
    INITO_IDX nested_inito_idx = make_INITO_IDX(nested_raw_idx, level);

    if (old_inito == nested_inito_idx) {
      Localize_All_INITVKIND_SYMOFFs(old_inito, vtab);
      return TRUE;
    }
    return FALSE;
  }
};


/*  Process C++ exception handling block.  */

static void 
Process_Exception_Region ( WN * node, VAR_TABLE * vtab )
{
  char *localname;
  INITO_IDX old_inito, new_inito;
  ST *old_initst, *new_initst;
  INITV_IDX old_initv, new_initv, pinito, parent, prev;
  STACK<INITV_IDX> old_stack(Malloc_Mem_Pool), new_stack(Malloc_Mem_Pool);

  Is_True(((WN_opcode(node) == OPC_REGION) && WN_ereg_supp(node)),
	  ("expecting region node with ereg_supp"));

#ifdef Is_True_On
  switch (mpt) {
  case MPP_SINGLE:
  case MPP_PDO:
  case MPP_PARALLEL_DO:
  case MPP_PARALLEL_REGION:
  case MPP_ORPHAN:
    break;
  default:
    Fail_FmtAssertion("not inside a PARALLEL region");
  }
#endif

#ifdef KEY
  // Don't do it if we reached here in Post_MP_Processing phase
  if (parallel_proc)
#endif
  Set_PU_has_exc_scopes(Pu_Table[ST_pu(*parallel_proc)]);

  old_inito  = WN_ereg_supp(node);

    // Fix PV 560818: Suppose old_inito is associated with the nested PU
    // (as can happen if it appears in a PDO inside a parallel region,
    // since Walk_And_Localize() gets called twice on its region).  Then
    // we localize by replacing old_inito's ST's, because if we had instead
    // created a new INITO then old_inito would have remained associated
    // with the nested PU (which is redundant, and probably incorrect) and
    // might have contained incorrect unlocalized STs.
  if (For_all_until(Inito_Table, Current_scope,
          Localize_Nested_PU_Exception_Region(old_inito, vtab,
	                                      Current_scope)) != 0)
    return;

  old_initst = INITO_st(old_inito);
  old_initv  = INITO_val(old_inito);

  localname = (char *) alloca(strlen(ST_name(old_initst)) + 32);
  sprintf ( localname, "__mpstatic_%s", ST_name(old_initst) );

  new_initst = New_ST (CURRENT_SYMTAB);
  ST_Init (new_initst,
           Save_Str ( localname ),
           ST_class(old_initst),
           ST_sclass(old_initst),
           ST_export(old_initst),
           ST_type(old_initst));

    // is blindly copying all flags correct? -- DRK
  new_initst->flags = old_initst->flags;
  new_initst->flags_ext = old_initst->flags_ext;

    // Because we're moving stmt_block out of the parent PU, and we assume
    // INITO's aren't shared with any other REGION, old_inito will no
    // longer be referenced by any REGION upon return from lower_mp(), so
    // mark its ST as unused. (If it actually is still referenced, we'll
    // hit an assertion in be/cg/cgemit.cxx.) If we don't mark it unused,
    // it may contain references to labels created by
    // Rename_Duplicate_Labels() whose WN's no longer exist in the parent,
    // which would cause a different assertion in cgemit.cxx.
  Set_ST_is_not_used(*old_initst);

  WN_ereg_supp(node) = new_inito = New_INITO ( new_initst );

  pinito = new_inito;
  parent = 0;
  prev = 0;

  while ( old_initv ) {

    (void) Initv_Table.New_entry(new_initv);
    INITV& new_initv_ref = Initv_Table[new_initv];
    INITV& old_initv_ref = Initv_Table[old_initv];

    if (pinito) {
      Set_INITO_val(pinito, new_initv);
      pinito = 0;
    } else if (parent) {
      Set_INITV_blk(parent, new_initv);
      parent = 0;
    } else if (prev) {
      Set_INITV_next(prev, new_initv);
    }

    INITVKIND k = INITV_kind(old_initv);
    switch ( k ) {

    case INITVKIND_SYMOFF:
      INITV_Set_SYMOFF(new_initv_ref, INITV_repeat1(old_initv_ref),
          INITV_st(old_initv_ref), INITV_ofst(old_initv_ref));
      Localize_INITVKIND_SYMOFF(new_initv, old_initv, vtab);
      old_initv = INITV_next(old_initv);
      prev = new_initv;
      break;

    case INITVKIND_ZERO:
      INITV_Set_ZERO(new_initv_ref, INITV_mtype(old_initv_ref),
          INITV_repeat2(old_initv_ref));
      old_initv = INITV_next(old_initv);
      prev = new_initv;
      break;

    case INITVKIND_ONE:
      INITV_Set_ONE(new_initv_ref, INITV_mtype(old_initv_ref),
          INITV_repeat2(old_initv_ref));
      old_initv = INITV_next(old_initv);
      prev = new_initv;
      break;

    case INITVKIND_VAL:
      INITV_Set_VAL(new_initv_ref, INITV_tc(old_initv_ref),
          INITV_repeat2(old_initv_ref));
      old_initv = INITV_next(old_initv);
      prev = new_initv;
      break;

    case INITVKIND_BLOCK:
      INITV_Set_BLOCK(new_initv_ref, INITV_repeat1(old_initv_ref), 0);
      old_stack.Push(old_initv);
      new_stack.Push(new_initv);
      old_initv = INITV_blk(old_initv);
      parent = new_initv;
      prev = 0;
      break;

    case INITVKIND_PAD:
      INITV_Set_PAD(new_initv_ref, INITV_pad(old_initv_ref));
      old_initv = INITV_next(old_initv);
      prev = new_initv;
      break;

    case INITVKIND_SYMDIFF:
    case INITVKIND_SYMDIFF16:
      INITV_Set_SYMDIFF(new_initv_ref, INITV_repeat1(old_initv_ref),
          INITV_lab1(old_initv_ref), INITV_st2(old_initv_ref),
          INITV_kind(old_initv) == INITVKIND_SYMDIFF16);
      old_initv = INITV_next(old_initv);
      prev = new_initv;
      break;

    case INITVKIND_LABEL:
      INITV_Set_LABEL(new_initv_ref, INITV_repeat1(old_initv_ref),
                      Translate_Label(INITV_lab(old_initv_ref)));
      old_initv = INITV_next(old_initv);
      prev = new_initv;
      break;

    default:
      Fail_FmtAssertion ( "unknown INITV kind %d", (INT) k );

    }

    while (!old_initv && old_stack.Elements() > 0) {
      old_initv = INITV_next(old_stack.Pop());
      prev = new_stack.Pop();
    }

  }
  
}


/*
To fix PV 553472, Walk_and_Localize() has to update alias information when
renaming variables in orphaned MP constructs.  These updates require that
we maintain parent pointers.  The following data structure allows
Walk_and_Localize() to maintain a list of parent pointers, for orphaned
constructs only.
*/

class Localize_Parent_Stack {
public:
  BOOL orphaned;
  STACK<WN *> parent_stack; // a node's parent is always below it on the stack
    // If _orphaned is FALSE, wn may be NULL; otherwise wn must be the top
    // of the Whirl tree to be localized
  Localize_Parent_Stack(BOOL _orphaned, WN *top) : orphaned(_orphaned),
    parent_stack(Malloc_Mem_Pool) { Push(top); }
  void Push(WN *wn) { if (orphaned) parent_stack.Push(wn); }
  void Pop() { if (orphaned) (void) parent_stack.Pop(); }
};


#define OMP_NON_POD_LASTLOCAL_FLAG_NAME "__omp_non_pod_lastlocal"
#define OMP_NON_POD_FIRST_AND_LASTLOCAL_FLAG_NAME \
        "__omp_non_pod_first_and_lastlocal"

/*
Return TRUE if wn is an IF node that looks like this:

   IF
     I4I4LDID 0 <1,84,st_name> T<4,.predef_I4,4>
     I4INTCONST 0 (0x0)
    I4I4NE
   THEN
    ....
   END_IF

where st_name can be either __omp_non_pod_lastlocal or
__omp_non_pod_first_and_lastlocal.

If return value is TRUE, return TRUE in *is_first_and_last if st_name
was __omp_non_pod_first_and_lastlocal, FALSE otherwise.
*/

BOOL 
Is_Nonpod_Finalization_IF(WN *wn, BOOL *is_first_and_last)
{
  if (WN_operator(wn) != OPR_IF)
    return FALSE;

  WN *test = WN_if_test(wn);
  if (WN_operator(test) != OPR_NE)
    return FALSE;

  WN *ldid = WN_kid0(test), *intconst = WN_kid1(test);
  ST *ldid_st;
  if (WN_operator(ldid) != OPR_LDID ||
      (ldid_st = WN_st(ldid)) == NULL)
    return FALSE;

  BOOL first_and_last;

  if (strcmp(ST_name(*ldid_st), OMP_NON_POD_LASTLOCAL_FLAG_NAME) == 0)
    first_and_last = FALSE;
  else if (strcmp(ST_name(*ldid_st),
                  OMP_NON_POD_FIRST_AND_LASTLOCAL_FLAG_NAME) == 0)
    first_and_last = TRUE;
  else
    return FALSE;

  if (WN_operator(intconst) != OPR_INTCONST ||
      WN_rtype(intconst) != MTYPE_I4 ||
      WN_const_val(intconst) != 0)
    return FALSE;

  *is_first_and_last = first_and_last;
  return TRUE;
}


/*
Walk the tree, replacing global references with local ones.  Within
parallel regions, also translate label numbers from those of the parent PU
to those of the child, and generate new INITO/INITV structures (for e.g.
C++ exception handling blocks) for the child PU.

Argument is_par_region must be TRUE iff tree is an MP construct that's a
parallel region.

In a non-recursive call to this routine, output argument
non_pod_finalization must point to a NULL WN *. Upon return,
(*non_pod_finalization) points to the non-POD finalization IF node (if one
was found in the tree), and this IF node is removed from the tree; the IF
node cannot have been the "tree" argument in the non-recursive call.

Note that within orphaned worksharing constructs, non-POD variables have
been localized already by the frontend, so we don't rewrite references to
such variables that appear in vtab.

In a non-recursive call to this routine, it is guaranteed that if the root
node of tree is not a load or store (e.g. it's a DO_LOOP or block), that
root node will not be replaced.
*/

static WN *
Walk_and_Localize (WN * tree, VAR_TABLE * vtab, Localize_Parent_Stack * lps,
                   BOOL is_par_region, WN **non_pod_finalization)
{
  OPCODE op;
  OPERATOR opr;
  INT32 i;
  WN *r;
  WN *temp;
  ST *old_sym;
  WN_OFFSET old_offset;
  VAR_TABLE *w;
  const BOOL is_orphaned_worksharing =
                (mpt == MPP_ORPHANED_PDO || mpt == MPP_ORPHANED_SINGLE);

  /* Ignore NULL subtrees. */

  if (tree == NULL)
    return (tree);

  /* Initialization. */

  op = WN_opcode(tree);
  opr = OPCODE_operator(op);

  /* Look for and replace any nodes referencing localized symbols */

  if (opr == OPR_LDID) {
    old_sym = WN_st(tree);
    old_offset = WN_offsetx(tree);
    for (w=vtab; w->orig_st; w++) {
      if ((w->orig_st == old_sym) &&
	  (w->has_offset ? (w->orig_offset == old_offset) : TRUE ) &&
          (w->vtype != VAR_REDUCTION_ARRAY) &&
	  ! (w->is_non_pod && is_orphaned_worksharing)) {
	if (w->is_static_array) {
	  temp = WN_Lda ( Pointer_type, w->new_offset, w->new_st);
	    // PV 682222: if the MP lowerer introduces LDA's on privatized
	    // ST's, we must run PU_adjust_addr_flags() before we run WOPT
	  Set_BE_ST_pu_needs_addr_flag_adjust (PU_Info_proc_sym(Current_PU_Info));
	  WN_Delete ( tree );
	  tree = temp;
	  // Don't do the following; it depends on a back-end-specific table
	  // that doesn't exist until we're compiling the nested MP
	  // procedure, and addr_used_locally is recomputed by the optimizer
	  // in any case.
	  // Set_BE_ST_addr_used_locally(w->new_st);
	  op = WN_opcode(tree);
	  opr = OPCODE_operator(op);
	} else {
          WN_st_idx(tree) = ST_st_idx(w->new_st);
	  // for reduction of a field of STRUCT, the TY_kind would be different
	  // And, we need to fix the TY for the wn, the field_id, and offsetx
	  // As the local_xxx symbol is always .predef..., so field_id should be 0
	  if (TY_kind(ST_type(w->new_st)) != TY_kind(WN_ty(tree))){
            WN_set_ty(tree, ST_type(w->new_st));
	    WN_set_field_id(tree, 0);
	  }
	  if (w->has_offset)
	    WN_set_offsetx(tree, w->new_offset);
	}
	if (w->is_dynamic_array) {  // fix PV 553472 by updating aliases
            // child of ldst that's on the path to tree
          WN *ldst, *ldst_child = tree;

            // search for first parent that's ILOAD or ISTORE
          for (INT i = 0; i < lps->parent_stack.Elements();
	       i++, ldst_child = ldst) {
            ldst = lps->parent_stack.Top_nth(i);
	    opr = WN_operator(ldst);

              // kid0 of ARRAY must be on path to tree (i.e., tree must
              // be part of the base address computation)
            if (opr == OPR_ARRAY && ldst_child != WN_kid0(ldst))
              break;
              // kid1 of ISTORE must be on path to tree (i.e., tree must
              // be part of the store address computation)
            if (opr == OPR_ISTORE && ldst_child != WN_kid1(ldst))
              break;

            if (opr != OPR_ILOAD && opr != OPR_ISTORE)
              continue;

              // this tells WOPT that despite the fact that w->new_st has
              // the PT_TO_UNIQUE_MEM bit set, ldst in fact accesses memory
              // that's pointed to by w->new_st
            Erase_Restricted_Mapping(ldst);
          }
	}
	break;
      }
    }
  } else if (opr == OPR_STID) {
    old_sym = WN_st(tree);
    old_offset = WN_offsetx(tree);
    for (w=vtab; w->orig_st; w++) {
      if ((w->vtree == NULL) &&
	  (w->orig_st == old_sym) &&
	  (w->has_offset ? (w->orig_offset == old_offset) : TRUE ) &&
	  ! (w->is_non_pod && is_orphaned_worksharing)) {
	WN_st_idx(tree) = ST_st_idx(w->new_st);
	if (TY_kind(ST_type(w->new_st)) != TY_kind(WN_ty(tree))){
	  WN_set_ty(tree, ST_type(w->new_st));
	  WN_set_field_id(tree, 0);
	}
	if (w->has_offset)
	  WN_set_offsetx(tree, w->new_offset);
	break;
      }
    }
  } else if (opr == OPR_ILOAD) {
    for (w=vtab; w->orig_st; w++) {
      if ((w->vtree && (WN_Compare_Trees(w->vtree, WN_kid0(tree)) == 0)) ||
	  (w->vtreex && (WN_Compare_Trees(w->vtreex, WN_kid0(tree)) == 0))) {
        Is_True(! w->is_non_pod, ("non-POD pointer expression?!?"));
	temp = WN_RLdid ( Promote_Type(w->mtype), w->mtype, w->new_offset,
			  w->new_st, w->ty );
#ifdef KEY // bug 10707: honor the type in the original tree node
	if (WN_rtype(tree) == MTYPE_F8 && WN_rtype(temp) == MTYPE_C8 ||
	    WN_rtype(tree) == MTYPE_F4 && WN_rtype(temp) == MTYPE_C4) {
	  WN_set_rtype(temp, WN_rtype(tree));
	  WN_set_desc(temp, WN_desc(tree));
	}
#endif
	WN_DELETE_Tree ( tree );
	tree = temp;
	op = WN_opcode(tree);
	opr = OPCODE_operator(op);
	break;
      }
    }
#ifdef KEY
    if (opr == OPR_ILOAD)  // There was no match above
      WN_kid0(tree) = Walk_and_Localize ( WN_kid0(tree), vtab, lps,
                                          is_par_region, non_pod_finalization );
#endif // KEY
  } else if (opr == OPR_ISTORE) {
    for (w=vtab; w->orig_st; w++) {
      if ((w->vtree && (WN_Compare_Trees(w->vtree, WN_kid1(tree)) == 0)) ||
	  (w->vtreex && (WN_Compare_Trees(w->vtreex, WN_kid1(tree)) == 0))) {
        Is_True(! w->is_non_pod, ("non-POD pointer expression?!?"));
	temp = WN_Stid ( w->mtype, w->new_offset, w->new_st, w->ty,
			 WN_kid0(tree) );
#ifdef KEY // bug 10707: honor the type in the original tree node
	if (WN_desc(tree) == MTYPE_F8 && WN_desc(temp) == MTYPE_C8 ||
	    WN_desc(tree) == MTYPE_F4 && WN_desc(temp) == MTYPE_C4) {
	  WN_set_desc(temp, WN_desc(tree));
	}
#endif
	WN_linenum(temp) = WN_linenum(tree);
	WN_prev(temp) = WN_prev(tree);
	if (WN_prev(temp))
	  WN_next(WN_prev(temp)) = temp;
	WN_next(temp) = WN_next(tree);
	if (WN_next(temp))
	  WN_prev(WN_next(temp)) = temp;
	WN_DELETE_Tree ( WN_kid1(tree) );
	WN_Delete ( tree );
	tree = temp;
	op = WN_opcode(tree);
	opr = OPCODE_operator(op);
	break;
      }
    }
    // PV 600983: don't translate labels (or exception regions) inside
    // orphaned constructs
  } else if ((op == OPC_REGION) && WN_ereg_supp(tree) &&
             !is_orphaned_worksharing) {
    Process_Exception_Region ( tree, vtab );
  } else if (OPCODE_has_label(op) && is_par_region && 
             !is_orphaned_worksharing) {
    LABEL_IDX new_lab =  Translate_Label (WN_label_number(tree));
    WN_label_number(tree) = new_lab;
    if (opr == OPR_LDA_LABEL) Set_LABEL_addr_saved(new_lab); // Bug 13821
  } else if (OPCODE_has_sym(op) && WN_st(tree)) {
    old_sym = WN_st(tree);
    old_offset = OPCODE_has_offset(op) ? WN_offsetx(tree) : 0;
    for (w=vtab; w->orig_st; w++) {
      if ((w->vtype != VAR_REDUCTION_ARRAY) &&
	  (w->orig_st == old_sym) &&
	  (w->has_offset ? (w->orig_offset == old_offset) : TRUE ) &&
	  ! (w->is_non_pod && is_orphaned_worksharing)) {
	WN_st_idx(tree) = ST_st_idx(w->new_st);
	if (OPCODE_has_offset(op) && w->has_offset)
	  WN_set_offsetx(tree, w->new_offset);
	break;
      }
    }
  }

  /* Walk all children */

  lps->Push(tree);
  if (op == OPC_BLOCK) {
    r = WN_first(tree);
    while (r) { // localize each node in block
      r = Walk_and_Localize ( r, vtab, lps, is_par_region, 
                              non_pod_finalization );
      if (WN_prev(r) == NULL)
        WN_first(tree) = r;
      if (WN_next(r) == NULL)
        WN_last(tree) = r;

      if (Is_Nonpod_Finalization_IF(r, &non_pod_first_and_lastprivate)) {
        if (*non_pod_finalization)
          Fail_FmtAssertion("already found non-POD finalization IF");
          // remove mem barriers around the IF
        WN *bar1 = WN_prev(r), *bar2 = WN_next(r), *then = WN_then(r),
           *bar3 = WN_first(then), *bar4 = WN_last(then);
        Is_True(bar1 && WN_operator(bar1) == OPR_FORWARD_BARRIER,
	        ("bad bar1"));
        Is_True(bar2 && WN_operator(bar2) == OPR_BACKWARD_BARRIER,
	        ("bad bar2"));
        Is_True(bar3 && WN_operator(bar3) == OPR_BACKWARD_BARRIER,
	        ("bad bar3"));
        Is_True(bar4 && WN_operator(bar4) == OPR_FORWARD_BARRIER,
	        ("bad bar4"));
        WN_DELETE_FromBlock(tree, bar1);
        WN_DELETE_FromBlock(tree, bar2);
        WN_DELETE_FromBlock(then, bar3);
        WN_DELETE_FromBlock(then, bar4);
          // extract finalization code without localizing it
        *non_pod_finalization = r;
        WN *tmp = WN_next(r);
        WN_EXTRACT_FromBlock(tree, r);
        r = tmp;
      } else {
      	r = WN_next(r);
      }
    }
  } else {
    for (i=0; i < WN_kid_count(tree); i++)
      WN_kid(tree, i) = Walk_and_Localize ( WN_kid(tree, i), vtab, lps,
                                            is_par_region,
					                                  non_pod_finalization );
  }
  lps->Pop();

  return (tree);
}   


/*
* Create any needed temporaries in local scope. especially for Dos 
* This function has been modified by csc.
*/


static void 
Make_Local_Temps ( void )
{
    // note that if multiple orphaned PDO's appear in a PU, then multiple
    // instances of these local variables will be inserted into the PU's
    // symbol table
  //Maybe need to be changed to Create_Temp
  Create_Preg_or_Temp( do_index_type, "temp_limit", &limit_st, &limit_ofst );

  Create_Temp( do_index_type, "do_upper", &local_upper );
  Set_ST_addr_passed( local_upper );

  Create_Temp( do_index_type, "do_lower", &local_lower );
  Set_ST_addr_passed( local_lower );

  Create_Temp( do_index_type, "do_stride", &local_stride );
  Set_ST_addr_passed( local_stride );

  Create_Temp( MTYPE_I4, "last_iter", &last_iter );
  Set_ST_addr_passed( last_iter );

}


/***********************************************************************
 *
 * Return a global TY of size 128-bytes, aligned at 128-bytes.
 * Useful for unioning with lock variables.
 *
 ***********************************************************************/
/* To replace static TY_IDX Lock_Padding_TY () with static void Create_Lock_Type( void ) */
// CAN BE DELETED. 
// since no more padding needed for Intel RTL lock type.

static TY_IDX 
Lock_Padding_TY () 
{

  static TY_IDX arr_ty = TY_IDX_ZERO;

  if (arr_ty == TY_IDX_ZERO) 
  {
      // define arr_ty as an array of 128 bytes
    TY& ty = New_TY (arr_ty);
    TY_Init (ty, 128, KIND_ARRAY, MTYPE_UNKNOWN, Save_Str("__lock_pad_type"));
    Set_TY_etype(ty, Be_Type_Tbl(MTYPE_I1));

    ARB_HANDLE arb = New_ARB ();
    ARB_Init (arb, 1, 1, 1);
    Set_ARB_dimension(arb, 1);
    Set_ARB_first_dimen(arb);
    Set_ARB_last_dimen(arb);
    Set_ARB_const_lbnd(arb);
    Set_ARB_lbnd_val(arb,0);
    Set_ARB_const_ubnd(arb);
    Set_ARB_ubnd_val(arb,128-1);
    Set_ARB_const_stride(arb);
    Set_ARB_stride_val(arb,1);

    Set_TY_arb(ty, arb);
    Set_TY_align (arr_ty, 128);
  }

  return arr_ty;
}

/*  Create named critical lock temporary in parent scope. */ 
// need a parent scope lock, not a common scope one?
static ST * 
Create_Critical_Lock ( void ) 
{
  ST *st;
  char st_name[32];

  st = New_ST (GLOBAL_SYMTAB);
  sprintf ( st_name, "__mplock_%d", ++lock_id );
  Create_Lock_Type( );
  ST_Init (st,
           Save_Str (st_name),
           CLASS_VAR,
           SCLASS_COMMON,
           EXPORT_PREEMPTIBLE,
           lock_ty_idx );

  Set_ST_addr_passed(st);

  shared_table[shared_count++] = st;

  
//  /* pad out to a cache-line. Union it with an 128-byte character array */
//  {
//    char name[64];
//    ST *pad;
//
//   sprintf (name, "%s_pad", st_name);
//  pad = New_ST (GLOBAL_SYMTAB);
//    ST_Init (pad,
//            Save_Str (name),
//             CLASS_VAR,
//             SCLASS_FSTATIC,
//             EXPORT_LOCAL,
//             Lock_Padding_TY());
//
//    Set_ST_addr_passed (pad);
//
//    St_Block_Union (st, pad);
//  }

  critical_lock_not_init = TRUE;

  return (st);
}

/*  Create unnamed critical lock temporary in parent scope. */
/*  lock lg */
static ST * 
Create_Unnamed_Critical_Lock ( void )
{
  if( unnamed_lock_st != NULL )
  {
    critical_lock_not_init = FALSE;
    return unnamed_lock_st;
  }
  ST *st;
  char st_name[32];

  st = New_ST (GLOBAL_SYMTAB);
  // The naming convention should be fixed to guarantee its uniform
  sprintf ( st_name, "__mplock_0" );
  Create_Lock_Type( );
  ST_Init (st,
           Save_Str (st_name),
           CLASS_VAR,
           SCLASS_COMMON,
           EXPORT_PREEMPTIBLE,
           lock_ty_idx );

  Set_ST_addr_passed(st);
//  Set_ST_is_initialized(st);
//  Set_ST_init_value_zero(st);

  shared_table[shared_count++] = st;

// 
//  /* pad out to a cache-line. Union it with an 128-byte character array */
//  {
//    char name[64];
//    ST *pad;
//
//    sprintf (name, "%s_pad", st_name);
//    pad = New_ST (GLOBAL_SYMTAB);
//    ST_Init (pad,
//             Save_Str (name),
//             CLASS_VAR,
//             SCLASS_FSTATIC,
//             EXPORT_LOCAL,
//             Lock_Padding_TY());
//
//   Set_ST_addr_passed (pad);
//    St_Block_Union (st, pad);
//  }

  critical_lock_not_init = TRUE;
  unnamed_lock_st = st;

  return (st);
}


/*  Create lock variable for named lock i.e. in a COMMON block.
 *  lock_name is the ST for the lock-name.
 */
static ST * Create_Name_Lock (ST* lock_name)
{
  ST *st;
//  ST *base_st;
  char *name;

  name = (char*) alloca (Targ_String_Length(ST_tcon_val(lock_name)) + 30);

  // For the IA64's cache line is 32 byte, so 
  // The padding is not need, But I'm not very
  // sure, need to be fixed. by csc.
/*  static TY_IDX struct_ty = 0;

 if (struct_ty == TY_IDX_ZERO) {
    // Create a struct for the common 
    FLD_HANDLE field = New_FLD ();
    FLD_Init(field, Save_Str("padding"), Lock_Padding_TY(), 0);
    Set_FLD_last_field(field);

    // Create a struct type with the above fields 
    TY &ty = New_TY(struct_ty);
    TY_Init(ty, TY_size(FLD_type(field)), KIND_STRUCT, MTYPE_M,
            Save_Str("padding_type"));
    Set_TY_fld(ty, field);
    Set_TY_align(struct_ty, TY_align(FLD_type(field)));
  }

  // Now create the ST entry for the COMMON block 
  name = (char*) alloca (Targ_String_Length(ST_tcon_val(lock_name)) + 30);
  sprintf (name, "__namelock_common_%s",
           Targ_String_Address(ST_tcon_val(lock_name)));

  base_st            = New_ST(GLOBAL_SYMTAB);
  ST_Init(base_st, Save_Str(name), CLASS_VAR, SCLASS_COMMON,
          EXPORT_PREEMPTIBLE, struct_ty);
*/
  /* now create the real ST for the variable */
  Create_Lock_Type( );
  sprintf (name, "__namelock_%s",
           Targ_String_Address(ST_tcon_val(lock_name)));
  st = New_ST (GLOBAL_SYMTAB);
  ST_Init(st, 
          Save_Str(name), 
          CLASS_VAR, 
          SCLASS_COMMON, 
          EXPORT_PREEMPTIBLE,
          lock_ty_idx );
  Set_ST_addr_passed(st);

  critical_lock_not_init = TRUE;

  return (st);
}


/*  Do all processing necessary for pregs existing in code destined for nested
    procedure.  */

static void 
Process_Preg_Temps ( WN * tree, BOOL is_region )
{
  INT32 i;
  INT32 tcnt  = PREG_Table_Size(CURRENT_SYMTAB);
  BOOL  anytemp = FALSE;
  ST    *st;
  TY_IDX ty;
  WN    *wn;
  TYPE_ID     mtype;
  PREG_INFO *preg;
  PREG_CLASS  pclass;
  char        tempname[32];

  if (tcnt == 0)
    return;

  PREG_INFO_TABLE prit(&mp_lower_pool, tcnt, TRUE, &preg_info_table);

  /*  Find out information about all preg usage in nested mp PU being
      created.  */

  Walk_and_Info_Pregs ( tree );

  for (wn = reduction_nodes; wn; wn = WN_next(wn))
    if ((WN_opcode(wn) == OPC_PRAGMA) && WN_st(wn) &&
     	 (ST_class(WN_st(wn)) == CLASS_PREG) &&
	     !Preg_Is_Dedicated(WN_offsetx(wn)) )
      prit[Get_Preg_Idx(WN_offsetx(wn))].reduction_list = TRUE;

  for (wn = lastlocal_nodes; wn; wn = WN_next(wn))
    if (WN_st(wn) && (ST_class(WN_st(wn)) == CLASS_PREG) &&
	     !Preg_Is_Dedicated(WN_offsetx(wn)) ) {
      prit[Get_Preg_Idx(WN_offsetx(wn))].lastlocal_list = TRUE;
      prit[Get_Preg_Idx(WN_offsetx(wn))].shared_flags = WN_pragma_arg2(wn);
  }

  for (wn = local_nodes; wn; wn = WN_next(wn))
    if (WN_st(wn) && (ST_class(WN_st(wn)) == CLASS_PREG) &&
	      !Preg_Is_Dedicated(WN_offsetx(wn)) )
      prit[Get_Preg_Idx(WN_offsetx(wn))].local_list = TRUE;

  for (wn = firstprivate_nodes; wn; wn = WN_next(wn))
    if (WN_st(wn) && (ST_class(WN_st(wn)) == CLASS_PREG) &&
	      !Preg_Is_Dedicated(WN_offsetx(wn)) )
          /* Treat FIRSTPRIVATE preg just like a LOCAL one */
      prit[Get_Preg_Idx(WN_offsetx(wn))].local_list = TRUE;

  for (wn = shared_nodes; wn; wn = WN_next(wn))
    if (WN_st(wn) && (ST_class(WN_st(wn)) == CLASS_PREG) &&
	     !Preg_Is_Dedicated(WN_offsetx(wn)) ) {
      prit[Get_Preg_Idx(WN_offsetx(wn))].shared_list = TRUE;
      prit[Get_Preg_Idx(WN_offsetx(wn))].shared_flags = WN_pragma_arg2(wn);
    }

  if (if_preamble_block)
    for (wn = WN_first(if_preamble_block); wn; wn = WN_next(wn))
      if ((WN_operator(wn) == OPR_STID) &&
	  (ST_class(WN_st(wn)) == CLASS_PREG) &&
	  !Preg_Is_Dedicated(WN_offsetx(wn)) )
        prit[Get_Preg_Idx(WN_offsetx(wn))].preamble_store = TRUE;

  if (do_preamble_block)
    for (wn = WN_first(do_preamble_block); wn; wn = WN_next(wn))
      if ((WN_operator(wn) == OPR_STID) &&
	  (ST_class(WN_st(wn)) == CLASS_PREG) &&
	  !Preg_Is_Dedicated(WN_offsetx(wn)) )
        prit[Get_Preg_Idx(WN_offsetx(wn))].preamble_store = TRUE;

  /*  Process preg information and create parent temps for any livein /
      liveout values.  */

  for (i = 1; i < tcnt; i++) {
    preg = &prit[i];

    if (preg->type == MTYPE_UNKNOWN)
      pclass = PCLASS_DEADIN_DEADOUT;
    else if (preg->lastlocal_list)
      if (preg->shared_flags & SHARED_DEADOUT)
	pclass = PCLASS_DEADIN_DEADOUT;
      else
	pclass = PCLASS_DEADIN_LIVEOUT;
    else if (preg->local_list)
      if (preg->preamble_store)
        pclass = PCLASS_COPYIN_DEADOUT;
      else
        pclass = PCLASS_DEADIN_DEADOUT;
    else if (preg->reduction_list)
      pclass = PCLASS_LIVEIN_LIVEOUT;
    else if (preg->shared_list)
      if (preg->shared_flags & SHARED_DEADIN)
	if (preg->shared_flags & SHARED_DEADOUT)
	  pclass = PCLASS_DEADIN_DEADOUT;
	else
	  pclass = PCLASS_DEADIN_LIVEOUT;
      else
	if (preg->shared_flags & SHARED_DEADOUT)
	  pclass = PCLASS_LIVEIN_DEADOUT;
	else
	  pclass = PCLASS_LIVEIN_LIVEOUT;
    else if (preg->preamble_store)
      pclass = PCLASS_COPYIN_DEADOUT;
    else
      pclass = PCLASS_DEADIN_DEADOUT;
    preg->pclass = pclass;

    if (pclass != PCLASS_DEADIN_DEADOUT) {

      anytemp = TRUE;
      PREG_NUM pnum = Get_Preg_Num(i);
      mtype = preg->type;
      ty = MTYPE_To_TY ( mtype );
      sprintf ( tempname, "__mptemp_preg%d", pnum );

      st = New_ST ( );
      ST_Init ( st, Save_Str ( tempname ), CLASS_VAR, SCLASS_AUTO,
        EXPORT_LOCAL, ty );

      Set_ST_has_nested_ref ( st );

      preg->temp = st;

        // if preg is copyin or livein, spill to corresponding temp in parent
      if ((pclass == PCLASS_COPYIN_DEADOUT) ||
	  (pclass == PCLASS_COPYIN_COPYOUT) ||
	  (pclass == PCLASS_LIVEIN_DEADOUT) ||
	  (pclass == PCLASS_LIVEIN_LIVEOUT)) {
	wn = WN_Stid ( mtype, 0, st, ty, WN_LdidPreg ( mtype, pnum ));
	WN_linenum(wn) = line_number;
	if (livein_block == NULL)
	  livein_block = WN_CreateBlock ( );
	WN_INSERT_BlockLast ( livein_block, wn );
      }

        // if preg is copyin, restore from parent's temp to child's preg
      if ((pclass == PCLASS_COPYIN_DEADOUT) ||
	  (pclass == PCLASS_COPYIN_COPYOUT)) {
	wn = WN_StidIntoPreg ( mtype, pnum, MTYPE_To_PREG ( mtype ),
			       WN_RLdid ( Promote_Type(mtype), mtype, 0, st,
					  ty ));
	WN_linenum(wn) = line_number;
	if (copyin_block == NULL)
	  copyin_block = WN_CreateBlock ( );
	WN_INSERT_BlockLast ( copyin_block, wn );
      }

        // if preg is copyout, spill child's preg to parent's temp
      if ((pclass == PCLASS_DEADIN_COPYOUT) ||
	  (pclass == PCLASS_COPYIN_COPYOUT)) {
	wn = WN_Stid ( mtype, 0, st, ty, WN_LdidPreg ( mtype, pnum ));
	WN_linenum(wn) = line_number;
	if (copyout_block == NULL)
	  copyout_block = WN_CreateBlock ( );
	WN_INSERT_BlockLast ( copyout_block, wn );
      }

        // if preg is copyout or liveout, restore parent's preg from temp
      if ((pclass == PCLASS_DEADIN_COPYOUT) ||
	  (pclass == PCLASS_COPYIN_COPYOUT) ||
	  (pclass == PCLASS_DEADIN_LIVEOUT) ||
	  (pclass == PCLASS_LIVEIN_LIVEOUT)) {
	wn = WN_StidIntoPreg ( mtype, pnum, MTYPE_To_PREG ( mtype ),
			       WN_RLdid ( Promote_Type(mtype), mtype, 0, st,
					  ty ));
	WN_linenum(wn) = line_number;
	if (liveout_block == NULL)
	  liveout_block = WN_CreateBlock ( );
	WN_INSERT_BlockLast ( liveout_block, wn );
	shared_table[shared_count++] = st;
      }

    }
  }

  /*  Finally, translate appropriate preg usage to uplevel temp usage.  */

  if (anytemp) {

    Walk_and_Replace_Pregs ( tree );

    for (wn = reduction_nodes; wn; wn = WN_next(wn))
      if ((WN_opcode(wn) == OPC_PRAGMA) && WN_st(wn) &&
	  (ST_class(WN_st(wn)) == CLASS_PREG) &&
	  !Preg_Is_Dedicated(WN_offsetx(wn)) ) {
	PREG_NUM pnum = Get_Preg_Idx(WN_offsetx(wn));
	preg = &prit[pnum];
	if (preg->temp) {
	  WN_st_idx(wn) = ST_st_idx(preg->temp);
	  WN_set_offsetx(wn, 0);
	}
      } else if (WN_opcode(wn) == OPC_XPRAGMA)
	Walk_and_Replace_Pregs ( WN_kid0(wn) );

    for (wn = lastlocal_nodes; wn; wn = WN_next(wn))
      if (WN_st(wn) && (ST_class(WN_st(wn)) == CLASS_PREG) &&
	  !Preg_Is_Dedicated(WN_offsetx(wn)) ) {
	PREG_NUM pnum = Get_Preg_Idx(WN_offsetx(wn));
	preg = &prit[pnum];
	if (preg->temp) {
	  WN_st_idx(wn) = ST_st_idx(preg->temp);
	  WN_set_offsetx(wn, 0);
	}
      }

    for (wn = local_nodes; wn; wn = WN_next(wn))
      if (WN_st(wn) && (ST_class(WN_st(wn)) == CLASS_PREG) &&
	  !Preg_Is_Dedicated(WN_offsetx(wn)) ) {
	PREG_NUM pnum = Get_Preg_Idx(WN_offsetx(wn));
	preg = &prit[pnum];
	if (preg->temp) {
	  WN_st_idx(wn) = ST_st_idx(preg->temp);
	  WN_set_offsetx(wn, 0);
	}
      }

    for (wn = firstprivate_nodes; wn; wn = WN_next(wn))
      if (WN_st(wn) && (ST_class(WN_st(wn)) == CLASS_PREG) &&
	  !Preg_Is_Dedicated(WN_offsetx(wn)) ) {
	PREG_NUM pnum = Get_Preg_Idx(WN_offsetx(wn));
	preg = &prit[pnum];
	if (preg->temp) {
	  WN_st_idx(wn) = ST_st_idx(preg->temp);
	  WN_set_offsetx(wn, 0);
	}
      }

    for (wn = shared_nodes; wn; wn = WN_next(wn))
      if (WN_st(wn) && (ST_class(WN_st(wn)) == CLASS_PREG) &&
	  !Preg_Is_Dedicated(WN_offsetx(wn)) ) {
	PREG_NUM pnum = Get_Preg_Idx(WN_offsetx(wn));
	preg = &prit[pnum];
	if (preg->temp) {
	  WN_st_idx(wn) = ST_st_idx(preg->temp);
	  WN_set_offsetx(wn, 0);
	}
      }

  }
}


/*
Transfer all maps (except WN_MAP_FEEDBACK) associated with each node in the
tree from the parent mapset to the kid's.
*/

static void
Transfer_Maps_R ( WN_MAP_TAB * parent, WN_MAP_TAB * child, WN * tree,
                  RID * root_rid );

static void
Transfer_Maps ( WN_MAP_TAB * parent, WN_MAP_TAB * child, WN * tree,
                RID * root_rid )
{
    // to preserve WN_MAP_FEEDBACK in child map table, copy its contents
    // to fb_map
  HASH_TABLE<WN *, INT32> fb_map(NUM_HASH_ELEMENTS, Malloc_Mem_Pool);
  WN_ITER *wni = WN_WALK_TreeIter(tree);

  for ( ; wni; wni = WN_WALK_TreeNext(wni)) {
    WN *wn = WN_ITER_wn(wni);

    fb_map.Enter(wn, IPA_WN_MAP32_Get(child, WN_MAP_FEEDBACK, wn));
  }

  Transfer_Maps_R(parent, child, tree, root_rid); // overwrites WN_MAP_FEEDBACK

    // now restore values for WN_MAP_FEEDBACK from fb_map
  HASH_TABLE_ITER<WN *, INT32> fb_map_iter(&fb_map);
  WN *wn;
  INT32 val;

  while (fb_map_iter.Step(&wn, &val))
    IPA_WN_MAP32_Set(child, WN_MAP_FEEDBACK, wn, val);

//  parent->_is_used[WN_MAP_FEEDBACK] = is_used;  // restore the flag
} // Transfer_Maps

// this function does the real work
static void
Transfer_Maps_R ( WN_MAP_TAB * parent, WN_MAP_TAB * child, WN * tree,
                  RID * root_rid )
{
  WN *node;
  INT32 i;

  if (tree) {
    if (WN_opcode(tree) == OPC_BLOCK) {
      for (node = WN_first(tree); node; node = WN_next(node))
	Transfer_Maps_R ( parent, child, node, root_rid );
    } else
      for (i = 0; i < WN_kid_count(tree); i++)
	Transfer_Maps_R ( parent, child, WN_kid(tree, i), root_rid );

    if (WN_map_id(tree) != -1) {
      RID *rid = REGION_get_rid ( tree );
      IPA_WN_Move_Maps_PU ( parent, child, tree );
      if (WN_opcode(tree) == OPC_REGION) {
	Is_True(root_rid != NULL, ("Transfer_Maps_R, NULL root RID"));
	RID_unlink ( rid );
	RID_Add_kid ( rid, root_rid );
      } 
    }
  }
} // Transfer_Maps_R


ST_IDX Make_MPRuntime_ST ( MPRUNTIME rop )
{
  Is_True(rop >= MPRUNTIME_FIRST && rop <= MPRUNTIME_LAST,
          ("Make_MPRuntime_ST: bad rop == %d", (INT) rop));

    // If the global type doesn't exist, create it and its pointer type.
  if (mpruntime_ty == TY_IDX_ZERO) {
    TY &mpr_ty = New_TY ( mpruntime_ty );
    TY_Init(mpr_ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN,
            Save_Str(".mpruntime"));
    Set_TY_align(mpruntime_ty, 1);

    TYLIST_IDX parm_idx;
    TYLIST& parm_list = New_TYLIST(parm_idx);
    Set_TY_tylist(mpr_ty, parm_idx);
    Set_TYLIST_type(parm_list, Be_Type_Tbl(MTYPE_I4));  // I4 return type
      // are there really no parameters? -- DRK
    Set_TYLIST_type(New_TYLIST(parm_idx), TY_IDX_ZERO); // end of parm list

    TY_IDX ty_idx;
    TY &ty = New_TY ( ty_idx );
    TY_Init(ty, Pointer_Size, KIND_POINTER, Pointer_Mtype,
      Save_Str ( ".mpruntime_ptr" ));
    Set_TY_pointed(ty, mpruntime_ty);

    Set_TY_align(ty_idx, Pointer_Size); // unnecessary? TY_Init does
                                        // not set alignment -- DRK
  }

  PU_IDX pu_idx;
  PU& pu = New_PU(pu_idx);
  PU_Init(pu, mpruntime_ty, CURRENT_SYMTAB);

  /*  Create the ST, fill in all appropriate fields and enter into the */
  /*  global symbol table.  */

  ST *st = New_ST ( GLOBAL_SYMTAB );
  ST_Init(st, Save_Str ( mpr_names[rop] ), CLASS_FUNC, SCLASS_EXTERN,
    EXPORT_PREEMPTIBLE, pu_idx);

  Allocate_Object ( st );

  mpr_sts[rop] = ST_st_idx(*st);
  return mpr_sts[rop];
}


static void
Gen_MP_LS_get_fld_id_and_ty(ST *st, WN_OFFSET offset, BOOL scalar_only, UINT &field_id, TY_IDX &ty, TY_IDX &result_ty)
{
  ty = ST_type(st);
  result_ty = ty;
#ifdef KEY // bug 7259
  if (scalar_only && TY_kind(ty) == KIND_STRUCT )
  {
    FLD_HANDLE fld = Get_FLD_From_Offset(ty, offset, &field_id);
    result_ty = FLD_type(fld);
  }
#endif
#ifdef KEY // bug 10681
  if (scalar_only && TY_kind(ty) == KIND_ARRAY)
    ty = TY_etype(ty);
#endif
  return; 
}
/*  Generate an appropriate load WN based on an ST.  */

static WN *
Gen_MP_Load( ST * st, WN_OFFSET offset, BOOL scalar_only )
{
  UINT field_id = 0;
  WN *wn;
  TY_IDX ty;
  TY_IDX result_ty;
  
  Gen_MP_LS_get_fld_id_and_ty(st, offset, scalar_only, field_id, ty, result_ty);

  wn = WN_Ldid ( TY_mtype(result_ty), offset, st, ty ,field_id);

  return (wn);
}

/*  Generate an appropriate store WN based on an ST.  */

static WN *
Gen_MP_Store( ST * st, WN_OFFSET offset, WN * value, BOOL scalar_only)
{
  UINT  field_id = 0;
  WN *wn;
  TY_IDX ty;
  TY_IDX result_ty;

  Gen_MP_LS_get_fld_id_and_ty(st, offset, scalar_only, field_id, ty, result_ty);
  
  wn = WN_Stid ( TY_mtype(result_ty), offset, st, ty, value, field_id );
  WN_linenum(wn) = line_number;

  return (wn);
}

/*  Generate appropriate load/store WN's based on two ST's.  */

static WN * 
Gen_MP_Load_Store ( ST * from_st, WN_OFFSET from_offset,
				            ST * to_st,   WN_OFFSET to_offset,
				            BOOL is_dynamic )
{
  TY_IDX ty;
  TY_IDX pty;
  WN *wn;
  WN *laddr_wn;
  WN *saddr_wn;
  WN *bytes_wn;
  BOOL is_from_ptr;
  BOOL is_to_ptr;

  is_from_ptr = (TY_kind(ST_type(from_st)) == KIND_POINTER);
  is_to_ptr   = (TY_kind(ST_type(to_st)) == KIND_POINTER);
  if (is_from_ptr && is_to_ptr && !is_dynamic)
    is_from_ptr = is_to_ptr = FALSE;

  ty = (is_from_ptr) ? TY_pointed(ST_type(from_st)) : ST_type(from_st);

#ifndef KEY
  if ((TY_kind(ty) != KIND_ARRAY) && (TY_kind(ty) != KIND_STRUCT))
#else
    if (((TY_kind(ty) != KIND_ARRAY) && (TY_kind(ty) != KIND_STRUCT)) ||
	// Bug 6633 - MP lowering of C++ classes with size 0 could trigger
	TY_kind(ty) == KIND_STRUCT && TY_size(ty) == 0)
#endif
    wn = Gen_MP_Store ( to_st, to_offset,
			Gen_MP_Load ( from_st, from_offset ));
  else {
    if (TY_size(ty) > INT32_MAX)
      bytes_wn = WN_Intconst ( MTYPE_I8, TY_size(ty) );
    else if (TY_size(ty) > 0)
      bytes_wn = WN_Intconst ( MTYPE_I4, TY_size(ty) );
    else
      bytes_wn = Calculate_Array_Size ( from_st, ty );
    if (is_from_ptr)
      laddr_wn = WN_RLdid ( Promote_Type(Pointer_type), Pointer_type,
			    from_offset, from_st, ST_type(from_st) );
    else
      laddr_wn = WN_Lda ( Pointer_type, from_offset, from_st );
    if (is_to_ptr)
      saddr_wn = WN_RLdid ( Promote_Type(Pointer_type), Pointer_type,
			    to_offset, to_st, ST_type(to_st) );
    else
      saddr_wn = WN_Lda ( Pointer_type, to_offset, to_st );
    pty = Make_Pointer_Type ( ty, FALSE );
    wn = WN_CreateMstore ( 0, pty,
			   WN_CreateMload ( 0, pty, laddr_wn,
					    WN_COPY_Tree (bytes_wn) ),
			   saddr_wn, bytes_wn );
  }

  return (wn);
}

/*
Generate a barrier call. csc.
Note that, the gtid can't be preg.
*/
static WN * 
Gen_Barrier (ST* gtid)
{
  WN *wn;

  wn = WN_Create ( OPC_VCALL, 0 );
  WN_st_idx(wn) = GET_MPRUNTIME_ST ( MPR_OMP_BARRIER );
  WN_Set_Call_Non_Data_Mod ( wn );
  WN_Set_Call_Non_Data_Ref ( wn );
  WN_Set_Call_Non_Parm_Mod ( wn );
  WN_Set_Call_Non_Parm_Ref ( wn );
  WN_Set_Call_Parm_Ref ( wn );
  WN_linenum(wn) = line_number;
  
  return (wn);
}

/*
copyin_wn must be a COPYIN pragma from an OpenMP PARALLEL or
combined worksharing directive.

Returns TRUE if copyin_wn is an OMP C++ non-POD COPYIN clause, FALSE
otherwise.
*/
// CAN BE DELETED. OBSOLETE

static BOOL
is_omp_non_pod_copyin(WN *copyin_wn)
{
  if (WN_opcode(copyin_wn) != OPC_XPRAGMA ||
      !WN_pragma_omp(copyin_wn))
    return FALSE;

  WN *kid = WN_kid0(copyin_wn);
  if (!kid)
    return FALSE;

  ST *st = WN_st(kid);
  if (!st)
    return FALSE;

  return (ST_sym_class(st) == CLASS_FUNC);
}

#ifdef KEY
extern ST *ST_Source_COMMON_Block(ST *st, ST **split, BOOL want_st);
extern ST *ST_Source_Block(ST *, ST **);

static ST*
Get_Threadprv_St(WN *prags, ST *copyin_st, ST **copyin_local_st)
{
  ST *split_block;
  ST *copyin_global_st = NULL;
  ST *common_block = ST_Source_Block(copyin_st, &split_block);
  if (!ST_is_thread_private(copyin_st) &&
     !(split_block && ST_is_thread_private(split_block)) &&
     !(common_block && ST_is_thread_private(common_block)))
    ErrMsg (EC_MPLOWER_copyin_st, copyin_st);
  BOOL match = FALSE;
  prags = WN_first(prags);

  while (prags) {
    if (WN_opcode(prags) == OPC_PRAGMA &&
        WN_pragma(prags) == WN_PRAGMA_THREADPRIVATE &&
        WN_st(prags) == common_block) {
      *copyin_local_st = ST_ptr(WN_pragma_arg1(prags));
      copyin_global_st = ST_ptr(WN_pragma_arg2(prags));
      match = TRUE;
    }
    prags = WN_next(prags);
  }
  if (!match)
    Fail_FmtAssertion ( "bad copyin st (%s) in MP processing",
                            ST_name(copyin_st) );
  if (copyin_global_st)
    common_block = copyin_global_st;
  else
    Is_True (FALSE, ("Copyin source ST not found in MP processing"));

  return common_block;
}

// Called from 2 places in be driver, at the end of MP lowering of a PU.
// Generates a library call at the start of a PU to get threadprivate pointer.
// Walks through the PU and replaces remaining occurrences of private
// variables with local copies.
void
Post_MP_Processing (WN * pu)
{
  Is_True (WN_operator (pu) == OPR_FUNC_ENTRY,
           ("Post_MP_Processing: Function entry node expected"));

  local_nodes = NULL;
  local_count = 0;

  WN * prags = WN_func_pragmas (pu);
  WN * body = WN_func_body (pu);

  reference_block = WN_CreateBlock ( );
  WN *thread_priv_prag = WN_first(prags);
  while (thread_priv_prag) {
    if (WN_opcode(thread_priv_prag) == OPC_PRAGMA &&
        WN_pragma(thread_priv_prag) == WN_PRAGMA_THREADPRIVATE) {
      WN_INSERT_BlockLast ( reference_block,
			WN_CreatePragma ( WN_PRAGMA_THREADPRIVATE,
                                          WN_st_idx(thread_priv_prag),
			                  WN_pragma_arg1(thread_priv_prag), 
                                          WN_pragma_arg2(thread_priv_prag) ));
    }
    thread_priv_prag = WN_next(thread_priv_prag);
  }

  Gen_Threadpriv_Func (prags, body, TRUE);
  
  // Get the set of local nodes from the function pragmas.
  WN * prag_iter, * next_prag_iter = WN_first (prags);
  while (prag_iter = next_prag_iter /* really an assignment */)
  {
    next_prag_iter = WN_next (prag_iter);

    if (WN_operator (prag_iter) == OPR_PRAGMA &&
        WN_pragma (prag_iter) == WN_PRAGMA_LOCAL)
    {
      WN * wn = NULL;
      for (wn = local_nodes; wn; wn = WN_next(wn))
        if (Identical_Pragmas(prag_iter, wn))
          break;
      if (wn == NULL)
      {
        prag_iter = WN_EXTRACT_FromBlock (prags, prag_iter);
        WN_next(prag_iter) = local_nodes;
        local_nodes = prag_iter;
	++local_count;
      } else
        WN_DELETE_FromBlock (prags, prag_iter);
    }
  }

  // Reset global var
  parallel_proc = NULL;
  // Localize the variables
  INT32 vsize = (local_count + 1) * sizeof(VAR_TABLE);
  var_table = (VAR_TABLE *) alloca (vsize);
  BZERO (var_table, vsize);

  mpt = MPP_ORPHAN;

  Create_Local_Variables (var_table, NULL, NULL, local_nodes, NULL,
                          &firstprivate_block, NULL, &alloca_block);
  Localize_Parent_Stack lps (FALSE, NULL);
  Walk_and_Localize (body, var_table, &lps, FALSE, &non_pod_finalization_nodes);
  // Clear vector for this PU
  localized_var_vect.clear();

  // Clear local-nodes
  WN * next_node;
  while (local_nodes) {
    next_node = WN_next (local_nodes);
    WN_Delete (local_nodes);
    local_nodes = next_node;
  }
}
#endif

/*  Generate a copyin call.  */
// TODO: should be replaced when implementing TLS

static WN * 
Gen_MP_Copyin ( BOOL is_omp )
{
  WN *wn;
  WN *wnx;
  WN *wny;
#ifdef KEY
  WN *wnz;
#endif
  TY_IDX ty;
  INT32 kid;
  INT32 size;
  BOOL have_pods = FALSE;
  BOOL have_nonpods = FALSE;
  INT count = 0;
  WN *block = WN_CreateBlock();

  // First scan the COPYIN nodes to determine what all we need
  // i.e. just PODS, just non-PODS, or both.

  for (wnx = copyin_nodes; wnx; wnx = WN_next(wnx)) {

    if (is_omp && is_omp_non_pod_copyin(wnx)) {
      // wnx must be a copyin for a C++ non-POD object.
      // We need to create special calls for those, and return them in
      // a block node. Process them in the subsequent loop.
      have_nonpods = TRUE;
    }
    else {
      // non-PODS require just a single call to mp/omp_copyin.
      count++;
      have_pods = TRUE;
    }
  }

  if (have_pods) {
#ifdef KEY
    wn = WN_Create ( OPC_VCALL, 3 * count + 1 );
    WN_st_idx(wn) = GET_MPRUNTIME_ST(MPR_OMP_COPYIN_THDPRV);
#else
    wn = WN_Create ( OPC_VCALL, 2 * count + 1 );
    WN_st_idx(wn) = (is_omp ? GET_MPRUNTIME_ST (MPR_OMP_COPYIN) :
                     GET_MPRUNTIME_ST ( MPR_COPYIN ));
#endif
    WN_Set_Call_Non_Data_Mod ( wn );
    WN_Set_Call_Non_Data_Ref ( wn );
    WN_Set_Call_Non_Parm_Mod ( wn );
    WN_Set_Call_Non_Parm_Ref ( wn );
    WN_Set_Call_Parm_Mod ( wn );
    WN_Set_Call_Parm_Ref ( wn );
    WN_linenum(wn) = line_number;

#ifdef KEY
    // The library expects the first argument (n) to be (# of triplets) * 3
    WN_kid0(wn) = WN_CreateParm ( MTYPE_I4,
                                  WN_Intconst ( MTYPE_I4, 3 * count ),
                                  Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE );
    for (wnx = copyin_nodes, kid = 1; wnx; wnx = WN_next(wnx), kid += 3)
#else
    WN_kid0(wn) = WN_CreateParm ( MTYPE_I4,
                                  WN_Intconst ( MTYPE_I4, count ),
                                  Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE );
    for (wnx = copyin_nodes, kid = 1; wnx; wnx = WN_next(wnx), kid += 2)
#endif
    {

      if (is_omp && is_omp_non_pod_copyin(wnx)) {
        continue; // skip C++ non-PODs
      }
#ifdef KEY
      if (WN_opcode(wnx) == OPC_PRAGMA) {
        ST *ppthd, *global;
        global = Get_Threadprv_St(reference_block, WN_st(wnx), &ppthd);
        wny = WN_CreateLdid(OPCODE_make_op(OPR_LDID,
                                           Pointer_type, Pointer_type),
                            0, ppthd,ST_type(ppthd)); 
        // See bugs 12688, 12696 for OpenMP library change in
        // __ompc_get_thdprv() that necessitates the following.
        wnz = WN_IloadLdid(TY_mtype(ST_type(global)), 0,
                           ST_type(global), global, 0);
        ty = ST_type(WN_st(wnx));
        size = TY_size(ty);
      } else
        Fail_FmtAssertion ( "bad copyin node (%s) in MP processing",
                            OPCODE_name(WN_opcode(WN_kid0(wnx))) );
      WN_kid(wn,kid)   = WN_CreateParm ( Pointer_type, wny,
                                         Make_Pointer_Type ( ty,
                                                             FALSE ),
                                         WN_PARM_BY_REFERENCE );
      WN_kid(wn,kid+1)   = WN_CreateParm ( Pointer_type, wnz,
                                         Make_Pointer_Type ( ty,
                                                             FALSE ),
                                         WN_PARM_BY_REFERENCE );
      WN_kid(wn,kid+2) = WN_CreateParm ( MTYPE_I4,
                                         WN_Intconst ( MTYPE_I4, size ),
                                         Be_Type_Tbl(MTYPE_I4),
                                         WN_PARM_BY_VALUE );
#else
      if (WN_opcode(wnx) == OPC_PRAGMA) {
        wny = WN_Lda ( Pointer_type, WN_offsetx(wnx), WN_st(wnx) );
        ty = ST_type(WN_st(wnx));
        size = TY_size(ty);
      } else if (WN_operator(WN_kid0(wnx)) == OPR_LDA) {
        wny = WN_COPY_Tree ( WN_kid0(wnx) );
        ty = ST_type(WN_st(WN_kid0(wnx)));
        size = TY_size(ty);
      } else if ((WN_operator(WN_kid0(wnx)) == OPR_ARRAY) &&
                 (WN_operator(WN_kid0(WN_kid0(wnx))) == OPR_LDA)) {
        wny = WN_COPY_Tree ( WN_kid0(wnx) );
        ty = TY_AR_etype(ST_type(WN_st(WN_kid0(WN_kid0(wnx))))); 
        size = TY_size(ty);
      } else
        Fail_FmtAssertion ( "bad copyin node (%s) in MP processing",
                            OPCODE_name(WN_opcode(WN_kid0(wnx))) );

      WN_kid(wn,kid)   = WN_CreateParm ( Pointer_type, wny,
                                         Make_Pointer_Type ( ty,
                                                             FALSE ),
                                         WN_PARM_BY_REFERENCE );
      WN_kid(wn,kid+1) = WN_CreateParm ( MTYPE_I4,
                                         WN_Intconst ( MTYPE_I4, size ),
                                         Be_Type_Tbl(MTYPE_I4),
                                         WN_PARM_BY_VALUE );
#endif
    }
    WN_INSERT_BlockFirst (block, wn);
#ifdef KEY
    // bug 5203: Ensure all threads get the value before giving the master
    // thread a chance to proceed/modify the variable.
    WN_INSERT_BlockAfter (block, wn, Gen_Barrier (local_gtid));
#endif // KEY
  }

  if (have_nonpods) {

    for (wnx = copyin_nodes; wnx; wnx = WN_next(wnx)) {

      if (!(is_omp && is_omp_non_pod_copyin(wnx))) {
        continue; // skip PODs
      }

      ST* st = WN_st(wnx);
      TY_IDX ty = ST_type(st);
      
      if (TY_kind(ty) != KIND_ARRAY) {

        // scalar non-POD

        wn = WN_Create (OPC_VCALL, 2);
        WN_st_idx(wn) = (GET_MPRUNTIME_ST (MPR_OMP_NONPOD_COPYIN));
        WN_Set_Call_Non_Data_Mod ( wn );
        WN_Set_Call_Non_Data_Ref ( wn );
        WN_Set_Call_Non_Parm_Mod ( wn );
        WN_Set_Call_Non_Parm_Ref ( wn );
        WN_Set_Call_Parm_Mod ( wn );
        WN_Set_Call_Parm_Ref ( wn );
        WN_linenum(wn) = line_number;
      
        // pass the address of the object
        WN *obj = WN_CreateLda (OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V),
                                0,
                                Make_Pointer_Type(ty,FALSE),
                                st);
        WN_kid0(wn) = WN_CreateParm (Pointer_type,
                                     obj,
                                     Make_Pointer_Type(ty, FALSE),
                                     WN_PARM_BY_REFERENCE);
        // second argument is the address of the assignment operator
        FmtAssert (ST_export(WN_st(WN_kid0(wnx))) == EXPORT_PREEMPTIBLE,
                   ("COPYIN (%s) requires a pre-emptible assignment operator",
                    ST_name(st)));
        WN_kid1(wn) = WN_CreateParm(Pointer_type,
                                    WN_COPY_Tree(WN_kid0(wnx)),
                                    WN_ty(WN_kid0(wnx)),
                                    WN_PARM_BY_REFERENCE);
      }
      else {

        // array non-pod 
        // Since these are thread-private, they must be globals,
        // therefore they must be fixed size, and cannot be VLAs.

        wn = WN_Create (OPC_VCALL, 4);
        WN_st_idx(wn) = (GET_MPRUNTIME_ST (MPR_OMP_NONPOD_ARRAY_COPYIN));
        WN_Set_Call_Non_Data_Mod ( wn );
        WN_Set_Call_Non_Data_Ref ( wn );
        WN_Set_Call_Non_Parm_Mod ( wn );
        WN_Set_Call_Non_Parm_Ref ( wn );
        WN_Set_Call_Parm_Mod ( wn );
        WN_Set_Call_Parm_Ref ( wn );
        WN_linenum(wn) = line_number;
      
        // pass the address of the object
        WN *obj = WN_CreateLda (OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V),
                                0,
                                Make_Pointer_Type(ty, FALSE),
                                st);
        WN_kid(wn,0) = WN_CreateParm (Pointer_type,
                                     obj,
                                     Make_Pointer_Type(ty, FALSE),
                                     WN_PARM_BY_REFERENCE);

        // second argument is the address of the assignment operator
	// Is there any particular specification for C++ object tree's
	// form? what is assignment operator? == Copy constructor?
	// by csc. 2002/9/16
        FmtAssert (ST_export(WN_st(WN_kid0(wnx))) == EXPORT_PREEMPTIBLE,
                   ("COPYIN (%s) requires a pre-emptible assignment operator",
                    ST_name(st)));
        WN_kid(wn,1) = WN_CreateParm(Pointer_type,
                                     WN_COPY_Tree(WN_kid0(wnx)),
                                     WN_ty(WN_kid0(wnx)),
                                     WN_PARM_BY_REFERENCE);

        // third argument is the size of each element
        INT size;
        size = TY_size(TY_AR_etype(ty));
        WN_kid(wn,2) = WN_CreateParm(MTYPE_I8,
                                     WN_CreateIntconst (OPC_I8INTCONST,size),
                                     Be_Type_Tbl(MTYPE_I8),
                                     WN_PARM_BY_VALUE);

        // fourth argument is the number of elements
        INT64 num_elems = 1;
        for (INT i=0; i<TY_AR_ndims(ty); i++) {

          FmtAssert (TY_AR_const_ubnd(ty,i) &&
                     TY_AR_const_lbnd(ty,i) &&
                     TY_AR_const_stride(ty,i),
                     ("COPYIN array (%s) has non-const bounds. Weird!\n",
                      ST_name(st)));

          num_elems = num_elems * (TY_AR_ubnd_val(ty,i) -
                                   TY_AR_lbnd_val(ty,i) + 1);
        }
        WN* elems = WN_CreateIntconst(OPC_I8INTCONST,num_elems);
        WN_kid(wn,3) = WN_CreateParm(MTYPE_I8,
                                     elems,
                                     Be_Type_Tbl(MTYPE_I8),
                                     WN_PARM_BY_VALUE);
      }
      
      WN_INSERT_BlockLast (block, wn);
    }
  }

  return (block);
}

#define MAX_NDIM 7
/***********************************************
 * 
 *  Helper routine: creates a DO loop node
 */
static WN * 
create_doloop_node(WN *index_id, WN *start, WN *end,
		               WN *step, WN *body)
{
  WN *temp;
  WN_Set_Linenum(start,line_number);
  WN_Set_Linenum(step,line_number);
  temp = WN_CreateDO(index_id, start, end, step, body, NULL);
  WN_Set_Linenum(temp,line_number);
  return (temp);
}

/***********************************************
 * 
 *  Helper routine, creates a DO loop running from 0 to count-1
 *  with loop control variable index, with body.
 * 
 *  index(output) - the preg_num for the created loop index
 *  index_name(input) - a name for the index variable
 *  count - number of iterations to execute the loop
 *  body - the loop body
 *  adopted from be/vho/f90_lower.cxx, csc
 * ****************************************************************/

static WN * 
create_doloop(PREG_NUM *index, char *index_name, 
		          WN *count, WN *body)
{
      WN *index_id, *count_expr, *start, *end, *step, *doloop;
      OPCODE intconst_op;
      TYPE_ID index_type;
      WN *temp;

//	 need to determing the loop index type.
//	 now assume I8 all the time
//	 csc.
//	 index_type = doloop_ty;
     index_type= MTYPE_I8;
     intconst_op = OPCODE_make_op(OPR_INTCONST,index_type,MTYPE_V);

     /* Create an index */
     *index = Create_Preg(index_type,Index_To_Str(Save_Str(index_name)));

     index_id = WN_CreateIdname(*index,MTYPE_To_PREG(index_type));
     count_expr = WN_CreateExp2(OPCODE_make_op(OPR_SUB,index_type,MTYPE_V),
                                count, WN_CreateIntconst(intconst_op,1));

     start = WN_StidPreg(index_type,*index, 
			 WN_CreateIntconst(intconst_op,0));
     end = WN_CreateExp2(OPCODE_make_op(OPR_LE,MTYPE_I4,index_type),
                         WN_LdidPreg(index_type,*index),
                         count_expr);

     step = WN_CreateExp2(OPCODE_make_op(OPR_ADD,index_type,MTYPE_V),
                          WN_LdidPreg(index_type,*index),
	                  WN_CreateIntconst(intconst_op,1));

     step = WN_StidPreg(index_type,*index,step);

    /* Make sure body is a BLOCK node */
     if (WN_opcode(body) != OPC_BLOCK)
     {
       temp = WN_CreateBlock();
       WN_INSERT_BlockFirst(temp,body);
     } 
     else
     {
         temp = body;
     }

     doloop = create_doloop_node(index_id, start, 
	                         end, step, temp);
     return doloop;
}


/*
 *   Helper routine to create a loop nest
 *   indices - (output) list of indices
 *   doloop - (output) the crated doloop nest
 *   sizes - (input) iteration counts
 *   ndim - (input) number of loops
 *   
 *   returns pointer to the block node in the loopnest
 *   from be/vho/f90_lower.cxx. csc.
 */

static WN * 
create_doloop_nest(PREG_NUM indices[], WN **doloop, WN *sizes[], INT ndim)
{
   INT i;
   WN *loopnest, *stlist;
   char tempname[32];
   static int num_temps = 0;
   PREG_NUM index;

   loopnest = WN_CreateBlock();
   stlist = loopnest;
   num_temps += 1;
   for (i=ndim-1; i >= 0; i--)
   {
      sprintf(tempname,"@f90li_%d_%d",i,num_temps);

   /* Create the DO loop node */
      loopnest = create_doloop(&index,tempname,
	                    sizes[i],loopnest);
      indices[i] = index;
   }
   *doloop = loopnest;
   return stlist;

}

/*
Given a REDUCTION_LIST for an MP construct, return an estimate (in machine
cycles) of the time for combining all the partial (local) reduction results
into the final (shared) result.  Return TRUE in *using_critical if all the
combining operations should be lowered by wrapping them in a single
CRITICAL section, or FALSE if each combining operation should be lowered as
an individual ATOMIC operation.

For now, assume 4*900MHz Itanium2.  Eventually these values
should come from platform-dependent tables (maybe targ_info?).
since critical performs over atomic, we always use critical to combine final
reduction results. so the following function is obsoleted. csc.
*/

extern INT
MP_Reduction_Combine_Cycles(REDUCTION_LIST *rlist, BOOL *using_critical)
{
  // IA64, For extern usage. csc.
  *using_critical = TRUE;
  return 0;

  // original implementation for SGI's machine.
  Is_True(rlist->Elements() > 0, ("no reductions"));

  const double cycle_time = 1 / 250.0e6;
  const double crit_time = 20.6e-6;   // time for a CRITICAL section
  const double cas_time =   8.9e-6;   // time for a compare-and-swap
  const double fop_time =   8.4e-6;   // time for a fetch-and-op

  INT tot_cycles = 0, i;

  *using_critical = TRUE;

#ifdef TARG_IA32
  return tot_cycles;
#endif

  if (rlist->Elements() > 2) {
    return (INT)(crit_time / cycle_time);
  }

    // if any reduction operation is "&&" or "||", we must use CRITICAL
  for (i = 0; i < rlist->Elements(); i++) {
    OPERATOR oper = WN_operator(WN_kid0((*rlist)[i]));
    if (oper == OPR_CAND || oper == OPR_CIOR)
      return (INT)(crit_time / cycle_time);
  }

  for (i = 0; i < rlist->Elements(); i++) {
    switch (WN_ATOMIC_STORE_Lowering_Class((*rlist)[i])) {
    case ALCLASS_SWAP:
      tot_cycles += (INT)(cas_time / cycle_time);
      break;
    case ALCLASS_DIRECT:
      tot_cycles += (INT)(fop_time / cycle_time);
      break;
    case ALCLASS_CRITICAL:
      return (INT)(crit_time / cycle_time);
    default:
      Fail_FmtAssertion("bogus ATOMIC_Lowering_Class");
    }
  }

  *using_critical = FALSE;
  return tot_cycles;
}


/*
Generate code blocks for initialization and final combine-and-store for all
reductions in a parallel construct.  Should only be called if var_table
contains reduction variables.  Returns init/store code in
init_block/store_block, with the root always a WHIRL block.

We guard all operations for combining partial reduction results
into final results by a single critical section. In SGI's original 
implementation, combine operation for less than 2 results is done
by using ATOMIC primitives. This is the case for SGI's origin/challenge,
but for Itanium2, the cost of atomic is larger than critical section,
so we always use critical section instead of atomic.
csc.
*/
static void
Gen_MP_Reduction(VAR_TABLE *var_table, INT num_vars, WN **init_block,
                 WN **store_block)
{
  INT i, num_redns = 0;
  VAR_TABLE *v;
  WN *sizes[MAX_NDIM];
	WN *sizes_bak[MAX_NDIM];
	PREG_NUM new_indices[MAX_NDIM];
	INT ndim_array;
  WN *loopnest;
  WN *stlist;
  WN *temp;


  *init_block = WN_CreateBlock ( );
  *store_block = WN_CreateBlock ( );

    // init. of partial reduction result
  for (i = 0, v = var_table; i < num_vars; i++, v++)
    if (v->vtype == VAR_REDUCTION_SCALAR ||
        v->vtype == VAR_REDUCTION_ARRAY  ||
	      v->vtype == VAR_REDUCTION_ARRAY_OMP ) 
	  {

      num_redns++;

      if(v->vtype == VAR_REDUCTION_ARRAY_OMP)
      {
	    //generate array reduction initialization code
	    //The code are partly ported from be/vho/f90_lower.cxx

	    // Create loop nest.
        TY_IDX array_ty= (TY_kind(v->ty) == KIND_POINTER ? 
                TY_pointed(v->ty) : v->ty);
        TY_IDX pointer_ty = (TY_kind(v->ty) == KIND_POINTER ?
            v->ty: Make_Pointer_Type(v->ty, false));

        ARB_HANDLE arb_base = TY_arb(array_ty);
	    ndim_array = ARB_dimension( arb_base );
	    Is_True(( ndim_array <= MAX_NDIM ) && ( ndim_array >= 0 ),
	          ("dimension of array is not 0-7"));
				
	    INT64 temp_size = 0;
	    for(int j=0; j<ndim_array; j++)
	    {
                ARB_HANDLE arb = arb_base[ ndim_array-1-j ];
                WN* lb = NULL;
                WN* ub = NULL;
                WN* size = NULL;
                TYPE_ID mtype = MTYPE_I8;
                TY_IDX ty_idx = 0;
                
                if (ARB_const_lbnd(arb))
                   lb = WN_Intconst(mtype, ARB_lbnd_val(arb));
                else
                {
                   ST_IDX st_idx = ARB_lbnd_var(arb);
                   ty_idx = ST_type(st_idx);
                   lb = WN_Ldid(TY_mtype(ty_idx), 0, st_idx, ty_idx);
                   lb = WN_Type_Conversion(lb, mtype);
                }
                
                if (ARB_const_ubnd(arb))
                   ub = WN_Intconst(mtype, ARB_ubnd_val(arb));
                else
                {
                   ST_IDX st_idx = ARB_ubnd_var(arb);
                   ty_idx = ST_type(st_idx);
                   ub = WN_Ldid(TY_mtype(ty_idx), 0, st_idx, ty_idx);
                   ub = WN_Type_Conversion(ub, mtype);
                }
                
                size = WN_Add(mtype, 
                WN_Sub(mtype, ub, lb),
                WN_Intconst(mtype, 1));
                
                sizes[ndim_array-j-1] = size;
                sizes_bak[j] = WN_COPY_Tree(size);
            }
                
            for( int j=ndim_array; j<MAX_NDIM; j++ )
            {
                sizes[j] = NULL;
                sizes_bak[j] = NULL;
            }
							
	    stlist = create_doloop_nest( new_indices,&loopnest,
	                           sizes,ndim_array );
 	    WN_linenum( loopnest ) = line_number;

	    // initialize Array element.
	    // Determine the proper value to store into array.
	    WN *wn_init_val = Make_Reduction_Identity( v->reduction_opr, v->mtype );

	    // store the value.
        OPCODE op_array = OPCODE_make_op( OPR_ARRAY, Pointer_type, MTYPE_V );
	    WN *local_array = WN_Create( op_array, 1+2*ndim_array );

	    WN_element_size( local_array ) = TY_size( TY_AR_etype(array_ty));
        if (TY_kind(v->ty) == KIND_POINTER)
	        WN_array_base( local_array ) = WN_Ldid(Pointer_type, 0, v->new_st, v->ty);
        else
            WN_array_base( local_array ) = WN_Lda(Pointer_type, 0, v->new_st);

	    for( int j=0; j<ndim_array; j++ )
	    {
	    //assume the index type to be I8. also assume the dim-size I8 type. 
	    //TODO: make it more adaptive. csc
		     WN_array_index( local_array, j ) = WN_LdidPreg( MTYPE_I8, 
		                                            new_indices[j] );
		     WN_array_dim( local_array, j ) = sizes_bak[ndim_array-j-1];
	    }
	    // Maybe need to set parent point of the array kids.

	    WN *wn_store_val = WN_Istore( v->mtype, 0, 
			    pointer_ty, local_array, wn_init_val ); 

	    WN_INSERT_BlockLast( stlist, wn_store_val );
	    WN_INSERT_BlockLast( *init_block, loopnest );

      }
      else
      {
	    	WN_INSERT_BlockLast(*init_block,
            Gen_MP_Store(v->new_st, v->new_offset,
            Make_Reduction_Identity(v->reduction_opr, v->mtype), TRUE));
      }
  }

  Is_True(num_redns > 0, ("no reductions!"));

  // count number of reductions we can do atomically
  BOOL lower_using_critical = FALSE;
  for (i = 0, v = var_table; i < num_vars; i++, v++)
  {
    if (v->vtype == VAR_REDUCTION_SCALAR ||
        v->vtype == VAR_REDUCTION_ARRAY) {

      // compute lvalue for final result
      WN *orig_val = (v->vtype == VAR_REDUCTION_SCALAR) ?
                    Gen_MP_Load(v->orig_st, v->orig_offset, TRUE) :
                    WN_Iload(v->mtype, 0, v->ty, WN_COPY_Tree(v->vtree));
      WN *new_val = Gen_MP_Load(v->new_st, v->new_offset, TRUE);
      if ( (v->reduction_opr == OPR_CAND || v->reduction_opr == OPR_CIOR) &&
           (v->mtype == MTYPE_F4 || v->mtype == MTYPE_F8 ||
	          v->mtype == MTYPE_F10 || v->mtype == MTYPE_FQ) ) {
          // convert float to integer for C operators && and || : PV 677602
        orig_val = WN_NE(v->mtype, orig_val, 
                         Make_Const(Host_To_Targ_Float(v->mtype, 0.0)));
        new_val = WN_NE(v->mtype, new_val,
                        Make_Const(Host_To_Targ_Float(v->mtype, 0.0)));
      }
      WN *result = WN_CreateExp2(Make_Final_Reduction_Op(v->reduction_opc),
                                 orig_val, new_val);


      // add CVT if needed
      TYPE_ID restype = OPCODE_rtype(v->reduction_opc);
      switch (v->mtype) { 
      case MTYPE_I1: 
        result = WN_CreateCvtl(OPC_I4CVTL, 8, result);
        restype = MTYPE_I4; 
        break;
      case MTYPE_I2: 
        result = WN_CreateCvtl(OPC_I4CVTL, 16, result);
        restype = MTYPE_I4; 
	      break;
      case MTYPE_U1: 
	      result = WN_CreateCvtl(OPC_U4CVTL, 8, result);
        restype = MTYPE_U4; 
	      break;
      case MTYPE_U2: 
	      result = WN_CreateCvtl(OPC_U4CVTL, 16, result);
        restype = MTYPE_U4; 
	      break;
      } 
      if ((( restype == MTYPE_I4 || restype == MTYPE_I8 ||
              restype == MTYPE_U4 || restype == MTYPE_U8 ) &&
             ( v->mtype == MTYPE_F4 || v->mtype == MTYPE_F8 ||
	             v->mtype == MTYPE_F10 || v->mtype == MTYPE_FQ)) ||
           (( v->mtype == MTYPE_I4 || v->mtype == MTYPE_I8 ||
              v->mtype == MTYPE_U4 || v->mtype == MTYPE_U8 ) &&
             ( restype == MTYPE_F4 || restype == MTYPE_F8 ||
	             restype == MTYPE_F10 || restype == MTYPE_FQ )))
        result = WN_Cvt(restype, v->mtype, result);
      else if ((restype == MTYPE_I4 || restype == MTYPE_I8 ||
              restype == MTYPE_U4 || restype == MTYPE_U8) && 
	            (v->mtype == MTYPE_I4 || v->mtype == MTYPE_I8 ||
              v->mtype == MTYPE_U4 || v->mtype == MTYPE_U8) && 
	            v->mtype != restype)
	      result = WN_Cvt(restype, v->mtype, result);
	   

        // create the store that follows the ATOMIC
      if (v->vtype == VAR_REDUCTION_SCALAR) { 
        WN_INSERT_BlockLast(*store_block,
            Gen_MP_Store(v->orig_st, v->orig_offset, result, TRUE));
	// Flush the variable to all threads
	// It seems this doesn't needed since there is a barrier
	// afterward. csc.
//	WN_INSERT_BlockLast(*store_block,
//	    Gen_Flush(v->orig_st, v->orig_offset));
      } else { 
        WN_INSERT_BlockLast(*store_block,
                            WN_Istore(v->mtype, 0,
                            Make_Pointer_Type(v->ty, FALSE), 
			    WN_COPY_Tree(v->vtree), result ));
      } 

    }else if(v->vtype == VAR_REDUCTION_ARRAY_OMP) {

      // Array reduction should always using critical section.
      lower_using_critical = TRUE;
      // stuff code for later use, will be clean up later. csc.
      // TODO:Insert combine code here.
      TY_IDX array_ty= (TY_kind(v->ty) == KIND_POINTER ?
                      TY_pointed(v->ty) : v->ty);
      TY_IDX pointer_ty = (TY_kind(v->ty) == KIND_POINTER ?
                  v->ty: Make_Pointer_Type(v->ty, false));
      ARB_HANDLE arb_base = TY_arb( array_ty);
      ndim_array = ARB_dimension( arb_base );
      Is_True(( ndim_array <= MAX_NDIM ) && ( ndim_array >= 0 ),
              ("dimension of array is not 0-7"));

      INT64 temp_size = 0;
      for(int j=0; j<ndim_array; j++)
      {
          ARB_HANDLE arb = arb_base[ ndim_array-1-j ];
          WN* lb = NULL;
          WN* ub = NULL;
          WN* size = NULL;
          TYPE_ID mtype = MTYPE_I8;
          TY_IDX ty_idx = 0;

          if (ARB_const_lbnd(arb))
             lb = WN_Intconst(mtype, ARB_lbnd_val(arb));
          else
          {
             ST_IDX st_idx = ARB_lbnd_var(arb);
             ty_idx = ST_type(st_idx);
             lb = WN_Ldid(TY_mtype(ty_idx), 0, st_idx, ty_idx);
             lb = WN_Type_Conversion(lb, mtype);
          }

          if (ARB_const_ubnd(arb))
             ub = WN_Intconst(mtype, ARB_ubnd_val(arb));
          else
          {
             ST_IDX st_idx = ARB_ubnd_var(arb);
             ty_idx = ST_type(st_idx);
             ub = WN_Ldid(mtype, 0, st_idx, ST_type(st_idx));
             ub = WN_Ldid(TY_mtype(ty_idx), 0, st_idx, ty_idx);
             ub = WN_Type_Conversion(ub, mtype);
          }

          size = WN_Add(mtype, 
                       WN_Sub(mtype, ub, lb),
                       WN_Intconst(mtype, 1));

          sizes[ndim_array-j-1] = size;
          sizes_bak[j] = WN_COPY_Tree(size);
      }

      for( int j=ndim_array; j<MAX_NDIM; j++ )
      {
          sizes[j] = NULL;
	        sizes_bak[j] = NULL;
      }

      stlist = create_doloop_nest( new_indices,&loopnest,
                                   sizes,ndim_array );
      WN_linenum( loopnest ) = line_number;

      WN *wn_init_val = Make_Reduction_Identity( v->reduction_opr, v->mtype );
      OPCODE op_array = OPCODE_make_op( OPR_ARRAY, Pointer_type, MTYPE_V );
      WN *new_array = WN_Create( op_array, 1+2*ndim_array );
      WN_element_size( new_array ) = TY_size( TY_AR_etype(array_ty));
      if (TY_kind(v->ty) == KIND_POINTER)
        WN_array_base( new_array ) = WN_Ldid(Pointer_type, 0, v->new_st, v->ty);
      else
        WN_array_base( new_array ) = WN_Lda(Pointer_type, 0, v->new_st);

      WN *old_array = WN_Create( op_array, 1+2*ndim_array );
      WN_element_size( old_array ) = TY_size( TY_AR_etype(array_ty));
      if (TY_kind(v->ty) == KIND_POINTER)
        WN_array_base( old_array ) = WN_Ldid(Pointer_type, 0, v->orig_st, v->ty);
      else
        WN_array_base(old_array ) = WN_Lda(Pointer_type, 0, v->orig_st);
					          
      for( int j=0; j<ndim_array; j++ )
      {
         //assume the index type to be I8. also assume the dim-size I8 type.
         //TODO: make it more adaptive. csc
	     WN_array_index( new_array, j ) = WN_LdidPreg( MTYPE_I8, new_indices[j] );
	     WN_array_dim( new_array, j ) = WN_COPY_Tree( sizes_bak[ndim_array-j-1] );
	     WN_array_index( old_array, j ) = WN_LdidPreg( MTYPE_I8, new_indices[j] );
	     WN_array_dim( old_array, j ) =  sizes_bak[ndim_array-j-1];
      }
			   // Maybe need to set parent point of the array kids.
      WN *wn_old_val = WN_Iload( v->mtype, 0, pointer_ty,
	                         WN_COPY_Tree( old_array ));
      WN *wn_new_val = WN_Iload( v->mtype, 0, pointer_ty,
	                         new_array );

      if(( v->reduction_opr == OPR_CAND || v->reduction_opr == OPR_CIOR) &&
	       ( v->mtype == MTYPE_F4 || v->mtype == MTYPE_F8 ||
	         v->mtype == MTYPE_F10 || v->mtype == MTYPE_FQ ))
      {
	       wn_old_val = WN_NE( v->mtype, wn_old_val,
	                       Make_Const( Host_To_Targ_Float( v->mtype, 0.0 )));
	       wn_new_val = WN_NE( v->mtype, wn_new_val,
	                       Make_Const( Host_To_Targ_Float( v->mtype, 0.0 )));
      }
      WN *result = WN_CreateExp2( Make_Final_Reduction_Op( v->reduction_opc ),
	                          wn_old_val, wn_new_val );

      TYPE_ID restype = OPCODE_rtype(v->reduction_opc);
      switch (v->mtype) {
      case MTYPE_I1:
		     result = WN_CreateCvtl(OPC_I4CVTL, 8, result);
		     restype = MTYPE_I4;
		     break;
      case MTYPE_I2:
		     result = WN_CreateCvtl(OPC_I4CVTL, 16, result);
		     restype = MTYPE_I4;
		     break;
      case MTYPE_U1:
		     result = WN_CreateCvtl(OPC_U4CVTL, 8, result);
		     restype = MTYPE_U4;
		     break;
      case MTYPE_U2:
		     result = WN_CreateCvtl(OPC_U4CVTL, 16, result);
		     restype = MTYPE_U4;
		     break;
      }
      if((( restype == MTYPE_I4 || restype == MTYPE_I8 ||
	          restype == MTYPE_U4 || restype == MTYPE_U8 ) &&
	        ( v->mtype == MTYPE_F4 || v->mtype == MTYPE_F8 ||
	          v->mtype == MTYPE_F10 || v->mtype == MTYPE_FQ )) ||
	       (( v->mtype == MTYPE_I4 || v->mtype == MTYPE_I8 ||
	          v->mtype == MTYPE_U4 || v->mtype == MTYPE_U8 ) &&
	        ( restype == MTYPE_F4 || restype == MTYPE_F8 ||
	          restype == MTYPE_F10 || restype == MTYPE_FQ )))
	      result = WN_Cvt( restype, v->mtype, result );
      else if(( restype == MTYPE_I4 || restype == MTYPE_I8 ||
		            restype == MTYPE_U4 || restype == MTYPE_U8) &&
	            ( v->mtype == MTYPE_I4 || v->mtype == MTYPE_I8 ||
	              v->mtype == MTYPE_U4 || v->mtype == MTYPE_U8 ) &&
	              v->mtype != restype )
	      result = WN_Cvt( restype, v->mtype, result );

      WN *wn_store_val = WN_Istore( v->mtype, 0, pointer_ty,
                                    old_array, result );
      WN_INSERT_BlockLast( stlist, wn_store_val );
      WN_INSERT_BlockLast( *store_block, loopnest );
      // Is a flush needed here?
    }
  }

  WN *unlock = NULL;

  if (!unlock) 
  {
      // insert critical section at start of *store_block
      ST *mplock = Create_Critical_Lock( );
      WN_INSERT_BlockFirst( *store_block, 
	                  Gen_Critical( local_gtid, mplock ));
      unlock = Gen_End_Critical ( local_gtid, mplock );
      WN_INSERT_BlockLast( *store_block, unlock );
      // Does this barrier necessary?
      WN_INSERT_BlockLast( *store_block, Gen_Barrier(local_gtid));
  }
}

static WN* 
WN_Integer_Cast(WN* tree, TYPE_ID to, TYPE_ID from)
{
  if (from != to)
    return WN_CreateExp1(OPCODE_make_op(OPR_CVT, to, from), tree);
  else
    return tree;
}

/* Return TRUE if tree contains a call to the alloca intrinsic anywhere,
   otherwise return FALSE. */

static BOOL 
Calls_Alloca(WN *tree)
{
  WN *kid;
  INT kidno;
  OPCODE opc;

  Is_True(tree, ("NULL tree passed to Calls_Alloca()"));

  opc = WN_opcode(tree);
  if ((opc == OPC_U4INTRINSIC_CALL &&
       WN_intrinsic(tree) == INTRN_U4I4ALLOCA) ||
      (opc == OPC_U8INTRINSIC_CALL &&
       WN_intrinsic(tree) == INTRN_U8I8ALLOCA)) {
    Is_True(!Alloca_Dealloca_On,
            ("Alloca_Dealloca_On yet found INTRN_ALLOCA"));
    return TRUE;
  }

  if (WN_operator(tree) == OPR_ALLOCA) {
    Is_True(Alloca_Dealloca_On,
            ("found OPR_ALLOCA yet not Alloca_Dealloca_On"));
    return TRUE;
  }

  if (!OPCODE_is_leaf(opc))
    if (opc == OPC_BLOCK) {
      for (kid = WN_first(tree); kid; kid = WN_next(kid))
        if (Calls_Alloca(kid))
          return TRUE;

    } else {
      for (kidno = 0; kidno < WN_kid_count(tree); kidno++)
        if (Calls_Alloca(WN_kid(tree, kidno)))
          return TRUE;
    }

  return FALSE;
}

/*
Hack for determining if an ST has an associated F90 dope vector (this is
true for F90 pointers, allocatable arrays, and arrays that may be
non-contiguous).
*/

/*
from dphillim:
so  if it has a dope TY and
    the dope_TY points to a KIND_ARRAY,
    and its not an f90 pointer 
    and not an argument (SCALAR_FORMAL_REF), 
    Then it's an allocatable because there's nothing left.
*/

static const char * const dope_str_prefix = ".dope." ;
static const INT dope_str_prefix_len = 6;

static BOOL
ST_Has_Dope_Vector(ST *st)
{
  if (ST_class(st) != CLASS_VAR)
    return FALSE;

  if ( TY_is_f90_pointer(Ty_Table[ST_type(*st)]) )
    return TRUE;

  TY_IDX ty = ST_type(st);
  while (TY_kind(ty) == KIND_POINTER)
    ty = TY_pointed(ty);

  if (TY_kind(ty) == KIND_STRUCT &&
      strncmp(TY_name(ty), dope_str_prefix, dope_str_prefix_len) == 0)
    return TRUE;

  return FALSE;
}


/* Return a block containing code for saving the stack pointer. 
   st_basename is the basename for a temporary that is generated for
   storing the SP.  The output parameter sp_save_stid is for use with
   Gen_Restore_Stack_Pointer(). */

static WN *
Gen_Save_Stack_Pointer(const char *st_basename, WN **sp_save_stid)
{
  char *newstname;
  static INT count = 0;
  WN *save_block = WN_CreateBlock(), *preg_stid_kid;

    // Create uniquely-named preg for saving SP
  newstname = (char *) alloca(strlen(st_basename) + 32);
  sprintf(newstname, "$%s%d__$stkptr", st_basename, count++);

  PREG_NUM sp_preg = Create_Preg(Pointer_type, (char *) newstname);

  if (Alloca_Dealloca_On) {
      // use ALLOCA operator with size 0
    preg_stid_kid = WN_CreateAlloca(WN_CreateIntconst(OPC_I4INTCONST, 0));

  } else {
      // Call intrinsic for reading SP
    WN *intrin_call;
    intrin_call = WN_Create(OPCODE_make_op(OPR_INTRINSIC_CALL, 
                                           Pointer_type, MTYPE_V), 0);
    WN_Set_Call_Non_Parm_Ref(intrin_call);
    WN_intrinsic(intrin_call) =
        (Pointer_Size == 8) ? INTRN_U8READSTACKPOINTER :
                              INTRN_U4READSTACKPOINTER;
    WN_linenum(intrin_call) = line_number;
    WN_INSERT_BlockLast(save_block, intrin_call);

      // Load returned SP value
    PREG_NUM rreg1, rreg2;

    GET_RETURN_PREGS(rreg1, rreg2, Pointer_type);
    preg_stid_kid = WN_LdidPreg(Pointer_type, rreg1);
  }

  *sp_save_stid = WN_StidIntoPreg(Pointer_type, sp_preg,
    MTYPE_To_PREG(Pointer_type), preg_stid_kid);
  WN_linenum(*sp_save_stid) = line_number;

  WN_INSERT_BlockLast(save_block, *sp_save_stid);
  return save_block;
}


// Given a list of alloca's for auto variables, this class builds a
// read-only list of the ST's that get allocated.

class Alloca_Var_List {
public:
  struct Info {
    ST *st;
    WN_OFFSET offset;
    TY_IDX ty;
    Info(ST *_st, WN_OFFSET _ofs, TY_IDX _ty) : st(_st), offset(_ofs), ty(_ty)
      { }
  };
private:
  DYN_ARRAY<Info> _list;
public:
  Alloca_Var_List(WN *alloca_block);
  const DYN_ARRAY<Info> *list() const { return &_list; }
};

Alloca_Var_List::Alloca_Var_List(WN *alloca_block) :
  _list(&mp_lower_pool)
{
  Is_True(WN_operator(alloca_block) == OPR_BLOCK, ("not a BLOCK"));

  if (!Alloca_Dealloca_On)
    return;

  for (WN *kid = WN_first(alloca_block); kid; kid = WN_next(kid)) {
    OPERATOR opr = WN_operator(kid);

    switch (opr) {
    case OPR_INTRINSIC_CALL:
      Is_True(WN_intrinsic(kid) == INTRN_U4I4ALLOCA ||
              WN_intrinsic(kid) == INTRN_U8I8ALLOCA, ("expected alloca()"));
      Is_True(!Alloca_Dealloca_On,
              ("Alloca_Dealloca_On yet found INTRN_ALLOCA"));
      break;
    case OPR_STID:
      _list.AddElement(Info(WN_st(kid), WN_offset(kid), WN_ty(kid)));
      break;
    default:
      Fail_FmtAssertion("unexpected opr == %d\n", (INT) opr);
    }
  }
}


// Return a Whirl block that restores the stack pointer. sp_save_stid is
// the output param. from Gen_Save_Stack_Pointer(), avlist is computed
// from all the alloca's following the stack pointer save.
static WN *
Gen_Restore_Stack_Pointer(WN *sp_save_stid, Alloca_Var_List *avlist)
{
  WN *restore_block = WN_CreateBlock();
  WN *dealloca;
  WN *ldid_old_sp = WN_CreateLdid(
    OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type),
    WN_offset(sp_save_stid), WN_st(sp_save_stid), WN_ty(sp_save_stid));

  if (Alloca_Dealloca_On) {
      // use DEALLOCA operator
    INT num_allocas = avlist->list()->Elements();
    dealloca = WN_CreateDealloca(num_allocas + 1);
    WN_kid0(dealloca) = ldid_old_sp;
      // kids 1 to N are auto vars whose lifetime ends at DEALLOCA
    for (INT i = 0; i < num_allocas; i++) {
      const Alloca_Var_List::Info *info = &(*avlist->list())[i];
      WN_kid(dealloca, i + 1) = WN_CreateLdid(
        OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type),
        info->offset, info->st, info->ty);
    }

  } else {
      // call SETSTACKPOINTER intrinsic
    dealloca = WN_Create(
      OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V), 1);
    WN_Set_Call_Non_Parm_Mod(dealloca);
    WN_Set_Call_Non_Parm_Ref(dealloca);
    WN_Set_Call_Parm_Ref(dealloca);

    WN_intrinsic(dealloca) = (Pointer_Size == 8) ?
        INTRN_U8I8SETSTACKPOINTER : INTRN_U4I4SETSTACKPOINTER;
    WN_kid0(dealloca) = WN_CreateParm(Pointer_type, ldid_old_sp,
      Be_Type_Tbl(Pointer_type), WN_PARM_BY_VALUE );
    WN_linenum(dealloca) = line_number;
  }

  WN_INSERT_BlockLast(restore_block, dealloca);
  return restore_block;
}


/* Given a block of code appearing within an OMP SINGLE or PCF
   SINGLEPROCESS directive, and flags for whether the directive was OMP
   and had the NOWAIT clause, return a block of corresponding MP-lowered
   code.  Argument single_block becomes part of the output block, so it
   must not already be part of the Whirl tree. */

static WN *
Gen_MP_SingleProcess_Block(WN *single_block, BOOL nowait, 
#ifdef KEY
                           WN *copyprivates,
#endif
                           BOOL is_omp, BOOL is_orphaned)
{
  WN *mp_single_block = WN_CreateBlock(), *return_stid, *single_test;
  WN *sp_block = WN_CreateBlock();
  ST *return_st;
  WN_OFFSET return_ofst;
  PREG_NUM rreg1, rreg2;

  if( is_orphaned)
  {
    // insert code to get gtid.
    WN_INSERT_BlockLast( mp_single_block, Gen_Store_Gtid());
  }
  WN_INSERT_BlockLast(mp_single_block,
    Gen_Single( WN_CreateIntconst(OPC_I4INTCONST, num_constructs),
       local_gtid, is_omp));

  Create_Preg_or_Temp(MTYPE_I4, MPSP_STATUS_PREG_NAME, &return_st,
                      &return_ofst);
  GET_RETURN_PREGS(rreg1, rreg2, MTYPE_I4);
  return_stid = WN_Stid(MTYPE_I4, return_ofst, return_st, ST_type(return_st),
			WN_LdidPreg(MTYPE_I4, rreg1));
  WN_linenum(return_stid) = line_number;
  WN_INSERT_BlockLast(mp_single_block, return_stid);

  WN *test = WN_EQ(MTYPE_I4,
                   Gen_MP_Load( return_st, return_ofst ),
                   WN_CreateIntconst ( OPC_I4INTCONST, 1));
  single_test = WN_CreateIf( test, single_block, WN_CreateBlock());
  WN_linenum(single_test) = line_number;
  WN_INSERT_BlockLast(mp_single_block, single_test);
  if (is_omp || !nowait)
    WN_INSERT_BlockLast(mp_single_block,
                        Gen_End_Single( 
                          WN_CreateIntconst(OPC_I4INTCONST, num_constructs),
                          local_gtid, is_omp, nowait));
#ifdef KEY
  if (copyprivates)
    Gen_MP_Copyprivate(copyprivates, &mp_single_block, Gen_MP_Load( return_st, return_ofst ));
#endif

  return mp_single_block;
}

#ifdef KEY /* Bug 4828 */
static WN *
Gen_MP_Workshare_Block(WN *workshare_block, BOOL nowait, 
                           BOOL is_omp, BOOL is_orphaned)
{
  WN *mp_workshare_block = WN_CreateBlock(), *return_stid;
  WN *sp_block = WN_CreateBlock();
  ST *return_st;
  WN_OFFSET return_ofst;
  PREG_NUM rreg1, rreg2;

  if( is_orphaned)
  {
    // insert code to get gtid.
    WN_INSERT_BlockLast( mp_workshare_block, Gen_Store_Gtid());
  }
  WN_INSERT_BlockLast( mp_workshare_block, workshare_block);

  if (is_omp || !nowait)
    WN_INSERT_BlockLast(mp_workshare_block, Gen_Barrier( local_gtid ));

  return mp_workshare_block;
}

static WN *Gen_MP_Workshare_Region(WN *workshare_region)
{
  WN *prag = WN_first(WN_region_pragmas(workshare_region)), *workshare_prag = prag;
  WN *workshare_block = WN_region_body(workshare_region);
  VAR_TABLE *vt;
  WN_PRAGMA_ID prag_id;
  INT vt_size = 0, i;
  WN **nested_list, *wn, *nested_prag;
  WN *nested_local_nodes = NULL;
  BOOL nowait = FALSE;
  BOOL do_dealloca = FALSE;
  WN *mp_workshare_block = WN_CreateBlock();
  WN *sp_save_stid;
  Alloca_Var_List *avlist;
  BOOL is_orphaned = FALSE;

  Is_True(mpt == MPP_WORKSHARE || mpt == MPP_ORPHANED_WORKSHARE,
          ("not inside a WORKSHARE"));

  is_orphaned = ( mpt == MPP_WORKSHARE ) ? FALSE : TRUE;

  if (mpt == MPP_ORPHANED_WORKSHARE) {
      // set up some globals
    psymtab = CURRENT_SYMTAB;
    ppuinfo = Current_PU_Info;
    pmaptab = Current_Map_Tab;
  }

  prag = WN_next(prag);
  WN_next(workshare_prag) = NULL;  /* Remove all but WORKSHARE pragma */
  WN_last(WN_region_pragmas(workshare_region)) = workshare_prag;

  while (prag)
    switch (prag_id = (WN_PRAGMA_ID) WN_pragma(prag)) {
    case WN_PRAGMA_LOCAL:
      nested_prag = prag;
      prag = WN_next(prag);
      nested_list = ( prag_id == WN_PRAGMA_LOCAL &&
                      !ST_Has_Dope_Vector(WN_st(nested_prag)) ) ?
                        &nested_local_nodes :
                        &nested_firstprivate_nodes;
      for (wn = *nested_list; wn; wn = WN_next(wn))
        if (Identical_Pragmas(nested_prag, wn))
          break;
      if (!wn) {
        WN_next(nested_prag) = *nested_list;
        *nested_list = nested_prag;
        vt_size++;
      } else
        WN_Delete(nested_prag);
      break;

    case WN_PRAGMA_SHARED:
      nested_prag = prag;
      prag = WN_next(prag);
      WN_Delete(nested_prag);
      break;

    case WN_PRAGMA_NOWAIT:
      nowait = TRUE;
      nested_prag = prag;
      prag = WN_next(prag);
      WN_Delete(nested_prag);
      break;

    case WN_PRAGMA_END_MARKER:
      nested_prag = prag;
      prag = WN_next(prag);
      WN_Delete(nested_prag);
      break; 

    default:
      Fail_FmtAssertion("illegal pragma type %d in SINGLE directive",
                        WN_pragma(prag));
    }

  WN_region_body(workshare_region) = NULL; /* splice out body of SINGLE region */

  if (vt_size) {  /* must localize some variables */
    WN *nested_alloca_block = NULL;

    vt = (VAR_TABLE *) alloca((vt_size + 1) * sizeof(VAR_TABLE));
    BZERO(vt, (vt_size + 1) * sizeof(VAR_TABLE));
    Create_Local_Variables(vt, NULL, NULL, nested_local_nodes,
                           NULL, NULL,
                           NULL, &nested_alloca_block);

    Localize_Parent_Stack lps(mpt == MPP_ORPHANED_WORKSHARE, workshare_block);
    WN *nested_non_pod_finalization_nodes = NULL;
    Walk_and_Localize(workshare_block, vt, &lps, FALSE,
                      &nested_non_pod_finalization_nodes);

    if (nested_alloca_block) {
      if ((do_dealloca = !Calls_Alloca(workshare_block)) != 0) {
        WN_INSERT_BlockLast(mp_workshare_block,
            Gen_Save_Stack_Pointer("mpworkshare", &sp_save_stid));
        avlist = CXX_NEW(Alloca_Var_List(nested_alloca_block),
	                 &mp_lower_pool);
      }
      WN_INSERT_BlockLast(mp_workshare_block, nested_alloca_block);
      Set_PU_has_alloca(Get_Current_PU());
    }

    for (i = 0; i < vt_size; i++) {
      if (vt[i].vtree)
        WN_DELETE_Tree(vt[i].vtree);
      if (vt[i].vtreex)
        WN_DELETE_Tree(vt[i].vtreex);
    }
  } // if (vt_size)

  WN_INSERT_BlockLast(mp_workshare_block,
    Gen_MP_Workshare_Block(workshare_block, nowait,
      WN_pragma_omp(workshare_prag), is_orphaned ));

  if (do_dealloca) {
    WN_INSERT_BlockLast(mp_workshare_block,
        Gen_Restore_Stack_Pointer(sp_save_stid, avlist));
    CXX_DELETE(avlist, &mp_lower_pool);
  }

    /* Delete PRIVATE pragmas */
  while (nested_local_nodes) {
    wn = WN_next(nested_local_nodes);
    WN_Delete(nested_local_nodes);
    nested_local_nodes = wn;
  }
  return mp_workshare_block;
}
#endif
/* Given a region for an OMP SINGLE work-sharing construct, return a block
   of corresponding MP-lowered code.  The input single_region is not
   deleted (though some of its pragmas are, and its body is spliced out), so
   the caller must still remove it from the Whirl tree. */

static WN *Gen_MP_SingleProcess_Region(WN *single_region)
{
  WN *prag = WN_first(WN_region_pragmas(single_region)), *single_prag = prag;
  WN *single_block = WN_region_body(single_region);
  VAR_TABLE *vt;
  WN_PRAGMA_ID prag_id;
  INT vt_size = 0, i;
  WN **nested_list, *wn, *nested_prag;
  WN *nested_local_nodes = NULL, *nested_firstprivate_nodes = NULL;
#ifdef KEY
  WN  *nested_copyprivate_nodes = NULL;
#endif
  BOOL nowait = FALSE;
  BOOL do_dealloca = FALSE;
  WN *mp_single_block = WN_CreateBlock();
  WN *sp_save_stid;
  Alloca_Var_List *avlist;
  BOOL is_orphaned = FALSE;

  Is_True(mpt == MPP_SINGLE || mpt == MPP_ORPHANED_SINGLE,
          ("not inside a SINGLE"));

  is_orphaned = ( mpt == MPP_SINGLE ) ? FALSE : TRUE;

  if (mpt == MPP_ORPHANED_SINGLE) {
      // set up some globals
    psymtab = CURRENT_SYMTAB;
    ppuinfo = Current_PU_Info;
    pmaptab = Current_Map_Tab;
  }

  prag = WN_next(prag);
  WN_next(single_prag) = NULL;  /* Remove all but SINGLE pragma */
  WN_last(WN_region_pragmas(single_region)) = single_prag;

  while (prag)
    switch (prag_id = (WN_PRAGMA_ID) WN_pragma(prag)) {
    case WN_PRAGMA_LOCAL:
    case WN_PRAGMA_FIRSTPRIVATE:
#ifdef KEY
    case WN_PRAGMA_COPYPRIVATE:
#endif
      nested_prag = prag;
      prag = WN_next(prag);
#ifdef KEY
      if (prag_id == WN_PRAGMA_COPYPRIVATE)
        nested_list = &nested_copyprivate_nodes;
      else
#endif
      nested_list = ( prag_id == WN_PRAGMA_LOCAL &&
                      !ST_Has_Dope_Vector(WN_st(nested_prag)) ) ?
                        &nested_local_nodes :
                        &nested_firstprivate_nodes;
      for (wn = *nested_list; wn; wn = WN_next(wn))
        if (Identical_Pragmas(nested_prag, wn))
          break;
      if (!wn) {
        WN_next(nested_prag) = *nested_list;
        *nested_list = nested_prag;
        vt_size++;
      } else
        WN_Delete(nested_prag);
      break;

      /* we can get a SHARED(preg) clause in a SINGLE region due to
         variable renaming in LNO; must ignore these pragmas */
    case WN_PRAGMA_SHARED:
      nested_prag = prag;
      prag = WN_next(prag);
      WN_Delete(nested_prag);
      break;

    case WN_PRAGMA_NOWAIT:
      nowait = TRUE;
      nested_prag = prag;
      prag = WN_next(prag);
      WN_Delete(nested_prag);
      break;

    case WN_PRAGMA_END_MARKER:
      nested_prag = prag;
      prag = WN_next(prag);
      WN_Delete(nested_prag);
      break; 

    default:
      Fail_FmtAssertion("illegal pragma type %d in SINGLE directive",
                        WN_pragma(prag));
    }

  WN_region_body(single_region) = NULL; /* splice out body of SINGLE region */

  if (vt_size) {  /* must localize some variables */
    WN *nested_alloca_block = NULL, *firstprivate_block = NULL;

    vt = (VAR_TABLE *) alloca((vt_size + 1) * sizeof(VAR_TABLE));
    BZERO(vt, (vt_size + 1) * sizeof(VAR_TABLE));
    Create_Local_Variables(vt, NULL, NULL, nested_local_nodes,
                           nested_firstprivate_nodes, &firstprivate_block,
                           NULL, &nested_alloca_block);

    Localize_Parent_Stack lps(mpt == MPP_ORPHANED_SINGLE, single_block);
    WN *nested_non_pod_finalization_nodes = NULL;
    Walk_and_Localize(single_block, vt, &lps, FALSE,
                      &nested_non_pod_finalization_nodes);

      /* If we need to dynamically allocate space for any localized
         variables, we also try to deallocate this space at the end of
         the SINGLE region.  We want to deallocate so that if the SINGLE
         is inside a loop, the stack won't become huge.  However, we
         don't deallocate if there's a call to the alloca() intrinsic
         inside the lexical extent of the SINGLE directive, because that
         would also deallocate the space the user requested. */
      /* Also, for now we always allocate any needed space for localized
         variables.  This isn't necessary (and is inefficient) when an
         orphaned SINGLE region is executed serially.  However, it's
         more trouble to clone the region into serial and parallel cases,
         and we think the serial cases will be rare enough that we'll
         defer this optimization for now.--DRK */

    if (nested_alloca_block) {
      if ((do_dealloca = !Calls_Alloca(single_block)) != 0) {
        WN_INSERT_BlockLast(mp_single_block,
            Gen_Save_Stack_Pointer("mpsingle", &sp_save_stid));
        avlist = CXX_NEW(Alloca_Var_List(nested_alloca_block),
	                 &mp_lower_pool);
      }
      WN_INSERT_BlockLast(mp_single_block, nested_alloca_block);
      Set_PU_has_alloca(Get_Current_PU());
    }

    if (firstprivate_block)
      WN_INSERT_BlockLast(mp_single_block, firstprivate_block);

    for (i = 0; i < vt_size; i++) {
      if (vt[i].vtree)
        WN_DELETE_Tree(vt[i].vtree);
      if (vt[i].vtreex)
        WN_DELETE_Tree(vt[i].vtreex);
    }
  } // if (vt_size)

#ifdef KEY
  WN_INSERT_BlockLast(mp_single_block,
    Gen_MP_SingleProcess_Block(single_block, nowait, 
      nested_copyprivate_nodes,
      WN_pragma_omp(single_prag), is_orphaned ));
#else
  WN_INSERT_BlockLast(mp_single_block,
    Gen_MP_SingleProcess_Block(single_block, nowait,
      WN_pragma_omp(single_prag), is_orphaned ));
#endif

  if (do_dealloca) {
    WN_INSERT_BlockLast(mp_single_block,
        Gen_Restore_Stack_Pointer(sp_save_stid, avlist));
    CXX_DELETE(avlist, &mp_lower_pool);
  }

    /* Delete PRIVATE/FIRSTPRIVATE pragmas */
  while (nested_local_nodes) {
    wn = WN_next(nested_local_nodes);
    WN_Delete(nested_local_nodes);
    nested_local_nodes = wn;
  }
  while (nested_firstprivate_nodes) {
    wn = WN_next(nested_firstprivate_nodes);
    WN_Delete(nested_firstprivate_nodes);
    nested_firstprivate_nodes = wn;
  }
#ifdef KEY
  while (nested_copyprivate_nodes) {
    wn = WN_next(nested_copyprivate_nodes);
    WN_Delete(nested_copyprivate_nodes);
    nested_copyprivate_nodes = wn;
  }
#endif
  return mp_single_block;
} // Gen_MP_SingleProcess_Region()

/* Given a region for an OMP MASTER construct, return a block
   of corresponding MP-lowered code.  The input region is not
   deleted (though some of its pragmas are, and its body is spliced out), so
   the caller must still remove it from the Whirl tree. 
   by csc.
*/
static WN *Lower_Master( WN *master_region )
{
  if (WN_opcode(master_region) != OPC_REGION) {
        return master_region;
  }

  Is_True(mpt == MPP_MASTER || mpt == MPP_ORPHANED_MASTER,
          ("not inside a MASTER"));

  WN *pragmas = WN_region_pragmas( master_region );
  INT pragma_count = 0;
  WN *wn = NULL;
  for ( wn = WN_first(pragmas); wn != NULL; wn = WN_next(wn))
     if (WN_opcode(wn) != OPC_PRAGMA
         || WN_pragma(wn) != WN_PRAGMA_END_MARKER)
            pragma_count++;
  Is_True( pragma_count == 1, ("OMP MASTER directive can not have subclauses. "));

  WN *master_block = WN_region_body(master_region);
  WN *mp_master_block = WN_CreateBlock();
  BOOL is_orphaned = FALSE;

  is_orphaned = ( mpt == MPP_MASTER ) ? FALSE : TRUE;

  WN_region_body(master_region) = NULL; /* splice out body of MASTER region */

  if( is_orphaned == TRUE )
  {
    WN_INSERT_BlockLast( mp_master_block, Gen_Store_Gtid());
  }

  // master call.
  WN_INSERT_BlockLast( mp_master_block, Gen_Master(local_gtid));

  // Get returned value of Is_Master judgement.
  ST *return_st;
  WN_OFFSET return_ofst;
  PREG_NUM rreg1, rreg2;
  Create_Preg_or_Temp ( MTYPE_I4, IS_MASTER_PREG_NAME, &return_st, &return_ofst );
  GET_RETURN_PREGS(rreg1, rreg2, MTYPE_I4);
  wn = WN_Stid ( MTYPE_I4, return_ofst, return_st, ST_type(return_st),
                 WN_LdidPreg ( MTYPE_I4, rreg1 ));
  WN_linenum(wn) = line_number;
  WN_INSERT_BlockLast( mp_master_block, wn );

  // Create IF
  WN *test = WN_EQ(MTYPE_I4,
                   Gen_MP_Load( return_st, return_ofst ),
                   WN_CreateIntconst ( OPC_I4INTCONST, 1));
  WN *if_wn = WN_CreateIf(test,WN_CreateBlock(),WN_CreateBlock());
  WN_linenum( if_wn ) = line_number; 

  // If Is_Master, execute the master_block
  WN_INSERT_BlockLast( WN_then( if_wn ), master_block );
  WN_INSERT_BlockLast( mp_master_block, if_wn );

  // end_master call.
#ifndef KEY
  WN_INSERT_BlockLast( mp_master_block, Gen_End_Master(local_gtid));
#endif

  return mp_master_block;
} // Lower_Master()

// CAN BE DELETED
/* Generate an OMP begin_SPR call */
// Why this is an extern one?
// TODO: find the invocation location and replace it.
// csc.
// replaced with current RTL call.
extern WN *
Gen_OMP_Begin_SPR (MP_process_type mpt) 
{

  // maybe LNO needs more complex handling.
  WN *return_block = WN_CreateBlock();
  WN *store_gtid;
      
  store_gtid = Gen_Store_Gtid();
  WN_INSERT_BlockLast(return_block, store_gtid );
  WN_INSERT_BlockLast(return_block, Gen_Serialized_Parallel(local_gtid));
  
  return return_block;
  /*
  WN *wn;
  UINT64 flag;

  wn = WN_Create (OPC_VCALL, 1);
  WN_st_idx(wn) = GET_MPRUNTIME_ST (MPR_OMP_BEGIN_SPR);
  WN_Set_Call_Non_Data_Mod ( wn );
  WN_Set_Call_Non_Data_Ref ( wn );
  WN_Set_Call_Non_Parm_Mod ( wn );
  WN_Set_Call_Non_Parm_Ref ( wn );
  WN_linenum(wn) = line_number;

  if (mpt == MPP_PARALLEL_REGION) flag = 0;
  else flag = 0x1;

  WN_kid0(wn) = WN_CreateParm (MTYPE_U8,
                               WN_CreateIntconst (OPC_U8INTCONST, flag),
                               Be_Type_Tbl(MTYPE_U8),
                               WN_PARM_BY_VALUE);
  return wn;
  */
}

// CAN BE DELETED
/* Generate an OMP end_SPR call */
/* replace with new RTL */
extern WN *
Gen_OMP_End_SPR (MP_process_type mpt) 
{
  // maybe LNO needs more complex handling.
  WN *return_block = WN_CreateBlock();
  WN *store_gtid;
      
  store_gtid = Gen_Store_Gtid();
  WN_INSERT_BlockLast(return_block, store_gtid );
  WN_INSERT_BlockLast(return_block, 
		  Gen_End_Serialized_Parallel(local_gtid));
  
  return return_block;
  /*
  WN *wn;
  UINT64 flag;

  wn = WN_Create (OPC_VCALL, 1);
  WN_st_idx(wn) = GET_MPRUNTIME_ST (MPR_OMP_END_SPR);
  WN_Set_Call_Non_Data_Mod ( wn );
  WN_Set_Call_Non_Data_Ref ( wn );
  WN_Set_Call_Non_Parm_Mod ( wn );
  WN_Set_Call_Non_Parm_Ref ( wn );
  WN_linenum(wn) = line_number;

  if (mpt == MPP_PARALLEL_REGION) flag = 0;
  else flag = 0x1;

  WN_kid0(wn) = WN_CreateParm (MTYPE_U8,
                               WN_CreateIntconst (OPC_U8INTCONST, flag),
                               Be_Type_Tbl(MTYPE_U8),
                               WN_PARM_BY_VALUE);
  return wn;
  */
}

static struct namelock_kind {
  ST *name;
  ST *lock;
} *namelock_list = NULL;

static INT32 nl_idx = 0;
static INT32 nl_max = 0;

/***********************************************************************
 *
 * Given an ST for a named-lock, return the ST for the lock
 * allocated in the COMMON block, if any. NULL otherwise.
 *
 ***********************************************************************/
static ST* 
NameLock_Find (ST *name) {
  INT32 i;
  for (i=0; i<nl_idx; i++) {
//    if (name == namelock_list[i].name) return (namelock_list[i].lock);
    // when frontend is fixed, change this back to previous line--DRK
    {
      FmtAssert (ST_class(name) == CLASS_CONST, ("non-CONST class"));
      FmtAssert (ST_class(namelock_list[i].name) == CLASS_CONST,
                 ("non-CONST class"));

      TCON &name_tcon = Tcon_Table[ST_tcon(name)];
      TCON &list_tcon = Tcon_Table[ST_tcon(namelock_list[i].name)];
      if (Targ_String_Length(name_tcon) == Targ_String_Length(list_tcon) &&
          strncmp(Targ_String_Address(name_tcon),
	          Targ_String_Address(list_tcon),
		  Targ_String_Length(name_tcon)) == 0)
        return (namelock_list[i].lock);
    }
  }
  return NULL;
}

/***********************************************************************
 *
 * Given the ST for a named lock and the corresponding COMMON lock variable,
 * add to the list. Barf if duplicate.
 *
 ***********************************************************************/
static void 
NameLock_Add (ST *name, ST *lock) {
  FmtAssert (NameLock_Find(name) == NULL,
             ("NameLock_Add called with duplicate entry"));

  if (nl_idx == nl_max) {
    /* grow the array. Add 20 elements. */
    namelock_list = (struct namelock_kind*)
      MEM_POOL_Realloc (Malloc_Mem_Pool,
                        namelock_list,
                        nl_max*sizeof(struct namelock_kind),
                        (nl_max+20)*sizeof(struct namelock_kind));
    nl_max += 20;
  }
  namelock_list[nl_idx].name = name;
  namelock_list[nl_idx].lock = lock;
  nl_idx++;
}

/***********************************************************************
 *
 * Given an ST for the "name" of a named critical section,
 * return the ST for the appropriate COMMON block variable.
 *
 ***********************************************************************/
static ST *
Get_NameLock_ST (ST *name) {
  ST *lock_st;

  if (name == NULL) return NULL;

  lock_st = NameLock_Find (name);
  if (lock_st != NULL)
  {
    critical_lock_not_init = FALSE; 
    return lock_st;
  }

  lock_st = Create_Name_Lock (name);
  NameLock_Add (name, lock_st);
  return lock_st;
}

/***********************************************************************
 * Given an ST, return TRUE if it's the COMMON block variable corresponding
 * to a named critical section, FALSE otherwise.
 ***********************************************************************/
static BOOL 
Is_NameLock_ST(ST *st)
{
  if (ST_name(st)[0] != '_')  /* fast-check */
    return FALSE;
  return (strncmp(ST_name(st), "__namelock_", 11) == 0);
}


/*
*  Tranlate ordered directives( maybe others also should be
*  put in this function). Only Ordered/End Ordered inside
*  Parallel DO, none-orphaned PDO need to be translated here.
*  Called after the Creation of new MicroTask.
*  csc.
*/
static void 
Delayed_MP_Translation( WN * tree )
{
    INT32 kidno;
    WN *kid;
    WN *next_kid;

    ST *lock_st;
    WN *new_kid;
    ST *lock_stack[128];
    INT32 lptr = 0;
    INT32 pcf_nest = 0;

  if (tree == NULL)
     return;

    if (WN_opcode(tree) == OPC_BLOCK) {
       for (kid = WN_first(tree); kid; kid = next_kid) {
         next_kid = WN_next(kid);
         if (((WN_opcode(kid) == OPC_PRAGMA) ||
             (WN_opcode(kid) == OPC_XPRAGMA)) &&
             (WN_pragmas[WN_pragma(kid)].users & PUSER_MP)) {
        if (WN_pragma(kid) == WN_PRAGMA_CRITICAL_SECTION_BEGIN) {
          /* always translate critical section begin nodes */
    // Keep it anyway.
    // Delay its transform after Microtask creation.

          ++pcf_nest;
          if (WN_opcode(kid) == OPC_PRAGMA &&
              WN_pragma_omp(kid) &&
              WN_st(kid)) {
            lock_st = Get_NameLock_ST(WN_st(kid));
          }
          else if ((WN_opcode(kid) == OPC_XPRAGMA) &&
                   (WN_operator(WN_kid0(kid)) == OPR_LDA))
            lock_st = WN_st(WN_kid0(kid));
          else if ((WN_opcode(kid) == OPC_PRAGMA) && WN_st(kid))
            lock_st = WN_st(kid);
          else{
            lock_st = NULL;
            Create_Unnamed_Critical_Lock( );
          }
          lock_stack[lptr++] = lock_st;
          if(lock_st){
            Linenum_Pusher p(WN_Get_Linenum(kid));
            new_kid = Gen_Critical( local_gtid, lock_st );
          } else {
            Linenum_Pusher p(WN_Get_Linenum(kid));
            new_kid = Gen_Critical( local_gtid, unnamed_lock_st );
          }
          if (WN_prev(kid))
            WN_next(WN_prev(kid)) = new_kid;
          else
            WN_first(tree) = new_kid;
          WN_prev(new_kid) = WN_prev(kid);
          WN_next(new_kid) = WN_next(kid);
          if (WN_next(kid))
            WN_prev(WN_next(kid)) = new_kid;
          else
            WN_last(tree) = new_kid;
          WN_DELETE_Tree ( kid );
        } else if (WN_pragma(kid) == WN_PRAGMA_CRITICAL_SECTION_END) {
          /* always translate critical section end nodes */
          --pcf_nest;
          lock_st = lock_stack[--lptr];
          if (WN_pragma_omp(kid) && WN_st(kid)) {
            FmtAssert (lock_st == Get_NameLock_ST(WN_st(kid)),
                       ("Mismatch in lock on critical section"));
          }
          if (lock_st) {
            Linenum_Pusher p(WN_Get_Linenum(kid));
            new_kid = Gen_End_Critical (local_gtid, lock_st);
          } else {
            Linenum_Pusher p(WN_Get_Linenum(kid));
            new_kid = Gen_End_Critical (local_gtid, unnamed_lock_st);
          }
          if (WN_prev(kid))
            WN_next(WN_prev(kid)) = new_kid;
          else
            WN_first(tree) = new_kid;
          WN_prev(new_kid) = WN_prev(kid);
          WN_next(new_kid) = WN_next(kid);
          if (WN_next(kid))
            WN_prev(WN_next(kid)) = new_kid;
          else
            WN_last(tree) = new_kid;
          WN_Delete ( kid );
        }else if (WN_pragma(kid) == WN_PRAGMA_ORDERED_BEGIN ||
                     WN_pragma(kid) == WN_PRAGMA_ORDERED_END) {
                   // delay the lowering of ordered after the scope switch.
                     WN *call;
                     call = ( WN_pragma( kid ) == WN_PRAGMA_ORDERED_BEGIN?
                              Gen_Ordered(local_gtid):
                              Gen_End_Ordered(local_gtid));
                     WN_INSERT_BlockAfter (tree, kid, call);
                     WN_Delete (WN_EXTRACT_FromBlock (tree, kid));
                  }
         } else if ( WN_opcode(kid) == OPC_REGION) {
            Delayed_MP_Translation( WN_region_body(kid));
         } else {
            Delayed_MP_Translation( kid );
         }
      }
    } else {
       for (kidno = 0; kidno < WN_kid_count(tree); kidno++)
         if (WN_kid(tree, kidno))
            Delayed_MP_Translation( WN_kid(tree, kidno));
    }
}

/*
Strip all nested MP pragmas and MP regions (except CRITICAL SECTION's).  In
the case of MP_IF's generated by LNO for versioning, if
inside_versioning_if is TRUE we set the test of the MP_IF to FALSE so that
we always execute the "else" part (i.e. the serialized version of the
code).
*/

static void 
Strip_Nested_MP ( WN * tree, BOOL pcf_ok )
{
  INT32 kidno;
  ST *lock_st;
  WN *new_kid;
  WN *kid;
  WN *next_kid;
  ST *lock_stack[128];
  INT32 lptr = 0;
  INT32 pcf_nest = 0;
  
  if (tree == NULL)
    return;

  if (WN_opcode(tree) == OPC_BLOCK) {
    for (kid = WN_first(tree); kid; kid = next_kid) {
      next_kid = WN_next(kid);
      if (((WN_opcode(kid) == OPC_PRAGMA) ||
	   (WN_opcode(kid) == OPC_XPRAGMA)) &&
	  (WN_pragmas[WN_pragma(kid)].users & PUSER_MP)) {
	if (WN_pragma(kid) == WN_PRAGMA_CRITICAL_SECTION_BEGIN) {
    // Delay the tranlation after MicroTask creation.
    // now keep it anyway.
    // csc.
        } else if (WN_pragma(kid) == WN_PRAGMA_CRITICAL_SECTION_END) {
    // Delay the tranlation after MicroTask creation.
    // now keep it anyway.
    // csc.
        } else if ((WN_pragma(kid) == WN_PRAGMA_BARRIER) &&
		   (pcf_nest == 0) && pcf_ok) {
	  /* allow barrier nodes to remain */
	} else if ((WN_pragma(kid) == WN_PRAGMA_ENTER_GATE) &&
		   (pcf_nest == 0) && pcf_ok) {
	  /* allow enter_gate nodes to remain */
	} else if ((WN_pragma(kid) == WN_PRAGMA_EXIT_GATE) &&
		   (pcf_nest == 0) && pcf_ok) {
	  /* allow exit_gate nodes to remain */
	} else if ((WN_pragma(kid) == WN_PRAGMA_INDEPENDENT_BEGIN) &&
		   (pcf_nest++ == 0) && pcf_ok) {
	  /* allow independent begin nodes to remain */
	} else if ((WN_pragma(kid) == WN_PRAGMA_INDEPENDENT_END) &&
		   (--pcf_nest == 0) && pcf_ok) {
	  /* allow independent end nodes to remain */

          /* hack: allow SINGLE BEGIN/END pairs: DRK */
	} else if ((WN_pragma(kid) == WN_PRAGMA_SINGLE_PROCESS_BEGIN) &&
		   (pcf_nest++ == 0) && pcf_ok) {
	  /* allow single begin nodes to remain */
    // SINGLE always start a region, is it right? csc.
	} else if ((WN_pragma(kid) == WN_PRAGMA_SINGLE_PROCESS_END) &&
		   (--pcf_nest == 0) && pcf_ok) {
	  /* allow single end nodes to remain */

	} else if ((WN_pragma(kid) == WN_PRAGMA_MASTER_BEGIN) &&
                    (pcf_nest == 0) && pcf_ok ) {
    /* allow master begin nodes to remain */
    // Can it really appear in such case? csc.
        } else if (WN_pragma(kid) == WN_PRAGMA_ORDERED_BEGIN ||
                   WN_pragma(kid) == WN_PRAGMA_ORDERED_END) {
          /* always lower; will cleanup later */
          // What's the meaning?
    // Keep it anyway. Lower it later in Delayed_MP_Translation.
	} else {
	  /* strip all other mp pragmas */
	  if (WN_prev(kid))
	    WN_next(WN_prev(kid)) = WN_next(kid);
	  else
	    WN_first(tree) = WN_next(kid);
	  if (WN_next(kid))
	    WN_prev(WN_next(kid)) = WN_prev(kid);
	  else
	    WN_last(tree) = WN_prev(kid);
	  WN_DELETE_Tree ( kid );
	}
      } else if ((WN_opcode(kid) == OPC_REGION) &&
		 WN_first(WN_region_pragmas(kid)) &&
		 (WN_pragmas[WN_pragma(WN_first(WN_region_pragmas(kid)))].users
								& PUSER_MP)) {
	WN_PRAGMA_ID region_type = (WN_PRAGMA_ID)
	    WN_pragma(WN_first(WN_region_pragmas(kid)));

	if ((region_type == WN_PRAGMA_SINGLE_PROCESS_BEGIN ||
	     region_type == WN_PRAGMA_PDO_BEGIN ||
#ifdef KEY /* Bug 4828 */
             region_type == WN_PRAGMA_PWORKSHARE_BEGIN ||
#endif
             region_type == WN_PRAGMA_MASTER_BEGIN) &&
	    (pcf_nest == 0) && pcf_ok) {
	    // Allow mp region to remain but strip contents. This leaves
	    // non-POD finalization code in place (if any).
      // Indeed, inside master region, no subclause is allowed.
#ifdef KEY /* Bug 4828 */
          if (region_type != WN_PRAGMA_PWORKSHARE_BEGIN)
#endif
  	  Strip_Nested_MP ( WN_region_body(kid), FALSE );
     } else {
	  if (region_type == WN_PRAGMA_PDO_BEGIN)
	    Move_Non_POD_Finalization_Code(WN_region_body(kid));

	  /* splice in the contents of mp region */

	  if (WN_first(WN_region_body(kid))) {
            if (WN_pragma_omp(WN_first(WN_region_pragmas(kid))) &&
                (region_type == WN_PRAGMA_DOACROSS ||
                 region_type == WN_PRAGMA_PARALLEL_DO ||
#ifdef KEY
                 region_type == WN_PRAGMA_PARALLEL_WORKSHARE ||
#endif
                 region_type == WN_PRAGMA_PARALLEL_BEGIN)) {
#ifdef KEY
              MP_process_type nested_mpt =
	        (region_type == (WN_PRAGMA_PARALLEL_BEGIN ||
                                 WN_PRAGMA_PARALLEL_WORKSHARE) ?
	                        MPP_PARALLEL_REGION : MPP_PARALLEL_DO);
#else
              MP_process_type nested_mpt =
	        (region_type == WN_PRAGMA_PARALLEL_BEGIN ?
	                        MPP_PARALLEL_REGION : MPP_PARALLEL_DO);
#endif

              /* insert the spr calls if necessary, before the splicing */
	      // TODO: gtid should be passed as an argument to both GSP/GESP
        // This constructure is already in other greater constructures,
        // sure the gtid is set yet.
        //      WN_INSERT_BlockFirst( WN_region_body( kid ),
        //                     Gen_Store_Gtid( NULL ));
#ifndef KEY
              WN_INSERT_BlockFirst(WN_region_body(kid),
	                           Gen_Serialized_Parallel( local_gtid ));
              WN_INSERT_BlockLast(WN_region_body(kid),
	                          Gen_End_Serialized_Parallel( local_gtid ));
#endif
            }

	    if (WN_prev(kid))
	      WN_next(WN_prev(kid)) = next_kid = WN_first(WN_region_body(kid));
	    else
	      WN_first(tree) = next_kid = WN_first(WN_region_body(kid));
	    WN_prev(WN_first(WN_region_body(kid))) = WN_prev(kid);
	    WN_next(WN_last(WN_region_body(kid))) = WN_next(kid);
	    if (WN_next(kid))
	      WN_prev(WN_next(kid)) = WN_last(WN_region_body(kid));
	    else
	      WN_last(tree) = WN_last(WN_region_body(kid));
	  } else {
	    if (WN_prev(kid))
	      WN_next(WN_prev(kid)) = next_kid = WN_next(kid);
	    else
	      WN_first(tree) = next_kid = WN_next(kid);
	    if (WN_next(kid))
	      WN_prev(WN_next(kid)) = WN_prev(kid);
	    else
	      WN_last(tree) = WN_prev(kid);
	  }
	  WN_DELETE_Tree ( WN_region_pragmas(kid) );
	  WN_DELETE_Tree ( WN_region_exits(kid) );
	  WN_Delete ( WN_region_body(kid) );
	  RID_Delete ( Current_Map_Tab, kid );
	  WN_Delete ( kid );

	}

        /* replace nested versioning MP_IF by its "else" (serialized) kid */
      } else if (WN_opcode(kid) == OPC_IF && WN_Is_If_MpVersion(kid)) {

              // temporary patch for PV 583176: always do
              // WN_Reset_If_MpVersion() on the IF, otherwise certain
              // nested parallel constructs cause a compiler assertion
	if (TRUE ||
	    inside_versioning_if) {
/*
What I really want to do here is to replace the entire MP_IF by the "else"
part, but this somehow corrupts the WHIRL so that CG chokes later on due
to a node with an invalid opcode (i.e. 0).  Need to look into this
later--DRK.
*/

//          DevWarn("replacing test in MP_IF by FALSE value");
//          WN *else_node = WN_else(kid);
          Strip_Nested_MP ( WN_then(kid) , ((pcf_nest == 0) && pcf_ok) );
          Strip_Nested_MP ( WN_else(kid) , ((pcf_nest == 0) && pcf_ok) );
          WN_DELETE_Tree(WN_if_test(kid));
          WN_if_test(kid) = WN_CreateIntconst(OPC_I4INTCONST, 0);
          WN_Reset_If_MpVersion(kid);

        } else {
          BOOL old_ivi = inside_versioning_if;  // restore value at end

          inside_versioning_if = TRUE;
          Strip_Nested_MP ( WN_then(kid) , ((pcf_nest == 0) && pcf_ok) );
          Strip_Nested_MP ( WN_else(kid) , ((pcf_nest == 0) && pcf_ok) );
          inside_versioning_if = old_ivi;
        }

      } else {
	/* recursively strip everything else */
	Strip_Nested_MP ( kid, ((pcf_nest == 0) && pcf_ok) );
      }
    }

  } else {

    for (kidno = 0; kidno < WN_kid_count(tree); kidno++)
      if (WN_kid(tree, kidno))
	Strip_Nested_MP ( WN_kid(tree, kidno), ((pcf_nest == 0) && pcf_ok) );

  }
}


/*  Walk through the preamble code (if any) in preamble blocks looking for
    stores into LOCAL, LASTLOCAL, and FIRSTPRIVATE variables (other than
    PREG's).  If any such
    stores are found then there needs to be a corresponding copyin operation
    created for each one.  */

static void 
Process_Preamble_Stores ( WN * tree, VAR_TABLE * vtab )
{
  OPCODE op;
  OPERATOR opr;
  INT32 i;
  WN *r;
  ST *old_sym;
  WN_OFFSET old_offset;
  VAR_TABLE *w;

  /* Ignore NULL subtrees. */

  if (tree == NULL)
    return;

  /* Initialization. */

  op = WN_opcode(tree);
  opr = OPCODE_operator(op);

  /* Look for any nodes storing into localized, non-preg symbols. */

  if (OPCODE_has_sym(op) && WN_st(tree) &&
      (ST_class(WN_st(tree)) != CLASS_PREG)) {
    if (opr == OPR_STID) {
      old_sym = WN_st(tree);
      old_offset = WN_offsetx(tree);
      for (w=vtab; w->orig_st; w++) {
	if ((w->vtree == NULL) &&
	    (w->orig_st == old_sym) &&
	    (w->has_offset ? (w->orig_offset == old_offset) : TRUE )) {
	  if (copyin_block == NULL)
	    copyin_block = WN_CreateBlock ( );
	  WN_INSERT_BlockLast ( copyin_block,
				Gen_MP_Load_Store ( w->orig_st, old_offset,
						    w->new_st, old_offset,
						    w->is_dynamic_array ));
	  break;
	}
      }
    } else if (opr == OPR_ISTORE) {
      for (w=vtab; w->orig_st; w++) {
	if ((w->vtree && (WN_Compare_Trees(w->vtree, WN_kid1(tree)) == 0)) ||
	    (w->vtreex && (WN_Compare_Trees(w->vtreex, WN_kid1(tree)) == 0))) {
	  if (copyin_block == NULL)
	    copyin_block = WN_CreateBlock ( );
	  WN_INSERT_BlockLast ( copyin_block,
				Gen_MP_Load_Store ( w->orig_st, w->orig_offset,
						    w->new_st, w->new_offset,
						    w->is_dynamic_array ));
	  break;
	}
      }
    }
  }

  /* Walk all children */

  if (op == OPC_BLOCK)
    for (r = WN_first(tree); r; r = WN_next(r))
      Process_Preamble_Stores ( r, vtab );
  else
    for (i=0; i < WN_kid_count(tree); i++)
      Process_Preamble_Stores ( WN_kid(tree, i), vtab );

  return;
}   

/*
 * Called with the serial instance of an ordered omp parallel do
 * Delete calls to 
 *      omp_ordered
 *      omp_end_ordered
 *      omp_pdo_ordered_begin_iter
 *      omp_pdo_ordered_end_iter
 *      omp_ordered_begin
 *      omp_ordered_end
 */
static void 
Cleanup_Ordered (WN* block_wn, WN* wn) {

  OPCODE opc;

  if (!wn) return;

  opc = WN_opcode(wn);
  if (opc == OPC_VCALL) {
    char* name = ST_name(WN_st(wn));
#ifdef KEY
    if (strcmp (name, "__ompc_ordered") == 0 ||
        strcmp (name, "__ompc_end_ordered") == 0 ) {
#else
    if (strcmp (name, "__omp_ordered") == 0 ||
        strcmp (name, "__omp_end_ordered") ==0 ||
        strcmp (name, "__omp_pdo_ordered_begin_iter") == 0 ||
        strcmp (name, "__omp_pdo_ordered_end_iter") == 0 ||
        strcmp (name, "__omp_begin_ordered") == 0 ||
        strcmp (name, "__omp_end_ordered") == 0 ) {
#endif
      WN_DELETE_Tree(WN_EXTRACT_FromBlock (block_wn, wn));
      return;
    }
  }

  if (opc == OPC_REGION && RID_TYPE_mp(REGION_get_rid(wn))) {
    /* don't go into nested parallel regions,
     * since they should retain their clauses.
     * Strictly speaking only nested parallel regions should retain
     * their clauses, while we can delete them from a PDO.
     */
    WN* pragma = WN_first(WN_region_pragmas(wn));
    while (pragma) {
      if (WN_pragma(pragma) == WN_PRAGMA_PARALLEL_BEGIN ||
#ifdef KEY
          WN_pragma(pragma) == WN_PRAGMA_PARALLEL_WORKSHARE ||
#endif
          WN_pragma(pragma) == WN_PRAGMA_PARALLEL_DO) {
        return;
      }
      pragma = WN_next(pragma);
    }
  }

  if (opc == OPC_BLOCK) {
    WN* kid = WN_first(wn);
    while (kid) {
      /* we need to get next since Cleanup_Ordered may blow-away kid */
      WN* tmp = WN_next(kid);
      Cleanup_Ordered (wn, kid);
      kid = tmp;
    }
  }
  else {
    INT i;
    for (i=0; i<WN_kid_count(wn); i++) {
      Cleanup_Ordered (block_wn, WN_kid(wn,i));
    }
  }
}

/*******************************************************************/

/* I move the content of wn_mp_dg.cxx into here.
*  For Copy_Non_MP_Tree need some stuff of this file.
*  csc.
*/

typedef  HASH_TABLE<VINDEX16,VINDEX16> VV_HASH_TABLE;
typedef STACK<VINDEX16> V_STACK;
static MEM_POOL MP_Dep_Pool;
static BOOL mp_dep_pool_initialized;
void Create_Vertices(WN *wn, VV_HASH_TABLE *parent_to_child,
                V_STACK *parent_vertices,
                ARRAY_DIRECTED_GRAPH16 *parent_graph,
                ARRAY_DIRECTED_GRAPH16 *child_graph);

// Fix up the array dependence graph during MP lowering
void 
MP_Fix_Dependence_Graph(PU_Info *parent_pu_info,
                                PU_Info *child_pu_info, WN *child_wn)
{
  ARRAY_DIRECTED_GRAPH16 *parent_graph =
     (ARRAY_DIRECTED_GRAPH16 *) PU_Info_depgraph_ptr(parent_pu_info);
  if (!parent_graph) { // no parent, no child
    Set_PU_Info_depgraph_ptr(child_pu_info,NULL);
    return;
  }
  if (!mp_dep_pool_initialized) {
    MEM_POOL_Initialize(&MP_Dep_Pool,"MP_Dep_Pool",FALSE);
    mp_dep_pool_initialized = TRUE;
  }
  MEM_POOL_Push(&MP_Dep_Pool);

  // Create a new dependence graph for the child region
  ARRAY_DIRECTED_GRAPH16 *child_graph  =
            CXX_NEW(ARRAY_DIRECTED_GRAPH16(100, 500,
                WN_MAP_DEPGRAPH, DEP_ARRAY_GRAPH), Malloc_Mem_Pool);
  Set_PU_Info_depgraph_ptr(child_pu_info,child_graph);
  Set_PU_Info_state(child_pu_info,WT_DEPGRAPH,Subsect_InMem);

  // mapping from the vertices in the parent graph to the corresponding
  // ones in the child
  VV_HASH_TABLE *parent_to_child =
    CXX_NEW(VV_HASH_TABLE(200,&MP_Dep_Pool),&MP_Dep_Pool);
  // a list of the parent vertices in the region
  V_STACK *parent_vertices = CXX_NEW(V_STACK(&MP_Dep_Pool),&MP_Dep_Pool);
  Create_Vertices(child_wn,parent_to_child,parent_vertices,parent_graph,
                                                        child_graph);

  // copy the edges, erase them from the parent graph
  INT i;
  for (i=0; i<parent_vertices->Elements(); i++) {
    VINDEX16 parent_v = parent_vertices->Bottom_nth(i);
    VINDEX16 child_v = parent_to_child->Find(parent_v);
    Is_True(child_v,("child_v missing "));
    EINDEX16 e;
    while (e = parent_graph->Get_Out_Edge(parent_v)) {
      VINDEX16 parent_sink = parent_graph->Get_Sink(e);
      VINDEX16 child_sink = parent_to_child->Find(parent_sink);
      Is_True(child_sink,("child_sink missing "));
      child_graph->Add_Edge(child_v,child_sink,
                parent_graph->Dep(e),parent_graph->Is_Must(e));

      parent_graph->Remove_Edge(e);
    }
  }
  for (i=0; i<parent_vertices->Elements(); i++) {
    // remove the vertex from the parent graph
    // since removing the vertex cleans the wn map, reset it
    VINDEX16 parent_v = parent_vertices->Bottom_nth(i);
    VINDEX16 child_v = parent_to_child->Find(parent_v);
    WN *wn = parent_graph->Get_Wn(parent_v);
    parent_graph->Delete_Vertex(parent_v);
    child_graph->Set_Wn(child_v,wn);
  }
  CXX_DELETE(parent_to_child,&MP_Dep_Pool);
  CXX_DELETE(parent_vertices,&MP_Dep_Pool);
  MEM_POOL_Pop(&MP_Dep_Pool);
}

// walk the child, find all the vertices, create corresponding vertices
// in the child graph, fill up the hash table and stack
void 
Create_Vertices(WN *wn, VV_HASH_TABLE *parent_to_child,
                V_STACK *parent_vertices,
                ARRAY_DIRECTED_GRAPH16 *parent_graph,
                ARRAY_DIRECTED_GRAPH16 *child_graph)
{
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Create_Vertices(kid,parent_to_child,parent_vertices,parent_graph,
                                                        child_graph);
      kid = WN_next(kid);
    }
    return;
  }
  if (OPCODE_is_load(opcode) || OPCODE_is_store(opcode)
      || OPCODE_is_call(opcode)) {
    VINDEX16 parent_v = parent_graph->Get_Vertex(wn);
    if (parent_v) {
      // can't overflow since parent graph has
      // at least the same number of vertices
      VINDEX16 child_v = child_graph->Add_Vertex(wn);
      parent_to_child->Enter(parent_v,child_v);
      parent_vertices->Push(parent_v);
    }
  }
  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    Create_Vertices(WN_kid(wn,kidno),parent_to_child,parent_vertices,
                                        parent_graph,child_graph);
  }
}


/*
Transform "lastprivate" non-POD finalization code from this:

#pragma omp for
  for (...) {
    ...
    if (__omp_non_pod_lastlocal) {
      ... // finalization code
    }
  }

to this:

#pragma omp for
 {
  for (...) {
    ...
  }
  ... // finalization code
 }

Note that due to pseudolowering, the lastprivate IF could appear deeper
in the Whirl tree than in this example, but in no case will it be beneath a
PDO region.

The block argument must be either the body_block of the PDO region, or (for
the serial version of MP code) a BLOCK that contains the DO_LOOP that in
turn contains the non-POD lastprivate IF. If the block doesn't contain a
DO_LOOP or the DO_LOOP body doesn't contain a non-POD lastprivate IF, the
block is left unchanged. Because the serial version of MP code can contain
serial DO loops before any serialized PDOs or multiple serialized PDOs, we
do this tranformation on every outermost loop we find in block
*/

static WN *Find_Non_POD_Finalization_Code(WN *wn, WN **final_if_parent);
static void Move_Non_POD_Finalization_Code_Rec(WN *wn);
static void Find_And_Move_Finalization_Code(WN *parent, WN *do_wn);

void 
Move_Non_POD_Finalization_Code(WN *block)
{
  Is_True(block && WN_operator(block) == OPR_BLOCK, ("bad block"));
  Move_Non_POD_Finalization_Code_Rec(block);
}

// This routine does all the recursion. We don't recurse into DO loops.
static void 
Move_Non_POD_Finalization_Code_Rec(WN *wn)
{
  Is_True(wn, ("NULL wn"));
  Is_True(WN_operator(wn) != OPR_DO_LOOP, ("recursed into a DO loop"));

  if (WN_operator(wn) == OPR_BLOCK) {
    for (WN *stmt = WN_first(wn); stmt; stmt = WN_next(stmt)) {
      if (WN_operator(stmt) == OPR_DO_LOOP)
        Find_And_Move_Finalization_Code(wn, stmt);
      else
        Move_Non_POD_Finalization_Code_Rec(stmt);
    }

  } else {
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
      Move_Non_POD_Finalization_Code_Rec(WN_kid(wn, kidno));
  }
}

static void 
Find_And_Move_Finalization_Code(WN *parent, WN *do_wn)
{
  Is_True(parent && WN_operator(parent) == OPR_BLOCK, ("bad parent"));
  Is_True(do_wn && WN_operator(do_wn) == OPR_DO_LOOP, ("bad do_wn"));

  WN *final_if, *final_if_parent, *do_body = WN_do_body(do_wn);

  final_if = Find_Non_POD_Finalization_Code(do_body, &final_if_parent);

  if (!final_if)
    return;

    // remove mem barriers around the IF
  WN *bar1 = WN_prev(final_if), *bar2 = WN_next(final_if),
     *then = WN_then(final_if), *bar3 = WN_first(then),
     *bar4 = WN_last(then);
  Is_True(bar1 && WN_operator(bar1) == OPR_FORWARD_BARRIER,
          ("bad bar1"));
  Is_True(bar2 && WN_operator(bar2) == OPR_BACKWARD_BARRIER,
          ("bad bar2"));
  Is_True(bar3 && WN_operator(bar3) == OPR_BACKWARD_BARRIER,
          ("bad bar3"));
  Is_True(bar4 && WN_operator(bar4) == OPR_FORWARD_BARRIER,
          ("bad bar4"));
  WN_DELETE_FromBlock(final_if_parent, bar1);
  WN_DELETE_FromBlock(final_if_parent, bar2);
  WN_DELETE_FromBlock(then, bar3);
  WN_DELETE_FromBlock(then, bar4);

    // move finalization code after the DO loop
  WN_EXTRACT_FromBlock(final_if_parent, final_if);
  WN *final_code = WN_then(final_if);
  WN_then(final_if) = NULL;
  WN_DELETE_Tree(final_if);
  WN_INSERT_BlockAfter(parent, do_wn, final_code);
}


static WN *
Find_Non_POD_Finalization_Code(WN *wn, WN **final_if_parent)
{
  Is_True(wn, ("NULL wn"));
  if (WN_operator(wn) == OPR_BLOCK) {
      // Search block for final_if. Recurse into all other statements,
      // except PDO regions.
    for (WN *stmt = WN_first(wn); stmt; stmt = WN_next(stmt)) {
      BOOL first_and_last;
      if (Is_Nonpod_Finalization_IF(stmt, &first_and_last)) {
        *final_if_parent = wn;
        return stmt;
      }

      if (WN_operator(stmt) == OPR_REGION &&
          WN_first(WN_region_pragmas(stmt)) &&
          WN_pragma(WN_first(WN_region_pragmas(stmt))) == WN_PRAGMA_PDO_BEGIN)
        continue; // don't recurse into PDO region

        // recuse into stmt
      WN *retval = Find_Non_POD_Finalization_Code(stmt, final_if_parent);
      if (retval)
        return retval;
    }

  } else {  // Recurse into all kids.
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
      WN *retval = Find_Non_POD_Finalization_Code(WN_kid(wn, kidno),
                                                  final_if_parent);
      if (retval)
        return retval;
    }
  }

  return NULL;  // didn't find it
}


/*  Copy a tree skipping all MP pragma nodes (except CRITICAL SECTION's).  */
/*  Copy the CG dependence graph if it exists */
/*  Move fecc-generated finalization code for non-POD "lastprivate" vars
    to the appropriate place */

static WN * Copy_Non_MP_Tree_Rec ( WN * tree , V_STACK *mp_vertices,
                                        VV_HASH_TABLE *mp_to_nonmp);

WN * 
Copy_Non_MP_Tree( WN * tree )
{
  V_STACK *mp_vertices=0;
  VV_HASH_TABLE *mp_to_nonmp = 0;
  if (Current_Dep_Graph) {
    if (!mp_dep_pool_initialized) {
      MEM_POOL_Initialize(&MP_Dep_Pool,"MP_Dep_Pool",FALSE);
      mp_dep_pool_initialized = TRUE;
    }
    MEM_POOL_Push(&MP_Dep_Pool);
    mp_vertices = CXX_NEW(V_STACK(&MP_Dep_Pool),&MP_Dep_Pool);
    mp_to_nonmp = CXX_NEW(VV_HASH_TABLE(200,&MP_Dep_Pool),&MP_Dep_Pool);
  }
  WN *result = Copy_Non_MP_Tree_Rec(tree,mp_vertices,mp_to_nonmp);

  // copy the edges
  if (Current_Dep_Graph) {
    for (INT i=0; i<mp_vertices->Elements(); i++) {
      VINDEX16 mp_v = mp_vertices->Bottom_nth(i);
      VINDEX16 nonmp_v = mp_to_nonmp->Find(mp_v);
      Is_True(nonmp_v,("nonmp_v missing "));
      EINDEX16 e = Current_Dep_Graph->Get_Out_Edge(mp_v);
      while (e) {
        VINDEX16 mp_sink = Current_Dep_Graph->Get_Sink(e);
        VINDEX16 nonmp_sink = mp_to_nonmp->Find(mp_sink);
        Is_True(nonmp_sink,("nonmp_sink missing "));
        if ((nonmp_v != nonmp_sink) || !
                        Current_Dep_Graph->Get_Edge(nonmp_v,nonmp_v)) {
          if (!Current_Dep_Graph->Add_Edge(nonmp_v,nonmp_sink,
                Current_Dep_Graph->Dep(e),Current_Dep_Graph->Is_Must(e))) {
            Current_Dep_Graph->Erase_Graph();
            Current_Dep_Graph = NULL;
            Set_PU_Info_depgraph_ptr(Current_PU_Info,Current_Dep_Graph);
            Set_PU_Info_state(Current_PU_Info,WT_DEPGRAPH,Subsect_InMem);

            return result;
          }
        }
        e = Current_Dep_Graph->Get_Next_Out_Edge(e);
      }
    }
  }
  return result;
}

static WN * 
Copy_Non_MP_Tree_Rec ( WN * tree , V_STACK *mp_vertices,
                                        VV_HASH_TABLE *mp_to_nonmp)
{
  INT32 kidno;
  ST *lock_st;
  WN *new_wn;
  WN *kid;
  WN *prev_kid;
  WN *next_kid;
  WN *new_kid;
#define STACK_CHUNK 10
  INT32 *spr_stack = NULL;      /* serialized parallel region?
                                 *      No? 0
                                 *      yes -- parallel region? 1
                                 *      yes -- doacross?        2
                                 */
  WN **kid_stack = NULL;
  INT32 kptr = 0;               /* index into kid_stack and spr_stack */
  INT32 kptr_max = 0;           /* size of kid_stack */
  ST **lock_stack = NULL;
  INT32 lptr = 0;               /* index into lock_stack */
  INT32 lptr_max = 0;           /* size of lock_stack */
                                /* need to call
                                   Move_Non_POD_Finalization_Code() on
                                   copied version of code */
  BOOL must_move_non_pod = FALSE;

  if (tree == NULL)
    return (NULL);

#ifdef Is_True_On
  if (WN_opcode(tree) == OPC_REGION) {
    RID *rid = REGION_get_rid(tree);
    Is_True(rid != NULL, ("Copy_Non_MP_Tree_Rec, NULL rid"));
  }
#endif

  new_wn = WN_CopyNode (tree);

  if (Current_Dep_Graph) {
    VINDEX16 mp_v = Current_Dep_Graph->Get_Vertex(tree);
    if (mp_v) {
      VINDEX16 nonmp_v = Current_Dep_Graph->Add_Vertex(new_wn);
      if (!nonmp_v) {
        Current_Dep_Graph->Erase_Graph();
        Current_Dep_Graph = NULL;
      } else {
        mp_vertices->Push(mp_v);
        mp_to_nonmp->Enter(mp_v,nonmp_v);
      }
    }
  }

  if (WN_opcode(tree) == OPC_BLOCK) {

    prev_kid = new_kid = NULL;
    for (kid = WN_first(tree); kid; kid = next_kid) {
      next_kid = WN_next(kid);
      if (((WN_opcode(kid) == OPC_PRAGMA) ||
           (WN_opcode(kid) == OPC_XPRAGMA)) &&
          (WN_pragmas[WN_pragma(kid)].users & PUSER_MP)) {
        /* translate critical section begin & end nodes, ignore all other mp
           pragma nodes */
        if (WN_pragma(kid) == WN_PRAGMA_CRITICAL_SECTION_BEGIN) {
          if ((WN_opcode(kid) == OPC_XPRAGMA) &&
              (WN_operator(WN_kid0(kid)) == OPR_LDA))
//Bug 4515
#ifdef KEY
            lock_st = Get_NameLock_ST( WN_st(WN_kid0(kid)) );
#else
            lock_st = WN_st(WN_kid0(kid));
#endif
          else if ((WN_opcode(kid) == OPC_PRAGMA) && WN_st(kid))
//Bug 4515
#ifdef KEY
            lock_st = Get_NameLock_ST( WN_st(kid) );
#else
            lock_st = WN_st(kid);
#endif
          else{
            lock_st = NULL;
	          Create_Unnamed_Critical_Lock( );
	  }
          if (lptr == lptr_max) {
                 lock_stack = (ST**) MEM_POOL_Realloc (Malloc_Mem_Pool, lock_stack,
                       sizeof(ST*)*lptr_max,
                       sizeof(ST*)*(lptr_max+STACK_CHUNK));
                 lptr_max += STACK_CHUNK;
                }
          lock_stack[lptr++] = lock_st;
          Create_Gtid_ST( );
          if (lock_st)
            new_kid = Gen_Critical( local_gtid, lock_st );
          else
            new_kid = Gen_Critical( local_gtid, unnamed_lock_st );
          WN_prev(new_kid) = prev_kid;
          if (prev_kid)
            WN_next(prev_kid) = new_kid;
          else
            WN_first(new_wn) = new_kid;
          prev_kid = new_kid;
        } else if (WN_pragma(kid) == WN_PRAGMA_CRITICAL_SECTION_END) {
          lock_st = lock_stack[--lptr];
          if (lock_st)
            new_kid = Gen_End_Critical(local_gtid, lock_st);
          else
            new_kid = Gen_End_Critical(local_gtid, unnamed_lock_st);
          WN_prev(new_kid) = prev_kid;
          if (prev_kid)
            WN_next(prev_kid) = new_kid;
          else
            WN_first(new_wn) = new_kid;
          prev_kid = new_kid;
        }
      } // if (((WN_opcode(kid) == OPC_PRAGMA) ...
      else if ((WN_opcode(kid) == OPC_REGION) &&
                 WN_first(WN_region_pragmas(kid)) &&
                 (WN_pragmas[WN_pragma(WN_first(WN_region_pragmas(kid)))].users
                                                                & PUSER_MP)) {

        WN_PRAGMA_ID pragma =
          (WN_PRAGMA_ID) WN_pragma(WN_first(WN_region_pragmas(kid)));
        if (kptr == kptr_max) {
          kid_stack = (WN**) MEM_POOL_Realloc (Malloc_Mem_Pool, kid_stack,
                                         sizeof(WN*) * kptr_max,
                                         sizeof(WN*) * (kptr_max+STACK_CHUNK));
          spr_stack = (INT32*) MEM_POOL_Realloc (Malloc_Mem_Pool, spr_stack,
                                         sizeof(INT32)*kptr_max,
                                         sizeof(INT32)*(kptr_max+STACK_CHUNK));
          kptr_max += STACK_CHUNK;
        }

        spr_stack[kptr] = 0;
        if (WN_pragma_omp(WN_first(WN_region_pragmas(kid))) &&
            (pragma == WN_PRAGMA_DOACROSS ||
             pragma == WN_PRAGMA_PARALLEL_DO ||
             pragma == WN_PRAGMA_PARALLEL_WORKSHARE ||
             pragma == WN_PRAGMA_PARALLEL_BEGIN)) {
#ifdef KEY
          MP_process_type mpt = ((pragma == WN_PRAGMA_PARALLEL_BEGIN ||
                                  pragma == WN_PRAGMA_PARALLEL_WORKSHARE) ?
                                 MPP_PARALLEL_REGION :
                                 MPP_PARALLEL_DO);
#else
          MP_process_type mpt = (pragma == WN_PRAGMA_PARALLEL_BEGIN ?
                                 MPP_PARALLEL_REGION :
                                 MPP_PARALLEL_DO);
#endif
          new_kid = Gen_OMP_Begin_SPR(mpt);
          WN_prev(new_kid) = prev_kid;
          if (prev_kid)
            WN_next(prev_kid) = new_kid;
          else
            WN_first(new_wn) = new_kid;
          prev_kid = new_kid;
          spr_stack[kptr] = (mpt == MPP_PARALLEL_REGION ? 1 : 2);
        } else if (WN_pragma_omp(WN_first(WN_region_pragmas(kid))) &&
                   pragma == WN_PRAGMA_PDO_BEGIN) {
          must_move_non_pod = TRUE;
        }
        // only copy the kids of mp regions
        kid_stack[kptr++] = next_kid;
        next_kid = WN_first(WN_region_body(kid));
      } else {
        // copy everything else
        new_kid = Copy_Non_MP_Tree_Rec ( kid, mp_vertices,mp_to_nonmp );
        WN_prev(new_kid) = prev_kid;
        if (prev_kid)
          WN_next(prev_kid) = new_kid;
        else
          WN_first(new_wn) = new_kid;
        prev_kid = new_kid;

        // make a new rid for non-mp region
        if (WN_opcode(kid) == OPC_REGION) {
          RID *rid = REGION_get_rid(kid);
          if (!RID_TYPE_mp(rid)) { // not MP region
              // new_kid (serial code) replaces kid (parallel code) in
              // parent PU, so new_kid must get kid's RID
            mUINT32 new_region_id = WN_region_id(new_kid);
            mUINT32 old_region_id = WN_region_id(kid);

            WN_set_region_id(new_kid, WN_region_id(kid));
            REGION_new_wn(new_kid, kid);
            WN_set_region_id(kid, New_Region_Id());
            REGION_clone(new_kid, kid, NULL);
          }

        }
      }
      if ((next_kid == NULL) && kptr) {
        if (spr_stack[kptr-1] != 0) {
          /* generate an end-spr */
          new_kid = Gen_OMP_End_SPR(spr_stack[kptr-1] == 1?
                                    MPP_PARALLEL_REGION :
                                    MPP_PARALLEL_DO);
          WN_prev(new_kid) = prev_kid;
          if (prev_kid)
            WN_next(prev_kid) = new_kid;
          else
            WN_first(new_wn) = new_kid;
          prev_kid = new_kid;
        }

        next_kid = kid_stack[--kptr];
#ifdef KEY
	// bug 4989: Pop the stack until a non-NULL node is found
	while (!next_kid && kptr)
	  next_kid = kid_stack[--kptr];
#endif // KEY
      }

    } // for (kid = WN_first(tree); kid; kid = next_kid)

    if (new_kid)
      WN_next(new_kid) = NULL;
    else
      WN_first(new_wn) = NULL;
    WN_last(new_wn) = new_kid;

    if (must_move_non_pod)
      Move_Non_POD_Finalization_Code(new_wn);

  } // if (WN_opcode(tree) == OPC_BLOCK)
  else {

    for (kidno = 0; kidno < WN_kid_count(tree); kidno++) {
      kid = WN_kid(tree, kidno);
      if (kid)
        WN_kid(new_wn, kidno) = Copy_Non_MP_Tree_Rec ( kid, mp_vertices,
                                                      mp_to_nonmp );
      else
        WN_kid(new_wn, kidno) = NULL;
    }

  }

  if (lock_stack) MEM_POOL_FREE (Malloc_Mem_Pool, lock_stack);
  if (kid_stack)  MEM_POOL_FREE (Malloc_Mem_Pool, kid_stack);
  if (spr_stack)  MEM_POOL_FREE (Malloc_Mem_Pool, spr_stack);
  return (new_wn);
}

/* End the Content of wn_mp_dg.cxx.
*  csc.
*/

/* standardize do comp operations.
 * required by RTL.
 * LT -> LE, GT -> LE
 * csc.
 * must be called before Extract_Do_Info
 */

static void
Standardize_Do (WN* do_tree)
{
  if (WN_operator(WN_end(do_tree)) == OPR_GE 
      || WN_operator(WN_end(do_tree)) == OPR_LE )
  {
    // need to do nothing.
    return;
  }
  else
  {
    WN_Upper_Bound_Standardize(do_tree, WN_end(do_tree), TRUE);
  }
}

/*
* Extract do info for mp scheduling. 
*/

static void 
Extract_Do_Info ( WN * do_tree )
{
  // standardize do tree.
  Standardize_Do(do_tree);

  WN        *do_idname  = WN_index(do_tree);
  ST        *do_id_st   = WN_st(do_idname);
  WN_OFFSET  do_id_ofst = WN_offsetx(do_idname);
  WN        *do_init;
  WN        *do_limit;
  WN        *do_stride;
  BOOL      was_kid0 = FALSE;

  /* Extract mp scheduling info from do */

  do_init = WN_kid0(WN_start(do_tree));
  WN_kid0(WN_start(do_tree)) = NULL;

#ifdef KEY
  {
    // bug 5767: handle cvt
    WN * kid0 = WN_kid0 (WN_end (do_tree));
    if (WN_operator (kid0) == OPR_CVT)
      kid0 = WN_kid0 (kid0);

    WN * kid1 = WN_kid1 (WN_end (do_tree));
    if (WN_operator (kid1) == OPR_CVT)
      kid1 = WN_kid0 (kid1);

    if (WN_operator (kid0) == OPR_LDID &&
        WN_st (kid0) == do_id_st &&
        WN_offsetx (kid0) == do_id_ofst)
    { // kid0
      was_kid0 = TRUE;
      do_limit = WN_kid1 (WN_end (do_tree));
      WN_kid1 (WN_end (do_tree)) = NULL;
    }
    else if (WN_operator (kid1) == OPR_LDID &&
             WN_st (kid1) == do_id_st &&
             WN_offsetx (kid1) == do_id_ofst)
    { // kid1
      do_limit = WN_kid0 (WN_end (do_tree));
      WN_kid0 (WN_end (do_tree)) = NULL;
    }
    else
    { // try again
      WN_Upper_Bound_Standardize ( do_tree, WN_end(do_tree), TRUE );
      // handle cvt
      kid0 = WN_kid0 (WN_end (do_tree));
      if (WN_operator (kid0) == OPR_CVT)
        kid0 = WN_kid0 (kid0);

      kid1 = WN_kid1 (WN_end (do_tree));
      if (WN_operator (kid1) == OPR_CVT)
        kid1 = WN_kid0 (kid1);

      if (WN_operator (kid0) == OPR_LDID &&
          WN_st (kid0) == do_id_st &&
          WN_offsetx (kid0) == do_id_ofst)
      { // kid0
        was_kid0 = TRUE;
        do_limit = WN_kid1 (WN_end (do_tree));
        WN_kid1 (WN_end (do_tree)) = NULL;
      }
      else if (WN_operator (kid1) == OPR_LDID &&
               WN_st (kid1) == do_id_st &&
               WN_offsetx (kid1) == do_id_ofst)
      { // kid1
        do_limit = WN_kid0 (WN_end (do_tree));
        WN_kid0 (WN_end (do_tree)) = NULL;
      }
      else // fail
        Fail_FmtAssertion ( "malformed limit test in MP processing" );
    }
  }
#else
  if ((WN_operator(WN_kid0(WN_end(do_tree))) == OPR_LDID) &&
      (WN_st(WN_kid0(WN_end(do_tree))) == do_id_st) &&
      (WN_offsetx(WN_kid0(WN_end(do_tree))) == do_id_ofst)) {
    was_kid0 = TRUE;
    do_limit = WN_kid1(WN_end(do_tree));
    WN_kid1(WN_end(do_tree)) = NULL;
  } else if ((WN_operator(WN_kid1(WN_end(do_tree))) == OPR_LDID) &&
	     (WN_st(WN_kid1(WN_end(do_tree))) == do_id_st) &&
	     (WN_offsetx(WN_kid1(WN_end(do_tree))) == do_id_ofst)) {
    do_limit = WN_kid0(WN_end(do_tree));
    WN_kid0(WN_end(do_tree)) = NULL;
  } else {
    WN_Upper_Bound_Standardize ( do_tree, WN_end(do_tree), TRUE );
    if ((WN_operator(WN_kid0(WN_end(do_tree))) == OPR_LDID) &&
	(WN_st(WN_kid0(WN_end(do_tree))) == do_id_st) &&
	(WN_offsetx(WN_kid0(WN_end(do_tree))) == do_id_ofst)) {
      was_kid0 = TRUE;
      do_limit = WN_kid1(WN_end(do_tree));
      WN_kid1(WN_end(do_tree)) = NULL;
    } else if ((WN_operator(WN_kid1(WN_end(do_tree))) == OPR_LDID) &&
	       (WN_st(WN_kid1(WN_end(do_tree))) == do_id_st) &&
	       (WN_offsetx(WN_kid1(WN_end(do_tree))) == do_id_ofst)) {
      do_limit = WN_kid0(WN_end(do_tree));
      WN_kid0(WN_end(do_tree)) = NULL;
    } else {
      Fail_FmtAssertion ( "malformed limit test in MP processing" );
    }
  }
#endif

  if ((WN_operator(WN_kid0(WN_kid0(WN_step(do_tree)))) == OPR_LDID) &&
      (WN_st(WN_kid0(WN_kid0(WN_step(do_tree)))) == do_id_st) &&
      (WN_offsetx(WN_kid0(WN_kid0(WN_step(do_tree)))) == do_id_ofst))
  {
    do_stride = WN_COPY_Tree ( WN_kid1(WN_kid0(WN_step(do_tree))) );
#ifdef KEY
    if (WN_operator (WN_kid0 (WN_step (do_tree))) == OPR_SUB)
    { // the loop goes down, don't miss '-' in (- non-const-stride)
      OPCODE negop = OPCODE_make_op (OPR_NEG, WN_rtype (do_stride), MTYPE_V);
      do_stride = WN_CreateExp1 (negop, do_stride);
    }
#endif // KEY
  }
  else
    do_stride = WN_COPY_Tree ( WN_kid0(WN_kid0(WN_step(do_tree))) );

  /* Generate mp scheduling expressions */

  base_node = do_init;
    // used by Rewrite_Do, need to be copied ?
  limit_node = WN_COPY_Tree( do_limit );

  if (((WN_operator(WN_end(do_tree)) == OPR_LT) && was_kid0) ||
      ((WN_operator(WN_end(do_tree)) == OPR_GT) && !was_kid0)) { 
    WN* wn_exp0 = WN_Sub(do_index_type, do_limit, WN_COPY_Tree(do_init));
    wn_exp0 = WN_Integer_Cast(wn_exp0, do_index_type, WN_rtype(wn_exp0));
    WN* wn_exp1 = WN_Add(do_index_type, wn_exp0, WN_COPY_Tree(do_stride));
    wn_exp1 = WN_Integer_Cast(wn_exp1, do_index_type, WN_rtype(wn_exp1));
    WN* wn_exp2 = WN_Sub(do_index_type, wn_exp1, WN_Intconst(do_index_type, 1));
    wn_exp2 = WN_Integer_Cast(wn_exp2, do_index_type, WN_rtype(wn_exp2));
    WN* wn_exp3 = WN_Div(do_index_type, wn_exp2, WN_COPY_Tree(do_stride));
    ntrip_node = wn_exp3; 
  } else if (((WN_operator(WN_end(do_tree)) == OPR_GT) && was_kid0) ||
           ((WN_operator(WN_end(do_tree)) == OPR_LT) && !was_kid0)) { 
    WN* wn_exp0 = WN_Sub(do_index_type, do_limit, WN_COPY_Tree(do_init));
    wn_exp0 = WN_Integer_Cast(wn_exp0, do_index_type, WN_rtype(wn_exp0));
    WN* wn_exp1 = WN_Add(do_index_type, wn_exp0, WN_Intconst(do_index_type, 1));
    wn_exp1 = WN_Integer_Cast(wn_exp1, do_index_type, WN_rtype(wn_exp1));
    WN* wn_exp2 = WN_Add(do_index_type, wn_exp1, WN_COPY_Tree(do_stride));
    wn_exp2 = WN_Integer_Cast(wn_exp2, do_index_type, WN_rtype(wn_exp2));
    WN* wn_exp3 = WN_Div(do_index_type, wn_exp2, WN_COPY_Tree(do_stride));
    ntrip_node = wn_exp3; 
  } else { 
    WN* wn_exp0 = WN_Sub(do_index_type, do_limit, WN_COPY_Tree(do_init));
    wn_exp0 = WN_Integer_Cast(wn_exp0, do_index_type, WN_rtype(wn_exp0));
    WN* wn_exp1 = WN_Add(do_index_type, wn_exp0, WN_COPY_Tree(do_stride));
    wn_exp1 = WN_Integer_Cast(wn_exp1, do_index_type, WN_rtype(wn_exp1));
    WN* wn_exp2 = WN_Div(do_index_type, wn_exp1, WN_COPY_Tree(do_stride));
    ntrip_node = wn_exp2; 
  } 
  stride_node = do_stride;

}

/*
* Rewrite do statement. New version. by csc. 
* Changes: rewrite do_tree's control vars.
* Note that, the iteration space is [lower, upper],
* So the test part of if must be compliant with this.
* stride == NULL for static/static even, not modify 
* stride of loop, else for others, need to modify.
*/

static WN *
Rewrite_Do_New ( WN * do_tree,
                 ST * lower,
                 ST * upper,
                 ST * stride )
{
  WN        *loop_info;
  WN        *do_idname  = WN_index(do_tree);
  ST        *do_id_st   = WN_st(do_idname);
  WN_OFFSET  do_id_ofst = WN_offsetx(do_idname);

  // So, how do you know which node to replace?
  // Is there any fix code must be presented here?
  /* Fix up the do loop controls */

/* Temporary fix for PV 381272.  Wopt cannot handle double convert of I8 -> I1/2
   and so we coerce I8 to I4 first.  Note that the offset (4) is correct for
   little endian systems only. */
  if ((ST_sclass(lower) == SCLASS_AUTO) &&
      (ST_btype(lower) == MTYPE_I8) &&
      ((WN_desc(WN_start(do_tree)) == MTYPE_I1) ||
       (WN_desc(WN_start(do_tree)) == MTYPE_I2)))
  { 
    WN_kid0(WN_start(do_tree)) = WN_RLdid ( Promote_Type(MTYPE_I4), MTYPE_I4, 4,
					    lower, MTYPE_To_TY(MTYPE_I4));
  } else
  { 
/* End temporary fix for PV 381272. */
    WN* wn_local_start = Gen_MP_Load(lower, 0);
    WN* wn_new_local_start = WN_Integer_Cast(wn_local_start, 
        Promote_Type(WN_desc(WN_start(do_tree))), WN_rtype(wn_local_start));
    WN_kid0(WN_start(do_tree)) = wn_new_local_start; 
  } 

  //The transform from LT/GT -> LE/GE is quite complex,
  //now let it be. touch it later. csc, 2002/6/4 
  //The common case is LE/GE, not sured.
  WN* wn_ldid = Gen_MP_Load( upper, 0 );
/*
  if( WN_operator( WN_end( do_tree )) == OPR_LT )
  {
    WN_operator( WN_end( do_tree )) = OPR_LE;
    wn_ldid = WN_Add( do_index_type, wn_ldid, 
        WN_CreateIntconst( do_index_type, 1));
  }
  else if( WN_operator( WN_end( do_tree )) == OPR_GT )
  {

  }
*/
  WN* wn_cvt_ldid = wn_ldid; 
  if (WN_rtype(wn_cvt_ldid) != WN_desc(WN_end(do_tree)))
     wn_cvt_ldid = WN_Integer_Cast(wn_cvt_ldid, WN_desc(WN_end(do_tree)),
       WN_rtype(wn_cvt_ldid));
  if (WN_kid0(WN_end(do_tree)) == NULL)
  { 
    WN_kid0(WN_end(do_tree)) = wn_cvt_ldid; 
  } else
  { 
    WN_kid1(WN_end(do_tree)) = wn_cvt_ldid;
  } 

  if( stride != NULL )
  {
    wn_ldid = Gen_MP_Load( stride, 0 );
    wn_cvt_ldid = wn_ldid;
      // really needed? WN_Integer_Cast does the judge itself.
    if( WN_rtype( wn_cvt_ldid ) != WN_desc(  WN_step( do_tree )))
    {
      wn_cvt_ldid = WN_Integer_Cast( wn_cvt_ldid,
          WN_desc( WN_step( do_tree )),
          WN_rtype( wn_cvt_ldid ));
    }
    if ((WN_operator(WN_kid0(WN_kid0(WN_step(do_tree)))) == OPR_LDID) &&
        (WN_st(WN_kid0(WN_kid0(WN_step(do_tree)))) == do_id_st) &&
        (WN_offsetx(WN_kid0(WN_kid0(WN_step(do_tree)))) == do_id_ofst))
    {
       WN_DELETE_Tree( WN_kid1( WN_kid0( WN_step( do_tree ))));
       WN_kid1( WN_kid0( WN_step( do_tree ))) = wn_cvt_ldid;
    }
    else
    {
       WN_DELETE_Tree( WN_kid0( WN_kid0( WN_step( do_tree ))));
       WN_kid0( WN_kid0( WN_step( do_tree ))) = wn_cvt_ldid;
    }
  }

  /* Fix up the optional LOOP_INFO node */
  // TODO: need to be fixed. csc

  loop_info = WN_do_loop_info(do_tree);
#ifndef KEY
  if (loop_info)
#else
  // Bug 4809 - we will preserve the loop trip count information if the 
  // original trip count is an integer constant. For MP DO loops, the loop
  // bounds are determined at run-time. However, if the loop bounds for the 
  // original loop was a compile time constant, it helps to transfer that 
  // information to the later compilation phases (the trip count information
  // is an estimate). In the case of STREAM -mp, this fix specifically lets the
  // CG loop optimizer to convert stores to non-temporal stores because the
  // loop trip count is now made available.
  if (loop_info && (!WN_loop_trip(loop_info) || 
		    !WN_operator_is(WN_loop_trip(loop_info), OPR_INTCONST)))
#endif
  {
		WN_set_do_loop_info( do_tree, NULL );
		WN_set_kid_count( do_tree, 5 );
		WN_DELETE_Tree( loop_info );
		// TODO: Fix the loop_info. csc. 2002/11/14
/*
    WN_loop_trip_est(loop_info) = 0;
    WN_loop_depth(loop_info) = 1;
//    WN_Reset_Loop_Nz_Trip ( loop_info );
    if (WN_loop_trip(loop_info))
    {
      WN_DELETE_Tree ( WN_loop_trip(loop_info) );
      // later we should fixup the ntrip node
//      WN_set_loop_trip ( loop_info, Gen_MP_Load ( local_ntrip, 0 ));
    }
*/
  }
  return do_tree;
} // Rewrite_Do_New()

/*
Transform do statement. This is a new version
written by csc.
Note: call Make_Local_Temps( ) before this.
return a tree to replace do_tree, and do_tree should not be
contained in other trees..
*/

static WN *
Transform_Do( WN * do_tree, 
                         SCHEDULE_TYPE schedule, 
                         WN * chunk_size)
{
  WN        *wn, *wn_tmp;
  WN        *do_idname  = WN_index(do_tree);
  ST        *do_id_st   = WN_st(do_idname);
  WN_OFFSET  do_id_ofst = WN_offsetx(do_idname);
  WN        *do_stride;
  WN        *do_schedule;
  WN	      *if_wn;
  WN        *test_wn, *then_wn, *else_wn;
  WN        *while_wn;
  WN        *while_test, *while_body;
//  WN        *wn_prev_do, *wn_next_do;

  WN        *call_wn;
  
  PREG_NUM rreg1, rreg2;

  WN        *loop_info;
  ST        *return_st;
  WN_OFFSET return_ofst;
  WN        *return_wn = WN_CreateBlock( );

  // Now, move to global scope.
//  ST        *limit_st;
//  WN_OFFSET  limit_ofst;

//  WN_OFFSET  temp_ofst;
  WN        *do_init_call = NULL;
  BOOL       is_do32 = TRUE;
  BOOL       is_LE = TRUE;
  BOOL       is_kid0_do_id = TRUE;

  is_do32 = fast_doacross ? TRUE : FALSE;
//  wn_prev_do = WN_prev( do_tree );
//  wn_next_do = WN_next( do_tree );
  //chunk_size = WN_Integer_Cast( chunk_size, is_do32 ? MTYPE_I4 : MTYPE_I8, 
  //		 WN_rtype( chunk_size )); 
  /* Generate do preamble code to calculate limit value */
  // Note, we need the start, limit, stride of the whole do as well
  // as the local ones. So, we have to do works in Extra_Do_Info again.
  // Maybe we should modify Extra_Do_Info instead.

  if ((WN_operator(WN_kid0(WN_kid0(WN_step(do_tree)))) == OPR_LDID) &&
      (WN_st(WN_kid0(WN_kid0(WN_step(do_tree)))) == do_id_st) &&
      (WN_offsetx(WN_kid0(WN_kid0(WN_step(do_tree)))) == do_id_ofst))
  {
    do_stride = WN_kid1(WN_kid0(WN_step(do_tree)));
#ifdef KEY
    if (WN_operator (WN_kid0 (WN_step (do_tree))) == OPR_SUB)
    { // the loop goes down, don't miss '-' in (- non-const-stride)
      OPCODE negop = OPCODE_make_op (OPR_NEG, WN_rtype (do_stride), MTYPE_V);
      do_stride = WN_CreateExp1 (negop, do_stride);
    }
#endif // KEY
  }
  else
    do_stride = WN_kid0(WN_kid0(WN_step(do_tree)));

  // first, we determine the operation type of do termination comparison.
  // In the form: do_index opr const
  // opr is supposed to be LE/GE, but since currently we don't standardize
  // the loop, we also should include LT/GT.
  if(( WN_kid0( WN_end( do_tree )) != NULL ) &&
     ( WN_operator( WN_kid0( WN_end( do_tree ))) == OPR_LDID ) &&
     ( WN_st( WN_kid0( WN_end( do_tree ))) == do_id_st ) &&
     ( WN_offsetx( WN_kid0( WN_end( do_tree ))) == do_id_ofst )) 
  {
    is_kid0_do_id = TRUE;
  }
  else
  {
    is_kid0_do_id = FALSE;
  }
  if(( WN_operator( WN_end( do_tree )) == OPR_LT ) ||
     ( WN_operator( WN_end( do_tree )) == OPR_LE ))
  {
    is_LE = TRUE;
  }
  else
  {
    is_LE = FALSE;
  }
  if( is_kid0_do_id == FALSE )
  {
    if( is_LE == TRUE )
      is_LE = FALSE;
    else
      is_LE = TRUE;
  }

  do_prefix = WN_CreateBlock( );
  do_suffix = WN_CreateBlock( );
   // These vars need to be type casted?
//  Create_Preg_or_Temp( do_index_type, "temp_limit", &limit_st, &limit_ofst );
  wn = WN_COPY_Tree ( limit_node );
  wn_tmp = WN_Stid ( do_index_type, limit_ofst, 
                  limit_st, ST_type(limit_st), wn );
  WN_INSERT_BlockLast( do_prefix, wn_tmp );
//  Create_Temp( do_index_type, "do_upper", &local_upper );
//  if ((WN_operator(WN_end(do_tree)) == OPR_LT) ||
//      (WN_operator(WN_end(do_tree)) == OPR_GT))
//    wn = WN_Add ( do_index_type,
//		  Gen_MP_Load ( local_start, 0 ),
//		  WN_Mpy ( do_index_type,
//			   Gen_MP_Load ( local_ntrip, 0 ),
//			   WN_COPY_Tree ( do_stride )));
//  else
//    wn = WN_Sub ( do_index_type,
//		  WN_Add ( do_index_type,
//			   Gen_MP_Load ( local_start, 0 ),
//			   WN_Mpy ( do_index_type,
//				    Gen_MP_Load ( local_ntrip, 0 ),
//				    WN_COPY_Tree ( do_stride ))),
//		  WN_COPY_Tree ( do_stride ));
    // Maybe using limit_node itself is also OK.
    // Note that, the type of local vars may be wrong.
//  wn = WN_COPY_Tree ( limit_node );
  wn_tmp = WN_Stid ( do_index_type, 0, 
    local_upper, ST_type(local_upper),
    Gen_MP_Load( limit_st, limit_ofst ));
  WN_linenum( wn_tmp ) = line_number;
  WN_INSERT_BlockLast ( do_prefix, wn_tmp );
  
//  Create_Temp( do_index_type, "do_lower", &local_lower );
  wn = WN_COPY_Tree( base_node );
  wn_tmp = WN_Stid( do_index_type, 0, 
                  local_lower, ST_type(local_lower), wn );
  WN_linenum( wn_tmp ) = line_number;
  WN_INSERT_BlockLast( do_prefix, wn_tmp );

//  Create_Temp( do_index_type, "do_stride", &local_stride );
  
//  Create_Temp( MTYPE_I4, "last_iter", &last_iter );

  wn = WN_Stid( MTYPE_I4, 0, last_iter, ST_type( last_iter ), 
                  WN_CreateIntconst( OPC_I4INTCONST, 0 )); 
  WN_linenum( wn ) = line_number;
  WN_INSERT_BlockLast( do_prefix, wn );

    
  do_schedule = WN_CreateIntconst( OPC_I4INTCONST, schedule );
  // What is the legal type of do_index_type?
  // Does type U4 compatible with I4? or a type casting must be made?
  // Currently, we take the strategy as in the lower_mp, while I don't
  // really understand it.
  // Maybe later we should ask Tian Xinmin
  // What if they are not matched?
  
  // OK, first we should fix type of do loop related vars.
  // Note, we define these vars in global scope, because they
  // May have later usage.
  // But, How to determine the type of Do lb, up, stride anyway?
  // How about If last_iter, local_upper|lower != NULL?
  // And how to determine the type of RTL calls?

  if (( schedule == OMP_SCHED_STATIC_EVEN ) ||
      ( schedule == OMP_SCHED_STATIC )) 
//      ( schedule == OMP_SCHED_ORDERED_STATIC_EVEN ) ||
//      ( schedule == OMP_SCHED_ORDERED_STATIC ))
  {
    // Prefix code for STATIC SCHEDULE
    do_init_call = WN_Create( OPC_VCALL, 7);
    WN_st_idx( do_init_call ) = GET_MPRUNTIME_ST( is_do32
               ? MPR_OMP_STATIC_INIT_4 : MPR_OMP_STATIC_INIT_8 );
    WN_Set_Call_Non_Data_Mod( do_init_call );
    WN_Set_Call_Non_Data_Ref( do_init_call );
#ifndef KEY // bug 4671
    WN_Set_Call_Non_Parm_Mod( do_init_call );
#endif
    WN_Set_Call_Non_Parm_Ref( do_init_call );
    WN_Set_Call_Parm_Mod( do_init_call );
    WN_Set_Call_Parm_Ref( do_init_call );
    WN_linenum( do_init_call ) = line_number;
    
    WN_kid( do_init_call, 0 ) = WN_CreateParm( MTYPE_I4, 
        Gen_MP_Load( local_gtid, 0 ),
        Be_Type_Tbl( MTYPE_I4 ), WN_PARM_BY_VALUE );
    WN_kid( do_init_call, 1 ) = WN_CreateParm( MTYPE_I4, do_schedule, 
        Be_Type_Tbl( MTYPE_I4 ), WN_PARM_BY_VALUE );
#ifndef KEY
    wn_tmp = WN_Lda( Pointer_type, 0, last_iter );
    WN_kid( do_init_call, 2 ) = WN_CreateParm( Pointer_type, wn_tmp,  
        WN_ty( wn_tmp ), WN_PARM_BY_REFERENCE );
#endif
    wn_tmp = WN_Lda( Pointer_type, 0, local_lower );
    WN_kid( do_init_call, 2 ) = WN_CreateParm( Pointer_type, wn_tmp,  
        WN_ty( wn_tmp ), WN_PARM_BY_REFERENCE );
    wn_tmp = WN_Lda( Pointer_type, 0, local_upper );
    WN_kid( do_init_call, 3 ) = WN_CreateParm( Pointer_type, wn_tmp,  
        WN_ty( wn_tmp ), WN_PARM_BY_REFERENCE );
    wn_tmp = WN_Lda( Pointer_type, 0, local_stride );
    WN_kid( do_init_call, 4 ) = WN_CreateParm( Pointer_type, wn_tmp,  
        WN_ty( wn_tmp ), WN_PARM_BY_REFERENCE );
       // What if the do_stride is not the same type as M_I4/ M_I8?
    wn = WN_COPY_Tree( do_stride );
    wn_tmp = WN_Integer_Cast( wn, is_do32 ? MTYPE_I4 : MTYPE_I8, 
	  	 WN_rtype( wn )); 
    WN_kid( do_init_call, 5 ) = WN_CreateParm( is_do32 ? MTYPE_I4 : MTYPE_I8, 
       wn_tmp,  Be_Type_Tbl( is_do32 ? MTYPE_I4 : MTYPE_I8 ),
       WN_PARM_BY_VALUE );
       // The type of chunk size also need to be fixed.
    wn_tmp = WN_Integer_Cast( chunk_size, is_do32 ? MTYPE_I4 : MTYPE_I8,
        WN_rtype( chunk_size ));
    WN_kid( do_init_call, 6 ) = WN_CreateParm( is_do32 ? MTYPE_I4 : MTYPE_I8,
       wn_tmp, Be_Type_Tbl( is_do32 ? MTYPE_I4 : MTYPE_I8 ),
       WN_PARM_BY_VALUE ); 
  }else 
  {
    // Prefix code for Dynamic code
    do_init_call = WN_Create( OPC_VCALL, 6);
    WN_st_idx( do_init_call ) = GET_MPRUNTIME_ST( is_do32
               ? MPR_OMP_SCHEDULER_INIT_4 : MPR_OMP_SCHEDULER_INIT_8 );
    WN_Set_Call_Non_Data_Mod( do_init_call );
    WN_Set_Call_Non_Data_Ref( do_init_call );
#ifndef KEY // bug 4671
    WN_Set_Call_Non_Parm_Mod( do_init_call );
#endif
    WN_Set_Call_Non_Parm_Ref( do_init_call );
    WN_Set_Call_Parm_Ref( do_init_call );
    WN_linenum( do_init_call ) = line_number;
    
    WN_kid( do_init_call, 0 ) = WN_CreateParm( MTYPE_I4, 
        Gen_MP_Load( local_gtid, 0 ),
        Be_Type_Tbl( MTYPE_I4 ), WN_PARM_BY_VALUE );
    WN_kid( do_init_call, 1 ) = WN_CreateParm( MTYPE_I4, do_schedule, 
        Be_Type_Tbl( MTYPE_I4 ), WN_PARM_BY_VALUE );
    wn_tmp = Gen_MP_Load( local_lower, 0 );
    WN_kid( do_init_call, 2 ) = WN_CreateParm( is_do32 ? MTYPE_I4 : MTYPE_I8,
       wn_tmp, Be_Type_Tbl( is_do32 ? MTYPE_I4 : MTYPE_I8 ), 
       WN_PARM_BY_VALUE );
    wn_tmp = Gen_MP_Load( local_upper, 0 );
    WN_kid( do_init_call, 3 ) = WN_CreateParm( is_do32 ? MTYPE_I4 : MTYPE_I8,
       wn_tmp, Be_Type_Tbl( is_do32 ? MTYPE_I4 : MTYPE_I8 ), 
       WN_PARM_BY_VALUE );
//    wn_tmp = WN_Lda( Pointer_type, 0, local_stride );
//    WN_kid( do_init_call, 5 ) = WN_CreateParm( Pointer_type, wn_tmp,  
//        WN_ty( wn_tmp ), WN_PARM_BY_REFERENCE );
       // What if the do_stride is not the same type as M_I4/ M_I8?
    wn_tmp = WN_COPY_Tree( do_stride );
    wn_tmp = WN_Integer_Cast( wn_tmp, is_do32 ? MTYPE_I4 : MTYPE_I8, 
	  	 WN_rtype( wn_tmp )); 
    WN_kid( do_init_call, 4 ) = WN_CreateParm( is_do32 ? MTYPE_I4 : MTYPE_I8,
       wn_tmp, Be_Type_Tbl( is_do32 ? MTYPE_I4 : MTYPE_I8 ),
       WN_PARM_BY_VALUE );
       // The type of chunk size also need to be fixed.
    wn_tmp = WN_Integer_Cast( chunk_size, is_do32 ? MTYPE_I4 : MTYPE_I8,
        WN_rtype( chunk_size ));
    WN_kid( do_init_call, 5 ) = WN_CreateParm( is_do32 ? MTYPE_I4 : MTYPE_I8,
       wn_tmp, Be_Type_Tbl( is_do32 ? MTYPE_I4 : MTYPE_I8 ),
       WN_PARM_BY_VALUE ); 
  }

  WN_INSERT_BlockLast( do_prefix, do_init_call );

  if ( schedule == OMP_SCHED_STATIC_EVEN ) 
//      ( schedule == OMP_SCHED_ORDERED_STATIC_EVEN )) 
  {
    // Rewrite DO body for STATIC EVEN SCHEDULE
    // If clause to fix upper returned by RTL
    // Must consider situation for stride < 0
      // adjust upper
    if( is_LE )
    {
       test_wn = WN_GT( do_index_type,
                        Gen_MP_Load( local_upper, 0 ),
                        Gen_MP_Load( limit_st, limit_ofst ));
    }
    else
    {
       test_wn = WN_LT( do_index_type,
                        Gen_MP_Load( local_upper, 0 ),
                        Gen_MP_Load( limit_st, limit_ofst ));
    }
    then_wn =  WN_CreateBlock( );
    wn_tmp = WN_Stid ( do_index_type, 0, 
      local_upper, ST_type(local_upper), 
		  Gen_MP_Load( limit_st, limit_ofst ));
    WN_INSERT_BlockLast( then_wn, wn_tmp );
    else_wn = WN_CreateBlock( );
    if_wn = WN_CreateIf( test_wn, then_wn, else_wn );
    WN_linenum( if_wn ) = line_number;
    WN_INSERT_BlockLast( do_prefix, if_wn );

    Rewrite_Do_New( do_tree, local_lower, local_upper, NULL );
    //return_wn = do_tree;
    // Maybe we should insert do_tree here.
    // Now, setup lastthread info. The lastiter is not set
    // by RTL in Static schedule case. I don't know why.
    // Not always needed.

    if ( lastlocal_nodes || nested_lastlocal_nodes )
    {
      if( is_LE )
      {
// Bug 4660
#ifdef KEY
        test_wn = WN_GT (do_index_type,
                         Gen_MP_Load( WN_st(WN_kid0(do_tree)), WN_offsetx(WN_kid0(do_tree)) ),
                         Gen_MP_Load( limit_st, limit_ofst ));

#else
        test_wn = WN_CAND( WN_LE( do_index_type,
                                  Gen_MP_Load( local_lower, 0 ),
                                  Gen_MP_Load( limit_st, limit_ofst )),
                           WN_GE( do_index_type,
                                  Gen_MP_Load( local_upper, 0 ),
                                  Gen_MP_Load( limit_st, limit_ofst )));
#endif
      }
      else
      {
// Bug 4660
#ifdef KEY
        test_wn = WN_LT (do_index_type,
                         Gen_MP_Load( WN_st(WN_kid0(do_tree)), WN_offsetx(WN_kid0(do_tree)) ),
                         Gen_MP_Load( limit_st, limit_ofst ));
#else
        test_wn = WN_CAND( WN_GE( do_index_type,
                                  Gen_MP_Load( local_lower, 0 ),
                                  Gen_MP_Load( limit_st, limit_ofst )),
                           WN_LE( do_index_type,
                                  Gen_MP_Load( local_upper, 0 ),
                                  Gen_MP_Load( limit_st, limit_ofst )));
#endif
      }
      then_wn =  WN_CreateBlock( );
      wn_tmp = WN_Stid ( MTYPE_I4 , 0, 
        last_iter, ST_type( last_iter ), 
		    WN_CreateIntconst( OPC_I4INTCONST, 1 ));
      WN_INSERT_BlockLast( then_wn, wn_tmp );
      else_wn = WN_CreateBlock( );
      if_wn = WN_CreateIf( test_wn, then_wn, else_wn );
      WN_linenum( if_wn ) = line_number;
      WN_INSERT_BlockLast( do_suffix, if_wn );
      // Flush/ barrier needed?
    }

// ompc_static_fini is obsolete in pathscale omp library
#ifndef KEY
    wn = WN_Create( OPC_VCALL, 1 );
    WN_st_idx( wn ) = GET_MPRUNTIME_ST( MPR_OMP_STATIC_FINI );
    WN_Set_Call_Non_Data_Mod( wn );
    WN_Set_Call_Non_Data_Ref( wn );
    WN_Set_Call_Non_Parm_Mod( wn );
    WN_Set_Call_Non_Parm_Ref( wn );
    WN_Set_Call_Parm_Ref( wn );
    WN_linenum( wn ) = line_number;

    WN_kid0( wn ) = WN_CreateParm( MTYPE_I4, 
      Gen_MP_Load( local_gtid, 0 ),
      Be_Type_Tbl( MTYPE_I4 ),
      WN_PARM_BY_VALUE );
    WN_INSERT_BlockLast( do_suffix, wn );
#endif
    // now, insert do_prefix and do_suffix

  }else if ( schedule == OMP_SCHED_STATIC ) 
//            ( schedule == OMP_SCHED_ORDERED_STATIC ))
  {
    // Rewrite DO body for STATIC SCHEDULE 
    // while test
    if( is_LE)
    {
      while_test = WN_LE( do_index_type,
                          Gen_MP_Load( local_lower, 0 ),
                          Gen_MP_Load( limit_st, limit_ofst ));
    }
    else
    {
      while_test = WN_GE( do_index_type,
                          Gen_MP_Load( local_lower, 0 ),
                          Gen_MP_Load( limit_st, limit_ofst ));
    }
    // while body
    while_body = WN_CreateBlock( );
      // adjust upper
    if( is_LE )
    {
       test_wn = WN_GT( do_index_type,
                        Gen_MP_Load( local_upper, 0 ),
                        Gen_MP_Load( limit_st, limit_ofst ));
    }
    else
    {
       test_wn = WN_LT( do_index_type,
                        Gen_MP_Load( local_upper, 0 ),
                        Gen_MP_Load( limit_st, limit_ofst ));
    }
    then_wn =  WN_CreateBlock( );
    wn_tmp = WN_Stid ( do_index_type, 0, 
      local_upper, ST_type(local_upper), 
		  Gen_MP_Load( limit_st, limit_ofst ));
    WN_INSERT_BlockLast( then_wn, wn_tmp );
    else_wn = WN_CreateBlock( );
    if_wn = WN_CreateIf( test_wn, then_wn, else_wn );
    WN_linenum( if_wn ) = line_number;
    WN_INSERT_BlockLast( while_body, if_wn );
      // insert do
    Rewrite_Do_New( do_tree, local_lower, local_upper, NULL);
    WN_INSERT_BlockLast( while_body, do_tree );
      // increase lower and upper.
    wn_tmp = WN_Add( do_index_type, 
                     Gen_MP_Load( local_lower, 0 ),
                     Gen_MP_Load( local_stride, 0 )); 
    wn = Gen_MP_Store( local_lower, 0, wn_tmp );
    WN_INSERT_BlockLast( while_body, wn );
    wn_tmp = WN_Add( do_index_type,
                     Gen_MP_Load( local_upper, 0 ),
                     Gen_MP_Load( local_stride, 0 ));
    wn = Gen_MP_Store( local_upper, 0, wn_tmp );
    WN_INSERT_BlockLast( while_body, wn );
    
    // replace original do with a new While wrapping do
    while_wn = WN_CreateWhileDo( while_test, while_body );
      // Does this cause new problems? if late operation must
      // check the type of do_tree, we must take a new method
      // to work around this.
    //WN_INSERT_BlockAfter( stmt_tree, wn_prev_do, while_wn );
    //WN_next( while_wn ) = wn_next_do;
    //if( wn_next_do != NULL )
    //{
    //  WN_prev( wn_next_do ) = while_wn;
    //}
    do_tree = while_wn;

    // last thread node.

    if ( lastlocal_nodes || nested_lastlocal_nodes )
    {
        // adjust lower/upper first.
      wn_tmp = WN_Sub( do_index_type, 
                       Gen_MP_Load( local_lower, 0 ),
                       Gen_MP_Load( local_stride, 0 )); 
      wn = Gen_MP_Store( local_lower, 0, wn_tmp );
      WN_INSERT_BlockLast( do_suffix, wn );
      wn_tmp = WN_Sub( do_index_type,
                       Gen_MP_Load( local_upper, 0 ),
                       Gen_MP_Load( local_stride, 0 ));
      wn = Gen_MP_Store( local_upper, 0, wn_tmp );
      WN_INSERT_BlockLast( do_suffix, wn );
      if( is_LE )
      {
        test_wn = WN_CAND( WN_LE( do_index_type,
                                  Gen_MP_Load( local_lower, 0 ),
                                  Gen_MP_Load( limit_st, limit_ofst )),
                           WN_GE( do_index_type,
                                  Gen_MP_Load( local_upper, 0 ),
                                  Gen_MP_Load( limit_st, limit_ofst )));
      }
      else
      {
        test_wn = WN_CAND( WN_GE( do_index_type,
                                  Gen_MP_Load( local_lower, 0 ),
                                  Gen_MP_Load( limit_st, limit_ofst )),
                           WN_LE( do_index_type,
                                  Gen_MP_Load( local_upper, 0 ),
                                  Gen_MP_Load( limit_st, limit_ofst )));
      }
      then_wn =  WN_CreateBlock( );
      wn_tmp = WN_Stid ( MTYPE_I4 , 0, 
        last_iter, ST_type( last_iter ), 
		    WN_CreateIntconst( OPC_I4INTCONST, 1 ));
      WN_INSERT_BlockLast( then_wn, wn_tmp );
      else_wn = WN_CreateBlock( );
      if_wn = WN_CreateIf( test_wn, then_wn, else_wn );
      WN_linenum( if_wn ) = line_number;
      WN_INSERT_BlockLast( do_suffix, if_wn );
      // Flush/ barrier needed?
    }

#ifndef KEY
    wn = WN_Create( OPC_VCALL, 1 );
    WN_st_idx( wn ) = GET_MPRUNTIME_ST( MPR_OMP_STATIC_FINI );
    WN_Set_Call_Non_Data_Mod( wn );
    WN_Set_Call_Non_Data_Ref( wn );
    WN_Set_Call_Non_Parm_Mod( wn );
    WN_Set_Call_Non_Parm_Ref( wn );
    WN_Set_Call_Parm_Ref( wn );
    WN_linenum( wn ) = line_number;

    WN_kid0( wn ) = WN_CreateParm( MTYPE_I4, 
      Gen_MP_Load( local_gtid, 0 ),
      Be_Type_Tbl( MTYPE_I4 ),
      WN_PARM_BY_VALUE );
    WN_INSERT_BlockLast( do_suffix, wn );
#endif
  }
  else
  {
    // Rewrite DO body for Other SCHEDULEs
    // while test
    // need to be rewritten.
    call_wn = WN_Create( OPC_I4CALL, 4);
    WN_st_idx( call_wn ) = GET_MPRUNTIME_ST( is_do32
               ? MPR_OMP_SCHEDULER_NEXT_4 : MPR_OMP_SCHEDULER_NEXT_8 );
    WN_Set_Call_Non_Data_Mod( call_wn );
    WN_Set_Call_Non_Data_Ref( call_wn );
#ifndef KEY // bug 4671
    WN_Set_Call_Non_Parm_Mod( call_wn );
#endif
    WN_Set_Call_Non_Parm_Ref( call_wn );
    WN_Set_Call_Parm_Mod( call_wn );
    WN_Set_Call_Parm_Ref( call_wn );
    WN_linenum( call_wn ) = line_number;
    
    WN_kid( call_wn, 0 ) = WN_CreateParm( MTYPE_I4, 
        Gen_MP_Load( local_gtid, 0),
        Be_Type_Tbl( MTYPE_I4 ), WN_PARM_BY_VALUE );
#ifndef KEY
    wn_tmp = WN_Lda( Pointer_type, 0, last_iter );
    WN_kid( call_wn, 1 ) = WN_CreateParm( Pointer_type, wn_tmp,  
        WN_ty( wn_tmp ), WN_PARM_BY_REFERENCE );
#endif
    wn_tmp = WN_Lda( Pointer_type, 0, local_lower );
    WN_kid( call_wn, 1 ) = WN_CreateParm( Pointer_type, wn_tmp,  
        WN_ty( wn_tmp ), WN_PARM_BY_REFERENCE );
    wn_tmp = WN_Lda( Pointer_type, 0, local_upper );
    WN_kid( call_wn, 2 ) = WN_CreateParm( Pointer_type, wn_tmp,  
        WN_ty( wn_tmp ), WN_PARM_BY_REFERENCE );
    wn_tmp = WN_Lda( Pointer_type, 0, local_stride );
    WN_kid( call_wn, 3 ) = WN_CreateParm( Pointer_type, wn_tmp,  
        WN_ty( wn_tmp ), WN_PARM_BY_REFERENCE );
       // What if the do_stride is not the same type as M_I4/ M_I8?
    WN_INSERT_BlockLast( do_prefix , call_wn );

    Create_Preg_or_Temp ( MTYPE_I4, "mpni_status", &return_st, &return_ofst );
    GET_RETURN_PREGS(rreg1, rreg2, MTYPE_I4);
    wn = WN_Stid ( MTYPE_I4, return_ofst, return_st, ST_type(return_st),
		    WN_LdidPreg ( MTYPE_I4, rreg1 ));
    WN_linenum(wn) = line_number;
    WN_INSERT_BlockLast( do_prefix , wn );

    while_test = Gen_MP_Load( return_st, return_ofst );
    // while body
    while_body = WN_CreateBlock( );
      // adjust upper
    if( is_LE )
    {
       test_wn = WN_GT( do_index_type,
                        Gen_MP_Load( local_upper, 0 ),
                        Gen_MP_Load( limit_st, limit_ofst ));
    }
    else
    {
       test_wn = WN_LT( do_index_type,
                        Gen_MP_Load( local_upper, 0 ),
                        Gen_MP_Load( limit_st, limit_ofst ));
    }
    then_wn =  WN_CreateBlock( );
    wn_tmp = WN_Stid ( do_index_type, 0, 
      local_upper, ST_type(local_upper), 
		  Gen_MP_Load( limit_st, limit_ofst ));
    WN_INSERT_BlockLast( then_wn, wn_tmp );
    else_wn = WN_CreateBlock( );
    if_wn = WN_CreateIf( test_wn, then_wn, else_wn );
    WN_linenum( if_wn ) = line_number;
    WN_INSERT_BlockLast( while_body, if_wn );
      // insert do
    Rewrite_Do_New( do_tree, local_lower, local_upper, local_stride);
    WN_INSERT_BlockLast( while_body, do_tree );
    WN_INSERT_BlockLast( while_body, WN_COPY_Tree( call_wn ));
    WN_INSERT_BlockLast( while_body, WN_COPY_Tree( wn )); 
      // increase lower and upper.
     
    // replace original do with a new While wrapping do
    while_wn = WN_CreateWhileDo( while_test, while_body );
      // Does this cause new problems? if late operation must
      // check the type of do_tree, we must take a new method
      // to work around this.
#ifdef KEY
    WN * old_do_tree = do_tree;
    {
      // Initialize the do-loop index variable outside the while loop, so
      // that a thread not doing any iteration does not land up with a 
      // wrong (uninitialized) value of the index variable. This is 
      // especially important for lastprivate handling which does a compare
      // to find out the last iteration.
      //
      WN * ldid_lower = WN_Ldid (TY_mtype (Ty_Table[ST_type (local_lower)]),
                                 0,
				 local_lower,
				 ST_type (local_lower));
      ST * do_index = WN_st (WN_index (old_do_tree));
      INT do_offset = WN_offset (WN_index (old_do_tree));
      WN * init_do_index = WN_Stid (WN_rtype (ldid_lower),
                                    do_offset,
				    do_index,
				    ST_type (do_index),
				    ldid_lower);
      WN_linenum (init_do_index) = line_number;
      // Insert the initialization before the schedule_next function call
      WN_INSERT_BlockBefore (do_prefix, call_wn, init_do_index);
    }
#endif

    do_tree = while_wn; 

    // last thread node.
    if( lastlocal_nodes || nested_lastlocal_nodes)
    {
#ifdef KEY
      // generate an if-stmt to check if this is the last iteration, and then
      // set last_iter, which will be checked to set lastprivate variables
      if( is_LE )
      {
        test_wn = WN_GT (do_index_type,
                         Gen_MP_Load( WN_st(WN_kid0(old_do_tree)), WN_offsetx(WN_kid0(old_do_tree)) ),
                         Gen_MP_Load( limit_st, limit_ofst ));

      }
      else
      {
        test_wn = WN_LT (do_index_type,
                         Gen_MP_Load( WN_st(WN_kid0(old_do_tree)), WN_offsetx(WN_kid0(old_do_tree)) ),
                         Gen_MP_Load( limit_st, limit_ofst ));
      }
      then_wn =  WN_CreateBlock( );
      wn_tmp = WN_Stid ( MTYPE_I4 , 0, 
        last_iter, ST_type( last_iter ), 
		    WN_CreateIntconst( OPC_I4INTCONST, 1 ));
      WN_INSERT_BlockLast( then_wn, wn_tmp );
      else_wn = WN_CreateBlock( );
      if_wn = WN_CreateIf( test_wn, then_wn, else_wn );
      WN_linenum( if_wn ) = line_number;
      WN_INSERT_BlockLast( do_suffix, if_wn );
#endif
      // Indeed, the RTL set the last_iter, so nothing to do.
        // adjust lower/upper first.
      // Flush/ barrier needed?
    }
}

  WN_INSERT_BlockLast( return_wn, do_prefix );
  WN_INSERT_BlockLast( return_wn, do_tree );
  WN_INSERT_BlockLast( return_wn, do_suffix );

#ifndef KEY
// Move the call to ompc_barrier down. We are not yet done handling the
// do-loop, like handling of lastprivate variables.
// So move the call down, and handle it in both the callsites of Transform_Do
  if( nested_nowait_node == NULL )
  {
//    WN_Delete( nested_nowait_node );
//    nested_nowait_node = NULL;
    WN_INSERT_BlockLast( return_wn, Gen_Barrier(local_gtid));
    // Flush should be inserted here.
    // csc.
  }
  else
  {
    WN_Delete( nested_nowait_node );
    nested_nowait_node = NULL;
  }
#endif // !KEY

  return return_wn;
} // Transform_Do()

/*  Rewrite do statement.  */
//CAN BE DELETED.
//TODO: delete and clean up this routine.
static void 
Rewrite_Do ( WN * do_tree )
{
  WN        *wn;
  WN        *do_idname  = WN_index(do_tree);
  ST        *do_id_st   = WN_st(do_idname);
  WN_OFFSET  do_id_ofst = WN_offsetx(do_idname);
  WN        *do_stride;
  WN        *loop_info;
  // The following variables move to global scope
//  ST        *limit_st;
//  WN_OFFSET  limit_ofst;

  /* Generate do preamble code to calculate limit value */

  if ((WN_operator(WN_kid0(WN_kid0(WN_step(do_tree)))) == OPR_LDID) &&
      (WN_st(WN_kid0(WN_kid0(WN_step(do_tree)))) == do_id_st) &&
      (WN_offsetx(WN_kid0(WN_kid0(WN_step(do_tree)))) == do_id_ofst))
    do_stride = WN_kid1(WN_kid0(WN_step(do_tree)));
  else
    do_stride = WN_kid0(WN_kid0(WN_step(do_tree)));

  Create_Preg_or_Temp ( do_index_type, "do_limit", &limit_st, &limit_ofst );
  if ((WN_operator(WN_end(do_tree)) == OPR_LT) ||
      (WN_operator(WN_end(do_tree)) == OPR_GT))
    wn = WN_Add ( do_index_type,
		  Gen_MP_Load ( local_start, 0 ),
		  WN_Mpy ( do_index_type,
			   Gen_MP_Load ( local_ntrip, 0 ),
			   WN_COPY_Tree ( do_stride )));
  else
    wn = WN_Sub ( do_index_type,
		  WN_Add ( do_index_type,
			   Gen_MP_Load ( local_start, 0 ),
			   WN_Mpy ( do_index_type,
				    Gen_MP_Load ( local_ntrip, 0 ),
				    WN_COPY_Tree ( do_stride ))),
		  WN_COPY_Tree ( do_stride ));
  do_prefix = WN_Stid ( do_index_type, limit_ofst, limit_st, ST_type(limit_st),
			wn );
  WN_linenum(do_prefix) = line_number;

  /* Fix up the do loop controls */

/* Temporary fix for PV 381272.  Wopt cannot handle double convert of I8 -> I1/2
   and so we coerce I8 to I4 first.  Note that the offset (4) is correct for
   little endian systems only. */
  if ((ST_sclass(local_start) == SCLASS_AUTO) &&
      (ST_btype(local_start) == MTYPE_I8) &&
      ((WN_desc(WN_start(do_tree)) == MTYPE_I1) ||
       (WN_desc(WN_start(do_tree)) == MTYPE_I2))) { 
    WN_kid0(WN_start(do_tree)) = WN_RLdid ( Promote_Type(MTYPE_I4), MTYPE_I4, 4,
					    local_start, MTYPE_To_TY(MTYPE_I4));
  } else { 
/* End temporary fix for PV 381272. */
    WN* wn_local_start = Gen_MP_Load(local_start, 0);
    WN* wn_new_local_start = WN_Integer_Cast(wn_local_start, Promote_Type(WN_desc(WN_start(do_tree))), WN_rtype(wn_local_start));
    WN_kid0(WN_start(do_tree)) = wn_new_local_start; 
  } 

  WN* wn_ldid = WN_Ldid(do_index_type, limit_ofst, limit_st, 
    ST_type(limit_st));
  WN* wn_cvt_ldid = wn_ldid; 
  if (WN_rtype(wn_cvt_ldid) != WN_desc(WN_end(do_tree)))
     wn_cvt_ldid = WN_Integer_Cast(wn_cvt_ldid, WN_desc(WN_end(do_tree)),
       WN_rtype(wn_cvt_ldid));
  if (WN_kid0(WN_end(do_tree)) == NULL) { 
    WN_kid0(WN_end(do_tree)) = wn_cvt_ldid; 
  } else { 
    WN_kid1(WN_end(do_tree)) = wn_cvt_ldid;
  } 

  /* Fix up the optional LOOP_INFO node */

  loop_info = WN_do_loop_info(do_tree);
#ifndef KEY
  if (loop_info)
#else
  // Bug 4809 - we will preserve the loop trip count information if the 
  // original trip count is an integer constant. For MP DO loops, the loop
  // bounds are determined at run-time. However, if the loop bounds for the 
  // original loop was a compile time constant, it helps to transfer that 
  // information to the later compilation phases (the trip count information
  // is an estimate). In the case of STREAM -mp, this fix specifically lets the
  // CG loop optimizer to convert stores to non-temporal stores because the
  // loop trip count is now made available.
  if (loop_info && (!WN_loop_trip(loop_info) || 
		    !WN_operator_is(WN_loop_trip(loop_info), OPR_INTCONST)))
#endif
  if (loop_info) {
    WN_loop_trip_est(loop_info) = 0;
    WN_loop_depth(loop_info) = 1;
    WN_Reset_Loop_Nz_Trip ( loop_info );
    if (WN_loop_trip(loop_info)) {
      WN_DELETE_Tree ( WN_loop_trip(loop_info) );
      WN_set_loop_trip ( loop_info, Gen_MP_Load ( local_ntrip, 0 ));
    }
  }
} // Rewrite_Do()

/*
Scale FB for do_loop to reflect worksharing of its iterations among all
the threads (e.g. multiply the number of iterations by
fraction_per_thread).
*/

static void 
Scale_FB_Parallel_Do(WN *do_loop, float fraction_per_thread)
{
  Is_True(do_loop && WN_operator(do_loop) == OPR_DO_LOOP,
          ("bad do_loop"));
  Is_True(fraction_per_thread <= 1.0, ("bogus fraction_per_thread"));
  FB_Info_Loop loop_fil = parallel_pu_fb->Query_loop(do_loop);

    // Assume we will reach the parallel loop the same number of times
    // as in the serial case, so leave freq_zero, freq_positive, and
    // freq_out alone.
    // In reality, depending on the loop's schedule these frequencies could
    // be different from the serial case, but precise estimates are too
    // hard to compute for all the different cases.
  
  if (fraction_per_thread <= 0.5 &&
      (fraction_per_thread * loop_fil.freq_iterate.Value() >=
       loop_fil.freq_positive.Value())) {
      // There is at least one iteration per thread on average, so we
      // can scale the loop body's FB by fraction_per_thread. This is
      // the normal case.
    loop_fil.freq_iterate *= fraction_per_thread;
    loop_fil.freq_back = loop_fil.freq_iterate - loop_fil.freq_positive;

  } else {
      // There's less than one iteration per thread on average. This is
      // an uncommon case, and it's hard to tell how to update the FB in
      // the loop body (in fact frequencies are different for different
      // threads!), so just give up and leave it alone.
    return;
  }

  parallel_pu_fb->Annot_loop(do_loop, loop_fil);

    // scale loop body FB by fraction_per_thread
  parallel_pu_fb->FB_scale(WN_do_body(do_loop),
                           FB_FREQ(fraction_per_thread, FALSE));
} // Scale_FB_Parallel_Do()


static WN *Process_PDO( WN * pdo_tree );
/*  Transform the contents of a parallel region.  */

static void 
Transform_Parallel_Block ( WN * tree )
{
  INT32 i;
  WN *wn;
  WN *wn2;
  WN *wn3;
  WN *wn4;
  WN *cur_node;
  WN *prev_node;
  WN *next_node;
  WN *sp_block;
  ST *lock_st;

  INT32 num_criticals;
  BOOL is_omp, is_region;
  WN_PRAGMA_ID cur_id, end_id;
  INT32 gate_construct_num;

  for (cur_node = WN_first(tree); cur_node; cur_node = next_node) {

    prev_node = WN_prev(cur_node);
    next_node = WN_next(cur_node);

    if (((WN_opcode(cur_node) == OPC_PRAGMA) ||
         (WN_opcode(cur_node) == OPC_XPRAGMA)) &&
        (WN_pragmas[WN_pragma(cur_node)].users & PUSER_MP)) {

      switch (cur_id = (WN_PRAGMA_ID) WN_pragma(cur_node)) {

	case WN_PRAGMA_BARRIER:

    //      wn = Gen_MP_Barrier (WN_pragma_omp(cur_node));
    wn = Gen_Barrier(local_gtid);
	  if (prev_node)
	    WN_next(prev_node) = wn;
	  else
	    WN_first(tree) = wn;
	  WN_prev(wn) = prev_node;
	  WN_next(wn) = next_node;
	  if (next_node)
	    WN_prev(next_node) = wn;
	  else
	    WN_last(tree) = wn;

	  WN_Delete ( cur_node );
	  break;

/* What is GATE ?  lg */
	case WN_PRAGMA_ENTER_GATE:

          gate_construct_num = ++num_constructs;
	  // modified by csc. will this work?
	  // or we can safely remove it.
	  wn = WN_CreateBlock();
//	  wn = Gen_MP_Enter_Gate (gate_construct_num);

	  if (prev_node)
	    WN_next(prev_node) = wn;
	  else
	    WN_first(tree) = wn;
	  WN_prev(wn) = prev_node;
	  WN_next(wn) = next_node;
	  if (next_node)
	    WN_prev(next_node) = wn;
	  else
	    WN_last(tree) = wn;

	  WN_Delete ( cur_node );
	  break;

	case WN_PRAGMA_EXIT_GATE:

	  // modified by csc. will this work?
	  // or we can safely remove it.
	  wn = WN_CreateBlock( );
//	  wn = Gen_MP_Exit_Gate (gate_construct_num);

	  if (prev_node)
	    WN_next(prev_node) = wn;
	  else
	    WN_first(tree) = wn;
	  WN_prev(wn) = prev_node;
	  WN_next(wn) = next_node;
	  if (next_node)
	    WN_prev(next_node) = wn;
	  else
	    WN_last(tree) = wn;

	  WN_Delete ( cur_node );
	  break;

	case WN_PRAGMA_CRITICAL_SECTION_BEGIN:

          if (WN_opcode(cur_node) == OPC_PRAGMA &&
              WN_pragma_omp(cur_node) &&
              WN_st(cur_node)) {
            lock_st = Get_NameLock_ST(WN_st(cur_node));
          }
	  else if ((WN_opcode(cur_node) == OPC_XPRAGMA) &&
                   (WN_operator(WN_kid0(cur_node)) == OPR_LDA))
	    lock_st = WN_st(WN_kid0(cur_node));
	  else if ((WN_opcode(cur_node) == OPC_PRAGMA) && WN_st(cur_node))
	    lock_st = WN_st(cur_node);
	  else{
	    lock_st = NULL;
	    Create_Unnamed_Critical_Lock( );
	  }
	  if (lock_st) {
	    Linenum_Pusher p(WN_Get_Linenum(cur_node));
	    wn = Gen_Critical (local_gtid, lock_st);
	  } else {
	    Linenum_Pusher p(WN_Get_Linenum(cur_node));
            wn = Gen_Critical (local_gtid, unnamed_lock_st);
	  }
	  if (prev_node)
	    WN_next(prev_node) = wn;
	  else
	    WN_first(tree) = wn;
	  WN_prev(wn) = prev_node;
	  WN_next(wn) = next_node;
	  if (next_node)
	    WN_prev(next_node) = wn;
	  else
	    WN_last(tree) = wn;

	  WN_DELETE_Tree ( cur_node );

	  num_criticals = 1;
	  cur_node = next_node;
	  while (cur_node) {
	    if ((WN_opcode(cur_node) == OPC_PRAGMA) ||
		(WN_opcode(cur_node) == OPC_XPRAGMA))
	      if (WN_pragma(cur_node) == WN_PRAGMA_CRITICAL_SECTION_BEGIN)
		++num_criticals;
	      else if (WN_pragma(cur_node) == WN_PRAGMA_CRITICAL_SECTION_END)
		if ((--num_criticals) == 0)
		  break;
	    cur_node = WN_next(cur_node);
	  }
	  if (cur_node == NULL)
	    Fail_FmtAssertion (
		      "missing pragma (CRITICAL_SECTION_END) in MP processing");
	  if (lock_st) {
	    Linenum_Pusher p(WN_Get_Linenum(cur_node));
	    wn = Gen_End_Critical(local_gtid, lock_st);
	  } else {
	    Linenum_Pusher p(WN_Get_Linenum(cur_node));
      wn = Gen_End_Critical(local_gtid, unnamed_lock_st);
	  }
	  WN_next(WN_prev(cur_node)) = wn;
	  WN_prev(wn) = WN_prev(cur_node);
	  WN_next(wn) = WN_next(cur_node);
	  if (WN_next(cur_node))
	    WN_prev(WN_next(cur_node)) = wn;
	  else
	    WN_last(tree) = wn;

	  WN_Delete ( cur_node );
	  break;

          /* an MP "independent" construct in C doesn't appear as a region */
          /* as a hack, continue to allow SINGLE BEGIN/END pairs, until
             LNO generates the right thing--DRK */
    case WN_PRAGMA_INDEPENDENT_BEGIN:
    case WN_PRAGMA_SINGLE_PROCESS_BEGIN:

          end_id = (cur_id == WN_PRAGMA_INDEPENDENT_BEGIN) ?
              WN_PRAGMA_INDEPENDENT_END : WN_PRAGMA_SINGLE_PROCESS_END;

	  ++num_constructs;
          is_omp = WN_pragma_omp(cur_node);

            /* create block from nodes inside BEGIN/END; delete BEGIN/END */
          wn = cur_node;  /* the BEGIN node */
          cur_node = wn2 = WN_next(cur_node); /* first node in block */
	  while (cur_node &&
		 ((WN_opcode(cur_node) != OPC_PRAGMA) ||
		  (WN_pragma(cur_node) != end_id)))
	    cur_node = WN_next(cur_node);
	  if (!cur_node)
	    Fail_FmtAssertion(
                "missing pragma (INDEPENDENT_END) in MP processing");
          wn3 = WN_prev(cur_node);  /* last node in block */
          wn4 = cur_node; /* the END node */
	  sp_block = WN_CreateBlock();
	  if (wn2 != wn4) { /* block is non-empty */
	    WN_EXTRACT_ItemsFromBlock(tree, wn2, wn3);
	    WN_first(sp_block) = wn2;
	    WN_last(sp_block) = wn3;
	  }
	  WN_DELETE_FromBlock(tree, wn);
	  WN_DELETE_FromBlock(tree, wn4);

    {
      WN *mp_sp_block = Gen_MP_SingleProcess_Block(sp_block, FALSE, 
#ifdef KEY
                                                         NULL,
#endif 
	                                                 is_omp, FALSE);
              // fix PV 589326: keep lowering MP constructs within and
              // after the INDEPENDENT/SINGLE construct
	    next_node = WN_first(mp_sp_block);
	    WN_INSERT_BlockAfter(tree, prev_node, mp_sp_block);
	  }
	  break;

    case WN_PRAGMA_ORDERED_BEGIN:
        WN_INSERT_BlockAfter( tree, prev_node, Gen_Ordered(local_gtid));
        WN_DELETE_FromBlock (tree, cur_node);
        break;
      
    case WN_PRAGMA_ORDERED_END:
        WN_INSERT_BlockAfter( tree, prev_node, Gen_End_Ordered(local_gtid));
        //WN_INSERT_BlockAfter (tree, prev_node, Gen_OMP_End_Ordered());
        WN_DELETE_FromBlock (tree, cur_node);
        break;

	default:
	  Fail_FmtAssertion (
	      "out of context pragma (%s) in MP {parallel region} processing",
	      WN_pragmas[WN_pragma(cur_node)].name);

      }

    } else if ((is_region = (WN_opcode(cur_node) == OPC_REGION &&
                             WN_first(WN_region_pragmas(cur_node)) &&
                             WN_opcode(WN_first(
			        WN_region_pragmas(cur_node))) ==
				OPC_PRAGMA) ) &&
	       WN_pragma(WN_first(WN_region_pragmas(cur_node))) ==
						WN_PRAGMA_PDO_BEGIN) {

      BOOL save_comp_gen_construct = comp_gen_construct;
      comp_gen_construct = ( WN_pragma_compiler_generated(
                                WN_first(WN_region_pragmas(cur_node))) != 0 );
      MP_process_type save_mpt = mpt;
      mpt = MPP_PDO;

      ++num_constructs;
      wn = Process_PDO( cur_node );
      if (non_pod_finalization_nodes)
        Fail_FmtAssertion("out of place non-POD finalization code");

      if (WN_first(wn)) {
	       if (prev_node)
	          WN_next(prev_node) = WN_first(wn);
	       else
	          WN_first(tree) = WN_first(wn);
	       WN_prev(WN_first(wn)) = prev_node;
	       WN_next(WN_last(wn)) = next_node;
	       if (next_node)
	          WN_prev(next_node) = WN_last(wn);
	       else
	          WN_last(tree) = WN_last(wn);
	       next_node = WN_first(wn);
      } else {
	       if (prev_node)
	          WN_next(prev_node) = next_node;
	       else
	          WN_first(tree) = next_node;
	       if (next_node)
	          WN_prev(next_node) = prev_node;
	       else
	          WN_last(tree) = prev_node;
      }

      WN_Delete ( WN_region_pragmas(cur_node) );
      WN_DELETE_Tree ( WN_region_exits(cur_node) );
      RID_Delete ( Current_Map_Tab, cur_node );
      WN_Delete ( cur_node );
      WN_Delete ( wn );

      comp_gen_construct = save_comp_gen_construct; // restore old value
      mpt = save_mpt;

    } else if (is_region &&
               WN_first(WN_region_pragmas(cur_node)) &&
	       WN_pragma(WN_first(WN_region_pragmas(cur_node))) ==
                  WN_PRAGMA_SINGLE_PROCESS_BEGIN) {

      BOOL save_comp_gen_construct = comp_gen_construct;
      comp_gen_construct = ( WN_pragma_compiler_generated(
                                WN_first(WN_region_pragmas(cur_node))) != 0 );
      MP_process_type save_mpt = mpt;
      mpt = MPP_SINGLE;

      ++num_constructs;
      wn = Gen_MP_SingleProcess_Region(cur_node);
      WN_EXTRACT_FromBlock(tree, cur_node);
      WN_INSERT_BlockAfter(tree, prev_node, wn);
      WN_DELETE_Tree(WN_region_pragmas(cur_node));
      WN_DELETE_Tree(WN_region_exits(cur_node));
      RID_Delete(Current_Map_Tab, cur_node);
      WN_Delete(cur_node);

      comp_gen_construct = save_comp_gen_construct; // restore old value
      mpt = save_mpt;

#ifdef KEY /* Bug 4828 */
    } else if (is_region &&
               WN_first(WN_region_pragmas(cur_node)) &&
	       WN_pragma(WN_first(WN_region_pragmas(cur_node))) ==
                  WN_PRAGMA_PWORKSHARE_BEGIN) {

      BOOL save_comp_gen_construct = comp_gen_construct;
      comp_gen_construct = ( WN_pragma_compiler_generated(
                                WN_first(WN_region_pragmas(cur_node))) != 0 );
      MP_process_type save_mpt = mpt;
      mpt = MPP_WORKSHARE;

      ++num_constructs;
      wn = Gen_MP_Workshare_Region(cur_node);
      Transform_Parallel_Block (wn);
      WN_EXTRACT_FromBlock(tree, cur_node);
      WN_INSERT_BlockAfter(tree, prev_node, wn);
      WN_DELETE_Tree(WN_region_pragmas(cur_node));
      WN_DELETE_Tree(WN_region_exits(cur_node));
      RID_Delete(Current_Map_Tab, cur_node);
      WN_Delete(cur_node);

      comp_gen_construct = save_comp_gen_construct; // restore old value
      mpt = save_mpt;
#endif

    } else if (is_region &&
               WN_first(WN_region_pragmas(cur_node)) &&
	             ( WN_pragma(WN_first(WN_region_pragmas(cur_node))) ==
               WN_PRAGMA_MASTER_BEGIN)) {

      BOOL save_comp_gen_construct = comp_gen_construct;
      comp_gen_construct = ( WN_pragma_compiler_generated(
                                WN_first(WN_region_pragmas(cur_node))) != 0 );
      MP_process_type save_mpt = mpt;
      mpt = MPP_MASTER;

      ++num_constructs;
      wn = Lower_Master(cur_node);
      WN_EXTRACT_FromBlock(tree, cur_node);
      WN_INSERT_BlockAfter(tree, prev_node, wn);
      WN_DELETE_Tree(WN_region_pragmas(cur_node));
      WN_DELETE_Tree(WN_region_exits(cur_node));
      RID_Delete(Current_Map_Tab, cur_node);
      WN_Delete(cur_node);

      comp_gen_construct = save_comp_gen_construct; // restore old value
      mpt = save_mpt;

    } else {
      for (i = 0; i < WN_kid_count(cur_node); i++)
        if (WN_kid(cur_node, i) &&
           (WN_opcode(WN_kid(cur_node, i)) == OPC_BLOCK))
              Transform_Parallel_Block ( WN_kid(cur_node, i) );

    }
  }
} // Transform_Parallel_Block()

/*
Translate MP schedule types to KMPC ones.
*/

SCHEDULE_TYPE
Translate_Schedule_Type( int mp_SchedType, 
 		BOOL ordered)
{
   SCHEDULE_TYPE kmpc_schedule_type = OMP_SCHED_UNKNOWN;
   
   switch( mp_SchedType )
   {
     case WN_PRAGMA_SCHEDTYPE_UNKNOWN:
	// kmpc_schedule_type = OMP_SCHED_UNKNOWN;
     	break;
     case WN_PRAGMA_SCHEDTYPE_RUNTIME:
        if( ordered )
          kmpc_schedule_type = OMP_SCHED_ORDERED_RUNTIME;
        else
	  kmpc_schedule_type = OMP_SCHED_RUNTIME;
        break;
     case WN_PRAGMA_SCHEDTYPE_SIMPLE:
        if( ordered )
          kmpc_schedule_type = OMP_SCHED_ORDERED_STATIC_EVEN;
        else
	  kmpc_schedule_type = OMP_SCHED_STATIC_EVEN;
        break;
     case WN_PRAGMA_SCHEDTYPE_INTERLEAVE:
        if( ordered )
	  kmpc_schedule_type = OMP_SCHED_ORDERED_STATIC;
        else
          kmpc_schedule_type = OMP_SCHED_STATIC;
        break;
     case WN_PRAGMA_SCHEDTYPE_DYNAMIC:
        if( ordered )
          kmpc_schedule_type = OMP_SCHED_ORDERED_DYNAMIC;
        else
	  kmpc_schedule_type = OMP_SCHED_DYNAMIC;
   	break;
     case WN_PRAGMA_SCHEDTYPE_GSS:
        if( ordered )
          kmpc_schedule_type = OMP_SCHED_ORDERED_GUIDED;
        else
	  kmpc_schedule_type = OMP_SCHED_GUIDED;
  	break;
     case WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED: 
         // What's this? csc
         // Translated into default schedule
        if( ordered )
	  kmpc_schedule_type = OMP_SCHED_ORDERED_STATIC_EVEN;
        else
          kmpc_schedule_type = OMP_SCHED_STATIC_EVEN;
        break;
     case MAX_PRAGMA_SCHEDTYPE:
	// kmpc_schedule_type = OMP_SCHED_UNKNOWN;
        break;
     default:
	// kmpc_schedule_type = OMP_SCHED_UNKNOWN;
        break;
   }

   return kmpc_schedule_type;
}

#ifdef KEY
// Returns TRUE if the immediate parent of "tree" is a parallel region, and
// "tree" is the last statement in this region.
static BOOL
Lastpdo_in_Parallel_Region (WN * tree)
{
  Is_True (WN_operator (tree) == OPR_REGION &&
           WN_region_kind (tree) == REGION_KIND_MP, ("MP region expected"));

  RID * rid = REGION_get_rid (tree);
  if (!rid) return FALSE;
  
  RID * parent_rid = RID_parent (rid);
  Is_True (parent_rid, ("Parent RID cannot be NULL"));

  WN * parent = RID_rwn (parent_rid);
  if (WN_region_kind (parent) == REGION_KIND_MP)
  {
    // Due to bug 5413, the parent region will have a pragma block, but
    // the pragma nodes are all garbage (OPERATOR_UNKNOWN). So we cannot
    // check the pragma nodes to verify if it is a parallel region.
    //
    // So the assumption is any MP region enclosing a DO region is the 
    // parallel region the DO region binds to.
    //
    // Check if the DO loop is the last statement in the parallel region
    WN * body = WN_region_body (parent);
    // It turns out the parent region of a DO loop need not always be
    // a parallel region, it could be a workshare region. So the above
    // assumption is wrong. The implementation in this file is so unclean
    // that in such a case "body" happens to be NULL, which means "tree"
    // is contained in the body ("body") of parent, but "body" is NULL.
    Is_True (WN_last (body), ("REGION body cannot be null"));

    if (WN_last (body) == tree) 
      return TRUE;
  }
  return FALSE;
}
#endif // KEY

/*
* Process pdo.
* Modified by csc.
* */

static WN * 
Process_PDO ( WN * tree )
{
  INT32      i;
  INT32      vsize;
  WN        *wn;
  WN        *wn1;
  WN        *wn2;
  WN        *cur_node;
  WN        *first_node;
  WN        *prev_node;
  WN        *next_node;
  WN        *pdo_node;
  WN        *mpsched_wn;
  WN        *chunk_wn = NULL;
  WN        *body_block;
  WN        *while_block;
  WN        *reduction_init_block;
  WN        *reduction_store_block;
  ST        *return_st;
  WN_OFFSET  return_ofst;
  PREG_NUM   rreg1, rreg2;
  BOOL       while_seen = FALSE;
  BOOL       is_omp;
  BOOL       do_dealloca = FALSE;
  WN         *nested_alloca_block = NULL, *nested_firstprivate_block = NULL;
  WN         *sp_save_stid;
  Alloca_Var_List *avlist;
  SCHEDULE_TYPE kmpc_schedule = OMP_SCHED_STATIC_EVEN; 
     // note that, chunk_size maybe not available as int.
     // So we present it as an WN node.
  // INT32 chunk_size = 0;
  //WN *chunk_wn = NULL;

  const BOOL orphaned = (mpt == MPP_ORPHANED_PDO);

  Is_True(mpt == MPP_PDO || mpt == MPP_ORPHANED_PDO,
          ("not inside a PDO loop"));

  /* Initialization. */

  nested_local_count      = 0;
  nested_reduction_count  = 0;
  nested_affinity_nodes   = NULL;
  nested_affinity_d_nodes = NULL;
  nested_affinity_t_nodes = NULL;
  nested_chunk_node       = NULL;
  nested_lastlocal_nodes  = NULL;
  nested_lastthread_node  = NULL;
  nested_local_nodes      = NULL;
  nested_firstprivate_nodes = NULL;
  nested_mpsched_node     = NULL;
  nested_nowait_node      = NULL;
  nested_ordered_node     = NULL;
  nested_reduction_nodes  = NULL;
  nested_shared_nodes     = NULL;
  nested_do_order_lb      = NULL;
  nested_do_order_stride  = NULL;

  if (orphaned) {
      // set up some globals
    // Why these information should be preserved.
    // When to restore?
    psymtab = CURRENT_SYMTAB;
    ppuinfo = Current_PU_Info;
    pmaptab = Current_Map_Tab;

    /* create temporaries for calls to runtime PDO routines */
    // Need the do_index_type, so move it to proper position.
    //Make_Local_Temps();
      // Any other side-effect may be caused?
    //local_start = mpbase_st;
    //local_ntrip = mptrips_st;
    //thread_info = mpflags_st;
  }

  cur_node = WN_first(WN_region_pragmas(tree));

  FmtAssert (cur_node &&
             WN_opcode(cur_node) == OPC_PRAGMA &&
             WN_pragma(cur_node) == WN_PRAGMA_PDO_BEGIN,
             ("Process_PDO: Unexpected first pragma node"));
  is_omp = WN_pragma_omp(cur_node);

  next_node = WN_next(cur_node);
#ifdef KEY
  WN_DELETE_FromBlock (WN_region_pragmas(tree), cur_node);
#else
  WN_Delete ( cur_node );
#endif

  while (cur_node = next_node) {

    next_node = WN_next(cur_node);

    if (((WN_opcode(cur_node) == OPC_PRAGMA) ||
	 (WN_opcode(cur_node) == OPC_XPRAGMA)) &&
	(WN_pragmas[WN_pragma(cur_node)].users & PUSER_MP)) {

      switch (WN_pragma(cur_node)) {

	case WN_PRAGMA_AFFINITY:
	  WN_next(cur_node) = nested_affinity_nodes;
	  nested_affinity_nodes = cur_node;
	  break;

	case WN_PRAGMA_DATA_AFFINITY:
	  WN_next(cur_node) = nested_affinity_d_nodes;
	  nested_affinity_d_nodes = cur_node;
	  break;

	case WN_PRAGMA_THREAD_AFFINITY:
	  WN_next(cur_node) = nested_affinity_t_nodes;
	  nested_affinity_t_nodes = cur_node;
	  break;

	case WN_PRAGMA_CHUNKSIZE:
	  if (nested_chunk_node)
	    WN_DELETE_Tree ( nested_chunk_node );
	  nested_chunk_node = cur_node;
	  break;

	case WN_PRAGMA_LASTLOCAL:
	  for (wn = nested_lastlocal_nodes; wn; wn = WN_next(wn))
	    if (Identical_Pragmas(cur_node, wn))
	      break;
	  if (wn == NULL) {
	    WN_next(cur_node) = nested_lastlocal_nodes;
	    nested_lastlocal_nodes = cur_node;
	    ++nested_local_count;
	    if (TY_kind(ST_type(WN_st(cur_node))) == KIND_SCALAR)
	      shared_table[shared_count++] = WN_st(cur_node);
	  } else
	    WN_Delete ( cur_node );
	  break;

	case WN_PRAGMA_LASTTHREAD:
	  if (nested_lastthread_node)
	    WN_Delete ( nested_lastthread_node );
	  nested_lastthread_node = cur_node;
	  break;

	case WN_PRAGMA_LOCAL:
	  for (wn = nested_local_nodes; wn; wn = WN_next(wn))
	    if (Identical_Pragmas(cur_node, wn))
	      break;
	  if (wn == NULL) {
	    if (ST_Has_Dope_Vector(WN_st(cur_node))) {
	        // F90 arrays with dope vectors must be initialized
	      WN_next(cur_node) = nested_firstprivate_nodes;
	      nested_firstprivate_nodes = cur_node;
	    } else {
	      WN_next(cur_node) = nested_local_nodes;
	      nested_local_nodes = cur_node;
	    }
	    ++nested_local_count;
	  } else
	    WN_Delete ( cur_node );
	  break;

	case WN_PRAGMA_FIRSTPRIVATE:
	  for (wn = nested_firstprivate_nodes; wn; wn = WN_next(wn))
	    if (Identical_Pragmas(cur_node, wn))
	      break;
	  if (wn == NULL) {
	    WN_next(cur_node) = nested_firstprivate_nodes;
	    nested_firstprivate_nodes = cur_node;
	    ++nested_local_count;
	  } else
	    WN_Delete ( cur_node );
	  break;

	case WN_PRAGMA_MPSCHEDTYPE:
	  if (nested_mpsched_node)
	    WN_Delete ( nested_mpsched_node );
	  nested_mpsched_node = cur_node;
	  break;

	case WN_PRAGMA_NOWAIT:
	  if (nested_nowait_node)
	    WN_Delete ( nested_nowait_node );
	  nested_nowait_node = cur_node;
	  break;

	case WN_PRAGMA_ORDERED:
	  if (nested_ordered_node)
	    WN_Delete ( nested_ordered_node );
	  nested_ordered_node = cur_node;
	  break;

        case WN_PRAGMA_ORDERED_LOWER_BOUND:
          if (nested_do_order_lb) 
            WN_Delete (nested_do_order_lb);
          nested_do_order_lb = cur_node;
          break;

        case WN_PRAGMA_ORDERED_STRIDE:
          if (nested_do_order_stride) 
            WN_Delete (nested_do_order_stride);
          nested_do_order_stride = cur_node;
          break;

	case WN_PRAGMA_PDO_END: 
	case WN_PRAGMA_END_MARKER:
	  break; 

	case WN_PRAGMA_REDUCTION:
	  for (wn = nested_reduction_nodes; wn; wn = WN_next(wn))
	    if (Identical_Pragmas(cur_node, wn))
	      break;
	  if (wn == NULL) {
	    WN_next(cur_node) = nested_reduction_nodes;
	    nested_reduction_nodes = cur_node;
	    ++nested_local_count;
	    ++nested_reduction_count;
	    if (WN_opcode(cur_node) == OPC_PRAGMA)
	      shared_table[shared_count++] = WN_st(cur_node);
	  } else
	    WN_DELETE_Tree ( cur_node );
	  break;

      /* we can get a SHARED(preg) clause in a SINGLE region due to
         variable renaming in LNO; must allow these pragmas */
	case WN_PRAGMA_SHARED:
	  for (wn = nested_shared_nodes; wn; wn = WN_next(wn))
	    if (Identical_Pragmas(cur_node, wn))
	      break;
	  if (wn == NULL) {
	    WN_next(cur_node) = nested_shared_nodes;
	    nested_shared_nodes = cur_node;
	    if (TY_kind(ST_type(WN_st(cur_node))) == KIND_SCALAR)
	      shared_table[shared_count++] = WN_st(cur_node);
	  } else
	    WN_Delete ( cur_node );
	  break;

	default:
	  Fail_FmtAssertion (
		  "out of context pragma (%s) in MP {region pragma} processing",
		  WN_pragmas[WN_pragma(cur_node)].name);

      }

    } else

      Fail_FmtAssertion ( "out of context node (%s) in MP{region} processing",
			  OPCODE_name(WN_opcode(cur_node)) );
  }

#ifdef KEY
  // Detect if a parallel region directly contains a DO loop, and if it
  // is the last stmt in the parallel region. In that case, a call to
  // ompc_barrier need not be inserted at the end of the DO loop even if
  // there is no nowait clause for the do loop.
  // This optimization is disabled, see comments in Lastpdo_in_Parallel_Region.
  if (OPT_MP_Barrier_Opt && !nested_nowait_node &&
      Lastpdo_in_Parallel_Region (tree))
    nested_nowait_node = WN_CreatePragma (WN_PRAGMA_NOWAIT,
                                          (ST_IDX) NULL, 0, 0);
#endif

  body_block = WN_region_body(tree);
  first_node = pdo_node = WN_first(body_block);
#ifdef KEY
  // bug 13534: Sometimes the DO_LOOP may be inside a C++ exception
  // region. Extract it out of the region, after some validation checks.
  if (pdo_node && WN_operator(pdo_node) == OPR_REGION &&
      WN_region_kind(pdo_node) == REGION_KIND_EH /* &&
      pdo_node == WN_last(body_block) */) {

    // The region is the only statement in the parallel DO.
    // Bug 14036: The above line is not true any more. The
    // restriction in the above line has been
    // removed, because there is often a label after the EH
    // region containing the DO loop, to account for the label
    // a "break" may jump to.
    WN * region_body = WN_region_body(pdo_node);
    WN * stmt = WN_first(region_body);
    for (; stmt; stmt = WN_next(stmt)) {
      if (stmt == WN_last(region_body) &&
          WN_opcode(stmt) == OPC_DO_LOOP)
        break;
    }
    Is_True (!stmt || WN_operator(stmt) == OPR_DO_LOOP,
             ("Expected statement to be DO_LOOP."));
    if (stmt) {
      // extract out of region
      stmt = WN_EXTRACT_FromBlock (region_body, stmt);
      // insert after region
      WN_INSERT_BlockAfter (body_block, pdo_node, stmt);
      first_node = pdo_node = WN_first(body_block);
    }
  }
#endif
  while (pdo_node && (WN_opcode(pdo_node) != OPC_DO_LOOP)) {
    if ((WN_opcode(pdo_node) == OPC_DO_WHILE) ||
	(WN_opcode(pdo_node) == OPC_WHILE_DO))
      while_seen = TRUE;
    pdo_node = WN_next(pdo_node);
  }

  // for orphaned PDO, you must stripoff extra MP, and lowering
  // necessary ones. The real lowering is in Delayed translation,
  // after gtid is set right.csc.
  if (orphaned)
  {
     Strip_Nested_MP( body_block, FALSE );
  }
  
  if (pdo_node) {
    WN *nested_non_pod_finalization_nodes;

    if (non_pod_finalization_nodes) {
        // In non-orphaned PDO, Process_Parallel_Region() will have
        // extracted non-POD finalization code (if any), so we re-insert it
      if (mpt != MPP_PDO)
        Fail_FmtAssertion("out of place non-POD finalization code");
      nested_non_pod_finalization_nodes = non_pod_finalization_nodes;
      non_pod_finalization_nodes = NULL;
    } else
      nested_non_pod_finalization_nodes = NULL;

    if (nested_local_count) {
        // privatize within DO_LOOP but not code before or after it
      vsize = (nested_local_count + 1) * sizeof(VAR_TABLE);
      nested_var_table = (VAR_TABLE *) alloca ( vsize );
      BZERO ( nested_var_table, vsize );
      Create_Local_Variables ( nested_var_table, nested_reduction_nodes,
			       nested_lastlocal_nodes, nested_local_nodes,
			       nested_firstprivate_nodes,
			       &nested_firstprivate_block,
			       nested_lastthread_node,
                                 /* orphaned PDO does its own allocation */
			       orphaned ? &nested_alloca_block :
			                  &alloca_block);
      Localize_Parent_Stack lps(orphaned, body_block);
        // Walk_and_Localize() won't replace DO_LOOP node
      (void) Walk_and_Localize ( pdo_node, nested_var_table, &lps, FALSE,
                                 &nested_non_pod_finalization_nodes );

    }


    prev_node = WN_prev(pdo_node);
    if (prev_node) {
        // add synchronization to sandwich code before PDO
      WN *code_before_pdo = WN_CreateBlock();
      WN_EXTRACT_ItemsFromBlock(body_block, first_node, prev_node);
      WN_first(code_before_pdo) = first_node;
      WN_last(code_before_pdo) = prev_node;
      WN_INSERT_BlockBefore(body_block, pdo_node, code_before_pdo);
      prev_node = WN_prev(pdo_node);
    }

    WN *code_after_pdo = NULL;  // BLOCK for sandwich code after PDO (if any)
    if (WN_next(pdo_node)) {
      WN *next_node = WN_next(pdo_node), *last_node = WN_last(body_block);
      code_after_pdo = WN_CreateBlock();
      WN_EXTRACT_ItemsFromBlock(body_block, next_node, last_node);
      WN_first(code_after_pdo) = next_node;
      WN_last(code_after_pdo) = last_node;
    }
    WN_EXTRACT_FromBlock(body_block, pdo_node);

    /* Determine user's real do index and type. */

    do_index_st = WN_st(WN_index(pdo_node));

    do_index_type = TY_mtype(ST_type(do_index_st));
    if (do_index_type == MTYPE_I1 || do_index_type == MTYPE_I2)
      do_index_type = MTYPE_I4;
    else if (do_index_type == MTYPE_U1 || do_index_type == MTYPE_U2)
      do_index_type = MTYPE_U4;

#if defined(TARG_X8664) || defined(TARG_MIPS)
    // Bug 7275 - this depends on the library implementation
    if (MTYPE_byte_size(do_index_type) == 4)
#else 
    if (do_index_type == MTYPE_I4)
#endif
    {
/*        if (lastthread_node)
           fast_doacross = TRUE;
        else if (ordered_node)
           fast_doacross = FALSE;
        else if (mpsched_node)
           fast_doacross =
             (WN_pragma_arg1(mpsched_node) == WN_PRAGMA_SCHEDTYPE_SIMPLE);
        else if (pu_mpsched_node)
           fast_doacross =
             (WN_pragma_arg1(pu_mpsched_node) == WN_PRAGMA_SCHEDTYPE_SIMPLE);
        else if (chunk_node || pu_chunk_node)
           fast_doacross = FALSE;
        else
*/           fast_doacross = TRUE;
    }
    else
      fast_doacross = FALSE;
        
//    if( orphaned )
//    {
    Make_Local_Temps( );
//    }
    /* Translate do statement itself. */

    Extract_Do_Info ( pdo_node );

     // determine schedule type and chunk size.
     // Maybe it should shrink into a separate function and move to somewhere
     // else. csc.
     // Why lastthread STATIC_EVEN and chunk_size must be 1? Is this the 
     // limitation caused by RTL? If, should be eliminated. 
  
    /*
    if( do_prefix != NULL )
      WN_DELETE_Tree( do_prefix );
    if( do_suffix != NULL )
      WN_DELETE_Tree( do_suffix );
    */
    do_prefix = NULL;
    do_suffix = NULL;
    // What's the relationship between order/nested_order
    BOOL ordered = ((nested_ordered_node && WN_pragma_omp(nested_ordered_node))||
                    ( ordered_node && WN_pragma_omp( ordered_node )))
                      ? TRUE : FALSE;
    if ( nested_lastthread_node)
    {
       if( ordered )
         kmpc_schedule = OMP_SCHED_ORDERED_STATIC_EVEN;
       else
         kmpc_schedule = OMP_SCHED_STATIC_EVEN;
       chunk_wn = WN_CreateIntconst( OPC_I4INTCONST, 1 );
    }
    else if (( ordered_node && !(WN_pragma_omp(ordered_node))) ||
        ( nested_ordered_node && !( WN_pragma_omp( nested_ordered_node ))))
    {
        // Why not for OpenMP ordered? How to handle OpenMP ordered?
       kmpc_schedule = OMP_SCHED_DYNAMIC;
       chunk_wn = WN_CreateIntconst( OPC_I4INTCONST, 1 );
    }
    else
    {
      if( nested_mpsched_node )
      {
        kmpc_schedule = Translate_Schedule_Type( WN_pragma_arg1( nested_mpsched_node ),
                                                 ordered );
      }
      else if( mpsched_node )
         kmpc_schedule = Translate_Schedule_Type(WN_pragma_arg1(mpsched_node), ordered);
      else if( pu_mpsched_node )
         kmpc_schedule = Translate_Schedule_Type(WN_pragma_arg1(pu_mpsched_node), ordered);
      // Is this proper?, maybe not.
      else if( nested_chunk_node || chunk_node || pu_chunk_node )
      {
         if( ordered )
           kmpc_schedule = OMP_SCHED_ORDERED_DYNAMIC;	
         else
           kmpc_schedule = OMP_SCHED_DYNAMIC;
      }
      else
      {
         if( ordered ) 
           kmpc_schedule = OMP_SCHED_ORDERED_STATIC_EVEN;
         else
           kmpc_schedule = OMP_SCHED_STATIC_EVEN;
      }
      if( nested_chunk_node )
      {
         chunk_wn = WN_COPY_Tree( WN_kid( nested_chunk_node, 0 ));
      }
      else if( chunk_node )
         chunk_wn = WN_COPY_Tree( WN_kid( chunk_node, 0 ));
      else if ( pu_chunk_node )
         chunk_wn = WN_COPY_Tree( WN_kid( pu_chunk_node, 0 ));
      else
         chunk_wn = WN_CreateIntconst( OPC_I4INTCONST, 1 );
    }

       // Fix exceptions, set all unknown schedule type to STATIC_EVEN
    if( kmpc_schedule == OMP_SCHED_UNKNOWN )
    {
       if( ordered )
         kmpc_schedule = OMP_SCHED_ORDERED_STATIC_EVEN;
       else
         kmpc_schedule = OMP_SCHED_STATIC_EVEN;
    }
    
       // Make sure schedule type is what we supported, This is unnecessary indeed.
    Is_True( ((kmpc_schedule >= OMP_SCHED_NORMAL_FIRST) && 
               (kmpc_schedule <= OMP_SCHED_NORMAL_LAST)) ||
             ((kmpc_schedule >= OMP_SCHED_ORDERED_FIRST) &&
             (kmpc_schedule <= OMP_SCHED_ORDERED_LAST)),
	   ( "A schedule type not supported by current RTL" )); 

    // Now, transform DO.
    // The variables generation is a big problem, maybe Transform_Do
    // Need to be modified.
    // Now the prev_node, last is wrong
    //Transform_Do( body_block, pdo_node, ompc_schedule, chunk_wn );
    /* Generate replacement code for pdo. */
  
    if (nested_alloca_block) {
      WN *last = WN_last(nested_alloca_block), *save_last;

        /* conditionally do deallocation of alloca()'d memory--see detailed
	   comment in Gen_MP_SingleProcess_Region() */
      if ((do_dealloca = !Calls_Alloca(body_block)) != 0) {
        wn = Gen_Save_Stack_Pointer("orphaned_pdo", &sp_save_stid);
        save_last = WN_last(wn);
        WN_INSERT_BlockAfter(body_block, prev_node, wn);
        prev_node = save_last;
        avlist = CXX_NEW(Alloca_Var_List(nested_alloca_block),
                         &mp_lower_pool);
      }
      WN_INSERT_BlockAfter(body_block, prev_node, nested_alloca_block);
      prev_node = last;

      Set_PU_has_alloca(Get_Current_PU());
    }

    // Don't know what is the content of lastthread_node, csc.
    if (nested_lastthread_node) {
      wn = Gen_MP_Store ( WN_st(nested_lastthread_node),
			  WN_offsetx(nested_lastthread_node),
			  WN_Intconst ( MTYPE_I4, 0 ));
      WN_INSERT_BlockAfter(body_block, prev_node, wn);
      prev_node = wn;
    }

    if (nested_firstprivate_block) {
      Is_True(nested_firstprivate_nodes, ("NULL nested_firstprivate_nodes"));

      WN *last = WN_last(nested_firstprivate_block);

      WN_INSERT_BlockAfter(body_block, prev_node, nested_firstprivate_block);
      prev_node = last;
    }

    /*
    * Set gtid for orphaned PDO. non-orphaned's is set by Create_MicroTask
    */
    if( orphaned )
    {
      WN *return_wn = Gen_Store_Gtid();
      WN *last = WN_last( return_wn );

      WN_INSERT_BlockAfter( body_block, prev_node, return_wn );
      prev_node = last;
    }

    if (nested_reduction_count) {
      WN *last;

      Gen_MP_Reduction(nested_var_table, nested_local_count,
                       &reduction_init_block, &reduction_store_block);
      last = WN_last(reduction_init_block);
      WN_INSERT_BlockAfter(body_block, prev_node, reduction_init_block);
      prev_node = last;
    }

    WN *return_wn = Transform_Do( pdo_node, kmpc_schedule, chunk_wn );
    WN *last = WN_last( return_wn );
    WN_INSERT_BlockAfter( body_block, prev_node, return_wn );
    prev_node = last;
    // Why always 64?
/*
    if (nested_ordered_node && WN_pragma_omp(nested_ordered_node)) {
      WN *order_begin;
      order_begin = Gen_OMP_Pdo_Ordered_Begin(WN_kid0(nested_do_order_lb),
                                              WN_kid0(nested_do_order_stride));
      WN_INSERT_BlockAfter (body_block, prev_node, order_begin);
      prev_node = order_begin;
    }

    if (numthreads_node)
      wn = Gen_MP_BeginPDO_64 ( base_node, ntrip_node, stride_node,
				WN_CreateIntconst ( OPC_I4INTCONST,
						    num_constructs ),
				WN_COPY_Tree ( WN_kid0(numthreads_node) ),
				mpsched_wn, chunk_wn, is_omp );
    else
      wn = Gen_MP_BeginPDO_64 ( base_node, ntrip_node, stride_node,
				WN_CreateIntconst ( OPC_I4INTCONST,
						    num_constructs ),
				WN_CreateIntconst ( OPC_I4INTCONST, 0 ),
				mpsched_wn, chunk_wn, is_omp );


    WN_INSERT_BlockAfter(body_block, prev_node, wn);
    prev_node = wn;
*/
/*    wn1 = Gen_MP_NextIters_64 ( WN_CreateIntconst ( OPC_I4INTCONST,
						    num_constructs ),
				WN_Lda ( Pointer_type, 0, mpbase_st ),
				WN_Lda ( Pointer_type, 0, mptrips_st ),
				WN_Lda ( Pointer_type, 0, mpflags_st ),
                                is_omp );
    WN_INSERT_BlockAfter(body_block, prev_node, wn1);
    prev_node = wn1;

    Create_Preg_or_Temp ( MTYPE_I4, "mpni_status", &return_st, &return_ofst );
    GET_RETURN_PREGS(rreg1, rreg2, MTYPE_I4);
    wn2 = WN_Stid ( MTYPE_I4, return_ofst, return_st, ST_type(return_st),
		    WN_LdidPreg ( MTYPE_I4, rreg1 ));
    WN_linenum(wn2) = line_number;
    WN_INSERT_BlockAfter(body_block, prev_node, wn2);
    prev_node = wn2;

    while_block = WN_CreateBlock ( );
    WN_INSERT_BlockLast ( while_block, do_prefix );
    WN_INSERT_BlockLast ( while_block, pdo_node );
    WN_INSERT_BlockLast ( while_block, WN_COPY_Tree ( wn1 ) );
    WN_INSERT_BlockLast ( while_block, WN_COPY_Tree ( wn2 ) );
    wn = WN_CreateWhileDo ( WN_Ldid ( MTYPE_I4, return_ofst, return_st,
				      ST_type(return_st) ),
			    while_block );
    WN_linenum(wn) = line_number;
    WN_INSERT_BlockAfter(body_block, prev_node, wn);
    prev_node = wn;
 */ 
#ifdef KEY
    if (code_after_pdo) {
        // insert post-PDO sandwich code after all lowered PDO code
//Bug 4792
      if (nested_var_table && nested_lastlocal_nodes){
        Localize_Parent_Stack lps(FALSE, NULL);
        code_after_pdo = Walk_and_Localize ( code_after_pdo, nested_var_table, &lps, TRUE,
                                   &nested_non_pod_finalization_nodes );
      }
      WN_INSERT_BlockBefore(body_block, prev_node, code_after_pdo);
    }
#endif
    if (nested_lastlocal_nodes || nested_non_pod_finalization_nodes) {
      BOOL found_non_pod = FALSE;
      WN *first_and_last_mp_barrier = non_pod_first_and_lastprivate ?
                                        Gen_Barrier(local_gtid) : NULL;
//                                      Gen_MP_Barrier(FALSE) : NULL;

      wn = WN_CreateBlock ( );
      for (i = 0; i < nested_local_count; i++) {
        VAR_TABLE *v = &nested_var_table[i];

	if (v->vtype == VAR_LASTLOCAL) {
            // if any variables are both LASTLOCAL and FIRSTPRIVATE, put
            // an MP barrier just before the LASTLOCAL finalization to
            // prevent the race condition described in PV 566923
	  if (v->is_last_and_firstprivate && !first_and_last_mp_barrier)
	    first_and_last_mp_barrier = Gen_Barrier(local_gtid);
	    //first_and_last_mp_barrier = Gen_MP_Barrier(FALSE);

            // generate finalization code, unless frontend already did so
	  if (!v->is_non_pod) {
	    WN_INSERT_BlockLast ( wn,
	        Gen_MP_Load_Store ( v->new_st, v->new_offset,
	                            v->orig_st, v->orig_offset,
	                            v->is_dynamic_array ) );
          } else
            found_non_pod = TRUE;
	}
      }

        // validate non-POD finalization code
      if (nested_non_pod_finalization_nodes) {
        if (mpt == MPP_ORPHANED_PDO) {
            // orphaned PDO case: lastprivate nodes are on PDO
          if (!found_non_pod)
            Fail_FmtAssertion("missing non-POD lastprivate clauses");
        } else {
#ifndef KEY
            // KEY: GNU42 front-end does not want to ensure this, unless
            // this is necessary. Unless required, the lastlocal nodes
            // attached to the proper region seem more accurate.
            //
            // non-orphaned PDO case: lastprivate nodes are on PARALLEL
          if (found_non_pod)
              // fecc should move lastprivate clauses to enclosing PARALLEL
            Fail_FmtAssertion("extraneous non-POD lastprivate clauses");
#endif
        }
      } else {
        if (found_non_pod)
            // should never have clauses without finalization code
          Fail_FmtAssertion("missing non-POD finalization code");
      }

      if (nested_non_pod_finalization_nodes) {
        WN *finalization_code = WN_then(nested_non_pod_finalization_nodes);

        WN_then(nested_non_pod_finalization_nodes) = NULL;
        WN_INSERT_BlockLast(wn, finalization_code);
        WN_DELETE_Tree(nested_non_pod_finalization_nodes);
      }

      if (nested_lastthread_node)
	      wn = WN_CreateIf ( Gen_MP_Load ( WN_st(nested_lastthread_node), 
			  		 WN_offsetx(nested_lastthread_node) ),
			       wn, WN_CreateBlock() );
      else
        wn = WN_CreateIf ( WN_EQ ( MTYPE_I4,
				   Gen_MP_Load ( last_iter, 0 ),
				   WN_CreateIntconst ( OPC_I4INTCONST, 1 )),
			   wn, WN_CreateBlock() );
      WN_linenum(wn) = line_number;

        // if needed, insert an MP barrier surrounded by memory barriers
      if (first_and_last_mp_barrier) {
      	WN *blk = WN_CreateBlock();
      	WN_INSERT_BlockLast(blk, WN_CreateBarrier(TRUE, 0));
      	WN_INSERT_BlockLast(blk, first_and_last_mp_barrier);
      	WN_INSERT_BlockLast(blk, WN_CreateBarrier(FALSE, 0));
      	WN *last = WN_last(blk);
      	WN_INSERT_BlockAfter(body_block, prev_node, blk);
      	prev_node = last;
      }
      
        // insert the IF, surrounded by memory barriers
      {
      	WN *blk = WN_CreateBlock();
      	WN_INSERT_BlockLast(blk, WN_CreateBarrier(TRUE, 0));
      	WN_INSERT_BlockLast(blk, wn);
      	WN_INSERT_BlockLast(blk, WN_CreateBarrier(FALSE, 0));

#ifdef KEY
	// bug 5035: insert the barrier after lastprivate handling
        if( nested_nowait_node == NULL )
        {
          WN_INSERT_BlockLast(blk, Gen_Barrier(local_gtid));
        }
        else
        {
          WN_Delete(nested_nowait_node);
          nested_nowait_node = NULL;
        }
#endif // KEY

      	WN *last = WN_last(blk);
      	WN_INSERT_BlockAfter(body_block, prev_node, blk);
      	prev_node = last;
      }

        // Insert barriers inside the if as well
      WN_INSERT_BlockBefore (WN_then(wn), NULL, WN_CreateBarrier(TRUE,0));
      WN_INSERT_BlockAfter (WN_then(wn), NULL, WN_CreateBarrier(FALSE,0));
    } // if (nested_lastlocal_nodes || nested_non_pod_finalization_nodes)
#ifdef KEY
    else
    {  // Fix up the barrier in the else case
      if( nested_nowait_node == NULL )
      {
        WN_INSERT_BlockAfter(body_block, prev_node, Gen_Barrier(local_gtid));
        prev_node = WN_last (body_block);
      }
      else
      {
        WN_Delete(nested_nowait_node);
        nested_nowait_node = NULL;
      }
    }
#endif // KEY

    if (nested_reduction_count) {
      WN *last = WN_last(reduction_store_block);

      WN_INSERT_BlockAfter(body_block, prev_node, reduction_store_block);
      prev_node = last;
    }

// TODO: barrier/flush code must be presented here.
//       Handle nowait syntax.
#ifndef KEY
    if (nested_nowait_node == NULL || is_omp) {
      if (nested_nowait_node == NULL) {
          // insert a forward barrier
        WN *bwn = WN_CreateBarrier(TRUE, 0);
        WN_INSERT_BlockAfter(body_block, prev_node, bwn);
        prev_node = bwn;
      }
/*      wn = Gen_MP_EndPDO ( WN_CreateIntconst ( OPC_I4INTCONST,
					       num_constructs ),
                           is_omp,
                           (nested_nowait_node ? FALSE : TRUE));
      WN_INSERT_BlockAfter(body_block, prev_node, wn);
      prev_node = wn;
*/    if (nested_nowait_node == NULL) {
        // insert a backward barrier
        wn = WN_CreateBarrier(FALSE, 0);
        WN_INSERT_BlockAfter(body_block, prev_node, wn);
        prev_node = wn;
      }
    }
#endif
/*
    if (nested_ordered_node && WN_pragma_omp(nested_ordered_node)) {
      WN *order_end = Gen_OMP_Pdo_Ordered_End ();
      WN_INSERT_BlockAfter(body_block, prev_node, order_end);
      prev_node = order_end;
    }
*/
    if (do_dealloca) {
      WN *restore_block = Gen_Restore_Stack_Pointer(sp_save_stid, avlist);
      CXX_DELETE(avlist, &mp_lower_pool);
      WN *last = WN_last(restore_block);

      WN_INSERT_BlockAfter(body_block, prev_node, restore_block);
      prev_node = last;
    }
#ifndef KEY
    if (code_after_pdo) {
        // insert post-PDO sandwich code after all lowered PDO code
      WN *last = WN_last(code_after_pdo);
      WN_INSERT_BlockAfter(body_block, prev_node, code_after_pdo);
      prev_node = last;
    }
#endif
    WN_next(prev_node) = NULL;
    WN_last(body_block) = prev_node;

    if (orphaned)
    {
       Delayed_MP_Translation( body_block );
    }

  } else {

    /* There was no do_loop node in spite of a pdo pragma. Any residual code
       can be discarded. */

    if (!while_seen) {
      if (WN_first(body_block)) {
      	WN_DELETE_Tree ( body_block );
      	body_block = WN_CreateBlock ( );
      }
    } else
      Fail_FmtAssertion
		  ("parallel DO was converted to WHILE and not converted back");

  } // if (pdo_node)

  /* Free up all saved nodes. */
  // TODO: clean up work should be inserted here. csc
  // Need a set of rules to regular the create/delete of WN.

  while (nested_affinity_nodes) {
    WN *wn = WN_next(nested_affinity_nodes);
    WN_DELETE_Tree ( nested_affinity_nodes );
    nested_affinity_nodes = wn;
  }

  while (nested_affinity_d_nodes) {
    WN *wn = WN_next(nested_affinity_d_nodes);
    WN_DELETE_Tree ( nested_affinity_d_nodes );
    nested_affinity_d_nodes = wn;
  }

  while (nested_affinity_t_nodes) {
    WN *wn = WN_next(nested_affinity_t_nodes);
    WN_DELETE_Tree ( nested_affinity_t_nodes );
    nested_affinity_t_nodes = wn;
  }

  if (nested_chunk_node)
    WN_DELETE_Tree ( nested_chunk_node );

  while (nested_lastlocal_nodes) {
    WN *wn = WN_next(nested_lastlocal_nodes);
    WN_Delete ( nested_lastlocal_nodes );
    nested_lastlocal_nodes = wn;
  }

  if (nested_lastthread_node)
    WN_Delete ( nested_lastthread_node );

  while (nested_local_nodes) {
    WN *wn = WN_next(nested_local_nodes);
    WN_Delete ( nested_local_nodes );
    nested_local_nodes = wn;
  }

  while (nested_firstprivate_nodes) {
    WN *wn = WN_next(nested_firstprivate_nodes);
    WN_Delete ( nested_firstprivate_nodes );
    nested_firstprivate_nodes = wn;
  }

  if (nested_mpsched_node)
    WN_Delete ( nested_mpsched_node );

  if (nested_nowait_node)
    WN_Delete ( nested_nowait_node );

  if (nested_ordered_node)
    WN_Delete ( nested_ordered_node );

  while (nested_reduction_nodes) {
    WN *wn = WN_next(nested_reduction_nodes);
    WN_Delete ( nested_reduction_nodes );
    nested_reduction_nodes = wn;
  }

  while (nested_shared_nodes) {
    WN *wn = WN_next(nested_shared_nodes);
    WN_Delete ( nested_shared_nodes );
    nested_shared_nodes = wn;
  }

  for (i=0; i < nested_local_count; i++) {
    if (nested_var_table[i].vtree)
      WN_DELETE_Tree ( nested_var_table[i].vtree );
    if (nested_var_table[i].vtreex)
      WN_DELETE_Tree ( nested_var_table[i].vtreex );
  }

  return (body_block);
} // Process_PDO()

/*  Process the contents of a parallel do.  */
//  Modified by csc. Insert contents originally in lower_mp.

static void 
Process_Parallel_Do ( void )
{
  INT32 i;
  WN *wn;
  WN *lastthread_init = NULL;
  WN *reduction_init_block = NULL;
  WN *reduction_store_block = NULL;
  WN *last_local_if = NULL;
     // default schedule type
  SCHEDULE_TYPE kmpc_schedule = OMP_SCHED_STATIC_EVEN; 
     // note that, chunk_size maybe not available as int.
     // So we present it as an WN node.
  // INT32 chunk_size = 0;
  WN *chunk_wn = NULL;
  //  WN *schedule_wn = NULL;
  
  Is_True(mpt == MPP_PARALLEL_DO, ("not inside a PARALLEL DO"));

     // determine schedule type and chunk size.
     // Maybe it should shrink into a separate function and move to somewhere
     // else. csc.
     // Why lastthread STATIC_EVEN and chunk_size must be 1? Is this the 
     // limitation caused by RTL? If, should be eliminated. 
 /* 
  if( do_prefix != NULL )
    WN_DELETE_Tree( do_prefix );
  if( do_suffix != NULL )
    WN_DELETE_Tree( do_suffix );
 */
        
  do_prefix = NULL;
  do_suffix = NULL;
  BOOL ordered = (ordered_node && WN_pragma_omp(ordered_node))
                    ? TRUE : FALSE;
  if ( lastthread_node)
  {
     if( ordered )
       kmpc_schedule = OMP_SCHED_ORDERED_STATIC_EVEN;
     else
       kmpc_schedule = OMP_SCHED_STATIC_EVEN;
     chunk_wn = WN_CreateIntconst( OPC_I4INTCONST, 1 );
  }
  else if ( ordered_node && !(WN_pragma_omp(ordered_node)))
  {
      // Why not for OpenMP ordered? How to handle OpenMP ordered?
     kmpc_schedule = OMP_SCHED_DYNAMIC;
     chunk_wn = WN_CreateIntconst( OPC_I4INTCONST, 1 );
  }
  else
  {
    if (mpsched_node)
       kmpc_schedule = Translate_Schedule_Type(WN_pragma_arg1(mpsched_node), ordered);
    else if (pu_mpsched_node)
       kmpc_schedule = Translate_Schedule_Type(WN_pragma_arg1(pu_mpsched_node), ordered);
    else if (chunk_node || pu_chunk_node)
    {
       if( ordered )
         kmpc_schedule = OMP_SCHED_ORDERED_DYNAMIC;	
       else
         kmpc_schedule = OMP_SCHED_DYNAMIC;
    }
    else
    {
       if( ordered ) 
         kmpc_schedule = OMP_SCHED_ORDERED_STATIC_EVEN;
       else
         kmpc_schedule = OMP_SCHED_STATIC_EVEN;
    }
    if (chunk_node)
       chunk_wn = WN_COPY_Tree( WN_kid(chunk_node, 0));
    else if ( pu_chunk_node )
       chunk_wn = WN_COPY_Tree( WN_kid(pu_chunk_node, 0));
    else
       chunk_wn = WN_CreateIntconst( OPC_I4INTCONST, 1 );
  }

     // Fix exceptions, set all unknown schedule type to STATIC_EVEN
  if( kmpc_schedule == OMP_SCHED_UNKNOWN )
  {
     if( ordered )
       kmpc_schedule = OMP_SCHED_ORDERED_STATIC_EVEN;
     else
       kmpc_schedule = OMP_SCHED_STATIC_EVEN;
  }
    
     // Make sure schedule type is what we supported, This is unnecessary indeed.
  Is_True( ((kmpc_schedule >= OMP_SCHED_NORMAL_FIRST) && 
             (kmpc_schedule <= OMP_SCHED_NORMAL_LAST)) ||
           ((kmpc_schedule >= OMP_SCHED_ORDERED_FIRST) &&
             (kmpc_schedule <= OMP_SCHED_ORDERED_LAST)),
	   ( "A schedule type not supported by current RTL" )); 

  //schedule_wn = WN_CreateIntconst( OPC_I4INTCONST, kmpc_schedule );

  /* Initialization. */

  psymtab = CURRENT_SYMTAB;
  ppuinfo = Current_PU_Info;
  pmaptab = Current_Map_Tab;


  /* Strip any nested mp stuff due to inlining. */

  Strip_Nested_MP ( stmt_block, FALSE );

  // Gather STs for inner scope VLAs

  Gather_Inner_Scope_Vlas ( stmt_block );

  /* Extract do info for mp scheduling. */
   // base, trip_count, and stride are stored in the global vars

  Extract_Do_Info (do_node);

  /* Create and initialize all parent temps that will be needed. */

  Process_Preg_Temps ( stmt_block, FALSE );

  /* Create parallel do function. */

    // The parameter is merely for compatible. nouse. csc
  Push_Some_Globals( );
  Create_MicroTask ( fast_doacross ? PAR_FUNC_DO32 : PAR_FUNC_DO64 );
  Delayed_MP_Translation( stmt_block );
  
  /* Create local variable correspondence table */

  Create_Local_Variables ( var_table, reduction_nodes, lastlocal_nodes,
			   local_nodes, firstprivate_nodes,
			   &firstprivate_block, lastthread_node,
			   &alloca_block );

  /* Do the replacement walk and identify reduction variables */

  Localize_Parent_Stack lps(FALSE, NULL);
  stmt_block = Walk_and_Localize ( stmt_block, var_table, &lps, TRUE,
                                   &non_pod_finalization_nodes );

#ifdef KEY
  if (LANG_Enable_CXX_Openmp && PU_misc_info(PU_Info_pu(ppuinfo)) &&
      PU_src_lang(PU_Info_pu(ppuinfo)) == PU_CXX_LANG)
  { // C++: This code has not yet been tested.
    Is_True (parallel_proc, ("Parallel block unavailable"));
    PU_IDX pu_idx = ST_pu(parallel_proc);
    PU &pu = Pu_Table[pu_idx];
    Set_PU_misc_info (pu,
        Process_PU_Exc_Info(PU_misc_info (PU_Info_pu(ppuinfo)), var_table));
  }

  // If chunk uses a private variable (e.g. threadprivate variable), fix it
  chunk_wn = Walk_and_Localize ( chunk_wn, var_table, &lps, FALSE,
                                 &non_pod_finalization_nodes );
  // bug 8224: If preamble uses a private variable, fix it
  if_preamble_block = Walk_and_Localize ( if_preamble_block, var_table, &lps,
                                          FALSE, &non_pod_finalization_nodes );
  do_preamble_block = Walk_and_Localize ( do_preamble_block, var_table, &lps,
                                          FALSE, &non_pod_finalization_nodes );
#endif // KEY

  /* Create copyin nodes for all local vars with preamble stores. */

  Process_Preamble_Stores ( if_preamble_block, var_table );
  Process_Preamble_Stores ( do_preamble_block, var_table );

  if (Cur_PU_Feedback) {
      // scale FB frequency of do loop by (1 / num_processors)
    Scale_FB_Parallel_Do(do_node, 1.0 / NOMINAL_PROCS);
  }

  /* Rewrite do loop */

     // Now, Do must be rewriten according to schedule type.
     // Note that, We now don't have local_start, locat_ntrip
     // and thread_info, must fix it.
  Make_Local_Temps( );
  WN *wn_prev_do = WN_prev( do_node );
  WN *wn_next_do = WN_next( do_node );
  WN_EXTRACT_FromBlock( stmt_block, do_node );
// Bug 4498
#ifdef KEY
  if (!nested_nowait_node)
    nested_nowait_node = WN_CreatePragma (WN_PRAGMA_NOWAIT,
                                          (ST_IDX) NULL, 0, 0);
#endif
  WN *return_wn = Transform_Do( do_node, kmpc_schedule, chunk_wn );
  WN_INSERT_BlockAfter( stmt_block, wn_prev_do, return_wn );
#ifdef KEY
  FmtAssert ( nested_nowait_node, ("NULL nested_nowait_node") );
  WN_Delete( nested_nowait_node );
  nested_nowait_node = NULL;
#endif // KEY

  if( do_preamble_block )
  {
    WN_INSERT_BlockFirst( stmt_block, do_preamble_block );
  }


//  if (ordered_node && WN_pragma_omp(ordered_node)) {
//    /* Insert pdo_ordered_begin/end calls */
//    WN *order_begin = Gen_OMP_Pdo_Ordered_Begin (WN_kid0(do_order_lb),
//                                                 WN_kid0(do_order_stride));
//    WN *order_end = Gen_OMP_Pdo_Ordered_End ();
//    WN_INSERT_BlockFirst (stmt_block, order_begin);
//    WN_INSERT_BlockLast (stmt_block, order_end);
//    WN_kid0(do_order_lb) = WN_kid0(do_order_stride) = NULL;
//  }

  if (copyin_nodes) {
    WN_INSERT_BlockFirst (stmt_block, Gen_MP_Copyin(TRUE));
  }
#ifdef KEY
  Gen_Threadpriv_Func(reference_block, parallel_func, FALSE);
#endif
  /* Generate initialization code for lastthread variable */

  if (lastthread_node)
    lastthread_init = Gen_MP_Store ( WN_st(lastthread_node),
				     WN_offsetx(lastthread_node),
				     WN_Intconst ( MTYPE_I4, 0 ));

  if (reduction_count)  /* Generate init/finish reduction code */
    Gen_MP_Reduction(var_table, local_count, &reduction_init_block,
                     &reduction_store_block);

  /* Build up the if for the last_locals */

  BOOL hit_first_last_priv_bug = FALSE;
  if (lastlocal_nodes) {

    wn = WN_CreateBlock ( );
    for (i=0; i < local_count; i++) {
      if (var_table[i].vtype == VAR_LASTLOCAL) {
          // if any variables are both LASTLOCAL and FIRSTPRIVATE, put
          // an MP barrier just before the LASTLOCAL finalization to
          // prevent the race condition described in PV 566923
        if (var_table[i].is_last_and_firstprivate &&
            !hit_first_last_priv_bug) {
            // We can't use a barrier to fix PV 566923 (as we can in the
            // PDO case) because the __mpdo_() nested routine may be called
            // multiple times, which means the finalization code gets called
            // multiple times, which can result in deadlock.  So this is
            // not yet implemented for PARALLEL DO.
          ErrMsgLine(EC_MPLOWER_first_last_priv, line_number);
          hit_first_last_priv_bug = TRUE;
        }

	WN_INSERT_BlockLast ( wn,
			      Gen_MP_Load_Store ( var_table[i].new_st,
						  var_table[i].new_offset,
						  var_table[i].orig_st,
						  var_table[i].orig_offset,
					       var_table[i].is_dynamic_array ));
       }
    }
    if (lastthread_node)
      last_local_if = WN_CreateIf ( Gen_MP_Load ( WN_st(lastthread_node), 
						  WN_offsetx(lastthread_node) ),
				    wn, WN_CreateBlock ( ) );
    else
      last_local_if = WN_CreateIf ( WN_EQ( MTYPE_I4,
		                           Gen_MP_Load ( last_iter, 0 ),
		                           WN_CreateIntconst ( OPC_I4INTCONST, 1)),
		                    wn, 
                                    WN_CreateBlock( ));
    WN_linenum(last_local_if) = line_number;
  }

  /* Consolidate all portions of nested parallel procedure */

  if (alloca_block)
    WN_INSERT_BlockLast ( parallel_func, alloca_block );
  WN_INSERT_BlockLast ( parallel_func,
			WN_CreatePragma ( WN_PRAGMA_PREAMBLE_END, ST_IDX_ZERO,
			                  0, 0 ));
  if (firstprivate_block)
    WN_INSERT_BlockLast ( parallel_func, firstprivate_block );
  if (copyin_block)
    WN_INSERT_BlockLast ( parallel_func, copyin_block );
  if (lastthread_init)
    WN_INSERT_BlockLast ( parallel_func, lastthread_init );
  if (reduction_init_block)
    WN_INSERT_BlockLast ( parallel_func, reduction_init_block );
    // Maybe do_prefix, do_suffix should be set to NULL at the begining of
    // This function. csc.
//  if (do_prefix)
//    WN_INSERT_BlockLast ( parallel_func, do_prefix );
  WN_INSERT_BlockLast ( parallel_func, stmt_block );
//  if( do_suffix )
//    WN_INSERT_BlockLast( parallel_func, do_suffix );
  if (last_local_if) {
    WN_INSERT_BlockLast(parallel_func, WN_CreateBarrier(FALSE, 0));
    WN_INSERT_BlockLast(parallel_func, last_local_if);
    WN_INSERT_BlockBefore(WN_then(last_local_if), NULL, 
			   WN_CreateBarrier(TRUE,0));
    WN_INSERT_BlockAfter (WN_then(last_local_if), NULL, 
					WN_CreateBarrier(FALSE,0));
    WN_INSERT_BlockLast (parallel_func, WN_CreateBarrier(TRUE, 0));// forward
  }
  if (reduction_store_block)
    WN_INSERT_BlockLast ( parallel_func, reduction_store_block );
  if (copyout_block)
    WN_INSERT_BlockLast ( parallel_func, copyout_block );
  
  /* Generate return at end of parallel function */

  wn = WN_CreateReturn ( );
  WN_linenum(wn) = line_number;
  WN_INSERT_BlockLast ( parallel_func, wn );

  /* Transfer any mappings for nodes moved from parent to parallel function */

  Transfer_Maps ( pmaptab, cmaptab, parallel_func,
		  PU_Info_regions_ptr(Current_PU_Info) );

  /* Create a new dependence graph for the child  and move all the 
     appropriate vertices from the parent to the child graph */

  Current_Map_Tab = cmaptab;
  MP_Fix_Dependence_Graph ( ppuinfo, Current_PU_Info, parallel_func ); 
  Current_Map_Tab = pmaptab;

  /* Create uplevel reference list and mark pu if it contains alloca. */

  pu_has_alloca = FALSE;
  pu_has_region = FALSE;
  WN_TO_BOOL_HASH guarded_set(NUM_HASH_ELEMENTS, Malloc_Mem_Pool);
  Gather_Uplevel_References ( reference_block, func_level, NULL, NULL,
			      parallel_func, &guarded_set );
  if (pu_has_alloca) {
      Set_PU_has_alloca(Get_Current_PU());
  }
  if (pu_has_region) {
      Set_PU_has_region(Get_Current_PU());
  }

  /* Restore parent information. */
  
  CURRENT_SYMTAB = psymtab;
  Current_PU_Info = ppuinfo;
  Current_pu = &Current_PU_Info_pu();
  Current_Map_Tab = pmaptab;
  Pop_Some_Globals( );
} // Process_Parallel_Do()


/*  Process the contents of a parallel region.  */

static void 
Process_Parallel_Region ( void )
{
  WN *wn, *reduction_init_block = NULL, *reduction_store_block = NULL;

  Is_True(mpt == MPP_PARALLEL_REGION, ("not in a PARALLEL region"));

  /* Initialization. */

  psymtab = CURRENT_SYMTAB;
  ppuinfo = Current_PU_Info;
  pmaptab = Current_Map_Tab;

  /* Strip any nested mp stuff due to inlining. */

  Strip_Nested_MP ( stmt_block, TRUE );

  // Gather STs for inner scope VLAs

  Gather_Inner_Scope_Vlas ( stmt_block );

  /* Create and initialize all parent temps that will be needed. */

  Process_Preg_Temps ( stmt_block, TRUE );


  Push_Some_Globals( );
  Create_MicroTask( PAR_FUNC_REGION );
  Delayed_MP_Translation( stmt_block );

  /* Create any needed local temps. */
// Not proper.
//  Make_Local_Temps ( );
// TODO: need to check if any other fixup work need to be done.
//  local_start = mpbase_st;
//  local_ntrip = mptrips_st;
//  thread_info = mpflags_st;
  
  /* Create local variable correspondence table and walk the tree replacing
     references. */

    // lastlocal_nodes are all non-POD lastprivates
  Create_Local_Variables ( var_table, reduction_nodes, lastlocal_nodes,
                           local_nodes, firstprivate_nodes,
			   &firstprivate_block, NULL, &alloca_block );
  Localize_Parent_Stack lps(FALSE, NULL);
  stmt_block = Walk_and_Localize ( stmt_block, var_table, &lps, TRUE , 
                                   &non_pod_finalization_nodes );
#ifdef KEY
  if (LANG_Enable_CXX_Openmp && PU_Info_pu(ppuinfo).misc &&
    PU_src_lang(PU_Info_pu(ppuinfo)) == PU_CXX_LANG)
  { // C++
    Is_True (parallel_proc, ("Parallel block unavailable"));
    PU_IDX pu_idx = ST_pu(parallel_proc);
    PU &pu = Pu_Table[pu_idx];
    // Update typeinfo data present in PU.
    pu.misc = Process_PU_Exc_Info(PU_Info_pu(ppuinfo).misc, var_table);
  }
#endif
  /* Transform contents of parallel region */

  if (copyin_nodes) {
    WN_INSERT_BlockFirst (stmt_block, Gen_MP_Copyin(TRUE));
  }

  Transform_Parallel_Block ( stmt_block );
  Verify_No_MP(stmt_block);
#ifdef KEY
  Gen_Threadpriv_Func(reference_block, parallel_func, FALSE);
#endif

  if (reduction_count)  /* Generate init/finish reduction code */
    Gen_MP_Reduction(var_table, local_count, &reduction_init_block,
                     &reduction_store_block);

  /* Consolidate all portions of nested parallel procedure */

  if (alloca_block)
    WN_INSERT_BlockLast ( parallel_func, alloca_block );
  WN_INSERT_BlockLast ( parallel_func,
		        WN_CreatePragma ( WN_PRAGMA_PREAMBLE_END, ST_IDX_ZERO,
			                  0, 0 ));
  if (firstprivate_block)
    WN_INSERT_BlockLast ( parallel_func, firstprivate_block );
  if (reduction_init_block)
    WN_INSERT_BlockLast ( parallel_func, reduction_init_block );
  WN_INSERT_BlockLast ( parallel_func, stmt_block );
  if (reduction_store_block)
    WN_INSERT_BlockLast ( parallel_func, reduction_store_block );

  /* Generate return at end of parallel function */

  wn = WN_CreateReturn ( );
  WN_linenum(wn) = line_number;
  WN_INSERT_BlockLast ( parallel_func, wn );

  /* Transfer any mappings for nodes moved from parent to parallel function */

  Transfer_Maps ( pmaptab, cmaptab, parallel_func, 
		  PU_Info_regions_ptr(Current_PU_Info) );

  /* Create a new dependence graph for the child  and move all the 
     appropriate vertices from the parent to the child graph */

  Current_Map_Tab = cmaptab;
  MP_Fix_Dependence_Graph ( ppuinfo, Current_PU_Info, parallel_func ); 
  Current_Map_Tab = pmaptab;

  /* Create uplevel reference list and mark pu if it contains alloca. */

  pu_has_alloca = FALSE;
  pu_has_region = FALSE;
  WN_TO_BOOL_HASH guarded_set(NUM_HASH_ELEMENTS, Malloc_Mem_Pool);
  Gather_Uplevel_References ( reference_block, func_level, NULL, NULL,
			      parallel_func, &guarded_set );

  if (pu_has_alloca) {
      Set_PU_has_alloca(Get_Current_PU());
  }
  if (pu_has_region) {
      Set_PU_has_region(Get_Current_PU());
  }


  /* Restore parent information. */
  
  CURRENT_SYMTAB = psymtab;
  Current_PU_Info = ppuinfo;
  Current_pu = &Current_PU_Info_pu();
  Current_Map_Tab = pmaptab;
  Pop_Some_Globals( );
} // Process_Parallel_Region()

// Localize variables in serialzed version of parallel region
static void Localize_in_serialized_parallel (void)
{
  INT32 vsize_l = (local_count + 1) * sizeof(VAR_TABLE);
  VAR_TABLE * var_table_l = (VAR_TABLE *) alloca ( vsize_l );
  BZERO ( var_table_l, vsize_l );
  Create_Local_Variables ( var_table_l, reduction_nodes, lastlocal_nodes,
                           local_nodes, firstprivate_nodes,
			   &firstprivate_block, NULL, &alloca_block );
  Localize_Parent_Stack lps_l(FALSE, NULL);
  serial_stmt_block = Walk_and_Localize ( serial_stmt_block,
                                          var_table_l, &lps_l, FALSE , 
                                          &non_pod_finalization_nodes );
}

/*  This is the main routine used to process parallel do's and parallel
    regions.  It calls support routines to handle the contents of the
    parallel do/region and copyin, but this code does everything else.  */

WN * 
lower_mp ( WN * block, WN * node, INT32 actions )
{
  INT32 i;			/* Temporary index */
  INT32 vsize;			/* Var_table size */
  INT32 lsize;			/* label_info_table size */
  INT32 ssize;			/* Shared_table size */
  INT32 msize;			/* Mpnum_table size */
  BOOL cont;			/* Loop control */
  WN   *wn;			/* Temporary node */
  WN   *temp_node;		/* Temporary node */
  WN   *stmt1_block;		/* If true statement block */
  WN   *stmt2_block;		/* If false statement block */
  WN   *cur_node;		/* Current node within original nodes */
  WN   *next_node;		/* Next node in sequence */
  WN   *return_nodes;		/* Nodes to be returned */
  WN   *fp;			/* Frame pointer uplink */
  WN   *mpsched_wn;		/* Real wn for mpsched node */
  WN   *chunk_wn;		/* Real wn for chunk node */
  WN   *mp_call_wn;		/* Real wn for mp call */
  WN   *if_cond_wn;		/* Real wn for if condition */
  ST   *lock_st;		/* ST for critical section lock */
  ST   *ntrip_st;		/* ST for loop trip count */
  ST   *return_st;		/* ST for mp status return */
  WN_OFFSET ntrip_ofst;		/* Offset for loop trip count */
  WN_OFFSET return_ofst;	/* Offset for mp status return */
  PREG_NUM rreg1, rreg2;	/* Pregs with I4 return values */
  INT32 num_criticals;		/* Number of nested critical sections */
  BOOL  while_seen;		/* While seen where do should be */
  BOOL  mp_if;			/* MP if transformation flag */

  /* Validate input arguments. */

  Is_True(actions & LOWER_MP,
	  ("actions does not contain LOWER_MP"));
  Is_True(((WN_opcode(node) == OPC_PRAGMA) ||
	   (WN_opcode(node) == OPC_XPRAGMA) ||
	   (WN_opcode(node) == OPC_IF) ||
	   (WN_opcode(node) == OPC_REGION)),
	  ("invalid mp node"));

  Is_True(PU_Info_proc_sym(Current_PU_Info) == last_pu_proc_sym,
          ("LowerMP_PU_Init() not called for this PU"));

  /* Special case handling of PU-scope pragmas. */

  if (block == NULL) {

    if ((WN_opcode(node) == OPC_PRAGMA) || (WN_opcode(node) == OPC_XPRAGMA)) {

      if (WN_pragma(node) == WN_PRAGMA_CHUNKSIZE) {
        pu_chunk_node = WN_COPY_Tree ( node );

      } else if (WN_pragma(node) == WN_PRAGMA_MPSCHEDTYPE) {
        pu_mpsched_node = WN_COPY_Tree ( node );
      }

    }

    return (NULL);

  }

  /* Initialization. */

  copyin_count       = 0;
  local_count        = 0;
  reduction_count    = 0;
  shared_count       = 0;
  num_constructs     = 0;
  num_criticals      = 0;
  affinity_nodes     = NULL;
  affinity_d_nodes   = NULL;
  affinity_t_nodes   = NULL;
  chunk_node         = NULL;
  copyin_nodes       = NULL;
  copyin_nodes_end   = NULL;
  if_node            = NULL;
  lastlocal_nodes    = NULL;
  lastthread_node    = NULL;
  local_nodes        = NULL;
  firstprivate_nodes = NULL;
  non_pod_finalization_nodes = NULL;
  non_pod_first_and_lastprivate = FALSE;
  mpnum_node         = NULL;
  mpsched_node       = NULL;
  numthreads_node    = NULL;
  ordered_node       = NULL;
  reduction_nodes    = NULL;
  shared_nodes       = NULL;
  stmt_block         = NULL;
  replace_block      = NULL;
  cont_nodes         = NULL;
  do_node            = NULL;
  ntrip_calc         = NULL;
  livein_block       = NULL;
  alloca_block       = NULL;
  copyin_block       = NULL;
  copyout_block      = NULL;
  firstprivate_block = NULL;
  liveout_block      = NULL;
  if_cond_wn         = NULL;
  if_preamble_block  = NULL;
  if_postamble_block = NULL;
  do_preamble_block  = NULL;
  serial_stmt_block  = NULL;
  fast_doacross      = FALSE;
  while_seen         = FALSE;
  mp_if              = FALSE;
  do_order_lb        = NULL;
  do_order_stride    = NULL;
  inside_versioning_if = FALSE;
  comp_gen_construct = FALSE;
  mpt                = MPP_UNKNOWN;
  csymtab            = SYMTAB_IDX_ZERO;
  psymtab            = SYMTAB_IDX_ZERO;
  ppuinfo            = NULL;
#ifdef KEY
  reference_block    = NULL;
#endif

  //Create_Loc_ST();

  inner_scope_vla.clear();

    // mempool initialization
  if (first_call) {
    MEM_POOL_Initialize(&mp_lower_pool, "MP Lowering Pool", TRUE);
    first_call = FALSE;
  }
  MEM_POOL_Popper popper(&mp_lower_pool);

  lsize = sizeof(LABEL_INFO_TABLE) * LABEL_Table_Size(CURRENT_SYMTAB);
  label_info_table = (LABEL_INFO_TABLE *) alloca ( lsize );
  BZERO ( label_info_table, lsize );

  ssize = 4096 * sizeof(SHARED_TABLE);
  shared_table = (SHARED_TABLE *) alloca ( ssize );
  BZERO ( shared_table, ssize );

  if (mpid_size == 0) {
    mpid_size = 1028;
    msize = mpid_size * sizeof(MPID_TABLE);
    mpid_table = (MPID_TABLE *) malloc ( msize );
    BZERO ( mpid_table, msize );
  }

  pu_has_eh = PU_has_exc_scopes(Get_Current_PU());

  /* Determine processing required based on first node. */

  start_processing:

  if ((WN_opcode(node) == OPC_PRAGMA) || (WN_opcode(node) == OPC_XPRAGMA)) {

    if (WN_pragma(node) == WN_PRAGMA_BARRIER) {

      // orphaned barrier, need to setup right gtid
      // csc. 2003/1/7
      // note, the gtid is not set properly.
      WN *call, *store_gtid;
      wn = WN_next(node);
      // does this work?
      if (!local_gtid)
      {
      	store_gtid = Gen_Store_Gtid();
      }
      else
      {
	store_gtid = WN_CreateBlock();
      }
      call = Gen_Barrier(local_gtid);
      WN_INSERT_BlockLast( store_gtid, call );
      WN_next(call) = wn;
      if (wn) WN_prev(wn) = call;
      WN *return_wn = WN_first( store_gtid );
      WN_DELETE_Tree(node);
      WN_Delete( store_gtid );
      return return_wn;

    } else if (WN_pragma(node) == WN_PRAGMA_CHUNKSIZE) {
      pu_chunk_node = node;
      return (WN_next(node));

    } else if (WN_pragma(node) == WN_PRAGMA_MPSCHEDTYPE) {
      pu_mpsched_node = node;
      return (WN_next(node));

    } else if (WN_pragma(node) == WN_PRAGMA_COPYIN) {

      mpt = MPP_COPYIN;
      next_node = node;

    } else if (WN_pragma(node) == WN_PRAGMA_COPYIN_BOUND) {

      wn = WN_next(node);
      WN_DELETE_Tree ( node );
      return (wn);

    } else if (WN_pragma(node) == WN_PRAGMA_CRITICAL_SECTION_BEGIN) {

      mpt = MPP_CRITICAL_SECTION;
      next_node = node;

    } else if (WN_pragma(node) == WN_PRAGMA_ORDERED_BEGIN) {
      // Why this is not been organized as a region?
      // orphaned ordered, need to set up gtid.
      // should Ordered/End_Ordered be lowered in DO/PDO?

      WN *call;
      wn = WN_next(node);
      // Need to fix gtid.
      WN *store_gtid = Gen_Store_Gtid();
      call = Gen_Ordered(local_gtid);
      WN_INSERT_BlockLast( store_gtid, call );
      WN_next(call) = wn;
      if (wn) WN_prev(wn) = call;
      WN *return_wn = WN_first( store_gtid );
      WN_DELETE_Tree(node);
      WN_Delete( store_gtid );
      return return_wn;

    } else if (WN_pragma(node) == WN_PRAGMA_ORDERED_END) {

      WN *call;
      wn = WN_next(node);
      //local_gtid should have been set up
      Is_True(local_gtid != NULL, ("unmatched END_ORDERED"));
      call = Gen_End_Ordered(local_gtid);
      WN_next(call) = wn;
      if (wn) WN_prev(wn) = call;
      WN_DELETE_Tree(node);
      return call;

    } else

      Fail_FmtAssertion (
	      "out of context pragma (%s) in MP {standalone pragma} processing",
	      WN_pragmas[WN_pragma(node)].name);

  } else if ((WN_opcode(node) == OPC_REGION) &&
	     WN_first(WN_region_pragmas(node)) &&
	     ((WN_opcode(WN_first(WN_region_pragmas(node))) == OPC_PRAGMA) ||
	      (WN_opcode(WN_first(WN_region_pragmas(node))) == OPC_XPRAGMA))) {

    WN *wtmp = WN_first(WN_region_pragmas(node));
    WN_PRAGMA_ID wid = (WN_PRAGMA_ID) WN_pragma(wtmp);

    comp_gen_construct = ( WN_pragma_compiler_generated(
                             WN_first(WN_region_pragmas(node))) != 0 );

    switch (wid) {
      /* orphaned SINGLE region: process it now and return */
    case WN_PRAGMA_SINGLE_PROCESS_BEGIN:
      ++num_constructs;
      mpt = MPP_ORPHANED_SINGLE;
      Strip_Nested_MP(WN_region_body(node), FALSE);
      wn = Gen_MP_SingleProcess_Region(node);
      if ((WN_next(wn) = WN_next(node)) != NULL)
        WN_prev(WN_next(wn)) = wn;
      WN_DELETE_Tree(WN_region_pragmas(node));
      WN_DELETE_Tree(WN_region_exits(node));
      RID_Delete(Current_Map_Tab, node);
      WN_Delete(node);
      return wn;

    case WN_PRAGMA_MASTER_BEGIN:
      mpt = MPP_ORPHANED_MASTER;
      wn = Lower_Master( node );
      if ((WN_next(wn) = WN_next(node)) !=NULL)
        WN_prev(WN_next(wn)) = wn;
      WN_DELETE_Tree(WN_region_pragmas(node));
      WN_DELETE_Tree(WN_region_exits(node));
      RID_Delete(Current_Map_Tab, node);
      WN_Delete(node);
      return wn;

    case WN_PRAGMA_DOACROSS:
    case WN_PRAGMA_PARALLEL_DO:
      mpt = MPP_PARALLEL_DO;
      break;

    case WN_PRAGMA_PDO_BEGIN:
      mpt = MPP_ORPHANED_PDO;
      break;

    case WN_PRAGMA_PARALLEL_BEGIN:
#ifdef KEY
    case WN_PRAGMA_PARALLEL_WORKSHARE:
#endif
      mpt = MPP_PARALLEL_REGION;
      break;

    default:
      printf("pragma value = %d", (int)wid); /* for test. by jhs,02.9.3 */
      Fail_FmtAssertion (
		 "out of context pragma (%s) in MP {primary pragma} processing",
		 WN_pragmas[wid].name);
    }

    next_node = WN_next(wtmp);
    cont_nodes = WN_next(node);
    stmt_block = WN_region_body(node);

    if (mpt != MPP_ORPHANED_PDO) {
      WN_Delete ( wtmp );
      WN_Delete ( WN_region_pragmas(node) );
      WN_DELETE_Tree ( WN_region_exits(node) );
    }

  } else if ((WN_opcode(node) == OPC_IF) && WN_Is_If_MpVersion(node)) {

    inside_versioning_if = TRUE;
    mp_if = TRUE;
    if_cond_wn = WN_if_test(node);
    next_node = WN_first(WN_then(node));
    serial_stmt_block = WN_else(node);

    while (next_node && (WN_opcode(next_node) != OPC_REGION)) {
      if (if_preamble_block == NULL)
	if_preamble_block = WN_CreateBlock ( );
      wn = WN_next(next_node);
      WN_INSERT_BlockLast ( if_preamble_block, next_node );
      next_node = wn;
    }

    if (next_node == NULL)
      Fail_FmtAssertion (
			 "missing versioned parallel region in MP processing" );

    temp_node = WN_next(next_node);
    while (temp_node) {
      if (if_postamble_block == NULL)
      	if_postamble_block = WN_CreateBlock ( );
      wn = WN_next(temp_node);
      WN_INSERT_BlockLast ( if_postamble_block, temp_node );
      temp_node = wn;
    }

    WN_next(next_node) = WN_next(node);
    WN_Delete ( WN_then(node) );
    WN_Delete ( node );

    node = next_node;
    goto start_processing;

  } else

    Fail_FmtAssertion ( "out of context node (%s) in MP {top-level} processing",
			OPCODE_name(WN_opcode(node)) );

  /* Process all mp pragma clauses and accumulate statements as appropriate. */

    // Process_PDO() parses clauses of orphanded PDO
  cont = (mpt != MPP_ORPHANED_PDO);

  while (cont && (cur_node = next_node)) {

    next_node = WN_next(cur_node);

    if (((WN_opcode(cur_node) == OPC_PRAGMA) ||
         (WN_opcode(cur_node) == OPC_XPRAGMA)) &&
        (WN_pragmas[WN_pragma(cur_node)].users & PUSER_MP)) {

      if (mpt == MPP_COPYIN) {

	if (WN_pragma(cur_node) == WN_PRAGMA_COPYIN) {
	  WN_prev(cur_node) = copyin_nodes_end;
	  WN_next(cur_node) = NULL;
	  if (copyin_nodes == NULL)
	    copyin_nodes_end = copyin_nodes = cur_node;
	  else
	    copyin_nodes_end = WN_next(copyin_nodes_end) = cur_node;
	  ++copyin_count;
	} else {
	  cont_nodes = cur_node;
	  cont = FALSE;
	}

      } else if (mpt == MPP_CRITICAL_SECTION) {

	if (WN_pragma(cur_node) == WN_PRAGMA_CRITICAL_SECTION_BEGIN) {
	  if ((num_criticals++) == 0) {
	    stmt_block = WN_CreateBlock ( );
	    WN_first(stmt_block) = next_node;
	    if (next_node)
	      WN_prev(next_node) = NULL;
	  }
	} else if (WN_pragma(cur_node) == WN_PRAGMA_CRITICAL_SECTION_END) {
	  if ((--num_criticals) == 0) {
	    WN_last(stmt_block) = WN_prev(cur_node);
	    if (WN_prev(cur_node))
	      WN_next(WN_prev(cur_node)) = NULL;
	    else
	      WN_first(stmt_block) = NULL;
	    cont_nodes = next_node;
	    cont = FALSE;
	  }
	}

      } else {

	switch (WN_pragma(cur_node)) {

	  case WN_PRAGMA_AFFINITY:
	    WN_next(cur_node) = affinity_nodes;
	    affinity_nodes = cur_node;
	    break;

	  case WN_PRAGMA_DATA_AFFINITY:
	    WN_next(cur_node) = affinity_d_nodes;
	    affinity_d_nodes = cur_node;
	    break;

	  case WN_PRAGMA_THREAD_AFFINITY:
	    WN_next(cur_node) = affinity_t_nodes;
	    affinity_t_nodes = cur_node;
	    break;

	  case WN_PRAGMA_CHUNKSIZE:
	    if (chunk_node)
	      WN_DELETE_Tree ( chunk_node );
	    chunk_node = cur_node;
	    break;

	  case WN_PRAGMA_IF:
	    if (if_node)
	      WN_DELETE_Tree ( if_node );
	    if_node = cur_node;
	    break;

	  case WN_PRAGMA_LASTLOCAL:
	    for (wn = lastlocal_nodes; wn; wn = WN_next(wn))
	      if (Identical_Pragmas(cur_node, wn))
		break;
	    if (wn == NULL) {
	      WN_next(cur_node) = lastlocal_nodes;
	      lastlocal_nodes = cur_node;
	      ++local_count;
	      if (TY_kind(ST_type(WN_st(cur_node))) == KIND_SCALAR)
		shared_table[shared_count++] = WN_st(cur_node);
	    } else
	      WN_Delete ( cur_node );
	    break;

	  case WN_PRAGMA_LASTTHREAD:
	    if (lastthread_node)
	      WN_Delete ( lastthread_node );
	    lastthread_node = cur_node;
	    break;

	  case WN_PRAGMA_LOCAL:
	    for (wn = local_nodes; wn; wn = WN_next(wn))
	      if (Identical_Pragmas(cur_node, wn))
		break;
	    if (wn == NULL) {
              if (ST_Has_Dope_Vector(WN_st(cur_node))) {
                WN_next(cur_node) = firstprivate_nodes;
                firstprivate_nodes = cur_node;
              } else {
                WN_next(cur_node) = local_nodes;
                local_nodes = cur_node;
              }
	      ++local_count;
	    } else
	      WN_Delete ( cur_node );
	    break;

	  case WN_PRAGMA_FIRSTPRIVATE:
	    for (wn = firstprivate_nodes; wn; wn = WN_next(wn))
	      if (Identical_Pragmas(cur_node, wn))
		break;
	    if (wn == NULL) {
	      WN_next(cur_node) = firstprivate_nodes;
	      firstprivate_nodes = cur_node;
	      ++local_count;
	    } else
	      WN_Delete ( cur_node );
	    break;

	  case WN_PRAGMA_MPNUM: 
	    if (mpnum_node)
	      WN_Delete ( mpnum_node );
	    mpnum_node = cur_node;
	    break;

	  case WN_PRAGMA_MPSCHEDTYPE:
	    if (mpsched_node)
	      WN_Delete ( mpsched_node );
	    mpsched_node = cur_node;
	    break;

	  case WN_PRAGMA_NUMTHREADS:
	    if (numthreads_node)
	      WN_DELETE_Tree ( numthreads_node );
	    numthreads_node = cur_node;
	    break;

	  case WN_PRAGMA_ORDERED:
	    if (ordered_node)
	      WN_Delete ( ordered_node );
	    ordered_node = cur_node;
	    break;

        case WN_PRAGMA_ORDERED_LOWER_BOUND:
          if (do_order_lb) WN_Delete (do_order_lb);
          do_order_lb = cur_node;
          break;

        case WN_PRAGMA_ORDERED_STRIDE:
          if (do_order_stride) WN_Delete (do_order_stride);
          do_order_stride = cur_node;
          break;

	  case WN_PRAGMA_PARALLEL_END: 
	  case WN_PRAGMA_END_MARKER:
	    break;

	  case WN_PRAGMA_REDUCTION:
	    for (wn = reduction_nodes; wn; wn = WN_next(wn))
	      if (Identical_Pragmas(cur_node, wn))
		break;
	    if (wn == NULL) {
              if (WN_opcode(cur_node) != OPC_PRAGMA &&
                WN_operator(WN_kid0(cur_node)) == OPR_ARRAY &&
                OPCODE_has_sym(WN_opcode(WN_kid0(WN_kid0(cur_node)))) == 0) {
                WN_DELETE_Tree ( cur_node );
              } else {
                WN_next(cur_node) = reduction_nodes;
                reduction_nodes = cur_node;
	        ++local_count;
	        ++reduction_count;
	        if (WN_opcode(cur_node) == OPC_PRAGMA)
		  shared_table[shared_count++] = WN_st(cur_node);
              }
	    } else
	      WN_DELETE_Tree ( cur_node );
	    break;

	  case WN_PRAGMA_SHARED:
	    for (wn = shared_nodes; wn; wn = WN_next(wn))
	      if (Identical_Pragmas(cur_node, wn))
		break;
	    if (wn == NULL) {
	      WN_next(cur_node) = shared_nodes;
	      shared_nodes = cur_node;
	      if (TY_kind(ST_type(WN_st(cur_node))) == KIND_SCALAR)
		shared_table[shared_count++] = WN_st(cur_node);
	    } else
	      WN_Delete ( cur_node );
	    break;

        case WN_PRAGMA_COPYIN:
          FmtAssert (WN_pragma_omp(cur_node), ("COPYIN clause must be OMP"));
	  WN_prev(cur_node) = copyin_nodes_end;
	  WN_next(cur_node) = NULL;
	  if (copyin_nodes == NULL)
	    copyin_nodes_end = copyin_nodes = cur_node;
	  else
	    copyin_nodes_end = WN_next(copyin_nodes_end) = cur_node;
	  ++copyin_count;
          break;
        
	case WN_PRAGMA_DEFAULT:
	  break;
	  
	  default:
	    Fail_FmtAssertion (
	       "out of context pragma (%s) in MP {top-level pragma} processing",
	       WN_pragmas[WN_pragma(cur_node)].name);

	}

      }

    } else if (mpt == MPP_COPYIN) {

      cont_nodes = cur_node;
      cont = FALSE;

    } else if (mpt == MPP_CRITICAL_SECTION) {

      /* just accumulate nodes */

#ifdef KEY // Bug 6273 - apsi basecompile with -apo triggers this case.
    } else if (mpt == MPP_PARALLEL_DO) {

      /* just accumulate nodes */

    } else if (mpt == MPP_PARALLEL_REGION) { // bug 6638

      /* just accumulate nodes */
#endif
    } else

      Fail_FmtAssertion (
		     "out of context node (%s) in MP {continuation} processing",
		     OPCODE_name(WN_opcode(cur_node)) );

  }

  if (num_criticals != 0)
    Fail_FmtAssertion (
		     "missing pragma (CRITICAL_SECTION_END) in MP processing" );


  /* Process all parallel constructs and generate replacement code. */

  if (cont_nodes)
    WN_prev(cont_nodes) = NULL;

  replace_block = WN_CreateBlock ( );

  if (mpt == MPP_COPYIN) {

    line_number = WN_linenum(node);
    Set_Error_Line(line_number);

    WN_INSERT_BlockLast ( replace_block, Gen_MP_Copyin ( FALSE ) );

  } else if (mpt == MPP_CRITICAL_SECTION) {

    line_number = WN_linenum(node);
    Set_Error_Line(line_number);

    if (WN_opcode(node) == OPC_PRAGMA &&
        WN_pragma_omp(node) &&
        WN_st(node)) {
      lock_st = Get_NameLock_ST(WN_st(node));
    }
    else if ((WN_opcode(node) == OPC_XPRAGMA) &&
             (WN_operator(WN_kid0(node)) == OPR_LDA))
      lock_st = WN_st(WN_kid0(node));
    else if ((WN_opcode(node) == OPC_PRAGMA) && WN_st(node))
      lock_st = WN_st(node);
    else{
      lock_st = NULL;
      Create_Unnamed_Critical_Lock( );
    }

    if (lock_st) {
        // redundant, but marks where we need to deal with this in future
        // also the line number for the unlock isn't set correctly
      Linenum_Pusher p(WN_Get_Linenum(node));
      WN_INSERT_BlockLast ( replace_block, Gen_Store_Gtid());
      WN_INSERT_BlockLast ( replace_block, Gen_Critical (local_gtid, lock_st));
      Strip_Nested_MP ( stmt_block, FALSE );
      WN_INSERT_BlockLast ( replace_block, stmt_block );
      WN_INSERT_BlockLast ( replace_block, Gen_End_Critical (local_gtid, lock_st));
    } else {
      Linenum_Pusher p(WN_Get_Linenum(node));
      WN_INSERT_BlockLast (replace_block, Gen_Store_Gtid());
      WN_INSERT_BlockLast (replace_block, Gen_Critical (local_gtid, unnamed_lock_st));
      Strip_Nested_MP ( stmt_block, FALSE );
      WN_INSERT_BlockLast ( replace_block, stmt_block );
      WN_INSERT_BlockLast ( replace_block, Gen_End_Critical (local_gtid, unnamed_lock_st));
    }

    WN_DELETE_Tree ( node );
    WN_Delete ( cur_node );

  } else if (mpt == MPP_PARALLEL_DO) {

    BOOL is_omp = WN_pragma_omp(WN_first(WN_region_pragmas(node)));

    next_node = WN_first(stmt_block);
    while ((do_node = next_node) && (WN_opcode(do_node) != OPC_DO_LOOP)) {
      next_node = WN_next(do_node);
      if ((WN_opcode(do_node) == OPC_DO_WHILE) ||
          (WN_opcode(do_node) == OPC_WHILE_DO))
            while_seen = TRUE;
      if (do_preamble_block == NULL)
        do_preamble_block = WN_CreateBlock ( );
      WN_INSERT_BlockLast ( do_preamble_block, do_node );
    }
#ifdef KEY //Bug 4660
    WN *cur_node;
    if (do_preamble_block && lastlocal_nodes){
      cur_node = WN_first(do_preamble_block);
      while (cur_node){
        next_node = WN_next(cur_node);
        if (WN_operator(cur_node) == OPR_STID &&
            ST_class(WN_st(cur_node)) != CLASS_PREG &&
            WN_st(cur_node) == WN_st(lastlocal_nodes))
          WN_DELETE_FromBlock(do_preamble_block, cur_node);
        cur_node = next_node;
      }
    }
#endif

    /* Do loop not found in spite of doacross node. Serialize any code in
       parallel region. */

    if (do_node == NULL) {
      if (!while_seen) {
        WN_Delete ( stmt_block );
        RID_Delete ( Current_Map_Tab, node );
        WN_Delete ( node );
        local_count = 0;
        WN_INSERT_BlockLast ( replace_block, do_preamble_block );
        goto finish_processing;
      } else
        Fail_FmtAssertion
		  ("parallel DO was converted to WHILE and not converted back");
    }

      /* Note: front end can put code for finalizing the loop index after DO
         but inside the parallel region */

    
    WN_first(stmt_block) = do_node;
    WN_prev(do_node) = NULL;
    

    line_number = WN_linenum(do_node);
    Set_Error_Line(line_number);

    do_index_st = WN_st(WN_index(do_node));

    do_index_type = TY_mtype(ST_type(do_index_st));
    if (do_index_type == MTYPE_I1 || do_index_type == MTYPE_I2)
      do_index_type = MTYPE_I4;
    else if (do_index_type == MTYPE_U1 || do_index_type == MTYPE_U2)
      do_index_type = MTYPE_U4;

#if defined(TARG_X8664) || defined(TARG_MIPS)
    // Bug 7275 - this depends on the library implementation
    if (MTYPE_byte_size(do_index_type) == 4)
#else 
    if (do_index_type == MTYPE_I4)
#endif
    {
/*      if (lastthread_node)
        fast_doacross = TRUE;
      else if (ordered_node)
        fast_doacross = FALSE;
      else if (mpsched_node)
        fast_doacross =
           (WN_pragma_arg1(mpsched_node) == WN_PRAGMA_SCHEDTYPE_SIMPLE);
      else if (pu_mpsched_node)
        fast_doacross =
            (WN_pragma_arg1(pu_mpsched_node) == WN_PRAGMA_SCHEDTYPE_SIMPLE);
      else if (chunk_node || pu_chunk_node)
        fast_doacross = FALSE;
      else
*/        fast_doacross = TRUE;
    }
    else
      fast_doacross = FALSE;

    vsize = (local_count + 1) * sizeof(VAR_TABLE);
    var_table = (VAR_TABLE *) alloca ( vsize );
    BZERO ( var_table, vsize );

    fp = WN_LdidPreg ( Pointer_type, Frame_Pointer_Preg_Offset );

    if (serial_stmt_block)
      Strip_Nested_MP ( serial_stmt_block, FALSE );
    else
      serial_stmt_block = Copy_Non_MP_Tree ( stmt_block );

    // the do_preamble code should also be inserted into serial_stmt.
    // csc.

    if( do_preamble_block )
    {
      WN_INSERT_BlockAfter( serial_stmt_block, NULL, 
                            WN_COPY_Tree( do_preamble_block ));
    }


//    do_preamble_block = NULL;

    if (is_omp) {
      WN *nest_wn;
      WN *store_gtid;
      
      // TODO: gtid should be passed as an argument to both GSP/GESP
      store_gtid = Gen_Store_Gtid();
#ifdef KEY
      ntrip_calc = WN_CreateBlock ( );
      WN *wtree = WN_first(serial_stmt_block);
      while (WN_operator(wtree) != OPR_DO_LOOP){
        WN_INSERT_BlockLast(ntrip_calc, WN_COPY_Tree(wtree));
        wtree = WN_next(wtree);
      }
#endif

      nest_wn = Gen_Serialized_Parallel( local_gtid);
      WN_INSERT_BlockAfter (serial_stmt_block, NULL, nest_wn);

      WN_INSERT_BlockAfter( serial_stmt_block, NULL, store_gtid );
      nest_wn = Gen_End_Serialized_Parallel (local_gtid);
      WN_INSERT_BlockBefore (serial_stmt_block, NULL, nest_wn);
    }

    Process_Parallel_Do ( );

    /* do this always -- serial portion may have had a begin/end ordered */
    Cleanup_Ordered (serial_stmt_block, serial_stmt_block);

    Create_Preg_or_Temp ( do_index_type, "trip_count", &ntrip_st, &ntrip_ofst );
    temp_node = WN_Stid ( do_index_type, ntrip_ofst, ntrip_st, 
		    ST_type(ntrip_st), ntrip_node );
    WN_linenum(temp_node) = line_number;
#ifdef KEY
    if (!ntrip_calc)
#endif
    ntrip_calc = WN_CreateBlock ( );
    WN_INSERT_BlockLast ( ntrip_calc, temp_node );
    ntrip_node = WN_Ldid ( do_index_type, ntrip_ofst, ntrip_st,
			   ST_type(ntrip_st) );

    if( numthreads_node )
    {
    	mp_call_wn = Gen_Fork ( parallel_proc, 
          	WN_COPY_Tree ( WN_kid0 ( numthreads_node ))); 
    }
    else
    {
    	mp_call_wn = Gen_Fork ( parallel_proc, NULL);
    }

    RID_Delete ( Current_Map_Tab, node );
    WN_Delete ( node );

  } else if (mpt == MPP_ORPHANED_PDO) {

    next_node = WN_first(stmt_block);
    while ((do_node = next_node) && (WN_opcode(do_node) != OPC_DO_LOOP)) {
      next_node = WN_next(do_node);
      if ((WN_opcode(do_node) == OPC_DO_WHILE) ||
	  (WN_opcode(do_node) == OPC_WHILE_DO))
	while_seen = TRUE;
      if (do_preamble_block == NULL)
	do_preamble_block = WN_CreateBlock ( );
      WN_INSERT_BlockLast ( do_preamble_block, do_node );
    }

    if (do_node == NULL) {
      if (!while_seen) {
          /* Do loop not found in spite of PDO node. Serialize all the code
             we did find. */
	WN_Delete ( stmt_block );
	WN_DELETE_Tree ( WN_region_pragmas(node) );
	WN_DELETE_Tree ( WN_region_exits(node) );
	RID_Delete ( Current_Map_Tab, node );
	WN_Delete ( node );
	local_count = 0;
	WN_INSERT_BlockLast ( replace_block, do_preamble_block );
	goto finish_processing;
      } else
        Fail_FmtAssertion
		  ("PDO was converted to WHILE and not converted back");
    }

    WN_first(stmt_block) = do_node;
    WN_prev(do_node) = NULL;

    line_number = WN_linenum(do_node);
    Set_Error_Line(line_number);

    Strip_Nested_MP(stmt_block, FALSE);
    if (serial_stmt_block) {
      Is_True(inside_versioning_if,
              ("where did serial_stmt_block come from???"));
      Move_Non_POD_Finalization_Code(serial_stmt_block);
    }
    Process_PDO(node);
    stmt_block = WN_region_body(node);

    WN_Delete ( WN_region_pragmas(node) );
    WN_DELETE_Tree ( WN_region_exits(node) );
    RID_Delete ( Current_Map_Tab, node );
    WN_Delete ( node );

  } else if (mpt == MPP_PARALLEL_REGION) {

    BOOL is_omp = WN_pragma_omp(WN_first(WN_region_pragmas(node)));

    line_number = WN_linenum(node);
    Set_Error_Line(line_number);

    vsize = (local_count + 1) * sizeof(VAR_TABLE);
    var_table = (VAR_TABLE *) alloca ( vsize );
    BZERO ( var_table, vsize );

    fp = WN_LdidPreg ( Pointer_type, Frame_Pointer_Preg_Offset );

    if (serial_stmt_block) {
      Is_True(inside_versioning_if,
              ("where did serial_stmt_block come from???"));
      Move_Non_POD_Finalization_Code(serial_stmt_block);
      Strip_Nested_MP ( serial_stmt_block, FALSE );
    } else
      serial_stmt_block = Copy_Non_MP_Tree ( stmt_block );


    Process_Parallel_Region ( );

#ifndef KEY // bug 7281
    if (is_omp) {
#endif
      WN *nest_wn;

      if( numthreads_node )
      {
    	mp_call_wn = Gen_Fork ( parallel_proc, 
          	WN_COPY_Tree ( WN_kid0 ( numthreads_node ))); 
      }
      else
      {
    	mp_call_wn = Gen_Fork ( parallel_proc, NULL);
      }
      /* now adjust serial_stmt_block with nested_region calls */
      // TODO: gtid should be passed as an argument
      //WN_INSERT_BlockAfter( serial_stmt_block, NULL,
      WN *store_gtid = Gen_Store_Gtid();
      nest_wn = Gen_Serialized_Parallel (local_gtid);
      WN_INSERT_BlockAfter (serial_stmt_block, NULL, nest_wn);
      WN_INSERT_BlockAfter (serial_stmt_block, NULL, store_gtid);
      /* unfortunately we don't know whether there really is an ordered PDO
       * inside or not.
       */
      Cleanup_Ordered (serial_stmt_block, serial_stmt_block);
      // TODO: gtid should be passed as an argument
      nest_wn = Gen_End_Serialized_Parallel (local_gtid);
      WN_INSERT_BlockBefore (serial_stmt_block, NULL, nest_wn);
#ifndef KEY // bug 7281
    }
    else 
    {
	// for LNO generated MP directives.
	// currently not support. 
	// TODO: no-omp directives handling. to enable auto-par
	// csc.
    }
#endif

    RID_Delete ( Current_Map_Tab, node );
    WN_Delete ( node );

  }

  /* Build final code for parallel loops and regions. */

  if ((mpt == MPP_PARALLEL_DO) || (mpt == MPP_PARALLEL_REGION)) {

    // note that now stmt1_block is merely parallel block
    stmt1_block = WN_CreateBlock ( );
    if (mp_if) {
      if (if_preamble_block)
        WN_INSERT_BlockLast ( stmt1_block, if_preamble_block );
      if (do_preamble_block)
        WN_INSERT_BlockLast ( stmt1_block, do_preamble_block );
      if (ntrip_calc)
        WN_INSERT_BlockLast ( stmt1_block, ntrip_calc );
    }
    if (livein_block)
      WN_INSERT_BlockLast ( stmt1_block, livein_block );
    // Push_Thread number code should be inserted here.
    // Note that, if nested parallelism should be supported,
    // A valid gtid node should be generated other than NULL.
/*    if( numthreads_node )
    {
      WN_INSERT_BlockLast( stmt1_block, 
          Set_Thread_Num( WN_COPY_Tree( WN_kid0( numthreads_node )))); 
    }
*/

    WN_INSERT_BlockLast( stmt1_block, mp_call_wn );
	
    if (liveout_block) {
//      wn = WN_CreateIf ( WN_Ldid ( MTYPE_I4, return_ofst, return_st,
//				   ST_type(return_st) ),
//			 WN_CreateBlock ( ), liveout_block );
//      WN_linenum(wn) = line_number;
      WN_INSERT_BlockLast ( stmt1_block, liveout_block );
    }
    if (mp_if && if_postamble_block)
      WN_INSERT_BlockLast ( stmt1_block, if_postamble_block );

    if (!mp_if) {
      if ((do_preamble_block) && (mpt != MPP_PARALLEL_DO ))
	      WN_INSERT_BlockLast ( replace_block, do_preamble_block );
      if (ntrip_calc)
	      WN_INSERT_BlockLast ( replace_block, ntrip_calc );
      if (if_node)
	      if_cond_wn = WN_COPY_Tree ( WN_kid0(if_node) );
      if (mpt == MPP_PARALLEL_DO) {
           // The threshold can be more proper(other than 1). csc
	      temp_node = WN_GT ( do_index_type, WN_Ldid ( do_index_type, ntrip_ofst,
		                  ntrip_st, ST_type(ntrip_st) ), 
                                  WN_Intconst ( do_index_type, 1 ));
	      if (if_cond_wn)
	        if_cond_wn = WN_CAND ( if_cond_wn, temp_node );
	      else
		if_cond_wn = temp_node;
      }

#ifndef KEY
      ST *in_parallel_st;
      WN_OFFSET in_parallel_ofst;
      Create_Preg_or_Temp( MTYPE_I4, "__ompv_in_parallel",
                          &in_parallel_st, &in_parallel_ofst);
      WN_INSERT_BlockLast( replace_block, Gen_In_Parallel( ));
      GET_RETURN_PREGS(rreg1, rreg2, MTYPE_I4);
      temp_node = Gen_MP_Store( in_parallel_st, in_parallel_ofst,
		         WN_LdidPreg( MTYPE_I4, rreg1 ));
      WN_linenum(temp_node) = line_number;
      WN_INSERT_BlockLast( replace_block, temp_node );
#endif

      ST *ok_to_fork_st;
      WN_OFFSET ok_to_fork_ofst;
      Create_Preg_or_Temp( MTYPE_I4, "__ompv_ok_to_fork",
                          &ok_to_fork_st, &ok_to_fork_ofst);
#ifndef KEY // bug 7772
      WN_INSERT_BlockLast( replace_block, Gen_Can_Fork( ));
      GET_RETURN_PREGS(rreg1, rreg2, MTYPE_I4);
      temp_node = Gen_MP_Store( ok_to_fork_st, ok_to_fork_ofst,
		         WN_LdidPreg( MTYPE_I4, rreg1 ));
      WN_linenum(temp_node) = line_number;
      WN_INSERT_BlockLast( replace_block, temp_node );

#ifdef KEY
      temp_node = WN_EQ(   MTYPE_I4, 
			   Gen_MP_Load( ok_to_fork_st, ok_to_fork_ofst ),
			   WN_CreateIntconst(OPC_I4INTCONST, 1));
#else
      temp_node = WN_CAND( WN_EQ( MTYPE_I4, 
			   Gen_MP_Load( in_parallel_st, in_parallel_ofst ),
                           WN_CreateIntconst(OPC_I4INTCONST, 0)),
		    WN_EQ( MTYPE_I4, 
			   Gen_MP_Load( ok_to_fork_st, ok_to_fork_ofst ),
			   WN_CreateIntconst(OPC_I4INTCONST, 1)));
#endif

      // Is this needed ? csc

      if (if_cond_wn)
        if_cond_wn = WN_CAND ( if_cond_wn, temp_node );
      else
        if_cond_wn = temp_node;

#else // bug 7772
      if (if_cond_wn == NULL)
	if_cond_wn = WN_CreateIntconst(OPC_I4INTCONST, 1);
      temp_node = Gen_MP_Store(ok_to_fork_st, ok_to_fork_ofst, if_cond_wn);
      WN_INSERT_BlockLast( replace_block, temp_node );

      WN *canfork_block = WN_CreateBlock ( );
      WN_INSERT_BlockLast( canfork_block, Gen_Can_Fork( ));
      GET_RETURN_PREGS(rreg1, rreg2, MTYPE_I4);
      temp_node = Gen_MP_Store( ok_to_fork_st, ok_to_fork_ofst,
		         WN_LdidPreg( MTYPE_I4, rreg1 ));
      WN_linenum(temp_node) = line_number;
      WN_INSERT_BlockLast( canfork_block, temp_node );

      temp_node = WN_CreateIf(Gen_MP_Load(ok_to_fork_st, ok_to_fork_ofst),
			      canfork_block, WN_CreateBlock());
      WN_INSERT_BlockLast( replace_block, temp_node );

      if_cond_wn = Gen_MP_Load(ok_to_fork_st, ok_to_fork_ofst);
#endif
    }

    // stmt2_block should be the serial one.
      stmt2_block = serial_stmt_block;
//    stmt2_block = WN_CreateBlock ( );
    
//    wn = WN_Stid ( MTYPE_I4, return_ofst, return_st, ST_type(return_st),
//		   WN_CreateIntconst ( OPC_I4INTCONST, 1 ));
//    WN_linenum(wn) = line_number;
//    WN_INSERT_BlockLast ( stmt2_block, wn );
    temp_node = WN_CreateIf ( if_cond_wn, stmt1_block, stmt2_block );
    WN_linenum(temp_node) = line_number;

    
     //Why Set_Loc() is here? Maybe improper, csc
    if (!mp_if) {
//        WN_INSERT_BlockLast( replace_block, Set_Loc( NULL ));
    }

    WN_INSERT_BlockLast ( replace_block, temp_node );


    // no other clean up job should be done then. csc
//    wn = WN_CreateIf ( WN_Ldid ( MTYPE_I4, return_ofst, return_st,
//				 ST_type(return_st) ),
//		       serial_stmt_block, WN_CreateBlock ( ) );
//    WN_linenum(wn) = line_number;
//    WN_INSERT_BlockLast ( replace_block, wn );

  } else if (mpt == MPP_ORPHANED_PDO) {

    Is_True(livein_block == NULL,
           ("non-NULL livein_block for MPP_ORPHANED_PDO"));
    Is_True(liveout_block == NULL,
           ("non-NULL liveout_block for MPP_ORPHANED_PDO"));

    if (do_preamble_block)
      WN_INSERT_BlockLast ( replace_block, do_preamble_block );
    WN_INSERT_BlockLast ( replace_block, stmt_block );

  }


  /* Free up all saved nodes. */

finish_processing:

  while (affinity_nodes) {
    next_node = WN_next(affinity_nodes);
    WN_DELETE_Tree ( affinity_nodes );
    affinity_nodes = next_node;
  }

  while (affinity_d_nodes) {
    next_node = WN_next(affinity_d_nodes);
    WN_DELETE_Tree ( affinity_d_nodes );
    affinity_d_nodes = next_node;
  }

  while (affinity_t_nodes) {
    next_node = WN_next(affinity_t_nodes);
    WN_DELETE_Tree ( affinity_t_nodes );
    affinity_t_nodes = next_node;
  }

  if (chunk_node)
    WN_DELETE_Tree ( chunk_node );

  while (copyin_nodes) {
    next_node = WN_next(copyin_nodes);
    WN_DELETE_Tree ( copyin_nodes );
    copyin_nodes = next_node;
  }

  if (if_node)
    WN_DELETE_Tree ( if_node );

  while (lastlocal_nodes) {
    next_node = WN_next(lastlocal_nodes);
    WN_Delete ( lastlocal_nodes );
    lastlocal_nodes = next_node;
  }

  while (firstprivate_nodes) {
    next_node = WN_next(firstprivate_nodes);
    WN_Delete ( firstprivate_nodes );
    firstprivate_nodes = next_node;
  }

  if (lastthread_node)
    WN_Delete ( lastthread_node );

  while (local_nodes) {
    next_node = WN_next(local_nodes);
    WN_Delete ( local_nodes );
    local_nodes = next_node;
  }

  if (mpnum_node)
    WN_Delete ( mpnum_node );

  if (mpsched_node)
    WN_Delete ( mpsched_node );

  if (numthreads_node)
    WN_DELETE_Tree ( numthreads_node );

  if (ordered_node)
    WN_Delete ( ordered_node );

  while (reduction_nodes) {
    next_node = WN_next(reduction_nodes);
    WN_DELETE_Tree ( reduction_nodes );
    reduction_nodes = next_node;
  }

  while (shared_nodes) {
    next_node = WN_next(shared_nodes);
    WN_Delete ( shared_nodes );
    shared_nodes = next_node;
  }

  for (i=0; i < local_count; i++) {
    if (var_table[i].vtree)
      WN_DELETE_Tree ( var_table[i].vtree );
    if (var_table[i].vtreex)
      WN_DELETE_Tree ( var_table[i].vtreex );
  }

  /* Setup the appropriate return code. */

  if (mp_if) {

    /* For mp if's return the entire replacement block and the caller will
       handle it from there. */

    return_nodes = replace_block;

  } else {

    /* For all other parallel nodes return the replacement code chained with
       any code following the original parallel constructs. */

    if (WN_first(replace_block)) {
      return_nodes = WN_first(replace_block);
      WN_next(WN_last(replace_block)) = cont_nodes;
      if (cont_nodes)
      WN_prev(cont_nodes) = WN_last(replace_block);
    } else
      return_nodes = cont_nodes;

    WN_Delete ( replace_block );
  }

  return (return_nodes);
} // lower_mp()

/***********************************************************************
 *
 * Per-PU initializations for the MP-lowerer.
 * Called by WN_Lower.
 *
 ***********************************************************************/

extern void 
LowerMP_PU_Init()
{
  if (PU_Info_proc_sym(Current_PU_Info) != last_pu_proc_sym) {
    last_pu_proc_sym = PU_Info_proc_sym(Current_PU_Info);
    nl_idx = 0;
    do_id = 0;
    region_id = 0;
    lock_id = 0;
    if (mpid_size > 0)
      BZERO ( mpid_table, mpid_size * sizeof(MPID_TABLE) );
      // ignore pu_chunk_node and pu_mpsched_node from prior PU; no need to
      // deallocate it since Whirl mempool gets popped at end of compiling
      // the PU
    pu_chunk_node = NULL;
    pu_mpsched_node = NULL;
    Init_PU_Globals( );

  }
}

extern BOOL 
WN_has_pragma_with_side_effect ( WN *wn )
{
  OPERATOR opr;
  opr = WN_operator(wn);
  FmtAssert (((opr == OPR_PRAGMA) || (opr == OPR_XPRAGMA)),
             ("WN must contain PRAGMA in WN_has_pragma_with_side_effect\n"));
  switch (WN_pragma(wn)) {
  case WN_PRAGMA_BARRIER:
  case WN_PRAGMA_CRITICAL_SECTION_BEGIN:
  case WN_PRAGMA_COPYIN:
  case WN_PRAGMA_ORDERED:
  case WN_PRAGMA_SINGLE_PROCESS_BEGIN:
  case WN_PRAGMA_MASTER_BEGIN:
  case WN_PRAGMA_FILL:
  case WN_PRAGMA_ALIGN:
  case WN_PRAGMA_INDEPENDENT_BEGIN:
  case WN_PRAGMA_ORDERED_BEGIN:
  case WN_PRAGMA_REDISTRIBUTE:
  case WN_PRAGMA_DISTRIBUTE:
  case WN_PRAGMA_ENTER_GATE:
  case WN_PRAGMA_PAGE_PLACE:
	return TRUE;
  default:
	return FALSE;
  }
}

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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


/* This may look like C code, but it is really -*- C++ -*-
 *
 * ====================================================================
 * ====================================================================
 *
 * Module: config_cache.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_cache.h,v $
 *
 * Revision history:
 *  14-Nov-96 - Original Version, duplicated from cache_parameters.h.
 *
 * Description:
 *
 * This is the description of the memory hierarcy.  It is only used
 * to determine how to SNL transform and for prefetching.  There's no
 * need to model memory hierarchy levels that are to be ignored during
 * transformation.  E.g. typically, there's no need to model main
 * memory.
 *
 * For instructions on adding new memory hierarchy configuration
 * parameters, see the header for config_lno.h.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef config_cache_INCLUDED
#define config_cache_INCLUDED

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *config_cache_rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_cache.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

/*
 * NOTE: this file is included by lnodriver.c and therefore needs to be
 * compilable in C.  This is accomplished by declaring everything as structs
 * with public fields, and then having member functions be protected by
 * conditional compilation (conditional on C++).
 */

/**
*** Description:
***
***    Unique prefix
***
***        MHD_        (meaning "memory hierarchy description")
***
***    MHD_TYPE
***
***        MHD_TYPE_NONE    (not modelling anything)
***        MHD_TYPE_CACHE   (cache: misses to further out cache or main memory)
***        MHD_TYPE_MEM     (main memory: misses to a disk)
***
***    MHD_LEVEL
***
***        The parameters that describe this level of the memory hierarchy.
***
***        MHD_TYPE  Type
***
***             Are we modelling cache, main memory, or nothing
***
***        INT64     Size
***
***             Number of bytes the cache or main memory holds.
***
***        INT64     Effective_Size
***
***             How many bytes the memory hierarcy level can effectively hold
***
***        INT32     Line_Size
***
***             Number of bytes in a cache line or page.
***
***        INT32     Clean_Miss_Penalty
***
***             Processor cycles to next level of memory hierarcy to replace
***             a clean line.
***
***        INT32     Dirty_Miss_Penalty
***
***             Processor cycles to next level of memory hierarcy to replace
***             a dirty line.
***
***        INT32     Miss_Penalty
***
***             Processor cycles to next level of memory hierarcy to replace
***             a clean or dirty line.  Used only to set the distinct
***		clean/dirty variables with a single option -- not to be
***		used outside option processing.
***
***        INT32     Associativity
***
***             Only for MHD_CACHE: 1 for direct mapped, etc
***
***        INT32     TLB_Entries
***
***             -1 if no TLB; otherwise, the number of entries
***
***        INT32     Page_Size
***
***             How many bytes are mapped by one TLB entry
***
***        INT32     TLB_Clean_Miss_Penalty
***        INT32     TLB_Dirty_Miss_Penalty
***        INT32     TLB_Miss_Penalty
***
***		How many cycles a TLB miss costs.
***		The third is used only during option processing to
***		allow setting the first two with a single option --
***		not to be used outside option processing.
***
***        double    Typical_Outstanding
***
***             How many loads may be outstanding at the same time typically.
***             1 means no overlap.  If a cache's max is 4, then 3 might be
***             a reasonable number to use here.  Don't use anything less
***             than 1.
***
***        double    Load_Op_Overlap_1
***        double    Load_Op_Overlap_2
***
***             How much of a load may be overlapped by machine ops.
***             0.9 is reasonable for a non-blocking cache, and 0.0
***             for a blocking cache.  If these are different, then
***             _1 is how much of the first miss cycle is overlapped, and
***             _2 is how much of the last miss cycle that's overlappable
***             is overlapped.  Thus 1.0 >= _1 >= _2 >= 0.  It's linear
***             inbetween those.  Thus if the cache miss cycles are half
***             the estimated machine cycles m, then the actual cycles lost
***             to cache miss would have to be
***                       (1/2 m) [(3/4)(1-_1) + (1/4)(1-_2)]
***
***        INT32   Pct_Excess_Writes_Nonhidable
***
***             If a dirty line costs more than a clean line, let's call
***             the additional cost the excess.  E.g. clean miss=10 cycles,
***             dirty miss=18 cycles.  Excess 8 cycles.  If this parameter
***             is 0, then all 8 additional cycles are treated just as
***             the first 10: it can be hidden.  Realistically, this is not
***             the case.  The processor has to wait longer, and might run
***             out of work.  Dirty misses also take up extra cache resource.
***             If this value is greater than 0, the part that isn't treated
***             has hidable is added directly to the cost (after division
***             by Typical_Outstanding).  A perfectly reasonable value is 100.
***             Because of the extra resources used, one can even imagine
***             a number >100.  We actually allow up to 1000 in the options
***             processing (for testing purposes), but that doesn't
***             make sense.
***
***        BOOL     Prefetch_Level
***
***             Prefetching desired at this memory level?
***
***	   The following are used only for option processing, and
***	   should not be referenced elsewhere:
***
***        INT32     Miss_Penalty
***        INT32     TLB_Miss_Penalty
***
***		Described above.
***
***        char *   CS_string
***
***             Cache size in string form for option processing.
***
***        BOOL	    <various>_Set
***
***             The relevant option was set from the command line.
***
***        Several of the values are specified by the constructor, which
***        from the cache size, line size, and associativity computes
***        the effective cache size.  Other field values are initialized
***        to -1, NULL, or FALSE as appropriate.  If any are
***	   inappropriately 0 or -1 after initialization, then the cache
***        specification is invalid, and Valid() returns false.
***
***        MHD_LEVEL()
***
***             Initialize fields to -1, NULL, or FALSE.
***
***        MHD_LEVEL(MHD_TYPE, INT64 cs, INT32 ls, INT32 mp, INT32 assoc,
***                  INT32 tlb_entries, INT32 pagesz, INT32 tlb_mp)
***
***             Initialize fields and compute Effective_Cache_Size.
***
***        ~MHD_LEVEL()
***        void          operator = (const MHD_LEVEL&);
***        void          Print(FILE* f) const;
***
***             destruct, assign, print
***            
***        void          Merge_Options(const MHD_LEVEL& o)
***
***             Alter specification by merging in defined values from o.
***
***        BOOL          Valid() const;
***
***             Returns true if and only if all cache fields >= 1 and type
***             not MHD_TYPE_NONE.
***
***        BOOL          TLB_Valid() const;
***
***             TLB_Entries, Page_Size and TLB_Miss_Penalty all >=1 and Valid()
***
***    MHD_MAX_LEVELS
***
***        The maximum number of memory hierarcy levels we will model.
***        Currently, 4.  If this changes, change lnodriver.c as well.
***
***    MHD
***
***        The memory hierarcy description, which contains MHD_LEVEL for
***        each level of the memory hierarcy to be modeled by the compiler,
***        and other system information useful for cache modelling.
***
***        BOOL           Non_Blocking_Loads
***
***             TRUE if processor continues after a cache miss.
***
***        INT32          Loop_Overhead_Base
***        INT32          Loop_Overhead_Memref
***
***             The loop overhead, in processor cycles, is 
***                  Loop_Overhead_Base + memrefs*Loop_Overhead_Memref
***             where memrefs is the number of non-cse'd memrefs in the
***             inner loop.  For example, do i a(i,j), a(i,j), a(i+1,j)
***             all are cse'd into one memref in the inner loop, but
***             do i a(i,j) a(i,j+1) have memref=2.
***             The base is lower for T5 than for others because pipeline
***             startup/winddown should be less when the fp pipelines are
***             shorter.  Also, multiple int/cycle.  The memref is the cost
***             to load an address into a register.
***
***        INT         TLB_Trustworthiness
***
***             0 to ignore tlb, 100 to fully trust it.  Even if it's fully
***             trusted, when there is no blocking and a low cache miss rate,
***             we still reduce the TLB penalty if TLB_NoBlocking_Model is set.
***
***        BOOL           TLB_NoBlocking_Model
***
***             If we are not blocking and there is a low cache miss rate,
***             should we back off on the TLB penalty?  If true
***             it favors not blocking.  It's there because the TLB model
***             is inaccurate and especially can overestimate the TLB miss
***             rate when we are not blocking.
***
***        MHD_LEVEL      L[MHD_MAX_LEVELS]
***
***             E.g. L[0] might be the primary cache.
***
***        INT            First()
***
***             The first valid level.  Returns -1 if none.
***
***        INT            Next(INT l)
***
***             The valid level after l.  Returns -1 when done.
***
***        void           Merge_Options(const MHD& o);
***
***            Alter specification by merging in defined values from o.
***
***        void           Initialize()
***
***            Use information about the target to set default values for
***            these cache parameters.
***
***        void           Print(FILE*) const
***        MHD()
***        ~MHD()
***
*** Exported Variables:
***
***    MHD Mhd;
***
***        The memory hierarchy description of we are compiling for
***
***    MHD Mhd_Options;
***
***        User specified options
**/

typedef enum MHD_TYPE {
  MHD_TYPE_NONE=222,
  MHD_TYPE_CACHE,
  MHD_TYPE_MEM
} MHD_TYPE;

typedef struct MHD_LEVEL {
  MHD_TYPE  Type;
  INT64   Size;
  INT64   Effective_Size;
  INT32   Line_Size;
  INT32   Clean_Miss_Penalty;
  INT32   Dirty_Miss_Penalty;
  INT32   Associativity;
  INT32   TLB_Entries;
  INT32   Page_Size;
  BOOL	  Prefetch_Level;
  INT32   TLB_Clean_Miss_Penalty;
  INT32   TLB_Dirty_Miss_Penalty;

  double  Typical_Outstanding;
  double  Load_Op_Overlap_1;
  double  Load_Op_Overlap_2;
  INT32   Pct_Excess_Writes_Nonhidable;

  /* Used just for option setting: */
  char *  CS_string;
  BOOL	  CMP_Set;	/* Clean_Miss_Penalty was set explicitly */
  BOOL	  DMP_Set;	/* Dirty_Miss_Penalty was set explicitly */
  BOOL	  Is_Mem_Level;
  BOOL	  Is_Mem_Level_Set;
  INT32	  Miss_Penalty;
  BOOL	  Miss_Penalty_Set;
  BOOL	  TLB_CMP_Set;	/* TLB_Clean_Miss_Penalty set explicitly */
  BOOL	  TLB_DMP_Set;	/* TLB_Dirty_Miss_Penalty set explicitly */
  INT32	  TLB_Miss_Penalty;
  BOOL	  TLB_MP_Set;

#if defined(_LANGUAGE_C_PLUS_PLUS)
  void      operator = (const MHD_LEVEL&);
  void      Print(FILE* f) const;
  void      Merge_Options(const MHD_LEVEL& o);
  BOOL      Valid() const;
  BOOL      TLB_Valid() const;

  MHD_LEVEL() : Type(MHD_TYPE_NONE),
                Size(-1), Line_Size(-1),
                Clean_Miss_Penalty(-1), Dirty_Miss_Penalty(-1),
                Associativity(-1), Effective_Size(-1),
                TLB_Entries(-1), Page_Size(-1), Prefetch_Level (-1),
                TLB_Clean_Miss_Penalty(-1), TLB_Dirty_Miss_Penalty(-1),
                Typical_Outstanding(-1.0),
                Load_Op_Overlap_1(-1.0), Load_Op_Overlap_2(-1.0),
		Pct_Excess_Writes_Nonhidable(-1),
		CS_string (NULL), CMP_Set (FALSE), DMP_Set (FALSE),
		Is_Mem_Level(-1), Is_Mem_Level_Set(FALSE),
		Miss_Penalty(-1), Miss_Penalty_Set(FALSE),
		TLB_CMP_Set (FALSE), TLB_DMP_Set (FALSE),
		TLB_Miss_Penalty(-1), TLB_MP_Set(FALSE) {}
  MHD_LEVEL(MHD_TYPE type, INT64 cs, INT32 ls, INT32 cmp, INT32 dmp,
            INT32 assoc, INT32 entries, INT32 pagesz,
            INT32 tlb_cmp, INT32 tlb_dmp,
            double outstanding, double op_overlap_1, double op_overlap_2,
	    INT32 pct_excess_writes_nonhidable);
  ~MHD_LEVEL() {}

 private:

  void      Compute_Effective_Size();
#ifdef KEY
  void      Reset_CS_String();
#endif
#endif
} MHD_LEVEL;

/* Don't forget to modify config_lno.c as well */
#define MHD_MAX_LEVELS 4

typedef struct MHD {
  INT32     Loop_Overhead_Base;
  INT32     Loop_Overhead_Memref;
  BOOL      Non_Blocking_Loads;
  INT32     TLB_Trustworthiness;
  BOOL      TLB_NoBlocking_Model;
  MHD_LEVEL L[MHD_MAX_LEVELS];

#if defined(_LANGUAGE_C_PLUS_PLUS)
  INT       First();
  INT       Next(INT);
  void      Merge_Options(const MHD&);
  void      Initialize();
  void      Print(FILE*) const;

  MHD() : Non_Blocking_Loads(-1),
          Loop_Overhead_Base(-1),
	  Loop_Overhead_Memref(-1),
          TLB_Trustworthiness(-1),
          TLB_NoBlocking_Model(-1) {}
  ~MHD() {}

 private:

  MHD(const MHD&);                /* undefined */
#endif
} MHD;

extern MHD Mhd;
extern MHD Mhd_Options;

#endif /* config_cache_INCLUDED */

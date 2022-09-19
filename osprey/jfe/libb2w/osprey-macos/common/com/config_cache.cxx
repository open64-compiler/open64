/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


// This may look like C code, but it is really -*- C++ -*-
//
// ====================================================================
// ====================================================================
//
// Module: config_cache.cxx
// Revision history:
//  14-Nov-96 - Original Version, copied from cache_parameters.cxx.
//
// Description:
//
// This is the description of the memory hierarcy.  It is only used
// to determine how to SNL transform and for prefetching.  There's no
// need to model memory hierarchy levels that are to be ignored during
// transformation.  E.g. typically, there's no need to model main
// memory.
//
// ====================================================================
// ====================================================================

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <math.h>

#include "defs.h"
#include "errors.h"
#include "config_cache.h"
#ifdef KEY
#include "flags.h"
#include "config_lno.h" // For EffectiveCacheSizePct
#endif

//---------------------------------------------------------------------------

MHD_LEVEL::MHD_LEVEL(MHD_TYPE type, INT64 cs, INT32 ls,
                     INT32 cmp, INT32 dmp, INT32 assoc,
                     INT32 tlb_entries, INT32 ps,
                     INT32 tlb_cmp, INT32 tlb_dmp,
                     double outstanding,
                     double op_overlap_1, double op_overlap_2,
		     INT pct_xwrites_nonhidable) :
  Type(type), Size(cs), Line_Size(ls),
  Clean_Miss_Penalty(cmp), Dirty_Miss_Penalty(dmp), Associativity(assoc),
  TLB_Entries(tlb_entries), Page_Size(ps), Prefetch_Level (-1),
  TLB_Clean_Miss_Penalty(tlb_cmp), TLB_Dirty_Miss_Penalty(tlb_dmp),
  Typical_Outstanding(outstanding),
  Load_Op_Overlap_1(op_overlap_1),  Load_Op_Overlap_2(op_overlap_2),
  Pct_Excess_Writes_Nonhidable(pct_xwrites_nonhidable),
  CS_string (NULL), CMP_Set (FALSE), DMP_Set (FALSE),
  Is_Mem_Level(-1), Is_Mem_Level_Set(FALSE),
  Miss_Penalty(-1), Miss_Penalty_Set(FALSE),
  TLB_CMP_Set (FALSE), TLB_DMP_Set (FALSE),
  TLB_Miss_Penalty(-1), TLB_MP_Set(FALSE)
{
  Compute_Effective_Size();
}

void MHD_LEVEL::operator = (const MHD_LEVEL& a)
{
  Type = a.Type;
  Line_Size = a.Line_Size;
  Size = a.Size;
  Clean_Miss_Penalty = a.Clean_Miss_Penalty;
  Dirty_Miss_Penalty = a.Dirty_Miss_Penalty;
  Associativity = a.Associativity;
  Effective_Size = a.Effective_Size;
  TLB_Entries = a.TLB_Entries;
  Page_Size = a.Page_Size;
  Prefetch_Level = a.Prefetch_Level;
  TLB_Clean_Miss_Penalty = a.TLB_Clean_Miss_Penalty;
  TLB_Dirty_Miss_Penalty = a.TLB_Dirty_Miss_Penalty;
  Typical_Outstanding = a.Typical_Outstanding;
  Load_Op_Overlap_1 = a.Load_Op_Overlap_1;
  Load_Op_Overlap_2 = a.Load_Op_Overlap_2;
  Pct_Excess_Writes_Nonhidable = a.Pct_Excess_Writes_Nonhidable;

  // The following are to be used only during option setting;
  // simply reset them to defaults:
  CS_string = NULL;
  CMP_Set = FALSE;
  DMP_Set = FALSE;
  Is_Mem_Level = -1;
  Is_Mem_Level_Set = FALSE;
  Miss_Penalty = -1;
  Miss_Penalty_Set = FALSE;
  TLB_CMP_Set = FALSE;
  TLB_DMP_Set = FALSE;
  TLB_Miss_Penalty = -1;
  TLB_MP_Set = FALSE;
}

BOOL MHD_LEVEL::Valid() const
{
  return Size >= 1 &&
         Line_Size  >= 1 &&
         (Associativity >= 1 || Type == MHD_TYPE_MEM) &&
         Clean_Miss_Penalty >= 1 && Dirty_Miss_Penalty >= 1;
}

BOOL MHD_LEVEL::TLB_Valid() const
{
  return TLB_Entries >= 1 &&
         Page_Size >= 1 &&
         TLB_Clean_Miss_Penalty >= 1 &&
         TLB_Dirty_Miss_Penalty >= 1 &&
         Valid();
}

void MHD_LEVEL::Merge_Options(const MHD_LEVEL& o)
{
  BOOL recompute_ecs = FALSE;
  
  if (o.Type != MHD_TYPE_NONE) {
    Type = o.Type;
    recompute_ecs = TRUE;
  }

  if (o.Line_Size != -1) {
    Line_Size = o.Line_Size;
    recompute_ecs = TRUE;
  }
  if (o.Size != -1) {
    Size = o.Size;
    recompute_ecs = TRUE;
  }
  if (o.Associativity != -1) {
    Associativity = o.Associativity;
    recompute_ecs = TRUE;
  }
  if (o.Clean_Miss_Penalty != -1)
    Clean_Miss_Penalty = o.Clean_Miss_Penalty;
  if (o.Dirty_Miss_Penalty != -1)
    Dirty_Miss_Penalty = o.Dirty_Miss_Penalty;

  if (o.TLB_Entries != -1)
    TLB_Entries = o.TLB_Entries;
  if (o.Page_Size != -1)
    Page_Size = o.Page_Size;
  if (o.Prefetch_Level != -1)
    Prefetch_Level = o.Prefetch_Level;
  if (o.TLB_Clean_Miss_Penalty != -1)
    TLB_Clean_Miss_Penalty = o.TLB_Clean_Miss_Penalty;
  if (o.TLB_Dirty_Miss_Penalty != -1)
    TLB_Dirty_Miss_Penalty = o.TLB_Dirty_Miss_Penalty;

  if (o.Typical_Outstanding >= 0.0)
    Typical_Outstanding = o.Typical_Outstanding;
  if (o.Load_Op_Overlap_1 >= 0.0)
    Load_Op_Overlap_1 = o.Load_Op_Overlap_1;
  if (o.Load_Op_Overlap_2 >= 0.0)
    Load_Op_Overlap_2 = o.Load_Op_Overlap_2;
  if (o.Pct_Excess_Writes_Nonhidable >= 0)
    Pct_Excess_Writes_Nonhidable = o.Pct_Excess_Writes_Nonhidable;

  if (Valid() && recompute_ecs)
    Compute_Effective_Size();
#ifdef KEY
  if(Valid())
   Reset_CS_String();
#endif
}

#ifdef KEY /* Bug 10252: set cache size */
void MHD_LEVEL::Reset_CS_String()
{
   if(Size < 0) return; /*nothing to do */
   if(CS_string != NULL)
      free(CS_string); /* safe to free, because it is malloced */
   const char *addition = "K"; /* appending character */
   INT64 remains = Size/1024;
   if(remains >= 1024)
   {
       addition = "M";
       remains = remains/1024;
       if(remains >= 1024){
       remains = remains/1024;
       addition = "G";
       }
   }
   CS_string = (char *)malloc(32);
   sprintf(CS_string, "%lld", remains);
   strcat(CS_string, addition);
}
#endif

void MHD_LEVEL::Compute_Effective_Size()
{
  // Make the effective cache fraction increases by 2% per 10x decrease in
  // cache size.  5% per 10x decrease in
  // line size, and 8% per 10x decrease of assoc.  Also, if it drops
  // below 7%, move it half way towards 7% and don't let it drop below
  // 3.5% (a real anomaly, but the user can specify any oddball cache).
  // Basis is 16% for a 64kB direct mapped cache with 16b lines.

  // For a memory, the associativity (and all other parameters, come to
  // think of it) are ignored, and we use use the effective cache size
  // as 90% of the cache.  (conservative, in case I miscomputed the area,
  // better to err on that side).

  double pct;
  
  switch (Type) {
   case MHD_TYPE_MEM:
    pct = 0.9;
    break;
   case MHD_TYPE_CACHE:
    pct = 0.16 - 0.02*log10(double(Size)/(64*1024))
               - 0.05*log10(double(Line_Size)/16)
               + 0.07*log10(double(MIN(Associativity,64)));

    if (pct < 0.0)
      pct = 0.035;
    else if (pct <= 0.07)
      pct += (0.07 - pct)/2;
    else if (pct > 0.50)
      pct = 0.50;
    break;
  }

  Effective_Size = (INT64) (pct*Size);
#ifdef KEY
  if (LNO_EffectiveCacheSizePct != 0)
    Effective_Size = (INT64) ((double)LNO_EffectiveCacheSizePct/100.0*Size);
#endif
}

void MHD_LEVEL::Print(FILE* f) const
{
  fprintf(f, "sz=%lld(%lld,%.1f%%)\n", 
          Size, Effective_Size, 100.0*Effective_Size/Size);
  fprintf(f, "  ls=%d cmp=%d dmp=%d\n", 
          Line_Size, Clean_Miss_Penalty, Dirty_Miss_Penalty);
  if (Type == MHD_TYPE_MEM)
    fprintf(f, "<mem> ");
  else
    fprintf(f, "  a=%d ", Associativity);
  fprintf(f, "tlbsz=%d ps=%d tlbcmp=%d tlbdmp=%d\n",
          TLB_Entries, Page_Size,
          TLB_Clean_Miss_Penalty, TLB_Dirty_Miss_Penalty);
  fprintf(f, "  out=%g, ovlp1=%g, ovlp2=%g\n",
          Typical_Outstanding, Load_Op_Overlap_1, Load_Op_Overlap_2);
}

//---------------------------------------------------------------------------

MHD Mhd;
MHD Mhd_Options;

//---------------------------------------------------------------------------

INT MHD::Next(INT i)
{
  if (i != -1) {
    for (i++; i < MHD_MAX_LEVELS; i++) {
      if (L[i].Valid())
        return i;
    }
  }
  return -1;
}

INT MHD::First()
{
  for (INT i = 0; i < MHD_MAX_LEVELS; i++) {
    if (L[i].Valid())
      return i;
  }
  return -1;
}

void MHD::Merge_Options(const MHD& o)
{
  for (INT i = 0; i < MHD_MAX_LEVELS; i++)
    L[i].Merge_Options(o.L[i]);

  if (o.Non_Blocking_Loads != -1)
    Non_Blocking_Loads = o.Non_Blocking_Loads;
  if (o.Loop_Overhead_Base >= 0)
    Loop_Overhead_Base = o.Loop_Overhead_Base;
  if (o.Loop_Overhead_Memref >= 0)
    Loop_Overhead_Memref = o.Loop_Overhead_Memref;
#ifdef KEY
  if (o.TLB_Trustworthiness >= 0)
    TLB_Trustworthiness = o.TLB_Trustworthiness;
#endif
}

void MHD::Print(FILE* f) const
{
  fprintf(f, "CACHE PARAMETERS: non_blocking_loads=%d loop_overhead=(%d,%d)\n",
          Non_Blocking_Loads, Loop_Overhead_Base, Loop_Overhead_Memref);
  for (INT i = 0; i < MHD_MAX_LEVELS; i++) {
    if (L[i].Valid()) {
      fprintf(f, "L[%d]: ", i);
      L[i].Print(f);
    }
  }
  fprintf(f, "\n"); 
}


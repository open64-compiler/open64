/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

//  GRA tracing
/////////////////////////////////////
//
//  Description:
//
//      Support for tracing GRA
//
//
/////////////////////////////////////


//  $Revision: 1.6 $
//  $Date: 05/12/05 08:59:10-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_trace.cxx $

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include "defs.h"
#include "tracing.h"
#include "cgir.h"
#include "register.h"
#include "gra_bb.h"
#include "gra_lrange.h"
#include "gra_region.h"
#include "gra_interfere.h"

static INT  trace_detail;
static BOOL trace_color;
static BOOL trace_split;
static BOOL trace_split_priority;
static BOOL trace_grants;
static BOOL trace_preference;
static BOOL trace_place;
static BOOL trace_stats;
static BOOL trace_neighbors;
static BOOL trace_loops;
static BOOL trace_homing;
static BOOL trace_memory;
static BOOL trace_perf_comp;
static BOOL trace_check_splits;

// Some brain death with stdarg.h and defs.h
#undef long

/////////////////////////////////////
void
GRA_Trace_Color_LRANGE( const char* str, LRANGE* lrange )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  char buff[100];

  if (trace_color && 1 <= trace_detail)
    fprintf(TFile,"<gra> %s p %g %s\n",str,lrange->Priority(),
                                           lrange->Format(buff));
}

/////////////////////////////////////
void
GRA_Trace_Color_REGION( GRA_REGION* region )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if ( trace_color ) {
    fprintf(TFile,"<gra> Coloring region ");
    RID_Fprint(TFile,region->Rid());
  }
}

/////////////////////////////////////
void
GRA_Trace_Grant( GRA_BB* gbb, ISA_REGISTER_CLASS cl, REGISTER reg )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if ( trace_grants ) {
    fprintf(TFile,"<gra> Granting local register %s to BB%d\n",
                  REGISTER_name(cl,reg),BB_id(gbb->Bb()));
  }
}

/////////////////////////////////////
void
GRA_Trace_Grant_Unused_Caller_Saved(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if ( trace_grants ) {
    fprintf(TFile,
	    "<gra> Attempting to grant unused caller saved registers.\n");
  }
}
    
/////////////////////////////////////
void
GRA_Trace(INT detail, const char *fmt, ...)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (detail <= trace_detail) {
    va_list args;
    va_start(args, fmt);
    fprintf(TFile, "<gra> ");
    vfprintf(TFile,fmt,args);
    fprintf(TFile,"\n");
    va_end(args);
  }
}

/////////////////////////////////////
void
GRA_Trace_Split(INT detail, const char *fmt, ...)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (trace_split && detail <= trace_detail) {
    va_list args;
    va_start(args, fmt);
    fprintf(TFile, "<gra> ");
    vfprintf(TFile,fmt,args);
    fprintf(TFile,"\n");
    va_end(args);
  }
}

/////////////////////////////////////
void
GRA_Trace_Split_Removing_Block(GRA_BB* gbb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (trace_split) {
    fprintf(TFile, "<gra> Removing BB:%d from split.\n",
	    BB_id(gbb->Bb()));
  }
}

/////////////////////////////////////
void
GRA_Trace_Split_Add_Priority(GRA_BB* gbb, BOOL is_store)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (trace_split_priority) {
    fprintf(TFile, "<gra> Adding to priority for %s in BB:%d \n",
	    is_store ? "spill" : "restore",
	    BB_id(gbb->Bb()));
  }
}

/////////////////////////////////////
void
GRA_Trace_Split_Sub_Priority(GRA_BB* gbb, BOOL is_store)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (trace_split_priority) {
    fprintf(TFile, "<gra> Subtracting from priority for %s in BB:%d \n",
	    is_store ? "spill" : "restore",
	    BB_id(gbb->Bb()));
  }
}

#ifdef KEY
/////////////////////////////////////
void
GRA_Trace_Split_Reclaim_Add_Priority(GRA_BB* gbb, BOOL is_store, float priority)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (trace_split_priority) {
    fprintf(TFile, "<gra> Adding %f to priority for reclaim %s in BB:%d \n",
	    priority,
	    is_store ? "spill" : "restore",
	    BB_id(gbb->Bb()));
  }
}

/////////////////////////////////////
void
GRA_Trace_Split_Reclaim_Sub_Priority(GRA_BB* gbb, BOOL is_store, float priority)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (trace_split_priority) {
    fprintf(TFile, "<gra> Subtracting %f from priority for reclaim %s in BB:%d \n",
	    priority,
	    is_store ? "spill" : "restore",
	    BB_id(gbb->Bb()));
  }
}
#endif

/////////////////////////////////////
void
GRA_Trace_Split_Priority_On(const char *msg)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (trace_check_splits) {
    trace_split_priority = TRUE;
    fprintf(TFile, "<gra> Calculate split priorities via %s:\n", msg);
  }
}

/////////////////////////////////////
void
GRA_Trace_Split_Priority_Off()
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  trace_split_priority = FALSE;
}

/////////////////////////////////////
void
GRA_Trace_Color(INT detail, const char *fmt, ...)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (trace_color && detail <= trace_detail) {
    va_list args;
    va_start(args, fmt);
    fprintf(TFile, "<gra> ");
    vfprintf(TFile,fmt,args);
    fprintf(TFile,"\n");
    va_end(args);
  }
}

void
GRA_Trace_Place( INT detail, const char *fmt, ...)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (trace_place && detail <= trace_detail) {
    va_list args;
    va_start(args, fmt);
    fprintf(TFile, "<gra> ");
    vfprintf(TFile,fmt,args);
    fprintf(TFile,"\n");
    va_end(args);
  }
}

/////////////////////////////////////
void
GRA_Trace_Place_LRANGE_GBB( const char* str, LRANGE* lrange,
                                             GRA_BB* gbb )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (trace_place && 1 <= trace_detail) {
    fprintf(TFile,"TN%d BB%d -- %s\n",TN_number(lrange->Tn()),
                                      BB_id(gbb->Bb()),
                                      str);
  }
}
    

/////////////////////////////////////
static void
print_lr( LRANGE *lr)
/////////////////////////////////////
//  print live range in format for
//  neighbor trace.
/////////////////////////////////////
{
  if (lr->Type() == LRANGE_TYPE_LOCAL) {
    if (lr->Has_Wired_Register()) {
      fprintf(TFile,"W%d(TN%d)",lr->Id(), lr->Reg());
    } else {
      fprintf(TFile,"L%d",lr->Id());
    }
  } else {
    fprintf(TFile,"C%d(TN%d)",lr->Id(), TN_number(lr->Tn()));
  }
}

/////////////////////////////////////
void
GRA_Trace_Preference_Attempt(LRANGE* lrange0, LRANGE* lrange1,
			     GRA_REGION* region, BOOL outcome )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  char buff0[80], buff1[80];

  if ( trace_preference ) {
    GRA_Trace(0,"Preference %s with %s [%s]",
                lrange0->Format(buff0),
                lrange1->Format(buff1),
                outcome ? "yes" : "no");
    if (!outcome && lrange1->Allocated()) {
      //
      // Print any neighbors that have the color.  Assumes that lrange1
      // is the one that we are attempting to preference *from*.
      //
      fprintf(TFile,"<gra> LRANGES preventing preference: ");
      LRANGE_NEIGHBOR_ITER iter;
      REGISTER reg = lrange1->Reg();
      for (iter.Init(lrange0,region); ! iter.Done(); iter.Step()) {
	LRANGE* nlr = iter.Current();
	if (nlr->Allocated() &&
	    nlr->Reg() == reg) {
	  print_lr(nlr);
	  fprintf(TFile," ");	  
	}
      }
      fprintf(TFile,"\n");

      if (lrange0->Type() ==  LRANGE_TYPE_COMPLEMENT) {
	fprintf(TFile,"<gra> BBs preventing preference: ");
	LRANGE_LIVE_GBB_ITER gbb_iter;
	ISA_REGISTER_CLASS rc = lrange0->Rc();
	for (gbb_iter.Init(lrange0); ! gbb_iter.Done(); gbb_iter.Step()) {
	  GRA_BB* gbb = gbb_iter.Current();
	  if (REGISTER_SET_MemberP(gbb->Registers_Used(rc), reg)) {
	    fprintf(TFile," BB%d", BB_id(gbb->Bb()));
	  }
	}
	fprintf(TFile,"\n");
      }
    }
  }
}

/////////////////////////////////////
void
GRA_Trace_Preference_Copy(LRANGE* lrange0, LRANGE* lrange1, GRA_BB* gbb )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  char buff0[80], buff1[80];

  if ( trace_preference ) {
    char *str;
    if (lrange0->Type() == LRANGE_TYPE_REGION ||
	lrange1->Type() == LRANGE_TYPE_REGION) {
      str = "Glue";
    } else if (lrange0->Type() == LRANGE_TYPE_LOCAL ||
	       lrange1->Type() == LRANGE_TYPE_LOCAL) {
      str = "Dedicated";
    } else {
      str = "Global";
    }
    GRA_Trace(0,"%s preference copy allowed in BB:%d %s with %s",
	      str,
	      BB_id(gbb->Bb()),
	      lrange0->Format(buff0),
	      lrange1->Format(buff1));

  }
}

/////////////////////////////////////
void
GRA_Trace_Preference_Conflict(LRANGE* lrange0, LRANGE* lrange1, GRA_BB* gbb )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  char buff0[80], buff1[80];

  if ( trace_preference ) {
    GRA_Trace(0,"Preference: conflict for copy in BB:%d %s with %s",
                BB_id(gbb->Bb()), 
		lrange0->Format(buff0),
                lrange1->Format(buff1));

  }
}


/////////////////////////////////////
void
GRA_Trace_Possible_Preference_Copy(LRANGE* lrange0, LRANGE* lrange1, GRA_BB* gbb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  char buff0[80], buff1[80];

  if ( trace_preference ) {
    char *str;
    if (lrange0->Type() == LRANGE_TYPE_REGION ||
	lrange1->Type() == LRANGE_TYPE_REGION) {
      str = "Glue";
    } else if (lrange0->Type() == LRANGE_TYPE_LOCAL ||
	       lrange1->Type() == LRANGE_TYPE_LOCAL) {
      str = "Dedicated";
    } else {
      str = "Global";
    }
    GRA_Trace(0,"%s TN preference copy possible in BB:%d %s with %s",
	      str,
	      BB_id(gbb->Bb()),
	      lrange0->Format(buff0),
	      lrange1->Format(buff1));
  }
}

/////////////////////////////////////
void
GRA_Trace_Global_Preference_Failure(LRANGE* lrange0, LRANGE* lrange1, GRA_BB* gbb)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  char buff0[80], buff1[80];

  if ( trace_preference ) {
    GRA_Trace(0,"Global TN preference conflict found in BB:%d %s with %s",
                BB_id(gbb->Bb()),
                lrange0->Format(buff0),
                lrange1->Format(buff1));

  }
}

/////////////////////////////////////
void
GRA_Trace_Global_Preference_Success(LRANGE* lrange0, LRANGE* lrange1)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  char buff0[80], buff1[80];

  if ( trace_preference ) {
    GRA_Trace(0,"Global TN preference successful for %s with %s",
                lrange0->Format(buff0),
                lrange1->Format(buff1));
  }
}


/////////////////////////////////////
void GRA_Trace_Spill_Stats(float freq_restore_count, INT restore_count,
			   float freq_spill_count, INT spill_count,
			   float priority_count)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if ( trace_stats ) {
    GRA_Trace(0,"freq weighted restores %g spills %g allocated priority %g",
                freq_restore_count,freq_spill_count, priority_count);
    GRA_Trace(0,"absolute restores %d spills %d",restore_count,spill_count);
  } else if ( trace_perf_comp ) {
    GRA_Trace(0,"%s %g %g %d %d %g\n", ST_name(Get_Current_PU_ST()),
	      freq_restore_count, freq_spill_count, restore_count,
	      spill_count, priority_count);
  }
}

/////////////////////////////////////
void GRA_Trace_LRANGE_Stats( INT32 complement_count,
                             INT32 region_count,
                             INT32 local_count,
                             INT32 split_count )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if ( trace_stats ) {
    GRA_Trace(0,"LRANGE counts complement %d region %d local %d split %d",
              complement_count,
              region_count,
              local_count,
              split_count);
  }
}

/////////////////////////////////////
void GRA_Trace_Regs_Stats( ISA_REGISTER_CLASS rc,
			   REGISTER_SET regs_all, 
			   REGISTER_SET &regs_used)
// Trace how many regs used by GRA out of how many available regs
{
  if ( trace_stats ) 
    GRA_Trace(0,"rc %d: used %d regs out of %d", rc, 
	      REGISTER_SET_Size(regs_used), REGISTER_SET_Size(regs_all));
}

/////////////////////////////////////
void
GRA_Trace_Wired_Local_Count( INT32 count )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if ( trace_stats ) {
    GRA_Trace(0,"wired local LRANGE count %d\n",count);
  }
}
  
/////////////////////////////////////
void
GRA_Trace_Complement_LRANGE_Neighbors( LRANGE *lr, GRA_REGION* region )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  LRANGE_NEIGHBOR_ITER iter;
  if ( trace_neighbors ) {
    fprintf(TFile,"<gra> Neighbors for ");
    print_lr(lr);
    fprintf(TFile,": ");
    for (iter.Init(lr,region); ! iter.Done(); iter.Step()) {
      LRANGE* nlr = iter.Current();
      print_lr(nlr);
      fprintf(TFile," ");
    }
    fprintf(TFile,"\n");
    if (lr->Type() == LRANGE_TYPE_COMPLEMENT) {
      fprintf(TFile, "live_bb_set: ");
      BB_SET_Print(lr->Live_BB_Set(), TFile); fprintf(TFile, "\n");
    }
  }
}

/////////////////////////////////////
BOOL 
GRA_Trace_Loops(void)
/////////////////////////////////////
//
// See interface description.
/////////////////////////////////////
{
  return trace_loops;
}

/////////////////////////////////////
void
GRA_Trace_Homing(TN *tn, BB* bb)
/////////////////////////////////////
{
  if (trace_homing) {
    fprintf(TFile,"<gra> Homing TN%d in BB:%d\n", TN_number(tn), BB_id(bb));
  }
}

/////////////////////////////////////
void
GRA_Trace_Home_Removal(TN *tn, GRA_BB* gbb, OP* op)
/////////////////////////////////////
//
// See interface description.
/////////////////////////////////////
{
  if (trace_homing) {
    fprintf(TFile,"<gra> Homing TN%d in BB:%d Delete op ",
	    TN_number(tn), BB_id(gbb->Bb()));
    Print_OP_No_SrcLine(op);
  }
}

static void *last_brk;

/////////////////////////////////////
void
GRA_Init_Trace_Memory(void)
/////////////////////////////////////
{
  if (trace_memory) {
    last_brk = sbrk(0);
  }
}

/////////////////////////////////////
void
GRA_Trace_Memory(const char *string)
/////////////////////////////////////
{
  if (trace_memory) {
    unsigned usage = ((INTPTR) sbrk(0)) - (INTPTR) last_brk;
    fprintf(TFile,"<gra> Memory use %s: %d\n", string, usage);
  }
}

/////////////////////////////////////
void
GRA_Trace_Memory_Realloc(const char *string)
/////////////////////////////////////
{
  if (trace_memory) {
    fprintf(TFile,"<gra> Memory realloc in %s\n", string);
  }
}

/////////////////////////////////////
void
GRA_Trace_Memory_Counts()
/////////////////////////////////////
{
  if (trace_memory) {
    fprintf(TFile,"<gra> Memory use for GRA_BBs: %ld\n",
	    (long)(gbb_mgr.Alloc_Count()*sizeof(GRA_BB)));
    fprintf(TFile,"<gra> Memory count of GRA_BBs: %d\n",
	    gbb_mgr.Alloc_Count());
    fprintf(TFile,"<gra> Memory use for GRA_REGIONs: %ld\n",
	    (long)(gra_region_mgr.Alloc_Count()*sizeof(GRA_REGION)));
    fprintf(TFile,"<gra> Memory count of GRA_REGIONs: %ld\n",
	    (long)(gra_region_mgr.Alloc_Count()));
  }
}

void 
GRA_Trace_Split_LUNIT_Spill(LUNIT *lunit)
{
  GRA_BB* gbb = lunit->Gbb();

  if (trace_split && gbb->Freq() > 1.0 && lunit->Split_Lunit()) {
    LRANGE* lrange  = lunit->Lrange();
    TN*     tn      = lrange->Tn();    
    fprintf(TFile,"<gra> Split spill in high frequency block BB:%d ",
	    BB_id(gbb->Bb()));
    fprintf(TFile,"for TN%d\n",TN_number(tn));
  }
}

/////////////////////////////////////
BOOL
GRA_Trace_Check_Splits()
{
  return trace_check_splits;
}

/////////////////////////////////////
void
GRA_Trace_LRANGE_Allocate(LRANGE* lrange )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  char buff0[80];

  if ( trace_color ) {
    GRA_Trace(0,"Allocated: %s", lrange->Format(buff0));
  }
}

#ifdef KEY
/////////////////////////////////////
void
GRA_Trace_LRANGE_Choose(LRANGE* lrange, REGISTER_SET allowed)
{
  if ( trace_color && lrange->Type() != LRANGE_TYPE_LOCAL) {
    fprintf(TFile, "<gra> choose from allowed ");
    REGISTER_SET_Print(allowed, TFile);
    fprintf(TFile, " for GTN%d\n", TN_number(lrange->Tn()));
  }
}
/////////////////////////////////////
void
GRA_Trace_LRANGE_Choose_Reclaimable(LRANGE* lrange, REGISTER_SET reclaimable)
{
  if ( trace_color && lrange->Type() != LRANGE_TYPE_LOCAL) {
    fprintf(TFile, "<gra> choose from reclaimable ");
    REGISTER_SET_Print(reclaimable, TFile);
    fprintf(TFile, " for GTN%d\n", TN_number(lrange->Tn()));
  }
}
#endif

/////////////////////////////////////
void
GRA_Trace_Initialize(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (Get_Trace(TP_GRA, 0xffffffff))
    fprintf(TFile, "<gra> start gra trace for %s\n", 
	    ST_name(Get_Current_PU_ST()));
  trace_detail         = Get_Trace(TP_GRA,    0x1) ? 1 : 0;
  trace_color          = Get_Trace(TP_GRA,    0x2);
  trace_split          = Get_Trace(TP_GRA,    0x4);
  trace_split_priority = FALSE;
  trace_grants         = Get_Trace(TP_GRA,    0x8);
  trace_preference     = Get_Trace(TP_GRA,   0x10);
  trace_place          = Get_Trace(TP_GRA,   0x20);
  trace_stats          = Get_Trace(TP_GRA,   0x40);
  trace_neighbors      = Get_Trace(TP_GRA,   0x80);
  trace_loops	       = Get_Trace(TP_GRA,  0x100);
  trace_homing	       = Get_Trace(TP_GRA,  0x200);
  trace_memory         = Get_Trace(TP_GRA,  0x400);
  trace_perf_comp      = Get_Trace(TP_GRA,  0x800);
  trace_check_splits   = Get_Trace(TP_GRA, 0x1000);
  // 0x2000 used by cg_spill.cxx trace
}

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

//  $Revision: 1.13 $
//  $Date: 05/12/05 08:59:09-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_bb.h $

//  Description:
//
//      GRA associats additional information with BBs.  When it uses
//      course grained interference (as when coloring the "complement"
//      region), it uses BBs as units of liveness when determining
//      interfernce.
//
//      Basically, GRA_BBs give us:
//
//          1. registers used by any LRANGE live in the block,
//          2. LUNITs for the global LRANGEs that have references in the
//             block,
//          3. local LRANGEs that want registers within the block, and
//          4. access to the underlying BB.
//
//      Additionally, they also provide support for live range splitting,
//      which is essentially the process of dividing the blocks in an
//      LRANGE.

#ifndef GRA_BB_INCLUDED
#define GRA_BB_INCLUDED

#ifndef GRA_BB_RCS_ID
#define GRA_BB_RCS_ID
#ifdef _KEEP_RCS_ID
static char *gra_bb_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_bb.h $ $Revision: 1.13 $";
#endif
#endif

#include "defs.h"
#include "cgir.h"
#include "bb.h"
#include "register.h"
#include "bb_map.h"
#include "bb_set.h"
#include "gra.h"
#include "gra_lunit.h"
#include "gra_lrange.h"
#include "gra_lrange_vset.h"
#include "lrange_list.h"
#include "lrange_set.h"
#include "tn_map.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "tn_set.h"
#include "gra_loop.h"
#include "gra_lrange_subuniverse.h"

class INTERFERE_DEREF;
typedef INTERFERE_DEREF* INTERFERE;

enum GBB_FLAG {
  GRA_BB_FLAGS_clear_value	=  0x0,
  GRA_BB_FLAGS_loop_prolog	=  0x1, // if block borders loop head
  GRA_BB_FLAGS_loop_epilog	=  0x2, // if block borders loop tail
  GRA_BB_FLAGS_region_prolog	=  0x4, // if block borders region entry
  GRA_BB_FLAGS_region_epilog	=  0x8, // if block borders region exit
  GRA_BB_FLAGS_setjmp		=  0x10,// if block contains a call to setjmp
#ifdef TARG_X8664
  GRA_BB_FLAGS_savexmms		=  0x20,// if block ends with savexmms pseudo-op
  GRA_BB_FLAGS_x87_OP		=  0x40,// if block contains x87 OPs
  GRA_BB_FLAGS_mmx_OP		=  0x80,// if block contains mmx OPs
#endif
};

class GRA_BB {
friend class GBB_MGR;
friend class GRA_BB_SPLIT_LIST_ITER;
friend class GRA_BB_LIVE_OUT_LRANGE_ITER;
friend class GRA_BB_RC_LUNIT_ITER;
friend class GRA_BB_LOCAL_LRANGE_ITER;
private:
  BB*           bb;
  GRA_REGION*   region;
  LRANGE*       local_lranges[ISA_REGISTER_CLASS_MAX+1];
    // Heads of internally linked (through _BB_Local_List) lists of local
    // LRANGEs by ISA_REGISTER_CLASS.
  LUNIT*        lunits[ISA_REGISTER_CLASS_MAX+1];
    // Heads of internally linked (through _BB_List) list of LUNITs by
    // ISA_REGISTER_CLASS.
  INTERFERE     global_lranges[ISA_REGISTER_CLASS_MAX+1];
    // All the complement LRANGEs live in the block by register class.
  REGISTER_SET  registers_used[ISA_REGISTER_CLASS_MAX+1];
    // the set of registers used in this <gbb> for <rc>.
  REGISTER_SET  glue_registers_used[ISA_REGISTER_CLASS_MAX+1];
    // the set of registers used in this <gbb> and <rc> only in glue copy 
    // references.
#ifdef KEY
  REGISTER_SET  registers_referenced[ISA_REGISTER_CLASS_MAX+1];
    // the set of registers referenced in this <gbb> for <rc>.
#endif
  GRA_BB*       region_next;
    // Used by GRA_REGION's to keep track of their blocks.
  INT32         split_mark;
    // Used with GRA_split_mark_counter to invalidate the _split fields
    // quickly.  If _split_mark != GRA_split_mark_counter, no _split field
    // has yet been set since the new split was begun.
  LUNIT*        split_lunit;
  GRA_BB*       split_list_next;
  float		split_priority; // This blocks part of the priority if it winds 
				// up in the allocated live range on its border.
  float		split_spill_cost; // This blocks part in the spill cost if it 
			  // winds up in the allocated live range on its border.
  INT		split_entry_count; // Count of predecessor blocks that are part 
				   // of the live range, but not yet added.
  INT		split_exit_count;  // Count of successor blocks that are part 
				   // of the live range, but not yet added.
  INT		split_succ_border_count; // Count of the successor blocks that 
					 // are on the border of the split.
  INT		split_lunit_count;
  LRANGE_SET*   spill_above[ISA_REGISTER_CLASS_MAX+1];
    // set of live ranges spilled at top of <gbb> for <rc>
  LRANGE_SET*   restore_below[ISA_REGISTER_CLASS_MAX+1];
    // set of live ranges restored at bottom of <gbb> for <rc>
  LRANGE_LIST*  unpreferenced_wired_lranges[ISA_REGISTER_CLASS_MAX+1];
    // Keeps track of wired local lranges that are not preferenced
    // to anything.  These are kept out of the normal list of local
    // live ranges so that they do not appear in the conflict graph.
    // This is for compile time.  As wired live ranges are colored
    // before other live ranges, and these are not preferenced, there
    // is not any benefit from having them in the conflict graph.
  union {
    // Set up as a union to enable cheap clearing.
    INT32       i32;        // Clear i32 to clear all of the following:
    struct { // Support for live range splitting
      mBOOL     in_alloc_lrange;
      mBOOL     queued;
      mBOOL     has_load;
      mBOOL     has_store;
    }           mb;
  }             split_boolean_properties;
  INT32         one_set_mark;
  GTN_SET*      needs_a_register;
    // Set of GTN that require a register in <gbb>.  (Well almost.
    // Some members of this set may be constants of integer zeros or
    // some such unclean thing that really doesn't need a register.)
    // This is the union of of the set of TN's needing a register on
    // entry to the block and the set requiring a register on exit
    // from the block.  The TNs requiring a register on entry to the
    // block are those that are both live into the block and which
    // have a reaching definition at the start of the block.
    // Similarly, for the exit of the block.
  mINT8         local_lrange_count[ISA_REGISTER_CLASS_MAX+1];
    // the number of local LRANGEs for this <gbb> and <rc>.
  GRA_LOOP*	loop;
  UINT8		flags;
#ifdef KEY
  // ------------ Support optimizing for boundary BBs. ------------
  mUINT16	OPs_count;	// Number of OPs in the BB.
  REGISTER_SET  usage_live_in[ISA_REGISTER_CLASS_MAX+1];
  REGISTER_SET  usage_live_out[ISA_REGISTER_CLASS_MAX+1];
  mUINT16	usage_start_index[ISA_REGISTER_CLASS_MAX+1][REGISTER_MAX+1];
  mUINT16	usage_end_index[ISA_REGISTER_CLASS_MAX+1][REGISTER_MAX+1];
    // While registers_used indicate if a register is used anywhere in the BB,
    // the following give more detail about how the registers are used in the
    // BB:
    //   usage_live_in		which usages are live in
    //   usage_live_out		which usages are live out
    //   usage_start_index	OP index in the BB where the usage begins
    //   usage_end_index	OP index in the BB where the usage ends
    // The usage_start_index and usage_end_index could be <, >, or = relative
    // to each other, depending on the live-in and live-out properties of the
    // usage.  Index 0 means no such OP.
    //
    // In general, a usage can have many disjoint segments in the BB.  We model
    // usages that have at most one segment that is fully contained in the BB.
    // There are 6 cases:
    //   live-in	(live-in only)
    //		usage_live_in=1, usage_live_out=0
    //		usage_end_index>0
    //   live-out	(live-out only)
    //		usage_live_in=0, usage_live_out=1
    //		usage_start_index>0
    //   pass-thru	(live-in, live-out, continuous)
    //		usage_live_in=1, usage_live_out=1
    //		usage_start_index=0, usage_end_index=0
    //   disjoint	(live-in, live-out, but not continuous)
    //		usage_live_in=1, usage_live_out=1
    //		usage_start_index>0, usage_end_index>0
    //		  usage_start_index >= usage_end_index
    //   contained	(usage fully contained in BB)
    //		usage_live_in=0, usage_live_out=0
    //		usage_start_index>0, usage_end_index>0
    //		  usage_start_index < usage_end_index
    //   empty		(does not appear in BB)
    //		usage_live_in=0, usage_live_out=0
    //		usage_start_index=0, usage_end_index=0
    // If a usage does not fit any of the above patterns, it is because the
    // usage has extra contained segments.  In that case, we combine the
    // contained segment(s) with each succeeding segment until one of the above
    // pattern is reached.  For example, if a usage has these three segments:
    //   segment 1:	live-in
    //   segment 2:	contained
    //   segment 3:	live-out
    // We treat segments 2 and 3 as one segment, resulting in the "disjoint"
    // case.
  // ------------ Support register reclaiming. ------------
  LRANGE*	lrange_owner[ISA_REGISTER_CLASS_MAX+1][REGISTER_MAX+1];
    // The lrange owner of the register in this <gbb>.  In the case of a
    // boundary BB with two lranges sharing the same register, lrange_owner is
    // the latest lranges that was allocated the register, since lrange_owner
    // is updated as the registers are allocated.
#endif

public:
  GRA_BB(void) {}
  ~GRA_BB(void) {}

  // access functions
  BB* Bb(void) 			{ return bb; }
  GRA_REGION* Region(void) 	{ return region; }
  LRANGE *Local_Lranges(ISA_REGISTER_CLASS rc) { return local_lranges[rc]; }
  void Local_Lranges_Set(ISA_REGISTER_CLASS rc, LRANGE *lr) 
				{ local_lranges[rc] = lr; }
  INT32 Local_Lrange_Count(ISA_REGISTER_CLASS rc) 
				{ return local_lrange_count[rc]; }
  void Incr_Local_Lrange_Count(ISA_REGISTER_CLASS rc) 
				{ ++local_lrange_count[rc]; }
  INTERFERE Global_Lranges(ISA_REGISTER_CLASS rc) { return global_lranges[rc]; }
  GRA_BB* Region_Next(void) 	{ return region_next; }
  void Region_Next_Set(GRA_BB *next) { region_next = next; }
  float Split_Priority(void)	{ return split_priority; }
  void Split_Priority_Set(float p) { split_priority = p; }
  INT Split_Entry_Count(void)   { return split_entry_count; }
  void Split_Entry_Count_Set(INT c) { split_entry_count = c; }
  float Split_Spill_Cost(void)	{ return split_spill_cost; }
  void Split_Spill_Cost_Set(float c) { split_spill_cost = c; }
  INT Split_Exit_Count(void)    { return split_exit_count; }
  void Split_Exit_Count_Set(INT c) { split_exit_count = c; }
  INT Split_Succ_Border_Count(void)   { return split_succ_border_count; }
  void Split_Succ_Border_Count_Set(INT c) { split_succ_border_count = c; }
  INT Split_Lunit_Count(void)   { return split_lunit_count; }
  void Split_Lunit_Count_Set(INT c) { split_lunit_count = c; }
  GTN_SET *Needs_A_Register(void) { return needs_a_register; }
  void Needs_A_Register_Set(GTN_SET *s) { needs_a_register = s; }
  LRANGE_SET* Restore_Below(ISA_REGISTER_CLASS rc) { return restore_below[rc]; }
  LRANGE_SET* Spill_Above(ISA_REGISTER_CLASS rc) { return spill_above[rc]; }
  LRANGE_LIST* Unpreferenced_Wired_LRANGEs( ISA_REGISTER_CLASS rc ) {
				    return unpreferenced_wired_lranges[rc]; }
  LRANGE_LIST* Unpreferenced_Wired_LRANGEs_Set(ISA_REGISTER_CLASS rc,
					       LRANGE_LIST *l) {
				    return unpreferenced_wired_lranges[rc] = l;}
  GRA_LOOP* Loop(void) 		{ return loop; }
  void Loop_Set(GRA_LOOP *l) 	{ loop = l; }
  REGISTER_SET Glue_Registers_Used(ISA_REGISTER_CLASS rc) {
  				  return glue_registers_used[rc]; }
  void Make_Glue_Register_Used(ISA_REGISTER_CLASS rc, REGISTER reg ) {
			    glue_registers_used[rc] =
			      REGISTER_SET_Union1(glue_registers_used[rc],reg);}

  void Clear_Flags(void) 	{ flags = GRA_BB_FLAGS_clear_value; }
    //Clear the flags field of a gbb, i.e. all flags unset.
  BOOL Loop_Prolog(void)	{ return flags & GRA_BB_FLAGS_loop_prolog; }
  void Loop_Prolog_Set(void)    { flags |= (UINT) GRA_BB_FLAGS_loop_prolog; }
  BOOL Loop_Epilog(void)	{ return flags & GRA_BB_FLAGS_loop_epilog; }
  void Loop_Epilog_Set(void)    { flags |= (UINT) GRA_BB_FLAGS_loop_epilog; }
  BOOL Region_Prolog(void)	{ return flags & GRA_BB_FLAGS_region_prolog; }
  void Region_Prolog_Set(void)  { flags |= (UINT) GRA_BB_FLAGS_region_prolog; }
  BOOL Region_Epilog(void)	{ return flags & GRA_BB_FLAGS_region_epilog; }
  void Region_Epilog_Set(void)  { flags |= (UINT) GRA_BB_FLAGS_region_epilog; }
  BOOL Setjmp(void)		{ return flags & GRA_BB_FLAGS_setjmp; }
  void Setjmp_Set(void)    	{ flags |= (UINT) GRA_BB_FLAGS_setjmp; }
#ifdef TARG_X8664
  BOOL Savexmms(void)		{ return flags & GRA_BB_FLAGS_savexmms; }
  void Savexmms_Set(void)    	{ flags |= (UINT) GRA_BB_FLAGS_savexmms; }
  BOOL x87_OP(void)		{ return flags & GRA_BB_FLAGS_x87_OP; }
  void x87_OP_Set(void)    	{ flags |= (UINT) GRA_BB_FLAGS_x87_OP; }
  BOOL mmx_OP(void)		{ return flags & GRA_BB_FLAGS_mmx_OP; }
  void mmx_OP_Set(void)    	{ flags |= (UINT) GRA_BB_FLAGS_mmx_OP; }
#endif
#ifdef KEY
  // Return TRUE if BB has OPs the clobber the register class RC.
  BOOL Clobbers_Reg_Class(ISA_REGISTER_CLASS rc) {
#ifdef TARG_X8664
    return ((rc == ISA_REGISTER_CLASS_x87 && mmx_OP()) ||
	    (rc == ISA_REGISTER_CLASS_mmx && x87_OP()));
#else
    return FALSE;
#endif
  }
  mUINT16 OPs_Count(void)	{ return OPs_count; }
  mUINT16 Usage_Start_Index(ISA_REGISTER_CLASS rc, REGISTER reg)
				{ return usage_start_index[rc][reg]; }
  void Usage_Start_Index_Set(ISA_REGISTER_CLASS rc, REGISTER reg, mUINT16 index)
				{ usage_start_index[rc][reg] = index; }
  mUINT16 Usage_End_Index(ISA_REGISTER_CLASS rc, REGISTER reg)
				{ return usage_end_index[rc][reg]; }
  void Usage_End_Index_Set(ISA_REGISTER_CLASS rc, REGISTER reg, mUINT16 index)
				{ usage_end_index[rc][reg] = index; }
  BOOL Is_Usage_Live_In(ISA_REGISTER_CLASS rc, REGISTER reg)
	{ return REGISTER_SET_MemberP(usage_live_in[rc], reg); }
  void Usage_Live_In_Set(ISA_REGISTER_CLASS rc, REGISTER reg)
	{ usage_live_in[rc] = REGISTER_SET_Union1(usage_live_in[rc], reg); }
  void Usage_Live_In_Clear(ISA_REGISTER_CLASS rc, REGISTER reg)
	{ usage_live_in[rc] = REGISTER_SET_Difference1(usage_live_in[rc], reg);}
  BOOL Is_Usage_Live_Out( ISA_REGISTER_CLASS rc, REGISTER reg)
	{ return REGISTER_SET_MemberP(usage_live_out[rc], reg); }
  void Usage_Live_Out_Set(ISA_REGISTER_CLASS rc, REGISTER reg)
	{ usage_live_out[rc] = REGISTER_SET_Union1(usage_live_out[rc], reg); }
  void Usage_Live_Out_Clear(ISA_REGISTER_CLASS rc, REGISTER reg)
	{usage_live_out[rc] = REGISTER_SET_Difference1(usage_live_out[rc],reg);}
    // Is the given <lrange> live-out of the given <gbb>?
  void Set_LRANGE_Owner(ISA_REGISTER_CLASS rc, REGISTER reg, LRANGE *lrange)
	{ lrange_owner[rc][reg] = lrange; }
  LRANGE *Get_LRANGE_Owner(ISA_REGISTER_CLASS rc, REGISTER reg)
	{ return lrange_owner[rc][reg]; }
#endif

  // inline functions
  GRA_BB* Split_List_Push( GRA_BB* new_elt ) { new_elt->split_list_next = this;
		  			       return new_elt; }
    // Push <new_elt> onto the the internally linked _Split_List
    // headed by <head> and return <new_elt> which will be the new_elt
    // head.  See the iterator type GRA_BB_SPLIT_LIST_ITER.
  float Freq(void) { if (OPT_Space) 
		       return BB_freq(bb) + GRA_spill_count_factor;
		     else return BB_freq(bb); }
    // Under OPT:space=on, we adjust the frequency upward
    // by adding in a "fudge factor".  The idea is
    // that we want spill code to be expensive regardless of whether or
    // not the block is infrequently executed.  This "fudge factor" is
    // controlled via -GRA:spill_count_factor=<float value>
  BOOL Is_Live_Out_LRANGE( LRANGE* lrange ) {
		  return GTN_SET_MemberP(BB_live_out(bb),lrange->Tn()); }
    // Is the given <lrange> live-out of the given <gbb>?
  BOOL Is_Live_In_LRANGE( LRANGE* lrange ) {
		  return GTN_SET_MemberP(BB_live_in(bb),lrange->Tn()); }
    // Is the given <lrange> live-in of the given <gbb>?

  // non-inlined
  INT Register_Girth( ISA_REGISTER_CLASS rc );
  void Create_Local_LRANGEs(ISA_REGISTER_CLASS  cl, INT32 coun);
  LRANGE* Create_Wired_LRANGE(ISA_REGISTER_CLASS  cl, REGISTER reg);
  void Create_Global_Interferences(void);
  void Rename_TN_References( TN*     orig_tn, TN*     new_tn);
  void Add_Live_Out_LRANGE( LRANGE* lrange );
  void Add_Live_In_LRANGE( LRANGE* lrange );
  void Remove_Live_In_LRANGE( LRANGE* lrange );
  void Remove_Live_Out_LRANGE( LRANGE* lrange );
  INT32 Global_Live_Lrange_Count( ISA_REGISTER_CLASS  rc);
  void Replace_Global_Interference( LRANGE* old_lr, LRANGE* new_lr );
  BOOL Has_Multiple_Predecessors(void);
  BOOL Region_Is_Complement(void);
  void Add_LUNIT(LUNIT *lunit);
  void Make_Register_Used(ISA_REGISTER_CLASS rc, REGISTER reg,
			  LRANGE* lrange = NULL, BOOL reclaim = FALSE);
  REGISTER_SET Registers_Used(ISA_REGISTER_CLASS  rc);
#ifdef KEY
  void Make_Register_Referenced(ISA_REGISTER_CLASS rc, REGISTER reg,
				LRANGE* lrange = NULL);
  REGISTER_SET Registers_Referenced(ISA_REGISTER_CLASS  rc);
#endif
  BOOL Spill_Above_Check(LRANGE *lrange);
  void Spill_Above_Set(LRANGE *lrange);
  void Spill_Above_Reset(LRANGE *lrange);
  BOOL Restore_Below_Check(LRANGE *lrange);
  void Restore_Below_Set(LRANGE *lrange);
  void Restore_Below_Reset(LRANGE *lrange);
  BOOL Is_Region_Block(BOOL swp_too);
  BOOL Is_Region_Entry_Block(void);
  void Check_Loop_Border(void);
};


// manages everything related to GRA_BB's
class GBB_MGR {
  INT32 one_set_counter;
  INT32 split_mark_counter;
  INT32 alloc_count;
  INT32 wired_local_count;
  BB_MAP map;   // Weird typedef includes the '*'.
  BB_SET *blocks_with_calls; // BB_SET containing those bb's that have calls.
  BB_SET *blocks_with_rot_reg_clob; // bb's that clobber rotating registers
#ifdef TARG_X8664
  BB_SET *blocks_with_x87_OP; // BB_SET containing those bb's that have x87 OPs.
  BB_SET *blocks_with_mmx_OP; // BB_SET containing those bb's that have MMX OPs.
#endif

public:
  GBB_MGR(void) {};
  ~GBB_MGR(void) {};

  // access functions
  BB_SET *Blocks_With_Calls(void)	{ return blocks_with_calls; }
  BB_SET *Blocks_With_Rot_Reg_Clob(void){ return blocks_with_rot_reg_clob; }
#ifdef TARG_X8664
  BB_SET *Blocks_With_x87_OP(void)	{ return blocks_with_x87_OP; }
  BB_SET *Blocks_With_mmx_OP(void)	{ return blocks_with_mmx_OP; }
#endif
  void Incr_Wired_Local_Count(void)	{ ++wired_local_count; }
  INT Alloc_Count(void)			{ return alloc_count; }

  // inlined
  GRA_BB* Get( BB* bb ) 	{ return (GRA_BB*) BB_MAP_Get(map,bb); }
    // Return the GRA_BB associated with <bb> or NULL if there is no such.
  void Clear_One_Set( void ) { ++one_set_counter; }
  BOOL One_Set_MemberP(GRA_BB *gbb){return gbb->one_set_mark== one_set_counter;}
  void One_Set_Union1(GRA_BB *gbb) { gbb->one_set_mark = one_set_counter; }
  void One_Set_Difference1(GRA_BB *gbb) {gbb->one_set_mark = one_set_counter-1;}
    // The above functions support a simple single set of GRA_BBs
    // with a very cheap implementation of a limited number of
    // useful operations.  Notice there is exactly one of these
    // sets (hence the name) in existence at a time and each
    // GRA_BB is either in or out.
  void Possibly_Clear_Split_Properties(GRA_BB *gbb) {
	// Check whether <gbb>'s split properties have been cleared since the
	// current split was begun and clear its split properties if not.
	  if ( gbb->split_mark != split_mark_counter ) {
	    gbb->split_mark = split_mark_counter;
	    gbb->split_boolean_properties.i32 = 0;// Hack: 4 for the price of 1.
	    gbb->split_lunit = NULL;
	  } 
	}
  void Begin_Split(void)	{ ++split_mark_counter; }
    // Clears all the GRA_BB_Split_ properties described below.  (Cheaply)
  BOOL Split_In_Alloc_LRANGE(GRA_BB *gbb) {
		  return gbb->split_mark == split_mark_counter &&
			 gbb->split_boolean_properties.mb.in_alloc_lrange; }
    // Has <gbb>'s in_alloc_lrange property been set since
    // the current split was begun?
  void Split_In_Alloc_LRANGE_Set(GRA_BB *gbb) {
		  Possibly_Clear_Split_Properties(gbb);
		  gbb->split_boolean_properties.mb.in_alloc_lrange = TRUE; }
  void Split_In_Alloc_LRANGE_Clear(GRA_BB *gbb) {
		  Possibly_Clear_Split_Properties(gbb);
		  gbb->split_boolean_properties.mb.in_alloc_lrange = FALSE; }
  BOOL Split_Queued(GRA_BB *gbb) {
		  return gbb->split_mark == split_mark_counter &&
			 gbb->split_boolean_properties.mb.queued; }
    // Has <gbb>'s queued property been set since the current split was begun?
  void Split_Queued_Set(GRA_BB *gbb) {
		  Possibly_Clear_Split_Properties(gbb);
		  gbb->split_boolean_properties.mb.queued = TRUE; }
  BOOL Split_Has_Load(GRA_BB *gbb) {
		  return gbb->split_mark == split_mark_counter &&
			 gbb->split_boolean_properties.mb.has_load; }
    // Has <gbb>'s _Split_Has_Load property been set since
    // the current split was begun?  This is done to indicate
    // that a restore for the LRANGE has been added to the top of GBB.
  void Split_Has_Load_Set(GRA_BB *gbb) {
		  Possibly_Clear_Split_Properties(gbb);
		  gbb->split_boolean_properties.mb.has_load = TRUE; }
  BOOL Split_Has_Store(GRA_BB *gbb) {
		  return gbb->split_mark == split_mark_counter &&
			 gbb->split_boolean_properties.mb.has_store; }
    // Has <gbb>'s _Has_Store property been set since
    // the current split was begun?  This is done to indicate
    // that a restore for the LRANGE has been added to the top of GBB.
  void Split_Has_Store_Set(GRA_BB *gbb) {
		  Possibly_Clear_Split_Properties(gbb);
		  gbb->split_boolean_properties.mb.has_store = TRUE; }
  LUNIT* Split_LUNIT(GRA_BB *gbb) {
		  if ( gbb->split_mark != split_mark_counter )
		    return NULL;
		  else return gbb->split_lunit; }
    // Return the Split_LUNIT set for <gbb> since the
    // current split was begin, or NULL if there no LUNIT has
    // been so associated.
  void Split_LUNIT_Set(GRA_BB *gbb,  LUNIT*  lunit ) {
		  Possibly_Clear_Split_Properties(gbb);
		  gbb->split_lunit = lunit; }
    // Make <gbb>'s Split_LUNIT be <lunit>.

  // non-inlined
  void Initialize(void);
  void Finalize(void);
  GRA_BB* Create(BB *bb, GRA_REGION* region);
};

extern GBB_MGR gbb_mgr;
#ifdef TARG_X8664
extern OP *gra_savexmms_op;
#endif

// Use to iterate over the elements of an internally linked _Split_List of BBs.
class GRA_BB_SPLIT_LIST_ITER {
  GRA_BB* current;
public:
  GRA_BB_SPLIT_LIST_ITER(void) {}
  ~GRA_BB_SPLIT_LIST_ITER(void) {}

  void Init(GRA_BB *gbb)	{ current = gbb; }
  BOOL Done(void)		{ return current == NULL; }
  GRA_BB *Current(void)		{ return current; }
  void Step(void)		{ current = current->split_list_next; }
};

// Use to iterate forward over the OPs in a GRA_BB.
class GRA_BB_OP_FORWARD_ITER {
  OP* current;
public:
  GRA_BB_OP_FORWARD_ITER(void) {}
  ~GRA_BB_OP_FORWARD_ITER(void) {}

  void Init(GRA_BB *gbb)	{ current = BB_first_op(gbb->Bb()); }
  BOOL Done(void)		{ return current == NULL; }
  OP *Current(void)		{ return current; }
  void Step(void)		{ current = OP_next(current); }
};

// Use to iterate backward over the OPs in a GRA_BB.
class GRA_BB_OP_BACKWARD_ITER {
  OP* current;
public:
  GRA_BB_OP_BACKWARD_ITER(void) {}
  ~GRA_BB_OP_BACKWARD_ITER(void) {}

  void Init(GRA_BB *gbb)	{ current = BB_last_op(gbb->Bb()); }
  BOOL Done(void)		{ return current == NULL; }
  OP *Current(void)		{ return current; }
  void Step(void)		{ current = OP_prev(current); }
};

// Use to iterate over a GRA_BB's flow successors or predecessors
// (GRA_BBs, not BBs).
class GRA_BB_FLOW_NEIGHBOR_ITER {
  BBLIST*   bblist;
public:
  GRA_BB_FLOW_NEIGHBOR_ITER(void) {}
  ~GRA_BB_FLOW_NEIGHBOR_ITER(void) {}

  void Succs_Init(GRA_BB *gbb)	{ bblist = BB_succs(gbb->Bb()); }
  void Preds_Init(GRA_BB *gbb)	{ bblist = BB_preds(gbb->Bb()); }
  BOOL Done(void)		{ return bblist == NULL; }
  GRA_BB *Current(void)		{ return gbb_mgr.Get(BBLIST_item(bblist)); }
  void Step(void)		{ bblist = BBLIST_next(bblist); }
};

// Use to iterate over the LUNITs for a GRA_BB and REGISTER_CLASS.
class GRA_BB_RC_LUNIT_ITER: public LUNIT_BB_LIST_ITER {
public:
  void Init(GRA_BB *gbb, ISA_REGISTER_CLASS rc) { 
				LUNIT_BB_LIST_ITER::Init(gbb->lunits[rc]); }
};

// Use to iterate over the local LRANGEs for a GRA_BB and REGISTER_CLASS
class GRA_BB_LOCAL_LRANGE_ITER: public LRANGE_BB_LOCAL_LIST_ITER {
//  We're just adding the _Init function to the LRANGE_BB_LOCAL_LIST_ITER
//  because we have the heads of the various lists.
public:
  void Init(GRA_BB *gbb, ISA_REGISTER_CLASS rc) { 
		      LRANGE_BB_LOCAL_LIST_ITER::Init(gbb->local_lranges[rc]);}
};

// Use to iterate over the LRANGEs live out (and defreach out) of a block
class GRA_BB_LIVE_OUT_LRANGE_ITER {
  TN* current_tn;
  BB* bb;
public:
  GRA_BB_LIVE_OUT_LRANGE_ITER(void) {}
  ~GRA_BB_LIVE_OUT_LRANGE_ITER(void) {}

  void Init(GRA_BB *gbb)	{ 
      bb = gbb->bb;
      current_tn = GTN_SET_Intersection_Choose(BB_live_out(gbb->bb),
						     BB_defreach_out(gbb->bb));
      while (current_tn != TN_SET_CHOOSE_FAILURE &&
	     lrange_mgr.Get(current_tn) == NULL) {
	current_tn = GTN_SET_Intersection_Choose_Next(BB_live_out(gbb->bb),
				    BB_defreach_out(gbb->bb), current_tn);
      }
    }
  BOOL Done(void)		{ return current_tn == TN_SET_CHOOSE_FAILURE; }
  LRANGE *Current(void)		{ return lrange_mgr.Get(current_tn); }
  void Step(void)		{ 
	do {
	  current_tn = GTN_SET_Intersection_Choose_Next(BB_live_out(bb),
					      BB_defreach_out(bb), current_tn);
	}
	while (current_tn != TN_SET_CHOOSE_FAILURE && 
	       lrange_mgr.Get(current_tn) == NULL );
      }
};

#endif

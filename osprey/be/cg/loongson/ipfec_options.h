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

//***********************************************************************
//
// extern SKIPLIST* IPFEC_Build_Skiplist ( OPTION_LIST *olist );
//   Build a SKIPLIST from OPTION_LIST. The result can be queried
//   by  IPFEC_Query_Skiplist ( SKIPLIST *skip, INT32 elmt ). To 
//   know more details of references, go to the ipfec_options.cxx
// 
// extern IPFEC_Query_Skiplist ( SKIPLIST *skip, INT32 elmt );
//   Query whether a given elememt is in the skip list and return a boolean value.
//   To know more details of references, go to the ipfec_options.cxx
// 
//***********************************************************************

//***********************************************************************
//
// If you want to add an option, just do as the following.
// Add an option which only has boolean value or something else. But
// a list of options is a special case.
// Step1:
//   Add a group's name in cgdriver.cxx, like static OPTION_DESC
//   Options_VT[] ={...}. This step is not necessary. If you want to
//   add a new one to an existing group, this step and step 2 can be 
//   ignored.
// Step2:
//   Add the group's name to the OPTION_GROUP Cg_Option_Groups[]
//   = {...}. OPTION_GROUP is in file "cgdriver.cxx"
// Step3:
//   Add the option's name to the group.
// Step4:
//   Add the corresponding variable to ipfec_options.h, an extern qualifier is
//   wanted. Otherwise, add the variable to the ipfec_options.cxx
//
// NOTICE: The old way of adding new options can works too.
// 
// Adding options with a kind of list needs more steps.
//
// Step1-3 is the same as above. 
//   Notice, the OVK_LIST and OV_SHY must be the first two parameters
//   of OPTION_DESC, and the group of option is Option_Skip .The name of 
//   the option must end with _before, _after or _equal ( abbreviation must 
//   be _b, _a or _e ). If you do not do so, all the functions can not work normally.
// Step4
//   After doing as above, you should define a SKIPLIST variable which
//   contains the info and can be queried.
// Step5
//   In the function of CG_Process_Command_Line in cgdriver.cxx, you will add a
//   sentence ,like locs_skip_bb = IPFEC_Build_Skiplist(raw_locs_skip_bb).
//   So you get the info you want. Notice: you should make sure that the options
//   you add are cg options.
// Step6
//    everywhere of cg, you can query the number( perhaps bb id, region id 
//   or something else). Include ipfec_options.h first. Using IPFEC_Query_
//   Skiplist(...) ,judge whether the given number is in the list or not.
// 
//*****************************************************************************************

#ifndef ipfec_options_INCLUDED
#define ipfec_options_INCLUDED

#include <flags.h>

// Ipfec flags.
extern BOOL CG_Enable_Ipfec_Phases;
extern BOOL EMIT_count_cycles;
extern INT32 IPFEC_Stacked_Cut_Num;
extern INT32 IPFEC_Stacked_Spill_Num;
extern BOOL IPFEC_Enable_Region_Formation;
extern BOOL IPFEC_Enable_Region_Decomposition;
extern BOOL IPFEC_Enable_Tail_Duplication;
extern BOOL IPFEC_Enable_Exit_Probability;
extern BOOL IPFEC_Enable_If_Conversion;
extern BOOL IPFEC_Force_If_Conv;
extern BOOL IPFEC_Force_Para_Comp_Gen;
extern BOOL IPFEC_Para_Comp_Gen;
extern BOOL IPFEC_Combine_Exit;
extern BOOL IPFEC_Disable_Merge_BB;
extern BOOL IPFEC_Enable_PRDB;
extern BOOL IPFEC_Enable_BB_Verify;
extern BOOL IPFEC_Enable_Opt_after_schedule;
extern BOOL IPFEC_Enable_Prepass_GLOS;
extern BOOL IPFEC_Enable_Postpass_GLOS;
extern BOOL IPFEC_Enable_Prepass_LOCS;
extern BOOL IPFEC_Enable_Postpass_LOCS;
extern BOOL IPFEC_Enable_Fill_Delay_Slot;
extern BOOL IPFEC_Enable_Fill_Delay_Slot_Mov;
extern BOOL IPFEC_Enable_Fill_Delay_Slot_Dup;
extern BOOL IPFEC_Enable_Speculation;
extern BOOL IPFEC_Enable_Data_Speculation;
extern BOOL IPFEC_Enable_Cntl_Speculation;
extern BOOL IPFEC_Enable_Edge_Profile;
extern BOOL IPFEC_Enable_Value_Profile;
extern BOOL IPFEC_Enable_Stride_Profile;
extern BOOL IPFEC_Enable_Cache_Profile;
extern BOOL IPFEC_Enable_Random_Prob;
extern BOOL IPFEC_Enable_Edge_Profile_Annot;
extern BOOL IPFEC_Enable_Value_Profile_Annot;
extern BOOL IPFEC_Enable_Stride_Profile_Annot;
extern BOOL IPFEC_Enable_Cache_Profile_Annot;
extern BOOL Enable_Cache_Profile_Annot;
extern BOOL IPFEC_Glos_Reg_Pressure_Aware;
extern BOOL IPFEC_Glos_Split_Entry_BB;
extern BOOL IPFEC_Glos_Split_Exit_BB;
extern BOOL IPFEC_Glos_Enable_P_Ready_Code_Motion;
extern BOOL IPFEC_Glos_Motion_Across_Calls ;
extern BOOL IPFEC_Glos_Code_Motion_Across_Nested_Rgn;
extern BOOL IPFEC_Glos_Enable_Cntl_Spec_If_Converted_Code;
extern BOOL IPFEC_Glos_Enable_Renaming;
extern BOOL IPFEC_Stress_Spec;
extern BOOL IPFEC_Adjust_Variable_Latency ;
extern BOOL IPFEC_Enable_Multi_Branch ;
extern BOOL IPFEC_Enable_Pre_Multi_Branch ;
extern BOOL IPFEC_Enable_Post_Multi_Branch ;
extern INT32 IPFEC_Enable_Stride_Prefetch;
extern BOOL IPFEC_Enable_LICM;

extern char *Instru_File_Name;
extern char *Fb_File_Name;
extern char *Value_Instru_File_Name;
extern char *Stride_Instru_File_Name;
extern char *Cache_Instru_File_Name;
extern UINT32 Value_Instr_Range;
extern UINT32 Value_Instr_Pu_Id;
extern UINT64 Value_Instr_Pu_Id_Mask;
extern char *Value_Fb_File_Name;
extern char *Stride_Fb_File_Name;
extern char *Cache_Fb_File_Name;

// This is the flag to control compressed template using. We do not
// use compressed template if it is turned off.
extern BOOL IPFEC_Enable_Compressed_Template;
extern BOOL IPFEC_Enable_Pre_Bundling;
extern BOOL IPFEC_Force_CHK_Fail;
extern BOOL IPFEC_Enable_Cascade;
extern BOOL IPFEC_Hold_Uses;
extern BOOL IPFEC_Profitability;
extern BOOL IPFEC_Chk_Compact;
extern BOOL IPFEC_Enable_Safety_Load;
extern BOOL IPFEC_Enable_Insert_UNAT;

extern char * IPFEC_safe_cntl_spec_prob ;
extern char * IPFEC_unsafe_cntl_spec_prob ;

extern BOOL IPFEC_Enable_Opt_Mem_OP;
extern BOOL IPFEC_Enable_Opt_St_In_Loop;
extern BOOL IPFEC_Enable_Enhanced_LRA;
extern BOOL IPFEC_Enable_Force_Enhanced_LRA;
extern BOOL IPFEC_Enable_Opt_Ld_After_LRA;
extern INT32 IPFEC_Enable_RA_OPT;
extern BOOL IPFEC_Enable_Sorted_GRA;
extern BOOL IPFEC_Enable_FTZ; // enable masking flush-to-zero bit in FSR
extern BOOL IPFEC_NoSched_Divmfhimflo;

// backup IPFEC_... flags
// see ipfec_options.cxx for details
extern INT32 ORC_Stacked_Cut_Num;
extern INT32 ORC_Stacked_Spill_Num;
extern BOOL ORC_Enable_Region_Formation;
extern BOOL ORC_Enable_Region_Decomposition;
extern BOOL ORC_Enable_Tail_Duplication;
extern BOOL ORC_Enable_Exit_Probability;
extern BOOL ORC_Enable_If_Conversion;
extern BOOL ORC_Force_If_Conv;
extern BOOL ORC_Force_Para_Comp_Gen;
extern BOOL ORC_Para_Comp_Gen;
extern BOOL ORC_Combine_Exit;
extern BOOL ORC_Disable_Merge_BB;
extern BOOL ORC_Enable_PRDB;
extern BOOL ORC_Enable_BB_Verify;
extern BOOL ORC_Enable_Opt_after_schedule;
extern BOOL ORC_Enable_Prepass_GLOS;
extern BOOL ORC_Enable_Postpass_GLOS;
extern BOOL ORC_Enable_Prepass_LOCS;
extern BOOL ORC_Enable_Postpass_LOCS;
extern BOOL ORC_Enable_Fill_Delay_Slot;
extern BOOL ORC_Enable_Fill_Delay_Slot_Mov;
extern BOOL ORC_Enable_Fill_Delay_Slot_Dup;
extern BOOL ORC_Enable_Speculation;
extern BOOL ORC_Enable_Data_Speculation;
extern BOOL ORC_Enable_Cntl_Speculation;
extern BOOL ORC_Enable_Edge_Profile;
extern BOOL ORC_Enable_Value_Profile;
extern BOOL ORC_Enable_Stride_Profile;
extern BOOL ORC_Enable_Cache_Profile;
extern BOOL ORC_Enable_Random_Prob;
extern BOOL ORC_Enable_Edge_Profile_Annot;
extern BOOL ORC_Enable_Value_Profile_Annot;
extern BOOL ORC_Enable_Stride_Profile_Annot;
extern BOOL ORC_Enable_Cache_Profile_Annot;
extern BOOL ORC_Glos_Reg_Pressure_Aware;
extern BOOL ORC_Glos_Split_Entry_BB;
extern BOOL ORC_Glos_Split_Exit_BB;
extern BOOL ORC_Glos_Enable_P_Ready_Code_Motion;
extern BOOL ORC_Glos_Motion_Across_Calls ;
extern BOOL ORC_Glos_Code_Motion_Across_Nested_Rgn;
extern BOOL ORC_Glos_Enable_Cntl_Spec_If_Converted_Code;
extern BOOL ORC_Glos_Enable_Renaming;
extern BOOL ORC_Stress_Spec;
extern BOOL ORC_Adjust_Variable_Latency ;
extern BOOL ORC_Enable_Multi_Branch ;
extern BOOL ORC_Enable_Pre_Multi_Branch ;
extern BOOL ORC_Enable_Post_Multi_Branch ;
extern INT32 ORC_Enable_Stride_Prefetch;
extern BOOL ORC_Enable_Cache_Analysis;


extern BOOL ORC_Enable_Compressed_Template;
extern BOOL ORC_Enable_Pre_Bundling;
extern BOOL ORC_Force_CHK_Fail;
extern BOOL ORC_Enable_Cascade;
extern BOOL ORC_Hold_Uses;
extern BOOL ORC_Profitability;
extern BOOL ORC_Chk_Compact;
extern BOOL ORC_Enable_Safety_Load;
extern BOOL ORC_Enable_Insert_UNAT;
extern BOOL ORC_Enable_LICM;

extern char * ORC_safe_cntl_spec_prob ;
extern char * ORC_unsafe_cntl_spec_prob ;

extern BOOL CG_Enable_Opt_Mem_OP;
extern BOOL CG_Enable_Opt_St_In_Loop;
extern BOOL CG_Enable_Enhanced_LRA;
extern BOOL CG_Enable_Force_Enhanced_LRA;
extern BOOL CG_Enable_Opt_Ld_After_LRA;
extern INT32 CG_Enable_RA_OPT;
extern BOOL CG_Enable_Sorted_GRA;
extern BOOL CG_Enable_FTZ; // enable masking flush-to-zero bit in FSR
extern BOOL CG_NoSched_Divmfhimflo;

//end of backup

// Flags for research experiments
typedef enum {
  Sched_care_nothing,  /* Machine model always alow scheduling */
  Sched_care_width,    /* Only care about machine width when scheduling */
  Sched_care_resource, /* Care machine width and resource when scheduling */
  Sched_care_bundle    /* Do integerated bundling when scheduling */
} SCHED_CARE;
extern INT32 IPFEC_sched_care_machine;
extern INT32 ORC_sched_care_machine;

// VT (Visualization Tool) flags.
extern BOOL VT_Enable_BB_OP;
extern BOOL VT_Enable_Global_CFG;
extern BOOL VT_Enable_Regional_CFG;
extern BOOL VT_Enable_Region_Tree;
extern BOOL VT_Enable_BB_DAG;
extern BOOL VT_Enable_Regional_DAG;
extern BOOL VT_Enable_Partition_Graph;
extern BOOL VT_Enable_CFG_Label;
extern BOOL VT_Enable_DAG_BR;

// The SKIPKIND and SKIPLIST are just copied from config.h 
// These kinds are for the building skiplist.

typedef enum {
  SK_NONE,	/* End of list */
  SK_AFTER,	/* Values after this one */
  SK_BEFORE,	/* Values before this one */
  SK_EQUAL	/* Just this one */
} SKIPKIND;

typedef class skiplist {
public:
  mINT32 _size;	/* Number of elements */
  mINT8 *_kind;	/* Array of kinds */
  mINT32 *_val;	/* Array of values */
  UINT _flags;  /* flags of skiplist */

  skiplist():
    _size(0),
    _kind(NULL),
    _val(NULL),
    _flags(0) {}

  ~skiplist() {}

  mINT32&  SKIPLIST_size()		      { return _size; }
  mINT8*&  SKIPLIST_kind_vec()		  { return _kind; }
  mINT8&   SKIPLIST_kind(UINT i)		{ return _kind[i]; }
  void     Set_SKIPLIST_kind(UINT i, mINT8 v)	{ _kind[i] = v; }
  mINT32*& SKIPLIST_val_vec()		    { return _val; }
  mINT32&  SKIPLIST_val(UINT i)	    { return _val[i]; }
  UINT&    SKIPLIST_flags()         { return _flags; }
}SKIPLIST;

//The flags of skiplist
#define SL_GLOBAL 0x0001    /* The list is global */
#define SL_LOCAL  0X0002    /* The list is local */

#define SL_global(sl)       (sl->SKIPLIST_flags() & SL_GLOBAL)
#define SL_local(sl)        (sl->SKIPLIST_flags() & SL_LOCAL)

#define Set_SL_global(sl)   (sl->SKIPLIST_flags() |= SL_GLOBAL)
#define Set_SL_local(sl)    (sl->SKIPLIST_flags() |= SL_LOCAL)

#define Reset_SL_global(sl) (sl->SKIPLIST_flags() &= ~SL_GLOBAL)
#define Reset_SL_local(sl)  (sl->SKIPLIST_flags() &= ~SL_LOCAL)


// The vasiable is declared there. Note the qualifier extern is 
// necessary. If option likes ***skip_b, the SKIPLIST is needed.


//Skip_list for if conversion
extern OPTION_LIST *raw_if_conv_skip_rgn;
extern SKIPLIST *if_conv_skip_rgn;
extern OPTION_LIST *raw_if_conv_skip_area;
extern SKIPLIST *if_conv_skip_area;
extern OPTION_LIST *raw_if_conv_skip_PU;
extern SKIPLIST *if_conv_skip_PU;


//Skip_list for speculation
extern OPTION_LIST *raw_spec_skip_bb;
extern SKIPLIST *spec_skip_bb;
extern OPTION_LIST *raw_spec_skip_rgn;
extern SKIPLIST *spec_skip_rgn;
extern OPTION_LIST *raw_spec_skip_op;
extern SKIPLIST *spec_skip_op;
extern OPTION_LIST *raw_spec_skip_PU;
extern SKIPLIST *spec_skip_PU;

//Skip_list for micro_scheduling
extern OPTION_LIST *raw_msched_skip_bb;
extern SKIPLIST *msched_skip_bb;
extern OPTION_LIST *raw_msched_skip_rgn;
extern SKIPLIST *msched_skip_rgn;

//Skip_list for scheduling(global or local)
extern OPTION_LIST *raw_glos_skip_rgn;
extern SKIPLIST *glos_skip_rgn;
extern OPTION_LIST *raw_glos_skip_bb;
extern SKIPLIST *glos_skip_bb;
extern OPTION_LIST *raw_glos_skip_op;
extern SKIPLIST *glos_skip_op;
extern OPTION_LIST *raw_pre_glos_skip_PU;
extern SKIPLIST *pre_glos_skip_PU;
extern OPTION_LIST *raw_post_locs_skip_PU;
extern SKIPLIST *post_locs_skip_PU;
extern OPTION_LIST *raw_locs_skip_bb;
extern SKIPLIST *locs_skip_bb;
extern OPTION_LIST *raw_dss_skip_bb;
extern SKIPLIST *dss_skip_bb;
extern OPTION_LIST *raw_glos_rename_skip_bb;
extern SKIPLIST *glos_rename_skip_bb;
extern OPTION_LIST *raw_glos_rename_skip_op;
extern SKIPLIST *glos_rename_skip_op;

//Skip_list for multi_branch
extern OPTION_LIST *raw_mlbr_skip_bb;
extern SKIPLIST *mlbr_skip_bb;

//Skip_list for PRDB 
extern OPTION_LIST *raw_PRDB_skip_PU;
extern SKIPLIST *PRDB_skip_PU;

// testing for latency2 added by llx
extern OPTION_LIST *raw_latency2;
extern SKIPLIST *latency2;

// Copy ORC_Flags into IPFEC_Flags
void Copy_Ipfec_Flags (void) ;


//Build skiplist
extern SKIPLIST* IPFEC_Build_Skiplist ( OPTION_LIST *olist );

//Query skiplist
extern BOOL IPFEC_Query_Skiplist ( SKIPLIST *skip, INT32 elmt ,INT32 field = -1 );

#endif


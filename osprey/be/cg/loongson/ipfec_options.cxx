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

#include "defs.h"
#include "config.h"
#include "ipfec_options.h"
#include "string.h"
#include "flags.h"
#include "tracing.h"
#include "mempool.h"
#include "cxx_memory.h"

// Ipfec flags.
BOOL CG_Enable_Ipfec_Phases = TRUE;
BOOL EMIT_count_cycles = FALSE;

BOOL IPFEC_Enable_Region_Formation = TRUE;
BOOL IPFEC_Enable_Region_Decomposition = TRUE;
INT32 IPFEC_Enable_Tail_Duplication = FALSE;
INT32 IPFEC_Enable_Exit_Probability = FALSE;
INT32 IPFEC_Stacked_Cut_Num = 0;
INT32 IPFEC_Stacked_Spill_Num = 0;
BOOL IPFEC_Enable_If_Conversion = TRUE;
BOOL IPFEC_Force_If_Conv = FALSE;
BOOL IPFEC_Force_Para_Comp_Gen = FALSE;
BOOL IPFEC_Para_Comp_Gen = TRUE;
BOOL IPFEC_Combine_Exit = FALSE;
BOOL IPFEC_Disable_Merge_BB = FALSE;
BOOL IPFEC_Enable_PRDB= TRUE;
BOOL IPFEC_Enable_BB_Verify = FALSE;
BOOL IPFEC_Enable_Prepass_GLOS = TRUE;
BOOL IPFEC_Enable_Opt_after_schedule = TRUE;
BOOL IPFEC_Enable_Postpass_GLOS = FALSE;
BOOL IPFEC_Enable_Prepass_LOCS = FALSE;
BOOL IPFEC_Enable_Postpass_LOCS = TRUE;
BOOL IPFEC_Enable_Fill_Delay_Slot = TRUE;
BOOL IPFEC_Enable_Fill_Delay_Slot_Mov = TRUE;
BOOL IPFEC_Enable_Fill_Delay_Slot_Dup = TRUE;
BOOL IPFEC_Enable_Speculation = TRUE;
BOOL IPFEC_Enable_Data_Speculation = TRUE;
BOOL IPFEC_Enable_Cntl_Speculation = TRUE;
BOOL IPFEC_Enable_Compressed_Template = FALSE;
INT32 IPFEC_sched_care_machine = Sched_care_resource;
BOOL IPFEC_Glos_Reg_Pressure_Aware = TRUE;
BOOL IPFEC_Stress_Spec             = FALSE;
BOOL IPFEC_Glos_Motion_Across_Calls = FALSE ;
BOOL IPFEC_Glos_Split_Entry_BB      = TRUE ;
BOOL IPFEC_Glos_Split_Exit_BB       = TRUE ;
BOOL IPFEC_Glos_Enable_P_Ready_Code_Motion = TRUE;
BOOL IPFEC_Glos_Code_Motion_Across_Nested_Rgn = TRUE;
BOOL IPFEC_Glos_Enable_Cntl_Spec_If_Converted_Code = TRUE;
BOOL IPFEC_Glos_Enable_Renaming = TRUE;
BOOL IPFEC_Adjust_Variable_Latency  = TRUE ;
BOOL IPFEC_Enable_Edge_Profile = FALSE;
BOOL IPFEC_Enable_Value_Profile = FALSE;
BOOL IPFEC_Enable_Stride_Profile = FALSE;
BOOL IPFEC_Enable_Cache_Profile = FALSE;
BOOL IPFEC_Enable_Random_Prob = FALSE;
BOOL IPFEC_Enable_Edge_Profile_Annot = FALSE;
INT32 IPFEC_Enable_Stride_Prefetch = FALSE;
INT32 Value_Profile_Pu_ID=-1;
UINT32 Value_Instr_Range=0x0000ffff;
UINT32 Value_Instr_Pu_Id=0x0000ffff;
UINT64 Value_Instr_Pu_Id_Mask=0; // if masked, then do not instrumentation for the pu id.
char *Instru_File_Name = "";
char *Fb_File_Name = "";
char *Value_Instru_File_Name = "";
char *Stride_Instru_File_Name = "";
char *Cache_Instru_File_Name = "";
char *Value_Fb_File_Name = "";
char *Stride_Fb_File_Name = "";
char *Cache_Fb_File_Name = "";
BOOL IPFEC_Enable_Value_Profile_Annot = FALSE;
BOOL IPFEC_Enable_Stride_Profile_Annot = FALSE;
BOOL IPFEC_Enable_Cache_Profile_Annot = FALSE;
BOOL Enable_Cache_Profile_Annot = FALSE;
BOOL IPFEC_Enable_Pre_Bundling = TRUE;
BOOL IPFEC_Force_CHK_Fail = FALSE;
BOOL IPFEC_Chk_Compact = TRUE;
BOOL IPFEC_Enable_Cascade = TRUE;
BOOL IPFEC_Hold_Uses = FALSE;
BOOL IPFEC_Enable_Safety_Load = TRUE;
BOOL IPFEC_Enable_Multi_Branch = TRUE;
BOOL IPFEC_Enable_Pre_Multi_Branch = TRUE;
BOOL IPFEC_Enable_Post_Multi_Branch = TRUE;
BOOL IPFEC_Profitability = FALSE;     // to set all flags considering profitability
BOOL IPFEC_Enable_LICM   = TRUE; 
BOOL IPFEC_Enable_Insert_UNAT = TRUE; // insert unat spill and restore code when need

BOOL IPFEC_Enable_Split_bb = FALSE;

BOOL IPFEC_Enable_Opt_Mem_OP = FALSE;
BOOL IPFEC_Enable_Opt_St_In_Loop = FALSE;
BOOL  IPFEC_Enable_Enhanced_LRA = TRUE;
BOOL  IPFEC_Enable_Force_Enhanced_LRA = TRUE;
BOOL  IPFEC_Enable_Opt_Ld_After_LRA = TRUE;
INT32 IPFEC_Enable_RA_OPT = 0;
BOOL IPFEC_Enable_Sorted_GRA=FALSE;
BOOL IPFEC_Enable_FTZ=FALSE;
BOOL IPFEC_NoSched_Divmfhimflo = FALSE;

char * IPFEC_safe_cntl_spec_prob = NULL;
char * IPFEC_unsafe_cntl_spec_prob = NULL;

BOOL ORC_Enable_Region_Formation = TRUE;
BOOL ORC_Enable_Region_Decomposition = TRUE;
INT32 ORC_Enable_Tail_Duplication = FALSE;
INT32 ORC_Enable_Exit_Probability = FALSE;
INT32 ORC_Stacked_Cut_Num = 0;
INT32 ORC_Stacked_Spill_Num = 0;
BOOL ORC_Enable_If_Conversion = FALSE;
BOOL ORC_Force_If_Conv = FALSE;
BOOL ORC_Force_Para_Comp_Gen = FALSE;
BOOL ORC_Para_Comp_Gen = TRUE;
BOOL ORC_Combine_Exit = FALSE;
BOOL ORC_Disable_Merge_BB = FALSE;
BOOL ORC_Enable_PRDB= FALSE;
BOOL ORC_Enable_BB_Verify = FALSE;
BOOL ORC_Enable_Prepass_GLOS = TRUE;
BOOL ORC_Enable_Opt_after_schedule = TRUE;
BOOL ORC_Enable_Postpass_GLOS = FALSE;
BOOL ORC_Enable_Prepass_LOCS = FALSE;
BOOL ORC_Enable_Postpass_LOCS = TRUE;
BOOL ORC_Enable_Speculation = FALSE;
BOOL ORC_Enable_Data_Speculation = FALSE;
BOOL ORC_Enable_Cntl_Speculation = FALSE;
BOOL ORC_Enable_Fill_Delay_Slot = TRUE;
BOOL ORC_Enable_Fill_Delay_Slot_Mov = TRUE;
BOOL ORC_Enable_Fill_Delay_Slot_Dup = TRUE;
BOOL ORC_Enable_Compressed_Template = TRUE;

BOOL ORC_Glos_Reg_Pressure_Aware = TRUE;
BOOL ORC_Stress_Spec             = FALSE;
BOOL ORC_Glos_Motion_Across_Calls = FALSE ;
BOOL ORC_Glos_Split_Entry_BB      = TRUE ;
BOOL ORC_Glos_Split_Exit_BB       = TRUE ;
BOOL ORC_Glos_Enable_P_Ready_Code_Motion = TRUE;
BOOL ORC_Glos_Code_Motion_Across_Nested_Rgn = TRUE;
BOOL ORC_Glos_Enable_Cntl_Spec_If_Converted_Code = FALSE;
BOOL ORC_Glos_Enable_Renaming = TRUE;
BOOL ORC_Adjust_Variable_Latency  = TRUE ;

BOOL ORC_Enable_Multi_Branch = FALSE;
BOOL ORC_Enable_Pre_Multi_Branch = FALSE;
BOOL ORC_Enable_Post_Multi_Branch = FALSE;
BOOL ORC_Enable_Cache_Analysis = FALSE;
INT32 ORC_sched_care_machine = Sched_care_resource;

BOOL ORC_Enable_Edge_Profile = FALSE;
BOOL ORC_Enable_Value_Profile = FALSE;
BOOL ORC_Enable_Stride_Profile = FALSE;
BOOL ORC_Enable_Cache_Profile = FALSE;
BOOL ORC_Enable_Random_Prob = FALSE;
BOOL ORC_Enable_Edge_Profile_Annot = FALSE;

BOOL ORC_Enable_Value_Profile_Annot = FALSE;
BOOL ORC_Enable_Stride_Profile_Annot = FALSE;
BOOL ORC_Enable_Cache_Profile_Annot = FALSE;

INT32 ORC_Enable_Stride_Prefetch = 3;	// default turn on strong single and phase prefetch

BOOL ORC_Enable_Pre_Bundling = TRUE;
BOOL ORC_Force_CHK_Fail = FALSE;
BOOL ORC_Chk_Compact = TRUE;
BOOL ORC_Enable_Cascade = TRUE;
BOOL ORC_Hold_Uses = FALSE;
BOOL ORC_Enable_Safety_Load = TRUE;
BOOL ORC_Profitability = FALSE;		// to set all flags considering profitability

BOOL ORC_Enable_Insert_UNAT = FALSE;	// insert unat spill and restore code when need
BOOL CG_Enable_Opt_Mem_OP = FALSE;
BOOL CG_Enable_Opt_St_In_Loop = FALSE;
BOOL CG_Enable_Enhanced_LRA = FALSE;
BOOL CG_Enable_Force_Enhanced_LRA = FALSE;
BOOL CG_Enable_Opt_Ld_After_LRA = FALSE;
INT32 CG_Enable_RA_OPT = 0;
BOOL CG_Enable_Sorted_GRA=FALSE;
BOOL CG_Enable_FTZ=FALSE;
BOOL CG_NoSched_Divmfhimflo = FALSE;
BOOL ORC_Enable_Split_bb = FALSE;
BOOL ORC_Enable_LICM     = TRUE;

char * ORC_safe_cntl_spec_prob = NULL;
char * ORC_unsafe_cntl_spec_prob = NULL;

BOOL VT_Enable_BB_OP = FALSE;
BOOL VT_Enable_Global_CFG = FALSE;
BOOL VT_Enable_Regional_CFG = FALSE;
BOOL VT_Enable_Region_Tree = FALSE;
BOOL VT_Enable_BB_DAG = FALSE;
BOOL VT_Enable_Regional_DAG = FALSE;
BOOL VT_Enable_Partition_Graph = FALSE;
BOOL VT_Enable_CFG_Label = FALSE;
BOOL VT_Enable_DAG_BR = FALSE;

// Various skip lists.
OPTION_LIST *raw_locs_skip_bb;
SKIPLIST *locs_skip_bb;
OPTION_LIST *raw_dss_skip_bb;
SKIPLIST *dss_skip_bb;
OPTION_LIST *raw_mlbr_skip_bb;
SKIPLIST *mlbr_skip_bb;

OPTION_LIST *raw_if_conv_skip_rgn;
SKIPLIST *if_conv_skip_rgn;
OPTION_LIST *raw_if_conv_skip_area;
SKIPLIST *if_conv_skip_area;

OPTION_LIST *raw_glos_skip_rgn;
SKIPLIST *glos_skip_rgn;
OPTION_LIST *raw_glos_skip_bb;
SKIPLIST *glos_skip_bb;
OPTION_LIST *raw_glos_skip_op;
SKIPLIST *glos_skip_op;


OPTION_LIST *raw_spec_skip_bb;
SKIPLIST *spec_skip_bb;
OPTION_LIST *raw_spec_skip_rgn;
SKIPLIST *spec_skip_rgn;
OPTION_LIST *raw_spec_skip_op;
SKIPLIST *spec_skip_op;

OPTION_LIST *raw_msched_skip_bb;
SKIPLIST *msched_skip_bb;
OPTION_LIST *raw_msched_skip_rgn;
SKIPLIST *msched_skip_rgn;

OPTION_LIST *raw_spec_skip_PU;
SKIPLIST *spec_skip_PU;
OPTION_LIST *raw_if_conv_skip_PU;
SKIPLIST *if_conv_skip_PU;
OPTION_LIST *raw_pre_glos_skip_PU;
SKIPLIST *pre_glos_skip_PU;
OPTION_LIST *raw_post_locs_skip_PU;
SKIPLIST *post_locs_skip_PU;
OPTION_LIST *raw_PRDB_skip_PU;
SKIPLIST *PRDB_skip_PU;

OPTION_LIST *raw_glos_rename_skip_bb;
SKIPLIST *glos_rename_skip_bb;
OPTION_LIST *raw_glos_rename_skip_op;
SKIPLIST *glos_rename_skip_op;

// testing for ld latency 
OPTION_LIST *raw_latency2;
SKIPLIST *latency2;

/* ===============================================
 *
 * Copy_Ipfec_Flags
 *
 * Copy the Flags who have the prefix "ORC_" to those flags who 
 *  have the prefix "IPFEC_"
 *
 * ==============================================
 */
void
Copy_Ipfec_Flags (void) {

  IPFEC_Enable_Region_Formation = ORC_Enable_Region_Formation;
  IPFEC_Enable_Region_Decomposition = ORC_Enable_Region_Decomposition;
  IPFEC_Enable_Tail_Duplication = ORC_Enable_Tail_Duplication;
  IPFEC_Enable_Exit_Probability = ORC_Enable_Exit_Probability;
  IPFEC_Stacked_Cut_Num = ORC_Stacked_Cut_Num;
  IPFEC_Stacked_Spill_Num = ORC_Stacked_Spill_Num;
  IPFEC_Enable_If_Conversion = ORC_Enable_If_Conversion;
  IPFEC_Force_If_Conv = ORC_Force_If_Conv;
  IPFEC_Force_Para_Comp_Gen = ORC_Force_Para_Comp_Gen;
  IPFEC_Para_Comp_Gen = ORC_Para_Comp_Gen;
  IPFEC_Combine_Exit = ORC_Combine_Exit;
  IPFEC_Disable_Merge_BB = ORC_Disable_Merge_BB;
  IPFEC_Enable_PRDB = ORC_Enable_PRDB;
  IPFEC_Enable_BB_Verify = ORC_Enable_BB_Verify;
  IPFEC_Enable_Prepass_GLOS = ORC_Enable_Prepass_GLOS;
  IPFEC_Enable_Opt_after_schedule = ORC_Enable_Opt_after_schedule;
  IPFEC_Enable_Postpass_GLOS = ORC_Enable_Postpass_GLOS;
  IPFEC_Enable_Prepass_LOCS = ORC_Enable_Prepass_LOCS;
  IPFEC_Enable_Postpass_LOCS = ORC_Enable_Postpass_LOCS;
  IPFEC_Enable_Fill_Delay_Slot = ORC_Enable_Fill_Delay_Slot && ( IPFEC_Enable_Prepass_LOCS||IPFEC_Enable_Postpass_LOCS);
  IPFEC_Enable_Fill_Delay_Slot_Mov = ORC_Enable_Fill_Delay_Slot_Mov;
  IPFEC_Enable_Fill_Delay_Slot_Dup = ORC_Enable_Fill_Delay_Slot_Dup;
  IPFEC_Enable_Speculation = ORC_Enable_Speculation;
  IPFEC_Enable_Data_Speculation = ORC_Enable_Data_Speculation;
  IPFEC_Enable_Cntl_Speculation = ORC_Enable_Cntl_Speculation;
  IPFEC_Enable_Compressed_Template = ORC_Enable_Compressed_Template;
  IPFEC_safe_cntl_spec_prob = ORC_safe_cntl_spec_prob;
  IPFEC_unsafe_cntl_spec_prob = ORC_unsafe_cntl_spec_prob;

  IPFEC_Glos_Reg_Pressure_Aware = ORC_Glos_Reg_Pressure_Aware;
  IPFEC_Stress_Spec             = ORC_Stress_Spec;
  IPFEC_Glos_Motion_Across_Calls = ORC_Glos_Motion_Across_Calls;
  IPFEC_Glos_Split_Entry_BB      = ORC_Glos_Split_Entry_BB      ;
  IPFEC_Glos_Split_Exit_BB       = ORC_Glos_Split_Exit_BB       ;
  IPFEC_Glos_Enable_P_Ready_Code_Motion = ORC_Glos_Enable_P_Ready_Code_Motion ;
  IPFEC_Glos_Code_Motion_Across_Nested_Rgn = ORC_Glos_Code_Motion_Across_Nested_Rgn;
  IPFEC_Glos_Enable_Cntl_Spec_If_Converted_Code = ORC_Glos_Enable_Cntl_Spec_If_Converted_Code;
  IPFEC_Glos_Enable_Renaming = ORC_Glos_Enable_Renaming;
  IPFEC_Adjust_Variable_Latency  = ORC_Adjust_Variable_Latency  ;
  IPFEC_Enable_Multi_Branch =ORC_Enable_Multi_Branch ;
  IPFEC_Enable_Pre_Multi_Branch = ORC_Enable_Pre_Multi_Branch;
  IPFEC_Enable_Post_Multi_Branch = ORC_Enable_Post_Multi_Branch ;

  IPFEC_Enable_Edge_Profile = ORC_Enable_Edge_Profile;
  IPFEC_Enable_Value_Profile = ORC_Enable_Value_Profile;
  IPFEC_Enable_Stride_Profile = ORC_Enable_Stride_Profile;
  IPFEC_Enable_Cache_Profile = ORC_Enable_Cache_Profile;
  IPFEC_Enable_Random_Prob = ORC_Enable_Random_Prob;
  IPFEC_Enable_Edge_Profile_Annot = ORC_Enable_Edge_Profile_Annot;

  IPFEC_Enable_Value_Profile_Annot = ORC_Enable_Value_Profile_Annot;
  IPFEC_Enable_Stride_Profile_Annot = ORC_Enable_Stride_Profile_Annot;
  IPFEC_Enable_Cache_Profile_Annot = ORC_Enable_Cache_Profile_Annot;
  IPFEC_Enable_Pre_Bundling = ORC_Enable_Pre_Bundling;
  IPFEC_Force_CHK_Fail = ORC_Force_CHK_Fail;
  IPFEC_Chk_Compact = ORC_Chk_Compact;
  IPFEC_Enable_Cascade = ORC_Enable_Cascade;
  IPFEC_Hold_Uses = ORC_Hold_Uses;
  IPFEC_Enable_Safety_Load = ORC_Enable_Safety_Load;
  IPFEC_Profitability = ORC_Profitability;
  IPFEC_sched_care_machine = ORC_sched_care_machine;
  IPFEC_Enable_Insert_UNAT = ORC_Enable_Insert_UNAT;
  IPFEC_Enable_Stride_Prefetch =ORC_Enable_Stride_Prefetch;

  IPFEC_Enable_Split_bb = ORC_Enable_Split_bb;
  IPFEC_Enable_LICM     = ORC_Enable_LICM;
  IPFEC_Enable_Opt_Mem_OP = CG_Enable_Opt_Mem_OP;
  IPFEC_Enable_Opt_St_In_Loop = CG_Enable_Opt_St_In_Loop;
  IPFEC_Enable_Enhanced_LRA = CG_Enable_Enhanced_LRA;
  IPFEC_Enable_Force_Enhanced_LRA = CG_Enable_Force_Enhanced_LRA;
  IPFEC_Enable_Opt_Ld_After_LRA = CG_Enable_Opt_Ld_After_LRA;
  IPFEC_Enable_RA_OPT = CG_Enable_RA_OPT;
  IPFEC_Enable_Sorted_GRA=CG_Enable_Sorted_GRA;
  IPFEC_Enable_FTZ=CG_Enable_FTZ;
  IPFEC_NoSched_Divmfhimflo = CG_NoSched_Divmfhimflo;
}

/* ====================================================================
 *
 * Print_Skiplist
 *
 * Print a skiplist.  
 *
 * ====================================================================
 */
void
IPFEC_Print_Skiplist ( SKIPLIST *skip, char *lab, FILE *tf=stderr )
{
  INT32 i;

  if ( skip == NULL ) {
    fprintf ( tf, "SKIPLIST %s empty\n", lab );
    return;
  }
  fprintf ( tf, "SKIPLIST %s:\n", lab );

  for ( i = 0; skip->SKIPLIST_kind(i) != SK_NONE; i++ ) {
    switch ( skip->SKIPLIST_kind(i) ) {
      case SK_EQUAL:
        fprintf ( tf, "  equal %d\n", skip->SKIPLIST_val(i) );
        break;
      case SK_AFTER:
        fprintf ( tf, "  after %d\n", skip->SKIPLIST_val(i) );
        break;
      case SK_BEFORE:
        fprintf ( tf, "  before %d\n", skip->SKIPLIST_val(i) );
        break;
    }
  }
  fprintf ( tf, "SKIPLIST %s end\n\n", lab );
}

/* ====================================================================
 *
 * Build_Skiplist
 *
 * Build a skiplist from a group option list.  For now, we assume that
 * the only choices are ***skip_a= (after) ***skip_b (before), and ***skip_e*
 * (equal).  Use % to separate the id. See Query_Skiplist below for the
 * list semantics.
 *
 * You can build skip_bb_op_e=1%2%4^3%5%6 which means that skip the op
 * 2,4 in bb 1,and op 5,6 in bb 3. Notice, you must put the field in the first of
 * the sequence. Use ^ to separate the field.
 *
 * Note that we interpret skip_equal=1%3-5%7-10%12%35-39 as you might
 * hope. If skip_a=3, the 3 is not in the list, which is the same as skip_b=3 
 *
 *
 * WARNING:  This routine does no error checking.  This option is for
 * internal use, and if the syntax is wrong, strange (non-fatal) things
 * may happen (typically ignoring the rest of the option).
 *
 * ====================================================================
 */
SKIPLIST *
IPFEC_Build_Skiplist ( OPTION_LIST *olist )
{
  UINT count = 0;
  OPTION_LIST *ol;
  SKIPLIST *sl = CXX_NEW(SKIPLIST(), &MEM_pu_pool);
  BOOL list_found = FALSE;
  BOOL set_first = TRUE;
  BOOL global_skip = TRUE;
  char *p;

  /* Count the elements: */
  if ( olist == NULL ) return NULL;
  for ( ol = olist; ol != NULL; ol = OLIST_next(ol) ) {

    /* At least one entry: */
    ++count;
    
    /* Check for commas and ranges: */
    p = OLIST_val(ol);
    while ( *p != ':' && *p != 0 ) {

    /* Use % - ^ to separate the id */
      if ( *p == '%' ||*p == '-' || *p == '^' || *p=='#' ) {
        if( *p == '%') {
          Set_SL_local(sl);
          global_skip = FALSE;
        }
        ++count;
        list_found = TRUE;
      }
      ++p;
    }
  }

  /* Allocate the skiplist: */
  sl->SKIPLIST_size() = count+1;
  sl->SKIPLIST_kind_vec() = (mINT8 *) calloc ( sizeof(mINT8), count+1 );
  sl->SKIPLIST_val_vec() = (mINT32 *) calloc ( sizeof(mINT32), count+1 );
  Set_SL_global(sl);
  /* Fill the skiplist: */
  for ( count = 0, ol = olist;
        ol != NULL;
        ++count, ol = OLIST_next(ol) )
  {
    if ( strstr(OLIST_opt(ol)+strlen(OLIST_opt(ol))-6, "_after" ) ||
          strstr(OLIST_opt(ol)+strlen(OLIST_opt(ol))-2, "_a" ) ) {
      sl->Set_SKIPLIST_kind ( count, SK_AFTER );
    } 
    else if ( strstr(OLIST_opt(ol)+strlen(OLIST_opt(ol))-7, "_before" ) ||
             strstr(OLIST_opt(ol)+strlen(OLIST_opt(ol))-2,"_b" )) {
      sl->Set_SKIPLIST_kind (  count, SK_BEFORE );
    } else {
      sl->Set_SKIPLIST_kind ( count, SK_EQUAL );
    }

    if( set_first ) {
      sl->SKIPLIST_val(count) = atoi ( OLIST_val(ol) );
      set_first = FALSE;
    }
    else 
      sl->SKIPLIST_val(count) =  -1*atoi( OLIST_val(ol) ) - 1;

    if (global_skip) {
      set_first = TRUE;
    }
    /* If this is skip_equal, look for a list... */
    if ( list_found && sl->SKIPLIST_kind(count) == SK_EQUAL ) {
      p = OLIST_val(ol);
  
      /* Do a cyclic to get the next char until the next is not a num */
      while ( *p >= '0' && *p <= '9' ) ++p;
      
      if ( *p == '-' ) {
        sl->Set_SKIPLIST_kind ( count, SK_AFTER );
        sl->SKIPLIST_val(count) --;
        ++p;
        ++count;
        sl->Set_SKIPLIST_kind ( count, SK_BEFORE );
        sl->SKIPLIST_val(count) = 1 + atoi ( p );
        while ( *p >= '0' && *p <= '9' ) ++p;
      }
      if (*p == '%') {
        while ( *p++ == '%' ) {
          ++count;
          sl->Set_SKIPLIST_kind ( count, SK_EQUAL );
          sl->SKIPLIST_val(count) = atoi ( p );
          while ( *p >= '0' && *p <= '9' ) ++p;
          if ( *p == '-' ) {
            sl->Set_SKIPLIST_kind ( count, SK_AFTER );
            sl->SKIPLIST_val(count) --;
            ++p;
            ++count;
            sl->Set_SKIPLIST_kind ( count, SK_BEFORE );
            sl->SKIPLIST_val(count) = 1 + atoi ( p );
            while ( *p >= '0' && *p <= '9' ) ++p;
          }
          if ( *p == '^' ) {
            p++;
            ++count;
            sl->Set_SKIPLIST_kind ( count, SK_EQUAL);
            // Letting the p to be negtive and minus 1 present p is a container.
            // Doing like that can avoid the condition that  id = 0;
            sl->SKIPLIST_val(count) = -1*atoi( p ) - 1;
            while ( *p >= '0' && *p <= '9' ) ++p;
          }
        }
      }
      if(*p == '#') {
        while (*p++ == '#') {
          ++count;
          sl->Set_SKIPLIST_kind ( count, SK_EQUAL );
          sl->SKIPLIST_val(count) = atoi ( p );

          while ( *p >= '0' && *p <= '9' ) ++p;

          if ( *p == '-' ) {
            sl->Set_SKIPLIST_kind ( count, SK_AFTER );
            sl->SKIPLIST_val(count) --;
            ++p;
            ++count;
            sl->Set_SKIPLIST_kind ( count, SK_BEFORE );
            sl->SKIPLIST_val(count) = 1 + atoi ( p );
            while ( *p >= '0' && *p <= '9' ) ++p;
          }
        }
      }

    // Other option except for the equal
    } else if(list_found){
      SKIPKIND field_kind = (SKIPKIND)sl->SKIPLIST_kind(count);
      p = OLIST_val(ol);
      while ( *p >= '0' && *p <= '9' ) ++p;

      while ( *p++ == '%' ) {
        ++count;

        sl->Set_SKIPLIST_kind ( count, field_kind );
        sl->SKIPLIST_val(count) = atoi ( p );
        while ( *p >= '0' && *p <= '9' ) ++p;
        if ( *p == '^' ) {
          p++;
          ++count;
          sl->Set_SKIPLIST_kind ( count, field_kind);
          //Letting the p to be negtive and minus 1 present p is a container.
          // Doing like that can avoid the condition that  id = 0;
          sl->SKIPLIST_val(count) = -1*atoi( p ) - 1;
          while ( *p >= '0' && *p <= '9' ) ++p;
        }
      }
    }
  }
  
  /*This is the end of SKIPLIST, keep it. */
  sl->Set_SKIPLIST_kind ( count, SK_NONE );

  if ( Get_Trace ( TP_MISC, 0x80 ) ) {
    IPFEC_Print_Skiplist ( sl, "Build_Skiplist", TFile );
  }

  return sl;
}

/* ====================================================================
 *
 * Query_Skiplist
 *
 * Query a skiplist.  A TRUE result means that the element queried is in
 * the skiplist.  The semantics of the list is as follows:  Return TRUE
 * if elmt is equal to an SK_EQUAL element of the list.  Return TRUE if
 * elmt is greater than an SK_AFTER element AND it is less than an
 * immediately following SK_BEFORE element; otherwise skip over the
 * following SK_BEFORE element.  Return TRUE if elmt is smaller than an
 * SK_BEFORE which does not immediately follow an SK_AFTER.  If nothing
 * on the list produces a TRUE result, return FALSE.  That is, a list
 * consists of SK_EQUAL elements, SK_AFTER/SK_BEFORE pairs in that
 * order, or SK_AFTER and SK_BEFORE elements that aren't in such pairs.
 * Any match of one of these tests causes a skip.
 *
 * You can query whether a given num is in a given field. e.g a given op in a given bb 
 * ====================================================================
 */

BOOL
IPFEC_Query_Skiplist ( SKIPLIST *skip, INT32 elmt, INT32 field )
{
  INT32 i;
  BOOL ok;
  INT32 field_id;

  if ( skip == NULL ) return FALSE;

  if (!SL_local(skip)) 
    field = -1;

  // If field is wanted, check from the second one.
  if ( field >=0 ) 
    i = 1;
  else
    i = 0;
  for (  ; skip->SKIPLIST_kind(i) != SK_NONE; i++ ) {
    switch ( skip->SKIPLIST_kind(i) ) {

    case SK_EQUAL:
      if ( skip->SKIPLIST_val(i) == elmt ) {
        if ( field >= 0 ) {
          // field_id recored the index of the field id
          field_id = i;
          while(!(skip->SKIPLIST_val(field_id) < 0 || field_id == 0) )
            field_id--;

          if ( field_id == 0) {
            if( skip->SKIPLIST_val(field_id) == field )
              return TRUE;
          }
          else { 
            if ( -1*skip->SKIPLIST_val(field_id)-1 == field ) 
              return TRUE;
          }
            
        }
        else return TRUE;
      }

      break;

    case SK_AFTER:
      if(skip->SKIPLIST_val(i) < 0)
        break;

      ok = ( skip->SKIPLIST_val(i) < elmt );
      if ( skip->SKIPLIST_kind(i+1) == SK_BEFORE 
            && skip->SKIPLIST_val(i+1) > skip->SKIPLIST_val(i)) 
      {
        if ( skip->SKIPLIST_val(++i) <= elmt ) ok = FALSE;
      }

      if ( ok ) {
        if ( field >= 0 ) {
          field_id = i;
          while(!(skip->SKIPLIST_val(field_id) < 0 || field_id == 0) )
            field_id--;

          if ( field_id == 0) {
            if( skip->SKIPLIST_val(field_id) == field )
              return TRUE;
            }
            else { 
              if ( -1*skip->SKIPLIST_val(field_id)-1 == field ) 
                return TRUE;
            }
        }
        else {
          return TRUE;
        }
      }

      break;

    case SK_BEFORE:
      if(skip->SKIPLIST_val(i) < 0)
        break;
      ok = (skip->SKIPLIST_val(i) > elmt ) ;
      if ( ok ) {
        if ( field >= 0 ) {
          field_id = i;
          while(!(skip->SKIPLIST_val(field_id) < 0 || field_id == 0) )
            field_id--;

          if ( field_id == 0) {
            if( skip->SKIPLIST_val(field_id) == field )
              return TRUE;
          }
          else { 
            if ( -1*skip->SKIPLIST_val(field_id)-1 == field ) 
              return TRUE;
          }
        }
        else  return TRUE;
      }

      break;

    default:

      break;
    }
  }

  return FALSE;
}


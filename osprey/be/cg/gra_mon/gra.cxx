/*
 * Copyright (C) 2008-2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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

//  Global register allocation driver
/////////////////////////////////////
//
//  Description:
//
//      Top level of global register allocation.  Try to limit to functions
//      exported outside of GRA, per Ken Lesniak.
//
/////////////////////////////////////


//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:29-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra.cxx $

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra.cxx $ $Revision: 1.2 $";
#endif

#include "defs.h"
#include "mempool.h"
#include "timing.h"
#include "tracing.h"
#include "cgir.h"
#include "cg.h"
#include "gtn_universe.h"
#include "dominate.h"
#include "cg_flags.h"
#include "gra_bb.h"
#include "gra.h"
#include "gra_region.h"
#include "gra_create.h"
#include "gra_color.h"
#include "gra_spill.h"
#include "gra_grant.h"
#include "gra_cflow.h"
#include "gra_trace.h"
#ifdef TARG_SL2 //para_region_mgr
#include "gra_para_region.h"
#endif 


static MEM_POOL  GRA_pool_struct;
MEM_POOL* const GRA_pool = &GRA_pool_struct;
		 // Pointer to a non-zeroed memory pool for general use in GRA. 
		 // Pushed in GRA_Create and Popped in GRA_Delete.
INT GRA_pu_num = 0;
float GRA_call_split_freq;
float GRA_spill_count_factor;

#ifdef TARG_IA64
BOOL gra_self_recursive = FALSE;
extern char *Cur_PU_Name;
#endif

static BOOL gra_spill_around_save_tn_copies;


/////////////////////////////////////
void 
GRA_Initialize(void)
/////////////////////////////////////
//
//  see interface description
//
/////////////////////////////////////
{
  gra_spill_around_save_tn_copies = TRUE;
}

/////////////////////////////////////
BOOL
GRA_Spill_Around_Save_TN_Copies(void)
/////////////////////////////////////
//
//  see interface description
//
/////////////////////////////////////
{
  return gra_spill_around_save_tn_copies;
}

/////////////////////////////////////
static void
Initialize_Memory(void)
/////////////////////////////////////
//
//  Prepare the GRA_pool for memory allocation.
//
/////////////////////////////////////
{
  static BOOL did_init = FALSE;

  if ( ! did_init ) {
    MEM_POOL_Initialize(GRA_pool,"GRA pool",FALSE);
    did_init = TRUE;
  }
  MEM_POOL_Push(GRA_pool);
}

/////////////////////////////////////
static void
Finalize_Memory(void)
/////////////////////////////////////
//
//  Delete all the memory that we used for strictly private stuff.
//
/////////////////////////////////////
{
  if (GRA_loop_splitting) {
    Free_Dominators_Memory();
  }
  MEM_POOL_Pop(GRA_pool);
}


/////////////////////////////////////
REGISTER_SET
GRA_Local_Register_Grant( BB* bb, ISA_REGISTER_CLASS rc )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  return GRA_GRANT_Get_Local_Registers(bb,rc);
}

/////////////////////////////////////
void
GRA_Finalize_Grants(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  GRA_GRANT_Finalize();
}

/////////////////////////////////////
static void
Clear_Spill_BB_Flags(void)
/////////////////////////////////////
//  The BB_gra_spill flag has served its purpose. Clear it now
//  from any blocks that have it set to allow cflow to remove them
//  if they are still empty.
/////////////////////////////////////
{
  BB *bb;

  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    Reset_BB_gra_spill(bb);
  }
}

/////////////////////////////////////
static void
Initialize_Flags()
/////////////////////////////////////
//
//  Initialize flags used by GRA.
//
/////////////////////////////////////
{
  gra_spill_around_save_tn_copies = FALSE;
  GRA_call_split_freq = atof(GRA_call_split_freq_string);
  GRA_spill_count_factor = atof(GRA_spill_count_factor_string);
}

#ifdef TARG_IA64
void 
GRA_Fat_Point_Estimate(void) {
  //Initialize_Flagsa(a);
  Initialize_Memory();
  lrange_sub_mgr.Initialize();
  lrange_mgr.Initialize();
  gbb_mgr.Initialize();
  gra_region_mgr.Initialize();

  GRA_Pre_Create();

  lrange_sub_mgr.Finalize();
  lrange_mgr.Finalize();
  gbb_mgr.Finalize();
  gra_region_mgr.Finalize();
   
  Finalize_Memory();
  //MEM_POOL_Push(&MEM_local_nz_pool);
  //MEM_POOL_Pop(&MEM_local_nz_pool);
}

//===================================
//
// Test whether this function is a 
// self_recursive function.
//
//===================================
BOOL 
Check_Self_Recursive(void) {
  BB *bb = NULL;
  OP *op = NULL;
  INT32 max_live_in = 0;
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    GTN_SET *live_in = BB_live_out(bb);
    INT32 size = BS_Size(live_in);
    if (size > max_live_in) max_live_in = size;
  }
  
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    FOR_ALL_BB_OPs (bb, op) {     
      if (OP_call(op)) {
         for (INT k = 0; k < OP_opnds(op);k++) {
            TN *tn = OP_opnd(op,k);
            if (TN_is_symbol(tn)) {
               ST *var = TN_var(tn);
               if (ST_class(var) !=  CLASS_CONST) {
                  char *called_func = ST_name(var);
                  if (strcmp(called_func,Cur_PU_Name) == 0) {
                      if ((!gra_self_recursive) && (max_live_in > 100)) {
                        gra_self_recursive = TRUE;
                        DevWarn("This FUNCTION IS FAT AND SELF_RECURSIVE ONE!The Max_Live_In is %d\n",max_live_in);
                        return gra_self_recursive;
                      }
                  }  
               }
            }
         }  
      }
   }
  }
  
  return FALSE;
}
INIT_USE_ONLY_GTN* GTN_USE_ONLY;

void Init_GTN_LIST()
{
     GTN_USE_ONLY = NULL;
}
INIT_USE_ONLY_GTN* Create_Use_Only_GTN(MEM_POOL *pool)
{
   INIT_USE_ONLY_GTN *gtn_use_only =TYPE_MEM_POOL_ALLOC(INIT_USE_ONLY_GTN,pool);
   gtn_use_only ->TN_Number=0;
   gtn_use_only ->Used_Time=0;
   gtn_use_only->Used_In_BB=0;
   gtn_use_only ->next = NULL;
   return gtn_use_only;
}

void Free_GTN (MEM_POOL *pool,INIT_USE_ONLY_GTN *delete_tn)
{
     MEM_POOL_FREE(pool,delete_tn);
}
BOOL Search_Used_Only_Once_GTN (TN *find_tn,BB* def_bb) {
     
     INIT_USE_ONLY_GTN *head = GTN_USE_ONLY;
     if (head==NULL)
        return FALSE;
     else
        while (head !=NULL){
           if ((head->TN_Number== TN_number(find_tn))&&(head->Used_Time == 1)) {
              if (head->Used_In_BB == BB_id(def_bb)) 
              return TRUE;
           }
           head = head->next;
        }
        return FALSE;
}
INIT_USE_ONLY_GTN* Search_GTN_In_List (TN *find_tn) 
{
     INIT_USE_ONLY_GTN *head = GTN_USE_ONLY;
     if (head==NULL) 
        return NULL;
     else 
        while (head !=NULL){
           if (head->TN_Number== TN_number(find_tn)) {
              return head;
           }
           head = head->next;
        }
        return NULL;
}
void Build_GTN_In_List (TN *tn,BB* bb)
{ 
     INIT_USE_ONLY_GTN *head = GTN_USE_ONLY;
     if (!TN_is_global_reg(tn)) return;
     if (head == NULL) {
        INIT_USE_ONLY_GTN *head = Create_Use_Only_GTN(GRA_pool);
        head->TN_Number = TN_number(tn);
        head->Used_In_BB=BB_id(bb);
        head->Used_Time ++;
        GTN_USE_ONLY=head;
        return ;          
     }else {
        INIT_USE_ONLY_GTN* find_tn=Search_GTN_In_List(tn);
        if(find_tn ==NULL) {
              INIT_USE_ONLY_GTN *insert_tn = Create_Use_Only_GTN(GRA_pool);
              insert_tn->TN_Number = TN_number(tn);
              insert_tn->Used_Time ++;
              insert_tn->Used_In_BB=BB_id(bb);
              find_tn = GTN_USE_ONLY;
              while (find_tn->next !=NULL) {
                    find_tn= find_tn->next;
              }
              find_tn->next = insert_tn;
           }else {
                 Is_True (find_tn->TN_Number==TN_number(tn), ("Tn has been changed !"));
                 find_tn->Used_Time ++;
                 if (find_tn->Used_Time==1) {
                    find_tn->Used_In_BB=BB_id(bb);
                 }else {
                    find_tn->Used_In_BB=0;
                 }
           }
        return;
     }
}
void Print_GTN()
{
   INIT_USE_ONLY_GTN *head = GTN_USE_ONLY;
   fprintf(stdout,"**************Used only once GTN  ********\n" );
   while (head!=NULL){
      if (head->Used_Time == 1)
          fprintf(stdout,"GTN :%d \n",head->TN_Number );
      head= head->next;
   }
   fprintf(stdout,"**************End            GTN  ********\n" );
}

#endif  // TARG_IA64

/////////////////////////////////////
void
GRA_Allocate_Global_Registers( BOOL is_region )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{

  Set_Error_Phase ("Global Register Allocation");
  Start_Timer (T_GRA_CU);

  Initialize_Flags();
  Initialize_Memory();
  lrange_sub_mgr.Initialize();
  lrange_mgr.Initialize();
  gbb_mgr.Initialize();
  gra_region_mgr.Initialize();
#ifdef TARG_SL2 //para_region_mgr
  gra_para_region_mgr.Initialize();
#endif 
  GRA_Spill_Initialize();
  GRA_Trace_Initialize();
#ifdef TARG_IA64
  Init_GTN_LIST();  
#endif

  GRA_Split_Entry_And_Exit_BBs(is_region);

  GRA_Create();
  GRA_Color();

  // Dump out OPs after GRA
  if (Get_Trace(TKIND_IR, TP_GRA, REGION_First_BB))
    Trace_IR(TP_GRA, "GRA0", NULL);

  GRA_Spill();      // Actually add the spills.
  GRA_Delete();

  GRA_GRANT_Unused_Caller_Saved();


#if defined (TARG_SL) //minor_reg_alloc
  gra_para_region_mgr.Set_Region_LRA_Budget();
#endif 

  lrange_sub_mgr.Finalize();
  lrange_mgr.Finalize();
  gbb_mgr.Finalize();
  gra_region_mgr.Finalize();

  GRA_Join_Entry_And_Exit_BBs();

#ifdef TARG_IA64
  Gen_UNAT_Spills_Entry_And_Exit_BB();
  GRA_Optimize_Restore();
#endif

  Clear_Spill_BB_Flags();

  Finalize_Memory();

  // Dump out OPs after GRA
  if (Get_Trace(TKIND_IR, TP_GRA, REGION_First_BB))
    Trace_IR(TP_GRA, "GRA", NULL);

  GRA_pu_num++;

  Stop_Timer ( T_GRA_CU );
}

/////////////////////////////////////
void
GRU_Fuse_Global_Spills( BOOL is_region )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
#ifdef TARG_X8664
  Set_Error_Phase ("Fuse Global Spills");
  Start_Timer (T_GRU_CU);

  // Dump out OPs before GRA
  if (Get_Trace(TKIND_IR, TP_GRA, REGION_First_BB))
    Trace_IR(TP_GRU, "GRU0", NULL);

  GRU_Fuse();      // Actually try to remove some spills.

  // Dump out OPs after GRA
  if (Get_Trace(TKIND_IR, TP_GRU, REGION_First_BB))
    Trace_IR(TP_GRU, "GRU", NULL);

  Stop_Timer ( T_GRU_CU );
#endif
}

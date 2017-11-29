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

//
//  GRA_PARA_REGION -- Region encapsulation
/////////////////////////////////////
//
//  Description:
//
//      Implementation details for GRA_PARA_REGIONs.
//
/////////////////////////////////////


//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:30-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_para_region.cxx $

#include "defs.h"
#include "cg.h"
#include "register.h"
#include "gra_bb.h"
#include "gra_lrange.h"
#include "gra_para_region.h"
#include "bb.h"
#include "tn.h"
#include "gra_grant.h"


GRA_PARA_REGION_MGR gra_para_region_mgr;

/////////////////////////////////////
// Add <bb> to the list of BBs associated with <region>.
void 
GRA_PARA_REGION::Add_BB( BB* bb )
{
   BBlist_Add_BB(&internal_bbs,  bb);
   return;
}

/////////////////////////////////////
// initialize the fields in GRA_PARA_REGION_MGR; called at start of GRA
void
GRA_PARA_REGION_MGR::Initialize(void)
{
  MEM_POOL_Push(&MEM_local_pool);

  _map = TYPE_MEM_POOL_ALLOC_N(GRA_PARA_REGION*,  &MEM_local_pool, Last_Region_Id() + 1);

  bzero(_map, sizeof(GRA_PARA_REGION*) * (Last_Region_Id() + 1));

  _rid_pair_map = TYPE_MEM_POOL_ALLOC_N(RID*, &MEM_local_pool, Last_Region_Id() + 1);

  bzero(_rid_pair_map, sizeof(RID*) * (Last_Region_Id() + 1));

  _alloc_count = 0;

  minor_rids = VECTOR_Init( Last_Region_Id()+1, &MEM_local_pool);  

  return; 
}

/////////////////////////////////////
//  Region creation common to complement and previously allocate regions.
GRA_PARA_REGION*
GRA_PARA_REGION_MGR::Create( RID* rid)
{
  ISA_REGISTER_CLASS rc;

  GRA_PARA_REGION* region = TYPE_MEM_POOL_ALLOC(GRA_PARA_REGION, &MEM_local_pool);

  _alloc_count++;

  region->rid = rid;

  FOR_ALL_ISA_REGISTER_CLASS( rc ) {

    region->registers_use[rc] = REGISTER_SET_EMPTY_SET;

    region->registers_def[rc] = REGISTER_SET_EMPTY_SET;	
  }

  region->flags = (PARA_REGION_FLAG) 0;

  return region;
}

/////////////////////////////////////
GRA_PARA_REGION*
GRA_PARA_REGION_MGR::Create_Region(  RID* rid )
{
  GRA_PARA_REGION* region = Create(rid);

  region->next = _first_para_region;

  _first_para_region = region;

  Is_True((RID_TYPE_minor(rid) || RID_TYPE_major(rid)) , 
 	        ("unsupport region type when creating parallel region")); 

  if(RID_TYPE_minor(rid)) {
    region->Set_Region_Minor();
  }
  else if (RID_TYPE_major(rid)) {
    region->Set_Region_Major();
  }	

  return region;
}

/////////////////////////////////////
// Get GRA's encapsulation for the region identified by <rid>.  Creates it 
// if not already.  The region is allocated in the GRA_pool.  <rid> is the 
// RID associated with the blocks in the region.  For previously allocated
// regions there will be exactly one of this for all the blocks.  For the 
// complement, there may be more than one.  I don't know that we care.
GRA_PARA_REGION*
GRA_PARA_REGION_MGR::Get( RID* rid )
{
  if ( rid != NULL) {
    Is_True(RID_id(rid) <= Last_Region_Id(), ("Region out of bounds"));
    //
    // only want outermost region contained within this unit (which itself
    // may be a region).  all inner regions are to be treated as a black
    // box.  this is primarily to allow splitting to stop at region boundries,
    // but we don't recolor regions anyway, so who cares.  if we ever want
    // to recolor them, though, we'll have to undo this and treat regions as
    // we do loops (i.e. set the regs_used bit vectors by walking the trees
    // and such ... blech).
    //
/* only add gra_para_region for para region type */     
    if(!RID_TYPE_sl2_para(rid))		
//    if(!(RID_TYPE_minor(rid) || RID_TYPE_major(rid)))		
	return NULL; 
	
    if ( _map[RID_id(rid)] != NULL )
      return _map[RID_id(rid)];
    else
      return _map[RID_id(rid)] = Create_Region(rid);
  }
  return NULL;
}

/* build a map to get pair rid use mapping
  */ 
void 
GRA_PARA_REGION_MGR::Build_Map_For_Pair_Region(void) 
{
  BB* bb;
  for(bb = REGION_First_BB; bb != NULL; bb=BB_next(bb))
  {
    if(BB_rid(bb) && RID_parent(BB_rid(bb)) && RID_TYPE_minor(BB_rid(bb)))
    {
      RID* parent = RID_parent(BB_rid(bb));
/* for now we have two parallel thread, so the first kid
 * for the first parallel body and the second kid for the 
 * second parallel body. Need to rethink how to handle cases
 * that threads number greater than 2
 */ 
      RID* kid0 = RID_first_kid(parent);
      RID* kid1 = RID_next(kid0);
      _rid_pair_map[RID_id(kid0)] = kid1;
      _rid_pair_map[RID_id(kid1)] = kid0;
    }
  }
  return; 
}

/* this function is used to collect register set which contains registers not used in all basic block of
  * each region. Why we need this set is to reallocate register budget for each basic block according 
  * to requirement. 
  */ 
void 
GRA_PARA_REGION_MGR::Collect_Share_Registers_For_Pair_Regions(GRA_PARA_REGION* r1, GRA_PARA_REGION* r2)
{
  BBLIST* list1, *list2, *item;
  BB* bb;

  list1 = r1->Internal_BBs();

  list2 = r2->Internal_BBs();

  REGISTER_SET unused_set1 = REGISTER_CLASS_allocatable(ISA_REGISTER_CLASS_integer);

  REGISTER_SET unused_set2 = REGISTER_CLASS_allocatable(ISA_REGISTER_CLASS_integer);	

  FOR_ALL_BBLIST_ITEMS(list1, item)
  {
    bb =  BBLIST_item(item);
    unused_set1 = REGISTER_SET_Intersection(unused_set1,  GRA_Local_Register_Grant(bb,  ISA_REGISTER_CLASS_integer));
  }

  FOR_ALL_BBLIST_ITEMS(list2, item)
  {
    bb =  BBLIST_item(item);

    unused_set2 = REGISTER_SET_Intersection(unused_set2,  GRA_Local_Register_Grant(bb,  ISA_REGISTER_CLASS_integer));
  }

  REGISTER_SET unused_share_set = REGISTER_SET_Intersection(unused_set1, unused_set2);

  Set_Unused_Share(unused_share_set);

  vector < REGISTER > share_register_index;
  for(INT i=0; i < REGISTER_MAX+1; i++)
  {
    if(unused_share_set & (1<< i)) 
       share_register_index.push_back(i);
  }

  INT size = share_register_index.size();

  if(size)
  { 
    #define thread_num 2     

    INT count = size / thread_num ;

    for(INT i = 0; i < count; i++) {

      REGISTER reg = share_register_index.back();

	share_register_index.pop_back();

       r1->Add_Reg_To_LRA_Budget(reg);
    }

    for(INT i = 0; i < size-count; i++) {

	REGISTER reg = share_register_index.back();

	share_register_index.pop_back();

	r2->Add_Reg_To_LRA_Budget(reg);
    }
  }
  return; 
}

/* regrant register for all bb in minor region according to budget calculated 
  */ 
void 
GRA_PARA_REGION_MGR::Grant_Register_For_Region(GRA_PARA_REGION* region) 
{
  BB* bb;
  BBLIST * item; 

  BBLIST * bblist = region->Internal_BBs();

  FOR_ALL_BBLIST_ITEMS(bblist, item)
  {
    bb =  BBLIST_item(item);

    REGISTER_SET reg_set = GRA_Local_Register_Grant(bb,  ISA_REGISTER_CLASS_integer);

    reg_set = REGISTER_SET_Difference(reg_set, share_unused);

    reg_set = REGISTER_SET_Union(reg_set,  region->Get_LRA_Reg_Budget());

    GRA_GRANT_REGISTER_SET_Set_For_BB(bb, ISA_REGISTER_CLASS_integer,  reg_set);
  }
  return; 
}

/* this function is used to set register budget for basic from different minor thread 
  * need more information to direct the register allocation for minor thread. Such as
  * following information could be useful.
  *     a) spill number inserted
  *     b) if there is a loop in the region 
  *     c) if the workload is balance 
  *
  * Algorithm:
  *      first get all register GRA grant to LRA and scan all bb for minor thread to see
  * the maxiam register requirement for each bb from different thread. 
  */ 
void 
GRA_PARA_REGION_MGR::Set_Region_LRA_Budget()
{

  vector < RID* > rid_processed;

  for(int i = 0; i < VECTOR_count(minor_rids); i++) 
  {

     RID* rid = (RID*)VECTOR_element(minor_rids, i);

     Is_True((rid), ("RID is null for minor thread in register allocation check"));

     RID* pair_rid = _rid_pair_map[RID_id(rid)];

     Is_True((pair_rid), ("PAIR RID is null for minor thread in register allocation check"));		

    if(find(rid_processed.begin(), rid_processed.end(), rid) != rid_processed.end())  // has been processed; 
      continue; 

    GRA_PARA_REGION * region = _map[RID_id(rid)];

    Is_True((region),  ("has no region for this rid"));

    GRA_PARA_REGION * pair_region = _map[RID_id(pair_rid)];

    Collect_Share_Registers_For_Pair_Regions(region,  pair_region);

    Grant_Register_For_Region(region);

    Grant_Register_For_Region(pair_region);

    rid_processed.push_back(rid);

    rid_processed.push_back(pair_rid);
  }
  return; 	
}

/* collect all rid whose type is minor, don't check register allocation for major rid for speed 
  */ 
void 
GRA_PARA_REGION_MGR::Add_Rid_Into_Minor_Vector(RID * rid)
{
  if(!VECTOR_Member_Element(minor_rids,  rid)) 
    VECTOR_Add_Element(minor_rids,  rid);
  return;  	  
}


GRA_PARA_REGION * 
GRA_PARA_REGION_MGR::Get_Pair_Region(GRA_PARA_REGION* region) 
{

  RID* rid = region->Rid();

  RID* pair_rid = Get_Pair_Rid(rid);

  return Get(pair_rid);
}

void 
GRA_PARA_REGION_MGR::Pre_Reserve_Registers_For_Minor() 
{

  vector < RID* > rid_processed;

  for(int i = 0; i < VECTOR_count(minor_rids); i++) 
  {

    RID* rid = (RID*)VECTOR_element(minor_rids, i);

    Is_True((rid), ("RID is null for minor thread in register allocation check"));

    RID* pair_rid = _rid_pair_map[RID_id(rid)];

    Is_True((rid), ("PAIR RID is null for minor thread in register allocation check"));		
	 
    if(find(rid_processed.begin(), rid_processed.end(), rid) != rid_processed.end())  // haven't been processed; 
        continue; 

    GRA_PARA_REGION * region = _map[RID_id(rid)];

    Is_True((region),  ("has no region for this rid"));

    GRA_PARA_REGION * pair_region = _map[RID_id(pair_rid)];

    Is_True((pair_region), ("NULL pair region for current rid")); 

    ISA_REGISTER_CLASS rc = ISA_REGISTER_CLASS_integer; 

    #define ODD_RESERVE_REGS  0xaaaaaaaa
    #define EVEN_RESERVE_REGS 0x55555554

    region->Set_Registers_Exclude(rc,  (REGISTER_SET)EVEN_RESERVE_REGS);

    pair_region->Set_Registers_Exclude(rc, (REGISTER_SET)ODD_RESERVE_REGS);

    rid_processed.push_back(rid);

    rid_processed.push_back(pair_rid);
  }
  return; 	
}

/* scan all op for each bb in current region and collect register use and def information 
  * for register allocation conflict checking later 
  */ 
void
GRA_PARA_REGION::Collect_Reg_Used_And_Def_For_BBs(void) 
{
   BB* bb;
   OP* op;
   BBLIST * item; 
   
   BBLIST * bblist = Internal_BBs();

   GRA_PARA_REGION * pair_region = gra_para_region_mgr.Get_Pair_Region(this);
   FOR_ALL_BBLIST_ITEMS(bblist, item)
   {
     bb =  BBLIST_item(item);

     FOR_ALL_BB_OPs(bb, op)
     {

	for(int i =0; i < OP_opnds(op); i++) 
       {

         TN* tn = OP_opnd(op, i);

         if(TN_is_register(tn) && !TN_is_zero_reg(tn)) {

	    Make_Register_Used(TN_register_class(tn),  TN_register(tn));

	    if(TN_register_class(tn) == ISA_REGISTER_CLASS_integer) 
	    {

	      REGISTER_SET def_set = pair_region ->Registers_Defined(TN_register_class(tn));

	      if(REGISTER_SET_Intersection1(def_set, TN_register(tn)))
               Fail_FmtAssertion(("TN %d has same register conflict in BB %d\n"), TN_number(tn), BB_id(bb));
	    }
         }
       }

	for(int i =0; i < OP_results(op); i++) 
       {
         TN* tn = OP_result(op, i);

         if(TN_is_register(tn) ) {

	    Make_Register_Defined(TN_register_class(tn),  TN_register(tn));

	    if(TN_register_class(tn) == ISA_REGISTER_CLASS_integer) {

	      REGISTER_SET use_set = pair_region ->Registers_Used(TN_register_class(tn));

	      REGISTER_SET def_set = pair_region ->Registers_Defined(TN_register_class(tn));

	      if(REGISTER_SET_Intersection1(def_set, TN_register(tn)) || 
               REGISTER_SET_Intersection1(use_set, TN_register(tn)))
               Fail_FmtAssertion(("TN %d has same register conflict in BB %d\n"), TN_number(tn), BB_id(bb));
						  
           }					  
         }
	}
     }
  }
  return; 	  
}
void 
GRA_PARA_REGION_MGR::Check_Register_Allocation(void)
{

  if(!minor_rids ) return; 
  
  for(int i = 0; i < VECTOR_count(minor_rids); i++) 
  {
    RID* rid = (RID*)VECTOR_element(minor_rids, i);

    Is_True((rid), ("RID is null for minor thread in register allocation check"));

    RID* pair_rid = _rid_pair_map[RID_id(rid)];

    Is_True((pair_rid), ("PAIR_RID is null for minor thread in register allocation check"));	

    GRA_PARA_REGION * region = _map[RID_id(rid)];

    Is_True((region),  ("has no region for this rid"));

    GRA_PARA_REGION * pair_region = _map[RID_id(pair_rid)];

    region->Collect_Reg_Used_And_Def_For_BBs();

    pair_region->Collect_Reg_Used_And_Def_For_BBs();

    ISA_REGISTER_CLASS rc;

    FOR_ALL_ISA_REGISTER_CLASS(rc)
    {
      if(rc == ISA_REGISTER_CLASS_integer) { // only check gpr for now

	 REGISTER_SET use_set1 = region->Registers_Used(rc);

	 REGISTER_SET use_set2 = pair_region->Registers_Used(rc);

	 REGISTER_SET def_set1 = region->Registers_Defined(rc);

	 REGISTER_SET def_set2 = pair_region->Registers_Defined(rc);	 

	 if(REGISTER_SET_Intersection(use_set1,  def_set2) || 
	   REGISTER_SET_Intersection(use_set2,  def_set1)  ||
	   REGISTER_SET_Intersection(def_set1,  def_set2) ) 
	   Fail_FmtAssertion("register allocation conflict for minor thread" );
      }		 
    }		 
       /* remove element from minor_rids vector after checking them */ 
    VECTOR_Delete_Element(minor_rids, rid);
    VECTOR_Delete_Element(minor_rids, pair_rid);
	
  }
  return; 	
}


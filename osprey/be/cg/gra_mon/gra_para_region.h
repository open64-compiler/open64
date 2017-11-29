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

//  GRA parallel region encapsulation

//  $revision: $
//  $Date: 02/11/07 23:41:30-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_region.h $

#ifndef GRA_PARA_REGION_INCLUDED
#define GRA_PARA_REGION_INCLUDED

#include "defs.h"
#include "register.h"
#include "mempool.h"
#include "region_util.h"
#include "bb.h"

enum PARA_REGION_FLAG {
  PARA_REGION_FLAGS_is_minor = 0x1,   // Region belongs to minor section
  PARA_REGION_FLAGS_is_major = 0x2,   // Region belongs to minor section
 };

class GRA_PARA_REGION {
friend class GRA_PARA_REGION_MGR;
  RID* rid;
  RID* pair_rid; 
  /* all register defined in this regions */ 
  REGISTER_SET registers_def[ISA_REGISTER_CLASS_MAX+1];
  /* all register used in this regions */ 
  REGISTER_SET registers_use[ISA_REGISTER_CLASS_MAX+1];
 /* this register set is needed to exclude from allocation candidate for this region
   * the array is designed for minor thread which need share one register file
   * register allocation need check the array to find if some register has been allocated
   * in other region
   */ 
  REGISTER_SET exclude_set[ISA_REGISTER_CLASS_MAX + 1];

  REGISTER_SET lra_reg_budget; 
    //  the set of registers of each REGISTER_CLASS 
    //  currently allocated to any LR in this region
  BBLIST* internal_bbs;
    //  Head of internally linked list of BBs associated with the region.
  BBLIST* boundary_bbs;
    //  Final BB on internally linked list.
  GRA_PARA_REGION* next;
    //  Head of internally linked list of previously allocated regions.
  PARA_REGION_FLAG flags;
public:
  GRA_PARA_REGION(void) {}
  ~GRA_PARA_REGION(void) {}

  // access member functions
  RID *Rid(void)			{ return rid; }
  
  REGISTER_SET Registers_Used(ISA_REGISTER_CLASS rc){return registers_use[rc];}

  void Make_Register_Used(ISA_REGISTER_CLASS rc, REGISTER reg) {
	      registers_use[rc] = REGISTER_SET_Union1(registers_use[rc], reg);   }
		  
  REGISTER_SET Registers_Defined(ISA_REGISTER_CLASS rc){return registers_def[rc];}

  void Make_Register_Defined(ISA_REGISTER_CLASS rc, REGISTER reg) {
	      registers_def[rc] = REGISTER_SET_Union1(registers_def[rc], reg);
   }

  REGISTER_SET Registers_Exclude(ISA_REGISTER_CLASS rc){return exclude_set[rc];}

  void Add_One_Exclude_Register(ISA_REGISTER_CLASS rc, REGISTER reg) {
	      exclude_set[rc] = REGISTER_SET_Union1(exclude_set[rc], reg);
   }

  void Set_Registers_Exclude(ISA_REGISTER_CLASS rc, REGISTER_SET set)  {  exclude_set[rc] = set; }
  

  void Add_Reg_To_LRA_Budget(REGISTER reg) { lra_reg_budget = REGISTER_SET_Union1(lra_reg_budget, reg); }

  REGISTER_SET Get_LRA_Reg_Budget() {return lra_reg_budget;}
  
  GRA_PARA_REGION *Next(void)		{ return next; }

  void Add_BB( BB* bb );

  BOOL Is_Minor(void) 			{ return flags & PARA_REGION_FLAGS_is_minor;}

  void Set_Region_Minor(void)   { flags = (PARA_REGION_FLAG) (flags |PARA_REGION_FLAGS_is_minor); } 

  BOOL Is_Major(void) 			{ return flags & PARA_REGION_FLAGS_is_major;}

  void Set_Region_Major(void)   { flags = (PARA_REGION_FLAG) (flags | PARA_REGION_FLAGS_is_major); } 

  BBLIST* Internal_BBs(void)  { return internal_bbs;}

  void  Collect_Reg_Used_And_Def_For_BBs();

};


// manages the GRA_REGION package
class GRA_PARA_REGION_MGR {
  INT32 _alloc_count;

  GRA_PARA_REGION* _first_para_region;
  
  GRA_PARA_REGION **_map;		// RIDs  -> GRA_PARA_REGIONs

  RID** _rid_pair_map; // RID -> pair RID 

  REGISTER_SET share_unused; // these registers is unused in all bbs of minor thread region.  

  VECTOR minor_rids; // this vector contains all rids for minor thread; 
  	
public:
  GRA_PARA_REGION_MGR(void) {}
  ~GRA_PARA_REGION_MGR(void) {}

  void Initialize(void);

  void Finalize(void) { MEM_POOL_Pop(&MEM_local_pool);}

  GRA_PARA_REGION *Create(RID *rid);

  GRA_PARA_REGION *Create_Region(RID *rid);

  GRA_PARA_REGION* Get( RID* rid );

  REGISTER_SET Get_Unused_Share() {return share_unused; }

  void Set_Unused_Share(REGISTER_SET set) { share_unused = set; }

  INT32 Alloc_Count(void)		{ return _alloc_count; }

  GRA_PARA_REGION* First_Para_Region(void){ return _first_para_region; }

  RID* Get_Pair_Rid(RID* rid) { return _rid_pair_map[RID_id(rid)]; }

  GRA_PARA_REGION * Get_Pair_Region(GRA_PARA_REGION* region) ;
  
  /* build pair relationship for regions which run in parallel mode */ 
  void Build_Map_For_Pair_Region();

  void Add_Rid_Into_Minor_Vector(RID* rid);

  void Check_Register_Allocation(void) ;

  void Set_Region_LRA_Budget(); 

  void Grant_Register_For_Region(GRA_PARA_REGION * region);

  void Pre_Reserve_Registers_For_Minor(void);
  
  void   Collect_Share_Registers_For_Pair_Regions(GRA_PARA_REGION * r1, GRA_PARA_REGION * r2);

};

extern GRA_PARA_REGION_MGR gra_para_region_mgr;

#endif


/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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


//-*-c++-*-
// ====================================================================
// ====================================================================
//
//  Module: cg_grouping.h
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/ia64/cg_grouping.h,v $
//
//  Synopsis:
//
//      A package for inquiring about grouping of ops, where the aim is
//      to group a given set of ops such that we do not get an issue-split.
//      This interface combines information from the targ_isa_bundle.h
//      interface and the ti_si.h interface. I.e. it combines bundling
//      and resource information into grouping information.
//
//  Interface Description:
//
//	Exported types:
//
//      Exported functions:
//
//      Usage:
//
//         This interface should only be used within the IA-64 target
//         directory, and all defs are therefore in the header file
//         such that we do not have to introduce cg_grouping to other
//         target directories.
//
// ====================================================================
// ====================================================================

#ifndef cg_grouping_INCLUDED
#define cg_grouping_INCLUDED

#include <vector>
#include "mempool_allocator.h"
#include "ti_si.h"
#include "topcode.h"
#include "targ_issue_port.h"

typedef mempool_allocator<INT32>           INT32_MEMALLOC;
typedef std::vector<INT32, INT32_MEMALLOC> INT32_VECTOR;

class CG_GROUPING
{
private:

  // Resources, as defined in targ_info/merced.c (Change this for other
  // IA-64 architectures.
  //
  SI_RESOURCE_ID         _mi_unit_id; // integer_or_memory (ALU insts)
  SI_RESOURCE_ID         _m_unit_id;  // memory
  SI_RESOURCE_ID         _m0_unit_id; // memory0
  SI_RESOURCE_ID         _m2_unit_id; // memory2
  SI_RESOURCE_ID         _m_ld_unit_id; // memory for ld M0 M1 in McKiney
  SI_RESOURCE_ID         _m_st_unit_id; // memory for st M2 M3 in McKiney
  SI_RESOURCE_ID         _i_unit_id;  // integer
  SI_RESOURCE_ID         _i0_unit_id; // integer0
  SI_RESOURCE_ID         _f_unit_id;  // floating-point
  SI_RESOURCE_ID         _f0_unit_id; // floating-point0
  SI_RESOURCE_ID         _b_unit_id;  // branch
  INT32_VECTOR           _reserved;   // Reserved resources
  ISA_EXEC_UNIT_PROPERTY _bundling_preference[ISA_MAX_SLOTS][ISA_EXEC_MAX];
  ISA_EXEC_UNIT_PROPERTY _no_of_preferences[ISA_MAX_SLOTS];
  INT                    _total_no_of_bundles;
  INT                    _no_of_bundles_in_group;

  void _init_resource(const char *name, SI_RESOURCE_ID res_id)
  {
    // Identify resources and map them to the appropriate members.
    // Ideally this should be unnecessary and an enumeration should
    // be exported to denote each resource.  However, for now this
    // reverse engineering is necessary.
    //
    // The way we here distinguish the different kinds of resources
    // is highly target dependent, and we must therefore change this
    // as the target resource descriptors change.  We catch such cases
    // by an assertion.
    //
    bool unknown_resource = FALSE;
    
    Is_True(name != NULL, ("CG_GROUPING cannot handle unnamed resource"));
    
    switch (name[0])
    {
    case 'B':
      if (   strcmp("B0_or_B1", name) != 0  // Ignore "B0_or_B1" for now
	  && strcmp("B0_or_B2", name) != 0 // Ignore "B0_or_B2" for now
	  && strcmp("B2", name) != 0) // Ignore "B2" for now
	unknown_resource = TRUE;
      break;

    case 'b':
      if (strcmp("branch", name) == 0)
	_b_unit_id = res_id;
      else
	unknown_resource = TRUE;
      break;
    
    case 'f':
      if (strcmp("floating-point0", name) == 0)
	_f0_unit_id = res_id;
      else if (strcmp("floating-point", name) == 0)
	_f_unit_id = res_id;
      else
	unknown_resource = TRUE;
      break;
      
    case 'i':
      if (strcmp("integer_or_memory", name) == 0)
	_mi_unit_id = res_id;
      else if (strcmp("integer0", name) == 0)
	_i0_unit_id = res_id;
      else if (strcmp("integer", name) == 0)
	_i_unit_id = res_id;
      else if (strcmp("issue", name) != 0) // Ignore "issue" for now
	unknown_resource = TRUE;
      break;
    
    case 'm':
      if (strcmp("memory0", name) == 0)
	_m0_unit_id = res_id;
      else if (strcmp("memory", name) == 0)
	_m_unit_id = res_id;
      else if (strcmp("memory_ld", name) == 0)
        _m_ld_unit_id = res_id; 
      else if (strcmp("memory_st", name) == 0)
        _m_st_unit_id = res_id;
      else if (strcmp("memory2", name) == 0)
        _m2_unit_id = res_id;
      else
	unknown_resource = TRUE;
      break;

    case 's':
      if (strcmp("sem", name) != 0) // Ignore "sem" for now
	unknown_resource = TRUE;
      break;

    default:
      unknown_resource = TRUE;
      break;
    }
    Is_True(!unknown_resource, 
	    ("CG_GROUPING cannot handle unknown resource (%s)", name));
  } // _init_resource

public:

  CG_GROUPING(MEM_POOL *mpool, SI_RESOURCE_ID resource_count) :
     _reserved(resource_count, 0, INT32_VECTOR::allocator_type(mpool)),
     _total_no_of_bundles(0),
     _no_of_bundles_in_group(0)
  {
    for (SI_RESOURCE_ID res_id = 0; res_id < resource_count; ++res_id)
      _init_resource(SI_RESOURCE_ID_Name(res_id), res_id);

    // Currently, this is Itanium(TM) specific.  It denotes the preferred 
    // ISA_EXEC_UNIT_PROPERTY we want in each slot, where the highest
    // preference is listed first.
    //
    Is_True(ISA_MAX_SLOTS == 3, ("Unexpected bundle size for CG_GROUPING"));

    // For slot 0:
    //
    _no_of_preferences[0] = 2;
    _bundling_preference[0][0] = ISA_EXEC_PROPERTY_M_Unit;
    _bundling_preference[0][1] = ISA_EXEC_PROPERTY_B_Unit;

    // For slot 1:
    //
    _no_of_preferences[1] = 4;
    _bundling_preference[1][0] = ISA_EXEC_PROPERTY_I_Unit;
    _bundling_preference[1][1] = ISA_EXEC_PROPERTY_F_Unit;
    _bundling_preference[1][2] = ISA_EXEC_PROPERTY_M_Unit;
    _bundling_preference[1][3] = ISA_EXEC_PROPERTY_B_Unit;

    // For slot 2:
    //
    _no_of_preferences[2] = 5;
    _bundling_preference[2][0] = ISA_EXEC_PROPERTY_I_Unit;
    _bundling_preference[2][1] = ISA_EXEC_PROPERTY_F_Unit;
    _bundling_preference[2][2] = ISA_EXEC_PROPERTY_I2_Unit;
    _bundling_preference[2][3] = ISA_EXEC_PROPERTY_B_Unit;
    _bundling_preference[2][4] = ISA_EXEC_PROPERTY_B2_Unit;
  }

  // -------------------- General utilities --------------------
  // -----------------------------------------------------------

  bool is_branch(TOP top) const
  {
    return EXEC_PROPERTY_is_B_Unit(top) || EXEC_PROPERTY_is_B2_Unit(top);
  }

  bool has_resource_choice(TOP top) const
  {
    return EXEC_PROPERTY_is_I_Unit(top) && EXEC_PROPERTY_is_M_Unit(top);
  }

  bool need_f0_unit(TOP top) const
  {
    SI_RESOURCE_TOTAL* rvec = TSI_Resource_Total_Vector(top);
    const UINT         size = TSI_Resource_Total_Vector_Size(top);
    for (UINT i = 0; i < size; ++i )
    {
      SI_RESOURCE_ID id = SI_RESOURCE_TOTAL_Resource_Id(&rvec[i]);
      if (id == _f0_unit_id)
	return true;
    }
    return false;
  }

  INT num_slots(TOP top) const {return ISA_PACK_Inst_Words(top);}
  INT max_slots_per_group() const {return ISA_MAX_SLOTS * ISA_MAX_ISSUE_BUNDLES; /* Itanium(TM) */}

  INT bundling_order(TOP top) const
  {
    // Try to order ops according to how ops are commonly ordered in
    // bundle templates: m->l->f0->f->i->mi->i2->b->b2.  Note that the order
    // of the following tests try to take into account the fact that
    // some ops may go to several alternative slots.  We use 16 as a
    // catch-all, if there are new slots introduced not handled here.
    //
    return (EXEC_PROPERTY_is_B_Unit(top)?   20 :
	    EXEC_PROPERTY_is_B2_Unit(top)?  30 :  // b2 and !b
	    EXEC_PROPERTY_is_L_Unit(top)?    6 :
	    (EXEC_PROPERTY_is_M_Unit(top) && 
	     EXEC_PROPERTY_is_I_Unit(top))? 10 :  // m and i
	    EXEC_PROPERTY_is_M_Unit(top)?    1 :  // m and !i
	    EXEC_PROPERTY_is_I_Unit(top)?    8 :
	    EXEC_PROPERTY_is_I2_Unit(top)?  15 :  // i2 and !l and !i
	    EXEC_PROPERTY_is_F_Unit(top)?    
	    (need_f0_unit(top) ? 4 : 5) :         // f0
	    16 /*catch-all*/);
  } // bundling_order

  // -------------------- Bundling slots --------------------
  // --------------------------------------------------------

  // Some accounting of the grouping as it proceeds.
  //
  void start_new_group(bool in_current_bundle)
  {
    _no_of_bundles_in_group = (in_current_bundle? 1 : 0);
  }
  void start_new_bundle(TI_BUNDLE *bundle)
  {
    TI_BUNDLE_Clear(bundle);
    ++_no_of_bundles_in_group;
    ++_total_no_of_bundles;
  }
  bool new_bundle_will_break_cycle() const
  {
    return (_no_of_bundles_in_group >= max_slots_per_group()/ISA_MAX_SLOTS);
  }
  bool in_first_bundle_of_group() const
  {
    return (_no_of_bundles_in_group == 1);
  }
  INT total_no_of_bundles() const
  {
    return _total_no_of_bundles;
  }
  INT absolute_slot_no(INT slot_in_bundle) const
  {
    return (_total_no_of_bundles - 1)*ISA_MAX_SLOTS + slot_in_bundle;
  }

  INT no_of_preferred_slot_kinds(INT slot) const
  {
    return _no_of_preferences[slot];
  }

  ISA_EXEC_UNIT_PROPERTY get_preferred_slot_kind(INT slot, INT pref) const
  {
    if (pref < _no_of_preferences[slot])
      return _bundling_preference[slot][pref];
    else
      return _bundling_preference[slot][_no_of_preferences[slot] - 1];
  }

  // -------------------- Resource availability --------------------
  // ---------------------------------------------------------------

  INT avail_m_units()  const
  {
    return SI_RESOURCE_ID_Avail_Per_Cycle(_m_unit_id) - _reserved[_m_unit_id];
  }
  INT avail_i_units()  const
  {
    return SI_RESOURCE_ID_Avail_Per_Cycle(_i_unit_id) - _reserved[_i_unit_id];
  }
  INT avail_f_units()  const
  {
    return SI_RESOURCE_ID_Avail_Per_Cycle(_f_unit_id) - _reserved[_f_unit_id];
  }
  INT avail_b_units()  const
  {
    return SI_RESOURCE_ID_Avail_Per_Cycle(_b_unit_id) - _reserved[_b_unit_id];
  }

  INT avail_resource_units(ISA_EXEC_UNIT_PROPERTY prop)
  {
    if (prop == ISA_EXEC_PROPERTY_M_Unit)
      return avail_m_units();
    else if (prop == ISA_EXEC_PROPERTY_I_Unit || 
	     prop == ISA_EXEC_PROPERTY_I2_Unit)
      return avail_i_units();
    else if (prop == ISA_EXEC_PROPERTY_F_Unit)
      return avail_f_units();
    else if (prop == ISA_EXEC_PROPERTY_B_Unit ||
	     prop == ISA_EXEC_PROPERTY_B2_Unit)
      return avail_b_units();
    else 
      return 0;
  } // avail_resource_units

  // -------------------- Resource usage --------------------
  // --------------------------------------------------------

  void clear_reserved()
  {
    for (INT32_VECTOR::iterator it = _reserved.begin(); 
	 it != _reserved.end();
	 ++it)
      *it = 0;
  } // clear_reserved

  bool inorder_reserve_resource(TOP top)
  {
    // Reserve resources based on the bundling type and the resources
    // reserved thus far.
    //
    bool               reserved_ok = TRUE;
    SI_RESOURCE_TOTAL* rvec = TSI_Resource_Total_Vector(top);
    const UINT         size = TSI_Resource_Total_Vector_Size(top);
  
    // First add 1 to all resources this opcode will use.
    //
    for (UINT i = 0; reserved_ok && i < size; ++i )
    {
      SI_RESOURCE_ID id = SI_RESOURCE_TOTAL_Resource_Id(&rvec[i]);
      if (_reserved[id] > SI_RESOURCE_ID_Avail_Per_Cycle(id))
	reserved_ok = false;
      else
	++_reserved[id];
    }
  
    if (reserved_ok)
    {
      // Update dependent resources.
      //
      if (_reserved[_m_unit_id] > 0 && _reserved[_m0_unit_id] == 0)
	_reserved[_m0_unit_id] = 1;  // The first m unit reserved must be m0
      else if (_reserved[_f_unit_id] > 0 && _reserved[_f0_unit_id] == 0)
	_reserved[_f0_unit_id] = 1;  // The first f unit reserved must be f0
      else if (_reserved[_i_unit_id] > 0 && _reserved[_i0_unit_id] == 0)
	_reserved[_i0_unit_id] = 1;  // The first i unit reserved must be i0
    }
    return reserved_ok;
  } // inorder_reserve_resource

  bool reserve_resource(ISA_EXEC_UNIT_PROPERTY prop)
  {
    bool ok = true;

    if (prop == ISA_EXEC_PROPERTY_M_Unit)
    {
      ++_reserved[_m_unit_id];
      if (_reserved[_m0_unit_id] == 0)
	_reserved[_m0_unit_id] = 1;
    }
    else if (prop == ISA_EXEC_PROPERTY_I_Unit)
    {
      ++_reserved[_i_unit_id];
      if (_reserved[_i0_unit_id] == 0)
	_reserved[_i0_unit_id] = 1;
    }
    else if (prop == ISA_EXEC_PROPERTY_F_Unit)
    {
      ++_reserved[_f_unit_id];
      if (_reserved[_f0_unit_id] == 0)
	_reserved[_f0_unit_id] = 1;
    }
    else if (prop == ISA_EXEC_PROPERTY_B_Unit || 
	     prop == ISA_EXEC_PROPERTY_B2_Unit)
      ++_reserved[_b_unit_id];
    else 
      ok = false;

    return ok;
  } // reserve_resource

  INT  resource_slack(TOP top) const
  {
    // This is a measure of how critical the scheduling of an op is; we
    // compare the number of available resources in a cycle with the
    // number of resources used by op, and return difference.
    //
    // The smaller the resource "resource_slack", the more critical 
    // scheduling of the op is assumed to be.
    //
    INT                slack = INT_MAX;
    SI_RESOURCE_TOTAL* rvec = TSI_Resource_Total_Vector(top);
    const UINT         size = TSI_Resource_Total_Vector_Size(top);
    for (UINT i = 0; i < size; ++i )
    {
      UINT diff = (SI_RESOURCE_TOTAL_Avail_Per_Cycle(&rvec[i]) -
		   SI_RESOURCE_TOTAL_Total_Used(&rvec[i]));
      if (diff < slack)
	slack = diff;
    }
    return slack;
  } // resource_slack

}; // class CG_GROUPING
  

#endif // cg_grouping_INCLUDED

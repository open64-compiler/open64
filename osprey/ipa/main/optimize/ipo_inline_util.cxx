/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include <stdint.h>
// This file contains utilities used during inlining.
// These utilities should not be member functions of IPO_INLINE
#include "wn_tree_util.h"	     // for Tree_iterators
#include "ipo_inline_util.h"         // for declarations
#include "targ_sim.h"                // for  Get_Return_Mtypes
#include "config_targ.h"             // for Pointer_type, Pointer_Size
#include "region_util.h"             // for REGION_is_mp 
#include "lwn_util.h"                // for LWN_Get_Parent(), etc
#ifdef KEY
#include "ipo_parent.h"		     // for WN_Get_Parent
#endif

// ======================================================================
// Processing related to Regions
// ======================================================================
// ======================================================================
// Compute the max region id in a function wn
// ======================================================================
// function object to count the max_region_id
struct WN_MAX_REGION_OBJ {
  mUINT32 max_val;
  WN_MAX_REGION_OBJ() : max_val(0){}
  void  operator()(WN* w) {
    if (WN_region_id(w) > max_val)
      max_val = WN_region_id(w);
  }
};

mUINT32 Compute_max_region_id(WN *caller_wn) 
{
  WN_TREE_CONTAINER<PRE_ORDER>  wcpre(caller_wn);
  WN_TREE_CONTAINER<PRE_ORDER>::iterator wipre;
  WN_MAX_REGION_OBJ region_max_obj;

  for (wipre=wcpre.begin(); wipre != wcpre.end(); ++wipre){
    switch(WN_operator(wipre.Wn())) {
    case OPR_REGION:
      region_max_obj( wipre.Wn());
      break;
    default:
      break;
    }
  }
  return region_max_obj.max_val;
}

// ======================================================================
// Preg related code
// ======================================================================
// ======================================================================
// Compute_Return_Pregs()
// ======================================================================
void
Compute_Return_Preg_Offset (WN *Callee, RETURN_PREG& rp,
			    BOOL use_lowered_return_preg, 
			    SCOPE *caller_scope_tab, SYMTAB_IDX caller_level)
{
    // get the return type from the callee's opcode
    const TY& func_type = Ty_Table[ST_pu_type (WN_st (Callee))];
    const TY& return_type = Ty_Table[TY_ret_type (func_type)];

    if (TY_kind (return_type) == KIND_VOID)
	return;

    if (!use_lowered_return_preg) {
	// callee is supposed to use RETURN_VAL to return the result via
	// special preg -1.
	if (TY_mtype (return_type) == MTYPE_M) {
	    // for structures , we use a tmp var
	    ST* st = New_ST (CURRENT_SYMTAB);
	    ST_Init (st, Save_Str ("rr"), CLASS_VAR, SCLASS_AUTO,
		     EXPORT_LOCAL, TY_ret_type (func_type));
	    Set_ST_is_temp_var (st);
	    rp.insert (st);
	} else
	    rp.insert (Create_Preg_explicit (TY_mtype (return_type),
					     "rr", caller_scope_tab,
					     caller_level),
		       MTYPE_To_PREG (TY_mtype (return_type)));
	return;
    } else {
	// obsolete code to be removed

	// function return code has been lowered into dedicated pregs.  We
	// now need to find out which and how many such pregs are used for
	// the return value.

	// CALLER SIDE OPTIMIZATION
	TYPE_ID ty1, ty2;
	PREG_NUM rreg1 = 0;
	PREG_NUM rreg2 = 0;


	// get the return type information 
	// MTYPE_V means that the value is not returned in registers
	
	if (WHIRL_Return_Info_On) {
	
	    RETURN_INFO return_info =
		Get_Return_Info (TY_ret_type (func_type), Use_Simulated);

	    if (RETURN_INFO_count(return_info) <= 2) {

		ty1 = RETURN_INFO_mtype (return_info, 0);
		ty2 = RETURN_INFO_mtype (return_info, 1);
		rreg1 = RETURN_INFO_preg (return_info, 0);
		rreg2 = RETURN_INFO_preg (return_info, 1);
	    } else
		Fail_FmtAssertion ("Compute_Return_Preg_Offset: more than 2 return registers");
	} else {
	    Get_Return_Mtypes (TY_ret_type (func_type), Use_Simulated,
			       &ty1, &ty2); 
	
	// get the return preg numbers
	// 0 means it is not returned in any register
	    Get_Return_Pregs( ty1, ty2, &rreg1, &rreg2);
	}
      
	// note current state should be caller's
	// TODO: Set Caller Tables? Create Preg in CALLER!
	// assign these registers unique values
	    
	if (rreg1)
	    rp.insert (rreg1, Create_Preg_explicit (ty1, "rr",
						    caller_scope_tab,
						    caller_level));
      
	// assign these registers unique values
	if (rreg2)
	    rp.insert (rreg2, Create_Preg_explicit (ty2, "rr",
						    caller_scope_tab,
						    caller_level));
    }
} // Compute_Return_Preg_Offset


static inline void
Fix_LDID_Of_Return_Preg (WN* stmt, const RETURN_PREG& rp)
{
    TREE_ITER iter (stmt);

    while (iter.Wn () != NULL) {
	WN* wn = iter.Wn ();
	if (WN_operator (wn) == OPR_LDID) {
	    // check is it's an LDID to a dedicated preg
	    if (ST_sclass (WN_st (wn)) == SCLASS_REG) {
		if (WN_offset (wn) == (PREG_IDX) -1) {
		    if (WN_desc(wn) == MTYPE_M) {
			WN_offset (wn) = 0;
			WN_st_idx (wn) = ST_st_idx (rp.find_st ());
		    } else {
			WN_offset(wn) = rp.find (WN_offset (wn));
			WN_st_idx (wn) =
			    ST_st_idx (MTYPE_To_PREG (WN_desc (wn)));
		    }
		} else if (Preg_Is_Dedicated (WN_offset(wn)))
		    WN_offset (wn) = rp.find (WN_offset (wn));
	    }
	}
	++iter;
    }
} // Fix_LDID_Of_Return_Preg


// ======================================================================
//   Fix_Return_Pregs();
// ======================================================================

// this is due to the flawed design of high whirl. Machine specific 
// return registers are exposed and have to be patched              
// during inlining. This involves  walking n stmts after the call  
// to look  for such LDIDs in the caller                            

void
Fix_Return_Pregs (WN *Call, const RETURN_PREG& rp)
{
    Is_True (rp.size () > 0 && WN_opcode (Call) != OPC_VCALL,
	     ("callee is not a function"));

    // CALLER SIDE OPTIMIZATION
    WN* node = WN_next(Call);

    if (node && WN_operator(node) == OPR_LABEL) {
	// This may be the INLINER inserted LABEL
	node = WN_next(node);
    }

    if (node && WN_operator (node) == OPR_DEALLOCA) {
	//inliner inserted DEALLOCA
	node = WN_next (node);
    }
    
    if (node != NULL) {

	if (WN_operator (node) == OPR_COMPGOTO) {
	    // this is a special case for computed gotos, the preg could
	    // occur in the comp goto switch value 
	    Fix_LDID_Of_Return_Preg (node, rp);
	    return;
	}

	for (INT i=0; i < rp.size (); ++i) {

	    // loading of the return preg must immediately followed the call
	    if (WN_operator (node) == OPR_STID
#if !(defined(_STANDALONE_INLINER) && defined(_LIGHTWEIGHT_INLINER))
        //bug fix for OSP_277, let ipa_inliner handle
		// MCALL
		//  MLDID PRE_RETURN
		// MRETURN_VAL
		//
		   || WN_operator(node) == OPR_RETURN_VAL
       //bug fix for OSP_327, let ipa_inliner handle
		// MCALL
		//  MLDID PRE_RETURN
		// MISTORE
		//
		   || WN_operator(node) == OPR_ISTORE
#endif		   
		   ) {
		Fix_LDID_Of_Return_Preg (WN_kid0 (node), rp);
		node = WN_next (node);
	    } else
		return;
	}
    } else {
	// check to see if this is under a common node.
	WN* parent = LWN_Get_Parent (Call);
	Is_True (parent, ("corrupted parent map"));
	WN* grandparent = LWN_Get_Parent (parent);
	if (grandparent && WN_operator (grandparent) == OPR_COMMA) {
	    Fix_LDID_Of_Return_Preg (WN_kid1 (grandparent), rp);
	}
    }
} // Fix_Return_Pregs

#ifdef KEY
// Get enclosing region information around callsite 'e' in caller 'n'
//
// An alternate way the search for an appropriate region can be handled is
// by using RID_map. First get the enclosing region for the call_wn using
// Parent_map as below, and then use RID_map to get any other enclosing region.
// Issues: RID_map is not accessible in ipa/inliner. It could be made
// available, but using 2 maps would require more complex logic than below.
// Moreover there should not be much difference in (compile-time) efficiency.
void
Get_enclosing_region (IPA_NODE * n, IPA_EDGE * e)
{
    PU caller = Pu_Table[ST_pu(n->Func_ST())];
    if (!(PU_src_lang (caller) & PU_CXX_LANG) || !PU_has_region (caller))
        return;
    // get caller scope
    SCOPE * old_scope = Scope_tab;
    Scope_tab = n->Scope();
                                                                                
    WN * call_wn = e->Whirl_Node();
    WN_MAP Caller_Parent_Map = n->Parent_Map();
    WN_MAP_TAB * Caller_Map_Tab = PU_Info_maptab(n->PU_Info());
                                                                                
    WN * parent = WN_Get_Parent (call_wn, Caller_Parent_Map, Caller_Map_Tab);
                                                                                
    for (; parent; parent=WN_Get_Parent (parent, Caller_Parent_Map, Caller_Map_Tab))
    {
        // the following covers REGION_KIND_EH and REGION_KIND_TRY
        if (WN_operator(parent) != OPR_REGION || !WN_region_is_EH(parent))
          continue;
        FmtAssert (WN_ereg_supp(parent), ("No EH information in EH region"));
        if (WN_block_empty (WN_region_pragmas (parent)))
            e->Set_EH_Whirl_Node (parent);  // enclosing EH region
        else
        {  // enclosing try block
            INITV_IDX initv = INITO_val (WN_ereg_supp (parent));
            e->Set_Try_Label (INITV_lab (initv));
        }
        if (e->EH_Whirl_Node() && e->Try_Label())
            break;
    }
    // restore old scope
    Scope_tab = old_scope;
}
#endif

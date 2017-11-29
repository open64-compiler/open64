/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: wn_verifier.cxx
// $Revision: 1.12 $
// $Date: 05/12/05 08:59:16-08:00 $
// $Author: bos@eng-24.pathscale.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.wn_verifier.cxx $
//
// Revision history:
//  8-20-97 naftulin - Original Version
//
// Description:
// Verifies a WHIRL tree is consistent.
//
// ====================================================================
// ====================================================================

/**********************************************************************
 * FILE:             wn_verifier.cxx
 * PROJECT:          WHIRL Verifier
 * PROGRAMMER:       Henry Naftulin (naftulin@cs.ucdavis.edu)
 * FUNCTION TO CALL: BOOL WN_verifier(WN *root);
 * DESCRIPTION:
 * This module verifies several basic assumptions about the WHIRL:
 *  1) That whirl is a tree (Is_WHIRL_tree)
 *  2) That blocks first and last nodes have appropriate
 *          pointers set to NULL (Proper_Block_Structure)
 *  3) That each LDID with return registers (2,3,32.34) has 
 *          a coresponding CALL statement (Call_parent_LDID)
 *  4) That PARM nodes parents are CALL nodes 
 *         (Param_parent_is_Call)
 *  5) That CALLs children are all PARM nodes
 *         (Call_children_are_PARM)
 *  6) That WHIRL nodes' opcode is legal (Is_legal_wn_opcode)
 *  7) Checks that nodes that suppose not to have a NULL ty
 *         indeed have valid ty (TY_is_not_NULL)
 *  8) Checks that nodes that suppose not to have a NULL st
 *         indeed have valid st (ST_is_not_NULL)
 *  9) Checks certain properties of the st of STID
 *         (STID_check_st_class)
 * 10) Checks it ty of LDA is not NULL and its type is a pointer
 *         (LDA_ty_not_NULL)
 * 11) Checks whether pragma that it supports are enclosed
 *         (* like parenthezation problem, or html tags 
 *            enclosing *) (Are_enclosed_pragmas)
 * 12) Checks that the TY and the field ids are consistent
 **********************************************************************/

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#define USE_STANDARD_TYPES
#include <algorithm>
#include <stdlib.h>
#include <vector>
#include <stack>

#include "defs.h"
#include "stab.h"
#include "wn.h"
#include "wn_map.h"
#include "wn_util.h"
#include "mempool.h"
#include "tracing.h"
#include "targ_sim.h"
#include "wn_pragmas.h"
#include "ir_reader.h"		// fdump_tree
#include "cxx_template.h"
#include "cxx_hash.h"
#include "config_asm.h"         // for User_Label_Number_Format
#include "glob.h"
#include "pu_info.h"

// Turning this switch to turn the debuging messages of the Verifier

#define Verifier_DEBUG 0

// This structure contains pragmas name and wn_parent
// pointer. It is used for cheking pragma scoping.

struct pragma_stack_type
{
  WN           *parent_wn;
  WN_PRAGMA_ID pragma_id;
};


// The following structure maps each of the supported
// PRAGMA_ID's to either pushing on the stack operation
// or matching the top of the stack with the pragma_starting_id
// Just like in parentization: '(' needs to be pushed,
// ')' - requres that the top of the stack should be '(' 

#define NUM_PRAGMAS_SUPPORTED 26

struct pragma_mapped_ids
{
  WN_PRAGMA_ID pragma_id;  
  BOOL         push;               // Do I need to push this ID on stack
  WN_PRAGMA_ID pragma_starting_id; // If not what ID should I expect on top
};                                 // of the stack


/*---------------------------------------------------------------------
 * The structure of the pragma_supported array is:
 * { pragma_name, push_it?, associated pragma on the top ot the stack }
 *---------------------------------------------------------------------*/
 
pragma_mapped_ids pragmas_supported[NUM_PRAGMAS_SUPPORTED] =
{
  { WN_PRAGMA_INLINE_BODY_START,      TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_INLINE_BODY_END,        FALSE, WN_PRAGMA_INLINE_BODY_START },
  { WN_PRAGMA_ENTER_GATE,             TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_EXIT_GATE,              FALSE, WN_PRAGMA_ENTER_GATE },
  { WN_PRAGMA_CRITICAL_SECTION_BEGIN, TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_CRITICAL_SECTION_END,   FALSE, WN_PRAGMA_CRITICAL_SECTION_BEGIN },
  { WN_PRAGMA_PARALLEL_BEGIN,         TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_PARALLEL_END,           FALSE, WN_PRAGMA_PARALLEL_BEGIN },
  { WN_PRAGMA_PDO_BEGIN,              TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_PDO_END,                FALSE, WN_PRAGMA_PDO_BEGIN },
  { WN_PRAGMA_PSECTION_BEGIN,         TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_PSECTION_END,           FALSE, WN_PRAGMA_PSECTION_BEGIN },
  { WN_PRAGMA_SINGLE_PROCESS_BEGIN,   TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_SINGLE_PROCESS_END,     FALSE, WN_PRAGMA_SINGLE_PROCESS_BEGIN },
  { WN_PRAGMA_FLIST_SKIP_BEGIN,       TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_FLIST_SKIP_END,         FALSE, WN_PRAGMA_FLIST_SKIP_BEGIN },
  { WN_PRAGMA_CLIST_SKIP_BEGIN,       TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_CLIST_SKIP_END,         FALSE, WN_PRAGMA_CLIST_SKIP_BEGIN },
  { WN_PRAGMA_INDEPENDENT_BEGIN,      TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_INDEPENDENT_END,        FALSE, WN_PRAGMA_INDEPENDENT_BEGIN },
  { WN_PRAGMA_CRI_CASE,               TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_CRI_ENDCASE,            FALSE, WN_PRAGMA_CRI_CASE },
  { WN_PRAGMA_CRI_GUARD,              TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_CRI_ENDGUARD,           FALSE, WN_PRAGMA_CRI_GUARD },
  { WN_PRAGMA_CRI_PARALLEL,           TRUE,  WN_PRAGMA_UNDEFINED },
  { WN_PRAGMA_CRI_ENDPARALLEL,        FALSE, WN_PRAGMA_CRI_PARALLEL}
};

class WN_Verifier{
  private:

    /*------------------------------------------------------
     * Private variable section
     *-----------------------------------------------------*/

    MEM_POOL _mem_pool;
    WN_MAP   _map;
    BOOL     _is_tree_OK;
    WN      *_func;
    std::stack< pragma_stack_type > _pragma_stack; 
   
    /*--------------------------------------------------------
     * Private function section
     * All those 4 functions support CALL_parent_LDID
     *--------------------------------------------------------*/
 
    BOOL     Is_return_register_of_call(WN *call_wn, PREG_NUM preg);
    WN      *One_level_removed_node(WN *parent_wn,OPERATOR opr);
    BOOL     Is_dedicated_return_register(WN_OFFSET preg);
 
 protected:

    /*--------------------------------------------------------
     * Protected function section
     *--------------------------------------------------------*/

    BOOL     Is_WHIRL_tree(WN *wn, WN *parent); 
    BOOL     CALL_parent_LDID(WN *wn, WN *parent_wn);
    BOOL     Proper_Block_Structure(WN *wn,OPCODE op);
    BOOL     Param_parent_is_Call(WN *wn,WN *parent_wn);
    BOOL     Call_children_are_PARM(WN *wn);
    BOOL     Is_legal_wn_opcode(OPCODE opc);
    BOOL     LDA_ty_not_NULL(WN *wn);
    BOOL     STID_check_st_class(WN *wn);
    BOOL     TY_is_not_NULL(WN *wn, OPCODE op);
    BOOL     ST_is_not_NULL(WN *wn, OPCODE op); 
    BOOL     Load_addr_TY_is_not_NULL(WN *wn, OPCODE op); 
    BOOL     Are_enclosed_pragmas(WN *wn, WN *parent_wn);
    BOOL     Field_id_valid (WN* wn);


  public:
             WN_Verifier(WN *wn);
            ~WN_Verifier();
    BOOL     WN_traverse_tree(WN *wn, WN *parent_wn);
};

/*-----------------------------------------------------------
 * This function initializes the memory map for later use in
 * Is_WHIRL_tree routine.
 *-----------------------------------------------------------*/


WN_Verifier::WN_Verifier(WN *wn)
{
  // Create and initialize memory pool

  MEM_POOL_Initialize(&_mem_pool, "Verifier_Pool", FALSE);
  MEM_POOL_Push(&_mem_pool);
  _map = WN_MAP_Create(&_mem_pool);
  
  // Empty WHIRL tree is OK

  _is_tree_OK=TRUE;

  if (WN_operator(wn) == OPR_FUNC_ENTRY)
    _func = wn;
  else
    _func = NULL;
}

// The destructor clears and gets rid of the map
WN_Verifier::~WN_Verifier(void)
{
  // Clear and dispose of the memory map

  WN_MAP_Delete(_map);
  MEM_POOL_Pop(&_mem_pool);
  MEM_POOL_Delete(&_mem_pool);
}

/*----------------------------------------------------------
 * This is the main driver of the verifier. It traverses the
 * tree calling appropriate verification procedures when the
 * needed.
 *----------------------------------------------------------*/


BOOL
WN_Verifier::WN_traverse_tree(WN *wn, WN *parent_wn)
{
  OPCODE op;
	
  if (wn)
  {
    op = WN_opcode(wn);
 
    // Check if WHIRL is a tree.
    _is_tree_OK &= Is_WHIRL_tree(wn, parent_wn);

    // Call appropriate verification procedures when you
    // see the opcodes listed below
    switch(OPCODE_operator(op))
    {
      case OPR_FUNC_ENTRY:
      case OPR_XGOTO:
      case OPR_ALTENTRY:
	    // check that TY should not be null 
	    _is_tree_OK &= ST_is_not_NULL(wn,op);
	    break;
      case OPR_LDID:
	    // check if the instruction before
	    // the parent of LDID is indeed CALL
	    // Also check if ST and TY aren't NULLs
            _is_tree_OK &= CALL_parent_LDID(wn, parent_wn);
            _is_tree_OK &= TY_is_not_NULL(wn,op);
	    _is_tree_OK &= ST_is_not_NULL(wn,op);
	    _is_tree_OK &= Field_id_valid(wn);
            break;
      case OPR_LDA:
	    // TY of LDA has to be not null - check that
	    // and it also has to be a pointer type.
	    // Also check that ST is not NULL
            _is_tree_OK &= LDA_ty_not_NULL(wn);
            _is_tree_OK &= ST_is_not_NULL(wn,op);
            break;
      case OPR_IDNAME:
      case OPR_CONST:
           // Check that ST is not NULL
	   _is_tree_OK &= ST_is_not_NULL(wn,op);
	   break;
      case OPR_STID:
	    // Here check the class of the ST
	    // Also check if TY and ST aren't NULLs
            _is_tree_OK &= STID_check_st_class(wn);
            _is_tree_OK &= TY_is_not_NULL(wn,op);
	    _is_tree_OK &= ST_is_not_NULL(wn,op);
	    _is_tree_OK &= Field_id_valid(wn);
	    break;
      case OPR_MLOAD:
      case OPR_ISTORE:
      case OPR_MSTORE:
	   // Check that TY is not NULL
	   _is_tree_OK &= TY_is_not_NULL(wn,op);
	   _is_tree_OK &= Field_id_valid(wn);
	   break;
      case OPR_ISTOREX:
      case OPR_TAS:
	   // Check that TY is not NULL
	   _is_tree_OK &= TY_is_not_NULL(wn,op);
	   break;
      case OPR_ILOAD:
           // Check if both TY's are not NULL's
           _is_tree_OK &= TY_is_not_NULL(wn,op);
           _is_tree_OK &= Load_addr_TY_is_not_NULL(wn,op);
	   _is_tree_OK &= Field_id_valid(wn);
           break;
      case OPR_PARM:
	    // Check if the parent is a CALL node
            _is_tree_OK &= Param_parent_is_Call(wn,parent_wn);
            break;
      case OPR_CALL:
      case OPR_PICCALL:
	    // Check if children of CALL node are PARM
	    // Also check the ST pointer
            _is_tree_OK &= Call_children_are_PARM(wn);
            _is_tree_OK &= ST_is_not_NULL(wn,op);
            break;
      case OPR_ICALL:
	   // Check if children of the CALL node are PARM
	   // Check that TY is not NULL
           _is_tree_OK &= Call_children_are_PARM(wn);
           _is_tree_OK &= TY_is_not_NULL(wn,op);
	   break;
      case OPR_INTRINSIC_CALL:
      case OPR_IO:
      case OPR_INTRINSIC_OP:
#ifdef KEY
      case OPR_PURE_CALL_OP:
#endif
 	   // Check if children of the CALL node are
	   // are PARMS
           _is_tree_OK &= Call_children_are_PARM(wn);
           break;
      case OPR_PRAGMA:
	   // Check pragma scoping
           _is_tree_OK &= Are_enclosed_pragmas(wn,parent_wn);
           break;
      default:
	  //check if the opcode is legal
	  _is_tree_OK &= Is_legal_wn_opcode(op);
    }
    // traverse the tree starting with this node.
    if ( op == OPC_BLOCK)
    {
      // Special traversal case for BLOCK structure
      Proper_Block_Structure(wn,op);
      for(WN *node = WN_first(wn); node; node = WN_next(node))
        _is_tree_OK &= WN_traverse_tree(node,wn);
    } else {
      for(INT32 i = 0; i < WN_kid_count(wn); i++)
       _is_tree_OK &= WN_traverse_tree(WN_kid(wn,i),wn);
    }
  }
  if (!parent_wn) {
    if (WN_Tree_Has_Duplicate_Labels(wn, &_mem_pool))
      Fail_FmtAssertion("WN_Verifier() found duplicate labels in "
                        "WHIRL tree");
  }
  return _is_tree_OK;
}

// This procedure checks whether TY of a node is not NULL
BOOL WN_Verifier::TY_is_not_NULL(WN *wn, OPCODE op)
{
  FmtAssert(WN_ty(wn) != (TY_IDX)NULL,
    ("WN_verifier Error (TY_is_not_NULL): whirl node %s has "
     "a TY == NULL", OPCODE_name(op)));
  return TRUE;
}

// This procedure checks whether address of the TY is not NULL
BOOL WN_Verifier::Load_addr_TY_is_not_NULL(WN *wn, OPCODE op)
{
  FmtAssert(WN_load_addr_ty(wn) != (TY_IDX)NULL,
	    ("WN_verifier Error (Load_addr_TY_is_not_NULL): whirl "
	     "node %s has a TY == NULL", OPCODE_name(op)));
  return TRUE;
}

// This procedure checks whether the ST of the
// node is null;

BOOL WN_Verifier::ST_is_not_NULL(WN *wn, OPCODE op)
{
  FmtAssert(WN_st_idx(wn)!=(ST_IDX)NULL,
	    ("WN_verifier Error (ST_is_not_NULL): whirl node %s "
	     "has a ST == NULL", OPCODE_name(op)));
  // IPA might leave behind stores to dead variables
  if (ST_is_not_used (WN_st(wn)) && !OPERATOR_is_store (WN_operator (wn)))
      DevWarn ("WN_verifier Error: whirl node %s references symbol"
	       " %s that is marked NOT_USED", OPCODE_name(op),
	       ST_name (WN_st(wn))); 
  return TRUE;
}

/*-----------------------------------------------------------
 * Check if the WHIRL tree is actually a tree
 * by checking if the current node was visited
 * before. It uses maps to accomplish this, maping a visited
 * node to its parent. If the node was not visited previously
 * the map should return NULL, and not the parent of the node
 *-----------------------------------------------------------*/
BOOL
WN_Verifier::Is_WHIRL_tree(WN *wn, WN *parent_wn)
{
  if (Is_legal_wn_opcode(WN_opcode(wn)) == FALSE)
    return FALSE;
  if ( WN_MAP_Get(_map, wn) != NULL ) {
    FmtAssert(FALSE, ("WN_verifier ERROR: This is not a WHIRL tree\n\t"
		      "(0x%x --> 0x%x, 0x%x --> 0x%x).\n",
		      WN_MAP_Get(_map, wn), wn, parent_wn, wn));
    return FALSE;
  } else
    WN_MAP_Set(_map, wn, (void *)parent_wn);
  return TRUE;
}

// This function returns TRUE iff the preg is indeed a dedicated
// preg: which are now pregs # 2, 3, 32, 34
BOOL WN_Verifier::Is_dedicated_return_register(WN_OFFSET preg)
{
#if defined(TARG_NVISA) || defined(TARG_SL)
  return Is_Return_Preg(preg);
#else
  return ((preg == 2) || (preg == 3) || (preg == 32) || (preg==34));
#endif
}

/*--------------------------------------------------------------
 * This function returns the node one level removed from
 * the LDID node.
 * [Call node]<->[STID node]
 *                  |
 *                  V
 *      [LDID dedicated return register]
 * Node call will be returned as the One_level_removed from LDID
 *--------------------------------------------------------------*/
WN *
WN_Verifier::One_level_removed_node(WN *parent_wn,OPERATOR opr)
{
  WN *temp_wn = NULL;
  
  if (opr == OPR_LDID && parent_wn != NULL && 
      WN_operator(parent_wn) == OPR_STID)
    temp_wn = WN_prev(parent_wn);
  return temp_wn;
}

/*---------------------------------------------------------------
 * This function returns TRUE iff the return register passed is
 * the valid return register of the call node call_wn.
 * This function gets the registers that the call_wn could
 * return and checks whether one of those is preg that is passed
 *  as a parameter.
 *---------------------------------------------------------------*/
BOOL
WN_Verifier::Is_return_register_of_call(WN *call_wn, PREG_NUM preg)
{      
  PREG_NUM retreg1, retreg2;
  TYPE_ID  ty1,ty2;

  if (Verifier_DEBUG)
    DevWarn("Verifier TODO: how do I know to use "
	    "Use_Similated/Complex_Not...");
  
  // I will need to worry about the Use_Similated later on
  const PU& pu = Pu_Table[ST_pu (WN_st (call_wn))];

#if defined(TARG_NVISA)
  FmtAssert(WHIRL_Return_Info_On, ("return_info required"));
#else
  if (WHIRL_Return_Info_On) 
#endif
  {

    RETURN_INFO return_info = Get_Return_Info (TY_ret_type (Ty_Table[PU_prototype (pu)]),
					       Complex_Not_Simulated
#ifdef TARG_X8664
					       , PU_ff2c_abi(pu)
#endif
					      );

    if (RETURN_INFO_count(return_info) <= MAX_NUMBER_OF_REGISTERS_FOR_RETURN) {
      INT i;
      for (i = 0; i < RETURN_INFO_count(return_info); ++i) {
        if (preg == RETURN_INFO_preg(return_info,i))
          return TRUE;
      }
      return FALSE;
    }
    else
      Fail_FmtAssertion (
	"WN_Verifier::Is_return_register_of_call: more than expected return registers");
  }

#if !defined(TARG_NVISA)
  else 
    Get_Return_Mtypes (TY_ret_type (Ty_Table[PU_prototype (pu)]),
		       Complex_Not_Simulated, &ty1,&ty2);
  //Get_(MTYPE_To_TY(WN_rtype(call_wn)),
  //                 Use_Simulated,
  //                 &ty1,&ty2);
  if (!WHIRL_Return_Info_On)
    Get_Return_Pregs(ty1,ty2,&retreg1,&retreg2);  
  return ((preg == retreg1) || (preg == retreg2)); 
#endif
}


/*---------------------------------------------------
 * Check if the instuction previous to parent 
 * of LDID is indeed a CALL node
 * and wheather the type of the registers
 * of the call node match the ones used in LDID.
 * So for example this would be OK:
 *    I4CALL
 *      LDID 2
 *    STID 45
 *---------------------------------------------------*/
BOOL WN_Verifier::CALL_parent_LDID(WN *wn, WN *parent_wn)
{
  OPCODE   opc=WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);
  WN       *temp_wn;  
  BOOL     slink_used;

#if !defined(TARG_IA64) && !defined(TARG_X8664) && !defined(TARG_PPC32)

  // At the LDID node with special registers that are dedicated only
  // to return values:
  //   check if the call is at most twice removed
  //   and that the registers that call can return are indeed
  //   the once that were returned.
  if (opr == OPR_LDID && ST_class(WN_st(wn)) == CLASS_PREG &&
      Is_dedicated_return_register(WN_offset(wn)))
    { 
      temp_wn = One_level_removed_node(parent_wn,opr);
      if (_func != NULL) {
	const PU& pu = Pu_Table[ST_pu (WN_st (_func))];
	slink_used = PU_is_nested_func(pu);
      } else
	slink_used = TRUE; // since we don't know, cannot assert
      // If the load was twise removed from call
      if ((temp_wn !=NULL) && (WN_operator(temp_wn) == OPR_STID))
        temp_wn = WN_prev(temp_wn);
      
      // here we need to consider another situation before filing a bug.
      // preg 2 can be used in the nested procedure, before STID to simlink
      // under function entry. 
      if (temp_wn==NULL) {
	if (!slink_used) {
	  DevWarn("WN_verifier Error (Call_parent_LDID): no CALL "
		  "instruction one/two level(s) removed from LDID %d",
		  WN_offset(wn));    
	  return FALSE;
	} else {
	  return TRUE;
	}
      }
      
      OPCODE   t_opc = WN_opcode(temp_wn);
      OPERATOR t_opr = OPCODE_operator(t_opc);
    
      // Check if parent indeed was the one of the CALL operators

      if ( t_opr == OPR_CALL || t_opr == OPR_ICALL ||
	   t_opr == OPR_INTRINSIC_CALL ||
	   t_opr == OPR_PICCALL || t_opr == OPR_IO ||
	   t_opr == OPR_INTRINSIC_OP) 
	{
	  // Unfortunately now I can only check OPR_CALL
	  if ( t_opr == OPR_CALL && 
	       !Is_return_register_of_call(temp_wn,WN_offset(wn)))
	    {
	      DevWarn("WN_verifier Error (Call_parent_LDID): different register "
		      "follows CALL than needed, LDID %d", WN_offset(wn));
	      return FALSE;
	    }
	  else if (Verifier_DEBUG)
	    DevWarn("Verifier (Call_parent_LDID): Call is Ok");
	}

      // here we need to consider another situation before filing a bug.
      // preg 2 can be used in the nested procedure, before STID to simlink
      // under function entry.
      else if (!slink_used)
	{
	  DevWarn("WN_verifier Error (Call_parent_LDID): LDID %d was not "
		  "following any CALL node", WN_offset(wn));
	  return FALSE;
	}
   
    }
#endif
  return TRUE;
}

// This procedure verifies that blocks' last statement
// and blocks first statement have appropriate NULL pointers
BOOL WN_Verifier::Proper_Block_Structure(WN *wn,OPCODE op)
{
  BOOL block_ok = TRUE;

  if (op == OPC_BLOCK)
  {
    // Check if first statement of block has prev == NULL;
    WN *first = WN_first(wn);
    WN *last = WN_last(wn);
    if ( first == NULL ) {
      FmtAssert( last == NULL,
		 ("WN_verifier Error (Proper_Block_Structure): first is NULL but last is not."));
      block_ok = FALSE;
    }
    if ( first != NULL && WN_prev(first) != NULL){ 
      FmtAssert(FALSE, ("WN_verifier Error (Proper_Block_Structure): This block does "
			"not have a null pointer in the first wn node"));
      block_ok = FALSE;
    }
    
    // Check if the last statement of block has next == NULL;
    if ( last != NULL && WN_next(last)  != NULL ) { 
      FmtAssert(FALSE, ("WN_verifier Error (Proper_Block_Structure): This block does "
			"not have a null pointer in the last wn node"));
      block_ok = FALSE;
    } 

    // Check WN_first can reach WN_last through WN_next
    WN *tmp = first;
    while (tmp && WN_next(tmp) != NULL) 
      tmp = WN_next(tmp);
    if (tmp != last) {
      FmtAssert (FALSE, ("WN_verifier Error (Proper_Block_Structure): last is not really last\n"));
      block_ok = FALSE;
    }

    // Check WN_last can reach WN_first through WN_prev
    tmp = last;
    while (tmp && WN_prev(tmp) != NULL) 
      tmp = WN_prev(tmp);
    if (tmp != first) {
      FmtAssert (FALSE, ("WN_verifier Error (Proper_Block_Structure): first is not really firstt\n"));
      block_ok = FALSE;
    }
  }
  return block_ok;
}

// This procedure checks that if the node is a PARM node, its parent have
// to be a CALL node. 

BOOL WN_Verifier::Param_parent_is_Call(WN *wn,WN *parent_wn)
{
  OPCODE   opc=WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);

  if (opr == OPR_PARM)
  {
    opc = WN_opcode(parent_wn);
    opr = OPCODE_operator(opc);
    if (opr == OPR_CALL || opr == OPR_ICALL ||
        opr == OPR_INTRINSIC_CALL ||
        opr == OPR_PICCALL || opr == OPR_IO ||
#ifdef KEY
	opr == OPR_PURE_CALL_OP ||
#endif
        opr == OPR_INTRINSIC_OP)
    {
      // It is ok, the parents of the PARM is a Call node.
      return TRUE;
    } else {
      // The parent of the PARM is not a Call node;
      DevWarn("WN_verifier Error (Param_parent_is_Call): The parent of the "
	      "PARM node is not a CALL node but a %s node",OPCODE_name(opc));
      return FALSE;
    }
  }
  return TRUE;
}


/*----------------------------------------------------------------
 * This procedure checks that the childern of the CALL statement
 * should only be PARMs. Several exeptions apply: the last
 * kid of the ICALL or PICCALL should not be a PARAM. Also
 * OPR_IO_ITEM could be a kid of OPR_IO.
 *----------------------------------------------------------------*/

BOOL WN_Verifier::Call_children_are_PARM(WN *wn)
{
  OPCODE   opc=WN_opcode(wn);
  OPCODE   parent_opc=opc;
  OPERATOR opr = OPCODE_operator(opc);
  OPERATOR parent_opr=opr;

  if (opr == OPR_CALL || 
      opr == OPR_INTRINSIC_CALL || 
      opr == OPR_INTRINSIC_OP   ||
#ifdef KEY
      opr == OPR_PURE_CALL_OP   ||
#endif
      opr == OPR_IO) 
  {
    for(INT32 i=0; i < WN_kid_count(wn); i++)
    {
      opc = WN_opcode(WN_kid(wn,i));
      opr = OPCODE_operator(opc);

      // If the cild of the call is not PARM
      // or is not IO_ITEM (* in a special case of OPR_IO *)
      // then error out.
      // NOTE: I belive that OPR_IO can have cildren called PARM
      // so thats why I structured my if checking for PARM first
      // and only than special condition for IO

      if ((opr != OPR_PARM ) &&
          ((parent_opr == OPR_IO) && (opr != OPR_IO_ITEM)) ) 
      {
        // The parent of the PARM is not a Call node;
        DevWarn("WN_verifier Error (Call_children_are_PARM): The child of %s "
		"node is not a PARM node but a %s node",
		OPCODE_name(parent_opc), OPCODE_name(opc));
        return FALSE;
      }
    }
  }
  else if ( opr == OPR_PICCALL || opr == OPR_ICALL)
  {
      for(INT32 i=0; i < WN_kid_count(wn); i++)
      { opc = WN_opcode(WN_kid(wn,i));
        opr = OPCODE_operator(opc);

        // Regular case: clidren of CALL except for the last 
	// one should be PARM
        if ((opr != OPR_PARM) && (i < (WN_kid_count(wn)-1) ))  
        {
          // The parent of the PARM is not a Call node;
          DevWarn("WN_verifier Error (Call_children_are_PARM): The child of "
		  "CALL node is not a  PARM node but a %s node",
		  OPCODE_name(opc));
          return FALSE;
        }
        // Here we need to check the exception
        else if ((opr == OPR_PARM) && (i == (WN_kid_count(wn)-1)))
	{
          DevWarn("WN_verifier Error (Call_children_are_PARM): The last "
		  "child of (P)ICALL node is a  PARM node");
          return FALSE;
	}
      }
  }
  
  return TRUE;
}


/*------------------------------------------------------------------
 * This procedure checks if the opcode is legal
 * Since all possible combinations of the opcodes are defined in
 * the opcode_gen_core.h file I only need to check the range of the
 * opcode
 *------------------------------------------------------------------*/
BOOL WN_Verifier::Is_legal_wn_opcode(OPCODE opc)
{
  
  if (opc < OPCODE_FIRST || opc > OPCODE_LAST)
  {
    DevWarn("WN_verifier Error (Is_legal_wn_opcode): The opcode %s "
	    "is illegal",OPCODE_name(opc));
  }
  return ((opc >= OPCODE_FIRST) && (opc <= OPCODE_LAST));
}

// This subroutine checks that LDA's TY is not NULL,
// and it has to be a pointer type
BOOL WN_Verifier::LDA_ty_not_NULL(WN *wn)
{
    OPCODE   opc = WN_opcode(wn);
    OPERATOR opr = OPCODE_operator(opc);
    
    if (opr == OPR_LDA) {
	const TY& ty = Ty_Table[WN_ty (wn)];
	if (WN_ty (wn) == 0 ||
	    !(TY_kind(ty) == KIND_POINTER || TY_kind(ty) == KIND_SCALAR)) {
	    DevWarn("WN_verifier Error (LDA_ty_not_NULL): TY of the %s is "
		    "either NULL or is not a pointer or scalar",
		    OPCODE_name(opc));
	    if (DevWarn_Enabled()) {
#ifdef KEY // print type name only for shorter output
	      if (TY_name_idx(ty) == 0)
		fprintf(stderr, "(anon)\n");
	      else fprintf(stderr, "%s\n", TY_name(ty));
#else
	    ty.Print (stderr);
#endif
	    }
	    return FALSE;
	}
    }
    return TRUE;
}

// This procedure checks restrictions on st of STID
// It also checks that if dedicated return
// register is used than the next statement
// after should be RETURN
BOOL WN_Verifier::STID_check_st_class(WN *wn)
{
  OPCODE   opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);
 
  if (opr == OPR_STID) {
    ST * st = WN_st(wn);
    // Restricting the ST_class
    if ( ( ST_class(st)!=CLASS_VAR ) &&
         ( ST_class(st)!=CLASS_PREG ) &&
         ( ST_class(st)!=CLASS_BLOCK)    )
    {
      DevWarn("WN_verifier Error (STID_check_st_class): ST of the STID is "
	      "not CLASS: VAR, PREG or Block but %d", ST_class(st));
      return FALSE;
    } 
#ifndef TARG_X8664
    if ( (ST_class(st) == CLASS_PREG) && 
         (Is_dedicated_return_register(WN_offset(wn))) )
    {
       WN *temp_wn = WN_next(wn);
       
       // There could be two STIDs following one another
       // before actual return statement. So
       if ((temp_wn != NULL) && 
	   (WN_operator(temp_wn) == OPR_STID) &&
           (ST_class(WN_st(temp_wn)) == CLASS_PREG ) &&
           (Is_dedicated_return_register(WN_offset(temp_wn)))  )
       {
	 // Than scip this STID and look at the next statement
	 // Since function can only have up to two 
	 // dedicatd return STIDs before OPC_RETURN
    
         temp_wn=WN_next(temp_wn);
       }

       //
       // Check if return follows the STID of the dedicated register
       // also check for call immediatly following use of register 2 (static link register)
       //
       if ((WN_offset(wn) != Static_Link_Preg_Offset) && 
	   ((temp_wn == NULL) || (WN_operator(temp_wn) != OPR_RETURN)) )
	 {
	   DevWarn("WN_verifier Error (STID_check_st_class): STID %d was "
		   "followed by %s and not by OPC_RETURN",
                   WN_offset(wn),
                   temp_wn ? OPCODE_name(WN_opcode(temp_wn)) : "NULL");
	 }
       else if ((WN_offset(wn) == Static_Link_Preg_Offset) && 
		((temp_wn == NULL) || (WN_operator(temp_wn) != OPR_RETURN)
		 && (WN_operator(temp_wn) != OPR_PICCALL)
		 && (WN_operator(temp_wn) != OPR_CALL)) )
	 {
#ifndef TARG_PPC32
	   DevWarn("WN_verifier Error (STID_check_st_class): STID %d was "
		   "followed by %s and not by OPC_RETURN or OPR_CALL or OPR_PICCALL",
		   WN_offset(wn),OPCODE_name(WN_opcode(temp_wn)));
#endif
	 }
    }
#endif

    // Restricting the ST_sclass
    switch(ST_sclass(st))
    {
      case SCLASS_UNKNOWN:
      case SCLASS_AUTO:
      case SCLASS_FORMAL:
      case SCLASS_PSTATIC:
      case SCLASS_FSTATIC:
      case SCLASS_COMMON:
      case SCLASS_EXTERN:
      case SCLASS_UGLOBAL:
      case SCLASS_DGLOBAL:
      case SCLASS_REG:
      case SCLASS_FORMAL_REF:
	   //if (Verifier_DEBUG)
           //    DevWarn("The SCLASS of ty is fine");
           break;

      case SCLASS_TEXT:
	   DevWarn("WN_verifier Error (STID_check_st_class): ST SCLASS "
		   "is SCALSS_TEXT");
	   Print_ST(stderr, st, FALSE);
	   return FALSE;
	   
     
    default:
           DevWarn("WN_verifier Error (STID_check_st_class): ST SCLASS "
		   "is unknown");
	   Print_ST(stderr, st, FALSE);
	   return FALSE;
    }
  }
  return TRUE;
}


/*-----------------------------------------------------
 * This routine will check the scoping of several
 * pragmas. Pragmas should be self enclosed. What 
 * is ment by that is that if I have a pragma 
 * that starts the scope I should be able to
 * find a pragma that ends the scope on the same
 * block level as pragma that started the scope.
 * So checking pragmas would be like checking
 * parentisation rules, where pragma itself and
 * parent wn node would be saved on stack.
 * Algo: 
 * whenever I see a pragmacall this routine.
 * If this is the pragma that I support -
 * if it starts section put it on stack,
 * if it ends section check the top of
 * the stack and if it is Ok than pop off
 * else give warning.
 *------------------------------------------------------*/
BOOL WN_Verifier::Are_enclosed_pragmas(WN *wn,WN *parent_wn)
{
  pragma_stack_type temp;
  WN_PRAGMA_ID temp_id= (WN_PRAGMA_ID) WN_pragma(wn);
  int  i = 0;

  // Check if the pragma passed is supported
  while(i<NUM_PRAGMAS_SUPPORTED)
  {
    // If the pragma passed is supported then
    if (pragmas_supported[i].pragma_id=temp_id)
    {
       // If the flag of the supported pragma says push
       // push the pragma and parent id on the stack.
       if (pragmas_supported[i].push)
       {
         temp.pragma_id = temp_id;
         temp.parent_wn = parent_wn;
         _pragma_stack.push(temp);
          return TRUE;
       }
       // Otherwise we need to pop pragma - so check that stack
       // is not empty
       else if (_pragma_stack.size()>0)
       {
          temp= _pragma_stack.top();

          // If the top of the stack contains the pragma we expected
          // and the pragma closing it is at the same level (checked by
          // parent_wn pointers) than we can pop pragma from the stack

          if (temp.pragma_id == pragmas_supported[i].pragma_starting_id &&
              temp.parent_wn == parent_wn)
          {
             if (Verifier_DEBUG)
               DevWarn("Stack worked!!");
             _pragma_stack.pop();
             return TRUE;
          }
          else if (temp.pragma_id != pragmas_supported[i].pragma_starting_id)
          {
            DevWarn("WN_verifier Error (Are_enclosed_pragmas): on stack "
		    "expecting %d but got %d",
		    pragmas_supported[i].pragma_starting_id, temp.pragma_id);
            return FALSE;
          } 
          else
          {
	    DevWarn("WN_verifier Error (Are_enclosed_pragmas): the pragma "
		    "is closed by different level of the parent");
            return FALSE;
          }
      
       }
       else // size of the stack is 0 but we need to pop
       {
         return FALSE;
       }
    } // if the pragma was supported
    i++;
  } // while you are going through supported pragmas
  return TRUE;
}


BOOL
WN_Verifier::Field_id_valid (WN* wn)
{
    const TY* ty = &Ty_Table[WN_ty (wn)];
    
    switch (WN_operator(wn)) {
    case OPR_MLOAD:
    case OPR_MSTORE:
	Is_True (TY_kind (*ty) == KIND_POINTER,
		 ("MLOAD/MSTORE expects a pointer type"));
	if (TY_kind(TY_pointed(*ty)) == KIND_STRUCT &&
	    WN_field_id(wn) == 0) {
	    WN* kid = WN_operator(wn) == OPR_MLOAD ? WN_kid1(wn) : WN_kid2(wn);
	    if (WN_operator(kid) == OPR_INTCONST) {
		INT64 ty_size = TY_size(TY_pointed(*ty));
		// must be multiple of the size of the high-level type
		Is_True (ty_size == 0 || WN_const_val (kid) % ty_size == 0,
			 ("MLOAD/MSTORE size is inconsistent with TY"));
	    }
	}
	break;
    case OPR_ILOAD:
        ty = &Ty_Table[WN_load_addr_ty(wn)];
	// fall thru
    case OPR_ISTORE:
	ty = &Ty_Table[TY_pointed (*ty)];
	if (strncmp(TY_name(*ty), ".dope.", 6) == 0)
	  break; // make exception for fortran dope vector
	// fall through
    case OPR_STID:
    case OPR_LDID:
	if (TY_kind(*ty) != KIND_STRUCT) {
	    Is_True (WN_field_id(wn) == 0,
		     ("non-zero field id for memory op on scalar"));
	} else if (WN_field_id(wn) == 0) {
#if !defined(TARG_PPC32)	
/* maybe this verify is not neccesary. 
	    Is_True (WN_desc(wn) == MTYPE_M ||
	    	     MTYPE_byte_size(WN_desc(wn)) == TY_size(*ty),
	    	     ("field_id and descriptor type are inconsistent")); 
*/
#endif
	}
	break;
    default:
        break;
    }

    return TRUE;
} // Field_id_valid

  
/* Return TRUE if tree rooted at pu_wn has any OPR_LABEL nodes that share
 * label numbers, return FALSE otherwise. tmp_pool must be initialized and
 * not frozen.
 */

BOOL WN_Tree_Has_Duplicate_Labels(WN *pu_wn, MEM_POOL *tmp_pool)
{
  MEM_POOL_Popper popper(tmp_pool);
  WN_ITER *it = WN_WALK_TreeIter(pu_wn);
  HASH_TABLE<LABEL_IDX, WN *> labels_found(257, tmp_pool);

  while (it) {
    WN *wn = it->wn;

    if (WN_operator(wn) == OPR_LABEL) {
      LABEL_IDX lab = WN_label_number(wn);
      Is_True(LABEL_IDX_index(lab) > 0, ("WN_verifier: found label with number 0"));
      Is_True(LABEL_IDX_index(lab)<=LABEL_Table_Size(LABEL_IDX_level(lab)),
              ("WN_verifier: label %d greater than last label %d",
               (INT) lab, LABEL_Table_Size(LABEL_IDX_level(lab))));
      WN *dup_lab_wn = labels_found.Find(lab);

      if (dup_lab_wn){
        WN_WALK_Abort(it);
        return TRUE;
      }

      labels_found.Enter(lab, wn);
    }

    it = WN_WALK_TreeNext(it);
  }

  return FALSE;
}


// classification of which label fields are present in a Whirl node
enum WN_Label_Fields {
  WN_HAS_NO_LABELS,
  WN_HAS_LABEL,     // WN_label_number(wn) is valid
  WN_HAS_LAST_LABEL // WN_last_label(wn) is valid
};

// return which type of label field is valid for wn
static WN_Label_Fields WN_Has_Label(WN *wn)
{
  Is_True(wn, ("NULL wn"));

  switch (WN_operator(wn)) {
  case OPR_CASEGOTO:
  case OPR_FALSEBR:
  case OPR_GOTO:
  case OPR_LABEL:
  case OPR_REGION_EXIT:
  case OPR_TRUEBR:
    return WN_HAS_LABEL;
  case OPR_COMPGOTO:
  case OPR_SWITCH:
    return WN_HAS_LAST_LABEL;
  default:
    break;
  }

  return WN_HAS_NO_LABELS;
}

typedef HASH_TABLE<LABEL_IDX, LABEL_IDX> LABEL_RENAMING_MAP;

static BOOL
References_Some_Label(WN *pu_wn, LABEL_RENAMING_MAP *lab_map, WN *orig_wn);

static void
Rename_INITV_Labels(INITO_IDX inito_idx, LABEL_RENAMING_MAP *lab_map,
                    MEM_POOL *tmp_pool);

BOOL
WN_Rename_Duplicate_Labels(WN *orig_wn, WN *copied_wn, WN *pu_wn,
                           MEM_POOL *tmp_pool)
{
  MEM_POOL_Popper popper(tmp_pool);
  LABEL_RENAMING_MAP lab_map(1021, tmp_pool);

    // walk orig_wn, creating a mapping in lab_map for each label we find
  WN_ITER *orig_it = WN_WALK_TreeIter(orig_wn);

  while (orig_it) {
    WN *wn = orig_it->wn;

    if (WN_operator(wn) == OPR_LABEL) {
        // create new label
      LABEL_IDX lab_idx = WN_label_number(wn), new_lab_idx;
      Is_True(!lab_map.Find(lab_idx),
              ("duplicate label %d in orig_wn", (INT) lab_idx));

      LABEL &new_lab = New_LABEL(LABEL_IDX_level(lab_idx), new_lab_idx);
      char* Cur_PU_Name = ST_name(PU_Info_proc_sym(Current_PU_Info));
      INT strsize = strlen(User_Label_Number_Format) + 64 + strlen(Cur_PU_Name);
      char * labelname = (char*) calloc(strsize, 1);
      sprintf(labelname, User_Label_Number_Format, (INT)LABEL_IDX_level(new_lab_idx),
              (INT) new_lab_idx, Cur_PU_Name);
      LABEL_Init(new_lab, Save_Str(labelname),
                 LABEL_kind((*Scope_tab[LABEL_IDX_level(new_lab_idx)].label_tab)[LABEL_IDX_index(lab_idx)]));

      lab_map.Enter(lab_idx, new_lab_idx);  // add mapping from old to new
    }

    orig_it = WN_WALK_TreeNext(orig_it);
  }

    // walk copied_wn, renaming each referenced label as per lab_map
  WN_ITER *copied_it = WN_WALK_TreeIter(copied_wn);

  while (copied_it) {
    WN *wn = copied_it->wn;
    WN_Label_Fields wn_lab = WN_Has_Label(wn);

    if (wn_lab == WN_HAS_LABEL || wn_lab == WN_HAS_LAST_LABEL) {
      LABEL_IDX lab_idx = (wn_lab == WN_HAS_LABEL) ? WN_label_number(wn) :
                          WN_last_label(wn);

      LABEL_IDX new_lab_idx = lab_map.Find(lab_idx);
      if (new_lab_idx) {
        if (wn_lab == WN_HAS_LABEL)
          WN_label_number(wn) = new_lab_idx;
        else
          WN_last_label(wn) = new_lab_idx;
      }

    } else if (wn_lab != WN_HAS_NO_LABELS)
      Fail_FmtAssertion("impossible return value from WN_Has_Label");

    if (WN_operator(wn) == OPR_REGION && WN_ereg_supp(wn))
      Rename_INITV_Labels(WN_ereg_supp(wn), &lab_map, tmp_pool);

    copied_it = WN_WALK_TreeNext(copied_it);
  }

  if (!pu_wn)
    return TRUE;

    // verify that each label in lab_map was "internal" to orig_wn
  return !References_Some_Label(pu_wn, &lab_map, orig_wn);
}

// Return TRUE if pu_wn references some label that's a key in lab_map, FALSE
// otherwise.  Skip subtree orig_wn if it's not NULL.
static BOOL
References_Some_Label(WN *pu_wn, LABEL_RENAMING_MAP *lab_map, WN *orig_wn)
{
  Is_True(pu_wn, ("NULL pu_wn"));

  if (pu_wn == orig_wn)
    return FALSE; // skip subtree orig_wn

  WN_Label_Fields wn_lab = WN_Has_Label(pu_wn);

  if (wn_lab == WN_HAS_LABEL || wn_lab == WN_HAS_LAST_LABEL) {
    LABEL_IDX lab_idx = (wn_lab == WN_HAS_LABEL) ? WN_label_number(pu_wn) :
                        WN_last_label(pu_wn);
    if (lab_map->Find(lab_idx))
      return TRUE;

  } else if (wn_lab != WN_HAS_NO_LABELS)
    Fail_FmtAssertion("impossible return value from WN_Has_Label");

  OPERATOR opr = WN_operator(pu_wn);

  if (!OPERATOR_is_leaf(opr)) {
    if (opr == OPR_BLOCK) {
      for (WN *kid = WN_first(pu_wn); kid; kid = WN_next(kid))
        if (References_Some_Label(kid, lab_map, orig_wn))
          return TRUE;

    } else {
      for (INT kidno = 0; kidno < WN_kid_count(pu_wn); kidno++)
        if (References_Some_Label(WN_kid(pu_wn, kidno), lab_map, orig_wn))
          return TRUE;
    }
  }

  return FALSE;
}


// If inito_idx's INITV tree contains any INITVs that reference labels in
// lab_map, then give inito_idx a deep copy of the INITV tree and rename
// all the labels as specified by lab_map
static void
Rename_INITV_Labels(INITO_IDX inito_idx, LABEL_RENAMING_MAP *lab_map,
                    MEM_POOL *tmp_pool)
{
  { // see if INITV tree contains any labels that require renaming
    BOOL renamed_labels_found = FALSE;
    STACK<INITV_IDX> initv_stack(tmp_pool);
    INITV_IDX val_idx = INITO_val(inito_idx);

    while (val_idx) {
      INITVKIND k = INITV_kind(val_idx);
      switch (k) {
      case INITVKIND_SYMOFF:
      case INITVKIND_ZERO:
      case INITVKIND_ONE:
      case INITVKIND_VAL:
      case INITVKIND_PAD:
        val_idx = INITV_next(val_idx);
        break;

      case INITVKIND_BLOCK:
        initv_stack.Push(val_idx);
        val_idx = INITV_blk(val_idx);
        break;

      case INITVKIND_SYMDIFF: // these 3 kinds contain labels
      case INITVKIND_SYMDIFF16:
      case INITVKIND_LABEL:
        {
          LABEL_IDX lab = (k == INITVKIND_LABEL) ? INITV_lab(val_idx) :
                          INITV_lab1(val_idx);
          if (lab_map->Find(lab))
            renamed_labels_found = TRUE;
#ifdef KEY
          // bug 5193
          else
            val_idx = INITV_next(val_idx);
#endif // KEY
        }
        break;

      default:
        Fail_FmtAssertion("unknown INITV kind %d", (INT) k);
      }
      if (renamed_labels_found)
        break;

      while (!val_idx && initv_stack.Elements() > 0) {
        val_idx = INITV_next(initv_stack.Pop());
      }
    }

    if (!renamed_labels_found)
      return; // no copying needed
  }

    // make a deep copy of INITV tree and rename the copy's labels 
  STACK<INITV_IDX> old_stack(tmp_pool), new_stack(tmp_pool);
  INITV_IDX old_initv = INITO_val(inito_idx),
            new_initv,
	    parent = INITV_IDX_ZERO,
	    prev = INITV_IDX_ZERO;
  BOOL first_new_initv = TRUE;

  while (old_initv) {
    new_initv = Copy_INITV(INITV_IDX_ZERO, INITO_IDX_ZERO, old_initv);

    if (first_new_initv) {
      Set_INITO_val(inito_idx, new_initv);
      first_new_initv = FALSE;
    } else if (parent) {
      Set_INITV_blk(parent, new_initv);
      parent = INITV_IDX_ZERO;
    } else if (prev) {
      Set_INITV_next(prev, new_initv);
    }

    INITVKIND k = INITV_kind(old_initv);
    switch (k) {
      case INITVKIND_SYMOFF:
      case INITVKIND_ZERO:
      case INITVKIND_ONE:
      case INITVKIND_VAL:
      case INITVKIND_PAD:
        old_initv = INITV_next(old_initv);
        prev = new_initv;
        break;

      case INITVKIND_BLOCK:
        old_stack.Push(old_initv);
        new_stack.Push(new_initv);
        old_initv = INITV_blk(old_initv);
        parent = new_initv;
        prev = INITV_IDX_ZERO;
        break;

      case INITVKIND_SYMDIFF:
      case INITVKIND_SYMDIFF16:
      case INITVKIND_LABEL:
        {
          LABEL_IDX lab = (k == INITVKIND_LABEL) ? INITV_lab(new_initv) :
                          INITV_lab1(new_initv),
                    new_lab = lab_map->Find(lab);

          if (new_lab)
            if (k == INITVKIND_LABEL)
              Set_INITV_lab(new_initv, new_lab);
            else
              Set_INITV_lab1(new_initv, new_lab);
        }
        old_initv = INITV_next(old_initv);
        prev = new_initv;
        break;

      default:
        Fail_FmtAssertion("unknown INITV kind %d", (INT) k);
    }

    while (!old_initv && old_stack.Elements() > 0) {
      old_initv = INITV_next(old_stack.Pop());
      prev = new_stack.Pop();
    }

  }
  
}


BOOL
WN_verifier(WN *wn)
{
  Temporary_Error_Phase ("WN_verifier");
#ifdef Is_True_On
  char *p = getenv ("WN_VERIFIER");
  if (p != 0 && strcasecmp (p, "off") == 0)
      return TRUE;
  if (Verifier_DEBUG)
      DevWarn("I am running newest verifier");
  WN_Verifier wnv(wn);
  return wnv.WN_traverse_tree(wn, NULL);
#else
  return TRUE;
#endif
}

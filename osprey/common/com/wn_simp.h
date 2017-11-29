/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/*---------------------------------------------------------------------------
                  WN simplifier

 Description:
  
  This file contains routines which simplify WHIRL trees as they  are 
  constructed.

  Exported functions:
  

  WN *WN_SimplifyIload(OPCODE opc, WN_OFFSET offset,
          TY_IDX ty, UINT field_id, TY_IDX load_addr_ty, WN *addr)
  WN *WN_SimplifyIstore(OPCODE opc, WN_OFFSET offset, TY_IDX ty, UINT field_id, WN *value, WN *addr);
  WN * WN_SimplifyIntrinsic(OPCODE opc, UINT32 intrinsic, INT32 n, WN *k[])
  WN * WN_SimplifyExp3(OPCODE opc, WN *k0, WN *k1, WN *k3)
  WN * WN_SimplifyExp2(OPCODE opc, WN *k0, WN *k1)
  WN * WN_SimplifyExp1(OPCODE opc, WN *k0)
  WN * WN_SimplifyCvtl(OPCODE opc, INT16 cvtl_bits, WN *k0)
       
    Simplify binary and unary operations. opc is the opcode we are
    trying to build, and k0 and k1 are the operands of the WHIRL node being constructed.
    If simplification occurred, the functions return a simplified node.
    If no simplifications occurred, the function return NULL. These functions
    delete unused whirl nodes and trees during simplification.

  INT32 WN_Simp_Compare_Trees(WN *t1, WN *t2)
    
     Walk the two trees and return -1 if t1 is "less than" t2, 1 if 
     t1 is "greater than" t2, and 0 if the two trees are formally identical. Note
     that commuted commutative expressions (x*y and y*x) are not considered formally
     identical for this routine. The order is computed as follows:
	1) If the root opcodes are different, the numerical value of the opcode
	   determines the order.
	2) Compare children. The result will be the result of the first non-zero
	   comparison. 
	3) Special cases:
	   a) INTCONST compares the signed value of WN_const_val.
	   b) LOAD, LOADX, LDA, MLOAD compare the offset first.
	   c) IDNAME and LDID compare offsets and then symbol table entries
	   d) CONST compares symbol table entries
	   e) ARRAY compares num_dim, element_size and then children
	   f) CVTL compares cvtl_bits before its child.
           g) Symbol table entries are compared by examining id first, then
	      index.

  WN * WN_Simplify_Tree(WN * tree)
      Run the simplifier over a tree using a bottom up walk. This routine is
      most useful if one changes a child of some node directly, and wants to 
      simplify the tree afterward.


  WN * WN_Simplify_Rebuild_Expr_Tree(WN * tree)
      Assumes that all the children of tree are already simplified, rebuild the 
      tree with simplification. This is useful to catch cases such as when an LDID of 
      a constant is replaced with a CONST node, and the tree can now be folded.

  BOOL WN_Simplifier_Enable(BOOL enable) 
      Turn on the simplifier, and return the old state of the simplifier.

   The simplifier is controlled by a few variables set in 
   config.c. Most of these are the same as the old cfold control
   variables. The variables which affect the simplifier are:

   Enable_WN_Simp: Turn on the simplifier. On by default at all optimization
                   levels. It is controlled by the switch OPT:wn_simplify.
		   Defined as SIMPNODE_enable.
  
   Enable_Cfold_Aggressive: Turn on "aggressive" optimizations. On by default at -O1.
   
   Enable_Cfold_Reassociate: Allow optimizations on floating point quantities which
                   require reassociation.

   Recip_Allowed: Allow the RECIP operator to be generated

   Rsqrt_Allowed: Allow the RSQRT operator to be generated

   Div_Split_Allowed: Allow a/b to become a*(1/b)
   

   Simp_Multiply_To_Shift: Allow conversion of multiplies by powers of 2 to shifts

   WN_Simp_Fold_ILOAD - allow ILOAD(LDA) to become LDID

   WN_Simp_Fold_LDA - allow LDA[offset] + c to become LDA[offset+c]
   


There are a couple of variables which control the interaction of the 
simplifier with other modules:

   WN_SimpParentMap - set to LNO's parent map. If not WN_MAP_UNDEFINED,
      causes the simplifier to keep parent pointers current.

   WN_SimpAlias_Manager - set to an alias manager. If set to non-null, causes
                         the simplifier to copy the alias informations on loads and stores
			 it rebuilds.


  **************************************************************************/

/** $Revision: 1.1.1.1 $
 $Date: 2005/10/21 19:00:00 $
 $Author: marcel $
 $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/wn_simp.h,v $
**/



#ifndef wn_simp_INCLUDED
#define wn_simp_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

extern BOOL WN_Simplifier_Enable(BOOL enable);

extern WN *WN_SimplifyCvtl(OPCODE opc, INT16 cvtl_bits, WN *k0);

extern WN *WN_SimplifyExp1(OPCODE opc, WN *k0);

extern WN *WN_SimplifyExp2(OPCODE opc, WN *k0, WN *k1);

extern WN *WN_SimplifyExp3(OPCODE opc, WN *k0, WN *k1, WN *k2);

extern WN *WN_SimplifyIntrinsic(OPCODE opc, UINT32 intrinsic, INT32 n, WN *k[]);

extern WN *WN_SimplifyIload(OPCODE opc, WN_OFFSET offset, 
			    TY_IDX ty, UINT field_id, TY_IDX load_addr_ty,
			    WN *addr);


extern WN *WN_SimplifyIstore(OPCODE opc, WN_OFFSET offset, 
			    TY_IDX ty, UINT field_id, WN *value, WN *addr);

extern INT32 WN_Simp_Compare_Trees(WN *t1, WN *t2);

extern OPCODE get_inverse_relop(OPCODE opc);

struct ALIAS_MANAGER;  /* Needed as a forward reference */

extern WN *WN_Simplify_Tree(WN *tree, ALIAS_MANAGER *am=NULL);

extern WN *WN_Simplify_Rebuild_Expr_Tree(WN *tree, ALIAS_MANAGER *am=NULL);

extern WN_MAP WN_SimpParentMap;

extern BOOL WN_Simp_Fold_ILOAD;

extern BOOL WN_Simp_Fold_LDA;

#ifdef KEY
extern BOOL WN_Simp_Rsqrt_Newton_Raphson;
#endif

#ifdef __cplusplus
}
#endif
#endif

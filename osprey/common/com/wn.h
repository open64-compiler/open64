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


#ifndef wn_INCLUDED
#define wn_INCLUDED

#ifndef WN_RCS_ID
#define WN_RCS_ID
#endif


#include "defs.h"
#include "opcode.h"
#include "mempool.h"
#include "srcpos.h"

#include "wn_core.h"		    /* core definitions of the tree node
				       structures */
#include "wn_map.h"
#include "wio.h"
#include "wn_pragmas.h"

#include "stab.h"
#include "wn_util.h"


/**                     Intermediate Language Tree Nodes
***                     --------------------------------
***
*** Description:
***
***     This module provides the routines to create and manipulate wns.
***
***
*** Exported functions:
***
***
***     WN *WN_Create(
***         OPERATOR	operator,
***         TYPE_ID	rtype,
***         TYPE_ID	desc,
***         mINT16	kid_count
***     )
***
***         Creates and returns a WN with the given 'opcode'.  'Kid_count'
***         give the number of kids when this is not determined by the
***         'opcode' and is ignored otherwise.  The 'opcode', 
***         'kid_count', and 'map_id' fields are initialized.  
***	    The initial values of all
***         other fields are undefined.
***
***    WN *WN_Create_Generic(
***	    OPERATOR	operator,
***	    TYPE_ID	rtype,
***	    TYPE_ID	desc,
***	    mINT16	kid_count,
***	    WN 		*next,
***	    WN		*prev,
***	    ST		*st,
***	    INT32	label_number,
***	    INT32	num_entries,
***	    TY		*ty,
***	    TY		*load_addr_ty,
***	    WN_OFFSET	offset,
***	    INT16	cvtl_bits,
***	    INT32	num_dim
***	    WN_ESIZE    element_size
***	    INT64	const_value,
***         UINT32	flag,
***         INTRINSIC	intrinsic)
***
***	     Call WN_Create and then fill in all the fields relevant for opcode
***
***
***	Higher level Create routines.  
***
***     These are routines built on top of WN_Create
***     to allow one to create different types of nodes.  They include
***     a fair amount of error checking for conformance to WHIRL rules.
***     Every control flow and statment has its own create function.
***     This is necessary as most have distinct internal fields.
***     For expressions with additional internal fields, we also supply
***     operator specific create functions, but for most expressions,
***     we provide generic (based on number of kids) create routines.  
***
***	    WN *WN_CreateBlock(void)
***	    WN *WN_CreateDO(
***                WN *index, WN *start, WN *end,
***		   WN *step, WN *body)
***	    WN *WN_CreateDoWhile(
***                WN *test, WN *body)
***	    WN *WN_CreateWhileDo(
***                WN *test, WN *body)
***	    WN *WN_CreateIf(
***                WN *test, WN *then, WN *else)
***	    WN *WN_CreateRegion(
***		   REGION_KIND kind, WN *body, WN *pragmas, WN *exits,
***		   INT region_id, struct inito * ereg_sup)
***	    WN *WN_CreateRegionExit(
***                ST *st, INT32 label_number)
***	    WN *WN_CreateGoto(
***                ST *st, INT32 label_number)
***	    WN *WN_CreateAgoto(
***                WN *addr)
***	    WN *WN_CreateAltentry(
***                ST *entry)
***	    WN *WN_CreateTruebr(
***                INT32 label_number, WN *exp)
***	    WN *WN_CreateFalsebr(
***                INT32 label_number, WN *exp)
***	    WN *WN_CreateReturn(void)
***	    WN *WN_CreateLabel(
***                ST *label, INT32 label_number, UINT32 label_flag,
***		   WN *loop_info)
***	    WN *WN_CreateCompgoto(
***                INT32 num_entries, WN *value, WN *block, WN *deflt, 
***		   INT32 last_label)
***	    WN *WN_CreateSwitch(
***                INT32 num_entries, WN *value, WN *block, WN *deflt, 
***		   INT32 last_label)
***		   if last_label for Compgoto and Switch is 0,
***		   then don't know last_label
***		   (and regions can ignore switch heirarchy).
***	    WN *WN_CreateCasegoto(
***                INT64 case_value, INT32 case_label_number)
***	    WN *WN_CreateXgoto(
***                INT32 num_entries, WN *value, WN *block, ST *st)
***	    WN *WN_CreateIstore(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
***                WN_OFFSET offset, TY_IDX ty,WN *value, WN *addr,
***		   UINT field_id)
***	    WN *WN_CreateIstorex(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
***                TY_IDX ty,WN *value, WN *addr1, WN *addr2)
***	    WN *WN_CreateMstore(
***                WN_OFFSET offset, TY_IDX ty,
***		   WN *value, WN *addr,WN *num_bytes)
***	    WN *WN_CreateStid(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
***                WN_OFFSET offset, ST *st, TY_IDX ty,WN *value, UINT field_id)
***	    WN *WN_CreatePrefetch(WN_OFFSET offset, UINT32 flag, WN *addr)
***	    WN *WN_CreatePrefetchx(UINT32 flag, WN *addr1, WN *addr2)
***	    WN *WN_CreateIo(IOSTATEMENT iostatement, mINT16 kid_count)
***	    WN *WN_CreateIoItem0(IOITEM ioitem, TY_IDX ty)
***	    WN *WN_CreateIoItem1(IOITEM ioitem, WN *kid0, TY_IDX ty)
***	    WN *WN_CreateIoItem2(IOITEM ioitem, WN *kid0, WN *kid1, TY_IDX ty)
***	    WN *WN_CreateIoItem3(IOITEM ioitem, WN *kid0, WN *kid1, WN *kid2,
***                              TY_IDX ty)
***	    WN *WN_CreateIoItemN(IOITEM ioitem, mINT16 kid_count, TY_IDX ty)
***	    WN *WN_CreateEval(
***                WN *exp)
***	    WN *WN_CreatePragma(WN_PRAGMA_ID pragma_name, ST *st, 
***	    	   INT64 const_val)
***	    WN *WN_CreateExp0(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc)
***	    WN *WN_CreateExp1(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
***                WN *kid0)
***	    WN *WN_CreateExp2(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
***                WN *kid0, WN *kid1)
***	    WN *WN_CreateExp3(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
***                WN *kid0, WN *kid1, WN *kid2)
***	    WN *WN_CreateCvtl(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
***                INT16 bits, WN *kid0)
***	    WN *WN_CreateIload(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, WN_OFFSET offset,
***		   TY_IDX ty, TY_IDX load_addr_ty,WN *addr, UINT field_id)
***	    WN *WN_CreateIloadx(
***		    OPCODE opr, TYPE_ID rtype, TYPE_ID desc, 
***		    TY_IDX ty,TY_IDX load_addr_ty,WN *addr1, WN *addr2)
***	    WN *WN_CreateMload(
***                WN_OFFSET offset, TY_IDX ty,WN *addr, 
***                WN *num_bytes)
***	    WN *WN_CreateLdid(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
***                WN_OFFSET offset, ST *st, TY_IDX ty, UINT field_id)
***	    WN *WN_CreateLda(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
***                WN_OFFSET offset, TY_IDX ty, ST *st, UINT field_id)
***	    WN *WN_CreateIlda(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
***                WN_OFFSET offset, TY_IDX ty)
***	    WN *WN_CreateIdname(
***                WN_OFFSET offset,ST *st)
***	    WN *WN_CreateConst(
***                OPERATOR opr, TYPE_ID rtype, TYPED_ID desc, ST *st )
***	    WN *WN_CreateIntconst(
***                OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, INT64 const_val)
***
***
***     void IPA_WN_Delete(
***	    WN_MAP_TAB	*maptab,
***         WN          *wn
***     )
***     void WN_Delete(
***         WN          *wn
***     )
***
***         Delete the given 'wn' and make the node storage available for
***         reallocation.  Also add the map_id to the free list in the
***	    map table.
***     
***     
***     void IPA_WN_DELETE_Tree(
***         WN_MAP_TAB  *maptab,
***         WN          *tree
***     )
***
***         Recursively delete the given whirl 'tree' and make the node
***         storage available for reallocation.  Also adds each node's
***         map_id to the free list in the appropriate map table.  Note
***         that this is the PU context-specific version of WN_DELETE_Tree
***         in wn_util.  However; wn_util has no knowledge of WN_MAP_TAB's
***         so the routine is placed here.
***     
***     
***     void WN_Register_Delete_Cleanup_Function(void (*cleanup_fn)(WN *wn))
***         Register <cleanup_fn> so that it is called just before a node
***         is deleted.  The node being deleted is passed to the cleanup
***         function.  If <cleanup_fn> was already registered, this call
***         has no effect.  Registered cleanup functions are invoked in an
***         arbitrary order.
***     
***     void WN_Remove_Delete_Cleanup_Function(void (*cleanup_fn)(WN *wn))
***         Stop calling the given <cleanup_fn> each time a node is deleted.
***
***
***     void WN_Mem_Push(void)
***         Saves the WN memory state for later restoration by WN_Mem_Pop.
***      
***      
***     void WN_Mem_Pop(void)
***         Deallocates all WN nodes created since the last call to
***         WN_Mem_Push.
***
***
***     WN *WN_CopyNode (
***	    const WN*	src_wn
***     )
***
***         Return a copy of a given src_wn.
***         It does a shallow copy, i.e. only the current node, no children.
***         Pointers to next/prev or to kids are NOT copied.
***         Furthermore, no annotations are copied.
***
***	 BOOL WN_Equiv(WN *wn1, WN *wn2)
***
***         Ignoring children and next-previous pointers, are wn1 and wn2
***         equivalent
***
***
***	void IPA_WN_Move_Maps (
***	    WN_MAP_TAB	*maptab,
***	    WN		*dst,
***	    WN		*src
***	)
***	void WN_Move_Maps (
***	    WN		*dst,
***	    WN		*src
***	)
***
***	    Move the information stored in the map table for one WN to
***	    another WN.
***
***     void IPA_WN_Move_Maps_PU(
***         WN_MAP_TAB      *src,
***         WN_MAP_TAB      *dst,
***         WN              *wn)
***
***     Move wn's mapping information from the source to the destination
***     table.  This is used when a WN is moved from one PU to another
***	This assumes the destination table will contain the same
***     WN_MAPs as the source.
***
***
***
***	These are much needed higher level routines built on the WN_Create
***	to build WN. Most of them determine the opcode based on type information
***	supplied.
***
***
***	WN *WN_Intconst (
***		TYPE_ID rtype,
***		INT64 value
***	)
***		Return an integer constant node of value
***
***	WN *WN_RotateIntconst(
***		WN	*tree
***		INT32	rotate
***	)
***		Rotate intconst bits right (rotate>0) or left (rotate<0)
***
***	WN *WN_Inverse (
***		TYPE_ID type,
***		WN	*tree
***	)
***		Return a recip or divide base on type and flags
***
***	WN *WN_Floatconst (
***		TYPE_ID rtype,
***		double value
***	)
***		Return an floating point constant node of value
***
***	WN *WN_UVConst(
***		TYPE_ID rtype,
***	)
***		Return the correct bit pattern for an uninitialized variable
***
***	WN * WN_Zerocon (
***		TYPE_ID ty
***	)
***		Return a zero of type ty 
***
***	WN *WN_Ldid (
***		TYPE_ID desc,
***		WN_OFFSET offset,
***		ST *sym,
***		TY_IDX align,
***		UINT field_id
***	)
***		Return a LDID (with offset) of type desc.
***		The rtype is computed from ST.
***		Alignment must be specified
***
***	WN *WN_RLdid (
***		TYPE_ID rtype,
***		TYPE_ID desc,
***		WN_OFFSET offset,
***		ST *sym,
***		TY_IDX align
***	)
***		Return a LDID (with offset) of type rtype, desc.
***		Alignment must be specified
***	
***	WN *WN_LdidPreg (
***		TYPE_ID desc,
***		WN_OFFSET pregno
***	)
***		Return a LDID of a given preg.
***	
***	WN *WN_Stid (
***		TYPE_ID desc,
***		WN_OFFSET offset,
***		ST *sym,
***		TY_IDX align,
***		WN *value,
***		UINT field_id
***	)
***		STID (with offset) value into sym of type desc.
***		Alignment must be specified
***	
***	WN *WN_StidIntoPreg(
***		TYPE_ID desc,
***		WN_OFFSET offset,
***		ST *sym,
***		WN *value
***	)
***		STID (with offset) value into sym of type desc.
***		Alignment is inferred from sym
***
***	WN *WN_StidPreg(
***		TYPE_ID desc,
***		WN_OFFSET offset,
***		WN *value
***	)
***		STID (with offset) value into sym of type desc.
***		Alignment is inferred from sym
***             This is a simplified interface to StidIntoPreg, where the preg sym
***             Is inferred from the type. 
***	
***	WN *WN_Iload (
***		TYPE_ID desc,
***		WN_OFFSET offset,
***		TY_IDX align,
***		WN *addr,
***		UINT field_id
***	)
***		Return a ILOAD (with offset) of type desc.
***		The rtype is computed from desc
***		Alignment must be specified
***	
***	WN *WN_RIload (
***		TYPE_ID rtype,
***		TYPE_ID desc,
***		WN_OFFSET offset,
***		TY_IDX align,
***		WN *addr
***	)
***		Return a ILOAD (with offset) of type rtype, desc.
***		Alignment must be specified
***
***	WN *WN_Istore (
***		TYPE_ID desc,
***		WN_OFFSET offset,
***		TY_IDX align,
***		WN *addr,
***		WN *value,
***		UINT field_id
***	)
***		Return a ISTORE (with offset) of type desc.
***		Alignment must be specified
***	
***	WN *WN_Unary(
***		OPERATOR opr,
***		TYPE_ID rtype,
***		WN *l
***	)
***		Return a unary operator opr with opcode of type rtype.
***		The following macros use this as a common base
***			WN_LNOT(WN *l)
***			WN_Bnot(TYPE_ID type, WN *l)
***			WN_Realpart(TYPE_ID rtype, WN *l)
***			WN_Imagpart(TYPE_ID rtype, WN *l)
***			WN_Paren(TYPE_ID rtype, WN *l)
***	
***	WN *WN_Binary (
***		OPERATOR opr,
***		TYPE_ID rtype,
***		WN *l, WN *r
***	)
***		Return a binary operator opr with opcode of type rtype.
***		The following macros use this as a common base
***			WN_Add(TYPE_ID rtype, WN *l, WN *r)
***			WN_Sub(TYPE_ID rtype, WN *l, WN *r)
***			WN_Mpy(TYPE_ID rtype, WN *l, WN *r)
***			WN_Div(TYPE_ID rtype, WN *l, WN *r)
***
***			WN_LAND(WN *l, WN *r)
***			WN_LIOR(WN *l, WN *r)
***
***			WN_Band(TYPE_ID rtype, WN *l, WN *r)
***			WN_Bior(TYPE_ID rtype, WN *l, WN *r)
***			WN_Bxor(TYPE_ID rtype, WN *l, WN *r)
***
***			WN_Lshr(TYPE_ID rtype, WN *l, WN *r)
***			WN_Ashr(TYPE_ID rtype, WN *l, WN *r)
***			WN_Shl(TYPE_ID rtype, WN *l, WN *r)
***
***			WN_Complex(TYPE_ID rtype, WN *l, WN *r)
***	
***	WN *WN_Ternary(
***		OPERATOR opr,
***		TYPE_ID rtype,
***		WN *kid0, WN *kid1, WN *kid2
***	)
***		Return a ternary operator opr with opcode of type rtype.
***		The following macros use this as a common base
***			WN_Select(TYPE_ID rtype, WN *rel, WN *true, WN *false)
***	
***	WN *WN_IloadLdid (
***		TYPE_ID desc,
***		WN_OFFSET offset,
***		TY_IDX align,
***		ST *sym,
***		WN_OFFSET symOffset
***	)
***		Return a ILOAD (with offset) of type desc with and address
***		of LDID (with symOffset) of Pointer_type.
***		Alignment must be specified
***
***	WN *WN_Cvt(
***		TYPE_ID desc,
***		TYPE_ID rtype,
***		WN *l, WN *r
***	)
***		Return a cvt operator with opcode of type desc -> rtype,
***	
***	WN *WN_Trunc(
***		TYPE_ID desc,
***		TYPE_ID rtype,
***		WN *l, WN *r
***	)
***		round to zero
***		Return a trunc operator with opcode of type desc -> rtype,
***	
***	WN *WN_Rnd(
***		TYPE_ID desc,
***		TYPE_ID rtype,
***		WN *l, WN *r
***	)
***		round to nearest int
***		Return a rnd operator with opcode of type desc -> rtype
***	
***	WN *WN_Ceil(
***		TYPE_ID desc,
***		TYPE_ID rtype,
***		WN *l, WN *r
***	)
***		round to +infinity
***		Return a ceil operator with opcode of type desc -> rtype,
***	
***	WN *WN_Floor(
***		TYPE_ID desc,
***		TYPE_ID rtype,
***		WN *l, WN *r
***	)
***		round to -infinity
***		Return a floor operator with opcode of type desc -> rtype,
***	
***	WN * WN_Int_Type_Conversion( 
***		WN *wn, 
***		TYPE_ID to_type )
***
***		Return the integer-typed wn converted to the given
***		integer to_type. (handles only int->int conversions)
***
***	WN * WN_Float_Type_Conversion( 
***		WN *wn, 
***		TYPE_ID to_type )
***
***		Return the float-typed wn converted to the given
***		float to_type. (handles only float->float conversions)
***
***	WN * WN_Type_Conversion( 
***		WN *wn, 
***		TYPE_ID to_type )
***
***		Return the int/float-typed wn converted to the given
***		int/float to_type.  (handles int->float, float->int,
***		int->int, and float->float conversions)
***
***	WN *WN_Relational(
***		OPERATOR opr,
***		TYPE_ID rtype,
***		WN *l, WN *r
***	)
***		Return a relational operator opr with opcode of type rtype.
***		The following macros use this as a common base
***			WN_EQ(TYPE_ID rtype, WN *l, WN *r)
***			WN_NE(TYPE_ID rtype, WN *l, WN *r)
***			WN_LT(TYPE_ID rtype, WN *l, WN *r)
***			WN_LE(TYPE_ID rtype, WN *l, WN *r)
***			WN_GT(TYPE_ID rtype, WN *l, WN *r)
***			WN_GE(TYPE_ID rtype, WN *l, WN *r)
***
***	WN *WN_ConstPowerOf2(
***		TYPE_ID rtype,
***		INT32 n
***	)
***		Return a constant of type rtype that is 2**n (1<<(n-1))
***
***	WN *WN_Lda(
***		TYPE_ID rtype,
***		WN_OFFSET ldaOffset,
***		ST *sym,
***		UINT field_id
***	)
***		Return  lda of type rtype of sym
***
***	WN *WN_LdaString(const char *str,
***		WN_OFFSET ldaOffset,
***		INT32 len
***	)
***		Create an LDA of type string
***
***	
***	WN *WN_Icall(
***		TYPE_ID rtype,
***		TYPE_ID desc,
***		INT32 n,
***		TY_IDX ty
***	)
***		Return an icall node with n children and a TY
***	
***	WN *WN_Call(
***		TYPE_ID rtype,
***		TYPE_ID desc,
***		INT32 n,
***		ST *sym
***	)
***		Return an call node with n children and ST sym
***		Uses generic WN_generic_call()
***	
***	WN *WN_Piccall(
***		TYPE_ID rtype,
***		TYPE_ID desc,
***		INT32 n,
***		ST *sym
***	)
***		Return an piccall node with n children and ST sym
***		Uses generic WN_generic_call()
***	
***	WN *WN_Create_Intrinsic(
***		OPERATOR opr,
***		TYPE_ID rtype,
***		TYPE_ID desc,
***             INTRINSIC intrinsic,
***		INT32 n,
***		WN *kids[]
***	)
***		Return an intrinsic node with n children
***	
***     WN *WN_CreateParm(TYPE_ID rtype,
***                WN *kid0,
***		   TY_IDX ty,
***		   UINT32 flag)
***             Return a parameter node
***
***	void WN_CopyMap(
***		WN	*dst,
***		WN_MAP	map,
***		WN	*src
***	)
***		Copy the contents of map for src to dst.
***		Uses Current_Map_Tab.
***		Does nothing if map is WN_MAP_UNDEFINED.
***	
***
***	WN *WN_Tas(
***		TYPE_ID rtype,
***		TY_IDX  ty,
***		WN	*l
***	)
***		Return an tas node with TY
***
**/

extern BOOL Types_Are_Compatible(TYPE_ID ltype, WN *wn);
extern BOOL IPO_Types_Are_Compatible(TYPE_ID ltype, TYPE_ID rtype);
extern BOOL Is_Const_Parm (WN *, INT );     /* for a call call_wn, 
					     is arg j a const parm */

#ifdef KEY
extern "C" INT32 New_Region_Id (void);
#endif // KEY

extern void Dont_Use_WN_Free_List (void);
extern MEM_POOL *WN_mem_pool_ptr;

extern WN *WN_Create(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, mINT16 kid_count);
inline WN *
WN_Create(OPCODE opcode, mINT16 kid_count) {
  return WN_Create (OPCODE_operator(opcode), OPCODE_rtype(opcode),
		    OPCODE_desc(opcode), kid_count);
}

extern WN *
WN_Create_Generic (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
		   mINT16 kid_count, WN *next, WN *prev,
		   ST_IDX st, INT32 label_number, INT32 num_entries,
		   TY_IDX ty, TY_IDX load_addr_ty, WN_OFFSET  offset,
		   INT16 cvtl_bits, INT32 num_dim, WN_ESIZE element_size,
		   INT64 const_value, UINT32 flag, INTRINSIC intrinsic);  
inline WN *
WN_Create_Generic (OPCODE opcode, mINT16 kid_count, WN *next, WN *prev,
		   ST_IDX st, INT32 label_number, INT32 num_entries,
		   TY_IDX ty, TY_IDX load_addr_ty, WN_OFFSET  offset,
		   INT16 cvtl_bits, INT32 num_dim, WN_ESIZE element_size,
		   INT64 const_value, UINT32 flag, INTRINSIC intrinsic) {
  return WN_Create_Generic (OPCODE_operator(opcode), OPCODE_rtype(opcode),
			    OPCODE_desc(opcode), kid_count, next, prev,
			    st, label_number, num_entries,
			    ty, load_addr_ty, offset, cvtl_bits, num_dim,
			    element_size, const_value, flag, intrinsic);
}

extern void IPA_WN_Delete(WN_MAP_TAB *maptab, WN *wn);

#define WN_Delete(wn) \
  IPA_WN_Delete(Current_Map_Tab, (wn))

extern void IPA_WN_DELETE_Tree(WN_MAP_TAB *maptab, WN *tree);

extern void WN_Register_Delete_Cleanup_Function(void (*cleanup_fn)(WN *wn));

extern void WN_Remove_Delete_Cleanup_Function(void (*cleanup_fn)(WN *wn));

#ifdef KEY // bug 9651
extern void WN_Reset_Num_Delete_Cleanup_Fns(void);
#endif

extern  BOOL WN_Equiv(WN *wn1, WN *wn2);

extern WN *WN_CreateBlock(void);

extern WN *WN_CreateDO(WN *index,
		       WN *start,
		       WN *end,
		       WN *step,
		       WN *body,
		       WN *loop_info);

extern WN *WN_CreateDoWhile(WN *test,
			  WN *body);

extern WN *WN_CreateWhileDo(WN *test,
			  WN *body);

extern WN *WN_CreateIf(WN *test,
		       WN *if_then,
		       WN *if_else);

extern WN *WN_CreateEntry (INT16 nkids, ST_IDX name, WN *body, WN *pragmas,
			   WN *varrefs);

extern WN *WN_CreateRegion(REGION_KIND kind,
			   WN *body, 
		    	   WN *pragmas,
			   WN *exits, 
			   INT region_id,
			   INITO_IDX ereg_supp);
extern WN *WN_CreateRegionExit (INT32 label_number);

/* set maximum region id seen so far;
 * special case 0 to mean reset region ids to 0 (done at each PU) */
extern void Set_Max_Region_Id (INT id);

extern WN *WN_CreateGoto(INT32 label_number);

extern WN *WN_CreateGotoOuterBlock (INT32 label_number, SYMTAB_IDX label_level);

extern WN *WN_CreateAgoto(WN *addr);

extern WN *WN_CreateAltentry(ST_IDX entry);

#define WN_CreateCondbr WN_CreateTruebr
extern WN *WN_CreateTruebr(INT32 label_number, WN *exp);
extern WN *WN_CreateFalsebr(INT32 label_number, WN *exp);

extern WN *WN_CreateReturn(void);
extern WN *WN_CreateReturn_Val (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, WN * val);
inline WN *
WN_CreateReturn_Val (OPCODE opc, WN * val) {
  return WN_CreateReturn_Val (OPCODE_operator (opc),
                              OPCODE_rtype (opc),
                              OPCODE_desc (opc),
                              val);
}

extern WN *WN_CreateLabel(INT32 label_number, UINT32 label_flag, WN *loop_info);

extern WN *WN_CreateCompgoto(INT32 num_entries,
		     	   WN *value, WN *block, WN *deflt, INT32 last_label);

extern WN *WN_CreateSwitch(INT32 num_entries,
			   WN *value, WN *block, WN *deflt, INT32 last_label);

extern WN *WN_CreateCasegoto(INT64 case_value, INT32 case_label_number);

extern WN *WN_CreateXgoto (INT32 num_entries, WN *value, WN *block, ST_IDX st);

extern WN *WN_CreateIstore(OPERATOR opr,
			  TYPE_ID rtype, 
			  TYPE_ID desc, 
			  WN_OFFSET offset, 
			  TY_IDX ty,
			  WN *value, 
			  WN *addr,
			  UINT field_id = 0);
inline WN *
WN_CreateIstore (OPCODE opc, WN_OFFSET offset, TY_IDX ty, WN *value, WN *addr,
		 UINT field_id = 0) {
  return WN_CreateIstore (OPCODE_operator(opc), OPCODE_rtype(opc),
                          OPCODE_desc(opc), offset, ty, value, addr, field_id);
}

extern WN *WN_CreateIstorex(OPERATOR opr, 
			  TYPE_ID rtype, 
			  TYPE_ID desc, 
		          TY_IDX ty,
			  WN *value,
			  WN *addr1,
			  WN *addr2);
inline WN *
WN_CreateIstorex(OPCODE opc, TY_IDX ty, WN *value, WN *addr1, WN *addr2) {
  return WN_CreateIstorex (OPCODE_operator(opc), OPCODE_rtype(opc),
			   OPCODE_desc(opc), ty, value, addr1, addr2);
}

extern WN *WN_CreateMstore(WN_OFFSET offset,
			   TY_IDX ty,
			   WN *value,
			   WN *addr,
			   WN *num_bytes);

extern WN *WN_CreateStid(OPERATOR opr,
			 TYPE_ID rtype, 
			 TYPE_ID desc, 
			 WN_OFFSET offset, 
			 ST *st, 
			 TY_IDX ty, 
			 WN *value,
			 UINT field_id = 0);
inline WN *
WN_CreateStid(OPCODE opc, WN_OFFSET offset, ST *st, TY_IDX ty, WN *value, UINT field_id = 0) {
  return WN_CreateStid (OPCODE_operator(opc), OPCODE_rtype(opc),
			OPCODE_desc(opc), offset, st, ty, value, field_id);
}

extern WN *WN_CreatePrefetch( WN_OFFSET offset,  UINT32 flag, WN *addr);
extern WN *WN_CreatePrefetchx(UINT32 flag, WN *addr1, WN *addr2);

extern WN *WN_CreateIo(IOSTATEMENT iostatement, mINT16 kid_count);
extern WN *WN_CreateIoItem0(IOITEM ioitem, TY_IDX ty);
extern WN *WN_CreateIoItem1(IOITEM ioitem, WN *kid0, TY_IDX ty);
extern WN *WN_CreateIoItem2(IOITEM ioitem, WN *kid0, WN *kid1, TY_IDX ty);
extern WN *WN_CreateIoItem3(IOITEM ioitem, WN *kid0, WN *kid1, WN *kid2, TY_IDX ty);
extern WN *WN_CreateIoItemN(IOITEM ioitem, mINT16 kid_count, TY_IDX ty);

extern WN *WN_CreateEval(WN *exp);

extern WN *WN_CreatePragma(WN_PRAGMA_ID pragma_name, ST_IDX st, 
			   INT32 arg1, INT32 arg2);
extern WN *WN_CreatePragma(WN_PRAGMA_ID pragma_name, ST_IDX st, 
			   INT32 arg1, PREG_NUM asm_copyout_preg,
			   UINT32 asm_opnd_num);
extern WN *WN_CreateXpragma(WN_PRAGMA_ID pragma_name, ST_IDX st, 
			    INT16 kid_count);

extern WN *WN_CreateExp0(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc);
inline WN *
WN_CreateExp0 (OPCODE opc) {
  return WN_CreateExp0 (OPCODE_operator(opc), OPCODE_rtype(opc),
                        OPCODE_desc(opc));
}

extern WN *WN_CreateExp1(OPERATOR opr,
			 TYPE_ID rtype,
			 TYPE_ID desc,
			 WN *kid0);
inline WN *
WN_CreateExp1 (OPCODE opc, WN *kid0) {
  return WN_CreateExp1 (OPCODE_operator(opc), OPCODE_rtype(opc),
                        OPCODE_desc(opc), kid0);
}

extern WN *WN_CreateExp2(OPERATOR opr,
			 TYPE_ID rtype,
			 TYPE_ID desc,
			 WN *kid0,
			 WN *kid1);
inline WN*
WN_CreateExp2 (OPCODE opc, WN *kid0, WN *kid1) {
  return WN_CreateExp2 (OPCODE_operator(opc), OPCODE_rtype(opc),
                        OPCODE_desc(opc), kid0, kid1);
}

extern WN *WN_CreateExp3(OPERATOR opr,
			 TYPE_ID rtype,
			 TYPE_ID desc,
			 WN *kid0,
			 WN *kid1,
			 WN *kid2);
inline WN *
WN_CreateExp3 (OPCODE opc, WN *kid0, WN *kid1, WN *kid2) {
  return WN_CreateExp3 (OPCODE_operator(opc), OPCODE_rtype(opc),
                        OPCODE_desc(opc), kid0, kid1, kid2);
}

extern WN *WN_CreateIload(OPERATOR opr,
			 TYPE_ID rtype,
			 TYPE_ID desc,
			 WN_OFFSET offset, 
			 TY_IDX ty,
			 TY_IDX load_addr_ty,
			 WN *addr,
			 UINT field_id = 0);
inline WN*
WN_CreateIload (OPCODE opc, WN_OFFSET offset, TY_IDX ty,
		TY_IDX load_addr_ty, WN *addr, UINT field_id = 0) {
  return WN_CreateIload (OPCODE_operator(opc), OPCODE_rtype(opc),
			 OPCODE_desc(opc), offset, ty, load_addr_ty, addr,
			 field_id);
}

extern WN *WN_CreateIloadx(OPERATOR opr, 
			 TYPE_ID rtype,
			 TYPE_ID desc,
			 TY_IDX ty,
			 TY_IDX load_addr_ty,
			 WN *addr1,
			 WN *addr2);
inline WN*
WN_CreateIloadx (OPCODE opc, TY_IDX ty, TY_IDX load_addr_ty,
		 WN *addr1, WN *addr2) {
  return WN_CreateIloadx (OPCODE_operator(opc), OPCODE_rtype(opc),
			  OPCODE_desc(opc), ty, load_addr_ty, addr1, addr2);
}

extern WN *WN_CreateMload(WN_OFFSET offset, 
			  TY_IDX ty,
			  WN *addr,
			  WN *num_bytes);

extern WN *WN_CreateLdid(OPERATOR opr,
			 TYPE_ID rtype,
			 TYPE_ID desc,
			 WN_OFFSET offset,
			 ST_IDX st,
			 TY_IDX ty,
			 UINT field_id = 0);
inline WN*
WN_CreateLdid (OPCODE opc, WN_OFFSET offset, ST_IDX st, TY_IDX ty, UINT field_id = 0) {
  return WN_CreateLdid (OPCODE_operator(opc), OPCODE_rtype(opc),
			OPCODE_desc(opc), offset, st, ty, field_id);
}

extern WN *WN_CreateLda(OPERATOR opr, 
			TYPE_ID rtype,
			TYPE_ID desc,
			WN_OFFSET offset,
			TY_IDX ty,
			ST_IDX st,
			UINT field_id = 0);
inline WN*
WN_CreateLda (OPCODE opc, WN_OFFSET offset, TY_IDX ty, ST_IDX st, 
	      UINT field_id = 0) {
  return WN_CreateLda (OPCODE_operator(opc), OPCODE_rtype(opc),
		       OPCODE_desc(opc), offset, ty, st, field_id);
}

extern WN *WN_CreateIlda(OPERATOR opr, 
			 TYPE_ID rtype,
			 TYPE_ID desc,
			 WN_OFFSET offset,
			 TY_IDX ty);

extern WN *WN_CreateLdaLabel(OPERATOR opr,
			     TYPE_ID rtype,
			     TYPE_ID desc,
			     mUINT32 label_number);

extern WN *WN_CreateIdname(WN_OFFSET offset,
			   ST_IDX st);

extern WN *WN_CreateConst (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, ST_IDX st);
inline WN*
WN_CreateConst (OPCODE opc, ST_IDX st) {
  return WN_CreateConst (OPCODE_operator(opc), OPCODE_rtype(opc),
			 OPCODE_desc(opc), st);
}

extern WN *WN_CreateIntconst(OPERATOR opr, 
			     TYPE_ID rtype,
			     TYPE_ID desc,
			     INT64 const_val);
inline WN *
WN_CreateIntconst (OPCODE opc, INT64 const_val) {
  return WN_CreateIntconst (OPCODE_operator(opc), OPCODE_rtype(opc),
			    OPCODE_desc(opc), const_val);
}

extern WN *WN_CreateCvtl(OPERATOR opr,
			 TYPE_ID rtype,
			 TYPE_ID desc,
			 INT16 cvtl_bits,
			 WN *kid0);
inline WN *
WN_CreateCvtl (OPCODE opc, INT16 cvtl_bits, WN *kid0) {
  return WN_CreateCvtl (OPCODE_operator(opc), OPCODE_rtype(opc),
			OPCODE_desc(opc), cvtl_bits, kid0);
}

extern WN *WN_Create_Intrinsic(OPERATOR opr,
			       TYPE_ID rtype,
			       TYPE_ID desc,
			       INTRINSIC intrinsic,
			       INT32 n,
			       WN *kids[]);
inline WN *
WN_Create_Intrinsic (OPCODE opc, INTRINSIC intrinsic, INT32 n, WN *kids[]) {
  return WN_Create_Intrinsic (OPCODE_operator(opc), OPCODE_rtype(opc),
                              OPCODE_desc(opc), intrinsic, n, kids);
}

extern WN *WN_CreateParm(TYPE_ID rtype,
			 WN *parm_node,
			 TY_IDX ty, 
			 UINT32 flag);

extern WN *WN_CreateComma(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, WN *block, WN *value);
inline WN *
WN_CreateComma (OPCODE opc, WN *block, WN *value) {
  return WN_CreateComma (OPCODE_operator(opc), OPCODE_rtype(opc),
			 OPCODE_desc(opc), block, value);
}

extern WN *WN_CreateRcomma(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, WN *value, WN *block);
inline WN *
WN_CreateRcomma (OPCODE opc, WN *value, WN *block) {
  return WN_CreateRcomma (OPCODE_operator(opc), OPCODE_rtype(opc),
			  OPCODE_desc(opc), value, block);
}

extern WN *WN_CreateComment (const char *s);	/* create comment node */
extern STR_IDX WN_GetComment (const WN *wn);  /* get string idx from comment node */

extern WN *WN_CreateAsm_Stmt (INT16 kid_count, char *asm_string);

extern WN *WN_CreateAsm_Input (char *constraint, UINT32 opnd_num, WN *opnd_expr);

extern WN *WN_CopyNode(const WN* src_wn);



extern void IPA_WN_Move_Maps_PU(WN_MAP_TAB *src, WN_MAP_TAB *dst, WN *wn);


extern void WN_Mem_Push(void);

extern void WN_Mem_Pop(void);

extern WN *WN_Ldid(TYPE_ID desc,
		   WN_OFFSET offset,
		   ST_IDX sym,
		   TY_IDX align,
		   UINT field_id = 0);

extern WN *WN_RLdid(TYPE_ID rtype,
		    TYPE_ID desc,
		    WN_OFFSET offset,
		    ST_IDX sym,
		    TY_IDX align);

extern WN *WN_LdidPreg(TYPE_ID desc,
		       WN_OFFSET pregno);

extern WN *WN_Iload(TYPE_ID desc,
		    WN_OFFSET offset,
		    TY_IDX align,
		    WN *addr,
		    UINT field_id = 0);

extern WN *WN_RIload(TYPE_ID rtype,
		     TYPE_ID desc,
		     WN_OFFSET offset,
		     TY_IDX align,
#ifndef KEY
		     WN *addr);
#else
                     WN *addri,
                     UINT field_id = 0);
#endif

extern WN *WN_IloadLdid(TYPE_ID desc,
			WN_OFFSET offset,
			TY_IDX align,
			ST *sym,
			WN_OFFSET symOffset);

extern WN *WN_Istore(TYPE_ID desc,
		     WN_OFFSET offset,
		     TY_IDX align,
		     WN *addr,
		     WN *value,
		     UINT field_id = 0);

extern WN *WN_Unary(OPERATOR opr,
		    TYPE_ID rtype,
		    WN *l);

extern WN *WN_Binary(OPERATOR opr,
		     TYPE_ID rtype,
		     WN *l,
		     WN *r);

extern WN *WN_Ternary(OPERATOR opr,
		      TYPE_ID rtype,
		      WN *kid0,
		      WN *kid1,
		      WN *kid2);

extern WN *WN_Stid(TYPE_ID desc,
		   WN_OFFSET offset,
		   ST *sym,
		   TY_IDX align,
		   WN *value,
		   UINT field_id = 0);

extern WN *WN_StidIntoPreg(TYPE_ID desc,
			   WN_OFFSET offset,
			   ST *sym,
			   WN *value);

#define WN_StidPreg(desc,pregno,value) WN_StidIntoPreg((desc),(pregno),MTYPE_To_PREG(desc),(value))

extern WN *WN_Binary(OPERATOR opr,
		     TYPE_ID rtype,
		     WN *l,
		     WN *r);

extern WN *WN_Intconst(TYPE_ID rtype,
		       INT64 value);

extern WN *WN_RotateIntconst(WN *tree,
			     INT32 rotate);

extern WN *WN_Inverse(TYPE_ID type,
		      WN *tree);

extern WN *WN_Floatconst(TYPE_ID type,
			 double value);

extern WN *WN_UVConst(TYPE_ID type);

extern WN * WN_Zerocon (TYPE_ID ty);

extern WN *WN_Cvt(TYPE_ID desc,
		  TYPE_ID rtype,
		  WN *kid0);

extern WN *WN_Trunc(TYPE_ID desc,
		    TYPE_ID rtype,
		    WN *kid0);

extern WN *WN_Rnd(TYPE_ID desc,
		  TYPE_ID rtype,
		  WN *kid0);

extern WN *WN_Ceil(TYPE_ID desc,
		   TYPE_ID rtype,
		   WN *kid0);

extern WN *WN_Floor(TYPE_ID desc,
		    TYPE_ID rtype,
		    WN *kid0);

extern WN *WN_Relational(OPERATOR opr,
			 TYPE_ID desc,
			 WN *kid0,
			 WN *kid1);

extern WN *WN_ConstPowerOf2(TYPE_ID rtype,
			    INT32 n);

extern WN *WN_Lda(TYPE_ID rtype,
		  WN_OFFSET ldaOffset,
		  ST *sym,
		  UINT field_id = 0);


extern WN *WN_LdaString(const char *str,
			WN_OFFSET ldaOffset,
			INT32 len);

extern WN *WN_LdaLabel(TYPE_ID rtype,
                       INT32   label_number);

extern WN *WN_Icall(TYPE_ID rtype,
		    TYPE_ID desc,
		    INT32 n,
		    TY_IDX ty);

extern WN *WN_generic_call(OPERATOR opr,
			   TYPE_ID rtype,
			   TYPE_ID desc,
			   INT32 n,
			   ST_IDX sym);

extern WN *WN_generic_intrinsic(OPERATOR opr,
				TYPE_ID rtype,
				TYPE_ID desc,
				INT32 n,
				INTRINSIC intrinsic);

#define	WN_Call(type,desc,n,s)		WN_generic_call(OPR_CALL,type,desc,n,s)
#define	WN_Piccall(type,desc,n,s)	WN_generic_call(OPR_PICCALL,type,desc,n,s)

extern WN *WN_CreateLoopInfo (WN *induction, WN *trip, UINT16 trip_est, UINT16 depth, INT32 flags);

extern WN *WN_CreateExcScopeBegin(INT32 id, INT16 nkids, struct inito* ereg_supp);

extern WN *WN_CreateExcScopeEnd(INT32 id);

extern WN *WN_CreateBarrier (BOOL forward, INT16 nkids);

extern WN *WN_CreateTrap (INT32 value);

extern WN *WN_CreateAssert (INT32 value, WN *condition);

extern void WN_CopyMap( WN	    *dst,
			WN_MAP	    map,
			const WN    *src);

extern WN *WN_Tas(TYPE_ID rtype,
		  TY_IDX ty,
		  WN *l);

extern WN *WN_Iloadx(TYPE_ID rtype,
		     TY_IDX ty,
		     TY_IDX addr_ty,
		     WN *base,
		     WN *index);

extern WN *WN_Istorex(TYPE_ID desc,
		      TY_IDX ty,
		      WN *value,
		      WN *base,
		      WN *index);

#define WN_Neg(type,l)                  WN_Unary(OPR_NEG,type,l)
#define WN_Abs(type,l)                  WN_Unary(OPR_ABS,type,l)
#define WN_Recip(type,l)                WN_Unary(OPR_RECIP,type,l)
#define WN_Sqrt(type,l)                 WN_Unary(OPR_SQRT,type,l)
#define WN_Rsqrt(type,l)                WN_Unary(OPR_RSQRT,type,l)
#define WN_Paren(type,l)                WN_Unary(OPR_PAREN,type,l)

#define WN_Select(type,rel,t,f)         WN_Ternary(OPR_SELECT,type,rel,t,f)
#define WN_Cselect(type,rel,t,f)	WN_Ternary(OPR_CSELECT,type,rel,t,f)
#define WN_Add(type,l,r)                WN_Binary(OPR_ADD,type,l,r)
#define WN_Sub(type,l,r)                WN_Binary(OPR_SUB,type,l,r)
#define WN_Mpy(type,l,r)                WN_Binary(OPR_MPY,type,l,r)
#define WN_Div(type,l,r)                WN_Binary(OPR_DIV,type,l,r)
#define WN_Madd(type,l,r,a)             WN_Ternary(OPR_MADD,type,l,r,a)
#define WN_Msub(type,l,r,a)             WN_Ternary(OPR_MSUB,type,l,r,a)
#define WN_Nmadd(type,l,r,a)            WN_Ternary(OPR_NMADD,type,l,r,a)
#define WN_Nmsub(type,l,r,a)            WN_Ternary(OPR_NMSUB,type,l,r,a)



#define WN_EQ(type,l,r)			WN_Relational(OPR_EQ,type,l,r)
#define WN_NE(type,l,r)			WN_Relational(OPR_NE,type,l,r)
#define WN_LT(type,l,r)			WN_Relational(OPR_LT,type,l,r)
#define WN_LE(type,l,r)			WN_Relational(OPR_LE,type,l,r)
#define WN_GT(type,l,r)			WN_Relational(OPR_GT,type,l,r)
#define WN_GE(type,l,r)			WN_Relational(OPR_GE,type,l,r)

#define WN_LNOT(l)                      WN_Unary(OPR_LNOT,Boolean_type,l)
#define WN_LAND(l,r)                    WN_Binary(OPR_LAND,Boolean_type,l,r)
#define WN_LIOR(l,r)                    WN_Binary(OPR_LIOR,Boolean_type,l,r)
#define WN_CAND(l,r)                    WN_Binary(OPR_CAND,Boolean_type,l,r)
#define WN_CIOR(l,r)                    WN_Binary(OPR_CIOR,Boolean_type,l,r)

#define WN_Lshr(type,l,r)               WN_Binary(OPR_LSHR,type,l,r)
#define WN_Ashr(type,l,r)               WN_Binary(OPR_ASHR,type,l,r)
#define WN_Shl(type,l,r)                WN_Binary(OPR_SHL,type,l,r)

#define WN_Bnot(type,l)                 WN_Unary(OPR_BNOT,type,l)
#define WN_Band(type,l,r)               WN_Binary(OPR_BAND,type,l,r)
#define WN_Bior(type,l,r)               WN_Binary(OPR_BIOR,type,l,r)
#define WN_Bxor(type,l,r)               WN_Binary(OPR_BXOR,type,l,r)

#define WN_Realpart(t,l)                WN_Unary(OPR_REALPART,t,l)
#define WN_Imagpart(t,l)                WN_Unary(OPR_IMAGPART,t,l)
#define WN_Complex(type,l,r)            WN_Binary(OPR_COMPLEX,type,l,r)

extern WN * WN_Int_Type_Conversion( WN *wn, TYPE_ID to_type );
extern WN * WN_Float_Type_Conversion( WN *wn, TYPE_ID to_type );
extern WN * WN_Type_Conversion( WN *wn, TYPE_ID to_type );

extern INT32 WN_Size_and_StartAddress (WN *wn, void **StartAddress);

extern BOOL WN_verifier(WN *);


/*
Return TRUE if tree rooted at pu_wn has any OPR_LABEL nodes that share
label numbers, return FALSE otherwise. tmp_pool must be initialized and not
frozen.
*/

extern BOOL WN_Tree_Has_Duplicate_Labels(
       WN *pu_wn,
       MEM_POOL *tmp_pool
);

/*
The Whirl tree copied_wn is a copy of orig_wn, created using LWN_Copy_Tree
or something similar. This routine renames all labels in copied_wn such
that all LABEL nodes get newly-created label numbers, and all other WNs
that refer to those labels are changed to refer to the new label numbers.
Finally, the return value is FALSE if any WN in the tree pu_wn references
one of the labels in orig_wn (i.e., if orig_wn contains any label that
isn't "internal"), or TRUE otherwise. Nodes in pu_wn that are also in
orig_wn are not checked for being internal (hence orig_wn is allowed to be
a subtree of pu_wn). tmp_pool must be initialized and not frozen.
*/

extern BOOL WN_Rename_Duplicate_Labels (
       WN *orig_wn,
       WN *copied_wn,
       WN *pu_wn,
       MEM_POOL *tmp_pool
);


inline WN *
WN_Create_Generic (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
		   mINT16 kid_count, WN *next, WN *prev,
		   ST *st, INT32 label_number, INT32 num_entries,
		   TY_IDX ty, TY_IDX load_addr_ty, WN_OFFSET  offset,
		   INT16 cvtl_bits, INT32 num_dim, WN_ESIZE element_size,
		   INT64 const_value, UINT32 flag, INTRINSIC intrinsic)
{
    return WN_Create_Generic (opr, rtype, desc,
			      kid_count, next, prev, ST_st_idx (st),
			      label_number, num_entries, ty, load_addr_ty,
			      offset, cvtl_bits, num_dim, element_size,
			      const_value, flag, intrinsic);
} // WN_Create_Generic

inline WN *
WN_Create_Generic (OPCODE opcode, mINT16 kid_count, WN *next, WN *prev,
		   ST *st, INT32 label_number, INT32 num_entries,
		   TY_IDX ty, TY_IDX load_addr_ty, WN_OFFSET  offset,
		   INT16 cvtl_bits, INT32 num_dim, WN_ESIZE element_size,
		   INT64 const_value, UINT32 flag, INTRINSIC intrinsic)
{
    return WN_Create_Generic (OPCODE_operator(opcode), OPCODE_rtype(opcode),
			      OPCODE_desc(opcode), kid_count, next, prev, st,
			      label_number, num_entries, ty, load_addr_ty,
			      offset, cvtl_bits, num_dim, element_size,
			      const_value, flag, intrinsic);
} // WN_Create_Generic

inline WN *
WN_CreateEntry (INT16 nkids, ST *name, WN *body, WN *pragmas, WN *varrefs)
{
    return WN_CreateEntry (nkids, ST_st_idx (name), body, pragmas, varrefs);
}

/* ARGSUSED */
inline WN *
WN_CreateRegionExit (ST *st, INT32 label_number)
{
    return WN_CreateRegionExit (label_number);
}
/* ARGSUSED */
inline WN *
WN_CreateRegionExit (ST_IDX st, INT32 label_number)
{
    return WN_CreateRegionExit (label_number);
}

/* ARGSUSED */
inline WN *
WN_CreateGoto (ST *st, INT32 label_number)
{
    return WN_CreateGoto (label_number);
}
/* ARGSUSED */
inline WN *
WN_CreateGoto (ST_IDX st, INT32 label_number)
{
    return WN_CreateGoto (label_number);
}

/*ARGSUSED */
inline WN *
WN_CreateLabel (ST_IDX st, INT32 label_number, UINT32 label_flag, WN *loop_info)
{
    return WN_CreateLabel (label_number, label_flag, loop_info);
}

inline WN *
WN_CreateAltentry (ST *entry)
{
    return WN_CreateAltentry (ST_st_idx (entry));
}

inline WN *
WN_CreateXgoto (INT32 num_entries, WN *value, WN *block, ST *st)
{
    return WN_CreateXgoto (num_entries, value, block, ST_st_idx (st));
}

inline WN *
WN_CreateStid (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
	       WN_OFFSET offset, ST_IDX st, TY_IDX ty, WN *value, 
	       UINT field_id = 0)
{
    return WN_CreateStid (opr, rtype, desc, offset, &St_Table[st], ty, value, field_id);
}

inline WN *
WN_CreateStid (OPCODE opc, WN_OFFSET offset, ST_IDX st, TY_IDX ty, WN *value,
	       UINT field_id = 0)
{
    return WN_CreateStid (OPCODE_operator(opc), OPCODE_rtype(opc),
			  OPCODE_desc(opc), offset, st, ty, value, field_id);
}

inline WN *
WN_CreatePragma (WN_PRAGMA_ID pragma_name, ST *st, INT32 arg1, INT32 arg2)
{
    return WN_CreatePragma (pragma_name, ST_st_idx (st), arg1, arg2);
}

inline WN *
WN_CreatePragma (WN_PRAGMA_ID  pragma_name,
		 ST           *st,
		 INT32         arg1,
		 PREG_NUM      asm_copyout_preg,
		 UINT32        asm_opnd_num)
{
    return WN_CreatePragma (pragma_name, ST_st_idx (st),
			    arg1, asm_copyout_preg, asm_opnd_num);
}

inline WN *
WN_CreateXpragma (WN_PRAGMA_ID pragma_name, ST *st, INT16 kid_count)
{
    return WN_CreateXpragma (pragma_name, ST_st_idx (st), kid_count);
}

inline WN *
WN_CreateLdid (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
	       WN_OFFSET offset, ST *st, TY_IDX ty, UINT field_id = 0)
{
    return WN_CreateLdid (opr, rtype, desc, offset, ST_st_idx (st), ty, field_id);
}

inline WN *
WN_CreateLdid (OPCODE opc, WN_OFFSET offset, ST *st, TY_IDX ty, UINT field_id = 0)
{
    return WN_CreateLdid (OPCODE_operator(opc), OPCODE_rtype(opc),
			  OPCODE_desc(opc), offset, st, ty, field_id);
}

inline WN *
WN_CreateLda (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
	      WN_OFFSET offset, TY_IDX ty, ST *st, UINT field_id = 0)
{
    return WN_CreateLda(opr, rtype, desc, offset, ty, ST_st_idx (st), field_id);
}

inline WN *
WN_CreateLda (OPCODE opc, WN_OFFSET offset, TY_IDX ty, ST *st)
{
    return WN_CreateLda (OPCODE_operator(opc), OPCODE_rtype(opc),
			 OPCODE_desc(opc), offset, ty, st);
}

inline WN *
WN_CreateIdname (WN_OFFSET offset, ST *st)
{
    return WN_CreateIdname (offset, ST_st_idx (st));
}

inline WN *
WN_CreateConst (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, ST *st )
{
    return WN_CreateConst (opr, rtype, desc, ST_st_idx (st));
}

inline WN *
WN_CreateConst (OPCODE opc, ST *st )
{
    return WN_CreateConst (OPCODE_operator(opc), OPCODE_rtype(opc),
			   OPCODE_desc(opc), st);
}

inline WN *
WN_Ldid (TYPE_ID desc, WN_OFFSET offset, ST *sym, TY_IDX align, UINT field_id = 0)
{
    return WN_Ldid (desc, offset, ST_st_idx (sym), align, field_id);
}

inline WN *
WN_RLdid (TYPE_ID rtype, TYPE_ID desc, WN_OFFSET offset, ST *sym, TY_IDX align)
{
    return WN_RLdid (rtype, desc, offset, ST_st_idx (sym), align);
}

inline WN *
WN_generic_call (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, INT32 n, ST *sym)
{
    return WN_generic_call (opr, rtype, desc, n, ST_st_idx (sym));
}

inline BOOL
OPERATOR_is_scalar_load (OPERATOR opr)
{
    return (opr == OPR_LDID || opr == OPR_LDBITS);
}

inline BOOL
OPERATOR_is_scalar_store (OPERATOR opr)
{
    return (opr == OPR_STID || opr == OPR_STBITS);
}

inline BOOL 
OPERATOR_is_scalar_iload (OPERATOR opr)
{
    return (opr == OPR_ILOAD || opr == OPR_ILDBITS || opr == OPR_ILOADX);
}
inline BOOL
OPERATOR_is_scalar_istore (OPERATOR opr)
{
    return (opr == OPR_ISTORE || opr == OPR_ISTBITS || opr == OPR_ISTOREX);
}

extern WN* WN_CreateAffirm (WN* condition);
extern WN* WN_CreateAlloca (WN* size);
extern WN* WN_CreateDealloca (INT32 n);
extern WN* WN_CreateLdma (TYPE_ID rtype, WN_OFFSET offset, TY_IDX ty, ST_IDX st);

extern void WN_set_st_addr_saved (WN *);

extern BOOL WN_has_side_effects (const WN*);

extern WN *WN_Rrotate (TYPE_ID desc, WN *src, WN *cnt);
extern BOOL WN_is_executable(WN *);
extern BOOL WN_is_bit_op(WN *);
extern int  WN_get_bit_from_const(WN *);
extern WN * WN_get_bit_from_expr(WN *);
extern BOOL WN_is_power_of_2(WN *);
extern WN * WN_get_bit_reduction(WN *);
extern WN * WN_find_loop_by_index(WN *, ST *, WN_MAP);
extern BOOL WN_has_const_diff(WN *, WN *, int *);
extern BOOL WN_has_compatible_iter_space(WN *, WN *, int *, int *, BOOL);
extern BOOL Identical_stmt(WN *, WN *);
extern BOOL WN_is_assign(WN *);
extern BOOL WN_is_assign_return(WN *);

extern WN *WN_CreateZDLBr(INT32 label_number);
 
#if defined(TARG_SL)
extern WN* WN_CreateFork(INT32 label_number, BOOL major);
extern BOOL WN_Intrinsic_OP_Slave(WN * wn);
#endif // TARG_SL
#endif /* wn_INCLUDED */




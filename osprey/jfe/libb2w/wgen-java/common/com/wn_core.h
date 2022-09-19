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


#ifndef wn_core_INCLUDED
#define wn_core_INCLUDED

#ifndef symtab_INCLUDED
#include "symtab.h"
#endif 

#ifndef irbdata_INCLUDED
#include "irbdata.h"
#endif

#ifndef wintrinsic_INCLUDED 
#include "wintrinsic.h"
#endif
#ifndef wio_INCLUDED
#include "wio.h"
#endif

#ifndef defs_INCLUDED
#include "defs.h"
#endif

/**                     Intermediate Language Tree Nodes
***                     --------------------------------
***
*** Description:
***
***     This is the basic data structure that all phases of the compiler
***     use to represent programs.  It is used to represent expressions,
***     leaves, statments (the side-effecting roots of expression
***     trees commonly called "stats" in other compilers), and also units
***     of hierarchical control flow such as do loops and if statements.
***
***
*** Reserved prefix:
***
***     WN              for tree nodes
***
***
*** Exported types:
***
***     WN
***
***         A tree node.  Every tree node contains the following fields:
***
***             OPCODE              opcode
***
***                 What kind of tree node is this?  All information
***                 common to nodes of a particular kind is derived from
***                 this.  For example, we could tell we were looking at a
***                 32 bit multiply by looking at its opcode.  
***		    Because multiplies are known to have two operands, 
***		    we know that this node has two operands.
***
***             mINT16     kid_count
***
***                 For every node except OPR_BLOCK, this
***		    gives the number of kids of the node.  For OPR_BLOCK,
***		    the value of this field is undefined.  This field is zero
***		    for leaf nodes. 
***
***         Depending on the type of the opcode field, one or more
***	    of the following fields is present:
***
***             For non-leaf expressions excluding OPR_BLOCK:
***
***
***                 WN              *kid(i)
***
***                     Refers to the 'i'th kid
***
***                 WN              *kid0
***
***                     Refers to the '0'th kid
***
***                 WN              *kid1
***
***                     Refers to the first kid
***
***                 WN              *kid2
***
***                     Refers to the second kid
***
***                 WN              *kid3
***
***                     Refers to the third kid
***
***             For OPR_BLOCK:
***
***                 WN    *first
***
***                     First element of the list of WNs or NULL if the
***                     list is empty
***
***                 WN    *last
***
***                     Last element of the list of WNs or NULL if the
***                     list is empty
***
***		For all statements:
***
***		    WN    *next
***			If a staement is a child of a OPR_BLOCK, this
***			points to the next statement under the OPR_BLOCK.
***			Otherwise, this is NULL.
***
***		    WN    *prev
***			If a staement is a child of a OPR_BLOCK, this
***			points to the previous statement under the OPR_BLOCK.
***			Otherwise, this is NULL.
***
***             For all loads
***
***                 WN_OFFSET              load_offset
***
***                     A constant offset.  offset can also be used (instead
***			of load_offset), but it might be less efficient.
***
***		For OPR_LDA
***
***		    WN_OFFSET		   lda_offset
***
***                     A constant offset.  offset can also be used (instead
***			of lda_offset), but it might be less efficient.
***
***             For all stores
***
***                 WN_OFFSET              store_offset
***
***                     A constant offset.  offset can also be used (instead
***			of store_offset), but it might be less efficient.
***
***		For OPR_IDNAME
***
***		    WN_OFFSET		    idname_offset
***
***                     A constant offset.  offset can also be used (instead
***			of idname_offset), but it might be less efficient.
***
***		For OPR_LABEL, OPR_TRUEBR, OPR_FALSEBR, OPR_GOTO
***
***		    INT32		    label_number
***
***		For OPR_LABEL, OPC_PREFETCH, OPC_PREFETCHX
***			OPR_INTRINSIC_CALL and OPR_INTRINSIC_OP
***
***		    UINT32		    flag		      
***
***		For OPR_INTRINSIC_CALL, OPR_INTRINSIC_OP, OPR_IO and
***			OPR_IO_ITEM
***
***		    INTRINSIC		intrinsic
***
***             For OPR_INTCONST, OPR_PRAGMA
***
***                 INT64              const_val
***
***                     A constant offset.  
***
***
***		For OPR_ILOAD, OPR_MLOAD, OPR_ILOADX, OPR_LDID, OPR_ICALL,
***			OPR_ISTORE,OPR_MSTORE,OPR_ISTOREX,OPR_STID,OPR_TAS,
***			OPR_IO_ITEM
***		    
***		    struct ty 		*WN_ty(const WN *wn)
***
***			The high level type.  This is a function
***			To set the type enter WN_set_ty(WN *wn, TY *ty)
***
***		FOR OPR_ILOAD, OPR_ILOADX
***
***		    struct ty	  	*WN_load_addr_ty
***
***		For OPR_CVTL
***
***		    INT16		cvtl_bits
***
***			The number of bits.
***
***
***		For OPR_LDID, OPR_LDA, OPR_IDNAME, OPR_STID, OPR_GOTO, 
***	            OPR_ALTENOPRY, OPR_LABEL, OPR_FUNC_ENOPRY, 
***		    OPR_CALL, OPR_CONST, OPR_PICCALL:
***
***                 ST                 st
***
***                     A symbol table index.  For the load and
***			store operators, this is the symbol being loaded or
***			stored.  For OPR_GOTO
***			this is the name of the label if the label is user 
***			defined and null otherwise.
***			For OPR_FUNC_ENTRY and OPR_CALL this is
***			the name of the function.  For OPR_CONST, this points
***			to the value of the literal.
***
***
***	        For OPR_COMP_GOTO:
***
***		    INT32	num_entries
***			
***			How many entries in the computed goto table.
***			A region created after structured control flow is 
***			lowered will also need a branch table to keep track 
***			of all the possible exits.
***
***             For OPR_ARRAY
***
***		    INT32  	num_dim
***
***			How many dimensions are in this array.
***
***		    WN_ESIZE	element_size
***
***			How big (in bytes) is each element
***
***		For OPR_FUNC_ENTRY:
***
***		    WN		*next_func, *prev_func
***
***			The next and previous function in a list
***			of functions
***
***
***
***         In addition to the above, synonyms are provided for the field
***         names of various types of tree nodes.  These provide
***         convenient and self documenting names.  Please add to this
***         list as appropriate:
***
***             For OPR_FUNC_ENTRY:
***
***                 ST              entry_name
***                     The name of the entry
***
***                 WN              *formal(i)
***                     The 'i'-th formal parameter
***
***		    INT16	    num_formals
***			The number of formal parameters
***
***		    WN		    *func_pragmas
***			List of pragmas for the function
***
***		    WN		    *func_varrefs
***			List of uplevel var refs for the function
***
***		    WN		    *func_body
***			The OPR_BLOCK that represents all the code in the body
***
***		    WN		    *entry_first,entry_last
***			The first and last statement in the OPR_BLOCK that 
***			represents all the code in the body.  This is aliased
***			to indirect through func_body
***
***             For OPR_[I]CALL:
***
***                 WN              *actual(i)
***                     The 'i'-th actual parameter
***
***		    INT16	    num_actuals
***			The number of actual parameters
***
***             For OPR_DO_LOOP:
***
***                 WN              *index
***                 WN              *start
***                 WN              *end
***                 WN              *step
***                 WN              *do_body
***
***             For OPR_DO_WHILE:
***
***                 WN              *while_test
***                 WN              *while_body
***
***             For OPR_WHILE_DO:
***
***                 WN              *while_test
***                 WN              *while_body
***
***             For OPR_IF
***
***                 WN              *if_test
***                 WN              *then
***                 WN              *else
***
***		For OPR_REGION:
***
***		    WN		    *region_body
***		    UINT32	    flag
***			Regions use the call_flag to track compilation level info
***
***		For OPR_ARRAY
***
***		    WN		    *array_index(i)
***			The i'th indexing dimension (i starts at 0)
***
***		    WN		    *array_dim(i)
***			The i'th dimension size (i starts at 0)
***
***		    WN		    *array_base
***			The base of the array.
***
***
***
***     WN_OFFSET
***
***         This is a signed integer type used in the offset fields of
***         various WNs mentioned above plus in OPC_PRAGMA.
***
***
*** Exported functions:
***
***
***     INT32 WN_Size(
***	    WN *wn
***     )
***     
*** 	    Return the size in bytes of this wn structure
***
***     void *WN_StartAddress(WN *wn)
***
***	   Return the starting address of wn
***        This is not necessarily &wn as we grow wns backwards to minimize
***        size
***
***     WN_Is_If_Guard(WN *if_wn) 
***		is the if statement a one-trip guard for a do loop
***     WN_Set_If_Guard(WN *if_wn) 
***		the if statement is a one-trip guard for a do loop
***     WN_Reset_If_Guard(WN *if_wn) 
***		the if statement is not a one-trip guard for a do loop
***
***	WN_Label_Is_Break(x)
***		is the label the label for a break from a case statement
***     WN_Set_Label_Is_Break(x)
***		the label is the label for a break from a case statement
***	WN_Reset_Label_Is_Break(x)
***		the label is not the label for a break from a case statement
***
***	WN_Is_Volatile_Mem(const WN *wn)
***	        Is <wn> a reference to a volatile memory location?
**/

#define MAX_FIELD_ID 0x3ffe
#define UNKNOWN_FIELD_ID 0x3fff

class WN;
class STMT_WN;

typedef INT32           WN_OFFSET;
typedef INT64           WN_ESIZE;
typedef INT32 		WN_MAP_ID;

typedef enum REGION_KIND {
  REGION_KIND_PRAGMA     = 0x0,
  REGION_KIND_FUNC_ENTRY = 0x1,
  REGION_KIND_LOOP	 = 0x2,
  REGION_KIND_OLIMIT	 = 0x3,
  REGION_KIND_MP	 = 0x4,
  REGION_KIND_RPI	 = 0x5,
  REGION_KIND_COLD	 = 0x6,
  REGION_KIND_SWP	 = 0x7,

  REGION_KIND_EH         = 0x8,	/* any kind with bit 3 set is EH */
  REGION_KIND_TRY        = 0x8,
  REGION_KIND_CLEANUP    = 0x9,
  REGION_KIND_EXC_SPEC   = 0xa,
  REGION_KIND_MASK       = 0xb,
  REGION_KIND_GUARD      = 0xc,
  REGION_KIND_NULL_CLEANUP = 0xd  /* cleanup with empty destructor */
} REGION_KIND;

class WN {
public:
   union {
      struct {
	 union {
	    WN_OFFSET	    load_offset; 
	    WN_OFFSET	    lda_offset; 
	    WN_OFFSET	    store_offset;
	    WN_OFFSET	    idname_offset;
	    INT32   	    num_entries; /* used by computed goto statements; may be used by regions */
	    TY_IDX	    loadx_addr_ty; /* for OPR_ILOADX */
	    INT16	    cvtl_bits;
	    INT32	    label_number;
	    UINT32	    call_flag;
	    UINT32	    if_flag;
	    UINT32	    io_flag;
	    UINT32	    asm_flag;
	    UINT32          asm_operand_num;
	    struct {
		mUINT16	    trip_est;
		mUINT16	    loop_depth;
	    } li;
            struct {
                mUINT16     pragma_flags;
                mUINT16     pragma_id;
            } pragma;
	    TY_IDX	    io_item_ty;  /* for IO_ITEM */
            struct {
                REGION_KIND region_kind: 4;
                mUINT32     region_id  :28;
            } region;
	 } ua;
	 union {
	    ST_IDX	    st_idx;	/* for ldid/stid/lda */
	    TY_IDX          ty;		/* for all types except lda,ldid,stid */
					/*  and io_item */
	    INT32  	    id;
	    INTRINSIC	    intrinsic;
	    IOSTATEMENT	    iostatement;
	    IOITEM	    ioitem;
	    UINT32	    prefetch_flag;
	    UINT32	    loop_flag;
	    INT32	    last_label;	/* end of switch */
	    INITO_IDX	    ereg_supp;
            UINT32          label_level; /* nest level for target of goto_outer_block */
	 } ub;
      } uu;
      WN_ESIZE	    element_size;
   } u1u2;

   // the following layout was used to minimize aliasing as 
   // kid_count is not used in most of the WHIRL nodes.
   // this permits loading of wn_operator, rtype and desc as bytes
   struct {
      OPERATOR          wn_operator : 8;  /* 8 bits of operator       */
      TYPE_ID           rtype       : 6;  /* result */
      mUINT32           kid_count   :14; /* gives kid_count for free */
      mINT64            map_id      :30;
      TYPE_ID           desc        : 6;  /* descriptor type */
   } common;

   union {
     struct {
       WN          *dummy1;
       TY_IDX       ty;		/* ty used for lda,ldid,stid,iload */
     } ty_fields;
     WN	           *kids[2];
     INT64	    const_val;
     struct {
       UINT32	    num_inputs;
       UINT32       num_clobbers;
     } asm_fields;
     struct {
       WN          *dummy2;
       UINT32       label_flag;
     } label_flag_fields;
     struct {
       WN          *first;
       WN          *last;
     } block;

      union {
        INT64       pragma_arg64;
        struct {
           INT32    pragma_arg1;
	   union {
	     INT32    pragma_arg2;
	     struct {
	       mUINT32  pragma_asm_opnd_num : 8;
	       PREG_NUM pragma_asm_copyout_preg : 24;
	     } asm_pragma;
	   };
        } up1;
        struct {
           mINT16   pragma_pad1;
           mINT8    pragma_distr_type;
           mINT8    pragma_index;
           INT32    pragma_preg;
        } up2;
      } pragma;
   } u3;

#ifndef WN_NO_ACCESSOR_FUNCTIONS

  WN () {}
#pragma set woff 3201
  WN (const WN& wn) {}
  WN (const WN* wn) {}
#pragma set woff 1116
  WN& operator= (const WN& wn) {}
#pragma reset woff 1116
#pragma reset woff 3201
  ~WN () {}

public:

  friend inline WN_OFFSET   WN_load_offset (const WN *);
  friend inline WN_OFFSET&  WN_load_offset (WN *);
  friend inline WN_OFFSET   WN_lda_offset (const WN *);
  friend inline WN_OFFSET&  WN_lda_offset (WN *);
  friend inline WN_OFFSET   WN_store_offset (const WN *);
  friend inline WN_OFFSET&  WN_store_offset (WN *);
  friend inline WN_OFFSET   WN_idname_offset (const WN *);
  friend inline WN_OFFSET&  WN_idname_offset (WN *);
  friend inline WN_OFFSET   WN_offset (const WN *);
  friend inline WN_OFFSET&  WN_offset (WN *);
  friend inline INT32       WN_num_entries (const WN *);
  friend inline INT32&      WN_num_entries (WN *);
//friend inline TY_IDX      WN_loadx_addr_ty (const WN *);
//friend inline TY_IDX&     WN_loadx_addr_ty (WN *);
  friend inline INT16       WN_cvtl_bits (const WN *);
  friend inline INT16&      WN_cvtl_bits (WN *);
  friend inline INT32&      WN_label_number (WN *);
  friend inline INT32       WN_label_number (const WN *);
  friend inline UINT32&     WN_call_flag (WN *);
  friend inline UINT32      WN_call_flag (const WN *);
  friend inline UINT32&     WN_if_flag (WN *);
  friend inline UINT32      WN_if_flag (const WN *);
  friend inline UINT32&     WN_io_flag (WN *);
  friend inline UINT32      WN_io_flag (const WN *);
  friend inline UINT32&     WN_asm_flag (WN *);
  friend inline UINT32      WN_asm_flag (const WN *);
  friend inline UINT32&     WN_asm_num_inputs (WN *);
  friend inline UINT32      WN_asm_num_inputs (const WN *);
  friend inline UINT32&     WN_asm_num_clobbers (WN *);
  friend inline UINT32      WN_asm_num_clobbers (const WN *);
  friend inline char *      WN_asm_string (const WN *);
  friend inline char *      WN_asm_input_constraint (const WN *);
  friend inline UINT32      WN_asm_opnd_num(const WN *);
  friend inline UINT32&     WN_label_level (WN *);
  friend inline UINT32      WN_label_level (const WN *);

  friend inline mUINT16     WN_loop_trip_est (const WN *);
  friend inline mUINT16&    WN_loop_trip_est (WN *);
  friend inline mUINT16     WN_loop_depth (const WN *);
  friend inline mUINT16&    WN_loop_depth (WN *);

  friend inline mUINT16     WN_pragma_flags (const WN *);
  friend inline mUINT16&    WN_pragma_flags (WN *);
  friend inline mUINT16     WN_pragma (const WN *);
  friend inline mUINT16&    WN_pragma (WN *);
  friend inline UINT32      WN_pragma_asm_opnd_num (const WN *);
  friend inline void        WN_set_pragma_asm_opnd_num (WN *, UINT32);

//friend inline TY_IDX      WN_io_item_ty (const WN *);
//friend inline TY_IDX&     WN_io_item_ty (WN *);

  friend inline REGION_KIND WN_region_kind (const WN *);
  friend inline void        WN_set_region_kind (WN *, REGION_KIND);
  friend inline mUINT32     WN_region_id (const WN *);
  friend inline void        WN_set_region_id (WN *, mUINT32);

  friend inline ST_IDX      WN_st_idx (const WN *);
  friend inline ST_IDX&     WN_st_idx (WN *);
//friend inline TY_IDX      WN_ty (const WN *);
//friend inline TY_IDX&     WN_ty (WN *);
  friend inline INTRINSIC   WN_intrinsic (const WN *);
  friend inline INTRINSIC&  WN_intrinsic (WN *);
  friend inline IOSTATEMENT WN_io_statement(const WN *);
  friend inline IOSTATEMENT&  WN_io_statement(WN *);
  friend inline IOITEM	    WN_io_item(const WN *);
  friend inline IOITEM&	    WN_io_item(WN *);
//friend inline INT32       WN_num_dim (const WN *);
//friend inline INT32&      WN_num_dim (WN *);
  friend inline UINT32      WN_prefetch_flag (const WN *);
  friend inline UINT32&     WN_prefetch_flag (WN *);
  friend inline UINT32      WN_loop_flag (const WN *);
  friend inline UINT32&     WN_loop_flag (WN *);
  friend inline INT32       WN_last_label (const WN *);
  friend inline INT32&      WN_last_label (WN *);
  friend inline INITO_IDX   WN_ereg_supp (const WN *);
  friend inline INITO_IDX&  WN_ereg_supp (WN *);

  friend inline WN_ESIZE    WN_element_size (const WN *);
  friend inline WN_ESIZE&   WN_element_size (WN *);

  friend inline OPERATOR    WN_operator (const WN *);
  friend inline void        WN_set_operator (WN *, OPERATOR);
  friend inline TYPE_ID     WN_rtype (const WN *);
  friend inline void        WN_set_rtype (WN *, TYPE_ID);
  friend inline INT         WN_kid_count (const WN *);
  friend inline void        WN_set_kid_count (WN *, UINT);
  friend inline UINT        WN_field_id (const WN *);
  friend inline void        WN_set_field_id (WN *, UINT);
  friend inline UINT        WN_bit_offset (const WN *);
  friend inline void        WN_set_bit_offset (WN *, UINT);
  friend inline UINT        WN_bit_size (const WN *);
  friend inline void        WN_set_bit_size (WN *, UINT);
  friend inline TYPE_ID     WN_desc (const WN *);
  friend inline void        WN_set_desc (WN *, TYPE_ID);
  friend inline INT32       WN_map_id (const WN *);

  friend inline TY_IDX      WN_ty (const WN *, const int);
  friend inline TY_IDX&     WN_ty (WN *, const int);
  friend inline WN*         WN_kid (const WN *, const int);
  friend inline WN*&        WN_kid (WN *, const int);
  friend inline WN*         WN_kid0 (const WN *);
  friend inline WN*&        WN_kid0 (WN *);
  friend inline WN*         WN_kid1 (const WN *);
  friend inline WN*&        WN_kid1 (WN *);
  friend inline WN*         WN_kid2 (const WN *);
  friend inline WN*&        WN_kid2 (WN *);
  friend inline WN*         WN_kid3 (const WN *);
  friend inline WN*&        WN_kid3 (WN *);
  friend inline INT64       WN_const_val (const WN *);
  friend inline INT64&      WN_const_val (WN *);
  friend inline UINT32      WN_label_flag (const WN *);
  friend inline UINT32&     WN_label_flag (WN *);

  friend inline WN*         WN_first (const WN *);
  friend inline WN*&        WN_first (WN *);
  friend inline WN*         WN_last (const WN *);
  friend inline WN*&        WN_last (WN *);

  friend inline INT64       WN_pragma_arg64 (const WN *);
  friend inline INT64&      WN_pragma_arg64 (WN *);
  friend inline INT32       WN_pragma_arg1 (const WN *);
  friend inline INT32&      WN_pragma_arg1 (WN *);
  friend inline INT32       WN_pragma_arg2 (const WN *);
  friend inline INT32&      WN_pragma_arg2 (WN *);
  friend inline mINT8       WN_pragma_distr_type (const WN *);
  friend inline mINT8&      WN_pragma_distr_type (WN *);
  friend inline mINT8       WN_pragma_index (const WN *);
  friend inline mINT8&      WN_pragma_index (WN *);
  friend inline INT32       WN_pragma_preg (const WN *);
  friend inline INT32&      WN_pragma_preg (WN *);

  friend inline char *      WN_pragma_asm_constraint (const WN *);
  friend inline PREG_NUM    WN_pragma_asm_copyout_preg (const WN *);
  friend inline void        WN_set_pragma_asm_copyout_preg (WN *, PREG_NUM);

  friend inline OPCODE      WN_opcode (const WN*);
  friend inline void        WN_set_opcode (WN*, OPCODE);
  friend inline TY_IDX      WN_ty (const WN*);
  friend inline void        WN_set_ty (WN*, TY_IDX);
  friend inline TY_IDX      WN_load_addr_ty (const WN *);
  friend inline void        WN_set_load_addr_ty (WN*, TY_IDX);

  friend inline void        WN_Copy_u1u2 (WN*, const WN*);
  friend inline void        WN_Copy_u3 (WN*, const WN*);

#endif /* WN_NO_ACCESSOR_FUNCTIONS */
};

#ifndef WN_NO_ACCESSOR_FUNCTIONS

inline WN_OFFSET  WN_load_offset (const WN* wn) { return wn->u1u2.uu.ua.load_offset; }
inline WN_OFFSET& WN_load_offset (WN* wn) { return wn->u1u2.uu.ua.load_offset; }
inline WN_OFFSET  WN_lda_offset (const WN* wn) { return wn->u1u2.uu.ua.lda_offset; }
inline WN_OFFSET& WN_lda_offset (WN* wn) { return wn->u1u2.uu.ua.lda_offset; }
inline WN_OFFSET  WN_store_offset (const WN* wn) { return wn->u1u2.uu.ua.store_offset; }
inline WN_OFFSET& WN_store_offset (WN* wn) { return wn->u1u2.uu.ua.store_offset; }
inline WN_OFFSET  WN_idname_offset (const WN* wn) { return wn->u1u2.uu.ua.idname_offset; }
inline WN_OFFSET& WN_idname_offset (WN* wn) { return wn->u1u2.uu.ua.idname_offset; }
inline WN_OFFSET  WN_offset (const WN* wn) { return wn->u1u2.uu.ua.idname_offset; }
inline WN_OFFSET& WN_offset (WN* wn) { return wn->u1u2.uu.ua.idname_offset; }
inline INT32  WN_num_entries (const WN* wn) { return wn->u1u2.uu.ua.num_entries; }
inline INT32& WN_num_entries (WN* wn) { return wn->u1u2.uu.ua.num_entries; }
inline INT16 WN_cvtl_bits (const WN* wn) { return wn->u1u2.uu.ua.cvtl_bits; }
inline INT16& WN_cvtl_bits (WN* wn) { return wn->u1u2.uu.ua.cvtl_bits; }
inline INT32 WN_label_number (const WN* wn) { return wn->u1u2.uu.ua.label_number; }
inline INT32& WN_label_number (WN* wn) { return wn->u1u2.uu.ua.label_number; }
inline UINT32 WN_call_flag (const WN* wn) { return wn->u1u2.uu.ua.call_flag; }
inline UINT32& WN_call_flag (WN* wn) { return wn->u1u2.uu.ua.call_flag; }
inline UINT32 WN_if_flag (const WN* wn) { return wn->u1u2.uu.ua.if_flag; }
inline UINT32& WN_if_flag (WN* wn) { return wn->u1u2.uu.ua.if_flag; }
inline UINT32 WN_io_flag (const WN* wn) { return wn->u1u2.uu.ua.io_flag; }
inline UINT32& WN_io_flag (WN* wn) { return wn->u1u2.uu.ua.io_flag; }
inline UINT32 WN_asm_flag (const WN *wn) { return wn->u1u2.uu.ua.asm_flag; }
inline UINT32& WN_asm_flag (WN *wn) { return wn->u1u2.uu.ua.asm_flag; }
inline UINT32 WN_label_level (const WN *wn) { return wn->u1u2.uu.ub.label_level; }
inline UINT32& WN_label_level (WN *wn) { return wn->u1u2.uu.ub.label_level; }
inline mUINT16 WN_loop_trip_est (const WN* wn) { return wn->u1u2.uu.ua.li.trip_est; }
inline mUINT16& WN_loop_trip_est (WN* wn) { return wn->u1u2.uu.ua.li.trip_est; }
inline mUINT16 WN_loop_depth (const WN* wn) { return wn->u1u2.uu.ua.li.loop_depth; }
inline mUINT16& WN_loop_depth (WN* wn) { return wn->u1u2.uu.ua.li.loop_depth; }
inline mUINT16 WN_pragma_flags (const WN* wn) { return wn->u1u2.uu.ua.pragma.pragma_flags; }
inline mUINT16& WN_pragma_flags (WN* wn) { return wn->u1u2.uu.ua.pragma.pragma_flags; }
inline mUINT16 WN_pragma (const WN* wn) { return wn->u1u2.uu.ua.pragma.pragma_id; }
inline mUINT16& WN_pragma (WN* wn) { return wn->u1u2.uu.ua.pragma.pragma_id; }

inline char *WN_pragma_asm_constraint(const WN *wn) { return ST_name(&St_Table[(ST_IDX) WN_pragma_arg1(wn)]); }
inline PREG_NUM WN_pragma_asm_copyout_preg(const WN *wn) { return wn->u3.pragma.up1.asm_pragma.pragma_asm_copyout_preg; }
inline void WN_set_pragma_asm_copyout_preg(WN *wn, PREG_NUM r) { wn->u3.pragma.up1.asm_pragma.pragma_asm_copyout_preg = r; }
inline UINT32 WN_pragma_asm_opnd_num(const WN *wn) { return wn->u3.pragma.up1.asm_pragma.pragma_asm_opnd_num; }
inline void WN_set_pragma_asm_opnd_num(WN *wn, UINT32 i) { wn->u3.pragma.up1.asm_pragma.pragma_asm_opnd_num = i; }

inline REGION_KIND WN_region_kind (const WN* wn) { return wn->u1u2.uu.ua.region.region_kind; }
inline void WN_set_region_kind (WN* wn, REGION_KIND k) { wn->u1u2.uu.ua.region.region_kind = k; }
inline mUINT32 WN_region_id (const WN* wn) { return wn->u1u2.uu.ua.region.region_id; }
inline void WN_set_region_id (WN* wn, mUINT32 i) { wn->u1u2.uu.ua.region.region_id = i; }

inline ST_IDX WN_st_idx (const WN* wn) { return wn->u1u2.uu.ub.st_idx; }
inline ST_IDX& WN_st_idx (WN* wn) { return wn->u1u2.uu.ub.st_idx; }
inline INTRINSIC WN_intrinsic (const WN* wn) { return wn->u1u2.uu.ub.intrinsic; }
inline INTRINSIC& WN_intrinsic (WN* wn) { return wn->u1u2.uu.ub.intrinsic; }
inline IOSTATEMENT WN_io_statement(const WN* wn) { return wn->u1u2.uu.ub.iostatement; }
inline IOSTATEMENT& WN_io_statement(WN* wn) { return wn->u1u2.uu.ub.iostatement; }
inline IOITEM WN_io_item(const WN* wn) { return wn->u1u2.uu.ub.ioitem; }
inline IOITEM & WN_io_item(WN* wn) { return wn->u1u2.uu.ub.ioitem; }
inline UINT32 WN_prefetch_flag (const WN* wn) { return wn->u1u2.uu.ub.prefetch_flag; }
inline UINT32& WN_prefetch_flag (WN* wn) { return wn->u1u2.uu.ub.prefetch_flag; }
inline UINT32 WN_loop_flag (const WN* wn) { return wn->u1u2.uu.ub.loop_flag; }
inline UINT32& WN_loop_flag (WN* wn) { return wn->u1u2.uu.ub.loop_flag; }
inline INT32 WN_last_label (const WN* wn) { return wn->u1u2.uu.ub.last_label; }
inline INT32& WN_last_label (WN* wn) { return wn->u1u2.uu.ub.last_label; }
inline INITO_IDX WN_ereg_supp (const WN* wn) { return wn->u1u2.uu.ub.ereg_supp; }
inline INITO_IDX& WN_ereg_supp (WN* wn) { return wn->u1u2.uu.ub.ereg_supp; }
inline WN_ESIZE WN_element_size (const WN* wn) { return wn->u1u2.element_size; }
inline WN_ESIZE& WN_element_size (WN* wn) { return wn->u1u2.element_size; }

inline OPERATOR   WN_operator (const WN* wn) { return wn->common.wn_operator; }
inline void       WN_set_operator (WN* wn, OPERATOR opr) { wn->common.wn_operator = opr; }
inline TYPE_ID    WN_rtype (const WN* wn) { return wn->common.rtype; }
inline void       WN_set_rtype (WN* wn, TYPE_ID ty) { wn->common.rtype = ty; }
inline INT        WN_kid_count (const WN* wn) { return OPERATOR_nkids(WN_operator(wn)) == -1 ? wn->common.kid_count : OPERATOR_nkids(WN_operator(wn)); }
inline void       WN_set_kid_count (WN* wn, UINT n) { if (OPERATOR_nkids(WN_operator(wn)) == -1) wn->common.kid_count = n; }
inline UINT32  	  WN_field_id (const WN* wn) { return wn->common.kid_count; }
inline void	  WN_set_field_id (WN* wn, UINT n) { wn->common.kid_count = (n > MAX_FIELD_ID) ? UNKNOWN_FIELD_ID : n; }
inline UINT	  WN_bit_offset (const WN* wn) { return wn->common.kid_count >> 7; }
inline UINT	  WN_bit_size (const WN* wn) { return wn->common.kid_count & 0x7f; }
inline void	  WN_set_bit_offset_size (WN* wn, UINT ofst, UINT siz) { wn->common.kid_count = ((ofst << 7) + siz); }
inline TYPE_ID    WN_desc (const WN* wn) { return wn->common.desc; }
inline void       WN_set_desc (WN* wn, TYPE_ID ty) { wn->common.desc = ty; }
inline INT32      WN_map_id (const WN* wn) { return wn->common.map_id; }
inline void       WN_set_map_id (WN* wn, INT32 m) { wn->common.map_id = m; }

inline WN* WN_kid (const WN* wn, int i) { return wn->u3.kids [i]; }
inline WN*& WN_kid (WN* wn, int i) { return wn->u3.kids [i]; }
inline WN* WN_kid0 (const WN* wn) { return wn->u3.kids [0]; }
inline WN*& WN_kid0 (WN* wn) { return wn->u3.kids [0]; }
inline WN* WN_kid1 (const WN* wn) { return wn->u3.kids [1]; }
inline WN*& WN_kid1 (WN* wn) { return wn->u3.kids [1]; }
#pragma set woff 1172
inline WN* WN_kid2 (const WN* wn) { return wn->u3.kids [2]; }
inline WN*& WN_kid2 (WN* wn) { return wn->u3.kids [2]; }
inline WN* WN_kid3 (const WN* wn) { return wn->u3.kids [3]; }
inline WN*& WN_kid3 (WN* wn) { return wn->u3.kids [3]; }
#pragma reset woff 1172
inline INT64 WN_const_val (const WN* wn) { return wn->u3.const_val; }
inline INT64& WN_const_val (WN* wn) { return wn->u3.const_val; }
inline UINT32 WN_label_flag (const WN* wn) { return wn->u3.label_flag_fields.label_flag; }
inline UINT32& WN_label_flag (WN* wn) { return wn->u3.label_flag_fields.label_flag; }
inline WN* WN_first (const WN* wn) { return wn->u3.block.first; }
inline WN*& WN_first (WN* wn) { return wn->u3.block.first; }
inline WN* WN_last (const WN* wn) { return wn->u3.block.last; }
inline WN*& WN_last (WN* wn) { return wn->u3.block.last; }
inline UINT32 WN_asm_num_inputs (const WN *wn) { return wn->u3.asm_fields.num_inputs; }
inline UINT32& WN_asm_num_inputs (WN *wn) { return wn->u3.asm_fields.num_inputs; }
inline UINT32 WN_asm_opnd_num (const WN *wn) { return wn->u1u2.uu.ua.asm_operand_num; }
inline UINT32& WN_asm_opnd_num (WN *wn) { return wn->u1u2.uu.ua.asm_operand_num; }
inline UINT32 WN_asm_num_clobbers (const WN *wn) { return wn->u3.asm_fields.num_clobbers; }
inline UINT32& WN_asm_num_clobbers (WN *wn) { return wn->u3.asm_fields.num_clobbers; }
inline INT64 WN_pragma_arg64 (const WN* wn) { return wn->u3.pragma.pragma_arg64; }
inline INT64& WN_pragma_arg64 (WN* wn) { return wn->u3.pragma.pragma_arg64; }
inline INT32 WN_pragma_arg1 (const WN* wn) { return wn->u3.pragma.up1.pragma_arg1; }
inline INT32& WN_pragma_arg1 (WN* wn) { return wn->u3.pragma.up1.pragma_arg1; }
inline INT32 WN_pragma_arg2 (const WN* wn) { return wn->u3.pragma.up1.pragma_arg2; }
inline INT32& WN_pragma_arg2 (WN* wn) { return wn->u3.pragma.up1.pragma_arg2; }
inline mINT8 WN_pragma_distr_type (const WN* wn) { return wn->u3.pragma.up2.pragma_distr_type; }
inline mINT8& WN_pragma_distr_type (WN* wn) { return wn->u3.pragma.up2.pragma_distr_type; }
inline mINT8 WN_pragma_index (const WN* wn) { return wn->u3.pragma.up2.pragma_index; }
inline mINT8& WN_pragma_index (WN* wn) { return wn->u3.pragma.up2.pragma_index; }
inline INT32 WN_pragma_preg (const WN* wn) { return wn->u3.pragma.up2.pragma_preg; }
inline INT32& WN_pragma_preg (WN* wn) { return wn->u3.pragma.up2.pragma_preg; }



inline void WN_Copy_u1u2 (WN* dst, const WN* src) { dst->u1u2 = src->u1u2; }
inline void WN_Copy_u3 (WN* dst, const WN* src) { dst->u3 = src->u3; }

#else

#define WN_load_offset(x)       ((x)->u1u2.uu.ua.load_offset)
#define WN_lda_offset(x)        ((x)->u1u2.uu.ua.lda_offset)
#define WN_store_offset(x)      ((x)->u1u2.uu.ua.store_offset)
#define WN_idname_offset(x)     ((x)->u1u2.uu.ua.idname_offset)
#define WN_offset(x)  	        ((x)->u1u2.uu.ua.idname_offset)
#define WN_num_entries(x)       ((x)->u1u2.uu.ua.num_entries)
#define WN_cvtl_bits(x)         ((x)->u1u2.uu.ua.cvtl_bits)
#define WN_label_number(x)      ((x)->u1u2.uu.ua.label_number)
#define WN_call_flag(x)         ((x)->u1u2.uu.ua.call_flag)
#define WN_if_flag(x)           ((x)->u1u2.uu.ua.if_flag)
#define WN_io_flag(x)           ((x)->u1u2.uu.ua.io_flag)
#define WN_asm_flag(x)          ((x)->u1u2.uu.ua.asm_flag)
#define WN_asm_string(x)        (ST_name(WN_st(x)))
#define WN_asm_input_constraint(x) (ST_name(WN_st(x)))
#define WN_loop_trip_est(x)     ((x)->u1u2.uu.ua.li.trip_est)
#define WN_loop_depth(x)        ((x)->u1u2.uu.ua.li.loop_depth)
#define WN_pragma_flags(x)      ((x)->u1u2.uu.ua.pragma.pragma_flags)
#define WN_pragma(x)  	        ((x)->u1u2.uu.ua.pragma.pragma_id)
#define WN_pragma_asm_constraint(x) (ST_name(&St_Table[(ST_IDX) WN_pragma_arg1(wn)]))
#define WN_pragma_asm_copyout_preg(x) ((PREG_NUM) (x)->u3.pragma.up1.asm_pragma.pragma_asm_copyout_preg)
#define WN_pragma_asm_opnd_num(x) ((x)->u3.pragma.up1.asm_pragma.pragma_asm_opnd_num)
#define WN_region_kind(x)       ((REGION_KIND)((x)->u1u2.uu.ua.region.region_kind))
#define WN_set_region_kind(x,y) ((x)->u1u2.uu.ua.region.region_kind = y)
#define WN_region_id(x)	        ((mUINT32)((x)->u1u2.uu.ua.region.region_id))
#define WN_st_idx(x)	        ((x)->u1u2.uu.ub.st_idx)
#define WN_intrinsic(x)         ((x)->u1u2.uu.ub.intrinsic)
#define WN_io_statement(x)      ((x)->u1u2.uu.ub.iostatement)
#define WN_io_item(x)           ((x)->u1u2.uu.ub.ioitem)
#define WN_prefetch_flag(x)     ((x)->u1u2.uu.ub.prefetch_flag)
#define WN_loop_flag(x)	        ((x)->u1u2.uu.ub.loop_flag)
#define WN_last_label(x)        ((x)->u1u2.uu.ub.last_label)
#define WN_ereg_supp(x)	        ((x)->u1u2.uu.ub.ereg_supp)

#define WN_asm_num_inputs(x)    ((x)->u3.asm_fields.num_inputs)
#define WN_asm_num_clobbers(x)  ((x)->u3.asm_fields.num_clobbers)

#define WN_asm_opnd_num(x)      ((x)->u1u2.uu.ua.asm_operand_num)

#define WN_element_size(x)      ((x)->u1u2.element_size)

#define WN_operator(x)          ((OPERATOR) (x)->common.wn_operator)
#define WN_set_operator(x,y)    ((x)->common.wn_operator = y)
#define WN_rtype(x)             ((TYPE_ID) (x)->common.rtype)
#define WN_set_rtype(x,y)       ((x)->common.rtype = y)
#define WN_kid_count(x)         (OPERATOR_nkids(WN_operator(x)) == -1 ? (x)->common.kid_count : OPERATOR_nkids(WN_operator(x)))
#define WN_set_kid_count(x,y)   if (OPERATOR_nkids(WN_operator(x)) == -1) (x)->common.kid_count = y; else
#define WN_field_id(x)		((x)->common.kid_count)
#define WN_set_field_id(x,y)	((x)->common.kid_count = (y > MAX_FIELD_ID) ? UNKNOWN_FIELD_ID : y)
#define WN_bit_offset(x)	((x)->common.kid_count >> 7)
#define WN_bit_size(x)		((x)->common.kid_count & 0x7f)
#define WN_set_bit_offset_size(x,y,z) ((x)->common.kid_count = ((y << 7) + z))
#define WN_desc(x)              ((TYPE_ID) (x)->common.desc)
#define WN_set_desc(x,y)        ((x)->common.desc = y)
#define WN_map_id(x)            ((x)->common.map_id)

#define WN_kid(x,i)             ((x)->u3.kids[i])
#define WN_kid0(x)              WN_kid((x),0)
#define WN_kid1(x)              WN_kid((x),1)
#define WN_kid2(x)              WN_kid((x),2)
#define WN_kid3(x)              WN_kid((x),3)
#define WN_const_val(x)         ((x)->u3.const_val)
#define WN_label_flag(x)        ((x)->u3.label_flag_fields.label_flag)
#define WN_first(x)             ((x)->u3.block.first)
#define WN_last(x)              ((x)->u3.block.last)
#define WN_pragma_arg64(x)      ((x)->u3.pragma.pragma_arg64)
#define WN_pragma_arg1(x)       ((x)->u3.pragma.up1.pragma_arg1)
#define WN_pragma_arg2(x)       ((x)->u3.pragma.up1.pragma_arg2)
#define WN_pragma_distr_type(x) ((x)->u3.pragma.up2.pragma_distr_type)
#define WN_pragma_index(x)      ((x)->u3.pragma.up2.pragma_index)
#define WN_pragma_preg(x)       ((x)->u3.pragma.up2.pragma_preg)

#define WN_Copy_u1u2(x,y)       ((x)->u1u2 = (y)->u1u2)
#define WN_Copy_u3(x,y)         ((x)->u3 = (y)->u3)

#endif /* WN_NO_ACCESSOR_FUNCTIONS */

#define WN_num_dim(x)           (WN_kid_count(x)>>1)

#define WN_asm_clobbers(x)	(WN_kid0(x))
#define WN_asm_constraints(x)	(WN_kid1(x))

inline OPCODE
WN_opcode (const WN *wn) {
   return OPCODE_make_op (wn->common.wn_operator,
			  wn->common.rtype,
			  wn->common.desc);
}

inline void
WN_set_opcode (WN *wn, OPCODE opc) {
  wn->common.wn_operator = OPCODE_operator(opc);
  wn->common.rtype       = OPCODE_rtype(opc);
  wn->common.desc        = OPCODE_desc(opc);
}

class STMT_WN {
public:
  WN      *prev;
  WN      *next;
  mUINT64 linenum;
  WN       wn;

#ifndef WN_NO_ACCESSOR_FUNCTIONS
  friend inline WN* WN_prev (const WN*);
  friend inline WN*& WN_prev (WN*);
  friend inline WN* WN_next (const WN*);
  friend inline WN*& WN_next (WN*);
  friend inline mUINT64 WN_linenum (const WN*);
  friend inline mUINT64& WN_linenum (WN*);
  friend inline void* WN_StartAddress (WN*);
  friend inline WN* WN_prev_free (const STMT_WN*);
  friend inline WN*& WN_prev_free (STMT_WN*);
  friend inline WN& WN_real_fields (STMT_WN*);
#endif /* WN_NO_ACCESSOR_FUNCTIONS */
};

inline UINTPS WN_offset_in_STMT_WN(const WN *x)
{
  return (UINTPS) &(((STMT_WN*) x)->wn) - (UINTPS) x;
}

inline STMT_WN* WN_cast_WN_to_STMT_WN(const WN *x)
{
  return (STMT_WN *) ((UINTPS) x - WN_offset_in_STMT_WN(x));
}

#ifndef WN_NO_ACCESSOR_FUNCTIONS
inline WN* WN_prev (const WN* wn) { return (WN_cast_WN_to_STMT_WN(wn)->prev); }
inline WN*& WN_prev (WN* wn) { return (WN_cast_WN_to_STMT_WN(wn)->prev); }
inline WN* WN_next (const WN* wn) { return (WN_cast_WN_to_STMT_WN(wn)->next); }
inline WN*& WN_next (WN* wn) { return (WN_cast_WN_to_STMT_WN(wn)->next); }
inline mUINT64 WN_linenum (const WN* wn) { return (WN_cast_WN_to_STMT_WN(wn)->linenum); }
inline mUINT64& WN_linenum (WN* wn) { return (WN_cast_WN_to_STMT_WN(wn)->linenum); }
inline WN* WN_prev_free (const STMT_WN* stmt_wn) { return stmt_wn->prev; }
inline WN*& WN_prev_free (STMT_WN* stmt_wn) { return stmt_wn->prev; }
inline WN& WN_real_fields (STMT_WN* stmt_wn) { return stmt_wn->wn; }
#else
#define WN_next(x)          (WN_cast_WN_to_STMT_WN(x)->next)
#define WN_next_func(x)     (WN_cast_WN_to_STMT_WN(x)->next)
#define WN_prev(x)          (WN_cast_WN_to_STMT_WN(x)->prev)
#define WN_linenum(x)       (WN_cast_WN_to_STMT_WN(x)->linenum)
/* Note: the following are only used internally for creating WNs and
   maintaining free lists; they should not be used outside of wn.c */
#define WN_real_fields(x)   ((x)->wn)
#define WN_prev_free(x)     ((x)->prev)
#endif /* WN_NO_ACCESSOR_FUNCTIONS */

#define WN_has_sym(x)           (OPERATOR_has_sym(WN_operator(x)))

/*REFERENCED*/
inline ST *
WN_st (const WN *x)
  {
    Is_True(WN_has_sym(x), ("WN_st: wn doesn't have ST field"));
    Is_True(ST_IDX_index(WN_st_idx(x)) != 0 ||
	    WN_st_idx(x) == 0,
	    ("WN_st: zero index in nonzero level disallowed"));
    return (WN_st_idx(x) != 0 ?
	    &St_Table[WN_st_idx(x)] :
	    NULL);
  }

#ifndef WN_NO_ACCESSOR_FUNCTIONS
inline char * WN_asm_string(const WN *wn) { return ST_name(WN_st(wn)); }
inline char * WN_asm_input_constraint(const WN *wn) { return ST_name(WN_st(wn)); }
#endif

#define WN_parm_flag(x)     WN_call_flag(x)
#define WN_region_is_EH(x)  (WN_region_kind(x) & REGION_KIND_EH)
#define WN_label_loop_info(x)	\
		(WN_kid_count(x)>0 ? WN_kid((x),0) : NULL)
#define WN_set_label_loop_info(x,li) (WN_kid0(x) = (li))

/* for pragma flags */

#define WN_PRAGMA_COMPILER_GENERATED 0x01
#define WN_PRAGMA_OMP                0x02

#define WN_pragma_compiler_generated(x) ((WN_pragma_flags(x))&WN_PRAGMA_COMPILER_GENERATED)
#define WN_set_pragma_compiler_generated(x) ((WN_pragma_flags(x))|=WN_PRAGMA_COMPILER_GENERATED)
#define WN_pragma_omp(x) ((WN_pragma_flags(x)) & WN_PRAGMA_OMP)
#define WN_set_pragma_omp(x) ((WN_pragma_flags(x)) |= WN_PRAGMA_OMP)
#define WN_reset_pragma_omp(x) ((WN_pragma_flags(x)) &= ~(WN_PRAGMA_OMP))

/* for FUNC_ENTRY: */
#define WN_entry_name(x)    WN_st_idx(x)
#define WN_formal(x,i)      WN_kid((x),i)
#define WN_num_formals(x)   (WN_kid_count(x)-3)
#define WN_func_pragmas(x)  WN_kid((x),WN_kid_count(x)-3)
#define WN_func_varrefs(x)  WN_kid((x),WN_kid_count(x)-2)
#define WN_func_body(x)	    WN_kid((x),WN_kid_count(x)-1)
#define WN_entry_first(x)   WN_first(WN_func_body(x))
#define WN_entry_last(x)    WN_last(WN_func_body(x))
#define WN_actual(x,i)      WN_kid((x),i)

/* for DO_LOOP: */
#define WN_index(x)         WN_kid((x),0)
#define WN_start(x)         WN_kid((x),1)
#define WN_end(x)           WN_kid((x),2)
#define WN_step(x)          WN_kid((x),3)
#define WN_do_body(x)       WN_kid((x),4)
#define WN_do_loop_info(x)  \
		(WN_kid_count(x)>5 ? WN_kid((x),5) : NULL)
#define WN_set_do_loop_info(x,li)  (WN_kid((x),5) = (li))

#define WN_while_test(x)    WN_kid((x),0)
#define WN_while_body(x)    WN_kid((x),1)

#define WN_if_test(x)       WN_kid((x),0)
#define WN_then(x)          WN_kid((x),1)
#define WN_else(x)          WN_kid((x),2)
#define WN_else_is_empty(x) (WN_first(WN_else(x)) == NULL)

#define WN_region_exits(x)  WN_kid((x),0)
#define WN_region_pragmas(x) WN_kid((x),1)
#define WN_region_body(x)   WN_kid((x),2)

#define WN_array_index(x,i) WN_kid((x),WN_num_dim(x)+i+1)
#define WN_array_dim(x,i)   WN_kid((x),i+1)
#define WN_array_base(x)    WN_kid((x),0)

/* for LOOP_INFO: */
#define WN_loop_induction(x)\
		(WN_kid_count(x)>0 ? WN_kid((x),0) : NULL)
#define WN_set_loop_induction(x,ind) (WN_kid((x),0) = (ind))
#define WN_loop_trip(x)	    \
		(WN_kid_count(x)>1 ? WN_kid((x),1) : NULL)
#define WN_set_loop_trip(x,trip) (WN_kid((x),1) = (trip))

/* for SWITCH: */
#define WN_switch_test(x)	WN_kid((x),0)
#define WN_switch_table(x)	WN_kid((x),1)
#define WN_switch_default(x)	WN_kid((x),2)

#ifndef KEY
#if defined(_LP64) && !defined(_SGI_COMPILER_VERSION)
/* workaround for g++ bug */
#else
#define max(a,b)  ((a > b) ? a : b)
#endif
#endif



/* ====================================================================
 *
 * OPERATOR WN_operator(x)
 *      Return OPERATOR associated with WN_opcode(x)
 *
 * BOOL WN_operator_is(x,y)
 *      Return true if WN_operator(x) == y
 *
 * TYPE_ID WN_rtype(x)
 * TYPE_ID WN_desc(x)
 *      Return type of WN *x
 *
 * TY *WN_type(x)
 *      Return ty of WN *x
 *
 * ST_CLASS WN_class(x)
 *      Return ST_CLASS of WN *x
 *
 * ST_SCLASS WN_sclass(x)
 *      Return ST_SCLASS of WN *x
 *
 * ====================================================================
 */

/* WN_opcode() compound */
/*
#define WN_operator(x)		(OPCODE_operator(WN_opcode(x)))
#define WN_rtype(x)		(OPCODE_rtype(WN_opcode(x)))
#define WN_desc(x)		(OPCODE_desc(WN_opcode(x)))
*/
#define WN_operator_is(x,y)	(WN_operator(x)==(y))

/* WN_st() compound */
#define WN_class(x)		(ST_class(WN_st(x)))
#define WN_sclass(x)		(ST_sclass(WN_st(x)))
#define WN_type(x)		(ST_type(WN_st(x)))
/*REFERENCED*/
inline TCON&
WN_val (const WN *x)		{ return ST_tcon_val (WN_st(x)); }
/*REFERENCED*/
inline TYPE_ID
WN_val_type (const WN *x)	{ return TCON_ty (WN_val (x)); }


#define WN_has_map_id(x)        (WN_map_id(x)!= -1)

#define WN_block_empty(x)	(WN_first(x) == NULL)
#define WN_block_nonempty(x)	(WN_first(x) != NULL)





/* types */
/*REFERENCED*/
inline TY_IDX WN_ty(const WN *wn)
{
  OPERATOR opr;
  opr = WN_operator(wn);
  if ((opr == OPR_LDA) || (opr == OPR_LDID) || (opr == OPR_STID) ||
      (opr == OPR_LDBITS) || (opr == OPR_STBITS)) {
    return(wn->u3.ty_fields.ty);
  } else if (opr == OPR_IO_ITEM) {
    return (wn->u1u2.uu.ua.io_item_ty);
  } else {
    return (wn->u1u2.uu.ub.ty);
  }
}

/*REFERENCED*/
inline void WN_set_ty(WN *wn, TY_IDX ty)
{
  OPERATOR opr;
  opr = WN_operator(wn);
  if ((opr == OPR_LDA) || (opr == OPR_LDID) || (opr == OPR_STID) ||
      (opr == OPR_LDBITS) || (opr == OPR_STBITS)) {
    wn->u3.ty_fields.ty = ty;
  } else if (opr == OPR_IO_ITEM) {
    wn->u1u2.uu.ua.io_item_ty = ty;
  } else {
    wn->u1u2.uu.ub.ty = ty;
  }
}



/*REFERENCED*/
inline TY_IDX WN_load_addr_ty(const WN *wn)
{
  OPERATOR opr;
  opr = WN_operator(wn);
  if (opr == OPR_ILOAD || opr == OPR_ILDBITS) { 
    return(wn->u3.ty_fields.ty);
  } else {
    return (wn->u1u2.uu.ua.loadx_addr_ty);
  }
}

/*REFERENCED*/
inline void WN_set_load_addr_ty(WN *wn, TY_IDX ty)
{
  OPERATOR opr;
  opr = WN_operator(wn);
  if (opr == OPR_ILOAD || opr == OPR_ILDBITS) {
    wn->u3.ty_fields.ty = ty;
  } else {
    wn->u1u2.uu.ua.loadx_addr_ty = ty;
  }
}




/* flags */
/*REFERENCED*/
inline UINT32 WN_flag(const WN *wn)
{
  OPERATOR opr;
  opr = WN_operator(wn);
  switch (opr) {
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_PICCALL:
  case OPR_INTRINSIC_CALL:
  case OPR_INTRINSIC_OP:
  case OPR_REGION:
  case OPR_PARM:
#ifdef KEY
  case OPR_PURE_CALL_OP:
#endif
    return(WN_call_flag(wn));
  case OPR_PREFETCH:
  case OPR_PREFETCHX:
    return(WN_prefetch_flag(wn));
  case OPR_IF:
    return (WN_if_flag(wn));
  case OPR_LOOP_INFO:
    return (WN_loop_flag(wn));
  case OPR_IO:
    return (WN_io_flag(wn));
  case OPR_ASM_STMT:
    return (WN_asm_flag(wn));
  default:
    return(WN_label_flag(wn));
  }
}

/*REFERENCED*/
inline void WN_set_flag(WN *wn, UINT32 flag)
{
  OPERATOR opr;
  opr = WN_operator(wn);
  switch (opr) {
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_PICCALL:
  case OPR_INTRINSIC_CALL:
  case OPR_INTRINSIC_OP:
  case OPR_REGION:
  case OPR_PARM:
#ifdef KEY
  case OPR_PURE_CALL_OP:
#endif
    WN_call_flag(wn) = flag;
    break;
  case OPR_PREFETCH:
  case OPR_PREFETCHX:
    WN_prefetch_flag(wn) = flag;
    break;
  case OPR_IF:
    WN_if_flag(wn) = flag;
    break;
  case OPR_IO:
    WN_io_flag(wn) = flag;
    break;
  case OPR_LOOP_INFO:
    WN_loop_flag(wn) = flag;
    break;
  default:
    WN_label_flag(wn) = flag;
    break;
  }
}


/* return starting address of wn */
/*REFERENCED*/
inline void *WN_StartAddress(WN *wn)
{
  if (OPCODE_has_next_prev(WN_opcode(wn))) {
    return((void *)WN_cast_WN_to_STMT_WN(wn));
  } else return((void *) wn);
}

/* return size of wn */
/*REFERENCED*/
inline INT32 WN_Size(WN *wn)
{
  INT16 extra_kids = MAX(0,WN_kid_count(wn)-2);
  if (OPCODE_has_next_prev(WN_opcode(wn))) {
    return(sizeof(WN) + (2+extra_kids)*sizeof(WN*) + sizeof(mUINT64));
  } else {
    return(sizeof(WN) + (0+extra_kids)*sizeof(WN*));
  }
}

/*REFERENCED*/
inline void WN_Set_Linenum(WN *wn, INT64 ln)
{
  Is_True(OPCODE_has_next_prev(WN_opcode(wn)),
	("can only set line numbers for statements"));
  WN_linenum(wn) = ln;
}

/*REFERENCED*/
inline INT64 WN_Get_Linenum(const WN *wn)
{
  if (OPCODE_has_next_prev(WN_opcode(wn))) {
    return WN_linenum(wn);
  } else {
    return 0;
  }
}


/*REFERENCED*/
inline BOOL WN_Is_Volatile_Mem(const WN *wn)
{
  OPCODE opc = WN_opcode(wn);
  if (OPCODE_has_1ty(opc) || OPCODE_has_2ty(opc)) {
    if (OPCODE_operator(opc) == OPR_ISTORE ||
	OPCODE_operator(opc) == OPR_MSTORE) {
      TY_IDX pointed = TY_pointed (Ty_Table[WN_ty (wn)]);
      DevAssert(pointed, ("TY_pointed of ISTORE/MSTORE type is NULL"));
      return TY_is_volatile(pointed);
    } else {
      return TY_is_volatile(WN_ty(wn)) ||
	OPCODE_has_2ty(opc) && TY_is_volatile(WN_load_addr_ty(wn));
    }
  }
  return FALSE;
}


#define WN_IF_IS_GUARD 0x1
#define WN_Is_If_Guard(x)	(WN_if_flag(x) & WN_IF_IS_GUARD)	
#define WN_Set_If_Guard(x)	(WN_if_flag(x) |= WN_IF_IS_GUARD)	
#define WN_Reset_If_Guard(x)	(WN_if_flag(x) &= ~(WN_IF_IS_GUARD))	

#define WN_IF_IS_MPVERSION 0x2
#define WN_Is_If_MpVersion(x)	 (WN_if_flag(x) & WN_IF_IS_MPVERSION)	
#define WN_Set_If_MpVersion(x)	 (WN_if_flag(x) |= WN_IF_IS_MPVERSION)	
#define WN_Reset_If_MpVersion(x) (WN_if_flag(x) &= ~(WN_IF_IS_MPVERSION))	

#define WN_LABEL_BREAK 0x1
#define WN_Label_Is_Break(x)		(WN_label_flag(x) & WN_LABEL_BREAK)
#define WN_Set_Label_Is_Break(x)	(WN_label_flag(x) |= WN_LABEL_BREAK)
#define WN_Reset_Label_Is_Break(x)	(WN_label_flag(x) &= ~(WN_LABEL_BREAK))

/* WN_loop_flag stuff */

#define WN_LOOP_INNERMOST 0x1
#define WN_LOOP_WINDDOWN_REG 0x2
#define WN_LOOP_WINDDOWN_CACHE 0x4
#define WN_LOOP_UNIMPORTANT_MISC 0x8
#define WN_LOOP_NZ_TRIP 0x10

/* Is the trip count estimate based on guessing the size of symbols */
#define WN_LOOP_SYMB_TRIP 0x20

/* Is the loop an innermost loop */
#define WN_Loop_Innermost(x)		(WN_loop_flag(x) & WN_LOOP_INNERMOST)
#define WN_Set_Loop_Innermost(x)	(WN_loop_flag(x) |= WN_LOOP_INNERMOST)
#define WN_Reset_Loop_Innermost(x)	(WN_loop_flag(x) &= ~WN_LOOP_INNERMOST)

/* does this loop arise because it is part of register winddown 
   from outer unrolling (and therefore its importance is less) */
#define WN_Loop_Winddown_Reg(x)	      (WN_loop_flag(x) & WN_LOOP_WINDDOWN_REG)
#define WN_Set_Loop_Winddown_Reg(x)   (WN_loop_flag(x) |= WN_LOOP_WINDDOWN_REG)
#define WN_Reset_Loop_Winddown_Reg(x) \
  (WN_loop_flag(x) &= ~WN_LOOP_WINDDOWN_REG)

/* does this loop arise because it is part of cache winddown 
   (and therefore its importance might be less, but probably isn't) */
#define WN_Loop_Winddown_Cache(x) \
  (WN_loop_flag(x) & WN_LOOP_WINDDOWN_CACHE)
#define WN_Set_Loop_Winddown_Cache(x) \
  (WN_loop_flag(x) |= WN_LOOP_WINDDOWN_CACHE)
#define WN_Reset_Loop_Winddown_Cache(x) \
  (WN_loop_flag(x) &= ~WN_LOOP_WINDDOWN_CACHE)

/* Is this loop deemed to take significantly less execution time than
   some other loop, and is therefore need not be optimizes as agressively
   at low optimization levels, and is that so for some reason other than
   indicated one of the other WN_Loop flags.  Currently, this is only set
   for loops that arise from distribution of the imperfect part of a loop
   nest. */
#define WN_Loop_Unimportant_Misc(x) \
  (WN_loop_flag(x) & WN_LOOP_UNIMPORTANT_MISC)
#define WN_Set_Loop_Unimportant_Misc(x) \
  (WN_loop_flag(x) |= WN_LOOP_UNIMPORTANT_MISC)
#define WN_Reset_Loop_Unimportant_Misc(x) \
  (WN_loop_flag(x) &= ~WN_LOOP_UNIMPORTANT_MISC)

#define WN_Loop_Unimportant(x) \
  (WN_loop_flag(x) & \
   (WN_LOOP_WINDDOWN_CACHE | WN_LOOP_WINDDOWN_REG | WN_LOOP_UNIMPORTANT_MISC))

#define WN_Loop_Nz_Trip(x)         (WN_loop_flag(x) & WN_LOOP_NZ_TRIP)
#define WN_Set_Loop_Nz_Trip(x)     (WN_loop_flag(x) |= WN_LOOP_NZ_TRIP)
#define WN_Reset_Loop_Nz_Trip(x)   (WN_loop_flag(x) &= ~WN_LOOP_NZ_TRIP)

#define WN_Loop_Symb_Trip(x)         (WN_loop_flag(x) & WN_LOOP_SYMB_TRIP)
#define WN_Set_Loop_Symb_Trip(x)     (WN_loop_flag(x) |= WN_LOOP_SYMB_TRIP)
#define WN_Reset_Loop_Symb_Trip(x)   (WN_loop_flag(x) &= ~WN_LOOP_SYMB_TRIP)

#define WN_LABEL_HANDLER_BEGIN 0x2
#define WN_Label_Is_Handler_Begin(x)	   (WN_label_flag(x) & \
					    WN_LABEL_HANDLER_BEGIN)
#define WN_Set_Label_Is_Handler_Begin(x)   (WN_label_flag(x) |= \
					    WN_LABEL_HANDLER_BEGIN)
#define WN_Reset_Label_Is_Handler_Begin(x) (WN_label_flag(x) &= \
					  ~(WN_LABEL_HANDLER_BEGIN))

#ifdef KEY
#define WN_LABEL_NOT_USED 0x4
#define WN_Label_Is_Not_Used(x)       (WN_label_flag(x) & \
					    WN_LABEL_NOT_USED)
#define WN_Set_Label_Is_Not_Used(x)   (WN_label_flag(x) |= \
					    WN_LABEL_NOT_USED)
#define WN_Reset_Label_Is_Not_Used(x) (WN_label_flag(x) &= \
					  ~(WN_LABEL_NOT_USED))
#endif

#define WN_Set_IO_Library(x,y)  (WN_io_flag(x) = (y))
#define WN_IO_Library(x)        ((IOLIB) WN_io_flag(x))

#define WN_PARM_BY_REFERENCE	    0x01
#define WN_PARM_BY_VALUE	    0x02
#define WN_PARM_IN		    WN_PARM_BY_VALUE
#define WN_PARM_OUT		    0x04
#define WN_PARM_DUMMY		    0x08
/* The next two WN_PARM_READ_ONLY, WN_PARM_PASSED_NOT_SAVED only for C/C++ */
#define WN_PARM_READ_ONLY           0x10 /* parm is only referenced, not
					    modified by the callee */ 
#define WN_PARM_PASSED_NOT_SAVED    0x20 /* parm is passed to callee,
					    however, the callee does not
					    save the address of the parm */
#define WN_PARM_NOT_EXPOSED_USE   0x40  /* there is no exposed use */
#define WN_PARM_IS_KILLED   0x80        /* the parameter is killed, for
					   pass by reference */

#define WN_Parm_By_Reference(x)		(WN_parm_flag(x) & WN_PARM_BY_REFERENCE)
#define WN_Set_Parm_By_Reference(x)	(WN_parm_flag(x) |= WN_PARM_BY_REFERENCE)
#define WN_Parm_By_Value(x)		(WN_parm_flag(x) & WN_PARM_BY_VALUE)
#define WN_Set_Parm_By_Value(x)		(WN_parm_flag(x) |= WN_PARM_BY_VALUE)
#define WN_Parm_In(x)			(WN_parm_flag(x) & WN_PARM_IN)
#define WN_Set_Parm_In(x)		(WN_parm_flag(x) |= WN_PARM_IN)
#define WN_Parm_Out(x)			(WN_parm_flag(x) & WN_PARM_OUT)
#define WN_Set_Parm_Out(x)		(WN_parm_flag(x) |= WN_PARM_OUT)
#define WN_Parm_Dummy(x)		(WN_parm_flag(x) & WN_PARM_DUMMY)
#define WN_Set_Parm_Dummy(x)		(WN_parm_flag(x) |= WN_PARM_DUMMY)
#define WN_Parm_Read_Only(x)		(WN_parm_flag(x) & WN_PARM_READ_ONLY)
#define WN_Set_Parm_Read_Only(x)	(WN_parm_flag(x) |= WN_PARM_READ_ONLY)
#define WN_Parm_Passed_Not_Saved(x)	(WN_parm_flag(x) & WN_PARM_PASSED_NOT_SAVED)
#define WN_Set_Parm_Passed_Not_Saved(x)	(WN_parm_flag(x) |= WN_PARM_PASSED_NOT_SAVED)
#define WN_Set_Parm_Not_Exposed_Use(x)  (WN_parm_flag(x) |= WN_PARM_NOT_EXPOSED_USE)
#define WN_Parm_Not_Exposed_Use(x)      (WN_parm_flag(x) & WN_PARM_NOT_EXPOSED_USE)
#define WN_Set_Parm_Is_Killed(x)        (WN_parm_flag(x) |= WN_PARM_IS_KILLED)
#define WN_Parm_Is_Killed(x)            (WN_parm_flag(x) & WN_PARM_IS_KILLED)

#define WN_CALL_NEVER_RETURN	0x01 /* call will never return */
#define WN_CALL_NON_DATA_MOD	0x02 /* modifies data not present in program */
#define WN_CALL_NON_PARM_MOD	0x04 /* modifies data whose address is not passed as parameter */
#define WN_CALL_PARM_MOD	0x08 /* modifies data whose address is passed as parameter */
#define WN_CALL_NON_DATA_REF	0x10 /* references data not present in program */
#define WN_CALL_NON_PARM_REF	0x20 /* references data whose address is not passed as parameter */
#define WN_CALL_PARM_REF	0x40 /* references data whose address is passed as parameter */
#define WN_CALL_INLINE		0x80 /* marked for IPA to attempt to inline */
#define WN_CALL_DONT_INLINE	0x100	/* marked for IPA to not inline */
#define WN_CALL_DOES_MEM_ALLOC	0x200	/* malloc_like function */
#define WN_CALL_DOES_MEM_FREE	0x400	/* free_like function */
#define WN_CALL_FORTRAN_POINTER_RULE 0x800 /* call obeys fortran semantics as
					      regards points-to
					      relationships */
#define WN_CALL_REPLACE_BY_JUMP 0x1000	/* replace call by jump in thunks */

     /* Some flags make promises when they're clear, and others when
      * they're set. The following macro tells us which are
      * which. Make sure to update the macro when you add a new
      * WN_CALL_* flag.
      */
#define WN_CALL_CONSERVATIVE    (WN_CALL_NON_DATA_MOD | \
				 WN_CALL_NON_PARM_MOD | \
				 WN_CALL_PARM_MOD     | \
				 WN_CALL_NON_DATA_REF | \
				 WN_CALL_NON_PARM_REF | \
				 WN_CALL_PARM_REF)

#define WN_Call_Never_Return(x)		(WN_call_flag(x) & WN_CALL_NEVER_RETURN)
#define WN_Set_Call_Never_Return(x)	(WN_call_flag(x) |= WN_CALL_NEVER_RETURN)
#define WN_Reset_Call_Never_Return(x)	(WN_call_flag(x) &= ~WN_CALL_NEVER_RETURN)
#define WN_Call_Non_Data_Mod(x)		(WN_call_flag(x) & WN_CALL_NON_DATA_MOD)
#define WN_Set_Call_Non_Data_Mod(x)	(WN_call_flag(x) |= WN_CALL_NON_DATA_MOD)
#define WN_Reset_Call_Non_Data_Mod(x)	(WN_call_flag(x) &= ~WN_CALL_NON_DATA_MOD)
#define WN_Call_Non_Data_Ref(x)		(WN_call_flag(x) & WN_CALL_NON_DATA_REF)
#define WN_Set_Call_Non_Data_Ref(x)	(WN_call_flag(x) |= WN_CALL_NON_DATA_REF)
#define WN_Reset_Call_Non_Data_Ref(x)	(WN_call_flag(x) &= ~WN_CALL_NON_DATA_REF)
#define WN_Call_Non_Parm_Mod(x)		(WN_call_flag(x) & WN_CALL_NON_PARM_MOD)
#define WN_Set_Call_Non_Parm_Mod(x)	(WN_call_flag(x) |= WN_CALL_NON_PARM_MOD)
#define WN_Reset_Call_Non_Parm_Mod(x)	(WN_call_flag(x) &= ~WN_CALL_NON_PARM_MOD)
#define WN_Call_Non_Parm_Ref(x)		(WN_call_flag(x) & WN_CALL_NON_PARM_REF)
#define WN_Set_Call_Non_Parm_Ref(x)	(WN_call_flag(x) |= WN_CALL_NON_PARM_REF)
#define WN_Reset_Call_Non_Parm_Ref(x)	(WN_call_flag(x) &= ~WN_CALL_NON_PARM_REF)
#define WN_Call_Parm_Mod(x)		(WN_call_flag(x) & WN_CALL_PARM_MOD)
#define WN_Set_Call_Parm_Mod(x)		(WN_call_flag(x) |= WN_CALL_PARM_MOD)
#define WN_Reset_Call_Parm_Mod(x)	(WN_call_flag(x) &= ~WN_CALL_PARM_MOD)
#define WN_Call_Parm_Ref(x)		(WN_call_flag(x) & WN_CALL_PARM_REF)
#define WN_Set_Call_Parm_Ref(x)		(WN_call_flag(x) |= WN_CALL_PARM_REF)
#define WN_Reset_Call_Parm_Ref(x)	(WN_call_flag(x) &= ~WN_CALL_PARM_REF)
#define WN_Call_Inline(x)		(WN_call_flag(x) & WN_CALL_INLINE)
#define WN_Set_Call_Inline(x)		(WN_call_flag(x) |= WN_CALL_INLINE)
#define WN_Reset_Call_Inline(x)		(WN_call_flag(x) &= ~WN_CALL_INLINE)
#define WN_Call_Dont_Inline(x)		(WN_call_flag(x) & WN_CALL_DONT_INLINE)
#define WN_Set_Call_Dont_Inline(x)	(WN_call_flag(x) |= WN_CALL_DONT_INLINE)
#define WN_Reset_Call_Dont_Inline(x)	(WN_call_flag(x) &= ~WN_CALL_DONT_INLINE)
#define WN_Call_Does_Mem_Alloc(x)	(WN_call_flag(x) & WN_CALL_DOES_MEM_ALLOC)
#define WN_Set_Call_Does_Mem_Alloc(x)	(WN_call_flag(x) |= WN_CALL_DOES_MEM_ALLOC)
#define WN_Reset_Call_Does_Mem_Alloc(x)	(WN_call_flag(x) &= ~WN_CALL_DOES_MEM_ALLOC)
#define WN_Call_Does_Mem_Free(x)	(WN_call_flag(x) & WN_CALL_DOES_MEM_FREE)
#define WN_Set_Call_Does_Mem_Free(x)	(WN_call_flag(x) |= WN_CALL_DOES_MEM_FREE)
#define WN_Reset_Call_Does_Mem_Free(x)	(WN_call_flag(x) &= ~WN_CALL_DOES_MEM_FREE)
#define WN_Call_Fortran_Pointer_Rule(x) (WN_call_flag(x) & WN_CALL_FORTRAN_POINTER_RULE)
#define WN_Set_Call_Fortran_Pointer_Rule(x) \
					(WN_call_flag(x) |= WN_CALL_FORTRAN_POINTER_RULE)
#define WN_Reset_Call_Fortran_Pointer_Rule(x) \
					(WN_call_flag(x) &= ~WN_CALL_FORTRAN_POINTER_RULE)
#define WN_Call_Replace_By_Jump(x)	(WN_call_flag(x) & WN_CALL_REPLACE_BY_JUMP)
#define WN_Set_Call_Replace_By_Jump(x)	(WN_call_flag(x) |= WN_CALL_REPLACE_BY_JUMP)
#define WN_Reset_Call_Replace_By_Jump(x) (WN_call_flag(x) &= ~WN_CALL_REPLACE_BY_JUMP)

#define WN_Set_Call_Default_Flags(x)	(WN_call_flag(x) |= WN_CALL_CONSERVATIVE)
#define WN_Call_No_Side_Effect(x)	((WN_call_flag(x) & (WN_CALL_NON_DATA_MOD | WN_CALL_NON_PARM_MOD | WN_CALL_PARM_MOD)) == 0)
#define WN_Call_Pure(x)			((WN_call_flag(x) & (WN_CALL_NON_DATA_MOD | WN_CALL_NON_PARM_MOD | WN_CALL_PARM_MOD | WN_CALL_NON_DATA_REF | WN_CALL_NON_PARM_REF)) == 0)

/* Macros for OPR_ASM_STMT flag access */

#define WN_ASM_VOLATILE         0x0001
#define WN_ASM_CLOBBERS_MEM     0x0002
#define WN_ASM_CLOBBERS_CC      0x0004

#define WN_Asm_Volatile(x)		(WN_asm_flag(x) & WN_ASM_VOLATILE)
#define WN_Set_Asm_Volatile(x)		(WN_asm_flag(x) |= WN_ASM_VOLATILE)
#define WN_Reset_Asm_Volatile(x)	(WN_asm_flag(x) &= ~WN_ASM_VOLATILE)
#define WN_Asm_Clobbers_Mem(x)		(WN_asm_flag(x) & WN_ASM_CLOBBERS_MEM)
#define WN_Set_Asm_Clobbers_Mem(x)	(WN_asm_flag(x) |= WN_ASM_CLOBBERS_MEM)
#define WN_Reset_Asm_Clobbers_Mem(x)	(WN_asm_flag(x) &= ~WN_ASM_CLOBBERS_MEM)
#define WN_Asm_Clobbers_Cc(x)		(WN_asm_flag(x) & WN_ASM_CLOBBERS_CC)
#define WN_Set_Asm_Clobbers_Cc(x)	(WN_asm_flag(x) |= WN_ASM_CLOBBERS_CC)
#define WN_Reset_Asm_Clobbers_Cc(x)	(WN_asm_flag(x) &= ~WN_ASM_CLOBBERS_CC)

/*
 *  Changed to inline function for compatibility.
 *  The actual number of kids (kid_count) may be much larger
 *  as dummy node are created to encapsulate variables used/modified by 
 *  the routine.
 *
 *	#define WN_num_actuals(x)   (WN_kid_count(x))
 */
/*REFERENCED*/
inline mINT16 WN_num_actuals(const WN *wn)
{
  INT32	n= WN_kid_count(wn);
  INT32	i;
  OPERATOR opr = WN_operator(wn);

  /* for indirect calls, the last kid is the address of the procedure 
   * being called. So, skip it while counting the parameters. 
   */
  if (opr == OPR_ICALL || opr == OPR_PICCALL) n--;

  for(i= n-1; i>=0; i--)
  {
    WN *kid = WN_kid(wn,i);
    OPERATOR kid_opr = WN_operator(kid);
    if ( (kid_opr != OPR_PARM) || !WN_Parm_Dummy(kid) )
    {
      return i+1;
    }
  }
  return 0;
}

/* Flag structure in a prefetch node, and macros to manipulate the flag */

/***********************************************************************
 *
 * flag structure
 *    3322 2222 2222 1111 1111 11  
 *    1098 7654 3210 9876 5432 1098 7654 3210
 *    xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx
 *                                  ^^^^^^^^^ 1st-lev stride   (8 bits,  0.. 7)
 *                        ^^^^^^^^^           2nd-lev stride   (8 bits,  8..15)
 *              ^^^^^^^^^                     unused           (8 bits, 16..23)
 *            ^                               read(0)/write(1) (1 bit,  24)
 *           ^                                1 if manual prefetch  (1 bit, 25)
 *         ^^                                 unused           (2 bits, 26..27)
 *    ^^^^                                    confidence #     (4 bits, 28..31)
 *
 ***********************************************************************/

#define PF_GET_READ(flag)       (((~(flag)) >> 24) & 0x1)
#define PF_GET_WRITE(flag)      (((flag)  >> 24) & 0x1)
#define PF_GET_STRIDE_1L(flag)  ((flag)  & 0xff)
#define PF_GET_STRIDE_2L(flag)  (((flag) >> 8)   & 0xff)
#define PF_GET_CONFIDENCE(flag) (((flag) >> 28)  & 0xf)
#define PF_GET_MANUAL(flag)     (((flag) >> 25) & 0x1)

#ifdef KEY //bug 10953
#define PF_GET_KEEP_ANYWAY(flag) (((flag) >> 26) & 0x1)
#define PF_GET_NON_TEMPORAL(flag) (((flag) >> 27) & 0x1)
#endif

#define PF_SET_READ(flag)          flag &= 0xfeffffff
#define PF_SET_WRITE(flag)         flag |= 0x01000000
#define PF_SET_STRIDE_1L(flag, x)  flag = (((flag)&0xffffff00) | ((x)&0xff))
#define PF_SET_STRIDE_2L(flag, x)  flag = (((flag)&0xffff00ff) | ((x)&0xff)<<8)
#define PF_SET_CONFIDENCE(flag, x) flag = (((flag)&0x0fffffff) | ((x)&0xf)<<28)
#define PF_SET_MANUAL(flag)        flag |= 0x02000000
#define PF_UNSET_MANUAL(flag)      flag &= 0xfdffffff

#ifdef KEY //bug 10953
#define PF_SET_KEEP_ANYWAY(flag)   flag |= 0x04000000
#define PF_UNSET_KEEP_ANYWAY(flag) flag &= 0xfbffffff
#define PF_SET_NON_TEMPORAL(flag)  flag |= 0x08000000
#define PF_UNSET_NON_TEMPORAL(flag) flag &= 0xf7ffffff
#endif


#define WN_pf_read(wn)       (((~(WN_prefetch_flag(wn))) >> 24) & 0x1)
#define WN_pf_write(wn)      (((WN_prefetch_flag(wn))  >> 24) & 0x1)
#define WN_pf_stride_1L(wn)  ((WN_prefetch_flag(wn))  & 0xff)
#define WN_pf_stride_2L(wn)  (((WN_prefetch_flag(wn)) >> 8)   & 0xff)
#define WN_pf_confidence(wn) (((WN_prefetch_flag(wn)) >> 28)  & 0xf)
#define WN_pf_manual(wn)     (((WN_prefetch_flag(wn)) >> 25) & 0x1)

#define WN_pf_set_read(wn)          WN_prefetch_flag(wn) &= 0xfeffffff
#define WN_pf_set_write(wn)         WN_prefetch_flag(wn) |= 0x01000000
#define WN_pf_set_stride_1L(wn, x)  WN_prefetch_flag(wn) = (((WN_prefetch_flag(wn))&0xffffff00) | ((x)&0xff))
#define WN_pf_set_stride_2L(wn, x)  WN_prefetch_flag(wn) = (((WN_prefetch_flag(wn))&0xffff00ff) | ((x)&0xff)<<8)
#define WN_pf_set_confidence(wn, x) WN_prefetch_flag(wn) = (((WN_prefetch_flag(wn))&0x0fffffff) | ((x)&0xf)<<28)
#define WN_pf_set_manual(wn)        WN_prefetch_flag(wn) |= 0x02000000
#define WN_pf_unset_manual(wn)      WN_prefetch_flag(wn) &= 0xfdffffff

/* end prefetch macros */


#endif /* wn_core_INCLUDED */

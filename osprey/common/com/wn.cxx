/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: wn.c  
 *
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Base Routines for tree nodes in WHIRL
 *
 * ====================================================================
 * ====================================================================
 */
#include <stdint.h>
#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <math.h>

#include "defs.h"
#include "stab.h"
#include "irbdata.h"
#include "wn.h"
#include "config_targ.h"
#include "wn_simp.h"
#include "wio.h"
#include "targ_const.h"
#include "const.h"
#include "strtab.h"
#include "config.h"
#ifdef BACK_END
#include "fb_whirl.h"
#include "tracing.h"
#endif /* BACK_END */

#include "wn_util.h"
#if !defined(FRONT_END_C) && !defined(IR_TOOLS) && defined(TARG_SL)
#include "intrn_info.h"
#endif

#include <set>
using std::set;

#ifdef KEEP_WHIRLSTATS
INT32 whirl_num_allocated=0;
INT32 whirl_bytes_allocated=0;
INT32 whirl_num_deallocated=0;
INT32 whirl_bytes_deallocated=0;

void whirlstats()
{
   fprintf(stderr,"Num allocated = %d\nbytes allocated = %d\n",whirl_num_allocated,whirl_bytes_allocated);
   fprintf(stderr,"Num deallocated = %d\nbytes deallocated = %d\n",whirl_num_deallocated,whirl_bytes_deallocated);
}
#endif

UINT32 WN::the_unique_id = 0;
BOOL IR_dump_wn_addr = FALSE;
BOOL IR_dump_wn_id = FALSE;

MEM_POOL WN_mem_pool;
MEM_POOL *WN_mem_pool_ptr = &WN_mem_pool;
BOOL WN_mem_pool_initialized = FALSE;

typedef enum {
  UNKNOWN_TYPE, INT_TYPE, UINT_TYPE, FLOAT_TYPE, COMPLEX_TYPE
} BTYPE;

static struct winfo {
  BTYPE base_type:8;
  BTYPE comp_type:8;
  INT   size:16;
} WINFO [MTYPE_LAST + 1] = {
  UNKNOWN_TYPE,  UNKNOWN_TYPE,  0,  /* UNKNOWN  */
  UNKNOWN_TYPE,  UNKNOWN_TYPE,  0,  /* MTYPE_B  */
  INT_TYPE,      UINT_TYPE,     1,  /* MTYPE_I1 */
  INT_TYPE,      UINT_TYPE,     2,  /* MTYPE_I2 */
  INT_TYPE,      UINT_TYPE,     4,  /* MTYPE_I4 */
  INT_TYPE,      UINT_TYPE,     8,  /* MTYPE_I8 */
  UINT_TYPE,     INT_TYPE,      1,  /* MTYPE_U1 */
  UINT_TYPE,     INT_TYPE,      2,  /* MTYPE_U2 */
  UINT_TYPE,     INT_TYPE,      4,  /* MTYPE_U4 */
  UINT_TYPE,     INT_TYPE,      8,  /* MTYPE_U8 */
  FLOAT_TYPE,    FLOAT_TYPE,    4,  /* MTYPE_F4 */
  FLOAT_TYPE,    FLOAT_TYPE,    8,  /* MTYPE_F8 */
#if defined(TARG_IA64) || defined(TARG_X8664)
  FLOAT_TYPE,    FLOAT_TYPE,   16,  /* MTYPE_F10*/
  FLOAT_TYPE,    FLOAT_TYPE,   16,  /* MTYPE_F16*/
#else
  UNKNOWN_TYPE,  UNKNOWN_TYPE,  0,  /* MTYPE_F10*/
  UNKNOWN_TYPE,  UNKNOWN_TYPE,  0,  /* MTYPE_F16*/
#endif
  UNKNOWN_TYPE,  UNKNOWN_TYPE,  0,  /* MTYPE_STR*/
  FLOAT_TYPE,    FLOAT_TYPE,   16,  /* MTYPE_FQ  */
  UNKNOWN_TYPE,  UNKNOWN_TYPE,  0,  /* MTYPE_M  */
  COMPLEX_TYPE,  COMPLEX_TYPE,  8,  /* MTYPE_C4 */
  COMPLEX_TYPE,  COMPLEX_TYPE, 16,  /* MTYPE_C8 */
  COMPLEX_TYPE,  COMPLEX_TYPE, 32,  /* MTYPE_CQ */
  UNKNOWN_TYPE,  UNKNOWN_TYPE,  0,  /* MTYPE_V  */
  INT_TYPE,  	 UINT_TYPE,  	0,  /* MTYPE_BS */
  UINT_TYPE,     INT_TYPE,      4,  /* MTYPE_A4 */
  UINT_TYPE,     INT_TYPE,      8,  /* MTYPE_A8 */
  COMPLEX_TYPE,  COMPLEX_TYPE, 32,  /* MTYPE_C10 */
#if defined(TARG_IA64) || defined(TARG_X8664)
  COMPLEX_TYPE,  COMPLEX_TYPE, 32,  /* MTYPE_C16 */
#else
  UNKNOWN_TYPE,  UNKNOWN_TYPE,  0,  /* MTYPE_C16*/
#endif
};

#define WTYPE_base_type(w) WINFO[w].base_type
#define WTYPE_comp_type(w) WINFO[w].comp_type
#define WTYPE_size(w)      WINFO[w].size

#if (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77)

BOOL FE_Address_Opt = TRUE;
BOOL FE_Store_Opt = TRUE;
BOOL FE_Cvtl_Opt = TRUE;

extern "C" { WN * WN_COPY_Tree ( WN * ); }

static UINT64 masks [] = { 0, 0xff, 0xffff, 0, 0xffffffffULL,
                           0, 0, 0, 0xffffffffffffffffULL };

static WN *
fe_combine_address_offset ( WN_OFFSET * offset, WN * addr )
{
  WN    * new_addr;
  INT64   const_val;
  INT32   i;

  if ( FE_Address_Opt == FALSE )
    return addr;

  i = -1;
  new_addr = addr;

  if (    WN_operator(addr) == OPR_ADD
       || WN_operator(addr) == OPR_SUB ) {

    if ( WN_operator(WN_kid0(addr)) == OPR_INTCONST )
      i = 0;

    else
    if ( WN_operator(WN_kid1(addr)) == OPR_INTCONST )
      i = 1;
  }

  if ( i >= 0 ) {

    if ( WN_operator(addr) == OPR_ADD )
      const_val = *offset + WN_const_val(WN_kid(addr,i));

    else
      const_val = *offset - WN_const_val(WN_kid(addr,i));

    if ( const_val >= INT32_MIN && const_val <= INT32_MAX ) {

      *offset = const_val;
      new_addr = WN_COPY_Tree ( WN_kid(addr,1-i) );
    }
  }

  return new_addr;
} /* fe_combine_address_offset */
#endif /* (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77) */

BOOL
Types_Are_Compatible ( TYPE_ID ltype, WN * wn )
{
  TYPE_ID  rtype = WN_rtype(wn);
  return (    ( WTYPE_base_type(ltype) == WTYPE_base_type(rtype) )
	   || ( WTYPE_base_type(ltype) == WTYPE_comp_type(rtype) ) );
} /* Types_Are_Compatible */


/*-----------------------------------------------------------------------*/
BOOL
IPO_Types_Are_Compatible ( TYPE_ID ltype, TYPE_ID rtype )
{
    BOOL   compatible;


    /* if the base types are the same or the base type of the stid  */
    /* is of comparable type and size of lhs is  */
    /* greater than size of rhs then return true */
#ifdef KEY
    BOOL type_compatible = ((WTYPE_base_type(ltype) == WTYPE_base_type(rtype))
		   || (WTYPE_base_type(ltype) == WTYPE_comp_type(rtype)));
    BOOL size_compatible = WTYPE_size(ltype) >= WTYPE_size(rtype);

    compatible = type_compatible && size_compatible;

    if (!compatible && type_compatible)
    {
      // incompatible size
      // If RHS is a 32-bit int, allow conversion to smaller-sized LHS.
      if (rtype == MTYPE_U4 || rtype == MTYPE_I4)
        compatible = TRUE;
    }
#else
    compatible = (((WTYPE_base_type(ltype) == WTYPE_base_type(rtype))
		   || (WTYPE_base_type(ltype) == WTYPE_comp_type(rtype)))
		  && (WTYPE_size(ltype) >= WTYPE_size(rtype)));
#endif // KEY

    return (compatible);
} /* IPO_Types_Are_Compatible */

/* ---------------------------------------------------------------------- */
/* Is_Const_Parm: returns true iff for call_wn and a childIndex i,
   the i-th formal  to the function is a const parameter 
   eg f(const int &x), or f(const int *x)
*/
/* ---------------------------------------------------------------------- */
/* doesn't seem like this is used anymore */



/* ---------------------------------------------------------------------
 *			    WN Free Lists
 *
 * Each WN_FREE_LIST is a list of WHIRL nodes of a certain size that have
 * been freed by WN_Delete.  Note that the node address stored is the
 * WN_StartAddress (i.e., the address of the allocated object, which may
 * not be the same as the WN * returned by the creator).
 *
 * TODO: Since I have very little time to implement this, the current
 * implementation keeps free lists only for nodes of the two most common
 * sizes.  We should generalize this to other sizes later.
 * ---------------------------------------------------------------------
 */

/* Use a field within the WN to link to next free WN.
 * This lets us use WN as the representation of WN_FREE_LIST.
 */
typedef WN * WN_FREE_LIST;

/* ---------------------------------------------------------------------
 * BOOL WN_FREE_LIST_Empty(WN_FREE_LIST *list)
 *
 * Is <list> empty?
 * ---------------------------------------------------------------------
 */
#define WN_FREE_LIST_Empty(list) (*(list) == NULL)

/*
 * provide a way to turn off free list
 */
static BOOL use_free_list = TRUE;

void
Dont_Use_WN_Free_List (void)
{
    use_free_list = FALSE;
}

/* ---------------------------------------------------------------------
 * void WN_FREE_LIST_Push(WN_FREE_LIST *list, WN *wn)
 *
 * Add <wn> (actually, WN_StartAddress(wn)) to the start of <list>.
 * ---------------------------------------------------------------------
 */
inline void WN_FREE_LIST_Push(WN_FREE_LIST *list, WN *wn)
{
  STMT_WN *stmt_wn;
  stmt_wn = (STMT_WN *) WN_StartAddress(wn);
  WN_prev_free(stmt_wn) = (WN *)(*list);
  *list = (WN_FREE_LIST)stmt_wn;
}

/* ---------------------------------------------------------------------
 * WN *WN_FREE_LIST_Pop(WN_FREE_LIST *list)
 *
 * Requires: WN_FREE_LIST_Empty(list) == FALSE
 *
 * Remove first item from <list> and return the item.
 * ---------------------------------------------------------------------
 */
inline WN *WN_FREE_LIST_Pop(WN_FREE_LIST *list)
{
  STMT_WN *item = (STMT_WN *)(*list);
  *list = (WN_FREE_LIST)WN_prev_free(item);
  return (WN *)item;
}

/* ---------------------------------------------------------------------
 * WN_FREE_LIST *Which_WN_FREE_LIST(INT32 size)
 *
 * Return the appropriate free list for a node of the given <size>,
 * or NULL if there is no appropriate list.
 *
 * See TODO in Free Lists description above for note about future work.
 * ---------------------------------------------------------------------
 */

/* For now, only two lists of the common sizes.  Note that freed expr
 * nodes with two kids will end up on the free_stmt_list, which is fine.
 * It's the size, not type, that's important when choosing the list.
 */
WN_FREE_LIST free_stmt_list, free_expr_list;

inline WN_FREE_LIST *Which_WN_FREE_LIST(INT32 size)
{
  if (size == sizeof(WN)+2*sizeof(WN *))
    return &free_stmt_list;
  else if (size == sizeof(WN))
    return &free_expr_list;
  return NULL;
}



/* ---------------------------------------------------------------------
 * void WN_Mem_Push(void)
 *
 * Saves the WN memory state for later restoration by WN_Mem_Pop.
 *
 * ---------------------------------------------------------------------
 */

void WN_Mem_Push(void)
{
    if (WN_mem_pool_ptr == &WN_mem_pool && !WN_mem_pool_initialized) {
	MEM_POOL_Initialize(WN_mem_pool_ptr, "WHIRL Nodes", TRUE);
	WN_mem_pool_initialized = TRUE;
    }
    MEM_POOL_Push(WN_mem_pool_ptr);
}



/* ---------------------------------------------------------------------
 * void WN_Mem_Pop(void)
 *
 * Deallocates all WN nodes created since the last call to WN_Mem_Push.
 *
 * ---------------------------------------------------------------------
 */

void WN_Mem_Pop(void)
{
    if (WN_mem_pool_ptr == &WN_mem_pool && !WN_mem_pool_initialized) {
	MEM_POOL_Initialize(WN_mem_pool_ptr, "WHIRL Nodes", TRUE);
	WN_mem_pool_initialized = TRUE;
    }
    free_stmt_list = free_expr_list = NULL;
    MEM_POOL_Pop(WN_mem_pool_ptr);
}


/* ---------------------------------------------------------------------
 * void IPA_WN_Delete(WN_MAP_TAB *maptab, WN *wn)
 *
 * Free node <wn> and its associated maps.
 * ---------------------------------------------------------------------
 */

#define MAX_CLEANUP_FNS 8
static void (*delete_cleanup_fns[MAX_CLEANUP_FNS])(WN *wn);
static UINT16 num_delete_cleanup_fns = 0;

void IPA_WN_Delete(WN_MAP_TAB *maptab, WN *wn)
{
  UINT16 i;
  WN_FREE_LIST *free_list;

  Is_True((WN_opcode(wn)),("Trying to delete WHIRL node with 0 opcode"));

  free_list = use_free_list ? Which_WN_FREE_LIST(WN_Size(wn)) : 0;
	  
  // trace delete
  Check_Traced_Wn_Node(wn);

#ifdef KEEP_WHIRLSTATS
  whirl_num_deallocated++;
  whirl_bytes_deallocated += WN_Size(wn);
#endif

  /* Invoke cleanup functions.
   */
  for (i = 0; i < num_delete_cleanup_fns; i++)
    (*delete_cleanup_fns[i])(wn);

  /* Add node to appropriate free list (if any).
   */
  if (free_list)
    WN_FREE_LIST_Push(free_list, wn);

  /* Free maps */
  WN_MAP_Add_Free_List(maptab, wn);

  /* For better error checking, set the opcode to an invalid one */
//WN_set_opcode(wn, OPCODE_UNKNOWN);
  WN_set_operator (wn, OPERATOR_UNKNOWN);
  WN_set_rtype (wn, MTYPE_UNKNOWN);
  WN_set_desc (wn, MTYPE_UNKNOWN);
}


/* ---------------------------------------------------------------------
 * void IPA_WN_DELETE_Tree(WN_MAP_TAB *maptab, WN *tree)
 *
 * Free node entire <tree> and its associated maps.
 * ---------------------------------------------------------------------
 */

void IPA_WN_DELETE_Tree(WN_MAP_TAB *maptab, WN *tree)
{
  WN *node;
  INT i;

  if (tree) {
    if (WN_opcode(tree) == OPC_BLOCK) {
      node = WN_first(tree);
      while (node != NULL) {
	WN *next = WN_next(node);
	IPA_WN_DELETE_Tree (maptab, node);
	node = next;
      }
    } else
      for (i = 0; i < WN_kid_count(tree); i++)
	IPA_WN_DELETE_Tree(maptab, WN_kid(tree, i));

    IPA_WN_Delete(maptab, tree);
  }
}


/* ---------------------------------------------------------------------
 * void WN_Register_Delete_Cleanup_Function(void (*cleanup_fn)(WN *wn))
 *
 * See "wn.h" for interface description.
 * ---------------------------------------------------------------------
 */

void WN_Register_Delete_Cleanup_Function(void (*cleanup_fn)(WN *wn))
{
  UINT16 i;
  for (i=0; i < num_delete_cleanup_fns; i++)
    if (delete_cleanup_fns[i] == cleanup_fn)
      return;
  FmtAssert(num_delete_cleanup_fns < MAX_CLEANUP_FNS,
	    ("attempting to register too many WN_Delete cleanup functions"));
  delete_cleanup_fns[num_delete_cleanup_fns++] = cleanup_fn;
}


/* ---------------------------------------------------------------------
 * void WN_Remove_Delete_Cleanup_Function(void (*cleanup_fn)(WN *wn))
 *
 * See "wn.h" for interface description.
 * ---------------------------------------------------------------------
 */

void WN_Remove_Delete_Cleanup_Function(void (*cleanup_fn)(WN *wn))
{
  UINT16 i;

  for (i = 0; i < num_delete_cleanup_fns; i++)
    if (delete_cleanup_fns[i] == cleanup_fn)
      break;
  if (i >= num_delete_cleanup_fns)
    return;
  --num_delete_cleanup_fns;
  for (; i < num_delete_cleanup_fns; i++)
    delete_cleanup_fns[i] = delete_cleanup_fns[i+1];
}

#ifdef KEY // bug 9651
void WN_Reset_Num_Delete_Cleanup_Fns(void)
{
  num_delete_cleanup_fns = 0;
}
#endif

/* ---------------------------------------------------------------------
 * WN_MAP_ID New_Map_Id( void )
 *
 * Return the map_id for a new WN.
 * ---------------------------------------------------------------------
 */
#define New_Map_Id() ((WN_MAP_ID) (-1))

static WN* trace_wn_node=NULL;
static mINT64 trace_wn_mapid = -1;
static UINT32 trace_wn_id = 0;
void Set_Trace_Wn_Node(WN* n) { trace_wn_node = n; }
void Set_Trace_Wn_mapid(mINT64 mapid) { trace_wn_mapid = mapid; }
void gdb_stop_here()
{
   return ;
}

void Check_Traced_Wn_Node(WN *n)
{
   if (n && (n == trace_wn_node 
             || trace_wn_mapid != -1 && WN_map_id(n) == trace_wn_mapid
#ifdef WHIRL_USE_UNIQUE_ID_FOR_DEBUG
             || trace_wn_id != 0 && trace_wn_id == WN_id(n)
#endif
             ) ) {
      gdb_stop_here();
   }
}

static set<UINT32> copied_ids;

void Set_Trace_Wn_id(UINT32 wn_id) 
{ 
#ifdef WHIRL_USE_UNIQUE_ID_FOR_DEBUG
   trace_wn_id = wn_id; 
   copied_ids.insert(wn_id);
#endif
}

void Trace_Wn_Copy(const WN *wn, const WN *src_wn)
{
#ifdef WHIRL_USE_UNIQUE_ID_FOR_DEBUG
   set<UINT32>::iterator itr = copied_ids.find(WN_id(src_wn));
   if (itr != copied_ids.end()) {
      // found src_wn's wn_id in copied list
      copied_ids.insert(WN_id(wn));
      gdb_stop_here();
   }
#endif
}

/* ---------------------------------------------------------------------
 * WN *WN_Create(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, mINT16 kid_count)
 *
 * Core create routine.  Return a new <opr, rtype, desc> node with <kid_count>
 * kids.  <kid_count> must be consistent with <opcode>.
 * ---------------------------------------------------------------------
 */

WN *
WN_Create (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, mINT16 kid_count)
{
    OPCODE opcode = OPCODE_make_op (opr, rtype, desc);
    INT16 next_prev_ptrs = (OPCODE_has_next_prev(opcode) ? 1 : 0);
    INT16 size = (sizeof(WN) +
		  (sizeof(WN *) * (MAX(0,kid_count-2))) +
		  next_prev_ptrs * (sizeof(mUINT64) + (2 * sizeof(WN *))));
    WN_FREE_LIST *free_list;
    WN *wn;

    free_list = use_free_list ? Which_WN_FREE_LIST(size) : 0;

#ifdef KEEP_WHIRLSTATS
    whirl_num_allocated++;
    whirl_bytes_allocated += size;
#endif

    Is_True(((OPCODE_nkids(opcode) == kid_count) || 
	     (OPCODE_nkids(opcode) == -1)),
	    ("Illegal number of kids for WN_Create"));

    /* Find the memory.  First try a free list, then go to the MEM_POOL.
     * Note: For safety, we use a bzero'd MEM_POOL and bzero new nodes
     *       popped from a free list, but clients should not be relying
     *	   on uninitialized fields being zeroed.
     */
    if (free_list && !WN_FREE_LIST_Empty(free_list)) {
	wn = WN_FREE_LIST_Pop(free_list);
	BZERO(wn, size);
    } else {
	if (WN_mem_pool_ptr == &WN_mem_pool && !WN_mem_pool_initialized) {
	    MEM_POOL_Initialize(WN_mem_pool_ptr, "WHIRL Nodes", TRUE);
	    MEM_POOL_Push(WN_mem_pool_ptr);
	    WN_mem_pool_initialized = TRUE;
	}
	wn = (WN *)MEM_POOL_Alloc(WN_mem_pool_ptr, size);
 	BZERO(wn, size);
    }

    /* Some nodes have next and previous pointers grow off the bottom.
     */
    if (next_prev_ptrs) {
        STMT_WN *stmt_wn;
        stmt_wn = (STMT_WN *)wn;
	wn = (WN *)&(WN_real_fields(stmt_wn));
    }

    /* Finish initialization.
     */
    WN_set_operator(wn, opr);
    WN_set_rtype(wn, rtype);
    WN_set_desc(wn, desc);
    WN_set_kid_count(wn, kid_count);
    WN_set_map_id(wn, New_Map_Id());
    wn->set_unique_id();

#ifdef TARG_SL
    /* SL initialization
     */
    WN_Set_vbuf_ofst_adjusted(wn, FALSE);
    WN_Set_is_internal_mem_ofst(wn, FALSE);
    WN_Set_is_compgoto_para(wn, FALSE);
    WN_Set_is_compgoto_for_minor(wn, FALSE);
#endif
    return wn;
}

WN *
WN_Create_Generic (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
		    mINT16 kid_count, WN *next, WN *prev,
		   ST_IDX st,INT32 label_number, INT32 num_entries,
		   TY_IDX ty, TY_IDX load_addr_ty, WN_OFFSET offset,
		   INT16 cvtl_bits, INT32 /* num_dim */,
		   WN_ESIZE element_size, INT64 const_value, UINT32 flag,
		   INTRINSIC intrinsic) 
{
  OPCODE opcode = OPCODE_make_op (opr, rtype, desc);
  WN *wn;


  wn = WN_Create(opcode,kid_count);
  if (OPCODE_has_next_prev(opcode)) {
    WN_next(wn) = next;
    WN_prev(wn) = prev;
  }
  if (OPCODE_has_sym(opcode)) {
    WN_st_idx(wn) = st;
  }
  if (OPCODE_has_label(opcode)) {
    WN_label_number(wn) = label_number;
  }
  if (OPCODE_has_num_entries(opcode)) {
    WN_num_entries(wn) = num_entries;
  }
  if (OPCODE_has_1ty(opcode)) {
    WN_set_ty(wn,ty);
  }
  if (OPCODE_has_2ty(opcode)) {
    WN_set_ty(wn,ty);
    WN_set_load_addr_ty(wn,load_addr_ty);
  }
  if (OPCODE_has_offset(opcode)) {
    WN_offset(wn) = offset;
  }
  if (OPCODE_has_bits(opcode)) {
    WN_cvtl_bits(wn) = cvtl_bits;
  }
  /* Num_dim is now just a read-only quantity */
  if (OPCODE_has_esize(opcode)) {
    WN_element_size(wn) = element_size;
  }
  if (OPCODE_has_value(opcode)) {
    WN_const_val(wn) = const_value;
  }
  if (OPCODE_has_flags(opcode)) {
    WN_set_flag(wn,flag);
  }
  if (OPCODE_has_inumber(opcode)) {
    WN_intrinsic(wn) = intrinsic;
  }
  return wn;
}

/* ignore children and next-previous pointers, are the two nodes
 * equivalent
 */
BOOL WN_Equiv(WN *wn1, WN *wn2)
{
  OPCODE opcode;

  opcode = WN_opcode(wn1);
  if (opcode != WN_opcode(wn2)) return(FALSE);
  if (opcode == OPC_BLOCK) return(TRUE);
  if (WN_kid_count(wn1) != WN_kid_count(wn2)) return(FALSE);

  if (OPCODE_has_sym(opcode)) {
    if (WN_st_idx(wn1) != WN_st_idx(wn2)) return(FALSE);
  }
  if (OPCODE_has_label(opcode)) {
    if (WN_label_number(wn1) != WN_label_number(wn2)) return(FALSE);
  }
  if (OPCODE_has_num_entries(opcode)) {
    if (WN_num_entries(wn1) != WN_num_entries(wn2)) return(FALSE);
  }
  if (OPCODE_has_1ty(opcode)) {
    if (WN_ty(wn1) != WN_ty(wn2)) return(FALSE);
  }
  if (OPCODE_has_2ty(opcode)) {
    if (WN_ty(wn1) != WN_ty(wn2)) return(FALSE);
    if (WN_load_addr_ty(wn1) != WN_load_addr_ty(wn2)) return(FALSE);
  }
  if (OPCODE_has_offset(opcode)) {
    if (WN_offset(wn1) != WN_offset(wn2)) return(FALSE);
  }
  if (OPCODE_has_bits(opcode)) {
    if (WN_cvtl_bits(wn1) != WN_cvtl_bits(wn2)) return(FALSE);
  }
  if (OPCODE_has_ndim(opcode)) {
    if (WN_num_dim(wn1) != WN_num_dim(wn2)) return(FALSE);
  }
  if (OPCODE_has_esize(opcode)) {
    if (WN_element_size(wn1) != WN_element_size(wn2)) return(FALSE);
  }
  if (OPCODE_has_value(opcode)) {
    if (WN_const_val(wn1) != WN_const_val(wn2)) return(FALSE);
  }
  if (OPCODE_has_flags(opcode)) {
    if (WN_flag(wn1) != WN_flag(wn2)) return(FALSE);
  }
  if (OPCODE_has_inumber(opcode)) {
    if (WN_intrinsic(wn1) != WN_intrinsic(wn2)) return(FALSE);
  }

  return(TRUE);
}


/*
 * Create hierarchical control flow nodes
 *
 */

WN *WN_CreateBlock(void)
{
  WN *wn;

  wn = WN_Create(OPC_BLOCK,0);
  WN_first(wn) = WN_last(wn) = NULL;
  return(wn);
}

WN *WN_CreateDO(WN *index, WN *start, WN *end, WN *step, WN *body, WN *loop_info)
{
  WN *wn;
  INT nkids = 5;
  if (loop_info != NULL) nkids++;

  Is_True(OPCODE_is_stmt(WN_opcode(start)),("Bad start in WN_CreateDO"));
  Is_True(OPCODE_is_stmt(WN_opcode(step)),("Bad step in WN_CreateDO"));
  Is_True(loop_info == NULL || WN_opcode(loop_info) == OPC_LOOP_INFO,
	("Bad loop_info in WN_CreateDO"));
/* TODO: can't find  Boolean type
  Is_True(WN_rtype(end)==Boolean_type,
	("Bad end in WN_CreateDO"));
*/
  Is_True(WN_opcode(body) == OPC_BLOCK,("Bad body in WN_CreateDO"));

  wn = WN_Create(OPC_DO_LOOP,nkids);
  WN_index(wn) = index;
  WN_start(wn) = start;
  WN_end(wn) = end;
  WN_step(wn) = step;
  WN_do_body(wn) = body;
  if (loop_info) WN_set_do_loop_info(wn, loop_info);

  return(wn);
}

WN *WN_CreateDoWhile(WN *test, WN *body)
{
  WN *wn;

  Is_True(WN_opcode(body) == OPC_BLOCK, ("Bad body in WN_CreateDoWhile"));
/* TODO: can't find  Boolean type
  Is_True(WN_rtype(test)==Boolean_type,
	  ("Bad test in WN_CreateDoWhile"));
*/
  wn = WN_Create(OPC_DO_WHILE,2);
  WN_while_test(wn) = test;
  WN_while_body(wn) = body;

  return(wn);
}

WN *WN_CreateWhileDo(WN *test, WN *body)
{
  WN *wn;

  Is_True(WN_opcode(body) == OPC_BLOCK, ("Bad body in WN_CreateWhileDo"));
/* TODO: can't find  Boolean type
  Is_True(WN_rtype(test)==Boolean_type,
	  ("Bad test in WN_CreateWhileDo"));
*/
  wn = WN_Create(OPC_WHILE_DO,2);
  WN_while_test(wn) = test;
  WN_while_body(wn) = body;

  return(wn);
}

WN *WN_CreateIf(WN *test, WN *if_then, WN *if_else)
{
  WN *wn;

  Is_True(WN_opcode(if_then) == OPC_BLOCK, ("Bad then in WN_CreateIf"));
  Is_True(WN_opcode(if_else) == OPC_BLOCK, ("Bad else in WN_CreateIf"));
/* TODO: can't find  Boolean type
  Is_True(WN_rtype(test)==Boolean_type,
	  ("Bad test in WN_CreateIf"));
*/
  wn = WN_Create(OPC_IF,3);
  WN_if_test(wn) = test;
  WN_then(wn) = if_then;
  WN_else(wn) = if_else;

  WN_Reset_If_Guard(wn);
  return(wn);
}

/*============================================================================
  REGION handling routines
============================================================================*/

/* check if a region element (statement list, pragma list, or exit list) has
   a block surrounding it, if not create one. Some lists may be NULL, in that
   case return an empty block	*/
static WN *WN_block_element(WN *element)
{
  if (element == NULL || WN_opcode(element) != OPC_BLOCK) {
    WN *block = WN_CreateBlock();
    if (element != NULL) {
    	WN_first(block) = element;
    	WN_last (block) = element;
    	WN_prev(element) = NULL;
    	WN_next(element) = NULL;
    }
    return block;
  } else
    return element;
}

static INT32 last_region_id = 0;

extern "C" INT32
New_Region_Id(void)
{
  return ++last_region_id;
}
extern "C" INT32
Last_Region_Id(void)
{
  return last_region_id;
}

/* set maximum region id seen so far;
 * special case 0 to mean reset region ids to 0 */
extern void
Set_Max_Region_Id (INT id)
{
	if (id == 0)
		last_region_id = 0;
	else
		last_region_id = MAX(last_region_id,id);
}

/* =======================================================================
 *
 *  WN_CreateRegion
 *
 *  body must be an SCF node.
 *  If body is not a block, 
 *    Create a REGION whose kid is a BLOCK which contains body.
 *  If body is a BLOCK
 *    Create a REGION whose kid is body.
 *  Copy the linenum from body to the result.
 *
 *		OPC_REGION
 *            /      |      \
 *   OPC_BLOCK   OPC_BLOCK    OPC_BLOCK
 *   exit list   pragma list  statement list
 *
 * region_id: pass in -1 for WN_CreateRegion to assign an unique region id,
 * otherwise specify a valid region_id (0-N) for the region.
 *
 * =======================================================================
 */
WN *
WN_CreateRegion (REGION_KIND kind, WN *body, WN *pragmas, WN *exits,
                 INT region_id, INITO_IDX ereg_supp) 
{
    WN *wn;

    Is_True( OPCODE_is_scf(WN_opcode(body)) , ("Bad body in WN_CreateRegion"));
    wn = WN_Create(OPC_REGION,3);       // 3 kids: exits, pragmas, statements 
    WN_Set_Linenum(wn,WN_Get_Linenum(body));

    WN_set_region_kind(wn, kind);
    WN_set_region_id(wn, (region_id == -1) ? New_Region_Id() : region_id);
    WN_region_body(wn) = WN_block_element(body);
    WN_region_pragmas(wn) = WN_block_element(pragmas);
    WN_region_exits(wn) = WN_block_element(exits);
    WN_ereg_supp(wn) = ereg_supp;

    Set_PU_has_region (Get_Current_PU ());

    return(wn);
}

WN *WN_CreateRegionExit (INT32 label_number)
{
  WN *wn;

  wn = WN_Create(OPC_REGION_EXIT,0);
  WN_label_number(wn) = label_number;

  return(wn);
}

// nkids is the number of function arguments
WN *
WN_CreateEntry (INT16 nkids, ST_IDX name, WN *body, WN *pragmas, WN *varrefs)
{
  WN *wn;
  wn = WN_Create (OPC_FUNC_ENTRY, nkids + 3);
  WN_entry_name(wn) = name;
  WN_func_body(wn) = body;
  WN_func_pragmas(wn) = WN_block_element(pragmas);
  WN_func_varrefs(wn) = WN_block_element(varrefs);
  return wn;
}

/*
 * Create statement nodes
 *
 */

#if defined(TARG_SL) //fork_joint
WN *WN_CreateFork(INT32 label_number,  BOOL major)
{
  WN *wn;
  wn = WN_Create(major ? OPC_SL2_FORK_MAJOR : OPC_SL2_FORK_MINOR , 0);
  WN_label_number(wn) = label_number;
  return(wn);
}
#endif 

// no st anymore in goto
WN *WN_CreateGoto(INT32 label_number)
{
  WN *wn;

  wn = WN_Create(OPC_GOTO,0);
  WN_label_number(wn) = label_number;

  return(wn);
}

WN *WN_CreateGotoOuterBlock (INT32 label_number, SYMTAB_IDX label_level)
{
  WN * wn;
  wn = WN_Create(OPC_GOTO_OUTER_BLOCK, 0);
  WN_label_number(wn) = label_number;
  WN_label_level(wn)  = label_level;

  return wn;
} /* WN_CreateGotoOuterBlock */

WN *WN_CreateAgoto(WN *addr)
{
  WN *wn;

  Is_True(MTYPE_byte_size(WN_rtype(addr)) == MTYPE_byte_size(Pointer_type),
          ("Bad addr in WN_CreateAgoto"));
  wn = WN_Create(OPC_AGOTO,1);
  WN_kid0(wn) = addr;

  return(wn);
}

WN *WN_CreateAltentry(ST_IDX entry)
{
  WN *wn;

  wn = WN_Create(OPC_ALTENTRY,0);
  WN_st_idx(wn) = entry;

  return(wn);
}

WN *WN_CreateTruebr(INT32 label_number, WN *exp)
{
  WN *wn;

  Is_True(OPCODE_is_expression(WN_opcode(exp)),
	  ("Bad exp in WN_CreateTruebr"));
  wn = WN_Create(OPC_TRUEBR,1);
  WN_kid0(wn) = exp;
  WN_label_number(wn) = label_number;

  return(wn);
}


WN *WN_CreateZDLBr(INT32 label_number) {
  WN *wn;
  
  wn = WN_Create(OPC_ZDLBR, 0);
  WN_label_number(wn) = label_number;
  
  return(wn);
}


WN *WN_CreateFalsebr(INT32 label_number, WN *exp)
{
  WN *wn;

  Is_True(OPCODE_is_expression(WN_opcode(exp)),
	  ("Bad exp in WN_CreateFalsebr"));
  wn = WN_Create(OPC_FALSEBR,1);
  WN_kid0(wn) = exp;
  WN_label_number(wn) = label_number;

  return(wn);
}

/* a return */
WN *WN_CreateReturn(void)
{
  WN *wn;

  wn = WN_Create(OPC_RETURN,0);

  return(wn);
}

WN *WN_CreateReturn_Val (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, WN *val)
{
  WN *wn;

  wn = WN_Create (opr, rtype, desc, 1);
  WN_kid0(wn) = val;

  return(wn);
}

WN *
WN_CreateLabel (
	INT32 label_number, UINT32 label_flag, WN *loop_info) 
{
  WN *wn;
  INT nkids = 0;
  if (loop_info != NULL) nkids++;
  Is_True(loop_info == NULL || WN_opcode(loop_info) == OPC_LOOP_INFO,
	("Bad loop_info in WN_CreateDO"));

  wn = WN_Create(OPC_LABEL, nkids);
  WN_label_number(wn) = label_number;
  WN_label_flag(wn) = label_flag;
  if ( loop_info ) 
    WN_set_label_loop_info(wn, loop_info);

  return(wn);
}

WN *WN_CreateCompgoto(INT32 num_entries, WN *value,
		      WN *block, WN *deflt, INT32 last_label)
{
  WN *wn;

  Is_True(OPCODE_is_expression(WN_opcode(value)),
	  ("Bad value in WN_CreateCompgoto"));
  Is_True(WN_opcode(block) == OPC_BLOCK, 
	  ("Bad block in WN_CreateCompgoto"));
  if (deflt) Is_True(WN_opcode(deflt) == OPC_GOTO, 
	  ("Bad deflt in WN_CreateCompgoto"));
  if (deflt) {
    wn = WN_Create(OPC_COMPGOTO,3);
  } else {
    wn = WN_Create(OPC_COMPGOTO,2);
  }
  WN_kid0(wn) = value;
  WN_kid(wn,1) = block;
  if (deflt) WN_kid(wn,2) = deflt;
  WN_num_entries(wn) = num_entries;
  WN_last_label(wn) = last_label;

  return(wn);
}

WN *WN_CreateSwitch(INT32 num_entries, WN *value,
		    WN *block, WN *deflt, INT32 last_label)
{
  WN *wn;

  Is_True(OPCODE_is_expression(WN_opcode(value)),
	  ("Bad value in WN_CreateSwitch"));
  Is_True(WN_opcode(block) == OPC_BLOCK, 
	  ("Bad block in WN_CreateSwitch"));
  if (deflt) Is_True(WN_opcode(deflt) == OPC_GOTO, 
	  ("Bad deflt in WN_CreateSwitch"));
  if (deflt) {
    wn = WN_Create(OPC_SWITCH,3);
  } else {
    wn = WN_Create(OPC_SWITCH,2);
  }
  WN_switch_test(wn) = value;
  WN_switch_table(wn) = block;
  if (deflt) WN_switch_default(wn) = deflt;
  WN_num_entries(wn) = num_entries;
  WN_last_label(wn) = last_label;

#ifdef FRONT_END
  Set_PU_has_very_high_whirl (Get_Current_PU ());
#endif /* FRONT_END */

  return(wn);
}

WN *WN_CreateCasegoto (INT64 case_value, INT32 case_label)
{
  WN *wn;
  wn = WN_Create(OPC_CASEGOTO,0); 
  WN_const_val(wn) = case_value;
  WN_label_number(wn) = case_label;
  return wn;
}

WN *WN_CreateXgoto(INT32 num_entries, WN *value, WN *block, ST_IDX st)
{
  WN *wn;

  Is_True(OPCODE_is_expression(WN_opcode(value)),
	  ("Bad value in WN_CreateXgoto"));
  Is_True(WN_opcode(block) == OPC_BLOCK, 
	  ("Bad block in WN_CreateXgoto"));
  wn = WN_Create(OPC_XGOTO,2);
  WN_kid0(wn) = value;
  WN_kid(wn,1) = block;
  WN_st_idx(wn) = st;
  WN_num_entries(wn) = num_entries;

  return(wn);
}

WN *
WN_CreateIstore (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
		 WN_OFFSET offset, TY_IDX ty, WN *value, WN *addr, 
		 UINT field_id)
{
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn;
  Is_True(MTYPE_is_pointer(WN_rtype(addr)) ||
          WN_rtype(addr)==MTYPE_U8 ||
	  WN_rtype(addr)==MTYPE_U4 ||
          WN_rtype(addr)==MTYPE_I8 ||
	  WN_rtype(addr)==MTYPE_I4,
          ("Bad addr in WN_CreateIstore"));
  Is_True(OPCODE_is_expression(WN_opcode(value)),
	  ("Bad value in WN_CreateIstore"));
  Is_True(Types_Are_Compatible(OPCODE_desc(opc),value),
	  ("Bad return type in WN_CreateIstore"));
  Is_True(opr == OPR_ISTORE || opr == OPR_ISTBITS,
          ("Bad opcode in WN_CreateIstore"));

#ifdef FRONT_END
 if (desc == MTYPE_M) { 
   Set_PU_has_very_high_whirl (Get_Current_PU ());
 }
#endif /* FRONT_END */

#if (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77)

  addr = fe_combine_address_offset ( &offset, addr );

  UINT64 ty_size;
  if (field_id == 0) {
    ty_size = TY_size(TY_pointed(ty));
  }
  else {
    UINT tmp = 0;
    ty_size = TY_size(FLD_type(FLD_get_to_field(TY_pointed(ty),field_id,tmp)));
  }

  if ( FE_Cvtl_Opt ) {

    OPCODE value_opc;

    value_opc = WN_opcode(value);

    if (    (    (    WN_operator(value) == OPR_CVTL
                   || WN_operator(value) == OPR_CVT )
              && WN_cvtl_bits(value) == ty_size * 8 )
         || (    (    value_opc == OPC_I4I8CVT
                   || value_opc == OPC_I4U8CVT
                   || value_opc == OPC_U4I8CVT
                   || value_opc == OPC_U4U8CVT )
              && ty_size < 8 ) )
      value = WN_kid0(value);
  }

  if (    FE_Store_Opt
       && WN_operator(value) == OPR_BAND ) {

    UINT64   mask;
    WN     * kid0;
    WN     * kid1;

    mask = masks [ty_size];
    kid0 = WN_kid0(value);
    kid1 = WN_kid1(value);

    if (    WN_operator(kid0) == OPR_INTCONST
         && ( ( WN_const_val(kid0) & mask ) == mask ) )
      value = kid1;

    else
    if (    WN_operator(kid1) == OPR_INTCONST
         && ( ( WN_const_val(kid1) & mask ) == mask ) )
      value = kid0;
  }
#endif /* (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77) */

  wn = WN_SimplifyIstore(opc,offset,ty,field_id,value,addr);

  if (!wn) {
     wn = WN_Create(opc,2);
     WN_kid0(wn) = value;
     WN_kid1(wn) = addr;
     WN_store_offset(wn) = offset;
     WN_set_ty(wn,ty);
     WN_set_field_id(wn, field_id);
  }
  else {
    /* Parent pointer (if it exists) for returned node must be NULL */
    if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
      WN_MAP_Set(WN_SimpParentMap, wn, NULL);
    }
  }
  
  return(wn);
}

WN *
WN_CreateIstorex (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
		  TY_IDX ty, WN *value, WN *addr1, WN *addr2)
{
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn;

  Is_True(OPCODE_operator(opc) == OPR_ISTOREX,
          ("Bad opcode in WN_CreateIstorex"));
  Is_True(OPCODE_is_expression(WN_opcode(value)),
	  ("Bad value in WN_CreateIstorex"));
  Is_True(WN_operator_is(addr1, OPR_LDID),
          ("Bad address 1 in WN_CreateIstorex"));
  Is_True(WN_operator_is(addr2, OPR_LDID),
          ("Bad address 2 in WN_CreateIstorex"));
  wn = WN_Create (opr, rtype, desc, 3);
  WN_store_offset(wn) = 0;
  WN_kid0(wn) = value;
  WN_kid1(wn) = addr1;
  WN_kid(wn,2) = addr2;
  WN_set_ty(wn,ty);
  return(wn);
}

WN *WN_CreatePrefetchx(UINT32 flag, WN *addr1, WN *addr2)
{
  WN *wn;

  Is_True(WN_operator_is(addr1, OPR_LDID),
          ("Bad address 1 in WN_CreatePrefetchx"));
  Is_True(WN_operator_is(addr2, OPR_LDID),
          ("Bad address 2 in WN_CreatePrefetchx"));
  wn = WN_Create(OPC_PREFETCHX,2);
  WN_kid0(wn) = addr1;
  WN_kid1(wn) = addr2;
  WN_set_flag(wn,flag);
  return(wn);
}

WN *WN_CreateMstore(WN_OFFSET offset, TY_IDX ty,
		    WN *value, WN *addr, WN *num_bytes)
{
  WN *wn;

  Is_True(MTYPE_is_pointer(WN_rtype(addr)) ||
          WN_rtype(addr)==MTYPE_U8 ||
	  WN_rtype(addr)==MTYPE_U4 ||
          WN_rtype(addr)==MTYPE_I8 ||
	  WN_rtype(addr)==MTYPE_I4,
          ("Bad addr in WN_CreateMstore"));
  Is_True(OPCODE_is_expression(WN_opcode(value)),
          ("Bad value in WN_CreateMstore"));
  Is_True(OPCODE_is_expression(WN_opcode(num_bytes)),
	  ("Bad num_bytes in WN_CreateMstore"));

#if (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77)

  addr = fe_combine_address_offset ( &offset, addr );

#endif /* (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77) */

  wn = WN_Create(OPC_MSTORE,3);
  WN_kid0(wn) = value;
  WN_kid1(wn) = addr;
  WN_kid(wn,2) = num_bytes;
  WN_store_offset(wn) = offset;
  WN_set_ty(wn,ty);

#ifdef FRONT_END
  Set_PU_has_very_high_whirl (Get_Current_PU ());
#endif /* FRONT_END */
  return(wn);
}

/* Create a STID, note this needs the opcode since there are many */
/* opcodes with the OPR_STID operator */
WN *
WN_CreateStid (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
	       WN_OFFSET offset, ST* st, TY_IDX ty, WN *value, UINT field_id)
{
    OPCODE opc = OPCODE_make_op (opr, rtype, desc);
    WN *wn;
    ST_IDX st_idx = ST_st_idx (st);

    Is_True(OPCODE_is_expression(WN_opcode(value)),
	    ("Bad value in WN_CreateStid"));
#ifndef KEY	// g++ can create cases where the actual parameter to a
		// function is a ptr, but the formal parameter is a struct.
    Is_True(Types_Are_Compatible(OPCODE_desc(opc),value),
	    ("Bad return type in WN_CreateStid"));
#endif
    Is_True(opr == OPR_STID || opr == OPR_STBITS,
	    ("Bad opcode in WN_CreateStid"));
#ifdef FRONT_END
    Is_True(!((offset == 0) && (st == Int32_Preg ||
				st == Int64_Preg ||
				st == Float32_Preg ||
				st == Float64_Preg)),
	    ("Preg offset 0 in WN_CreateStid"));
#endif /* FRONT_END */

#ifdef FRONT_END
   if (desc == MTYPE_M && CURRENT_SYMTAB > GLOBAL_SYMTAB) {
   	 Set_PU_has_very_high_whirl (Get_Current_PU ());
   }
#endif /* FRONT_END */

#if (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77)

    UINT64 ty_size;
    if (field_id == 0) {
      ty_size = TY_size(ty);
    }
    else {
      UINT tmp = 0;
      ty_size = TY_size(FLD_type(FLD_get_to_field(ty, field_id, tmp)));
    }

    if (FE_Cvtl_Opt && ST_class(st) != CLASS_PREG ) {

	OPCODE value_opc;

	value_opc = WN_opcode(value);

	if (    (    (    WN_operator(value) == OPR_CVTL
                       || WN_operator(value) == OPR_CVT )
                  && WN_cvtl_bits(value) == ty_size * 8 )
             || (    (    value_opc == OPC_I4I8CVT
                       || value_opc == OPC_I4U8CVT
                       || value_opc == OPC_U4I8CVT
                       || value_opc == OPC_U4U8CVT )
                  && ty_size < 8 ) )
	    value = WN_kid0(value);
    }

    if (FE_Store_Opt && ST_class(st) != CLASS_PREG &&
	WN_operator(value) == OPR_BAND ) {

	UINT64   mask;
	WN     * kid0;
	WN     * kid1;

	mask = masks [ty_size];
	kid0 = WN_kid0(value);
	kid1 = WN_kid1(value);

	if (    WN_operator(kid0) == OPR_INTCONST
	    && ( ( WN_const_val(kid0) & mask ) == mask ) )
	    value = kid1;

	else
	    if (    WN_operator(kid1) == OPR_INTCONST
		&& ( ( WN_const_val(kid1) & mask ) == mask ) )
		value = kid0;
    }

#endif /* (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77) */
#ifdef TARG_NVISA
    if ((MTYPE_byte_size(desc) == 8 && MTYPE_byte_size(WN_rtype(value)) == 4)
     || (MTYPE_byte_size(desc) == 4 && MTYPE_byte_size(WN_rtype(value)) == 8))
    {
        // Rather than create I8STID(I4*),
        // create I8STID(I8I4CVT(I4*).
        // We need explicit conversion
        // because we use different registers for the two sizes,
        DevWarn("insert cvt of stid, %d %d", desc, WN_rtype(value));
        value = WN_Cvt(WN_rtype(value), desc, value);
    }
#endif

    wn = WN_Create(opc,1);
    WN_kid0(wn) = value;
    WN_store_offset(wn) = offset;
    WN_st_idx(wn) = st_idx;
    WN_set_ty(wn,ty);
    WN_set_field_id(wn, field_id);

    return(wn);
}

WN *WN_CreatePrefetch(WN_OFFSET offset, UINT32 flag, WN *addr)
{
  WN *wn;
  Is_True(MTYPE_is_pointer(WN_rtype(addr)),
          ("Bad addr in WN_CreatePrefetch"));
  wn = WN_Create(OPC_PREFETCH,1);
  WN_kid0(wn) = addr;
  WN_offset(wn) = offset;
  WN_set_flag(wn,flag);
  return(wn);
}

WN *WN_CreateIo(IOSTATEMENT iostatement, mINT16 kid_count)
{
  WN *wn;

  Is_True(kid_count >= 1,("Bad kid_count in WN_CreateIo"));
  wn = WN_Create(OPC_IO,kid_count);
  WN_io_statement(wn) = iostatement;
  WN_Set_IO_Library(wn,target_io_library);

  return(wn);
}

WN *WN_CreateIoItem0(IOITEM ioitem, TY_IDX ty)
{
  WN *wn;

  wn = WN_Create(OPC_IO_ITEM,0);
  WN_io_item(wn) = ioitem;
  WN_set_ty(wn, ty);

  return(wn);
}

WN *WN_CreateIoItem1(IOITEM ioitem, WN *kid0, TY_IDX ty)
{
  WN *wn;

  wn = WN_Create(OPC_IO_ITEM,1);
  WN_io_item(wn) = ioitem;
  WN_kid0(wn) = kid0;
  WN_set_ty(wn, ty);

  return(wn);
}

WN *WN_CreateIoItem2(IOITEM ioitem, WN *kid0, WN *kid1, TY_IDX ty)
{
  WN *wn;

  wn = WN_Create(OPC_IO_ITEM,2);
  WN_io_item(wn) = ioitem;
  WN_kid0(wn) = kid0;
  WN_kid1(wn) = kid1;
  WN_set_ty(wn, ty);

  return(wn);
}

WN *WN_CreateIoItem3(IOITEM ioitem, WN *kid0, WN *kid1, WN *kid2, TY_IDX ty)
{
  WN *wn;

  wn = WN_Create(OPC_IO_ITEM,3);
  WN_io_item(wn) = ioitem;
  WN_kid0(wn) = kid0;
  WN_kid1(wn) = kid1;
  WN_kid(wn,2) = kid2;
  WN_set_ty(wn, ty);

  return(wn);
}

WN *WN_CreateIoItemN(IOITEM ioitem, mINT16 kid_count, TY_IDX ty)
{
  WN *wn;

  wn = WN_Create(OPC_IO_ITEM,kid_count);
  WN_io_item(wn) = ioitem;
  WN_set_ty(wn, ty);

  return(wn);
}

WN *WN_CreateEval(WN *exp)
{
  WN *wn;

  Is_True(OPCODE_is_expression(WN_opcode(exp)),
	  ("Bad exp in WN_CreateEval"));
  wn = WN_Create(OPC_EVAL,1);
  WN_kid0(wn) = exp;

  return(wn);
}

WN *
WN_CreatePragma (WN_PRAGMA_ID pragma_name, ST_IDX st, INT32 arg1, INT32 arg2)
{
  WN *wn;

  wn = WN_Create(OPC_PRAGMA,0);
  WN_pragma(wn) = pragma_name;
  WN_st_idx(wn) = st;
  WN_pragma_flags(wn) = 0;
  WN_pragma_arg1(wn) = arg1;
  WN_pragma_arg2(wn) = arg2;

  return(wn);
}

WN *
WN_CreatePragma (WN_PRAGMA_ID pragma_name,
		 ST_IDX       st,
		 INT32        arg1,
		 PREG_NUM     asm_copyout_preg,
		 UINT32       asm_opnd_num)
{
  WN *wn;

  Is_True(pragma_name == WN_PRAGMA_ASM_CONSTRAINT,
	  ("ASM_CONSTRAINT-specific CreatePragma can't be used for "
	   "other pragmas"));
  wn = WN_Create(OPC_PRAGMA,0);
  WN_pragma(wn) = pragma_name;
  WN_st_idx(wn) = st;
  WN_pragma_flags(wn) = 0;
  WN_pragma_arg1(wn) = arg1;
  WN_set_pragma_asm_copyout_preg(wn, asm_copyout_preg);
  WN_set_pragma_asm_opnd_num(wn, asm_opnd_num);

  return(wn);
}

WN *WN_CreateXpragma(WN_PRAGMA_ID pragma_name, ST_IDX st, INT16 kid_count)
{
  WN *wn;

  wn = WN_Create(OPC_XPRAGMA, kid_count);
  WN_pragma(wn) = pragma_name;
  WN_st_idx(wn) = st;
  WN_pragma_flags(wn) = 0;
  WN_pragma_arg2(wn) = 0;
  WN_kid(wn, 0) = NULL;

  return(wn);
}

/*
 * Create expression nodes
 *
 */

/* first the generic ones */

WN *WN_CreateExp0(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc)
{
  WN *wn;

  wn = WN_Create(opr, rtype, desc, 0);

  return(wn);
}

WN *WN_CreateExp1(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,WN *kid0)
{
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn;

  Is_True(OPCODE_is_expression(WN_opcode(kid0)),
	  ("Bad kid0 in WN_CreateExp1"));

  wn = WN_SimplifyExp1(opc, kid0);
  if (!wn) {
     wn = WN_Create(opr, rtype, desc, 1);
     WN_kid0(wn) = kid0;
  }
  else {
    /* Parent pointer (if it exists) for returned node must be NULL */
    if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
      WN_MAP_Set(WN_SimpParentMap, wn, NULL);
    }
  }

  return(wn);
}

WN *WN_CreateExp2(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, WN *kid0, WN *kid1)
{
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn = NULL;

  Is_True(OPCODE_is_expression(WN_opcode(kid0)),
	  ("Bad kid0 in WN_CreateExp2"));
  Is_True(OPCODE_is_expression(WN_opcode(kid1)),
	  ("Bad kid1 in WN_CreateExp2"));


  /* bug#2731 */
#ifdef KEY
  if( !WN_has_side_effects(kid0) &&
      !WN_has_side_effects(kid1) &&
      !Disable_Simplification_For_FE) /* Disable Simplification if it is called in WGEN. */
#endif
    wn = WN_SimplifyExp2(opc, kid0, kid1);

  if (!wn) {
     wn = WN_Create(opr,rtype,desc,2);
     WN_kid0(wn) = kid0;
     WN_kid1(wn) = kid1;
  }
  else {
    /* Parent pointer (if it exists) for returned node must be NULL */
    if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
      WN_MAP_Set(WN_SimpParentMap, wn, NULL);
    }
  }

  return(wn);
}

WN *WN_CreateExp3(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
		  WN *kid0, WN *kid1, WN *kid2)
{
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn = NULL;

  Is_True(OPCODE_is_expression(WN_opcode(kid0)),
	  ("Bad kid0 in WN_CreateExp3"));
  Is_True(OPCODE_is_expression(WN_opcode(kid1)),
	  ("Bad kid1 in WN_CreateExp3"));
  Is_True(OPCODE_is_expression(WN_opcode(kid2)),
	  ("Bad kid2 in WN_CreateExp3"));

  /* bug#2731 */
#ifdef KEY
  if( !WN_has_side_effects(kid0) )
#endif
    wn = WN_SimplifyExp3(opc, kid0, kid1, kid2);

  if (!wn) {
     wn = WN_Create(opr,rtype,desc,3);
     WN_kid0(wn) = kid0;
     WN_kid1(wn) = kid1;
     WN_kid(wn,2) = kid2;
  }
  else {
    /* Parent pointer (if it exists) for returned node must be NULL */
    if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
      WN_MAP_Set(WN_SimpParentMap, wn, NULL);
    }
  }

  return(wn);
}

WN *WN_CreateCvtl(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
		  INT16 cvtl_bits, WN* kid0)
{
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn;

  Is_True(OPCODE_operator(opc) == OPR_CVTL, ("Bad opcode in WN_CreateCvtl"));

  wn = WN_SimplifyCvtl(opc, cvtl_bits, kid0);
  if (!wn) {
     wn = WN_CreateExp1(opr, rtype, desc, kid0);
     WN_cvtl_bits(wn) = cvtl_bits;
  }
  else {
    /* Parent pointer (if it exists) for returned node must be NULL */
    if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
      WN_MAP_Set(WN_SimpParentMap, wn, NULL);
    }
  }

  return(wn);
}

WN *
WN_CreateIload (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
		WN_OFFSET offset, TY_IDX ty,
		TY_IDX load_addr_ty, WN *addr, UINT field_id) 
{
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn;

  Is_True(MTYPE_is_pointer(WN_rtype(addr)) ||
          WN_rtype(addr) == MTYPE_U8 ||
	  WN_rtype(addr) == MTYPE_U4 ||
	  WN_rtype(addr) == MTYPE_I8 ||
	  WN_rtype(addr) == MTYPE_I4,
          ("Bad addr in WN_CreateIload"));

  Is_True(opr == OPR_ILOAD || opr == OPR_ILDBITS,
          ("Bad opcode in WN_CreateIload"));

#if (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77) 

  addr = fe_combine_address_offset ( &offset, addr );

#endif /* (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77) */
  
  wn = WN_SimplifyIload(opc,offset,ty,field_id,load_addr_ty,addr);
  if (!wn) {
#ifdef TARG_NVISA
     if ((MTYPE_byte_size(rtype) == 8 && MTYPE_byte_size(desc) == 4)
      || (MTYPE_byte_size(rtype) == 4 && MTYPE_byte_size(desc) == 8))
     {
        // Rather than create I8I4ILOAD,
        // create I8I4CVT(I4I4ILOAD).
        // I8I4ILOAD can cause later problems with pregs,
        // which have same rtype and desc, so wopt sometimes loses info
        // about the implicit conversion, and we need explicit conversion
        // because we use different registers for the two sizes,
        DevWarn("insert cvt of iload, %d %d", rtype, desc);
        wn = WN_CreateExp1(opr,desc,desc,addr);
        WN_load_offset(wn) = offset;
        WN_set_ty(wn,ty);
        WN_set_load_addr_ty(wn,load_addr_ty);
        WN_set_field_id(wn, field_id);
        wn = WN_Cvt(desc, rtype, wn);
        return wn;
     }
#endif
     wn = WN_CreateExp1(opr,rtype,desc,addr);
     WN_load_offset(wn) = offset;
     WN_set_ty(wn,ty);
     WN_set_load_addr_ty(wn,load_addr_ty);
     WN_set_field_id(wn, field_id);
  }
  else {
    /* Parent pointer (if it exists) for returned node must be NULL */
    if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
      WN_MAP_Set(WN_SimpParentMap, wn, NULL);
    }
  }

  return(wn);
}

WN *
WN_CreateIloadx (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
		 TY_IDX ty, TY_IDX load_addr_ty, WN *addr1,
		 WN *addr2) 
{
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn;

  Is_True(OPCODE_operator(opc) == OPR_ILOADX,
          ("Bad opcode in WN_CreateIloadx"));
  Is_True(WN_operator(addr1) == OPR_LDID,
          ("Bad address1 in WN_CreateIloadx"));
  Is_True(WN_operator(addr2) == OPR_LDID,
          ("Bad address2 in WN_CreateIloadx"));
  wn = WN_CreateExp2(opr,rtype,desc,addr1,addr2);
  WN_load_offset(wn) = 0;
  WN_set_ty(wn,ty);
  WN_set_load_addr_ty(wn,load_addr_ty);
  return(wn);
}


WN *WN_CreateMload (WN_OFFSET offset, TY_IDX ty,WN *addr, WN *num_bytes)
{
  WN *wn;

  Is_True(MTYPE_is_pointer(WN_rtype(addr)) ||
          WN_rtype(addr)==MTYPE_U8 ||
	  WN_rtype(addr)==MTYPE_U4 ||
          WN_rtype(addr)==MTYPE_I8 ||
	  WN_rtype(addr)==MTYPE_I4,
          ("Bad addr in WN_CreateMload"));

#if (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77)

  addr = fe_combine_address_offset ( &offset, addr );

#endif /* (defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)) && !defined(FRONT_END_MFEF77) */

  wn = WN_CreateExp2(OPC_MLOAD,addr,num_bytes);
  WN_load_offset(wn) = offset;
  WN_set_ty(wn,ty);

  return(wn);
}


WN *
WN_CreateLdid (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
	       WN_OFFSET offset, ST_IDX st, TY_IDX ty, UINT field_id)
{
    OPCODE opc = OPCODE_make_op (opr, rtype, desc);
    WN *wn;

    Is_True (opr == OPR_LDID || opr == OPR_LDBITS,
	    ("Bad opcode in WN_CreateLdid"));
#ifdef FRONT_END
    Is_True (!((offset == 0) && (&St_Table [st] == Int32_Preg ||
				 &St_Table [st] == Int64_Preg ||
				 &St_Table [st] == Float32_Preg ||
				 &St_Table [st] == Float64_Preg)),
	     ("Preg offset 0 in WN_CreateLdid"));
#endif /* FRONT_END */
#ifdef TARG_NVISA
    if ((MTYPE_byte_size(rtype) == 8 && MTYPE_byte_size(desc) == 4)
     || (MTYPE_byte_size(rtype) == 4 && MTYPE_byte_size(desc) == 8))
    {
        // Rather than create I8I4LDID,
        // create I8I4CVT(I4I4LDID).
        // I8I4LDID can cause later problems with pregs,
        // which have same rtype and desc, so wopt sometimes loses info
        // about the implicit conversion, and we need explicit conversion
        // because we use different registers for the two sizes,
        DevWarn("insert cvt of ldid, %d %d", rtype, desc);
        wn = WN_Create(opr,desc,desc,0);
        WN_load_offset(wn) = offset;
        WN_st_idx(wn) = st;
        WN_set_ty(wn,ty);
        WN_set_field_id(wn, field_id);
        wn = WN_Cvt(desc, rtype, wn);
        return wn;
    }
#endif
    wn = WN_Create(opr,rtype,desc,0);
    WN_load_offset(wn) = offset;
    WN_st_idx(wn) = st;
    WN_set_ty(wn,ty);
    WN_set_field_id(wn, field_id);

    return(wn);
}


WN *WN_CreateLda (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
		  WN_OFFSET offset, TY_IDX ty, ST_IDX st, UINT field_id)
{
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn;

  Is_True(MTYPE_is_pointer(OPCODE_rtype(opc)),
          ("Bad addr in WN_CreateLda"));
  Is_True(OPCODE_operator(opc) == OPR_LDA,
          ("Bad opcode in WN_CreateLda"));
  wn = WN_Create(opr,rtype,desc,0);
  WN_load_offset(wn) = offset;
  WN_st_idx(wn) = st;
  WN_set_ty(wn,ty);
  WN_set_field_id(wn, field_id);

  return(wn);
}

WN *
WN_CreateIlda (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
	       WN_OFFSET offset, TY_IDX ty)
{
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn;

  Is_True(MTYPE_is_pointer(OPCODE_rtype(opc)),
          ("Bad addr in WN_CreateIlda"));
  Is_True(OPCODE_operator(opc) == OPR_ILDA,
          ("Bad opcode in WN_CreateIlda"));
  wn = WN_Create(opr,rtype,desc,0);
  WN_load_offset(wn) = offset;
  WN_set_ty(wn,ty);

  return(wn);
}


WN *WN_CreateIdname(WN_OFFSET offset, ST_IDX st)
{
  WN *wn;

  wn = WN_Create(OPC_IDNAME,  0);
  WN_idname_offset(wn) = offset;
  WN_st_idx(wn) = st;

  return(wn);
}


WN *WN_CreateConst(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, ST_IDX st)
{ 
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn; 

  Is_True(OPCODE_operator(opc) == OPR_CONST, 
	 ("Bad opcode in WN_CreateConst")); 
  wn = WN_Create(opr,rtype,desc,0); 
  WN_st_idx(wn) = st;

  return(wn);
}

WN *WN_CreateIntconst(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, INT64 const_val)
{ 
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn; 

  Is_True(OPCODE_operator(opc) == OPR_INTCONST, 
	 ("Bad opcode in WN_CreateIntconst")); 
  wn = WN_Create(opr,rtype,desc,0); 
  if (opc == OPC_U4INTCONST) {
#ifndef TARG_X8664
	/* make sure that 32-bit value is sign-extended */
	UINT32 uval = const_val;
	INT32 sval = uval;
  	WN_const_val(wn) = (INT64) sval;
#else
	/* bug 12869 make sure that high order 32 bits is zero */
  	WN_const_val(wn) = const_val & 0x0ffffffffLL;
#endif
  } else {
  	WN_const_val(wn) = const_val;
  }

  return(wn);
}

WN *WN_CreateComma(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
		   WN *block, WN *value)
{
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn;

  Is_True(OPCODE_is_expression(WN_opcode(value)),
          ("Bad value in WN_CreateComma"));
  Is_True(WN_opcode(block) == OPC_BLOCK,
          ("Bad block in WN_CreateComma"));
  wn = WN_Create(opr,rtype,desc,2); 
  WN_kid0(wn) = block;
  WN_kid1(wn) = value;

#ifdef FRONT_END
  Set_PU_has_very_high_whirl (Get_Current_PU ());
#endif /* FRONT_END */

  return(wn);
}

WN *WN_CreateRcomma(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
		    WN *value, WN *block)
{
  OPCODE opc = OPCODE_make_op (opr, rtype, desc);
  WN *wn;

  Is_True(OPCODE_is_expression(WN_opcode(value)),
          ("Bad value in WN_CreateComma"));
  Is_True(WN_opcode(block) == OPC_BLOCK,
          ("Bad block in WN_CreateComma"));
  wn = WN_Create(opr,rtype,desc,2); 
  WN_kid0(wn) = value;
  WN_kid1(wn) = block;

#ifdef FRONT_END
  Set_PU_has_very_high_whirl (Get_Current_PU ());
#endif /* FRONT_END */

  return(wn);
}

WN *WN_CreateAsm_Stmt (INT16 kid_count, char *asm_string)
{
  WN *wn = WN_Create(OPC_ASM_STMT, kid_count);

  ST *asm_st = New_ST(CURRENT_SYMTAB);
  WN_st_idx(wn) = ST_st_idx(asm_st);
  ST_Init(asm_st,
	  Str_To_Index (Save_Str(asm_string), Current_Strtab),
	  CLASS_NAME,
	  SCLASS_UNKNOWN,
	  EXPORT_LOCAL,
	  (TY_IDX) 0);

  return wn;
}

WN *WN_CreateAsm_Input (char   *constraint_string,
			UINT32  opnd_num,
			WN     *value)
{
  WN *wn = WN_Create(OPC_ASM_INPUT, 1);

  ST *constraint_st = New_ST(CURRENT_SYMTAB);
  WN_st_idx(wn) = ST_st_idx(constraint_st);
  ST_Init(constraint_st,
	  Str_To_Index (Save_Str(constraint_string), Current_Strtab),
	  CLASS_NAME,
	  SCLASS_UNKNOWN,
	  EXPORT_LOCAL,
	  (TY_IDX) 0);

  WN_kid0(wn) = value;
  WN_asm_opnd_num(wn) = opnd_num;
  return wn;
}

WN *WN_CreateComment (const char *s)
{
  WN *wn;
  wn = WN_Create(OPC_COMMENT,0);

  ST *comment_st = New_ST(CURRENT_SYMTAB);
  WN_st_idx(wn) = ST_st_idx(comment_st);
  ST_Init(comment_st,
	  Str_To_Index (Save_Str(s), Current_Strtab),
	  CLASS_NAME,
	  SCLASS_UNKNOWN,
	  EXPORT_LOCAL,
	  (TY_IDX) 0);

  return wn;
}

STR_IDX WN_GetComment (const WN *wn)
{
	Is_True(WN_opcode(wn) == OPC_COMMENT, ("Bad opcode in WN_GetComment"));
	return ST_name_idx(WN_st(wn));
}

WN *WN_CopyNode (const WN* src_wn)
{
    WN* wn;
    OPCODE opcode = WN_opcode(src_wn);

    if (src_wn == NULL) return NULL;
    wn = WN_Create (opcode, WN_kid_count(src_wn));

    WN_Copy_u1u2 (wn, src_wn);
    WN_Copy_u3 (wn, src_wn);
#if defined(TARG_SL)
    //vbuf_offset
    WN_Set_is_internal_mem_ofst(wn, WN_is_internal_mem_ofst(src_wn)); 
    //fork_joint
    WN_Set_is_compgoto_para(wn, WN_is_compgoto_para(src_wn));
    WN_Set_is_compgoto_for_minor(wn, WN_is_compgoto_for_minor(src_wn));
#endif 

    WN_set_field_id(wn, WN_field_id(src_wn));

    if (opcode == OPC_REGION && WN_ereg_supp(src_wn) != (INITO_IDX) 0) {
	const INITO& src_ino = Inito_Table[WN_ereg_supp (src_wn)];
	const ST *st = Copy_ST (INITO_st (src_ino));
	WN_ereg_supp(wn) = New_INITO (st, src_ino.val);
    }

    if (OPCODE_has_next_prev(opcode)) {
	WN_linenum(wn) = WN_linenum(src_wn);
    }
#ifdef KEY // bug 10105
    if (WN_kid_count(src_wn) == 3)
      WN_kid(wn, 2) = WN_kid(src_wn, 2);
#endif

    // trace copy wn node here
    Trace_Wn_Copy(wn, src_wn);
    return(wn);
}


void IPA_WN_Move_Maps_PU (WN_MAP_TAB *src, WN_MAP_TAB *dst, WN *wn)
{
  INT32 i;
  OPERATOR_MAPCAT category = OPCODE_mapcat(WN_opcode(wn));
  INT32 old_map_id = WN_map_id(wn);
  UINT c;

  if (old_map_id == WN_MAP_UNDEFINED)
    return;

  WN_MAP_Add_Free_List(src, wn);
  WN_set_map_id(wn, WN_MAP_UNDEFINED);
  WN_MAP_Set_ID(dst,wn);

  /* iterate through the mappings */
  /* Note that we transfer all the maps that are live, not just the
     reserved ones. This is because we want to have all the maps when
     we start processing the child PU (Preopt and LNO are not run
     on the child again to recreate the maps so we save them here.) */
  for (i = 0; i < WN_MAP_MAX; i++) {
    if (src->_is_used[i] && !WN_MAP_Get_dont_copy(i)) {

      if (!dst->_is_used[i]) { /* Create the destination */
        dst->_is_used[i] = TRUE;
	for (c = 0; c < WN_MAP_CATEGORIES; c++) {
	  dst->_map_size[c][i] = 0;
	  dst->_mapping[c][i] = NULL;
	}
	dst->_pool[i] = src->_pool[i];
	dst->_kind[i] = src->_kind[i];
      }

      switch (src->_kind[i]) {
      case WN_MAP_KIND_VOIDP: {
        if (old_map_id < src->_map_size[category][i]) {
	  IPA_WN_MAP_Set(dst, i, wn, src->_mapping[category][i][old_map_id]);
        }
	break;
      }
      case WN_MAP_KIND_INT32: {
        if (old_map_id < src->_map_size[category][i]) {
	  IPA_WN_MAP32_Set(dst, i, wn, 
		((INT32*) src->_mapping[category][i])[old_map_id]);
        }
	break;
      }
      case WN_MAP_KIND_INT64: {
        if (old_map_id < src->_map_size[category][i]) {
	  IPA_WN_MAP64_Set(dst, i, wn, 
		((INT64*) src->_mapping[category][i])[old_map_id]);
        }
	break;
      }
      default:
	Is_True(FALSE, ("IPA_WN_Move_Maps_PU: unknown map kind"));
      }
    }
  }

}


INT32
WN_Size_and_StartAddress (WN *wn, void **StartAddress)
{
    if (OPCODE_has_next_prev(WN_opcode(wn))) {
	*StartAddress = (void *)&(WN_prev(wn));
	return sizeof(WN) + sizeof(mUINT64) +
		(MAX (2, WN_kid_count(wn)) * sizeof(WN*));
    } else {
	*StartAddress = (void *) wn;
	return sizeof(WN) + MAX(0, WN_kid_count(wn) - 2) * sizeof(WN*);
    }
	
}


WN *WN_CreateLoopInfo (WN *induction, WN *trip, UINT16 trip_est, UINT16 depth, INT32 flags)
{
  WN *wn;
  INT16 nkids = 0;
  if (induction != NULL) nkids++;
  if (trip != NULL) nkids++;
  Is_True(!(induction==NULL && trip != NULL), 
	("trip must be null if induction is null in WN_CreateLoopInfo"));

  wn = WN_Create (OPC_LOOP_INFO, nkids);
  WN_loop_trip_est(wn) = trip_est;
  WN_loop_depth(wn) = depth;
  WN_loop_flag(wn) = flags;
  if (induction) WN_set_loop_induction(wn, induction);
  if (trip) WN_set_loop_trip(wn, trip);
  return wn;
}

WN *WN_CreateExcScopeBegin (INT32 id, INT16 nkids, INITO_IDX ereg_supp)
{
  WN *wn;
  wn = WN_Create (OPC_EXC_SCOPE_BEGIN, nkids);
  WN_ereg_supp(wn) = ereg_supp;
  WN_offset(wn) = id;
  return wn;
}

WN *WN_CreateExcScopeEnd (INT32 id)
{
  WN *wn;
  wn = WN_Create (OPC_EXC_SCOPE_END, 0);
  WN_offset(wn) = id;
  return wn;
}

WN *WN_CreateBarrier (BOOL forward, INT16 nkids)
{
  WN *wn;
  if (forward)
	wn = WN_Create (OPC_FORWARD_BARRIER, nkids);
  else
	wn = WN_Create (OPC_BACKWARD_BARRIER, nkids);
  return wn;
}

WN *WN_CreateTrap (INT32 value)
{
  WN *wn;
  wn = WN_Create (OPC_TRAP, 0);
  WN_offset(wn) = value;
  return wn;
}

WN *WN_CreateAssert (INT32 value, WN *condition)
{
  WN *wn;
  wn = WN_Create (OPC_ASSERT, 1);
  WN_kid(wn,0) = condition;
  WN_offset(wn) = value;
  return wn;
}

WN * WN_Zerocon (TYPE_ID ty)
{

  if (MTYPE_type_class(ty)&MTYPE_CLASS_INTEGER)
    return WN_Intconst(ty, (INT64) 0);
  else
    return Make_Const(Targ_Conv(ty, Host_To_Targ (MTYPE_I4, 0)));
}

WN *WN_Tas(TYPE_ID rtype, TY_IDX ty, WN *l)
{
  WN	*tas;

  tas = WN_CreateExp1(OPR_TAS, rtype, MTYPE_V, l);
  /* The simplifier may have set this to something other than a TAS,
     so check before setting the TY */
  if (WN_operator_is(tas, OPR_TAS)) {
     WN_set_ty(tas, ty);
  }

  return tas;
}

/* ---------------------------------------------------------------------
 * 
 * WN *WN_Create_Intrinsic(OPCODE opc, INTRINSIC intrinsic, INT32 n, WN *kids[])
 *
 *  Create an intrinsic node, calling the simplifier if necessary
 * ---------------------------------------------------------------------
 */
WN *WN_Create_Intrinsic(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc,
			INTRINSIC intrinsic, INT32 n, WN *kids[])
{
   OPCODE opc = OPCODE_make_op (opr, rtype, desc);
   WN *call;
   INT32 i;

   call = WN_SimplifyIntrinsic(opc, intrinsic, n, kids);
   if (!call) {
      call = WN_Create(opr, rtype, desc, n);
      WN_intrinsic(call) = intrinsic;
      for (i=0; i<n; i++) {
	 WN_kid(call,i) = kids[i];
      }
   }
  else {
    /* Parent pointer (if it exists) for returned node must be NULL */
    if (WN_SimpParentMap != WN_MAP_UNDEFINED) {
      WN_MAP_Set(WN_SimpParentMap, call, NULL);
    }
  }

   return call;
}

WN *WN_CreateParm(TYPE_ID rtype, WN *parm_node, TY_IDX ty, UINT32 flag)
{
    OPCODE op = OPCODE_make_op(OPR_PARM, rtype, MTYPE_V);
    WN *wn;

    if ( parm_node != NULL ) {
       if (op != OPC_VPARM) {
	  Is_True( OPCODE_is_expression(WN_opcode(parm_node)),
		  ("Bad parm_node in WN_CreateParm"));
	  Is_True( Types_Are_Compatible (rtype, parm_node ),
		  ("rtype and parm_node's rtype are different in WN_CreateParm"));
       }
       wn = WN_CreateExp1(op, parm_node);
    }
    else {
	Is_True( rtype == MTYPE_V,
		("non-VOID type in WN_CreateParm with null parm_node") );
	wn = WN_CreateExp0(op);
    }
    WN_set_ty(wn, ty);
    WN_set_flag(wn, flag);
    return wn;
}

WN *WN_Intconst(TYPE_ID rtype, INT64 value) 
{
  return WN_CreateIntconst(OPR_INTCONST, rtype, MTYPE_V, value);
}

WN *WN_Ldid(TYPE_ID desc, WN_OFFSET offset, ST_IDX sym, TY_IDX align, 
	    UINT field_id)
{
  TYPE_ID rtype= Mtype_comparison(desc);

  return WN_CreateLdid (OPR_LDID, rtype, desc, offset, sym, align, field_id);
}

WN *
WN_RLdid (TYPE_ID rtype, TYPE_ID desc, WN_OFFSET offset, ST_IDX sym,
	  TY_IDX align) 
{
  return WN_CreateLdid (OPR_LDID, rtype, desc, offset, sym, align);
}

WN *WN_LdidPreg(TYPE_ID desc, WN_OFFSET pregno)
{
  ST	*preg = MTYPE_To_PREG(desc);

  return WN_Ldid(desc, pregno, preg, ST_type(preg));
}

WN *
WN_Stid (TYPE_ID desc, WN_OFFSET offset, ST* sym, TY_IDX align, WN *value,
	 UINT field_id)
{
  return WN_CreateStid(OPR_STID, MTYPE_V, desc, offset, sym, align, value, 
		       field_id);
}

WN *WN_StidIntoPreg(TYPE_ID desc, WN_OFFSET offset, ST* preg, WN *value)
{
  return WN_CreateStid(OPR_STID, MTYPE_V, desc, offset, preg, ST_type(preg), value);
}


/*
 *	These are much needed higher level routines built on the WN_Create
 *	to build WN. Most of them determine the opcode based on type information
 *	supplied.
 *
 *	For a more detailed description, see wn.h
 */


WN *WN_Iload(TYPE_ID desc, WN_OFFSET offset, TY_IDX align, WN *addr, 
	     UINT field_id)
{
    TY_IDX palign;
    TYPE_ID rtype= Mtype_comparison(desc);
    palign = Make_Pointer_Type (align);

    return WN_CreateIload (OPR_ILOAD, rtype, desc, offset, align, palign, addr,
			   field_id);
}

WN *
WN_RIload (TYPE_ID rtype, TYPE_ID desc, WN_OFFSET offset, TY_IDX align,
           WN *addr, UINT field_id)
{
  TY_IDX palign;
  palign = Make_Pointer_Type(align);

  return WN_CreateIload (OPR_ILOAD, rtype, desc, offset, align, palign, addr,
  			 field_id);
}

WN *
WN_Istore (TYPE_ID desc, WN_OFFSET offset, TY_IDX align, WN *addr, WN *value,
	   UINT field_id)
{
  return WN_CreateIstore (OPR_ISTORE, MTYPE_V, desc, offset, align, value, addr,
			  field_id);
}

WN *WN_Unary(OPERATOR opr, TYPE_ID rtype, WN *l)
{
  return WN_CreateExp1(opr, rtype, MTYPE_V, l);
}

WN *WN_Binary(OPERATOR opr, TYPE_ID rtype, WN *l, WN *r)
{
  return WN_CreateExp2(opr, rtype, MTYPE_V, l, r);
}

WN *WN_Ternary(OPERATOR opr, TYPE_ID rtype, WN *kid0, WN *kid1, WN *kid2)
{
  return WN_CreateExp3(opr, rtype, MTYPE_V, kid0, kid1, kid2);
}

WN *
WN_IloadLdid (TYPE_ID desc, WN_OFFSET offset, TY_IDX align, ST* sym,
	      WN_OFFSET symOffset) 
{
  WN	*ldid = WN_Ldid(Pointer_type, symOffset, sym, ST_type(sym));

  return WN_Iload(desc, offset, align, ldid);
}

WN *WN_Cvt(TYPE_ID desc, TYPE_ID rtype, WN *l)
{
  return WN_CreateExp1(OPR_CVT, rtype, desc, l);
}

WN *WN_Trunc(TYPE_ID desc, TYPE_ID rtype, WN *l)
{
  return WN_CreateExp1(OPR_TRUNC, rtype, desc, l);
}

WN *WN_Rnd(TYPE_ID desc, TYPE_ID rtype, WN *l)
{
  return WN_CreateExp1(OPR_RND, rtype, desc, l);
}

WN *WN_Ceil(TYPE_ID desc, TYPE_ID rtype, WN *l)
{
  return WN_CreateExp1(OPR_CEIL, rtype, desc, l);
}

WN *WN_Floor(TYPE_ID desc, TYPE_ID rtype, WN *l)
{
  return WN_CreateExp1(OPR_FLOOR, rtype, desc, l);
}

WN *WN_Relational(OPERATOR opr, TYPE_ID rtype, WN *l, WN *r)
{
  return WN_CreateExp2(opr, Boolean_type, rtype, l, r);
}

WN *WN_ConstPowerOf2( TYPE_ID type, INT32 n)
{

  switch(type)
  {
  case MTYPE_F4:
  case MTYPE_F8:
  case MTYPE_F10:
  case MTYPE_F16:
  case MTYPE_FQ:
  case MTYPE_C4:
  case MTYPE_C8:
  case MTYPE_C10:
  case MTYPE_CQ:
    {
      double val = pow( 2.0, n);
	
      return Make_Const (Host_To_Targ_Float (type, val));
    }
  default:              /* integral constant    */
    {
      INT64	one= 1;
      UINT64	val= one << n;
      Is_True(((0<=n) && (n <= 63)), ("invalid power of 2"));
      return WN_Intconst(type, val);
    }
  }

}

WN *WN_Floatconst( TYPE_ID type, double value)
{

  switch(type)
  {
  case MTYPE_F4:
  case MTYPE_F8:
  case MTYPE_F10:
  case MTYPE_FQ:
  case MTYPE_F16:
  case MTYPE_C4:
  case MTYPE_C8:
  case MTYPE_C10:
  case MTYPE_CQ:
    return Make_Const (Host_To_Targ_Float (type, value));
#ifdef TARG_X8664
  case MTYPE_V16F4:
    return Make_Const (Create_Simd_Const (type, 
					  Host_To_Targ_Float (MTYPE_F4, 
							      value )));
  case MTYPE_V16F8:
    return Make_Const (Create_Simd_Const (type, 
					  Host_To_Targ_Float (MTYPE_F8, 
								value )));
#endif
  }
  Is_True(FALSE, ("expected floating const type"));
  return NULL;
}

WN *WN_UVConst( TYPE_ID type)
{
  switch(type)
  {
  case MTYPE_I1:
  case MTYPE_U1:
    return WN_Intconst( Mtype_TransferSign(type, MTYPE_I4), 0x5a);
  case MTYPE_I2:
  case MTYPE_U2:
    return WN_Intconst( Mtype_TransferSign(type, MTYPE_I4), 0x5a5a);
  case MTYPE_I4:
  case MTYPE_U4:
#ifdef KEY
    return WN_Intconst(type, 0xffa5a5a5);		// Signalling NaN
#else
    return WN_Intconst(type, 0xfffa5a5a);
#endif
  case MTYPE_I8:
  case MTYPE_U8:
#ifdef KEY
    return WN_Intconst(type, 0xfff5a5a5fff5a5a5ll);	// Signalling NaN
#else
    return WN_Intconst(type, 0xfffa5a5afffa5a5all);
#endif
  case MTYPE_F4:
  case MTYPE_F8:
  case MTYPE_F10:
  case MTYPE_FQ:
  case MTYPE_F16:
  case MTYPE_C4:
  case MTYPE_C8:
  case MTYPE_C10:
  case MTYPE_CQ:
#ifdef TARG_X8664
  case MTYPE_V8I1:
  case MTYPE_V8I2:
  case MTYPE_M8I1:
  case MTYPE_M8I2:
  case MTYPE_V8I4:
  case MTYPE_V8I8:
  case MTYPE_M8I4:
#endif
    return Make_Const (Host_To_Targ_UV(type));
  case MTYPE_STR:
  case MTYPE_M:
  case MTYPE_V:
  case MTYPE_B:
    break;
  }

  Is_True(FALSE, ("expected Uninitialized Variable const type"));
  return NULL;
}

#define	NBITMASK(x)		((1ll<<(x))-1)
WN *WN_RotateIntconst(WN *tree, INT32 rotate)
{
  TYPE_ID	type=	WN_rtype(tree);
  INT32		size=	MTYPE_size_reg(type);
  UINT64	n=	WN_const_val(tree);
  UINT64 	t;

  Is_True(WN_operator_is(tree, OPR_INTCONST), ("expected INTCONST"));

  rotate=	rotate % size;

  if (rotate>0)
  {
    UINT64 t=	n & NBITMASK(rotate);

    n >>=	rotate;
    n &=	NBITMASK(size-rotate);
    n =		n | (t << (size - rotate));
    WN_const_val(tree)=	n;
  }
  else if (rotate < 0)
  {
    rotate=	-rotate;

    t =		n & ~NBITMASK(size - rotate);
    n <<=	rotate;
    n &=	NBITMASK(rotate);
    n =		n | (t >> (size - rotate));
    WN_const_val(tree)=	n;
  }

  return tree;
}

WN *WN_Inverse(TYPE_ID type, WN *tree)
{
 /*
  * there are no integer recips.
  * there are no quad emulations for recip either
  */
  if (MTYPE_float(type))
  {
    if (MTYPE_is_quad(type)==FALSE && Recip_Allowed == TRUE)
    {
      return WN_Recip(type, tree);
    } 
    return WN_Div(type, WN_Floatconst(type, 1.0), tree);
  }
  return WN_Div(type, WN_Intconst(type, 1), tree);
}

WN *
WN_Lda (TYPE_ID rtype, WN_OFFSET ldaOffset, ST* sym, UINT field_id)
{
    TY_IDX pty;


    TY_IDX ty;

    if (ST_class (sym) == CLASS_BLOCK)
      ty = Make_Align_Type (MTYPE_To_TY(rtype), STB_align(sym));
    else
      ty = ST_class (sym) == CLASS_FUNC ? ST_pu_type (sym) : ST_type(sym);
    Is_True (ty != 0, ("WN_lda(): NULL ty"));
    pty = Make_Pointer_Type (ty);


    return WN_CreateLda(OPR_LDA, rtype, MTYPE_V, ldaOffset, pty, sym, field_id);
}

WN *
WN_LdaLabel (TYPE_ID rtype, INT32 label_number)
{
  WN *wn;
  TY_IDX ty;
  ty = Make_Pointer_Type (Be_Type_Tbl (MTYPE_V));
  wn = WN_Create(OPR_LDA_LABEL, rtype, MTYPE_V, 0);
  WN_label_number (wn) = label_number;
  WN_set_ty (wn, ty);
  return wn;
} /* WN_LdaLabel */

WN *WN_Icall(TYPE_ID rtype, TYPE_ID desc, INT32 n, TY_IDX ty)
{
  WN	*call;

  call = WN_Create(OPR_ICALL, rtype, desc, n);
  WN_set_ty(call,ty);

  return call;
}

/* ---------------------------------------------------------------------
 * 
 * WN *WN_generic_call(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, INT32 n, ST *sym)
 *
 * generic for macros
 *	WN_Call
 *	WN_Piccall
 *
 * ---------------------------------------------------------------------
 */
WN *
WN_generic_call (OPERATOR opr, TYPE_ID rtype, TYPE_ID desc, INT32 n,
		 ST_IDX sym) 
{
  WN		*call;

  if (MTYPE_is_complex(rtype))
    rtype = Mtype_complex_to_real(rtype);

  if (MTYPE_is_complex(desc))
    desc = Mtype_complex_to_real(desc);

  call =	WN_Create(opr, rtype, desc, n);
  WN_st_idx(call) = sym;

  return call;
}

void WN_CopyMap(WN *dst, WN_MAP map, const WN *src)
{
  if ( map == WN_MAP_UNDEFINED )
    return;

  switch ( Current_Map_Tab->_kind[map] ) {
  case WN_MAP_KIND_VOIDP:
    WN_MAP_Set( map, dst, WN_MAP_Get( map, src ) );
    break;
  case WN_MAP_KIND_INT32:
    WN_MAP32_Set( map, dst, WN_MAP32_Get( map, src ) );
    break;
  case WN_MAP_KIND_INT64:
    WN_MAP64_Set( map, dst, WN_MAP64_Get( map, src ) );
    break;
  default:
    Is_True( FALSE, ("WN_CopyMap: unknown map kind") );
  }
}


/*----------------------------------------------------------------------
 * WN_Int_Type_Conversion( WN *wn, TYPE_ID to_type )
 *
 * returns original wn if no conversion is necessary
 *         otherwise returns a CVT or CVTL WN as appropriate.  
 *----------------------------------------------------------------------
 */

extern WN *
WN_Int_Type_Conversion( WN *wn, TYPE_ID to_type )
{
  /* infer the "from" type from the given whirl */
  TYPE_ID from_type = WN_rtype(wn);

  Is_True( from_type == MTYPE_I1 ||
	   from_type == MTYPE_I2 ||
	   from_type == MTYPE_I4 ||
	   from_type == MTYPE_I8 ||
	   from_type == MTYPE_U1 ||
	   from_type == MTYPE_U2 ||
	   from_type == MTYPE_U4 ||
	   from_type == MTYPE_U8,
      ("WN_Int_Type_Conversion: bad from_type: %d\n", from_type) );

  /* quickie check */
  if ( from_type == to_type )
    return wn;

  /* pretty long-winded way to do it, but it's cleaner and faster than
   * any other way I could figure out.
   */
  switch ( to_type ) {
  case MTYPE_I1:
    switch ( from_type ) {
    case MTYPE_I1:
      return wn;
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
      return WN_CreateCvtl( OPC_I4CVTL, 8, wn );
    case MTYPE_I8:
    case MTYPE_U8:
      return WN_CreateCvtl( OPC_I8CVTL, 8, wn );
    } /* end to_type = I1 */
  case MTYPE_I2:
    switch ( from_type ) {
    case MTYPE_I1:
    case MTYPE_I2:
      return wn;
    case MTYPE_I4:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
      return WN_CreateCvtl( OPC_I4CVTL, 16, wn );
    case MTYPE_I8:
    case MTYPE_U8:
      return WN_CreateCvtl( OPC_I8CVTL, 16, wn );
    } /* end to_type = I2 */
  case MTYPE_I4:
    switch ( from_type ) {
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
      return wn;
    case MTYPE_I8:
    case MTYPE_U8:
      return WN_Cvt( from_type, to_type, wn );
    } /* end to_type = I4 */
  case MTYPE_I8:
    switch ( from_type ) {
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
      return WN_Cvt( from_type, to_type, wn );
    case MTYPE_I8:
    case MTYPE_U8:
      return wn;
    } /* end to_type = I8 */
  case MTYPE_U1:
    switch ( from_type ) {
    case MTYPE_I1:
      return wn;
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
      return WN_CreateCvtl( OPC_U4CVTL, 8, wn );
    case MTYPE_I8:
    case MTYPE_U8:
      return WN_CreateCvtl( OPC_U8CVTL, 8, wn );
    } /* end to_type = U1 */
  case MTYPE_U2:
    switch ( from_type ) {
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_U4:
      return WN_CreateCvtl( OPC_U4CVTL, 16, wn );
    case MTYPE_U1:
    case MTYPE_U2:
      return wn;
    case MTYPE_I8:
    case MTYPE_U8:
      return WN_CreateCvtl( OPC_U8CVTL, 16, wn );
    } /* end to_type = U2 */
  case MTYPE_U4:
    switch ( from_type ) {
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
      return wn;
    case MTYPE_I8:
    case MTYPE_U8:
      return WN_Cvt( from_type, to_type, wn );
    } /* end to_type = U4 */
  case MTYPE_U8:
    switch ( from_type ) {
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
      return WN_Cvt( from_type, to_type, wn );
    case MTYPE_I8:
    case MTYPE_U8:
      return wn;
    } /* end to_type = U8 */
  default:
    FmtAssert( FALSE,
      ("WN_Int_Type_Conversion: bad to_type: %d\n", to_type) );
    return wn;
  }
}

/*----------------------------------------------------------------------
 * WN_Float_Type_Conversion( WN *wn, TYPE_ID to_type )
 *
 * returns original wn if no conversion is necessary
 *         otherwise returns a CVT as appropriate.  
 *----------------------------------------------------------------------
 */

extern WN *
WN_Float_Type_Conversion( WN *wn, TYPE_ID to_type )
{
  /* infer the "from" type from the given whirl */
  TYPE_ID from_type = WN_rtype(wn);

  Is_True( from_type == MTYPE_F4 || from_type == MTYPE_F8 ||
	   from_type == MTYPE_F10 || from_type == MTYPE_FQ,
    ("WN_Float_Type_Conversion: unexpected from_type: %d\n",from_type));
  Is_True( to_type == MTYPE_F4 || to_type == MTYPE_F8 ||
	   to_type == MTYPE_F10 || to_type == MTYPE_FQ,
    ("WN_Float_Type_Conversion: unexpected to_type: %d\n", to_type) );

  /* quickie check */
  if ( from_type == to_type )
    return wn;
  else
    return WN_Cvt( from_type, to_type, wn );
}

/*----------------------------------------------------------------------
 * WN_Type_Conversion( WN *wn, TYPE_ID to_type )
 *
 * returns original wn if no conversion is necessary
 *         otherwise returns a CVT or CVTL WN as appropriate.  
 *
 * NOTE that it may be necessary to add multiple conversions to get
 * from/to the given types, but this function handles that for you.
 *----------------------------------------------------------------------
 */

extern WN *
WN_Type_Conversion( WN *wn, TYPE_ID to_type )
{
  /* infer the "from" type from the given whirl */
  TYPE_ID from_type = WN_rtype(wn);
  BOOL from_flt,to_flt;		/* is type a floating type? */

  //OSP_434
  //special handling for vector intrinsic, CG expansion need to generated special
  //instruction queue for this CVT, can't optimization this
  if(MTYPE_is_vector(from_type))
    return wn;

  /* quickie check of no-op */
  if ( from_type == to_type )
    return wn;

  Is_True( from_type == MTYPE_I1 ||
	   from_type == MTYPE_I2 ||
	   from_type == MTYPE_I4 ||
	   from_type == MTYPE_I8 ||
	   from_type == MTYPE_U1 ||
	   from_type == MTYPE_U2 ||
	   from_type == MTYPE_U4 ||
	   from_type == MTYPE_U8 ||
	   from_type == MTYPE_F4 ||
	   from_type == MTYPE_F8 ||
	   from_type == MTYPE_F10 ||
	   from_type == MTYPE_FQ,
    ("WN_Type_Conversion: unexpected from_type: %d\n", from_type) );
  Is_True( to_type == MTYPE_I1 ||
	   to_type == MTYPE_I2 ||
	   to_type == MTYPE_I4 ||
	   to_type == MTYPE_I8 ||
	   to_type == MTYPE_U1 ||
	   to_type == MTYPE_U2 ||
	   to_type == MTYPE_U4 ||
	   to_type == MTYPE_U8 ||
	   to_type == MTYPE_F4 ||
	   to_type == MTYPE_F8 ||
	   to_type == MTYPE_F10 ||
	   to_type == MTYPE_FQ,
    ("WN_Type_Conversion: unexpected to_type: %d\n", to_type) );

  from_flt = MTYPE_is_float(from_type);
  to_flt   = MTYPE_is_float(to_type);
  
  /* to or from floating point type? */
  if ( from_flt ) {
    if ( to_flt ) {
      /* both are floating point types, so bypass heavy-duty call */
      return WN_Cvt( from_type, to_type, wn );
    }

    /* from float to int */
    if ( MTYPE_size_min(to_type) >= 32/* bits */ ) {
      /* appropriately sized target, so just do the conversion */
      return WN_Cvt( from_type, to_type, wn );
    }
    else {
      /* need to first convert to 4-byte integer with correct sign */
      TYPE_ID tmp_to_type = Mtype_TransferSign( to_type, MTYPE_I4 );
      WN *tmp_wn = WN_Cvt( from_type, tmp_to_type, wn );
      /* ok, we have a 4-byte integer, so let the all-integer
       * code handle the conversion from this 4-byte thing to the
       * ultimate type
       */
      return WN_Int_Type_Conversion( tmp_wn, to_type );
    }
  }
  else if ( to_flt ) {
    /* from int to float */
    if ( MTYPE_size_min(from_type) >= 32/* bits */ ) {
      /* appropriately sized source, so just do the conversion */
      return WN_Cvt( from_type, to_type, wn );
    }
    else {
      /* need to first convert to appropriately sized integer */
      TYPE_ID tmp_to_type = Mtype_TransferSign( to_type, MTYPE_I4 );
      WN *tmp_wn = WN_Int_Type_Conversion( wn, tmp_to_type );

      /* and then convert that to float */
      return WN_Cvt( tmp_to_type, to_type, tmp_wn );
    }
  }
  else {
    /* all integers */
    return WN_Int_Type_Conversion( wn, to_type );
  }
}


WN *WN_Iloadx(TYPE_ID rtype, TY_IDX ty, TY_IDX addr_ty, WN *base, WN *index)
{
  return WN_CreateIloadx(OPR_ILOADX, rtype, MTYPE_V, ty, addr_ty, base, index);
}


WN *WN_Istorex(TYPE_ID desc, TY_IDX ty, WN *value, WN *base, WN *index)
{
  return WN_CreateIstorex(OPR_ISTOREX, MTYPE_V, desc, ty, value, base, index);
}

WN *WN_LdaString(const char *str, WN_OFFSET ldaOffset, INT32 len)
{
  TCON	tc;
  ST	*st;

  tc = Host_To_Targ_String(MTYPE_STRING, str, len);
  st = Gen_String_Sym(&tc, MTYPE_To_TY(MTYPE_STRING), FALSE);
  return WN_Lda (Pointer_type, ldaOffset, st);
}


WN*
WN_CreateAffirm (WN* condition)
{
  WN* wn;
  wn = WN_Create (OPR_AFFIRM, MTYPE_V, MTYPE_V, 1);
  WN_kid0(wn) = condition;
  return wn;
} /* WN_CreateAffirm */

WN*
WN_CreateAlloca (WN* size)
{
  WN* wn;
  wn = WN_Create (OPR_ALLOCA, Pointer_Mtype, MTYPE_V, 1);
  WN_kid0(wn) = size;
  return wn;
} /* WN_CreateAlloca */

WN*
WN_CreateDealloca (INT32 nkids)
{
  WN* wn;
  wn = WN_Create (OPR_DEALLOCA, MTYPE_V, MTYPE_V, nkids);
  return wn;
} /* WN_CreateDealloca */

WN*
WN_CreateLdma (TYPE_ID rtype, WN_OFFSET offset, TY_IDX ty, ST_IDX st)
{
  WN *wn;

  wn = WN_Create (OPR_LDMA, rtype, MTYPE_V, 0);
  WN_load_offset(wn) = offset;
  WN_st_idx(wn) = st;
  WN_set_ty(wn,ty);

  return(wn);
} /* WN_CreateLdma */


// Traverse the tree and set the address saved bits appropriately.

void
WN_set_st_addr_saved (WN* wn)
{
  ST* st;

  switch (WN_operator (wn)) {

    case OPR_LDA:
    case OPR_LDMA:

      st = WN_st(wn);

      if ( ST_class(st) == CLASS_VAR || ST_class(st) == CLASS_FUNC )
        Set_ST_addr_saved(st);
      break;

    case OPR_ARRAY:

      WN_set_st_addr_saved (WN_kid0(wn));
      break;

    case OPR_LDID:
    case OPR_CONST:
    case OPR_ILOAD:
    case OPR_MLOAD:
    case OPR_INTCONST:
    case OPR_INTRINSIC_OP:
    case OPR_CALL:
    case OPR_EQ:
    case OPR_NE:
    case OPR_GT:
    case OPR_GE:
    case OPR_LT:
    case OPR_LE:
    case OPR_ALLOCA:
#ifdef KEY
    case OPR_PURE_CALL_OP:
#endif

      break;

    case OPR_EVAL:
    case OPR_TAS:
    case OPR_CVT:
    case OPR_CVTL:
    case OPR_NEG:
    case OPR_ABS:
    case OPR_SQRT:
    case OPR_REALPART:
    case OPR_IMAGPART:
    case OPR_PAREN:
    case OPR_RND:
    case OPR_TRUNC:
    case OPR_CEIL:
    case OPR_FLOOR:
    case OPR_BNOT:
    case OPR_LNOT:
    case OPR_LOWPART:
    case OPR_HIGHPART:
    case OPR_MINPART:
    case OPR_MAXPART:
    case OPR_RECIP:
    case OPR_RSQRT:
    case OPR_PARM:
    case OPR_OPTPARM:
#ifdef TARG_X8664
    case OPR_ATOMIC_RSQRT:
#endif

      WN_set_st_addr_saved (WN_kid0(wn));
      break;

    case OPR_CSELECT:

      WN_set_st_addr_saved (WN_kid1(wn));
      WN_set_st_addr_saved (WN_kid2(wn));
      break;

    case OPR_SELECT:
    case OPR_ADD:
    case OPR_SUB:
    case OPR_MPY:
    case OPR_DIV:
    case OPR_MOD:
    case OPR_REM:
    case OPR_DIVREM:
    case OPR_MAX:
    case OPR_MIN:
    case OPR_MINMAX:
    case OPR_BAND:
    case OPR_BIOR:
    case OPR_BXOR:
    case OPR_BNOR:
    case OPR_LAND:
    case OPR_LIOR:
    case OPR_SHL:
    case OPR_ASHR:
    case OPR_LSHR:
    case OPR_COMPLEX:
    case OPR_HIGHMPY:

      WN_set_st_addr_saved (WN_kid0(wn));
      WN_set_st_addr_saved (WN_kid1(wn));
      break;

    case OPR_CAND:
    case OPR_CIOR:

      break;

    case OPR_COMMA:

      WN_set_st_addr_saved (WN_kid1(wn));
      break;

    case OPR_RCOMMA:

      WN_set_st_addr_saved (WN_kid0(wn));
      break;

#ifdef KEY
    case OPR_COMPOSE_BITS:

      WN_set_st_addr_saved (WN_kid0(wn));
      WN_set_st_addr_saved (WN_kid1(wn));
      break;

    case OPR_EXTRACT_BITS:

      WN_set_st_addr_saved (WN_kid0(wn));
      break;
#endif /* KEY */

#ifdef TARG_X8664
    case OPR_REPLICATE:
    case OPR_SHUFFLE:

      WN_set_st_addr_saved (WN_kid0(wn));
      break;
#endif // TARG_X8664

    default:

      Fail_FmtAssertion ("WN_set_st_addr_saved not implemented for %s",
                         OPERATOR_name (WN_operator (wn)));
      break;
  }
} /* WN_set_st_addr_saved */

// Traverse the tree and check whether it has side effects.

BOOL
WN_has_side_effects (const WN* wn)
{
  switch (WN_operator (wn)) {

    case OPR_ABS:
    case OPR_BNOT:
    case OPR_CEIL:
    case OPR_CVT:
    case OPR_CVTL:
    case OPR_FIRSTPART:
    case OPR_FLOOR:
    case OPR_HIGHPART:
    case OPR_LOWPART:
    case OPR_LNOT:
    case OPR_MAXPART:
    case OPR_MINPART:
    case OPR_NEG:
    case OPR_PAREN:
    case OPR_PARM:
    case OPR_OPTPARM:
    case OPR_RND:
    case OPR_RSQRT:
    case OPR_RECIP:
    case OPR_SECONDPART:
    case OPR_SQRT:
    case OPR_TAS:
    case OPR_TRUNC:
    case OPR_EXTRACT_BITS:
#ifdef TARG_X8664
    case OPR_REDUCE_ADD:
    case OPR_REDUCE_MAX:
    case OPR_REDUCE_MIN:
    case OPR_REDUCE_MPY:
    case OPR_SHUFFLE:
    case OPR_REPLICATE:
    case OPR_ATOMIC_RSQRT:
#endif // TARG_X8664
#ifdef KEY
    case OPR_ILDA:
#endif // KEY

      return WN_has_side_effects (WN_kid0(wn));

    case OPR_ADD:
    case OPR_ASHR:
    case OPR_BAND:
    case OPR_BIOR:
    case OPR_BXOR:
    case OPR_BNOR:
    case OPR_DIV:
    case OPR_DIVREM:
    case OPR_EQ:
    case OPR_GT:
    case OPR_GE:
    case OPR_HIGHMPY:
    case OPR_LAND:
    case OPR_LE:
    case OPR_LIOR:
    case OPR_LSHR:
    case OPR_LT:
    case OPR_MAX:
    case OPR_MIN:
    case OPR_MINMAX:
    case OPR_MOD:
    case OPR_MPY:
    case OPR_NE:
    case OPR_PAIR:
    case OPR_REM:
    case OPR_SHL:
    case OPR_SUB:
    case OPR_XMPY:
    case OPR_COMPOSE_BITS:
#ifdef KEY
    case OPR_RROTATE:
#endif

      if (WN_has_side_effects (WN_kid0(wn)))
        return TRUE;

      return WN_has_side_effects (WN_kid1(wn));

    case OPR_SELECT:
    case OPR_MADD:
    case OPR_MSUB:
    case OPR_NMADD:
    case OPR_NMSUB:
#ifdef KEY
    case OPR_TRIPLET:
#endif

      if (WN_has_side_effects (WN_kid0(wn)))
        return TRUE;

      if (WN_has_side_effects (WN_kid1(wn)))
        return TRUE;

      return WN_has_side_effects (WN_kid2(wn));

    case OPR_ARRAY:
 
      if (WN_has_side_effects (WN_kid0(wn)))
        return TRUE;

      else {

        INT32 n = (WN_kid_count (wn)) >> 1;

        for (INT32 i = n + 1; i <= 2 * n; i++) {

          if (WN_has_side_effects (WN_kid (wn, i)))
            return TRUE;
        }

        return FALSE;
      }

#ifdef KEY
    case OPR_PURE_CALL_OP:
#endif
    case OPR_INTRINSIC_OP: {

      INT32 n = WN_kid_count (wn);

      for (INT32 i = 0; i < n; i++) {

        if (WN_has_side_effects (WN_kid (wn, i)))
          return TRUE;
      }

      return FALSE;
    }

    case OPR_LDID:
    case OPR_LDBITS:

      if (TY_is_volatile (WN_ty(wn)) ||
          TY_is_volatile(WN_object_ty(wn)))
        return (TRUE);

      return FALSE;

    case OPR_ILOAD:
    case OPR_ILOADX:
    case OPR_ILDBITS:

      if (TY_is_volatile (WN_ty(wn)) || TY_is_volatile(WN_load_addr_ty(wn)))
        return (TRUE);

      return WN_has_side_effects (WN_kid0(wn));

    case OPR_MLOAD:

      if (TY_is_volatile (WN_ty(wn)))
        return (TRUE);

      if (WN_has_side_effects (WN_kid0(wn)))
        return TRUE;

      return WN_has_side_effects (WN_kid1(wn));

    case OPR_CONST:
    case OPR_IDNAME:
    case OPR_INTCONST:
    case OPR_LDA:
    case OPR_LDMA:
#ifdef KEY
    case OPR_LDA_LABEL:
#endif

      return FALSE;

    case OPR_ALLOCA:

      return TRUE;

    case OPR_CAND:
    case OPR_CIOR:

      return TRUE;

    case OPR_COMMA:
    case OPR_RCOMMA:

      return TRUE;

    case OPR_CSELECT:

      return TRUE;

#ifdef KEY
  case OPR_ASM_INPUT:
    return TRUE;

  case OPR_ARRAYEXP:
  case OPR_ARRSECTION:
    return TRUE;

  case OPR_WHERE:
    return TRUE;

  case OPR_IO:
  case OPR_IO_ITEM:
    return TRUE;

  case OPR_STID:
  case OPR_ISTBITS:
  case OPR_ISTORE:
  case OPR_ISTOREX:
  case OPR_STBITS:
  case OPR_MSTORE:
    return TRUE;
#endif // KEY

    default:
      Fail_FmtAssertion ("WN_has_side_effects not implemented for %s",
                         OPERATOR_name (WN_operator (wn)));

      return FALSE;
  }
} /* WN_has_side_effects */

// Check whether given WHIRL is an executable statement.
BOOL
WN_is_executable(WN * wn)
{
  switch (WN_operator(wn)) {
  case OPR_PRAGMA:
  case OPR_LABEL:
    break;
  default:
    return TRUE;
  }

  return FALSE;
}

WN *
WN_Rrotate (TYPE_ID desc, WN *src, WN *cnt)
{
  Set_PU_has_very_high_whirl (Get_Current_PU ());
  return WN_CreateExp2 (OPR_RROTATE, Mtype_comparison (desc), desc, src, cnt);
} /* WN_Rotate */

#if !defined(FRONT_END_C)  &&  !defined(IR_TOOLS) && defined(TARG_SL)
BOOL WN_Intrinsic_OP_Slave (WN *wn) {
    if (WN_operator(wn) == OPR_INTRINSIC_OP) {
	INTRINSIC ins = WN_intrinsic(wn);
	 if (INTRN_is_slave(ins)) 
	 	return TRUE;
    }
    return FALSE;	
}
#endif

// Query whether wn represents a bit operation.
BOOL
WN_is_bit_op(WN * wn)
{
  switch (WN_operator(wn)) {
  case OPR_BAND:
  case OPR_BXOR:
  case OPR_BNOT:
  case OPR_BIOR:
  case OPR_BNOR:
    return TRUE;
  default:
    ;
  }
  return FALSE;
}

// Get bit position of the TRUE bit for an integer constant WHIRL if its value is a power of 2.
// Return -1 if the value is not a power of 2.
int
WN_get_bit_from_const(WN * wn)
{
  OPERATOR opr = WN_operator(wn);
  FmtAssert((opr == OPR_INTCONST), ("Expect an integer constant"));

  INT64 val = WN_const_val(wn);
  int count = 0;
  int bit_pos = 0;
  int bit_true;

  while (val > 0) {
    if ((val & 0x1) == 1) {
      count++;
      bit_true = bit_pos;
    }
      
    val >>= 1;
    bit_pos++;
  }

  if (count == 1)
    return bit_true;

  return -1;
}

// Get bit position of the TRUE bit for an integral expression if its value is a power of 2.
// Return NULL if the value is not a power of 2 or if we can't tell.
// This routine does not process constants. Use WN_get_bit_from_const for constants.
WN *
WN_get_bit_from_expr(WN * wn)
{
  OPERATOR opr = WN_operator(wn);
  FmtAssert((opr != OPR_INTCONST), ("Do not expect an integer constant"));

  if (opr == OPR_CVT) {
    WN * wn_kid = WN_kid(wn, 0);
    if (WN_operator(wn_kid) != OPR_INTCONST)
      return WN_get_bit_from_expr(wn_kid);
  }
  else if (opr == OPR_SHL) {
    WN * wn_tmp = WN_kid(wn, 0);

    if ((WN_operator(wn_tmp) == OPR_INTCONST)
	&& (WN_const_val(wn_tmp) == 1)) 
      return WN_kid(wn, 1);
  }

  return NULL;
}

// Query whether wn has a value that is a power of 2.
// Return FALSE if the value is not a power of 2 or if we can't tell.
BOOL
WN_is_power_of_2(WN * wn)
{
  OPERATOR opr = WN_operator(wn);

  if (opr == OPR_INTCONST) {
    if (WN_get_bit_from_const(wn) >= 0)
      return TRUE;
  }
  else if (WN_get_bit_from_expr(wn)) 
    return TRUE;

  return FALSE;
}

// Match pattern:
//  a = a  bit-op  ( 1 << b)
// where bit-op is a bit operation.
//
// If matched, Return the RHS WHIRL tree "a bit-op ( 1 << b)",
WN *
WN_get_bit_reduction(WN * wn)
{
  OPERATOR opr = WN_operator(wn);

  if (OPERATOR_is_store(opr)) {
    WN * wn_data = WN_kid(wn, 0);
    WN * wn_addr = NULL;

    if (!OPERATOR_is_scalar_store(opr))
      wn_addr = WN_kid(wn, 1);

    if (WN_is_bit_op(wn_data)) {
      WN * wn_tmp = WN_kid(wn_data, 0);
      opr = WN_operator(wn_tmp);
      
      if (OPERATOR_is_load(opr)) {
	if (!OPERATOR_is_scalar_load(opr)) {
	  if (wn_addr && (WN_Simp_Compare_Trees(WN_kid(wn_tmp, 0), wn_addr) == 0)) {
	    wn_tmp = WN_kid(wn_data, 1);
	    if (wn_tmp && WN_is_power_of_2(wn_tmp))
	      return wn_data;
	  }
	}
	else if ((wn_addr == NULL) && (WN_st_idx(wn_tmp) == WN_st_idx(wn))
		 && WN_offset(wn_tmp) == WN_offset(wn)) {
	  wn_tmp = WN_kid(wn_data, 1);
	  if (wn_tmp && WN_is_power_of_2(wn_tmp))
	    return wn_data;
	}
      }
    }
  }
  
  return NULL;
}

// Check whether wn1 and wn2 have values whose difference is a constant.
// Return FALSE if the difference is not a constant or if we can't tell.
// Return the difference of wn1 and wn2 in 'diff'.
BOOL
WN_has_const_diff(WN * wn1, WN * wn2, int * diff)
{
  if (WN_Simp_Compare_Trees(wn1, wn2) == 0) {
    if (diff)
      *diff = 0;
    return TRUE;
  }

  OPERATOR opr1 = WN_operator(wn1);
  OPERATOR opr2 = WN_operator(wn2);

  if ((opr1 == opr2) && (opr1 == OPR_INTCONST)) {
    if (diff)
      *diff = (WN_const_val(wn1) - WN_const_val(wn2));
    return TRUE;
  }
  else {
    if ((opr1 == OPR_ADD) || (opr1 == OPR_SUB)) {
      WN * kid1 = WN_kid(wn1, 1);
      if (WN_operator(kid1) == OPR_INTCONST) {
	if (WN_has_const_diff(WN_kid(wn1, 0), wn2, diff)) {
	  if (opr1 == OPR_ADD) {
	    if (diff)
	      *diff = *(diff) + WN_const_val(kid1);
	  }
	  else {
	    if (diff)
	      *diff = (*diff) - WN_const_val(kid1);
	  }
	  return TRUE;
	}
      }
    }
     
    if ((opr2 == OPR_ADD) || (opr2 == OPR_SUB)) {
      WN * kid1 = WN_kid(wn2, 1);
      if (WN_operator(kid1) == OPR_INTCONST) {
	if (WN_has_const_diff(WN_kid(wn2, 0), wn1, diff)) {
	  if (opr2 == OPR_ADD) {
	    if (diff)
	      *diff = -1 * (*diff) - WN_const_val(kid1);
	  }
	  else {
	    if (diff)
	      *diff = -1 * (*diff) + WN_const_val(kid1);
	  }
	  return TRUE;
	}
      }
    }
  }

  return FALSE;
}

// Check whether two loops have compatible iteration spaces:
// 1. Difference of the lower bounds is a constant.
// 2. Difference of the upper bounds is a constant.
// 3. Loop increment is the same.
// 4. End-tests's operators are compatible.
// 
// If 'lower_diff' is not NULL, return difference of lower bound in it.
// If 'upper_diff' is not NULL, return difference of upper bound in it.
// If 'do_eq' is TRUE, End-tests' operators must be equal to be considered as compatible.
BOOL WN_has_compatible_iter_space(WN * wn1, WN * wn2, int *lower_diff, int* upper_diff, BOOL do_eq)
{
  OPCODE ub_compare;
  WN * lower_bound = WN_LOOP_LowerBound(wn1);
  WN * upper_bound = WN_LOOP_UpperBound(wn1, &ub_compare, TRUE);
  BOOL is_incr;
  WN * loop_step = WN_LOOP_Increment(wn1, &is_incr);
  OPCODE cmp_ub_compare;
  WN * cmp_lower_bound = WN_LOOP_LowerBound(wn2);
  WN * cmp_upper_bound = WN_LOOP_UpperBound(wn2, &cmp_ub_compare, TRUE);
  BOOL cmp_is_incr;
  WN * cmp_loop_step = WN_LOOP_Increment(wn2, &cmp_is_incr);
  int adjustment = 0;

  if (ub_compare != cmp_ub_compare) {
    OPERATOR opr1 = OPCODE_operator(ub_compare);
    OPERATOR opr2 = OPCODE_operator(cmp_ub_compare);
    BOOL is_compatible = FALSE;

    if (!do_eq) {
      switch (opr1) {
      case OPR_LT:
      case OPR_LE:
	if ((opr2 == OPR_LE) || (opr2 == OPR_LT))
	  is_compatible = TRUE;
	break;
      case OPR_GE:
      case OPR_GT:
	if ((opr2 == OPR_GE) || (opr2 == OPR_GT))
	  is_compatible = TRUE;
	break;
      default:
	break;
      }
    }
    else if (opr1 != opr2)
      is_compatible = FALSE;

    if (!is_compatible)
      return FALSE;

    if (is_incr) {
      if ((opr1 == OPR_LT) || (opr1 == OPR_GT))
	adjustment = -1;
      if ((opr2 == OPR_LT) || (opr2 == OPR_GT))
	adjustment = 1;
    }
    else {
      if ((opr1 == OPR_LT) || (opr1 == OPR_GT))
	adjustment = 1;
      if ((opr2 == OPR_LT) || (opr2 == OPR_GT))
	adjustment = -1;
    }
  }
	    
  if ((is_incr == cmp_is_incr)
      && lower_bound && cmp_lower_bound
      && upper_bound && cmp_upper_bound
      && (WN_Simp_Compare_Trees(loop_step, cmp_loop_step) == 0)
      && WN_has_const_diff(lower_bound, cmp_lower_bound, lower_diff)
      && WN_has_const_diff(upper_bound, cmp_upper_bound, upper_diff)) {
    if (upper_diff)
      (*upper_diff) += adjustment;
    return TRUE;
  }

  return FALSE;
}

// Find containing loop of 'wn' whose index's symbol is identical to 'index',
// 'parent_map' gives a map of a WHIRL node to its parent node.
WN *
WN_find_loop_by_index(WN * wn, ST * index, WN_MAP parent_map)
{
  if (index != NULL) {
    WN * wn_iter = wn;
    while (wn_iter) {
      if (WN_operator(wn_iter) == OPR_DO_LOOP) {
	WN * wn_ind = WN_index(wn_iter);
	ST * st2 = WN_st(wn_ind);
	if (index == st2)
	  return wn_iter;
      }
      wn_iter = (WN *) WN_MAP_Get(parent_map, wn_iter);
    }
  }

  return NULL;
}

// bug 8581
// determine if two WHIRL statements are identical; only handle store statements
// for now
BOOL
Identical_stmt(WN *stmt1, WN *stmt2)
{
  if (stmt1 == NULL || stmt2 == NULL)
    return FALSE;
  if (WN_opcode(stmt1) != WN_opcode(stmt2))
    return FALSE;
  switch(WN_operator(stmt1)) {
  case OPR_STID:
    if (WN_store_offset(stmt1) != WN_store_offset(stmt2))
      return FALSE;
    if (WN_field_id(stmt1) != WN_field_id(stmt2))
      return FALSE;
    if (WN_desc(stmt1) != WN_desc(stmt2))
      return FALSE;
    if (WN_st_idx(stmt1) != WN_st_idx(stmt2))
      return FALSE;
    return WN_Simp_Compare_Trees(WN_kid0(stmt1), WN_kid0(stmt2)) == 0;
  case OPR_STBITS:
    if (WN_store_offset(stmt1) != WN_store_offset(stmt2))
      return FALSE;
    if (WN_bit_offset(stmt1) != WN_bit_offset(stmt2))
      return FALSE;
    if (WN_bit_size(stmt1) != WN_bit_size(stmt2))
      return FALSE;
    if (WN_st_idx(stmt1) != WN_st_idx(stmt2))
      return FALSE;
    return WN_Simp_Compare_Trees(WN_kid0(stmt1), WN_kid0(stmt2)) == 0;
  case OPR_ISTORE:
    if (WN_store_offset(stmt1) != WN_store_offset(stmt2))
      return FALSE;
    if (WN_field_id(stmt1) != WN_field_id(stmt2))
      return FALSE;
    if (WN_desc(stmt1) != WN_desc(stmt2))
      return FALSE;
    if (WN_Simp_Compare_Trees(WN_kid0(stmt1), WN_kid0(stmt2)) != 0)
      return FALSE;
    return WN_Simp_Compare_Trees(WN_kid1(stmt1), WN_kid1(stmt2)) == 0;
  case OPR_ISTBITS:
    if (WN_store_offset(stmt1) != WN_store_offset(stmt2))
      return FALSE;
    if (WN_bit_offset(stmt1) != WN_bit_offset(stmt2))
      return FALSE;
    if (WN_bit_size(stmt1) != WN_bit_size(stmt2))
      return FALSE;
    if (WN_Simp_Compare_Trees(WN_kid0(stmt1), WN_kid0(stmt2)) != 0)
      return FALSE;
    return WN_Simp_Compare_Trees(WN_kid1(stmt1), WN_kid1(stmt2)) == 0;
  case OPR_MSTORE:
    if (WN_store_offset(stmt1) != WN_store_offset(stmt2))
      return FALSE;
    if (WN_field_id(stmt1) != WN_field_id(stmt2))
      return FALSE;
    if (WN_Simp_Compare_Trees(WN_kid0(stmt1), WN_kid0(stmt2)) != 0)
      return FALSE;
    if (WN_Simp_Compare_Trees(WN_kid1(stmt1), WN_kid1(stmt2)) != 0)
      return FALSE;
    return WN_Simp_Compare_Trees(WN_kid2(stmt1), WN_kid2(stmt2)) == 0;
  case OPR_BLOCK: {
      WN *s1 = WN_first(stmt1);
      WN *s2 = WN_first(stmt2);
      for ( ; ; ) {
	if (s1 == NULL && s2 == NULL)
	  return TRUE;
	if (! Identical_stmt(s1, s2))
	  return FALSE;
	s1 = WN_next(s1);
	s2 = WN_next(s2);
      }
    }
  default: ;
  }
  return FALSE;
}

BOOL
WN_is_assign(WN * wn)
{
  WN *wn_first = WN_first(wn);
  WN *wn_last  = WN_last(wn);
  return ((wn_first && (wn_first == wn_last) && OPERATOR_is_store(WN_operator(wn_first)))
    || WN_operator(wn) == OPR_SELECT);
}

BOOL
WN_is_assign_return(WN * wn)
{
  WN *wn_first = WN_first(wn);
  WN *wn_last  = WN_last(wn);

  if (wn_last) {
    WN *wn_last_prev = WN_prev(WN_last(wn));
    return ((WN_operator(wn_last) == OPR_RETURN) 
      && (wn_first == wn_last_prev) && OPERATOR_is_store(WN_operator(wn_first)));    
  } else {
    return FALSE;
  }  
}

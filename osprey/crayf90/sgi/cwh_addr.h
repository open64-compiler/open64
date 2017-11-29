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
 * Module: cwh_addr.h
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Export prototypes for addressing operations.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_ADDR_INCLUDED
#define CWH_ADDR_INCLUDED

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

/* Offset field as 64 bits - translate into 32 later ***FIX */

enum p_flag { p_RETURN_PARENT,  p_RETURN_SECTION } ;


extern  void cwh_addr_init_target(void) ;
extern  WN * cwh_addr_load_ST(ST *st, OFFSET_64 off, TY_IDX  ty)  ;
extern  WN * cwh_addr_load_WN(WN * wn, OFFSET_64 off, TY_IDX  ty) ;
extern  WN * cwh_addr_address_ST(ST * st,OFFSET_64 off = 0,TY_IDX ty = 0) ;
extern  void cwh_addr_store_ST(ST *st,OFFSET_64 off, TY_IDX  ty, WN *rhs);
extern  WN * cwh_addr_stid(ST *st, OFFSET_64 off, TY_IDX  ty , WN * rhs);
extern  WN * cwh_addr_ldid (ST *st, OFFSET_64 off, TY_IDX  ty) ;
extern  WN * cwh_addr_istore(WN *wn, OFFSET_64 off, TY_IDX  ty, WN * rhs) ;
extern  WN * cwh_addr_mload(WN *wt, OFFSET_64 off, TY_IDX  ty, WN * sz) ;
extern  WN * cwh_addr_mstore(WN * ad, OFFSET_64 off, TY_IDX  ty, WN * rhs) ;
extern  void cwh_addr_store_WN(WN * lhs, OFFSET_64 off, TY_IDX  ty,  WN * rhs) ;
extern  BOOL cwh_addr_is_array(WN * wn)  ;
extern  BOOL cwh_addr_is_section(WN * wn)  ;
extern WN *  cwh_addr_find_address(WN * wn) ;
extern WN *  cwh_addr_temp_section(WN * ad, TY_IDX  ty)  ;
extern WN * cwh_addr_extent(WN * lb, WN *ub, WN *str);
extern WN * cwh_addr_ubound_from_triplet(WN * triplet);
extern ST * cwh_addr_WN_ST(WN * wn);
extern void cwh_addr_nonc_util(WN **a, WN **b) ;
extern WN *  cwh_addr_find_section(WN * awn , enum p_flag flag) ;
extern  WN * cwh_addr_mk_ldid(ST *st, OFFSET_64 off, TYPE_ID bt, TY_IDX  ty);
extern BOOL cwh_addr_f90_pointer_reference(WN * addr);


typedef struct { OFFSET_64 off ; TY_IDX  type ;} FLD_det ;
extern FLD_det cwh_addr_offset(void) ;

extern OPCODE opc_lda;
extern OPCODE opc_call;
extern OPCODE opc_array;
extern OPCODE opc_section;
extern OPCODE opc_triplet;
extern OPCODE opc_pint;
extern OPCODE opc_sint;
extern TYPE_ID   cwh_addr_char_len_typeid;
extern TYPE_ID   cwh_bound_int_typeid;
extern TYPE_ID   cwh_doloop_typeid;

#define IS_ALTENTRY_TEMP(s) ((ST_class(s) == CLASS_VAR)     && \
	                     (ST_base_idx(s) != ST_st_idx(s)) && \
	                     (ST_is_return_var(ST_base(s))))      

/* Map for name information on ARRAY nodes */

extern WN_MAP array_name_map;
#define SET_ARRAY_NAME_MAP(a,m) WN_MAP_Set(array_name_map,(a),(void *) (m))
#define GET_ARRAY_NAME_MAP(a) ((char *) WN_MAP_Get(array_name_map,(a)))
    
#endif /* CWH_ADDR_INCLUDED */


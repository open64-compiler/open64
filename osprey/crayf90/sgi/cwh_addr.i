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

OPCODE opc_lda;
OPCODE opc_call;
OPCODE opc_array;
OPCODE opc_section;
OPCODE opc_triplet;
OPCODE opc_pint;
OPCODE opc_sint;

TYPE_ID   cwh_addr_char_len_typeid;
TYPE_ID   cwh_bound_int_typeid;
TYPE_ID   cwh_doloop_typeid;

/*
static OPCODE opc_ldid;
static OPCODE opc_pldid;
static OPCODE opc_padd;
static OPCODE opc_pstid;
static OPCODE opc_psub;
static OPCODE  opc_pmpy;
static OPCODE  opc_pdiv;
static OPCODE  opc_psra;
static OPCODE  opc_pload;
static OPCODE  opc_pne;
static OPCODE  opc_ptas;
static OPCODE  opc_reg_sra;
*/


OPCODE Ldid_Opcode [MTYPE_LAST + 1] = {
  OPCODE_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPCODE_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPC_I4I1LDID,   /* MTYPE_I1 */
  OPC_I4I2LDID,   /* MTYPE_I2 */
  OPC_I4I4LDID,   /* MTYPE_I4 */
  OPC_I8I8LDID,   /* MTYPE_I8 */
  OPC_U4U1LDID,   /* MTYPE_U1 */
  OPC_U4U2LDID,   /* MTYPE_U2 */
  OPC_U4U4LDID,   /* MTYPE_U4 */
  OPC_U8U8LDID,   /* MTYPE_U8 */
  OPC_F4F4LDID,   /* MTYPE_F4 */
  OPC_F8F8LDID,   /* MTYPE_F8 */
  OPCODE_UNKNOWN,    /* MTYPE_F10 */
  OPCODE_UNKNOWN,    /* MTYPE_F16 */
  OPCODE_UNKNOWN,    /* MTYPE_STR */
  OPC_FQFQLDID,   /* MTYPE_FQ */
  OPC_MMLDID,    /* MTYPE_M */
  OPC_C4C4LDID,   /* MTYPE_C4 */
  OPC_C8C8LDID,   /* MTYPE_C8 */
  OPC_CQCQLDID,   /* MTYPE_CQ */
  OPCODE_UNKNOWN     /* MTYPE_V */
};

OPCODE Stid_Opcode [MTYPE_LAST + 1] = {
  OPCODE_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPCODE_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPC_I1STID,     /* MTYPE_I1 */
  OPC_I2STID,     /* MTYPE_I2 */
  OPC_I4STID,     /* MTYPE_I4 */
  OPC_I8STID,     /* MTYPE_I8 */
  OPC_U1STID,     /* MTYPE_U1 */
  OPC_U2STID,     /* MTYPE_U2 */
  OPC_U4STID,     /* MTYPE_U4 */
  OPC_U8STID,     /* MTYPE_U8 */
  OPC_F4STID,     /* MTYPE_F4 */
  OPC_F8STID,     /* MTYPE_F8 */
  OPCODE_UNKNOWN,    /* MTYPE_F10 */
  OPCODE_UNKNOWN,    /* MTYPE_F16 */
  OPCODE_UNKNOWN,    /* MTYPE_STR */
  OPC_FQSTID,     /* MTYPE_FQ */
  OPC_MSTID,    /* MTYPE_M */
  OPC_C4STID,     /* MTYPE_C4 */
  OPC_C8STID,     /* MTYPE_C8 */
  OPC_CQSTID,     /* MTYPE_CQ */
  OPCODE_UNKNOWN     /* MTYPE_V */
};

OPCODE Load_Opcode [MTYPE_LAST + 1] = {
  OPCODE_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPCODE_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPC_I4I1ILOAD,  /* MTYPE_I1 */
  OPC_I4I2ILOAD,  /* MTYPE_I2 */
  OPC_I4I4ILOAD,  /* MTYPE_I4 */
  OPC_I8I8ILOAD,  /* MTYPE_I8 */
  OPC_U4U1ILOAD,  /* MTYPE_U1 */
  OPC_U4U2ILOAD,  /* MTYPE_U2 */
  OPC_U4U4ILOAD,  /* MTYPE_U4 */
  OPC_U8U8ILOAD,  /* MTYPE_U8 */
  OPC_F4F4ILOAD,  /* MTYPE_F4 */
  OPC_F8F8ILOAD,  /* MTYPE_F8 */
  OPCODE_UNKNOWN,    /* MTYPE_F10 */
  OPCODE_UNKNOWN,    /* MTYPE_F16 */
  OPCODE_UNKNOWN,    /* MTYPE_STR */
  OPC_FQFQILOAD,  /* MTYPE_FQ */
  OPC_MMILOAD,    /* MTYPE_M */
  OPC_C4C4ILOAD,  /* MTYPE_C4 */
  OPC_C8C8ILOAD,  /* MTYPE_C8 */
  OPC_CQCQILOAD,  /* MTYPE_CQ */
  OPCODE_UNKNOWN     /* MTYPE_V */
};



OPCODE Store_Opcode [MTYPE_LAST + 1] = {
  OPCODE_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPCODE_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPC_I1ISTORE,    /* MTYPE_I1 */
  OPC_I2ISTORE,    /* MTYPE_I2 */
  OPC_I4ISTORE,    /* MTYPE_I4 */
  OPC_I8ISTORE,    /* MTYPE_I8 */
  OPC_U1ISTORE,    /* MTYPE_U1 */
  OPC_U2ISTORE,    /* MTYPE_U2 */
  OPC_U4ISTORE,    /* MTYPE_U4 */
  OPC_U8ISTORE,    /* MTYPE_U8 */
  OPC_F4ISTORE,    /* MTYPE_F4 */
  OPC_F8ISTORE,    /* MTYPE_F8 */
  OPCODE_UNKNOWN,    /* MTYPE_F10 */
  OPCODE_UNKNOWN,    /* MTYPE_F16 */
  OPCODE_UNKNOWN,    /* MTYPE_STR */
  OPC_FQISTORE,    /* MTYPE_FQ */
  OPC_MISTORE,    /* MTYPE_M */
  OPC_C4ISTORE,    /* MTYPE_C4 */
  OPC_C8ISTORE,    /* MTYPE_C8 */
  OPC_CQISTORE,    /* MTYPE_CQ */
  OPCODE_UNKNOWN     /* MTYPE_V */
};

/*
 * NONCONTIG_BY_DIVIDE determines if we should deal with element size scaling
 * by using division (if NONCONTIG_BY_DIVIDE is defined) or by altering the array
 * element_size field (if NONCONTIG_BY_DIVIDE is not defined).
 * 
 * NONCONTIG_BY_ARRAYNC means use the non-contiguous array method.
 */

/* These are used by fei_nseq_subscr */ 
static BOOL may_be_noncontig;

/* This is used to turn on bounds checking for an access. It is set by 
 * fei_subscr_size
 */
static BOOL check_bounds_this_access;

/* test for an offset which doesn't fit in WN_OFFSET */

#define BIG_OFFSET(off) (off > 0xffffffffLL)

/* is ST reference in host symbol table? */
#define HOST_ASSOCIATED(st) ((ST_level(st) < CURRENT_SYMTAB) &&   \
 	                     (ST_class(st) == CLASS_VAR)     &&   \
	                     (ST_has_nested_ref(st)))

/* is FORMAL passed by value? */

#define BY_VALUE(t) ((TY_kind(t) == KIND_SCALAR) || \
		     ((TY_kind(t) == KIND_POINTER) && (TY_kind(TY_pointed(t)) == KIND_FUNCTION)))

/* get rank of array section node */

#define GET_ARRSECTION_RANK(a) ((WN_kid_count(a)-1)/2) ;

/* loop over axes of OPC_ARRAY/ARRSECTION */

#define FOREACH_AXIS(i,nk) for(i = 1 ; i <= nk/2 ; i++) 
#define SZ_OFF(rk) 0
#define SUB_OFF(rk) (rk/2)

/* forward references */

static WN * cwh_addr_add_bound(WN * ar, WN * sz, WN *sub);
static WN * cwh_addr_array(OPCODE op, WN * ad,  TY_IDX  ty) ;
static WN * cwh_addr_triplet(WN *lb,WN *ub,WN *str) ;
static WN * cwh_addr_zero_based(WN *sub, WN * lb);
static WN * cwh_addr_do_bounds_check(WN *sb, WN *lb, WN *ex);
static void cwh_addr_insert_bounds_check(WN *assertion, WN *ar);

static BOOL cwh_addr_is_triplet(WN * wn) ;

static  WN * cwh_addr_iload(WN *wn, OFFSET_64 off, TY_IDX  ty) ;
static  WN * cwh_addr_lda(ST * st, OFFSET_64 off, TY_IDX  ty)  ;
static  WN * cwh_addr_adjust_array(WN *wn, TY_IDX  ty) ;
static  WN * cwh_addr_nonc_recast(WN *wt, WN *wa) ;
static  void cwh_addr_access_flags(ST *, INT fg) ;
static  BOOL cwh_addr_f90_pointer_reference_ls(WN * ls);
static  W_node cwh_addr_substr_util(OFFSET_64 off,  TY_IDX  dty ) ;


/* Holds extra information about ARRAY nodes. Currently only 
 * an array name is stored there.
 */

WN_MAP array_name_map;

#define POINTER_TY (Be_Type_Tbl(Pointer_Mtype))


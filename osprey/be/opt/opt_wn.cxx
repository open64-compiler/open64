/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_wn.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_wn.cxx,v $
//
// Revision history:
//  12-SEP-94 shin - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description: Defines utilities for Optimizer
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_wn_CXX	"opt_wn.cxx"
static char *rcs_id = 	opt_wn_CXX"$Revision: 1.31 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>         // for pu_info.h
#endif /* ! defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "tracing.h"
#include "mempool.h"
#include "opt_base.h"
#include "topcode.h"

#include "wn.h"
#include "wn_util.h"
#include "pu_info.h"
#include "stab.h"
#include "w2op.h"
#include "stblock.h"
#include "region_util.h"
#include "opt_wn.h"
#include "ir_reader.h"
#include "opt_sym.h"
#include "stab.h"
#include "opt_htable.h"
#include "opt_mu_chi.h"
#include "dep_graph.h"
#include "pf_cg.h"
#include "opt_alias_interface.h"
#include "opt_points_to.h"
#include "opt_alias_rule.h"
#include "config.h"		// Alias_Pointer_Parms
#include "config_opt.h"		// for Delay_U64_Lowering
#include "opt_cvtl_rule.h"
#include "opt_main.h"
#include "wn_simp.h"


STMT_ITER::STMT_ITER(WN *f)
{
  head = cur = f;
  for (tail = f; WN_next(tail) != NULL; tail = WN_next(tail));
}

void
STMT_ITER::Print( FILE *fp )
{
  for (First(); !Is_Empty(); Next()) {
    if (OPCODE_is_scf(WN_opcode(Cur())))
      fdump_wn_no_st( fp, Cur() );
    else
      fdump_tree_no_st( fp, Cur() );
  }
}

void
STMT_CONTAINER::Insert_before(WN *me, WN *wn)
{
  // and set head to nd if me is head
  WN *p, *n;
  if (me == NULL) return;
  p = WN_prev(me); n = me;
  if (p) {
    WN_next(p) = wn;
    WN_prev(wn) = p;
  }
  WN_prev(n) = wn;
  WN_next(wn) = n;
  if (me == head) head = wn;
}

void
STMT_CONTAINER::Insert_after (WN *me, WN *wn)
{
  // and set tail to wn if me is tail
  WN *p, *n;
  if (me == NULL) return;
  p = me; n = WN_next(me);
  WN_next(p) = wn;
  WN_prev(wn) = p;
  if (n) {
    WN_prev(n) = wn;
    WN_next(wn) = n;
  }
  if (me == tail) tail = wn;
}

void
STMT_CONTAINER::Insert_lst_before(WN *me, WN *wn_f, WN *wn_l)
{
  // and set head to nd if me is head
  WN *p, *n;
  if (me == NULL) return;
  if (wn_f == NULL) {
    // make sure that the last is null if the first was
    Is_True(wn_l == NULL, 
      ("STMT_CONTAINER::Insert_lst_before: wn_l non-null") );
    return;
  }
  p = WN_prev(me); n = me;
  if (p) WN_next(p) = wn_f;
  WN_prev(wn_f) = p;
  WN_prev(n) = wn_l;
  WN_next(wn_l) = n;
  if (me == head) head = wn_f;
}

void
STMT_CONTAINER::Insert_lst_after (WN *me, WN *wn_f, WN *wn_l)
{
  // and set tail to wn if me is tail
  WN *p, *n;
  if (me == NULL) return;
  if (wn_f == NULL) {
    // make sure that the last is null if the first was
    Is_True(wn_l == NULL, 
      ("STMT_CONTAINER::Insert_lst_after: wn_l non-null") );
    return;
  }
  p = me; n = WN_next(me);
  WN_next(p) = wn_f;
  WN_prev(wn_f) = p;
  if (n) WN_prev(n) = wn_l;
  WN_next(wn_l) = n;
  if (me == tail) tail = wn_l;
}

void
STMT_CONTAINER::Remove(WN *me)
{
  // and reset head/tail if it's head/tail
  WN *p, *n;
  if (me == NULL) return;
  p = WN_prev(me); n = WN_next(me);
  if (p) WN_next(p) = n;
  if (n) WN_prev(n) = p;

  WN_prev(me) = NULL;
  WN_next(me) = NULL;
  if (me == head) 
    head = n;
  if (me == tail)
    tail = p;
}

void
STMT_CONTAINER::Print(FILE *fp)
{
  STMT_ITER stmt_iter(head, tail);
  stmt_iter.Print(fp);
}


WN *WN_copy(WN *wn)
{
  return WN_COPY_Tree(wn);
}

MTYPE Mtype_from_class_size(MTYPE t1, MTYPE t2)
// return the mtype that is of the class of t1 but the size of t2;
// if t1 is neither signed or unsigned int, just return t1
{
  Is_True(t1 != MTYPE_BS && t2 != MTYPE_BS,
  	  ("Mtype_from_class_size: MTYPE_BS not handled here."));
  if ((MTYPE_type_class(t1) & MTYPE_CLASS_UNSIGNED_INTEGER) == 0)
    return t1;
#ifdef KEY
  if (MTYPE_is_vector(t1))
    return t1;
#endif
  if (MTYPE_signed(t1))
    switch (MTYPE_size_best(t2)) {
    case 8: return MTYPE_I1;
    case 16: return MTYPE_I2;
    case 32: return MTYPE_I4;
    case 64: return MTYPE_I8;
    default: Is_True(FALSE, ("WN_mtype_from_class_size: unrecognized bit size for mtype"));
    }
  else
    switch (MTYPE_size_best(t2)) {
    case 8: return MTYPE_U1;
    case 16: return MTYPE_U2;
    case 32: return MTYPE_U4;
    case 64: return MTYPE_U8;
    default: Is_True(FALSE, ("WN_mtype_from_class_size: unrecognized bit size for mtype"));
    }
  return MTYPE_V;
}

BOOL WN_has_chi(const WN *wn, const REGION_LEVEL region_level)
{
  const OPCODE opc = WN_opcode(wn);
  if (opc == OPC_REGION) {
    Is_True(region_level > RL_UNKNOWN && region_level < RL_LAST,
	    ("WN_has_chi, region_level out of bounds"));
    RID *rid = REGION_get_rid(wn);
    Is_True(rid != NULL, ("WN_has_chi, NULL RID"));
    if (RID_TYPE_mp(rid) || RID_TYPE_eh(rid) ||
	// kludge for 7.2, see PV 457243
	region_level == RL_LNO_PREOPT || region_level == RL_PREOPT ||
	region_level == RL_IPA_PREOPT)
      return FALSE;
    else
      return TRUE;
  }
  return OPCODE_has_chi(opc);
}

BOOL OPERATOR_has_chi( OPERATOR opr )
{
  switch ( opr ) {
  case OPR_ISTORE:
  case OPR_ISTOREX:
  case OPR_ISTBITS:
  case OPR_MSTORE:
  case OPR_STID:
  case OPR_STBITS: 
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
  case OPR_IO:
  case OPR_FORWARD_BARRIER:
  case OPR_BACKWARD_BARRIER:
  case OPR_DEALLOCA:
  case OPR_OPT_CHI:
  case OPR_REGION: // black-box region only in CR or SR form
  case OPR_ASM_STMT:
    return TRUE;
  default:
    return FALSE;
  }
}

BOOL OPCODE_has_chi( OPCODE opc )
{
  return OPERATOR_has_chi(OPCODE_operator(opc));
}

// need the WN so we can tell a black box region (has mu) from a transparent
// MP region (no mu).
BOOL WN_has_mu( const WN *wn, const REGION_LEVEL region_level )
{
  const OPCODE opc = WN_opcode(wn);
  switch ( OPCODE_operator(opc) ) {
#ifdef KEY
    case OPR_ASM_STMT:
#endif
    case OPR_ILOAD:
    case OPR_ILDBITS:
    case OPR_ILOADX:
    case OPR_MLOAD:
    case OPR_CALL:
    case OPR_ICALL:
    case OPR_INTRINSIC_CALL:
    case OPR_IO:
    case OPR_RETURN:
    case OPR_RETURN_VAL:
    case OPR_FORWARD_BARRIER:
    case OPR_BACKWARD_BARRIER:
    case OPR_REGION_EXIT:
#ifdef KEY
    case OPR_GOTO_OUTER_BLOCK:
#endif
      return TRUE;
    case OPR_REGION:	// this can be a black-box or MP region
    {
      Is_True(region_level > RL_UNKNOWN && region_level < RL_LAST,
	      ("WN_has_mu, region_level out of bounds"));
      RID *rid = REGION_get_rid(wn);
      Is_True(rid != NULL, ("WN_has_mu(), NULL rid"));
      if (RID_TYPE_mp(rid) || RID_TYPE_eh(rid) ||
	  // kludge for 7.2, see PV 457243
	  region_level == RL_LNO_PREOPT || region_level == RL_PREOPT ||
	  region_level == RL_IPA_PREOPT)
	return FALSE;
      else
	return TRUE;
    }
    case OPR_PARM:
      return (WN_Parm_By_Reference(wn) || WN_Parm_Dereference(wn));
    default:
      return FALSE;
  }
}

// version for CODEREP and STMTREP - uses OPERATOR
BOOL OPERATOR_has_mu( OPERATOR opr )
{
  switch ( opr ) {
#ifdef KEY
    case OPR_ASM_STMT:
#endif
    case OPR_ILOAD:
    case OPR_ILDBITS:
    case OPR_ILOADX:
    case OPR_MLOAD:
    case OPR_CALL:
    case OPR_ICALL:
    case OPR_INTRINSIC_CALL:
    case OPR_IO:
    case OPR_RETURN:
    case OPR_RETURN_VAL:
    case OPR_FORWARD_BARRIER:
    case OPR_BACKWARD_BARRIER:
    case OPR_REGION: // black-box region only in CR or SR form
    case OPR_REGION_EXIT:
      return TRUE;
    case OPR_PARM:  // may or may not have mu
      return TRUE;
    default:
      return FALSE;
  }
}

BOOL OPCODE_has_mu( OPCODE opc )
{
  return OPERATOR_has_mu(OPCODE_operator(opc));
}

static WN_MAP  _wn_flag_map;

void WN_init_flags(MEM_POOL *pool)
{
  _wn_flag_map = WN_MAP32_Create(pool);
}

void WN_fini_flags(void) 
{
  WN_MAP_Delete(_wn_flag_map);
}

// Obtain the flags of the WN node 
//
INT32 Wn_flags(const WN *wn)
{
  return WN_MAP32_Get(_wn_flag_map, wn);
}

// Set the flags of the WN node
//
void Set_wn_flags(WN *wn, INT32 flags)
{
  WN_MAP32_Set(_wn_flag_map, wn, flags); 
}


//  LDID and STID may have their ST renamed to a VER.
//
BOOL WN_has_ver(const WN *wn)
{
  OPERATOR opr = WN_operator(wn);
  return (OPERATOR_is_scalar_load (opr) || OPERATOR_is_scalar_store (opr));
}

// The WN node with this operator need to be or has been converted to
// OPT_STAB index.
BOOL OPERATOR_has_aux(const OPERATOR opr)
{
  return (OPERATOR_is_scalar_load (opr) ||
	  OPERATOR_is_scalar_store (opr) ||
	  opr == OPR_LDA);
}

BOOL OPCODE_has_aux(const OPCODE opc)
{
  return OPERATOR_has_aux(OPCODE_operator(opc));
}

// The ST field of the WN node need to be or has been converted to
// OPT_STAB index.
extern BOOL WN_has_aux(const WN *wn)
{
  OPERATOR opr = WN_operator(wn);
  return OPERATOR_has_aux(opr);
}

ST *WN_sym(const WN *wn)
{
  Is_True((Wn_flags(wn) & WN_FLAG_ST_TYPE) == WN_ST_IS_SYM,
	  ("WN_st is not of type ST."));
  return WN_st(wn);
}


AUX_ID WN_aux(const WN *wn)
{
  Is_True((Wn_flags(wn) & WN_FLAG_ST_TYPE) == WN_ST_IS_AUX,
	  ("WN_st is not of type AUX."));
  return (AUX_ID) WN_st_idx(wn);
}


VER_ID WN_ver(const WN *wn)
{
  Is_True((Wn_flags(wn) & WN_FLAG_ST_TYPE) == WN_ST_IS_VER,
	  ("WN_st is not of type VER."));
  return (VER_ID) WN_st_idx(wn);
}


//  Keep track of the type in WN_st() field
//
void WN_set_aux(WN *wn, AUX_ID s)
{
#ifdef Is_True_On
  Set_wn_flags(wn, (Wn_flags(wn) & ~WN_FLAG_ST_TYPE) | WN_ST_IS_AUX);
#endif
  WN_st_idx(wn) = (ST_IDX) s;
}

//  Keep track of the type in WN_st() field
//
void WN_set_ver(WN *wn, VER_ID v)
{
#ifdef Is_True_On
  Set_wn_flags(wn, (Wn_flags(wn) & ~WN_FLAG_ST_TYPE) | WN_ST_IS_VER);
#endif
  WN_st_idx(wn) = (ST_IDX) v;
}

void WN_copy_stmap(WN *src, WN *dst)
{
  INT i;
  for (i = 0; i < WN_MAP_MAX; i++) { 
    if (Current_Map_Tab->_is_used[i]) {
      switch (Current_Map_Tab->_kind[i]) {
      case WN_MAP_KIND_VOIDP: {
        WN_MAP_Set(i, dst, WN_MAP_Get(i, src));
	break;
      }
      case WN_MAP_KIND_INT32: {
        WN_MAP32_Set(i, dst, WN_MAP32_Get(i, src));
	break;
      }
      case WN_MAP_KIND_INT64: {
        WN_MAP64_Set(i, dst, WN_MAP64_Get(i, src));
	break;
      }
      default:
	Is_True(FALSE, ("WN_copy_stmap: unknown map kind"));
      }
    }
  }
  if (!OPCODE_is_leaf(WN_opcode(src)))
    for (i = 0; i < WN_kid_count(src); i++)
      WN_copy_stmap(WN_kid(src, i), WN_kid(dst, i));
}


// Determine the register class of the WN node
INT32 Get_mtype_class(MTYPE mtype)
{
  INT32 mclass = MTYPE_type_class(mtype);
  // strip off unsigned-ness
  if (mclass == MTYPE_CLASS_UNSIGNED)
    mclass = MTYPE_CLASS_INTEGER;

  if (mclass == MTYPE_CLASS_UNSIGNED_INTEGER)
    mclass = MTYPE_CLASS_INTEGER;
  return mclass;
}

// ====================================================================
// Get a LDID opcode given an mtype.  This function assumes the
// caller wants a register sized result that is as small as possible
// (i.e., I1 type returns I4 register, but I8 type returns I8 register)
// and has the same signedness of the given type.
// ====================================================================

extern OPCODE 
Ldid_from_mtype( MTYPE mtype )
{
  switch ( mtype ) {
    case MTYPE_I1:	return OPC_I4I1LDID;
    case MTYPE_I2:	return OPC_I4I2LDID;
    case MTYPE_I4:	return OPC_I4I4LDID;
    case MTYPE_I8:	return OPC_I8I8LDID;
    case MTYPE_U1:	return OPC_U4U1LDID;
    case MTYPE_U2:	return OPC_U4U2LDID;
    case MTYPE_U4:	return OPC_U4U4LDID;
    case MTYPE_U8:	return OPC_U8U8LDID;
    case MTYPE_F4:	return OPC_F4F4LDID;
    case MTYPE_F8:	return OPC_F8F8LDID;
#if defined(TARG_IA64) || defined(TARG_X8664)
    case MTYPE_F10:	return OPC_F10F10LDID;
#endif
    case MTYPE_FQ:	return OPC_FQFQLDID;
    case MTYPE_C4:	return OPC_C4C4LDID;
    case MTYPE_C8:	return OPC_C8C8LDID;
#if defined(TARG_IA64) || defined(TARG_X8664)
    case MTYPE_C10:	return OPC_C10C10LDID;
#endif
    case MTYPE_CQ:	return OPC_CQCQLDID;
#if defined(TARG_X8664) || defined(VECTOR_MTYPES)
    case MTYPE_V16I1:	return OPC_V16I1V16I1LDID;
    case MTYPE_V16I2:	return OPC_V16I2V16I2LDID;
    case MTYPE_V16I4:	return OPC_V16I4V16I4LDID;
    case MTYPE_V16I8:	return OPC_V16I8V16I8LDID;
    case MTYPE_V16F4:	return OPC_V16F4V16F4LDID;
    case MTYPE_V16F8:	return OPC_V16F8V16F8LDID;
    case MTYPE_V16C4:	return OPC_V16C4V16C4LDID;
    case MTYPE_V16C8:	return OPC_V16C8V16C8LDID;
    case MTYPE_V8I1:	return OPC_V8I1V8I1LDID;
    case MTYPE_V8I2:	return OPC_V8I2V8I2LDID;
    case MTYPE_V8I4:	return OPC_V8I4V8I4LDID;
    case MTYPE_V8I8:	return OPC_V8I8V8I8LDID;
    case MTYPE_V8F4:	return OPC_V8F4V8F4LDID;
    case MTYPE_M8I1:	return OPC_M8I1M8I1LDID;
    case MTYPE_M8I2:	return OPC_M8I2M8I2LDID;
    case MTYPE_M8I4:	return OPC_M8I4M8I4LDID;
    case MTYPE_M8F4:	return OPC_M8F4M8F4LDID;
    case MTYPE_V32I1:	return OPC_V32I1V32I1LDID;
    case MTYPE_V32I2:	return OPC_V32I2V32I2LDID;
    case MTYPE_V32I4:	return OPC_V32I4V32I4LDID;
    case MTYPE_V32I8:	return OPC_V32I8V32I8LDID;
    case MTYPE_V32F4:	return OPC_V32F4V32F4LDID;
    case MTYPE_V32F8:	return OPC_V32F8V32F8LDID;
    case MTYPE_V32C4:	return OPC_V32C4V32C4LDID;
    case MTYPE_V32C8:	return OPC_V32C8V32C8LDID;
#endif

    case MTYPE_B:
    case MTYPE_F16:
    case MTYPE_STRING:
    case MTYPE_M:
    case MTYPE_V:
      FmtAssert( FALSE, ("Ldid_from_mtype: bad mtype: %s",
		 Mtype_Name(mtype)) );
      return OPCODE_UNKNOWN;

    case MTYPE_UNKNOWN:
    default:
      FmtAssert( FALSE, ("Ldid_from_mtype: unknown mtype: %d", mtype) );
      return OPCODE_UNKNOWN;
  }
}

// ====================================================================
// Get an MTYPE given an mtype class and size.
// ====================================================================

extern MTYPE
Mtype_from_mtype_class_and_size( INT mtype_class, INT bytes )
{
#if defined(TARG_X8664) || defined(VECTOR_MTYPES)
  if ( mtype_class & MTYPE_CLASS_VECTOR ) {
    if ( ( mtype_class & MTYPE_CLASS_SVECTOR ) == MTYPE_CLASS_SVECTOR ) {
      if ( mtype_class & MTYPE_CLASS_INTEGER ) {
        switch ( bytes ) {
        case 1: return MTYPE_V8I1; 
        case 2: return MTYPE_V8I2; 
        case 4: return MTYPE_V8I4; 
        case 8: return MTYPE_V8I8; 
        }
      } else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
        if ( bytes == 4 )
          return MTYPE_V8F4; 
      }
    } else if ( ( mtype_class & MTYPE_CLASS_MVECTOR ) == MTYPE_CLASS_MVECTOR ) {
      if ( mtype_class & MTYPE_CLASS_INTEGER ) {
        switch ( bytes ) {
        case 1: return MTYPE_M8I1; 
        case 2: return MTYPE_M8I2; 
        case 4: return MTYPE_M8I4; 
        }
      } else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
        if ( bytes == 4 )
          return MTYPE_M8F4; 
      }
    } else if ( ( mtype_class & MTYPE_CLASS_AVECTOR ) == MTYPE_CLASS_AVECTOR ) {
      // 256-bit AVX vector
      if ( mtype_class & MTYPE_CLASS_INTEGER ) {
        switch ( bytes ) {
        case 1: return MTYPE_V32I1; 
        case 2: return MTYPE_V32I2; 
        case 4: return MTYPE_V32I4; 
        case 8: return MTYPE_V32I8; 
        }
      } else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
        switch ( bytes ) {
        case 4: return MTYPE_V32F4; 
        case 8: return MTYPE_V32F8; 
        case 16: return MTYPE_V32C8;
        }
      }
    } else // 128-bit vectors
    if ( mtype_class & MTYPE_CLASS_INTEGER ) {
      switch ( bytes ) {
      case 1: return MTYPE_V16I1; 
      case 2: return MTYPE_V16I2; 
      case 4: return MTYPE_V16I4; 
      case 8: return MTYPE_V16I8; 
      }
    } else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
      switch ( bytes ) {
      case 4: return MTYPE_V16F4; 
      case 8: return MTYPE_V16F8; 
      case 16: return MTYPE_V16C8;
      }
    }
    FmtAssert( FALSE, 
	       ("Mtype_from_mtype_class_and_size: unknown type"));
  } else
#endif
  // unsigned integer?
  if ( (mtype_class & MTYPE_CLASS_UNSIGNED) || 
       Only_Unsigned_64_Bit_Ops && ! Delay_U64_Lowering && (mtype_class & MTYPE_CLASS_INTEGER) ) {
    switch ( bytes ) {
      case 1: return MTYPE_U1;
      case 2: return MTYPE_U2;
      case 4: return MTYPE_U4;
      case 8: return MTYPE_U8;
    }
  }
  else if ( mtype_class & MTYPE_CLASS_INTEGER ) {
    switch ( bytes ) {
      case 1: return MTYPE_I1;
      case 2: return MTYPE_I2;
      case 4: return MTYPE_I4;
      case 8: return MTYPE_I8;
    }
  }
  else if ( mtype_class & MTYPE_CLASS_COMPLEX ) {
    switch ( bytes ) {
      case 8:  return MTYPE_C4;
      case 16: return MTYPE_C8;
#if defined(TARG_IA64)
      case 32: return MTYPE_C10;
#elif defined(TARG_X8664) 
      case 24:  // -m32 long double complex
      case 32: return MTYPE_C10;
#else
      case 24:  // -m32 long double complex
      case 32: return MTYPE_CQ;
#endif
    }
  }
  else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
    switch ( bytes ) {
      case 4:  return MTYPE_F4;
      case 8:  return MTYPE_F8;
#if defined(TARG_IA64)
      case 16: return MTYPE_F10;
#elif defined(TARG_X8664) 
      case 12:  // -m32 long double
      case 16: return MTYPE_F10;
#else
      case 12:
      case 16: return MTYPE_FQ;
#endif
    }
  }

  return MTYPE_UNKNOWN;
}

// ====================================================================
// Get a LDID opcode given an mtype class and size.  This function 
// assumes the caller wants a register sized result that is as small as
// possible (i.e., INTEGER with size 1 returns I4 register, but INTEGER
// with size 8 return I8 register)
// ====================================================================

extern OPCODE 
Ldid_from_mtype_class_and_size( INT mtype_class, INT bytes )
{
#if defined(TARG_X8664) || defined(VECTOR_MTYPES)
  if ( mtype_class & MTYPE_CLASS_VECTOR ) {
    if ( ( mtype_class & MTYPE_CLASS_SVECTOR ) == MTYPE_CLASS_SVECTOR ) {
      if ( mtype_class & MTYPE_CLASS_INTEGER ) {
        switch ( bytes ) {
        case 1: return OPC_V8I1V8I1LDID; 
        case 2: return OPC_V8I2V8I2LDID; 
        case 4: return OPC_V8I4V8I4LDID; 
        case 8: return OPC_V8I8V8I8LDID; 
        }
      } else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
        if ( bytes == 4 )
          return OPC_V8F4V8F4LDID; 
      }
    } else if ( ( mtype_class & MTYPE_CLASS_MVECTOR ) == MTYPE_CLASS_MVECTOR ) {
      if ( mtype_class & MTYPE_CLASS_INTEGER ) {
        switch ( bytes ) {
        case 1: return OPC_M8I1M8I1LDID; 
        case 2: return OPC_M8I2M8I2LDID; 
        case 4: return OPC_M8I4M8I4LDID; 
        }
      } else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
        if ( bytes == 4 )
          return OPC_M8F4M8F4LDID; 
      }
    } else if ( ( mtype_class & MTYPE_CLASS_AVECTOR ) == MTYPE_CLASS_AVECTOR ) {
      // AVX 256-bit vector
      if ( mtype_class & MTYPE_CLASS_INTEGER ) {
        switch ( bytes ) {
        case 1: return OPC_V32I1V32I1LDID; 
        case 2: return OPC_V32I2V32I2LDID; 
        case 4: return OPC_V32I4V32I4LDID; 
        case 8: return OPC_V32I8V32I8LDID; 
        }
      } else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
        switch ( bytes ) {
        case 4: return OPC_V32F4V32F4LDID; 
        case 8: return OPC_V32F8V32F8LDID; 
        case 16: return OPC_V32C8V32C8LDID;
        }
      } 
    } else // 128-bit vectors
    if ( mtype_class & MTYPE_CLASS_INTEGER ) {
      switch ( bytes ) {
      case 1: return OPC_V16I1V16I1LDID; 
      case 2: return OPC_V16I2V16I2LDID; 
      case 4: return OPC_V16I4V16I4LDID; 
      case 8: return OPC_V16I8V16I8LDID; 
      }
    } else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
      switch ( bytes ) {
      case 4: return OPC_V16F4V16F4LDID; 
      case 8: return OPC_V16F8V16F8LDID; 
      case 16: return OPC_V16C8V16C8LDID;
      }
    } 
    FmtAssert( FALSE, 
	       ("Ldid_from_mtype_class_and_size: unknown type"));
  } else
#endif
  // unsigned integer?
  if ( (mtype_class & MTYPE_CLASS_UNSIGNED) || 
       Only_Unsigned_64_Bit_Ops && ! Delay_U64_Lowering && (mtype_class & MTYPE_CLASS_INTEGER) ) {
    switch ( bytes ) {
      case 1: return OPC_U4U1LDID;
      case 2: return OPC_U4U2LDID;
      case 4: return OPC_U4U4LDID;
      case 8: return OPC_U8U8LDID;
    }
  }
  else if ( mtype_class & MTYPE_CLASS_INTEGER ) {
    switch ( bytes ) {
      case 1: return OPC_I4I1LDID;
      case 2: return OPC_I4I2LDID;
      case 4: return OPC_I4I4LDID;
      case 8: return OPC_I8I8LDID;
    }
  }
  else if ( mtype_class & MTYPE_CLASS_COMPLEX ) {
    switch ( bytes ) {
      case 8:  return OPC_C4C4LDID;
      case 16: return OPC_C8C8LDID;
#if defined(TARG_IA64)
      case 32: return OPC_C10C10LDID;
#elif defined(TARG_X8664)
      case 24:
      case 32: return OPC_C10C10LDID;
#else
      case 24:
      case 32: return OPC_CQCQLDID;
#endif
    }
  }
  else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
    switch ( bytes ) {
      case 4:  return OPC_F4F4LDID;
      case 8:  return OPC_F8F8LDID;
#if defined(TARG_IA64)
      case 16: return OPC_F10F10LDID;
#elif defined(TARG_X8664)
      case 12:
      case 16: return OPC_F10F10LDID;
#else
      case 12:
      case 16: return OPC_FQFQLDID;
#endif
    }
  }

  // if we get to here, it's bad news
  FmtAssert( FALSE, 
    ("Ldid_from_mtype_class_and_size: unknown class/size: %d/%d",
     mtype_class, bytes) );
  return OPCODE_UNKNOWN;
}
// ====================================================================
// Get a STID opcode given an mtype class and size.
// ====================================================================

extern OPCODE 
Stid_from_mtype_class_and_size( INT mtype_class, INT bytes )
{
#if defined(TARG_X8664) || defined(VECTOR_MTYPES)
  if ( mtype_class & MTYPE_CLASS_VECTOR ) {
    if ( ( mtype_class & MTYPE_CLASS_SVECTOR ) == MTYPE_CLASS_SVECTOR ) {
      if ( mtype_class & MTYPE_CLASS_INTEGER ) {
        switch ( bytes ) {
        case 1: return OPC_V8I1STID; 
        case 2: return OPC_V8I2STID; 
        case 4: return OPC_V8I4STID; 
        case 8: return OPC_V8I8STID; 
        }
      } else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
        if ( bytes == 4 )
          return OPC_V8F4STID; 
      }
    } else if ( ( mtype_class & MTYPE_CLASS_MVECTOR ) == MTYPE_CLASS_MVECTOR ) {
      if ( mtype_class & MTYPE_CLASS_INTEGER ) {
        switch ( bytes ) {
        case 1: return OPC_M8I1STID; 
        case 2: return OPC_M8I2STID; 
        case 4: return OPC_M8I4STID; 
        }
      } else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
        if ( bytes == 4 )
          return OPC_M8F4STID; 
      }
    } else if ( ( mtype_class & MTYPE_CLASS_AVECTOR ) == MTYPE_CLASS_AVECTOR ) {
      // 256-bit AVX vector
      if ( mtype_class & MTYPE_CLASS_INTEGER ) {
        switch ( bytes ) {
        case 1: return OPC_V32I1STID; 
        case 2: return OPC_V32I2STID; 
        case 4: return OPC_V32I4STID; 
        case 8: return OPC_V32I8STID; 
        }
      } else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
        switch ( bytes ) {
        case 4: return OPC_V32F4STID; 
        case 8: return OPC_V32F8STID; 
        case 16: return OPC_V32C8STID; 
        }
      } 
    } else // 128-bit vectors
    if ( mtype_class & MTYPE_CLASS_INTEGER ) {
      switch ( bytes ) {
      case 1: return OPC_V16I1STID; 
      case 2: return OPC_V16I2STID; 
      case 4: return OPC_V16I4STID; 
      case 8: return OPC_V16I8STID; 
      }
    } else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
      switch ( bytes ) {
      case 4: return OPC_V16F4STID; 
      case 8: return OPC_V16F8STID; 
      case 16: return OPC_V16C8STID; 
      }
    } 
    FmtAssert( FALSE, 
	       ("Stid_from_mtype_class_and_size: unknown type"));    
  } else
#endif
  // unsigned integer?
  if ( (mtype_class & MTYPE_CLASS_UNSIGNED) || 
       Only_Unsigned_64_Bit_Ops && ! Delay_U64_Lowering && (mtype_class & MTYPE_CLASS_INTEGER) ) {
    switch ( bytes ) {
      case 1: return OPC_U1STID;
      case 2: return OPC_U2STID;
      case 4: return OPC_U4STID;
      case 8: return OPC_U8STID;
    }
  }
  else if ( mtype_class & MTYPE_CLASS_INTEGER ) {
    switch ( bytes ) {
      case 1: return OPC_I1STID;
      case 2: return OPC_I2STID;
      case 4: return OPC_I4STID;
      case 8: return OPC_I8STID;
    }
  }
  else if ( mtype_class & MTYPE_CLASS_COMPLEX ) {
    switch ( bytes ) {
      case 8:  return OPC_C4STID;
      case 16: return OPC_C8STID;
#if defined(TARG_IA64)
      case 32: return OPC_C10STID;
#elif defined(TARG_X8664)
      case 24:
      case 32: return OPC_C10STID;
#else
      case 24:
      case 32: return OPC_CQSTID;
#endif
    }
  }
  else if ( mtype_class & MTYPE_CLASS_FLOAT ) {
    switch ( bytes ) {
      case 4:  return OPC_F4STID;
      case 8:  return OPC_F8STID;
#if defined(TARG_IA64)
      case 16: return OPC_F10STID;
#elif defined(TARG_X8664)
      case 12:
      case 16: return OPC_F10STID;
#else
      case 12:
      case 16: return OPC_FQSTID;
#endif
    }
  }

  // if we get to here, it's bad news
  FmtAssert( FALSE, 
    ("Stid_from_mtype_class_and_size: unknown class/size: %d/%d",
     mtype_class, bytes) );
  return OPCODE_UNKNOWN;
}


// Initialize the LNO information for the main emitter.
void
Init_lno_info_for_main_emitter(void)
{
  if (Current_Dep_Graph != NULL)
    Current_Dep_Graph->Clear_Map();
}


// Dump the dependence graph (for debugging)
void
Print_dep_graph(FILE *fp)
{
  if (Current_Dep_Graph != NULL)
    Current_Dep_Graph->Print(fp);
}


// Obtain the LNO dep graph vertex id
INT32
WN_get_dep_graph_vertex(WN *wn)
{
  if (Current_Dep_Graph != NULL)
    return Current_Dep_Graph->Get_Vertex(wn);
  else
    return 0;
}


// Detach the WN node from the LNO dep graph.
void
WN_detach_wn_from_dep_graph(INT32 vertex)
{
  if (Current_Dep_Graph != NULL && vertex != 0)
    Current_Dep_Graph->Clear_Map_For_Vertex((VINDEX16) vertex);
}


// Add LNO info the WN node.
void
WN_add_lno_info(WN *wn, CODEREP *cr)
{
  Is_True(OPCODE_is_load(WN_opcode(wn)) || OPCODE_is_store(WN_opcode(wn)),
	  ("opcode is not load/store."));
  if (Current_Dep_Graph != NULL && cr->Kind() == CK_IVAR) {
    VINDEX16 vertex = OPCODE_is_load(WN_opcode(wn)) ? 
      cr->Ivar_occ()->Lno_dep_vertex_load() :
      cr->Ivar_occ()->Lno_dep_vertex_store() ;
    if (vertex != 0 && Current_Dep_Graph != NULL) {
      if (Current_Dep_Graph->Get_Wn(vertex) == NULL)
	Current_Dep_Graph->Set_Wn(vertex, wn);
      else {
	VINDEX16 newvertex = Current_Dep_Graph->Add_Vertex(wn);
	BOOL ok = Current_Dep_Graph->Copy_Vertex(vertex, newvertex);
	if (!ok) {
	  Current_Dep_Graph->Erase_Graph();
	  Current_Dep_Graph = NULL;
	}
      }
    }
  }
  PF_POINTER *pf;
  if ((pf = cr->Ivar_occ()->Pf_pointer()) != NULL) {
    if (!VISITED_EM(pf)) {
      SET_VISITED_EM(pf);
      if (PF_PTR_wn_pref_1L(pf) != NULL) {
	PF_PTR_wn_pref_1L(pf) = ((STMTREP *) PF_PTR_wn_pref_1L(pf))->Prefetch_wn();
	WN_MAP_Set(WN_MAP_PREFETCH, PF_PTR_wn_pref_1L(pf), pf);
      }
      if (PF_PTR_wn_pref_2L(pf) != NULL) {
	PF_PTR_wn_pref_2L(pf) = ((STMTREP *) PF_PTR_wn_pref_2L(pf))->Prefetch_wn();
	WN_MAP_Set(WN_MAP_PREFETCH, PF_PTR_wn_pref_2L(pf), pf);
      }
      WN_MAP_Set(WN_MAP_PREFETCH, wn, pf);
    }
  }
}


// Need to clone the LNO dep graph vertex when we clone the WN node
void
WN_dup_dep_vertex(WN *oldwn, WN *newwn)
{
  Is_True(OPCODE_is_load(WN_opcode(oldwn)) || OPCODE_is_store(WN_opcode(oldwn)),
	  ("opcode is not load/store."));
  Is_True(OPCODE_is_load(WN_opcode(newwn)) || OPCODE_is_store(WN_opcode(newwn)),
	  ("opcode is not load/store."));
  if (Current_Dep_Graph != NULL) {
    VINDEX16 vertex = Current_Dep_Graph->Get_Vertex(oldwn);
    if (vertex != 0) {
      VINDEX16 newvertex = Current_Dep_Graph->Add_Vertex(newwn);
      BOOL ok = Current_Dep_Graph->Copy_Vertex(vertex, newvertex);
      if (!ok) {
	Current_Dep_Graph->Erase_Graph();
	Current_Dep_Graph = NULL;
      }
    }
  }
}


//  Obtain the PF pointer
PF_POINTER *WN_get_pf_pointer(WN *wn)
{
  return (PF_POINTER *) WN_MAP_Get (WN_MAP_PREFETCH, wn);
}


//  Print the content of the PF_POINTER (for debugging)
void Print_pf_pointer(FILE *fp, PF_POINTER *p)
{
  fprintf(fp, "\tpref1=0x%p trip1=%d ", PF_PTR_wn_pref_1L(p), PF_PTR_lrnum_1L(p));
  fprintf(fp, "pref2=0x%p trip2=%d\n", PF_PTR_wn_pref_2L(p), PF_PTR_lrnum_2L(p));
}


// If a STRUCT contains both volatile and non-volatile fields, then
// then the entire STRUCT should be treated at volatile for the
// purpose of DSE and DCE.  But, the STRUCT should be treated as
// non-volatile for the purpose of generating MU and CHI nodes.
// (Addition of the field_id may make this a non-issue.)


// Check if a LDID/STID contains any volatility
#ifdef KEY
#include <ext/hash_map>

struct TY_IDX_EQ
{
  bool operator() (const TY_IDX ty1, const TY_IDX ty2) const
  {
    return TY_IDX_index (ty1) == TY_IDX_index (ty2);
  }
};

static hash_map<const TY_IDX, INT, __gnu_cxx::hash<TY_IDX>, TY_IDX_EQ> TY_volatility;

#define STRUCT_HAS_VOLATILITY_COMPUTED 1
#define STRUCT_HAS_VOLATILE 2

BOOL
Lod_TY_is_volatile(TY_IDX ty)
{
  if (ty == TY_IDX_ZERO) return FALSE;
  if (TY_is_volatile(ty)) return TRUE;
  if (TY_kind(ty) == KIND_STRUCT) {
    if (TY_volatility[ty] & STRUCT_HAS_VOLATILITY_COMPUTED)
      return TY_volatility[ty] & STRUCT_HAS_VOLATILE;
    if (!TY_fld (ty).Is_Null ()) {
      FLD_ITER fld_iter = Make_fld_iter (TY_fld (ty));
      do {
	TY_IDX fld_ty = FLD_type (fld_iter);
	if (Lod_TY_is_volatile(fld_ty)) {
	  TY_volatility[ty] = STRUCT_HAS_VOLATILITY_COMPUTED | STRUCT_HAS_VOLATILE;
	  return TRUE;
        }
      } while (!FLD_last_field (fld_iter++));
      TY_volatility[ty] = STRUCT_HAS_VOLATILITY_COMPUTED;
    }
  }
  return FALSE;
}
#else
BOOL
Lod_TY_is_volatile(TY_IDX ty)
{
  if (ty == TY_IDX_ZERO) return FALSE;
  if (TY_is_volatile(ty)) return TRUE;
  if (TY_kind(ty) == KIND_STRUCT) {
    if (!TY_fld (ty).Is_Null ()) {
      FLD_ITER fld_iter = Make_fld_iter (TY_fld (ty));
      do {
	TY_IDX fld_ty = FLD_type (fld_iter);
	if (Lod_TY_is_volatile(fld_ty))
	  return TRUE;
      } while (!FLD_last_field (fld_iter++));
    }
  }
  return FALSE;
}
#endif // KEY


// Check if a ILOAD/ISTORE accesses/contains any volatility
BOOL
Ilod_TY_is_volatile(TY_IDX ty)
{
  if (ty == TY_IDX_ZERO) return FALSE;
  if (TY_is_volatile(ty)) return TRUE;
  return Lod_TY_is_volatile(TY_pointed(ty));
}


// Obtain the constant value from the WHIRL and present it in INT64
INT64
WN_get_const_val(WN *wn)
{
  Is_True(WN_operator(wn) == OPR_INTCONST,
          ("WN_get_const_val: must be OPR_INTCONST"));
  union {
    mINT64  i8;
    mUINT64 u8;
    struct {
      mINT32  hi;
      mINT32  lo;
    } i4;
    struct {
      mUINT32  uhi;
      mUINT32  ulo;
    } u4;
    struct {
      mINT16  s4;
      mINT16  s3;
      mINT16  s2;
      mINT16  s1;
    } i2;
    struct {
      mUINT16 us4;
      mUINT16 us3;
      mUINT16 us2;
      mUINT16 us1;
    } u2;
    struct {
      mINT8  b8;
      mINT8  b7;
      mINT8  b6;
      mINT8  b5;
      mINT8  b4;
      mINT8  b3;
      mINT8  b2;
      mINT8  b1;
    } i1;
    struct {
      mINT8 ub8;
      mINT8 ub7;
      mINT8 ub6;
      mINT8 ub5;
      mINT8 ub4;
      mINT8 ub3;
      mINT8 ub2;
      mINT8 ub1;
    } u1;
  } val;

  val.i8 = WN_const_val(wn);
  return val.i8;
}

// for CODEREP
UINT Actual_data_size(CODEREP *cr, OPT_STAB *opt_stab, INT &signess)
{
  signess = 0;
  MTYPE  rtype = cr->Dtyp();
  INT    actual_size;

  if ((MTYPE_type_class(rtype) & MTYPE_CLASS_INTEGER) == 0)
    return MTYPE_size_min(rtype);

  switch (cr->Kind()) {
  case CK_CONST:
    {
      INT64 val = cr->Const_val();
      if ( (val & 0xFFFFFFFFFFFFFF80ll) == 0 ) {
	signess |= (SIGN_1_EXTD | SIGN_0_EXTD);
	return 8;
      } else if ( (val & 0xFFFFFFFFFFFFFF00ll) == 0 ) {
	signess |= SIGN_0_EXTD;
	return 8;
      } else if ( (val & 0xFFFFFFFFFFFFFF80ll) == 0xFFFFFFFFFFFFFF80ll ) {
	signess |= SIGN_1_EXTD;
	return 8;
      } else if ( (val & 0xFFFFFFFFFFFF8000ll) == 0 ) {
	signess |= (SIGN_1_EXTD | SIGN_0_EXTD);
	return 16;
      } else if ( (val & 0xFFFFFFFFFFFF0000ll) == 0 ) {
	signess |= SIGN_0_EXTD;
	return 16;
      } else if ( (val & 0xFFFFFFFFFFFF8000ll) == 0xFFFFFFFFFFFF8000ll ) {
	signess |= SIGN_1_EXTD;
	return 16;
      } else if ( (val & 0xFFFFFFFF80000000ll) == 0 ) {
	signess |= (SIGN_1_EXTD | SIGN_0_EXTD);
        return 32;
      } else if ( (val & 0xFFFFFFFF00000000ll) == 0 ) {
	signess |= SIGN_0_EXTD;
	return 32;
      } else if ( (val & 0xFFFFFFFF80000000ll) == 0xFFFFFFFF80000000ll ) {
	signess |= SIGN_1_EXTD;
      	return 32;
      }
      return MTYPE_size_min(rtype);
    }
  case CK_OP:
    {
      switch ( cr->Opr() ) {
      case OPR_CVTL:
	if (MTYPE_is_signed(rtype)) signess |= SIGN_1_EXTD;
	if (MTYPE_is_unsigned(rtype)) signess |= SIGN_0_EXTD;
	return cr->Offset();
      case OPR_CVT:
	{
	  switch ( cr->Op() ) {
	  case OPC_I4I8CVT:
	  case OPC_U4I8CVT:
	  case OPC_I4U8CVT:
	  case OPC_U4U8CVT:
#ifdef TARG_X8664
	    signess |= SIGN_0_EXTD;
#else
	    signess |= SIGN_1_EXTD;
#endif
	    return MTYPE_size_min(rtype);
	  case OPC_I8U4CVT:
	  case OPC_U8U4CVT:
	    signess |= SIGN_0_EXTD;
	    return MTYPE_size_min(cr->Dsctyp());
	  default:
	    break;
	  }
	  return MTYPE_size_min(rtype);
	}
      case OPR_BAND:
	{
	  INT k0s = 0;
	  INT k1s = 0;
	  actual_size = MTYPE_size_min(rtype);
	  INT kid0_size = Actual_data_size(cr->Opnd(0), opt_stab, k0s);
	  INT kid1_size = Actual_data_size(cr->Opnd(1), opt_stab, k1s);
	  if (k0s & SIGN_0_EXTD) actual_size = MIN(actual_size, kid0_size);
	  if (k1s & SIGN_0_EXTD) actual_size = MIN(actual_size, kid1_size);
	  if (actual_size < MTYPE_size_min(rtype)) {
	    signess |= SIGN_0_EXTD;
	    return actual_size;
	  }
	  return MTYPE_size_min(rtype);
	}
      case OPR_ASHR:
      case OPR_LSHR:
	{
	  // The kid1 contains the number of bits.  
	  CODEREP *bits = cr->Opnd(1);
	  
	  // skip if kid 1 is not a constant.
	  if ( bits->Kind() == CK_CONST ) {
	    UINT bit_cnt = bits->Const_val();
	    if (MTYPE_size_min(rtype) == 32)
	      bit_cnt &= 0x1F;  // use the low 5 bits
	    else
	      bit_cnt &= 0x3F;
	    
	    INT ks = 0;
	    actual_size = Actual_data_size(cr->Opnd(0), opt_stab, ks);
	    actual_size -= bit_cnt;
	    if (actual_size < 0) actual_size = 0;
	    if (cr->Opr() == OPR_ASHR) {
	      signess |= SIGN_1_EXTD;
	    } else {
	      signess |= SIGN_0_EXTD;
	    }
	    if (actual_size > MTYPE_size_min(rtype)) // pv 364274
	      actual_size = MTYPE_size_min(rtype);
	    return actual_size;
	  }
	  break;
	}
      default:
	;
      }
      break;
    }
  case CK_VAR:
    { 
      AUX_STAB_ENTRY *aux_entry = opt_stab->Aux_stab_entry(cr->Aux_id());
      if (ST_class(aux_entry->St()) == CLASS_PREG) {
	if (aux_entry->Value_size() > 0) {
	  if (aux_entry->Is_sign_extd()) signess |= SIGN_1_EXTD;
	  if (aux_entry->Is_zero_extd()) signess |= SIGN_0_EXTD;
	  return aux_entry->Value_size();
	} else 
	  return MTYPE_size_min(rtype);
      } else {
	if (cr->Is_sign_extd()) 
	  signess |= SIGN_1_EXTD;
	else 
	  signess |= SIGN_0_EXTD;
	return aux_entry->Bit_size() ?
	    cr->Bit_size () : MTYPE_size_min(cr->Dsctyp());
      }
    }
  case CK_IVAR:
    {
      if (OPERATOR_is_scalar_iload (cr->Opr())) {
	if (cr->Is_sign_extd())
	  signess |= SIGN_1_EXTD;
	else 
	  signess |= SIGN_0_EXTD;
	return cr->Opr() == OPR_ILDBITS ?
	    cr->I_bit_size () : MTYPE_size_min(cr->Dsctyp());
      }
    }
  }

  return MTYPE_size_min(rtype);
}


// Find the type to use for Create_identity_assignment()
// Returns null if a reasonable type for a ldid/stid cannot be
// created.
//
extern TY_IDX 
Identity_assignment_type( AUX_STAB_ENTRY *sym, OPT_PHASE phase )
{
  ST     *st  = sym->St();
  TY_IDX  ty = ST_type(st);

  // The following is a heuristic:
  //  if we try to get one element of an one-dimension or multi-dimension  array,
  //  use its element type!
  //
  while (TY_kind(ty) == KIND_ARRAY && sym->Byte_size() < TY_size(ty))
    ty = TY_AR_etype(ty);

#ifdef KEY // bug 3091: avoid generating bad identity assignments for bitfields
  if (phase != MAINOPT_PHASE && sym->Field_id() != 0 && sym->Bit_size() > 0 &&
      Is_Structure_Type(ty))
    return ty;
#endif

  // the size needs to match
  if ( sym->Byte_size() != TY_size(ty) )
    return TY_IDX_ZERO;

  // if we don't have a simple type, i.e., we have struct/class,
  // determine if we can substitute a predefined type if it has
  // same characteristics (alignment,signedness,etc.)
  if ( ! Is_Simple_Type( ty ) ) {
    MTYPE mtype;
   
    if (sym->Mtype()==MTYPE_M || MTYPE_is_vector(sym->Mtype()))
    	mtype = sym->Mtype();
    else {
        mtype = Mtype_from_mtype_class_and_size(sym->Mclass(),
						  sym->Byte_size());
#ifdef KEY // bug 8186
	if (MTYPE_is_unsigned(sym->Mtype()))
	  mtype = Mtype_TransferSign(MTYPE_U4, mtype);
#endif
    }

    if ( mtype == MTYPE_UNKNOWN )
      return TY_IDX_ZERO;

    TY_IDX newty = MTYPE_To_TY(mtype);

    // check alignment of replaced type
    if ( TY_align(ty) == TY_align(newty) )
      ty = newty;
    else
      return TY_IDX_ZERO;
  }

  return ty;
}


// Create an assignment of the form "i = i" for the symbol
//
WN *
Create_identity_assignment(AUX_STAB_ENTRY *sym, AUX_ID aux_id, TY_IDX ty)
{
  ST          *st  = sym->St();
  OPCODE ldidop;
  OPCODE stidop;

  if (sym->Mtype() == MTYPE_M) {
     ldidop = OPCODE_make_op(OPR_LDID, sym->Mtype(), sym->Mtype());
     stidop = OPCODE_make_op(OPR_STID, MTYPE_V, sym->Mtype());
  } else {
#if defined(TARG_X8664) || defined(VECTOR_MTYPES)
    TYPE_ID type = sym->Mtype();
    INT bytes = sym->Byte_size();
    switch (type) {
    case MTYPE_M8I1:
    case MTYPE_V8I1:
    case MTYPE_V16I1:
    case MTYPE_V32I1: bytes = 1; break;
    case MTYPE_M8I2:
    case MTYPE_V8I2:
    case MTYPE_V16I2:
    case MTYPE_V32I2: bytes = 2; break;
    case MTYPE_M8I4:
    case MTYPE_M8F4:
    case MTYPE_V8I4:
    case MTYPE_V8F4:
    case MTYPE_V16I4:
    case MTYPE_V16F4:
    case MTYPE_V32I4:
    case MTYPE_V32F4: bytes = 4; break;
    case MTYPE_V8I8:
    case MTYPE_V16I8:
    case MTYPE_V16F8:
    case MTYPE_V32I8:
    case MTYPE_V32F8: bytes = 8; break;
    case MTYPE_V16C8:
    case MTYPE_V32C8: bytes = 16; break;
    }
    ldidop = Ldid_from_mtype_class_and_size(sym->Mclass(), bytes);
    stidop = Stid_from_mtype_class_and_size(sym->Mclass(), bytes);
    if (MTYPE_is_unsigned(sym->Mtype())) { // bug 11732
      ldidop = OPCODE_make_op(OPR_LDID,
                              Mtype_TransferSign(MTYPE_U4,OPCODE_rtype(ldidop)),
                              Mtype_TransferSign(MTYPE_U4,OPCODE_desc(ldidop)));
      stidop = OPCODE_make_op(OPR_STID, MTYPE_V,
                              Mtype_TransferSign(MTYPE_U4,OPCODE_desc(ldidop)));
    }
#else
     ldidop = Ldid_from_mtype_class_and_size(sym->Mclass(), sym->Byte_size());
     stidop = Stid_from_mtype_class_and_size(sym->Mclass(), sym->Byte_size());
#endif
  }

  WN *rhs = WN_CreateLdid( ldidop, sym->St_ofst(), st, ty );
  WN *copy = WN_CreateStid( stidop, sym->St_ofst(), st, ty, rhs);

  if (sym->Bit_size () > 0) {
    if (sym->Field_id() == 0) { 
      WN_change_operator (rhs, OPR_LDBITS);
      WN_set_bit_offset_size (rhs, sym->Bit_ofst (), sym->Bit_size ());
  
      WN_change_operator (copy, OPR_STBITS);
      WN_set_bit_offset_size (copy, sym->Bit_ofst (), sym->Bit_size ());
    }
    else { // if field id != 0, then it is MTYPE_BS, not LD_BITS
      WN_set_desc(rhs, MTYPE_BS);
      WN_set_desc(copy, MTYPE_BS);
#ifdef KEY // bug 3091: get field id, offset field and ty from a good WN
      	   // 		(the offset field in opt_stab is not what we want)
      WN_set_field_id(rhs, WN_field_id(sym->Wn()));
      WN_load_offset(rhs) = WN_offset(sym->Wn());
      WN_set_ty(rhs, WN_ty(sym->Wn()));
      WN_set_field_id(copy, WN_field_id(sym->Wn()));
      WN_load_offset(copy) = WN_offset(sym->Wn());
      WN_set_ty(copy, WN_ty(sym->Wn()));
#endif
    }
  }
  WN_set_aux(rhs, aux_id);
  WN_set_aux(copy, aux_id);
  return copy;
}  



//  Obtain the mod-ref information from the uplevel procedure variable list
//
READ_WRITE 
Get_MP_modref(const WN *pragma_list, const POINTS_TO *pt,
	      const ALIAS_RULE *rule)
{
  Warn_todo("Get_MP_modref:  need to reimplement once the problem is fully understood.");

  POINTS_TO pt2;
  ST *st, *base;
  mINT64 ofst;

  // Fix 366737.
  // Hack to make mp-x6.f work!  Assume all parameter-addr are required by a MP-call.
  // This won't be necessary once we changed the F-param strategy after MR.
  if (IS_FORTRAN && pt->Const() && pt->Base_is_fixed() && ST_sclass(pt->Base()) == SCLASS_FORMAL)
    return READ;
  
  for (WN *wn = WN_first(pragma_list); wn != NULL; wn = WN_next(wn)) {
    Is_True(WN_pragma(wn) == WN_PRAGMA_ACCESSED_ID, ("Get_MP_modref:  not WN_PRAGMA_ACCESSED_ID."));
    st = WN_st(wn);
    if (st != NULL) {
      if ((WN_pragma_arg2(wn) & (ACCESSED_LOAD|ACCESSED_STORE)) != 0) {
	pt2.Init();
	pt2.Set_expr_kind(EXPR_IS_ADDR);
	pt2.Set_ofst_kind(OFST_IS_FIXED);
	pt2.Set_base_kind(BASE_IS_FIXED);
	Expand_ST_into_base_and_ofst(st, 0, &base, &ofst);
	pt2.Set_base(base);
	pt2.Set_byte_ofst(ofst);
	pt2.Set_byte_size(TY_size(ST_type(st)));
	pt2.Set_bit_ofst_size(0, 0);
	pt2.Set_named();
	// Fix 366737.
	// Hack to make mp-x6.f work!  Assume all parameter-addr are required by a MP-call.
	// This won't be necessary once we changed the F-param strategy after MR.
	if (rule->Aliased_Memop(pt, &pt2)) {
	  // hack to make it work!
	  if (IS_FORTRAN && ST_sclass(st) == SCLASS_FORMAL) 
	    return READ;
	  else
	    return READ_AND_WRITE;
	}
      }
      if ((WN_pragma_arg2(wn) & (ACCESSED_ILOAD|ACCESSED_ISTORE)) != 0) {
	pt2.Init();
	pt2.Set_expr_kind(EXPR_IS_ADDR);
	pt2.Set_ofst_kind(OFST_IS_FIXED);
	pt2.Set_base_kind(BASE_IS_FIXED);
	if (IS_FORTRAN && ST_sclass(st) == SCLASS_FORMAL && Alias_Pointer_Parms) {
	  pt2.Set_F_param();
	  pt2.Set_based_sym(st);
	  pt2.Set_base_kind(BASE_IS_UNKNOWN);
	  pt2.Set_global();
	  pt2.Set_named();   // For the Ragnarok option
	}
	if (rule->Aliased_Memop(pt, &pt2)) {
	  return READ_AND_WRITE;
	}
      }
    }
  }
  return NO_READ_NO_WRITE;
}


// Obtain the accessed id list for the nested procedure
//
WN *
Get_MP_accessed_id_list(const ST *st)
{
  PU_Info *pu_info;
  for (pu_info = PU_Info_child(Current_PU_Info);
       pu_info != NULL;
       pu_info = PU_Info_next(pu_info)) {
    if (PU_Info_proc_sym(pu_info) == ST_st_idx(st) &&
	PU_Info_state(pu_info, WT_TREE) == Subsect_InMem)
      return WN_func_varrefs(PU_Info_tree_ptr(pu_info));
  }
  return NULL;
}

// Check if we allow this opcode to be copy-propagated
BOOL 
Op_can_be_propagated(OPCODE op, OPT_PHASE phase)
{
  // Do not copy propagate things that LNO cannot handle in dependency tests
  // in preopt
  // TODO: need to remove this code after LNO is fixed to handle it in 
  // dependency analysis
  if (phase != MAINOPT_PHASE && !WOPT_Enable_Copy_Prop_LNO_Ops &&
      MTYPE_IS_INTEGER(OPCODE_rtype(op)) &&
      (OPCODE_operator(op) == OPR_DIV || OPCODE_operator(op) == OPR_REM)) 
    return FALSE;

  OPERATOR opr = OPCODE_operator(op);
  if (!WOPT_Enable_CSE_FP_comparison &&
      (opr == OPR_EQ || opr == OPR_NE ||
       opr == OPR_LT || opr == OPR_LE || 
       opr == OPR_GT || opr == OPR_GE) &&
      MTYPE_is_float(OPCODE_desc(op)))
    return FALSE;

  return TRUE;
}

// tell if the region has the given pragma
extern BOOL
Is_region_with_pragma(WN *wn, WN_PRAGMA_ID pragma_id)
{
  wn = WN_region_pragmas(wn);
  STMT_ITER stmt_iter;
  WN *stmt;

  //  Iterate through each statement
  FOR_ALL_ELEM (stmt, stmt_iter, Init(WN_first(wn), WN_last(wn))) {
    if (WN_operator(stmt) == OPR_PRAGMA &&
        (WN_pragma(stmt) == pragma_id))
      return TRUE;
  }
  return FALSE;
}


BOOL Is_hi_sign_extended(MTYPE result_ty, MTYPE desc_ty)
{
  Is_True(MTYPE_is_integral(result_ty),
          ("Is_hi_sign_extended: handles integral type only"));
#ifdef TARG_NVISA
  // This routine is trying to deal with the case of having a 32bit value
  // in a 64bit register, so need to make sure the upper 32bits are properly
  // sign extended.  This is for architectures that use 64bit registers for
  // all values.  For PTX we have separate registers for 32 and 64bit 
  // values, with explicit converts between them, so don't need to implicitly 
  // sign-extend 32bit.  In fact, doing so causes problems because
  // the convert is not just a sign-extension, is a move, and we want 
  // sizes to match (e.g. multifunc was generating I8I4CVT(U8U4LDID),
  // where U8 LDID puts value in b64 reg, whereas I8I4CVT moves from b32 
  // to b64).  An alternative would be to allow sign-extension within
  // b64 registers, thus keeping values in the larger regs, but that
  // wastes register space.
  if (MTYPE_size_min(desc_ty) == MTYPE_size_min(MTYPE_I4)
   && MTYPE_size_min(result_ty) == MTYPE_size_min(MTYPE_I4))
    return FALSE; // both sizes are 32bit, no need to sign-extend.
#endif
#ifndef TARG_X8664
  if (MTYPE_size_min(desc_ty) < MTYPE_size_min(result_ty) &&
      (MTYPE_size_min(result_ty) == MTYPE_size_min(MTYPE_I4) ||
       MTYPE_is_signed(result_ty)))
    return TRUE;

  if (MTYPE_is_signed(result_ty)) return TRUE;

  if (MTYPE_size_min(result_ty) == MTYPE_size_min(MTYPE_I4)) return TRUE;

  return FALSE;
#else
  if (MTYPE_size_min(desc_ty) < MTYPE_size_min(result_ty) &&
      (MTYPE_size_min(result_ty) == MTYPE_size_min(MTYPE_I4) ||
       ! MTYPE_is_signed(result_ty)))
    return FALSE;

  if (! MTYPE_is_signed(result_ty)) return FALSE;

  if (MTYPE_size_min(result_ty) == MTYPE_size_min(MTYPE_I4)) return FALSE;

  return TRUE;
#endif
}

BOOL Is_lo_sign_extended(MTYPE result_ty, MTYPE desc_ty)
{
  Is_True(MTYPE_is_integral(result_ty),
          ("Is_lo_sign_extended: handles integral type only"));
#ifndef TARG_X8664
  if (MTYPE_size_min(desc_ty) < MTYPE_size_min(result_ty) &&
      MTYPE_is_signed(desc_ty))
    return TRUE;

  if (MTYPE_is_signed(result_ty)) return TRUE;

  return FALSE;
#else
  if (MTYPE_size_min(desc_ty) < MTYPE_size_min(result_ty) &&
      ! MTYPE_is_signed(desc_ty))
    return FALSE;

  if (! MTYPE_is_signed(result_ty)) return FALSE;

  return TRUE;
#endif
}

// this routine determines the signness, and use the size from the lod_typ
MTYPE
Type_for_saved_load(BOOL hi_ever_sign_extended,
                    BOOL lo_ever_sign_extended,
                    MTYPE lod_type)
{
  if (hi_ever_sign_extended) {
    if (lo_ever_sign_extended) {
      if (MTYPE_size_min(lod_type) == MTYPE_size_min(MTYPE_I4))
        return MTYPE_I4;
      else
        return MTYPE_I8;
    }
    else {
      if (MTYPE_size_min(lod_type) == MTYPE_size_min(MTYPE_I4))
        return MTYPE_U4;
      else
        return MTYPE_U8;
    }
  }
  Is_True(lo_ever_sign_extended == FALSE,
          ("Type_for_saved_load: do not expect to see lo_ever_sign_extended"));
  if (MTYPE_size_min(lod_type) == MTYPE_size_min(MTYPE_I4))
    return MTYPE_U4;
  else
    return MTYPE_U8;
}

void
Connect_cr_wn(WN_MAP *cr_wn_map, CODEREP *cr, WN *wn)
{
  WN_MAP_Set( *cr_wn_map, wn, cr );
}


// Return TRUE for "fake" operator.   Fake operator are WHIRL statements
// are implemented as expression in CODEMAP.
BOOL OPERATOR_is_fake(OPERATOR oper) 
{
  return (OPERATOR_is_call(oper) ||
	  oper == OPR_FORWARD_BARRIER ||
	  oper == OPR_BACKWARD_BARRIER ||
	  oper == OPR_ASM_STMT ||
	  oper == OPR_DEALLOCA); 
}

BOOL OPCODE_is_fake(OPCODE opc)
{
  return OPERATOR_is_fake(OPCODE_operator(opc));
}

BOOL OPERATOR_is_volatile(OPERATOR oper)
{
  return (oper == OPR_ALLOCA || 
	  oper == OPR_DEALLOCA ||
	  oper == OPR_FORWARD_BARRIER ||
	  oper == OPR_BACKWARD_BARRIER ||
	  // ASM_STMT shouldn't really be volatile; it should be able
	  // to move, be deleted if it's dead, etc. It is listed here
	  // as a hacky workaround for 753832, in which we try to do
	  // PRE on the ASM_INPUT kids of ASM_STMT.
	  oper == OPR_ASM_STMT ||
	  oper == OPR_ASM_INPUT ||
	  OPERATOR_is_call(oper));
}


BOOL OPCODE_is_volatile(OPCODE opc)
{
  return OPERATOR_is_volatile(OPCODE_operator(opc));
}

// Given an integral WHIRL, query whether it is evaluatable and get the evaluated value.
// 'map' gives a WHIRL-to-WHIRL  map that maps a WHIRL to another WHIRL containing the same value.
// (TODO: Implementation is incomplete for all operators)
std::pair<bool,int>
WN_get_val(WN * wn, std::map<WN *, WN*> & map)
{
  int val1, val2, val;
  OPERATOR opr = WN_operator(wn);
  WN * op1;
  WN * op2;

  if (opr == OPR_INTCONST) {
    val = WN_const_val(wn);
    return std::pair<bool, int>(TRUE, val);
  }
  else {
    WN * wn_val = map[wn];
    if (wn_val)
      return WN_get_val(wn_val, map);
  }

  std::pair<bool,int> pair1;
  std::pair<bool,int> pair2;
  switch (opr) {
  case OPR_ADD:
    pair1 = WN_get_val(WN_kid(wn, 0), map);
    pair2 = WN_get_val(WN_kid(wn, 1), map);
    val1 = pair1.second;
    val2 = pair2.second;
    if (pair1.first
	&& pair2.first) {
      val = val1 + val2;
      return std::pair<bool,int>(TRUE, val);
    }
    break;
  case OPR_MPY:
    op1 = WN_kid(wn, 0);
    op2 = WN_kid(wn, 1);
    pair1 = WN_get_val(op1, map);
    pair2 = WN_get_val(op2, map);
    val1 = pair1.second;
    val2 = pair2.second;

    if (pair1.first
	&& pair2.first
	&& (val1 > 0)
	&& (val2 > 0)
	&& ((WN_operator(op1) == OPR_INTCONST)
	    || (WN_operator(op2) == OPR_INTCONST))) {
      // Note that we evaluate "c0 * b" based on ""b <= c1"
      // Or "b >= c2", where c0, c1, c2 are constants.
      // The inferred value can be incorrect if either "c0",
      // "c1" or "c2" is non-positive.
      val = val1 * val2;
      return std::pair<bool,int>(TRUE, val);
    }
    break;
  default:
    ;
  }

  return std::pair<bool,int>(FALSE,0);
}

// Add all integer constant elements in 'stk'.
static INT64 Add_const_in_stack(STACK<WN *> * stk)
{
  INT64 sum = 0;
  for (int i = 0; i < stk->Elements(); i++) {
    WN * wn_iter = stk->Top_nth(i);
    if (WN_operator(wn_iter) == OPR_INTCONST)
      sum += WN_const_val(wn_iter);
  }
  return sum;
}

// Collect addition and substraction operands in 'wn',
// save addition operands in 'stack1', save substraction operands in 'stack2'.
static void Collect_wn_stack(WN * wn, STACK<WN *> * stack1, STACK<WN *> * stack2, MEM_POOL * pool)
{
  STACK<WN *> * add_stk = CXX_NEW(STACK<WN *> (pool), pool);
  STACK<WN *> * sub_stk = CXX_NEW(STACK<WN *> (pool), pool);

  Collect_operands(wn, add_stk, sub_stk);

  if (add_stk->Is_Empty() && sub_stk->Is_Empty()) 
    stack1->Push(wn);

  while (!add_stk->Is_Empty()) {
    WN * wn_iter = add_stk->Pop();
    stack1->Push(wn_iter);
  }

  while (!sub_stk->Is_Empty()) {
    WN * wn_iter = sub_stk->Pop();
    stack2->Push(wn_iter);
  }

  CXX_DELETE(add_stk, pool);
  CXX_DELETE(sub_stk, pool);
}

// Find elements in 'stack' that matches 'wn_match' and remove such elements 
// from 'stack'.  Return TRUE if found.  Expression "x" is considered to match
// expression "c * x", where c is an integer constant. In this case, also return 
// the diff "(c - 1) * x".
static std::pair<WN *, BOOL>
Get_diff(WN * wn_match, STACK<WN *> * stack, MEM_POOL * pool)
{
  WN * wn_mul = NULL;
  WN * wn_diff = NULL;

  if ((WN_operator(wn_match) == OPR_MPY)
      && (WN_operator(WN_kid1(wn_match)) == OPR_INTCONST))
    wn_mul = WN_kid0(wn_match);

  BOOL found = FALSE;
  STACK<WN *> * stack_tmp = CXX_NEW(STACK<WN *>(pool), pool);
  for (int j = 0; j < stack->Elements(); j++) {
    WN * wn1_iter = stack->Top_nth(j);
    if (WN_Simp_Compare_Trees(wn1_iter, wn_match) == 0) {
      stack->DeleteTop(j);
      found = TRUE;
      break;
    }
    else if (wn_mul
	     && (WN_Simp_Compare_Trees(wn1_iter, wn_mul) == 0)) {
      // expressions like 'x+x' is equal to '2*x'.
      int cnt = 0;
      WN * wn_tmp;
      while (!stack_tmp->Is_Empty())
	stack_tmp->Pop();

      for (int k = 0; k < stack->Elements(); k++) {
	wn_tmp = stack->Top_nth(k);
	if (WN_Simp_Compare_Trees(wn_mul, wn_tmp) == 0)
	  cnt++;
	else 
	  stack_tmp->Push(wn_tmp);
      }
      
      INT64 val = WN_const_val(WN_kid1(wn_match));
      if (cnt == val) {
	// Remove matched elements from 'stack'.
	while (!stack->Is_Empty())
	  stack->Pop();
	      
	while (!stack_tmp->Is_Empty()) {
	  wn_tmp = stack_tmp->Pop();
	  stack->Push(wn_tmp);
	}
	found = TRUE;
	break;
      }
      else if (cnt + 1 == val) {
	found = TRUE;
	wn_diff = wn_mul;
	break;
      }
    }
  }
  CXX_DELETE(stack_tmp, pool);
  return std::pair<WN *, bool>(wn_diff, found);
}

// Obtain the hashed value in 'map' for 'wn',
// where map is a hash from 'AUX_ID' to 'WN *'.
WN * WN_get_deriv(WN * wn, std::map<AUX_ID, WN *> &map)
{
  WN * wn_deriv = NULL;
  if (OPERATOR_is_scalar_load(WN_operator(wn))) 
    wn_deriv = map[WN_aux(wn)];
  
  return wn_deriv;
}

// Query whether two integral WHIRLs have disjointed value ranges.
// Return FALSE if this is not the case or if we can't tell.
// 'lo_map' and 'hi_map' are maps from "WN *" to "UNSIGNED long long" that
// give low/high boundaries that are evaluated to be integer constants.
// 'deriv_map' hashes the AUX_ID of a comparison expression's LHS to 
// the expression itself like the example below.
//   I4I4LDID 49 <st 5>  -- hash key
//    I4I4LDID 0 <st 3>
//    I4INTCONST -1
//   I4ADD
//  I4I4LT
//
// These maps are used to derive value ranges of expressions.
BOOL
WN_has_disjoint_val_range(WN * wn1, WN * wn2, std::map<WN *, WN *> & lo_map, std::map<WN *, WN *> & hi_map, std::map<AUX_ID, WN *> & deriv_map)
{
  FmtAssert((MTYPE_is_integral(WN_rtype(wn1)) && MTYPE_is_integral(WN_rtype(wn2))),
	    ("Expect integral values"));

  OPERATOR opr1 = WN_operator(wn1);
  OPERATOR opr2 = WN_operator(wn2);
  int val;
  std::pair<bool,int> p_val;

  if ((opr1 == OPR_INTCONST) && (opr2 == OPR_INTCONST)) {
    return (WN_const_val(wn1) != WN_const_val(wn2));
  }
  else if (opr1 == OPR_INTCONST) {
    // Swap parameters so that the second one is a constant.
    return WN_has_disjoint_val_range(wn2, wn1, lo_map, hi_map, deriv_map);
  }
  else if (WN_is_power_of_2(wn1) && WN_is_power_of_2(wn2)) {
    // Only need to compare position of TRUE bits for power-of-2 values.
    WN * bit1 = WN_get_bit_from_expr(wn1); 

    if (opr2 == OPR_INTCONST) {
      int bit2 = WN_get_bit_from_const(wn2);
      p_val = WN_get_val(bit1, lo_map);
      val = p_val.second;
      if (p_val.first && (val > bit2)) 
	return TRUE;
      else {
	p_val = WN_get_val(bit1, hi_map);
	val = p_val.second;
	if (p_val.first && (val < bit2))
	  return TRUE;
      }
    }
    else {
      WN * bit2 = WN_get_bit_from_expr(wn2);

      if (WN_has_disjoint_val_range(bit1, bit2, lo_map, hi_map, deriv_map))
	return TRUE;
    }
  }
  else if ( opr2 == OPR_INTCONST) {
    int int_val = WN_const_val(wn2);
    p_val = WN_get_val(wn1, lo_map);
    val = p_val.second;
    if (p_val.first && (val > int_val))
      return TRUE;
    else{
      p_val = WN_get_val(wn1, hi_map);
      val = p_val.second;
      if (p_val.first && (val < int_val))
	return TRUE;
    }
  }
  else {
    MEM_POOL * pool = Malloc_Mem_Pool;
    STACK<WN *> * stack1 = CXX_NEW(STACK<WN *> (pool), pool);
    STACK<WN *> * stack2 = CXX_NEW(STACK<WN *> (pool), pool);

    Collect_wn_stack(wn1, stack1, stack2, pool);
    Collect_wn_stack(wn2, stack2, stack1, pool);
    STACK<WN *> * stack_tmp1 = NULL;
    STACK<WN *> * stack_tmp2 = NULL;
    BOOL do_swap = FALSE;

    // Check whether stack2 contains elements whose AUX_ID is hashed to 
    // a "x < y" expression in the "deriv_map". If so, swap stack1 and stack2.
    for (int i = 0; i < stack2->Elements(); i++) {
      WN * wn_iter = stack2->Top_nth(i);
      WN * wn_deriv = WN_get_deriv(wn_iter, deriv_map);
      if (wn_deriv) {
	if (WN_operator(wn_deriv) == OPR_LT)
	  do_swap = TRUE;
	else {
	  do_swap = FALSE;
	  break;
	}
      }
    }

    if (do_swap) {
      stack_tmp1 = stack1;
      stack1 = stack2;
      stack2 = stack_tmp1;
    }
    
    // Mirror "stack1" and "stack2" in "stack_tmp1" and "stack_tmp2" so that we can replace
    // elements by their hashed values.
    stack_tmp1 = CXX_NEW(STACK<WN *>(pool), pool);
    stack_tmp2 = CXX_NEW(STACK<WN *>(pool), pool);
    for (int i = 0; i < stack1->Elements(); i++) { 
      WN * wn_iter = stack1->Bottom_nth(i);
      WN * wn_deriv = WN_get_deriv(wn_iter, deriv_map);
      if (wn_deriv && (WN_operator(wn_deriv) == OPR_LT)) {
	wn_deriv = WN_kid1(wn_deriv);
	Collect_wn_stack(wn_deriv, stack_tmp1, stack_tmp2, pool);
      }
      else 
	stack_tmp1->Push(wn_iter);
    }

    for (int i = 0; i < stack2->Elements(); i++) { 
      WN * wn_iter = stack2->Bottom_nth(i);
      stack_tmp2->Push(wn_iter);
    }
    
    // Check whether the summation of all elements in "stack_tmp1" is 
    // less than the summation of all elements in "stack_tmp2". If yes,
    // 

    int delta = 0;
    if (stack_tmp1) 
      delta -= Add_const_in_stack(stack_tmp1);

    if (stack_tmp2) 
      delta += Add_const_in_stack(stack_tmp2);

    // Evalute diff of stack_tmp1 and stack_tmp2 using deriv_map.
    BOOL is_disjoint = TRUE;
    for (int i = 0; i < stack_tmp2->Elements(); i++) {
      WN * wn2_iter = stack_tmp2->Top_nth(i);

      if (WN_operator(wn2_iter) == OPR_INTCONST)
	continue;

      std::pair<WN *, BOOL> p_ret = Get_diff(wn2_iter, stack_tmp1, pool);
      WN * wn_diff = p_ret.first;
      if (p_ret.second) {
	if (wn_diff) {
	  std::pair<bool, int> p_val = WN_get_val(wn_diff, lo_map);
	  if (!p_val.first || (p_val.second < 0)) {
	    is_disjoint = FALSE;
	    break;
	  }
	}
      }
      else {
	is_disjoint = FALSE;
	break;
      }
    }
    
    CXX_DELETE(stack_tmp1, pool);
    CXX_DELETE(stack_tmp2, pool);

    if (is_disjoint && (delta > 0)) {
      CXX_DELETE(stack1, pool);
      CXX_DELETE(stack2, pool);
      return TRUE;
    }

    // Shuffle stack1 and stack2 so that stack1 contains more elements.
    if (stack1->Elements() < stack2->Elements()) {
      stack_tmp1 = stack1;
      stack1 = stack2;
      stack2 = stack_tmp1;
    }

    // Evaluate diff of stack1 and stack2 using lo_map and hi_map.
    delta = Add_const_in_stack(stack1);
    delta -= Add_const_in_stack(stack2);
    int delta_lo = delta;
    int delta_hi = delta;
    std::pair<bool, int> p_val;

    for (int i = 0; i < stack2->Elements(); i++) {
      WN * wn2_iter = stack2->Top_nth(i);
	
      if (WN_operator(wn2_iter) == OPR_INTCONST)
	continue;

      std::pair<WN *, BOOL> p_ret = Get_diff(wn2_iter, stack1, pool);
      WN * wn_diff = p_ret.first;
      BOOL found = p_ret.second;

      if (!found || (wn_diff != NULL)) {
	int val;
	p_val = WN_get_val(wn2_iter, lo_map);
	val = p_val.second;

	if (p_val.first)
	  delta_hi -= val;
	else {
	  p_val = WN_get_val(wn2_iter, hi_map);
	  val = p_val.second;
	  if (p_val.first)
	    delta_lo -= val;
	  else {
	    CXX_DELETE(stack1, pool);
	    CXX_DELETE(stack2, pool);
	    return FALSE;
	  }
	}
      }
    }

    for (int i = 0; i < stack1->Elements(); i++) {
      WN * wn_iter = stack1->Top_nth(i);
      int val;

      if (WN_operator(wn_iter) == OPR_INTCONST)
	continue;

      p_val = WN_get_val(wn_iter, lo_map);
      val = p_val.second;
      if (p_val.first)
	delta_lo += val;
      else {
	p_val = WN_get_val(wn_iter, hi_map);
	val = p_val.second;
	if (p_val.first)
	  delta_hi += val;
	else {
	  CXX_DELETE(stack1, pool);
	  CXX_DELETE(stack2, pool);
	  return FALSE;
	}
      }
    }

    CXX_DELETE(stack1, pool);
    CXX_DELETE(stack2, pool);

    if ((delta_lo > 0) || (delta_hi < 0))
      return TRUE;
  }

  return FALSE;
}

// Query whether the WHIRL tree rooted at 'wn' contains any indirect loads.
BOOL
WN_has_indir_load(WN * wn)
{
  OPERATOR opr = WN_operator(wn);
  FmtAssert(opr != OPR_BLOCK, ("Illegal input WHILR"));
  
  if (OPERATOR_is_load(opr) && !OPERATOR_is_scalar_load(opr))
    return TRUE;

  for (int i = 0; i < WN_kid_count(wn); i++) {
    if (WN_has_indir_load(WN_kid(wn,i)))
      return TRUE;
  }
  
  return FALSE;
}

// Walk the WHIRL tree rooted at 'wn', collect nodes having addition effects in 'l_stk'
// and nodes having substration effects in 'r_stk'.
void
Collect_operands(WN * wn, STACK<WN *> * l_stk, STACK<WN *> * r_stk) 
{
  OPERATOR p_opr = WN_operator(wn);
  if (((p_opr == OPR_ADD) || (p_opr == OPR_SUB))
      && (WN_rtype(wn) == MTYPE_I4)) {
    for (int i = 0; i <= 1; i++) {
      WN * operand = WN_kid(wn, i);
      OPERATOR c_opr = WN_operator(operand);

      switch (c_opr) {
      case OPR_ADD:
      case OPR_SUB:
	if ((p_opr == OPR_ADD) || (i == 0))
	  Collect_operands(operand, l_stk, r_stk);
	else 
	  Collect_operands(operand, r_stk, l_stk);
	break;
      default:
	if ((p_opr == OPR_ADD) || (i == 0)) {
	  if (l_stk) 
	    l_stk->Push(operand);
	}
	else {
	  if (r_stk) 
	    r_stk->Push(operand);
	}
      }
    }
  }
}

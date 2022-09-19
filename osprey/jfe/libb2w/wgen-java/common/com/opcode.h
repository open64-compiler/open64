/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/**
***		Opcodes, Opcode Types, and Opcode Flags
***		---------------------------------------
***
*** Description:
***
***	This file includes opcode_core.h (see the interface there) and
***	provides some accessing functions to the data.  It is split like
***	this because the accessors call FmtAssert() and other routines
***	that non-compiler passes might want to avoid.
***
***     This file should be updated whenever opcode_gen is updated.
***     opcode_gen is a perl file that is somewhat readable -- if the
***     comments get out of date here, one can always look there (and
***     bring them up to date).
***
*** Exported Functions:
***
***	OPCODE OPCODE_make_op(
***		OPERATOR opr,
***		TYPE_ID rtype,
***		TYPE_ID desc
***	)
***
***	    Given the operator, rtype and desc type, return
***	    the opcode.  The opr, rtype and desc MUST map to a valid
***	    OPCODE.  Otherwise, the compilation terminates with an
***	    internal error.
***
***	const char *OPCODE_name(OPCODE op)
***
***	    Returns a string representation of the opcode.
***
***	const char *OPERATOR_name(OPERATOR opr)
***
***	    Returns a string representation of the operator.
***
***	The remaining exported functions are actually not declared
***	in this file but in the automatically generated include files.
***
***	mINT8 OPCODE_nkids(OPCODE op)
***
***	    Returns the number of kids this opcode must have, or -1 if
***	    not fixed, i.e. call, array, return.
***
***	The following exported functions return 0 if the opcode does not
***	have the stated property.  Otherwise, the value returned is non-zero.
***
***	UINT32 OPCODE_is_scf(OPCODE op)
***
***	    Opcode is OPR_FUNC_ENTRY, OPR_BLOCK, OPR_DO_LOOP, OPR_DO_WHILE,
***	    OPR_WHILE_DO, OPR_IF.
***
***	UINT32 OPCODE_is_stmt(OPCODE op)
***
***	    Opcode occurs at top of an expression tree, e.g. a store, call, 
***	    goto, pragma, etc.  No structured control flow falls in this
***	    category.
***
***	UINT32 OPCODE_is_expression(OPCODE op)
***
***	    Opcode occurs underneath a statement, e.g. OPR_DADD.
***
***	UINT32 OPCODE_is_leaf(OPCODE op)
***
***	    Opcode is an expression or statement that never has children.
***	    Note that OPR_RETURN is not a leaf, since it may have children.
***
***	UINT32 OPCODE_is_load(OPCODE op)
***	UINT32 OPCODE_is_store(OPCODE op)
***
***	    Opocde gets memory, e.g. ILOAD, MLOAD, LDID, etc (or store)
***	    Goto, branch, label, altentry
***
***	UINT32 OPCODE_is_call(OPCODE op)
***
***         Intrinsic call or regular, but not an intrinsic op.
***
***	UINT32 OPCODE_is_compare(OPCODE op)
***
***         Comparison, e.g. GE
***
***	UINT32 OPCODE_is_non_scf(OPCODE op)
***
***	    Opcode directly relates to control flow, but is not structured.
***	    Goto, branch, label, altentry
***
***	UINT32 OPCODE_is_boolean(OPCODE op)
***
***         Boolean return value, e.g. a comparison.
***
***	UINT32 OPCODE_is_endsbb(OPCODE op)
***
***	    Call, a goto, branch, call, return.
***
***	UINT32 OPCODE_is_comp_unit_if(OPCODE op)
***
***	    Compilation unit interface, e.g. REGION
***
***	UINT32 OPCODE_is_not_executable(OPCODE op)
***
***	    Currently PRAGMA, COMMENT, LABEL and OPT_ opcodes.
***
***	UINT32 OPCODE_is_prefetcn(OPCODE op)
***
***	    Obvious.
***
***     OPCODE OPCODE_commutative_op(OPCODE op)
***
***         Return an opcode op1 such that x op y is y op1 x. If none
***         exists, return (OPCODE) 0.
***
***	The following exported functions describe qualities of the WN
***	that has this opcode.  E.g. OPCODE_has_sym(op) indicates that a
***     WN holding this opcode also holds an ST_IDX.
***
***	UINT32 OPCODE_has_next_prev(OPCODE op)
***	UINT32 OPCODE_has_sym(OPCODE op)
***	UINT32 OPCODE_has_label(OPCODE op)
***	UINT32 OPCODE_has_num_entries(OPCODE op)
***	UINT32 OPCODE_has_offset(OPCODE op)
***	UINT32 OPCODE_has_bits(OPCODE op)
***	UINT32 OPCODE_has_ndim(OPCODE op)
***	UINT32 OPCODE_has_esize(OPCODE op)
***	UINT32 OPCODE_has_value(OPCODE op)
***	UINT32 OPCODE_has_flags(OPCODE op)
***	UINT32 OPCODE_has_inumber(OPCODE op)
***	UINT32 OPCODE_has_1ty(OPCODE op)
***	UINT32 OPCODE_has_2ty(OPCODE op)
***
***	    The 'has' fields indicate which fields of a WN are used.
***
***	OPCODE_MAPCAT OPCODE_mapcat(OPCODE op)
***
***	    Annotation category of an opcode.  Current values are
***         OPCODE_MAPCAT_{HDR,SCF,LDST,PRAGMA,OSTMT,OEXP,ARRAY,CALL}.
**/

/** $Revision: 1.1.1.1 $
*** $Date: 2005/10/21 19:00:00 $
*** $Author: marcel $
*** $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/opcode.h,v $
**/

#ifndef opcode_INCLUDED
#define opcode_INCLUDED "opcode.h"

#ifdef _KEEP_RCS_ID
static char *opcode_rcs_id = opcode_INCLUDED "$Revision: 1.4 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "opcode_core.h"
#include "errors.h"
#include "opcode_gen.h"

#ifdef __cplusplus
extern "C" {
#endif

extern const char	*OPERATOR_name(OPERATOR);
extern OPCODE           OPCODE_commutative_op(OPCODE op);

/*
 * The order of these array subscripts is significant because wopt
 * relies heavily on this routine and usually calls it with opr and
 * desc as constants.  Thus if we make rtype the first subscript, we
 * eliminate a multiply in most cases.
 */

extern BOOL Operator_To_Opcode_Table_Inited;
void Init_Operator_To_Opcode_Table(void);

#define OPCODE_make_op_MACRO(opr,rtype,desc)  ((OPCODE) ((opr) | ((rtype) << 8) | ((desc) << 14)))



#if 1
#if 0
inline OPCODE OPCODE_make_op_no_assert(OPERATOR opr, TYPE_ID rtype,
				       TYPE_ID desc)
{
  OPCODE ret;
      
  ret = OPCODE_make_op_MACRO(opr,rtype,desc);
  ret = Is_Valid_Opcode (ret) ? ret : OPCODE_UNKNOWN;
  
  return ret;
}
#endif

inline OPCODE OPCODE_make_op(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc)
{
  OPCODE ret;

  ret = OPCODE_make_op_MACRO(opr,rtype,desc);
  Is_True(Is_Valid_Opcode(ret),
	  ("OPCODE_make_op: no opcode available: %d %d %d", opr, rtype, desc));

  return ret;
}
#else /* Is_True_On */
#define OPCODE_make_op(opr,rtype,desc) OPCODE_MAKE_VALID( OPCODE_make_op_MACRO(opr,rtype,desc))
#endif

#ifdef __cplusplus
}
#endif

#endif

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


/* Conversion from WHIRL opcode to CG opcode. */

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include "defs.h"
#include "errors.h"
#include "util.h"
#include "topcode.h"
#include "targ_isa_properties.h"
#include "wn.h"
#include "wn_util.h"
#include "opcode.h"
#include "config_targ.h"
#include "config.h"
#include "config_debug.h"
#include "tracing.h"
#include "opt_alias_interface.h"
#include "opt_alias_mgr.h"
#include "betarget.h"
#include "w2op.h"


/* only return machine_ops, TOP_UNDEFINED if not an exact correspondence */
TOP
WHIRL_To_TOP (WN *wn)
{
  OPCODE opcode = WN_opcode(wn);
  TOP top;

  top = OPCODE_To_TOP (opcode);

  /* For TAS WHIRL operators there is no direct translation. We 
   * have to look at the kids to determine the corresponding TOP.
   */
  if (top == TOP_UNDEFINED && OPCODE_operator(opcode) == OPR_TAS) {
      top = TAS_To_TOP (wn);
  }
  return top;
}


/* determines if the TOP can be speculatively executed taking into account
 * eagerness level
 */
BOOL TOP_Can_Be_Speculative (TOP opcode)
{
  switch (Eager_Level) {
  case EAGER_NONE:

    /* not allowed to speculate anything
     */
    break;

  case EAGER_SAFE:

    /* Only exception-safe speculative ops are allowed
     */
    if (TOP_is_ftrap(opcode) || TOP_is_itrap(opcode)) break;
    /*FALLTHROUGH*/

  case EAGER_ARITH:

    /* Arithmetic exceptions allowed
     */
    if (TOP_is_fdiv(opcode)) break;
    /*FALLTHROUGH*/

  case EAGER_DIVIDE:

    /* Divide by zero exceptions allowed 
     */
    if (TOP_is_memtrap(opcode)) break;
    /*FALLTHROUGH*/

  case EAGER_MEMORY:

    /* Memory exceptions allowed / All speculative ops allowed
     */
    if (TOP_is_unsafe(opcode)) break;
    return TRUE;

  default:
    DevWarn("unhandled eagerness level: %d", Eager_Level);
    break;
  }

   return FALSE;
}


/* ====================================================================
 *
 * BOOL OPCODE_Can_Be_Speculative(OPCODE opcode)
 *
 * Determine is the opcode can be executed speculatively, given that
 * its children are speculative
 *
 * Note:
 * TOP div (integer) does NOT trap, although -DEBUG:div_check (default)
 * inserts a trap to check divide by zero. So we have the strange case
 * that OPC_{I,U}{4,8}DIV does trap but the TOP div does not.
 *
 * ====================================================================
 */

BOOL OPCODE_Can_Be_Speculative(OPCODE opcode)
{
  if (Eager_Level == EAGER_NONE)
    return FALSE;

  switch(OPCODE_operator(opcode))
  {
  case OPR_LDA:
  case OPR_CONST:
  case OPR_INTCONST:
  case OPR_IDNAME:
  case OPR_PREFETCH:
  case OPR_PREFETCHX:
  case OPR_ARRAY:
    return EAGER_SAFE <= Eager_Level;

  case OPR_ILOAD:
  case OPR_ILOADX:
  case OPR_MLOAD:
  case OPR_LDID:
    return EAGER_MEMORY <= Eager_Level;

  case OPR_CVT:
  case OPR_NEG:
  case OPR_ABS:
  case OPR_RND:
  case OPR_TRUNC:
  case OPR_CEIL:
  case OPR_FLOOR:
  case OPR_MADD:
  case OPR_MSUB:
  case OPR_NMADD:
  case OPR_NMSUB:
    if (MTYPE_is_float(OPCODE_rtype(opcode)))
      return EAGER_ARITH <= Eager_Level;
    return EAGER_SAFE <= Eager_Level;

  case OPR_SQRT:
  case OPR_DIV:
  case OPR_RECIP:
  case OPR_RSQRT:
  case OPR_REM:
  case OPR_DIVREM:
    if (MTYPE_is_float(OPCODE_rtype(opcode)) ||
	DEBUG_Div_Zero_Check)
      return EAGER_DIVIDE <= Eager_Level;
    return EAGER_SAFE <= Eager_Level;

  case OPR_REALPART:
  case OPR_IMAGPART:
  case OPR_PAREN:
  case OPR_LOWPART:
  case OPR_HIGHPART:
  case OPR_MINMAX:
  case OPR_MINPART:
  case OPR_MAXPART:
  case OPR_COMPLEX:
    return EAGER_SAFE <= Eager_Level;

  case OPR_BNOT:
  case OPR_LNOT:
  case OPR_ADD:
  case OPR_SUB:
  case OPR_MPY:
  case OPR_MOD:
  case OPR_MAX:
  case OPR_MIN:
  case OPR_TAS:
  case OPR_CVTL:
  case OPR_BAND:
  case OPR_BIOR:
  case OPR_BNOR:
  case OPR_BXOR:
  case OPR_LAND:
  case OPR_LIOR:
  case OPR_SHL:
  case OPR_ASHR:
  case OPR_LSHR:
    if (MTYPE_is_float(OPCODE_rtype(opcode)))
      return EAGER_DIVIDE <= Eager_Level;
    return EAGER_SAFE <= Eager_Level;

  case OPR_CAND:
  case OPR_CIOR:
  case OPR_SELECT:
  case OPR_CSELECT:
    return EAGER_SAFE <= Eager_Level;

  case OPR_EQ:
  case OPR_NE:
  case OPR_GT:
  case OPR_GE:
  case OPR_LT:
  case OPR_LE:
    return EAGER_SAFE <= Eager_Level;

  case OPR_INTRINSIC_OP:
    return FALSE;
  }
  return FALSE;
}


/* ====================================================================
 *
 * BOOL WN_Can_Be_Speculative (WN *wn, struct ALIAS_MANAGER *alias)
 *
 * Determine is the wn can be executed speculatively
 * This is a close approximation to be/cg/cgtarget.c: CGTAG_Can_Be_Speculative()
 *
 *
 * TODO:
 *	handle classes of ILOADs
 *
 * ====================================================================
 */
BOOL WN_Can_Be_Speculative (WN *wn, struct ALIAS_MANAGER *alias)
{
  switch(WN_operator(wn))
  {
  case OPR_CONST:
  case OPR_INTCONST:
  case OPR_LDA:
    return TRUE;

  case OPR_LDID:
    if (WN_class(wn) == CLASS_PREG)
      return TRUE;

    if (ST_is_constant(WN_st(wn)))
      return TRUE;

#ifdef KEY // bug 8581
    if (ST_sclass(WN_st(wn)) != SCLASS_FORMAL_REF) // bug 9599
      return ! WN_Is_Volatile_Mem(wn);
#endif
    
#ifdef TARG_NVISA
    // we want to be aggressive about speculating short-circuit operators,
    // at which point we don't have alias info, so instead check that it is
    // a non-dynamic reference (so may be garbage but won't segfault).
    // pointer derefs will generate ILOAD; LDID should always be safe?

    if (WN_Is_Volatile_Mem(wn))
      return FALSE;

    return TRUE;
#endif

  case OPR_ILOAD:
  case OPR_ILOADX:
    if (WN_Is_Volatile_Mem(wn))
      return FALSE;

    if (alias && alias->Safe_to_speculate(wn))
      return TRUE;
    return FALSE;

  case OPR_DIV:
  case OPR_MOD:
  case OPR_REM:
    TYPE_ID rtype = WN_rtype(wn); 
    if ((rtype == MTYPE_I4 || rtype == MTYPE_I8 || rtype == MTYPE_U4
	|| rtype == MTYPE_U8) && WN_operator(WN_kid1(wn)) 
        == OPR_INTCONST && WN_const_val(wn) != 0)
      return TRUE; 

  } 
  return OPCODE_Can_Be_Speculative(WN_opcode(wn));
}

/* ====================================================================
 *
 * BOOL WN_Expr_Can_Be_Speculative (WN *wn, struct ALIAS_MANAGER *alias)
 *
 * Determine if the whirl expression can be executed speculatively
 * It can only be speculative if the expression and all its children are speculative
 *
 * ====================================================================
 */
BOOL WN_Expr_Can_Be_Speculative (WN *wn, struct ALIAS_MANAGER *alias)
{
  INT16 i;

  for(i=0; i<WN_kid_count(wn); i++)
  {
    if (WN_Expr_Can_Be_Speculative(WN_kid(wn,i), alias) == FALSE)
      return FALSE;
  }

  return WN_Can_Be_Speculative(wn, alias);
}

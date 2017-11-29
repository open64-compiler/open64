/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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
 * Module: variants.cxx
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:28-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.variants.cxx $
 *
 * Description:
 *
 * Functions related to OP variant field contents.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static const char rcs_id[] = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.variants.cxx $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "config.h"
#include "glob.h"
#include "erglob.h"
#include "erbe.h"
#include "tracing.h"

#include "variants.h"

/* ====================================================================
 *
 * Negate_BR_Variant
 *
 * See interface description.
 *
 * ====================================================================
 */
VARIANT
Negate_BR_Variant(VARIANT variant)
{
  BOOL is_false = V_false_br(variant);

  switch (V_br_condition(variant)) {
  case V_BR_NONE: break;

  case V_BR_I8EQ0: variant = V_BR_I8NE0; break;
  case V_BR_I8NE0: variant = V_BR_I8EQ0; break;
  case V_BR_I8GT0: variant = V_BR_I8LE0; break;
  case V_BR_I8GE0: variant = V_BR_I8LT0; break;
  case V_BR_I8LT0: variant = V_BR_I8GE0; break;
  case V_BR_I8LE0: variant = V_BR_I8GT0; break;

  case V_BR_I8EQ: variant = V_BR_I8NE; break;
  case V_BR_I8NE: variant = V_BR_I8EQ; break;
  case V_BR_I8GT: variant = V_BR_I8LE; break;
  case V_BR_I8GE: variant = V_BR_I8LT; break;
  case V_BR_I8LT: variant = V_BR_I8GE; break;
  case V_BR_I8LE: variant = V_BR_I8GT; break;

  case V_BR_U8EQ0: variant = V_BR_U8NE0; break;
  case V_BR_U8NE0: variant = V_BR_U8EQ0; break;
  case V_BR_U8GT0: variant = V_BR_U8LE0; break;
  case V_BR_U8GE0: variant = V_BR_U8LT0; break;
  case V_BR_U8LT0: variant = V_BR_U8GE0; break;
  case V_BR_U8LE0: variant = V_BR_U8GT0; break;

  case V_BR_U8EQ: variant = V_BR_U8NE; break;
  case V_BR_U8NE: variant = V_BR_U8EQ; break;
  case V_BR_U8GT: variant = V_BR_U8LE; break;
  case V_BR_U8GE: variant = V_BR_U8LT; break;
  case V_BR_U8LT: variant = V_BR_U8GE; break;
  case V_BR_U8LE: variant = V_BR_U8GT; break;

  case V_BR_I4EQ: variant = V_BR_I4NE; break;
  case V_BR_I4NE: variant = V_BR_I4EQ; break;
  case V_BR_I4GT: variant = V_BR_I4LE; break;
  case V_BR_I4GE: variant = V_BR_I4LT; break;
  case V_BR_I4LT: variant = V_BR_I4GE; break;
  case V_BR_I4LE: variant = V_BR_I4GT; break;

  case V_BR_U4EQ: variant = V_BR_U4NE; break;
  case V_BR_U4NE: variant = V_BR_U4EQ; break;
  case V_BR_U4GT: variant = V_BR_U4LE; break;
  case V_BR_U4GE: variant = V_BR_U4LT; break;
  case V_BR_U4LT: variant = V_BR_U4GE; break;
  case V_BR_U4LE: variant = V_BR_U4GT; break;

  case V_BR_F_FALSE: variant = V_BR_F_TRUE;  break;
  case V_BR_F_TRUE:  variant = V_BR_F_FALSE; break;

  case V_BR_PEQ: variant = V_BR_PNE; break;
  case V_BR_PNE: variant = V_BR_PEQ; break;

  case V_BR_FEQ:
  case V_BR_FNE:
  case V_BR_FGT:
  case V_BR_FGE:
  case V_BR_FLT:
  case V_BR_FLE:

  case V_BR_DEQ:
  case V_BR_DNE:
  case V_BR_DGT:
  case V_BR_DGE:
  case V_BR_DLT:
  case V_BR_DLE:

#if defined(TARG_IA64) || defined(TARG_X8664)
  case V_BR_XEQ:
  case V_BR_XNE:
  case V_BR_XGT:
  case V_BR_XGE:
  case V_BR_XLT:
  case V_BR_XLE:
#endif

  case V_BR_QEQ:
  case V_BR_QNE:
  case V_BR_QGT:
  case V_BR_QGE:
  case V_BR_QLT:
  case V_BR_QLE:
  case V_BR_FOR:
  case V_BR_DOR:
  case V_BR_DUO:
  case V_BR_FUO:
    // changing the variant will effect the comparison if NaNs are
    // involved, so just flip the sense.
    is_false = !is_false;
    break;

  case V_BR_P_TRUE:
  case V_BR_CLOOP:
  case V_BR_CTOP:
  case V_BR_CEXIT:
  case V_BR_WTOP:
  case V_BR_WEXIT:
    // no inverse condition -- invert the sense
    is_false = !is_false;
    break;

  case V_BR_ALWAYS: variant = V_BR_NEVER;  break;
  case V_BR_NEVER:  variant = V_BR_ALWAYS; break;

  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE, ("unexpected variant %lld", variant));
    /*NOTREACHED*/
  }

  // set the sense.
  if (is_false) {
    Set_V_false_br(variant);
  } else {
    Set_V_true_br(variant);
  }

  return variant;
}


/* ====================================================================
 *
 * Invert_BR_Variant
 *
 * See interface description.
 *
 * ====================================================================
 */
VARIANT
Invert_BR_Variant(VARIANT variant)
{
  BOOL is_false = V_false_br(variant);

  switch (V_br_condition(variant)) {
  case V_BR_NONE: break;

  case V_BR_I8EQ: variant = V_BR_I8EQ; break;
  case V_BR_I8NE: variant = V_BR_I8NE; break;
  case V_BR_I8GT: variant = V_BR_I8LT; break;
  case V_BR_I8GE: variant = V_BR_I8LE; break;
  case V_BR_I8LT: variant = V_BR_I8GT; break;
  case V_BR_I8LE: variant = V_BR_I8GE; break;

  case V_BR_U8EQ: variant = V_BR_U8EQ; break;
  case V_BR_U8NE: variant = V_BR_U8NE; break;
  case V_BR_U8GT: variant = V_BR_U8LT; break;
  case V_BR_U8GE: variant = V_BR_U8LE; break;
  case V_BR_U8LT: variant = V_BR_U8GT; break;
  case V_BR_U8LE: variant = V_BR_U8GE; break;

  case V_BR_FEQ: variant = V_BR_FEQ; break;
  case V_BR_FNE: variant = V_BR_FNE; break;
  case V_BR_FGT: variant = V_BR_FLT; break;
  case V_BR_FGE: variant = V_BR_FLE; break;
  case V_BR_FLT: variant = V_BR_FGT; break;
  case V_BR_FLE: variant = V_BR_FGE; break;

  case V_BR_DEQ: variant = V_BR_DEQ; break;
  case V_BR_DNE: variant = V_BR_DNE; break;
  case V_BR_DGT: variant = V_BR_DLT; break;
  case V_BR_DGE: variant = V_BR_DLE; break;
  case V_BR_DLT: variant = V_BR_DGT; break;
  case V_BR_DLE: variant = V_BR_DGE; break;

#if defined(TARG_IA64) || defined(TARG_X8664)
  case V_BR_XEQ: variant = V_BR_XEQ; break;
  case V_BR_XNE: variant = V_BR_XNE; break;
  case V_BR_XGT: variant = V_BR_XLT; break;
  case V_BR_XGE: variant = V_BR_XLE; break;
  case V_BR_XLT: variant = V_BR_XGT; break;
  case V_BR_XLE: variant = V_BR_XGE; break;
#endif

  case V_BR_QEQ: variant = V_BR_QEQ; break;
  case V_BR_QNE: variant = V_BR_QNE; break;
  case V_BR_QGT: variant = V_BR_QLT; break;
  case V_BR_QGE: variant = V_BR_QLE; break;
  case V_BR_QLT: variant = V_BR_QGT; break;
  case V_BR_QLE: variant = V_BR_QGE; break;

  case V_BR_I4EQ: variant = V_BR_I4EQ; break;
  case V_BR_I4NE: variant = V_BR_I4NE; break;
  case V_BR_I4GT: variant = V_BR_I4LT; break;
  case V_BR_I4GE: variant = V_BR_I4LE; break;
  case V_BR_I4LT: variant = V_BR_I4GT; break;
  case V_BR_I4LE: variant = V_BR_I4GE; break;

  case V_BR_U4EQ: variant = V_BR_U4EQ; break;
  case V_BR_U4NE: variant = V_BR_U4NE; break;
  case V_BR_U4GT: variant = V_BR_U4LT; break;
  case V_BR_U4GE: variant = V_BR_U4LE; break;
  case V_BR_U4LT: variant = V_BR_U4GT; break;
  case V_BR_U4LE: variant = V_BR_U4GE; break;

  case V_BR_PEQ: variant = V_BR_PEQ; break;
  case V_BR_PNE: variant = V_BR_PNE; break;

  case V_BR_I8EQ0:
  case V_BR_I8NE0:
  case V_BR_I8GT0:
  case V_BR_I8GE0:
  case V_BR_I8LT0:
  case V_BR_I8LE0:
  case V_BR_U8EQ0:
  case V_BR_U8NE0:
  case V_BR_U8GT0:
  case V_BR_U8GE0:
  case V_BR_U8LT0:
  case V_BR_U8LE0:
  case V_BR_F_FALSE:
  case V_BR_F_TRUE:
  case V_BR_P_TRUE:
    // these branches only have one operand, so swapping operands
    // makes no sense.
    break;

  case V_BR_ALWAYS:
  case V_BR_NEVER:
  case V_BR_CLOOP:
  case V_BR_CTOP:
  case V_BR_CEXIT:
  case V_BR_WTOP:
  case V_BR_WEXIT:
     // these branches effectively have no operands, so swapping
    // operands makes no sense.
    break;

  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE, ("unexpected variant %lld", variant));
    /*NOTREACHED*/
  }

  // set the sense.
  if (is_false) {
    Set_V_false_br(variant);
  } else {
    Set_V_true_br(variant);
  }

  return variant;
}


/* ====================================================================
 *
 * BR_Variant_Name
 *
 * See interface description.
 *
 * ====================================================================
 */
const char *
BR_Variant_Name(VARIANT variant)
{
  BOOL is_false = V_false_br(variant);
  switch (variant & V_BR_MASK) {
  case V_BR_NONE:    return is_false ? "!NONE"    : "NONE";

  case V_BR_I8EQ0:   return is_false ? "!I8EQ0"   : "I8EQ0";
  case V_BR_I8NE0:   return is_false ? "!I8NE0"   : "I8NE0";
  case V_BR_I8GT0:   return is_false ? "!I8GT0"   : "I8GT0";
  case V_BR_I8GE0:   return is_false ? "!I8GE0"   : "I8GE0";
  case V_BR_I8LT0:   return is_false ? "!I8LT0"   : "I8LT0";
  case V_BR_I8LE0:   return is_false ? "!I8LE0"   : "I8LE0";

  case V_BR_I8EQ:    return is_false ? "!I8EQ"    : "I8EQ";
  case V_BR_I8NE:    return is_false ? "!I8NE"    : "I8NE";
  case V_BR_I8GT:    return is_false ? "!I8GT"    : "I8GT";
  case V_BR_I8GE:    return is_false ? "!I8GE"    : "I8GE";
  case V_BR_I8LT:    return is_false ? "!I8LT"    : "I8LT";
  case V_BR_I8LE:    return is_false ? "!I8LE"    : "I8LE";

  case V_BR_U8EQ0:   return is_false ? "!U8EQ0"   : "U8EQ0";
  case V_BR_U8NE0:   return is_false ? "!U8NE0"   : "U8NE0";
  case V_BR_U8GT0:   return is_false ? "!U8GT0"   : "U8GT0";
  case V_BR_U8GE0:   return is_false ? "!U8GE0"   : "U8GE0";
  case V_BR_U8LT0:   return is_false ? "!U8LT0"   : "U8LT0";
  case V_BR_U8LE0:   return is_false ? "!U8LE0"   : "U8LE0";

  case V_BR_U8EQ:    return is_false ? "!U8EQ"    : "U8EQ";
  case V_BR_U8NE:    return is_false ? "!U8NE"    : "U8NE";
  case V_BR_U8GT:    return is_false ? "!U8GT"    : "U8GT";
  case V_BR_U8GE:    return is_false ? "!U8GE"    : "U8GE";
  case V_BR_U8LT:    return is_false ? "!U8LT"    : "U8LT";
  case V_BR_U8LE:    return is_false ? "!U8LE"    : "U8LE";

  case V_BR_FEQ:     return is_false ? "!FEQ"     : "FEQ";
  case V_BR_FNE:     return is_false ? "!FNE"     : "FNE";
  case V_BR_FGT:     return is_false ? "!FGT"     : "FGT";
  case V_BR_FGE:     return is_false ? "!FGE"     : "FGE";
  case V_BR_FLT:     return is_false ? "!FLT"     : "FLT";
  case V_BR_FLE:     return is_false ? "!FLE"     : "FLE";

  case V_BR_DEQ:     return is_false ? "!DEQ"     : "DEQ";
  case V_BR_DNE:     return is_false ? "!DNE"     : "DNE";
  case V_BR_DGT:     return is_false ? "!DGT"     : "DGT";
  case V_BR_DGE:     return is_false ? "!DGE"     : "DGE";
  case V_BR_DLT:     return is_false ? "!DLT"     : "DLT";
  case V_BR_DLE:     return is_false ? "!DLE"     : "DLE";

#if defined(TARG_IA64) || defined(TARG_X8664)
  case V_BR_XEQ:     return is_false ? "!XEQ"     : "XEQ";
  case V_BR_XNE:     return is_false ? "!XNE"     : "XNE";
  case V_BR_XGT:     return is_false ? "!XGT"     : "XGT";
  case V_BR_XGE:     return is_false ? "!XGE"     : "XGE";
  case V_BR_XLT:     return is_false ? "!XLT"     : "XLT";
  case V_BR_XLE:     return is_false ? "!XLE"     : "XLE";
#endif

  case V_BR_QEQ:     return is_false ? "!QEQ"     : "QEQ";
  case V_BR_QNE:     return is_false ? "!QNE"     : "QNE";
  case V_BR_QGT:     return is_false ? "!QGT"     : "QGT";
  case V_BR_QGE:     return is_false ? "!QGE"     : "QGE";
  case V_BR_QLT:     return is_false ? "!QLT"     : "QLT";
  case V_BR_QLE:     return is_false ? "!QLE"     : "QLE";

  case V_BR_I4EQ:    return is_false ? "!I4EQ"    : "I4EQ";
  case V_BR_I4NE:    return is_false ? "!I4NE"    : "I4NE";
  case V_BR_I4GT:    return is_false ? "!I4GT"    : "I4GT";
  case V_BR_I4GE:    return is_false ? "!I4GE"    : "I4GE";
  case V_BR_I4LT:    return is_false ? "!I4LT"    : "I4LT";
  case V_BR_I4LE:    return is_false ? "!I4LE"    : "I4LE";

  case V_BR_U4EQ:    return is_false ? "!U4EQ"    : "U4EQ";
  case V_BR_U4NE:    return is_false ? "!U4NE"    : "U4NE";
  case V_BR_U4GT:    return is_false ? "!U4GT"    : "U4GT";
  case V_BR_U4GE:    return is_false ? "!U4GE"    : "U4GE";
  case V_BR_U4LT:    return is_false ? "!U4LT"    : "U4LT";
  case V_BR_U4LE:    return is_false ? "!U4LE"    : "U4LE";

  case V_BR_F_FALSE: return is_false ? "!F_FALSE" : "F_FALSE";
  case V_BR_F_TRUE:  return is_false ? "!F_TRUE"  : "F_TRUE";

  case V_BR_P_TRUE:  return is_false ? "!P_TRUE"  : "P_TRUE";
  case V_BR_PEQ:     return is_false ? "!PEQ"     : "PEQ";
  case V_BR_PNE:     return is_false ? "!PNE"     : "PNE";

  case V_BR_CLOOP:   return is_false ? "!CLOOP"   : "CLOOP";
  case V_BR_CTOP:    return is_false ? "!CTOP"    : "CTOP";
  case V_BR_CEXIT:   return is_false ? "!CEXIT"   : "CEXIT";
  case V_BR_WTOP:    return is_false ? "!WTOP"    : "WTOP";
  case V_BR_WEXIT:   return is_false ? "!WEXIT"   : "WEXIT";

  case V_BR_ALWAYS:  return is_false ? "!ALWAYS"  : "ALWAYS";
  case V_BR_NEVER:   return is_false ? "!NEVER"   : "NEVER";
  }

  return "--UNKNOWN--";
}

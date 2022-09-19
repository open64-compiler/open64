/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

#ifndef intrn_info_INCLUDED
#define intrn_info_INCLUDED "intrn_info.h"

#include "defs.h"
#include "wintrinsic.h"

/* Enumeration of mnemonic names for the return types of intrinsic
 * functions and operators.  
 */
typedef enum INTRN_RETKIND {
  IRETURN_UNKNOWN,           /* Indeterminate type */
  IRETURN_V,                 /* MTYPE_V */
  IRETURN_I1,                /* MTYPE_I1 */
  IRETURN_I2,                /* MTYPE_I2 */
  IRETURN_I4,                /* MTYPE_I4 */
  IRETURN_I8,                /* MTYPE_I8 */
  IRETURN_U1,                /* MTYPE_U1 */
  IRETURN_U2,                /* MTYPE_U2 */
  IRETURN_U4,                /* MTYPE_U4 */
  IRETURN_U8,                /* MTYPE_U8 */
  IRETURN_F4,                /* MTYPE_F4 */
  IRETURN_F8,                /* MTYPE_F8 */
  IRETURN_FQ,                /* MTYPE_FQ */
  IRETURN_C4,                /* MTYPE_C4 */
  IRETURN_C8,                /* MTYPE_C8 */
  IRETURN_CQ,                /* MTYPE_CQ */
  IRETURN_PV,                /* Pointer to MTYPE_V */
  IRETURN_PU1,               /* Pointer to MTYPE_U1 */
  IRETURN_DA1,               /* Dereference argument 1 */
  IRETURN_SZT,               /* size_t */
  IRETURN_PC,                /* pointer to char */
  IRETURN_F10,               /* MTYPE_F10 */
#ifdef TARG_X8664
  IRETURN_V16I2,	     /* MTYPE_V16I2 */
  IRETURN_V16I4,	     /* MTYPE_V16I4 */
  IRETURN_V16F4,	     /* MTYPE_V16F4 */
  IRETURN_V16F8,	     /* MTYPE_V16F8 */
  IRETURN_V16C8,	     /* MTYPE_V16C8 */
  IRETURN_M8I1,              /* MTYPE_M8I1 */
  IRETURN_M8I2,              /* MTYPE_M8I2 */
  IRETURN_M8I4,              /* MTYPE_M8I4 */
  IRETURN_V16I8,             /* MTYPE_V16I8 */
  IRETURN_V16I1,             /* MTYPE_V16I1 */
#endif
  IRETURN_PPU2,         /* return type of ctype_b_loc() */
  IRETURN_PPI4,         /* return type of ctype_toupper_loc() and ctype_tolower_loc() */
} INTRN_RETKIND;
#define INTRN_RETKIND_LAST IRETURN_F10

// some defines to make parameters more readable
#define BYVAL		TRUE
#define NOT_BYVAL	FALSE
#define PURE		TRUE
#define NOT_PURE	FALSE
#define NO_SIDEEFFECTS	TRUE
#define SIDEEFFECTS	FALSE
#define NEVER_RETURN	TRUE
#define DOES_RETURN	FALSE
#define ACTUAL		TRUE
#define NOT_ACTUAL	FALSE
#define CGINTRINSIC	TRUE
#define NOT_CGINTRINSIC	FALSE

// the info we store for each intrinsic
typedef struct intrn_info_t {
 mBOOL		is_by_val;
 mBOOL		is_pure;
 mBOOL		has_no_side_effects;
 mBOOL		never_returns;
 mBOOL		is_actual;
 mBOOL		is_cg_intrinsic;
 INTRN_RETKIND	return_kind;
 char		*c_name;
 char		*specific_name;
 char		*runtime_name;
} intrn_info_t;

extern const intrn_info_t intrn_info[INTRINSIC_LAST+1];

inline BOOL INTRN_by_value (const INTRINSIC i)
{
  return intrn_info[i].is_by_val;
}

inline BOOL INTRN_is_pure (const INTRINSIC i)
{
  return intrn_info[i].is_pure;
}

inline BOOL INTRN_has_no_side_effects (const INTRINSIC i)
{
  return intrn_info[i].has_no_side_effects;
}

inline BOOL INTRN_never_returns (const INTRINSIC i)
{
  return intrn_info[i].never_returns;
}

inline BOOL INTRN_is_actual (const INTRINSIC i)
{
  return intrn_info[i].is_actual;
}

inline BOOL INTRN_cg_intrinsic (const INTRINSIC i)
{
  return intrn_info[i].is_cg_intrinsic;
}

inline INTRN_RETKIND INTRN_return_kind (const INTRINSIC i)
{
  return intrn_info[i].return_kind;
}

inline char * INTRN_c_name (const INTRINSIC i)
{
  return intrn_info[i].c_name;
}

inline char * INTRN_specific_name (const INTRINSIC i)
{
  return intrn_info[i].specific_name;
}

inline char * INTRN_rt_name (const INTRINSIC i)
{
  return intrn_info[i].runtime_name;
}

#endif

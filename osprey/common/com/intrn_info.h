/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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
#include "mtypes.h"
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
  IRETURN_C10,               /* MTYPE_C10 */
  IRETURN_F16,               /* MTYPE_F16 */
  IRETURN_C16,               /* MTYPE_C16 */
#ifdef TARG_X8664
  IRETURN_V16I2,	     /* MTYPE_V16I2 */
  IRETURN_V16I4,	     /* MTYPE_V16I4 */
  IRETURN_V16F4,	     /* MTYPE_V16F4 */
  IRETURN_V16F8,	     /* MTYPE_V16F8 */
  IRETURN_V16C8,	     /* MTYPE_V16C8 */
  IRETURN_V8I1,              /* MTYPE_V8I1 */
  IRETURN_V8I2,              /* MTYPE_V8I2 */
  IRETURN_V8I4,              /* MTYPE_V8I4 */
  IRETURN_V8I8,              /* MTYPE_V8I8 */
  IRETURN_M8I1,              /* MTYPE_M8I1 */
  IRETURN_M8I2,              /* MTYPE_M8I2 */
  IRETURN_M8I4,              /* MTYPE_M8I4 */
  IRETURN_V16I8,             /* MTYPE_V16I8 */
  IRETURN_V16I1,             /* MTYPE_V16I1 */
  IRETURN_V32C4,             /* MTYPE_V32C4 */
  IRETURN_V32C8,             /* MTYPE_V32C8 */
  IRETURN_V32I1,             /* MTYPE_V32I1 */
  IRETURN_V32I2,             /* MTYPE_V32I2 */
  IRETURN_V32I4,             /* MTYPE_V32I4 */
  IRETURN_V32I8,             /* MTYPE_V32I8 */
  IRETURN_V32F4,             /* MTYPE_V32F4 */
  IRETURN_V32F8,             /* MTYPE_V32F8 */
#endif
  IRETURN_PPU2,         /* return type of ctype_b_loc() */
  IRETURN_PPI4,         /* return type of ctype_toupper_loc() and ctype_tolower_loc() */
} INTRN_RETKIND;
#define INTRN_RETKIND_LAST IRETURN_F10

#if defined(TARG_IA64) || defined(TARG_X8664)
#define IRETURN_LD IRETURN_F10
#else
#define IRETURN_LD IRETURN_FQ
#endif

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

#ifdef TARG_SL
#define NOT_SLAVE       FALSE 
#define SLAVE           TRUE 
#else
#define NOT_SLAVE       CGINTRINSIC
#define SLAVE           NOT_CGINTRINSIC
#endif

// the info we store for each intrinsic
typedef struct intrn_info_t {
 mBOOL		is_by_val;
 mBOOL		is_pure;
 mBOOL		has_no_side_effects;
 mBOOL		never_returns;
 mBOOL		is_actual;
  mBOOL		is_cg_intrinsic;
 mBOOL          slave;
 INTRN_RETKIND	return_kind;
 const char    *c_name;
 const char    *specific_name; // deprecated, but leave in struct for now
 const char    *runtime_name;
} intrn_info_t;

extern const intrn_info_t intrn_info[];

extern TYPE_ID INTRN_Size_Mtype (const INTRINSIC id);

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

inline const char * INTRN_c_name (const INTRINSIC i)
{
  return intrn_info[i].c_name;
}

inline const char * INTRN_specific_name (const INTRINSIC i)
{
  return intrn_info[i].specific_name;
}

inline const char * INTRN_rt_name (const INTRINSIC i)
{
  return intrn_info[i].runtime_name;
}
inline const char * INTRINSIC_name (const INTRINSIC i)
{
  if (INTRN_c_name(i))
	return INTRN_c_name(i);
  else if (INTRN_rt_name(i))
	return INTRN_rt_name(i);
  else
  	return intrn_info[i].specific_name;
}

#if defined(TARG_SL)
inline BOOL INTRN_is_sl (const INTRINSIC i)
{
  if(i>=INTRN_SL_INTRN_BGN && i<=INTRN_SL_INTRN_END) 
    return TRUE;
  else 
    return FALSE;
}

inline BOOL INTRN_is_slave (const INTRINSIC i)
{
  return intrn_info[i].slave;
}

inline BOOL INTRN_copy_addr(const INTRINSIC i)
{
  if(i==INTRN_C3_INIT_ADDR || i==INTRN_C3_SAVE_ADDR ||
      i==INTRN_VBUF_OFFSET || i==INTRN_SBUF_OFFSET)
    return TRUE;
  return FALSE;
}

#define SL_MAX_MEMOP_COUNT 2

typedef struct  {
  INTRINSIC id;
  BOOL specific; //whether the intrinsic by defination specify the memory operation
  BOOL like_store;
  INT32 memop_count; //the count of memory operation carried by the intrinsic
  INT32 addr_id[SL_MAX_MEMOP_COUNT];  //the parameter ids which carry addresses accessed by the intrinsic
  struct{
    INT32 sw_id; //the parameter id which carry the search window mode specification for sl2 intrinsic
    INT32 macro_id; //the parameter id which carry the macro mode specification for sl2 intrinsic
    INT32 size_id;  //the parameter id which carry the size info for sl2 intrinsic
    INT32 size_coeff;  //the size coeff for sl2 intrinsic
    INT32 size_stride;  //the mem operation stride for sl2 intrinsic
    BOOL maybe_strided;  //whether the mem operation is stried for sl2 intrinsic
  }  mem_spec;
} sl_intrn_meminfo_t; 

#define INVALID_PID -1
#define SL_INTRN_MEMINFO_LAST 27 
static sl_intrn_meminfo_t sl_intrn_meminfo_tab[SL_INTRN_MEMINFO_LAST] = {
  // new C3 intrinsics
  INTRN_C3_SET_CIRCBUF, FALSE, FALSE, 2, {3, 4}, {0},
  INTRN_C3DMAC_A, 	FALSE, FALSE, 2, {2, 4}, {0},
  INTRN_C3DMULA_A, 	FALSE, FALSE, 1, {2, INVALID_PID}, {0},
  INTRN_C3LD, 		FALSE, FALSE, 1, {0, INVALID_PID}, {0},
  INTRN_C3ST, 		FALSE, TRUE,  1, {1, INVALID_PID}, {0},
  INTRN_C3MAC_A, 	FALSE, FALSE, 2, {2, 4}, {0},
  INTRN_C3MAC_AR, 	FALSE, FALSE, 1, {3, INVALID_PID}, {0},
  INTRN_C3MULA_A, 	FALSE, FALSE, 1, {2, 4}, {0},
  INTRN_C3MULA_AR, 	FALSE, FALSE, 1, {3, INVALID_PID}, {0},
  INTRN_C3SAADD_A, 	FALSE, FALSE, 2, {0, 2}, {0},
  INTRN_C3SAADDH_A, 	FALSE, FALSE, 2, {0, 2}, {0},
  INTRN_C3SADDA_A, 	FALSE, FALSE, 1, {2, INVALID_PID}, {0},
  INTRN_C3SAMULH_A, 	FALSE, FALSE, 2, {0, 2}, {0},
  //c2 intrinsics
  INTRN_C2_LD_V, 	TRUE, FALSE, 1, {1, INVALID_PID}, {2, 3, 5, 16, 0, FALSE},
  INTRN_C2_LD_G,	TRUE, FALSE, 1, {0, INVALID_PID}, {INVALID_PID, INVALID_PID, 3, 1, 0, FALSE},
  INTRN_C2_LD_V2G, 	TRUE, FALSE, 1, {0, INVALID_PID}, {INVALID_PID, INVALID_PID, 2, 1, 16, TRUE},
  INTRN_C2_LD_V_IMM, 	TRUE, FALSE, 1, {4, INVALID_PID}, {INVALID_PID, 1, 3, 16, 0, FALSE},
  INTRN_C2_LD_C_IMM, 	TRUE, FALSE, 1, {1, INVALID_PID}, {INVALID_PID, INVALID_PID, INVALID_PID, 1, 0, FALSE},
  INTRN_C2_LD_G_IMM, 	TRUE, FALSE, 1, {2, INVALID_PID}, {INVALID_PID, INVALID_PID, 1, 1, 0, FALSE},
  INTRN_C2_LD_V2G_IMM,	TRUE, FALSE, 1, {2, INVALID_PID}, {INVALID_PID, INVALID_PID, 1, 1, 16, TRUE},
  INTRN_C2_ST_V, 	TRUE, TRUE,  1, {1, INVALID_PID}, {INVALID_PID, 2, 3, 16, 0, FALSE},
  INTRN_C2_ST_G, 	TRUE, TRUE,  1, {1, INVALID_PID}, {INVALID_PID, INVALID_PID, 3, 1, 0, FALSE},
  INTRN_C2_ST_G2V, 	TRUE, TRUE,  1, {1, INVALID_PID}, {INVALID_PID, INVALID_PID, 2, 1, 16, TRUE},
  INTRN_C2_ST_V_IMM, 	TRUE, TRUE,  1, {3, INVALID_PID}, {INVALID_PID, 2, 1, 16, 0, FALSE},
  INTRN_C2_ST_C_IMM, 	TRUE, TRUE,  1, {1, INVALID_PID}, {INVALID_PID, INVALID_PID, INVALID_PID, 1, 0, FALSE},
  INTRN_C2_ST_G_IMM, 	TRUE, TRUE,  1, {2, INVALID_PID}, {INVALID_PID, INVALID_PID, 1, 1, 0, FALSE},
  INTRN_C2_ST_G2V_IMM, 	TRUE, TRUE,  1, {2, INVALID_PID}, {INVALID_PID, INVALID_PID, 1, 1, 16, TRUE},
};


inline INT32 INTRN_vbuf_linesize()
{
  return 16;
}

inline INT32 INTRN_vbuf_sw_crossline_size()
{
  return 28;
}

inline INT32 INTRN_vbuf_sw_crossline_hintbofs()
{
  return 4;
}

inline INT32 INTRN_vbuf_sw_crossline_hinthrd()
{
  return 3;
}

inline INT32 INTRN_get_max_scalar_size()
{
  return 4;
}

inline BOOL INTRN_carry_memop(INTRINSIC intrn)
{
  for(INT i=0;i<SL_INTRN_MEMINFO_LAST;i++) {
    if(sl_intrn_meminfo_tab[i].id==intrn)
      return TRUE;
  }
  return FALSE;
}

inline BOOL INTRN_like_store(INTRINSIC intrn) {
  for(INT i=0;i<SL_INTRN_MEMINFO_LAST;i++) {
    if(sl_intrn_meminfo_tab[i].id==intrn && sl_intrn_meminfo_tab[i].like_store==TRUE)
      return TRUE;
  }
  return FALSE;
}

inline BOOL INTRN_specify_memop(INTRINSIC intrn)
{
  for(INT i=0;i<SL_INTRN_MEMINFO_LAST;i++) {
    if(sl_intrn_meminfo_tab[i].id==intrn && sl_intrn_meminfo_tab[i].specific==TRUE)
      return TRUE;
  }
  return FALSE;
}

inline INT32 INTRN_get_memop_count(INTRINSIC intrn) 
{
  for(INT i=0;i<SL_INTRN_MEMINFO_LAST;i++) {
    if(sl_intrn_meminfo_tab[i].id==intrn)
      return sl_intrn_meminfo_tab[i].memop_count;
  }
  return 0;
}

inline INT32 INTRN_get_addr_parm(INTRINSIC intrn, INT id)
{
  for(INT i=0;i<SL_INTRN_MEMINFO_LAST;i++) {
    if(sl_intrn_meminfo_tab[i].id==intrn)
      return sl_intrn_meminfo_tab[i].addr_id[id];
  }
  return INVALID_PID;
}

inline INT32 INTRN_get_size_parm(INTRINSIC intrn)
{
  for(INT i=0;i<SL_INTRN_MEMINFO_LAST;i++) {
    if(sl_intrn_meminfo_tab[i].id==intrn)
      return sl_intrn_meminfo_tab[i].mem_spec.size_id;
  }
  return INVALID_PID;
}

inline INT32 INTRN_get_size(INT i)
{
  switch(i){
    case 0: return 2;
    case 1: return 4;
    case 2: return 1;
    default: 
      return -1;
  }
}

inline INT32 INTRN_get_size_coeff(INTRINSIC intrn)
{
  for(INT i=0;i<SL_INTRN_MEMINFO_LAST;i++) {
    if(sl_intrn_meminfo_tab[i].id==intrn)
      return sl_intrn_meminfo_tab[i].mem_spec.size_coeff;
  }
  return -1;
}

inline INT32 INTRN_get_size_stride(INTRINSIC intrn)
{
  for(INT i=0;i<SL_INTRN_MEMINFO_LAST;i++) {
    if(sl_intrn_meminfo_tab[i].id==intrn)
      return sl_intrn_meminfo_tab[i].mem_spec.size_stride;
  }
  return -1;
}

inline INT32 INTRN_get_macro_parm(INTRINSIC intrn)
{
  for(INT i=0;i<SL_INTRN_MEMINFO_LAST;i++) {
    if(sl_intrn_meminfo_tab[i].id==intrn)
      return sl_intrn_meminfo_tab[i].mem_spec.macro_id;
  }
  return INVALID_PID;
}

inline INT32 INTRN_get_sw_parm(INTRINSIC intrn)
{
  for(INT i=0;i<SL_INTRN_MEMINFO_LAST;i++) {
    if(sl_intrn_meminfo_tab[i].id==intrn)
      return sl_intrn_meminfo_tab[i].mem_spec.sw_id;
  }
  return INVALID_PID;
}

inline BOOL INTRN_maybe_stride(INTRINSIC intrn)
{
  for(INT i=0;i<SL_INTRN_MEMINFO_LAST;i++) {
    if(sl_intrn_meminfo_tab[i].id==intrn)
      return sl_intrn_meminfo_tab[i].mem_spec.maybe_strided;
  }
  return FALSE;
}

#endif // TARG_SL

#endif

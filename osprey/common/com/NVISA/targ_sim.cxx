/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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


/*
 * This defines the ABI subprogram interface,
 * and is used to determine how parameters and results are passed.
 * We have an array of tables, where each table describes the info
 * for one abi.  The array is indexed by the TARGET_ABI enumeration.
 * The register values are the PREG offsets, so these values can be
 * used in WHIRL.
 */

#define TRACE_ENTRY(x)
#define TRACE_EXIT(x)
#define TRACE_EXIT_i(x,i)

#include <stdint.h>
#include <limits.h>
#include "defs.h"
#include "mtypes.h"
#include "errors.h"
#include "erglob.h"
#include "stab.h"
#include "config_targ.h"
#include "targ_sim.h"

#include "targ_sim_body.h"

#define IP0 First_Int32_Preg_Param_Offset
#define LP0 First_Int64_Preg_Param_Offset
#define FP0 First_Float32_Preg_Param_Offset
#define DP0 First_Float64_Preg_Param_Offset
#define IR0 First_Int32_Preg_Return_Offset
#define LR0 First_Int64_Preg_Return_Offset
#define FR0 First_Float32_Preg_Return_Offset
#define DR0 First_Float64_Preg_Return_Offset
#define NP1 MAX_NUMBER_OF_REGISTER_PARAMETERS-1
#define NR1 MAX_NUMBER_OF_REGISTERS_FOR_RETURN-1

#if (__GNUC__ == 2)
static
#endif /* _LP64 */
SIM SIM_Info[] = {
  /* flags */
  /* int args, int64 args, flt args, dbl args */
  /* int res , int64 res, flt res, dbl res */
  /* int type, int64 type, flt type, dbl type */
  /* save area, formal-area, var ofst */
  /* struct arg, struct res, slink, pic */
  {/* ABI_UNDEF */
    0,
    {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0},
    {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0},
    0, 0, 0, 0,
    0, 0, 0, 
    0, 0, 0, 0
  },

  { /* ABI_n32 */
    SIM_FLT_AFTER_INT | SIM_COORD_MEM_REG 
    | SIM_REG_STRUCTS | SIM_FLT_RTN_COMPLEX 
    | SIM_FLT_REG_FIELDS | SIM_DBL_REG_FIELDS | SIM_VARARGS_FLOATS ,
    {IP0,IP0+NP1,1}, {LP0,LP0+NP1,1}, {FP0,FP0+NP1,1}, {DP0,DP0+NP1,1},
    {IR0,IR0+NR1,1}, {LR0,LR0+NR1,1}, {FR0,FR0+NR1,1}, {DR0,DR0+NR1,1},
    MTYPE_I4, MTYPE_I8, MTYPE_F4, MTYPE_F8,
    0, 0, -64/*TODO*/, 
    128, 128, 0, 0
  },

  { /* ABI_n64 */
    SIM_FLT_AFTER_INT | SIM_COORD_MEM_REG 
    | SIM_REG_STRUCTS | SIM_FLT_RTN_COMPLEX 
    | SIM_FLT_REG_FIELDS | SIM_DBL_REG_FIELDS | SIM_VARARGS_FLOATS ,
    {IP0,IP0+NP1,1}, {LP0,LP0+NP1,1}, {FP0,FP0+NP1,1}, {DP0,DP0+NP1,1},
    {IR0,IR0+NR1,1}, {LR0,LR0+NR1,1}, {FR0,FR0+NR1,1}, {DR0,DR0+NR1,1},
    MTYPE_I4, MTYPE_I8, MTYPE_F4, MTYPE_F8,
    0, 64/*TODO*/, -64/*TODO*/, 
    128, 128, 0, 0
  },
  { /* ABI_w64 */
    SIM_FLT_AFTER_INT | SIM_COORD_MEM_REG 
    | SIM_REG_STRUCTS | SIM_FLT_RTN_COMPLEX 
    | SIM_FLT_REG_FIELDS | SIM_DBL_REG_FIELDS | SIM_VARARGS_FLOATS ,
    {IP0,IP0+NP1,1}, {LP0,LP0+NP1,1}, {FP0,FP0+NP1,1}, {DP0,DP0+NP1,1},
    {IR0,IR0+NR1,1}, {LR0,LR0+NR1,1}, {FR0,FR0+NR1,1}, {DR0,DR0+NR1,1},
    MTYPE_I4, MTYPE_I8, MTYPE_F4, MTYPE_F8,
    0, 64/*TODO*/, -64/*TODO*/, 
    128, 128, 0, 0
  }
};

/* return whether preg is a return preg */
extern BOOL 
Is_Return_Preg (PREG_NUM preg)
{
	return (preg >= First_Int32_Preg_Return_Offset &&
	        preg <= Last_Int32_Preg_Return_Offset) || 
	       (preg >= First_Float32_Preg_Return_Offset && 
		preg <= Last_Float32_Preg_Return_Offset) ||
	       (preg >= First_Int64_Preg_Return_Offset && 
		preg <= Last_Int64_Preg_Return_Offset) ||
	       (preg >= First_Float64_Preg_Return_Offset && 
		preg <= Last_Float64_Preg_Return_Offset);
}

/* return whether preg is an output preg */
extern BOOL 
Is_Int_Output_Preg (PREG_NUM preg)
{
  Fail_FmtAssertion (
    ("Is_Int_Output_Preg not applicable to x8664 targets"));
  return FALSE;
}

/* return whether preg is an input preg */
extern BOOL
Is_Formal_Preg (PREG_NUM preg)
{
	return (preg >= First_Int32_Preg_Param_Offset && 
		preg <= Last_Int32_Preg_Param_Offset) || 
	       (preg >= First_Float32_Preg_Param_Offset && 
		preg <= Last_Float32_Preg_Param_Offset) ||
	       (preg >= First_Int64_Preg_Param_Offset && 
		preg <= Last_Int64_Preg_Param_Offset) || 
	       (preg >= First_Float64_Preg_Param_Offset && 
		preg <= Last_Float64_Preg_Param_Offset);
}


/* This routine figures out the mtypes of the return registers that are 
 * used for returning an object of the given type.
 * This returns the mtypes to use for the CALL opcode in high-level whirl.
 * This means that returns of simulated objects, like FQ, are just shown
 * as returning FQ, which will later be split into F8F8.
 * However, structures that return in registers are specified explicitly.
 */
/*ARGSUSED*/
extern void
Get_Return_Mtypes (
  TY_IDX rtype,		/* The result type */
  Mtype_Return_Level level,	/* whether to lower the mtypes */
  TYPE_ID *mreg1,	/* out: mtype for result register 1 */
  TYPE_ID *mreg2)	/* out: mtype for result register 2 */
{
  Fail_FmtAssertion (
    ("Get_Return_Mtypes should not be invoked; invoke Get_Return_Info instead"));
}

/* This routine figures out which return registers are to be used
 * for returning an object with the given mtypes.
 * It is assumed that the mtypes will be determined by calling
 * Get_Return_Mtypes.
 */
/*ARGSUSED*/
extern void
Get_Return_Pregs (
  TYPE_ID mreg1,	/* in:  mtype for result register 1 */
  TYPE_ID mreg2,	/* in:  mtype for result register 2 */
  PREG_NUM *rreg1,	/* out: result register 1 */
  PREG_NUM *rreg2)	/* out: result register 2 */
{
  Fail_FmtAssertion (
    ("Get_Return_Pregs should not be invoked; invoke Get_Return_Info instead"));
}

RETURN_INFO
Get_Return_Info(TY_IDX rtype, Mtype_Return_Level level)
{
  TYPE_ID mtype = TY_mtype (rtype);
  RETURN_INFO info;
  INT32 i; 
  INT64 size;

  info.return_via_first_arg = FALSE;

  switch (mtype) {

    case MTYPE_UNKNOWN:

      // FORTRAN character array
      info.count = 0;
      // f90 already has made visible the arg for arrays
      // info.return_via_first_arg = TRUE;
      break;

    case MTYPE_FQ:

      info.count = 1;
      info.mtype[0] = mtype;
      info.preg[0] = PR_first_reg(SIM_INFO.flt_results);
      break;

    case MTYPE_V:

      info.count = 0;
      break;

    case MTYPE_I8:
    case MTYPE_U8:
    case MTYPE_A8:
      info.count = 1;
      info.mtype [0] = mtype;
      info.preg  [0] = PR_first_reg(SIM_INFO.int64_results);
      break;

    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
    case MTYPE_A4:

      info.count = 1;
      info.mtype [0] = mtype;
      info.preg  [0] = PR_first_reg(SIM_INFO.int_results);
      break;

    case MTYPE_F4:
#ifdef TARG_SUPPORTS_VECTORS
    case MTYPE_V16F4:
#endif
      info.count = 1;
      info.mtype [0] = mtype;
      info.preg  [0] = PR_first_reg(SIM_INFO.flt_results);
      break;
    case MTYPE_F8:
#ifdef TARG_SUPPORTS_VECTORS
    case MTYPE_V16F8:
#endif
      info.count = 1;
      info.mtype [0] = mtype;
      info.preg  [0] = PR_first_reg(SIM_INFO.dbl_results);
      break;

    case MTYPE_C4:
      if (Is_Target_32bit()) {
	/* Under -m32 for C and C++, if type "float _Complex" is passed as argument,
	   there is no need to introduce a fake first parameter; and if it is a return
	   value, the real part and the imaginary part are set aside in
	   %eax and %edx, respectively.    (bug#2707)
	 */
	if( PU_c_lang(Get_Current_PU()) ||
	    PU_cxx_lang(Get_Current_PU()) ){

	  if( level == Use_Simulated ){
	    info.count = 1;
	    info.mtype[0] = mtype;
	    info.preg[0] = PR_first_reg(SIM_INFO.flt_results);

	  } else {
	    info.count = 2;
	    info.mtype[0] = info.mtype[1] = SIM_INFO.int_type;
	    info.preg[0] = PR_first_reg(SIM_INFO.int_results);
	    info.preg[1] = info.preg[0] + PR_skip_value(SIM_INFO.int_results);
	  }

	} else {
	  info.count = 0;
	  info.return_via_first_arg = TRUE;
	}
      } else if( level == Use_Simulated ){
	info.count     = 1;
	info.mtype [0] = mtype;
	info.preg  [0] = PR_first_reg(SIM_INFO.flt_results);

      } else {
	// For bug:143
	
        info.count     = 2;
        info.mtype [0] = Mtype_complex_to_real(mtype);
        info.mtype [1] = Mtype_complex_to_real(mtype);
        info.preg  [0] = PR_first_reg(SIM_INFO.flt_results);
        info.preg  [1] =   PR_first_reg(SIM_INFO.flt_results)
                         + PR_skip_value(SIM_INFO.flt_results);
      }

      break;

    case MTYPE_C8:
      if (Is_Target_32bit()) {
        info.count = 0;
        info.return_via_first_arg = TRUE;
      } else if (level == Use_Simulated) {

        info.count     = 1;
        info.mtype [0] = mtype;
        info.preg  [0] = PR_first_reg(SIM_INFO.dbl_results);
      }

      else {

        info.count     = 2;
        info.mtype [0] = Mtype_complex_to_real(mtype);
        info.mtype [1] = Mtype_complex_to_real(mtype);
        info.preg  [0] = PR_first_reg(SIM_INFO.dbl_results);
        info.preg  [1] =   PR_first_reg(SIM_INFO.dbl_results)
                         + PR_skip_value(SIM_INFO.dbl_results);
      }
      break;

    case MTYPE_CQ:
      if (Is_Target_32bit()) {
        info.count = 0;
        info.return_via_first_arg = TRUE;
      }
      else if (level == Use_Simulated) {

        info.count     = 1;
        info.mtype [0] = mtype;
        info.preg  [0] = PR_first_reg(SIM_INFO.flt_results);
      }

      else {

        info.count     = 2;
        info.mtype [0] = Mtype_complex_to_real(mtype);
        info.mtype [1] = Mtype_complex_to_real(mtype);
        info.preg  [0] = PR_first_reg(SIM_INFO.flt_results);
        info.preg  [1] =   PR_first_reg(SIM_INFO.flt_results)
                         + PR_skip_value(SIM_INFO.flt_results);
      }
      break;

    case MTYPE_M:

      info.count = 0;

      size = TY_size(Ty_Table[rtype]);
      if (size == 0)
	break;

      if (size*8 <= SIM_INFO.max_struct_result
        && TY_can_be_vector(rtype) 
        && TY_vector_count(rtype) <= MAX_NUMBER_OF_REGISTERS_FOR_RETURN) 
      {
        Preg_Range prange;
        TYPE_ID etype = TY_mtype(TY_vector_elem_ty(rtype));
        switch (etype) {
        case MTYPE_F4:
          prange = SIM_INFO.flt_results; break;
        case MTYPE_F8:
          prange = SIM_INFO.dbl_results; break;
        case MTYPE_I4:
        case MTYPE_U4:
          prange = SIM_INFO.int_results; break;
        case MTYPE_I8:
        case MTYPE_U8:
          prange = SIM_INFO.int64_results; break;
        default:
          FmtAssert(FALSE,("NYI"));
        }
        PREG_NUM preg = PR_first_reg(prange);
        info.count = TY_vector_count(rtype);
        for (i = 0; i < info.count; i++) {
          info.mtype [i] = etype;
          info.preg  [i] = preg;
          preg += PR_skip_value(prange);
        }
      }
      else {
        info.return_via_first_arg = TRUE;
      }
      break;

    default:

      info.count = 0;
      Fail_FmtAssertion ("Invalid return mtype %s encountered",
                         (MTYPE_name(mtype)));
      break;
  } /* switch (mtype) */

  for (i = info.count; i < MAX_NUMBER_OF_REGISTERS_FOR_RETURN; i++) {

    info.mtype [i] = MTYPE_V;
    info.preg  [i] = 0;
  }

  return info;
} /* Get_Return_Info */

static PLOC
Setup_Parameter_Locations (TY_IDX pu_type)
{
    static PLOC plocNULL;

    TY_IDX ret_type = (TY_kind(pu_type) == KIND_FUNCTION ? TY_ret_type(pu_type)
			: pu_type);
    RETURN_INFO info = Get_Return_Info (ret_type, No_Simulated);
    if (TY_is_varargs (pu_type)) {
	// find last fixed parameter
	TYLIST_IDX idx = TY_tylist (pu_type);
	Last_Fixed_Param = -1;
	for (++idx; Tylist_Table[idx] != 0; ++idx)
	    ++Last_Fixed_Param;
	// old style varargs is counting va_alist and should not
	if ( ! TY_has_prototype(pu_type))
	    --Last_Fixed_Param;
	// account for functions returning to first parameter
	if (TY_return_to_param (pu_type))
	    ++Last_Fixed_Param;
    } else
	Last_Fixed_Param = INT_MAX;

    Current_Param_Num = -1;		// count all parameters
    Last_Param_Offset = 0;
    return plocNULL;
} // Setup_Parameter_Locations


static PLOC
Get_Parameter_Location (TY_IDX ty, BOOL is_output)
{
    PLOC ploc;				// return location

    ploc.reg = 0;
    ploc.start_offset = Last_Param_Offset;
    ploc.size = 0;
    ploc.vararg_reg = 0;               // to silence purify
    if (TY_kind (ty) == KIND_VOID) {
	return ploc;
    }

    /* check for array case where fe doesn't fill in right btype */
    TYPE_ID pmtype = Fix_TY_mtype (ty);	/* Target type */
    ploc.size = MTYPE_RegisterSize(pmtype);

    ++Current_Param_Num;

    INT rpad = 0;			/* padding to right of object */

    // do need to pad when start offset not already aligned
    rpad = (ploc.start_offset % TY_align(ty));

    switch (pmtype) {
	
    case MTYPE_I1:
    case MTYPE_U1:
    case MTYPE_I2:
    case MTYPE_U2:
    case MTYPE_I4:
    case MTYPE_U4:
    case MTYPE_A4:
	ploc.reg = PR_first_reg(SIM_INFO.int_args) + Current_Param_Num;
	if (ploc.reg > PR_last_reg(SIM_INFO.int_args)) 
	  ploc.reg = 0;
	break;

    case MTYPE_I8:
    case MTYPE_U8:
    case MTYPE_A8:
	ploc.reg = PR_first_reg(SIM_INFO.int64_args) + Current_Param_Num;
	if (ploc.reg > PR_last_reg(SIM_INFO.int64_args)) 
	  ploc.reg = 0;
	break;
	
#ifdef TARG_SUPPORTS_VECTORS
    case MTYPE_V16I4:
    case MTYPE_V16F4:
#endif
    case MTYPE_F4:
	ploc.reg = PR_first_reg(SIM_INFO.flt_args) + Current_Param_Num;
	if (ploc.reg > PR_last_reg(SIM_INFO.flt_args)) {
	  ploc.reg = 0;
	  /* ptx param space has no padding */
	  // if( Is_Target_64bit() )
	  //   rpad = MTYPE_RegisterSize(SIM_INFO.flt_type) - ploc.size;
	}
	break;
#ifdef TARG_SUPPORTS_VECTORS
    case MTYPE_V16F8:
#endif
    case MTYPE_F8:
	ploc.reg = PR_first_reg(SIM_INFO.dbl_args) + Current_Param_Num;
	if (ploc.reg > PR_last_reg(SIM_INFO.dbl_args)) {
	  ploc.reg = 0;
	}
	break;

#ifdef TARG_SUPPORTS_VECTORS
    case MTYPE_V8I1:
    case MTYPE_V8I2:
    case MTYPE_V8I4:
    case MTYPE_V8I8:
      ploc.reg = 0; // pass in memory
      break;
#endif

    case MTYPE_CQ:
    case MTYPE_FQ:
      ploc.reg = 0;  /* pass in memory */
      break;

    case MTYPE_C4:
        ++Current_Param_Num;
	ploc.reg = PR_first_reg(SIM_INFO.flt_args) + Current_Param_Num;
	if (ploc.reg > PR_last_reg(SIM_INFO.flt_args)) 
	  ploc.reg = 0;
	break;
	
    case MTYPE_C8:
        ++Current_Param_Num;
	ploc.reg = PR_first_reg(SIM_INFO.dbl_args) + Current_Param_Num;
	if (ploc.reg > PR_last_reg(SIM_INFO.dbl_args)) {
          --Current_Param_Num;
	  ploc.reg = 0;	/* pass in memory */
	}
	else {
          ++Current_Param_Num;
	}
	break;

    case MTYPE_M:
	ploc.size = TY_size (ty);
	/* default is to pass whole struct in memory */
	ploc.reg = 0;

        if (ploc.size*8 <= SIM_INFO.max_struct_size
          && TY_can_be_vector(ty) 
          && (Current_Param_Num + TY_vector_count(ty)) 
            <= MAX_NUMBER_OF_REGISTER_PARAMETERS)
        {
          Preg_Range prange;
          switch (TY_mtype(TY_vector_elem_ty(ty))) {
          case MTYPE_F4:
            prange = SIM_INFO.flt_args; break;
          case MTYPE_F8:
            prange = SIM_INFO.dbl_args; break;
          case MTYPE_I4:
          case MTYPE_U4:
          case MTYPE_I2:
          case MTYPE_U2:
          case MTYPE_I1:
          case MTYPE_U1:
            prange = SIM_INFO.int_args; break;
          case MTYPE_I8:
          case MTYPE_U8:
            prange = SIM_INFO.int64_args; break;
          default:
            FmtAssert(FALSE,("NYI"));
          }
          ploc.reg = PR_first_reg(prange) + Current_Param_Num;
          Current_Param_Num += TY_vector_count(ty) - 1;
        }
	break;
	
    default:
	FmtAssert (FALSE, ("Get_Parameter_Location:  mtype %s",
			   MTYPE_name(pmtype)));
    }
    if (ploc.reg == 0)
      Last_Param_Offset = ploc.start_offset + ploc.size + rpad;
    return ploc;
} // Get_Parameter_Location

struct PSTRUCT {
  BOOL	first_call;
  TYPE_ID fldtype;
  INT64	size;
  INT64	offset;
};

struct PSTRUCT pstruct;

static void
Setup_Struct_Parameter_Locations (TY_IDX struct_ty)
{
  pstruct.first_call = TRUE;
  pstruct.size = TY_size(struct_ty);
  if (TY_can_be_vector(struct_ty)) 
    pstruct.fldtype = TY_mtype(TY_vector_elem_ty(struct_ty));
  else
    pstruct.fldtype = MTYPE_U4; // default to 32bit int
}

static PLOC 
Get_Struct_Parameter_Location (PLOC prev)
{
  PLOC next;
  PLOC_size(next) = MTYPE_RegisterSize(pstruct.fldtype);
  if (pstruct.first_call) {
    pstruct.first_call = FALSE;
    PLOC_offset(next) = PLOC_offset(prev);
    pstruct.offset = PLOC_offset(prev);
    PLOC_reg(next) = PLOC_reg(prev);
  }
  else {
    PLOC_offset(next) = PLOC_offset(prev) + PLOC_size(prev);
    PLOC_reg(next) = PLOC_reg(prev) + 1;
  }
  if (PLOC_reg(prev) == 0)
    PLOC_reg(next) = 0;
  if (PLOC_offset(next) >= pstruct.offset + pstruct.size) {
    PLOC_size(next) = 0;
    return next;
  }

  return next;
} // Get_Struct_Parameter_Location


/* Iterate over vararg non-fixed parameters */
static PLOC
Get_Vararg_Parameter_Location (PLOC prev)
{
  FmtAssert(FALSE, ("varargs not supported"));
}

BOOL Is_Caller_Save_GP;  /* whether GP is caller-save */

INT Formal_Save_Area_Size = 0;
INT Stack_Offset_Adjustment = 0;

extern void 
Init_Targ_Sim (void)
{
	Is_Caller_Save_GP = SIM_caller_save_gp;
}


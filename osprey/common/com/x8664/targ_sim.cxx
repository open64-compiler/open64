/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007, 2008 PathScale, LLC.  All rights reserved.
 */
/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
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


#if (__GNUC__ == 2)
static
#endif /* _LP64 */
SIM SIM_Info[] = {
  /* flags */
  /* int args, flt args, dbl args */
  /* int res , flt res, dbl res */
  /* int type, flt type, dbl type */
  /* save area, formal-area, var ofst */
  /* struct arg, struct res, slink, pic */
  {/* ABI_UNDEF */
    0,
    {0,0,0}, {0,0,0}, {0,0,0},
    {0,0,0}, {0,0,0}, {0,0,0},
    0, 0, 0,
    0, 0, 0, 
    0, 0, 0, 0
  },

  { /* ABI_n32 */
    SIM_FLT_AFTER_INT | SIM_COORD_MEM_REG 
    | SIM_REG_STRUCTS | SIM_FLT_RTN_COMPLEX 
    | SIM_FLT_REG_FIELDS | SIM_DBL_REG_FIELDS | SIM_VARARGS_FLOATS ,
    {0,0,0}, {0,0,0}, {0,0,0},
    {RAX,RDX,RDX-RAX}, {ST0,ST1,1}, {ST0,ST1,1},
    MTYPE_I4, MTYPE_F8, MTYPE_F8,
    0, 0, -64/*TODO*/, 
#if defined(BUILD_OS_DARWIN)
    /* Structures <= 8 bytes return via registers */
    0, 64, RCX, 0
#else /* defined(BUILD_OS_DARWIN) */
    0, 0, RCX, 0
#endif /* defined(BUILD_OS_DARWIN) */
  },

  { /* ABI_n64 */
    SIM_FLT_AFTER_INT | SIM_COORD_MEM_REG 
    | SIM_REG_STRUCTS | SIM_FLT_RTN_COMPLEX 
    | SIM_FLT_REG_FIELDS | SIM_DBL_REG_FIELDS | SIM_VARARGS_FLOATS ,
    {RDI,R9,1},        {XMM0,XMM7,1}, {XMM0,XMM7,1},
    {RAX,RDX,RDX-RAX}, {XMM0,XMM1,1}, {XMM0,XMM1,1},
    MTYPE_I8, MTYPE_F8, MTYPE_F8,
    0, 64/*TODO*/, -64/*TODO*/, 
    128, 128, R10, 0
  }
};

/* return whether preg is a return preg */
extern BOOL 
Is_Return_Preg (PREG_NUM preg)
{
	return (preg == First_Int_Preg_Return_Offset ||
	        preg == Last_Int_Preg_Return_Offset) || 
	       (preg >= First_X87_Preg_Return_Offset && 
	        preg <= Last_X87_Preg_Return_Offset) ||
	       (preg >= First_Float_Preg_Return_Offset && 
	        preg <= Last_Float_Preg_Return_Offset) ||
	       (preg >= First_MMX_Preg_Return_Offset && 
	        preg <= Last_MMX_Preg_Return_Offset);
}

/* return whether preg is an output preg */
extern BOOL 
Is_Int_Output_Preg (PREG_NUM preg)
{
  Fail_FmtAssertion (
    ("Is_Int_Output_Preg not applicable to x8664 targets"));
}

/* return whether preg is an input preg */
extern BOOL
Is_Formal_Preg (PREG_NUM preg)
{
	return (preg >= First_Int_Preg_Param_Offset && 
		preg <= Last_Int_Preg_Param_Offset) || 
	       (preg >= First_Float_Preg_Param_Offset && 
		preg <= Last_Float_Preg_Param_Offset)||
	       (preg >= First_MMX_Preg_Return_Offset && 
		preg <= Last_MMX_Preg_Return_Offset);
}

/* return the result of merging class1 and class2, implemented according to
 * the X86-64 ABI */
static enum X86_64_PARM_CLASS Merge_Classes(enum X86_64_PARM_CLASS class1,
					   enum X86_64_PARM_CLASS class2)
{
  /* rule 1: if the two classes are equal, this is the resulting class */
  if (class1 == class2)
    return class1;

  /* rule 2: if one of the classes is NO_CLASS, return the other class */
  if (class1 == X86_64_NO_CLASS)
    return class2;
  if (class2 == X86_64_NO_CLASS)
    return class1;

  /* rule 3: if one of the classes is MEMORY, result is MEMORY */
  if (class1 == X86_64_MEMORY_CLASS || class2 == X86_64_MEMORY_CLASS)
    return X86_64_MEMORY_CLASS;

  /* rule 4: if one of the classes is INTEGER, result is INTEGER */
  if (class1 == X86_64_INTEGER_CLASS || class2 == X86_64_INTEGER_CLASS)
    return X86_64_INTEGER_CLASS;

  /* rule 5: if one of the classes is X87, X87UP, or COMPLEX_X87 class,
     result is MEMORY */
  if (class1 == X86_64_X87_CLASS || class2 == X86_64_X87_CLASS ||
      class1 == X86_64_X87UP_CLASS || class2 == X86_64_X87UP_CLASS ||
      class1 == X86_64_COMPLEX_X87_CLASS ||
      class2 == X86_64_COMPLEX_X87_CLASS)
    return X86_64_MEMORY_CLASS;
  
  /* rule 6: otherwise, SSE class */
  return X86_64_SSE_CLASS;
}

/* return the number of doublewords passed in register, with classes giving
 * the register class used; return 0 if whole aggregate is passed in memory.
 * Implemented according to the X86-64 ABI */
INT Classify_Aggregate(const TY_IDX ty, 
			  enum X86_64_PARM_CLASS classes[MAX_CLASSES], INT byte_offset )
{
  INT i, n;
  enum X86_64_PARM_CLASS subclasses[MAX_CLASSES];

  if(TY_size(ty) == 0 && TY_kind(ty) == KIND_STRUCT && PU_cxx_lang(Get_Current_PU()) ){
   classes[0] = X86_64_SSE_CLASS;
   return 1;
  }

  if (TY_size(ty) > 16 || TY_size(ty) == 0 ||
      TY_is_non_pod(ty) /* || TY_is_packed(ty) bug 11892 */)
    return 0;
  byte_offset %= 8;
  INT size_in_dwords = (TY_size(ty) + byte_offset + 7) / 8;
  
  for (i = 0; i < size_in_dwords; i++)
    classes[i] = X86_64_NO_CLASS;
  if (TY_kind(ty) == KIND_STRUCT ||
      TY_kind(ty) == KIND_ARRAY) {
    if (TY_kind(ty) == KIND_STRUCT && Ty_Table[ty].Fld()) {
      FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));
      do {
        FLD_HANDLE fld(fld_iter);
        if (TY_size(FLD_type(fld)) == 0 && 
	    !(TY_kind(FLD_type(fld)) == KIND_STRUCT && PU_cxx_lang(Get_Current_PU())))
	  continue;
        n = Classify_Aggregate(FLD_type(fld), subclasses, byte_offset + FLD_ofst(fld));
        if (n == 0)
	  return 0;
        INT inx = (FLD_ofst(fld) + byte_offset) / 8;
        for (i = 0; i < n; i++) 
	  classes[i + inx] = Merge_Classes(classes[i + inx], subclasses[i]);
      } while (!FLD_last_field(fld_iter++));
    }
    else { /* (TY_kind(ty) == KIND_ARRAY) */
      n = Classify_Aggregate(TY_etype(ty), subclasses, byte_offset);
      if (n == 0)
        return 0;
      for (i = 0; i < size_in_dwords; i++)
        classes[i] = subclasses[i % n];
    }
    /* post merger cleanup */
    for (i = 0; i < size_in_dwords; i++) {
      if (classes[i] == X86_64_MEMORY_CLASS)
	return 0;
    }
    return size_in_dwords;
  }
  else 
    switch (TY_mtype(ty)) {
    case MTYPE_I1:
    case MTYPE_U1:
    case MTYPE_I2:
    case MTYPE_U2:
    case MTYPE_I4:
    case MTYPE_U4:
    case MTYPE_A4:
    case MTYPE_I8:
    case MTYPE_U8:
    case MTYPE_A8:
      classes[0] = X86_64_INTEGER_CLASS;
      return 1;
    case MTYPE_F4:
    case MTYPE_F8:
      classes[0] = X86_64_SSE_CLASS;
      return 1;
    case MTYPE_F10:
      classes[0] = X86_64_X87_CLASS;
      classes[1] = X86_64_X87UP_CLASS;
      return 2;
    case MTYPE_C10:
      classes[0] = X86_64_COMPLEX_X87_CLASS;
      return 1;
    case MTYPE_C4:
      classes[0] = X86_64_SSE_CLASS;
      return 1;
    case MTYPE_C8:
      classes[0] = X86_64_SSE_CLASS;
      classes[1] = X86_64_SSE_CLASS;
      return 2;
    case MTYPE_V16F4:
    case MTYPE_V16F8:
    case MTYPE_V16C4:
    case MTYPE_V16C8:
      classes[0] = X86_64_SSE_CLASS;
      classes[1] = X86_64_SSEUP_CLASS;
      return 2;
    case MTYPE_V16I1:
    case MTYPE_V16I2:
    case MTYPE_V16I4:
    case MTYPE_V16I8:
      classes[0] = X86_64_INTEGER_CLASS;
      classes[1] = X86_64_INTEGER_CLASS;
      return 2;
    case MTYPE_V8F4:
      classes[0] = X86_64_SSE_CLASS;
      return 1;
    case MTYPE_V8I1:
    case MTYPE_V8I2:
    case MTYPE_V8I4:
    case MTYPE_V8I8:
    case MTYPE_M8I1:
    case MTYPE_M8I2:
    case MTYPE_M8I4:
      classes[0] = X86_64_INTEGER_CLASS;
      return 1;
    case MTYPE_M8F4:
      classes[0] = X86_64_X87_CLASS;
      return 1;
    case MTYPE_V32I1:
    case MTYPE_V32I2:
    case MTYPE_V32I4:
    case MTYPE_V32I8:
      classes[0] = X86_64_INTEGER_CLASS;
      classes[1] = X86_64_INTEGER_CLASS;
      classes[2] = X86_64_INTEGER_CLASS;
      classes[3] = X86_64_INTEGER_CLASS;
      return 4;
    case MTYPE_V32F4:
    case MTYPE_V32F8:
    case MTYPE_V32C4:
    case MTYPE_V32C8:
      classes[0] = X86_64_SSE_CLASS;
      classes[1] = X86_64_SSEUP_CLASS;
      classes[2] = X86_64_SSEUP_CLASS;
      classes[3] = X86_64_SSEUP_CLASS;
      return 4;
    default:
	FmtAssert (FALSE, ("Classify_Aggregate:  mtype %s",
			   MTYPE_name(TY_mtype(ty))));
    }
}
#if defined(BUILD_OS_DARWIN)
/* Like Classify_Aggregate(), but for Darwin IA32. According to
 * developer.apple.com, 4-byte structures are passed in EAX, 8-byte
 * structures are passed in EAX plus EDX, and larger structures are
 * passed by reference. However, I observe that the gcc provided by
 * Apple passes a structure containing a single real or a single
 * double using the X87 instead. */
INT Classify_Aggregate32(const TY_IDX ty, 
			  enum X86_64_PARM_CLASS classes[MAX_CLASSES])
{
  enum X86_64_PARM_CLASS subclasses[MAX_CLASSES];

  if (TY_size(ty) > 8 || TY_size(ty) == 0 ||
      TY_is_non_pod(ty) /* || TY_is_packed(ty) bug 11892 */)
    return 0;

  INT size_in_words = (TY_size(ty) + 3) / 4;
  classes[0] = X86_64_INTEGER_CLASS;
  classes[1] = X86_64_NO_CLASS;

  FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));
  FLD_HANDLE fld(fld_iter);
  TYPE_ID first_field_type = TY_mtype(FLD_type(fld));
  if (size_in_words == 1 && first_field_type == MTYPE_F4) {
    classes[0] = X86_64_X87_CLASS;
  }
  else if (size_in_words == 2) {
    if (first_field_type == MTYPE_F8) {
      size_in_words = 1;
      classes[0] = X86_64_X87_CLASS;
    }
    else {
      classes[1] = X86_64_INTEGER_CLASS;
    }
  }
  return size_in_words;
}
#endif /* defined(BUILD_OS_DARWIN) */

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
Get_Return_Info(TY_IDX rtype, Mtype_Return_Level level, BOOL ff2c_abi)
{
  TYPE_ID mtype = TY_mtype (rtype);
  RETURN_INFO info;
  INT32 i; 
  INT64 size;

  BOOL ssereg_parms_enabled = Is_Target_32bit() &&
                   TY_has_sseregister_parm (PU_prototype (Get_Current_PU()));
  info.return_via_first_arg = FALSE;

  switch (mtype) {

    case MTYPE_UNKNOWN:

      // FORTRAN character array
      info.count = 0;
      // f90 already has made visible the arg for arrays
      // info.return_via_first_arg = TRUE;
      break;

    case MTYPE_F10:

      info.count = 1;
      info.mtype[0] = mtype;
      info.preg[0]  = First_X87_Preg_Return_Offset;
      break;

    case MTYPE_V:

      info.count = 0;
      break;

    case MTYPE_I8:
    case MTYPE_U8:
      if( Is_Target_32bit() &&
	  level == No_Simulated ){
	info.count = 2;
	info.mtype[0] = info.mtype[1] = ( mtype == MTYPE_I8 ? MTYPE_I4 : MTYPE_U4 );
	info.preg[0] = PR_first_reg(SIM_INFO.int_results);
        info.preg[1] =
	  PR_first_reg(SIM_INFO.int_results) + PR_skip_value(SIM_INFO.int_results);

	break;
      }

    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
    case MTYPE_A4:
    case MTYPE_A8:

      info.count = 1;
      info.mtype [0] = mtype;
      info.preg  [0] = PR_first_reg(SIM_INFO.int_results);
      break;

    case MTYPE_F4:
    case MTYPE_F8:
      info.count = 1;
      info.mtype [0] = mtype;
      if (ssereg_parms_enabled)
        info.preg  [0] = XMM0;
      else
        info.preg  [0] = PR_first_reg(SIM_INFO.flt_results);
      break;

    case MTYPE_V8I1:
    case MTYPE_V8I2:
    case MTYPE_V8I4:
    case MTYPE_V8I8:
    case MTYPE_V8F4:
    case MTYPE_M8I1:
    case MTYPE_M8I2:
    case MTYPE_M8I4:
    case MTYPE_M8F4:
      info.count = 1;
      info.mtype [0] = mtype;
      if (Is_Target_32bit() && Target_MMX) {
        info.preg  [0] = MM0;
      }
      else {
        info.preg  [0] = PR_first_reg(SIM_INFO.flt_results);
      }
      break;
    case MTYPE_V16I1:
    case MTYPE_V16I2:
    case MTYPE_V16I4:
    case MTYPE_V16I8:
    case MTYPE_V16F4:
    case MTYPE_V16F8:
    case MTYPE_V32I1:
    case MTYPE_V32I2:
    case MTYPE_V32I4:
    case MTYPE_V32I8:
    case MTYPE_V32F4:
    case MTYPE_V32F8:
    case MTYPE_V32C4:
    case MTYPE_V32C8:
      info.count = 1;
      info.mtype [0] = mtype;
      if (Is_Target_32bit() && Target_SSE) {
        info.preg  [0] = XMM0;
      }
      else {
        info.preg  [0] = PR_first_reg(SIM_INFO.flt_results);
      }
      break;

    case MTYPE_C4:
      if (Is_Target_32bit()) {
	/* Under -m32 for C and C++, if type "float _Complex" is passed as argument,
	   there is no need to introduce a fake first parameter; and if it is a return
	   value, the real part and the imaginary part are set aside in
	   %eax and %edx, respectively.    (bug#2707)
	 */
	if( 
#if defined(BUILD_OS_DARWIN)
	    1 /* Darwin -m32 returns Fortran SP complex in registers too */
#else /* defined(BUILD_OS_DARWIN) */
	    PU_c_lang(Get_Current_PU()) ||
	    PU_cxx_lang(Get_Current_PU())
#endif /* defined(BUILD_OS_DARWIN) */
	    ){

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
      }
      else if (ff2c_abi || F2c_Abi) { // bug 1664
        info.count = 0;
        info.return_via_first_arg = TRUE;
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
      }
      else if (ff2c_abi || F2c_Abi) { // bug 1664
        info.count = 0;
        info.return_via_first_arg = TRUE;
      } else if (level == Use_Simulated) {

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

    case MTYPE_C10:
      if (Is_Target_32bit()) {
        info.count = 0;
        info.return_via_first_arg = TRUE;
      }
      else if (level == Use_Simulated) {

        info.count     = 1;
        info.mtype [0] = mtype;
        info.preg  [0] = First_X87_Preg_Return_Offset;
      }
      else {
        info.count     = 2;
        info.mtype [0] = Mtype_complex_to_real(mtype);
        info.mtype [1] = Mtype_complex_to_real(mtype);
        info.preg  [0] = First_X87_Preg_Return_Offset;
        info.preg  [1] = Last_X87_Preg_Return_Offset;
      }
      break;


    case MTYPE_M:

      info.count = 0;

      size = TY_size(Ty_Table[rtype]);
      if (size == 0)
	break;

      info.return_via_first_arg = TRUE;

#if defined(BUILD_OS_DARWIN)
      /* For Darwin, even -m32 returns small structures in registers */
#else /* defined(BUILD_OS_DARWIN) */
      if (Is_Target_32bit())
	break;
#endif /* defined(BUILD_OS_DARWIN) */

      if (SIM_INFO.max_struct_result != 0) {

        if (size > 0 && 8 * size <= SIM_INFO.max_struct_result) {
	    PREG_NUM next_float_return_num, next_int_return_num;
	    enum X86_64_PARM_CLASS classes[MAX_CLASSES];
#if defined(BUILD_OS_DARWIN)
            INT n = Is_Target_32bit() ?
	      Classify_Aggregate32(rtype, classes) :
	      Classify_Aggregate(rtype, classes);
#else /* defined(BUILD_OS_DARWIN) */
	    INT n = Classify_Aggregate(rtype, classes);
#endif /* defined(BUILD_OS_DARWIN) */
	    if (n > 0) {
              info.return_via_first_arg = FALSE;
              info.count = n;

	      if (classes[0] == X86_64_X87_CLASS) {
                  info.count = 1;
                  info.mtype[0] = MTYPE_F10;
                  info.preg[0]  = First_X87_Preg_Return_Offset;
              }
              else if (classes[0] == X86_64_COMPLEX_X87_CLASS) {
                  if (Is_Target_32bit()) {
                    info.count = 0;
                    info.return_via_first_arg = TRUE;
                  }
                  else if (level == Use_Simulated) {
            
                    info.count     = 1;
                    info.mtype [0] = MTYPE_C10;
                    info.preg  [0] = First_X87_Preg_Return_Offset;
                  }
                  else {
                    info.count     = 2;
                    info.mtype [0] = Mtype_complex_to_real(MTYPE_C10);
                    info.mtype [1] = Mtype_complex_to_real(MTYPE_C10);
                    info.preg  [0] = First_X87_Preg_Return_Offset;
                    info.preg  [1] = Last_X87_Preg_Return_Offset;
                  }
              } 
              else {
                if (classes[0] == X86_64_SSE_CLASS) {
		  info.mtype[0] = SIM_INFO.dbl_type;
		  info.preg[0] = PR_first_reg(SIM_INFO.dbl_results);
		  next_float_return_num = PR_last_reg(SIM_INFO.dbl_results);
		  next_int_return_num = PR_first_reg(SIM_INFO.int_results);
	        }
	        else {
		  info.mtype[0] = SIM_INFO.int_type;
		  info.preg[0] = PR_first_reg(SIM_INFO.int_results);
		  next_float_return_num = PR_first_reg(SIM_INFO.dbl_results);
		  next_int_return_num = PR_last_reg(SIM_INFO.int_results);
	        }
  
	        if (n > 1) {
	          if (classes[1] == X86_64_SSE_CLASS) {
		    info.mtype[1] = SIM_INFO.dbl_type;
		    info.preg[1] = next_float_return_num;
		  }
		  else {
		    info.mtype[1] = SIM_INFO.int_type;
		    info.preg[1] = next_int_return_num;
		  }
	        }
	      }
           }
        }
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

static INT Current_Int_Param_Num = -1;		// count integer parameters only
static INT Current_Float_Param_Num = -1;	// count float parameters only
static INT Current_MMX_Param_Num = -1;
static INT Register_Parms = 0;
static BOOL SSE_Register_Parms = FALSE;
static BOOL FASTCALL_Parms = FALSE;
const INT SSE_Register_Parms_Allowed = 3;
const INT MMX_Register_Parms_Allowed = 3;


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

    if (Is_Target_32bit()) {
      // -m32 is enforced in front-end, use it here also to be more specific.
      SSE_Register_Parms = TY_has_sseregister_parm (pu_type);
      Register_Parms = TY_register_parm (pu_type);

      FASTCALL_Parms = TY_has_fastcall (pu_type);
      if (FASTCALL_Parms) {
         Register_Parms = 2;  // use ECX and EDX for fast call
      }

    }

    Current_Param_Num = -1;		// count all parameters
    Current_Int_Param_Num = -1;		// count integer parameters only
    Current_Float_Param_Num = -1;	// count float parameters only
    Current_MMX_Param_Num = -1;   
    Last_Param_Offset = 0;
    return plocNULL;
} // Setup_Parameter_Locations


static PLOC
Get_Parameter_Location (TY_IDX ty, BOOL is_output)
{
    PLOC ploc;				// return location
    mDED_PREG_NUM regparm_regs_array[] = {RAX, RDX, RCX};
    mDED_PREG_NUM flt_regs_array[] = {XMM0, XMM1, XMM2};
    mDED_PREG_NUM mmx_regs_array[] = {MM0, MM1, MM2};
    mDED_PREG_NUM fastcall_regs_array[] = {RCX, RDX};
    mDED_PREG_NUM *int_regs_array = 
       FASTCALL_Parms ? fastcall_regs_array : regparm_regs_array;

    ploc.reg = 0;
    ploc.reg2 = 0;
    ploc.start_offset = Last_Param_Offset;
    ploc.size = 0;
    ploc.vararg_reg = 0;               // to silence purify
    if (TY_kind (ty) == KIND_VOID) {
	return ploc;
    }

    /* check for array case where fe doesn't fill in right btype */
    TYPE_ID pmtype = Fix_TY_mtype (ty);	/* Target type */
    ploc.size = MTYPE_RegisterSize(pmtype);

    if (Is_Target_32bit() && MTYPE_is_vector(pmtype) &&
        (Last_Param_Offset & 7) != 0) { // bug 12075
      Last_Param_Offset += 8 - Last_Param_Offset % 8;
      ploc.start_offset = Last_Param_Offset;
    }

    ++Current_Param_Num;

    INT rpad = 0;			/* padding to right of object */

    switch (pmtype) {
	
    case MTYPE_I1:
    case MTYPE_U1:
    case MTYPE_I2:
    case MTYPE_U2:
    case MTYPE_I4:
    case MTYPE_U4:
    case MTYPE_A4:
	/* Pad to doubleword */
	rpad = (MTYPE_RegisterSize(SIM_INFO.int_type) - ploc.size);

	/* fall thru */
    case MTYPE_I8:
    case MTYPE_U8:
    case MTYPE_A8:
        ++Current_Int_Param_Num;
	if (Is_Target_32bit() && Current_Int_Param_Num < Register_Parms) {
	  ploc.reg = int_regs_array[Current_Int_Param_Num];
          // handle 64 bit integer passed in register pair
          if (ploc.size == 8) {
            ++Current_Int_Param_Num;
            if (FASTCALL_Parms) {
              // fastcall doesn't pass 8byte value in register
              ploc.reg = 0;
            }
            else if (Current_Int_Param_Num < Register_Parms) {
              // check if there is another register left for the lower 32 bit
	      ploc.reg2 = int_regs_array[Current_Int_Param_Num];
            }
            else {
              // no enough register to pass 64bit parameter, remove the register
              ploc.reg = 0;
            }
          }
	}
	else {
	  ploc.reg = PR_first_reg(SIM_INFO.int_args) + Current_Int_Param_Num;
	  if (ploc.reg > PR_last_reg(SIM_INFO.int_args)) 
	    ploc.reg = 0;
          // keep the int param number in sync for 32bit if the type is 64bit
          if (Is_Target_32bit() && ploc.size == 8) {
            ++Current_Int_Param_Num;
          }
          // for IA-32, stack needs to be aligned. So, if the last formal argument  
          // needs pading, the total stack size computed from (just) function decl
          // is different from the stack size computed from actual callsite    
          if (Is_Target_32bit() && PLOC_on_stack(ploc)) {
              ploc.size += rpad;
              rpad = 0;
          }
	}
	break;
	
    case MTYPE_V8I1:
    case MTYPE_V8I2:
    case MTYPE_V8I4:
    case MTYPE_V8I8:
    case MTYPE_V8F4:
    case MTYPE_M8I1:
    case MTYPE_M8I2:
    case MTYPE_M8I4:
    case MTYPE_M8F4:
      if (Is_Target_32bit()) {
	//8 bytes vector in 32bit system, if enable mmx
	//use mm0, mm1, mm2 for parameter passing
	++Current_MMX_Param_Num;
	if (Current_MMX_Param_Num < MMX_Register_Parms_Allowed && Target_MMX) {
	  ploc.reg = mmx_regs_array[Current_MMX_Param_Num];
	}
	else {
	  ploc.reg = 0;  /* pass in memory */
	}
	break;
      }
    case MTYPE_F4:
    case MTYPE_F8:
      if (Is_Target_32bit()) {
	//float and double in 32bit system, if allow sse register parms
	//use xmm0, xmm1, xmm2 for parameter passing
	++Current_Float_Param_Num;
	if ( SSE_Register_Parms && Current_Float_Param_Num < SSE_Register_Parms_Allowed) {
	  ploc.reg = flt_regs_array[Current_Float_Param_Num];
	}
	else {
	  ploc.reg = 0;  /* pass in memory */
	}
	break;
      }
    case MTYPE_V16I1:
    case MTYPE_V16I2:
    case MTYPE_V16I4:
    case MTYPE_V16I8:
    case MTYPE_V16F4:
    case MTYPE_V16F8:
    case MTYPE_V32I1:
    case MTYPE_V32I2:
    case MTYPE_V32I4:
    case MTYPE_V32I8:
    case MTYPE_V32F4:
    case MTYPE_V32F8:
    case MTYPE_V32C4:
    case MTYPE_V32C8:
      ++Current_Float_Param_Num;
      if (Is_Target_32bit()) {
	//16 bytes vector in 32bit system, if enable sse
	//use xmm0, xmm1, xmm2 for parameter passing
	if (Current_Float_Param_Num < SSE_Register_Parms_Allowed && Target_SSE) {
	  ploc.reg = flt_regs_array[Current_Float_Param_Num];
	}
	else {
	  ploc.reg = 0;  /* pass in memory */
	}
      }
      else {
	//in 64bit system all vectors and float double use xmm0-xmm7 
	//for parameter passing
	ploc.reg = PR_first_reg(SIM_INFO.flt_args) + Current_Float_Param_Num;
	if (ploc.reg > PR_last_reg(SIM_INFO.flt_args)) {
	  ploc.reg = 0;
	  rpad = MTYPE_RegisterSize(SIM_INFO.flt_type) - ploc.size;
	}
      }
      break;

    case MTYPE_C10:
    case MTYPE_F10:
    case MTYPE_C16:
    case MTYPE_F16:
      ploc.reg = 0;  /* pass in memory */
      break;

    case MTYPE_C4:
        ++Current_Float_Param_Num;
	ploc.reg = PR_first_reg(SIM_INFO.flt_args) + Current_Float_Param_Num;
	if (ploc.reg > PR_last_reg(SIM_INFO.flt_args)) 
	  ploc.reg = 0;
	break;
	
    case MTYPE_C8:
        ++Current_Float_Param_Num;
	ploc.reg = PR_first_reg(SIM_INFO.flt_args) + Current_Float_Param_Num;
	if (ploc.reg > PR_last_reg(SIM_INFO.flt_args)) {
          --Current_Float_Param_Num;
	  ploc.reg = 0;	/* pass in memory */
	}
	else {
          ++Current_Float_Param_Num;
	  ploc.reg2 = PR_first_reg(SIM_INFO.flt_args) + Current_Float_Param_Num;
	  if (ploc.reg2 > PR_last_reg(SIM_INFO.flt_args)) {
            Current_Float_Param_Num -= 2;
	    ploc.reg = 0;	/* whole thing pass in memory */
	  }
	}
	break;

    case MTYPE_M:
        {
	  enum X86_64_PARM_CLASS classes[MAX_CLASSES];
	  INT Save_Current_Int_Param_Num = Current_Int_Param_Num;
	  INT Save_Current_Float_Param_Num = Current_Float_Param_Num;
          ploc.size = TY_size (ty);
	  INT n = Classify_Aggregate(ty, classes);
          // handle X87 X87UP and COMPLEX_X87 cases
          if (n != 0 && (classes[0] == X86_64_X87_CLASS ||
                         classes[0] == X86_64_X87UP_CLASS ||
                         classes[0] == X86_64_COMPLEX_X87_CLASS)) {
             // x87, x87up and complex_x87 are passed in memory
             n = 0;
          }
	  if (n > 0) { // passed in registers
	    if (classes[0] == X86_64_SSE_CLASS) {
	      ++Current_Float_Param_Num;
	      ploc.reg = PR_first_reg(SIM_INFO.flt_args) + 
		  	 Current_Float_Param_Num;
	      if (ploc.reg > PR_last_reg(SIM_INFO.flt_args)) {
	        Current_Float_Param_Num = Save_Current_Float_Param_Num;
	        ploc.reg = 0;	/* pass in memory */
		n = 0;
	      }
	    }
	    else {
	      ++Current_Int_Param_Num;
	      ploc.reg = PR_first_reg(SIM_INFO.int_args) + 
		  	 Current_Int_Param_Num;
	      if (ploc.reg > PR_last_reg(SIM_INFO.int_args)) {
	        Current_Int_Param_Num = Save_Current_Int_Param_Num;
	        ploc.reg = 0;	/* pass in memory */
		n = 0;
	      }
	    }
	    if (n > 1) {
	      if (classes[1] == X86_64_SSE_CLASS) {
	        ++Current_Float_Param_Num;
	        ploc.reg2 = PR_first_reg(SIM_INFO.flt_args) + 
		  	    Current_Float_Param_Num;
	        if (ploc.reg2 > PR_last_reg(SIM_INFO.flt_args)) {
	          Current_Float_Param_Num = Save_Current_Float_Param_Num;
	          Current_Int_Param_Num = Save_Current_Int_Param_Num;
	          ploc.reg = 0;	/* pass in memory */
		  n = 0;
	        }
	      }
	      else if (classes[1] == X86_64_INTEGER_CLASS) {
	        ++Current_Int_Param_Num;
	        ploc.reg2 = PR_first_reg(SIM_INFO.int_args) + 
		  	    Current_Int_Param_Num;
	        if (ploc.reg2 > PR_last_reg(SIM_INFO.int_args)) {
	          Current_Float_Param_Num = Save_Current_Float_Param_Num;
	          Current_Int_Param_Num = Save_Current_Int_Param_Num;
	          ploc.reg = 0;	/* pass in memory */
		  n = 0;
	        }
	      }
	    }
	  }
	  // bug 3926: for -m32, we pass in memory even if n > 0, so check
	  // for reg.
	  if (n == 0 || ploc.reg == 0) { // passed in memory
	    INT psize = TY_size (ty) / MTYPE_RegisterSize(SIM_INFO.int_type);
	    /* round up */
	    if ((TY_size (ty) % MTYPE_RegisterSize(SIM_INFO.int_type)) != 0)
	      psize++;
	    /* structures are left-justified, so may be padding at end */
	    rpad = (psize * MTYPE_RegisterSize(SIM_INFO.int_type)) - ploc.size;

	    /* Although the size of a class/struct param is 0, we should at
	       least adjust the position for the next param.
	       This extra padding is not required for C.
	     */
	    if( TY_size(ty) == 0  &&
		PU_cxx_lang(Get_Current_PU()) ){
	      if (Is_Target_32bit())
	        rpad = 4; // bug 1740
	      else
	        rpad = 8; // bug 12830
	    }
	  }
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
  INT64	size;
};

struct PSTRUCT pstruct;

static void
Setup_Struct_Parameter_Locations (TY_IDX struct_ty)
{
  pstruct.first_call = TRUE;
  pstruct.size = TY_size(struct_ty);
}

static PLOC 
Get_Struct_Parameter_Location (PLOC ploc)
{
  if (pstruct.first_call) {
    pstruct.first_call = FALSE;
    if (ploc.reg == 0)
      return ploc; // pass in memory
    ploc.size = MIN(pstruct.size, 8);
    return ploc;
  }
  if (ploc.reg2 != 0) { // second call
    ploc.reg = ploc.reg2;
    ploc.reg2 = 0;
    ploc.start_offset += 8;
    ploc.size = pstruct.size - 8;
    return ploc;
  }
  ploc.reg = 0;
  ploc.size = 0; // cause empty condition 
  return ploc;
} // Get_Struct_Parameter_Location


/* Iterate over vararg non-fixed parameters */
static PLOC
Get_Vararg_Parameter_Location (PLOC prev)
{
  PLOC next;
  if (Current_Int_Param_Num + 1 < MAX_NUMBER_OF_INT_REGISTER_PARAMETERS) {
    Current_Param_Num++;
    Current_Int_Param_Num++;
    next.reg = PR_first_reg(SIM_INFO.int_args) + Current_Int_Param_Num;
    next.size = MTYPE_RegisterSize(SIM_INFO.int_type);
    return next;
  }
  if (Current_Float_Param_Num + 1 < MAX_NUMBER_OF_FLOAT_REGISTER_PARAMETERS) {
    Current_Param_Num++;
    Current_Float_Param_Num++;
    next.reg = PR_first_reg(SIM_INFO.flt_args) + Current_Float_Param_Num;
    next.size = MTYPE_RegisterSize(SIM_INFO.flt_type) * 2;
    return next;
  }
  next.reg = 0;
  next.size = 0;
  return next;
}

BOOL Is_Caller_Save_GP;  /* whether GP is caller-save */

INT Formal_Save_Area_Size = 0;
INT Stack_Offset_Adjustment = 0;

extern void 
Init_Targ_Sim (void)
{
	Is_Caller_Save_GP = SIM_caller_save_gp;
}


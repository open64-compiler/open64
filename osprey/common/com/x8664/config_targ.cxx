/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007, 2008.  Pathscale, LLC. All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: config_targ.c
 * $Revision: 1.50 $
 * $Date: 05/12/01 11:41:37-08:00 $
 * $Author: tkong@hyalite.keyresearch $
 * $Source: common/com/x8664/SCCS/s.config_targ.cxx $
 *
 *
 * Description:
 *
 * Configuration specific to the target machine/system.
 *
 * NOTE:  There is an approximate distinction between -TARG option
 * group flags and their configuration (in config_targ_opt.c), and more
 * generic target configuration (in this file).  Note that the related
 * header file config_targ.h is included in config.h, and hence in most
 * source files, whereas config_targ_opt.h is only included directly, so
 * putting new -TARG option-related variables in config_targ_opt.c is to
 * be preferred to putting them here.
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "config.h"
#include "config_asm.h"
#include "config_debug.h"
#include "config_targ_opt.h"
#include "config_opt.h"
#include "erglob.h"
#include "tracing.h"
#include "mtypes.h"
#include "stab.h"
#include "targ_sim.h"
#ifdef BACK_END
#include "config_lno.h"
#endif

#if defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)
typedef unsigned char an_integer_kind;
#include "c_int_model.h"
#endif

extern char *Ofast;		/* defined in config_opt.cxx */


/* Architecture specific definitions */
BOOL ARCH_generate_nor = FALSE; // Generate the NOR opcode
BOOL ARCH_mask_shift_counts = FALSE; // shift counts are masked by the hardware (vs. truncated)

/* Target selection */
TARGET_ABI Target_ABI = ABI_UNDEF;
TARGET_PROCESSOR Target = TARGET_UNDEF;		/* -Tc */
TARGET_ISA  Target_ISA  = TARGET_ISA_UNDEF;	/* -Tc.Ic */

/* We need to know the machine type of the integer and floating point
 * registers for purposes of subprogram save/restore:
 */
CLASS_INDEX Spill_Int_Mtype = 0;
CLASS_INDEX Spill_Float_Mtype = 0;
CLASS_INDEX Spill_Int32_Mtype = 0;
CLASS_INDEX Spill_Float32_Mtype = 0;

/* The maximum integer machine type corresponding directly to the
 * machine registers, and the default integer machine type:
 */
CLASS_INDEX Max_Int_Mtype = 0;
CLASS_INDEX Max_Uint_Mtype = 0;
CLASS_INDEX Def_Int_Mtype = 0;
CLASS_INDEX Def_Uint_Mtype = 0;

/* On MIPS III targets, should we use 32-bit pointers? */
BOOL Use_32_Bit_Pointers = FALSE;

/* For various targets, what are the characteristics of pointers */
INT		Pointer_Size; 
CLASS_INDEX	Pointer_Mtype;
CLASS_INDEX	Pointer_Mtype2;

/* What are pointers and booleans aliased to in WHIRL */
TYPE_ID Pointer_type;
TYPE_ID Pointer_type2;
TYPE_ID Boolean_type;
TYPE_ID Boolean_type2;
TYPE_ID Integer_type;

/* For various targets, what is the comparison result type? */
INT		Comparison_Result_Size;		/* in bytes */
CLASS_INDEX	Comparison_Result_Mtype;

/* The assembler directive for emitting an address depends on the target
 * pointer size.  The following is declared in config_asm.h:
 */
const char *AS_ADDRESS;
const char *AS_ADDRESS_UNALIGNED;

/* Is the "char" type signed? */
BOOL Char_Type_Is_Signed = FALSE;

/* Various categories which are static, for now: */
static BOOL Target_int64;	/* 64-bit integer registers? */

/* Miscellaneous exception control */
#define FPX_DEF EXC_ALL	/* Default enables */
INT16 FP_Exception_Enable_Max = FPX_DEF;/* Max FP trap enables */
INT16 FP_Exception_Enable_Min = 0;	/* Min FP trap enables */

INT32 Align_Instructions = 0;	/* 0 means hasn't been set */
BOOL Avoid_TFP_blikely_bug = FALSE;
BOOL Avoid_TFP_blikely_bug_overridden = FALSE;

/***** IEEE 754 options *****/
BOOL Force_IEEE_Comparisons = TRUE;    /* IEEE NaN comparisons? */

/***** INTERNAL group options *****/

BOOL WHIRL_Return_Val_On  = TRUE;
BOOL WHIRL_Mldid_Mstid_On = TRUE;
BOOL WHIRL_Return_Info_On = TRUE;


/* ====================================================================
 *
 * Target debugging options
 *
 * ====================================================================
 */

/* Symbolic Debug mode as specified on command line.  (The mode can
 * change from PU to PU because, for example, we encounter a call to
 * the routine 'alloca()' -- we must restore the mode to the value as
 * it was specified on the command line, so we remember it.)
 */
INT16 Symbolic_Debug_Mode;
INT16 Max_Symbolic_Debug_Mode;	/* Maximum for any PU */


/* ====================================================================
 *
 * Miscellaneous options
 *
 * ====================================================================
 */

/* Can 64-bit values be 4-byte aligned in memory? */
BOOL Allow_Word_Aligned_Doubles = FALSE;

/* Should we generate position-independent code by default? */
BOOL Generate_Position_Independent_Code = FALSE;

/* Split 64-bit integer ops into 32-bit ops, and simulate them? */
BOOL Split_64_Bit_Int_Ops = FALSE;

/* Split quad-precision ops into double-precision, and simulate them? */
BOOL Split_Quad_Ops = TRUE;

/* Should we simulate 32-bit subprogram interface by default? */
BOOL Simulate_32_Bit_Interface = FALSE;

/* Work around a TFP branch cache problem. */
BOOL No_Quad_Aligned_Branch = FALSE;

/* Does target provides only unsigned 64-bit instructions? */
BOOL Only_Unsigned_64_Bit_Ops = FALSE;

BOOL Has_GP_Groups = FALSE;

/* Does target have offsets in load and store instructions?
 * Note: CG should instead test:
 * ( TOP_Find_Operand_Use( OP_code(op), OU_offset ) >= 0 ) */
BOOL Use_Load_Store_Offset = TRUE;

#if defined (FRONT_END_C) || defined (FRONT_END_CPLUSPLUS)

// This should be in synch with (order of registers in) reg_names in the 
// Gnu front-end and definition of preg order in common/com/arch/targ_sim.h
PREG_NUM Map_Reg_To_Preg [] = {
  0x001 /* ax       */,
  0x007 /* dx       */,
  0x008 /* cx       */,
  0x002 /* bx       */,
  0x006 /* si       */,
  0x005 /* di       */,
  0x003 /* bp       */,
  0x004 /* sp       */,
  0x021 /* st       */,
  0x022 /* st(1)    */,
  0x023 /* st(2)    */,
  0x024 /* st(3)    */,
  0x025 /* st(4)    */,
  0x026 /* st(5)    */,
  0x027 /* st(6)    */,
  0x028 /* st(7)    */,
  -1    /*          */,
  -1    /* flags    */,
  -1    /* fpsr     */,
  -1    /* dirflags */,
  -1    /* frame    */,
  0x011 /* xmm0     */,
  0x012 /* xmm1     */,
  0x013 /* xmm2     */,
  0x014 /* xmm3     */,
  0x015 /* xmm4     */,
  0x016 /* xmm5     */,
  0x017 /* xmm6     */,
  0x018 /* xmm7     */,
  -1    /* mm0      */,
  -1    /* mm1      */,
  -1    /* mm2      */,
  -1    /* mm3      */,
  -1    /* mm4      */,
  -1    /* mm5      */,
  -1    /* mm6      */,
  -1    /* mm7      */,
  0x009 /* r8       */,
  0x00a /* r9       */,
  0x00b /* r10      */,
  0x00c /* r11      */,
  0x00d /* r12      */,
  0x00e /* r13      */,
  0x00f /* r14      */,
  0x010 /* r15      */,
  0x019 /* xmm8     */,
  0x01a /* xmm9     */,
  0x01b /* xmm10    */,
  0x01c /* xmm11    */,
  0x01d /* xmm12    */,
  0x01e /* xmm13    */,
  0x01f /* xmm14    */,
  0x020 /* xmm15    */,
  -1
};

#endif /* defined (FRONT_END_C) || defined (FRONT_END_CPLUSPLUS) */


/* ====================================================================
 *
 * Abi_Name / Isa_Name / Targ_Name
 *
 * Produce printable names for the target choices.
 *
 * ====================================================================
 */

static struct bnm {
  char name[16];
} bnb[4];
static INT16 bnb_used = 0;

#ifndef MONGOOSE_BE
const char *
Abi_Name ( TARGET_ABI b)
{
  char *r;

  switch ( b ) {
    case ABI_n32:	return "n32";
    case ABI_n64:	return "n64";
    default:
      r = bnb[bnb_used].name;
      bnb_used = (bnb_used + 1) % 4;
      sprintf (r, "ABI_%d", b);
      return r;
  }
}
#endif /* MONGOOSE_BE */

char *
Isa_Name ( TARGET_ISA b)
{
  char *r;

  switch ( b ) {
    default:
      r = bnb[bnb_used].name;
      bnb_used = (bnb_used + 1) % 4;
      sprintf (r, "ISA_%d", b);
      return r;
  }
}

const char *
Targ_Name ( TARGET_PROCESSOR b)
{
  char *r;

  switch ( b ) {
    case TARGET_opteron: return "Opteron";
    case TARGET_athlon64: return "Athlon64";
    case TARGET_athlon: return "Athlon";
    case TARGET_em64t: return "EM64T"; 
    case TARGET_core: return "Core"; 
    case TARGET_wolfdale: return "Wolfdale"; 
    case TARGET_pentium4: return "Pentium4";
    case TARGET_xeon: return "Xeon";
    case TARGET_anyx86: return "Anyx86";
    case TARGET_barcelona: return "Barcelona";
    case TARGET_orochi: return "Bdver1";
    default:
      r = bnb[bnb_used].name;
      bnb_used = (bnb_used + 1) % 4;
      sprintf (r, "PROCESSOR_%d", b);
      return r;
  }
}

/* ====================================================================
 *
 * Preconfigure_Target
 *
 * Configuration of target-specific parameters, before flag processing.
 *
 * ====================================================================
 */

void
Preconfigure_Target ( void )
{
  return;
}


/* Adjust the size and alignment for some data types under -m32.
 */
static void Adjust_m32_MTYPE_Info()
{
  MTYPE_alignment( MTYPE_I8 ) = 4;
  MTYPE_alignment( MTYPE_U8 ) = 4;

  MTYPE_alignment(MTYPE_F8) = 4;
  MTYPE_alignment(MTYPE_C8) = 4;

  MTYPE_bit_size(MTYPE_F10)  = 96;
  MTYPE_alignment(MTYPE_F10) = 4;

  MTYPE_bit_size(MTYPE_C10)  = 192;
  MTYPE_alignment(MTYPE_C10) = 4;
}


/* ====================================================================
 *
 * Prepare_Target
 *
 * Given target specification choices, fill in defaulted pieces and
 * check for conflicting specifications.  When this routine is done,
 * ABI, Target_ISA, Target, and Target_FPRs are all valid.  We also
 * use the target information to set the descriptive variables
 * Target_int64, Target_Int_Model, and Use_32_Bit_Pointers.
 *
 * TODO:  Pending final conversion of the driver, we may have incoming
 * information from either or both of -T... and -TARG:... options.  We
 * effectively give precedence to the -TARG: specifications, and after
 * final conversion should remove the -T support.  Note that we ignore
 * the pointer size and integer model specifications from -Tx,Pnn,Mm.
 *
 * ====================================================================
 */

static void
Prepare_Target ( void )
{
  TARGET_ISA isa_default = TARGET_ISA_UNDEF;
  TARGET_PROCESSOR targ_default = TARGET_UNDEF;

  /* First check the ABI: */
  if ( ABI_Name != NULL ) {
    if ( strcmp ( ABI_Name, "n64" ) == 0 ) {
      Target_ABI = ABI_n64;
      isa_default = TARGET_ISA_x86_64;
      targ_default = TARGET_opteron;
      if( !Target_SSE2_Set)
	Target_SSE2 = TRUE;

      FmtAssert( Target_SSE2,
		 ("Option -mno_sse2 is not yet implemented for x86-64.") );

    } else if( strcmp( ABI_Name, "n32" ) == 0 ){
      Target_ABI = ABI_n32;
      isa_default = TARGET_ISA_x86_64;
      targ_default = TARGET_opteron;
      if( !Target_SSE2_Set)
	Target_SSE2 = TRUE;

    } else {
      ErrMsg ( EC_Inv_TARG, "abi", ABI_Name );
    }
  }

  /* Next check the ISA from -TARG:isa=xxx: */
  if ( ISA_Name != NULL ) {
    TARGET_ISA isa;

    if ( strcasecmp ( ISA_Name, "i386" ) == 0 || strcasecmp ( ISA_Name, "ia32" ) == 0 ) {
      Target_ABI = ABI_n32;
      isa = TARGET_ISA_x86_64;
      targ_default = TARGET_opteron;
    }
    else if ( strcasecmp ( ISA_Name, "x86_64" ) == 0 ) {
      isa = TARGET_ISA_x86_64;
      targ_default = TARGET_opteron;
    } else
    {
      ErrMsg ( EC_Inv_TARG, "isa", ISA_Name );
    }

    /* If there's an ISA from -Tn,Ix it must match: */
    if ( Target_ISA != TARGET_ISA_UNDEF && Target_ISA != isa ) {
      ErrMsg ( EC_Incons_TARG, "isa", ISA_Name,
	       "isa", Isa_Name(Target_ISA) );
    }
    Target_ISA = isa;
  }

  /* Now make sure ISA and ABI are consistent if both specified,
   * and if one is default the other:
   */
  switch ( Target_ISA ) {
    case TARGET_ISA_UNDEF:
      Target_ISA = isa_default;
      break;
  }

  /* Now check the target processor from -TARG:processor=xxx: */
  if ( Processor_Name != NULL ) {
    TARGET_PROCESSOR targ;

    if ( strcasecmp ( Processor_Name, "opteron" ) == 0 ) {
      targ = TARGET_opteron;
    }
    else if ( strcasecmp ( Processor_Name, "barcelona" ) == 0 ) {
      if (!Target_SSE2_Set && !Target_SSE3_Set)
        Target_SSE3 = TRUE;
      targ = TARGET_barcelona;
    }
    else if ( strcasecmp ( Processor_Name, "bdver1" ) == 0 ) {
      if (!Target_SSE2_Set && !Target_SSE3_Set)
        Target_SSE3 = TRUE;
      if (!Target_SSE2_Set && !Target_SSSE3_Set)
        Target_SSSE3 = TRUE;
      if (!Target_SSE2_Set && !Target_SSE41_Set)
        Target_SSE41 = TRUE;
      if (!Target_SSE2_Set && !Target_SSE42_Set)
        Target_SSE42 = TRUE;
      if (!Target_SSE2_Set && !Target_AES_Set)
        Target_AES = TRUE;
      if (!Target_SSE2_Set && !Target_PCLMUL_Set)
        Target_PCLMUL = TRUE;
      if (!Target_SSE2_Set && !Target_AVX_Set)
        Target_AVX = TRUE;
      if (!Target_SSE2_Set && !Target_XOP_Set)
        Target_XOP = TRUE;
      if (!Target_SSE2_Set && !Target_FMA4_Set)
        Target_FMA4 = TRUE;
      targ = TARGET_orochi;
    }
    else if ( strcasecmp ( Processor_Name, "bdver2" ) == 0 ) {
      if (!Target_SSE2_Set && !Target_SSE3_Set)
        Target_SSE3 = TRUE;
      if (!Target_SSE2_Set && !Target_SSSE3_Set)
        Target_SSSE3 = TRUE;
      if (!Target_SSE2_Set && !Target_SSE41_Set)
        Target_SSE41 = TRUE;
      if (!Target_SSE2_Set && !Target_SSE42_Set)
        Target_SSE42 = TRUE;
      if (!Target_SSE2_Set && !Target_AES_Set)
        Target_AES = TRUE;
      if (!Target_SSE2_Set && !Target_PCLMUL_Set)
        Target_PCLMUL = TRUE;
      if (!Target_SSE2_Set && !Target_AVX_Set)
        Target_AVX = TRUE;
      if (!Target_SSE2_Set && !Target_XOP_Set)
        Target_XOP = TRUE;
      if (!Target_SSE2_Set && !Target_FMA_Set)
        Target_FMA = TRUE;
      if (!Target_SSE2_Set && !Target_FMA4_Set)
        Target_FMA4 = TRUE;
      targ = TARGET_orochi;
    }
    else if ( strcasecmp ( Processor_Name, "athlon64fx" ) == 0 ) {
      targ = TARGET_opteron;
    }
    else if ( strcasecmp ( Processor_Name, "athlon64" ) == 0 ) {
      targ = TARGET_athlon64;
    }
    else if ( strcasecmp ( Processor_Name, "athlon" ) == 0 ) {
      targ = TARGET_athlon;
    }
    else if ( strcasecmp ( Processor_Name, "pentium4" ) == 0 ) {
      targ = TARGET_pentium4;
    }
    else if ( strcasecmp ( Processor_Name, "xeon" ) == 0 ) {
      targ = TARGET_xeon;
    }
    else if ( strcasecmp ( Processor_Name, "em64t" ) == 0 ) {
      targ = TARGET_em64t;
      if (!Target_SSE2_Set && !Target_SSE3_Set)
        Target_SSE3 = TRUE;
    }
    else if ( strcasecmp ( Processor_Name, "core" ) == 0 ) {
      targ = TARGET_core;
      if (!Target_SSE2_Set && !Target_SSE3_Set)
        Target_SSE3 = TRUE;
    }
    else if ( strcasecmp ( Processor_Name, "wolfdale" ) == 0 ) {
      targ = TARGET_wolfdale;
      if (!Target_SSE2_Set && !Target_SSE3_Set)
        Target_SSE3 = TRUE;
      if (!Target_SSE2_Set && !Target_SSSE3_Set)
        Target_SSSE3 = TRUE;
    }
    else if ( strcasecmp ( Processor_Name, "anyx86" ) == 0 ) {
      targ = TARGET_anyx86;
    }
    else {
      ErrMsg ( EC_Inv_TARG, "processor", Processor_Name );
      targ = TARGET_UNDEF;
    }
    
    /* If there's a processor spec from -Tn it must match: */
    if ( Target != TARGET_UNDEF && Target != targ ) {
      ErrMsg ( EC_Incons_TARG, "processor", Processor_Name,
	       "processor", Targ_Name(Target) );
    }
    Target = targ;
  }

  /* Now make sure ABI/ISA and Target are consistent if both specified,
   * and if one is default the other:
   */
  if ( Is_Target_x86_64() ) {
    if( Target_ABI == ABI_UNDEF ){
      Target_ABI = ABI_n64;
    }
    Target_ISA = TARGET_ISA_x86_64;
  } else {
    Target = targ_default;
    if ( Target == TARGET_UNDEF ) {
      /* Default everything: */
      Target_ABI = ABI_n64;
      Target_ISA = TARGET_ISA_x86_64;
      Target = TARGET_opteron;
    }
  }

  /* Now deal with FP register count: */
  switch ( Target_FPRs ) {
    default:	/* Invalid value */
      ErrMsg ( EC_Inv_FPRs, Target_FPRs );
      /* fall through */
    case 0:	/* Unspecified */
      Target_FPRs = 128;
      break;
    case 16:	/* Always OK, warning possible */
      ErrMsg ( EC_FPR_16 );
      break;
    case 32:
      ErrMsg ( EC_FPR_32 );
      break;
  }
  
  /* Check x87 precision. */
  if (Target_x87_Precision != 32 &&
      Target_x87_Precision != 64 &&
      Target_x87_Precision != 80) {
    ErrMsg (EC_Inv_x87_Prec, Target_x87_Precision);
  }

  /* Set descriptive variables: */
  Target_int64 = TRUE;
  Use_32_Bit_Pointers = (Target_ABI != ABI_n64);

  if( Target_ABI == ABI_n32 ){
    Adjust_m32_MTYPE_Info();
  }

#if defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)
  Target_Int_Model = ( Target_ABI == ABI_n64 ) ? TARGET_INT_LP64
				       : TARGET_INT_ILP32;
  Make_Int_Model_Consistent ();
#endif
}

/* ====================================================================
 *
 * Configure_Target
 *
 * Configuration of target-specific parameters, after flag processing.
 *
 * ====================================================================
 */

void
Configure_Target ( void )
{

#if defined(linux) || defined(BUILD_OS_DARWIN)
  Target_Byte_Sex = LITTLE_ENDIAN;
#else  
  Target_Byte_Sex = BIG_ENDIAN;
#endif
  Same_Byte_Sex = ( Target_Byte_Sex == Host_Byte_Sex );

  Gen_PIC_Calls = FALSE;
  GP_Is_Preserved = FALSE;

  /* Set up the target processor and ISA: */
  Prepare_Target ();

  /* Unrolling defaults */
  if (OPT_unroll_times > 0 && !OPT_unroll_times_overridden)
    OPT_unroll_times = 4;

  /* Set up the target register set: */
  switch ( Target_ISA ) {
    case TARGET_ISA_x86_64:
      Spill_Int_Mtype = Is_Target_64bit() ? MTYPE_I8 : MTYPE_I4;
      Spill_Float_Mtype = MTYPE_F8;
      Spill_Int32_Mtype = MTYPE_I4;
      Spill_Float32_Mtype = MTYPE_F4;
      Max_Int_Mtype  = Def_Int_Mtype  = Is_Target_64bit() ? MTYPE_I8 : MTYPE_I4;
      Max_Uint_Mtype = Def_Uint_Mtype = Is_Target_64bit() ? MTYPE_U8 : MTYPE_U4;
      Boolean_type  = MTYPE_I4;
      Boolean_type2 = MTYPE_I4;
      Integer_type = MTYPE_I4;

      Split_Quad_Ops = TRUE;
      Split_64_Bit_Int_Ops = ! Is_Target_64bit();
      break;
  }

#if defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)
#ifndef EDG_FORTRAN
  Make_Int_Model_Consistent();
#endif
#endif


  /* Initialize pointer information */
  if ( Use_32_Bit_Pointers ) {
    Pointer_Size = 4;
    Pointer_Mtype  = WHIRL_Mtype_A_On ? MTYPE_A4 : MTYPE_U4;
    Pointer_type   = Pointer_Mtype;
    Pointer_Mtype2 = MTYPE_U4;
    Pointer_type2  = MTYPE_U4;
  } else {
    Pointer_Size = 8;
    Pointer_Mtype  = WHIRL_Mtype_A_On ? MTYPE_A8: MTYPE_U8;
    Pointer_type   = Pointer_Mtype;
    Pointer_Mtype2 = MTYPE_U8;
    Pointer_type2  = MTYPE_U8;
  }

  if ( Use_32_Bit_Pointers ) {
    AS_ADDRESS = AS_WORD;
    AS_ADDRESS_UNALIGNED = AS_WORD;
  } else {
    AS_ADDRESS = AS_DWORD;
    AS_ADDRESS_UNALIGNED = AS_DWORD_UNALIGNED;
  }

  /* If the user has requested aggregate alignment without specifying
   * a threshhold, set it to the register size.  Otherwise, make sure
   * it's a power of two.  WARNING:  The option group processing sets
   * it to -1 if no threshhold is given, and otherwise restricts it to
   * a reasonable range, so we don't worry about overflow or bad values.
   * Also, if the user has "turned down" alignment, don't try to
   * realign objects (pv 525474)
   */
  if ( Aggregate_Alignment > 0 ) {
    INT32 i = 1;
    while ( i < Aggregate_Alignment ) i <<= 1;
    Aggregate_Alignment = i;

    if (Aggregate_Alignment < (Target_int64 ? 8 : 4))
    {
      Align_Object = FALSE;
    }
  }

#if defined(BACK_END)
  Init_Targ_Sim();	/* must be done before initialize_stack_frame */
#endif

#define IS_POW2(n)              (((n) & ((n)-1))==0)
  FmtAssert (IS_POW2(Align_Instructions), 
	("-OPT:align_instructions=<n> must equal power of two"));

#ifdef BACK_END
  /* Value of LNO_Iter_threshold is interpreted as default in which case 
     the flag is set based on target. Otherwise use user-specified value.
   */
  LNO_Iter_threshold = (Is_Target_SSE41())? 8 : 0;
#endif

  return;
}

/* ====================================================================
 *
 * IPA_Configure_Target
 *
 * IPA-specific configuration.  Similar to Configure_Target but only set up
 * those variables that IPA cares.
 *
 * ====================================================================
 */
void
IPA_Configure_Target (void)
{
    if (Target_ABI == ABI_n64) {
	Pointer_Size = 8;
	Pointer_Mtype  = WHIRL_Mtype_A_On ? MTYPE_A8 : MTYPE_U8;
	Pointer_type   = Pointer_Mtype;
	Pointer_Mtype2 = MTYPE_U8;
	Pointer_type2  = MTYPE_U8;
        Split_64_Bit_Int_Ops = FALSE;
    } else {
	Pointer_Size = 4;
	Pointer_Mtype  = WHIRL_Mtype_A_On ? MTYPE_A4 : MTYPE_U4;
	Pointer_type   = Pointer_Mtype;
	Pointer_Mtype2 = MTYPE_U4;
	Pointer_type2  = MTYPE_U4;
        Split_64_Bit_Int_Ops = TRUE;

	Adjust_m32_MTYPE_Info();
    }

    Integer_type = MTYPE_I4;
    Boolean_type  = MTYPE_I4;
    Boolean_type2 = MTYPE_I4;

#ifdef KEY // Tell IPA the target byte-order
#if defined(linux) || defined(BUILD_OS_DARWIN)
  Target_Byte_Sex = LITTLE_ENDIAN;
#else  
  Target_Byte_Sex = BIG_ENDIAN;
#endif
#endif

} /* IPA_Configure_Target */

/* ====================================================================
 *
 * Configure_Source_Target
 *
 * Reconfiguration of target-specific parameters for each source file.
 *
 * ====================================================================
 */

void
Configure_Source_Target ( char * /* filename */ )
{
  char *option;

  /* IA-64 doesn't have index loads.
   */
  Indexed_Loads_Allowed = FALSE;

  /* pv #297274 describes why we cannot put initialized data in .bss */
  if ( Kernel_Code ) {
    Zeroinit_in_bss = FALSE;
  }

  /* Miscellaneous exception control */
  if ( FP_Excp_Max != NULL ) {
    FP_Exception_Enable_Max = 0;
    option = FP_Excp_Max;
    while ( *option ) {
      switch ( *option ) {
	case 'I':	FP_Exception_Enable_Max |= FPX_I; break;
	case 'U':	FP_Exception_Enable_Max |= FPX_U; break;
	case 'O':	FP_Exception_Enable_Max |= FPX_O; break;
	case 'Z':	FP_Exception_Enable_Max |= FPX_Z; break;
	case 'V':	FP_Exception_Enable_Max |= FPX_V; break;
      }
      option++;
    }
  }
  if ( FP_Excp_Min != NULL ) {
    FP_Exception_Enable_Min = 0;
    option = FP_Excp_Min;
    while ( *option ) {
      switch ( *option ) {
	case 'I':	FP_Exception_Enable_Min |= FPX_I; break;
	case 'U':	FP_Exception_Enable_Min |= FPX_U; break;
	case 'O':	FP_Exception_Enable_Min |= FPX_O; break;
	case 'Z':	FP_Exception_Enable_Min |= FPX_Z; break;
	case 'V':	FP_Exception_Enable_Min |= FPX_V; break;
      }
      option++;
    }
  }

  if ( DEBUG_Trap_Uv )
    FP_Exception_Enable_Min |= FPX_V;

  // TMP: ignore cpic until we figure out what to do with it
  if (Gen_PIC_Call_Shared)
    Gen_PIC_Call_Shared = FALSE;

  return;
}

/* return FALSE if abi mismatch */
extern BOOL 
Set_Target_ABI (BOOL is_64bit, INT isa)
{
  if (is_64bit) {
    switch (Target_ABI) {
    case ABI_UNDEF:
      Target_ABI = ABI_n64;
      break;
    case ABI_n64:
      break;
    default:
      return FALSE;
    }
  } else {	/* 32 */
    switch (Target_ABI) {
    case ABI_UNDEF:
      Target_ABI = ABI_n32;
      break;
    case ABI_n32:
      break;
    default:
      return FALSE;
    }
  }

  if (Target_ISA == TARGET_ISA_UNDEF) {
      Target_ISA = TARGET_ISA_x86_64;
  }

  return TRUE;
}

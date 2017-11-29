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
 * $Revision: 1.4 $
 * $Date: 2006/07/25 07:59:10 $
 * $Author: weitang $
 * $Source: 
 *
 * Description:
 *
 * Configuration specific to the target machine/system.
 *
 * NOTE:  There is an approximate distinction between -TARG option
 * group flags and their configuration (in config_TARG.c), and more
 * generic target configuration (in this file).  Note that the related
 * header file config_targ.h is included in config.h, and hence in most
 * source files, whereas config_targ_opt.h is only included directly, so
 * putting new -TARG option-related variables in config_TARG.c is to
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
char *AS_ADDRESS;
char *AS_ADDRESS_UNALIGNED;

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
BOOL Split_64_Bit_Int_Ops = TRUE;

/* Split quad-precision ops into double-precision, and simulate them? */
BOOL Split_Quad_Ops = TRUE;

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

PREG_NUM Map_Reg_To_Preg [] = {
  /* General registers. */
  0x000, 0x001, 0x002, 0x003, 0x004, 0x005, 0x006, 0x007,
  0x008, 0x009, 0x00a, 0x00b, 0x00c, 0x00d, 0x00e, 0x00f,
  0x010, 0x011, 0x012, 0x013, 0x014, 0x015, 0x016, 0x017,
  0x018, 0x019, 0x01a, 0x01b, 0x01c, 0x01d, 0x01e, 0x01f,
  /* Floating-point registers. */
  0x020, 0x021, 0x022, 0x023, 0x024, 0x025, 0x026, 0x027,
  0x028, 0x029, 0x02a, 0x02b, 0x02c, 0x02d, 0x02e, 0x02f,
  0x030, 0x031, 0x032, 0x033, 0x034, 0x035, 0x036, 0x037,
  0x038, 0x039, 0x03a, 0x03b, 0x03c, 0x03d, 0x03e, 0x03f,
  0x040, 0x041, 0x042, 0x043, 0x044, 0x045, 0x046, 0x047,
  0x048, 0x049, 0x04a, 0x04b, 0x04c, 0x04d, 0x04e, 0x04f,
  0x050, 0x051, 0x052, 0x053, 0x054, 0x055, 0x056, 0x057,
  0x058, 0x059, 0x05a, 0x05b, 0x05c, 0x05d, 0x05e, 0x05f,
  0x060, 0x061, 0x062, 0x063, 0x064, 0x065, 0x066, 0x067,
  0x068, 0x069, 0x06a, 0x06b, 0x06c, 0x06d, 0x06e, 0x06f,
  0x070, 0x071, 0x072, 0x073, 0x074, 0x075, 0x076, 0x077,
  0x078, 0x079, 0x07a, 0x07b, 0x07c, 0x07d, 0x07e, 0x07f,
  0x080, 0x081, 0x082, 0x083, 0x084, 0x085, 0x086, 0x087,
  0x088, 0x089, 0x08a, 0x08b, 0x08c, 0x08d, 0x08e, 0x08f,
  0x090, 0x091, 0x092, 0x093, 0x094, 0x095, 0x096, 0x097,
  0x098, 0x099, 0x09a, 0x09b, 0x09c, 0x09d, 0x09e, 0x09f,
  0x0a0, 0x0a1, 0x0a2, 0x0a3, 0x0a4, 0x0a5, 0x0a6, 0x0a7,
  0x0a8, 0x0a9, 0x0aa, 0x0ab, 0x0ac, 0x0ad, 0x0ae, 0x0af,
  0x0b0, 0x0b1, 0x0b2, 0x0b3, 0x0b4, 0x0b5, 0x0b6, 0x0b7,
  0x0b8, 0x0b9, 0x0ba, 0x0bb, 0x0bc, 0x0bd, 0x0be, 0x0bf,
  0x0c0, 0x0c1, 0x0c2, 0x0c3, 0x0c4, 0x0c5, 0x0c6, 0x0c7,
  0x0c8, 0x0c9, 0x0ca, 0x0cb, 0x0cc, 0x0cd, 0x0ce, 0x0cf,
  0x0d0, 0x0d1, 0x0d2, 0x0d3, 0x0d4, 0x0d5, 0x0d6, 0x0d7,
  0x0d8, 0x0d9, 0x0da, 0x0db, 0x0dc, 0x0dd, 0x0de, 0x0df,
  0x0e0, 0x0e1, 0x0e2, 0x0e3, 0x0e4, 0x0e5, 0x0e6, 0x0e7,
  0x0e8, 0x0e9, 0x0ea, 0x0eb, 0x0ec, 0x0ed, 0x0ee, 0x0ef,
  0x0f0, 0x0f1, 0x0f2, 0x0f3, 0x0f4, 0x0f5, 0x0f6, 0x0f7,
  0x0f8, 0x0f9, 0x0fa, 0x0fb, 0x0fc, 0x0fd, 0x0fe, 0x0ff,
  /* Predicate registers. */
     -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
  /* Branch registers. */
  0x100, 0x101, 0x102, 0x103, 0x104, 0x105, 0x106, 0x107,
  /* Outgoing registers. */
  0x07f, 0x07e, 0x07d, 0x07c, 0x07b, 0x07a, 0x079, 0x078,
  /* FP */
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

char *
Abi_Name ( TARGET_ABI b)
{
  char *r;

  switch ( b ) {
    case ABI_N32:	return "n32";
    case ABI_N64:	return "n64";
    default:
      r = bnb[bnb_used].name;
      bnb_used = (bnb_used + 1) % 4;
      sprintf (r, "ABI_%d", b);
      return r;
  }
}

char *
Isa_Name ( TARGET_ISA b)
{
  char *r;

  switch ( b ) {
    case TARGET_ISA_Mips64:	return "mips64";
    default:
      r = bnb[bnb_used].name;
      bnb_used = (bnb_used + 1) % 4;
      sprintf (r, "ISA_%d", b);
      return r;
  }
}

char *
Targ_Name ( TARGET_PROCESSOR b)
{
  char *r;

  switch ( b ) {
    case TARGET_R10K: return "r10000";
    case TARGET_sb1: return "sb1";
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
    if ( strcmp ( ABI_Name, "n32" ) == 0 ) {
      Target_ABI = ABI_N32;
      isa_default = TARGET_ISA_M2;
#ifdef TARG_SL
      targ_default = TARGET_sl1_pcore;
#else
      targ_default = TARGET_R10K;
#endif
      Use_32_Bit_Pointers = TRUE;
    } else if ( strcmp ( ABI_Name, "n64" ) == 0 ) {
      Target_ABI = ABI_N64;
      isa_default = TARGET_ISA_Mips64;
      targ_default = TARGET_sb1;
      Use_32_Bit_Pointers = FALSE;
    } else {
      ErrMsg ( EC_Inv_TARG, "abi", ABI_Name );
    }
  }

  /* Next check the ISA from -TARG:isa=xxx: */
  if ( ISA_Name != NULL ) {
    TARGET_ISA isa;

    if ( strcasecmp ( ISA_Name, "mips64" ) == 0 ) {
      isa = TARGET_ISA_Mips64;
      targ_default = TARGET_sb1;
    } else
    isa = TARGET_ISA_M2;

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

    if ( strcasecmp ( Processor_Name, "sb1" ) == 0 ) {
      targ = TARGET_sb1;
    } else if ( strcasecmp ( Processor_Name, "r10000" ) == 0 ) {
      targ = TARGET_R10K;
    } 
#ifdef TARG_SL
    else if ( strcasecmp (Processor_Name, "sl1_pcore") == 0 ) {
      targ = TARGET_sl1_pcore;
    } else if (strcasecmp (Processor_Name, "sl1_dsp") == 0) {
      targ = TARGET_sl1_dsp;
    } else if (strcasecmp (Processor_Name, "sl2_pcore") == 0 ) {
      targ = TARGET_sl2_pcore;
    } else if (strcasecmp (Processor_Name, "sl2_mcore") == 0 ) {
      targ = TARGET_sl2_mcore;
    }
#endif
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
  switch ( Target ) {
    case TARGET_sb1:
	Target_ISA = TARGET_ISA_Mips64;
	Target = TARGET_sb1;
	break;
    case TARGET_R10K:
	Target_ISA = TARGET_ISA_Mips64;
	Target = TARGET_R10K;
	break;
#ifdef TARG_SL
    case TARGET_sl1_pcore:
        Target_ISA = TARGET_ISA_Mips64;
        Target = TARGET_sl1_pcore;
        break;
    case TARGET_sl1_dsp:
        Target_ISA = TARGET_ISA_Mips64;
        Target = TARGET_sl1_dsp;
        break;
    case TARGET_sl2_pcore:
        Target_ISA = TARGET_ISA_Mips64;
        Target = TARGET_sl2_pcore;
        break;
    case TARGET_sl2_mcore:
        Target_ISA = TARGET_ISA_Mips64;
        Target = TARGET_sl2_mcore;
        break;
#endif
    case TARGET_UNDEF:
      Target = targ_default;
      if ( Target == TARGET_UNDEF ) {
	/* Default everything: */
	Target_ISA = TARGET_ISA_Mips64;
	Target = TARGET_sb1;
      }
      break;
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

  /* Set descriptive variables: */
  Target_int64 = FALSE;
  Use_32_Bit_Pointers = (Target_ABI == ABI_N32);
#if defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)
#ifndef KEY
  // At KEY we initialize integer model in Initialize_C_Int_Model
  // Look in gccfe/c_int_model.c
  Target_Int_Model = ( Target_ABI == ABI_N64 ) ? TARGET_INT_LP64
				       : TARGET_INT_ILP32;
#endif
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

#if !(defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS))
  if (Endian_Name == NULL)
#if defined(TARG_SL) || defined(TARG_MIPS)
    Target_Byte_Sex = LITTLE_ENDIAN;
#else
    Target_Byte_Sex = BIG_ENDIAN;
#endif
  else Target_Byte_Sex = strncmp(Endian_Name, "lit", 3) == 0 ? LITTLE_ENDIAN 
      							     : BIG_ENDIAN;
#endif
  Same_Byte_Sex = ( Target_Byte_Sex == Host_Byte_Sex );
#ifdef TARG_SL 
  Gen_PIC_Calls = FALSE;	
#else 
  Gen_PIC_Calls = TRUE;	
#endif
  GP_Is_Preserved = FALSE;

  /* Set up the target processor and ISA: */
  Prepare_Target ();

  /* Unrolling defaults */
  if (OPT_unroll_times > 0 && !OPT_unroll_times_overridden) 
#if defined(TARG_SL)
    OPT_unroll_times = 0;
#else
    OPT_unroll_times = 4;
#endif

  /* Set up the target register set: */
#if defined(TARG_SL)
  Spill_Int_Mtype = MTYPE_I4;
  Spill_Float_Mtype = MTYPE_F4;
  Spill_Int32_Mtype = MTYPE_I4;
  Spill_Float32_Mtype = MTYPE_F4;
  Max_Int_Mtype = Def_Int_Mtype = MTYPE_I4;
  Max_Uint_Mtype = Def_Uint_Mtype = MTYPE_U4;
#else
  Spill_Int_Mtype = MTYPE_I8;
  Spill_Int32_Mtype = MTYPE_I4;
  Spill_Float32_Mtype = MTYPE_F4;
  Spill_Float_Mtype = MTYPE_F8;
  Max_Int_Mtype = Def_Int_Mtype = MTYPE_I8;
  Max_Uint_Mtype = Def_Uint_Mtype = MTYPE_U8;
#endif
  Boolean_type  = MTYPE_I4;
  Boolean_type2 = MTYPE_I4;
  Integer_type = MTYPE_I4;

  Split_Quad_Ops = TRUE;
  Split_64_Bit_Int_Ops = FALSE;

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
    if (Target_ABI == ABI_N32) {
	 Pointer_Size = 4;
	 Pointer_Mtype  = WHIRL_Mtype_A_On ? MTYPE_A4 : MTYPE_U4;
	 Pointer_type   = Pointer_Mtype;
	 Pointer_Mtype2 = MTYPE_U4;
	 Pointer_type2  = MTYPE_U4;
    } else {
	Pointer_Size = 8;
	Pointer_Mtype  = WHIRL_Mtype_A_On ? MTYPE_A8 : MTYPE_U8;
	Pointer_type   = Pointer_Mtype;
	Pointer_Mtype2 = MTYPE_U8;
	Pointer_type2  = MTYPE_U8;
    }

    Integer_type = MTYPE_I4;
    Boolean_type  = MTYPE_I4;
    Boolean_type2 = MTYPE_I4;

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

  Indexed_Loads_Allowed = TRUE;

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
      Target_ABI = ABI_N64;
      break;
    case ABI_N64:
      break;
    default:
      return FALSE;
    }
  } else {	/* 32 */
    switch (Target_ABI) {
    case ABI_UNDEF:
      Target_ABI = ABI_N32;
      break;
    case ABI_N32:
      break;
    default:
      return FALSE;
    }
  }

  if (Target_ISA == TARGET_ISA_UNDEF) {
      Target_ISA = TARGET_ISA_Mips64;
  }

  return TRUE;
}

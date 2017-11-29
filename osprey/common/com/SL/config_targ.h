/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#ifndef config_targ_INCLUDED
#define config_targ_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: config_targ.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:18:02 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/com/MIPS/config_targ.h,v $
 *
 * Revision history:
 *  15-Jun-91 - Original Version
 *  25-Sep-91 - Added subprogram interface pointers.
 *  27-Nov-91 - Reconfiguration of target representation for TP.
 *  21-Apr-93 - Set TARG_NEEDS_QUAD_OP to 1
 *  14-Mar-97 - Removed most -TARG group flags to config_targ_opt.h.
 *
 * Description:
 *
 * This file defines general configuration parameters which are
 * specific to the compiler's target machine and system.
 * There is an approximate distinction between -TARG option group
 * flags and their configuration (in config_targ_opt.[hcxx]), and more
 * generic target configuration (in this file).  Note that this file
 * is included in config.h, and hence indirectly in most source files,
 * whereas config_targ_opt.h is only included directly, so putting new
 * -TARG option-related variables in config_targ_opt.[hcxx] is to be
 * preferred to putting them here.
 *
 * See com/config.h for a detailed description of configuration
 * parameters, where they are declared, and how they are processed.
 *
 * See also:
 *	TARGET/com/targ_const.h -- Manipulation of target constants, as
 *		appropriate on the host system.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *config_targ_rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/common/com/MIPS/config_targ.h,v $ $Revision: 1.1 $";
#endif /* _KEEP_RCS_ID */

#include "mtypes.h"


/* ====================================================================
 *
 * Target selection
 *
 * Note that the current target is also given by a #define name of the
 * form TARG_xxx.  However, since a single compiler may support several
 * target variants, this symbol (defined in the TDT) will generally be
 * less specific than the variable Target below.
 *
 * ====================================================================
 */

/* ========== */
/* Target ABI */
/* ========== */

/* ABI_N32 collides with the ABI_N32 macro defined in */
/* kg++fe/gnu/config/MIPS/mips.h. */
#undef ABI_N32
typedef enum {
  ABI_UNDEF,	/* Undefined */
  ABI_N32,	/* 32-bit */
  ABI_N64	/* 64-bit */
} TARGET_ABI;

extern TARGET_ABI Target_ABI;

/* ================ */
/* Target processor */
/* ================ */

/* Define an enum representing all current target processors.
 * For variants and modifiers of these, see tp/com/config_targ.h.
 */
typedef enum {
  TARGET_UNDEF,		/* Undefined */
  TARGET_sb1,
  TARGET_R10K,
  TARGET_sl1_pcore,
  TARGET_sl1_dsp,
  TARGET_sl2_pcore,
  TARGET_sl2_mcore,
  TARGET_sl5,
} TARGET_PROCESSOR;
#define Is_Target_Sb1() (Target == TARGET_sb1)
#define Is_Target_R10K() (Target == TARGET_R10K)
#ifdef TARG_SL
#define Is_Target_Sl1_pcore() (Target == TARGET_sl1_pcore)
#define Is_Target_Sl1_dsp() (Target == TARGET_sl1_dsp)
#define Is_Target_Sl2_pcore() (Target == TARGET_sl2_pcore)
#define Is_Target_Sl2_mcore() (Target == TARGET_sl2_mcore)
#define Is_Target_Sl5() (Target == TARGET_sl5)
#endif
#define Is_Target_64bit() (Target_ABI == ABI_N64)
#define Is_Target_Opteron() (0)

extern TARGET_PROCESSOR Target;		/* -Tc */

/* return the target name for <target> */
extern char *Targ_Name (TARGET_PROCESSOR target);

#define Is_Target_Itanium()	(Target==TARGET_ITANIUM)
#define Is_Target_32bit()   (Target_ABI==ABI_N32)


/* ========== */
/* Target ISA */
/* ========== */

typedef enum {
  TARGET_ISA_UNDEF,	/* Undefined */
  TARGET_ISA_M1,
  TARGET_ISA_M2,
  TARGET_ISA_M3,
  TARGET_ISA_M4,
  TARGET_ISA_Mips64,
  TARGET_ISA_SL1,
  TARGET_ISA_SL2,
  TARGET_ISA_SL5
} TARGET_ISA;

extern TARGET_ISA Target_ISA;	/* -Tc.Rc */

/* return the ISA name corresponding to <target_isa> */
extern char *Isa_Name (TARGET_ISA target_isa);

#define Is_Target_ISA_M1()	(Target_ISA==TARGET_ISA_M1)
#define Is_Target_ISA_M2()	(Target_ISA==TARGET_ISA_M2)
#define Is_Target_ISA_M3()	(Target_ISA==TARGET_ISA_M3)
#define Is_Target_ISA_M4()	(Target_ISA==TARGET_ISA_M4)
#define Is_Target_ISA_Mips64()	(Target_ISA==TARGET_ISA_Mips64)
#define Is_Target_ISA_SL1()	(Target_ISA==TARGET_ISA_SL1)
#define Is_Target_ISA_SL2()	(Target_ISA==TARGET_ISA_SL2)
#define Is_Target_ISA_SL5()	(Target_ISA==TARGET_ISA_SL5)
#define Is_Target_ISA_M1Plus()	(Target_ISA >= TARGET_ISA_M1)
#define Is_Target_ISA_M2Plus()	(Target_ISA >= TARGET_ISA_M2)
#define Is_Target_ISA_M3Plus()	(Target_ISA >= TARGET_ISA_M3)
#define Is_Target_ISA_M4Plus()	(Target_ISA >= TARGET_ISA_M4)

/* What is the floating point format? */
#define IEEE_FP_FORMAT	TRUE
#define IBM_FP_FORMAT	FALSE

/* Do we need to implement quad float? */
/* #define TARG_NEEDS_QUAD_OPS */

/* Target-specific controls: */

/* We need to know the machine type of the integer and floating point
 * registers for purposes of subprogram save/restore:
 */
extern CLASS_INDEX Spill_Int_Mtype;
extern CLASS_INDEX Spill_Float_Mtype;
extern CLASS_INDEX Spill_Int32_Mtype;
extern CLASS_INDEX Spill_Float32_Mtype;

/* We need to know the maximum machine type corresponding directly to
 * the machine registers:
 */
extern CLASS_INDEX Max_Int_Mtype, Max_Uint_Mtype;

/* We need to know the default machine type to be used for internally
 * generated integers, e.g. DO loop indices.  These will default to the
 * max int types above, but the FEs may override the defaults:
 */
extern CLASS_INDEX Def_Int_Mtype, Def_Uint_Mtype;

#if defined(INT32_MAX)
#undef INT32_MAX
#undef INT32_MIN
#endif
#define INT32_MAX INT_MAX
#define INT32_MIN INT_MIN

/* =================== */
/* Target Pointer Size */
/* =================== */

/* On MIPS III targets, should we use 32-bit pointers? */
extern BOOL Use_32_Bit_Pointers;	/* -Tc.P32 */

/* For various targets, what are the characteristics of pointers */
extern INT		Pointer_Size;	/* in bytes */
extern CLASS_INDEX	Pointer_Mtype;
extern CLASS_INDEX	Pointer_Mtype2;

/* What are pointers and booleans aliased to in WHIRL */
extern TYPE_ID Pointer_type;
extern TYPE_ID Pointer_type2;
extern TYPE_ID Boolean_type;
extern TYPE_ID Boolean_type2;
extern TYPE_ID Integer_type;

/* On every target, there is an upper bound on the size pf data object
 * that can be defined.  The following two definitions define this
 * upper bound for 32 and 64 bits mode compilations respectively.
 */
#define MAX_64BITS_OBJECT_SIZE 0x000000ffffffffffULL /* 40 bits */
#define MAX_32BITS_OBJECT_SIZE 0x000000007fffffffULL /* 31 bits */

/* On most targets, it will be necessary to decide whether a text
 * address is usable in an instruction literal field.  The following
 * specifies the maximum value which we may assume a function or label
 * address will take on.
 *
 * NOTE:  On the MIPS, jumps are relative to the current 256MB segment,
 * and we always assume that we can reach anything we want to jump/call
 * to, since the linker will insert stubs if necessary.  So we treat
 * the limit as 256MB here.  We make it smaller than necessary to allow
 * for some arithmetic without overflowing the limits.
 */
#define MAX_LABEL_SYMVAL	0x03fffff0
#define MAX_FUNC_SYMVAL		0x03fffff0

/* maximum gspace (gp-relative) size */
//#define DEFAULT_GSPACE         0x3fffff
#define DEFAULT_GSPACE		     0x7fff

/* ================== */
/* Comparison Results */
/* ================== */

/* For various targets, what is the comparison result type? */
extern INT		Comparison_Result_Size;	/* in bytes */
extern CLASS_INDEX	Comparison_Result_Mtype;

/* ============= */
/* Miscellaneous */
/* ============= */

/* Miscellaneous exception control */
#define FPX_I	0x01	/* IEEE-754 inexact exception */
#define FPX_U	0x02	/* IEEE-754 underflow exception */
#define FPX_O	0x04	/* IEEE-754 overflow exception */
#define FPX_Z	0x08	/* IEEE-754 zero divide exception */
#define FPX_V	0x10	/* IEEE-754 invalid operation exception */
#define FPX_ALL	0x1f	/* All the IEEE-754 exceptions */
#define EXC_ALL	0x1f	/* All the exceptions */
extern INT16 FP_Exception_Enable_Max;	/* Max FP trap enables */
extern INT16 FP_Exception_Enable_Min;	/* Min FP trap enables */

/* Is the "char" type signed? */
extern BOOL Char_Type_Is_Signed;


/* ====================================================================
 *
 * Target debugging options
 *
 * ====================================================================
 */

/* Symbolic Debug mode as specified on command line.  (The mode can
 * change from PU to PU because, for example, we encounter a call to
 * the routine 'alloca()' -- we must restore the mode to the value as it
 * was specified on the command line, so we remember it.)
 */
#define SDM_NONE	0x00	/* No information */
#define SDM_LINE	0x01	/* Line numbers */
#define SDM_SYMBOL	0x02	/* Symbol table information */
#define SDM_SEQLINE	0x04	/* Force sequentail line numbers */
#define SDM_GEN_FP	0x08	/* Save/restore of fp for sdb */
#define SDM_USE_FP	0x10	/* Use fp for sdb definitions */

extern INT16 Symbolic_Debug_Mode;	/* Current */
extern INT16 Max_Symbolic_Debug_Mode;	/* Maximum for any PU */

#define Want_Line_Debugging	(Symbolic_Debug_Mode & SDM_LINE)
#define Want_Sequential_Line_Debugging	(Symbolic_Debug_Mode & SDM_SEQLINE)
#define Want_Symbol_Debugging	(Symbolic_Debug_Mode & SDM_SYMBOL)
#define Want_Any_Debugging	(Symbolic_Debug_Mode &(SDM_LINE|SDM_SEQLINE|SDM_SYMBOL))
#define Gen_Frame_Ptr		(Symbolic_Debug_Mode & SDM_GEN_FP)
#define Set_Gen_Frame_Ptr	(Max_Symbolic_Debug_Mode |= SDM_GEN_FP, \
				  Symbolic_Debug_Mode |= SDM_GEN_FP)
#define Use_Frame_Ptr_For_Sdb	(Symbolic_Debug_Mode & SDM_USE_FP)
#define Set_Use_Frame_Ptr_For_Sdb (Max_Symbolic_Debug_Mode |= SDM_USE_FP, \
				   Symbolic_Debug_Mode |= SDM_USE_FP)

/* I believe the frame pointer flags function as follows:
 *
 * The debugger may require that a frame pointer be generated and
 * available (Gen_Frame_Ptr), and/or that it be used in the external
 * symbol table definitions of variables' addressing emitted in
 * coffsdb.c (Use_Frame_Ptr_For_Sdb).  We currently treat these
 * independently, though perhaps Use_Frame_Ptr_For_Sdb should imply
 * Gen_Frame_Ptr.
 *
 * In addition, memmodel.[hc] contains variables Gen_Frame_Pointer
 * and Use_Frame_Pointer which actually control whether code is
 * generated to produce the FP on PU entry and use it for referencing
 * part of the stack frame, respectively.  Gen_Frame_Pointer is
 * initialized to Gen_Frame_Ptr, but may also be turned on by the
 * Use_Frame_Pointer requirement.  Use_Frame_Pointer is currently
 * set only if the stack frame is large enough that we want both
 * pointers to improve addressibility; it will also probably be set
 * if we need to deal with dynamic local data allocation.  It is
 * therefore completely independent of Use_Frame_Ptr_For_Sdb.
 */

/* ====================================================================
 *
 * Miscellaneous options
 *
 * ====================================================================
 */

/* Do we need to force NOOPs in empty cycles? */
#define FORCE_NOOPS	FALSE

/* Can 64-bit values be 4-byte aligned in memory? */
extern BOOL Allow_Word_Aligned_Doubles;

/* Should we generate position-independent code by default? */
extern BOOL Generate_Position_Independent_Code;

/* Split 64-bit integer ops into 32-bit ops, and simulate them? */
extern BOOL Split_64_Bit_Int_Ops;

/* Split quad-precision ops into double-precision, and simulate them? */
extern BOOL Split_Quad_Ops;

/* Should we simulate 32-bit subprogram interface by default? */
extern BOOL Simulate_32_Bit_Interface;

/* Workaround for TFP branch cache problem. */
extern BOOL No_Quad_Aligned_Branch;
extern BOOL No_Quad_Aligned_Branch;

/* Does target provides only unsigned 64-bit instructions? */
extern BOOL Only_Unsigned_64_Bit_Ops;

extern INT32 Align_Instructions;	/* Align frequent blocks and funcs */

extern BOOL Eager_Bottom_Load;
extern BOOL Eager_Ptr_Deref;
extern BOOL Eager_Null_Ptr_Deref;

/* file has non-standard gp groups, i.e. SYMTAB_gp_group(Global_Symtab) */
extern BOOL Has_GP_Groups;

/* Does target have offsets in load and store instructions?
 * Note: CG should instead test:
 * ( TOP_Find_Operand_Use( OP_code(op), OU_offset ) >= 0 ) */
extern BOOL Use_Load_Store_Offset;


/* ====================================================================
 *
 * Initialization interface
 *
 * ====================================================================
 */

/* Configuration prior to flag processing: */
extern void Preconfigure_Target (void);

/* Configuration after flag processing: */
extern void Configure_Target (void);
extern void IPA_Configure_Target (void);

/* Reconfiguration for each source file: */
extern void Configure_Source_Target ( char *filename );

/* return FALSE if abi mismatch */
extern BOOL Set_Target_ABI (BOOL is_64bit, INT isa);


/*================================================================*/
/* Architecture specific options. Variables are set in config_targ.cxx,
   #defines are set in config_targ.h
*/

extern BOOL ARCH_mask_shift_counts;
extern BOOL ARCH_generate_nor;
#define ARCH_recip_is_exact TRUE
#define ARCH_has_bit_tests TRUE

/* default value for WHIRL_Keep_Cvt */
#define DEFAULT_KEEP_CVT	TRUE

#ifdef __cplusplus
}
#endif
#endif /* config_targ_INCLUDED */

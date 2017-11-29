/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef config_INCLUDED
#define config_INCLUDED

/* ====================================================================
 * ====================================================================
 *
 * Module: config.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config.h,v $
 *
 * Revision history:
 *  09-Apr-90 - Original Version
 *  01-Feb-91 - Copied for TP/Muse
 *  15-Jun-91 - Restructuring, Josie integration
 *
 * Description:
 *
 * Declare parameters describing the current host/target configuration,
 * and configuration options selected by the user in the command line.
 *
 * ====================================================================
 * NOTE:  We want to split out the declarations for various option
 * groups, so that adding to them doesn't cause virtually everything
 * to compile.  These are declared in files named
 * common/com/config_groupname.h, and should be used instead of adding
 * to this file, although many of the -OPT group options are still
 * here (feel free to move them).
 * ====================================================================
 *
 * Most of the compiler is designed to be independent of the host
 * (the machine and system where the compiler runs) and of the target
 * (the machine and system where the compiled code will run).  This
 * file (with those it includes) is the central repository for
 * parameters and data which help isolate those parts of the compiler
 * which are not host- and/or target-independent.  Note that we assume
 * in general that the "host" for building the compiler and the "host"
 * for running the compiler are the same.  Although this need not be
 * true, it generally will be except for bootstrap phases.  Given a
 * cross-compiler on the build host which is reasonably compatible with
 * the eventual native compiler on the compilation host, the
 * differences in the compiler code should be limited to type
 * definitions in defs.h and conceivably to the tdgen programs, which
 * will be built and run on the build host.
 *
 * CONFIGURATION PARAMETER CLASSES
 *
 * We recognize several classes of such parameters -- the following
 * attempts to provide guidelines on where to declare them and where
 * to process them.
 *
 * In the following description, "TARGET" refers to the directory
 * containing the host- and target-specific sources and build
 * directories; in the Twin Peaks case, it is "tp".
 *
 *  Ubiquitous:	Some configuration parameters are used essentially
 *		everywhere in the compiler, such as the basic data
 *		type declarations INT32, TN_NUM, and the like.
 *		Notwithstanding the following classifications, these
 *		are declared in TARGET/com/defs.h, which is included
 *		in (virtually) all source files.
 *
 *  Host Machine: Some parameters reflect the capabilities of the host
 *		hardware.  The most important other than the basic
 *		types defined in defs.h involves the representation
 *		and manipulation of target constants on the host
 *		machine, declared in file TARGET/com/targ_const.h.
 *		The remainder will be defined in the file
 *		TARGET/com/config_host.h, which is included in this
 *		file.
 *
 *  Host System: Some parameters reflect the configuration of the host
 *		software, e.g. the pathnames of library files, the
 *		syntax of the assembler, etc.  Those required broadly
 *		will be declared in TARGET/com/config_host.h; those
 *		with a more restricted clientele (e.g. the assembler
 *		interface) will be maintained in more specific files.
 *
 *		Note that we include in this class parameters which
 *		depend on which compiler process is being built, i.e.
 *		driver, front end, or back end.  Definitions specific
 *		to a single process should be #ifdef'ed based on
 *		DRIVER, FRONT_END, or BACK_END.
 *
 *  Target Machine: Most parameters dependent on the target machine
 *		are defined by the Target Information facility, and
 *		are used in general for driving the code generation process
 *		throughout the compiler.  The interface to the target
 *		information is in the form of generated header files
 *		in TARGET/targ_info. The generator programs and their
 *		input are all under common/targ_info.
 *
 *		The second significant component of this class is the
 *		handling of target constants; as described above, the
 *		interface to this functionality is in the file
 *		TARGET/com/targ_const.h.
 *
 *		Other parameters in this class should be declared in
 *		TARGET/com/config_targ.h.
 *
 *  Target System: Parameters in this class, e.g. runtime library
 *		interface information, will generally have a limited
 *		clientele, and will be in files reflecting this usage.
 *		Some such information is in the TDT, either due to its
 *		effect on code generation or due to the need to
 *		preprocess it in tdgen.  Anything else should be in
 *		TARGET/com/config_targ.h.
 *
 *  Compiler:	Parameters in this class control processing choices
 *		made by the compiler, independent of host or target,
 *		e.g. which optimizations to perform, how to schedule,
 *		and the like.  They will generally be declared in the
 *		driver modules for the relevant phases (e.g.
 *		be/cg/cg.h, be/gopt/opt.h), and initialized by the
 *		configuration routines (see below).
 *
 *  Options:	Many configuration parameters are set or affected by
 *		options specified by the command line or environment.
 *		Such parameters which need to be widely available will
 *		be in this file; those required only by command line
 *		processing will be in flags.h.  NOTE: choices made
 *		based on user interface flags like optimization level
 *		should be implemented by declaring a control variable
 *		and setting it in the configuration routines.  We want
 *		to be able to modify the effect of specific flag
 *		settings in a central place where the interactions are
 *		clear, rather than by searching the compiler for who
 *		depends on something like optimization level.
 *
 *		NOTE ALSO:  There is a general interface to compiler
 *		controls, settable via either the command line or
 *		pragmas, in controls.h.  It will still be preferable
 *		to interface to a control variable declared here, but
 *		the general, direct interface is available there.
 *
 *  Global Data: Such data, of a non-configuration nature (e.g. the
 *		current file names), used to be declared in flags.h.
 *		It is now declared in the process' miscellaneous
 *		globals header (i.e. glob.h) and defined in the
 *		process driver (e.g. bedriver.c for the back end).
 *
 * CONFIGURATION PROCESSING
 *
 * Each of the configuration files described above has an associated
 * source file with three external routines (at least):
 *
 *  Preconfigure_Xxx:  Does configuration required before flag
 *		processing (Xxx = Host, Target, etc.).
 *
 *  Configure_Xxx:  Does configuration required after flag processing.
 *
 *  Configure_Source_Xxx:  Does configuration required for each source
 *		file compiled.
 *
 * Processing will involve calling, at compiler initialization time,
 * the Preconfigure routines, flag processing, and the Configure
 * routines.  Then, as each source file is processed, the driver will
 * call the Configure_Source routine.
 *
 * CONTENTS
 *
 * - Memory allocation configuration.
 *
 *
 * Exported variables:
 *
 *	TODO: Cleanup organization of the *_Level and Max_Sdata_Elt_Size
 *	controls.  For example, the Debug and Profile maximums and defaults
 *	are determined by identical code in fedriver.c and bedriver.c.
 *	Eagerness levels need their own enumerated type.
 *
 *	INT8 Opt_Level
 *	    Optimization level, as set by -0 switch.  Bounded by
 *	    MAX_OPT_LEVEL.  Default is DEF_OPT_LEVEL if -O not given,
 *	    or DEF_O_LEVEL if -O given with no argument.
 *
 *	INT8 Debug_Level
 *	    Debug level, as set by -g switch.  Bounded by MAX_DEBUG_LEVEL.
 *	    Defaults to DEF_DEBUG_LEVEL if -g not given or 2 if -g given
 *	    with no argument (see fedriver.c, bedriver.c).
 *
 *	INT32 Max_Sdata_Elt_Size
 *	    Maximum size of data elements allowed in .sdata/.sbss 
 *	    (short data/bss) sections.  Bounded by MAX_SDATA_ELT_SIZE.
 *	    Defaults to DEF_SDATA_ELT_SIZE.
 *
 *	INT16 Eager_Level
 *	    Eagerness level for speculative operations, as set by -X
 *	    switch.  Valid levels are:
 *	     EAGER_NONE		No speculative operations
 *	     EAGER_SAFE		Only exception-safe speculative ops
 *	     EAGER_ARITH	Arithmetic exceptions off
 *	     EAGER_DIVIDE	Divide by zero exceptions off
 *	     EAGER_MEMORY	Memory exceptions off
 *	     EAGER_OTHER	All speculative ops allowed
 *	    Each level includes the levels above (in this text) it.
 *	    Default is EAGER_SAFE.
 *
 *
 * SEE ALSO
 *
 *   common/com/MIPS/config_platform.h -- target platform configuration
 *   common/com/MIPS/config_targ.h -- target processor configuration
 *   common/com/MIPS/config_host.h -- compiler host configuration
 *   common/com/defs.h -- ubiquitous host types and configuration
 *			  options.
 *   common/util/flags.h -- command-line processing utilities.
 *
 *   common/com/config_ipa.h -- -IPA/-INLINE group options.
 *   common/com/config_opt.h -- -OPT group options.
 *   common/com/config_wopt.h -- -WOPT group options.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *config_rcs_id = "$Source: common/com/SCCS/s.config.h $ $Revision: 1.24 $";
#endif /* _KEEP_RCS_ID */

#include "config_host.h"	/* in TARGET/com */
#include "config_targ.h"	/* in TARGET/com */

#if defined(__MINGW32__) 
/* some systems define these, some don't */
#ifndef BIG_ENDIAN
#define BIG_ENDIAN      4321
#endif
#ifndef LITTLE_ENDIAN
#define LITTLE_ENDIAN   1234
#endif
#elif defined(BUILD_OS_DARWIN) || defined(__CYGWIN__) || defined(__APPLE__)
#include <machine/endian.h>	/* for BIG_ENDIAN, LITTLE_ENDIAN */
#elif !(defined(linux))
#include <sys/endian.h>		/* for BIG_ENDIAN, LITTLE_ENDIAN */
#else
#include <endian.h>		/* for BIG_ENDIAN, LITTLE_ENDIAN */
#endif

#include "mempool.h"	/* Include the "fundamental" routines */
#include "flags.h"

/* IR builder sometimes needs to know whether we're in front end,
 * either to do something differently, or to complain about attempts
 * to handle something that shouldn't be present.  Note that the
 * arguments to the *Assert_Front_End macros are parenthesized
 * parameter lists to ErrMsg or printf.
 */
#ifdef SINGLE_PROCESS
  extern INT16 In_Front_End;
# ifdef Is_True_On
#   define Assert_Front_End(x)		Assert ( In_Front_End, x )
#   define FmtAssert_Front_End(x)	FmtAssert ( In_Front_End, x )
# endif
#else
# ifdef FRONT_END
#   define In_Front_End	TRUE
# else
#   define In_Front_End	FALSE
#   ifdef Is_True_On
#     define Assert_Front_End(x)	ErrMsg x
#     define FmtAssert_Front_End(x)	FmtAssert ( FALSE, x )
#   endif
# endif
#endif

#ifndef Assert_Front_End
# define Assert_Front_End(x)	(void)0
# define FmtAssert_Front_End(x)	(void)0
#endif



/* ====================================================================
 *
 * Miscellaneous configuration options
 *
 * ====================================================================
 */

/***** Language being compiled -- initialized in flags.c *****/
#include "language.h"

#ifdef __cplusplus
extern "C" {
#endif

extern LANGUAGE Language;

/* What is the model to be used for logical values in Fortran?
 *   TRUE:	.TRUE. is 1		(i.e. standard C/Unix model).
 *		truth test is zero/non-zero.
 *   FALSE:	.TRUE. is -1		(i.e. VMS Fortran model).
 *		truth test is LSB test.
 * This variable is defaulted to TRUE; the Fortran FE must set it to
 * FALSE prior to any IRB conversions if the VMS model is desired.
 */
extern	BOOL Use_C_Like_Logicals;

/***** LANGuage group options *****/
extern BOOL CXX_Bool_On;
extern BOOL CXX_Bool_Set;
extern BOOL CXX_Exceptions_On;
extern BOOL CXX_Exceptions_Set;
extern BOOL CXX_Alias_Const;
extern BOOL CXX_Alias_Const_Set;
extern BOOL LANG_Recursive;	/* Fortran program contains recursion */
extern BOOL LANG_Recursive_Set;
extern BOOL CXX_Wchar_On;
extern BOOL CXX_Wchar_Set;

extern BOOL CXX_Namespaces_On;
extern BOOL CXX_Namespaces_Set;
extern BOOL CXX_Ansi_For_Init_Scope_On;
extern BOOL CXX_Ansi_For_Init_Scope_Set;
extern BOOL CXX_Explicit_On;
extern BOOL CXX_Explicit_Set;
extern BOOL CXX_Typename_On;
extern BOOL CXX_Typename_Set;
extern BOOL CXX_Mutable_On;
extern BOOL CXX_Mutable_Set;
extern BOOL CXX_Packed_On;
extern BOOL CXX_Packed_Set;

extern BOOL CXX_Standard_C_Plus_Plus_On;
extern BOOL CXX_Standard_C_Plus_Plus_Set;

extern BOOL LANG_Pch;
extern BOOL LANG_Pch_Set;
extern char *LANG_Create_Pch;
extern BOOL LANG_Create_Pch_Set;
extern char *LANG_Use_Pch;
extern BOOL LANG_Use_Pch_Set;
extern char *LANG_Pchdir;
extern BOOL LANG_Pchdir_Set;

extern char *LANG_cxx_dialect;
extern BOOL LANG_cxx_dialect_Set;

extern BOOL LANG_Microsoft_Mode;
extern BOOL LANG_Microsoft_Mode_Set;

extern BOOL LANG_Ansi_Setjmp_On;
extern BOOL LANG_Ansi_Setjmp_Set;

extern BOOL LANG_Ignore_Carriage_Return_On;
extern BOOL LANG_Ignore_Carriage_Return_Set;

#ifdef KEY /* Bug 3405 */
extern BOOL LANG_IEEE_Minus_Zero_On;
extern BOOL LANG_IEEE_Minus_Zero_Set;
#endif /* KEY Bug 3405 */

extern BOOL C_Restrict_On;
extern BOOL C_Restrict_Set;

extern char *C_Auto_Restrict;
extern BOOL C_Auto_Restrict_Set;

extern BOOL FTN_Short_Circuit_On;
extern BOOL FTN_Short_Circuit_Set;

extern BOOL WHIRL_Comma_Rcomma_On;
extern BOOL WHIRL_Comma_Rcomma_Set;

extern BOOL Macro_Expand_Pragmas_On;
extern BOOL Macro_Expand_Pragmas_Set;

extern BOOL C_VLA_On;
extern BOOL C_VLA_Set;

extern BOOL WHIRL_Merge_Types_On;
extern BOOL WHIRL_Merge_Types_Set;

extern BOOL LANG_Symtab_Verify_On;
extern BOOL LANG_Symtab_Verify_Set;
#ifdef KEY
extern BOOL LANG_Formal_Deref_Unsafe;
extern BOOL LANG_Math_Errno;
extern BOOL LANG_Enable_CXX_Openmp;
extern BOOL LANG_Enable_Global_Asm;
#endif

extern BOOL WHIRL_Mtype_A_On;
extern BOOL WHIRL_Mtype_B_On;
extern BOOL WHIRL_Mtype_BS_On;
extern BOOL WHIRL_Return_Val_On;
extern BOOL WHIRL_Flatten_Field_On;
extern BOOL WHIRL_Mldid_Mstid_On;
extern BOOL WHIRL_Vfcall_On;
extern BOOL WHIRL_Addr_Passed_On;
extern BOOL WHIRL_Addr_Saved_For_Passed_On;
extern BOOL WHIRL_Addr_Saved_On;
extern BOOL WHIRL_Keep_Cvt_On;
extern BOOL WHIRL_Return_Info_On;

extern BOOL Global_Pragmas_In_Dummy_PU_On;
extern BOOL Malloc_Free_On;
extern BOOL Alloca_Dealloca_On;
extern BOOL Barrier_Lvalues_On;

#ifdef TARG_IA64
extern BOOL Use_Call_Shared_Link; /*For redundant save/restore gp opt */
extern BOOL Gp_Save_Restore_Opt;
extern BOOL Gp_Rel_Aggresive_Opt;
#endif

/***** The following is TRUE for C++  unless -no_exceptions is specified *****/
extern BOOL Allow_Exceptions;

/***** Compiler debug/trace options *****/
extern BOOL Tracing_Enabled;	/* Any trace options set? */

/* Control usage of the .I and .J files: */
extern BOOL Open_IJ_Files;

/* For communication between driver and config routines ONLY: */
extern INT8 Debug_Level;
# define MAX_DEBUG_LEVEL	3

typedef enum {
  EAGER_NONE,
  EAGER_SAFE,
  EAGER_ARITH,
  EAGER_DIVIDE,
  EAGER_MEMORY,
  EAGER_OTHER,
  EAGER_EXCESS
} EAGER_LEVEL;
extern EAGER_LEVEL Eager_Level;

/***** Miscellaneous optimization options *****/
/* Should idict commute operands in seeking match? */
extern BOOL Idict_Commutable_Match;
extern BOOL Enable_FE_Optimization;	/* Enable FE (KAP) scalar opt? */
extern BOOL Alias_Pointer_Parms;	/* Reference parms indep? */
extern BOOL Alias_Pointer_Types;	/* Ptrs to distinct basic types indep? */
extern BOOL Alias_Not_In_Union;          /* C++ ONLY rule: assume ptrs to objects with user-constructors are NOT in unions */
extern BOOL Alias_Pointer_Strongly_Typed; /* Ptrs to distinct types indep? */
extern BOOL Alias_Pointer_Named_Data;	/* No pointers to named data? */
extern BOOL Alias_Pointer_Restricted;	/* *p and *q not aliased */
extern BOOL Alias_Pointer_Disjoint;     /* **p and **q not aliased */
extern BOOL Alias_Pointer_Cray;         /* Cray pointer semantics? */
extern BOOL Alias_Common_Scalar;        /* Distinguish scalar from other array
                                           in a common block */

extern BOOL CSE_Elim_Enabled;		/* Is CSE-elim on? -- this does
					 * not control it, it just
					 * shadows the opt. level
					 */
extern BOOL Enable_GOT_Call_Conversion; /* %call16 -> %got_disp? */
extern BOOL OPT_Unroll_Analysis;	/* Enable unroll limitations? */
extern BOOL OPT_Unroll_Analysis_Set;	/* ... option seen? */

extern BOOL Disable_Simplification_For_FE;  /* Disable Simplification for Front End*/

/***** Various Scalar Optimizer options *****/
extern BOOL Enable_Copy_Propagate;

/***** Put all-zero initialized file-level data in the BSS section? *****/
extern BOOL Zeroinit_in_bss;

/***** IEEE 754 options *****/
typedef enum {
  IEEE_STRICT = 0,	/* Conform strictly */
	/* IEEE_STRICT is not supported.  It might be useful to
	 * avoid madds, do gradual underflow, etc...
	 */
  IEEE_ACCURATE = 1,	/* Do not degrade IEEE accuracy */
  IEEE_INEXACT = 2,	/* Inexact results may not be IEEE */
  IEEE_ANY = 3		/* Anything goes */
} IEEE_LEVEL;
extern IEEE_LEVEL IEEE_Arithmetic;  /* IEEE arithmetic? */
extern  BOOL IEEE_Arith_Set;	/* ... option seen? */

/***** Constant folding options *****/
typedef enum {
  ROUNDOFF_NONE,	/* No roundoff-inducing transformations */
  ROUNDOFF_SIMPLE,	/* Simple roundoff transformations */
  ROUNDOFF_ASSOC,	/* Reassociation transformations */
  ROUNDOFF_ANY		/* Anything goes */
} ROUNDOFF;
extern ROUNDOFF Roundoff_Level;		/* -OPT_roundoff=n value */
extern BOOL Roundoff_Set;		/* ... option seen? */
extern BOOL Enable_WN_Simp;             /* Use the WHIRL simplifier? */
extern BOOL Regions_Around_Inner_Loops; /* Put REGIONs around inner loops */
extern BOOL Region_Boundary_Info;	/* calc boundary info for regions */
extern BOOL Cray_Ivdep;   		/* Use Cray meaning for ivdep */
extern BOOL Liberal_Ivdep;   		/* Use liberal meaning for ivdep */
#ifdef TARG_X8664
extern UINT32 Rsqrt_Allowed;		/* Generate RSQRT instruction? */
#else
extern BOOL Rsqrt_Allowed;		/* Generate RSQRT instruction? */
#endif
extern BOOL Recip_Allowed;		/* Generate RECIP instruction? */
extern BOOL Enable_Cfold_Aggressive;	/* Complex constant folding? */
extern BOOL Ptr_Opt_Allowed;	        /* Treat pointers as arrays */
extern BOOL Fast_Complex_Allowed;	/* Enable fast c_div and c_abs? */
extern BOOL Fast_Complex_Set;		/* ... option seen? */
extern BOOL Fast_Bit_Allowed;		/* Fast inlined bit intrinsics? */
extern BOOL Fast_Bit_Set;		/* ... option seen? */
extern BOOL Fast_NINT_Allowed;		/* Fast NINT and ANINT? */
extern BOOL Fast_NINT_Set;		/* ... option seen? */
#ifdef TARG_X8664
extern BOOL Fast_ANINT_Allowed;		/* Fast ANINT? */
extern BOOL Fast_ANINT_Set;		/* ... option seen? */
#endif
extern BOOL Fast_trunc_Allowed;		/* Fast trunc of NINT/ANINT/AINT/AMOD */
extern BOOL Fast_trunc_Set;		/* ... option seen? */
extern BOOL Fast_IO_Allowed;		/* Fast printf/scanf/printw */
extern BOOL Inline_Intrinsics_Allowed;	/* Inline intrinsics? Or lib calls? */
extern BOOL Inline_Intrinsics_Set;	/* ... option seen? */
extern BOOL Simp_Multiply_To_Shift;	/* Change multiplies to shifts? */
extern BOOL Simp_Canonicalize;          /* Simple canon/reassoc */
extern BOOL Simp_Fold_Unsigned_Relops;  /* Simplify unsigned relops */
extern BOOL Simp_Unsafe_Relops;         /* Allow foldings which might cause error if overflow occurs */
extern BOOL Enable_NaryExpr;		/* Allow nary expr in the lowerer */
extern BOOL Enable_NaryExpr_Set;	/* ... option seen? */

#if defined(__linux__) || defined(BUILD_OS_DARWIN)
extern BOOL Enable_WFE_DFE;		/* frontend dead function elimination? */
#endif /* __linux __ */

/***** Global Code Motion (GCM) options *****/
extern BOOL GCM_Eager_Null_Ptr_Deref;   /* allow speculation past the NULL
					   ptr test. assumes page zero as
					   readable */
extern BOOL GCM_Eager_Null_Ptr_Deref_Set; /* ... option seen? */

/***** Miscellaneous GOPT options *****/
#define MAX_OPT_LEVEL	3
#define DEF_O_LEVEL	2	/* Level implied by -O */
#define DEF_OPT_LEVEL	1
extern INT32 Opt_Level;		/* -On level */
extern INT32 OPT_unroll_times;
extern INT32 OPT_unroll_level;
extern BOOL OPT_keep_extsyms;
extern BOOL OPT_unroll_times_overridden;
extern INT32 OPT_unroll_size;
extern BOOL OPT_unroll_size_overridden;
extern BOOL OPT_Lower_Splitsinglecand;
extern BOOL OPT_Lower_Speculate;
extern BOOL OPT_Lower_Treeheight;
extern BOOL OPT_Inline_Divide;
extern BOOL OPT_Space;
extern INT32 Olimit;	/* stop optimization or use regions at this limit */
extern BOOL Olimit_opt;	/* FALSE => stop optimization if Olimit reached;
			 * TRUE  => use regions to optimize if Olimit reached */
extern BOOL CG_mem_intrinsics;
extern BOOL Emulate_memset;
extern INT32 CG_memmove_inst_count;
extern INT32 Optimize_exception_ranges;
extern BOOL Optimize_exception_ranges_set;
extern BOOL CG_memmove_inst_count_overridden;
extern BOOL CG_bcopy_cannot_overlap;
extern BOOL CG_memcpy_cannot_overlap;
extern BOOL CG_memmove_cannot_overlap;
extern BOOL CG_memmove_nonconst;
extern BOOL Allow_wrap_around_opt;
#define DEF_FOLD_ARITH_MAX_INS_CNT 1000
extern INT32 Fold_Arith_Max_INS_CNT;
#define DEF_CONST_COPY_TN_CNT 10000
extern INT32 Const_Copy_TN_CNT;
#define DEF_GOPT_TN_CNT 15000
extern INT32 Gopt_TN_CNT;
extern BOOL Enable_BB_Splitting; /* Split long basic blocks? */
extern INT32 Split_BB_Length;	/* split BBs that are > than this */
#if defined(TARG_IA32) || defined(TARG_X8664)
#define DEF_BBLENGTH	 1300	/* default value for Split_BB_Length */
#define MIN_BBLENGTH	 5	/* allow smaller due to fewer regs and CISC */
#else
#define DEF_BBLENGTH	 300	/* default value for Split_BB_Length */
#define MIN_BBLENGTH	 100	/* don't let the value get too small */
#endif
#define MAX_BBLENGTH	5000	/* don't let the value get too big */

/***** What is the byte sex of the host and target? *****/
extern UINT8 Host_Byte_Sex;	/* Set in config_host.c */
extern UINT8 Target_Byte_Sex;	/* Set in config_targ.c */
extern BOOL  Same_Byte_Sex;	/* Set in config_targ.c */

extern INT32 iolist_reuse_limit;

/***** Misaligned memory reference control *****/
extern INT32 Aggregate_Alignment; /* This alignment for aggregate layout */
extern BOOL Aggregate_Alignment_Set;
extern INT32 Aggregate_UnrollFactor;	/* When lowering aggregate copy into */

extern BOOL Align_Object;	/* Try to improve the alignment of objects */
extern BOOL Align_Padding;	/* Pad objects to their natural alignment */
extern BOOL UseAlignedCopyForStructs;	/* always use aligned copy */
extern BOOL UnweaveCopyForStructs;      /* clump loads before stores */

/***** Miscellaneous code generation options *****/
extern BOOL Gen_PIC_Call_Shared; /* CPIC */
extern BOOL Gen_PIC_Shared;	/* PIC */
extern BOOL Gen_PIC_Calls;	/* do calls as PIC code */
extern BOOL Guaranteed_Small_GOT; /* GOT < 64kB? */
extern BOOL Non_Volatile_GOT;	/* GOT entries volatile? */
extern BOOL PIC_Local_Names;	/* Names local by default? */
extern BOOL PIC_Protected_Names; /* Names protected by default? */
extern BOOL PIC_Fixed_Addresses; /* Names fixed by default? */
extern BOOL PIC_No_Page_Offset;	/* Don't use page/offset addressing? */
extern BOOL Force_Mem_Formals;	/* Always force formals to memory? */
extern BOOL Kernel_Code;	/* Compiling OS kernel? */
extern INT32 Short_Data;	/* Objects of this size in .sdata */
extern INT32 Short_Lits;	/* Literals of this size in .litX */
extern INT32 Max_Sdata_Elt_Size;/* -Gn: sdata size */
extern INT32 Gspace_Available;	/* -Gspace: available space for gprel objects */
extern BOOL Force_GP_Prolog;	/* force usage of gp prolog */
extern INT32 Heap_Allocation_Threshold; /* Allocate objects > this on the heap 
					 * (-1 means always use stack), 
					 * 0 always use heap
					 * default is 0
					 */
extern BOOL Strings_Not_Gprelative;	/* don't make strings gp-relative */
#define MAX_SDATA_ELT_SIZE	32760
#define DEF_SDATA_ELT_SIZE	8
extern BOOL Varargs_Prototypes;	/* Varargs have prototypes for FP? */
extern BOOL Gen_Profile;
extern const char *Gen_Profile_Name;
extern BOOL Call_Mcount;	/* generate a call to mcount in pu entry */
extern BOOL GP_Is_Preserved;	/* GP is neither caller or callee-save */
extern BOOL Constant_GP;	/* GP never changes */

extern char *Emit_Global_Data;	/* only emit global data */
extern char *Read_Global_Data;	/* only read global data */

extern char *Library_Name;              /* -TENV:io_library=xxx */

/* -foptimize-regions implies this internal variable,
 * which is used in cgdwarf_targ.cxx to close emit .restore directive
 */
extern BOOL Omit_UE_DESTROY_FRAME;  /* tmp close Epilogue overflow error */

extern INT  target_io_library;

#if defined (TARG_SL) 
extern BOOL Sl2_Inibuf;
extern char* Sl2_Ibuf_Name;
#endif

#ifdef TARG_X8664
extern char* Mcmodel_Name;              /* -TENV:mcmodel=xxx */
typedef enum {
  SMALL = 0, /* The program and its symbols must be linked in the lower
		2 GB of the address space. This is the default code model.
	     */
  KERNEL,    /* The kernel runs in the negative 2 GB of the address space.
		This model has to be used for Linux kernel code.
	     */
  MEDIUM,    /* The program is linked in the lower 2 GB of the address space but
		symbols can be located anywhere in the address space.
		Programs can be statically or dynamically linked, but building of
		shared libraries are not supported with the medium model.
	     */
  LARGE      /* This model makes no assumptions about addresses and sizes of sections.
	      */
} MCMODEL;
extern MCMODEL mcmodel;
#endif // TARG_X8664

extern BOOL Meld_Schedule;	/* Attempt to meld basic blocks	*/
extern BOOL Gap_Schedule;	/* Attempt to perform gap scheduling */
extern BOOL Attempt_Bypass;	/* Attempt to use bypass registers */
extern BOOL Enable_SWP;		/* Do software pipelining */
extern BOOL Enable_SWP_overridden; /* override on command line */
extern BOOL Enable_LOH;		/* Do loop overhead */
extern BOOL Enable_LOH_overridden; /* Enable_LOH overridden on cmd line */
extern BOOL Enable_Spec_Loads;	/* Allow speculation of loads */
extern BOOL Isolate_Lines;	/* Don't overlap source	lines */
extern BOOL Fill_Delay_Slots;	/* Attempt to fill branch delay slots */
extern BOOL Enable_GDSE;	/* Do global dead store elimination */
extern BOOL Enable_CG_Peephole;	/* Enable peephole optimization in cgprep */
extern BOOL Optimize_CVTL_Exp;	/* Optimize expansion of CVTL operators */
extern BOOL Enable_EBO_Post_Proc_Rgn; /* Enable ebo after global scheduling */
extern BOOL Optimine_CVTL_Exp;	/* Optimize expansion of CVTL operators */
extern BOOL Enable_CVT_Opt;	/* Optimize expansion of CVT operators */
extern BOOL Indexed_Loads_Allowed; /* enable generation of indexed loads/stores */
extern BOOL Early_MP_Processing; /* Do MP lowering before LNO/PREOPT */
extern BOOL Implied_Do_Io_Opt;   /* Do implied-do loop optimization for I/O */

/* back end phases options */
#ifdef BACK_END
extern BOOL Run_lno;		    /* run loop-nest optimizer */
extern BOOL Run_lego;               /* run lego-lowering */
extern BOOL Run_lego_given;         /* was run lego-lowering given/not */
extern BOOL Run_wopt;		    /* run WHIRL global optimizer */
extern BOOL Run_preopt;		    /* run WHIRL preopt optimizer */
extern BOOL Run_cg;		    /* run code generator */
extern BOOL Run_w2c;		    /* run whirl2c */
extern BOOL Run_w2f;		    /* run whirl2f */
extern BOOL Run_w2fc_early;	    /* run whirl2f after LNO parallelization */
extern BOOL Run_ipl;		    /* run summary phase of IPA */
extern char *LNO_Path;		    /* path to lno.so */
extern char *WOPT_Path;		    /* path to wopt.so */
extern char *CG_Path;		    /* path to cg.so */
extern char *W2C_Path;		    /* path to whirl2c.so */
extern char *W2F_Path;		    /* path to whirl2f.so */
extern char *Ipl_Path;		    /* path to ipl.so */
#if defined(TARG_SL)
extern BOOL Run_ipisr;  /* run ipisr register allocation */
#endif
#endif /* BACK_END */
extern char *Inline_Path;           /* path to inline.so */
#if defined(BACK_END) || defined(QIKKI_BE)
extern char *Targ_Path;		    /* path to targinfo .so */
#endif /* defined(BACK_END) || defined(QIKKI_BE) */

extern char *Schedlist_Option;

/* Force EH Region offsets to be long */
extern BOOL Force_Long_EH_Range_Offsets;
/* Force stack frame to use large model */
extern BOOL Force_Large_Stack_Model;
#ifdef TARG_X8664
/* Force stack frame to use frame pointer */
extern BOOL Force_Frame_Pointer;
extern BOOL Force_Frame_Pointer_Set;

/* Cause lowerer to change MTYPE_C8 datatype to MTYPE_V16C8 for SSE3. */
extern BOOL Vcast_Complex;
extern BOOL Vcast_Complex_Set;

/* Use g77 ABI (affects complex and real function return values) */
extern BOOL F2c_Abi;
extern BOOL F2c_Abi_Set;
/* Align double and long double stack variables on a double word boundary. */
extern BOOL Align_Double;
extern BOOL Align_Double_Set;

extern BOOL SIMD_IMask;
extern BOOL SIMD_DMask;
extern BOOL SIMD_ZMask;
extern BOOL SIMD_OMask;
extern BOOL SIMD_UMask;
extern BOOL SIMD_PMask;
extern BOOL SIMD_AMask;
extern BOOL SIMD_FMask;

extern BOOL Use_Sse_Reg_Parm;
extern INT32 Use_Reg_Parm;
#endif
/* put each function in its own text section */
extern BOOL Section_For_Each_Function;

/* list of registers that are not allocatable */
extern OPTION_LIST *Registers_Not_Allocatable;

/* Unique ident from IPA */
extern INT32 Ipa_Ident_Number;

#ifdef KEY
/* Tell ipa_link about the LD_LIBRARY_PATH that was in effect before the
   compiler was run. */
extern char *IPA_old_ld_library_path;

/* Tell ipa_link which compiler to invoke. */
extern char *IPA_cc_name;

/* Tell ipa_link about the source language. */
extern char *IPA_lang;
#endif

extern char *IPA_Object_Name;           /* for -shared -ipa compile */
extern BOOL Scalar_Formal_Ref;		/* for fortran formal scalar refs */
extern BOOL Non_Scalar_Formal_Ref;	/* for fortran formal non_scalar refs */

/***** Maximum sizes of recursive structures we will handle *****/
#define MAXDONEST	300
#define MAXIFNEST	300

/* The following define specifies the maximum evaluation depth from
 * leaf to root in an expression containing boolean operators (AND,
 * OR, NOT).  The amount of space specified will be allocated to hold
 * the inherited attributes of boolean expressions and flow of control
 * statements.
 */
#define MAXBOOLDEPTH    (100 + MAXIFNEST)

/* ====================================================================
 *
 * Skip option interface
 *
 * For debugging purposes, we support the suppression (skipping) of
 * optimization for specific PUs in a compilation (e.g. in WOPT and in
 * IPA).  This is controlled by specifying a range (before/after) or
 * item (ident) of numbers to skip, which produces a group option list
 * from the group processing in flags.c.  The support here provides for
 * (1) converting the group option list to a form easier to query, and
 * (2) querying the resulting skip list.
 *
 * ====================================================================
 */

typedef struct skiplist SKIPLIST;
struct option_list;

/* Convert an option list to a SKIPLIST: */
SKIPLIST *Build_Skiplist ( struct option_list *olist );

/* Query a SKIPLIST -- result TRUE means element is in list: */
BOOL Query_Skiplist ( SKIPLIST *slist, INT32 elmt );

/* SKIPLIST for All Optimizations */
extern SKIPLIST *Optimization_Skip_List;     /* Processed list */
extern SKIPLIST *Region_Skip_List;	     /* regions to skip, processed */
#ifdef KEY
extern SKIPLIST *Goto_Skip_List;     	     /* Processed list */
extern SKIPLIST *DDB_Skip_List;     	     /* Processed list */
#endif

/* ====================================================================
 *
 * Initialization interface
 *
 * ====================================================================
 */

/***** Perform configuration functions prior to flag processing *****/
extern void Preconfigure (void);

/***** Perform configuration specifically in FE if Olegacy is set *****/
extern void Configure_Olegacy (BOOL in_FE);

/***** Perform configuration functions after flag processing *****/
extern void Configure (void);

/***** Perform configuration functions after flag processing for IPA *****/
extern void Configure_IPA (void);

/***** Perform configuration functions for each source file *****/
extern void Configure_Source ( char *filename );

/***** Perform configuration functions for the alias analysis options *****/
extern void Configure_Alias_Options (void);

extern void Configure_Feedback_Options (struct option_list *);


/***** Process a trace option string *****/
extern BOOL Process_Trace_Option ( char *option );

/***** List options to the given file *****/
extern void List_Compile_Options (
  FILE *file,		/* File to which to write */
  const char *pfx,	/* Prefix output with this string */
  BOOL internal,	/* Internal or user listing? */
  BOOL full_list,	/* Groups (user)?  All options (internal)? */
  BOOL update );	/* Reset option changed/modified flags? */


#ifndef Is_Target_R4K
#define Is_Target_R4K()		(0)
#endif
#ifndef Is_Target_R5K
#define Is_Target_R5K()		(0)
#endif
#ifndef Is_Target_R8K
#define Is_Target_R8K()		(0)
#endif
#ifndef Is_Target_R10K
#define Is_Target_R10K()	(0)
#endif
#ifndef Is_Target_TP
#define Is_Target_TP()		Is_Target_R8K()
#endif
#ifndef Is_Target_T5
#define Is_Target_T5()		Is_Target_R10K()
#endif
#ifndef Is_Target_Pentium
#define Is_Target_Pentium()	(0)
#endif
#ifndef Is_Target_Itanium
#define Is_Target_Itanium()	(0)
#endif

#ifndef Is_Target_ISA_M1
#define Is_Target_ISA_M1()	(0)
#endif
#ifndef Is_Target_ISA_M2
#define Is_Target_ISA_M2()	(0)
#endif
#ifndef Is_Target_ISA_M3
#define Is_Target_ISA_M3()	(0)
#endif
#ifndef Is_Target_ISA_M4
#define Is_Target_ISA_M4()	(0)
#endif
#ifndef Is_Target_ISA_M2Plus
#define Is_Target_ISA_M2Plus()	(0)
#endif
#ifndef Is_Target_ISA_M3Plus
#define Is_Target_ISA_M3Plus()	(0)
#endif
#ifndef Is_Target_ISA_M4Plus
#define Is_Target_ISA_M4Plus()	(0)
#endif
#ifndef Is_Target_ISA_I1
#define Is_Target_ISA_I1()	(0)
#endif
#ifndef Is_Target_ISA_I1Plus
#define Is_Target_ISA_I1Plus()	(0)
#endif

#ifndef STRTOK
#define STRTOK(a,b)     strtok(a,b)
#endif
#ifndef STRDUP
#define STRDUP(a)       strdup(a)
#endif
#ifndef STRCMP
#define STRCMP(a,b)     strcmp(a,b)
#endif
#ifndef STRCASECMP
#define STRCASECMP(a,b) strcasecmp(a,b)
#endif
#ifndef STRLEN
#define STRLEN(a)       strlen(a)
#endif
#ifndef STRTOL
#define STRTOL(a,b,c)   strtol(a,b,c)
#endif
#ifndef STRCHR
#define STRCHR(a,b)     strchr(a,b)
#endif
#ifndef STRNCMP
#define STRNCMP(a,b,c)  strncmp(a,b,c)
#endif
#ifndef STRCAT
#define STRCAT(a,b)     strcat(a,b)
#endif
#ifndef STRCPY
#define STRCPY(a,b)     strcpy(a,b)
#endif

#ifdef __cplusplus
}
#endif
#endif /* config_INCLUDED */

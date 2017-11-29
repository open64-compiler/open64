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


#ifndef __SYS_IA64_ELF_H__
#define __SYS_IA64_ELF_H__

/* WARNING:  This file is included in, and logically a part of,
 * /usr/include/elf.h.  The distribution of material between them
 * is artificial, generally reflecting the distinction between generic
 * and system-dependent, and may change.  Do not include this file
 * directly -- include /usr/include/elf.h instead.
 */

/*
 *	NAMING CONVENTIONS
 *	------------------
 *
 *	*_IRIX_*	-- symbols defined by SGI/IRIX that are valid 
 *			   on both mips and ia64 processors.
 *
 *	*_MIPS_*	-- mips specific symbols, defined by SGI/IRIX, 
 *			   and valid on mips processors only.
 *
 *	*_IA64_*	-- ia64 specific symbols, defined by SGI/IRIX,
 *			   and valid on ia64 processors only.
 *
 *	*_IA_64_*	-- ia64 specific symbols, defined by INTEL.
 *			   These symbols are NEVER used by SGI/IRIX
 *			   and are included here for documentation 
 *			   purposes only.  In general, for every
 *			   *_IA_64_*  symbol there is a corresponding
 *			   *_IA64_*   symobl, and whenever possible, they
 *			   have the identical binary values.
 *
 */


#include <sys/elftypes.h> 

/* 
 * Random constants
 */

#define ELF_IA64_MAXPGSZ ELF_IRIX_MAXPGSZ
#define ELF_IA64_MINPGSZ ELF_IRIX_MINPGSZ

/* ====================================================================
 *
 * Elf header
 *
 * ====================================================================
 */

/*
 * e_flags -- defined by Intel by not used on Irix/ia64
 *
 *	#define	EF_IA_64_MASKOS		0x0000000f
 *		Reserved by Intel for OS specified features.
 *		Not currently used by Irix.
 *
 *	#define EF_IA_64_ABI64		0x00000010 
 *		If the bit is set   (1) then this is a 64bit ABI file
 *		If the bit is clear (0) then this is a 32bit ABI file.
 *
 *	#define	EF_IA_64_ARCH		0xff000000
 *		Reserved by Intel for architecture version identifier.
 *		Intended to indicate the minimum level of the architecture
 *		required by the object code.  Currently, the only valid
 *		value for this field is 0x00 
 */

/*
 * e_flags -- used by Irix on Intel
 */
#define EF_IA64_PIC		0x00000002
	/* Uses PIC calling conventions.  This code can be part of
	 * either an a.out or a dso.
	 */

#define EF_IA64_CPIC		0x00000004
	/* Uses CPIC (ie. Call PIC) calling conventions.  
	 * This is not currently used in Irix/ia64, but we'll reserve
	 * the bit for future use.
	 */

#define	EF_IA64_ARCH		0xff000000
 	/* Reserved by Intel for architecture version identifier.
	 * Intended to indicate the minimum level of the architecture
 	 * required by the object code.  Currently, the only valid
 	 * value for this field is 0x00 
 	 */
#define	EF_IA64_ABI64		EF_IRIX_ABI64	/* 0x00000010		*/
	/* If the bit is set   (1) then this is a 64bit ABI file
	 * If the bit is clear (0) then this is a 32bit ABI file.
	 * See explanation in /usr/include/sys/elf.h
	 */
/*
 *  Temporary version number for formats prior to formal release.
 *  e_ident[EI_TVERSION]
 */
#define EI_TVERSION 15
#define EV_T_CURRENT 1

/* ====================================================================
 *
 * Program header
 *
 * ====================================================================
 */


/* 
 * Program header types defined by Intel
 */

#define	PT_IA64_ARCHEXT			(PT_LOPROC + 0)
	/* This segment contains SHT_IA64_EXT sections.  If this entry
	 * is present, it must precede all entries of type PT_LOAD
	 */
#define	PT_IA64_UNWIND			(PT_LOPROC + 1)
	/* This segment contains stack unwind tables.
	 */

/*
 * Program header types defined by Irix/ia64
 */
#define	PT_IA64_OPTIONS			(PT_LOPROC + 2)
	/* This segment contains the options section.  By putting
	 * the option section into a special segment, it makes it
	 * easy for the kernel, exec, and/or rld to quickly find
	 * the option section.
	 */

/* 
 * special p_flags
 */

#define PF_IA64_NORECOV		0x80000000
	/* This segment contains SHT_IA64_NORECOV sections 
	 */
/* #define PF_IA64_LOCAL		0x10000000 */
	/* This segment is local to a thread.
	 * 
	 * --------   WARNING:  DO NOT USE PF_IA64_LOCAL ------------
	 *
	 * Currently, it is only .data sections (and .bss ?)
	 * that can marked as LOCAL.   Because of this, it
	 * seems reasonable that we should not use a p_flag but,
	 * rather, should create new p_types for any local sections.
	 *
	 * The decision has not yet be made on this issue.
	 *
	 * TBD: Should PF_IA64_LOCAL be a flag or new p_type ?
	 *
	 */

/* ====================================================================
 *
 * Section Headers
 *
 * ====================================================================
 */

/*
 * sh_type 	-- defined by Intel but not used on Irix/ia64.
 *
 *	#define	SHT_IA64_EXT		(SHT_LOPROC  + 0)	
 *	#define SHT_IA64_UNWIND		(SHT_LOPROC  + 1)
 */

/*
 * sh_type	-- defined by Irix
 *
 *	Section Types are common to all Irix binaries. 
 *
 *	Those with the name SHT_IA64_* are valid on IA64
 *	
 *	
 *	
 */

#define SHT_IA64_EXT		SHT_IRIX_EXT		
#define SHT_IA64_UNWIND		SHT_IRIX_UNWIND		
#define SHT_IA64_UNWIND_INFO	SHT_IRIX_UNWIND_INFO	
#define SHT_IA64_PLT		SHT_IRIX_PLT		
#define SHT_IA64_PLTOFF		SHT_IRIX_PLTOFF		

#define SHT_IA64_LIBLIST	SHT_IRIX_LIBLIST	
#define SHT_IA64_MSYM		SHT_IRIX_MSYM		
#define SHT_IA64_CONFLICT	SHT_IRIX_CONFLICT	
#define SHT_IA64_IFACE		SHT_IRIX_IFACE		
#define SHT_IA64_OPTIONS	SHT_IRIX_OPTIONS	
#define SHT_IA64_SYMBOL_LIB	SHT_IRIX_SYMBOL_LIB	
#define SHT_IA64_EVENTS		SHT_IRIX_EVENTS        	
#define SHT_IA64_DWARF		SHT_IRIX_DWARF		
#define SHT_IA64_CONTENT	SHT_IRIX_CONTENT      	
#define SHT_IA64_PIXIE		SHT_IRIX_PIXIE     	
#define SHT_IA64_XLATE		SHT_IRIX_XLATE		
#define SHT_IA64_XLATE_DEBUG	SHT_IRIX_XLATE_DEBUG	
#define SHT_IA64_WHIRL		SHT_IRIX_WHIRL		

#define SHT_IA64_NUM		SHT_IRIX_NUM 	/* obsolete. do not use */	

/*
 * sh_flags 	-- defined by Intel, but not used on Irix/ia64.
 *
 *	#define SHF_IA_64_SHORT		0x10000000
 *	#define SHF_IA_64_NORECOV	0x20000000
 */

/*
 * sh_flags 	-- defined by Irix/ia64
 */
#define SHF_IA64_NORECOV	0x01000000
#define SHF_IA64_SHORT		SHF_IRIX_GPREL	/* 0x10000000 */

/*
 * special section names
 *
 *      These are the section names for those sections
 *      that exist in both Irix/mips and Irix/ia64 objects.
 */
#define IA64_LIBLIST            ".liblist"
#define IA64_MSYM               ".msym"
#define IA64_CONFLICT           ".conflict"
#define IA64_SDATA              ".sdata"
#define IA64_REL_SDATA          ".rel.sdata"
#define IA64_SRDATA             ".srdata"
#define IA64_RDATA              ".rdata"
#define IA64_SBSS               ".sbss"
#define IA64_LIT4               ".lit4"
#define IA64_LIT8               ".lit8"
#define IA64_LIT16              ".lit16"
#define IA64_EVENTS             ".IA64.events"
#define IA64_INTERFACES         ".IA64.interfaces"
#define IA64_OPTIONS            ".IA64.options"
#define IA64_SYMBOL_LIB         ".IA64.symlib"
#define IA64_DEBUG_INFO         ".debug_info"
#define IA64_DEBUG_LINE         ".debug_line"
#define IA64_DEBUG_ABBREV       ".debug_abbrev"
#define IA64_DEBUG_FRAME        ".debug_frame"
#define IA64_DEBUG_ARANGES      ".debug_aranges"
#define IA64_DEBUG_PUBNAMES     ".debug_pubnames"
#define IA64_DEBUG_STR          ".debug_str"
#define IA64_DEBUG_FUNCNAMES    ".debug_funcnames"
#define IA64_DEBUG_TYPENAMES    ".debug_typenames"
#define IA64_DEBUG_VARNAMES     ".debug_varnames"
#define IA64_DEBUG_WEAKNAMES    ".debug_weaknames"
#define IA64_XLATE              ".IA64.Xlate"
#define IA64_XLATE_DEBUG	".IA64.Xlate_debug"
#define IA64_WHIRL              ".IA64.WHIRL"
#define IA64_CONTENT            ".IA64.content"


#ifdef __osf__
#define IA64_PACKAGE            ".package"
#define IA64_PACKSYM            ".packsym"
#endif /* __osf__ */

/*
 * special section names
 *
 *	These are the section names for sections 
 *	defined by Intel that are unique to ia64.
 */
#define IA64_ARCHEXT		".IA_64.archext"
#define IA64_PLTOFF		".IA_64.pltoff"
#define IA64_UNWIND		".IA_64.unwind"
#define IA64_UNWIND_INFO	".IA_64.unwind_info"

/* ====================================================================
 *
 * Symbol table
 *
 * ====================================================================
 */


/* ====================================================================
 *
 * .IRIX.options Section
 *
 *	[ IA64 specific information documented in this file. ]
 *
 * ====================================================================
 */

/*
 *      Elf_Options.kind == ODK_IA64_REGINFO
 *
 *      masks for Elf_Options.info
 */
typedef struct 
{
	Elf64_Addr	ri_gp_value;	/* initial value of gp		*/

	/* TBD: define rest of Elf_IA64_RegInfo record	*/

}
Elf_IA64_RegInfo;


/*
 *      Elf_Options.kind == ODK_IA64_EXCEPTIONS
 *
 *      masks for Elf_Options.info
 */

/*
 *      Elf_Options.kind == ODK_IA64_HWAND
 *
 *      masks for Elf_Options.info
 */
 /* #define OHWA0_... */

/*
 *      Elf_Options.kind == ODK_IA64_HWAND
 *
 *      masks for following Elf_Options.hwp_flags1
 */
 /* #define OHWA1_... */

/*
 *      Elf_Options.kind == ODK_IA64_HWAND
 *
 *      masks for following Elf_Options.hwp_flags2
 */
 /* #define OHWA2_... */

/*
 *      Elf_Options.kind == ODK_IA64_HWOR
 *
 *      masks for Elf_Options.info
 */
 /* #define OHWO0_... */

/*
 *      Elf_Options.kind == ODK_IA64_HWOR
 *
 *      masks for following Elf_Options.hwp_flags1
 */
 /* #define OHWO1_... */

/*
 *      Elf_Options.kind == ODK_IA64_HWOR
 *
 *      masks for following Elf_Options.hwp_flags2
 */
 /* #define OHWO2_... */


/* ====================================================================
 *
 * .rel, .rela Section
 *
 * ====================================================================
 */

/*
 *----------------------------------------------------------------------------
 * The relocation types, defined below, use the following notations
 * in the description of their calculations/actions.
 *
 *	A		ADDEND	-- The addend used to compute the value of 
 *			the relocatable field.
 *
 *	BD		BASE ADDRESS DIFFERENCE -- A constant that must be
 *			applied to a virtual address.  This constant represents
 *			the difference between the run-time virtual address 
 *			and the link-time virtual address of a particular
 *			segment.  This segment is implied by the value of the 
 *			link-time virtual address.
 *
 *	C		CONTENTS -- The contents of the relocatable field.
 *
 *	P		PLACE	 -- The section offset or address of the storage
 *			unit being relocated (computed using r_offset).  If
 *			the relocation applies to an instruction, this is the
 *			address of the instruction bundle containing the
 *			instruction.
 *
 *	S		SYMBOL	-- The value of the symbol whose index 
 *			resides in the relocation entry.
 *
 *	@gprel(expr)	GP RELATIVE DISPLACEMENT	-- computes the
 *			difference between the effective address specified
 *			by 'expr'  and the value of $GP.
 *
 *	@ltoff(expr)	GOT ENTRY	-- requests creation of a GOT entry
 *			that will hold the full value of the effective address
 *			and computes the gp-relative displacement to the
 *			GOT entry.
 *
 *	@pltoff(symbol)	PLT ENTRY	-- requests creation of a function
 *			descriptor entry for the given symbol and computes the
 *			gp-relative displace to the function descriptor entry.
 *			(function descriptors are kept in the GOT)
 *
 *	@segrel(expr)	SEGMENT RELATIVE	-- computes a segment relative
 *			displacement: the difference between the effective
 *			address of 'expr' and address of the (output) segment
 *			containing the relocatable object.  This is designed
 *			for data structures in read-only memory that contain
 *			pointers.  The relocatable object and the effective
 *			address must be contained within the same segment. 
 *			Applications using these 'pointers' must be aware that
 *			they are segment-relative and must adjust their
 *			values at run-time, using the load address of the
 *			containing segment. No output relocations are 
 *			generated for @segrel's.
 *
 *	@secrel(expr)	SECTION RELATIVE	-- computes a section relative
 *			displacement: the difference between the effective
 *			address of 'expr' and address of the (output) section
 *			containing the effective address.  This relocation
 *			type is designed for references from one 
 *			non-allocatable section to another.  Applications using
 *			these values must be aware that the are section
 *			relative and must adjust their values at run-time,
 *			using the adjusted address of the target section.
 *			No output relocations are generated for @secrel's.
 *
 *	@fptr(symbol)	FUNCTION PTR	-- evaluates to the address of
 *			the "official" function descriptor for the given
 *			symbol. 
 *
 *
 *	NOTE 1:		Relocation type values have been chosen so that the
 *			expression type can be easily extracted by masking
 *			off the lower three or four bits, and the data or
 *			instruction format can be determined in most cases
 *			by looking only at the lower four bits.
 *
 *	NOTE 2:		These relocations appear only in relocatable objects.
 *			They behave identically their R_IA_64_DIR* 
 *			counterparts, with the exception that no output
 *			relocations are generated.  The runtime consumer
 *			of the information is expected to relocate these
 *			values at runtime.
 *
 *	NOTE 3:		These relocations appear only in dynamic executables
 *			and shared objects.  They instruction RLD to
 *			initialize the corresponding function descriptor
 *			with the address and the GP value of the 
 *			referenced function.
 *
 * 
 *----------------------------------------------------------------------------
 */

enum {

/*----------------------------------------------------------------------------
 *  Relocation TYPE	= value	  Field			Calculation
 *----------------------------------------------------------------------------
 */

    R_IA_64_NONE	= 0x00,	/* None			None		*/

    /* unused:	0x01 .. 0x20   						*/

    R_IA_64_IMM14	= 0x21,	/* instr: immediate14	S+A		*/
    R_IA_64_IMM22	= 0x22,	/* instr: immediate22	S+A		*/
    R_IA_64_IMM64	= 0x23,	/* instr: immediate64	S+A		*/
    R_IA_64_DIR32MSB	= 0x24,	/* word32 MSB		S+A		*/
    R_IA_64_DIR32LSB	= 0x25,	/* word32 LSB		S+A		*/
    R_IA_64_DIR64MSB	= 0x26,	/* word64 MSB		S+A		*/
    R_IA_64_DIR64LSB	= 0x27,	/* word64 LSB		S+A		*/

    /* unused:	0x28 .. 0x29   						*/

    R_IA_64_GPREL22	= 0x2a,	/* instr: immediate22	@gprel(S+A)	*/
    R_IA_64_GPREL64I 	= 0x2b,	/* instr: immediate64	@gprel(S+A)	*/

    /* unused:	0x2c .. 0x2d						*/

    R_IA_64_GPREL64MSB	= 0x2e,	/* word64 MSB		@gprel(S+A)	*/
    R_IA_64_GPREL64LSB	= 0x2f,	/* word64 LSB		@gprel(S+A)	*/

    /* unused:	0x30 .. 0x31						*/

    R_IA_64_LTOFF22	= 0x32,	/* instr: immediate22	@ltoff(S+A)	*/
    R_IA_64_LTOFF64I	= 0x33,	/* instr: immediate64	@ltoff(S+A)	*/

    /* unused:	0x34 .. 0x39						*/

    R_IA_64_PLTOFF22	= 0x3a,	/* instr: immediate22	@pltoff(S+A)	*/
    R_IA_64_PLTOFF64I	= 0x3b,	/* instr: immediate64	@pltoff(S+A)	*/

    /* unused:	0x3c .. 0x3d						*/

    R_IA_64_PLTOFF64MSB = 0x3e,	/* word64 MSB		@pltoff(S+A)	*/
    R_IA_64_PLTOFF64LSB	= 0x3f,	/* wordL4 MSB		@pltoff(S+A)	*/

    /* unused:	0x40 .. 0x42						*/

    R_IA_64_FPTR64I	= 0x43,	/* instr: immediate64	@fptr(S+A)	*/
    R_IA_64_FPTR32MSB	= 0x44,	/* word32 MSB		@fptr(S+A)	*/
    R_IA_64_FPTR32LSB	= 0x45,	/* word32 LSB		@fptr(S+A)	*/
    R_IA_64_FPTR64MSB	= 0x46,	/* word64 MSB		@fptr(S+A)	*/
    R_IA_64_FPTR64LSB	= 0x47,	/* word64 LSB		@fptr(S+A)	*/

    /* unused:	0x48 .. 0x48						*/

    R_IA_64_PCREL21B	= 0x49,	/* instr: imm21 (form1)	S+A-P		*/
    R_IA_64_PCREL21M	= 0x4a,	/* instr: imm21 (form2)	S+A-P		*/
    R_IA_64_PCREL21F	= 0x4b,	/* instr: imm21 (form3) S+A-P           */
    R_IA_64_PCREL32MSB	= 0x4c,	/* word32 MSB		S+A-P		*/
    R_IA_64_PCREL32LSB	= 0x4d,	/* word32 LSB		S+A-P		*/
    R_IA_64_PCREL64MSB	= 0x4e,	/* word64 MSB		S+A-P		*/
    R_IA_64_PCREL64LSB	= 0x4f,	/* word64 LSB		S+A-P		*/

    /* unused:	0x50 .. 0x51						*/

    R_IA_64_LTOFF_FPTR22 = 0x52,/* instr: immediate22	@ltoff(@fptr(S+A)) */
    R_IA_64_LTOFF_FPTR64I= 0x53,/* instr: immediate64	@ltoff(@fptr(S+A)) */

    /* unused:	0x54 .. 0x5b						*/

    R_IA_64_SEGREL32MSB	= 0x5c,	/* word32 MSB		@segrel(S+A)	*/
    R_IA_64_SEGREL32LSB	= 0x5d,	/* word32 LSB		@segrel(S+A)	*/
    R_IA_64_SEGREL64MSB	= 0x5e,	/* word64 MSB		@segrel(S+A)	*/
    R_IA_64_SEGREL64LSB	= 0x5f,	/* word64 LSB		@segrel(S+A)	*/

    /* unused:	0x60 .. 0x63						*/

    R_IA_64_SECREL32MSB = 0x64,	/* word32 MSB		@secrel(S+A)	*/
    R_IA_64_SECREL32LSB	= 0x65,	/* word32 LSB		@secrel(S+A)	*/
    R_IA_64_SECREL64MSB	= 0x66,	/* word64 MSB		@secrel(S+A)	*/
    R_IA_64_SECREL64LSB	= 0x67,	/* word64 LSB		@secrel(S+A)	*/

    /* unused:	0x68 .. 0x6b						*/

    R_IA_64_REL32MSB	= 0x6c,	/* word32 MSB		BD+C		*/
    R_IA_64_REL32LSB	= 0x6d,	/* word32 LSB		BD+C		*/
    R_IA_64_REL64MSB	= 0x6e,	/* word64 MSB		BD+C		*/
    R_IA_64_REL64LSB	= 0x6f,	/* word64 LSB		BD+C		*/
    R_IA_64_LTV32MSB	= 0x70,	/* word32 MSB		S+A [note 2]	*/
    R_IA_64_LTV32LSB	= 0x71,	/* word32 LSB		S+A [note 2]	*/
    R_IA_64_LTV64MSB	= 0x72,	/* word64 MSB		S+A [note 2]	*/
    R_IA_64_LTV64LSB	= 0x73,	/* word64 LSB		S+A [note 2]	*/

    /* unused:	0x74 .. 0x7f						*/

    R_IA_64_IPLTMSB	= 0x80,	/* func desc MSB	[note 3]	*/
    R_IA_64_IPLTLSB	= 0x81,	/* func desc LSB	[note 3]	*/

    /* unused:	0x82 .. 0xff						*/

    R_IA_64_END_	= 0x82	/* R_IA_64_END_ is not a relocation type.
				 * It marks the end of the list of types.
				 */
}; 

 
/* ====================================================================
 *
 * .IA64.plt
 *
 * sh_type:     SHT_IA64_PLT
 * sh_link:     ?
 * sh_info:     ?
 * attributes:  SHF_ALLOC, SHF_IA64_NOSTRIP
 *
 *
 * --- WARNING ---- WARNING ---- WARNING ---- WARNING ---- WARNING ----
 *
 *	TBD:  The IA64.plt section has not yet been defined.
 *
 * ====================================================================
 */
/* ====================================================================
 *
 * .IA64.content Section
 *
 * sh_type:     SHT_IA64_CONTENT
 * sh_link:     section header index of section classified
 * sh_info:     0
 * attributes:  SHF_ALLOC, SHF_IA64_NOSTRIP
 *
 *
 * --- WARNING ---- WARNING ---- WARNING ---- WARNING ---- WARNING ----
 *
 *	TBD:  The IA64.content section has not yet been defined.
 *	TBD:  Define Elf_IA64_Content_Kind
 *
 * ====================================================================
 */
#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef enum {
	CK_IA64_NULL	= 0x00		/* no valid information	*/
} Elf_IA64_Content_Kind;

#endif


/* ====================================================================
 *
 * .IA64.events Section
 *
 * sh_type:	SHT_IA64_EVENTS
 * sh_link:	section header index of section whose events tracked
 * sh_info:	section header index of associated interface section
 * attributes:	SHF_ALLOC, SHF_IRIX_NOSTRIP
 *
 *
 * --- WARNING ---- WARNING ---- WARNING ---- WARNING ---- WARNING ----
 *
 *	TBD:  The IA64.events section has not yet been defined.
 *	TBD:  define Elf_IA64_Event_Kind
 *
 * ====================================================================
 */
typedef enum {
	EK_IA64_NULL	= 0x00			/* no valid information	*/
} Elf_IA64_Event_Kind;

/* ====================================================================
 *
 * .dynamic Section
 *
 * --- WARNING ---- WARNING ---- WARNING ---- WARNING ---- WARNING ----
 *
 *      TBD:  The .dynamic section has not yet been prepared for
 *            use in both Irix/mips and Irix/ia64
 *
 *      I believe that most of the tags will be the same in both
 *      Irix/mips and Irix/ia64, but it still needs to be reviewed.
 * 
 *      For now, we'll document them only in mips/elf.h
 * ====================================================================
 */
 
#endif /* __SYS_IA64_ELF_H__ */

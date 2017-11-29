/*
 * Copyright 2005 PathScale, Inc.  All Rights Reserved.
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

#ifndef _ELF_STUFF_H

#define	_ELF_STUFF_H 1

#ifdef __MINGW32__
#ifndef	_SYS_CDEFS_H
#define _SYS_CDEFS_H
#ifdef	__cplusplus
#define	__BEGIN_DECLS	extern "C" {
#define	__END_DECLS	}
#else
#define	__BEGIN_DECLS
#define	__END_DECLS
#endif
#define __P(protos)     protos		/* full-blown ANSI C */
#endif
#else
#include <sys/cdefs.h>
#endif /* __MINGW32__ */

__BEGIN_DECLS

/* Standard ELF types. */

#include <elf.h>
#include <stdint.h>

#if defined(__MIPS_AND_IA64_ELF_H)
typedef unsigned char Elf32_Byte;
typedef unsigned char Elf64_Byte;
#endif

/* The ELF file header.  This appears at the start of every ELF file.  */

#define EI_NIDENT (16)

/* Fields in the e_ident array.  The EI_* macros are indices into the
   array.  The macros under each EI_* macro are the values the byte
   may have.  */

#define EI_MAG0		0		/* File identification byte 0 index */
#define ELFMAG0		0x7f		/* Magic number byte 0 */

#define EI_MAG1		1		/* File identification byte 1 index */
#define ELFMAG1		'E'		/* Magic number byte 1 */

#define EI_MAG2		2		/* File identification byte 2 index */
#define ELFMAG2		'L'		/* Magic number byte 2 */

#define EI_MAG3		3		/* File identification byte 3 index */
#define ELFMAG3		'F'		/* Magic number byte 3 */

/* Conglomeration of the identification bytes, for easy testing as a word.  */
#define	ELFMAG		"\177ELF"
#define	SELFMAG		4

#define IS_ELF(ehdr)	((ehdr).e_ident[EI_MAG0] == ELFMAG0 && \
			(ehdr).e_ident[EI_MAG1] == ELFMAG1 && \
			(ehdr).e_ident[EI_MAG2] == ELFMAG2 && \
			(ehdr).e_ident[EI_MAG3] == ELFMAG3)

#define EI_CLASS	4		/* File class byte index */
#define ELFCLASSNONE	0		/* Invalid class */
#define ELFCLASS32	1		/* 32-bit objects */
#define ELFCLASS64	2		/* 64-bit objects */

#define EI_DATA		5		/* Data encoding byte index */
#define ELFDATANONE	0		/* Invalid data encoding */
#define ELFDATA2LSB	1		/* 2's complement, little endian */
#define ELFDATA2MSB	2		/* 2's complement, big endian */

#define EI_VERSION	6		/* File version byte index */
					/* Value must be EV_CURRENT */

/* Legal values for e_type (object file type).  */

#define ET_NONE		0		/* No file type */
#define ET_REL		1		/* Relocatable file */
#define ET_EXEC		2		/* Executable file */
#define ET_DYN		3		/* Shared object file */
#define ET_CORE		4		/* Core file */
#define	ET_NUM		5		/* Number of defined types.  */
#define ET_LOPROC	0xff00		/* Processor-specific */
#define ET_HIPROC	0xffff		/* Processor-specific */

#if defined(__MIPS_AND_IA64_ELF_H)
#define ET_IR	(ET_LOPROC + 0)		/* file is a 32-bit or 64-bit WHIRL file*/
#endif

/* Legal values for e_machine (architecture).  */

#define EM_NONE		0		/* No machine */
#define EM_M32		1		/* AT&T WE 32100 */
#define EM_SPARC	2		/* SUN SPARC */
#define EM_386		3		/* Intel 80386 */
#define EM_68K		4		/* Motorola m68k family */
#define EM_88K		5		/* Motorola m88k family */
#define EM_486		6		/* Intel 80486 */
#define EM_860		7		/* Intel 80860 */
#define EM_MIPS		8		/* MIPS R3000 big-endian */
#define EM_S370		9		/* Amdahl */
#define EM_MIPS_RS4_BE 10		/* MIPS R4000 big-endian */

#if defined(__MIPS_AND_IA64_ELF_H)
/* NOTE THAT THIS VALUE CLASHES WITH AN OFFICIAL ONE JUST ABOVE. */
#define	EM_XIA64	10		/* Experimental IA64 objects
					 * used only by the simulator.
					 */
#define EM_IA_64	50		/* Intel IA64 */
#define EM_EAS2_3       0x0406
#endif

#define EM_SPARC64     11		/* SPARC v9 (not official) 64-bit */

#define EM_PARISC      15		/* HPPA */
#define EM_PPC         20		/* PowerPC */

/* If it is necessary to assign new unofficial EM_* values, please
   pick large random numbers (0x8523, 0xa7f2, etc.) to minimize the
   chances of collision with official or non-GNU unofficial values.  */

#define EM_ALPHA	0x9026

/* Legal values for e_version (version).  */

#define EV_NONE		0		/* Invalid ELF version */
#define EV_CURRENT	1		/* Current version */

/* Section header.  */

#if defined(__MIPS_AND_IA64_ELF_H)
/* Special section names */
#define ELF_BSS		".bss"
#define ELF_COMMENT	".comment"
#define ELF_DATA	".data"
#define ELF_DATA_PU	".data_pu"
#define ELF_DEBUG	".debug"
#define ELF_DYNAMIC	".dynamic"
#define ELF_DYNSTR	".dynstr"
#define ELF_DYNSYM	".dynsym"
#define ELF_MSYM	".msym"
#define ELF_FINI	".fini"
#define ELF_GOT		".got"
#define ELF_PLT		".plt"
#define ELF_HASH	".hash"
#define ELF_INIT	".init"
#define ELF_REL_DATA	".rel.data"
#define ELF_REL_FINI	".rel.fini"
#define ELF_REL_INIT	".rel.init"
#define ELF_REL_DYN	".rel.dyn"
#define ELF_REL_RODATA	".rel.rodata"
#define ELF_REL_TEXT	".rel.text"
#define ELF_RODATA	".rodata"
#define ELF_RODATA_PU	".rodata_pu"
#define ELF_SHSTRTAB	".shstrtab"
#define ELF_STRTAB	".strtab"
#define ELF_SYMTAB	".symtab"
#define ELF_TEXT	".text"
#ifdef KEY
#define ELF_TBSS        ".tbss"
#define ELF_TDATA       ".tdata"
#endif

/*
 * special section names
 *
 *	These are the section names for those sections
 *	that exist in both Irix/mips and Irix/ia64 objects.
 */
#define MIPS_LIBLIST            ".liblist"
#define MIPS_MSYM               ".msym"
#define MIPS_CONFLICT           ".conflict"
#define MIPS_SDATA              ".sdata"
#define MIPS_SDATA_PU           ".sdata_pu"
#define MIPS_REL_SDATA          ".rel.sdata"
#define MIPS_SRDATA             ".srdata"
#define MIPS_SRDATA_PU          ".srdata_pu"
#define MIPS_RDATA              ".rdata"
#define MIPS_SBSS               ".sbss"
#define MIPS_LIT4               ".lit4"
#define MIPS_LIT8               ".lit8"
#define MIPS_LIT16              ".lit16"
#define MIPS_EVENTS             ".MIPS.events"
#define MIPS_INTERFACES         ".MIPS.interfaces"
#define MIPS_OPTIONS            ".MIPS.options"
#define MIPS_SYMBOL_LIB         ".MIPS.symlib"
#define MIPS_DEBUG_INFO         ".debug_info"
#define MIPS_DEBUG_LINE         ".debug_line"
#define MIPS_DEBUG_ABBREV       ".debug_abbrev"
#define MIPS_DEBUG_FRAME        ".debug_frame"
#define MIPS_DEBUG_ARANGES      ".debug_aranges"
#define MIPS_DEBUG_PUBNAMES     ".debug_pubnames"
#define MIPS_DEBUG_STR          ".debug_str"
#define MIPS_DEBUG_FUNCNAMES    ".debug_funcnames"
#define MIPS_DEBUG_TYPENAMES    ".debug_typenames"
#define MIPS_DEBUG_VARNAMES     ".debug_varnames"
#define MIPS_DEBUG_WEAKNAMES    ".debug_weaknames"
#define MIPS_XLATE              ".MIPS.Xlate"
#define MIPS_XLATE_DEBUG	".MIPS.Xlate_debug"
#define MIPS_CONTENT		".MIPS.content"
#define MIPS_WHIRL              ".WHIRL"	/* Mips uses this name       */

#if defined(TARG_SL)
/* this name is what assembler need and will be replaced with above one */
#define MIPS_SSDATA              ".sbuf"


#define MIPS_VSDATA              ".vbuf"
#define MIPS_VS1DATA             ".vs1data"
#define MIPS_VS2DATA             ".vs2data"
#define MIPS_VS4DATA             ".vs4data"   
#endif 

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
 *	that are unique to Irix/ia64.
 */
#define IA64_ARCHEXT		".IA_64.archext"
#define IA64_PLTOFF		".IA_64.pltoff"
#define IA64_UNWIND		".IA_64.unwind"
#define IA64_UNWIND_INFO	".IA_64.unwind_info"

#define ELF64_FSZ_ADDR		8
#define ELF64_FSZ_HALF		2
#define ELF64_FSZ_OFF		8
#define ELF64_FSZ_SWORD		4
#define ELF64_FSZ_WORD		4
#define ELF64_FSZ_SXWORD	8
#define ELF64_FSZ_XWORD		8

#define ELF32_FSZ_ADDR	4
#define ELF32_FSZ_HALF	2
#define ELF32_FSZ_OFF	4
#define ELF32_FSZ_SWORD	4
#define ELF32_FSZ_WORD	4

/* Content kind -- valid for ELF-32 and ELF-64: */
typedef enum {
    CK_NULL	= 0,	    /* Invalid, same as EK_NULL */
    CK_DEFAULT	= 0x30,	    /* Default type of data for section */
    CK_ALIGN	= 0x31,	    /* Alignment for described range */
    CK_INSTR	= 0x32,	    /* Instructions */
    CK_DATA	= 0x33,	    /* Non-address data */
    CK_SADDR_32	= 0x34,	    /* Simple 32-bit addresses */
    CK_GADDR_32	= 0x35,	    /* GP-relative 32-bit addresses */
    CK_CADDR_32	= 0x36,	    /* Complex 32-bit addresses */
    CK_SADDR_64	= 0x37,	    /* Simple 64-bit addresses */
    CK_GADDR_64	= 0x38,	    /* GP-relative 64-bit addresses */
    CK_CADDR_64	= 0x39,	    /* Complex 64-bit addresses */
    CK_NO_XFORM	= 0x3a,	    /* No transformations allowed in this range */
    CK_NO_REORDER = 0x3b,   /* No reordering allowed in this range */
    CK_GP_GROUP = 0x3c,	    /* Text/data in range with length given by
			       second argument references GP group given
			       by first. */
    CK_STUBS	= 0x3d	    /* Text in range is stub code. ULEB */
} Elf_MIPS_Content_Kind;

/*
 * 	Elf_Options.kind		Options descriptor kinds
 */
#define ODK_NULL                 0      /* Undefined */
#define ODK_MIPS_REGINFO         1      /* MIPS Register usage information */
#define ODK_REGINFO         1      /* MIPS Register usage information */
#define ODK_MIPS_EXCEPTIONS      2      /* MIPS Exception processing options  */
#define ODK_EXCEPTIONS		 2      /* MIPS Exception processing options  */
#define ODK_PAD                  3      /* Section padding options */
#define ODK_MIPS_HWPATCH         4      /* MIPS Hardware workarounds performed*/
#define ODK_HWPATCH		 4      /* MIPS Hardware workarounds performed*/
#define ODK_FILL                 5      /* The fill value used by the linker */
#define ODK_TAGS                 6      /* Space for desktop tools to write */
#define ODK_MIPS_HWAND           7      /* HW workarounds.'AND' when merging */
#define ODK_HWAND		 7      /* HW workarounds.'AND' when merging */
#define ODK_MIPS_HWOR            8      /* HW workarounds.'OR'  when merging */
#define ODK_HWOR		 8      /* HW workarounds.'OR'  when merging */
#define ODK_GP_GROUP             9      /* GP group for text/data sections */
#define ODK_IDENT               10      /* ID information */
#define ODK_IA64_REGINFO        11      /* TO BE CHANGED! */
#define ODK_PAGESIZE            12      /* Alternate segment page size */


#define ODK_IA64_EXCEPTIONS     12      /* NOT USED! */
#define ODK_IA64_HWAND          13      /* NOT USED! */
#define ODK_IA64_HWOR           14      /* NOT USED! */

/*
 *	Elf_Options.kind == ODK_MIPS_EXCEPTIONS	
 *
 *      masks for Elf_Options.info
 */
#define OEX_PAGE0       0x10000 /* page zero must be mapped */
#define OEX_SMM         0x20000 /* Force sequential memory mode? */
#define OEX_FPDBUG      0x40000 /* Force floating point debug mode? */
#define OEX_PRECISEFP   OEX_FPDBUG
#define OEX_DISMISS     0x80000 /* Dismiss invalid address faults? */
#define OEX_FPU_MIN     0x1f    /* FPE's which MUST be enabled */
#define OEX_FPU_MAX     0x1f00  /* FPE's which MAY be enabled */
#define OEX_FPU_INVAL   0x10
#define OEX_FPU_DIV0    0x08
#define OEX_FPU_OFLO    0x04
#define OEX_FPU_UFLO    0x02
#define OEX_FPU_INEX    0x01

/*
 *	Elf_Options.kind == ODK_MIPS_HWPATCH
 *
 *      masks for Elf_Options.info
 */
#define OHW_R4KEOP      0x1     /* R4000 end-of-page patch */
#define OHW_R8KPFETCH   0x2     /* may need R8000 prefetch patch */
#define OHW_R5KEOP      0x4     /* R5000 end-of-page patch */
#define OHW_R5KCVTL     0x8     /* R5000 cvt.[ds].l bug.  clean=1 */
#define OHW_R10KLDL     0x10    /* R10000 requires LDL patch    */

/*
 *      Elf_Options.kind == ODK_MIPS_HWOR
 *
 *      masks for Elf_Options.info
 */
#define OHWO0_FIXADE	0x00000001	/* Object requires FIXADE call */

/*
 *      Elf_Options.kind == ODK_PAD
 *
 *      masks for Elf_Options.info
 */
#define OPAD_PREFIX	0x1	
#define OPAD_POSTFIX	0x2
#define OPAD_SYMBOL	0x4

/*
 *      Elf_Options.kind == ODK_GP_GROUP
 *
 *      masks for Elf_Options.info
 */
#define OGP_GROUP	0x0000ffff	/* GP group number */
#define OGP_SELF	0x00010000	/* Self-contained GP groups */
#endif

/* Special section indices.  */

#define SHN_UNDEF	0		/* Undefined section */
#define SHN_LORESERVE	0xff00		/* Start of reserved indices */
#define SHN_LOPROC	0xff00		/* Start of processor-specific */
#define SHN_HIPROC	0xff1f		/* End of processor-specific */
#define SHN_ABS		0xfff1		/* Associated symbol is absolute */
#define SHN_COMMON	0xfff2		/* Associated symbol is common */
#define SHN_HIRESERVE	0xffff		/* End of reserved indices */

/* Legal values for sh_type (section type).  */

#define SHT_NULL	0		/* Section header table entry unused */
#define SHT_PROGBITS	1		/* Program data */
#define SHT_SYMTAB	2		/* Symbol table */
#define SHT_STRTAB	3		/* String table */
#define SHT_RELA	4		/* Relocation entries with addends */
#define SHT_HASH	5		/* Symbol hash table */
#define SHT_DYNAMIC	6		/* Dynamic linking information */
#define SHT_NOTE	7		/* Notes */
#define SHT_NOBITS	8		/* Program space with no data (bss) */
#define SHT_REL		9		/* Relocation entries, no addends */
#define SHT_SHLIB	10		/* Reserved */
#define SHT_DYNSYM	11		/* Dynamic linker symbol table */
#define SHT_GNU_verdef	0x6ffffffd	/* Version definition section.  */
#define SHT_GNU_verneed	0x6ffffffe	/* Version needs section.  */
#define SHT_GNU_versym	0x6fffffff	/* Version symbol table.  */
#define SHT_HISUNW	0x6fffffff	/* Sun-specific high bound.  */
#define SHT_LOPROC	0x70000000	/* Start of processor-specific */
#define SHT_HIPROC	0x7fffffff	/* End of processor-specific */
#define SHT_LOUSER	0x80000000	/* Start of application-specific */
#define SHT_HIUSER	0x8fffffff	/* End of application-specific */

/* Legal values for sh_flags (section flags).  */

#define SHF_WRITE	(1 << 0)	/* Writable */
#define SHF_ALLOC	(1 << 1)	/* Occupies memory during execution */
#define SHF_EXECINSTR	(1 << 2)	/* Executable */
#ifndef SHF_TLS
#define SHF_TLS         (1 << 10)       /* Section hold thread-local data. */
#endif
#define SHF_MASKPROC	0xf0000000	/* Processor-specific */

/* Symbol table entry.  */

/* Special section index.  */

#define SHN_UNDEF	0		/* No section, undefined symbol.  */

/* How to extract and insert information held in the st_info field.  */

#define ELF32_ST_BIND(val)		(((unsigned char) (val)) >> 4)
#define ELF32_ST_TYPE(val)		((val) & 0xf)
#define ELF32_ST_INFO(bind, type)	(((bind) << 4) + ((type) & 0xf))

/* Both Elf32_Sym and Elf64_Sym use the same one-byte st_info field.  */
#define ELF64_ST_BIND(val)		ELF32_ST_BIND (val)
#define ELF64_ST_TYPE(val)		ELF32_ST_TYPE (val)
#define ELF64_ST_INFO(bind, type)	ELF32_ST_INFO ((bind), (type))

/* Legal values for ST_BIND subfield of st_info (symbol binding).  */

#define STB_LOCAL	0		/* Local symbol */
#define STB_GLOBAL	1		/* Global symbol */
#define STB_WEAK	2		/* Weak symbol */
#define	STB_NUM		3		/* Number of defined types.  */
#define STB_LOPROC	13		/* Start of processor-specific */
#define STB_HIPROC	15		/* End of processor-specific */

/* Legal values for ST_TYPE subfield of st_info (symbol type).  */

#define STT_NOTYPE	0		/* Symbol type is unspecified */
#define STT_OBJECT	1		/* Symbol is a data object */
#define STT_FUNC	2		/* Symbol is a code object */
#define STT_SECTION	3		/* Symbol associated with a section */
#define STT_FILE	4		/* Symbol's name is file name */
#define STT_LOPROC	13		/* Start of processor-specific */
#define STT_HIPROC	15		/* End of processor-specific */


/* Symbol table indices are found in the hash buckets and chain table
   of a symbol hash table section.  This special index value indicates
   the end of a chain, meaning no further symbols are found in that bucket.  */

#define STN_UNDEF	0		/* End of a chain.  */


/* Relocation table entry without addend (in section of type SHT_REL).
   These are the same size as Elf64_Rel and Elf64_Rela, but laid out
   a little differently. */

typedef struct
{
  Elf64_Addr	r_offset;
  Elf64_Byte	r_type;			/* 1st relocation op type */
  Elf64_Byte	r_type2;		/* 2nd relocation op type */
  Elf64_Byte	r_type3;		/* 3rd relocation op type */
  Elf64_Byte	r_ssym;			/* Special symbol */
  Elf64_Word	r_sym;			/* Symbol index */
} Elf64_AltRel;

typedef struct {
  Elf64_Addr	r_offset;
  Elf64_Byte	r_type;			/* 1st relocation op type */
  Elf64_Byte	r_type2;		/* 2nd relocation op type */
  Elf64_Byte	r_type3;		/* 3rd relocation op type */
  Elf64_Byte	r_ssym;			/* Special symbol */
  Elf64_Word	r_sym;			/* Symbol index */
  Elf64_Sxword	r_addend;
} Elf64_AltRela;

#if defined(__MIPS_AND_IA64_ELF_H)
/* Values for the r_ssym field: */
typedef enum {
    RSS_UNDEF	= 0,	/* Undefined */
    RSS_GP	= 1,	/* Context pointer (gp) value */
    RSS_GP0	= 2,	/* gp value used to create object being relocated */
    RSS_LOC	= 3	/* Address of location being relocated */
} Elf64_Rel_Ssym;
#else
/* I have seen two different definitions of the Elf64_Rel and
   Elf64_Rela structures, so we'll leave them out until Novell (or
   whoever) gets their act together.  */
/* The following, at least, is used on Sparc v9, MIPS, and Alpha.  */

typedef struct
{
  Elf64_Addr	r_offset;		/* Address */
  Elf64_Xword	r_info;			/* Relocation type and symbol index */
} Elf64_Rel;

typedef struct
{
  Elf64_Addr	r_offset;		/* Address */
  Elf64_Xword	r_info;			/* Relocation type and symbol index */
  Elf64_Sxword	r_addend;		/* Addend */
} Elf64_Rela;
#endif

/* How to extract and insert information held in the r_info field.  */

#define ELF32_R_SYM(val)		((val) >> 8)
#define ELF32_R_TYPE(val)		((val) & 0xff)
#define ELF32_R_INFO(sym, type)		(((sym) << 8) + ((type) & 0xff))

#define ELF64_R_SYM(i)			((i) >> 32)
#define ELF64_R_TYPE(i)			((i) & 0xffffffff)

/* Legal values for p_type (segment type).  */

#define	PT_NULL		0		/* Program header table entry unused */
#define PT_LOAD		1		/* Loadable program segment */
#define PT_DYNAMIC	2		/* Dynamic linking information */
#define PT_INTERP	3		/* Program interpreter */
#define PT_NOTE		4		/* Auxiliary information */
#define PT_SHLIB	5		/* Reserved */
#define PT_PHDR		6		/* Entry for header table itself */
#define PT_LOPROC	0x70000000	/* Start of processor-specific */
#define PT_HIPROC	0x7fffffff	/* End of processor-specific */

/* Legal values for p_flags (segment flags).  */

#define PF_X		(1 << 0)	/* Segment is executable */
#define PF_W		(1 << 1)	/* Segment is writable */
#define PF_R		(1 << 2)	/* Segment is readable */
#define PF_MASKPROC	0xf0000000	/* Processor-specific */

/* Legal values for note segment descriptor types for core files. */

#define NT_PRSTATUS	1		/* Contains copy of prstatus struct */
#define NT_FPREGSET	2		/* Contains copy of fpregset struct */
#define NT_PRPSINFO	3		/* Contains copy of prpsinfo struct */

/* Legal values for the  note segment descriptor types for object files.  */

#define NT_VERSION	1		/* Contains a version string.  */

/* Legal values for d_tag (dynamic entry type).  */

#define DT_NULL		0		/* Marks end of dynamic section */
#define DT_NEEDED	1		/* Name of needed library */
#define DT_PLTRELSZ	2		/* Size in bytes of PLT relocs */
#define DT_PLTGOT	3		/* Processor defined value */
#define DT_HASH		4		/* Address of symbol hash table */
#define DT_STRTAB	5		/* Address of string table */
#define DT_SYMTAB	6		/* Address of symbol table */
#define DT_RELA		7		/* Address of Rela relocs */
#define DT_RELASZ	8		/* Total size of Rela relocs */
#define DT_RELAENT	9		/* Size of one Rela reloc */
#define DT_STRSZ	10		/* Size of string table */
#define DT_SYMENT	11		/* Size of one symbol table entry */
#define DT_INIT		12		/* Address of init function */
#define DT_FINI		13		/* Address of termination function */
#define DT_SONAME	14		/* Name of shared object */
#define DT_RPATH	15		/* Library search path */
#define DT_SYMBOLIC	16		/* Start symbol search here */
#define DT_REL		17		/* Address of Rel relocs */
#define DT_RELSZ	18		/* Total size of Rel relocs */
#define DT_RELENT	19		/* Size of one Rel reloc */
#define DT_PLTREL	20		/* Type of reloc in PLT */
#define DT_DEBUG	21		/* For debugging; unspecified */
#define DT_TEXTREL	22		/* Reloc might modify .text */
#define DT_JMPREL	23		/* Address of PLT relocs */
#define DT_LOPROC	0x70000000	/* Start of processor-specific */
#define DT_HIPROC	0x7fffffff	/* End of processor-specific */
#define	DT_PROCNUM	DT_MIPS_NUM	/* Most used by any processor */

/* The versioning entry types.  The next are defined as part of the
   GNU extension.  */
#define DT_VERSYM	0x6ffffff0

/* These were chosen by Sun.  */
#define	DT_VERDEF	0x6ffffffc	/* Address of version definition
					   table */
#define	DT_VERDEFNUM	0x6ffffffd	/* Number of version definitions */
#define	DT_VERNEED	0x6ffffffe	/* Address of table with needed
					   versions */
#define	DT_VERNEEDNUM	0x6fffffff	/* Number of needed versions */
#define DT_VERSIONTAGIDX(tag)	(DT_VERNEEDNUM - (tag))	/* Reverse order! */
#define DT_VERSIONTAGNUM 16

/* Sun added these machine-independent extensions in the "processor-specific"
   range.  Be compatible.  */
#define DT_AUXILIARY    0x7ffffffd      /* Shared object to load before self */
#define DT_FILTER       0x7fffffff      /* Shared object to get values from */
#define DT_EXTRATAGIDX(tag)	((Elf32_Word)-((Elf32_Sword) (tag) <<1>>1)-1)
#define DT_EXTRANUM	3

/* Legal values for vd_version (version revision).  */
#define VER_DEF_NONE	0		/* No version */
#define VER_DEF_CURRENT	1		/* Current version */
#define VER_DEF_NUM	2		/* Given version number */

/* Legal values for vd_flags (version information flags).  */
#define VER_FLG_BASE	0x1		/* Version definition of file itself */
#define VER_FLG_WEAK	0x2		/* Weak version identifier */

/* Legal values for vn_version (version revision).  */
#define VER_NEED_NONE	 0		/* No version */
#define VER_NEED_CURRENT 1		/* Current version */
#define VER_NEED_NUM	 2		/* Given version number */

/* Legal values for vna_flags.  */
#define VER_FLG_WEAK	0x2		/* Weak verison identifier */

/* Legal values for a_type (entry type).  */

#define AT_NULL		0		/* End of vector */
#define AT_IGNORE	1		/* Entry should be ignored */
#define AT_EXECFD	2		/* File descriptor of program */
#define AT_PHDR		3		/* Program headers for program */
#define AT_PHENT	4		/* Size of program header entry */
#define AT_PHNUM	5		/* Number of program headers */
#define AT_PAGESZ	6		/* System page size */
#define AT_BASE		7		/* Base address of interpreter */
#define AT_FLAGS	8		/* Flags */
#define AT_ENTRY	9		/* Entry point of program */
#define AT_NOTELF	10		/* Program is not ELF */
#define AT_UID		11		/* Real uid */
#define AT_EUID		12		/* Effective uid */
#define AT_GID		13		/* Real gid */
#define AT_EGID		14		/* Effective gid */

/* Motorola 68k specific definitions.  */

/* m68k relocs.  */

#define R_68K_NONE	0		/* No reloc */
#define R_68K_32	1		/* Direct 32 bit  */
#define R_68K_16	2		/* Direct 16 bit  */
#define R_68K_8		3		/* Direct 8 bit  */
#define R_68K_PC32	4		/* PC relative 32 bit */
#define R_68K_PC16	5		/* PC relative 16 bit */
#define R_68K_PC8	6		/* PC relative 8 bit */
#define R_68K_GOT32	7		/* 32 bit PC relative GOT entry */
#define R_68K_GOT16	8		/* 16 bit PC relative GOT entry */
#define R_68K_GOT8	9		/* 8 bit PC relative GOT entry */
#define R_68K_GOT32O	10		/* 32 bit GOT offset */
#define R_68K_GOT16O	11		/* 16 bit GOT offset */
#define R_68K_GOT8O	12		/* 8 bit GOT offset */
#define R_68K_PLT32	13		/* 32 bit PC relative PLT address */
#define R_68K_PLT16	14		/* 16 bit PC relative PLT address */
#define R_68K_PLT8	15		/* 8 bit PC relative PLT address */
#define R_68K_PLT32O	16		/* 32 bit PLT offset */
#define R_68K_PLT16O	17		/* 16 bit PLT offset */
#define R_68K_PLT8O	18		/* 8 bit PLT offset */
#define R_68K_COPY	19		/* Copy symbol at runtime */
#define R_68K_GLOB_DAT	20		/* Create GOT entry */
#define R_68K_JMP_SLOT	21		/* Create PLT entry */
#define R_68K_RELATIVE	22		/* Adjust by program base */
#define R_68K_NUM	23

/* Intel 80386 specific definitions.  */

/* i386 relocs.  */

#define R_386_NONE	0		/* No reloc */
#define R_386_32	1		/* Direct 32 bit  */
#define R_386_PC32	2		/* PC relative 32 bit */
#define R_386_GOT32	3		/* 32 bit GOT entry */
#define R_386_PLT32	4		/* 32 bit PLT address */
#define R_386_COPY	5		/* Copy symbol at runtime */
#define R_386_GLOB_DAT	6		/* Create GOT entry */
#define R_386_JMP_SLOT	7		/* Create PLT entry */
#define R_386_RELATIVE	8		/* Adjust by program base */
#define R_386_GOTOFF	9		/* 32 bit offset to GOT */
#define R_386_GOTPC	10		/* 32 bit PC relative offset to GOT */

/* Intel IA64 specific definitions. */
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

/* SUN SPARC specific definitions.  */

/* SPARC relocs.  */

#define R_SPARC_NONE	0		/* No reloc */
#define R_SPARC_8	1		/* Direct 8 bit */
#define R_SPARC_16	2		/* Direct 16 bit */
#define R_SPARC_32	3		/* Direct 32 bit */
#define R_SPARC_DISP8	4		/* PC relative 8 bit */
#define R_SPARC_DISP16	5		/* PC relative 16 bit */
#define R_SPARC_DISP32	6		/* PC relative 32 bit */
#define R_SPARC_WDISP30	7		/* PC relative 30 bit shifted */
#define R_SPARC_WDISP22	8		/* PC relative 22 bit shifted */
#define R_SPARC_HI22	9		/* High 22 bit */
#define R_SPARC_22	10		/* Direct 22 bit */
#define R_SPARC_13	11		/* Direct 13 bit */
#define R_SPARC_LO10	12		/* Truncated 10 bit */
#define R_SPARC_GOT10	13		/* Truncated 10 bit GOT entry */
#define R_SPARC_GOT13	14		/* 13 bit GOT entry */
#define R_SPARC_GOT22	15		/* 22 bit GOT entry shifted */
#define R_SPARC_PC10	16		/* PC relative 10 bit truncated */
#define R_SPARC_PC22	17		/* PC relative 22 bit shifted */
#define R_SPARC_WPLT30	18		/* 30 bit PC relative PLT address */
#define R_SPARC_COPY	19		/* Copy symbol at runtime */
#define R_SPARC_GLOB_DAT 20		/* Create GOT entry */
#define R_SPARC_JMP_SLOT 21		/* Create PLT entry */
#define R_SPARC_RELATIVE 22		/* Adjust by program base */
#define R_SPARC_UA32	23		/* Direct 32 bit unaligned */

/* MIPS R3000 specific definitions.  */

/* Legal values for e_flags field of Elf32_Ehdr.  */

#define EF_MIPS_NOREORDER 1		/* A .noreorder directive was used */
#define EF_MIPS_PIC	  2		/* Contains PIC code */
#define EF_MIPS_CPIC	  4		/* Uses PIC calling sequence */
#define EF_MIPS_ARCH	  0xf0000000	/* MIPS architecture level */

#if defined(__MIPS_AND_IA64_ELF_H)
/*
 *  Temporary version number for formats prior to formal release.
 *  e_ident[EI_TVERSION]
 */
#define EI_TVERSION 15
#define EV_T_CURRENT 1

#define	EF_IRIX_ABI64		0x00000010	
	/* If the bit is set   (1) then this is a 64bit ABI file
	 * If the bit is clear (0) then this is a 32bit ABI file.
	 *
	 * If set, this is a 64bit ABI elf file using a LP64 data model.
	 * And if this is a WHIRL file, (ie. e_type == ET_IR) then this 
	 * holds 64bit-format WHIRL code.
	 *
	 * Similarly, if not set, this is a 32bit ABI elf file using a ILP32 
	 * data model.  And if it is a WHIRL file, (ie. e_type == ET_IR) this 
	 * holds 32bit format WHIRL code.
	 *
	 * NOTE: In the past, we implied 32bit ABI vs. 64bit ABI from
	 * the value of the ELFCLASS32 or ELFCLASS64.  
	 */
#define EF_MIPS_OPSEX		EF_MIPS_NOREORDER
#define EF_MIPS_OPTIONS_FIRST	0x00000080 
/* obsolete names */
#define EF_MIPS_UGEN_ALLOC	EF_MIPS_XGOT
#define EF_MIPS_UGEN_RESERVED	EF_MIPS_64BIT_WHIRL

#define EF_MIPS_ABI64		EF_IRIX_ABI64
	/* see explanation above for EF_IRIX_ABI64 */

/*
 *	The EF_MIPS_ARCH field of e_flags describes the ISA of the object.
 *		size:	4 bits
 *		type:	int
 */
#define EF_MIPS_ARCH		0xf0000000	/* mask: 4 bit field */
#define EF_MIPS_ARCH_1		0x00000000
#define EF_MIPS_ARCH_2          0x10000000
#define EF_MIPS_ARCH_3          0x20000000
#define EF_MIPS_ARCH_4          0x30000000
#define EF_MIPS_ARCH_5          0x40000000
#define EF_MIPS_ARCH_6          0x50000000

/*
 *	The EF_MIPS_ARCH_ASE field of e_flags describes the set of 
 *	Application Specific Extensions used by the object.
 *		size:	4 bits
 *		type:	bit-field
 */
#define EF_MIPS_ARCH_ASE	0x0f000000	/* mask: 4 bit field	*/
#define EF_MIPS_ARCH_ASE_MDMX	0x08000000	/* multi-media extensions*/
#define EF_MIPS_ARCH_ASE_M16	0x04000000	/* MIPS16 isa extensions */

/*
 *	Please reserve these 8 bits of e_flags for future
 *	expansion of the EF_MIPS_ARCH_ASE field;  increasing
 *	the field from 4 bits to 12 bits.
 *	
 *		0x00ff0000	
 *	
 *	If and when we expand it, we'll redefine the EF_MIPS_ARCH_ASE 
 *	macro to be:
 *	
 *		0x0fff0000.
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
 * Random constants
 */

#define _TEXT_ALIGN 0x10000
#define _DATA_ALIGN 0x10000
#define ELF_IRIX_MAXPGSZ (64*1024)
#define ELF_IRIX_MINPGSZ (0x1000)

#define ELF_IA64_MAXPGSZ ELF_IRIX_MAXPGSZ
#define ELF_IA64_MINPGSZ ELF_IRIX_MINPGSZ

#define ELF_MIPS_MAXPGSZ ELF_IRIX_MAXPGSZ
#define ELF_MIPS_MINPGSZ ELF_IRIX_MINPGSZ
#endif

/* Legal values for MIPS architecture level.  */

#define E_MIPS_ARCH_1	  0x00000000	/* -mips1 code.  */
#define E_MIPS_ARCH_2	  0x10000000	/* -mips2 code.  */
#define E_MIPS_ARCH_3	  0x20000000	/* -mips3 code.  */

/* 
 * special p_flags
 */

#define PF_MIPS_LOCAL		0x10000000

/* Special section indices.  */

#define SHN_MIPS_ACOMMON 0xff00		/* Allocated common symbols */
#define SHN_MIPS_TEXT	 0xff01		/* Allocated test symbols.  */
#define SHN_MIPS_DATA	 0xff02		/* Allocated data symbols.  */
#define SHN_MIPS_SCOMMON 0xff03		/* Small common symbols */
#define SHN_MIPS_SUNDEFINED 0xff04	/* Small undefined symbols */

/* Legal values for sh_type field of Elf32_Shdr.  */

#define SHT_MIPS_LIBLIST  0x70000000	/* Shared objects used in link */
#define SHT_MIPS_CONFLICT 0x70000002	/* Conflicting symbols */
#define SHT_MIPS_GPTAB	  0x70000003	/* Global data area sizes */
#define SHT_MIPS_UCODE	  0x70000004	/* Reserved for SGI/MIPS compilers */
#define SHT_MIPS_DEBUG	  0x70000005	/* MIPS ECOFF debugging information */
#define SHT_MIPS_REGINFO  0x70000006	/* Register usage information */
#define SHT_MIPS_OPTIONS  0x7000000d	/* Miscellaneous options.  */
#define SHT_MIPS_DWARF    0x7000001e	/* DWARF debugging information.  */
#define SHT_MIPS_EVENTS	  0x70000021	/* Event section.  */
#define SHT_MIPS_XLATE    0x70000024
#define SHT_MIPS_XLATE_DEBUG 0x70000025
#define SHT_MIPS_WHIRL	     0x70000026
#define SHT_MIPS_XLATE_OLD   0x70000028

/* Legal values for sh_flags field of Elf32_Shdr.  */

#define SHF_MIPS_GPREL	0x10000000	/* Must be part of global data area */

#if defined(__MIPS_AND_IA64_ELF_H)
#define SHT_IRIX_LIBLIST	(SHT_LOPROC + 0)
#define SHT_IRIX_MSYM		(SHT_LOPROC + 1)
#define SHT_IRIX_CONFLICT	(SHT_LOPROC + 2)
#define SHT_IRIX_GPTAB		(SHT_LOPROC + 3)
#define SHT_IRIX_UCODE		(SHT_LOPROC + 4)
#define SHT_IRIX_DEBUG          (SHT_LOPROC + 5)
#define SHT_IRIX_REGINFO        (SHT_LOPROC + 6)

#ifdef __osf__
#define	SHT_IRIX_PACKAGE	(SHT_LOPROC + 7)
#define	SHT_IRIX_PACKSYM	(SHT_LOPROC + 8)
#endif /* __osf__ */

#define SHT_IRIX_RELD		(SHT_LOPROC + 9)
#define SHT_IRIX_DONTUSE	(SHT_LOPROC + 10)

#define SHT_IRIX_IFACE		(SHT_LOPROC + 11)
#define SHT_IRIX_CONTENT	(SHT_LOPROC + 12)
#define SHT_IRIX_OPTIONS	(SHT_LOPROC + 13)

#define SHT_IRIX_SHDR		(SHT_LOPROC + 16)
#define SHT_IRIX_FDESC		(SHT_LOPROC + 17)
#define SHT_IRIX_EXTSYM		(SHT_LOPROC + 18)
#define SHT_IRIX_DENSE		(SHT_LOPROC + 19)
#define SHT_IRIX_PDESC		(SHT_LOPROC + 20)
#define SHT_IRIX_LOCSYM		(SHT_LOPROC + 21)
#define SHT_IRIX_AUXSYM		(SHT_LOPROC + 22)
#define SHT_IRIX_OPTSYM		(SHT_LOPROC + 23)
#define SHT_IRIX_LOCSTR		(SHT_LOPROC + 24)
#define SHT_IRIX_LINE		(SHT_LOPROC + 25)
#define SHT_IRIX_RFDESC		(SHT_LOPROC + 26)

#define SHT_IRIX_DELTASYM	(SHT_LOPROC + 27)
#define SHT_IRIX_DELTAINST	(SHT_LOPROC + 28)
#define SHT_IRIX_DELTACLASS	(SHT_LOPROC + 29)

#define SHT_IRIX_DWARF		(SHT_LOPROC + 30)
#define SHT_IRIX_DELTADECL	(SHT_LOPROC + 31)
#define SHT_IRIX_SYMBOL_LIB	(SHT_LOPROC + 32)
#define SHT_IRIX_EVENTS        	(SHT_LOPROC + 33)
#define SHT_IRIX_TRANSLATE     	(SHT_LOPROC + 34)
#define SHT_IRIX_PIXIE     	(SHT_LOPROC + 35)
#define SHT_IRIX_XLATE		(SHT_LOPROC + 36)
#define SHT_IRIX_XLATE_DEBUG	(SHT_LOPROC + 37)
#define SHT_IRIX_WHIRL		(SHT_LOPROC + 38)
#define SHT_IRIX_EH_REGION	(SHT_LOPROC + 39)
#define SHT_IRIX_XLATE_OLD	(SHT_LOPROC + 40)
#define SHT_IRIX_PDR_EXCEPTION	(SHT_LOPROC + 41)

/* The next five section types are valid only on irix/ia64  */
#define SHT_IRIX_EXT            (SHT_LOPROC + 42)
#define SHT_IRIX_UNWIND         (SHT_LOPROC + 43)
#define SHT_IRIX_UNWIND_INFO    (SHT_LOPROC + 44)
#define SHT_IRIX_PLT            (SHT_LOPROC + 45)
#define SHT_IRIX_PLTOFF         (SHT_LOPROC + 46)
#define SHT_IA64_UNWIND         SHT_IRIX_UNWIND         
#define SHT_IA64_UNWIND_INFO    SHT_IRIX_UNWIND_INFO    
#define SHT_IA64_OPTIONS        SHT_IRIX_OPTIONS


#define SHT_IRIX_NUM		47
	/* SHT_IRIX_NUM is one more than the highest offset to SHT_LOCPROC;
	 * it is the number of extensions, but is *NOT* a section type.
	 */

#define SHT_MIPS_DONTUSE	SHT_IRIX_DONTUSE

/*
 * sh_flags
 *
 *	We'll try and use the same flags for both irix/mips and irix/ia64.
 *	If this ever becomes a problem, we'll have to split them into
 *	two different namespaces.
 */
#define SHF_IRIX_GPREL          0x10000000
#define SHF_IRIX_MERGE          0x20000000
#define SHF_IRIX_ADDR           0x40000000	/* Reserved for future	*/
#define SHF_IRIX_STRINGS        0x80000000	/* Reserved for future	*/
#define SHF_IRIX_NOSTRIP        0x08000000
#define SHF_IRIX_LOCAL          0x04000000
#define SHF_IRIX_NAMES          0x02000000	/* Reserved for future	*/
#define SHF_IRIX_NODUPE         0x01000000	/* Reserved for future	*/

/* ====================================================================
 *
 * Symbol table
 *
 * ====================================================================
 */

/*
 * Special Irix st_other
 */
#define STO_DEFAULT		0x0
#define STO_INTERNAL		0x1
#define STO_HIDDEN		0x2
#define STO_PROTECTED		0x3
#define STO_OPTIONAL		0x4
#define STO_SC_ALIGN_UNUSED	0xff	/* No longer used 		*/

/*
 * Special Irix st_info
 */
#define STB_SPLIT_COMMON	(STB_LOPROC+0)

/* ====================================================================
 *
 * Section Headers
 *
 * ====================================================================
 */

/* 
 * Special Irix section indices
 */

#define SHN_IRIX_ACOMMON	(SHN_LOPROC + 0)
#define SHN_IRIX_TEXT		(SHN_LOPROC + 1)
#define SHN_IRIX_DATA		(SHN_LOPROC + 2)
#define SHN_IRIX_SCOMMON	(SHN_LOPROC + 3)
#define SHN_IRIX_SUNDEFINED	(SHN_LOPROC + 4)
#define SHN_IRIX_LCOMMON	(SHN_LOPROC + 5)
#define SHN_IRIX_LUNDEFINED	(SHN_LOPROC + 6)

/* 
 * Special mips section indices
 */

#define SHN_MIPS_LCOMMON	SHN_IRIX_LCOMMON
#define SHN_MIPS_LUNDEFINED	SHN_IRIX_LUNDEFINED


/* Event kind -- valid for ELF-32 and ELF-64: */
typedef enum {
    EK_NULL = 0x00,	    /* No valid information */
    EK_ADDR_RESET = 0x01,   /* Reset offset into associated text section */
    EK_INCR_LOC_EXT = 0x02, /* Increment offset into associated text section */
    EK_ENTRY = 0x03,	    /* Subprogram entrypoint */
    EK_IF_ENTRY = 0x04,	    /* Subprogram entrypoint with associated interface offset */
    EK_EXIT = 0x05,	    /* Subprogram exit */
    EK_PEND = 0x06,	    /* Subprogram end (last instruction) */

    EK_SWITCH_32 = 0x7,	    /* jr for switch stmt, table entries are 32bit */
    EK_SWITCH_64 = 0x8,	    /* jr for switch stmt, table entries are 64bit */
    EK_DUMMY = 0x09,	    /* empty slot */

    EK_BB_START = 0x0a,	    /* Basic block beginning */
    EK_INCR_LOC_UNALIGNED = 0x0b,    /* Increment unaligned byte offset */
    EK_GP_PROLOG_HI = 0x0c, /* Establish high 16bits of GP */
    EK_GP_PROLOG_LO = 0x0d, /* Establish low 16bits of GP */
    EK_GOT_PAGE = 0x0e,	    /* Compact relocation: GOT page pointer */
    EK_GOT_OFST = 0x0f,     /* Compact relocation: GOT page offset */
    EK_HI = 0x10,	    /* Compact relocation: high 16bits of abs. addr */
    EK_LO = 0x11,	    /* Compact relocation: low 16bits of abs. addr */
    EK_64_HIGHEST = 0x12,   /* Compact relocation: most significant 16 bits
			       of a 64bit absolute address */
    EK_64_HIGHER = 0x13,    /* Compact relocation: second most significant
			       16 bits of a 64bit absolute address */
    EK_64_HIGH = 0x14,	    /* Compact relocation: third most significant
			       16 bits of a 64bit absolute address */
    EK_64_LOW = 0x15,       /* Compact relocation: least significant 16 bits
			       of a 64bit absolute address */
    EK_GPREL = 0x16,        /* Compact relocation: GP relative reference */

    EK_DEF = 0x17,	    /* Define new event kind format */

    EK_FCALL_LOCAL = 0x18,	/* point-of-call (jalr) to a local procedure */
    EK_FCALL_EXTERN = 0x19,	/* jalr to extern procedure (small got case) */
    EK_FCALL_EXTERN_BIG = 0x1a,	/* jalr to extern procedure (large got case) */
    EK_FCALL_MULT = 0x1b,	/* jalr to more than one procedure */
    EK_FCALL_MULT_PARTIAL = 0x1c, /* jalr to multiple + unknown procedures */

    EK_LTR_FCALL = 0x1d,	/* jalr to rld lazy-text res.  index of
				   symbol associated. */
    EK_PCREL_GOT0 = 0x1e, 	/* immediate is hi 16 bits of 32-bit
				   constant.  Argument is offset to lo,
				   in instructions, not bytes*/

    /* The following events are reserved for supporting Purify-type tools: */
    EK_MEM_COPY_LOAD = 0x1f,    /* load only for copying data */
    EK_MEM_COPY_STORE = 0x20,   /* store only for copying data --
                                   LEB128 operand is word offset to
                                   paired load */
    EK_MEM_PARTIAL_LOAD = 0x21, /* load for reference to a subset of bytes --
                                   BYTE operand's 8 bits indicate which
                                   bytes are actually used */
    EK_MEM_EAGER_LOAD = 0x22,   /* load is speculative */
    EK_MEM_VALID_LOAD = 0x23,   /* load of data known to be valid */

				  
		/*
		 * Yet to be defined kinds with no fields (like EK_EXIT)
		 */
    EK_CK_UNUSED_NONE_0 = 0x50, /*  */
    EK_CK_UNUSED_NONE_1 = 0x51, /*  */
    EK_CK_UNUSED_NONE_2 = 0x52, /*  */
    EK_CK_UNUSED_NONE_3 = 0x53, /*  */
    EK_CK_UNUSED_NONE_4 = 0x54, /*  */

		/*
		 * Yet to be defined kinds with 1 16 bit field
		 */
    EK_CK_UNUSED_16BIT_0 = 0x55,
    EK_CK_UNUSED_16BIT_1 = 0x56,
    EK_CK_UNUSED_16BIT_2 = 0x57, /*  */
    EK_CK_UNUSED_16BIT_3 = 0x58, /*  */
    EK_CK_UNUSED_16BIT_4 = 0x59, /*  */

		/*
		 * Yet to be defined kinds with 1 32 bit field
		 */
    EK_CK_UNUSED_32BIT_0 = 0x5a, /*  */
    EK_CK_UNUSED_32BIT_1 = 0x5b, /*  */
    EK_CK_UNUSED_32BIT_2 = 0x5c, /*  */

		/*
		 * Yet to be defined kinds with 1 64 bit field
		 */

    EK_CK_UNUSED_64BIT_0 = 0x5d,
    EK_CK_UNUSED_64BIT_1 = 0x5e,
    EK_CK_UNUSED_64BIT_2 = 0x5f, /*  */
    EK_CK_UNUSED_64BIT_3 = 0x60, /*  */
    EK_CK_UNUSED_64BIT_4 = 0x61, /*  */

		/*
		 * Yet to be defined kinds with 1 uleb128 field
		 */
    EK_CK_UNUSED_ULEB128_0 = 0x62, /* */
    EK_CK_UNUSED_ULEB128_1 = 0x63, /*  */
    EK_CK_UNUSED_ULEB128_2 = 0x64, /*  */
    EK_CK_UNUSED_ULEB128_3 = 0x65, /*  */
    EK_CK_UNUSED_ULEB128_4 = 0x66, /*  */
    EK_CK_UNUSED_ULEB128_5 = 0x67, /*  */
    EK_CK_UNUSED_ULEB128_6 = 0x68, /*  */
    EK_CK_UNUSED_ULEB128_7 = 0x69, /*  */
    EK_CK_UNUSED_ULEB128_8 = 0x6a, /*  */
    EK_CK_UNUSED_ULEB128_9 = 0x6b, /*  */


    EK_INCR_LOC = 0x80	    /* Increment offset into associated text section */

} Elf_MIPS_Event_Kind;
typedef	Elf_MIPS_Event_Kind Elf_Event_Kind;

/* The following defines list the various types of operands that are 
 * supported with the EK_DEF event kind.
 */
#define EK_DEF_UCHAR	(1)	    /* unsigned char (8 bits) */
#define EK_DEF_USHORT	(2)	    /* unsigned short (16 bits) */
#define EK_DEF_UINT	(3)	    /* unsigned int (32 bits) */
#define EK_DEF_ULONG	(4)	    /* unsigned long (64 bits) */
#define EK_DEF_ULEB128	(5)	    /* unsigned LEB128 encoded number */
#define EK_DEF_CHAR	(6)	    /* signed char (8 bits) */
#define EK_DEF_SHORT	(7)	    /* signed short (16 bits) */
#define EK_DEF_INT	(8)	    /* signed int (32 bits) */
#define EK_DEF_LONG	(9)	    /* signed long (64 bits) */
#define EK_DEF_LEB128	(10)	    /* signed LEB128 encoded number */
#define EK_DEF_STRING	(11)	    /* null terminated string */
#define EK_DEF_VAR	(12)	    /* variable length field: the first 2
				       bytes is an unsigned short
				       specifying the total number of bytes
				       of this field including the first 2
				       bytes */
#define CK_DEF EK_DEF

#define R_MIPS_GPREL		7

typedef struct {
    Elf64_Word symbol;		/* symbol table index of subprogram, or 0 */
    Elf64_Half attrs;		/* Attributes: See list below */
    Elf64_Byte pcnt;		/* Parameter count */
    Elf64_Byte fpmask;		/* bit on indicates an FP parameter register */
} Elf_Ifd;

typedef Elf_Ifd Elf_Interface_Descriptor;  /* for compatibility */

/* Flags that can be set in the 'attrs' field of Elf_Interface_Descriptor */
#define SA_PROTOTYPED	0x8000	/* Does def or ref have prototype ? */
#define SA_VARARGS	0x4000	/* Is this a varargs subprogram ? */
#define SA_PIC		0x2000	/* Are memory references PIC? */
#define SA_DSO_ENTRY	0x1000	/* Is subprogram valid DSO entry? */
#define SA_ADDRESSED	0x0800	/* Is subprogram address taken? */
#define SA_FUNCTION	0x0400	/* Does subprogram return a result? */
#define SA_NESTED	0x0200	/* Is subprogram nested? */
#define SA_IGNORE_ERROR	0x0100	/* Ignore consistency errors? */
#define SA_DEFINITION	0x0080	/* Is this a definition (no just call)? */
#define SA_AT_FREE	0x0040	/* Is the at register free at all branches? */
#define SA_FREE_REGS	0x0020	/* Free register mask precedes parm profile */
#define SA_PARAMETERS	0x0010	/* Parameter profile follows descriptor? */
#define SA_ALTINTERFACE 0x0008	/* Alternate descriptor follows? */

/* Fundamental Parameter Types */
#define FT_unknown         0x0000
#define FT_signed_char     0x0001
#define FT_unsigned_char   0x0002
#define FT_signed_short    0x0003
#define FT_unsigned_short  0x0004
#define FT_signed_int32    0x0005
#define FT_unsigned_int32  0x0006
#define FT_signed_int64    0x0007
#define FT_unsigned_int64  0x0008
#define FT_pointer32       0x0009
#define FT_pointer64       0x000a
#define FT_float32         0x000b
#define FT_float64         0x000c
#define FT_float128        0x000d
#define FT_complex64       0x000e
#define FT_complex128      0x000f
#define FT_complex256      0x0010
#define FT_void            0x0011
#define FT_bool32          0x0012
#define FT_bool64          0x0013
#define FT_label32         0x0014
#define FT_label64         0x0015
#define FT_struct          0x0020
#define FT_union           0x0021
#define FT_enum            0x0022
#define FT_typedef         0x0023
#define FT_set             0x0024
#define FT_range           0x0025
#define FT_member_ptr      0x0026
#define FT_virtual_ptr     0x0027
#define FT_class           0x0028

/* Parameter Qualifiers (aka Modifiers)  */
#define MOD_pointer_to     0x01
#define MOD_reference_to   0x02
#define MOD_const          0x03
#define MOD_volatile       0x04
#define MOD_function       0x80
#define MOD_array_of       0x81

/* Parameter descriptor masks */
#define PDM_TYPE	0x00ff	/* Fundamental type of parameter */
#define PDM_REFERENCE	0x4000	/* Reference parameter ? */
#define PDM_SIZE	0x2000	/* Followed by explicit 32-bit byte count? */
#define PDM_Qualifiers	0x0f00	/* Count of type qualifiers << 8 */

/* Parameter descriptor mask flags */
#define PDMF_REFERENCE  0x40
#define PDMF_SIZE       0x20
#define PDMF_Qualifiers 0x0f
#endif

/* MIPS relocs.  */

#define R_MIPS_NONE	0		/* No reloc */
#define R_MIPS_16	1		/* Direct 16 bit */
#define R_MIPS_32	2		/* Direct 32 bit */
#define R_MIPS_REL32	3		/* PC relative 32 bit */
#define R_MIPS_26	4		/* Direct 26 bit shifted */
#define R_MIPS_HI16	5		/* High 16 bit */
#define R_MIPS_LO16	6		/* Low 16 bit */
#define R_MIPS_GPREL16	7		/* GP relative 16 bit */
#define R_MIPS_LITERAL	8		/* 16 bit literal entry */
#define R_MIPS_GOT16	9		/* 16 bit GOT entry */
#define R_MIPS_PC16	10		/* PC relative 16 bit */
#define R_MIPS_CALL16	11		/* 16 bit GOT entry for function */
#define R_MIPS_GPREL32	12		/* GP relative 32 bit */
#define R_MIPS_SHIFT5           16
#define R_MIPS_SHIFT6           17
#define R_MIPS_64               18
#define R_MIPS_GOT_DISP         19
#define R_MIPS_GOT_PAGE         20
#define R_MIPS_GOT_OFST         21
#define R_MIPS_GOT_HI16         22
#define R_MIPS_GOT_LO16         23
#define R_MIPS_SUB              24
#define R_MIPS_INSERT_A         25
#define R_MIPS_INSERT_B         26
#define R_MIPS_DELETE           27
#define R_MIPS_HIGHER           28
#define R_MIPS_HIGHEST          29
#define R_MIPS_CALL_HI16        30
#define R_MIPS_CALL_LO16        31
#define R_MIPS_SCN_DISP         32
#define R_MIPS_REL16            33
#define R_MIPS_ADD_IMMEDIATE    34
#define R_MIPS_PJUMP            35
#define R_MIPS_RELGOT           36
#define R_MIPS_JALR             37


#define _R_MIPS_COUNT_		38	/* Number of relocations */
	/* _R_MIPS_COUNT_ is not a relocation type, it is
	** a count of relocation types. 
        ** Must be one greater than the highest relocation
        ** type.
	*/

/* Legal values for p_type field of Elf32_Phdr.  */

#define PT_MIPS_REGINFO	0x70000000	/* Register usage information */

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

/* Legal values for d_tag field of Elf32_Dyn.  */

#define DT_MIPS_RLD_VERSION  0x70000001	/* Runtime linker interface version */
#define DT_MIPS_TIME_STAMP   0x70000002	/* Timestamp */
#define DT_MIPS_ICHECKSUM    0x70000003	/* Checksum */
#define DT_MIPS_IVERSION     0x70000004	/* Version string (string tbl index) */
#define DT_MIPS_FLAGS	     0x70000005	/* Flags */
#define DT_MIPS_BASE_ADDRESS 0x70000006	/* Base address */
#define DT_MIPS_MSYM            0x70000007
#define DT_MIPS_CONFLICT     0x70000008	/* Address of CONFLICT section */
#define DT_MIPS_LIBLIST	     0x70000009	/* Address of LIBLIST section */
#define DT_MIPS_LOCAL_GOTNO  0x7000000a	/* Number of local GOT entries */
#define DT_MIPS_CONFLICTNO   0x7000000b	/* Number of CONFLICT entries */
#define DT_MIPS_LIBLISTNO    0x70000010	/* Number of LIBLIST entries */
#define DT_MIPS_SYMTABNO     0x70000011	/* Number of DYNSYM entries */
#define DT_MIPS_UNREFEXTNO   0x70000012	/* First external DYNSYM */
#define DT_MIPS_GOTSYM	     0x70000013	/* First GOT entry in DYNSYM */
#define DT_MIPS_HIPAGENO     0x70000014	/* Number of GOT page table entries */
#define DT_MIPS_RLD_MAP	     0x70000016	/* Address of run time loader map.  */

/* Legal values for DT_MIPS_FLAG Elf32_Dyn entry.  */

#define RHF_NONE		   0		/* No flags */
#define RHF_QUICKSTART		   (1 << 0)	/* Use quickstart */
#define RHF_NOTPOT		   (1 << 1)	/* Hash size not power of 2 */
#define RHF_NO_LIBRARY_REPLACEMENT (1 << 2)	/* Ignore LD_LIBRARY_PATH */

/* Legal values for l_flags.  */

#define LL_EXACT_MATCH	  (1 << 0)	/* Require exact match */
#define LL_IGNORE_INT_VER (1 << 1)	/* Ignore interface version */

#if defined(__MIPS_AND_IA64_ELF_H)
/*
 * l_flags  These flags exist in each liblist entry.
 * The dynamic section flag bit RHF_SGI_ONLY
 * must be on for most of these to be honored by rld(1).
 * LL_DELAY_LOAD is honored regardless of RHF_SGI_ONLY.
 * If RHF_SGI_ONLY is not on no version matches are done
 * (just as if LL_IGNORE_INT_VER were on).
 * 
 * Recall that a liblist entry has a version string with a single version,
 * while the dynamic section on a DSO has a version string with
 * possibly multiple versions listed (separated by :).  man dso for
 * more details on multiple versions.
 *
 * By default version matches do NOT consider the minor version number.
 *
 * LL_EXACT_MATCH means (if on) that only a DSO with exactly the
 * checksum and timestamp and version from the liblist entry
 * (which were gotten at static-link time but might have
 * been updated by rqs(1)) will match.
 *
 * LL_REQUIRE_MINOR means (if on) that the version match must
 * match the minor version number.
 *
 * LL_NONE is just a convenient name requesting the default checks.
 *
 * LL_DELAY_LOAD means  (if on) that  rld will not load the DSO
 * refered to by the liblist entry till something in the DSO
 * is called.
 *
 * LL_EXPORTS is ignored by rld. It informs that ld exported symbols 
 * from the liblist-entry DSO into the main DSO (that this
 * liblist entry is part of).
 *
 * LL_IGNORE_INT_VER means (if on) that all version checks
 * should be suppressed and the DSO accepted based on
 * name alone.
 *
 * LL_DELTA means that the DSO referenced is Delta C++.
 *
 */

#define LL_NONE			0

#define DT_IRIX_LTR			0x70000033
#define DT_IRIX_LPT			0x70000034
#define DT_IRIX_LPT_IDX			0x70000035
#define DT_IRIX_LPT_NO			0x70000036
#define DT_IRIX_PLT			0x70000037
#define DT_IRIX_PLT_NON_PREEMPT_IDX	0x70000038
#define DT_IRIX_PLT_NON_PREEMPT_NO	0x70000039
#define DT_IRIX_PLT_IN_DYNSYM_IDX	0x7000003A
#define DT_IRIX_PLT_IN_DYMSYM_NO	0x7000003B
#define DT_IRIX_PLT_PROTECTED_IDX	0x7000003C
#define DT_IRIX_PLT_PROTECTED_NO	0x7000003D
#define DT_IRIX_PLT_PREEMPT_IDX		0x7000003E
#define DT_IRIX_PLT_PREEMPT_NO		0x7000003F
#define DT_IRIX_DLT			0x70000040
#define DT_IRIX_DLT_NON_PREEMPT_IDX	0x70000041
#define DT_IRIX_DLT_NON_PREEMPT_NO	0x70000042
#define DT_IRIX_DLT_IN_DYNSYM_IDX	0x70000043
#define DT_IRIX_DLT_IN_DYMSYM_NO	0x70000044
#define DT_IRIX_DLT_PROTECTED_IDX	0x70000045
#define DT_IRIX_DLT_PROTECTED_NO	0x70000046
#define DT_IRIX_DLT_PREEMPT_IDX		0x70000047
#define DT_IRIX_DLT_PREEMPT_NO		0x70000048
#define DT_IRIX_DSYM_PLT_IDX		0x70000049
#define DT_IRIX_DSYM_DLT_IDX		0x7000004A

/* ====================================================================
 *
 * .dynamic Section
 *
 * --- WARNING ---- WARNING ---- WARNING ---- WARNING ---- WARNING ----
 *
 *      TBD:  The .dynamic section has not yet been prepared for
 *	      use in both Irix/mips and Irix/ia64
 *
 *	I believe that most of the tags will be the same in both
 *	Irix/mips and Irix/ia64, but it still needs to be reviewed.
 *	
 *	For now, we'll document them only in mips/elf.h
 * ====================================================================
 */

#ifndef __osf__

#define DT_MIPS_70000015	0x70000015 	  /* reserved */

/*
 *	The following tags are for DELTA_C_PLUS_PLUS only
 *	and are obsolete.  
 */
#if	(defined(_DELTA_C_PLUS_PLUS) && (_DELTA_C_PLUS_PLUS==1))

#define DT_MIPS_DELTA_CLASS 	0x70000017	/* DCC class definitions      */
#define DT_MIPS_DELTA_CLASS_NO 	0x70000018	/* No. DCC class definitions  */
#define DT_MIPS_DELTA_INSTANCE	0x70000019	/* DCC class instances        */
#define DT_MIPS_DELTA_INSTANCE_NO 0x7000001A	/* No. DCC class instances    */
#define DT_MIPS_DELTA_RELOC	0x7000001B	/* DCC relocations            */
#define DT_MIPS_DELTA_RELOC_NO	0x7000001C	/* No.  DCC relocations       */
#define DT_MIPS_DELTA_SYM	0x7000001D	/* DCC symbols Refered to by
						 * DCC relocations            */
#define DT_MIPS_DELTA_SYM_NO	0x7000001E	/* No. DCC symbols	      */
#define DT_MIPS_DELTA_CLASSSYM	0x70000020	/* DCC class declarations     */
#define DT_MIPS_DELTA_CLASSSYM_NO 0x70000021	/* No. DCC class declarations */

#endif	/* _DELTA_C_PLUS_PLUS */


#define	DT_MIPS_CXX_FLAGS	0x70000022	/* Flags:  C++ flavor         */
#define	DT_MIPS_PIXIE_INIT	0x70000023	/* Init code for pixie	      */
#define	DT_MIPS_SYMBOL_LIB	0x70000024
#define DT_MIPS_LOCALPAGE_GOTIDX 0x70000025
#define DT_MIPS_LOCAL_GOTIDX	0x70000026
#define DT_MIPS_HIDDEN_GOTIDX	0x70000027
#define DT_MIPS_PROTECTED_GOTIDX 0x70000028
#define DT_MIPS_OPTIONS		0x70000029	/* Address of .options        */
#define DT_MIPS_INTERFACE	0x7000002a	/* Address of .interface      */
#define DT_MIPS_DYNSTR_ALIGN	0x7000002b
#define DT_MIPS_INTERFACE_SIZE	0x7000002c	/* size of the .interface     */
#define	DT_MIPS_RLD_TEXT_RESOLVE_ADDR 0x7000002d /* Addr of rld_text_resolve  */
#define	DT_MIPS_PERF_SUFFIX	0x7000002e	/* suffix to be added to dso
						 * name before dlopen() call  */
#define DT_MIPS_COMPACT_SIZE	0x7000002f	/* (O32)Size of .compact_rel  */
#define DT_MIPS_GP_VALUE	0x70000030	/* gp value for aux gots      */
#define DT_MIPS_AUX_DYNAMIC	0x70000031      /* Address of aux .dynamic    */

#else  /* __osf__ */

#define DT_MIPS_PACKAGE        	0x70000014
#define DT_MIPS_PACKAGENO       0x70000015
#define DT_MIPS_PACKSYM		0x70000016
#define DT_MIPS_PACKSYMNO	0x70000017
#define	DT_MIPS_IMPACKNO	0x70000018
#define	DT_MIPS_EXPACKNO	0x70000019
#define	DT_MIPS_IMPSYMNO	0x7000001A
#define	DT_MIPS_EXPSYMNO	0x7000001B
#define DT_MIPS_HIPAGENO        0x7000001C

#endif /* __osf__ */
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
#endif

/* Entries found in sections of type SHT_MIPS_CONFLICT.  */

typedef Elf64_Addr Elf64_Conflict;
 
extern Elf64_Conflict   _ConflictList64 [];

#define RLD_VERSION            1


/* HPPA specific definitions.  */

/* Legal values for sh_type field of Elf32_Shdr.  */

#define SHT_PARISC_GOT		0x70000000 /* GOT for external data.  */
#define SHT_PARISC_ARCH		0x70000001 /* Architecture extensions.  */
#define SHT_PARISC_GLOBAL	0x70000002 /* Definition of $global$.  */
#define SHT_PARISC_MILLI	0x70000003 /* Millicode routines.  */
#define SHT_PARISC_PLT		0x70000005 /* Procedure linkage table.  */
#define SHT_PARISC_SDATA	0x70000006 /* Short initialized data.  */
#define SHT_PARISC_SBSS		0x70000007 /* Short uninitialized data.  */
#define SHT_PARISC_SYMEXTN	0x70000008 /* Argument/relocation info.  */
#define SHT_PARISC_STUBS	0x70000009 /* Linker stubs.  */

/* Legal values for sh_flags field of Elf32_Shdr.  */

#define SHF_PARISC_SHORT	0x20000000 /* Section with short addressing. */

/* Legal values for ST_TYPE subfield of st_info (symbol type).  */

#define STT_PARISC_MILLICODE	13	/* Millicode function entry point.  */


/* Alpha specific declarations.  */

/* Alpha relocs.  */

#define R_ALPHA_NONE		0	/* No reloc */
#define R_ALPHA_REFLONG		1	/* Direct 32 bit */
#define R_ALPHA_REFQUAD		2	/* Direct 64 bit */
#define R_ALPHA_GPREL32		3	/* GP relative 32 bit */
#define R_ALPHA_LITERAL		4	/* GP relative 16 bit w/optimization */
#define R_ALPHA_LITUSE		5	/* Optimization hint for LITERAL */
#define R_ALPHA_GPDISP		6	/* Add displacement to GP */
#define R_ALPHA_BRADDR		7	/* PC+4 relative 23 bit shifted */
#define R_ALPHA_HINT		8	/* PC+4 relative 16 bit shifted */
#define R_ALPHA_SREL16		9	/* PC relative 16 bit */
#define R_ALPHA_SREL32		10	/* PC relative 32 bit */
#define R_ALPHA_SREL64		11	/* PC relative 64 bit */
#define R_ALPHA_OP_PUSH		12	/* OP stack push */
#define R_ALPHA_OP_STORE	13	/* OP stack pop and store */
#define R_ALPHA_OP_PSUB		14	/* OP stack subtract */
#define R_ALPHA_OP_PRSHIFT	15	/* OP stack right shift */
#define R_ALPHA_GPVALUE		16
#define R_ALPHA_GPRELHIGH	17
#define R_ALPHA_GPRELLOW	18
#define R_ALPHA_IMMED_GP_16	19
#define R_ALPHA_IMMED_GP_HI32	20
#define R_ALPHA_IMMED_SCN_HI32	21
#define R_ALPHA_IMMED_BR_HI32	22
#define R_ALPHA_IMMED_LO32	23
#define R_ALPHA_COPY		24	/* Copy symbol at runtime */
#define R_ALPHA_GLOB_DAT	25	/* Create GOT entry */
#define R_ALPHA_JMP_SLOT	26	/* Create PLT entry */
#define R_ALPHA_RELATIVE	27	/* Adjust by program base */

__END_DECLS

#endif	/* elf.h */

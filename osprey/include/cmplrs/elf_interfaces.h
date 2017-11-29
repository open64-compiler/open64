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


/* elf_interfaces.h 
 *
 * The following macros provide the consumer interface to the
 * .MIPS.interfaces ELF section.  Full interface specifications
 * including parameter type information are currently emitted only
 * when the -DEBUG:full_interface_check flag is provided to the
 * compiler on the command-line.  Without this flag, interface
 * specifications are generated only for varargs and unprototyped
 * function calls and definitions and these specifications do not
 * include parameter type information.  For details, consult secton
 * 2.11 of the 64-bit Elf Object File Format Specification.
 * 
 * /usr/include/sys/elf.h should also be included when using these
 * macros.
 *
 */

#ifndef ELF_INTERFACES_H_INCLUDED
#define ELF_INTERFACES_H_INCLUDED

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))

/* Elf_Ifc_Size[] provides the sizes (if known) for each fundamental
   type.  The current compilers will never generate some of these
   types.  In other cases, not enough semantic information remains in
   the backend symbol tables to determine the true category of a type */

static Elf64_Byte Elf_Ifc_Size[] = {
  0,  /* 0x0000 = FT_unknown        */
  1,  /* 0x0001 = FT_signed_char    */
  1,  /* 0x0002 = FT_unsigned_char  */
  2,  /* 0x0003 = FT_signed_short   */
  2,  /* 0x0004 = FT_unsigned_short */
  4,  /* 0x0005 = FT_signed_int32   */
  4,  /* 0x0006 = FT_unsigned_int32 */
  8,  /* 0x0007 = FT_signed_int64   */
  8,  /* 0x0008 = FT_unsigned_int64 */
  4,  /* 0x0009 = FT_pointer32      */
  8,  /* 0x000a = FT_pointer64      */
  4,  /* 0x000b = FT_float32        */
  8,  /* 0x000c = FT_float64        */
  16, /* 0x000d = FT_float128       */
  8,  /* 0x000e = FT_complex64      */
  16, /* 0x000f = FT_complex128     */
  32, /* 0x0010 = FT_complex256     */
  0,  /* 0x0011 = FT_void           */
  4,  /* 0x0012 = FT_bool32         */
  8,  /* 0x0013 = FT_bool64         */
  4,  /* 0x0014 = FT_label32        */
  8,  /* 0x0015 = FT_label64        */
  0,  /* 0x0016 = unused            */
  0,  /* 0x0017 = unused            */
  0,  /* 0x0018 = unused            */
  0,  /* 0x0019 = unused            */
  0,  /* 0x001a = unused            */
  0,  /* 0x001b = unused            */
  0,  /* 0x001c = unused            */
  0,  /* 0x001d = unused            */
  0,  /* 0x001e = unused            */
  0,  /* 0x001f = unused            */
  0,  /* 0x0020 = FT_struct         */
  0,  /* 0x0021 = FT_union          */
  0,  /* 0x0022 = FT_enum           */
  0,  /* 0x0023 = FT_typedef        */
  0,  /* 0x0024 = FT_set            */
  0,  /* 0x0025 = FT_range          */
  0,  /* 0x0026 = FT_member_ptr     */
  0,  /* 0x0027 = FT_virtual_ptr    */
  0   /* 0x0028 = FT_class          */
};

/* Consumer Macros */

/* Interface descriptions are complicated data structures which
 * include a variable-length (and variable-composition) section of
 * possibly unaligned data.  The following macros encapsulate most of
 * the details of accessing this information
 *
 * The following conventions apply in these macros:
 * pifd = pointer to beginning of an Elf_Ifd (declared in sys/elf.h)
 * ppar = pointer to beginning of a parameter descriptor
 * 
 * The layout of this information is described in the 64-bit Elf
 * Object File Specification section 2.11
 *
 */

#define ELF_IFD_HAS_VARIABLE_PART(pifd) \
(((pifd)->attrs & (SA_FREE_REGS|SA_PARAMETERS)) ? 1 : 0)
  
#define ELF_IFD_HAS_FREE_REG_MASK(pifd) \
(((pifd)->attrs & SA_FREE_REGS) ? 1 : 0)

#define ELF_IFD_HAS_AUX_PCNT(pifd) \
((pifd)->pcnt == 255)

#define ELF_IFD_HAS_RETURN_TYPE(pifd) \
((pifd)->attrs & SA_FUNCTION)

#define ELF_IFD_FIXED_SIZE (sizeof(Elf_Ifd))

#define ELF_IFD_VARIABLE_SIZE(pifd) \
(ELF_IFD_HAS_VARIABLE_PART(pifd) \
 ? *(Elf64_Half *)((Elf64_Byte *)(pifd) + ELF_IFD_FIXED_SIZE) \
 : 0 )

#define ELF_IFD_TOTAL_SIZE(pifd) \
(ELF_IFD_FIXED_SIZE + ELF_IFD_VARIABLE_SIZE(pifd))

#define ELF_IFD_NEXT_INTERFACE(pifd) \
((Elf_Ifd *)((Elf64_Byte *)(pifd) + ELF_IFD_TOTAL_SIZE(pifd)))

#define ELF_IFD_VARIABLE_PART(pifd) \
(ELF_IFD_HAS_VARIABLE_PART(pifd) ? \
 ((Elf64_Byte *) pifd + ELF_IFD_FIXED_SIZE) : 0)

#define ELF_IFD_FREE_REG_MASK(pifd) \
(*(Elf64_Half *)((Elf64_Byte *)(pifd) + ELF_IFD_FIXED_SIZE + \
		 sizeof(Elf64_Word)))

#define ELF_IFD_PARAMETER_COUNT(pifd) \
(ELF_IFD_HAS_AUX_PCNT(pifd) \
 ? *(Elf64_Half *)((Elf64_Byte *)(pifd) + ELF_IFD_FIXED_SIZE + \
		   sizeof(Elf64_Half)) \
 : (pifd)->pcnt)

#define ELF_IFD_FIRST_PARAMETER(pifd) \
((Elf64_Byte *)(pifd) + ELF_IFD_FIXED_SIZE + sizeof(Elf64_Half) + \
 (ELF_IFD_HAS_AUX_PCNT(pifd) * sizeof(Elf64_Half)) + \
 (ELF_IFD_HAS_FREE_REG_MASK(pifd) * sizeof(Elf64_Half)))

#define ELF_IFD_NEXT_PARAMETER(ppar) \
((Elf64_Byte *)(ppar) + sizeof(Elf64_Half) + \
 (ELF_IFD_PARAMETER_SPECIFIES_SIZE(ppar) * sizeof(Elf64_Word)) + \
 (ELF_IFD_QUALIFIER_COUNT(ppar) * sizeof(Elf64_Byte)))

#define ELF_IFD_PARAMETER_SPECIFIES_SIZE(ppar) \
(((*(Elf64_Byte *)(ppar)) & PDMF_SIZE) ? 1 : 0)

#define ELF_IFD_QUALIFIER_COUNT(ppar) \
((*(Elf64_Byte *)(ppar)) & PDMF_Qualifiers)

#define ELF_IFD_FIRST_QUALIFIER(ppar) \
((Elf64_Byte *)(ppar) + sizeof(Elf64_Half) + \
 ELF_IFD_PARAMETER_SPECIFIES_SIZE(ppar) * sizeof(Elf64_Word))

#define ELF_IFD_PARAMETER_TYPE(ppar) \
(*((Elf64_Byte *)(ppar) + sizeof(Elf64_Byte)))

#define ELF_IFD_PARAMETER_SIZE(ppar) \
(ELF_IFD_PARAMETER_SPECIFIES_SIZE(ppar) \
 ? ((Elf64_Word)((((((((Elf64_Byte *)(ppar))[2]<<8) + \
		     ((Elf64_Byte *)(ppar))[3])<<8) + \
		   ((Elf64_Byte *)(ppar))[4])<<8) + \
		 ((Elf64_Byte *)(ppar))[5])) \
 : Elf_Ifc_Size[ELF_IFD_PARAMETER_TYPE(ppar)])
 
#endif /* (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)) */

#endif /* ELF_INTERFACES_H_INCLUDED */




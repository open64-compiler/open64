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


#ifndef _dwarf_stuff_h
#define _dwarf_stuff_h
#ifdef __cplusplus
extern "C" {
#endif

#include "dwarf.h"
#include "libdwarf.h"

#ifdef TARG_X8664
#undef DW_FRAME_RA_COL
#undef DW_FRAME_STATIC_LINK
#undef DW_FRAME_LAST_REG_NUM
#define DW_FRAME_RA_COL 16 /* column recording ra */

#define DW_FRAME_STATIC_LINK 16 /* column recording static link*/
                                /* applicable to up-level      */
                                /* addressing, as in mp code,  */
                                /* pascal, etc */

/* This is the number of columns in the Frame Table. This constant should
   be kept in sync with DW_REG_TABLE_SIZE defined in libdwarf.h */
#define DW_FRAME_LAST_REG_NUM   (DW_FRAME_STATIC_LINK + 1)

#endif // TARG_X8664

extern Dwarf_Bool	generate_fpic_dwarf;
extern Dwarf_Bool	generate_m64_dwarf;

#if defined (KEY) && defined (TARG_X8664)
#undef DW_REG_TABLE_SIZE
#define DW_REG_TABLE_SIZE  17
#endif // KEY && TARG_X8664

/* The augmenter string for CIE */
#ifdef KEY
#undef DW_CIE_AUGMENTER_STRING_V0
#define DW_CIE_AUGMENTER_STRING_V0              "zPL"
#define PIC_DW_CIE_AUGMENTER_STRING_V0		"zPLR"
// Bug 7278 - implement "zR" CFA augmentation for non-C++ code.
#define PIC_NONCPLUS_DW_CIE_AUGMENTER_STRING_V0 "zR"
#endif // KEY

// Bug 1188
Dwarf_P_Attribute dwf_add_AT_unsigned_const_ext(Dwarf_P_Debug /*dbg*/, 
    Dwarf_P_Die 	/*ownerdie*/, 
    Dwarf_Half 		/*attr*/, 
    Dwarf_Unsigned 	/*value*/, 
    Dwarf_Error* 	/*error*/,
    Dwarf_Unsigned 	/*size*/);
// Bug 1785
Dwarf_P_Attribute dwf_add_AT_complex_const(Dwarf_P_Debug /*dbg*/, 
    Dwarf_P_Die 	/*ownerdie*/, 
    Dwarf_Half 		/*attr*/, 
    Dwarf_Unsigned 	/*value*/, 
    Dwarf_Unsigned 	/*value*/, 
    Dwarf_Error* 	/*error*/,
    Dwarf_Unsigned 	/*size*/);

Dwarf_Unsigned dwf_add_frame_cie(Dwarf_P_Debug /*dbg*/,
    const char*         /*augmenter*/,
    Dwarf_Small         /*code_alignent_factor*/,
    Dwarf_Small         /*data_alignment_factor*/,
    Dwarf_Small         /*return_address_reg*/,
#ifdef KEY
    Dwarf_Unsigned      /* personality routine */,
    Dwarf_Bool          /* generate fpic ? */,
    Dwarf_Bool          /* for -m64 or -m32 ? */,
#endif // KEY
    Dwarf_Ptr           /*initialization_bytes*/,
    Dwarf_Unsigned      /*init_byte_len*/,
    Dwarf_Error*        /*error*/);

Dwarf_Unsigned dwf_add_ehframe_cie(Dwarf_P_Debug /*dbg*/, 
    const char* 	/*augmenter*/, 
    Dwarf_Small 	/*code_alignent_factor*/, 
    Dwarf_Small 	/*data_alignment_factor*/, 
    Dwarf_Small 	/*return_address_reg*/, 
    Dwarf_Unsigned	/* personality routine */,
    Dwarf_Bool		/* generate fpic ? */,
    Dwarf_Bool		/* for -m64 or -m32 ? */,
    Dwarf_Ptr 		/*initialization_bytes*/, 
    Dwarf_Unsigned 	/*init_byte_len*/, 
    Dwarf_Error* 	/*error*/);

Dwarf_Unsigned dwf_add_ehframe_fde( 
    Dwarf_P_Debug 	/*dbg*/,
    Dwarf_P_Fde 	/*fde*/, 
    Dwarf_P_Die 	/*corresponding subprogram die*/,
    Dwarf_Unsigned 	/*cie_to_use*/, 
    Dwarf_Unsigned  	/*virt_addr_of_described_code*/, 
    Dwarf_Unsigned  	/*length_of_code*/, 
    Dwarf_Unsigned 	/*symbol_index*/, 
    Dwarf_Error* 	/*error*/);

Dwarf_Unsigned dwf_add_ehframe_fde_b(
        Dwarf_P_Debug  /*dbg*/,
        Dwarf_P_Fde    /*fde*/,
        Dwarf_P_Die    /*die*/,
        Dwarf_Unsigned /*cie*/,
        Dwarf_Addr     /*virt_addr*/,
        Dwarf_Unsigned /*code_len*/,
        Dwarf_Unsigned /*sym_idx*/,
        Dwarf_Unsigned /*sym_idx_of_end*/,
        Dwarf_Addr     /*offset_from_end_sym*/,
        Dwarf_Error*   /*error*/);

Dwarf_Unsigned dwf_add_ehframe_info_b( 
    Dwarf_P_Debug dbg   /*dbg*/,
    Dwarf_P_Fde 	/*fde*/,
    Dwarf_P_Die 	/*die*/,
    Dwarf_Unsigned 	/*cie*/,
    Dwarf_Addr 	        /*virt_addr*/,
    Dwarf_Unsigned 	/*code_len*/,
    Dwarf_Unsigned 	/*symidx*/,
    Dwarf_Unsigned      /* end_symbol */,
    Dwarf_Addr          /* offset_from_end_symbol */,
    Dwarf_Signed   	/*offset_into_exception_tables*/,
    Dwarf_Unsigned 	/*exception_table_symbol*/,
    Dwarf_Error*	/*error*/);

Dwarf_Unsigned dwf_add_ehframe_info( 
    Dwarf_P_Debug dbg   /*dbg*/,
    Dwarf_P_Fde 	/*fde*/,
    Dwarf_P_Die 	/*die*/,
    Dwarf_Unsigned 	/*cie*/,
    Dwarf_Addr 	        /*virt_addr*/,
    Dwarf_Unsigned 	/*code_len*/,
    Dwarf_Unsigned 	/*symidx*/,
    Dwarf_Signed   	/*offset_into_exception_tables*/,
    Dwarf_Unsigned 	/*exception_table_symbol*/,
    Dwarf_Error*	/*error*/);

#if defined(BUILD_OS_DARWIN)
    /* Mach-O x86 linker requires local "L" labels in Dwarf sections */
#  define DEBUG_INFO_SECTNAME "L.debug_info"
#  define DEBUG_LINE_SECTNAME "L.debug_line"
#  define DEBUG_ABBREV_SECTNAME "L.debug_abbrev"
#  define DEBUG_FRAME_SECTNAME "L.debug_frame"
#  define DEBUG_ARANGES_SECTNAME "L.debug_aranges"
#  define DEBUG_PUBNAMES_SECTNAME "L.debug_pubnames"
#  define DEBUG_STR_SECTNAME "L.debug_str"
#  define DEBUG_FUNCNAMES_SECTNAME "L.debug_funcnames"
#  define DEBUG_TYPENAMES_SECTNAME "L.debug_typenames"
#  define DEBUG_VARNAMES_SECTNAME "L.debug_varnames"
#  define DEBUG_WEAKNAMES_SECTNAME "L.debug_weaknames"
#  define DEBUG_MACINFO_SECTNAME "L.debug_macinfo"
#  define DEBUG_LOC_SECTNAME "L.debug_loc"
#  define EH_FRAME_SECTNAME "L.eh_frame"
#else /* defined(BUILD_OS_DARWIN) */
#  define DEBUG_INFO_SECTNAME ".debug_info"
#  define DEBUG_LINE_SECTNAME ".debug_line"
#  define DEBUG_ABBREV_SECTNAME ".debug_abbrev"
#  define DEBUG_FRAME_SECTNAME ".debug_frame"
#  define DEBUG_ARANGES_SECTNAME ".debug_aranges"
#  define DEBUG_PUBNAMES_SECTNAME ".debug_pubnames"
#  define DEBUG_STR_SECTNAME ".debug_str"
#  define DEBUG_FUNCNAMES_SECTNAME ".debug_funcnames"
#  define DEBUG_TYPENAMES_SECTNAME ".debug_typenames"
#  define DEBUG_VARNAMES_SECTNAME ".debug_varnames"
#  define DEBUG_WEAKNAMES_SECTNAME ".debug_weaknames"
#  define DEBUG_MACINFO_SECTNAME ".debug_macinfo"
#  define DEBUG_LOC_SECTNAME ".debug_loc"
#  define EH_FRAME_SECTNAME ".eh_frame"
#endif /* defined(BUILD_OS_DARWIN) */

#ifdef __cplusplus
}
#endif
#endif /* _dwarf_stuff_h */


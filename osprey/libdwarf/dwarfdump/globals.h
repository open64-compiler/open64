/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/* 
  Copyright (C) 2000,2004 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1500 Crittenden Lane,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan



$Header: /var/tmp/pathscale-compiler-sources-2.4-2006.03.27/kpro64/libdwarf/dwarfdump/globals.h 1.4 05/12/05 09:01:16-08:00 bos@eng-24.pathscale.com $ */
#ifndef globals_INCLUDED
#define globals_INCLUDED

#include "config.h"

/* We want __uint32_t and __uint64_t and __int32_t __int64_t
   properly defined but not duplicated, since duplicate typedefs
   are not legal C.
*/
/*
 HAVE___UINT32_T
 HAVE___UINT64_T will be set by configure if
 our 4 types are predefined in compiler
*/


#if (!defined(HAVE___UINT32_T)) && defined(HAVE_SGIDEFS_H)
#include <sgidefs.h> /* sgidefs.h defines them */
#define HAVE___UINT32_T 1
#define HAVE___UINT64_T 1
#endif



#if (!defined(HAVE___UINT32_T)) && defined(HAVE_SYS_TYPES_H) && defined(HAVE___UINT32_T_IN_SYS_TYPES_H)
#  include <sys/types.h>
/* we assume __[u]int32_t and __[u]int64_t defined 
   since __uint32_t defined in the sys/types.h in use */
#define HAVE___UINT32_T 1
#define HAVE___UINT64_T 1
#endif

#ifndef HAVE___UINT32_T
typedef int __int32_t;
typedef unsigned  __uint32_t;
#define HAVE___UINT32_T 1
#endif
#ifndef HAVE___UINT64_T
typedef long long __int64_t;
typedef unsigned long long  __uint64_t;
#define HAVE___UINT64_T 1
#endif


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_ELF_H
#include <elf.h>
#endif
#ifdef HAVE_LIBELF_H
#include "libelf/libelf.h"
#else
#ifdef HAVE_LIBELF_LIBELF_H
#include "libelf/libelf.h"
#endif
#endif
#include <dwarf.h>
#include <libdwarf.h>

typedef char * string;
typedef int boolean;
#ifndef FALSE
#define FALSE 0
#endif 
#ifndef TRUE
#define TRUE 1
#endif

/* size of attrib_buffer, defined in print_die.c */
#define ATTRIB_BUFSIZ 999

typedef struct {
    int checks;
    int errors;
} Dwarf_Check_Result;

extern int verbose;
extern boolean dense;
extern boolean ellipsis;
extern boolean dst_format;

extern boolean check_pubname_attr;
extern boolean check_attr_tag;
extern boolean check_tag_tree;
extern boolean check_type_offset;

extern Dwarf_Check_Result abbrev_code_result;
extern Dwarf_Check_Result pubname_attr_result;
extern Dwarf_Check_Result reloc_offset_result;
extern Dwarf_Check_Result attr_tag_result;
extern Dwarf_Check_Result tag_tree_result;
extern Dwarf_Check_Result type_offset_result;

extern boolean info_flag;
extern boolean use_old_dwarf_loclist;

extern char cu_name[ ];
extern boolean cu_name_flag;
extern Dwarf_Unsigned cu_offset;
extern Dwarf_Off fde_offset_for_cu_low;
extern Dwarf_Off fde_offset_for_cu_high;


extern int check_error;
extern Dwarf_Error err;
extern void print_error (Dwarf_Debug dbg, string msg,int res, Dwarf_Error err);

extern void print_line_numbers_this_cu (Dwarf_Debug dbg, Dwarf_Die in_die);
extern void print_frames (Dwarf_Debug dbg);
extern void print_pubnames (Dwarf_Debug dbg);
extern void print_macinfo (Dwarf_Debug dbg);
extern void print_locs (Dwarf_Debug dbg);
extern void print_abbrevs (Dwarf_Debug dbg);
extern void print_strings (Dwarf_Debug dbg);
extern void print_aranges (Dwarf_Debug dbg);
extern void print_relocinfo (Dwarf_Debug dbg);
extern void print_static_funcs(Dwarf_Debug dbg);
extern void print_static_vars(Dwarf_Debug dbg);
extern void print_types(Dwarf_Debug dbg);
extern void print_weaknames(Dwarf_Debug dbg);
extern void print_exception_tables(Dwarf_Debug dbg);
extern string get_fde_proc_name(Dwarf_Debug dbg, Dwarf_Addr low_pc);
extern void print_die_and_children(
	Dwarf_Debug dbg, 
	Dwarf_Die in_die,
	char **srcfiles,
	Dwarf_Signed cnt);
extern void print_one_die(
	Dwarf_Debug dbg, 
	Dwarf_Die die, 
	boolean print_information,
	char **srcfiles,
	Dwarf_Signed cnt);

#define DWARF_CHECK_ERROR(str) {\
	printf("*** DWARF CHECK: %s ***\n", str);\
	check_error ++; \
}

#define DWARF_CHECK_ERROR2(str1, str2) {\
	printf("*** DWARF CHECK: %s: %s ***\n", str1, str2);\
	check_error ++; \
}

#endif /* globals_INCLUDED */

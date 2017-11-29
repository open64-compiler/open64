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


#ifndef dwarf_DST_INCLUDED
#define dwarf_DST_INCLUDED


#ifdef _KEEP_RCS_ID
static char *dwarf_DST_rcs_id = "$Source: common/com/SCCS/s.dwarf_DST.h $ $Revision: 1.15 $";
#endif /* _KEEP_RCS_ID */

/* The version number will be accessible through the routines in
 * dwarf_DST_mem.c and is passed through the .B file.  This will
 * permit a compatibility check on the consumer side in the compiler
 * back-end.
 *
 * Version 0 : Initial version of the DST data structure!
 */
#define dwarf_DST_version 0x0


#include "dwarf_stuff.h"
#include "dwarf_DST_mem.h"
#include "symtab_idx.h"	/* defines ST_IDX */
#include "srcpos.h"  /* defines USRCPOS */

#ifdef __cplusplus
extern "C" {
#endif

                      /*----------------------------*
                      * Basic types and definitions *
                      *-----------------------------*/


/* General purpose flags, both representing explicit and implicit 
 * attributes of DST_info objects.
*/
#define DST_no_flag                0x00000000 /* No flag values are set */
#define DST_flag_external          0x00000001 /* AT_external = TRUE */
#define DST_flag_declaration       0x00000002 /* AT_declaration = TRUE */
#define DST_flag_prototyped        0x00000008 /* AT_prototyped = TRUE */
#define DST_flag_memdef            0x00000010 /* Has an AT_specification */
#define DST_flag_const             0x00000020 /* Has an AT_const_value */
#define DST_flag_automatic         0x00000040 /* Is a local variable */
#define DST_flag_optional_parm     0x00000080 /* AT_is_optional = TRUE */
#define DST_flag_variable_parm     0x00000100 /* AT_variable_parameter=TRUE */
#define DST_flag_bitfield          0x00000200 /* TRUE when a bitfield member */
#define DST_flag_comm		   0x00000400 /* TRUE if variable is part of a common block */
#define DST_flag_lb_cval	   0x00000800 /* true if lower bound of subrange is constant */
#define DST_flag_ub_cval	   0x00001000 /* true if upper bound of subrange is constant */
#define DST_flag_cval	   	   0x00002000 /* true if it is a constant */
#define DST_flag_artificial	   0x00004000 /* true if it is an artificial entity */
#define DST_flag_deref		   0x00008000 /* true if symbol should be dereferenced */
#define DST_flag_base_deref	   0x00010000 /* true if symbol's base should be dereferenced */
#define DST_flag_count   	   0x00020000 /* true if subrange contains extent(ie: !upper bound) */
#define DST_flag_stride_1byte  	   0x00040000 /* true if DW_AT_stride is in byte increments   */
#define DST_flag_stride_2byte  	   0x00080000 /* true if DW_AT_stride is in 2 byte increments */ 
#define DST_flag_f90_pointer       0x00100000 /* true if symbol is a pointer */
#define DST_flag_allocatable       0x00200000 /* true if symbol is allocatable*/
#define DST_flag_assumed_shape     0x00400000 /* true if symbol is assumed shape*/
#define DST_flag_assumed_size      0x00800000 /* true if symbol is assumed size*/
#if !defined(TARG_NVISA)
//Obsolete
#define DST_flag_assoc_fe          0x10000000 /* pointer to front-end entity */
#define DST_flag_assoc_be          0x20000000 /* pointer to back-end entity */
#define DST_flag_assoc_idx         0x40000000 /* index to back-end entity */
#endif

#define DST_flag_info_mark         0x80000000 /* DST_info mark for traversal */
#define DST_flag_static		   0x100000000LL /* Has no AT_data_member_location */
#ifdef TARG_X8664
#define DST_flag_GNU_vector        0x200000000LL /* GNU vector extension */
#endif
#define DST_flag_mask              0xffffffff

#define DST_SET_external(flag) (flag |= DST_flag_external)
#define DST_SET_declaration(flag) (flag |= DST_flag_declaration)
#define DST_SET_prototyped(flag) (flag |= DST_flag_prototyped)
#define DST_SET_memdef(flag) (flag |= DST_flag_memdef)
#define DST_SET_const(flag) (flag |= DST_flag_const)
#define DST_SET_comm(flag) (flag |= DST_flag_comm)
#define DST_SET_lb_cval(flag) (flag |= DST_flag_lb_cval)
#define DST_SET_ub_cval(flag) (flag |= DST_flag_ub_cval)
#define DST_SET_cval(flag) (flag |= DST_flag_cval)
#define DST_SET_count(flag) (flag |= DST_flag_count)
#define DST_SET_stride_1byte(flag) (flag |= DST_flag_stride_1byte)
#define DST_SET_stride_2byte(flag) (flag |= DST_flag_stride_2byte)
#define DST_SET_artificial(flag) (flag |= DST_flag_artificial)
#define DST_SET_deref(flag) (flag |= DST_flag_deref)
#define DST_SET_base_deref(flag) (flag |= DST_flag_base_deref)
#define DST_SET_automatic(flag) (flag |= DST_flag_automatic)
#define DST_SET_optional_parm(flag) (flag |= DST_flag_optional_parm)
#define DST_SET_variable_parm(flag) (flag |= DST_flag_variable_parm)
#define DST_SET_bitfield(flag) (flag |= DST_flag_bitfield)
#define DST_SET_f90_pointer(flag) (flag |= DST_flag_f90_pointer)
#define DST_SET_allocatable(flag) (flag |= DST_flag_allocatable)
#define DST_SET_assumed_shape(flag) (flag |= DST_flag_assumed_shape)
#define DST_SET_assumed_size(flag) (flag |= DST_flag_assumed_size)
#if !defined(TARG_NVISA)
#define DST_SET_assoc_fe(flag) (flag |= DST_flag_assoc_fe)
#define DST_SET_assoc_be(flag) (flag |= DST_flag_assoc_be)
#define DST_SET_assoc_idx(flag) (flag |= DST_flag_assoc_idx)
#endif
#define DST_SET_info_mark(flag) (flag |= DST_flag_info_mark)
#define DST_SET_static(flag) (flag |= DST_flag_static)
#ifdef TARG_X8664
#define DST_SET_GNU_vector(flag) (flag |= DST_flag_GNU_vector)
#endif

#define DST_RESET_all(flag) (flag &= ~DST_flag_mask)
#define DST_RESET_external(flag) (flag &= ~DST_flag_external)
#define DST_RESET_declaration(flag) (flag &= ~DST_flag_declaration)
#define DST_RESET_prototyped(flag) (flag &= ~DST_flag_prototyped)
#define DST_RESET_memdef(flag) (flag &= ~DST_flag_memdef)
#define DST_RESET_const(flag) (flag &= ~DST_flag_const)
#define DST_RESET_comm(flag) (flag &= ~DST_flag_comm)
#define DST_RESET_lb_cval(flag) (flag &= ~DST_flag_lb_cval)
#define DST_RESET_ub_cval(flag) (flag &= ~DST_flag_ub_cval)
#define DST_RESET_cval(flag) (flag &= ~DST_flag_cval)
#define DST_RESET_count(flag) (flag &= ~DST_flag_count)
#define DST_RESET_stride_1byte(flag) (flag &= ~DST_flag_stride_1byte)
#define DST_RESET_stride_2byte(flag) (flag &= ~DST_flag_stride_2byte)
#define DST_RESET_artificial(flag) (flag &= ~DST_flag_artificial)
#define DST_RESET_deref(flag) (flag &= ~DST_flag_deref)
#define DST_RESET_base_deref(flag) (flag &= ~DST_flag_base_deref)
#define DST_RESET_automatic(flag) (flag &= ~DST_flag_automatic)
#define DST_RESET_optional_parm(flag) (flag &= ~DST_flag_optional_parm)
#define DST_RESET_variable_parm(flag) (flag &= ~DST_flag_variable_parm)
#define DST_RESET_bitfield(flag) (flag &= ~DST_flag_bitfield)
#define DST_RESET_f90_pointer(flag) (flag &= ~DST_flag_f90_pointer)
#define DST_RESET_allocatable(flag) (flag &= ~DST_flag_allocatable)
#define DST_RESET_assumed_shape(flag) (flag &= ~DST_flag_assumed_shape)
#define DST_RESET_assumed_size(flag) (flag &= ~DST_flag_assumed_size)
#if !defined(TARG_NVISA)
#define DST_RESET_assoc_fe(flag) (flag &= ~DST_flag_assoc_fe)
#define DST_RESET_assoc_be(flag) (flag &= ~DST_flag_assoc_be)
#define DST_RESET_assoc_idx(flag) (flag &= ~DST_flag_assoc_idx)
#endif
#define DST_RESET_info_mark(flag) (flag &= ~DST_flag_info_mark)
#define DST_RESET_static(flag) (flag &= ~DST_flag_static)
#ifdef TARG_X8664
#define DST_RESET_GNU_vector(flag) (flag &= ~DST_flag_GNU_vector)
#endif

#define DST_IS_external(flag) (flag & DST_flag_external)
#define DST_IS_declaration(flag) (flag & DST_flag_declaration)
#define DST_IS_prototyped(flag) (flag & DST_flag_prototyped)
#define DST_IS_memdef(flag) (flag & DST_flag_memdef)
#define DST_IS_const(flag) (flag & DST_flag_const)
#define DST_IS_comm(flag) (flag & DST_flag_comm)
#define DST_IS_lb_cval(flag) (flag & DST_flag_lb_cval)
#define DST_IS_ub_cval(flag) (flag & DST_flag_ub_cval)
#define DST_IS_cval(flag) (flag & DST_flag_cval)
#define DST_IS_count(flag) (flag & DST_flag_count)
#define DST_IS_stride_1byte(flag) (flag & DST_flag_stride_1byte)
#define DST_IS_stride_2byte(flag) (flag & DST_flag_stride_2byte)
#define DST_IS_artificial(flag) (flag & DST_flag_artificial)
#define DST_IS_deref(flag) (flag & DST_flag_deref)
#define DST_IS_base_deref(flag) (flag & DST_flag_base_deref)
#define DST_IS_automatic(flag) (flag & DST_flag_automatic)
#define DST_IS_optional_parm(flag) (flag & DST_flag_optional_parm)
#define DST_IS_variable_parm(flag) (flag & DST_flag_variable_parm)
#define DST_IS_bitfield(flag) (flag & DST_flag_bitfield)
#define DST_IS_f90_pointer(flag) (flag & DST_flag_f90_pointer)
#define DST_IS_allocatable(flag) (flag & DST_flag_allocatable)
#define DST_IS_assumed_shape(flag) (flag & DST_flag_assumed_shape)
#define DST_IS_assumed_size(flag) (flag & DST_flag_assumed_size)
#if !defined(TARG_NVISA)
#define DST_IS_assoc_fe(flag) (flag & DST_flag_assoc_fe)
#define DST_IS_assoc_be(flag) (flag & DST_flag_assoc_be)
#define DST_IS_assoc_idx(flag) (flag & DST_flag_assoc_idx)
#endif
#define DST_IS_info_mark(flag) (flag & DST_flag_info_mark)
#define DST_IS_static(flag) (flag & DST_flag_static)
#ifdef TARG_X8664
#define DST_IS_GNU_vector(flag) (flag & DST_flag_GNU_vector)
#endif


/* Attribute types, used to hold the values defined in "dwarf.h"
*/
typedef UINT64 DST_flag;          /* DST_flag value (see above definitions) */
typedef UINT16 DST_DW_tag;        /* DW_TAG value */
typedef UINT8  DST_ATE_encoding;  /* AT_encoding codes (basetypes) */
typedef UINT8  DST_addr_class;    /* AT_address_class codes. */
typedef UINT16 DST_language;      /* AT_language codes */
typedef UINT16 DST_identifier_case; /* AT_identifier codes */
typedef UINT8  DST_inline;        /* AT_inline (INL_not_inlined if absent) */
typedef UINT8  DST_virtuality;    /* AT_virtuality (member functions and bases */
#ifdef KEY
typedef UINT8  DST_accessibility; /* AT_accessibility 
                                     (member functions and bases */
#endif
typedef UINT16 DST_vtable_elem_location;
				  /* Used for index of function in vtable */
typedef UINT8  DST_bitsize;       /* Used for bit_offset and bit_size */
typedef UINT64 DST_size_t;        /* Used for byte_size of types */
typedef INT64  DST_bounds_t;	  /* Used to hold array bounds */
typedef void  *DST_die_ptr;	  /* Used in backend for die ptr. */


/* Different types of indices to DST entries.  Note that when accessing
 * an entry through DST_IDX_TO_PTR(), the second argument must be of a
 * type as reflected by the form of index.
*/
typedef DST_IDX DST_INFO_IDX; /* Index to a DST_INFO entry */
typedef DST_IDX DST_ATTR_IDX; /* Index to an attribute struct */
typedef DST_IDX DST_FILE_IDX; /* Index to a DST_FILE_NAME entry */
typedef DST_IDX DST_DIR_IDX;  /* Index to a DST_INCLUDE_DIR entry */
typedef DST_IDX DST_MACR_IDX; /* Index to macro info (not yet defined) */
typedef DST_IDX DST_STR_IDX;  /* Index to a string (char*) */


                 /*-------------------------------*
                  * Pointers to back-end entities *
                  *-------------------------------*/

class ST;

typedef union DST_assoc_info
{
   ST_IDX  st_idx;	/* idx to symbol table */
   ST     *st_ptr;	/* A temporary ptr used by fe */
#if !defined(TARG_NVISA)
   /* obsolete */
   union 
   {
     void *fe_ptr;
     UINTPS fe_uint;
   } st_u;
#endif
} DST_ASSOC_INFO;
#define DST_ASSOC_INFO_st_idx(p) ((p).st_idx)
#define DST_ASSOC_INFO_st_level(p) (ST_IDX_level((p).st_idx))
#define DST_ASSOC_INFO_st_index(p) (ST_IDX_index((p).st_idx))
#define DST_ASSOC_INFO_st_ptr(p) ((p).st_ptr)
#if !defined(TARG_NVISA)
#define DST_ASSOC_INFO_fe_ptr(p) ((p).st_u.fe_ptr)
#endif

#define pDST_ASSOC_INFO_st_idx(p) ((p)->st_idx)
#define pDST_ASSOC_INFO_st_level(p) (ST_IDX_level((p)->st_idx))
#define pDST_ASSOC_INFO_st_index(p) (ST_IDX_index((p)->st_idx))
#define pDST_ASSOC_INFO_st_ptr(p) ((p)->st_ptr)
#if !defined(TARG_NVISA)
#define pDST_ASSOC_INFO_fe_ptr(p) ((p)->st_u.fe_ptr)
#endif


                  /*--------------------*
                   * Data Representation *
                   *---------------------*/


/* Forms of constant values.
*/
typedef enum DST_const_form
{
   DST_FORM_STRING, 
   DST_FORM_DATA1, 
   DST_FORM_DATA2, 
   DST_FORM_DATA4, 
#ifndef KEY
   DST_FORM_DATA8
#else
   DST_FORM_DATA8, 
   DST_FORM_DATAC4,
   DST_FORM_DATAC8
#endif // KEY
} DST_CONST_FORM;

typedef struct DST_const_value
{
   DST_CONST_FORM form;
   union 
   {
      DST_IDX form_string;
      UINT8   form_data1;
      UINT16  form_data2;
      UINT32  form_data4;
      UINT64  form_data8;
#ifdef KEY
      struct {
	UINT32 form_crdata4;
	UINT32 form_cidata4;
      } cdata4;
      struct {
	UINT64 form_crdata8;
	UINT64 form_cidata8;
      } cdata8;
#endif // KEY
   } value;
} DST_CONST_VALUE;

#define DST_CONST_VALUE_form(c) ((c).form)
#define DST_CONST_VALUE_form_string(c) ((c).value.form_string)
#define DST_CONST_VALUE_form_data1(c) ((c).value.form_data1)
#define DST_CONST_VALUE_form_data2(c) ((c).value.form_data2)
#define DST_CONST_VALUE_form_data4(c) ((c).value.form_data4)
#define DST_CONST_VALUE_form_data8(c) ((c).value.form_data8)
#ifdef KEY
#define DST_CONST_VALUE_form_crdata4(c) ((c).value.cdata4.form_crdata4)
#define DST_CONST_VALUE_form_cidata4(c) ((c).value.cdata4.form_cidata4)
#define DST_CONST_VALUE_form_crdata8(c) ((c).value.cdata8.form_crdata8)
#define DST_CONST_VALUE_form_cidata8(c) ((c).value.cdata8.form_cidata8)
#endif // KEY



/* Holds info necessary for the "include_directories" part of the
 * .debug_line section as a list of DST_INCLUDE_DIR records.
*/
typedef struct DST_include_dir
{
   DST_STR_IDX path;  /* a string */
   DST_DIR_IDX next;  /* next include dir */
}  DST_INCLUDE_DIR;

#define DST_INCLUDE_DIR_path(dir) ((dir)->path)
#define DST_INCLUDE_DIR_next(dir) ((dir)->next)


/* Holds info necessary for the "file_names" part of the
 * .debug_line section as a list of DST_FILE_NAME records.
*/
typedef struct DST_file_name
{
   DST_STR_IDX  name;  /* a string */
   UINT16       dir;   /* Ordinal number of DST_INCLUDE_DIR (0 if none) */
   UINT64       size;  /* size in bytes; zero if unknown */
   UINT64       modt;  /* last mod time_t (<sys/time.h>); zero if unknown */
   DST_FILE_IDX next;  /* next file name */
}  DST_FILE_NAME;

#define DST_FILE_NAME_name(f) ((f)->name)
#define DST_FILE_NAME_dir(f) ((f)->dir)
#define DST_FILE_NAME_size(f) ((f)->size)
#define DST_FILE_NAME_modt(f) ((f)->modt)
#define DST_FILE_NAME_next(f) ((f)->next)


#ifdef KEY
/* Holds info necessary for the macros.
 */
typedef struct DST_macinfo
{
  DST_STR_IDX macro;
  UINT fileno;
  UINT lineno;
  DST_DW_tag tag;
  DST_MACR_IDX next;
} DST_MACR;

#define DST_MACR_fileno(attr) ((attr)->fileno)
#define DST_MACR_lineno(attr) ((attr)->lineno)
#define DST_MACR_macro(attr)  ((attr)->macro)
#define DST_MACR_tag(attr)    ((attr)->tag)
#define DST_MACR_next(attr)   ((attr)->next)
#endif

/* This structure definition encapsulates the information kept for each
 * type of .debug_info entry.  The "attributes" can be any of the records
 * defined below, while the tag determines the form of entry and, hence,
 * which attribute record pointed to.  Most entries can have siblings.
 * The flag values that apply to each form of DW_TAG are indicated in a
 * comment for each form of attribute struct (no comment when no flag value
 * applies).
*/
typedef struct DST_info
{
   DST_DW_tag   tag;         /* DW_TAG value */
   DST_flag     flag;        /* A set of flags */
   DST_INFO_IDX sibling;     /* dwarf sibling ptr */
   DST_ATTR_IDX attributes;  /* Ptr to attribute struct */
   DST_die_ptr	dieptr;      /* ptr to the die for this DST (used in be)  */
} DST_INFO;

#define DST_INFO_tag(info) ((info)->tag)
#define DST_INFO_flag(info) ((info)->flag)
#define DST_INFO_sibling(info) ((info)->sibling)
#define DST_INFO_attributes(info) ((info)->attributes)
#define DST_INFO_dieptr(info) ((info)->dieptr)


/* The children of a DST entry are linked through the "sibling" indices
 * in DST_INFO, and the parent will have indices to the first and last
 * child in the list.
 */
typedef struct DST_CHILDREN
{
   DST_INFO_IDX  first;
   DST_INFO_IDX last;
} DST_CHILDREN;

   
/* [tag==DW_TAG_compile_unit]: Back-end must supply low/high pc, 
 * as well as relocation info for the low/high pc, statement list 
 * and macro information.
*/
typedef struct DST_compile_unit
{
   DST_STR_IDX   name;        /* Relative path of first source file */
   DST_STR_IDX   comp_dir;    /* cwd when issuing compilation command */
   DST_STR_IDX   producer;    /* Info about compiler */
   DST_language  language;    /* The DW_LANG code */
   DST_identifier_case identifier_case; /* The DW_ID code */
   DST_CHILDREN  child;       /* The file-scope decls/defs */
} DST_COMPILE_UNIT;

#define DST_COMPILE_UNIT_name(attr) ((attr)->name)
#define DST_COMPILE_UNIT_comp_dir(attr) ((attr)->comp_dir)
#define DST_COMPILE_UNIT_producer(attr) ((attr)->producer)
#define DST_COMPILE_UNIT_language(attr) ((attr)->language)
#define DST_COMPILE_UNIT_identifier_case(attr) ((attr)->identifier_case)
#define DST_COMPILE_UNIT_first_child(attr) ((attr)->child.first)
#define DST_COMPILE_UNIT_last_child(attr) ((attr)->child.last)



/* [tag==DW_TAG_inlined_subroutine]:  Back-end must supply 
 * low/high pc (and their reloc info ) and AT_address_class (2.10).
*/
typedef struct DST_inlined_subroutine
{
   DST_ASSOC_INFO low_pc;	   /* Get pc value through back-end LABEL   */
   DST_ASSOC_INFO high_pc;	   /* Get pc value through back-end LABEL   */
   DST_INFO_IDX   abstract_origin; /* The abstract version of this instance */
   DST_TYPE       abstract_dst;    /* dst where abstract_origin is located */
   DST_CHILDREN   child;           /* Formal parameters, and local scope*/
   USRCPOS	  decl;		   /* so can check if cross-file */
   DST_STR_IDX	  abstract_name;   /* "name" for cross-file matching */
} DST_INLINED_SUBROUTINE;

#define DST_INLINED_SUBROUTINE_low_pc(attr) ((attr)->low_pc)
#define DST_INLINED_SUBROUTINE_high_pc(attr) ((attr)->high_pc)
#define DST_INLINED_SUBROUTINE_abstract_origin(attr)\
   ((attr)->abstract_origin)
#define DST_INLINED_SUBROUTINE_first_child(attr) ((attr)->child.first)
#define DST_INLINED_SUBROUTINE_last_child(attr) ((attr)->child.last)
#define DST_INLINED_SUBROUTINE_decl(attr) ((attr)->decl)
#define DST_INLINED_SUBROUTINE_abstract_name(attr) ((attr)->abstract_name)
#define DST_INLINED_SUBROUTINE_abstract_dst(attr) ((attr)->abstract_dst)



/* [tag==DW_TAG_subprogram]: We here explicitly distinguish between four
 * types of subprogram symbol-table entries.  Back-end must supply 
 * low/high pc (and their reloc info ) and AT_address_class (2.10) for 
 * all forms of subprogram, except decls, and AT_calling_convention for 
 * defs.
*/

typedef struct DST_subpr_decl
{
   USRCPOS        decl;       /* Source location */
   DST_STR_IDX    name;       /* Unmangled name of subroutine */
   DST_STR_IDX    linkage_name;  /* Mangled name of subroutine */
   DST_INFO_IDX   type;       /* Return type, DST_INVALID_IDX if void */
   DST_inline     inlin;      /* AT_inline codes (page 24 in V2, Draft 5) */
   DST_virtuality virtuality; /* AT_virtuality codes (page 17 in V2, Draft 5) */
   DST_vtable_elem_location vtable_elem_location;  /* index into virtual table */
   DST_CHILDREN   child;      /* Formal parameter types */
   DST_INFO_IDX	  origin;     /* if weak name, point to original dst */
} DST_SUBPR_DECL;
   
typedef struct DST_subpr_def
{
   USRCPOS        decl;    /* Source location */
   DST_STR_IDX    name;    /* Name of subroutine */
   DST_STR_IDX    linkage_name;  /* Mangled name of subroutine */
   DST_STR_IDX    pubname; /* pubnames name of subroutine (for member fns) */
   DST_INFO_IDX   type;    /* Return type, DST_INVALID_IDX if void */
   DST_ASSOC_INFO st;      /* low/high pc values through ST entry */
   DST_inline     inlin;   /* AT_inline codes (page 24 in V2, Draft 5) */
   DST_virtuality virtuality; /* AT_virtuality code (page 27 in V2, Draft 5) */
   DST_vtable_elem_location vtable_elem_location;  /* index into virtual table */
   DST_INFO_IDX   specification; /* for member function, pointer from def to
				  * decl */
   DST_CHILDREN   child;   /* Formal parameter types */
   DST_INFO_IDX   origin;  /* for clone origin */
} DST_SUBPR_DEF;

typedef struct DST_subpr_memdef
{
   USRCPOS        decl;       /* Source location */
   DST_INFO_IDX   spec;       /* Ptr to class/struct member */
   DST_ASSOC_INFO st;         /* low/high pc values through ST entry */
   DST_CHILDREN   child;      /* Formal parameters, and local scope */
} DST_SUBPR_MEMDEF;

/* "flag" is subset of 
 * [external, declaration, memdef, prototyped]
*/
typedef union DST_subprogram
{
   DST_SUBPR_MEMDEF   memdef;   /* DST_IS_memdef(flag) */
   DST_SUBPR_DECL     decl;     /* DST_IS_declaration(flag) */
   DST_SUBPR_DEF      def;      /* !DST_IS_declaration(flag) */
} DST_SUBPROGRAM;

#define DST_SUBPROGRAM_memdef_decl(attr) ((attr)->memdef.decl)
#define DST_SUBPROGRAM_memdef_spec(attr) ((attr)->memdef.spec)
#define DST_SUBPROGRAM_memdef_st(attr) ((attr)->memdef.st)
#define DST_SUBPROGRAM_memdef_first_child(attr) ((attr)->memdef.child.first)
#define DST_SUBPROGRAM_memdef_last_child(attr) ((attr)->memdef.child.last)

#define DST_SUBPROGRAM_decl_decl(attr) ((attr)->decl.decl)
#define DST_SUBPROGRAM_decl_name(attr) ((attr)->decl.name)
#define DST_SUBPROGRAM_decl_linkage_name(attr) ((attr)->decl.linkage_name)
#define DST_SUBPROGRAM_decl_type(attr) ((attr)->decl.type)
#define DST_SUBPROGRAM_decl_inline(attr) ((attr)->decl.inlin)
#define DST_SUBPROGRAM_decl_virtuality(attr) ((attr)->decl.virtuality)
#define DST_SUBPROGRAM_decl_vtable_elem_location(attr) \
					((attr)->decl.vtable_elem_location)
#define DST_SUBPROGRAM_decl_first_child(attr) ((attr)->decl.child.first)
#define DST_SUBPROGRAM_decl_last_child(attr) ((attr)->decl.child.last)
#define DST_SUBPROGRAM_decl_origin(attr) ((attr)->decl.origin)

#define DST_SUBPROGRAM_def_decl(attr) ((attr)->def.decl)
#define DST_SUBPROGRAM_def_name(attr) ((attr)->def.name)
#define DST_SUBPROGRAM_def_linkage_name(attr) ((attr)->def.linkage_name)
#define DST_SUBPROGRAM_def_pubname(attr) ((attr)->def.pubname)
#define DST_SUBPROGRAM_def_type(attr) ((attr)->def.type)
#define DST_SUBPROGRAM_def_st(attr) ((attr)->def.st)
#define DST_SUBPROGRAM_def_inline(attr) ((attr)->def.inlin)
#define DST_SUBPROGRAM_def_virtuality(attr) ((attr)->def.virtuality)
#define DST_SUBPROGRAM_def_vtable_elem_location(attr) \
				((attr)->def.vtable_elem_location)
#define DST_SUBPROGRAM_def_specification(attr) ((attr)->def.specification)
#define DST_SUBPROGRAM_def_first_child(attr) ((attr)->def.child.first)
#define DST_SUBPROGRAM_def_last_child(attr) ((attr)->def.child.last)
#define DST_SUBPROGRAM_def_clone_origin(attr) ((attr)->def.origin)


/* [tag==DW_TAG_entry_point]:  Back-end must supply 
* low pc (and their reloc info )
*/
typedef struct DST_entry_point
{
   USRCPOS        decl;    /* Source location */
   DST_STR_IDX    name;    /* Name of subroutine */
   DST_INFO_IDX   type;    /* Return type, DST_INVALID_IDX if void */
   DST_ASSOC_INFO st;      /* low pc values through ST entry */
   DST_CHILDREN   child;   /* Formal parameter types */
} DST_ENTRY_POINT; 

#define DST_ENTRY_POINT_decl(attr) ((attr)->decl)
#define DST_ENTRY_POINT_name(attr) ((attr)->name)
#define DST_ENTRY_POINT_type(attr) ((attr)->type)
#define DST_ENTRY_POINT_st(attr) ((attr)->st)
#define DST_ENTRY_POINT_first_child(attr) ((attr)->child.first)
#define DST_ENTRY_POINT_last_child(attr) ((attr)->child.last)

/* [tag==DW_TAG_common_block]: back end supplies location
 * 
*/
typedef struct DST_common_block
{
   DST_STR_IDX    name;	   /* name of common block */
   DST_ASSOC_INFO st;	   /* for getting at_location attribute */
   DST_CHILDREN   child;   /* children within common block */
} DST_COMMON_BLOCK;

#define DST_COMMON_BLOCK_name(attr)  ((attr)->name)
#define DST_COMMON_BLOCK_st(attr)  ((attr)->st)
#define DST_COMMON_BLOCK_first_child(attr)  ((attr)->child.first)
#define DST_COMMON_BLOCK_last_child(attr) ((attr)->child.last)


/* [tag=DW_TAG_common_inclusion]: back end supplies location 
 *
*/
typedef struct DST_common_incl
{
   USRCPOS        decl;    /* Source decl location */
   DST_INFO_IDX   com_blk;	/* reference to common block die */
} DST_COMMON_INCL;

#define DST_COMMON_INCL_decl(attr) ((attr)->decl)
#define DST_COMMON_INCL_com_blk(attr) ((attr)->com_blk)

#ifdef KEY /* Bug 3507 */
typedef struct DST_imported_decl
{
   DST_ASSOC_INFO import;  /* Get offset into .debuginfo via back-end LABEL */
   DST_STR_IDX    name;	   /* name of included declaration (e.g. module) */
} DST_IMPORTED_DECL;
#define DST_IMPORTED_DECL_import(attr)  ((attr)->import)
#define DST_IMPORTED_DECL_name(attr)  ((attr)->name)

typedef struct DST_module
{
   USRCPOS        decl;       /* Source location */
   DST_STR_IDX    name;       /* Name of module */
   DST_CHILDREN   child;      /* Members of module */
} DST_MODULE;
#define DST_MODULE_decl(attr) ((attr)->decl)
#define DST_MODULE_name(attr) ((attr)->name)
#define DST_MODULE_first_child(attr) ((attr)->child.first)
#define DST_MODULE_last_child(attr) ((attr)->child.last)
#endif /* KEY Bug 3507 */

/* [tag==DW_TAG_lexical_block]: Back-end must supply low/high pc and 
 * pc reloc info.
*/
typedef struct DST_lexical_block
{
   DST_STR_IDX    name;            /* For named blocks */
   DST_ASSOC_INFO low_pc;          /* Get pc value through back-end LABEL */
   DST_ASSOC_INFO high_pc;         /* Get pc value through back-end LABEL */
   DST_INFO_IDX   abstract_origin; /* When part of an inlined subroutine */
   DST_CHILDREN   child;           /* Local decls and inner blocks */
} DST_LEXICAL_BLOCK;

#define DST_LEXICAL_BLOCK_name(attr) ((attr)->name)
#define DST_LEXICAL_BLOCK_low_pc(attr) ((attr)->low_pc)
#define DST_LEXICAL_BLOCK_high_pc(attr) ((attr)->high_pc)
#define DST_LEXICAL_BLOCK_abstract_origin(attr) ((attr)->abstract_origin)
#define DST_LEXICAL_BLOCK_first_child(attr) ((attr)->child.first)
#define DST_LEXICAL_BLOCK_last_child(attr) ((attr)->child.last)



/* [tag==DW_TAG_label]: Back-end must supply low pc and 
 * pc reloc info.
*/
typedef struct DST_label
{
   DST_STR_IDX    name;            /* For named blocks */
   DST_ASSOC_INFO low_pc;          /* Get pc value through back-end LABEL */
   DST_INFO_IDX   abstract_origin; /* When part of an inlined subroutine */
} DST_LABEL;

#define DST_LABEL_name(attr) ((attr)->name)
#define DST_LABEL_low_pc(attr) ((attr)->low_pc)
#define DST_LABEL_abstract_origin(attr) ((attr)->abstract_origin)



/* [tag==DW_TAG_variable]:  Back-end must supply start_scope offset and
 * location of variable (BLOCKn, REF) for automatic or defining non-const 
 * declarations.  We distinguish between four types of variables.
*/
typedef struct DST_var_decl
{
   USRCPOS      decl;  /* Source location */
   DST_STR_IDX  name;  /* Name of variable */
   DST_INFO_IDX type;  /* Type of variable */
#ifdef KEY
   DST_STR_IDX    linkage_name;  /* Mangled name of variable */
#endif
} DST_VAR_DECL;

typedef struct DST_var_def
{
   USRCPOS        decl;   /* Source location */
   DST_STR_IDX    name;   /* Name of variable */
   DST_INFO_IDX   type;   /* Type of variable */
   DST_ASSOC_INFO st;     /* Location of variable (through back-end ST) */
   UINT64	  offs;	  /* offset of variable from st */
   DST_INFO_IDX   specification; /* for data member, pointer from def to
				  * field decl */
   DST_INFO_IDX abstract_origin;
   DST_INFO_IDX dopetype;   /* Type of dope vector */
#ifdef KEY
   DST_STR_IDX    linkage_name;  /* Mangled name of variable */
#endif
}  DST_VAR_DEF;

typedef struct DST_var_const
{
   USRCPOS         decl; /* Source location */
   DST_STR_IDX     name; /* Name of const variable */
   DST_INFO_IDX    type; /* Type of const variable */
   DST_CONST_VALUE cval; /* Constant value */
} DST_VAR_CONST;

typedef struct DST_var_comm
{
   USRCPOS 	   decl; /* Source location */
   DST_STR_IDX	   name; /* name of variable in common block */
   DST_INFO_IDX    type; /* Type of variable */
   DST_ASSOC_INFO  st;   /* Location of enclosing common block */
   UINT64 	   offs; /* Constant offset from base of common block */
   DST_INFO_IDX    dopetype;   /* Type of dope vector */
} DST_VAR_COMM;


typedef struct DST_var_memdef
{ 
   USRCPOS        decl;   /* Source location */
   DST_ASSOC_INFO st;     /* Location of variable (through back-end ST) */
   DST_INFO_IDX   spec;   /* Ptr to C++ class member decl */
} DST_VAR_MEMDEF;


/* "flag" is subset of [automatic, external, declaration, const, memdef]
*/
typedef union DST_variable
{
   DST_VAR_DECL   decl;      /* DST_IS_declaration(flag) */
   DST_VAR_DEF    def;       /* !DST_IS_declaration(flag) */
   DST_VAR_CONST  constant;  /* DST_IS_const(flag) */
   DST_VAR_COMM	  comm;	     /* DST_IS_comm(flag) */
   DST_VAR_MEMDEF memdef;    /* DST_IS_memdef(flag) */
} DST_VARIABLE;

#define DST_VARIABLE_decl_decl(attr) ((attr)->decl.decl)
#define DST_VARIABLE_decl_name(attr) ((attr)->decl.name)
#define DST_VARIABLE_decl_type(attr) ((attr)->decl.type)
#ifdef KEY
#define DST_VARIABLE_decl_linkage_name(attr) ((attr)->decl.linkage_name)
#endif

#define DST_VARIABLE_def_decl(attr) ((attr)->def.decl)
#define DST_VARIABLE_def_name(attr) ((attr)->def.name)
#define DST_VARIABLE_def_type(attr) ((attr)->def.type)
#define DST_VARIABLE_def_st(attr) ((attr)->def.st)
#define DST_VARIABLE_def_offs(attr) ((attr)->def.offs)
#define DST_VARIABLE_def_specification(attr) ((attr)->def.specification)
#ifdef KEY
#define DST_VARIABLE_def_linkage_name(attr) ((attr)->def.linkage_name)
#endif
#define DST_VARIABLE_def_abstract_origin(attr) ((attr)->def.abstract_origin)
#define DST_VARIABLE_def_dopetype(attr) ((attr)->def.dopetype)

#define DST_VARIABLE_constant_decl(attr) ((attr)->constant.decl)
#define DST_VARIABLE_constant_name(attr) ((attr)->constant.name)
#define DST_VARIABLE_constant_type(attr) ((attr)->constant.type)
#define DST_VARIABLE_constant_cval(attr) ((attr)->constant.cval)

#define DST_VARIABLE_comm_decl(attr) ((attr)->comm.decl)
#define DST_VARIABLE_comm_name(attr) ((attr)->comm.name)
#define DST_VARIABLE_comm_type(attr) ((attr)->comm.type)
#define DST_VARIABLE_comm_st(attr) ((attr)->comm.st)
#define DST_VARIABLE_comm_offs(attr) ((attr)->comm.offs)
#define DST_VARIABLE_comm_dopetype(attr) ((attr)->comm.dopetype)

#define DST_VARIABLE_memdef_decl(attr) ((attr)->memdef.decl)
#define DST_VARIABLE_memdef_st(attr) ((attr)->memdef.st)
#define DST_VARIABLE_memdef_spec(attr) ((attr)->memdef.spec)



/* [tag==DW_TAG_formal_parameter]: Back-end must supply run-time
 * location of variable (BLOCKn, REF) if belonging to a definition.
 *
 * "flag" is subset of [optional, variable_parameter]
*/
typedef struct DST_formal_parameter
{
   USRCPOS      decl;             /* Source location */
   DST_STR_IDX  name;             /* Name of parameter */
   DST_INFO_IDX type;             /* Type of parameter */
   DST_ASSOC_INFO st;         /* Location of variable (through back-end ST) */
   DST_INFO_IDX abstract_origin;  /* When part of an inlined subroutine */
   DST_INFO_IDX default_val;      /* Default value of (C++) parameter */
   DST_INFO_IDX dopetype;         /* Type of dope vector */
} DST_FORMAL_PARAMETER;

#define DST_FORMAL_PARAMETER_decl(attr) ((attr)->decl)
#define DST_FORMAL_PARAMETER_name(attr) ((attr)->name)
#define DST_FORMAL_PARAMETER_type(attr) ((attr)->type)
#define DST_FORMAL_PARAMETER_st(attr) ((attr)->st)
#define DST_FORMAL_PARAMETER_abstract_origin(attr) ((attr)->abstract_origin)
#define DST_FORMAL_PARAMETER_default_val(attr) ((attr)->default_val)
#define DST_FORMAL_PARAMETER_dopetype(attr) ((attr)->dopetype)



/* [tag==DW_TAG_unspecified_parameters]
*/
typedef struct DST_unspecified_parameters
{
   USRCPOS      decl;             /* Source location */
   DST_INFO_IDX abstract_origin;  /* When part of an inlined subroutine */
} DST_UNSPECIFIED_PARAMETERS;

#define DST_UNSPECIFIED_PARAMETERS_decl(attr) ((attr)->decl)
#define DST_UNSPECIFIED_PARAMETERS_abstract_origin(attr)\
   ((attr)->abstract_origin)



/* [tag==DW_TAG_constant]
 * "flag" is subset of [declaration, external]
*/
typedef struct DST_const_def
{
   USRCPOS         decl;  /* Source location */
   DST_STR_IDX     name;  /* Name of constant */
   DST_INFO_IDX    type;  /* Type of constant */
   DST_CONST_VALUE cval;  /* Constant value, only if not declaration */
} DST_CONST_DEF;

typedef struct DST_const_decl
{
   USRCPOS         decl;  /* Source location */
   DST_INFO_IDX    name;  /* Name of constant */
   DST_INFO_IDX    type;  /* Type of constant */
} DST_CONST_DECL;

typedef union DST_constant
{
   DST_CONST_DECL  decl;  /* Non-defining declaration */
   DST_CONST_DEF   def;   /* Defining declaration */
} DST_CONSTANT;

#define DST_CONSTANT_decl_decl(attr) ((attr)->decl.decl)
#define DST_CONSTANT_decl_name(attr) ((attr)->decl.name)
#define DST_CONSTANT_decl_type(attr) ((attr)->decl.type)

#define DST_CONSTANT_def_decl(attr) ((attr)->def.decl)
#define DST_CONSTANT_def_name(attr) ((attr)->def.name)
#define DST_CONSTANT_def_type(attr) ((attr)->def.type)
#define DST_CONSTANT_def_cval(attr) ((attr)->def.cval)



/* [tag==DW_TAG_base_type]
*/
typedef struct DST_basetype
{
   DST_STR_IDX      name;      /* Name of type */
   DST_ATE_encoding encoding;  /* How to encode/interpret these objects */
   DST_size_t       byte_size; /* Size of object of this type */
} DST_BASETYPE;

#define DST_BASETYPE_name(attr) ((attr)->name)
#define DST_BASETYPE_encoding(attr) ((attr)->encoding)
#define DST_BASETYPE_byte_size(attr) ((attr)->byte_size)



/* [tag==DW_TAG_const_type]: The back-end must supply the AT_start_scope
 * attribute.
*/
typedef struct DST_qualified_type
{
   DST_INFO_IDX type;    /* Type that is qualified */
} DST_QUALIFIED_TYPE;

typedef DST_QUALIFIED_TYPE DST_CONST_TYPE;
#define DST_CONST_TYPE_type(attr) ((attr)->type)



/* [tag==DW_TAG_volatile_type]: The back-end must supply the AT_start_scope
 * attribute.
*/
typedef DST_QUALIFIED_TYPE DST_VOLATILE_TYPE;
#define DST_VOLATILE_TYPE_type(attr) ((attr)->type)



/* [tag==DW_TAG_pointer_type]:  The back-end must supply the AT_start_scope
 * attribute.  The AT_address_class attribute would normally be DW_ADDR_none!
*/
typedef struct DST_pointer_type
{
   DST_INFO_IDX   type;
      /* Type that is pointed to, DST_INVALID_IDX if void */

   DST_addr_class address_class;
      /* How to dereference objects of this type */

   DST_size_t     byte_size;
      /* Size of pointer type (4 or 8 bytes) */
} DST_POINTER_TYPE;

#define DST_POINTER_TYPE_type(attr) ((attr)->type)
#define DST_POINTER_TYPE_address_class(attr) ((attr)->address_class)
#define DST_POINTER_TYPE_byte_size(attr) ((attr)->byte_size)



/* [tag==DW_TAG_reference_type]:  The back-end must supply the AT_start_scope
 * attribute.  The AT_address_class attribute would normally be DW_ADDR_none!
*/
typedef DST_POINTER_TYPE DST_REFERENCE_TYPE;

#define DST_REFERENCE_TYPE_type(attr) ((attr)->type)
#define DST_REFERENCE_TYPE_address_class(attr) ((attr)->address_class)
#define DST_REFERENCE_TYPE_byte_size(attr) ((attr)->byte_size)



/* [tag==DW_TAG_typedef]:  The back-end must supply the AT_start_scope
 * attribute.
*/
typedef struct DST_typedef
{
   USRCPOS      decl; /* Source location */
   DST_STR_IDX  name; /* Name of type */
   DST_INFO_IDX type; /* Defining type, NULL for incomplete declaration */
   DST_INFO_IDX abstract_origin; /* Defined inside inlined proc */
} DST_TYPEDEF;

#define DST_TYPEDEF_decl(attr) ((attr)->decl)
#define DST_TYPEDEF_name(attr) ((attr)->name)
#define DST_TYPEDEF_type(attr) ((attr)->type)
#define DST_TYPEDEF_abstract_origin(attr) ((attr)->abstract_origin)



/* [tag==DW_TAG_array_type]:  The back-end must supply the AT_start_scope
 * attribute.  "flags" in [declaration].
*/
typedef struct DST_array_type   /* Assume row-major order for multi-dim. */
{
   USRCPOS      decl;      /* Source location */
   DST_STR_IDX  name;      /* Name of type */
   DST_INFO_IDX type;      /* Element type */
   DST_size_t   byte_size; /* Size of array, if known at compile time */
   DST_INFO_IDX abstract_origin; /* Defined inside inlined proc */
   DST_CHILDREN child;     /* Bounds for each dimension (DST_subrange_type) */
} DST_ARRAY_TYPE;

#define DST_ARRAY_TYPE_decl(attr) ((attr)->decl)
#define DST_ARRAY_TYPE_name(attr) ((attr)->name)
#define DST_ARRAY_TYPE_type(attr) ((attr)->type)
#define DST_ARRAY_TYPE_byte_size(attr) ((attr)->byte_size)
#define DST_ARRAY_TYPE_abstract_origin(attr) ((attr)->abstract_origin)
#define DST_ARRAY_TYPE_first_child(attr) ((attr)->child.first)
#define DST_ARRAY_TYPE_last_child(attr) ((attr)->child.last)



/* [tag==DW_TAG_subrange_type]: Represents array bounds (child of 
 * array_type).
*/
typedef union 
{
   DST_bounds_t cval;
   DST_INFO_IDX ref;
} DST_cval_ref;

typedef struct DST_subrange_type
{
   DST_cval_ref lower;		/* lower bound */
   DST_cval_ref upper;		/* upper bound */
   DST_cval_ref stride;		/* stride - for non-contiguous sections (F90) */
} DST_SUBRANGE_TYPE;

#define DST_SUBRANGE_TYPE_count(attr) ((attr)->upper.cval - (attr)->lower.cval + 1)
#define DST_SUBRANGE_TYPE_lower_cval(attr) ((attr)->lower.cval)
#define DST_SUBRANGE_TYPE_upper_cval(attr) ((attr)->upper.cval)
#define DST_SUBRANGE_TYPE_lower_ref(attr) ((attr)->lower.ref)
#define DST_SUBRANGE_TYPE_upper_ref(attr) ((attr)->upper.ref)
#define DST_SUBRANGE_TYPE_stride_ref(attr) ((attr)->stride.ref)


/* [tag==DW_TAG_string_type]: Represents FORTRAN or pascal string 
 * type.
*/
typedef struct DST_string_type
{
   USRCPOS	decl;	   /* Source location */
   DST_STR_IDX  name;      /* Name of type */
   DST_cval_ref len;	   /* length of string */
} DST_STRING_TYPE;

#define DST_STRING_TYPE_decl(attr) ((attr)->decl)
#define DST_STRING_TYPE_name(attr) ((attr)->name)
#define DST_STRING_TYPE_len_cval(attr) ((attr)->len.cval)
#define DST_STRING_TYPE_len_ref(attr) ((attr)->len.ref)


/* [tag==DW_TAG_structure_type]:  The back-end must supply the AT_start_scope
 * attribute.  "flag" is subset of [declaration]
*/
typedef struct DST_structure_type
{
   USRCPOS      decl;      /* Source location */
   DST_STR_IDX  name;      /* Name of type */
   DST_size_t   byte_size; /* Size of struct, if known at compile time */
   DST_INFO_IDX abstract_origin; /* Inside inlined instance of proc. */
   DST_INFO_IDX inheritance; /* Inheritance entries (DW_TAG_inheritance) */
   DST_CHILDREN child;     /* Struct members (DW_TAG_member) */
} DST_STRUCTURE_TYPE;

#define DST_STRUCTURE_TYPE_decl(attr) ((attr)->decl)
#define DST_STRUCTURE_TYPE_name(attr) ((attr)->name)
#define DST_STRUCTURE_TYPE_byte_size(attr) ((attr)->byte_size)
#define DST_STRUCTURE_TYPE_abstract_origin(attr) ((attr)->abstract_origin)
#define DST_STRUCTURE_TYPE_inheritance(attr) ((attr)->inheritance)
#define DST_STRUCTURE_TYPE_first_child(attr) ((attr)->child.first)
#define DST_STRUCTURE_TYPE_last_child(attr) ((attr)->child.last)



/* [tag==DW_TAG_union_type]:  The back-end must supply the AT_start_scope
 * attribute.
*/
typedef DST_STRUCTURE_TYPE DST_UNION_TYPE;

#define DST_UNION_TYPE_decl(attr) ((attr)->decl)
#define DST_UNION_TYPE_name(attr) ((attr)->name)
#define DST_UNION_TYPE_byte_size(attr) ((attr)->byte_size)
#define DST_UNION_TYPE_abstract_origin(attr) ((attr)->abstract_origin)
#define DST_UNION_TYPE_inheritance(attr) ((attr)->inheritance)
#define DST_UNION_TYPE_first_child(attr) ((attr)->child.first)
#define DST_UNION_TYPE_last_child(attr) ((attr)->child.last)



/* [tag==DW_TAG_class_type]:  The back-end must supply the AT_start_scope
 * attribute.
*/
typedef DST_STRUCTURE_TYPE DST_CLASS_TYPE;

#define DST_CLASS_TYPE_decl(attr) ((attr)->decl)
#define DST_CLASS_TYPE_name(attr) ((attr)->name)
#define DST_CLASS_TYPE_byte_size(attr) ((attr)->byte_size)
#define DST_CLASS_TYPE_abstract_origin(attr) ((attr)->abstract_origin)
#define DST_CLASS_TYPE_inheritance(attr) ((attr)->inheritance)
#define DST_CLASS_TYPE_first_child(attr) ((attr)->child.first)
#define DST_CLASS_TYPE_last_child(attr) ((attr)->child.last)


/* [tag==DW_TAG_member]
 *
 * "flag" is subset of [declaration bitfield]
*/
typedef struct DST_member
{
   USRCPOS      decl;      /* Source location */
   DST_STR_IDX  name;         /* Name of member */
   DST_INFO_IDX type;         /* Type of member */
   DST_size_t   memb_loc;     /* Offset of member (data-block) within
			         struct/class  (0 for union) */
   DST_size_t   byte_size;    /* Size of bitfield data-block */
   DST_bitsize  bit_offset;   /* Offset of bitfield within block */
   DST_bitsize  bit_size;     /* Size of bitfield */
   DST_INFO_IDX dopetype;     /* Type of dope vector */
#ifdef KEY
   DST_accessibility accessibility; /* AT_accessibility */
#endif
} DST_MEMBER;

#define DST_MEMBER_decl(attr) ((attr)->decl)
#define DST_MEMBER_name(attr) ((attr)->name)
#define DST_MEMBER_type(attr) ((attr)->type)
#define DST_MEMBER_memb_loc(attr) ((attr)->memb_loc)
#define DST_MEMBER_byte_size(attr) ((attr)->byte_size)
#define DST_MEMBER_bit_offset(attr) ((attr)->bit_offset)
#define DST_MEMBER_bit_size(attr) ((attr)->bit_size)
#define DST_MEMBER_dopetype(attr) ((attr)->dopetype)
#ifdef KEY
#define DST_MEMBER_accessibility(attr) ((attr)->accessibility)
#endif



/* [tag==DW_TAG_inheritance]
 *
*/
typedef struct DST_inheritance
{
   USRCPOS        decl;         /* Source location */
   DST_INFO_IDX   type;         /* Type of member */
   DST_virtuality virtuality;   /* AT_virtuality codes (Page 17, V2, Draft 5) */
   DST_size_t     memb_loc;     /* Offset of member (data-block) within
			         struct/class  (0 for union) */
#ifdef KEY
   DST_accessibility 	accessibility;/* Accessibility for C++ */ 
#endif
} DST_INHERITANCE;

#define DST_INHERITANCE_decl(attr)      ((attr)->decl)
#define DST_INHERITANCE_type(attr)      ((attr)->type)
#define DST_INHERITANCE_virtuality(attr) ((attr)->virtuality)
#define DST_INHERITANCE_memb_loc(attr)  ((attr)->memb_loc)
#ifdef KEY
#define DST_INHERITANCE_accessibility(attr) ((attr)->accessibility)
#endif



/* [tag==DW_TAG_template_type_parameter]
 *
*/
typedef struct DST_template_type_parameter
{
   USRCPOS      decl;         /* Source location */
   DST_STR_IDX  name;         /* Name of formal type parameter */
   DST_INFO_IDX type;         /* Actual parameter type */
} DST_TEMPLATE_TYPE_PARAMETER;

#define DST_TEMPLATE_TYPE_PARAMETER_decl(attr) ((attr)->decl)
#define DST_TEMPLATE_TYPE_PARAMETER_name(attr) ((attr)->name)
#define DST_TEMPLATE_TYPE_PARAMETER_type(attr) ((attr)->type)



/* [tag==DW_TAG_template_value_parameter]
 *
*/
typedef struct DST_template_value_parameter
{
   USRCPOS         decl;         /* Source location */
   DST_STR_IDX     name;         /* Name of formal type parameter */
   DST_INFO_IDX    type;         /* Actual parameter type */
   DST_CONST_VALUE cval;         /* Constant value, only if not declaration */
} DST_TEMPLATE_VALUE_PARAMETER;

#define DST_TEMPLATE_VALUE_PARAMETER_decl(attr) ((attr)->decl)
#define DST_TEMPLATE_VALUE_PARAMETER_name(attr) ((attr)->name)
#define DST_TEMPLATE_VALUE_PARAMETER_type(attr) ((attr)->type)
#define DST_TEMPLATE_VALUE_PARAMETER_cval(attr) ((attr)->cval)



/* [tag==DW_TAG_enumeration_type]:  The back-end must supply the 
 * AT_start_scope attribute.
 *
 * "flag" is subset of [declaration]
*/
typedef struct DST_enumeration_type
{
   USRCPOS      decl;      /* Source location */
   DST_STR_IDX  name;      /* Name of type */
   DST_size_t   byte_size; /* Size of enumeration objects */
   DST_INFO_IDX abstract_origin; /* Declared in inlined instance of proc. */
   DST_CHILDREN child;     /* enumerators (DW_TAG_enumerator entries) */
} DST_ENUMERATION_TYPE;

#define DST_ENUMERATION_TYPE_decl(attr) ((attr)->decl)
#define DST_ENUMERATION_TYPE_name(attr) ((attr)->name)
#define DST_ENUMERATION_TYPE_byte_size(attr) ((attr)->byte_size)
#define DST_ENUMERATION_TYPE_abstract_origin(attr) ((attr)->abstract_origin)
#define DST_ENUMERATION_TYPE_first_child(attr) ((attr)->child.first)
#define DST_ENUMERATION_TYPE_last_child(attr) ((attr)->child.last)



/* [tag==DW_TAG_enumerator]
*/
typedef struct DST_enumerator
{
   USRCPOS         decl;    /* Source location */
   DST_STR_IDX     name;    /* Name of type */
   DST_CONST_VALUE cval;    /* Enumeration value */
} DST_ENUMERATOR;

#define DST_ENUMERATOR_decl(attr) ((attr)->decl)
#define DST_ENUMERATOR_name(attr) ((attr)->name)
#define DST_ENUMERATOR_cval(attr) ((attr)->cval)



/* [tag==DW_TAG_subroutine_type]
 *
 * "flag" is subset of [prototyped]
*/
typedef struct DST_subroutine_type
{
   USRCPOS      decl;   /* Source location */
   DST_STR_IDX  name;   /* Name of type */
   DST_INFO_IDX type;   /* Return type, DST_INVALID_IDX if void */
   DST_INFO_IDX abstract_origin; /* In inlined proc. instance */
   DST_CHILDREN child;  /* Parameters (may have unspecified param) */
} DST_SUBROUTINE_TYPE;

#define DST_SUBROUTINE_TYPE_decl(attr) ((attr)->decl)
#define DST_SUBROUTINE_TYPE_name(attr) ((attr)->name)
#define DST_SUBROUTINE_TYPE_type(attr) ((attr)->type)
#define DST_SUBROUTINE_TYPE_abstract_origin(attr) ((attr)->abstract_origin)
#define DST_SUBROUTINE_TYPE_first_child(attr) ((attr)->child.first)
#define DST_SUBROUTINE_TYPE_last_child(attr) ((attr)->child.last)


/* [tag==DW_TAG_ptr_to_member_type]: Back-end must supply run-time location
 * of member pointed to (.debug_loc offset).
*/
typedef struct DST_ptr_to_member_type
{
   DST_STR_IDX  name;       /* Name of member pointed to */
   DST_INFO_IDX type;       /* Type of member pointed to */
   DST_INFO_IDX class_type; /* Type of class of member pointed to */
} DST_PTR_TO_MEMBER_TYPE;

#define DST_PTR_TO_MEMBER_TYPE_name(attr) ((attr)->name)
#define DST_PTR_TO_MEMBER_TYPE_type(attr) ((attr)->type)
#define DST_PTR_TO_MEMBER_TYPE_class_type(attr) ((attr)->class_type)



      /*--------------------------------*
      * General purpose routines/macros *
      *--------------------------------*/


/* Converts an index to a pointer to an object of the given type.
*/
#define DST_IDX_TO_PTR(i, btype) ((btype *)DST_idx_to_string(i))
#define DST_INFO_IDX_TO_PTR(i) ((DST_INFO *)DST_idx_to_string(i))
#define DST_ATTR_IDX_TO_PTR(i, attr_struct) DST_IDX_TO_PTR(i, attr_struct)
#define DST_FILE_IDX_TO_PTR(i) ((DST_FILE_NAME *)DST_idx_to_string(i))
#define DST_DIR_IDX_TO_PTR(i) ((DST_INCLUDE_DIR *)DST_idx_to_string(i))
#define DST_STR_IDX_TO_PTR(i) ((char *)DST_idx_to_string(i))
#ifdef KEY
#define DST_MACR_IDX_TO_PTR(i) ((DST_MACR *)DST_idx_to_string(i))
#endif

#ifdef KEY
/* [tag==DW_TAG_namelist]
*/
typedef struct DST_namelist
{
   USRCPOS      decl;      /* Source location */
   DST_STR_IDX  name;      /* Name of type */
   DST_CHILDREN child;     /* namelist items (DW_TAG_namelist_item entries) */
} DST_NAMELIST;

#define DST_NAMELIST_decl(attr) ((attr)->decl)
#define DST_NAMELIST_name(attr) ((attr)->name)
#define DST_NAMELIST_first_child(attr) ((attr)->child.first)
#define DST_NAMELIST_last_child(attr) ((attr)->child.last)

/* [tag==DW_TAG_namelist_item]
*/
typedef struct DST_namelist_item
{
   USRCPOS      decl;      /* Source location */
   DST_STR_IDX  name;      /* Name of type */
} DST_NAMELIST_ITEM;

#define DST_NAMELIST_ITEM_decl(attr) ((attr)->decl)
#define DST_NAMELIST_ITEM_name(attr) ((attr)->name)
#endif



    /*-----------------------------------------------------------------
    * Routines for traversing the child/sibling structure of DST nodes 
    *------------------------------------------------------------------*/


/* When the entry has no child attribute or the child attribute is null 
 * (DST_IS_NULL) then an invalid idx is returned (DST_INVALID_IDX),
 * otherwise an index to the first child is returned.
*/
extern DST_INFO_IDX DST_first_child(DST_INFO_IDX parent);
extern DST_INFO_IDX DST_last_child(DST_INFO_IDX parent);



/* Appends a child to the end of the list of children (linked by sibling 
 * indices) of the "parent".  The child may be the first child of the
 * parent.
*/
extern void
DST_append_child(DST_INFO_IDX parent, DST_INFO_IDX child);



/* Used to traverse the DST subgraph rooted at "i".  Note that we
 * only follow child/sibling pointers, and the graph will therefore
 * appear as a tree (a node can only be the child of at most one parent,
 * and the sibling of at most one predecessor).  The parameters are the 
 * root of the tree to be traversed ("i"), an initial value to be used in
 * visiting the root of the tree ("init_val"), and an action to be
 * performed on each tree-node encountered in the pre-order tree-walk
 * ("action"). The action takes the value returned from the action on the
 * parent, as well as the tag, flag, attribute, and index of the node
 * currently visited.
*/
extern void
DST_preorder_visit(
   DST_INFO_IDX i,
   INT32        init_val, 
   INT32 (*action)(INT32, DST_DW_tag, DST_flag, DST_ATTR_IDX, DST_INFO_IDX));


#ifdef __cplusplus
}
#endif
#endif /* dwarf_DST_INCLUDED */


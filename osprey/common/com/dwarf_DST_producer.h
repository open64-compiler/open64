/*
 * Copyright 2005-2007 NVIDIA Corporation.  All rights reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef dwarf_DST_producer_INCLUDED
#define dwarf_DST_producer_INCLUDED

/* ====================================================================
 * ====================================================================
 *
 * Module: dwarf_DST_producer.h
 * $Revision: 1.9 $
 * $Date: 04/12/21 15:18:03-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /scratch/mee/Patch0002-taketwo/kpro64-pending/common/com/SCCS/s.dwarf_DST_producer.h $
 *
 * Revision history:
 *  25-Apr-93 - Original Version
 *
 * Description:
 *
 *  The interface to be used for generation of debugging information
 *  by the front-end, where the resulting information is used by the 
 *  back-end (see dwarf_DST_consumer.h) to generate dwarf debugging
 *  sections in the object code.  Note that we currently only generate
 *  the C specific DST_INFO forms, i.e. some of the forms of DST_INFO
 *  entries defined in "dwarf_DST.h" are not currently generated (e.g.
 *  DW_TAG_constant entries).  This interface may be enhanced to also
 *  support other languages, such as C++ and Fortran.
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *dwarf_DST_producer_rcs_id = "$Source: /scratch/mee/Patch0002-taketwo/kpro64-pending/common/com/SCCS/s.dwarf_DST_producer.h $ $Revision: 1.9 $";
#endif /* _KEEP_RCS_ID */


#include "dwarf_DST.h"

#ifdef __cplusplus
extern "C" {
#endif

   /*--------------------------
    * Initiation & Finalization
    *---------------------------*/


/* Must be called first!  Initiates the memory system used to hold 
 * DST structures.
*/
extern void DST_init_mem(void);



/* Must be called last!  Writes all blocks in memory to the .B file 
 * and resets the memory system.
*/
extern void DST_final_write(void);


   /*-----------------------------------------------------------
    * Creation of ordered list of directories for include files
    *-----------------------------------------------------------*/


/* The entries will be listed in the order in which they are created.
*/
extern DST_DIR_IDX DST_mk_include_dir(char *path);


/* The number of include directories entered thus far.
*/
extern mUINT16 DST_number_of_include_dirs(void);


   /*---------------------------------------
    * Creation of ordered listof file_names
    *---------------------------------------*/


/* The entries will be listed in the order in which they are created.
 * The incl_dir is the ordinal position of the dir in the include_dirs
 * list, the size is in bytes (zero if abscent), the modt is the last
 * modification time (e.g. a time_t from <sys/time.h>).
 * Returns the ordinal position of the file-name in the list
 * of file-names.
*/
extern DST_FILE_IDX DST_mk_file_name(char *file_name, 
				     mUINT16 incl_dir,
				     UINT64  size,
				     UINT64  modt);


/* The number of file_names entered thus far.
*/
extern mUINT16 DST_number_of_files(void);

#ifdef KEY
/* Define or undefine a macro.
 */
extern DST_MACR_IDX
DST_mk_macr (UINT lineno, /* source line number */
	     char *name,  /* macro and definition */
	     INT  tag     /* DW_MACINFO_* */);
  
/* End a file for the macinfo section.
 */
extern DST_MACR_IDX
DST_mk_macr_end_file (void);
  
/* Start a file for macinfo section.
 */
extern DST_MACR_IDX
DST_mk_macr_start_file (UINT lineno, /* source line number */
			UINT fileno  /* a file number */);
  
/* The number of macros entered so far.
 */
extern mUINT16 DST_number_of_macros(void);
#endif

   /*------------------------------------------------
    * Creation of .debug_macinfo section information
    *------------------------------------------------*/

                 /* To be done! */


   /*---------------------------------------------
    * Creation of .debug_info section information
    *---------------------------------------------*/

/* The first two routines will force all new info entries to be
 * allocated in the file_scope memory region.  Take care not to call
 * DST_begin_PU() or DST_end_PU() while a lock is in effect.  A call 
 * to DST_begin_PU() will not take effect until the lock to file_scope 
 * memory is released, and a call to DST_end_PU() may not end the 
 * PU memory region if it is followed by a release of this lock.  Use
 * the next three routines to handle situations where it becomes
 * necessary temporarily to reset all locks.
 */
extern void DST_lock_to_file_scope_mem(void);
extern void DST_release_from_file_scope_mem(void);

extern mINT32 DST_unwind_locks(void);
extern void DST_wind_up_locks(mINT32 number_of_locks);
extern BOOL DST_is_locked_to_file_scope_mem(void);


/* Creates a DW_TAG_compile_unit node and must be called before creating 
 * any nodes for file_scope decls/defs.  Thus, this is the first 
 * information recorded for the .debug_info section of the object file.
 * "src_path" is the relative or full path to the primary source file, 
 * "comp_dir" is the current working directory when the compilation 
 * command was issued, "comp_info" is information about the version of 
 * compiler used, and "language" is a DW_LANG code (see dwarf.h).
 * Returns an index to the created DW_TAG_compile_unit entry.
*/
extern DST_INFO_IDX DST_mk_compile_unit(char        *src_path,
					char        *comp_dir,
					char        *comp_info,
					DST_language language,
					DST_identifier_case id_case);


		
/* Must be called to allow memory allocation on a program unit (PU)
 * basis in the back-end.  Will start a new memory region with the 
 * subsequent _mk_.  No nesting of PUs are allowed.  When all entries 
 * for the PU have been made, return to earlier memory region by 
 * calling "DST_end_PU()".
*/
extern void DST_begin_PU(void);


extern DST_STR_IDX DST_mk_string(const char *s);


/* Must be called after all symbols local to a PU have been created
 * and before any more file-scope symbols are created.
*/
extern void DST_end_PU(void);



/* Creates a DW_TAG_inlined_subroutine entry and returns its idx.
*/
extern DST_INFO_IDX 
DST_mk_inlined_subroutine(ST_IDX low_pc,
			  ST_IDX high_pc,
			  DST_INFO_IDX abstract_origin,
			  DST_TYPE abstract_dst);


/* Creates a DW_TAG_subprogram entry and returns its idx.
*/
extern DST_INFO_IDX 
DST_mk_subprogram_memdef(USRCPOS      decl,  /* Source location */
			 ST_IDX	subpr, 
			 BOOL         is_prototyped,
			 DST_INFO_IDX spec); /* decl in class */


/* Creates a DW_TAG_subprogram entry and returns its idx
 * (for a regular declaration/definition).
*/
extern DST_INFO_IDX 
DST_mk_subprogram(USRCPOS      decl,
		  char        *name,
		  DST_INFO_IDX type,
		  DST_INFO_IDX origin,
		  ST_IDX	subpr,
		  DST_inline   inlin,
		  DST_virtuality virtuality,
		  DST_vtable_elem_location vtable_elem_location,
		  BOOL         is_declaration,
		  BOOL         is_prototyped,
#ifdef KEY      
                  BOOL         is_artificial,
#endif
		  BOOL         is_external);

/* Adds a pubname pseudo-attribute to the given subprogram.  Called for C++
 * member functions whose name in pubnames is class::function
*/
extern void
DST_add_pubname_to_subprogram (DST_INFO_IDX subprogram,
			       char        *pubname);

/* Adds a linkage_name (i.e. a mangled name) attribute to the given 
 * subprogram.  Called for C++ functions.
*/
extern void
DST_add_linkage_name_to_subprogram(DST_INFO_IDX subprogram,
				   char        *linkage_name);


/* Adds a DW_AT_specification attribute linking the body definition of a
 * member function to its definition within a class.
*/
extern void
DST_add_specification_to_subprogram (DST_INFO_IDX subprogram_def,
				     DST_INFO_IDX subprogram_decl);

/* Adds a DW_AT specification attribute linking the definition of a static
 * member variable to its declaration within a class.
 */
extern void
DST_add_specification_to_variable  (DST_INFO_IDX variable_def,
				    DST_INFO_IDX field_decl);

#ifdef KEY
/* Adds a linkage_name (i.e. a mangled name) attribute to the given 
 * subprogram.  Called for C++ functions.
*/
extern void
DST_add_linkage_name_to_variable(DST_INFO_IDX variable_def,
				   char        *linkage_name);
#endif

/* Creates a DW_TAG_entry_point and returns its idx
 * (Fortran specific)
*/
extern DST_INFO_IDX 
DST_mk_entry_point(USRCPOS      decl,
		  char        *name,
		  DST_INFO_IDX type,
		  ST_IDX	subpr);


/* create a DW_TAG_common_block entry and return its index 
 *
*/
DST_INFO_IDX 
DST_mk_common_block(char	*name,
		    ST_IDX	subpr);

/* create a DW_TAG_common_inclusion and return its idx 
 *
*/
DST_INFO_IDX 
DST_mk_common_incl( USRCPOS      decl,
		    DST_INFO_IDX comblk);

#ifdef KEY /* Bug 3507 */
/* create a DW_TAG_imported_declaration and return its idx
*/
DST_INFO_IDX
DST_mk_imported_decl( char *mangled_name,
                    char *name);
DST_INFO_IDX
DST_mk_module( USRCPOS decl,
               char    *name);
#endif /* KEY Bug 3507 */

/* Creates a DW_TAG_lexical_block entry and returns its idx.
*/
extern DST_INFO_IDX 
DST_mk_lexical_block(char        *name,         /* NULL if unnamed */
		     ST_IDX 	low_pc,
                     ST_IDX 	high_pc,
		     DST_INFO_IDX abstract_origin); /* NULL if none */



/* Creates a DW_TAG_label entry and returns its idx.
*/
extern DST_INFO_IDX 
DST_mk_label(char         *name,             /* NULL if unnamed */
	     ST_IDX	low_pc,
	     DST_INFO_IDX  abstract_origin); /* NULL if none */



/* Creates a DW_TAG_variable entry for a constant variable and returns
 * its idx.  Only use this if the constant variable has no allocated
 * memory location.
*/
extern DST_INFO_IDX 
DST_mk_variable_const(USRCPOS         decl,    /* Source location */
		      char           *name,    /* Name of const variable */
		      DST_INFO_IDX    type,    /* Type of const variable */
                      BOOL            is_automatic,
                      BOOL            is_external,
		      DST_CONST_VALUE cval);   /* Constant value */


/* creates a DW_TAG+variable entry for a variable within a common 
 * block. feptr is a front end pointer to the common block, offset
 * is an offset from the beginning of the common block.
*/
extern DST_INFO_IDX 
DST_mk_variable_comm( USRCPOS         decl, /* Source location */
		      char           *name,    /* Name of const variable */
		      DST_INFO_IDX    type,    /* Type of const variable */
		      ST_IDX	var,
		      UINT64 	      offset);    /* offset from common block */


/* Creates a DW_TAG_variable entry for the definition for a member of
 * class and returns its idx.
*/
extern DST_INFO_IDX 
DST_mk_variable_memdef(USRCPOS      decl,  /* Source location */
		       ST_IDX	var,
		       DST_INFO_IDX spec); /* Class member decl */



/* Creates a DW_TAG_variable entry for the definition/declaration
 * for other forms of variables than those handled above.
*/
extern DST_INFO_IDX 
DST_mk_variable(USRCPOS      decl,     /* Source location */
		char        *name,     /* Name of variable */
		DST_INFO_IDX type,     /* Type of variable */
		UINT64	     offs,     /* offset from front end variable */
		ST_IDX	     var,      /* symbol */
		DST_INFO_IDX abstract_origin, /* For inlined proc */
		BOOL         is_declaration,
		BOOL         is_automatic,
		BOOL         is_external,
		BOOL	     is_artificial);



/* Creates a DW_TAG_formal_parameter entry.
*/
extern DST_INFO_IDX 
DST_mk_formal_parameter(USRCPOS     decl,          /* Source location */
			char        *name,          /* Name of parm */
			DST_INFO_IDX type,          /* Type of parm */
			ST_IDX	    parm,          /* symbol */
                        DST_INFO_IDX abstract_origin, /* For inlined proc */
			DST_INFO_IDX default_val,   /* (C++) param value */
			BOOL         is_optional,   /* Optional param (C++) */
			BOOL         is_variable,  /* Variable param  */
				     // is_variable TRUE where the 
				     // language says a change
				     // in the argument changes the caller
				     // value. FALSE for C, C++.
			BOOL 	     is_artificial, // compiler inserted
			BOOL       is_declaration_only); // Param is 
					// declaration
					// because function is decl only.

/* Creates a DW_TAG_unspecified_parameters entry.
*/
extern DST_INFO_IDX 
DST_mk_unspecified_parameters(USRCPOS     decl,         /* Source location */
                              DST_INFO_IDX abstract_origin); /* For inlined */



/* Creates a DW_TAG_constant entry.
*/
DST_INFO_IDX 
DST_mk_constant_def(USRCPOS         decl,  /* Source location */
		    char           *name,  /* Name of constant */
		    DST_INFO_IDX    type,  /* Type of constant */
		    DST_CONST_VALUE cval,  /* Value of constant */
		    BOOL            is_external);  /* External? */



/* Creates a DW_TAG_constant entry.
*/
extern DST_INFO_IDX 
DST_mk_constant_decl(USRCPOS      decl,  /* Source location */
		     char        *name,  /* Name of constant */
		     DST_INFO_IDX type,  /* Type of constant */
		     BOOL         is_external);  /* External? */



/* Creates a DW_TAG_basetype entry.
*/
extern DST_INFO_IDX 
DST_mk_basetype(const char      *name,      /* Name of type */
		DST_ATE_encoding encoding,  /* How to encode/interpret data */
		DST_size_t       byte_size);/* Size of object */



/* Creates a DW_TAG_const_type entry.
*/
extern DST_INFO_IDX 
DST_mk_const_type(DST_INFO_IDX type);   /* Qualified type */



/* Creates a DW_TAG_volatile_type entry.
*/
extern DST_INFO_IDX 
DST_mk_volatile_type(DST_INFO_IDX type);  /* Qualified type */



/* Creates a DW_TAG_pointer_type entry.
*/
extern DST_INFO_IDX 
DST_mk_pointer_type(DST_INFO_IDX   type,          /* Type pointed to */
		    DST_addr_class address_class, /* How to dereference */
		    DST_size_t     byte_size);    /* Size */



/* Creates a DW_TAG_reference_type entry.
*/
extern DST_INFO_IDX 
DST_mk_reference_type(DST_INFO_IDX   type,          /* Type pointed to */
		      DST_addr_class address_class, /* How to dereference */
		      DST_size_t     byte_size);    /* Size */



/* Creates a DW_TAG_typedef entry.
*/
extern DST_INFO_IDX
DST_mk_typedef(USRCPOS      decl,             /* Source location */
	       char        *name,             /* Name of type */
	       DST_INFO_IDX type,             /* Defining type */
               DST_INFO_IDX abstract_origin); /* In scope of inlined proc */

/* Creates a DW_TAG_ptr_to_member_type entry.
*/
extern DST_INFO_IDX
DST_mk_ptr_to_member_type(USRCPOS      decl,        /* Source location */
			  char	      *name,        /* Name of type */
			  DST_INFO_IDX type,	    /* Type of member */
			  DST_INFO_IDX class_type); /* Type of class */

/* Creates a DW_TAG_array_type entry.
*/
extern DST_INFO_IDX
DST_mk_array_type(USRCPOS      decl,      /* Source location */
		  char        *name,      /* Name of type */
		  DST_INFO_IDX type,      /* Element type */
		  DST_size_t   byte_size, /* Size of array, if known */
		  DST_INFO_IDX abstract_origin, /* In scope of inlined */
		  BOOL         is_incomplete); /* Incomplete array */



/* Creates a DW_TAG_subrange_type entry.
*/
extern DST_INFO_IDX
DST_mk_subrange_type(DST_flag is_lb_cval,
		     DST_cval_ref low, 		/* lower bound */
		     DST_flag is_ub_cval,
		     DST_cval_ref high); 	/* upper bound */



/* Creates a DW_TAG_string_type entry.
*/
extern DST_INFO_IDX
DST_mk_string_type(USRCPOS	   decl,	/* Source location */
		   char		  *name,	/* Name of type */
		   DST_flag	   is_len_cval,	/* if length is a constant */
		   DST_cval_ref	   len);	/* Length of string */



/* Creates a DW_TAG_structure_type entry.
*/
extern DST_INFO_IDX
DST_mk_structure_type(USRCPOS      decl,      /* Source location */
		      char        *name,      /* Name of type */
		      DST_size_t   byte_size, /* Size of struct, if known */
		      DST_INFO_IDX abstract_origin,
		      BOOL         is_incomplete);



/* Creates a DW_TAG_union_type entry.
*/
extern DST_INFO_IDX
DST_mk_union_type(USRCPOS      decl,      /* Source location */
		  char        *name,      /* Name of type */
		  DST_size_t   byte_size, /* Size of union, if known */
		  DST_INFO_IDX abstract_origin,
		  BOOL         is_incomplete);



/* Creates a DW_TAG_class_type entry.
*/
extern DST_INFO_IDX
DST_mk_class_type(USRCPOS      decl,      /* Source location */
		  char        *name,      /* Name of type */
		  DST_size_t   byte_size, /* Size of class, if known */
		  DST_INFO_IDX abstract_origin,
		  BOOL         is_incomplete);



/* Creates a DW_TAG_member entry.  Note that memb_loc is the field-
 * offset within the struct for non-bitfields.  The byte_size, bit-
 * offset, and bit_size only apply to bitfields, while memb_loc is the
 * location of the block containing (properly aligned) the bitfield.
*/
#ifndef KEY
extern DST_INFO_IDX
DST_mk_member(USRCPOS      decl,       /* Source location */
	      char        *name,       /* Name of member */
              DST_INFO_IDX type,       /* Type of member */
	      DST_size_t   memb_loc,   /* Byte-offset of member container */
	      DST_size_t   byte_size,  /* Byte-size of container */
	      DST_bitsize  bit_offset, /* Offset within container */
	      DST_bitsize  bit_size,   /* Size of bitfield member; will be
                                          zero for non-bitfield members. */
	      BOOL         is_bitfield,
	      BOOL         is_static,
	      BOOL         is_declaration,
	      BOOL	   is_artificial);
#else
extern DST_INFO_IDX
DST_mk_member(USRCPOS      decl,       /* Source location */
	      char        *name,       /* Name of member */
              DST_INFO_IDX type,       /* Type of member */
	      DST_size_t   memb_loc,   /* Byte-offset of member container */
	      DST_size_t   byte_size,  /* Byte-size of container */
	      DST_bitsize  bit_offset, /* Offset within container */
	      DST_bitsize  bit_size,   /* Size of bitfield member; will be
                                          zero for non-bitfield members. */
	      BOOL         is_bitfield,
	      BOOL         is_static,
	      BOOL         is_declaration,
	      BOOL	   is_artificial,
	      DST_accessibility accessibility=DW_ACCESS_public);
#endif



/* Creates a DW_TAG_inheritance entry.
*/
#ifndef KEY
extern DST_INFO_IDX
DST_mk_inheritance(USRCPOS      decl,         /* Source location */
		   DST_INFO_IDX type,         /* Type of member */
		   DST_virtuality virtuality, /* AT_virtuality code */
		   DST_size_t   memb_loc);    /* Byte-offset of member container */
#else
extern DST_INFO_IDX
DST_mk_inheritance(USRCPOS      decl,         /* Source location */
		   DST_INFO_IDX type,         /* Type of member */
		   DST_virtuality virtuality, /* AT_virtuality code */
		   DST_size_t   memb_loc,     /* Byte-offset of member container */
		   DST_accessibility accessibility);/* AT_accessibility */
#endif

/* Creates a DW_TAG_template_type_parameter entry.
*/
extern DST_INFO_IDX
DST_mk_template_type_parameter(USRCPOS      decl,     /* Source location */
			       char        *name,     /* Name of formal */
			       DST_INFO_IDX type);    /* Actual type */

/* Creates a DW_TAG_template_value_parameter entry.
*/
extern DST_INFO_IDX
DST_mk_template_value_parameter(USRCPOS         decl,     /* Source location */
			        char           *name,     /* Name of formal */
 			        DST_INFO_IDX    type,     /* Actual type */
			        DST_CONST_VALUE cval);    /* Actual type */


/* Creates a DW_TAG_enumeration_type entry.
*/
extern DST_INFO_IDX
DST_mk_enumeration_type(USRCPOS      decl,      /* Source location */
			char        *name,      /* Name of type */
			DST_size_t   byte_size, /* Size, if known */
			DST_INFO_IDX abstract_origin, /* In inlined inst. */
			BOOL         is_incomplete);



/* Creates a DW_TAG_enumerator entry.
*/
extern DST_INFO_IDX 
DST_mk_enumerator(USRCPOS         decl,  /* Source location */
		  char           *name,  /* Name of enumerator */
		  DST_CONST_VALUE cval); /* Value of enumerator */



/* Creates a DW_TAG_subroutine_type entry, with the parameter types
 * as children.
*/
extern DST_INFO_IDX
DST_mk_subroutine_type(USRCPOS      decl,            /* Source location */
		       char        *name,            /* Name */
		       DST_INFO_IDX type,            /* Return type */
		       DST_INFO_IDX abstract_origin, /* In inlined instance */
		       BOOL         is_prototyped);

#ifdef KEY
/* Creates a DW_TAG_namelist entry, with the namelist items as children.
 */
extern DST_INFO_IDX
DST_mk_namelist(USRCPOS      decl,            /* Source location */
		char        *name             /* Name */);

/* Creates a DW_TAG_namelist_items entry.
 */
extern DST_INFO_IDX
DST_mk_namelist_item(USRCPOS      decl,            /* Source location */
		     char        *name             /* Name */);
#endif

#if defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER)
extern void
DST_label_add_name(DST_LABEL *, char *);

extern void
DST_lexical_block_add_name(DST_LEXICAL_BLOCK *, char *);

extern char *
DST_get_file(mUINT16 , UINT64 *, UINT64 *, char **);

extern char *
DST_get_dirname(mUINT16);

extern void
DST_subprogram_concrete_to_abstract(DST_INFO_IDX);

extern mUINT16 
DST_get_cross_inlined_file_id(char 	*,
			      char 	*,
			      UINT64,
			      UINT64);

extern DST_INFO_IDX 
DST_mk_cross_inlined_subroutine(
                 ST_IDX,            
                 ST_IDX,   
                 char          *,     
                 mUINT16       *,        
                 UINT64        ,         
                 UINT64        ,        
                 USRCPOS       ,        
                 char          *,      
                 char          *,
                 DST_INFO_IDX  ,
                 DST_TYPE);     
#endif

#if defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER) || defined(_LEGO_CLONER)
extern DST_INFO_IDX
DST_mk_cloned_subprogram(USRCPOS,
                  char        *,
                  DST_INFO_IDX ,
                  DST_INFO_IDX ,
                  ST_IDX       ,   /* front-end st */
                  DST_inline   ,
                  DST_virtuality );

#endif

#ifdef __cplusplus
}
#endif
#endif /* dwarf_DST_producer_INCLUDED */

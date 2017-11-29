/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


static char USMID[] = "@(#) libcif/cifgetrec.c	30.18	06/27/97 14:34:02";


/* -------------------------------------------------------------------------
 * CIF record retrieval and conversion routines
 * Tabs are set up to be read with tab spacing = 3
 * --------------------------------------------------------------------------
 */

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#if defined(BUILD_OS_DARWIN)
#include <stdlib.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <malloc.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cif_int.h"

/* --- function pointer array - ASCII format conversion routines --- */

static int ascii_callsite __((struct Cif_callsite *));
static int ascii_cifhdr __((struct Cif_cifhdr *));
static int ascii_comblk __((struct Cif_comblk *));
static int ascii_const __((struct Cif_const *));
static int ascii_entry __((struct Cif_entry *));
static int ascii_file __((struct Cif_file *));
static int ascii_loop __((struct Cif_loop *));
static int ascii_include __((struct Cif_include *));
static int ascii_label __((struct Cif_label *));
static int ascii_message __((struct Cif_message *));
static int ascii_namelist __((struct Cif_namelist *));
static int ascii_object __((struct Cif_object *));
static int ascii_srcfile __((struct Cif_srcfile *));
static int ascii_summary __((struct Cif_summary *));
static int ascii_unit __((struct Cif_unit *));
static int ascii_endunit __((struct Cif_endunit *));
static int ascii_usage __((struct Cif_usage *));
static int ascii_f90_usage __((struct Cif_usage *));
static int ascii_nd_msg __((struct Cif_nd_msg *));
static int ascii_edopts __((struct Cif_edopts *));
static int ascii_mach_char __((struct Cif_mach_char *));
static int ascii_misc_opts __((struct Cif_misc_opts *));
static int ascii_opt_opts __((struct Cif_opt_opts *));
static int ascii_stmt_type __((struct Cif_stmt_type *));
static int ascii_transform __((struct Cif_transform *));

static int ascii_cdir __((struct Cif_cdir *));
static int ascii_cdir_doshared __((struct Cif_cdir_doshared *));
static int ascii_geometry __((struct Cif_geometry *));
static int ascii_continuation __((struct Cif_continuation *));

static int ascii_c_tag __((struct Cif_c_tag *));
static int ascii_c_opts __((struct Cif_c_opts *));
static int ascii_c_message __((struct Cif_c_message *));
static int ascii_c_const __((struct Cif_c_const *));
static int ascii_c_entry __((struct Cif_c_entry *));
static int ascii_c_object __((struct Cif_c_object *));
static int ascii_c_entry_end __((struct Cif_c_entry_end *));
static int ascii_c_lint_directive __((struct Cif_c_lint_directive *));
static int ascii_c_macro_def __((struct Cif_c_macro_def *));
static int ascii_c_macro_undef __((struct Cif_c_macro_undef *));
static int ascii_c_macro_usage __((struct Cif_c_macro_usage *));

#ifndef CRAY2
static int ascii_f90_callsite __((struct Cif_f90_callsite *));
static int ascii_f90_comblk __((struct Cif_f90_comblk *));
static int ascii_f90_const __((struct Cif_f90_const *));
static int ascii_f90_entry __((struct Cif_f90_entry *));
static int ascii_f90_loop __((struct Cif_f90_loop *));
static int ascii_f90_derived_type __((struct Cif_f90_derived_type *));
static int ascii_f90_label __((struct Cif_f90_label *));
static int ascii_f90_namelist __((struct Cif_f90_namelist *));
static int ascii_f90_object __((struct Cif_f90_object *));
static int ascii_f90_misc_opts __((struct Cif_f90_misc_opts *));
static int ascii_f90_opt_opts __((struct Cif_f90_opt_opts *));
static int ascii_f90_begin_scope __((struct Cif_f90_begin_scope *));
static int ascii_f90_end_scope __((struct Cif_f90_end_scope *));
static int ascii_f90_scope_info __((struct Cif_f90_scope_info *));
static int ascii_f90_use_module __((struct Cif_f90_use_module *));
static int ascii_f90_rename __((struct Cif_f90_rename *));
static int ascii_f90_int_block __((struct Cif_f90_int_block *));
static int ascii_f90_vectorization __((struct Cif_f90_vectorization *));

static int ascii_BE_node __((struct Cif_BE_node *));
static int ascii_BE_fid __((struct Cif_BE_fid *));
static int ascii_cc_type __((struct Cif_cc_type *));
static int ascii_cc_entry __((struct Cif_cc_entry *));
static int ascii_cc_obj __((struct Cif_cc_obj *));
static int ascii_cc_subtype __((struct Cif_cc_subtype *));
static int ascii_cc_enum __((struct Cif_cc_enum *));
static int ascii_cc_expr __((struct Cif_cc_expr *));
static int ascii_src_pos __((struct Cif_src_pos *));
static int ascii_orig_cmd __((struct Cif_orig_cmd *));
#endif

static int (*ascii_record[CIF_MAXRECORD]) () = {
	0,									/* 00= */
	ascii_callsite,				/* 01= CIF_CALLSITE */
	ascii_cifhdr,			       	/* 02= CIF_CIFHDR */
	ascii_comblk,			       	/* 03= CIF_COMBLK */
	ascii_const,			       	/* 04= CIF_CONST */
	ascii_cdir,    			       	/* 05= CIF_CDIR */
	ascii_entry,			       	/* 06= CIF_ENTRY */
	ascii_file,			        /* 07= CIF_FILE */
	ascii_loop,			       	/* 08= CIF_LOOP */
	ascii_include,			       	/* 09= CIF_INCLUDE */
	ascii_label,			       	/* 10= CIF_LABEL */
	ascii_message,			       	/* 11= CIF_MESSAGE */
	ascii_namelist,				/* 12= CIF_NAMELIST */
	ascii_object,			       	/* 13= CIF_OBJECT */
	ascii_srcfile,			        /* 14= CIF_SRCFILE */
	ascii_summary,			       	/* 15= CIF_SUMMARY */
	ascii_cdir_doshared,   	       		/* 16= CIF_CDIR_DOSHARED*/
	ascii_unit,			        /* 17= CIF_UNIT */
	ascii_endunit,			       	/* 18= CIF_ENDUNIT */
	ascii_usage,			       	/* 19= CIF_USAGE */
	ascii_nd_msg,			       	/* 20= CIF_ND_MSG */
	ascii_edopts,			       	/* 21= CIF_EDOPTS */
	ascii_mach_char,		       	/* 22= CIF_MACH_CHAR */
	ascii_misc_opts,		       	/* 23= CIF_MISC_OPTS */
	ascii_opt_opts,				/* 24= CIF_OPT_OPTS */
	ascii_stmt_type,		       	/* 25= CIF_STMT_TYPE */
	ascii_geometry,				/* 26= CIF_GEOMETRY */
	ascii_continuation,			/* 27= CIF_CONTINUATION */
#ifndef CRAY2
	ascii_f90_callsite,    			/* 28= CIF_F90_CALLSITE */
	ascii_f90_comblk,	       		/* 29= CIF_F90_COMBLK */
	ascii_f90_const,	       		/* 30= CIF_F90_CONST */
	ascii_f90_entry,	       		/* 31= CIF_F90_ENTRY */
	ascii_f90_loop,				/* 32= CIF_F90_LOOP */
	ascii_f90_derived_type,	       		/* 33= CIF_F90_DERIVED_TYPE */
	ascii_f90_label,	       		/* 34= CIF_F90_LABEL */
	ascii_f90_namelist,	       		/* 35= CIF_F90_NAMELIST */
	ascii_f90_object,	       		/* 36= CIF_F90_OBJECT */
	ascii_f90_misc_opts,	       		/* 37= CIF_F90_MISC_OPTS */
	ascii_f90_opt_opts,	       		/* 38= CIF_F90_OPT_OPTS */
	ascii_f90_begin_scope,	       		/* 39= CIF_F90_BEGIN_SCOPE */
	ascii_f90_end_scope,	       		/* 40= CIF_F90_END_SCOPE */
	ascii_f90_scope_info,	       		/* 41= CIF_F90_SCOPE_INFO */
	ascii_f90_use_module,	       		/* 42= CIF_F90_USE_MODULE */
	ascii_f90_rename,	       		/* 43= CIF_F90_RENAME */
	ascii_f90_int_block,	       		/* 44= CIF_F90_INT_BLOCK */
	ascii_f90_vectorization,       		/* 45= CIF_F90_VECTORIZATION */
	ascii_BE_node,	 			/* 46= CIF_BE_NODE */
#else
	NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL,
	NULL, NULL, NULL,
#endif /* ndef CRAY2 */
	ascii_transform,			/* 47= CIF_TRANSFORM */
	0,0,					/* 48-49 */
	ascii_BE_fid,	 			/* 50= CIF_BE_FID */
	ascii_c_tag,			       	/* 51= CIF_C_TAG */
	ascii_c_opts,			       	/* 52= CIF_C_OPTS */
	ascii_c_message,		       	/* 53= CIF_C_MESSAGE */
	ascii_c_const,			       	/* 54= CIF_C_CONST */
	ascii_c_entry,			       	/* 55= CIF_C_ENTRY */
	ascii_c_object,				/* 56= CIF_C_OBJECT */
	ascii_c_lint_directive,			/* 57= CIF_C_LINT_DIRECTIVE */
	ascii_c_macro_def,    			/* 58= CIF_C_MACRO_DEF */
	ascii_c_macro_undef,   			/* 59= CIF_C_MACRO_UNDEF */
	ascii_c_macro_usage,   			/* 60= CIF_C_MACRO_USAGE */
	ascii_c_entry_end,    			/* 61= CIF_C_ENTRY_END */
	0,0,0,0,0,0,0,0,			/* 62-69 */
	ascii_orig_cmd,				/* 70= CIF_ORIG_CMD */
	0,0,0,0,0,0,0,0,0,			/* 71-79 */
	ascii_cc_type,				/* 80 = CIF_CC_TYPE */
	ascii_cc_entry,				/* 81 = CIF_CC_ENTRY */
	ascii_cc_obj,				/* 82 = CIF_CC_OBJ */
	ascii_cc_subtype,			/* 83 = CIF_CC_SUBTYPE */
	ascii_cc_enum,				/* 84 = CIF_CC_ENUM */
	ascii_cc_expr,				/* 85 = CIF_CC_EXPR */
	ascii_src_pos				/* 86 = CIF_SRC_POS */


};



/* --- valid binary record indicators --- */
static short valid_record[CIF_MAXRECORD] = {
	NO,		/* 00= */
	YES,		/* 01= CIF_CALLSITE */
	YES,		/* 02= CIF_CIFHDR */
	YES,		/* 03= CIF_COMBLK */
	YES,		/* 04= CIF_CONST */
	YES,		/* 05= CIF_CDIR */
	YES,		/* 06= CIF_ENTRY */
	YES,		/* 07= CIF_FILE */
	YES,		/* 08= CIF_LOOP */
	YES,		/* 09= CIF_INCLUDE */
	YES,		/* 10= CIF_LABEL */
	YES,		/* 11= CIF_MESSAGE */
	YES,		/* 12= CIF_NAMELIST */
	YES,		/* 13= CIF_OBJECT */
	YES,		/* 14= CIF_SRCFILE */
	YES,		/* 15= CIF_SUMMARY */
	YES,		/* 16= CIF_CDIR_DOSHARED */
	YES,		/* 17= CIF_UNIT */
	YES,		/* 18= CIF_ENDUNIT */
	YES,		/* 19= CIF_USAGE */
	YES,		/* 20= CIF_ND_MSG */
	YES,		/* 21= CIF_EDOPTS */
	YES,		/* 22= CIF_MACH_CHAR */
	YES,		/* 23= CIF_MISC_OPTS */
	YES,		/* 24= CIF_OPT_OPTS */
	YES,		/* 25= CIF_STMT_TYPE */
	YES,		/* 26= CIF_GEOMETRY */
	YES,		/* 27= CIF_CONTINUATION */
	YES,		/* 28= CIF_F90_CALLSITE */
	YES,		/* 29= CIF_F90_COMBLK */
	YES,		/* 30= CIF_F90_CONST */
	YES,		/* 31= CIF_F90_ENTRY */
	YES,		/* 32= CIF_F90_LOOP */
	YES,		/* 33= CIF_F90_DERIVED_TYPE */
	YES,		/* 34= CIF_F90_LABEL */
	YES,		/* 35= CIF_F90_NAMELIST */
	YES,		/* 36= CIF_F90_OBJECT */
	YES,		/* 37= CIF_F90_MISC_OPTS */
	YES,		/* 38= CIF_F90_OPT_OPTS */
	YES,		/* 39= CIF_F90_BEGIN_SCOPE */
	YES,		/* 40= CIF_F90_END_SCOPE */
	YES,		/* 41= CIF_F90_SCOPE_INFO */
	YES,		/* 42= CIF_F90_USE_MODULE */
	YES,		/* 43= CIF_F90_RENAME */
	YES,		/* 44= CIF_F90_INT_BLOCK */
	YES,		/* 45= CIF_F90_VECTORIZATION */
	YES,		/* 46= CIF_BE_NODE */
	YES, 		/* 47 = CIF_TRANSFORM */
	YES,		/* 48 = CIF_FILEDIR */
	YES,		/* 49 = CIF_UNITDIR */
	YES,		/* 50 = CIF_BE_FID */
	YES,		/* 51= CIF_C_TAG */
	YES,		/* 52= CIF_C_OPTS */
	YES,		/* 53= CIF_C_MESSAGE */
	YES,		/* 54= CIF_C_CONST */
	YES,		/* 55= CIF_C_ENTRY */
	YES,		/* 56= CIF_C_OBJECT */
	YES,		/* 57= CIF_C_LINT_DIRECTIVE */
	YES,		/* 58= CIF_C_MACRO_DEF */
	YES,		/* 59= CIF_C_MACRO_UNDEF */
	YES,		/* 60= CIF_C_MACRO_USAGE */
	YES,		/* 61= CIF_C_ENTRY_END */
	NO, NO, NO, NO, NO, NO, NO, NO, /* 62-69 */
	YES,		/* 70= CIF_ORIG_CMD */
	NO, NO, NO, NO, NO, NO, NO, NO, NO, /* 71-79 */
	YES,		/* 80= CIF_CC_TYPE */
	YES,		/* 81= CIF_CC_ENTRY */
	YES,		/* 82= CIF_CC_OBJ */
	YES,		/* 83= CIF_CC_SUBTYPE */
	YES,		/* 84= CIF_CC_ENUM */
	YES,		/* 85= CIF_CC_EXPR */
	YES		/* 86= CIF_SRC_POS */

};

/* Buffer to use when mapping a version 1 cif to a version 2 cif record.
 * Required because some records increased in size so reading a v2 record
 * into space for a v1 cif would not work. Data is read into this buffer and
 * shaped into the correct cif record structure that the application requested.
 */
struct Cif_generic *_cif_map_buffer = (struct Cif_generic *) NULL;


static int lcifd;					/* cif descriptor for current invocation */
static int lmode;					/* memory mgmt mode for current invocation */

static int binary_record __((struct Cif_generic **, FILE *));

/*
 * We need this so that when returning a v1 cif, we do not
 * return the file record associated with the message catalog;
 * when we hit the file that matches this, it will not be returned.
 */

static int global_msgfid = -1;

/* --------------------------------------------------------------------------
 * ASCII record token scanning stuff 
 *
 * The "token" routine returns a pointer to the next SEPARATOR or "\n"
 * delimited token in the current record.  "delim" is set to the character
 * that terminated the token.  The routine is initialized to begin a new
 * record by setting "ntoken" to the record buffer address.
 * --------------------------------------------------------------------------
 */

static char *ntoken;				/* pointer to next token in buffer */
static char delim;				/* character that terminated current token */
static char *token () {
	char *tok;

	if (*ntoken == '\0')
		return ((char *)NULL);
	else {
		tok = ntoken;
		delim = *ntoken++;
		while (delim != SEPARATOR && delim != '\n' && delim != '\0')
			delim = *ntoken++;
		*(ntoken-1) = '\0';
		return (tok);
	}
}


/* --------------------------------------------------------------------------
 * "compuse" compares two usage records by file, line, and charpos.  It is
 * passed to "qsort" to sort usage records.
 * --------------------------------------------------------------------------
 */

static int compuse (u1, u2)
struct Cif_use *u1, *u2;
{
	int ret;

	if ((ret = ( u1->fid - u2->fid )) != 0)
		return (ret);
	else if ((ret = ( u1->line - u2->line )) != 0)
		return (ret);
	else
		return ( u1->cpos - u2->cpos );
}

/* --------------------------------------------------------------------------
 *
 * Cif_Getrecord returns the next record from a CIF file.  If the file is an
 * ASCII format file, the next record from the file is read and converted
 * into structure format.  If a binary format file, the next record is read
 * from the file.  In both cases, the neccessary space to contain the
 * structure and associated information is acquired.  The status value is
 * returned via the function return value.  The pointer to the structure is
 * returned by setting the "cif_record" argument to the structure pointer.
 *
 * --------------------------------------------------------------------------
 */

int Cif_Getrecord
#ifdef __STDC__
(int cifd, struct Cif_generic **cif_record)
#else
(cifd, cif_record)
int cifd;									/* input CIF file descriptor */
struct Cif_generic **cif_record;		/* pointer to pointer to CIF structure */
#endif
{

	int rtype;						/* record type code */
	int status = 0;				/* status value */

	if (cifd < 0 || cifd >= CIF_FT_SIZE || _Cif_filetbl[cifd].form == NOT_A_CIF)
		return (CIF_NOTOPEN);
	else if (_Cif_filetbl[cifd].optype == 'w')
		return (CIF_BADREQ);
	lcifd = cifd;

	/*
	 * If memory management mode isn't set, then set to FIXED.  If the mode is
	 * FIXED, reset the amount of buffer used.
	 */

	lmode = _Cif_filetbl[cifd].mode;
	if (lmode == CIF_MEM_DEFAULT) {
		if ((status = Cif_Memmode (cifd, CIF_MEM_FIXED)) != 0)
			return (status);
		lmode = _Cif_filetbl[cifd].mode;
	}
	if (lmode == CIF_MEM_FIXED)
		_Cif_memarea[_Cif_filetbl[cifd].fme].mused = 0;

	if (_Cif_filetbl[cifd].form == ASCII_CIF) {

		/* Read next record from file.  Continue reading records until a
		 * record that should be returned is encountered.  The record must
		 * legal for this CIF version and must be allowed by the record
		 * mask for this file.
		 */

	  do {
		do {
			if (_Cif_filetbl[cifd].ifull == NO) {
				if (fgets(_Cif_filetbl[cifd].ip, CIF_BUFSIZE, _Cif_filetbl[cifd].fd)
					== NULL)
				{
					if (feof (_Cif_filetbl[cifd].fd))
						return (CIF_EOF);
					else
						return (CIF_SYSERR);
				}
			}
			_Cif_filetbl[cifd].ifull = NO;
			ntoken = _Cif_filetbl[cifd].ip;
			rtype = atoi (token ());
		} while (rtype >= CIF_MAXRECORD || ascii_record[rtype] == 0 ||
		         _Cif_filetbl[cifd].rmask[rtype] == '\0');

		/* Allocate space for record structure and call conversion
		 * routine based on record type.
		 */

		*cif_record = (struct Cif_generic *) _Cif_space[lmode]
		          (_Cif_structsize[rtype][_Cif_filetbl[cifd].return_version], lcifd);
		if (*cif_record  == NULL)
			status = CIF_NOMEM;
		else {
			(void) memset ((char *)*cif_record,
				       '\0',
				       _Cif_structsize
				       		[rtype][_Cif_filetbl[cifd].return_version]);

			(*cif_record)->rectype = rtype;
			status = ascii_record[rtype] (*cif_record);

		}
		/*
		 * If status == maxrecord, we don't want to return this record.
		 * This can only happen if we are returning a v1 cif, but reading a v2
		 * and it is a stmt record (#25) with type CDIR; v1 didn't have that
		 * record
		 */
		if (status == CIF_MAXRECORD &&
		    lmode == CIF_MEM_INDIV)
		  Cif_Free(*cif_record);

	      } while (status == CIF_MAXRECORD);
	}

	else 		/* form is BINARY_CIF so */
		status = binary_record (cif_record, _Cif_filetbl[cifd].fd);

	return (status);
}




/* --------------------------------------------------------------------------
 * Binary record version mapping
 *
 * Converts a binary record from one version to another
 * --------------------------------------------------------------------------
 */
int _Cif_binary_map_version (rtype, map_buffer, cr)
int rtype;
struct Cif_generic *map_buffer;
struct Cif_generic *cr;
{

  switch (rtype) {

  case CIF_OBJECT : {
    struct Cif_object *to = (struct Cif_object *) cr;
    struct Cif_object *from = (struct Cif_object *) map_buffer;

    /*
     * copy the required bytes to fill the return version; if there are not
     * enough bytes to copy, that's okay (ie if v1 copied to v2), the buffer
     * has sufficient space after it to avoid memory problems. If copying a
     * smaller version (v1) into a larger, that's also okay as we will set
     * the new geometry and distribution fields added in v2, below.
     */

    (void) memcpy((char *) cr, (char *) map_buffer,
	   _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);

    if (_Cif_filetbl[lcifd].return_version == 1) {
      if (from->storage == CIF_ST_DATA)  /* in v2, static was split into bss and data,
					  * but as bss has the same value as static (6),
					  * that's okay, so we just have to map back the
					  * data variant (8)
					  */
	to->storage = CIF_ST_STATIC;
    }
    else
	if (_Cif_filetbl[lcifd].version == 1) {
	  /* returning v2, add the extra distribution and geomid */
	  to->geomid = 0;
	  to->dist = 0;
	  to->pointer = 0;
	}

    break;
  }

  case CIF_CONST : {
    struct Cif_const *to = (struct Cif_const *) cr;

    /*
     * copy the required bytes to fill the return version; if there are not
     * enough bytes to copy, that's okay (ie if v1 copied to v2), the buffer
     * has sufficient space after it to avoid memory problems. If copying a
     * smaller version (v1) into a larger, that's also okay as we will set
     * the original form fields added in v2 below.
     */

    (void) memcpy((char *) cr, (char *) map_buffer,
	   _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);

    if (_Cif_filetbl[lcifd].version == 1 &&
	_Cif_filetbl[lcifd].return_version != 1) {
      /* returning v2, add the extra
       * original format fields
       */

      to->origform = 0;
      to->olen = 0;
      to->oform = (char *) NULL;
    }

    break;
  }


  case CIF_FILE : {
    struct Cif_file *to = (struct Cif_file *) cr;

    struct Cif_file *from = (struct Cif_file *) map_buffer;

    (void) memcpy((char *) cr, (char *) map_buffer,
	   _Cif_shortsize[rtype][_Cif_filetbl[lcifd].version]);
	   /* do NOT change this shortsize to "return_version"
	      because we need to pick up the onlen field */

    break;
  }

  case CIF_COMBLK : {
    struct Cif_comblk *to = (struct Cif_comblk *) cr;

    /*
     * copy the required bytes to fill the return version; if there are not
     * enough bytes to copy, that's okay (ie if v1 copied to v2), the buffer
     * has sufficient space after it to avoid memory problems. If copying a
     * smaller veriosn (v1) into a larger, that's also okay as we will set
     * the new distribution field added in v2, below.
     */

    (void) memcpy((char *) cr, (char *) map_buffer,
	   _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);

    if (_Cif_filetbl[lcifd].version == 1 &&
	_Cif_filetbl[lcifd].return_version != 1)  /* add the extra geomid */
      to->dist = 0;

    break;
  }

  case CIF_USAGE : {
    struct Cif_usage *to = (struct Cif_usage *) cr;

    /*
     * copy the required bytes to fill the return version; if there are not
     * enough bytes to copy, that's okay (ie if v1 copied to v2), the buffer
     * has sufficient space after it to avoid memory problems. If copying a
     * smaller version (v1) into a larger, that's also okay as we will set
     * the new nmembs and membs fields added in v2, below.
     */

    (void) memcpy((char *) cr, (char *) map_buffer,
	   _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);

    if (_Cif_filetbl[lcifd].version == 1 &&
	_Cif_filetbl[lcifd].return_version != 1) {
	to->nmembs = 0;
	to->membs = (long *) NULL;
    }

    break;
  }

  case CIF_MACH_CHAR : {
    struct Cif_mach_char *to = (struct Cif_mach_char *) cr;

    /*
     * copy the required bytes to fill the return version; if there are not
     * enough bytes to copy, that's okay (ie if v1 copied to v2), the buffer
     * has sufficient space after it to avoid memory problems. If copying a
     * smaller veriosn (v1) into a larger, that's also okay as we will set
     * the new distribution field added in v2, below.
     */

    (void) memcpy((char *) cr, (char *) map_buffer,
	   _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);

    if (_Cif_filetbl[lcifd].version == 1 &&
	_Cif_filetbl[lcifd].return_version != 1) { /* add the extra fields */
	to->numbanks = 0;	/* number of memory banks */
	to->numcpus = 0;	/* number of cpus */
	to->instbufsize = 0;	/* instruction buffer size */
	to->clockperiod = 0;	/* clock period in picoseconds */
	to->numclregs = 0;	/* number of register clusters */
	to->bankbusytime = 0;	/* number of clock periods that the memory bank is reserved */
    }

    break;
  }

  case CIF_C_MESSAGE : {
    struct Cif_c_message *to = (struct Cif_c_message *) cr;

    (void) memcpy((char *) cr, (char *) map_buffer,
	   _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);

    if (_Cif_filetbl[lcifd].version == 1 &&
	_Cif_filetbl[lcifd].return_version != 1)
	/* add in extra msgcode field */
	to->msgcode = 0;

    break;
  }

  case CIF_MISC_OPTS : {
    struct Cif_misc_opts *to = (struct Cif_misc_opts *) cr;

    (void) memcpy((char *) cr, (char *) map_buffer,
	   _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);

    if (_Cif_filetbl[lcifd].version == 1 &&
	_Cif_filetbl[lcifd].return_version != 1)  { /* add in extra v2 fields */
      to->llen = 0;
      to->cifopt = 0;
      to->inputlen = 0;
      to->runtime = 0;
      to->numincs = 0;
    }

    break;
  }


  case CIF_OPT_OPTS : {

    if (_Cif_filetbl[lcifd].return_version == 1) { /* map a v2 cif to pass to a v1 application */

      struct Cif_opt_opts *from = (struct Cif_opt_opts *) map_buffer;
      struct Cif_opt_opts_1 *to = (struct Cif_opt_opts_1 *) cr;

      to->values = from->values;

    } else if (_Cif_filetbl[lcifd].version == 1) {
	  /* a v1 cif to a v(>1) application */

	  struct Cif_opt_opts_1 *from = (struct Cif_opt_opts_1 *) map_buffer;
	  struct Cif_opt_opts *to = (struct Cif_opt_opts *) cr;

	  to->values = from->values;
	  to->inlevel = 0;  /* v1 cif didn't set this inline level field */

    } else {
	  /* map v2 to v3 or vice versa */
	  (void) memcpy((char *) cr, (char *) map_buffer,
	     _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);
    }
    break;
  }


  case CIF_C_ENTRY : {

    if (_Cif_filetbl[lcifd].return_version == 1) { /* map a v2 cif to pass to a v1 application */

      struct Cif_c_entry *from = (struct Cif_c_entry *) map_buffer;
      struct Cif_c_entry_1 *to = (struct Cif_c_entry_1 *) cr;

      to->rectype = from->rectype;
      to->ptype = from->ptype;
      if (from->symclass == 5)
	to->symclass = 0;
      else
	to->symclass = from->symclass;
      to->retvalue = from->retvalue;
      to->varargs = from->varargs;
      to->scope = from->scope;
      to->nlen = from->nlen;
      to->symid = from->symid;
      to->nargs = from->nargs;
      to->nmods = from->nmods;
      to->qual = from->qual;
      to->btype = from->btype;

    }
    else

      if (_Cif_filetbl[lcifd].version == 1) {
	/* a v1 cif to a v(>1) application */
	struct Cif_c_entry_1 *from = (struct Cif_c_entry_1 *) map_buffer;
	struct Cif_c_entry *to = (struct Cif_c_entry *) cr;

	to->rectype = from->rectype;
	to->ptype = from->ptype;
	to->symclass = from->symclass;
	to->retvalue = from->retvalue;
	to->varargs = from->varargs;
	to->tagid = 0;
	to->scope = from->scope;
	to->nlen = from->nlen;
	to->symid = from->symid;
	to->nargs = from->nargs;
	to->nmods = from->nmods;
	to->qual = from->qual;
	to->btype = from->btype;
	to->link = 0;

      }
      else {
	(void) memcpy((char *) cr, (char *) map_buffer,
		      _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);
    }

    break;
  }

  case CIF_C_TAG : {
    struct Cif_c_tag *tag = (struct Cif_c_tag *) cr;

    (void) memcpy((char *) cr, (char *) map_buffer,
	   _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);

    /* v1 entity value 10 maps to v2 enity values of 9 */

    if (_Cif_filetbl[lcifd].version == 1 &&
	_Cif_filetbl[lcifd].return_version != 1 &&
	tag->entity == 10)
      tag->entity = 9;
    else
      if (_Cif_filetbl[lcifd].version != 1 &&
	  _Cif_filetbl[lcifd].return_version == 1 &&
	  tag->entity == 9)
	tag->entity = 10;
    
    break;
  }

  case CIF_C_OBJECT : {

      struct Cif_c_object *obj = (struct Cif_c_object *) cr;

      (void) memcpy((char *) cr, (char *) map_buffer,
		    _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);

      if (_Cif_filetbl[lcifd].return_version == 1) { /* map a v2 cif to pass to a v1 application */

	  struct Cif_c_object *from = (struct Cif_c_object *) map_buffer;
	  struct Cif_c_object_1 *to = (struct Cif_c_object_1 *) cr;

	  to->mods = from->mods;
	  to->name = from->name;

      }
      else

	  if (_Cif_filetbl[lcifd].version == 1) {
	    /* a v1 cif to a v(>1) application */

	    struct Cif_c_object_1 *from = (struct Cif_c_object_1 *) map_buffer;
	    struct Cif_c_object *to = (struct Cif_c_object *) cr;

	    to->mods = from->mods;
	    to->name = from->name;

	  }

      if (_Cif_filetbl[lcifd].version == 1 &&
	  _Cif_filetbl[lcifd].return_version != 1 &&
	  (obj->entity == 11 || obj->entity == 12))
	  obj->entity --;
      else
	  if (_Cif_filetbl[lcifd].version != 1 &&
	      _Cif_filetbl[lcifd].return_version == 1 &&
	      (obj->entity == 10 || obj->entity == 11))
	      obj->entity ++;
    
      break;
  }

  case CIF_MESSAGE :
      {
	(void) memcpy((char *) cr, (char *) map_buffer,
		      _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);

	if (_Cif_filetbl[lcifd].return_version < 3 &&
	    _Cif_filetbl[lcifd].version == 3) {

	  struct Cif_message *from = (struct Cif_message *) map_buffer;
	  struct Cif_message_1 *to = (struct Cif_message_1 *) cr;

	  to->fid = from->pfid;

	  ((struct Cif_message *)cr)->nlen = from->nlen;
		/* this kludge allows _Cif_binread() to read in the "name"
		   field */

	}

	break;
      }


  case CIF_F90_INT_BLOCK :
  case CIF_F90_DERIVED_TYPE :
  case CIF_BE_NODE :
      {

      (void) memcpy((char *) cr, (char *) map_buffer,
		    _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);
      break;
    }

  default:
    	/*
	 * if we are not going to return this record, make sure that what we
	 * are copying matches what we read (eg there is not version 1 macro
	 * def record, but we could open a v2 cif and read with a v1 application
	 * which should never see the macro def, but it has to be read from the
	 * file before the next one can be
	 */

    	if (_Cif_filetbl[lcifd].rmask[rtype] == '\0') {

	    (void) memcpy((char *) cr, (char *) map_buffer,
		   _Cif_shortsize[rtype][_Cif_filetbl[lcifd].version]);

	  }
	  else {

	    (void) memcpy((char *) cr, (char *) map_buffer,
		   _Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);

	  }

    	/*
	 * If we are returning a version 1 cif from a v2 cif on disk
	 * and this is a stm_type and it is of type CDIR (80) then don't
	 * return it as v1 cifs don't know about thie stmt type
	 */
    	if (rtype == CIF_STMT_TYPE &&
	    CIFSTMT(cr)->type == CIF_TP_CDIR &&
	    _Cif_filetbl[lcifd].return_version == 1) {
	  return( /* keep = */ 0 );
	}
	    
  }


  return( /* keep = */ 1 );


}



/* --------------------------------------------------------------------------
 * Binary record input routine.
 *
 * Read the prepended record type, get space for the record, read in the
 * structure, read in any variable fields.
 * --------------------------------------------------------------------------
 */
static int binary_record (cif_record, fd)
struct Cif_generic **cif_record;		/* ptr to ptr to record */
FILE *fd;									/* file descriptor of cif file */
{

	int rtype, stat, size;
	register char *cp;
	struct Cif_generic *cr;
	struct Cif_generic rechdr;
	int keep = 1;

	do { /* look for a valid (ie what the user has asked for) record type */
	  cp = (char *)&rechdr;

	  if (fread (cp, sizeof(char), 1, fd) != 1) {

	    if (feof(fd)) return (CIF_EOF);
	    else return (CIF_SYSERR);

	  }

	  rtype = rechdr.rectype;


	  if (rtype > CIF_MAXRECORD || valid_record[rtype] == NO) {
	      return (CIF_BADFORM);
	  }

	  /* If we are not going to return this record, just make sure that we allocate
	     the correct amount of space to read it in, ie make it equal what is on
	     disk, not what we have to return; same for memset below. */

	  if (_Cif_filetbl[lcifd].rmask[rtype] == '\0') {

	    cr = *cif_record = (struct Cif_generic *) _Cif_space[lmode]
	      (_Cif_structsize[rtype][_Cif_filetbl[lcifd].version],
	       lcifd);

	  }
	  else {

	    cr = *cif_record = (struct Cif_generic *) _Cif_space[lmode]
	      (_Cif_structsize[rtype][_Cif_filetbl[lcifd].return_version],
	       lcifd);

	  }

	  /* We are in trouble if there's no memory...best we can do is get out of here */
	  if (cr == NULL)
	    return (CIF_NOMEM);


	  if (_Cif_filetbl[lcifd].rmask[rtype] == '\0') {

	    (void) memset ((char *)cr, '\0',
			   _Cif_structsize[rtype][_Cif_filetbl[lcifd].version]);

	  }
	  else {

	    (void) memset ((char *)cr, '\0',
			   _Cif_structsize[rtype][_Cif_filetbl[lcifd].return_version]);

	  }

	  cp = (char *)cr + 1;
	  size = _Cif_shortsize[rtype][_Cif_filetbl[lcifd].version] - 1;

	  /* If the cif versions are not equal, we have to map the data, ie some
	     records changed between cif verions, became larger or values within
	     them changed */

	  /*
	   * A new problem has been discovered recently in the structure of
	   * the following code, but it is now too close to release to fix it
	   * the right way.  A kludge work-around to the immediate problem
	   * has been implemented and warning comments have been placed at a 
	   * few critical places.  This note is left as a reminder to come 
	   * back later and do the full fix.
	   *
	   * When the CIF file is version 3 and the user asks for version 2,
	   * the following code reads in the fixed portion of the record,
	   * calls _Cif_binary_map_version() to convert it to version 2, and
	   * then passes the V2 record to _Cif_binread() to read in the 
	   * "auxiliary" fields.  However, for some records like CIF_MESSAGE 
	   * and CIF_FILE, _Cif_binread() needs information from the V3 record
	   * to correctly read in the auxiliary fields.
	   *
	   * So, this code should be restructured to read in both the fixed
	   * and auxiliary portions of the current record, and then call
	   * _Cif_binary_map_version() to convert both parts.  This will
	   * require lots of simple changes to _Cif_binread() and
	   * _Cif_binary_map_version().  This was deemed to be too risky
	   * since we are (in theory) getting close to field test.
	   * TWH  7/13/95
	   */

	  if (_Cif_filetbl[lcifd].version == _Cif_filetbl[lcifd].return_version) {

	    if (fread (cp, size, 1, fd) != 1)
	      IO_ERROR;
	  }
	  else {  /* need to map between versions */

	    /* create a temporary, static buffer in which to read data into, just
	       in case the cif on disk is larger than the cif record to be returned
	       (eg cif v2 on disk; application wants a v1) */

	    if (_cif_map_buffer == (struct Cif_generic *) NULL)
	      _cif_map_buffer = (struct Cif_generic *) malloc(CIF_MAX_SSIZE);

	    (void) memset ((char *)_cif_map_buffer, '\0',
		_Cif_shortsize[rtype][_Cif_filetbl[lcifd].return_version]);

	    if (fread ((char *) _cif_map_buffer + 1, size, 1, fd) != 1)
	      IO_ERROR;

	    keep = _Cif_binary_map_version(rtype, _cif_map_buffer, cr);

	  }
	

	  cr->rectype = rtype;

	  if ((stat = _Cif_binread (lcifd, rtype, cr, fd)) < 0)
	    return (stat);

	  /*
	   * User doesn't want this record (or it's from a cif that the
	   * application can't read (eg v2 for a v1 application ), so free
	   * up this record space
	   */

	  if ((keep == 0 ||
	       _Cif_filetbl[lcifd].rmask[rtype] == '\0') &&
	      _Cif_filetbl[lcifd].mode == CIF_MEM_INDIV)
	    Cif_Free(cr);

	}
	while (keep == 0 ||  _Cif_filetbl[lcifd].rmask[rtype] == '\0');

	return (rtype);

}
 
/* --------------------------------------------------------------------------
 * strlist process a list of string tokens and builds an array of pointers to
 * character strings.
 * --------------------------------------------------------------------------
 */

static int strlist(args)
register char ***args;			/* number of strings in the list */
{
	register int n, i, len;
	register char *c;
	char **aptr;

	aptr = NULL;
	if ( (n = atoi (token()) ) > 0) {
		aptr = (char **)_Cif_space[lmode] (sizeof(char *)*n, lcifd);
		if (aptr == NULL)
			return (CIF_NOMEM);
		for (i = 0; i < n; i++) {
			c = token();
			len = strlen (c);
			aptr[i] = _Cif_space[lmode] (len+1, lcifd);
			if (aptr[i] == NULL)
				return (CIF_NOMEM);
			(void) strcpy (aptr[i], c);
		}
	}
	*args = aptr;
	return(n);
}

/* --------------------------------------------------------------------------
 * llist processes a list of integer tokens and builds an array of pointers to
 * long ints.
 * --------------------------------------------------------------------------
 */
 
static int llist(args, varargs)
long **args;
int *varargs;
{
	register int n, i;
	register char *c;
	register long *aptr;

	c = token();

	/* String could be "*" (unknown) or "*n" for n known arguments
	 * in a varargs list.  Set the varargs flag if "*" appears.
	 */

	if (varargs != NULL) {
		*varargs = 0;
		if (*c == '*') {
			*varargs = 1;
			c++;
		}
	}

	aptr = NULL;
	if ( (n = atoi (c) ) > 0) {
		aptr = (long *)_Cif_space[lmode] (sizeof(long)*n, lcifd);
		if (aptr == NULL)
			return (CIF_NOMEM);
		for (i = 0; i < n; i++)
			aptr[i] = atoi (token());
	}
	*args = aptr;
	return(n);
}

/* --------------------------------------------------------------------------
 * filltype processes a C type descriptor.  The hex digit string and
 * associated id fields are returned as the basic type, qualifier, and
 * modifier structure.
 * --------------------------------------------------------------------------
 */

static int filltype(basic, qual, tmod)
register int *basic;
register int *qual;
register struct Cif_tmod **tmod;
{
	register char *c;
	register char *c_array;
	register int len, n, i, funcseen = NO;
	static char digit[ ] = { '\0', '\0' };
	struct Cif_tmod *tm;

	/* Get the basic type (last two hex digits) and the qualifier (3rd from
	 * last hex digit.  If that's all, return.
	 */

	len = strlen(c = token());

	*basic = strtol(&c[len-2], (char **)NULL, 16);
	c[len-2] = '\0';
	*qual = strtol(&c[len-3], (char **)NULL, 16);
	c[len-3] = '\0';
	len -= 3;
	if (len <= 0) {
		*tmod = (struct Cif_tmod *)NULL;
		return (0);
	}

	/* Get Type modifiers -- 4th - 15th hex digits */

	tm= (struct Cif_tmod *)_Cif_space[lmode](sizeof(struct Cif_tmod)*len, lcifd);
	if (tm == NULL)
		return (CIF_NOMEM);

	/* Go through the hex digits from left to right, grabbing another
	 * token for each "array of" and all but the last "function
	 * returning" modifier
	 */

	for (i = 0; i < len; i++) {
		digit[0] = c[i];

		/* Save the modifier (hex. digit). */

		tm[i].mod = n = strtol(digit, (char **)NULL, 16);

		/* If an "array of" type, grab the next token
		 * which will be a dimension value.  If a "function
		 * returning" type, grab the next token ONLY if
		 * it is NOT the first "function returning" type.
		 * If it is the first "function returning" types,
		 * bypass getting the next token
		 */

		if (n == CIF_TMC_ARRAY) {
		  c_array = token();
		  if (c_array != (char *) NULL)
		    if (atol(c_array) < 0)
		      tm[i].val = 0;
		    else
		      tm[i].val = atol(c_array);
		  else
		    tm[i].val = 0;
		}
		else if (n == CIF_TMC_FUNCNOPR || n == CIF_TMC_FUNCPRO) {
			if (funcseen == NO) {
				funcseen = YES;
				tm[i].val = 0;
				continue;
			}
			else
				tm[i].val = atol(token());
		}
		else
			tm[i].val = 0;
	}
	*tmod = tm;
	return (len);
}

/* --------------------------------------------------------------------------
 * ASCII record conversion routines
 *
 * Each routine accepts a pointer of the generic structure type, casts it to
 * the appropriate type, scans the current record via the "token" routine,
 * converts the tokens to binary values, and fills in the record structure.
 * --------------------------------------------------------------------------
 */

static int ascii_c_const (con)
struct Cif_c_const *con;
{
	register char *c1, *c2;
	register long i;

	con->symid = atol (token());
	con->btype = strtol (token(), (char **)NULL, 16);
	c1 = token();
	i = strlen (c1) + 1;
	if (delim == SEPARATOR) {
		c2 = token();
		i += strlen (c2) + 1;
	}
	else
		c2 = NULL;
	con->vlen = i;
	con->value = _Cif_space[lmode] (i, lcifd);
	if (con->value == NULL)
		return (CIF_NOMEM);
	(void) strcpy (con->value, c1);
	if (c2 != NULL)
		(void) strcpy (con->value+strlen(c1)+1, c2);
	return (CIF_C_CONST);

}


/* V1 and V2 differences : v2 has a tag id field; the symclass can have the value 5 */

static int ascii_c_entry (ent)
struct Cif_c_entry *ent;
{
	register long i;
	register char *c;
	int basic, qual, v;

	/* If the user wants a v1 cif, then we need to use a different data
	   structure */

	if (_Cif_filetbl[lcifd].return_version == 1) {
	  struct Cif_c_entry_1 *ent1 = (struct Cif_c_entry_1 *) ent;
	  int symclass;

	  c = token();
	  ent1->nlen = i = strlen (c);
	  ent1->name = _Cif_space[lmode] (i+1, lcifd);
	  if (ent1->name == NULL)
	    return (CIF_NOMEM);
	  (void) strcpy (ent1->name, c);
	  ent1->symid = atol (token());
	  ent1->ptype = atoi (token());
	  symclass = atoi (token());
	  if (_Cif_filetbl[lcifd].version != 1 && /* a v2 or greater cif */
	      symclass == 5)
	    symclass = 0;	/* symclass of 5 doesn't exist in a v1 cif */

	  ent1->symclass = symclass;
	  ent1->scope = atoi (token());

	  /* No tag id in v1 c_entry record */
	  /* ent1->tagid = atol (token()); */

	  if (_Cif_filetbl[lcifd].version != 1) /* but we are reading a v2 cif
						  which has the tag id, so skip it */
	    (void) token();

	  c = token();
	  ent1->retvalue = (*c == 'F') ? 0 : ((*c == 'T') ? 1 : 2);
	  if ((i = llist(&(ent1->argids), &v)) < 0)
	    return (CIF_NOMEM);
	  ent1->nargs = i;
	  ent1->varargs = v;
	  if ((i = filltype(&basic, &qual, &(ent1->mods))) < 0)
	    return (CIF_NOMEM);
	  ent1->nmods = i;
	  ent1->qual = qual;
	  ent1->btype = basic;

	}
	else {  /* User wants a v2 cif */

	  c = token();
	  ent->nlen = i = strlen (c);
	  ent->name = _Cif_space[lmode] (i+1, lcifd);
	  if (ent->name == NULL)
	    return (CIF_NOMEM);
	  (void) strcpy (ent->name, c);
	  ent->symid = atol (token());
	  ent->ptype = atoi (token());
	  ent->symclass = atoi (token());
	  ent->scope = atoi (token());

	  if (_Cif_filetbl[lcifd].version != 1) /* v1 cif doesn't have a tag id */
	    ent->tagid = atol (token());
	  /* else the tagid value has already been set to zero */

	  c = token();
	  ent->retvalue = (*c == 'F') ? 0 : ((*c == 'T') ? 1 : ((*c == 'I') ? 3 : 2));
	  if ((i = llist(&(ent->argids), &v)) < 0)
	    return (CIF_NOMEM);
	  ent->nargs = i;
	  ent->varargs = v;
	  if ((i = filltype(&basic, &qual, &(ent->mods))) < 0)
	    return (CIF_NOMEM);
	  ent->nmods = i;
	  ent->qual = qual;
	  ent->btype = basic;
	  if (delim == SEPARATOR) {
	      ent->link = atol(token());
	  }
	}

	return (CIF_C_ENTRY);
}



static int ascii_c_entry_end (ent_end)
struct Cif_c_entry_end *ent_end;
{
	register long i;
	register char *c;

	c = token();
	ent_end->nlen = i = strlen (c);
	ent_end->name = _Cif_space[lmode] (i+1, lcifd);
	if (ent_end->name == NULL)
		return (CIF_NOMEM);
	(void) strcpy (ent_end->name, c);
	ent_end->symid = atol (token());
	ent_end->fid = atol (token());
	ent_end->strline = atol (token());
	ent_end->endline = atol (token());

	return (CIF_C_ENTRY_END);
}


static int ascii_c_lint_directive (lint_dir)
struct Cif_c_lint_directive *lint_dir;
{
  register long i;
  register char *c;

  c = token();
  lint_dir->nlen = i = strlen (c);
  lint_dir->name = _Cif_space[lmode] (i+1, lcifd);
  if (lint_dir->name == NULL)
    return (CIF_NOMEM);
  (void) strcpy (lint_dir->name, c);
  lint_dir->val = atol (token());
  lint_dir->objid = atol (token());
  lint_dir->fid = atol (token());
  lint_dir->strline = atol (token());
  lint_dir->strpos = atol (token());
  lint_dir->endline = atol (token());
  lint_dir->endpos = atol (token());

  return (CIF_C_LINT_DIRECTIVE);
}

static int ascii_c_macro_def (macro_def)
struct Cif_c_macro_def *macro_def;
{
  register long i;
  register char *c;

  macro_def->symid = atol (token());

  c = token();
  macro_def->nlen = i = strlen (c);
  macro_def->name = _Cif_space[lmode] (i+1, lcifd);
  if (macro_def->name == NULL)
    return (CIF_NOMEM);
  (void) strcpy (macro_def->name, c);

  macro_def->fid = atol (token());
  macro_def->strline = atol (token());
  macro_def->strpos = atol (token());
  macro_def->endline = atol (token());
  macro_def->endpos = atol (token());

  return (CIF_C_MACRO_DEF);
}

static int ascii_c_macro_undef (macro_undef)
struct Cif_c_macro_undef *macro_undef;
{

  macro_undef->symid = atol (token());
  macro_undef->fid = atol (token());
  macro_undef->line = atol (token());
  macro_undef->cpos = atol (token());

  return (CIF_C_MACRO_UNDEF);
}

static int ascii_c_macro_usage (macro_use)
struct Cif_c_macro_usage *macro_use;
{
  macro_use->useid = atol(token());
  macro_use->symid = atol(token());
  macro_use->fid = atol(token());
  macro_use->strline = atol(token());
  macro_use->strpos = atol(token());
  macro_use->endline = atol(token());
  macro_use->endpos = atol(token());

  return (CIF_C_MACRO_USAGE);
}


/* v1 to v2 difference : v2 has a message code value */

static int ascii_c_message (msg)
struct Cif_c_message *msg;
{
	register char *c;
	register int tmp;

	/* If the user wants a v1 cif, then we need to use a different data
	   structure */

	if (_Cif_filetbl[lcifd].return_version == 1) {
	  struct Cif_c_message_1 *msg1 = (struct Cif_c_message_1 *) msg;

	  msg1->severity = atoi(token());
	  msg1->msgno = atoi(token());

	  /* No message code in v1 c_message record */

	  /* msg->msgcode = atoi(token()); */

	  if (_Cif_filetbl[lcifd].version != 1) /* but we are reading a v2 cif
						  which has the message code, so skip it */
	    (void) token();

	  msg1->fid = atol(token());
	  msg1->fline = atol(token());
	  c = token();
	  msg1->flinesuf = *c;
	  msg1->incid = atol(token());
	  msg1->iline = atol(token());
	  tmp = strlist(&(msg1->args));
	  if (tmp < 0)
	    return (CIF_NOMEM);
	  msg1->nargs = tmp;
	}
	else { /* a v2 cif */

	  msg->severity = atoi(token());
	  msg->msgno = atoi(token());

	  if (_Cif_filetbl[lcifd].version != 1) /* v1 cif doesn't have a message code id */
	    msg->msgcode = atoi(token());
	  /* else the message code value has already been set to zero */

	  msg->fid = atol(token());
	  msg->fline = atol(token());
	  c = token();
	  msg->flinesuf = *c;
	  msg->incid = atol(token());
	  msg->iline = atol(token());
	  tmp = strlist(&(msg->args));
	  if (tmp < 0)
	    return (CIF_NOMEM);
	  msg->nargs = tmp;
	}

	return (CIF_C_MESSAGE);
}



/* v1 to v2 difference :  entity value 11 -> 10, 12 -> 11 */

static int ascii_c_object (obj)
struct Cif_c_object *obj;
{
	register char *c;
	register int i;
	int basic, qual;


	/*
	 * If the user wants a v1 cif, then we need to use a different data
	 * structure
	 */

	if (_Cif_filetbl[lcifd].return_version == 1) {
	  struct Cif_c_object_1 *obj1 = (struct Cif_c_object_1 *) obj;

	  c = token();
	  obj1->nlen = i = strlen (c);
	  obj1->name = _Cif_space[lmode] (i+1, lcifd);
	  if (obj1->name == NULL)
	      return (CIF_NOMEM);
	  (void) strcpy (obj1->name, c);
	  obj1->symid = atol (token());
	  obj1->entity = atoi (token());

	  /* v1 entity values 11 and 12 map to v2 enity values of 10 and 11 */

	  if (_Cif_filetbl[lcifd].version != 1 &&
	      (obj1->entity == 10 || obj1->entity == 11))
	      obj1->entity ++;

	  obj1->symclass = atoi (token());
	  obj1->scope = atoi (token());
	  obj1->tagid = atol (token());
	  obj1->psymid = atol (token());
	  obj1->size = atoi (token());
	  if ((i = filltype(&basic, &qual, &(obj1->mods))) < 0)
	      return (CIF_NOMEM);
	  obj1->nmods = i;
	  obj1->qual = qual;
	  obj1->btype = basic;

      }
	else { /* return a v2 cif */

	    c = token();
	    obj->nlen = i = strlen (c);
	    obj->name = _Cif_space[lmode] (i+1, lcifd);
	    if (obj->name == NULL)
		return (CIF_NOMEM);
	    (void) strcpy (obj->name, c);
	    obj->symid = atol (token());
	    obj->entity = atoi (token());

	    /* v1 entity values 11 and 12 map to v2 enity values of 10 and 11 */

	    if (_Cif_filetbl[lcifd].version == 1 &&
		(obj->entity == 11 || obj->entity == 12))
		obj->entity --;

	    obj->symclass = atoi (token());
	    obj->scope = atoi (token());
	    obj->tagid = atol (token());
	    obj->psymid = atol (token());
	    obj->size = atoi (token());
	    if ((i = filltype(&basic, &qual, &(obj->mods))) < 0)
		return (CIF_NOMEM);
	    obj->nmods = i;
	    obj->qual = qual;
	    obj->btype = basic;

	    if (delim == SEPARATOR) {
		obj->link = atol(token());
	    }
	}

	return (CIF_C_OBJECT);
}

static int ascii_c_opts (opt)
struct Cif_c_opts *opt;
{
	register char *c;
	register long i;

	c = token();
	opt->nlen = i = strlen (c);
	opt->name = _Cif_space[lmode] (i+1, lcifd);
	if (opt->name == NULL)
		return (CIF_NOMEM);
	(void) strcpy (opt->name, c);
	(void) strcpy (opt->bopts, token());
	opt->msglev = atoi(token());
	opt->truncval = atoi (token());
	opt->debug = *token();
	(void) strncpy (opt->report, token(), sizeof(opt->report));
	opt->atsklev = atoi (token());
	opt->inlnlev = atoi (token());
	opt->sclrlev = atoi (token());
	opt->vctrlev = atoi (token());
	if ((i = strlist(&(opt->incs))) < 0)
		return (CIF_NOMEM);
	opt->nincs = i;
	if ((i = strlist(&(opt->defs))) < 0)
		return (CIF_NOMEM);
	opt->ndefs = i;
	if ((i = strlist(&(opt->udefs))) < 0)
		return (CIF_NOMEM);
	opt->nudefs = i;

	return (CIF_C_OPTS);
}


/* v1 to v2 difference :  entity value 10 -> 9 */

static int ascii_c_tag (tag)
struct Cif_c_tag *tag;
{
	register char *c;
	register long i;
	int basic, qual;
	register int tmp;

	c = token();
	tag->nlen = i = strlen(c);
	tag->name = _Cif_space[lmode] (i+1, lcifd);
	if (tag->name == NULL)
		return (CIF_NOMEM);
	(void) strcpy (tag->name, c);
	tag->tagid = atol (token());
	tag->entity = atoi (token());

	/* v1 entity value 10 maps to v2 enity values of 9 */

	if (_Cif_filetbl[lcifd].version == 1 &&
	    _Cif_filetbl[lcifd].return_version != 1 &&
	    tag->entity == 10)
	  tag->entity = 9;
	else
	  if (_Cif_filetbl[lcifd].version != 1 &&
	      _Cif_filetbl[lcifd].return_version == 1 &&
	      tag->entity == 9)
	    tag->entity = 10;

	tag->size = atoi (token());
	tmp = llist(&(tag->memids), (int *) NULL);
	if (tmp < 0)
	  return (CIF_NOMEM);
	tag->nmems = tmp;
	tmp = filltype(&basic, &qual, &(tag->mods));
	if (tmp < 0)
	  return (CIF_NOMEM);
	tag->nmods = tmp;
	tag->qual = qual;
	tag->btype = basic;

	return (CIF_C_TAG);
}

static int ascii_callsite (cs)
struct Cif_callsite *cs;
{

	register long i;
	register int nargs;

	cs->entryid = atol (token());
	cs->fid = atol (token());
	cs->line = atol (token());
	cs->cpos = atol (token());
	if ( (nargs = cs->nargs = atoi (token())) > 0) {
		cs->argids = (long *)_Cif_space[lmode] (sizeof(long)*nargs, lcifd);
		if (cs->argids == NULL)
			return (CIF_NOMEM);
		i = 0;
		while (i < nargs)
			cs->argids[i++] = atol (token());
	}
	if (delim == SEPARATOR)
		cs->valused = (*token() == 'F' ? 0 : 1);
	return (CIF_CALLSITE);

}

static int ascii_cifhdr (hdr)
struct Cif_cifhdr *hdr;
{

	(void) token(); /* must be "cif" */
	hdr->version = atoi (token()+1);
	hdr->lang = _Cif_filetbl[lcifd].lang = atoi (token());

	/*
	 * Set the srcfid, set earlier from looking
	 * ahead to the srcfile record
	 * in v1, this used to be an unused field, so no need to mask out
	 */

	hdr->srcfid = _Cif_filetbl[lcifd].srcfid;

	/*
	 * F90 has a slightly different cif_usage record, so use a different
	 * parse routine
	 */

#ifndef CRAY2
	if (_Cif_filetbl[lcifd].lang == CIF_LG_F90) {
	  ascii_record[CIF_USAGE] = ascii_f90_usage;
	}
	else {
	  ascii_record[CIF_USAGE] = ascii_usage;
	}
#endif /* CRAY2 */

	(void) strcpy (hdr->cvers, token());
	(void) strcpy (hdr->date, token());
	(void) strcpy (hdr->time, token());
	(void) strcpy (hdr->group, token());
	hdr->msgfid = atol (token());

	/*
	 * We need this so that when returning a v1 cif, we do not
	 * return the file record associated with the message catalog
	 */

	global_msgfid = hdr->msgfid;
	(void) strncpy (hdr->machname, token(), 8);
	(void) strncpy (hdr->hostcpu, token(), 8);
	hdr->hostcpu[8] = '\0'; /* make sure that the string is terminated */
	hdr->canpos = _Cif_filetbl[lcifd].seek;
	hdr->form = ASCII_CIF_FORMAT;
	hdr->form = 0;
	return (CIF_CIFHDR);

}

static int ascii_comblk (cb)
struct Cif_comblk *cb;
{

	register char *c;
	register long i;

	/*
	 * If the user wants a v1 cif, then we need to use a different data
	 * structure
	 */

	if (_Cif_filetbl[lcifd].return_version == 1) {
	  struct Cif_comblk_1 *cb1 = (struct Cif_comblk_1 *) cb;

	  c = token();
	  cb1->nlen = i = strlen (c);
	  cb1->name = _Cif_space[lmode] (i+1, lcifd);
	  if (cb1->name == NULL)
	    return (CIF_NOMEM);
	  (void) strcpy (cb1->name, c);
	  cb1->symid = atol (token());
	  cb1->cbtype = atoi (token());
	  cb1->length = atol (token());

	  /* No distribution code in v1 c_message record */

	  /* cb1->dist = atoi(token()); */

	  if (_Cif_filetbl[lcifd].version != 1) /* but we are reading a v2 cif
						  which has the distribution code, so skip it */
	    (void) token();

	}

	else { /* version 2 cif */

	  c = token();
	  cb->nlen = i = strlen (c);
	  cb->name = _Cif_space[lmode] (i+1, lcifd);
	  if (cb->name == NULL)
	    return (CIF_NOMEM);
	  (void) strcpy (cb->name, c);
	  cb->symid = atol (token());
	  cb->cbtype = atoi (token());
	  cb->length = atol (token());

	  if (_Cif_filetbl[lcifd].version != 1) { /* v1 cif doesn't have a distribution code id */
	    c = token();
	    if (c != (char *) NULL &&
		*c != (char) NULL)
	      cb->dist = atoi(c);
	  }
	  /* else the distribution code value has already been set to zero */

	}

	return (CIF_COMBLK);

}


static int ascii_const (con)
struct Cif_const *con;
{
	register int i;
	register char *c;
	register long attr;
	register int n;

	/*
	 * If the user wants a v1 cif, then we need to use a different data
	 * structure
	 */

	if (_Cif_filetbl[lcifd].return_version == 1) {
	  struct Cif_const_1 *con1 = (struct Cif_const_1 *) con;

	  c = token();
	  con1->nlen = i= strlen (c);
	  con1->name = _Cif_space[lmode] (i+1, lcifd);
	  if (con1->name == NULL)
	    return (CIF_NOMEM);
	  (void) strcpy (con1->name, c);
	  con1->symid = atol (token());
	  con1->dtype = atoi (token());
	  if (con1->dtype == 100)
	    con1->dtype = 0;
	  else
	    (con1->dtype)++;

	  /* get constant value - multiple values not implemented */

	  if ( (con1->nvalues = atoi (token())) == 1) {

	    con1->vlen = i = strlen (c = token());
	    con1->value = _Cif_space[lmode] (i+1, lcifd);
	    if (con1->value == NULL)
	      return (CIF_NOMEM);
	    (void) strcpy (con1->value, c);

	  }

	  /* get attributes */

	  attr = strtol (token(), (char **)NULL, 16);
	  con1->imptype = ((attr & CO_ATTR_IMPTYPE) != 0);

	  /* No original form flag in v1 c_message record */

	  /* cb1->origform = 0; */
	  /* cb1->olen = 0; */
	  /* cb1->oform = (char *) NULL; */

	  if (_Cif_filetbl[lcifd].version != 1) { /* but we are reading a v2 cif
						   * which has the original flag/value,
						   * so skip them
						   */
	    (void) token();
	    if (delim == SEPARATOR)
	      (void) token();
	  }
	}
	else { /* version 2 cif */
	  c = token();
	  con->nlen = i= strlen (c);
	  con->name = _Cif_space[lmode] (i+1, lcifd);
	  if (con->name == NULL)
	    return (CIF_NOMEM);
	  (void) strcpy (con->name, c);
	  con->symid = atol (token());
	  con->dtype = atoi (token());
	  if (con->dtype == 100)
	    con->dtype = 0;
	  else
	    (con->dtype)++;

	  /* get constant value - multiple values not implemented */

	  if ( (con->nvalues = atoi (token())) == 1) {
	    con->vlen = i = strlen (c = token());
	    con->value = _Cif_space[lmode] (i+1, lcifd);
	    if (con->value == NULL)
	      return (CIF_NOMEM);
	    (void) strcpy (con->value, c);
	  }

	  /* get attributes */

	  attr = strtol (token(), (char **)NULL, 16);
	  con->imptype = ((attr & CO_ATTR_IMPTYPE) != 0);
	  /* If character type, read the character length */
	  if ((attr & CO_ATTR_CHAR) != 0) {
	    (void) token();
	  }

	  /* Read the dimensions - not stored in the cif */
	  if ((attr & CO_ATTR_DIM) != 0) {
	    n = atoi(token());

	    while (n > 0) {
	      (void) token();  /* discard lower bound */
	      (void) token();  /* discard upper bound */
	      n--;
	    }

	  }

	  if (_Cif_filetbl[lcifd].version != 1) { /* v1 cif doesn't have the oiginal
						   * form fields
						   */
	    con->origform = atoi(token());
	    if (con->origform) {
	      c = token();
	      con->olen = i = strlen (c);
	      con->oform = _Cif_space[lmode] (i+1, lcifd);
	      if (con->oform == NULL)
		return (CIF_NOMEM);
	      (void) strcpy (con->oform, c);
	    }
	  }
	  /* else the original form fields have already been set to zero */
	}
	return (CIF_CONST);

}


static int ascii_edopts (eo)
struct Cif_edopts *eo;
{

	eo->opts = strtol (token(), (char **)NULL, 16);
	return (CIF_EDOPTS);

}

static int ascii_entry (entry)
struct Cif_entry *entry;
{

	register char *c;
	register long i, len;

	c = token();
	entry->nlen = len = strlen (c);
	entry->name = _Cif_space[lmode] (len+1, lcifd);
	if (entry->name == NULL)
		return (CIF_NOMEM);
	(void) strcpy (entry->name, c);
	entry->symid = atol (token());
	entry->etype = atol (token());
	entry->dtype = atol (token());
	if (entry->dtype == 100)
		entry->dtype = 0;
	else
		(entry->dtype)++;

	/* get argument ids */

	if ( (len = atoi (token())) >= 0) {
		entry->valargs = 1;
		if (len > 0) {
			entry->nargs = len;
			entry->argids = (long *)_Cif_space[lmode] (sizeof(long)*len, lcifd);
			if (entry->argids == NULL)
				return (CIF_NOMEM);
			for (i = 0; i < len; i++)
				(entry->argids)[i] = atol (token());
		}
	}

	/* get attributes */

	i = strtol (token(), (char **)NULL, 16);
	entry->recur = ((i & EN_ATTR_RECUR) != 0);
	entry->stmtfunc = ((i & EN_ATTR_STMTF) != 0);
	entry->extrn = ((i & EN_ATTR_EXTERN) != 0);
	entry->intrin = ((i & EN_ATTR_INTRIN) != 0);
	entry->imptype = ((i & EN_ATTR_IMPTYPE) != 0);
	if ((i & EN_ATTR_CHAR) != 0)
		entry->charlen = atol (token());
	else
		entry->charlen = 0;
	return (CIF_ENTRY);

}

static int ascii_file (file)
struct Cif_file *file;
{

	register char *c;
	register long len;

	if (_Cif_filetbl[lcifd].return_version == 3) {

	  c = token();

	  file->nlen = len = strlen (c);
	  file->name = _Cif_space[lmode] (len+1, lcifd);
	  if (file->name == NULL)
	      return (CIF_NOMEM);
	  (void) strcpy (file->name, c);
#ifdef KEY
          char *fld = token();
          if (fld == NULL) { 
            fprintf(stderr, "libcif, Cif_file error : Could not open ascii_file\n");
            exit(-1);
          }

	  file->fid = atol (fld);
#else
          file->fid = atol (token());
#endif

	  if (delim == SEPARATOR) {

	    c = token();

	    file->onlen = len = strlen (c);
	    file->oname = _Cif_space[lmode] (len+1, lcifd);
	    if (file->oname == NULL)
		return (CIF_NOMEM);
	    (void) strcpy (file->oname, c);
	  }
	}
	else { /* returning a v<3 CIF */

	  struct Cif_file_1 *file1 = (struct Cif_file_1 *) file;

	  c = token();

	  file1->nlen = len = strlen (c);
	  file1->name = _Cif_space[lmode] (len+1, lcifd);
	  if (file1->name == NULL)
	      return (CIF_NOMEM);
	  (void) strcpy (file1->name, c);
	  file1->fid = atol (token());

	  /*
	   * If we are returning a version 1 cif and the file is the
	   * message catalog, which didn't appear in v1 cif's
	   * then don't return it.
	   */
	  if (_Cif_filetbl[lcifd].lang == CIF_LG_F77 &&
	      file1->fid == global_msgfid &&
	      _Cif_filetbl[lcifd].return_version == 1) {
	    return ( CIF_MAXRECORD );  /* flags an invalid record */
	  }


	}
	return (CIF_FILE);

}

static int ascii_include (inc)
struct Cif_include *inc;
{

	inc->srcid = atol (token());
	inc->line = atol (token());
	inc->cpos = atol (token());
	inc->incid = atol (token());
	return (CIF_INCLUDE);

}

static int ascii_label (label)
struct Cif_label *label;
{

	register char *c;
	register long i;

	c = token();
	label->nlen = i = strlen (c);
	label->name = _Cif_space[lmode] (i+1, lcifd);
	if (label->name == NULL)
		return (CIF_NOMEM);
	(void) strcpy (label->name, c);
	label->symid = atol (token());
	label->ltype = atoi (token());
	return (CIF_LABEL);

}

static int ascii_loop (loop)
struct Cif_loop *loop;
{

	loop->lptype = atol (token());
	loop->sfid = atol (token());
	loop->strline = atol (token());
	loop->strcpos = atol (token());
	loop->efid = atol (token());
	loop->endline = atol (token());
	loop->endcpos = atol (token());
	if (delim == SEPARATOR)
		loop->symid = atol (token());
	if (delim == SEPARATOR)
		loop->labelid = atol (token());
	return (CIF_LOOP);

}

/* --- Fortran machine characteristic values mask --- */
#define NFORTCHARS			8
#define CIF_MCF_TAILGT		0x01
#define CIF_MCF_BDM			0x02
#define CIF_MCF_CIGS			0x04
#define CIF_MCF_EMA			0x08
#define CIF_MCF_READVL		0x10
#define CIF_MCF_VPOP			0x20
#define CIF_MCF_VRECUR		0x40
#define CIF_MCF_AVL			0x80


/*
 * Note. Machine characteristic became unnecessarily complicated
 * between C, F77 and F90 and from v1 to v2. F77 started off with
 * it's own subset of the full set. C provided a more complete
 * set of characteristics, so for F77 we mapped the compiler
 * provided list to be what C gave. In v2, F77 now provides
 * the more complete list (as do F90 for the start). But if a user
 * requires a v1 cif from a v2 cif, we have to mask out thouse bits
 * not previosly available.
 */


static int ascii_mach_char (mc)
struct Cif_mach_char *mc;
{
	int i;
	long valmask;

	static int fort_mc[NFORTCHARS] = {
		CIF_MCF_TAILGT,
		CIF_MCF_BDM,
		CIF_MCF_CIGS,
		CIF_MCF_EMA,
		CIF_MCF_READVL,
		CIF_MCF_VPOP,
		CIF_MCF_VRECUR,
		CIF_MCF_AVL
	};
	static int gen_mc[NFORTCHARS] = {
		CIF_MC_TAILGT,
		CIF_MC_BDM,
		CIF_MC_CIGS,
		CIF_MC_EMA,
		CIF_MC_READVL,
		CIF_MC_VPOP,
		CIF_MC_VRECUR,
		CIF_MC_AVL
	};

	/*
	 * If the user wants a v1 cif, then we need to use a different data
	 * structure
	 */

	if (_Cif_filetbl[lcifd].return_version == 1) {
	  struct Cif_mach_char_1 *mc1 = (struct Cif_mach_char_1 *) mc;

	  for (i = 0; i < 16; i++) mc1->cpuname[i] = '\0';
	  (void) strcpy (mc1->cpuname, token());
	  mc1->memspeed = atoi (token());
	  mc1->memsize = atol (token());
	  valmask = strtol (token(), (char **)NULL, 16);

	  /*
	   * If we are reading a cft77 v2 cif, we may have more characteristics
	   * then v1 could handle, and they will be in different bit places
	   */

	  if (_Cif_filetbl[lcifd].lang == CIF_LG_F77) {
	    if (_Cif_filetbl[lcifd].version != 1) {
	      /*
	       * We have the correct bits, but some of them could
	       * not be set in v1, so mask those out
	       */
#ifdef CRAY2
	      /*
	       * On a Cray2, TAILGT was 0x01 in v1, it is now 0x02
	       * It is the only valid cray2 value for a v1 cif
	       */
	      if (mc1->valmask | CIF_MC_TAILGT_1)  {
		mc1->valmask = CIF_MC_TAILGT;
	      }
	      else {
		mc1->valmask = 0;
	      }
#else  /* Non-Cray2 have more bits to mask out */
	      mc1->valmask = valmask & CIF_MC_MASK;
#endif /* CRAY2 */

	  }
	    else { /*
		    * v1 to v1; map what the cif is giving to the correct
		    * return values, consistent with C
		    */
	      mc1->valmask = 0;
	      for (i = 0; i < NFORTCHARS; i++)
		if (valmask & fort_mc[i]) mc1->valmask |= gen_mc[i];
	    }
	}
	  else {  /* valmask is already okay for C and F90 */
	      mc1->valmask = valmask;
	  }
	  /*
	   * No : number of banks, number of cpus, instruction buffer size
	   * clock period, number of cluster registers or bank busy time
	   * fields in a version 1 cif
	   */

	  if (_Cif_filetbl[lcifd].version != 1 &&
	      delim == SEPARATOR) {
	    /*
	     * but we are reading a v2 cif which has them
	     */
	    (void) token();  	/* num banks */
	    (void) token();  	/* num cpus */
	    (void) token();  	/* instruction buffer size */
	    (void) token();  	/* clock period */
	    (void) token();  	/* number of cluster register sets */
	    (void) token();  	/* bank busy time */
	  }

      }
	else { /* return a v2 cif */

	  (void) strcpy (mc->cpuname, token());

	  mc->memspeed = atoi (token());
	  mc->memsize = atol (token());
	  valmask = strtol (token(), (char **)NULL, 16);

	  /*
	   * we only have to go through this mapping business for F77,
	   * C and F90 provide the right machine characteristics
	   */

	  if (_Cif_filetbl[lcifd].lang == CIF_LG_F77) {

	    /*
	     * if it's a v1 cif, we have to map the bits still from
	     * what the compiler provides to what we want
	     */

	    if (_Cif_filetbl[lcifd].version == 1) {
	      mc->valmask = 0;
	      for (i = 0; i < NFORTCHARS; i++)
		if (valmask & fort_mc[i]) mc->valmask |= gen_mc[i];
	    }
	    else /*
		  * v2 to v2, so just propagate the value which F77 is
		  * now provide in the correct format
		  */
	      mc->valmask = valmask;
	  }
	  else { /* C and F90 provide the correct format directly */
	    mc->valmask = valmask;
	  }

	  /* Read the extra v2 cif fields (if present) :
	   * number of banks, number of cpus, instruction buffer size
	   * clock period, number of cluster registers or bank busy time
	   */

	  if (_Cif_filetbl[lcifd].version != 1 &&
	      delim == SEPARATOR) {

	    mc->numbanks = atol (token());
	    mc->numcpus = atol (token());
	    mc->instbufsize = atol (token());
	    mc->clockperiod = atol (token());
	    mc->numclregs = atol (token());
	    mc->bankbusytime = atol (token());

	    if (delim == SEPARATOR)
	      mc->tbitlen = atoi (token());

	  }
	  /* else they will already be set to zero */

	}

	return (CIF_MACH_CHAR);

}

static int ascii_message (msg)
struct Cif_message *msg;
{
	register char *c;
	register long i;
	register int tmp;

	if (_Cif_filetbl[lcifd].return_version <= 2) {
	  struct Cif_message_1 *msg1 = (struct Cif_message_1 *) msg;

	  msg1->severity = atoi (token());
	  msg1->msgno = atol (token());
	  msg1->fid = atol (token());
	  msg1->uline = atol (token());
	  msg1->cpos = atoi (token());
	  msg1->fline = atol (token());
	  tmp = strlist(&(msg1->args));
	  if (tmp < 0)
	    return (CIF_NOMEM);
	  msg1->nargs = tmp;

	}
	else { /* Version 3 CIF */
	  msg->severity = atoi (token());
	  msg->msgno = atol (token());
	  msg->fid = atol (token());
	  msg->uline = atol (token());
	  msg->cpos = atoi (token());
	  msg->fline = atol (token());
	  tmp = strlist(&(msg->args));
	  if (tmp < 0)
	    return (CIF_NOMEM);
	  msg->nargs = tmp;

	  /* scoping unit names are only present in V3 CIF's */
	  if (_Cif_filetbl[lcifd].version >= 3 &&
	      delim == SEPARATOR) {

	    c = token();
	    msg->nlen = i = strlen(c);
	    msg->name = _Cif_space[lmode] (i+1, lcifd);
	    if (msg->name == NULL)
		return (CIF_NOMEM);
	    (void) strcpy (msg->name, c);
	    if (delim == SEPARATOR) {
		msg->order = atoi(token());
		if (delim == SEPARATOR) {
		  msg->flags = atoi(token());
		  if (delim == SEPARATOR) {
		    msg->pfid = atol(token());
		  }
		}
	      }
	  }
	  else {
		/* We are creating a version 3 record from a Version 2
		 * CIF. If this is subsequently passed to a Version 2
		 * reading application, the pfid is copied into the fid
		 * as that is the more correct value for V3 CIF's. When
	 	 * messages are coming from inlined files the V2 CIF's
		 * do not contain enough information to make use of the
		 * two fid values, so the pfid value should be used.
		 * Once we have a V3 record, libcif can not tell if this
		 * came from a valid V3 CIF or from a V2->V3 mapping
		 * process, so it is appropriate to copy the fid into
		 * the pfid to allow it to be copied back again later
		 * if necessary.
		 */
		msg->pfid = msg->fid;
	}

	}

	return (CIF_MESSAGE);
}

static int ascii_misc_opts (mo)
struct Cif_misc_opts *mo;
{
	register int i, j;
	register char *c;
	register int tmp;

	/*
	 * If the user wants a v1 cif, then we need to use a different data
	 * structure
	 */

	if (_Cif_filetbl[lcifd].return_version == 1) {
	  struct Cif_misc_opts_1 *mo1 = (struct Cif_misc_opts_1 *) mo;

	  mo1->malloc = atoi (token());
	  mo1->intlen = atoi (token());
	  mo1->msglvl = atoi (token());
	  mo1->vopt = atoi (token());
	  mo1->amode = atoi (token ());
	  mo1->trunc = atoi (token ());
	  mo1->truncval = atoi (token());
	  tmp = llist(&(mo1->msgno), (int *) NULL);
	  if (tmp < 0)
	    return (CIF_NOMEM);
	  mo1->nmsgs = tmp;

	  tmp = strlist (&(mo1->cdirs));
	  if (tmp < 0)
	    return (CIF_NOMEM);

	  mo1->ncdirs = tmp;
	  c = token();
	  if ((mo1->onlen = i = strlen (c)) > 0) {
	    mo1->objname = _Cif_space[lmode] (i+1, lcifd);
	    if (mo1->objname == NULL)
	      return (CIF_NOMEM);
	    (void) strcpy (mo1->objname, c);
	  }
	  c = token();
	  if (c != (char *) NULL && (mo1->cnlen = i = strlen (c)) > 0) {
	    mo1->calname = _Cif_space[lmode] (i+1, lcifd);
	    if (mo1->calname == NULL)
	      return (CIF_NOMEM);
	    (void) strcpy (mo1->calname, c);
	  }

	  c = token();
	  if (delim == SEPARATOR) {
	    if (c != (char *) NULL && (mo1->inlen = i = strlen (c)) > 0) {
	      mo1->inname = _Cif_space[lmode] (i+1, lcifd);
	      if (mo1->inname == NULL)
		return (CIF_NOMEM);
	      (void) strcpy (mo1->inname, c);
	    }
	  }

	  /*
	   * No listing name, cif option, input length, run-time checking
	   * or include file fields in a version 1 cif
	   */

	  if (_Cif_filetbl[lcifd].version != 1) { /*
						   * but we are reading a v2 cif
						   * which has the distribution and geometry
						   * fields, so skip them
						   */
	    (void) token();  	/* listing name */
	    (void) token();  	/* cif option */
	    (void) token();  	/* input length */
	    (void) token();  	/* run-time checking */
	    i = atoi(token());  /* number includes */
	    for (j = 0; j < i; j++)
	      (void) token(); 	/* include file name */
	  }


	}

	else { /* return a v2 cif */

	  mo->malloc = atoi (token());
	  mo->intlen = atoi (token());
	  mo->msglvl = atoi (token());
	  mo->vopt = atoi (token());
	  mo->amode = atoi (token ());
	  mo->trunc = atoi (token ());
	  mo->truncval = atoi (token());
	  tmp = llist(&(mo->msgno), (int *) NULL);
	  if (tmp < 0)
	    return (CIF_NOMEM);
	  mo->nmsgs = tmp;

	  tmp = strlist (&(mo->cdirs));
	  if (tmp < 0)
	    return (CIF_NOMEM);
	  mo->ncdirs = tmp;

	  c = token();
	  if ((mo->onlen = i = strlen (c)) > 0) {
	    mo->objname = _Cif_space[lmode] (i+1, lcifd);
	    if (mo->objname == NULL)
	      return (CIF_NOMEM);
	    (void) strcpy (mo->objname, c);
	  }

	  c = token();
	  if ((mo->cnlen = i = strlen (c)) > 0) {
	    mo->calname = _Cif_space[lmode] (i+1, lcifd);
	    if (mo->calname == NULL)
	      return (CIF_NOMEM);
	    (void) strcpy (mo->calname, c);
	  }

	  c = token();
	  if (delim == SEPARATOR) {
	    if ((mo->inlen = i = strlen (c)) > 0) {
	      mo->inname = _Cif_space[lmode] (i+1, lcifd);
	      if (mo->inname == NULL)
		return (CIF_NOMEM);
	      (void) strcpy (mo->inname, c);
	    }
	  }

	  /* Read the extra v2 cif fields */

	  if (_Cif_filetbl[lcifd].version != 1) {

	    c = token();
	    if (delim == SEPARATOR) {
	      if (c != (char *) NULL && (mo->llen = i = strlen (c)) > 0) {
		mo->lname = _Cif_space[lmode] (i+1, lcifd);
		if (mo->lname == NULL)
		  return (CIF_NOMEM);
		(void) strcpy (mo->lname, c);
	      }
	    }

	    mo->cifopt = strtol (token(), (char **)NULL, 16);
	    mo->inputlen = atoi(token());
	    mo->runtime = strtol (token(), (char **)NULL, 16);
	    tmp = strlist (&(mo->incdirs));
	    if (tmp < 0)
	      return (CIF_NOMEM);
	    mo->numincs = tmp;
	    }

	  }


	return (CIF_MISC_OPTS);

}

static int ascii_namelist (nl)
struct Cif_namelist *nl;
{

	register long i;
	register char *c;

	c = token();
	nl->nlen = i = strlen (c);
	nl->name = _Cif_space[lmode] (i+1, lcifd);
	if (nl->name == NULL)
		return (CIF_NOMEM);
	(void) strcpy (nl->name, c);
	nl->symid = atol (token());
	if ((nl->nids = atoi (token())) > 0) {
		nl->ids = (long *) _Cif_space[lmode] (sizeof(long)*nl->nids, lcifd);
		if (nl->ids == NULL)
			return (CIF_NOMEM);
		for (i = 0; i < (int) nl->nids; i++)
			(nl->ids)[i] = atol (token());

	}
	return (CIF_NAMELIST);

}

static int ascii_nd_msg (nmsg)
struct Cif_nd_msg *nmsg;
{
        register int tmp;

	nmsg->severity = atoi (token());
	nmsg->msgno = atol (token());
	nmsg->fid = atol (token());
	nmsg->fline = atol (token());
	nmsg->cpos = atoi (token());
	nmsg->uline = atol (token());
	(void) strncpy (nmsg->group, token(), 16);
	nmsg->msgfid = atol (token());

	tmp = strlist(&(nmsg->args));
	if (tmp < 0)
	  return (CIF_NOMEM);
	nmsg->nargs = tmp;
	return (CIF_ND_MSG);

}


/* v1 to v2 difference : v2 has a distribution field and geometry field added */

static int ascii_object (obj)
struct Cif_object *obj;
{

	register char *c;
	register long i, attr;
	struct Cif_dim *dim;

	/*
	 * If the user wants a v1 cif, then we need to use a different data
	 * structure
	 */

	if (_Cif_filetbl[lcifd].return_version == 1) {
	  struct Cif_object_1 *obj1 = (struct Cif_object_1 *) obj;

	  c = token();
	  if ((obj1->nlen = i = strlen (c)) > 0) {
	    obj1->name = _Cif_space[lmode] (i+1, lcifd);
	    if (obj1->name == NULL)
	      return (CIF_NOMEM);
	    (void) strcpy (obj1->name, c);
	  }
	  else
	    obj1->name = NULL;
	  obj1->symid = atol (token());
	  obj1->dtype = atoi (token());
	  if (obj1->dtype == 100)
	    obj1->dtype = 0;
	  else
	    (obj1->dtype)++;

	  obj1->symclass = atoi (token());

	  obj1->storage = atol (token());
	  if ((i = atol (token())) >= 0) {
	    obj1->valoffset = 1;
	    obj1->offset = i;
	  }


	  /* get attributes */
	  attr = strtol (token(), (char **)NULL, 16);
	  obj1->aarray = ((attr & CO_ATTR_AUTO) != 0);
	  obj1->equiv = ((attr & CO_ATTR_EQUIV) != 0);
	  obj1->data = ((attr & CO_ATTR_DATA) != 0);
	  obj1->save = ((attr & CO_ATTR_SAVE) != 0);
	  obj1->imptype = ((attr & CO_ATTR_IMPTYPE) != 0);

	  /*
	   * PE_RESIDENT, pointee, array and geometry 
	   * declaration fields didn't exist on v1, so don't set them
	   */

	  /*
	    obj1->peresident == ((attr & CO_ATTR_PE_RESIDENT) != 0);
	    obj1->pointee == ((attr & CO_ATTR_POINTEE) != 0);
	    obj1->array_dec == ((attr & CO_ATTR_ARRAY_DEC) != 0);
	    obj1->geom_dec == ((attr & CO_ATTR_GEOM_DEC) != 0);
	    */

	  if ((attr & CO_ATTR_CHAR) != 0) {
	    c = token ();
	    if (*c == '*')
	      obj1->cltype = 1;
	    else
	      obj1->charlen = atol (c);
	  }
	  else
	    obj1->charlen = 0;
	  if ((attr & CO_ATTR_DIM) == 0)
	    obj1->ndims = 0;
	  else {
	    obj1->ndims = atoi (token());
	    dim = obj1->dim = (struct Cif_dim *)_Cif_space[lmode]
	      (sizeof(struct Cif_dim)*obj1->ndims, lcifd);
	    if (dim == NULL)
	      return (CIF_NOMEM);
	    for (i=0; i < (int) obj1->ndims; i++) {
	      c = token ();
	      if (*c == 'E')
		dim->ltype = CIF_DM_EXPR;
	      else if (*c == '*')
		dim->ltype = CIF_DM_ASSUMED;
	      else {
		dim->ltype = CIF_DM_CONSTANT;
		dim->lower = atol (c);
	      }
	      c = token ();
	      if (*c == 'E')
		dim->utype = CIF_DM_EXPR;
	      else if (*c == '*')
		dim->utype = CIF_DM_ASSUMED;
	      else {
		dim->utype = CIF_DM_CONSTANT;
		dim->upper = atol (c);
	      }
	      dim++;
	    }
	  }

	  /* No distribution, geometry id or pointer id in v1 cif */

	  /* obj1->distribution = atoi(token()); */
	  /* obj1->geomid = atoi(token()); */
	  /* obj1->pointer = atoi(token()); */

	  if (_Cif_filetbl[lcifd].version != 1) { /*
						   * but we are reading a v2 cif
						   * which has the distribution and geometry
						   * fields, so skip them
						   */
	    (void) token();  /* distribution, geometry id and pointer id */
	    (void) token();  /* they wouldn't all be there, but that's okay, */
	    (void) token();  /* token will just keep giving the eol */
	  }

	}
	else {  /* returning a v2 cif */
	  c = token();
	  if ((obj->nlen = i = strlen (c)) > 0) {
	    obj->name = _Cif_space[lmode] (i+1, lcifd);
	    if (obj->name == NULL)
	      return (CIF_NOMEM);
	    (void) strcpy (obj->name, c);
	  }
	  else
	    obj->name = NULL;
	  obj->symid = atol (token());
	  obj->dtype = atoi (token());
	  if (obj->dtype == 100)
	    obj->dtype = 0;
	  else
	    (obj->dtype)++;

	  obj->symclass = atoi (token());
	  obj->storage = atol (token());
	  if ((i = atol (token())) >= 0) {
	    obj->valoffset = 1;
	    obj->offset = i;
	  }


	  /* get attributes */
	  attr = strtol (token(), (char **)NULL, 16);
	  obj->aarray = ((attr & CO_ATTR_AUTO) != 0);
	  obj->equiv = ((attr & CO_ATTR_EQUIV) != 0);
	  obj->data = ((attr & CO_ATTR_DATA) != 0);
	  obj->save = ((attr & CO_ATTR_SAVE) != 0);
	  obj->imptype = ((attr & CO_ATTR_IMPTYPE) != 0);

	  /*
	   * There's no chance of getting these next few from a v1 cif, but
	   * it does no harm to look
	   */

	  obj->peresident = ((attr & CO_ATTR_PE_RESIDENT) != 0);
	  obj->pointee = ((attr & CO_ATTR_POINTEE) != 0);
	  obj->arraydec = ((attr & CO_ATTR_ARRAY_DEC) != 0);
	  obj->geomdec = ((attr & CO_ATTR_GEOM_DEC) != 0);

	  if ((attr & CO_ATTR_CHAR) != 0) {
	    c = token ();
	    if (*c == '*')
	      obj->cltype = 1;
	    else
	      obj->charlen = atol (c);
	  }
	  else
	    obj->charlen = 0;
	  if ((attr & CO_ATTR_DIM) == 0)
	    obj->ndims = 0;
	  else {
	    obj->ndims = atoi (token());
	    dim = obj->dim = (struct Cif_dim *)_Cif_space[lmode]
	      (sizeof(struct Cif_dim)*obj->ndims, lcifd);

	    if (dim == NULL)
	      return (CIF_NOMEM);
	    for (i=0; i < (int) obj->ndims; i++) {
	      c = token ();
	      if (*c == 'E')
		dim->ltype = CIF_DM_EXPR;
	      else if (*c == '*')
		dim->ltype = CIF_DM_ASSUMED;
	      else {
		dim->ltype = CIF_DM_CONSTANT;
		dim->lower = atol (c);
	      }
	      c = token ();
	      if (*c == 'E')
		dim->utype = CIF_DM_EXPR;
	      else if (*c == '*')
		dim->utype = CIF_DM_ASSUMED;
	      else {
		dim->utype = CIF_DM_CONSTANT;
		dim->upper = atol (c);
	      }
	      dim++;
	    }
	  }

	  if (_Cif_filetbl[lcifd].version != 1) { /*
						   * v1 cif doesn't have the distribution,
						   * geometry id or pointer fields
						   */
	    /*
	     * read the token first to see of it is not null; it is possible that
	     * a non-MPP compkiler will not put out these fields, or they will be NULL
	     */

	    c = token();
	    if (c != (char *) NULL &&
		*c != (char) NULL)
	      obj->dist = atoi(c);

	    if (obj->pointee || obj->dist == 3) {   /* 3 is SHARED dimensional */
	      c = token();
	      if (c != (char *) NULL &&
		  *c != (char) NULL)
		if (obj->pointee)
		  obj->pointer = atol(c);
	      else
		obj->geomid = atol(c);
	    }
	  /* else the distribution, geometry and pointer fields have already been set to zero */

	  }
	}

	return (CIF_OBJECT);

}

static int ascii_opt_opts (oo)
struct Cif_opt_opts *oo;
{
	/*
	 * If the user wants a v1 cif, then we need to use a different data
	 * structure
	 */

	if (_Cif_filetbl[lcifd].return_version == 1) {
	  struct Cif_opt_opts_1 *oo1 = (struct Cif_opt_opts_1 *) oo;

	  oo1->values = strtol (token(), (char **)NULL, 16);

	  /* No inline level field in a v1 cif */

	  if (_Cif_filetbl[lcifd].version != 1) { /*
						   * but we are reading a v2 cif
						   * which has the inline level field
						   */
	    if (oo1->values == CIF_OOF_INLINE)
	      (void) token();
	  }


	}
	else {  /* returning a v2 cif */

	  oo->values = strtol (token(), (char **)NULL, 16);

	  if (_Cif_filetbl[lcifd].version != 1) { /*
						   * v1 cif doesn't have the inline level field
						   */
	    if (oo->values == CIF_OOF_INLINE)
	      oo->inlevel = atoi(token());
	  }
	}

	return (CIF_OPT_OPTS);

}

static int ascii_srcfile (src)
struct Cif_srcfile *src;
{

	src->fid = atol (token());
	if (delim == SEPARATOR)
		src->form = atoi (token());
	else
		src->form = 0;
	return (CIF_SRCFILE);

}

static int ascii_transform (tran)
struct Cif_transform *tran;
{
	tran->type = atoi(token());
	tran->fid = atol(token());
	tran->line = atol(token());

	return (CIF_TRANSFORM);

}

static int ascii_stmt_type (stmt)
struct Cif_stmt_type *stmt;
{

	stmt->type = atol (token());
	stmt->fid = atol (token());
	stmt->line = atol (token());
	stmt->cpos = atol (token());

	if ( *ntoken != 0 )
	    stmt->efid = atol (token());
	if ( *ntoken != 0 )
	    stmt->eline = atol (token());
	if ( *ntoken != 0 )
	    stmt->ecpos = atol (token());

	/* If we are returning a version 1 cif and the stmt type == CDIR,
	 * then don't return it as version 1 cif's didn't have cdir stmts
	 */

	if (stmt->type == CIF_TP_CDIR &&
	    _Cif_filetbl[lcifd].return_version == 1) {
	  return ( CIF_MAXRECORD );  /* flags an invalid record */

	}

	return (CIF_STMT_TYPE);

}

static int ascii_summary (sum)
struct Cif_summary *sum;
{

	(void) strcpy (sum->level, token());
	(void) strcpy (sum->gdate, token());
	(void) strcpy (sum->gtime, token());
	(void) strcpy (sum->ctime, token());
	sum->fldlen = atol (token());
	sum->nlines = atol (token());
	sum->csize = atol (token());
	sum->dsize = atol (token());
	return (CIF_SUMMARY);

}


static int ascii_cdir (cdir)
struct Cif_cdir *cdir;
{
	register long i;

  	cdir->type = atoi (token());
	cdir->fid = atol (token());
	cdir->line = atol (token());
	cdir->cpos = atol (token());
	cdir->nids = atoi (token());
	if (cdir->nids > 0) {
	  cdir->ids = (long *)_Cif_space[lmode] (sizeof(long)*(cdir->nids), lcifd);
	  for (i = 0; i < (int) cdir->nids; i++) {
	    cdir->ids[i] = atol (token());
	  }
	}

  	return(CIF_CDIR);
}


static int ascii_cdir_doshared (dos)
struct Cif_cdir_doshared *dos;
{
	register long i;
	char *c;

  	dos->type = atoi (token());
	c = token();
	dos->random = (*c == '1' ? 1 : 0);
	dos->fid = atol (token());
	dos->line = atol (token());
	dos->cpos = atol (token());
	c = token();
	if (*c == 'E') {
	  dos->mexpr = 1;
	  dos->m = atol(c);
	}
	else {
	  dos->mexpr = 0;
	}
	dos->mfid = atol (token());
	dos->mline = atol (token());
	dos->mcpos = atol (token());
	dos->nids = atoi (token());
	if (dos->nids > 0) {
	  dos->ids = (long *)_Cif_space[lmode] (sizeof(long)*(dos->nids), lcifd);
	  for (i = 0; i < (int) dos->nids; i++) {
	    dos->ids[i] = atol (token());
	  }
	}

  	return(CIF_CDIR_DOSHARED);
}

static int ascii_geometry (geom)
struct Cif_geometry *geom;
{
	register long i;
  	char *c;
	struct Cif_geometry_dim *dim;

  	c = token();
	if (c != (char *) NULL) {
	  	geom->nlen = i = strlen (c);
		geom->name = _Cif_space[lmode] (i+1, lcifd);
		if (geom->name == NULL)
		  	return (CIF_NOMEM);
		(void) strcpy (geom->name, c);
	}
	else
	  	geom->nlen = 0;

	geom->geomid = atol (token());
	geom->ndims = atoi (token());
	dim = geom->dim = (struct Cif_geometry_dim *)_Cif_space[lmode]
	  			(sizeof(struct Cif_geometry_dim)*geom->ndims, lcifd);
	if (dim == NULL)
	  	return (CIF_NOMEM);
	for (i=0; i < (int) geom->ndims; i++) {
	  	dim->dist = strtol (token(), (char **)NULL, 16);
		c = token();
		if (*c == 'E') {
		  	dim->wtype = 1;
			dim->weight = 0;
		}
		else {
		  	dim->wtype = 0;
			dim->weight = atol (c);
		}
		dim->wfid = atol (token());
		dim->wline = atol (token());
		dim->wcpos = atol (token());
		c = token();
		if (*c == 'E') {
		  	dim->btype = 1;
			dim->bsize = 0;
		}
		else {
		  	dim->btype = 0;
			dim->bsize = atol (c);
		}
		dim->bfid = atol (token());
		dim->bline = atol (token());
		dim->bcpos = atol (token());

		dim++;
      }

	return(CIF_GEOMETRY);
}

static int ascii_continuation (co)
struct Cif_continuation *co;
{
  	char *c;

	c = token();
  	co->type = (*c == '0' ? 0 : 1);
	co->fid = atol (token());
	co->line = atol (token());
	co->cpos = atol (token());

  	return(CIF_CONTINUATION);
}


#ifndef CRAY2
static int ascii_f90_callsite (cs)
struct Cif_f90_callsite *cs;
{
	register int i, j;
	register int nargs;
	register char *c;

	cs->entryid = atol (token());
	cs->scopeid = atol (token());
	cs->fid = atol (token());
	cs->line = atol (token());
	cs->cpos = atol (token());
	cs->procid = atol (token());
	nargs = atoi (token());
	if (nargs >= 0)
	    cs->nargs = nargs;
	else
	    cs->nargs = 0;
	if (nargs > 0) {

		cs->argids = (long *)_Cif_space[lmode] (sizeof(long)*nargs, lcifd);
		if (cs->argids == NULL)
			return (CIF_NOMEM);

		cs->nmembs = (int *)_Cif_space[lmode] (sizeof(long)*nargs, lcifd);
		if (cs->nmembs == NULL)
			return (CIF_NOMEM);

		cs->membs = (long **)_Cif_space[lmode] (sizeof(long *)*nargs, lcifd);
		if (cs->membs == NULL)
			return (CIF_NOMEM);

		for (i = 0; i < nargs; i++) {
		  	c = token();
			/*
			 * If the symbol id has members, this field will be %,
			 * otherwise it is the symbol id
			 */

			if (*c == '%') {
			  cs->nmembs[i] = atoi (token()) - 1;
			  cs->argids[i] = atol (token());

			  cs->membs[i] =
			    (long *)_Cif_space[lmode] (sizeof(long)*cs->nmembs[i], lcifd);
			  for (j = 0; j < cs->nmembs[i]; j++) {
			    cs->membs[i][j] = atol( token());
			  }
			  /* now read the lst '%'...redundant really, but no harm done */
			  (void) token();
		      }
			else {
			    cs->argids[i] = atol (c);
			    cs->nmembs[i] = 0;
			    cs->membs[i] = 0;
			}
		    }

	}

/*  Not issued by the f90 compiler at all; left for comment only
	if (delim == SEPARATOR)
		cs->valused = (*token() == 'F' ? 0 : 1);
*/

	if (delim == SEPARATOR) {

	    cs->rank = 1;
	    cs->ranks = (int *)_Cif_space[lmode] (sizeof(int)*nargs, lcifd);
	    if (cs->ranks == NULL)
		return (CIF_NOMEM);

	    for (i = 0; i < nargs; i++) {
		cs->ranks[i] = atoi(token());
	    }
	}

	return(CIF_F90_CALLSITE);
}


static int ascii_f90_comblk (cb)
struct Cif_f90_comblk *cb;
{
  	register char *c;
	register long i;

	c = token();
	cb->nlen = i = strlen (c);
	cb->name = _Cif_space[lmode] (i+1, lcifd);
	if (cb->name == NULL)
		return (CIF_NOMEM);
	(void) strcpy (cb->name, c);
	cb->symid = atol (token());
	cb->scopeid = atol (token());
	cb->cbtype = atoi (token());
	cb->moduleid = atol (token());
	cb->length = atol (token());
	c = token();
	if (c != (char *) NULL)
	    cb->dist = atoi (c);

	return(CIF_F90_COMBLK);
}


static int ascii_f90_const (con)
struct Cif_f90_const *con;
{
	register int i;
	register char *c;

	con->symid = atol (token());
	con->scopeid = atol (token());
	c = token();
	con->aggregate = (*c == '0' ? 0 : 1);

	/* get constant value - multiple values not implemented */
	
	if (con->aggregate == 0) {
	  	con->vlen = i = strlen (c = token());
		con->value = _Cif_space[lmode] (i+1, lcifd);
		if (con->value == NULL)
			return (CIF_NOMEM);
		(void) strcpy (con->value, c);
	      }
	else
	  c = token();  /* pass the null field for an aggregate constant */

	con->fid = atol (token());
	con->strline = atol (token());
	con->strpos = atol (token());
	con->endline = atol (token());
	con->endpos = atol (token());

	return(CIF_F90_CONST);
}


static int ascii_f90_entry (entry)
struct Cif_f90_entry *entry;
{

	register char *c;
	register long i, len;

	c = token();
	entry->nlen = len = strlen (c);
	entry->name = _Cif_space[lmode] (len+1, lcifd);
	if (entry->name == NULL)
		return (CIF_NOMEM);
	(void) strcpy (entry->name, c);
	entry->symid = atol (token());
	entry->scopeid = atol (token());
	entry->etype = atoi (token());
	entry->ptype = atoi (token());

	/* get attributes */

	i = strtol (token(), (char **)NULL, 16);
	entry->defined = ((i & F90_EN_ATTR_DEFINED) != 0);
	entry->intblock = ((i & F90_EN_ATTR_INT_BLOCK) != 0);
	entry->referenced = ((i & F90_EN_ATTR_REFERENCED) != 0);
	entry->optional = ((i & F90_EN_ATTR_OPTIONAL) != 0);
	entry->priv = ((i & F90_EN_ATTR_PRIVATE) != 0);
	entry->recur = ((i & F90_EN_ATTR_RECUR) != 0);
	entry->useassoc = ((i & F90_EN_ATTR_USE) != 0);

	entry->stmtfunc = (entry->etype == CIF_F90_ET_STMT);

	entry->resultid = atol (token());

	entry->moduleid = atol (token());

	/* get argument ids */

	if ( (len = atoi (token())) >= 0) {
		entry->valargs = 1;
		if (len > 0) {
			entry->nargs = len;
			entry->argids = (long *)_Cif_space[lmode] (sizeof(long)*len, lcifd);
			if (entry->argids == NULL)
				return (CIF_NOMEM);
			for (i = 0; i < len; i++)
				(entry->argids)[i] = atol (token());
		}
        }
	else {
	  entry->valargs = 0;
	  entry->nargs = 0;
	}


	return(CIF_F90_ENTRY);
}


static int ascii_f90_loop (loop)
struct Cif_f90_loop *loop;
{
  int statementID;

  	loop->scopeid = atol (token());
	loop->lptype = atol (token());
	loop->sfid = atol (token());
	loop->strline = atol (token());
	loop->strcpos = atol (token());
	loop->efid = atol (token());
	loop->endline = atol (token());
	loop->endcpos = atol (token());
	if (delim == SEPARATOR)
		loop->symid = atol (token());
	if (delim == SEPARATOR)
		loop->labelid = atol (token());
	if (delim == SEPARATOR)
		loop->nameid = atol (token());
	/* Statement id's for the loops terminating line */
	if (delim == SEPARATOR) {
	  statementID = atol (token());
          /* No space to hold the statement field directly, so we have
           * to split the value into 3 parts - done magically through
           * setStmtid */
	  setStmtid(loop, statementID);
	}

	return(CIF_F90_LOOP);
}


static int ascii_f90_derived_type (dt)
struct Cif_f90_derived_type *dt;
{
  	register char *c;
	register long i, len;

	if (_Cif_filetbl[lcifd].return_version <= 2) {
	  struct Cif_f90_derived_type_2 *dt2 = (struct Cif_f90_derived_type_2 *) dt;

	  c = token();
	  dt2->nlen = len = strlen (c);
	  dt2->name = _Cif_space[lmode] (len+1, lcifd);
	  if (dt2->name == NULL)
	      return (CIF_NOMEM);
	  (void) strcpy (dt2->name, c);
	  dt2->symid = atol (token());
	  dt2->scopeid = atol (token());
	  dt2->dervtype = atol (token());

	  dt2->flag = strtol (token(), (char **)NULL, 16);

	  dt2->sequence = ((dt2->flag & CIF_DRT_SEQUENCE) != 0);
	  dt2->defprivate = ((dt2->flag & CIF_DRT_PRIVATE) != 0);
	  dt2->comprivate = ((dt2->flag & CIF_DRT_COMP_PRIVATE) != 0);

	  /* read member ids */

	  if ( (len = atoi (token())) > 0) {
	    dt2->nmembs = len;
	    dt2->memids = (long *)_Cif_space[lmode] (sizeof(long)*len, lcifd);
	    if (dt2->memids == NULL)
		return (CIF_NOMEM);
	    for (i = 0; i < len; i++)
		(dt2->memids)[i] = atol (token());
	  }
	}
	else { /* version 3 CIF record, as above + add the moduleid field */
	  c = token();
	  dt->nlen = len = strlen (c);
	  dt->name = _Cif_space[lmode] (len+1, lcifd);
	  if (dt->name == NULL)
	      return (CIF_NOMEM);
	  (void) strcpy (dt->name, c);
	  dt->symid = atol (token());
	  dt->scopeid = atol (token());
	  dt->dervtype = atol (token());

	  dt->flag = strtol (token(), (char **)NULL, 16);

	  dt->sequence = ((dt->flag & CIF_DRT_SEQUENCE) != 0);
	  dt->defprivate = ((dt->flag & CIF_DRT_PRIVATE) != 0);
	  dt->comprivate = ((dt->flag & CIF_DRT_COMP_PRIVATE) != 0);

	  /* read member ids */

	  if ( (len = atoi (token())) > 0) {
	    dt->nmembs = len;
	    dt->memids = (long *)_Cif_space[lmode] (sizeof(long)*len, lcifd);
	    if (dt->memids == NULL)
		return (CIF_NOMEM);
	    for (i = 0; i < len; i++)
		(dt->memids)[i] = atol (token());
	  }

	  /* moduleid's are only present in V3 CIF's */
	  if (_Cif_filetbl[lcifd].version >= 3 &&
	      delim == SEPARATOR) {
	    dt->moduleid = atol (token());
	  }
	}

	return (CIF_F90_DERIVED_TYPE);
}



static int ascii_f90_label (label)
struct Cif_f90_label *label;
{
	register char *c;
	register long i;

	c = token();
	label->nlen = i = strlen (c);
	label->name = _Cif_space[lmode] (i+1, lcifd);
	if (label->name == NULL)
		return (CIF_NOMEM);
	(void) strcpy (label->name, c);
	label->symid = atol (token());
	label->scopeid = atol (token());
	label->ltype = atoi (token());

	return(CIF_F90_LABEL);
}


static int ascii_f90_namelist (nl)
struct Cif_f90_namelist *nl;
{
	register long i;
	register char *c;

	c = token();
	nl->nlen = i = strlen (c);
	nl->name = _Cif_space[lmode] (i+1, lcifd);
	if (nl->name == NULL)
		return (CIF_NOMEM);
	(void) strcpy (nl->name, c);
	nl->symid = atol (token());
	nl->scopeid = atol (token());
	nl->moduleid = atol (token());
	if ((nl->nids = atoi (token())) > 0) {
		nl->ids = (long *) _Cif_space[lmode] (sizeof(long)*nl->nids, lcifd);
		if (nl->ids == NULL)
			return (CIF_NOMEM);
		for (i = 0; i < (int) nl->nids; i++)
			(nl->ids)[i] = atol (token());

	}

	return(CIF_F90_NAMELIST);
}


static int ascii_f90_object (obj)
struct Cif_f90_object *obj;
{
	register char *c;
	register long i, attr, storeagid;
	struct Cif_dim *dim;

	c = token();
	if ((obj->nlen = i = strlen (c)) > 0) {
		obj->name = _Cif_space[lmode] (i+1, lcifd);
		if (obj->name == NULL)
			return (CIF_NOMEM);
		(void) strcpy (obj->name, c);
	}
	else
		obj->name = NULL;
	obj->symid = atol (token());
	obj->scopeid = atol (token());

	/*
	 * Map the compiler generated data type into a new value which
	 * corresponds where it can with f77 and is non-overlapping for
	 * other cases that have no direct equivalent; see CIF_[F90_]_DT_?? in cif.h
	 */
	obj->dtype = atoi (token());
	if (_Cif_filetbl[lcifd].version < 3) {
	    if (obj->dtype < CIF_F90_DT_MAX)
		obj->dtype = _Cif_f90_to_f77_dtypes[obj->dtype];
	    /* else it's a derived type, so leave the number */
	}

	obj->symclass = atoi (token());
	obj->storage = atol (token());

	c = token();
	if (c != (char *) NULL &&
	    *c != (char) NULL) {
	    storeagid = atol(c);
	    if (storeagid < 0)
		obj->storageid = 0;
	    else
		obj->storageid = storeagid;
	}

	c = token();
	if (*c != '\0' &&
	    (i = atol (c)) >= 0) {
		obj->valoffset = 1;
		obj->offset = i;
	}


	/* get attributes */
	attr = strtol (token(), (char **)NULL, 16);
	obj->imptype = ((attr & F90_CO_ATTR_IMPTYPE) != 0);
	obj->pointee = ((attr & F90_CO_ATTR_POINTEE) != 0);
	obj->deftype = ((attr & F90_CO_ATTR_DEF_TYPE) != 0);
	obj->startype = ((attr & F90_CO_ATTR_STAR_TYPE) != 0);
	obj->kindtype = ((attr & F90_CO_ATTR_KIND_TYPE) != 0);
	obj->save = ((attr & F90_CO_ATTR_SAVE) != 0);
	obj->data = ((attr & F90_CO_ATTR_DATA) != 0);
	obj->equiv = ((attr & F90_CO_ATTR_EQUIV) != 0);
	obj->arraydec = ((attr & F90_CO_ATTR_ARRAY_DEC) != 0);
	obj->geomdec = ((attr & F90_CO_ATTR_GEOM_DEC) != 0);
	obj->peresident = ((attr & F90_CO_ATTR_PE_RESIDENT) != 0);
	obj->allocatable = ((attr & F90_CO_ATTR_ALLOCATABLE) != 0);
	obj->intentin = ((attr & F90_CO_ATTR_INTENTIN) != 0);
	obj->intentout = ((attr & F90_CO_ATTR_INTENTOUT) != 0);
	obj->intentinout = ((attr & F90_CO_ATTR_INTENTINOUT) != 0);
	obj->optional = ((attr & F90_CO_ATTR_OPTIONAL) != 0);
	obj->pointer = ((attr & F90_CO_ATTR_POINTER) != 0);
	obj->priv = ((attr & F90_CO_ATTR_PRIVATE) != 0);
	obj->target = ((attr & F90_CO_ATTR_TARGET) != 0);
	obj->localname = ((attr & F90_CO_ATTR_LOCAL_NAME) != 0);

	/* we only get a derived type is when this object is a component of structure */

	if (obj->symclass == CIF_F90_SC_STRUCT)
	  obj->dervid = atol (token());
	else
	  c = token();

	c = token ();
	if (*c == 'E')
	  obj->chartype = CIF_DM_EXPR;
	else if (*c == '*')
	  obj->chartype = CIF_DM_ASSUMED;
	else {
	  obj->chartype = CIF_DM_CONSTANT;
	  obj->charlen = atol (c);
	}

	obj->ndims = atoi (token());

	if (obj->ndims == 0) {
	  c = token();  /* remove the array type which isn't valid for a non-array (scalar) */
	}
	else {
	  obj->atype = atoi(token());
	  if (obj->atype != CIF_AT_DEFERRED) {  /* deferred arrays have have all dimensions
						 * assumed as ':', so are not given in the cif
						 */

	    dim = obj->dim = (struct Cif_dim *)_Cif_space[lmode]
	      (sizeof(struct Cif_dim)*obj->ndims, lcifd);
	    if (dim == NULL)
	      return (CIF_NOMEM);
	    for (i=0; i < (int) obj->ndims; i++) {
	      c = token ();
	      if (*c == 'E') {
		dim->ltype = CIF_DM_EXPR;
		dim->lower = 0;
	      } else if (*c == '*') {
		dim->ltype = CIF_DM_ASSUMED;
		dim->lower = 0;
	      } else {
		dim->ltype = CIF_DM_CONSTANT;
		dim->lower = atol (c);
	      }
	      if (obj->atype == CIF_AT_ASSUMED) {
		dim->utype = CIF_DM_ASSUMED;
		dim->upper = 0;
	      }
	      else {
		c = token ();
		if (*c == 'E') {
		  dim->utype = CIF_DM_EXPR;
		  dim->upper = 0;
		} else if (*c == '*') {
		  dim->utype = CIF_DM_ASSUMED;
		  dim->upper = 0;
		} else {
		  dim->utype = CIF_DM_CONSTANT;
		  dim->upper = atol (c);
		}
	      }
	      dim++;
	    }
	  }
      }

	/* read distribution code, arrays only */
	c = token();
	if (c != (char *) NULL &&
	    *c != (char) NULL) {
	    obj->dist = atoi (c);
	}

	/* read geometry id, present only for certain array distributions */
	c = token();
	if (c != (char *) NULL &&
	    *c != (char) NULL) {
	    obj->geomid = atol (c);
	}

	/* read pointer id, only if this is a CRI pointee */

	c = token();
	if (c != (char *) NULL &&
	    *c != (char) NULL) {
	    obj->pointerid = atol (c);
	}

	return(CIF_F90_OBJECT);
}


static int ascii_f90_misc_opts (mo)
struct Cif_f90_misc_opts *mo;
{
	register int i;
	register char *c;
	register int tmp;

	mo->intlen = atoi (token());
	mo->msglvl = atoi (token());
	mo->vopt = atoi (token());
	mo->trunc = atoi (token ());
	mo->truncval = atoi (token());
	tmp = llist(&(mo->msgno), (int *) NULL);
	if (tmp < 0)
	  return (CIF_NOMEM);
	mo->nmsgs = tmp;

	tmp = strlist (&(mo->cdirs));
	if (tmp < 0)
	  return (CIF_NOMEM);
	mo->ncdirs = tmp;

	c = token();
	if ((mo->onlen = i = strlen (c)) > 0) {
		mo->objname = _Cif_space[lmode] (i+1, lcifd);
		if (mo->objname == NULL)
			return (CIF_NOMEM);
		(void) strcpy (mo->objname, c);
	}
	c = token();
	if ((mo->cnlen = i = strlen (c)) > 0) {
		mo->calname = _Cif_space[lmode] (i+1, lcifd);
		if (mo->calname == NULL)
			return (CIF_NOMEM);
		(void) strcpy (mo->calname, c);
	}

	c = token();
	if ((mo->inlen = i = strlen (c)) > 0) {
		mo->inname = _Cif_space[lmode] (i+1, lcifd);
		if (mo->inname == NULL)
			return (CIF_NOMEM);
		(void) strcpy (mo->inname, c);
	}

	c = token();
	if ((mo->ciflen = i = strlen (c)) > 0) {
		mo->cifname = _Cif_space[lmode] (i+1, lcifd);
		if (mo->cifname == NULL)
			return (CIF_NOMEM);
		(void) strcpy (mo->cifname, c);
	}

	mo->cifopts = strtol (token(), (char **)NULL, 16);
	mo->swidth = atoi (token ());

	tmp = strlist (&(mo->Pdirs));
	if (tmp < 0)
	  return (CIF_NOMEM);
	mo->nPdirs = tmp;

	tmp = strlist (&(mo->pdirs));
	if (tmp < 0)
	  return (CIF_NOMEM);
	mo->npdirs = tmp;

	c = token();
	mo->srcform = (*c == '0' ? 0 : 1);

	/*
	 * If we are not at a separator, we must be at the end of line,
	 * which means that there are no more records
	 */
	if (delim == SEPARATOR) {
	    mo->runtime = strtol (token(), (char **)NULL, 16);
	}

	return(CIF_F90_MISC_OPTS);
}


static int ascii_f90_opt_opts (opt)
struct Cif_f90_opt_opts *opt;
{
	register int i;
	struct Cif_f90_level_opts *optlevel;

  	opt->values = strtol (token(), (char **)NULL, 16);
	opt->noptlevels = atoi (token());
	optlevel = opt->lopts = (struct Cif_f90_level_opts *)_Cif_space[lmode]
		  (sizeof(struct Cif_f90_level_opts)*opt->noptlevels, lcifd);
	if (optlevel== NULL)
	  return (CIF_NOMEM);
	for (i=0; i < (int) opt->noptlevels; i++) {
	  optlevel->optinlevel = strtol (token(), (char **)NULL, 16);
	  optlevel->level = atoi (token ());
	  optlevel++;
	}
	opt->newdef = 1; /* internal flag set on this version of the cif
			  * to allow cifbinread to correctly read both old
			  * and new binary formats */

	return(CIF_F90_OPT_OPTS);
}


static int ascii_f90_begin_scope (bs)
struct Cif_f90_begin_scope *bs;
{
  	bs->scopeid = atol (token ());
	bs->symid = atol (token ());
	bs->fid = atol (token ());
	bs->line = atol (token ());
	bs->cpos = atol (token ());
	bs->stype = atol (token ());
	bs->level = atoi (token ());
	bs->parentid = atol (token ());

	return(CIF_F90_BEGIN_SCOPE);
}


static int ascii_f90_end_scope (es)
struct Cif_f90_end_scope *es;
{
  	es->scopeid = atol (token ());
	es->fid = atol (token ());
	es->line = atol (token ());
	es->cpos = atol (token ());
	es->error = atoi (token ());

	return(CIF_F90_END_SCOPE);
}


static int ascii_f90_scope_info (si)
struct Cif_f90_scope_info *si;
{
	register long attr;
	register int i;

	si->scopeid = atol (token ());

	attr = strtol (token(), (char **)NULL, 16);
	si->impnone = ((attr & SC_ATTR_IMPNONE) != 0);
	si->doesio = ((attr & SC_ATTR_IO) != 0);
	si->hascalls = ((attr & SC_ATTR_CALL) != 0);
	si->hascmics = ((attr & SC_ATTR_CMIC) != 0);

	/*
	 * If we are not at a separator, we must be at the end of line,
	 * which means that there are no more records
	 */
	if (delim == SEPARATOR) {
	  	si->numalts = atoi (token ());
		if (si->numalts > 0) {
		  	si->entryids =
			  (long *) _Cif_space[lmode] (sizeof(long)*si->numalts, lcifd);
			if (si->entryids == NULL)
			  	return (CIF_NOMEM);
			for (i = 0; i < (int) si->numalts; i++)
			  (si->entryids)[i] = atol (token());
		      }
	}

	return(CIF_F90_SCOPE_INFO);
}


static int ascii_f90_use_module (um)
struct Cif_f90_use_module *um;
{
  	um->modid = atol (token ());
  	um->modfid = atol (token ());
  	um->direct = atoi (token ());

	return(CIF_F90_USE_MODULE);
}


static int ascii_f90_rename (rn)
struct Cif_f90_rename *rn;
{
  	register char *c;
	register int i;
	register int max_id = 5;

  	rn->scopeid = atol (token ());
	c = token();
	if ((rn->nlen = i = strlen (c)) > 0) {
		rn->name = _Cif_space[lmode] (i+1, lcifd);
		if (rn->name == NULL)
			return (CIF_NOMEM);
		(void) strcpy (rn->name, c);
	}

  	rn->nameid = atol (token ());
  	rn->modid = atol (token ());

	c = token();
	if ((rn->orignlen = i = strlen (c)) > 0) {
		rn->origname = _Cif_space[lmode] (i+1, lcifd);
		if (rn->origname == NULL)
			return (CIF_NOMEM);
		(void) strcpy (rn->origname, c);
	}

  	rn->origmodid = atol (token ());

	/*
	 * Assume a max of 5 and increase as necessary; will an object
	 * ever be renamed more than 5 times ?
	 */

	rn->localid = (long *) malloc (sizeof(long) * max_id);
	i = 0;
	while (1) {
	    c = token();
	    if (c != (char *) NULL &&
		*c != (char) NULL) {

		rn->localid[i] = atol (c);
		i++;
		if (i == max_id) {
		    max_id+=5;
		    rn->localid = (long *) realloc((char *) rn->localid,
						   sizeof(long) * max_id);   
		}
	    }
	    else {
		break;
	    }
	}
	rn->nlocalids = i;

  	return(CIF_F90_RENAME);
}


static int ascii_f90_int_block (ib)
struct Cif_f90_int_block *ib;
{
  	register char *c;
	register int i;

	if (_Cif_filetbl[lcifd].return_version <= 2) {
	  struct Cif_f90_int_block_2 *ib2 = (struct Cif_f90_int_block_2 *) ib;

	  c = token();
	  if ((ib2->nlen = i = strlen (c)) > 0) {
	    ib2->name = _Cif_space[lmode] (i+1, lcifd);
	    if (ib2->name == NULL)
		return (CIF_NOMEM);
	    (void) strcpy (ib2->name, c);
	  }

	  ib2->intid = atol (token ());
	  ib2->scopeid = atol (token());
	  ib2->type = atoi (token ());

	  /*
	   * this next field could be a set of attributes, but for now it's
	   * only possible value is 1 for PRIVATE
	   */

	  ib2->priv = (*token() == '1');

	  if ((ib2->numints = atoi (token())) > 0) {
	    ib2->procids = (long *) _Cif_space[lmode] (sizeof(long)*ib2->numints, lcifd);
	    if (ib2->procids == NULL)
		return (CIF_NOMEM);
	    for (i = 0; i < (int) ib2->numints; i++)
		(ib2->procids)[i] = atol (token());

	  }
	}
	else {  /* version 3 CIF record, as above + add the moduleid field */

	  c = token();
	  if ((ib->nlen = i = strlen (c)) > 0) {
	    ib->name = _Cif_space[lmode] (i+1, lcifd);
	    if (ib->name == NULL)
		return (CIF_NOMEM);
	    (void) strcpy (ib->name, c);
	  }

	  ib->intid = atol (token ());
	  ib->scopeid = atol (token());
	  ib->type = atoi (token ());

	  /*
	   * this next field could be a set of attributes, but for now it's
	   * only possible value is 1 for PRIVATE
	   */

	  ib->priv = (*token() == '1');

	  if ((ib->numints = atoi (token())) > 0) {
	    ib->procids = (long *) _Cif_space[lmode] (sizeof(long)*ib->numints, lcifd);
	    if (ib->procids == NULL)
		return (CIF_NOMEM);
	    for (i = 0; i < (int) ib->numints; i++)
		(ib->procids)[i] = atol (token());
	  }

	  /* moduleid's are only present in V3 CIF's */
	  if (_Cif_filetbl[lcifd].version >= 3 &&
	      delim == SEPARATOR) {
	    ib->moduleid = atol (token());
	  }
	}

  	return(CIF_F90_INT_BLOCK);
}


static int ascii_f90_vectorization (vect)
struct Cif_f90_vectorization *vect;
{
    (void) fprintf(stderr, "libcif: vectorization message %p\n", vect);
    return(CIF_F90_VECTORIZATION);
}
#endif /* ndef CRAY2 */





static int ascii_unit (unit)
struct Cif_unit *unit;
{
	register int i;
	register char *c;

	c = token();
	unit->nlen = i = strlen (c);
	unit->name = _Cif_space[lmode] (i+1, lcifd);
	if (unit->name == NULL)
		return (CIF_NOMEM);
	(void) strcpy (unit->name, c);
	unit->fid = atol (token());
	unit->line = atol (token());
	unit->cpos = atol (token());
	return (CIF_UNIT);

}

static int ascii_endunit (eu)
struct Cif_endunit *eu;
{
	register int i;
	register char *c;

	c = token();
	eu->nlen = i = strlen (c);
	eu->name = _Cif_space[lmode] (i+1, lcifd);
	if (eu->name == NULL)
		return (CIF_NOMEM);
	(void) strcpy (eu->name, c);
	eu->fid = atol (token());
	eu->line = atol (token());
	eu->cpos = atol (token());
	return (CIF_ENDUNIT);

}

static int ascii_usage (usage)
struct Cif_usage *usage;
{
#	define UBINCR 5	            	/* use buffer increment value */

	register char *c;
	long i, nuses, note_pos = -10; /* a file position, note 0 and -1 have meaning, so
					* -10 is used to be 'undefined'
					*/

	long utype;

	static struct Cif_use *ubuff = NULL;	/* pointer to base of use buffer */
	static int ubi;			        /* index to next slot in ubuff */
	static int ubsize = 0;		       	/* current max size of ubuff */


	/*
	 * If the user wants a v1 cif, then we need to use a different data
	 * structure
	 */

	if (_Cif_filetbl[lcifd].return_version == 1) {
	  struct Cif_usage_1 *usage1 = (struct Cif_usage_1 *) usage;

	  usage1->symid = atol (token());
	  ubi = 0;

	  while (1) {

	    /* count up number of uses in record */

	    i = 0;
	    c = ntoken;
	    while (*c != '\0')
	      if (*c++ == SEPARATOR) i++;
	    nuses = (++i) / 4;

	    /* if not enough space in use buffer, make buffer bigger */

	    if (ubi + nuses > ubsize) {
	      if (ubsize == 0) {
		ubsize = UBINCR;

		ubuff = (struct Cif_use *) malloc (sizeof(struct Cif_use)*UBINCR);
	      }
	      else {
		ubsize += UBINCR;
		ubuff = (struct Cif_use *)realloc (ubuff,
 						   sizeof(struct Cif_use)*ubsize);
	      }
	      if (ubuff == NULL)
		return (CIF_NOMEM);
	    }
	    for (i = 0; i < nuses; i++) {
	      ubuff[ubi].fid = atol (token());
	      ubuff[ubi].line = atol (token());
	      ubuff[ubi].cpos = atol (token());

	      /*
	       * Fortran and C usage values have different meanings
	       * In C, values are in hex and bit significant;
	       * Fortran is decimal and different nummeric values
	       * represent different usages with no bit significance
	       */

	      if (_Cif_filetbl[lcifd].lang == CIF_LG_C ||
                  _Cif_filetbl[lcifd].lang == CIF_LG_CC) {
		ubuff[ubi].utype =
		  strtol (token(), (char **)NULL, 16);
	      }
	      else {
		utype = atol (token());
		ubuff[ubi].utype = utype % 100;
		/*
		 * in a v2 cif, a usage type > 100 indicates that the object
		 * is used in a data statement
		 */
		if (_Cif_filetbl[lcifd].return_version != 1) {
		    if (_Cif_filetbl[lcifd].return_version != 1) {
			if (utype >= 200)
			    ubuff[ubi].init = 1;
			else
			    if (utype >= 100)
				ubuff[ubi].data = 1;
		    }
		}
	      }
	      ubi++;
	    }

	    /*
	     * Read next record.  If usage and same object, go back and add to
	     * current list.  Otherwise, mark that buffer contains record and go on.
	     */

	    /*
	     * note where we are now so that if the next record is not
	     * a usage we can position the curent pos at it correctly
	     */

	    note_pos = Cif_Getpos(lcifd);

	    if (fgets (_Cif_filetbl[lcifd].ip, CIF_BUFSIZE, _Cif_filetbl[lcifd].fd)
		== NULL)
	      {
		if (feof(_Cif_filetbl[lcifd].fd))
		  break;
		else
		  return (CIF_SYSERR);
	      }
	    ntoken = _Cif_filetbl[lcifd].ip;
	    if (atoi (token ()) != CIF_USAGE) {
	      _Cif_filetbl[lcifd].ifull = YES;
	      break;
	    }
	    else if (atol (token ()) != usage1->symid) {
	      _Cif_filetbl[lcifd].ifull = YES;
	      break;
	    }
	  }

	  /* Make sure that the file position is correct before we continue */

	  if (note_pos != -10)
	      (void) Cif_Setpos(lcifd, note_pos);

	  /* Sort the usages, allocate a new buffer, and copy them over */

	  if (ubi > 1)
	      (void) qsort ( (char *)ubuff, ubi, sizeof(struct Cif_use), (int(*)()) compuse);
	  i = sizeof(struct Cif_use) * ubi;
	  usage1->use = (struct Cif_use *)_Cif_space[lmode] (i, lcifd);
	  if (usage1->use == NULL)
	    return (CIF_NOMEM);
	  (void) memcpy ((char *)usage1->use, (char *)ubuff, i);
	  usage1->nuses = ubi;
	}

	else  { /* returning a v2 cif */

	  usage->symid = atol (token());
	  ubi = 0;

	  while (1) {

	    /* count up number of uses in record */

	    i = 0;
	    c = ntoken;
	    while (*c != '\0')
	      if (*c++ == SEPARATOR) i++;
	    nuses = (++i) / 4;

	    /* if not enough space in use buffer, make buffer bigger */

	    if (ubi + nuses > ubsize) {
	      if (ubsize == 0) {
		ubsize = UBINCR;
	        ubuff = (struct Cif_use *) malloc (sizeof(struct Cif_use)*UBINCR);
		}
	      else {
		ubsize += UBINCR;
		ubuff = (struct Cif_use *)realloc (ubuff,
 						   sizeof(struct Cif_use)*ubsize);
	      }
	      if (ubuff == NULL)
		return (CIF_NOMEM);
	    }
	    for (i = 0; i < nuses; i++) {
              (void) memset((char *)&ubuff[ubi], 0, sizeof(struct Cif_use));
	      ubuff[ubi].fid = atol (token());
	      ubuff[ubi].line = atol (token());
	      ubuff[ubi].cpos = atol (token());

	      /* Fortran and C usage values have different meanings
	       * In C, values are in hex and bit significant;
	       * Fortran is decimal and different nummeric values
	       * represent different usages with no bit significance
	       */

	      if (_Cif_filetbl[lcifd].lang == CIF_LG_C ||
                  _Cif_filetbl[lcifd].lang == CIF_LG_CC) {
		ubuff[ubi].utype =
		  strtol (token(), (char **)NULL, 16);
	      }
	      else {
		  utype = atol (token());
		  ubuff[ubi].utype = utype % 100;
		  /*
		   * in a v2 cif, a usage type > 100 indicates that the object
		   * is used in a data statement
		   */
		  if (_Cif_filetbl[lcifd].return_version != 1) {
		      if (_Cif_filetbl[lcifd].return_version != 1) {
			  if (utype >= 200)
			      ubuff[ubi].init = 1;
			  else
			      if (utype >= 100)
				  ubuff[ubi].data = 1;
		      }
		  }
	      }
	      ubi++;
	  }

	    /*
	     * Read next record.  If usage and same object, go back and add to
	     * current list.  Otherwise, mark that buffer contains record and go on.
	     */

	    /*
	     * note where we are now so that if the next record is not
	     * a usage we can position the curent pos at it correctly
	     */

	    note_pos = Cif_Getpos(lcifd);

	    if (fgets (_Cif_filetbl[lcifd].ip, CIF_BUFSIZE, _Cif_filetbl[lcifd].fd)
		== NULL)
	      {
		if (feof(_Cif_filetbl[lcifd].fd))
		  break;
		else
		  return (CIF_SYSERR);
	      }
	    ntoken = _Cif_filetbl[lcifd].ip;
	    if (atoi (token ()) != CIF_USAGE) {
	      _Cif_filetbl[lcifd].ifull = YES;
	      break;
	    }
	    else if (atol (token ()) != usage->symid) {
	      _Cif_filetbl[lcifd].ifull = YES;
	      break;
	    }
	  }

	  /* Make sure that the file position is correct before we continue */

	  if (note_pos != -10)
	      (void) Cif_Setpos(lcifd, note_pos);

	  /* Sort the usages, allocate a new buffer, and copy them over */

	  if (ubi > 1)
	      (void) qsort ( (char *)ubuff, ubi, sizeof(struct Cif_use), (int(*)()) compuse);
	  i = sizeof(struct Cif_use) * ubi;
	  usage->use = (struct Cif_use *)_Cif_space[lmode] (i, lcifd);
	  if (usage->use == NULL)
	    return (CIF_NOMEM);
	  (void) memcpy ((char *)usage->use, (char *)ubuff, i);
	  usage->nuses = ubi;

      }

	return (CIF_USAGE);

}


/*
 * cif_usage changed slightly for f90 in that only one usage will
 * be created per record and that there is an optional new last field
 * which describes the member symbol id's; that is if a%b%c is referenced,
 * a will be given as the usual symbol id, b and c will be the member symbol
 * id's. As such it is easier to parse these separately.
 */

static int ascii_f90_usage (usage)
struct Cif_usage *usage;
{
	long i;
	long utype;

	static struct Cif_use *ubuff = NULL;	/* pointer to base of use buffer */

	usage->symid = atol (token());

	ubuff = (struct Cif_use *) _Cif_space[lmode] (sizeof(struct Cif_use),
						      lcifd);
	ubuff->fid = atol (token());
	ubuff->line = atol (token());
	ubuff->cpos = atol (token());
	utype = atol(token());
	ubuff->utype = utype % 100;
 	/*
	 * in a v2 cif, a usage type > 100 indicates that the object
	 * is used in a data statement
	 */
	ubuff->init = 0;
	ubuff->data = 0;
	if (_Cif_filetbl[lcifd].return_version != 1) {
	    if (utype >= 200)
		ubuff->init = 1;
	    else
		if (utype >= 100)
		    ubuff->data = 1;
	}

	/*
	 * Now see if there are any extra symol id's which will represent the
	 * parent symbol id's (see comment at head of this function)
	 */

	/* There are still things to read if we are just at a delimiter */

	if (delim == SEPARATOR) {
	  usage->nmembs = atoi (token());
	  if (usage->nmembs > 0) {
	    usage->membs = (long *) _Cif_space[lmode] ((sizeof(long) * usage->nmembs),
						       lcifd);
	    for (i = 0; i < (int) usage->nmembs; i++) {
	      usage->membs[i] = atol (token());
	    }
	  }
	}

	usage->use = ubuff;
	usage->nuses = 1;

	return (CIF_USAGE);

}


#ifndef CRAY2
static int ascii_BE_node (ent)
struct Cif_BE_node *ent;
{
	char *cp;
	int i, n;

	if ( _Cif_filetbl[lcifd].return_version == 2 ) {

	    struct Cif_BE_node_2 *v2 = (struct Cif_BE_node_2 *) ent;
	    v2->block = atoi (token());
	    v2->blocklet = atoi (token());
	    v2->is_entry = atoi (token());
	    v2->nsuccs = n = atoi (token());
	    if ( n < 0 ) {
		return( CIF_BADFORM );
	    } else if ( n == 0 ) {
		v2->succs = (int *) NULL;
	    } else {
		v2->succs = (int *) 
		    _Cif_space[ lmode ](n * sizeof( int ), lcifd);
		if ( v2->succs == NULL )
		    return( CIF_NOMEM );
		for ( i = 0; i < n; i++ ) {
		    v2->succs[ i ] = atoi (token());
		}
	    }
	    v2->nlines = n = atoi (token());
	    if ( n < 0 ) {
		return( CIF_BADFORM );
	    } else if ( n == 0 ) {
		v2->lines = (int *) NULL;
	    } else {
		if ( _Cif_filetbl[lcifd].version >= 3 ) {
		    /* user wants V2 record, but this is V3
		     * so read up fids and toss them */
		    for ( i = 0; i < n; i++ ) {
			(void) token();
		    }
		}
		v2->lines = (int *) 
		    _Cif_space[ lmode ](n * sizeof( int ), lcifd);
		if ( v2->lines == NULL )
		    return( CIF_NOMEM );
		for ( i = 0; i < n; i++ ) {
		    v2->lines[ i ] = atoi (token());
		}
	    }
	    v2->type = atoi (token());
	    v2->subtype = atoi (token());
	    v2->index = atoi (token());
	    cp = token();
	    n = strlen (cp);
	    v2->label = _Cif_space[ lmode ](n+1, lcifd);
	    if ( v2->label == NULL )
		return( CIF_NOMEM );
	    (void) strcpy( v2->label, cp );
	    for ( i = 0; i < CIF_IT_MAX; i++ ) {
		v2->icnt[ i ] = atoi (token());
	    }
	    v2->app_before = atoi (token());
	    v2->app_after = atoi (token());
	    v2->clocks = atoi (token());

	} else {		/* user wants a V3 format record */

	    ent->block = atoi (token());
	    ent->blocklet = atoi (token());
	    ent->is_entry = atoi (token());
	    ent->nsuccs = n = atoi (token());
	    if ( n < 0 ) {
		return( CIF_BADFORM );
	    } else if ( n == 0 ) {
		ent->succs = (int *) NULL;
	    } else {
		ent->succs = (int *) 
		    _Cif_space[ lmode ](n * sizeof( int ), lcifd);
		if ( ent->succs == NULL )
		    return( CIF_NOMEM );
		for ( i = 0; i < n; i++ ) {
		    ent->succs[ i ] = atoi (token());
		}
	    }
	    ent->nlines = n = atoi (token());
	    if ( n < 0 ) {
		return( CIF_BADFORM );
	    } else if ( n == 0 ) {
		ent->fid = (int *) NULL;
		ent->lines = (int *) NULL;
	    } else {
		ent->fid = (int *) 
		    _Cif_space[ lmode ](n * sizeof( int ), lcifd);
		if ( ent->fid == NULL )
		    return( CIF_NOMEM );
		ent->lines = (int *) 
		    _Cif_space[ lmode ](n * sizeof( int ), lcifd);
		if ( ent->lines == NULL )
		    return( CIF_NOMEM );
		if ( _Cif_filetbl[lcifd].version >= 3 ) {
		    for ( i = 0; i < n; i++ ) {
			ent->fid[ i ] = atoi (token());
		    }
		    for ( i = 0; i < n; i++ ) {
			ent->lines[ i ] = atoi (token());
		    }
		} else {
		    for ( i = 0; i < n; i++ ) {
			ent->fid[ i ] = 0;
			ent->lines[ i ] = atoi (token());
		    }
		}
	    }
	    ent->type = atoi (token());
	    ent->subtype = atoi (token());
	    ent->index = atoi (token());
	    cp = token();
	    n = strlen (cp);
	    ent->label = _Cif_space[ lmode ](n+1, lcifd);
	    if ( ent->label == NULL )
		return( CIF_NOMEM );
	    (void) strcpy( ent->label, cp );
	    for ( i = 0; i < CIF_IT_MAX; i++ ) {
		ent->icnt[ i ] = atoi (token());
	    }
	    ent->app_before = atoi (token());
	    ent->app_after = atoi (token());
	    ent->clocks = atoi (token());
	    if ( *ntoken != 0 )
		return( CIF_BADFORM );
	}
	return( CIF_BE_NODE );
}


static int ascii_BE_fid (ent)
struct Cif_BE_fid *ent;
{
	char *cp;
	int i, n;

	ent->block = atoi (token());
	ent->blocklet = atoi (token());

	ent->nfid = n = atoi (token());
	if ( n < 0 ) {
	    return( CIF_BADFORM );
	} else if ( n == 0 ) {
	    ent->fid = (int *) NULL;
	} else {
	    ent->fid = (int *) 
		_Cif_space[ lmode ](n * sizeof( int ), lcifd);
	    if ( ent->fid == NULL )
		return( CIF_NOMEM );
	    for ( i = 0; i < n; i++ ) {
		ent->fid[ i ] = atoi (token());
	    }
	}
	return( CIF_BE_FID );
}
#endif /* CRAY2 */


static int ascii_cc_type (spos)
struct Cif_cc_type *spos;
{
    int i, n;
    char *c;

    spos->scopeid = atoi(token());
    spos->ptype = atol(token());
    spos->size = atol(token());
    spos->typeId = atol(token());
    spos->type = atoi(token());

    switch( spos->type ) {
    case CIF_CCT_INT:
	spos->flags = atoi(token());
	spos->prec = atoi(token());
	break;
    case CIF_CCT_FLOAT:
	spos->subtype = atoi(token());
	break;
    case CIF_CCT_COMPLEX:
	spos->subtype = atoi(token());
	break;
    case CIF_CCT_CLASS:
    case CIF_CCT_STRUCT:
    case CIF_CCT_UNION:
    case CIF_CCT_ENUM:
	c = token();
	spos->nlen = i = strlen (c);
	spos->name = _Cif_space[lmode] (i+1, lcifd);
	if (spos->name == NULL)
	    return (CIF_NOMEM);
	(void) strcpy (spos->name, c);
	spos->symid = atoi(token());
	spos->nmem = i = atoi(token());
	i *= sizeof( int );
	spos->mem = (int *)_Cif_space[lmode] (i, lcifd);
	if (spos->mem == NULL)
	    return (CIF_NOMEM);
	for ( i = 0; i < (int)spos->nmem; i++ )
	    spos->mem[ i ] = atoi(token());
	break;
    case CIF_CCT_TYPEDEF:
	c = token();
	spos->nlen = i = strlen (c);
	spos->name = _Cif_space[lmode] (i+1, lcifd);
	if (spos->name == NULL)
	    return (CIF_NOMEM);
	(void) strcpy (spos->name, c);
	spos->symid = atoi(token());
	spos->btype = atoi(token());
	break;
    case CIF_CCT_QUALIFIED:
	spos->btype = atoi(token());
	spos->flags = atoi(token());
	break;
    case CIF_CCT_FUNCTION:
	spos->rtype = atoi(token());
	spos->flags = atoi(token());
	spos->nmem = i = atoi(token());
	i *= sizeof( int );
	spos->mem = (int *)_Cif_space[lmode] (i, lcifd);
	if (spos->mem == NULL)
	    return (CIF_NOMEM);
	for ( i = 0; i < (int)spos->nmem; i++ )
	    spos->mem[ i ] = atoi(token());
	break;
    case CIF_CCT_POINTER:
	spos->btype = atoi(token());
	break;
    case CIF_CCT_ARRAY:
	break;
    case CIF_CCT_PTRMEM:
	break;
    case CIF_CCT_TEMPAR:
	break;
    }

    if ( spos->name == NULL ) {
	spos->nlen = i = 0;
	spos->name = _Cif_space[lmode] (i+1, lcifd);
	if (spos->name == NULL)
	    return (CIF_NOMEM);
	(void) strcpy (spos->name, "");
    }

    return( CIF_CC_TYPE );
}


static int ascii_cc_entry (spos)
struct Cif_cc_entry *spos;
{
    register int i, n;
    register char *c;

    c = token();
    spos->nlen = i = strlen (c);
    spos->name = _Cif_space[lmode] (i+1, lcifd);
    if (spos->name == NULL)
	return (CIF_NOMEM);
    (void) strcpy (spos->name, c);

    c = token();
    spos->elen = i = strlen (c);
    spos->ename = _Cif_space[lmode] (i+1, lcifd);
    if (spos->ename == NULL)
	return (CIF_NOMEM);
    (void) strcpy (spos->ename, c);

    spos->symid = atoi(token());
    spos->linkage = atol(token());
    spos->typeId = atol(token());
    spos->attr = atoi(token());
    spos->scopeid = atoi(token());
    spos->ptype = atoi(token());
    spos->sfid = atoi(token());
    spos->sline = atoi(token());
    spos->scol = atoi(token());
    spos->efid = atoi(token());
    spos->eline = atoi(token());
    spos->ecol = atoi(token());
    spos->fsymid = atoi(token());

    spos->nparam = i = atoi(token());
    i *= sizeof( int );
    spos->param = (int *)_Cif_space[lmode] (i, lcifd);
    if (spos->param == NULL)
	return (CIF_NOMEM);
    for ( i = 0; i < (int)spos->nparam; i++ )
	spos->param[ i ] = atoi(token());

    return( CIF_CC_ENTRY );
}


static int ascii_cc_obj (spos)
struct Cif_cc_obj *spos;
{
    register int i;
    register char *c;

    c = token();
    spos->nlen = i = strlen (c);
    spos->name = _Cif_space[lmode] (i+1, lcifd);
    if (spos->name == NULL)
	return (CIF_NOMEM);
    (void) strcpy (spos->name, c);

    spos->symid = atoi(token());
    spos->typeId = atol(token());
    spos->symcl = atol(token());
    spos->linkage = atol(token());
    spos->storage = atol(token());
    spos->scopeid = atol(token());
    spos->offset = atol(token());
    spos->ptype = atoi(token());
    return( CIF_CC_OBJ );
    }


static int ascii_cc_subtype (spos)
struct Cif_cc_subtype *spos;
{
    spos->symid = atoi(token());
    spos->symkind = atoi(token());
    spos->subkind = atoi(token());
    spos->flags = atol(token());
    spos->ptype = atoi(token());
    return( CIF_CC_SUBTYPE );
}


static int ascii_cc_enum (spos)
struct Cif_cc_enum *spos;
{
    register int i;
    register char *c;

    spos->symid = atoi(token());
    c = token();
    spos->nlen = i = strlen (c);
    spos->name = _Cif_space[lmode] (i+1, lcifd);
    if (spos->name == NULL)
	return (CIF_NOMEM);
    (void) strcpy (spos->name, c);

    spos->typeId = atol(token());
    c = token();
    spos->vlen = i = strlen (c);
    spos->value = _Cif_space[lmode] (i+1, lcifd);
    if (spos->value == NULL)
	return (CIF_NOMEM);
    (void) strcpy (spos->value, c);
    return( CIF_CC_ENUM );
}


static int ascii_cc_expr (spos)
struct Cif_cc_expr *spos;
{
    spos->exprid = atoi(token());
    spos->type = atol(token());
    spos->fid = atol(token());
    spos->line = atol(token());
    spos->col = atol(token());
    spos->noper = atol(token());
    /* need to get operands */
    return( CIF_CC_EXPR );
}


static int ascii_src_pos (spos)
struct Cif_src_pos *spos;
{
  spos->kind = atoi(token());

  spos->srcid = atol(token());
  spos->psrcid = atol(token());

  spos->sline = atol(token());
  spos->scol = atoi(token());

  if (spos->kind == CIF_SRC_KIND_MAIN ||
      spos->kind == CIF_SRC_KIND_INCLUDE ||
      spos->kind == CIF_SRC_KIND_INLINE ||
      spos->kind == CIF_SRC_KIND_TAIL)
      spos->fid = atol(token());
  else { /* macro */
    spos->eline = atol(token());
    spos->ecol = atoi(token());
    spos->symid = atol(token());
  }

  return( CIF_SRC_POS );
}


static int ascii_orig_cmd (ocmd)
struct Cif_orig_cmd *ocmd;
{
  register int i;
  register char *c;

  c = token();
  ocmd->nlen = i = strlen (c);
  ocmd->name = _Cif_space[lmode] (i+1, lcifd);
  if (ocmd->name == NULL)
      return (CIF_NOMEM);
  (void) strcpy (ocmd->name, c);

  return( CIF_ORIG_CMD );
}


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


static char USMID[] = "@(#) libcif/cif_conv.c	30.22	12/08/96 14:42:46";


/* --------------------------------------------------------------------------
 * "cifconv" Compiler Information File reformatter.
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
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "unitrecord.h"
#include "cif_int.h"

#include <sys/types.h>
#include <sys/stat.h>

enum Boolean {False, True};


static int sortfile (struct Cif_generic *);
static int copy_units (void);
static int write_header (void);
static int get_srcfid (void);
static int write_unit (int, enum Boolean, enum Boolean);
int cif_VerifyCanWrite( char *file );
static int lang;

/*
 * global return code value, in case cifconv needs to exit so that the
 * correct error flag can be passed back to the caller
 */
static int cifconv_return_code = 0;

static void free_copied_blocks();

#define CIFCONV_INVALID_RECORDS 2

#undef Cif_Cifconv  /* ensure that we don't try to map this to Cif_Cifconv_Vx */

#define MEM_ERROR { \
	(void) fprintf (stderr, "libcif: %s\n", Cif_Errstring(CIF_NOMEM)); \
	exit (1); \
}

#define OUT_ERROR(STR,FD,STATUS) { \
	(void) fprintf (stderr, "libcif: error %s file %s - %s\n", \
		STR, (FD==outfd ? global_outfile : tfile), Cif_Errstring(STATUS)); \
	exit (STATUS); \
}

/* --- file names and file descriptors --- */
static int infd;					/* input cif descriptor */
static int outfd;					/* output cif descriptor */
static int tmpfd = -1;					/* temporary file cif descriptor */
static char *tfile = NULL;		/* pointer to temporary file name */
static long fdirpos;				/* file positon of file directory */
static enum Boolean canpos = False;		/* generate positioning info flag */
static int *global_nuses = (int *) NULL;	/* count of use's per usage used in merge_usages */
static int global_nuses_allocated = 0;

#ifdef DEBUG
	static FILE *dfd;				/* file descriptor for debug file output */
	static FILE *sfd;				/* file descriptor for standard output */
	void dump_patbl ();
	int valid_patbl ();
#endif

/* --- id remapping data --- */
#define ID_BUMP 500		       	/* amount to increment tbl size */
static enum Boolean Remap_id;		/* set if ids need to be remapped */
struct Id_tbl {
	long *tbl;		       	/* ptr to array of original id values */
	int max;		    	/* current maximum size of tbl */
	int cur;		       	/* index of next available slot in tbl */
};
static struct Id_tbl sid;		/* symbol id remapping data */
#ifdef REMAP_FID
static struct Id_tbl fid;		/* file id remapping data */
#endif

static enum Boolean first_time = True;

#ifdef REMAP_FID
/* The mapped srcfid to go into the cif_cifhdr record */
static int srcfid = 0;
#endif

/* --- pointer array management structure --- */
static struct {
	struct Cif_generic **aptr;	/* ptr to array of ptrs to record structs */
	int psize;		        /* current allocated size of ptr array */
	int next;		       	/* index of next available slot in ptr array */
} patbl[CIF_MAXRECORD];

static char *global_outfile;  /*
			       * allows output filename to be printed from
			       * anywhere when an error condition occurs
			       */
/* global_error_report is set to true if the cif_cifconv
 * user wants errors reported to stderr. Error such as
 * a usage record not having a matching definition record.
 * If false, these errors are ignored.
 */
static enum Boolean global_error_report = False;

/* --- pointer array increment values --- */
static const int pabump[CIF_MAXRECORD] = {
	0,	       	/* 00 = unused */
	100,		/* 01 = CIF_CALLSITE */
	1,	       	/* 02 = CIF_CIFHDR */
	100,		/* 03 = CIF_COMBLK */
	200,		/* 04 = CIF_CONST */
	100,		/* 05 = CIF_CDIR */
	100,		/* 06 = CIF_ENTRY */
	100,		/* 07 = CIF_FILE */
	100,		/* 08 = CIF_LOOP */
	100,		/* 09 = CIF_INCLUDE */
	200,		/* 10 = CIF_LABEL */
	100,		/* 11 = CIF_MESSAGE */
	100,		/* 12 = CIF_NAMELIST */
	400,		/* 13 = CIF_OBJECT */
	1,	       	/* 14 = CIF_SRCFILE */
	1,	       	/* 15 = CIF_SUMMARY */
	100,		/* 16 = CIF_CDIR_DOSHARED */
	1,	       	/* 17 = CIF_UNIT */
	1,	       	/* 18 = CIF_ENDUNIT */
	1000,		/* 19 = CIF_USAGE */
	100,		/* 20 = CIF_ND_MSG */
	1,	       	/* 21 = CIF_EDOPTS */
	1,	       	/* 22 = CIF_MACH_CHAR */
	1,	       	/* 23 = CIF_MISC_OPTS */
	1,	       	/* 24 = CIF_OPT_OPTS */
	1000,		/* 25 = CIF_STMT_TYPE */
	100,		/* 26 = CIF_GEOMETRY */
	100,		/* 27 = CIF_CONTINUATION */
	100,		/* 28 = CIF_F90_CALLSITE */
	100,		/* 29 = CIF_F90_COMBLK */
	200,		/* 30 = CIF_F90_CONST */
	100,		/* 31 = CIF_F90_ENTRY */
	100,		/* 32 = CIF_F90_LOOP */
	100,		/* 33 = CIF_F90_DERIVED_TYPE */
	200,		/* 34 = CIF_F90_LABEL */
	100,		/* 35 = CIF_F90_NAMELIST */
	400,		/* 36 = CIF_F90_OBJECT */
	1,		/* 37 = CIF_F90_MISC_OPTS */
	1,		/* 38 = CIF_F90_OPT_OPTS */
	100,		/* 39 = CIF_F90_BEGIN_SCOPE */
	100,		/* 40 = CIF_F90_END_SCOPE */
	100,		/* 41 = CIF_F90_SCOPE_INFO */
	100,		/* 42 = CIF_F90_USE_MODULE */
	100,		/* 43 = CIF_F90_RENAME */
	100,		/* 44 = CIF_F90_INT_BLOCK */
	100,		/* 45 = CIF_F90_VECTORIZATION */
	100,		/* 46 = CIF_BE_NODE */
	100,		/* 47 = CIF_TRANSFORM */
	1,	       	/* 48 = CIF_FILEDIR */
	1,	       	/* 49 = CIF_UNITDIR */
	100,		/* 50 = CIF_BE_FID */
	400,		/* 51= CIF_C_TAG */
	1,	       	/* 52= CIF_C_OPTS */
	100,		/* 53= CIF_C_MESSAGE */
	200,		/* 54= CIF_C_CONST */
	100,		/* 55= CIF_C_ENTRY */
	400,		/* 56= CIF_C_OBJECT */
	200,		/* 57= CIF_C_LINT_DIRECTIVE */
	200,		/* 58= CIF_C_MACRO_DEF */
	200,		/* 59= CIF_C_MACRO_UNDEF */
	400,		/* 60= CIF_C_MACRO_USAGE */
	100,		/* 61= CIF_C_ENTRY_END */
	0,0,0,0,0,0,0,0, /* 62-69 */
	1,		/* 70= CIF_ORIG_CMD */
	0,0,0,0,0,0,0,0,0, /* 71-79 */
	100,		/* 80= CIF_CC_TYPE */
	100,		/* 81= CIF_CC_ENTRY */
	100,		/* 82= CIF_CC_OBJ */
	100,		/* 83= CIF_CC_SUBTYPE */
	100,		/* 84= CIF_CC_ENUM */
	100,		/* 85= CIF_CC_EXPR */
	100		/* 86= CIF_SRC_POS */


};

/* --- function pointer array - qsort structure comparison routines --- */
static int comp_callsite (struct Cif_callsite **, struct Cif_callsite **);
static int comp_comblk (struct Cif_comblk **, struct Cif_comblk **);
static int comp_const (struct Cif_const **, struct Cif_const **);
static int comp_entry (struct Cif_entry **, struct Cif_entry **);
static int comp_file (struct Cif_file **, struct Cif_file **);
static int comp_label (struct Cif_label **, struct Cif_label **);
static int comp_loop (struct Cif_loop **, struct Cif_loop **);
static int comp_message (struct Cif_message **, struct Cif_message **);
static int comp_namelist (struct Cif_namelist **, struct Cif_namelist **);
static int comp_object (struct Cif_object **, struct Cif_object **);
static int comp_stmt_type (struct Cif_stmt_type **,struct Cif_stmt_type **);
static int comp_usage (struct Cif_usage **, struct Cif_usage **);
static int comp_c_tag (struct Cif_c_tag **, struct Cif_c_tag **);
static int comp_c_message (struct Cif_c_message **, struct Cif_c_message **);
static int comp_c_const (struct Cif_c_const **, struct Cif_c_const **);
static int comp_c_entry (struct Cif_c_entry **, struct Cif_c_entry **);
static int comp_c_object (struct Cif_c_object **, struct Cif_c_object **);
static int comp_c_lint_directive (struct Cif_c_lint_directive **, struct Cif_c_lint_directive **);
static int comp_c_macro_def (struct Cif_c_macro_def **, struct Cif_c_macro_def **);
static int comp_c_macro_undef (struct Cif_c_macro_undef **, struct Cif_c_macro_undef **);
static int comp_c_macro_usage (struct Cif_c_macro_usage **, struct Cif_c_macro_usage **);
static int comp_c_entry_end (struct Cif_c_entry_end **, struct Cif_c_entry_end **);

static int comp_cdir (struct Cif_cdir **, struct Cif_cdir **);
static int comp_cdir_doshared (struct Cif_cdir_doshared **, struct Cif_cdir_doshared **);
static int comp_geometry (struct Cif_geometry **, struct Cif_geometry **);
static int comp_continuation (struct Cif_continuation **, struct Cif_continuation **);
static int comp_transform (struct Cif_transform **, struct Cif_transform **);

#ifndef CRAY2
static int comp_f90_callsite (struct Cif_f90_callsite **, struct Cif_f90_callsite **);
static int comp_f90_comblk (struct Cif_f90_comblk **, struct Cif_f90_comblk **);
static int comp_f90_const (struct Cif_f90_const **, struct Cif_f90_const **);
static int comp_f90_entry (struct Cif_f90_entry **, struct Cif_f90_entry **);
static int comp_f90_loop (struct Cif_f90_loop **, struct Cif_f90_loop **);
static int comp_f90_derived_type (struct Cif_f90_derived_type **, struct Cif_f90_derived_type **);
static int comp_f90_label (struct Cif_f90_label **, struct Cif_f90_label **);
static int comp_f90_namelist (struct Cif_f90_namelist **, struct Cif_f90_namelist **);
static int comp_f90_object (struct Cif_f90_object **, struct Cif_f90_object **);
static int comp_f90_begin_scope (struct Cif_f90_begin_scope **, struct Cif_f90_begin_scope **);
static int comp_f90_end_scope (struct Cif_f90_end_scope **, struct Cif_f90_end_scope **);
static int comp_f90_scope_info (struct Cif_f90_scope_info **, struct Cif_f90_scope_info **);
static int comp_f90_use_module (struct Cif_f90_use_module **, struct Cif_f90_use_module **);
static int comp_f90_rename (struct Cif_f90_rename **, struct Cif_f90_rename **);
static int comp_f90_int_block (struct Cif_f90_int_block **, struct Cif_f90_int_block **);
static int comp_f90_vectorization (struct Cif_f90_vectorization **, struct Cif_f90_vectorization **);

#endif /* CRAY2 */

static int comp_cc_type (struct Cif_cc_type **, struct Cif_cc_type **);
static int comp_cc_entry (struct Cif_cc_entry **, struct Cif_cc_entry **);
static int comp_cc_obj (struct Cif_cc_obj **, struct Cif_cc_obj **);
static int comp_cc_subtype (struct Cif_cc_subtype **, struct Cif_cc_subtype **);
static int comp_cc_enum (struct Cif_cc_enum **, struct Cif_cc_enum **);
static int comp_cc_expr (struct Cif_cc_expr **, struct Cif_cc_expr **);

static int comp_src_pos (struct Cif_src_pos **, struct Cif_src_pos **);
static int comp_orig_cmd (struct Cif_orig_cmd **, struct Cif_orig_cmd **);

static int (*qcompare[CIF_MAXRECORD]) () = {
	0,			       	/* 00 */
	comp_callsite,			/* 01 = CIF_CALLSITE */
	0,			       	/* 02 */
	comp_comblk,			/* 03 = CIF_COMBLK */
	comp_const,		       	/* 04 = CIF_CONST */
	comp_cdir,			/* 05 = CIF_CDIR */
	comp_entry,		       	/* 06 = CIF_ENTRY */
	comp_file,		       	/* 07 = CIF_FILE */
	comp_loop,		       	/* 08 = CIF_LOOP */
	0,			       	/* 09 */
	comp_label,		       	/* 10 = CIF_LABEL */
	comp_message,			/* 11 = CIF_MESSAGE */
	comp_namelist,			/* 12 = CIF_NAMELIST */
	comp_object,			/* 13 = CIF_OBJECT */
	0,0,				/* 14, 15 */
	comp_cdir_doshared,    		/* 16 = CIF_CDIR_DOSHARED */
	0,0,		       		/* 17, 18 */
	comp_usage,		       	/* 19 = CIF_USAGE */
	0,0,0,0,0,		       	/* 20-24 */
	comp_stmt_type,			/* 25 = CIF_STMT_TYPE */
	comp_geometry,			/* 26 = CIF_GEOMETRY */
	comp_continuation,		/* 27 = CIF_CONTINUATION */
#ifndef CRAY2
	comp_f90_callsite,		/* 28 = CIF_F90_CALLSITE */
	comp_f90_comblk,		/* 29 = CIF_F90_COMBLK */
	comp_f90_const,			/* 30 = CIF_F90_CONST */
	comp_f90_entry,			/* 31 = CIF_F90_ENTRY */
	comp_f90_loop,			/* 32 = CIF_F90_LOOP */
	comp_f90_derived_type,		/* 33 = CIF_F90_DERIVED_TYPE */
	comp_f90_label,			/* 34 = CIF_F90_LABEL */
	comp_f90_namelist,		/* 35 = CIF_F90_NAMELIST */
	comp_f90_object,		/* 36 = CIF_F90_OBJECT */
#else
	0,0,0,0,0,0,0,0,0,
#endif /* CRAY2 */
	0,				/* 37 = CIF_F90_MISC_OPTS */
	0,				/* 38 = CIF_F90_OPT_OPTS */
#ifndef CRAY2
	comp_f90_begin_scope,		/* 39 = CIF_F90_BEGIN_SCOPE */
	comp_f90_end_scope,		/* 40 = CIF_F90_END_SCOPE */
	comp_f90_scope_info,		/* 41 = CIF_F90_SCOPE_INFO */
	comp_f90_use_module,		/* 42 = CIF_F90_USE_MODULE */
	comp_f90_rename,		/* 43 = CIF_F90_RENAME */
	comp_f90_int_block,		/* 44 = CIF_F90_INT_BLOCK */
	comp_f90_vectorization,		/* 45 = CIF_F90_VECTORIZATION */
#else
	0,0,0,0,0,0,0,
#endif /* CRAY2 */
	0,				/* 46 = CIF_BE_NODE */
	comp_transform,			/* 47 = CIF_TRANSFORM */
	0, 0,				/* 48, 49 */
	0,				/* 50 = CIF_BE_FID */
	comp_c_tag,		       	/* 51= CIF_C_TAG */
	0, 			       	/* 52= CIF_C_OPTS */
	comp_c_message,			/* 53= CIF_C_MESSAGE */
	comp_c_const,			/* 54= CIF_C_CONST */
	comp_c_entry,			/* 55= CIF_C_ENTRY */
	comp_c_object,			/* 56= CIF_C_OBJECT */
	comp_c_lint_directive, 		/* 57= CIF_C_LINT_DIRECTIVE */
	comp_c_macro_def,      		/* 58= CIF_C_MACRO_DEF */
	comp_c_macro_undef,	       	/* 59= CIF_C_MACRO_UNDEF */
	comp_c_macro_usage,    		/* 60= CIF_C_MACRO_USAGE*/
	comp_c_entry_end,	       	/* 61= CIF_C_ENTRY_END */
	0,0,0,0,0,0,0,0,		/* 62-69 */
	comp_orig_cmd,			/* 70= CIF_ORIG_CMD */
	0,0,0,0,0,0,0,0,0,		/* 71-79 */
	comp_cc_type,			/* 80= CIF_CC_TYPE */
	comp_cc_entry,			/* 81= CIF_CC_ENTRY */
	comp_cc_obj,			/* 82= CIF_CC_OBJ */
	comp_cc_subtype,		/* 83= CIF_CC_SUBTYPE */
	comp_cc_enum,			/* 84= CIF_CC_ENUM */
	comp_cc_expr,			/* 85= CIF_CC_EXPR */
	comp_src_pos			/* 86= CIF_SRC_POS */
};

/* --- forward references --- */
static void addstruct (struct Cif_generic *);	
static void addunit (struct Cif_generic *);
static void add_id (struct Id_tbl *, long);
static int comp_ids (long *, long *);
static long get_max_fid (void);
static long get_max_sid (void);
static void init_id (struct Id_tbl *);
static void makeudir (void);
static void remap_files (void);
static void remap_symbols (void);


/* returns True if file_2 is older than file_1 */

static int later_date
#ifdef __STDC__
	  (char *file_1, char *file_2)
#else
	  (file_1, file_2)
char *file_1, *file_2;
#endif
{
  struct stat buf_1, buf_2;

  (void) stat(file_1, &buf_1);
  (void) stat(file_2, &buf_2);

  return(buf_2.st_mtime >= buf_1.st_mtime);  
}



/* Function: cif_next_entry */

static
  int cif_next_entry
#ifdef __STDC__
	( int cifd, long *cifpos, struct Cif_generic **cif_record )
#else
	( cifd, cifpos, cif_record )
int cifd;
long *cifpos;
struct Cif_generic **cif_record;
#endif
{
  int rtype;

  if ((rtype = Cif_Setpos (cifd, *cifpos)) < 0) {
    (void) fprintf(stderr, "libcif: set pos returns %d %s for cifd %d %ld\n",
	    rtype,
	    Cif_Errstring(rtype),
	    cifd,
	    *cifpos);
  }

  if ((rtype = Cif_Getrecord (cifd, cif_record)) < 0) {
    (void) fprintf (stderr, "libcif: Unknown record type at %ld for %d: (%d) %s\n",
	    *cifpos,
	    cifd,
	    rtype,
	    Cif_Errstring(rtype));
  }

  *cifpos = Cif_Getpos(cifd);
  return(rtype);
}



/*
 * cifconv_type returns true if this file is in cifconv format already
 * false otherwise.
 */


static int cifconv_type
#ifdef __STDC__
	(char *cif_name)
#else
	(cif_name)
char *cif_name;
#endif
{
  int cifd;
  long filepos = CIF_FIRST_RECORD;
  int return_code;
  struct Cif_generic *cif_record;

  cifd = Cif_Open(cif_name, "r", NULL, CIF_VERSION);

  if (cifd >= 0 &&
      cif_next_entry(cifd, &filepos, &cif_record) == CIF_CIFHDR) {

    return_code = (CIFHDR(cif_record)->form == CIF_FORM_SORTED);
    return_code &= (CIFHDR(cif_record)->bintype == CIF_FORM_CIFCONV);

    lang = CIFHDR(cif_record)->lang;  /* note the source language */

    (void) Cif_Close (cifd, CIF_MEM_FREE);
    return(return_code);
  }
  else {
    return(0);
  }
}


/* --------------------------------------------------------------------------
 * main opens the input CIF file.  If binary format, it just copies it.  If
 * ASCII format, it organizes the records into order, adds directory
 * information, and writes out a binary format file.
 *
 * Record structures are managed by the an array of pointers to structure
 * for each type.  As each structure is encountered, a pointer to the
 * structure is added to the appropriate pointer array.  The pointer arrays
 * are dynamically expanded as needed.  Before writing out the structures,
 * the pointer arrays are sorted in the correct order for that type.
 * --------------------------------------------------------------------------
 */

static char *Cif_Make_Cifconv
#ifdef __STDC__
	(char *infile, char *outfile, int *rtypes)
#else
	(infile, outfile, rtypes)
char *infile;
char *outfile;
int *rtypes;
#endif
{

	int arg;       			/* argument counter */
	struct Cif_generic *cifp;	/* CIF structure pointer */
	int ret;

#ifdef DEBUG
	dfd = fopen ("debug", "w");
	sfd = stdout;
#endif

	/*
	 * Note the filename to allow it to be printed
	 * in error messages
	 */
	global_outfile = infile;

	/* Open the input and output files */

	if ((infd = Cif_Open(infile, "r", rtypes, CIF_VERSION)) < 0) {
	  /* store the return code for returning from Cif_Cifconv */
	  cifconv_return_code = infd;
	  return ((char *) NULL);
	}


	if ((ret = Cif_Memmode(infd,CIF_MEM_MANAGED)) < 0)  {
	    (void) fprintf(stderr,"\nlibcif: Unrecoverable internal error - Cif_Memmode\
(%d,CIF_MEM_MANAGED=%d) returns %d:\n\t Possible cause is memory \
expansion request denial by OS\n\t Note -- Internal CIF-Library \
Message:\n\n\t\t \"%s\"\n",
		infd,CIF_MEM_MANAGED,ret,Cif_Errstring(ret));
	    exit(ret);
       }

	if ((outfd = Cif_Open(outfile, "w", NULL, CIF_VERSION)) < 0) {
		(void) Cif_Close(infd, CIF_MEM_FREE);
                cifconv_return_code = outfd; 
		return ((char *) NULL);
	}

	/*
	 * Get the header record.  If the form is sorted, copy the file.
	 * Otherwise sort it.  
	 */

	if ((arg = Cif_Getrecord (infd, &cifp)) == CIF_CIFHDR) {

		if (CIFHDR(cifp)->form == CIF_FORM_SORTED) {
			(void) free(outfile);
			return(strdup(infile));
		}
		else {
			if (CIFHDR(cifp)->cont_id == CIF_ID_NONCONTIG) {

				/* Make sure the old memory is freed off */

    				if (sid.tbl != (long *) NULL)
					(void) free(sid.tbl);

				if ((sid.tbl = (long *) malloc (sizeof(long)*ID_BUMP)) == NULL)
					MEM_ERROR;
				sid.max = ID_BUMP;

#ifdef REMAP_FID
    				if (fid.tbl != (long *) NULL)
					(void) free(fid.tbl);

				if ((fid.tbl = (long *) malloc (sizeof(long)*ID_BUMP)) == NULL)
					MEM_ERROR;
				fid.max = ID_BUMP;
#endif
				Remap_id = True;
			}
			else
				Remap_id = False;
			/*
			 * sortfile will return -1 iff no units are found,
			 * in which case, just copy the infile to the outfile
			 * as there's little extra to be done
			 */
			if (sortfile (cifp) < 0) {
				(void) Cif_Close(outfd, CIF_MEM_FREE);
				unlink(outfile);
				return(strdup(infile));
			}
		}
	}
	else {
		if (arg > 0)
			return((char *) NULL);
	}

	/* Free up any space allocated via Cif_Duplicate calls */
	free_copied_blocks();

	/* Close the files and quit */

	(void) Cif_Close (infd, CIF_MEM_KEEP);
	(void) Cif_Close (outfd, CIF_MEM_KEEP);

	/* Close the tmp file as necessary */

	if (tmpfd != -1) {
       		(void) Cif_Close (tmpfd, CIF_MEM_KEEP);
		tmpfd = -1;
	}

#ifdef DEBUG
	(void) fclose (dfd);
#endif
	return(outfile);
}


/*
 * An unpublished routine, used by xbrowse to set the location of
 * .TT files. xbrowse does all validation of the directory, so no
 * need to do any checking here
 */

static char *cif_tt_dir = (char *) NULL;

void Cif_ConvDir
#ifdef __STDC__
	(char *dir)
#else
	(dir)
char *dir;
#endif
{
    cif_tt_dir = dir;
}


/* Join two strings together */
static char *concat
#ifdef _STDC__
	( char *str1, char *str2 )
#else
	( str1, str2 )
char *str1, *str2;
#endif
{
  char *buf;

  buf = (char *) malloc (sizeof(char) * (strlen(str1) + strlen(str2) + 1));
  sprintf(buf, "%s%s",str1, str2);
  return(buf);
}

/* Verify that the directory passed exists and is writable */
int assertCanWriteDir
#ifdef __STDC__
	(char *dir)
#else
	(dir)
char *dir;
#endif
{
  struct stat buf;
  int mode;
  char * test_file = concat(dir,"/write_test");
  FILE * fd;

  mode = stat(dir, &buf);

  if (mode == 0 &&
      S_ISDIR(buf.st_mode)) {
    if (NULL == (fd = fopen(test_file,"w"))) {
      return(0);  /* directory is not writable */
    };
    fclose(fd);
    unlink(test_file);
  }
  else {
   return(0);
  }

  free(test_file);

  return(1); /* directory exists and is writable */
}      

static char *cif_convert_to_cifconv
#ifdef __STDC__
	(char *filename, int keep, int *tmp_cif, int *rtypes)
#else
	(filename, keep, tmp_cif, rtypes)
char *filename;
int keep;
int *tmp_cif;
int *rtypes;
#endif
{
    char *value;
    char *cifdir = (char *) NULL;
    char *cifdir_file = (char *) NULL;
    char *tt_file = (char *) NULL;
    char *outfile = (char *) NULL;
    char *create_cif_file = (char *) NULL;
    char *tmpdir = (char *) NULL;

    /* Assume that a tmp file will not be created, until proved otherwise */

    *tmp_cif = 0;

    /* 1. check if the file is already a cifconv file */

    if (cifconv_type( filename ) == 1)
	return(strdup(filename));


    /* See if cif_convdir has been called to set the location of .TT files */

    if (cif_tt_dir != (char *) NULL) {

	cifdir = cif_tt_dir;

	tt_file = (char *) malloc(sizeof(char) *
				  (strlen(cif_tt_dir) + 
				   strlen(cif_basename(filename)) +
				   3));

	(void) sprintf(tt_file, "%s/%sT", cif_tt_dir,
		       cif_basename(filename));

	if (!access(tt_file, R_OK)) {

	    if (later_date(filename, tt_file) &&
		cifconv_type(tt_file) == 1) {
		return(tt_file);
	    }
	}
    }



    /* 2. see if CIFDIR is set */

    value = getenv("CIFDIR");
    if (value != (char *) NULL) {
	cifdir = value;

	cifdir_file = (char *) malloc(sizeof(char) *
				      (strlen(cifdir) + 
				       strlen(cif_basename(filename)) +
				       3));

	(void) sprintf(cifdir_file, "%s/%sT", cifdir, cif_basename(filename));

	if (!access(cifdir_file, R_OK)) {

	    if (later_date(filename, cifdir_file) &&
		cifconv_type(cifdir_file) == 1) {

		if (tt_file != (char *) NULL)
		    free(tt_file);

		return(cifdir_file);
	    }
	}
    }

    /* 3. See if the cifconv cif exists next to the original cif */

    outfile = (char *) malloc(sizeof(char) *
			      (strlen(filename) + 2));
    (void) sprintf(outfile, "%sT", filename);

    if (!access(outfile, R_OK)) {

	if (later_date(filename, outfile) &&
	    cifconv_type(outfile) == 1) {

	    if (tt_file != (char *) NULL)
		free(tt_file);

	    if (cifdir_file != (char *) NULL)
		free(cifdir_file);

	    return(outfile);
	}
    }



    /* Cifconv does not exist already, so we have to create it */

    /* Only want to create the cif in none-tmp space if keep == True */
    if (keep == 1) {

	/*
	 * 4. See if we need to write into the tt_directory, only used
	 * by xbrowse through an unpublished function cif_convdir(dir).
	 */

	if (tt_file != (char *) NULL &&
	    cif_VerifyCanWrite(tt_file)) {

	    create_cif_file = strdup(tt_file);
	}
	else

	/* 4. See if we can write to CIFDIR */

	if (cifdir_file != (char *) NULL &&
	    cif_VerifyCanWrite(cifdir_file)) {

	    create_cif_file = strdup(cifdir_file);
	}
	else

	    /* 5. See if we can write next to the original file */

	    if (outfile != (char *) NULL &&
		cif_VerifyCanWrite(outfile)) {

		create_cif_file = strdup(outfile);
	    }
    }

    if (create_cif_file == (char *) NULL) {

	/* 6. put the cif into /tmp, see of TMPDIR is available */

	value = getenv("TMPDIR");
	if (value != (char *) NULL && assertCanWriteDir(value) == 1) {
	    tmpdir = value;
	}
	else {

	    create_cif_file = (char *) malloc(sizeof(char) *
					      (strlen("/tmp/") + 
					       strlen(cif_basename(filename)) +
					       7));

	    /* 7. else create a tmp, tmp directory */
	    (void) sprintf(create_cif_file, "/tmp/%sXXXXXX", cif_basename(filename));
	    (void) mktemp(create_cif_file);
	}

	if (create_cif_file == (char *) NULL) {
	    create_cif_file = (char *) malloc(sizeof(char) *
					      (strlen(tmpdir) + 
					       strlen(cif_basename(filename)) +
					       3));

	    (void) sprintf(create_cif_file, "%s/%sT", tmpdir, cif_basename(filename));
	}

	/*
	 * indicate that a tmp cif file is about to be created; it will
	 * be removed on cif_close
	 */

	*tmp_cif = 1;

    }

    create_cif_file = Cif_Make_Cifconv(filename, create_cif_file, rtypes);

    /* free up allocated strings */

    if (cifdir_file != (char *) NULL) (void) free(cifdir_file);
    if (tt_file != (char *) NULL) (void) free(tt_file);
    if (outfile != (char *) NULL) (void) free(outfile);

    return(create_cif_file);
}


int Cif_Cifconv
#ifdef __STDC__
(char *filename, char *optype, int *rtypes, int version, int keep)
#else
(filename, optype, rtypes, version, keep)
char *filename;			/* file name */
char *optype;		        /* open type */
int *rtypes;		       	/* ptr to array of selected record types */
int version;		       	/* CIF version expected by tools */
int keep;		       	/* keep the file on exit ? */
#endif
{

    char *cif_name;
    int tmp_cif;  /*
		   * set by cif_convert_to_cifconv if a tmp file has been
		   * created to hold the cifconv cif; the cif should be deleted
		   * by cif_close
		   */
    int ret;

    if (_cif_version == 0)
	_cif_version = 1;

    /*
     * If keep & 0x100 this is a hidden flag to say that cifconv should
     * report on any inconsistencies it finds in the cif to stderr
     */

    if (keep & 0x100) {
	global_error_report = True;
    }

    /*
     * If this is the first time through, we must initialize some
     * data structures
     */

    if (first_time == True) {
	sid.tbl = (long *) NULL;
	sid.max = 0;
	sid.cur = 0;
#ifdef REMAP_FID
	fid.tbl = (long *) NULL;
	fid.max = 0;
	fid.cur = 0;
#endif
	first_time = False;
    }

    /* convert filename to cif_cifconv name */
    cif_name = cif_convert_to_cifconv(filename, keep & 0xff, &tmp_cif, rtypes);

    /* an empty filename means that the cif is invalid or not there */
    if (cif_name == (char *) NULL) {
	return(cifconv_return_code);
    }

    /* open the file and return */

    if (version == 2) {
        ret = Cif_Open_V2(cif_name, optype, rtypes, version);
    }
    else {
        ret = Cif_Open_V3_1(cif_name, optype, rtypes, version, 
			    CIF_SUB_VERSION_3);
    }

    /*
     * If a valid open, set the tmp_cif flag according to indicate if a
     * temporary file has been created that should be removed on cif_close
     */

    if (ret >= 0) {
	_Cif_filetbl[ret].tmp_cif = tmp_cif;
    }

    /* free up the converted filename, cif_open will have take a copy */

    free(cif_name);

    /* Return whatever cif_open returned */

    return(ret);
}


/*
 * As above + added a check to see of the cif.h in use matches what this
 * library was compiled with...looks at CIF_SUB_VERSION_2 which must match
 * the sub_version passed. See cif_open macros in cif.h for more details.
 */

int Cif_Cifconv_V2_1
#ifdef __STDC__
(char *filename, char *optype, int *rtypes, int version, int keep, int sub_version)
#else
(filename, optype, rtypes, version, keep, sub_version)
char *filename;			/* file name */
char *optype;		        /* open type */
int *rtypes;		       	/* ptr to array of selected record types */
int version;		       	/* CIF version expected by tools */
int keep;		       	/* keep the file on exit ? */
int sub_version;		/* version number of the cif.h */
#endif
{

    _cif_version = 2;
    if (sub_version != CIF_SUB_VERSION_2)
	return(CIF_SUBVER);

    return(Cif_Cifconv(filename, optype, rtypes,
		       version, keep));
}

/*
 * As above for version 3
 */

int Cif_Cifconv_V3_1
#ifdef __STDC__
(char *filename, char *optype, int *rtypes, int version, int keep, int sub_version)
#else
(filename, optype, rtypes, version, keep, sub_version)
char *filename;			/* file name */
char *optype;		        /* open type */
int *rtypes;		       	/* ptr to array of selected record types */
int version;		       	/* CIF version expected by tools */
int keep;		       	/* keep the file on exit ? */
int sub_version;		/* version number of the cif.h */
#endif
{
    _cif_version = 3;
    if (sub_version != CIF_SUB_VERSION_3)
	return(CIF_SUBVER);

    return(Cif_Cifconv(filename, optype, rtypes,
		       version, keep));
}


/*
 * Duplicate a cif record, noting the new new one so that it can be
 * freed later to avoid memory leaks.
 */

static struct Cif_generic **record_store = (struct Cif_generic **) NULL;
static int current_store_record = 0;
static int max_store_record = 0;

static struct Cif_generic *cif_copy_record(struct Cif_generic *cifp)
{
    struct Cif_generic *return_cifp = Cif_Duplicate(cifp);

    if (current_store_record == max_store_record) {
	max_store_record+=200;
	if (record_store == (struct Cif_generic **) NULL)
	    record_store = (struct Cif_generic **)
		malloc(max_store_record * sizeof(struct Cif_generic *));
	else
	    record_store = (struct Cif_generic **)
		realloc(record_store,
			max_store_record * sizeof(struct Cif_generic *));

    }

    record_store[current_store_record++] = return_cifp;
    return(return_cifp);
}


/* Free up all of the space attached to copied records */
static void free_copied_blocks()
{
    int i;

    for (i = 0; i < current_store_record; i++) {
	Cif_Free(record_store[i]);
    }
    current_store_record = 0;
}



/* ------------------------------------------------------------------------
 * sortfile reorganizes the file into sorted order and adds the directory
 * records.
 * ------------------------------------------------------------------------
 */

static int sortfile (
	struct Cif_generic *cifp) 			/* pointer to input cif header structure */
{

	int i, rtype;
	int unit_found = 0;				/* previous unit encountered flag */
	struct Cif_generic *fdp;		/* pointer to file directory */
	struct Cif_generic *cifp1;		/* cif record pointer */
	long sid;
	struct Cif_urectbl *urp;
	int in_unit = 0;			/* not currently processing
						 * a unit */

	/* Test the output file for ability to position */

	if (Cif_Getpos (outfd) >= 0)
		canpos = True;

	/* Add header structure to pointer array.  Create the unit directory table.
	 * Read each record and process.
	 */

	CIFHDR(cifp)->form = CIF_FORM_SORTED;
	/*
	 * We are writing a cifconv mode binary file, so set the bintype
	 * to show that cifconv wrote this cif file
	 */

	CIFHDR(cifp)->bintype = CIF_FORM_CIFCONV;

	/* Make sure that the record structures are all empty */
	for (i = 0; i < CIF_MAXRECORD; i++) {
	    patbl[i].aptr = NULL;
	    patbl[i].psize = 0;
	    patbl[i].next = 0;
	}

	if ((cifp1 = cif_copy_record(cifp)) == NULL)
		MEM_ERROR;
	addstruct (cifp1);
	while ((rtype = Cif_Getrecord (infd, &cifp)) >= 0) {
		if (rtype == CIF_UNIT) {

		    in_unit = 1;

			/* If one previous unit encountered, create the scratch file.  If
			 * any previous units found, write the unit to the scratch file,
			 * release the memory used for the previous unit and clear the patbl
			 * arrays for unit records.  Add the unit record.
			 */

			if ((cifp = cif_copy_record (cifp)) == NULL) MEM_ERROR;
			if (unit_found == 1) {
				tfile = tempnam (NULL, "cif.");
				if ((tmpfd = Cif_Open (tfile, "w", NULL, CIF_VERSION)) < 0) {
					(void) fprintf (stderr, "libcif: cannot create temporary file %s - %s\n",
						tfile, Cif_Errstring(tmpfd));
					return (tmpfd);
				}
			}
			if (unit_found > 0) {
				if ((i = write_unit (tmpfd, False, True)) < 0)
					return (i);
				(void) Cif_Release (infd, CIF_MEM_KEEP);
				for (i = 0; i < CIF_MAXRECORD; i++)
					if (unit_record[i]) patbl[i].next = 0;
			}
			unit_found++;
			addstruct (cifp);
			addunit(cifp);
			makeudir();
		}

		else if (rtype == CIF_ENDUNIT) {

			addstruct (cifp);
			cifp1 = *(patbl[CIF_UNITDIR].aptr);
			urp = CIFUDIR(cifp1)->ur;
			for (i = 0; i < CIF_MAXRECORD; i++) {
				if (unit_record[i])
					urp[i].nrecords = patbl[i].next;
			}

			in_unit = 0;
		}

		else if (unit_record[rtype])
			addstruct (cifp);

		else {

			/* Copy the record to new space so it will survive a call to
			 * Cif_Release and add to the record list.
			 */

			if ((cifp1 = cif_copy_record (cifp)) == NULL)
				MEM_ERROR;
			addstruct (cifp1);
		}

	}
	if (rtype != CIF_EOF) {
		(void) fprintf (stderr, "libcif: error reading input file - %s\n",
			 Cif_Errstring(rtype));
		return (rtype);
	}

	/*
	 * If we have hit EOF without the endunit, something has gone
	 * wrong, so flush the unit
	 */
	if (in_unit == 1) {
	    struct Cif_urectbl *urp;

	    addstruct (cifp);
	    cifp1 = *(patbl[CIF_UNITDIR].aptr);
	    urp = CIFUDIR(cifp1)->ur;
	    for (i = 0; i < CIF_MAXRECORD; i++) {
		if (unit_record[i])
		    urp[i].nrecords = patbl[i].next;
	    }
	}

	/* If more than one unit, copy last unit to scratch file.  */

	if (unit_found > 1) {
		if ((i = write_unit (tmpfd, False, True)) < 0)
			return (i);
	}

	/* Remap the file ids if needed then determine and save the values for
	 * the file directory.
	 */

#ifdef REMAP_FID
	if (Remap_id == True)
		remap_files ();
#endif

	/* If the file directory doesn't exist, create it and the unit table */

	if (patbl[CIF_FILEDIR].aptr == NULL) {
		if ((fdp = (struct Cif_generic *) malloc (sizeof(struct Cif_filedir)))
			== NULL) MEM_ERROR;
		(void) memset ((char *)fdp, '\0', sizeof(struct Cif_filedir));
		CIFFDIR(fdp)->rectype = CIF_FILEDIR;
		CIFFDIR(fdp)->nunits = 0;
		addstruct (fdp);
	}

	cifp1 = *(patbl[CIF_FILEDIR].aptr);
	CIFFDIR(cifp1)->maxfid = get_max_fid ();
	CIFFDIR(cifp1)->nfiles = patbl[CIF_FILE].next;
	CIFFDIR(cifp1)->nincs = patbl[CIF_INCLUDE].next;

	/* Write out the header.  If a single unit, write it from patbl.  If
	 * multiple units, copy 'em from the scratch file.
	 */
	
	if (unit_found == 1) {
		if (Remap_id == True)
			remap_symbols();
		sid = get_max_sid ();
		cifp1 = *(patbl[CIF_UNITDIR].aptr);
		CIFUDIR(cifp1)->maxsid = sid;
		cifp1 = *(patbl[CIF_FILEDIR].aptr);
		CIFFDIR(cifp1)->maxsid = sid;
	}
	if (write_header() != 0)
		return(1);

	/* If no units found, something has gone wring */
	if (unit_found == 0 &&
	    in_unit == 0)
	    return(-1);

	if (unit_found == 1) {
		if ((i = write_unit(outfd, canpos, True)) < 0)
			return (i);
	}
	else {
	    if (unit_found >= 1) {
		if (copy_units () != 0)
		    return (1);
	    }
	}

	if (canpos == True) {
		if ((i = Cif_Setpos (outfd, fdirpos)) < 0)
			OUT_ERROR ("positioning", outfd, i);
		cifp1 = *(patbl[CIF_FILEDIR].aptr);
		if ((i = Cif_Putrecord (outfd, cifp1)) < 0)
			OUT_ERROR ("writing", outfd, i);
	}

	/* Free up the memory */
	for (i = 0; i < CIF_MAXRECORD; i++) {

	    if (patbl[i].aptr != (struct Cif_generic **) NULL) {

		if (i == CIF_FILEDIR) {
		    if (CIFFDIR(patbl[i].aptr[0])->ut !=
			(struct Cif_unittbl *) NULL)
			(void) free(CIFFDIR(patbl[i].aptr[0])->ut);

		    (void) free(CIFFDIR(patbl[i].aptr[0]));
		}
		(void) free(patbl[i].aptr);
		patbl[i].aptr = (struct Cif_generic **) NULL;
	    }
	    patbl[i].psize = 0;
	    patbl[i].next = 0;
	}

	return (0);

}

/* --------------------------------------------------------------------------
 * addstruct adds one structure to the appropriate pointer array in patbl.
 * --------------------------------------------------------------------------
 */

static void addstruct (
	struct Cif_generic *cifp)		/* pointer to CIF structure */
{
	static int last_line = 0; /* stores the last position of a stmt record */
	static int last_cpos = 0; /* stmt's at the same position need to get
				   * an extra marker showing their relative
				   * line positions as qsort is not
				   * guaranteed to preserve "like" records
				   * the rectype field is used for this purpose;
				   * which we ensure is reset after the sort.
				   */
	static int last_position = 0; /* order for records at some position */

	register int i, rtype;

	rtype = cifp->rectype;
	if ((lang == CIF_LG_C || lang == CIF_LG_CC) && rtype == CIF_STMT_TYPE) {
		if (CIFSTMT(cifp)->line == last_line &&
		    CIFSTMT(cifp)->cpos == last_cpos) {
			last_position++;
			cifp->rectype = last_position;
				/* this is really bad, but we can get away with
				 * it because; a) rectype will be reset,
				 *	       b) it is not needed for now
				 * and	       c) we can't increase the record
				 *		 size to do it correctly for
				 *		 users already use this struct
				 */
		} 
		else {
			last_line = CIFSTMT(cifp)->line;
			last_cpos = CIFSTMT(cifp)->cpos;
			last_position = 0;
			cifp->rectype = 0;
		}

	}
	if (patbl[rtype].aptr == NULL) {
		patbl[rtype].psize = pabump[rtype];
		if ((patbl[rtype].aptr = (struct Cif_generic **) malloc (
				sizeof(struct Cif_generic *) * patbl[rtype].psize)) == NULL)
		{
			MEM_ERROR;
		}
	}
	i = patbl[rtype].next++;
	if ((i) >= patbl[rtype].psize) {
		patbl[rtype].psize += pabump[rtype];

		if ((patbl[rtype].aptr = (struct Cif_generic **) realloc (
				(char *)patbl[rtype].aptr,
				sizeof(struct Cif_generic *)*patbl[rtype].psize)) == NULL)
		{
			MEM_ERROR;
		}
	}
	(patbl[rtype].aptr)[i] = cifp;
}


/* --------------------------------------------------------------------------
 * addunit adds another unit to the file directory.  If the file directory
 * doesn't exit, it's created first.
 * --------------------------------------------------------------------------
 */

static void addunit (
	struct Cif_generic *unitp)		/* pointer to unit record structure */
{

#	define UTBUMP 100				/* Increment size for unit table */
	static int utsize = 0;		/* current allocated size of unit table */

	int i;
	struct Cif_generic *fdp;		/* pointer to file directory */
	struct Cif_unittbl *ut;			/* pointer to unit table */

	/* If the file directory doesn't exist, create it and the unit table */

	if (patbl[CIF_FILEDIR].aptr == NULL) {
		if ((fdp = (struct Cif_generic *) malloc (sizeof(struct Cif_filedir)))
			== NULL) MEM_ERROR;
		(void) memset ((char *)fdp, '\0', sizeof(struct Cif_filedir));
		CIFFDIR(fdp)->rectype = CIF_FILEDIR;
		CIFFDIR(fdp)->nunits = 0;
		utsize = UTBUMP;
		if ((ut = CIFFDIR(fdp)->ut = (struct Cif_unittbl *) malloc
			(sizeof(struct Cif_unittbl)*utsize)) == NULL) MEM_ERROR;
		addstruct (fdp);
	}

	/* If the unit table is full, increase the size. */

	fdp = *(patbl[CIF_FILEDIR].aptr);
	if ((int) CIFFDIR(fdp)->nunits >= utsize) {
		utsize += UTBUMP;
		if ((ut = CIFFDIR(fdp)->ut = (struct Cif_unittbl *) realloc (
				(char *) CIFFDIR(fdp)->ut,
				sizeof(struct Cif_unittbl)*utsize)) == NULL) MEM_ERROR;

	}

	/* add the unit to the unit table */

	i = CIFFDIR(fdp)->nunits++;
	ut = CIFFDIR(fdp)->ut;
        (void) memset((char *)&ut[i], 0, sizeof(struct Cif_unittbl));
	ut[i].name = /* strdup( */ CIFUNIT(unitp)->name /* ) */;
	ut[i].nlen = strlen(ut[i].name);
	ut[i].unitpos = 0;

}

/* --------------------------------------------------------------------------
 * The various record compare routines compare two CIF structures for sorting
 * by qsort.  Structures are sorted as follows:
 *
 * CIF_CALLSITE		comp_callsite 		by symbol id, file, line, and column
 * CIF_COMBLK		comp_comblk	       	by symbol id
 * CIF_CONST		comp_const	 	by symbol id
 * CIF_ENTRY		comp_entry	       	by symbol id
 * CIF_FILE	       	comp_file	       	by file id
 * CIF_LABEL		comp_label	       	by symbol id
 * CIF_LOOP	       	comp_label	       	by file, line, and column
 * CIF_MESSAGE		comp_message	 	by file, line, and column
 * CIF_NAMELIST		comp_namelist 		by symbol id
 * CIF_OBJECT		comp_object	       	by symbol id
 * CIF_STMT_TYPE	comp_stmt_type 		by file, line, and column
 * CIF_USAGE		comp_usage	       	by symbol id
 * CIF_CDIR		comp_cdir		by file, line and column
 * CIF_CDIR_DOSHARED   	comp_cdir_doshared     	by file, line and column
 # CIF_GEOMETRY		comp_geometry		by geometry id
 * CIF_CONTINUATION	comp_continuation	by file, line and column
 * CIF_TRANSFORM	comp_transform	       	by file and line
 * CIF_C_TAG		comp_c_tag	       	by symbol id
 * CIF_C_MESSAGE	comp_c_message		by file, line
 * CIF_C_CONST		comp_c_const		by symbol id
 * CIF_C_ENTRY		comp_c_entry		by symbol id
 * CIF_C_OBJECT		comp_c_object		by symbol id
 * CIF_C_LINT_DIRECTIVE	comp_c_lint_directive  	by object id
 * CIF_C_MACRO_DEF	comp_c_macro_def       	by macro id
 * CIF_C_MACRO_UNDEF	comp_c_macro_undef     	by macro id
 * CIF_C_MACRO_USAGE	comp_c_macro_usage     	by macro id
 * CIF_C_ENTRY_END	comp_c_entry_end       	by symbol id
 * CIF_F90_CALLSITE 	comp_f90_callsite,	by symbol id, file, line, and column
 * CIF_F90_COMBLK  	comp_f90_comblk,      	by symbol id
 * CIF_F90_CONST	comp_f90_const,	 	by symbol id
 * CIF_F90_ENTRY	comp_f90_entry,	       	by symbol id
 * CIF_F90_LOOP    	comp_f90_loop,	       	by file, line, and column
 * CIF_F90_DERIVED_TYPE	comp_f90_derived_type,	by symbol id
 * CIF_F90_LABEL	comp_f90_label,	       	by file, line, and colum
 * CIF_F90_NAMELIST	comp_f90_namelist,     	by symbol id
 * CIF_F90_OBJECT	comp_f90_object,       	by symbol id
 * CIF_F90_BEGIN_SCOPE	comp_f90_begin_scope,	by scope id
 * CIF_F90_END_SCOPE	comp_f90_end_scope,	by scope id
 * CIF_F90_SCOPE_INFO	comp_f90_scope_info,	by scope id
 * CIF_F90_USE_MODULE	comp_f90_use_module,	by symbol id
 * CIF_F90_RENAME	comp_f90_rename,	by symbol id
 * CIF_F90_INT_BLOCK	comp_f90_int_block,	by scope id
 * CIF_F90_VECTORIZATION comp_f90_vectorization, ??
 * CIF_CC_TYPE 		comp_cc_type, 		by type id
 * CIF_CC_ENTRY 	comp_cc_entry, 		by symbol id
 * CIF_CC_OBJ 		comp_cc_obj, 		by symbol id
 * CIF_CC_SUBTYPE	comp_cc_subtype,	by symbol id
 * CIF_CC_ENUM 		comp_cc_enum, 		by symbol id
 * CIF_CC_EXPR 		comp_cc_expr, 		by expression id
 * --------------------------------------------------------------------------
 */

static int comp_callsite (
	struct Cif_callsite **p1,
	struct Cif_callsite **p2)
{
	register int ret;

	if ((ret = ( (*p1)->entryid - (*p2)->entryid )) != 0)
	    return (ret);
	else if ((ret = ( (*p1)->fid - (*p2)->fid )) != 0)
		return (ret);
	else if ((ret = ( (*p1)->line - (*p2)->line )) != 0)
		return (ret);
	else
		return ( (*p1)->cpos - (*p2)->cpos );
}

static int comp_comblk (
	struct Cif_comblk **p1, 
	struct Cif_comblk **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_const (
	struct Cif_const **p1,
	struct Cif_const **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_entry (
	struct Cif_entry **p1,
	struct Cif_entry **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_file (
	struct Cif_file **p1,
	struct Cif_file **p2)
{
	return ( (*p1)->fid - (*p2)->fid );
}

static int comp_label (
	struct Cif_label **p1,
	struct Cif_label **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_loop (
	struct Cif_loop **p1,
	struct Cif_loop **p2)
{
	register int ret;

	if ((ret = ( (*p1)->sfid - (*p2)->sfid )) != 0)
		return (ret);
	else if ((ret = ( (*p1)->strline - (*p2)->strline )) != 0)
		return (ret);
	else
		return ( (*p1)->strcpos - (*p2)->strcpos );
}

static int comp_message (
	struct Cif_message **p1,
	struct Cif_message **p2)
{
	register int ret;

	if ((ret = ( (*p1)->fid - (*p2)->fid )) != 0)
		return (ret);
	else if ((ret = ( (*p1)->fline - (*p2)->fline )) != 0)
		return (ret);
	else
		return ( (*p1)->cpos - (*p2)->cpos );
}

static int comp_namelist (
	struct Cif_namelist **p1,
	struct Cif_namelist **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_object (
	struct Cif_object **p1,
	struct Cif_object **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_stmt_type (
	struct Cif_stmt_type **p1,
	struct Cif_stmt_type **p2)
{
	register int ret;

	if ((ret = ( (*p1)->fid - (*p2)->fid )) != 0)
		return (ret);
	else if ((ret = ( (*p1)->line - (*p2)->line )) != 0)
		return (ret);
        else if (lang == CIF_LG_F90 && _cif_version >= 3) {
          /* F90 V3 has introduced a statement ordering field which
           * is stored in the eline field - otherwise unused for f90 - 
           * this orders statements on the same line as the cpos is
           * not guaranteed.
           */
          if ((ret = ( (*p1)->eline - (*p2)->eline )) != 0)
            return(ret);
          else
             return((*p1)->cpos - (*p2)->cpos);
        }
        else if ((ret = ( (*p1)->cpos - (*p2)->cpos )) != 0)
		return(ret);
	else /* rectype will contain indicator of original ordering */
		return((*p1)->rectype - (*p2)->rectype);
}


static int comp_usage (
	struct Cif_usage **p1,
	struct Cif_usage **p2)
{
    register int ret;
    register int i;

    if ((ret = ( (*p1)->symid - (*p2)->symid )) != 0)
	return (ret);
    else/* sort on # member id's, if any */
	if ((ret = ( (*p1)->nmembs - (*p2)->nmembs)) != 0)
	    return (ret);
	else
	    if ((*p1)->nmembs == 0)
		return(0);
	    else { /* sort on each member id in turn */
		for (i = 0; i < (int) (*p1)->nmembs; i++) {
		    if ((ret = ( (*p1)->membs[i] - (*p2)->membs[i] )) != 0)
			return (ret);
		}
	    }

    return(0);
}



#ifndef CRAY2
static int comp_f90_callsite (
	struct Cif_f90_callsite **p1,
	struct Cif_f90_callsite **p2)
{
	register int ret;

	if ((ret = ( (*p1)->entryid - (*p2)->entryid )) != 0)
		return (ret);
	else if ((ret = ( (*p1)->fid - (*p2)->fid )) != 0)
		return (ret);
	else if ((ret = ( (*p1)->line - (*p2)->line )) != 0)
		return (ret);
	else
		return ( (*p1)->cpos - (*p2)->cpos );
}

static int comp_f90_comblk (
	struct Cif_f90_comblk **p1, 
	struct Cif_f90_comblk **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_f90_const (
	struct Cif_f90_const **p1,
	struct Cif_f90_const **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_f90_entry (
	struct Cif_f90_entry **p1,
	struct Cif_f90_entry **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}


static int comp_f90_label (
	struct Cif_f90_label **p1,
	struct Cif_f90_label **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_f90_loop (
	struct Cif_f90_loop **p1,
	struct Cif_f90_loop **p2)
{
	register int ret;

	if ((ret = ( (*p1)->sfid - (*p2)->sfid )) != 0)
		return (ret);
	else if ((ret = ( (*p1)->strline - (*p2)->strline )) != 0)
		return (ret);
	else
		return ( (*p1)->strcpos - (*p2)->strcpos );
}

static int comp_f90_namelist (
	struct Cif_f90_namelist **p1,
	struct Cif_f90_namelist **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_f90_object (
	struct Cif_f90_object **p1,
	struct Cif_f90_object **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_f90_derived_type (
	struct Cif_f90_derived_type **p1,
	struct Cif_f90_derived_type **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_f90_begin_scope (
	struct Cif_f90_begin_scope **p1,
	struct Cif_f90_begin_scope **p2)
{
	return ( (*p1)->scopeid - (*p2)->scopeid );
}

static int comp_f90_end_scope (
	struct Cif_f90_end_scope **p1,
	struct Cif_f90_end_scope **p2)
{
	return ( (*p1)->scopeid - (*p2)->scopeid );
}

static int comp_f90_scope_info (
	struct Cif_f90_scope_info **p1,
	struct Cif_f90_scope_info **p2)
{
	return ( (*p1)->scopeid - (*p2)->scopeid );
}

static int comp_f90_int_block (
	struct Cif_f90_int_block **p1,
	struct Cif_f90_int_block **p2)
{
	return ( (*p1)->intid - (*p2)->intid );
}

static int comp_f90_use_module (
	struct Cif_f90_use_module **p1,
	struct Cif_f90_use_module **p2)
{
	return ( (*p1)->modid - (*p2)->modid );
}

static int comp_f90_rename (
	struct Cif_f90_rename **p1,
	struct Cif_f90_rename **p2)
{
	return ( (*p1)->modid - (*p2)->modid );
}

static int comp_f90_vectorization (
	struct Cif_f90_vectorization **p1,
	struct Cif_f90_vectorization **p2)
{
	return ( (*p1)->rectype - (*p2)->rectype );
}
#endif /* CRAY2 */


static int comp_cdir (
	struct Cif_cdir **p1,
	struct Cif_cdir **p2)
{
	register int ret;

	if ((ret = ( (*p1)->fid - (*p2)->fid )) != 0)
		return (ret);
	else if ((ret = ( (*p1)->line - (*p2)->line )) != 0)
		return (ret);
	else
		return ( (*p1)->cpos - (*p2)->cpos );
}

static int comp_cdir_doshared (
	struct Cif_cdir_doshared **p1,
	struct Cif_cdir_doshared **p2)
{
	register int ret;

	if ((ret = ( (*p1)->fid - (*p2)->fid )) != 0)
		return (ret);
	else if ((ret = ( (*p1)->line - (*p2)->line )) != 0)
		return (ret);
	else
		return ( (*p1)->cpos - (*p2)->cpos );
}


static int comp_geometry (
	struct Cif_geometry **p1,
	struct Cif_geometry **p2)
{
	return ( (*p1)->geomid - (*p2)->geomid );
}


static int comp_continuation (
	struct Cif_continuation **p1,
	struct Cif_continuation **p2)
{
	register int ret;

	if ((ret = ( (*p1)->fid - (*p2)->fid )) != 0)
		return (ret);
	else if ((ret = ( (*p1)->line - (*p2)->line )) != 0)
		return (ret);
	else
		return ( (*p1)->cpos - (*p2)->cpos );
}

static int comp_transform (
	struct Cif_transform **p1,
	struct Cif_transform **p2)
{
	register int ret;

	if ((ret = ( (*p1)->fid - (*p2)->fid )) != 0)
		return (ret);
	else
		return ( (*p1)->line - (*p2)->line );
}


static int comp_c_tag (
	struct Cif_c_tag **p1,
	struct Cif_c_tag **p2)
{
	return ( (*p1)->tagid - (*p2)->tagid );
}

static int comp_c_message (
	struct Cif_c_message **p1,
	struct Cif_c_message **p2)
{
	register int ret;

	if ((ret = ( (*p1)->fline - (*p2)->fline )) != 0)
		return (ret);
	else 
		return ( (*p1)->iline - (*p2)->iline );
}

static int comp_c_const (
	struct Cif_c_const **p1,
	struct Cif_c_const **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_c_entry (
	struct Cif_c_entry **p1,
	struct Cif_c_entry **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_c_object (
	struct Cif_c_object **p1,
	struct Cif_c_object **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_c_lint_directive (
	struct Cif_c_lint_directive **p1,
	struct Cif_c_lint_directive **p2)
{
	return ( (*p1)->objid - (*p2)->objid );
}

static int comp_c_macro_def (
	struct Cif_c_macro_def **p1,
	struct Cif_c_macro_def **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_c_macro_undef (
	struct Cif_c_macro_undef **p1,
	struct Cif_c_macro_undef **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}


static int comp_c_macro_usage (
	struct Cif_c_macro_usage **p1,
	struct Cif_c_macro_usage **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_c_entry_end (
	struct Cif_c_entry_end **p1,
	struct Cif_c_entry_end **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_cc_type (
	struct Cif_cc_type **p1,
	struct Cif_cc_type **p2)
{
	return ( (*p1)->typeId - (*p2)->typeId );
}

static int comp_cc_entry (
	struct Cif_cc_entry **p1,
	struct Cif_cc_entry **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_cc_obj (
	struct Cif_cc_obj **p1,
	struct Cif_cc_obj **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_cc_subtype (
	struct Cif_cc_subtype **p1,
	struct Cif_cc_subtype **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_cc_enum (
	struct Cif_cc_enum **p1,
	struct Cif_cc_enum **p2)
{
	return ( (*p1)->symid - (*p2)->symid );
}

static int comp_cc_expr (
	struct Cif_cc_expr **p1,
	struct Cif_cc_expr **p2)
{
	return ( (*p1)->exprid - (*p2)->exprid );
}

static int comp_src_pos (
	struct Cif_src_pos **p1,
	struct Cif_src_pos **p2)
{
	register int ret;

	if ((ret = ( (*p1)->kind - (*p2)->kind )) != 0)
		return (ret);
	else if ((ret = ( (*p1)->fid - (*p2)->fid )) != 0)
		return (ret);
	else
		return ( (*p1)->symid - (*p2)->symid );
}

static int comp_orig_cmd (
	struct Cif_orig_cmd **p1,
	struct Cif_orig_cmd **p2)
{
	return ( (*p1)->nlen - (*p2)->nlen );
}




/* --------------------------------------------------------------------------
 * copy_units copies all the units from the scratch file to the output file.
 * --------------------------------------------------------------------------
 */

static int copy_units (
	void)
{

	int recno=0;
	int i, rtype;
	long sid;
	struct Cif_generic *cifp;

	if (tmpfd != -1) {
	    (void) Cif_Close (tmpfd, CIF_MEM_KEEP);
	    tmpfd = -1;
	}
	if ((tmpfd = Cif_Open (tfile, "r", NULL, CIF_VERSION)) < 0) {
		(void) fprintf (stderr, "libcif: cannot open temporary file %s - %s\n",
				tfile, Cif_Errstring(tmpfd));
		return (tmpfd);
	}
	(void) Cif_Memmode (tmpfd, CIF_MEM_MANAGED);
#ifndef DEBUG
	/* don't remove the temporary file in debug mode */
	(void) unlink (tfile);
#endif

	while ((rtype = Cif_Getrecord (tmpfd, &cifp)) >= 0) {

		recno++;
		if (rtype == CIF_UNIT) {
			for (i = 0; i < CIF_MAXRECORD; i++)
				if (unit_record[i]) patbl[i].next = 0;
			addstruct (cifp);
		}

		else if (rtype == CIF_ENDUNIT) {
			addstruct (cifp);
			if (Remap_id == True)
				remap_symbols ();
			sid = get_max_sid ();
			cifp = *(patbl[CIF_UNITDIR].aptr);
			CIFUDIR(cifp)->maxsid = sid;
			cifp = *(patbl[CIF_FILEDIR].aptr);
			if (sid > (int) CIFFDIR(cifp)->maxsid)
				CIFFDIR(cifp)->maxsid = sid;
			if ((i = write_unit (outfd, canpos, False)) < 0)
				return (i);
			(void) Cif_Release (tmpfd, CIF_MEM_KEEP);
		}

		else
			addstruct (cifp);
	}
	if (rtype != CIF_EOF)
		OUT_ERROR ("reading", tmpfd, rtype);

	return (0);
}

/* --------------------------------------------------------------------------
 * makeudir creates a Cif_unitdir structure to contain entries for each type
 * of unit structure present.
 * --------------------------------------------------------------------------
 */

static void makeudir(
	void)
{

	int i, n;
	static struct Cif_unitdir *udp = (struct Cif_unitdir *) NULL;
	static struct Cif_urectbl *urp = (struct Cif_urectbl *) NULL;

	if (udp == (struct Cif_unitdir *) NULL) {
	    if ((udp = (struct Cif_unitdir *) malloc (sizeof(struct Cif_unitdir))) ==
		NULL) MEM_ERROR;
	}
	(void) memset ((char *)udp, '\0', sizeof(struct Cif_unitdir));
	udp->rectype = CIF_UNITDIR;
	udp->nsections = CIF_MAXRECORD;
	n = sizeof(struct Cif_urectbl) * CIF_MAXRECORD;
	if (urp == (struct Cif_urectbl *) NULL) {
	    if ((urp = udp->ur = (struct Cif_urectbl *) malloc (n)) == NULL)
		MEM_ERROR;
	}
	else {
	    udp->ur = urp;
	}
	(void) memset ((char *)urp, '\0', n);
	for (i = 0; i < CIF_MAXRECORD; i++) {
		if (unit_record[i])
			urp[i].rectype = i;
	}
	addstruct (CIFGEN(udp));
}

/* --------------------------------------------------------------------------
 * merge_usages combines the use tables for usage records of the same id.
 * comp_use compares two use entries for sorted by qsort.
 * --------------------------------------------------------------------------
 */

static int comp_use (
	struct Cif_use *u1,
	struct Cif_use *u2)
{
	register int ret;

	if ((ret = ( u1->fid - u2->fid )) != 0)
		return (ret);
	else if ((ret = ( u1->line - u2->line )) != 0)
		return (ret);
	else
		return ( u1->cpos - u2->cpos );
}

static int merge_usages (
	void)
{
	int i, j, n;
	int numu;							/* number of usage records */
	int nuses;							/* number of uses */
	int si;								/* usage array scanning index */
	int ti;								/* usage array trailing index */
	struct Cif_usage **up;			/* array of pointers to usage records */
	struct Cif_use *use;				/* pointer to combined use array */
	struct Cif_use *use1, *use2;	/* pointers for moving use entries */

	struct Cif_generic *cifp1;		/* cif record pointer */
	struct Cif_urectbl *urp;

	/* Check if duplicate usage ids present */

	numu = patbl[CIF_USAGE].next;
	up = (struct Cif_usage **)patbl[CIF_USAGE].aptr;

	if (numu > global_nuses_allocated) {
	    if (global_nuses == (int *) NULL)
		global_nuses = (int *) malloc(numu * sizeof(int));
	    else {
		global_nuses = (int *) realloc(global_nuses,
					       numu * sizeof(int));
	    }
	    global_nuses_allocated = numu;
	}
	memset((char *) global_nuses, '\0', numu * sizeof(int));

	for (si = 0; si < numu-1; si++) {
	    if ( up[si]->symid == up[si+1]->symid) break;
	    global_nuses[si] = 1;
	}

	if (si < numu - 1) {

		/* Scan usages, merging records where ids are the same */

		si = ti = 0;
		while (si < numu) {
			if (si == numu-1 ||
			    up[si]->symid != up[si+1]->symid ||
			    up[si]->nmembs !=  up[si+1]->nmembs) {

			    /* single usage for this id, copy it. */
			    global_nuses[ti] = 1;
			    *up[ti++] = *up[si++];

			}
			else {
				/* Multiple usages - find number, combine, and sort them */
				nuses = up[si]->nuses;;
				for (n = 1; si+n < numu && up[si+n]->symid == up[si]->symid; n++) {

				    /*
				     * number of parent symbold ids, eg a in a%b is different,
				     * so this usage is not identical to the last
				     */

				    if (up[si]->nmembs !=  up[si+n]->nmembs)
					break;

				    for (i = 0; i < (int) up[si]->nmembs; i++) {
					if (up[si]->membs[i] != up[si+n]->membs[i])
					    break;
				    }

				    /*
				     * If we didn't get to the end, a difference was found,
				     * so this usage is not idenical to the last
				     */

				    if (i < (int) up[si]->nmembs)
					break;

				    nuses += up[si+n]->nuses;
				}

				/* Combine all of the usages into one new record */

				if ((use = (struct Cif_use *) malloc (sizeof(struct Cif_use)*nuses))
					 == NULL) MEM_ERROR;

				use1 = use;
				for (i = 0; i < n; i++) {
					use2 = up[si+i]->use;
					for (j = 0; j < (int) up[si+i]->nuses; j++)
						(use1++)[0] = (use2++)[0];
				}

				/* Sort the usages on fid, line number and column position */

				(void) qsort ((char *)use,
					      nuses, sizeof(struct Cif_use),
					      (int(*)()) comp_use);

				/*
				 * Put the new usage record back into the list;
				 * note, ti must be <= si, so never overwrite anything
				 * that we are about to read
				 */

				up[ti]->nuses = nuses;
				up[ti]->symid = up[si]->symid;
				up[ti]->nmembs = up[si]->nmembs;
				up[ti]->membs = up[si]->membs;
				up[ti]->use = use;

				global_nuses[ti] = nuses;

				si += n;
				ti++;
			}
		}
		patbl[CIF_USAGE].next = ti;

		/* Update the unitdir record entry */

		cifp1 = *(patbl[CIF_UNITDIR].aptr);
		urp = CIFUDIR(cifp1)->ur;
		urp[CIF_USAGE].nrecords = ti;
	}

	return (0);
}

/* --------------------------------------------------------------------------
 * write_header writes out the records that go at the head of the file.
 * --------------------------------------------------------------------------
 */

/* --- output order of record types in output file trailer section --- */
#define HRECORDS 15				/* number of trailer record types */
static const int horder[HRECORDS] = {
	CIF_CIFHDR,
	CIF_FILEDIR,
	CIF_SRCFILE,
	CIF_FILE,
	CIF_INCLUDE,
	CIF_SRC_POS,
	CIF_ORIG_CMD,
	CIF_EDOPTS,
	CIF_MACH_CHAR,
	CIF_MISC_OPTS,
	CIF_F90_MISC_OPTS,
	CIF_OPT_OPTS,
	CIF_F90_OPT_OPTS,
	CIF_C_OPTS,
	CIF_SUMMARY
};

static int write_header (
	void)
{

	int i, j, rtype;
	long status;
	struct Cif_generic *sp;

	/* Sort the CIF_FILE records.  Write 'em out. */

	if (patbl[CIF_FILE].next > 1)
		(void) qsort ((char *)patbl[CIF_FILE].aptr, patbl[CIF_FILE].next,
			sizeof(struct Cif_generic *), qcompare[CIF_FILE]);

	sp = *(patbl[CIF_CIFHDR].aptr);
	CIFHDR(sp)->cont_id = 1;
	CIFHDR(sp)->srcfid = get_srcfid();  /* set the mapped srcfid */

	/*
	 * We are writing a cifconv mode binary file, so set the bintype
	 * to show that cifconv wrote this cif file
	 */

	CIFHDR(sp)->bintype = CIF_FORM_CIFCONV;

	if (canpos == True)
		CIFHDR(sp)->posinfo = 1;
	for (i = 0; i < HRECORDS; i++) {
		rtype = horder[i];
		if (rtype == CIF_FILEDIR && canpos == True) {
			if ((fdirpos = Cif_Getpos (outfd)) < 0)
				OUT_ERROR ("positioning", outfd, fdirpos);
		}
		for (j = 0; j < patbl[rtype].next; j++) {
			sp = (patbl[rtype].aptr)[j];
			if ((status = Cif_Putrecord(outfd, sp)) < 0)
				OUT_ERROR ("writing", outfd, status);
		}
	}

	return (0);

}

static int get_srcfid (
	void)
{
	struct Cif_generic *sp, **spp;
	int rcnt, srcfid;

	srcfid = 0;
	spp = patbl[CIF_SRCFILE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_SRCFILE].next; rcnt++) {
		sp = *spp++;
		srcfid = CIFSRC(sp)->fid;
	}
	return(srcfid);
}


/* --------------------------------------------------------------------------
 * write_unit writes out all of the records for one unit from memory.
 * --------------------------------------------------------------------------
 */

/* --- output order of record types in each unit of CIF --- */
#define URECORDS 54				/* number of unit record types */
static const int uorder[URECORDS] = {
	CIF_UNIT,
	CIF_UNITDIR,
	CIF_ENTRY,
	CIF_COMBLK,
	CIF_OBJECT,
	CIF_CONST,
	CIF_NAMELIST,
	CIF_C_ENTRY,
	CIF_C_OBJECT,
	CIF_C_LINT_DIRECTIVE,
	CIF_C_MACRO_DEF,
	CIF_C_MACRO_UNDEF,
	CIF_C_MACRO_USAGE,
	CIF_C_ENTRY_END,
	CIF_C_TAG,
	CIF_C_CONST,
	CIF_LABEL,
	CIF_CALLSITE,
	CIF_USAGE,
	CIF_STMT_TYPE,
	CIF_LOOP,
	CIF_MESSAGE,
	CIF_ND_MSG,
	CIF_CDIR,
	CIF_CDIR_DOSHARED,
	CIF_GEOMETRY,
	CIF_CONTINUATION,
	CIF_TRANSFORM,
	CIF_F90_CALLSITE,
	CIF_F90_COMBLK,
	CIF_F90_CONST,
	CIF_F90_ENTRY,
	CIF_F90_LOOP,
	CIF_F90_DERIVED_TYPE,
	CIF_F90_LABEL,
	CIF_F90_NAMELIST,
	CIF_F90_OBJECT,
	CIF_F90_BEGIN_SCOPE,
	CIF_F90_END_SCOPE,
	CIF_F90_SCOPE_INFO,
	CIF_F90_USE_MODULE,
	CIF_F90_RENAME,
	CIF_F90_INT_BLOCK,
	CIF_F90_VECTORIZATION,
	CIF_CC_TYPE,
	CIF_CC_ENTRY,
	CIF_CC_OBJ,
	CIF_CC_SUBTYPE,
	CIF_CC_ENUM,
	CIF_CC_EXPR,
	CIF_BE_NODE,
	CIF_BE_FID,
	CIF_C_MESSAGE,
	CIF_ENDUNIT
};

static int write_unit (
	int fd,								/* cif output file descriptor */
	enum Boolean markpos,			/* set if positioning should be done */
	enum Boolean merge_use)			/* set if usage records should be merged */
{

	int i, j, rtype;
	long status, savepos;
	char *name;
	struct Cif_generic *sp;
	struct Cif_urectbl *urp;
	struct Cif_unittbl *utp;
	int numu;							/* number of usage records */
	struct Cif_usage **up;			/* array of pointers to usage records */
	int si;					/* usage array scanning index */
	struct Cif_generic **spp;	

	/* Get the file position of the unit and save in the file directory */

	if (markpos) {
		name = CIFUNIT(*patbl[CIF_UNIT].aptr)->name;
		utp = CIFFDIR(*patbl[CIF_FILEDIR].aptr)->ut;
		i = CIFFDIR(*patbl[CIF_FILEDIR].aptr)->nunits;
		for (j = 0; j < i; j++)
			if (strcmp(name,utp[j].name) == 0 &&
			    utp[j].unitpos == 0) break;

		if ((status = utp[j].unitpos = Cif_Getpos(fd)) < 0)
			OUT_ERROR ("positioning", fd, status);
	}

	/* Sort the unit records.  Merge usage records as needed. */

	for (rtype = 0; rtype < CIF_MAXRECORD; rtype++) {
		if (unit_record[rtype] && patbl[rtype].next > 1 &&
				 qcompare[rtype] != 0)
			(void) qsort ((char *)patbl[rtype].aptr, patbl[rtype].next,
				sizeof(struct Cif_generic *), qcompare[rtype]);
	}

	/* Reset the cif_stmt_type records; ie the rectype back
	 * to CIF_STMT_TYPE
 	 */

	if (lang == CIF_LG_C || lang == CIF_LG_CC) {
		spp = patbl[CIF_STMT_TYPE].aptr;
		for (i = 0; i < patbl[CIF_STMT_TYPE].next; i++) {
			CIFSTMT(*spp++)->rectype = CIF_STMT_TYPE;
		}
	}

	/* Merge the usages records on symbol ids */

	if (merge_use)
	    (void) merge_usages();

/* merge_usages always returns 0
		if ((i = merge_usages ()) < 0) return (i);
*/

	/* Write the records out in order as specified by the uorder table.  Get
	 * the file position at the start of new record type and stick it in the
	 * unit directory.  After the end_unit record, save the file postion,
	 * rewrite the unit directory and restore the file position.
	 */

	sp = *(patbl[CIF_UNITDIR].aptr);
	urp = CIFUDIR(sp)->ur;
	for (i = 0; i < URECORDS; i++) {
		rtype = uorder[i];
		if (patbl[rtype].next > 0) {
			if (markpos) {
				if ((status = Cif_Getpos (fd)) < 0)
					OUT_ERROR ("positioning", fd, status);
			}
			else
				status = 0;
			urp[rtype].recpos = status;
			for (j = 0; j < patbl[rtype].next; j++) {
				if ((status = Cif_Putrecord (fd, (patbl[rtype].aptr)[j])) < 0)
					OUT_ERROR ("writing", fd, status);
			    }
		}
	}

	/* Free up space allocated for merged usages */

	if (merge_use &&
	    global_nuses != (int *) NULL) {
	    numu = patbl[CIF_USAGE].next;
	    up = (struct Cif_usage **)patbl[CIF_USAGE].aptr;
	    for (si = 0; si < numu; si++) {
		if (global_nuses[si] > 1)
		    free((char *) up[si]->use);
	    }
	}

	if (markpos) {
		if ((savepos = Cif_Getpos (fd)) < 0)
			OUT_ERROR ("positioning", fd, status);
		if ((status = Cif_Setpos (fd, urp[CIF_UNITDIR].recpos)) < 0)
			OUT_ERROR ("positioning", fd, status);
		if ((status = Cif_Putrecord (fd, *(patbl[CIF_UNITDIR].aptr))) < 0)
			OUT_ERROR ("writing", fd, status);
		if ((status = Cif_Setpos (fd, savepos)) < 0)
			OUT_ERROR ("positioning", fd, status);
	}

	return (0);
}

#ifdef DEBUG
/* --------------------------------------------------------------------------
 * dump_patbl displays the contents of patbl for debugging purposes.
 * --------------------------------------------------------------------------
 */
void dump_patbl (
	FILE *fd)
{
	int i, j;
	struct Cif_generic **rptr;

	for (i = 0; i < CIF_MAXRECORD; i++) {
		if (patbl[i].psize != 0) {
			(void) fprintf (fd, "\npatbl[%d] aptr= %d  psize= %d  next= %d\n",
						i, patbl[i].aptr, patbl[i].psize, patbl[i].next);
			rptr = patbl[i].aptr;
			for (j = 0; j < patbl[i].next; j++) {
				(void) fprintf (fd, "    item %4d  *aptr= %d  rtype= %d\n",
				         j, rptr, (*rptr)->rectype);
				rptr++;
			}
		}
	}
	
}

/* --------------------------------------------------------------------------
 * valid_patbl checks the validity of the patbl and offers a convenient
 * place for the debugger to set a breakpoint.
 * --------------------------------------------------------------------------
 */
static int valid_patbl (
	void)
{
	int i, j;
	struct Cif_generic **rptr;

	for (i = 0; i < CIF_MAXRECORD; i++) {
		if (patbl[i].psize != 0) {
			rptr = patbl[i].aptr;
			for (j = 0; j < patbl[i].next; j++) {
				if ((*rptr)->rectype != i)
					return (0);
				rptr++;
			}
		}
	}
	return (1);
}
#endif

/* --------------------------------------------------------------------------
 * init_id initializes an Id_tbl so it can start receiving ids via add_id.
 * --------------------------------------------------------------------------
 */
static void init_id (
	struct Id_tbl *idtp)
{
    idtp->cur = 1;
    (idtp->tbl)[0] = 0;
}

/* --------------------------------------------------------------------------
 * add_id adds a new id value to an Id_tbl tbl.  The table is expanded if
 * neccessary.
 * --------------------------------------------------------------------------
 */
static void add_id (
	struct Id_tbl *idtp,		/* ptr to Id_tbl structure that will receive id */
	long id)						/* value of id to add */
{

	if (idtp->cur >= idtp->max) {
		idtp->max += ID_BUMP;
		if ((idtp->tbl = (long *) realloc ((char *)idtp->tbl,
				sizeof(long)*idtp->max)) == NULL)
			MEM_ERROR;
	}
	idtp->tbl[idtp->cur++] = id;
}

/* --------------------------------------------------------------------------
 * comp_ids compares two symbol ids (longs) for use by qsort.
 * --------------------------------------------------------------------------
 */
static int comp_ids (
	long *id1, 
	long *id2)
{
	return (*id1 - *id2);
}

/* --------------------------------------------------------------------------
 * get_id retrieves the new value for a remapped id given the original value.
 * The tbl array will contain all original ids in ascending order.  get_id
 * performs a binary search of the array and returns the array index of the
 * original value.
 * --------------------------------------------------------------------------
 */
static long get_id (
	struct Id_tbl *idtp,			/* ptr to Id_tbl to scan for id value */
	long id)							/* original id value */
{

	int lower = 1; 				/* tbl search boundaries */
	int upper = idtp->cur-1;
	int mid;

	if (id == 0)
		return (0);


	mid = (upper + lower) / 2;

	while (lower < upper) {

		if (id < (idtp->tbl)[mid])
			upper = mid - 1;
		else if (id > (idtp->tbl)[mid])
			lower = mid + 1;
		else
			break;

		mid = (upper + lower) / 2;
	}

	/*
	 * Now check to see if the lookup was successful; the only way that this
	 * would not be the case, would be if the compiler has generated invalid
	 * object id's; not alot we can do about them, but report the error.
	 */

	if ((idtp->tbl)[mid] != id) {

	    if (global_error_report == True) {
		(void) fprintf(stderr,
			       "libcif : Invalid id in cif %s.\nClosest match to %ld is %ld, so using 0 in output record.\n",
			       global_outfile, id, (idtp->tbl)[mid]);
	    }
	    mid = 0;
	}

	return (mid);

}

/* --------------------------------------------------------------------------
 * remap_symbols scans the patbl contents for the current unit and remaps the
 * symbol and file id values.  It is assumed that the file ids have already
 * been accumulated and sorted.  remap_symbols collects all the symbol ids,
 * sorts 'em, and then changes all ids (both file and symbol) to the new values.
 * --------------------------------------------------------------------------
 */
static void remap_symbols (
	void)
{

	struct Cif_generic *sp, **spp;	
	int rcnt, i, j;

	init_id (&sid);

	/* Scan patbl for those structure that define symbol ids and add them
	 * to the symbol table list.
	 */

	spp = patbl[CIF_COMBLK].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_COMBLK].next; rcnt++)
		add_id (&sid, CIFCB(*spp++)->symid);

	spp = patbl[CIF_CONST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CONST].next; rcnt++)
		add_id (&sid, CIFCON(*spp++)->symid);

	spp = patbl[CIF_ENTRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_ENTRY].next; rcnt++)
		add_id (&sid, CIFENTRY(*spp++)->symid);

	spp = patbl[CIF_LABEL].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_LABEL].next; rcnt++)
		add_id (&sid, CIFLABEL(*spp++)->symid);

	spp = patbl[CIF_NAMELIST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_NAMELIST].next; rcnt++)
		add_id (&sid, CIFNL(*spp++)->symid);

	spp = patbl[CIF_OBJECT].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_OBJECT].next; rcnt++)
		add_id (&sid, CIFOBJ(*spp++)->symid);

	spp = patbl[CIF_C_CONST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_CONST].next; rcnt++)
		add_id (&sid, CIFCCON(*spp++)->symid);

	spp = patbl[CIF_C_ENTRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_ENTRY].next; rcnt++)
		add_id (&sid, CIFCENTRY(*spp++)->symid);

	spp = patbl[CIF_C_OBJECT].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_OBJECT].next; rcnt++)
		add_id (&sid, CIFCOBJ(*spp++)->symid);

	spp = patbl[CIF_C_LINT_DIRECTIVE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_LINT_DIRECTIVE].next; rcnt++)
		add_id (&sid, CIFCLDIR(*spp++)->objid);

	spp = patbl[CIF_C_MACRO_DEF].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_MACRO_DEF].next; rcnt++)
		add_id (&sid, CIFCMDEF(*spp++)->symid);

	spp = patbl[CIF_C_TAG].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_TAG].next; rcnt++)
		add_id (&sid, CIFCTAG(*spp++)->tagid);



	spp = patbl[CIF_CC_TYPE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CC_TYPE].next; rcnt++) {
		sp = *spp++;
		add_id (&sid, CIFCCTYPE(sp)->symid);
		j = CIFCCTYPE(sp)->nmem;
		for (i = 0; i < j; i++ ) {
		    add_id (&sid, CIFCCTYPE(sp)->mem[i]);
		}
	}

	spp = patbl[CIF_CC_ENTRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CC_ENTRY].next; rcnt++) {
		sp = *spp++;
		add_id (&sid, CIFCCENT(sp)->symid);
		add_id (&sid, CIFCCENT(sp)->fsymid);
		j = CIFCCENT(sp)->nparam;
		for (i = 0; i < j; i++ ) {
		    add_id (&sid, CIFCCENT(sp)->param[i]);
		}
	}

	spp = patbl[CIF_CC_OBJ].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CC_OBJ].next; rcnt++)
		add_id (&sid, CIFCCOBJ(*spp++)->symid);

	spp = patbl[CIF_CC_SUBTYPE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CC_SUBTYPE].next; rcnt++)
		add_id (&sid, CIFCCSUB(*spp++)->symid);

	spp = patbl[CIF_CC_ENUM].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CC_ENUM].next; rcnt++)
		add_id (&sid, CIFCCENUM(*spp++)->symid);



	spp = patbl[CIF_F90_COMBLK].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_COMBLK].next; rcnt++)
		add_id (&sid, CIFF90CB(*spp++)->symid);

	spp = patbl[CIF_F90_CONST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_CONST].next; rcnt++)
		add_id (&sid, CIFF90CON(*spp++)->symid);

	spp = patbl[CIF_F90_ENTRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_ENTRY].next; rcnt++) {
		add_id (&sid, CIFF90ENTRY(*spp++)->symid);
	    }

	spp = patbl[CIF_F90_LABEL].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_LABEL].next; rcnt++)
		add_id (&sid, CIFF90LABEL(*spp++)->symid);

	spp = patbl[CIF_F90_NAMELIST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_NAMELIST].next; rcnt++)
		add_id (&sid, CIFF90NL(*spp++)->symid);

	spp = patbl[CIF_F90_OBJECT].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_OBJECT].next; rcnt++) {
/*
		add_id (&sid, CIFF90OBJ(*spp)->storageid);
*/
		add_id (&sid, CIFF90OBJ(*spp++)->symid);
	    }

	spp = patbl[CIF_F90_BEGIN_SCOPE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_BEGIN_SCOPE].next; rcnt++) {
		add_id (&sid, CIFF90BS(*spp++)->scopeid);
		/* add_id (&sid, CIFF90BS(*spp++)->symid); */
	    }

	spp = patbl[CIF_F90_INT_BLOCK].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_INT_BLOCK].next; rcnt++) {
		add_id (&sid, CIFF90IB(*spp)->intid);
		for (i = 0; i < (int) CIFF90IB(*spp)->numints; i++)
			add_id (&sid, CIFF90IB(*spp)->procids[i]);
		spp++;
	}

	spp = patbl[CIF_F90_RENAME].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_RENAME].next; rcnt++) {
		add_id (&sid, CIFF90RN(*spp)->nameid);
		spp++;
	}

	spp = patbl[CIF_GEOMETRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_GEOMETRY].next; rcnt++)
		add_id (&sid, CIFGEOM(*spp++)->geomid);

	spp = patbl[CIF_F90_DERIVED_TYPE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_DERIVED_TYPE].next; rcnt++) {
	    add_id (&sid, CIFF90DTYPE(*spp++)->symid);
	}


	/* Sort the accumulated ids, then go thru and replace all the ids. */

	(void) qsort ((char *)sid.tbl, sid.cur, sizeof(long), (int(*)()) comp_ids);

	spp = patbl[CIF_CALLSITE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CALLSITE].next; rcnt++) {
		sp = *spp++;
		CIFCS(sp)->entryid = get_id (&sid, CIFCS(sp)->entryid);
#ifdef REMAP_FID
		CIFCS(sp)->fid = get_id (&fid, CIFCS(sp)->fid);
#endif
		for (i = 0; i < (int) CIFCS(sp)->nargs; i++)
			CIFCS(sp)->argids[i] = get_id (&sid, CIFCS(sp)->argids[i]);
	}

	spp = patbl[CIF_COMBLK].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_COMBLK].next; rcnt++) {
		sp = *spp++;
		CIFCB(sp)->symid = get_id (&sid, CIFCB(sp)->symid);
	}

	spp = patbl[CIF_CONST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CONST].next; rcnt++) {
		sp = *spp++;
		CIFCON(sp)->symid = get_id (&sid, CIFCON(sp)->symid);
	}

	spp = patbl[CIF_ENTRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_ENTRY].next; rcnt++) {
		sp = *spp++;
		CIFENTRY(sp)->symid = get_id (&sid, CIFENTRY(sp)->symid);
		for (i = 0; i < (int) CIFENTRY(sp)->nargs; i++)
			CIFENTRY(sp)->argids[i] = get_id (&sid, CIFENTRY(sp)->argids[i]);
	}

	spp = patbl[CIF_LABEL].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_LABEL].next; rcnt++) {
		sp = *spp++;
		CIFLABEL(sp)->symid = get_id (&sid, CIFLABEL(sp)->symid);
	}

	spp = patbl[CIF_LOOP].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_LOOP].next; rcnt++) {
		sp = *spp++;
#ifdef REMAP_FID
		CIFLOOP(sp)->sfid = get_id (&fid, CIFLOOP(sp)->sfid);
		CIFLOOP(sp)->efid = get_id (&fid, CIFLOOP(sp)->efid);
#endif
		CIFLOOP(sp)->labelid = get_id (&sid, CIFLOOP(sp)->labelid);
		CIFLOOP(sp)->symid = get_id (&sid, CIFLOOP(sp)->symid);
	}

#ifdef REMAP_FID
	spp = patbl[CIF_MESSAGE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_MESSAGE].next; rcnt++) {
		sp = *spp++;
		CIFMSG(sp)->fid = get_id (&fid, CIFMSG(sp)->fid);
		CIFMSG(sp)->pfid = get_id (&fid, CIFMSG(sp)->pfid);
	}
#endif

	spp = patbl[CIF_NAMELIST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_NAMELIST].next; rcnt++) {
		sp = *spp++;
		CIFNL(sp)->symid = get_id (&sid, CIFNL(sp)->symid);
		for (i = 0; i < (int) CIFNL(sp)->nids; i++)
			CIFNL(sp)->ids[i] = get_id (&sid, CIFNL(sp)->ids[i]);
	}

	spp = patbl[CIF_OBJECT].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_OBJECT].next; rcnt++) {
		sp = *spp++;
		CIFOBJ(sp)->symid = get_id (&sid, CIFOBJ(sp)->symid);
		CIFOBJ(sp)->geomid = get_id (&sid, CIFOBJ(sp)->geomid);
		CIFOBJ(sp)->pointer = get_id (&sid, CIFOBJ(sp)->pointer);
		if (CIFOBJ(sp)->symclass == CIF_SC_COMMON || CIFOBJ(sp)->symclass ==
		    CIF_SC_EQUIV)
		{
			CIFOBJ(sp)->storage = get_id (&sid, CIFOBJ(sp)->storage);
		}
	}

#ifdef REMAP_FID
	spp = patbl[CIF_STMT_TYPE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_STMT_TYPE].next; rcnt++) {
		sp = *spp++;
		CIFSTMT(sp)->fid = get_id (&fid, CIFSTMT(sp)->fid);
                if (CIFSTMT(sp)->efid > 0) {
		  CIFSTMT(sp)->efid = get_id (&fid, CIFSTMT(sp)->efid);
                }
	}

	spp = patbl[CIF_UNIT].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_UNIT].next; rcnt++) {
		sp = *spp++;
		CIFUNIT(sp)->fid = get_id (&fid, CIFUNIT(sp)->fid);
	}

	spp = patbl[CIF_ENDUNIT].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_ENDUNIT].next; rcnt++) {
		sp = *spp++;
		CIFENDU(sp)->fid = get_id (&fid, CIFENDU(sp)->fid);
	}
#endif

	spp = patbl[CIF_USAGE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_USAGE].next; rcnt++) {
		struct Cif_use *use;
		sp = *spp++;
		CIFUSAGE(sp)->symid = get_id (&sid, CIFUSAGE(sp)->symid);
		for (i = 0; i < (int) CIFUSAGE(sp)->nmembs; i++) {
		  CIFUSAGE(sp)->membs[i] = get_id (&sid, CIFUSAGE(sp)->membs[i]);
		}
#ifdef REMAP_FID
		use = CIFUSAGE(sp)->use;
		for (i = 0; i < (int) CIFUSAGE(sp)->nuses; i++) {
			use->fid = get_id (&fid, use->fid);
			use++;
		}
#endif
	}




	spp = patbl[CIF_CDIR].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CDIR].next; rcnt++) {
		sp = *spp++;
#ifdef REMAP_FID
		CIFCDIR(sp)->fid = get_id (&fid, CIFCDIR(sp)->fid);
#endif
		for (i = 0; i < (int) CIFCDIR(sp)->nids; i++) {
		  CIFCDIR(sp)->ids[i] = get_id (&sid, CIFCDIR(sp)->ids[i]);
		}
	}

	spp = patbl[CIF_CDIR_DOSHARED].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CDIR_DOSHARED].next; rcnt++) {
		sp = *spp++;
#ifdef REMAP_FID
		CIFCDIRDO(sp)->fid = get_id (&fid, CIFCDIRDO(sp)->fid);
		CIFCDIRDO(sp)->mfid = get_id (&fid, CIFCDIRDO(sp)->mfid);
#endif
		for (i = 0; i < (int) CIFCDIRDO(sp)->nids; i++) {
		  CIFCDIRDO(sp)->ids[i] = get_id (&sid, CIFCDIRDO(sp)->ids[i]);
		}
	}


	spp = patbl[CIF_GEOMETRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_GEOMETRY].next; rcnt++) {
	  	struct Cif_geometry_dim *dim;
		sp = *spp++;
		CIFGEOM(sp)->geomid = get_id (&sid, CIFGEOM(sp)->geomid);
#ifdef REMAP_FID
		dim = CIFGEOM(sp)->dim;
		for (i = 0; i < (int) CIFGEOM(sp)->ndims; i++) {
		  	dim->wfid = get_id (&fid, dim->wfid);
		  	dim->bfid = get_id (&fid, dim->bfid);
			dim++;
		}
#endif
	}

#ifdef REMAP_FID
	spp = patbl[CIF_CONTINUATION].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CONTINUATION].next; rcnt++) {
	  	sp = *spp++;
		CIFCONT(sp)->fid = get_id (&fid, CIFCONT(sp)->fid);
	}

	spp = patbl[CIF_TRANSFORM].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_TRANSFORM].next; rcnt++) {
	  	sp = *spp++;
		CIFTRAN(sp)->fid = get_id (&fid, CIFTRAN(sp)->fid);
	}
#endif


	spp = patbl[CIF_F90_CALLSITE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_CALLSITE].next; rcnt++) {
		sp = *spp++;
		CIFF90CS(sp)->entryid = get_id (&sid, CIFF90CS(sp)->entryid);
#ifdef REMAP_FID
		CIFF90CS(sp)->fid = get_id (&fid, CIFF90CS(sp)->fid);
#endif
		CIFF90CS(sp)->procid = get_id(&sid, CIFF90CS(sp)->procid);
		CIFF90CS(sp)->scopeid = get_id(&sid, CIFF90CS(sp)->scopeid);
		for (i = 0; i < (int) CIFF90CS(sp)->nargs; i++) {
			CIFF90CS(sp)->argids[i] = get_id (&sid, CIFF90CS(sp)->argids[i]);

			for (j = 0; j < CIFF90CS(sp)->nmembs[i]; j++) {

			    CIFF90CS(sp)->membs[i][j] =
				get_id (&sid, CIFF90CS(sp)->membs[i][j]);
			}
		}
	}

	spp = patbl[CIF_F90_COMBLK].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_COMBLK].next; rcnt++) {
		sp = *spp++;
		CIFF90CB(sp)->symid = get_id (&sid, CIFF90CB(sp)->symid);
		CIFF90CB(sp)->moduleid = get_id (&sid, CIFF90CB(sp)->moduleid);
		CIFF90CB(sp)->scopeid = get_id (&sid, CIFF90CB(sp)->scopeid);
	}

	spp = patbl[CIF_F90_CONST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_CONST].next; rcnt++) {
		sp = *spp++;
		CIFF90CON(sp)->symid = get_id (&sid, CIFF90CON(sp)->symid);
#ifdef REMAP_FID
		CIFF90CON(sp)->fid = get_id (&fid, CIFF90CON(sp)->fid);
#endif
		CIFF90CON(sp)->scopeid = get_id (&sid, CIFF90CON(sp)->scopeid);
	}

	spp = patbl[CIF_F90_ENTRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_ENTRY].next; rcnt++) {
		sp = *spp++;

		CIFF90ENTRY(sp)->symid = get_id (&sid, CIFF90ENTRY(sp)->symid);
		CIFF90ENTRY(sp)->scopeid = get_id (&sid, CIFF90ENTRY(sp)->scopeid);
		CIFF90ENTRY(sp)->moduleid = get_id (&sid, CIFF90ENTRY(sp)->moduleid);
		CIFF90ENTRY(sp)->resultid = get_id (&sid, CIFF90ENTRY(sp)->resultid);
		for (i = 0; i < (int) CIFF90ENTRY(sp)->nargs; i++)
			CIFF90ENTRY(sp)->argids[i] = get_id (&sid, CIFF90ENTRY(sp)->argids[i]);
	}

	spp = patbl[CIF_F90_LABEL].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_LABEL].next; rcnt++) {
		sp = *spp++;
		CIFF90LABEL(sp)->symid = get_id (&sid, CIFF90LABEL(sp)->symid);
		CIFF90LABEL(sp)->scopeid = get_id (&sid, CIFF90LABEL(sp)->scopeid);
	}

	spp = patbl[CIF_F90_LOOP].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_LOOP].next; rcnt++) {
		sp = *spp++;
#ifdef REMAP_FID
		CIFF90LOOP(sp)->sfid = get_id (&fid, CIFF90LOOP(sp)->sfid);
		CIFF90LOOP(sp)->efid = get_id (&fid, CIFF90LOOP(sp)->efid);
#endif
		CIFF90LOOP(sp)->labelid = get_id (&sid, CIFF90LOOP(sp)->labelid);
		CIFF90LOOP(sp)->symid = get_id (&sid, CIFF90LOOP(sp)->symid);
		CIFF90LOOP(sp)->scopeid = get_id (&sid, CIFF90LOOP(sp)->scopeid);
		CIFF90LOOP(sp)->nameid = get_id (&sid, CIFF90LOOP(sp)->nameid);
	}

	spp = patbl[CIF_F90_NAMELIST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_NAMELIST].next; rcnt++) {
		sp = *spp++;
		CIFF90NL(sp)->symid = get_id (&sid, CIFF90NL(sp)->symid);
		CIFF90NL(sp)->scopeid = get_id (&sid, CIFF90NL(sp)->scopeid);
		CIFF90NL(sp)->moduleid = get_id (&sid, CIFF90NL(sp)->moduleid);
		for (i = 0; i < (int) CIFF90NL(sp)->nids; i++)
			CIFF90NL(sp)->ids[i] = get_id (&sid, CIFF90NL(sp)->ids[i]);
	}

	spp = patbl[CIF_F90_OBJECT].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_OBJECT].next; rcnt++) {
		sp = *spp++;

		CIFF90OBJ(sp)->symid = get_id (&sid, CIFF90OBJ(sp)->symid);
		CIFF90OBJ(sp)->scopeid = get_id (&sid, CIFF90OBJ(sp)->scopeid);

		if (CIFF90OBJ(sp)->symclass == CIF_F90_SC_COMMON ||
		    CIFF90OBJ(sp)->symclass == CIF_F90_SC_MODULE ||
		    CIFF90OBJ(sp)->symclass == CIF_F90_SC_EQUIV ||
		    CIFF90OBJ(sp)->symclass == CIF_F90_SC_NAMED_CONST)

		    CIFF90OBJ(sp)->storageid =
			get_id (&sid, CIFF90OBJ(sp)->storageid);

		CIFF90OBJ(sp)->geomid =
		    get_id (&sid, CIFF90OBJ(sp)->geomid);
		CIFF90OBJ(sp)->pointerid =
		    get_id (&sid, CIFF90OBJ(sp)->pointerid);
	    }

	spp = patbl[CIF_F90_DERIVED_TYPE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_DERIVED_TYPE].next; rcnt++) {
		sp = *spp++;
		CIFF90DTYPE(sp)->symid = get_id (&sid, CIFF90DTYPE(sp)->symid);
		CIFF90DTYPE(sp)->scopeid = get_id (&sid, CIFF90DTYPE(sp)->scopeid);
		CIFF90DTYPE(sp)->moduleid = get_id (&sid, CIFF90DTYPE(sp)->moduleid);
		for (i = 0; i < (int) CIFF90DTYPE(sp)->nmembs; i++)
			CIFF90DTYPE(sp)->memids[i] = get_id (&sid, CIFF90DTYPE(sp)->memids[i]);
	}

	spp = patbl[CIF_F90_BEGIN_SCOPE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_BEGIN_SCOPE].next; rcnt++) {
		sp = *spp++;
		CIFF90BS(sp)->symid = get_id (&sid, CIFF90BS(sp)->symid);
		CIFF90BS(sp)->scopeid = get_id (&sid, CIFF90BS(sp)->scopeid);
		CIFF90BS(sp)->parentid = get_id (&sid, CIFF90BS(sp)->parentid);
#ifdef REMAP_FID
		CIFF90BS(sp)->fid = get_id (&fid, CIFF90BS(sp)->fid);
#endif
	}

	spp = patbl[CIF_F90_END_SCOPE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_END_SCOPE].next; rcnt++) {
		sp = *spp++;
		CIFF90ES(sp)->scopeid = get_id (&sid, CIFF90ES(sp)->scopeid);
#ifdef REMAP_FID
		CIFF90ES(sp)->fid = get_id (&fid, CIFF90ES(sp)->fid);
#endif
	}

	spp = patbl[CIF_F90_SCOPE_INFO].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_SCOPE_INFO].next; rcnt++) {
		sp = *spp++;
		CIFF90SI(sp)->scopeid = get_id (&sid, CIFF90SI(sp)->scopeid);
		for (i = 0; i < (int) CIFF90SI(sp)->numalts; i++)
		  	CIFF90SI(sp)->entryids[i] = get_id (&sid, CIFF90SI(sp)->entryids[i]);
	}

	spp = patbl[CIF_F90_USE_MODULE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_USE_MODULE].next; rcnt++) {
		sp = *spp++;
		CIFF90USE(sp)->modid = get_id (&sid, CIFF90USE(sp)->modid);
#ifdef REMAP_FID
		CIFF90USE(sp)->modfid = get_id (&fid, CIFF90USE(sp)->modfid);
#endif
	}

	spp = patbl[CIF_F90_RENAME].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_RENAME].next; rcnt++) {
		sp = *spp++;
		CIFF90RN(sp)->modid = get_id (&sid, CIFF90RN(sp)->modid);
		CIFF90RN(sp)->origmodid = get_id (&sid, CIFF90RN(sp)->origmodid);
		CIFF90RN(sp)->nameid = get_id (&sid, CIFF90RN(sp)->nameid);
		CIFF90RN(sp)->scopeid = get_id (&sid, CIFF90RN(sp)->scopeid);

		for (i = 0; i < (int) CIFF90RN(sp)->nlocalids; i++)
		    CIFF90RN(sp)->localid[i] =
			get_id (&sid, CIFF90RN(sp)->localid[i]);
	}

	spp = patbl[CIF_F90_INT_BLOCK].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_INT_BLOCK].next; rcnt++) {
		sp = *spp++;
		CIFF90IB(sp)->intid = get_id (&sid, CIFF90IB(sp)->intid);
		CIFF90IB(sp)->moduleid = get_id (&sid, CIFF90IB(sp)->moduleid);
		CIFF90IB(sp)->scopeid = get_id (&sid, CIFF90IB(sp)->scopeid);
		for (i = 0; i < (int) CIFF90IB(sp)->numints; i++)
			CIFF90IB(sp)->procids[i] = get_id (&sid, CIFF90IB(sp)->procids[i]);
	}


	spp = patbl[CIF_C_TAG].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_TAG].next; rcnt++) {
		sp = *spp++;
		CIFCTAG(sp)->tagid = get_id (&sid, CIFCTAG(sp)->tagid);
		for (i = 0; i < (int) CIFCTAG(sp)->nmems; i++)
			CIFCTAG(sp)->memids[i] = get_id (&sid, CIFCTAG(sp)->memids[i]);
	}

#ifdef REMAP_FID
	spp = patbl[CIF_C_MESSAGE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_MESSAGE].next; rcnt++) {
		sp = *spp++;
		CIFCMSG(sp)->fid = get_id (&fid, CIFCMSG(sp)->fid);
		CIFCMSG(sp)->incid = get_id (&fid, CIFCMSG(sp)->incid);
	}
#endif

	spp = patbl[CIF_C_CONST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_CONST].next; rcnt++) {
		sp = *spp++;
		CIFCCON(sp)->symid = get_id (&sid, CIFCCON(sp)->symid);
	}


	spp = patbl[CIF_C_ENTRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_ENTRY].next; rcnt++) {
		sp = *spp++;

		CIFCENTRY(sp)->symid = get_id (&sid, CIFCENTRY(sp)->symid);
		CIFCENTRY(sp)->tagid = get_id (&sid, CIFCENTRY(sp)->tagid);
		CIFCENTRY(sp)->link = get_id (&sid, CIFCENTRY(sp)->link);

		for (i = 0; i < (int) CIFCENTRY(sp)->nargs; i++) {
		  CIFCENTRY(sp)->argids[i] = get_id (&sid, CIFCENTRY(sp)->argids[i]);
		}

	}


	spp = patbl[CIF_C_OBJECT].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_OBJECT].next; rcnt++) {
		sp = *spp++;
		CIFCOBJ(sp)->symid = get_id (&sid, CIFCOBJ(sp)->symid);
		CIFCOBJ(sp)->psymid = get_id (&sid, CIFCOBJ(sp)->psymid);
		CIFCOBJ(sp)->tagid = get_id (&sid, CIFCOBJ(sp)->tagid);
		CIFCOBJ(sp)->link = get_id (&sid, CIFCOBJ(sp)->link);
	}


	spp = patbl[CIF_C_LINT_DIRECTIVE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_LINT_DIRECTIVE].next; rcnt++) {
		sp = *spp++;
		CIFCLDIR(sp)->objid = get_id (&sid, CIFCLDIR(sp)->objid);
#ifdef REMAP_FID
		CIFCLDIR(sp)->fid = get_id (&fid, CIFCLDIR(sp)->fid);
#endif
	}

	spp = patbl[CIF_C_MACRO_DEF].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_MACRO_DEF].next; rcnt++) {
		sp = *spp++;
		CIFCMDEF(sp)->symid = get_id (&sid, CIFCMDEF(sp)->symid);
#ifdef REMAP_FID
		CIFCMDEF(sp)->fid = get_id (&fid, CIFCMDEF(sp)->fid);
#endif
	}

	spp = patbl[CIF_C_MACRO_UNDEF].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_MACRO_UNDEF].next; rcnt++) {
		sp = *spp++;
		CIFCMUDEF(sp)->symid = get_id (&sid, CIFCMUDEF(sp)->symid);
#ifdef REMAP_FID
		CIFCMUDEF(sp)->fid = get_id (&fid, CIFCMUDEF(sp)->fid);
#endif
	}



	spp = patbl[CIF_C_MACRO_USAGE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_MACRO_USAGE].next; rcnt++) {
		sp = *spp++;
		CIFCMUSE(sp)->symid = get_id (&sid, CIFCMUSE(sp)->symid);
#ifdef REMAP_FID
		CIFCMUSE(sp)->fid = get_id (&fid, CIFCMUSE(sp)->fid);
#endif
	}

	spp = patbl[CIF_C_ENTRY_END].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_ENTRY_END].next; rcnt++) {
		sp = *spp++;
		CIFCEEND(sp)->symid = get_id (&sid, CIFCEEND(sp)->symid);
#ifdef REMAP_FID
		CIFCEEND(sp)->fid = get_id (&fid, CIFCEEND(sp)->fid);
#endif
	}


	spp = patbl[CIF_CC_TYPE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CC_TYPE].next; rcnt++) {
	    sp = *spp++;
	    CIFCCTYPE(sp)->symid = get_id (&sid, CIFCCTYPE(sp)->symid);
            CIFCCTYPE(sp)->scopeid = get_id (&sid, CIFCCTYPE(sp)->scopeid);
	    j = CIFCCTYPE(sp)->nmem;
	    for (i = 0; i < j; i++ ) {
		CIFCCTYPE(sp)->mem[i] = get_id (&sid, CIFCCTYPE(sp)->mem[i]);
	    }
	}

	spp = patbl[CIF_CC_ENTRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CC_ENTRY].next; rcnt++) {
	    sp = *spp++;
	    CIFCCENT(sp)->symid = get_id (&sid, CIFCCENT(sp)->symid);
	    CIFCCENT(sp)->fsymid = get_id (&sid, CIFCCENT(sp)->fsymid);
            CIFCCENT(sp)->scopeid = get_id (&sid, CIFCCENT(sp)->scopeid);
#ifdef REMAP_FID
	    CIFCCENT(sp)->sfid = get_id (&fid, CIFCCENT(sp)->sfid);
	    CIFCCENT(sp)->efid = get_id (&fid, CIFCCENT(sp)->efid);
#endif
	    j = CIFCCENT(sp)->nparam;
	    for (i = 0; i < j; i++ ) {
		CIFCCENT(sp)->param[i] = get_id (&sid, CIFCCENT(sp)->param[i]);
	    }
	}

	spp = patbl[CIF_CC_OBJ].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CC_OBJ].next; rcnt++) {
	    sp = *spp++;
	    CIFCCOBJ(sp)->symid = get_id (&sid, CIFCCOBJ(sp)->symid);
            CIFCCOBJ(sp)->scopeid = get_id (&sid, CIFCCOBJ(sp)->scopeid);
	}

	spp = patbl[CIF_CC_SUBTYPE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CC_SUBTYPE].next; rcnt++) {
	    sp = *spp++;
	    CIFCCSUB(sp)->symid = get_id (&sid, CIFCCSUB(sp)->symid);
	}

	spp = patbl[CIF_CC_ENUM].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CC_ENUM].next; rcnt++) {
	    sp = *spp++;
	    CIFCCENUM(sp)->symid = get_id (&sid, CIFCCENUM(sp)->symid);
	}

#ifdef REMAP_FID
	spp = patbl[CIF_CC_EXPR].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CC_EXPR].next; rcnt++) {
	    sp = *spp++;
	    CIFCCEXPR(sp)->fid = get_id (&fid, CIFCCEXPR(sp)->fid);
	}
#endif


#ifdef REMAP_FID
	spp = patbl[CIF_BE_NODE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_BE_NODE].next; rcnt++) {
	    sp = *spp++;
	    j = CIFBENODE(sp)->nlines;
	    for (i = 0; i < j; i++ ) {
		CIFBENODE(sp)->fid[i] = get_id (&fid, CIFBENODE(sp)->fid[i]);
	    }
	}

	spp = patbl[CIF_BE_FID].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_BE_FID].next; rcnt++) {
	    sp = *spp++;
	    j = CIFBEFID(sp)->nfid;
	    for (i = 0; i < j; i++ ) {
		CIFBEFID(sp)->fid[i] = get_id (&fid, CIFBEFID(sp)->fid[i]);
	    }
	}
#endif

}


/* --------------------------------------------------------------------------
 * get_max_sid scans the patbl contents for the current unit and determines
 * what the largest symbol id value is.
 * --------------------------------------------------------------------------
 */
static long get_max_sid (void)
{

	struct Cif_generic **spp;	
	int rcnt;
	long sid, maxsid = 0;

	spp = patbl[CIF_COMBLK].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_COMBLK].next; rcnt++) {
		if ((sid = CIFCB(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_CONST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_CONST].next; rcnt++) {
		if ((sid = CIFCON(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_ENTRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_ENTRY].next; rcnt++) {
		if ((sid = CIFENTRY(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_LABEL].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_LABEL].next; rcnt++) {
		if ((sid = CIFLABEL(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_NAMELIST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_NAMELIST].next; rcnt++) {
		if ((sid = CIFNL(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_OBJECT].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_OBJECT].next; rcnt++) {
		if ((sid = CIFOBJ(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_F90_COMBLK].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_COMBLK].next; rcnt++) {
		if ((sid = CIFF90CB(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_F90_CONST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_CONST].next; rcnt++) {
		if ((sid = CIFF90CON(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_F90_ENTRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_ENTRY].next; rcnt++) {
		if ((sid = CIFF90ENTRY(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_F90_LABEL].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_LABEL].next; rcnt++) {
		if ((sid = CIFF90LABEL(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_F90_NAMELIST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_NAMELIST].next; rcnt++) {
		if ((sid = CIFF90NL(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_F90_OBJECT].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_OBJECT].next; rcnt++) {
		if ((sid = CIFF90OBJ(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_F90_BEGIN_SCOPE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_F90_BEGIN_SCOPE].next; rcnt++) {
		if ((sid = CIFF90BS(*spp++)->scopeid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_GEOMETRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_GEOMETRY].next; rcnt++) {
		if ((sid = CIFGEOM(*spp++)->geomid) > maxsid)
			maxsid = sid;
	}


	spp = patbl[CIF_C_CONST].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_CONST].next; rcnt++) {
		if ((sid = CIFCCON(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_C_ENTRY].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_ENTRY].next; rcnt++) {
		if ((sid = CIFCENTRY(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_C_OBJECT].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_OBJECT].next; rcnt++) {
		if ((sid = CIFCOBJ(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_C_LINT_DIRECTIVE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_LINT_DIRECTIVE].next; rcnt++) {
		if ((sid = CIFCLDIR(*spp++)->objid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_C_MACRO_DEF].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_MACRO_DEF].next; rcnt++) {
		if ((sid = CIFCMDEF(*spp++)->symid) > maxsid)
			maxsid = sid;
	}

	spp = patbl[CIF_C_TAG].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_C_TAG].next; rcnt++) {
		if ((sid = CIFCTAG(*spp++)->tagid) > maxsid)
			maxsid = sid;
	}

	return (maxsid);
}

#ifdef REMAP_FID
/* --------------------------------------------------------------------------
 * remap_files scans the patbl contents for the all non-unit records that
 * contain file id values.  remap_files collects all the file ids, sorts 'em,
 * and then changes the file ids to new values.
 * --------------------------------------------------------------------------
 */
static void remap_files (
	void)
{
	struct Cif_generic *sp, **spp;
	int rcnt;

	init_id (&fid);

	spp = patbl[CIF_FILE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_FILE].next; rcnt++) {
		add_id (&fid, CIFFILE(*spp++)->fid);
	}

	(void) qsort ((char *)fid.tbl, fid.cur, sizeof(long), (int(*)()) comp_ids);

	spp = patbl[CIF_FILE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_FILE].next; rcnt++) {
		sp = *spp++;
		CIFFILE(sp)->fid = get_id (&fid, CIFFILE(sp)->fid);
	}

	spp = patbl[CIF_INCLUDE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_INCLUDE].next; rcnt++) {
		sp = *spp++;
		CIFINC(sp)->srcid = get_id (&fid, CIFINC(sp)->srcid);
		CIFINC(sp)->incid = get_id (&fid, CIFINC(sp)->incid);
	}

	spp = patbl[CIF_SRCFILE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_SRCFILE].next; rcnt++) {
		sp = *spp++;
		CIFSRC(sp)->fid = get_id (&fid, CIFSRC(sp)->fid);
		srcfid = CIFSRC(sp)->fid;
	}

	spp = patbl[CIF_SRC_POS].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_SRC_POS].next; rcnt++) {
		sp = *spp++;
		CIFSPOS(sp)->srcid  = get_id (&fid, CIFSPOS(sp)->srcid);
		CIFSPOS(sp)->psrcid = get_id (&fid, CIFSPOS(sp)->psrcid);
		CIFSPOS(sp)->fid    = get_id (&fid, CIFSPOS(sp)->fid);
	}
}
#endif

/* --------------------------------------------------------------------------
 * get_max_fid scans the patbl contents for the file records and locates
 * the largest id value.
 * --------------------------------------------------------------------------
 */
static long get_max_fid (
	void)
{

	struct Cif_generic **spp;
	int rcnt;
	long fid, maxfid = 0;

	spp = patbl[CIF_FILE].aptr;
	for (rcnt = 0; rcnt < patbl[CIF_FILE].next; rcnt++) {
		if ((fid = CIFFILE(*spp++)->fid) > maxfid)
			maxfid = fid;
	}

	return (maxfid);
}



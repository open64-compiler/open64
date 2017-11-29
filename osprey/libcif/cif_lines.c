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


#pragma ident "@(#) libcif/cif_lines.c	30.12	08/26/97 07:43:58"


/*
 * cif_lines converts a cif into "lines format" as described below :
 *
 * Summary :
 *
 * 	cifhdr
 * 	srcfile
 * 	files
 * 	misc-non-unit-based-records
 * 	unit
 * 	  begin_scope
 * 	    non-line-numbered-records referenced in this scope block
 * 	    line numbered records sorted by line number
 * 	    begin_scope
 * 		...
 * 	    end_scope
 * 	    ...
 * 	  end_scope
 *           ...
 * 	end_unit
 * 	...
 * 	summary
 *
 *
 * 1. First record will always be cifhdr.
 * 	it will have the following fields set as indicated
 *
 * 	form = 1(binary)   bintype = 1(lines)
 *
 * 2. Second record will be the srcfile record
 *
 * 3. Next set of records will be the file records in order of fid.
 *
 * 4. Before the first unit record will be miscellaneous records that
 * 	do not belong to any particular unit; f90_misc_opts,
 * 	include records, machine characteristics, optimization options;
 * 	messages (that do not belong to a unit)....
 *
 * 5. Following these will be begin-unit { unit records } end-unit groupings
 *
 * 6. The last cif record will be the summary record.
 *
 * 7. All records associated with a unit will be contained within the
 * 	begin-unit, end-unit pair.
 * 
 * 8. As applicable (ie for F90), each unit will be split into nested
 * 	begin_scope, end_scope blocks in the same (line) order as
 * 	in the original source.
 * 
 * 9. All records associated with a particular scope will be contained
 * 	within the begin_scope, end_scope pair.
 *
 * 10. Immediately following a begin_scope will be the f90_entry record
 * 	of the same symid as the begin scope.
 *
 * [When no scope-blocks are defined (eg CF77), the entry record will
 * immediately follow the unit record].
 *
 * 11. The grouping within a begin-scope/end-scope block will be as follows :
 *
 * 	begin_scope
 * 	  f90_entry matching this begin_scope (see 10. for the exception)
 * 	  other f90_entries belonging to this scope sorted by symid
 * 	  all other records which have no specific line number, sorted
 * 		on record type and then symid.
 * 	  records with line numbers sorted on line and column position followed
 * 		by record type.
 * 	end_scope.
 *
 * 12. All of the above will be true for ascii cif's and binary (cifconv'd)
 * 	cif's alike, except that cifconv'd cif's will have the additional
 * 	filedir and unitdir records which will have non-meaningful positions
 * 	(ie they still provide useful data in terms of obtaining the number
 * 	of units present, but can not be used to position from in the file.
 * 	The filedir will appear before the first unit, and a unitdir will be
 * 	present in each begin-unit, end-unit block.
 *
 * 	Problem : cifconv goes to great lengths to coalesce usage records;
 * 	which lines does not break apart. This will break the strictly
 * 	line order natures of the lines output. eg you might see
 *
 * 	stmt record at line 10 cpos 7
 * 	usage record for xyz at line 10 cpos 8
 * 		                line 15 cpos 10
 * 				line 123 cpos 15
 * 	stmt record at line 11 cpos 7
 * 	usage record for abc at line 11 cpos 9
 * 				etc
 *
 * 	where the usages are sorted on the first usage line, but as all are
 * 	grouped into one record, the subsequent usages will be out of order.
 *
 * ------------------------------------------------------------------------- */

#ifndef __STDC__
#	define const
#endif

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "cif_int.h"			/* CIF field name arrays */
#include "unitrecord.h"			/* table indicating if record in unit or not */

#define CIF_NOT 	0
#define CIF_ASCII 	1
#define CIF_BINARY 	2

struct unit_list {   		/* record info array */
    struct Cif_generic *rptr;  	/* record pointer array */
    int recno;		       	/* number of record */
    long filepos;	       	/* file position */
};

struct record {
    struct unit_list *ul;
    int ulcur;		       /* next slot in unit_list */
    int ulmax;		       /* max size of unit_list */
};

static struct record ul;
static struct record wl;
static struct record nl;

/* dynmic array to store modid's and there respective direct/indirect
 * flag. When f90_entries are being processed, when a module is hit. this
 * list is scanned to append the f90_entry with a new bit that says if the
 * module was imported directly or indirectly
 */
struct mod_struct {
    int modid;
    int direct;
};

static struct mod_struct *modids = (struct mod_struct *) NULL;
static int modid_max = 0;
static int modid_current = 0;
#define MODID_BUMP 10


#undef Cif_Lines  /* ensure that we don't try to map this to Cif_Cifconv_Vx */

/* --- forward reference prototypes --- */
static void save_record (struct record *, struct Cif_generic *, int, long);
static void print_records (struct record *, struct record *);
static void print_header_records (struct record *);
static int get_id (struct Cif_generic *);
static int get_line (struct Cif_generic *);
static int get_cpos (struct Cif_generic *);
static int get_fid (struct Cif_generic *);
static int get_scope (struct Cif_generic *);
static int get_type (struct Cif_generic *);
static int get_adjusted_scope (struct Cif_generic *);

static int outfd;
static char *outfile = "-";

static int global_srcfid = 0;

static int global_last_scope = -1;

/*
 * This will be set if any begin_scopes exist in the cif which dictates
 * how print_records prints the records out in lines format
 * ie if scopes, then we want to group records by scope, otherwise, by
 * unit-end unit only.
 */
static int global_scope_found = 0;


/* global_cif_status is set according to the flag in the cif_summary
 * record. >= 0 indicates that the CIF is valid (global_cif_status is
 * set to 0), -1 indicates that 100 errors were found and that the CIF
 * may well be incomplete, but should be okay as far as it goes. -2, and
 * -3 are different internal compiler errors meaning that this CIF should
 * not be trusted. Negative values are propagated into global_cif_status
 */
static int global_cif_status = 0;

/* return the global_cif_status for the last CIF converted with cif_lines */

int Cif_CifStatus() {

	return( global_cif_status );

}

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


/* Cif_Filename returns the filename associated with a given cifd */


char *Cif_Filename
#ifdef __STDC__
    	(int cifd)
#else
	(cifd)
int cifd;
#endif
{
    if (cifd < 0 || cifd >= CIF_FT_SIZE ||_Cif_filetbl[cifd].form == NOT_A_CIF)
	return ((char *) NULL);
    else {

	return(_Cif_filetbl[cifd].filename);

    }
}




/*
 * lines_type returns true if this file is in lines format already
 * false otherwise.
 */


static int lines_type
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

      global_srcfid = CIFHDR(cif_record)->srcfid;

      return_code = (CIFHDR(cif_record)->form == CIF_FORM_SORTED);
      return_code &= (CIFHDR(cif_record)->bintype == CIF_FORM_LINES);

      Cif_Close (cifd, CIF_MEM_FREE);
      return(return_code);
  }
  else {
    return(0);
  }
}


static char *cif_concat
#ifdef _STDC__
	( char *str1, char *str2 )
#else
	( str1, str2 )
char *str1, *str2;
#endif
{
  char *return_str;

  return_str =
      (char *) malloc ( (strlen( str1) + strlen(str2) + 1) * sizeof (char));

  if (return_str == (char *) NULL) {
      (void) fprintf(stderr,
	      "libcif, cif_lines error : Couldn't malloc space in cif_concat\n");
      exit(-1);
  }

  (void) sprintf(return_str, "%s%s",str1, str2);

  return( return_str );
}



/*
 * returns the basename of a given filename, ie removing any attached
 * directory path
 */

char *cif_basename
#ifdef __STDC__
	( char *name )
#else
( name )
     char *name;
#endif
{
  char *return_str = strrchr(name, '/');

  if (return_str == (char *) NULL)
    return( name );
  else
    return( (char *) (++return_str));
}



/*
 * Returns the directory part of a fully pathed file
 */

static char *cif_dirname
#ifdef __STDC__
	( char *name )
#else
( name )
     char *name;
#endif
{
  int i = strlen( name ) - 1;
  char *dirname_tmp;

  while ( i >= 0 &&
	  name[i]!='/')
    i--;

  if ( i > 0 ) {
    dirname_tmp = (char *) malloc ( (i+1) * sizeof(char));
    if (!dirname_tmp) {
      (void) fprintf(stderr,
	      "libcif, Cif_Lines error : Couldn't malloc space in cif_dirname\n");
      exit(-1);
    }
    (void) strncpy(dirname_tmp, name, i);
    dirname_tmp[i] = '\0';
  }
  else
    dirname_tmp = strdup("./");

  return(dirname_tmp);
}


int cif_VerifyCanWrite
#ifdef __STDC__
	( char *file )
#else
	(file)
char *file;
#endif
{
    if (!access(file,F_OK)) {
	if(access(file,W_OK)) {
	    /* This file exists but can not be written to */
	    return(0);
	}
	else {
	    return(1);
	}
    }
    else {
	char *dir = cif_dirname(file);
	struct stat buf;
	int mode;
	char *test_file = cif_concat(dir,"/write_test");
	FILE *fd;

	if (access(dir,F_OK)) {
	    /* Directory does not exist */
	    (void) free(dir);
	    (void) free(test_file);
	    return(0);
	}

	mode = stat(dir, &buf);

	if (mode == -1) {
	    /* Directory can not be accessed */
	    return(0);
	}

	if (S_ISDIR(buf.st_mode)) {
	    if (NULL == (fd = fopen(test_file,"w"))) {
		/* Directory is not writeable" */
		(void) free(dir);
		(void) fclose(fd);
		(void) unlink(test_file);
		(void) free(test_file);
		return(0);
	    }
	    else {
		(void) free(dir);
		(void) fclose(fd);
		(void) unlink(test_file);
		(void) free(test_file);
		return(1);
	    }
	}
	else {
	    /* directory exists, but is not a directory */
	    (void) free(test_file);
	    (void) free(dir);
	    return(0);
	}
    }
}





/* --- convert a cif onto a lines mode cif --- */
static char *Cif_Make_Lines
#ifdef __STDC__
	(char *infile, char *outfile)
#else
	(infile, outfile)
char *infile, *outfile;
#endif
{
    	static int first = 1;
	int record_num;
	int rtype;
	int cifd;
	long filepos;
	struct Cif_generic *cif_record;
	int cif_ending_early = 0;

        /* Assume that CIF is valid until the summary record says otherwise */
	global_cif_status = 0;

	/*
	 * Open the input file.  If records are to be sorted,
	 * memory mode must be managed.  Set the memory management mode.
	 */

	if ((cifd = Cif_Open(infile, "r", NULL, CIF_VERSION)) < 0) {
		(void) fprintf (stderr ,"libcif: can't open file %s - %s\n",
			 infile, Cif_Errstring(cifd));
		return((char *) NULL);
	}


	if ((outfd = Cif_Open(outfile, "w", NULL, CIF_VERSION)) < 0) {
	    Cif_Close(cifd, CIF_MEM_FREE);
	    (void) fprintf (stderr,
		     "libcif: can't open output file %s - %s\n",outfile,
		     Cif_Errstring(outfd));
	    return ((char *) NULL);
	}

	(void) Cif_Memmode (cifd, CIF_MEM_MANAGED);

	/*
	 * Read each record.  If not sorted, display immediately.  If sorted
	 * and a non-unit record, display immediately.  If sorted and a
	 * unit record, save the record pointer in unit_list till the
	 * CIF_ENDUNIT record has been reached, then sort and print all the
	 * records.
	 */

	/*
	 * Make sure that memory is reset on the first time through
	 * as soon we will check this value for NULL and allocate
	 * storage based on it, if unitialised, it could cause problems
	 */
	if (first == 1) {
	    ul.ul = (struct unit_list *) NULL;
	    nl.ul = (struct unit_list *) NULL;
	    wl.ul = (struct unit_list *) NULL;
	    first = 0;
	}

	ul.ulcur = 0;	nl.ulcur = 0;	wl.ulcur = 0;

	ul.ulmax = 0;	nl.ulmax = 0;	wl.ulmax = 0;


	record_num = 1;
	filepos = Cif_Getpos(cifd);
	while ((rtype = Cif_Getrecord (cifd, &cif_record)) >= 0) {

	    if (rtype > CIF_MAXRECORD)
		(void) fprintf (stderr, "libcif:    unknown record type, %d\n", rtype);
	    else if (rtype == CIF_SUMMARY &&
		    CIFSUM(cif_record)->fldlen < 0) {
		    global_cif_status = CIFSUM(cif_record)->fldlen;
		}

	    else if (get_scope(cif_record) != 0)
	    {
		/*
		 * If the cif is ending prematurely, a summary record will
		 * be issued containing fldlen == -1; this indicates that
		 * all remaining records should be flushed at the end
		 * as there will not be an end unit record to do this
		 * via normal processing
		 */
		if (rtype == CIF_SUMMARY &&
		    CIFSUM(cif_record)->fldlen < 0) {
		    global_cif_status = CIFSUM(cif_record)->fldlen;
		    cif_ending_early = 1;
		}


		if (rtype == CIF_UNIT) {

		    global_last_scope = -1;
		    modid_current = 0;  /* no use_mod records
					 * found in this unit, yet. Used to
					 * cross reference entries with their
					 * corresponding use_mods
					 */
		}

		if (! unit_record[rtype] &&
		    rtype != CIF_INCLUDE) {
		    save_record(&nl, cif_record, record_num, filepos);
		}
		else {
		    if (has_line[rtype]) {
			save_record (&wl, cif_record, record_num, filepos);

			if (rtype == CIF_F90_BEGIN_SCOPE &&
			    global_last_scope == -1) {
			    global_last_scope = CIFF90BS(cif_record)->scopeid;

			}
		    }
		    else {
			save_record (&ul, cif_record, record_num, filepos);
			/* add the use_module so that module entries can be
			 * looked up to see of they are directly used
			 */
			if (rtype == CIF_F90_USE_MODULE) {
			    /* See if more space is needed */

			    if (modid_max == modid_current) {
				modid_max += MODID_BUMP;
				if (modid_max == MODID_BUMP) {

				    modids = (struct mod_struct *) malloc((sizeof(struct mod_struct) * modid_max));
				}
				else {
				    modids = (struct mod_struct *) realloc(modids, (sizeof(struct mod_struct) * modid_max));
				}
			    }
			    modids[modid_current].modid = CIFF90USE(cif_record)->modid;
			    modids[modid_current].direct = CIFF90USE(cif_record)->direct;
			    modid_current++;

			}
		    }

		    if (rtype == CIF_ENDUNIT) {

			print_header_records (&nl);
			print_records (&wl, &ul);

			ul.ulcur = 0;
			nl.ulcur = 0;
			wl.ulcur = 0;

			(void) Cif_Release (cifd, CIF_MEM_KEEP);
		    }

		    if (rtype == CIF_USAGE)
			record_num += CIFUSAGE(cif_record)->nuses;
		    else
			record_num++;
		}
		filepos = Cif_Getpos(cifd);
	    }
	}

	/* print out any remaining non-unit records, normally the summary */

	/*
	 * If the cif has terminated early (because of > 100 errors), then
	 * we need to flush the remaining errors
	 */
	if (cif_ending_early == 1) {
	    print_records (&wl, &ul);
	}

	if (nl.ulcur > 0)
	    print_header_records (&nl);

	/* All done so clean up */

	if (rtype != CIF_EOF)
	    (void) fprintf (stderr, "CIF error - %s\n",
		     Cif_Errstring(rtype));
	(void) Cif_Close (cifd, CIF_MEM_FREE);
	(void) Cif_Close (outfd, CIF_MEM_KEEP);
	return (outfile);
}


static char *cif_convert_to_lines
#ifdef __STDC__
	(char *filename, int keep, int *tmp_cif)
#else
	(filename, keep, tmp_cif)
char *filename;
int keep;
int *tmp_cif;
#endif
{
    char *value;
    char *cifdir = (char *) NULL;
    char *cifdir_file = (char *) NULL;
    char *outfile = (char *) NULL;
    char *create_cif_file = (char *) NULL;
    char *tmpdir = (char *) NULL;

    /* Assume that a tmp file will not be created, until proved otherwise */

    *tmp_cif = 0;

    /* 1. check if the file is already a lines file */

    if (lines_type( filename ) == 1)
	return(strdup(filename));

    /* 2. see if CIFDIR is set */

    value = getenv("CIFDIR");
    if (value != (char *) NULL) {
	cifdir = value;

	cifdir_file = (char *) malloc(sizeof(char) *
				      (strlen(cifdir) + 
				       strlen(cif_basename(filename)) +
				       3));

	(void) sprintf(cifdir_file, "%s/%sL", cifdir, cif_basename(filename));
	
	if (!access(cifdir_file, R_OK)) {

	    if (later_date(filename, cifdir_file) &&
		lines_type(cifdir_file) == 1) {
		return(cifdir_file);
	    }
	}
    }

    /* 3. See if the lines cif exists next to the original cif */

    outfile = (char *) malloc(sizeof(char) *
			      (strlen(filename) + 2));
    (void) sprintf(outfile, "%sL", filename);

    if (!access(outfile, R_OK)) {

	if (later_date(filename, outfile) &&
	    lines_type(outfile) == 1) {
	    return(outfile);
	}
    }


    /* Lines for does not exist already, so we have to create it */

    /* Only want to create the cif in non-tmp space if keep == True */
    if (keep == 1) {

	/* 4. See if we can write to CIFDIR */

	if (cifdir_file != (char *) NULL &&
	    cif_VerifyCanWrite(cifdir_file)) {

	    create_cif_file = cifdir_file;
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

	if (value != (char *) NULL) (void) free(value);

	value = getenv("TMPDIR");
	if (value != (char *) NULL) {
	    tmpdir = value;
	}
	else {
	    create_cif_file = (char *) malloc(sizeof(char) *
					      (strlen("/tmp/") + 
					       strlen(cif_basename(filename)) +
					       7));

	    (void) sprintf(create_cif_file, "/tmp/%sXXXXXX", cif_basename(filename));
	    (void) mktemp(create_cif_file);

	}

	if (create_cif_file == (char *) NULL) {
	    create_cif_file = (char *) malloc(sizeof(char) *
					      (strlen(tmpdir) + 
					       strlen(cif_basename(filename)) +
					       3));

	    (void) sprintf(create_cif_file, "%s/%sL", tmpdir, cif_basename(filename));
	}

	/*
	 * indicate that a tmp cif file is about to be created; it will
	 * be removed on cif_close
	 */

	*tmp_cif = 1;

    }

    create_cif_file = Cif_Make_Lines(filename, create_cif_file);

    /* free up allocated strings */

    if (cifdir_file != (char *) NULL) (void) free(cifdir_file);
    if (outfile != (char *) NULL) (void) free(outfile);

    return(create_cif_file);
}


int Cif_Lines
#ifdef __STDC__
(char *filename, char *optype, int *rtypes, int version, int keep)
#else
(filename, optype, rtypes, version, keep)
char *filename;			/* file name */
char *optype;				/* open type */
int *rtypes;				/* ptr to array of selected record types */
int version;				/* CIF version expected by tools */
int keep;				/* keep the file on exit ? */
#endif
{

    char *cif_name;
    int tmp_cif;  /*
		   * set by cif_convert_to_lines if a tmp file has been
		   * created to hold the lines cif; the cif should be deleted
		   * by cif_close
		   */
    int ret;

    if (_cif_version == 0)
	_cif_version = 1;

    /* make sure that global values are reset */

    global_scope_found = 0;
    global_srcfid = 0;

    /* convert filename to cif_lines name */

    cif_name = cif_convert_to_lines(filename, keep, &tmp_cif);

    /* an empty filename means that the cif is invalid or not there */
    if (cif_name == (char *) NULL) {
	return(CIF_NOTCIF);
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
     * cif_convert_to_lines always returns a copy of the filename to
     * be opened, but cif_open will be copying this, so we don't need
     * the string anymore
     */

    (void) free(cif_name);
    /*
     * If a valid open, set the tmp_cif flag according to indicate if a
     * temporary file has been created that should be removed on cif_close
     */

    if (ret >= 0) {
	_Cif_filetbl[ret].tmp_cif = tmp_cif;
    }

    /* Return whatever cif_open returned */

    return(ret);
}


/*
 * As above + added a check to see of the cif.h in use matches what this
 * library was compiled with...looks at CIF_SUB_VERSION_2 which must match
 * the sub_version passed. See cif_open macros in cif.h for more details.
 */

int Cif_Lines_V2_1
#ifdef __STDC__
(char *filename, char *optype, int *rtypes, int version, int keep, int sub_version)
#else
(filename, optype, rtypes, version, keep, sub_version)
char *filename;			/* file name */
char *optype;			/* open type */
int *rtypes;			/* ptr to array of selected record types */
int version;			/* CIF version expected by tools */
int keep;			/* keep the file on exit ? */
int sub_version;		/* version number of the cif.h */
#endif
{
    _cif_version = 2;

    if (sub_version != CIF_SUB_VERSION_2)
	return(CIF_SUBVER);

    return(Cif_Lines(filename, optype, rtypes,
		     version, keep));
}

/*
 * As above for Version 3
 */

int Cif_Lines_V3_1
#ifdef __STDC__
(char *filename, char *optype, int *rtypes, int version, int keep, int sub_version)
#else
(filename, optype, rtypes, version, keep, sub_version)
char *filename;			/* file name */
char *optype;			/* open type */
int *rtypes;			/* ptr to array of selected record types */
int version;			/* CIF version expected by tools */
int keep;			/* keep the file on exit ? */
int sub_version;		/* version number of the cif.h */
#endif
{
    _cif_version = 3;

    if (sub_version != CIF_SUB_VERSION_3)
	return(CIF_SUBVER);

    return(Cif_Lines(filename, optype, rtypes,
		     version, keep));
}

/* --------------------------------------------------------------------------
 * Add a record pointer to unit_list.  Create unit_list if not created yet.
 * Expand unit_unit if full.
 * -------------------------------------------------------------------------- */
static void save_record
    (struct record *l,
     struct Cif_generic *cif_record,
     int recno,
     long filepos)
{
	if (l->ul == (struct unit_list *) NULL) {
		l->ulmax = 10000;
		l->ul=
		    (struct unit_list *)
			calloc (l->ulmax, sizeof(struct unit_list));
	}
	else
	    if (l->ulcur >= l->ulmax) {
		l->ulmax += 1000;
		l->ul = (struct unit_list *)
		    realloc (l->ul, sizeof(struct unit_list) * l->ulmax);

		(void) memset((char *) (&(l->ul[l->ulmax - 1000])), '\0',
		       (1000 * sizeof(struct unit_list)));

	    }

	l->ul[l->ulcur].rptr = cif_record;
	l->ul[l->ulcur].recno = recno;
	l->ul[l->ulcur++].filepos = filepos;
}


/* --------------------------------------------------------------------------
 * qsort comparsion routine for comparing symbol ids
 * -------------------------------------------------------------------------- */
static int comp_id (
	struct Cif_generic **r1,
	struct Cif_generic **r2)
{
    int ret;

    if (((ret = (get_fid(*r1) - get_fid(*r2)))) != 0)
	return (ret);
    else
	if (((ret = (get_line(*r1) - get_line(*r2)))) != 0)
	    return (ret);
    else
	if (((ret = ( get_cpos(*r1) - get_cpos(*r2)))) != 0)
	    return(ret);
	else {
	    if (((ret = ((*r1)->rectype - (*r2)->rectype ))) != 0)
		return (ret);
	    else
		if ((*r1)->rectype == CIF_F90_END_SCOPE)

		    /*
		     * note that we want to compare scopes the other way
		     * around, ie if it's the same position/record, we are
		     * looking at two end scopes and as scope x will have
		     * started before x + 1, we want x+1 end to come before x.
		     */

		    return ( get_scope(*r2) - get_scope(*r1) );
		else
		    return ( get_scope(*r1) - get_scope(*r2) );
	}
}


/* --------------------------------------------------------------------------
 * qsort comparsion routine for comparing symbol scopes
 * --------------------------------------------------------------------------*/
static int comp_scope (
	struct Cif_generic **r1,
	struct Cif_generic **r2)
{
    int ret;
    if ((ret = (get_adjusted_scope(*r1) - get_adjusted_scope(*r2))) != 0)
	return(ret);
    else
    if ((ret = (get_type(*r1) - get_type(*r2))) != 0)
	return(ret);
    else
	return ( get_id(*r1) - get_id(*r2) );
}

/* --------------------------------------------------------------------------
 * qsort comparsion routine for comparing record types
 * --------------------------------------------------------------------------*/
static int comp_rtype (
	struct Cif_generic **r1,
	struct Cif_generic **r2)
{
    int rtype_1, rtype_2;

    rtype_1 = (*r1)->rectype;
    rtype_2 = (*r2)->rectype;
    if (rtype_1 == rtype_2 &&
	rtype_2 == CIF_FILE)
	return(CIFFILE(*r1)->fid - CIFFILE(*r2)->fid);
    else
    if (rtype_1 == CIF_SRCFILE)
	rtype_1 = CIF_FILE - 1;   /* want srcfiles before files */
    else
    if (rtype_2 == CIF_SRCFILE)
	rtype_2 = CIF_FILE - 1;

    return ( rtype_1 - rtype_2 );
}


/*
 * If an include record is found, all records belonging to this include
 * file should be listed after it, even though this will seemingly
 * break the natural line number order. eg line 10 of fred.f and line
 * 11 of inc1.h should not be compared, so that an ordered line list
 * is provided for all things in incl1.h immediately after the include
 * line record. Note, this is recrsive to account for the fact that
 * include files may be nested.
 */

/* When multiple units are within the same include file, we need
 * to re-enter this routine with the second and subsequent units,
 * as such, we need to remember what include file we were processing
 * at the time. last_inc stores this. When current comes in as -1,
 * it means that we are processing another unit within the include,
 * so use the previous include fid value
 */

static int last_inc = 0;

static void print_include_records(struct record *w, struct record *n, int start, int current, int *pscope_index)
{
    int inner_index;
    int inc_fid;
    int scope_index = *pscope_index;
    int save_scope_index;
    int ret;

    /* if current == -1 then we are starting a new unit within the same
     * include file. As such, use the inc_fid from before
     */
     if (current == -1) inc_fid = last_inc;
     else /* pick up the include file id from the include record */
       inc_fid = CIFINC(w->ul[current].rptr)->incid;	

     /* note the include file id, just in case we return with another
      * unit within the same include file
      */
     last_inc = inc_fid;

    /* Search for matching fid to this include */
    for (inner_index = start + 1;
	 w->ul[inner_index].rptr != NULL &&
	 get_fid(w->ul[inner_index].rptr) != inc_fid;
	 inner_index++) {
    }

    for (;
	 w->ul[inner_index].rptr != NULL &&
	 get_fid(w->ul[inner_index].rptr) == inc_fid;
	 inner_index++) {

	if ((ret =
	     Cif_Putrecord(outfd,
			   w->ul[inner_index].rptr)) < 0) {
	    (void) fprintf (stderr,
		     "cif_lines: error writing output file %s - %s\n",
		     outfile, Cif_Errstring(ret));
	    exit (ret);
	}

	if (CIFGEN(w->ul[inner_index].rptr)->rectype == CIF_INCLUDE) {
	    print_include_records(w, n, start, inner_index, &scope_index);
	}
	else {
	  if (CIFGEN(w->ul[inner_index].rptr)->rectype ==CIF_F90_BEGIN_SCOPE ||
	      global_scope_found == 0 ) {

	    /* Look for the entry matching this scope and make
	     * sure that it comes out first
	     */
	    save_scope_index = scope_index;
	    for (; scope_index < n->ulcur; scope_index++) {

	      if (CIFGEN(n->ul[scope_index].rptr)->rectype !=
		  CIF_F90_ENTRY)
		break;

	      if (CIFF90ENTRY(n->ul[scope_index].rptr)->symid ==
		  CIFF90BS(w->ul[inner_index].rptr)->symid) {
				
		if ((ret =
		     Cif_Putrecord(outfd,
				   n->ul[scope_index].rptr)) < 0) {
		  (void) fprintf (stderr,
				  "cif_lines: error writing output file %s - %s\n",
				  outfile, Cif_Errstring(ret));
		  exit (ret);
		}
		CIFGEN(n->ul[scope_index].rptr)->rectype = 0;
		break;
	      }
	    }
	    scope_index = save_scope_index;
	    
	    for (; scope_index < n->ulcur; scope_index++) {

	      if (CIFGEN(n->ul[scope_index].rptr)->rectype == 0)
		continue;

	      if (get_scope(n->ul[scope_index].rptr) ==
		  CIFF90BS(w->ul[inner_index].rptr)->scopeid ||
		  global_scope_found == 0) {

		if (get_scope(n->ul[scope_index].rptr) == 0)
		  continue;

		/*
		 * For module entries, look to see if they are
		 * used directly, or indirectly
		 */
		if (CIFGEN(n->ul[scope_index].rptr)->rectype == CIF_F90_ENTRY &&
		    CIFF90ENTRY(n->ul[scope_index].rptr)->etype == CIF_F90_ET_MODULE) {
		  int mod;

		  for (mod = 0; mod < modid_current; mod++) {
	
		    if (modids[mod].modid ==
			CIFF90ENTRY(n->ul[scope_index].rptr)->symid) {
		      CIFF90ENTRY(n->ul[scope_index].rptr)->direct = modids[mod].direct;
		      break;
		    }
		  }
		}

		if ((ret =
		     Cif_Putrecord(outfd,
				   n->ul[scope_index].rptr)) < 0) {
		  (void) fprintf (stderr,
				  "cif_lines: error writing output file %s - %s\n",
				  outfile, Cif_Errstring(ret));
		  exit (ret);
		}
	      }
	      else
		break;
	    }
	  }
	}

	CIFGEN(w->ul[inner_index].rptr)->rectype = 0;

    }
   
    *pscope_index = scope_index;

}


/* --------------------------------------------------------------------------
 * Sort the records as selected and print 'em out.
 * -------------------------------------------------------------------------- */
static void print_records (struct record *w, struct record *n)
{
	int i, ret, scope_index;
	int scope_count = 0;
	int save_scope_index = 0;

	(void) qsort ((char *)w->ul, w->ulcur, sizeof(struct unit_list), (int(*)()) comp_id);
	(void) qsort ((char *)n->ul, n->ulcur, sizeof(struct unit_list), (int(*)()) comp_scope);

	/* Find the unit record first */
	for (i=0, scope_index = 0; i < w->ulcur; i++) {
	  if ((w->ul[i].rptr)->rectype == CIF_UNIT) {

	    if (get_fid(w->ul[i].rptr) > global_srcfid) {

	      /* put out the unit record */
	      if ((ret = Cif_Putrecord(outfd, w->ul[i].rptr)) < 0) {
		(void) fprintf (stderr,
		       "cif_lines: error writing output file %s - %s\n",
				outfile, Cif_Errstring(ret));
		exit (ret);
	      }

	      (w->ul[i].rptr)->rectype = 0;

	      /* print the records that exist within the include file
	       * within this unit */
	      print_include_records(w, n, i, -1, &scope_index);
	    }
	    break;
	  }
	}

	for (i=0, scope_index = 0; i < w->ulcur; i++) {

	    /*
	     * Any non-local (ie in a different file) objects will
	     * have been printed out after their matching cif_include line
	     */
#ifdef notdef
	    if (get_fid(w->ul[i].rptr) > global_srcfid) {
              if ((w->ul[i].rptr)->rectype == CIF_UNIT) {

                /* put out the unit record */
                if ((ret = Cif_Putrecord(outfd, w->ul[i].rptr)) < 0) {
                  (void) fprintf (stderr,
                         "cif_lines: error writing output file %s - %s\n",
                         outfile, Cif_Errstring(ret));
                  exit (ret);
                }
                /* print the records that exist within the include file
                 * within this unit */
		print_include_records(w, n, i, -1, &scope_index);
}
		break;
}
#endif
	    /*
	     * Not sure why there would ever be a zero, but just in case,
	     * as cif_putrecord would exit if it found a zero rectype
	     */
	    if ((w->ul[i].rptr)->rectype == 0)
		continue;

	    /*
	     * Check for two usages at the same point for the same symid;
	     * remove the first, it is a duplicate of the second. This is
	     * a workaround to a compiler problem
	     */
	    if (CIFGEN(w->ul[i].rptr)->rectype == CIF_USAGE &&
		i < w->ulcur - 1 &&
		CIFGEN(w->ul[i+1].rptr)->rectype == CIF_USAGE) {

		if (CIFUSAGE(w->ul[i].rptr)->use->line == CIFUSAGE(w->ul[i+1].rptr)->use->line &&
		    CIFUSAGE(w->ul[i].rptr)->use->cpos == CIFUSAGE(w->ul[i+1].rptr)->use->cpos &&
		    CIFUSAGE(w->ul[i].rptr)->symid == CIFUSAGE(w->ul[i+1].rptr)->symid) {

		    if (CIFUSAGE(w->ul[i].rptr)->use->utype == CIF_F90_OB_MODIFIED &&
			CIFUSAGE(w->ul[i+1].rptr)->use->utype == CIF_F90_OB_OPER_ARG

			)
			CIFUSAGE(w->ul[i+1].rptr)->use->utype = CIF_F90_OB_MODIFIED_ASN;
		    continue;
		}
	    }

	    if ((ret = Cif_Putrecord(outfd, w->ul[i].rptr)) < 0) {
		(void) fprintf (stderr,
			 "cif_lines: error writing output file %s - %s\n",
			 outfile, Cif_Errstring(ret));
		exit (ret);
	    }
	    else

		if (CIFGEN(w->ul[i].rptr)->rectype == CIF_INCLUDE) {
		    print_include_records(w, n, i, i, &scope_index);
		}
		else {
		    if (CIFGEN(w->ul[i].rptr)->rectype ==CIF_F90_BEGIN_SCOPE ||
			global_scope_found == 0 ) {

			/* Look for the entry matching this scope and
			 * make sure that it comes out first
			 */

			save_scope_index = scope_index;
			for (; scope_index < n->ulcur; scope_index++) {
			    if (get_scope(n->ul[scope_index].rptr) !=
				CIFF90BS(w->ul[i].rptr)->scopeid ||
				global_scope_found == 0) {
			      continue;
			    }

			    if (CIFGEN(n->ul[scope_index].rptr)->rectype !=
					CIF_F90_ENTRY)
				break;

			    if (CIFF90ENTRY(n->ul[scope_index].rptr)->symid ==
					CIFF90BS(w->ul[i].rptr)->symid) {
				

				if ((ret =
				     Cif_Putrecord(outfd,
						   n->ul[scope_index].rptr)) < 0) {
				    (void) fprintf (stderr,
						    "cif_lines: error writing output file %s - %s\n",
						    outfile, Cif_Errstring(ret));
				    exit (ret);
				}
				CIFGEN(n->ul[scope_index].rptr)->rectype = 0;
				break;
			    }
			}
			scope_index = save_scope_index;

			for (; scope_index < n->ulcur; scope_index++) {

			    if (CIFGEN(n->ul[scope_index].rptr)->rectype == 0)
				continue;

			    if (get_scope(n->ul[scope_index].rptr) < CIFF90BS(w->ul[i].rptr)->scopeid)
				continue;

			    if (get_scope(n->ul[scope_index].rptr) ==
				CIFF90BS(w->ul[i].rptr)->scopeid ||
				global_scope_found == 0) {

				if (get_scope(n->ul[scope_index].rptr) == 0)
				    continue;

				/*
				 * For module entries, look to see if they are
				 * used directly, or indirectly
				 */
				if (CIFGEN(n->ul[scope_index].rptr)->rectype == CIF_F90_ENTRY &&
				    CIFF90ENTRY(n->ul[scope_index].rptr)->etype == CIF_F90_ET_MODULE) {
				    int mod;


				    for (mod = 0; mod < modid_current; mod++) {
	
					if (modids[mod].modid ==
					    CIFF90ENTRY(n->ul[scope_index].rptr)->symid) {
					    CIFF90ENTRY(n->ul[scope_index].rptr)->direct = modids[mod].direct;
					    break;
					}
				    }
				}

				if ((ret =
				     Cif_Putrecord(outfd,
						   n->ul[scope_index].rptr)) < 0) {
				    (void) fprintf (stderr,
					     "cif_lines: error writing output file %s - %s\n",
					     outfile, Cif_Errstring(ret));
				    exit (ret);
				}
			    }
			    else {
			      break;
			    }
			}
		    }
		}
	}
}


/* --------------------------------------------------------------------------
 * Sort the header records and write them out
 * -------------------------------------------------------------------------- */
static void print_header_records (struct record *l)
{
	int i, ret;

	(void) qsort ((char *)l->ul, l->ulcur, sizeof(struct unit_list), (int(*)()) comp_rtype);
	for (i=0; i < l->ulcur; i++) {

	    /*
	     * We are writing a lines mode binary file, so set the bintype
	     * to show that lines wrote this cif file
	     */

	    if (l->ul[i].rptr->rectype == CIF_CIFHDR) {
		CIFHDR(l->ul[i].rptr)->bintype = CIF_FORM_LINES;
	    }

	    if ((ret = Cif_Putrecord(outfd, l->ul[i].rptr)) < 0) {
		(void) fprintf (stderr,
			 "cif_lines: error writing output file %s - %s\n",
			 outfile, Cif_Errstring(ret));
		exit (ret);
	    }
	}
}

/* --------------------------------------------------------------------------
 * Extract the symbol id from a record.  Return 0 if the record doesn't 
 * contain a symbol id.
 * -------------------------------------------------------------------------- */
static int get_id (
	struct Cif_generic *rptr)
{

	int id;

	switch (rptr->rectype) {
	case CIF_CALLSITE:
		id = CIFCS(rptr)->entryid;
		break;
	case CIF_COMBLK:
		id = CIFCB(rptr)->symid;
		break;
	case CIF_CONST:
		id = CIFCON(rptr)->symid;
		break;
	case CIF_ENTRY:
		id = CIFENTRY(rptr)->symid;
		break;
	case CIF_LABEL:
		id = CIFLABEL(rptr)->symid;
		break;
	case CIF_NAMELIST:
		id = CIFNL(rptr)->symid;
		break;
	case CIF_OBJECT:
		id = CIFOBJ(rptr)->symid;
		break;
	case CIF_USAGE:
		id = CIFUSAGE(rptr)->symid;
		break;

#if CIF_VERSION != 1
	case CIF_F90_CALLSITE:
		id = CIFF90CS(rptr)->entryid;
		break;
	case CIF_F90_COMBLK:
		id = CIFF90CB(rptr)->symid;
		break;
	case CIF_F90_CONST:
		id = CIFF90CON(rptr)->symid;
		break;
	case CIF_F90_ENTRY:
		id = CIFF90ENTRY(rptr)->symid;
		break;
	case CIF_F90_LABEL:
		id = CIFF90LABEL(rptr)->symid;
		break;
	case CIF_F90_NAMELIST:
		id = CIFF90NL(rptr)->symid;
		break;
	case CIF_F90_OBJECT:
		id = CIFF90OBJ(rptr)->symid;
		break;
	case CIF_F90_DERIVED_TYPE:
		id = CIFF90DTYPE(rptr)->symid;
		break;
	case CIF_F90_BEGIN_SCOPE:
		id = CIFF90BS(rptr)->symid;
		global_scope_found = 1;
		break;
	case CIF_F90_USE_MODULE:
		id = CIFF90USE(rptr)->modid;
		break;
	case CIF_F90_RENAME:
		id = CIFF90RN(rptr)->modid;
		break;
	case CIF_F90_INT_BLOCK:
		id = CIFF90IB(rptr)->intid;
		break;

	case CIF_GEOMETRY:
		id = CIFGEOM(rptr)->geomid;
		break;


	case CIF_C_LINT_DIRECTIVE:
		id = CIFCLDIR(rptr)->objid;
		break;
	case CIF_C_MACRO_DEF:
		id = CIFCMDEF(rptr)->symid;
		break;
	case CIF_C_MACRO_UNDEF:
		id = CIFCMUDEF(rptr)->symid;
		break;
	case CIF_C_MACRO_USAGE:
		id = CIFCMUSE(rptr)->symid;
		break;
	case CIF_C_ENTRY_END:
		id = CIFCEEND(rptr)->symid;
		break;

#endif /* CIF_VERSION != 1 */


#if CIF_VERSION == 3

	case CIF_SRC_POS:
		id = CIFSPOS(rptr)->symid;
		break;

#endif

	case CIF_C_TAG:
		id = CIFCTAG(rptr)->tagid;
		break;
	case CIF_C_CONST:
		id = CIFCCON(rptr)->symid;
		break;
	case CIF_C_ENTRY:
		id = CIFCENTRY(rptr)->symid;
		break;
	case CIF_C_OBJECT:
		id = CIFCOBJ(rptr)->symid;
		break;
	default:
		id = 0;
	}
	return (id);

}


/* --------------------------------------------------------------------------
 * Extract the line number from a record.  Return -? if the record doesn't 
 * contain a line number.
 * ------------------------------------------------------------------------- */
static int get_line (
	struct Cif_generic *rptr)
{
	int id;

	switch (rptr->rectype) {
	case CIF_UNIT:
		id = CIFUNIT(rptr)->line;
		break;
	case CIF_ENDUNIT:
		id = CIFENDU(rptr)->line;
		break;
	case CIF_CALLSITE:
		id = CIFCS(rptr)->line;
		break;
	case CIF_LOOP:
		id = CIFLOOP(rptr)->strline;
		break;
	case CIF_COMBLK:
		id = -1;
		break;
	case CIF_CONST:
		id = -2;
		break;
	case CIF_ENTRY:
		id = -3;
		break;
	case CIF_LABEL:
		id = -4;
		break;
	case CIF_MESSAGE:
		id = CIFMSG(rptr)->fline;;
		break;
	case CIF_ND_MSG:
		id = CIFNMSG(rptr)->fline;;
		break;
	case CIF_NAMELIST:
		id = -5;
		break;
	case CIF_OBJECT:
		id = -6;
		break;
	case CIF_USAGE:
		id = CIFUSAGE(rptr)->use->line;
		break;
	case CIF_STMT_TYPE:
		id = CIFSTMT(rptr)->line;
		break;
	case CIF_INCLUDE:
		id = CIFINC(rptr)->line;
		break;

#if CIF_VERSION != 1
	case CIF_CDIR:
		id = CIFCDIR(rptr)->line;
		break;
	case CIF_CDIR_DOSHARED:
		id = CIFCDIRDO(rptr)->line;
		break;
	case CIF_CONTINUATION:
		id = CIFCONT(rptr)->line;
		break;


	case CIF_F90_CALLSITE:
		id = CIFF90CS(rptr)->line;
		break;
	case CIF_F90_COMBLK:
		id = -7;
		break;
	case CIF_F90_LOOP:
		id = CIFF90LOOP(rptr)->strline;
		break;
	case CIF_F90_ENTRY:
		id = -8;
		break;
	case CIF_F90_CONST:
		id = CIFF90CON(rptr)->strline;
		break;
	case CIF_F90_LABEL:
		id = -10;
		break;
	case CIF_F90_NAMELIST:
		id = -11;
		break;
	case CIF_F90_OBJECT:
		id = -12;
		break;
	case CIF_F90_DERIVED_TYPE:
		id = -13;
		break;
	case CIF_F90_BEGIN_SCOPE:
		id = CIFF90BS(rptr)->line;
		global_scope_found = 1;
		break;
	case CIF_F90_END_SCOPE:
		id = CIFF90ES(rptr)->line;
		break;
	case CIF_F90_SCOPE_INFO:
		id = -14;
		break;
	case CIF_F90_USE_MODULE:
		id = -15;
		break;
	case CIF_F90_RENAME:
		id = -16;
		break;
	case CIF_F90_INT_BLOCK:
		id = -17;
		break;

	case CIF_GEOMETRY:
		id = -18;
		break;


	case CIF_C_LINT_DIRECTIVE:
		id = CIFCLDIR(rptr)->strline;
		break;
	case CIF_C_MACRO_DEF:
		id = CIFCMDEF(rptr)->strline;
		break;
	case CIF_C_MACRO_UNDEF:
		id = CIFCMUDEF(rptr)->line;
		break;
	case CIF_C_MACRO_USAGE:
		id = CIFCMUSE(rptr)->strline;
		break;
	case CIF_C_ENTRY_END:
		id = CIFCEEND(rptr)->strline;
		break;

	case CIF_BE_NODE:
		id = -19;
		break;

	case CIF_BE_FID:
		id = -19;
		break;

#endif /* CIF_VERSION != 1 */

#if CIF_VERSION >= 3

	case CIF_CC_TYPE:
		id = -24;
		break;

	case CIF_CC_ENTRY:
		id = CIFCCENT(rptr)->sline;
		break;

	case CIF_CC_OBJ:
		id = -24;
		break;

	case CIF_CC_SUBTYPE:
		id = -24;
		break;

	case CIF_CC_ENUM:
		id = -24;
		break;

	case CIF_CC_EXPR:
		id = CIFCCEXPR(rptr)->line;
		break;

	case CIF_SRC_POS:
		id = CIFSPOS(rptr)->sline;
		break;

#endif 

	case CIF_C_TAG:
		id = -20;
		break;
	case CIF_C_CONST:
		id = -21;
		break;
	case CIF_C_MESSAGE:
		id = CIFCMSG(rptr)->fline;
		break;
	case CIF_C_ENTRY:
		id = -22;
		break;
	case CIF_C_OBJECT:
		id = -23;
		break;
	default:
		id = 0;
	}
	return (id);
}


/* --------------------------------------------------------------------------
 * Extract the fid from a record.  Return -? if the record doesn't 
 * contain a fid.
 * ------------------------------------------------------------------------- */
static int get_fid (
	struct Cif_generic *rptr)
{
	int id;

	switch (rptr->rectype) {
	case CIF_UNIT:
		id = CIFUNIT(rptr)->fid;
		break;
	case CIF_ENDUNIT:
		id = CIFENDU(rptr)->fid;
		break;
	case CIF_CALLSITE:
		id = CIFCS(rptr)->fid;
		break;
	case CIF_COMBLK:
		id = -1;
		break;
	case CIF_CONST:
		id = -2;
		break;
	case CIF_ENTRY:
		id = -3;
		break;
	case CIF_LOOP:
		id = CIFLOOP(rptr)->sfid;
		break;
	case CIF_LABEL:
		id = -4;
		break;
	case CIF_MESSAGE:
		id = CIFMSG(rptr)->fid;
		break;
	case CIF_ND_MSG:
		id = CIFNMSG(rptr)->fid;
		break;
	case CIF_NAMELIST:
		id = -5;
		break;
	case CIF_OBJECT:
		id = -6;
		break;
	case CIF_USAGE:
		id = CIFUSAGE(rptr)->use->fid;
		break;
	case CIF_STMT_TYPE:
		id = CIFSTMT(rptr)->fid;
		break;
	case CIF_INCLUDE:
		id = CIFINC(rptr)->srcid;
		break;

#if CIF_VERSION != 1
	case CIF_CDIR:
		id = CIFCDIR(rptr)->fid;
		break;
	case CIF_CDIR_DOSHARED:
		id = CIFCDIRDO(rptr)->fid;
		break;
	case CIF_CONTINUATION:
		id = CIFCONT(rptr)->fid;
		break;


	case CIF_F90_CALLSITE:
		id = CIFF90CS(rptr)->fid;
		break;
	case CIF_F90_COMBLK:
		id = -7;
		break;
	case CIF_F90_LOOP:
		id = CIFF90LOOP(rptr)->sfid;
		break;
	case CIF_F90_ENTRY:
		id = -8;
		break;
	case CIF_F90_CONST:
		id = CIFF90CON(rptr)->fid;
		break;
	case CIF_F90_LABEL:
		id = -10;
		break;
	case CIF_F90_NAMELIST:
		id = -11;
		break;
	case CIF_F90_OBJECT:
		id = -12;
		break;
	case CIF_F90_DERIVED_TYPE:
		id = -13;
		break;
	case CIF_F90_BEGIN_SCOPE:
		id = CIFF90BS(rptr)->fid;
		global_scope_found = 1;
		break;
	case CIF_F90_END_SCOPE:
		id = CIFF90ES(rptr)->fid;
		break;
	case CIF_F90_SCOPE_INFO:
		id = -14;
		break;
	case CIF_F90_USE_MODULE:
		id = -15;
		break;
	case CIF_F90_RENAME:
		id = -16;
		break;
	case CIF_F90_INT_BLOCK:
		id = -17;
		break;

	case CIF_GEOMETRY:
		id = -18;
		break;


	case CIF_C_LINT_DIRECTIVE:
		id = CIFCLDIR(rptr)->fid;
		break;
	case CIF_C_MACRO_DEF:
		id = CIFCMDEF(rptr)->fid;
		break;
	case CIF_C_MACRO_UNDEF:
		id = CIFCMUDEF(rptr)->fid;
		break;
	case CIF_C_MACRO_USAGE:
		id = CIFCMUSE(rptr)->fid;
		break;
	case CIF_C_ENTRY_END:
		id = CIFCEEND(rptr)->fid;
		break;

	case CIF_BE_NODE:
		id = -19;
		break;

	case CIF_BE_FID:
		id = -19;
		break;

#endif

#if CIF_VERSION == 3

	case CIF_CC_TYPE:
		id = -24;
		break;

	case CIF_CC_ENTRY:
		id = CIFCCENT(rptr)->sfid;
		break;

	case CIF_CC_OBJ:
		id = -24;
		break;

	case CIF_CC_SUBTYPE:
		id = -24;
		break;

	case CIF_CC_ENUM:
		id = -24;
		break;

	case CIF_CC_EXPR:
		id = CIFCCEXPR(rptr)->fid;
		break;

	case CIF_SRC_POS:
		id = CIFSPOS(rptr)->fid;
		break;

#endif

	case CIF_C_TAG:
		id = -20;
		break;
	case CIF_C_CONST:
		id = -21;
		break;
	case CIF_C_MESSAGE:
		id = CIFCMSG(rptr)->fid;
		break;
	case CIF_C_ENTRY:
		id = -22;
		break;
	case CIF_C_OBJECT:
		id = -23;
		break;
	default:
		id = 0;
	}
	return (id);
}





/* --------------------------------------------------------------------------
 * Extract the scope from a record.  Return 0 if the record doesn't 
 * contain a scope
 * ------------------------------------------------------------------------- */
static int get_scope (
	struct Cif_generic *rptr)
{
	int id;

	switch (rptr->rectype) {

#if CIF_VERSION != 1

	case CIF_F90_COMBLK:
		id = CIFF90CB(rptr)->scopeid;
		break;
	case CIF_F90_LOOP:
		id = CIFF90LOOP(rptr)->scopeid;
		break;
	case CIF_F90_ENTRY:
		id = CIFF90ENTRY(rptr)->scopeid;
		break;
	case CIF_F90_LABEL:
		id = CIFF90LABEL(rptr)->scopeid;
		break;
	case CIF_F90_NAMELIST:
		id = CIFF90NL(rptr)->scopeid;
		break;
	case CIF_F90_OBJECT:
		id = CIFF90OBJ(rptr)->scopeid;
		break;
	case CIF_F90_DERIVED_TYPE:
		id = CIFF90DTYPE(rptr)->scopeid;
		break;
	case CIF_F90_BEGIN_SCOPE:
		id = CIFF90BS(rptr)->scopeid;
		global_scope_found = 1;
		break;
	case CIF_F90_END_SCOPE:
		id = CIFF90ES(rptr)->scopeid;
		break;
	case CIF_F90_SCOPE_INFO:
		id = CIFF90SI(rptr)->scopeid;
		break;
	case CIF_F90_INT_BLOCK:
		id = CIFF90IB(rptr)->scopeid;
		break;
	case CIF_F90_RENAME:
		id = CIFF90RN(rptr)->scopeid;
		break;
	case CIF_F90_CONST:
		id = CIFF90CON(rptr)->scopeid;
		break;
	case CIF_F90_USE_MODULE:
		id = global_last_scope; /* use_modules belong to the last
					   scope block seen; this really
					   should be a field in the use_module
					   record, but it isn't so this
					   will have to do for now */
		break;

#endif

	case CIF_C_ENTRY:
		id = CIFCENTRY(rptr)->scope;
		break;
	default:
		id = -1;
	}
	return (id);
}


/* --------------------------------------------------------------------------
 * Extract the type from a record. Moves some records higher than
 * they should be to bump them sooner in the output
 * ------------------------------------------------------------------------- */
static int get_type (
	struct Cif_generic *rptr)
{
    int id;

    switch (rptr->rectype) {

#if CIF_VERSION != 1
	case CIF_F90_ENTRY:
	id = -10;
	break;
#endif

	case CIF_ENTRY:
	id = -10;
	break;

	default:
	id = rptr->rectype;
	break;
    }

    return(id);
}


/* --------------------------------------------------------------------------
 * Extract the scope from a record.  Return 99999 if the record doesn't 
 * contain a scope such that they come after any records with a scope
 * ------------------------------------------------------------------------- */
static int get_adjusted_scope (
	struct Cif_generic *rptr)
{
	int id;

	switch (rptr->rectype) {

#if CIF_VERSION != 1

	case CIF_F90_COMBLK:
		id = CIFF90CB(rptr)->scopeid;
		break;
	case CIF_F90_LOOP:
		id = CIFF90LOOP(rptr)->scopeid;
		break;
	case CIF_F90_ENTRY:
		id = CIFF90ENTRY(rptr)->scopeid;
		break;
	case CIF_F90_LABEL:
		id = CIFF90LABEL(rptr)->scopeid;
		break;
	case CIF_F90_NAMELIST:
		id = CIFF90NL(rptr)->scopeid;
		break;
	case CIF_F90_OBJECT:
		id = CIFF90OBJ(rptr)->scopeid;
		break;
	case CIF_F90_DERIVED_TYPE:
		id = CIFF90DTYPE(rptr)->scopeid;
		break;
	case CIF_F90_BEGIN_SCOPE:
		id = CIFF90BS(rptr)->scopeid;
		global_scope_found = 1;
		break;
	case CIF_F90_END_SCOPE:
		id = CIFF90ES(rptr)->scopeid;
		break;
	case CIF_F90_SCOPE_INFO:
		id = CIFF90SI(rptr)->scopeid;
		break;
	case CIF_F90_INT_BLOCK:
		id = CIFF90IB(rptr)->scopeid;
		break;
	case CIF_F90_RENAME:
		id = CIFF90RN(rptr)->scopeid;
		break;
	case CIF_F90_CONST:
		id = CIFF90CON(rptr)->scopeid;
		break;
	case CIF_F90_USE_MODULE:
		id = global_last_scope; /* use_modules belong to the last
					   scope block seen; this really
					   should be a field in the use_module
					   record, but it isn't so this
					   will have to do for now */
		break;

#endif 

	case CIF_C_ENTRY:
		id = CIFCENTRY(rptr)->scope;
		break;
	default:
		id = 99999;
	}
	return (id);
}



/* --------------------------------------------------------------------------
 * Extract the cpos number from a record.  Return 0 if the record doesn't 
 * contain a cpos.
 * ------------------------------------------------------------------------- */
static int get_cpos (
	struct Cif_generic *rptr)
{
	int id;

	switch (rptr->rectype) {
	case CIF_UNIT:
		id = CIFUNIT(rptr)->cpos - 2; /* must come first, before
						 stmt's and scopes at same
						 line and cpos */
		break;
	case CIF_ENDUNIT:
		id = CIFENDU(rptr)->cpos + 2;  /* must come last, after stmt's
						  and end scopes at same line
						  and cpos */
		break;
	case CIF_CALLSITE:
		id = CIFCS(rptr)->cpos;
		break;
	case CIF_MESSAGE:
		id = CIFMSG(rptr)->cpos;;
		break;
	case CIF_ND_MSG:
		id = CIFNMSG(rptr)->cpos;;
		break;
	case CIF_USAGE:
		id = CIFUSAGE(rptr)->use->cpos;
		break;
	case CIF_STMT_TYPE:
		id = CIFSTMT(rptr)->cpos;
		break;
	case CIF_INCLUDE:
		id = CIFINC(rptr)->cpos;
		break;

#if CIF_VERSION != 1
	case CIF_CDIR:
		id = CIFCDIR(rptr)->cpos;
		break;
	case CIF_CDIR_DOSHARED:
		id = CIFCDIRDO(rptr)->cpos;
		break;
	case CIF_CONTINUATION:
		id = CIFCONT(rptr)->cpos;
		break;


	case CIF_F90_CALLSITE:
		id = CIFF90CS(rptr)->cpos;
		break;
	case CIF_F90_BEGIN_SCOPE:
		id = CIFF90BS(rptr)->cpos - 1;  /* ensure that this
						   occurs before all other
						   stmts at this line/cpos */
		global_scope_found = 1;
		break;
	case CIF_F90_END_SCOPE:
		id = CIFF90ES(rptr)->cpos + 1; /* as above, only after */
		break;

	case CIF_C_LINT_DIRECTIVE:
		id = CIFCLDIR(rptr)->strpos;
		break;
	case CIF_C_MACRO_DEF:
		id = CIFCMDEF(rptr)->strpos;
		break;
	case CIF_C_MACRO_UNDEF:
		id = CIFCMUDEF(rptr)->cpos;
		break;
	case CIF_C_MACRO_USAGE:
		id = CIFCMUSE(rptr)->strpos;
		break;

#endif 


#if CIF_VERSION == 3

	case CIF_SRC_POS:
		id = CIFSPOS(rptr)->scol;
		break;

#endif 

	default:
		id = 0;
	}
	return (id);

}


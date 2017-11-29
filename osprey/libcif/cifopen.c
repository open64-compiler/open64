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


static char USMID[] = "@(#) libcif/cifopen.c	30.6	05/22/97 11:49:31";


/*
 * Cif_Open opens a compiler input input file, either the original ASCII
 * format or the compressed binary format.  For input files if the "rtypes"
 * argument is not NULL, it represents a zero terminated array of record type
 * values.  Only records of the types selected will be returned by cifgetrec.
 * If "rtypes" is NULL, all record types will be returned.
 *
 * tabs are set to be read with tab spacing = 3
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
#include <sys/types.h>
#include <sys/stat.h>

#define DEFGLOBAL
#include "cif_int.h"

#define HDRBUF_SIZE	128	/* size of buffer used to read the file header */
#define HDRTOKENS		10		/* number of tokens in header before tools field */



/* The cif version number == value of CIF_VERSION
 * picked up by Cif_Open function through the use of
 * alternative entries to it depending on the value of
 * CIF_VERSION. Used in cifdup.c to allow Cif_Duplicate
 * to know what version cif to create and copy, and in
 * ciffree.c for Cif_Free to know what storage to free.
 */

int _cif_version = 0;


#undef Cif_Open  /* ensure that we don't try to map this to Cif_Open_Vx */


/* Special case of cif_open for applications compiled with CIF_VERSION == 1,
   which is the default. A check is made that they are not trying to open
   a cif with a version greater than (ie, > 1) the library that they
   were compiled with. Probably just catches applications that want to read
   a version 2 cif, but haven't set CIF_VERSION 2 before including cif.h */

int Cif_Open_V1
(char *filename, char *optype, int *rtypes, int version)
{

  /* We can only enter this function if CIF_VERSION used by the application
     is set to 1, so if the same application is trying to read a cif version > 1,
     the data structures wont match, so better to issue the error now than later */

  if (version > 1)
    return (CIF_EXP_VERS);

  _cif_version = 1;  /* record the CIF_VERSION in use */

  /* call the real cif open */

  return( Cif_Open(filename, optype, rtypes, version) );

}

/* Special case of cif_open for applications compiled with CIF_VERSION == 2.
   A check is made that they are not trying to open
   a cif with a version less than (ie, < 2) the library that they
   were compiled with. If they compile with version 2, they have to read version 2 */

int Cif_Open_V2
(char *filename, char *optype, int *rtypes, int version)
{

  /* We can only enter this function if CIF_VERSION used by the application
     is set to 2, so if the same application is trying to read a cif version < 2,
     the data structures wont match, so better to issue the error now than later */

  if (version < 2)
    return (CIF_EXP_VERS2);

  _cif_version = 2;  /* record the CIF_VERSION in use */

  /* call the real cif open */

  return( Cif_Open(filename, optype, rtypes, version) );

}


/*
 * As above + added a check to see of the cif.h in use matches what this
 * library was compiled with...looks at CIF_SUB_VERSION_2 which must match
 * the sub_version passed. See cif_open macros in cif.h for more details.
 */

int Cif_Open_V2_1
(char *filename, char *optype, int *rtypes, int version, int sub_version)
{

  /* We can only enter this function if CIF_VERSION used by the application
     is set to 2, so if the same application is trying to read a cif version < 2,
     the data structures wont match, so better to issue the error now than later */

  if (version < 2)
    return (CIF_EXP_VERS2);

  if (sub_version != CIF_SUB_VERSION_2)
      return(CIF_SUBVER);

  _cif_version = 2;  /* record the CIF_VERSION in use */

  /* call the real cif open */

  return( Cif_Open(filename, optype, rtypes, version) );

}
/*
 * As above for version 3
 */

int Cif_Open_V3_1
(char *filename, char *optype, int *rtypes, int version, int sub_version)
{

  /* We can only enter this function if CIF_VERSION used by the application
     is set to 3, so if the same application is trying to read a cif
     version < 3, the data structures wont match, so better to issue the
     error now than later */

  if (version < 3)
    return (CIF_EXP_VERS2);

  if (sub_version != CIF_SUB_VERSION_3)
      return(CIF_SUBVER);

  _cif_version = 3;  /* record the CIF_VERSION in use */

  /* call the real cif open */

  return( Cif_Open(filename, optype, rtypes, version) );

}


/*
 * Cif_Open can only be called directly by a v1 application which
 * has been re-linked, but not recompiled. If it had been re-compiled.
 * either Cif_Open_V1, or Cif_Open_V2 would be used. If called directly,
 * _cif_version will still be 0, so set it to 1 (you can't get v2 records
 * without recompiling) and validate against the cif_open version request
 */

int Cif_Open
(char *filename, char *optype, int *rtypes, int version)
{
	int i;
	int cx;					/* _Cif_filetbl index */
	int *rt;					/* pointer to record type selectors */
	FILE *fd;				/* file descriptor for file to open */
	char hdrbuf[HDRBUF_SIZE];	/* buffer to contain header record */
	struct stat statbuf;	/* file status structure */

	static char ascii_hdrkey[6] = { '2',SEPARATOR,'c','i','f',SEPARATOR };
	static char binary_hdrkey[6] = { SEPARATOR,'c','i','f','b',SEPARATOR };
	static char binary_version = _CIF_INT_VERSION;

	int rtype;
	struct Cif_generic *rptr;

	if (_cif_version == 0) {

	  /*
	   * Cif_open must have been called directly by a version 1 application
	   * (see comment above). Validate the requested version.
	   */

	  if (version > 1)
	    return (CIF_EXP_VERS);

	  _cif_version = 1;  /* record the CIF_VERSION in use */

	}



	/* search for open slot in CIF file table */

	if (version > _CIF_INT_VERSION)
		return (CIF_EXP_VERS);
	for (cx=0; cx < CIF_FT_SIZE; cx++)
		if (_Cif_filetbl[cx].form == NOT_A_CIF) break;
	if (cx >= CIF_FT_SIZE)
		return (CIF_MAXOPENS);
	_Cif_filetbl[cx].ifull = NO;
	_Cif_filetbl[cx].seek = NO;
	_Cif_filetbl[cx].version = 0;
	_Cif_filetbl[cx].return_version = 0;
	_Cif_filetbl[cx].ip = NULL;
	_Cif_filetbl[cx].mode = CIF_MEM_DEFAULT;

	/* open the file */

	if (strcmp (optype, "r") == 0 || strcmp (optype, "w") == 0)
		_Cif_filetbl[cx].optype = *optype;
	else
		return (CIF_BADREQ);
	if (strcmp(filename, "-") == 0) {
		if (*optype == 'r')
			fd = stdin;
		else
			fd = stdout;
	}
	else if ((fd = fopen(filename, optype)) == NULL)
		return (CIF_SYSERR);


	/* if input file, allocate buffer and verify file format then process
	 * the record selection mask */

	if (*optype == 'r') {

		if (fread (hdrbuf, sizeof(char), 6, fd) != 6) {
			(void) fclose (fd);
			return (CIF_NOTCIF);
		}

		else if (strncmp (hdrbuf, ascii_hdrkey, 6) == 0) {

			/* ASCII file, set file format and reset file position to beginning */

			_Cif_filetbl[cx].form = ASCII_CIF;
			if ((_Cif_filetbl[cx].ip = malloc (CIF_BUFSIZE)) == NULL)
				return (CIF_NOMEM);
			(void) strncpy (_Cif_filetbl[cx].ip, hdrbuf, 6);
			if (fgets (_Cif_filetbl[cx].ip+6, CIF_BUFSIZE-6, fd) == NULL) 
				IO_ERROR;
			i = atoi(_Cif_filetbl[cx].ip+7);
			if (i > _CIF_INT_VERSION)
				return (CIF_FILE_VERS);

			_Cif_filetbl[cx].version = i; 	 	       	/* version of cif on disk */
			_Cif_filetbl[cx].return_version = version; 	/* version that the user wants */

			_Cif_filetbl[cx].ifull = YES;
		}

		else if (strncmp (hdrbuf, binary_hdrkey, 6) == 0) {

			/* Binary file, set file format.  Read CIF version and validate. */

			_Cif_filetbl[cx].form = BINARY_CIF;
			if (fread (hdrbuf, sizeof(char), 1, fd) != 1)
				IO_ERROR;
			if (hdrbuf[0] > _CIF_INT_VERSION)
				return (CIF_FILE_VERS);

			_Cif_filetbl[cx].version = hdrbuf[0]; 	       	/* version of cif on disk */
			_Cif_filetbl[cx].return_version = version;   	/* version that the user wants */

		}

		else
			return (CIF_NOTCIF);

		if (rtypes == NULL)
			for (i = 0; i < CIF_MAXRECORD; i++) _Cif_filetbl[cx].rmask[i] = '\1';
		else {
			for (i = 0; i < CIF_MAXRECORD; i++) _Cif_filetbl[cx].rmask[i] = '\0';
			for (rt = rtypes; *rt != 0; rt++)
				_Cif_filetbl[cx].rmask[*rt] = '\1';
		}

		/* Take out records that the requested cif version doesn't know about */

		if (version == 1) {
		  _Cif_filetbl[cx].rmask[CIF_C_LINT_DIRECTIVE] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_C_MACRO_DEF] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_C_MACRO_UNDEF] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_C_MACRO_USAGE] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_C_ENTRY_END] = '\0';

		  _Cif_filetbl[cx].rmask[CIF_CDIR] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_CDIR_DOSHARED] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_GEOMETRY] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_CONTINUATION] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_TRANSFORM] = '\0';

		  _Cif_filetbl[cx].rmask[CIF_F90_CALLSITE] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_COMBLK] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_CONST] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_ENTRY] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_LOOP] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_DERIVED_TYPE] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_LABEL] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_NAMELIST] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_OBJECT] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_MISC_OPTS] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_OPT_OPTS] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_BEGIN_SCOPE] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_END_SCOPE] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_SCOPE_INFO] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_USE_MODULE] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_RENAME] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_INT_BLOCK] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_F90_VECTORIZATION] = '\0';

		  _Cif_filetbl[cx].rmask[CIF_BE_NODE] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_BE_FID] = '\0';

		}

		if (version < 3) {

		  _Cif_filetbl[cx].rmask[CIF_CC_TYPE] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_CC_ENTRY] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_CC_OBJ] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_CC_SUBTYPE] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_CC_ENUM] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_CC_EXPR] = '\0';

		  _Cif_filetbl[cx].rmask[CIF_SRC_POS] = '\0';
		  _Cif_filetbl[cx].rmask[CIF_ORIG_CMD] = '\0';

		}
	}

	else {	/* output file so */
		_Cif_filetbl[cx].form = BINARY_CIF;
		if (fwrite (binary_hdrkey, sizeof(char), 6, fd) != 6)
			return (CIF_SYSERR);
		if (fwrite (&binary_version, sizeof(char), 1, fd) != 1)
			return (CIF_SYSERR);

		/* Set both of the cif versions to the cif_open version; it is
		   not possible to open a cif as one version and then write
		   records from another version into it; ie if the users
		   compiled with v1 then that's what they get for all opens;
		   likewise for v2 */

		_Cif_filetbl[cx].version = version;	    /* version of cif on disk */
		_Cif_filetbl[cx].return_version = version;  /* version that the user wants */
	      }

	/* determine if seeks are allowed on file */

	(void) fstat (fileno(fd), &statbuf);
	_Cif_filetbl[cx].seek = (S_ISREG(statbuf.st_mode) ? YES : NO);

	_Cif_filetbl[cx].fd = fd;

	/* Make a note of the cif being opened */
	_Cif_filetbl[cx].filename = strdup(filename);

	/*
	 * Assume cif not created specially, cif_lines and cif_conv will
	 * alter this if they are invoking cif_open indirectly for the user
	 * and a temporary cif has been opened
	 */
	_Cif_filetbl[cx].tmp_cif = 0;


	if (*optype == 'r' &&  /* reading from the file */
	    _Cif_filetbl[cx].seek == YES &&  /* Otherwise, we can't backup */
	    _Cif_filetbl[cx].return_version > 1) {  /* v2 cif or greater */

	    /*
	     * This is really messy, but we want to read ahead until
	     * the srcfile record is read so that the src file id can
	     * be put into the cifhdr record.
	     */

	    /* ensure that the file is at the beginning */
	    (void) Cif_Setpos(cx, CIF_FIRST_RECORD);

	    do {
		rtype =  Cif_Getrecord(cx, &rptr);
	    }
	    while (rtype > 0 && rtype != CIF_SRCFILE &&
		   rtype != CIF_UNIT);

	    if (rtype == CIF_SRCFILE) {
		_Cif_filetbl[cx].srcfid = CIFSRC(rptr)->fid;
	    }

	    /* Reset, so that it looks like we haven't read anything yet */
	    _Cif_filetbl[cx].mode = CIF_MEM_DEFAULT;
	}

	/* ensure that the file is at the beginning */
	if (_Cif_filetbl[cx].seek == YES)
	    (void) Cif_Setpos(cx, CIF_FIRST_RECORD);

	return (cx);
	
}

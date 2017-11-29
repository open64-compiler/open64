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


/* USMID @(#) libcif/cif_int.h	30.1	05/06/96 07:34:46 */


/*
 * Compiler Information File interface routines internal information
 */

#ifndef __CIFINT_H_
#define __CIFINT_H_

#ifndef __STDC__
#	define const
#endif

#define IO_ERROR { \
	if (feof(fd)) return (CIF_BADFORM); \
	else return (CIF_SYSERR); \
}

/* --- CIF boolean indicators --- */
#define NO		0
#define YES		1

/* --- CIF format indicator values --- */
#define NOT_A_CIF	0		/* invalid or unknown cif format */
#define ASCII_CIF	1		/* ASCII file format indicator */
#define BINARY_CIF	2		/* binary file format indicator */

/* --- CIF formats to add to the returned cifhdr record --- */
/* The above defines cant be used because only one bit was
 * allocated for this field in the record. As we will never
 * return a NOT_A_CIF cifhdr anyway, this is okay
 */
#define ASCII_CIF_FORMAT 0
#define BINARY_CIF_FORMAT 1


/* --- CIF internal values --- */
#define CIF_FT_SIZE	10		/* no. of entries in _Cif_Filetbl  */
#define CIF_BUFSIZE	8192		/* size of record input buffer */
#define SEPARATOR	'\036'		/* record separator = CNTL-^ */
#define BINARY_HDR_LEN	7		/* no. chars in binary file header */

/* --- Constant/object attibute masks --- */
#define CO_ATTR_IMPTYPE		0x01
#define CO_ATTR_CHAR		0x02
#define CO_ATTR_DIM		0x04
#define CO_ATTR_SAVE		0x08
#define CO_ATTR_DATA		0x10
#define CO_ATTR_EQUIV		0x20
#define CO_ATTR_AUTO		0x40
#define CO_ATTR_PE_RESIDENT	0x80
#define CO_ATTR_POINTEE		0x100
#define CO_ATTR_ARRAY_DEC	0x200
#define CO_ATTR_GEOM_DEC	0x400

/* --- F90 Constant/object attibute masks --- */
#define F90_CO_ATTR_IMPTYPE		0x01
#define F90_CO_ATTR_POINTEE		0x02
#define F90_CO_ATTR_DEF_TYPE       	0x04
#define F90_CO_ATTR_STAR_TYPE       	0x08
#define F90_CO_ATTR_KIND_TYPE       	0x10

#define F90_CO_ATTR_SAVE		0x20
#define F90_CO_ATTR_DATA		0x40
#define F90_CO_ATTR_EQUIV		0x80

#define F90_CO_ATTR_ARRAY_DEC    	0x100
#define F90_CO_ATTR_GEOM_DEC    	0x200
#define F90_CO_ATTR_PE_RESIDENT    	0x400

#define F90_CO_ATTR_ALLOCATABLE   	0x800
#define F90_CO_ATTR_INTENTIN    	0x1000
#define F90_CO_ATTR_INTENTOUT    	0x2000
#define F90_CO_ATTR_INTENTINOUT    	0x4000
#define F90_CO_ATTR_OPTIONAL    	0x8000
#define F90_CO_ATTR_POINTER    		0x10000
#define F90_CO_ATTR_PRIVATE    		0x20000
#define F90_CO_ATTR_TARGET    		0x40000
#define F90_CO_ATTR_LOCAL_NAME  	0x80000

/* --- F90 scope information attibute masks --- */
#define SC_ATTR_IMPNONE		0x01
#define SC_ATTR_IO		0x02
#define SC_ATTR_CALL		0x04
#define SC_ATTR_CMIC		0x08

/* --- Entry attribute masks --- */
#define EN_ATTR_IMPTYPE		0x01
#define EN_ATTR_CHAR		0x02
#define EN_ATTR_DIM		0x04
#define EN_ATTR_INTRIN		0x08
#define EN_ATTR_EXTERN		0x10
#define EN_ATTR_STMTF		0x20
#define EN_ATTR_RECUR		0x40


/* --- F90 Entry attribute masks --- */
#define F90_EN_ATTR_DEFINED		0x01
#define F90_EN_ATTR_INT_BLOCK      	0x02
#define F90_EN_ATTR_ALT_ENTRY		0x04
#define F90_EN_ATTR_USE			0x04
#define F90_EN_ATTR_REFERENCED		0x08
#define F90_EN_ATTR_OPTIONAL		0x10
#define F90_EN_ATTR_PRIVATE		0x20
#define F90_EN_ATTR_RECUR		0x40

/* --- memory area table --- */
#define CIF_MEM_BUMP	50	/* number of entries to add/increas */
struct _Cif_Mem_Area {
	short used;		/* entry in use */
	int nme;		/* index of next entry used for file */
	int mused;		/* amount of buffer space used */
	int msize;		/* number of bytes in memory area */
	char *mbp;		/* pointer to beginning of buffer */
}; 
#ifdef DEFGLOBAL
int _Cif_memasize;		/* current no of entries in _Cif_memarea */
struct _Cif_Mem_Area *_Cif_memarea;
#else
extern int _Cif_memasize;
extern struct _Cif_Mem_Area *_Cif_memarea;
#endif

/* --- CIF open file table --- */
struct _Cif_File_Tbl {
	short form;		/* ASCII/binary file format indicator */
	short ifull;		/* buffer already contains a record */ 
	short seek;		/* seeking allowed on file flag */
	short mode;		/* record memory management mode for file */
	int fme;		/* index of first memory entry; managed mode */
	int lme;		/* index of curr memory entry; managed mode */
	char rmask[CIF_MAXRECORD]; /* record selection marker array */
	FILE *fd;		/* file descriptor of cif file */
	char *ip;		/* pointer to input buffer for file */
	char optype;		/* type of open: r = input, w = output */
	int lang;		/* language of cif file */
	int version;		/* CIF version */
	int return_version;	/* CIF version expected by user */
	int srcfid;		/* source file id, read from srcfile record */
	char *filename;		/* Filename opened */
	int tmp_cif;		/* Set if cif was created by cif_lines or
				 *  cif_convert functions */
};

#ifdef DEFGLOBAL
struct _Cif_File_Tbl _Cif_filetbl [CIF_FT_SIZE];
#else
extern struct _Cif_File_Tbl _Cif_filetbl [CIF_FT_SIZE];
#endif

#if CIF_VERSION != 1

/* Buffer to use when mapping a version 1 cif to a version 2 cif record.
 * Required because some records increased in size so reading a v2 record
 * into space for a v1 cif would not work. Data is read into this buffer and
 * shaped into the correct cif record structure that the application requested.
 */
extern struct Cif_generic *_cif_map_buffer;

/* The cif version number == value of CIF_VERSION
 * picked up by Cif_Open function through the use of
 * alternative entries to it depending on the value of
 * CIF_VERSION. Used in cifdup.c to allow Cif_Duplicate
 * to know what version cif to create and copy, and in
 * ciffree.c for Cif_Free to know what storage to free.
 * See cifopen.c to see how it is initialised. */

extern int _cif_version;
#endif /* CIF_VERSION != 1 */


/* --------------------------------------------------------------------------
 * declarations of CIF internal global items
 * --------------------------------------------------------------------------
 */

/* The + 1`s allow the arrays to be indexed by CIF_VERSION which is 1 based */

extern const short _Cif_structsize[CIF_MAXRECORD][_CIF_INT_VERSION + 1];
extern const short _Cif_shortsize[CIF_MAXRECORD][_CIF_INT_VERSION + 1];

/*
 * Mapping between compiler generated object data type values and
 * those that the library wants to return. ie a set that is consistent with
 * f77 as far as possible and non-overlapping when not
 */
extern const int _Cif_f90_to_f77_dtypes[];


int _Cif_binary_map_version (int rtype, struct Cif_generic *map_buffer, struct Cif_generic *cr);
extern int _Cif_Open (char *, char *, int *, int);
extern int Cif_Open_V2 (char *filename, char *optype, int *rtypes, int version);
extern int _Cif_binread (int, int, struct Cif_generic *, FILE *);
extern int _Cif_mementry (unsigned int);
extern int _Cif_memtbl ();
extern char * (*_Cif_space[])();

#endif /* __CIFINT_H_ */

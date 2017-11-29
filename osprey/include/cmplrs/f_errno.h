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


/* $Header$ */
/*
 *
 * f77 I/O error definitions
 */

#include	<errno.h>

extern int errno;

#define F_ER		100	/* base offset of f77 error numbers */

#define F_ERFMT		100	/* error in format */
#define F_ERUNIT	101	/* illegal unit number */
#define F_ERNOFIO	102	/* formatted io not allowed */
#define F_ERNOUIO	103	/* unformatted io not allowed */
#define F_ERNODIO	104	/* direct io not allowed */
#define F_ERNOBKSP	106	/* can't backspace file */
#define F_ERNFILE	107	/* null file name */
#define F_ERDUPOPEN	109	/* file has been opened as different unit */
#define F_EREREC	110	/* off end of record */
#define F_ERTRUNC	111	/* truncation failed */
#define F_ERLIO		112	/* incomprehensible list input */
#define F_ERSPACE	113	/* out of free space */
#define F_ERNOPEN	114	/* unit not connected */
#define F_ERRDCHR	115	/* read unexpected character */
#define F_ERLOGIF	116	/* blank logical input field */
#define F_ERBVT		117	/* bad variable type in namelist*/
#define F_ERNLN		118	/* bad namelist name */
#define F_ERVNL		119	/* variable not in namelist */
#define F_ERNER		120	/* no end record */
#define F_ERNSR		121	/* namelist subscript out of range */
#define F_ERNREP	122	/* negative repeat count */
#define F_ERILLOP	123	/* illegal operation for channel or device */
#define F_ERBREC      124     /* off beginning of record */
#define F_ERREPT	125	/* no * after repeat count */
#define F_ERNEWF	126	/* 'new' file exists */
#define F_EROLDF	127	/* can't find 'old' file */
#define F_ERARG		130	/* illegal argument */
#define F_ERDUPKEY	131	/* duplicate key value on write */
#define F_ERIDXNOTOPEN	132	/* indexed file not open */
#define F_ERISAMARG	133	/* bad isam argument */
#define F_ERBADKEY	134	/* bad key description */
#define F_ERIDXFILES	135	/* too many open indexed files */
#define F_ERBADISAM	136	/* corrupted isam file */
#define F_ERISAMNOTEXCL	137	/* isam file not opened for exclusive access */
#define F_ERLOCKED	138	/* record locked */
#define F_ERKEYEXISTS	139	/* key already exists */
#define F_ERDELKEY	140	/* cannot delete primary key */
#define F_ERBOFEOF	141	/* beginning or end of file reached */
#define F_ERNOREC	142	/* cannot find requested record */
#define F_ERUNDREC	143	/* current record not defined */
#define F_ERISAMEXCL	144	/* isam file is exclusively locked */
#define F_ERFNAME	145	/* filename too long */
#define F_ERCREATELOCK	146	/* cannot create lock file */
#define F_ERRECNAME	147	/* record too long */
#define F_ERNOMATCHSTR	148	/* key structure does not match file structure */
#define F_ERDIRACCESS	149	/* direct access on an indexed file not allowed */
#define F_ERKEYACCESS_S	150	/* keyed access on a sequential file not allowed */
#define F_ERKEYACCESS_R	151	/* keyed access on a relative file not allowed */
#define F_ERAPPACCESS	152	/* append access on an indexed file not allowed */
#define F_ERUNKRECLEN	153	/* must specify record length */
#define F_ERNOMATCHVAL	154	/* key field value type does not match key type */
#define F_ERKEYLONG	155	/* character key field value length too long */
#define F_ERFIXED_S	156	/* fixed record on sequential file not allowed */
#define F_ERVAR_S	157	/* variable records allowed only on unformatted sequential file */
#define F_ERSTREAM_S	158	/* stream records allowed only on formatted sequential file */
#define F_ERTOOMANYRECS	159	/* maximum number of records in direct access file exceeded */
#define F_ERREADONLY	160	/* attempt to write to a readonly file */
#define F_ERUNKKEYDESC	161	/* must specify key descriptions */
#define F_ERCARRIAGE	162	/* carriage control not allowed for unformatted units */
#define F_ERIDXONLY	163	/* indexed files only */
#define F_ERISINDEXED	164	/* cannot use on indexed file */
#define F_ERISIDXAPP	165	/* cannot use on indexed or append file */
#define F_ERCLOSE	166	/* error in closing file */
#define F_ERINVFMT	167	/* invalid code in format specification */
#define F_ERINVRECNO	168	/* invalid record number in direct access file */
#define F_ERISNONSEQ	169	/* cannot have endfile record on non-sequential file */
#define F_ERPOSITION	170 	/* cannot position within current file */
#define F_ERSEQONDIR	171 	/* cannot have sequential records on direct access file */
#define F_ERNONAMELIST	172 	/* cannot find namelist in input file  */
#define F_ERREADSTDOUT	173 	/* cannot read from stdout */
#define F_ERWRITESTDIN	174 	/* cannot write to stdin */
#define F_ERFAILEDSTAT	175 	/* stat call failed in f77inode */

/* These are the error coded added in the libI90 implementation */
#define F_ERILLSPEC     176     /* Illegal specifer */
#define F_EREORNOPAD    177     /* EOF when PAD=NO */
#define F_EREORNOADV    178     /* EOR= specifier used with ADVANCE=NO */
#define F_ERSIZENOADV   179     /* SIZE= specifier present with ADVANCE=NO */
#define F_ERRDWRONLY    180     /* attempt to read from a writeonly file */

/* libI77 error codes */
#define F_ERNODUIO	181	/* Direct unformatted I/O not allowed. */
#define F_EROPENDIR     182     /* cannot open a directory */
#define F_ERSUBSCRIPT	183	/* subscript out of bounds */
#define F_ERNOTVARARG	184	/* function not declared as varargs */
#define F_ERINTERNAL	185	/* internal error */
#define F_ERBADINPUT    186	/* illegal input value */
#define F_ERPOSITIONUSE 187	/* position specifier is allowed only 
					for sequential files */
#define F_EPOSITIONVALUE 188	/* position specifier has an illegal value */
#define F_ERMEMORY	189	/* Memory exhausted */
#define F_ERALLOCATEDUP 190	/* already ALLOCATED (see F90 6.3.1.1) */
#define F_ERALLOCATENOT 191	/* not currently ALLOCATED (see F90 6.3.3.1) */
#define F_ERASSOCIATEDNOT 192	/* not currently ASSOCIATED (see F90 6.3.3.2) */
#define F_ERALLOCATECREATE 193	/* not created by ALLOCATE (see F90 6.3.3.2) */
#define F_ERDEALLOCTEPTR 194	/* cannot be DEALLOCATEd via a pointer 
					(see F90 6.3.3.2) */
#define F_ERKEEPSCRATCH  195	/* cannot keep a file opened as a scratch file */
#define F_TYPECONFLICT 196	/* data type conflicts with format */


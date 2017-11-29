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



/*
 *       C-ISAM version 3.00
 *  Indexed Sequential Access Method
 *  Relational Database Systems, Inc.
 */
/***********************************************************************
 *
 *  Edit history:
 *
 *  12-Sep-87   MAN     (012)
 *      Added EBIGRECN for COBOL85, used when the relative record number
 *      read, no longer fits into the COBOL data-item that is to hold it.
 *  24-Aug-87   RPL 011
 *      Add error code 166 for bad PL/I F format (PER 6060).
 *      Remove tabs from some comments.
 *  10-Aug-87   IAG 010
 *      This version is to be used with rev. 3 of ISAM. 
 *   4-Jun-87   RPL 009
 *      Move extern data and structure declarations outside of #ifdef.
 *  11-May-87   WKD 008
 *      Add new file status codes for variable length records.
 *  04-Apr-87   WKD 007
 *      Add cobol85 new file status codes.
 *  12-Mar-87   TLF 006
 *      Added comment stating that COLLATE3 code is now used by LPI-RPG
 *      to support indexed files in EBCDIC collating sequence.
 *   6-Mar-87   ABC 005
 *      Comment out extraneous tokens on 'endif' line (non-portable)
 *  21-Jan-87   PLB 004
 *      Undo macro defs for ldint/stint for STORAGE_RIGHT_TO_LEFT case
 *      (these guys are portable as is)
 *  30-Jul-86   RCG 003
 *      Corrected a #end to #endif (causing cc problems)
 *  10-Jul-86  PLB
 *      Handle macro defs for ldint/stint for 386 portability
 *      Add Error defs 142-149 to match with RTLERRORS_IN
 *      Reverse edit history
 *  30-Jan-86 nlb
 *      Add error code for not open for read
 *  21-Aug-85  RPL
 *      Add definitions to support RPG packed keys.
 *
 **********************************************************************/

#ifndef _ISAM_INCL		/* avoid multiple include problems */
#define _ISAM_INCL

#ifdef __cplusplus
extern "C" {
#endif

#define CHARTYPE	0
#define DECIMALTYPE	0
#define CHARSIZE	1

#define INTTYPE		1
#define INTSIZE		2

#define LONGTYPE	2
#define LONGSIZE	4

#define DOUBLETYPE	3
#define DOUBLESIZE	(sizeof(double))

#define FLOATTYPE	4
#define FLOATSIZE	(sizeof(float))

/*  the following 2 defines are to support RPG Packed keys:  */
#define PACKTYPE        100
#define PACKSIZE        0

#define USERCOLL(x)	((x))

#define COLLATE1	0x10
#define COLLATE2	0x20

/*  COLLATE3 is used by LPI_RPG to support indexed files in EBCDIC collating sequence  */

#define COLLATE3	0x30
#define COLLATE4	0x40
#define COLLATE5	0x50
#define COLLATE6	0x60
#define COLLATE7	0x70

#define MAXTYPE		5
#define ISDESC		0x80	/* add to make descending type	*/
#define TYPEMASK	0x7F	/* type mask			*/

#ifdef _BBN			/* BBN Machine has 10 bits/byte	*/
#define BYTEMASK  0x3FF		/* mask for one byte		*/
#define BYTESHFT  10		/* shift for one byte		*/
#else
#define BYTEMASK  0xFF		/* mask for one byte		*/
#define BYTESHFT  8		/* shift for one byte		*/
#endif

#ifndef	__ldint
#define ldint(p)	((short)(((p)[0]<<BYTESHFT)+((p)[1]&BYTEMASK)))
#define stint(i,p)	((p)[0]=(i)>>BYTESHFT,(p)[1]=(i))
#endif	/* __ldint */

#define ISFIRST		0	/* position to first record	*/
#define ISLAST		1	/* position to last record	*/
#define ISNEXT		2	/* position to next record	*/
#define ISPREV		3	/* position to previous record	*/
#define ISCURR		4	/* position to current record	*/
#define ISEQUAL		5	/* position to equal value	*/
#define ISGREAT		6	/* position to greater value	*/
#define ISGTEQ		7	/* position to >= value		*/

/* isread lock modes */
#define ISLOCK     	0x100	/* record lock			*/
#define ISWAIT		0x400	/* wait for record lock		*/
#define ISLCKW		0x500   /* ISLOCK + ISWAIT              */
#define ISSLEEPLOCK     0x1000  /* IAG                          */

/* isopen, isbuild lock modes */
#define ISAUTOLOCK	0x200	/* automatic record lock	*/
#define ISMANULOCK	0x400	/* manual record lock		*/
#define ISEXCLLOCK	0x800	/* exclusive isam file lock	*/

#define ISINPUT		0	/* open for input only		*/
#define ISOUTPUT	1	/* open for output only		*/
#define ISINOUT		2	/* open for input and output	*/
#define ISTRANS		4	/* open for transaction proc	*/
#define ISNOLOG		8	/* no loggin for this file	*/

/* audit trail mode parameters */
#define AUDSETNAME	0	/* set new audit trail name	*/
#define AUDGETNAME	1	/* get audit trail name		*/
#define AUDSTART	2	/* start audit trail 		*/
#define AUDSTOP		3	/* stop audit trail 		*/
#define AUDINFO		4	/* audit trail running ?	*/

#define MAXKEYSIZE	120	/* max number of bytes in key	*/
#define NPARTS		8	/* max number of key parts	*/

#define k_start   k_part[0].kp_start
#define k_leng    k_part[0].kp_leng
#define k_type    k_part[0].kp_type

#define ISNODUPS  000		/* no duplicates allowed	*/
#define ISDUPS	  001		/* duplicates allowed		*/
#define DCOMPRESS 002		/* duplicate compression	*/
#define LCOMPRESS 004		/* leading compression		*/
#define TCOMPRESS 010		/* trailing compression		*/
#define COMPRESS  016		/* all compression		*/
#define ISCLUSTER 020		/* index is a cluster one       */

#define ISAM_ERR_OFFSET	31
#define EDUPL     100+ISAM_ERR_OFFSET           /* Duplicate record     */
#define ENOTOPEN  101+ISAM_ERR_OFFSET           /* File not open        */
#define EBADARG   102+ISAM_ERR_OFFSET           /* Illegal argument     */
#define EBADKEY   103+ISAM_ERR_OFFSET           /* Illegal key desc     */
#define ETOOMANY  104+ISAM_ERR_OFFSET           /* Too many files open  */
#define EBADFILE  105+ISAM_ERR_OFFSET           /* Bad isam file format */
#define ENOTEXCL  106+ISAM_ERR_OFFSET           /* Non-exclusive access */
#define ELOCKED   107+ISAM_ERR_OFFSET           /* Record locked        */
#define EKEXISTS  108+ISAM_ERR_OFFSET           /* Key already exists   */
#define EPRIMKEY  109+ISAM_ERR_OFFSET           /* Is primary key       */
#define EENDFILE  110+ISAM_ERR_OFFSET           /* End/begin of file    */
#define ENOREC    111+ISAM_ERR_OFFSET           /* No record found      */
#define ENOCURR   112+ISAM_ERR_OFFSET           /* No current record    */
#define EFLOCKED  113+ISAM_ERR_OFFSET           /* File locked          */
#define EFNAME    114+ISAM_ERR_OFFSET           /* File name too long   */
#define ENOLOK    115+ISAM_ERR_OFFSET           /* Can't create lock file */



#define EDUPWARN  116+ISAM_ERR_OFFSET           /* Duplicate record (warning) */
#define ENOCRP    117+ISAM_ERR_OFFSET           /* Current record pointer undefined */
#define EBADMEM   118+ISAM_ERR_OFFSET           /* Can't alloc memory   */
#define EBADCOLL  119+ISAM_ERR_OFFSET           /* Bad custom collating */

#define EINVOP    120+ISAM_ERR_OFFSET           /* Invalid operation */
#define EBOUNDARY 121+ISAM_ERR_OFFSET           /* Boundary violation on write */
#define EBOUNDSQ  122+ISAM_ERR_OFFSET           /* Boundary violation on sq file */
#define EFNOTAVL  123+ISAM_ERR_OFFSET           /* Not available, closed w/ lock */
#define EINVOPN   124+ISAM_ERR_OFFSET           /* Inconsistent attributes */
#define EOPKYAPP  125+ISAM_ERR_OFFSET           /* Open -APPEND on KEYED file */
#define EOPILDAM  126+ISAM_ERR_OFFSET           /* Open -DAM on non-IDRECT file */
#define EOPKYSAM  127+ISAM_ERR_OFFSET           /* Open -SAM on KEYED file */
#define EILLREAD  128+ISAM_ERR_OFFSET           /* Read on output file */
#define EILLWRITE 129+ISAM_ERR_OFFSET           /* Write on input file */
#define ENOTCLOS  130+ISAM_ERR_OFFSET           /* Open on file not yet closed */
#define EKEYSEQ   131+ISAM_ERR_OFFSET           /* KEY specified on sequential file */
#define EKEYSTR   132+ISAM_ERR_OFFSET           /* KEY specified on stream file */
#define ENKYKEY   133+ISAM_ERR_OFFSET           /* No KEY specified on KEYED file */
#define ENPRINT   134+ISAM_ERR_OFFSET           /* Not a PRINT file */
#define ENSTREAM  135+ISAM_ERR_OFFSET           /* Not a STREAM file */
#define ECALLNF   136+ISAM_ERR_OFFSET           /* CALL dataname name not found */
#define EBFORMAT  137+ISAM_ERR_OFFSET           /* B-format error (35) */
#define EBIFORMAT 138+ISAM_ERR_OFFSET           /* BI format error (36) */
#define ECFORMAT  139+ISAM_ERR_OFFSET           /* C format error (32) */
#define EEFORMAT  140+ISAM_ERR_OFFSET           /* E format error (34) */
#define EIOSTACK  141+ISAM_ERR_OFFSET           /* I/O stack overflow */
#define ECONVERT  142+ISAM_ERR_OFFSET           /* Conversion error */
#define ELISTOVF  143+ISAM_ERR_OFFSET           /* LIST input item too long */
#define EIMPOPEN  144+ISAM_ERR_OFFSET           /* Implicit open failed */
#define EEOFFMT   145+ISAM_ERR_OFFSET           /* EOF found while parsing edited input */
#define EENDSTR   146+ISAM_ERR_OFFSET           /* End of string on GET/PUT STRING */
#define EINVSFMT  147+ISAM_ERR_OFFSET           /* Invalid format item for GET/PUT STRING */
#define EINVIFMT  148+ISAM_ERR_OFFSET           /* Invalid format for GET */
#define ERPGSCR   149+ISAM_ERR_OFFSET           /* Cannot initialize RPG screens */
#define ENOTOPRD  150+ISAM_ERR_OFFSET           /* Not open for read */
#define EILLREW   151+ISAM_ERR_OFFSET           /* REWRITE on input file */
#define ENOPOPEN  152+ISAM_ERR_OFFSET           /* File not OPEN on NON-OPTIONAL */
#define EOPOPEN   153+ISAM_ERR_OFFSET           /* File OPEN on OPTIONAL */
#define ESEQERR   154+ISAM_ERR_OFFSET           /* Sequential error on REW or DELE COBOL85 */
#define ESEQERNR  155+ISAM_ERR_OFFSET           /* Sequential error no next record COBOL85 */
#define EBADSIZE  156+ISAM_ERR_OFFSET           /* Bad record size on variable length record */

#define ELOGREAD  157+ISAM_ERR_OFFSET           /* Cannot read log rec  */
#define EBADLOG   158+ISAM_ERR_OFFSET           /* Bad log record       */
#define ELOGOPEN  159+ISAM_ERR_OFFSET           /* Cannot open log file */
#define ELOGWRIT  160+ISAM_ERR_OFFSET           /* Cannot write log rec */
#define ENOTRANS  161+ISAM_ERR_OFFSET           /* No transaction       */
#define ENOSHMEM  162+ISAM_ERR_OFFSET           /* No shared memory     */
#define ENOBEGIN  163+ISAM_ERR_OFFSET           /* No begin work yet    */
#define ENONFS    164+ISAM_ERR_OFFSET           /* Can't use nfs        */
#define EAUDIT    165+ISAM_ERR_OFFSET           /* Audit trail exists   */

#define EFFORMAT  166+ISAM_ERR_OFFSET           /* F format error       */
#define EBIGRECN  167+ISAM_ERR_OFFSET           /* Record number tobig for data item */
#ifdef sgi
#define ELONGNAME 168+ISAM_ERR_OFFSET           /* ISAM guest account and/or host names too long */
#endif

/* NOTE: Any error codes added to the end of the list above MUST be  */
/*       duplicated in errmap.h for COBOL error mapping and in       */
/*       rtlerrs.in for use by PL/I runtimes.  Corresponding changes */
/*       should be recorded in the language User's Guides.           */


/*
 * For system call errors
 *   iserrno = errno (system error code 1-99)
 *   iserrio = IO_call + IO_file
 *		IO_call  = what system call
 *		IO_file  = which file caused error
 */

#define IO_OPEN	  0x10		/* open()	*/
#define IO_CREA	  0x20		/* creat()	*/
#define IO_SEEK	  0x30		/* lseek()	*/
#define IO_READ	  0x40		/* read()	*/
#define IO_WRIT	  0x50		/* write()	*/
#define IO_LOCK	  0x60		/* locking()	*/
#define IO_IOCTL  0x70		/* ioctl()	*/

#define IO_IDX	  0x01		/* index file	*/
#define IO_DAT	  0x02		/* data file	*/
#define IO_AUD	  0x03		/* audit file	*/
#define IO_LOK	  0x04		/* lock file	*/
#define IO_SEM	  0x05		/* semaphore file */

#define AUDHEADSIZE  14		/* num of bytes in audit header	*/

#ifdef __cplusplus
}
#endif

#ifndef	__ldlong
int ldlong(char *);
#endif	/* __ldlong */

#ifndef _NOFLOAT
#ifndef	__ldfloat
double	ldfloat(char *);
#endif	/* __ldfloat */
#ifndef	__lddbl
double	lddbl(char *);
#endif	/* __lddbl */
double ldfltnull(char *, short *);
double lddblnull(char * , short *);
#endif

struct keypart
    {
    short kp_start;		/* starting byte of key part	*/
    short kp_leng;		/* length in bytes		*/
    short kp_type;		/* type of key part		*/
    };

struct keydesc
    {
    short k_flags;		/* flags			*/
    short k_nparts;		/* number of parts in key	*/
    struct keypart
	k_part[NPARTS];		/* each key part		*/
		    /* the following is for internal use only	*/
    short k_len;		/* length of whole key		*/
    int k_rootnode;		/* pointer to rootnode		*/
    };

struct dictinfo
    {
    short di_nkeys;		/* number of keys defined	*/
    short di_recsize;		/* data record size		*/
    short di_idxsize;		/* index record size		*/
    int di_nrecords;		/* number of records in file	*/
    };

extern int iserrno;		/* isam error return code	*/
extern int iserrio;		/* system call error code	*/
extern int isrecnum;		/* record number of last call	*/
extern char isstat1;		/* cobol status characters	*/
extern char isstat2;
extern char *isversnumber;	/* C-ISAM version number	*/
extern char *isserial;		/* C-ISAM software serial number */
extern int  issingleuser;	/* set for single user access	*/
extern int  is_nerr;		/* highest C-ISAM error code	*/
extern char *is_errlist[];	/* C-ISAM error messages	*/
/*  error message usage:
 *	if (iserrno >= 100 && iserrno < is_nerr)
 *	    printf("ISAM error %d: %s\n", iserrno, is_errlist[iserrno-100]);
 */

struct audhead
    {
    char au_type[2];		/* audit record type aa,dd,rr,ww*/
    char au_time[4];		/* audit date-time		*/
    char au_procid[2];		/* process id number		*/
    char au_userid[2];		/* user id number		*/
    char au_recnum[4];		/* record number		*/
    };

/* Added for prototyping for use in other directories, like libI77 */
extern int mkidxname(char *, char *);
extern int mkdatname(char *, char *);
extern int mklokname(char *, char *);

#endif /* ISAM_INCL */

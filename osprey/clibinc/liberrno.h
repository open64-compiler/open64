/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/* USMID @(#) clibinc/liberrno.h	92.7	11/09/99 14:39:08 */

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

#ifndef	_LIBERRNO_H
#define	_LIBERRNO_H

#ifdef	_LITTLE_ENDIAN
#include <asm/errno.h>
#else
#include <sys/errno.h>
#endif

/*
 * Symbolic library run-time error codes
 *
 *
 * These error codes start at a large number so as to not overlap with
 * system error codes (<sys/errno.h>).  Therefore, there is no need to
 * map system error codes into library run-time error codes.
 *
 * Because error numbers are compiled into existing binary programs, and
 * because they are documented in several places, existing error numbers
 * should not be substantially changed (fixing spelling errors and general
 * re-wording is acceptable as long as the basic meaning is unchanged).
 * Changing the number or type of substitutable parameters between releases
 * is strictly forbidden.  Symbolic names for error message numbers are
 * provided as a means to easily cross-reference errors and not as a means
 * to change their numbers.
 *
 * Messages are grouped by origin and type, with gaps between groups for
 * future expansion.  Thus, related message can be documented together
 * (e.g., "The following messages apply to OPEN processing...").
 *
 * If a message must be substantially changed, leave the old message in
 * and add a new message.  For example, suppose we have an existing message
 * number 4444 with the symbolic name of FEIFNQ and the text of the message
 * is:
 *	"File name on OPEN must begin with the letter Q"
 *
 * Sometime later, this restriction is changed such that the file name must
 * contain only the letters Q.  Since this substantially changes the meaning
 * of the error message, we do the following:
 *
 *	1) Sever the association between the symbolic name FEIFNQ and 4444,
 *	   by changing the definition in this file:
 *		#define FEIFNQ   4444	File name must begin with Q
 *	   becomes:
 *		#define FE__4444 4444	DEAD MESSAGE (n.n) - DO NOT REUSE
 *
 *         where 'n.n' is the first release level (of UNICOS or CrayLibs) 
 *	   where the message was no longer issued.
 *
 *	2) Create a new symbolic name (we can reuse the symbol FEIFNQ if it
 *	   is convenient, but we don't have to) with some other number:
 *		#define FEIFNQ  nnnn	File name must have only Qs
 *
 *	3) Change the old message in the message source file 
 *	   (libu/errmsg/lib.msg):
 *		$msg FEIFNQ File name on OPEN must begin with the letter Q
 *	   becomes:
 *		$msg FE__4444 File name on OPEN must begin with the letter Q
 *
 *	4) Add the 'new' message to the message source file:
 *		$msg FEIFNQ File name on OPEN must contain only the letter Q
 *
 * The 'DEAD MESSAGE' note in this file and the hard-coded constant in the
 * message source file signal the existence of dead messages.  Let them
 * rest in peace (at least for a release or two).
 *
 * Existing binary programs will continue to get the messages that apply to
 * them.
 */

#ifndef	SYMSONLY
/*
 * Any function prototypes or other definitions which are not needed for
 * inclusion when lib.msg is processed to generate the .cat file should
 * be located here.  The SYMSONLY symbol is defined when cpp is invoked
 * for lib.msg. 
 */

#include <stdarg.h>
#include <sys/cdefs.h>

__BEGIN_DECLS
extern	void	_lerror		(int _Hndlcode, int _Errno, ...);
extern	void	_lmessage	(int _Errno, char *_Severity, va_list args);
extern	int	_fwarn		(int _Errno, ...);
extern	int	_lwarn		(int _Errno, ...);
#ifdef KEY /* Bug 7479 */
extern void _lcomment(int, ...);
#endif /* KEY Bug 7479 */
#ifdef KEY /* Bug 6673 */
extern int verbose_message(char *, int);
#endif /* KEY Bug 6673 */
__END_DECLS

#define	MAXMLN	800		/* Maximum length of a message in bytes	*/

#if	defined(_CRAYT3D) && defined(_UNICOS_MAX)
#define	FEMCN	"mpplib"	/* Library Error Message Catalog Name	*/
#else
#define	FEMCN	"lib"		/* Library Error Message Catalog Name	*/
#endif

/*
 * Error levels are defined for the benefit of function _lerror().  The
 * error handling mode determines what kind of information should be printed
 * when a program aborts with an error. 
 */

#define _LELVL_ABORT	4	/* Message, abort and traceback		*/
#define _LELVL_EXIT	3	/* Message, exit with nonzero status	*/
#define _LELVL_MSG	2	/* Message, then return error status	*/
#define _LELVL_RETURN	0	/* No message, just return error status	*/

#endif	/* !SYMSONLY */

/*
 * Define the range for the Fortran run-time library error messages,
 * based on sys/errno.h.
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#ifndef	__FTNBASE	/* __FTNBASE defined at kudzu level */
#define	BASE	4000
#else
#define	BASE	__FTNBASE
#endif
#elif	_UNICOS
#define	BASE	EFLEFIRST
#else
#define	BASE	1000
#endif

/*
 * The range BASE to BASE+999 is reserved for Fortran I/O.
 */
 
#define FERDPEOF -(BASE+1)	/* Tried to read past end of file	*/
#ifdef KEY /* F2003 */
/* ISO/IEC 1539-1:2004 13.8.2.5 requires a single value for any EOF */
#define FERDEMPT (FERDPEOF)	/* Tried to read an empty file		*/
#define FERDENDR (FERDPEOF)	/* Tried to read past endfile record	*/
#define FERDNLEF (FERDPEOF)	/* Tried to read past EOF on namelist	*/
#define FERDIEOF (FERDPEOF)	/* Tried to read past internal file EOF */
#else /* KEY F2003 */
#define FERDEMPT -(BASE+2)	/* Tried to read an empty file		*/
#define FERDENDR -(BASE+3)	/* Tried to read past endfile record	*/
#define FERDNLEF -(BASE+4)	/* Tried to read past EOF on namelist	*/
#define FERDIEOF -(BASE+5)	/* Tried to read past internal file EOF */
#endif /* KEY F2003 */

#define FEEORCND -(BASE+6)	/* Read past EOR with ADVANCE='NO'	*/

#define FE____10 (BASE+10)	/* DEAD MESSAGE (7.3) -- DO NOT RE-USE	*/
#define FE____11 (BASE+11)	/* DEAD MESSAGE (7.3) -- DO NOT RE-USE	*/

#define FEIVUNIT (BASE+12)	/* Invalid unit number			*/
#define FEIVUNTO (BASE+13)	/* Invalid unit number on OPEN		*/
#define FENOTOPN (BASE+14)	/* Unit is not connected 		*/
#define FEIVRECN (BASE+15)	/* Invalid record number (%d)		*/
#define FENORECN (BASE+16)	/* Record number does not exist in file	*/
 
#define FECONNDA (BASE+21)	/* Unit not opened for direct access	*/
#define FECONNTP (BASE+22)	/* Unit not connected to tape		*/
#define FE____23 (BASE+23)	/* DEAD MESSAGE (7.3) -- DO NOT RE-USE	*/
#define FE____24 (BASE+24)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define FEOPOTHR (BASE+25)	/* File is opened with another structure*/
#define FEOPAUXT (BASE+26)	/* File is opened by an auxiliary i/o	*/
#define FEFILACT (BASE+27)	/* ACTION= conflicts with file perms	*/
#define FENOSKPB (BASE+28)	/* File does not support SKIPBAD	*/
#define FESTIOER (BASE+29)	/* Error on underlying stdio request	*/

#define FEOPSTAT (BASE+30)	/* Unknown STATUS parameter on OPEN	*/
#define FEOPACCS (BASE+31)	/* Unknown ACCESS parameter on OPEN	*/
#define FEOPFORM (BASE+32)	/* Unknown FORM parameter on OPEN	*/
#define FEOPRECL (BASE+33)	/* Unknown RECL parameter on OPEN	*/
#define FEOPBLNK (BASE+34)	/* Unknown BLANK parameter on OPEN	*/
#define FEOPPOSN (BASE+35)	/* Unknown POSITION parameter on OPEN	*/

#define FEOPACTB (BASE+38)	/* Unknown ACTION specifier on OPEN	*/
#define FEOPDLMB (BASE+39)	/* Unknown DELIM specifier on OPEN	*/

#define FEOPFNRQ (BASE+40)	/* FILE specifier required on OPEN	*/
#define FEOPFNIV (BASE+41)	/* FILE specifier invalid on OPEN	*/
#define FEOPRCRQ (BASE+42)	/* RECL specifier required on OPEN	*/

#define FEOPBKIV (BASE+44)	/* BLANK specifier invalid on OPEN	*/
#define FEOPPSIV (BASE+45)	/* POSITION specifier invalid on OPEN	*/
#define FEOPASGN (BASE+46)	/* ASSIGN/ASGCMD conflict		*/
#define FE____47 (BASE+47)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define FEOPPADB (BASE+48)	/* Unknown PAD specifier on OPEN	*/
#define FEOPDLMI (BASE+49)	/* DELIM specifier invalid on OPEN	*/

#define FE____50 (BASE+50)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define FEOPFNNX (BASE+51)	/* File must not exist prior to OPEN	*/
#define FEOPFNCN (BASE+52)	/* File is connected to another unit	*/

#define FEOPCBNK (BASE+54)	/* Only BLANK can be changed on reopen	*/
#define FEOPNOFS (BASE+55)	/* File cannot be opened (structure)	*/
#define FEOPNNDA (BASE+56)	/* File cannot be opened for direct acc.*/
#define FEOPCAPY (BASE+57)	/* File cannot be opened per dgl's specs*/
#define FEOPNNEW (BASE+58)	/* STATUS=NEW on currently-open-file	*/
#define FEOPIVRL (BASE+59)	/* Incorrect RECL for existing d.a. file*/

#define FEOPSTFN (BASE+60)	/* Attempt to OPEN standard file wrong	*/
#define FEOPAQPZ (BASE+61)	/* Invalid AQPSIZE parameter in AQOPEN 	*/
#define FE____62 (BASE+62)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define FEWTOMNY (BASE+63)	/* Too many open word addressable files */
#define FEWABLKS (BASE+64)	/* Too many blocks requested in WINIT	*/
#define FERQTRNC (BASE+65)	/* file requires truncation after write */
#define FE____66 (BASE+66)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define FEOPPDIV (BASE+67)	/* PAD specifier invalid on OPEN	*/
#define FEOPNUNF (BASE+68)	/* File cannot be opened for unfmtd acc */
#define FEOPNFMT (BASE+69)	/* File cannot be opened for fmtd acc	*/

#define FECLSTAT (BASE+70)	/* Unknown STATUS parameter on CLOSE	*/
#define FECLSTIV (BASE+71)	/* Invalid STATUS parameter on CLOSE	*/
#define FEINCZER (BASE+72)	/* Increment in implied-do is zero	*/
#define FEAQBADH (BASE+73)	/* AQIO call with invalid file handle	*/
#define FEMDUPOP (BASE+74)	/* File connected to unit with "-m off" */
#define FEANOSUP (BASE+75)	/* assign opt unsupported by file type	*/
#define FEMIXSCP (BASE+76)	/* can't open unit private and global   */
#define FENOGLOB (BASE+77)	/* -P global not supported 		*/
#define FENOPRIV (BASE+78)	/* -P private not supported 		*/
#define FEBSPNRD (BASE+79)	/* BACKSPACE requires read permission	*/
#define FEFMTTIV (BASE+80)	/* Formatted I/O invalid on unformatted	*/
#define FEUNFMIV (BASE+81)	/* Unformatted I/O invalid on formatted	*/
#define FEDIRTIV (BASE+82)	/* Direct access I/O invalid on seq.	*/
#define FESEQTIV (BASE+83)	/* Sequential I/O invalid on direct acc.*/
#define FEBKSPIV (BASE+84)	/* BACKSPACE invalid on direct access	*/
#define FEENDFIV (BASE+85)	/* ENDFILE invalid on direct access	*/
#define FERWNDIV (BASE+86)	/* REWIND invalid on direct access	*/
#define FERDAFWR (BASE+87)	/* Read after write invalid on seq.	*/
#define FE____88 (BASE+88)	/* DEAD MESSAGE (7.3) -- DO NOT RE-USE	*/

#define FENOREAD (BASE+90)	/* No read permission			*/
#define FENOWRIT (BASE+91)	/* No write permission			*/
#define FENOBKSP (BASE+92)	/* File does not support BACKSPACE	*/
#define FENOENDF (BASE+93)	/* File does not support ENDFILE	*/
#define FE____94 (BASE+94)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define FEWRAFEN (BASE+95)	/* WRITE or PRINT invalid after ENDFILE	*/
#define FEENAFEN (BASE+96)	/* ENDFILE invalid after ENDFILE	*/
#define FE____97 (BASE+97)	/* DEAD MESSAGE (7.3) -- DO NOT RE-USE	*/
#define FENOBKPI (BASE+98)	/* Unable to BACKSPACE pipe		*/
#define FENORWPI (BASE+99)	/* Unable to REWIND pipe		*/

#define FE___100 (BASE+100)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define FENODELT (BASE+101)	/* Unable to delete file on CLOSE	*/

#define FENOTHRD (BASE+103)	/* -P thread not supported 		*/
#define FENOTEAM (BASE+104)	/* -P team not supported 		*/

#define FEFMTILF (BASE+117)	/* Infinite loop in format		*/
#define FEFMTLII (BASE+118)	/* Literal invalid in input format	*/
#define FEFMTQIO (BASE+119)	/* Q edit-desc. invalid in output format*/

#define FENOSKPF (BASE+140)	/* File does not support SKIPF 		*/

#define FERDTYPE (BASE+170)	/* Data type mismatch on READ		*/
#define FEWRTYPE (BASE+171)	/* Data type mismatch on WRITE		*/

#define FERDIVLG (BASE+173)	/* Invalid logical input field		*/

#define FELDUNKI (BASE+180)	/* Unknown input on list-directed read	*/
#define FELDNOCX (BASE+181)	/* Invalid complex on list-directed read*/
#define FELDSTRL (BASE+182)	/* String too long on list-directed read*/

#define FENICVIC (BASE+190)	/* Invalid character in numeric input	*/
#define FENICVOF (BASE+191)	/* Overflow converting numeric input	*/
#define FENICVEU (BASE+192)	/* Exponent underflow on numeric input	*/
#define FENICVEO (BASE+193)	/* Exponent overflow on numeric input	*/
#define FENICVBK (BASE+194)	/* Blank numeric input field		*/

#ifdef KEY /* F2003 */
/* ISO/IEC 1539-1:2004 13.8.2.5 requires a single value for any EOR */
#define FERDPEOR (FEEORCND)	/* Tried to read past end of record	*/
#else /* KEY F2003 */
#define FERDPEOR (BASE+201)	/* Tried to read past end of record	*/
#endif /* KEY F2003 */
#define FERDWRER (BASE+202)	/* Read/wrote too little data 		*/
#define FERDMEMY (BASE+203)	/* Unable to get memory for fmt'd read	*/

#define FENOMEMY (BASE+205)	/* Unable to request more memory space	*/
#define FENOSDSP (BASE+206)	/* Unable to request more SDS space	*/

#define FEIOACTV (BASE+208)	/* An I/O statement was already active	*/
#define FESDSFSS (BASE+209)	/* SDS file size must be specified	*/

#define FEWRLONG (BASE+211)	/* Tried to write a too long record	*/
#define FEWRIEND (BASE+212)	/* Tried to write beyond internal file	*/
#define FEPTRNAS (BASE+213)	/* Ptr/alloc array not assoc/alloc'ed	*/
#define FEFMTPAL (BASE+214)	/* FMT var not allocated or associated	*/
#define FEUNOTAL (BASE+215)	/* UNIT var not allocated or associated	*/
#define FEFMTNUL (BASE+216)	/* FMT var or array is zero-sized	*/
#define FERDMALR (BASE+217)	/* Read encountered a malformed record	*/

#define FEINTUNK (BASE+220)	/* Internal Fortran library error	*/
#define FEINTFST (BASE+221)	/* Internal error - unknown file struct.*/

#define FEINTDTY (BASE+223)	/* Internal error - unknown data type	*/
#define FEINTIPF (BASE+224)	/* Internal error invalid parsed format */

#define FEINTTAP (BASE+226)	/* Internal error on tape read		*/

/*
 * The range BASE+275 to BASE+329 is reserved for namelist I/O.
 */

#define FENLSTRN (BASE+295)	/* Null substring, nl read		*/
#define FENLSTRG (BASE+296)	/* Bad substring contents, nl read	*/
#define FENLSUBN (BASE+297)	/* Null subscript, nl read		*/
#define FENLSUBD (BASE+298)	/* Bad char after subscript, nl read	*/
#define FENLSUBS (BASE+299)	/* Bad char in subscript value, nl read	*/

#define FENLLGNM (BASE+303)	/* NL variable name too long + name	*/
#define FENLIVGP (BASE+304)	/* NL input group name mismatch + name	*/
#define FENLIVIT (BASE+305)	/* Input float data to INTEGER type	*/
#define FENLONEC (BASE+306)	/* First/last character unknown nl read */
#define FENLUNKI (BASE+307)	/* Unknown input on namelist read 	*/
#define FENLZRCH (BASE+308)	/* Zero length char in nl for cf90/cf77 */
#define FENLARSC (BASE+309)	/* Array section input to cf90/cf77 nml */
#define FENLIOER (BASE+310)	/* Namelist read error			*/

#define FENLTYPE (BASE+312)	/* Invalid char passed to namelist rtn.	*/
#define FENLLONG (BASE+313)	/* Namelist variable name too long	*/
#define FENLIVGN (BASE+314)	/* Namelist input group name mismatch	*/
#define FENLUNKN (BASE+315)	/* Unrecognized namelist variable name	*/
#define FENLNOVL (BASE+316)	/* Unable to obtain namelist value	*/
#define FENLIVLG (BASE+317)	/* Invalid logical data in namelist read*/
#define FENLIVCX (BASE+318)	/* Invalid complex data in namelist read*/

#define FENLRECL (BASE+320)	/* Input rec. too long on namelist read	*/
#define FENLBNDY (BASE+321)	/* Attempted namelist read beyond array	*/

#define FENLTOOM (BASE+323)	/* Too many namelist elements specified */
#define FENLNREC (BASE+324)	/* Unrecognized namelist variable name	*/
#define FENLTYPI (BASE+325)	/* Data type mismatch on namelist read	*/
#define FENLNMSZ (BASE+326)	/* Namelist name is larger than recsize */
#define FENLDBCP (BASE+327)	/* Double complex illegal for f77 mode	*/
#define FENLSTCT (BASE+328)	/* Structures illegal for f77 mode	*/
#define FENLPRAM (BASE+329)	/* Bad pre-ampersand character in f90	*/

#define FEBIONDA (BASE+330)	/* Direct access file invalid for BUFIO	*/
#define FEBIONFM (BASE+331)	/* Formatted file invalid for BUFIO	*/
#define FEBIOFWA (BASE+332)	/* Start address > end address for BUFIO*/

#define FEBIOISP (BASE+334)	/* Invalid argument to SETPOS		*/
#define FEBIOSNT (BASE+335)	/* Positioning operation not supported	*/

#define FEMIXBUF (BASE+338)	/* Mixing BUFIO/READ/WRITE on pure file	*/
#define FEMIXAUX (BASE+339)	/* Mixing auxiliary and Fortran I/O	*/

#define FEDECDRL (BASE+340)	/* Invalid DECODE record length		*/
#define FEENCDRL (BASE+341)	/* Invalid ENCODE record length		*/
#define FEBIOFWD (BASE+342)	/* Invalid number of items for BUFIO	*/

#define FEADVSPC (BASE+343)	/* Invalid ADVANCE= specifier on rd/wrt	*/
#define FEADVSIZ (BASE+344)	/* ADVANCE='NO' required with SIZE=	*/
#define FEADVEOR (BASE+345)	/* ADVANCE='NO' required with EOR=	*/

#define FETAPNBN (BASE+350)	/* Negative tape block number is invalid*/
#define FETAPNBS (BASE+351)	/* Invalid NBS parameter		*/
#define FETAPNVS (BASE+352)	/* Invalid NVS parameter		*/

#define FETAPBSX (BASE+354)	/* Maximum tape block size exceeded	*/
#define FETAPCMB (BASE+355)	/* Invalid combination of parameters	*/
#define FETAPUTE (BASE+356)	/* Unrecovered tape error on tape read	*/
#define FETASKPF (BASE+357)	/* Cannot SKIPF forward			*/
#define FETAPNVY (BASE+358)	/* Invalid NVS or NV parameter 		*/

#define FETBARGS (BASE+360) 	/* Tblmgr routine called with bad args. */
#define FETBNTAB (BASE+361) 	/* Tblmgr routine called with bad NTAB	*/
#define FETBTNUM (BASE+362) 	/* Tblmgr routine called with bad tab. #*/
#define FETBINCR (BASE+363) 	/* Tblmgr routine called with bad incr. */

#define FEUBCINV (BASE+370)	/* Read or write of nonbyte-data is inv.*/
#define FELDDCNV (BASE+371)	/* Data conversion routine not loaded	*/
#define FENCNV90 (BASE+372)	/* Can't convert this type with f90	*/
#define FEKNTSUP (BASE+373)	/* I/O not supported for this KIND 	*/
#define FESHRSUP (BASE+374)	/* Shared variables inv. with this I/O	*/
#define FENOICNV (BASE+375)	/* Data conversion not sup on this unit */

#define FEARGLST (BASE+380)	/* Argument list is not valid		*/

#define FEKLUDG1 (BASE+391)	/* System error 1			*/
#define FEKLUDG2 (BASE+392)	/* System error 2			*/
#define FEKLUDG3 (BASE+393)	/* System error 3			*/
#define FEKLUDG4 (BASE+394)	/* System error 4			*/

#define FEARGCNT (BASE+402)	/* Routine called with wrong # of args.	*/
#define FEARGSHP (BASE+403)	/* Actual/dummy argument shape conflict */

/*
 * The range BASE+405 to BASE+499 is reserved for Fortran-90 intrinsics and 
 * Fortran-90 statement support.
 */

#define FESCIDIM (BASE+405)	/* Array intrin. has illegal DIM arg. 	*/
#define FESCIRNK (BASE+406)	/* Illegal rank for array intrin. arg. 	*/
#define FESCICNF (BASE+407)	/* Matrix operands are not conformable. */
#define FERSHNPD (BASE+408)	/* RESHAPE srcsize.lt.moldsize, no PAD	*/
#define FERSHNEG (BASE+409)	/* RESHAPE arg SHAPE has negative value */
#define FEPTRARR (BASE+410)	/* Source arg not associated | allocated*/
#define FEALALLO (BASE+411)	/* ALLOCATE array already allocated	*/
#define FENODEAL (BASE+412)	/* DEALLOCATE arg not alloc or ptr-alloc*/
#define FEDEALSZ (BASE+413)	/* DEALLOCATE arg not allocated size	*/
#define FERPTNEG (BASE+414)	/* negative ncopies argument to REPEAT	*/
#define FENEARZS (BASE+415)	/* NEAREST argument S is 0.0		*/
#define FEIPOWZR (BASE+416)	/* Two zero arguments to POWER routine	*/
#define FEBDORDR (BASE+417)	/* Bad value in ORDER for RESHAPE	*/
#define FEBADMLD (BASE+418)	/* Result of TRANSFER <= size of source	*/
#define FESHPSZZ (BASE+419)	/* Shape array to RESHAPE is 0-sized	*/
#define FEVECUNP (BASE+420)	/* Too few elts in UNPACK vector array	*/
#define FENGFLCL (BASE+421)	/* GETFIRST array not allocated locally */
#define FEDEASIZ (BASE+422)	/* DEALLOC argsize mismatch,give sizes	*/
#define FENMPTAR (BASE+423)	/* Source arg not associated:allocated	*/
#define FENMSCDM (BASE+424)	/* nm Array intrin. has illegal DIM arg	*/

/*
 * The range BASE+500 to BASE+599 is reserved for assign processing.
 *	500-579	Assign errors
 *	580-599	Assign warnings
 */

#define ERAS_UNFILE (BASE+500)	/* Can't read global environment file	*/
#define ERAS_WRERR (BASE+501)	/* Can't write global environment file	*/

#define ERAS_ATTFMT (BASE+503)	/* Bad format of assign attributes in	*/
				/* assign environment file		*/
#define FE___504 (BASE+504)	/* DEAD MESSAGE (3.0) -- DO NO RE-USE	*/
#define FE___505 (BASE+505)	/* DEAD MESSAGE (3.0) -- DO NO RE-USE	*/
#define FE___506 (BASE+506)	/* DEAD MESSAGE (3.0) -- DO NO RE-USE	*/
#define ERAS_FSNSUP (BASE+507)	/* File structure not supported		*/
#define ERAS_MIXFS (BASE+508)	/* Invalid mixing of -F and -s options	*/
#define ERAS_MIXFO (BASE+509)	/* Invalid mix of -F with -b, -p, etc.	*/
#define FE___510 (BASE+510)	/* DEAD MESSAGE (1.2) -- DO NOT RE-USE	*/
#define ERAS_ASNCTL (BASE+511)	/* Invalid arguments passed to ASNCTL	*/
#define FE___512 (BASE+512)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define ERAS_BADTYPE (BASE+513)	/* Unrecognized assign object		*/
#define ERAS_BADUNIT (BASE+514)	/* Unrecognized assign unit object	*/
#define ERAS_INCOV (BASE+515)	/* -I and -O cannot both be provided	*/
#define ERAS_REMV (BASE+516)	/* -V and -R cannot both be provided	*/
#define ERAS_REMAT (BASE+517)	/* -R cannot be provided with attrs	*/
#define ERAS_VIEWAT (BASE+518)	/* -V cannot be provided with attrs	*/
#define FE___519 (BASE+519)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define ERAS_NOOBJS (BASE+520)	/* No assign objects specified		*/
#define FE___521 (BASE+521)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define FE___522 (BASE+522)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define ERAS_DIRFMT (BASE+523)	/* Assign directive has bad format	*/
				/* in assign file or CALL ASSIGN	*/
#define ERAS_FNBL (BASE+524)	/* ASNFILE 1st arg has imbedded blnks	*/
#define ERAS_ATTRP (BASE+525)	/* Invalid -p option value on assign 	*/
#define ERAS_COREQN (BASE+526)	/* -c requires -n to be specified	*/
#define ERAS_FILENV (BASE+527)	/* FILENV must be set when TMPDIR isn't	*/
#define ERAS_NOPOP (BASE+528)	/* ASNCTL('POP') called w/ empty stack	*/
#define ERAS_MIXAD (BASE+529)	/* Invalid mixing of -a with -D options	*/
#define ERAS_ATTSPC (BASE+530)	/* No space for asn attribs in char var */
#define ERAS_BADRECF (BASE+531)	/* Bad assign record in assign env file */
#define ERAS_BADRECE (BASE+532)	/* Bad assign/asgcmd record in penv	*/
#define ERAS_NULLVNAM (BASE+533) /* Environment variable name is null	*/
#define ERAS_PATCONF (BASE+534)	/* Matched >1 filename pattern		*/
#define ERAS_MTRUNC (BASE+535)	/* "-m on" not valid with "-T on"	*/
#define ERAS_FILENVPOS (BASE+536)	/* FILENV not set (non-UNICOS systems)*/
#define ERAS_BADCLASS (BASE+537)	/* Bad -F class			*/
#define ERAS_BADOPT (BASE+538)		/* Bad option with valid -F class */
#define ERAS_CONFLICT (BASE+539)	/* Conflicting options on -F	*/
#define ERAS_BADSYNT (BASE+540)	/* Bad syntax with -F			*/
#define ERAS_OPTREQ (BASE+541)	/* Option is required w. this -F class  */
#define ERAS_PSTRING (BASE+542)	/* -F specification is too long		*/
#define ERAS_TOKUNI (BASE+543)	/* Invalid units with -F		*/
#define ERAS_BADMBS (BASE+544)	/* Bad mbs with -F 			*/
#define ERAS_INMAX (BASE+545)	/* Bad initial maximum value combination*/
#define ERAS_BADREC (BASE+546)	/* Bad record with -F			*/
#define ERAS_BADRFMT (BASE+547)	/* Bad record format			*/
#define ERAS_NUMREQ (BASE+548)	/* Numeric value required		*/
#define ERAS_TOOBIG (BASE+549)	/* A numeric option is too big 		*/
#define ERAS_TOOSMALL (BASE+550)	/* A numeric option is too small*/
#define ERAS_TOOLAY (BASE+551)	/* Too many layers specified		*/
#define ERAS_OPTVAL1 (BASE+552)	/* Invalid attribute option value	*/
#define ERAS_OPTVAL2 (BASE+553)	/* Invalid attribute option value - %s	*/
#define ERAS_UNOPT (BASE+554)	/* Unrecognized option			*/
#define ERAS_LRAW (BASE+555)	/* -l not valid with -o_raw or -o_ldraw */

#define WNAS_ANSUPY (BASE+580)	/* Option not supported on CX/1/CEA	*/
#define FE___583 (BASE+583)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define FE___584 (BASE+584)	/* DEAD MESSAGE (1.2) -- DO NOT RE-USE	*/
#define FE___585 (BASE+585)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define WNAS_DEPASDS (BASE+586)	/* Use of "-a SDS" is deprecated	*/
#define WNAS_UBMX (BASE+587)	/* "-u" requires tape device on CX/1/CEA*/
#define FE___588 (BASE+588)	/* DEAD MESSAGE (3.0) -- DO NOT RE-USE	*/
#define WNAS_QNSTRD (BASE+589)	/* -q and -n :stride conflict; using -q */
#define WNAS_NUMERIC (BASE+590)	/* Too many numerics specified		*/

/*
 * The range BASE+600 to BASE+650 is reserved for Format errors
 * (see cray/format.h)
 */

#define FEFMTBAS (BASE+611)	/* Base for all format errors		*/

#define FEFMTELP (BASE+611)	/* Expecting left parenthesis		*/
#define FEFMTERP (BASE+612)	/* Expecting right parenthesis		*/
#define FEFMTEIN (BASE+613)	/* Expecting integer			*/
#define FEFMTEPE (BASE+614)	/* Expecting period			*/
#define FEFMTEPX (BASE+615)	/* Expecting P or X			*/
#define FEFMTIRP (BASE+616)	/* Invalid repetition count		*/
#define FEFMTZRP (BASE+617)	/* Zero repetition count		*/
#define FEFMTZFW (BASE+618)	/* Zero field width			*/
#define FEFMTFTL (BASE+619)	/* Field too large			*/
#define FEFMTZMH (BASE+620)	/* Zero or missing hollerith count	*/
#define FEFMTIED (BASE+621)	/* Invalid edit descriptor		*/
#define FEFMTNLS (BASE+622)	/* Nonterminated literal string		*/
#define FEFMTMEM (BASE+623)	/* Unable to allocate memory		*/

/*
 * The range BASE+691 to BASE+699 is reserved for PACK/UNPACK errors
 */

#define FEPCKARG (BASE+691)	/* PACK/UNPACK called with too few args.*/
#define FEPCKNEG (BASE+692)	/* PACK/UNPACK called with count < 0	*/
#define FEPCKPW2 (BASE+693)	/* PACK/UNPACK called with invalid NBITS*/



#define BMMVLCHK (BASE+800)	/* Bad VL in BMM simulation routines	*/

/*
 * The range BASE+850 to BASE+899 is for POSIX 1003.9 errors	
 */

#define ENONAME	(BASE+850)	/* Invalid struct, const. or comp. name */
#define ENOHANDLE (BASE+851)	/* Handle not created			*/
#define ETRUNC	(BASE+852)	/* Out character argument was truncated	*/
#define EARRAYLEN (BASE+853)	/* No. of array elements exceeds IALEN	*/
#define EEND	(BASE+854)	/* End of file, record or directory	*/
#define EBADHANDLE (BASE+855)     /* Invalid handle ID or type. Not part
				   of standard                          */
#define EBADID (BASE+856)       /* Invalid ID. Not part of standard.    */

/*
 * The range BASE+900 to BASE+999 is for library warning (nonfatal) messages.
 */

#define FWDEFBSZ (BASE+907)	/* DEFBUFSIZ environment var ignored	*/

#define FWFUNKTP (BASE+931)	/* KIND type param conflict, CALL - f90 */
#define FWFUNRNK (BASE+932)	/* Array RANK conflict for CALL   - f90 */
#define FWFUNSIZ (BASE+933)	/* Array SIZE conflict for CALL   - f90 */
#define FWFUNCHL (BASE+934)	/* Character Length conflict, CALL- f90 */
#define FWFUNDVT (BASE+935)	/* Derived Types mismatch for CALL- f90 */
#define FWFUNNPT (BASE+936)	/* PTR func but no PTR attr, CALL - f90 */
#define FWFUNPTR (BASE+937)	/* PTR attr but no PTR func, CALL - f90 */
#define FWARGOPT (BASE+938)	/* Nonoptional arg missing, CALL  - f90 */
#define FWARGKTP (BASE+939)	/* KIND type conflict for argument- f90 */
#define FWARGDVT (BASE+940)	/* Derived Type conflict for arg  - f90 */
#define FWARGOUT (BASE+941)	/* INTENT IN/OUT conflict for arg - f90 */
#define FWARGPTR (BASE+942)	/* Actual/Dummy ptr arg conflict  - f90 */
#define FWARGRNK (BASE+943)	/* Actual/Dummy arg rank conflict - f90 */
#define FWARGPCL (BASE+944)	/* Act/Dummy ptr/ass-shape charlen- f90 */
#define FWARGASS (BASE+945)	/* Act/Dummy array type conflict  - f90 */
#define FWARGSCA (BASE+946)	/* Act/Dummy array/scalar conflict- f90 */
#define FWARGCHL (BASE+947)	/* Act/Dummy arg charlen conflict - f90 */
#define FWARGARS (BASE+948)	/* Act/Dummy scalar/array conflict- f90 */
#define FWARGSIZ (BASE+949)	/* Act/Dummy array extent conflict- f90 */
#define FWBNDCHK (BASE+950)	/* Subscript value out of bounds...	*/
#define FWCONFCK (BASE+951)	/* Arrays are not conformant...		*/
#define FWNUMARG (BASE+952)	/* Number of arguments is not correct	*/
#define FWARGTYP (BASE+953)	/* Actual/dummy argument type conflict	*/
#define FWFUNTYP (BASE+954)	/* Actual/dummy argument type conflict	*/
#define FWARGSHP (BASE+955)	/* Actual/dummy argument shape conflict */

#define FWARGDIM (BASE+960)	/* array dimension nonconformant  - f90	*/
#define FWARGSBV (BASE+961)	/* Subscript value out of bounds  - f90	*/
#define FWARGSBR (BASE+962)	/* Subscript range out of bounds  - f90	*/
#define FWARGSTR (BASE+963)	/* Substring range out of bounds  - f90	*/
#define FWARGSVB (BASE+964)	/* Subscript val out of bounds, mipsf90	*/
#define FWARGDMD (BASE+965)	/* array dim nonconform,dim,ep, mipsf90	*/
#define FWARGDMZ (BASE+966)	/* array dim nonconform,ep,     mipsf90	*/
#define FWASOCPT (BASE+967)	/* unassociated pointer			*/
#define FWALOCAR (BASE+968)	/* unallocated allocatable array	*/
#define FWASASAR (BASE+969)	/* unassociated assumed shape array	*/

#define CWPTRCHK (BASE+975)	/* Dereference of suspicious C pointer	*/
#define CWBNDCHK (BASE+976)	/* C subscript value out of bounds	*/

/* ifdef KEY Bug 4260 */
#define FWFILENV (BASE+977)	/* FILENV overrides compilation option  */
/* endif KEY Bug 4260 */
/* ifdef KEY Bug 7479 */
#define FWSTKNOMOD (BASE+978)	/* PSC_STACK_LIMIT is empty, so will not mod */
#define FWSTKNOCALC (BASE+979)	/* Could not discover prior stack size limit */
#define FWSTKNOLIM (BASE+980)	/* Stack size was unlimited prior to start */
#define FWSTKLIM (BASE+981)	/* Prior stack size limits: %ld current, */
#define FWSTKPHYSMEM (BASE+982)	/* Physical memory: %lld bytes\nNumber of */
#define FWSTKPHYSSHR (BASE+983)	/* Physical memory is shared between %d */
#define FWSTKAUTOMAX (BASE+984)	/* Automatic maximum stack size limit: */
#define FWSTKILLFORM (BASE+985)	/* Requested stack size limit of \"%s\" */
#define FWSTKBADMAX (BASE+986)	/* Bad maximum stack size limit of %lld */
#define FWSTKUNLIM (BASE+987)	/* Treating requested stack size limit of */
#define FWSTKINFIN (BASE+988)	/* You requested no limit on the stack size */
#define FWSTKLIMIT (BASE+989)	/* You have asked for a stack size limit of */
#define FWSTKVSPHYS (BASE+990)	/* Your requested stack size limit is %lld%% */
#define FWSTKNOREDUCE (BASE+991)	/* Stack limit is already greater */
#define FWSTKNOCHG (BASE+992)	/* Could not change stack size limit */
#define FWSTKINCORRECT (BASE+993)	/* Stack size limit was not set */
/* endif KEY Bug 7479 */

/*
 * Errors generated by the ffio package, range is 5000-5999.
 */

#define FDC_ERRB	5000	/* FFIO Error number base */
#define FDC_ERR_INTERR	5000	/* Deep weeds, internal error */
#define FDC_ERR_CHAIN	5001	/* The chain of layers has some consistency */
				/* problem.  Issued from ffopen(). */
#define FDC_ERR_NOSUP	5002	/* request not supported */
#define FDC_ERR_NOBDRY	5003	/* format depends on lower level */
				/* record boundaries, which are not there */
#define FDC_ERR_UBC	5004	/* bad UBC count */
#define FDC_ERR_FMT	5005	/* error in FDC record format, usually */
				/* means that the file has been corrupted. */
#define FDC_ERR_SCC	5006	/* bad SCC found in foreign file */
#define FDC_ERR_MXREC	5007	/* maximum record size exceeded */
#define FDC_ERR_MXBLK	5008	/* maximum block size exceeded */
#define FDC_ERR_RAWR	5009	/* read after write error */
#define FDC_ERR_NOMEM	5010	/* no memory! */
#define FDC_ERR_REQ	5011	/* Bad request */
#define FDC_ERR_PITM	5012	/* A read produced a partial item, when */
				/* numeric conversion was requested. */
#define FDC_ERR_WPEOD	5013	/* Write past EOD */
#define FDC_ERR_RPEOD	5014	/* Read past EOD */
#define FDC_ERR_PADD	5015	/* remainder of record is not mult of rec */
#define FDC_ERR_UXEND	5016	/* unexpected end, bad format */
#define FDC_ERR_CDCBT	5017	/* bad cyber block terminator */
#define FDC_ERR_CDCICW	5018	/* bad cyber I control word */
#define FDC_ERR_CDCWCW	5019	/* bad cyber W control word */

#define FDC_ERR_WRARD	5021	/* Write after READ, not yet supported */
#define FDC_ERR_DISABL	5022	/* Tried to use disabled layer */
#define FDC_ERR_CCVRT	5023	/* Character conversion error */
#define FDC_ERR_NCVRT	5024	/* Numeric conversion error */
#define FDC_ERR_BADSPC	5025	/* Bad open spec, usually layer */
#define FDC_ERR_BADBCW	5026	/* Bad BCW in COS layer */
#define FDC_ERR_BADRCW	5027	/* Bad RCW in COS layer */
#define FDC_ERR_WRDEV	5028	/* wrong device type for term layer */
#define FDC_ERR_BADNVE	5029	/* bad NOS/VE V control word */
#define FDC_ERR_FSSALO	5030	/* sds/mr allocation failed */
#define FDC_ERR_SDSIO	5031	/* an ssread/sswrite failed */
#define FDC_ERR_BADSK	5032	/* bad seek request */
#define FDC_ERR_BADCOS	5033	/* corrupted/bad COS blocked file */
#define FDC_ERR_FSSOVF	5034	/* FSS overflow not permitted */
#define FDC_ERR_NWEOF	5035	/* cannot WEOF */
#define FDC_ERR_BADPRI	5036	/* Bad PRI field in COS blocked file */
#define FDC_ERR_BADPFI	5037	/* Bad PFI field in COS blocked file */
#define FDC_ERR_NOPARM	5038	/* Not enough parameters in call */
#define FDC_ERR_NOGPOS	5039	/* GETPOS not supported */
#define FDC_ERR_NOSPOS	5040	/* SETPOS not supported */
#define FDC_ERR_NOTREC	5041	/* Rqst must occur only on rec bdry */
#define FDC_ERR_BADPTR	5042	/* fio pointer does not point to valid struct */
#define FDC_ERR_NBUF0	5043	/* negative number of buffers requested */
#define FDC_ERR_BUFSIZ	5044	/* buffer size is negative or too large */
#define FDC_ERR_NOSTRM	5045	/* format depends on lower level layers(s) */
				/* being stream, which they are not */
#define FDC_ERR_BADOVF	5046	/* FSS Overflow not permitted */
#define FDC_ERR_SDSWB	5047	/* ssread/sswrite requires word boundary */
#define FDC_ERR_OPNGRAN	5048	/* file size is not a multiple of granularity */
#define FDC_ERR_GRAN	5049	/* layer data size granularity is violated */
#define FDC_ERR_BCKDOOR 5050	/* layer below cache.ssd must support O_SSD */
#define FDC_ERR_NOER90	5051	/* File is not an ER90 */

#define FDC_ERR_OAPPEND	5052	/* layer does not support O_APPEND open flag */
#define FDC_ERR_OSSD	5053	/* layer does not support O_SSD flag */
#define FDC_ERR_LSTIO	5054	/* listio subrequests not all for same file */
#define FDC_ERR_NOSCR	5055	/* file cannot be opened as a scratch file */
#define FDC_ERR_NOTAPE	5056	/* File is not a tape file.  It is ER90 */
#define FDC_ERR_WRTERR	5057	/* Wrote too little data */
#define FDC_ERR_RDERR	5058	/* Read too little data */
#define FDC_ERR_SHCAC	5059	/* Cannot have stacked shared caches */

#define FDC_ERR_MTLSTIO	5060	/* Layer must be able to handle listio */
#define FDC_ERR_MTLOCK 	5061	/* Cannot lock */
#define FDC_ERR_GLMFILE 5062	/* Different files attached on different PEs */
#define FDC_ERR_BADSHC	5063	/* Bad stacked shared caches */
#define FDC_ERR_NOSTCA	5064	/* stacked shared caches not allowed*/
#define FDC_ERR_EOVDIS	5065	/* EOV processing is disabled */
#define FDC_ERR_EOVALOW	5066	/* Operation not allowed during special eov */

/*
 * Errors generated by the 'cmp' UserLayer will be in the range 5500-5599.
 */

#define FDC_ERR_CMPLYR		5501	/* Can't alloc memory for cmp layer */
#define FDC_ERR_SEGSIZ		5502	/* Segment size is not within range */ 
#define FDC_ERR_CACHESZ		5503	/* Invalid data segment cache size  */
#define FDC_ERR_DEBUGLVL	5504	/* Invalid Debug level */
#define FDC_ERR_BADTRANS	5505	/* Invalid transformation type */
#define FDC_ERR_INITHDR		5506	/* Unitialized header */

#define FDC_ERR_WRHDR		5507	/* Error writing header */
#define FDC_ERR_CACHE		5508	/* Error allocating DataSegD cache */
#define FDC_ERR_DSEGD		5509	/* Error writing DataSegD */
#define FDC_ERR_DSEG		5510	/* Error writing data segment */
#define FDC_ERR_DSEGBUF		5511	/* Error allocating cmpDSegBuffer */
#define FDC_ERR_WRCACHE		5512	/* Error writing DSegD Cache */
#define FDC_ERR_WRTRLR		5513	/* Error writing trailer */

#define FDC_ERR_RDHDR		5514	/* Unable to read file header */
#define FDC_ERR_BADHDR		5515	/* Invalid file header */
#define FDC_ERR_SEEKTRL		5516	/* Unable to seek to trailer */
#define FDC_ERR_RDTRL		5517	/* Unable to read file trailer */
#define FDC_ERR_SEEKCACHE	5518	/* Couldn't seek to cache location */
#define FDC_ERR_RDCACHE		5519	/* Couldn't read segment cache */
#define FDC_ERR_SEEKFDSEGD	5520	/* Couldn't seek to first data */
					/* segment descriptor in file  */
#define FDC_ERR_BADCACHE	5521	/* Invalid segment descriptor cache */
#define	FDC_ERR_NODSEGD		5522	/* No segment descriptors found */
#define FDC_ERR_FILESZ		5523	/* Inconsistent file size */
#define FDC_ERR_CMPBUF		5524	/* Couldn't alloc compressed buf */
#define FDC_ERR_UNCMPBUF	5525	/* Couldn't alloc uncompressed buf */
#define FDC_ERR_RDDSEG		5526	/* Couldn't read data segment */
#define FDC_ERR_RDADSEG		5527	/* Couldn't read data segment */
#define FDC_ERR_SEGLEN		5528	/* Bad data segment length */
#define FDC_ERR_SEEKDSEG	5529	/* Couldn't seek to data segment */
#define FDC_ERR_BADFLAGS	5530	/* Can't write to a file that was */
					/* not opened for writing */
#define FDC_ERR_WRSEEK		5531	/* Can't seek when writing a */
					/* compressed file */

#define FDC_ERR_SEEKBEG		5532	/* Couldn't seek to beginning */
#define FDC_ERR_SEEKEND		5533	/* Couldn't seek to last data seg */
#define FDC_ERR_UPDHDR		5534	/* Error updating file header */
#define FDC_ERR_BADAPPEND	5535	/* Error appending data */
#define FDC_ERR_APPOPEN		5536	/* Can't open append file for read */
#define FDC_ERR_APPCLOSE	5537	/* Can't close append file */

#define FDC_ERR_BADSEGBUF	5538	/* Bad uncompressed cached buffer */
#define FDC_ERR_UNCMPSEGLIST	5539	/* Couldn't allocate uncompressed */
					/* segment buffer cache	          */
#define FDC_ERR_SEEKSET		5540	/* Pos for SEEK_SET is invalid	  */
#define FDC_ERR_TRACE_FILE	5541	/* Couldn't open trace file	  */

/*
 * Error codes for the Lempel-Ziv compression and decompression algorithms.
 */

#define FDC_ERR_LZ_UNPACK	5542	/* Couldn't unpack compressed info */
#define FDC_ERR_LZ_BADCHAR	5543	/* Unknown character encountered   */
#define FDC_ERR_LZ_NOCOMP	5544	/* No compression achieved	   */
#define FDC_ERR_LZ_BUFALLOC	5545	/* Couldn't allocate buffer space  */
#define FDC_ERR_LZ_WRITEMAX	5546	/* Couldn't write max bits	   */
#define FDC_ERR_LZ_BADHEADER	5547	/* Wrong magic header bits found   */
#define FDC_ERR_LZ_BADMAXBITS	5548	/* Wrong max bits encountered	   */
#define FDC_ERR_LZ_BADINPUT	5549	/* Bad input during decompression  */
#define FDC_ERR_LZ_WRITEHDR	5550	/* Error writing magic header	   */
#define FDC_ERR_LZ_UNCMPLEN	5551	/* Wrong uncompressed length	   */
#define FDC_ERR_LZ_BADCOMP	5552	/* Compression not working (DEBUG) */

#endif /* !_LIBERRNO_H */

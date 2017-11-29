/* USMID @(#)10/cmd/tmf/include/tmf/fesdefs.h	10.0	02/18/98 14:05:12 */


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


/*
 *   fesdefs.h
 *
 *   This module defines the constants used to support front-end
 *   servicing.
 *
 */

#ifndef	__FESDEFS_H_
#define	__FESDEFS_H_


/*
 *   Table size definitions
 */
#define	DEX_SIZE		2	/* Size of DEX table (words)	  */
#define	DUX_SIZE		3	/* Size of DUX table (words)	  */
#define	FSH_SIZE		10	/* Size of FSH table (words)	  */
#define	LBL_SIZE		4	/* Size of LBL table w/o labels   */
#define	LBL_LABELSIZE		10	/* Size of each label(vol1,hdr1,hdr2) */
#define	LDT_SIZE		4	/* Size of LDT header (words)	  */
#define	LDTVSN_SIZE		1	/* Size of VSN structure (words)  */
#define	LDTVSN_MAXVSN		255	/* Max number of vsns allowed	  */
#define	LDTVOL1_SIZE		5	/* Size of VOL1 w/o vsn list	  */
#define	LDTHDR1_SIZE		17	/* Size of HDR1 table (words)	  */
#define	LDTHDR2_SIZE		5	/* Size of HDR2 table (words)	  */
#define	RMX_SIZE		5	/* Size of RMX table (words)	  */
#define	RMX_MAXTXT_SIZE		10	/* Max size of RMX text (words)   */
#define	TMSG_SIZE		1	/* Size of TMSG structure (words) */
#define	TMSG_TEXT_SIZE		10	/* Size of TMSG text (words)	  */
#define	VAX_SIZE		3	/* Size of VAX table (words)	  */
#define	VUX_SIZE		4	/* Size of VUX table (words)	  */
#define	LIX_SIZE		136	/* Size of LIX table (words)	  */
#define	LFX_SIZE		16	/* Size of LFX table (words)	  */
#define	MAX_LX_SIZE		156	/* Max lix/lfx reply size	  */

/*
 *   Values for fields used in Front-end Servicing requests and replies
 */
#define	DEX_ASTNIMP	0		/* not implemented */
#define	DEX_ASTYESS	1		/* yes, mainframe is secure */
#define	DEX_ASTNO	2		/* no */
#define	DEX_ASTYESNS	3		/* yes, mainframe is not secure */
#define	DEX_ESTNIMP	0		/* not implemented */
#define	DEX_ESTEXST	1		/* exists in catalog */
#define	DEX_ESTNICAT	2		/* not in catalog */
#define	DEX_ESTNOCAT	3		/* no catalog on mainframe */
#define	DEX_ARGIN	0		/* ring in */
#define	DEX_ARGOUT	1		/* ring out */
#define DEX_APBRD	0x80000000	/* read */
#define DEX_APBWR	0x40000000	/* write */
#define DEX_APBRDWR	0x20000000	/* read/write */
#define DEX_APBWRRD	0x10000000	/* write/read */
#define DEX_APBEXT	0x08000000	/* extend */
#define DEX_APBDEL	0x04000000	/* delete */
#define DEX_APBREC	0x02000000	/* recatalog */
#define DEX_APBCC	0x01000000	/* characteristic change */
#define	DEX_PRBRD	0x80000000	/* read */
#define	DEX_PRBWR	0x40000000	/* write */
#define	DEX_PRBRDWR	0x20000000	/* read/write */
#define	DEX_PRBWRRD	0x10000000	/* write/read */
#define	DEX_PRBEXT	0x08000000	/* extend */
#define	DEX_PRBDEL	0x04000000	/* delete */
#define	DEX_PRBREC	0x02000000	/* recatalog */
#define	DEX_PRBCC	0x01000000	/* characteristic change */

#define	DUX_FCNOP	0		/* no operation */
#define	DUX_FCENT	1		/* enter into catalog */
#define	DUX_FCUPDT	2		/* update catalog entry */
#define	DUX_FCDEL	3		/* delete from catalog */
#define	DUX_STNPER	0		/* not performed */
#define	DUX_STPASS	1		/* pass */
#define	DUX_STFAIL	2		/* fail */
#define	DUX_STACAT	3		/* already cataloged */
#define	DUX_STNCAT	4		/* not cataloged */
#define	DUX_STNOCAT	5		/* no catalog on mainframe */

#define	LBL_VOL1B	4		/* offset to vol1 in table (words) */
#define	LBL_HDR1B	14		/* offset to hdr1 in table (words) */
#define	LBL_HDR2B	24		/* offset to hdr2 in table (words) */

#define	LDT_CTNONE	0		/* no conversion */
#define	LDT_CTIBM	1		/* 32-bit conversion */
#define	LDT_LTNL	0		/* non labeled */
#define	LDT_LTAL	1		/* ANSI standard label */
#define	LDT_LTSL	2		/* IBM standard label */
#define	LDT_IDCOLD	0		/* old */
#define	LDT_IDCNEW	1		/* new */
#define	LDT_IDCMOD	2		/* mod */

#define	LDTVOL1_DT6250	0		/* 6250 bpi */
#define	LDTVOL1_DT1600	1		/* 1600 bpi */
#define	LDTVOL1_VDCEX	0		/* existing */
#define	LDTVOL1_VDCNEW	1		/* new volume */

#define	LDTHDR2_FMTF	0x46		/* fixed */
#define	LDTHDR2_FMTV	0x56		/* variable */
#define	LDTHDR2_FMTU	0x55		/* undefined */
#define	LDTHDR2_FMTD	0x44		/* variable */
#define	LDTHDR2_FMTS	0x53		/* span blocks */
#define	LDTHDR2_BAB	0x42		/* blocks are mult of record size */
#define	LDTHDR2_BAS	0x53		/* records span blocks */
#define	LDTHDR2_BAR	0x52		/* records span blocks and    */
					/*     are mult of block size */

#define	RMX_DT6250	0		/* 6250 bpi */
#define	RMX_DT1600	1		/* 1600 bpi */
#define	RMX_RGIN	0		/* ring in */
#define	RMX_RGOUT	1		/* ring out */
#define	RMX_DCOLD	0		/* old */
#define	RMX_DCNEW	1		/* new */
#define	RMX_LTNL	0		/* non labeled */
#define	RMX_LTAL	1		/* ANSI standard label */
#define	RMX_LTSL	2		/* IBM standard label */
#define	RMX_RCLNREJ	0		/* no reject condition */
#define	RMX_RCLWVSN	1		/* wrong vsn */
#define	RMX_RCLWLT	2		/* wrong label type */
#define	RMX_RCLRCH	3		/* ring change */
#define	RMX_RCLNSCR	4		/* not scratchable */
#define	RMX_RCLLDE	5		/* label data error */
#define	RMX_RCLRESET	6		/* reset hit */
#define RMX_RESELECT	7		/* operator reselect request */

#define	TMSG_SNOLOG	0		/* log in system log */
#define	TMSG_SLOG	1		/* don't log in system log */
#define	TMSG_UNOLOG	0		/* log in user log */
#define	TMSG_ULOG	1		/* don't log in user log */

#define	VAX_CORD	1		/* read */
#define	VAX_COWR	2		/* write */
#define	VAX_DDSOLD	0		/* old */
#define	VAX_DDSNEW	1		/* new */
#define	VAX_DDSMOD	2		/* mod */
#define	VAX_ASTNIMP	0		/* not implemented */
#define	VAX_ASTYESS	1		/* yes, mainframe is secure */
#define	VAX_ASTNO	2		/* no */
#define	VAX_ASTYESNS	3		/* yes, mainframe is not secure */
#define	VAX_ESTNIMP	0		/* not implemented */
#define	VAX_ESTEXST	1		/* exists, accept */
#define	VAX_ESTNICAT	2		/* not in catalog, reject */
#define	VAX_ESTNOCAT	3		/* no catalog, accept */
#define	VAX_XSTNIMP	0		/* not implemented */
#define	VAX_XSTEXP	1		/* expired */
#define	VAX_XSTNEXP	2		/* not expired */
#define	VAX_XSTINVAL	3		/* invalid xpdt */
#define	VAX_ARGIN	0		/* ring in */
#define	VAX_ARGOUT	1		/* ring out */
#define	VAX_FATCONT	0		/* continue */
#define	VAX_FATABORT	1		/* abort */

#define	VUX_LORD	1		/* read */
#define	VUX_LOWR	2		/* write */
#define	VUX_DDSOLD	0		/* old */
#define	VUX_DDSNEW	1		/* new */
#define	VUX_DDSMOD	2		/* mod */
#define	VUX_ALTNL	0		/* non labeled */
#define	VUX_ALTAL	1		/* ANSI standard label */
#define	VUX_ALTSL	2		/* IBM standard label */
#define	VUX_VSTBOV	0		/* BOV */
#define	VUX_VSTEOV	1		/* EOV */
#define	VUX_VSTEOF	2		/* EOF */
#define	VUX_VSTCLOSE	3		/* close (read only) */
#define	VUX_VSTREW	4		/* rewind (read only) */
#define	VUX_EVSBOV	001		/* BOV */
#define	VUX_EVSEOV	002		/* EOV */
#define	VUX_EVSEOF	004		/* EOF */
#define	VUX_EVSCLOSE	010		/* close (read only) */
#define	VUX_EVSREW	020		/* rewind (read only) */
#define	VUX_STNPERF	0		/* not performed */
#define	VUX_STPASS	1		/* pass */
#define	VUX_STFAIL	2		/* fail */
#define VUX_STCA1ERR	3		/* CA-1 disfunctional */

#define	LFX_NOP		0		/* No Operation */
#define LFX_MOUNT	1		/* Mount Volume */
#define LFX_DISMOUNT	2		/* Dismount Volume */
#define LFX_SWAP	3		/* Volume Swap (deferred implement.) */
#define LFX_RESETL	4		/* Reset Loader (deferred implement.) */
#define LFX_RESETD	5		/* Reset Device (deferred implement.) */
#define LFX_EXPORT	6		/* Export Volume from a loader     */
#define LFX_IMPORT	7		/* Import Volume into a loader     */
#define LFX_SCRATCH	8		/* Scratch Volume */
#define LFX_STOP	9		/* Stop Autoloader */
#define LFX_AML		10		/* Assign drive to a media loader */
#define LFX_PIN		11		/* Obtain process information */
#define LFX_CAM		12		/* Change the loader access mode	*/
#define LFX_VOLUME	13		/* query an autoloader about vsns	*/
#define LFX_EES		14		/* Obtain enter/exit station info. */
#define LFX_CANCEL	15		/* Cancel a pending server request */
#define LFX_DRIVE	16		/* query the ldr on its drive(s)   */
#define LFX_MEDCQ	17		/* query the ldr on media classes  */
#define LFX_ARCHQ	18		/* query the svr on its archives   */
#define	LFX_ESTNIMP	0		/* Status : Not Implemented        */
#define LFX_ESTGOOD	1		/* Status : Successful             */
#define LFX_ESTFAILV	2		/* Status : Failed, Volume Problem */
#define LFX_ESTFAILL	3		/* Status : Failed, Loader Problem */
#define LFX_ESTRETRY	4		/* Status : Failed, Retry Later    */
#define LFX_ESTFAIL	5		/* Status : Failed, no specifics   */
#define LFX_ESTFAILD	6		/* Status : Failed, Drive  Problem */

#define LIX_NOP		0		/* No Operation */
#define LIX_INQUIRE	1		/* Volume Inquiry */
#define LIX_STATUS	2		/* Status Inquiry */

#define	TYPE_STK		2	/* stk autoloader type for vm/mvs/uscp*/
					/*			protocol  */
#define STK_MOUNT_SPD	140		/* Volume Mount Suppressed */
#define STK_DISMOUNT_SPD 123		/* Dismount Suppressed due to */
					/* mount suppressed*/
#define	STK_UNABLE_TO_MOUNT	906	/* HSC could not perform Mount */
#define STK_QUEUED_ELSEWHERE	16386	/* Volume not in cell */
#define	STK_ALREADY_SCRATCH	16392	/* Volume already scratch status */
#define	STK_NOT_IN_ACS		28684	/* Volume not in ACS */
#define	SRVR_ERR		0Xde	/* Error code from server	*/
#define	SS_UNAVAIL		2001	/* Storage Server Unavailable */

typedef	unsigned long long	U_LONG;
#define	WORD			(sizeof(U_LONG))
#define	SSLOT_SIZE		(128*WORD)

#endif	/* __FESDEFS_H */

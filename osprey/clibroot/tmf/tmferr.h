/* USMID @(#)10/cmd/tmf/include/tmf/tmferr.h	10.1	04/24/98 06:35:04 */


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
 *   tmferr.h
 *
 *   Define TMF error codes.
 *
 */

#ifndef __TMFERR_H_
#define __TMFERR_H_

/*
 *   The following #define, ERRBASE, is the base number at which our 
 *   error codes begin.  
 */

#define ERRBASE		90000
#define ETFIRST		90000


/*
 *   Misc errors.
 */
#define	ETNODEM		90000			/* No TMF daemon	      */
#define ETTERM		90001			/* Tape Daemon Terminating    */
#define	ETSYS		90002			/* Tape system error	      */
#define	ETPRM		90003			/* Parameter error	      */
#define	ETNOTALLOWED	90004			/* Operation is not allowed   */
#define	ETUSR		90005			/* User error		      */

/*
 *   Resource errors.
 */
#define	ETJLX		90040			/* Job limit exceeded	      */
#define	ETMUSR		90041			/* Max user exceeded	      */
#define	ETRSRCLIM	90042			/* Resource limit reached     */
#define	ETSTMLIM	90043			/* Exceeded stream limit      */
#define	ETNSU		90044			/* No such user		      */
#define ETNRSV		90045			/* Unable to reserve resource */
#define	ETRSV		90046			/* Already have reservation   */
#define	ETPIU		90047			/* Path name already in use   */
#define	ETPBUSY		90048			/* Path is busy 	      */
#define	ETEXIST		90049			/* Path name exists	      */
#define	ETPNF		90050			/* Path not found	      */
#define	ETBUSY		90051			/* TMF daemon busy working on */
						/*      previous request      */
#define	ETDLK		90052			/* Possible deadlock	      */
#define	ETRLSP		90053			/* Release is pending	      */
#define	ETAUTOLOADER	90054			/* Has autoloader	      */
#define	ETMIXEDLOADERS	90055			/* Mix of manual/auto loaders */
#define	ETOVERMAX	90056			/* Overcommit max reached     */
#define	ETOVERACTIVE	90057			/* Overcommit is active	      */


/*
 *   File errors.
 */
#define	ETLBL		90080			/* Bad label structure	      */
#define	ETILC		90081			/* Incorrect label conversion */
#define	ETFSQ		90082			/* Bad file sequence number   */
#define	ETOMN		90083			/* File section > num of vsn  */
#define	ETNPC   	90084			/* No path to concatenate     */
#define	ETCCN		90085			/* Cannot concat new/app file */
#define	ETEOF		90086			/* At end of file	      */
#define	ETBOF		90087			/* Beginning of file status   */
#define	ETFMNA		90088			/* File mark not allowed      */
#define	ETOPN		90089			/* Open error		      */
#define	ETFNEW		90090			/* File cannot be new	      */

/*
 *   Volume errors.
 */
#define ETNVS		90200			/* No VSN		      */
#define ETWVSN          90201                   /* Waiting for VSN	      */
#define ETNPM           90202                   /* VSN req rejctd by userexit */
#define ETMVL           90203                   /* Exceeded volume limit      */
#define	ETOFF		90204			/* Bad offset		      */
#define ETBLPRNG	90205			/* No BLP w/ring option       */
#define	ETNSC		90206			/* Not scratch		      */
#define	ETIVSN		90207			/* Invalid VSN specified      */

/*
 *   Device request errors.
 */
#define ETNAV		90240			/* Device is not available    */
#define	ETDASN		90241			/* Device is assigned	      */
#define	ETNDV		90242			/* No device allocated	      */
#define	ETNRS		90243			/* Device not reserved	      */
#define	ETWDV		90244			/* Waiting for device	      */
#define	ETIDN		90245			/* Invalid device name	      */
#define	ETIDG		90246			/* Invalid device group name  */
#define	ETDVR		90247			/* Bad device range	      */
#define ETIDEN		90248			/* Invalid density	      */
#define	ETCOMPRESS	90249			/* Compression not supported  */
#define	ETVARBLK	90250			/* Not variable block device  */
#define ETBLKSZ		90251			/* Invalid block size	      */
#define	ETNAVR		90252			/* Device cannot be AVR	      */
#define	ETNOAVR		90253			/* AVR not active	      */
#define	ETNDG		90254			/* Device is not down for     */
						/* 	group reassignment    */
#define	ETNDN		90255			/* Device is not down	      */


/*
 *   Communication errors.
 */
#define	ETTMO		90300			/* Time out		      */
#define	ETPIPNM		90301			/* Invalid pipe name	      */
#define ETALC		90302			/* Unable to allocate space   */
#define ETMKN		90303			/* Make node error	      */
#define	ETCHO		90304			/* chown error		      */
#define ETACC		90305			/* Cannot access file	      */
#define	ETRPCF		90306			/* RPC failure		      */
#define	ETINT		90307			/* Interrupt		      */
#define	ETSRP		90308			/* Send reply error	      */
#define ETSRQ		90309			/* Send request error	      */
#define	ETDCE		90310			/* Daemon communication error */
#define	ETCPE		90311			/* Create process error	      */
#define	ETMRPP		90312			/* Make reply pipe error      */
#define	ETWRQ		90313			/* Write request error	      */
#define	ETRRQ		90314			/* Read request error	      */
#define	ETWRQI		90315			/* Write indirect error	      */
#define	ETMINF		90316			/* Indirect file create error */

/*
 *   CRL, FES, and loader errors.
 */
#define ETNCRL		90400			/* CRL not enabled	      */
#define ETVMFE		90401			/* CRL error		      */
#define ETBFE		90402			/* FES selected, CRL enabled  */
#define	ETFESE		90403			/* Front-end servicing error  */
#define	ETFESR		90404			/* Front-end servicing request*/
						/*   rejected by front-end    */
#define	ETFEID		90405			/* Invalid front-end id	      */
#define	ETNFE		90406			/* Front-end servicing not on */
#define	ETNMF		90407			/* No mainframe for FES	      */
#define	ETSCPE		90408			/* Station Call Processor err */
#define	ETILT		90409			/* Invalid loader index       */
#define	ETLDR		90410			/* Loader availability error  */
#define	ETIST		90411			/* Invalid state for ldr type */
#define	ETNLP		90412			/* No stknet process for ldr  */

/*
 *   Device errors.
 */
#define	ETFMS		90500			/* File mark status	      */
#define	ETLBK		90501			/* Large block error	      */
#define	ETUDE		90502			/* Unrecoverable data error   */
#define	ETWRP		90503			/* Write protected	      */
#define	ETBOT		90504			/* Beginning of tape	      */
#define	ETEOT		90505			/* End of tape		      */
#define	ETNDY		90506			/* Not ready		      */
#define	ETNOP		90507			/* Not operational	      */
#define	ETBRQ		90508			/* Bad request		      */
#define ETMEDIA		90509			/* Media Compatibility error  */
#define ETLOADF		90510			/* Load Failure		      */
#define ETSTAPE		90511			/* Short Tape		      */
#define	ETEOM		90512			/* End of Media Detected      */
#define	ETCHG		90513			/* Device parameter changed   */
#define	ETSMK		90514			/* Setmark was detected	      */
#define	ETMODE		90515			/* Incompatible modes	      */
#define	ETEOD		90516			/* End of recorded data	      */
#define	ETBLKLEN	90517			/* Block length set to	      */
						/*		wrong size    */
#define	ETPART		90518			/* Partition not supported    */
#define	ETFMTID		90519			/* Format id not supported    */
#define	ETSYSZ		90520			/* System zone positioning    */
						/*	    not supported     */
#define	ETFMTVAL	90521			/* Format id validation not   */
						/*   		supported     */

#define	ETLAST		90521			/* Last tape error code       */

#endif /* !__TMFERR_H_ */

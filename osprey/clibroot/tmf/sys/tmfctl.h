/* USMID @(#)10/kern/sys/tmfctl.h	10.3	04/24/98 06:35:04 */


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
 *   tmfctl
 *
 *   Define structures and request codes for the TMF driver ioctls.
 *
 */


#ifndef __SYS_TMFCTL_H_
#define __SYS_TMFCTL_H_

#include "tmfdefs.h"

/*
 *   Define the TMF ioctl codes.
 */
#define	TMFC_ACK_TMFERR		1
#define	TMFC_CLEAR		2
#define	TMFC_DAEMON		3
#define	TMFC_DMNREP		4
#define	TMFC_DMNREQ		5
#define	TMFC_EOD		6
#define	TMFC_FREE		7
#define	TMFC_IDENT		8
#define	TMFC_OPEN		9
#define	TMFC_RESUME		10
#define	TMFC_RSTCLR		11
#define	TMFC_SETIO		12
#define	TMFC_SETSID		13
#define	TMFC_UERR		14
#define	TMFC_VSN		15
#define	TMFC_PRIV		16


/*
 *   Generic TMF ioctl request structure.
 */
typedef	struct	tmfctl	{
	int	tmfc_dev;			/* Device number	   */
	int	tmfc_owner;			/* Stream owner		   */
	int	tmfc_count;			/* Function count	   */
	int	tmfc_mbs;			/* Maximum block size	   */
	int	tmfc_flag;
	char	tmfc_vsn[L_MAXVSN];		/* Volume name		   */
} tmfctl_t;

#define	TMF_IOC_VARBLK		0x00000001
#define	TMF_IOC_CSP		0x00000002	/* CSP access privilege	   */


/*
 *   Define the TMF request codes issued to the TMF daemon using ioctl
 *   TMF_DMNREQ.
 */
#define	TR_CLV		0001		/* User close volume		   */
#define	TR_EOV		0002		/* EOV select and deselect	   */
#define	TR_INFO		0003		/* Obtain TMF stream information   */
#define	TR_PABS		0004		/* Position to an absolute track   */
					/*    address			   */
#define	TR_PBLKS	0005		/* Position block forward/backward */
#define	TR_PFMS		0006		/* Position tape mark forward or   */
					/* 	backward		   */
#define	TR_PVOL		0007		/* Position to the beginning	   */
					/* 	of volume with the given   */
					/*      offset in the VSN list	   */
#define	TR_PVSN		0010		/* Position to beginning of	   */
					/* 	volume with given VSN	   */
#define	TR_RWD		0011		/* Rewind tape file		   */
#define	TR_WFM		0012		/* Write a file mark		   */

/*
 *   Define the TMF_DMNREQ request structure.  A TMF request consists of
 *   a header and, for those requests which require additional information,
 *   a request body.  Structures for requests which require additional
 *   information are listed below.
 *
 *   The following must be set in the request:
 *	request		TMF request type
 *	length		Size of the request excluding the header
 *	async		Set for asynchronous processing.  The request status
 *			can then be obtained with the TMF_DMNREP ioctl.
 *
 *   The following is returned by TMF for synchronous requests:
 *	reply		Reply status returned by TMF
 *	residual	The portion of the request which did not complete
 *
 */

typedef	struct	tmfreqhdr	{
	int	request;
	int	length;
	int	reply;
	int	residual;
	int	async;
} tmfreqhdr_t;


/*
 *   TR_EOV
 *
 *   Select or deselect user end-of-volume special processing.  Select
 *   user end-of-volume special processing by setting `select' to a non-zero
 *   value.  Deselect user end-of-volume special processing by setting
 *   `select' to zero.
 */
typedef	struct	tmfeov {
	tmfreqhdr_t	rh;
	short		select;
} tmfeov_t;


/*
 *   TR_INFO
 *
 *   Obtain information relating to a tape stream.  The stream information
 *   is returned in structure 'tsdata'.  `databuf' is a pointer to memory
 *   which must be at least as large as the `tsdata' structure.  `datalen'
 *   specifies the size of the buffer.
 */
typedef	struct	tmfinfo {
	tmfreqhdr_t	 rh;
	int		 datalen;
	void		*databuf;
} tmfinfo_t;

#if _KERNEL
typedef struct	irix5_tmfinfo {
	tmfreqhdr_t	 rh;
	int		 datalen;
	app32_ptr_t	 databuf;
} irix5_tmfinfo_t;
#endif  /* _KERNEL */


/*
 *   TR_PABS
 *
 *   Position to a specified location using a block address obtained
 *   with the MTIOCGET ioctl request.
 */
typedef	struct	tmfpabs {
	tmfreqhdr_t	rh;
	int		blkaddr;
} tmfpabs_t;


/*
 *   TR_PBLKS
 *
 *   Position by blocks.  `count' is the number of blocks to position.
 */
typedef	struct	tmfpblk {
	tmfreqhdr_t	rh;
	int		count;
} tmfpblk_t;


/*
 *   TR_PFMS
 *
 *   Position by filemarks (tapemarks).  `count' is the number of filemarks
 *   to position.
 */
typedef	struct	tmfpfm {
	tmfreqhdr_t	rh;
	int		count;
} tmfpfm_t;


/*
 *   TR_PVSN
 *
 *   Position to the beginning of the specified volume.  `evsn' is the
 *   external volume identifier.  `fvsn' is the format identifier and
 *   is valid only for D2 volumes.
 */
typedef	struct	tmfpvsn {
	tmfreqhdr_t	rh;
	char		evsn[L_MAXVSN];
	char		fvsn[L_MAXVSN];
} tmfpvsn_t;


/*
 *   TR_PVOL
 *
 *   Position to the beginning of the volume with the given offset
 *   in the volume identifier list (from the tmmnt command).  `index'
 *   specifies the offset.  '1' specifies the first volume in the volume
 *   list, '2' the second volume, etc.
 */
typedef	struct	tmfpvol {
	tmfreqhdr_t	rh;
	int		index;
} tmfpvol_t;


/*
 *   TR_WFM
 *
 *   Write a filemark (tapemark).  `count' specifies the number of filemarks
 *   to write.
 */
typedef	struct	tmfwfm {
	tmfreqhdr_t	rh;
	int		count;
} tmfwfm_t;

typedef	union {
	tmfpblk_t	tmfpblk;
	tmfpfm_t	tmfpfm;
	tmfpvsn_t	tmfpvsn;
	tmfpvol_t	tmfpvol;
	tmfpabs_t	tmfpabs;
	tmfeov_t	tmfeov;
	tmfinfo_t	tmfinfo;
	tmfwfm_t	tmfwfm;
} tmfreq_t;

#define	tmfreq_header		tmfpblk.rh
#define	tmfreq_request		tmfpblk.rh.request
#define	tmfreq_length		tmfpblk.rh.length
#define	tmfreq_reply		tmfpblk.rh.reply
#define	tmfreq_residual		tmfpblk.rh.residual
#define	tmfreq_async		tmfpblk.rh.async
#define	tmfreq_data		tmfpblk.count


/*
 *   TMFC_DMNREP
 *
 *   The following must be set in the request for those requests which
 *   return data:
 *	databuf		Pointer to a data buffer in which information gathered
 *			by TMF will be copied
 *	datalen		Length of the data buffer
 *
 *   Returned by TMF:
 *	reply		Reply status returned by TMF
 *	residual	The portion of the request which did not complete
 *
 */
typedef	struct	tmfrep	{
	int		 reply;
	int		 residual;
	int		 datalen;
	void		*databuf;
} tmfrep_t;

#if _KERNEL
typedef	struct irix5_tmfrep {
	int		 reply;
	int		 residual;
	int		 datalen;
	app32_ptr_t	 databuf;
} irix5_tmfrep_t;
#endif  /* _KERNEL */


#endif

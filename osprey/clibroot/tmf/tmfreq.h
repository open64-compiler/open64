/* USMID @(#)10/cmd/tmf/include/tmf/tmfreq.h	10.2	04/28/98 06:32:37 */


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
 *   tmfreq.h
 *
 *   Define values used to issue TMF requests to the TMF daemon via the
 *   TMF driver.  These requests are issued to the driver using ioctl,
 *   TMFC_DMNREQ.
 *
 */

#ifndef __TMFREQ_H_
#define __TMFREQ_H_

#include <sys/types.h>
#include <sys/param.h>
#include <tmf/tmfdefaults.h>

/*
 *   Define miscellaneous TMF request values.
 */
#define	TMF_RETRY_COUNT	5		/* Number of times to retry a TMF   */
					/*	request			    */

#define	LBL_NS		-1		/* Not specified on tmmnt(1)	    */
#define LBL_NL		1		/* No label			    */
#define LBL_AL		2		/* ANSI label			    */
#define LBL_SL		3		/* IBM standard label	  	    */
#define	LBL_BLP		4		/* By pass label processing	    */
#define	LBL_ST		5		/* Only one file mark on tape	    */
					/* This may be used for SEGY tapes  */
#define	LBL_ULP		6

#define FST_NEW		1		/* New file status		    */
#define FST_OLD		2		/* Old file status		    */
#define FST_APP		3		/* Append file status		    */



/*
 *   Define the structures used to request tape status information.
 */
typedef	struct	tsdata	{
	int	ts_ord;			/* Stream ordinal		      */
	int	ts_dev;			/* Device major/minor number	      */
	int	ts_fcn;			/* Last device function		      */
	short	ts_dst;			/* Last device status		      */
	short	ts_erreg[5];		/* Last device error status'	      */
	int	ts_block;		/* File block number		      */
	int	ts_bnum;		/* Block number relative to last user */
					/*	file mark written	      */
	int	ts_fmdir;		/* Direction from last user file mark */
					/*	0   After file mark	      */
					/*	1   Before file mark	      */
	/*
	 *   Tape file information
	 */
	char	ts_path[L_STMPATH];	/* Path name			      */
	char    ts_dgn[L_MAXDGP];	/* Device group name		      */
        char    ts_dvn[L_MAXDEV];	/* Device name			      */
	int	ts_year;		/* Current year			      */
	int	ts_day;			/* Current day			      */
	char	ts_fid[L_FILENAME];	/* File identifier		      */
	char	ts_rf[L_RECFMT];	/* Record format		      */
	int	ts_den;			/* Density			      */
	int	ts_dty;			/* Device type			      */
	int	ts_rl;			/* Record length		      */
	int	ts_blocksize;		/* Block size			      */
	int	ts_mbs;			/* Maximum block size		      */
	int	ts_fst;			/* File status			      */
	int	ts_lb;			/* Label type			      */
	int	ts_fsec;		/* File section number		      */
	int	ts_fseq;		/* File sequence number		      */
	int	ts_ffseq;		/* File sequence number of first file */
					/*			on tape	      */
	int	ts_ring;		/* Write ring status		      */
	int	ts_xyear;		/* Expiration year		      */
	int	ts_xday;		/* Expiration day		      */
	char	ts_first;		/* Offset of first VSN of file	      */ 
	char	ts_v1[L_LABEL];		/* VOL1 label			      */
	char	ts_h1[L_LABEL];		/* HDR1 label			      */
	char	ts_h2[L_LABEL];		/* HDR2 label			      */
	int	ts_numvsn;		/* Number of VSNs		      */
	int	ts_vsnoff;		/* Offset to VSN list from 	      */
					/*     beginning of structure tsdata  */
	int	ts_cvsn;		/* Current VSN index		      */
	int	ts_eov;			/* User end-of-volume (EOV) selected  */
					/*	0  EOV processing not selected*/
					/*	1  EOV processing selected    */
	int	ts_eovproc;		/* Processing at end-of-volume (EOV)  */
					/*	0  Not processing at EOV      */
					/*	1  Processing at EOV	      */
	int	ts_urwfm;		/* User read/write file marks allowed */
					/*	0   Not allowed		      */
					/*	1   Allowed		      */
	char	ts_ba[L_BLKATTR];	/* Block attribute		      */
	int	ts_blank1;		/* unused			      */
	int	ts_blank2;		/* unused			      */
	int	ts_blank3;		/* unused			      */
	int	ts_blank4;		/* unused			      */
	int	ts_blank5;		/* unused			      */
	int	ts_blank6;		/* unused			      */
	int	ts_blank7;		/* unused			      */
	int	ts_blank8;		/* unused			      */
	int	ts_blank9;		/* unused			      */
	int	ts_blank10;		/* unused			      */
} tsdata_t;


/*
 *   Define the structures and request information required for a user
 *   exit verifying a volume list.
 */
#define VSN_READ        1
#define VSN_WRITE       2

struct  vsn_req {
        int     num;                            /* Number of VSNs in list */
        int     uid;
        int     gid;
        int     mode;
        char    vsn[MAXVSN][L_MAXVSN];
};

struct	vsninfo	{
	uint	fidflg     : 1,			/* File id coded flag	      */
		partflag   : 1,			/* Partition specified flag   */
		fvsnflag   : 1,			/* Format id specified flag   */
		nofmtidval : 1,			/* No format id validation    */
		queued     : 1;			/* Request has been queued    */
	int	numvsn;				/* Number of VSNs	      */
	char	ivsn[MAXVSN][L_MAXVSN];		/* Array of internal VSNs     */
	char	evsn[MAXVSN][L_MAXVSN];		/* Array of external VSNs     */
	char	fvsn[MAXVSN][L_MAXVSN];		/* Array of format IDs        */
	int	partition[MAXVSN];		/* Array of partition numbers */
};


/*
 *   Define file structure
 */
typedef	struct	fs	{
	char	fn[L_MAXPATH];		/* File name		*/
	char	dvn[L_MAXDEV];
	int	fd;			/* File descriptor	*/
} fs_t;

#endif /* !__TMFREQ_H_ */

/* USMID @(#)10/cmd/tmf/include/tmf/tmfdefaults.h	10.3	05/06/98 06:24:39 */


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
 *   tmfdefaults.h
 *
 *   This module contains the default values for the Tape Management
 *   Facility.
 *
 */

#ifndef __TMFDEFAULT_H_
#define __TMFDEFAULT_H_


/*
 *   Define TMF files.
 */
#define TMFDIR		"/var/spool/tmf/"	/* Directory for TMF files */

#define	TMF_FITDIRNAME	"fit"
#define	TMF_SSPDIRNAME	"ssp"
#define	TMF_STMDIRNAME	"stream"

#define	TMF_FITDIR	TMFDIR TMF_FITDIRNAME
#define	TMF_SSPDIR	TMFDIR TMF_SSPDIRNAME
#define	TMF_STREAMDIR	TMFDIR TMF_STMDIRNAME

#define	TMF_FN		"/dev/tmfdaem"		/* TMF daemon file.  Used by  */
						/* the TMF daemon to commun-  */
						/* icate with the TMF driver  */

#define	TMFDMN_REQDEV	"tmfdaemreq"		/* File used to issue non-    */
						/* device related requests to */
						/* the TMF driver.	      */
#define	TMFDMN_REQ	TMFDIR TMFDMN_REQDEV


#define	PIPEDIRNAME	"reqpipe"
#define	PIPEDIR		TMFDIR PIPEDIRNAME	/* Directory to create      */
						/*	request	pipes	      */

#define	TR_REQPIPE	"daemon.request"
#define TR_PIPE         TMFDIR TR_REQPIPE
						/* TMF request pipe.	      */
						/* Requests from users and    */
						/* TMF commands are sent to   */
						/* TMF via this pipe.  This   */
						/* must specify a full path   */
						/* and the directory must     */
						/* exist.		      */

#define	DEF_CONFIG_FILE	"/etc/config/tmf"



/*
 *   TMF maximum values.
 */
#define CHILD_NAME_LENGTH	15
#define CHILD_BLANKS		"               "
#define	MAX_TAPE_USERS		55	/* Maximum number tape users	      */
#define	MAXPATH			80	/* Maximum length of tape path	      */
#define	MAXVID			255	/* Maximum VIDs per file (VMF)        */
#define	MAXVSN			255	/* Maximum number of VSNs per file    */
#define PIPE_NAME_LENGTH	79	/* Maximum length of TMF pipe	      */


/*
 *   TMF request values.
 */
#define	USER_DIR	"TMPDIR" /* User environment variable set to directory*/
				/* in which to create a message file if one   */
				/* is not specified and -t option specified   */
				/* on tmrsv command.			      */

#define	MSGFILE		"tape.msg" /* TMF message file name.  If a message    */
				/* is not specified with the -m option of the */
				/* tmrsv command, the specified file for      */
				/* MSGFILE will be created.  If -t is not     */
				/* specified on the tmrsv command, a MSGFILE  */
				/* does not specify a full path name, the     */
				/* message file will be created in the	      */
				/* directory from which the tmrsv is invoked. */

#define DEF_FES_PORT	1167	/* Default port for FES socket (fes)	      */

#define	SCR_KEEP	1	/* Keep scratch volumes allocated by	      */
				/* autoloader.				      */

#define	SCR_FREE	2	/* Return scratch volumes allocated by	      */
				/* autoloader.				      */


/*
 *   Define AVR values.
 */
#define	AVR		1		/* Define the AVR option.	    */
					/* Values:			    */
					/*	0  Disable		    */
					/*	1  Enable AVR		    */

#define	MAX_NUMDGP	8		/* Maximum number of device groups  */

					/* Define the default device groups */
#define	AVRDGPT								  \
									  \
static	struct	avrdgp	{						  \
		char	dgn[16];		/* device group name */	  \
		int	avr;			/* AVR: 0 = off, 1 = on */\
	}	avrdgpt[MAX_NUMDGP] =	{				  \
									  \
	{ "CART", 1 },		/* AVR for device group "CART" */	  \
	{ "TAPE", 1 },		/* AVR for device group "TAPE" */	  \
	{ "", 0 },		/* AVR for device group ""     */	  \
	{ "", 0 },		/* AVR for device group ""     */	  \
	{ "", 0 },		/* AVR for device group ""     */	  \
	{ "", 0 },		/* AVR for device group ""     */	  \
	{ "", 0 },		/* AVR for device group ""     */	  \
	{ "", 0 }		/* AVR for device group ""     */	  \
									  \
}


/*
 *   Default TMF options.
 */
#define	ASK_BLP		1	/* Operator will be asked to give permission  */
				/* for a user to bypass label processing.     */
				/* Values:				      */
				/*	0  Operator will not be asked	      */
				/*	1  Operator will be asked if user     */
				/*		may bypass label processing   */
				/* NOTE:  With bypass label processing, any   */
				/*	  user may read/write any tape.  This */
				/*	  can be prevented by setting this    */
				/*	  value appropriately.		      */

#define	ASK_LBSW	1	/* Operator will be asked to give permission  */
				/* for a user to switch to another label type */
				/*	0  Operator will not be asked	      */
				/*	1  Operator will be asked if user     */
				/*		may switch label types	      */

#define ASK_VER_SCR	0	/* Operator will be asked to verify that a    */
				/* volume can be used for scratch.  This is   */
				/* done only cataloging is not being used and */
				/* when the mount was not verified with a     */
				/* device name.				      */
				/* Values:				      */
				/*	0   Operator will not be asked	      */
				/*	1   Operator will be asked	      */

#define	ASK_VSN		0	/* Operator will be asked to enter the VSN of */
				/* the mounted volume when it cannot be ob-   */
				/* tained from the label, e.g., the volume is */
				/* non-labeled or bypass label was requested. */
				/* Values:				      */
				/*	0  Operator will not always be asked  */
				/*		Only when the volume name is  */
				/*		required, e.g., when switching*/
				/*		from non-labeled scratch to   */
				/*		a labeled tape.		      */
				/*	1  Operator will always be asked to   */
				/*		enter the VSN		      */

#define	CHK_FID		1	/* Define whether the file identifier in the  */
				/* label should be checked against the file   */
				/* identifier (or path) specified on the tmmnt*/
				/* Values:				      */
				/*	0   Do not check the file id	      */
				/*	1   Always check the file id	      */
				/*	2   Check the file id only if it was  */
				/*     		coded on the tmmnt	      */

#define	CHK_PROT	0	/* Define whether the check protection flag   */
				/* in the HDR1 label should be examined       */
				/*	0   Do not check protection flags     */
				/* 	1   Check protection flags            */

#define	CHK_VSN		1	/* Define whether the VSN in the label should */
				/* be checked against the requested VSN	      */
				/* Values:				      */
				/* 	0   Do not check the VSN	      */
				/* 	1   Check the VSN		      */

#define	CHK_XPD		1	/* Define whether the expiration date should  */
				/* checked in the HDR1 label.		      */
				/* 	0   Do not check expiration date      */
				/* 	1   Check expiration date	      */

#define	DEF_BLKSIZE	32768	/* Default block size in decimal bytes	      */

#define	DEF_BLP_RING	0	/* Define ring option for blp label type      */
				/* Values:				      */
				/*	 0   No restrictions		      */
				/*	!0   User may only mount a volume with*/
				/*	     the ring out.  If no ring option */
				/*	     specified, the ring option will  */
				/*	     default to ring out regardless   */
				/*	     of what the default ring status  */
				/*	     is.			      */

#define	DEF_DEN		DEN_6250/* Default density			      */
				/* Values: 				      */
				/*	DEN_800				      */
				/*	DEN_1600			      */
				/*	DEN_3200			      */
				/*	DEN_4000			      */
				/*	DEN_6250			      */
				/*	DEN_7000			      */
				/*	DEN_8200			      */
				/*	DEN_8500			      */

#define	DEF_DGN		"SCSI"	/* Default device group name.  This values    */
				/* must have be defined in configuration file */

#define	DEF_MOUNT_FEID	"  "	/* Front-end to send mount messages to if     */
				/* it was requested that all mount messages be*/
				/* set to a front-end. (MOUNT_DEFAULT set to  */
				/* 2)					      */

#define	DEF_FST		FST_OLD	/* Default file status			      */
				/* Values:				      */
				/*	FST_OLD	  File exists on tape	      */
				/*	FST_NEW   File does not exists on tape*/

#define	DEF_LB		LBL_AL	/* Default label type			      */
				/* Values:				      */
				/*	LBL_AL	ANSI label		      */
				/*	LBL_SL	IBM standard label	      */
				/*	LBL_NL	no label		      */

#define	DEF_MAX_STREAMS	100	/* Default value for the maximum tape streams */

#define	DEF_MM_TYPE	1	/* Defines the type of mount messages sent if */
				/* mount messages will be sent to a front-end */
				/* (MOUNT_DEFAULT set to 2).		      */
				/* Values:				      */
				/*	1  Type 1 station messages	      */
				/*	2  Type 3 station messages	      */

#define	DEF_RING	0	/* Default ring status.  Used if no ring      */
				/* option is specified on the tmmnt command   */
				/* Values:				      */
				/*	0   Accept tape with or without ring  */
				/* 	1   Accept tape only if ring is in    */
				/* 	2   Accept tape only if ring is out   */

#define	DEF_RTP		0	/* Default retention period in days (decimal) */

#define DEF_SCR_RETRY	3	/* Number of times to attempt to get a scratch*/
				/* volume out of the autoloader scratch pool  */

#define	DEF_SFEID_IS_MANDATORY	0 /* If non-zero, DEFAULT_SFEID will be used  */
				/* by all jobs no matter where they come from */
				/* or what sfeid (-m option on tmmnt command) */
				/* they specify				      */

#define DEF_STM_RETRY	10	/* Number of times to attempt to send a       */
				/* request to the autoloader before cancelling*/
				/* request */


#define	DEFAULT_SFEID	"  " 	/* Default servicing front-end id.  This is   */
				/* used only when the sfeid (-m option on     */
				/* tmmnt command) is not specified, and the   */
				/* job has no station slot		      */

#define DEST_DEFAULT	MM_IRIX /* Message destination default		      */

#define	FRONT_END_SERVICING	0  /* Configure front-end servicing	      */
#define HOST_NAME       '        ' /* Host name of TMF system		      */


#define	LDR_DEVICE_GROUP  0

#define	LDR_DEVICE_ORDER  0 	/* Define how loader device should be chosen  */
				/* Values:				      */
				/*	0    Normal round-robin method	      */
				/* 	1    Use order of device list         */
				/*	       returned from volume inquiry   */

#define	MIN_NUM_STREAMS	1	/* Minimum number for maximum TMF stream      */
#define	MAX_NUM_STREAMS	999	/* Maximum number of active TMF streams       */

#define	MOUNT_DEFAULT	1	/* Define the destination for all mount       */
				/* messages.				      */
				/* Values:				      */
				/*	1  IRIX operator		      */
				/*	2  Front-end defined by DEF_MOUNT_FEID*/

#define OPER_REP	' '	/* This character is the special character    */
				/* delimiting the operator's reply to a       */
				/* message and the optional comment field.    */

#define	RSL_DEVICE	0	/* Reselect to another device when switching  */
				/* volumes.				      */
				/*	0   Do not reselect		      */
				/*	1   Reselect			      */

#define	TRACE_SAVEDIR		"/var/spool/tmf/save/trace"
				/* Directory for saved trace files.  	      */

#define	SCR_VSN		"??????" /* Scratch volume name			      */

#define	SECURE_FRONT_END  0	/* Enforce secure front-end		      */

#define	SYS_CODE	"SGI/IRIX"
				/* System code value set in HDR1 label.  Must */
				/* a string of 13 characters or less.	      */

#define	TAPE_OWN	"SGI/IRIX"	/* Tape owner value set in VOL1 label */

#define	TRACE_DIR	"/var/spool/tmf/trace"
				/* Trace file directory.  Traces will be      */
				/* written into this directory.		      */

#define	TRACE_GROUP	9	/* Group id of tape trace files		      */
#define	TRACE_MODE	0640	/* Mode of tape trace files		      */
#define	TRACE_OWN	0	/* Owner id of tape trace files		      */

#define	TRACE_SIZE	409600	/* Trace file size in bytes		      */

#define VMF_SFEID	"RL"	/* pseudo front-end id for VMF processing     */



/*
 *   Tape label migration table
 *	
 *   The table contains an array for each label type.  The first element of
 *   each label type array contains the label type of the tape.  The remainder
 *   of the array contains the label type that the tape is allowed to
 *   migrate to.  The array must be terminated by a zero.
 */
#define	NUM_LBT	3			/* Number of label type		  */	
#define	NUM_EPL	5			/* Number of entry per label type */

#define	MIGTABLE							\
static	int	migtable[NUM_LBT][NUM_EPL]  =	{			\
									\
	{	LBL_AL , LBL_NL , LBL_SL , 0	},	/* to al from nl,sl */\
	{	LBL_SL , LBL_AL , LBL_NL , 0	},	/* to sl from al,nl */\
	{	LBL_NL , LBL_AL , LBL_SL , 0	}	/* to nl from al,sl */\
									\
}

#endif /* !_TMFDEFAULT_H */

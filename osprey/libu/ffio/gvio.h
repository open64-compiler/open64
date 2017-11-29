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


/* USMID @(#) libu/ffio/gvio.h	92.1	01/14/99 12:32:18 */


#ifndef _GVIO_H
#define _GVIO_H

/* CLASS_V */
/* These constants are referenced in libc/fdcio/fxrmisc.c and in */
/* libc/fdc/chknset.c */
#define TR_IBM_U	1	/* IBM U */
#define TR_IBM_V	2	/* IBM V */
#define TR_IBM_VB	3	/* IBM VB */
#define TR_IBM_VBS	4	/* IBM VBS */
#define TR_VMS_V_DSK	5	/* VMS V.DISK */
#define TR_VMS_V_TP	6	/* VMS V.TAPE */
#define TR_VMS_V_TR	7	/* VMS V.TR */
#define TR_VMS_S_DSK	8	/* VMS S.DISK */
#define TR_VMS_S_TP	9	/* VMS S.TAPE */
#define TR_VMS_S_TR	10	/* VMS S.TR */
#define TR_NVE_D	11	/* NOS/VE D almost identical to VMS V.TAPE */
#define TR_NVE_S	12	/* NOS/VE S */

#define NUM_V_TYPES	13

/*
 * V class bdw definitions
 */
#define BDW_IBM		0	/* IBM type BDW */

/*
 * V class sdw definitions
 */
#define SDW_IBM		1	/* IBM type SDW */
#define SDW_D		2	/* VMS ANSI D type SDW of 4 ASCII chars */
#define SDW_V		3	/* VMS 16 bit swapped count */
#define SDW_NVE_S	4	/* NOS/VE S, like D w/1 byte SCC prefix */
#define SDW_NVE_V	5	/* NOS/VE V */

#ifdef	_CRAY
#define	uint64	unsigned long
#else
#define	uint64	unsigned long long
#endif
/*
 * DEBUG stuff
 */
#if DEBUG
#define GVTRACE(bit)	if (FDCTRACE(bit))
#else
#define GVTRACE(bit)	if (0)
#endif

/* IBM control words */

/* The BDW preceded each block of data and defines its length. */
union bdw_u
	{
	struct
		{
		int ll:16;		/* block length including bdw */
		int zero:16;		/* must be zero */
		} fld;
	uint64 wword;
	};

/*
 * The IBM SDW preceeds each data segment and defines its length.
 * It also contains a 2 bit code that determines the segment's 
 * position inside a logical record.
 *
 *	! Be sure to change the scc_xxx_convert tables in gvread/write
 *	! if more types are added.
 */
#define ISCCFULL 0
#define ISCCFRST 1
#define ISCCLAST 2
#define ISCCMIDL 3

/*
 * The VMS SDW preceeds each data segment too, but the SCC codes are different
 */
#define VSCCFULL 3
#define VSCCFRST 1
#define VSCCLAST 2
#define VSCCMIDL 0

/*
 * The following is the default buffer size for all of the V class record
 * types. It is in bits, and also is the maximum block size that
 * can be read without error.
 */
#define DEFVBSIZE	32768*8
/*
 * IBM Segment Descriptor Word (SDW) structure
 */
union sdw_u
	{
	struct
		{
		int ll:16;		/* segment length including sdw */
		int scc:8;		/* segment control code */
					/* 0= complete */
					/* 1= First segment */
					/* 2= last segment */
					/* 3= middle segment */
		int zero:7;		/* reserved, zero */
		unsigned nullflag:1;	/* null segment flag */
		} fld;
	uint64 wword;
	};

/* VMS control words */
union vrec_u
	{
	struct
		{
		unsigned char lsb;	/* 2 byte binary record/seg */
		unsigned char msb;	/* size. not including size */
		} fld;
	uint64 wword;
	};
/*
 * For D type records, 4 character decimal length count
 */
union drec_u
	{
	struct
		{
		char size[4];		/* 4 byte ASCII rec size, */
					/* INCLUDING count field */
		} fld;
	uint64 wword;
	};

/*
 * VMS length word, byte swapped.
 */
union vms_sdw_u
	{
	struct
		{
		int lobyte:8;
		int hibyte:8;
		} fld;
	uint64 wword;
	};

struct gen_vf			/* generic V format */
	{
	int	bdwlen;		/* block descriptor word length */
	int	bdwtype;	/* block descriptor word type */
				/* (for backpatching) */
	int	sdwlen;		/* segment descriptor word length */
				/* including any SCC */
	int	sdwtype;	/* segment descriptor word type */
				/* (for backpatching) */
	int	scclen;		/* segment control code length */
	int	scctype;	/* segment control code type */
	int	sccpos;		/* SCC position in SDW */

	bitptr	sdwaddr;	/* segment descriptor word address */
	bitptr	bdwaddr;	/* block descriptor word address */
	bitptr	sccaddr;	/* segment control code address */
				/* (for backpatching) */
	struct ffc_info_s ffci;	/* FC_GETINFO information from lower layer */
	unsigned skipbad:1;	/* automatically skip bad data */
	};

/*
 *	Define a structure that is to be filled with limits and defaults
 *	for record lengths and block sizes.
 *	The data associated with this structure is in fxrmisc.c
 */
struct v_rec_limit_s
	{
	int min_rsz;	/* minimum record size */
	int max_rsz;	/* maximum record size */
	int def_rsz;	/* default record size */
	int min_mbs;	/* minimum block size */
	int max_mbs;	/* maximum block size */
	int def_mbs;	/* default block size */
	};

#if _MIPS_SZPTR == _MIPS_SZLONG
typedef unsigned long uintps_t;
#else
typedef unsigned long long uintps_t;
#endif

#endif

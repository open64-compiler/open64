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


/* USMID @(#) libu/ffio/cdcio.h	92.0	10/08/98 14:57:41 */


/* CLASS CDC */
/* record formats */
#define TR_CDC_CZ	1	/* CDC type CZ */
#define TR_CDC_IW	2	/* CDC type IW */
#define TR_CDC_CW	3	/* CDC type CW */
#define TR_CDC_CS	4	/* CDC type CS */
/* block types */
#define TR_CDC_BT_DISK	1	/* block type DISK */
#define TR_CDC_BT_I	2	/* block type I */
#define TR_CDC_BT_SI	3	/* block type SI */

/*
 * The CDC SCC codes are as follows.  They are in the W control words.
 */
#define CSCCFULL 0
#define CSCCFRST 1
#define CSCCLAST 3
#define CSCCMIDL 2

/*
 * CDC control words
 *	The CDC ICW and WCW control words function similarly to COS
 *	BCW and RCW respectively.
 */

union cdc_icw_u
	{
	struct
		{
		unsigned parity	:1;	/* parity bit */
		int rsv1	:2;	/* reserved for CDC */
		int rsv2	:3;	/* reserved for users */
		int blk_ord	:12;	/* block ordinal (mod 2^12) */
		int rec_num	:24;	/* record number (mod 2^24) */
		int wrd_off	:18;	/* word offset of first W cntrl word */
		} fld;
	unsigned long wword;
	};
		
#define CDC_WCW_NORMAL	00
#define CDC_WCW_DELETED	01
#define CDC_WCW_EOP	02
#define CDC_WCW_EOS	03	/* unused ? */

union cdc_wcw_u
	{
	struct
		{
		unsigned parity	:1;	/* parity bit */
		int flags	:2;	/* flags as follows:		*/
					/*  11 = end of section		*/
					/*  10 = end of partition	*/
					/*  01 = deleted record		*/
					/*  00 = Normal record		*/
		int rsv1	:3;	/* reserved for users */
		int rsv2	:10;	/* reserved for users */
		int scc		:2;	/* SCC as follows: (WCR in CDC doc) */
					/*  00 = complete record	*/
					/*  01 = first part		*/
					/*  10 = middle			*/
					/*  11 = last			*/
		int prv_siz	:18;	/* Size of previous record, including */
					/* control word (zero for first rec) */
		int ubc		:6;	/* unused bit count */
		int wrd_cnt	:18;	/* Word count of record.  For partial */
					/* records, (WCR/SCC is zero) this */
					/* only refers to that part of the */
					/* record */
		} fld;
	unsigned long wword;
	};
/*
 * CDC block terminator. 48-bits long.
 *	This is put on the end of I and SI format tapes.  For
 *	SI format, a terminator is only placed on blocks less than
 *	MaxBlockSize (5120 6-bit bytes).
 *
 *	For I format a terminator is placed on the end of every block.
 */
union cdc_bt_u
	{
	struct
		{
		int count	:12;	/* count of 12-bit 'bytes' in block */
					/* including 48-bit terminator */
		int blknum	:24;	/* block number (mod 2^24) */
		int zero	:8;	/* unused */
		int level	:4;	/* 0=EOR, 017=EOF */
		} fld;
	unsigned long wword;
	};

/*
 *	Private info necessary to process CDC data.
 */
struct cdc_f
	{
	int	ztlen;		/* zero terminator length */
	int	czflag;		/* flag situation where last byte in a */
				/* block is a zero */
	int 	lastchar;	/* The last character in a record (LJZF) */
				/* If it is % or :, then add a blank */
	bitptr	icwaddr;	/* i control word for this block*/
	bitptr	wcwaddr;	/* last w control word */
	int	needicw;	/* Flag need to put values in icw */
	int	lastwc;		/* word count of last segment */
	int	blknum;		/* block number */
	int	recnum;		/* record number */
	union cdc_bt_u	bt;	/* block terminator for current block */
				/* This is set to zero when the info in */
				/* in it is processed on read */
	int	btflag;		/* 0 for no, 1 for yes */
	int	eoslr;		/* flag telling whether we are at system */
				/* Logical EOR.  Currently only used for disk */
	struct ffc_info_s ffci;	/* FC_GETINFO information from lower layer */
	};

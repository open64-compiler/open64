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


#pragma ident "@(#) libu/ffio/errlist.c	92.1	06/29/99 13:16:47"

#include <ffio.h>

/*
 *  I/O error list
 */

/* List starts at FDC_ERRB (5000) */
/* All error mnemonics are preceeded by FDC_ERR_xxx */

char *_fdc_errlist[] = 
	{
	"FDC internal error",				/* INTERR FDC_ERRB+0 */
		/* serious internal error, see site analyst.	*/
	"FDC specification inconsistency",		/* CHAIN  FDC_ERRB+1 */
		/* The chain of layers has some consistency	*/
		/* problem. This normally means that the	*/
		/* list of FDC specifications includes a	*/
		/* spec that has its own system interface	*/
		/* (such as tape/bmx) and is followed by	*/
		/* another specification.  It can also mean	*/
		/* that the blocksizes required by a layer	*/
		/* are not handled by the underlying layer.	*/
	"request not supported",			/* NOSUP  FDC_ERRB+2 */
		/* request requires features that are not	*/
		/* supported.					*/
	"lower level block boundary not found",		/* NOBDRY FDC_ERRB+3 */
		/* Some formats require lower levels to 	*/
		/* provide block boundary information.		*/
		/* If they are not found, this error is		*/
		/* issued. 					*/
	"bad UBC count",				/* UBC	  FDC_ERRB+4 */
		/* The UBC count specified is not supported  	*/
		/* for the layer being used.			*/
	"error in FDC record format",			/* FMT	  FDC_ERRB+5 */
		/* This is issued when tht control information	*/
		/* in a file is found to be in error.  It	*/
		/* normally means that the file was specified	*/
		/* incorrectly, It can also mean that the	*/
		/* file has been corrupted.			*/
		/* Otherwise, it indicates an internal error.	*/
	"bad SCC found in foreign file",		/* SCC	  FDC_ERRB+6 */
		/* This is a more specific form of the		*/
		/* FMT error					*/
	"maximum record size exceeded",			/* MXREC  FDC_ERRB+7  */
	"maximum block size exceeded",			/* MXBLK  FDC_ERRB+8  */
	"read after write error",			/* RAWR   FDC_ERRB+9 */
		/* A write operation implies a file		*/
		/* truncation immediately following the		*/
		/* write.  A following read makes no		*/
		/* sense					*/
	"no memory!",					/* NOMEM  FDC_ERRB+10 */
		/* A memory request failed.			*/
	"bad request",					/* REQ    FDC_ERRB+11 */
	"partial item with data conversion is illegal",	/* PITM   FDC_ERRB+12 */
		/* A read produced a partial item when		*/
		/* numeric conversion was requested.		*/
	"Write past EOD",				/* WPEOD  FDC_ERRB+13 */
		/* A write request was issued past the		*/
		/* logical end of data.				*/
	"Read past EOD",				/* RPEOD  FDC_ERRB+14 */
		/* A read request was issued past the		*/
		/* logical end of data.				*/
	"length of block is not a multiple of a record",/* PADD	  FDC_ERRB+15 */
		/* When reading fixed length foreign		*/
		/* files that require the MBS to be a		*/
		/* multiple of the record size, the		*/
		/* remainder of the block was found		*/
		/* not to meet this test.			*/
	"Unexpected EOF/EOD",				/* UXEND  FDC_ERRB+16 */
		/* an unexpected EOF or EOD was			*/
		/* encountered where it was not legal		*/
	"Bad CDC block terminator",			/* CDCBT  FDC_ERRB+17 */
	"Bad CDC I-control word",			/* CDCICW FDC_ERRB+18 */
	"Bad CDC W-control word",			/* CDCWCW FDC_ERRB+19 */
	"Write after EOD",				/* WAEOD  FDC_ERRB+20 */
		/* A write operation was requested		*/
		/* following reading or writing of EOD		*/
	"Write after READ",				/* WRARD  FDC_ERRB+21 */
		/* A write operation followed a READ		*/
		/* without an intervening REWIND		*/
		/* This is not yet supported.			*/
	"assigned blocking/conversion routines disabled",/* DISABL FDC_ERRB+22 */
		/* The blocking type and/or data		*/
		/* conversion selected was disabled		*/
		/* at system build time.			*/
	"character conversion error",			/* CCVRT  FDC_ERRB+23 */
		/* A character conversion error			*/
		/* was encountered while doing			*/
		/* implicit character conversion.		*/
	"numeric conversion error",			/* NCVRT  FDC_ERRB+24 */
		/* A numeric conversion error			*/
		/* was encountered while doing			*/
		/* implicit numeric data conversion.		*/
	"bad open spec",				/* BADSPC FDC_ERRB+25 */
		/* An open request failed			*/
		/* because a parameter was out of		*/
		/* range.  Usually caused by using		*/
		/* an asg/assign command that is newer		*/
		/* than the libraries loaded with the		*/
		/* program.					*/
	"Bad BCW in cos blocked file",			/* BADBCW FDC_ERRB+26 */
		/* A bad Block control word was			*/
		/* encountered while reading a cos		*/
		/* blocked file.				*/
	"Bad RCW in cos blocked file",			/* BADRCW FDC_ERRB+27 */
		/* A bad Record control word was		*/
		/* encountered while reading a cos		*/
		/* blocked file.				*/
	"device is incompatible with the requested I/O", /* WRDEV FDC_ERRB+28 */
		/* A layer was requested that is		*/
		/* intended for a specific device type.		*/
		/* At open time the device was found		*/
		/* to be of a different type.			*/
	"bad NOS/VE V control word",			/* BADNVE FDC_ERRB+29 */
		/* A bad Record control word was		*/
		/* detected in a NOS/VE V file			*/
	"SDS allocation failed",			/* SDSALO FDC_ERRB+30 */
		/* SDS allocation, or reallocation		*/
		/* failed					*/
	"SDS I/O failed",				/* SDSALO FDC_ERRB+31 */
		/* an ssread or sswrite call failed		*/
	"bad ffseek parameter(s)",			/* BADSK  FDC_ERRB+32 */
		/* an ffseek was requested with 1 or		*/
		/* more bad parameter(s).  Probably		*/
		/* 'whence' was not 0-2.			*/
	"bad or corrupted COS blocked file",		/* BADCOS FDC_ERRB+33 */
		/* An inconsistency was found in the		*/
		/* data in a file being processed as		*/
		/* COS blocked.  The file is probably		*/
		/* corrupted, or it is not a COS blocked	*/
		/* file.					*/
	"FSS overflow error",				/* FSSOVF FDC_ERRB+34 */
		/* The user selected no overflow on		*/
		/* the -F parameter on the SDS or MR		*/
		/* layer, and the layer needed to 		*/
		/* overflow.			 		*/
	"EOF write not supported",			/* NWEOF  FDC_ERRB+35 */
		/* This layer with its current options has 	*/
		/* no representation for an EOF mark.	 	*/
		/* Hence, a request to write one has failed. 	*/
	"bad PRI field in COS blocked file",		/* BADPRI FDC_ERRB+36 */
		/* During a backspace operation, a PRI		*/
		/* field was found that was not consistent	*/
		/* with the structure of the file.  This	*/
		/* normally indicates a corrupted file.		*/
	"bad PFI field in COS blocked file",		/* BADPFI FDC_ERRB+37 */
		/* During a backfile operation, a PFI		*/
		/* field was found that was not consistent	*/
		/* with the structure of the file.  This	*/
		/* normally indicates a corrupted file.		*/
	"Not enough parameters in this call",		/* NOPARM FDC_ERRB+38 */
		/* A call was made to a routine and		*/
		/* not enough parameters were passed to		*/
		/* process the request.				*/
	"getpos operation not supported",		/* NOGPOS FDC_ERRB+39 */
		/* The GETPOS operation is not supported for */
		/* this file specification */
	"setpos operation not supported",		/* NOSPOS FDC_ERRB+40 */
		/* The GETPOS operation is not supported for */
		/* this file specification */
	"not on record boundary",			/* NOTREC FDC_ERRB+41 */
		/* A request was issued that must occur on a	*/
		/* record boundary, such as GETPOS/SETPOS */
	"unknown error",				/* UNU42  FDC_ERRB+42 */
		/* This error should not be issued		*/
	"unknown error",				/* UNU43  FDC_ERRB+43 */
		/* This error should not be issued		*/
	"unknown error",				/* UNU44  FDC_ERRB+44 */
		/* This error should not be issued		*/
	"unknown error",				/* UNU45  FDC_ERRB+45 */
		/* This error should not be issued		*/
	"unknown error",				/* UNU46  FDC_ERRB+46 */
		/* This error should not be issued		*/
	"unknown error",				/* UNU47  FDC_ERRB+47 */
		/* This error should not be issued		*/
	"unknown error",				/* UNU48  FDC_ERRB+48 */
		/* This error should not be issued		*/
	"unknown error",				/* UNU49  FDC_ERRB+49 */
		/* This error should not be issued		*/
	"unknown error",				/* UNU50  FDC_ERRB+50 */
		/* This error should not be issued		*/
	};

int _fdc_nerr = (sizeof(_fdc_errlist)/sizeof(char *));

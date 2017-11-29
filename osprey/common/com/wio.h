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


/**
***		I/O, I/O Types, and I/O Flags
***		-----------------------------
***
*** Description:
***
***	This interface describes all the i/o names, operators, types
***	associated with i/o, and properties associated with i/o.
***
*** Reserved Prefixes:
***
***	IOS		for IOSTATEMENT members only.
***
***	IOU		for unit IOITEM members only.
***	IOF		for format IOITEM members only.
***	IOC		for control IOITEM members only.
***	IOL		for list IOITEM members only.
***
*** Exported types:
***
***	IOSTATEMENT
***
***	    An enumerated type.  The members are a complete set of all I/O
***	    statements supported in Fortran 77.
***
***	    All IOSTATEMENTs are prefixed with IOS.
***
***	IOITEM
***
***	    An enumerated type.  The members are a complete set of all item
***	    types found in Fortran 77 I/O statements.  This includes all of
***	    the unit, format, and control information items as well as the
***	    i/o list items.
***
***	    All unit IOITEMs are prefixed with IOU.
***	    All format IOITEMs are prefixed with IOF.
***	    All control IOITEMs are prefixed with IOC.
***	    All list IOITEMs are prefixed with IOL.
***
*** Exported data:
***
***	    none
***
**/

/** $Revision: 1.1.1.1 $
*** $Date: 2005/10/21 19:00:00 $
*** $Author: marcel $
*** $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/wio.h,v $
**/

#ifndef wio_INCLUDED
#define wio_INCLUDED "wio.h"

#ifdef __cplusplus
extern "C" {
#endif


typedef enum {

  IOSTATEMENT_FIRST = 1,

    /* F77 I/O statements which manipulate external files.  None of these
       statements can contain an I/O list. */

    IOS_BACKSPACE = 1,
    IOS_CLOSE = 2,
    IOS_DEFINEFILE = 3,
    IOS_DELETE = 4,
    IOS_ENDFILE = 5,
    IOS_FIND = 6,
    IOS_INQUIRE = 7,
    IOS_NAMELIST = 8,
    IOS_OPEN = 9,
    IOS_REWIND = 10,
    IOS_UNLOCK = 11,

    /* F77 I/O statements which transfer data to & from external files.  All
       of these statements can contain an I/O list. */

    IOS_ACCEPT = 12,
    IOS_DECODE = 13,
    IOS_ENCODE = 14,
    IOS_PRINT = 15,
    IOS_READ = 16,
    IOS_REWRITE = 17,
    IOS_TYPE = 18,
    IOS_WRITE = 19,

    /* IOS coming out of the CRAY FE are already classified as formatted vs.
       unformatted and we need separate entries for those */
    IOS_CR_FWU = 20, 	/* Write Unformatted */
    IOS_CR_FRU = 21,	/* Read Unformatted */
    IOS_CR_FWF = 22,	/* Write Formatted */
    IOS_CR_FRF = 23,	/* Read Formatted */
    IOS_CR_FWN = 24,	/* Write Namelist */
    IOS_CR_FRN = 25,    /* Read Namelist */
    IOS_INQLENGTH = 26, /* Inquire IOlength */
    IOS_CR_OPEN = 27,	/* Cray Open */
    IOS_CR_CLOSE = 28,	/* Cray Close */
    IOS_CR_ENDFILE = 29,/* Cray endfile */
    IOS_CR_REWIND = 30, /* Cray Rewind */
    IOS_CR_INQUIRE = 31,/* Cray Inquire */
    IOS_CR_BACKSPACE = 32, /* Cray Backspace */
    IOS_CR_BUFFERIN = 33, /* Cray Buffer In */
    IOS_CR_BUFFEROUT = 34, /*Cray Buffer Out */



    /* End of IOSTATEMENT list */

  IOSTATEMENT_LAST = 34

} IOSTATEMENT;


typedef enum {

  IOITEM_ERROR = 0,

  IOITEM_FIRST = 1,

    /* F77 I/O unit information items. */

    IOU_ERROR = IOITEM_ERROR,
    IOU_NONE = 1,
    IOU_DEFAULT = 2,
    IOU_EXTERNAL = 3,
    IOU_INTERNAL = 4,
    IOU_DOPE = 5,

    /* F77 I/O format information items. */

    IOF_ERROR = IOITEM_ERROR,
    IOF_NONE = 10,
    IOF_ASSIGNED_VAR = 11,
    IOF_CHAR_EXPR = 12,
    IOF_LABEL = 13,
    IOF_LIST_DIRECTED = 14,
    IOF_NAMELIST_DIRECTED = 15,
    IOF_UNFORMATTED = 16,
    IOF_CR_PARSFMT = 17,
    IOF_CR_FMTSRC = 18,
    IOF_CR_FMTSRC_DOPE = 19,



    /* F77 I/O control information items. */

    IOC_ERROR = IOITEM_ERROR,
    IOC_ACCESS = 25,
    IOC_ASSOCIATEVARIABLE = 26,
    IOC_BLANK = 27,
    IOC_CARRIAGECONTROL = 28,
    IOC_DEFAULTFILE = 29,
    IOC_DIRECT = 30,
    IOC_DISPOSE = 31,
    IOC_END = 32,
    IOC_ERR = 33,
    IOC_EXIST = 34,
    IOC_FILE = 35,
    IOC_FORM = 36,
    IOC_FORMATTED = 37,
    IOC_IOSTAT = 38,
    IOC_KEY = 39,
    IOC_KEYEQ = 40,
    IOC_KEYGE = 41,
    IOC_KEYGT = 42,
    IOC_KEY_START = 43,
    IOC_KEY_END = 44,
    IOC_KEY_CHARACTER = 45,
    IOC_KEY_INTEGER = 46,
    IOC_KEYED = 47,
    IOC_KEYID = 48,
    IOC_MAXREC = 49,
    IOC_NAME = 50,
    IOC_NAMED = 51,
    IOC_NEXTREC = 52,
    IOC_NML = 53,
    IOC_NUMBER = 54,
    IOC_OPENED = 55,
    IOC_ORGANIZATION = 56,
    IOC_READONLY = 57,
    IOC_REC = 58,
    IOC_RECCOUNT = 59,
    IOC_RECL = 60,
    IOC_RECORDTYPE = 61,
    IOC_SEQUENTIAL = 62,
    IOC_SHARED = 63,
    IOC_STATUS = 64,
    IOC_TYPE = 65,
    IOC_U = 66,
    IOC_UNFORMATTED = 67,
    IOC_VARFMT = 68,
    IOC_VARFMT_ORIGFMT = 69,
    IOC_CR_EEEFLAG = 70,
    IOC_ADVANCE = 71,
    IOC_SIZE = 72,
    IOC_CR_FLFLAG = 73,   /* Contains info on first and last flags */
    IOC_EOR = 74,
    IOC_INQLENGTH_VAR = 75,
    IOC_CR_EDFLAG = 76,
    IOC_PAD = 77,
    IOC_DELIM = 78,
    IOC_ACTION = 79,
    IOC_POSITION = 80,
    IOC_READWRITE = 81,
    IOC_WRITE = 82,
    IOC_READ = 83,
    IOC_ERRFLAG = 84,
    
    

    /* F77 I/O list items. */

    IOL_ARRAY = 95,
    IOL_CHAR = 96,
    IOL_CHAR_ARRAY = 97,
    IOL_EXPR = 98,
    IOL_IMPLIED_DO = 99,
    IOL_IMPLIED_DO_1TRIP = 100,
    IOL_LOGICAL = 101,
    IOL_RECORD = 102,
    IOL_VAR = 103,
    IOL_DOPE = 104,

    /* End of IOITEM list */

  IOITEM_LAST = 114

} IOITEM;

typedef enum {
  IOLIB_UNSPECIFIED = 0,
  IOLIB_MIPS = 1,
  IOLIB_CRAY = 2,

  IOLIB_LAST = 2
} IOLIB;

#ifdef __cplusplus
}
#endif

#endif

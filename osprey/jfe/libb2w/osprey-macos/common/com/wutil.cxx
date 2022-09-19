/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


#include <stdio.h>
#include <stdlib.h>
#include <cmplrs/rcodes.h>

#define TRUE  1
#define FALSE 0

#include "defs.h"
#include "wintrinsic.h"
#include "wio.h"
#include "wutil.h"

//#if defined(USE_OLD_INTRN_METHOD)
static const struct {
  INTRINSIC   opcode;
  const char *name;
} intrinsic_name_table [] = {

  INTRINSIC_NONE,	"NONE",

/* All intrinsic names are moved to intrn_entry.def */
#define NEED_INTRN_ID_NAME
#  include "intrn_entry.def"
#undef NEED_INTRN_ID_NAME

  INTRINSIC_LAST,       "INTRINSIC_LAST",
};
//#endif // USE_OLD_INTRN_METHOD

struct iostatement_name_table_t {
  IOSTATEMENT   opcode;
  const char   *name;
};

struct iostatement_name_table_t  iostatement_name_table [] = {
  (IOSTATEMENT) 0,	NULL,
  IOS_BACKSPACE,	"BACKSPACE",
  IOS_CLOSE,		"CLOSE",
  IOS_DEFINEFILE,	"DEFINEFILE",
  IOS_DELETE,		"DELETE",
  IOS_ENDFILE,		"ENDFILE",
  IOS_FIND,		"FIND",
  IOS_INQUIRE,		"INQUIRE",
  IOS_NAMELIST,		"NAMELIST",
  IOS_OPEN,		"OPEN",
  IOS_REWIND,		"REWIND",
  IOS_UNLOCK,		"UNLOCK",
  IOS_ACCEPT,		"ACCEPT",
  IOS_DECODE,		"DECODE",
  IOS_ENCODE,		"ENCODE",
  IOS_PRINT,		"PRINT",
  IOS_READ,		"READ",
  IOS_REWRITE,		"REWRITE",
  IOS_TYPE,		"TYPE",
  IOS_WRITE,		"WRITE",
  IOS_CR_FWU,		"UNFORMATTED_WRITE",
  IOS_CR_FRU,		"UNFORMATTED_READ",
  IOS_CR_FWF,		"FORMATTED_WRITE",
  IOS_CR_FRF,		"FORMATTED_READ",
  IOS_CR_FWN,		"NAMELIST_WRITE",
  IOS_CR_FRN,		"NAMELIST_READ",
  IOS_INQLENGTH,	"INQUIRE_LENGTH",
  IOS_CR_OPEN,		"OPEN",
  IOS_CR_CLOSE,		"CLOSE",
  IOS_CR_ENDFILE,	"ENDFILE",
  IOS_CR_REWIND,	"REWIND",
  IOS_CR_INQUIRE,	"INQUIRE",
  IOS_CR_BACKSPACE,	"BACKSPACE",
  IOS_CR_BUFFERIN,	"BUFFERIN",
  IOS_CR_BUFFEROUT,	"BUFFEROUT"
};

struct ioitem_name_table_t {
  IOITEM      opcode;
  const char *name;
};

struct ioitem_name_table_t ioitem_name_table [] = {
  (IOITEM) 0,		NULL,
  IOU_NONE,		"NONE",
  IOU_DEFAULT,		"DEFAULT",
  IOU_EXTERNAL,		"EXTERNAL",
  IOU_INTERNAL,		"INTERNAL",
  IOU_DOPE,		"DOPE",
  (IOITEM) 6,		NULL,		/* spare */
  (IOITEM) 7,		NULL,		/* spare */
  (IOITEM) 8,		NULL,		/* spare */
  (IOITEM) 9,		NULL,		/* spare */
  IOF_NONE,		"NONE",
  IOF_ASSIGNED_VAR,	"ASSIGNED_VAR",
  IOF_CHAR_EXPR,	"CHAR_EXPR",
  IOF_LABEL,		"LABEL",
  IOF_LIST_DIRECTED,	"LIST_DIRECTED",
  IOF_NAMELIST_DIRECTED,"NAMELIST_DIRECTED",
  IOF_UNFORMATTED,	"UNFORMATTED",
  IOF_CR_PARSFMT,	"PREPARSED_FORMAT",
  IOF_CR_FMTSRC,	"FORMAT_SOURCE",
  IOF_CR_FMTSRC_DOPE,	"FORMAT_SOURCE_DOPE",
  (IOITEM) 20,		NULL,		/* spare */
  (IOITEM) 21,		NULL,		/* spare */
  (IOITEM) 22,		NULL,		/* spare */
  (IOITEM) 23,		NULL,		/* spare */
  (IOITEM) 24,		NULL,		/* spare */
  IOC_ACCESS,		"ACCESS",
  IOC_ASSOCIATEVARIABLE,"ASSOCIATEVARIABLE",
  IOC_BLANK,		"BLANK",
  IOC_CARRIAGECONTROL,	"CARRIAGECONTROL",
  IOC_DEFAULTFILE,	"DEFAULTFILE",
  IOC_DIRECT,		"DIRECT",
  IOC_DISPOSE,		"DISPOSE",
  IOC_END,		"END",
  IOC_ERR,		"ERR",
  IOC_EXIST,		"EXIST",
  IOC_FILE,		"FILE",
  IOC_FORM,		"FORM",
  IOC_FORMATTED,	"FORMATTED",
  IOC_IOSTAT,		"IOSTAT",
  IOC_KEY,		"KEY",
  IOC_KEYEQ,		"KEYEQ",
  IOC_KEYGE,		"KEYGE",
  IOC_KEYGT,		"KEYGT",
  IOC_KEY_START,	"KEY_START",
  IOC_KEY_END,		"KEY_END",
  IOC_KEY_CHARACTER,	"KEY_CHARACTER",
  IOC_KEY_INTEGER,	"KEY_INTEGER",
  IOC_KEYED,		"KEYED",
  IOC_KEYID,		"KEYID",
  IOC_MAXREC,		"MAXREC",
  IOC_NAME,		"NAME",
  IOC_NAMED,		"NAMED",
  IOC_NEXTREC,		"NEXTREC",
  IOC_NML,		"NML",
  IOC_NUMBER,		"NUMBER",
  IOC_OPENED,		"OPENED",
  IOC_ORGANIZATION,	"ORGANIZATION",
  IOC_READONLY,		"READONLY",
  IOC_REC,		"REC",
  IOC_RECCOUNT,		"RECCOUNT",
  IOC_RECL,		"RECL",
  IOC_RECORDTYPE,	"RECORDTYPE",
  IOC_SEQUENTIAL,	"SEQUENTIAL",
  IOC_SHARED,		"SHARED",
  IOC_STATUS,		"STATUS",
  IOC_TYPE,		"TYPE",
  IOC_U,		"U",
  IOC_UNFORMATTED,	"UNFORMATTED",
  IOC_VARFMT,		"VARFMT",
  IOC_VARFMT_ORIGFMT,	"VARFMT_ORIGFMT",
  IOC_CR_EEEFLAG,	"END_EOR_EOF_FLAG",
  IOC_ADVANCE,		"ADVANCE",
  IOC_SIZE,		"SIZE",
  IOC_CR_FLFLAG,	"FIRST_LAST_FLAG",
  IOC_EOR,		"EOR",
  IOC_INQLENGTH_VAR,	"INQLENGTH_VAR",
  IOC_CR_EDFLAG,	"ENCODE_DECODE_FLAG",
  IOC_PAD,		"PAD",
  IOC_DELIM,		"DELIM",
  IOC_ACTION,		"ACTION",
  IOC_POSITION,		"POSITION",
  IOC_READWRITE,	"READWRITE",
  IOC_WRITE,		"WRITE",
  IOC_READ,		"READ",
  IOC_ERRFLAG,		"ERRFLAG",
  (IOITEM) 85,		NULL,		/* spare */
  (IOITEM) 86,		NULL,		/* spare */
  (IOITEM) 87,		NULL,		/* spare */
  (IOITEM) 88,		NULL,		/* spare */
  (IOITEM) 89,		NULL,		/* spare */
  (IOITEM) 90,		NULL,		/* spare */
  (IOITEM) 91,		NULL,		/* spare */
  (IOITEM) 92,		NULL,		/* spare */
  (IOITEM) 93,		NULL,		/* spare */
  (IOITEM) 94,		NULL,		/* spare */
  IOL_ARRAY,		"ARRAY",
  IOL_CHAR,		"CHAR",
  IOL_CHAR_ARRAY,	"CHAR_ARRAY",
  IOL_EXPR,		"EXPR",
  IOL_IMPLIED_DO,	"IMPLIED_DO",
  IOL_IMPLIED_DO_1TRIP,	"IMPLIED_DO_1TRIP",
  IOL_LOGICAL,		"LOGICAL",
  IOL_RECORD,		"RECORD",
  IOL_VAR,		"VAR",
  IOL_DOPE,		"DOPE",
  (IOITEM) 105,		NULL,		/* spare */
  (IOITEM) 106,		NULL,		/* spare */
  (IOITEM) 107,		NULL,		/* spare */
  (IOITEM) 108,		NULL,		/* spare */
  (IOITEM) 109,		NULL,		/* spare */
  (IOITEM) 110,		NULL,		/* spare */
  (IOITEM) 111,		NULL,		/* spare */
  (IOITEM) 112,		NULL,		/* spare */
  (IOITEM) 113,		NULL,		/* spare */
  (IOITEM) 114,		NULL,		/* spare */
};

//#if defined(USE_OLD_INTRN_METHOD)

const char *
get_intrinsic_name ( INTRINSIC opcode )
{
  static INT32 init_intrinsic = FALSE;

  if ( ! init_intrinsic ) {
    init_intrinsic = TRUE;

    for (INT opc = INTRINSIC_FIRST; opc <= INTRINSIC_LAST; opc++ ) {
      if ( opc != intrinsic_name_table [opc].opcode ) {
        printf ( "get_intrinsic_name : %d %d %s\n", opc,
                 intrinsic_name_table [opc].opcode,
                 intrinsic_name_table [opc].name );
        printf ( "intrinsic names out of sync\n" );
	exit (RC_INTERNAL_ERROR);
      }
    }
  }

  return intrinsic_name_table [opcode].name;
}
//#endif

const char *
get_iostatement_name ( IOSTATEMENT opcode )
{
  static INT32 init_iostatement = FALSE;

  if ( ! init_iostatement ) {
    init_iostatement = TRUE;

    for (INT opc = IOSTATEMENT_FIRST; opc <= IOSTATEMENT_LAST; opc++ ) {
      if ( opc != iostatement_name_table [opc].opcode ) {
        printf ( "get_iostatement_name : %d %d %s\n", opc,
                 iostatement_name_table [opc].opcode,
                 iostatement_name_table [opc].name );
        printf ( "iostatement names out of sync\n" );
	exit (RC_INTERNAL_ERROR);
      }
    }
  }

  return iostatement_name_table [opcode].name;
}

const char *
get_ioitem_name ( IOITEM opcode )
{
  static INT32 init_ioitem = FALSE;

  if ( ! init_ioitem ) {
    init_ioitem = TRUE;

    for (INT opc = IOITEM_FIRST; opc <= IOITEM_LAST; opc++ ) {
      if ( opc != ioitem_name_table [opc].opcode ) {
        printf ( "get_ioitem_name : %d %d %s\n", opc,
                 ioitem_name_table [opc].opcode,
                 ioitem_name_table [opc].name );
        printf ( "ioitem names out of sync\n" );
	exit (RC_INTERNAL_ERROR);
      }
    }
  }

  return ioitem_name_table [opcode].name;
}

const char *
get_iolibrary_name( IOLIB lib)
{
  switch (lib) {
   case IOLIB_UNSPECIFIED:
     return ("unspec");
   case IOLIB_MIPS:
     return ("mips");
   case IOLIB_CRAY:
     return ("cray");
   default:
     return ("unknown");
  }
}

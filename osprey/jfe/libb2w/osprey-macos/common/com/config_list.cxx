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


/* ====================================================================
 * ====================================================================
 *
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_list.cxx,v $
 *
 * Revision history:
 *  18-Mar-97 - Original Version, extracted from config.c.
 *
 * Description:
 *
 * Configure the -LIST group (included in config.c).
 *
 * ====================================================================
 * ====================================================================
 */

/* This file is included in config.c, so it doesn't need its own set of
 * standard includes -- only the following:
 */
#include "config_list.h"

/* ====================================================================
 * List of global variables that are set by the -LIST option group
 * ====================================================================
 */

/***** Listing options *****/
BOOL List_Enabled = FALSE;	/* -LIST:=ON */
BOOL List_All_Options = FALSE;	/* -LIST:all_options - option listing */
BOOL List_Cite = FALSE;		/* -LIST:cite - Cite tool support */
BOOL List_Options = FALSE;	/* -LIST:options - option listing */
BOOL List_Symbols = FALSE;	/* -LIST:symbols - symbol table listing */
BOOL List_Notes = TRUE;		/* -LIST:notes - in .s file */
static BOOL List_Notes_Set = FALSE;	/* 	... option set */
BOOL List_Software_Names = FALSE; /* -LIST:software_names - in .s file */
BOOL List_Source = TRUE;	/* -LIST:source - in .s file */
char* List_Build_Date = NULL;	/* if NULL then not set */
#if defined(TARG_SL)
BOOL List_Profile=FALSE;
#endif

/* Listing options: */
static OPTION_DESC Options_LIST[] = {

    { OVK_BOOL, OV_VISIBLE,	FALSE, "",		"",
      0, 0, 0,	&List_Enabled,	NULL,
      "Enable listing file (.l) generation" },

    { OVK_BOOL, OV_VISIBLE,	FALSE, "all_options",	"al",
      0, 0, 0,	&List_All_Options,	NULL,
      "List all options in listing file" },

    { OVK_BOOL, OV_VISIBLE,	FALSE, "cite",		"ci",
      0, 0, 0,	&List_Cite,	NULL,
      "List CITE information in listing file" },

    { OVK_BOOL, OV_INTERNAL,	FALSE, "efile",	"ef",
      0, 0, 0,	&Err_File_Name,	NULL,
      "Specify listing file name instead of <base>.l" },

    { OVK_BOOL, OV_VISIBLE,	FALSE, "options",	"op",
      0, 0, 0,	&List_Options,	NULL,
      "List set/modified options in listing file" },

    { OVK_BOOL, OV_VISIBLE,	FALSE, "notes",	"no",
      0, 0, 0,	&List_Notes,	&List_Notes_Set,
      "Put compiler notes in .s file" },

    { OVK_BOOL, OV_INTERNAL,	FALSE, "source",	"so",
      0, 0, 0,	&List_Source,	NULL,
      "Intersperse source code in .s file" },

    { OVK_BOOL, OV_VISIBLE,	FALSE, "symbols",	"sy",
      0, 0, 0,	&List_Symbols,	NULL,
      "List symbols in listing file" },

#if defined (TARG_SL)
    { OVK_BOOL, OV_VISIBLE,     FALSE, "profile",       "prof",
      0, 0, 0,  &List_Profile,          NULL,
      "List profile in listing file" },
#endif

    { OVK_BOOL, OV_VISIBLE,	FALSE, "software_names", "soft",
      0, 0, 0,	&List_Software_Names,	NULL,
      "Use software (ABI) register names in .s file" },

    { OVK_NAME, OV_VISIBLE,	FALSE, "build_date", "",
      0, 0, 0,	&List_Build_Date,	NULL,
      "Put build date in assembly file" },

    { OVK_COUNT }		    /* List terminator -- must be last */
};

/* ====================================================================
 *
 * LIST_Configure
 *
 * Configure -LIST group options.
 *
 * ====================================================================
 */

static void
LIST_Configure ( void )
{
  /* Listing all options implies listing modified ones: */
  if ( List_All_Options ) List_Options = TRUE;

  /* Turn on the listing file if specific listing options specified: */
#if defined(TARG_SL)
  if ( List_Symbols || List_Cite || List_Options || List_Profile ) {
#else
  if ( List_Symbols || List_Cite || List_Options ) {
#endif
    List_Enabled = TRUE;
  }
}


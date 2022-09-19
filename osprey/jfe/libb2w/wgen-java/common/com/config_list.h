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
 * Module: config_list.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_list.h,v $
 *
 * Revision history:
 *  18-Mar-97 - Original Version
 *
 * Description:
 *
 * Global variables holding -LIST option group settings.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef config_list_INCLUDED
#define config_list_INCLUDED

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *config_list_h_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */


/* ====================================================================
 * List of global variables that are set by the -LIST option group
 * ====================================================================
 */

/***** Listing options *****/
extern BOOL List_Enabled;	/* -LIST:=ON */
extern BOOL List_Cite;		/* -LIST:cite - Cite tool support */
extern BOOL List_Options;	/* -LIST:options - option listing */
extern BOOL List_All_Options;	/* -LIST:all_options - option listing */
extern BOOL List_Source;	/* -LIST:source - source code listing */
extern BOOL List_Symbols;	/* -LIST:symbols - symbol table listing */
extern BOOL List_Notes;		/* -LIST:notes - in .s file */
extern BOOL List_Software_Names; /* -LIST:software_names - in .s file */

#endif /* config_list_INCLUDED */

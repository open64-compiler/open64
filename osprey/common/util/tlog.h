/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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



/* ====================================================================
 * ====================================================================
 *
 * Module: tlog.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:17:56 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/tlog.h,v $
 *
 * Revision history:
 *  14-Sep-95 - Original Version
 *
 * Description:  function prototypes for tlog.c which generates
 *               transformation log (t-log)
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef tlog_INCLUDED
#define tlog_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

#ifdef _KEEP_RCS_ID
static char *tlog_rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/tlog.h,v $ $Revision: 1.1 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>

/* 
   interface for generating one tlog record phase_name, trans_name, and
   keyword has to be one word input_string, output_string, and
   aux_info_string cannot have " { " or " } " but can have white-space,
   etc.
*/

extern void Generate_Tlog(
  const char*	phase_name,	/* e.g. "IPA", "LNO" */
  const char* 	trans_name,	/* e.g. "scalar_renaming" */
  SRCPOS	srcpos, 	/* source position where this */
				/* transformation occurs */
  const char*   keyword,	/* one word to distinguish this */
				/* transformation. can be null pointer */
  const char*	input_string,
  const char*	output_string,
  const char*	aux_info_string
);

extern FILE * Get_Tlog_File();


#ifdef __cplusplus
}
#endif

#endif /* tlog_INCLUDED */



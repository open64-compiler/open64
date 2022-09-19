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


#ifndef err_host_INCLUDED
#define err_host_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: err_host.h (Microcode compiler version)
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/err_host.h,v $
 *
 * Revision history:
 *  07-Sep-89 - Original Version
 *  01-Feb-91 - Copied for TP/Muse
 *
 * Description:
 *
 * Interface definition for the host program-specific portions of the
 * error handling package in util/errors.c.  Contains:
 *
 *  1)	Literal #defines for host program-specific phase IDs.
 *
 *  2)	Literal #defines for host program-specific parameter kinds.
 *
 *  3)	Prototypes for host program-specific error reporting functions.
 *
 *  4)	Definition of the Signal_Cleanup routine.
 *
 * This module should only be included by the host program-specific
 * erxxx.h files, errors.c, and err_host.c.  See also the
 * initialization of the error descriptor table in err_host.tab.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *err_host_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

/* Define any host program-specific phase literals not defined in
 * errors.h (EP_phase):
 */
/*	none for microcode compiler	*/
#ifndef MONGOOSE_BE
#define EP_DRIVER	(EP_PREDEFINED+1)	/* Compiler driver */
#define EP_IR_BUILDER	(EP_PREDEFINED+2)	/* SGIR builder */
#define EP_IR_READER	(EP_PREDEFINED+3)	/* SGIR reader */
#define EP_GLOB_OPT	(EP_PREDEFINED+4)	/* Global optimizer */
#define EP_GRA		(EP_PREDEFINED+5)	/* Global allocator */
#define EP_TARGET	(EP_PREDEFINED+6)	/* Target-specific */
#define EP_LAST		(EP_PREDEFINED+6)
#endif

/* Define any host program-specific error parameter kinds not defined
 * in errors.h (ET_kind):
 */
#define ET_LAST		ET_PREDEFINED+1

/* Format host program-specific error message parameters.  The caller
 * is expected to copy the formatted string out of the result before
 * calling this routine again.
 */
extern char *Host_Format_Parm (
  INT kind,	/* One of the parameter types in errors.h or above */
  MEM_PTR parm	/* A message parameter passed by the reporter */
);

/* Do any necessary host program-specific cleanup before aborting due
 * to a signal:
 */
extern void Signal_Cleanup (
  INT sig	/* The signal ID */
);

#ifdef __cplusplus
}
#endif
#endif /* err_host_INCLUDED */

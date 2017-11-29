/* ====================================================================
 * ====================================================================
 *
 * Module: opt_error.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_error.h,v $
 *
 * Revision history:
 *  30-AUG-94 - Original Version
 *
 * ====================================================================
 *
 * Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Further, this software is distributed without any warranty that it
 * is free of the rightful claim of any third person regarding
 * infringement  or the like.  Any license provided herein, whether
 * implied or otherwise, applies only to this software file.  Patent
 * licenses, if any, provided herein do not apply to combinations of
 * this program with other software, or any other product whatsoever.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
 *
 * Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
 * Mountain View, CA 94043, or:
 *
 * http://www.sgi.com
 *
 * For further information regarding this notice, see:
 *
 * http://oss.sgi.com/projects/GenInfo/NoticeExplan
 *
 * ====================================================================
 *
 * Description:
 *             OPT_ERROR(type, errno) 
 *             OPT_ERROR_FMT(type, errno, errmsg_list) 
 *             OPT_ASSERT(cond, message)
 *
 * ====================================================================
 * ====================================================================
 */


#ifndef opt_error_INCLUDED
#define opt_error_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif


#ifdef _KEEP_RCS_ID
static char *opt_error_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */


/*************************************************************************
 *
 *  ER_FATAL:    Fatal error.  Compilation stops.
 *  ER_ERROR:    Optimizer internal error.   Compilation may proceed.
 *  ER_INFO:     Information such as compile-phase, compilation-time, memory-usage, etc.
 *  ER_VERBOSE:  Information such as which PU is being compiled, ... 
 *
 *  ASSERT is ER_FATAL. 
 *
 *   
 *************************************************************************/

#define ER_FATAL        1
#define ER_ERROR        2
#define ER_INFO         3
#define ER_VERBOSE      4


/*
 *  Error number for the optimizer
 */
typedef enum {
  ERN_IGNORED,
  ERN_INTERNAL,
  ERN_OUT_OF_MEM,
  ERN_BAD_OPTION,
      MAX_ERN_NUMBER
  } ERROR_NUMBER;


/* Macros */


/*
 *   Four ways of reporting error:
 *    1.  use error number 
 *    2.  use format-string and a variable number of optional arguments
 *    3.  to assert with one fixed message, use OPT_ASSERT
 *    4.  to assert with more arguments, use OPT_ASSERT_FMT.
 */
#define OPT_ERROR(type, errno) \
                  (opt_Abort_File = __FILE__, opt_Abort_Loc = __LINE__, \
		   OPT_Error(type, errno))
  
#define OPT_ERROR_FMT(type, errmsg_list) \
                      (opt_Abort_File = __FILE__, opt_Abort_Loc = __LINE__,\
                       OPT_Error_Fmt (type, errmsg_list))
  
#define OPT_ASSERT(cond, message) \
                   if (cond) (OPT_ERROR_FMT(ER_FATAL, (message)))

#define OPT_ASSERT_FMT(cond, errmsg_list)  \
                       if (cond) (OPT_ERROR_FMT(ER_FATAL, errmsg_list))


/* Global Variables */

extern char *opt_Abort_File;		/* name of the file that asserted */
extern INT   opt_Abort_Loc;		/* line number that asserted */

/* Error routine */
extern void OPT_Error(INT type, INT errnum, ...);

extern void OPT_Error_Fmt(INT type, const char *fmt_str, ...);

extern void Opt_Catch_Signals (void);

#ifdef __cplusplus
}
#endif

#endif /* opt_error_INCLUDED */

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
 * Module: be_version.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:34-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.be_version.h $
 *
 * Revision history:
 *  06-Dec -95 - Original Version
 *
 * Description:
 *    Utilities for all backend components.
 *
 * ====================================================================
 * ====================================================================
 */

/* Updated the BE_Version if the interface between be.so and wopt.so
 * (or any other component) is changed.
 * 
 *  e.g., POINTS_TO analysis is now defined in be.so and used by wopt.so
 *  If POINTS_TO data structure are changed, the BE_VERSION in config.h 
 *  should be updated.
 */
#define BE_VERSION "1.01"           /* define current be version */

#ifdef __cplusplus
extern "C" const char *Get_BE_Version(void);	/* version of be and be.so */
#else
extern const char *Get_BE_Version(void);	/* version of be and be.so */
#endif

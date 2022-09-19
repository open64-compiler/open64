/*
 *  Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
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
 * Module: language.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/language.h,v $
 *
 * Revision history:
 *  26-Feb-96 - Original Version (extracted from config.h)
 *
 * Description:
 *
 * Define an enumeration type for the language being compiled.  Used
 * primarily by config.c, but extracted for use in other contexts
 * (e.g. IPA).
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef language_INCLUDED
#define language_INCLUDED

#ifdef _KEEP_RCS_ID
static char *language_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/***** Language being compiled *****/
typedef enum {
  LANG_UNKNOWN,
  LANG_F77,
  LANG_F90,
  LANG_KR_C,	/* Kernighan & Richie C */
  LANG_ANSI_C,	/* ANSI standard C */
  LANG_CPLUS,	/* simple C++ */
  LANG_JAVA,	/* Java */
  LANG_DELTA,	/* Delta C++ */
  LANG_COUNT	/* Must be last */
} LANGUAGE;

#ifdef __cplusplus
}
#endif /* __cplusplus */
    
#endif /* xxx_INCLUDED */


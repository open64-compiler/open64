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


#ifndef erlink_INCLUDED
#define erlink_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif



#ifdef _KEEP_RCS_ID
static char *erlink_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include "errors.h"	/* Always needed */

/* The error codes in each erxxx.h file should start from some multiple
 * of 1000, which is the phase number.
 */
#define EC_BASE_LINK	EP_LINK*1000

/* File manipulation error codes: */
#define EC_R_Exists	EC_BASE_LINK		/* str */
#define EC_R_Open	EC_BASE_LINK+1		/* str, err */
#define EC_R_Create	EC_BASE_LINK+2		/* str, err */
#define EC_R_Delete	EC_BASE_LINK+3		/* str, err */
#define EC_R_Close	EC_BASE_LINK+4		/* str, err */
#define EC_No_R		EC_BASE_LINK+5		/* str */
#define EC_U_Exists	EC_BASE_LINK+6		/* str */
#define EC_U_Open	EC_BASE_LINK+7		/* str, err */
#define EC_U_Create	EC_BASE_LINK+8		/* str, err */
#define EC_U_Delete	EC_BASE_LINK+9		/* str, err */
#define EC_U_Close	EC_BASE_LINK+10		/* str, err */
#define EC_No_U		EC_BASE_LINK+11		/* str */
#define EC_R_Magic	EC_BASE_LINK+12		/* str, int */
#define EC_Link_Unresolved EC_BASE_LINK+13	/* str */
#define EC_Link_Dupname	EC_BASE_LINK+14		/* str */
#define EC_Command_Open EC_BASE_LINK+15		/* str, err */
#define EC_Link_Insuf_Pages EC_BASE_LINK+16	/* nothing */
#define EC_Link_2Merge	EC_BASE_LINK+17		/* nothing */
#define EC_U_Header	EC_BASE_LINK+18		/* str */
#define EC_U_Magic	EC_BASE_LINK+19		/* str */
#define EC_U_Strange	EC_BASE_LINK+20		/* str */

#ifdef __cplusplus
}
#endif
#endif /* erlink_INCLUDED */

/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
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
 * Module: c_int_model.h
 * $Revision: 1.6 $
 * $Date: 04/12/21 14:57:43-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/kg++fe/SCCS/s.c_int_model.h $
 *
 * Revision history:
 *  ??-???-92 - Original Version
 *
 * Description:
 *
 * External interface to C integer model configuration support.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef c_int_model_INCLUDED
#define c_int_model_INCLUDED

#ifdef _KEEP_RCS_ID
static char *c_int_model_rcs_id = "$Source: /home/bos/bk/kpro64-pending/kg++fe/SCCS/s.c_int_model.h $ $Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

#ifndef defs_INCLUDED
#include "defs.h"
#endif
#ifndef mtypes_INCLUDED
#include "mtypes.h"
#endif

/* Define the integer model choices.  WARNING:  There are tables in the
 * associated source file which depend on these value assignments.
 */
typedef enum {
  TARGET_INT_ILP32 = 0,
  TARGET_INT_LP64  = 1,
  TARGET_INT_ILP64 = 2
} TARGET_INT_MODEL;
extern TARGET_INT_MODEL Target_Int_Model;

#ifdef __cplusplus
extern "C" {
#endif

extern void Initialize_C_Int_Model ( void );
extern TYPE_ID FE_Int_Type_To_Mtype(int);
extern TYPE_ID FE_Pointer_Type_To_Mtype(void);
extern TYPE_ID FE_int_To_Mtype(void);
extern void Make_Int_Model_Consistent(void);

#ifdef __cplusplus
}
#endif

#endif /* c_int_model_INCLUDED */

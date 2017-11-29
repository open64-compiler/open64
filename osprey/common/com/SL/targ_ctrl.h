/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#ifndef targ_ctrl_INCLUDED
#define targ_ctrl_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: targ_ctrl.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:18:02 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/com/MIPS/targ_ctrl.h,v $
 *
 * Description:
 *
 * This file contains target-specific control information.  It is
 * included in controls.c and should not be visible elsewhere.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *targ_ctrl_rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/common/com/MIPS/targ_ctrl.h,v $ $Revision: 1.1 $";
#endif /* _KEEP_RCS_ID */

static STR_LIST Targ_1 = {"MIPS", NULL};

#define Possible_Targets Targ_1
#define TARG_FIRST_DEF 0
#define TARG_SECOND_DEF 0

#ifdef __cplusplus
}
#endif
#endif /* targ_ctrl_INCLUDED */

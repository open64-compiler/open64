/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef ercg_INCLUDED
#define ercg_INCLUDED

/* ====================================================================
 * ====================================================================
 *
 * Module: ercg.h
 * $Revision: 1.4 $
 * $Date: 05/12/05 08:59:04-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.ercg.h $
 *
 * Revision history:
 *  02-Nov-89 - Original Version
 *  01-Feb-91 - Copied for TP/Muse
 *
 * Description:
 *
 * Define the Muse code generator error codes for use with the error
 * message handler errors.c.  The associated error descriptors may be
 * found in the file ercg.desc.
 *
 * ====================================================================
 * ====================================================================
 */


#include "errors.h"	/* Always needed */

/* The error codes in each erxxx.h file should start from some multiple
 * of 1000, which is the phase number.
 */
#define EC_BASE_CG	EP_CG*1000


/* Scheduling Preparation: */
#define EC_Ill_Cycle	EC_BASE_CG+1	/* int, str */

/* Register Allocation: */
#define EC_Ill_Reg_Spill1 EC_BASE_CG+2	/* string(register-name) */
#define EC_Ill_Reg_Spill2b EC_BASE_CG+3 /* int (suggested -O level) */

/* asm() processing related. */
#define EC_ASM_Bad_Operand EC_BASE_CG+4 /* int (line number), 
                                           int (bad operand number), 
                                           string (what was expected) */

/* Start all target-specific codes here: */
#define EC_TARGET	EC_BASE_CG+200

#endif /* ercg_INCLUDED */

/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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
 *  Module: ti_init.h
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/access/ti_init.h,v $
 *
 *  Synopsis:
 *
 *	Initialize the targ-info package.
 *
 *  Interface Description:
 *
 *	Misc. functions:
 *
 *	    void TI_Initialize( ABI_PROPERTIES_ABI, ISA_SUBSET, PROCESSOR )
 *
 *		Initialize the package.  Needs to be called once per run
 *		of the backend, before any other operations in the
 *		targ-info package are used.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef ti_init_INCLUDED
#define ti_init_INCLUDED

#ifdef _KEEP_RCS_ID
static const char ti_init_rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/access/ti_init.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#include "targ_abi_properties.h"
#include "targ_isa_subset.h"
#include "targ_proc.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void
TI_Initialize (ABI_PROPERTIES_ABI, ISA_SUBSET, PROCESSOR);

#ifdef __cplusplus
}
#endif
#endif /* ti_init_INCLUDED */

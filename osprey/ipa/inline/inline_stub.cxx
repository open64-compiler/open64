/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include "defs.h"

#include "symtab.h"

#include "wn.h"
struct ip_file_hdr;
#ifdef __cplusplus
extern "C" {
#endif

/*ARGSUSED*/
void IP_WRITE_pu ( struct ip_file_hdr *s, INT pindex ) {}

#ifdef __cplusplus
}
#endif

#if !defined(SHARED_BUILD)
/* no weak version, so need stub to compile (real version is in libwopt) */
#include "opt_defs.h"
AUX_ID WN_aux (const WN*) {}

/* from whirl2c */
extern "C" {
void W2C_Cleanup(void) {}
void W2C_Push_PU(const WN* a, WN* b) {}
void W2C_Pop_PU(void) {}
void W2C_Translate_Wn(FILE* a, const WN* b) {}
}
#endif

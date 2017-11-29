/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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
#include "errors.h"
#include "config_targ.h"
#include "config_platform.h"

void Fill_Align_Initialize_Parameters(INT *L1_sz, INT *L2_sz, INT *pg_sz)
{
  INT L1_cache_line_sz;
  INT L2_cache_line_sz;

  switch (Target) {

    case TARGET_sb1:
      L1_cache_line_sz = 32;
      L2_cache_line_sz = 32;
      break;

    default:
      L1_cache_line_sz = 32;
      L2_cache_line_sz = 128;
      break;
  }
  
  *L1_sz = L1_cache_line_sz;
  *L2_sz = L2_cache_line_sz;
  *pg_sz = 16384;
}


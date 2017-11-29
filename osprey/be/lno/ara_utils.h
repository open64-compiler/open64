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


// This may look like C code, but it is really -*- C++ -*-

#ifndef _ara_utils_INCLUDED
#define _ara_utils_INCLUDED

#include "lnopt_main.h"

extern void print_indent(FILE *fp, INT indent);

extern void Merge_Scalar_List(SCALAR_STACK* st_from,
                              SCALAR_STACK* st_to);

extern void Merge_Scalar_List_Covered(SCALAR_STACK* st_from,
                                      ARA_LOOP_INFO* ali_to,
                                      SCALAR_STACK* st_to_covered,
                                      SCALAR_STACK* st_to_not_covered);

extern POINTS_TO* Points_To(ARA_REF* ara_ref, 
                            MEM_POOL* mem_pool);

extern POINTS_TO* Points_To(SCALAR_NODE* sn,
			    MEM_POOL* mem_pool);

extern POINTS_TO* Points_To(WN* wn, 
			    MEM_POOL* mem_pool);

#endif

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


//* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#ifndef ipl_linex_INCLUDED
#define ipl_linex_INCLUDED

#ifndef loop_info_INCLUDED
#include "loop_info.h"
#endif

#ifndef ipa_section_INCLUDED
#include "ipa_section.h"
#endif

extern ARRAY_SUMMARY Array_Summary;

class SUMMARY_PROCEDURE;

extern void 
IPL_Access_Vector_To_Projected_Region(WN* w, 
                                      SUMMARY_PROCEDURE* proc,
				      INT pu_first_formal_idx,
				      INT pu_last_formal_idx,
                                      INT pu_first_actual_idx, 
                                      INT pu_last_actual_idx,
                                      INT pu_first_callsite_idx, 
                                      INT pu_last_callsite_idx);

#endif

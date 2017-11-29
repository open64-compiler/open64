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


#ifndef ipa_lno_reshape_INCLUDED
#define ipa_lno_reshape_INCLUDED "ipa_lno_reshape.h"

extern PROJECTED_REGION* Map_Formal(IPA_LNO_READ_FILE* IPA_LNO_File,
                                    WN* wn_call,
                                    INT idx_formal);

extern PROJECTED_REGION* Projected_Region(IPA_LNO_READ_FILE* IPA_LNO_File,
                                          INT idx_formal,
					  WN* wn_call);

extern PROJECTED_REGION* Map_Projected_Region(IPA_LNO_READ_FILE* IPA_LNO_File,
                                              PROJECTED_REGION* pr_memory,
                                              WN* wn_call);

extern BOOL Shape_Mismatch_At_Formal(IPA_LNO_READ_FILE* IPA_LNO_File,
                                     WN* wn_call,
                                     INT formal_number,
                                     INT idx_formal);

extern BOOL Array_Shapes_Match_At_Formal(IPA_LNO_READ_FILE* IPA_LNO_File,
                                         WN* wn_call,
                                         INT formal_number,
					 PROJECTED_REGION* pr_formal);

extern BOOL Shape_Mismatch_At_Common(IPA_LNO_READ_FILE* IPA_LNO_File,
                                     INT idx_common);

#endif /* ipa_lno_reshape_INCLUDED */

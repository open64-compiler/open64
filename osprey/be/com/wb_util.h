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


#ifndef wb_util_INCLUDED
#define wb_util_INCLUDED "wb_util.h"

enum WB_SANITY_CHECK_LEVEL {
  WB_SC_DISABLE,
  WB_SC_DU_ONLY,
  WB_SC_DU_AND_ARRAY,
  WB_SC_FULL_SNL
};

extern void WB_Bell();
extern void WB_Prompt();
extern const char* WB_Whirl_Symbol(WN* wn); 
extern INT WB_Dump_Whirl_Expr(WN* wn_root, WN* wn, char* buffer, INT cc);

#endif /* wb_util_INCLUDED */

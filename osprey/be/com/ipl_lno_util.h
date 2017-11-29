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
#ifndef ipl_lno_util_INCLUDED
#define ipl_lno_util_INCLUDED

class IPA_LNO_READ_FILE; 
class LINEX; 
class PROJECTED_REGION; 

extern INT32 Formal_Position(const ST* formal_st);
extern TYPE_ID Machine_Type(WN* wn_argument);
extern INT Node_Count(WN* wn_node, INT limit = 0, BOOL symbol_only = FALSE);
extern WN* True_Bound(WN* wn_func, ST_IDX st_idx_exp);
extern BOOL Exp_To_Linex(WN* wn_exp, LINEX* lx_exp, MEM_POOL* mem_pool,
  BOOL negate = FALSE, BOOL Is_LNO = FALSE, IPA_LNO_READ_FILE* 
  IPA_LNO_File = NULL);
extern PROJECTED_REGION* Projected_Region_From_St(WN* wn_func,
  ST* st, MEM_POOL* mem_pool, BOOL Is_LNO = FALSE, 
  IPA_LNO_READ_FILE* IPA_LNO_File = NULL);
extern PROJECTED_REGION* Projected_Region_From_Access_Array(ACCESS_ARRAY* aa,
  MEM_POOL* mem_pool, IPA_LNO_READ_FILE* IPA_LNO_File);

#endif

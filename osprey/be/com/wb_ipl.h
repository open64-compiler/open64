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


#ifndef wb_ipl_INCLUDED
#define wb_ipl_INCLUDED

#ifndef ipl_summarize_INCLUDED
class SUMMARY; 
#endif 

#ifndef ipa_section_INCLUDED
class ARRAY_SUMMARY;
#endif

extern void WB_IPL_Save();

extern void WB_IPL_Initialize(WN* wn_global, DU_MANAGER* du, ALIAS_MANAGER* am);

extern void WB_IPL_Set_Access_Array_Map(WN_MAP access_array_map);

extern void WB_IPL_Set_Reduction_Map(WN_MAP reduction_map);

extern void WB_IPL_Set_Scalar_Summary(SUMMARY* scalar_summary);

extern void WB_IPL_Set_Array_Summary(ARRAY_SUMMARY* array_summary);

extern void WB_IPL_Terminate();

extern void WB_IPL_Restore();

extern void s_ipl_debug(const char init_buffer[]);

#endif /* wb_ipl_INCLUDED */

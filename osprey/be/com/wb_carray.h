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


#ifndef wb_carray_INCLUDED
#define wb_carray_INCLUDED "wb_carray.h"

const INT WB_MAX_SAVED_NODES = 500;

class WB_CARRAY {
  INT _next_index; 
  WN* _carray[WB_MAX_SAVED_NODES];
public: 
  void Reset_Index() { _next_index = 0; }
  INT Next_Index() { return _next_index; }
  WN* Element(INT element) { return _carray[element]; }
  void Enter_This_Node(WN* wn);
  INT Enter_This_Node_Unique(WN* wn);
};

#endif /* wb_carray_INCLUDED */

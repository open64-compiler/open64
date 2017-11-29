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


#ifndef cgb_carray_INCLUDED
#define cgb_carray_INCLUDED "cgb_carray.h"

#ifndef dwarf_DST_INCLUDED
#include "dwarf_DST.h"
#endif

#ifndef ipc_file_INCLUDED
#include "ipc_file.h"
#endif

#ifndef cxx_ipa_cg_INCLUDED
#include "ipa_cg.h"
#endif 

const INT CGB_MAX_SAVED_NODES = 500;

class CGB_CARRAY {
  INT _next_index; 
  IPA_NODE* _nodes[CGB_MAX_SAVED_NODES];
  NODE_INDEX _vertices[CGB_MAX_SAVED_NODES];
public: 
  void Reset_Index() { _next_index = 0; }
  INT Next_Index() { return _next_index; }
  IPA_NODE* Node(INT element) { return _nodes[element]; }
  NODE_INDEX Vertex(INT element) { return _vertices[element]; }
  void Enter_This_Pair(IPA_NODE* ipan, NODE_INDEX v);
  INT Enter_This_Pair_Unique(IPA_NODE* ipan, NODE_INDEX v);
  INT Find_This_Pair(IPA_NODE* ipan, NODE_INDEX v);
  void List_All_Pairs(FILE* fp);
};

#endif /* cgb_carray_INCLUDED */

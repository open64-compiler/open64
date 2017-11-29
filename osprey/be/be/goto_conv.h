/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/* 
   Copyright (C) 2001-2004 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
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


//-*-c++-*-
//                     Promote Gotos to Loops and IFs 
//                     ------------------------------
//
// Description:
//
//     Promote gotos to loops and ifs.  This is loosely based on the
//     McGill paper.  See the .cxx file for more details.
//
//
//
// Exported types and functions:
//
//     GTABLE
//
//		A table of goto_descriptors
//
//	    GTABLE(WN *func_nd, MEM_POOL *pool)
//
//		Build a descriptor table.  
//
//	    void Remove_Gotos()
//
//		Get rid of the gotos.
//
//	    void Print(FILE *fp)
//
//     	GDESCRIPTOR
//
//		Describe the goto.
//
//	    WN *Goto_Wn
//
//		The wn of the goto.
//
//	    WN *Label_Wn
//
//		The wn of the corresponding label
//
//	    INT Goto_Offset, Label_Offset
//
//		The offsets of the goto and its corresponding
//		label as defined in the paper.
//
// 	    GDESCRIPTOR(WN *wn,WN *label_wn,
//				INT goto_offset,INT label_offset) 
//
//	    BOOL Is_Dismantled
//
//		Does the goto still exist
//
//  	LDESCRIPTOR
//
//		Describe a label
//
//	    WN *Label_Wn
//
//		The wn of the label
//
//	    INT Offset
//
//		The offset as defined in the paper
//
//	    LDESCRIPTOR(WN *label_wn, INT offset)
//
//
//	MAX_GOTO_EXPANDING_TRANSFORMATIONS = 20
//
//	    Some of the transformations increase the code size.  In particular moving
//	gotos out of IFs.  This puts in a circuit breaker to avoid the bad cases.
//	
//
//    Code adapted from be/com/opt_goto.h

#ifndef goto_conv_INCLUDED
#define goto_conv_INCLUDED "goto_conv.h"

#include "wn.h"
#include "cxx_memory.h"
#include "cxx_template.h"
#include "cxx_hash.h"

class GDESCRIPTOR {
public:
  WN *Goto_Wn;
  WN *Label_Wn;
  INT Goto_Offset;
  INT Label_Offset;
  BOOL Is_Dismantled;
  BOOL Is_Compgoto;
  GDESCRIPTOR(WN *goto_wn,WN *label_wn,INT goto_offset, INT label_offset,
			BOOL is_compgoto) {
    Goto_Wn = goto_wn;
    Label_Wn = label_wn;
    Goto_Offset = goto_offset;
    Label_Offset = label_offset;
    Is_Dismantled = FALSE;
    Is_Compgoto = is_compgoto;
  }
};

class LDESCRIPTOR {
public:
  WN *Label_Wn;
  INT Offset;
  LDESCRIPTOR(WN *label_wn,INT offset) {
    Label_Wn = label_wn;
    Offset = offset;
  }
};

typedef STACK<GDESCRIPTOR> GDESCRIPTOR_STACK;
typedef STACK<LDESCRIPTOR> LDESCRIPTOR_STACK;
typedef STACK<WN *> GTABLE_WN_STACK;

class GTABLE {
  GDESCRIPTOR_STACK _gd;
  GTABLE_WN_STACK _altentry;
  LDESCRIPTOR_STACK _bad_label;
  MEM_POOL *_pool;
  WN *_func_nd;
  HASH_TABLE<INT,LDESCRIPTOR *> *_label_table;
  WN_MAP _parent_map;
  INT _offset;
  BOOL _contains_altentry;
public:
  GTABLE(WN *func_nd, MEM_POOL *pool) : _gd(pool), _bad_label(pool),
					    _altentry(pool), _pool(pool) {
    _offset = 0;
    _func_nd = func_nd;
    _contains_altentry=FALSE;
    Build();
  }
  void Remove_Gotos();
  void Print(FILE *fp);
  ~GTABLE()  {  
    WN_MAP_Delete(_parent_map); 
    CXX_DELETE(_label_table,_pool);
  }
private:
  WN *Get_Parent(WN *wn) const { return (WN *) WN_MAP_Get(_parent_map,wn); }
  void Set_Parent(WN *wn, WN *p) { WN_MAP_Set(_parent_map, wn, (void *)p); };
  void Build();
  void Build_Rec(WN *wn, WN *parent, BOOL inside_compgoto);
  void Fixup_Parents(WN *wn, WN *parent);
  void Backpatch();
  BOOL Sibling(const GDESCRIPTOR *gd) const ;
  void Replace_Goto_With_If(GDESCRIPTOR *gd);
  BOOL Replace_Goto_With_While(GDESCRIPTOR *gd);
  void Create_Truebr(GDESCRIPTOR *gd);
  void Promote_Do_While(WN *wn);
  void Patch_Do_While(WN *while_wn, WN *parent);
  WN *Func_Nd() { return _func_nd;};
  BOOL Is_Truebr(const GDESCRIPTOR *gd) const;
  BOOL Goto_Is_Noop(const GDESCRIPTOR *gd) const;
  INT Find_Level(WN *wn);
  WN *Find_Common_Ancestor(WN *wn1, WN *wn2);
  void Dismantle(WN *bad, WN *parent);
  enum {MAX_GOTO_EXPANDING_TRANSFORMATIONS = 20 };
};
#endif  // goto_conv_INCLUDED



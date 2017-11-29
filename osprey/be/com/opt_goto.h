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
//     GOTO_TABLE
//
//		A table of goto_descriptors
//
//	    GOTO_TABLE(WN *func_nd, MEM_POOL *pool)
//
//		Build a descriptor table.  
//
//	    void Remove_Gotos()
//
//		Get rid of the gotos.
//
//	    void Print(FILE *fp)
//
//     	GOTO_DESCRIPTOR
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
// 	    GOTO_DESCRIPTOR(WN *wn,WN *label_wn,
//				INT goto_offset,INT label_offset) 
//
//	    BOOL Is_Dismantled
//
//		Does the goto still exist
//
//  	LABEL_DESCRIPTOR
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
//	    LABEL_DESCRIPTOR(WN *label_wn, INT offset)
//
//
//	MAX_GOTO_EXPANDING_TRANSFORMATIONS = 20
//
//	    Some of the transformations increase the code size.  In particular moving
//	gotos out of IFs.  This puts in a circuit breaker to avoid the bad cases.
//	
//

#ifndef opt_goto_INCLUDED
#define opt_goto_INCLUDED "opt_goto.h"

/** $Revision: 1.2 $
*** $Date: 02/11/07 23:41:38-00:00 $
*** $Author: fchow@keyresearch.com $
*** $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.opt_goto.h $
**/

#ifdef _KEEP_RCS_ID
static char *opt_goto_rcs_id = opt_goto_INCLUDED" $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "wn.h"
#include "cxx_memory.h"
#include "cxx_template.h"
#include "cxx_hash.h"

class GOTO_DESCRIPTOR {
public:
  WN *Goto_Wn;
  WN *Label_Wn;
  INT Goto_Offset;
  INT Label_Offset;
  BOOL Is_Dismantled;
  BOOL Is_Compgoto;
  GOTO_DESCRIPTOR(WN *goto_wn,WN *label_wn,INT goto_offset, INT label_offset,
			BOOL is_compgoto) {
    Goto_Wn = goto_wn;
    Label_Wn = label_wn;
    Goto_Offset = goto_offset;
    Label_Offset = label_offset;
    Is_Dismantled = FALSE;
    Is_Compgoto = is_compgoto;
  }
};

class LABEL_DESCRIPTOR {
public:
  WN *Label_Wn;
  INT Offset;
  LABEL_DESCRIPTOR(WN *label_wn,INT offset) {
    Label_Wn = label_wn;
    Offset = offset;
  }
};

typedef STACK<GOTO_DESCRIPTOR> GOTO_DESCRIPTOR_STACK;
typedef STACK<LABEL_DESCRIPTOR> LABEL_DESCRIPTOR_STACK;
typedef STACK<WN *> GOTO_TABLE_WN_STACK;

class GOTO_TABLE {
  GOTO_DESCRIPTOR_STACK _gd;
  GOTO_TABLE_WN_STACK _altentry;
  LABEL_DESCRIPTOR_STACK _bad_label;
  MEM_POOL *_pool;
  WN *_func_nd;
  HASH_TABLE<INT,LABEL_DESCRIPTOR *> *_label_table;
  WN_MAP _parent_map;
  INT _offset;
  BOOL _contains_altentry;
public:
  GOTO_TABLE(WN *func_nd, MEM_POOL *pool) : _gd(pool), _bad_label(pool),
					    _altentry(pool), _pool(pool) {
    _offset = 0;
    _func_nd = func_nd;
    _contains_altentry=FALSE;
    Build();
  }
  void Remove_Gotos();
  void Print(FILE *fp);
  ~GOTO_TABLE()  {  
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
  BOOL Ancestor_Through_If(GOTO_DESCRIPTOR *gd);
  BOOL Parent_Through_If(GOTO_DESCRIPTOR *gd);
  BOOL Sibling(GOTO_DESCRIPTOR *gd);
  void Move_Goto_Out(GOTO_DESCRIPTOR *gd);
  void Replace_Goto_With_If(GOTO_DESCRIPTOR *gd);
  void Replace_Goto_With_While(GOTO_DESCRIPTOR *gd);
  void Move_Into_Else(GOTO_DESCRIPTOR *gd);
  void Create_Truebr(GOTO_DESCRIPTOR *gd);
  void Promote_Do_While(WN *wn);
  WN *Func_Nd() { return _func_nd;};
  BOOL Can_Move_Into_Else(GOTO_DESCRIPTOR *gd);
  BOOL Is_Truebr(GOTO_DESCRIPTOR *gd);
  BOOL Goto_Is_Noop(GOTO_DESCRIPTOR *gd) const;
  INT Find_Level(WN *wn);
  WN *Find_Common_Ancestor(WN *wn1, WN *wn2);
  void Dismantle(WN *bad, WN *parent);
  enum {MAX_GOTO_EXPANDING_TRANSFORMATIONS = 20 };
};

class GOTO_WN_PARENT {
public:
    WN *wn;
    WN *parent;
    GOTO_WN_PARENT(WN *w, WN *p) { wn = w; parent = p;}
};

#endif  // opt_goto_INCLUDED



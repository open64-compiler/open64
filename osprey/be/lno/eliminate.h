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


#ifndef eliminate_INCLUDED
#define eliminate_INCLUDED "eliminate.h"

class GOTO_LIST {
  MEM_POOL* _mem_pool;
  WN* _label;
  INT _label_number;
  DYN_ARRAY<WN*>* _gotos;
public:
  GOTO_LIST(MEM_POOL* mem_pool);
  MEM_POOL* Mem_Pool() {return _mem_pool;};
  WN* Label() {return _label;};
  void Set_Label(WN* label) {_label = label;};
  INT Label_Number() {return _label_number;};
  void Set_Label_Number(INT label_number)
    {_label_number = label_number;};
  INT Elements() {return _gotos->Lastidx() + 1;};
  WN* Goto(INT idx) {return (*_gotos)[idx];};
  void Add_Goto(WN* wn) {_gotos->AddElement(wn);};
  void Add_Goto_Unique(WN* wn);
  void Reset_Targets() {_gotos->Resetidx();};
  void Print(FILE* fp, INT increment = 2);
};

class LABEL_LIST {
  MEM_POOL* _mem_pool;
  DYN_ARRAY<GOTO_LIST>* _labels;
  BOOL _has_assigned_goto;
  void Label_List_Label_Traverse(MEM_POOL* mem_pool, WN* wn_tree);
  void Label_List_Goto_Traverse(MEM_POOL* mem_pool, WN* wn_tree);
  void Remove_Label(WN* wn_label);
  void Remove_Target(WN* wn_target);
public:                                                     
  LABEL_LIST(MEM_POOL* mem_pool);
  LABEL_LIST(MEM_POOL* mem_pool, WN* wn_func);
  MEM_POOL* Mem_Pool() {return _mem_pool;};
  BOOL Has_Assigned_Goto() {return _has_assigned_goto;};
  INT Elements() {return _labels == NULL ? 0 : _labels->Lastidx() + 1;};
  GOTO_LIST* Label(INT i) {return &(*_labels)[i];};
  void Add_Label(WN* wn_label);
  void Add_Label_Unique(WN* wn_label);
  void Add_Goto(WN* wn_goto);
  void Add_Goto_Unique(WN* wn_goto);
  GOTO_LIST* Find_Label_Number(INT label_number);
  BOOL Label_Is_Targeted_Outside_Scope(WN* wn_label);
  BOOL Has_Targeted_Label(WN* wn_tree);
  void Remove_Tree(WN* wn_tree);
  void Print(FILE* fp, INT increment = 2);
};

#endif /* eliminate_INCLUDED */

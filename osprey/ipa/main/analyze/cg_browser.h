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


#ifndef cg_browser_INCLUDED
#define cg_browser_INCLUDED "cg_browser.h"

#ifndef wb_buffer_INCLUDED
#include "wb_buffer.h"
#endif

#ifndef cgb_carray_INCLUDED
#include "cgb_carray.h"
#endif

#ifndef ipa_section_INCLUDED
#include "ipa_section.h"
#endif 

#ifndef cxx_ipa_df_INCLUDED
#include "ipa_df.h"
#endif 

#ifndef cxx_ipa_cprop_INCLUDED
#include "ipa_cprop.h"
#endif 

const INT CGB_MAX_STRING_LENGTH = 1000;
const INT CGB_ASCII_CHAR_COUNT = 256;

const INT CGB_FANCY_MIN = 1;
const INT CGB_FANCY_MAX = 2;

enum CGB_REQUIRED_PIECES {
  CGBR_NONE = 0
};

struct CGB_COMMAND {
  char _command;
  CGB_REQUIRED_PIECES _required_fields;
  CGB_COMMAND* _subcommand;
  const char* _text;
};

class CG_BROWSER { 
private:
  IPA_CALL_GRAPH* _ipa_cg; 
  CGB_COMMAND* _command_list; 
  IPA_NODE* _cnode; 
  NODE_INDEX _cvertex; 
  WB_BUFFER _buffer; 
  CGB_CARRAY _carray; 
  CGB_COMMAND* _old_command_list;  
  MEM_POOL _pool; 
  INT _fancy_level; 
  BOOL _davinci_mode; 
  char _keymap[CGB_ASCII_CHAR_COUNT];
  char _old_keymap[CGB_ASCII_CHAR_COUNT];
  BOOL _is_subcommand; 
protected:  
  CGB_COMMAND* Command_List() {return _command_list;};
  CGB_COMMAND* Old_Command_List() {return _old_command_list;};
  void Set_Old_Command_List(CGB_COMMAND* old_command_list)
    {_old_command_list = old_command_list;};
  WB_BUFFER & Buffer() {return _buffer;};
  CGB_CARRAY & Carray() {return _carray;};
  char Command(INT i) {return (_command_list)[i]._command;};
  CGB_REQUIRED_PIECES Required_Fields(INT i)
    {return (_command_list)[i]._required_fields;}
  const char* Command_Text(INT i) {return (_command_list)[i]._text;}
  CGB_COMMAND* Subcommand(INT i) {return (_command_list)[i]._subcommand;};
  void Set_Subcommand(char ch);
  void Reset_Subcommand();
  BOOL Is_Subcommand() {return _is_subcommand;};
  BOOL Required_Fields_Present(INT i);
  INT Fancy_Level() {return _fancy_level;};
  BOOL Bad_Node(); 
  BOOL Bad_File(); 
  void Fancy_Up();
  void Fancy_Down();
  BOOL DaVinci_Mode() {return _davinci_mode;};
  void DaVinci_Toggle();
  void Address(FILE* fp); 
  void Root(FILE* fp); 
  void Callers(FILE* fp);
  void Callees(FILE* fp); 
  void Edges_In(FILE* fp);
  void Edges_Out(FILE* fp);
  void Set_Node(FILE* fp);
  void Set_Vertex(FILE* fp); 
  void Graph(FILE* fp); 
  void Find(FILE* fp);
  void Whirl_Browser(); 
  void Symbol();
  void Type();
  char* Symbol_Name(INT i, char** function_name = NULL);
  char* Formal_Name(INT i);
  char* Ivar_Name(INT i);
  char* Ivar_Global_Name(INT i, mINT64* offset);
  INT Print_Linex(char* cp, INT cc, LINEX* lx);
  INT Print_Projected_Node_Lower_Bound(char* cp, INT cc, PROJECTED_NODE* pn);
  INT Print_Projected_Node_Upper_Bound(char* cp, INT cc, PROJECTED_NODE* pn);
  INT Print_Projected_Node_Step(char* cp, INT cc, PROJECTED_NODE* pn);
  INT Print_Projected_Node_Segment_Length(char* cp, INT cc, 
    PROJECTED_NODE* pn);
  INT Print_Projected_Node_Segment_Stride(char* cp, INT cc, 
    PROJECTED_NODE* pn);
  INT Print_Projected_Node(char* cp, INT cc, PROJECTED_NODE* pn);
  INT Print_Projected_Array(char* cp, INT cc, PROJECTED_ARRAY* pa);
  INT Print_Projected_Region(char* cp, INT cc, PROJECTED_REGION* pr);
  void Print_Summary_Value(FILE* fp, SUMMARY_VALUE* sv);
  void Mod_Ref(FILE* fp);
  void Values(FILE* fp);
  void Expressions(FILE* fp);
  void Print_Formal_Cprop_Annot(FILE* fp, INT spaces, 
    VALUE_DYN_ARRAY* formal_array); 
  void Cprop_Formals(FILE* fp); 
  void Print_Common_Cprop_Annot(FILE* fp, INT spaces, 
    GLOBAL_ANNOT* global_annot);
  void Cprop_Commons(FILE* fp); 
  void Cprop_Predecessor_Edges(FILE* fp);
  void Cprop_Successor_Edges(FILE* fp);
  void Cprop(FILE* fp); 
  void Summary_Symbol(FILE* fp, INT symbol_index, BOOL is_list);
  void Summary_Ivar(FILE* fp, INT ivar_index);
  void Summary_Ivar_Global(FILE* fp, INT ivar_index);
  void Summary_Formal(FILE* fp, INT formal_index);
  void Summary_Common(FILE* fp, INT common_index);
  void Summary_Common_Shape(FILE* fp, INT common_shape_index);
  void Summary_Global(FILE* fp, INT global_index);
  void Summary_Control_Dependence(FILE* fp, INT control_index);
  void Summary_Callsite(FILE* fp, INT callsite_index);
  void Summary_Actual(FILE* fp, INT actual_index);
  void Summary_Value(FILE* fp, INT value_index);
  void Summary_Cfg_Node(FILE* fp, INT cfg_index);
  void Summary_Scalar(FILE* fp, INT scalar_index);
  void Summary_Region(FILE* fp, INT region_index);
  void Summary_Projected_Region(FILE* fp, INT proj_region_index);
  void Summary_Projected_Node(FILE* fp, INT proj_node_index);
  void Summary_Term(FILE* fp, INT term_index);
  void Summary_Loop_Info(FILE* fp, INT loop_info_index);
  void Summary_Phi(FILE* fp, INT phi_index);
  void Summary_Chi(FILE* fp, INT chi_index);
  void Summary_Expr_Node(FILE* fp, SUMMARY_EXPR* expr, INT kid);
  void Summary_Expr(FILE* fp, INT expr_index);
  void Summary_Tcon(FILE* fp, INT tcon_index);
  void Summary_Stid(FILE* fp, INT stid_index);
  void Summary_Stmt(FILE* fp, INT stmt_index);
  void Summary_Feedback(FILE* fp, INT feedback_index);
  void Summary_Procedure(FILE* fp, INT procedure_index);
  void Summary_Locate(FILE* fp);
  BOOL Summary_Valid_Command(char ch);
  INT Summary_Size(char ch);
  void Summary_Single(FILE* fp, char ch, INT index, BOOL is_list);
  void Summary(FILE* fp);
  void Help();
  void This_Node(FILE* fp, IPA_NODE* ipan, NODE_INDEX vdx);
  void Print_This_Node(FILE* fp, IPA_NODE* ipan, NODE_INDEX vdx);
  void Error_Cleanup();
  BOOL Unmappable_Character(char ch);
  void Initialize_Keymap(char ch);
  void Invoke_Command(char ch);
public: 
  // Initialization
  CG_BROWSER();
  CG_BROWSER(IPA_CALL_GRAPH* ipa_cg, CGB_COMMAND* command_list);
  void Set_Command_List(CGB_COMMAND* command_list)
    {_command_list = command_list;};
  // The following are used to print traces 
  IPA_CALL_GRAPH* Ipa_Cg() {return _ipa_cg;}; 
  void Set_Ipa_Cg(IPA_CALL_GRAPH* ipa_cg) {_ipa_cg = ipa_cg;};
  IPA_NODE* Cnode() {return _cnode;};
  void Set_Cnode(IPA_NODE* cnode) {_cnode = cnode;};
  NODE_INDEX Cvertex() {return _cvertex;};
  void Set_Cvertex(NODE_INDEX v) {_cvertex = v;};
  NODE_INDEX Find_Vertex(IPA_NODE* ipan);
  void Mod_Ref_Formals(FILE* fp); 
  void Mod_Ref_Commons(FILE* fp); 
  void Tlog_Mod_Ref_Formals(); 
  void Tlog_Mod_Ref_Commons(); 
  // Primary interface 
  void Sdebug(const char init_buffer[]);
  void Debug(); 
}; 

#endif /* cg_browser_INCLUDED */

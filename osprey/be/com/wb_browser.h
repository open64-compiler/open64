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


#ifndef wb_browser_INCLUDED
#define wb_browser_INCLUDED "wb_browser.h"

#ifndef wb_buffer_INCLUDED
#include "wb_buffer.h"
#endif 

#ifndef wb_carray_INCLUDED
#include "wb_carray.h"
#endif 

#if defined(BUILD_OS_DARWIN)
#include "darwin_elf.h"
#else /* defined(BUILD_OS_DARWIN) */
#ifndef __SYS_ELF_H__
#include <elf.h>
#endif
#endif /* defined(BUILD_OS_DARWIN) */

#ifndef ipl_summary_INCLUDED
#ifndef UINT16_MAX
#define  UINT16_MAX       (65535u)
#endif
#include "ipl_summary.h"
#endif 

#ifndef ipl_summarize_INCLUDED
#include "ipl_summarize.h"
#endif

#include "dep_graph.h"

class ARRAY_SUMMARY; 
const INT WB_MAX_STRING_LENGTH = 1000;
const INT WB_ASCII_CHAR_COUNT = 256; 

enum WB_SOURCE_LANGUAGE {
  WB_SRC_NONE, 
  WB_SRC_FORTRAN, 
  WB_SRC_C
};

enum WB_REQUIRED_PIECES { 
  WBR_NONE = 0, 
  WBR_DU = 1, 
  WBR_DG = 2, 
  WBR_ALIAS = 4, 
  WBR_PARENT = 8, 
  WBR_AAMAP = 16,
  WBR_REDMAP = 32,
}; 

struct WB_COMMAND { 
  char _command; 
  WB_REQUIRED_PIECES _required_fields; 
  WB_COMMAND* _subcommand; 
  const char* _text; 
}; 

class WB_BROWSER {
private: 
  WN* _global_fd; 
  DU_MANAGER* _du; 
  ALIAS_MANAGER* _alias_mgr; 
  WB_COMMAND* _command_list; 
  WB_COMMAND* _old_command_list; 
  ARRAY_DIRECTED_GRAPH16* _dg; 
  WN_MAP _parent_map; 
  WN_MAP _access_array_map;
  WN_MAP _reduction_map;
  SUMMARIZE<IPL>* _scalar_summary;
  ARRAY_SUMMARY* _array_summary; 
  WB_SOURCE_LANGUAGE _source_language; 
  WN* _cnode; 
  INT _fancy_level; 
  INT _sanity_check_level; 
  BOOL _davinci_mode; 
  PU* _pu; 
  WB_BUFFER _buffer; 
  WB_CARRAY _carray; 
  char _keymap[WB_ASCII_CHAR_COUNT]; 
  char _old_keymap[WB_ASCII_CHAR_COUNT]; 
  BOOL _is_subcommand;  
protected: 
  WB_COMMAND* Command_List() {return _command_list;};
  WB_COMMAND* Old_Command_List() {return _old_command_list;};
  void Set_Old_Command_List(WB_COMMAND* old_command_list)
    {_old_command_list = old_command_list;};
  ARRAY_DIRECTED_GRAPH16* Dg() { return _dg; }
  WN_MAP Parent_Map() { return _parent_map; }
  WN_MAP Access_Array_Map() { return _access_array_map; }
  WN_MAP Reduction_Map() { return _reduction_map; }
  char Command(INT i) { return (_command_list)[i]._command; }
  WB_REQUIRED_PIECES Required_Fields(INT i) 
    { return (_command_list)[i]._required_fields; }
  const char* Command_Text(INT i) { return (_command_list)[i]._text; }
  WB_COMMAND* Subcommand(INT i) {return (_command_list)[i]._subcommand;};
  void Set_Subcommand(char ch);
  void Reset_Subcommand();
  BOOL Is_Subcommand() {return _is_subcommand;};
  BOOL Required_Fields_Present(INT i); 
  SUMMARIZE<IPL>* Scalar_Summary() { return _scalar_summary; }
  ARRAY_SUMMARY* Array_Summary() { return _array_summary; }
  WB_SOURCE_LANGUAGE Source_Language() { return _source_language; } 
  WN* Cnode() { return _cnode; }
  INT Fancy_Level() { return _fancy_level; } 
  INT Sanity_Check_Level() { return _sanity_check_level; } 
  BOOL DaVinci_Mode() { return _davinci_mode; }
  void Set_Source_Language(WB_SOURCE_LANGUAGE source_language) 
    { _source_language = source_language; } 
  void Set_Cnode(WN* cnode) { _cnode = cnode; }; 
  void Set_Fancy_Level(INT fancy_level) { _fancy_level = fancy_level; }
  WB_BUFFER & Buffer() { return _buffer; } 
  WB_CARRAY & Carray() { return _carray; } 
  void Error_Cleanup(); 
  void This_Node(WN* wn, BOOL print_vertex = TRUE, 
    BOOL print_brackets = FALSE); 
  void Print_This_Node(WN* wn, BOOL print_vertex = TRUE,
    BOOL print_brackets = FALSE);
  void Find_Walk(char *s, WN* wn); 
  void Find_Operator_Walk(OPERATOR opr_test, WN* wn); 
  void Access_Array();
  void Reduction_Node(WN* wn, FILE* fp);
  void Reduction_Walk(WN* wn_tree, FILE* fp);
  void Reduction();
  BOOL Aliased_Node(WN* wn);
  void Alias_Walk(WN* wn_test, WN* wn_start, ALIAS_RESULT ar);
  void Dep_Symbol(WN* wn); 
  void Deps_Loop();
  void Deps_Ref();
  void Loops_Walk(WN* wn, FILE* fp, INT spaces, INT increment);
  BOOL Unmappable_Character(char ch);
  void Initialize_Keymap(char ch);
  void Initialize_Language();
  void Address_Walk(WN* wn_tree, INT spaces, INT increment);
  void Root(); 
  void Next(); 
  void Previous(); 
  void Set_Node(); 
  void Address(); 
  void Addresses(); 
  void Fancy_Up(); 
  void Fancy_Down(); 
  void Kids(); 
  void Statements(); 
  void This_Tree(); 
  void Symbol(); 
  void Type(); 
  void Find(); 
  void Find_Symbols(); 
  void Find_Operator(); 
  void Uses(); 
  void Defs();
  void Alias(); 
  void Parent(); 
  void Ancestors(); 
  void Whirl2fc(); 
  void Whirl2fset(); 
  void Whirl2cset(); 
  void Deps(); 
  void Vertices(); 
  void Vertex_Set_Node(); 
  void Loops(); 
  void Set_Map_Id(); 
  void Map_Id(); 
  void DaVinci_Toggle();
  void Summary_Symbol(FILE* fp, INT symbol_index, BOOL is_list); 
  void Summary_Ivar(FILE* fp, INT ivar_index);
  void Summary_Ivar_Global(FILE* fp, INT ivar_index);
  void Summary_Formal(FILE* fp, INT formal_index);
  void Summary_Common(FILE* fp, INT common_index);
  void Summary_Common_Shape(FILE* fp, INT common_shape_index);
  void Summary_Procedure(FILE* fp, INT procedure_index);
  void Summary_Global(FILE* fp, INT global_index);
  void Summary_Callsite(FILE* fp, INT callsite_index);
  void Summary_Control_Dependence(FILE* fp, INT control_index);
  void Summary_Actual(FILE* fp, INT actual_index);
  void Summary_Value(FILE* fp, INT value_index);
  void Summary_Cfg_Node(FILE* fp, INT cfg_index);
  void Summary_Region(FILE* fp, INT region_index);
  void Summary_Projected_Region(FILE* fp, INT proj_region_index);
  void Summary_Projected_Node(FILE* fp, INT proj_node_index);
  void Summary_Term(FILE* fp, INT term_index);
  void Summary_Loop_Info(FILE* fp, INT loop_info_index);
  void Summary_Phi(FILE* fp, INT phi_index);
  void Summary_Chi(FILE* fp, INT chi_index);
  void Summary_Expr(FILE* fp, INT expr_index);
  void Summary_Stid(FILE* fp, INT stid_index);
  void Summary_Stmt(FILE* fp, INT stmt_index);
  void Summary_Feedback(FILE* fp, INT feedback_index);
  BOOL Summary_Valid_Command(char ch);
  BOOL Summary_Scalar_Command(char ch);
  BOOL Summary_Array_Command(char ch);
  INT Summary_Size(char ch);
  void Summary_Single(FILE* fp, char ch, INT index, BOOL is_list);
  void Summary_Locate(FILE* fp);  
  void Help(); 
  void Invoke_Command(char ch); 
public: 
  WB_BROWSER(); 
  WB_BROWSER(WN* global_fd, DU_MANAGER* du, ALIAS_MANAGER* alias_mgr,
    WN_MAP access_arrray_map, WN_MAP reduction_map, PU* pu,
    WB_COMMAND* command_list);
  void Summary(FILE* fp) __attribute__((weak));
  PU* Pu() { return _pu; }
  void Set_Pu(PU* pu) { _pu = pu; }
  WN* Global_Fd() { return _global_fd; }
  void Set_Global_Fd(WN* global_fd) { _global_fd = global_fd; }
  DU_MANAGER* Du() { return _du; } 
  void Set_Du(DU_MANAGER* du) { _du = du; } 
  ALIAS_MANAGER* Alias_Mgr() { return _alias_mgr; }
  void Set_Alias_Manager(ALIAS_MANAGER* alias_mgr) 
    { _alias_mgr = alias_mgr; } 
  void Set_Command_List(WB_COMMAND* command_list) 
    { _command_list = command_list; } 
  void Set_Dg(ARRAY_DIRECTED_GRAPH16* dg) { _dg = dg; }
  void Set_Parent_Map(WN_MAP parent_map) { _parent_map = parent_map; }
  void Set_Access_Array_Map(WN_MAP access_array_map) 
    { _access_array_map = access_array_map; }
  void Set_Reduction_Map(WN_MAP reduction_map) 
    { _reduction_map = reduction_map; }
  void Set_Scalar_Summary(SUMMARIZE<IPL>* scalar_summary) 
    { _scalar_summary = scalar_summary; }
  void Set_Array_Summary(ARRAY_SUMMARY* array_summary) 
    { _array_summary = array_summary; }
  void Set_Sanity_Check_Level(INT sanity_check_level) 
    { _sanity_check_level = sanity_check_level; }
  void Sdebug(const char init_buffer[]);
  void Debug(); 
}; 

// Summary is actually only defined in ipa,
// so create external name that we can do weak naming trick on.
extern "C" void WB_BROWSER_Summary (FILE *fp, WB_BROWSER *wb);

#endif /* wb_browser_INCLUDED */

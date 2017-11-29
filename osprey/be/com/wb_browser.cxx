/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2005-2007 NVIDIA Corporation.  All rights reserved.
 */

/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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



#include <stdint.h>
#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <sys/types.h>
#if defined(BUILD_OS_DARWIN)
#include "darwin_elf.h"
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <ctype.h>
#include "wn.h"
#include "wn_map.h"
#include "wn_util.h" 
#include <stdio.h>
#include "opt_du.h"
#include "opt_alias_mgr.h"
#include "dep_graph.h"
#include "ir_reader.h"
#include "access_vector.h"
#include "if_info.h"
#include "loop_info.h"
#include "wb_util.h"
#include "wb_buffer.h"
#include "wb_carray.h"
#include "wb_browser.h"
#include "whirl2src.h"

#if defined(__linux__) || !defined(SHARED_BUILD)
extern void (*Print_ACCESS_ARRAY_p)(FILE *fp, ACCESS_ARRAY *a);
#define Print_ACCESS_ARRAY (*Print_ACCESS_ARRAY_p)
extern void (*Print_IF_INFO_p)(FILE *fp, IF_INFO *i);
#define Print_IF_INFO (*Print_IF_INFO_p)
extern void (*WB_BROWSER_Summary_p)(FILE *fp, WB_BROWSER *wb);
#define WB_BROWSER_Summary (*WB_BROWSER_Summary_p)
extern void (*Print_DO_LOOP_INFO_BASE_p)(FILE *fp, DO_LOOP_INFO_BASE *b);
#define Print_DO_LOOP_INFO_BASE (*Print_DO_LOOP_INFO_BASE_p)
#else
#pragma weak Print_ACCESS_ARRAY
#pragma weak Print_IF_INFO
#pragma weak Print_DO_LOOP_INFO_BASE
#pragma weak WB_BROWSER_Summary
#pragma weak WB_BROWSER_Summary_p
#endif


static const char *operator_table[OPERATOR_LAST + 1] =
{
  "UNKNOWN",
  "ABS",
  "ADD",
  "AGOTO",
  "ALTENTRY",
  "ARRAY",
  "ARRAYEXP",
  "ARRSECTION",
  "ASHR",
  "ASSERT",
  "BACKWARD_BARRIER",
  "BAND",
  "BIOR",
  "BLOCK",
  "BNOR",
  "BNOT",
  "BXOR",
  "CALL",
  "CAND",
  "CASEGOTO",
  "CEIL",
  "CIOR",
  "COMMA",
  "COMMENT",
  "COMPGOTO",
  "COMPLEX",
  "CONST",
  "CSELECT",
  "CVT",
  "CVTL",
  "DIV",
  "DIVREM",
  "DO_LOOP",
  "DO_WHILE",
  "EQ",
  "EVAL",
  "EXC_SCOPE_BEGIN",
  "EXC_SCOPE_END",
  "FALSEBR",
  "FLOOR",
  "FORWARD_BARRIER",
  "FUNC_ENTRY",
  "GE",
  "GOTO",
  "GT",
  "HIGHMPY",
  "HIGHPART",
  "ICALL",
  "IDNAME",
  "IF",
  "ILDA",
  "ILDBITS",
  "ILOAD",
  "ILOADX",
  "IMAGPART",
  "INTCONST",
  "INTRINSIC_CALL",
  "INTRINSIC_OP",
  "IO",
  "IO_ITEM",
  "ISTBITS",
  "ISTORE",
  "ISTOREX",
  "LABEL",
  "LAND",
  "LDA",
  "LDBITS",
  "LDID",
  "LE",
  "LIOR",
  "LNOT",
  "LOOP_INFO",
  "LOWPART",
  "LSHR",
  "LT",
  "MADD",
  "MAX",
  "MAXPART",
  "MIN",
  "MINMAX",
  "MINPART",
  "MLOAD",
  "MOD",
  "MPY",
  "MSTORE",
  "MSUB",
  "NE",
  "NEG",
  "NMADD",
  "NMSUB",
  "OPTPARM",
  "OPT_CHI",
  "OPT_RESERVE2",
  "PAREN",
  "PARM",
  "PICCALL",
  "PRAGMA",
  "PREFETCH",
  "PREFETCHX",
  "RCOMMA",
  "REALPART",
  "RECIP",
  "REGION",
  "REGION_EXIT",
  "REM",
  "RETURN",
  "RETURN_VAL",
  "RND",
  "RSQRT",
  "SELECT",
  "SHL",
  "SQRT",
  "STBITS",
  "STID",
  "SUB",
  "SWITCH",
  "TAS",
  "TRAP",
  "TRIPLET",
  "TRUEBR",
  "TRUNC",
  "VFCALL",
  "WHERE",
  "WHILE_DO",
  "XGOTO",
  "XMPY",
  "XPRAGMA",
  "AFFIRM",
  "ALLOCA",
  "DEALLOCA",
  "LDMA"
};

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::WB_BROWSER
// FUNCTION: Default constructor for Whirl Browser 
//-----------------------------------------------------------------------

WB_BROWSER::WB_BROWSER()
{
   _global_fd = NULL; 
   _du = NULL; 
   _alias_mgr = NULL; 
   _command_list = NULL; 
   _old_command_list = NULL; 
   _is_subcommand = FALSE; 
   _dg = NULL; 
   _parent_map = WN_MAP_UNDEFINED; 
   _access_array_map = WN_MAP_UNDEFINED;
   _reduction_map = WN_MAP_UNDEFINED;
   _pu = NULL; 
   _scalar_summary = NULL; 
   _array_summary = NULL; 
   _cnode = NULL; 
   _fancy_level = 2; 
   _source_language = WB_SRC_NONE; 
   _sanity_check_level = 0; 
   for (INT i = 0; i < WB_ASCII_CHAR_COUNT; i++) 
     _keymap[i] = i; 
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Unmappable_Character
// FUNCTION: Return TRUE if the character 'ch' is not remappable in the
//   .wb_keymap file.  Return FALSE otherwise.
//-----------------------------------------------------------------------

BOOL WB_BROWSER::Unmappable_Character(char ch)
{
  switch (ch) {
  case ' ':
  case '\t':
  case '\n':
  case 'Q':
  case 'q':
  case 'H':
  case 'h':
    return TRUE;
  default:
    return FALSE;
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::WB_BROWSER
// FUNCTION: Constructor for Whirl Browser 
//-----------------------------------------------------------------------

WB_BROWSER::WB_BROWSER(WN* global_fd, 
 		       DU_MANAGER* du, 
 		       ALIAS_MANAGER* alias_mgr,
		       WN_MAP access_array_map, 
		       WN_MAP reduction_map, 
		       PU* pu, 
		       WB_COMMAND* command_list) : 
   _global_fd(global_fd), _du(du), _alias_mgr(alias_mgr),
   _access_array_map(access_array_map), _reduction_map(reduction_map), _pu(pu),
   _command_list(command_list)
{
   _old_command_list = NULL; 
   _is_subcommand = FALSE; 
   _dg = NULL; 
   _parent_map = WN_MAP_UNDEFINED; 
   _scalar_summary = NULL; 
   _array_summary = NULL; 
   _cnode = global_fd; 
   _fancy_level = 2; 
   _source_language = WB_SRC_NONE; 
   _sanity_check_level = 0; 
   for (INT i = 0; i < WB_ASCII_CHAR_COUNT; i++) 
     _keymap[i] = i; 
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::This_Node
// FUNCTION: Write the address of 'wn' and its whirl symbol to 'stdout'.
//-----------------------------------------------------------------------

void WB_BROWSER::This_Node(WN* wn, 
		           BOOL print_vertex,
			   BOOL print_brackets)
{
  if (wn == NULL) {
    fprintf(stdout, "<NULL>");
    return;
  }
  const char* ch = OPCODE_name(WN_opcode(wn));
  if (print_brackets)
    fprintf(stdout, "[0x%p] ", wn);
  else 
    fprintf(stdout, "0x%p ", wn);
  if (print_vertex && Dg() != NULL && Dg()->Get_Vertex(wn) != 0)
    fprintf(stdout, "V#%d ", Dg()->Get_Vertex(wn));
  fprintf(stdout, "%s ", ch);
  if (Fancy_Level() >= 3)
    if (OPCODE_has_next_prev(WN_opcode(wn)))
      fprintf(stdout, "(%d) ", Srcpos_To_Line(WN_linenum(wn)));
  if (WN_operator(wn) == OPR_INTCONST) {
    fprintf(stdout, "%lld ", WN_const_val(wn));
  } else {
    const char* wn_symbol = WB_Whirl_Symbol(wn);
    if (wn_symbol != NULL)
      fprintf(stdout, "%s ", wn_symbol);
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Print_This_Node
// FUNCTION: Print the node 'wn' and a newline.
//-----------------------------------------------------------------------

void WB_BROWSER::Print_This_Node(WN* wn, 
			         BOOL print_vertex,
				 BOOL print_brackets)
{
  This_Node(wn, print_vertex, print_brackets);
  fprintf(stdout, "\n");
}

//-----------------------------------------------------------------------
// NAME: Address_Walk
// FUNCTION: Walk the tree rooted at 'wn_tree', printing the adddresses
//   of all of the nodes in the subtree, with a short description of each
//   node.
//-----------------------------------------------------------------------

void WB_BROWSER::Address_Walk(WN* wn_tree,
                              INT spaces,
                              INT increment)
{ 
  for (INT i = 0; i < spaces; i++)
    fprintf(stdout, " ");
  Print_This_Node(wn_tree, FALSE, TRUE);

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Address_Walk(wn, spaces + increment, increment);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Address_Walk(WN_kid(wn_tree, i), spaces + increment, increment);
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Addresses
// WHIRL BROWSER COMMAND: '%'
// FUNCTION: Print the addresses of nodes in this subtree
//-----------------------------------------------------------------------

void WB_BROWSER::Addresses()
{
  Address_Walk(Cnode(), 0, 2);
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Error_Cleanup
// FUNCTION: Sound the bell, write the prompt to 'stdout' and reload the
//   input buffer, scanning past blanks and tabs.
//-----------------------------------------------------------------------

void WB_BROWSER::Error_Cleanup()
{ 
  WB_Bell();
  WB_Prompt();
  Buffer().Load_Buffer();
  Buffer().Scan_Blanks_And_Tabs();
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Root
// WHIRL BROWSER COMMAND: 'R'
// FUNCTION: Go to the root of the program unit
//-----------------------------------------------------------------------

void WB_BROWSER::Root()
{
  Set_Cnode(Global_Fd());
  Print_This_Node(Cnode());
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Next
// WHIRL BROWSER COMMAND: 'N'
// FUNCTION: Go to the next node in the subtree chain
//-----------------------------------------------------------------------

void WB_BROWSER::Next()
{
  WN* pnode = WN_next(Cnode());
  if (pnode == NULL) {
    Error_Cleanup();
    return;
  }
  Set_Cnode(pnode);
  Print_This_Node(Cnode());
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Previous
// WHIRL BROWSER COMMAND: 'P'
// FUNCTION: Go to the previous node in the subtree chain
//-----------------------------------------------------------------------

void WB_BROWSER::Previous()
{
  WN* pnode = WN_prev(Cnode());
  if (pnode == NULL)  {
    Error_Cleanup();
    return;
  }
  Set_Cnode(pnode);
  Print_This_Node(Cnode());
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Set_Node()
// WHIRL BROWSER COMMAND: '='
// FUNCTION: Set the current node to the following address
//-----------------------------------------------------------------------

void WB_BROWSER::Set_Node()
{
  INT node;
  Buffer().Scan_HexInteger(&node);
  Set_Cnode((WN*)(INTPTR) node);
  Print_This_Node(Cnode());
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Address()
// WHIRL BROWSER COMMAND: '@'
// FUNCTION: Set the current node to this numbered node
//-----------------------------------------------------------------------

void WB_BROWSER::Address()
{
  INT integer;
  Buffer().Scan_Integer(&integer);
  if (integer < 0 || integer >= Carray().Next_Index()) {
    Error_Cleanup();
    return;
  }
  Set_Cnode(Carray().Element(integer));
  Print_This_Node(Cnode());
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Fancy_Up()
// WHIRL BROWSER COMMAND: '>'
// FUNCTION: Increase fanciness level for printing
//-----------------------------------------------------------------------

void WB_BROWSER::Fancy_Up()
{
  const INT fancy_max = 3; 
  if (Fancy_Level() == fancy_max) { 
    Error_Cleanup();
    return;
  }
  INT fancy_level = Fancy_Level(); 
  Set_Fancy_Level(++fancy_level);
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Fancy_Down()
// WHIRL BROWSER COMMAND: '<'
// FUNCTION: Decrease fanciness level for printing
//-----------------------------------------------------------------------

void WB_BROWSER::Fancy_Down()
{
  const INT fancy_min = 2; 
  if (Fancy_Level() == fancy_min) { 
    Error_Cleanup();
    return;
  }
  INT fancy_level = Fancy_Level(); 
  Set_Fancy_Level(--fancy_level);
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Kids()
// WHIRL BROWSER COMMAND: 'K'
// FUNCTION: Print the kids of this node
//-----------------------------------------------------------------------

void WB_BROWSER::Kids()
{
  if (WN_kid_count(Cnode()) == 0) {
    Error_Cleanup();
    return;
  }
  Carray().Reset_Index(); 
  for (INT i = 0; i < WN_kid_count(Cnode()); i++) {
    WN* kid = WN_kid(Cnode(), i);
    fprintf(stdout, "[%d] ", i);
    This_Node(kid); 
    Carray().Enter_This_Node(kid);
    fprintf(stdout, "\n");
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Statements()
// WHIRL BROWSER COMMAND: 'S'
// FUNCTION: Print the list of statements under this node
//-----------------------------------------------------------------------

void WB_BROWSER::Statements()
{
  INT i = 0;
  if (WN_opcode(Cnode()) != OPC_BLOCK) {
    Error_Cleanup();
    return;
  }
  if (WN_first(Cnode()) == NULL) {
    Error_Cleanup();
    return;
  }
  Carray().Reset_Index(); 
  for (WN* node = WN_first(Cnode()); node != NULL; i++, node = WN_next(node)) {
    fprintf(stdout, "[%d] ", i);
    This_Node(node); 
    Carray().Enter_This_Node(node);
    fprintf(stdout, "\n");
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::This_Tree()
// WHIRL BROWSER COMMAND: 'T'
// FUNCTION: Print the tree at the current node
//-----------------------------------------------------------------------

void WB_BROWSER::This_Tree()
{
  if (Cnode() == NULL) {
    Error_Cleanup();
    return;
  }
  dump_tree(Cnode());
}

//-----------------------------------------------------------------------
// NAME: Compute_ST_IDX
// FUNCTION: Convert a <'st_level','st_index'> to its ST_IDX and return
//   the value.
//-----------------------------------------------------------------------

extern ST_IDX Compute_ST_IDX(UINT32 st_level,
                             UINT32 st_index)
{
  UINT32 st_idx = st_level + (st_index << 8);
  return (ST_IDX) (st_idx);
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Symbol()
// WHIRL BROWSER COMMAND: 's'
// FUNCTION: Print the symbol table entry for this node
//-----------------------------------------------------------------------

void WB_BROWSER::Symbol()
{
  if (Buffer().Is('<')) { 
    UINT32 st_level;
    UINT32 st_index;
    Buffer().Scan_Character();
    Buffer().Scan_Unsigned(&st_level);
    char separator = Buffer().Scan_Character();
    if (separator != ',') {
      Error_Cleanup();
      return;
    }
    Buffer().Scan_Unsigned(&st_index);
    char right_angle_bracket = Buffer().Scan_Character();
    if (right_angle_bracket != '>') {
      Error_Cleanup();
      return; 
    }
    ST_IDX st_idx = Compute_ST_IDX(st_level, st_index);
    ST* st = &St_Table[st_idx];
    fprintf(stdout, "ST_IDX: %d\n", st_idx);
    Print_ST(stdout, st, TRUE);
  } else if (Buffer().Is_Integer()) { 
    ST_IDX st_idx;
    Buffer().Scan_Unsigned(&st_idx);
    ST* st = &St_Table[st_idx];
    fprintf(stdout, "ST_IDX: %d\n", st_idx);
    Print_ST(stdout, st, TRUE);
  } else {  
    if (!OPCODE_has_sym(WN_opcode(Cnode()))) {
      Error_Cleanup();
      return;
    }
    Print_ST(stdout, WN_st(Cnode()), TRUE);
  } 
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Type()
// WHIRL BROWSER COMMAND: 't'
// FUNCTION: Print the type table entry for this node
//-----------------------------------------------------------------------

void WB_BROWSER::Type()
{
  if (Buffer().Is('<')) { 
    UINT32 ty_index = 0;
    Buffer().Scan_Character();
    Buffer().Scan_Unsigned(&ty_index);
    char right_angle_bracket = Buffer().Scan_Character();
    if (right_angle_bracket != '>') {
      Error_Cleanup();
      return;
    } 
    Ty_tab[ty_index].Print(stdout);
  } else if (Buffer().Is_Integer()) { 
    TY_IDX ty_idx = 0;
    Buffer().Scan_Unsigned(&ty_idx);
    Print_TY(stdout, ty_idx);
  } else { 
    if (!OPCODE_has_sym(WN_opcode(Cnode()))) {
      Error_Cleanup();
      return;
    }
    Ty_Table[ST_type(WN_st(Cnode()))].Print(stdout);
  }
}

//-----------------------------------------------------------------------
// NAME: Is_Substring
// FUNCTION: Return TRUE if 's1' is a substring of 's2', return FALSE
//   otherwise.
//-----------------------------------------------------------------------

static BOOL Is_Substring(char s1[],
                         char s2[])
{
  INT substring_length = strlen(s1);
  INT string_length = strlen(s2);
  INT difference = string_length - substring_length;
  if (substring_length > string_length)
    return FALSE;
  INT i, j;
  for (i = 0; i <= difference; i++) {
    for (j = 0; j < substring_length; j++)
      if (s1[j] != s2[i + j])
        break;
    if (j == substring_length)
      return TRUE;
  }
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Find()
// WHIRL BROWSER COMMAND: 'F'
// FUNCTION: Find nodes with this symbol in the subtree
//-----------------------------------------------------------------------

void WB_BROWSER::Find_Walk(char *s, WN* wn)
{
  if (wn == NULL)
    return;
  char* wn_symbol = (char *) WB_Whirl_Symbol(wn);
  BOOL test_substring = s[0] == '\'';
  if (wn_symbol != NULL && (!test_substring && strcmp(wn_symbol, s) == 0
      || test_substring && Is_Substring(&s[1], wn_symbol))) {
    fprintf(stdout, "[%d] ", Carray().Next_Index());
    This_Node(wn); 
    Carray().Enter_This_Node(wn);
    fprintf(stdout, "\n");
  }
  for (INT i = 0; i < WN_kid_count(wn); i++)
    Find_Walk(s, WN_kid(wn, i));
  if (WN_opcode(wn) == OPC_BLOCK)
    for (WN* wn_sub = WN_first(wn); wn_sub != NULL; wn_sub = WN_next(wn_sub))
      Find_Walk(s, wn_sub);
}

void WB_BROWSER::Find()
{
  char s[WB_MAX_STRING_LENGTH];
  Buffer().Scan_Alphanumeric(s);
  Carray().Reset_Index(); 
  Find_Walk(s, Cnode());
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Find_Operator()
// WHIRL BROWSER COMMAND: 'o'
// FUNCTION: Find nodes with this operator in the subtree
//-----------------------------------------------------------------------

void WB_BROWSER::Find_Operator_Walk(OPERATOR opr_test, WN* wn)
{
  if (wn == NULL)
    return;
  if (WN_operator(wn) == opr_test) {
    fprintf(stdout, "[%d] ", Carray().Next_Index());
    This_Node(wn); 
    Carray().Enter_This_Node(wn);
    fprintf(stdout, "\n");
  }
  if (WN_opcode(wn) == OPC_BLOCK) { 
    for (WN* wn_sub = WN_first(wn); wn_sub != NULL; wn_sub = WN_next(wn_sub))
      Find_Operator_Walk(opr_test, wn_sub);
  } else { 
    for (INT i = 0; i < WN_kid_count(wn); i++)
      Find_Operator_Walk(opr_test, WN_kid(wn, i));
  } 
}

void WB_BROWSER::Find_Operator()
{
  char s[WB_MAX_STRING_LENGTH];
  Buffer().Scan_Alphanumeric(s);
  INT i;
  for (i = 1; i <= OPERATOR_LAST; i++)
    if (!strcmp(s, operator_table[i]))
      break;
  if (i > OPERATOR_LAST) {
    Error_Cleanup();
    return; 
  } 
  Carray().Reset_Index(); 
  Find_Operator_Walk((OPERATOR) i, Cnode());
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Uses()
// WHIRL BROWSER COMMAND: 'U'
// FUNCTION: Print the uses at the current node
//-----------------------------------------------------------------------

void WB_BROWSER::Uses()
{
  if (Du() == NULL) {
    Error_Cleanup();
    return;
  }
  USE_LIST *use_list = Du()->Du_Get_Use(Cnode());
  if (use_list == NULL) {
    Error_Cleanup();
    return;
  }
  Carray().Reset_Index();
  if (use_list->Incomplete())
    fprintf(stdout, "WARNING: USE LIST INCOMPLETE\n");
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = NULL;
  INT i = 0;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* use = node->Wn();
    fprintf(stdout, "[%d] ", i++);
    Print_This_Node(use);
    Carray().Enter_This_Node(use);
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Defs
// WHIRL BROWSER COMMAND: 'D'
// FUNCTION: Print the definitions at the current node
//-----------------------------------------------------------------------

void WB_BROWSER::Defs()
{
  if (Du() == NULL) {
    Error_Cleanup();
    return;
  }
  DEF_LIST* def_list = Du()->Ud_Get_Def(Cnode());
  if (def_list == NULL) {
    Error_Cleanup();
    return;
  }
  if (def_list->Incomplete())
    fprintf(stdout, "WARNING: DEF LIST INCOMPLETE\n");
  fprintf(stdout, "Loop Statement: 0x%p\n", def_list->Loop_stmt());
  DEF_LIST_ITER iter(def_list);
  const DU_NODE* node = NULL;
  INT i = 0;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* def = node->Wn();
    fprintf(stdout, "[%d] ", i++);
    Print_This_Node(def);
    Carray().Enter_This_Node(def);
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Access_Array()
// WHIRL BROWSER COMMAND: 'A'
// FUNCTION: Print the access array info for this node
//-----------------------------------------------------------------------

void WB_BROWSER::Access_Array()
{
  if (Access_Array_Map() == -1) { 
    Error_Cleanup();
    return; 
  } 
  if (WN_operator(Cnode()) == OPR_ARRAY) { 
    ACCESS_ARRAY *array 
      = (ACCESS_ARRAY *) WN_MAP_Get(Access_Array_Map(), Cnode());
    if (array != NULL) {
      fprintf(stdout, "The access array is \n"); 
      Print_ACCESS_ARRAY (stdout, array);
    } else {
      fprintf(stdout, "Null ACCESS_ARRAY\n");
    }
  } else if (WN_operator(Cnode()) == OPR_IF) { 
    IF_INFO *info = (IF_INFO *) WN_MAP_Get(Access_Array_Map(), Cnode());
    if (info != NULL) {
      fprintf(stdout, "The if info is \n"); 
      Print_IF_INFO(stdout, info);
    } else {
      fprintf(stdout, "Null IF_INFO\n");
    }
  } else if (WN_operator(Cnode()) == OPR_DO_LOOP) { 
    DO_LOOP_INFO_BASE* info 
      = (DO_LOOP_INFO_BASE*) WN_MAP_Get(Access_Array_Map(), Cnode());
    if (info != NULL) {
      fprintf(stdout, "The loop info is \n");
      Print_DO_LOOP_INFO_BASE(stdout, info);	
    } else {
      fprintf(stdout, "NulleDO_LOOP_INFO_BASE\n");
    }
  } else { 
    Error_Cleanup();
  }  
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Reduction()
// WHIRL BROWSER COMMAND: 'r'
// FUNCTION: Print the reduction manager
//-----------------------------------------------------------------------

void WB_BROWSER::Reduction_Node(WN* wn, 
				FILE* fp)
{ 
  if (OPCODE_is_load(WN_opcode(wn)) || OPCODE_is_store(WN_opcode(wn))) {
    INT32 red_type = WN_MAP32_Get(Reduction_Map(), wn);
    switch (red_type) {
    case RED_ADD:
      fprintf(fp, "[%d] 0x%p RED_ADD ", Carray().Next_Index(), wn);
      break;
    case RED_MPY:
      fprintf(fp, "[%d] 0x%p RED_MPY ", Carray().Next_Index(), wn);
      break;
    case RED_MIN:
      fprintf(fp, "[%d] 0x%p RED_MIN ", Carray().Next_Index(), wn);
      break;
    case RED_MAX:
      fprintf(fp, "[%d] 0x%p RED_MAX ", Carray().Next_Index(), wn);
      break;
    }
    switch (red_type) {
    case RED_ADD:
    case RED_MPY:
    case RED_MIN:
    case RED_MAX:
      OPERATOR oper = WN_operator(wn);
      if (oper == OPR_ILOAD || oper == OPR_ISTORE) {
	Dep_Symbol(wn);
	fprintf(fp, "\n");
      } else {
	const char *name = WB_Whirl_Symbol(wn);
	fprintf(fp, "%s\n", name);
      }
      Carray().Enter_This_Node(wn);
      break;
    }
  }
} 

void WB_BROWSER::Reduction_Walk(WN* wn_tree, 
				FILE* fp) 
{
  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))  
      Reduction_Walk(wn, fp);
  } else { 
    Reduction_Node(wn_tree, fp);
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Reduction_Walk(WN_kid(wn_tree, i), fp);
  } 
} 

void WB_BROWSER::Reduction()
{ 
  if (Reduction_Map() == WN_MAP_UNDEFINED) { 
    Error_Cleanup();
    return;
  } 
  Carray().Reset_Index();
  Reduction_Walk(Cnode(), stdout);
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Alias()
// WHIRL BROWSER COMMAND: 'a'
// FUNCTION: Print the alias info for this node
//-----------------------------------------------------------------------

BOOL WB_BROWSER::Aliased_Node(WN* wn)
{
  OPCODE opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);
  return (OPCODE_is_load(opc) || OPCODE_is_store(opc) || opr == OPR_PARM)
    && Alias_Mgr()->Id(wn) != 0;
}

void WB_BROWSER::Alias_Walk(WN* wn_test,
                            WN* wn_start,
                            ALIAS_RESULT ar)
{
  if (Aliased_Node(wn_start)) {
    ALIAS_RESULT result = Aliased(Alias_Mgr(), wn_test, wn_start);
    switch (result) {
    case NOT_ALIASED:
      break;
    case POSSIBLY_ALIASED:
    case SAME_LOCATION:
      if (ar == result) {
        fprintf(stdout, "  [%d] ", Carray().Next_Index());
        Print_This_Node(wn_start);
        Carray().Enter_This_Node(wn_start);
      }
      break;
    }
  }

  if (WN_opcode(wn_start) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_start); wn != NULL; wn = WN_next(wn))
      Alias_Walk(wn_test, wn, ar);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_start); i++)
      Alias_Walk(wn_test, WN_kid(wn_start, i), ar);
  }
}

void WB_BROWSER::Alias()
{
  if (!Aliased_Node(Cnode())) {
    Error_Cleanup();
    return;
  }
  Carray().Reset_Index();
  fprintf(stdout, "POSSIBLY ALIASED: \n");
  Alias_Walk(Cnode(), Global_Fd(), POSSIBLY_ALIASED);
  if (Carray().Next_Index() == 0)
    fprintf(stdout, "  NO LOCATIONS\n");
  INT possible_aliases = Carray().Next_Index();
  fprintf(stdout, "SAME LOCATION: \n");
  Alias_Walk(Cnode(), Global_Fd(), SAME_LOCATION);
  if (Carray().Next_Index() == possible_aliases)
    fprintf(stdout, "  NO LOCATIONS\n");
}

//-----------------------------------------------------------------------
// NAME: WB_Parent_Search
// FUNCTION: For the tree rooted at 'wn_root', find the path of nodes
//   leading from this node to 'wn_node' and push it on the stack
//   'stk_parent'.  If there is no path from 'wn_root' to 'wn_node',
//   return an empty stack.
//-----------------------------------------------------------------------

static BOOL WB_Parent_Search(WN* wn_root,
                             STACK<WN*>* stk_parent,
                             WN* wn_node)
{
  stk_parent->Push(wn_root);
  if (wn_root == wn_node)
    return TRUE;
  if (WN_opcode(wn_root) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_root); wn != NULL; wn = WN_next(wn)) {
      BOOL found_path = WB_Parent_Search(wn, stk_parent, wn_node);
      if (found_path)
        return TRUE;
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn_root); i++) {
      BOOL found_path = WB_Parent_Search(WN_kid(wn_root, i), stk_parent,
        wn_node);
      if (found_path)
        return TRUE;
    }
  }
  stk_parent->Pop();
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Parent
// WHIRL BROWSER COMMAND: 'E'
// FUNCTION: Go to the parent of the current node
//-----------------------------------------------------------------------

void WB_BROWSER::Parent()
{
  if (Cnode() == Global_Fd()) {
    Error_Cleanup();
    return;
  }
  if (Parent_Map() == -1) {
    MEM_POOL_Push(&MEM_local_pool);
    STACK<WN*> stk_parent(&MEM_local_pool);
    BOOL found_path = WB_Parent_Search(Global_Fd(), &stk_parent, Cnode());
    if (!found_path) {
      Error_Cleanup();
      MEM_POOL_Pop(&MEM_local_pool);
      return;
    }
    Set_Cnode(stk_parent.Bottom_nth(stk_parent.Elements() - 2));
    Print_This_Node(Cnode());
    MEM_POOL_Pop(&MEM_local_pool);
  } else {
    WN* pnode = (WN*) WN_MAP_Get(Parent_Map(), Cnode());
    if (pnode == NULL) {
      Error_Cleanup();
      return;
    }
    Set_Cnode(pnode);
    Print_This_Node(Cnode());
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER:Ancestors
// WHIRL BROWSER COMMAND: 'e'
// FUNCTION: Print the ancestors of this node
//-----------------------------------------------------------------------

void WB_BROWSER::Ancestors()
{
  if (Cnode() == Global_Fd()) {
    Error_Cleanup();
    return;
  }
  if (Parent_Map() == -1) {
    Carray().Reset_Index();
    MEM_POOL_Push(&MEM_local_pool);
    STACK<WN*> stk_parent(&MEM_local_pool);
    BOOL found_path = WB_Parent_Search(Global_Fd(), &stk_parent, Cnode());
    if (!found_path) {
      Error_Cleanup();
      MEM_POOL_Pop(&MEM_local_pool);
      return;
    }
    INT i = 0; 
    for (INT j = stk_parent.Elements() - 1; j >= 0; j--) {
      Carray().Enter_This_Node(stk_parent.Bottom_nth(j));
      fprintf(stdout, "[%d] ", i++);
      Print_This_Node(stk_parent.Bottom_nth(j));
    }
    MEM_POOL_Pop(&MEM_local_pool);
  } else {
    Carray().Reset_Index();
    WN* wnn = NULL; 
    INT i = 0; 
    for (WN* wn = Cnode(); wn != NULL; wn = wnn) {
      wnn = ((WN*) WN_MAP_Get(Parent_Map(), (WN*) wn)); 
      Carray().Enter_This_Node(wn);
      fprintf(stdout, "[%d] ", i++);
      Print_This_Node(wn);
    }
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Whirl2fc()
// WHIRL BROWSER COMMAND: 'W'
// FUNCTION: Print tree in whirl2[fc] format at current node
//-----------------------------------------------------------------------

void WB_BROWSER::Whirl2fc()
{
  if (Source_Language() == WB_SRC_NONE) {
    Whirl2Src_Init(Global_Fd());
    Whirl2Src_Emit(stdout, Cnode());
    fprintf(stdout, "\n");
  } else if (Source_Language() == WB_SRC_FORTRAN) {
    Whirl2F_Init(Global_Fd());
    Whirl2F_Emit(stdout, Cnode());
    fprintf(stdout, "\n");
  } else if (Source_Language() == WB_SRC_C) {
    Whirl2C_Init(Global_Fd());
    Whirl2C_Emit(stdout, Cnode());
    fprintf(stdout, "\n");
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Whirl2fset
// WHIRL BROWSER COMMAND: 'f'
// FUNCTION: Change WHIRL-TO-SOURCE language to FORTRAN
//-----------------------------------------------------------------------

void WB_BROWSER::Whirl2fset()
{
  if (Source_Language() != WB_SRC_FORTRAN) {
    Whirl2F_Init(Global_Fd());
    Set_Source_Language(WB_SRC_FORTRAN);
  }
  fprintf(stdout, "WHIRL-TO-SOURCE language is FORTRAN.\n");
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Whirl2cset
// WHIRL BROWSER COMMAND: 'c'
// FUNCTION: Change WHIRL-TO-SOURCE language to C
//-----------------------------------------------------------------------

void WB_BROWSER::Whirl2cset()
{
  if (Source_Language() != WB_SRC_C) {
    Whirl2C_Init(Global_Fd());
    Set_Source_Language(WB_SRC_C);
  }
  fprintf(stdout, "WHIRL-TO-SOURCE language is C.\n");
}

//-----------------------------------------------------------------------
// NAME: Compact_Buffer
// FUNCTION: Compact 'buffer' by replacing strings like '+-' with '-'
//   and '--' with '+'.
//-----------------------------------------------------------------------

static void Compact_Buffer(char buffer[])
{
  INT i, j;
  for (i = 0, j = 0; buffer[i] != '\0'; i++) {
    if (buffer[i] == '+' && buffer[i+1] == '-') {
      buffer[j++] = '-';
      i++;
    } else if (buffer[i] == '-' && buffer[i+1] == '-') {
      buffer[j++] = '+';
      i++;
    } else {
      buffer[j++] = buffer[i];
    }
  }
  buffer[j] = '\0';
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Dep_Symbol()
// FUNCTION: Write to 'stdout' a representation of the symbol for 'wn'.
//-----------------------------------------------------------------------

void WB_BROWSER::Dep_Symbol(WN *wn)
{
  WN* symbol_node = NULL;
  char buffer[WB_MAX_STRING_LENGTH];
  switch (WN_operator(wn)) {
  case OPR_ISTORE:
    symbol_node = WN_kid1(wn);
    break;
  case OPR_ILOAD:
    symbol_node = WN_kid0(wn);
    break;
  }
  if (symbol_node == NULL)
    return;
  INT cc = WB_Dump_Whirl_Expr(symbol_node, symbol_node, buffer, 0);
  Compact_Buffer(buffer);
  if (cc > WB_MAX_STRING_LENGTH - 1) {
    fprintf(stdout, "Expression too long!\n");
    Error_Cleanup();
    return;
  }
  fputs(buffer, stdout);
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Deps()
// WHIRL BROWSER COMMAND: 'G'
// FUNCTION: Print the dep graph info at the current node
//-----------------------------------------------------------------------

void WB_BROWSER::Deps_Loop()
{
  if (Dg() == NULL) {
    Error_Cleanup();
    return;
  }
  Carray().Reset_Index(); 
  WN* start_node = NULL;
  switch (WN_opcode(Cnode())) {
  case OPC_DO_LOOP:
    start_node = WN_do_body(Cnode());
    break;
  case OPC_DO_WHILE:
  case OPC_WHILE_DO:
    start_node = WN_while_body(Cnode());
    break;
  }
  WN_ITER* itr = WN_WALK_TreeIter(start_node);
  for (; itr != NULL; itr = WN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    VINDEX16 v = Dg()->Get_Vertex(wn);
    if (v == 0)
      continue;
    INT j = Carray().Enter_This_Node_Unique(wn);
    fprintf(stdout, "[%d] ", j);
    This_Node(wn); 
    fprintf(stdout, "V#%d ", v);
    Dep_Symbol(wn);
    fprintf(stdout, "\n");
    EINDEX16 e;
    if (Dg()->Get_In_Edge(v)) {
      fprintf(stdout, "    ");
      fprintf(stdout, "IN EDGES:\n");
      for (e = Dg()->Get_In_Edge(v); e != 0; e = Dg()->Get_Next_In_Edge(e)) {
        fprintf(stdout, "    ");
	INT j = Carray().Enter_This_Node_Unique(
	  Dg()->Get_Wn(Dg()->Get_Source(e))); 
	fprintf(stdout, "[%d] ", j);
	This_Node(Dg()->Get_Wn(Dg()->Get_Source(e)));  
        fprintf(stdout, "E#%d ", e);
        Dep_Symbol(Dg()->Get_Wn(Dg()->Get_Source(e)));
      }
    }
    if (Dg()->Get_Out_Edge(v)) {
      fprintf(stdout, "    ");
      fprintf(stdout, "OUT EDGES:\n");
      for (e = Dg()->Get_Out_Edge(v); e != 0; e = Dg()->Get_Next_Out_Edge(e)) {
        fprintf(stdout, "    ");
	INT j = Carray().Enter_This_Node_Unique(
          Dg()->Get_Wn(Dg()->Get_Sink(e))); 
	fprintf(stdout, "[%d] ", j);
	This_Node(Dg()->Get_Wn(Dg()->Get_Sink(e)));  
        fprintf(stdout, "E#%d ", e);
        Dep_Symbol(Dg()->Get_Wn(Dg()->Get_Sink(e)));
      }
    }
  }
}

void WB_BROWSER::Deps_Ref()
{
  if (Dg() == NULL) {
    Error_Cleanup();
    return;
  }
  VINDEX16 v = Dg()->Get_Vertex(Cnode());
  if (v == 0) {
    Error_Cleanup();
    return;
  }
  EINDEX16 e;
  if (!Dg()->Get_In_Edge(v) && !Dg()->Get_Out_Edge(v))
    fprintf(stdout, "V#%d\n", v);
  Carray().Reset_Index(); 
  if (Dg()->Get_In_Edge(v)) {
    fprintf(stdout, "V#%d ", v);
    fprintf(stdout, "IN EDGES:\n");
    for (e = Dg()->Get_In_Edge(v); e != 0; e = Dg()->Get_Next_In_Edge(e)) {
      INT j = Carray().Enter_This_Node_Unique(
	Dg()->Get_Wn(Dg()->Get_Source(e))); 
      fprintf(stdout, "[%d] ", j);
      This_Node(Dg()->Get_Wn(Dg()->Get_Source(e)));  
      fprintf(stdout, "E#%d ", e);
    }
  }
  if (Dg()->Get_Out_Edge(v)) {
    fprintf(stdout, "V#%d ", v);
    fprintf(stdout, "OUT EDGES:\n");
    for (e = Dg()->Get_Out_Edge(v); e != 0; e = Dg()->Get_Next_Out_Edge(e)) {
      INT j = Carray().Enter_This_Node_Unique(
	Dg()->Get_Wn(Dg()->Get_Sink(e))); 
      fprintf(stdout, "[%d] ", j);
      This_Node(Dg()->Get_Wn(Dg()->Get_Sink(e)));  
      fprintf(stdout, "E#%d ", e);
    }
  }
}

void WB_BROWSER::Deps()
{
  switch (WN_opcode(Cnode())) {
  case OPC_DO_LOOP:
  case OPC_DO_WHILE:
  case OPC_WHILE_DO:
    Deps_Loop();
    break;
  default:
    Deps_Ref();
    break;
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Vertices
// WHIRL BROWSER COMMAND: 'V'
// FUNCTION: Print the vertices of the dep graph
//-----------------------------------------------------------------------

void WB_BROWSER::Vertices()
{
  if (Dg() == NULL) {
    Error_Cleanup();
    return;
  }
  VINDEX16 v;
  for (v = Dg()->Get_Vertex(); v; v = Dg()->Get_Next_Vertex(v)) {
    WN* wn = Dg()->Get_Wn(v);
    fprintf(stdout, "V#%d ", (INT) v);
    This_Node(wn, FALSE);
    Dep_Symbol(wn);
    fprintf(stdout, "\n");
  }

  VINDEX16 w;
  for (v = Dg()->Get_Vertex(); v; v = Dg()->Get_Next_Vertex(v))
    for (w = Dg()->Get_Next_Vertex(v); w; w = Dg()->Get_Next_Vertex(w))
      if (Dg()->Get_Wn(v) == Dg()->Get_Wn(w))
        fprintf(stdout, "Vertices %d and %d are for the same node!\n", v, w);
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Vertex_Set_Node
// WHIRL BROWSER COMMAND: 'v'
// FUNCTION: Set the current node to the node with the following vertex 
//   number
//-----------------------------------------------------------------------

void WB_BROWSER::Vertex_Set_Node()
{
  if (Dg() == NULL) {
    Error_Cleanup();
    return;
  }
  INT vertex_number = 0;
  Buffer().Scan_Integer(&vertex_number);
  VINDEX16 v;  
  for (v = Dg()->Get_Vertex(); v != 0; v = Dg()->Get_Next_Vertex(v))
    if (v == (VINDEX16) vertex_number)
      break;
  if (v == 0) {
    Error_Cleanup();
    return;
  }
  Set_Cnode(Dg()->Get_Wn((VINDEX16) vertex_number));
  Print_This_Node(Cnode());
}

INT loop_count = 0;

//-----------------------------------------------------------------------
// NAME: Dump_Spaces
// FUNCTION: Print 'spaces' number of spaces to the file 'fp'.
//-----------------------------------------------------------------------

static void Dump_Spaces(FILE* fp, 
		        INT spaces)
{
  for (INT i = 0; i < spaces; i++)
    fprintf(fp, " ");
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Loops()
// WHIRL BROWSER COMMAND: 'L'
// FUNCTION: Sketch the loop nests in the program unit
//-----------------------------------------------------------------------

void WB_BROWSER::Loops_Walk(WN* wn, 
		            FILE* fp, 
		            INT spaces, 
		            INT increment)
{
  WN* w = 0; 
  const char* name = NULL; 
  switch (WN_opcode(wn)) {
  case OPC_BLOCK:
    for (w = WN_first(wn); w; w = WN_next(w))
      Loops_Walk(w, fp, spaces, increment);
    break;
  case OPC_DO_LOOP:
    Dump_Spaces(fp, spaces);
    name = WB_Whirl_Symbol(wn);
    fprintf(fp, "[%d] 0x%p DOLOOP (%d) %s\n", Carray().Next_Index(), wn,
      Srcpos_To_Line(WN_linenum(wn)), name);
    Carray().Enter_This_Node(wn); 
    Loops_Walk(WN_do_body(wn), fp, spaces + increment, increment);
    break;
  case OPC_FUNC_ENTRY:
    Dump_Spaces(fp, spaces);
    fprintf(fp, "[%d] 0x%p FUNC_ENTRY %s\n", Carray().Next_Index(), wn,
      WB_Whirl_Symbol(wn));
    Carray().Enter_This_Node(wn);
    Loops_Walk(WN_func_body(wn), fp, spaces + increment, increment);
    break;
  case OPC_IF:
    if (Fancy_Level() < 3) {
      Loops_Walk(WN_then(wn), fp, spaces, increment);
      Loops_Walk(WN_else(wn), fp, spaces, increment);
    } else {
      Dump_Spaces(fp, spaces);
      fprintf(fp, "[%d] 0x%p IF ([%d] 0x%p) THEN [%d] 0x%p\n",
        Carray().Next_Index(), wn, Carray().Next_Index() + 1, WN_if_test(wn),
        Carray().Next_Index() + 2, WN_then(wn));
      INT if_loop_count = Carray().Next_Index();
      Carray().Enter_This_Node(wn);
      Carray().Enter_This_Node(WN_if_test(wn));
      Carray().Enter_This_Node(WN_then(wn));
      Loops_Walk(WN_then(wn), fp, spaces + increment, increment);
      if (!WN_else_is_empty(wn)) {
        Dump_Spaces(fp, spaces);
        fprintf(fp, "[%d] 0x%p ELSE\n", Carray().Next_Index(), WN_else(wn));
        Carray().Enter_This_Node(WN_else(wn));
        Loops_Walk(WN_else(wn), fp, spaces + increment, increment);
      }
      Dump_Spaces(fp, spaces);
      fprintf(fp, "[%d] 0x%p ENDIF\n", if_loop_count, wn);
    }
    break;
  case OPC_DO_WHILE:
    Dump_Spaces(fp, spaces);
    fprintf(fp, "[%d] 0x%p DO_WHILE_LOOP \n", Carray().Next_Index(), wn);
    Carray().Enter_This_Node(wn);
    Loops_Walk(WN_while_body(wn), fp, spaces + increment, increment);
    break;
  case OPC_WHILE_DO:
    Dump_Spaces(fp, spaces);
    fprintf(fp, "[%d] 0x%p WHILE_DO_LOOP \n", Carray().Next_Index(), wn);
    Carray().Enter_This_Node(wn);
    Loops_Walk(WN_while_body(wn), fp, spaces + increment, increment);
    break;
  case OPC_REGION:
    if (Fancy_Level() >= 3) {
      Dump_Spaces(fp, spaces);
      fprintf(fp, "[%d] 0x%p REGION \n", Carray().Next_Index(), wn);
      Carray().Enter_This_Node(wn);
      for (INT i = 0; i < WN_kid_count(wn); i++)
        Loops_Walk(WN_kid(wn, i), fp, spaces + increment, increment);
    } else {
      for (INT i = 0; i < WN_kid_count(wn); i++)
        Loops_Walk(WN_kid(wn, i), fp, spaces, increment);
    }
  }
}

void WB_BROWSER::Loops()
{
  Carray().Reset_Index();
  Loops_Walk(Cnode(), stdout, 0, 2);
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Set_Map_Id
// WHIRL BROWSER COMMAND: 'M'
// FUNCTION: Set the current node to node with this map id
//-----------------------------------------------------------------------

void WB_BROWSER::Set_Map_Id()
{
  INT map_id = 0;
  Buffer().Scan_Integer(&map_id); 
  WN_ITER* itr = WN_WALK_TreeIter(Global_Fd());
  for (; itr != NULL; itr = WN_WALK_TreeNext(itr))
    if (WN_map_id(itr->wn) == map_id)
      break;
  if (itr == NULL) {
    Error_Cleanup();
    return;
  }
  Set_Cnode(itr->wn);
  Print_This_Node(Cnode());
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Map_Id
// WHIRL BROWSER COMMAND: 'm'
// FUNCTION: Print the map-id at this node 
//-----------------------------------------------------------------------

void WB_BROWSER::Map_Id()
{
  fprintf(stdout, "%d\n", WN_map_id(Cnode()));
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::DaVinci_Toggle
// WHIRL BROWSER COMMAND: 'd'
// FUNCTION: Toggle Da Vinci mode
//-----------------------------------------------------------------------

void WB_BROWSER::DaVinci_Toggle()
{
  if (DaVinci_Mode()) {
    _davinci_mode = FALSE;
    fprintf(stdout, "DAVINCI is OFF.\n");
  } else {
    _davinci_mode = TRUE;
    fprintf(stdout, "DAVINCI is ON.\n");
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Find_Symbols
// WHIRL BROWSER COMMAND: '$'
// FUNCTION: Print the symbol table entry with this name
//-----------------------------------------------------------------------

void WB_BROWSER::Find_Symbols()
{
  ST* st = NULL;
  char s[WB_MAX_STRING_LENGTH];
  Buffer().Scan_Alphanumeric(s);
  Carray().Reset_Index(); 
  BOOL test_substring = s[0] == '\'';
  INT i; 
  FOREACH_SYMBOL(CURRENT_SYMTAB, st, i) {
    if ((!test_substring && strcmp(ST_name(st), s) == 0
        || test_substring && Is_Substring(&s[1], ST_name(st)))) {
      Print_ST(stdout, st, TRUE);
    }
  }
  FOREACH_SYMBOL(GLOBAL_SYMTAB, st, i) {
    if ((!test_substring && strcmp(ST_name(st), s) == 0
        || test_substring && Is_Substring(&s[1], ST_name(st)))) {
      Print_ST(stdout, st, TRUE);
    }
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Required_Fields_Present
// FUNCTION: Return TRUE if the 'num'th command has all of the fields 
//   in the WB_BROWSER needed to print the associated information. 
//-----------------------------------------------------------------------

BOOL WB_BROWSER::Required_Fields_Present(INT num)
{ 
  if (Required_Fields(num) == WBR_NONE)
    return TRUE; 
  if ((Required_Fields(num) & WBR_DU) && Du() == NULL)
    return FALSE; 
  if ((Required_Fields(num) & WBR_DG) && Dg() == NULL)
    return FALSE; 
  if ((Required_Fields(num) & WBR_ALIAS) && Alias_Mgr() == NULL)
    return FALSE; 
  if ((Required_Fields(num) & WBR_PARENT) && Parent_Map() == WN_MAP_UNDEFINED)
    return FALSE; 
  if ((Required_Fields(num) & WBR_AAMAP) && Access_Array_Map() == WN_MAP_UNDEFINED) 
    return FALSE; 
  if ((Required_Fields(num) & WBR_REDMAP) && Reduction_Map() == WN_MAP_UNDEFINED) 
    return FALSE; 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Help
// WHIRL BROWSER COMMAND: 'H'
// FUNCTION: Print this information
//-----------------------------------------------------------------------

void WB_BROWSER::Help()
{ 
  for (INT i = 0; Command(i) != '\0'; i++) {
    if (Required_Fields_Present(i)) {
      for (INT j = 0; j < WB_ASCII_CHAR_COUNT; j++) 
	if (_keymap[j] == Command(i)) 
           fprintf(stdout, "  %c: %s\n", j, Command_Text(i)); 
    }
  }
  fprintf(stdout, "  Q: Exit the debugger\n");
  fprintf(stdout, "  q: Exit the debugger\n");
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Invoke_Command
// FUNCTION: Invoke the command indicated by character 'ch'. 
//-----------------------------------------------------------------------

void WB_BROWSER::Invoke_Command(char ch)
{ 
  switch (_keymap[ch]) { 
  case 'R': 
    Root(); 
    break;
  case 'N': 
    Next(); 
    break; 
  case 'P': 
    Previous(); 
    break;
  case '=':
    Set_Node(); 
    break;
  case '@':
    Address(); 
    break;
  case '>': 
    Fancy_Up(); 
    break; 
  case '<': 
    Fancy_Down(); 
    break; 
  case 'K': 
    Kids(); 
    break; 
  case 'S': 
    Statements(); 
    break; 
  case 'T': 
    This_Tree(); 
    break; 
  case '%': 
    Addresses(); 
    break; 
  case 's': 
    Symbol(); 
    break; 
  case 't': 
    Type(); 
    break;
  case 'F': 
    Find(); 
    break; 
  case '$':
    Find_Symbols();
    break;
  case 'o': 
    Find_Operator();
    break; 
  case 'H':
  case 'h':
    Help(); 
    break; 
  case 'U':
    Uses(); 
    break; 
  case 'D':
    Defs(); 
    break; 
  case 'G': 
    Deps(); 
    break; 
  case 'V': 
    Vertices(); 
    break; 
  case 'v': 
    Vertex_Set_Node(); 
    break; 
  case 'A': 
    Access_Array();
    break; 
  case 'a':
    Alias(); 
    break; 
  case 'r': 
    Reduction();
    break; 
  case 'E': 
    Parent(); 
    break; 
  case 'e': 
    Ancestors(); 
    break; 
  case 'W':
    Whirl2fc(); 
    break; 
  case 'f': 
    Whirl2fset(); 
    break;
  case 'c': 
    Whirl2cset(); 
    break; 
  case 'L': 
    Loops(); 
    break;
  case 'M': 
    Set_Map_Id();
    break; 
  case 'm': 
    Map_Id();
    break; 
  case 'd': 
    DaVinci_Toggle();
    break; 
  case '~': 
    WB_BROWSER_Summary(stdout, this);
    break;
  default: 
    fprintf(stdout, "Bad character: %c\n", ch);
    break; 
  }
}  

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Initialize_Language
// FUNCTION: Set the value of 'WB_language' and print out that language.
//-----------------------------------------------------------------------

void WB_BROWSER::Initialize_Language() 
{
  if (Pu() != NULL) { 
    switch (PU_src_lang(*Pu())) {
    case PU_C_LANG:
    case PU_CXX_LANG:
      Set_Source_Language(WB_SRC_C);
      fprintf(stdout, "WHIRL-TO-SOURCE language is C.\n");
      break;
    case PU_F90_LANG:
    case PU_F77_LANG:
      Set_Source_Language(WB_SRC_FORTRAN); 
      fprintf(stdout, "WHIRL-TO-SOURCE language is FORTRAN.\n");
      break;
    default:
      Set_Source_Language(WB_SRC_NONE);
      fprintf(stdout,
	"Can't do WHIRL-TO-SOURCE tranformations in this language.\n");
      break;
    }
  } else { 
    Set_Source_Language(WB_SRC_FORTRAN); 
    fprintf(stdout, "WHIRL-TO-SOURCE language is FORTRAN.\n");
  } 
}

//-----------------------------------------------------------------------
// NAME: Scan_Blanks_And_Tabs
// FUNCTION: In the 'buffer' advance 'buffer_start' past all blanks and 
//   tabs. 
//-----------------------------------------------------------------------

static void Scan_Blanks_And_Tabs(char buffer[],
                                 INT* buffer_start)
{
  char ch;
  do {
    ch = buffer[(*buffer_start)++];
  } while (ch == ' ' || ch == '\t');
  (*buffer_start)--;
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Initialize_Keymap
// FUNCTION: Initialize the keymap my writing over the default key settings
//   with other, user specified key settings.
//-----------------------------------------------------------------------

void WB_BROWSER::Initialize_Keymap(char ch)
{ 
  const INT BUFFER_MAX = 132; 
  char file_name[BUFFER_MAX];
  strcpy(file_name, getenv("HOME"));
  strcat(file_name, "/.wb_keymap");
  FILE* fp_keymap = fopen(file_name, "r");
  if (fp_keymap == NULL)
    return;
  BOOL print_log = TRUE;
  char key_buffer[BUFFER_MAX];
  INT line_number = 0;
  while (fgets(key_buffer, BUFFER_MAX-1, fp_keymap) != NULL) {
    line_number++;
    INT key_buffer_start = 0;
    Scan_Blanks_And_Tabs(key_buffer, &key_buffer_start);
    if (strncasecmp(&key_buffer[key_buffer_start], "SILENT",
        strlen("SILENT")) == 0) {
      print_log = FALSE;
    } else if (strncasecmp(&key_buffer[key_buffer_start], "VERBOSE",
        strlen("VERBOSE")) == 0) {
      print_log = TRUE;
    } else if (strncasecmp(&key_buffer[key_buffer_start], "TRANSLATE",
        strlen("TRANSLATE")) == 0) {
      key_buffer_start += strlen("TRANSLATE");
      Scan_Blanks_And_Tabs(key_buffer, &key_buffer_start);
      char old_char = key_buffer[key_buffer_start++];
      if (Unmappable_Character(old_char)) {
        fprintf(stdout,
          ".wb_keymap: Error on line %d: Cannot map %c\n",
          line_number, old_char);
        continue;
      }
      if (ch == ' ') { 
        if (key_buffer[key_buffer_start] != ' '
            && key_buffer[key_buffer_start] != '\t')
          continue;
        Scan_Blanks_And_Tabs(key_buffer, &key_buffer_start);
        char new_char = key_buffer[key_buffer_start++];
        if (Unmappable_Character(new_char)) {
          fprintf(stdout,
            ".wb_keymap: Error on line %d: Cannot map %c\n",
            line_number, new_char);
          continue;
        }
        if (print_log)
          fprintf(stdout, ".wb_keymap: Translating '%c' to '%c'\n",
            old_char, new_char);
        _keymap[new_char] = old_char;
      } else {
        if (old_char == ch) {
          old_char = key_buffer[key_buffer_start++];
          if (Unmappable_Character(old_char)) {
            fprintf(stdout,
              ".wb_keymap: Error on line %d: Cannot map %c\n",
              line_number, old_char);
            continue;
          }
          Scan_Blanks_And_Tabs(key_buffer, &key_buffer_start);
          char new_char = key_buffer[key_buffer_start++];
          if (Unmappable_Character(new_char)) {
            fprintf(stdout,
              ".wb_keymap: Error on line %d: Cannot map %c\n",
              line_number, new_char);
            continue;
          }
          if (print_log)
            fprintf(stdout, ".wb_keymap: Translating '%c' to '%c'\n",
              old_char, new_char);
          _keymap[new_char] = old_char;
        }
      }
    } else {
      fprintf(stdout, ".wb_keymap: Error on line %d: Unrecognized command\n",
        line_number);
    }
  } 
  fclose(fp_keymap);
} 

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Sdebug
// FUNCTION: The 'string debugger'. Run the whirl browser using 'init_buffer'
//   as the initial set of commands.
// NOTE: 'sdebug' can be called from the compiler as an aid to debugging in
//   batch mode.  For example, to print out the loop nests at a certain
//   place during compiler execution, put a call of the form:
//     sdebug("RLQ");
//   in the compiler where you want to see the loops.
//-----------------------------------------------------------------------

void WB_BROWSER::Sdebug(const char init_buffer[])
{
  char ch;
  BOOL reload;
 
  if (Global_Fd() == NULL) {
    fprintf(stdout, "Whirl browser not valid in this phase.\n");
    Error_Cleanup();
    return;
  } 
  Initialize_Keymap(' ');
  Initialize_Language();
  Root();
  fprintf(stdout, "Root node is: ");
  Print_This_Node(Cnode());
  WB_Prompt();  
  Buffer().Reset_Buffer();
  if (init_buffer[0] == '\0') {
    reload = TRUE;
  } else {
    reload = FALSE;
    Buffer().Load_Buffer(init_buffer);
    for (INT i = 0; init_buffer[i] != '\0'; i++)
      fprintf(stdout, "%c", init_buffer[i]);
    fprintf(stdout, "\n");
  }
  while (TRUE) {
    if (reload) {
      Buffer().Load_Buffer();
      reload = FALSE;
    }
    ch = Buffer().Get_Command();
    if (ch == '\n') {
      WB_Prompt();
      reload = TRUE;
      continue;
    }
    if (ch == 'Q' || ch == 'q') 
      return; 
    Invoke_Command(ch); 
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Set_Subcommand
// FUNCTION: Set the command list to the list corresponding to the
//   command whose character is 'ch'.
//-----------------------------------------------------------------------

void WB_BROWSER::Set_Subcommand(char ch)
{
  INT i;
  for (i = 0; Command(i) != '\0'; i++)
    if (Command(i) == ch)
       break;
  if (Command(i) == '\0')
    return;
  if (Subcommand(i) == NULL)
    return;
  _is_subcommand = TRUE;
  Set_Old_Command_List(Command_List());
  Set_Command_List(Subcommand(i));
  for (i = 0; i < WB_ASCII_CHAR_COUNT; i++)
    _old_keymap[i] = _keymap[i];
  for (i = 0; i < WB_ASCII_CHAR_COUNT; i++)
    _keymap[i] = i;
  Initialize_Keymap(ch);
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Reset_Subcommand
// FUNCTION: Reset the command list to its original value.
//-----------------------------------------------------------------------

void WB_BROWSER::Reset_Subcommand()
{
  _is_subcommand = FALSE;
  Set_Command_List(Old_Command_List());
  Set_Old_Command_List(NULL);
  for (INT i = 0; i < WB_ASCII_CHAR_COUNT; i++)
    _keymap[i] = _old_keymap[i];
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Debug 
// FUNCTION: The main entry point to the whirl browser.  To start up the
//   whirl browser in dbx, say 'p debug()'.
//-----------------------------------------------------------------------

void WB_BROWSER::Debug()
{
  Sdebug("");
}


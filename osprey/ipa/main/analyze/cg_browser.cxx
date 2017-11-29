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


#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <cmplrs/host.h>
#include "assert.h"
#define USE_STANDARD_TYPES         
#include "defs.h"
#include "cxx_memory.h"
#include "cxx_hash.h"
#include "erglob.h"
#include "glob.h"
#include "mempool.h"
#include "sparse_bv.h"
#include "tracing.h"
#include "strtab.h"
#include "stab.h"
#include "wn.h"
#include "const.h"
#include "pu_info.h"
#include "irbdata.h"                   
#include "dwarf_DST_mem.h"            
#include "ipc_defs.h"
#include "ipc_weak.h"                
#include "ipc_file.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "dep_graph.h"
#include "wb_util.h"
#include "wb_browser.h"
#include "wb.h"
#include "wb_ipa.h"
#include "cg_browser.h"
#include "ipaa.h"
#include "ipa_section_annot.h"
#include "ipa_df.h"
#include "ipa_cprop.h"
#include "ipl_summary.h"
#include "ipa_summary.h"
#include "ipa_lno_write.h"
#include "ipc_symtab_merge.h"           // IPC_GLOBAL_IDX_MAP
#include "ipo_defs.h" 
#include "be_symtab.h"

//-----------------------------------------------------------------------
// NAME: BE_Current_Init
// FUNCTION: Initialize the back-end symbol table for the current PU.
//-----------------------------------------------------------------------

static void BE_Current_Init()
{
  BE_symtab_alloc_scope_level(CURRENT_SYMTAB);
  Scope_tab[CURRENT_SYMTAB].st_tab->
    Register(*Be_scope_tab[CURRENT_SYMTAB].be_st_tab);
}

//-----------------------------------------------------------------------
// NAME: BE_Current_Fini
// FUNCTION: Finalize the back-end symbol table for the current PU.
//-----------------------------------------------------------------------

static void BE_Current_Fini()
{
  Scope_tab[CURRENT_SYMTAB].st_tab->
    Un_register(*Be_scope_tab[CURRENT_SYMTAB].be_st_tab);
  Be_scope_tab[CURRENT_SYMTAB].be_st_tab->Clear();
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Bad_Node()
// FUNCTION: Return TRUE if all but the simplest commands must be disabled
//   because the code for this PU is not accessible.
//-----------------------------------------------------------------------

BOOL CG_BROWSER::Bad_Node()
{
  PU_Info* pu = Cnode()->PU_Info();
  Subsect_State state = PU_Info_state(pu, WT_TREE);         
  return (state == Subsect_Missing || state == Subsect_Written);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Bad_File()
// FUNCTION: Return TRUE if all but the simplest commands must be disabled
//   because the code for this file is not accessible.
//-----------------------------------------------------------------------

BOOL CG_BROWSER::Bad_File()
{
  const IP_FILE_HDR& file_header = Cnode()->File_Header();
  return 
    IP_PROC_INFO_state(*IP_FILE_HDR_proc_info(file_header)) == IPA_WRITTEN;
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Unmappable_Character
// FUNCTION: Return TRUE if the character 'ch' is not remappable in the 
//   .cgb_keymap file.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL CG_BROWSER::Unmappable_Character(char ch)
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
// NAME: CG_BROWSER::Set_Subcommand
// FUNCTION: Set the command list to the list corresponding to the 
//   command whose character is 'ch'. 
//-----------------------------------------------------------------------

void CG_BROWSER::Set_Subcommand(char ch)
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
  for (i = 0; i < CGB_ASCII_CHAR_COUNT; i++) 
    _old_keymap[i] = _keymap[i]; 
  for (i = 0; i < CGB_ASCII_CHAR_COUNT; i++) 
    _keymap[i] = i; 
  Initialize_Keymap(ch);   
} 
  
//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Reset_Subcommand
// FUNCTION: Reset the command list to its original value. 
//-----------------------------------------------------------------------

void CG_BROWSER::Reset_Subcommand()
{
  _is_subcommand = FALSE; 
  Set_Command_List(Old_Command_List());
  Set_Old_Command_List(NULL);
  for (INT i = 0; i < CGB_ASCII_CHAR_COUNT; i++)
    _keymap[i] = _old_keymap[i]; 
}

//-----------------------------------------------------------------------
// NAME: CGB_Prompt
// FUNCTION: Print the call graph browser prompt symbol to 'stdout'.
//-----------------------------------------------------------------------

extern void CGB_Prompt()
{
  fprintf(stdout, "CGB> ");
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::CG_BROWSER
// FUNCTION: Default constructor for Call Graph Browser
//-----------------------------------------------------------------------

CG_BROWSER::CG_BROWSER()
{
  _ipa_cg = NULL;
  _cnode = NULL; 
  _cvertex = 0;
  _command_list = NULL; 
  _old_command_list = NULL; 
  _fancy_level = 1; 
  _is_subcommand = FALSE; 
  _davinci_mode = FALSE; 
  for (INT i = 0; i < CGB_ASCII_CHAR_COUNT; i++)
    _keymap[i] = i;
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::CG_BROWSER
// FUNCTION: Constructor for Call Graph Browser
//-----------------------------------------------------------------------

CG_BROWSER::CG_BROWSER(IPA_CALL_GRAPH* ipa_cg,
		       CGB_COMMAND* command_list)
{
  _ipa_cg = ipa_cg; 
  _cnode = NULL; 
  _cvertex = 0; 
  _command_list = command_list; 
  _old_command_list = NULL; 
  _fancy_level = 1;
  _is_subcommand = FALSE; 
  _davinci_mode = FALSE; 
  for (INT i = 0; i < CGB_ASCII_CHAR_COUNT; i++)
    _keymap[i] = i;
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Required_Fields_Present
// FUNCTION: Return TRUE if the 'num'th command has all of the fields
//   in the CG_BROWSER needed to print the associated information.
//-----------------------------------------------------------------------

BOOL CG_BROWSER::Required_Fields_Present(INT num)
{
  if (Required_Fields(num) == CGBR_NONE)
    return TRUE;
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Error_Cleanup
// FUNCTION: Sound the bell, write the prompt to 'stdout' and reload the
//   input buffer, scanning past blanks and tabs.
//-----------------------------------------------------------------------

void CG_BROWSER::Error_Cleanup()
{
  WB_Bell();
  CGB_Prompt();
  Buffer().Load_Buffer();
  Buffer().Scan_Blanks_And_Tabs();
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::This_Node
// FUNCTION: Write the address of 'ipan' and its function name to 'stdout'.
//-----------------------------------------------------------------------

void CG_BROWSER::This_Node(FILE* fp, 
			   IPA_NODE* ipan,
			   NODE_INDEX vdx)
{
  if (ipan == NULL) {
    fprintf(fp, "<NULL>");
    return;
  }
  fprintf(fp, "%p ", ipan);
  fprintf(fp, "V#%d ", vdx);
  fprintf(fp, "%s", IPA_Node_Name(ipan));
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Print_This_Node
// FUNCTION: Print the node 'ipan' and a newline.
//-----------------------------------------------------------------------

void CG_BROWSER::Print_This_Node(FILE* fp, 
			         IPA_NODE* ipan,
				 NODE_INDEX vdx)
{  
  This_Node(fp, ipan, vdx);
  fprintf(fp, "\n");
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Fancy_Up
// CALL GRAPH BROWSER COMMAND: '>'
// FUNCTION: Increase the fanciness level for printing 
//-----------------------------------------------------------------------

void CG_BROWSER::Fancy_Up()
{ 
  if (_fancy_level == CGB_FANCY_MAX) { 
    Error_Cleanup(); 
    return; 
  } 
  _fancy_level++; 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Fancy_Down
// CALL GRAPH BROWSER COMMAND: '<'
// FUNCTION: Decreases the fanciness level for printing 
//-----------------------------------------------------------------------

void CG_BROWSER::Fancy_Down()
{ 
  if (_fancy_level == CGB_FANCY_MIN) { 
    Error_Cleanup(); 
    return; 
  } 
  _fancy_level--; 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Address()
// CALL GRAPH BROWSER COMMAND: '@'
// FUNCTION: Set the current node to this numbered node
//-----------------------------------------------------------------------

void CG_BROWSER::Address(FILE* fp)
{
  INT integer;
  Buffer().Scan_Integer(&integer);
  if (integer < 0 || integer >= Carray().Next_Index()) {
    Error_Cleanup();
    return;
  }
  Set_Cnode(Carray().Node(integer));
  Set_Cvertex(Carray().Vertex(integer));
  Print_This_Node(fp, Cnode(), Cvertex());
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Root
// CALL GRAPH BROWSER COMMAND: 'R'
// FUNCTION: Go to the root of the program unit
//-----------------------------------------------------------------------

void CG_BROWSER::Root(FILE* fp)
{
  IPA_GRAPH* ipa_graph = Ipa_Cg()->Graph();  
  NODE_INDEX ventry = GRAPH_root(ipa_graph);
  Set_Cnode(NULL);
  Set_Cvertex(ventry);
  Carray().Reset_Index();
  MEM_POOL_Push(&_pool);
  NODE_ITER viter(ipa_graph, Cvertex());
  INT root_count = 0;
  NODE_INDEX v = viter.First_Succ();
  for (; v != INVALID_NODE_INDEX; v = viter.Next_Succ()) 
    root_count++; 
  if (root_count == 1) { 
    NODE_INDEX v = viter.First_Succ();
    IPA_NODE* ipan = ipa_graph->Node_User(v);
    Set_Cnode(ipan);
    Set_Cvertex(v);
  } else { 
    INT i = 0;
    fprintf(fp, "CALL GRAPH ROOT NODES:\n");
    NODE_INDEX v = viter.First_Succ();
    for (; v != INVALID_NODE_INDEX; v = viter.Next_Succ()) {
      IPA_NODE* ipan = ipa_graph->Node_User(v);
      fprintf(fp, "[%d] ", i++);
      This_Node(fp, ipan, v);
      Carray().Enter_This_Pair(ipan, v);
      fprintf(stdout, "\n");
    }
    Set_Cnode(Carray().Node(0));
    Set_Cvertex(Carray().Vertex(0));
    fprintf(fp, "    SELECTING: ");
  } 
  Print_This_Node(fp, Cnode(), Cvertex());
  MEM_POOL_Pop(&_pool);
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Callers
// CALL GRAPH BROWSER COMMAND: 'C'
// FUNCTION: List the callers of the current node. 
//-----------------------------------------------------------------------

void CG_BROWSER::Callers(FILE* fp)
{
  if (Cnode() == NULL) {
    Error_Cleanup();
    return;
  }
  IPA_GRAPH* ipa_graph = Ipa_Cg()->Graph();
  MEM_POOL_Push(&_pool);
  NODE_ITER viter(ipa_graph, Cvertex());
  INT i = 0; 
  NODE_INDEX v = viter.First_Pred();
  for (; v != INVALID_NODE_INDEX; v = viter.Next_Pred()) { 
    IPA_NODE* ipan = ipa_graph->Node_User(v);
    if (ipan == NULL)
      continue; 
    if (i++ == 0)
      Carray().Reset_Index();
    Carray().Enter_This_Pair_Unique(ipan, v);
  }
  MEM_POOL_Pop(&_pool);
  INT caller_count = i; 
  if (caller_count == 0) { 
    Error_Cleanup();
    return;
  } 
  for (i = 0; i < Carray().Next_Index(); i++) { 
    fprintf(fp, "[%d] ", i);
    Print_This_Node(fp, Carray().Node(i), Carray().Vertex(i));
  } 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Edges_In
// CALL GRAPH BROWSER COMMAND: 'E'
// FUNCTION: List the edges into the current node. 
//-----------------------------------------------------------------------

void CG_BROWSER::Edges_In(FILE* fp)
{
  if (Cnode() == NULL) {
    Error_Cleanup();
    return;
  }
  IPA_GRAPH* ipa_graph = Ipa_Cg()->Graph();
  IPA_PRED_ITER edge_iter(Cnode());
  INT i = 0; 
  for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {
    IPA_EDGE* e = edge_iter.Current_Edge();
    if (e == NULL)
      continue; 
    IPA_NODE* ipan_caller = Ipa_Cg()->Caller(e->Edge_Index());
    if (ipan_caller == NULL)
      continue; 
    NODE_INDEX v_caller = ipan_caller->Node_Index(); 
    if (i == 0)
      Carray().Reset_Index();
    Carray().Enter_This_Pair(ipan_caller, v_caller);
    fprintf(fp, "[%d] ", i);
    This_Node(fp, Carray().Node(i), Carray().Vertex(i));
    fprintf(stdout, " E#%d ", e->Edge_Index());
    VALUE_DYN_ARRAY* formal_array = (VALUE_DYN_ARRAY *) e->Cprop_Annot();
    Print_Formal_Cprop_Annot(fp, -1, formal_array);
    fprintf(stdout, "\n");
    i++;
  } 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Edges_Out
// CALL GRAPH BROWSER COMMAND: 'e'
// FUNCTION: List the edges out of the current node. 
//-----------------------------------------------------------------------

void CG_BROWSER::Edges_Out(FILE* fp)
{
  if (Cnode() == NULL) {
    Error_Cleanup();
    return;
  }
  IPA_GRAPH* ipa_graph = Ipa_Cg()->Graph();
  IPA_SUCC_ITER edge_iter(Cnode());
  INT i = 0; 
  for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {
    IPA_EDGE* e = edge_iter.Current_Edge();
    if (e == NULL)
      continue; 
    IPA_NODE* ipan_callee = Ipa_Cg()->Callee(e->Edge_Index());
    if (ipan_callee == NULL)
      continue; 
    NODE_INDEX v_callee = ipan_callee->Node_Index(); 
    if (i == 0)
      Carray().Reset_Index();
    Carray().Enter_This_Pair(ipan_callee, v_callee);
    fprintf(fp, "[%d] ", i);
    This_Node(fp, Carray().Node(i), Carray().Vertex(i));
    fprintf(stdout, " E#%d ", e->Edge_Index());
    VALUE_DYN_ARRAY* formal_array = (VALUE_DYN_ARRAY *) e->Cprop_Annot();
    Print_Formal_Cprop_Annot(fp, -1, formal_array);
    fprintf(stdout, "\n");
    i++;
  } 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Callees
// CALL GRAPH BROWSER COMMAND: 'c'
// FUNCTION: List the callees of the current node. 
//-----------------------------------------------------------------------

void CG_BROWSER::Callees(FILE* fp)
{ 
  if (Cnode() == NULL) {
    Error_Cleanup();
    return;
  }
  IPA_GRAPH* ipa_graph = Ipa_Cg()->Graph();
  MEM_POOL_Push(&_pool);
  NODE_ITER viter(ipa_graph, Cvertex());
  INT i = 0; 
  NODE_INDEX v = viter.First_Succ();
  for (; v != INVALID_NODE_INDEX; v = viter.Next_Succ()) { 
    IPA_NODE* ipan = ipa_graph->Node_User(v);
    if (ipan == NULL)
      continue; 
    if (i++ == 0)
      Carray().Reset_Index();
    Carray().Enter_This_Pair_Unique(ipan, v);
  }
  MEM_POOL_Pop(&_pool);
  INT callee_count = i; 
  if (callee_count == 0) { 
    Error_Cleanup();
    return;
  } 
  for (i = 0; i < Carray().Next_Index(); i++) { 
    fprintf(fp, "[%d] ", i);
    Print_This_Node(fp, Carray().Node(i), Carray().Vertex(i));
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

  for (INT i = 0; i <= difference; i++) {
    INT j;

    for (j = 0; j < substring_length; j++)
      if (s1[j] != s2[i + j])
        break;

    if (j == substring_length)
      return TRUE;
  }
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Find()
// CALL GRAPH BROWSER COMMAND: 'F'
// FUNCTION: Find the node with the given name. 
//-----------------------------------------------------------------------

void CG_BROWSER::Find(FILE* fp)
{
  char s[CGB_MAX_STRING_LENGTH];
  Buffer().Scan_Alphanumeric(s);
  IPA_NODE* ipan = NULL; 
  IPA_GRAPH* ipa_graph = Ipa_Cg()->Graph();
  BOOL test_substring = s[0] == '\'';
  INT i = 0;
  for (NODE_INDEX v = 0; v < GRAPH_vmax(ipa_graph); v++) {
    if (!ipa_graph->Is_Node(v))
      continue;
    ipan = ipa_graph->Node_User(v);
    if (ipan == NULL)
      continue; 
    if (!test_substring && !strcmp(s, IPA_Node_Name(ipan))
        || test_substring && Is_Substring(&s[1], IPA_Node_Name(ipan))) { 
      if (i++ == 0)
	Carray().Reset_Index();
      Carray().Enter_This_Pair_Unique(ipan, v);
    } 
  } 
  for (i = 0; i < Carray().Next_Index(); i++) { 
    fprintf(fp, "[%d] ", i);
    Print_This_Node(fp, Carray().Node(i), Carray().Vertex(i));
  } 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Find_Vertex
// FUNCTION: In the graph 'ipa_cg' return the vertex for the node 'ipan'
//   if there is one, otherwise return INVALID_NODE_INDEX.
//-----------------------------------------------------------------------

NODE_INDEX CG_BROWSER::Find_Vertex(IPA_NODE* ipan)
{ 
  IPA_GRAPH* ipa_graph = Ipa_Cg()->Graph(); 
  for (NODE_INDEX v = 0; v < GRAPH_vmax(ipa_graph); v++) {
    if (!ipa_graph->Is_Node(v))
      continue; 
    IPA_NODE* node = ipa_graph->Node_User(v);
    if (ipan == (IPA_NODE*) node)
      return v;
  } 
  return INVALID_NODE_INDEX; 
} 
   
//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Set_Node()
// CALL GRAPH BROWSER COMMAND: '='
// FUNCTION: Set the current node to the following address
//-----------------------------------------------------------------------

void CG_BROWSER::Set_Node(FILE* fp)
{
  INT node;
  Buffer().Scan_HexInteger(&node);
  IPA_GRAPH* ipa_graph = Ipa_Cg()->Graph(); 
  NODE_INDEX v = Find_Vertex((IPA_NODE*)(INTPTR)node);
  if (v == INVALID_NODE_INDEX) { 
    Error_Cleanup();
    return; 
  } 
  Set_Cnode((IPA_NODE*)(INTPTR)node);
  Set_Cvertex(v);
  Print_This_Node(fp, Cnode(), Cvertex());
}

//------------------------------------------------------------------------
// NAME: CG_BROWSER::Set_Vertex()
// CALL GRAPH BROWSER COMMAND: 'v'
// FUNCTION: Set the current vertex to the following number
//-----------------------------------------------------------------------

void CG_BROWSER::Set_Vertex(FILE* fp)
{ 
  INT vertex; 
  Buffer().Scan_Integer(&vertex);
  IPA_GRAPH* ipa_graph = Ipa_Cg()->Graph(); 
  NODE_INDEX v = (NODE_INDEX) vertex;
  if (v < 0 || v >= GRAPH_vmax(ipa_graph)) { 
    Error_Cleanup();
    return; 
  }
  if (!ipa_graph->Is_Node(v)) {
    Error_Cleanup();
    return; 
  } 
  IPA_NODE* ipan = ipa_graph->Node_User(v);
  Set_Cnode((IPA_NODE*) ipan);
  Set_Cvertex(v);
  Print_This_Node(fp, Cnode(), Cvertex());
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Graph
// CALL GRAPH BROWSER COMMAND: 'G'
// FUNCTION: Print the call graph
//-----------------------------------------------------------------------

void CG_BROWSER::Graph(FILE* fp)
{
  INT i = 0; 
  const INT MAX_BAD_NODES = 25; 
  INT bad_node_count = 0;
  IPA_NODE* bad_node[MAX_BAD_NODES];
  NODE_INDEX bad_vertex[MAX_BAD_NODES];
  BOOL bad_pred[MAX_BAD_NODES];
  IPA_GRAPH* ipa_graph = Ipa_Cg()->Graph(); 
  Carray().Reset_Index();
  IPA_NODE_ITER cg_iter(Ipa_Cg(), PREORDER);
  for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
    IPA_NODE* ipan = cg_iter.Current();
    if (ipan == NULL)
      continue; 
    NODE_INDEX v = Find_Vertex(ipan);
    if (v == INVALID_NODE_INDEX) {
      fprintf(fp, "Could not find vertex for node.\n");
      Error_Cleanup();
      return; 
    }
    Carray().Enter_This_Pair(ipan, v);
  } 
  for (i = 0; i < Carray().Next_Index(); i++) { 
    IPA_NODE* ipan = Carray().Node(i);
    NODE_INDEX varray = Carray().Vertex(i);
    fprintf(fp, "[%d] ", i);
    This_Node(fp, ipan, varray);
    fprintf(fp, " ");
    MEM_POOL_Push(&_pool);
    NODE_ITER viter(ipa_graph, varray);
    fprintf(fp, "[");
    NODE_INDEX v = viter.First_Pred();
    BOOL add_comma = FALSE; 
    for (; v != INVALID_NODE_INDEX; v = viter.Next_Pred()) { 
      if (add_comma)
	fprintf(fp, ",");
      IPA_NODE* ipan = ipa_graph->Node_User(v);
      if (ipan == NULL)
	continue; 
      INT j = Carray().Find_This_Pair(ipan, v); 
      if (j == -1) { 
	j = Carray().Next_Index();
	Carray().Enter_This_Pair(ipan, v);
	bad_node[bad_node_count] = ipan; 
	bad_vertex[bad_node_count] = v;
	bad_pred[bad_node_count] = TRUE; 
	bad_node_count++;
      } 
      fprintf(fp, "%d", j);
      add_comma = TRUE; 
    }
    fprintf(fp, "|");
    v = viter.First_Succ();
    add_comma = FALSE; 
    for (; v != INVALID_NODE_INDEX; v = viter.Next_Succ()) { 
      if (add_comma)
	fprintf(fp, ",");
      IPA_NODE* ipan = ipa_graph->Node_User(v);
      if (ipan == NULL)
	continue; 
      INT j = Carray().Find_This_Pair(ipan, v); 
      if (j == -1) { 
	j = Carray().Next_Index();
	Carray().Enter_This_Pair(ipan, v);
	bad_node[bad_node_count] = ipan; 
	bad_vertex[bad_node_count] = v;
	bad_pred[bad_node_count] = FALSE; 
	bad_node_count++;
      } 
      fprintf(fp, "%d", j);
      add_comma = TRUE; 
    }
    fprintf(fp, "]\n");
    MEM_POOL_Pop(&_pool);
  } 
  if (bad_node_count > 0) { 
    fprintf(fp, "WARNING!!\n");
    fprintf(fp, "These nodes were missed on a preorder traversal\n");
    for (i = 0; i < bad_node_count; i++) { 
      fprintf(fp, "%p V#%d %s\n", bad_node[i], bad_vertex[i], 
	bad_pred[i] ? "PRED" : "SUCC"); 
    } 
  } 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Symbol_Name 
// FUNCTION: Returns the name of the 'i'th symbol of the current node.
//-----------------------------------------------------------------------

char* CG_BROWSER::Symbol_Name(INT i,
			      char** function_name)
{ 
  SUMMARY_SYMBOL* symbol_array = IPA_get_symbol_array(Cnode());
  SUMMARY_SYMBOL* ss = &symbol_array[i]; 
  if (ST_IDX_level(ss->St_idx()) > 1) {
    ST_IDX func_st_idx = ss->Get_st_idx_func();
    PU_IDX pu_idx = ST_pu(ST_ptr(func_st_idx)); 
    NODE_INDEX node_index = AUX_PU_node(Aux_Pu_Table[pu_idx]);
    IPA_NODE* cnode = IPA_Call_Graph->Graph()->Node_User(node_index); 
    IPA_NODE_CONTEXT context(cnode);
    if (function_name != NULL) 
      *function_name = ST_name(func_st_idx);
    return ST_name(ss->St_idx()); 
  } else {
    IPA_NODE_CONTEXT context(Cnode());
    if (function_name != NULL)
      *function_name = NULL; 
    return ST_name(ss->St_idx()); 
  }
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Formal_Name 
// FUNCTION: Returns the name of the 'i'th formal of the current node.
//-----------------------------------------------------------------------

char* CG_BROWSER::Formal_Name(INT i)
{
  SUMMARY_FORMAL* formal_array = IPA_get_formal_array(Cnode());
  SUMMARY_PROCEDURE* ipasp = Cnode()->Summary_Proc();
  INT formal_base = ipasp->Get_formal_index();
  INT formal_count = ipasp->Get_formal_count();
  FmtAssert(i >= 0 && i < formal_count, 
            ("CG_BROWSER::Formal_Name: Formal index out of range"));
  INT formal_index = formal_base + i;  
  SUMMARY_FORMAL* sf = &formal_array[formal_index];
  INT symbol_index = sf->Get_symbol_index();
  return Symbol_Name(symbol_index);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Mod_Ref_Formals
// FUNCTION: Print mod/ref information for formals at current node.
//-----------------------------------------------------------------------

void CG_BROWSER::Mod_Ref_Formals(FILE* fp)
{ 
  char buffer[CGB_MAX_STRING_LENGTH];
  IPAA_NODE_INFO* ipaa = Cnode()->Mod_Ref_Info();
  IPA_NODE_SECTION_INFO* ipas = Cnode()->Section_Annot();
  if (ipas == NULL) { 
    INT formal_count = ipaa->Get_fcount();
    for (INT i = 0; i < formal_count; i++) { 
      BOOL indent = FALSE;
      if (ipaa->Is_formal_dmod_elmt(i) || ipaa->Is_formal_imod_elmt(i)) { 
	fprintf(fp, "MOD ");
	fprintf(fp, "%s", Formal_Name(i));
	indent = TRUE; 
      } 
      if (ipaa->Is_formal_dref_elmt(i) || ipaa->Is_formal_iref_elmt(i)) {
	if (indent)
	  fprintf(fp, "     ");
	fprintf(fp, "REF ");
	fprintf(fp, "%s", Formal_Name(i));
	indent = TRUE;
      }
      if (!indent)
	fprintf(fp, "    %s", Formal_Name(i));
      fprintf(fp, "\n");
    } 
  } else { 
    INT formal_count = ipas->Get_formal_count();
    if (formal_count > 0) { 
      for (INT i = 0; i < formal_count; i++) { 
	fprintf(fp, "F#%d: ", i);
	STATE* ipaf = ipas->Get_formal(i);
	if (ipaf->Is_scalar()) { 
	  BOOL indent = FALSE;
	  if (ipaa->Is_formal_dmod_elmt(i) || ipaa->Is_formal_imod_elmt(i)) { 
	    fprintf(fp, "MOD ");
	    fprintf(fp, "%s", Formal_Name(i));
	    indent = TRUE; 
	  } 
	  if (ipaa->Is_formal_dref_elmt(i) || ipaa->Is_formal_iref_elmt(i)) {
	    if (indent)
	      fprintf(fp, "     ");
	    fprintf(fp, "REF ");
	    fprintf(fp, "%s", Formal_Name(i));
	    indent = TRUE;
	  }
	  if (!indent)
	    fprintf(fp, "    %s", Formal_Name(i));
	  fprintf(fp, "\n");
	} else { 
	  BOOL indent = FALSE;
	  if (ipaf->Get_projected_mod_region() != NULL) {
	    fprintf(fp, "MOD ");
	    fprintf(fp, "%s", Formal_Name(i));
	    INT cc = Print_Projected_Region(buffer, 0, 
	      ipaf->Get_projected_mod_region());
	    if (cc >= CGB_MAX_STRING_LENGTH - 1)
	      fprintf(fp, "Warning: string exceeded buffer length!!");
	    fprintf(fp, "%s", buffer);
	    fprintf(fp, "\n");
	    indent = TRUE; 
	  }
	  if (ipaf->Get_projected_ref_region() != NULL) {
	    if (indent)
	      fprintf(fp, "     ");
	    fprintf(fp, "REF ");
	    fprintf(fp, "%s", Formal_Name(i));
	    INT cc = Print_Projected_Region(buffer, 0,
	      ipaf->Get_projected_ref_region());  
	    if (cc >= CGB_MAX_STRING_LENGTH - 1)
	      fprintf(fp, "Warning: string exceeded buffer length!!"); 
	    fprintf(fp, "%s", buffer);
	    fprintf(fp, "\n");
	    indent = TRUE; 
	  }
	  if (ipaf->Get_projected_dcl_region() != NULL) {
	    if (indent)
	      fprintf(fp, "     ");
	    fprintf(fp, "DCL ");
	    fprintf(fp, "%s", Formal_Name(i));
	    INT cc = Print_Projected_Region(buffer, 0,
	      ipaf->Get_projected_dcl_region());  
	    if (cc >= CGB_MAX_STRING_LENGTH - 1)
	      fprintf(fp, "Warning: string exceeded buffer length!!"); 
	    fprintf(fp, "%s", buffer);
	    fprintf(fp, "\n");
	    indent = TRUE; 
	  }
	  if (!indent) { 
	    fprintf(fp, "    %s", Formal_Name(i));
	    fprintf(fp, "\n");
	  }
	} 
      } 
    } 
  } 
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Mod_Ref_Commons
// FUNCTION: Print mod/ref information for commons at current node.
//-----------------------------------------------------------------------

void CG_BROWSER::Mod_Ref_Commons(FILE* fp)
{
  char buffer[CGB_MAX_STRING_LENGTH];

  // Print information about global scalars (non-arrays)
  IPAA_NODE_INFO* ipaa = Cnode()->Mod_Ref_Info();
  ST* st;
  INT i;
  FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {
    if (ST_class(st) == CLASS_VAR &&
        ST_base_idx(st) == ST_st_idx(st) &&
        TY_kind(ST_type(st)) != KIND_ARRAY &&
        !ST_is_split_common(st)) {
      UINT32 mod_ref_key = ST_IDX_index(ST_st_idx(st));
      if (ipaa->Is_def_elmt(mod_ref_key))
        fprintf(fp, "MOD /%s/\n", ST_name(st));
      if (ipaa->Is_eref_elmt(mod_ref_key))
        fprintf(fp, "REF /%s/\n", ST_name(st));
    }
  }
  
  // Print information about individual arrays
  IPA_NODE_SECTION_INFO* ipas = Cnode()->Section_Annot();
  if (ipas == NULL) 
    return; 
  GLOBAL_ARRAY_TABLE* cst = ipas->Global_Array_Table();
  GLOBAL_ARRAY_TABLE_ITER cst_iter(cst);
  GLOBAL_ARRAY_LIST* list = NULL;
  ST_IDX base_st_idx;
  while (cst_iter.Step(&base_st_idx, &list)) { 
    if (list->Is_messy()) { 
      fprintf(fp, "    /%s/ <MESSY> ", ST_name(base_st_idx));
      fprintf(fp, "\n");
      continue;
    } 
    GLOBAL_ARRAY_LIST_ITER iter(list);
    for (iter.First(); !iter.Is_Empty(); iter.Next()) { 
      GLOBAL_ARRAY_INFO* css = iter.Cur();
      if (css->Get_projected_mod_region() != NULL) { 
	fprintf(fp, "MOD ");
	fprintf(fp, "/%s/ ", ST_name(base_st_idx));
	fprintf(fp, "%s", ST_name(css->St_Idx()));
	if (Fancy_Level() >= 2) {
          INT64 common_offset = ST_ofst(ST_ptr(css->St_Idx()));
          INT64 common_size = TY_size(ST_type(css->St_Idx()));
          fprintf(fp, "(%lld:%lld)", common_offset, common_size);
        }
	INT cc = Print_Projected_Region(buffer, 0,
	  css->Get_projected_mod_region());     
	if (cc >= CGB_MAX_STRING_LENGTH - 1)
	  fprintf(fp, "Warning: string exceeded buffer length!!"); 
	fprintf(fp, "%s", buffer);
	fprintf(fp, "\n");
      } 
      if (css->Get_projected_ref_region() != NULL) { 
	fprintf(fp, "REF ");
	fprintf(fp, "/%s/ ", ST_name(base_st_idx));
	fprintf(fp, "%s", ST_name(css->St_Idx()));
	if (Fancy_Level() >= 2) {
          INT64 common_offset = ST_ofst(ST_ptr(css->St_Idx()));
          INT64 common_size = TY_size(ST_type(css->St_Idx()));
          fprintf(fp, "(%lld:%lld)", common_offset, common_size);
        }
	INT cc = Print_Projected_Region(buffer, 0,
	  css->Get_projected_ref_region());
	if (cc >= CGB_MAX_STRING_LENGTH - 1) 
	  fprintf(fp, "Warning: string exceeded buffer length!!");
	fprintf(fp, "%s", buffer);
	fprintf(fp, "\n");
      } 
    }
  } 
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Mod_Ref
// CALL GRAPH BROWSER COMMAND: 'M'
// FUNCTION: Print array section info at this node
//-----------------------------------------------------------------------

void CG_BROWSER::Mod_Ref(FILE* fp)
{ 
  if (Cnode() == NULL || Bad_Node()) { 
    Error_Cleanup();
    return;
  } 
  SECTION_FILE_ANNOT* ipaf = IP_FILE_HDR_section_annot(Cnode()->File_Header());
  if (ipaf == NULL) { 
    Error_Cleanup(); 
    return; 
  }
  if (Cnode()->Summary_Proc() != NULL 
      && Cnode()->Summary_Proc()->Has_incomplete_array_info())
    fprintf(fp, "INCOMPLETE ARRAY INFO\n");
  Mod_Ref_Formals(fp); 
  Mod_Ref_Commons(fp);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Values
// CALL GRAPH BROWSER COMMAND: 'V'
// FUNCTION: Print the values for the execution cost 
//-----------------------------------------------------------------------

void CG_BROWSER::Values(FILE* fp)
{ 
  IPA_NODE_SECTION_INFO* ipas = Cnode()->Section_Annot();
  if (ipas == NULL) { 
    Error_Cleanup();
    return;
  }
  DYN_ARRAY<SUMMARY_VALUE>* sv = ipas->Get_value();
  if (sv == NULL) { 
    Error_Cleanup();
    return;
  } 
  for (INT i = 0; i <= sv->Lastidx(); i++) { 
    SUMMARY_VALUE* svv = &(*sv)[i]; 
    svv->WB_Print(fp, i);
  } 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Expressions
// CALL GRAPH BROWSER COMMAND: 'X'
// FUNCTION: Print the expressions for the execution cost 
//-----------------------------------------------------------------------

void CG_BROWSER::Expressions(FILE* fp)
{ 
  IPA_NODE_SECTION_INFO* ipas = Cnode()->Section_Annot();
  if (ipas == NULL) { 
    Error_Cleanup();
    return;
  }
  DYN_ARRAY<SUMMARY_EXPR>* sx = ipas->Get_expr();
  if (sx == NULL) { 
    Error_Cleanup();
    return;
  } 
  for (INT i = 0; i <= sx->Lastidx(); i++) { 
    SUMMARY_EXPR* sxx = &(*sx)[i];
    sxx->WB_Print(fp, i);
  } 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::DaVinci_Toggle
// CALL GRAPH BROWSER COMMAND: 'd'
// FUNCTION: Toggle Da Vinci mode
//-----------------------------------------------------------------------

void CG_BROWSER::DaVinci_Toggle()
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
// NAME: CG_BROWSER::Print_Formal_Cprop_Annot
// FUNCTION: Print constant propagation information for formals in 
//   'formal_array'.
//-----------------------------------------------------------------------

void CG_BROWSER::Print_Formal_Cprop_Annot(FILE* fp, 
					  INT spaces, 
					  VALUE_DYN_ARRAY* formal_array)
{ 
  if (formal_array == NULL) {
    fprintf(fp, "<TOP>");
    return; 
  } 
  if (formal_array == (void*) -1) { 
    fprintf(fp, "<BOTTOM>");
    return; 
  } 
  if (spaces < 0) { 
    BOOL add_comma = FALSE; 
    fprintf(fp, "<%d:", formal_array->Lastidx() + 1);
    for (INT i = 0; i <= formal_array->Lastidx(); i++) {
      SUMMARY_VALUE* sv = &(*formal_array)[i];
      if (sv->Is_int_const()) { 
        if (add_comma)
	  fprintf(fp, ",");
	fprintf(fp, "F%d=%lld", i, sv->Get_int_const_value());
	add_comma = TRUE; 
      } else if (sv->Is_const_st()) { 
        if (add_comma)
	  fprintf(fp, ",");
	ST_IDX st_idx = sv->Get_const_st_idx();
	ST* st = &St_Table[st_idx];
	fprintf(fp, "F%d=%s", i, Targ_Print(NULL, Tcon_Table[st->u1.tcon]));
	add_comma = TRUE; 
      } 
    } 
    fprintf(fp, ">");
  } else { 
    for (INT i = 0; i <= formal_array->Lastidx(); i++) {
      SUMMARY_VALUE* sv = &(*formal_array)[i];
      if (sv != NULL && !sv->Is_not_const()) { 
	for (INT j = 0; j < spaces; j++)
	  fprintf(fp, " ");
	fprintf(fp, "F#%d: ", i);
	Print_Summary_Value(fp, sv);
	fprintf(fp, "\n");
      } 
    } 
  } 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Cprop_Formals
// FUNCTION: Print constant propagation information for formals at current 
//   node.
//-----------------------------------------------------------------------

void CG_BROWSER::Cprop_Formals(FILE* fp)
{
  const INT spaces = 0;
  VALUE_DYN_ARRAY* formal_array = (VALUE_DYN_ARRAY*) Cnode()->Cprop_Annot();
  if (formal_array == NULL)  
    return; 
  Print_Formal_Cprop_Annot(fp, spaces, formal_array);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Print_Common_Cprop_Annot
// FUNCTION: Print constant propagation information for commons in 
//   'global_annot'.
//-----------------------------------------------------------------------

void CG_BROWSER::Print_Common_Cprop_Annot(FILE* fp, 
					  INT spaces, 
                                          GLOBAL_ANNOT* global_annot)
{ 
  for (UINT32 i = 0; i < GLOBAL_ANNOT::Size; ++i) {
    if (!global_annot->Top(i) && !global_annot->Bottom(i)) {
      char* name = ST_name(GLOBAL_ANNOT::Common_ST[i]);
      const GLOBAL_DYN_ARRAY& gvals = global_annot->Global_Value_Array(i);
      for (INT32 j = 0; j < gvals.Elements(); ++j) {
        if (gvals[j].Value()) {
          for (INT k = 0; k < spaces; k++)
            fprintf(fp, " ");
          fprintf(fp, "/%s/.", name);
          fprintf(fp, "%lld", gvals[j].Offset());
          fprintf(fp, "(%d)", gvals[j].Size());
          fprintf(fp, " ");
          Print_Summary_Value(fp, gvals[j].Value());
          fprintf(fp, "\n");
        }
      }
    }
  }
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Cprop_Commons
// FUNCTION: Print constant propagation information for commons at current 
//   node.
//-----------------------------------------------------------------------

void CG_BROWSER::Cprop_Commons(FILE* fp)
{
  const INT spaces = 0;
  GLOBAL_ANNOT* ga = Cnode()->Global_Annot(); 
  if (ga == NULL)  
    return; 
  if (ga->Bottom()) {  
    fprintf(fp, "COMMONS AT BOTTOM\n"); 
    return; 
  } 
  Print_Common_Cprop_Annot(fp, spaces, ga);
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Cprop_Predecessor_Edges()
// FUNCTION: Print constant propagation information for predecessors of
//   the current node. 
//-----------------------------------------------------------------------

void CG_BROWSER::Cprop_Predecessor_Edges(FILE* fp)
{ 
  const INT spaces = 4;
  fprintf(fp, "PREDECESSORS:\n");
  IPA_PRED_ITER edge_iter(Ipa_Cg(), Cnode());
  for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {
    IPA_EDGE* e = edge_iter.Current_Edge();
    if (e != NULL && e->Cprop_Annot() != NULL) { 
      IPA_NODE* ipan_caller = Ipa_Cg()->Caller(e);
      fprintf(fp, "  ");
      Print_This_Node(fp, ipan_caller, ipan_caller->Node_Index());  
      VALUE_DYN_ARRAY* formal_array = (VALUE_DYN_ARRAY *) e->Cprop_Annot();
      Print_Formal_Cprop_Annot(fp, spaces, formal_array);     
    }    
  } 
}       

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Cprop_Successor_Edges()
// FUNCTION: Print constant propagation information for successors of
//   the current node. 
//-----------------------------------------------------------------------

void CG_BROWSER::Cprop_Successor_Edges(FILE* fp)
{ 
  const INT spaces = 4;
  fprintf(fp, "SUCCESSORS:\n");
  IPA_SUCC_ITER edge_iter(Ipa_Cg(), Cnode());
  for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {
    IPA_EDGE* e = edge_iter.Current_Edge();
    if (e != NULL && e->Cprop_Annot() != NULL) { 
      IPA_NODE* ipan_callee = Ipa_Cg()->Callee(e);
      fprintf(fp, "  ");
      Print_This_Node(fp, ipan_callee, ipan_callee->Node_Index());  
      VALUE_DYN_ARRAY* formal_array = (VALUE_DYN_ARRAY *) e->Cprop_Annot();
      Print_Formal_Cprop_Annot(fp, spaces, formal_array);     
    }    
  } 
}       

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Cprop()
// CALL GRAPH BROWSER COMMAND: 'P'
// FUNCTION: Print info about constants 
//-----------------------------------------------------------------------

void CG_BROWSER::Cprop(FILE* fp)
{ 
  if (Cnode() == NULL || Bad_Node()) { 
    Error_Cleanup();
    return; 
  } 
  Cprop_Formals(fp);  
  Cprop_Commons(fp);
  if (Fancy_Level() >= 2) { 
    Cprop_Predecessor_Edges(fp);
    Cprop_Successor_Edges(fp);
  } 
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Whirl_Browser()
// CALL GRAPH BROWSER COMMAND: 'W'
// FUNCTION: Enter the whirl browser at this node
//-----------------------------------------------------------------------

void CG_BROWSER::Whirl_Browser()
{ 
  PU_Info* pu = Cnode()->PU_Info(); 
  Subsect_State state = PU_Info_state(pu, WT_TREE);
  if (Cnode() == NULL || Bad_Node()) { 
    Error_Cleanup();
    return; 
  } 
  IPA_NODE_CONTEXT context(Cnode());
  BE_Current_Init();
  WB_IPA_Initialize(Cnode()->Whirl_Tree(), &Cnode()->Get_PU()); 
  s_ipa_debug("");
  BE_Current_Fini();
  WB_IPA_Terminate();
} 

//-----------------------------------------------------------------------
// NAME: Compute_ST_IDX
// FUNCTION: Convert a <'st_level','st_index'> to its ST_IDX and return
//   the value.
//-----------------------------------------------------------------------

static ST_IDX Compute_ST_IDX(UINT32 st_level,
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

void CG_BROWSER::Symbol()
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
    Print_ST(stdout, Cnode()->Func_ST(), TRUE);
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BROWSER::Type()
// WHIRL BROWSER COMMAND: 't'
// FUNCTION: Print the type table entry for this node
//-----------------------------------------------------------------------

void CG_BROWSER::Type()
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
    Ty_Table[ST_type(Cnode()->Func_ST())].Print(stdout);
  }
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Help
// CALL GRAPH BROWSER COMMAND: 'H'
// FUNCTION: Print this information
//-----------------------------------------------------------------------

void CG_BROWSER::Help()
{
  for (INT i = 0; Command(i) != '\0'; i++)
    if (Required_Fields_Present(i))
      for (INT j = 0; j < CGB_ASCII_CHAR_COUNT; j++)
        if (_keymap[j] == Command(i))
           fprintf(stdout, "  %c: %s\n", j, Command_Text(i));
  if (!Is_Subcommand()) { 
    fprintf(stdout, "  Q: Exit the debugger\n");
    fprintf(stdout, "  q: Exit the debugger\n");
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

void CG_BROWSER::Initialize_Keymap(char ch)
{
  const INT BUFFER_MAX = 132;
  char file_name[BUFFER_MAX];
  strcpy(file_name, getenv("HOME"));
  strcat(file_name, "/.cgb_keymap");
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
	  ".cgb_keymap: Error on line %d: Cannot map %c\n", 
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
	    ".cgb_keymap: Error on line %d: Cannot map %c\n", 
	    line_number, new_char);
	  continue; 
        } 
	if (print_log)
	  fprintf(stdout, ".cgb_keymap: Translating '%c' to '%c'\n",
	    old_char, new_char);
	_keymap[new_char] = old_char;
      } else { 
        if (old_char == ch) { 
	  old_char = key_buffer[key_buffer_start++];
	  if (Unmappable_Character(old_char)) {
	    fprintf(stdout, 
	      ".cgb_keymap: Error on line %d: Cannot map %c\n", 
	      line_number, old_char);
	    continue; 
          } 
	  Scan_Blanks_And_Tabs(key_buffer, &key_buffer_start);
	  char new_char = key_buffer[key_buffer_start++];
	  if (Unmappable_Character(new_char)) {
	    fprintf(stdout, 
	      ".cgb_keymap: Error on line %d: Cannot map %c\n", 
	      line_number, new_char);
            continue; 
          } 
	  if (print_log)
	    fprintf(stdout, ".cgb_keymap: Translating '%c' to '%c'\n",
	      old_char, new_char);
	  _keymap[new_char] = old_char;
	}
      } 
    } else {
      fprintf(stdout, ".cgb_keymap: Error on line %d: Unrecognized command\n",
        line_number);
    }
  }
  fclose(fp_keymap);
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Invoke_Command
// FUNCTION: Invoke the command indicated by character 'ch'.
//-----------------------------------------------------------------------

void CG_BROWSER::Invoke_Command(char ch)
{
  switch (_keymap[ch]) { 
  case 'R': 
    Root(stdout); 
    break; 
  case 'C': 
    Callers(stdout);
    break;
  case 'c': 
    Callees(stdout);
    break; 
  case 'E': 
    Edges_In(stdout);
    break; 
  case 'e': 
    Edges_Out(stdout);
    break; 
  case '=': 
    Set_Node(stdout); 
    break; 
  case 'v': 
    Set_Vertex(stdout);
    break;
  case '@': 
    Address(stdout);
    break; 
  case 'G': 
    Graph(stdout); 
    break; 
  case 'F': 
    Find(stdout); 
    break; 
  case 'P': 
    Cprop(stdout);
    break; 
  case 'M': 
    Mod_Ref(stdout); 
    break; 
  case 'V': 
    Values(stdout);
    break; 
  case 'X':
    Expressions(stdout);
    break;
  case 'd': 
    DaVinci_Toggle();
    break;
  case 'S': 
    Summary(stdout);
    break; 
  case 's': 
    Symbol();
    break;
  case 't': 
    Type();
    break;
  case '<':
    Fancy_Down(); 
    break;
  case '>': 
    Fancy_Up(); 
    break; 
  case 'W': 
    Whirl_Browser(); 
    break; 
  case 'H': 
  case 'h': 
    Help(); 
    break; 
  default:
    fprintf(stdout, "Bad character: %c\n", ch);
    break;
  }
} 

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Sdebug
// FUNCTION: The 'string debugger'. Run the call graph browser using 
//   'init_buffer' as the initial set of commands.
//-----------------------------------------------------------------------

void CG_BROWSER::Sdebug(const char init_buffer[])
{
  char ch;
  BOOL reload;

  if (Ipa_Cg() == NULL) {
    fprintf(stdout, "Call graph browser not valid in this phase.\n");
    Error_Cleanup();
    return;
  }
  MEM_POOL_Initialize(&_pool, "cgb_pool", FALSE);
  Buffer().Reset_Buffer(); 
  Initialize_Keymap(' ');
  fprintf(stdout, "CALL GRAPH BROWSER: \n");
  Root(stdout); 
  CGB_Prompt();
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
      CGB_Prompt();
      reload = TRUE;
      continue;
    }
    if (ch == 'Q' || ch == 'q') {
      MEM_POOL_Delete(&_pool);
      return;
    } 
    Invoke_Command(ch);
  }
}

//-----------------------------------------------------------------------
// NAME: CG_BROWSER::Debug
// FUNCTION: The main entry point to the call graph browser.  To start up the
//   call graph browser in dbx, say 'p cgdebug()'.
//-----------------------------------------------------------------------

void CG_BROWSER::Debug() 
{ 
  Sdebug("");
}

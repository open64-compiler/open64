/*
 * Copyright (C) 2009, 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include "pu_info.h"
#include "ara.h"
#include "fiz_fuse.h"
#include "lwn_util.h"
#include "dep_graph.h"
#include "snl.h"
#include "snl_nest.h"
#include "snl_xbounds.h"
#include "scalar_expand.h"
#include "wn_simp.h"
#include "parmodel.h"

#include "targ_sim.h"
#include "strtab.h"
#include "dvector.h"
#include "opt_wn.h"
#include "access_vector.h"
#include "dep_graph.h"
#include "lego_opts.h"

#include "lnoutils.h"
#include "lnopt_main.h"
#include "stab.h"
#include "targ_const.h"
#include "reduc.h"
#include "opt_alias_mgr.h"
#include "snl_trans.h"
#include "permute.h"
#include "fusion.h"
#include "reverse.h"
#include "model.h"
#include "cache_model.h"
#include "debug.h" 
#include "whirl2src.h"
#include "sxlist.h"
#include "sxlimit.h"
#include "parallel.h"
#include "snl_dist.h"
#include "mat.h"
#include "lu_mat.h"
#include "wb_util.h"
#include "wb_carray.h"
#include "wb_buffer.h"
#include "wb_browser.h"
#include "wb.h"
#include "DaVinci.h"
#include "wutil.h"
#include "intrn_info.h"  // INTRINSIC_name

#define 	BUFFER_MAX 		132 
#define 	MAX_FORMULA_VARS 	100
#define 	ASCII_CHAR_COUNT	256

enum SOURCE_LANGUAGE {SRC_NONE, SRC_FORTRAN, SRC_C}; 
enum SKIP_CLASS	     {SKIP_NONE, SKIP_ALPHANUMERIC, SKIP_NUMERIC, SKIP_HEX}; 

WN*     	WB_global_fd = NULL;
DU_MANAGER*  	WB_du_mgr;
ALIAS_MANAGER* 	WB_alias_mgr;
INT     	WB_sanity_check_level = 0;
SOURCE_LANGUAGE WB_language = SRC_NONE;  
char 		WB_keymap[ASCII_CHAR_COUNT]; 
char 		last_ch = '\0'; 
BOOL 		init_mode = FALSE;
BOOL 		WB_davinci_mode = FALSE; 

extern ARRAY_DIRECTED_GRAPH16* Array_Dependence_Graph;
extern REDUCTION_MANAGER* red_manager;

typedef void (*VOID_FUNC_PTR)();
typedef BOOL (*BOOL_FUNC_PTR)();
const INT MAX_STRING_LENGTH = 1000; 

static WN* cnode = NULL; 
static INT fancy = 2; 
const INT fancy_min = 2; 
const INT fancy_max = 3; 
static VOID_FUNC_PTR last_fp;

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
  "LDMA",
#ifdef KEY
  "ASM_STMT",
  "ASM_EXPR",
  "ASM_INPUT",
  "RROTATE",
  "LDA_LABEL",
  "GOTO_OUTER_BLOCK",
  "EXTRACT_BITS",
  "COMPOSE_BITS",
#endif
#ifdef TARG_X8664
  "REPLICATE",
  "REDUCE_ADD",
  "REDUCE_MPY",
  "REDUCE_MAX",
  "REDUCE_MIN",
  "PURE_CALL_OP",
  "SHUFFLE",
  "ATOMIC_RSQRT",
#elif defined(TARG_LOONGSON)
  "MPYU2",
  "MPYI2",
  "PURE_CALL_OP",
#elif defined(TARG_MIPS)
  "PURE_CALL_OP",
#endif
}; 

//-----------------------------------------------------------------------
// THE FOLLOWING FUNCTIONS ARE EXTERNAL FUNCTIONS USED TO SET UP THE 
// WHIRL BROWSER AND PROVIDE PRINTING FOR DEBUGGING UNDER THE VERBOSE 
// OPTION. (-tt31:0x4). 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: WB_LNO_Initialize 
// FUNCTION: Initialize the whirl browser to operate on the function 
//   with descriptor 'global_fd', DU manager 'du_mgr', and ALIAS manager
//   'alias_mgr'. Also start it at 'sanity_check_level' for checking 
//   code consistency. 
//-----------------------------------------------------------------------

extern void WB_LNO_Initialize(WN* global_fd,
			      DU_MANAGER* du_mgr, 
			      ALIAS_MANAGER* alias_mgr,  
		              INT sanity_check_level)
{ 
  WB_Set_Phase(WBP_LNO); 
  WB_global_fd = global_fd;
  WB_du_mgr = du_mgr; 
  WB_alias_mgr = alias_mgr;  
  WB_sanity_check_level = sanity_check_level; 
  for (INT i = 0; i < ASCII_CHAR_COUNT; i++)
    WB_keymap[i] = i; 
} 

//-----------------------------------------------------------------------
// NAME: WB_LNO_Terminate 
// FUNCTION: Terminate operation of the whirl browser on the current 
//   program unit. 
//-----------------------------------------------------------------------

extern void WB_LNO_Terminate(void)
{
  WB_Set_Phase(WBP_NONE); 
  WB_global_fd = NULL; 
  WB_sanity_check_level = 0; 
} 

//-----------------------------------------------------------------------
// NAME: WB_Set_Sanity_Check_Level 
// FUNCTION: External interface function to change the value of the 
//   whirl browser sanity level to 'sanity_check_level'. 
//-----------------------------------------------------------------------

extern void WB_Set_Sanity_Check_Level(INT sanity_check_level)
{  
  FmtAssert(sanity_check_level >= WBC_MIN && sanity_check_level <= WBC_MAX,  
    ("Set sanity check level to invalid value.")); 
  WB_sanity_check_level = sanity_check_level; 
} 

//-----------------------------------------------------------------------
// NAME: WB_Whirl_Symbol 
// FUNCTION: Returns a pointer to a printable string of characters 
//   describing the symbol of the node 'wn'.  For loads and stores, 
//   the symbol is printed, if any.  For do loops, the symbol of the 
//   do loop is printed.
//-----------------------------------------------------------------------

extern const char* WB_Whirl_Symbol(WN* wn) 
{ 
  if (wn == NULL)
    return NULL; 
  WN* wn_symbol = NULL;
  const char* name = NULL; 
  OPCODE opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);
  if (opc == OPC_PRAGMA || opc == OPC_XPRAGMA) 
    return WN_pragmas[WN_pragma(wn)].name;  
  if (opr == OPR_INTRINSIC_CALL)
    return INTRINSIC_name((INTRINSIC) WN_intrinsic(wn));
  wn_symbol = (opc == OPC_DO_LOOP) ? WN_index(wn) : (OPCODE_has_sym(opc))
    ? wn : NULL; 
  if (wn_symbol == NULL) 
    return NULL;
  if (WN_st(wn_symbol) == NULL) 
    return NULL;   
  name = ST_class(WN_st(wn_symbol)) != CLASS_PREG
    ? ST_name(WN_st(wn_symbol)) :
      WN_offset(wn_symbol) > Last_Dedicated_Preg_Offset
    ? Preg_Name(WN_offset(wn_symbol)) : "DEDICATED PREG";
  return name; 
} 

//-----------------------------------------------------------------------
// NAME: WB_Whirl_Symbol_Type 
// FUNCTION: Returns a pointer to a printable string of characters 
//   describing the symbol of the node 'wn'.  For loads and stores, 
//   the symbol is printed, if any.  For do loops, the symbol of the 
//   do loop is printed.  For nodes with no symbol information, the 
//   type is printed.
//-----------------------------------------------------------------------

extern const char* WB_Whirl_Symbol_Type(WN* wn) 
{ 
  WN* wn_symbol = NULL;
  const char* name = NULL; 
  OPCODE opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);
  if (opc == OPC_PRAGMA || opc == OPC_XPRAGMA) 
    return WN_pragmas[WN_pragma(wn)].name;  
  wn_symbol = (opc == OPC_DO_LOOP) ? WN_index(wn) : (OPCODE_has_sym(opc))
    ? wn : NULL; 
  if (wn_symbol == NULL) 
    return (char *) (OPCODE_name(WN_opcode(wn)) + 4);
  if (WN_st(wn_symbol) == NULL) 
    return NULL;   
  name = ST_class(WN_st(wn_symbol)) != CLASS_PREG
    ? ST_name(WN_st(wn_symbol)) :
      WN_offset(wn_symbol) > Last_Dedicated_Preg_Offset
    ? Preg_Name(WN_offset(wn_symbol)) : "DEDICATED PREG";

  return name; 
} 

//-----------------------------------------------------------------------
// THE FOLLOWING TABLES ARE THE MAIN DRIVING MECHANISM FOR THE WHIRL 
// BROWSER.  
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: Command_List 
// FUNCTION: This is the main command list for the whirl browser.  Each 
//   row has four entries: 
//   (1) The single character which invokes the command. 
//   (2) Whether the command is complete in itself (TRUE) or needs 
//       additional to prompt the user for additional information (FALSE). 
//       Commands needing additional information cannot be reinvoked with 
//       the '.' command.  
//   (3) A one line description of the command. 
//   (4) The function invoked when the command is typed. 
//   By convention, all names of these functions should start with the 
//   prefix WB.  
//-----------------------------------------------------------------------

struct WB_CHAR_FUNCPTR { 
  char ch;
  BOOL context_independent; 
  const char* message;  
  VOID_FUNC_PTR fp; 
};  

static void WB_root(); 
static void WB_parent();
static void WB_kids(); 
static void WB_next(); 
static void WB_previous(); 
static void WB_statements(); 
static void WB_set_node(); 
static void WB_address(); 
static void WB_this_tree();
static void WB_addresses();
static void WB_uses(); 
static void WB_defs(); 
static void WB_access();
static void WB_RR_Map();
static void WB_node_deps(); 
static void WB_stmt_deps(); 
static void WB_fancyup();  
static void WB_fancydown();  
static void WB_loops(); 
static void WB_check(); 
static void WB_find(); 
static void WB_find_symbols();
static void WB_findopr(); 
static void WB_symbol(); 
static void WB_type(); 
static void WB_type_table(); 
static void WB_vertices(); 
static void WB_reduction(); 
static void WB_alias(); 
static void WB_ancestors();
static void WB_lisp_loops();
static void WB_transform(); 
static void WB_whirl2fc();
static void WB_whirl2fset();
static void WB_whirl2cset();
static void WB_scratch(); 
static void WB_last_command(); 
static void WB_Set_Map_Id(); 
static void WB_Map_Id(); 
static void WB_private(); 
static void WB_vset_node(); 
static void WB_davinci(); 
static void WB_help(); 

static WB_CHAR_FUNCPTR Command_List[] = 
{
  'R', TRUE,  "Go to the root of the program unit", 		WB_root, 
  'E', TRUE,  "Go to the parent of the current node", 		WB_parent,
  'N', TRUE,  "Go to the next node in the subtree chain", 	WB_next, 
  'P', TRUE,  "Go to the previous node in the subtree chain",	WB_previous, 
  '=', FALSE, "Set the current node to the following address",  WB_set_node, 
  '@', FALSE, "Set the current node to this numbered node", 	WB_address, 
  '>', TRUE,  "Increase the fanciness level for printing", 	WB_fancyup, 
  '<', TRUE,  "Decrease the fanciness level for printing", 	WB_fancydown, 
  'W', TRUE,  "Print tree in whirl2[fc] format at current node",WB_whirl2fc,
  'K', TRUE,  "Print the kids of this node",			WB_kids, 
  'S', TRUE,  "Print the list of statements under this node", 	WB_statements, 
  'T', TRUE,  "Print the tree at the current node", 		WB_this_tree,
  '%', TRUE,  "Print the addresses of nodes in this subtree",   WB_addresses, 
  'U', TRUE,  "Print the uses at the current node", 		WB_uses, 
  'D', TRUE,  "Print the definitions at the current node", 	WB_defs, 
  'A', TRUE,  "Print the access info at the current node", 	WB_access,
  'G', TRUE,  "Print the node dep graph info at current node", 	WB_node_deps, 
  'g', TRUE,  "Print the stmt dep graph info at current node", 	WB_stmt_deps, 
  'V', TRUE,  "Print the vertices of the dep graph",		WB_vertices,
  'r', TRUE,  "Print the reduction manager", 			WB_reduction,  
  'a', TRUE,  "Print the alias info for this node", 		WB_alias,  
  'e', TRUE,  "Print the ancestors of this node", 		WB_ancestors, 
  's', TRUE,  "Print the symbol table entry for this node",     WB_symbol, 
  't', TRUE,  "Print the type table entry for this node",       WB_type, 
  '#', FALSE, "Print node of this type at this address",	WB_type_table, 
  'L', TRUE,  "Sketch the loop nests in the program unit", 	WB_loops, 
  'l', TRUE,  "Sketch the loop nests in Suresh Tool format",	WB_lisp_loops,
  'y', FALSE, "Perform a transformation",			WB_transform, 
  'C', TRUE,  "Check the program unit for consistency", 	WB_check, 
  'F', FALSE, "Find nodes with this symbol in the subtree", 	WB_find, 
  '$', FALSE, "Find symbols with this name",                    WB_find_symbols,
  'o', FALSE, "Find nodes with this operator type", 		WB_findopr,   
  'f', TRUE,  "Change WHIRL-TO-SOURCE language to FORTRAN", 	WB_whirl2fset,
  'c', TRUE,  "Change WHIRL-TO-SOURCE language to C", 		WB_whirl2cset,
  'B', FALSE, "Print the scratch register with this number", 	WB_scratch, 
  '.', TRUE,  "Execute the previous command", 			WB_last_command,
  'Y', TRUE,  "Print the reshaped-ref map for array nodes",     WB_RR_Map,
  'M', TRUE,  "Set the current node to node with this map id",  WB_Set_Map_Id, 
  'm', TRUE,  "Print the map-id at this node", 			WB_Map_Id, 
  'p', TRUE,  "Print the privatization info at this node",      WB_private, 
  'v', FALSE, "Go to the node with this vertex number", 	WB_vset_node,  
  'd', TRUE,  "Toggle Da Vinci mode", 				WB_davinci, 
  'H', TRUE,  "Print this information", 			WB_help, 
  'h', TRUE,  "Print this information", 			WB_help, 
  '\0',FALSE, "", NULL
};  

//-----------------------------------------------------------------------
// NAME: Type_List
// FUNCTION: The following table is used by the '#' command to print a 
//   list of types understood by the whirl browser.  Each table entry 
//   has two parts: 
//   (1) A string listing the name of the type. 
//   (2) The function invoked to print an object of this type.
//   By convention, all names of these function should start with the 
//   prefix WBT.   
//-----------------------------------------------------------------------

struct WB_STRING_VOIDFUNCPTR { 
  const char* type;
  VOID_FUNC_PTR fp; 
};  

static void WBT_SNL_DEP_INFO();
static void WBT_SNL_DEP_MATRIX(); 
static void WBT_SNL_NEST_INFO(); 
static void WBT_DOLOOP_STACK(); 
static void WBT_ARRAY_REF_STAR(); 
static void WBT_ARRAY_REF_LIST(); 
static void WBT_COMPUTE_FOOTPRINT_RVAL(); 
static void WBT_MHD_LEVEL(); 
static void WBT_MHD(); 
static void WBT_FORMULA(); 
static void WBT_FORMULA_EVALUATE();
static void WBT_PAR_STAT(); 
static void WBT_MAT_INT(); 
static void WBT_MAT_FRAC(); 
static void WBT_LU_MAT_INT(); 
static void WBT_LU_MAT_FRAC(); 

static WB_STRING_VOIDFUNCPTR Type_List[] = 
{ 
  "SNL_DEP_INFO", 		WBT_SNL_DEP_INFO, 
  "SNL_DEP_MATRIX",		WBT_SNL_DEP_MATRIX, 
  "SNL_NEST_INFO", 		WBT_SNL_NEST_INFO, 
  "DOLOOP_STACK",		WBT_DOLOOP_STACK, 
  "ARRAY_REF*",     		WBT_ARRAY_REF_STAR,
  "ARRAY_REF_LIST",     	WBT_ARRAY_REF_LIST,
  "COMPUTE_FOOTPRINT_RVAL", 	WBT_COMPUTE_FOOTPRINT_RVAL, 
  "MHD_LEVEL", 			WBT_MHD_LEVEL, 
  "MHD", 			WBT_MHD, 
  "FORMULA", 			WBT_FORMULA, 
  "FORMULA EVALUATE", 		WBT_FORMULA_EVALUATE, 
  "PAR_STAT",			WBT_PAR_STAT, 
  "MAT<INT>", 		        WBT_MAT_INT,  
  "MAT<FRAC>", 		        WBT_MAT_FRAC,  
  "LU_MAT<INT>", 		WBT_LU_MAT_INT,  
  "LU_MAT<FRAC>", 		WBT_LU_MAT_FRAC,  
  "", 				NULL	
};  

//-----------------------------------------------------------------------
// NAME: Transform_List 
// FUNCTION: The following table is used by the 'y' command to let the 
//   user specify loop transformations known by the whirl browser.  Each 
//   table entry has two parts: 
//   (1) A string listing the name of the transformation. 
//   (2) The function invoked to prompt the user for information needed
//       to do the transformation and perform the transformation. 
//   By convention, all names of these functions should start with the 
//   prefix WBTR.  
//-----------------------------------------------------------------------

struct WB_STRING_BOOLFUNCPTR { 
  const char* type;
  BOOL_FUNC_PTR fp; 
};  

static BOOL WBTR_Pre_Loop_Peeling(); 
static BOOL WBTR_Post_Loop_Peeling(); 
static BOOL WBTR_Loop_Reversal(); 
static BOOL WBTR_Loop_Permutation(); 
static BOOL WBTR_Scalar_Expansion(); 
static BOOL WBTR_Distribution(); 
static BOOL WBTR_SNL_INV_Limited_SE_And_Dist();  
static BOOL WBTR_Loop_Tiling();  

static WB_STRING_BOOLFUNCPTR Transform_List[] =
{   
  "Pre Loop Peeling", 	  	WBTR_Pre_Loop_Peeling,   
  "Post Loop Peeling", 		WBTR_Post_Loop_Peeling,   
  "Loop Reversal", 		WBTR_Loop_Reversal, 
  "Loop Permutation",		WBTR_Loop_Permutation, 
  "Scalar Expansion", 		WBTR_Scalar_Expansion, 
  "Distribution", 		WBTR_Distribution, 
  "Limited SE and Dist", 	WBTR_SNL_INV_Limited_SE_And_Dist, 
  "Loop Tiling", 		WBTR_Loop_Tiling, 
  "", 				NULL
};   

//-----------------------------------------------------------------------
// SOME SUPPORT FUNCTIONS NEEDED TO IMPLEMENT THE WHIRL BROWSER COMMANDS. 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: this_node 
// FUNCTION: Write the address of 'wn' and its whirl symbol to 'stdout'. 
//-----------------------------------------------------------------------

static void this_node(WN* wn, 
		      BOOL print_vertex = TRUE,
		      BOOL print_brackets = FALSE) 
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  if (wn == NULL) { 
    fprintf(stdout, "<NULL>"); 
    return; 
  } 
  const char* ch = OPCODE_name(WN_opcode(wn));  
  if (print_brackets) 
    fprintf(stdout, "[0x%p] ", wn); 
  else 
    fprintf(stdout, "0x%p ", wn); 
  if (print_vertex && dg != NULL && dg->Get_Vertex(wn) != 0)
    fprintf(stdout, "V#%d ", dg->Get_Vertex(wn)); 
  fprintf(stdout, "%s ", ch); 
  if (fancy >= 3) 
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
// NAME: print_this_node 
// FUNCTION: Print the node 'wn' and a newline. 
//-----------------------------------------------------------------------

static void print_this_node(WN* wn, 
	                    BOOL print_vertex = TRUE,
			    BOOL print_brackets = FALSE)
{
  this_node(wn, print_vertex, print_brackets); 
  fprintf(stdout, "\n");
}

//-----------------------------------------------------------------------
// NAME: print_this_node 
// FUNCTION: Print the current node and a newline. 
//-----------------------------------------------------------------------

static void print_this_node()
{
  print_this_node(cnode); 
} 

//-----------------------------------------------------------------------
// NAME: bell
// FUNCTION: Sound the bell, indicating that an error has occurred. 
//-----------------------------------------------------------------------

static void bell()
{ 
  fprintf(stdout, "%c", '\007');
  fflush(stdout); 
} 

//-----------------------------------------------------------------------
// NAME: dump_spaces
// FUNCTION: Print 'spaces' number of spaces to the file 'fp'. 
//-----------------------------------------------------------------------

static void dump_spaces(FILE* fp, INT spaces)
{
  for (INT i = 0; i < spaces; i++)
    fprintf(fp, " ");
}

//-----------------------------------------------------------------------
// NAME: prompt 
// FUNCTION: Print the whirl browser prompt symbol to 'stdout'. 
//-----------------------------------------------------------------------

static void prompt()
{ 
  fprintf(stdout, "WB> "); 
}  

//-----------------------------------------------------------------------
// NAME: this_fp
// FUNCTION: Return the a pointer to the function in the 'Command_List' 
//   which corresponds to the character 'ch'.  Return NULL if no whirl 
//   browser command corresponds to this command.  
//-----------------------------------------------------------------------

static VOID_FUNC_PTR this_fp(char ch)
{ 
  for (INT i = 0; Command_List[i].ch != '\0'; i++) 
    if (Command_List[i].ch == ch)
      return Command_List[i].fp; 
  return NULL; 
}

//-----------------------------------------------------------------------
// SUPPORT FUNCTIONS WHICH MANIPULATE 'carray', THE SAVED NODE ARRAY. 
//-----------------------------------------------------------------------

const INT MAX_SAVED_NODES = 500; 
static WN* carray[MAX_SAVED_NODES];  // saved node array 
static INT carray_max = 0;	     // next free index in 'carray' 

//-----------------------------------------------------------------------
// NAME: enter_this_node 
// FUNCTION: Enter the node 'wn' into the saved node array 'carray'. 
//   Update the value of 'index', the index of the next available entry 
//   in 'carray'.  Print index and address of 'wn' node.
// NOTE: The 'wn' is not entered into the 'carray' if it is already full.  
//-----------------------------------------------------------------------

static void enter_this_node(INT& index, WN* wn, BOOL print_vertex = TRUE) 
{
  fprintf(stdout, "[%d] ", index); 
  this_node(wn, print_vertex); 
  if (index < MAX_SAVED_NODES) 
    carray[index] = wn; 
  index++;
} 

//-----------------------------------------------------------------------
// NAME: enter_this_node_unique
// FUNCTION: Same as 'enter_this_node', except the 'wn' is not entered 
//   if it is already there. 
//-----------------------------------------------------------------------

static void enter_this_node_unique(INT& index, 
				   WN* wn, 
				   BOOL print_vertex = TRUE) 
{
  INT i;
  for (i = 0; i < index; i++) 
    if (wn == carray[i]) 
      break; 
  fprintf(stdout, "[%d] ", i);
  this_node(wn, print_vertex); 
  if (i < index) 
    return;  
  if (index < MAX_SAVED_NODES)
    carray[index] = wn;
  index++; 
} 

//-----------------------------------------------------------------------
// SUPPORT FUNCTIONS WHICH MANIPULATE 'buffer', THE COMMAND INPUT BUFFER. 
//-----------------------------------------------------------------------

char		buffer[BUFFER_MAX];	// command input buffer 
INT 		buffer_start = 0;	// next free index in 'buffer'

//-----------------------------------------------------------------------
// NAME: load_buffer
// FUNCTION: Load the 'buffer' up to the next newline character with 
//   input from 'stdin'. 
//-----------------------------------------------------------------------

static void load_buffer()
{
  for (INT i = 0; ; i++) {
    buffer[i] = fgetc(stdin);
    if (buffer[i] == '\n')
      break;
  }
  buffer_start = 0;
}

//-----------------------------------------------------------------------
// NAME: scan_blanks_and_tabs 
// FUNCTION: In the 'buffer' advance 'buffer_start' past all blanks and 
//   tabs. 
//-----------------------------------------------------------------------

static void scan_blanks_and_tabs()
{
  char ch; 
  do { 
    ch = buffer[buffer_start++];  
  } while (ch == ' ' || ch == '\t');
  buffer_start--;
}

//-----------------------------------------------------------------------
// NAME: scan_blanks_and_tabs 
// FUNCTION: In the 'buffer' advance 'buffer_start' past all blanks and 
//   tabs. 
//-----------------------------------------------------------------------

static void scan_blanks_and_tabs(char buffer[],
				 INT* buffer_start)
{
  char ch; 
  do { 
    ch = buffer[(*buffer_start)++];  
  } while (ch == ' ' || ch == '\t');
  (*buffer_start)--;
}

//-----------------------------------------------------------------------
// NAME: skip_to_separator
// FUNCTION: In the 'buffer' advance 'buffer_start' past the type of 
//   characters specified 'skip_type'.  
//-----------------------------------------------------------------------

static void skip_to_separator(SKIP_CLASS skip_type)
{
  switch (skip_type) {
  case SKIP_ALPHANUMERIC:
    while (buffer[buffer_start] != ' ' &&  buffer[buffer_start] != '\t' 
        && buffer[buffer_start] != ';' && buffer[buffer_start] != '\n')
      buffer_start++;
    break; 
  case SKIP_NUMERIC:
    while (isdigit(buffer[buffer_start])) 
      buffer_start++;
    break; 
  case SKIP_HEX:
    while (isxdigit(buffer[buffer_start])) 
      buffer_start++;
    break; 
  default:
    while (buffer[buffer_start] != ' ' &&  buffer[buffer_start] != '\t' 
        && buffer[buffer_start] != ';' && buffer[buffer_start] != '\n')
      buffer_start++;
  }
  if (buffer[buffer_start] == '\n')  
    return;
  while (buffer[buffer_start] == ' ' || buffer[buffer_start] == '\t' 
      || buffer[buffer_start] == ';')
    buffer_start++;  
}

//-----------------------------------------------------------------------
// NAME: Error_Cleanup 
// FUNCTION: Sound the bell, write the prompt to 'stdout' and reload the 
//   input buffer, scanning past blanks and tabs.  
//-----------------------------------------------------------------------

static void Error_Cleanup()
{
  bell();
  prompt();
  load_buffer();
  scan_blanks_and_tabs();
}

//-----------------------------------------------------------------------
// NAME: Load_Loop 
// FUNCTION: Query the user for a loop and place the result at '*wn_loop'.  
//-----------------------------------------------------------------------

static BOOL Load_Loop(WN** wn_loop) 
{
  load_buffer(); 
  scan_blanks_and_tabs(); 
  sscanf(buffer + buffer_start, "0x%p", wn_loop); 
  buffer_start += 2;
  skip_to_separator(SKIP_HEX);  
  if (WN_opcode(*wn_loop) != OPC_DO_LOOP) {
    fprintf(stdout, "Address is not address of a do loop.\n"); 
    Error_Cleanup(); 
    return FALSE; 
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Load_UINT32  
// FUNCTION: Query the user for an UINT32 and place the result at '*int_value'. 
//-----------------------------------------------------------------------

static void Load_UINT32(UINT32* int_value) 
{
  load_buffer(); 
  scan_blanks_and_tabs(); 
  sscanf(buffer + buffer_start, "%d", int_value); 
  skip_to_separator(SKIP_NUMERIC); 
}

//-----------------------------------------------------------------------
// NAME: Load_Integer 
// FUNCTION: Query the user for an Integer and place the result at '*int_value'.
//-----------------------------------------------------------------------

static void Load_Integer(INT* int_value) 
{
  load_buffer(); 
  scan_blanks_and_tabs(); 
  sscanf(buffer + buffer_start, "%d", int_value); 
  skip_to_separator(SKIP_NUMERIC); 
}

//-----------------------------------------------------------------------
// NAME: Load_mINT32 
// FUNCTION: Query the user for a mINT32 and place the result at '*value'.
//-----------------------------------------------------------------------

static void Load_mINT32(mINT32* value) 
{
  load_buffer(); 
  scan_blanks_and_tabs(); 
  sscanf(buffer + buffer_start, "%d", value); 
  skip_to_separator(SKIP_NUMERIC); 
}

//-----------------------------------------------------------------------
// NAME: Load_mINT64 
// FUNCTION: Query the user for a mINT64 and place the result at '*value'.
//-----------------------------------------------------------------------

static void Load_mINT64(mINT64* value) 
{
  load_buffer(); 
  scan_blanks_and_tabs(); 
  sscanf(buffer + buffer_start, "%lld", value); 
  skip_to_separator(SKIP_NUMERIC); 
}

//-----------------------------------------------------------------------
// NAME: Load_Double 
// FUNCTION: Query the user for a Double and place the result at '*value'.
//-----------------------------------------------------------------------

static void Load_Double(double* value) 
{
  load_buffer(); 
  scan_blanks_and_tabs(); 
  sscanf(buffer + buffer_start, "%lg", value); 
  skip_to_separator(SKIP_NUMERIC); 
}

//-----------------------------------------------------------------------
// NAME: Load_Boolean 
// FUNCTION: Query the user for a boolean value, and place the result at 
//   'bool_value'. If 'default_present' is TRUE, a default value (either 
//   'Y' or 'N' should be assumed, even when the user gives no response. 
//   When 'default_present' is TRUE, 'default_value' should be set to TRUE
//   if the assumed default value is 'TRUE', and to 'FALSE' if the assumed
//   value is FALSE.   
//-----------------------------------------------------------------------

static void Load_Boolean(BOOL* bool_value, BOOL default_present = FALSE, 
  BOOL default_value = FALSE)
{
  load_buffer();
  scan_blanks_and_tabs();
  if (!default_present) {
    if (buffer[buffer_start] == 'Y' || buffer[buffer_start] == 'y')
      *bool_value = TRUE; 
    if (buffer[buffer_start] == 'N' || buffer[buffer_start] == 'n')
      *bool_value = FALSE;
    return; 
  }
  if (default_value == TRUE) {
    *bool_value = TRUE; 
    if (buffer[buffer_start] == 'N' || buffer[buffer_start] == 'n')
      *bool_value = FALSE;
  } else { 
    *bool_value = FALSE; 
    if (buffer[buffer_start] == 'Y' || buffer[buffer_start] == 'y')
      *bool_value = TRUE;
  }
}

//-----------------------------------------------------------------------
// SUPPORT FUNCTIONS TO PRINT NODE REPRESENTATIONS IN THE WHIRL BROWSER. 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: dump_whirl_node 
// FUNCTION: Write into 'buffer' a short but informative description of 
//   the node 'wn'.  Start writing at buffer[cc].  Return the position 
//   of the first empty character in 'buffer'.  
//-----------------------------------------------------------------------

static INT dump_whirl_node(WN* wn, 
		       char* buffer, 
		       INT cc)
{
  BOOL printed = FALSE;

  if (wn == NULL) {
    cc += sprintf(buffer + cc, "<null>");
    return cc;
  }

  OPCODE   opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);

  printed = TRUE;
  switch(opr) {
   case OPR_CONST:
    switch (OPCODE_rtype(opc)) {
     case MTYPE_F4:
      cc += sprintf(buffer + cc, "%g", STC_val(WN_st(wn)).vals.fval);
      break;
     case MTYPE_F8:
      cc += sprintf(buffer + cc, "%g", STC_val(WN_st(wn)).vals.dval);
      break;
     default:
      printed = FALSE;
      break;
    }
    break;
   case OPR_INTCONST:
    cc += sprintf(buffer + cc, "%lld", WN_const_val(wn));
    break;
   case OPR_LDID:
    cc += sprintf(buffer + cc, "%s", WB_Whirl_Symbol(wn));
    break; 
   case OPR_ADD:
    cc += sprintf(buffer + cc, "+");
    break;
   case OPR_SUB:
    cc += sprintf(buffer + cc, "-");
    break;
   case OPR_MPY:
    cc += sprintf(buffer + cc, "*");
    break;
   case OPR_DIV:
    cc += sprintf(buffer + cc, "/");
    break;
   default:
    printed = FALSE;
    break;
  }

  if (!printed) {
    FmtAssert(strncmp(OPCODE_name(opc), "OPC_", 4) == 0,
	      ("opname=%s", OPCODE_name(opc)));

    cc += sprintf(buffer + cc, "%s", OPCODE_name(opc) + 4);
    if (OPCODE_has_sym(opc))
      cc += sprintf(buffer + cc, " %s", SYMBOL(wn).Name());
    if (OPCODE_has_label(opc))
      cc += sprintf(buffer + cc, " LAB%d", WN_offset(wn));
    if (opr == OPR_INTRINSIC_OP || opr == OPR_INTRINSIC_CALL) {
      INTRINSIC        i = (INTRINSIC) WN_intrinsic(wn);
      if (i >= INTRINSIC_FIRST && i <= INTRINSIC_LAST)
        cc += sprintf(buffer + cc, "<%s>", INTRINSIC_name(i));
      else
        cc += sprintf(buffer + cc, "<bad intr #=%d>", i);
    }
    else if (opr == OPR_IO)
      cc += sprintf(buffer + cc, "<io=%d>", WN_io_statement(wn));
    else if (opr == OPR_IO_ITEM)
      cc += sprintf(buffer + cc, "<io item=%d>", WN_io_item(wn));
  }
  return cc; 
}

//-----------------------------------------------------------------------
// NAME: Is_Leaf 
// FUNCTION: Return TRUE if the node is a leaf node, FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Is_Leaf(WN* wn) 
{ 
  switch (WN_operator(wn)) { 
  case OPR_INTCONST: 
  case OPR_LDID: 
    return TRUE; 
  default: 
    return FALSE; 
  } 
} 

//-----------------------------------------------------------------------
// NAME: dump_whirl_expr 
// FUNCTION: Write a short, but informative description of the tree rooted
//   at 'wn' which is part of the larger tree rooted at 'wn_root'.  Place
//   the description in the 'buffer' starting at 'buffer[cc]'.  Return 
//   the value of first free index in 'buffer'.  
//-----------------------------------------------------------------------

static INT dump_whirl_expr(WN* wn_root, 
                       WN* wn, 
		       char* buffer, 
		       INT cc)
{
  if (wn != wn_root && !Is_Leaf(wn)) 
    cc += sprintf(buffer + cc, "(");
  switch (WN_operator(wn)) { 
  case OPR_ADD: 
  case OPR_SUB: 
  case OPR_MPY: 
  case OPR_DIV:  
    {
      for (INT i = 0; i < WN_kid_count(wn); i++) { 
        cc = dump_whirl_expr(wn_root, WN_kid(wn, i), buffer, cc);
        if (i < WN_kid_count(wn) - 1) 
          cc = dump_whirl_node(wn, buffer, cc); 
      }
    }
    break;
  case OPR_ARRAY: 
    {
      cc = dump_whirl_node(WN_array_base(wn), buffer, cc);
      cc += sprintf(buffer + cc, "[");  
      for (INT i = 0; i < WN_num_dim(wn); i++) { 
        cc = dump_whirl_expr(WN_array_index(wn, i), WN_array_index(wn, i), 
          buffer, cc);  
        if (i < WN_num_dim(wn) - 1)
          cc += sprintf(buffer + cc, ","); 
      }
      cc += sprintf(buffer + cc, "]");  
    }
    break;
  case OPR_LDID:
  case OPR_INTCONST:
  case OPR_CONST:   
    cc = dump_whirl_node(wn, buffer, cc);  
    break; 
  case OPR_CALL: 
  case OPR_INTRINSIC_CALL:
  case OPR_ICALL:
  case OPR_PICCALL:
    cc += sprintf(buffer + cc, "<CALL NODE>"); 
    break; 
  default: 
    {
      cc = dump_whirl_node(wn, buffer, cc);
      cc += sprintf(buffer + cc, "("); 
      for (INT i = 0; i < WN_kid_count(wn); i++) { 
        cc = dump_whirl_expr(wn_root, WN_kid(wn, i), buffer, cc);
        if (i < WN_kid_count(wn) - 1) 
	  cc += sprintf(buffer + cc, ","); 
      } 
      cc += sprintf(buffer + cc, ")"); 
    }
    break;   
  }   
  if (wn != wn_root && !Is_Leaf(wn)) 
    cc += sprintf(buffer + cc, ")"); 
  return cc; 
} 

//-----------------------------------------------------------------------
// NAME: compact_buffer 
// FUNCTION: Compact 'buffer' by replacing strings like '+-' with '-' 
//   and '--' with '+'.  
//-----------------------------------------------------------------------

static void compact_buffer(char* buffer) 
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
// NAME: WB_Dep_Symbol 
// FUNCTION: Write to 'stdout' a representation of the symbol for 'wn'. 
//-----------------------------------------------------------------------

extern BOOL WB_Dep_Symbol(WN* wn, 
			  char buffer[],
			  INT max_string) 
{
  FmtAssert(max_string >= strlen("Expression too long!") + 1, 
   ("WB_Dep_Symbol: Too short for error message")); 
  WN* symbol_node = NULL;  
  switch (WN_operator(wn)) { 
  case OPR_ISTORE:
    symbol_node = WN_kid1(wn); 
    break; 
  case OPR_ILOAD: 
    symbol_node = WN_kid0(wn); 
    break; 
  case OPR_CALL: 
  case OPR_INTRINSIC_CALL:
  case OPR_ICALL:
  case OPR_PICCALL:
    symbol_node = wn; 
    break; 
  }
  if (symbol_node == NULL) {
    sprintf(buffer, " ");
    return TRUE;
  } 
  INT cc = dump_whirl_expr(symbol_node, symbol_node, buffer, 0);
  if (cc > max_string) {
    sprintf(buffer, "Expression too long!");
    return FALSE; 
  } 
  compact_buffer(buffer);  
  return TRUE; 
}

static void WB_Dep_Symbol(WN* wn)
{ 
  char buffer[MAX_STRING_LENGTH]; 
  BOOL fits = WB_Dep_Symbol(wn, buffer, MAX_STRING_LENGTH - 1); 
  if (!fits) 
    bell(); 
  fprintf(stdout, "%s", buffer); 
} 

//-----------------------------------------------------------------------
// WHIRL BROWSER COMMANDS 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: WB_root 
// WHIRL BROWSER COMMAND: 'R'
// FUNCTION: Go to the root of the program unit 
//-----------------------------------------------------------------------

static void WB_root()
{ 
  cnode = WB_global_fd;
  print_this_node(); 
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
// NAME: WB_parent 
// WHIRL BROWSER COMMAND: 'E'
// FUNCTION: Go to the parent of the current node 
//-----------------------------------------------------------------------

static void WB_parent()      
{
  if (cnode == WB_global_fd) { 
    Error_Cleanup(); 
    return; 
  } 
  extern WN_MAP Parent_Map; 
  if (Parent_Map <= 0) { 
    MEM_POOL_Push(&MEM_local_pool); 
    STACK<WN*> stk_parent(&MEM_local_pool); 
    BOOL found_path = WB_Parent_Search(WB_global_fd, &stk_parent, cnode);
    if (!found_path) { 
      Error_Cleanup(); 
      MEM_POOL_Pop(&MEM_local_pool); 
      return; 
    } 
    cnode = stk_parent.Bottom_nth(stk_parent.Elements() - 2); 
    print_this_node(); 
    MEM_POOL_Pop(&MEM_local_pool); 
  } else {  
    WN* pnode = LWN_Get_Parent(cnode);  
    if (pnode == NULL) { 
      Error_Cleanup();     
      return; 
    } 
    cnode = pnode; 
    print_this_node();
  } 
} 

//-----------------------------------------------------------------------
// NAME: WB_next 
// WHIRL BROWSER COMMAND: 'N'
// FUNCTION: Go to the next node in the subtree chain 
//-----------------------------------------------------------------------

static void WB_next()
{ 
  WN* pnode = WN_next(cnode);  
  if (pnode == NULL) { 
    Error_Cleanup();  
    return; 
  } 
  cnode = pnode; 
  print_this_node();
} 

//-----------------------------------------------------------------------
// NAME: WB_previous 
// WHIRL BROWSER COMMAND: 'P'
// FUNCTION: Go to the previous node in the subtree chain 
//-----------------------------------------------------------------------

static void WB_previous()
{ 
  WN* pnode = WN_prev(cnode);  
  if (pnode == NULL)  {
    Error_Cleanup();  
    return; 
  } 
  cnode = pnode; 
  print_this_node();
} 

//-----------------------------------------------------------------------
// NAME: WB_set_node
// WHIRL BROWSER COMMAND: '='
// FUNCTION: Set the current node to the following address 
//-----------------------------------------------------------------------

static void WB_set_node() 
{
  WN* node;
  scan_blanks_and_tabs();  
  (void) sscanf(buffer + buffer_start, "0x%p", &node);
  buffer_start += 2;
  skip_to_separator(SKIP_HEX); 
  cnode = node; 
  print_this_node(); 
}

//-----------------------------------------------------------------------
// NAME: WB_vset_node
// WHIRL BROWSER COMMAND: '='
// FUNCTION: Set the current node to the following address 
//-----------------------------------------------------------------------

static void WB_vset_node() 
{
  WN* node;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  INT vertex_number = 0; 
  scan_blanks_and_tabs();  
  (void) sscanf(buffer + buffer_start, "%d", &vertex_number);
  buffer_start += 2;
  skip_to_separator(SKIP_NUMERIC); 
  VINDEX16 v;
  for (v = dg->Get_Vertex(); v != 0; v = dg->Get_Next_Vertex(v)) 
    if (v == (VINDEX16) vertex_number)
      break; 
  if (v == 0) {
    Error_Cleanup(); 
    return;
  } 
  cnode = dg->Get_Wn((VINDEX16) vertex_number); 
  print_this_node(); 
}

//-----------------------------------------------------------------------
// NAME: WB_davinci
// WHIRL BROWSER COMMAND: 'd'
// FUNCTION: Toggle Da Vinci mode
//-----------------------------------------------------------------------

static void WB_davinci()
{
  if (WB_davinci_mode) { 
    WB_davinci_mode = FALSE; 
    fprintf(stdout, "DAVINCI is OFF.\n");
  } else {
    WB_davinci_mode = TRUE; 
    fprintf(stdout, "DAVINCI is ON.\n");
  }
} 

//-----------------------------------------------------------------------
// NAME: WB_Set_Map_Id
// WHIRL BROWSER COMMAND: 'M'
// FUNCTION: Set the current node to node with this map id
//-----------------------------------------------------------------------

static void WB_Set_Map_Id() 
{
  INT map_id;
  scan_blanks_and_tabs();  
  (void) sscanf(buffer + buffer_start, "%d", &map_id);
  skip_to_separator(SKIP_NUMERIC); 
  LWN_ITER* itr = LWN_WALK_TreeIter(WB_global_fd);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) 
    if (WN_map_id(itr->wn) == map_id)
      break;
  if (itr == NULL) {
    Error_Cleanup(); 
    return;
  }
  cnode = itr->wn; 
  print_this_node(); 
}

//-----------------------------------------------------------------------
// NAME: WB_Map_Id
// WHIRL BROWSER COMMAND: 'm'
// FUNCTION: Print the map-id at this node
//-----------------------------------------------------------------------

static void WB_Map_Id()
{
  fprintf(stdout, "%d\n", WN_map_id(cnode));
}

//-----------------------------------------------------------------------
// NAME: WB_private
// WHIRL BROWSER COMMAND: 'p'
// FUNCTION: Print the privatization info at this node 
//-----------------------------------------------------------------------

static void WB_private()
{
  if (cnode == NULL) {
    Error_Cleanup();
    return;
  }
  switch (WN_opcode(cnode)) {
  case OPC_DO_LOOP: 
    {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(cnode, TRUE);
      if (dli != NULL && dli->ARA_Info != NULL) 
        dli->ARA_Info->WB_Print(stdout);  
      else 
        Error_Cleanup();
    }
    return; 
  default: 
    return;
  }
}

//-----------------------------------------------------------------------
// NAME: WB_address 
// WHIRL BROWSER COMMAND: '@'
// FUNCTION: Set the current node to this numbered node 
//-----------------------------------------------------------------------

static void WB_address() 
{
  INT integer; 
  scan_blanks_and_tabs(); 
  sscanf(buffer + buffer_start, "%d", &integer); 
  skip_to_separator(SKIP_NUMERIC);   
  if (integer < 0 || integer >= carray_max) { 
    Error_Cleanup(); 
    return;  
  } 
  cnode = carray[integer]; 
  print_this_node();  
} 

//-----------------------------------------------------------------------
// NAME: Address_Walk
// FUNCTION: Walk the tree rooted at 'wn_tree', printing the adddresses 
//   of all of the nodes in the subtree, with a short description of each
//   node.  
//-----------------------------------------------------------------------

static void Address_Walk(WN* wn_tree, 
   			 INT spaces, 
			 INT increment)
{  
  for (INT i = 0; i < spaces; i++) 
    fprintf(stdout, " "); 
  print_this_node(wn_tree, FALSE, TRUE); 

  if (WN_opcode(wn_tree) == OPC_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))  
      Address_Walk(wn, spaces + increment, increment); 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)  
      Address_Walk(WN_kid(wn_tree, i), spaces + increment, increment); 
  } 
} 

//-----------------------------------------------------------------------
// NAME: WB_addresses
// WHIRL BROWSER COMMAND: '%'
// FUNCTION: Print the addresses of nodes in this subtree
//-----------------------------------------------------------------------

static void WB_addresses()
{
  Address_Walk(cnode, 0, 2); 
} 

//-----------------------------------------------------------------------
// NAME: WB_fancyup
// WHIRL BROWSER COMMAND: '>'
// FUNCTION: Increase fanciness level for printing
//-----------------------------------------------------------------------

static void WB_fancyup()
{
  if (fancy == fancy_max) { 
    Error_Cleanup(); 
    return; 
  }
  fancy++;
} 

//-----------------------------------------------------------------------
// NAME: WB_fancydown
// WHIRL BROWSER COMMAND: '<'
// FUNCTION: Decrease fanciness level for printing
//-----------------------------------------------------------------------

static void WB_fancydown()
{
  if (fancy == fancy_min) { 
    Error_Cleanup(); 
    return; 
  }
  fancy--;
} 

//-----------------------------------------------------------------------
// NAME: WB_whirl2fc 
// WHIRL BROWSER COMMAND: 'W'
// FUNCTION: Print tree in whirl2[fc] format at current node 
//-----------------------------------------------------------------------

static void WB_whirl2fc()
{
  if (WB_language == SRC_NONE) {
    Whirl2Src_Init(WB_global_fd); 
    Whirl2Src_Emit(stdout, cnode); 
    fprintf(stdout, "\n"); 
  } else if (WB_language == SRC_FORTRAN) {
    Whirl2F_Init(WB_global_fd);
    Whirl2F_Emit(stdout, cnode); 
    fprintf(stdout, "\n"); 
  } else if (WB_language == SRC_C) {
    Whirl2C_Init(WB_global_fd); 
    Whirl2C_Emit(stdout, cnode); 
    fprintf(stdout, "\n"); 
  }
}

//-----------------------------------------------------------------------
// NAME: WB_kids 
// WHIRL BROWSER COMMAND: 'K'
// FUNCTION: Print the kids of this node
//-----------------------------------------------------------------------

static void WB_kids()
{
  if (WN_kid_count(cnode) == 0) { 
    Error_Cleanup(); 
    return; 
  }  
  for (INT i = 0; i < WN_kid_count(cnode); i++) { 
    WN* kid = WN_kid(cnode, i); 
    fprintf(stdout, "[%d] ", i);
    print_this_node(kid); 
    if (i < MAX_SAVED_NODES) 
      carray[i] = kid; 
  }
  carray_max = WN_kid_count(cnode); 
} 

//-----------------------------------------------------------------------
// NAME: WB_statements
// WHIRL BROWSER COMMAND: 'S'
// FUNCTION: Print the list of statements under this node
//-----------------------------------------------------------------------

static void WB_statements()
{
  INT i = 0;
  if (WN_opcode(cnode) != OPC_BLOCK) {
    Error_Cleanup(); 
    return; 
  }  
  if (WN_first(cnode) == NULL) { 
    Error_Cleanup(); 
    return; 
  }  
  for (WN* node = WN_first(cnode); node != NULL; i++, node = WN_next(node)) { 
    fprintf(stdout, "[%d] ", i);
    print_this_node(node);
    if (i < MAX_SAVED_NODES) 
      carray[i] = node;  
  }
  carray_max = i; 
} 

//-----------------------------------------------------------------------
// NAME: WB_this_tree
// WHIRL BROWSER COMMAND: 'T'
// FUNCTION: Print the tree at the current node
//-----------------------------------------------------------------------

static void WB_this_tree()
{
  if (cnode == NULL) { 
    Error_Cleanup(); 
    return; 
  }  
  fdump_tree(stdout, cnode); 
} 

//-----------------------------------------------------------------------
// NAME: WB_uses
// WHIRL BROWSER COMMAND: 'U'
// FUNCTION: Print the uses at the current node
//-----------------------------------------------------------------------

static void WB_uses_loop()
{
  if (WB_du_mgr == NULL) {
    Error_Cleanup();
    return;
  }
  ARRAY_DIRECTED_GRAPH16 *dg;
  dg = Array_Dependence_Graph;
  if (dg == NULL) {
    Error_Cleanup();
    return;
  }
  INT index = 0;
  WN* start_node = NULL;
  switch (WN_opcode(cnode)) {
  case OPC_DO_LOOP:
    start_node = WN_do_body(cnode);
    break;
  case OPC_DO_WHILE:
  case OPC_WHILE_DO:
    start_node = WN_while_body(cnode);
    break;
  }
  LWN_ITER* itr = LWN_WALK_TreeIter(start_node);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    USE_LIST *use_list = WB_du_mgr->Du_Get_Use(wn);
    if (use_list == NULL) 
      continue;  
    enter_this_node_unique(index, wn); 
    WB_Dep_Symbol(wn); 
    fprintf(stdout, "\n");
    if (use_list->Incomplete())
      fprintf(stdout, "    WARNING: USE LIST INCOMPLETE\n");
    USE_LIST_ITER iter(use_list);
    const DU_NODE* node = NULL;
    INT i = carray_max;
    for (node = iter.First(); !iter.Is_Empty(); i++, node = iter.Next()) {
      WN* use = node->Wn();
      fprintf(stdout, "    "); 
      enter_this_node_unique(index, use);
      fprintf(stdout, "\n");
    }
    carray_max = i;
  }
}

static void WB_uses_ref()
{ 
  if (WB_du_mgr == NULL) { 
    Error_Cleanup(); 
    return; 
  }  
  USE_LIST *use_list = WB_du_mgr->Du_Get_Use(cnode);
  if (use_list == NULL) {
    Error_Cleanup();  
    return; 
  } 
  if (use_list->Incomplete())
    fprintf(stdout, "WARNING: USE LIST INCOMPLETE\n"); 
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = NULL;
  INT i = 0; 
  for (node = iter.First(); !iter.Is_Empty(); i++, node = iter.Next()) {
    WN* use = node->Wn();
    fprintf(stdout, "[%d] ", i);
    print_this_node(use); 
    if (i < MAX_SAVED_NODES) 
      carray[i] = use; 
  } 
  carray_max = i;  
} 
 
static void WB_uses()
{
  switch (WN_opcode(cnode)) {
  case OPC_DO_LOOP:
  case OPC_DO_WHILE:
  case OPC_WHILE_DO:
    WB_uses_loop();
    break;
  default:
    WB_uses_ref();
    break;
  }
}

//-----------------------------------------------------------------------
// NAME: WB_defs
// WHIRL BROWSER COMMAND: 'D'
// FUNCTION: Print the definitions at the current node
//-----------------------------------------------------------------------

static void WB_defs_loop()
{
  if (WB_du_mgr == NULL) {
    Error_Cleanup();
    return;
  }
  ARRAY_DIRECTED_GRAPH16 *dg;
  dg = Array_Dependence_Graph;
  if (dg == NULL) {
    Error_Cleanup();
    return;
  }
  INT index = 0;
  WN* start_node = NULL;
  switch (WN_opcode(cnode)) {
  case OPC_DO_LOOP:
    start_node = WN_do_body(cnode);
    break;
  case OPC_DO_WHILE:
  case OPC_WHILE_DO:
    start_node = WN_while_body(cnode);
    break;
  }
  LWN_ITER* itr = LWN_WALK_TreeIter(start_node);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    DEF_LIST *def_list = WB_du_mgr->Ud_Get_Def(wn);
    if (def_list == NULL) 
      continue;  
    enter_this_node_unique(index, wn); 
    WB_Dep_Symbol(wn); 
    fprintf(stdout, "\n");
    if (def_list->Incomplete())
      fprintf(stdout, "    WARNING: DEF LIST INCOMPLETE\n");
    fprintf(stdout, "    Loop Statement: 0x%p\n", def_list->Loop_stmt());
    DEF_LIST_ITER iter(def_list);
    const DU_NODE* node = NULL;
    INT i = carray_max;
    for (node = iter.First(); !iter.Is_Empty(); i++, node = iter.Next()) {
      WN* def = node->Wn();
      fprintf(stdout, "    "); 
      enter_this_node_unique(index, def);
      fprintf(stdout, "\n");
    }
    carray_max = i;
  }
}

static void WB_defs_ref()
{
  if (WB_du_mgr == NULL) { 
    Error_Cleanup(); 
    return; 
  }  
  DEF_LIST* def_list = WB_du_mgr->Ud_Get_Def(cnode);
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
  for (node = iter.First(); !iter.Is_Empty(); i++, node = iter.Next()) {
    WN* def = node->Wn();
    fprintf(stdout, "[%d] ", i);
    print_this_node(def); 
    if (i < MAX_SAVED_NODES) 
      carray[i] = def; 
  }
  carray_max = i; 
}  

static void WB_defs()
{
  switch (WN_opcode(cnode)) {
  case OPC_DO_LOOP:
  case OPC_DO_WHILE:
  case OPC_WHILE_DO:
    WB_defs_loop();
    break;
  default:
    WB_defs_ref();
    break;
  }
}

//-----------------------------------------------------------------------
// NAME: WB_access
// WHIRL BROWSER COMMAND: 'A'
// FUNCTION: Print the access info at the current node
//-----------------------------------------------------------------------

static void WB_access()  
{ 
  OPCODE opc = WN_opcode(cnode);
  OPERATOR opr = OPCODE_operator(opc);  
  if (opc == OPC_DO_LOOP || opc == OPC_IF || opr == OPR_REGION 
      || opr == OPR_CALL || opr == OPR_ARRAY) {
    LNO_Print_One_Access(stdout, cnode);
    if (opr == OPR_DO_LOOP && Get_Do_Loop_Info(cnode, TRUE) != NULL) {
      WN* wn_outer_tile = Outer_Tile(cnode, WB_du_mgr); 
      fprintf(stdout, "Outer Tile is 0x%p\n", wn_outer_tile); 
    }
  } else {
    Error_Cleanup(); 
  }  
} 

//-----------------------------------------------------------------------
// NAME: WB_RR_Map
// WHIRL BROWSER COMMAND: 'Y'
// FUNCTION: Print the reshaped-reference map at the current node
//-----------------------------------------------------------------------

static void WB_RR_Map()  
{ 
  OPCODE opc = WN_opcode(cnode);
  OPERATOR opr = OPCODE_operator(opc);  
  if (opr != OPR_ARRAY) {
    Error_Cleanup(); 
  } else {
    RR_INFO* rri = Get_RR_Map ((WN*) cnode);
    if (rri) rri->Print(stdout);
    else fprintf (stdout, "RR_Map is NULL\n");
  }
} 

//-----------------------------------------------------------------------
// NAME: WB_node_deps
// WHIRL BROWSER COMMAND: 'G'
// FUNCTION: Print the node dep graph info at the current node
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: WB_stmt_deps
// WHIRL BROWSER COMMAND: 'g'
// FUNCTION: Print the statement dep graph info at the current node
//-----------------------------------------------------------------------

static void WB_DIRECTION_Print(DIRECTION dir, FILE *fp)
{
  switch (dir) {
    case DIR_POS: fprintf(fp, "+"); break;
    case DIR_NEG: fprintf(fp, "-"); break;
    case DIR_EQ: fprintf(fp, "="); break;
    case DIR_POSNEG: fprintf(fp, "+-"); break;
    case DIR_POSEQ: fprintf(fp, "+="); break;
    case DIR_NEGEQ: fprintf(fp, "-="); break;
    case DIR_STAR: fprintf(fp, "*"); break;
    default: Is_True(0,("Illegal direction in WB_DIRECTION_Print"));
  };
}

static void WB_DEP_Print(const DEP dep,FILE *fp)
{
  if (DEP_IsDistance(dep)) 
    fprintf(fp,"%d", DEP_Distance(dep));
  else 
    WB_DIRECTION_Print(DEP_Direction(dep), fp);
}

static void WB_DEPV_Print(const DEPV *depv, FILE *fp, UINT8 num_dim)
{
  INT i;

  fprintf(fp, "(");
  for (i = 0; i < num_dim; i++) {
    WB_DEP_Print(DEPV_Dep(depv, i), fp);
    if (i < num_dim - 1) 
      fprintf(fp, ","); 
  }
  fprintf(fp, ")");
}

void DEPV_ARRAY_Print(const DEPV_ARRAY* depv_array, FILE *fp) 
{
  for (INT i = 0; i < depv_array->Num_Vec(); i++) {
    WB_DEPV_Print(depv_array->Depv(i), fp, depv_array->Num_Dim());
    if (i < depv_array->Num_Vec() - 1) 
      fprintf(fp, " ");
  } 
  fprintf(fp, "\n");
}

static BOOL WB_TXT_deps_loop(ARRAY_DIRECTED_GRAPH16* dg,
			     BOOL print_arrays)
{
  if (dg == NULL) {
    Error_Cleanup(); 
    return FALSE; 
  } 
  INT index = 0;  
  WN* start_node = NULL; 
  switch (WN_opcode(cnode)) { 
  case OPC_DO_LOOP: 
    start_node = WN_do_body(cnode);
    break;   
  case OPC_DO_WHILE: 
  case OPC_WHILE_DO:
    start_node = WN_while_body(cnode); 
    break; 
  }  
  LWN_ITER* itr = LWN_WALK_TreeIter(start_node);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) { 
    WN* wn = itr->wn; 
    VINDEX16 v = dg->Get_Vertex(wn); 
    if (v == 0) 
      continue;
    enter_this_node_unique(index, wn, FALSE);
    fprintf(stdout, "V#%d ", v); 
    WB_Dep_Symbol(wn); 
    fprintf(stdout, "\n");  
    EINDEX16 e; 
    if (dg->Get_In_Edge(v)) { 
      fprintf(stdout, "    ");     
      fprintf(stdout, "IN EDGES:\n"); 
      for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {  
        fprintf(stdout, "    ");     
        enter_this_node_unique(index, dg->Get_Wn(dg->Get_Source(e)), FALSE);
	fprintf(stdout, "V#%d ", dg->Get_Source(e));
   	fprintf(stdout, "E#%d ", e);
        WB_Dep_Symbol(dg->Get_Wn(dg->Get_Source(e)));
	fprintf(stdout, " "); 
	if (print_arrays)    
          DEPV_ARRAY_Print(dg->Depv_Array(e), stdout);   
        else 
	  fprintf(stdout, "\n"); 
      } 
    } 
    if (dg->Get_Out_Edge(v)) {  
      fprintf(stdout, "    ");     
      fprintf(stdout, "OUT EDGES:\n"); 
      for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {   
        fprintf(stdout, "    ");     
        enter_this_node_unique(index, dg->Get_Wn(dg->Get_Sink(e)), FALSE); 
	fprintf(stdout, "V#%d ", dg->Get_Sink(e));
   	fprintf(stdout, "E#%d ", e);
        WB_Dep_Symbol(dg->Get_Wn(dg->Get_Sink(e)));     
	fprintf(stdout, " ");    
	if (print_arrays)    
          DEPV_ARRAY_Print(dg->Depv_Array(e), stdout);   
        else 
	  fprintf(stdout, "\n"); 
      } 
    } 
  } 
  carray_max = index;
  return TRUE; 
} 

// ---: Begin daVinci support for drawing dep graph. ---

static DaVinci *DV_deps              = NULL;
static BOOL     DV_deps_mempool_init = FALSE;
static MEM_POOL DV_deps_mempool;

class DV_DEPS_CALLBACK : public DaVinci_Callback {
public:
  virtual void Node_Select(const INT n_ids, const NODE_ID id_array[]);
  virtual void Edge_Select(const EDGE_ID& edge_id);
  virtual void Menu_Select(const char *menu_id);
};

void DV_DEPS_CALLBACK::Node_Select(const INT n_ids, 
				   const NODE_ID id_array[])
{
  ARRAY_DIRECTED_GRAPH16 *dg = Array_Dependence_Graph;
  for (INT i = 0; i < n_ids; ++i) {
    VINDEX16 v = VINDEX16(INTPTR(id_array[i]));
    WN *wn = dg->Get_Wn(v);
    fprintf(stdout, "V ");
    this_node(wn);
    fprintf(stdout, "V#%d ", v);
    WB_Dep_Symbol(wn);
    fprintf(stdout, "\n");
  }
}

static ARRAY_DIRECTED_GRAPH16* Current_Dependence_Graph = NULL; 

void DV_DEPS_CALLBACK::Edge_Select(const EDGE_ID& edge_id) 
{
  ARRAY_DIRECTED_GRAPH16 *dg = Current_Dependence_Graph;
  VINDEX16 v_src = VINDEX16(INTPTR(edge_id.src));
  VINDEX16 v_dst = VINDEX16(INTPTR(edge_id.dst));
  EINDEX16 e;

  for (e = dg->Get_Out_Edge(v_src); e; e = dg->Get_Next_Out_Edge(e)) 
    if (dg->Get_Sink(e) == v_dst) 
      break;
  if (!e) {
    fprintf(stdout, "ERROR: Array_Dependence_Graph edge %d -> %d not found.\n",
      v_src, v_dst);
    return;
  }
  fprintf(stdout, "E ");
  this_node(dg->Get_Wn(v_src));
  fprintf(stdout, "V#%d ", dg->Get_Source(e));
  fprintf(stdout, "E#%d ", e);
  WB_Dep_Symbol(dg->Get_Wn(dg->Get_Source(e)));
  fprintf(stdout, "\n");
  fprintf(stdout, "  ");
  this_node(dg->Get_Wn(v_dst));
  fprintf(stdout, "V#%d ", dg->Get_Source(e));
  fprintf(stdout, "E#%d ", e);
  WB_Dep_Symbol(dg->Get_Wn(dg->Get_Source(e)));
  fprintf(stdout, "\n");
  fprintf(stdout, "  ");
  if (Current_Dependence_Graph == Array_Dependence_Graph) 
    DEPV_ARRAY_Print(dg->Depv_Array(e), stdout);
   else 
    fprintf(stdout, "\n"); 
}

void DV_DEPS_CALLBACK::Menu_Select(const char *menu_id)
{
  fprintf(stdout, "DV_DEPS_CALLBACK::Menu_Select(%s)\n", menu_id); // more ..
}

static void WB_DAV_draw_deps_loop(ARRAY_DIRECTED_GRAPH16* dg,
				  BOOL print_arrays)
{
  NODE_TYPE nt_box_pink;
  nt_box_pink.Shape(NS_BOX);
  nt_box_pink.Color("pink");
  NODE_TYPE nt_box_green;
  nt_box_green.Shape(NS_BOX);
  nt_box_green.Color("green");
  NODE_TYPE nt_box_blue;
  nt_box_blue.Shape(NS_BOX);
  nt_box_blue.Color("LightBlue");
  NODE_TYPE nt_circle_pink; 
  nt_circle_pink.Shape(NS_CIRCLE);
  nt_circle_pink.Color("pink");
  NODE_TYPE nt_circle_green; 
  nt_circle_green.Shape(NS_CIRCLE);
  nt_circle_green.Color("green");
  NODE_TYPE nt_circle_blue; 
  nt_circle_blue.Shape(NS_CIRCLE);
  nt_circle_blue.Color("LightBlue");
  EDGE_TYPE et;
  BOOL did_loops = WB_TXT_deps_loop(dg, print_arrays);
  if (!did_loops)
    return; 
  DV_deps->Graph_Begin();
  for (INT i = 0; i < carray_max; i++) { 
    char vnum_buffer[BUFFER_MAX];
    WN* wn = carray[i]; 
    VINDEX16 v = dg->Get_Vertex(wn);
    sprintf(vnum_buffer, "%d", v);
    NODE_TYPE nt_local;
    if (Wn_Is_Inside(wn, cnode)) {
      if (OPCODE_is_load(WN_opcode(wn))) { 
	nt_local = nt_circle_green;
      } else if (OPCODE_is_store(WN_opcode(wn))) {
	nt_local = nt_circle_pink;
      } else { 
 	nt_local = nt_circle_blue;
      } 
    } else { 
      if (OPCODE_is_load(WN_opcode(wn))) { 
	nt_local = nt_box_green;
      } else if (OPCODE_is_store(WN_opcode(wn))) {
	nt_local = nt_box_pink;
      } else { 
 	nt_local = nt_box_blue;
      } 
    }  
    DV_deps->Node_Begin(NODE_ID(INTPTR(v)), vnum_buffer, nt_local);
    for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = dg->Get_Next_Out_Edge(e)) {
      VINDEX16 v2 = dg->Get_Sink(e);
      DV_deps->Out_Edge(EDGE_ID(NODE_ID(INTPTR(v)), NODE_ID(INTPTR(v2))),
			et, NODE_ID(INTPTR(v2)));
    }
    DV_deps->Node_End();
  }
  DV_deps->Graph_End();
} 

static void WB_DAV_deps_loop(ARRAY_DIRECTED_GRAPH16* dg,
			     BOOL print_arrays)
{
  if (!DaVinci::enabled(true)) 
    return;

  if (!DV_deps_mempool_init) {
    MEM_POOL_Initialize(&DV_deps_mempool, "DV_deps_mempool", FALSE);
    DV_deps_mempool_init = TRUE;
  } 
  FILE* fp_dv = NULL;
  DV_deps = CXX_NEW(DaVinci(&DV_deps_mempool, fp_dv), &DV_deps_mempool);
  if (DV_deps->is_ok()) {
    char buffer[BUFFER_MAX];
    char* program_name = ST_name(WN_st(WB_global_fd));
    sprintf(buffer, "Dependence Graph for %s: Loop 0x%p", 
      program_name, cnode);
    DV_deps->Title(buffer);
    WB_DAV_draw_deps_loop(dg, print_arrays);
    DV_DEPS_CALLBACK DV_deps_callback;
    DV_deps->Event_Loop(&DV_deps_callback);
  } else {
    fprintf(stdout, "Unable to start daVinci: Check path and DAVINCIHOME\n");
    Error_Cleanup();
  }  
  CXX_DELETE(DV_deps, &DV_deps_mempool);
  DV_deps = NULL;
} 

static INT WB_TXT_deps_ref(ARRAY_DIRECTED_GRAPH16* dg,
			   BOOL print_arrays) 
{
  if (dg == NULL) { 
    Error_Cleanup(); 
    return -1; 
  }  
  VINDEX16 v = dg->Get_Vertex(cnode); 
  if (v == 0) { 
    Error_Cleanup(); 
    return -1; 
  }
  EINDEX16 e; 
  INT index = 0;
  if (!dg->Get_In_Edge(v) && !dg->Get_Out_Edge(v))
    fprintf(stdout, "V#%d\n", v); 
  if (dg->Get_In_Edge(v)) {  
    fprintf(stdout, "V#%d ", v); 
    fprintf(stdout, "IN EDGES:\n"); 
    for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) { 
      enter_this_node(index, dg->Get_Wn(dg->Get_Source(e)), FALSE); 
      fprintf(stdout, "V#%d ", dg->Get_Source(e));
      fprintf(stdout, "E#%d ", e);
      WB_Dep_Symbol(dg->Get_Wn(dg->Get_Source(e)));
      fprintf(stdout, " "); 
      if (print_arrays)   
        DEPV_ARRAY_Print(dg->Depv_Array(e), stdout);   
      else 
	fprintf(stdout, "\n");
     }
  } 
  INT in_index_count = index;  
  if (dg->Get_Out_Edge(v)) {  
    fprintf(stdout, "V#%d ", v); 
    fprintf(stdout, "OUT EDGES:\n"); 
    for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) { 
      enter_this_node(index, dg->Get_Wn(dg->Get_Sink(e)), FALSE); 
      fprintf(stdout, "V#%d ", dg->Get_Sink(e));
      fprintf(stdout, "E#%d ", e);
      WB_Dep_Symbol(dg->Get_Wn(dg->Get_Source(e)));
      fprintf(stdout, " ");    
      if (print_arrays)   
        DEPV_ARRAY_Print(dg->Depv_Array(e), stdout);   
      else 
	fprintf(stdout, "\n");
    }
  } 
  carray_max = index;  
  return in_index_count; 
}   

static void WB_DAV_draw_deps_ref(ARRAY_DIRECTED_GRAPH16* dg,
				 BOOL print_arrays)
{
  const INT HT_SIZE = 131; 
  MEM_POOL_Push(&LNO_local_pool);
  typedef HASH_TABLE<VINDEX16, INT> HT_VINDEX;
  HT_VINDEX* ht = CXX_NEW(HT_VINDEX(HT_SIZE, &LNO_local_pool), 
    &LNO_local_pool);
  VINDEX16 v = dg->Get_Vertex(cnode); 
  INT in_index_count = WB_TXT_deps_ref(dg, print_arrays);
  if (in_index_count < 0)
    return; 
  NODE_TYPE nt_box_pink;
  nt_box_pink.Shape(NS_BOX);
  nt_box_pink.Color("pink");
  NODE_TYPE nt_box_green;
  nt_box_green.Shape(NS_BOX);
  nt_box_green.Color("green");
  NODE_TYPE nt_box_blue;
  nt_box_blue.Shape(NS_BOX);
  nt_box_blue.Color("LightBlue");
  NODE_TYPE nt_circle_pink; 
  nt_circle_pink.Shape(NS_CIRCLE);
  nt_circle_pink.Color("pink");
  NODE_TYPE nt_circle_green; 
  nt_circle_green.Shape(NS_CIRCLE);
  nt_circle_green.Color("green");
  NODE_TYPE nt_circle_blue; 
  nt_circle_blue.Shape(NS_CIRCLE);
  nt_circle_blue.Color("LightBlue");
  EDGE_TYPE et; 
  DV_deps->Graph_Begin();
  for (INT i = 0; i < in_index_count; i++) {
    char vnum_buffer[BUFFER_MAX];
    WN* wn = carray[i];
    VINDEX16 vl = dg->Get_Vertex(wn);
    sprintf(vnum_buffer, "%d", vl);
    NODE_TYPE nt_local;
    if (OPCODE_is_load(WN_opcode(wn))) {
      nt_local = nt_box_green;
    } else if (OPCODE_is_store(WN_opcode(wn))) {
      nt_local = nt_box_pink;
    } else { 
      nt_local = nt_box_blue;
    } 
    if (vl != v) { 
      ht->Enter(vl, 1);
      DV_deps->Node_Begin(NODE_ID(INTPTR(vl)), vnum_buffer, nt_local);
      DV_deps->Out_Edge(EDGE_ID(NODE_ID(INTPTR(vl)), NODE_ID(INTPTR(v))),
	et, NODE_ID(INTPTR(v)));
      DV_deps->Node_End();
    } 
  } 
  char vnum_buffer[BUFFER_MAX];
  sprintf(vnum_buffer, "%d", v);
  NODE_TYPE nt_local;
  if (OPCODE_is_load(WN_opcode(cnode))) {
    nt_local = nt_circle_green;
  } else if (OPCODE_is_store(WN_opcode(cnode))) {
    nt_local = nt_circle_pink;
  } else { 
    nt_local = nt_circle_blue;
  } 
  DV_deps->Node_Begin(NODE_ID(INTPTR(v)), vnum_buffer, nt_local);
  EINDEX16 e;
  for (e = dg->Get_Out_Edge(v); e; e = dg->Get_Next_Out_Edge(e)) {
    VINDEX16 v2 = dg->Get_Sink(e);
    DV_deps->Out_Edge(EDGE_ID(NODE_ID(INTPTR(v)), NODE_ID(
INTPTR(v2))),
      et, NODE_ID(INTPTR(v2)));
  } 
  DV_deps->Node_End();
  ht->Enter(v, 1);
  for (e = dg->Get_Out_Edge(v); e; e = dg->Get_Next_Out_Edge(e)) {
    VINDEX16 v2 = dg->Get_Sink(e);
    if (!ht->Find(v2)) {
      ht->Enter(v2, 1);
      WN* wn = dg->Get_Wn(v2);
      if (OPCODE_is_load(WN_opcode(wn))) {
	nt_local = nt_box_green;
      } else if (OPCODE_is_store(WN_opcode(wn))) {
	nt_local = nt_box_pink;
      } else { 
	nt_local = nt_box_blue;
      } 
      char vnum_buffer[BUFFER_MAX];
      sprintf(vnum_buffer, "%d", v2);
      DV_deps->Node_Begin(NODE_ID(INTPTR(v2)), vnum_buffer, nt_local);
      DV_deps->Node_End();
    }
  }
  const char* error_string = DV_deps->Graph_End();
  if (error_string != NULL)
    fprintf(stdout, "%s", error_string);
  MEM_POOL_Pop(&LNO_local_pool);
}

static void WB_DAV_deps_ref(ARRAY_DIRECTED_GRAPH16* dg,
			    BOOL print_arrays)
{
  if (!DV_deps_mempool_init) {
    MEM_POOL_Initialize(&DV_deps_mempool, "DV_deps_mempool", FALSE);
    DV_deps_mempool_init = TRUE;
  } 
  FILE* fp_dv = NULL;
  DV_deps = CXX_NEW(DaVinci(&DV_deps_mempool, fp_dv), &DV_deps_mempool);
  if (DV_deps->is_ok()) {
    char buffer[BUFFER_MAX];
    char* program_name = ST_name(WN_st(WB_global_fd));
    sprintf(buffer, "Dependence Graph for %s: Node 0x%p Vertex %d", 
      program_name, cnode, dg->Get_Vertex(cnode));
    DV_deps->Title(buffer);
    WB_DAV_draw_deps_ref(dg, print_arrays);
    DV_DEPS_CALLBACK DV_deps_callback;
    DV_deps->Event_Loop(&DV_deps_callback);
  } else {
    fprintf(stdout, "Unable to start daVinci: Check path and DAVINCIHOME\n");
    Error_Cleanup();
  }  
  CXX_DELETE(DV_deps, &DV_deps_mempool);
  DV_deps = NULL;
} 

static void WB_deps_loop(ARRAY_DIRECTED_GRAPH16* dg,
			 BOOL print_arrays)
{
  if (WB_davinci_mode)
    WB_DAV_deps_loop(dg, print_arrays);
  else 
    WB_TXT_deps_loop(dg, print_arrays);
} 

static void WB_deps_ref(ARRAY_DIRECTED_GRAPH16* dg,
			BOOL print_arrays)
{
  if (WB_davinci_mode)
    WB_DAV_deps_ref(dg, print_arrays);
  else 
    WB_TXT_deps_ref(dg, print_arrays);
} 

static void WB_deps(ARRAY_DIRECTED_GRAPH16* dg,
		    BOOL print_arrays)
{
  switch (WN_opcode(cnode)) { 
  case OPC_DO_LOOP:
  case OPC_DO_WHILE:
  case OPC_WHILE_DO:
    WB_deps_loop(dg, print_arrays);
    break; 
  default:
    WB_deps_ref(dg, print_arrays); 
    break; 
  } 
} 

static void WB_node_deps()
{
  Current_Dependence_Graph = Array_Dependence_Graph; 
  WB_deps(Array_Dependence_Graph, TRUE);
  Current_Dependence_Graph = NULL; 
} 

static void WB_stmt_deps()
{
  Current_Dependence_Graph = Statement_Dependence_Graph; 
  WB_deps(Statement_Dependence_Graph, FALSE);
  Current_Dependence_Graph = NULL; 
} 

//-----------------------------------------------------------------------
// NAME: WB_vertices
// WHIRL BROWSER COMMAND: 'V'
// FUNCTION: Print the vertices of the dep graph
//-----------------------------------------------------------------------

static void WB_vertices()
{ 
  ARRAY_DIRECTED_GRAPH16 *dg; 
  dg = Array_Dependence_Graph; 
  if (dg == NULL) { 
    Error_Cleanup(); 
    return;
  }  
  VINDEX16 v;
  for (v = dg->Get_Vertex(); v; v = dg->Get_Next_Vertex(v)) { 
    WN* wn = dg->Get_Wn(v); 
    fprintf(stdout, "V#%d ", (INT) v);
    this_node(wn, FALSE);
    WB_Dep_Symbol(wn); 
    fprintf(stdout, "\n");
  }
  for (v = dg->Get_Vertex(); v; v = dg->Get_Next_Vertex(v)) 
    for (VINDEX16 w = dg->Get_Next_Vertex(v); w; w = dg->Get_Next_Vertex(w))  
      if (dg->Get_Wn(v) == dg->Get_Wn(w)) 
        fprintf(stdout, "Vertices %d and %d are for the same node!\n", v, w);  
} 

//-----------------------------------------------------------------------
// NAME: WB_reduction
// WHIRL BROWSER COMMAND: 'r'
// FUNCTION: Print the reduction manager
//-----------------------------------------------------------------------

static INT reduction_count = 0; 

static void dump_reductions(WN* wn, REDUCTION_MANAGER* rm)
{
  if (WN_opcode(wn) == OPC_BLOCK) {  
    for (WN* wn_temp = WN_first(wn); wn_temp; wn_temp = WN_next(wn_temp)) 
      dump_reductions(wn_temp, rm);
  } else {
    if (OPCODE_is_load(WN_opcode(wn)) || OPCODE_is_store(WN_opcode(wn))) {
      REDUCTION_TYPE red_type = rm->Which_Reduction(wn); 
      switch (red_type) { 
      case RED_ADD:
	fprintf(stdout, "[%d] 0x%p RED_ADD ", reduction_count, wn);  
        break; 
      case RED_MPY:
	fprintf(stdout, "[%d] 0x%p RED_MPY ", reduction_count, wn);  
        break; 
      case RED_MIN:
	fprintf(stdout, "[%d] 0x%p RED_MIN ", reduction_count, wn);  
        break; 
      case RED_MAX:
	fprintf(stdout, "[%d] 0x%p RED_MAX ", reduction_count, wn);  
        break; 
      }
      switch (red_type) { 
      case RED_ADD:
      case RED_MPY:
      case RED_MIN:
      case RED_MAX:
	OPERATOR oper = WN_operator(wn); 
	if (oper == OPR_ILOAD || oper == OPR_ISTORE) {
	  WB_Dep_Symbol(wn); 
	  fprintf(stdout, "\n"); 
	} else { 
	  const char *name = WB_Whirl_Symbol(wn);
	  fprintf(stdout, "%s\n", name); 
	} 
	if (reduction_count < MAX_SAVED_NODES)
	  carray[reduction_count++] = wn;
        break;
      } 
    }
    for (INT i = 0; i < WN_kid_count(wn); i++) 
      dump_reductions(WN_kid(wn, i), rm);   
  }  
}

static void WB_reduction()
{
  REDUCTION_MANAGER *rm; 
  rm = red_manager;
  if (rm == NULL) { 
    Error_Cleanup(); 
    return; 
  }
  reduction_count = 0;  
  dump_reductions(cnode, rm); 
  carray_max = reduction_count;  
} 

//-----------------------------------------------------------------------
// NAME: WB_alias
// WHIRL BROWSER COMMAND: 'a'
// FUNCTION: Print the alias info for this node
//-----------------------------------------------------------------------

static INT alias_count = 0; 

static BOOL aliased_node(WN* wn)  
{
  OPCODE opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc); 
  return (OPCODE_is_load(opc) || OPCODE_is_store(opc) || opr == OPR_PARM)
    && WB_alias_mgr->Id(wn) != 0;
}

static void alias_walk(WN* wn_test, 
		       WN* wn_start, 
		       ALIAS_RESULT ar)
{
  if (aliased_node(wn_start)) {
    ALIAS_RESULT result = Aliased(WB_alias_mgr, wn_test, wn_start);
    switch (result) {
    case NOT_ALIASED:
      break;
    case POSSIBLY_ALIASED:
    case SAME_LOCATION:
      if (ar == result) {
	fprintf(stdout, "  [%d] ", alias_count);  
	print_this_node(wn_start); 
	if (alias_count < MAX_SAVED_NODES)
	  carray[alias_count++] = wn_start; 
      }
      break;
    }
  }
   
  if (WN_opcode(wn_start) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_start); wn != NULL; wn = WN_next(wn)) 
      alias_walk(wn_test, wn, ar); 
  } else {
    for (INT i = 0; i < WN_kid_count(wn_start); i++) 
      alias_walk(wn_test, WN_kid(wn_start, i), ar);
  } 
}

static void WB_alias() 
{
  if (!aliased_node(cnode)) {
    Error_Cleanup(); 
    return;
  }
  alias_count = 0;
  fprintf(stdout, "POSSIBLY ALIASED: \n");
  alias_walk(cnode, WB_global_fd, POSSIBLY_ALIASED); 
  if (alias_count == 0)
    fprintf(stdout, "  NO LOCATIONS\n");
  INT possible_aliases = alias_count;    
  fprintf(stdout, "SAME LOCATION: \n");
  alias_walk(cnode, WB_global_fd, SAME_LOCATION); 
  if (alias_count == possible_aliases) 
    fprintf(stdout, "  NO LOCATIONS\n");
  carray_max = alias_count; 
}

//-----------------------------------------------------------------------
// NAME: WB_ancestors
// WHIRL BROWSER COMMAND: 'e'
// FUNCTION: Print the ancestors of this node
//-----------------------------------------------------------------------

static void WB_ancestors()
{
  INT index = 0;
  if (cnode == WB_global_fd) { 
    Error_Cleanup();
    return; 
  } 
  extern WN_MAP Parent_Map; 
  if (Parent_Map <= 0) { 
    MEM_POOL_Push(&MEM_local_pool);
    STACK<WN*> stk_parent(&MEM_local_pool); 
    BOOL found_path = WB_Parent_Search(WB_global_fd, &stk_parent, cnode); 
    if (!found_path) { 
      Error_Cleanup();
      MEM_POOL_Pop(&MEM_local_pool);
      return;
    } 
    for (INT i = stk_parent.Elements() - 1; i >= 0; i--) { 
      enter_this_node(index, stk_parent.Bottom_nth(i)); 
      fprintf(stdout, "\n"); 
    } 
    carray_max = index; 
    MEM_POOL_Pop(&MEM_local_pool);
  } else { 
    for (WN* wn = cnode; wn != NULL; wn = LWN_Get_Parent(wn)) {
      enter_this_node(index, wn);
      fprintf(stdout, "\n");
    }
    carray_max = index;
  } 
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
// NAME: WB_symbol
// WHIRL BROWSER COMMAND: 's'
// FUNCTION: Print the symbol table entry for this node
//-----------------------------------------------------------------------

static void WB_symbol()
{
  if (buffer[buffer_start] == '<') { 
    UINT32 st_level; 
    UINT32 st_index; 
    char comma; 
    char right_angle_bracket; 
    buffer_start++;
    scan_blanks_and_tabs();
    (void) sscanf(buffer + buffer_start, "%d", &st_level);
    skip_to_separator(SKIP_NUMERIC);
    scan_blanks_and_tabs();
    (void) sscanf(buffer + buffer_start, "%c", &comma);
    if (comma != ',') { 
      Error_Cleanup();
      return; 
    } 
    buffer_start++; 
    scan_blanks_and_tabs();
    (void) sscanf(buffer + buffer_start, "%d", &st_index);  
    skip_to_separator(SKIP_NUMERIC);
    scan_blanks_and_tabs();
    (void) sscanf(buffer + buffer_start, "%c", &right_angle_bracket);
    if (right_angle_bracket != '>') { 
      Error_Cleanup();
      return; 
    } 
    buffer_start++; 
    ST_IDX st_idx = Compute_ST_IDX(st_level, st_index);
    ST* st = &St_Table[st_idx]; 
    fprintf(stdout, "ST_IDX: %d\n", st_idx);
    Print_ST(stdout, st, TRUE);
  } else if (isdigit(buffer[buffer_start])) { 
    ST_IDX st_idx;  
    (void) sscanf(buffer + buffer_start, "%d", &st_idx);
    skip_to_separator(SKIP_NUMERIC);
    ST* st = &St_Table[st_idx]; 
    fprintf(stdout, "ST_IDX: %d\n", st_idx);
    Print_ST(stdout, st, TRUE);
  } else { 
    if (!OPCODE_has_sym(WN_opcode(cnode))) {
      Error_Cleanup(); 
      return; 
    } 
    fprintf(stdout, "ST_IDX: %d\n", WN_st_idx (cnode));
    Print_ST(stdout, WN_st(cnode), TRUE); 
  } 
}

//-----------------------------------------------------------------------
// NAME: WB_type
// WHIRL BROWSER COMMAND: 't'
// FUNCTION: Print the type table entry for this node 
//-----------------------------------------------------------------------

static void WB_type()
{
  if (buffer[buffer_start] == '<') {
    char right_angle_bracket; 
    UINT32 ty_index = 0; 
    buffer_start++; 
    scan_blanks_and_tabs();
    (void) sscanf(buffer + buffer_start, "%d", &ty_index);
    skip_to_separator(SKIP_NUMERIC);
    scan_blanks_and_tabs();
    (void) sscanf(buffer + buffer_start, "%c", &right_angle_bracket);
    if (right_angle_bracket != '>') { 
      Error_Cleanup();
      return; 
    } 
    buffer_start++; 
    Ty_tab[ty_index].Print(stdout);
  } else if (isdigit(buffer[buffer_start])) {
    TY_IDX ty_idx = 0; 
    (void) sscanf(buffer + buffer_start, "%d", &ty_idx);
    skip_to_separator(SKIP_NUMERIC);
    Print_TY(stdout, ty_idx);
  } else { 
    if (!OPCODE_has_sym(WN_opcode(cnode))) {
      Error_Cleanup(); 
      return; 
    } 
    Print_TY(stdout, ST_type(WN_st(cnode)));
  } 
}

//-----------------------------------------------------------------------
// NAME: WB_type_table
// WHIRL BROWSER COMMAND: '#'
// FUNCTION: Print node of this type at this address
//-----------------------------------------------------------------------

static void WB_type_table()
{ 
  WN* node;
  INT type_number; 
  fprintf(stdout, "Available types:\n"); 
  INT i;
  for (i = 0; Type_List[i].type[0] != '\0'; i++)
    fprintf(stdout, "  (%d) %s\n", i, Type_List[i].type); 
  INT last_type_number = i - 1; 
  fprintf(stdout, "Select a type by number: "); 
  load_buffer(); 
  scan_blanks_and_tabs();
  sscanf(buffer + buffer_start, "%d", &type_number); 
  skip_to_separator(SKIP_NUMERIC); 
  if (type_number < 0 || type_number > last_type_number) {
    fprintf(stdout, "Incorrect number selected.\n"); 
    Error_Cleanup(); 
    return; 
  }
  fprintf(stdout, "Enter address: "); 
  load_buffer();
  scan_blanks_and_tabs(); 
  sscanf(buffer + buffer_start, "\n0x%p", &node); 
  buffer_start += 3; 
  skip_to_separator(SKIP_HEX);
  cnode = node;
  if (cnode == 0) { 
    fprintf(stdout, "Bad address.\n"); 
    Error_Cleanup();
    return;  
  } 
  (*(Type_List[type_number].fp))(); 
  prompt(); 
  load_buffer(); 
  scan_blanks_and_tabs(); 
}

//-----------------------------------------------------------------------
// NAME: WB_loops 
// WHIRL BROWSER COMMAND: 'L'
// FUNCTION: Sketch the loop nests in the program unit
//-----------------------------------------------------------------------

static INT loop_count = 0; 

#define WN_MAP_check_kind(maptab, wn_map, kind) \
    ((maptab)->_kind[(wn_map)] == (kind))

static BOOL WN_Map_Valid(WN_MAP wn_map) 
{
  WN_MAP_TAB* maptab = Current_Map_Tab; 
  if (!maptab->_is_used[wn_map])
    return FALSE; 
  if (!WN_MAP_check_kind(maptab, wn_map, WN_MAP_KIND_VOIDP))
    return FALSE; 
  return TRUE; 
} 

extern void dump_loops(WN* wn, FILE* fp, INT spaces, INT increment)
{
  switch (WN_opcode(wn)) { 
  case OPC_BLOCK: {
      for (WN* w = WN_first(wn); w; w = WN_next(w))
        dump_loops(w, fp, spaces, increment);
    }
    break;
  case OPC_DO_LOOP: { 
      dump_spaces(fp, spaces);
      DO_LOOP_INFO* dli = WN_Map_Valid(LNO_Info_Map) 
	? Get_Do_Loop_Info(wn, TRUE) : NULL;
      if (dli == NULL) {
	const char *name = WB_Whirl_Symbol(wn);
        fprintf(fp, "[%d] 0x%p DOLOOP (%d) %s\n", loop_count, wn,
          Srcpos_To_Line(WN_linenum(wn)), name);
      } else {  
	const char *name = WB_Whirl_Symbol(wn); 
        fprintf(fp, "[%d] 0x%p DOLOOP %d (%d) %s\n", loop_count,
          wn, dli->Depth, Srcpos_To_Line(WN_linenum(wn)), name);
      } 
      if (loop_count < MAX_SAVED_NODES) 
	carray[loop_count++] = wn; 
      dump_loops(WN_do_body(wn), fp, spaces + increment, increment);
    } 
    break; 
  case OPC_FUNC_ENTRY: 
    dump_spaces(fp, spaces);
    fprintf(fp, "[%d] 0x%p FUNC_ENTRY %s\n", loop_count, wn,
      WB_Whirl_Symbol(wn));
    if (loop_count < MAX_SAVED_NODES) 
      carray[loop_count++] = wn; 
    dump_loops(WN_func_body(wn), fp, spaces + increment, increment);
    break; 
  case OPC_IF:
    if (fancy < 3) { 
      dump_loops(WN_then(wn), fp, spaces, increment);  
      dump_loops(WN_else(wn), fp, spaces, increment); 
    } else {
      dump_spaces(fp, spaces); 
      fprintf(fp, "[%d] 0x%p IF ([%d] 0x%p) THEN [%d] 0x%p\n", 
        loop_count, wn, loop_count + 1, WN_if_test(wn), 
        loop_count + 2, WN_then(wn));
      INT if_loop_count = loop_count; 
      if (loop_count < MAX_SAVED_NODES)
	carray[loop_count++] = wn; 
      if (loop_count < MAX_SAVED_NODES)
	carray[loop_count++] = WN_if_test(wn);  
      if (loop_count < MAX_SAVED_NODES)
	carray[loop_count++] = WN_then(wn); 
      dump_loops(WN_then(wn), fp, spaces + increment, increment);
      if (!WN_else_is_empty(wn)) { 
	dump_spaces(fp, spaces); 
	fprintf(fp, "[%d] 0x%p ELSE\n", loop_count, WN_else(wn));
	if (loop_count < MAX_SAVED_NODES)
	  carray[loop_count++] = WN_else(wn); 
	dump_loops(WN_else(wn), fp, spaces + increment, increment); 
      } 
      dump_spaces(fp, spaces); 
      fprintf(fp, "[%d] 0x%p ENDIF\n", if_loop_count, wn); 
    } 
    break; 
  case OPC_DO_WHILE: 
    dump_spaces(fp, spaces);
    fprintf(fp, "[%d] 0x%p DO_WHILE_LOOP (%d) \n", loop_count, wn,
      Srcpos_To_Line(WN_linenum(wn)));
    if (loop_count < MAX_SAVED_NODES)
      carray[loop_count++] = wn;
    dump_loops(WN_while_body(wn), fp, spaces + increment, increment);
    break; 
  case OPC_WHILE_DO:
    dump_spaces(fp, spaces);
    fprintf(fp, "[%d] 0x%p WHILE_DO_LOOP (%d) \n", loop_count, wn,
      Srcpos_To_Line(WN_linenum(wn)));
    if (loop_count < MAX_SAVED_NODES)
      carray[loop_count++] = wn;
    dump_loops(WN_while_body(wn), fp, spaces + increment, increment);
    break; 
  case OPC_REGION:
    if (fancy >= 3) {
      dump_spaces(fp, spaces);
      fprintf(fp, "[%d] 0x%p REGION \n", loop_count, wn);
      if (loop_count < MAX_SAVED_NODES)
	carray[loop_count++] = wn;
      for (INT i = 0; i < WN_kid_count(wn); i++) 
	dump_loops(WN_kid(wn, i), fp, spaces + increment, increment); 
    } else {  
      for (INT i = 0; i < WN_kid_count(wn); i++) 
	dump_loops(WN_kid(wn, i), fp, spaces, increment); 
    } 
  }
}

static void WB_loops()
{
  loop_count = 0; 
  dump_loops(cnode, stdout, 0, 2);
  carray_max = loop_count; 
}

//-----------------------------------------------------------------------
// NAME: WB_lisp_loops 
// WHIRL BROWSER COMMAND: 'l'
// FUNCTION: Sketch the loop nests in Suresh Tool format
//-----------------------------------------------------------------------

static void Lisp_Loops_Traverse(WN* wn_tree, 
		                INT spaces, 
		                INT increment, 
		        	FILE* fp)
{
  switch (WN_opcode(wn_tree)) {
  case OPC_DO_LOOP:
    {
      fprintf(fp, "\n"); 
      fflush(fp); 
      dump_spaces(fp, spaces); 
      fprintf(fp, "(DO ");
      const char *name = WB_Whirl_Symbol(wn_tree);
      fprintf(fp, "\"%s\"", name);
      if (WB_sanity_check_level != WBC_DISABLE) {
        DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree);
        fprintf(fp, "\n"); 
        fflush(fp); 
        dump_spaces(fp, spaces + increment); 
        fprintf(fp, " (LOOP_INFO");
        if (dli->Is_Cache_Winddown()) {
	  fprintf(fp, "\n");
	  fflush(fp); 
	  dump_spaces(fp, spaces + 2 * increment); 
	  fprintf(fp, "(CACHE_WINDDOWN)"); 
        }
        if (dli->Is_Register_Winddown()) {
	  fprintf(fp, "\n");
	  fflush(fp); 
	  dump_spaces(fp, spaces + 2 * increment); 
	  fprintf(fp, "(REGISTER_WINDDOWN)"); 
        }
        if (dli->Is_Generally_Unimportant()) {
	  fprintf(fp, "\n");
	  fflush(fp); 
	  dump_spaces(fp, spaces + 2 * increment); 
	  fprintf(fp, "(GENERALLY_UNIMPORTANT)"); 
        }
        fprintf(fp, "\n"); 
        fflush(fp); 
        dump_spaces(fp, spaces + 2 * increment); 
        fprintf(fp, " (TOTAL_ITERATIONS");    
        INT64 iteration_count = Iterations(wn_tree, &LNO_local_pool); 
        if (iteration_count != -1LL) 
	  fprintf(fp, " (EXACT %lld)", iteration_count); 
        INT64 upper_bound = -1LL; 
        if (dli->Is_Inner_Tile && dli->Tile_Size > 0 
	    && (upper_bound == -1LL || upper_bound > dli->Tile_Size))
	  upper_bound = dli->Tile_Size; 
        if (dli->Est_Max_Iterations_Index != -1LL
	    && (upper_bound == -1LL 
	    || upper_bound > dli->Est_Max_Iterations_Index))
	  upper_bound = dli->Est_Max_Iterations_Index;  
        if (upper_bound != -1LL)   
	  fprintf(fp, " (UPPER_BOUND %lld)", upper_bound);
        if (dli->Num_Iterations_Profile)
	  fprintf(fp, " (FEEDBACK %lld)", dli->Est_Num_Iterations);
        else if (iteration_count == -1LL && dli->Est_Num_Iterations != -1LL) 
	  fprintf(fp, " (ESTIMATE %lld)", dli->Est_Num_Iterations); 
        fprintf(fp, ")"); // For TOTAL_ITERATIONS  
        fprintf(fp, ")"); // For LOOP_INFO 
      }
      for (INT i = 0; i < WN_kid_count(wn_tree); i++)
        Lisp_Loops_Traverse(WN_kid(wn_tree, i), spaces + increment, 
	  increment, fp);
      fprintf(fp, ")"); // For DO 
    }
    break; 
  case OPC_DO_WHILE:
    {
      fprintf(fp, "\n"); 
      fflush(fp); 
      dump_spaces(fp, spaces);
      fprintf(fp, "(DO-WHILE "); 
      for (INT i = 0; i < WN_kid_count(wn_tree); i++)
        Lisp_Loops_Traverse(WN_kid(wn_tree, i), spaces + increment, 
	  increment, fp);
      fprintf(fp, ")"); // For DO-WHILE 
    }
    break; 
  case OPC_WHILE_DO: 
    {
      fprintf(fp, "\n"); 
      fflush(fp); 
      dump_spaces(fp, spaces);
      fprintf(fp, "(WHILE-DO "); 
      for (INT i = 0; i < WN_kid_count(wn_tree); i++)
        Lisp_Loops_Traverse(WN_kid(wn_tree, i), spaces + increment, 
	  increment, fp);
      fprintf(fp, ")"); // For WHILE-DO  
    }
    break; 
  case OPC_BLOCK:
    {
      for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
        Lisp_Loops_Traverse(wn, spaces, increment, fp);
    }
    break;
  default: 
    {
      for (INT i = 0; i < WN_kid_count(wn_tree); i++)
        Lisp_Loops_Traverse(WN_kid(wn_tree, i), spaces, increment, fp);
    }
    break;
  }  
}

extern void Lisp_Loops(WN* wn_root, 
		       FILE *fp)
{
  fprintf(fp, "(POST_LNO_LOOPS"); 
  Lisp_Loops_Traverse(wn_root, 2, 2, fp); 
  fprintf(fp, ")\n"); // For POST_LNO_LOOPS 
  fflush(fp); 
}

static void WB_lisp_loops()
{
  Lisp_Loops(WB_global_fd, stdout); 
}

//-----------------------------------------------------------------------
// NAME: WB_transform 
// WHIRL BROWSER COMMAND: 'y'
// FUNCTION: Perform a transformation
//-----------------------------------------------------------------------

static void WB_transform()
{
  INT transform_number;
  fprintf(stdout, "Current loop nest structure:\n"); 
  WB_loops(); 
  fprintf(stdout, "\n"); 
  fprintf(stdout, "Available transformations:\n");
  INT i;
  for (i = 0; Transform_List[i].type[0] != '\0'; i++)
    fprintf(stdout, "  (%d) %s\n", i, Transform_List[i].type);
  INT last_transform_number = i - 1;
  fprintf(stdout, "Select a transformation by number: ");
  Load_Integer(&transform_number); 
  if (transform_number < 0 || transform_number > last_transform_number) {
    fprintf(stdout, "Incorrect number selected.\n");
    Error_Cleanup(); 
    return; 
  }
  BOOL trans_result = (*(Transform_List[transform_number].fp))(); 
  if (!trans_result) 
    return; 
  fprintf(stdout, "\n"); 
  fprintf(stdout, "Transformed loop nest structure:\n"); 
  WB_loops(); 
  prompt(); 
  load_buffer(); 
  scan_blanks_and_tabs(); 
}

//-----------------------------------------------------------------------
// NAME: WB_check 
// WHIRL BROWSER COMMAND: 'C'
// FUNCTION: Check the program unit for consistency
//-----------------------------------------------------------------------

static void WB_check()
{
  switch (WB_sanity_check_level) { 
  case WBC_DISABLE:
    fprintf(stdout, "SANITY CHECKING IS DISABLED\n"); 
    Error_Cleanup(); 
    break;  
  case WBC_DU_ONLY: 
#ifdef Is_True_On
    Du_Sanity_Check(WB_global_fd); 
#endif 
    break;  
  case WBC_DU_AND_ARRAY: 
#ifdef Is_True_On
    Du_Sanity_Check(WB_global_fd); 
    Array_Dependence_Graph->Check_Graph();
    LNO_Check_Graph(Array_Dependence_Graph);
#endif 
    break;  
  case WBC_FULL_SNL: 
#ifdef Is_True_On
    Du_Sanity_Check(WB_global_fd); 
    Array_Dependence_Graph->Check_Graph();
    LNO_Check_Graph(Array_Dependence_Graph);
    SNL_Sanity_Check_Func(WB_global_fd);
#endif 
    break;  
  default: 
    FmtAssert(0, ("Bad value for sanity check level in whirl browser.")); 
    break; 
  }  
} 

//-----------------------------------------------------------------------
// NAME: Is_Substring
// FUNCTION: Return TRUE if 's1' is a substring of 's2', return FALSE 
//   otherwise. 
//-----------------------------------------------------------------------

static BOOL Is_Substring(const char s1[], 
			 const char s2[])
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
// NAME: WB_find 
// WHIRL BROWSER COMMAND: 'F'
// FUNCTION: Find nodes with this symbol in the subtree
// NOTE: If first character of string is "'", find all nodes which have 
//   the given string (after the "'") as a substring of their symbol. 
//-----------------------------------------------------------------------

static INT find_count = 0; 
 
static void find_walk(char *s, WN* wn) 
{
  if (wn == NULL) 
    return;  
  BOOL test_substring = s[0] == '\''; 
  const char* wn_symbol = WB_Whirl_Symbol(wn); 
  if (wn_symbol != NULL && (!test_substring && strcmp(wn_symbol, s) == 0
      || test_substring && Is_Substring(&s[1], wn_symbol))) { 
    fprintf(stdout, "[%d] ", find_count);
    print_this_node(wn); 
    if (find_count < MAX_SAVED_NODES) 
      carray[find_count] = wn; 
    find_count++; 
  } 
  for (INT i = 0; i < WN_kid_count(wn); i++)  
    find_walk(s, WN_kid(wn, i));
  if (WN_opcode(wn) == OPC_BLOCK)  
    for (WN* wn_sub = WN_first(wn); wn_sub != NULL; wn_sub = WN_next(wn_sub))
      find_walk(s, wn_sub);  
} 

static void WB_find_string(char* s)
{ 
  find_count = 0;
  find_walk(s, cnode);
  carray_max = find_count;
} 
 
static void WB_find() 
{
  char s[MAX_STRING_LENGTH];  
  scan_blanks_and_tabs(); 
  sscanf(buffer + buffer_start, "%s", s); 
  INT i;
  for (i = 0; s[i] != '\0'; i++) 
    if (s[i] == '\n' || s[i] == ';')
      break; 
  s[i] = '\0';  
  skip_to_separator(SKIP_ALPHANUMERIC);  
  WB_find_string(s); 
} 

//-----------------------------------------------------------------------
// NAME: WB_find_symbols
// WHIRL BROWSER COMMAND: '$'
// FUNCTION: Print the symbol table entry with this name
//-----------------------------------------------------------------------

static void WB_find_symbols()
{
  ST* st = NULL;
  char s[MAX_STRING_LENGTH];
  scan_blanks_and_tabs();
  sscanf(buffer + buffer_start, "%s", s);
  INT i;
  for (i = 0; s[i] != '\0'; i++)
    if (s[i] == '\n' || s[i] == ';')
      break;
  s[i] = '\0';
  skip_to_separator(SKIP_ALPHANUMERIC);
  BOOL test_substring = s[0] == '\'';
#ifdef _NEW_SYMTAB
  FOREACH_SYMBOL(CURRENT_SYMTAB, st, i) {
#else
  SYMTAB* symtab = Current_Symtab;
  for (st = SYMTAB_symbols(symtab); st != NULL; st = ST_next(st)) {
#endif
    if ((!test_substring && strcmp(ST_name(st), s) == 0
        || test_substring && Is_Substring(&s[1], ST_name(st)))) {
      Print_ST(stdout, st, TRUE);
    }
  }
#ifdef _NEW_SYMTAB
  FOREACH_SYMBOL(GLOBAL_SYMTAB, st, i) {
    if ((!test_substring && strcmp(ST_name(st), s) == 0
        || test_substring && Is_Substring(&s[1], ST_name(st)))) {
      Print_ST(stdout, st, TRUE);
    }
  }
#endif
}

//-----------------------------------------------------------------------
// NAME: WB_findopr 
// WHIRL BROWSER COMMAND: 'o'
// FUNCTION: Find nodes with this operator in the subtree
//-----------------------------------------------------------------------

static INT findopr_count = 0; 

static void findopr_walk(OPERATOR opr_test, WN* wn) 
{
  if (wn == NULL) 
    return;  
  if (WN_operator(wn) == opr_test) { 
    fprintf(stdout, "[%d] ", findopr_count);
    print_this_node(wn); 
    if (findopr_count < MAX_SAVED_NODES) 
      carray[findopr_count] = wn; 
    findopr_count++; 
  } 
  if (WN_opcode(wn) == OPC_BLOCK) { 
    for (WN* wn_sub = WN_first(wn); wn_sub != NULL; wn_sub = WN_next(wn_sub))
      findopr_walk(opr_test, wn_sub);  
  } else { 
    for (INT i = 0; i < WN_kid_count(wn); i++)  
      findopr_walk(opr_test, WN_kid(wn, i));
  } 
} 

static void WB_findopr_string(OPERATOR opr_test)
{ 
  findopr_count = 0;
  findopr_walk(opr_test, cnode);
  carray_max = findopr_count;
} 
 
static void WB_findopr()
{ 
  OPERATOR opr_test; 
  char s[MAX_STRING_LENGTH];
  scan_blanks_and_tabs();
  sscanf(buffer + buffer_start, "%s", s);
  INT i;
  for (i = 0; s[i] != '\0'; i++)
    if (s[i] == '\n' || s[i] == ';')
      break;
  s[i] = '\0';
  skip_to_separator(SKIP_ALPHANUMERIC);
  for (i = 1; i <= OPERATOR_LAST; i++)
    if (!strcmp(s, operator_table[i]))
      break; 
  if (i > OPERATOR_LAST) { 
    Error_Cleanup(); 
    return; 
  } 
  WB_findopr_string((OPERATOR) i);
} 

//-----------------------------------------------------------------------
// NAME: WB_whirl2fset 
// WHIRL BROWSER COMMAND: 'f'
// FUNCTION: Change WHIRL-TO-SOURCE language to FORTRAN
//-----------------------------------------------------------------------

static void WB_whirl2fset()
{
  if (WB_language != SRC_FORTRAN) { 
    Whirl2F_Init(WB_global_fd); 
    WB_language = SRC_FORTRAN; 
  }
  fprintf(stdout, "WHIRL-TO-SOURCE language is FORTRAN. "); 
  fprintf(stdout, "\n");
}

//-----------------------------------------------------------------------
// NAME: WB_whirl2cset 
// WHIRL BROWSER COMMAND: 'c'
// FUNCTION: Change WHIRL-TO-SOURCE language to C
//-----------------------------------------------------------------------

static void WB_whirl2cset()
{
  if (WB_language != SRC_C) {
    Whirl2C_Init(WB_global_fd); 
    WB_language = SRC_C; 
  }
  fprintf(stdout, "WHIRL-TO-SOURCE language is C.\n"); 
}

//-----------------------------------------------------------------------
// NAME: WB_scratch 
// WHIRL BROWSER COMMAND: 'B'
// FUNCTION: Print the scratch register with this number  
//-----------------------------------------------------------------------

static void WB_scratch()
{
  if (FORMULA::Fpool == NULL) { 
    Error_Cleanup();
    return; 
  } 
  INT scratch_register = -1;
  scan_blanks_and_tabs();
  (void) sscanf(buffer + buffer_start, "%d", &scratch_register);
  skip_to_separator(SKIP_NUMERIC);
  if (!(scratch_register >= 0 && scratch_register 
      < FORMULA::SCRATCH_REGISTERS)) {
    Error_Cleanup();
    return; 
  } 
  double dvalue = FORMULA::Use(scratch_register)->Eval(0, (double *) NULL);
  fprintf(stdout, "Value is %.4g\n", dvalue);
}

//-----------------------------------------------------------------------
// NAME: WB_last_command 
// WHIRL BROWSER COMMAND: '.'
// FUNCTION: Execute the previous command
//-----------------------------------------------------------------------

static void WB_last_command() 
{
  INT i;
  for (i = 0; Command_List[i].ch != '\0'; i++)
    if (Command_List[i].context_independent && Command_List[i].ch == last_ch) 
      break;
  if (Command_List[i].ch == '\0') {
    Error_Cleanup(); 
    return; 
  }
  (*(Command_List[i].fp))();
}      

//-----------------------------------------------------------------------
// NAME: WB_help 
// WHIRL BROWSER COMMAND: 'H'
// FUNCTION: Print this information
//-----------------------------------------------------------------------

static void WB_help()
{
  fprintf(stdout, "The following commands are available:\n"); 
  for (INT i = 0; Command_List[i].ch != '\0'; i++) 
    for (INT j = 0; j < ASCII_CHAR_COUNT; j++)
      if (WB_keymap[j] == Command_List[i].ch)
        fprintf(stdout, "  %c: %s\n", j, Command_List[i].message);
  fprintf(stdout, "  Q: Exit the debugger\n"); 
  fprintf(stdout, "  q: Exit the debugger\n"); 
}

//-----------------------------------------------------------------------
// COMMANDS USED TO PRINT TYPES WITH THE '#' COMMAND 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: WBT_SNL_DEP_INFO 
// FUNCTION: Print the SNL_DEP_INFO 
//-----------------------------------------------------------------------

static void WBT_SNL_DEP_INFO() 
{ 
  SNL_DEP_INFO* this_address = (SNL_DEP_INFO*) cnode; 
  this_address->Print(stdout); 
} 

//-----------------------------------------------------------------------
// NAME: WBT_SNL_DEP_MATRIX
// FUNCTION: Print the SNL_DEP_MATRIX 
//-----------------------------------------------------------------------

static void WBT_SNL_DEP_MATRIX() 
{ 
  SNL_DEP_MATRIX* this_address = (SNL_DEP_MATRIX*) cnode; 
  this_address->Print(stdout); 
} 

//-----------------------------------------------------------------------
// NAME: WBT_SNL_NEST_INFO 
// FUNCTION: Print the SNL_NEST_INFO 
//-----------------------------------------------------------------------

static void WBT_SNL_NEST_INFO() 
{ 
  SNL_NEST_INFO* this_address = (SNL_NEST_INFO*) cnode; 
  this_address->Print(stdout); 
} 

//-----------------------------------------------------------------------
// NAME: WBT_SNL_NEST_INFO 
// FUNCTION: Print an object of type SNL_NEST_INFO 
//-----------------------------------------------------------------------

static void WBT_DOLOOP_STACK() 
{
  DOLOOP_STACK* this_address = (DOLOOP_STACK*) cnode; 
  if (WB_sanity_check_level == WBC_DISABLE) { 
    Error_Cleanup(); 
    return;
  } 
  fprintf(stdout, "Stack has %d elements\n", this_address->Elements());
  INT i;
  for (i = 0; i < this_address->Elements(); i++) 
    if (WN_MAP_Get(LNO_Info_Map, this_address->Bottom_nth(i)) == NULL) 
      break; 
  if (i < this_address->Elements()) { 
    fprintf(stdout, "Stack has unmarked do loop.\n"); 
    Error_Cleanup(); 
    return; 
  } 
  for (i = 0; i < this_address->Elements(); i++) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(this_address->Bottom_nth(i));
    fprintf(stdout, "[%d] 0x%p %d\n", i, this_address->Bottom_nth(i), 
      dli->Depth);
  }
} 

//-----------------------------------------------------------------------
// NAME: WBT_ARRAY_REF_STAR 
// FUNCTION: Print an object of type ARRAY_REF_STAR
//-----------------------------------------------------------------------

static void WBT_ARRAY_REF_STAR()
{
  ARRAY_REF* this_address = (ARRAY_REF*) cnode; 
  this_address->Print(stdout); 
}

//-----------------------------------------------------------------------
// NAME: WBT_ARRAY_REF_LIST 
// FUNCTION: Print an object of type ARRAY_REF_LIST 
//-----------------------------------------------------------------------

static void WBT_ARRAY_REF_LIST()
{
  ARRAY_REF_LIST* this_address = (ARRAY_REF_LIST*) cnode; 
  this_address->Print(stdout); 
}

//-----------------------------------------------------------------------
// NAME: WBT_COMPUTE_FOOTPRINT_RVAL 
// FUNCTION: Print an object of type COMPUTE_FOOTPRINT_RVAL 
//-----------------------------------------------------------------------

static void WBT_COMPUTE_FOOTPRINT_RVAL()
{
  COMPUTE_FOOTPRINT_RVAL* this_address = (COMPUTE_FOOTPRINT_RVAL*) cnode; 
  this_address->Print(stdout);
}

//-----------------------------------------------------------------------
// NAME: WBT_MHD_LEVEL
// FUNCTION: Print an object of type MHD_LEVEL 
//-----------------------------------------------------------------------

static void WBT_MHD_LEVEL()
{
  MHD_LEVEL* this_address = (MHD_LEVEL*) cnode;
  this_address->Print(stdout);
  fprintf(stdout, "\n"); 
}

//-----------------------------------------------------------------------
// NAME: WBT_MHD
// FUNCTION: Print an object of type MHD 
//-----------------------------------------------------------------------

static void WBT_MHD()
{
  MHD* this_address = (MHD*) cnode;
  this_address->Print(stdout);
  fprintf(stdout, "\n"); 
}

//-----------------------------------------------------------------------
// NAME: WBT_FORMULA
// FUNCTION: Print an object of type FORMULA 
//-----------------------------------------------------------------------

static void WBT_FORMULA()
{
  FORMULA* this_address = (FORMULA*) cnode; 
  this_address->Print(stdout); 
  fprintf(stdout, "\n");
}

static void WBT_FORMULA_EVALUATE()
{
  INT count = -1;  
  INT variable_type = -1; 
  double vars_double[MAX_FORMULA_VARS]; 
  mINT32 vars_int32[MAX_FORMULA_VARS]; 
  mINT64 vars_int64[MAX_FORMULA_VARS];

  FORMULA* this_address = (FORMULA*) cnode;
  fprintf(stdout, 
    "Available variable types: [0] double [1] INT32 [2] INT64\n"); 
  fprintf(stdout, "Select a variable type by number: "); 
  Load_Integer(&variable_type); 
  fprintf(stdout, "Enter the number of values: "); 
  Load_Integer(&count); 
  fprintf(stdout, "Enter the values in order: \n"); 
  for (INT i = 0; i < count; i++) {
    fprintf(stdout, "[%d] ", i); 
    switch (variable_type) {
    case 0:
      Load_Double(&vars_double[i]);  
      this_address->Eval(count, vars_double); 
      fprintf(stdout, "Value is %g\n", vars_double[i]);   
      break;  
    case 1: 
      Load_mINT32(&vars_int32[i]);  
      this_address->Eval(count, vars_int32); 
      fprintf(stdout, "Value is %d\n", vars_int32[i]);   
      break;  
    case 2: 
      Load_mINT64(&vars_int64[i]);  
      this_address->Eval(count, vars_int64); 
      fprintf(stdout, "Value is %lld\n", vars_int64[i]);   
      break; 
    } 
  }
}

//-----------------------------------------------------------------------
// NAME: WBT_PAR_STAT
// FUNCTION: Print an object of type PAR_STAT 
//-----------------------------------------------------------------------

static void WBT_PAR_STAT()
{
  PAR_STAT* this_address = (PAR_STAT*) cnode; 
  this_address->Print(stdout, 0); 
  fprintf(stdout, "\n");
}

//-----------------------------------------------------------------------
// NAME: WBT_MAT_INT 
// FUNCTION: Print an object of type WBT_MAT_INT 
//-----------------------------------------------------------------------

static void WBT_MAT_INT()
{
  MAT<INT>* this_address = (MAT<INT>*) cnode; 
  this_address->Print(stdout); 
  fprintf(stdout, "\n");
}

//-----------------------------------------------------------------------
// NAME: WBT_MAT_FRAC
// FUNCTION: Print an object of type MAT<FRAC> 
//-----------------------------------------------------------------------

static void WBT_MAT_FRAC()
{
  MAT<FRAC>* this_address = (MAT<FRAC>*) cnode; 
  this_address->Print(stdout); 
  fprintf(stdout, "\n");
}

//-----------------------------------------------------------------------
// NAME: WBT_LU_MAT_INT 
// FUNCTION: Print an object of type LU_MAT<INT> 
//-----------------------------------------------------------------------

static void WBT_LU_MAT_INT()
{
  LU_MAT<INT>* this_address = (LU_MAT<INT>*) cnode; 
  this_address->Print(stdout); 
  fprintf(stdout, "\n");
}

//-----------------------------------------------------------------------
// NAME: WBT_LU_MAT_FRAC 
// FUNCTION: Print an object of type LU_MAT<FRAC> 
//-----------------------------------------------------------------------

static void WBT_LU_MAT_FRAC()
{
  LU_MAT<FRAC>* this_address = (LU_MAT<FRAC>*) cnode; 
  this_address->Print(stdout); 
  fprintf(stdout, "\n");
}

//-----------------------------------------------------------------------
// COMMANDS USED TO PERFORM TRANSFORMATIONS WITH THE 'y' COMMAND 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: WBTR_Pre_Loop_Peeling 
// FUNCTION: Perform pre-loop-peeling.  
//-----------------------------------------------------------------------

static BOOL WBTR_Pre_Loop_Peeling()
{
  WN* wn_loop = NULL; 
  UINT32 iter_count = 0; 
  BOOL unrolled = TRUE; 
  BOOL preserve_loop_index = TRUE;  
  fprintf(stdout, "Enter a loop address: "); 
  if (!Load_Loop(&wn_loop))
    return FALSE;  
  fprintf(stdout, "Enter iteration count: "); 
  Load_UINT32(&iter_count); 
  fprintf(stdout, "Do you want the peeled section unrolled [Y]? "); 
  Load_Boolean(&unrolled, TRUE, TRUE); 
  fprintf(stdout, "Do you want to preserve the loop index [Y]? "); 
  Load_Boolean(&preserve_loop_index, TRUE, TRUE); 
  fprintf(stdout, "Peeling %d iterations from the front of loop 0x%p\n",   
    iter_count, wn_loop);   
  Pre_loop_peeling(wn_loop, iter_count, unrolled, preserve_loop_index); 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: WBTR_Post_Loop_Peeling 
// FUNCTION: Perform post-loop-peeling.  
//-----------------------------------------------------------------------

static BOOL WBTR_Post_Loop_Peeling()
{
  WN* wn_loop = NULL; 
  UINT32 iter_count = 0; 
  BOOL unrolled = TRUE;  
  BOOL preserve_loop_index = TRUE;  
  fprintf(stdout, "Enter a loop address: "); 
  if (!Load_Loop(&wn_loop))
    return FALSE; 
  fprintf(stdout, "Enter iteration count: "); 
  Load_UINT32(&iter_count); 
  fprintf(stdout, "Do you want the peeled section unrolled [Y]? "); 
  Load_Boolean(&unrolled, TRUE, TRUE); 
  fprintf(stdout, "Do you want to preserve the loop index [Y]? "); 
  Load_Boolean(&preserve_loop_index, TRUE, TRUE); 
  fprintf(stdout, "Peeling %d iterations from the back of loop 0x%p\n", 
    iter_count, wn_loop);   
  Post_loop_peeling(wn_loop, iter_count, unrolled, preserve_loop_index); 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: WBTR_Loop_Reversal 
// FUNCTION: Perform loop reversal. 
//-----------------------------------------------------------------------

static BOOL WBTR_Loop_Reversal()
{
  WN* wn_loop = NULL; 
  BOOL legality_check = TRUE; 
  fprintf(stdout, "Enter a loop address: ");
  if (!Load_Loop(&wn_loop))
    return FALSE;  
  fprintf(stdout, "Check for legality [Y]? "); 
  Load_Boolean(&legality_check, TRUE, TRUE); 
  if (legality_check) { 
    if (!RV_Is_Legal(wn_loop)) {
      fprintf(stdout, "Reversal of loop 0x%p is NOT legal.\n", wn_loop); 
      Error_Cleanup(); 
      return FALSE;
    }
    fprintf(stdout, "Reversal of loop 0x%p is legal.\n", wn_loop); 
  }
  fprintf(stdout, "Reversing loop 0x%p\n", wn_loop); 
  RV_Reverse_Loop(wn_loop);
  return TRUE;  
}

#define MAX_PERMUTE_LOOPS 20 

//-----------------------------------------------------------------------
// NAME: Load_SNL_And_Permutation 
// FUNCTION: Query the user for the addresses of the outermost loop in an 
//   SNL 'wn_outer', the innermost loop in an SNL 'wn_inner' and a 'per-
//   mutation' of Do_Loop_Depth(wn_inner) - Do_Loop_Depth(wn_outer) + 1.     
//   Returns TRUE if all data were gathered without problems, FALSE 
//   otherwise. 
//-----------------------------------------------------------------------

static BOOL Load_SNL_And_Permutation(WN** wn_outer_addr, 
				     WN** wn_inner_addr, 
				     INT permutation[])
{
  WN* wn_outer = NULL; 
  WN* wn_inner = NULL; 
  BOOL legality_check = TRUE; 
  fprintf(stdout, "Enter the outermost loop's address: "); 
  if (!Load_Loop(&wn_outer))
    return FALSE;
  fprintf(stdout, "Enter the innermost loop's address: "); 
  if (!Load_Loop(&wn_inner))
    return FALSE;
  INT permutation_count = 0; 
  WN* wn = 0;
  for (wn = wn_inner; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP)
      permutation_count++; 
    if (wn == wn_outer) 
      break; 
  }
  if (wn == NULL) {
    fprintf(stdout, "Inner loop not nested in outer loop.\n"); 
    Error_Cleanup(); 
    return FALSE;
  }
  if (permutation_count < 2) {
    fprintf(stdout, "Must have at least two loops in permutation.\n"); 
    Error_Cleanup(); 
    return FALSE; 
  }
  if (permutation_count > MAX_PERMUTE_LOOPS) { 
    fprintf(stdout, "Too many loops in this permutation.\n"); 
    Error_Cleanup(); 
    return FALSE; 
  }
  for (INT i = 0; i < permutation_count; i++) {
    fprintf(stdout, "Enter Permutation Element (%d): ", i); 
    Load_Integer(&permutation[i]); 
  }
  if (!Is_Permutation_Vector(permutation, permutation_count)) {
    fprintf(stdout, "Permutation specified is not a possible permutation.\n");
    Error_Cleanup();
    return FALSE; 
  }
  *wn_outer_addr = wn_outer; 
  *wn_inner_addr = wn_inner; 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: WBTR_Loop_Permutation 
// FUNCTION: Perform loop permutation. 
//-----------------------------------------------------------------------

static BOOL WBTR_Loop_Permutation()
{
  WN* wn_outer = NULL; 
  WN* wn_inner = NULL; 
  BOOL legality_check = TRUE; 
  INT permutation[MAX_PERMUTE_LOOPS]; 
  if (!Load_SNL_And_Permutation(&wn_outer, &wn_inner, permutation)) 
    return FALSE; 
  INT nloops = Do_Loop_Depth(wn_inner) - Do_Loop_Depth(wn_outer) + 1;
  fprintf(stdout, "Check for legality [Y]? "); 
  Load_Boolean(&legality_check, TRUE, TRUE); 
  if (legality_check) { 
    WN* wn_safe_outer = Minimal_Kernel(wn_outer, nloops); 
    if (wn_outer != wn_safe_outer) {
      fprintf(stdout, "Safest outermost loop is 0x%p.\n", wn_safe_outer); 
      Error_Cleanup(); 
      return FALSE; 
    } 
    if (SNL_Permutation_Needs_Distribution(wn_outer, permutation, nloops)) { 
      fprintf(stdout, "This permutation is illegal due to sandwiched code.\n"); 
      Error_Cleanup(); 
      return FALSE;
    }
    SNL_DEP_MATRIX** sdm_inv = Inv_Dep_Info(wn_outer, nloops, FALSE, FALSE); 
    if (!SNL_Legal_Perm_Deps(sdm_inv[nloops - 1], permutation, nloops)) {
      fprintf(stdout, "This permutation of loops has illegal dependences.\n"); 
      Error_Cleanup(); 
      return FALSE;
    }
  } 
  BOOL is_general = General_Permutation(wn_outer, permutation, nloops); 
  BOOL is_invariant = Invariant_Permutation(wn_outer, permutation, nloops); 
  if (is_invariant) {
    fprintf(stdout, "This permutation of loops is legal and invariant.\n"); 
    SNL_INV_Permute_Loops(wn_outer, permutation, nloops, TRUE); 
  } else if (is_general) { 
    fprintf(stdout, "This permutation of loops is legal and general.\n"); 
    SNL_GEN_Permute_Loops(wn_outer, permutation, nloops, TRUE); 
  } else {
    fprintf(stdout, "This permutation of loops is neither general "); 
    fprintf(stdout, "nor invariant.\n"); 
    Error_Cleanup(); 
    return FALSE;
  }  
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: WBTR_Scalar_Expansion 
// FUNCTION: Perform scalar expansion. 
//-----------------------------------------------------------------------

static BOOL WBTR_Scalar_Expansion()
{
  WN* wn_outer = NULL; 
  WN* wn_inner = NULL; 
  BOOL legality_check = TRUE; 
  INT permutation[MAX_PERMUTE_LOOPS]; 
  if (!Load_SNL_And_Permutation(&wn_outer, &wn_inner, permutation)) 
    return FALSE; 
  INT nloops = Do_Loop_Depth(wn_inner) - Do_Loop_Depth(wn_outer) + 1;
  SX_INFO sx_info(&LNO_default_pool); 
  sx_info.Make_Sx_Info(wn_outer, nloops);
  fprintf(stdout, "Check for legality [Y]? "); 
  Load_Boolean(&legality_check, TRUE, TRUE); 
  if (legality_check) { 
    WN* wn_safe_outer = Minimal_Kernel(wn_outer, nloops); 
    if (wn_outer != wn_safe_outer) {
      fprintf(stdout, "Safest outermost loop is 0x%p.\n", wn_safe_outer); 
      Error_Cleanup(); 
      return FALSE; 
    } 
    const SX_PNODE* p = NULL;
    INT ftd = sx_info.First_Transformable_Depth(&p);  
    INT i;
    for (i = 0; i < nloops; i++) 
      if (permutation[i] != i) 
	break;
    if (ftd > Do_Loop_Depth(wn_outer) + i) {
      fprintf(stdout, 
        "Scalar expansion for this permutation of loops is NOT legal.\n"); 
      Error_Cleanup(); 
      return FALSE;
    }
    fprintf(stdout, 
      "Scalar expansion for this permutation of loops is legal.\n"); 
  }
  SX_PLIST* plist = &(sx_info.Plist); 
  BOOL invariant = Invariant_Permutation(wn_outer, permutation, nloops); 
  if (invariant) 
    SNL_INV_Scalar_Expand(wn_outer, permutation, nloops, plist);
  else 
    SNL_GEN_Scalar_Expand(wn_outer, permutation, nloops, plist);
  return TRUE;  
}

//-----------------------------------------------------------------------
// NAME: WBTR_Distribution 
// FUNCTION: Perform loop distribution.  
//-----------------------------------------------------------------------

static BOOL WBTR_Distribution()
{
  WN* wn_outer = NULL; 
  WN* wn_inner = NULL; 
  BOOL legality_check = TRUE; 
  INT permutation[MAX_PERMUTE_LOOPS]; 
  if (!Load_SNL_And_Permutation(&wn_outer, &wn_inner, permutation)) 
    return FALSE; 
  INT nloops = Do_Loop_Depth(wn_inner) - Do_Loop_Depth(wn_outer) + 1;
  fprintf(stdout, "Check for legality [Y]? "); 
  Load_Boolean(&legality_check, TRUE, TRUE); 
  if (legality_check) { 
    WN* wn_safe_outer = Minimal_Kernel(wn_outer, nloops); 
    if (wn_outer != wn_safe_outer) {
      fprintf(stdout, "Safest outermost loop is 0x%p.\n", wn_safe_outer); 
      Error_Cleanup(); 
      return FALSE; 
    } 
    if (!SNL_Permutation_Is_Distributable(wn_outer, permutation, nloops)) {
      fprintf(stdout, "Distribution for this permutation is not legal.\n");
      Error_Cleanup(); 
      return FALSE; 
    }
    fprintf(stdout, "Distribution for this permutation of loops is legal.\n"); 
  } 
  SNL_Distribute_For_Permutation(wn_outer, wn_inner, permutation, nloops);
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: WBTR_SNL_INV_Limited_SE_And_Dist 
// FUNCTION: Perform limited tile size scalar expansion tiling and 
//   distribution.  
//-----------------------------------------------------------------------

static BOOL WBTR_SNL_INV_Limited_SE_And_Dist()
{
  WN* wn_outer = NULL; 
  WN* wn_inner = NULL; 
  BOOL legality_check = TRUE; 
  INT permutation[MAX_PERMUTE_LOOPS]; 
  if (!Load_SNL_And_Permutation(&wn_outer, &wn_inner, permutation)) 
    return FALSE; 
  SX_INFO sx_info(&LNO_default_pool); 
  INT nloops = Do_Loop_Depth(wn_inner) - Do_Loop_Depth(wn_outer) + 1; 
  sx_info.Make_Sx_Info(wn_outer, nloops);
  fprintf(stdout, "Check for legality [Y]? "); 
  Load_Boolean(&legality_check, TRUE, TRUE); 
  if (legality_check) { 
    WN* wn_safe_outer = Minimal_Kernel(wn_outer, nloops); 
    if (wn_outer != wn_safe_outer) {
      fprintf(stdout, "Safest outermost loop is 0x%p.\n", wn_safe_outer); 
      Error_Cleanup(); 
      return FALSE; 
    } 
    const SX_PNODE* p = NULL;
    INT ftd = sx_info.First_Transformable_Depth(&p);  
    INT i;
    for (i = 0; i < nloops; i++) 
      if (permutation[i] != i) 
	break;
    if (ftd < Do_Loop_Depth(wn_outer) + i) {
      fprintf(stdout, 
        "Scalar expansion for this permutation of loops is NOT legal.\n"); 
      Error_Cleanup(); 
      return FALSE;
    }
    fprintf(stdout, "Scalar expansion and distribution "); 
    fprintf(stdout, "for this permutation of loops is legal.\n"); 
  }
  SX_PLIST* plist = &(sx_info.Plist); 
  SNL_TILE_INFO* ti_se = NULL; 
  SE_New_Tile_Infos(wn_outer, plist, permutation, nloops, &LNO_local_pool, 
    &ti_se, FALSE); 
  SNL_INV_Limited_SE_And_Dist(wn_outer, ti_se, permutation, nloops, plist,
    FALSE); 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Tiling_Prefix_List
// FUNCTION: The following table is used by the "Loop Tiling" transfor- 
//   mation invoked with the 'y' command.  Each table entry has three parts:
//   (1) A string used for the prefix of the tile index variable
//   (2) A string representing the reason for tiling  
//   (3) The SNL_INV_CACHE_BLOCK_REASON equivalent of (2)
//-----------------------------------------------------------------------

struct WB_STRING_REASON {
  const char *type; 
  const char *reason_string;
  SNL_INV_CACHE_BLOCK_REASON reason; 
};

static WB_STRING_REASON Tiling_Prefix_List[] = 
{
  "$tile", 	"SNL_INV_TILE_ONLY", 	SNL_INV_TILE_ONLY,
  "$seonly",	"SNL_INV_SE_ONLY", 	SNL_INV_SE_ONLY, 	
  "$setile",	"SNL_INV_TILE_SE", 	SNL_INV_TILE_SE, 
  "$dsmtile",	"SNL_INV_LEGO_TILE", 	SNL_INV_LEGO_TILE, 
  "$datile",	"SNL_INV_MP_TILE", 	SNL_INV_MP_TILE,  
  "",  		"SNL_INV_UNDEFINED", 	SNL_INV_UNDEFINED 
};  

//-----------------------------------------------------------------------
// NAME: WBTR_Loop_Tiling
// FUNCTION: Perform loop tiling  
//-----------------------------------------------------------------------

static BOOL WBTR_Loop_Tiling()
{
  BOOL legality_check = TRUE; 
  WN* wn_loop = NULL; 
  INT tile_size = 0;
  const char* prefix = NULL; 
  INT transform_number = -1; 
  SYMBOL* sym_pid = NULL;
  SNL_INV_CACHE_BLOCK_REASON reason = SNL_INV_UNDEFINED; 
  ARRAY_DIRECTED_GRAPH16 *dg = Array_Dependence_Graph; 
  DU_MANAGER* du = WB_du_mgr; 
  REDUCTION_MANAGER* rm = red_manager;  
  fprintf(stdout, "Enter a loop address: "); 
  if (!Load_Loop(&wn_loop))
    return FALSE; 
  fprintf(stdout, "Enter a tile size: "); 
  Load_Integer(&tile_size);  
  fprintf(stdout, "Select an reason for tiling by number:\n"); 
  INT i;
  for (i = 0; Tiling_Prefix_List[i].type[0] != '\0'; i++) 
    fprintf(stdout, "  (%d) %s\n", i, Tiling_Prefix_List[i].reason_string); 
  INT last_transform_number = i - 1;
  fprintf(stdout, "Select a transformation by number: ");
  Load_Integer(&transform_number);
  if (transform_number < 0 || transform_number > last_transform_number) {
    fprintf(stdout, "Incorrect number selected.\n");
    Error_Cleanup();
    return FALSE;
  }
  fprintf(stdout, "Check for legality [Y]? "); 
  Load_Boolean(&legality_check, TRUE, TRUE); 
  if (legality_check) { 
    WN* wn_safe_loop = Minimal_Kernel(wn_loop, 1); 
    if (wn_loop != wn_safe_loop) {
      fprintf(stdout, "Loop 0x%p was not safe for tiling\n", wn_loop); 
      Error_Cleanup(); 
      return FALSE; 
    } 
    if (Step_Size(wn_loop) != 1) { 
      fprintf(stdout, "Loop 0x%p does not have unity step\n", wn_loop); 
      Error_Cleanup(); 
      return FALSE; 
    } 
  } 
  prefix = Tiling_Prefix_List[transform_number].type; 
  reason = Tiling_Prefix_List[transform_number].reason; 
  SYMBOL oldsym(WN_index(wn_loop));
  INT required_length = strlen(prefix) + strlen(oldsym.Name()) + 1;
  char* Str_Buf = CXX_NEW_ARRAY(char, required_length, &LNO_local_pool); 
  sprintf(Str_Buf, "%s%s", prefix, oldsym.Name());
  sym_pid = CXX_NEW(SYMBOL(Create_Preg_Symbol(Str_Buf, Do_Wtype(wn_loop))),
    &LNO_default_pool);
  Tile_Loop(wn_loop, tile_size, 0, reason, sym_pid, &LNO_default_pool); 
  return TRUE; 
}

//-----------------------------------------------------------------------
// SUPPORT FUNCTIONS FOR THE MAIN DRIVER OF THE WHIRL BROWSER.  
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: get_command 
// FUNCTION: Using the 'buffer' scan past spaces, tabs, and semicolons to 
//   get to the next character, and return it as the character represen-
//   tation of the next command.  
//-----------------------------------------------------------------------

static char get_command()
{
  INT i = buffer_start;
  while (buffer[i] == ' ' || buffer[i] == '\t' || buffer[i] == ';') i++;
  buffer_start = i + 1;
  return buffer[i];
}

//-----------------------------------------------------------------------
// NAME: Initialize_Language
// FUNCTION: Set the value of 'WB_language' and print out that language.
//-----------------------------------------------------------------------

static void Initialize_Language() {
  switch (PU_src_lang(Get_Current_PU())) {
  case PU_C_LANG:
  case PU_CXX_LANG:
    WB_language = SRC_C; 
    fprintf(stdout, "WHIRL-TO-SOURCE language is C.  "); 
    break;
  case PU_F90_LANG:
  case PU_F77_LANG:
    WB_language = SRC_FORTRAN; 
    fprintf(stdout, "WHIRL-TO-SOURCE language is FORTRAN.  "); 
    break;
  default:
    WB_language = SRC_NONE; 
    fprintf(stdout,  
      "Can't do WHIRL-TO-SOURCE tranformations in this language.  "); 
    break;
  }
}

//-----------------------------------------------------------------------
// NAME: Unmappable_Character
// FUNCTION: Return TRUE if the character 'ch' is not remappable in the
//   .wb_keymap file.  Return FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL Unmappable_Character(char ch) { 
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
// NAME: Initialize_Keymap
// FUNCTION: Initialize the keymap by using user specifed values to 
//   override the default values of the keys. 
//-----------------------------------------------------------------------

static void Initialize_Keymap() { 
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
    scan_blanks_and_tabs(key_buffer, &key_buffer_start);
    if (strncasecmp(&key_buffer[key_buffer_start], "SILENT",
        strlen("SILENT")) == 0) { 
      print_log = FALSE; 
    } else if (strncasecmp(&key_buffer[key_buffer_start], "VERBOSE",
        strlen("VERBOSE")) == 0) { 
      print_log = TRUE; 
    } else if (strncasecmp(&key_buffer[key_buffer_start], "TRANSLATE",
        strlen("TRANSLATE")) == 0) {
      key_buffer_start += strlen("TRANSLATE");
      scan_blanks_and_tabs(key_buffer, &key_buffer_start);
      char old_char = key_buffer[key_buffer_start++];
      if (Unmappable_Character(old_char)) {
        fprintf(stdout,
          ".wb_keymap: Error on line %d: Cannot map %c\n",
          line_number, old_char);
        continue;
      }
      scan_blanks_and_tabs(key_buffer, &key_buffer_start);
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
      WB_keymap[new_char] = old_char; 
    } else { 
      fprintf(stdout, ".wb_keymap: Error on line %d: Unrecognized command\n",
        line_number);
    } 
  }  
  fclose(fp_keymap);
} 

//-----------------------------------------------------------------------
// NAME: s_lno_debug 
// FUNCTION: The 'string debugger'. Run the whirl browser using 'init_buffer'
//   as the initial set of commands. 
// NOTE: 'sdebug' can be called from the compiler as an aid to debugging in 
//   batch mode.  For example, to print out the loop nests at a certain 
//   place during compiler execution, put a call of the form: 
//     sdebug("RLQ");
//   in the compiler where you want to see the loops.   
//-----------------------------------------------------------------------

extern void s_lno_debug(const char init_buffer[]) 
{
  char ch;
  VOID_FUNC_PTR fp;
  BOOL reload;
  
  last_ch = '\0';
  if (WB_global_fd == NULL) {
    fprintf(stdout, "Whirl browser only valid in Loop Nest Optimizer.\n"); 
    Error_Cleanup(); 
    return; 
  }
  Initialize_Keymap();  
  Initialize_Language(); 
  fprintf(stdout, "DAVINCI is %s. ", WB_davinci_mode ? "ON" : "OFF");
  fprintf(stdout, "\n");
  cnode = WB_global_fd; 
  fprintf(stdout, "Root node is: "); 
  print_this_node();
  prompt();
  buffer_start = 0; 
  if (init_buffer[0] == '\0') {
    reload = TRUE;
    init_mode = FALSE; 
  } else { 
    reload = FALSE; 
    INT i;
    for (i = 0; init_buffer[i] != '\0'; i++) { 
      buffer[i] = init_buffer[i];
      fprintf(stdout, "%c", init_buffer[i]);
    } 
    buffer[i] = '\n';
    fprintf(stdout, "\n");
    init_mode = TRUE;  
  }
  while (TRUE) {
    if (reload) {
      load_buffer();
      reload = FALSE; 
    }
    if (ch != '.' && ch != '\n') 
      last_ch = ch; 
    ch = get_command();
    if (ch == '\n') {
      prompt();
      reload = TRUE; 
      init_mode = FALSE; 
      continue; 
    }
    fp = this_fp(WB_keymap[ch]);
    if (fp != NULL) {
      (*fp)();
    } else if (ch == 'Q' || ch == 'q') {
      cnode = NULL;
      return;
    } else {
      fprintf(stdout, "Bad character: %c\n", ch);
    }
  }  
}


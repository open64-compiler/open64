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
#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include "wn.h"
#include "wn_simp.h"
#include "strtab.h"
#include "opt_du.h"
#include "opt_alias_mgr.h"
#include "dep_graph.h"
#include "wb_util.h"
#include "wb_browser.h" 
#include "wb.h"

#if defined(TARG_NVISA) || defined(TARG_SL) || defined(TARG_X8664)
// this is in ipa, and we don't build it, so ignore
extern "C" void cg_sdebug(const char init_buffer[]) { 
	FmtAssert(FALSE, ("NYI"));
}
// this is in lno, and we don't build it, so ignore
extern "C" void s_lno_debug(const char init_buffer[]) { 
	FmtAssert(FALSE, ("NYI"));
}
#else
#pragma weak _Z11s_lno_debugPKc
#pragma weak _Z9cg_sdebugPKc
#endif

extern void s_f90_lower_debug(const char init_buffer[]); 
extern void s_omp_debug(const char init_buffer[]); 
extern void s_lno_debug(const char init_buffer[]); 
extern void s_lwr_debug(const char init_buffer[]); 
extern void s_ipl_debug(const char init_buffer[]);
extern void cg_sdebug(const char init_buffer[]);

static WB_PHASE WB_Current_Phase = WBP_NONE; 

static WB_COMMAND WBS_Command_List[] =
{
  'S', WBR_NONE, NULL, "Print info about SUMMARY_SYMBOLs",
  'I', WBR_NONE, NULL, "Print info about IVARs",
  'J', WBR_NONE, NULL, "Print info about IVAR_GLOBALs",
  'F', WBR_NONE, NULL, "Print info about SUMMARY_FORMALs",
  'G', WBR_NONE, NULL, "Print info about SUMMARY_GLOBALs",
  'A', WBR_NONE, NULL, "Print info about SUMMARY_ACTUALs",
  'V', WBR_NONE, NULL, "Print info about SUMMARY_VALUEs",
  'C', WBR_NONE, NULL, "Print info about SUMMARY_COMMONs",
  'K', WBR_NONE, NULL, "Print info about SUMMARY_COMMON_SHAPEs",
  'D', WBR_NONE, NULL, "Print info about SUMMARY_CONTROL_DEPENDENCEs",
  'L', WBR_NONE, NULL, "Print info about SUMMARY_CALLSITEs",
  'P', WBR_NONE, NULL, "Print info about SUMMARY_PROCEDUREs",
  'a', WBR_NONE, NULL, "Print info about CFG_NODE_INFOs",
  's', WBR_NONE, NULL, "Print info about SCALAR_INFOs",
  'R', WBR_NONE, NULL, "Print info about REGION_ARRAYSs",
  'r', WBR_NONE, NULL, "Print info about PROJECTED_REGIONs",
  'n', WBR_NONE, NULL, "Print info about PROJECTED_NODEs",
  't', WBR_NONE, NULL, "Print info about TERMs",
  'l', WBR_NONE, NULL, "Print info about LOOPINFOs",
  'p', WBR_NONE, NULL, "Print info about SUMMARY_PHIs",
  'x', WBR_NONE, NULL, "Print info about SUMMARY_CHIs",
  'T', WBR_NONE, NULL, "Print info about TCONs",
  'E', WBR_NONE, NULL, "Print info about SUMMARY_EXPRs",
  'X', WBR_NONE, NULL, "Print info about SUMMARY_STIDs",
  'Y', WBR_NONE, NULL, "Print info about SUMMARY_STMTs",
  'f', WBR_NONE, NULL, "Print info about SUMMARY_FEEDBACKs",
  '=', WBR_NONE, NULL, "Locate a summary at this address",
  'H', WBR_NONE, NULL, "Print this information",
  'h', WBR_NONE, NULL, "Print this information",
  '\0', WBR_NONE, NULL,  ""
};

static WB_COMMAND WB_Command_List[] =
{
  'R', WBR_NONE,    NULL, "Go to the root of the program unit",
  'N', WBR_NONE,    NULL, "Go to the next node in the subtree chain",
  'P', WBR_NONE,    NULL, "Go to the previous node in the subtree chain",
  '=', WBR_NONE,    NULL, "Set the current node to the following address",
  '@', WBR_NONE,    NULL, "Set the current node to this numbered node",
  '>', WBR_NONE,    NULL, "Increase the fanciness level for printing",
  '<', WBR_NONE,    NULL, "Decrease the fanciness level for printing",
  'K', WBR_NONE,    NULL, "Print the kids of this node",
  'S', WBR_NONE,    NULL, "Print the list of statements under this node",
  'T', WBR_NONE,    NULL, "Print the tree at the current node",
  '%', WBR_NONE,    NULL, "Print the addresses of nodes in this subtree",
  's', WBR_NONE,    NULL, "Print the symbol table entry for this node",
  't', WBR_NONE,    NULL, "Print the type table entry for this node",
  'F', WBR_NONE,    NULL, "Find nodes with this symbol in the subtree",
  '$', WBR_NONE,    NULL, "Find symbols with this name",  
  'o', WBR_NONE,    NULL, "Find nodes with this operator in the subtree",
  'U', WBR_DU,      NULL, "Print the uses at the current node",          
  'D', WBR_DU,      NULL, "Print the definitions at the current node",  
  'G', WBR_DG,      NULL, "Print the dep graph info at the current node",  
  'V', WBR_DG,      NULL, "Print the vertices of the dep graph",          
  'v', WBR_DG,      NULL, "Go to the node with this vertex number",      
  'a', WBR_ALIAS,   NULL, "Print the alias info for this node",        
  'E', WBR_NONE,    NULL, "Go to the parent of the current node",
  'e', WBR_NONE,    NULL, "Print the ancestors of this node",
  'A', WBR_AAMAP,   NULL, "Print the access info at the current node",
  'r', WBR_REDMAP,  NULL, "Print the reduction in this subtree",
  'W', WBR_NONE,    NULL, "Print tree in whirl2[fc] format at current node",
  'f', WBR_NONE,    NULL, "Change WHIRL-TO-SOURCE language to FORTRAN",
  'c', WBR_NONE,    NULL, "Change WHIRL-TO-SOURCE language to C", 
  'L', WBR_NONE,    NULL, "Sketch the loop nests in the program unit",
  'M', WBR_NONE,    NULL, "Set the current node to node with this map id",
  'm', WBR_NONE,    NULL, "Print the map-id at this node", 
  'd', WBR_NONE,    NULL, "Toggle Da Vinci mode",
  '~', WBR_NONE,    &WBS_Command_List[0], 
			  "Print summary info (Type ~H for commands)", 
  'H', WBR_NONE,    NULL, "Print this information",
  'h', WBR_NONE,    NULL, "Print this information",
  '\0', WBR_NONE,   NULL, ""
};

extern void WB_Initialize(WB_BROWSER* wb, 
			  WN* wn_global, 
			  PU* pu, 
			  DU_MANAGER* du,
                          ALIAS_MANAGER* alias_mgr)
{
  wb->Set_Global_Fd(wn_global); 
  wb->Set_Pu(pu);
  wb->Set_Du(du); 
  wb->Set_Alias_Manager(alias_mgr); 
  wb->Set_Command_List(WB_Command_List); 
} 

extern void WB_Terminate(WB_BROWSER* wb)
{ 
  wb->Set_Global_Fd(NULL); 
  wb->Set_Du(NULL); 
  wb->Set_Alias_Manager(NULL); 
  wb->Set_Command_List(NULL); 
} 

extern WB_PHASE WB_Phase() 
{ 
  return WB_Current_Phase; 
} 

extern void WB_Set_Phase(WB_PHASE WB_Phase) 
{ 
  WB_Current_Phase = WB_Phase; 
} 

extern void sdebug(const char init_buffer[])
{
  switch(WB_Current_Phase) { 
  case WBP_F90_LOWER: 
    s_f90_lower_debug(init_buffer);
    break;
  case WBP_OMP_PRELOWER: 
    s_omp_debug(init_buffer); 
    break;
  case WBP_LNO: 
    s_lno_debug(init_buffer); 
    break; 
  case WBP_LOWER: 
    s_lwr_debug(init_buffer); 
    break;
  case WBP_IPL:
    s_ipl_debug(init_buffer);
    break; 
  case WBP_IPA: 
    cg_sdebug(init_buffer);
    break;
  default: 
    fprintf(stdout, "Whirl browser not valid in this phase.\n");
    break; 
  } 
} 

extern void swb(char init_buffer[])
{
  sdebug(init_buffer); 
} 

extern void debug()
{ 
  sdebug(""); 
} 

extern void wb()
{
  debug();
}    

extern void debug_root(WN* wn_root) 
{ 
  WB_BROWSER wb;
  WB_Initialize(&wb, wn_root, NULL, NULL, NULL);
  wb.Sdebug("");
  WB_Terminate(&wb);
}

extern void wb_root(WN* wn_root)
{ 
  debug_root(wn_root);
} 


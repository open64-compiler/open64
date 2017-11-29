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
#include <stdio.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>
#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include "wn.h"
#include "wn_map.h"
#include "wn_util.h"
#include "dwarf_DST.h"
#include "ipc_file.h"
#include "wb_util.h"
#include "cg_browser.h"
#include "cgb.h"

extern void s_cg_ipa_debug(const char init_buffer[]);

static CGB_PHASE CGB_Current_Phase = CGBP_NONE; 

static CGB_COMMAND CGBS_Command_List[] = 
{ 
  'S', CGBR_NONE, NULL, "Print info about SUMMARY_SYMBOLs",
  'I', CGBR_NONE, NULL, "Print info about IVARs",
  'F', CGBR_NONE, NULL, "Print info about SUMMARY_FORMALs", 
  'G', CGBR_NONE, NULL, "Print info about SUMMARY_GLOBALs",
  'A', CGBR_NONE, NULL, "Print info about SUMMARY_ACTUALs", 
  'V', CGBR_NONE, NULL, "Print info about SUMMARY_VALUEs", 
  'C', CGBR_NONE, NULL, "Print info about SUMMARY_COMMONs", 
  'K', CGBR_NONE, NULL, "Print info about SUMMARY_COMMON_SHAPEs", 
  'D', CGBR_NONE, NULL, "Print info about SUMMARY_CONTROL_DEPENDENCEs",
  'L', CGBR_NONE, NULL, "Print info about SUMMARY_CALLSITEs",
  'P', CGBR_NONE, NULL, "Print info about SUMMARY_PROCEDUREs", 
  'a', CGBR_NONE, NULL, "Print info about CFG_NODE_INFOs", 
  's', CGBR_NONE, NULL, "Print info about SCALAR_INFOs", 
  'R', CGBR_NONE, NULL, "Print info about REGION_ARRAYSs", 
  'r', CGBR_NONE, NULL, "Print info about PROJECTED_REGIONs", 
  'n', CGBR_NONE, NULL, "Print info about PROJECTED_NODEs", 
  't', CGBR_NONE, NULL, "Print info about TERMs", 
  'l', CGBR_NONE, NULL, "Print info about LOOPINFOs", 
  'p', CGBR_NONE, NULL, "Print info about SUMMARY_PHIs",
  'x', CGBR_NONE, NULL, "Print info about SUMMARY_CHIs",
  'T', CGBR_NONE, NULL, "Print info about TCONs", 
  'E', CGBR_NONE, NULL, "Print info about SUMMARY_EXPRs", 
  'X', CGBR_NONE, NULL, "Print info about SUMMARY_STIDs", 
  'Y', CGBR_NONE, NULL, "Print info about SUMMARY_STMTs", 
  'f', CGBR_NONE, NULL, "Print info about SUMMARY_FEEDBACKs", 
  '=', CGBR_NONE, NULL, "Locate a summary at this address",
  'H', CGBR_NONE, NULL, "Print this information", 
  'h', CGBR_NONE, NULL, "Print this information", 
  '\0', CGBR_NONE, NULL,  ""
};

static CGB_COMMAND CGB_Command_List[] = 
{ 
  'R', CGBR_NONE, NULL, "Print the roots of call graph", 
  'G', CGBR_NONE, NULL, "Print the call graph",  
  'C', CGBR_NONE, NULL, "Print the callers of this node",
  'c', CGBR_NONE, NULL, "Print the callees of this node",
  'E', CGBR_NONE, NULL, "Print the edges in to this node",
  'e', CGBR_NONE, NULL, "Print the edges out of this node",
  'M', CGBR_NONE, NULL, "Print Mod/Ref info at this node",
  'V', CGBR_NONE, NULL, "Print Values for execution cost at this node",
  'X', CGBR_NONE, NULL, "Print Expressions for execution cost at this node",
  'P', CGBR_NONE, NULL, "Print Constant Propagation info at this node",
  '=', CGBR_NONE, NULL, "Set the current node to the following address",
  'v', CGBR_NONE, NULL, "Set the current vertex to the following number",
  '@', CGBR_NONE, NULL, "Set the current node to this numbered node", 
  'F', CGBR_NONE, NULL, "Find the node with the given name",  
  'W', CGBR_NONE, NULL, "Enter whirl browser at current node", 
  'd', CGBR_NONE, NULL, "Toggle Da Vinci mode", 
  'S', CGBR_NONE, &CGBS_Command_List[0], 
    "Print summary info (Type SH for commands)", 
  's', CGBR_NONE, NULL, "Print the symbol table entry for this node",
  't', CGBR_NONE, NULL, "Print the type table entry for this node",
  '<', CGBR_NONE, NULL, "Increase the fanciness level for printing",
  '>', CGBR_NONE, NULL, "Decrease the fanciness level for printing",
  'H', CGBR_NONE, NULL, "Print this information", 
  'h', CGBR_NONE, NULL, "Print this information", 
  '\0', CGBR_NONE, NULL,   ""
};

extern void CGB_Set_Phase(CGB_PHASE CGB_Phase) 
{
  CGB_Current_Phase = CGB_Phase; 
} 

extern void CGB_Initialize(CG_BROWSER* cgb, 
			   IPA_CALL_GRAPH* ipa_cg)
{
  cgb->Set_Ipa_Cg(ipa_cg);
  cgb->Set_Command_List(CGB_Command_List);
} 

extern void CGB_Terminate(CG_BROWSER* cgb)
{
  cgb->Set_Ipa_Cg(NULL);
}

extern "C" void cg_sdebug(const char init_buffer[])
{
  switch (CGB_Current_Phase) {
  case CGBP_IPA:
    s_cg_ipa_debug(init_buffer);
    break; 
  default:
    fprintf(stdout, "Call graph browser not valid in this phase.\n");
    break;
  } 
}

extern void scgb(const char init_buffer[])
{
  cg_sdebug(init_buffer);
} 

extern void cg_debug()
{
  cg_sdebug("");
}

extern void cgb()
{ 
  cg_debug();
} 

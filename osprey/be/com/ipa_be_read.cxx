/*
 Copyright (C) 2010, Hewlett-Packard Development Company, L.P.
 All Rights Reserved.

 Open64 is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 Open64 is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA.
*/
#include <sys/elf_whirl.h>
#include "cxx_template.h"  // DYN_ARRAY
#include "pu_info.h"
#include "mempool.h"
#include "wn.h"
#include "wn_map.h"
#include "opt_defs.h"
#include "ir_bread.h"
#include "ir_bwrite.h"
#include "ir_bcom.h"
#include "ipa_be_read.h"
#include "ipa_be_summary.h"

IPA_BE_SUMMARY CURRENT_BE_SUMMARY(Malloc_Mem_Pool);

void
IPA_read_alias_summary(void* fhandle, PU_Info* pu_info, MEM_POOL* mem_pool)
{
  OFFSET_AND_SIZE shdr = get_section(fhandle, SHT_MIPS_WHIRL, WT_IPA_SUMMARY);
  if (shdr.offset == 0)
    return;

  
  
  const char* base = (char*) fhandle + shdr.offset;

  NEW_BE_SUMMARY_HEADER* header = 
    (NEW_BE_SUMMARY_HEADER*) (base + *((Elf64_Word*) base));

  if (Get_Trace(TP_ALIAS,NYSTROM_SUMMARY_FLAG))
    fprintf(stderr, "BE: cg_nodes = %d, cg_stinfos = %d, cg_callsites = %d,"
            " cg_nodeids = %d, cg_modranges = %d, siloed_refereces = %d\n",
            header->GetCGNodesSize(), 
            header->GetCGStInfosSize(),
            header->GetCGCallsitesSize(),
            header->GetCGNodeIdsSize(),
            header->GetCGModRangesSize(),
            header->GetSiloedReferencesSize());


  CURRENT_BE_SUMMARY.SetCGNodesCount(header->GetCGNodesSize());
  CURRENT_BE_SUMMARY.SetCGStInfosCount(header->GetCGStInfosSize());
  CURRENT_BE_SUMMARY.SetPUHdrsCount(header->GetProcHeadersSize());

  CURRENT_BE_SUMMARY.SetProcHeadersArray
    ((SUMMARY_CONSTRAINT_GRAPH_PU_HEADER*) (base + 
                                            header->GetProcHeadersOffset()));
  CURRENT_BE_SUMMARY.SetCGNodesArray
    ((SUMMARY_CONSTRAINT_GRAPH_NODE*) (base + header->GetCGNodesOffset()));
  CURRENT_BE_SUMMARY.SetCGStInfosArray
    ((SUMMARY_CONSTRAINT_GRAPH_STINFO*) (base + header->GetCGStInfosOffset()));
  CURRENT_BE_SUMMARY.SetCGCallsitesArray
    ((SUMMARY_CONSTRAINT_GRAPH_CALLSITE*) (base + 
                                           header->GetCGCallsitesOffset()));
  CURRENT_BE_SUMMARY.SetCGNodeIdsArray
    ((UINT32*) (base + header->GetCGNodeIdsOffset()));
  CURRENT_BE_SUMMARY.SetCGModRangesArray
    ((SUMMARY_CONSTRAINT_GRAPH_MODRANGE*) (base + 
                                           header->GetCGModRangesOffset()));
  
  CURRENT_BE_SUMMARY.SetSiloedReferenceArray
    ((SUMMARY_SILOED_REFERENCE*) (base + header->GetSiloedReferencesOffset()));

}

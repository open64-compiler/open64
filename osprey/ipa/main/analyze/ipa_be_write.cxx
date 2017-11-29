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
#include "ir_bwrite.h"
#include "ir_bcom.h"
#include "ipa_be_write.h"
#include "ipa_be_summary.h"
#include "ipa_nystrom_alias_analyzer.h"
#include "be_ipa_util.h"
#include "ipa_pcg.h"

void
IPA_Summary_ProcessPointsToSet(DYN_ARRAY<UINT32>* cg_nodeids,
                               SUMMARY_CONSTRAINT_GRAPH_NODE* sumCGNode,
                               const PointsTo &gbl,
                               const PointsTo &hz,
                               const PointsTo &dn,
                               mUINT32 &numNodeIds)
{
  // Process GBLs
  UINT32 numGBLids = 0;
  for (PointsTo::SparseBitSetIterator iter(&gbl, 0); iter != 0; ++iter) {
    CGNodeId id = *iter;
    INT new_idx = cg_nodeids->Newidx();
    (*cg_nodeids)[new_idx] = id;
    numGBLids++;
  }  
  sumCGNode->numBitsPtsGBL(numGBLids);
  sumCGNode->ptsGBLidx(cg_nodeids->Lastidx() - numGBLids + 1);
  // Process HZs
  UINT32 numHZids = 0;
  for (PointsTo::SparseBitSetIterator iter(&hz, 0); iter != 0; ++iter) {
    CGNodeId id = *iter;
    INT new_idx = cg_nodeids->Newidx();
    (*cg_nodeids)[new_idx] = id;
    numHZids++;
  }  
  sumCGNode->numBitsPtsHZ(numHZids);
  sumCGNode->ptsHZidx(cg_nodeids->Lastidx() - numHZids + 1);
  // Process DNs
  UINT32 numDNids = 0;
  for (PointsTo::SparseBitSetIterator iter(&dn, 0); iter != 0; ++iter) {
    CGNodeId id = *iter;
    INT new_idx = cg_nodeids->Newidx();
    (*cg_nodeids)[new_idx] = id;
    numDNids++;
  }  
  sumCGNode->numBitsPtsDN(numDNids);
  sumCGNode->ptsDNidx(cg_nodeids->Lastidx() - numDNids + 1);
  numNodeIds += numGBLids + numHZids + numDNids;
}

UINT32
IPA_Summary_ProcessModRange(DYN_ARRAY<SUMMARY_CONSTRAINT_GRAPH_MODRANGE> 
                            *cg_modranges, ModulusRange* mr)
{
  INT new_idx = cg_modranges->Newidx();
  (*cg_modranges)[new_idx].Init();
  SUMMARY_CONSTRAINT_GRAPH_MODRANGE *summMR = &(*cg_modranges)[new_idx];

  INT idx = cg_modranges->Lastidx();
  summMR->startOffset(mr->startOffset());
  summMR->endOffset(mr->endOffset());
  summMR->modulus(mr->mod());
#ifdef Is_True_On
  summMR->ty_idx(mr->ty_idx());
#endif
  if (mr->child())
    summMR->childIdx(IPA_Summary_ProcessModRange(cg_modranges, mr->child()));
  if (mr->next())
    summMR->nextIdx(IPA_Summary_ProcessModRange(cg_modranges, mr->next()));
  return idx;  
}


#define BE_SUMMARY_HEADER_ADDR(offset) \
  ((NEW_BE_SUMMARY_HEADER*)(fl->map_addr + offset))

#define HEADER_ADDR(offset) \
 ((Elf64_Word*)(fl->map_addr + offset))

#define HEADER_OFFSET(offset) \
  ((Elf64_Word*)(fl->map_addr + offset))



// This routine is used to write the Nystrom Alias Analysis results,
// including the new Mod-Ref info based on NAA.  The output file in
// this case is #.I and only information relevant to the PUs in the
// output file are written out
void
IPA_write_alias_summary (PU_Info* pu_info_tree, Output_File *fl)
{
    Section *cur_section;

    cur_section = get_section(WT_IPA_SUMMARY, MIPS_WHIRL_SUMMARY, fl);

    fl->file_size = ir_b_align(fl->file_size, sizeof(mINT64), 0);
    cur_section->shdr.sh_offset = fl->file_size;
    
    IPA_irb_write_nystrom_alias_info(pu_info_tree, fl);

    cur_section->shdr.sh_size = fl->file_size - cur_section->shdr.sh_offset;
    cur_section->shdr.sh_addralign = sizeof(mINT64);
}

UINT64 
IPA_irb_adjust_cg_st_idx(UINT64 orig_idx, IPA_NODE* cur_node)
{
  UINT16 file_idx = (orig_idx >> 48);
  UINT16 pu_idx = (orig_idx >> 32) & 0x0000ffff;
  if ((file_idx == (UINT16) cur_node->File_Index()) && 
      (pu_idx == (UINT16) cur_node->Proc_Info_Index()))
    return (orig_idx & 0x00000000ffffffffLL);

  file_idx++;
  pu_idx++;
  UINT32 filePUIdx = (file_idx << 16) | pu_idx;
  return (((UINT64)filePUIdx) << 32) | (orig_idx & 0x00000000ffffffffLL);
}

static void 
Flat_Pu_Info_Trees(PU_Info* pu_info_tree, DYN_ARRAY<PU_Info*> *pu_infos)
{
  for (PU_Info* cur_pu = pu_info_tree; cur_pu != NULL;
     cur_pu = PU_Info_next(cur_pu)) 
  {
    pu_infos->AddElement(cur_pu);
    if(PU_Info_child(cur_pu)) 
    {
      Flat_Pu_Info_Trees(PU_Info_child(cur_pu), pu_infos);
    }
  }
}

void
IPA_irb_write_nystrom_alias_info(PU_Info* pu_info_tree, Output_File *fl)
{
  IPA_NystromAliasAnalyzer *naa =
    static_cast<IPA_NystromAliasAnalyzer*>(IPA_NystromAliasAnalyzer::aliasAnalyzer());
  if (!naa) return;

  // TODO!!!!! Iterate over all
  DYN_ARRAY<SUMMARY_CONSTRAINT_GRAPH_PU_HEADER> cg_headers(Malloc_Mem_Pool);
  DYN_ARRAY<SUMMARY_CONSTRAINT_GRAPH_NODE> cg_nodes(Malloc_Mem_Pool);
  DYN_ARRAY<SUMMARY_CONSTRAINT_GRAPH_STINFO> cg_stinfos(Malloc_Mem_Pool);
  DYN_ARRAY<SUMMARY_CONSTRAINT_GRAPH_CALLSITE> cg_callsites(Malloc_Mem_Pool);
  DYN_ARRAY<UINT32> cg_nodeids(Malloc_Mem_Pool);
  DYN_ARRAY<SUMMARY_CONSTRAINT_GRAPH_MODRANGE> cg_modranges(Malloc_Mem_Pool);
  
  DYN_ARRAY<SUMMARY_SILOED_REFERENCE> siloed_references(Malloc_Mem_Pool);

  // flatten the hierachy put_info_tree then, we can use the loop to iterate.
  // otherwise we need pass all DYN_Arrays.
  DYN_ARRAY<PU_Info*> pu_infos(Malloc_Mem_Pool);
  Flat_Pu_Info_Trees(pu_info_tree, &pu_infos);
  
  for (INT pu_idx = 0; pu_idx <= pu_infos.Lastidx();  pu_idx++) 
  {
    PU_Info* cur_pu = pu_infos[pu_idx];
    IPA_NODE* ipa_node = Get_Node_From_PU(cur_pu);
    
    ConstraintGraph* cg = naa->cg(ipa_node->Node_Index());

    if (Get_Trace(TP_ALIAS,NYSTROM_SUMMARY_FLAG))
      fprintf(stderr, "writing summary for %s\n", cg->name());

    mUINT32 num_cgnodes = 0;
    mUINT32 num_stinfos = 0;
    mUINT32 num_callsites = 0;
    mUINT32 num_nodeids = 0;

    mUINT32 num_siloedrefs = 0;
  
    INT hdr_idx = cg_headers.Newidx();
    cg_headers[hdr_idx].Init();
    if (Get_Trace(TP_ALIAS,NYSTROM_SUMMARY_FLAG))
      fprintf(stderr, "cur_hdr idx = %d\n", hdr_idx);
    SUMMARY_CONSTRAINT_GRAPH_PU_HEADER* cur_hdr = &(cg_headers[hdr_idx]);

    // collect the set of relevant constraint graph nodes
    for (CGNodeToIdMapIterator iter = cg->lBegin(); iter != cg->lEnd(); iter++)
    {
      ConstraintGraphNode* cgNode = iter->first;
      if (cgNode->checkFlags(CG_NODE_FLAGS_COLLAPSED))
      {
        // if it's a collapsed node with a missing StInfo, don't emit
        // it its parent has probably been remapped during update
        // local syms and since this collapsed node was disconnected,
        // its st idx did not get updated. this node should not be
        // queried directly in the backend, so it seems unnecessary to
        // emit its summary.
        StInfo* stInfo = cg->stInfo(cgNode->cg_st_idx());
        if (!stInfo)
        {
          if (Get_Trace(TP_ALIAS,NYSTROM_SUMMARY_FLAG))
            fprintf(stderr, "*** skipping summary for cgnode id %d\n",
                  cgNode->id());
          continue;
        }
      }
      
      INT new_idx = cg_nodes.Newidx();
      cg_nodes[new_idx].Init();
      SUMMARY_CONSTRAINT_GRAPH_NODE *summCGNode = &(cg_nodes[new_idx]);

      ConstraintGraphNode* parent = cgNode->parent();
      // We don't care about parents anymore, since the parent might
      // not be present in the local graph. So add the pts set directly
      // to the node.
      summCGNode->repParent(0);
      IPA_Summary_ProcessPointsToSet(&cg_nodeids, 
                                     summCGNode, cgNode->pointsTo(CQ_GBL),
                                     cgNode->pointsTo(CQ_HZ), 
                                     cgNode->pointsTo(CQ_DN),
                                     num_nodeids);
      summCGNode->cgNodeId(cgNode->id());
      // if this is a 'local' st, strip off the file and pu ids
      UINT64 cgstidx;
      StInfo* stInfo = cg->stInfo(cgNode->cg_st_idx());
      if (stInfo->checkFlags(CG_ST_FLAGS_GLOBAL)) {
        FmtAssert(PU_NUM_ST_IDX(cgNode->cg_st_idx()) == 0 &&
                  FILE_NUM_ST_IDX(cgNode->cg_st_idx()) == 0, 
                  ("file/pu should be 0 for globals"));
        cgstidx = cgNode->cg_st_idx();
      } else
        cgstidx = IPA_irb_adjust_cg_st_idx(cgNode->cg_st_idx(), ipa_node);
      summCGNode->cg_st_idx(cgstidx);
      summCGNode->offset(cgNode->offset());
      summCGNode->flags(cgNode->flags());
      summCGNode->inKCycle(cgNode->inKCycle());
      if (cgNode->nextOffset())
        summCGNode->nextOffset(cgNode->nextOffset()->id());
      else
        summCGNode->nextOffset(0);
      if (cgNode->checkFlags(CG_NODE_FLAGS_COLLAPSED))
      {
        CGNodeId cpid = cgNode->collapsedParent();
        ConstraintGraphNode* cp = cg->cgNode(cpid);
        while (cp && (cp->checkFlags(CG_NODE_FLAGS_COLLAPSED)))
        {
          cpid = cp->collapsedParent();
          cp = cg->cgNode(cpid);
        }
        summCGNode->collapsedParent(cpid);
      }

      num_cgnodes++;
    }

    // StInfos
    CGStInfoMap& stInfos = cg->stInfoMap();
    for (CGStInfoMapIterator stiter = stInfos.begin(); stiter != stInfos.end();
         stiter++)
    {
      CG_ST_IDX cg_st_idx = stiter->first;
      StInfo *s = stiter->second;
      if (s->firstOffset() == ConstraintGraph::blackHole())
        continue;

      INT new_idx = cg_stinfos.Newidx();
      cg_stinfos[new_idx].Init();
      SUMMARY_CONSTRAINT_GRAPH_STINFO *summStInfo = &(cg_stinfos[new_idx]);
      UINT64 cgstidx;
      if (s->checkFlags(CG_ST_FLAGS_GLOBAL)) {
        FmtAssert(PU_NUM_ST_IDX(cg_st_idx) == 0 &&
                  FILE_NUM_ST_IDX(cg_st_idx) == 0, 
                  ("file/pu should be 0 for globals"));
        cgstidx = cg_st_idx;
      } else
        cgstidx = IPA_irb_adjust_cg_st_idx(cg_st_idx, ipa_node);
      summStInfo->cg_st_idx(cgstidx);
      summStInfo->flags(s->flags());
      summStInfo->varSize(s->varSize());
      summStInfo->ty_idx(s->ty_idx());
      if (s->checkFlags(CG_ST_FLAGS_MODRANGE)) {
        UINT32 modRangeIdx = 
          IPA_Summary_ProcessModRange(&cg_modranges, s->modRange());
        summStInfo->modulus(modRangeIdx);
      } else
        summStInfo->modulus(s->mod());
      summStInfo->firstOffset(s->firstOffset() ? s->firstOffset()->id() : 0);
      num_stinfos++;
    }

    // Callsites
    CallSiteMap& callSites = cg->callSiteMap();
    for (CallSiteIterator citer = callSites.begin(); citer != callSites.end();
         citer++) {
      CallSite *cs = citer->second;

      INT new_idx = cg_callsites.Newidx();
      cg_callsites[new_idx].Init();
      SUMMARY_CONSTRAINT_GRAPH_CALLSITE *summCallSite = &(cg_callsites[new_idx]);

      summCallSite->id(cs->id());
      summCallSite->flags(cs->flags());
      summCallSite->actualModeled(cs->actualModeled());
      if (cs->isDirect() && !cs->isIntrinsic())
        summCallSite->st_idx(cs->st_idx());// TODO: adjust?
      else if (cs->isIndirect())
        summCallSite->cgNodeId(cs->cgNodeId());
      else if (cs->isIntrinsic())
        summCallSite->intrinsic(cs->intrinsic());
      // Process params
      list<CGNodeId> parms = cs->parms();
      UINT32 pcount = 0;
      list<CGNodeId>::iterator iter;
      for (iter = parms.begin(); iter != parms.end(); iter++) {
        CGNodeId id = *iter;
        INT new_idx = cg_nodeids.Newidx();
        cg_nodeids[new_idx] = id;
        pcount++;
      }
      summCallSite->numParms(pcount);
      summCallSite->parmNodeIdx(cg_nodeids.Lastidx() - 
                                pcount + 1);
      summCallSite->returnId(cs->returnId());
      num_callsites++;
      num_nodeids += pcount;
    }

    // Formal parameters/returns
    list<CGNodeId>::iterator iter;
    list<CGNodeId> parameters = cg->parameters();
    list<CGNodeId> returns = cg->returns();
    // Add the cgnode ids of formal parameters
    mUINT32 param_count = 0;
    for (iter = parameters.begin(); iter != parameters.end(); iter++) {
      INT new_idx = cg_nodeids.Newidx();
      cg_nodeids[new_idx] = *iter;
      param_count++;
    }
    cur_hdr->cgFormalsIdx(cg_nodeids.Lastidx() - param_count + 1);
    cur_hdr->cgFormalsCount(param_count);  
  
    // Add the cgnode ids of formal returns
    mUINT32 rcount = 0;
    for (iter = returns.begin(); iter != returns.end(); iter++) {
      INT new_idx = cg_nodeids.Newidx();
      cg_nodeids[new_idx] = *iter;
      rcount++;
    }
    cur_hdr->cgReturnsIdx(cg_nodeids.Lastidx() - rcount + 1);
    cur_hdr->cgReturnsCount(rcount);

    num_nodeids += param_count + rcount;

    // now set the rest of the indices and counts in the proc header
    cur_hdr->stIdx(PU_Info_proc_sym(cur_pu));
    cur_hdr->cgNodesIdx(cg_nodes.Lastidx() - num_cgnodes + 1);
    cur_hdr->cgNodesCount(num_cgnodes);

    cur_hdr->cgStInfosIdx(cg_stinfos.Lastidx() - num_stinfos + 1);
    cur_hdr->cgStInfosCount(num_stinfos);

    cur_hdr->cgCallsitesIdx(cg_callsites.Lastidx() - num_callsites + 1);
    cur_hdr->cgCallsitesCount(num_callsites);

    cur_hdr->cgNodeIdsIdx(cg_nodeids.Lastidx() - num_nodeids + 1);
    cur_hdr->cgNodeIdsCount(num_nodeids);

    // Siloed refrences
    const ALIAS_TAG_SET &siloed_tag_set =
  		  IPA_Concurrency_Graph->Get_siloed_references(ipa_node);

    for(ALIAS_TAG_SET::iterator it = siloed_tag_set.begin();
  		  it != siloed_tag_set.end(); it++) {
  	  AliasTag tag = *it;
  	  num_siloedrefs++;

  	  INT new_idx = siloed_references.Newidx();
  	  siloed_references[new_idx].Init();
  	  SUMMARY_SILOED_REFERENCE *summSiloedRef = &(siloed_references[new_idx]);

  	  summSiloedRef->aliasTag(tag);
    }

    cur_hdr->siloedReferenceIdx(siloed_references.Lastidx() - num_siloedrefs + 1);
    cur_hdr->siloedReferenceCount(num_siloedrefs);

    if (Get_Trace(TP_ALIAS,NYSTROM_SUMMARY_FLAG))
      fprintf(stderr,"xxx nodes = %d, stinfos = %d, callsites = %d, nodeids = %d\n",
              cur_hdr->cgNodesCount(), cur_hdr->cgStInfosCount(),
              cur_hdr->cgCallsitesCount(), cur_hdr->cgNodeIdsCount());

  }

  INT offset, header_loc;
  
  INT offset_pu_headers = 0;
  INT offset_cg_nodes = 0;
  INT offset_cg_stinfos = 0;
  INT offset_cg_callsites = 0;
  INT offset_cg_nodeids = 0;
  INT offset_cg_modranges = 0;
  
  INT offset_siloed_references = 0;

  Elf64_Word temp;
  
  INT cur_sec_disp = fl->file_size;
  
  // store the offset of the header structure in this field
  header_loc = (INT) ir_b_save_buf(&temp, sizeof(Elf64_Word),
                                   sizeof(INT64),0,fl);

  // write the pu headers
  INT size = 
    (cg_headers.Lastidx() + 1) * sizeof(SUMMARY_CONSTRAINT_GRAPH_PU_HEADER);
  offset_pu_headers = 
    (INT) ir_b_save_buf(&(cg_headers[0]), size, sizeof(INT64), 0, fl);
  offset_pu_headers = offset_pu_headers - cur_sec_disp;

  // write the nodes
  size = 
    (cg_nodes.Lastidx() + 1) * sizeof(SUMMARY_CONSTRAINT_GRAPH_NODE);
  offset_cg_nodes = 
    (INT) ir_b_save_buf(&(cg_nodes[0]), size, sizeof(INT64), 0, fl);
  offset_cg_nodes = offset_cg_nodes - cur_sec_disp;

  // write the StInfos
  size = 
    (cg_stinfos.Lastidx() + 1) * sizeof(SUMMARY_CONSTRAINT_GRAPH_STINFO);
  offset_cg_stinfos = 
    (INT) ir_b_save_buf(&(cg_stinfos[0]), size, sizeof(INT64), 0, fl);
  offset_cg_stinfos = offset_cg_stinfos - cur_sec_disp;

  // write the CallSites
  size = 
    (cg_callsites.Lastidx() + 1) * sizeof(SUMMARY_CONSTRAINT_GRAPH_CALLSITE);
  offset_cg_callsites = 
    (INT) ir_b_save_buf(&(cg_callsites[0]), size, sizeof(INT64), 0, fl);
  offset_cg_callsites = offset_cg_callsites - cur_sec_disp;

  // write the points-to node_ids
  size = (cg_nodeids.Lastidx() + 1) * sizeof(UINT32);
  offset_cg_nodeids =
    (INT) ir_b_save_buf(&(cg_nodeids[0]), size, sizeof(INT64), 0, fl);
  offset_cg_nodeids = offset_cg_nodeids - cur_sec_disp;

  // write the mod ranges
  size = 
    (cg_modranges.Lastidx() + 1) * sizeof(SUMMARY_CONSTRAINT_GRAPH_MODRANGE);
  offset_cg_modranges = 
    (INT) ir_b_save_buf(&(cg_modranges[0]), size, sizeof(INT64), 0, fl);
  offset_cg_modranges = offset_cg_modranges - cur_sec_disp;

  // write the siloed references
  size =
    (siloed_references.Lastidx() + 1) * sizeof(SUMMARY_SILOED_REFERENCE);
  offset_siloed_references =
    (INT) ir_b_save_buf(&(siloed_references[0]), size, sizeof(INT64), 0, fl);
  offset_siloed_references = offset_siloed_references - cur_sec_disp;

  // now update the pointer to the header and the actual header
  NEW_BE_SUMMARY_HEADER header;
  offset = (INT) ir_b_save_buf(&header, sizeof(NEW_BE_SUMMARY_HEADER),
                               sizeof(INT64), 0, fl);
  *(HEADER_ADDR(header_loc)) = offset - cur_sec_disp;
  NEW_BE_SUMMARY_HEADER* header_addr = BE_SUMMARY_HEADER_ADDR(offset);

  header_addr->SetProcHeadersOffset(offset_pu_headers);
  header_addr->SetCGNodesOffset(offset_cg_nodes);
  header_addr->SetCGStInfosOffset(offset_cg_stinfos);
  header_addr->SetCGNodeIdsOffset(offset_cg_nodeids);
  header_addr->SetCGCallsitesOffset(offset_cg_callsites);
  header_addr->SetCGModRangesOffset(offset_cg_modranges);

  header_addr->SetSiloedReferencesOffset(offset_siloed_references);

  header_addr->SetProcHeadersSize(cg_headers.Lastidx() + 1);
  header_addr->SetCGNodesSize(cg_nodes.Lastidx() + 1);
  header_addr->SetCGStInfosSize(cg_stinfos.Lastidx() + 1);
  header_addr->SetCGCallsitesSize(cg_callsites.Lastidx() + 1);
  header_addr->SetCGNodeIdsSize(cg_nodeids.Lastidx() + 1);
  header_addr->SetCGModRangesSize(cg_modranges.Lastidx() + 1);

  header_addr->SetSiloedReferencesSize(siloed_references.Lastidx() + 1);

  if (Get_Trace(TP_ALIAS,NYSTROM_SUMMARY_FLAG))
    fprintf(stderr, "cg_nodes = %d, cg_stinfos = %d, cg_callsites = %d,"
            " cg_nodeids = %d cg_modranges = %d\n", 
            header_addr->GetCGNodesSize(), 
            header_addr->GetCGStInfosSize(),
            header_addr->GetCGCallsitesSize(),
            header_addr->GetCGNodeIdsSize(),
            header_addr->GetCGModRangesSize());
}

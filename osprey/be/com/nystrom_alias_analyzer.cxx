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
#include <stdio.h>
#include "be_util.h"
#include "cxx_memory.h"
#include "opt_wn.h"
#include "wn_util.h"
#include "nystrom_alias_analyzer.h"

#include "pu_info.h"
#include "be_ipa_util.h"
#include "ipa_be_summary.h"
#include "ipa_be_read.h"
#include "config_opt.h"

extern BOOL Write_ALIAS_CGNODE_Map;

PointsTo NystromAliasAnalyzer::emptyPointsToSet;

// post IPA is true, because it has the post IPAA alias info.
NystromAliasAnalyzer::NystromAliasAnalyzer(ALIAS_CONTEXT &ac)
  : AliasAnalyzer(true),
    _nextAliasTag(InitialAliasTag),
    _isPostIPA(true)
{
  // Activate the use of the Nystrom points-to analysis by the
  // ALIAS_RULE harness and disable alias classification rules.
  ac |= ALIAS_ANALYZER_RULE;
  ac &= ~(CLAS_RULE|IP_CLAS_RULE); 
}


NystromAliasAnalyzer::NystromAliasAnalyzer(ALIAS_CONTEXT &ac, WN* entryWN,
                                           bool backend)
  : AliasAnalyzer(),
    _nextAliasTag(InitialAliasTag),
    _isPostIPA(true)
{
  // Activate the use of the Nystrom points-to analysis by the
  // ALIAS_RULE harness and disable alias classification rules.
  ac |= ALIAS_ANALYZER_RULE;
  ac &= ~(CLAS_RULE|IP_CLAS_RULE);

  // Create ConstraintGraph from the summary information generated at IPA
  _constraintGraph = CXX_NEW(ConstraintGraph((WN*) NULL, &_memPool), 
                             &_memPool);

#if 0
  if (Get_Trace(TP_ALIAS,NYSTROM_CG_POST_FLAG)) {
    fprintf(stderr,"Nystrom IPA BE...\n");
    fprintf(stderr, "Printing ConstraintGraph\n");
    _constraintGraph->print(stderr);
  }
#endif

  // Map WNs to AliasTags
  createAliasTags(entryWN);

  if(backend)
    _constraintGraph->map_blk_Section_STs();

  // Set flag to dump the WN to CGNodeId map during Write_PU_Info
  Write_ALIAS_CGNODE_Map = TRUE;
}

NystromAliasAnalyzer::NystromAliasAnalyzer(ALIAS_CONTEXT &ac,
                                           WN *entryWN)
  : AliasAnalyzer(),
    _nextAliasTag(InitialAliasTag),
    _isPostIPA(false)
{
  // Activate the use of the Nystrom points-to analysis by the
  // ALIAS_RULE harness and disable alias classification rules.
  ac |= ALIAS_ANALYZER_RULE;
  ac &= ~(CLAS_RULE|IP_CLAS_RULE);

  _constraintGraph = CXX_NEW(ConstraintGraph(entryWN, &_memPool), &_memPool);
  if (Get_Trace(TP_ALIAS,NYSTROM_CG_PRE_FLAG)) {
    fprintf(stderr, "Printing initial ConstraintGraph\n");
    _constraintGraph->print(stderr);
    fdump_tree(stderr, entryWN);
  }

  if (Get_Trace(TP_ALIAS,NYSTROM_CG_VCG_FLAG)) {
    char buf[1024];
    sprintf(buf,"%s_initial",ST_name(WN_st(entryWN)));
    ConstraintGraphVCG::dumpVCG(buf);
  }

  _constraintGraph->simpleOptimizer();

  // Solve the constraint graph
  if (!_constraintGraph->nonIPASolver())
    return;

  if (Get_Trace(TP_ALIAS,NYSTROM_CG_VCG_FLAG)) {
    char buf[1024];
    sprintf(buf,"%s_final",ST_name(WN_st(entryWN)));
    ConstraintGraphVCG::dumpVCG(buf);
  }

  if (Get_Trace(TP_ALIAS,NYSTROM_CG_POST_FLAG)) {
    fprintf(stderr,"Nystrom analysis...complete\n");
    fprintf(stderr, "Printing final ConstraintGraph\n");
    _constraintGraph->print(stderr);
  }

  // Map WNs to AliasTags
  createAliasTags(entryWN);

  // Delete nodes that are marked deleted
  _constraintGraph->deleteOptimizedNodes();

  // Set flag to dump the WN to CGNodeId map during Write_PU_Info
  Write_ALIAS_CGNODE_Map = TRUE;
}

NystromAliasAnalyzer::~NystromAliasAnalyzer() 
{
  // Clear the static members of the ConstraintGraph
  ConstraintGraph::reset();
}

ALIAS_RESULT
NystromAliasAnalyzer::aliased(AliasTag tag1, AliasTag tag2)
{
  incrAliasQueryCount();

  if (tag1 == InvalidAliasTag || tag1 == EmptyAliasTag ||
      tag2 == InvalidAliasTag || tag2 == EmptyAliasTag ||
      tag1 == tag2)
    return POSSIBLY_ALIASED;

  // Canonicalize the queries, eventually we will be caching
  // the result rather than performing a set intersection
  if (tag1 > tag2) {
    AliasTag tmp = tag2;
    tag2 = tag1;
    tag1 = tmp;
  }

  // triage, check if this pair of alias tag is force aliased.
  if(tag1 < AA_force_tag_alias_before_dim1 ||
     (tag1 == AA_force_tag_alias_before_dim1 &&
      tag2 <= AA_force_tag_alias_before_dim2)) {
    return POSSIBLY_ALIASED;
  }

  bool result;
  if (checkQueryFile((UINT32)Current_PU_Count(),tag1,tag2,result))
    return result ? POSSIBLY_ALIASED : NOT_ALIASED;

  // First we check the query cache to determine if this query
  // has been made of this pair of tags before.  Presumably this
  // will be faster than actually performing the set intersection
  // for redundant queries and that savings will compensate for
  // the additional hash_map lookup for "new" query.
  QueryCacheKey key(tag1,tag2);
  QueryCacheIterator iter = _queryCacheMap.find(key);
  if (iter != _queryCacheMap.end()) {
    if (Get_Trace(TP_ALIAS,NYSTROM_QUERY_TRACE_FLAG))
      fprintf(TFile,"Found <%d,%d> in cache: %d\n",tag1,tag2,iter->second);
    return iter->second ? POSSIBLY_ALIASED : NOT_ALIASED;
  }

  PointsTo& ptsSet1 = pointsTo(tag1);
  PointsTo& ptsSet2 = pointsTo(tag2);

  FmtAssert(!ptsSet1.isEmpty(),
            ("Points-to set of alias tag %d is unexpectedly empty",tag1));
  FmtAssert(!ptsSet2.isEmpty(),
            ("Points-to set of alias tag %d is unexpectedly empty",tag2));
  result = ptsSet1.intersect(ptsSet2);

  // Update the query cache
  _queryCacheMap[key] = result;

  return result ? POSSIBLY_ALIASED : NOT_ALIASED;
}

/*
 * Generates an AliasTag for a provided <ST,offset,size> triple.
 * Assembles a points-to set that consists of the CGNodeIds for each
 * existing <ST,offset> .... <ST,offset+size-1> that exists.
 */
AliasTag
NystromAliasAnalyzer::genAliasTag(ST *st, INT64 offset, INT64 size, bool direct)
{
  AliasTag aliasTag = InvalidAliasTag;

  if (_constraintGraph == NULL)
    return aliasTag;

  // Scale preg offsets by Pointer_Size
  if (ST_class(st) == CLASS_PREG)
    offset *= Pointer_Size;

  if (offset < 0)
    offset = -offset;

  // First we adjust the requested offset by any modulus that
  // is being model by the constraint graph for this symbol.
  ConstraintGraph *cg;
  CG_ST_IDX cg_st_idx;
  // in IPA:preopt mode, check if st is global. Find st info in global CG.
  if (ipaMode() && StInfo::isGlobalStInfo(st)) {
    cg = ConstraintGraph::globalCG();
    cg_st_idx = CG_ST_st_idx(st);
  }
  else {
    cg = _constraintGraph;
    if (ipaMode()) {
      cg_st_idx = IPA_CG_ST_st_idx(curFilePUIdx(), st);
    }
    else {
      cg_st_idx = CG_ST_st_idx(st);
    }
  }
  StInfo *stInfo = cg->stInfo(cg_st_idx);
  if (!stInfo) {
    if(isPostIPA() && ST_class(st) == CLASS_BLOCK) {
      INT64 new_offset;
      ST* new_st = cg->Get_blk_Section_ST(st, offset, new_offset);
      if(new_st != NULL) {
        return genAliasTag(new_st, new_offset, size, direct);
      }
    }
    return aliasTag;
  }
  if (!stInfo->checkFlags(CG_ST_FLAGS_PREG))
  {
    offset = stInfo->alignOffset(stInfo->ty_idx(), offset);
    offset = stInfo->applyModulus(offset);
  }
  // First we check to see if we have been asked this question before...
  StToAliasTagKey atKey(cg_st_idx,offset,size);
  StToAliasTagMapIterator atIter = _stToAliasTagMap.find(atKey);
  if (atIter != _stToAliasTagMap.end())
    aliasTag = atIter->second;
  else {
    // Apparently we did not find a previous query.  So, we either
    // (a) find <ST,offset> exactly when size == 0
    // (b) find <ST,offset> ... <ST,offset+size-1> when size >= zero
    if (size == 0) {
      ConstraintGraphNode *node = cg->checkCGNode(cg_st_idx,offset);
      if (node) {
        node = cg->aliasedSym(node);
        aliasTag = newAliasTag();
        AliasTagInfo *aliasTagInfo = _aliasTagInfo[aliasTag];
        if (direct) {
          // Record the CGNode that materialized this aliasTag, so
          // that we can restore the mapping from WN to CGNodeIds during
          // CODEREP to WN lowering
          _aliasTagToCGNodeIdMap[aliasTag] = node->id();
          // Add self reference and black hole if required
          if (node->checkFlags(CG_NODE_FLAGS_COLLAPSED))
            aliasTagInfo->pointsTo().setBit(node->collapsedParent());
          else
            aliasTagInfo->pointsTo().setBit(node->id());
          // If the node is address taken, we must add "black hole" to
          // the points-to set to cover intersection with other escaped
          // references.  If the node is *not* address taken nothing can
          // point to it.
          if (node->stInfo()->checkFlags(CG_ST_FLAGS_ESCLOCAL) &&
              node->checkPointsTo(ConstraintGraph::blackHole(),CQ_GBL))
            aliasTagInfo->pointsTo().setBit(ConstraintGraph::blackHoleId());
        }
        else {
          aliasTagInfo->pointsTo().setUnion(node->pointsTo(CQ_GBL));
          aliasTagInfo->pointsTo().setUnion(node->pointsTo(CQ_DN));
          aliasTagInfo->pointsTo().setUnion(node->pointsTo(CQ_HZ));
        }
        _stToAliasTagMap[atKey] = aliasTag;
      }
    }
    else {
      ConstraintGraphNode *cur = stInfo->firstOffset();
      AliasTagInfo *aliasTagInfo = NULL;
      while (cur && cur->offset() < offset+size) {
        if (cur->offset() >= offset) {
          ConstraintGraphNode *node = cg->aliasedSym(cur);
          if (aliasTag == InvalidAliasTag) {
            aliasTag = newAliasTag();
            aliasTagInfo = _aliasTagInfo[aliasTag];
            // Save this result in case we are asked for it again
            _stToAliasTagMap[atKey] = aliasTag;
            // Record the CGNode that materialized this aliasTag, so
            // that we can restore the mapping from WN to CGNodeIds during
            // CODEREP to WN lowering
            if (direct)
              _aliasTagToCGNodeIdMap[aliasTag] = node->id();
          }
          if (direct) {
            // Add self reference and black hole if required
            if (node->checkFlags(CG_NODE_FLAGS_COLLAPSED))
              aliasTagInfo->pointsTo().setBit(node->collapsedParent());
            else
              aliasTagInfo->pointsTo().setBit(node->id());
            // If the node is address taken, we must add "black hole" to
            // the points-to set to cover intersection with other escaped
            // references.  If the node is *not* address taken nothing can
            // point to it.
            if (node->stInfo()->checkFlags(CG_ST_FLAGS_ESCLOCAL) &&
                node->checkPointsTo(ConstraintGraph::blackHole(),CQ_GBL))
              aliasTagInfo->pointsTo().setBit(ConstraintGraph::blackHoleId());
          }
          else {
            aliasTagInfo->pointsTo().setUnion(node->pointsTo(CQ_GBL));
            aliasTagInfo->pointsTo().setUnion(node->pointsTo(CQ_DN));
            aliasTagInfo->pointsTo().setUnion(node->pointsTo(CQ_HZ));
          }
        }
        cur = cur->nextOffset();
      }
    }
    if (aliasTag != InvalidAliasTag &&
        Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG)) {
      fprintf(stderr, "genAliasTag: new aliasTag %d for %s <%d,%d,%d> with aliasTagInfo: ",
              (UINT32)aliasTag,ST_name(ST_st_idx(st)),(INT32)ST_st_idx(st),(INT32)offset,(INT32)size);
      _aliasTagInfo[aliasTag]->print(stderr);
      fprintf(stderr, "\n");
    }
  }
  return aliasTag;
}

void
NystromAliasAnalyzer::aliasedWithCall(ST *call, AliasTag symTag,
                                      BOOL &mod, BOOL &ref)
{
  mod = ref = TRUE;
}

BOOL
NystromAliasAnalyzer::pointsToSet(AliasTag, PointsTo &)
{
  return FALSE;
}

AliasTag 
NystromAliasAnalyzer::newAliasTag(void) 
{ 
  AliasTag tag = _nextAliasTag;
  AliasTagInfo *aliasTagInfo = CXX_NEW(AliasTagInfo(&_memPool), &_memPool);
  // Associate the AliasTagInfo with the aliasTag
  _aliasTagInfo[_nextAliasTag] = aliasTagInfo;
  _nextAliasTag = (AliasTag)((UINT32)_nextAliasTag + 1);
  return tag;
}

AliasTag 
NystromAliasAnalyzer::newCallAliasTag(void) 
{ 
  AliasTag tag = _nextAliasTag;
  CallAliasTagInfo *aliasTagInfo = 
                    CXX_NEW(CallAliasTagInfo(&_memPool), &_memPool);
  // Associate the AliasTagInfo with the aliasTag
  _aliasTagInfo[_nextAliasTag] = aliasTagInfo;
  _nextAliasTag = (AliasTag)((UINT32)_nextAliasTag + 1);
  return tag;
}

// Unions the points-to set of srcTag into the points-to set of dstTag.
void
NystromAliasAnalyzer::mergePointsTo(AliasTag dstTag, AliasTag srcTag)
{
  AliasTagInfo *dstInfo = _aliasTagInfo[dstTag];
  AliasTagInfo *srcInfo = _aliasTagInfo[srcTag];

  FmtAssert(dstInfo != NULL, ("No AliasTagInfo associated with dstTag: %d\n",
            dstTag));
  FmtAssert(srcInfo != NULL, ("No AliasTagInfo associated with srcTag: %d\n",
            srcTag));

  dstInfo->pointsTo().setUnion(srcInfo->pointsTo());
}

AliasTag
NystromAliasAnalyzer::meet(AliasTag dstTag, AliasTag srcTag)
{
  if (dstTag == InvalidAliasTag || srcTag == InvalidAliasTag)
    return InvalidAliasTag;

  AliasTag retTag = dstTag;
  if (dstTag == EmptyAliasTag)
    retTag = newAliasTag();
  mergePointsTo(retTag,srcTag);
  if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG)) {
    fprintf(stderr,"meet: aliasTag %d, aliasTag %d ->",dstTag,srcTag);
    fprintf(stderr," result aliasTag %d [",retTag);
    pointsTo(retTag).print(stderr);
    fprintf(stderr,"]\n");
  }
  return retTag;
}

void
NystromAliasAnalyzer::transferAliasTag(WN *dstWN, const WN *srcWN)
{
  if (_constraintGraph == NULL)
    return;

  // First, we check the alias tag map to see if an aliasTag is
  // easily retrievable.
  AliasTag tag = getAliasTag(srcWN);
  if (tag == InvalidAliasTag) {
    // Now, provided that the target operation is one for which we
    // are interested in providing an alias tag, we will work a bit
    // harder to manufacture an aliasTag.  If the source node
    // as a CGNode, then we will create new AliasTag for the target
    // node.
    const OPCODE   opc = WN_opcode(dstWN);
    const OPERATOR opr = OPCODE_operator(opc);

    // We only consider indirects; direct references are handled
    // by genAliasTag called during Transfer_alias_tag
    if (OPERATOR_is_scalar_istore(opr) ||
        OPERATOR_is_scalar_iload(opr) ||
        opr == OPR_MSTORE ||
        opr == OPR_MLOAD)
    {
      CGNodeId id;
      // For ILOADS, the points-to set is associated with the address
      // of the iload. So get the CGNode corresponding to the address WN.
      const OPCODE   srcOpc = WN_opcode(srcWN);
      const OPERATOR srcOpr = OPCODE_operator(srcOpc);
      if (srcOpr == OPR_ILDBITS || srcOpr == OPR_MLOAD || srcOpr == OPR_ILOAD)
        id = WN_MAP_CGNodeId_Get(WN_kid0(srcWN));
      else
        id = WN_MAP_CGNodeId_Get(srcWN);

      // WN not mapped to any CGNodes
      if (id == 0)
        return;

      ConstraintGraphNode *cgNode = _constraintGraph->cgNode(id);
      FmtAssert(cgNode != NULL, ("CGNodeId : %d not mapped to a "
          "ConstraintGraphNode\n", id));

      // Any node flagged as unknown is a bad situation, we need
      // to prevent generating an AliasTag for this node
      if (cgNode->checkFlags(CG_NODE_FLAGS_UNKNOWN))
        return;

      tag = newAliasTag();
      AliasTagInfo *aliasTagInfo = _aliasTagInfo[tag];

      // Union all the points-to sets
      aliasTagInfo->pointsTo().setUnion(cgNode->pointsTo(CQ_GBL));
      aliasTagInfo->pointsTo().setUnion(cgNode->pointsTo(CQ_DN));
      aliasTagInfo->pointsTo().setUnion(cgNode->pointsTo(CQ_HZ));

      // If the points-to set of the alias tag is empty at this point then
      // either we have an escape analysis bug or an uninitialized variable.
      ConstraintGraph *cg = cgNode->cg();
      if (aliasTagInfo->pointsTo().isEmpty() &&
          !cg->stInfo(cgNode->cg_st_idx())->checkFlags(CG_ST_FLAGS_GLOBAL))
        aliasTagInfo->pointsTo().setBit(cgNode->id());

      WN_MAP_CGNodeId_Set(dstWN,id);

      if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG)) {
        fprintf(stderr, "transferAliasTag: mapping aliasTag %d to aliasTagInfo: ",
                (UINT32)tag);
        _aliasTagInfo[tag]->print(stderr);
        fprintf(stderr, "\n");
      }
    }
  }
  setAliasTag(dstWN,tag);
}

// Map the WNs to an AliasTag. Each AliasTag in turn is mapped to
// an AliasTagInfo which stores the points-to set (CGNodeIds) of the
// locations accessed by the WN
void
NystromAliasAnalyzer::createAliasTags(WN *entryWN)
{
  // if one node points to a black hole cg node.
  // it should also points to globals and escaple locals.
  PointsTo bh_points_to;
  if (!_isPostIPA) {
    for (CGNodeToIdMapIterator iter = _constraintGraph->lBegin();
         iter != _constraintGraph->lEnd(); 
         iter++) {
      ConstraintGraphNode *node = iter->first;
      StInfo *stinfo = node->stInfo();
      // in ipa mode, not a pointer doesn't have stinfo.
      // check ConstraintGraph::buildCGFromSummary
      if (stinfo && stinfo->checkFlags(CG_ST_FLAGS_GLOBAL | CG_ST_FLAGS_ESCLOCAL)) {
        bh_points_to.setBit(node->id());
      }
    }
  }
  
  for (WN_ITER *wni = WN_WALK_TreeIter(entryWN);
      wni; wni = WN_WALK_TreeNext(wni))
  {
    WN *wn = WN_ITER_wn(wni);
    const OPCODE   opc = WN_opcode(wn);
    const OPERATOR opr = OPCODE_operator(opc);

    AliasTag aliasTag = InvalidAliasTag;

    // Check if the WN's CGNodeId has been marked as deleted and remap
    if (!OPCODE_is_call(opc))
      _constraintGraph->remapDeletedNode(wn);

    // We only consider indirects; direct references are handled by genAliasTag
    // called during Transfer_alias_tag
    if (OPERATOR_is_scalar_istore(opr) ||
        OPERATOR_is_scalar_iload(opr) ||
        opr == OPR_MSTORE ||
        opr == OPR_MLOAD)
    {
      CGNodeId id;
      // For ILOADS, the points-to set is associated with the address
      // of the iload. So get the CGNode corresponding to the address WN.
      if (opr == OPR_ILDBITS || opr == OPR_MLOAD || opr == OPR_ILOAD) {
        _constraintGraph->remapDeletedNode(WN_kid0(wn));
        id = WN_MAP_CGNodeId_Get(WN_kid0(wn));
      } else
        id = WN_MAP_CGNodeId_Get(wn);

      // WN not mapped to any CGNodes
      if (id == 0)
        continue;

      ConstraintGraphNode *cgNode = ConstraintGraph::cgNode(id);
      FmtAssert(cgNode != NULL, ("CGNodeId : %d not mapped to a "
          "ConstraintGraphNode\n", id));

      // Any node flagged as unknown is a bad situation, we need
      // to prevent generating an AliasTag for this node
      if (cgNode->checkFlags(CG_NODE_FLAGS_UNKNOWN))
        continue;

      aliasTag = newAliasTag();
      AliasTagInfo *aliasTagInfo = _aliasTagInfo[aliasTag];

      // Union all the points-to sets
      if (!_isPostIPA) {
        cgNode->findRep()->postProcessPointsTo(aliasTagInfo->pointsTo());
        if (aliasTagInfo->pointsTo().isSet(ConstraintGraph::blackHoleId())) {
          aliasTagInfo->pointsTo().setUnion(bh_points_to);
        }
      }
      else {
        aliasTagInfo->pointsTo().setUnion(cgNode->pointsTo(CQ_GBL));
        aliasTagInfo->pointsTo().setUnion(cgNode->pointsTo(CQ_DN));
        aliasTagInfo->pointsTo().setUnion(cgNode->pointsTo(CQ_HZ));
      }

      // To handle corner case where we might have a null pointer read/write
      if (cgNode->checkFlags(CG_NODE_FLAGS_NOT_POINTER)) {
        if (aliasTagInfo->pointsTo().isEmpty())
          aliasTagInfo->pointsTo().setBit(ConstraintGraph::notAPointer()->id());
      }

      // If the points-to set of the alias tag is empty at this point then
      // either we have an escape analysis bug or an uninitialized variable.

      // We expect all alias sets to be non-empty...
      // FmtAssert(!aliasTagInfo->pointsTo().isEmpty(),
      //           ("Alias tag %d (from cgnode %d) has empty alias set",
      //               aliasTag,cgNode->id()));
      if (aliasTagInfo->pointsTo().isEmpty()) {
        // fprintf(stderr, "Alias tag %d (from cgnode %d) has empty alias set\n",
        //         aliasTag,cgNode->id());
        aliasTagInfo->pointsTo().setBit(ConstraintGraph::notAPointer()->id());
      }
      
      // Map aliasTags to the cgnode ids
      _aliasTagToCGNodeIdMap[aliasTag] = id;
    }
    // For calls, get the mod/ref info from the callsite
    else if (opr == OPR_ICALL || opr == OPR_VFCALL || opr == OPR_CALL)
    {
      CallSiteId id = WN_MAP_CallSiteId_Get(wn);

      if (id == 0)
        continue;

      CallSite *cs = _constraintGraph->callSite(id);
      FmtAssert(cs != NULL, ("CallSiteId : %d not mapped to a CallSite\n", id));

      // Ignore if marked UNKNOWN or has no mod/ref information
      if (cs->checkFlags(CS_FLAGS_UNKNOWN) ||
          !cs->checkFlags(CS_FLAGS_HAS_MOD_REF))
        continue;

      aliasTag = newCallAliasTag();
      CallAliasTagInfo *callAliasTagInfo =
          (CallAliasTagInfo *)_aliasTagInfo[aliasTag];

      callAliasTagInfo->mod().setUnion(cs->mod());
      callAliasTagInfo->ref().setUnion(cs->ref());
    }
    else
      continue;

    // Map the WN to the new aliasTag
    setAliasTag(wn, aliasTag);

    if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG)) {
      fprintf(stderr, "createAliasTag: mapping aliasTag %d to aliasTagInfo: ",
              (UINT32)aliasTag);
      _aliasTagInfo[aliasTag]->print(stderr);
      fprintf(stderr, "\n");
    }
  }
}

void
NystromAliasAnalyzer::print_AliasTag(AliasTag tag, FILE* file)
{
    hash_map<UINT32, AliasTagInfo *>::iterator iter = 
                                        _aliasTagInfo.find((UINT32)tag);
    if (iter == _aliasTagInfo.end()) {
        fprintf(file, "tag has no point to info\n");
        return;
    }
    
    fprintf(file, "alias tag %d: points to cg nodes ", tag); 
    _aliasTagInfo[tag]->print(file);
    fprintf(file, "\n");
}

void
NystromAliasAnalyzer::print_All_AliasTag(FILE* f)
{
    for(UINT32 tag = 0 ; tag < (UINT32)_nextAliasTag; tag++) {
        print_AliasTag((AliasTag)tag, f);
    }
}

StInfo *
ConstraintGraph::buildStInfo(SUMMARY_CONSTRAINT_GRAPH_STINFO *summ)
{
  CG_ST_IDX cg_st_idx = summ->cg_st_idx();
  
  UINT32 flags = summ->flags();
  INT64 varSize = summ->varSize();
  TY_IDX ty_idx = summ->ty_idx();
  StInfo* st_info = 
    CXX_NEW(StInfo(flags, varSize, ty_idx, _memPool), _memPool);
  if (flags & CG_ST_FLAGS_MODRANGE)
  {
    ModulusRange* mr = buildModRange(summ->modulus());
    st_info->modRange(mr);
  }
  else
  {
    UINT32 modulus = summ->modulus();
    st_info->mod(modulus);
  }
  _cgStInfoMap[cg_st_idx] = st_info;
}

ModulusRange*
ConstraintGraph::buildModRange(UINT32 modRangeIdx)
{
  INT32 size;
  SUMMARY_CONSTRAINT_GRAPH_MODRANGE* summModRanges =
    CURRENT_BE_SUMMARY.GetCGModRangesArray();
  SUMMARY_CONSTRAINT_GRAPH_MODRANGE& summMR = summModRanges[modRangeIdx];
  ModulusRange* mr = CXX_NEW(ModulusRange(summMR.startOffset(),
                                          summMR.endOffset(),
                                          summMR.modulus(),
                                          summMR.ty_idx()), _memPool);
  if (summMR.childIdx() != 0)
    mr->child(buildModRange(summMR.childIdx()));
  if (summMR.nextIdx() != 0)
    mr->next(buildModRange(summMR.nextIdx()));

  return mr;
}

ConstraintGraphNode* 
ConstraintGraph::buildCGNode(SUMMARY_CONSTRAINT_GRAPH_NODE* summ)
{
  CG_ST_IDX cg_st_idx = summ->cg_st_idx();
  ConstraintGraphNode* cgNode = checkCGNode(cg_st_idx, summ->offset());
  FmtAssert(cgNode == NULL, ("CGNode: %d already exists", cgNode->id()));

  cgNode = CXX_NEW(ConstraintGraphNode(cg_st_idx, summ->offset(),
                                       summ->flags(), summ->inKCycle(),
                                       summ->cgNodeId(), this), _memPool);
  cgNode->ty_idx(summ->ty_idx());
  // Set the collapsed parent
  if (cgNode->checkFlags(CG_NODE_FLAGS_COLLAPSED))
    cgNode->collapsedParent(summ->collapsedParent());

  cgIdToNodeMap[summ->cgNodeId()] = cgNode;
  _cgNodeToIdMap[cgNode] = summ->cgNodeId();

  nextCGNodeId = MAX(nextCGNodeId, summ->cgNodeId());
  nextCGNodeId++;
}

void
ConstraintGraph::buildCGFromSummary()
{
  // now build the constraint graph from the summary
  SUMMARY_CONSTRAINT_GRAPH_PU_HEADER* summPUHeaders = 
    CURRENT_BE_SUMMARY.GetProcHeadersArray();
  SUMMARY_CONSTRAINT_GRAPH_NODE* summCGNodes = 
    CURRENT_BE_SUMMARY.GetCGNodesArray();
  SUMMARY_CONSTRAINT_GRAPH_STINFO* summStInfos = 
    CURRENT_BE_SUMMARY.GetCGStInfosArray();
  SUMMARY_CONSTRAINT_GRAPH_CALLSITE* summCallsites = 
    CURRENT_BE_SUMMARY.GetCGCallsitesArray();
  UINT32* nodeIds =
    CURRENT_BE_SUMMARY.GetCGNodeIdsArray();
  
  // Get the header corresponding to the current PU
  SUMMARY_CONSTRAINT_GRAPH_PU_HEADER& cur_hdr = summPUHeaders[0];
  bool found = false;
  for (int i = 0; i < CURRENT_BE_SUMMARY.GetPUHdrsCount(); i++)
  {
    if (summPUHeaders[i].stIdx() == PU_Info_proc_sym(Current_PU_Info))
    {
      cur_hdr = summPUHeaders[i];
      found = true;
      break;
    }    
  }
  FmtAssert(found == true, ("constraint graph not found!"));
  if (!found) return;
    
  UINT32 nodesCount =     cur_hdr.cgNodesCount();
  UINT32 stInfosCount =   cur_hdr.cgStInfosCount();
  UINT32 callsitesCount = cur_hdr.cgCallsitesCount();
  UINT32 nodeIdsCount =   cur_hdr.cgNodeIdsCount();
  UINT32 formalsCount =   cur_hdr.cgFormalsCount();
  UINT32 returnsCount =   cur_hdr.cgReturnsCount();

  UINT32 nodesIdx =     cur_hdr.cgNodesIdx();
  UINT32 stInfosIdx =   cur_hdr.cgStInfosIdx();
  UINT32 callsitesIdx = cur_hdr.cgCallsitesIdx();
  UINT32 nodeIdsIdx =   cur_hdr.cgNodeIdsIdx();
  UINT32 formalsIdx =   cur_hdr.cgFormalsIdx();
  UINT32 returnsIdx =   cur_hdr.cgReturnsIdx();
  
  if (Get_Trace(TP_ALIAS,NYSTROM_SUMMARY_FLAG))
    fprintf(stderr, 
            "*** nodes = %d, stinfos = %d, callsites = %d, nodeids = %d\n",
            nodesCount, stInfosCount,callsitesCount, nodeIdsCount);

  // add the STInfos
  for (UINT32 i = 0; i < stInfosCount; i++)
  {
    SUMMARY_CONSTRAINT_GRAPH_STINFO& summStInfo = summStInfos[stInfosIdx + i];
    if (summStInfo.firstOffset() == notAPointer()->id())
      continue;
    StInfo * stInfo = buildStInfo(&summStInfo);
  }

  // add ConstraintGraphNodes
  for (UINT32 i = 0; i < nodesCount; i++)
  {
    SUMMARY_CONSTRAINT_GRAPH_NODE& summNode = summCGNodes[nodesIdx + i];
    ConstraintGraphNode* cg_node = buildCGNode(&summNode);
  } 

  for (UINT32 i = 0; i < stInfosCount; i++)
  {
    SUMMARY_CONSTRAINT_GRAPH_STINFO& summStInfo = summStInfos[stInfosIdx + i];
    if (summStInfo.firstOffset() == notAPointer()->id())
      continue;
    UINT32 firstOffsetId = summStInfo.firstOffset();
    if (firstOffsetId != 0) {
      StInfo* stinfo = _cgStInfoMap.find(summStInfo.cg_st_idx())->second;
      FmtAssert(!stinfo->checkFlags(CG_ST_FLAGS_PREG),
                ("PREGs should have no firstOffset"));
      ConstraintGraphNode * firstOffset = cgNode(firstOffsetId);
      FmtAssert(firstOffset != NULL, ("Expecting firstOffset"));
      bool added = addCGNodeInSortedOrder(stinfo, firstOffset);
      if (added)
        stinfo->incrNumOffsets();
    }
  }
  
  for (UINT32 i = 0; i < nodesCount; i++)
  {
    SUMMARY_CONSTRAINT_GRAPH_NODE& summNode = summCGNodes[nodesIdx + i];
    if (summNode.cgNodeId() == notAPointer()->id())
      continue;
    ConstraintGraphNode* cgnode = cgNode(summNode.cgNodeId());
    UInt32 nextOffsetId = summNode.nextOffset();
    if (nextOffsetId != 0)
    {
      StInfo* stinfo = _cgStInfoMap.find(summNode.cg_st_idx())->second;
      FmtAssert(!stinfo->checkFlags(CG_ST_FLAGS_PREG),
                ("PREGs should have no nextOffset"));
      ConstraintGraphNode* nextOffset = cgNode(nextOffsetId);
      FmtAssert(nextOffset != NULL, ("Expecting nextOffset"));
      bool added = addCGNodeInSortedOrder(stinfo, nextOffset);
      if (added)
        stinfo->incrNumOffsets();
    }
    // Add the pts set
    UINT32 numBitsPtsGBL = summNode.numBitsPtsGBL();
    UINT32 numBitsPtsHZ  = summNode.numBitsPtsHZ();
    UINT32 numBitsPtsDN  = summNode.numBitsPtsDN();
    UINT32 ptsGBLidx     = summNode.ptsGBLidx();
    UINT32 ptsHZidx      = summNode.ptsHZidx();
    UINT32 ptsDNidx      = summNode.ptsDNidx();

    // GBL
    for (UINT32 i = 0; i < numBitsPtsGBL; i++) {
      CGNodeId id = (CGNodeId)nodeIds[ptsGBLidx + i];
      cgnode->setPointsTo(id, CQ_GBL);
    }

    // HZ
    for (UINT32 i = 0; i < numBitsPtsHZ; i++) {
      CGNodeId id = (CGNodeId)nodeIds[ptsHZidx + i];
      cgnode->setPointsTo(id, CQ_HZ);
    }

    // DN
    for (UINT32 i = 0; i < numBitsPtsDN; i++) {
      CGNodeId id = (CGNodeId)nodeIds[ptsDNidx + i];
      cgnode->setPointsTo(id, CQ_DN);
    }
  }
  
  // formal parms and rets
  for (UINT32 i = 0; i < formalsCount; i++)
  {
    CGNodeId id = (CGNodeId)nodeIds[formalsIdx + i];
    parameters().push_back(id);
  }

  for (UINT32 i = 0; i < returnsCount; i++)
  {
    CGNodeId id = (CGNodeId)nodeIds[returnsIdx + i];
    returns().push_back(id);
  }

  // callsites
  for (UINT32 i = 0; i < callsitesCount; i++)
  {
    SUMMARY_CONSTRAINT_GRAPH_CALLSITE& summCallsite = 
      summCallsites[callsitesIdx + i];
    CallSite* cs = CXX_NEW(CallSite(summCallsite.id(), summCallsite.flags(),
                                    _memPool), _memPool);
    _callSiteMap[cs->id()] = cs;
    cs->setActualModeled(summCallsite.actualModeled());
    if (cs->checkFlags(CS_FLAGS_VIRTUAL))
      cs->virtualClass(summCallsite.virtualClass());
    if (cs->isDirect() && !cs->isIntrinsic())
      cs->st_idx(summCallsite.st_idx());
    else if (cs->isIndirect())
      cs->cgNodeId(summCallsite.cgNodeId());
    else if (cs->isIntrinsic())
      cs->intrinsic(summCallsite.intrinsic());

    for (UINT32 i = 0; i < formalsCount; i++)
    {
      CGNodeId id = (CGNodeId) nodeIds[formalsIdx + i];
      cs->addParm(id);
    }
    CGNodeId retId = summCallsite.returnId();
    if (retId != 0) 
    {
      cs->returnId(retId);
    }
  }
}


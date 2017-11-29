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
#include "constraint_graph_escanal.h"
#include "cse_table.h"
#include "intrn_info.h"
#include "opt_defs.h"
#include "tracing.h"

static PointsTo emptyPointsToSet;

void
EscapeAnalysis::printStFlags(UINT32 flags) const
{
  fprintf(stderr,"[");
  if (flags & CG_ST_FLAGS_HOLDING)
    fprintf(stderr," HOLDING");
  if (flags & CG_ST_FLAGS_HOLDING_ESC)
    fprintf(stderr," HOLDING_ESC");
  if (flags & CG_ST_FLAGS_OPAQUE)
    fprintf(stderr," OPAQUE");
  if (flags & CG_ST_FLAGS_PROPAGATES)
    fprintf(stderr," PROPAGATES");
  if (flags & CG_ST_FLAGS_PROPAGATES_RET)
    fprintf(stderr," PROPAGATES_RET");
  fprintf(stderr,"]");
}

UINT32
EscapeAnalysis::escapeStFlags(const ConstraintGraphNode *node) const
{
  StTable::const_iterator iter;
  iter = _stTable.find(StTableKey(node->cg_st_idx(),node->offset()));
  if (iter != _stTable.end())
    return iter->second;
  return 0;
}

UINT32
EscapeAnalysis::findStFlags(ConstraintGraphNode *node)
{
  UINT32 &stFlags = _stTable[StTableKey(node->cg_st_idx(),node->offset())];
  if (!(stFlags & CG_ST_FLAGS_CACHE)) {
    stFlags |= node->stInfo()->flags();
    stFlags |= CG_ST_FLAGS_CACHE;
  }
  return stFlags;
}

void
EscapeAnalysis::addStFlagsAll(ConstraintGraphNode *node, UINT32 flags)
{
  if (node->stInfo()->checkFlags(CG_ST_FLAGS_PREG)) {
    addStFlags(node,flags);
    return;
  }
  ConstraintGraphNode *cur = node->stInfo()->firstOffset();
  while (cur) {
    addStFlags(cur,flags);
    cur = cur->nextOffset();
  }
}

void
EscapeAnalysis::addStFlags(ConstraintGraphNode *node, UINT32 flags)
{
  UINT32 &stFlags = _stTable[StTableKey(node->cg_st_idx(),node->offset())];
  if (!(stFlags & CG_ST_FLAGS_CACHE)) {
    stFlags |= node->stInfo()->flags();
    stFlags |= CG_ST_FLAGS_CACHE;
  }
  stFlags |= flags;
}

void
EscapeAnalysis::addToWorkList(ConstraintGraphNode *node)
{
  if (node == ConstraintGraph::notAPointer())
    return;
  if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
    fprintf(stderr, "ESCANAL:       node %d (single)\n",node->id());
  _workList.push(node);
}

void
EscapeAnalysis::addStToWorkList(ConstraintGraphNode *node)
{
  if (node == ConstraintGraph::notAPointer())
    return;

  if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG)) {
    fprintf(stderr,"ESCANAL:      adding all nodes for this ST to worklist- ");
    printStFlags(findStFlags(node));
    fprintf(stderr,"\n");
  }

  if (node->stInfo()->checkFlags(CG_ST_FLAGS_PREG)) {
    if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
      fprintf(stderr, "ESCANAL:       node %d (ST)\n",node->id());
    _workList.push(node);
    return;
  }

  ConstraintGraphNode *cur = node->stInfo()->firstOffset();
  if (cur && cur->offset() == -1)
    cur = cur->nextOffset();
  while (cur) {
    if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
      fprintf(stderr, "ESCANAL:       node %d (ST)\n",cur->id());
    _workList.push(cur);
    cur = cur->nextOffset();
  }
}

EscapeAnalysis::~EscapeAnalysis()
{
}

bool
EscapeAnalysis::observed(ConstraintGraphNode *node)
{
  return !node->pointsTo(CQ_DN).isEmpty();
}

void
EscapeAnalysis::newContEscapeSt(ConstraintGraphNode *node, UINT32 flags)
{
  if (node->stInfo()->checkFlags(CG_ST_FLAGS_PREG)) {
    newContEscapeNode(node,flags);
    return;
  }
  ConstraintGraphNode *n = node->stInfo()->firstOffset();
  if (n && n->offset() == -1)
    n = n->nextOffset();
  while (n) {
    newContEscapeNode(n,flags);
    n = n->nextOffset();
  }
}

void
EscapeAnalysis::newContEscapeNode(ConstraintGraphNode *node, UINT32 flags)
{
  if (_graph && node->cg() != _graph)
    return;

  UINT32 stFlags = findStFlags(node);
  if (_summaryMode && !observed(node))
    return;

  if (flags & CG_ST_FLAGS_LCONT_ESC) {
    /* Globals are never marked for local summarization */
    if (stFlags & CG_ST_FLAGS_LCONT_ESC ||
        (_summaryMode &&
         (stFlags & (CG_ST_FLAGS_GLOBAL|CG_ST_FLAGS_NOCNTXT))))
      return;

    if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
      fprintf(stderr,"ESCANAL:     Node %d CG_ST_IDX %lld (%s) marked holding\n",
               node->id(),node->cg_st_idx(),node->stName());
    addStFlags(node,CG_ST_FLAGS_LCONT_ESC);
    if (node->checkFlags(CG_NODE_FLAGS_MERGED))
      newContEscapeNode(node->parent(),CG_ST_FLAGS_LCONT_ESC);
    else
      addToWorkList(node);
  }
}

void
EscapeAnalysis::newPropEscapeSt(ConstraintGraphNode *node, UINT32 flags)
{
  if (node->stInfo()->checkFlags(CG_ST_FLAGS_PREG)) {
    newPropEscapeNode(node,flags);
    return;
  }
  ConstraintGraphNode *n = node->stInfo()->firstOffset();
  if (n && n->offset() == -1)
    n = n->nextOffset();
  while (n) {
    newPropEscapeNode(n,flags);
    n = n->nextOffset();
  }
}

void
EscapeAnalysis::newPropEscapeNode(ConstraintGraphNode *node, UINT32 flags)
{
  if (_graph && node->cg() != _graph)
    return;

  if (flags & CG_ST_FLAGS_LPROP_ESC) {
    UINT32 stFlags = findStFlags(node);
    /* Globals are never marked for local summarization */
     if (stFlags & CG_ST_FLAGS_LPROP_ESC ||
         (_summaryMode &&
          (stFlags & (CG_ST_FLAGS_GLOBAL|CG_ST_FLAGS_NOCNTXT))))
       return;

     if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
       fprintf(stderr,"ESCANAL:     Node %d CG_ST_IDX %lld (%s) marked propagates\n",
               node->id(),node->cg_st_idx(),node->stName());
     addStFlags(node,CG_ST_FLAGS_LPROP_ESC);
     if (node->checkFlags(CG_NODE_FLAGS_MERGED))
       newPropEscapeNode(node->parent(),CG_ST_FLAGS_LPROP_ESC);
     else
       addToWorkList(node);
  }

  if (flags & CG_ST_FLAGS_RETPROP_ESC) {
    UINT32 stFlags = findStFlags(node);
    if (!(stFlags & CG_ST_FLAGS_RETPROP_ESC)) {
      if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
        fprintf(stderr,"ESCANAL:     Node %d CG_ST_IDX %lld (%s) marked propagates_ret\n",
                node->id(),node->cg_st_idx(),node->stName());
      addStFlags(node,CG_ST_FLAGS_RETPROP_ESC);
      if (node->checkFlags(CG_NODE_FLAGS_MERGED))
        newPropEscapeNode(node->parent(),CG_ST_FLAGS_RETPROP_ESC);
      else
        addToWorkList(node);
    }
  }
}

void
EscapeAnalysis::newFullEscapeNode(ConstraintGraphNode *node, UINT32 flags)
{
  if (_graph && node->cg() != _graph)
      return;

  if (flags & (CG_ST_FLAGS_LFULL_ESC|CG_ST_FLAGS_LPROP_ESC)) {
    UINT32 stFlags = findStFlags(node);
    /* Globals are never marked for local summarization */
    if (stFlags & CG_ST_FLAGS_LFULL_ESC ||
        (_summaryMode &&
         (stFlags & (CG_ST_FLAGS_GLOBAL|CG_ST_FLAGS_NOCNTXT))))
      return;

#if 0
    /* If node is original to a function, it can only
         escape through its own parameters - technically
         a summary node is owned by the owner of its callsite
         that incorporated it ... however there is no access to
         this info at the moment */
    if (IPA_FLAG_ISCLR(node->flags, IPA_CG_NODE_FLAGS_SUMMARY))
    {
      IPA_symbol_info_t *sym;
      sym = IPA_symbol_find_by_id(info, node->data.var_id);
      if (sym->fninfo != param_fninfo)
      {
        /*debug_print("\nDOES NOT REALLY esclocal: ",node,NULL,"\n");*/
        return;
      }
    }
#endif
    if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
      fprintf(stderr,"ESCANAL:     Node %d CG_ST_IDX %lld (%s) marked opaque\n",
              node->id(),node->cg_st_idx(),node->stName());

    addStFlagsAll(node,CG_ST_FLAGS_LFULL_ESC|
                       CG_ST_FLAGS_LCONT_ESC|
                       CG_ST_FLAGS_LPROP_ESC);
    addStToWorkList(node);

    FmtAssert(!_summaryMode ||
              !node->cg()->stInfo(node->cg_st_idx())->checkFlags(
                                                    CG_ST_FLAGS_GLOBAL),(""));
    FmtAssert(!_summaryMode ||
              !node->cg()->stInfo(node->cg_st_idx())->checkFlags(
                                                    CG_ST_FLAGS_NOCNTXT),(""));

    if (stFlags & CG_ST_FLAGS_GLOBAL)
      _globalEscapeCnt++;
    else {
      _localEscapeCnt++;
      node->cg()->stInfo(node->cg_st_idx())->addFlags(CG_ST_FLAGS_ESCLOCAL);
    }
  }


  /* Only addresses that are allowed to escape through returns
   *   can become fully escaping at a RETPROP node
   */
  if (flags & CG_ST_FLAGS_RETPROP_ESC) {
    UINT32 stFlags = findStFlags(node);
    if (!(stFlags & CG_ST_FLAGS_NOLOCAL) ||
        (_summaryMode &&
            (stFlags & (CG_ST_FLAGS_GLOBAL|CG_ST_FLAGS_NOCNTXT))))
      return;

    if (stFlags & CG_ST_FLAGS_LFULL_ESC)
      return;

    if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
      fprintf(stderr,"ESCANAL:     Node %d CG_ST_IDX %lld (%s) marked opaque\n",
              node->id(),node->cg_st_idx(),node->stName());

    addStFlagsAll(node,CG_ST_FLAGS_LFULL_ESC|
                       CG_ST_FLAGS_LCONT_ESC|
                       CG_ST_FLAGS_LPROP_ESC);
    addStToWorkList(node);

    FmtAssert(!_summaryMode ||
              !node->cg()->stInfo(node->cg_st_idx())->checkFlags(
                                                    CG_ST_FLAGS_GLOBAL),(""));
    FmtAssert(!_summaryMode ||
              !node->cg()->stInfo(node->cg_st_idx())->checkFlags(
                                                    CG_ST_FLAGS_NOCNTXT),(""));

    if (stFlags & CG_ST_FLAGS_GLOBAL)
      _globalEscapeCnt++;
    else {
      _localEscapeCnt++;
      node->cg()->stInfo(node->cg_st_idx())->addFlags(CG_ST_FLAGS_ESCLOCAL);
    }
  }
}

extern void printPointsTo(const PointsTo &pts);

void
EscapeAnalysis::examineCallSites(ConstraintGraph *graph)
{
  CallSiteIterator iter = graph->callSiteMap().begin();
  for ( ; iter != graph->callSiteMap().end(); ++iter) {
    CallSite *callsite = iter->second;
    if (callsite->isDirect()) {
      bool inTable;
      CallSideEffectInfo callInfo =
          !callsite->isIntrinsic() ?
              CallSideEffectInfo::GetCallSideEffectInfo(&St_Table[callsite->st_idx()],
                                                        &inTable) :
              CallSideEffectInfo::GetCallSideEffectInfo(callsite->intrinsic(),&inTable);

      // Here we assume that if we have a routine performing heap
      // (de)allocation that non- of the arguments escape.
      if (callInfo.isHeapAllocating() || callInfo.isHeapDeallocating())
        continue;

      // Here we process the actuals only for calls in the cse table.  Other
      // calls are handled below.
      if (inTable) {
        UINT32 argPos = 0;
        for(list<CGNodeId>::const_iterator li = callsite->parms().begin();
            li != callsite->parms().end(); ++li, ++argPos) {
          UINT32 argAttr = callInfo.GetArgumentAttr(argPos,NULL,
                                                    !callsite->percN());

          if (argAttr & CPA_no_ptr_deref_and_expose)
            continue;

          // Have we been explicitly told that the argument does not
          // escape?
          if (argAttr & CPA_is_not_escaping)
            continue;

          // If the argument is not being written and is not exposed to
          // globals, then it does not escape.
          UINT32 written = CPA_one_level_write|
              CPA_two_level_write|
              CPA_multi_level_write;
          UINT32 exposed = CPA_exposed_to_globals|
              CPA_exposed_to_return; /* until connect this */
          if (!(argAttr & (written|exposed)))
            continue;

          ConstraintGraphNode *actual = ConstraintGraph::cgNode(*li);
          if (callsite->actualParmModeled(argPos))
            continue;

          if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG)) {
            fprintf(stderr,"ESCANAL: cse param %d of %s prop\n",
                    argPos,
                    !callsite->isIntrinsic()?ST_name(callsite->st_idx()):
                        INTRN_c_name(callsite->intrinsic()));
            for (PointsToIterator pti(graph->cgNode(*li)); pti != 0; ++pti) {
              PointsTo &pts = *pti;
              for (PointsTo::SparseBitSetIterator iter(&pts,0); iter != 0; ++iter) {
                ConstraintGraphNode *node = ConstraintGraph::cgNode(*iter);
                fprintf(stderr,"<%d,%d> ",node->id(),node->offset());
              }
              fprintf(stderr,"\n");
            }
          }
          newPropEscapeSt(actual,CG_ST_FLAGS_LPROP_ESC);
        }
        if (callsite->returnId()) {
          // Have we been explicitly told that the return does not
          // escape?
          if (callInfo.returnIsNotEscaping())
            continue;
          ConstraintGraphNode *actual = 
                               ConstraintGraph::cgNode(callsite->returnId());
          if (!callsite->actualReturnModeled()) {
            if(Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
              fprintf(stderr,"ESCANAL: cse return of %s holding\n",
                      !callsite->isIntrinsic()?ST_name(callsite->st_idx()):
                          INTRN_c_name(callsite->intrinsic()));
            newContEscapeSt(actual,CG_ST_FLAGS_LCONT_ESC);
          }
        }
        continue;
      }
      // The actuals of pure, side-effect free calls do not escape
      // Could we actually refine this to be side-effect free?
      else if (callsite->isIntrinsic()) {
        if (INTRN_is_pure(callsite->intrinsic()) &&
            INTRN_has_no_side_effects(callsite->intrinsic()))
          continue;
        // The arguments to va_start() do not escape
        else if (callsite->intrinsic() == INTRN_VA_START)
          continue;
      }
    }

    // If we get here, then we need to mark the actual parameter/return
    // appropriately.  There are three possibilities:
    // 1) We are not in -ipa mode, hence all actuals are propagating escape
    // 2) During -ipa, if the call is direct to a non-external function,
    //    the actuals are not escaping.
    // 3) During -ipa, if the call is indirect, we have hooked up all
    //    possible callees, so the actuals are not escaping
    if ( _ipaMode != IPANo && callsite->isDirect() && !callsite->isIntrinsic()) {
      if (!externalCall(callsite->st_idx()))  /* (2) */
        continue;
      else {
        if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
          fprintf(stderr,"ESCANAL: external call %s\n",ST_name(callsite->st_idx()));
      }
    }

    if (_ipaMode == IPAComplete && callsite->isIndirect())  /* (3) */
      continue;

    UINT32 argPos = 0;
    for (list<CGNodeId>::const_iterator li = callsite->parms().begin();
        li != callsite->parms().end(); ++li, ++argPos) {
      ConstraintGraphNode *actual = ConstraintGraph::cgNode(*li);
      if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG)) {
        fprintf(stderr,"ESCANAL: call to %s (param %d) not in cse table\n",
                   callsite->isDirect() ?
                       (!callsite->isIntrinsic() ?
                           ST_name(callsite->st_idx()) :
                           INTRN_specific_name(callsite->intrinsic())) :
                           "????",argPos);
        for (PointsToIterator pti(graph->cgNode(*li)); pti != 0; ++pti) {
          PointsTo &pts = *pti;
          for (PointsTo::SparseBitSetIterator iter(&pts,0); iter != 0; ++iter) {
            ConstraintGraphNode *node = ConstraintGraph::cgNode(*iter);
            fprintf(stderr,"<%d,%d> ",node->id(),node->offset());
          }
          fprintf(stderr,"\n");
        }
      }
      newPropEscapeSt(actual,CG_ST_FLAGS_LPROP_ESC);
    }
    if (callsite->returnId()) {
      ConstraintGraphNode *actual = 
                           ConstraintGraph::cgNode(callsite->returnId());
      newContEscapeSt(actual,CG_ST_FLAGS_LCONT_ESC);
    }
  }
}

bool
EscapeAnalysis::exposed(CG_ST_IDX idx)
{
  FmtAssert(!_wholeProgramMode,
            ("Symbol only exposed when not in whole program mode!\n"));

  // If we are not in ipa mode, consider all globals as exposed
  if (!_ipaMode)
    return true;

  // Since we are not in whole program mode, these symbols must
  // be considered as exposed during an -ipa compile
  // of analysis scope
  ST_IDX stIdx = SYM_ST_IDX(idx);
  ST_EXPORT stExport = ST_export(&St_Table[stIdx]);
  if (stExport == EXPORT_PREEMPTIBLE ||
      stExport == EXPORT_PROTECTED)
    return true;

  return false;
}

bool
EscapeAnalysis::formalsEscape(ConstraintGraph *graph) const
{
  // This routine is more interesting for the IPA case
  return true;
}

bool
EscapeAnalysis::externalCall(ST_IDX idx) const
{
  // All calls are external for non-ipa compiles
  return true;
}

void
EscapeAnalysis::init(void)
{
  initGraph(_graph);
}

void
EscapeAnalysis::initGraph(ConstraintGraph *graph)
{
  /* formal(u) actual(u)  global(u)
   * --------  ---------  ---------
   *   CE(u)      PE(u)     FE(u)
   *
   *   Note that we handle actual parameter/return values
   *   in a separate walk where we take into account the
   *   any known semantics of the callee.
   */
  bool escFormals = formalsEscape(graph);
  for (CGNodeToIdMapIterator iter = graph->lBegin();
      iter != graph->lEnd(); ++iter) {
    ConstraintGraphNode *node = iter->first;
    if (escFormals) {

      if (node->checkFlags(CG_NODE_FLAGS_FORMAL_PARAM)) {
        if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
          fprintf(stderr,"ESCANAL: formal param of %s\n",graph->name());
        newContEscapeSt(node,CG_ST_FLAGS_LCONT_ESC);
      }
      if (node->checkFlags(CG_NODE_FLAGS_FORMAL_RETURN)) {
        if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
          fprintf(stderr,"ESCANAL: formal return of %s\n",graph->name());
        newPropEscapeSt(node,CG_ST_FLAGS_RETPROP_ESC);
      }
    }
    // Global variables are only considered candidates for escape
    // analysis iff:
    // (1) We are not computing summary.  When computing summary,
    //     globals are context insensitive (and should be in
    //     another graph) and are irrelevant.
    // (2) We are not in whole program mode.  If we are in whole
    //     program mode, the only globals that are considered to
    //     be escaped are "libc globals"
    //     TODO:  Handle "libc globals" here
    // (3) The global is visible outside the scope of the scope
    //     of analysis, e.g. object file at -O2 or load module
    //     under -ipa.  We ask is it "exposed"?
    if (!_summaryMode &&
        !_wholeProgramMode &&
        node->stInfo()->checkFlags(CG_ST_FLAGS_GLOBAL) &&
        exposed(node->cg_st_idx()))
      newFullEscapeNode(node,CG_ST_FLAGS_LFULL_ESC);
  }
  examineCallSites(graph);
}

void
EscapeAnalysis::processContEscapeNode(ConstraintGraphNode *node)
{
  if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
    fprintf(stderr,"ESCANAL:   Process Holding\n");

  /* New CE node effects:
   *    - IN : store edges
   *    - OUT: copy/skew, load edges
   */

  UINT32 nodeStFlags = findStFlags(node);

  /* u = v , FE(v) or CE(v)        u = v , CE(v)
   * ----------------------  ===>  -------------  DOWN
   *          CE(u)                     CE(u)
   */
  for (CGEdgeSetIterator outCopyIter = 
       node->parent()->outCopySkewEdges().begin();
       outCopyIter != node->parent()->outCopySkewEdges().end(); ++outCopyIter) 
  {
     ConstraintGraphEdge *edge = *outCopyIter;
     if (_summaryMode)
       if (edge->edgeQual() == CQ_GBL || edge->edgeQual() == CQ_UP ||
           edge->edgeQual() == CQ_DN)
         continue;
     if (edge->destNode()->offset() == -1)
       newContEscapeSt(edge->destNode(), nodeStFlags);
     newContEscapeNode(edge->destNode(),nodeStFlags);
  }



  /* u = *v, FE(v) or CE(v)        u = *v , CE(v)
   * ----------------------  ===>  --------------  DOWN
   *         CE(u)                      CE(u)
   */
  for (CGEdgeSetIterator outLdStIter = 
       node->parent()->outLoadStoreEdges().begin();
       outLdStIter != node->parent()->outLoadStoreEdges().end(); ++outLdStIter) 
  {
     ConstraintGraphEdge *edge = *outLdStIter;
     if (edge->edgeType() == ETYPE_STORE)
       continue;
     if (_summaryMode)
       if (edge->edgeQual() == CQ_GBL || edge->edgeQual() == CQ_UP ||
           edge->edgeQual() == CQ_DN)
         continue;
     newContEscapeNode(edge->destNode(),nodeStFlags);
  }


  /* *u = v, FE(u) or CE(u)        *u = v , CE(u)
   * ----------------------  ===>  --------------  UP
   *          PE(v)                     PE(v)
   */
  for (CGEdgeSetIterator inLdStIter = 
       node->parent()->inLoadStoreEdges().begin();
       inLdStIter != node->parent()->inLoadStoreEdges().end(); ++inLdStIter) 
  {
    ConstraintGraphEdge *edge = *inLdStIter;
    if (edge->edgeType() == ETYPE_LOAD)
      continue;
    if (_summaryMode)
      if (edge->edgeQual() == CQ_GBL || edge->edgeQual() == CQ_UP ||
          edge->edgeQual() == CQ_DN)
        continue;
    newPropEscapeNode(edge->srcNode(),CG_ST_FLAGS_LPROP_ESC);
  }
}

void
EscapeAnalysis::processPropEscapeNode(ConstraintGraphNode *node)
{
  if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
    fprintf(stderr,"ESCANAL:   Process Propagates\n");

  /* New PE node effects:
   *    - IN : copy/skew, points-to contents
   *    - OUT:
   */
  UINT32 nodeStFlags = findStFlags(node);

  /* u = v , FE(u) or PE(u)        u = v , PE(u)
     * ----------------------  ===>  -------------  UP
     *         PE(v)                      PE(v)
     */
  for (CGEdgeSetIterator inCopyIter = node->parent()->inCopySkewEdges().begin();
       inCopyIter != node->parent()->inCopySkewEdges().end(); ++inCopyIter) 
  {
    ConstraintGraphEdge *edge = *inCopyIter;
    if (_summaryMode)
      if (edge->edgeQual() == CQ_GBL || edge->edgeQual() == CQ_UP ||
          edge->edgeQual() == CQ_DN)
      continue;
    if (edge->srcNode()->offset() == -1)
      newPropEscapeSt(edge->srcNode(), nodeStFlags);
    newPropEscapeNode(edge->srcNode(),nodeStFlags);
  }

  /* u = &v, FE(u) or PE(u)         u = &v , PE(u)
    * ----------------------  ===>   --------------  UP
    *       FE(v)                         FE(v)
    */
  for (PointsToIterator pti(node); pti != 0; ++pti) {
    if (!_summaryMode || pti.qual() == CQ_HZ) {
      PointsTo &pts = *pti;
      for (PointsTo::SparseBitSetIterator iter(&pts,0); iter != 0; ++iter) {
        ConstraintGraphNode *n = ConstraintGraph::cgNode(*iter);
        if (n->offset() == -1)
          newFullEscapeNode(n->nextOffset(),nodeStFlags);
        newFullEscapeNode(n,nodeStFlags);
      }
    }
  }
}

void
EscapeAnalysis::processFullEscapeNode(ConstraintGraphNode *node)
{
  if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
    fprintf(stderr,"ESCANAL:   Process Opaque\n");

  /* New FE node effects:
   *    - IN : copy/skew, points-to contents, store
   *    - OUT: copy/skew, (assign_addr), load
   */

  /* u = &v , FE(v)                 u = &v , FE(v)
    * --------------          ===>   --------------  DOWN
    *     CE(u)                           CE(u)
    */
  UINT32 stFlags = findStFlags(node);

  // Here we are looking for the outgoing points-to relationship,
  // i.e. who points to the current node.
  for (PointsToIterator pti(node,PtsRev); pti != 0; ++pti) {
     if (!_summaryMode || pti.qual() == CQ_HZ) {
       PointsTo &pts = *pti;
       for (PointsTo::SparseBitSetIterator iter(&pts,0); iter != 0; ++iter) {
         ConstraintGraphNode *revNode = ConstraintGraph::cgNode(*iter);
         newContEscapeNode(revNode,stFlags);
         if (!_graph || revNode->cg() == _graph)
           if (!_summaryMode ||
               !(stFlags & (CG_ST_FLAGS_GLOBAL|CG_ST_FLAGS_NOCNTXT)))
             addStFlags(revNode,CG_ST_FLAGS_LCONT_ESCLCL);
       }
     }
   }
}

void
EscapeAnalysis::markEscaped(void)
{
  ConstraintGraphNode *bh = ConstraintGraph::blackHole();
  for (CGIdToNodeMapIterator iter = ConstraintGraph::gBegin();
      iter != ConstraintGraph::gEnd(); iter++) {
    ConstraintGraphNode *node = iter->second;
    if (node == ConstraintGraph::notAPointer())
      continue;
    UINT32 flags = escapeStFlags(node);
    if (flags & (CG_ST_FLAGS_HOLDING|
                 CG_ST_FLAGS_HOLDING_ESC|
                 CG_ST_FLAGS_OPAQUE)) {
      // The "black hole" is meant to represent all memory that is possibly
      // accessed by symbols that have references outside the scope of the
      // current procedure.
      node->addPointsTo(bh,CQ_GBL);

      // Has this non-global node fully escaped?
      if (!(flags & (CG_ST_FLAGS_GLOBAL|CG_ST_FLAGS_HEAP|CG_ST_FLAGS_FUNC)) &&
          (flags & CG_ST_FLAGS_OPAQUE))
        node->stInfo()->addFlags(CG_ST_FLAGS_ESCLOCAL);
    }
  }
}

bool
EscapeAnalysis::escaped(const ConstraintGraphNode *node) const
{
  return (escapeStFlags(node) & CG_ST_FLAGS_ESCALL) != 0;
}

void
EscapeAnalysis::perform(void)
{
  if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
    fprintf(stderr,"ESCANAL: Summary %d, IPA %d, WPM %d\n",
            _summaryMode, _ipaMode, _wholeProgramMode);

  init();

  time_t startTime = time(NULL);

  UINT32 cnt = 0;
  while (!_workList.empty())  {
    ConstraintGraphNode *node = _workList.pop();

    if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG))
      fprintf(stderr,"ESCANAL: Processing node %d\n",node->id());

    UINT32 stFlags = findStFlags(node);
    if (stFlags & (CG_ST_FLAGS_LPROP_ESC|CG_ST_FLAGS_RETPROP_ESC))
      processPropEscapeNode(node);
    if (stFlags & CG_ST_FLAGS_LCONT_ESC)
      processContEscapeNode(node);
    if (stFlags & CG_ST_FLAGS_LFULL_ESC)
      processFullEscapeNode(node);
  }

  if (Get_Trace(TP_ALIAS,NYSTROM_SOLVER_FLAG)) {
    fprintf(stderr,"ESCANAL: local escape  = %d\n",_localEscapeCnt);
    fprintf(stderr,"ESCANAL: global escape = %d\n",_globalEscapeCnt);
    fprintf(stderr,"ESCANAL: observed      = %d\n",_observedCnt);
  }

  time_t endTime = time(NULL);

  if (Get_Trace(TP_ALIAS,NYSTROM_LW_SOLVER_FLAG))
    fprintf(stderr,"EscapeAnalysis required %lds\n", endTime-startTime);
}

void
EscapeAnalysis::identifyMallocWrappers()
{
  hash_set<CGNodeId> nonEscapingHeaps;

  for (CGNodeToIdMapIterator iter = _graph->lBegin(); 
       iter != _graph->lEnd(); ++iter) {
    ConstraintGraphNode *n = iter->first;
    // Expect to have a single offset
    if (n->stInfo()->numOffsets() != 1)
      continue;
    // Is a heap
    if (!n->stInfo()->checkFlags(CG_ST_FLAGS_HEAP))
      continue;
    // No in/out edges
    if (!n->inLoadStoreEdges().empty() || !n->inCopySkewEdges().empty() ||
        !n->outLoadStoreEdges().empty() || !n->outCopySkewEdges().empty())
      continue;
    // Scan the reverse points-to set
    for (PointsToIterator piter(n,PtsRev); piter != 0; ++piter) {
      PointsTo &rpts = *piter;
      for (PointsTo::SparseBitSetIterator bsi(&rpts, 0); bsi != 0; ++bsi) {
        ConstraintGraphNode *rn = ConstraintGraph::cgNode(*bsi);
        UINT32 flags = findStFlags(rn);
        if ((flags & CG_ST_FLAGS_PROPAGATES) || (flags & CG_ST_FLAGS_OPAQUE))
          return;
      }
    }
    nonEscapingHeaps.insert(n->id());
  }

  if (nonEscapingHeaps.empty())
    return;

  if (_graph->returns().empty())
    return;

  for (list<CGNodeId>::const_iterator retIter = _graph->returns().begin();
       retIter != _graph->returns().end(); retIter++) {
    ConstraintGraphNode *formalRet = ConstraintGraph::cgNode(*retIter);
    // Check if the return contain only the heap variables
    for (PointsToIterator piter(formalRet); piter != 0; ++piter) {
      PointsTo &pts = *piter;
      for (PointsTo::SparseBitSetIterator bsi(&pts, 0); bsi != 0; ++bsi) {
        CGNodeId id = *bsi;
        if (id == _graph->blackHole()->id())
          continue;
        if (nonEscapingHeaps.find(id) == nonEscapingHeaps.end())
          return;
      }
    }
    // Check if all the heap variables are in the return
    for (hash_set<CGNodeId>::const_iterator siter = nonEscapingHeaps.begin();
         siter != nonEscapingHeaps.end(); siter++) {
      CGNodeId h = *siter;
      bool present = false;
      for (PointsToIterator piter(formalRet); piter != 0; ++piter) {
        PointsTo &pts = *piter;
        if (pts.isSet(h)) {
          present = true;
          break;
        }
      }
      if (!present)
        return;
    }
  }

  if (Get_Trace(TP_ALIAS,NYSTROM_LW_SOLVER_FLAG))
    fprintf(stderr, "%s is malloc wrapper\n", ST_name(Get_Current_PU_ST()));
  Set_PU_has_attr_malloc(Get_Current_PU());
}

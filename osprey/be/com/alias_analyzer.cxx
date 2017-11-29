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
#include "alias_analyzer.h"
#include "be_util.h"
#include "config_opt.h"
#include "ir_reader.h"  /* fdump_tree */
#include "nystrom_alias_analyzer.h"
#include "opt_defs.h"  /* Trace flags */
#include "tracing.h"
#include "pu_info.h"

extern BOOL Read_ALIAS_CGNODE_Map;

// There will be one instance of an AliasAnalyzer object, the
// results of the alias analysis are either provided via summary
// from ipa_link or computed locally during a non-ipa compile
AliasAnalyzer *AliasAnalyzer::_alias_analyzer = NULL;

AliasAnalyzer *
AliasAnalyzer::Create_Alias_Analyzer(ALIAS_CONTEXT &ac, WN *tree)
{
  // if create nystrom alias analyzer in IPA phase.
  // only creat one instance for all PUs.
  if (Alias_Nystrom_Analyzer && Alias_Analyzer_in_IPA) {
    if (_alias_analyzer == NULL) {
      _alias_analyzer = new NystromAliasAnalyzer(ac);
    }

    // this alias analyzer is for IPA:preopt
    Is_True(Current_IPANode_File_PU_Idx != UINT32_MAX, ("invlaid IPANode PUFileIdx\n"));
    _alias_analyzer->curFilePUIdx(Current_IPANode_File_PU_Idx);

    FmtAssert(ConstraintGraph::IPANodeCG() != NULL, ("Create Alias anlyzer IPA node cg is NULL")); 
    ((NystromAliasAnalyzer*)_alias_analyzer)->constraintGraph(ConstraintGraph::IPANodeCG());
    ((NystromAliasAnalyzer*)_alias_analyzer)->createAliasTags(tree);
    return _alias_analyzer;
  }
  
  if (_alias_analyzer != NULL) {
    // Activate the use of the Nystrom points-to analysis by the
    // ALIAS_RULE harness and disable alias classification rules.
    //ac |= ALIAS_ANALYZER_RULE;
    //ac &= ~(CLAS_RULE|IP_CLAS_RULE);
    return _alias_analyzer;
  }

  // What alias analyzer are we going to use?
  if ( Alias_Nystrom_Analyzer ) {
    if (FILE_INFO_ipa(File_info) && !PU_mp(Get_Current_PU ()) )
      _alias_analyzer = new NystromAliasAnalyzer(ac, tree, true);
    else
      _alias_analyzer = new NystromAliasAnalyzer(ac, tree);
    if (Get_Trace(TP_ALIAS,NYSTROM_CG_POST_FLAG)){
      fdump_tree(stderr, tree);
      fprintf(stderr, "Printing final ConstraintGraph\n");
      ((NystromAliasAnalyzer*)_alias_analyzer)->constraintGraph()->print(stderr);
    }
 
    return _alias_analyzer;
  }
  else
    return NULL;
}

void
AliasAnalyzer::Delete_Alias_Analyzer()
{
  if (_alias_analyzer) {
    delete _alias_analyzer;
    _alias_analyzer = NULL;
  }
}
  
AliasAnalyzer::AliasAnalyzer(bool ipaMode) 
  : _aliasQueryCount(0),
    _aliasedCount(0),
    _aliasTagMap(WN_MAP_UNDEFINED),
    _queryFileMap(NULL),
    _ipaMode(ipaMode),
    _curFilePUIdx(0)
{
  MEM_POOL_Initialize(&_memPool, "AliasAnalyzer_pool", FALSE);
  // if in IPA:preopt mode, use the inter-procedure _IPAAliasTageMap
  // to record WN->alias tag map.
  if (!_ipaMode) {
    _aliasTagMap = IPA_WN_MAP32_Create(Current_Map_Tab, &_memPool);
  }

  if (Alias_Query_File) {
    loadQueryFile(Alias_Query_File);
  }
}

AliasAnalyzer::~AliasAnalyzer() 
{
  if (_queryFileMap)
    delete _queryFileMap;
  if (Get_Trace(TP_ALIAS,NYSTROM_QUERY_TRACE_FLAG))
    fprintf(stderr, "Query stats: Proc: %s(%d) total: %d aliased: %d (%f)\n",
            ST_name(Current_PU_Info->proc_sym), Current_PU_Count(),
            _aliasQueryCount, _aliasedCount, 
            (float)_aliasedCount/(float)_aliasQueryCount * 100.0);

  if (!_ipaMode) {
    IPA_WN_MAP_Delete(Current_Map_Tab, _aliasTagMap);
  }
  MEM_POOL_Delete(&_memPool);
}

ALIAS_RESULT
AliasAnalyzer::aliased(AliasTag, AliasTag)
{
  incrAliasQueryCount();
  return POSSIBLY_ALIASED;
}

AliasTag
AliasAnalyzer::genAliasTag(ST *, INT64, INT64, bool)
{
  return InvalidAliasTag;
}

void
AliasAnalyzer::aliasedWithCall(ST *, AliasTag, BOOL &mod, BOOL &ref)
{
  mod = ref = TRUE;
}

AliasTag
AliasAnalyzer::meet(AliasTag, AliasTag)
{
  return InvalidAliasTag;
}

void
AliasAnalyzer::transferAliasTag(WN *dstWN, const WN *srcWN)
{
  AliasTag tag = getAliasTag(srcWN);
  if (tag != InvalidAliasTag)
    setAliasTag(dstWN, tag);
}

void 
AliasAnalyzer::print_All_AliasTag(FILE* f)
{
    return;
}

bool
AliasAnalyzer::checkQueryFile(UINT32 pu, AliasTag tag1, AliasTag tag2,
                              bool &result)
{
  if (_queryFileMap) {
    QueryFileKey key(pu,tag1,tag2);
    QueryFileMap::const_iterator iter = _queryFileMap->find(key);
    if (iter != _queryFileMap->end()) {
      result = iter->second;
      return true;
    }
  }
  return false;
}

void
AliasAnalyzer::loadQueryFile(char *filename)
{
  FILE *qf = fopen(filename,"r");
  if (qf) {
    _queryFileMap = new QueryFileMap();
    UINT32 pu, qId;
    AliasTag tag1, tag2;
    char result[8];
    char result2[8];
    while ( fscanf(qf,"Query %d,%d: aliased memop %d %d: %3c Alias (ac %3c)\n",
                   &pu,&qId,(UINT32*)&tag1,(UINT32*)&tag2,result,result2) == 6 ) {
      if (pu == Current_PU_Count()) {
        QueryFileKey key(pu,tag1,tag2);
        bool alias = (result[0] == 'N') ? false : true;
        (*_queryFileMap)[key] = alias;

        fprintf(stderr,"QueryMap <%d,%d,%d>: %s\n",
                pu,tag1,tag2,alias ? "May" : "No");
      }
    }
    fclose(qf);
  }
}

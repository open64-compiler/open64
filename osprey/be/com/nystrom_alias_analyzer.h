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
#ifndef nystrom_alias_analysis_INCLUDED
#define nystrom_alias_analysis_INCLUDED

struct WN;

#include "alias_analyzer.h"
#include "constraint_graph.h"

using namespace __gnu_cxx;

class StToAliasTagKey
{
public:
  StToAliasTagKey(CG_ST_IDX idx,UINT32 off,UINT32 size)
  : _idx(idx),_offset(off),_size(size) {}

  bool operator ==(const StToAliasTagKey &that) const
  {
    return _idx == that._idx &&
           _offset == that._offset &&
           _size == that._size;
  }
  size_t hash() const { return _idx << 16 ^ _offset << 8 ^ _size; }
private:
  CG_ST_IDX _idx;
  UINT32 _offset;
  UINT32 _size;
};
typedef struct {
bool operator()(const StToAliasTagKey &k1,
                const StToAliasTagKey &k2) const { return (k1 == k2); }
} equalStToAliasTagKey;
typedef struct {
  size_t operator() (const StToAliasTagKey &k) const { return (k.hash()); }
} hashStToAliasTagKey;
typedef hash_map<StToAliasTagKey, AliasTag,
                 hashStToAliasTagKey,equalStToAliasTagKey> StToAliasTagMap;
typedef StToAliasTagMap::iterator StToAliasTagMapIterator;

typedef pair<AliasTag,AliasTag> QueryCacheKey;
typedef struct {
  bool operator()(const QueryCacheKey &k1,
                const QueryCacheKey &k2) const
  { return k1.first == k2.first && k1.second == k2.second; }
} equalQueryCacheKey;
typedef struct {
  size_t operator() (const QueryCacheKey &k) const
  { return k.first << 16 ^ k.second; }
} hashQueryCacheKey;
typedef hash_map<QueryCacheKey, bool,
                 hashQueryCacheKey,equalQueryCacheKey> QueryCacheMap;
typedef QueryCacheMap::iterator QueryCacheIterator;

// Class to map AliasTags to points-to set
class AliasTagInfo 
{
public:

  AliasTagInfo(MEM_POOL *memPool) : 
    _pointsToSet(memPool)
  {}

  PointsTo &pointsTo() { return _pointsToSet; }

  virtual void print(FILE *file) 
  {
    fprintf(file, "[");
    _pointsToSet.print(file);
    fprintf(file, "]");
  }

private:
  PointsTo _pointsToSet;  // Set of CGNodeIds associated with this alias tag 
};

// Class to map AliasTag of a call to its mod/ref set
class CallAliasTagInfo : public AliasTagInfo
{
public:
  CallAliasTagInfo(MEM_POOL *memPool) :
    AliasTagInfo(memPool),
    _refPointsToSet(memPool)
  {}

  void print(FILE *file) 
  {
    fprintf(file, "mod: [");
    mod().print(file);
    fprintf(file, "]");
    fprintf(file, " ref: [");
    ref().print(file);
    fprintf(file, "]");
  }

  PointsTo &mod() { return pointsTo(); }
  PointsTo &ref() { return _refPointsToSet; }

private:
  PointsTo _refPointsToSet;
};

class NystromAliasAnalyzer : public AliasAnalyzer {

   friend class AliasAnalyzer;
   
public:
   // Invoked in ipa mode, alias analyzer shared by all PUs.
   NystromAliasAnalyzer(ALIAS_CONTEXT&);
   
   // Invoked in non-ipa mode when WN to CGNodeId map is not available
   NystromAliasAnalyzer(ALIAS_CONTEXT &,WN *);

   // Invoked during post-ipa be when WN to CGNodeId map exists
   NystromAliasAnalyzer(ALIAS_CONTEXT &, WN*, bool);

   ~NystromAliasAnalyzer();

   // Implement base class virtual methods
   
   // Provide the alias result for the references, objects
   // represented by the provided tags.
   ALIAS_RESULT aliased(AliasTag tag1, AliasTag tag2);

   // Given a symbol, provide the corresponding AliasTag
   // For use in creating POINTS_TO from symbol and by mod-ref
   AliasTag genAliasTag(ST *st, INT64 ofst, INT64 size, bool direct);

   // Mod-ref
   // Given a call (ST *) and the AliasTag of a possibly
   // referenced symbol, this method determines whether the
   // symbol may be modified or read by that call. Will not
   // pessimize initial values, i.e. will not transform a
   // 'false' value to 'true'.
   void aliasedWithCall(ST *call, AliasTag symTag,
                        BOOL &mod, BOOL &ref);

   // Implement additional interfaces needed by the ALIAS_MANAGER

   // Other public methods specific to this analyzer

   // For Mod-Ref. Returns 'true' if the points-to information 
   // associated with the requested symbol is complete and the 
   // points-to set is provided.  Otherwise, the routine 
   // returns 'false' and the points-to set is undefined.
   BOOL pointsToSet(AliasTag, PointsTo &);

   AliasTag meet(AliasTag dstTag, AliasTag srcTag);

   void transferAliasTag(WN *dstWN, const WN *srcWN);

   ConstraintGraph *constraintGraph() const { return _constraintGraph; }

   void constraintGraph(ConstraintGraph *cg) { _constraintGraph=cg; }

   CGNodeId cgNodeId(AliasTag tag) 
   {
     hash_map<UINT32, CGNodeId>::iterator iter = 
                                 _aliasTagToCGNodeIdMap.find((UINT32)tag);
     if (iter != _aliasTagToCGNodeIdMap.end())
       return iter->second;
     return 0;
   }

   bool isPostIPA() const { return _isPostIPA; }
   void print_AliasTag(AliasTag tag, FILE* f);
   void print_All_AliasTag(FILE* f);

private:

   // Creates a new AliasTag for use during client update of
   // alias information.  The underlying points-to set does not
   // have an associated symbol or perhaps even a constraint node
   AliasTag newAliasTag(void);

   // Create a new AliasTag for a call
   AliasTag newCallAliasTag(void);

   // Traverse the whirl tree starting from the func entry
   // and for each WN for which there is a valid CGNodeId, create
   // an AliasTag and its associated AliasTagInfo and update the aliasTagMap
   void createAliasTags(WN *entryWN);

   // Unions the points-to set of srcTag into the points-to set
   // of dstTag.
   void mergePointsTo(AliasTag dstTag, AliasTag srcTag);

   PointsTo &pointsTo(AliasTag tag) 
   {
      hash_map<UINT32, AliasTagInfo *>::iterator iter = 
                                        _aliasTagInfo.find((UINT32)tag);
      if (iter != _aliasTagInfo.end())
        return iter->second->pointsTo();
      return emptyPointsToSet;
   }

   ConstraintGraph* buildCGFromSummary();

   AliasTag _nextAliasTag;  // to generate unique AliasTags

   ConstraintGraph *_constraintGraph;

   // Maps the AliasTags to points-to sets
   hash_map<UINT32, AliasTagInfo *> _aliasTagInfo;

   // Maps a symbol,offset,size triple to a previously created AliasTag
   StToAliasTagMap _stToAliasTagMap;

   static PointsTo emptyPointsToSet;

   // Map AliasTags to their corresponding CGNode ids
   // so that during CODEREP -> WHIRL phase we can attach the original
   // CGNodeIds to the new WHIRL nodes from their AliasTags
   hash_map<UINT32, CGNodeId>   _aliasTagToCGNodeIdMap;

   // Cache the results of alias queries to avoid set intersections
   // on repeat queries of the same tags, presumably the map lookup
   // will be faster.
   QueryCacheMap  _queryCacheMap;

   bool _isPostIPA;
};

#endif

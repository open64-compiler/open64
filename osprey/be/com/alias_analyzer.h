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

#ifndef alias_analysis_INCLUDED
#define alias_analysis_INCLUDED

#include <ext/hash_map>
#include "mempool.h"
#include "sparse_bitset.h"
#include "opt_alias_interface.h"
#include "opt_alias_rule.h"
#include "wn_map.h"
#include "config_opt.h"

#define IPA_WN_MAP_ID(filePUIdx, wn_map_id) ((((UINT64)filePUIdx)<<32) | ((wn_map_id)&0xffffffff))
struct WN;
struct ST;

enum AliasTag {
  // We use zero as the indicator of an unknown or invalid tag since
  // that is the default value returned by WN_MAP for WNs for which
  // no mapping has been established.
  InvalidAliasTag = 0,
  // Indicates an empty alias set, e.g. this is the initial value of
  // the _alias_tag within a POINTS_TO object
  EmptyAliasTag,
  // First non-predefined alias tag to be associated with memory
  // references
  InitialAliasTag,
};

class QueryFileKey {
public:
  QueryFileKey(UINT32 pu, AliasTag tag1, AliasTag tag2)
   : _pu(pu), _tag1(tag1), _tag2(tag2) {}

  UINT32   _pu;
  AliasTag _tag1;
  AliasTag _tag2;
};

typedef struct
{
  size_t operator()(const QueryFileKey &k) const
  {
    return (size_t)(k._pu << 16 | k._tag1 << 8 | k._tag2);
  }
} hashQueryFileKey;

typedef struct
{
  bool operator()(const QueryFileKey &k1,
                  const QueryFileKey &k2) const
  {
    return (k1._pu == k2._pu &&
        k1._tag1 == k2._tag1 &&
        k1._tag2 == k2._tag2);
  }
} equalQueryFileKey;

typedef hash_map<QueryFileKey,bool,
                 hashQueryFileKey,equalQueryFileKey> QueryFileMap;

template <class _Key> struct IPAIndexhash { };
template <> struct IPAIndexhash<UINT64> {
  size_t operator()(const UINT64 x)const{return (size_t)x;}
};

typedef hash_map<UINT64, UINT32, IPAIndexhash<UINT64> > IPAWNAliasTagMap;

class AliasAnalyzer {

private:
   static AliasAnalyzer *_alias_analyzer;
   WN_MAP _aliasTagMap; // Maps WNs to AliasTags
   UINT32 _aliasQueryCount;   // Used for debugging
   UINT32 _aliasedCount;      // Used for debugging
   QueryFileMap *_queryFileMap;

   // Map WN to AliasTags in IPA:preopt mode
   // It maps all PUs's alias tag in one map.
   // key is [file_idx_16bit][pu_idx_16bit][wn_map_id_32_bit]
   IPAWNAliasTagMap _IPAAliasTageMap;
   bool _ipaMode;
   UINT32 _curFilePUIdx;   // current process IPA node's file and PU index

protected:
   MEM_POOL _memPool;

public:
   AliasAnalyzer(bool ipaMode=false);

   virtual ~AliasAnalyzer();

   static AliasAnalyzer *Create_Alias_Analyzer(ALIAS_CONTEXT &ac, 
                                               WN *tree);

   static void Delete_Alias_Analyzer();

   static AliasAnalyzer *aliasAnalyzer() { return _alias_analyzer; }

   // Provide the alias result for the references, objects
   // represented by the provided tags.
   virtual ALIAS_RESULT aliased(AliasTag tag1, AliasTag tag2);

   // Given a symbol, provide the corresponding AliasTag
   // For use in creating POINTS_TO from symbol and by mod-ref
   virtual AliasTag genAliasTag(ST *st, INT64 ofst, INT64 size, bool direct);

   // Mod-ref
   // Given a call (ST *) and the AliasTag of a possibly
   // referenced symbol, this method determines whether the
   // symbol may be modified or read by that call.  Will not
   // pessimize initial values, i.e. will not transform a
   // 'false' value to 'true'.
   virtual void aliasedWithCall(ST *call, AliasTag symTag,
                                BOOL &mod, BOOL &ref);

   // Require interfaces to support ALIAS_MANAGER routines
   // Copy_alias_info()
   // Duplicate_alias_info()
   // Create_vector_alias()
   // Valid_alias()
   virtual void transferAliasTag(WN *dstWN, const WN *srcWN);
   virtual void print_All_AliasTag(FILE* f);

   // Require interfaces to support ALIAS_MANAGER routines
   // Create_local_alias()
   // Create_global_alias()
   // Create_formal_alias()
   // Create_unique_pointer_alias()
   // Create_lda_array_alias()

   virtual AliasTag meet(AliasTag destTag, AliasTag srcTag);

   void setAliasTag(WN *wn, AliasTag tag, UINT32 filePUIdx=UINT32_MAX)
   {
     if (!_ipaMode) {
       IPA_WN_MAP32_Set(Current_Map_Tab, _aliasTagMap, wn, (INT32)tag);
     }
     else {
       INT32 wn_map_id = WN_map_id(wn);
       if (wn_map_id == -1) {
         WN_MAP_Set_ID(Current_Map_Tab, wn);
         wn_map_id = WN_map_id(wn);
       }
       Is_True(wn_map_id != -1, ("invalid WN map id\n"));
       if (filePUIdx == UINT32_MAX) {
         filePUIdx = curFilePUIdx();
       }
       UINT64 ipa_map_id = IPA_WN_MAP_ID(filePUIdx, wn_map_id);
       _IPAAliasTageMap[ipa_map_id] = (UINT32)tag;
     }
   }

   AliasTag getAliasTag(const WN *wn, UINT32 filePUIdx=UINT32_MAX) const
   {
     if (!_ipaMode) { 
       return (AliasTag)IPA_WN_MAP32_Get(Current_Map_Tab, _aliasTagMap, wn);
     }
     else {
       // get WN map id and compose IPA_WN_MAP_id
       INT32 wn_map_id = WN_map_id(wn);
       if (wn_map_id == -1) {
         return InvalidAliasTag;
       }
       if (filePUIdx == UINT32_MAX) {
         filePUIdx = curFilePUIdx();
       }
       UINT64 ipa_map_id = IPA_WN_MAP_ID(filePUIdx, wn_map_id);

       // search in _IPAAliasTageMap map.
       IPAWNAliasTagMap::const_iterator iter = _IPAAliasTageMap.find(ipa_map_id);
       if (iter != _IPAAliasTageMap.end())
        return (AliasTag)iter->second;

       return InvalidAliasTag;
     }
   }

   UINT32 aliasQueryCount(void)      { return _aliasQueryCount; }
   UINT32 incrAliasQueryCount(void)  { return _aliasQueryCount++; }
   UINT32 incrAliasedCount(void)     { return _aliasedCount++; }

   // IPA preopt related
   bool ipaMode() const { return _ipaMode; }
   void ipaMode(bool ipaMode) { _ipaMode = ipaMode; } 
   UINT32 curFilePUIdx() const {return _curFilePUIdx; }
   void curFilePUIdx(UINT32 filePUIdx) { _curFilePUIdx = filePUIdx; }

   bool checkQueryFile(UINT32 pu, AliasTag tag1, AliasTag tag2, bool &result);

   WN_MAP aliasTagMap() { return _aliasTagMap; }

private:

   void loadQueryFile(char *filename);
};

#endif // alias_analysis_INCLUDED

//
// Created by Jason Lu on 3/8/2018.
//

#ifndef OSPREY_JSON_SYMBOLTREE_H
#define OSPREY_JSON_SYMBOLTREE_H

#include "jgen_include.h"
#include "jgen_base_decl.h"
#include <json/json.h>
#include <vector>

namespace JGEN {


class Json_SymbolTree_Simple : public JGEN_SymbolTree_Base {

 public:

  Json::Value *_tree;
  Json::Value *_currentptr{};
  int _currentId;

  Json_SymbolTree_Simple() {
    _tree = nullptr;
    _currentId = 0;
    _currentptr = nullptr;
  }

  Json_SymbolTree_Simple(Json::Value &symtree) {
    _tree = nullptr;
    _currentId = 0;
    _currentptr = nullptr;
    init((void *) &symtree);
  }

  void init(void *tree) override;

  // read Kind from Node
  U64U getKind(U32U pos) override;
  // read Name from Node
  std::string getKindName(U32U pos) override;
  // read Kind from Node
  U64U getFlag(U32U pos) override;
  // get DefId form Node
  int getJsonRefId(U32U pos) override;

  // retrieve the previously bound Idx
  int getIdx(U32U pos) override;

  //      Json_MemberFields &getMemberFields(int idx) override ;
  int gotoStId(U32U pos) override ;

  bool isPublic (U32U jIndex) override
  {
    return (getFlag(jIndex) & JGEN_ST_FLAG_MASK_PUBLIC) > 0;
  }
  bool isWeak (U32U jIndex) override
  {
    return (getFlag(jIndex) & JGEN_ST_FLAG_MASK_WEAK) > 0;
  }
  bool isConstructor (U32U jIndex)  override;
  string getNameString(U32U jIndex) override;
  std::vector<U32U> getMemberFields(U32U jIndex) override;
  U32U getParent(U32U jIndex) override;
  bool isPureVFunc (U32U jIndex)  override;
  bool isMethodOfClass (U32U jIndex)  override;
  U32U get_method_base_type (U32U jIndex)  override;
  bool isContextNamespace (U32U jIndex)  override;
  bool isContextRecord (U32U jIndex)  override;
  bool isLangSpecific (U32U jIndex)  override;
  bool isReallyExtern (U32U jIndex)  override;
  bool isNoThrow (U32U jIndex)  override;
  std::map<U32U, Json::Value *> internalIdValMap;
  static Json_SymbolTree_Simple *me;
  void setTypeIdx(U32U jIndex, TY_IDX ty_idx) override;
  bool isInitial(U32U jIndex) override;
  bool isCommon(U32U jIndex) override;
  bool isExternal(U32U jIndex) override;
  bool isStatic(U32U jIndex) override;
  bool hasName(U32U jIndex) override;
  bool is_guard_var(U32U jIndex) override;
  U32U getLineNum(U32U jIndex) override;
  U32U get_sym_type(U32U jIndex) override;
};

}


#endif //OSPREY_JSON_SYMBOLTREE_H

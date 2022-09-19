//
// Created by xc5 on 3/8/2018.
//

#ifndef OSPREY_JSON_TYPETREE_H
#define OSPREY_JSON_TYPETREE_H

#include <json/json.h>
#include "jgen_base_decl.h"
#include "jgen_include.h"
#include <map>

namespace JGEN{

class Json_Typetree_Simple : public JGEN_Typetree_Base {

 public:

  Json::Value * _tree;
  Json::Value *_currentptr;
  int _currentId;
  std::map<U32U, Json::Value *> internalIdValMap;
  static Json_Typetree_Simple *me;

  Json_Typetree_Simple() {
    _currentptr = nullptr;
    _currentId = 0;
    _tree = nullptr;
  }

  Json_Typetree_Simple(Json::Value & val) {
    _tree = nullptr;
    _currentptr = nullptr;
    _currentId = 0;
    init(val);
  }

  void init(Json::Value &tree);
  U64U getKind(U32U jIndex) override;
  string getKindName(U32U jIndex) override;
  U64U getFlag(U32U jIndex) override;
  int getJsonRefId(U32U jIndex) override;
  string getNameString(U32U jIndex) override;
  int getIdx(U32U jIndex) override;
  void setTypeIdx(U32U jIndex, int idx) override;
  void getTypeIdx(U32U jIndex) override;
  bool isZeroMaxValue(U32U jIndex) override;
  bool isSizeMaxValueConstant(U32U jIndex) override;
  bool isVariableSize(U32U jIndex) override;
  int get_type_size(U32U jIndex) override;
  int get_args_count(U32U jIndex) override;
  bool isUnsigned(U32U jIndex) override;
  U32U get_element_type(U32U jIndex) override;
  bool isConst(U32U jIndex) override;
  int get_element_size_unit(U32U jIndex) override;
  long get_max_value(U32U jIndex) override;
  bool isReadonly(U32U jIndex) override;
  bool isVolatile(U32U jIndex) override;
  bool isRestrict(U32U jIndex) override;
  U32U getRetVal(U32U jIndex) override;
  vector<U32U> getArgs(U32U jIndex) override;
  bool isLangSpecific(U32U jIndex) override;
  bool isAnonymous(U32U jIndex) override;
  bool isAggregateValue(U32U jIndex) override;
  int getAlignWidth(U32U jIndex) override;
  vector<U32U> getFields(U32U jIndex) override;

  virtual ~Json_Typetree_Simple();
 private:
  int gotoId(U32U jIndex) override;
};

}

#endif //OSPREY_JSON_TYPETREE_H

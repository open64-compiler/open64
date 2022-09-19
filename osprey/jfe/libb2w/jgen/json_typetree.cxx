//
// Created by xc5 on 3/8/2018.
//

#include "json_typetree.h"
#include "jgen_global.h"

namespace JGEN {

Json_Typetree_Simple * Json_Typetree_Simple::me = nullptr;

void Json_Typetree_Simple::init(Json::Value &tree) {
  // Take whatever needed form tree
  // Mark the length, and current cursot to zero.
  _tree = &tree;
  int count = this->_tree->size();
  for (int i = 0; i < count; i++) {
    Json::Value *decl = (new Json::Value(Json::nullValue));
    Json::Value val = (*_tree)[i];
    decl->copy(val);
    internalIdValMap.insert(std::make_pair(100 + i, decl));
  }
}


U64U json_ty_kind_list[] = {9,12,11};
U64U internal_ty_kind_list[] = {JGEN_TYPE_RECORD,JGEN_TYPE_PACKAGE,JGEN_TYPE_METHOD};

U64U Json_Typetree_Simple::getKind(U32U jIndex) {
  gotoId(jIndex);
  U64U jsonkind = 0;
  if(_currentptr != nullptr){
    jsonkind = (*_currentptr)["kind"].asUInt64();
  }else if(_tree != nullptr){
    jsonkind = (*_tree)["kind"].asUInt64();
  }
  if(jsonkind == 0)  return 0;
  // Low efficiency method? map better?
  {
    for(int i = 0; i < sizeof(json_ty_kind_list) ; i++){
      if(json_ty_kind_list[i] == jsonkind)
        return internal_ty_kind_list[i];
    }
  }
  return jsonkind;
}
std::string Json_Typetree_Simple::getKindName(U32U jIndex) {
  gotoId(jIndex);
  return (*_currentptr)["kindname"].asString();
}
U64U Json_Typetree_Simple::getFlag(U32U jIndex) {
  gotoId(jIndex);
  return (*_currentptr)["flag"].asUInt64();
}
int Json_Typetree_Simple::getJsonRefId(U32U jIndex) {
  gotoId(jIndex);
  return jIndex;
}
string Json_Typetree_Simple::getNameString(U32U jIndex) {
  gotoId(jIndex);
  return (*_currentptr)["name"].asString();
}
int Json_Typetree_Simple::getIdx(U32U jIndex) {
  gotoId(jIndex);
  return 0;
}
void Json_Typetree_Simple::setTypeIdx(U32U jIndex, int idx) {
  gotoId(jIndex);

}
void Json_Typetree_Simple::getTypeIdx(U32U jIndex) {
  gotoId(jIndex);

}
bool Json_Typetree_Simple::isZeroMaxValue(U32U jIndex) {
  gotoId(jIndex);
  return false;
}
bool Json_Typetree_Simple::isSizeMaxValueConstant(U32U jIndex) {
  gotoId(jIndex);
  return false;
}
bool Json_Typetree_Simple::isVariableSize(U32U jIndex) {
  gotoId(jIndex);
  return false;
}
// for Variable length stuff.
int Json_Typetree_Simple::get_type_size(U32U jIndex) {
  gotoId(jIndex);
  return 0;
}

int Json_Typetree_Simple::get_args_count(U32U jIndex) {
  gotoId(jIndex);
  return 0;
}
bool Json_Typetree_Simple::isUnsigned(U32U jIndex) {
  gotoId(jIndex);
  return false;
}
U32U Json_Typetree_Simple::get_element_type(U32U jIndex) {
  gotoId(jIndex);
  return 0;
}
bool Json_Typetree_Simple::isConst(U32U jIndex) {
  gotoId(jIndex);
  return false;
}
int Json_Typetree_Simple::get_element_size_unit(U32U jIndex) {
  gotoId(jIndex);
  return 0;
}
long Json_Typetree_Simple::get_max_value(U32U jIndex) {
  gotoId(jIndex);
  return 0;
}
bool Json_Typetree_Simple::isReadonly(U32U jIndex) {
  gotoId(jIndex);
  return false;
}
bool Json_Typetree_Simple::isVolatile(U32U jIndex) {
  gotoId(jIndex);
  return false;
}
bool Json_Typetree_Simple::isRestrict(U32U jIndex) {
  gotoId(jIndex);
  return false;
}
U32U Json_Typetree_Simple::getRetVal(U32U jIndex) {
  gotoId(jIndex);
  return 0;
}
vector<U32U> Json_Typetree_Simple::getArgs(U32U jIndex) {
  gotoId(jIndex);
  return vector<U32U>();
}

int Json_Typetree_Simple::gotoId(U32U jIndex) {
  if(internalIdValMap.find((int) jIndex) != internalIdValMap.end()) {
    Json::Value * ptr = internalIdValMap.at((int) jIndex);
    _currentptr = ptr;
    return 0;
  }
  logger("-- JSON_ERROR : [Json_TypeTree_Simple][gotoId] cannot find such typeId in map.");
  return -2;
}
bool Json_Typetree_Simple::isLangSpecific(U32U jIndex) {
  return true;
}
bool Json_Typetree_Simple::isAnonymous(U32U jIndex) {
  return false;
}
bool Json_Typetree_Simple::isAggregateValue(U32U jIndex) {
  return false;
}
int Json_Typetree_Simple::getAlignWidth(U32U jIndex) {
  return 0;
}
vector<U32U> Json_Typetree_Simple::getFields(U32U jIndex) {
  return vector<U32U>();
}

Json_Typetree_Simple::~Json_Typetree_Simple() {

};

}

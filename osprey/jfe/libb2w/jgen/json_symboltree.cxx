//
// Created by xc5 on 3/8/2018.
//

#include "json_symboltree.h"
#include "jgen_global.h"

namespace JGEN{


Json_SymbolTree_Simple * Json_SymbolTree_Simple::me = nullptr;

void Json_SymbolTree_Simple::init(void * tree) {
  int i;
  FmtAssert(tree != nullptr, ("Error [init][symtree] nullptr"));
  this->_tree = (Json::Value *) tree;
  // length
  int count = this->_tree->size();
  for (i = 0; i < count; i++) {
    Json::Value *decl = (new Json::Value(Json::nullValue));
    Json::Value val = (*_tree)[i];
    decl->copy(val);
    internalIdValMap.insert(std::make_pair(100 + i, decl));
  }
}

U64U json_kind_list[] = {1, 2, 16, 4};
U64U internal_kind_list[] = {JGEN_ST_PACKAGE, JGEN_ST_CLASS, JGEN_ST_METHOD, JGEN_ST_VAR};

U64U Json_SymbolTree_Simple::getKind(U32U jIndex) {
  gotoStId(jIndex);
  U64U jsonkind = 0;
  if(_currentptr != nullptr){
    jsonkind = (*_currentptr)["kind"].asUInt64();
  }else{
    return 0;
  }
  if(jsonkind == 0)  return 0;
  // Low efficiency method? map better?
  {
    for(int i = 0; i < sizeof(json_kind_list) ; i++){
      if(json_kind_list[i] == jsonkind)
        return internal_kind_list[i];
    }
  }
  return jsonkind;
}

std::string Json_SymbolTree_Simple::getNameString(U32U jIndex) {
  gotoStId(jIndex);
  if(_currentptr != nullptr){
    return (*_currentptr)["name"].asString();
  }
  return *(new string("[JSON_Symbol_TREE]"));
}

int Json_SymbolTree_Simple::gotoStId(U32U ir_sym_id){
  // indexed doing
  if(internalIdValMap.find((int) ir_sym_id) != internalIdValMap.end()) {
    Json::Value * ptr = internalIdValMap.at((int) ir_sym_id);
    _currentptr = ptr;
    return 0;
  }
  logger("-- JSON_ERROR : [Json_SymbolTree_Simple][gotoStId] cannot find such symId in map.");
  return -2;
}

vector<U32U> Json_SymbolTree_Simple::getMemberFields(U32U jIndex) {
  return vector<U32U>();
}
U32U Json_SymbolTree_Simple::getParent(U32U jIndex) {
  return 0;
}

std::string Json_SymbolTree_Simple::getKindName(U32U pos) {
  gotoStId(pos);
  if(_currentptr != nullptr){
    return (*_currentptr)["kindname"].asString();
  }
  return *(new string("[JSON_Symbol_TREE]"));
}
U64U Json_SymbolTree_Simple::getFlag(U32U pos) {
  gotoStId(pos);
  if(_currentptr != nullptr){
    return (*_currentptr)["flag"].asUInt64();
  }
  return 0;
}
int Json_SymbolTree_Simple::getJsonRefId(U32U pos) {
  gotoStId(pos);
  return 0;
}

int Json_SymbolTree_Simple::getIdx(U32U pos) {
  gotoStId(pos);
  return 0;
}

bool Json_SymbolTree_Simple::isConstructor(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}
bool Json_SymbolTree_Simple::isPureVFunc(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}
bool Json_SymbolTree_Simple::isMethodOfClass(U32U jIndex) {
  gotoStId(jIndex);
  return (*_currentptr)["kind"].asUInt() == 16;
}
TY_IDX Json_SymbolTree_Simple::get_method_base_type(U32U jIndex) {
  gotoStId(jIndex);
  return (*_currentptr)["type"].asUInt();
}
bool Json_SymbolTree_Simple::isContextNamespace(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}
bool Json_SymbolTree_Simple::isContextRecord(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}
bool Json_SymbolTree_Simple::isLangSpecific(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}
bool Json_SymbolTree_Simple::isReallyExtern(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}
bool Json_SymbolTree_Simple::isNoThrow(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}

void Json_SymbolTree_Simple::setTypeIdx(U32U jIndex, TY_IDX ty_idx) {
  gotoStId(jIndex);
}
bool Json_SymbolTree_Simple::isInitial(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}
bool Json_SymbolTree_Simple::isCommon(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}
bool Json_SymbolTree_Simple::isExternal(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}
bool Json_SymbolTree_Simple::isStatic(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}
bool Json_SymbolTree_Simple::hasName(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}
bool Json_SymbolTree_Simple::is_guard_var(U32U jIndex) {
  gotoStId(jIndex);
  return false;
}
U32U Json_SymbolTree_Simple::getLineNum(U32U jIndex) {
  gotoStId(jIndex);
  return 0;
}
U32U Json_SymbolTree_Simple::get_sym_type(U32U jIndex) {
  gotoStId(jIndex);
  return (*_currentptr)["type"].asUInt();
}

}
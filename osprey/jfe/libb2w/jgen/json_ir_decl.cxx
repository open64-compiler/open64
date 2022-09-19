//
// Created by xc5 on 27/7/2018.
//

#include "json_ir_decl.h"
#include "json_reader.h"
#include "jgen_include.h"
#include "jgen_global.h"
#include "json_symboltree.h"
#include "json_typetree.h"

namespace JGEN
{

Json_IR_Decl::Json_IR_Decl (Json::Value &code_table_, Json::Value &root_)
{
  root = root_;
  decl = code_table_;

  child_count = 0;

  tag_json = decl["tag"].asInt ();
  tag_name = decl["tag_name"].asString ();

  switch (tag_json)
    {
  case 3: // CLASSDEF
    kind = JGEN_DECL_CLASS;
      if (!decl["defs"].isNull ())
        {
          child_count = decl["defs"].size ();
        }
      break;
  case 4: // METHODDEF
    kind = JGEN_DECL_METHOD;
      if (!decl["body"].isNull ())
        {
          child_count = 1;
        }
      break;
  case 7:
    kind = JGEN_DECL_BLOCK;
    if (!decl["body"].isNull ())
      {
        child_count = 1;
      }
    break;
  case 20:// EXEC
      kind = JGEN_DECL_STMT;
      break;
  case 37://IDENT
      kind = JGEN_DECL_OPERATOR;
  case 26://APPLY
      kind = JGEN_DECL_OPERATOR;
      break;
  default:kind = JGEN_DECL_UNKNOWN_KIND;
      break;
    }
  if (decl["type"].isInt ())
  {
      type_json_id = decl["type"].asInt ();
  }
  if (decl["sym"].isInt ())
  {
      symbol_json_id = decl["sym"].asInt ();
  }
}


int JGEN::Json_IR_Decl::hasChild ()
{
  return child_count;
}

Json_IR_Decl *Json_IR_Decl::getChildAtPosition (unsigned int pos)
{

  if (child_count <= pos)
    {
      logger ("");
      return nullptr;
    }

  if (kind == JGEN_DECL_CLASS)
    {
      if (!decl["defs"].empty())
        {
          return (new Json_IR_Decl (decl["defs"][0], root));
        }
      return 0;
    }
  else if (kind == JGEN_DECL_METHOD)
    {
      if (!decl["body"].isNull ())
        {
          return (new Json_IR_Decl (decl["body"], root));
        }
      return 0;
    }
  else if (kind == JGEN_DECL_BLOCK)
    {
      if (!decl["stats"].isNull ())
        {
          return (new Json_IR_Decl (decl["stats"], root));
        }
      return 0;
    }

  return nullptr;
}
int JGEN::Json_IR_Decl::getDeclKind ()
{
  return kind;
}

U64U Json_IR_Decl::getKind() {
  return kind;
}

const Json::Value &Json_IR_Decl::getRoot() {
  return root;
}
void Json_IR_Decl::setRoot(const Json::Value &root) {
  Json_IR_Decl::root = root;
}
const Json::Value &Json_IR_Decl::getDecl() {
  return decl;
}
void Json_IR_Decl::setDecl(const Json::Value &decl) {
  Json_IR_Decl::decl = decl;
}
int Json_IR_Decl::getSymbolId() {
  return symbol_json_id;
}
int Json_IR_Decl::getTypeId() {
  return type_json_id;
}
int Json_IR_Decl::getTag_json() {
  return tag_json;
}
void Json_IR_Decl::setTag_json(int tag_json) {
  Json_IR_Decl::tag_json = tag_json;
}
void Json_IR_Decl::setKind(int kind) {
  Json_IR_Decl::kind = kind;
}

const string &Json_IR_Decl::getTag_name() {
  return tag_name;
}

void Json_IR_Decl::setTag_name(const string &tag_name) {
  Json_IR_Decl::tag_name = tag_name;
}

int Json_IR_Decl::getChild_count() {
  return child_count;
}

JGEN_SymbolTree_Base * Json_IR_Decl::get_symbol_tree() {
  if(Json_SymbolTree_Simple::me != nullptr) return Json_SymbolTree_Simple::me;
  Json_SymbolTree_Simple * sym = new Json_SymbolTree_Simple(root["symbol_table"]);
  Json_SymbolTree_Simple::me = sym;
  return sym;
}
JGEN_Typetree_Base * Json_IR_Decl::get_type_tree() {
  if(Json_Typetree_Simple::me != nullptr) return Json_Typetree_Simple::me;
  Json_Typetree_Simple * typ = new Json_Typetree_Simple(root["type_table"]);
  Json_Typetree_Simple::me = typ;
  return (JGEN_Typetree_Base *) typ;
}

}
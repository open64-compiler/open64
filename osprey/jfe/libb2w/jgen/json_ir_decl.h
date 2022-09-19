#ifndef OSPREY_JGEN_DECL_H
#define OSPREY_JGEN_DECL_H

#include <string>
#include <iostream>
#include "jgen_include.h"
#include <json/json.h>
#include "jgen_base_decl.h"

using std::string;

namespace JGEN{

    /**
       *  Json IR Decl Interface
       * */
    class Json_IR_Decl : public JGEN_IR_Decl{
     private:
       Json::Value root;
       Json::Value decl;
       int symbol_json_id;
       int type_json_id;
       int tag_json;
       int kind;
       string tag_name;
       int child_count;

     public:

      Json_IR_Decl(Json::Value & code_table, Json::Value & root);

      /**
       *   check if have child (childnum)
       *   @return remainning child nums;
       */
      int hasChild() override;

      /**
       *  Returns a child
       */
      Json_IR_Decl * getChildAtPosition (unsigned int pos) override;

      U64U getKind() override;
      int getChild_count() override;

      JGEN_SymbolTree_Base * get_symbol_tree() override;
      JGEN_Typetree_Base * get_type_tree() override;

      int getSymbolId() override;
      int getTypeId() override;


      /***
       *  Get the Kind of the Decl
       *  @return longlong JGEN_DECL_CLASS
       */
      int getDeclKind();
      const Json::Value &getRoot();
      void setRoot(const Json::Value &root);
      const Json::Value &getDecl();
      void setDecl(const Json::Value &decl);
      int getTag_json();
      void setTag_json(int tag_json);
      void setKind(int kind);
      const string &getTag_name();
      void setTag_name(const string &tag_name);

    };
}

#endif
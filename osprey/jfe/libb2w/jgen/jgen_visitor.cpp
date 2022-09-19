//
// Created by xc5 on 27/7/2018.
//

#include <sstream>
#include "jgen_visitor.h"
#include "json_ir_decl.h"
#include "jgen_global.h"
#include "json_reader.h"
using std::stringstream;

namespace JGEN
{

    JGEN_SymbolTree_Base * JGEN_Visitor::symtree = nullptr;
    JGEN_Typetree_Base * JGEN_Visitor::typetree = nullptr;

    int JGEN_Visitor::visit_top_decl (JGEN_IR_Decl & provider)
    {
        if(symtree == nullptr || typetree == nullptr){
            symtree = provider.get_symbol_tree();
            typetree = provider.get_type_tree();
            JGEN_TY::typetree = typetree;
            JGEN_ST::symtree = symtree;
        }
        // STAGE-PARSING1
        visit_first_round(provider);
        visit_second_round(provider);
        visit_child_round(provider);
        return 0;
    }
    int JGEN_Visitor::visit_decl (JGEN_IR_Decl & provider)
    {
        // If it's a class,
        visit_first_round(provider);
        visit_second_round(provider);
        visit_child_round(provider);
        return 0;
    }


    void JGEN_Visitor::visit_first_round (JGEN_IR_Decl &provider)
    {
        switch(provider.getKind ()){
            case JGEN_DECL_CLASS:
              logger("-- [Json_Visitor]:: ***  visiting a class  defition.  ***");
              JGEN_ST::Get_ST(provider.getSymbolId());
              // Preserve symbol_json_id <--> ST_IDX
              break;
            case JGEN_DECL_METHOD:
              logger("-- [Json_Visitor]:: ***  visiting a method defition.  ***");
              JGEN_ST::Get_ST(provider.getSymbolId());
              break;
            case JGEN_DECL_VAR:
              logger("-- [Json_Visitor]:: ***  visiting a var    defition.  ***");
              break;
            case JGEN_DECL_BLOCK:
              logger("-- [Json_Visitor]:: ***  visiting a block  defition.  ***");
              break;
            default:
              // UNKNOWN
              logger("-- [Jgen_Visitor]:: *** visiting a non-supported (kind) of provider decl. decl_kind : " + int2str(provider.getKind()) + " ***");
            return ;
        }
    }

    void JGEN_Visitor::visit_second_round (JGEN_IR_Decl &provider)
    {
      if(provider.getKind () == JGEN_DECL_CLASS){

      }
    }
    void JGEN_Visitor::visit_child_round (JGEN_IR_Decl & provider)
    {
        unsigned int position = 0;
        for(position = 0; position < provider.hasChild () && position <= INT_MAX ; position++)
        {
            JGEN_IR_Decl * decl = provider.getChildAtPosition (position);
            if (decl != nullptr){
                visit_decl (*decl);
            }
            delete decl;
        }
    }
}
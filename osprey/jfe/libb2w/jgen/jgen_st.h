#ifndef OSPREY_JGEN_ST_H
#define OSPREY_JGEN_ST_H

#include <vector>
#include <string>
#include <iostream>
#include "jgen_include.h"
#include "jgen_base_decl.h"
#include "jgen_type.h"

// Return Params

using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;

namespace JGEN
{
    extern BOOL JGEN_processing_function_prototypes;
    extern BOOL JGEN_Keep_Zero_Length_Structs;

    class JGEN_ST {

      static TY_IDX ty_idx;
      static ST *st;
      static ST_SCLASS sclass;
      static ST_EXPORT eclass;
      static SYMTAB_IDX level;
      static INT anon_count;

     public:

      static JGEN_SymbolTree_Base * symtree;
      static ST_IDX Get_ST(U32U jIndex);
      static ST_IDX create_func (U32U jIndex);
      static ST_IDX createParam (U32U jIndex);
      static ST_IDX createVar (U32U jIndex);
      static ST_IDX createClass (U32U jIndex);
      static ST_IDX createNameSpace (U32U jIndex);

     private:

      static TY_IDX get_related_TY (U32U jSymIndex)
      {
          U32U jTypeIndex = symtree->get_sym_type(jSymIndex);
          return JGEN_TY::Get_TY(jTypeIndex);
      }

      static void JGEN_add_pragma_to_location (WN_PRAGMA_ID id, ST *st)
      {

      }

      static U32U getSymtabLevel(U32U index);
    };

}

#endif

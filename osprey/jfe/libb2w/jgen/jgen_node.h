#ifndef JGEN_NODE_H
#define JGEN_NODE_H

#include <json/json.h>
#include <string>
#include <vector>
#include <fstream>
#include "jgen_include.h"
#include "jgen_st.h"
#include "jgen_global.h"
#include "json_reader.h"
#include "json_ir_decl.h"

/***
 *
 *    JGEN Node is the key to the whole Json->Whirl Generation Process
 *
 *    Prerequisite Class :
 *    Json_IR
 *    J
 *
 *
 *    Key Class:
 *
 *
 *
 *
 */

using JGEN::Json_IR;
using JGEN::JGEN_Typetree_Base;

/***
    WGEN_decl Ended
 ***/

using std::string;
using std::vector;

namespace JGEN
{

    class JGEN_NODE {
     protected:
      WN * whirl;
      string name;
      void * json_value;
      JGEN_NODE * root;
     public:
      JGEN_NODE ()
      {
        whirl = nullptr;
        root = nullptr;
        name = "empty_node";
        json_value = nullptr;
      }

      void set_line_info_and_file (int line, string &fn);
    };

    class JGEN_Variable : public JGEN_NODE {
     public:
      JGEN_Variable () = default;;
    };

    class JGEN_Root : public JGEN_NODE {
     protected:
      string output_file;
     public:
      JGEN_Root ()
      {
        output_file = "default.B";
      }
      const string &getOutput_file () const;
      void setOutput_file (const string &output_file);
      explicit JGEN_Root (string &out)
      {
        output_file = out;
      };
      void init ();
      void init (string &fn);
      void finish ();
      void write_types (JGEN_Typetree_Base type_tree);
      void traverse_decl (JGEN_IR_Decl * decl);

    };

    class JGEN_Function : public JGEN_NODE {
     private:
      JGEN_NODE parent;
     public:
      JGEN_Function (JGEN_Root & parent)
      {
        root = &parent;

        if (!map_mempool_initialized)
          {
            MEM_POOL_Initialize(&Map_Mem_Pool, "Map_Mem_Pool", FALSE);
            map_mempool_initialized = TRUE;
          }
        else
          {
            MEM_POOL_Pop(&Map_Mem_Pool);
          }

        /* create the map table for the next PU */
        (void) WN_MAP_TAB_Create (&Map_Mem_Pool);
        New_Scope (CURRENT_SYMTAB + 1, Malloc_Mem_Pool, TRUE);
        //jgen_set_line_and_file (lineno, fn);
      }
    };
}
#endif

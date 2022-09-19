/**
 * Json AST Reading Base Class Declaration
 * Author: Jason Lu : lu.gt@163.com
 * Version : V0.1 
 * Usage:
 *  1. Instantiate the JGEN::Json_IR class,
 *  2. JsonIR::open (file_name)
 *  3. JsonIR::read ()
 ***/

#ifndef JSON_READER_H_
#define JSON_READER_H_

#include <json/json.h>
#include <map>
#include "json_ir_decl.h"
#include "jgen_base_decl.h"

extern int JSON_READING_STATE;

namespace JGEN {


    class Json_MemberFields {
        void init(Json::Value &val);

        int next();

        std::string getName();

        int getIdx();

        unsigned long long getKind();

        unsigned long long getFlag();
    };

    class Json_Global_ST {
        void init(Json::Value &val);

        int next();

        std::string getName();

        int getIdx();

        unsigned long long getKind();

        unsigned long long getFlag();
    };

    class Json_Method_Stmt_Tree {
        void init(Json::Value &val);
    };

    class Json_Global_Method {
        void init(Json::Value &val);

        int next();

        std::string getName();

        int getIdx();

        unsigned long long getKind();

        unsigned long long getFlag();

        Json_Method_Stmt_Tree getStmt();
    };



    class Json_IR {
    private:
        Json::Reader reader;
        Json::Value root;
        const char *fn;

    public:

        Json_IR(){};

        int read();

        int read_string(const char *str);

        int open(const char *fn);

        Json::Value get_defs();

        Json::Value get_type_tree();

        Json::Value get_sym_tree();

        JGEN_IR_Decl * get_top_decl ();

      bool isNull();
    };


}


#endif

/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

#include "json_reader.h"
#include <string>
#include <iostream>
#include "jgen_node.h"
#include "jgen_global.h"

using std::string;
using std::cout;
using std::endl;
using namespace JGEN;


namespace JGEN {


    void parse_class_def(Json_IR &ir, JGEN_Root & root) {
      // Predef
      // Type
      Json::Value types = ir.get_type_tree();

      //Json_Typetree_Simple tptr(types);
      //root.write_types(tptr);

      // Defs . Member Fields
      Json::Value defs = ir.get_defs();
    }

    void parse_one_function(JGEN::Json_IR ir) {

    }

    std::string infn;
    std::string outfn;

}

int main(int argc, char ** argv){
  outfn = "out.B";
  JGEN_Root root;

  if(argc >= 2){
    cout << "---------  File:  ---------"<< endl
	 << argv[1] << endl;
    JGEN::Json_IR ir;
    cout << endl << "---------       [IR] Opening       --------" << endl;
    ir.open(argv[1]);
    cout << endl << "---------       [IR] Reading       --------" << endl;
    ir.read();
    cout << endl << "-------- [Gen:Root] Init root  -------------" << endl;
    root.init(outfn);
    cout << endl << "-------- [Gen:Root] init finished ----------" << endl;
    cout << endl << "-------- [Test] parse_class_def ------------" << endl;
    parse_class_def(ir, root);
    cout << endl << "-------- [Test] parse_class_def finished ---" << endl;
    root.finish();
    cout << endl << "-------- [Gen:root] finished --------" << endl;
    cout << endl << "-------- [Jwtest] all procedure finished  --------" << endl;
  }else{
    cout << "----------   Usage -------------" << endl;
    cout << " -  jwtest <json_file_path> [...arguments] " << endl;
    cout << "-------- File Not Given --------" << endl;
    return -1;
  }
  return 0;
}

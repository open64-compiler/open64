/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan PSL v1.
 * You can use this software according to the terms and conditions of the Mulan PSL v1.
 * You may obtain a copy of Mulan PSL v1 at:
 *
 *     http://license.coscl.org.cn/MulanPSL
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v1 for more details.
 */

#include "mir_parser.h"
#include "bin_mplt.h"
#include "opcode_info.h"
#include <cstdlib>
#include "mir_function.h"
#include "constant_fold.h"
#include "mir_type.h"
#include <iostream>
#include <fstream>

using namespace maple;

std::unordered_set<std::string> dumpFuncSet = {};

bool ConstantFoldModule(maple::MIRModule *module) {
  maple::ConstantFold cf(module);
  MapleVector<maple::MIRFunction *> &funcList = module->functionList;
  for (MapleVector<maple::MIRFunction *>::iterator it = funcList.begin(); it != funcList.end(); it++) {
    maple::MIRFunction *curfun = *it;
    maple::BlockNode *block = curfun->body;
    module->SetCurFunction(curfun);
    if (!block) {
      continue;
    }
    cf.Simplify(block);
  }
  return true;
}

int main(int argc, char **argv) {
  if (argc < 2) {
    MIR_PRINTF(
      "usage: ./irbuild [-b] [-dumpfunc=<string>] <any number of .mplt, .mpl, .bpl or .tmpl files>\n"
      "    By default, the files are converted to corresponding ascii format.\n"
      "    If -b is specified, output is binary format instead.\n"
      "    If -dumpfunc= is specified, only functions with name containing the string is output.\n"
      "    -dumpfunc= can be specified multiple times to give multiple strings.\n"
      "    Each output file has .irb added after its file stem.\n");
    exit(1);
  }
  std::vector<maple::MIRModule *> themodule(argc, nullptr);
  bool useBinary = false;
  // process the options which must come first
  maple::int32 i = 1;
  while (argv[i][0] == '-' ) {
    if (argv[i][1] == 'b' && argv[i][2] == '\0') {
      useBinary = true;
    } else if (strncmp(argv[i], "-dumpfunc=", 10) == 0 && strlen(argv[i]) > 10) {
      std::string funcName(&argv[i][10]);
      dumpFuncSet.insert(funcName);
    } else {
      ERR(kLncErr, "irbuild: unrecognized command line option");
      return 1;
    }
    i++;
  }
  // process the input files
  while (i < argc) {
    themodule[i] = new maple::MIRModule(argv[i]);
    std::string::size_type lastdot = themodule[i]->fileName.find_last_of(".");
    bool ismplt = themodule[i]->fileName.compare(lastdot, 5, ".mplt") == 0;
    bool istmpl = themodule[i]->fileName.compare(lastdot, 5, ".tmpl") == 0;
    bool ismpl = themodule[i]->fileName.compare(lastdot, 5, ".mpl\0") == 0;
    bool isbpl = themodule[i]->fileName.compare(lastdot, 5, ".bpl\0") == 0;
    if (!ismplt && !istmpl && !ismpl && !isbpl) {
      ERR(kLncErr, "irbuild: input must be .mplt or .mpl or .bpl or .tmpl file");
      return 1;
    }
    // input the file
    if (ismpl || istmpl) {
      maple::MIRParser theparser(*themodule[i]);
      if (!theparser.ParseMIR()) {
        theparser.EmitError(themodule[i]->fileName.c_str());
        return 1;
      }
    } else {
      BinaryMplImport binMplt(*themodule[i]);
      binMplt.imported = false;
      std::string modid = themodule[i]->fileName;
      if (!binMplt.Import(modid, true)) {
        ERR(kLncErr, "irbuild: cannot open .mplt or .bpl file: %s", modid.c_str());
        return 1;
      }
    }
    /* ConstantFoldModule(themodule[i]); */

    // output the file
    if (!useBinary) {
      themodule[i]->OutputAsciiMpl(".irb", (ismpl || isbpl) ? ".mpl" : ".tmpl", &dumpFuncSet, true, false);
    } else {
      BinaryMplt binMplt(*themodule[i]);
      std::string modid = themodule[i]->fileName;
      binMplt.GetBinExport().not2mplt = ismpl || isbpl;
      std::string filestem = modid.substr(0, lastdot);
      binMplt.Export(filestem + ((ismpl || isbpl) ? ".irb.bpl" : ".irb.mplt"), &dumpFuncSet);
    }
    i++;
  }
  return 0;
}

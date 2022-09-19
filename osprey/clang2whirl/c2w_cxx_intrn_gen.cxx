/*
  Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement
  or the like.  Any license provided herein, whether implied or
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with
  other software, or any other product whatsoever.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  http://www.xcalibyte.com

  For more information, see:
  http://github.com/open64-compiler/open64
  http://gitee.com/open64-compiler/open64

*/

// generate files for C++ standard library/well-defined 3rd party library
// include:
// common/com/intrn_entry_cxx_<prefix>.def:
//   defines all intrinsics to replace template functions defined in
//   std/3rd party library
// clang2whirl/c2w_intrn_cxx_<prefix>.inc:
//   compare the function name and return corresppoonding cxx intrinsic ID
// be/rbc/certc/rbc_intrn_cxx_<prefix>.inc
//   skeleton for cxx intrinsic function rule model and checking
//

#include "c2w_builder.h"
#include "c2w_decl.h"
#include <fstream>
#include <unordered_set>

using namespace clang;

#define COMMON_INTRN_ENTRY_HEADER \
  "/*\n" \
  " * Copyright (C) %s Xcalibyte (Shenzhen) Limited.\n" \
  " */\n" \
  "\n" \
  "/*\n" \
  "  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.\n\n" \
  "  This program is free software; you can redistribute it and/or modify it\n" \
  "  under the terms of version 2 of the GNU General Public License as\n" \
  "  published by the Free Software Foundation.\n\n" \
  "  This program is distributed in the hope that it would be useful, but\n" \
  "  WITHOUT ANY WARRANTY; without even the implied warranty of\n" \
  "  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n" \
  "  Further, this software is distributed without any warranty that it is\n" \
  "  free of the rightful claim of any third person regarding infringement\n" \
  "  or the like.  Any license provided herein, whether implied or\n" \
  "  otherwise, applies only to this software file.  Patent licenses, if\n" \
  "  any, provided herein do not apply to combinations of this program with\n" \
  "  other software, or any other product whatsoever.\n\n" \
  "  You should have received a copy of the GNU General Public License along\n" \
  "  with this program; if not, write the Free Software Foundation, Inc., 59\n" \
  "  Temple Place - Suite 330, Boston MA 02111-1307, USA.\n\n" \
  "  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,\n" \
  "  Mountain View, CA 94043, or:\n\n" \
  "  http://www.sgi.com\n\n" \
  "  For further information regarding this notice, see:\n\n" \
  "  http://oss.sgi.com/projects/GenInfo/NoticeExplan\n\n" \
  "*/\n"

#define C2W_INTRN_HEADER \
  "/*\n" \
  "  Copyright (C) %s Xcalibyte (Shenzhen) Limited.\n\n" \
  "  This program is free software; you can redistribute it and/or modify it\n" \
  "  under the terms of version 2 of the GNU General Public License as\n" \
  "  published by the Free Software Foundation.\n\n" \
  "  This program is distributed in the hope that it would be useful, but\n" \
  "  WITHOUT ANY WARRANTY; without even the implied warranty of\n" \
  "  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n" \
  "  Further, this software is distributed without any warranty that it is\n" \
  "  free of the rightful claim of any third person regarding infringement\n" \
  "  or the like.  Any license provided herein, whether implied or\n" \
  "  otherwise, applies only to this software file.  Patent licenses, if\n" \
  "  any, provided herein do not apply to combinations of this program with\n" \
  "  other software, or any other product whatsoever.\n\n" \
  "  You should have received a copy of the GNU General Public License along\n" \
  "  with this program; if not, write the Free Software Foundation, Inc., 59\n" \
  "  Temple Place - Suite 330, Boston MA 02111-1307, USA.\n\n" \
  "  http://www.xcalibyte.com\n\n" \
  "  For more information, see:\n" \
  "  http://github.com/open64-compiler/open64\n" \
  "  http://gitee.com/open64-compiler/open64\n\n" \
  "*/\n"

#define RBC_MODEL_CHECK_HEADER \
  "/*\n" \
  "   Copyright (C) %s Xcalibyte (Shenzhen) Limited.\n\n" \
  "   Licensed under the Apache License, Version 2.0 (the \"License\");\n" \
  "   you may not use this file except in compliance with the License.\n" \
  "   You may obtain a copy of the License at\n\n" \
  "     http://www.apache.org/licenses/LICENSE-2.0\n\n" \
  "   Unless required by applicable law or agreed to in writing, software\n" \
  "   distributed under the License is distributed on an \"AS IS\" BASIS,\n" \
  "   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n" \
  "   See the License for the specific language governing permissions and\n" \
  "   limitations under the License.\n" \
  " */\n\n"

#define COMMON_INTRN_ENTRY_ITEM \
  "/* %s %s%s%s */\n" \
  "DEF_INTRN_ENTRY(INTRN_%s, \"%s\", BYVAL, NOT_PURE, SIDEEFFECTS,\n" \
  "                DOES_RETURN, NOT_ACTUAL, NOT_CGINTRINSIC, NOT_SLAVE,\n" \
  "                IRETURN_%s, \"%s\", \"%s%s%s\", \"%s%s\")\n"

#define C2W_INTRN_ITEM \
  "  /* %s%s */\n" \
  "  if (strcmp(\"%s\", fname) == 0) { return INTRN_%s; }\n"

#define RBC_MODEL_CHECK_ITEM \
  "/* %s%s */\n" \
  "#ifdef DECL_RBC_for%s\n" \
  "void %s()\n" \
  "{\n" \
  "  DECL_RBC_for%s();\n" \
  "}\n" \
  "#endif\n\n"

#define SBAR \
  "------------------------------------------------"

namespace wgen {

const char *
GetOperatorName(OverloadedOperatorKind kind) {
#define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) #Name,
  static const char *name[] = {
    "None",
# include "clang/Basic/OperatorKinds.def"
    "NumOverloadedOperators"
  };
#undef OVERLOADED_OPERATOR
  return (kind > OO_None && kind < NUM_OVERLOADED_OPERATORS) ? name[kind]
                                                             : "Error";
}

void
WhirlBuilder::InitGenCppIntrnFilter(const char *filter) {
  Is_True(GenCppIntrn(), ("-gen-cpp-intrn is off"));
  // filter: class1.method1,class1.method2,class2.*
  const char *ptr = filter;
  const char *cls_begin = NULL;
  const char *cls_end = NULL;
  const char *method_begin = NULL;
  const char *method_end = NULL;

  char ch;
  while ((ch = *ptr) != '\0') {
    if (isspace(ch)) {
      if (ch != '\r' && cls_begin != NULL) {
        printf("Error gen-cpp-intrn-filter: "
               "space inside %s name at position %ld, ignore rest filter string:\n%s\n",
               method_begin == NULL ? "class" : "method", ptr - filter, ptr);
        return;
      }
      ++ptr;
      continue;
    }
    if (cls_begin == NULL) {
      if (ch == '.' || ch == ',') {
        printf("Error gen-cpp-intrn-filter: "
               "no class found before `%c' at position %ld, ignore rest filter string:\n%s\n",
               ch, ptr - filter, ptr);
        return;
      }
      cls_begin = ptr;
    }
    else {
      if (ch == '.') {
        Is_True(cls_begin != NULL && cls_end == NULL &&
                method_begin == NULL && method_end == NULL,
                ("wrong state when hit `.'"));
        cls_end = ptr;
        method_begin = ptr + 1;
      }
      else if (ch == ':') {
        if (*(ptr + 1) != ':') {
          printf("Error gen-cpp-intrn-filter: "
                 "wrong character `%c' found after `:' at position %ld, ignore rest filter string:\n%s\n",
                 ch, ptr + 1 - filter, ptr + 1);
          return;
        }
        Is_True(cls_begin != NULL && cls_end == NULL &&
                method_begin == NULL && method_end == NULL,
                ("wrong state when hit `:'"));
        cls_end = ptr;
        method_begin = ptr + 2;
        ++ptr;
      }
      else if (ch == ',' || ch == '\n') {
        Is_True(cls_begin != NULL,
                ("wrong state when hit `,'"));
        if (cls_end == NULL) {
          Is_True(method_begin == NULL && method_end == NULL,
                  ("wrong state when hit `,'"));
          cls_end = ptr;
          wgen_tstr func(cls_begin, cls_end - cls_begin);
          _cpp_func_filter_set.insert(func);
          if (Verbose()) {
            fprintf(stderr, "INFO init-intrn-filter: %.*s\n",
                    (int)(cls_end - cls_begin), cls_begin);
          }
          cls_end = NULL;
        }
        else {
          Is_True(method_begin != NULL && method_end == NULL,
                  ("wrong state when hit `,'"));
          method_end = ptr;
          wgen_tstr cls(cls_begin, cls_end - cls_begin);
          wgen_tstr mtd(method_begin, method_end - method_begin);
          _cpp_class_filter_map[cls].insert(mtd);
          if (Verbose()) {
            fprintf(stderr, "INFO init-intrn-filter: %.*s.%.*s\n",
                    (int)(cls_end - cls_begin), cls_begin,
                    (int)(method_end - method_begin), method_begin);
          }
          // reset cls_begin/cls_end/method_begin/method_end
          cls_end = method_begin = method_end = NULL;
        }
        cls_begin = NULL;
      }
    }
    ++ptr;
  }
  // last entry
  if (cls_begin != NULL && method_begin != NULL) {
    Is_True(cls_end != NULL && method_end == NULL,
            ("wrong state when hit `,'"));
    method_end = ptr;
    wgen_tstr cls(cls_begin, cls_end - cls_begin);
    wgen_tstr mtd(method_begin, method_end - method_begin);
    _cpp_class_filter_map[cls].insert(mtd);
  }
}

void
WhirlBuilder::InitGenCppIntrn() {
  Is_True(GenCppIntrn(), ("-gen-cpp-intrn is off"));
  Is_True(_com_intrn_entry_file == NULL &&
          _c2w_intrn_use_file == NULL &&
          _rbc_intrn_model_file == NULL, ("file not NULL"));

  // process filter
  std::string &filter = _cpp_intrn_filter.getValue();
  if (!filter.empty()) {
    if (filter[0] == '@') {
      // read file if filter starts with '@'
      std::ifstream ifs(filter.c_str() + 1);
      filter.assign(std::istreambuf_iterator<char>(ifs),
                    std::istreambuf_iterator<char>());
    }
  }
  if (!filter.empty()) {
    InitGenCppIntrnFilter(filter.c_str());
  }

  // open files to write
  char filename[256];
  snprintf(filename, 256, "intrn_entry_cxx_%s.def", CppIntrnPrefix().c_str());
  _com_intrn_entry_file = fopen(filename, "w");
  Is_True(_com_intrn_entry_file != NULL,
          ("fail to open %s for write.\n", filename));

  snprintf(filename, 256, "c2w_intrn_cxx_%s.inc", CppIntrnPrefix().c_str());
  _c2w_intrn_use_file = fopen(filename, "w");
  Is_True(_c2w_intrn_use_file != NULL,
          ("fail to open %s for write.\n", filename));

  snprintf(filename, 256, "rbc_intrn_cxx_%s.inc", CppIntrnPrefix().c_str());
  _rbc_intrn_model_file = fopen(filename, "w");
  Is_True(_rbc_intrn_model_file != NULL,
          ("fail to open %s for write.\n", filename));

  if (Verbose()) {
    snprintf(filename, 256, "verbose_%s.list", CppIntrnPrefix().c_str());
    _verbose_list_file = fopen(filename, "w");
    Is_True(_rbc_intrn_model_file != NULL,
            ("fail to open %s for write.\n", filename));
  }

  time_t ctm = time(NULL);
  struct tm *tm = localtime(&ctm);
  char year_buf[32];
  int year = tm->tm_year + 1900;
  if (year == 2021) {
    strcpy(year_buf, "2021");
  }
  else {
    snprintf(year_buf, 32, "2021-%d", year);
  }
  char ts_buf[64];
  snprintf(ts_buf, 64, "%04d-%02d-%02d %02d:%02d:%02d",
           year, tm->tm_mon + 1, tm->tm_mday,
           tm->tm_hour, tm->tm_min, tm->tm_sec);

  // common/com/intrn_entry_cxx_prefix.def
  fprintf(_com_intrn_entry_file, COMMON_INTRN_ENTRY_HEADER, year_buf);
  fprintf(_com_intrn_entry_file,
          "\n\n/* auto-generated intrinsics for c++ %s intrinsic. %s */\n\n",
          CppIntrnPrefix().c_str(), ts_buf);

  // clang2whirl c2w_intrn_cxx_prefix.inc
  fprintf(_c2w_intrn_use_file, C2W_INTRN_HEADER, year_buf);
  fprintf(_c2w_intrn_use_file,
          "\n\n/* auto-generated intrinsics for c++ %s intrinsic. %s */\n\n",
          CppIntrnPrefix().c_str(), ts_buf);

  // be/rbc/certc/rbc_intrn_cxx_prefix.inc
  fprintf(_rbc_intrn_model_file, RBC_MODEL_CHECK_HEADER, year_buf);
  fprintf(_rbc_intrn_model_file,
          "\n\n/* auto-generated intrinsics for c++ %s intrinsic. %s */\n\n",
          CppIntrnPrefix().c_str(), ts_buf);
}

void
WhirlBuilder::FiniGenCppIntrn() {
  Is_True(GenCppIntrn(), ("-gen-cpp-intrn is off"));

  // common/com/intrn_entry_cxx_prefix.def
  fprintf(_com_intrn_entry_file,
          "/* end */\n");

  // c2w_intrn_cxx_prefix.inc
  fprintf(_c2w_intrn_use_file,
          "/* end */\n");

  // be/rbc/certc/rbc_intrn_cxx_prefix.inc
  fprintf(_rbc_intrn_model_file,
          "/* end */\n");

  fclose(_com_intrn_entry_file);
  fclose(_c2w_intrn_use_file);
  fclose(_rbc_intrn_model_file);
  if (_verbose_list_file)
    fclose(_verbose_list_file);
}

static void
GetRetValuesFromBuiltinType(const BuiltinType *type, const char *&intrn_ret) {
  switch (type->getKind()) {
  case BuiltinType::Void:
    intrn_ret = "V";
    break;

  case BuiltinType::Bool:
  case BuiltinType::Char_S:
  case BuiltinType::SChar:
    intrn_ret = "I1";
    break;

  case BuiltinType::Char_U:
  case BuiltinType::UChar:
    intrn_ret = "U1";
    break;

  case BuiltinType::Short:
    intrn_ret = "I2";
    break;

  case BuiltinType::UShort:
  case BuiltinType::Char16:
    intrn_ret = "U2";
    break;

  case BuiltinType::Int:
  case BuiltinType::WChar_S:
    intrn_ret = "I4";
    break;

  case BuiltinType::UInt:
  case BuiltinType::WChar_U:
  case BuiltinType::Char32:
    intrn_ret = "U4";
    break;

  case BuiltinType::Long:
    intrn_ret = TARGET_64BIT ? "I8" : "I4";
    break;

  case BuiltinType::ULong:
    intrn_ret = TARGET_64BIT ? "U8" :  "U4";
    break;

  case BuiltinType::LongLong:
  case BuiltinType::Int128:
    intrn_ret = "I8";
    break;

  case BuiltinType::ULongLong:
  case BuiltinType::UInt128:
    intrn_ret = "U8";
    break;

  case BuiltinType::Half: // F2
    TRACE_WARN(("F2 is not supported, promote to F4"));
  case BuiltinType::Float:
    intrn_ret = "F4";
    break;

  case BuiltinType::Double:
    intrn_ret = "F8";
    break;

  case BuiltinType::LongDouble:
  case BuiltinType::Float128:
    intrn_ret = "F10";
    break;
  case BuiltinType::NullPtr:
    intrn_ret = "PV";
    break;

  default:
    Is_True(FALSE, ("Unsupported builtin type: %d", type->getKind()));
    intrn_ret = TARGET_64BIT ? "I8" : "I4";
    break;
  }
}

static void
GetRetValuesFromType(ASTContext *ctx, QualType qty, const char *&intrn_ret) {
  const Type *type = ctx->getCanonicalType(qty).getTypePtr();
  Is_True(type != NULL, ("invalid QualType"));
  switch (type->getTypeClass()) {
  case Type::Builtin:
    GetRetValuesFromBuiltinType(cast<BuiltinType>(type), intrn_ret);
    break;

  case Type::InjectedClassName:
    GetRetValuesFromType(ctx,
                         cast<InjectedClassNameType>(type)->getInjectedSpecializationType(),
                         intrn_ret);
    break;

  case Type::LValueReference:
  case Type::Pointer:
    intrn_ret = "PV";
    break;

  case Type::Record:
    intrn_ret = "PV";
    break;

  case Type::DependentName:
  case Type::TemplateTypeParm:
  case Type::TemplateSpecialization:
    // assume intptr_t
    intrn_ret = TARGET_64BIT ? "I8" : "I4";
    break;

  default:
    type->dump();
    Is_True(FALSE, ("TODO: handle %s\n", type->getTypeClassName()));
    intrn_ret = TARGET_64BIT ? "I8" : "I4";
    break;
  }
}

void
WhirlBuilder::GenCppIntrnForClassTemplate(const clang::ClassTemplateDecl *decl) {
  Is_True(GenCppIntrn(), ("-gen-cpp-intrn is off"));
  Is_True(decl->isTemplated(), ("not a template decl"));

  if (!decl->isThisDeclarationADefinition()) {
    if (Verbose()) {
      fprintf(stderr, "INFO gen-cpp-intrn: ignore non-definition decl.\n" SBAR "\n");
      decl->dump();
    }
    return;
  }

  CXXRecordDecl *rec_decl = decl->getTemplatedDecl();
  Is_True(rec_decl != NULL, ("failed to get template decl"));

  std::string cls_name = rec_decl->getNameAsString();
  wgen_tstr cls_tstr(cls_name.c_str(), cls_name.size());
  CLASS_METHOD_MAP::const_iterator cls_iter = _cpp_class_filter_map.find(cls_tstr);
  if (cls_iter == _cpp_class_filter_map.end()) {
    if (Verbose()) {
      fprintf(stderr, "INFO gen-cpp-intrn: filter out class %s.\n" SBAR "\n",
              cls_name.c_str());
    }
    if (_verbose_list_file) {
      fprintf(_verbose_list_file, "%s.*,\n", cls_name.c_str());
    }
    return;
  }

  std::string mtd_pfx = rec_decl->getQualifiedNameAsString();
  TemplateParameterList *tmpl_parms = decl->getTemplateParameters();
  if (tmpl_parms) {
    mtd_pfx.append("<");
    BOOL need_comma = FALSE;
    for (TemplateParameterList::iterator it = tmpl_parms->begin();
         it != tmpl_parms->end(); ++it) {
      if (need_comma) {
        mtd_pfx.append(", ");
      }
      else {
        need_comma = TRUE;
      }
      if (isa<NonTypeTemplateParmDecl>(*it)) {
        NonTypeTemplateParmDecl *parm_decl = cast<NonTypeTemplateParmDecl>(*it);
        mtd_pfx.append(parm_decl->getType().getAsString());
        mtd_pfx.append(" ");
      }
      else {
        mtd_pfx.append("typename ");
      }
      mtd_pfx.append((*it)->getNameAsString());
    }
    mtd_pfx.append(">");
  }
  mtd_pfx.append("::");

  wgen_tstr all_tstr("*", 1);
  BOOL mtd_all = (cls_iter->second.find(all_tstr) != cls_iter->second.end());

  BOOL mtd_ctor = mtd_all;
  BOOL mtd_dtor = mtd_all;
  if (mtd_all == FALSE) {
    mtd_ctor = (cls_iter->second.find(cls_tstr) != cls_iter->second.end());

    std::string dtor_cstr("~");
    dtor_cstr.append(cls_name);
    wgen_tstr dtor_tstr(dtor_cstr.c_str(), dtor_cstr.size());
    mtd_dtor = (cls_iter->second.find(dtor_tstr) != cls_iter->second.end());
  }

  std::unordered_set<std::string> method_set;
  // method
  for (const Decl *decl : rec_decl->decls()) {
    const FunctionTemplateDecl *tmpl_decl = nullptr;
    if (isa<FunctionTemplateDecl>(decl)) {
      tmpl_decl = cast<FunctionTemplateDecl>(decl);
      decl = tmpl_decl->getTemplatedDecl();
    }
    if (!isa<CXXMethodDecl>(decl))
      continue;

    const CXXMethodDecl *method = cast<CXXMethodDecl>(decl);
    std::string mtd_name = method->getNameAsString();
    if (_verbose_list_file) {
      fprintf(_verbose_list_file, "%s.%s,\n",
              cls_name.c_str(), mtd_name.c_str());
    }
    int lt_pos;
    if (isa<CXXConstructorDecl>(method) &&
        (lt_pos = mtd_name.find('<')) != std::string::npos) {
      mtd_name.erase(lt_pos);
    }

    wgen_tstr mtd_tstr(mtd_name.c_str(), mtd_name.size());
    if (mtd_all == FALSE &&
        (mtd_ctor == FALSE || !isa<CXXConstructorDecl>(method)) &&
        cls_iter->second.find(mtd_tstr) == cls_iter->second.end()) {
      if (Verbose()) {
        fprintf(stderr, "INFO gen-cpp-intrn: filter out method %s::%s.\n" SBAR "\n",
                cls_name.c_str(), mtd_name.c_str());
      }
      continue;
    }

    if (mtd_dtor == TRUE && isa<CXXDestructorDecl>(method))
      mtd_dtor = FALSE;

    INT variant = isa<CXXDestructorDecl>(method) ? CXXDtorType::Dtor_Complete : 0;
    std::string name = DeclBuilder().GetMangledName(method, variant);
    if (method_set.find(name) != method_set.end())
      continue;
    method_set.insert(name);

    std::string mtd_name_pfx = mtd_pfx + mtd_name;
    GenCppIntrnForClassMethod(cls_name.c_str(), mtd_name_pfx.c_str(), name.c_str(),
                              method, tmpl_decl);
  }

  const CXXDestructorDecl *dtor;
  if (mtd_dtor == TRUE && (dtor = DeclBuilder().GetDestructor(rec_decl))) {
    std::string name = DeclBuilder().GetMangledName(dtor, CXXDtorType::Dtor_Complete);
    std::string mtd_name_pfx = mtd_pfx + dtor->getNameAsString();
    GenCppIntrnForClassMethod(cls_name.c_str(), mtd_name_pfx.c_str(), name.c_str(),
                              dtor, nullptr);
  }
}

void
WhirlBuilder::GenCppIntrnForClassMethod(const char *cls_pfx, const char *mtd_pfx, const char *mangle_name,
                                        const FunctionDecl *method, const FunctionTemplateDecl *tmpl) {
  const char *intrn_name = mangle_name;
  std::string mtd_name = method->getNameAsString();
  std::string mtd_full_name;

  // method template parameter
  TemplateParameterList *tmpl_parms;
  if (tmpl && (tmpl_parms = tmpl->getTemplateParameters())) {
    mtd_full_name.append("<");
    BOOL need_comma = FALSE;
    for (TemplateParameterList::iterator it = tmpl_parms->begin();
         it != tmpl_parms->end(); ++it) {
      if (need_comma) {
        mtd_full_name.append(", ");
      }
      else {
        need_comma = TRUE;
      }
      if (isa<NonTypeTemplateParmDecl>(*it)) {
        NonTypeTemplateParmDecl *parm_decl = cast<NonTypeTemplateParmDecl>(*it);
        mtd_full_name.append(parm_decl->getType().getAsString());
        mtd_full_name.append(" ");
      }
      else {
        mtd_full_name.append("typename ");
      }
      mtd_full_name.append((*it)->getNameAsString());
    }
    mtd_full_name.append(">");
  }

  // method parameter
  mtd_full_name.append("(");
  BOOL need_comma = FALSE;
  for (const ParmVarDecl *parm_decl : method->parameters()) {
    if (need_comma) {
      mtd_full_name.append(", ");
    }
    else {
      need_comma = TRUE;
    }
    mtd_full_name.append(parm_decl->getType().getAsString());
    mtd_full_name.append(" ");
    mtd_full_name.append(parm_decl->getNameAsString());
  }
  mtd_full_name.append(")");

  if (isa<CXXConstructorDecl>(method) ||
      isa<CXXDestructorDecl>(method)) {
    int lt_pos = mtd_name.find('<');
    if (lt_pos != std::string::npos) {
      mtd_name.erase(lt_pos);
    }
  }
  std::string mtd_macro_name = mtd_name;
  if (isa<CXXDestructorDecl>(method)) {
    int bnot_pos = mtd_macro_name.find('~');
    if (bnot_pos != std::string::npos) {
      mtd_macro_name.replace(bnot_pos, 1, "dtor_");
    }
  }
  else if (method->isOverloadedOperator()) {
    OverloadedOperatorKind opr = method->getOverloadedOperator();
    mtd_macro_name.erase(8);
    mtd_macro_name.append(GetOperatorName(opr));
  }

  const char *intrn_ret = NULL;
  QualType qty = method->getReturnType();
  GetRetValuesFromType(Context(), qty, intrn_ret);

  // common/com/intrn_entry_cxx_prefix.def
  const char *cls_sep = *cls_pfx ? "::" : "";
  fprintf(_com_intrn_entry_file, COMMON_INTRN_ENTRY_ITEM,
          intrn_name, cls_pfx, cls_sep, mtd_name.c_str(),
          intrn_name, intrn_name,
          intrn_ret,
          mangle_name,
          cls_pfx, cls_sep, mtd_name.c_str(),
          mtd_pfx, mtd_full_name.c_str());

  // c2w_intrn_cxx_prefix.inc
  fprintf(_c2w_intrn_use_file, C2W_INTRN_ITEM,
          mtd_pfx, mtd_full_name.c_str(),
          mangle_name, intrn_name);

  // be/rbc/certc/rbc_intrn_cxx_prefix.inc
  const char *macro_sep = *cls_pfx ? "_" : "";
  fprintf(_rbc_intrn_model_file, RBC_MODEL_CHECK_ITEM,
          mtd_pfx, mtd_full_name.c_str(),
          mangle_name, mangle_name, mangle_name);
}

void
WhirlBuilder::GenCppIntrnForFunctionTemplate(const clang::FunctionTemplateDecl *decl) {
  Is_True(GenCppIntrn(), ("-gen-cpp-intrn is off"));
  Is_True(decl->isTemplated(), ("not a template decl"));

  if (!decl->isThisDeclarationADefinition()) {
    if (Verbose()) {
      fprintf(stderr, "INFO gen-cpp-intrn: ignore non-definition decl.\n" SBAR "\n");
      decl->dump();
    }
    return;
  }

  FunctionDecl *func_decl = decl->getTemplatedDecl();
  Is_True(func_decl != NULL, ("failed to get template decl"));

  std::string func_name = func_decl->getNameAsString();
  wgen_tstr func_tstr(func_name.c_str(), func_name.size());
  METHOD_SET::const_iterator func_iter = _cpp_func_filter_set.find(func_tstr);
  if (func_iter == _cpp_func_filter_set.end()) {
    if (Verbose()) {
      fprintf(stderr, "INFO gen-cpp-intrn: filter out function %s.\n" SBAR "\n",
              func_name.c_str());
    }
    if (_verbose_list_file) {
      fprintf(_verbose_list_file, "%s,\n", func_name.c_str());
    }
    return;
  }

  std::string intrn_name = DeclBuilder().GetMangledName(func_decl, 0);
  std::string func_full_name = func_decl->getQualifiedNameAsString();
  GenCppIntrnForClassMethod("", func_full_name.c_str(), intrn_name.c_str(), func_decl, decl);
}

void
WhirlBuilder::GenCppIntrnForDecl(const Decl *decl) {
  Is_True(GenCppIntrn(), ("-gen-cpp-intrn is off"));
  Is_True(decl->isTemplated(), ("not a template decl"));

  switch (decl->getKind()) {
  case Decl::ClassTemplate:
    GenCppIntrnForClassTemplate(cast<ClassTemplateDecl>(decl));
    break;
  case Decl::FunctionTemplate:
    GenCppIntrnForFunctionTemplate(cast<FunctionTemplateDecl>(decl));
    break;
  default:
    break;
  }
}

} // namespace wgen


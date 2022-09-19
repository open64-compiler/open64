/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/// Copyright [year] <Copyright Owner>
#ifndef JS2MPL_INCLUDE_JS2MPL_H_
#define JS2MPL_INCLUDE_JS2MPL_H_
#include <string.h>
using namespace std;
namespace maple {
class MIRModule;
struct JSMIRContext {
  bool isplugin_;
  const string &wrapper_name_;
  bool with_main_;  // whether generate main()
  bool simp_call_;  // whether use simple call if possible

  bool jsop_only_;  // dump jsop only

  JSMIRContext(bool isplugin, const string &name, bool with_main, bool jsop_only, bool simp_call)
    : isplugin_(isplugin), wrapper_name_(name), with_main_(with_main), jsop_only_(jsop_only), simp_call_(simp_call) {}
};
bool js2mpldriver(const char *, const char *, MIRModule *, JSMIRContext &);
}  // namespace maple

#endif  // JS2MPL_INCLUDE_JS2MPL_H_

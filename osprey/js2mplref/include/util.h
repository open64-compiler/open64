/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/// Copyright [year] <Copyright Owner>
#ifndef JS2MPL_INCLUDE_UTIL_H_
#define JS2MPL_INCLUDE_UTIL_H_

#include "js/src/jsatom.h"

extern int js2mplDebug;
extern int js2mplDebugIndent;

#define DEBUG js2mplDebug
#define DEADBEEF 0xdeadbeef

#define PLUGINPREFIX "plugin_"

#define THIS_POSITION_IN_ARGS 0
#define ENV_POSITION_IN_ARGS 1
#define FORMAL_POSITION_IN_ARGS 2
//#define LENGTH_POSITION_IN_ARGS 3

namespace maple {

extern void PrintIndentation(int indent);

class Util {
 public:
  static const char *getOpcodeName[228];
  static void AdjIndent(int n);
  static void SetIndent(int n);
  static char *GetString(JSAtom *atom, MemPool *mp, JSContext *ctx);
  static char *GetSequentialName0(const char *prefix, uint32_t num, MemPool *mp);
  static char *GetSequentialName(const char *prefix, uint32_t &num, MemPool *mp);
  static char *GetNameWithPrefix(const char *orig_name, const char *prefix, MemPool *mp);
  static char *GetNameWithSuffix(const char *orig_name, const char *suffix, MemPool *mp);
  static char *GetFuncName(const char *parent, const char *name, uint32_t num, MemPool *mp);
};
}  // namespace maple

#include "macros.h"

#endif  // JS2MPL_INCLUDE_UTIL_H_

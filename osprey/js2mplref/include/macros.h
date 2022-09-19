/// Copyright [year] <Copyright Owner>
#ifndef JS2MPL_INCLUDE_MACROS_H_
#define JS2MPL_INCLUDE_MACROS_H_

#include <iostream>

#define _LOCATION __func__ << "() at " << __FILE__ << ":" << __LINE__

// print empty line
#define DEBUGPRINT0                  \
  do {                               \
    if (js2mplDebug > 0) {           \
      std::cout << " " << std::endl; \
    }                                \
  } while (0);

// print string
#define DEBUGPRINTIND(n)   \
  do {                     \
    if (js2mplDebug > 0) { \
      PrintIndentation(n); \
    }                      \
  } while (0);

// print str
#define DEBUGPRINT_S_LEVEL(str, level)      \
  do {                                      \
    if (js2mplDebug >= level) {             \
      PrintIndentation(js2mplDebugIndent);  \
      std::cout << " " << str << std::endl; \
    }                                       \
  } while (0);
#define DEBUGPRINTs(var) DEBUGPRINT_S_LEVEL(var, 1)
#define DEBUGPRINTs2(var) DEBUGPRINT_S_LEVEL(var, 2)
#define DEBUGPRINTs3(var) DEBUGPRINT_S_LEVEL(var, 3)
#define DEBUGPRINTs4(var) DEBUGPRINT_S_LEVEL(var, 4)
#define DEBUGPRINTs5(var) DEBUGPRINT_S_LEVEL(var, 5)
#define DEBUGPRINTs6(var) DEBUGPRINT_S_LEVEL(var, 6)

#define DEBUGPRINTfunc(name)                  \
  do {                                        \
    int ind = js2mplDebugIndent;              \
    Util::SetIndent(1);                       \
    if (js2mplDebug > 0) {                    \
      PrintIndentation(js2mplDebugIndent);    \
      std::cout << name << " {" << std::endl; \
    }                                         \
    Util::SetIndent(ind);                     \
  } while (0);

#define DEBUGPRINTfuncEnd(name)            \
  do {                                     \
    int ind = js2mplDebugIndent;           \
    Util::SetIndent(1);                    \
    if (js2mplDebug > 0) {                 \
      PrintIndentation(js2mplDebugIndent); \
      std::cout << "}\n" << std::endl;     \
    }                                      \
    Util::SetIndent(ind);                  \
  } while (0);

#define DEBUGPRINTnode(node)                            \
  do {                                                  \
    if (js2mplDebug > 1) {                              \
      PrintIndentation(js2mplDebugIndent);              \
      std::cout << "  >> node: ";                       \
      static_cast<StmtNode *>(node)->Dump(mirModule, 0); \
      DEBUGPRINT0;                                      \
    }                                                   \
  } while (0);

// print var = val
#define DEBUGPRINT_V_LEVEL(var, level)                                    \
  do {                                                                    \
    if (js2mplDebug >= level) {                                           \
      PrintIndentation(js2mplDebugIndent);                                \
      std::cout << _LOCATION << " " << #var << " = " << var << std::endl; \
    }                                                                     \
  } while (0);
#define DEBUGPRINT(var) DEBUGPRINT_V_LEVEL(var, 1)
#define DEBUGPRINT2(var) DEBUGPRINT_V_LEVEL(var, 2)
#define DEBUGPRINT3(var) DEBUGPRINT_V_LEVEL(var, 3)
#define DEBUGPRINT4(var) DEBUGPRINT_V_LEVEL(var, 4)
#define DEBUGPRINT5(var) DEBUGPRINT_V_LEVEL(var, 5)
#define DEBUGPRINT6(var) DEBUGPRINT_V_LEVEL(var, 6)

// print var = val
#define DEBUGPRINT_V_LEVEL_PURE(var, level)           \
  do {                                                \
    if (js2mplDebug >= level) {                       \
      PrintIndentation(js2mplDebugIndent);            \
      std::cout << #var << " = " << var << std::endl; \
    }                                                 \
  } while (0);
#define DEBUGPRINTpure(var) DEBUGPRINT_V_LEVEL_PURE(var, 1)
#define DEBUGPRINT2pure(var) DEBUGPRINT_V_LEVEL_PURE(var, 2)
#define DEBUGPRINT3pure(var) DEBUGPRINT_V_LEVEL_PURE(var, 3)
#define DEBUGPRINT4pure(var) DEBUGPRINT_V_LEVEL_PURE(var, 4)
#define DEBUGPRINT5pure(var) DEBUGPRINT_V_LEVEL_PURE(var, 5)
#define DEBUGPRINT6pure(var) DEBUGPRINT_V_LEVEL_PURE(var, 6)

// print val0 val1
#define DEBUGPRINT_NN_LEVEL(var0, var1, level) \
  do {                                         \
    if (js2mplDebug >= level) {                \
      PrintIndentation(js2mplDebugIndent);     \
      std::cout << var0 << " " << var1;        \
    }                                          \
  } while (0);
#define DEBUGPRINTnn(var0, var1) DEBUGPRINT_NN_LEVEL(var0, var1, 1)
#define DEBUGPRINTnn2(var0, var1) DEBUGPRINT_NN_LEVEL(var0, var1, 2)
#define DEBUGPRINTnn3(var0, var1) DEBUGPRINT_NN_LEVEL(var0, var1, 3)

// print var0 = val0, var1 = val1
#define DEBUGPRINT_VV_LEVEL(var0, var1, level)                                                                \
  do {                                                                                                        \
    if (js2mplDebug >= level) {                                                                               \
      PrintIndentation(js2mplDebugIndent);                                                                    \
      std::cout << _LOCATION << " " << #var0 << " = " << var0 << ", " << #var1 << " = " << var1 << std::endl; \
    }                                                                                                         \
  } while (0);
#define DEBUGPRINTvv(var0, var1) DEBUGPRINT_VV_LEVEL(var0, var1, 1)
#define DEBUGPRINTvv2(var0, var1) DEBUGPRINT_VV_LEVEL(var0, var1, 2)
#define DEBUGPRINTvv3(var0, var1) DEBUGPRINT_VV_LEVEL(var0, var1, 3)

// print val0, var = val
#define DEBUGPRINT_SV_LEVEL(val0, var, level)                                             \
  do {                                                                                    \
    if (js2mplDebug >= level) {                                                           \
      PrintIndentation(js2mplDebugIndent);                                                \
      std::cout << _LOCATION << " " << val0 << ", " << #var << " = " << var << std::endl; \
    }                                                                                     \
  } while (0);
#define DEBUGPRINTsv(var0, var) DEBUGPRINT_SV_LEVEL(var0, var, 1)
#define DEBUGPRINTsv2(var0, var) DEBUGPRINT_SV_LEVEL(var0, var, 2)
#define DEBUGPRINTsv3(var0, var) DEBUGPRINT_SV_LEVEL(var0, var, 3)

#endif  // JS2MPL_INCLUDE_MACROS_H_

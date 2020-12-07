/*
  Copyright (C) 2019-2020 Xcalibyte Limited, Inc.  All Rights Reserved.

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

#ifndef CLANG2WHIRL_TRACER_H
#define CLANG2WHIRL_TRACER_H

#include "c2w_utils.h"
#include "c2w_file.h"
#include <stdio.h>
#include <stdarg.h>
#include <vector>
#include <stack>

namespace wgen {

class TraceManager : private NonCopyable {
private:
  TraceManager() {}
  
  ~TraceManager() {}

private:
  static TraceManager *Instance() {
    static TraceManager mgr;
    return &mgr;
  }

private:
  struct FuncFrame {
    const char *_fname;
    const char *_file;
    int _line;
    
    FuncFrame(const char *fname, const char *file, int line)
      : _fname(fname), _file(file), _line(line) {
    }
  };
  
  typedef std::vector<FuncFrame> FuncFrameStack;
  FuncFrameStack _framestack;

public:
  static void Enter(const char *fname, const char *file, int line) {
    FuncFrameStack &stack = Instance()->_framestack;
    stack.push_back(FuncFrame(fname, file, line));
  }
  
  static void Exit() {
    FuncFrameStack &stack = Instance()->_framestack;
    raw_assert(stack.size() > 0);
    stack.pop_back();
  }
  
  static void BackTrace(int indent, FILE *fp) {
    FuncFrameStack &stack = Instance()->_framestack;
    for (FuncFrameStack::iterator it = stack.begin();
         it != stack.end();
         ++it) {
      fprintf(fp, "%*s%s at %s:%d\n",
              indent, " ", it->_fname, it->_file, it->_line);
      indent++;
    }
  }
  
  static void FuncTrace(int indent, FILE *fp) {
    FuncFrameStack &stack = Instance()->_framestack;
    raw_assert(stack.size() > 0);
    if (stack.size()) {
      const FuncFrame &frame = stack.back();
      fprintf(fp, "%*s%s at %s:%d\n",
              indent + (int) stack.size(), " ", frame._fname, frame._file, frame._line);
    }
  }
}; // TraceManager


class FuncTracer {
public:
  FuncTracer(const char *fname, const char *file, int line) {
    TraceManager::Enter(fname, file, line);
    TraceManager::FuncTrace(0, FileManager::TraceFile(TK_FUNC));
  }
  
  ~FuncTracer() {
    TraceManager::FuncTrace(0, FileManager::TraceFile(TK_FUNC));
    TraceManager::Exit();
  }
}; // FuncTracer


template<enum TraceKind _kind>
class Tracer {
  const char *_fname;
  const char *_file;
  int _line;
public:
  Tracer(const char *fname, const char *file, int line)
    : _fname(fname), _file(file), _line(line) {
  }
  
  ~Tracer() {
  }
  
  void operator()(const char *fmt, ...) {
    FILE *fp = FileManager::TraceFile(_kind);
    switch (_kind) {
      case TK_TODO:
        fprintf(fp, "TODO: ");
        break;
      case TK_WARN:
        fprintf(fp, "WARN: ");
        break;
      case TK_CALL:
        fprintf(fp, "CALL: ");
        break;
      case TK_ASSERT:
        fprintf(fp, "ASSERT: ");
        break;
      default:
        fprintf(fp, "TRC: ");
        break;
    }
    va_list arg;
    va_start(arg, fmt);
    vprintf(fmt, arg);
    va_end(arg);
    printf(" in %s at %s:%d\n", _fname, _file, _line);
  }
}; // template<enum TraceKind> Tracer

class TodoTracer : public Tracer<TK_TODO> {
};

class WarnTracer : public Tracer<TK_WARN> {
};

class DbgAssert : public Tracer<TK_ASSERT> {
};

class RelAssert : public Tracer<TK_ASSERT> {
};

} // namespace wgen

//#define TRACE_FUNC()      wgen::FuncTracer __internal_ftrc_obj__(__FUNCTION__, __FILE__, __LINE__)
#define TRACE_FUNC()
#define TRACE_TODO(msg)   wgen::Tracer<wgen::TK_TODO>(__FUNCTION__, __FILE__, __LINE__)msg
#define TRACE_WARN(msg)   wgen::Tracer<wgen::TK_WARN>(__FUNCTION__, __FILE__, __LINE__)msg
#define TRACE_DBG_ASSERT(msg) wgen::Tracer<wgen::TK_ASSERT>(__FUNCTION__, __FILE__, __LINE__)msg
#define TRACE_REL_ASSERT(msg) wgen::Tracer<wgen::TK_ASSERT>(__FUNCTION__, __FILE__, __LINE__)msg
#define TRACE_CALL(call)  (wgen::Tracer<wgen::TK_CALL>(__FUNCTION__, __FILE__, __LINE__)("%s", #call), call)
#define TRACE_UNSUPPORTED(id)   wgen::Tracer<wgen::TK_TODO>(__FUNCTION__, __FILE__, __LINE__)("Unsupported: " #id);

#define DBG_ASSERT(cond, msg) \
    if (!(cond)) TRACE_DBG_ASSERT(msg)

#define REL_ASSERT(cond, msg) \
    if (!(cond)) TRACE_REL_ASSERT(msg)

#endif /* CLANG2WHIRL_TRACER_H */

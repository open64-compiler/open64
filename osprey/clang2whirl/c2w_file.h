/*
  Copyright (C) 2019-2020 XC5 Limited, Inc.  All Rights Reserved.

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

#ifndef CLANG2WHIRL_FILE_H
#define CLANG2WHIRL_FILE_H

#include "c2w_utils.h"
#include "c2w_enum.h"
#include <stdio.h>
#include <vector>

namespace wgen {

class FileManager : private NonCopyable {
private:
  FileManager() {}
  
  ~FileManager() {}

private:
  static FileManager *Instance() {
    static FileManager mgr;
    return &mgr;
  }

private:
  struct FileInfo {
    const char *_name;
    FILE *_file;
    short _kind;
    short _status;
    
    FileInfo(const char *name, FILE *fp, FileKind kind, FileStatus status)
      : _name(name), _file(fp), _kind(kind), _status(status) {
    }
  };
  
  typedef std::vector<FileInfo> FileInfoArray;
  FileInfoArray _filearray;

public:
  static void RegisterFile(const char *name, FILE *fp, FileKind kind, FileStatus status) {
    FileInfoArray &array = Instance()->_filearray;
    array.push_back(FileInfo(name, fp, kind, FS_ACTIVE));
  }
  
  static FILE *Open(FileKind kind, const char *name, const char *mode) {
    FILE *fp = fopen(name, mode);
    if (fp == NULL)
      return NULL;
    RegisterFile(name, fp, kind, FS_ACTIVE);
  }
  
  static FILE *DefaultTraceFile() {
    return stdout;
  }
  
  static FILE *TraceFile(TraceKind kind) {
    return stdout;
  }
  
  static FILE *DefaultDumpFile() {
    return stdout;
  }
  
  static FILE *DumpFile(DumpKind kind) {
    return DefaultDumpFile();
  }
};

} // namespace wgen


#endif /* CLANG2WHIRL_FILE_H */

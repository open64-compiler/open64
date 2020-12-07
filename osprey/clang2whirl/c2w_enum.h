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

#ifndef CLANG2WHIRL_ENUM_H
#define CLANG2WHIRL_ENUM_H

namespace wgen {

enum TraceKind {
  TK_FUNC,     // trace the function entry & exit
  TK_TODO,     // trace all TODO items
  TK_WARN,     // trace all warning items
  TK_CALL,     // trace function calls
  TK_ASSERT,   // trace debug assertion
  TK_IR,       // trace ir
  TK_SYM       // trace symbol
};

enum DumpKind {
  DK_BINARY,   // dump to binary form
  DK_TEXT,     // dump to text form
  DK_VCG       // dump to VCG
};

enum FileKind {
  FK_INPUT,    // the file is for input
  FK_OUTPUT,   // the file is for output
  FK_TRACE,    // the file is for trace
  FK_DUMP      // the file is for dump
};

enum FileStatus {
  FS_ACTIVE,   // the file is active (current open)
  FS_INACTIVE  // the file is inactive (closed)
};

enum INPUT_LANG {
  IL_C,        // source is C
  IL_CPP       // source is C++ (requires C++ABI)
};

} // namespace wgen

#endif /* CLANG2WHIRL_ENUM_H */


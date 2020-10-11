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

#ifndef CLANG2WHIRL_CXXABI_H
#define CLANG2WHIRL_CXXABI_H

#include "clangdecl.h"
#include "open64decl.h"
#include "c2w_tracer.h"
#include "clang/Basic/ABI.h"

namespace wgen {

class WhirlStrtabBuilder;

class WhirlBuilder;

enum class StructorType {
  Complete, // constructor or destructor
  Base,     // constructor or destructor
  Deleting  // destructor only
};

inline clang::CXXCtorType toCXXCtorType(StructorType T) {
  switch (T) {
    case StructorType::Complete:
      return clang::Ctor_Complete;
    case StructorType::Base:
      return clang::Ctor_Base;
    case StructorType::Deleting:
      REL_ASSERT(false, ("cannot have a deleting ctor"));
  }
  REL_ASSERT(false, ("not a StructorType"));
}

inline StructorType getFromCtorType(clang::CXXCtorType T) {
  switch (T) {
    case clang::Ctor_Complete:
      return StructorType::Complete;
    case clang::Ctor_Base:
      return StructorType::Base;
    case clang::Ctor_Comdat:
      REL_ASSERT(false, ("not expecting a COMDAT"));
    case clang::Ctor_CopyingClosure:
    case clang::Ctor_DefaultClosure:
      REL_ASSERT(false, ("not expecting a closure"));
  }
  REL_ASSERT(false, ("not a CXXCtorType"));
}

inline clang::CXXDtorType toCXXDtorType(StructorType T) {
  switch (T) {
    case StructorType::Complete:
      return clang::Dtor_Complete;
    case StructorType::Base:
      return clang::Dtor_Base;
    case StructorType::Deleting:
      return clang::Dtor_Deleting;
  }
  REL_ASSERT(false, ("not a StructorType"));
}

inline StructorType getFromDtorType(clang::CXXDtorType T) {
  switch (T) {
    case clang::Dtor_Deleting:
      return StructorType::Deleting;
    case clang::Dtor_Complete:
      return StructorType::Complete;
    case clang::Dtor_Base:
      return StructorType::Base;
    case clang::Dtor_Comdat:
      REL_ASSERT(false, ("not expecting a COMDAT"));
  }
  REL_ASSERT(false, ("not a CXXDtorType"));
}

class CXXABI {
public:
  static STR_IDX MangledName(llvm::StringRef N);
};

}

#endif /* CLANG2WHIRL_CXXABI_H */


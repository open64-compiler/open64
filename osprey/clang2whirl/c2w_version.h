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

#ifndef CLANG2WHIRL_VERSION_H
#define CLANG2WHIRL_VERSION_H

// #include "build_version.h"

#include "clang/Basic/Version.inc"

#define TO_STRING(x) #x
#ifndef CLANG_VERSION_STRING
#define CLANG_VERSION_STRING TO_STRING(CLANG_VERSION)
#endif

#define BUILD_DATE "02Apr2019 22:36:21"
#define OPEN64_VERSION "Open64 1.0"
#define SCM_VERSION "clangfe-1980eaec77ad6fde0bd21adbe43f3fbb0733a11f"
#define COMPILER_FULL_VERSION OPEN64_VERSION " " BUILD_DATE " " SCM_VERSION " ( Clang " CLANG_VERSION_STRING " )"

#endif


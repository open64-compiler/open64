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

#ifndef CLANG2WHIRL_FUNC_H
#define CLANG2WHIRL_FUNC_H

// forward clang declarations
#include "clangdecl.h"

// forward open64 declarations
#include "open64decl.h"
#include "mempool.h"
#include "c2w_builder.h"
#include <set>

namespace wgen {

class R;
class WhirlBuilder;

class WhirlFuncBuilder {
private:
  WhirlBuilder *_builder;
  MEM_POOL _local_pool;

public:
  WhirlFuncBuilder(WhirlBuilder *builder);
  
  ~WhirlFuncBuilder();

private:
  PU_Info *CreatePUInfo(ST_IDX st_idx);
  
  void CreateLocalSymtab(ST_IDX st_idx);
  
  void DestroyLocalSymtab();

  void EmitVTableFieldInitialization(WN *blk, const clang::CXXRecordDecl *record_decl);

  WN  *EmitDestructorCall(const clang::CXXDestructorDecl *decl,
                          clang::CXXDtorType dtor, UINT32 field_id);

  SRCPOS  SetSrcPos(clang::SourceLocation sl) { return _builder->SetSrcPos(sl); }
  SRCPOS  GetSrcPos(void)                     { return _builder->GetSrcPos(); }

  WN  *EmitAdjustThunk(WN *wn, clang::ThunkInfo thunk_info, BOOL adjust_this);

  void EmitVariablyModifiedType(clang::QualType type);

public:
  PU_Info *ConvertFunction(clang::GlobalDecl gd, ST_IDX st_idx);

  PU_Info *EmitCXXGlobalVarDeclInitialization(ST_IDX st_idx);

  PU_Info *EmitCXXGlobalInitialization(ST_IDX st_idx, int priority_size);

  PU_Info *EmitDtorPtr(const clang::VarDecl *decl, ST_IDX &st_idx);

  PU_Info *EmitThunkFunction(const clang::ThunkInfo &ti, ST_IDX st_idx);
};

} // namespace wgen

#endif /* CLANG2WHIRL_DECL_H */

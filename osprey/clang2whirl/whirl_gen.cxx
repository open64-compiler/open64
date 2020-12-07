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

#include "clang/CodeGen/CodeGenAction.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/CodeGen/BackendUtil.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "c2w_consumer.h"
#include "c2w_enum.h"
#include "c2w_tracer.h"
#include <stdio.h>

// dummy implementation for Rewrite
#include "clang/Rewrite/Frontend/FrontendActions.h"

namespace clang {

CodeGenAction::CodeGenAction(unsigned _Act, llvm::LLVMContext *_VMContext)
  : Act(_Act),
    VMContext(_VMContext ? _VMContext : new llvm::LLVMContext),
    OwnsVMContext(!_VMContext) {}

CodeGenAction::~CodeGenAction() {
  TheModule.reset();
  if (OwnsVMContext)
    delete VMContext;
}

bool CodeGenAction::hasIRSupport() const {
  TRACE_FUNC();
  return true;
}

void
CodeGenAction::EndSourceFileAction() {
  TRACE_FUNC();
}

std::unique_ptr<llvm::Module>
CodeGenAction::takeModule() {
  TRACE_FUNC();
  return std::unique_ptr<llvm::Module>(nullptr);
}

llvm::LLVMContext *
CodeGenAction::takeLLVMContext() {
  TRACE_FUNC();
  OwnsVMContext = false;
  return VMContext;
}

std::unique_ptr<ASTConsumer>
CodeGenAction::CreateASTConsumer(CompilerInstance &CI,
                                 StringRef InFile) {
  TRACE_FUNC();
  std::string OutFile = CI.getFrontendOpts().OutputFile;
  if (OutFile.empty())
    OutFile = InFile.substr(0, InFile.rfind('.')).str() + ".B";
  
  ASTConsumer *consumer = new wgen::WhirlGenConsumer(CI.getDiagnostics(),
                                                     CI.getCodeGenOpts(),
                                                     CI.getTargetOpts(),
                                                     CI.getLangOpts(),
                                                     CI.getFrontendOpts().ShowTimers,
                                                     InFile.str(),
                                                     OutFile,
                                                     nullptr, /*LinkModule,*/
                                                     VMContext);
  
  return std::unique_ptr<ASTConsumer>(consumer);
}

void CodeGenAction::ExecuteAction() {
  TRACE_FUNC();
  this->ASTFrontendAction::ExecuteAction();
}

EmitAssemblyAction::EmitAssemblyAction(llvm::LLVMContext *_VMContext)
  : CodeGenAction(Backend_EmitAssembly, _VMContext) {}

void EmitAssemblyAction::anchor() {}

EmitBCAction::EmitBCAction(llvm::LLVMContext *_VMContext)
  : CodeGenAction(Backend_EmitBC, _VMContext) {}

void EmitBCAction::anchor() {}

EmitLLVMAction::EmitLLVMAction(llvm::LLVMContext *_VMContext)
  : CodeGenAction(Backend_EmitLL, _VMContext) {}

void EmitLLVMAction::anchor() {}

EmitLLVMOnlyAction::EmitLLVMOnlyAction(llvm::LLVMContext *_VMContext)
  : CodeGenAction(Backend_EmitNothing, _VMContext) {}

void EmitLLVMOnlyAction::anchor() {}

EmitCodeGenOnlyAction::EmitCodeGenOnlyAction(llvm::LLVMContext *_VMContext)
  : CodeGenAction(Backend_EmitMCNull, _VMContext) {}

void EmitCodeGenOnlyAction::anchor() {}

EmitObjAction::EmitObjAction(llvm::LLVMContext *_VMContext)
  : CodeGenAction(Backend_EmitObj, _VMContext) {}

void EmitObjAction::anchor() {}

#if 0
// dummy implementation for Rewrite
bool FixItRecompile::BeginInvocation(CompilerInstance &CI) { return false; }

bool RewriteIncludesAction::BeginSourceFileAction(CompilerInstance &CI) { return false; }
void RewriteIncludesAction::ExecuteAction() { }

void RewriteMacrosAction::ExecuteAction() { }

void RewriteTestAction::ExecuteAction() { }
#endif
}


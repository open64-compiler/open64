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

#ifndef CLANG2WHIRL_CONSUMER_H
#define CLANG2WHIRL_CONSUMER_H

// clang
#include "clanginc.h"
// llvm
#include "llvm/Support/Timer.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
// WhirlBuilder
#include "c2w_builder.h"
// std
#include <string>
#include <memory>

namespace wgen {

using namespace clang;
using namespace llvm;

class WhirlGenConsumer : public ASTConsumer {
  const CodeGenOptions &CodeGenOpts;
  const clang::TargetOptions &TargetOpts;
  const LangOptions &LangOpts;
  Timer IRGenTimer;
  const std::string InFile;
  const std::string OutFile;
  std::unique_ptr<llvm::Module> TheModule, LinkModule;
  
  //
  WhirlBuilder Builder;
  ASTConsumer *Dumper;

public:
  WhirlGenConsumer(DiagnosticsEngine &_Diags,
                   const CodeGenOptions &compopts,
                   const clang::TargetOptions &targetopts,
                   const LangOptions &langopts,
                   bool TimePasses,
                   const std::string &infile,
                   const std::string &outfile,
                   llvm::Module *LinkModule,
                   LLVMContext *C);
  
  ~WhirlGenConsumer();
  
  llvm::Module *takeModule() { return TheModule.release(); }
  
  llvm::Module *takeLinkModule() { return LinkModule.release(); }
  
  void Initialize(ASTContext &Context);
  
  bool HandleTopLevelDecl(DeclGroupRef D);
  
  void HandleInlineMethodDefinition(CXXMethodDecl *D);
  
  void HandleInterestingDecl(DeclGroupRef D);
  
  void HandleTranslationUnit(ASTContext &Ctx);
  
  void HandleTagDeclDefinition(TagDecl *D);
  
  void HandleTagDeclRequiredDefinition(const TagDecl *D);
  
  void HandleCXXImplicitFunctionInstantiation(FunctionDecl *D);
  
  void HandleTopLevelDeclInObjCContainer(DeclGroupRef D);
  
  void HandleImplicitImportDecl(ImportDecl *D);
  
  void HandleLinkerOptionPragma(llvm::StringRef Opts);
  
  void HandleDetectMismatch(llvm::StringRef Name, llvm::StringRef Value);
  
  void HandleDependentLibrary(llvm::StringRef Lib);
  
  void CompleteTentativeDefinition(VarDecl *D);
  
  void AssignInheritanceModel(CXXRecordDecl *RD);
  
  void HandleCXXStaticMemberVarInstantiation(VarDecl *D);
  
  void HandleVTable(CXXRecordDecl *RD, bool DefinitionRequired);
  
  ASTMutationListener *GetASTMutationListener();
  
  ASTDeserializationListener *GetASTDeserializationListener();
  
  void PrintStats();
  
  bool shouldSkipFunctionBody(clang::Decl *D);
};

} // namespace wgen

#endif /* CLANG2WHIRL_CONSUMER_H */

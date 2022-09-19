/*
  Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

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

#include "c2w_consumer.h"
#include "c2w_tracer.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Frontend/ASTConsumers.h"

namespace wgen {

WhirlGenConsumer::WhirlGenConsumer(DiagnosticsEngine &_Diags,
                                   const CodeGenOptions &compopts,
                                   const clang::TargetOptions &targetopts,
                                   const LangOptions &langopts,
                                   bool TimePasses,
                                   const std::string &infile,
                                   const std::string &outfile,
                                   llvm::Module *LinkModule,
                                   LLVMContext *C)
  : CodeGenOpts(compopts), TargetOpts(targetopts),
    LangOpts(langopts),
    IRGenTimer("IRGT", "IR Generator Time"),
    InFile(infile), OutFile(outfile), LinkModule(LinkModule),
    Builder(langopts.CPlusPlus ? IL_CPP : IL_C, _Diags) {
  TRACE_FUNC();
  Dumper = createASTDumper();
}

WhirlGenConsumer::~WhirlGenConsumer() {
  TRACE_FUNC();
  delete Dumper;
}

void
WhirlGenConsumer::Initialize(ASTContext &Context) {
  TRACE_FUNC();
  Builder.SetContext(&Context);
  Builder.Initialize(InFile.c_str(), OutFile.c_str());
}

bool
WhirlGenConsumer::HandleTopLevelDecl(DeclGroupRef D) {
  TRACE_FUNC();
  //Dumper->HandleTopLevelDecl(D);
  //D->print(llvm::outs())
  return Builder.HandleTopLevelDecl(D);
}

void
WhirlGenConsumer::HandleInlineMethodDefinition(CXXMethodDecl *D) {
  TRACE_FUNC();
}

void
WhirlGenConsumer::HandleInterestingDecl(DeclGroupRef D) {
  TRACE_FUNC();
}

void
WhirlGenConsumer::HandleTranslationUnit(ASTContext &Ctx) {
  TRACE_FUNC();
  //Dumper->HandleTranslationUnit(Ctx);
  Builder.HandleTranslationUnit(Ctx);
  Builder.Finalize();
}

void
WhirlGenConsumer::HandleTagDeclDefinition(TagDecl *D) {
  TRACE_FUNC();
}

void
WhirlGenConsumer::HandleTagDeclRequiredDefinition(const TagDecl *D) {
  TRACE_FUNC();
}

void
WhirlGenConsumer::HandleCXXImplicitFunctionInstantiation(FunctionDecl *D) {
  TRACE_FUNC();
}

void
WhirlGenConsumer::HandleTopLevelDeclInObjCContainer(DeclGroupRef D) {
  TRACE_FUNC();
}

void
WhirlGenConsumer::HandleImplicitImportDecl(ImportDecl *D) {
  TRACE_FUNC();
}

void
WhirlGenConsumer::HandleLinkerOptionPragma(llvm::StringRef Opts) {
  TRACE_FUNC();
}

void
WhirlGenConsumer::HandleDetectMismatch(llvm::StringRef Name, llvm::StringRef Value) {
  TRACE_FUNC();
}

void
WhirlGenConsumer::HandleDependentLibrary(llvm::StringRef Lib) {
  TRACE_FUNC();
}

void
WhirlGenConsumer::CompleteTentativeDefinition(VarDecl *D) {
  TRACE_FUNC();
}

void
WhirlGenConsumer::AssignInheritanceModel(CXXRecordDecl *RD) {
  TRACE_FUNC();
}

void
WhirlGenConsumer::HandleCXXStaticMemberVarInstantiation(VarDecl *D) {
  TRACE_FUNC();
  auto DK = D->isThisDeclarationADefinition();
  if (DK == VarDecl::Definition && D->hasAttr<DLLImportAttr>())
    return;

  TemplateSpecializationKind TSK = D->getTemplateSpecializationKind();
  // If we have a definition, this might be a deferred decl. If the
  // instantiation is explicit, make sure we emit it at the end.
  if (D->getDefinition() && TSK == TSK_ExplicitInstantiationDefinition)
    //TODO: should return constant here;
    return;

  Builder.EmitTopLevelDecl(D);
}

void
WhirlGenConsumer::HandleVTable(CXXRecordDecl *RD, bool DefinitionRequired) {
  TRACE_FUNC();
}

ASTMutationListener *
WhirlGenConsumer::GetASTMutationListener() {
  TRACE_FUNC();
  return nullptr;
}

ASTDeserializationListener *
WhirlGenConsumer::GetASTDeserializationListener() {
  TRACE_FUNC();
  return nullptr;
}

void
WhirlGenConsumer::PrintStats() {
  TRACE_FUNC();
}

bool
WhirlGenConsumer::shouldSkipFunctionBody(Decl *D) {
  TRACE_FUNC();
  return true;
}

} // namespace wgen


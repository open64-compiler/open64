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

#ifndef CLANGDECL_H
#define CLANGDECL_H

// forward declaration for all clang classes
namespace clang {
class ASTContext;

class DiagnosticsEngine;

class DeclGroupRef;

class FileID;

class FileEntry;

class SourceLocation;
} // namespace clang

namespace clang {
class AccessSpecDecl;

class BlockDecl;

class CapturedDecl;

class ClassScopeFunctionSpecializationDecl;

class ClassTemplateDecl;

class ClassTemplatePartialSpecializationDecl;

class ClassTemplateSpecializationDecl;

class CXXBaseSpecifier;

class CXXConstructorDecl;

class CXXConversionDecl;

class CXXDestructorDecl;

class CXXMethodDecl;

class CXXRecordDecl;

class Decl;

class NamedDecl;

class DeclaratorDecl;

class EmptyDecl;

class EnumDecl;

class EnumConstantDecl;

class FieldDecl;

class FileScopeAsmDecl;

class FriendDecl;

class FriendTemplateDecl;

class FunctionDecl;

class FunctionTemplateDecl;

class GlobalDecl;

class ImplicitParamDecl;

class ImportDecl;

class IndirectFieldDecl;

class LabelDecl;

class LinkageSpecDecl;

class MSPropertyDecl;

class NamespaceDecl;

class NamespaceAliasDecl;

class NonTypeTemplateParmDecl;

class ParmVarDecl;

class RecordDecl;

class StaticAssertDecl;

class TemplateTemplateParmDecl;

class TemplateTypeParmDecl;

class TranslationUnitDecl;

class TypeAliasDecl;

class TypeAliasTemplateDecl;

class TypeDecl;

class TypedefDecl;

class UnresolvedUsingTypenameDecl;

class UnresolvedUsingValueDecl;

class UsingDecl;

class UsingDirectiveDecl;

class UsingShadowDecl;

class ValueDecl;

class VarDecl;

class VarTemplateDecl;

class VarTemplatePartialSpecializationDecl;

class VarTemplateSpecializationDecl;

class MangleContext;
} // namespace clang

namespace clang {
class AtomicType;

class BlockPointerType;

class BuiltinType;

class ComplexType;

class ConstantArrayType;

class EnumType;

class FunctionType;

class IncompleteArrayType;

class MemberPointerType;

class PointerType;

class QualType;

class ReferenceType;

class Type;

class VariableArrayType;

class VectorType;

class RecordType;
} // namespace clang

namespace clang {
class ArraySubscriptExpr;

class BinaryConditionalOperator;

class BinaryOperator;

class CallExpr;

class ChooseExpr;

class CompoundAssignOperator;

class CompoundLiteralExpr;

class ConditionalOperator;

class CStyleCastExpr;

class CXXBindTemporaryExpr;

class CXXNewExpr;

class CXXConstCastExpr;

class CXXConstructExpr;

class CXXDefaultArgExpr;

class CXXDefaultInitExpr;

class CXXDynamicCastExpr;

class CXXFunctionalCastExpr;

class CXXMemberCallExpr;

class CXXOperatorCallExpr;

class CXXReinterpretCastExpr;

class CXXStaticCastExpr;

class CXXTemporaryObjectExpr;

class CXXTypeidExpr;

class CXXUuidofExpr;

class DeclRefExpr;

class Expr;

class ExprWithCleanups;

class ExtVectorElementExpr;

class GenericSelectionExpr;

class ImplicitCastExpr;

class InitListExpr;

class IntegerLiteral;

class FloatingLiteral;

class CharacterLiteral;

class LambdaExpr;

class MaterializeTemporaryExpr;

class MemberExpr;

class OpaqueValueExpr;

class ParenExpr;

class PredefinedExpr;

class PseudoObjectExpr;

class StmtExpr;

class StringLiteral;

class SubstNonTypeTemplateParmExpr;

class UnaryOperator;

class UserDefinedLiteral;

class VAArgExpr;

class ImplicitValueInitExpr;

class CXXThisExpr;
} // namespace clang

namespace clang {
class GCCAsmStmt;

class AttributedStmt;

class BreakStmt;

class CapturedStmt;

class CompoundStmt;

class ContinueStmt;

class CXXCatchStmt;

class CXXForRangeStmt;

class CXXTryStmt;

class DeclStmt;

class DoStmt;

class ForStmt;

class GotoStmt;

class IfStmt;

class IndirectGotoStmt;

class LabelStmt;

class NullStmt;

class ReturnStmt;

class Stmt;

class StmtExpr;

class SwitchStmt;

class WhileStmt;
} // namespace clang

namespace clang {
class APValue;

class VTableLayout;

struct ThunkInfo;
}

namespace llvm {
class StringRef;

class raw_svector_ostream;
} // namespace llvm

#endif /* CLANGDECL_H */

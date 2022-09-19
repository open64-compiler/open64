//
// Created by xc5 on 29/7/2018.
//

#ifndef OSPREY_JGEN_BASE_DECL_H
#define OSPREY_JGEN_BASE_DECL_H

#include "jgen_include.h"
#include <string>
#include <vector>

using std::string;
using std::vector;

namespace JGEN {

// U64U = unsigned long long
// U32U = unsigned int

class JGEN_Typetree_Base {

 public:

  // read Kind from Node
  virtual U64U getKind(U32U jIndex) = 0;

  // read Name from Node
  virtual std::string getKindName(U32U jIndex) = 0;

  // read Kind from Node
  virtual U64U getFlag(U32U jIndex) = 0;

  // get DefId form Node
  virtual int getJsonRefId(U32U jIndex) = 0;

  // get (Symbol) Name from Node
  virtual std::string getNameString(U32U jIndex) = 0;

  // retrieve the previously bound Idx
  virtual int getIdx(U32U jIndex) = 0;

  // bind Idx to the tree node
  virtual void setTypeIdx(U32U jIndex, int idx) = 0;

  // get bound Idx to the tree node
  virtual void getTypeIdx(U32U jIndex)  = 0;

  virtual bool isZeroMaxValue (U32U jIndex) = 0;

  virtual bool isSizeMaxValueConstant (U32U jIndex)  = 0;

  virtual bool isVariableSize (U32U jIndex)  = 0;

  virtual int get_type_size (U32U jIndex) = 0;

  virtual int get_args_count(U32U jIndex) = 0;

  virtual bool isUnsigned (U32U jIndex)  = 0;

  virtual U32U get_element_type (U32U jIndex) = 0;

  virtual bool isConst (U32U jIndex) = 0;

  virtual int get_element_size_unit (U32U jIndex) = 0;

  virtual long get_max_value (U32U jIndex) = 0;
  virtual bool isReadonly (U32U jIndex)  = 0;
  virtual bool isVolatile (U32U jIndex)  = 0;
  virtual bool isRestrict (U32U jIndex)  = 0;

  virtual bool isAnonymous (U32U jIndex)  = 0;
  virtual bool isAggregateValue (U32U jIndex)  = 0;
  virtual int getAlignWidth (U32U jIndex) = 0;

  virtual U32U getRetVal(U32U jIndex) = 0;
  virtual vector<U32U> getArgs(U32U jIndex) = 0;
  virtual bool isLangSpecific(U32U jIndex) = 0;
  virtual vector<U32U> getFields(U32U jIndex) = 0;

 private :
  // goto some type with an typetree internal Id
  virtual int gotoId(U32U jIndex) = 0;

};



class JGEN_SymbolTree_Base {

 public:

  virtual void init(void * tree)  = 0;

  // read Kind from Node
  virtual U64U getKind(U32U jIndex) = 0;

  // read Name from Node
  virtual std::string getKindName(U32U jIndex) = 0;

  // read Kind from Node
  virtual unsigned long long getFlag(U32U jIndex) = 0;

  // get DefId form Node
  virtual int getJsonRefId(U32U jIndex) = 0;

  // get (Symbol) Name from Node
  virtual std::string getNameString (U32U jIndex) = 0;

  // retrieve the previously bound Idx
  virtual int getIdx(U32U jIndex) = 0;

  // bind Idx to the tree node
  virtual void setTypeIdx(U32U jIndex, TY_IDX ty_idx) = 0;

  // bind Idx to the tree node
  virtual vector<U32U> getMemberFields(U32U jIndex) = 0;

  virtual U32U getParent(U32U jIndex) = 0;

  virtual int gotoStId(U32U jIndex) = 0;


  virtual bool isPublic (U32U jIndex)
  {
    return (getFlag(jIndex) & JGEN_ST_FLAG_MASK_PUBLIC) > 0;
  }

  virtual bool isWeak (U32U jIndex)
  {
    return (getFlag(jIndex) & JGEN_ST_FLAG_MASK_WEAK) > 0;
  }

  virtual bool isConstructor (U32U jIndex) = 0;

  virtual bool isPureVFunc (U32U jIndex) = 0;

  virtual bool isMethodOfClass (U32U jIndex) = 0;

  virtual U32U get_method_base_type (U32U jIndex) = 0;

  virtual bool isContextNamespace (U32U jIndex) = 0;

  virtual bool isContextRecord (U32U jIndex) = 0;

  virtual bool isLangSpecific (U32U jIndex) = 0;

  virtual bool isReallyExtern (U32U jIndex) = 0;

  virtual bool isNoThrow (U32U jIndex) = 0;

  virtual bool isInitial (U32U jIndex) = 0;

  virtual bool isCommon (U32U jIndex) = 0;

  virtual bool isExternal (U32U jIndex) = 0;

  virtual bool isStatic (U32U jIndex) = 0;

  virtual bool hasName (U32U jIndex) = 0;

  virtual U32U getLineNum(U32U jIndex) = 0;

  virtual bool is_guard_var (U32U jIndex) = 0;

  virtual U32U get_sym_type (U32U jIndex) = 0;
};


class JGEN_IR_Decl{
 public:

  /**
   *   check if have child (childnum)
   *   @return remainning child nums;
   */
  virtual int hasChild() = 0;

  /**
   *  Returns a child
   */
  virtual JGEN_IR_Decl * getChildAtPosition (U32U pos) = 0;


  /***
   *  Get the Kind of the Decl
   *  @return longlong JGEN_DECL_CLASS
   */
  virtual U64U getKind() = 0;
  virtual int getChild_count() = 0;

  virtual JGEN_SymbolTree_Base * get_symbol_tree() = 0;
  virtual JGEN_Typetree_Base * get_type_tree() = 0;

  virtual int getSymbolId() = 0;
  virtual int getTypeId() = 0;

};

}

#endif //OSPREY_JGEN_BASE_DECL_H

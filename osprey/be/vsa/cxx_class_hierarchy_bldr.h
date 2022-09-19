/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

#ifndef CXX_CLASS_HIERARCHY_BULIDER_INCLUDE
#define CXX_CLASS_HIERARCHY_BULIDER_INCLUDE
#include "class_hierarchy.h"

// class type RTTI INITO layout
// abi::__class_type_info
//   { TYPE_INFO_VTABLE_SYM, TYPE_INFO_NAME_SYM }
// abi::__si_class_type_info
//   { TYPE_INFO_VTABLE_SYM, TYPE_INFO_NAME_SYM, BASE_TYPE_INFO_SYM }
// abi::__vmi_class_type_info
//   { TYPE_INFO_VTABLE_SYM, TYPE_INFO_NAME_SYM,
//     VMI_FLAG, VMI_BASE_CNT,
//     { VMI_BASE_TYPE_SYM, VMI_BASE_TYPE_OFF } * VMI_BASE_CNT }
enum CLASS_TYPE_INFO_ENTRY {
  TYPE_INFO_VTABLE_SYM    = 0,
  TYPE_INFO_NAME_SYM      = 1,
  BASE_TYPE_INFO_SYM      = 2,
  VMI_FLAG                = 2,
  VMI_BASE_CNT            = 3,
  VMI_BASE_TYPE_SYM_BEGIN = 4,
  VMI_BASE_TYPE_OFF_BEGIN = 5
};
#define CLASS_TI_PREFIX "__class_type_info"
#define SI_TI_PREFIX    "__si_class_type_info"
#define VMI_TI_PREFIX   "__vmi_class_type_info"

//==============================================================================
//
// CXX_CLASS_HIERARCHY_BUILDER class  build class hierarchy for C++
//
//==============================================================================
class CXX_CLASS_HIERARCHY_BUILDER : public CLASS_HIERARCHY 
{
  private:
    CXX_CLASS_HIERARCHY_BUILDER();                                                // REQUIRED UNDEFINED UNWANTED methods
    CXX_CLASS_HIERARCHY_BUILDER(const CXX_CLASS_HIERARCHY_BUILDER&);              // REQUIRED UNDEFINED UNWANTED methods
    CXX_CLASS_HIERARCHY_BUILDER& operator= (const CXX_CLASS_HIERARCHY_BUILDER&);  // REQUIRED UNDEFINED UNWANTED methods

  public:
    CXX_CLASS_HIERARCHY_BUILDER(MEM_POOL *pool, MEM_POOL *lpool) : CLASS_HIERARCHY(pool, lpool)
    {
       Build_class_info();
    }
  
  private:
    void Build_class_info();
    void Add_vtable_method(INITO *entry, TY_IDX idx);
};
#endif

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

#ifndef J_CLASS_HIREACHY_BLDR_H
#define J_CLASS_HIREACHY_BLDR_H

#include "class_hierarchy.h"
#include "symtab_defs.h"
#include <string.h>
#include "irbdata.h"
#include "java_defs.h"
using namespace std;

/*
  We define annodataion data structure with referece to jvm spec and gcj source code.
  The data structure are compatible with gcj and support new annotation feature defined in jvm8.
  There is no document for gcj, need to clone the source code and extract info from source code.
  clone gcc source code repo, then exec: git checkout gcc-6_4_0-release
  please read the source code located at gcc source tree: libjava/java/lang/natClass.cc
  this file shows how to parse annotations
*/

typedef enum {
  JV_CLASS_ATTR,
  JV_METHOD_ATTR,
  JV_FIELD_ATTR,
  JV_DONE_ATTR,
  JV_CODE_ATTR,
} JV_ATTR_TYPE;

typedef enum {
  JV_INNER_CLASSES_KIND,
  JV_ENCLOSING_METHOD_KIND,
  JV_SIGNATURE_KIND,
  JV_ANNOTATIONS_KIND,
  JV_PARAMETER_ANNOTATIONS_KIND,
  JV_ANNOTATION_DEFAULT_KIND,
  JV_TYPE_ANNOTATIONS_KIND,
} JV_ATTR_KIND;

typedef enum {
  JV_CONSTANT_Undefined            = 0,
  JV_CONSTANT_Utf8                 = 1,
  JV_CONSTANT_Unicode              = 2,
  JV_CONSTANT_Integer              = 3,
  JV_CONSTANT_Float                = 4,
  JV_CONSTANT_Long                 = 5,
  JV_CONSTANT_Double               = 6,
  JV_CONSTANT_Class                = 7,
  JV_CONSTANT_String               = 8,
  JV_CONSTANT_Fieldref             = 9,
  JV_CONSTANT_Methodref            = 10,
  JV_CONSTANT_InterfaceMethodref   = 11,
  JV_CONSTANT_NameAndType          = 12,
  JV_CONSTANT_ResolvedFlag         = 16,
  JV_CONSTANT_LazyFlag             = 32,
  JV_CONSTANT_ResolvedString       = 16 | 8,
  JV_CONSTANT_ResolvedClass        = 16 | 7,
} JV_CONSTANT_TAG;

extern const char *JV_ATTR_TYPE_NAME[];
extern const char *JV_ATTR_KIND_NAME[];
extern const char *JV_CONSTANT_TAG_NAME[];

static inline const char *
Jv_attr_type_name(JV_ATTR_TYPE type)
{
  return JV_ATTR_TYPE_NAME[type];
}

static inline const char *
Jv_attr_kind_name(JV_ATTR_KIND kind)
{
  return JV_ATTR_KIND_NAME[kind];
}

static inline const char *
Jv_constant_tag_name(JV_CONSTANT_TAG tag)
{
  return JV_CONSTANT_TAG_NAME[tag];
}

static inline UINT8
Read_UINT8(char *&buf)
{
  return *buf++;
}

static inline UINT16
Read_UINT16(char *&buf)
{
  UINT16 value = *((UINT16 *) buf);
  buf += 2;
#if HOST_IS_LITTLE_ENDIAN
  UINT16 u1 = (value << 8) & 0xff00;
  UINT16 u2 = (value >> 8) & 0x00ff;
  return u1 | u2;
#else
  return value;
#endif
}

static inline UINT32
Read_UINT32(char *&buf)
{
  UINT32 value = *((UINT32 *) buf);
  buf += 4;
#if HOST_IS_LITTLE_ENDIAN
  UINT32 u1 = (value << 24) & 0xff000000;
  UINT32 u2 = (value << 16) & 0x00ff0000;
  UINT32 u3 = (value >> 8) & 0x0000ff00;
  UINT32 u4 = (value >> 24) & 0x000000ff;
  return u1 | u2 | u3 | u4;
#else
  return value;
#endif
}

class ANNOTATION;
class ELEMENT_VALUE;

struct ENUM_CONST_VALUE {
  UINT16                 type_name_index;        // the constant table index of type name
  UINT16                 const_name_index;       // the constant table index of const name
};

struct ARRAY_VALUE {
  UINT16                 num_elements;           // the number of elements array
  ELEMENT_VALUE        **elements;               // an array of annotation element
};

struct CONST_VALUE_DESC {
  char                   tag;
  TYPE_ID                mtype;
  JV_CONSTANT_TAG        constant_tag;
};

//=============================================================================
//  tag Item Type value Item Constant Type
//  B byte const_value_index CONSTANT_Integer
//  C char const_value_index CONSTANT_Integer
//  D double const_value_index CONSTANT_Double
//  F float const_value_index CONSTANT_Float
//  I int const_value_index CONSTANT_Integer
//  J long const_value_index CONSTANT_Long
//  S short const_value_index CONSTANT_Integer
//  Z boolean const_value_index CONSTANT_Integer
//  s String const_value_index CONSTANT_Utf8
//  e Enum type enum_const_value Not applicable
//  c Class class_info_index Not applicable
//  @ Annotation type annotation_value Not applicable
//  [ Array type array_value Not applicable
//=============================================================================
class ELEMENT_VALUE {
private:
  ELEMENT_VALUE(const ELEMENT_VALUE&);
  ELEMENT_VALUE& operator = (const ELEMENT_VALUE&);

public:
  UINT8                  tag;                    // tag for the this element value
  union {
    UINT16               const_value_index;      // tag: B->Byte, C->Character, D->Double, F->Float
                                                 //      I->Integer, J->Long, S->Short, Z->Boolean, s->String
                                                 // the index of constant table, get the string, parse as value
    ENUM_CONST_VALUE     enum_const_value;       // tag: e
    UINT16               class_info_index;       // tag: c, the constant pool inddex of class name, get the string, parse as class name
    ANNOTATION          *annotation_value;       // tag: @, a pointer of annotation
    ARRAY_VALUE          array_value;            // tag: [
  } value;

  // the following members were added by us
  union {
    union {
      INT8               byte_value;
      UINT16             character_value;
      double             double_value;
      float              float_value;
      INT32              integer_value;
      INT64              long_value;
      INT16              short_value;
      UINT8              boolean_value;
      char              *string_value;
    } const_value;
    struct {
      char              *type_name;
      char              *const_name;
    } enum_const_value;
    char *class_info_value;
    ANNOTATION *annotation_value;
    struct {
      UINT16             num_elements;
      ELEMENT_VALUE    **elements;
    } array_value;
  } real_value;

public:
  ELEMENT_VALUE(char *&buf, INITV_ENTRIES &constant_data,
                INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool);

  void                   Write_to_real_value(TYPE_ID mtype, INT64 value, UINT32_MAP *st_inito_cache, MEM_POOL *pool);
  void                   Print(FILE *fp);

private:
  static CONST_VALUE_DESC const_value_desc[9];
};

class ELEMENT_VALUE_PAIR {
private:
  ELEMENT_VALUE_PAIR(const ELEMENT_VALUE_PAIR&);              // REQUIRED UNDEFINED UNWANTED METHs
  ELEMENT_VALUE_PAIR& operator = (const ELEMENT_VALUE_PAIR&); // REQUIRED UNDEFINED UNWANTED METHs

public:
  UINT16                 element_name_index;     // the constant table index of name
  ELEMENT_VALUE         *element_value;          // an annotation element

  // the following members were added by us
  char                  *element_name;

public:
  ELEMENT_VALUE_PAIR(char *&buf, INITV_ENTRIES &constant_data,
                     INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool);

  void                   Print(FILE *fp);
};

class ANNOTATION {
private:
  ANNOTATION(const ANNOTATION&);                 // REQUIRED UNDEFINED UNWANTED METHs
  ANNOTATION& operator = (const ANNOTATION&);    // REQUIRED UNDEFINED UNWANTED METHs

public:
  UINT16                 type_index;             // the constant table index of type name
  UINT16                 num_element_value_pairs;// number of pairs in this annotation
  ELEMENT_VALUE_PAIR   **element_value_pairs;    // an array of annotation element pair, of length num_element_value_pairs

  // the following members were added by us
  char                  *type_name;

public:
  ANNOTATION(char *&buf, INITV_ENTRIES &constant_data,
             INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool);

  void                   Print(FILE *fp);
};

class TYPE_PATH_INFO {
private:
  TYPE_PATH_INFO(const TYPE_PATH_INFO&);              // REQUIRED UNDEFINED UNWANTED METHs
  TYPE_PATH_INFO& operator = (const TYPE_PATH_INFO&); // REQUIRED UNDEFINED UNWANTED METHs

public:
  UINT8                  type_path_kind;
  UINT8                  type_argument_index;

public:
  TYPE_PATH_INFO(char *&buf, INITV_ENTRIES &constant_data,
                 INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool);
};

//=============================================================================
//  • If a nested type T1.T2 is used in a declaration or expression, then an annotation
//  may appear on the name of the top level type or any member type.
//  • If a parameterized type T<A> or T<? extends A> or T<? super A> is used in a
//  declaration or expression, then an annotation may appear on any type argument
//  or on the bound of any wildcard type argument.
//  For example, consider the different parts of String[][] that are annotated in:
//  @Foo String[][] // Annotates the class type String
//  String @Foo [][] // Annotates the array type String[][]
//  String[] @Foo [] // Annotates the array type String[]
//  or the different parts of the nested type Outer.Middle.Inner that are annotated in:
//  @Foo Outer.Middle.Inner
//  Outer.@Foo Middle.Inner
//  Outer.Middle.@Foo Inner
//  or the different parts of the parameterized types Map<String,Object> and List<...>
//  that are annotated in:
//  @Foo Map<String,Object>
//  Map<@Foo String,Object>
//  Map<String,@Foo Object>
//  List<@Foo ? extends String>
//  List<? extends @Foo String>
//=============================================================================
class TYPE_PATH {
private:
  TYPE_PATH(const TYPE_PATH&);                   // REQUIRED UNDEFINED UNWANTED METHs
  TYPE_PATH& operator = (const TYPE_PATH&);      // REQUIRED UNDEFINED UNWANTED METHs

public:
  UINT8                  path_length;
  TYPE_PATH_INFO       **path;

public:
  TYPE_PATH(): path_length(0), path(NULL)        {}
};

class ANNO_LOCAL_TYPE {
private:
  ANNO_LOCAL_TYPE(const ANNO_LOCAL_TYPE&);
  ANNO_LOCAL_TYPE& operator = (const ANNO_LOCAL_TYPE&);

public:
  UINT16                 start_pc;
  UINT16                 length;
  UINT16                 index;

public:
  ANNO_LOCAL_TYPE(char *&buf, INITV_ENTRIES &constant_data,
                  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool);
};

//=============================================================================
//  The first three items - target_type, target_info, and target_path -
//  specify the precise location of the annotated type. The last three items
//  - type_index, num_element_value_pairs, and element_value_pairs[] -
//  specify the annotation's own type and element-value pairs.
//
//  The value of the target_type item determines
//  whether the type_annotation structure appears in a
//  RuntimeVisibleTypeAnnotations attribute in a ClassFile structure,
//  a field_info structure, a method_info structure, or a
//  Code attribute. Table 4.7.20-C gives the location of the
//  RuntimeVisibleTypeAnnotations attribute for a type_annotation
//  structure with each legal target_type value.
//
//  Value Kind of target target_info item
//  0x00 type parameter declaration of generic class or interface type_parameter_target
//  0x01 type parameter declaration of generic method or constructor type_parameter_target
//  0x10 type in extends or implements clause  of class declaration
//       (including the direct superclass or direct superinterface of an anonymous class
//        declaration), or in extends clause of interface declaration supertype_target
//  0x11 type in bound of type parameter declaration of generic class or interface type_parameter_bound_target
//  0x12 type in bound of type parameter declaration of generic method or constructor type_parameter_bound_target
//  0x13 type in field declaration empty_target
//  0x14 return type of method, or type of newly constructed object empty_target
//  0x15 receiver type of method or constructor empty_target
//  0x16 type in formal parameter declaration of method, constructor, or lambda expression formal_parameter_target
//  0x17 type in throws clause of method or constructor throws_target
//
//  Value | Kind-of-target | Location
//  0x00 type parameter declaration of generic class or interface ClassFile
//  0x01 type parameter declaration of generic method or constructor method_info
//  0x10 type in extends clause of class or interface declaration, or in implements clause of interface declaration ClassFile
//  0x11 type in bound of type parameter declaration of generic class or interface ClassFile
//  0x12 type in bound of type parameter declaration of generic method or constructor method_info
//  0x13 type in field declaration field_info
//  0x14 return type of method or constructor method_info
//  0x15 receiver type of method or constructor method_info
//  0x16 type in formal parameter declaration of method, constructor, or lambda expression method_info
//  0x17 type in throws clause of method or constructor method_info
//  0x40-0x4B types in local variable declarations, resource variable declarations, exception parameter declarations, expressions Code
//=============================================================================
class TYPE_ANNOTATION {
private:
  TYPE_ANNOTATION(const TYPE_ANNOTATION&);               // REQUIRED UNDEFINED UNWANTED METHs
  TYPE_ANNOTATION& operator = (const TYPE_ANNOTATION&);  // REQUIRED UNDEFINED UNWANTED METHs

public:
  UINT8                 target_type;
  union {
    struct {
      UINT8             type_parameter_index;
    } type_parameter_target;
    struct {
      UINT16            supertype_index;
    } supertype_target;
    struct {
      UINT8             type_parameter_index;
      UINT8             bound_index;
    } type_parameter_bound_target;
    struct {
    } empty_target;
    struct {
      UINT8              formal_parameter_index;
    } formal_parameter_target;
    struct {
      UINT16             throws_type_index;
    } throws_target;
    struct {
      UINT16             table_length;
      ANNO_LOCAL_TYPE  **table;
    } localvar_target;
    struct {
      UINT16             exception_table_index;
    } catch_target;
    struct {
      UINT16             offset;
    } offset_target;
    struct {
      UINT16             offset;
      UINT8              type_argument_index;
    } type_argument_target;
  } target_info;
  TYPE_PATH              target_path;
  UINT16                 type_index;
  UINT16                 num_element_value_pairs;
  ELEMENT_VALUE_PAIR   **element_value_pairs;

public:
  TYPE_ANNOTATION(char *&buf, INITV_ENTRIES &constant_data,
                  INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool);
};

class RUNTIME_VISIABLE_ANNOTATIONS;
typedef RUNTIME_VISIABLE_ANNOTATIONS R_VIS_ANNOS;
class PARAMETER_ANNOTATION;
typedef PARAMETER_ANNOTATION PARAM_ANNO;
class RUNTIME_VISIABLE_PARAMETER_ANNOTATIONS;
typedef RUNTIME_VISIABLE_PARAMETER_ANNOTATIONS R_VIS_PARAM_ANNOS;
class RUNTIME_VISIABLE_TYPE_ANNOTATIONS;
typedef RUNTIME_VISIABLE_TYPE_ANNOTATIONS R_VIS_TYPE_ANNOS;

class RUNTIME_VISIABLE_ANNOTATIONS {
private:
  RUNTIME_VISIABLE_ANNOTATIONS(const RUNTIME_VISIABLE_ANNOTATIONS&);              // REQUIRED UNDEFINED UNWANTED METHs
  RUNTIME_VISIABLE_ANNOTATIONS& operator = (const RUNTIME_VISIABLE_ANNOTATIONS&); // REQUIRED UNDEFINED UNWANTED METHs

public:
  UINT16                 member_index;           // ATTENTION: this does not exist if JV_ATTR_TYPE == JV_CLASS_ATTR
  UINT16                 num_annotations;
  ANNOTATION           **annotations;            // array

public:
  RUNTIME_VISIABLE_ANNOTATIONS(char *&buf, UINT8 type, INITV_ENTRIES &constant_data,
              INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool);

  void                   Print(FILE *fp);
};

class PARAMETER_ANNOTATION {
private:
  PARAMETER_ANNOTATION(const PARAMETER_ANNOTATION&);              // REQUIRED UNDEFINED UNWANTED METHs
  PARAMETER_ANNOTATION& operator = (const PARAMETER_ANNOTATION&); // REQUIRED UNDEFINED UNWANTED METHs

public:
  UINT16                 num_annotations;        // the number of params
  ANNOTATION           **annotations;            // an array, annotation[num_annotations]

public:
  PARAMETER_ANNOTATION(char *&buf, INITV_ENTRIES &constant_data,
                       INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool);

  void                               Print(FILE *fp);
};

class RUNTIME_VISIABLE_PARAMETER_ANNOTATIONS {
private:
  RUNTIME_VISIABLE_PARAMETER_ANNOTATIONS(const R_VIS_PARAM_ANNOS&);              // REQUIRED UNDEFINED UNWANTED METHs
  RUNTIME_VISIABLE_PARAMETER_ANNOTATIONS& operator = (const R_VIS_PARAM_ANNOS&); // REQUIRED UNDEFINED UNWANTED METHs

public:
  UINT16                 member_index;           // method index or field index;
  UINT8                  num_parameters;
  PARAMETER_ANNOTATION **parameter_annotations;  // an array

public:
  RUNTIME_VISIABLE_PARAMETER_ANNOTATIONS(char *&buf, UINT8 type, INITV_ENTRIES &constant_data,
                    INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool);

  void                   Print(FILE *fp);
};

class RUNTIME_VISIABLE_TYPE_ANNOTATIONS {
private:
  RUNTIME_VISIABLE_TYPE_ANNOTATIONS(const RUNTIME_VISIABLE_TYPE_ANNOTATIONS&);              // REQUIRED UNDEFINED UNWANTED METHs
  RUNTIME_VISIABLE_TYPE_ANNOTATIONS& operator = (const RUNTIME_VISIABLE_TYPE_ANNOTATIONS&); // REQUIRED UNDEFINED UNWANTED METHs

public:
  UINT16                 member_index;           // ATTENTION: this does not exist if JV_ATTR_TYPE == JV_CLASS_ATTR
  UINT16                 num_annotations;        // the number of params
  TYPE_ANNOTATION      **type_annotations;       // an array, annotation[num_annotations]

public:
   RUNTIME_VISIABLE_TYPE_ANNOTATIONS(char *&buf, UINT8 type, INITV_ENTRIES &constant_data,
                     INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool);
};

class ANNOTATIONS_ATTR {
private:
  ANNOTATIONS_ATTR(const ANNOTATIONS_ATTR&);                    // REQUIRED UNDEFINED UNWANTED METHs
  ANNOTATIONS_ATTR& operator = (const ANNOTATIONS_ATTR&);       // REQUIRED UNDEFINED UNWANTED METHs

public:
  UINT8                  type;                   // type: JV_ATTR_TYPE
  UINT32                 len;                    // the offset to the next annotation, base is the start address of next field
  UINT8                  kind;                   // type: JV_ATTR_KIND
  union {
    R_VIS_ANNOS         *r_vis_annos;
    R_VIS_PARAM_ANNOS   *r_vis_param_annos;
    R_VIS_TYPE_ANNOS    *r_vis_type_annos;
  } annos_data;

public:
  ANNOTATIONS_ATTR(char *&buf, INITV_ENTRIES &constant_data,
                   INITV_ENTRIES &constant_tags, UINT32_MAP *st_inito_cache, MEM_POOL *pool);

  void                  Print(FILE *fp);
};

class IPSA;
class TYPE_NODE;

typedef mempool_allocator<TYPE_NODE * > TN_ALLOCATOR;
typedef vector<TYPE_NODE *, TN_ALLOCATOR> TN_VEC;
typedef TN_VEC::iterator TN_VEC_ITER;

typedef mempool_allocator< pair<ST_IDX, TYPE_NODE *> > ST_TN_ALLOCATOR;
typedef hash_map<ST_IDX, TYPE_NODE *, __gnu_cxx::hash<ST_IDX>, __gnu_cxx::equal_to<ST_IDX>, ST_TN_ALLOCATOR > ST_TN_MAP;
typedef ST_TN_MAP::iterator ST_TN_MAP_ITER;

class TYPE_NODE {
public:
  MEM_POOL*             _pool;

private:
  TYPE_NODE();
  TYPE_NODE(const TYPE_NODE&);
  TYPE_NODE& operator = (const TYPE_NODE&);
  ST_IDX              _st_idx;
  TYPE_NODE*          _parent;
  TN_VEC*             _children;
  TN_VEC*             _interfaces;

public:
  TYPE_NODE(ST_IDX st_idx, MEM_POOL *pool) {_st_idx = st_idx; _pool = pool; _parent = NULL; _children = NULL; _interfaces = NULL; }
  void                Set_parent(TYPE_NODE *parent);
  TYPE_NODE*          Get_parent() { return _parent; }
  void                Set_children(TN_VEC *children) { _children = children; }
  TN_VEC*             Get_children() { return _children; }
  ST_IDX              Get_st_idx() { return _st_idx; }
  void                Add_interface(TYPE_NODE *interface);
  TN_VEC*             Get_interfaces() { return _interfaces; }
};

class JAVA_CLASS_HIERARCHY_BUILDER : public CLASS_HIERARCHY {
private:
  JAVA_CLASS_HIERARCHY_BUILDER();                                                 // REQUIRED UNDEFINED UNWANTED METHs
  JAVA_CLASS_HIERARCHY_BUILDER(const JAVA_CLASS_HIERARCHY_BUILDER&);              // REQUIRED UNDEFINED UNWANTED METHs
  JAVA_CLASS_HIERARCHY_BUILDER& operator = (const JAVA_CLASS_HIERARCHY_BUILDER&); // REQUIRED UNDEFINED UNWANTED METHs

private:
  UINT32_MAP      _st_inito_cache;

public:
  JAVA_CLASS_HIERARCHY_BUILDER(MEM_POOL *pool, MEM_POOL *lpool) : CLASS_HIERARCHY(pool, lpool),
      _st_inito_cache(DEFAULT_HASH_TABLE_SIZE, __gnu_cxx::hash<mUINT32>(),
          __gnu_cxx::equal_to<mUINT32>(), UINT32_ALLOCATOR(lpool))
  {
    Build_tmp_st_inito_cache();
    Build_class_info();
  }
private:
  void            Build_tmp_st_inito_cache();
  INITV_IDX       Get_initv_idx_by_st_idx(ST_IDX st_idx);
  void            Build_class_info();
  void            Find_related_class(TYPE_NODE *node, hash_set<ST_IDX> &related_class_sym);
  void            Build_class_from_vtable_file();
  void            Add_interface(C_STR ty, C_STR interf);
  void            Add_meth_sig(C_STR ty, C_STR sig, CALL_OFF off);
  C_STR           Create_meth_sig(STR_IDX name_idx, STR_IDX sig_idx);
  CLASS_INFO*     New_class_info(C_STR name);
  JAVA_METH_INFO* New_meth_table();

  inline void     Read_class_symbol(INITO *entry);
  inline void     Read_vtable(INITO *entry, BOOL filt = FALSE);
  void            Read_interface_table(C_STR class_ty, ST_IDX intf_st);
  void            Read_meth_table(C_STR class_ty, ST_IDX meth_st_idx);
  void            Read_annotations_attr(C_STR class_ty, ST_IDX reflection_st,
                                        INITV_ENTRIES &constant_data,
                                        INITV_ENTRIES &constant_tags);
  void            Set_class_is_interface(C_STR);
  void            Set_class_has_interface(C_STR);
  void            Set_class_acc_flag(CLASS_INFO *info, UINT16 flags);
  UINT32_MAP     *Get_st_inito_cache()                               { return &_st_inito_cache; }
};

class JAVA_CLASS_HIERARCHY_HELPER {
private:
  static NAME_SET*      rbc_class_sym_set;
  static NAME_SET*      rbc_vtable_sym_set;

private:
  JAVA_CLASS_HIERARCHY_HELPER();

public:
  static void           Init(MEM_POOL *pool);
  static NAME_SET*      Get_class_sym_set() { return rbc_class_sym_set; }
  static NAME_SET*      Get_vtable_sym_set() { return rbc_vtable_sym_set; }
};

#endif

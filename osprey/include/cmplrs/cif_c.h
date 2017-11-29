/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#ifndef __CIF_C_H
#define __CIF_C_H
#ifdef __cplusplus
extern "C" {
#endif



/*
 * Type used for all fields of CIF records
 */
typedef unsigned int CIF_field;

#ifndef __inttypes_INCLUDED
typedef unsigned int uint32_t;
#endif /* __inttypes_INCLUDED */


/* 
 * Identification for a Mongoose-generated CIF file 
 */
#define CIF_MAGIC_STRING  "CIF"


/* 
 * Version number for Mongoose-generated CIF for C/C++ 
 */
#define CIF_C_VERSION  1U


/* 
 * Langugages supported in Mongoose-generated CIF 
 */
#define CIF_LANGUAGE_c          1U
#define CIF_LANGUAGE_cplusplus  2U


/*
 * CIF record tags for C/C++
 */
#define CIF_TAG_pu_def         1U
#define CIF_TAG_object         2U
#define CIF_TAG_basic_type     3U
#define CIF_TAG_one_kid_type   4U
#define CIF_TAG_two_kids_type  5U
#define CIF_TAG_n_kids_type    6U
#define CIF_TAG_enum_constant  7U
#define CIF_TAG_source_file    8U
#define CIF_TAG_macro          9U
#define CIF_TAG_namespace      10U
#define CIF_TAG_function       11U


/*
 * CIF object kinds 
 */
#define CIF_OBJECT_variable     1U
#define CIF_OBJECT_parameter    2U
#define CIF_OBJECT_data_member  3U


/*
 * CIF access kinds
 */
#define CIF_ACCESS_public     1U
#define CIF_ACCESS_private    2U
#define CIF_ACCESS_protected  3U


/* 
 * Basic type kinds 
 */
#define CIF_BASIC_TYPE_void     1U
#define CIF_BASIC_TYPE_bool     2U
#define CIF_BASIC_TYPE_wchar_t  3U
#define CIF_BASIC_TYPE_char     4U
#define CIF_BASIC_TYPE_int      5U
#define CIF_BASIC_TYPE_float    6U
#define CIF_BASIC_TYPE_double   7U


/* 
 * One-kid type kinds 
 */
#define CIF_ONE_KID_TYPE_pointer    1U
#define CIF_ONE_KID_TYPE_reference  2U
#define CIF_ONE_KID_TYPE_qualified  3U


/*
 * Two-kid type kinds
 */
#define CIF_TWO_KIDS_TYPE_array       1U
#define CIF_TWO_KIDS_TYPE_ptr_to_mbr  2U
#define CIF_TWO_KIDS_TYPE_typedef     3U


/*
 * N-kid type kinds
 */
#define CIF_N_KIDS_TYPE_class     1U
#define CIF_N_KIDS_TYPE_struct    2U
#define CIF_N_KIDS_TYPE_union     3U
#define CIF_N_KIDS_TYPE_enum      4U
#define CIF_N_KIDS_TYPE_function  5U


/*
 * CIF kid kinds
 */
#define CIF_KID_base_class          1U
#define CIF_KID_member_variable     2U
#define CIF_KID_member_function     3U
#define CIF_KID_member_type         4U
#define CIF_KID_friend_class        5U
#define CIF_KID_friend_function     6U
#define CIF_KID_enum_constant       7U
#define CIF_KID_nested_namespace    8U
#define CIF_KID_return_type         9U
#define CIF_KID_parameter_type      10U
#define CIF_KID_exception_type      11U


/* 
 * CIF usage codes
 * Extreme caution should be exercised when changing these;
 * they depend on symbol reference kinds generated in the front end.
 */
#define CIF_USAGE_CODE_declaration    0x1U
#define CIF_USAGE_CODE_definition     0x2U
#define CIF_USAGE_CODE_reference      0x4U
#define CIF_USAGE_CODE_use            0x8U
#define CIF_USAGE_CODE_modification   0x10U
#define CIF_USAGE_CODE_address_taken  0x20U
#define CIF_USAGE_CODE_error          0x40U
#define CIF_USAGE_CODE_implicit       0x80U


/*
 * CIF_file_header is always the first record in a CIF file
 */
typedef struct CIF_file_header {

  CIF_field magic_number;        /* magic number for identifying CIFs    */
  CIF_field cif_version;         /* version number for CIF format        */
  CIF_field language;            /* language in the primary source file  */
  CIF_field cif_level;           /* level of detail in CIF information   */

  CIF_field comp_dir_offset;     /* directory where compilation was done */
  CIF_field cmd_line_offset;     /* command line used to compile         */

  CIF_field num_pu_defs;         /* number of function definitions       */
  CIF_field pu_def_offset;       /* offset of the first func. def. scope */

  CIF_field num_src_files;       /* number of source files               */
  CIF_field src_file_list;       /* offset of the source file list       */

  CIF_field num_macros;          /* number of macro records              */
  CIF_field macro_offset;        /* offset of the first macro record     */

  CIF_field num_namespaces;      /* number of namespace records          */ 
  CIF_field namespace_offset;    /* offset of the first namespace        */

  CIF_field num_nonlocal_vars;   /* number of non-local variables        */
  CIF_field nonlocal_var_offset; /* offset of the first non-local var    */

  CIF_field num_nonlocal_types;  /* number of non-local types            */
  CIF_field nonlocal_type_offset;/* offset of the first non-local type   */

  CIF_field num_unnamed_types;   /* number of unnamed types              */
  CIF_field unnamed_type_offset; /* offset of the first unnamed type     */

  CIF_field num_functions;       /* number of function records           */ 
  CIF_field function_offset;     /* offset of the first function         */

  CIF_field hash_table_size;
  CIF_field hash_table_offset;

  CIF_field hash_chain_size;
  CIF_field hash_chain_offset;
  
  CIF_field id_map_size;         /* size of the CIF_index_entry array    */
  CIF_field id_map_offset;       /* offset of the CIF_index_entry array  */

  CIF_field string_table_size;
  CIF_field string_table_offset;

} CIF_file_header;



/*
 * CIF record used for function definitions
 */
typedef struct CIF_pu_def {
  CIF_field cif_tag:8;           /* record tag = CIF_TAG_pu_def  */
  CIF_field cif_id:24;           /* CIF id for the program unit  */
  CIF_field start_file_index:16; /*                              */
  CIF_field start_column:16;     /* First source position        */
  CIF_field start_line_num:32;   /*                              */
  CIF_field end_file_index:16;   /*                              */
  CIF_field end_column:16;       /* Last source position         */
  CIF_field end_line_num:32;     /*                              */
  CIF_field num_callees:32;      /* number of callees            */
} CIF_pu_def;


/*
 * CIF record used for variables, parameters, and data members (fields)
 */
typedef struct CIF_object {
  CIF_field cif_tag:8;        /* record tag = CIF_TAG_object     */
  CIF_field cif_id:24;        /* CIF ID for this var/param/field */
  CIF_field parent_id:24;     /* CIF ID of the parent            */
  CIF_field object_kind:2;    /* variable, parameter, data_member*/
  CIF_field access_kind:2;    /* public, private, protected      */
  CIF_field is_extern:1;      
  CIF_field is_static:1;
  CIF_field is_auto:1;
  CIF_field is_bitfield:1;    /* can be set only for fields      */
  CIF_field type_id:32;
  CIF_field num_usages:32;
} CIF_object;


/*
 * CIF record used for representing basic types.
 */
typedef struct CIF_basic_type {
  CIF_field cif_tag:8;       /* record tag = CIF_TAG_basic_type  */
  CIF_field cif_id:24;       /* CIF ID for the type              */
  CIF_field size:16;         /* size in bytes for this type      */
  CIF_field type_kind:8;     /* what kind of type is this        */
  CIF_field is_short:1; 
  CIF_field is_long:1; 
  CIF_field is_longlong:1; 
  CIF_field is_signed:1;     /* explicitly                       */
  CIF_field is_unsigned:1;   /* explicitly                       */
  CIF_field align_dummy:3;
} CIF_basic_type;


/*
 * CIF record for types that have one child type
 */
typedef struct CIF_one_kid_type {
  CIF_field cif_tag:8;       /* record tag = CIF_TAG_one_kid_type */
  CIF_field cif_id:24;       /* CIF ID for the type               */
  CIF_field size:32;         /* size of the type in bytes         */
  CIF_field base_type:24;    /* CIF IF of the base type           */
  CIF_field type_kind:2;     /* pointer/reference/qualified       */
  CIF_field is_const:1;   
  CIF_field is_volatile:1;   
  CIF_field is_restrict:1;   
  CIF_field align_dummy:3;
} CIF_one_kid_type;


/*
 * CIF record for types that have two children
 */
typedef struct CIF_two_kids_type {
  CIF_field cif_tag:8;       /* record tag = CIF_TAG_two_kids_type */
  CIF_field cif_id:24;       /* CIF ID for the type                */
  CIF_field size:32;         /* size of the type in bytes          */
  CIF_field base_type:24;    /* CIF ID of the base type            */
  CIF_field type_kind:8;     /* array/ptr_to_member/typedef        */
  union {
    CIF_field num_elems:32;  /* array: number of elements      */
    CIF_field class_id:32;   /* ptr_to_mbr: class cif_id       */
    CIF_field parent_id:32;  /* typedef: parent cif_id         */
  } other_kid;                
  CIF_field num_usages:32;   /* number of uses for this type   */
} CIF_two_kids_type;


/*
 * CIF record for aggregate types with variable number of children
 */
typedef struct CIF_n_kids_type {
  CIF_field cif_tag:8;       /* record tag = CIF_TAG_n_kids_type */
  CIF_field cif_id:24;       /* CIF ID for the type              */
  CIF_field parent_id:24;    /* CIF ID for the parent entity     */
  CIF_field type_kind:8;     /* class/struct/union/enum/func     */
  CIF_field size:32;         /* size in bytes for this type      */
  CIF_field num_kids:32;     /* number of kids for the type      */
  CIF_field num_usages:32;   /* number of uses for this type     */
} CIF_n_kids_type;


/*
 * CIF record for enumerator constants
 */
typedef struct CIF_enum_constant {
  CIF_field cif_tag:8;       /* record tag = CIF_TAG_enum_constant */
  CIF_field cif_id:24;       /* CIF ID for this enumerator         */
  CIF_field parent_id:32;    /* CIF ID for the enum type           */
  CIF_field value:32;        /* integer value of the constant      */
  CIF_field num_usages:32;   /* number of usages for this enum     */
} CIF_enum_constant;


/*
 * CIF record for representing source files
 */
typedef struct CIF_source_file {
  CIF_field cif_tag:8;           /* record tag = CIF_TAG_source_file */
  CIF_field cif_id:24;           /* CIF ID for the source file       */
  CIF_field parent_index:30;     /* parent file ID (0 if none)       */
  CIF_field is_include:1;        /* is it an included file           */
  CIF_field is_sys_include:1;    /* included by <file.h> notation    */
  CIF_field include_line:32;     /* #include location (0 if none)    */
  CIF_field name_offset:32;      /* StrTable offset of file name     */
} CIF_source_file;


/*
 * CIF record for representing macros
 */
typedef struct CIF_macro {
  CIF_field cif_tag:8;           /* record tag = CIF_TAG_macro    */
  CIF_field cif_id:24;           /* CIF ID of this macro          */
  CIF_field num_usages:32;       /* number of uses for this macro */
} CIF_macro;


/*
 * CIF record for representing namespaces 
 */
typedef struct CIF_namespace {
  CIF_field cif_tag:8;           /* record tag = CIF_TAG_namespace  */
  CIF_field cif_id:24;           /* CIF ID of this namespace        */
  CIF_field parent_id:24;        /* CIF ID of the parent            */
  CIF_field is_alias:8;          /* is this a namespace alias       */
  union {                        /* if is_alias                     */
    CIF_field alias_of:32;       /*   CIF ID of assoc_namespace     */
    CIF_field num_kids:32;       /* else                            */
  } extra_info;                  /*   number of kids                */
  CIF_field num_usages:32;       /* number of uses for namespace    */
} CIF_namespace;


/*
 * CIF record for representing functions (defined and/or declared)
 */
typedef struct CIF_function {
  CIF_field cif_tag:8;        /* record tag = CIF_TAG_function */
  CIF_field cif_id:24;        /* CIF ID for this function      */
  CIF_field parent_id:24;     /* CIF ID of the parent          */
  CIF_field access_kind:2;    /* public, private, protected    */
  CIF_field func_flags:6;     /* Flags to be filled later      */
  CIF_field type_id:24;       /* CIF ID for function type      */
  CIF_field is_defined:1;
  CIF_field is_extern:1;      
  CIF_field is_static:1;
  CIF_field is_inline:1;
  CIF_field is_virtual:1;
  CIF_field is_operator:1;
  CIF_field is_constructor:1;
  CIF_field is_destructor:1;
  CIF_field pu_def_offset:32; /* Offset of CIF_pu_def record   */
  CIF_field num_usages:32;    /* Number of func_usages         */
} CIF_function;


/*
 * CIF record for representing a generic child of another CIF record
 */
typedef struct CIF_kid {
  CIF_field kid_kind:4;
  CIF_field access_kind:2;
  CIF_field is_virtual:1;
  CIF_field is_static:1;
  CIF_field cif_id:24;
} CIF_kid;


/* 
 * CIF record for representing usage information (except for functions)
 */
typedef struct CIF_usage {
  CIF_field file_index:16;
  CIF_field column_number:16;
  CIF_field line_number:24;
  CIF_field usage_code:8;
} CIF_usage;


/*
 * CIF record for representing function usage information
 */
typedef struct CIF_func_usage {
  CIF_field caller_id;
  CIF_field file_index:16;
  CIF_field column_number:16;
  CIF_field line_number:24;
  CIF_field usage_code:8;
} CIF_func_usage;


/*
 * An entry in the ID map, which for each CIF ID contains
 * the file offset of the corresponding record and
 * the string table offset of the corresponding name
 */
typedef struct CIF_index_entry {
  CIF_field cif_tag:5;           /* Tag for identifying CIF record  */
  CIF_field name_offset:27;      /* String table offset of the name */
  CIF_field record_offset:32;    /* File offset of the CIF record   */
} CIF_index_entry;


/*
 * Each entry in the hash table contains the CIF ID of a
 * syntactic element, the string table offset for its name,
 * and a link to the next entry in the same hash chain.
 * Currently, cif_tag is not set!
 */
typedef struct CIF_hash_entry {
  CIF_field cif_tag:8;            /* Tag for identifying CIF record  */
  CIF_field cif_id:24;            /* Index into Cif_Id_Map           */
  CIF_field next_in_chain:32;     /* Index of the next hash-table    */
                                  /* entry in the same chain         */
} CIF_hash_entry;


/*
 * Generic CIF record that is a subset of all other records
 */
typedef struct CIF_generic {    
  CIF_field cif_tag:8;  
  CIF_field cif_id:24;           
} CIF_generic;
 

typedef CIF_field CIF_id;            /* Wrapper type for CIF IDs     */
typedef CIF_field CIF_tag;           /* Wrapper type for CIF tags    */
typedef CIF_field CIF_offset;        /* Wrapper type for CIF offsets */
typedef CIF_field CIF_kid_kind;      /* Wrapper type for kid kind    */
typedef CIF_field CIF_type_kind;     /* Wrapper type for type kind   */
typedef CIF_field CIF_object_kind;   /* Wrapper type for object kind */
typedef CIF_field CIF_access_kind;   /* Wrapper type for access kind */
typedef CIF_field CIF_usage_code;    /* Wrapper type for usage codes */


/***************************************************
 *    Macros for accessing fields of CIF records   *
 ***************************************************/

/* CIF_file_header*/
#define CIF_LANGUAGE(hdr)           ((hdr)->language)

#define CIF_NUM_PU_DEFS(hdr)        ((hdr)->num_pu_defs)
#define CIF_PU_DEF_LIST(hdr)        ((hdr)->pu_def_offset)

#define CIF_NUM_SRC_FILES(hdr)      ((hdr)->num_src_files)
#define CIF_SRC_FILE_LIST(hdr)      ((hdr)->src_file_list)

#define CIF_NUM_MACROS(hdr)         ((hdr)->num_macros)
#define CIF_MACRO_LIST(hdr)         ((hdr)->macro_offset)

#define CIF_NUM_NAMESPACES(hdr)     ((hdr)->num_namespaces)
#define CIF_NAMESPACE_LIST(hdr)     ((hdr)->namespace_offset)

#define CIF_NUM_NONLOCAL_VARS(hdr)  ((hdr)->num_nonlocal_vars)
#define CIF_NONLOCAL_VAR_LIST(hdr)  ((hdr)->nonlocal_var_offset)

#define CIF_NUM_NONLOCAL_TYPES(hdr) ((hdr)->num_nonlocal_types)
#define CIF_NONLOCAL_TYPE_LIST(hdr) ((hdr)->nonlocal_type_offset)

#define CIF_NUM_UNNAMED_TYPES(hdr)  ((hdr)->num_unnamed_types)
#define CIF_UNNAMED_TYPE_LIST(hdr)  ((hdr)->unnamed_type_offset)

#define CIF_NUM_FUNCTIONS(hdr)      ((hdr)->num_functions)
#define CIF_FUNCTION_LIST(hdr)      ((hdr)->function_offset)

#define CIF_HASH_TABLE_SIZE(hdr)    ((hdr)->hash_table_size)
#define CIF_HASH_TABLE_OFFSET(hdr)  ((hdr)->hash_table_offset)

#define CIF_HASH_CHAIN_SIZE(hdr)    ((hdr)->hash_chain_size)
#define CIF_HASH_CHAIN_OFFSET(hdr)  ((hdr)->hash_chain_offset)

#define CIF_ID_MAP_SIZE(hdr)        ((hdr)->id_map_size)
#define CIF_ID_MAP_OFFSET(hdr)      ((hdr)->id_map_offset)

#define CIF_STR_TABLE_SIZE(hdr)     ((hdr)->string_table_size)
#define CIF_STR_TABLE_OFFSET(hdr)   ((hdr)->string_table_offset)

#define CIF_STRING_TABLE(hdr)       ((char*)(hdr) + CIF_STR_TABLE_OFFSET(hdr))


/* For a given CIF ID get its CIF_index_entry */
#define CIF_INDEX_ENTRY(hdr,id) \
  ((CIF_index_entry*)(((char*)(hdr)) + CIF_ID_MAP_OFFSET(hdr)) + id)

/* For a given hash table index get its CIF_hash_entry */
#define CIF_HASH_TABLE_ENTRY(hdr,index) \
  ((CIF_hash_entry*)(((char*)(hdr)) + CIF_HASH_TABLE_OFFSET(hdr)) + index)

/* For a given hash chain index get its CIF_hash_entry */
#define CIF_HASH_CHAIN_ENTRY(hdr,index) \
  ((CIF_hash_entry*)(((char*)(hdr)) + CIF_HASH_CHAIN_OFFSET(hdr)) + index)

/* Get the CIF_source_file record with a given index (index is not CIF ID) */
/* It is safer to use the functional interface via cif_src_file            */
#define CIF_SRC_FILE(hdr,index) \
  ((CIF_source_file*)((char*)(hdr) + CIF_SRC_FILE_LIST(hdr)) + index)


/* Fields common to various CIF records */
#define CIF_ID(record)              ((record)->cif_id)
#define CIF_TAG(record)             ((record)->cif_tag)
#define CIF_TYPE(record)            ((record)->type_id)
#define CIF_PARENT(record)          ((record)->parent_id)
#define CIF_NUM_USAGES(record)      ((record)->num_usages)
#define CIF_ACCESS_KIND(record)     ((record)->access_kind)
#define CIF_NAME_OFFSET(record)     ((record)->name_offset)


/* CIF_index_entry */
#define CIF_RECORD_OFFSET(entry)    ((entry)->record_offset)


/* CIF_hash_entry */
#define CIF_NEXT_IN_CHAIN(entry)    ((entry)->next_in_chain)


/* CIF_pu_def 
 * start and end file indexes are not CIF IDs 
 */
#define CIF_PU_DEF_START_FILE(pu)   ((pu)->start_file_index)
#define CIF_PU_DEF_START_LINE(pu)   ((pu)->start_line_num)
#define CIF_PU_DEF_START_COL(pu)    ((pu)->start_column)
#define CIF_PU_DEF_END_FILE(pu)     ((pu)->end_file_index)
#define CIF_PU_DEF_END_LINE(pu)     ((pu)->end_line_num)
#define CIF_PU_DEF_END_COL(pu)      ((pu)->end_column)
#define CIF_PU_DEF_NUM_CALLEES(pu)  ((pu)->num_callees)
#define CIF_PU_DEF_CALLEES(pu)      ((CIF_id*)((pu)+1))
#define CIF_PU_DEF_START_FILE_ID(hdr,pu) \
  CIF_ID(CIF_SRC_FILE((hdr), CIF_PU_DEF_START_FILE(pu)))
#define CIF_PU_DEF_END_FILE_ID(hdr,pu) \
  CIF_ID(CIF_SRC_FILE((hdr), CIF_PU_DEF_END_FILE(pu)))


/* CIF_object */
#define CIF_OBJECT_KIND(obj)        ((obj)->object_kind)
#define CIF_MEMBER_STATIC(obj)      ((obj)->is_static)


/* Various CIF type records */
#define CIF_TYPE_KIND(type)         ((type)->type_kind)
#define CIF_TYPE_SIZE(type)         ((type)->size)
#define CIF_TYPE_SHORT(type)        ((type)->is_short)
#define CIF_TYPE_LONG(type)         ((type)->is_long)
#define CIF_TYPE_LONGLONG(type)     ((type)->is_longlong)
#define CIF_TYPE_SIGNED(type)       ((type)->is_signed)
#define CIF_TYPE_UNSIGNED(type)     ((type)->is_unsigned)
#define CIF_TYPE_CONST(type)        ((type)->is_const)
#define CIF_TYPE_VOLATILE(type)     ((type)->is_volatile)
#define CIF_TYPE_RESTRICT(type)     ((type)->is_restrict)
#define CIF_TYPE_BASE(type)         ((type)->base_type)
#define CIF_TYPE_NUM_KIDS(type)     ((type)->num_kids)

#define CIF_ARRAY_NUM_ELEMS(array)  ((array)->other_kid.num_elems)
#define CIF_PTR_TO_MBR_CLASS(ptr)   ((ptr)->other_kid.class_id)
#define CIF_TYPEDEF_PARENT(type)    ((type)->other_kid.parent_id)


/* CIF_enum_constant */
#define CIF_ENUM_VALUE(constant)    ((constant)->value)


/* CIF_source_file
 * parent_index is not the same as the parent's CIF ID 
 */
#define CIF_SRC_FILE_PARENT_INDEX(file) ((file)->parent_index)
#define CIF_SRC_FILE_IS_INCLUDE(file)   ((file)->is_include)
#define CIF_SRC_FILE_SYS_INCLUDE(file)  ((file)->is_sys_include)
#define CIF_SRC_FILE_INCLUDE_LINE(file) ((file)->include_line)

/* It is safer to use the functional interface via 
 * cif_src_file_parent_id and cif_src_file_full_name 
 */
#define CIF_SRC_FILE_PARENT_ID(hdr,file) \
  CIF_ID(CIF_SRC_FILE((hdr), CIF_SRC_FILE_PARENT_INDEX(file)))
#define CIF_SRC_FILE_FULL_NAME(hdr,file) \
  (CIF_STRING_TABLE(hdr) + CIF_NAME_OFFSET(file))


/* CIF_namespace */
#define CIF_NSP_IS_ALIAS(nsp)       ((nsp)->is_alias)
#define CIF_NSP_ALIAS_OF(nsp)       ((nsp)->extra_info.alias_of)
#define CIF_NSP_NUM_KIDS(nsp)       ((nsp)->extra_info.num_kids)


/* CIF_function */
#define CIF_FUNC_DEF_OFFSET(func)   ((func)->pu_def_offset)
#define CIF_FUNC_VIRTUAL(func)      ((func)->is_virtual)
#define CIF_FUNC_INLINE(func)       ((func)->is_inline)
#define CIF_FUNC_CTOR(func)         ((func)->is_constructor)
#define CIF_FUNC_DTOR(func)         ((func)->is_destructor)
#define CIF_FUNC_OPERATOR(func)     ((func)->is_operator)
#define CIF_FUNC_DEFINED(func)      ((func)->is_defined)
#define CIF_FUNC_USAGES(func)       ((CIF_func_usage*)((func)+1))

/* It is safer to use the functional interface via cif_func_def */
#define CIF_FUNC_PU_DEF(hdr,func) \
  ((CIF_pu_def*)((char*)((hdr)) + CIF_FUNC_DEF_OFFSET(func)))


/* CIF_kid records */
#define CIF_KID_ID(kid)             ((kid)->cif_id)
#define CIF_KID_KIND(kid)           ((kid)->kid_kind)
#define CIF_KID_VIRTUAL(kid)        ((kid)->is_virtual)
#define CIF_KID_STATIC(kid)         ((kid)->is_static)


/* CIF_usage records
 * file_index is not the same as CIF ID 
 */
#define CIF_USAGE_CALLER(usage)     ((usage)->caller_id)
#define CIF_USAGE_FILE_INDEX(usage) ((usage)->file_index)
#define CIF_USAGE_LINE(usage)       ((usage)->line_number)
#define CIF_USAGE_COLUMN(usage)     ((usage)->column_number)
#define CIF_USAGE_CODE(usage)       ((usage)->usage_code)
#define CIF_USAGE_FILE_ID(hdr,usage) \
  CIF_ID(CIF_SRC_FILE((hdr), CIF_USAGE_FILE_INDEX(usage)))


/* Get the name associated with a CIF_index_entry */
#define CIF_INDEX_ENTRY_NAME(hdr,entry) \
  (CIF_STRING_TABLE(hdr) + CIF_NAME_OFFSET(entry))

/* Get the name associated with a CIF_hash_entry */
#define CIF_HASH_ENTRY_NAME(hdr,entry) \
  CIF_INDEX_ENTRY_NAME(hdr, CIF_INDEX_ENTRY(hdr, CIF_ID(entry)))


#define CIF_ERROR 0


/* 
 * Hash function used for hashing named CIF records
 */
#if defined(__CIF_C_CONSUMER__) || defined(__CIF_C_PRODUCER__)
#ifndef __STRING_H__
#include <string.h>
#endif /* __STRING_H__ */
#ifdef __cplusplus
inline
#else 
static
#endif /* __cplusplus */ 
CIF_field
cif_hash (const char* name)
{
#define CIF_HASH_FACTOR ((CIF_field) 73)  
  CIF_field   hash_value = 0;
  const char* ptr        = name;
  size_t      length     = strlen(name);

  if (length > 9) {
    hash_value = (CIF_field)*ptr++;
    hash_value = (hash_value * CIF_HASH_FACTOR) + (CIF_field)*ptr++;
    hash_value = (hash_value * CIF_HASH_FACTOR) + (CIF_field)*ptr;
    ptr = name + (length >> 1) - 1;
    hash_value = (hash_value * CIF_HASH_FACTOR) + (CIF_field)*ptr++;
    hash_value = (hash_value * CIF_HASH_FACTOR) + (CIF_field)*ptr++;
    hash_value = (hash_value * CIF_HASH_FACTOR) + (CIF_field)*ptr;
    ptr = name + length - 3;
    hash_value = (hash_value * CIF_HASH_FACTOR) + (CIF_field)*ptr++;
    hash_value = (hash_value * CIF_HASH_FACTOR) + (CIF_field)*ptr++;
    hash_value = (hash_value * CIF_HASH_FACTOR) + (CIF_field)*ptr;
  } 
  else {
    size_t i;
    for (i = 0; i < length; ++i) {
      hash_value = (hash_value * CIF_HASH_FACTOR) + (CIF_field)*ptr++;
    }
  }

  return hash_value;
}
#endif /* __CIF_C_PRODUCER__ || __CIF_C_PRODUCER__ */


#ifdef __cplusplus
}
#endif
#endif /* __CIF_C_H */

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


/*!
 Rule Based Checker API reference
 */
#ifndef RBC_BASE_H
#define RBC_BASE_H

#ifdef LANG_JAVA
#define RBC_IDTYPE      long
#define RBC_UINT64      long
#define RBC_BOOL        long
#define RBC_TRUE        1
#define RBC_FALSE       0
#define RBC_STRING      String
#define STAR
#define PUBLIC
#define PRIVATE
#define PUB_MOD         public
#define PRIV_MOD        private
#define VOID
#define OBJECT          Object
#define RBC_CONST
#define STATIC          static
#define VOID_BODY       { return; }
#define INT_BODY        { return 0; }
#define OBJ_BODY        { return null; }
#define BOOL_BODY       INT_BODY
#define PACKAGE         package io.xc5;
#define ENUM_BEGIN(x)   public class x {
#define ENUMERATOR(x, y, v) public static final int y = v;
#define ENUM_END(x)     };
#define FILE_PTR        Object
#define TAGOBJ_VARARG   Object[] tagobjArr
#define TAGOBJ_IDX      long
#else // C, __cplusplus
#define RBC_IDTYPE      unsigned int
#define RBC_UINT64      unsigned long long
#define RBC_BOOL        int
#define RBC_TRUE        ((RBC_BOOL) 1)
#define RBC_FALSE       ((RBC_BOOL) 0)
#define RBC_STRING      char*
#define STAR            *
#define PUBLIC          public:
#define PRIVATE         private:
#define PUB_MOD
#define PRIV_MOD
#define VOID            void
#define OBJECT          void*
#define RBC_CONST       const
#define STATIC
#define VOID_BODY
#define INT_BODY
#define OBJ_BODY
#define BOOL_BODY
#define PACKAGE
#define ENUM_BEGIN(x)   typedef enum {
#define ENUMERATOR(x,y,v) x##_##y = v,
#define ENUM_END(x)     } x;
#define FILE_PTR        FILE*
#define TAGOBJ_VARARG   ...
#define TAGOBJ_IDX      unsigned int
#endif

PACKAGE
/**
 \class RBC_ENGINE
 \brief RBC_ENGINE is the container for all Rule Based Checker (RBC) API.

 RBC_ENGINE is the container for all RBC API for user definable rule implementation.
 Each rule file should define a global RBC_ENGINE instance named 'rbc'.
 All RBC API should be invoked as 'rbc.API(params)'. For example:

     RBC_ENGINE rbc;
     void Deprecated() {
         rbc.Rbc_assert(0, "Deprecated function is called");
     }

 RBC API supports the following data types:
 - Basic types:
     -# bool\n
bool type only with two possible values: true, false
     -# int\n
integer type. It's mapped to `int' in both C/C++ and Java rules.
     -# uint64\n
64-bit integer type. It's mapped to `long' in both C/C++ and Java rules.
     .
 - Reference types:
     -# string\n
A constant string object. For C/C++, it's mapped to const char*. For Java rule, it's mapped to java.lang.String.
     -# object\n
A general handle to RBC internal object. For C/C++, it's an pointer to internal object. For Java rule, it's a reference to the object.
     .
 .

 */

PUB_MOD class RBC_ENGINE {

PUBLIC

  ENUM_BEGIN(TYPE_KIND)
    ENUMERATOR(TYPE_KIND, INVALID, 0)
    ENUMERATOR(TYPE_KIND, PRIMITIVE, 1)
    ENUMERATOR(TYPE_KIND, CLASS,2)
    ENUMERATOR(TYPE_KIND, INTERFACE,3)
    ENUMERATOR(TYPE_KIND, TYPE_VARIABLE,4)
    ENUMERATOR(TYPE_KIND, ARRAY,5)
  ENUM_END(TYPE_KIND)

  ENUM_BEGIN(TAG_DEF_VAL)
    ENUMERATOR(TAG, KEEP, 0)
    ENUMERATOR(TAG, SET, 1)
    ENUMERATOR(TAG, UNSET,2)
  ENUM_END(TAG_DEF_VAL)

  ENUM_BEGIN(EXEC_KIND)
    // operator
    ENUMERATOR(EXEC_KIND,CONST,0)
    // Relational
    ENUMERATOR(EXEC_KIND,CMP_GT,1)
    ENUMERATOR(EXEC_KIND,CMP_GE,2)
    ENUMERATOR(EXEC_KIND,CMP_LT,3)
    ENUMERATOR(EXEC_KIND,CMP_LE,4)
    ENUMERATOR(EXEC_KIND,CMP_EQ,5)
    ENUMERATOR(EXEC_KIND,CMP_NE,6)
    // Arith operator
    ENUMERATOR(EXEC_KIND,ARITH_ADD,7)
    ENUMERATOR(EXEC_KIND,ARITH_SUB,8)
    ENUMERATOR(EXEC_KIND,ARITH_MPY,9)
    ENUMERATOR(EXEC_KIND,ARITH_DIV,10)
    ENUMERATOR(EXEC_KIND,ARITH_MOD,11)
    // Conditional
    ENUMERATOR(EXEC_KIND,AND,17)
    ENUMERATOR(EXEC_KIND,OR,18)
    ENUMERATOR(EXEC_KIND,NOT,16)
    ENUMERATOR(EXEC_KIND,IF,50)

    ENUMERATOR(EXEC_KIND,IS_STR_EQ, 197)
    ENUMERATOR(EXEC_KIND,IS_STR_SUB, 198)
    ENUMERATOR(EXEC_KIND,STR_REG_MATCH,199)

    // data (variable) related
    ENUMERATOR(EXEC_KIND,GET_ARG,200)
    ENUMERATOR(EXEC_KIND,GET_RET,201)
    ENUMERATOR(EXEC_KIND,GET_ARGCNT,202)
    ENUMERATOR(EXEC_KIND,GET_MEM_SIZE,203)
    ENUMERATOR(EXEC_KIND,GET_VALUE,204)
    ENUMERATOR(EXEC_KIND,GET_ELEM_COUNT,205)
    ENUMERATOR(EXEC_KIND,GET_THIS_POINTER,206)

    // data type related
    ENUMERATOR(EXEC_KIND,GET_TYPE_NAME,298)
    ENUMERATOR(EXEC_KIND,GET_TYPE_KIND,299)

    ENUMERATOR(EXEC_KIND,SET_PARM_TAINTED,300)
    ENUMERATOR(EXEC_KIND,SET_IMPLICIT_ASSIGN,301)
    ENUMERATOR(EXEC_KIND,SET_PARM_DEREF,302)
    ENUMERATOR(EXEC_KIND,SET_PARM_MOD,303)
    ENUMERATOR(EXEC_KIND,SET_PARM_BASE_AND_FLDNM,304)
    ENUMERATOR(EXEC_KIND,SET_FUNC_MAY_SLEEP,305)
    ENUMERATOR(EXEC_KIND,SET_ATOMIC_REGION_BEGIN,306)
    ENUMERATOR(EXEC_KIND,SET_ATOMIC_REGION_END,307)
    ENUMERATOR(EXEC_KIND,SET_FUNC_ATOMIC,308)
    ENUMERATOR(EXEC_KIND,SET_FUNC_SHUTDOWN,309)
    ENUMERATOR(EXEC_KIND,SET_FUNC_COLL_APPEND, 310)
    ENUMERATOR(EXEC_KIND,SET_FUNC_COLL_REMOVE, 311)
    ENUMERATOR(EXEC_KIND,SET_FUNC_COLL_GET, 312)
    ENUMERATOR(EXEC_KIND,SET_FUNC_COLL_BACK, 313)

    // resource related
    ENUMERATOR(EXEC_KIND,DECLARE_MALLOC_SIMILAR,505)
    ENUMERATOR(EXEC_KIND,DECLARE_FREE_SIMILAR,506)
    ENUMERATOR(EXEC_KIND,SET_TAG,510)
    ENUMERATOR(EXEC_KIND,UNSET_TAG,511)
    ENUMERATOR(EXEC_KIND,IS_TAG_SET,512)
    ENUMERATOR(EXEC_KIND,OR_TAG,513)
    ENUMERATOR(EXEC_KIND,COPY_TAG,514)
    ENUMERATOR(EXEC_KIND,EVAL_TAG,515)
    ENUMERATOR(EXEC_KIND,SET_TAG_CONST_DEFVAL,516)
    ENUMERATOR(EXEC_KIND,DECL_TAG_EQUAL,517)
    ENUMERATOR(EXEC_KIND,SET_TAG_TO_ALL_PARAMS,518)

    // assertion checks
    ENUMERATOR(EXEC_KIND,PRE_SANITIZED,600)
    ENUMERATOR(EXEC_KIND,PRE_CALL,604)
    ENUMERATOR(EXEC_KIND,POST_CHECK_VAR,606)
    ENUMERATOR(EXEC_KIND,POST_CHECK_VAR_VALUE,607)
    ENUMERATOR(EXEC_KIND,POST_CALL,610)
    ENUMERATOR(EXEC_KIND,PARM_IS_DEF_BY_FUNC,612)
    ENUMERATOR(EXEC_KIND,FUNC_MAY_ENTER_RECURSION,613)
    ENUMERATOR(EXEC_KIND,FUNC_MAY_NOT_RETURN,614)
    ENUMERATOR(EXEC_KIND,FUNC_IS_ASYNCHRONOUS_SAFE,615)
    ENUMERATOR(EXEC_KIND,FUNC_PERFORMS_SANITIZE,616)
    ENUMERATOR(EXEC_KIND,IS_AUTOMATIC_VARIABLE,620)
    ENUMERATOR(EXEC_KIND,IS_DYNAMICALLY_ALLOCATED_IF_COPIED,621)
    ENUMERATOR(EXEC_KIND,IS_COMPATIBLE_PARM_TYPE,622)
    ENUMERATOR(EXEC_KIND,IS_PARM_TAINTED,623)
    ENUMERATOR(EXEC_KIND,DO_NOT_GET_CALLED,624)
    ENUMERATOR(EXEC_KIND,DO_NOT_ACCESS_SHARED_OBJ,625)
    ENUMERATOR(EXEC_KIND,DO_NOT_CALL_SLEEP_IN_ATM,626)
    ENUMERATOR(EXEC_KIND,IMPLICIT_CALL,627)
    ENUMERATOR(EXEC_KIND,CALL_SUPER,629)
    ENUMERATOR(EXEC_KIND,FUNC_INVOKED_BY_SUBCLASS,630)
    ENUMERATOR(EXEC_KIND,IS_OBJ_METH_OVERRIDE,631)

    // JNI realted
    ENUMERATOR(EXEC_KIND,JNI_MODEL_PRAGMA,1000)
  ENUM_END(EXEC_KIND)

  /*!
   \fn RBC_ENGINE::Rbc_assert()
   \brief Assert `cond' is satisfied.
   \param cond the boolean expression to be asserted.
   \param rule_name the rule to be reported if the assertion fails.
   \return BOOL

   Assert `cond' is satisfied. Otherwise a vilation of `rule_name' is reported.

   */
  PUB_MOD STATIC RBC_BOOL   Rbc_assert(RBC_BOOL cond, RBC_CONST RBC_STRING rule_name) BOOL_BODY;

  // disable rule in certain function with condition
  PUB_MOD STATIC RBC_BOOL   Rbc_rule_exception(RBC_CONST RBC_STRING rule_name, RBC_BOOL except) BOOL_BODY;
  // register annotations
  PUB_MOD STATIC RBC_BOOL   Rbc_annotate(RBC_CONST RBC_STRING aname, RBC_BOOL expr) BOOL_BODY;
  // disable rules globally
  PUB_MOD STATIC RBC_BOOL   Rbc_disable_rule(RBC_CONST RBC_STRING rule_name) BOOL_BODY;

  // apply rule with rule_name
  PUB_MOD STATIC RBC_BOOL   Rbc_apply_rule(RBC_CONST RBC_STRING rule_name) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Rbc_enable_builtin(RBC_CONST RBC_STRING rule_name) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   For_all_exec_path(RBC_BOOL cond, RBC_CONST RBC_STRING rule_name) BOOL_BODY;
  // apply all model & assert the same as function 'fname'
  PUB_MOD STATIC RBC_BOOL   Rbc_same_as_func(RBC_CONST RBC_STRING fname) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Rbc_global_used(RBC_CONST RBC_STRING vname, RBC_CONST RBC_STRING file, RBC_UINT64 def_line, RBC_UINT64 call_line) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Rbc_define_call(RBC_CONST RBC_STRING fname, RBC_CONST RBC_STRING file, RBC_UINT64 def_line, RBC_UINT64 call_line) BOOL_BODY;

  /*!
   \fn RBC_ENGINE::For_all_func()
   \brief  execute expr on all function
   \param exec_expr execute expression that performs on all function

  Limitations:
    exec_expr only support Rbc_assert for now
    exec_expr cannot contains API that unable apply on all function, for ex:Get_arg/Get_ret which may return invalid results for different function
  */
  PUB_MOD STATIC RBC_BOOL   For_all_func(RBC_BOOL exec_expr) BOOL_BODY;

  /*!
   \fn RBC_ENGINE::Rbc_set_rule_set()
   \brief add rule with rule_name to rule_set
   \param rule_name the name of the rule
   \param rule_set the name of rule_set
   */
  PUB_MOD STATIC RBC_BOOL   Rbc_set_rule_set(RBC_CONST RBC_STRING rule_name, RBC_CONST RBC_STRING rule_set) BOOL_BODY;

  // Logical operations
  /*!
   \fn RBC_ENGINE::Not()
   \brief Perform logical `not' operation.
   \param v boolean expression
   \return boolean expression (!v)

   Perform a logical `not' operation on input boolean expression `v' and return the result `!(v)'.

   */
  PUB_MOD STATIC RBC_BOOL   Not(RBC_BOOL v) BOOL_BODY;
  /*!
   \fn RBC_ENGINE::Or()
   \brief Perform logical `or' operation.
   \param v1 boolean expression
   \param v2 boolean expression
   \return boolean expression of (v1 || v2)

   Perform a logical `or' operation between input boolean expression `v1' and `v2'; returns the result of `(v1 || v2)'.

   */
  PUB_MOD STATIC RBC_BOOL   Or(RBC_BOOL v1, RBC_BOOL v2) BOOL_BODY;
  /*!
   \fn RBC_ENGINE::And()
   \brief Perform logical `and' operation.
   \param v1 boolean expression
   \param v2 boolean expression
   \return boolean expression of (v1 && v2)

   Perform a logical `and' operation between input boolean expression `v1' and `v2'; returns the result of `(v1 && v2)'.

   */
  PUB_MOD STATIC RBC_BOOL   And(RBC_BOOL v1, RBC_BOOL v2) BOOL_BODY;

  // Conditional operations
  PUB_MOD STATIC RBC_BOOL   If(RBC_BOOL cond, RBC_BOOL true_exp, RBC_BOOL false_exp) BOOL_BODY;

  // pre-conditions, backward assertions
  PUB_MOD STATIC RBC_BOOL   Pre_sanitized(OBJECT v) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Pre_call(RBC_CONST RBC_STRING fname) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_errno_cleared_before(VOID) BOOL_BODY;

  // post-conditions, forward assertions
  PUB_MOD STATIC RBC_BOOL   Post_check_var_value(OBJECT v, RBC_CONST RBC_STRING opr, OBJECT value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Post_check_var_func(OBJECT v, RBC_CONST RBC_STRING fname, RBC_CONST RBC_STRING value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Post_call(RBC_CONST RBC_STRING fname) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_func_exec_successful(RBC_CONST RBC_STRING fname, RBC_CONST RBC_STRING opr, int value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_var_used_after(OBJECT v) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_var_defined_after(OBJECT v) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_var_invalid_and_used_after(OBJECT v) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_errno_checked_after(VOID) BOOL_BODY;

  // assertions about internal implementation
  PUB_MOD STATIC RBC_BOOL   Parm_is_def_by_func(OBJECT arg, RBC_CONST RBC_STRING func) BOOL_BODY;

  // assertion primitives
  PUB_MOD STATIC RBC_BOOL   Is_called_by(RBC_CONST RBC_STRING fname) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_called_in_thread(VOID) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_called_in_isr(VOID) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_called_in_loop(VOID) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_memory_overlap(OBJECT tgt, RBC_UINT64 size, OBJECT src) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_memory_big_enough(OBJECT tgt, RBC_UINT64 elem_sz, RBC_UINT64 elem_cnt) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Func_may_enter_recursion(OBJECT func) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Func_may_not_return(OBJECT func) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Func_is_asynchronous_safe(OBJECT func) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Func_performs_sanitize(VOID) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_automatic_variable(OBJECT v) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_dynamically_allocated_if_copied(OBJECT v) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_compatible_parm_type(int i) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_parm_tainted(OBJECT obj) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_std_output(OBJECT v) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Do_not_get_called(VOID) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Do_not_call(RBC_CONST RBC_STRING fname) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Do_not_access_shared_obj(OBJECT func) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Do_not_call_sleep_in_atm(VOID) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Implicit_call(OBJECT obj, RBC_CONST RBC_STRING fname, OBJECT parm) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Call_super() BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Func_invoked_by_subclass(RBC_CONST RBC_STRING fname) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_obj_meth_override(OBJECT obj, RBC_CONST RBC_STRING fname) BOOL_BODY;

  PUB_MOD STATIC RBC_BOOL   Model_decl(RBC_BOOL ret) BOOL_BODY;

  // assumption
  PUB_MOD STATIC RBC_BOOL   Assume_parm(int i, RBC_UINT64 lower, RBC_UINT64 upper) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Assume_var(RBC_CONST RBC_STRING var, RBC_UINT64 line, RBC_UINT64 lower, RBC_UINT64 upper) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Assume_ret(RBC_UINT64 lower, RBC_UINT64 upper) BOOL_BODY;

  // resource related
  PUB_MOD STATIC RBC_BOOL   Declare_malloc_similar(int i) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Declare_free_similar(int i) BOOL_BODY;

  // tag related
  PUB_MOD STATIC RBC_BOOL   Set_tag(OBJECT obj, RBC_STRING tag) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_tag_for_all_parm(RBC_STRING tag) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Unset_tag(OBJECT obj, RBC_CONST RBC_STRING tag) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_tag_attr(OBJECT tgt, OBJECT src, RBC_CONST RBC_STRING tag, RBC_CONST RBC_STRING tag_attr) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_tag_set(OBJECT v, RBC_CONST RBC_STRING tag) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_tag_attr_set(OBJECT v, RBC_CONST RBC_STRING tag, RBC_CONST RBC_STRING tag_attr) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_tag_attr_set_for_all_parm(RBC_CONST RBC_STRING tag, RBC_CONST RBC_STRING tag_attr) BOOL_BODY;

  PUB_MOD STATIC RBC_BOOL   Or_tag(OBJECT tgt, OBJECT src) BOOL_BODY;  // deprecated
  PUB_MOD STATIC RBC_BOOL   Merge_tag(OBJECT tgt, OBJECT src1, OBJECT src2) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Copy_tag(OBJECT tgt, OBJECT src) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Eval_tag(OBJECT tgt, OBJECT src) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_tag_const_defval(RBC_CONST RBC_STRING tag, RBC_UINT64 value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_tag_input_defval(RBC_CONST RBC_STRING tag, RBC_UINT64 value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Decl_tag_equal(RBC_CONST RBC_STRING tag1, RBC_CONST RBC_STRING tag2) BOOL_BODY;

  // function tag related
  /*!
  \fn RBC_ENGINE::Set_func_tag()
  \brief Set function same behaviour as <func_name>, and describe the side-effect sequence of return/parameter
  \param func_name the name of same behaviour function
  \param TAGOBJ_VARARG variable parameter to specify return/parameter's side-effect sequence
  \return BOOL TRUE if success, FALSE if failed
  Usage Example:
    // malloced memory is return value; malloc size is first parameter
    void* malloc (size_t size)
    {
      // malloc related rules
    }
    User defined their malloc function; and we want to attach all "malloc related rules" to user_malloc
    use below API
    void user_malloc(void *ptr, size_t size)
    {
      // "malloc" is the same behaviour function name
      // rbc.Get_arg(1) to specify func <malloc>'s return value - malloced memory
      // rbc.Get_arg(2) to specify func <malloc>'s first parameter - malloced size
      rbc.Model_decl(rbc.Set_func_tag("malloc", rbc.Get_arg(1), rbc.Get_arg(2));
    }
  */
  PUB_MOD STATIC RBC_BOOL   Set_func_tag(RBC_CONST RBC_STRING func_name, TAGOBJ_VARARG) BOOL_BODY;
  // type related
  /*!
  \fn RBC_ENGINE::Set_TY_is_mutex()
  \brief Set type with given type_name is mutex
  \param type_name the name of a type
  \return BOOL
  */
  PUB_MOD STATIC RBC_BOOL   Set_TY_is_mutex(RBC_STRING type_name) BOOL_BODY;
  /*!
  \fn RBC_ENGINE::Set_TY_is_atomic()
  \brief Set type is atomic
  \param type_name the name of a type
  \return BOOL
  */
  PUB_MOD STATIC RBC_BOOL   Set_TY_is_atomic(RBC_STRING type_name) BOOL_BODY;
  /*!
  \fn RBC_ENGINE::Set_TY_is_thread()
  \brief Set type is thread
  \param type_name the name of a type
  \return BOOL
  */
  PUB_MOD STATIC RBC_BOOL   Set_TY_is_thread(RBC_STRING type_name) BOOL_BODY;

  PUB_MOD STATIC RBC_STRING Get_type_name(OBJECT v) OBJ_BODY;
  PUB_MOD STATIC int        Get_type_kind(OBJECT v) INT_BODY;

  // data (variable) related
  PUB_MOD STATIC OBJECT     Get_this_pointer(VOID) OBJ_BODY;
  PUB_MOD STATIC OBJECT     Get_arg(int i) OBJ_BODY;
  PUB_MOD STATIC OBJECT     Get_ret(VOID) OBJ_BODY;
  PUB_MOD STATIC int        Get_argcnt(OBJECT func) INT_BODY;
  PUB_MOD STATIC int        Get_mem_size(OBJECT expr) INT_BODY;
  PUB_MOD STATIC int        Get_value(OBJECT expr) INT_BODY;
  PUB_MOD STATIC int        Get_elem_count(OBJECT v) INT_BODY;
  PUB_MOD STATIC int        Get_strlen(OBJECT v) INT_BODY;
  PUB_MOD STATIC int        Get_max_call_depth(VOID) INT_BODY;
  PUB_MOD STATIC int        Get_max_stack_size(VOID) INT_BODY;
  PUB_MOD STATIC int        Get_max_stack_size(OBJECT func) INT_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_parm_tainted(OBJECT obj) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_implicit_assign(OBJECT tgt, OBJECT src) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_parm_deref(OBJECT obj) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_parm_mod(OBJECT obj) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_parm_base_and_fld_name(OBJECT base, OBJECT name) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_may_sleep(OBJECT v) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_atomic_region_begin(VOID) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_atomic_region_end(VOID) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_atomic(VOID) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_shutdown(VOID) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_errno_setting(VOID) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_container_init(OBJECT obj, OBJECT value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_coll_append(OBJECT obj, OBJECT value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_coll_insert(OBJECT obj, OBJECT pos, OBJECT cnt, OBJECT value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_coll_remove(OBJECT obj, OBJECT idx) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_coll_get(OBJECT obj, OBJECT idx) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_coll_back(OBJECT obj) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_coll_end(OBJECT obj) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_map_put(OBJECT obj, OBJECT key, OBJECT value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_map_get(OBJECT obj, OBJECT key) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_str_get(OBJECT base, OBJECT idx) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_coll_append_ref(OBJECT obj, OBJECT value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_coll_get_ref(OBJECT obj, OBJECT idx) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_coll_back_ref(OBJECT obj) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_map_put_ref(OBJECT obj, OBJECT key, OBJECT value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_map_get_ref(OBJECT obj, OBJECT key) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_func_thread(RBC_BOOL is_multi_thread) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Set_class_sensitive() BOOL_BODY;

  PUB_MOD STATIC RBC_BOOL   Hard_coded_password(OBJECT var, OBJECT value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_null_term_str(OBJECT var, RBC_UINT64 len) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_const_str_eq(RBC_CONST RBC_STRING str1, RBC_CONST RBC_STRING str2) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_str_eq(OBJECT var, RBC_CONST RBC_STRING str) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_str_sub(OBJECT var, RBC_CONST RBC_STRING str) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_str_match(OBJECT var, RBC_CONST RBC_STRING str) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_parm_constant(OBJECT value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_init_by_const_str(OBJECT value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_compression_extraction_safe(OBJECT value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_parm_plain_old_func(OBJECT value) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_return_value_checked(VOID) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Is_parm_type_addr_passed(RBC_CONST RBC_STRING type_name) BOOL_BODY;

  // FSM related
  PUB_MOD STATIC RBC_BOOL   Fsm_use(RBC_CONST RBC_STRING fsm_name) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Fsm_build_begin(RBC_CONST RBC_STRING fsm_name) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Fsm_new_start_state(RBC_CONST RBC_STRING state_name) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Fsm_new_final_state(RBC_CONST RBC_STRING state_name) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Fsm_add_transition(RBC_CONST RBC_STRING state, RBC_CONST RBC_STRING action,
                                               OBJECT key, RBC_BOOL cond, RBC_CONST RBC_STRING next_state,
                                               RBC_CONST RBC_STRING rule_name, int msg_id) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Fsm_set_default_action(RBC_CONST RBC_STRING state_name,
                                                   RBC_CONST RBC_STRING rule_name, int msg_id) BOOL_BODY;
  PUB_MOD STATIC RBC_BOOL   Fsm_build_end(RBC_CONST RBC_STRING fsm_name) BOOL_BODY;

  // Evaluation APIs
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr) INT_BODY;
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr, RBC_UINT64 arg1) INT_BODY;
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr, OBJECT arg1) INT_BODY;
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr, RBC_UINT64 arg1, RBC_UINT64 arg2) INT_BODY;
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr, OBJECT arg1, OBJECT arg2) INT_BODY;
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr, RBC_UINT64 arg1, RBC_UINT64 arg2, RBC_UINT64 arg3) INT_BODY;
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr, OBJECT arg1, OBJECT arg2, OBJECT arg3) INT_BODY;
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr, RBC_UINT64 arg1, RBC_UINT64 arg2,
                                      RBC_UINT64 arg3, RBC_UINT64 arg4) INT_BODY;
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr, OBJECT arg1, OBJECT arg2,
                                      OBJECT arg3, OBJECT arg4) INT_BODY;
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr, RBC_UINT64 arg1, RBC_UINT64 arg2, RBC_UINT64 arg3,
                                      RBC_UINT64 arg4, RBC_UINT64 arg5) INT_BODY;
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr, OBJECT arg1, OBJECT arg2, OBJECT arg3, OBJECT arg4,
                                      OBJECT arg5) INT_BODY;
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr, RBC_UINT64 arg1, RBC_UINT64 arg2, RBC_UINT64 arg3,
                                      RBC_UINT64 arg4, RBC_UINT64 arg5, RBC_UINT64 arg6) INT_BODY;
  PUB_MOD STATIC RBC_UINT64 Exec_eval(int opr, OBJECT arg1, OBJECT arg2, OBJECT arg3, OBJECT arg4,
                                      OBJECT arg5, OBJECT arg6) INT_BODY;

  // JNI related
  PUB_MOD STATIC RBC_BOOL   Jni_model_pragma(VOID) BOOL_BODY;
}; // RBC_ENGINE

#endif


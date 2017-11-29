/* 
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 * File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
 * front-ends to GNU 3.2.2 release.
 */

/* Verification of a tree produced by the g++ front end in whole-
   translation-unit mode. The top-level function is verify_namespace.
   Verification does nothing visible if it's successful, and aborts
   upon failure.
*/

#include "gnu_config.h"

#include <assert.h>
#include <limits.h>
#include <stdio.h>

extern "C" {
#include "gnu/tree.h"
#include "cp-tree.h"
}

#include <hash_map>
#include <hash_set>
#include <vector>

// Tree verification.  The top-level function is verify_namespace.
// Most of the functions return void, and abort if any assumptions 
// are violated.  A few of them return a value, if there's something
// useful we can compute while doing the verification.

// Functions to verify decl nodes
void verify_namespace(tree ns);
void verify_global_namespace(tree ns);
enum scope { namespace_scope, class_scope, function_scope };
void verify_decl(tree decl, scope sc);
void verify_template_decl(tree decl, scope sc);
void verity_overload(tree ovl);
const char* verify_identifier(tree id);
const char* verity_ordinary_identifier(tree id);
void verify_function(tree fn);
void verify_ctor_initializer(tree ctor);
void verify_overload(tree fn, tree ns);
tree verify_label_decl(tree);
void verify_const_decl(tree);
void verify_var_decl(tree);
void verify_parm_decl(tree);
void verify_field_decl(tree);

// Functions to verify expr nodes
void verify_expr(tree expr);
void verify_integer_cst(tree);
void verify_string_cst(tree);
void verify_simple_string_cst(tree);
void verify_ptrmem_cst(tree);
void verify_negate_expr(tree);
void verify_bit_not_expr(tree);
void verify_truth_not_expr(tree);
void verify_incr_or_decr(tree);
void verify_addr_expr(tree);
void verify_indirect_ref(tree);
void verify_trunc_expr(tree);
void verify_float_expr(tree);
void verify_nop_expr(tree);
void verify_convert_expr(tree);
void verify_throw_expr(tree);
void verify_shift_expr(tree);
void verify_bitwise_binary_op(tree);
void verify_logical_binary_op(tree);
void verify_arithmetic_binary_op(tree);
void verify_comparison(tree);
void verify_modify_expr(tree);
void verify_component_ref(tree);
void verify_compound_expr(tree);
void verify_cond_expr(tree);
void verify_call_expr(tree);
void verify_bind_expr(tree);
void verify_constructor_expr(tree);
void verify_init_expr(tree);
void verify_save_expr(tree);
void verify_target_expr(tree);
void verify_array_ref(tree);
bool is_expr_code(tree_code);

// Functions to verify statement nodes
void verify_statement(tree stmt);
void verify_asm_statement(tree);
void verify_case_label(tree);
void verify_decl_statement(tree);
void verify_var_decl_statement(tree);
void verify_do_statement(tree);
void verify_for_statement(tree);
void verify_goto_statement(tree);
void verify_if_statement(tree);
void verify_return_statement(tree);
void verify_switch_statement(tree);
void verify_try_block(tree);
void verify_while_statement(tree);
void verify_compound_statement(tree stmt);
void verify_cleanup_statement(tree);
void verify_scope_statement(tree);
void verify_controlling_expr(tree parent, tree t);

// Functions to verify types
void verify_type(tree type);
void verify_integer_type(tree);
void verify_real_type(tree);
void verify_complex_type(tree);
void verify_enumeral_type(tree);
void verify_pointer_type(tree);
void verify_reference_type(tree);
void verify_function_type(tree);
void verify_method_type(tree);
void verify_function_arguments(tree);
void verify_array_type(tree);
void verify_class_type(tree);
const char* verify_named_type(tree);
void verify_type_name_is(tree, const char*);

// Other kinds of verification functions.
const char* verify_ordinary_identifier(tree);
const char* verify_identifier(tree id);

// Helper functions
tree ancestor_is_function(tree);
bool ancestor_is_loop(tree);
bool ancestor_is_switch(tree);
bool is_integral_type(tree);
bool is_numeric_scalar_type(tree);
bool is_integral_constant_expression(tree);
unsigned long integer_cst_remainder(tree a, unsigned long N);
bool integer_cst_le(size_t x, tree y);

// Helper functions for the parent map, a backward reference from a
// node (e.g. a function declaration) to its parent (e.g. a
// namespace).  A slightly subtle point: the "tree" is really a DAG;
// there are some cases (such as IDENTIFIER_NODEs) where we have
// several pointers to the same node.

// ??? The goal is that a node will appear in the parent map only when
// there is an unambiguous parent.  We put in statement nodes (a statement's
// parent is the block in which it appears) and declaration nodes (a
// declaration's parent is the scope in which the declaration appears).
// We don't put expression nodes or type nodes (except for classes)
// in the map.

// We also have a second, related map.  Classes and functions can refer
// to each other, so there's a danger of infinite loops.  The first time
// we verify a function or class we mark it as seen, so that we don't try
// to verify it again.

tree get_parent(tree);
tree get_parent_or_null(tree);
void verify_parentless(tree);
void add_child_to_map(tree parent, tree child);

bool tree_node_already_seen(tree);
void mark_tree_node_as_seen(tree);


//------------------------------------------------------------

void verify_global_namespace(tree ns)
{
  // It's a namespace;
  assert(ns != NULL_TREE);
  assert(TREE_CODE(ns) == NAMESPACE_DECL);

  // It's equal to global_namespace
  assert(ns == global_namespace);

  // It has no parent, and the DECL_CONTEXT is consistent with that.
  assert(get_parent_or_null(ns) == NULL_TREE);
  assert(DECL_CONTEXT(ns) == NULL_TREE);

  // The name is "::"
  assert(strcmp(verify_ordinary_identifier(DECL_NAME(ns)), "::") == 0);
}

void verify_namespace(tree ns)
{
  // It's a namespace.
  assert(ns != NULL_TREE);
  assert(TREE_CODE(ns) == NAMESPACE_DECL);

  // Checks that only make sense for std_node.
  if (ns == std_node) {
    // std_node doesn't appear if we're in honor-std mode.
    assert(!flag_honor_std);

    // Its parent is the global namespace.
    verify_global_namespace(get_parent(ns));
  }

  // Checks that only make sense for something other than std_node.
  else {
    // It has a name.
    const char* name = verify_ordinary_identifier(DECL_NAME(ns));

    // If it has any of the properties of the global namespace, it has all.
    if (ns == global_namespace ||
        get_parent_or_null(ns) == NULL_TREE ||
        DECL_CONTEXT(ns) == NULL_TREE ||
        strcmp(name, "::") == 0)
      verify_global_namespace(ns);

    // All of its declarations are correct.
    for (tree t = cp_namespace_decls(ns) ; t != NULL_TREE ; t = TREE_CHAIN(t)) {

      // Everthing in the chain is a declaration or a list.
      assert(TREE_CODE_CLASS(TREE_CODE(t)) == 'd' || TREE_CODE(t) == TREE_LIST);

      // If it's a list, then its value is an OVERLOAD and it has no purpose.
      if (TREE_CODE(t) == TREE_LIST) {
        verify_overload(TREE_VALUE(t), ns);
        assert(TREE_PURPOSE(t) == NULL_TREE);
      }

      // Otherwise, it's a valid decl.
      else {
        add_child_to_map(ns, t);
        verify_decl(t, namespace_scope);
      }
    }
  }
}

void verify_decl(tree decl, scope sc) {
  // Tests that are valid for any decl node.
  

  // Its class is correct.
  assert(decl != NULL_TREE);
  const tree_code code = TREE_CODE(decl);
  assert(TREE_CODE_CLASS(code) == 'd');

  
  // It has a valid identifier node.  (Except possible for functions and
  // type decls, which are more complicated.)
  if (code != FUNCTION_DECL && code != TYPE_DECL)
    verify_ordinary_identifier(DECL_NAME(decl));

  // It has a type.
  tree type = TREE_TYPE(decl);
  assert(type != NULL_TREE);
  bool is_integer = TREE_CODE(type) == INTEGER_TYPE;
  verify_type(type);

  // It has a source location.
  const char* file = DECL_SOURCE_FILE(decl);
  assert(file != 0);
  assert(strlen(file) != 0);
  assert(DECL_SOURCE_LINE(decl) >= 0);

  // Dispatch to verification of individual decl types.
  switch(code) {
  case FUNCTION_DECL:
    // g++ does not support nested functions
    assert(sc == namespace_scope || sc == class_scope);

    verify_function(decl);
    break;

  case LABEL_DECL:
    assert(sc == function_scope);
    verify_label_decl(decl);
    break;

  case CONST_DECL:
    assert(sc == namespace_scope || sc == class_scope || sc == function_scope);
    verify_const_decl(decl);
    break;

  case TYPE_DECL:
    assert(sc == namespace_scope || sc == class_scope || sc == function_scope);
    
    // ??? In the current front end, not all TYPE_DECLs have names.  Is
    // this a bug?  
    if (is_integer && DECL_NAME(type) == NULL_TREE) {
    }
    else
      verify_ordinary_identifier(DECL_NAME(decl));
    break;

  case VAR_DECL:
    // We use FIELD_DECL, not VAR_DECL, for member variables.
    assert(sc == namespace_scope || sc == function_scope);
    verify_var_decl(decl);
    break;

  case PARM_DECL:
    assert(sc == function_scope);
    verify_parm_decl(decl);
    break;

  case RESULT_DECL:
    assert(sc == function_scope);
    fprintf(stderr, "RESULT_DECLs should never occur in the IR\n");
    assert(0);

  case FIELD_DECL:
    assert(sc == class_scope);
    verify_field_decl(decl);
    break;

  case NAMESPACE_DECL:
    assert(sc == namespace_scope);
    verify_namespace(decl);
    break;

  case TEMPLATE_DECL:
    assert(sc == namespace_scope || sc == class_scope || sc == function_scope);
    verify_template_decl(decl, sc);
    break;

  case THUNK_DECL:
    assert(sc == class_scope || sc == namespace_scope);
    fprintf(stderr, "THUNK_DECLs should never occur in the IR\n");
    assert(0);

  case USING_DECL:
    assert(sc == namespace_scope || sc == class_scope || sc == function_scope);
    // These nodes may appear in the IR, but should be ignored by back ends.
    break;

  default:
    fprintf(stderr, "Unknown tree code %d\n", (int) code);
    assert(0);
  }
}

void verify_template_decl(tree tmpl, scope sc)
{
  assert(tmpl != NULL_TREE);
  assert(TREE_CODE(tmpl) == TEMPLATE_DECL);

  // List of specializations of the template.  
  tree spec = DECL_TEMPLATE_SPECIALIZATIONS(tmpl);
  while (spec != NULL_TREE) {
    assert(TREE_CODE(spec) == TREE_LIST);
    tree decl = TREE_VALUE(spec);
    assert(decl != NULL_TREE);

    switch(TREE_CODE(decl)) {
    case TEMPLATE_DECL:         // ??? What checks, if any, should we do here?
      break;
    case FUNCTION_DECL:
      add_child_to_map(tmpl, decl);
      verify_decl(decl, sc);
      break;
    default:
      fprintf(stderr, "Unknown tree code in tmpl specialization list",
              (int) TREE_CODE(decl));
      assert(0);
    }

    spec = TREE_CHAIN(spec);
  }
}


// An overload node, which is not a decl, may appear in namespace scope.
// (And nowhere else.) It represents a set of overloaded functions.
void verify_overload(tree ovl, tree enclosing_namespace)
{
  assert(ovl != NULL_TREE);
  assert(enclosing_namespace != NULL_TREE);
  assert(TREE_CODE(enclosing_namespace) == NAMESPACE_DECL);

  while (ovl != NULL_TREE) {
    assert(TREE_CODE(ovl) == OVERLOAD);

    tree fn = OVL_CURRENT(ovl);
    assert(fn != NULL_TREE);
    assert(TREE_CODE(fn) == FUNCTION_DECL);
    add_child_to_map(enclosing_namespace, fn);
    verify_decl(fn, namespace_scope);

    ovl = OVL_NEXT(ovl);
  }
}

void verify_function(tree fn)
{
  // It's a function node.
  assert(fn != NULL_TREE);
  assert(TREE_CODE(fn) == FUNCTION_DECL);

  if (tree_node_already_seen(fn))
    return;
  mark_tree_node_as_seen(fn);

  // Unless it's a special function, it has a name.
  if (!DECL_CONSTRUCTOR_P(fn) &&
      !DECL_DESTRUCTOR_P(fn) &&
      !DECL_OVERLOADED_OPERATOR_P(fn) &&
      !DECL_CONV_FN_P(fn) &&
      !DECL_ARTIFICIAL(fn))
    verify_ordinary_identifier(DECL_NAME(fn));
  
  // It has a mangled name regardless of whether it's a special function.
  tree id = DECL_ASSEMBLER_NAME(fn);
  assert(id != NULL_TREE);
  assert(TREE_CODE(id) == IDENTIFIER_NODE);
  assert(IDENTIFIER_LENGTH(id) > 0);
  assert(IDENTIFIER_POINTER(id) != 0);
  assert(IDENTIFIER_LENGTH(id) == strlen(IDENTIFIER_POINTER(id)));


  // Constructors, destructors, and conversion operators are member functions.
  if (DECL_CONSTRUCTOR_P(fn) || DECL_DESTRUCTOR_P(fn) || DECL_CONV_FN_P(fn))
    assert(DECL_FUNCTION_MEMBER_P(fn));

  // If it's a member function, then its parent is a class.  If it's
  // a template specialization then its parent is a template whose
  // parent is a namespace.  Otherwise, the parent is just a namespace.
  // There are no nested functions in C++.
  tree context = DECL_CONTEXT(fn);
  if (DECL_FUNCTION_MEMBER_P(fn)) {
    assert(context != NULL_TREE);
    assert(TREE_CODE(context) == RECORD_TYPE ||
           TREE_CODE(context) == UNION_TYPE);
  }
  else
    assert(context == NULL_TREE);
  
  tree parent = get_parent_or_null(fn);
  if (parent != NULL_TREE) {
    const tree_code parent_code = TREE_CODE(parent);
    if (DECL_FUNCTION_MEMBER_P(fn)) {
      assert(parent_code == RECORD_TYPE || parent_code == UNION_TYPE);
      assert(context != NULL_TREE);
      assert(same_type_p(TYPE_MAIN_VARIANT(context),
                         TYPE_MAIN_VARIANT(parent)));
    }
    else if (parent_code == TEMPLATE_DECL)
      assert(TREE_CODE(get_parent(parent)) == NAMESPACE_DECL);
    else
      assert(parent_code == NAMESPACE_DECL);
  }

  // DECL_ARGUMENTS points to a chain of valid PARM_DECL nodes.
  for (tree arg = DECL_ARGUMENTS(fn); arg ; arg = TREE_CHAIN(arg)) {

    // It's a PARM_DECL.
    assert(TREE_CODE(arg) == PARM_DECL);

    // We haven't already seen it.
    add_child_to_map(fn, arg);

    // It's a valid declaration.
    verify_decl(arg, function_scope);
  }

  // DECL_INITIAL is non-null if and only if DECL_SAVED_TREE is non-null.
  // DECL_INITIAL is either null or error_mark_node.
  tree init = DECL_INITIAL(fn);
  assert((init == NULL_TREE) == (DECL_SAVED_TREE(fn) == NULL_TREE));
  assert(init == NULL_TREE || init == error_mark_node ||
         TREE_CODE(init) == BLOCK);

  tree body = DECL_SAVED_TREE(fn);
  if (body != NULL_TREE) {
    // The body may be any valid statement.  For functions that
    // use the gnu named-return extension the body may also be a
    // RETURN_INIT, a special node that can appear nowhere else.

    if (TREE_CODE(body) == RETURN_INIT) {
      // Constructors never use the named-return extension.
      assert(!DECL_CONSTRUCTOR_P(fn));

      // We haven't already seen this statement.
      add_child_to_map(fn, body);

      // ??? What further checks should we do with the RETURN_INIT node?

      body = TREE_CHAIN(body);
      assert(body != NULL_TREE);
    }

    while (body != NULL_TREE) {
      add_child_to_map(fn, body); 
      verify_statement(body);
      body = TREE_CHAIN(body);
    }


  }
}

void verify_ctor_initializer(tree ctor)
{
  assert(ctor != NULL_TREE);
  assert(TREE_CODE(ctor) == CTOR_INITIALIZER);

  // ??? How do we verify the base- and member-initializers?

  tree ctor_body = TREE_CHAIN(ctor);
  assert(ctor_body != NULL_TREE);
  assert(TREE_CODE(ctor_body) == COMPOUND_STMT);
  add_child_to_map(ctor, ctor_body);
  verify_compound_statement(ctor_body);
}

// Return value is the function that the label is declared in.
tree verify_label_decl(tree decl)
{
  assert(decl != NULL_TREE);
  assert(TREE_CODE(decl) == LABEL_DECL);

  verify_ordinary_identifier(DECL_NAME(decl));

  tree fn = ancestor_is_function(decl);
  assert(fn != NULL_TREE);
  return fn;
}

void verify_const_decl(tree decl)
{
  assert(decl != NULL_TREE);
  assert(TREE_CODE(decl) == CONST_DECL);

  // It has a name.
  verify_ordinary_identifier(DECL_NAME(decl));

  // It has a type.
  tree decl_type = TREE_TYPE(decl);
  assert(decl_type != NULL_TREE);
  assert(TREE_CODE_CLASS(TREE_CODE(decl_type)) == 't');

  // It has a value, which is an INTEGER_CST.
  tree value = DECL_INITIAL(decl);
  assert(value != NULL_TREE);
  assert(TREE_CODE(value) == INTEGER_CST);

  // The value has an associated type.
  tree value_type = TREE_TYPE(value);
  assert(value_type != NULL_TREE);
  assert(TREE_CODE_CLASS(TREE_CODE(value_type)) == 't');

  // The decl type and value type are the same.
  assert(same_type_p(decl_type, value_type));
}

void verify_var_decl(tree decl)
{
  assert(decl != NULL_TREE);
  assert(TREE_CODE(decl) == VAR_DECL);

  // The variable has a type.
  tree var_type = TREE_TYPE(decl);
  assert(var_type != NULL_TREE);
  assert(TREE_CODE_CLASS(TREE_CODE(var_type)) == 't');

  // Variable's size is at least as large as type size.
  assert(!tree_int_cst_lt(DECL_SIZE(decl), TYPE_SIZE(var_type)));

  // Alignment is nonzero, and is a multiple of the byte size.
  assert(DECL_ALIGN(decl) > 0);
  assert((DECL_ALIGN(decl) % CHAR_BIT) == 0);

  // The variable may have an initializer.  If it has one, then
  // it's a valid expression with the same type as the variable.
  tree initializer = DECL_INITIAL(decl);
  if (initializer != NULL_TREE) {

    // It's an expression.

    assert(is_expr_code(TREE_CODE(initializer)));

    // It has a type, which is the same as the variable's type.
    tree initializer_type = TREE_TYPE(initializer);
    assert(initializer_type != NULL_TREE);
    assert(TREE_CODE_CLASS(TREE_CODE(initializer_type)) == 't');
    verify_type(initializer_type);
    assert(same_type_p(var_type, initializer_type));

    // It's a valid expression.
    verify_expr(initializer);
  }
}

void verify_parm_decl(tree decl)
{
  assert(decl != NULL_TREE);
  assert(TREE_CODE(decl) == PARM_DECL);

  // The parameter has a type.
  tree var_type = TREE_TYPE(decl);
  assert(var_type != NULL_TREE);
  assert(TREE_CODE_CLASS(TREE_CODE(var_type)) == 't');

  // Parameter's size is at least as large as type size.
  assert(!tree_int_cst_lt(DECL_SIZE(decl), TYPE_SIZE(var_type)));

  // Alignment is nonzero and is a multiple of the byte size.
  assert(DECL_ALIGN(decl) > 0);
  assert((DECL_ALIGN(decl) % CHAR_BIT) == 0);

  // ??? A parameter has no initializer.  There seems to be a bug.
  //  assert(DECL_INITIAL(decl) == NULL_TREE);
  
  // Chain is either null or another parm decl.
  tree chain = TREE_CHAIN(decl);
  assert(chain == NULL_TREE || TREE_CODE(chain) == PARM_DECL);

  // The node appears in a function's argument list.
  tree fn = get_parent(decl);
  assert(TREE_CODE(fn) == FUNCTION_DECL);

  tree cur = DECL_ARGUMENTS(fn);
  while (cur != NULL_TREE && cur != decl)
    cur = TREE_CHAIN(cur);
  assert(cur == decl);
}

void verify_field_decl(tree decl)
{
  assert(decl != NULL_TREE);
  assert(TREE_CODE(decl) == FIELD_DECL);
  
  // The field has a type.
  tree field_type = TREE_TYPE(decl);
  assert(field_type != NULL_TREE);
  assert(TREE_CODE_CLASS(TREE_CODE(field_type)) == 't');

  // The field has a non-negative size and offset, and a positive alignment.
  // The alignment is a multiple of the byte size, and the offset is a 
  // multiple of the alignment.
  assert(!tree_int_cst_lt(DECL_SIZE(decl), integer_zero_node));
  assert(DECL_ALIGN(decl) > 0);
  assert((DECL_ALIGN(decl) % CHAR_BIT) == 0);

  tree offset = DECL_FIELD_BITPOS(decl);
  assert(offset != NULL_TREE);
  assert(TREE_CODE(offset) == INTEGER_CST);
  assert(tree_int_cst_sgn(offset) >= 0);
  assert(integer_cst_remainder(offset, DECL_ALIGN(decl)) == 0);

  // The chain is either null or another non-function class member.
  tree chain = TREE_CHAIN(decl);
  assert(chain == NULL_TREE ||
         TREE_CODE(chain) == FIELD_DECL ||
         TREE_CODE(chain) == VAR_DECL ||
         TREE_CODE(chain) == TYPE_DECL ||
         TREE_CODE(chain) == CONST_DECL);

  // The field is a member of a class.
  tree parent  = get_parent(decl);
  tree context = DECL_CONTEXT(decl);
  assert(context != NULL_TREE);
  assert(TREE_CODE(parent) == RECORD_TYPE || TREE_CODE(parent) == UNION_TYPE);
  assert(TREE_CODE(context) == RECORD_TYPE ||
         TREE_CODE(context) == UNION_TYPE);
  assert(same_type_p(TYPE_MAIN_VARIANT(parent), TYPE_MAIN_VARIANT(context)));

  tree cur = TYPE_FIELDS(parent);
  while (cur != NULL_TREE && cur != decl)
    cur = TREE_CHAIN(cur);
  assert(cur == decl);
}

//------------------------------------------------------------

void verify_statement(tree st)
{
  // Verify that the class is consistent with it being a statement.
  // (statements have the same class as expressions.)
  assert(st != NULL_TREE);
  const tree_code code = TREE_CODE(st);
  assert(code == CASE_LABEL || code == LABEL_DECL || 
         TREE_CODE_CLASS(code) == 'e');

  // Everything except CASE_LABEL and LABEL_DECL has a valid
  // STMT_LINENO.
  if (code == LABEL_DECL)
    assert(DECL_SOURCE_LINE(st) >= 0);
  else if (code != CASE_LABEL)
    assert(STMT_LINENO(st) >= 0);

  // The statement was declared in function scope.
  assert(ancestor_is_function(st));

  // Dispatch on various statement types.
  switch(code) {
  case ASM_STMT:
    verify_asm_statement(st);
    break;

  case BREAK_STMT:
    assert(ancestor_is_loop(st) || ancestor_is_switch(st));
    break;    
    
  case CASE_LABEL:
    verify_case_label(st);
    break;
    
  case COMPOUND_STMT:
    verify_compound_statement(st);
    break;
    
  case CONTINUE_STMT:
    assert(ancestor_is_loop(st));
    break;
    
  case DECL_STMT:
    verify_decl_statement(st);
    break;

  case DO_STMT:
    verify_do_statement(st);
    break;
    
  case EXPR_STMT:
    break;
    
  case FOR_STMT:
    verify_for_statement(st);
    break;
    
  case GOTO_STMT:
    verify_goto_statement(st);
    break;
    
  case IF_STMT:
    verify_if_statement(st);
    break;
    
  case LABEL_DECL:
    verify_label_decl(st);
    break;

  case LABEL_STMT:
    {
      tree label = LABEL_STMT_LABEL(st);
      assert(label != NULL_TREE);
      assert(TREE_CODE(label) == LABEL_DECL);
      add_child_to_map(st, label);
      verify_label_decl(label);
    }
    break;
    
  case RETURN_STMT:
    verify_return_statement(st);
    break;
    
  case SWITCH_STMT:
    verify_switch_statement(st);
    break;
    
  case TRY_BLOCK:
    verify_try_block(st);
    break;
    
  case WHILE_STMT:
    verify_while_statement(st);
    break;

  case CLEANUP_STMT:
    verify_cleanup_statement(st);
    break;

  case SCOPE_STMT:
    verify_scope_statement(st);
    break;

  default:
    fprintf(stderr, "Unknown tree code for statement, %d\n",
            (int) code);
    assert(0);   
  }
}

void verify_asm_statement(tree st)
{
  verify_simple_string_cst(ASM_STRING(st));

  // ??? Are there any checks we can do with ASM_VOLATILE_P(st)?

  if (ASM_OUTPUTS(st) != NULL_TREE)
    verify_simple_string_cst(ASM_OUTPUTS(st));
  if (ASM_INPUTS(st) != NULL_TREE)
    verify_simple_string_cst(ASM_INPUTS(st));
  if (ASM_CLOBBERS(st) != NULL_TREE)
    verify_simple_string_cst(ASM_CLOBBERS(st));
}

void verify_case_label(tree st)
{
  assert(ancestor_is_switch(st));

  tree low = CASE_LOW(st);
  tree high = CASE_HIGH(st);

  if (low == NULL_TREE)                 // default label
    assert(high == NULL_TREE);
  else if (high == NULL_TREE)  { // Ordinary case label.
    verify_expr(low);
    assert(is_integral_constant_expression(low));
  }
  else {                      // gnu extension, case label with range.
    verify_expr(low);
    verify_expr(high);
    assert(is_integral_constant_expression(low));
    assert(is_integral_constant_expression(high));
    assert(same_type_p(TREE_TYPE(low), TREE_TYPE(high)));
  }
}

void verify_compound_statement(tree st)
{
  // tree code is correct.
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == COMPOUND_STMT);

  // It has a valid STMT_LINENO.
  assert(STMT_LINENO(st) >= 0);

  // It was declared in function scope.
  ancestor_is_function(st);
 
  // Each of the substatements is valid.
  for (tree cur = COMPOUND_BODY(st) ; cur ; cur = TREE_CHAIN(cur)) {
    // We haven't already seen it.
    add_child_to_map(st, cur);

    // It's a valid statement.
    verify_statement(cur);
  }
}

void verify_decl_statement(tree st)
{
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == DECL_STMT);
  
  tree decl = DECL_STMT_DECL(st);
  assert(decl != NULL_TREE);
  assert(TREE_CODE_CLASS(TREE_CODE(decl)) == 'd');

  // ??? Should we always enter the decl into the parent map?
  add_child_to_map(st, decl);

  verify_decl(decl, function_scope);
}

// Like verify_decl_statement, but also verify that it's a variable
// declaration.
void verify_var_decl_statement(tree st)
{
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == DECL_STMT);
  
  tree decl = DECL_STMT_DECL(st);
  assert(decl != NULL_TREE);
  assert(TREE_CODE_CLASS(TREE_CODE(decl)) == 'd');
  assert(TREE_CODE(decl) == VAR_DECL);

  // ??? Should we always enter the decl into the parent map?
  add_child_to_map(st, decl);

  verify_decl(decl, function_scope);
}

void verify_do_statement(tree st)
{
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == DO_STMT);

  tree body = DO_BODY(st);
  tree test = DO_COND(st);

  assert(body != NULL_TREE);
  assert(test != NULL_TREE);

  add_child_to_map(st, body);

  verify_statement(body);
  verify_controlling_expr(st, test);
}

void verify_for_statement(tree st)
{
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == FOR_STMT);

  tree init = FOR_INIT_STMT(st);
  tree test = FOR_COND(st);
  tree iter = FOR_EXPR(st);
  tree body = FOR_BODY(st);

  if (init != NULL_TREE) {
    add_child_to_map(st, init);
    verify_statement(init);
  }

  if (test != NULL_TREE) {
    verify_controlling_expr(st, test);
  }

  if (iter != NULL_TREE) {
    verify_expr(iter);
  }

  assert(body != NULL_TREE);
  add_child_to_map(st, body);
  verify_statement(body);
}

void verify_goto_statement(tree st)
{
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == GOTO_STMT);

  // Function that the goto statement is found in
  tree parent_fn = ancestor_is_function(st);
  assert(parent_fn != NULL_TREE);

  // Where we're jumping to.
  tree dest = GOTO_DESTINATION(st);
  assert(dest != NULL_TREE);

  // The ordinary case, we're jumping to a label.
  if (TREE_CODE(dest) == LABEL_DECL) {
    verify_ordinary_identifier(DECL_NAME(dest));
  }

  // We're using the gnu computed goto extension.  Destination is an 
  // expression with pointer type.
  else {
    verify_expr(dest);
    assert(TREE_CODE(TREE_TYPE(dest)) == POINTER_TYPE);
  }
}

void verify_if_statement(tree st)
{
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == IF_STMT);

  tree cond    = IF_COND(st);
  tree then_st = THEN_CLAUSE(st);
  tree else_st = ELSE_CLAUSE(st);

  assert(cond != NULL_TREE);
  verify_controlling_expr(st, cond);

  assert(then_st != NULL_TREE);
  add_child_to_map(st, then_st);
  verify_statement(then_st);

  if (else_st != NULL_TREE) {
    add_child_to_map(st, else_st);
    verify_statement(else_st);
  }
}

void verify_return_statement(tree st)
{
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == RETURN_STMT);

  tree retval = RETURN_EXPR(st);
  if (retval) {                 // May be null for an unadorned return.
    verify_expr(retval);
  }
}

void verify_switch_statement(tree st)
{
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == SWITCH_STMT);

  tree cond = SWITCH_COND(st);
  tree body = SWITCH_BODY(st);

  assert(cond != NULL_TREE);
  assert(body != NULL_TREE);

  add_child_to_map(st, body);

  // cond is either a DECL_STMT or an expr.
  if (TREE_CODE(cond) == DECL_STMT)
    verify_var_decl_statement(cond);
  else
    verify_controlling_expr(st, cond);

  // The body must be a compound statement.
  assert(TREE_CODE(body) == COMPOUND_STMT);
  verify_statement(body);
}

void verify_try_block(tree st) {
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == TRY_BLOCK);

  // The body is a compound statement.
  tree body = TRY_STMTS(st);
  assert(body != NULL_TREE);
  add_child_to_map(st, body);
  assert(TREE_CODE(body) == COMPOUND_STMT);
  verify_statement(body);
  
  // The catch clauses are a chain of HANDLER nodes.
  for (tree cur = TRY_HANDLERS(st) ; cur != NULL_TREE ; cur = TREE_CHAIN(cur)) {
    assert(TREE_CODE(cur) == HANDLER);
    add_child_to_map(st, cur);

    // ??? What can we do with HANDLER_PARMS(cur)?

    // The handler body is a compund statement.
    tree hb = HANDLER_BODY(cur);
    assert(hb != NULL_TREE);
    add_child_to_map(st, hb);
    assert(TREE_CODE(hb) == COMPOUND_STMT);
    verify_statement(hb);
  }

}

void verify_while_statement(tree st)
{
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == WHILE_STMT);

  tree cond = WHILE_COND(st);
  tree body = WHILE_BODY(st);

  assert(cond != NULL_TREE);
  assert(body != NULL_TREE);

  add_child_to_map(st, body);

  // cond is either a DECL_STMT, as in "while (int c = getc())", or an expr.
  if (TREE_CODE(cond) == DECL_STMT)
    verify_var_decl_statement(cond);
  else
    verify_controlling_expr(st, cond);

  verify_statement(body);
}

void verify_cleanup_statement(tree st)
{
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == CLEANUP_STMT);

  tree decl = CLEANUP_DECL(st);
  tree expr = CLEANUP_EXPR(st);

  assert(decl != NULL_TREE);
  assert(expr != NULL_TREE);
  assert(TREE_CODE(decl) == VAR_DECL);
  assert(is_expr_code(TREE_CODE(expr)));

  verify_decl(decl, function_scope);
  verify_expr(expr);
}

void verify_scope_statement(tree st)
{
  assert(st != NULL_TREE);
  assert(TREE_CODE(st) == SCOPE_STMT);
  // ??? Is there anything we can test here?
}

// t is something that appears as a controlling expression for node
// parent, which is an if/while/for/do statement.  This helper
// function checks if it's valid.  The "controlling expression" might
// be a simple expr node, or it might be something more complicated.
void verify_controlling_expr(tree parent, tree t)
{
  assert (t != NULL_TREE);
  tree expr = t;

  // The controlling expression might be an arbitrary number of
  // statements followed by an expr.  If so, it is represented 
  // as a list node.
  if (TREE_CODE(t) == TREE_LIST) {
    tree st = TREE_PURPOSE(t);
    while (st != NULL_TREE) {
      add_child_to_map(parent, st);
      verify_statement(st);
      st = TREE_CHAIN(st);
    }

    expr = TREE_VALUE(t);
  }

  // The expression itself is valid.
  verify_expr(expr);

  // It is integer- or boolean-typed.
  tree type = TREE_TYPE(expr);
  tree_code code = TREE_CODE(type);
  assert(code == INTEGER_TYPE || code == BOOLEAN_TYPE ||
         code == ENUMERAL_TYPE);
}

//------------------------------------------------------------

void verify_expr(tree expr)
{
  // Verify that the class is consistent with it being an expr.
  assert(expr != NULL_TREE);
  const tree_code code = TREE_CODE(expr);
  assert(is_expr_code(code));

  // All expressions have types.  Verify that the type is sane.
  tree type = TREE_TYPE(expr);
  assert(type != NULL_TREE);
  verify_type(type);

  switch(code) {
  case INTEGER_CST:
    verify_integer_cst(expr);
    break;

  case REAL_CST:
    // ??? What validity checks can we do on a REAL_CST?
    break;

  case STRING_CST:
    verify_string_cst(expr);
    break;

  case PTRMEM_CST:
    verify_ptrmem_cst(expr);
    break;

  case VAR_DECL:
    verify_var_decl(expr);
    break;    

  case PARM_DECL:
    verify_parm_decl(expr);
    break;    

  case NEGATE_EXPR:
    verify_negate_expr(expr);
    break;

  case BIT_NOT_EXPR:
    verify_bit_not_expr(expr);
    break;

  case TRUTH_NOT_EXPR:
    verify_truth_not_expr(expr);
    break;

  case PREDECREMENT_EXPR:
  case POSTDECREMENT_EXPR:
  case PREINCREMENT_EXPR:
  case POSTINCREMENT_EXPR:
    verify_incr_or_decr(expr);
    break;

  case ADDR_EXPR:
    verify_addr_expr(expr);
    break;

  case INDIRECT_REF:
    verify_indirect_ref(expr);
    break;

  case FIX_TRUNC_EXPR:
    verify_trunc_expr(expr);
    break;

  case FLOAT_EXPR:
    verify_float_expr(expr);
    break;

  case NOP_EXPR:
  case NON_LVALUE_EXPR:
    verify_nop_expr(expr);
    break;

  case CONVERT_EXPR:
    verify_convert_expr(expr);
    break;

  case THROW_EXPR:
    verify_throw_expr(expr);
    break;

  case LSHIFT_EXPR:
  case RSHIFT_EXPR:
    verify_shift_expr(expr);
    break;

  case BIT_IOR_EXPR:
  case BIT_XOR_EXPR:
  case BIT_AND_EXPR:
    verify_bitwise_binary_op(expr);
    break;

  case TRUTH_ANDIF_EXPR:
  case TRUTH_ORIF_EXPR:
    verify_logical_binary_op(expr);
    break;

  case PLUS_EXPR:
  case MINUS_EXPR:
  case MULT_EXPR:
  case TRUNC_DIV_EXPR:
  case TRUNC_MOD_EXPR:
  case RDIV_EXPR:
    verify_arithmetic_binary_op(expr);
    break;

  case LT_EXPR:
  case LE_EXPR:
  case GT_EXPR:
  case GE_EXPR:
  case EQ_EXPR:
  case NE_EXPR:
    verify_comparison(expr);
    break;

  case MODIFY_EXPR:
    verify_modify_expr(expr);
    break;

  case COMPONENT_REF:
    verify_component_ref(expr);
    break;

  case COMPOUND_EXPR:
    verify_compound_expr(expr);
    break;

  case COND_EXPR:
    verify_cond_expr(expr);
    break;

  case CALL_EXPR:
    verify_call_expr(expr);
    break;

  case BIND_EXPR:
    verify_bind_expr(expr);
    break;

  case CONSTRUCTOR:
    verify_constructor_expr(expr);
    break;

  case INIT_EXPR:
    verify_init_expr(expr);
    break;

  case SAVE_EXPR:
    verify_save_expr(expr);
    break;

  case TARGET_EXPR:
    verify_target_expr(expr);
    break;

  case ARRAY_REF:
    verify_array_ref(expr);
    break;

  case AGGR_INIT_EXPR:
    fprintf(stderr,
            "AGGR_INIT_EXPR may only appear as 2nd operand of TARGET_EXPR\n");
    assert(0);

  default:
    fprintf(stderr, "Unknown tree code for expression, %d\n",
            (int) code);
    assert(0);       
  }
}

void verify_integer_cst(tree cst)
{
  assert(cst != NULL_TREE);
  assert(TREE_CODE(cst) == INTEGER_CST);

  // It's an integral type.
  tree type = TREE_TYPE(cst);
  assert(is_integral_type(type));

  // The value is consistent with the min and max bounds for the type.
  assert(!tree_int_cst_lt(cst, TYPE_MIN_VALUE(type)));
  assert(!tree_int_cst_lt(TYPE_MAX_VALUE(type), cst));
}

void verify_string_cst(tree s)
{
  assert(s != NULL_TREE);
  assert(TREE_CODE(s) == STRING_CST);

  int n = TREE_STRING_LENGTH(s);
  assert(n >= 0);               // ??? Is a 0-length string valid?

  char* p = TREE_STRING_POINTER(s);
  assert(p != 0);
}

// Verify that s is a STRING_CST where the string is null-terminated and
// contains no embedded nulls.
void verify_simple_string_cst(tree s)
{
  assert(s != NULL_TREE);
  assert(TREE_CODE(s) == STRING_CST);

  int n = TREE_STRING_LENGTH(s);
  assert(n > 0);                // length includes the terminating null.

  char* p = TREE_STRING_POINTER(s);
  assert(p != 0);

  assert(p[n-1] == '\0');       // Null termination
  assert(strlen(p) == n-1);     // No embedded nulls.
}

// Pointer-to-data-member constant.
void verify_ptrmem_cst(tree ptr)
{
  assert(ptr != NULL_TREE);
  assert(TREE_CODE(ptr) == PTRMEM_CST);

  // The declaration of the data member we're pointing to.
  tree field      = PTRMEM_CST_MEMBER(ptr);
  assert(field != NULL_TREE);
  assert(TREE_CODE(field) == FIELD_DECL);
  assert(TREE_CODE(DECL_CONTEXT(field)) == RECORD_TYPE ||
         TREE_CODE(DECL_CONTEXT(field)) == UNION_TYPE);

  // The type of that member in the declaration.
  tree field_type = TREE_TYPE(field);
  assert(field_type != NULL_TREE);
  assert(TREE_CODE_CLASS(TREE_CODE(field_type)) == 't');

  // Type is pointer-to-member.
  tree ptrmem_type = TREE_TYPE(ptr);
  assert(ptrmem_type != NULL_TREE);
  assert(TREE_CODE(ptrmem_type) == POINTER_TYPE);
  assert(TYPE_PTRMEM_P(ptrmem_type));

  // Type of the class that the point-to-member points into.
  tree class_type = PTRMEM_CST_CLASS(ptr);
  assert(class_type != NULL_TREE);
  assert(TREE_CODE(class_type) == RECORD_TYPE ||
         TREE_CODE(class_type) == UNION_TYPE);

  // Class type is correct.
  assert(same_type_p(class_type, TYPE_PTRMEM_CLASS_TYPE(ptrmem_type)));

  // Pointed-to type is correct.
  assert(same_type_p(field_type, TYPE_PTRMEM_POINTED_TO_TYPE(ptrmem_type)));
}

void verify_negate_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == NEGATE_EXPR);

  // Type of the expr
  tree expr_type = TREE_TYPE(expr);
  assert(is_numeric_scalar_type(expr_type));

  // The operand exists, and is a valid expression.
  tree operand = TREE_OPERAND(expr, 0);
  assert(operand != NULL_TREE);
  verify_expr(operand);

  // The expression and the operand have the same types.
  assert(same_type_p(expr_type, TREE_TYPE(operand)));
}

void verify_bit_not_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == BIT_NOT_EXPR);

  // Type of the expr
  tree expr_type = TREE_TYPE(expr);
  assert(is_integral_type(expr_type));

  // The operand exists, and is a valid expression.
  tree operand = TREE_OPERAND(expr, 0);
  assert(operand != NULL_TREE);
  verify_expr(operand);

  // The expression and the operand have the same types.
  assert(same_type_p(expr_type, TREE_TYPE(operand)));
}

void verify_truth_not_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == TRUTH_NOT_EXPR);

  // Type of the expr
  tree expr_type = TREE_TYPE(expr);
  assert(is_integral_type(expr_type));

  // The operand exists, and is a valid expression.
  tree operand = TREE_OPERAND(expr, 0);
  assert(operand != NULL_TREE);
  verify_expr(operand);
  assert(is_integral_type(TREE_TYPE(operand)));
}

void verify_incr_or_decr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == PREINCREMENT_EXPR  ||
         TREE_CODE(expr) == PREDECREMENT_EXPR  ||
         TREE_CODE(expr) == POSTINCREMENT_EXPR ||
         TREE_CODE(expr) == POSTDECREMENT_EXPR);

  // Type of the expr
  tree expr_type = TREE_TYPE(expr);
  assert(is_numeric_scalar_type(expr_type));

  // The operand exists, and is a valid expression.
  tree operand = TREE_OPERAND(expr, 0);
  assert(operand != NULL_TREE);
  verify_expr(operand);

  // The expression and the operand have the same types.
  assert(same_type_p(expr_type, TREE_TYPE(operand)));
}

void verify_addr_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == ADDR_EXPR);

  tree operand = TREE_OPERAND(expr, 0);
  assert(operand != NULL_TREE);

  tree type = TREE_TYPE(expr);
  assert(type != NULL_TREE);
  assert(TREE_CODE(type) == POINTER_TYPE ||
         TREE_CODE(type) == REFERENCE_TYPE);
  if (TREE_CODE(type) == POINTER_TYPE)
    assert(!TYPE_PTRMEM_P(type));
  tree pointed_to_type = TREE_TYPE(type);
  assert(pointed_to_type != NULL_TREE);

  // Gnu extension, address of label
  if (TREE_CODE(operand) == LABEL_DECL) {
    verify_label_decl(operand);

    // The type of address-of-label is void*.
    assert(TREE_CODE(type) == POINTER_TYPE);
    assert(TREE_CODE(pointed_to_type) == VOID_TYPE);
  }

  // Address of a function
  else if (TREE_CODE(operand) == FUNCTION_DECL) {
    verify_function(operand);

    assert(TREE_CODE(type) == POINTER_TYPE);
    assert(TREE_CODE(pointed_to_type) == FUNCTION_TYPE);
    assert(same_type_p(TYPE_MAIN_VARIANT(TREE_TYPE(operand)),
                       TYPE_MAIN_VARIANT(pointed_to_type)));
  }

  // Address of a VAR_DECL or an expression.
  else {
    verify_expr(operand);

    // Make sure types are consistent.
    tree operand_type = TREE_TYPE(operand);
    assert(operand_type != NULL_TREE);
    assert(same_type_p(operand_type, pointed_to_type));
  }
}

// Dereference of a pointer.
void verify_indirect_ref(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == INDIRECT_REF);

  tree type = TREE_TYPE(expr);
  assert(type != NULL_TREE);

  tree operand = TREE_OPERAND(expr, 0);
  assert(operand != NULL_TREE);
  verify_expr(operand);

  // Verify that types are consistent.
  tree operand_type = TREE_TYPE(operand);
  assert(TREE_CODE(operand_type) == POINTER_TYPE ||
         TREE_CODE(operand_type) == REFERENCE_TYPE);

  tree pointed_to_type = TREE_TYPE(operand_type);
  assert(pointed_to_type != NULL_TREE);
  assert(same_type_p(type, pointed_to_type));
}

// Conversion of a floating-point value to an integer value.
void verify_trunc_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == FIX_TRUNC_EXPR);

  tree operand = TREE_OPERAND(expr, 0);
  assert(operand != NULL_TREE);
  verify_expr(operand);

  assert(TREE_CODE(TREE_TYPE(operand)) == REAL_TYPE);
  assert(is_integral_type(TREE_TYPE(expr)));
}

// Conversion of an integer type to a floating-point type.
void verify_float_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == FLOAT_EXPR);

  tree operand = TREE_OPERAND(expr, 0);
  assert(operand != NULL_TREE);
  verify_expr(operand);

  assert(is_integral_type(TREE_TYPE(operand)));
  assert(TREE_CODE(TREE_TYPE(expr)) == REAL_TYPE);
}

// Conversions that don't require any code to be generated.
void verify_nop_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == NOP_EXPR || TREE_CODE(expr) == NON_LVALUE_EXPR);

  tree operand = TREE_OPERAND(expr, 0);
  assert(operand != NULL_TREE);
  verify_expr(operand);

  // ??? We should do some checks on the types here.  What kind?  Is it
  // always conversion of one pointer to another, or are there other kinds
  // of nop conversions?
}

void verify_convert_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == CONVERT_EXPR);

  tree operand = TREE_OPERAND(expr, 0);
  assert(operand != NULL_TREE);
  verify_expr(operand);

  // ??? We should do some checks on the types here, too.  Again, what
  // kinds of argument and result types are valid here?  int* to int,
  // presumably conversion of one integer type to another; what else?
}

void verify_throw_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == THROW_EXPR);

  tree operand = TREE_OPERAND(expr, 0);

  // Operand may be null, which means an unadorned "throw;".
  if (operand != NULL_TREE) {
    verify_expr(operand);
  }

  // Make sure we're in a function.
  assert(ancestor_is_function(expr));
}

// <<, >>
void verify_shift_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == LSHIFT_EXPR || TREE_CODE(expr) == RSHIFT_EXPR);

  tree lhs = TREE_OPERAND(expr, 0);
  tree rhs = TREE_OPERAND(expr, 1);

  assert(lhs != NULL_TREE);
  assert(rhs != NULL_TREE);
  verify_expr(lhs);
  verify_expr(rhs);

  assert(is_integral_type(TREE_TYPE(lhs)));
  assert(is_integral_type(TREE_TYPE(rhs)));
  assert(same_type_p(TREE_TYPE(expr), TREE_TYPE(lhs)));
}

// |, ^, &
void verify_bitwise_binary_op(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == BIT_IOR_EXPR ||
         TREE_CODE(expr) == BIT_XOR_EXPR ||
         TREE_CODE(expr) == BIT_AND_EXPR);

  tree lhs = TREE_OPERAND(expr, 0);
  tree rhs = TREE_OPERAND(expr, 1);

  assert(lhs != NULL_TREE);
  assert(rhs != NULL_TREE);
  verify_expr(lhs);
  verify_expr(rhs);

  assert(is_integral_type(TREE_TYPE(lhs)));
  assert(is_integral_type(TREE_TYPE(rhs)));
  assert(same_type_p(TREE_TYPE(lhs), TREE_TYPE(rhs)));
  assert(same_type_p(TREE_TYPE(expr), TREE_TYPE(lhs)));
}

// &&, ||
void verify_logical_binary_op(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == TRUTH_ANDIF_EXPR ||
         TREE_CODE(expr) == TRUTH_ORIF_EXPR);

  tree lhs = TREE_OPERAND(expr, 0);
  tree rhs = TREE_OPERAND(expr, 1);

  assert(lhs != NULL_TREE);
  assert(rhs != NULL_TREE);
  verify_expr(lhs);
  verify_expr(rhs);

  assert(is_integral_type(TREE_TYPE(lhs)));
  assert(is_integral_type(TREE_TYPE(rhs)));
  assert(is_integral_type(TREE_TYPE(expr)));
}

// +, -, *, /
void verify_arithmetic_binary_op(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == PLUS_EXPR ||
         TREE_CODE(expr) == MINUS_EXPR ||
         TREE_CODE(expr) == MULT_EXPR ||
         TREE_CODE(expr) == TRUNC_DIV_EXPR ||
         TREE_CODE(expr) == TRUNC_MOD_EXPR ||
         TREE_CODE(expr) == RDIV_EXPR);

  tree lhs = TREE_OPERAND(expr, 0);
  tree rhs = TREE_OPERAND(expr, 1);

  assert(lhs != NULL_TREE);
  assert(rhs != NULL_TREE);
  verify_expr(lhs);
  verify_expr(rhs);

  // Operands and result are of numeric types.
  assert(is_numeric_scalar_type(TREE_TYPE(lhs)));
  assert(is_numeric_scalar_type(TREE_TYPE(rhs)));
  assert(is_numeric_scalar_type(TREE_TYPE(expr)));

  // Operands and result have the same type.
  assert(same_type_p(TREE_TYPE(lhs), TREE_TYPE(rhs)));
  assert(same_type_p(TREE_TYPE(lhs), TREE_TYPE(expr)));

  // For integer division, it's an integral type.
  if (TREE_CODE(expr) == TRUNC_DIV_EXPR ||
      TREE_CODE(expr) == TRUNC_MOD_EXPR) {
    assert(is_integral_type(TREE_TYPE(lhs)));
  }

  // For floating-point division, it's a floating-point type.
  else if (TREE_CODE(expr) == RDIV_EXPR) {
    assert(TREE_CODE(TREE_TYPE(lhs)) == REAL_TYPE);
  }
}

  // <. <=, >, >=, ==, !=
void verify_comparison(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == LT_EXPR ||
         TREE_CODE(expr) == LE_EXPR ||
         TREE_CODE(expr) == GT_EXPR ||
         TREE_CODE(expr) == GE_EXPR ||
         TREE_CODE(expr) == EQ_EXPR ||
         TREE_CODE(expr) == NE_EXPR);

  tree lhs = TREE_OPERAND(expr, 0);
  tree rhs = TREE_OPERAND(expr, 1);

  assert(lhs != NULL_TREE);
  assert(rhs != NULL_TREE);
  verify_expr(lhs);
  verify_expr(rhs);

  // The left and right operands both have numeric types.
  // ??? What about pointers?
  assert(is_numeric_scalar_type(TREE_TYPE(lhs)));
  assert(is_numeric_scalar_type(TREE_TYPE(rhs)));

  // The left and right operands have the same type.
  assert(same_type_p(TREE_TYPE(lhs), TREE_TYPE(rhs)));

  // The result is an integral type.
  assert(is_integral_type(TREE_TYPE(expr)));
}


// =
void verify_modify_expr(tree expr) 
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == MODIFY_EXPR);

  tree lhs = TREE_OPERAND(expr, 0);
  tree rhs = TREE_OPERAND(expr, 1);

  assert(lhs != NULL_TREE);
  assert(rhs != NULL_TREE);
  verify_expr(lhs);
  verify_expr(rhs);

  // ??? lhs must be an lvalue.  How do we check for that?  Can we tell
  // from the type?

  assert(same_type_p(TYPE_MAIN_VARIANT(TREE_TYPE(lhs)),
                     TYPE_MAIN_VARIANT(TREE_TYPE(rhs))));
}

// Data member access
void verify_component_ref(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == COMPONENT_REF);

  tree lhs = TREE_OPERAND(expr, 0);
  tree rhs = TREE_OPERAND(expr, 1);

  assert(lhs != NULL_TREE);
  assert(rhs != NULL_TREE);

  // Left-hand side is an expression that evaluates to a class.
  verify_expr(lhs);
  tree lhs_type = TREE_TYPE(lhs);
  assert(TREE_CODE(lhs_type) == RECORD_TYPE ||
         TREE_CODE(lhs_type) == UNION_TYPE);

  // Right-hand side is a field decl for a data member.
  verify_field_decl(rhs);

  // ??? How do we verify that it's a field reference that's appropriate
  // for the class we've got on the lhs?  We can't just walk the list of
  // lhs_type's field decls, because it might come from a base class.
}

// Comma operator
void verify_compound_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == COMPOUND_EXPR);

  tree lhs = TREE_OPERAND(expr, 0);
  tree rhs = TREE_OPERAND(expr, 1);

  assert(lhs != NULL_TREE);
  assert(rhs != NULL_TREE);

  // lhs is an expression that gets thrown away.
  verify_expr(lhs);

  // rhs is an expression whose value gets used.
  verify_expr(rhs);
  assert(same_type_p(TREE_TYPE(expr), TREE_TYPE(rhs)));
}

// ?: operator
void verify_cond_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == COND_EXPR);

  tree cond     = TREE_OPERAND(expr, 0);
  tree if_true  = TREE_OPERAND(expr, 1);
  tree if_false = TREE_OPERAND(expr, 2);

  assert(cond != NULL_TREE);
  assert(if_true != NULL_TREE);
  assert(if_false != NULL_TREE);
  
  // Conditional is an expression of boolean or integer type.
  verify_expr(cond);
  assert(is_integral_type(TREE_TYPE(cond)));

  verify_expr(if_true);
  verify_expr(if_false);

  // ??? The C++ standard (5.16/3) describes conversions by which the
  // second and third arguments of the ternary operator are converted
  // to a common type.  I assume that these conversions have been 
  // performed by the time we see this expression.
  assert(same_type_p(TREE_TYPE(if_true), TREE_TYPE(if_false)));
  assert(same_type_p(TREE_TYPE(if_true), TREE_TYPE(expr)));
}

// Function invocation.  (Including member function invocation.)
void verify_call_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == CALL_EXPR);

  tree fptr = TREE_OPERAND(expr, 0);
  tree args = TREE_OPERAND(expr, 1);

  assert(fptr != NULL_TREE);
  assert(args != NULL_TREE);

  // The first operand is an expression that evaluates to a function pointer.
  verify_expr(fptr);
  assert(TREE_CODE(TREE_TYPE(fptr)) == POINTER_TYPE);

  tree fn_type = TREE_TYPE(TREE_TYPE(fptr));
  assert(fn_type != NULL_TREE);
  assert(TREE_CODE(fn_type) == FUNCTION_TYPE ||
         TREE_CODE(fn_type) == METHOD_TYPE);

  // The second operand is a list of arguments.
  for (tree cur = args ; cur != NULL_TREE ; cur = TREE_CHAIN(cur)) {
    assert(TREE_CODE(cur) == TREE_LIST);
    tree arg = TREE_VALUE(cur);
    verify_expr(arg);
  }

  // ??? How do we verify that argument list and parameter list match?
}

// Statement expression extension
void verify_bind_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == BIND_EXPR);

  // ??? What checks should we perform?
}

// Brace initialization of an array or a class aggregate.
void verify_constructor_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == CONSTRUCTOR);

  tree type = TREE_TYPE(expr);
  assert(type != NULL_TREE);
  verify_type(type);

  // Not a typo.  operand 0 is reserved for the back end.
  tree initializers = TREE_OPERAND(expr, 1);

  assert(TREE_CODE(type) == RECORD_TYPE || TREE_CODE(type) == UNION_TYPE ||
         TREE_CODE(type) == ARRAY_TYPE);

  // Initialization of an array.
  if (TREE_CODE(type) == ARRAY_TYPE) {
    tree element_type = TYPE_MAIN_VARIANT(TREE_TYPE(type));

    // Keep a count of the number of initalizers, so we can verify that it
    // not greater than the number of elements in the array.
    size_t num_initializers = 0;
    tree cur = initializers;
    while (cur != NULL_TREE) {
      assert(TREE_CODE(cur) == TREE_LIST);

      tree index = TREE_PURPOSE(cur);
      tree value = TREE_VALUE(cur);

      // gcc has an extension allowing scattershot array initialization,
      // but g++ does not.  The index should always be null, meaning "next
      // available index".
      assert(index == NULL_TREE);

      // The value is an expression giving the initial value.
      assert(value != NULL_TREE);
      verify_expr(value);
      assert(same_type_p(TYPE_MAIN_VARIANT(TREE_TYPE(value)), element_type));

      cur = TREE_CHAIN(cur);
      ++num_initializers;
    }

    // Number of initializers <= max_index + 1.
    tree bounds = TYPE_DOMAIN(type);
    assert(bounds != NULL_TREE);
    assert(TREE_CODE(bounds) == INTEGER_TYPE);
    tree max_index = TYPE_MAX_VALUE(bounds);
    assert(max_index != NULL_TREE);
    assert(TREE_CODE(max_index) == INTEGER_CST);
    assert(num_initializers == 0 ||
           integer_cst_le(num_initializers - 1, max_index));
  }

  // Initialization of a class aggregate.
  else {
    for (tree cur = initializers ; cur != NULL_TREE ; cur = TREE_CHAIN(cur)) {
      assert(TREE_CODE(cur) == TREE_LIST);

      tree field = TREE_PURPOSE(cur);
      tree value = TREE_VALUE(cur);
      assert(field != NULL_TREE);
      assert(value != NULL_TREE);

      // This is actually a field of the class we're initializing.
      // (And yes, we're using a quadratic algorithm to check this.  Sigh)
      tree class_fields = TYPE_FIELDS(type);
      while (class_fields != NULL_TREE && class_fields != field)
        class_fields = TREE_CHAIN(class_fields);
      assert(class_fields == field);

      // The initializer is a valid expression.
      verify_expr(value);

      // The types of the field and the initializer are consistent.
      assert(same_type_p(TYPE_MAIN_VARIANT(TREE_TYPE(field)),
                         TYPE_MAIN_VARIANT(TREE_TYPE(value))));
    }
  }
}

void verify_init_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == INIT_EXPR);

  // ??? What checks should we perform?
}

void verify_save_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == SAVE_EXPR);

  // ??? What checks should we perform?
}

// Represents a temporary variable.  Operand 0 is a var_decl,
// operand 1 is an initializer.
void verify_target_expr(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == TARGET_EXPR);

  tree var = TREE_OPERAND(expr, 0);
  tree ini = TREE_OPERAND(expr, 1);

  // var is a valid var_decl, and ini is a valid expression.
  assert(var != NULL_TREE);
  assert(ini != NULL_TREE);
  assert(TREE_CODE(var) == VAR_DECL);
  verify_expr(var);

  // Second argument may either be an AGGR_INIT_EXPR or an ordinary expr.
  // An AGGR_INIT_EXPR may only occur in a TARGET_EXPR, so we do the check
  // here instead of in verify_expr.
  if (TREE_CODE(ini) == AGGR_INIT_EXPR) {
    tree fptr = TREE_OPERAND(ini, 0);
    tree args = TREE_OPERAND(ini, 1); // List of arguments.

    // fptr is an expression that evaluates to pointer-to-function.
    assert(fptr != NULL_TREE);
    verify_expr(fptr);
    assert(TREE_CODE(TREE_TYPE(fptr)) == POINTER_TYPE);

    tree fn_type = TREE_TYPE(TREE_TYPE(fptr));
    assert(fn_type != NULL_TREE);
    assert(TREE_CODE(fn_type) == FUNCTION_TYPE ||
           TREE_CODE(fn_type) == METHOD_TYPE);

    // Function return type is the same as the AGGR_INIT_EXPR's type.
    assert(same_type_p(TREE_TYPE(fn_type), TREE_TYPE(ini)));

    // Verify the list of arguments.
    // ??? How do we verify that the argument and parameter lists match?
    for (tree cur = args ; cur != NULL_TREE ; cur = TREE_CHAIN(cur)) {
      assert(TREE_CODE(cur) == TREE_LIST);
      tree arg = TREE_VALUE(cur);
      verify_expr(arg);
    }

    if (AGGR_INIT_VIA_CTOR_P(ini)) { // It's a constructor invocation.
      // ??? What does this mean?
      // If @code{AGGR_INIT_VIA_CTOR_P} holds of the
      // @code{AGGR_INIT_EXPR}, then the intialization is via a
      // constructor call.  The address of the first operand of the
      // @code{AGGR_INIT_EXPR}, which is always a @code{VAR_DECL}, is
      // taken, and this value replaces the first argument in the
      // argument list.  In this case, the value of the expression is
      // the @code{VAR_DECL} given by the first operand to the
      // @code{AGGR_INIT_EXPR}; constructors do not return a value.      
    }
    
  }
  else                          // An ordinary expr, not an AGGR_INIT_EXPR.
    verify_expr(ini);

  // var was introduced by the compiler.
  assert(DECL_ARTIFICIAL(var));

  // var and ini have the same type.
  assert(same_type_p(TYPE_MAIN_VARIANT(TREE_TYPE(var)),
                     TYPE_MAIN_VARIANT(TREE_TYPE(ini))));
}

void verify_array_ref(tree expr)
{
  assert(expr != NULL_TREE);
  assert(TREE_CODE(expr) == ARRAY_REF);

  tree array = TREE_OPERAND(expr, 0);
  tree index = TREE_OPERAND(expr, 1);
  assert(array != NULL_TREE);
  assert(index != NULL_TREE);
  
  verify_expr(array);
  verify_expr(index);

  assert(TREE_CODE(TREE_TYPE(array)) == ARRAY_TYPE);
  assert(TREE_CODE(TREE_TYPE(index)) == INTEGER_TYPE);

  // ??? What should the type of an ARRAY_REF be?  The same as the array's
  // element type, or a reference to the array's element type?
}

// Is this tree code something that can appear as an expr? This function 
// is slightly more general than IS_EXPR_CODE_CLASS, since IS_EXPR_CODE_CLASS
// doesn't include VAR_DECLs or constants.
bool is_expr_code(tree_code code)
{
  char cls = TREE_CODE_CLASS(code);
  return code == VAR_DECL || code == PARM_DECL ||
         cls == 'c' || cls == '<' || cls == '1' || cls == '2' || cls == 'e' ||
         cls == 'r';
}

//------------------------------------------------------------
// Verification of types

void verify_type(tree type)
{
  // Class is correct.
  assert(type != NULL_TREE);
  const tree_code code = TREE_CODE(type);
  assert(TREE_CODE_CLASS(code) == 't');

  // If it isn't cv-qualified, the type is the same as its main
  // variant.
  if (!CP_TYPE_CONST_P(type) &&
      !CP_TYPE_VOLATILE_P(type) &&
      !CP_TYPE_RESTRICT_P(type))
    assert(same_type_p(TYPE_MAIN_VARIANT(type), type));

  // Unless it's an incomplete type, it has a positive size and
  // a positive alignment that's a multiple of the byte size.
  tree size = TYPE_SIZE(type);

  // It's an incomplete type
  if (size == NULL_TREE) {
    // ??? Are arrays whose bounds aren't given considered to be incomplete
    // types?
    assert(code == ARRAY_TYPE || code == RECORD_TYPE || code == UNION_TYPE
                              || code == VOID_TYPE);
  }

  else if (code != VOID_TYPE && code != LANG_TYPE) {
    verify_integer_cst(size);   
    assert(tree_int_cst_lt(integer_zero_node, size));
    assert(TYPE_ALIGN(type) > 0);
    assert((TYPE_ALIGN(type) % CHAR_BIT) == 0);
  }

  switch(code) {
  case VOID_TYPE:
    assert(same_type_p(TYPE_MAIN_VARIANT(type), void_type_node));

    // void has size 0, alignment 8, and the name "void".
    assert(TYPE_ALIGN(type) == 8);
    assert(tree_int_cst_equal(integer_zero_node, size));
    verify_type_name_is(type, "void");

    break;
    
  case INTEGER_TYPE:
    verify_integer_type(type);
    break;
    
  case REAL_TYPE:
    verify_real_type(type);
    break;
    
  case COMPLEX_TYPE:
    verify_complex_type(type);
    break;
    
  case ENUMERAL_TYPE:
    verify_enumeral_type(type);
    break;
    
  case BOOLEAN_TYPE:
    verify_named_type(type);
    break;
    
  case POINTER_TYPE:
    verify_pointer_type(type);
    break;
    
  case REFERENCE_TYPE:
    verify_reference_type(type);
    break;
    
  case FUNCTION_TYPE:
    verify_function_type(type);
    break;
    
  case METHOD_TYPE:
    verify_method_type(type);
    break;
    
  case ARRAY_TYPE:
    verify_array_type(type);
    break;
    
  case RECORD_TYPE:
  case UNION_TYPE:
    verify_class_type(type);
    break;

  case LANG_TYPE:
    // ??? What checks should we do here?
    break;
    
  default:
    fprintf(stderr, "Unknown tree code for type, %d\n", (int) code);
    assert(0);
  }

}

void verify_integer_type(tree type)
{
  assert(type != NULL_TREE);
  assert(TREE_CODE(type) == INTEGER_TYPE);

  // ??? Some integer types have no name.  Is this a bug? Is there a
  // way to characterize which ones should have names, so we can call
  // verify_named_type() where appropriate?

  // The precision is consistent with the size.
  tree size = TYPE_SIZE(type);
  verify_integer_cst(size);
  assert(tree_int_cst_lt(integer_zero_node, size));
  assert(TREE_INT_CST_HIGH(size) == 0);
  assert(TYPE_PRECISION(type) <= TREE_INT_CST_LOW(size));

  // The minimum and maximum are reasonable.
  tree min_value = TYPE_MIN_VALUE(type);
  tree max_value = TYPE_MAX_VALUE(type);
  verify_integer_cst(min_value);
  verify_integer_cst(max_value);

  assert(tree_int_cst_lt(min_value, max_value));

  if (TREE_UNSIGNED(type))
    assert(tree_int_cst_equal(min_value, integer_zero_node));
  else
    assert(tree_int_cst_lt(min_value, integer_zero_node));
}

void verify_real_type(tree type)
{
  assert(type != NULL_TREE);
  assert(TREE_CODE(type) == REAL_TYPE);

  // All floating-piont types are builtin, so the type has a name.
  verify_named_type(type);

  // The precision is consistent with the size.
  tree size = TYPE_SIZE(type);
  verify_integer_cst(size);
  assert(tree_int_cst_lt(integer_zero_node, size));
  assert(TREE_INT_CST_HIGH(size) == 0);
  assert(TYPE_PRECISION(type) <= TREE_INT_CST_LOW(size));

}

void verify_complex_type(tree type)
{
  assert(type != NULL_TREE);
  assert(TREE_CODE(type) == COMPLEX_TYPE);

  // ??? What checks should we perform here?
}

void verify_enumeral_type(tree type)
{
  assert(type != NULL_TREE);
  assert(TREE_CODE(type) == ENUMERAL_TYPE);

  // An enumeral type may or may not have a name.
  if (TYPE_NAME(type) != NULL_TREE)
    verify_named_type(type);

  // The precision is consistent with the size.
  tree size = TYPE_SIZE(type);
  verify_integer_cst(size);
  assert(tree_int_cst_lt(integer_zero_node, size));
  assert(TREE_INT_CST_HIGH(size) == 0);
  assert(TYPE_PRECISION(type) <= TREE_INT_CST_LOW(size));

  // The minimum and maximum are reasonable.
  tree min_value = TYPE_MIN_VALUE(type);
  tree max_value = TYPE_MAX_VALUE(type);
  verify_integer_cst(min_value);
  verify_integer_cst(max_value);

  assert(tree_int_cst_lt(min_value, max_value));

  if (TREE_UNSIGNED(type))
    assert(!tree_int_cst_lt(min_value, integer_zero_node));  
  else
    assert(tree_int_cst_lt(min_value, integer_zero_node));  

  // Each of the enum constants has a name and value, and has a type that's
  // identical to this type.  All of the values are within the range
  // [min_value, max_value], and both the min and the max value appear in 
  // the list.
  bool min_seen = false;
  bool max_seen = false;

  for (tree cur = TYPE_VALUES(type); cur != NULL_TREE; cur = TREE_CHAIN(cur)) {
    assert(TREE_CODE(cur) == TREE_LIST);

    tree name  = TREE_PURPOSE(cur);
    tree value = TREE_VALUE(cur);

    verify_ordinary_identifier(name);
    verify_integer_cst(value);

    assert(same_type_p(TREE_TYPE(value), type));

    assert(!tree_int_cst_lt(value, min_value));
    assert(!tree_int_cst_lt(max_value, value));

    min_seen = min_seen || tree_int_cst_equal(value, min_value);
    max_seen = min_seen || tree_int_cst_equal(value, max_value);
  }

  assert(min_seen);
  assert(max_seen);
}

void verify_pointer_type(tree type)
{
  assert(type != NULL_TREE);
  assert(TREE_CODE(type) == POINTER_TYPE);

  // Pointer to member, a type of the form T X::*
  if (TYPE_PTRMEM_P(type)) {
    tree T = TYPE_PTRMEM_POINTED_TO_TYPE(type);
    tree X = TYPE_PTRMEM_CLASS_TYPE(type);

    assert(T != NULL_TREE);
    assert(X != NULL_TREE);
    assert(TREE_CODE(X) == RECORD_TYPE || TREE_CODE(X) == UNION_TYPE);
    verify_type(T);
    verify_type(X);
  }

  // Ordinary pointer
  else {
    tree pointed_to = TREE_TYPE(type);
    assert(pointed_to != NULL_TREE);
    verify_type(pointed_to);
  }
}

void verify_reference_type(tree type)
{
  assert(type != NULL_TREE);
  assert(TREE_CODE(type) == REFERENCE_TYPE);

  tree pointed_to = TREE_TYPE(type);
  assert(pointed_to != NULL_TREE);
  verify_type(pointed_to);
}

void verify_function_type(tree type)
{
  assert(type != NULL_TREE);
  assert(TREE_CODE(type) == FUNCTION_TYPE);

  tree return_type = TREE_TYPE(type);
  assert(return_type != NULL_TREE);
  verify_type(return_type);

  verify_function_arguments(TYPE_ARG_TYPES(type));
}

void verify_method_type(tree type)
{
  assert(type != NULL_TREE);
  assert(TREE_CODE(type) == METHOD_TYPE);

  // Class that the member function is defined in.
  tree enclosing_class = TYPE_METHOD_BASETYPE(type);
  assert(enclosing_class != NULL_TREE);
  assert(TREE_CODE(enclosing_class) == RECORD_TYPE ||
         TREE_CODE(enclosing_class) == UNION_TYPE);

  // Verify that the class really does have a member function with this type.
  tree methods = TYPE_METHODS(enclosing_class);
  while (methods != NULL_TREE) {
    assert(TREE_CODE(methods) == FUNCTION_DECL);
    assert(DECL_FUNCTION_MEMBER_P(methods));
    if (same_type_p(type, TREE_TYPE(methods)))
      break;

    methods = TREE_CHAIN(methods);
  }
  assert(methods != NULL_TREE);

  verify_function_arguments(TYPE_ARG_TYPES(type));
}

// Helper function for verify_function_type and verify_method_type.
void verify_function_arguments(tree args)
{
  while (args != NULL_TREE) {
    assert(TREE_CODE(args) == TREE_LIST);
    tree arg_type    = TREE_VALUE(args);
    tree default_arg = TREE_PURPOSE(args);
    tree next        = TREE_CHAIN(args);

    assert(arg_type != NULL_TREE);
    verify_type(arg_type);

    // A function need not have any default arguments, of course
    if (default_arg != NULL_TREE) {
      verify_expr(default_arg);
      assert(same_type_p(TREE_TYPE(default_arg), arg_type));
    }

    // The last list node, and only the last, may have an argument whose
    // type is void.  This signifies that it's a vararg function.
    if (same_type_p(arg_type, void_type_node))
      assert(next == NULL_TREE);

    args = next;
  }
}


void verify_array_type(tree type)
{
  assert(type != NULL_TREE);
  assert(TREE_CODE(type) == ARRAY_TYPE);

  // An array type has no name.
  assert(TYPE_NAME(type) == NULL_TREE);

  // It's an array of some valid, complete type.
  tree element_type = TREE_TYPE(type);
  assert(element_type != NULL_TREE);
  verify_type(element_type);
  assert(TYPE_SIZE(element_type) != NULL_TREE);

  // ??? Is it true that the array has bounds if and only if it is
  // a complete type?

  // It's an incomplete type.
  if (TYPE_SIZE(type) == NULL_TREE) {
    assert(TYPE_DOMAIN(type) == NULL_TREE);
  }

  // It's a complete type
  else {
    tree bounds = TYPE_DOMAIN(type);
    assert(bounds != NULL_TREE);
    assert(TREE_CODE(bounds) == INTEGER_TYPE);

    // The bounds are sane.
    tree min_value = TYPE_MIN_VALUE(bounds);
    tree max_value = TYPE_MAX_VALUE(bounds);

    assert(min_value != NULL_TREE);
    assert(max_value != NULL_TREE);
    assert(TREE_CODE(min_value) == INTEGER_CST);
    assert(TREE_CODE(max_value) == INTEGER_CST);

    assert(tree_int_cst_equal(min_value, integer_zero_node));
    assert(!tree_int_cst_lt(max_value, min_value));

    // the size of the array is equal to the element size times
    // the number of elements.  For the moment we'll do the check 
    // just for the easy case.
    // ??? Are there functions for INTEGER_CST arithmetic?  
    if (TREE_INT_CST_HIGH(TYPE_SIZE(type)) == 0) {
      assert(TREE_INT_CST_HIGH(TYPE_SIZE(element_type)) == 0);
      assert(TREE_INT_CST_HIGH(max_value) == 0);

      HOST_WIDE_INT n = TREE_INT_CST_LOW(max_value) + 1;
      HOST_WIDE_INT esize = TREE_INT_CST_HIGH(TYPE_SIZE(element_type));
      HOST_WIDE_INT asize = TREE_INT_CST_HIGH(TYPE_SIZE(type));
      assert(asize == esize * n);
    }
  }
}

void verify_class_type(tree type)
{
  assert(type != NULL_TREE);
  assert(TREE_CODE(type) == RECORD_TYPE || TREE_CODE(type) == UNION_TYPE);

  if (tree_node_already_seen(type))
    return;
  mark_tree_node_as_seen(type);

  // Pointers to member functions are represented as record types.
  if (TREE_CODE(type) == RECORD_TYPE && TYPE_PTRMEMFUNC_P(type)) {
    tree fptr_type = TYPE_PTRMEMFUNC_FN_TYPE(type);
    assert(fptr_type != NULL_TREE);
    assert(TREE_CODE(fptr_type) == POINTER_TYPE);

    tree pointed_to = TREE_TYPE(fptr_type);
    assert(pointed_to != NULL_TREE);
    assert(TREE_CODE(pointed_to) == METHOD_TYPE);
    verify_type(fptr_type);
  }

  // It's an ordinary class.
  else {
    
    // Check member functions.
    tree fn = TYPE_METHODS(type);
    while (fn != NULL_TREE) {
      assert(TREE_CODE(fn) == FUNCTION_DECL);

      // We haven't seen this class before, but we might have seen a 
      // qualified variant of it.
      tree fn_parent = get_parent_or_null(fn);
      if (fn_parent) {
        assert(TREE_CODE(fn_parent) == RECORD_TYPE ||
               TREE_CODE(fn_parent) == UNION_TYPE);
        assert(type != fn_parent);
        assert(tree_node_already_seen(fn_parent));
        assert(same_type_p(TYPE_MAIN_VARIANT(type),
                           TYPE_MAIN_VARIANT(fn_parent)));
      }
      else
        add_child_to_map(type, fn);

      verify_decl(fn, class_scope);
      fn = TREE_CHAIN(fn);
    }

    // Check other class members.
    tree mem = TYPE_FIELDS(type);
    while (mem) {
      // Only some declarations may appear as class members.
      const tree_code code = TREE_CODE(mem);
      assert(code == FIELD_DECL || // Non-static member variable
             code == VAR_DECL ||   // Static member variable
             code == TYPE_DECL ||  // Nested type
             code == CONST_DECL);  // Enumeration constant

      // Add it to the parent map.  Again, it might already be the child 
      // of a different variant of this type.
      tree mem_parent = get_parent_or_null(mem);
      if (mem_parent) {
        assert(TREE_CODE(mem_parent) == RECORD_TYPE ||
               TREE_CODE(mem_parent) == UNION_TYPE);
        assert(type != mem_parent);
        assert(tree_node_already_seen(mem_parent));
        assert(same_type_p(TYPE_MAIN_VARIANT(type),
                           TYPE_MAIN_VARIANT(mem_parent)));
      }
      else
        add_child_to_map(type, mem);

      // Verify that it's a valid declaration.
      verify_decl(mem, class_scope);

      // If it's an enumeration constant, make sure we've got an enum type.
      // ??? Can we assume that an enum type will appear before any of the
      // CONST_DECLs?  If so, we can simplify this step.
      if (code == CONST_DECL) {
        tree enum_type = TREE_TYPE(mem);
        tree cur = TYPE_FIELDS(type);
        while (cur) {
          if (TREE_CODE(cur) == TYPE_DECL);
          tree t1 = TREE_TYPE(cur);
          assert(t1 != NULL_TREE);
          if (TREE_CODE(t1) == ENUMERAL_TYPE && same_type_p(enum_type, t1))
            break;
          cur = TREE_CHAIN(cur);
        }
        assert(cur != NULL_TREE);
      }

      mem = TREE_CHAIN(mem);
    }

    // Check base types.
    tree binfo = TYPE_BINFO(type);
    assert(binfo != NULL_TREE);
    assert(TREE_CODE(binfo) == TREE_VEC); // Internal representation of binfo
    tree binfo_type = BINFO_TYPE(binfo);
    assert(binfo_type != NULL_TREE);
    assert(TREE_CODE(binfo_type) == RECORD_TYPE ||
           TREE_CODE(binfo_type) == UNION_TYPE);
    // ??? Do we really need this TYPE_MAIN_VARIANT protection?
    assert(same_type_p(TYPE_MAIN_VARIANT(type),
                       TYPE_MAIN_VARIANT(binfo_type)));

    tree basetypes = BINFO_BASETYPES(binfo);

    // basetypes is null if and only if there are no base classes.
    if (basetypes != NULL_TREE) {
      assert(TREE_CODE(basetypes) == TREE_VEC);

    // Loop through the vector of base class binfos.
      size_t n = TREE_VEC_LENGTH(basetypes);
      assert(n != 0);

      for(size_t i = 0; i < n; ++i) {
        tree base_binfo = TREE_VEC_ELT(basetypes, i);
        assert(base_binfo != NULL_TREE);
        assert(TREE_CODE(base_binfo) == TREE_VEC); // Internal repr. of binfo

        // It's either a public, private, or protected base class.
        int num_access_types = 0;
        num_access_types += TREE_VIA_PUBLIC(base_binfo)    ? 1 : 0;
        num_access_types += TREE_VIA_PROTECTED(base_binfo) ? 1 : 0;
        num_access_types += TREE_VIA_PRIVATE(base_binfo)   ? 1 : 0;
        assert(num_access_types == 1);

        // The base class is a valid class or union.
        tree base_class = BINFO_TYPE(base_binfo);
        assert(base_class != NULL_TREE);
        assert(TREE_CODE(base_class) == RECORD_TYPE ||
               TREE_CODE(base_class) == UNION_TYPE);
        if (TREE_CODE(base_class) == RECORD_TYPE)
          assert(!TYPE_PTRMEMFUNC_P(type));
        verify_type(base_class);
      }
    }
  }
}

// Verify that this type has a name.  (Not all types are supposed to.)
const char* verify_named_type(tree type)
{
  assert(type != NULL_TREE);
  assert(TREE_CODE_CLASS(TREE_CODE(type)) == 't');

  // TYPE_NAME returns a type_decl node, not an identifier node.
  tree decl = TYPE_NAME(type);
  assert(decl != NULL_TREE);
  assert(TREE_CODE(decl) == TYPE_DECL);

  tree id = DECL_NAME(decl);
  assert(id != NULL_TREE);
  return verify_ordinary_identifier(id);
}

// Verify that this type has a name, and that the name is s.
void verify_type_name_is(tree type, const char* s)
{
  assert(s != 0);
  size_t len = strlen(s);

  const char* name = verify_named_type(type);
  assert(strncmp(name, s, len) == 0);
  assert(name[len] == '\0');
}

//------------------------------------------------------------
// Implementation of parent map functions

struct void_ptr_hash {
  size_t operator()(void* p) const { return reinterpret_cast<size_t>(p); }
};


std::hash_map<tree, tree, void_ptr_hash> parent_map;
std::hash_set<tree, void_ptr_hash> seen_map;


// Verify that t has a parent, and return it.
tree get_parent(tree t)
{
  assert(t != NULL_TREE);
  assert(parent_map.find(t) != parent_map.end());

  tree result = parent_map[t];
  assert(result != NULL_TREE);
  return result;
}

// Return t's parent, if it exists, or a null pointer if it does not.
tree get_parent_or_null(tree t)
{
  assert(t != NULL_TREE);
  if (parent_map.find(t) == parent_map.end())
    return NULL_TREE;
  else {
    tree result = parent_map[t];
    assert(result != NULL_TREE);
    return result;
  }
}  

// Verify that t has no parent.
void verify_parentless(tree t)
{
  assert(t != NULL_TREE);
  assert(parent_map.find(t) == parent_map.end());
}

// First, verify that child doesn't already have a parent.  Then
// add it to the map, with the specified parent.  If it's a
// constant node then we never add it to the tree, because it
// may appear in many places.
void add_child_to_map(tree parent, tree child)
{
  assert(parent != NULL_TREE);
  assert(child != NULL_TREE);
  assert(parent_map.find(child) == parent_map.end());
  parent_map[child] = parent;
}

bool tree_node_already_seen(tree t)
{
  return seen_map.find(t) != seen_map.end();
}

void mark_tree_node_as_seen(tree t)
{
  seen_map.insert(t);
}


//------------------------------------------------------------
// Helper functions

// Verify that id is an ordinary identifier node
const char* verify_ordinary_identifier(tree id) {
  assert(id != NULL_TREE);

  // It's an identifier node
  assert(TREE_CODE(id) == IDENTIFIER_NODE);
  
  // It's not an overloaded operator
  assert(!IDENTIFIER_OPNAME_P(id));

  // It points to a string
  const char* s = IDENTIFIER_POINTER(id);
  assert(s != 0);

  // The string's length is represented consistently
  assert(IDENTIFIER_LENGTH(id) == strlen(s));

  return s;
}

// Verify that id is an identifier node, not necessarily an ordinary one.
// For overloaded operators, return value is (const char*) 0.  Otherwise 
// it's the name, represented as a null-terminated string.
const char* verify_identifier(tree id) {
  assert(id != NULL_TREE);

  // It's an identifier node
  assert(TREE_CODE(id) == IDENTIFIER_NODE);

  // Either it's an overloaded operator, or else it's a valid
  // ordinary identifier.
  return IDENTIFIER_OPNAME_P(id)
    ? (const char*) 0
    : verify_ordinary_identifier(id);
}


// Verify that t exists in function scope.  That is: t has a parent,
// and, as we ascend from parent to parent, we'll get to a function 
// scope before we get to either of the other high-level scoping 
// constructs.  Returns a pointer to that function node if it exists,
// otherwise a null pointer.
tree ancestor_is_function(tree t)
{
  tree cur = get_parent(t);
  while (cur != NULL_TREE) {
    const tree_code c = TREE_CODE(cur);
    if (c == FUNCTION_DECL)     // OK, an ancestor is a function.
      return cur;
    else if (c == NAMESPACE_DECL || c == RECORD_TYPE || c == UNION_TYPE)
      return NULL_TREE;

    cur = get_parent(cur);
  }

  return NULL_TREE;
}

// Verify that t is defined within a loop (do/while/for).  That is:
// as we ascend from parent to parent, we get to a loop before we
// get to a function, namespace, or class node.
bool ancestor_is_loop(tree t)
{
  tree cur = get_parent(t);
  while (cur != NULL_TREE) {
    const tree_code c = TREE_CODE(cur);
    if (c == DO_STMT || c == FOR_STMT || c == WHILE_STMT)
      return true;
    else if (c == NAMESPACE_DECL ||
             c == RECORD_TYPE || c == UNION_TYPE ||
             c == FUNCTION_DECL)      
      return false;

    cur = get_parent(cur);
  }

  return false;
}


// Verify that t is defined within a switch statement.  That is:
// as we ascend from parent to parent, we get to a switch before we
// get to a function, namespace, or class node.
bool ancestor_is_switch(tree t)
{
  tree cur = get_parent(t);
  while (cur != NULL_TREE) {
    const tree_code c = TREE_CODE(cur);
    if (c == SWITCH_STMT)
      return true;
    else if (c == NAMESPACE_DECL ||
             c == RECORD_TYPE || c == UNION_TYPE ||
             c == FUNCTION_DECL)      
      return false;

    cur = get_parent(cur);
  }

  return false;
}

bool is_integral_type(tree type)
{
  assert(type != NULL_TREE);
  const tree_code c = TREE_CODE(type);
  return c == INTEGER_TYPE || c == ENUMERAL_TYPE || c == BOOLEAN_TYPE;
}

bool is_numeric_scalar_type(tree type)
{
  assert(type != NULL_TREE);
  return is_integral_type(type) || TREE_CODE(type) == REAL_TYPE;
}
    
bool is_integral_constant_expression(tree expr)
{
  assert(expr != NULL_TREE);
  return is_integral_type(TREE_TYPE(expr)) && TREE_CONSTANT(expr);
}

// Conceptually, compute a % N.  We need to avoid overflow.
unsigned long integer_cst_remainder(tree a, unsigned long N)
{
  assert(a != NULL_TREE);
  assert(TREE_CODE(a) == INTEGER_CST);
  assert(tree_int_cst_sgn(a) >= 0);
  assert(N != 0);

  // a's value is (high * 2^HOST_BITS_PER_WIDE_INT) + low.
  // We will assume that HOST_BITS_PER_WIDE_INT is even, that
  // a long has at least (HOST_BITS_PER_WIDE_INT/2) bits, and 
  // that N is small enough so that N^2 won't overflow a long.
  assert(HOST_BITS_PER_WIDE_INT % 2 == 0);
  assert(N < (1 << (sizeof(long) * CHAR_BIT / 2)));

  const unsigned long rhalf = (1 << (HOST_BITS_PER_WIDE_INT/2)) % N;

  unsigned long rhigh = TREE_INT_CST_HIGH(a) % N;
  unsigned long rlow  = TREE_INT_CST_LOW(a)  % N;

  unsigned long result = (rhalf * rhalf) % N;
  result = (result * rhigh) % N;
  result = (result + rlow) % N;

  return result;
}

// Tests x <= y, where x is an integer and y is an INTEGER_CST.
bool integer_cst_le(size_t x, tree y)
{
  assert(y != NULL_TREE);
  assert(TREE_CODE(y) == INTEGER_CST);
  assert(!tree_int_cst_lt(y, integer_zero_node));

  HOST_WIDE_INT x_low;
  HOST_WIDE_INT x_high;

  if (sizeof(HOST_WIDE_INT) >= sizeof(size_t)) {
    x_low  = x;
    x_high = 0;
  }

  else {
    x_low  = (HOST_WIDE_INT) x;
    x_high = (HOST_WIDE_INT)
             (x >> CHAR_BIT * (sizeof(size_t) - sizeof(HOST_WIDE_INT)));
  }

  return x_high <= TREE_INT_CST_HIGH(y) &&
         x_low  <= TREE_INT_CST_LOW(y);
}

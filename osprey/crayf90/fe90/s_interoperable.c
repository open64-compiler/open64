/*
 * Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
 */

/*
 * Semantic checking related to F2003 [proc-]language-binding-spec
 */

#include "defines.h"
#include "host.m"
#include "host.h"
#include "target.m"
#include "globals.m"
#include "globals.h"
#include "sytb.m"
#include "tokens.h"
#include "sytb.h"
#include "s_globals.h"

#if defined(TARG_X8664) || defined(TARG_IA32) || defined(TARG_MIPS) || defined(TARG_IA64) || defined(TARG_LOONGSON)
/* True if this type is interoperable with C. */
static unsigned char interoperable_types[] = {
  0,	/* Type_Void */
  0,	/* Short_Char_Const */
  0,	/* Short_Typeless_Const */
  0,	/* Typeless_1 */
  0,	/* Typeless_2 */
  0,	/* Typeless_4 */
  0,	/* Typeless_8 */
  0,	/* Long_Typeless */
  1,	/* Integer_1 */
  1,	/* Integer_2 */
  1,	/* Integer_4 */
  1,	/* Integer_8 */
  1,	/* Real_4 */
  1,	/* Real_8 */
  0,	/* Real_16 */
  1,	/* Complex_4 */
  1,	/* Complex_8 */
  0,	/* Complex_16 */
  0,	/* CRI_Ptr_8 */
  1,	/* Logical_1 */
  0,	/* Logical_2 */
  0,	/* Logical_4 */
  0,	/* Logical_8 */
  0,	/* Character_1 *//* Not interoperable unless len=1 */
  0,	/* Character_2 */
  0,	/* Character_4 */
  0,	/* CRI_Ch_Ptr_8 */
  0,	/* Structure_Type */
  0	/* CRI_Parcel_Ptr_8 */
  };
#else
# error "Define interoperable_types for target architecture"
#endif /* defined(TARG_X8664) || defined(TARG_IA32) || defined(TARG_MIPS) || defined(TARG_IA64)*/

static void check_interoperable_data(int);
static void check_interoperable_pgm_unit(int);
static void check_interoperable_derived_type(int);

/*
 * Report error if type of attr_idx is not interoperable (regardless of
 * whether attr_idx itself is interoperable.) For example, a variable with
 * type integer(c_int) has interoperable type even if the variable itself
 * lacks "bind(c)"; a variable of type integer has interoperable type, but
 * if it also has the ALLOCATABLE attribute then the variable is not
 * itself interoperable. This function cares only about the type.
 *
 * attr_idx	AT_Tbl_Idx for a data object (not its type)
 * quiet	If true, suppress error message
 * ck_arrayness	If true, check constraints on array characteristics and on
 *		character length parameter
 * returns	TRUE if type of data object is interoperable
 */
boolean
check_interoperable_type(int attr_idx, boolean quiet, boolean ck_arrayness) {
#ifdef _DEBUG
  static int checked = 0;
  if (!checked) {
    checked = 1;
    if ((sizeof interoperable_types) != Num_Linear_Types) {
      if (!quiet) {
	PRINTMSG(1, 1044, Internal, 1,
	  "interoperable_types table out of sync with linear types");
      }
      return FALSE;
    }
  }
#endif /* _DEBUG */

  int atd_array_idx = ATD_ARRAY_IDX(attr_idx);
  if (ck_arrayness && atd_array_idx != NULL_IDX) {
    bd_array_type bd_array_class = BD_ARRAY_CLASS(atd_array_idx);
    if (bd_array_class != Explicit_Shape && bd_array_class != Assumed_Size) {
      if (!quiet) {
	PRINTMSG(AT_DEF_LINE(attr_idx), 1691, Error, AT_DEF_COLUMN(attr_idx),
	  AT_OBJ_NAME_PTR(attr_idx),
	  "non-explicit-shape or non-assumed-size array");
      }
      return FALSE;
    }
  }
  int type_idx = ATD_TYPE_IDX(attr_idx);
  int linear_type = TYP_LINEAR(type_idx);
  if (linear_type == Structure_Type) {
    int type_attr_idx = TYP_IDX(type_idx);
    if (!AT_BIND_ATTR(type_attr_idx)) {
      if (!quiet) {
	PRINTMSG(AT_DEF_LINE(attr_idx), 1693, Error, AT_DEF_COLUMN(attr_idx),
	  AT_OBJ_NAME_PTR(attr_idx), AT_OBJ_NAME_PTR(type_attr_idx));
      }
      return FALSE;
    }
    else {
      check_interoperable_derived_type(type_attr_idx);
    }
  }
  else if (linear_type == Character_1) {
    if (ck_arrayness && !length_type_param_is_one(attr_idx)) {
      if (!quiet) {
	PRINTMSG(AT_DEF_LINE(attr_idx), 1695, Error, AT_DEF_COLUMN(attr_idx),
	  AT_OBJ_NAME_PTR(attr_idx));
      }
      return FALSE;
    }
  }
  else if (!interoperable_types[linear_type]) {
    if (!quiet) {
      PRINTMSG(AT_DEF_LINE(attr_idx), 1691, Error, AT_DEF_COLUMN(attr_idx),
	AT_OBJ_NAME_PTR(attr_idx), get_basic_type_str(type_idx));
    }
    return FALSE;
  }
return TRUE;
}

/*
 * Assuming a derived type has the BIND attribute, report any resulting
 * constraint violations
 *
 * attr_idx	AT_Tbl_idx index for derived type
 * return	true if derived type has the BIND attribute (even if there are
 * 		violations)
 */
static void
check_interoperable_derived_type(int attr_idx) {
  /* C1502 If has BIND, must not have type parameters (not implemented yet) */
  /* C1503 If has BIND, must not have EXTENDS (not implemented yet) */
  /* C1504 If has BIND, must not have type-bound-procedure-part (not
   * implemented yet) */

  /* C1501 If has BIND, must not be SEQUENCE */
  /* C1505 If has BIND, each component must be nonpointer, nonallocatable,
   * interoperable */
  char *problem = 0;
  if (ATT_SEQUENCE_SET(attr_idx)) {
    problem = "SEQUENCE";
  }
  /*
   * Superfluous because we're going to check components individually
   * else if (ATT_ALLOCATABLE_CPNT(attr_idx)) {
   *   problem = "ALLOCATABLE component";
   * }
   * else if (ATT_POINTER_CPNT(attr_idx)) {
   *   problem = "POINTER component";
   * }
   */
  if (problem) {
    PRINTMSG(AT_DEF_LINE(attr_idx), 1691, Error, AT_DEF_COLUMN(attr_idx),
      AT_OBJ_NAME_PTR(attr_idx), problem);
  }

  for (int sn_idx = ATT_FIRST_CPNT_IDX(attr_idx); sn_idx != NULL_IDX;
    sn_idx = SN_SIBLING_LINK(sn_idx)) {
    check_interoperable_data(SN_ATTR_IDX(sn_idx));
  }
}

/*
 * Assuming entity is subject to BIND constraints, check that it doesn't
 * have OPTIONAL, ALLOCATABLE, or POINTER attributes
 *
 * attr_idx	AT_Tbl_Idx for entity
 */
static int
check_allocatable_pointer_optional(int attr_idx) {
  char *problem = 0;
  if (AT_OPTIONAL(attr_idx)) {
    problem = "OPTIONAL";
  }
  else if (ATD_ALLOCATABLE(attr_idx)) {
    problem = "ALLOCATABLE";
  }
  else if (ATD_POINTER(attr_idx)) {
    problem = "POINTER";
  }
  if (problem) {
    PRINTMSG(AT_DEF_LINE(attr_idx), 1691, Error, AT_DEF_COLUMN(attr_idx),
      AT_OBJ_NAME_PTR(attr_idx), problem);
    AT_DCL_ERR(attr_idx) = TRUE;
    return FALSE;
  }
  return TRUE;
}

/*
 * Check constraints related to BIND attribute on a data entity. If an
 * entity can have the BIND attribute (e.g. a Variable), we assume it does;
 * for dummy arguments, function results, and structure components (which can
 * not themselves have the BIND attribute) we assume they occur in a context
 * which requires them to be interoperable.
 *
 * attr_idx	AT_Tbl_Idx for data entity
 */
static void
check_interoperable_data(int attr_idx) {

  switch (ATD_CLASS(attr_idx)) {

    case Variable:
      if (!AT_MODULE_OBJECT(attr_idx)) {
        PRINTMSG(AT_DEF_LINE(attr_idx), 1694, Error, AT_DEF_COLUMN(attr_idx),
	  AT_OBJ_NAME_PTR(attr_idx));
	AT_DCL_ERR(attr_idx) = TRUE;
      }
      break;

    case Function_Result:
    case Struct_Component:
    case Dummy_Argument:
      /* C530: Dummy argument must not be allocatable, pointer, or optional */
      /* C1238: Each dummy argument must be nonoptional interoperable variable
       * or procedure; function result must be interoperable variable.
       */
      break;

    case Compiler_Tmp:
    case CRI__Pointee:
    case Constant:
    default:
      /* BIND is inapplicable */
      return;
      break;
  }

  if (!(check_allocatable_pointer_optional(attr_idx) &&
    check_interoperable_type(attr_idx, FALSE, TRUE))) {
    AT_DCL_ERR(attr_idx) = TRUE;
  }
}

/*
 * Print error messages for constraint violations related to the BIND attribute
 *
 * attr_idx	AT_Tbl_Idx index for function or subroutine
 */
static void
check_interoperable_procedure(int attr_idx) {

  /* C1242: Must not be elemental */
  /* C1238: All dummy arguments must be variables (not "*") */
  char *problem = 0;
  if (ATP_ELEMENTAL(attr_idx)) {
    problem = "ELEMENTAL";
  }
  else if (ATP_HAS_ALT_RETURN(attr_idx)) {
    problem = "alternate return";
  }
  if (problem) {
    PRINTMSG(AT_DEF_LINE(attr_idx), 1691, Error, AT_DEF_COLUMN(attr_idx),
      AT_OBJ_NAME_PTR(attr_idx), problem);
  }

  switch (ATP_PROC(attr_idx)) {

    /* C532 and C550: BIND requires a variable */
    case Intrin_Proc:
      PRINTMSG(AT_DEF_LINE(attr_idx), 1691, Error, AT_DEF_COLUMN(attr_idx),
        AT_OBJ_NAME_PTR(attr_idx), "INTRINSIC");
      break;

    /* C1237: BIND not allowed on internal procedure */
    case Intern_Proc:
      PRINTMSG(AT_DEF_LINE(attr_idx), 1691, Error, AT_DEF_COLUMN(attr_idx),
        AT_OBJ_NAME_PTR(attr_idx), "an internal procedure");
      break;

    case Module_Proc:
    case Extern_Proc:
      /* BIND not applicable */
      break;

    case Dummy_Proc:
      /* C1236: Dummy procedure must have bind=; parser prohibits name= */
      /* Ditto for body in abstract interface (not yet implemented) */
      if (!AT_BIND_ATTR(attr_idx)) {
	PRINTMSG(AT_DEF_LINE(attr_idx), 1699, Error, AT_DEF_COLUMN(attr_idx),
	  AT_OBJ_NAME_PTR(attr_idx), AT_OBJ_NAME_PTR(ATP_PARENT_IDX(attr_idx)));
      }
      break;

    case Imported_Proc:
    case Unknown_Proc:
    default:
      PRINTMSG(AT_DEF_LINE(attr_idx), 1044, Internal, AT_DEF_COLUMN(attr_idx),
        "Unexpected ATP_PROC in check_interoperable_procedure()");
      break;
  }
  for (int sn_idx = ATP_FIRST_IDX(attr_idx);
    sn_idx < (ATP_FIRST_IDX(attr_idx) + ATP_NUM_DARGS(attr_idx)); sn_idx += 1) {
    int arg_attr_idx = SN_ATTR_IDX(sn_idx);
    if (AT_OBJ_CLASS(arg_attr_idx) == Pgm_Unit) {
      check_interoperable_pgm_unit(arg_attr_idx);
    }
    else if (!AT_DCL_ERR(arg_attr_idx)) {
      check_interoperable_data(arg_attr_idx);
    }
  }
}

/*
 * Print error messages for constraint violations related to the BIND attribute
 *
 * attr_idx	AT_Tbl_Idx index for program unit
 */
static void
check_interoperable_pgm_unit(int attr_idx) {
  switch (ATP_PGM_UNIT(attr_idx)) {
    case Function:
      check_interoperable_data(ATP_RSLT_IDX(attr_idx));
      check_interoperable_procedure(attr_idx);
      break;

    case Subroutine:
      check_interoperable_procedure(attr_idx);
      break;

    case Program:
    case Blockdata:
    case Module:
    case Pgm_Unknown:
    default:
      PRINTMSG(AT_DEF_LINE(attr_idx), 1044, Internal, AT_DEF_COLUMN(attr_idx),
        "Unexpected ATP_PGM_UNIT in check_interoperable_pgm_unit()");
      break;
  }
}

/*
 * Print error messages for constraint violations related to the BIND attribute
 *
 * attr_idx	AT_Tbl_Idx for an entity
 */
void
check_interoperable_constraints(int attr_idx) {

  switch (AT_OBJ_CLASS(attr_idx)) {
    case Data_Obj:
      switch (ATD_CLASS(attr_idx)) {
        case Dummy_Argument:
	  /* AT_BIND_ATTR() doesn't exist for dummy variables */
	  break;
	case Variable:
	  if (AT_BIND_ATTR(attr_idx)) {
	    check_interoperable_data(attr_idx);
	  }
	  else if (ATD_IN_COMMON(attr_idx) &&
	    SB_BIND_ATTR(ATD_STOR_BLK_IDX(attr_idx))) {
	    check_interoperable_type(attr_idx, FALSE, TRUE);
	  }
	  break;
	default:
	  if (AT_BIND_ATTR(attr_idx)) {
	    check_interoperable_data(attr_idx);
	  }
	  break;
       }
      break;

    case Pgm_Unit:
      if (AT_BIND_ATTR(attr_idx)) {
	check_interoperable_pgm_unit(attr_idx);
      }
      break;

    case Derived_Type:
      if (AT_BIND_ATTR(attr_idx)) {
	check_interoperable_derived_type(attr_idx);
      }
      break;

    case Common_Block:
      /* No constraints regarding contents of common block having BIND? */
      break;

    case Label:
    case Interface:
    case Namelist_Grp:
    default:
      if (AT_BIND_ATTR(attr_idx)) {
	PRINTMSG(AT_DEF_LINE(attr_idx), 1044, Internal, AT_DEF_COLUMN(attr_idx),
	  "Unexpected AT_OBJ_CLASS in check_interoperable_constraints()");
      }
      break;
  }
}

/*
 * Check the criteria for "interoperable variable" in F2003 15.2.4 or 15.2.5.
 * Note that interoperation with global variables has additional requirements,
 * as does the use of a dummy variable in a subroutine or function.
 *
 * attr_idx	AT_Tbl_Idx for a variable
 * return	TRUE if it meets the criteria
 */
boolean
interoperable_variable(int attr_idx) {
  return AT_OBJ_CLASS(attr_idx) == Data_Obj &&
    check_interoperable_type(attr_idx, TRUE, FALSE) &&
    (!ATD_POINTER(attr_idx)) &&
    (!ATD_ALLOCATABLE(attr_idx));
}

/*
 * Check that character length type param is one (or missing, which implies
 * one.)
 *
 * attr_idx	AT_Tbl_Idx for a variable
 * return	FALSE if it's a character type with len != 1
 */
boolean
length_type_param_is_one(int attr_idx) {
  int type_idx = ATD_TYPE_IDX(attr_idx);
  int linear_type = TYP_LINEAR(type_idx);
  return linear_type != Character_1 ||
    (TYP_CHAR_CLASS(type_idx) == Const_Len_Char &&
      TYP_FLD(type_idx) == CN_Tbl_Idx &&
      1 == *(long *) &CN_CONST(TYP_IDX(type_idx)));
}

/*
 * Check the "no length type parameter" criterion for iso_c_binding intrinsic
 * function c_loc. We can't distinguish between "character" and "character(1)",
 * so we allow both.
 */
boolean
no_length_type_param(int attr_idx) {
  return length_type_param_is_one(attr_idx);
  }

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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



static char USMID[] = "\n@(#)5.0_pl/sources/p_driver.c	5.13	10/20/99 16:13:01\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "p_globals.m"
# include "debug.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"
# include "p_driver.h"
# include "fmath.h"
#ifdef KEY /* Bug 4607 */
# include "../sgi/path_intrinsic_list.h"
#endif /* KEY Bug 4607 */


/*****************************************************************\
|* Function prototypes of static functions declared in this file *|
\*****************************************************************/

       void	init_parse_prog_unit(void);
static void	check_for_dup_derived_type_lbl(void);
static void	ck_lbl_construct_name(void);
static void     enter_intrinsic_info (void);
static void	init_const_tbl(void);
static void	set_integer_default_type(void);
static void	stmt_level_semantics(void);

# if defined(_EXPRESSION_EVAL)
static void	parse_expr_for_evaluator(void);
# endif

#ifdef KEY /* Bug 4656 */
/* Used in constructing a set of pointers to roots ("unindented entries")
 * within intrin_tbl */
typedef struct {
  char *name;	/* Name of intrinsic */
  int idx;	/* Index into intrin_tbl */
  } intrin_root_t;
#endif /* KEY Bug 4656 */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Copy the information from the static intrinsic table into the         *|
|*	dictionary.                                                           *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void complete_intrinsic_definition(int		generic_attr)

{
   int		al_idx;
   int          arg_attr_idx;
   int          arg_idx;
   int          attr_idx;
   int          intrin_tbl_idx;
   int          j;
   id_str_type	name;
   int          np_idx;
   int          result_attr;
   int          sn_idx;
   int          tmp_len;
   id_str_type	tmp_nam;
   int     	dp_specific_args;
   int     	dp_specific_rslt;


   TRACE (Func_Entry, "complete_intrinsic_definition", NULL);

# if defined(_DEBUG)

   if ((ATI_FIRST_SPECIFIC_IDX(generic_attr) == NULL_IDX &&
        ATI_NUM_SPECIFICS(generic_attr) > 0)||
       (ATI_FIRST_SPECIFIC_IDX(generic_attr) != NULL_IDX &&
        ATI_NUM_SPECIFICS(generic_attr) == 0)) {
      PRINTMSG(stmt_start_line, 626, Internal, 0,
               "correct intrinsic", "complete_intrinsic_definition");
   }
# endif

   intrin_tbl_idx = ATI_INTRIN_TBL_IDX(generic_attr);
#ifdef KEY /* Bug 4656 */
   int non_ansi = (0 == (intrin_tbl[intrin_tbl_idx].families & ANSI_FAMILY));
#endif /* KEY Bug 4656 */
   j = intrin_tbl_idx + 1;

   while ((! intrin_tbl[j].generic) && 
#ifdef KEY /* Bug 7543 */
          (j < MAX_INTRIN_TBL_SIZE) &&
#endif /* KEY Bug 7543 */
          (intrin_tbl[j].name_len > 0)) {  /* just so don't run off end */

      if (cmd_line_flags.s_pointer8) {
         if ((strcmp("_MALLOC_I4_I4", (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_MALLOC_I4_I8", (char *)&intrin_tbl[j].id_str) == 0)) {
            j = j + 1;  /* skip over the first specific entry */

            while (intrin_tbl[j].intrin_enum == 0 &&
                   intrin_tbl[j].external == 0) {
              j = j + 1;  /* skip over the dummy arguments */
            }
         }
      }

      if (INTEGER_DEFAULT_TYPE == Integer_8 ||
          LOGICAL_DEFAULT_TYPE == Logical_8) {
         if ((strcmp("_SIZE_4", (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_SCAN_4", (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_SIZEOF_4", (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_LBOUND0_4", (char *)&intrin_tbl[j].id_str) == 0) ||
#ifdef KEY /* Bug 6009 */
	     /* Want to allow system_clock with i*4 arguments in -i8 mode */
#else /* KEY Bug 6009 */
             (strcmp("_SYSTEM_CLOCK_4", (char *)&intrin_tbl[j].id_str) == 0) ||
#endif /* KEY Bug 6009 */
             (strcmp("_ASSOCIATED_4", (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_SELECTED_REAL_KIND_4", 
                     (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_FP_CLASS_I4_H", 
                     (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_FP_CLASS_I4_R", 
                     (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_FP_CLASS_I4_D", 
                     (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_UBOUND0_4", (char *)&intrin_tbl[j].id_str) == 0)) {
            j = j + 1;  /* skip over the first specific entry */

            while (intrin_tbl[j].intrin_enum == 0 &&
                   intrin_tbl[j].external == 0) {
              j = j + 1;  /* skip over the dummy arguments */
            }
         }
      }

      dp_specific_args = 0;
      dp_specific_rslt = 0;
      /* If double precision enabled, alter the     */
      /* result type of the intrinsic in the        */
      /* static intrinsic table now to reflect the  */
      /* correct result type of this intrinsic.     */

# if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)
         if (intrin_tbl[intrin_tbl_idx].n_specifics == 1) {
            if (intrin_tbl[intrin_tbl_idx].id_str.string[0] == 'Q' ||
                (intrin_tbl[intrin_tbl_idx].id_str.string[0] == 'C' &&
                 intrin_tbl[intrin_tbl_idx].id_str.string[1] == 'Q')) {
               PRINTMSG (stmt_start_line, 541, Error, 1);
            }
         }
# endif

      if (intrin_tbl[intrin_tbl_idx].n_specifics == 1 &&
#ifdef KEY /* Bug 9060 */
	  /* This appalling hackery must not affect "idim" */
          (0 != strcmp("IDIM", (char *)&intrin_tbl[intrin_tbl_idx].id_str)) &&
#endif /* KEY Bug 9060 */
# if defined(_QUAD_PRECISION)
          (intrin_tbl[intrin_tbl_idx].id_str.string[0] == 'D' ||
           (intrin_tbl[intrin_tbl_idx].id_str.string[0] == 'I' &&
            intrin_tbl[intrin_tbl_idx].id_str.string[1] == 'D') ||
           (intrin_tbl[intrin_tbl_idx].id_str.string[0] == 'C' &&
            intrin_tbl[intrin_tbl_idx].id_str.string[1] == 'D'))) {
# else 
          /*
          Treat QUAD precision intrinsics as if they were double precision
          on platforms that do not actually support quad precision arithmetic.
          ie.  PVP, T3E
          */
          (intrin_tbl[intrin_tbl_idx].id_str.string[0] == 'D' ||
           intrin_tbl[intrin_tbl_idx].id_str.string[0] == 'Q' ||
           (intrin_tbl[intrin_tbl_idx].id_str.string[0] == 'I' &&
            intrin_tbl[intrin_tbl_idx].id_str.string[1] == 'D') ||   
           (intrin_tbl[intrin_tbl_idx].id_str.string[0] == 'I' &&
            intrin_tbl[intrin_tbl_idx].id_str.string[1] == 'Q') ||   
           (intrin_tbl[intrin_tbl_idx].id_str.string[0] == 'C' &&
            intrin_tbl[intrin_tbl_idx].id_str.string[1] == 'D') ||   
           (intrin_tbl[intrin_tbl_idx].id_str.string[0] == 'C' &&
            intrin_tbl[intrin_tbl_idx].id_str.string[1] == 'Q'))) {
# endif

         if ((intrin_tbl[j].data_type == Real_8 ||
              intrin_tbl[j].data_type == Real_16) &&
             (1<<intrin_tbl[j].data_type) == intrin_tbl[j+1].data_type) {
            dp_specific_args = (1<<DOUBLE_DEFAULT_TYPE);
            dp_specific_rslt = DOUBLE_DEFAULT_TYPE;
         }
         else if (intrin_tbl[j].data_type == Integer_4) {
            dp_specific_args = (1<<DOUBLE_DEFAULT_TYPE);
            dp_specific_rslt = INTEGER_DEFAULT_TYPE;
         }
         else if ((intrin_tbl[j].data_type == Complex_8 ||
                   intrin_tbl[j].data_type == Complex_16) &&
                  (1<<intrin_tbl[j].data_type) == intrin_tbl[j+1].data_type) {
            dp_specific_args = (1<<DOUBLE_COMPLEX_DEFAULT_TYPE);
            dp_specific_rslt = DOUBLE_COMPLEX_DEFAULT_TYPE;
         }

         if (strcmp("CDABS", (char *)&intrin_tbl[intrin_tbl_idx].id_str) == 0) {
            dp_specific_args = (1<<DOUBLE_COMPLEX_DEFAULT_TYPE);
            dp_specific_rslt = DOUBLE_DEFAULT_TYPE;
         }

         if (strcmp("DIMAG", (char *)&intrin_tbl[intrin_tbl_idx].id_str) == 0) {
            dp_specific_args = (1<<DOUBLE_COMPLEX_DEFAULT_TYPE);
            dp_specific_rslt = DOUBLE_DEFAULT_TYPE;
         }

         if (strcmp("DREAL", (char *)&intrin_tbl[intrin_tbl_idx].id_str) == 0) {
            dp_specific_args = intrin_tbl[j+1].data_type;
            dp_specific_rslt = DOUBLE_DEFAULT_TYPE;
         }

# ifndef _QUAD_PRECISION
         if (strcmp("CQABS", (char *)&intrin_tbl[intrin_tbl_idx].id_str) == 0) {
            dp_specific_args = (1<<DOUBLE_COMPLEX_DEFAULT_TYPE);
            dp_specific_rslt = DOUBLE_DEFAULT_TYPE;
         }

         if (strcmp("QIMAG", (char *)&intrin_tbl[intrin_tbl_idx].id_str) == 0) {
            dp_specific_args = (1<<DOUBLE_COMPLEX_DEFAULT_TYPE);
            dp_specific_rslt = DOUBLE_DEFAULT_TYPE;
         }

         if (strcmp("QREAL", (char *)&intrin_tbl[intrin_tbl_idx].id_str) == 0) {
            dp_specific_args = intrin_tbl[j+1].data_type;
            dp_specific_rslt = DOUBLE_DEFAULT_TYPE;
         }
# endif
            
      }

      tmp_len = intrin_tbl[j].name_len;
      strcpy(&tmp_nam.string[0], &intrin_tbl[j].id_str.string[0]);

      CREATE_ID(name,
                tmp_nam.string, 
                tmp_len);

      NTR_NAME_POOL(&(name.words[0]), intrin_tbl[j].name_len, np_idx);

      NTR_ATTR_TBL(attr_idx); 
      COPY_COMMON_ATTR_INFO(generic_attr, attr_idx, Pgm_Unit);
      AT_NAME_LEN(attr_idx) = intrin_tbl[j].name_len;
      AT_NAME_IDX(attr_idx) = np_idx;

      NTR_INTERFACE_IN_SN_TBL(sn_idx, 
                              attr_idx,
                              generic_attr,
                              stmt_start_line,
                              stmt_start_col);

      AT_OBJ_CLASS(attr_idx)		= Pgm_Unit;
      AT_ELEMENTAL_INTRIN(attr_idx)	= intrin_tbl[j].elemental;
#ifdef KEY /* Bug 4656 & 902 */
      /* random_number is not elemental, but does act like an elemental
       * by taking an array as an argument */
      ATP_ELEMENTAL(attr_idx)		= intrin_tbl[j].elemental &&
      					  (!non_ansi) &&
					  (intrin_tbl[j].intrin_enum !=
					    Random_Number_Intrinsic);
#else /* KEY Bug 4656 & 902 */
      ATP_ELEMENTAL(attr_idx)		= intrin_tbl[j].elemental &&
                                          !(intrin_tbl[j].non_ansi);
#endif /* KEY Bug 4656 & 902 */
      ATP_PURE(attr_idx)		= ATP_ELEMENTAL(attr_idx);
      ATP_PROC(attr_idx)		= Intrin_Proc;
      AT_IS_INTRIN(attr_idx)		= TRUE;
      ATP_EXPL_ITRFC(attr_idx)		= TRUE;
      MAKE_EXTERNAL_NAME(attr_idx, 
                         AT_NAME_IDX(attr_idx), 
                         AT_NAME_LEN(attr_idx));
      ATP_IN_INTERFACE_BLK(attr_idx)	= TRUE;
      ATP_EXTERNAL_INTRIN(attr_idx)	= intrin_tbl[j].external;
#ifdef KEY /* Bug 4656 */
      ATP_NON_ANSI_INTRIN(attr_idx)	= non_ansi;
#else /* KEY Bug 4656 */
      ATP_NON_ANSI_INTRIN(attr_idx)	= intrin_tbl[j].non_ansi;
#endif /* KEY Bug 4656 */
      ATP_INTRIN_ENUM(attr_idx)		= intrin_tbl[j].intrin_enum;

      if (intrin_tbl[j].function) {
         NTR_ATTR_TBL(result_attr); 
         COPY_COMMON_ATTR_INFO(attr_idx, result_attr, Data_Obj);
         ATD_CLASS(result_attr)	= Function_Result;
         ATP_RSLT_IDX(attr_idx)	= result_attr;
         ATD_FUNC_IDX(result_attr) = attr_idx;
         ATD_TYPE_IDX(result_attr) = intrin_tbl[j].data_type;

         if (dp_specific_rslt != 0) 
            ATD_TYPE_IDX(result_attr) = dp_specific_rslt;

         if ((strcmp("KIND", (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_LBOUND", (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_UBOUND", (char *)&intrin_tbl[j].id_str) == 0)) {
            ATD_TYPE_IDX(result_attr) = INTEGER_DEFAULT_TYPE;
         }

         ATD_ARRAY_IDX(result_attr) = intrin_tbl[j].n_specifics;
         ATD_IM_A_DOPE(result_attr) = intrin_tbl[j].dope;
         ATP_PGM_UNIT(attr_idx) = Function;
         ATP_NOSIDE_EFFECTS(attr_idx) = TRUE;
         ATP_PURE(attr_idx) = TRUE;
      }
      else {
         ATP_PGM_UNIT(attr_idx) = Subroutine;
      }

      ATP_SCP_IDX(attr_idx) = curr_scp_idx;

      j = j + 1;

      while ((intrin_tbl[j].intrin_enum == 0) &&
             (intrin_tbl[j].name_len > 0) &&  /* just so don't run off end */
             (!intrin_tbl[j].external) &&
             (!intrin_tbl[j].generic)) {
         CREATE_ID(name,
                   intrin_tbl[j].id_str.string, 
                   intrin_tbl[j].name_len);

         NTR_NAME_POOL(&(name.words[0]), 
                       intrin_tbl[j].name_len,
                       np_idx);

         NTR_ATTR_TBL(arg_attr_idx);
         AT_DEF_LINE(arg_attr_idx) = stmt_start_line;
         AT_DEF_COLUMN(arg_attr_idx) = stmt_start_col;
         AT_NAME_LEN(arg_attr_idx) = intrin_tbl[j].name_len;
         AT_NAME_IDX(arg_attr_idx) = np_idx;

         NTR_SN_TBL(arg_idx);
         SN_ATTR_IDX(arg_idx) = arg_attr_idx;
         SN_NAME_LEN(arg_idx) = intrin_tbl[j].name_len;
         SN_NAME_IDX(arg_idx) = np_idx;

         if (ATP_FIRST_IDX(attr_idx) == NULL_IDX) {
            ATP_FIRST_IDX(attr_idx) = arg_idx;
         }

         ATP_NUM_DARGS(attr_idx) += 1;

         if (intrin_tbl[j].function) {
            AT_OBJ_CLASS(arg_attr_idx) = Pgm_Unit;
            AT_NAME_LEN(arg_attr_idx) = intrin_tbl[j].name_len;
            AT_NAME_IDX(arg_attr_idx) = np_idx;
            ATP_PROC(arg_attr_idx) = Dummy_Proc;
            ATP_EXT_NAME_LEN(arg_attr_idx) = intrin_tbl[j].name_len;
            ATP_EXT_NAME_IDX(arg_attr_idx) = np_idx;
         }
         else {
            AT_OBJ_CLASS(arg_attr_idx) = Data_Obj;
            ATD_CLASS(arg_attr_idx) = Dummy_Argument;
            ATD_INTRIN_DARG(arg_attr_idx) = TRUE;
            ATD_INTRIN_DARG_TYPE(arg_attr_idx) = intrin_tbl[j].data_type;
            if (dp_specific_args != 0) 
               ATD_INTRIN_DARG_TYPE(arg_attr_idx) = dp_specific_args;
            ATD_IM_A_DOPE(arg_attr_idx) = intrin_tbl[j].dope;
            ATD_ARRAY_IDX(arg_attr_idx) = intrin_tbl[j].n_specifics;
         }

         AT_IS_DARG(arg_attr_idx) = TRUE;
         AT_OPTIONAL(arg_attr_idx) = intrin_tbl[j].optional;
         j = j + 1;
      }

      if (!cmd_line_flags.s_pointer8) {
         if ((strcmp("_MALLOC_I8_I4", (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_MALLOC_I8_I8", (char *)&intrin_tbl[j].id_str) == 0)) {
            j = j + 1;  /* skip over the first specific entry */

            while ((intrin_tbl[j].intrin_enum == 0) &&
                   (! intrin_tbl[j].generic)) {
              j = j + 1;  /* skip over the dummy arguments */
            }
         }
      }

      if (INTEGER_DEFAULT_TYPE == Integer_4 ||
          LOGICAL_DEFAULT_TYPE == Logical_4) {
         if ((strcmp("_SIZE_8", (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_SCAN_8", (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_SIZEOF_8", (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_LBOUND0_8", (char *)&intrin_tbl[j].id_str) == 0) ||
#ifdef KEY /* Bug 6009 */
             /* Want to allow system_clock with i*8 args in -i4 mode */
#else /* KEY Bug 6009 */
             (strcmp("_SYSTEM_CLOCK_8", (char *)&intrin_tbl[j].id_str) == 0) ||
#endif /* KEY Bug 6009 */
             (strcmp("_ASSOCIATED_8", (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_SELECTED_REAL_KIND_8", 
                     (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_FP_CLASS_I8_H",
                     (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_FP_CLASS_I8_R",
                     (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_FP_CLASS_I8_D",
                     (char *)&intrin_tbl[j].id_str) == 0) ||
             (strcmp("_UBOUND0_8", (char *)&intrin_tbl[j].id_str) == 0)) {
            j = j + 1;  /* skip over the first specific entry */

            while ((intrin_tbl[j].intrin_enum == 0) &&
                   (! intrin_tbl[j].generic)) { 
              j = j + 1;  /* skip over the dummy arguments */
            }
         }
      }
   }

   /* Do not allow duplicate attrs on the list.  It causes havoc in modules. */
   /* And modules is why we keep this list in the first place.               */

   al_idx = expanded_intrinsic_list;

   while (al_idx != NULL_IDX) {

      if (generic_attr == AL_ATTR_IDX(al_idx)) {
         break;
      }
      al_idx = AL_NEXT_IDX(al_idx);
   }

   if (al_idx == NULL_IDX) {  /* Not found - add to list */
      NTR_ATTR_LIST_TBL(al_idx);
      AL_ATTR_IDX(al_idx) = generic_attr;
      AL_NEXT_IDX(al_idx) = expanded_intrinsic_list;
      expanded_intrinsic_list = al_idx;
   }

   TRACE (Func_Exit, "complete_intrinsic_definition", NULL);

   return;

}  /* complete_intrinsic_definition */

#ifdef KEY /* Bug 5089 */
/*
 * Machinery for implementing intrinsic modules (such as TR15580, IEEE floating
 * point support.) When a module has the "intrinsic" attribute and we find it
 * in the compiler-provided intrinsic module directory, then we set
 * AT_IS_INTRIN on the module. For each generic interface and specific
 * procedure we read from the module that is not excluded due to the "only"
 * clause, we look up the original name (not the "rename" name, if any) in the
 * intrin_module_table and, if we find it, we tweak various members of the
 * attr_table_entry for that interface or procedure to achieve the desired
 * behavior.
 *
 * The two kinds of entries within the table are:
 *
 *  imt_inline		Call a procedure in s_intrin.c to generate IR to
 *			implement this procedure
 *  imt_external	Generate a call to an external procedure rather than
 *			the one provided by the module (if any.)
 *
 * If the procedure is not found in imt_entry_intrinsic_module_table, we
 * leave the attr_table_entry alone so that it will be treated as a normal
 * import from the module. When writing the module itself, note that the
 * front end requires procedures that are intrinsic to precede procedures
 * that are nonintrinsic within the list of specific procedures for a
 * particular generic interface.
 * 
 * The name of each procedure in the intrinsic modules of TR15580 is unique
 * across all of its modules. If we someday add an intrinsic module containing
 * a procedure whose name duplicates that of a procedure in another intrinsic
 * module, then intrinsic_module_lookup and intrinsic_module_table will need
 * to change to use AT_MODULE_IDX or AT_ORIG_MODULE_IDX (not sure which) to
 * distinguish them.
 *
 * When constructing the intrinsic module itself, one should, where possible,
 * use an interface to an external procedure when one doesn't want to implement
 * the procedure in Fortran within the module itself: the external name will
 * be changed from "function_" to "_Function" to keep it out of the usual
 * Fortran name space and the non-library portion of the C namespace.
 *
 * If that isn't possible (e.g. an argument to the procedure needs to be
 * declared with a non-sequence derived type) the name will be changed from
 * "FUNCTION.in.MODULE" to "_Function".
 *
 * This naming scheme for external procedures assumes that you compile the
 * module with -fsecond-underscore (the default) or -funderscore because it
 * sometimes consumes the trailing underscore.
 */

typedef enum {
  imt_inline,	/* Call xxx_intrinsic procedure to emit code inline */
  imt_extern,	/* Generate call to external procedure */
  imt_type	/* Mark type for special handling */
  } imt_kind;

typedef struct {
  char *name;
  imt_kind kind;
  intrinsic_type index; /* Ignored unless kind == imt_inline */
  } imt_entry;

/* Must be alphabetical by "name" member */
static imt_entry intrinsic_module_table[] = {
  { "C_ASSOCIATED_FUNPTR",		imt_extern, Unknown_Intrinsic},
  { "C_ASSOCIATED_PTR",			imt_extern, Unknown_Intrinsic},
  /* For now, call external procedure for C_FUNLOC and C_LOC because front
   * end blows up if loc_intrinsic() in s_intrin.c creates a Loc_Opr whose
   * result is a derived type. */
  { "C_FUNLOC",				imt_extern, C_Funloc_Intrinsic},
  { "C_FUNPTR",				imt_type,   Unknown_Intrinsic},
  { "C_F_POINTERA",			imt_extern, C_F_Pointer_Intrinsic},
  { "C_F_POINTERS",			imt_extern, C_F_Pointer_Intrinsic},
  { "C_F_PROCPOINTER",			imt_inline, C_F_Procpointer_Intrinsic},
  { "C_LOC",				imt_extern, C_Loc_Iso_Intrinsic},
  { "C_PTR",				imt_type,   Unknown_Intrinsic},
  { "IEEE_CLASS_4",			imt_extern, Unknown_Intrinsic},
  { "IEEE_CLASS_8",			imt_extern, Unknown_Intrinsic},
  { "IEEE_COPY_SIGN_4",			imt_inline, Ieee_Copy_Sign_Intrinsic},
  { "IEEE_COPY_SIGN_4_8",		imt_inline, Ieee_Copy_Sign_Intrinsic},
  { "IEEE_COPY_SIGN_8",			imt_inline, Ieee_Copy_Sign_Intrinsic},
  { "IEEE_COPY_SIGN_8_4",		imt_inline, Ieee_Copy_Sign_Intrinsic},
  { "IEEE_GET_FLAG",			imt_extern, Unknown_Intrinsic},
  { "IEEE_GET_HALTING_MODE",		imt_extern, Unknown_Intrinsic},
  { "IEEE_GET_ROUNDING_MODE",		imt_extern, Unknown_Intrinsic},
  { "IEEE_GET_STATUS",			imt_extern, Unknown_Intrinsic},
  { "IEEE_GET_UNDERFLOW_MODE",		imt_extern, Unknown_Intrinsic},
  { "IEEE_IS_FINITE_4",			imt_inline, Ieee_Finite_Intrinsic},
  { "IEEE_IS_FINITE_8",			imt_inline, Ieee_Finite_Intrinsic},
  { "IEEE_IS_NAN_4",			imt_inline, Ieee_Is_Nan_Intrinsic},
  { "IEEE_IS_NAN_8",			imt_inline, Ieee_Is_Nan_Intrinsic},
  { "IEEE_IS_NEGATIVE_4",		imt_extern, Unknown_Intrinsic},
  { "IEEE_IS_NEGATIVE_8",		imt_extern, Unknown_Intrinsic},
  { "IEEE_IS_NORMAL_4",			imt_extern, Unknown_Intrinsic},
  { "IEEE_IS_NORMAL_8",			imt_extern, Unknown_Intrinsic},
  { "IEEE_LOGB_4",			imt_extern, Unknown_Intrinsic},
  { "IEEE_LOGB_8",			imt_extern, Unknown_Intrinsic},
  { "IEEE_NEXT_AFTER_4",		imt_inline, Ieee_Next_After_Intrinsic},
  { "IEEE_NEXT_AFTER_4_8",		imt_inline, Ieee_Next_After_Intrinsic},
  { "IEEE_NEXT_AFTER_8",		imt_inline, Ieee_Next_After_Intrinsic},
  { "IEEE_NEXT_AFTER_8_4",		imt_inline, Ieee_Next_After_Intrinsic},
  { "IEEE_REM_4",			imt_inline, Ieee_Remainder_Intrinsic},
  { "IEEE_REM_4_8",			imt_inline, Ieee_Remainder_Intrinsic},
  { "IEEE_REM_8",			imt_inline, Ieee_Remainder_Intrinsic},
  { "IEEE_REM_8_4",			imt_inline, Ieee_Remainder_Intrinsic},
  { "IEEE_RINT_4",			imt_extern, Unknown_Intrinsic},
  { "IEEE_RINT_8",			imt_extern, Unknown_Intrinsic},
  { "IEEE_SCALB_4",			imt_inline, Ieee_Binary_Scale_Intrinsic},
  { "IEEE_SCALB_4_8",			imt_inline, Ieee_Binary_Scale_Intrinsic},
  { "IEEE_SCALB_8",			imt_inline, Ieee_Binary_Scale_Intrinsic},
  { "IEEE_SCALB_8_4",			imt_inline, Ieee_Binary_Scale_Intrinsic},
  { "IEEE_SELECTED_REAL_KIND",		imt_inline, SRK_Intrinsic},
  { "IEEE_SET_FLAG",			imt_extern, Unknown_Intrinsic},
  { "IEEE_SET_HALTING_MODE",		imt_extern, Unknown_Intrinsic},
  { "IEEE_SET_ROUNDING_MODE",		imt_extern, Unknown_Intrinsic},
  { "IEEE_SET_STATUS",			imt_extern, Unknown_Intrinsic},
  { "IEEE_SET_UNDERFLOW_MODE",		imt_extern, Unknown_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_4",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_4A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_4B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_4C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_4D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_4E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_4F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_4G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_8",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_8A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_8B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_8C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_8D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_8E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_8F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DATATYPE_8G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_4",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_4A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_4B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_4C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_4D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_4E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_4F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_4G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_8",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_8A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_8B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_8C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_8D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_8E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_8F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DENORMAL_8G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_4",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_4A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_4B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_4C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_4D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_4E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_4F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_4G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_8",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_8A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_8B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_8C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_8D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_8E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_8F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_DIVIDE_8G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_4",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_4A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_4B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_4C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_4D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_4E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_4F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_4G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_8",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_8A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_8B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_8C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_8D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_8E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_8F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_FLAG_8G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_HALTING",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF",			imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_4",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_4A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_4B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_4C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_4D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_4E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_4F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_4G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_8",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_8A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_8B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_8C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_8D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_8E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_8F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_INF_8G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO",			imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_4",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_4A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_4B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_4C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_4D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_4E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_4F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_4G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_8",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_8A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_8B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_8C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_8D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_8E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_8F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_IO_8G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN",			imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_4",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_4A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_4B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_4C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_4D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_4E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_4F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_4G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_8",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_8A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_8B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_8C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_8D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_8E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_8F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_NAN_8G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_4",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_4A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_4B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_4C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_4D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_4E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_4F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_4G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_8",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_8A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_8B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_8C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_8D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_8E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_8F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_ROUNDING_8G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_4",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_4A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_4B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_4C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_4D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_4E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_4F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_4G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_8",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_8A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_8B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_8C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_8D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_8E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_8F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_SQRT_8G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_4",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_4A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_4B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_4C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_4D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_4E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_4F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_4G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_8",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_8A",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_8B",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_8C",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_8D",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_8E",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_8F",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_STANDARD_8G",		imt_inline, True_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL",	imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_4",	imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_4A",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_4B",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_4C",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_4D",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_4E",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_4F",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_4G",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_8",	imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_8A",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_8B",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_8C",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_8D",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_8E",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_8F",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_SUPPORT_UNDERFLOW_CONTROL_8G",imt_inline, Support_Uflow_Intrinsic},
  { "IEEE_UNORDERED_4",			imt_inline, Ieee_Unordered_Intrinsic},
  { "IEEE_UNORDERED_4_8",		imt_inline, Ieee_Unordered_Intrinsic},
  { "IEEE_UNORDERED_8",			imt_inline, Ieee_Unordered_Intrinsic},
  { "IEEE_UNORDERED_8_4",		imt_inline, Ieee_Unordered_Intrinsic},
  { "IEEE_VALUE_4",			imt_extern, Unknown_Intrinsic},
  { "IEEE_VALUE_8",			imt_extern, Unknown_Intrinsic}
};

static int intrinsic_module_cmp(const void *va, const void *vb)
{
  imt_entry *a = (imt_entry *) va;
  imt_entry *b = (imt_entry *) vb;
  return strcmp(a->name, b->name);
}

/*
 * name		name of attribute in module
 * returns	pointer to intrinsic_module_table entry for that name, or null
 */
static imt_entry *
find_imt(char *name) {
  imt_entry key;
  key.name = name;
  return bsearch(&key,
    intrinsic_module_table,
    ((sizeof intrinsic_module_table) / (sizeof *intrinsic_module_table)),
    sizeof *intrinsic_module_table,
    intrinsic_module_cmp);
  }

/*
 * Given a symbol from an intrinsic module, if it is a generic interface,
 * mark it as "intrinsic" and also mark its specific procedures as "intrinsic".
 *
 * attr_idx	Attribute of the interface
 * returns	true if its procedures are intrinsic
 */
int intrinsic_module_lookup(int attr_idx)
{
  /* Only interface or program unit can be intrinsic */
  obj_class_type obj_class = AT_OBJ_CLASS(attr_idx);
  if (Derived_Type == obj_class) {
    imt_entry *keyp = find_imt(AT_ORIG_NAME_PTR(attr_idx));
    if (keyp) {
      if (is_x8664_n32() && keyp->kind == imt_type) {
        AT_IS_INTRIN(attr_idx) = TRUE;
	if (AT_OBJ_CLASS(attr_idx) == Derived_Type) {
	  ATT_NUM_CPNTS(attr_idx) = 0;
	  ATT_FIRST_CPNT_IDX(attr_idx) = 0;
	}
	return 1;
      }
      return 0;
    }
  }
  else if (Interface != obj_class && Derived_Type != obj_class) {
    return 0;
  }

  /* Iterate through specific procedures, marking them intrinsic if
   * they appear in the intrinsic_module_table */
  int spec_sn_idx = ATI_FIRST_SPECIFIC_IDX(attr_idx);
  int found = 0;
  int elemental = 0;
  for (int s = ATI_NUM_SPECIFICS(attr_idx); s > 0; s -= 1) {
    int spec_idx = SN_ATTR_IDX(spec_sn_idx);
    imt_entry *keyp = find_imt(AT_ORIG_NAME_PTR(spec_idx));
    if (keyp) {
      found = keyp->index;
      if (imt_inline == keyp->kind || Unknown_Intrinsic != keyp->index) {
	elemental = AT_ELEMENTAL_INTRIN(spec_idx) = ATP_ELEMENTAL(spec_idx);
	ATP_PROC(spec_idx) = Intrin_Proc;
	AT_IS_INTRIN(spec_idx) = TRUE;
	ATP_EXTERNAL_INTRIN(spec_idx) = (imt_extern == keyp->kind);
	ATP_NON_ANSI_INTRIN(spec_idx) = FALSE;
	ATP_INTRIN_ENUM(spec_idx) = keyp->index;
      }
      if (imt_extern == keyp->kind) {
        /* Change name from FUNCTION.in.MODULE to _Function or from
	 * function_ to _Function. We're doing this in place because if
	 * we use NTR_NAME_POOL to allocate space for a new name, the
	 * heap gets corrupted (probably there's funny-business going on
	 * in the code that reads module info from a file.) */
	char *user_name = ATP_EXT_NAME_PTR(spec_idx);
	char *dot = strchr(user_name, '.');
	if (0 == dot) {
	  dot = user_name + strlen(user_name) - 1;
	}
	else {
	  *(dot + 1) = '\0';
	}
	for (dot -= 1; dot > user_name; dot -= 1) {
	  *(dot + 1) = tolower(*dot);
	}
	*(dot + 1) = toupper(*dot);
	*user_name = '_';
      }
      /* For x8664 -m32, types C_PTR and C_FUNPTR must be treated as scalars
       * compatible with C "void *", not as structures. In user code, they
       * are marked with AT_IS_INTRIN: the c_ptr_abi_trouble() function
       * then ensures that functions which return them treat them as scalars.
       * But when compiling iso_c_binding.F90, they are not marked specially,
       * so here we must treat specially the two functions C_LOC and C_FUNLOC
       * which return them. */
      if (is_x8664_n32() && (keyp->index == C_Loc_Iso_Intrinsic ||
	keyp->index == C_Funloc_Intrinsic)) {
	ATP_EXTRA_DARG(spec_idx) = FALSE;
	ATP_NUM_DARGS(spec_idx) = 1;
	ATP_FIRST_IDX(spec_idx) += 1;
      }
    }
    spec_sn_idx = SN_SIBLING_LINK(spec_sn_idx);
  }

  /* If any specific procedure was intrinsic, interface is intrinsic */
  if (found) {
    AT_IS_INTRIN(attr_idx) = TRUE;
    ATI_INTRIN_TBL_IDX(attr_idx) = found;
    ATI_INTRIN_PASSABLE(attr_idx) = FALSE;
    ATI_GENERIC_INTRINSIC(attr_idx) = FALSE; /* Dope? */
    AT_ELEMENTAL_INTRIN(attr_idx) = elemental;
  }

  return found;
}

#endif /* KEY Bug 5089 */
#ifdef KEY /* Bug 4656 */
/* Compare two args of type intrin_root_t **, for use in qsort or bsearch */
static int root_cmp(const void *o1, const void *o2) {
  intrin_root_t *i1 = *(intrin_root_t **) o1;
  intrin_root_t *i2 = *(intrin_root_t **) o2;
  return strcmp(i1->name, i2->name);
}

/* Compare two args of type intrin_family_t **, for use in qsort or bsearch */
static int family_cmp(const void *o1, const void *o2) {
  intrin_family_t *f1 = (intrin_family_t *) o1;
  intrin_family_t *f2 = (intrin_family_t *) o2;
  return strcmp(f1->name, f2->name);
}

#endif /* KEY Bug 4656 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Copy the information from the static intrinsic table into the         *|
|*	dictionary.                                                           *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void enter_intrinsic_info (void)

{
   int          attr_idx;
   int          name_idx        = 2;
   token_type   tmp_token;
   int          i;

   TRACE (Func_Entry, "enter_intrinsic_info", NULL);
   
   i = 1;
   tmp_token = initial_token;
   TOKEN_COLUMN(tmp_token) = 1;
   TOKEN_LINE(tmp_token) = 1;
   TOKEN_VALUE(tmp_token) = Tok_Id;

#ifdef KEY /* Bug 4656 */
   int intrin_roots_len = 0;
   intrin_root_t **intrin_roots =
     malloc(MAX_INTRIN_TBL_SIZE * (sizeof *intrin_roots));
   intrin_root_t *intrin_roots_data =
     malloc(MAX_INTRIN_TBL_SIZE * (sizeof intrin_roots));
   for (int e = 1; e < MAX_INTRIN_TBL_SIZE; e += 1) {
     if (!intrin_tbl[e].generic) {
       continue;
     }

     /* Make sure that "-intrinsic=EVERY" means "every intrinsic" */
     intrin_tbl[e].families |= EVERY_FAMILY;

     /* Default is to enable ANSI_FAMILY if -ansi is in effect, or
      * TRADITIONAL_FAMILY otherwise. If -mp is in effect, add OMP_FAMILY */
     int families = intrin_tbl[e].families;
     int default_family = on_off_flags.issue_ansi_messages ?
       ANSI_FAMILY :
       TRADITIONAL_FAMILY;
     intrin_tbl[e].enabled = ((0 != (families & default_family)) ||
       (dump_flags.open_mp && (0 != (families & OMP_FAMILY))));

     /* Build a directory, mapping intrinsic name to index for each entry in
      * intrin_tbl which represents the root of an intrinsic description (i.e.
      * a "singly-indented" entry in source code terms.) */
     intrin_roots[intrin_roots_len] = &(intrin_roots_data[intrin_roots_len]);
     intrin_roots[intrin_roots_len]->name = intrin_tbl[e].id_str.string;
     intrin_roots[intrin_roots_len++]->idx = e;
   }
   qsort(intrin_roots, intrin_roots_len, sizeof *intrin_roots, root_cmp);

   int olen = path_intrinsic_list_length();
   intrin_option_t **options = path_intrinsic_list_list();
   for (int o = 0; o < olen; o += 1) {
     boolean found = FALSE;
     intrin_option_t *option = options[o];

     /* Enable or disable family of intrinsics */
     if (option->isfamily_) {
       intrin_family_t key;
       key.name = option->name_;
       intrin_family_t *keyp = (intrin_family_t *) bsearch(&key,
         intrin_families, SIZEOF_INTRIN_FAMILIES, (sizeof *intrin_families),
	 family_cmp);
       if (keyp) {
	 for (int r = 0; r < intrin_roots_len; r += 1) {
	   intrin_tbl_type *entry = &(intrin_tbl[intrin_roots[r]->idx]);
	   if (0 != (entry->families & keyp->mask)) {
	     entry->enabled = option->added_;
	   }
	 }
	 found = TRUE;
       }
     }

     /* Enable or disable individual intrinsic */
     if (!found) {
       char subr_name[sizeof intrin_tbl[0].id_str.string];
       intrin_root_t key;
       intrin_root_t *keyp = &key;
       key.name = option->name_;
       for (int k = 0; k <= 1; k += 1) {
	 intrin_root_t **keypp = (intrin_root_t **) bsearch(&keyp, intrin_roots,
	   intrin_roots_len, sizeof *intrin_roots, root_cmp);
	 if (keypp) {
	   intrin_tbl[(*keypp)->idx].enabled = option->added_;
	   /* Issue warning only once if there's an extra subroutine version */
	   if (option->isfamily_ && !found) {
	     PRINTMSG(0, 1681, Log_Warning, 0, option->name_);
	   }
	   found = TRUE;
	 }
	 /* Now see if there's an extra G77 subroutine version of the
	  * name provided by the user; must enable or disable it as well */
	 key.name = strcat(strcpy(subr_name, option->name_),
	   INTRIN_SUBR_SUFFIX);
       }
     }

     /* Didn't find the name provided by the user */
     if (!found) {
       PRINTMSG(0, 701, Log_Error, 0, option->name_);
     }
   }
#endif /* KEY Bug 4656 */

#ifdef KEY /* Bug 4656 */
   for (int r = 0; r < intrin_roots_len; r += 1) {
      i = intrin_roots[r]->idx;
      /* Command-line options did not enable this intrinsic */
      if (!intrin_tbl[i].enabled) {
	continue;
	}
#else /* KEY Bug 4656 */
   while (i < MAX_INTRIN_TBL_SIZE) {
      if (intrin_tbl[i].generic) {
#endif /* KEY Bug 4656 */

         CREATE_ID(TOKEN_ID(tmp_token), 
                   intrin_tbl[i].id_str.string, 
                   intrin_tbl[i].name_len);

         TOKEN_LEN(tmp_token) = intrin_tbl[i].name_len;

         attr_idx = ntr_sym_tbl(&tmp_token, name_idx);

         AT_OBJ_CLASS(attr_idx)	= Interface;
         AT_IS_INTRIN(attr_idx)	= TRUE;
         ATI_INTRIN_PASSABLE(attr_idx) = intrin_tbl[i].passable;
         ATI_GENERIC_INTRINSIC(attr_idx) = intrin_tbl[i].dope;
         AT_ELEMENTAL_INTRIN(attr_idx) = intrin_tbl[i].elemental;

         if (intrin_tbl[i].function) {
            ATI_INTERFACE_CLASS(attr_idx) = Generic_Function_Interface;
         }
         else {
            ATI_INTERFACE_CLASS(attr_idx) = Generic_Subroutine_Interface;
         }
        
         ATI_INTRIN_TBL_IDX(attr_idx) = i;
         name_idx = name_idx + 1;
#ifndef KEY /* Bug 4656 */
      }

      i = i + 1;
   }  
#else /* KEY Bug 4656 */
   }  
#endif /* KEY Bug 4656 */

   expanded_intrinsic_list	= NULL_IDX;

   TRACE (Func_Exit, "enter_intrinsic_info", NULL);

   return;

}  /* enter_intrinsic_info */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	To drive the parsing of each statement of a program unit by deter-    *|
|*	mining the statement's type (based on the statement's first non-      *|
|*	label, non-construct name token), calling out the approprate state-   *|
|*	ment parser, and determining if the valid statement is ordered	      *|
|*	properly and allowed in the current program unit context.	      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void parse_prog_unit (void)

{
   int		defer_msg			= 0;
   int		name_idx;
   int		need_ez_debug_label		= FALSE;
   int		prev_stmt_start_line;
   int		save_blk_stk_idx;
   int		sh_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int		ir_idx;
# endif


   TRACE (PU_Start, NULL, NULL);

   TRACE (Func_Entry, "parse_prog_unit", NULL);

   if (first_time_tbl_alloc) {
      first_time_tbl_alloc = FALSE;
      /* init_parse_prog_unit already called */
   }
   else {
      init_parse_prog_unit();
   }

   prev_stmt_start_line	= stmt_start_line;
   pgm_unit_start_line	= stmt_start_line;

# if defined(_EXPRESSION_EVAL)

   if (cmd_line_flags.expression_eval_expr) {  /* Parsing just an expression */
      parse_expr_for_evaluator();        /* Should point to EOPU       */
   }

# endif

   while (!EOPU_encountered) {

      TRACE_NEW_STMT(NULL);

      stmt_type	= Null_Stmt;

      if (need_new_sh) {
         sh_idx				= curr_stmt_sh_idx;
         curr_stmt_sh_idx		= ntr_sh_tbl();
         SH_NEXT_IDX(sh_idx)		= curr_stmt_sh_idx;
         SH_PREV_IDX(curr_stmt_sh_idx)	= sh_idx;
      }
      else {
         /* clear out the old one. */
         /* except for prev idx    */
         sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
         CLEAR_TBL_NTRY(sh_tbl, curr_stmt_sh_idx);
         SH_PREV_IDX(curr_stmt_sh_idx) = sh_idx;
      }

      ck_lbl_construct_name();

      if (MATCHED_TOKEN_CLASS(Tok_Class_Keyword)) {
         determine_stmt_type();

         if (curr_stmt_category == Init_Stmt_Cat &&
             cdir_switches.implicit_use_idx != NULL_IDX) {

            /* We have an implicit use.  Is this an unnamed program unit? */

            switch (stmt_type) {
            case Blockdata_Stmt:
            case Elemental_Stmt:
            case Function_Stmt:
            case Module_Stmt:
            case Program_Stmt:
            case Pure_Stmt:
            case Recursive_Stmt:
            case Subroutine_Stmt:

                /* We know for sure that this is a program type statement. */
                /* implicit_use_semantics will be called in p_dcl_pu       */

            case Directive_Stmt:

                /* Status is unknown - Use the next statement to determine. */

                /* Intentionally blank                                      */

                break;

            case Type_Decl_Stmt:

               if ((TOKEN_VALUE(token) == Tok_Kwd_Type && 
                    LA_CH_VALUE != LPAREN) ||
                    stmt_has_double_colon()) {

                  /* This is a type statement or derived type statement,  */
                  /* so this is an unnamed program unit.                  */

                  implicit_use_semantics();
               }

               /* else treat as a typed function statement. */

               break;

            default:
               implicit_use_semantics();              /* Program is $MAIN. */
               break;
            }
         }

         if (stmt_type != Use_Stmt &&
             SCP_USED_MODULE_LIST(curr_scp_idx) != NULL_IDX) {
            use_stmt_semantics();
         }

         if (need_ez_debug_label) {
            gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
            need_ez_debug_label = FALSE;
         }

         if (cmd_line_flags.debug_lvl == Debug_Lvl_0) {  /* -ed  -G0 */

            /* Generate debug labels for all statements. */

            if (prev_stmt_start_line != stmt_start_line) {

               /* End statements have their own code in parse_end_stmt */

               switch (stmt_type) {
                  case Allocate_Stmt:
                  case Arith_If_Stmt:
                  case Assign_Stmt:
                  case Assignment_Stmt:
                  case Backspace_Stmt:
                  case Buffer_Stmt:
                  case Call_Stmt:
                  case Case_Stmt:
                  case Close_Stmt:
                  case Continue_Stmt:
                  case Cycle_Stmt:
                  case Deallocate_Stmt:
                  case Decode_Stmt:
                  case Do_Iterative_Stmt:
                  case Do_While_Stmt:
                  case Do_Infinite_Stmt:
                  case Else_Where_Stmt:
                  case Encode_Stmt:
                  case Endfile_Stmt:
                  case Entry_Stmt:
                  case Exit_Stmt:
                  case Goto_Stmt:
                  case If_Cstrct_Stmt:
                  case If_Stmt:
                  case Inquire_Stmt:
                  case Nullify_Stmt:
                  case Open_Stmt:
                  case Outmoded_If_Stmt:
                  case Pause_Stmt:
                  case Print_Stmt:
                  case Read_Stmt:
                  case Return_Stmt:
                  case Rewind_Stmt:
                  case Select_Stmt:
                  case Stop_Stmt:
                  case Then_Stmt:
                  case Where_Cstrct_Stmt:
                  case Where_Stmt:
                  case Write_Stmt:
                     gen_debug_lbl_stmt(curr_stmt_sh_idx,
                                        Ldbg_Stmt_Lbl,
                                        NULL_IDX);
                     break;

               }
            }
            prev_stmt_start_line = stmt_start_line;
         }

         if (stmt_label_idx != NULL_IDX) {
            gen_attr_and_IR_for_lbl(TRUE);
         }

         (*stmt_parsers[stmt_type])();

         stmt_level_semantics();

         if (cdir_switches.implicit_use_idx != NULL_IDX) {

            /* This is an unnamed program unit, whose first */
            /* statement is a type declaration statement.   */

            implicit_use_semantics();
         }

         if (cmd_line_flags.debug_lvl == Debug_Lvl_1) {  /* -ez  -G1 */

            /* For optimized debugging - these statements need       */
            /* labels at the start of the statement following them.  */

            switch (stmt_type) {
               case If_Cstrct_Stmt:
               case Else_Stmt:
               case Else_If_Stmt:
               case Else_Where_Stmt:
               case Case_Stmt:
               case Where_Cstrct_Stmt:
                  need_ez_debug_label = TRUE;
                  break;
            }
         }
      }
      else {   /*  MATCHED_TOKEN_CLASS(Tok_Class_Keyword) failed.  */

         /* This is a label or construct with nothing on it.  This is an      */
         /* error situation.  Because we are in error recovery, we do a       */
         /* little extra work to get more meaningful messages for the user.   */

         /* Pathological case:    1. First stmt in prog unit is in error.     */
         /*                       2. Second stmt in prog unit is END.         */
         /* If the Unit record has not been output yet, we need to do it now  */
         /* because the next stmt to be parsed is the END stmt, which cause   */
         /* all the END stmt records to precede the head-of-prog-unit records.*/

         /* We also need to do a little setup for $MAIN and statement numbers */

         stmt_start_line		= LA_CH_LINE;
         stmt_start_col			= LA_CH_COLUMN;
         SH_GLB_LINE(curr_stmt_sh_idx)	= stmt_start_line;
         SH_COL_NUM(curr_stmt_sh_idx)	= stmt_start_col;
         SH_STMT_TYPE(curr_stmt_sh_idx)	= stmt_type;

         if (SCP_ATTR_IDX(curr_scp_idx) == glb_tbl_idx[Main_Attr_Idx] &&
             !AT_DEFINED(glb_tbl_idx[Main_Attr_Idx]) &&
             !AT_DCL_ERR(glb_tbl_idx[Main_Attr_Idx])) {

            if (curr_stmt_category == Init_Stmt_Cat) { 
               curr_stmt_category = Use_Stmt_Cat;
            }

            token		= main_token;
            TOKEN_LINE(token)	= stmt_start_line;
            TOKEN_COLUMN(token)	= stmt_start_col;
            save_blk_stk_idx	= blk_stk_idx;
            blk_stk_idx		= BLK_HEAD_IDX;
            defer_msg		= 1;

            start_new_prog_unit(Program, Program_Blk, TRUE, FALSE, &defer_msg);

            CURR_BLK_NAME	= NULL_IDX;
            blk_stk_idx		= save_blk_stk_idx;
         }

         if (cif_need_unit_rec  &&
             stmt_type != Directive_Stmt &&
             stmt_type != End_Parallel_Stmt &&
             stmt_type != End_Do_Parallel_Stmt &&
             stmt_type != End_Parallel_Case_Stmt &&
             stmt_type != Parallel_Case_Stmt &&
             stmt_type != End_Guard_Stmt &&
             stmt_type != Open_MP_Section_Stmt &&
             stmt_type != Open_MP_End_Parallel_Stmt &&
             stmt_type != Open_MP_End_Do_Stmt &&
             stmt_type != Open_MP_End_Parallel_Sections_Stmt &&
             stmt_type != Open_MP_End_Sections_Stmt &&
             stmt_type != Open_MP_End_Section_Stmt &&
             stmt_type != Open_MP_End_Single_Stmt &&
             stmt_type != Open_MP_End_Parallel_Do_Stmt &&
             stmt_type != Open_MP_End_Master_Stmt &&
             stmt_type != Open_MP_End_Critical_Stmt &&
             stmt_type != Open_MP_End_Ordered_Stmt &&
             stmt_type != SGI_Section_Stmt &&
             stmt_type != SGI_End_Psection_Stmt &&
             stmt_type != SGI_End_Pdo_Stmt &&
             stmt_type != SGI_End_Parallel_Stmt &&
             stmt_type != SGI_End_Critical_Section_Stmt &&
             stmt_type != SGI_End_Single_Process_Stmt &&
             stmt_type != SGI_Region_End_Stmt) {

            cif_unit_rec();

            if (cif_flags) {
               cif_begin_scope_rec();

               if (cif_flags & XREF_RECS) {
                  cif_usage_rec(glb_tbl_idx[Main_Attr_Idx],
                                AT_Tbl_Idx,
                                stmt_start_line,
                                stmt_start_col,
                                CIF_Symbol_Declaration);
               }
            }
         }

	 if ((stmt_label_idx | stmt_construct_idx) == NULL_IDX) {

            if (LA_CH_CLASS == Ch_Class_Digit && ! label_ok) {
               PRINTMSG (LA_CH_LINE, 407, Error, LA_CH_COLUMN);
            }
            else {

               /* A statement must begin with a label, construct name,   */
               /* keyword or identifier.			         */

	       PRINTMSG (LA_CH_LINE, 100, Error, LA_CH_COLUMN);
            }
         }
         else {

	    /* If a stmt label exists, issue an error because a keyword or    */
            /* identifier must follow it.				      */

            if (stmt_label_idx != NULL_IDX) {
	       PRINTMSG (LA_CH_LINE, 6, Error, LA_CH_COLUMN);

               /* If the label has any forward references, they must be       */
               /* abandoned.  They can't be processed because the statement   */
               /* type of the current statement is unknown.		      */
      
               if (CURR_BLK != Derived_Type_Blk) {
                  stmt_label_idx = srch_sym_tbl(TOKEN_STR(label_token),
			    	                TOKEN_LEN(label_token),
					  	&name_idx);

                  if (stmt_label_idx != NULL_IDX    &&
                      ! AT_DEFINED(stmt_label_idx)  &&
                      ATL_FWD_REF_IDX(stmt_label_idx) != NULL_IDX) {
                     AT_DCL_ERR(stmt_label_idx) = TRUE;
                     resolve_fwd_lbl_refs(); 
                  } 
               }
            }

            /* If a construct name exists, issue an error because a keyword   */
            /* or identifier must follow it.				      */
          
            if (stmt_construct_idx != NULL_IDX) {
               PRINTMSG (LA_CH_LINE, 6, Error, LA_CH_COLUMN);
               AT_DCL_ERR(stmt_construct_idx) = TRUE;
            }
	 }
         parse_err_flush(Find_EOS, NULL);
         NEXT_LA_CH;

         if (defer_msg > 1 && LA_CH_CLASS != Ch_Class_EOF) {
            PRINTMSG (AT_DEF_LINE(SCP_ATTR_IDX(curr_scp_idx)), defer_msg,

# if defined(_ERROR_DUPLICATE_GLOBALS)
                      Error,
# else
                      Warning,
# endif
                      AT_DEF_COLUMN(SCP_ATTR_IDX(curr_scp_idx)));
         }

      }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      if (stmt_type != Directive_Stmt) {
         directives_are_global = FALSE;
      }

      if (stmt_type != Directive_Stmt &&
          stmt_type != End_Parallel_Stmt &&
          stmt_type != End_Do_Parallel_Stmt &&
          stmt_type != End_Parallel_Case_Stmt &&
          stmt_type != Parallel_Case_Stmt &&
          stmt_type != End_Guard_Stmt &&
          stmt_type != Open_MP_Section_Stmt &&
          stmt_type != Open_MP_End_Parallel_Stmt &&
          stmt_type != Open_MP_End_Do_Stmt &&
          stmt_type != Open_MP_End_Parallel_Sections_Stmt &&
          stmt_type != Open_MP_End_Sections_Stmt &&
          stmt_type != Open_MP_End_Section_Stmt &&
          stmt_type != Open_MP_End_Single_Stmt &&
          stmt_type != Open_MP_End_Parallel_Do_Stmt &&
          stmt_type != Open_MP_End_Master_Stmt &&
          stmt_type != Open_MP_End_Critical_Stmt &&
          stmt_type != Open_MP_End_Ordered_Stmt &&
          stmt_type != SGI_Section_Stmt &&
          stmt_type != SGI_End_Psection_Stmt &&
          stmt_type != SGI_End_Pdo_Stmt &&
          stmt_type != SGI_End_Parallel_Stmt &&
          stmt_type != SGI_End_Critical_Section_Stmt &&
          stmt_type != SGI_End_Single_Process_Stmt &&
          stmt_type != SGI_Region_End_Stmt &&
          cdir_switches.inline_here_sgi) {

         /* generate an End_Inline_Here_Star_Opr stmt next */

         need_new_sh = TRUE;
      
         if (SH_IR_IDX(curr_stmt_sh_idx)) {
            /* maybe this should be gen_sh instead */

#ifdef KEY /* Bug 7498 */
      /* ntr_sh_tbl() may change the value of sh_tbl, which is used inside
       * SH_NEXT_IDX(), so we need an ANSI C sequence point in between. */
	    int new_stmt = ntr_sh_tbl();
            SH_NEXT_IDX(curr_stmt_sh_idx) = new_stmt;
#else /* KEY Bug 7498 */
            SH_NEXT_IDX(curr_stmt_sh_idx) = ntr_sh_tbl();
#endif /* KEY Bug 7498 */
            SH_PREV_IDX(SH_NEXT_IDX(curr_stmt_sh_idx)) = curr_stmt_sh_idx;
            curr_stmt_sh_idx  = SH_NEXT_IDX(curr_stmt_sh_idx);
            SH_STMT_TYPE(curr_stmt_sh_idx) = Directive_Stmt;
         }
      
         SH_GLB_LINE(curr_stmt_sh_idx)= stmt_start_line;
         SH_COL_NUM(curr_stmt_sh_idx) = stmt_start_col;

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)               = End_Inline_Here_Star_Opr;

         /* must have a type idx */

         IR_TYPE_IDX(ir_idx)          = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)          = stmt_start_line;
         IR_COL_NUM(ir_idx)           = stmt_start_col;

         SH_IR_IDX(curr_stmt_sh_idx)  = ir_idx;
         cdir_switches.inline_here_sgi = FALSE;
      }
# endif

      if (LA_CH_CLASS == Ch_Class_EOF) {		/* EOF following EOS  */
	 EOPU_encountered = TRUE;
      }
   }  /* while */

# if defined(_EXPRESSION_EVAL)
   
   /* Force an END statement.  This is   */
   /* for the expression evaluator only. */

   if (cmd_line_flags.expression_eval_stmt || 
       cmd_line_flags.expression_eval_expr) {

      stmt_type	= End_Stmt;

      if (need_new_sh) {
         sh_idx				= curr_stmt_sh_idx;
         curr_stmt_sh_idx		= ntr_sh_tbl();
         SH_NEXT_IDX(sh_idx)		= curr_stmt_sh_idx;
         SH_PREV_IDX(curr_stmt_sh_idx)	= sh_idx;
      }
      else {
         /* clear out the old one. */
         /* except for prev idx    */
         sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
         CLEAR_TBL_NTRY(sh_tbl, curr_stmt_sh_idx);
         SH_PREV_IDX(curr_stmt_sh_idx) = sh_idx;
      }

      expression_eval_end();
   }

# endif

   if (blk_stk_idx != NULL_IDX) {

      if (SCP_ATTR_IDX(curr_scp_idx) == glb_tbl_idx[Main_Attr_Idx] &&
          stmt_start_line == AT_DEF_LINE(glb_tbl_idx[Main_Attr_Idx]) &&
          !SCP_IN_ERR(curr_scp_idx) &&
          LA_CH_CLASS == Ch_Class_EOF) {

         /* This is just a trailing statement. */

         SCP_IN_ERR(curr_scp_idx)			= TRUE;
         AT_DCL_ERR(SCP_ATTR_IDX(curr_scp_idx))		= TRUE;
         PRINTMSG (stmt_start_line, 1581, Error, stmt_start_col);
      }

      /* Clear blk stack and issue missing end msgs for stack.  Also takes    */
      /* care of appropriate end of program unit processing.                  */

      clearing_blk_stk = TRUE;

      if (need_new_sh) {
         sh_idx				= curr_stmt_sh_idx;
         curr_stmt_sh_idx		= ntr_sh_tbl();
         SH_NEXT_IDX(sh_idx)		= curr_stmt_sh_idx;
         SH_PREV_IDX(curr_stmt_sh_idx)	= sh_idx;
      }
      else { /* clear out the old one  - except for prev idx  */
         sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
         CLEAR_TBL_NTRY(sh_tbl, curr_stmt_sh_idx);
         SH_PREV_IDX(curr_stmt_sh_idx)	= sh_idx;
         need_new_sh			= TRUE;
      }

      SH_ERR_FLG(curr_stmt_sh_idx)	= TRUE;

      pop_and_err_blk_stk(NULL_IDX, TRUE);

      clearing_blk_stk = FALSE;
   }

   /* The block stack table is not needed for the rest of compilation. */

   TBL_FREE(blk_stk);

   TRACE (Func_Exit, "parse_prog_unit", NULL);

   return;

}  /* parse_prog_unit */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Initialize for parsing of a program unit.			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void init_parse_prog_unit()

{
   int		 i;
   int		 idx;
   id_str_type	 name;
   int           name_idx;
   token_type    npes_token;
   int           npes_attr;
   int		 save_stmt_start_line;
   long		*type_tbl_base;
   long		*static_type_tbl_base;


   TRACE (Func_Entry, "init_parse_prog_unit", NULL);

   save_stmt_start_line	= stmt_start_line;
   stmt_start_line	= 1;


   /* This sets/resets the integer sizes back to the sizes specified on the   */
   /* commandline in case a CDIR INTEGER changed them.                        */

   set_integer_default_type();


   if (on_off_flags.recognize_minus_zero) {
      for (i = 0; i < MAX_INTRIN_MAP_SIZE; i++) {
          if ((strcmp("SIGN", (char *)&intrin_map[i].id_str) == 0) ||
              (strcmp("DSIGN", (char *)&intrin_map[i].id_str) == 0)) {
#ifdef KEY /* Bug 3405 */
             /* Strings are less than 8 bytes long */
             intrin_map[i].mapped_4.string[1] = 'I';
             intrin_map[i].mapped_8.string[1] = 'I';
#else
             intrin_map[i].mapped_4.string[8] = '_';
             intrin_map[i].mapped_8.string[8] = '_';
#endif /* KEY Bug 3405 */
          }
      }
   }


   /* ALLOCATE memory for tables that exist for the complete compilation.   */
   /* The third argument is what the tables index gets set to.  If the      */
   /* index is not set to NULL, either these entries in the table are       */
   /* filled in here, or it is used as a work area.                         */
   /* BOUNDS_LAST_USED_IDX -> The bounds table has the 0-7 entries reserved */
   /*    as a work area for adding new entries.                             */
   /* NAME_POOL_LAST_IDX -> The name pool has entries 0-2 reserved.  Entry  */
   /*    0 is not used.  1 and 2 are set to smallest and largest strings.   */

   CHECK_INITIAL_ALLOC (blk_stk,	NULL_IDX);

   CHECK_INITIAL_ALLOC (attr_list_tbl,	NULL_IDX);
   CHECK_INITIAL_ALLOC (attr_tbl,	NULL_IDX);
   CHECK_INITIAL_ALLOC (attr_aux_tbl,	NULL_IDX);
   CHECK_INITIAL_ALLOC (bounds_tbl,	BD_LAST_USED_IDX);
   CHECK_INITIAL_ALLOC (const_tbl,	NULL_IDX);
   CHECK_INITIAL_ALLOC (const_pool,	NULL_IDX);
   CHECK_INITIAL_ALLOC (sec_name_tbl,	NULL_IDX);
   CHECK_INITIAL_ALLOC (stor_blk_tbl,	NULL_IDX);
   CHECK_INITIAL_ALLOC (loc_name_tbl,	NULL_IDX);
   CHECK_INITIAL_ALLOC (hidden_name_tbl,NULL_IDX);
   CHECK_INITIAL_ALLOC (name_pool,	NP_LAST_USED_IDX);
   CHECK_INITIAL_ALLOC (scp_tbl,	NULL_IDX);
   CHECK_INITIAL_ALLOC (type_tbl,	TYP_LAST_USED_IDX);
   CHECK_INITIAL_ALLOC (equiv_tbl, 	NULL_IDX);

   CHECK_INITIAL_ALLOC (ir_tbl,		NULL_IDX);
   CHECK_INITIAL_ALLOC (sh_tbl,		NULL_IDX);
   CHECK_INITIAL_ALLOC (ir_list_tbl,	NULL_IDX);


   /* Clear the zero entry of the list tables, because this entry is   */
   /* used to keep the free list for the table.                        */

   CLEAR_TBL_NTRY(attr_list_tbl,	NULL_IDX);
   CLEAR_TBL_NTRY(ir_tbl,		NULL_IDX);
   CLEAR_TBL_NTRY(ir_list_tbl,		NULL_IDX);
   CLEAR_TBL_NTRY(sh_tbl,		NULL_IDX);
   CLEAR_TBL_NTRY(bounds_tbl,		NULL_IDX);


   /* Clear globals attribute indexes */

   for (idx = 0; idx < Num_Glb_Tbl_Idxs; idx++) {
      glb_tbl_idx[idx]	= 0;
   }

   init_target_opnd = null_opnd;

   type_tbl_base	= (long *) type_tbl;
   static_type_tbl_base	= (long *) type_init_tbl;

   for (idx = 0; idx < ((Num_Linear_Types+2) * NUM_TYP_WDS); idx++) {
      type_tbl_base[idx]	= static_type_tbl_base[idx];
   }


   /* Initialize the zeroth entry for all those oprs that do not type things.*/

   type_tbl[NULL_IDX]	= type_tbl[INTEGER_DEFAULT_TYPE];


   /* Initialize the search limits of the local name table.            */
   /* This consists of setting the initial entry, fw, to reference     */
   /* the first entry in the name pool and the second entry, lw, to    */
   /* reference the second entry in the name pool.  The first entry in */
   /* the name is a word of zeros, the second entry is a word of ones. */
   /* pool is a word of zeros.  These limits are needed in order to    */
   /* search and enter names into the local name table.		       */

   name_pool[0].name_long = 0;
   name_pool[1].name_long = 0;
   name_pool[2].name_long = LARGE_WORD_FOR_TBL_SRCH;

   curr_scp_idx = INTRINSIC_SCP_IDX;

   CLEAR_TBL_NTRY(scp_tbl, INTRINSIC_SCP_IDX);

   init_name_and_stor_tbls(INTRINSIC_SCP_IDX, FALSE);

   enter_intrinsic_info();   

   SCP_FIRST_CHILD_IDX(INTRINSIC_SCP_IDX)	= 1;
   SCP_NUM_CHILDREN(INTRINSIC_SCP_IDX)		= 1;

   NTR_SCP_TBL(curr_scp_idx);

   SCP_PARENT_IDX(curr_scp_idx) = 0;


   /* Init first 2 enteries in loc_name_table and stor_blk_tbl. */

   init_name_and_stor_tbls(curr_scp_idx, TRUE);


   /* Put $MAIN into the string pool and the attr table, but do not put it  */
   /* into the local name table, unless it is necessary.   If $MAIN is      */
   /* needed because of a missing PROGRAM statement, it is entered into the */
   /* local name table by p_driver.                                         */

   NTR_ATTR_TBL(glb_tbl_idx[Main_Attr_Idx]);
   AT_NAME_LEN(glb_tbl_idx[Main_Attr_Idx])	= 5;
   AT_NAME_IDX(glb_tbl_idx[Main_Attr_Idx])	= name_pool_idx + 1;
   AT_DEF_LINE(glb_tbl_idx[Main_Attr_Idx])	= curr_glb_line;
   AT_DEF_COLUMN(glb_tbl_idx[Main_Attr_Idx])	= 1;
   AT_OBJ_CLASS(glb_tbl_idx[Main_Attr_Idx])	= Pgm_Unit;
   ATP_EXT_NAME_LEN(glb_tbl_idx[Main_Attr_Idx])	= 5;
   ATP_EXT_NAME_IDX(glb_tbl_idx[Main_Attr_Idx])	= name_pool_idx + 1;
   ATP_PGM_UNIT(glb_tbl_idx[Main_Attr_Idx])	= Program;
   ATP_SCP_IDX(glb_tbl_idx[Main_Attr_Idx])	= curr_scp_idx;
   ATP_EXPL_ITRFC(glb_tbl_idx[Main_Attr_Idx])	= TRUE;

   CREATE_ID(name, UNNAMED_PROGRAM_NAME, UNNAMED_PROGRAM_NAME_LEN);
   NTR_NAME_POOL(&(name.words[0]), UNNAMED_PROGRAM_NAME_LEN, idx);

   SCP_ATTR_IDX(curr_scp_idx)		= glb_tbl_idx[Main_Attr_Idx];

   PUSH_BLK_STK(Program_Blk);
   SCP_IMPL_NONE(curr_scp_idx)		= FALSE;
   SCP_PARENT_NONE(curr_scp_idx)	= FALSE;

   for (idx = 0; idx < MAX_IMPL_CHS; idx++) {
       IM_TYPE_IDX(curr_scp_idx, idx)	= REAL_DEFAULT_TYPE;
       IM_SET(curr_scp_idx, idx)	= FALSE;
   }

   for (idx = IMPL_IDX('I'); idx <= IMPL_IDX('N'); idx++) {
       IM_TYPE_IDX(curr_scp_idx, idx)	= INTEGER_DEFAULT_TYPE;
   }

   init_const_tbl();

   /* Enter N$PES into symbol table */

   CREATE_ID(TOKEN_ID(npes_token), "N$PES", 5);

   TOKEN_COLUMN(npes_token)	= 1;
   TOKEN_LEN(npes_token)	= 5;
   TOKEN_LINE(npes_token)	= curr_glb_line;
   npes_attr			= srch_sym_tbl(TOKEN_STR(npes_token),
                                               TOKEN_LEN(npes_token),
                                               &name_idx);
   npes_attr				= ntr_sym_tbl(&npes_token,name_idx);
   LN_DEF_LOC(name_idx)			= TRUE;
   AT_OBJ_CLASS(npes_attr)		= Data_Obj;
   AT_COMPILER_GEND(npes_attr)		= TRUE;
   AT_SEMANTICS_DONE(npes_attr)		= TRUE;
   ATD_SYMBOLIC_CONSTANT(npes_attr)	= TRUE;
   ATD_TYPE_IDX(npes_attr)		= CG_INTEGER_DEFAULT_TYPE;

   /* Enter value for N$PES into the constant table and attr entry.  The  */
   /* value will be 0 if not entered on the command line; this will mean  */
   /* that the value for N$PES will be supplied at link time or run time. */
   /* On non MPP platforms, num_pes will default to 1 if not specified.   */

   if (cmd_line_flags.MPP_num_pes == 0) {
      ATD_CLASS(npes_attr)	= Variable;
   }
   else {
      ATD_CLASS(npes_attr)	= Constant;
      AT_DEFINED(npes_attr)	= TRUE;
      ATD_FLD(npes_attr)        = CN_Tbl_Idx;
      ATD_CONST_IDX(npes_attr)  = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              cmd_line_flags.MPP_num_pes);
   }

   const_safevl_idx		= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              target_safevl);

   /* Initialize the Type table for CHARACTER*(1) for all possible defaults.  */

   TYP_IDX(Character_1)	= CN_INTEGER_ONE_IDX;
   TYP_FLD(Character_1)	= CN_Tbl_Idx;
   TYP_IDX(Character_2)	= CN_INTEGER_ONE_IDX;
   TYP_FLD(Character_1)	= CN_Tbl_Idx;
   TYP_IDX(Character_4)	= CN_INTEGER_ONE_IDX;
   TYP_FLD(Character_4)	= CN_Tbl_Idx;


  /* Now that the TYP table has been set up, TYP_LINEAR and                   */
  /* INTEGER_DEFAULT_TYPE can be referenced.				      */

# ifdef _TARGET_OS_SOLARIS

   if (TYP_LINEAR(INTEGER_DEFAULT_TYPE) == Integer_8) {
      storage_bit_size_tbl[CRI_Ptr_8] = 64;
   }

# endif


   /* Initialize the bounds table for deferred shape arrays */

   for (idx = BD_DEFERRED_1_IDX; idx <= BD_DEFERRED_7_IDX; idx++) {
      CLEAR_TBL_NTRY(bounds_tbl, idx);
      BD_ARRAY_CLASS(idx)	= Deferred_Shape;
      BD_RANK(idx)		= idx;
      BD_USED_NTRY(idx)		= TRUE;
      BD_NTRY_SIZE(idx)		= 1;
      BD_GLOBAL_IDX(idx)	= idx;
   }

   PRINT_INTRIN;	/* If -u intrin and DEBUG compiler, print intrin tbl */

   EOPU_encountered	= FALSE;
   curr_stmt_category	= Init_Stmt_Cat;
   curr_internal_lbl	= 0;
   curr_debug_lbl	= 0;
   if_stmt_lbl_idx	= NULL_IDX;

   cif_prog_unit_init();            /* Also needed for buffered message file. */

   curr_stmt_sh_idx			= ntr_sh_tbl();
   SH_STMT_TYPE(curr_stmt_sh_idx)	= Null_Stmt;
   SCP_FIRST_SH_IDX(curr_scp_idx)	= curr_stmt_sh_idx;
   CURR_BLK_FIRST_SH_IDX		= curr_stmt_sh_idx;
   need_new_sh				= FALSE;

   /* initialize the cdir stuff */

   init_directive (1);
   stmt_start_line			= save_stmt_start_line;

   TRACE (Func_Exit, "init_parse_prog_unit", NULL);

   return;

}  /* init_parse_prog_unit */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	If the current statement is prefixed by a statement label and/or a    *|
|*      construct name, this routine captures the statement label token and/or*|
|*      processes the construct name.  This routine exits without fetching    *|
|*      the first token of the statement.	      			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void ck_lbl_construct_name(void)

{
   int         ir_idx;
   int         name_idx;
   int         sh_idx;


   TRACE (Func_Entry, "ck_lbl_construct_name", NULL);

   stmt_label_idx     = NULL_IDX;
   stmt_construct_idx = NULL_IDX;

   /* An invalid label (e.g. 000) is caught by the scanner which emits a      */
   /* message and leaves the input stream so that the next call to get_token  */
   /* produces the token following the invalid label.                         */

   if (label_ok &&
       MATCHED_TOKEN_CLASS(Tok_Class_Label) &&
       ! TOKEN_ERR(token)) {

      /* Because a label in a derived type definition (including the TYPE and */
      /* END TYPE statements) belongs to the derived type scope (but we don't */
      /* build a SCP entry to represent a derived type definition scoping     */
      /* unit), we can't decide what to do with a label until after the       */
      /* statement is parsed (so we know what kind of a statement we have) so */
      /* just hold on to the token for now (and set stmt_label_idx to a       */
      /* nonzero value so it looks like a label exists).  More label          */
      /* processing is done by gen_attr_and_IR_for_lbl after the statement    */
      /* type has been determined and some final label processing is          */
      /* completed after the statement has been parsed (see the code          */
      /* following the call to stmt_parsers).				      */

      label_token    = token;
      stmt_label_idx = -911;
   }
          
   if (MATCHED_TOKEN_CLASS(Tok_Class_Construct_Def)) {
 
      /* If match - token = construct name - the : following is consumed */

      stmt_construct_idx = srch_sym_tbl(TOKEN_STR(token),
                                        TOKEN_LEN(token),
                                        &name_idx);

      if (stmt_construct_idx == NULL_IDX || 
          AT_OBJ_CLASS(stmt_construct_idx) != Label) {

         if (stmt_construct_idx == NULL_IDX) {
            stmt_construct_idx = ntr_sym_tbl(&token, name_idx);
         }
         else {
            fnd_semantic_err(Obj_Construct,
                             TOKEN_LINE(token),
                             TOKEN_COLUMN(token),
                             stmt_construct_idx,
                             TRUE);
            CREATE_ERR_ATTR(stmt_construct_idx, TOKEN_LINE(token),
                            TOKEN_COLUMN(token), Label);
         }

         LN_DEF_LOC(name_idx)			= TRUE;
         AT_OBJ_CLASS(stmt_construct_idx) 	= Label;
         AT_DEFINED(stmt_construct_idx)	  	= TRUE;
         AT_DEF_LINE(stmt_construct_idx)	= TOKEN_LINE(token);
         ATL_DEBUG_CLASS(stmt_construct_idx)	= Ldbg_Construct_Name;

         gen_sh(Before, Construct_Def, TOKEN_LINE(token), TOKEN_COLUMN(token),
                FALSE, FALSE, FALSE);

         sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         if (SCP_FIRST_SH_IDX(curr_scp_idx) == curr_stmt_sh_idx) {
            SCP_FIRST_SH_IDX(curr_scp_idx) = sh_idx;
         }

         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(sh_idx)     = ir_idx;
         IR_OPR(ir_idx)        = Label_Opr;
         IR_TYPE_IDX(ir_idx)   = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)   = TOKEN_LINE(token); 
         IR_COL_NUM(ir_idx)    = TOKEN_COLUMN(token);
         IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
         IR_COL_NUM_L(ir_idx)  = TOKEN_COLUMN(token);
         IR_FLD_L(ir_idx)      = AT_Tbl_Idx;
         IR_IDX_L(ir_idx)      = stmt_construct_idx;
         IR_FLD_R(ir_idx)      = SH_Tbl_Idx;
         IR_IDX_R(ir_idx)      = curr_stmt_sh_idx;

         if (cif_flags & XREF_RECS) {
            cif_usage_rec(stmt_construct_idx, AT_Tbl_Idx,
                          TOKEN_LINE(token), TOKEN_COLUMN(token),
                          CIF_Symbol_Declaration);
         }

      }
      else {
         AT_DCL_ERR(stmt_construct_idx) = TRUE;
         PRINTMSG(TOKEN_LINE(token), 171, Error, TOKEN_COLUMN(token),
                  AT_OBJ_NAME_PTR(stmt_construct_idx),
                  AT_DEF_LINE(stmt_construct_idx));
      }
   }

   TRACE (Func_Exit, "ck_lbl_construct_name", NULL);

   return;

}  /* ck_lbl_construct_name */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      check_stmt_type is						      *|
|*        TRUE :  If the stmt is a type declaration or type definition stmt,  *|
|*                or END (TYPE) stmt, it's context is ambiguous, so wait      *|
|*                until a later call to process the label.  Otherwise, search *|
|*                for the Attr, do conflict checking, etc. now.               *|
|*        FALSE:  A stmt type that was previously ambiguous is now known to   *|
|*                something outside of a derived type definition so we can at *|
|*                last check it for conflicts, generate its Attr entry, and   *|
|*                the Label_Def IR.					      *|
|*									      *|
|* Input parameters:							      *|
|*	check_stmt_type : see above explanation				      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void gen_attr_and_IR_for_lbl(boolean	check_stmt_type)

{
   int		ir_idx;
   int		name_idx;
   int		sh_idx;


   TRACE (Func_Entry, "gen_attr_and_IR_for_lbl", NULL);

   if (check_stmt_type  &&
       (stmt_type == Type_Decl_Stmt  ||  
        stmt_type == Function_Stmt  ||  
        stmt_type == Recursive_Stmt  ||  
        stmt_type == Subroutine_Stmt  ||  
        stmt_type == Pure_Stmt  ||  
        stmt_type == Elemental_Stmt  ||  
        stmt_type == End_Stmt)) {
      goto EXIT;
   }

   stmt_label_idx = srch_sym_tbl(TOKEN_STR(label_token),
                                 TOKEN_LEN(label_token),
                                 &name_idx);

   if (stmt_label_idx == NULL_IDX) {
      stmt_label_idx                  = ntr_sym_tbl(&label_token, name_idx);
      AT_OBJ_CLASS(stmt_label_idx)    = Label;
      ATL_DEBUG_CLASS(stmt_label_idx) = Ldbg_User_Lbl;

      if (! check_stmt_type) {
         ATL_CLASS(stmt_label_idx) = Lbl_User; 
      }

      LN_DEF_LOC(name_idx) = TRUE;
   }

   if (AT_DEFINED(stmt_label_idx)) {
      PRINTMSG(TOKEN_LINE(label_token), 146, Error,
               TOKEN_COLUMN(label_token),
               AT_OBJ_NAME_PTR(stmt_label_idx),
               AT_DEF_LINE(stmt_label_idx));
   }
   else {

      /* If there are forward references to the label, AT_DEFINED and         */
      /* ATL_DEF_STMT_IDX will be set after the forward refs are processed.   */

      if (ATL_FWD_REF_IDX(stmt_label_idx) == NULL_IDX) {
         AT_DEFINED(stmt_label_idx)       = TRUE;
         ATL_DEF_STMT_IDX(stmt_label_idx) = curr_stmt_sh_idx;
      }

      if (! cdir_switches.vector) {
         ATL_NOVECTOR(stmt_label_idx)  = TRUE;
      }

      AT_DEF_LINE(stmt_label_idx)  = TOKEN_LINE(label_token);
      SH_LABELED(curr_stmt_sh_idx) = TRUE;

      gen_sh(Before, Label_Def,
             TOKEN_LINE(label_token), TOKEN_COLUMN(label_token),
             FALSE, FALSE, FALSE);

      sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      if (SCP_FIRST_SH_IDX(curr_scp_idx) == curr_stmt_sh_idx) {
         SCP_FIRST_SH_IDX(curr_scp_idx) = sh_idx;
      }

      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(sh_idx)     = ir_idx;
      IR_OPR(ir_idx)        = Label_Opr;
      IR_TYPE_IDX(ir_idx)   = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)   = TOKEN_LINE(label_token);
      IR_COL_NUM(ir_idx)    = TOKEN_COLUMN(label_token);
      IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(label_token);
      IR_COL_NUM_L(ir_idx)  = TOKEN_COLUMN(label_token);
      IR_FLD_L(ir_idx)      = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)      = stmt_label_idx;
      IR_FLD_R(ir_idx)      = SH_Tbl_Idx;
      IR_IDX_R(ir_idx)      = curr_stmt_sh_idx;
         
      if (cif_flags & XREF_RECS) {
         cif_usage_rec(stmt_label_idx, AT_Tbl_Idx,
                       TOKEN_LINE(label_token), TOKEN_COLUMN(label_token),
                       CIF_Symbol_Declaration);
      }

   }

EXIT:

   TRACE (Func_Exit, "gen_attr_and_IR_for_lbl", NULL);

   return;

}  /* gen_attr_and_IR_for_lbl */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Based on the keyword-form token just obtained, determine the stmt     *|
|*	type.  This code exists as a separate procedure because it's also     *|
|*      needed by logical IF stmt processing to determine the stmt type of    *|
|*      stmt following the IF condition.				      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

void determine_stmt_type(void)

{
   int		buf_idx;
   int		stmt_num;

   TRACE (Func_Entry, "determine_stmt_type", NULL);
   stmt_start_line = TOKEN_LINE(token);
   stmt_start_col  = TOKEN_COLUMN(token);
   buf_idx	   = TOKEN_BUF_IDX(token);
   stmt_num        = TOKEN_STMT_NUM(token);
   stmt_type 	   = token_to_stmt_type [TOKEN_VALUE(token)];

   SH_GLB_LINE(curr_stmt_sh_idx) = stmt_start_line;
   SH_COL_NUM(curr_stmt_sh_idx)  = stmt_start_col;

#ifdef KEY /* Bug 1317 */
   /* Something like "10 format(i2) = 3.14159" is not a format statement */
#else
   if (stmt_type == Format_Stmt &&
       stmt_label_idx           &&
       LA_CH_VALUE == LPAREN)   {

      /* if it is label "format" "(" then it is a format stmt. */
      /* whether you like it or not.                           */
      /* intentionally blank.                                  */
   }
   else
#endif /* KEY Bug 1317 */
   if (stmt_type != Assignment_Stmt &&
            stmt_type != Directive_Stmt &&
            stmt_type != End_Parallel_Stmt &&
            stmt_type != End_Do_Parallel_Stmt &&
            stmt_type != End_Parallel_Case_Stmt &&
            stmt_type != Parallel_Case_Stmt &&
            stmt_type != End_Guard_Stmt &&
            stmt_type != Open_MP_Section_Stmt &&
            stmt_type != Open_MP_End_Parallel_Stmt &&
            stmt_type != Open_MP_End_Do_Stmt &&
            stmt_type != Open_MP_End_Parallel_Sections_Stmt &&
            stmt_type != Open_MP_End_Sections_Stmt &&
            stmt_type != Open_MP_End_Section_Stmt &&
            stmt_type != Open_MP_End_Single_Stmt &&
            stmt_type != Open_MP_End_Parallel_Do_Stmt &&
            stmt_type != Open_MP_End_Master_Stmt &&
            stmt_type != Open_MP_End_Critical_Stmt &&
            stmt_type != Open_MP_End_Ordered_Stmt &&
            stmt_type != SGI_Section_Stmt &&
            stmt_type != SGI_End_Psection_Stmt &&
            stmt_type != SGI_End_Pdo_Stmt &&
            stmt_type != SGI_End_Parallel_Stmt &&
            stmt_type != SGI_End_Critical_Section_Stmt &&
            stmt_type != SGI_End_Single_Process_Stmt &&
            stmt_type != SGI_Region_End_Stmt) {

      if (TOKEN_VALUE(token) == Tok_Kwd_Double  &&  ! set_stmt_type_known()) {

         /* Kludge to handle the fixed form case of DOUBLE */
         /* possibly being a DO stmt (e.g. DO uble = ...)  */

         reset_lex (buf_idx, stmt_num);
         MATCHED_TOKEN_CLASS (Tok_Class_DO);
         stmt_type = Do_Iterative_Stmt;
      }

      if (stmt_type == Do_Iterative_Stmt) {

         if (! set_stmt_type_known() ) {

            if (! stmt_is_DO_stmt () ) {
               stmt_type = Assignment_Stmt;
            }
         }
      }
      else if (stmt_type == Data_Stmt) {

          if ( ! stmt_is_DATA_stmt () ) {
             stmt_type = Assignment_Stmt;
           }
      }
      else if (! set_stmt_type_known() ) {
         stmt_type = Assignment_Stmt;
      }
   }

   if (stmt_type == Assignment_Stmt) {

      if (TOKEN_VALUE(token) != Tok_Id) {

         /* The token is a keyword but is NOT a beginning of stmt keyword     */
         /* and therefore the first token must be reinterpreted as an id so   */
	 /* that fixed form stmts get parsed right (e.g. for "INDEX = 0" we   */
         /* have keyword IN at this point and it needs to be id INDEX).       */
	 /* NOTE:  the backup col and line don't have to be reset.	      */

	 reset_lex (buf_idx, stmt_num);
	 MATCHED_TOKEN_CLASS(Tok_Class_Id);
      }
   }

   /* Fill in rest of stmt header.                                            */

   SH_STMT_TYPE(curr_stmt_sh_idx) = stmt_type;

   /* need_new_sh = TRUE will cause a new SH to be generated for the next     */
   /* statement.  If this statement is a declarative statement and doesn't    */
   /* have a label, the new SH can be reused for the next statement, so       */
   /* need_new_sh is set to FALSE.		                              */

   need_new_sh = TRUE;


   /* If "other" CIF records were requested, call cif_stmt_type_rec to        */
   /* perhaps output a Statement Type record (it will only be output if the   */
   /* statement can be exactly determined by this procedure; otherwise, the   */
   /* specific parsing code will call cif_stmt_type_rec again with the exact  */
   /* statement type.							      */

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(FALSE, CIF_Not_Exact, statement_number);
   }

   TRACE (Func_Exit, "determine_stmt_type", NULL);

   return;

}  /* determine_stmt_type */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      This routine checks thru the block stack to see if there really is    *|
|*      a block or context error.  NOTE:  The CYCLE, END, and EXIT stmts      *|
|*      handle there own stuff.    					      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if there really is a block or context error.  FALSE if we are in *|
|*	     an error recovery situation and the error shouldn't be issued.   *|
|*									      *|
\******************************************************************************/
boolean iss_blk_stk_err(void)

{
   int			blk_idx;
   int			err_msg;
   boolean		iss_msg;


   TRACE (Func_Entry, "iss_blk_stk_err", NULL);
   
   /* If the action-stmt of a logical IF is being parsed, don't issue a block */
   /* stack error for the action-stmt (it's already been issued for the IF    */
   /* statement).							      */

   if (if_stmt_lbl_idx != NULL_IDX) {
      iss_msg = FALSE;
      goto EXIT;
   }

   err_msg	= 5;		/* Default to stmt out of order msg */
   iss_msg	= TRUE;		/* Assume msg will be issued        */

   if (STMT_CANT_BE_IN_BLK(stmt_type, CURR_BLK)) {

      for (blk_idx = blk_stk_idx;

           /* Keep going while blocks still left on stack, they are in  */
           /* error, and the stmt doesn't fit into the block.           */

           blk_idx > NULL_IDX &&
           BLK_ERR(blk_idx) == TRUE &&
           STMT_CANT_BE_IN_BLK(stmt_type, BLK_TYPE(blk_idx));

           blk_idx--);

      if (blk_idx > NULL_IDX) {
         err_msg = (STMT_CANT_BE_IN_BLK(stmt_type, BLK_TYPE(blk_idx))) ?
                    blk_err_msgs[BLK_TYPE(blk_idx)] : FALSE;
      }
   }

   switch (stmt_type) {

      case Blockdata_Stmt:
      case Module_Stmt:
      case Program_Stmt:
         /* Pop all blocks on stack and issue errors for all unclosed blocks. */
         pop_and_err_blk_stk(NULL_IDX, TRUE);
         init_parse_prog_unit();
         err_msg = 0;
         iss_msg = FALSE;
         break;

      case Function_Stmt: 
      case Subroutine_Stmt:
         for (blk_idx = blk_stk_idx;
              (blk_idx > NULL_IDX &&
               BLK_TYPE(blk_idx) != Interface_Blk &&
               BLK_TYPE(blk_idx) != Contains_Blk);
              blk_idx--);

         /* Pop back to the first Interface_Blk or Contains_Blk found.  If */
         /* neither is on the stack, pop the complete stack.  Issue errors */
          
         pop_and_err_blk_stk(blk_idx, TRUE);

         if (blk_idx == NULL_IDX) {
            /* Need to generate some cif records */

            if (cif_flags & BASIC_RECS) {
               cif_send_sytb();
            }
            init_parse_prog_unit();
         }

         err_msg = 0;
         iss_msg = FALSE;
         break;

      case Return_Stmt:
         if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Function &&
             ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Subroutine) {

            switch (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx))) {
               case Module:
                  err_msg = 19;
                  break;

               case Blockdata:
                  err_msg = 15;
                  break;

               case Program:
                  err_msg = 16;
                  break;
# ifdef _DEBUG
               default:
                  PRINTMSG(stmt_start_line, 179, Internal,
                           stmt_start_col, "iss_blk_stk_err");
                  break;
# endif
            }  /* End switch */
         }
         break;

      case Private_Stmt:
      case Public_Stmt:

         if (STMT_LEGAL_IN_BLK(stmt_type, CURR_BLK)) {

            if (STMT_CANT_BE_IN_BLK(stmt_type, BLK_TYPE(BLK_HEAD_IDX))) {
               err_msg	= blk_err_msgs[BLK_TYPE(BLK_HEAD_IDX)];
               iss_msg	= TRUE;
            }
         }
         break;

      default:
         break;

   }  /* End switch */

   if (iss_msg && err_msg != 0) {
      PRINTMSG(stmt_start_line, err_msg, Error, stmt_start_col, 
               stmt_type_str[stmt_type]);

      if (err_msg != 5) {	/* Block error */
         SCP_IN_ERR(curr_scp_idx)	= TRUE;
      }
   }

EXIT:

   TRACE (Func_Exit, "iss_blk_stk_err", NULL);

   return(iss_msg);

}  /* iss_blk_stk_err */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This is a Debug routine that issues an internal error if the parse    *|
|*	driver ends up calling this.                                          *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_bad_stmt(void)

{
   TRACE (Func_Entry, "parse_bad_stmt", NULL);

   PRINTMSG(TOKEN_LINE(token), 141, Internal, TOKEN_COLUMN(token));

   TRACE (Func_Exit, "parse_bad_stmt", NULL);

   return;

}  /* parse_bad_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Check to see if a label has been duplicated within a derived type     *|
|*	definition.							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static	void check_for_dup_derived_type_lbl(void)
{
   int		al_idx;
   int		lbl_list_idx;
   int		np_idx;


   TRACE (Func_Entry, "check_for_dup_derived_type_lbl", NULL);

   if (CURR_BLK_NAME == NULL_IDX || AT_DCL_ERR(CURR_BLK_NAME) || CURR_BLK_ERR) {

      /* In error situation - clear label and return */

      stmt_label_idx	= NULL_IDX;
      return;
   }

   lbl_list_idx = (stmt_type == End_Type_Stmt) ?
                     ATT_LABEL_LIST_IDX(BLK_NAME(blk_stk_idx + 1)) :
                     ATT_LABEL_LIST_IDX(CURR_BLK_NAME);

   while (lbl_list_idx != NULL_IDX  &&
          ! EQUAL_STRS(TOKEN_STR(label_token),
                       AT_OBJ_NAME_PTR(AL_ATTR_IDX(lbl_list_idx)))) {
      lbl_list_idx = AL_NEXT_IDX(lbl_list_idx);
   }

   if (lbl_list_idx == NULL_IDX) {
      NTR_ATTR_TBL(stmt_label_idx);
      AT_DEF_COLUMN(stmt_label_idx) = TOKEN_COLUMN(label_token);
      AT_DEF_LINE(stmt_label_idx)   = TOKEN_LINE(label_token);

      NTR_NAME_POOL(TOKEN_ID(label_token).words,
                    TOKEN_LEN(label_token),
                    np_idx);

      AT_NAME_IDX(stmt_label_idx)      = np_idx;
      AT_NAME_LEN(stmt_label_idx)      = TOKEN_LEN(label_token);
      AT_OBJ_CLASS(stmt_label_idx)     = Label;
      AT_DEFINED(stmt_label_idx)       = TRUE;
      ATL_CLASS(stmt_label_idx)        = Lbl_User;
      ATL_DEF_STMT_IDX(stmt_label_idx) = curr_stmt_sh_idx;
      ATL_DEBUG_CLASS(stmt_label_idx)  = Ldbg_User_Lbl;

      NTR_ATTR_LIST_TBL(al_idx);
      AL_ATTR_IDX(al_idx) = stmt_label_idx;
      AL_NEXT_IDX(al_idx) = ATT_LABEL_LIST_IDX(CURR_BLK_NAME);
      ATT_LABEL_LIST_IDX(CURR_BLK_NAME) = al_idx;
   
      if (cif_flags & INFO_RECS) {
         cif_label_rec(stmt_label_idx); 
      }

      if (cif_flags & XREF_RECS) {
         cif_usage_rec(stmt_label_idx,
                       AT_Tbl_Idx,
                       AT_DEF_LINE(stmt_label_idx),
                       AT_DEF_COLUMN(stmt_label_idx),
                       CIF_Symbol_Declaration);
      }
   }
   else {
      PRINTMSG(TOKEN_LINE(label_token), 146, Error,
               TOKEN_COLUMN(label_token),
               TOKEN_STR(label_token),
               AT_DEF_LINE(AL_ATTR_IDX(lbl_list_idx)));
      stmt_label_idx = AL_ATTR_IDX(lbl_list_idx);
   }

   TRACE (Func_Exit, "check_for_dup_derived_type_lbl", NULL);

   return;

}  /* check_for_dup_derived_type_lbl */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This sets the initial default type values based on commandline        *|
|*	options.  This is called once per compilation.                        *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

extern	void init_type(void)

{
   linear_type_type	dp_linear_type;

   TRACE (Func_Entry, "init_type", NULL);

   set_integer_default_type();

   /* Set the correct linear type for the -dp type table entry. */

   dp_linear_type = half_linear_type[Fortran_Double];

   type_init_tbl[DOUBLE_PRECISION_TYPE_IDX].fld.linear_type =
                          (cmd_line_flags.s_doubleprecision16) ? Real_16 :
                          init_default_linear_type[Fortran_Double];

   type_init_tbl[DOUBLE_COMPLEX_TYPE_IDX].fld.linear_type =
                          (cmd_line_flags.s_doublecomplex16) ? Complex_16:
                          init_default_linear_type[Fortran_Double_Complex];

   LOGICAL_DEFAULT_TYPE	= (cmd_line_flags.s_logical8) ? Logical_8 :
                          init_default_linear_type[Fortran_Logical];
   REAL_DEFAULT_TYPE	= (cmd_line_flags.s_real8) ? Real_8 :
                          init_default_linear_type[Fortran_Real];
   COMPLEX_DEFAULT_TYPE	= (cmd_line_flags.s_complex8) ? Complex_8 :
                          init_default_linear_type[Fortran_Complex];

   CHARACTER_DEFAULT_TYPE      = init_default_linear_type[Fortran_Character];

# if defined(_ACCEPT_CMD_s_32)

   if (cmd_line_flags.s_default32) {
      CHARACTER_DEFAULT_TYPE	  = half_linear_type[Fortran_Character];
      COMPLEX_DEFAULT_TYPE	  = half_linear_type[Fortran_Complex];
      LOGICAL_DEFAULT_TYPE	  = half_linear_type[Fortran_Logical];
      REAL_DEFAULT_TYPE		  = half_linear_type[Fortran_Real];

      type_init_tbl[DOUBLE_PRECISION_TYPE_IDX].fld.linear_type =
                                    half_linear_type[Fortran_Double];

      type_init_tbl[DOUBLE_COMPLEX_TYPE_IDX].fld.linear_type =
                                    half_linear_type[Fortran_Double_Complex];


# ifdef _TARGET_OS_MAX
      dp_linear_type = half_linear_type[Fortran_Real];
# endif

   }

# endif


# if defined(_TARGET32) || defined(_WHIRL_HOST64_TARGET64) || (defined(_HOST32) && defined(_TARGET64))

   if (cmd_line_flags.s_default64) {
      CHARACTER_DEFAULT_TYPE	  = double_linear_type[Fortran_Character];
      COMPLEX_DEFAULT_TYPE	  = double_linear_type[Fortran_Complex];
      LOGICAL_DEFAULT_TYPE	  = double_linear_type[Fortran_Logical];
      REAL_DEFAULT_TYPE		  = double_linear_type[Fortran_Real];
// Bug 2238
# ifdef KEY
      type_init_tbl[DOUBLE_PRECISION_TYPE_IDX].fld.linear_type =
                                    init_default_linear_type[Fortran_Double];
# else
      type_init_tbl[DOUBLE_PRECISION_TYPE_IDX].fld.linear_type =
                                    double_linear_type[Fortran_Double];
# endif

      type_init_tbl[DOUBLE_COMPLEX_TYPE_IDX].fld.linear_type =
                                    double_linear_type[Fortran_Double_Complex];
   }
   else if (cmd_line_flags.s_float64) {
      CHARACTER_DEFAULT_TYPE	= init_default_linear_type[Fortran_Character];
      LOGICAL_DEFAULT_TYPE	= init_default_linear_type[Fortran_Logical];
      REAL_DEFAULT_TYPE		= double_linear_type[Fortran_Real];
      COMPLEX_DEFAULT_TYPE	= double_linear_type[Fortran_Complex];

      type_init_tbl[DOUBLE_PRECISION_TYPE_IDX].fld.linear_type =
                                  double_linear_type[Fortran_Double];

      type_init_tbl[DOUBLE_COMPLEX_TYPE_IDX].fld.linear_type =
                                  double_linear_type[Fortran_Double_Complex];
   }

# endif

   if (!on_off_flags.enable_double_precision) {
      type_init_tbl[DOUBLE_PRECISION_TYPE_IDX].fld.linear_type = dp_linear_type;

      type_init_tbl[DOUBLE_COMPLEX_TYPE_IDX].fld.linear_type =
          COMPLEX_DEFAULT_TYPE;

      type_init_tbl[DOUBLE_PRECISION_TYPE_IDX].fld.dp_hit_me = TRUE;
      type_init_tbl[DOUBLE_COMPLEX_TYPE_IDX].fld.dp_hit_me = TRUE;
   }


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

   if (cmd_line_flags.s_pointer8) {
      storage_bit_size_tbl[CRI_Ptr_8] = 64;
      storage_bit_size_tbl[CRI_Ch_Ptr_8] = 128;
      storage_bit_size_tbl[CRI_Parcel_Ptr_8] = 64;
   }

# endif


   TRACE (Func_Exit, "init_type", NULL);

   return;

}  /* init_type */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This sets the initial default type values based on commandline        *|
|*	options.  This is called once per compilation.                        *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static	void set_integer_default_type(void)

{
   TRACE (Func_Entry, "set_integer_default_type", NULL);


   /* Set the correct linear type for integer from commandline. */

   if (cmd_line_flags.integer_32) {
      INTEGER_DEFAULT_TYPE = Integer_4;
   }
   else {
# if defined(_TARGET32) || defined(_WHIRL_HOST64_TARGET64) || (defined(_HOST32) && defined(_TARGET64))
      INTEGER_DEFAULT_TYPE = Integer_4;
# else
      INTEGER_DEFAULT_TYPE = Integer_8;
# endif
   }

   if (cmd_line_flags.s_integer8) {
      INTEGER_DEFAULT_TYPE = Integer_8;
   }


# if defined(_ACCEPT_CMD_s_32)
   if (cmd_line_flags.s_default32) {
      INTEGER_DEFAULT_TYPE = half_linear_type[Fortran_Integer];
   }
# endif

# if defined(_TARGET32)
   if (cmd_line_flags.s_default64) {
      INTEGER_DEFAULT_TYPE = double_linear_type[Fortran_Integer];
   }
# endif

   TRACE (Func_Exit, "set_integer_default_type", NULL);

   return;

}  /* set_integer_default_type */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*	Set up the Constant table, the Constant Search table, and the         *|
|*	Constant Search Index table.					      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

static void init_const_tbl(void)
{
   int                     idx;


   TRACE (Func_Entry, "init_const_tbl", NULL);

/* IF YOU WANT TO PREDEFINE A NEW CONSTANT YOU MUST ADD IT TO BOTH OF */
/* THE FOLLOWING IFDEF SECTIONS IN THE APPROPRIATE WAY.               */


   for (idx = 0; idx < Num_Linear_Types; idx++) {
      cn_root_idx[idx] = 0;
   }

   /* Initialize first 6 entries of constant table.  These are used as        */
   /* follows:                                                                */
   /*                                                                         */
   /*   CN_INTEGER_ZERO_IDX             entry  1                              */
   /*   CN_INTEGER_ONE_IDX              entry  2                              */
   /*   CN_INTEGER_TWO_IDX              entry  3                              */
   /*   CN_INTEGER_THREE_IDX            entry  4                              */
   /*   CN_INTEGER_NEG_ONE_IDX          entry  5                              */
   /*   CN_INTEGER_BITS_PER_WORD_IDX    entry  6                              */
   /*   CN_INTEGER_CHAR_BIT_IDX         entry  7                              */

   idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 0);

# ifdef _DEBUG
   if (idx != CN_INTEGER_ZERO_IDX) {
      PRINTMSG(1, 626, Internal, 0,
               "CN_INTEGER_ZERO_IDX = 1", "init_const_tbl");
   }
# endif

   idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 1);

# ifdef _DEBUG
   if (idx != CN_INTEGER_ONE_IDX) {
      PRINTMSG(1, 626, Internal, 0,
               "CN_INTEGER_ONE_IDX = 2", "init_const_tbl");
   }
# endif

   idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 2);

# ifdef _DEBUG
   if (idx != CN_INTEGER_TWO_IDX) {
      PRINTMSG(1, 626, Internal, 0,
               "CN_INTEGER_TWO_IDX = 3", "init_const_tbl");
   }
# endif

   idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 3);

# ifdef _DEBUG
   if (idx != CN_INTEGER_THREE_IDX) {
      PRINTMSG(1, 626, Internal, 0,
               "CN_INTEGER_THREE_IDX = 4", "init_const_tbl");
   }
# endif

   idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, -1);

# ifdef _DEBUG
   if (idx != CN_INTEGER_NEG_ONE_IDX) {
      PRINTMSG(1, 626, Internal, 0,
               "CN_INTEGER_NEG_ONE_IDX = 5", "init_const_tbl");
   }
# endif

   /* OSP_467, #3, set the right size of CG_INTEGER_DEFAULT_TYPE */
   idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, 
                     storage_bit_size_tbl[CG_INTEGER_DEFAULT_TYPE]);

# ifdef _DEBUG
   if (idx != CN_INTEGER_BITS_PER_WORD_IDX) {
      PRINTMSG(1, 626, Internal, 0,
               "CN_INTEGER_BITS_PER_WORD_IDX = 6", "init_const_tbl");
   }
# endif

   idx = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, CHAR_BIT);

# ifdef _DEBUG
   if (idx != CN_INTEGER_CHAR_BIT_IDX) {
      PRINTMSG(1, 626, Internal, 0,
               "CN_INTEGER_CHAR_BIT_IDX = 7", "init_const_tbl");
   }
# endif

   /* This is only used by ntr_abnormal_ieee_const but it needs to be      */
   /* initialized for each program unit.	                           */

   for (idx = 0;  idx < 18;  ++idx) {
      ieee_const_tbl_idx[idx] = NULL_IDX;
   }

   TRACE (Func_Exit, "init_const_tbl", NULL);

   return;

}  /*  init_const_tbl  */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure is called to add modules specified on the -A           *|
|*      commandline option to the SCP_USED_MODULE_LIST for this scope.        *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

void implicit_use_semantics(void)
{
   int			attr_idx;
   int			fp_idx;
   int			list_idx;
   int			name_idx;
   token_type		name_token;


   TRACE (Func_Entry, "implicit_use_semantics", NULL);

   fp_idx				= cdir_switches.implicit_use_idx;
   cdir_switches.implicit_use_idx	= NULL_IDX;

   while (fp_idx != NULL_IDX) {
      CREATE_ID(TOKEN_ID(name_token),(FP_NAME_PTR(fp_idx)),FP_NAME_LEN(fp_idx));

      TOKEN_COLUMN(name_token)	= 1;
      TOKEN_LEN(name_token)	= FP_NAME_LEN(fp_idx);
      TOKEN_LINE(name_token)	= stmt_start_line;

      attr_idx = srch_sym_tbl(TOKEN_STR(name_token),
                              TOKEN_LEN(name_token),
                              &name_idx);

      if (attr_idx != NULL_IDX) {  /* Name exists in symbol table already */

         if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
             ATP_PGM_UNIT(attr_idx) == Module) {

            /* The only way this could be here, is if it is           */
            /* specified multiple times on the -A commandline option. */

            list_idx = SCP_USED_MODULE_LIST(curr_scp_idx);

            while (list_idx != NULL_IDX) {

               if (AL_ATTR_IDX(list_idx) == attr_idx) {
                  break;
               }
               list_idx = AL_NEXT_IDX(list_idx);
            }

            if (list_idx == NULL_IDX) {

               /* Found end of module list.  The attr is not */
               /* in the list.  Add the attr to the list.    */

               /* This should be an error - as in it is the  */
               /* same name as the module we are in.  An     */
               /* error will be issued during use semantics. */

               NTR_ATTR_LIST_TBL(list_idx);
               AL_ATTR_IDX(list_idx)                                 = attr_idx;
               AL_PREV_MODULE_IDX(SCP_USED_MODULE_LIST(curr_scp_idx))= list_idx;
               AL_NEXT_IDX(list_idx)       = SCP_USED_MODULE_LIST(curr_scp_idx);
               SCP_USED_MODULE_LIST(curr_scp_idx) = list_idx;
               AT_USE_ASSOCIATED(attr_idx)        = TRUE;
               AT_MODULE_IDX(attr_idx)            = attr_idx;
            }
         }
         else { /* This is already something else in this scope.  */
            PRINTMSG(TOKEN_LINE(name_token), 1496, Error,
                     TOKEN_COLUMN(name_token),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
      }
      else {
         attr_idx                          = ntr_sym_tbl(&name_token, name_idx);
         AT_OBJ_CLASS(attr_idx)            = Pgm_Unit;
         ATP_PGM_UNIT(attr_idx)            = Module;
         ATP_SCP_IDX(attr_idx)             = curr_scp_idx;
         ATP_IMPLICIT_USE_MODULE(attr_idx) = TRUE;
         MAKE_EXTERNAL_NAME(attr_idx,
                            AT_NAME_IDX(attr_idx),
                            AT_NAME_LEN(attr_idx));
         NTR_ATTR_LIST_TBL(list_idx);
         AL_ATTR_IDX(list_idx)                                     = attr_idx;
         AL_PREV_MODULE_IDX(SCP_USED_MODULE_LIST(curr_scp_idx))    = list_idx;
         AL_NEXT_IDX(list_idx)             = SCP_USED_MODULE_LIST(curr_scp_idx);
         SCP_USED_MODULE_LIST(curr_scp_idx)= list_idx;
         AT_USE_ASSOCIATED(attr_idx)       = TRUE;
         AT_MODULE_IDX(attr_idx)           = attr_idx;
         LN_DEF_LOC(name_idx)              = TRUE;
      }

      if (AT_ORIG_NAME_IDX(attr_idx) == NULL_IDX) {
         AT_ORIG_NAME_IDX(attr_idx)        = AT_NAME_IDX(attr_idx);
         AT_ORIG_NAME_LEN(attr_idx)        = AT_NAME_LEN(attr_idx);
      }

      fp_idx = FP_NEXT_FILE_IDX(fp_idx);
   }

   TRACE (Func_Exit, "implicit_use_semantics", NULL);

   return;

}  /*  implicit_use_semantics  */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Perform statement level semantics necessary after processing a        *|
|*	statement but before going to the next.                               *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void stmt_level_semantics(void)
{
   int		blk_idx;
   int		defer_msg;
   int		save_blk_stk_idx;
   boolean	stmt_is_directive;


   TRACE (Func_Entry, "stmt_level_semantics", NULL);

   switch (stmt_type) {
      case Directive_Stmt:
      case End_Parallel_Stmt:
      case End_Do_Parallel_Stmt:
      case End_Parallel_Case_Stmt:
      case Parallel_Case_Stmt:
      case End_Guard_Stmt:
      case Open_MP_Section_Stmt:
      case Open_MP_End_Parallel_Stmt:
      case Open_MP_End_Do_Stmt:
      case Open_MP_End_Parallel_Sections_Stmt:
      case Open_MP_End_Sections_Stmt:
      case Open_MP_End_Section_Stmt:
      case Open_MP_End_Single_Stmt:
      case Open_MP_End_Parallel_Do_Stmt:
      case Open_MP_End_Master_Stmt:
      case Open_MP_End_Critical_Stmt:
      case Open_MP_End_Ordered_Stmt:
      case Open_MP_End_Parallel_Workshare_Stmt:
      case Open_MP_End_Workshare_Stmt:
      case SGI_Section_Stmt:
      case SGI_End_Psection_Stmt:
      case SGI_End_Pdo_Stmt:
      case SGI_End_Parallel_Stmt:
      case SGI_End_Critical_Section_Stmt:
      case SGI_End_Single_Process_Stmt:
      case SGI_Region_End_Stmt:

         /*  Special case if a directive is the very last statement  */
         /*  in compilation.  Need to force a program unit for this  */
         /*  directive so compilation can finish up neatly.          */

         if (LA_CH_CLASS == Ch_Class_EOF) {
            stmt_is_directive	= FALSE;
            defer_msg		= 1;
         }
         else {
            stmt_is_directive	= TRUE;
            defer_msg		= 0;
         }
        
         break;

      default:
         stmt_is_directive	= FALSE;
         defer_msg		= 0;
         break;
   }

   /* Do stuff for program unit with no PROGRAM statement.              */
   /* SCP_ATTR_IDX may be set to $MAIN, if the first statement was an   */
   /* entry style statement, but had an error in the name  (i.e., a     */
   /* PROGRAM statement with a missing name is entered as $MAIN).       */
   /* There is no beginning statement that can cause a new scope to     */
   /* be entered.  Neither the CONTAINS or the INTERFACE statements     */
   /* cause new scopes.  It is the FUNCTION/SUBROUTINE following the    */
   /* CONTAINS or INTERFACE that causes the new scope.  If this first   */
   /* statement is a FUNCTION or a SUBROUTINE, then we'll fail the if   */
   /* test, because this program unit has an entry name.                */

   if (SCP_ATTR_IDX(curr_scp_idx) == glb_tbl_idx[Main_Attr_Idx] &&
       !AT_DEFINED(glb_tbl_idx[Main_Attr_Idx]) &&
       !AT_DCL_ERR(glb_tbl_idx[Main_Attr_Idx]) &&
       (!stmt_is_directive || LA_CH_CLASS == Ch_Class_EOF)) {

      if (curr_stmt_category == Init_Stmt_Cat && !stmt_is_directive) {
         curr_stmt_category = Use_Stmt_Cat;
      }

      token			= main_token;
      TOKEN_LINE(token)		= stmt_start_line;
      TOKEN_COLUMN(token)	= stmt_start_col;
      save_blk_stk_idx		= blk_stk_idx;
      blk_stk_idx		= BLK_HEAD_IDX;

      start_new_prog_unit(Program, Program_Blk, TRUE, FALSE, &defer_msg);

      CURR_BLK_NAME		= NULL_IDX;
      blk_stk_idx	  	= save_blk_stk_idx;
   }

   /* If basic CIF records were requested, output the Unit record for */
   /* this external program unit (main program, external subprogram   */
   /* (procedure or block data subprogram), or module).		      */
   /* If the first statement of a main program is INTERFACE, the      */
   /* Begin Scope record has already been output.		      */

   if (cif_need_unit_rec &&  !stmt_is_directive) {

      /* If we have the pathological case where the program unit      */
      /* consists of nothing but an END statement, the action is the  */
      /* the same as if we're in error recovery mode.		      */

      if (blk_stk_idx == 0) {
         cif_pgm_unit_error_recovery = TRUE;
      }

      if (cif_pgm_unit_error_recovery) {

         /* See explanation of cif_pgm_unit_error_recovery in         */
         /* blk_match_err in p_end.c.				      */

         if (cif_flags == 0) {
            cif_pgm_unit_error_recovery = FALSE;
         }

         blk_stk_idx = 1;
         cif_unit_rec();
         blk_stk_idx = 0;
      }
      else {
         cif_unit_rec();
      }

      if (cif_flags != 0) {

         if (cif_pgm_unit_error_recovery) {

            /* See explanation of cif_pgm_unit_error_recovery */
            /* in blk_match_err in p_end.c.	              */

            blk_stk_idx				= 1;
            cif_begin_scope_rec();
            blk_stk_idx				= 0;
            cif_copy_temp_to_actual_CIF();
            cif_pgm_unit_error_recovery		= FALSE;
         }
         else if (BLK_CIF_SCOPE_ID(blk_stk_idx) == 0) {
            cif_begin_scope_rec();
         }
         else if (CURR_BLK == Do_Blk) {

            /* BLK_TC_TEMP_IDX and BLK_CIF_SCOPE_ID are overlayed.      */

            save_blk_stk_idx = blk_stk_idx;
            --blk_stk_idx;
            cif_begin_scope_rec(); 
            blk_stk_idx      = save_blk_stk_idx;
         }
      }
   }

   /* An END statement that ends a scoping unit has its label, if any,  */
   /* processed before coming back here.  As part of that processing,   */
   /* stmt_label_idx is set to NULL_IDX so the following code will not  */
   /* be executed.					 	        */

   if (stmt_label_idx != NULL_IDX) {

      switch(stmt_type) {
         case Allocate_Stmt:
         case Arith_If_Stmt:
         case Assign_Stmt:
         case Assignment_Stmt:
         case Backspace_Stmt:
         case Buffer_Stmt:
         case Call_Stmt:
         case Case_Stmt:
         case Close_Stmt:
         case Continue_Stmt:
         case Cycle_Stmt:
         case Deallocate_Stmt:
         case Decode_Stmt:
         case Do_Iterative_Stmt:
         case Do_While_Stmt:
         case Do_Infinite_Stmt:
         case Else_Stmt:
         case Else_If_Stmt:
         case Else_Where_Stmt:
         case Encode_Stmt:
         case Endfile_Stmt:
         case Entry_Stmt:
         case Exit_Stmt:
         case Forall_Cstrct_Stmt:
         case Forall_Stmt:
         case Goto_Stmt:
         case If_Cstrct_Stmt:
         case If_Stmt:
         case Inquire_Stmt:
         case Nullify_Stmt:
         case Open_Stmt:
         case Outmoded_If_Stmt:
         case Pause_Stmt:
         case Print_Stmt:
         case Read_Stmt:
         case Return_Stmt:
         case Rewind_Stmt:
         case Select_Stmt:
         case Stop_Stmt:
         case Then_Stmt:
         case Where_Cstrct_Stmt:
         case Where_Stmt:
         case Write_Stmt:

#ifdef KEY /* Bug 8871 */
	    /* If it's already a format-statement label, either it's a
	     * duplicate or we're jumping to it illegally. Both of these
	     * errors are reported elsewhere. Right now we just avoid
	     * changing it to Lbl_User, because we would then attempt to
	     * read attr fields which aren't valid (e.g.
	     * ATL_CMIC_BLK_STMT_IDX) and possibly crash. */
            if (Lbl_Format == ATL_CLASS(stmt_label_idx)) {
	      break;
	    }
#endif /* KEY Bug 8871 */
            ATL_EXECUTABLE(stmt_label_idx)	= TRUE;
            ATL_CLASS(stmt_label_idx)		= Lbl_User;
            ATL_DEBUG_CLASS(stmt_label_idx)	= Ldbg_User_Lbl;

            /* Check to see if this label is within a parallel region and */
            /* save the statement header that begins the region if it is. */

            blk_idx = blk_stk_idx;

            while (blk_idx > 0) {

               if (BLK_IS_PARALLEL_REGION(blk_idx)) {
                  ATL_CMIC_BLK_STMT_IDX(stmt_label_idx) =
                           BLK_FIRST_SH_IDX(blk_idx);
                  break;
               }
               blk_idx--;
            }

            /* Connect the label to its containing blocking statement.      */
            /* (If the label is defined at the procedure level, the label   */
            /* is already at the outermost level so just leave              */
            /* ATL_BLK_STMT_IDX as NULL_IDX.				    */
            /*  - The label is defined on an IF construct statement:        */
            /*      The label really belongs to the scope containing the    */
            /*      IF stmt.  Skip over the If_Then_Blk and If_Blk frames.  */
            /*      (If the label is marked in error, it means we don't     */
            /*      know what kind of IF we really have so don't do         */
            /*      anything.)				                    */
            /*  - The label is defined on a DO, SELECT, or WHERE construct: */
            /*      The label really belongs to the scope containing the    */
            /*      construct so go back one block stack frame.             */
            /*  - Otherwise, just use the top frame. 		            */

            switch (stmt_type) {
               case If_Cstrct_Stmt:

                  if (!AT_DCL_ERR(stmt_label_idx) ) {
                     blk_idx = blk_stk_idx - 2;

                     while (BLK_IS_PARALLEL_REGION(blk_idx) ||
                            BLK_TYPE(blk_idx) == Do_Parallel_Blk    ||
                            BLK_TYPE(blk_idx) == Wait_Blk    ||
                            BLK_TYPE(blk_idx) == SGI_Region_Blk) {
                        blk_idx--;
                     }

                     if (BLK_TYPE(blk_idx) > Interface_Body_Blk) {
                        ATL_BLK_STMT_IDX(stmt_label_idx) = 
                            BLK_FIRST_SH_IDX(blk_idx);
                     }
                  }
                  break;

               case Do_Iterative_Stmt:
               case Do_While_Stmt:
               case Do_Infinite_Stmt:
               case Select_Stmt:
               case Where_Cstrct_Stmt:
               case Forall_Cstrct_Stmt:
                  blk_idx = blk_stk_idx - 1;

                  while (BLK_IS_PARALLEL_REGION(blk_idx) ||
                         BLK_TYPE(blk_idx) == Do_Parallel_Blk    ||
                         BLK_TYPE(blk_idx) == Wait_Blk      ||
                         BLK_TYPE(blk_idx) == SGI_Region_Blk) {
                     blk_idx--;
                  }

                  if (BLK_TYPE(blk_idx) > Interface_Body_Blk) {
                     ATL_BLK_STMT_IDX(stmt_label_idx)=BLK_FIRST_SH_IDX(blk_idx);
                  }
                  break;

               default:
                  blk_idx = blk_stk_idx;

                  while (BLK_IS_PARALLEL_REGION(blk_idx) ||
                         BLK_TYPE(blk_idx) == Do_Parallel_Blk    ||
                         BLK_TYPE(blk_idx) == Wait_Blk      ||
                         BLK_TYPE(blk_idx) == SGI_Region_Blk) {
                     blk_idx--;
                  }

                  if (BLK_TYPE(blk_idx) > Interface_Body_Blk) {
                     ATL_BLK_STMT_IDX(stmt_label_idx)=BLK_FIRST_SH_IDX(blk_idx);
                  }
                  break;

            }  /* End switch */

            end_labeled_do();
            break;

         case Elemental_Stmt:
         case Function_Stmt:
         case Pure_Stmt:
         case Recursive_Stmt:
         case Subroutine_Stmt:
            gen_attr_and_IR_for_lbl(FALSE);
            break;

         case Null_Stmt:
         case Allocatable_Stmt:
         case Automatic_Stmt:
#ifdef KEY /* Bug 14150 */
	 case Bind_Stmt:
#endif /* KEY Bug 14150 */
         case Common_Stmt:
         case Contains_Stmt:
         case Cpnt_Decl_Stmt:
         case Data_Stmt:
         case Derived_Type_Stmt:
         case Dimension_Stmt:
         case Directive_Stmt:
         case Equivalence_Stmt:
         case External_Stmt:
         case Format_Stmt:
         case Implicit_Stmt:
         case Implicit_None_Stmt:
#ifdef KEY /* Bug 11741 */
	 case Import_Stmt:
#endif /* KEY Bug 11741 */
#ifdef KEY /* Bug 10572 */
	 case Enum_Stmt:
	 case Enumerator_Stmt:
#endif /* KEY Bug 10572 */
         case Intent_Stmt:
         case Interface_Stmt:
         case Intrinsic_Stmt:
         case Module_Proc_Stmt:
         case Namelist_Stmt:
         case Optional_Stmt:
         case Parameter_Stmt:
         case Pointer_Stmt:
         case Private_Stmt:
         case Public_Stmt:
         case Save_Stmt:
         case Sequence_Stmt:
         case Stmt_Func_Stmt:
         case Target_Stmt:
         case Task_Common_Stmt:
         case Type_Decl_Stmt:
         case Use_Stmt:
         case Volatile_Stmt:
#ifdef KEY /* Bug 14150 */
         case Value_Stmt:
#endif /* KEY Bug 14150 */

            /* The label is defined on a spec stmt.  Normally, the stmt  */
            /* doesn't need to be processed by the Semantics Pass (DATA  */
            /* is an exception).					 */

            if (stmt_type != Data_Stmt) {
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
            }

            /* If the label was defined on a type declaration stmt outside */
            /* of a derived type definition, it's Attr can be created now. */
            /* If it was defined within a type declaration, check to see   */
            /* if this is a duplicate definition by checking the label     */
            /* list attached to the derived type Attr.  If it's not a      */
            /* duplicate, create an Attr for it by hand (so it doesn't go  */
            /* in the Local Name table).  				   */

            if (CURR_BLK == Derived_Type_Blk) {
               check_for_dup_derived_type_lbl();
            }
            else if (stmt_type == Type_Decl_Stmt) { 
               gen_attr_and_IR_for_lbl(FALSE);
            }
               
            /* See if the user tried to end a loop with a spec stmt like a */
            /* FORMAT or DATA stmt.					*/

            end_labeled_do();
            break;

         case Blockdata_Stmt:
         case Module_Stmt:
         case Program_Stmt:
         case End_Blockdata_Stmt:
         case End_Do_Stmt:
         case End_Function_Stmt:
         case End_If_Stmt:
         case End_Interface_Stmt:
#ifdef KEY /* Bug 10572 */
         case End_Enum_Stmt:
#endif /* KEY Bug 10572 */
         case End_Module_Stmt:
         case End_Program_Stmt:
         case End_Select_Stmt:
         case End_Stmt:
         case End_Subroutine_Stmt:
         case End_Type_Stmt:
         case End_Where_Stmt:
         case End_Forall_Stmt:
         case Type_Init_Stmt:
         case Label_Def:
         case Construct_Def:
         case Automatic_Base_Calc_Stmt:
         case Automatic_Base_Size_Stmt:
         case End_Parallel_Stmt:
         case End_Do_Parallel_Stmt:
         case End_Parallel_Case_Stmt:
         case Parallel_Case_Stmt:
         case End_Guard_Stmt:
         case Statement_Num_Stmt:
         case SGI_Section_Stmt:
         case SGI_End_Psection_Stmt:
         case SGI_End_Pdo_Stmt:
         case SGI_End_Parallel_Stmt:
         case SGI_End_Critical_Section_Stmt:
         case SGI_End_Single_Process_Stmt:
         case SGI_Region_End_Stmt:
         case Open_MP_Section_Stmt:
         case Open_MP_End_Parallel_Stmt:
         case Open_MP_End_Do_Stmt:
         case Open_MP_End_Parallel_Sections_Stmt:
         case Open_MP_End_Sections_Stmt:
         case Open_MP_End_Section_Stmt:
         case Open_MP_End_Single_Stmt:
         case Open_MP_End_Parallel_Do_Stmt:
         case Open_MP_End_Master_Stmt:
         case Open_MP_End_Critical_Stmt:
         case Open_MP_End_Ordered_Stmt:
	 case Open_MP_End_Parallel_Workshare_Stmt:
	 case Open_MP_End_Workshare_Stmt:

            /* Check to see if this label is within a parallel region and */
            /* save the statement header that begins the region if it is. */

            blk_idx = blk_stk_idx + 1;

            while (blk_idx > 0) {

               if (BLK_IS_PARALLEL_REGION(blk_idx)) {
                  ATL_CMIC_BLK_STMT_IDX(stmt_label_idx) =
                           BLK_FIRST_SH_IDX(blk_idx);
                  break;
               }
               blk_idx--;
            }

            /* The label is defined on an END statement.	            */
            /* The block stack has already been popped so we need to cheat  */
            /* a little by using the index to the just-popped frame (we're  */
            /* assuming it hasn't been destroyed between the pop and        */
            /* arriving here).		 			            */
            /* Don't call end_labeled_do here.  It's called by the END      */
            /* routines themselves (too late by the time we get back here). */
               
            switch (stmt_type) {
            case End_Do_Stmt:
               ATL_EXECUTABLE(stmt_label_idx)   = TRUE;
               ATL_CLASS(stmt_label_idx)        = Lbl_User;
               ATL_DEBUG_CLASS(stmt_label_idx)  = Ldbg_User_Lbl;

               blk_idx = blk_stk_idx + 1;

               while (BLK_IS_PARALLEL_REGION(blk_idx) ||
                      BLK_TYPE(blk_idx) == Do_Parallel_Blk    ||
                      BLK_TYPE(blk_idx) == Wait_Blk      ||
                      BLK_TYPE(blk_idx) == SGI_Region_Blk) {

                  blk_idx++;
               }

               ATL_BLK_STMT_IDX(stmt_label_idx) = BLK_FIRST_SH_IDX(blk_idx);
               break;

            case End_If_Stmt:
            case End_Select_Stmt:
            case End_Where_Stmt:
               ATL_EXECUTABLE(stmt_label_idx)   = TRUE;
               ATL_CLASS(stmt_label_idx)        = Lbl_User;
               ATL_DEBUG_CLASS(stmt_label_idx)  = Ldbg_User_Lbl;

               blk_idx = blk_stk_idx + 1;

               while (BLK_IS_PARALLEL_REGION(blk_idx) ||
                      BLK_TYPE(blk_idx) == Do_Parallel_Blk    ||
                      BLK_TYPE(blk_idx) == Wait_Blk      ||
                      BLK_TYPE(blk_idx) == SGI_Region_Blk) {

                  blk_idx++;
               }

               ATL_BLK_STMT_IDX(stmt_label_idx) = BLK_FIRST_SH_IDX(blk_idx);

               end_labeled_do();
               break;

            case End_Type_Stmt:
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
               check_for_dup_derived_type_lbl();
               break;

#ifdef KEY /* Bug 10572 */
	    case End_Enum_Stmt:
#endif /* KEY Bug 10572 */
            case End_Interface_Stmt: /* Labeled declaration statements */
               SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
               break;

            default:
               break;
         }  
         break;
      }

      /* If the label has forward references to it, verify that they  */
      /* are correct.						      */

      if (stmt_label_idx != NULL_IDX &&
          !AT_DEFINED(stmt_label_idx)  &&  
          ATL_FWD_REF_IDX(stmt_label_idx) != NULL_IDX ) {
         resolve_fwd_lbl_refs();
      }
   }
   else {

      /* If this is a specification statement, the statement header may */
      /* be reused, so set need_new_sh = FALSE.  NOTE that DATA stmt    */
      /* and USE stmt are NOT included here because they generate IR.   */

      switch (stmt_type) {
         case Allocatable_Stmt:
         case Automatic_Stmt:
#ifdef KEY /* Bug 14150 */
	 case Bind_Stmt:
#endif /* KEY Bug 14150 */
         case Common_Stmt:
         case Contains_Stmt:
         case Cpnt_Decl_Stmt:
         case Derived_Type_Stmt:
         case Dimension_Stmt:
         case Equivalence_Stmt:
         case External_Stmt:
         case Format_Stmt:
         case Implicit_Stmt:
         case Implicit_None_Stmt:
#ifdef KEY /* Bug 11741 */
	 case Import_Stmt:
#endif /* KEY Bug 11741 */
#ifdef KEY /* Bug 10572 */
	 case Enum_Stmt:
	 case Enumerator_Stmt:
#endif /* KEY Bug 10572 */
         case Intent_Stmt:
         case Interface_Stmt:
         case Intrinsic_Stmt:
         case Module_Proc_Stmt:
         case Namelist_Stmt:
         case Optional_Stmt:
         case Parameter_Stmt:
         case Pointer_Stmt:
         case Private_Stmt:
         case Public_Stmt:
         case Save_Stmt:
         case Sequence_Stmt:
         case Stmt_Func_Stmt:
         case Target_Stmt:
         case Task_Common_Stmt:
         case Type_Decl_Stmt:
         case End_Interface_Stmt:
#ifdef KEY /* Bug 10572 */
         case End_Enum_Stmt:
#endif /* KEY Bug 10572 */
         case End_Type_Stmt:
         case Volatile_Stmt:
#ifdef KEY /* Bug 14150 */
         case Value_Stmt:
#endif /* KEY Bug 14150 */
            need_new_sh = FALSE;
            break;

         default:
            break;
      }  /* End switch */
   } 

   if (stmt_construct_idx != NULL_IDX) {

      /* Construct name not allowed on this type of statement.  IF, DO, */
      /* and SELECT statements use stmt_construct_idx and then clear it */
      /* so that this check does not have to check statement type.      */

      PRINTMSG(stmt_start_line, 7, Error, stmt_start_col,
               stmt_type_str[stmt_type]);
   }

   TRACE (Func_Exit, "stmt_level_semantics", NULL);

   return;

}  /*  stmt_level_semantics  */

# if defined(_EXPRESSION_EVAL)
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	When the FE is used as an expression evaluator, we can be sent        *|
|*	expressions rather than whole statement.  This routine parses         *|
|*	that expression and turns it into a whole statement.                  *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void parse_expr_for_evaluator(void)
{
   int			attr_idx;
   int			ir_idx;
   opnd_type		opnd;
   int			sh_idx;


   TRACE (Func_Entry, "parse_expr_for_evaluator", NULL);

   stmt_type			= Assignment_Stmt;
   sh_idx			= curr_stmt_sh_idx;
   curr_stmt_sh_idx		= ntr_sh_tbl();
   SH_NEXT_IDX(sh_idx)		= curr_stmt_sh_idx;
   SH_PREV_IDX(curr_stmt_sh_idx)= sh_idx;

   if (parse_expr(&opnd)) {

      stmt_level_semantics();
   }
   else { /* Problems with expression - exit */
   }

   NEXT_LA_CH;  /* Should be End */

   TRACE (Func_Exit, "parse_expr_for_evaluator", NULL);

   return;

}  /*  parse_expr_for_evaluator  */
# endif

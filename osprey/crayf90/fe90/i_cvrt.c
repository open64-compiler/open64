/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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



/* USMID:  "\n@(#)5.0_pl/sources/i_cvrt.c	5.34	10/28/99 10:03:56\n" */

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "debug.m"
# include "s_globals.m"
# include "i_cvrt.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"
# include "s_globals.h"
# include "i_cvrt.h"
#ifdef KEY /* Bug 6845 */
#define FOR_I_CVRT
#include "../sgi/cwh_types.h"
#endif /* KEY Bug 6845 */
#ifdef KEY /* Bug 14150 */
#include "../sgi/decorate_utils.h"
#include "../sgi/cwh_stab.i"
#endif /* KEY Bug 14150 */

/*****************************************************************\
|* Function prototypes of static functions declared in this file *|
\*****************************************************************/

static  void	allocate_pdg_link_tbls(void);
static	void	cvrt_exp_to_pdg(int, fld_type);
static	void	cvrt_ir_to_pdg(int);
static	void	cvrt_proc_to_pdg(char *);
static	void	cvrt_sytb_to_pdg(void);
static	void	finish_symbolic_expr(void);
static	TYPE	get_basic_type(int, int, int);
static	TYPE	get_type_desc(int);
static	void	send_attr_ntry(int);
static	void	send_darg_list(int, int);
static	TYPE	send_derived_type(int);
static	void	send_dummy_procedure(int);
static	void	send_label(int);
static	void	send_label_def(int);
static	void	send_mod_file_name(void);
static	void	send_namelist_group(int);
static	TYPE	send_non_standard_aligned_type(int, int);
static	void	send_procedure(int, int, int);
static	void	send_stor_blk(int, int *);

/*****************************************************************\
|* Static data used in this module.                              *|
\*****************************************************************/

static  pdg_type_tbl_type      *RESTRICT pdg_type_tbl;

static	int	data_value_idx;
static	int	global_attr_idx;
static  int     blank_pad_text;
static  int     data_attr;
static  int     data_character_bit_length;
static  boolean stack_data_object;
static	boolean	whole_subscript;
static	boolean	whole_substring;
static  boolean stack_data_constant;
static	boolean	symbolic_constant_expr;
static	int    	processing_call;
static	int    	curr_sh;
static	int	io_type;
static  int     case_cmic_vpr_idx;
static  int     guard_cmic_vpr_idx;
static  int     doparallel_cmic_vpr_idx;
static  int     parallel_cmic_vpr_idx;
static  boolean processing_io_stmt;
static  TYPE    null_type;
static	TYPE    pdg_type_void;
static  int     pdg_type_tbl_idx;
static  int     pdg_type_tbl_inc;
static  int     pdg_type_tbl_init_size;
static  int     pdg_type_tbl_limit      = (1 << 16) - 1;
static  int     pdg_type_tbl_num_wds    = HOST_BYTES_TO_WORDS(
                                          (sizeof(pdg_type_tbl_type)));
static  int     pdg_type_tbl_size;
static  int     pdg_type_tbl_largest_idx;

static  int             pdg_basic_type[Num_Basic_Types] = {
                        Integral,
                        L_ogical,
                        Floating_Pt,
                        C_omplex,
                        CRI_Pointer,
                        BT_func_ptr,
                        CRI_Pointer_Char,
                        T_ypeless,
                        Char_Fortran,
                        S_tructure};

static	char            *p_proc[] = {
                        "Unknown_Proc",
		        "Extern_Proc",
		        "Intern_Proc_Ref",
		        "Dummy_Proc",
		        "Intern_Proc",
		        "Imported_Proc",
		        "Module_Proc"};

static	char		*p_obj_sym_class[] = {
                        "Unknown_Sym",
		        "User_Variable",
			"Dummy_Arg",
			"Function_Rslt",
			"Compiler_Temp",
			"Cray_Pointee",
			"Component",
			"Vector_Temp",
			"Vector_Maskk",
			"Vector_Iota",
			"MPP_Object",
			"Call_Dummy",
			"Dummy_Procedure",
			"Hosted_User_Variable",
			"Hosted_Dummy_Arg",
			"Scratch_Symbol",
			"Hosted_Dummy_Procedure",
                        "Hosted_Compiler_Temp",
                        "Name" };

static	char            *p_return[] = {
                        "Unknown_Return",
                        "Normal_Return",
                        "Void_Return",
                        "Alternate_Return" };

static	char            *p_atp_pgm_unit[] = {
                        "Pgm_Unknown",
                        "C Function",
                        "ERROR",
                        "Function",
                        "Subroutine",
                        "Program",
                        "Blockdata",
                        "Module" };

static 	char            *p_boolean[] = {
                        "F", 
                        "T"};

static	char		*p_label[] = {
                        "Lbl_Unknown",
                        "Lbl_User",
                        "Lbl_Format",
                        "Lbl_Internal",
                        "Lbl_Debug",
                        "Lbl_Cstrct" };

static	char		*p_dbg_label[] = {
                        "Ldbg_None",
                        "Ldbg_Stmt_Lbl",
                        "Ldbg_Exit_Point",
                        "Ldbg_End_Prologue",
                        "Ldbg_Start_Epilogue",
                        "Ldbg_Construct_Name",
                        "Ldbg_Loop_Lbl",
                        "Ldbg_User_Lbl" };

static	char            *p_intent[] = {
                        "Intent_Unseen",
                        "Intent_In",
                        "Intent_Out",
                        "Intent_Inout" };

static	char            *p_io[] = {
                        "Write",
                        "Read",
                        "Write_Namelist",
                        "Read_Namelist" };

static	char            *p_sequence[] = {
                        "Unknown_Seq",
                        "Unsequenced",
                        "Numeric_Seq",
                        "Char_Seq",
                        "Mixed_Seq" };

static	char            *p_sb_blk_type_str[] = {
                        "Unknown",
                        "Static",
                        "Stack",
                        "Formal",
                        "Common",
                        "Extern",
                        "Exported",
                        "Task_Common",
                        "Soft_External",
                        "Global_Breg",
                        "Global_Treg",
                        "Static_Named",
                        "Based",
                        "Equivalenced",
                        "Restricted",
                        "Distributed",
                        "LM_Static",
                        "LM_Common",
                        "LM_Extern",
                        "Auxiliary",
                        "Static_Local",
                        "Non_Local_Stack",
                        "Non_Local_Formal",
                        "Hosted_Stack",
                        "Threadprivate",
                        "Coment"  };

static	char		*p_table_type[]	= {
			"Unknown",
			"Basic",
			"Pointer",
			"Array",
			"Function" };

static	char		*p_distribution[] = {
			"No_Distribution",
			"Block_Distribution",
			"Cyclic_Distribution",
			"Star_Distribution" };

static	char		*p_basic_type[]	= {
			"Unknown",
			"Logical",
			"Typeless",
			"Void",
			"Char_Fortran",
			"Char_C",
			"Struct",
			"Union",
			"Integral",
			"Floating_Pt",
			"Complex",
			"Cray_Pointer",
			"Cray_Pointer_Ch",
			"Cray_Parcel" };

static	char		*p_const_class[] = {
			"No_Const",
			"Arith_Const",
			"Addr_Const",
			"Pattern_Const",
			"Array_Const",
			"Struct_Const",
			"Null_Arg_Const" };

static	char		*p_tasking_context[] = {
			"Context_None",
			"Context_Private",
			"Context_Shared",
			"Context_Value",
			"Context_Iterate",
			"Context_Getfirst",
			"Context_Control",
			"Context_Induction",
			"Context_PE_Res_Func",
			"Context_PE_Res_Loop",
			"Context_Lastlocal",
			"Context_Affinity",
			"Context_Nest",
			"Context_Lastthread",
			"Context_Omp_Reduction_Max",
			"Context_Omp_Reduction_Min",
			"Context_Omp_Reduction_Band",
			"Context_Omp_Reduction_Bor",
			"Context_Omp_Reduction_Bneqv",
			"Context_Omp_Reduction_Plus",
			"Context_Omp_Reduction_Mult",
			"Context_Omp_Reduction_Minus",
			"Context_Omp_Reduction_And",
			"Context_Omp_Reduction_Or",
			"Context_Omp_Reduction_Eqv",
			"Context_Omp_Reduction_Neqv",
        		"Context_Omp_Private",
        		"Context_Omp_Shared",
        		"Context_Omp_Firstprivate",
        		"Context_Omp_Lastprivate",
        		"Context_Omp_Copyin",
        		"Context_Omp_Copyprivate", /* by jhs, 02/7/22 */
        		"Context_Omp_Affinity",
			"Context_Omp_Nest"
						 };

int     pdg_align[8] = {0,                      /* Signifies no alignment */
                        Bit_Align,
                        Byte_Align,
                        Parcel_Align,
                        HWord_Align,
                        Word_Align,
                        DWord_Align,
                        FWord_Align
                        };


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Perform blank padding for a user implied DO.                          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
void blank_padding (long64    pad,
                    long64    offset)

{
# if defined(_HOST_LITTLE_ENDIAN)
   long   a_blank    = (long)' ';
#else
   long   a_blank    = (long)' ' << (sizeof(long)*CHAR_BIT - CHAR_BIT);
# endif
   TYPE   basic;


   TRACE (Func_Entry, "blank_padding", NULL);

   /* stack a blank */

   basic = get_basic_type(Character_1, Align_Bit, NULL_IDX);

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("fei_constant");
   PDG_DBG_PRINT_T("(1) type", basic);
   PDG_DBG_PRINT_S("(2) const class", p_const_class[Pattern_Const]);
   PDG_DBG_PRINT_LD("(3) const", a_blank);
   PDG_DBG_PRINT_D("(4) bit length", CHAR_BIT);
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
   fei_constant(basic, Pattern_Const, (char *) &a_blank, (long64) CHAR_BIT);
# endif

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("fei_static_next_simple_init");
   PDG_DBG_PRINT_D("(1) CHAR_BIT", CHAR_BIT);
   PDG_DBG_PRINT_LLD("(2) pad", pad);
   PDG_DBG_PRINT_LLD("(3) offset", offset);
   PDG_DBG_PRINT_D("(4) unused", 0);
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
   fei_static_next_simple_init((long64) CHAR_BIT,
                               pad,
                               offset,
                               0);
# endif

   TRACE (Func_Exit, "blank_padding", NULL);

   return;
}  /*  blank_padding  */





/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*    Start up the interface.                                                 *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*              none                                                          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*              none                                                          *|
|*                                                                            *|
|* Returns:     nothing                                                       *|
|*                                                                            *|
\******************************************************************************/

void init_PDGCS(void)

{
   		char		*code_file_name;
   		char		compiler_name[8];
   		long		flags					= 0;
   		char		path_name[MAX_FILE_NAME_SIZE];
   extern	char		release_level[8];
   extern	char    	*command_name;

   TRACE (Func_Entry, "init_PDGCS", NULL);

   if (dump_flags.preinline) {
      code_file_name = NULL;
   }
   else if (binary_output) {
      code_file_name = bin_file;
   }
   else if (assembly_output) {
      code_file_name = assembly_file;
   }
   else {
      return;   /* No binary or cal output - exit */
   }

   /* Need to open the debug file if dumping the PDGCS interface calls. */
   if (dump_flags.pdgcs) {
      init_debug_file();
   }

   getcwd (path_name, MAX_FILE_NAME_SIZE);

   compiler_name[0] = 'C';
   compiler_name[1] = 'F';
   compiler_name[2] = 'T';
   compiler_name[3] = '9';
   compiler_name[4] = '0';
   compiler_name[5] = NULL_CHAR;


   flags = flags |
   ((long) cmd_line_flags.dalign	      << PDGCS_INITIALIZE_DOUBLE_ALIGN)|
   ((long) assembly_output		      << PDGCS_INITIALIZE_OBJ_FILE)|
   ((long) (cmd_line_flags.debug_lvl == Debug_Lvl_1)
                                              << PDGCS_INITIALIZE_PART_DEBUG)|
   ((long) (cmd_line_flags.debug_lvl == Debug_Lvl_2)
                                              << PDGCS_INITIALIZE_OPT_DEBUG)|
   ((long) on_off_flags.atexpert	      << PDGCS_INITIALIZE_ATEXPERT)|
   ((long) on_off_flags.upper_case_names      << PDGCS_INITIALIZE_KERNEL)|
   ((long) (cmd_line_flags.debug_lvl <= Debug_Lvl_2)
                                              << PDGCS_INITIALIZE_DBG_TABLES)|
   ((long) (!on_off_flags.round_mult_operations)
                                              << PDGCS_INITIALIZE_TRUNCATE)|
   (!((cif_flags & MESSAGE_RECS) || opt_flags.msgs) 
                                              << PDGCS_INITIALIZE_NO_OPT_MSGS)|
   ((long) (cmd_line_flags.solaris_profile || on_off_flags.flowtrace_option) 
                                              << PDGCS_INITIALIZE_FLOWTRACE)|
   ((long) (opt_flags.unroll_lvl == Unroll_Lvl_2)
                                              << PDGCS_INITIALIZE_MEM_HIER_OPT)|
   ((long) cmd_line_flags.small_pic_model     << PDGCS_INITIALIZE_SMALL_PIC)|
   ((long) cmd_line_flags.large_pic_model     << PDGCS_INITIALIZE_BIG_PIC)|
   (!opt_flags.neg_msgs		              << PDGCS_INITIALIZE_NO_NEG_MSGS)|
   ((long) dump_flags.mp		      << PDGCS_INITIALIZE_MP)|
   ((long) on_off_flags.MPP_apprentice	      << PDGCS_INITIALIZE_APPRENTICE)|
   ((long) on_off_flags.shared_to_private_coer<< PDGCS_INITIALIZE_S2P_COERCE)|
   ((long) opt_flags.jump		      << PDGCS_INITIALIZE_CCG_JUMPS)|
   ((long) opt_flags.opt_info		      << PDGCS_INITIALIZE_OPT_INFO)|
   ((long) on_off_flags.integer_1_and_2       << PDGCS_INITIALIZE_INTEGER_1_2)|
   (((long) opt_flags.split_lvl == Split_Lvl_2) 
                                              << PDGCS_INITIALIZE_STREAMSPLIT);

   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_C("PDGCS_initialize");
   PDG_DBG_PRINT_S("compiler_name", compiler_name);
   PDG_DBG_PRINT_S("(1) language", "PDGCS_Fortran_90");
   PDG_DBG_PRINT_LO("(2) flags", flags);
   PDG_DBG_PRINT_S("(3) command_name", command_name);
   PDG_DBG_PRINT_S("(4) release_level", release_level);
   PDG_DBG_PRINT_S("(5) code_file_name", code_file_name); 
   PDG_DBG_PRINT_S("(6) list_file_name", (char *) &assembly_listing_file);
   PDG_DBG_PRINT_D("(7) trunc bits", cmd_line_flags.truncate_bits);
   PDG_DBG_PRINT_LD("(8) CCG flags", ccg_dump_flags);
   PDG_DBG_PRINT_S("(9) src_path_name", get_src_path_name());
   PDG_DBG_PRINT_S("(10) cif_file_name", (char *) &cif_name);
   PDG_DBG_PRINT_S("(11) debug_file_name", (char *) &debug_file_name);
   PDG_DBG_PRINT_LD("(12) debug file ptr", (long) debug_file);
   PDG_DBG_PRINT_LD("(13) cif file ptr", (long) cif_actual_file);
   PDG_DBG_PRINT_S("(14) GL_FILE_NAME", GL_FILE_NAME_PTR(1));
   PDG_DBG_PRINT_S("(15) path name", path_name);
   PDG_DBG_PRINT_D("(16) N$PE", cmd_line_flags.MPP_num_pes);
   PDG_DBG_PRINT_D("(17) meta test", dump_flags.pvp_test);
   PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
   PDGCS_initialize(Fortran_90,
                    flags,
                    command_name,
                    release_level,
                    code_file_name,
                    (char *)&assembly_listing_file,
                    cmd_line_flags.truncate_bits,
                    ccg_dump_flags,
                    get_src_path_name(),
                    (char *)&cif_name,
                    (char *)&debug_file_name,
                    debug_file,
                    cif_actual_file,
                    GL_FILE_NAME_PTR(1),
                    path_name,
                    cmd_line_flags.MPP_num_pes, 
                    dump_flags.pvp_test);
# endif


   TRACE (Func_Exit, "init_PDGCS", NULL);

   return;

}  /* init_PDGCS */




/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This does initialization, calls the routines to pass the symbol       *|
|*	and the IR to PDG.  It then does cleanup.  This routine should        *|
|*	handle internal program units.  (Whichever way it's decided).         *|
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
void cvrt_to_pdg (char	*compiler_gen_date)

{
   int		align;
   int		i;
   int		bit_size;
   int		flags;

# if defined(_TARGET_OS_MAX) || defined(_TARGET_OS_UNICOS)
   int		child_idx;
# endif


   TRACE (Func_Entry, "cvrt_to_pdg", NULL);

   /* This drives the PDGCS interface for a program unit.  It is called once  */
   /* for each BLOCKDATA, PROGRAM, SUBROUTINE, MODULE, external SUBROUTINE or */
   /* external FUNCTION.                                                      */

   /* Initialize variables and tables here. */

   case_cmic_vpr_idx = NULL_IDX;
   guard_cmic_vpr_idx = NULL_IDX;

   init_directive(3);

   for (i = 1; i <= global_line_tbl_idx; i++) {
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("cwh_add_to_used_files_table");
      PDG_DBG_PRINT_S("(1) path", GL_FILE_NAME_PTR(i));
      PDG_DBG_PRINT_D("(2) one", 1);
      PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      cwh_add_to_used_files_table(GL_FILE_NAME_PTR(i), 1);
# endif
   }

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("fei_descriptor");
   PDG_DBG_PRINT_O("(1) flags", 0);
   PDG_DBG_PRINT_S("(2) table type", p_table_type[Basic]);
   PDG_DBG_PRINT_D("(3) bit_size", 0);
   PDG_DBG_PRINT_S("(4) basic type", p_basic_type[V_oid]);
   PDG_DBG_PRINT_D("(5) aux info", 0);
   PDG_DBG_PRINT_D("(6) alignment", 0);
   PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
   pdg_type_void = fei_descriptor(0,
                                  Basic,
                                  0,
                                  V_oid,
                                  0,
                                  0);
# endif
   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_T("(r) type", pdg_type_void);
   PDG_DBG_PRINT_END

   pgm_unit_start_line = SH_GLB_LINE(SCP_FIRST_SH_IDX(curr_scp_idx));

# if defined(_TARGET_OS_MAX) || defined(_TARGET_OS_UNICOS)

   /* On Cray systems,                                                        */
   /* Modules must go thru first, because MODULE PDT's must be first.  Clear  */
   /* SCP_FIRST_CHILD_IDX for the module, so its children do not get sent     */
   /* thru.  (cvrt_proc_to_pdg finds the innermost child and sends it first.) */
   /* After the module is sent thru, send all its children via the normal     */
   /* order.  Then when that's thru send the module information table.        */

   if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
      child_idx	= SCP_FIRST_CHILD_IDX(curr_scp_idx);
      SCP_FIRST_CHILD_IDX(curr_scp_idx) = NULL_IDX;

      cvrt_proc_to_pdg(compiler_gen_date);

      if (child_idx != NULL_IDX) {
         curr_scp_idx = child_idx;
         cvrt_proc_to_pdg(compiler_gen_date);
      }

      curr_scp_idx = MAIN_SCP_IDX;

      if (on_off_flags.module_to_mod) {
         send_mod_file_name();    /* Sends the file name */
         create_mod_info_tbl();   /* Creates the table.  */
         output_mod_info_file();  /* Writes the table.   */
      }
      else { /* Cray and MPP go out in a special &%% module because of segldr.*/
         send_mod_file_name();
      }
      free_tables();
   }
   else {
      cvrt_proc_to_pdg(compiler_gen_date);
   }

# else
   cvrt_proc_to_pdg(compiler_gen_date);
# endif

   TBL_FREE(pdg_link_tbl);
   TBL_FREE(pdg_type_tbl);

   TRACE (Func_Exit, "cvrt_to_pdg", NULL);

   return;

}  /*  cvrt_to_pdg  */

#ifdef KEY /* Bug 14150 */
/* Remember info about the source construct which led us to create the previous
 * definition of an external label */
typedef struct {
  const char *id;	/* source identifier */
  int line;		/* global source line */
  int bind_attr;	/* source declaration has BIND attribute */
  } external_name_t;

/*
 * Issue an error if we're about to use the same external label for two
 * definitiions which have different identifiers in the source code. (The
 * reason is that the BIND attribute allows the user to create duplicates
 * involving program units and common, even involving "hidden" names
 * like MAIN__ and _BLNK_, and sometimes this causes the back end to crash.
 * Since it's hard to be sure that duplicates are never legitimate at the
 * point in the front end where we call this--remarkably, there's not a
 * simple bit distinguishing "definition" from "reference" in the attr--we
 * issue the error only if at least one of the source constructs had the
 * BIND attribute; any spurious errors will then be limited to code using
 * that new F2003 construct. As yet we don't call this function in every
 * place where an external label might be generated (e.g.
 * "__pathscale_compiler") but hopefully we've got the cases which are apt
 * to cause trouble.
 *
 * fld		AT_Tbl_Idx or SB_Tbl_Idx
 * idx		Index into attr_tbl or stor_blk_tbl
 * ext_name	External label
 */
static void
check_duplicate_external_name(fld_type fld, int idx, const char *ext_name) {
  const char *id = (AT_Tbl_Idx == fld) ? AT_OBJ_NAME_PTR(idx) :
    SB_NAME_PTR(idx);
  int line = (AT_Tbl_Idx == fld) ? AT_DEF_LINE(idx) : SB_DEF_LINE(idx);
  int bind_attr = (AT_Tbl_Idx == fld) ? AT_BIND_ATTR(idx) : SB_BIND_ATTR(idx);
  external_name_t *prev = get_external_label(ext_name);
  if (!prev) {
    prev = malloc(sizeof prev);
    prev->id = strdup(id);
    prev->line = line;
    prev->bind_attr = bind_attr;
    put_external_label(ext_name, prev);
  }

  if ((bind_attr || prev->bind_attr) && !!strcmp(prev->id, id)) {
      int column = (AT_Tbl_Idx == fld) ? AT_DEF_COLUMN(idx) :
	SB_DEF_COLUMN(idx);
      char *fal = file_and_line(prev->line);
      PRINTMSG(line, 1701, Error, column, id, prev->id, ext_name, fal);
      free(fal);
   }
}
#endif /* KEY Bug 14150 */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This does initialization, calls the routines to pass the symbol       *|
|*	and the IR to PDG.  It then does cleanup.                             *|
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
static void cvrt_proc_to_pdg (char	*compiler_gen_date)

{
   boolean	check_scp	= TRUE;
   int		mobes_sades;
   char	       *name_ptr;
   long		opt_flag;
   int		pgm_attr_idx;
   int		pgm_code	= 0;
   int		pgm_data	= 0;
   int		save_curr_scp_idx;

extern void dim_reshape_pass_driver(void);
extern void runtime_ptr_chk_driver(void);

   TRACE (Func_Entry, "cvrt_proc_to_pdg", NULL);

#ifdef KEY /* Bug 3507 */
   int attr_idx = SCP_ATTR_IDX(curr_scp_idx);
   boolean is_module = (Module == ATP_PGM_UNIT(attr_idx));
     
   if (is_module) {
     int lineno = AT_DEF_LINE(attr_idx);
     char *file_name = global_to_local_file(lineno);
     INT32 local_lineno = global_to_local_line_number(lineno);
     cwh_dst_enter_module(AT_OBJ_NAME_PTR(attr_idx), file_name, local_lineno);
   }
#endif /* KEY Bug 3507 */

PROCESS_SIBLING:

   /* The innermost children go thru the interface first.  The external */
   /* procedure goes thru last.                                         */

   if (SCP_FIRST_CHILD_IDX(curr_scp_idx) != NULL_IDX) {
      save_curr_scp_idx	= curr_scp_idx;
      curr_scp_idx = SCP_FIRST_CHILD_IDX(curr_scp_idx);
      cvrt_proc_to_pdg(compiler_gen_date);
      curr_scp_idx = save_curr_scp_idx;
   }

   if (opt_flags.reshape) {
      dim_reshape_pass_driver();
   }

   if (cmd_line_flags.runtime_ptr_chk) {
      runtime_ptr_chk_driver();
   }


   allocate_pdg_link_tbls();

   pgm_attr_idx	= SCP_ATTR_IDX(curr_scp_idx);
   ATP_SCP_ALIVE(pgm_attr_idx) = TRUE;
   stmt_start_line = SH_GLB_LINE(SCP_FIRST_SH_IDX(curr_scp_idx));
   stmt_start_col = 0;

   cvrt_sytb_to_pdg();

   name_ptr = ATP_EXT_NAME_PTR(pgm_attr_idx);
#ifdef KEY /* Bug 14150 */
   /* Imitate kludgy transformation performed by sgi/cwh_stab.cxx */
   const char *ext_name = ATP_EXT_NAME_PTR(pgm_attr_idx);
   if (Program == ATP_PGM_UNIT(pgm_attr_idx)) {
     ext_name = def_main_u;
   }
   check_duplicate_external_name(AT_Tbl_Idx, pgm_attr_idx, ext_name);
#endif /* KEY Bug 14150 */

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("PDGCS_comp_unit");
   PDG_DBG_PRINT_S("(1) name_ptr", name_ptr);
   PDG_DBG_PRINT_D("(2) unused", 0);
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
   PDGCS_comp_unit(name_ptr, NULL_IDX);
# endif

   opt_flag = 
   ((long) opt_flags.aggress                << PDGCS_NEW_PROC_IGNORE_THROTTLE)|
   ((long) (cmd_line_flags.debug_lvl != Debug_Lvl_0)
                                            << PDGCS_NEW_PROC_ELIM_DEAD_CODE)|
   ((long) opt_flags.pattern                << PDGCS_NEW_PROC_PATTERN_MATCHING)|
   ((long) on_off_flags.reciprical_divide   << PDGCS_NEW_PROC_IEEE_RECIPS)|
   ((long) (opt_flags.task_lvl == Task_Lvl_3)
                                            << PDGCS_NEW_PROC_TASK_INNER_LOOPS)|
   ((long) cmd_line_flags.runtime_conformance << PDGCS_NEW_PROC_CONFORM_CHECK)|
   ((long) opt_flags.ieeeconform	    << PDGCS_NEW_PROC_IEEE_CONFORM)|
   ((long) cmd_line_flags.do_UDB_checks     << PDGCS_NEW_PROC_DO_UBD_ANALYSIS);

   send_attr_ntry(pgm_attr_idx);

   mobes_sades = ATP_USES_EREGS(pgm_attr_idx) ? 512 : 0;

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("PDGCS_new_proc");
   PDG_DBG_PRINT_D("(1) num ir", 2000);
   PDG_DBG_PRINT_LD("(2) PDG_AT_IDX", PDG_AT_IDX(pgm_attr_idx));
   PDG_DBG_PRINT_D("(3) alt entries", SCP_ALT_ENTRY_CNT(curr_scp_idx));
   PDG_DBG_PRINT_D("(4) scalar level", opt_flags.scalar_lvl);
   PDG_DBG_PRINT_D("(5) vector level", opt_flags.vector_lvl);
   PDG_DBG_PRINT_D("(6) task level", opt_flags.task_lvl);
   PDG_DBG_PRINT_LO("(7) flags", opt_flag);
   PDG_DBG_PRINT_D("(8) user mobes", mobes_sades);
   PDG_DBG_PRINT_D("(9) user sades", mobes_sades);
   PDG_DBG_PRINT_D("(10) pipeline level", opt_flags.pipeline_lvl);
   PDG_DBG_PRINT_D("(11) stream level", opt_flags.stream_lvl);
   PDG_DBG_PRINT_END
  

# ifdef _ENABLE_FEI
   PDGCS_new_proc(2000,
                  PDG_AT_IDX(pgm_attr_idx), 
                  SCP_ALT_ENTRY_CNT(curr_scp_idx),
                  opt_flags.scalar_lvl,
                  opt_flags.vector_lvl,
                  opt_flags.task_lvl,
                  opt_flag,
                  mobes_sades,
                  mobes_sades,
                  opt_flags.pipeline_lvl, 
                  opt_flags.stream_lvl);
# endif

   cvrt_ir_to_pdg(curr_scp_idx);

   ATP_SCP_ALIVE(pgm_attr_idx) = FALSE;

   if (cmd_line_flags.debug_lvl <= Debug_Lvl_2) {
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("PDGCS_debug_init");
      PDG_DBG_PRINT_S("(1) src path name", get_src_path_name());
      PDG_DBG_PRINT_S("(2) gen date", compiler_gen_date);
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI  
      PDGCS_debug_init(get_src_path_name(),
                       compiler_gen_date);
# endif
   }

   PDG_DBG_PRINT_START
# ifdef _DEBUG
   fflush(debug_file);
# endif
   PDG_DBG_PRINT_END

   /*
   If this is the outermost parent scope and this is not a module,
   free all the tables.
   */

   if (curr_scp_idx == MAIN_SCP_IDX &&
       ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Module) {
      free_tables();
      check_scp = FALSE;   /* Tables are gone */
   }

# if !defined(_TARGET_OS_MAX) && !defined(_TARGET_OS_UNICOS)
   if (curr_scp_idx == MAIN_SCP_IDX &&
       ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {

      /* Cray and MPP go out in a special &%% module because of segldr. */

      send_mod_file_name();    /* Sends the file name */
      create_mod_info_tbl();   /* Creates the table.  */
      output_mod_info_file();  /* Writes the table.   */
      free_tables();
      check_scp = FALSE;   /* Tables are gone */
   }
# endif


   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("PDGCS_do_proc");
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
#ifdef KEY /* Bug 3507 */
   /* If this is a module, we have already generated the Dwarf for it */
   PDGCS_do_proc(is_module);            /* Start up the backend */
#else /* KEY Bug 3507 */
   PDGCS_do_proc();            /* Start up the backend */
#endif /* KEY Bug 3507 */
# endif

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("PDGCS_end_procs");
   PDG_DBG_PRINT_D("(1) pgm_code", pgm_code);
   PDG_DBG_PRINT_D("(2) pgm_data", pgm_data);
   PDG_DBG_PRINT_END
  
# ifdef _ENABLE_FEI
   PDGCS_end_procs(&pgm_code,
                   &pgm_data);
# endif

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_D("(r) pgm_code", pgm_code);
   PDG_DBG_PRINT_D("(r) pgm_data", pgm_data);
   PDG_DBG_PRINT_END
  


   code_size = code_size + pgm_code;
   data_size = data_size + pgm_data;

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("PDGCS_end_comp_unit");
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
   PDGCS_end_comp_unit();
# endif

   if (check_scp && SCP_SIBLING_IDX(curr_scp_idx) != NULL_IDX) {
      curr_scp_idx = SCP_SIBLING_IDX(curr_scp_idx);
      goto PROCESS_SIBLING;
   }

#ifdef KEY /* Bug 3507 */
   if (is_module) {
     cwh_dst_exit_module();
   }
#endif /* KEY Bug 3507 */

   TRACE (Func_Exit, "cvrt_proc_to_pdg", NULL);

   return;

}  /* cvrt_proc_to_pdg */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get the next data value from the constant list following a data       *|
|*	statement.                                                            *|
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
void push_data_value (int      t_idx)

{
   char         *new_str_ptr;
   char         *old_str_ptr;
   boolean       ignore_types;
   long_type     target_length;
#ifdef KEY /* Bug 10177 */
   int           idx = 0;
   int           l_idx = 0;
   fld_type      fld = NO_Tbl_Idx;
   int           cn_idx = 0;
#else /* KEY Bug 10177 */
   int           idx;
   int           l_idx;
   fld_type      fld;
   int           cn_idx;
#endif /* KEY Bug 10177 */
   int           type_idx;
   int           new_str_idx;
   long64	 len;
#ifdef KEY /* Bug 10177 */
   long64   	 rep_count = 0;
   long64   	 stride = 0;
   long64   	 padd = 0;
   long64        num_chars = 0;
#else /* KEY Bug 10177 */
   long64   	 rep_count;
   long64   	 stride;
   long64   	 padd = 0;
   long64        num_chars;
#endif /* KEY Bug 10177 */
   int           i;


   TRACE (Func_Entry, "push_data_value", NULL);

   if (IL_LIST_CNT(data_value_idx) == 0) {
      data_value_idx = IL_NEXT_LIST_IDX(data_value_idx);
   }

   IL_LIST_CNT(data_value_idx) = IL_LIST_CNT(data_value_idx) - 1;

   /* stack the linearizable object */

   data_attr = NULL_IDX;
   stack_data_object = TRUE;
   cvrt_exp_to_pdg(IL_IDX(t_idx),
                   IL_FLD(t_idx));
   stack_data_object = FALSE;

   ignore_types = FALSE;

   switch (IL_FLD(data_value_idx)) {

   case CN_Tbl_Idx :   
      idx = IL_IDX(data_value_idx);
      fld = IL_FLD(data_value_idx);
      rep_count	= (long64) 1;
      stride = (long64) 0;
      cn_idx = IL_IDX(data_value_idx);

      if (TYP_TYPE(CN_TYPE_IDX(idx)) == Typeless ||
          (TYP_TYPE(CN_TYPE_IDX(idx)) == Character &&
           TYP_TYPE(ATD_TYPE_IDX(data_attr)) != Character)) { 
         ignore_types = TRUE;
      }
      break;

   case IR_Tbl_Idx :   
      if (IR_FLD_R(IL_IDX(data_value_idx)) == CN_Tbl_Idx) {
         idx = IR_IDX_R(IL_IDX(data_value_idx));
         fld = IR_FLD_R(IL_IDX(data_value_idx));
         rep_count = (long64) 1;
         stride	= (long64) 0;
         cn_idx	= IR_IDX_R(IL_IDX(data_value_idx));

         if (TYP_TYPE(CN_TYPE_IDX(idx)) == Typeless ||
             (TYP_TYPE(CN_TYPE_IDX(idx)) == Character &&
             TYP_TYPE(ATD_TYPE_IDX(data_attr)) != Character)) { 
            ignore_types = TRUE;
         }
      }
      else {  /* we have a Mult_Opr - blank pad here */
         idx = IR_IDX_R(IR_IDX_R(IL_IDX(data_value_idx)));
         fld = IR_FLD_R(IR_IDX_R(IL_IDX(data_value_idx)));
         rep_count = CN_INT_TO_C(IR_IDX_L(IR_IDX_R(IL_IDX(data_value_idx))));
         stride = (long64) CHAR_BIT;
         cn_idx = CN_INTEGER_ZERO_IDX;
      }
      break;
   }


   if ((TYP_TYPE(CN_TYPE_IDX(cn_idx)) == Character) &&
       ((IR_OPR(blank_pad_text) == Whole_Substring_Opr) ||
        (IR_OPR(blank_pad_text) == Substring_Opr))) {
      num_chars = CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(cn_idx)));
      l_idx = IR_IDX_R(blank_pad_text);   /* first list item */
      l_idx = IL_NEXT_LIST_IDX(l_idx);    /* second list item */
      l_idx = IL_NEXT_LIST_IDX(l_idx);    /* third list item */
      padd = CN_INT_TO_C(IL_IDX(l_idx));
      padd = padd - num_chars;
   }

   if (padd < 0) { /* need to truncate constant */

      /* JEFFL - Why does target_length come out of the table */
      /*         and go back in with no changes?              */
      /*         Also target_len will not hold a 64 bit int   */
      /*         on a 32 bit platform.  It needs too.         */

      target_length = CN_CONST(IL_IDX(l_idx));
      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX) = Character;
      TYP_LINEAR(TYP_WORK_IDX) = CHARACTER_DEFAULT_TYPE;
      TYP_CHAR_CLASS(TYP_WORK_IDX) = Const_Len_Char;
      TYP_FLD(TYP_WORK_IDX) = CN_Tbl_Idx;
      TYP_IDX(TYP_WORK_IDX) = ntr_const_tbl(CG_INTEGER_DEFAULT_TYPE,
                                            FALSE,
                                            &target_length);
      type_idx = ntr_type_tbl();

      new_str_idx = ntr_const_tbl(type_idx, TRUE, NULL);
      new_str_ptr = (char *) &CN_CONST(new_str_idx);
      old_str_ptr = (char *) &CN_CONST(idx);
      len = CN_INT_TO_C(TYP_IDX(type_idx));

      for (i = 0;  i < len;  i++) {
         new_str_ptr[i] = old_str_ptr[i];
      }

      idx = new_str_idx;
   }

   /* stack the constant */
   stack_data_constant = TRUE;
   cvrt_exp_to_pdg(idx, fld);
   stack_data_constant = FALSE;

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("fei_static_simple_init");
   PDG_DBG_PRINT_LLD("(1) rep_count", rep_count);
   PDG_DBG_PRINT_LLD("(2) stride", stride);
   PDG_DBG_PRINT_D("(3) ignore_types", ignore_types);
   PDG_DBG_PRINT_D("(4) unused", 0);
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
   fei_static_simple_init(rep_count, 
                          stride,
                          ignore_types,
                          0);
# endif

   if (padd > 0) { /* if blank padding necessary on a user implied DO */
      blank_padding(padd, (long64) (num_chars * CHAR_BIT));
   }

   TRACE (Func_Exit, "push_data_value", NULL);

   return;

}  /*  push_data_value  */


/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static void	cvrt_sytb_to_pdg(void)
{
   int		al_idx;
   int		attr_idx;
   int		name_idx;
   int		unused;
   int		scp_idx;


   TRACE (Func_Entry, "cvrt_sytb_to_pdg", NULL);

   /* Need PDG symbols for current program unit and all the parents, because */
   /* send_stor_blk may use them.  Assign PDG symbol ids, but do not send    */
   /* them over until the rest of the symbols have been sent thru.  This is  */
   /* because the Dummy Args may have been host associated and need to go    */
   /* through first so the defined, and inner_ref, def flags are set         */
   /* correctly.                                                             */

   scp_idx = curr_scp_idx;

   while (scp_idx != NULL_IDX) {
      attr_idx = SCP_ATTR_IDX(scp_idx);

      if (ATP_PGM_UNIT(attr_idx) == Module && scp_idx != curr_scp_idx) {
         break;		/* DO NOT SEND a parent across if it is a module. */
      }

      if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_next_func_idx");
         PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
         PDG_AT_IDX(attr_idx) = fei_next_func_idx(ATP_PGM_UNIT(attr_idx),
                                                  ATP_PROC(attr_idx),
                                                  ATP_ALT_ENTRY(attr_idx));
# endif
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_LD("(r) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
         PDG_DBG_PRINT_END    
      }
      else {
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_next_symbol");
         PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
         PDG_AT_IDX(attr_idx) = fei_next_symbol(TRUE);
# endif
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_LD("(r) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
         PDG_DBG_PRINT_END    
      }

      al_idx = SCP_ENTRY_IDX(scp_idx);
      while (al_idx != NULL_IDX) {
         attr_idx = AL_ATTR_IDX(al_idx);

         if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("fei_next_func_idx");
            PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
            PDG_AT_IDX(attr_idx) = fei_next_func_idx(ATP_PGM_UNIT(attr_idx),
                                                     ATP_PROC(attr_idx),
                                                     ATP_ALT_ENTRY(attr_idx));
# endif
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_LD("(r) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
            PDG_DBG_PRINT_END    
         }
         else {
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("fei_next_symbol");
            PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
            PDG_AT_IDX(attr_idx) = fei_next_symbol(TRUE);
# endif
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_LD("(r) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
            PDG_DBG_PRINT_END    
         }
         al_idx	= AL_NEXT_IDX(al_idx);
      }
      scp_idx = SCP_PARENT_IDX(scp_idx);
   }

   /*
   A stack segment must go over first for CCG.  If there is a host stack
   for this program unit, send it first.  The second argument is a pointer
   to a symbol class.  This will not be used because the STACK is in the
   current scope.  Pass a zero'd out integer field.
   */
   if (SCP_PARENT_IDX(curr_scp_idx) != NULL_IDX &&
       SCP_SB_HOSTED_STACK_IDX(SCP_PARENT_IDX(curr_scp_idx)) != NULL_IDX) {
      send_stor_blk(SCP_SB_HOSTED_STACK_IDX(SCP_PARENT_IDX(curr_scp_idx)),
                    &unused);
   }
   else if (SCP_SB_HOSTED_STACK_IDX(curr_scp_idx) != NULL_IDX) {
      send_stor_blk(SCP_SB_HOSTED_STACK_IDX(curr_scp_idx),
                    &unused);
   }
   else {
      send_stor_blk(SCP_SB_STACK_IDX(curr_scp_idx),
                    &unused);
   }

   /*
   Sends main entry point, alternate entry points, function
   results, all dummy arguments, and any accessed derived types.
   */
   send_procedure(SCP_ATTR_IDX(curr_scp_idx), 
                  SCP_ENTRY_IDX(curr_scp_idx),
                  Definition);

   for (name_idx = SCP_LN_FW_IDX(curr_scp_idx) + 1;
        name_idx < SCP_LN_LW_IDX(curr_scp_idx); name_idx++) {
      attr_idx = LN_ATTR_IDX(name_idx);
      send_attr_ntry(attr_idx);
   }

   al_idx = SCP_ATTR_LIST(curr_scp_idx);
   while (al_idx != NULL_IDX) {
      send_attr_ntry(AL_ATTR_IDX(al_idx));
      al_idx = AL_NEXT_IDX(al_idx);
   }

   /* Send all parent procedures. */
   scp_idx = SCP_PARENT_IDX(curr_scp_idx);
   while (scp_idx != NULL_IDX && 
          ATP_PGM_UNIT(SCP_ATTR_IDX(scp_idx)) != Module) {

      send_procedure(SCP_ATTR_IDX(scp_idx), 
                     SCP_ENTRY_IDX(scp_idx),
                     Parent);

      scp_idx = SCP_PARENT_IDX(scp_idx);
   }

   TRACE (Func_Exit, "cvrt_sytb_to_pdg", NULL);

   return;

}  /* cvrt_sytb_to_pdg */


#ifdef KEY /* Bug 6845 */
/*
 * ir_idx	IR_Tbl_Idx index for a dope vector operator
 * return	corresponding dv_idx_type for use with fei_{g,s}et_hdr_fld
 *		in SGI part of front end
 */
static dv_idx_type
opr_to_dv_hdr_fld(int ir_idx)
{
  switch (IR_OPR(ir_idx)) {
    case Dv_Set_Base_Addr :
    case Dv_Access_Base_Addr :
	 return DV_BASE_IDX;
	 break;
    case Dv_Set_El_Len :
    case Dv_Access_El_Len :
	 return DV_EL_LEN_IDX;
	 break;
    case Dv_Set_Assoc :
    case Dv_Access_Assoc :
	 return DV_ASSOC_IDX;
	 break;
    case Dv_Set_Ptr_Alloc :
    case Dv_Access_Ptr_Alloc :
	 return DV_PTR_ALLOC_IDX;
	 break;
    case Dv_Set_P_Or_A :
    case Dv_Access_P_Or_A :
	 return DV_P_OR_A_IDX;
	 break;
    case Dv_Set_A_Contig :
    case Dv_Access_A_Contig :
	 return DV_A_CONTIG_IDX;
	 break;
    case Dv_Set_N_Dim :
    case Dv_Access_N_Dim :
	 return DV_NUM_DIMS_IDX;
	 break;
    case Dv_Set_Typ_Code :
    case Dv_Access_Typ_Code :
	 return DV_TYPE_CODE_IDX;
	 break;
    case Dv_Set_Orig_Base :
    case Dv_Access_Orig_Base :
	 return DV_ORIG_BASE_IDX;
	 break;
    case Dv_Set_Orig_Size :
    case Dv_Access_Orig_Size :
	 return DV_ORIG_SIZE_IDX;
	 break;
    default:
         PRINTMSG(IR_LINE_NUM(ir_idx), 1044, Internal, IR_COL_NUM(ir_idx),
	   "opr_to_dv_hdr_fld");
	 return 0;
         break;
  }
}
#endif /* KEY Bug 6845 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static void	cvrt_exp_to_pdg(int         ir_idx,
                                fld_type    field)
{

   static	boolean			high_range_present;
   static	boolean			low_range_present;
   static	boolean			member;
   static	boolean			static_initialization;

		long_type               loc_value[MAX_WORDS_FOR_NUMERIC];
   		int			attr_idx;
   		int			type_idx;
                int			base_attr;
   		TYPE			basic;
		int			col;
   		TYPE			arg_type;
                INT32			big_int;
   		long64			bit_length 		= 0;
		boolean			bound_chk;
   		int			bound_idx;
   		int			cn_idx;
   		int			idx;
   		int			constant_class;
   		int			count;
   		int			dim;
   		long64			end;
		long64			flags 			= 0;
   		int      		j;
   		long64			i;
   		boolean			ignore_types;
   		long64			inc;
   		opnd_type		l_opnd;
   		long			first_task_idx		= NULL_IDX; /* by jhs, 02.9.25 */
   		long			last_task_idx		= NULL_IDX;
		int			line;
   		int			list_array[OPEN_MP_LIST_CNT];
   		int			list_cnt;
   		int			list_idx1;
   		int			list_idx2;
   		int			list_idx3;
   		int			list_idx4;
   		int			name_idx;
   		int			next_idx;
   		int			number_actual_args;
		long64			offset;
   		int			path_idx;
   		long			pdg_cn_idx;
   		long64			rep_count;
   		int			return_kind;
		boolean			save_symbolic_constant_expr;
   		int			search_idx;
   		int			ss;
   		long64			start;
   		INT64			static_subscripts[7];
   		long64			stride;
   		int			t_idx;
   		int			task_tbl_idx;
   		long			task_maxcpus_idx;
   		long			task_wdist_idx;
   		long			which;
   		long			assertion;
   		long			task_if_idx		= 0; /* by jhs, 02.9.5 */
   		long			task_num_threads_idx = 0; /* by jhs, 02.7.20 */
   		char			*criticalname  		= 0;
   		long			level    		= 0;
   		long			span    		= 1;
   		long			point    		= 0;
   		long			scheduletype		= 0;
   		long			schedulechunk		= 0; /* by jhs, 02.9.25 */
   		long			schedtype		= 0;
   		long			chunkcount		= 0;
   		long			threadcount		= 0;
   		long			datacount		= 0;
   		long			reductioncount		= 0;
   		long			ontocount		= 0;
   		long			ordered  		= 0;
   		long			defaultt  		= 0;
   		long			nowait   		= 0;
   		long			varcount  		= 0;
   		long			C_value  		= 0;
   		long			prefetch_manual 	= 0;
   		long			prefetch_array  	= 0;
   		long			redistribute_array  	= 0;
   		long			context  		= -1;
   		long			prefetch_size   	= -1;
   		long			prefetch_kind   	= -1;
   		int			stride_list     	= 0;
   		int			level_list      	= 0;
   		int			cyclic_list     	= 0;
   		int			onto_list       	= 0;
   		long			n1			= 0;
   		long			n2			= 0;
   		long			cyclic_exists   	= 0;
   		long			onto_exists     	= 0;
   		int			tmp_ir_idx;
   		int			trip;
   		TYPE			type_desc;
		boolean			unused;
   		long64			vv;
   		int			fld;
   		boolean			EXPAND			= FALSE;
		size_offset_type	offset1;
                size_offset_type	offset2;
		opnd_type		opnd;
                size_offset_type	result;
                int			pe_bd_idx;
   		long64			arith_constant[4];
   		int			arg_idx;
   		int			lhs_idx;
   		int			mask_idx;
                int			proc_idx;
   		int			rhs_idx;


   TRACE (Func_Entry, "cvrt_exp_to_pdg", NULL);

   switch (field) {
   
   case NO_Tbl_Idx :

      if (processing_call > 0) {
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_null_expr");
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_null_expr();
# endif
      }
      break;

   case SB_Tbl_Idx :
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_seg_ref");
      PDG_DBG_PRINT_S("SB_NAME_PTR", SB_NAME_PTR(ir_idx));
      PDG_DBG_PRINT_LD("(1) PDG_SB_IDX", PDG_SB_IDX(ir_idx));
      PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
      fei_seg_ref(PDG_SB_IDX(ir_idx));
# endif
# endif
      break;


   case CN_Tbl_Idx :
      constant_class = Arith_Const;

      if (data_attr != NULL_IDX &&
          stack_data_constant &&
          (TYP_TYPE(CN_TYPE_IDX(ir_idx)) == Integer ||
           TYP_TYPE(CN_TYPE_IDX(ir_idx)) == Real ||
           TYP_TYPE(CN_TYPE_IDX(ir_idx)) == Complex) &&
          TYP_LINEAR(ATD_TYPE_IDX(data_attr)) != CRI_Ptr_8 && 
          TYP_LINEAR(ATD_TYPE_IDX(data_attr)) != CRI_Ch_Ptr_8 && 
          TYP_LINEAR(ATD_TYPE_IDX(data_attr)) != 
          TYP_LINEAR(CN_TYPE_IDX(ir_idx))) {
         /* PDGCS does not like it if the value is not the same size as the   */
         /* target; for example, the value is a double precision constant and */
         /* the target is a single precision variable.  So explicitly convert */
         /* the value to the type and kind type parameter of the target for   */
         /* all combinations to be consistent.                                */

         type_idx = ATD_TYPE_IDX(data_attr);
         line = 1; col = 1;
         if (folder_driver( (char *) &CN_CONST(ir_idx),
                            CN_TYPE_IDX(ir_idx),
                            NULL,
                            NULL_IDX,
                            loc_value,
                           &type_idx,
                            line,
                            col,
                            1,
                            Cvrt_Opr)) 
            ir_idx = ntr_const_tbl(type_idx,
                                   FALSE,
                                   loc_value);
      }


      switch (TYP_TYPE(CN_TYPE_IDX(ir_idx))) {
         case Character :
            bit_length = CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(ir_idx)));

            if (stack_data_constant &&
                data_character_bit_length != 0 &&
                IL_FLD(data_character_bit_length) == CN_Tbl_Idx) {
               bit_length = CN_INT_TO_C(IL_IDX(data_character_bit_length));
               PDG_CN_IDX(ir_idx) = 0;
            }

            constant_class = Pattern_Const;
            bit_length = bit_length * CHAR_BIT;
            data_character_bit_length = 0;
            break;

         case Typeless :
            bit_length = TYP_BIT_LEN(CN_TYPE_IDX(ir_idx));
            constant_class = Pattern_Const;

            /*
            If the length of the typeless constant is TBPW,
            make the constant an arithmetic constant.
            */
            if (bit_length == TARGET_BITS_PER_WORD ||
                TYP_LINEAR(CN_TYPE_IDX(ir_idx)) == Typeless_4 ||
                TYP_LINEAR(CN_TYPE_IDX(ir_idx)) == Typeless_8) {
               constant_class = Arith_Const;
            }
            break;
      }

      /*
      If the constant has never been created; or, we have exceeded the
      size of the link table, we want to call fei_constant.      
      fei_constant will create a new constant and stack it on the 
      expression stack.   A call to fei_push_arith_con or fei_push_pattern_con
      assumes that the constant was already created and we can just stack
      the saved index for that particular constant.
      */
      if (PDG_CN_IDX(ir_idx) == 0 || ir_idx >= pdg_link_tbl_size) { 
         basic = get_basic_type(CN_TYPE_IDX(ir_idx), 0, NULL_IDX);


         if (symbolic_constant_expr) {
            PDG_DBG_PRINT_START
            PDG_DBG_PRINT_C("fei_arith_con");
            PDG_DBG_PRINT_T("(1) type", basic);
#if defined(_HOST32) && defined(_TARGET64)
            PDG_DBG_PRINT_VD("(2) const", CN_CONST(ir_idx));
#else
            PDG_DBG_PRINT_LVD("(2) const", CN_CONST(ir_idx));
#endif
            PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
            PDG_CN_IDX(ir_idx) = fei_arith_con(basic,
                                               (long *)&CN_CONST(ir_idx));
# endif
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_LD("(r) PDG_CN_IDX", PDG_CN_IDX(ir_idx));
            PDG_DBG_PRINT_END    

            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("npex_constant");
            PDG_DBG_PRINT_LD("(1) PDG_CN_IDX", PDG_CN_IDX(ir_idx));
            PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
            npex_constant(PDG_CN_IDX(ir_idx));
# endif
         }
         else {
            PDG_DBG_PRINT_START
            PDG_DBG_PRINT_C("fei_constant");
            PDG_DBG_PRINT_T("(1) type", basic);
#if defined(_HOST32) && defined(_TARGET64)
            PDG_DBG_PRINT_S("(2) class", p_const_class[constant_class]);
            PDG_DBG_PRINT_VD("(3) const", CN_CONST(ir_idx));
            PDG_DBG_PRINT_VD("(4) bit length", bit_length);
#else
            PDG_DBG_PRINT_S("(2) class", p_const_class[constant_class]);
            PDG_DBG_PRINT_LVD("(3) const", CN_CONST(ir_idx));
            PDG_DBG_PRINT_LLD("(4) bit length", bit_length);
#endif
            PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI

# if defined(_TARGET_SV2)
            if (constant_class == Arith_Const) {
               if (TYP_LINEAR(CN_TYPE_IDX(ir_idx)) == Complex_4 ||
                   TYP_LINEAR(CN_TYPE_IDX(ir_idx)) == Integer_8 ||
                   TYP_LINEAR(CN_TYPE_IDX(ir_idx)) == Real_8) {
                  /* pack the constant up into one 64 bit word */
                  arith_constant[0] = CN_CONST(ir_idx);
                  arith_constant[0] = arith_constant[0] << 32;
                  arith_constant[0] |= CP_CONSTANT(CN_POOL_IDX(ir_idx) + 1);
               }
               else if (TYP_LINEAR(CN_TYPE_IDX(ir_idx)) == Complex_8 ||
                        TYP_LINEAR(CN_TYPE_IDX(ir_idx)) == Real_16) {
                  /* pack the constant up into two 64 bit words */
                  arith_constant[0] = CN_CONST(ir_idx);
                  arith_constant[0] = arith_constant[0] << 32;
                  arith_constant[0] |= CP_CONSTANT(CN_POOL_IDX(ir_idx) + 1);
                  arith_constant[1] = CP_CONSTANT(CN_POOL_IDX(ir_idx) + 2);
                  arith_constant[1] = arith_constant[1] << 32;
                  arith_constant[1] |= CP_CONSTANT(CN_POOL_IDX(ir_idx) + 3);
               }
               else if (TYP_LINEAR(CN_TYPE_IDX(ir_idx)) == Complex_16) {
                  /* pack the constant up into four 64 bit words */
                  arith_constant[0] = CN_CONST(ir_idx);
                  arith_constant[0] = arith_constant[0] << 32;
                  arith_constant[0] |= CP_CONSTANT(CN_POOL_IDX(ir_idx) + 1);
                  arith_constant[1] = CP_CONSTANT(CN_POOL_IDX(ir_idx) + 2);
                  arith_constant[1] = arith_constant[1] << 32;
                  arith_constant[1] |= CP_CONSTANT(CN_POOL_IDX(ir_idx) + 3);
                  arith_constant[2] = CP_CONSTANT(CN_POOL_IDX(ir_idx) + 4);
                  arith_constant[2] = arith_constant[2] << 32;
                  arith_constant[2] |= CP_CONSTANT(CN_POOL_IDX(ir_idx) + 5);
                  arith_constant[3] = CP_CONSTANT(CN_POOL_IDX(ir_idx) + 6);
                  arith_constant[3] = arith_constant[3] << 32;
                  arith_constant[3] |= CP_CONSTANT(CN_POOL_IDX(ir_idx) + 7);
               }
               else {
                  arith_constant[0] = CN_CONST(ir_idx);
               }

               pdg_cn_idx = fei_constant(basic,
                                         constant_class,
                                         (char *) &arith_constant,
                                         bit_length);
            }
            else {  /* Pattern_Const */
               pdg_cn_idx = fei_constant(basic,
                                         constant_class,
                                         (char *) &CN_CONST(ir_idx),
                                         bit_length);
            }

# elif defined(_TARGET_OS_MAX)  /* JEFFL */
            if (TYP_LINEAR(CN_TYPE_IDX(ir_idx)) == Complex_4) {
               /* pack the complex constant up into one word */
               i = CN_CONST(ir_idx) << 32;
               i |= CP_CONSTANT(CN_POOL_IDX(ir_idx) + 1);
               pdg_cn_idx = fei_constant(basic,
                                         constant_class,
                                         (char *) &i,
                                         bit_length);
            }
            else {
               pdg_cn_idx = fei_constant(basic,
                                         constant_class,
                                         (char *) &CN_CONST(ir_idx),
                                         bit_length);
            }
# else
            pdg_cn_idx = fei_constant(basic,
                                      constant_class,
                                      (char *) &CN_CONST(ir_idx),
                                      bit_length);
# endif

# endif
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_LD("(r) PDG_CN_IDX", pdg_cn_idx);
            PDG_DBG_PRINT_END    

            if (ir_idx < pdg_link_tbl_size) {
               PDG_CN_IDX(ir_idx) = pdg_cn_idx;
            }
         }
      }
      else {
         if (constant_class == Pattern_Const) {
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("fei_push_pattern_con");
            PDG_DBG_PRINT_LD("(1) PDG_CN_IDX", PDG_CN_IDX(ir_idx));
            PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
            fei_push_pattern_con(PDG_CN_IDX(ir_idx));
# endif
         }
         else if (symbolic_constant_expr) {
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("npex_constant");
            PDG_DBG_PRINT_LD("(1) PDG_CN_IDX", PDG_CN_IDX(ir_idx));
            PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
            npex_constant(PDG_CN_IDX(ir_idx));
# endif
         }
         else {
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("fei_push_arith_con");
            PDG_DBG_PRINT_LD("(1) PDG_CN_IDX", PDG_CN_IDX(ir_idx));
            PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
            fei_push_arith_con(PDG_CN_IDX(ir_idx));
# endif
         }
      }
      break;


   case IL_Tbl_Idx :

      while (ir_idx != NULL_IDX) {
         cvrt_exp_to_pdg(IL_IDX(ir_idx), 
                         IL_FLD(ir_idx));
         ir_idx = IL_NEXT_LIST_IDX(ir_idx);
      }

      break;


   case AT_Tbl_Idx :
      send_attr_ntry(ir_idx);

# if defined(_TARGET_OS_MAX)

      if (AT_OBJ_CLASS(ir_idx) == Data_Obj && ATD_SYMBOLIC_CONSTANT(ir_idx)) {

         if (ATD_CLASS(ir_idx) == Variable) {         /* N$PEs */

            if (symbolic_constant_expr) {
               PDG_DBG_PRINT_START    
               PDG_DBG_PRINT_C("npex_npes");
               PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
               npex_npes();
# endif
            }
            else {
               PDG_DBG_PRINT_START    
               PDG_DBG_PRINT_C("fei_n_pes");
               PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
               fei_n_pes();
# endif
            }
         }
         else {

            if (symbolic_constant_expr) {
               PDG_DBG_PRINT_START    
               PDG_DBG_PRINT_C("npex_push");
               PDG_DBG_PRINT_D("(1) PDG_AT_IDX", PDG_AT_IDX(ir_idx));
               PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
               npex_push(PDG_AT_IDX(ir_idx));
# endif
            }
            else {
               PDG_DBG_PRINT_START    
               PDG_DBG_PRINT_C("npex_to_expr");
               PDG_DBG_PRINT_D("PDG_AT_IDX", PDG_AT_IDX(ir_idx));
               PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
               npex_to_expr(PDG_AT_IDX(ir_idx));
# endif
            }
         }
         break;
      }
# endif


      if (AT_OBJ_CLASS(ir_idx) == Data_Obj) {
         if (ATD_CLASS(ir_idx) == Compiler_Tmp &&
             ATD_TMP_INIT_NOT_DONE(ir_idx)) {
            insert_init_stmt_for_tmp(ir_idx);
         }

         if (data_attr == NULL_IDX) {
            data_attr = ir_idx;
         }

         if (stack_data_object && member) {
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("fei_static_member");
            PDG_DBG_PRINT_S("AT_OBJ_NAME", AT_OBJ_NAME_PTR(ir_idx));
            PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(ir_idx));
            PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
            fei_static_member(PDG_AT_IDX(ir_idx)); 
# endif
         }
         else if (stack_data_object) {
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("fei_static_base");
            PDG_DBG_PRINT_S("AT_OBJ_NAME", AT_OBJ_NAME_PTR(ir_idx));
            PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(ir_idx));
            PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
            fei_static_base(PDG_AT_IDX(ir_idx)); 
# endif
         }
         else if (member) {
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("fei_member_ref");
            PDG_DBG_PRINT_S("AT_OBJ_NAME", AT_OBJ_NAME_PTR(ir_idx));
            PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(ir_idx));
            PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
            fei_member_ref(PDG_AT_IDX(ir_idx)); 
# endif
         }
         else {
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("fei_object_ref");
            PDG_DBG_PRINT_S("AT_OBJ_NAME", AT_OBJ_NAME_PTR(ir_idx));
            PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(ir_idx));
            PDG_DBG_PRINT_D("(2) whole_subscript", whole_subscript);
            PDG_DBG_PRINT_D("(3) whole_substring", whole_substring);
            PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
            fei_object_ref(PDG_AT_IDX(ir_idx),
                           whole_subscript,
                           whole_substring);
# endif
         }
      }
      else if (AT_OBJ_CLASS(ir_idx) == Namelist_Grp) {
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_namelist_ref");
         PDG_DBG_PRINT_S("AT_OBJ_NAME", AT_OBJ_NAME_PTR(ir_idx));
         PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(ir_idx));
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_namelist_ref(PDG_AT_IDX(ir_idx)); 
# endif
      }
      else if (AT_OBJ_CLASS(ir_idx) == Pgm_Unit &&
               ATP_PROC(ir_idx) == Dummy_Proc) {
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_object_ref");
         PDG_DBG_PRINT_S("AT_OBJ_NAME", AT_OBJ_NAME_PTR(ir_idx));
         PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(ir_idx));
         PDG_DBG_PRINT_D("(2) whole_subscript", whole_subscript);
         PDG_DBG_PRINT_D("(3) whole_substring", whole_substring);
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_object_ref(PDG_AT_IDX(ir_idx),
                        whole_subscript,
                        whole_substring);
# endif
      }
      else if (AT_OBJ_CLASS(ir_idx) == Pgm_Unit) {
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_function_ref");
         PDG_DBG_PRINT_S("AT_OBJ_NAME", AT_OBJ_NAME_PTR(ir_idx));
         PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(ir_idx));
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_function_ref(PDG_AT_IDX(ir_idx)); 
# endif
      }
      else if (AT_OBJ_CLASS(ir_idx) == Label) {
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_label_ref");
         PDG_DBG_PRINT_S("AT_OBJ_NAME", AT_OBJ_NAME_PTR(ir_idx));
         PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(ir_idx));
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_label_ref(PDG_AT_IDX(ir_idx));
# endif
      }

      break;



   case IR_Tbl_Idx :

# ifdef _DEBUG
      if (IR_TYPE_IDX(ir_idx) == NULL_IDX) {
         print_ir(ir_idx);
         PRINTMSG(IR_LINE_NUM(ir_idx), 993, Internal,
                  IR_COL_NUM(ir_idx));
      }

      if (IR_LINE_NUM(ir_idx) <= 0) {
         print_ir(ir_idx);
         PRINTMSG(1, 1150, Internal, IR_COL_NUM(ir_idx));
      }

      if ((IR_FLD_L(ir_idx) == AT_Tbl_Idx ||
           IR_FLD_L(ir_idx) == CN_Tbl_Idx) &&
          IR_LINE_NUM_L(ir_idx) <= 0) {
         print_ir(ir_idx);
         PRINTMSG(1, 1150, Internal, IR_COL_NUM(ir_idx));
      }

      if ((IR_FLD_R(ir_idx) == AT_Tbl_Idx ||
           IR_FLD_R(ir_idx) == CN_Tbl_Idx) &&
          IR_LINE_NUM_R(ir_idx) <= 0) {

         print_ir(ir_idx);
         PRINTMSG(1, 1150, Internal, IR_COL_NUM(ir_idx));
      }

      if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
         list_idx1 = IR_IDX_L(ir_idx);

         while (list_idx1) {
            if ((IL_FLD(list_idx1) == AT_Tbl_Idx ||
                 IL_FLD(list_idx1) == CN_Tbl_Idx) &&
                IL_LINE_NUM(list_idx1) <= 0) {

               print_ir(ir_idx);
               PRINTMSG(1, 1150, Internal, IR_COL_NUM(ir_idx));
            }
            list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
         }
      }

      if (IR_FLD_R(ir_idx) == IL_Tbl_Idx) {
         list_idx1 = IR_IDX_R(ir_idx);

         while (list_idx1) {
            if ((IL_FLD(list_idx1) == AT_Tbl_Idx ||
                 IL_FLD(list_idx1) == CN_Tbl_Idx) &&
                IL_LINE_NUM(list_idx1) <= 0) {

               print_ir(ir_idx);
               PRINTMSG(1, 1150, Internal, IR_COL_NUM(ir_idx));
            }
            list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
         }
      }
# endif

      switch (IR_OPR(ir_idx)) { 

      case Stmt_Expansion_Opr:
# ifdef _DEBUG
         PRINTMSG(IR_LINE_NUM(ir_idx), 626, Internal, IR_COL_NUM(ir_idx),
                  "no Stmt_Expansion_Opr", "cvrt_exp_to_pdg");
# endif
         /* send the left opnd anyway */
         cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                         IR_FLD_L(ir_idx));
         break;

      case Case_Range_Opr :
         if (IR_IDX_L(ir_idx) == NULL_IDX) {
            low_range_present = FALSE;
         } 
         else {
            cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                            IR_FLD_L(ir_idx));
            low_range_present = TRUE;
         }

         if (IR_IDX_R(ir_idx) == NULL_IDX) {
            high_range_present = FALSE;
         } 
         else {
            cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                            IR_FLD_R(ir_idx));
            high_range_present = TRUE;
         }
         break;




      case Case_Opr :
         if (IR_IDX_L(ir_idx) != NULL_IDX) { 
            cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                            IR_FLD_L(ir_idx));

            big_int     = CN_INT_TO_C(IR_IDX_R(ir_idx));

            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("fei_new_select_case");
            PDG_DBG_PRINT_S("(1) low_range", p_boolean[low_range_present]);
            PDG_DBG_PRINT_S("(2) high_range", p_boolean[high_range_present]);
#if defined(_HOST32) && defined(_TARGET64)
            PDG_DBG_PRINT_D("(3) const", big_int);
#else
            PDG_DBG_PRINT_VD("(3) const", big_int);
#endif
            PDG_DBG_PRINT_END    


# ifdef _ENABLE_FEI
            fei_new_select_case(low_range_present,
                                high_range_present,
                                big_int);
# endif
            low_range_present = FALSE;
            high_range_present = FALSE;
         }
         break;





      case Select_Opr :
         cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                         IR_FLD_L(ir_idx));

         list_idx1 = IR_IDX_R(ir_idx);
         list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
         list_idx3 = IL_NEXT_LIST_IDX(list_idx2);

         if (IR_LIST_CNT_R(ir_idx) == 2) {
            list_idx3 = list_idx2;
         }

         send_attr_ntry(IL_IDX(list_idx3));
         big_int	= CN_INT_TO_C(IL_IDX(list_idx1));

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_new_select");
#if defined(_HOST32) && defined(_TARGET64)
         PDG_DBG_PRINT_D("(1) num cases", big_int);
#else
         PDG_DBG_PRINT_VD("(1) num cases", big_int);
#endif
         PDG_DBG_PRINT_LD("(2) default label", PDG_AT_IDX(IL_IDX(list_idx3)));
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_new_select(big_int,
#ifdef KEY /* Bug 12319 */
                        PDG_AT_IDX(IL_IDX(list_idx2)),
#endif /* KEY Bug 12319 */
                        PDG_AT_IDX(IL_IDX(list_idx3)));
# endif
         break;




      case Ptr_Asg_Opr :
         cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                         IR_FLD_L(ir_idx));

         cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                         IR_FLD_R(ir_idx));

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_dv_ptr_asg");
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_dv_ptr_asg();
# endif
         break;






      case Length_Opr :
         cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                         IR_FLD_L(ir_idx));

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_length");
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_length();
# endif
         break;




      case Getpos_Opr :
         cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                         IR_FLD_L(ir_idx));

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_getpos");
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_getpos();
# endif
         break;



      case Unit_Opr :
         cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                         IR_FLD_L(ir_idx));

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_unit");
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_unit();
# endif
         break;






      case Index_Opr :
         cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                         IR_FLD_L(ir_idx));

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_index");
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_index();
# endif
         break;




   case Cmplx_Opr :
         cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                         IR_FLD_L(ir_idx));

         basic = get_basic_type(IR_TYPE_IDX(ir_idx), 0, NULL_IDX);

         PDG_DBG_PRINT_START
         PDG_DBG_PRINT_C("fei_complex");
         PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
         fei_complex(basic);
# endif
         break;




      case Concat_Opr :
         cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                         IR_FLD_L(ir_idx));

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_concat");
         PDG_DBG_PRINT_D("(1) num ops", IR_LIST_CNT_L(ir_idx));
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_concat(IR_LIST_CNT_L(ir_idx));
# endif
         break;







   case Lock_Release_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_lock_release");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_lock_release();
# endif
# endif

        break;




   case Omp_Test_Lock_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_omp_test_lock");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_omp_test_lock();
# endif
        break;




   case Omp_Set_Lock_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_omp_set_lock");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_omp_set_lock();
# endif
        break;





   case Omp_Unset_Lock_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_omp_unset_lock");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_omp_unset_lock();
# endif
        break;






   case Synchronize_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_synchronize");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_synchronize();
# endif
# endif

        break;




   case Compare_And_Swap_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_compare_and_swap");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_compare_and_swap(basic);
# endif
# endif

        break;




   case Lock_Test_And_Set_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_lock_test_and_set");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_lock_test_and_set(basic);
# endif
# endif

        break;





   case Fetch_And_Add_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_fetch_and_add");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_fetch_and_add(basic);
# endif
# endif

        break;




   case Add_And_Fetch_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_add_and_fetch");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_add_and_fetch(basic);
# endif
# endif

        break;





   case Fetch_And_And_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_fetch_and_and");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_fetch_and_and(basic);
# endif
# endif

        break;





   case And_And_Fetch_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_and_and_fetch");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_and_and_fetch(basic);
# endif
# endif

        break;





   case Fetch_And_Nand_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_fetch_and_nand");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_fetch_and_nand(basic);
# endif
# endif

        break;





   case Nand_And_Fetch_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_nand_and_fetch");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_nand_and_fetch(basic);
# endif
# endif

        break;






   case Fetch_And_Or_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_fetch_and_or");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_fetch_and_or(basic);
# endif
# endif

        break;




   case Or_And_Fetch_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_or_and_fetch");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_or_and_fetch(basic);
# endif
# endif

        break;




   case Fetch_And_Sub_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_fetch_sub_add");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_fetch_and_sub(basic);
# endif
# endif

        break;




   case Sub_And_Fetch_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_sub_and_fetch");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_sub_and_fetch(basic);
# endif
# endif

        break;






   case Fetch_And_Xor_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_fetch_and_xor");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_fetch_and_xor(basic);
# endif
# endif

        break;




   case Xor_And_Fetch_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_xor_and_fetch");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_xor_and_fetch(basic);
# endif
# endif

        break;








   case Mmx_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_mbmx");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_mbmx();
# endif
        
        break;






   case Mldmx_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_mbmxl");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_mbmxl();
# endif
        
        break;







   case Mld_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_mbld");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_mbld();
# endif
        
        break;







   case Mul_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_mbul");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_mbul();
# endif
        break;






   case Mcbl_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_mbclr");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_mbclr();
# endif

        break;








   case Real_Div_To_Int_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_ridiv");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_ridiv();
# endif
        
        break;











   case Modulo_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_modulo");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_modulo(basic);
# endif
        
        break;








   case Len_Trim_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_len_trim");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_len_trim();
# endif
        
        break;







   case Nearest_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_near");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_near(basic);
# endif
        
        break;






   case Rrspacing_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_rrspace");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_rrspace(basic);
# endif
        
        break;






   case Scale_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_scale");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_scale(basic);
# endif
        
        break;






   case Scan_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_scan");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_scan(basic);
# endif
        
        break;







   case Set_Exponent_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_set_exponent");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_set_exponent(basic);
# endif
        
        break;







   case Verify_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_verify");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_verify(basic);
# endif
        
        break;









   case Spacing_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_space");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_space(basic);
# endif
        
        break;








   case Ceiling_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_ceiling");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_ceiling(basic);
# endif
        
        break;


 




   case Adjustl_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_adjustl");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_adjustl(basic);
# endif
        
        break;





   case Adjustr_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_adjustr");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_adjustr(basic);
# endif
        
        break;






   case Exponent_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_exponent");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_exponent(basic);
# endif
        
        break;





   case Fraction_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_fraction");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_fraction();
# endif
        
        break;








#ifdef KEY /* Bug 10410 */
   case Cselect_Opr :
#endif /* KEY Bug 10410 */
   case Cvmgt_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_select");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
#ifdef KEY /* Bug 10410 */
        fei_select(basic, Cselect_Opr == IR_OPR(ir_idx));
#else /* KEY Bug 10410 */
        fei_select(basic);
#endif /* KEY Bug 10410 */
# endif
        
        break;





   case Csmg_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_mbits");
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_mbits(basic);
# endif
        
        break;






   case Rtc_Opr :
        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_rtc");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_rtc(basic);
# endif
        
        break;



 
   case My_Pe_Opr :
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_my_pe");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_my_pe();
# endif

        break;
 




   case Numarg_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_numargs");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_numargs(basic);
# endif
        
        break;






# ifdef _HIGH_LEVEL_DO_LOOP_FORM

   case Loop_Info_Opr :
        switch (SH_STMT_TYPE(curr_sh)) {
           case Do_Iterative_Stmt :

              /* SH for the stmt that ends the loop. */

              line = (IR_FLD_L(ir_idx) == SH_Tbl_Idx) ?
                                          SH_GLB_LINE(IR_IDX_L(ir_idx)) : 0;

              if (IL_FLD(IR_IDX_R(ir_idx)) == IL_Tbl_Idx) {
                 idx = IL_IDX(IR_IDX_R(ir_idx));
              }
              else {
                 idx = IR_IDX_R(ir_idx);
              }
              cvrt_exp_to_pdg(IL_IDX(idx),    /* LCV */
                              IL_FLD(idx));

              idx = IL_NEXT_LIST_IDX(idx);
              cvrt_exp_to_pdg(IL_IDX(idx),    /* stack start */
                              IL_FLD(idx));

              idx = IL_NEXT_LIST_IDX(idx); 
              cvrt_exp_to_pdg(IL_IDX(idx),    /* stack end */
                              IL_FLD(idx));

              idx = IL_NEXT_LIST_IDX(idx);
              cvrt_exp_to_pdg(IL_IDX(idx),    /* stack inc */
                              IL_FLD(idx));


              PDG_DBG_PRINT_START    
              PDG_DBG_PRINT_C("fei_doloop");
              PDG_DBG_PRINT_D("(1) line", line);
              PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
              fei_doloop(line);
# endif
              break;


           case Do_While_Stmt :
              idx = IR_IDX_R(ir_idx);
              cvrt_exp_to_pdg(IL_IDX(idx),    /* condition */
                              IL_FLD(idx));

              PDG_DBG_PRINT_START
              PDG_DBG_PRINT_C("fei_dowhile");
              PDG_DBG_PRINT_END
 
# ifdef _ENABLE_FEI
              fei_dowhile();
# endif
              break;


           case Do_Infinite_Stmt :
              PDG_DBG_PRINT_START
              PDG_DBG_PRINT_C("fei_doforever");
              PDG_DBG_PRINT_END
 
# ifdef _ENABLE_FEI
              fei_doforever();
# endif
              break;

        }
        break;




   case Loop_End_Opr :
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_enddo");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_enddo();
# endif
        break;
# endif





# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   case Mvbits_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_mvbits");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_mvbits(basic);
# endif
        
        break;


   case Ishftc_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_ishftc");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_ishftc(basic);
# endif
        
        break;



   case Ibits_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_ibits");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_ibits(basic);
# endif
        
        break;
# endif




   case Abs_Opr :
   case Paren_Opr :
   case Const_Tmp_Loc_Opr :
   case Loc_Opr :
   case Ieee_Finite_Opr :
   case Ieee_Is_Nan_Opr :
   case Ieee_Class_Opr :
   case Uplus_Opr :
   case Uminus_Opr :
   case Not_Opr :
   case Bnot_Opr :
   case Floor_Opr :
   case Clen_Opr :
   case Conjg_Opr :
   case Mask_Opr :
   case Int_Opr :
   case Logical_Opr :
   case Real_Opr :
   case Ichar_Opr :
   case Char_Opr :
   case Cvrt_Opr :
   case Cvrt_Unsigned_Opr :
   case Ranget_Opr :
   case Ranset_Opr :
   case Dim_Opr :
   case Aint_Opr :
   case Leadz_Opr :
   case Poppar_Opr :
   case Popcnt_Opr :
   case Nint_Opr :
   case Anint_Opr :
   case Sign_Opr :
   case Cot_Opr :
   case Exp_Opr :
   case Sqrt_Opr :
   case Acos_Opr :
   case Asin_Opr :
   case Atan_Opr :
   case Aimag_Opr :
   case Atan2_Opr :
   case Cosh_Opr :
   case Sinh_Opr :
   case Tanh_Opr :
   case Cos_Opr :
   case Sin_Opr :
   case Tan_Opr :
   case Log_E_Opr :
   case Log_10_Opr :
   case Cosd_Opr :
   case Sind_Opr :
   case Tand_Opr :
   case Acosd_Opr :
   case Asind_Opr :
   case Atand_Opr :
   case Atan2d_Opr :

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        switch (IR_OPR(ir_idx)) {
        case Abs_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_abs");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_abs(basic);
# endif
             break;

        case Paren_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_paren");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_paren(basic);
# endif
             break;

        case Const_Tmp_Loc_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_addr_con");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_addr_con(basic);
# endif
             break;

        case Loc_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_addr");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_addr(basic);
# endif
             break;

        case Ieee_Finite_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_isfinite");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_isfinite(basic);
# endif
             break;

        case Ieee_Is_Nan_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_isnan");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_isnan(basic);
# endif
             break;

        case Ieee_Class_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_fpclass");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_fpclass(basic);
# endif
             break;

        case Uplus_Opr :
             break;

        case Uminus_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_uminus");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_uminus(basic);
# endif
             break;

        case Not_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_lneg");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_lneg(basic);
# endif
             break;

        case Bnot_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_bneg");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_bneg(basic);
# endif
             break;

        case Floor_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_floor");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_floor(basic);
# endif
             break;

        case Clen_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_len");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_len(basic);
# endif
             break;

        case Conjg_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_conjg");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_conjg(basic);
# endif
             break;

        case Mask_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_mask");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_mask(basic);
# endif
             break;

        case Int_Opr :
        case Logical_Opr :
        case Real_Opr :
        case Ichar_Opr :
        case Char_Opr :
        case Cvrt_Opr :
        case Cvrt_Unsigned_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_cvtop");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_cvtop(basic);
# endif
             break;

        case Ranget_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_ranget");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_ranget(basic);
# endif
             break;

        case Ranset_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_ranset");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_ranset(basic);
# endif
             break;

        case Dim_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_pos_diff");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_pos_diff(basic);
# endif
             break;

        case Aint_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_trunc");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_trunc(basic);
# endif
             break;

        case Leadz_Opr :
             arg_type	= null_type;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
             if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
                arg_idx = IR_IDX_L(ir_idx);

                if (IL_FLD(arg_idx) == AT_Tbl_Idx) {
                   if (AT_OBJ_CLASS(IL_IDX(arg_idx)) == Data_Obj) {
                      arg_type = get_basic_type(ATD_TYPE_IDX(IL_IDX(arg_idx)),
                                                0,
                                                NULL_IDX);
                   }
                }
                else if (IL_FLD(arg_idx) == CN_Tbl_Idx) {
                   arg_type = get_basic_type(CN_TYPE_IDX(IL_IDX(arg_idx)),
                                             0,
                                             NULL_IDX);
                }
                else if (IL_FLD(arg_idx) == IR_Tbl_Idx) {
                   arg_type = get_basic_type(IR_TYPE_IDX(IL_IDX(arg_idx)),
                                             0,
                                             NULL_IDX);
                }
             }
# endif

             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_leadz");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_leadz(basic, arg_type);
# endif
             break;

        case Poppar_Opr :
             arg_type	= null_type;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
             if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
                arg_idx = IR_IDX_L(ir_idx);

                if (IL_FLD(arg_idx) == AT_Tbl_Idx) {
                   if (AT_OBJ_CLASS(IL_IDX(arg_idx)) == Data_Obj) {
                      arg_type = get_basic_type(ATD_TYPE_IDX(IL_IDX(arg_idx)),
                                                0,
                                                NULL_IDX);
                   }
                }
                else if (IL_FLD(arg_idx) == CN_Tbl_Idx) {
                   arg_type = get_basic_type(CN_TYPE_IDX(IL_IDX(arg_idx)),
                                             0,
                                             NULL_IDX);
                }
                else if (IL_FLD(arg_idx) == IR_Tbl_Idx) {
                   arg_type = get_basic_type(IR_TYPE_IDX(IL_IDX(arg_idx)),
                                             0,
                                             NULL_IDX);
                }
             }
# endif

             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_poppar");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_poppar(basic, arg_type);
# endif
             break;

        case Popcnt_Opr :
             arg_type	= null_type;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
             if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
                arg_idx = IR_IDX_L(ir_idx);

                if (IL_FLD(arg_idx) == AT_Tbl_Idx) {
                   if (AT_OBJ_CLASS(IL_IDX(arg_idx)) == Data_Obj) {
                      arg_type = get_basic_type(ATD_TYPE_IDX(IL_IDX(arg_idx)),
                                                0,
                                                NULL_IDX);
                   }
                }
                else if (IL_FLD(arg_idx) == CN_Tbl_Idx) {
                   arg_type = get_basic_type(CN_TYPE_IDX(IL_IDX(arg_idx)),
                                             0,
                                             NULL_IDX);
                }
                else if (IL_FLD(arg_idx) == IR_Tbl_Idx) {
                   arg_type = get_basic_type(IR_TYPE_IDX(IL_IDX(arg_idx)),
                                             0,
                                             NULL_IDX);
                }
             }
# endif

             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_popcnt");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_popcnt(basic, arg_type);
# endif
             break;

        case Nint_Opr :
        case Anint_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_round");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_round(basic);
# endif
             break;

        case Sign_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_sign_xfer");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_sign_xfer(basic);
# endif
             break;

        case Cot_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_cot");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_cot(basic);
# endif
             break;

        case Exp_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_exp");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_exp(basic);
# endif
             break;

        case Sqrt_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_sqrt");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_sqrt(basic);
# endif
             break;

        case Acos_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_acos");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_acos(basic);
# endif
             break;

        case Asin_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_asin");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_asin(basic);
# endif
             break;

        case Atan_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_atan");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_atan(basic);
# endif
             break;

        case Aimag_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_imag");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_imag(basic);
# endif
             break;

        case Atan2_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_atan2");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_atan2(basic);
# endif
             break;

        case Cosh_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_cosh");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_cosh(basic);
# endif
             break;

        case Sinh_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_sinh");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_sinh(basic);
# endif
             break;

        case Tanh_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_tanh");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_tanh(basic);
# endif
             break;

        case Cos_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_cos");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_cos(basic);
# endif
             break;

        case Sin_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_sin");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_sin(basic);
# endif
             break;

        case Tan_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_tan");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_tan(basic);
# endif
             break;

        case Log_E_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_log");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_log(basic);
# endif
             break;

        case Log_10_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_log10");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_log10(basic);
# endif
             break;

   	case Cosd_Opr :  
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_cosd");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_cosd(basic);
# endif
             break;

   	case Sind_Opr :  
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_sind");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_sind(basic);
# endif
             break;

   	case Tand_Opr :  
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_tand");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_tand(basic);
# endif
             break;

   	case Acosd_Opr : 
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_acosd");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_acosd(basic);
# endif
             break;

   	case Asind_Opr : 
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_asind");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_asind(basic);
# endif
             break;

   	case Atand_Opr : 
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_atand");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_atand(basic);
# endif
             break;

   	case Atan2d_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_atan2d");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_atan2d(basic);
# endif
             break;
        }
        break;



#ifdef KEY /* Bug 1324 */
   case Erf_Opr :
	cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
			IR_FLD_L(ir_idx));
        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);
	PDG_DBG_PRINT_START
	PDG_DBG_PRINT_C("fei_erf");
	PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
	fei_erf(basic, 0);
# endif
	break;

   case Erfc_Opr :
	cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
			IR_FLD_L(ir_idx));
        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);
	PDG_DBG_PRINT_START
	PDG_DBG_PRINT_C("fei_erfc");
	PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
	fei_erf(basic, 1);
# endif
	break;
#endif /* KEY Bug 1324 */






   case Alt_Return_Opr :
   case Asg_Opr :
   case Dv_Whole_Copy_Opr :
   case Dv_Def_Asg_Opr :
        if ((IR_FLD_L(ir_idx) == AT_Tbl_Idx) &&
            (AT_OBJ_CLASS(IR_IDX_L(ir_idx)) == Label)) {

           cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                           IR_FLD_R(ir_idx));

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_label_addr");
           PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(IR_IDX_L(ir_idx)));
           PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
           fei_label_addr(PDG_AT_IDX(IR_IDX_L(ir_idx)));
# endif
        }
        else {
           cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                           IR_FLD_L(ir_idx));

           cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                           IR_FLD_R(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_store");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_store(pdg_type_void);
# endif
        break;





   case Flat_Array_Asg_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_non_conform_store");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_non_conform_store(pdg_type_void);
# endif
        break;


# ifdef _SAVE_IO_STMT
   case Start_Io_Opr :
      cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                      IR_FLD_L(ir_idx));

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_start_ioblock");
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      fei_start_ioblock();
# endif
      break;



   case End_Io_Opr :
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_end_ioblock");
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      fei_end_ioblock();
# endif
      break;
# endif




   case Readsm_Opr :
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_readsm");
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      fei_readsm();
# endif
      break;
   


   case Read_Unformatted_Opr :
      io_type = READ_STMT;

      processing_call = processing_call + 1;
      processing_io_stmt = TRUE;
      cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                      IR_FLD_L(ir_idx));

      PDG_DBG_PRINT_START    
      PDG_DBG_PRINT_C("fei_control_list");
      PDG_DBG_PRINT_S("(1) io_type", p_io[io_type]);
      PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
      fei_control_list(io_type);
# endif
       
     cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                     IR_FLD_R(ir_idx));

     if (IR_IDX_R(ir_idx) != NULL_IDX) {
        list_cnt = IR_LIST_CNT_R(ir_idx);

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_IO_list");
        PDG_DBG_PRINT_D("(1) list_cnt", list_cnt);
        PDG_DBG_PRINT_S("(2) io_type", p_io[io_type]);
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_IO_list(list_cnt, io_type);
# endif
     }

     PDG_DBG_PRINT_START    
     PDG_DBG_PRINT_C("fei_unformatted_read");
     PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
     fei_unformatted_read();
# endif

     processing_io_stmt = FALSE;
     processing_call = processing_call - 1;
     break;





   case Backspace_Opr:
# if defined(_FILE_IO_OPRS)
      processing_call = processing_call + 1;
      cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                      IR_FLD_L(ir_idx));

      cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                      IR_FLD_R(ir_idx));

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_backspace");
      PDG_DBG_PRINT_END

      fei_backspace();
      processing_call = processing_call - 1;
# endif
      break;




   case Close_Opr:
# if defined(_FILE_IO_OPRS)
      processing_call = processing_call + 1;
      cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                      IR_FLD_L(ir_idx));

      cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                      IR_FLD_R(ir_idx));

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_close");
      PDG_DBG_PRINT_END

      fei_close();
      processing_call = processing_call - 1;
# endif
      break;





   case Endfile_Opr:
# if defined(_FILE_IO_OPRS)
      processing_call = processing_call + 1;
      cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                      IR_FLD_L(ir_idx));

      cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                      IR_FLD_R(ir_idx));

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_endfile");
      PDG_DBG_PRINT_END

      fei_endfile();
      processing_call = processing_call - 1;
# endif
      break;





   case Inquire_Opr:
# if defined(_FILE_IO_OPRS)
      processing_call = processing_call + 1;
      cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                      IR_FLD_L(ir_idx));

      cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                      IR_FLD_R(ir_idx));

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_inquire");
      PDG_DBG_PRINT_END

      fei_inquire();
      processing_call = processing_call - 1;
# endif
      break;





   case Open_Opr:
# if defined(_FILE_IO_OPRS)
      processing_call = processing_call + 1;
      cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                      IR_FLD_L(ir_idx));

      cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                      IR_FLD_R(ir_idx));

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_open");
      PDG_DBG_PRINT_END

      fei_open();
      processing_call = processing_call - 1;
# endif
      break;




   case Rewind_Opr:
# if defined(_FILE_IO_OPRS)
      processing_call = processing_call + 1;
      cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                      IR_FLD_L(ir_idx));

      cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                      IR_FLD_R(ir_idx));

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_rewind");
      PDG_DBG_PRINT_END

      fei_rewind();
      processing_call = processing_call - 1;
# endif
      break;


   case Io_Item_Type_Code_Opr:
      make_io_type_code(IR_TYPE_IDX(ir_idx), loc_value);
      basic = get_basic_type(IO_TYPE_CODE_TYPE,
                             type_alignment_tbl[IO_TYPE_CODE_TYPE],
                             NULL_IDX);

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_arith_con");
      PDG_DBG_PRINT_T("(1) type", basic);
#if defined(_HOST32) && defined(_TARGET64)
      PDG_DBG_PRINT_VD("(2) value", loc_value[0]);
#else
      PDG_DBG_PRINT_LVD("(2) value", loc_value[0]);
#endif
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      pdg_cn_idx = fei_arith_con(basic, (long *)loc_value);
# endif
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_LD("(r) PDG_CN_IDX", pdg_cn_idx);
      PDG_DBG_PRINT_END

      cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                      IR_FLD_L(ir_idx));


      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_push_arith_con");
      PDG_DBG_PRINT_LD("(1) PDG_CN_IDX", pdg_cn_idx);
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      fei_push_arith_con(pdg_cn_idx);
# endif

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_iotype");
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      fei_iotype();
# endif
      break;



   case Write_Unformatted_Opr :
      io_type = WRITE_STMT;
      processing_call = processing_call + 1;
      processing_io_stmt = TRUE;
      cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                      IR_FLD_L(ir_idx));

      PDG_DBG_PRINT_START    
      PDG_DBG_PRINT_C("fei_control_list");
      PDG_DBG_PRINT_S("(1) io_type", p_io[io_type]);
      PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
      fei_control_list(io_type);
# endif


      cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                      IR_FLD_R(ir_idx));

      if (IR_IDX_R(ir_idx) != NULL_IDX) {
         list_cnt = IR_LIST_CNT_R(ir_idx);

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_IO_list");
         PDG_DBG_PRINT_D("(1) list_cnt", list_cnt);
         PDG_DBG_PRINT_S("(2) io_type", p_io[io_type]);
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_IO_list(list_cnt, io_type);
# endif
      }

      PDG_DBG_PRINT_START    
      PDG_DBG_PRINT_C("fei_unformatted_write");
      PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
      fei_unformatted_write();
# endif

      processing_io_stmt = FALSE;
      processing_call = processing_call - 1;
      break;




   case Read_Namelist_Opr :
        io_type = READ_NML_STMT;

# if !(defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
        /* remove the namelist group attr from the format item */
        list_idx1 = IR_IDX_L(ir_idx);
        for (i = 0; i < 7; i++) {
           list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
        }

        IL_FLD(list_idx1) = NO_Tbl_Idx;
        IL_IDX(list_idx1) = NULL_IDX;
# endif

        processing_call = processing_call + 1;
        processing_io_stmt = TRUE;
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_control_list");
        PDG_DBG_PRINT_S("(1) io_type", p_io[io_type]);
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_control_list(io_type);
# endif

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));

        if (IR_IDX_R(ir_idx) != NULL_IDX) {
           list_cnt = IR_LIST_CNT_R(ir_idx);

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_IO_list");
           PDG_DBG_PRINT_D("(1) list_cnt", list_cnt);
           PDG_DBG_PRINT_S("(2) io_type", p_io[io_type]);
           PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
           fei_IO_list(list_cnt, io_type);
# endif
        }

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_namelist_read");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_namelist_read();
# endif

        processing_io_stmt = FALSE;
        processing_call = processing_call - 1;
        break;






   case Write_Namelist_Opr :
        io_type = WRITE_NML_STMT;

# if !(defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
        /* remove the namelist group attr from the format item */
        list_idx1 = IR_IDX_L(ir_idx);
        for (i = 0; i < 7; i++) {
           list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
        }

        IL_FLD(list_idx1) = NO_Tbl_Idx;
        IL_IDX(list_idx1) = NULL_IDX;
# endif

        processing_call = processing_call + 1;
        processing_io_stmt = TRUE;
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_control_list");
        PDG_DBG_PRINT_S("(1) io_type", p_io[io_type]);
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_control_list(io_type);
# endif

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));

        if (IR_IDX_R(ir_idx) != NULL_IDX) {
           list_cnt = IR_LIST_CNT_R(ir_idx);

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_IO_list");
           PDG_DBG_PRINT_D("(1) list count", list_cnt);
           PDG_DBG_PRINT_S("(2) io_type", p_io[io_type]);
           PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
           fei_IO_list(list_cnt, io_type);
# endif
        }

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_namelist_write");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_namelist_write();
# endif

        processing_io_stmt = FALSE;
        processing_call = processing_call - 1;
        break;





   case Read_Formatted_Opr :
        io_type = READ_STMT;
        processing_call = processing_call + 1;
        processing_io_stmt = TRUE;
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_control_list");
        PDG_DBG_PRINT_S("(1) io_type", p_io[io_type]);
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_control_list(io_type);
# endif

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx));

        if (IR_IDX_R(ir_idx) != NULL_IDX) {
           list_cnt = IR_LIST_CNT_R(ir_idx);

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_IO_list");
           PDG_DBG_PRINT_D("(1) list_cnt", list_cnt);
           PDG_DBG_PRINT_S("(2) io_type", p_io[io_type]);
           PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
           fei_IO_list(list_cnt, io_type);
# endif
        }

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_formatted_read");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_formatted_read();
# endif

        processing_io_stmt = FALSE;
        processing_call = processing_call - 1;
        break;






   case Write_Formatted_Opr :
	io_type = WRITE_STMT;
        processing_call = processing_call + 1;
        processing_io_stmt = TRUE;
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_control_list");
        PDG_DBG_PRINT_S("(1) io_type", p_io[io_type]);
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_control_list(io_type);
# endif

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx));

        if (IR_IDX_R(ir_idx) != NULL_IDX) {
           list_cnt = IR_LIST_CNT_R(ir_idx);

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_IO_list");
           PDG_DBG_PRINT_D("(1) list_cnt", list_cnt);
           PDG_DBG_PRINT_S("(2) io_type", p_io[io_type]);
           PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
           fei_IO_list(list_cnt, io_type);
# endif
        }

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_formatted_write");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_formatted_write();
# endif

        processing_io_stmt = FALSE;
        processing_call = processing_call - 1;
        break;





   case Inquire_Iolength_Opr:
        io_type = WRITE_STMT;
        processing_call = processing_call + 1;
        processing_io_stmt = TRUE;
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));

        if (IR_IDX_R(ir_idx) != NULL_IDX) {
           list_cnt = IR_LIST_CNT_R(ir_idx);

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_IO_list");
           PDG_DBG_PRINT_D("(1) list_cnt", list_cnt);
           PDG_DBG_PRINT_S("(2) io_type", p_io[io_type]);
           PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
           fei_IO_list(list_cnt, io_type);
# endif
        }

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_iolength");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_iolength();
# endif

        processing_io_stmt = FALSE;
        processing_call = processing_call - 1;
        break;












   case Fcd_Opr :
   case Int_Mult_Upper_Opr :
   case Ieee_Unordered_Opr :
   case Ieee_Remainder_Opr :
   case Ieee_Real_Opr :
   case Ieee_Int_Opr :
   case Ieee_Copy_Sign_Opr :
   case Ieee_Exponent_Opr :
   case Ieee_Next_After_Opr :
   case Ieee_Binary_Scale_Opr :
   case Mod_Opr : 
   case Eq_Opr : 
   case Ne_Opr :
   case Lg_Opr :
   case Lt_Opr :    
   case Llt_Opr :
   case Le_Opr :
   case Lle_Opr :
   case Gt_Opr :  
   case Lgt_Opr :  
   case Ge_Opr :
   case Lge_Opr :
   case And_Opr :
   case Band_Opr :
   case Or_Opr :
   case Bor_Opr :
   case Eqv_Opr :       
   case Neqv_Opr :       
   case Bneqv_Opr :
   case Beqv_Opr :
   case Plus_Opr :
   case Minus_Opr :
   case Div_Opr :
   case Mult_Opr :
   case Dprod_Opr :
   case Power_Opr :
   case Shifta_Opr :
   case Shiftr_Opr :
   case Shiftl_Opr :
   case Shift_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
           cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                           IR_FLD_R(ir_idx));
        }

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        switch (IR_OPR(ir_idx)) { 
        case Fcd_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_fcd");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_fcd(basic);
# endif
             break;

        case Int_Mult_Upper_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_multiply_high");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_multiply_high(basic);
# endif
             break;

        case Ieee_Unordered_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_isunordered");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_isunordered(basic);
# endif
             break;

        case Ieee_Remainder_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_remainder");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_remainder(basic);
# endif
             break;

        case Ieee_Real_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_ieee_round");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_ieee_round(basic);
# endif
             break;

        case Ieee_Int_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_ieee_trunc");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_ieee_trunc(basic);
# endif
             break;

        case Ieee_Copy_Sign_Opr :
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_ieee_sign_xfer");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_ieee_sign_xfer(basic);
# endif
# else
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_sign_xfer");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_sign_xfer(basic);
# endif
# endif
             break;

        case Ieee_Exponent_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_logb");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_logb(basic);
# endif
             break;

        case Ieee_Next_After_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_nextafter");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_nextafter(basic);
# endif
             break;

        case Ieee_Binary_Scale_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_scalb");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_scalb(basic);
# endif
             break;

        case Mod_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_mod");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_mod(basic);
# endif
             break;

        case Eq_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_eq");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_eq(basic);
# endif
             break;

        case Ne_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_ne");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_ne(basic);
# endif
             break;

        case Lg_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_islg");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_islg(basic);
# endif
             break;

        case Lt_Opr :    
        case Llt_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_lt");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_lt(basic);
# endif
             break;

        case Le_Opr :    
        case Lle_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_le");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_le(basic);
# endif
             break;

        case Gt_Opr :  
        case Lgt_Opr :  
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_gt");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_gt(basic);
# endif
             break;

        case Ge_Opr :
        case Lge_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_ge");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_ge(basic);
# endif
             break;

        case And_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_land");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_land(basic);
# endif
             break;

        case Band_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_and");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_and(basic);
# endif
             break;

        case Or_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_lor");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_lor(basic);
# endif
             break;

        case Bor_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_or");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_or(basic);
# endif
             break;

        case Eqv_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_leqv");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_leqv(basic);
# endif
             break;

        case Neqv_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_lxor");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_lxor(basic);
# endif
             break;

        case Bneqv_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_xor");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_xor(basic);
# endif
             break;

        case Beqv_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_eqv");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_eqv(basic);
# endif
             break;

        case Plus_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_plus");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_plus(basic);
# endif
             break;

        case Minus_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_minus");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_minus(basic);
# endif
             break;

        case Div_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_div");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_div(basic);
# endif
             break;

        case Mult_Opr :
        case Dprod_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_mult");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_mult(basic);
# endif
             break;

        case Power_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_exponentiate");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_exponentiate(basic);
# endif
             break;

        case Shifta_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_ashift");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_ashift(basic);
# endif
             break;

        case Shiftr_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_rshift");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_rshift(basic);
# endif
             break;

        case Shiftl_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_lshift");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_lshift(basic);
# endif
             break;

        case Shift_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_new_binop_cshift");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_new_binop_cshift(basic);
# endif
             break;

        }
        break;












   case Entry_Opr :
        if (ATP_ALT_ENTRY(IR_IDX_L(ir_idx))) {
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_entry_pt");
           PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(IR_IDX_L(ir_idx)));
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           fei_entry_pt(PDG_AT_IDX(IR_IDX_L(ir_idx)));
# endif
        }
        break;


   case Label_Opr :
        attr_idx = IR_IDX_L(ir_idx);

        if (ATL_CLASS(attr_idx) == Lbl_Format ||
            (ATL_CLASS(attr_idx) <= Lbl_User && !ATL_EXECUTABLE(attr_idx))) {
           /* Intentionally blank */
        }
        else {
           send_label_def(ir_idx);
        }
        break;



   case Whole_Substring_Opr :
   case Substring_Opr :
        whole_substring = IR_OPR(ir_idx) == Whole_Substring_Opr;
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));
        whole_substring = FALSE;

        bound_chk = cmd_line_flags.runtime_substring;

        if (IR_BOUNDS_DONE(ir_idx)) {
           bound_chk = FALSE;
        }

        list_idx1 = IR_IDX_R(ir_idx);
        list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
        list_idx3 = IL_NEXT_LIST_IDX(list_idx2);

        if (stack_data_object) {
           big_int = CN_INT_TO_C(IL_IDX(list_idx1));

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_static_substr");
#if defined(_HOST32) && defined(_TARGET64)
           PDG_DBG_PRINT_D("(1) constant", big_int);
#else
           PDG_DBG_PRINT_VD("(1) constant", big_int);
#endif
           PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
           fei_static_substr(big_int);
# endif

           data_character_bit_length = list_idx3; 
           break;
        }

        cvrt_exp_to_pdg(IL_IDX(list_idx1),
                        IL_FLD(list_idx1));

        /* The third value is the length. */
        cvrt_exp_to_pdg(IL_IDX(list_idx3),
                        IL_FLD(list_idx3));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_substr");
        PDG_DBG_PRINT_D("(1) bound_chk", bound_chk);
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_substr(bound_chk);
# endif
        break;





   case Return_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx));

        if (IR_IDX_L(ir_idx) != NULL_IDX) {
           return_kind = Alternate_Return;
        }
        else if (IR_IDX_R(ir_idx) != NULL_IDX) {
           return_kind = Normal_Return;
        }
        else {
           return_kind = Void_Return;
        }

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_return");
        PDG_DBG_PRINT_S("(1) return_kind", p_return[return_kind]);
        PDG_DBG_PRINT_T("(2) type", pdg_type_void);
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_return(return_kind, pdg_type_void);
# endif
        break;




   case Stop_Opr:
# ifdef _STOP_IS_OPR
        /* stop code is in list item on right */
        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_stop");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_stop();
# endif

# else
        PRINTMSG(IR_LINE_NUM(ir_idx), 626, Internal, 
                 IR_COL_NUM(ir_idx),
                 "no Stop_Opr", "cvrt_exp_to_pdg");
# endif
        break;






   case Matmul_Opr :
   case Spread_Opr :
   case Reshape_Opr :
   case Transpose_Opr :
   case Sum_Opr :
   case Eoshift_Opr :
   case Maxval_Opr :
   case Minval_Opr :
   case Maxloc_Opr :
   case Minloc_Opr :
   case All_Opr :
   case Any_Opr :
   case Count_Opr :
   case Product_Opr :
   case Unpack_Opr :
   case Pack_Opr :
   case Cshift_Opr :
   case Dot_Product_Opr :
   case Dot_Product_Logical_Opr :
        processing_call = processing_call + 1;

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        switch (IR_OPR(ir_idx)) {
        case Matmul_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_matmul");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_matmul(basic);
# endif
             break;

        case Spread_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_spread");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_spread(basic);
# endif
             break;

        case Reshape_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_reshape");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_reshape(basic);
# endif
             break;

        case Transpose_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_transpose");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_transpose(basic);
# endif
             break;

        case Sum_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_sum");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_sum(basic);
# endif
             break;

        case Eoshift_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_eoshift");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_eoshift(basic);
# endif
             break;

        case Maxval_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_maxval");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_maxval(basic);
# endif
             break;

        case Minval_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_minval");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_minval(basic);
# endif
             break;

        case Maxloc_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei__maxloc");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei__maxloc(basic);
# endif
             break;

        case Minloc_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei__minloc");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei__minloc(basic);
# endif
             break;

        case All_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_all");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_all(basic);
# endif
             break;

        case Any_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_any");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_any(basic);
# endif
             break;

        case Count_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_count");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_count(basic);
# endif
             break;

        case Product_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_product");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_product(basic);
# endif
             break;

        case Unpack_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_unpack");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_unpack(basic);
# endif
             break;

        case Pack_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_pack");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_pack(basic);
# endif
             break;

        case Cshift_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_cshift");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_cshift(basic);
# endif
             break;

        case Dot_Product_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_dot_product");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_dot_product(basic);
# endif
             break;

        case Dot_Product_Logical_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_dot_product_logical)");
             PDG_DBG_PRINT_END
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
             fei_dot_product_logical(basic);
# endif
# endif
             break;
        }

        processing_call = processing_call - 1;
        break;












   case Memory_Barrier_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_loc_cmr");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_loc_cmr();
# endif
        break;





   case Write_Memory_Barrier_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_wmb");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_wmb();
# endif
        break;






   case Remote_Write_Barrier_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_rem_cmr");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_rem_cmr();
# endif
        break;







   case Get_Ieee_Status_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_readsr");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_readsr();
# endif
        break;





   case Set_Ieee_Status_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_set_ieee_stat");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_set_ieee_stat();
# endif
        break;





   case Get_Ieee_Exceptions_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_get_all_estat");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_get_all_estat();
# endif
        break;





   case Set_Ieee_Exceptions_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_set_all_estat");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_set_all_estat();
# endif
        break;





   case Test_Ieee_Exception_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_test_estat");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_test_estat();
# endif
        break;





   case Clear_Ieee_Exception_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_set_estat");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_set_estat(); 
# endif
        break;





   case Set_Ieee_Exception_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_set_estat");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_set_estat();
# endif
        break;





   case Get_Ieee_Interrupts_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_get_interupt");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_get_interupt();
# endif
        break;




   case Set_Ieee_Interrupts_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_set_interupt");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_set_interupt();
# endif
        break;





   case Test_Ieee_Interrupt_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_test_interupt");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_test_interupt();
# endif
        break;








   case Enable_Ieee_Interrupt_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_enbl_interupt");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_enbl_interupt();
# endif
        break;







   case Disable_Ieee_Interrupt_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_dsbl_interupt");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_dsbl_interupt();
# endif
        break;





   case Get_Ieee_Rounding_Mode_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_get_rmode");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_get_rmode();
# endif
        break;




   case Set_Ieee_Rounding_Mode_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_set_rmode");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_set_rmode();
# endif
        break;






















   case Alloc_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_alloc");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_alloc();
# endif
        break;




   case Malloc_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_malloc");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
        fei_malloc();
# endif
# endif
        break;





   case SSD_Alloc_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_ssd_alloc");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_ssd_alloc();
# endif
        break;




   case Symmetric_Alloc_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_mpp_symmetric_alloc");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_mpp_symmetric_alloc();
# endif
        break;





   case Dealloc_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_free");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_free();
# endif
        break;




   case Free_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_mfree");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
        fei_mfree();
# endif
# endif
        break;






   case SSD_Dealloc_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_ssd_free");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_ssd_free();
# endif
        break;



   case Symmetric_Dealloc_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_mpp_symmetric_free");
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_mpp_symmetric_free();
# endif
        break;




   case Copyin_Bound_Opr :
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
        send_attr_ntry(IR_IDX_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_copyin_bound");
        PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(IR_IDX_L(ir_idx)));
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_copyin_bound(PDG_AT_IDX(IR_IDX_L(ir_idx)));
# endif
# endif
        break;



   case Copy_In_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_copyin");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_copyin();
# endif
        break;






   case Copy_Out_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_copyout");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_copyout(); 
# endif
        break;













   case Struct_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));
       
        data_attr = NULL_IDX;
        member = TRUE;
        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));
        member = FALSE;

        if (stack_data_object) {
           break;
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_field_dot");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_field_dot(null_type);
# endif

        break;






   case Triplet_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_subscr_triplet");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_subscr_triplet(pdg_type_void);
# endif
        break;




   case Max_Opr :
   case Min_Opr :
        tmp_ir_idx = IR_IDX_L(ir_idx);
        count = 0;
        while (tmp_ir_idx != NULL_IDX) {
           if (IL_IDX(tmp_ir_idx) != NULL_IDX) {
              cvrt_exp_to_pdg(IL_IDX(tmp_ir_idx),
                              IL_FLD(tmp_ir_idx));

              count = count + 1;
           }
           tmp_ir_idx = IL_NEXT_LIST_IDX(tmp_ir_idx);
        } 

        basic = get_basic_type(IR_TYPE_IDX(ir_idx),0, NULL_IDX);

        if (IR_OPR(ir_idx) == Min_Opr) {
           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_min");
           PDG_DBG_PRINT_D("(1) count", count);
           PDG_DBG_PRINT_T("(2) type", basic);
           PDG_DBG_PRINT_END    
# ifdef _ENABLE_FEI
           fei_min(count, basic);
# endif
        }
        else {
           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_max");
           PDG_DBG_PRINT_D("(1) count", count);
           PDG_DBG_PRINT_T("(2) type", basic);
           PDG_DBG_PRINT_END    
# ifdef _ENABLE_FEI
           fei_max(count, basic);
# endif
        }
        break;







   case Implied_Do_Opr :
        if (static_initialization) {
           /* skip over DO control variable before starting loop */
           search_idx = IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx));

           trip = 1;
           while (trip != 4) {
              if (trip == 1) {  /* start */
                 COPY_OPND(l_opnd, IL_OPND(search_idx));
                 vv = 1;
                 cn_idx = get_next_array_expr_element(&l_opnd, &vv);  
                 COPY_OPND(IL_OPND(search_idx), l_opnd);
                 start = CN_INT_TO_C(cn_idx);
              }

              if (trip == 2) {  /* end */
                 COPY_OPND(l_opnd, IL_OPND(search_idx));
                 vv = 1;
                 cn_idx = get_next_array_expr_element(&l_opnd, &vv);  
                 COPY_OPND(IL_OPND(search_idx), l_opnd);
                 end = CN_INT_TO_C(cn_idx);
              }

              if (trip == 3) {  /* inc */
                 COPY_OPND(l_opnd, IL_OPND(search_idx));
                 vv = 1;
                 cn_idx = get_next_array_expr_element(&l_opnd, &vv);  
                 COPY_OPND(IL_OPND(search_idx), l_opnd);
                 inc = CN_INT_TO_C(cn_idx);
              }

              trip = trip + 1;
              search_idx = IL_NEXT_LIST_IDX(search_idx);
           }

           /* Save the guts of the LCV attr.   Store them in a constant entry */
           /* pointed to by ATD_TMP_IDX.                                      */

           GET_LCV_CONST(IL_IDX(IR_IDX_R(ir_idx)), 
                 loc_value[0],      /* target constant */
                 num_host_wds[TYP_LINEAR(
                       ATD_TYPE_IDX(IL_IDX(IR_IDX_R(ir_idx))))]);

           ATD_FLD(IL_IDX(IR_IDX_R(ir_idx))) = CN_Tbl_Idx;
           ATD_TMP_IDX(IL_IDX(IR_IDX_R(ir_idx))) =
                 ntr_const_tbl(ATD_TYPE_IDX(IL_IDX(IR_IDX_R(ir_idx))),
                               FALSE,
                               loc_value);

           if (inc > 0) { /* run the loop forward */
              for (i = start; i <= end; i = i + inc) {

                  if (search_idx == NULL_IDX) {   
                     cn_idx = C_INT_TO_CN(NULL_IDX, i);  /* Macro sets type */
                  }
                  else {
                     COPY_OPND(l_opnd, IL_OPND(search_idx));
                     vv = i;
                     cn_idx = get_next_array_expr_element(&l_opnd, &vv);  
                     COPY_OPND(IL_OPND(search_idx), l_opnd);
                  }
#ifdef KEY
                  SET_LCV_CONST(IL_IDX(IR_IDX_R(ir_idx)),
                                CN_CONST(cn_idx), 
                                num_host_wds[TYP_LINEAR(
                                     ATD_TYPE_IDX(IL_IDX(IR_IDX_R(ir_idx))))], 
				num_host_wds[TYP_LINEAR(
				     CN_TYPE_IDX(cn_idx))]);
#else
                  SET_LCV_CONST(IL_IDX(IR_IDX_R(ir_idx)),
                                CN_CONST(cn_idx), 
                                num_host_wds[TYP_LINEAR(
                                     ATD_TYPE_IDX(IL_IDX(IR_IDX_R(ir_idx))))]);
#endif

                  t_idx = IR_IDX_L(ir_idx);
                  while (t_idx != NULL_IDX) {

                     if ((IL_FLD(t_idx) == IR_Tbl_Idx) &&
                         (IR_OPR(IL_IDX(t_idx)) == Implied_Do_Opr)) {
                        cvrt_exp_to_pdg(IL_IDX(t_idx),
                                        IL_FLD(t_idx));
                     }
                     else {
                        blank_pad_text = IL_IDX(t_idx);
                        push_data_value(t_idx);
                     }

                     t_idx = IL_NEXT_LIST_IDX(t_idx);
                  }
              }
           }
           else {  /* increment is <= 0 */
              for (i = start; i >= end; i = i + inc) {

                  if (search_idx == NULL_IDX) {
                     cn_idx = C_INT_TO_CN(NULL_IDX, i);  /* Macro sets type */
                  }
                  else {
                     COPY_OPND(l_opnd, IL_OPND(search_idx));
                     vv = i;
                     cn_idx = get_next_array_expr_element(&l_opnd, &vv);
                     COPY_OPND(IL_OPND(search_idx), l_opnd);
                  }
#ifdef KEY
                  SET_LCV_CONST(IL_IDX(IR_IDX_R(ir_idx)),
                                CN_CONST(cn_idx),
                                num_host_wds[TYP_LINEAR(
                                   ATD_TYPE_IDX(IL_IDX(IR_IDX_R(ir_idx))))],
				num_host_wds[TYP_LINEAR(
				     CN_TYPE_IDX(cn_idx))]);
#else
                  SET_LCV_CONST(IL_IDX(IR_IDX_R(ir_idx)),
                                CN_CONST(cn_idx),
                                num_host_wds[TYP_LINEAR(
                                   ATD_TYPE_IDX(IL_IDX(IR_IDX_R(ir_idx))))]);
#endif

                  t_idx = IR_IDX_L(ir_idx);
                  while (t_idx != NULL_IDX) {

                     if ((IL_FLD(t_idx) == IR_Tbl_Idx) &&
                        (IR_OPR(IL_IDX(t_idx)) == Implied_Do_Opr)) {
                        cvrt_exp_to_pdg(IL_IDX(t_idx),
                                        IL_FLD(t_idx));
                     }
                     else {
                        blank_pad_text = IL_IDX(t_idx);
                        push_data_value(t_idx);
                     }

                     t_idx = IL_NEXT_LIST_IDX(t_idx);
                  }
              }
           }

           /* Restore the guts of the LCV temp attr. */
#ifdef KEY
           SET_LCV_CONST(IL_IDX(IR_IDX_R(ir_idx)),
                         CN_CONST(ATD_TMP_IDX(IL_IDX(IR_IDX_R(ir_idx)))),
                         num_host_wds[TYP_LINEAR(
                              ATD_TYPE_IDX(IL_IDX(IR_IDX_R(ir_idx))))],
			 num_host_wds[TYP_LINEAR(
			      CN_TYPE_IDX(ATD_TMP_IDX(IL_IDX(IR_IDX_R(ir_idx)))))]);
#else
           SET_LCV_CONST(IL_IDX(IR_IDX_R(ir_idx)),
                         CN_CONST(ATD_TMP_IDX(IL_IDX(IR_IDX_R(ir_idx)))),
                         num_host_wds[TYP_LINEAR(
                              ATD_TYPE_IDX(IL_IDX(IR_IDX_R(ir_idx))))]);
#endif

        }
        else {
           search_idx = IR_IDX_R(ir_idx);
           while (search_idx != NULL_IDX) {
              cvrt_exp_to_pdg(IL_IDX(search_idx),
                              IL_FLD(search_idx));

              search_idx = IL_NEXT_LIST_IDX(search_idx);
           }

           cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                           IR_FLD_L(ir_idx));

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_IO_list");
           PDG_DBG_PRINT_D("(1) list count", IR_LIST_CNT_L(ir_idx));
           PDG_DBG_PRINT_S("(2) io_type", p_io[io_type]);
           PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
           fei_IO_list(IR_LIST_CNT_L(ir_idx), io_type);
# endif

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_implied_do");
           PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
           fei_implied_do();
# endif

        }
        break; 




  
   case Init_Opr :
        if ((IR_FLD_L(ir_idx) == IR_Tbl_Idx) &&
            (IR_OPR(IR_IDX_L(ir_idx)) == Implied_Do_Opr)) {

           static_initialization = TRUE;

           data_value_idx = IR_IDX_R(ir_idx);
           while (data_value_idx != NULL_IDX) {
              if (IL_FLD(data_value_idx) == IR_Tbl_Idx) {
                 IL_LIST_CNT(data_value_idx) = 
                 CN_INT_TO_C(IR_IDX_L(IL_IDX(data_value_idx)));
              }
              else {
                 IL_LIST_CNT(data_value_idx) = 1; 
              }
              data_value_idx = IL_NEXT_LIST_IDX(data_value_idx);
           }
           data_value_idx = IR_IDX_R(ir_idx);

           cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                           IR_FLD_L(ir_idx));

           static_initialization = FALSE;
        }
        else {
           data_attr = NULL_IDX;
           stack_data_object = TRUE;
           cvrt_exp_to_pdg(IR_IDX_L(ir_idx),    /* stack the object */
                           IR_FLD_L(ir_idx));
           stack_data_object = FALSE;

           list_idx1 = IR_IDX_R(ir_idx);
           stack_data_constant = TRUE;
           cvrt_exp_to_pdg(IL_IDX(list_idx1),   /* stack the constant */
                           IL_FLD(list_idx1));
           stack_data_constant = FALSE;

           list_idx2 = IL_NEXT_LIST_IDX(list_idx1);
           list_idx3 = IL_NEXT_LIST_IDX(list_idx2);
          
           if (TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx1))) == Typeless ||
               (TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx1))) == Character &&
                TYP_TYPE(ATD_TYPE_IDX(data_attr)) != Character)) { 
              ignore_types = TRUE;
           }
           else {
              ignore_types = FALSE;
           }
           rep_count = CN_INT_TO_C(IL_IDX(list_idx2));
           stride = CN_INT_TO_C(IL_IDX(list_idx3));

# if defined(_TARGET_OS_MAX) || defined(_HOST32)

           /* JEFFL - Is this necessary for HOST32 ? */
           if (TYP_LINEAR(CN_TYPE_IDX(IL_IDX(list_idx3))) != Integer_8) {
              SIGN_EXTEND(stride);
           }
# endif

           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_static_simple_init");
           PDG_DBG_PRINT_LLD("(1) rep_count", rep_count);
           PDG_DBG_PRINT_LLD("(2) stride", stride);
           PDG_DBG_PRINT_D("(3) ignore_types", ignore_types);
           PDG_DBG_PRINT_D("(4) unused", 0);
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           fei_static_simple_init(rep_count,
                                  stride,
                                  ignore_types,
                                  0);
# endif
        }

        break;



   case Init_Reloc_Opr :
        data_attr = NULL_IDX;
        stack_data_object = TRUE;
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),    /* stack the object */
                        IR_FLD_L(ir_idx));
        stack_data_object = FALSE;

        offset = 0;

# ifdef _INIT_RELOC_BASE_OFFSET

        offset2.idx = IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)));
        offset2.fld = IL_FLD(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)));

        COPY_OPND(opnd, IL_OPND(IR_IDX_R(ir_idx)));
        attr_idx = find_left_attr(&opnd);
        send_attr_ntry(attr_idx);
        offset1.idx = ATD_OFFSET_IDX(attr_idx);
        offset1.fld = ATD_OFFSET_FLD(attr_idx);
        result.idx = ATD_OFFSET_IDX(
                              SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx)));
        result.fld = ATD_OFFSET_FLD(
                              SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx)));

        size_offset_binary_calc(&offset1, &result, Minus_Opr, &result);
        size_offset_binary_calc(&offset2,  &result, Plus_Opr,  &result);

        /*  Someday this may not be a constant.            (KAYKAY)   */

        if (result.fld == CN_Tbl_Idx) {
           offset = CN_INT_TO_C(CN_CONST(result.idx));

# if defined(_TARGET_OS_MAX) || defined(_HOST32)  /* JEFFL - BRIANJ */
           if (TYP_LINEAR(CN_TYPE_IDX(result.idx)) != Integer_8) {
              SIGN_EXTEND(offset);
           }
# endif
        }
        else {
           offset = F_INT_TO_C(result.constant, TYP_LINEAR(result.type_idx));
        }

        attr_idx = SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx));
        send_attr_ntry(attr_idx);

        OPND_FLD(opnd) = AT_Tbl_Idx;
        OPND_IDX(opnd) = attr_idx;
        OPND_LINE_NUM(opnd) = IR_LINE_NUM(ir_idx);
        OPND_COL_NUM(opnd) = IR_COL_NUM(ir_idx);

        if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
           unused = gen_whole_substring(&opnd, 0);
        }

        /* assumes there is a loc or aloc on top */
        COPY_OPND(IR_OPND_L(IL_IDX(IR_IDX_R(ir_idx))), opnd);
# endif

        cvrt_exp_to_pdg(IL_IDX(IR_IDX_R(ir_idx)),
                        IL_FLD(IR_IDX_R(ir_idx)));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_static_simple_reloc_init");
        PDG_DBG_PRINT_LLD("(1) offset", offset);
        PDG_DBG_PRINT_LLD("(2) rep count", (long64) 1);
        PDG_DBG_PRINT_D("(3) stride", TARGET_BITS_PER_WORD);
        PDG_DBG_PRINT_LLD("(4) bit size", (long64) 0);
        PDG_DBG_PRINT_D("(5) ignore_types", 1);
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_static_simple_reloc_init(offset,
                                     (long64) 1,
                                     (long64) TARGET_BITS_PER_WORD,
                                     (long64) 0,
                                     1);
# endif
      break;



   case Whole_Subscript_Opr :
   case Section_Subscript_Opr:
   case Subscript_Opr :
        whole_subscript = IR_WHOLE_ARRAY(ir_idx);
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));
        whole_subscript = FALSE;


        if (IR_OPR(ir_idx) == Whole_Subscript_Opr &&
            !processing_io_stmt &&
            IR_CONTIG_ARRAY(ir_idx) != 0) {

           /* this is a contiguous array. */
           /* call fei_as_ref and ignore subscripts */

           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_as_ref");
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           fei_as_ref(null_type);
# endif
           break;
        }
     
        attr_idx = find_base_attr(&IR_OPND_L(ir_idx), &line, &col);
        bound_idx = ATD_ARRAY_IDX(attr_idx);
# if defined(_F_MINUS_MINUS) && defined(_TARGET_OS_MAX)
        pe_bd_idx = ATD_PE_ARRAY_IDX(attr_idx);
# endif
        base_attr = find_left_attr(&(IR_OPND_L(ir_idx)));

        bound_chk = (cdir_switches.bounds ||
                     ATD_BOUNDS_CHECK(base_attr)) &&
                    !ATD_NOBOUNDS_CHECK(base_attr);

        bound_chk &= ! (IR_WHOLE_ARRAY(ir_idx));

        if (IR_BOUNDS_DONE(ir_idx)) {
           bound_chk = FALSE;
        }
                        
        next_idx = IR_IDX_R(ir_idx);
        dim = IR_LIST_CNT_R(ir_idx);
        while (dim != 1) {
           next_idx = IL_NEXT_LIST_IDX(next_idx);
           dim = dim - 1;
        }

        dim = IR_LIST_CNT_R(ir_idx);
        ss = 0;
        while (dim > 0) {
              if (stack_data_object) { 
                 COPY_OPND(l_opnd, IL_OPND(next_idx));
                 /* l_opnd will always be a scalar expression here */
                 vv = 1;
                 cn_idx = get_next_array_expr_element(&l_opnd, &vv);  
                 COPY_OPND(IL_OPND(next_idx), l_opnd);
                 static_subscripts[ss] = CN_INT_TO_C(cn_idx);

# if defined(_TARGET_OS_MAX) || defined(_HOST32)  /* JEFFL */
                 if (TYP_LINEAR(CN_TYPE_IDX(cn_idx)) != Integer_8) {
                    SIGN_EXTEND(static_subscripts[ss]);
                 }
# endif
                 goto CONTINUE;
              }

              cvrt_exp_to_pdg(IL_IDX(next_idx),
                              IL_FLD(next_idx));

              if (ATD_IM_A_DOPE(attr_idx)) {
                 cvrt_exp_to_pdg(IR_IDX_L(IR_IDX_L(ir_idx)),
                                 IR_FLD_L(IR_IDX_L(ir_idx)));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_get_dv_low_bnd");
                 PDG_DBG_PRINT_D("(1) dim", dim);
                 PDG_DBG_PRINT_D("(2) expand immed", EXPAND);
                 PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
                 fei_get_dv_low_bnd(dim, EXPAND);
# endif
              }
# if defined(_F_MINUS_MINUS) && defined(_TARGET_OS_MAX)
              else if (IL_PE_SUBSCRIPT(next_idx)) {
                 cvrt_exp_to_pdg(CN_INTEGER_ONE_IDX,
                                 CN_Tbl_Idx);
              }
# endif
              else {
                 cvrt_exp_to_pdg(BD_LB_IDX(bound_idx, dim),
                                 BD_LB_FLD(bound_idx, dim));
              }

              if (ATD_IM_A_DOPE(attr_idx)) {
                 cvrt_exp_to_pdg(IR_IDX_L(IR_IDX_L(ir_idx)),
                                 IR_FLD_L(IR_IDX_L(ir_idx)));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_get_dv_extent");
                 PDG_DBG_PRINT_D("(1) dim", dim);
                 PDG_DBG_PRINT_D("(2) expand immed", EXPAND);
                 PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
                 fei_get_dv_extent(dim, EXPAND);
# endif
              }
# if defined(_F_MINUS_MINUS) && defined(_TARGET_OS_MAX)
              else if (IL_PE_SUBSCRIPT(next_idx)) {
                 cvrt_exp_to_pdg(BD_LEN_IDX(pe_bd_idx),
                                 BD_LEN_FLD(pe_bd_idx));
              }
# endif
              else {
                 cvrt_exp_to_pdg(BD_XT_IDX(bound_idx, dim),
                                 BD_XT_FLD(bound_idx, dim));
              }

              if (ATD_IM_A_DOPE(attr_idx)) {
                 cvrt_exp_to_pdg(IR_IDX_L(IR_IDX_L(ir_idx)),
                                 IR_FLD_L(IR_IDX_L(ir_idx)));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_get_dv_str_mult");
                 PDG_DBG_PRINT_D("(1) dim", dim);
                 PDG_DBG_PRINT_D("(2) expand immed", EXPAND);
                 PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
                 fei_get_dv_str_mult(dim, EXPAND);
# endif
              }
# if defined(_EXTENDED_CRI_CHAR_POINTER)
              else if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
                       ATD_CLASS(attr_idx) == CRI_Pointee &&
                       TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character &&
                       TYP_CHAR_CLASS(ATD_TYPE_IDX(attr_idx)) == 
                                                           Assumed_Size_Char) {
                 cvrt_exp_to_pdg(BD_SM_IDX(bound_idx, dim),
                                 BD_SM_FLD(bound_idx, dim));

                 cvrt_exp_to_pdg(attr_idx, AT_Tbl_Idx);

                 basic = get_basic_type(ATD_TYPE_IDX(attr_idx), 
                                        ATD_ALIGNMENT(attr_idx),
                                        attr_idx);

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_len");
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 fei_len(basic);
# endif

                 basic = get_basic_type(INTEGER_DEFAULT_TYPE, 
                                       type_alignment_tbl[INTEGER_DEFAULT_TYPE],
                                       NULL_IDX);

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_mult");
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 fei_mult(basic);
# endif
              }
# endif
# if defined(_F_MINUS_MINUS) && defined(_TARGET_OS_MAX)
              else if (IL_PE_SUBSCRIPT(next_idx)) {
                 cvrt_exp_to_pdg(CN_INTEGER_ONE_IDX,
                                 CN_Tbl_Idx);
              }
# endif
              else {
                 cvrt_exp_to_pdg(BD_SM_IDX(bound_idx, dim),
                                 BD_SM_FLD(bound_idx, dim));
              }

              if (BD_ARRAY_CLASS(bound_idx) == Assumed_Size) {
                 if (BD_RANK(bound_idx) == IR_LIST_CNT_R(ir_idx) && ss == 0) {
                    bound_chk = FALSE;
                 } 
              } 

              PDG_DBG_PRINT_START
              PDG_DBG_PRINT_C("fei_subscr_size");
              PDG_DBG_PRINT_T("(1) type", pdg_type_void);
              PDG_DBG_PRINT_D("(2) bound_chk", bound_chk);
              PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
              fei_subscr_size(pdg_type_void, bound_chk);
# endif

              if (ATD_IM_A_DOPE(attr_idx)) {
                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_nseq_subscr");
                 PDG_DBG_PRINT_T("(1) type", null_type);
                 PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
                 fei_nseq_subscr(null_type);
# endif
              }
              else {
                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_seq_subscr");
                 PDG_DBG_PRINT_T("(1) type", null_type);
                 PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
                 fei_seq_subscr(null_type);
# endif
              }

CONTINUE:
              next_idx = IL_PREV_LIST_IDX(next_idx);
              dim = dim - 1;
              ss = ss + 1;
        }
        
        if (stack_data_object) {
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_static_subscripts");
           for (i = 0; i < ss; i++) {
              PDG_DBG_PRINT_LLD("static subscript", static_subscripts[i]);
           }
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           fei_static_subscripts(static_subscripts);
# endif
        }
        break;





   case Present_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_present");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_present();
# endif
        break;




   case Argchck_Present_Opr :
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_argchk");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_argchk();
# endif

        break;




   case Argchck_Loc_Opr :
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_argloc");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_argloc();
# endif

        break;





   case Aloc_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        global_attr_idx = NULL_IDX;
        global_attr_idx = find_left_attr(&(IR_OPND_L(ir_idx)));
        basic = get_basic_type(IR_TYPE_IDX(ir_idx), 0, NULL_IDX);
        global_attr_idx = NULL_IDX;

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_arg_addr");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_arg_addr(basic);
# endif
        break;






   case Dv_Deref_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_dv_deref");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_dv_deref(null_type);
# endif
        break;





   case Dv_Whole_Def_Opr :
        processing_call = processing_call + 1;

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_dv_def");
        PDG_DBG_PRINT_D("dim", IR_DV_DIM(ir_idx));
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
#ifdef KEY /* Bug 6845 */
        fei_dv_def(IR_DV_DIM(ir_idx), IR_DV_N_ALLOC_CPNT(ir_idx));
#else /* KEY Bug 6845 */
        fei_dv_def(IR_DV_DIM(ir_idx));
#endif /* KEY Bug 6845 */
# endif
        processing_call = processing_call - 1;
        break;








   case Dv_Access_Stride_Mult :
   case Dv_Access_Extent :
   case Dv_Access_Low_Bound :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        switch (IR_OPR(ir_idx)) {
        case Dv_Access_Stride_Mult :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_get_dv_str_mult");
             PDG_DBG_PRINT_D("(1) dim", IR_DV_DIM(ir_idx));
             PDG_DBG_PRINT_D("(2) expand", EXPAND);
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_get_dv_str_mult(IR_DV_DIM(ir_idx), EXPAND);
# endif
             break;

        case Dv_Access_Extent :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_get_dv_extent");
             PDG_DBG_PRINT_D("(1) dim", IR_DV_DIM(ir_idx));
             PDG_DBG_PRINT_D("(2) expand", EXPAND);
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_get_dv_extent(IR_DV_DIM(ir_idx), EXPAND);
# endif
             break;

        case Dv_Access_Low_Bound :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_get_dv_low_bnd");
             PDG_DBG_PRINT_D("(1) dim", IR_DV_DIM(ir_idx));
             PDG_DBG_PRINT_D("(2) expand", EXPAND);
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_get_dv_low_bnd(IR_DV_DIM(ir_idx), EXPAND);
# endif
             break;
        }
        break;








   case Dv_Set_Stride_Mult :
   case Dv_Set_Extent :
   case Dv_Set_Low_Bound :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));

        switch (IR_OPR(ir_idx)) {
        case Dv_Set_Stride_Mult :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_set_dv_str_mult");
             PDG_DBG_PRINT_D("(1) dim", IR_DV_DIM(ir_idx));
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_set_dv_str_mult(IR_DV_DIM(ir_idx)); 
# endif
             break;

        case Dv_Set_Extent :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_set_dv_extent");
             PDG_DBG_PRINT_D("(1) dim", IR_DV_DIM(ir_idx));
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_set_dv_extent(IR_DV_DIM(ir_idx)); 
# endif
             break;

        case Dv_Set_Low_Bound :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_set_dv_low_bnd");
             PDG_DBG_PRINT_D("(1) dim", IR_DV_DIM(ir_idx));
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
             fei_set_dv_low_bnd(IR_DV_DIM(ir_idx)); 
# endif
             break;
        }
        break;







   case Dv_Set_Base_Addr :
   case Dv_Set_El_Len :
   case Dv_Set_Assoc :
   case Dv_Set_Ptr_Alloc :
   case Dv_Set_P_Or_A :
   case Dv_Set_A_Contig :
   case Dv_Set_N_Dim :
   case Dv_Set_Typ_Code :
   case Dv_Set_Orig_Base :
   case Dv_Set_Orig_Size :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
           cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                           IR_FLD_R(ir_idx));
        }

#ifndef KEY /* Bug 6845 */
        switch (IR_OPR(ir_idx)) {
        case Dv_Set_Base_Addr :
             fld = 1;
             break;
        case Dv_Set_El_Len :
             fld = 2;
             break;
        case Dv_Set_Assoc :
             fld = 3;
             break;
        case Dv_Set_Ptr_Alloc :
             fld = 4;
             break;
        case Dv_Set_P_Or_A :
             fld = 5;
             break;
        case Dv_Set_A_Contig :
             fld = 6;
             break;
        case Dv_Set_N_Dim :
             fld = 7;
             break;
        case Dv_Set_Typ_Code :
             fld = 8;
             break;
        case Dv_Set_Orig_Base :
             fld = 9;
             break;
        case Dv_Set_Orig_Size :
             fld = 10;
             break;
        }
#endif /* KEY Bug 6845 */

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_set_dv_hdr_fld");
        PDG_DBG_PRINT_D("(1) field num", fld);
        PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
#   ifdef KEY /* Bug 6845 */
        fei_set_dv_hdr_fld(opr_to_dv_hdr_fld(ir_idx));
#   else /* KEY Bug 6845 */
        fei_set_dv_hdr_fld(fld);
#   endif /* KEY Bug 6845 */
# endif
        break;





   case Dv_Access_Base_Addr :
   case Dv_Access_El_Len :
   case Dv_Access_Assoc :
   case Dv_Access_Ptr_Alloc :
   case Dv_Access_P_Or_A :
   case Dv_Access_A_Contig :
   case Dv_Access_N_Dim :
   case Dv_Access_Typ_Code :
   case Dv_Access_Orig_Base :
   case Dv_Access_Orig_Size :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
           cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                           IR_FLD_R(ir_idx));
        }

#ifndef KEY /* Bug 6845 */
        switch (IR_OPR(ir_idx)) {
        case Dv_Access_Base_Addr :
             fld = 1;
             break;
        case Dv_Access_El_Len :
             fld = 2;
             break;
        case Dv_Access_Assoc :
             fld = 3;
             break;
        case Dv_Access_Ptr_Alloc :
             fld = 4;
             break;
        case Dv_Access_P_Or_A :
             fld = 5;
             break;
        case Dv_Access_A_Contig :
             fld = 6;
             break;
        case Dv_Access_N_Dim :
             fld = 7;
             break;
        case Dv_Access_Typ_Code :
             fld = 8;
             break;
        case Dv_Access_Orig_Base :
             fld = 9;
             break;
        case Dv_Access_Orig_Size :
             fld = 10;
             break;
        }
#endif /* KEY Bug 6845 */

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_get_dv_hdr_fld");
        PDG_DBG_PRINT_D("(1) field num", fld);
        PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
#   ifdef KEY /* Bug 6845 */
        fei_get_dv_hdr_fld(opr_to_dv_hdr_fld(ir_idx));
#   else /* KEY Bug 6845 */
        fei_get_dv_hdr_fld(fld);
#   endif /* KEY Bug 6845 */
# endif
        break;





   case Allocate_Opr :
   case Deallocate_Opr :
        /* do the right side first */
        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_allocate");
        PDG_DBG_PRINT_D("(1) list count", IR_LIST_CNT_L(ir_idx) + 3);
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_allocate(IR_LIST_CNT_L(ir_idx) + 3);
# endif
        break;




   case Call_Opr :
        processing_call = processing_call + 1;

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        if (IR_IDX_R(ir_idx) != NULL_IDX &&
            IR_LIST_CNT_R(ir_idx) > 0) {
#ifdef KEY /* Bug 4602 */
	   /* The %val() operator doesn't appear in the tree, except as the
	    * absence of an Aloc_Opr at the top of the subtree representing
	    * the formal argument. For a scalar, this works fine because
	    * cvrt_exp_to_pdg() computes the value if there's not an Aloc_Opr,
	    * and computes the address if there is. For a subscripted array,
	    * cvrt_exp_to_pdg normally computes a WHIRL ARRAY op, and relies
	    * on some operator or assignment node above it to emit a WHIRL
	    * LOAD op for the array element. We need that LOAD op when the
	    * ALoc_Opr is missing; but it's hard for cvrt_exp_to_pdg to
	    * know that the Subscript_Opr is a formal argument; and it's hard
	    * for fei_call to know which of its arguments were missing the
	    * Aloc_Opr in the tree; so we create an ugly special case here.
	    */
	   if (IR_FLD_R(ir_idx) == IL_Tbl_Idx) {
	     for (int arg_il_idx = IR_IDX_R(ir_idx);
	       arg_il_idx != NULL_IDX;
	       arg_il_idx = IL_NEXT_LIST_IDX(arg_il_idx)) {
	       int arg_idx = IL_IDX(arg_il_idx);
	       int arg_fld = IL_FLD(arg_il_idx);
	       cvrt_exp_to_pdg(arg_idx, arg_fld);
	       if (arg_fld == IR_Tbl_Idx && IR_OPR(arg_idx) == Subscript_Opr) {
		 fei_array_element_by_value();
	         }
	       }
	     }
	   else
#endif /* KEY Bug 4602 */
           cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                           IR_FLD_R(ir_idx));
        }

        if (IR_IDX_R(ir_idx) == NULL_IDX) {
           number_actual_args = 0;
        }
        else {
           number_actual_args = IR_LIST_CNT_R(ir_idx);
        }

        if (ATP_PGM_UNIT(IR_IDX_L(ir_idx)) == Subroutine ||
#ifdef KEY /* Bug 5089 */
            special_case_fcn_to_sub(IR_IDX_L(ir_idx)) ||
#endif /* KEY Bug 5089 */
            ATP_EXTRA_DARG(IR_IDX_L(ir_idx))) {
           type_desc = pdg_type_void;

           if (ATP_HAS_ALT_RETURN(IR_IDX_L(ir_idx))) {
              type_desc = get_basic_type(CG_INTEGER_DEFAULT_TYPE,
                                    type_alignment_tbl[CG_INTEGER_DEFAULT_TYPE],
                                    NULL_IDX);
           }
        }
        else {
           type_desc = get_type_desc(IR_IDX_L(ir_idx));
        }

        flags = 0;
        flags |= ATP_DOES_NOT_RETURN(IR_IDX_L(ir_idx)) 
                                           << FEI_CALL_DOES_NOT_RETURN;

        /* Always pass FALSE for the alternate return argument.  PDGCS does */
        /* not use this for Fortran 90.                                     */

        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("fei_call");
        PDG_DBG_PRINT_S("AT_OBJ_NAME", AT_OBJ_NAME_PTR(IR_IDX_L(ir_idx)));
        PDG_DBG_PRINT_D("(1) num actual args", number_actual_args);
        PDG_DBG_PRINT_T("(2) type", type_desc);
        PDG_DBG_PRINT_S("(3) call type", "By_Value");
        PDG_DBG_PRINT_S("(4) alt return", p_boolean[FALSE]);
        PDG_DBG_PRINT_D("(5) IR_INLINE_STATE", IR_INLINE_STATE(ir_idx));
        PDG_DBG_PRINT_LLO("(6) flags", flags);
        PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
        fei_call(number_actual_args,
                 type_desc,
                 By_Value_Call,
                 FALSE,
                 IR_INLINE_STATE(ir_idx),
                 flags);
# endif
        processing_call = processing_call - 1;
        break;




   case Br_Uncond_Opr :
        send_attr_ntry(IR_IDX_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_goto");
        PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(IR_IDX_R(ir_idx)));
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_goto(PDG_AT_IDX(IR_IDX_R(ir_idx)));
# endif
        break;




     
   case Br_True_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        send_attr_ntry(IR_IDX_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_brtrue");
        PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(IR_IDX_R(ir_idx)));
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_brtrue(PDG_AT_IDX(IR_IDX_R(ir_idx)));
# endif
        break;


   case If_Opr:
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_if");
        PDG_DBG_PRINT_END
# ifdef _HIGH_LEVEL_IF_FORM
# ifdef _ENABLE_FEI
        fei_if();
# endif
# endif
        break;


   case Else_Opr:

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_else");
        PDG_DBG_PRINT_END
# ifdef _HIGH_LEVEL_IF_FORM
# ifdef _ENABLE_FEI
        fei_else();
# endif
# endif
        break;

   case Endif_Opr:

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_endif");
        PDG_DBG_PRINT_END
# ifdef _HIGH_LEVEL_IF_FORM
# ifdef _ENABLE_FEI
        fei_endif();
# endif
# endif
        break;



   case Br_Aif_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        send_attr_ntry(IL_IDX(IR_IDX_R(ir_idx)));

        send_attr_ntry(IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))));

        send_attr_ntry(IL_IDX(IL_NEXT_LIST_IDX(
                                 IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)))));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_arith_goto");
        PDG_DBG_PRINT_LD("label idx1", PDG_AT_IDX(IL_IDX(IR_IDX_R(ir_idx))));
        PDG_DBG_PRINT_LD("label idx2", PDG_AT_IDX(IL_IDX(IL_NEXT_LIST_IDX(
                                               IR_IDX_R(ir_idx)))));
        PDG_DBG_PRINT_LD("label idx3", PDG_AT_IDX(IL_IDX(IL_NEXT_LIST_IDX(
                                    IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))))));
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_arith_goto(
                   PDG_AT_IDX(IL_IDX(IR_IDX_R(ir_idx))),
                   PDG_AT_IDX(IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)))),
                   PDG_AT_IDX(IL_IDX(IL_NEXT_LIST_IDX(
                                     IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))))));
# endif
        break;





   case Br_Asg_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        list_cnt = 0;
        i = SCP_ASSIGN_LBL_CHAIN(curr_scp_idx);

        while (i != NULL_IDX) {
           send_attr_ntry(i);

           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_label_ref");
           PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(i));
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           fei_label_ref(PDG_AT_IDX(i));
# endif

           list_cnt = list_cnt + 1;
           i = ATL_NEXT_ASG_LBL_IDX(i);
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_indirect_goto");
        PDG_DBG_PRINT_D("(1) list_cnt", list_cnt);
        PDG_DBG_PRINT_D("(2) TRUE", TRUE);
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_indirect_goto(list_cnt, TRUE);
# endif
        break;




   case Br_Index_Opr :
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        list_cnt = IR_LIST_CNT_R(ir_idx);
        tmp_ir_idx = IR_IDX_R(ir_idx);

        for (i = 0; i < list_cnt; i++) {
           send_attr_ntry(IL_IDX(tmp_ir_idx));

           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_label_ref");
           PDG_DBG_PRINT_LD("(1) label idx", PDG_AT_IDX(IL_IDX(tmp_ir_idx)));
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
            fei_label_ref(PDG_AT_IDX(IL_IDX(tmp_ir_idx)));
# endif
            tmp_ir_idx = IL_NEXT_LIST_IDX(tmp_ir_idx);
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_indirect_goto");
        PDG_DBG_PRINT_D("(1) list cnt", list_cnt);
        PDG_DBG_PRINT_D("(2) FALSE", FALSE);
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_indirect_goto(list_cnt, FALSE);
# endif
        break;






   case Where_Opr :
        if (IR_LIST_CNT_L(ir_idx) == 4) {
           /* defined assignment */
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
           proc_idx = IR_IDX_L(ir_idx);
           lhs_idx = IL_NEXT_LIST_IDX(proc_idx);
           mask_idx = IL_NEXT_LIST_IDX(lhs_idx);
           rhs_idx = IL_NEXT_LIST_IDX(mask_idx);
           cvrt_exp_to_pdg(IL_IDX(proc_idx),
                           IL_FLD(proc_idx));

           cvrt_exp_to_pdg(IL_IDX(lhs_idx),
                           IL_FLD(lhs_idx));

           cvrt_exp_to_pdg(IL_IDX(rhs_idx),
                           IL_FLD(rhs_idx));

           cvrt_exp_to_pdg(IL_IDX(mask_idx),
                           IL_FLD(mask_idx));
# else
           cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                           IR_FLD_L(ir_idx));
# endif

           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_where");
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           fei_where(TRUE, IR_INLINE_STATE(ir_idx));
# endif
        }
        else {
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
           lhs_idx = IR_IDX_L(ir_idx);
           mask_idx = IL_NEXT_LIST_IDX(lhs_idx);
           rhs_idx = IL_NEXT_LIST_IDX(mask_idx);
           cvrt_exp_to_pdg(IL_IDX(lhs_idx),
                           IL_FLD(lhs_idx));

           cvrt_exp_to_pdg(IL_IDX(rhs_idx),
                           IL_FLD(rhs_idx));

           cvrt_exp_to_pdg(IL_IDX(mask_idx),
                           IL_FLD(mask_idx));
# else
           cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                           IR_FLD_L(ir_idx));
# endif

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_where");
           PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
           fei_where(FALSE, 0);
# endif
        }
        break;


   case Where_Cnstrct_Opr:
   case Else_Where_Mask_Opr:
   case Else_Where_Opr:
        /* these are ignored */
        break;


   case Bounds_Cdir_Opr :
   case Nobounds_Cdir_Opr :
      bounds_cdir_handler(ir_idx);
      break;





   case Case_Cmic_Opr:
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_par_case");
      PDG_DBG_PRINT_D("(1) cmic vpr idx", case_cmic_vpr_idx);
      PDG_DBG_PRINT_D("(2) line num", IR_LINE_NUM(ir_idx));
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      task_tbl_idx = fei_par_case(case_cmic_vpr_idx, 
                                  IR_LINE_NUM(ir_idx));
# endif

      if (case_cmic_vpr_idx == NULL_IDX) {
         case_cmic_vpr_idx = task_tbl_idx;
      }
      break;







   case Endcase_Cmic_Opr:
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_par_endcase");
      PDG_DBG_PRINT_D("(1) cmic vpr idx", case_cmic_vpr_idx);
      PDG_DBG_PRINT_D("(2) line num", IR_LINE_NUM(ir_idx));
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      fei_par_endcase(case_cmic_vpr_idx, IR_LINE_NUM(ir_idx));
# endif

      case_cmic_vpr_idx = NULL_IDX;
      break;




   case Send_Cmic_Opr:
      if (IR_FLD_L(ir_idx) == CN_Tbl_Idx ||
          IR_FLD_L(ir_idx) == AT_Tbl_Idx) {
         cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                         IR_FLD_L(ir_idx));
         point = 1;
      }

      /* process IF clause */
      if (IR_FLD_R(ir_idx) == AT_Tbl_Idx) {
         send_attr_ntry(IR_IDX_R(ir_idx));
         task_if_idx = PDG_AT_IDX(IR_IDX_R(ir_idx));
      }

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_task_send");
      PDG_DBG_PRINT_LD("(1) point", point);
      PDG_DBG_PRINT_D("(2) task_if_idx", task_if_idx);
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      fei_task_send(point, task_if_idx);
# endif

      break;




   case Wait_Cmic_Opr:
      if (IR_FLD_L(ir_idx) == CN_Tbl_Idx ||
          IR_FLD_L(ir_idx) == AT_Tbl_Idx) {
         cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                         IR_FLD_L(ir_idx));
         point = 1;
      }

      if (IR_FLD_R(ir_idx) == CN_Tbl_Idx) {
         span = (long) CN_INT_TO_C(IR_IDX_R(ir_idx));
      }

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_task_wait");
      PDG_DBG_PRINT_LD("(1) point", point);
      PDG_DBG_PRINT_LD("(2) span", span);
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      fei_task_wait(point, span);
# endif

      break;



   case Guard_Cmic_Opr:
      if (IR_FLD_L(ir_idx) == AT_Tbl_Idx) {
         send_attr_ntry(IR_IDX_L(ir_idx));
      }

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_guard");
      PDG_DBG_PRINT_LD("(1) idx", (IR_FLD_L(ir_idx) == NO_Tbl_Idx) ?
                            0 : PDG_AT_IDX(IR_IDX_L(ir_idx)));
      PDG_DBG_PRINT_D("(2) line num", IR_LINE_NUM(ir_idx));
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      guard_cmic_vpr_idx = fei_guard((IR_FLD_L(ir_idx) == NO_Tbl_Idx) ?
                                         0 : PDG_AT_IDX(IR_IDX_L(ir_idx)),
                                         IR_LINE_NUM(ir_idx));
# endif

      break;






   case Endguard_Cmic_Opr:
      if (IR_FLD_L(ir_idx) == AT_Tbl_Idx) {
         send_attr_ntry(IR_IDX_L(ir_idx));
      }

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_endguard");
      PDG_DBG_PRINT_LD("(1) guard idx", (IR_FLD_L(ir_idx) == NO_Tbl_Idx) ? 0 :
                                  PDG_AT_IDX(IR_IDX_L(ir_idx)));
      PDG_DBG_PRINT_D("(2) line num", IR_LINE_NUM(ir_idx));
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      fei_endguard(guard_cmic_vpr_idx, 
                       (IR_FLD_L(ir_idx) == NO_Tbl_Idx) ? 0 :
                                                   PDG_AT_IDX(IR_IDX_L(ir_idx)),
                       IR_LINE_NUM(ir_idx));
# endif

      guard_cmic_vpr_idx = NULL_IDX;
      break;





   case Numcpus_Cmic_Opr :
      cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                      IR_FLD_L(ir_idx));

      PDG_DBG_PRINT_START    
      PDG_DBG_PRINT_C("fei_numcpus");
      PDG_DBG_PRINT_D("(1) line num", IR_LINE_NUM(ir_idx));
      PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
      fei_numcpus(IR_LINE_NUM(ir_idx));
# endif
      break;






   case Ranf_Opr :
        basic = get_basic_type(IR_TYPE_IDX(ir_idx), 0, NULL_IDX);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_ranf");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_ranf(basic);
# endif

        break;







   case Regionbegin_Star_Opr:
   case Regionend_Star_Opr:
   case End_Critical_Section_Par_Opr:
   case End_Parallel_Par_Opr:
   case Section_Par_Opr:
   case Barrier_Par_Opr:
        switch (IR_OPR(ir_idx)) {
        case Regionbegin_Star_Opr:
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_regionbegin");
             PDG_DBG_PRINT_END
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
             fei_regionbegin();
# endif
# endif
             break;

        case Regionend_Star_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_regionend");
             PDG_DBG_PRINT_END
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
             fei_regionend();
# endif
# endif
             break;

        case End_Critical_Section_Par_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_endcriticalsection");
             PDG_DBG_PRINT_END
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
             fei_endcriticalsection();
# endif
# endif
             break;

        case End_Parallel_Par_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_endparallel");
             PDG_DBG_PRINT_END
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
             fei_endparallel();
# endif
# endif
             break;

        case Section_Par_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_section");
             PDG_DBG_PRINT_END
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
             fei_section();
# endif
# endif
             break;

        case Barrier_Par_Opr :
             PDG_DBG_PRINT_START
             PDG_DBG_PRINT_C("fei_barrier");
             PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
             fei_barrier();
# endif
# endif
             break;
        }
        break;










   case End_Pdo_Par_Opr:
        if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
           nowait = (long) CN_INT_TO_C(IR_IDX_L(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_endpdo");
        PDG_DBG_PRINT_LD("(1) nowait", nowait);
        PDG_DBG_PRINT_END


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_endpdo(nowait);
# endif
# endif
        break;




   case End_Psection_Par_Opr:
        if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
           nowait = (long) CN_INT_TO_C(IR_IDX_L(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_endpsection");
        PDG_DBG_PRINT_LD("(1) nowait", nowait);
        PDG_DBG_PRINT_END


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_endpsection(nowait);
# endif
# endif
        break;



   case End_Singleprocess_Par_Opr:
       if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
          nowait = (long) CN_INT_TO_C(IR_IDX_L(ir_idx));
       }

       PDG_DBG_PRINT_START
       PDG_DBG_PRINT_C("fei_endsingleprocess");
       PDG_DBG_PRINT_LD("(1) nowait", nowait);
       PDG_DBG_PRINT_END


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
       fei_endsingleprocess(nowait);
# endif
# endif
       break;




   case Critical_Section_Par_Opr:
        if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {
           cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                           IR_FLD_L(ir_idx));
           varcount = 1;
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_criticalsection");
        PDG_DBG_PRINT_LD("(1) varcount", varcount);
        PDG_DBG_PRINT_END


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_criticalsection(varcount);
# endif
# endif
        break;


   case User_Code_Start_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_user_code_start");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_user_code_start();
# endif
# endif
        break;



   case Psection_Par_Opr:
   case Singleprocess_Par_Opr:
   case Parallel_Do_Par_Opr:
   case Parallel_Par_Opr:
   case Pdo_Par_Opr:
   case Doacross_Dollar_Opr:
        list_idx1 = IR_IDX_L(ir_idx);

        for (i = 0; i < MP_DIR_LIST_CNT; i++) {
           list_array[i] = list_idx1;
           list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
        }


        /* process IF clause */
        if (IL_FLD(list_array[MP_DIR_IF_IDX]) == NO_Tbl_Idx) {
           task_if_idx = 0;
        }
        else {
           send_attr_ntry(IL_IDX(list_array[MP_DIR_IF_IDX]));
           task_if_idx = PDG_AT_IDX(IL_IDX(list_array[MP_DIR_IF_IDX]));
        }


        /* process ORDERED clause */
        if (IL_FLD(list_array[MP_DIR_ORDERED_IDX]) == CN_Tbl_Idx) {
           ordered = (long) CN_INT_TO_C(IL_IDX(list_array[MP_DIR_ORDERED_IDX]));
        }


        /* process SCHEDTYPE clause */
        if (IL_FLD(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) == CN_Tbl_Idx) {
           schedtype = (long)
                       CN_INT_TO_C(IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]));
        }


        /* process LASTTHREAD clause */
        if (IL_FLD(list_array[MP_DIR_LASTTHREAD_IDX]) == AT_Tbl_Idx) {
           send_attr_ntry(IL_IDX(list_array[MP_DIR_LASTTHREAD_IDX]));

           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_task_var");
           PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
                       PDG_AT_IDX(IL_IDX(list_array[MP_DIR_LASTTHREAD_IDX]))); 
           PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Lastthread]);
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           last_task_idx = fei_task_var(
                       PDG_AT_IDX(IL_IDX(list_array[MP_DIR_LASTTHREAD_IDX])),  
                       Context_Lastthread);
# endif
        }


        /* process SHARED clause */
        if (IL_FLD(list_array[MP_DIR_SHARE_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[MP_DIR_SHARE_IDX]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
					PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Shared]);

                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)), 
                                              Context_Shared);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
        }


        /* process LASTLOCAL clause */
        if (IL_FLD(list_array[MP_DIR_LASTLOCAL_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[MP_DIR_LASTLOCAL_IDX]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
                                   PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Lastlocal]);

                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Lastlocal);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2); 
           }  
        }






        /* process PRIVATE clause */
        if (IL_FLD(list_array[MP_DIR_LOCAL_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[MP_DIR_LOCAL_IDX]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
                                         PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Private]);

                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Private);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
        }




        /* process NEST clause */
        if (IL_FLD(list_array[MP_DIR_NEST_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[MP_DIR_NEST_IDX]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", 
                                     PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Nest]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Nest);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2); 
           }  
        }



        /* process AFFINITY clause */
        if (IL_FLD(list_array[MP_DIR_AFFINITY_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[MP_DIR_AFFINITY_IDX]);

           while (list_idx2 != NULL_IDX) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
                                        PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Affinity]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Affinity);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }

           cvrt_exp_to_pdg(IL_IDX(list_array[MP_DIR_THREAD_DATA_IDX]), 
                           IL_FLD(list_array[MP_DIR_THREAD_DATA_IDX]));

           cvrt_exp_to_pdg(IL_IDX(list_array[MP_DIR_THREAD_DATA_IDX]), 
                           IL_FLD(list_array[MP_DIR_THREAD_DATA_IDX]));

           which = (long) CN_INT_TO_C(IL_IDX(list_array[MP_DIR_IS_THREAD_IDX]));

           if (which == 1) {
              threadcount = 1;
           }
           else {
              datacount = 1;
           }
        }




        /* process ONTO clause */
        if (IL_FLD(list_array[MP_DIR_ONTO_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[MP_DIR_ONTO_IDX]);

           ontocount = 0;
           while (list_idx2 != NULL_IDX) {
              cvrt_exp_to_pdg(IL_IDX(list_idx2), 
                              IL_FLD(list_idx2));

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
              ontocount = ontocount + 1;
           }
        }




        /* process REDUCTION clause */
        if (IL_FLD(list_array[MP_DIR_REDUCTION_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[MP_DIR_REDUCTION_IDX]);

           reductioncount = 0;
           while (list_idx2 != NULL_IDX) {
              cvrt_exp_to_pdg(IL_IDX(list_idx2), 
                              IL_FLD(list_idx2));

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
              reductioncount = reductioncount + 1;
           }
        }



        /* process CHUNK clause */
        if (IL_FLD(list_array[MP_DIR_CHUNK_IDX]) != NO_Tbl_Idx) {
           cvrt_exp_to_pdg(IL_IDX(list_array[MP_DIR_CHUNK_IDX]), 
                           IL_FLD(list_array[MP_DIR_CHUNK_IDX]));
           chunkcount = 1;
        }



        switch (IR_OPR(ir_idx)) {

        case Doacross_Dollar_Opr :   
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_doacross");
           PDG_DBG_PRINT_D("(1) if idx", task_if_idx);
           PDG_DBG_PRINT_LD("(2) schedtype", schedtype);
           PDG_DBG_PRINT_LD("(3) threadcount", threadcount);
           PDG_DBG_PRINT_LD("(4) datacount", datacount);
           PDG_DBG_PRINT_LD("(5) ontocount", ontocount);
           PDG_DBG_PRINT_LD("(6) reductioncount", reductioncount);
           PDG_DBG_PRINT_LD("(7) chunkcount", chunkcount);
           PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
           fei_doacross(task_if_idx,
                        schedtype,
                        threadcount,
                        datacount,
                        ontocount,
                        reductioncount,
                        chunkcount);

# endif
# endif
           break;



        case Pdo_Par_Opr :
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_pdo");
           PDG_DBG_PRINT_LD("(1) schedtype", schedtype);
           PDG_DBG_PRINT_LD("(2) ordered", ordered);
           PDG_DBG_PRINT_LD("(3) threadcount", threadcount);
           PDG_DBG_PRINT_LD("(4) datacount", datacount);
           PDG_DBG_PRINT_LD("(5) ontocount", ontocount);
           PDG_DBG_PRINT_LD("(6) reductioncount", reductioncount);
           PDG_DBG_PRINT_LD("(7) chunkcount", chunkcount);
           PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
           fei_pdo(schedtype,
                   ordered, 
                   threadcount,
                   datacount,
                   ontocount,
                   reductioncount,
                   chunkcount);
# endif
# endif
           break;



        case Parallel_Do_Par_Opr :
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_paralleldo");
           PDG_DBG_PRINT_D("(1) if idx", task_if_idx);
           PDG_DBG_PRINT_LD("(2) schedtype", schedtype);
           PDG_DBG_PRINT_LD("(3) threadcount", threadcount);
           PDG_DBG_PRINT_LD("(4) datacount", datacount);
           PDG_DBG_PRINT_LD("(5) ontocount", ontocount);
           PDG_DBG_PRINT_LD("(6) reductioncount", reductioncount);
           PDG_DBG_PRINT_LD("(7) chunkcount", chunkcount);
           PDG_DBG_PRINT_END


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
           fei_paralleldo(task_if_idx,
                          schedtype,
                          threadcount,
                          datacount,
                          ontocount,
                          reductioncount,
                          chunkcount);
# endif
# endif
           break;




        case Parallel_Par_Opr :
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_parallel");
           PDG_DBG_PRINT_D("(1) if idx", task_if_idx);
           PDG_DBG_PRINT_END


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
           fei_parallel(task_if_idx);
# endif
# endif
           break;



        case Singleprocess_Par_Opr :
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_singleprocess");
           PDG_DBG_PRINT_END


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
           fei_singleprocess();
# endif
# endif
           break;




        case Psection_Par_Opr:
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_psection");
           PDG_DBG_PRINT_END


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
           fei_psection();
# endif
# endif
           break;

        }
        break;





   case Concurrentize_Star_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_concurrentize");
        PDG_DBG_PRINT_D("(1) state", TRUE);
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_concurrentize(TRUE);
# endif
# endif

      break;





   case Noconcurrentize_Star_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_concurrentize");
        PDG_DBG_PRINT_D("(1) state", FALSE);
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_concurrentize(FALSE);
# endif
# endif

      break;








   case Interchange_Dir_Opr:

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_interchange");
        PDG_DBG_PRINT_D("(1) expressions", IR_LIST_CNT_L(ir_idx));
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_interchange(IR_LIST_CNT_L(ir_idx));
# endif
# endif
        break;






   case Blockable_Dir_Opr:

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_blockable");
        PDG_DBG_PRINT_D("(1) expressions", IR_LIST_CNT_L(ir_idx));
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_blockable(IR_LIST_CNT_L(ir_idx));
# endif
# endif
        break;



   case Fission_Star_Opr:
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_fission");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_fission();
# endif
# endif
        break;


#ifdef KEY
   case Forall_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_forall");
        PDG_DBG_PRINT_END
                                                                                                                                                             
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_forall();
# endif
# endif
        break;
#endif


   case Flush_Star_Opr:
        list_cnt = 0;
        if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
           list_cnt = IR_LIST_CNT_L(ir_idx);
        }

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_flush");
        PDG_DBG_PRINT_D("(1) list_cnt", list_cnt);
        PDG_DBG_PRINT_END


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_flush(list_cnt);
# endif
# endif
        break;

   case Fuse_Star_Opr:
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        if (IR_FLD_R(ir_idx) == CN_Tbl_Idx) {
           level = (long) CN_INT_TO_C(IR_IDX_R(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_fuse");
        PDG_DBG_PRINT_LD("(1) level", level);
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_fuse(level);
# endif
# endif
        break;








   case Assert_Star_Opr:
        assertion = (long) CN_INT_TO_C(IR_IDX_L(ir_idx));

        if (IR_FLD_R(ir_idx) == NO_Tbl_Idx) {
           count = 0;
        }
        else if (IR_FLD_R(ir_idx) == IL_Tbl_Idx) {
           count = IR_LIST_CNT_R(ir_idx);
        }
        else {
           count = 1;
        }

        if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
           cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                           IR_FLD_R(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_assert");
        PDG_DBG_PRINT_LD("(1) assertion", assertion);
        PDG_DBG_PRINT_D("(2) count    ", count);
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_assert(assertion, count);
# endif
# endif
        break;





   case Unroll_Star_Opr:
// Bug 1520
#ifdef KEY
   case Unroll_Cdir_Opr:
#endif
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_unroll");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_unroll();
# endif
# endif
        break;





   case Section_Nongp_Star_Opr:
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_section_nongp");
        PDG_DBG_PRINT_D("(1) list count", IR_LIST_CNT_L(ir_idx));
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_section_nongp(IR_LIST_CNT_L(ir_idx));
# endif
# endif
        break;






   case Section_Gp_Star_Opr:
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_section_gp");
        PDG_DBG_PRINT_D("(1) list count", IR_LIST_CNT_L(ir_idx));
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_section_gp(IR_LIST_CNT_L(ir_idx));
# endif
# endif
        break;





   case Copyin_Dollar_Opr:
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_copy_in");
        PDG_DBG_PRINT_D("(1) list count", IR_LIST_CNT_L(ir_idx));
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_copy_in(IR_LIST_CNT_L(ir_idx));
# endif
# endif
        break;









   case Align_Symbol_Star_Opr:
        if (IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
           list_cnt = 0;
        }
        else {
           list_cnt = 1;
           cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                           IR_FLD_L(ir_idx));
        }

        if (IR_FLD_R(ir_idx) == CN_Tbl_Idx) {
           C_value = (long) CN_INT_TO_C(IR_IDX_R(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_align_symbol");
        PDG_DBG_PRINT_D("(1) list_cnt", list_cnt);
        PDG_DBG_PRINT_LD("(2) C_value", C_value);
        PDG_DBG_PRINT_END


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_align_symbol(list_cnt, C_value);
# endif
# endif

        break;





   case Fill_Symbol_Star_Opr:
        if (IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
           list_cnt = 0;
        }
        else {
           list_cnt = 1;
           cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                           IR_FLD_L(ir_idx));
        }

        if (IR_FLD_R(ir_idx) == CN_Tbl_Idx) {
           C_value = (long) CN_INT_TO_C(IR_IDX_R(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_fill_symbol");
        PDG_DBG_PRINT_D("(1) list_cnt", list_cnt);
        PDG_DBG_PRINT_LD("(2) C_value", C_value);
        PDG_DBG_PRINT_END


# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_fill_symbol(list_cnt, C_value);
# endif
# endif
        break;







   case Blockingsize_Dir_Opr:
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_blocking_size");
        PDG_DBG_PRINT_END

# if !defined(_TARGET_OS_MAX)
# ifdef _ENABLE_FEI
        fei_blocking_size();
# endif
# endif
        break;




   case Opaque_Star_Opr:
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_opaque");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_opaque();
# endif
# endif
        break;







   case Dynamic_Dollar_Opr:
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        list_cnt = IR_LIST_CNT_L(ir_idx);

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_dynamic");
        PDG_DBG_PRINT_D("(1) list count", list_cnt);
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_dynamic(list_cnt);
# endif
# endif
        break;





   case Page_Place_Dollar_Opr:
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_page_place");
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_page_place();
# endif
# endif
        break;




   case Prefetch_Manual_Star_Opr:
        if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
           prefetch_manual = (long) CN_INT_TO_C(IR_IDX_L(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_prefetch_manual");
        PDG_DBG_PRINT_LD("(1) prefetch_manual", prefetch_manual);
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_prefetch_manual(prefetch_manual);
# endif
# endif
        break;






   case Prefetch_Star_Opr:
        if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
           n1 = (long) CN_INT_TO_C(IR_IDX_L(ir_idx));
        }

        if (IR_FLD_R(ir_idx) == CN_Tbl_Idx) {
           n2 = (long) CN_INT_TO_C(IR_IDX_R(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_prefetch");
        PDG_DBG_PRINT_LD("(1) n1", n1);
        PDG_DBG_PRINT_LD("(2) n2", n2);
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_prefetch(n1, n2);
# endif
# endif
        break;

#ifdef KEY /* Bug 2660 */
   case Options_Dir_Opr:
	{
        char *c1 = (char *) &CN_CONST(IR_IDX_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_options");
        PDG_DBG_PRINT_S("(1) c1", c1);
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
	fei_options(c1);
# endif
# endif
	}
        break;
#endif /* KEY Bug 2660 */





   case Prefetch_Ref_Disable_Star_Opr:
        if (IR_FLD_L(ir_idx) == AT_Tbl_Idx) {
           send_attr_ntry(IR_IDX_L(ir_idx));
           prefetch_array = (long) PDG_AT_IDX(IR_IDX_L(ir_idx));
        }

        if (IR_FLD_R(ir_idx) == CN_Tbl_Idx) {
           prefetch_size = (long) CN_INT_TO_C(IR_IDX_R(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_prefetch_ref_disable");
        PDG_DBG_PRINT_LD("(1) prefetch_array", prefetch_array);
        PDG_DBG_PRINT_LD("(2) prefetch_size", prefetch_size);
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_prefetch_ref_disable(prefetch_array, prefetch_size);
# endif
# endif
        break;







   case Prefetch_Ref_Star_Opr:
        list_idx1 = IR_IDX_L(ir_idx);

        for (i = 0; i < 5; i++) {
           list_array[i] = list_idx1;
           list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
        }

        /* stack the array reference */
        cvrt_exp_to_pdg(IL_IDX(list_array[0]),
                        IL_FLD(list_array[0]));

        /* stack the stride list - 1 or 2 items in list */
        cvrt_exp_to_pdg(IL_IDX(list_array[1]),
                        IL_FLD(list_array[1]));
        if (IL_FLD(list_array[1]) != NO_Tbl_Idx) {
           stride_list = IL_LIST_CNT(list_array[1]);
        }

        /* stack the level list - 1 or 2 items in list */
        cvrt_exp_to_pdg(IL_IDX(list_array[2]),
                        IL_FLD(list_array[2]));
        if (IL_FLD(list_array[2]) != NO_Tbl_Idx) {
           level_list = IL_LIST_CNT(list_array[2]);
        }

        if (IL_FLD(list_array[3]) == CN_Tbl_Idx) {
           prefetch_kind = (long) CN_INT_TO_C(IL_IDX(list_array[3]));
        }

        if (IL_FLD(list_array[4]) == CN_Tbl_Idx) {
           prefetch_size = (long) CN_INT_TO_C(IL_IDX(list_array[4]));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_prefetch_ref");
        PDG_DBG_PRINT_D("(1) stride_list", stride_list);
        PDG_DBG_PRINT_D("(2) level_list", level_list);
        PDG_DBG_PRINT_LD("(3) prefetch_kind", prefetch_kind);
        PDG_DBG_PRINT_LD("(4) prefetch_size", prefetch_size);
        PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
        fei_prefetch_ref(stride_list,
                         level_list,
                         prefetch_kind, 
                         prefetch_size);
# endif
# endif

        break;






   case Redistribute_Dollar_Opr:
        if (IR_FLD_L(ir_idx) == AT_Tbl_Idx) {
           send_attr_ntry(IR_IDX_L(ir_idx));
           redistribute_array = PDG_AT_IDX(IR_IDX_L(ir_idx));
        }

        cyclic_list = IR_IDX_R(ir_idx);
        list_cnt = IL_LIST_CNT(cyclic_list);
        onto_list = IL_NEXT_LIST_IDX(cyclic_list);
        cyclic_list = IL_IDX(cyclic_list);

        if (onto_list) {
           onto_list = IL_IDX(onto_list);
        }

        for (j = 1; j <= list_cnt; j++) {
           cyclic_exists = 0;
           onto_exists = 0;

           if (onto_list) {
              cvrt_exp_to_pdg(IL_IDX(onto_list),
                              IL_FLD(onto_list));
              if (IL_FLD(onto_list) != NO_Tbl_Idx) {
                 onto_exists = 1;
              }
           }

           cvrt_exp_to_pdg(IL_IDX(cyclic_list),
                           IL_FLD(cyclic_list));
           if (IL_FLD(cyclic_list) != NO_Tbl_Idx) {
              cyclic_exists = 1;
           }

           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_redistribute");
           PDG_DBG_PRINT_S("AT_OBJ_NAME", 
                             AT_OBJ_NAME_PTR(IR_IDX_L(ir_idx)));
           PDG_DBG_PRINT_D("(1) array", redistribute_array);
           PDG_DBG_PRINT_D("(2) dimension", j);
           PDG_DBG_PRINT_D("(3) distribution", IL_DISTRIBUTION(cyclic_list));
           PDG_DBG_PRINT_LD("(4) cyclic_exists", cyclic_exists);
           PDG_DBG_PRINT_LD("(5) onto_exists", onto_exists);
           PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _ENABLE_FEI
           fei_redistribute(redistribute_array,
                            j, 
                            IL_DISTRIBUTION(cyclic_list),
                            cyclic_exists,
                            onto_exists);
# endif
# endif
           cyclic_list = IL_NEXT_LIST_IDX(cyclic_list);

           if (onto_list) {
              onto_list = IL_NEXT_LIST_IDX(onto_list);
           }
        }
        break;



   case Doall_Cmic_Opr:
        list_idx1 = IR_IDX_L(ir_idx);

        for (i = 0; i < 10; i++) {
           list_array[i] = list_idx1;
           list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
        }

        first_task_idx = NULL_IDX;
        last_task_idx = first_task_idx;

        /* process SHARED list */

        if (IL_FLD(list_array[1]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[1]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
					PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Shared]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)), 
                                              Context_Shared);
# endif

                 if (first_task_idx == NULL_IDX) {
                    first_task_idx = last_task_idx;
                 }
              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
        }

        /* process PRIVATE list */

        if (IL_FLD(list_array[2]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[2]);

           idx = Context_Private; 
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
           if (IL_FLD(list_array[6]) == CN_Tbl_Idx) { /* SAVELAST */
              idx = Context_Lastlocal; 
           }
# endif

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
					PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context", p_tasking_context[idx]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              idx);
# endif

                 if (first_task_idx == NULL_IDX) {
                    first_task_idx = last_task_idx;
                 }
              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2); 
           }  
        }

        /* process GETFIRST list */

        if (IL_FLD(list_array[3]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[3]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
					PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Getfirst]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Getfirst);
# endif

                 if (first_task_idx == NULL_IDX) {
                    first_task_idx = last_task_idx;
                 }
              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
        }


        /* process CONTROL list */

        if (IL_FLD(list_array[5]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[5]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
					PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Control]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Control);
# endif

                 if (first_task_idx == NULL_IDX) {
                    first_task_idx = last_task_idx;
                 }
              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2); 
           }  
        }

        j = 0;

        if (IL_FLD(list_array[4]) == CN_Tbl_Idx) {
           /* AUTOSCOPE */
           j = j + 2;
        }

        if (IL_FLD(list_array[6]) == CN_Tbl_Idx) {
           /* SAVELAST */
           j = j + 1;
        }

        if (IL_FLD(list_array[0]) == NO_Tbl_Idx) {
           task_if_idx = 0;
        }
        else {
           send_attr_ntry(IL_IDX(list_array[0]));
           task_if_idx = PDG_AT_IDX(IL_IDX(list_array[0]));
        }

        if (IL_FLD(list_array[7]) == NO_Tbl_Idx) {
           task_maxcpus_idx = 0;
        }
        else {
           send_attr_ntry(IL_IDX(list_array[7]));
           task_maxcpus_idx = PDG_AT_IDX(IL_IDX(list_array[7]));
        }

        if (IL_FLD(list_array[9]) == NO_Tbl_Idx) {
           task_wdist_idx = 0;
        }
        else {
           send_attr_ntry(IL_IDX(list_array[9]));
           task_wdist_idx = PDG_AT_IDX(IL_IDX(list_array[9]));
        }

        send_attr_ntry(IR_IDX_R(ir_idx));
        big_int = CN_INT_TO_C(IL_IDX(list_array[8]));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_doall");
        PDG_DBG_PRINT_D("(1) if idx", task_if_idx);
        PDG_DBG_PRINT_D("(2) maxcpus idx", task_maxcpus_idx);
        PDG_DBG_PRINT_D("(3) first task idx", first_task_idx);
        PDG_DBG_PRINT_D("(4) last task idx", last_task_idx);
        PDG_DBG_PRINT_LD("(5) obj idx", PDG_AT_IDX(IR_IDX_R(ir_idx)));
#if defined(_HOST32) && defined(_TARGET64)
        PDG_DBG_PRINT_D("(6) const", big_int);
#else
        PDG_DBG_PRINT_VD("(6) const", big_int);
#endif
        PDG_DBG_PRINT_D("(7) wdist idx", task_wdist_idx);
        PDG_DBG_PRINT_D("(8)  ", j);
        PDG_DBG_PRINT_D("(9) line num", IR_LINE_NUM(ir_idx));
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_doall(task_if_idx,
                  task_maxcpus_idx,
                  first_task_idx,
                  last_task_idx,
                  PDG_AT_IDX(IR_IDX_R(ir_idx)),
                  big_int,
                  task_wdist_idx,
                  j,
                  IR_LINE_NUM(ir_idx));
# endif

        break;





   case Doparallel_Cmic_Opr:
        list_idx1 = IR_IDX_L(ir_idx);

        for (i = 0; i < 2; i++) {
           list_array[i] = list_idx1;
           list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
        }

        if (IL_FLD(list_array[1]) == NO_Tbl_Idx) {
           task_wdist_idx = 0;
        }
        else {
           send_attr_ntry(IL_IDX(list_array[1]));
           task_wdist_idx = PDG_AT_IDX(IL_IDX(list_array[1]));
        }

        send_attr_ntry(IR_IDX_R(ir_idx));
        big_int = CN_INT_TO_C(IL_IDX(list_array[0]));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_doparallel");
        PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(IR_IDX_R(ir_idx)));
#if defined(_HOST32) && defined(_TARGET64)
        PDG_DBG_PRINT_D("(2) const", big_int);
#else
        PDG_DBG_PRINT_VD("(2) const", big_int);
#endif
        PDG_DBG_PRINT_D("(3) task idx", task_wdist_idx);
        PDG_DBG_PRINT_D("(4) line num", IR_LINE_NUM(ir_idx));
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        doparallel_cmic_vpr_idx = 
                       fei_doparallel(PDG_AT_IDX(IR_IDX_R(ir_idx)),
                                      big_int,
                                      task_wdist_idx,
                                      IR_LINE_NUM(ir_idx));
# endif

        break;




   case Enddo_Cmic_Opr:

        if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
           nowait = (long) CN_INT_TO_C(IR_IDX_L(ir_idx));
        }
            
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_task_endloop");
        PDG_DBG_PRINT_D("(1) pdg idx", doparallel_cmic_vpr_idx);
        PDG_DBG_PRINT_D("(2) line num", IR_LINE_NUM(ir_idx));
        PDG_DBG_PRINT_LD("(3) nowait", nowait);
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_task_endloop(doparallel_cmic_vpr_idx, 
                         IR_LINE_NUM(ir_idx),
                         nowait);
# endif
        doparallel_cmic_vpr_idx = NULL_IDX;
        break;
           





   case Parallel_Cmic_Opr:
        list_idx1 = IR_IDX_L(ir_idx);

        for (i = 0; i < 7; i++) {
           list_array[i] = list_idx1;
           list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
        }

        first_task_idx = NULL_IDX;

        /* process SHARED list */

        if (IL_FLD(list_array[1]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[1]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
					PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Shared]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Shared);
# endif

                 if (first_task_idx == NULL_IDX) {
                    first_task_idx = last_task_idx;
                 }
              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
        }

        /* process PRIVATE list */

        if (IL_FLD(list_array[2]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[2]);

           while (list_idx2) { 
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
					PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Private]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Private);
# endif

                 if (first_task_idx == NULL_IDX) {
                    first_task_idx = last_task_idx;
                 }
              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
        }

        /* process GETFIRST list */

        if (IL_FLD(list_array[3]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[3]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
					PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Getfirst]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Getfirst);
# endif

                 if (first_task_idx == NULL_IDX) {
                    first_task_idx = last_task_idx;
                 }
              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
        }

        /* process CONTROL list */

        if (IL_FLD(list_array[5]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[5]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", 
					PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Control]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Control);
# endif

                 if (first_task_idx == NULL_IDX) {
                    first_task_idx = last_task_idx;
                 }
              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
        }

        i = 0L;

        if (IL_FLD(list_array[4]) == CN_Tbl_Idx) {
           i = 2;
        }

        if (IL_FLD(list_array[0]) == NO_Tbl_Idx) {
           task_if_idx = 0;
        }
        else {
           send_attr_ntry(IL_IDX(list_array[0]));
           task_if_idx = PDG_AT_IDX(IL_IDX(list_array[0]));
        }

        if (IL_FLD(list_array[6]) == NO_Tbl_Idx) {
           task_maxcpus_idx = 0;
        }
        else {
           send_attr_ntry(IL_IDX(list_array[6]));
           task_maxcpus_idx = PDG_AT_IDX(IL_IDX(list_array[6]));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_parallel_region");
        PDG_DBG_PRINT_D("(1) if idx", task_if_idx);
        PDG_DBG_PRINT_D("(2) maxcpus idx", task_maxcpus_idx);
        PDG_DBG_PRINT_D("(3) first task idx", first_task_idx);
        PDG_DBG_PRINT_D("(4) last task idx", last_task_idx);
        PDG_DBG_PRINT_D("(5) line num", IR_LINE_NUM(ir_idx));
        PDG_DBG_PRINT_D("(6)  ", (int) i);
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        parallel_cmic_vpr_idx = fei_parallel_region(task_if_idx,
                                                    task_maxcpus_idx,
                                                    first_task_idx,
                                                    last_task_idx,
                                                    IR_LINE_NUM(ir_idx),
                                                    i);
# endif

        break;





   case Endparallel_Cmic_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_endparallel_region");
        PDG_DBG_PRINT_D("(1) cmic vpr idx", doparallel_cmic_vpr_idx);
        PDG_DBG_PRINT_D("(2) line num", IR_LINE_NUM(ir_idx));
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        fei_endparallel_region(parallel_cmic_vpr_idx, IR_LINE_NUM(ir_idx));
# endif
        parallel_cmic_vpr_idx = NULL_IDX;
        break;





   case Suppress_Opr:
        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        send_label_def(ir_idx);
        break;

    
   case Use_Opr:

# if defined(_MODULE_TO_DOT_o)
        if (!on_off_flags.module_to_mod &&
# else
        if (
# endif
            !ATP_IN_CURRENT_COMPILE(IR_IDX_L(ir_idx))) {

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_next_name");
           PDG_DBG_PRINT_D("(1) logical", TRUE);
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           path_idx = fei_next_name(TRUE);
# endif

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_name");
           PDG_DBG_PRINT_S("(1) name_ptr",
                    ATP_MOD_PATH_NAME_PTR(IR_IDX_L(ir_idx)));
           PDG_DBG_PRINT_D("(2) unused", 0);
           PDG_DBG_PRINT_D("(3) unused", 0);
           PDG_DBG_PRINT_D("(4) unused", 0);
           PDG_DBG_PRINT_D("(5) index to use", path_idx);
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           fei_name(ATP_MOD_PATH_NAME_PTR(IR_IDX_L(ir_idx)),
                    0,
                    0,
                    0,
                    path_idx);
# endif


           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_next_name");
           PDG_DBG_PRINT_D("(1) logical", TRUE);
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           name_idx = fei_next_name(TRUE);
# endif

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_name");
           PDG_DBG_PRINT_S("(1) AT_OBJ_NAME", 
                             AT_OBJ_NAME_PTR(IR_IDX_L(ir_idx)));
           PDG_DBG_PRINT_D("(2) unused", 0);
           PDG_DBG_PRINT_D("(3) unused", 0);
           PDG_DBG_PRINT_D("(4) unused", 0);
           PDG_DBG_PRINT_D("(5) index to use", name_idx);
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           fei_name(AT_OBJ_NAME_PTR(IR_IDX_L(ir_idx)), 
                    0,
                    0, 
                    0, 
                    name_idx);
# endif

           PDG_DBG_PRINT_START    
           PDG_DBG_PRINT_C("fei_add_use_path");
           PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", 
                         PDG_AT_IDX(SCP_ATTR_IDX(curr_scp_idx)));
           PDG_DBG_PRINT_D("(2) path index", path_idx);
           PDG_DBG_PRINT_D("(3) name index", name_idx);
           PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
           fei_add_use_path(PDG_AT_IDX(SCP_ATTR_IDX(curr_scp_idx)),
                            path_idx,
                            name_idx);
# endif
        }
        break;




   case Symbolic_Mult_Opr:
        save_symbolic_constant_expr = symbolic_constant_expr;
        symbolic_constant_expr = TRUE;

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("npex_mult");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        npex_mult();
# endif

        symbolic_constant_expr = save_symbolic_constant_expr;

        if (!symbolic_constant_expr) {
           finish_symbolic_expr();
        }
        break;





   case Symbolic_Div_Opr:
        save_symbolic_constant_expr = symbolic_constant_expr;
        symbolic_constant_expr = TRUE;

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("npex_div");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        npex_div();
# endif

        symbolic_constant_expr = save_symbolic_constant_expr;

        if (!symbolic_constant_expr) {
           finish_symbolic_expr();
        }
        break;




   case Symbolic_Plus_Opr:
        save_symbolic_constant_expr = symbolic_constant_expr;
        symbolic_constant_expr = TRUE;

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("npex_plus");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        npex_plus();
# endif

        symbolic_constant_expr = save_symbolic_constant_expr;

        if (!symbolic_constant_expr) {
           finish_symbolic_expr();
        }
        break;





   case Symbolic_Minus_Opr:
        save_symbolic_constant_expr = symbolic_constant_expr;
        symbolic_constant_expr = TRUE;

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("npex_minus");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        npex_minus();
# endif

        symbolic_constant_expr = save_symbolic_constant_expr;

        if (!symbolic_constant_expr) {
           finish_symbolic_expr();
        }
        break;





   case Symbolic_Uminus_Opr :
        save_symbolic_constant_expr = symbolic_constant_expr;
        symbolic_constant_expr = TRUE;

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("npex_uminus");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        npex_uminus();
# endif

        symbolic_constant_expr = save_symbolic_constant_expr;

        if (!symbolic_constant_expr) {
           finish_symbolic_expr();
        }
        break;




   case Symbolic_Uplus_Opr :
        save_symbolic_constant_expr = symbolic_constant_expr;
        symbolic_constant_expr = TRUE;

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));


        symbolic_constant_expr = save_symbolic_constant_expr;

        if (!symbolic_constant_expr) {
           finish_symbolic_expr();
        }
        break;




   case Symbolic_Mod_Opr:
        save_symbolic_constant_expr = symbolic_constant_expr;
        symbolic_constant_expr = TRUE;

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("npex_mod");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        npex_mod();
# endif

        symbolic_constant_expr = save_symbolic_constant_expr;

        if (!symbolic_constant_expr) {
           finish_symbolic_expr();
        }
        break;




   case Symbolic_Shiftr_Opr:
        save_symbolic_constant_expr = symbolic_constant_expr;
        symbolic_constant_expr = TRUE;

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("npex_shiftr");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        npex_shiftr();
# endif

        symbolic_constant_expr = save_symbolic_constant_expr;

        if (!symbolic_constant_expr) {
           finish_symbolic_expr();
        }
        break;





   case Symbolic_Shiftl_Opr:
        save_symbolic_constant_expr = symbolic_constant_expr;
        symbolic_constant_expr = TRUE;

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx));

        cvrt_exp_to_pdg(IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("npex_shiftl");
        PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
        npex_shiftl();
# endif

        symbolic_constant_expr = save_symbolic_constant_expr;

        if (!symbolic_constant_expr) {
           finish_symbolic_expr();
        }
        break;





   case Symbolic_Max_Opr :
        save_symbolic_constant_expr = symbolic_constant_expr;
        symbolic_constant_expr = TRUE;

        tmp_ir_idx = IR_IDX_L(ir_idx);
        count = 0;

        while (tmp_ir_idx != NULL_IDX) {

           if (IL_IDX(tmp_ir_idx) != NULL_IDX) {
              cvrt_exp_to_pdg(IL_IDX(tmp_ir_idx),
                              IL_FLD(tmp_ir_idx));
              count += 1;
           }
           tmp_ir_idx = IL_NEXT_LIST_IDX(tmp_ir_idx);
        } 


        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("npex_max");
        PDG_DBG_PRINT_END    

# ifdef _TARGET_OS_MAX
# ifdef _ENABLE_FEI
        npex_max();
# endif
# endif

        symbolic_constant_expr = save_symbolic_constant_expr;

        if (!symbolic_constant_expr) {
           finish_symbolic_expr();
        }
        break;





   case Symbolic_Min_Opr :
        save_symbolic_constant_expr = symbolic_constant_expr;
        symbolic_constant_expr = TRUE;

        tmp_ir_idx = IR_IDX_L(ir_idx);
        count = 0;

        while (tmp_ir_idx != NULL_IDX) {

           if (IL_IDX(tmp_ir_idx) != NULL_IDX) {
              cvrt_exp_to_pdg(IL_IDX(tmp_ir_idx),
                              IL_FLD(tmp_ir_idx));
              count += 1;
           }
           tmp_ir_idx = IL_NEXT_LIST_IDX(tmp_ir_idx);
        } 


        PDG_DBG_PRINT_START    
        PDG_DBG_PRINT_C("npex_min");
        PDG_DBG_PRINT_END    

# ifdef _TARGET_OS_MAX
# ifdef _ENABLE_FEI
        npex_min();
# endif
# endif

        symbolic_constant_expr = save_symbolic_constant_expr;

        if (!symbolic_constant_expr) {
           finish_symbolic_expr();
        }
        break;



   case Parallel_Open_Mp_Opr:
   case Do_Open_Mp_Opr:
   case Sections_Open_Mp_Opr:
   case Single_Open_Mp_Opr:
   case Workshare_Open_Mp_Opr:
   case Paralleldo_Open_Mp_Opr:
   case Parallelsections_Open_Mp_Opr:
   case Parallelworkshare_Open_Mp_Opr:
        list_idx1 = IR_IDX_L(ir_idx);

        for (i = 0; i < OPEN_MP_LIST_CNT; i++) {
           list_array[i] = list_idx1;
           list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
        }


        /* process IF clause */
        if (IL_FLD(list_array[OPEN_MP_IF_IDX]) == AT_Tbl_Idx) {
           send_attr_ntry(IL_IDX(list_array[OPEN_MP_IF_IDX]));
           task_if_idx = PDG_AT_IDX(IL_IDX(list_array[OPEN_MP_IF_IDX]));
        }

	/* process NUM_THREADS clause */ /* by jhs, 02/7/20 */
        if(IL_FLD(list_array[OPEN_MP_NUM_THREADS]) == AT_Tbl_Idx){
        	send_attr_ntry(IL_IDX(list_array[OPEN_MP_NUM_THREADS]));
        	task_num_threads_idx = PDG_AT_IDX(IL_IDX(list_array[OPEN_MP_NUM_THREADS]));
        }




        /* process PRIVATE clause */
        if (IL_FLD(list_array[OPEN_MP_PRIVATE_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[OPEN_MP_PRIVATE_IDX]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
                                         PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Omp_Private]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Omp_Private);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
        }



        /* process SHARED clause */
        if (IL_FLD(list_array[OPEN_MP_SHARED_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[OPEN_MP_SHARED_IDX]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
					PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Omp_Shared]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)), 
                                              Context_Omp_Shared);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
        }



        /* process FIRSTPRIVATE clause */
        if (IL_FLD(list_array[OPEN_MP_FIRSTPRIVATE_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[OPEN_MP_FIRSTPRIVATE_IDX]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
                                         PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Omp_Firstprivate]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Omp_Firstprivate);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
        }



        /* process DEFAULT clause */
        if (IL_FLD(list_array[OPEN_MP_DEFAULT_IDX]) == CN_Tbl_Idx) {
           defaultt =(long)CN_INT_TO_C(IL_IDX(list_array[OPEN_MP_DEFAULT_IDX]));
        }




        /* process COPYIN clause */
        if (IL_FLD(list_array[OPEN_MP_COPYIN_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[OPEN_MP_COPYIN_IDX]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", 
                                     PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Omp_Copyin]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Omp_Copyin);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2); 
           }  
        }





        /* process REDUCTION clause */
        if (IL_FLD(list_array[OPEN_MP_REDUCTION_OPR_IDX]) == IL_Tbl_Idx &&
            IL_FLD(list_array[OPEN_MP_REDUCTION_LIST_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[OPEN_MP_REDUCTION_OPR_IDX]);
           list_idx3 = IL_IDX(list_array[OPEN_MP_REDUCTION_LIST_IDX]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == IR_Tbl_Idx &&
                  IL_FLD(list_idx3) == IL_Tbl_Idx) {
                 switch (IR_OPR(IL_IDX(list_idx2))) {
                    case Max_Opr :   
                         context = Context_Omp_Reduction_Max;
                         break;
                    case Min_Opr :   
                         context = Context_Omp_Reduction_Min;
                         break;
                    case Band_Opr :   
                         context = Context_Omp_Reduction_Band;
                         break;
                    case Bor_Opr :   
                         context = Context_Omp_Reduction_Bor;
                         break;
                    case Bneqv_Opr :   
                         context = Context_Omp_Reduction_Bneqv;
                         break;
                    case Plus_Opr :   
                         context = Context_Omp_Reduction_Plus;
                         break;
                    case Mult_Opr :   
                         context = Context_Omp_Reduction_Mult;
                         break;
                    case Minus_Opr :   
                         context = Context_Omp_Reduction_Minus;
                         break;
                    case And_Opr :   
                         context = Context_Omp_Reduction_And;
                         break;
                    case Or_Opr :   
                         context = Context_Omp_Reduction_Or;
                         break;
                    case Eqv_Opr :   
                         context = Context_Omp_Reduction_Eqv;
                         break;
                    case Neqv_Opr :   
                         context = Context_Omp_Reduction_Neqv;
                         break;
                 }
                 list_idx4 = IL_IDX(list_idx3);
                 while (list_idx4) {

                    if (IL_FLD(list_idx4) == AT_Tbl_Idx) {
                       send_attr_ntry(IL_IDX(list_idx4));

                       PDG_DBG_PRINT_START
                       PDG_DBG_PRINT_C("fei_task_var");
                       PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
                                   PDG_AT_IDX(IL_IDX(list_idx4)));
                       PDG_DBG_PRINT_S("(2) context", 
                                   p_tasking_context[context]);
                       PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                       last_task_idx = fei_task_var(
                                                PDG_AT_IDX(IL_IDX(list_idx4)),
                                                context);
# endif
                    }
                    list_idx4 = IL_NEXT_LIST_IDX(list_idx4); 
                 }
              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2); 
              list_idx3 = IL_NEXT_LIST_IDX(list_idx3); 
           }  
        }






        /* process LASTPRIVATE clause */
        if (IL_FLD(list_array[OPEN_MP_LASTPRIVATE_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[OPEN_MP_LASTPRIVATE_IDX]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
                                   PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Omp_Lastprivate]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Omp_Lastprivate);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2); 
           }  
        }




        /* process ORDERED clause */
        if (IL_FLD(list_array[OPEN_MP_ORDERED_IDX]) == CN_Tbl_Idx) {
           ordered = (long)CN_INT_TO_C(IL_IDX(list_array[OPEN_MP_ORDERED_IDX]));
        }



        /* process SCHEDULE (type) clause */
        if (IL_FLD(list_array[OPEN_MP_SCHEDULE_TYPE_IDX]) == CN_Tbl_Idx) {
           scheduletype =  (long)
                CN_INT_TO_C(IL_IDX(list_array[OPEN_MP_SCHEDULE_TYPE_IDX]));
        }



        /* process SCHEDULE (chunk) clause */
        if (IL_FLD(list_array[OPEN_MP_SCHEDULE_CHUNK_IDX]) == AT_Tbl_Idx) {
           send_attr_ntry(IL_IDX(list_array[OPEN_MP_SCHEDULE_CHUNK_IDX]));
           schedulechunk = 
               PDG_AT_IDX(IL_IDX(list_array[OPEN_MP_SCHEDULE_CHUNK_IDX]));
        }

        /* process NEST clause */
        if (IL_FLD(list_array[OPEN_MP_NEST_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[OPEN_MP_NEST_IDX]);

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
                                     PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Omp_Nest]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Omp_Nest);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
        }



        /* process AFFINITY clause */
        if (IL_FLD(list_array[OPEN_MP_AFFINITY_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[OPEN_MP_AFFINITY_IDX]);

           while (list_idx2 != NULL_IDX) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
                                        PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Omp_Affinity]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Omp_Affinity);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }

           cvrt_exp_to_pdg(IL_IDX(list_array[OPEN_MP_THREAD_DATA_IDX]),
                           IL_FLD(list_array[OPEN_MP_THREAD_DATA_IDX]));

           cvrt_exp_to_pdg(IL_IDX(list_array[OPEN_MP_THREAD_DATA_IDX]),
                           IL_FLD(list_array[OPEN_MP_THREAD_DATA_IDX]));

           which = (long)CN_INT_TO_C(IL_IDX(list_array[OPEN_MP_IS_THREAD_IDX]));
           if (which == 1) {
              threadcount = 1;
           }
           else {
              datacount = 1;
           }
        }




        /* process ONTO clause */
        if (IL_FLD(list_array[OPEN_MP_ONTO_IDX]) == IL_Tbl_Idx) {
           list_idx2 = IL_IDX(list_array[OPEN_MP_ONTO_IDX]);

           ontocount = 0;
           while (list_idx2 != NULL_IDX) {
              cvrt_exp_to_pdg(IL_IDX(list_idx2),
                              IL_FLD(list_idx2));

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
              ontocount = ontocount + 1;
           }
        }



        switch (IR_OPR(ir_idx)) {

        case Parallel_Open_Mp_Opr:
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_parallel_open_mp");
           PDG_DBG_PRINT_D("(1) if idx", task_if_idx);
           PDG_DBG_PRINT_D("(2) num_threads idx", task_num_threads_idx); /* by jhs, 02/7/20 */
           PDG_DBG_PRINT_LD("(3) defaultt", defaultt);
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           fei_parallel_open_mp(task_if_idx,
           			    task_num_threads_idx, /* by jhs, 02/7/20 */
                                defaultt);

# endif
           break;



        case Do_Open_Mp_Opr:
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_do_open_mp");
           PDG_DBG_PRINT_LD("(1) ordered", ordered);
           PDG_DBG_PRINT_LD("(2) scheduletype", scheduletype);
           PDG_DBG_PRINT_D("(3) schedulechunk", schedulechunk);
           PDG_DBG_PRINT_LD("(4) threadcount", threadcount);
           PDG_DBG_PRINT_LD("(5) datacount", datacount);
           PDG_DBG_PRINT_LD("(6) ontocount", ontocount);
           PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
           fei_do_open_mp(ordered,
                          scheduletype,
                          schedulechunk,
			  threadcount,
                          datacount,
                          ontocount);
# endif
           break;



        case Sections_Open_Mp_Opr:
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_sections_open_mp");
           PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
           fei_sections_open_mp();
# endif
           break;





        case Single_Open_Mp_Opr:
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_single_open_mp");
           PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
           fei_single_open_mp();
# endif
           break;

        case Workshare_Open_Mp_Opr:
	   PDG_DBG_PRINT_START
	   PDG_DBG_PRINT_C("fei_workshare_open_mp");
	   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
	   fei_workshare_open_mp();
# endif
	   break;

	case Parallelworkshare_Open_Mp_Opr:
	   PDG_DBG_PRINT_START
	   PDG_DBG_PRINT_C("fei_parallelworkshare_open_mp");
	   PDG_DBG_PRINT_D("(1) if idx", task_if_idx);
	   PDG_DBG_PRINT_D("(2) num_threads idx", task_num_threads_idx);
	   PDG_DBG_PRINT_LD("(3) defaultt", defaultt);
	   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
	   fei_parallelworkshare_open_mp(task_if_idx,
				task_num_threads_idx,
				defaultt);
# endif
	   break;

        case Paralleldo_Open_Mp_Opr:
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_paralleldo_open_mp");
           PDG_DBG_PRINT_D("(1) if idx", task_if_idx);
           PDG_DBG_PRINT_D("(2) num_threads idx", task_num_threads_idx); /* by jhs, 02/7/20 */
           PDG_DBG_PRINT_LD("(3) defaultt", defaultt);
           PDG_DBG_PRINT_LD("(4) ordered", ordered);
           PDG_DBG_PRINT_LD("(5) scheduletype", scheduletype);
           PDG_DBG_PRINT_D("(6) schedulechunk", schedulechunk);
           PDG_DBG_PRINT_LD("(7) threadcount", threadcount);
           PDG_DBG_PRINT_LD("(8) datacount", datacount);
           PDG_DBG_PRINT_LD("(9) ontocount", ontocount);
           PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
           fei_paralleldo_open_mp(task_if_idx,
       			      task_num_threads_idx, /* by jhs, 02/7/20 */
                                  defaultt,
                                  ordered,
                                  scheduletype,
                                  schedulechunk,
                                  threadcount,
                                  datacount,
                                  ontocount);
# endif
           break;

        case Parallelsections_Open_Mp_Opr:
           PDG_DBG_PRINT_START
           PDG_DBG_PRINT_C("fei_parallelsections_open_mp");
           PDG_DBG_PRINT_D("(1) if idx", task_if_idx);
           PDG_DBG_PRINT_D("(2) num_threads idx", task_num_threads_idx); /* by jhs, 02/7/20 */
           PDG_DBG_PRINT_LD("(3) defaultt", defaultt);
           PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
           fei_parallelsections_open_mp(task_if_idx,
           				     task_num_threads_idx, /* by jhs, 02/7/20 */
                                        defaultt);
# endif
           break;
        }
        break;


     case Endworkshare_Open_Mp_Opr:
	if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
	   nowait = (long) CN_INT_TO_C(IR_IDX_L(ir_idx));
	}

	PDG_DBG_PRINT_START
	PDG_DBG_PRINT_C("fei_endworkshare_open_mp");
	PDG_DBG_PRINT_LD("(1) nowait", nowait);
	PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
	fei_endworkshare_open_mp(nowait);
# endif
	break;

     case Endparallelworkshare_Open_Mp_Opr:
	PDG_DBG_PRINT_START
	PDG_DBG_PRINT_C("fei_endparallelworkshare_open_mp");
	PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
	fei_endparallelworkshare_open_mp();
# endif
	break;

     case Barrier_Open_Mp_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_barrier_open_mp");
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_barrier_open_mp();
# endif
        break;



     case Master_Open_Mp_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_master_open_mp");
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_master_open_mp();
# endif
        break;



     case Endmaster_Open_Mp_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_endmaster_open_mp");
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_endmaster_open_mp();
# endif
        break;



     case Ordered_Open_Mp_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_ordered_open_mp");
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_ordered_open_mp();
# endif
        break;



     case Endordered_Open_Mp_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_endordered_open_mp");
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_endordered_open_mp();
# endif
        break;



     case Endparalleldo_Open_Mp_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_endparalleldo_open_mp");
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_endparalleldo_open_mp();
# endif
        break;



     case Endparallel_Open_Mp_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_endparallel_open_mp");
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_endparallel_open_mp();
# endif
        break;



     case Endparallelsections_Open_Mp_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_endparallelsections_open_mp");
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_endparallelsections_open_mp();
# endif
        break;



     case Section_Open_Mp_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_section_open_mp");
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_section_open_mp();
# endif
        break;



     case Enddo_Open_Mp_Opr:
        if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
           nowait = (long) CN_INT_TO_C(IR_IDX_L(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_enddo_open_mp");
        PDG_DBG_PRINT_LD("(1) nowait", nowait);
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_enddo_open_mp(nowait);
# endif
        break;



     case Endsections_Open_Mp_Opr:
        if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
           nowait = (long) CN_INT_TO_C(IR_IDX_L(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_endsections_open_mp");
        PDG_DBG_PRINT_LD("(1) nowait", nowait);
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_endsections_open_mp(nowait);
# endif
        break;



     case Endsingle_Open_Mp_Opr:
     	/* the following is added and modified by jhs, 02/7/22 */
        list_idx1 = IR_IDX_L(ir_idx);

	 if(IL_FLD(list_idx1) == CN_Tbl_Idx){ /* NOWAIT clause */
           nowait = (long) CN_INT_TO_C(IL_IDX(list_idx1));
	 }
	 else if(IL_FLD(IL_NEXT_LIST_IDX(list_idx1)) == IL_Tbl_Idx){/* COPYPRIVATE clause */
           list_idx2 = IL_IDX(IL_NEXT_LIST_IDX(list_idx1));

           while (list_idx2) {
              if (IL_FLD(list_idx2) == AT_Tbl_Idx) {
                 send_attr_ntry(IL_IDX(list_idx2));

                 PDG_DBG_PRINT_START
                 PDG_DBG_PRINT_C("fei_task_var");
                 PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
                                         PDG_AT_IDX(IL_IDX(list_idx2)));
                 PDG_DBG_PRINT_S("(2) context",
                                   p_tasking_context[Context_Omp_Copyprivate]);
                 PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                 last_task_idx = fei_task_var(PDG_AT_IDX(IL_IDX(list_idx2)),
                                              Context_Omp_Copyprivate);
# endif

              }

              list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
           }
	 }
     	/* the above is added and modified by jhs, 02/7/22 */
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_endsingle_open_mp");
        PDG_DBG_PRINT_LD("(1) nowait", nowait);
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_endsingle_open_mp(nowait);
# endif
        break;




     case Flush_Open_Mp_Opr:
        list_cnt = 0;
        if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
           list_cnt = IR_LIST_CNT_L(ir_idx);
        }

        cvrt_exp_to_pdg(IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx));

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_flush_open_mp");
        PDG_DBG_PRINT_D("(1) list_cnt", list_cnt);
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_flush_open_mp(list_cnt);
# endif
        break;




     case Critical_Open_Mp_Opr:
        if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
           criticalname = (char *) &CN_CONST(IR_IDX_L(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_critical_open_mp");
        PDG_DBG_PRINT_S("(1) criticalname", criticalname);
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_critical_open_mp(criticalname);
# endif
        break;



     case Atomic_Open_Mp_Opr:
        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_atomic_open_mp");
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_atomic_open_mp();
# endif
        break;




     case Endcritical_Open_Mp_Opr:
        if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
           criticalname = (char *) &CN_CONST(IR_IDX_L(ir_idx));
        }

        PDG_DBG_PRINT_START
        PDG_DBG_PRINT_C("fei_endcritical_open_mp");
        PDG_DBG_PRINT_S("(1) criticalname", criticalname);
        PDG_DBG_PRINT_END


# ifdef _ENABLE_FEI
        fei_endcritical_open_mp(criticalname);
# endif
        break;

     }
  }

    
TRACE (Func_Exit, "cvrt_exp_to_pdg", NULL);

return;

}  /* cvrt_exp_to_pdg */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/
static	void	finish_symbolic_expr()
{
   int		npex_expr_idx;


   TRACE (Func_Entry, "finish_symbolic_expr", NULL);

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("npex_end");
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
   npex_expr_idx = npex_end();
# endif

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_D("(r) npex_expr_idx", npex_expr_idx);
   PDG_DBG_PRINT_END

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("npex_to_expr");
   PDG_DBG_PRINT_D("npex_expr_idx", npex_expr_idx);
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
   npex_to_expr(npex_expr_idx);
# endif

   TRACE (Func_Exit, "finish_symbolic_expr", NULL);

   return;

}  /* finish_symbolic_expr */


/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static void	cvrt_ir_to_pdg(int	scp_idx)
{
   long		flag;


   TRACE (Func_Entry, "cvrt_ir_to_pdg", NULL);

   curr_sh = SCP_FIRST_SH_IDX(scp_idx);

   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_C("fei_proc_body");
   PDG_DBG_PRINT_D("(1) SH_GLB_LINE", SH_GLB_LINE(curr_sh));
   PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
   fei_proc_body(SH_GLB_LINE(curr_sh));
# endif

   while (curr_sh != NULL_IDX) {

      flag = ((long) (SH_STMT_TYPE(curr_sh) == Automatic_Base_Calc_Stmt) 
                          		      << FEI_STMT_ARY_BASE);

      flag = flag | ((long) (SH_STMT_TYPE(curr_sh) == Automatic_Base_Size_Stmt) 
                          		      << FEI_STMT_ARY_SIZE);

      flag = flag | ((long) SH_COMPILER_GEN(curr_sh) << FEI_STMT_INTERNAL);

      PDG_DBG_PRINT_START    
      PDG_DBG_PRINT_C("fei_stmt");
      PDG_DBG_PRINT_D("(1) SH_GLB_LINE", SH_GLB_LINE(curr_sh));
      PDG_DBG_PRINT_LO("(2) flags", flag);
      PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
      fei_stmt(SH_GLB_LINE(curr_sh), flag);
# endif

      if (SH_IR_IDX(curr_sh) != NULL_IDX) {
         stmt_start_line = SH_GLB_LINE(curr_sh);
         cvrt_exp_to_pdg(SH_IR_IDX(curr_sh), IR_Tbl_Idx);
      }

      curr_sh = SH_NEXT_IDX(curr_sh);
   }

   TRACE (Func_Exit, "cvrt_ir_to_pdg", NULL);

   return;

}  /* cvrt_ir_to_pdg */



/******************************************************************************\
|*									      *|
|* Description:								      *|
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

static TYPE get_basic_type(int	type_idx,
			   int	alignment,
			   int	input_attr_idx)

{
   int		align;
   long64	bit_size;
   INTPTR	cdx;
   int      	flags		= 0;
   TYPE		idx;

# if !defined(_ALTERNATIVE_INTERFACE_FOR_POINTEES)
   int		attr_idx;
   int		p_idx;
   int		save_pdg_at_idx;
# endif


   TRACE (Func_Entry, "get_basic_type", NULL);

   /* The concept of alignment is only implmented for IRIX platforms     */
   /* Compare the alignment being passed in with the default alignment   */
   /* for the type.  If the alignment coming in is less than the default */
   /* alignment, call fei_descriptor with the smaller alignment.         */

   idx = null_type;

   if (input_attr_idx != NULL_IDX && ATD_VOLATILE(input_attr_idx)) {
      flags = flags | (1 << FEI_DESCRIPTOR_VOLAT_C);
   }

   switch (TYP_TYPE(type_idx)) {

   case Typeless:
      align = pdg_align[type_alignment_tbl[Long_Typeless]];

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_descriptor");
      PDG_DBG_PRINT_O("(1) flags", flags);
      PDG_DBG_PRINT_S("(2) table type", p_table_type[Basic]);
      PDG_DBG_PRINT_LLD("(3) bit len", TYP_BIT_LEN(type_idx));
      PDG_DBG_PRINT_S("(4) basic type", p_basic_type[T_ypeless]);
      PDG_DBG_PRINT_D("(5) aux info", 0);
      PDG_DBG_PRINT_D("(6) alignment", align);
      PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
      idx = fei_descriptor(flags,
                           Basic,
                           TYP_BIT_LEN(type_idx),  /* JEFFL bit_len is 64 now */
                           T_ypeless,
                           0, 
                           align);
# endif
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_T("(r) type", idx);
      PDG_DBG_PRINT_END
      break;



   case CRI_Parcel_Ptr:
      if (global_attr_idx == NULL_IDX) {
         PRINTMSG(1, 993, Internal, 1);
      }

# ifdef _ALTERNATIVE_INTERFACE_FOR_POINTEES     
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_descriptor");
      PDG_DBG_PRINT_O("(1) flags", flags);
      PDG_DBG_PRINT_S("(2) table type", p_table_type[Func_tion]);
      PDG_DBG_PRINT_LD("(3) PDG_AT_IDX", PDG_AT_IDX(global_attr_idx));
      PDG_DBG_PRINT_S("(4) basic type", p_basic_type[BT_func_ptr]);
      PDG_DBG_PRINT_D("(5) aux info", 0);
      PDG_DBG_PRINT_D("(6) alignment", 0);
      PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
      idx = fei_descriptor(flags,
                           Func_tion,
                           PDG_AT_IDX(global_attr_idx),
                           BT_func_ptr,
                           0, 
                           0);
# endif
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_T("(r) type", idx);
      PDG_DBG_PRINT_END

# else

      attr_idx = global_attr_idx;
      save_pdg_at_idx = PDG_AT_IDX(attr_idx);
      PDG_AT_IDX(attr_idx) = NULL_IDX;

      send_procedure(attr_idx,
                     NULL_IDX,             /* No alt entries or parent procs */
                     Definition);

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_pointee");
      PDG_DBG_PRINT_T("(1) type", pdg_type_void);
      PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
      p_idx = fei_pointee(pdg_type_void);
# endif
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_D("(r) p_idx", p_idx);
      PDG_DBG_PRINT_END



      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_descriptor");
      PDG_DBG_PRINT_O("(1) flags", flags);
      PDG_DBG_PRINT_S("(2) table type", p_table_type[Func_tion]);
      PDG_DBG_PRINT_D("(3) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
      PDG_DBG_PRINT_S("(4) basic type", p_basic_type[V_oid]);
      PDG_DBG_PRINT_D("(5) aux info", p_idx);
      PDG_DBG_PRINT_D("(6) alignment", 0);
      PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
      idx = fei_descriptor(flags,   
                           Func_tion,
                           PDG_AT_IDX(attr_idx),
                           V_oid,
                           p_idx, 
                           0);
# endif
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_T("(r) type", idx);
      PDG_DBG_PRINT_END




      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_pointee");
      PDG_DBG_PRINT_T("(1) idx", idx);
      PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
      p_idx = fei_pointee(idx);
# endif
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_D("(r) p_idx", p_idx);
      PDG_DBG_PRINT_END




      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_descriptor");
      PDG_DBG_PRINT_O("(1) flags", flags);
      PDG_DBG_PRINT_S("(2) table type", p_table_type[Pointer]);
      PDG_DBG_PRINT_D("(3) p_idx", p_idx);
      PDG_DBG_PRINT_S("(4) basic type", p_basic_type[V_oid]);
      PDG_DBG_PRINT_D("(5) aux info", 0);
      PDG_DBG_PRINT_D("(6) alignment", 0);
      PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
      idx = fei_descriptor(flags,
                           Pointer,
                           p_idx,
                           V_oid,
                           0, 
                           0);
# endif
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_T("(r) type", idx);
      PDG_DBG_PRINT_END

      PDG_AT_IDX(attr_idx) = save_pdg_at_idx;
# endif
      break;



   case Integer:
   case Logical:
   case Real:
   case Complex:
   case CRI_Ch_Ptr:
      idx = send_non_standard_aligned_type(alignment, type_idx);
      break;


   case Character:
      switch (TYP_CHAR_CLASS(type_idx)) {
      case Assumed_Size_Char:    
      case Var_Len_Char:     
         if (global_attr_idx != NULL_IDX) {
            if (ATD_CLASS(global_attr_idx) == CRI__Pointee &&
                ATD_PTR_IDX(global_attr_idx) != NULL_IDX) {
               global_attr_idx = ATD_PTR_IDX(global_attr_idx);
            }

            if (ATD_STOR_BLK_IDX(global_attr_idx) != NULL_IDX &&
                SB_SCP_IDX(ATD_STOR_BLK_IDX(global_attr_idx)) != curr_scp_idx) {
               flags = flags | (1 << FEI_DESCRIPTOR_HOSTED_TYPE);
            }
         }

         send_attr_ntry(TYP_IDX(type_idx));
         flags = flags | (1 << FEI_DESCRIPTOR_AUTO_F);

         PDG_DBG_PRINT_START
         PDG_DBG_PRINT_C("fei_descriptor");
         PDG_DBG_PRINT_O("(1) flags", flags);
         PDG_DBG_PRINT_S("(2) table type", p_table_type[Basic]);
         PDG_DBG_PRINT_LD("(3) PDG_AT_IDX", PDG_AT_IDX(TYP_IDX(type_idx)));
         PDG_DBG_PRINT_S("(4) basic type", p_basic_type[Char_Fortran]);
         PDG_DBG_PRINT_D("(5) aux info", CHAR_BIT);
         PDG_DBG_PRINT_D("(6) alignment", Align_Bit);
         PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
         idx = fei_descriptor(flags,
                              Basic,
                              PDG_AT_IDX(TYP_IDX(type_idx)),
                              Char_Fortran,
                              CHAR_BIT, 
                              Align_Bit);
# endif
         PDG_DBG_PRINT_START
         PDG_DBG_PRINT_T("(r) type", idx);
         PDG_DBG_PRINT_END
         break;


      case Const_Len_Char:     
         if (global_attr_idx != NULL_IDX) {
            if (ATD_CLASS(global_attr_idx) == CRI__Pointee &&
                ATD_PTR_IDX(global_attr_idx) != NULL_IDX) {
               global_attr_idx = ATD_PTR_IDX(global_attr_idx);
            }

            if (ATD_STOR_BLK_IDX(global_attr_idx) != NULL_IDX &&
                SB_SCP_IDX(ATD_STOR_BLK_IDX(global_attr_idx)) != curr_scp_idx) {
               flags = flags | (1 << FEI_DESCRIPTOR_HOSTED_TYPE);
            }
         }
         bit_size = CN_INT_TO_C(TYP_IDX(type_idx));
         bit_size = bit_size * CHAR_BIT;

         idx = get_basic_type(Integer_8,type_alignment_tbl[Integer_8],NULL_IDX);

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_arith_con");
         PDG_DBG_PRINT_T("(1) type", idx);
         PDG_DBG_PRINT_LLD("(2) bit_size", bit_size);
         PDG_DBG_PRINT_END    
# ifdef _ENABLE_FEI
         cdx = fei_arith_con(idx, (long *)&bit_size);
# endif
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_LD("(r) PDG_CN_IDX", cdx);
         PDG_DBG_PRINT_END    


         PDG_DBG_PRINT_START
         PDG_DBG_PRINT_C("fei_descriptor");
         PDG_DBG_PRINT_O("(1) flags", flags);
         PDG_DBG_PRINT_S("(2) table type", p_table_type[Basic]);
         PDG_DBG_PRINT_LD("(3) PDG_CN_IDX", cdx);
         PDG_DBG_PRINT_S("(4) basic type", p_basic_type[Char_Fortran]);
         PDG_DBG_PRINT_D("(5) aux info", CHAR_BIT);
         PDG_DBG_PRINT_D("(6) alignment", Align_Bit);
         PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
         idx = fei_descriptor(flags,
                              Basic,
                              cdx,
                              Char_Fortran,
                              CHAR_BIT, 
                              Align_Bit);
# endif
         PDG_DBG_PRINT_START
         PDG_DBG_PRINT_T("(r) type", idx);
         PDG_DBG_PRINT_END
         break;
      }
      break;


   case Structure:
      idx = send_derived_type(type_idx);
      break;


   case CRI_Ptr:
# ifdef _TARGET32
      if (TYP_PTR_INCREMENT(type_idx) > TARGET_BITS_PER_WORD) {
         PDG_DBG_PRINT_START
         PDG_DBG_PRINT_C("fei_descriptor");
         PDG_DBG_PRINT_O("(1) flags", flags);
         PDG_DBG_PRINT_S("(2) table type", p_table_type[Basic]);
         PDG_DBG_PRINT_D("(3) bit_size", storage_bit_size_tbl[CRI_Ptr_8]);
         PDG_DBG_PRINT_S("(4) basic type", p_basic_type[CRI_Pointer]);
         PDG_DBG_PRINT_D("(5) aux info", DWord_Align);
         PDG_DBG_PRINT_D("(6) alignment", 0);
         PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
         idx = fei_descriptor(flags,
                              Basic,
                              storage_bit_size_tbl[CRI_Ptr_8],
                              CRI_Pointer,
                              DWord_Align, 
                              0);
# endif
         PDG_DBG_PRINT_START
         PDG_DBG_PRINT_T("(r) type", idx);
         PDG_DBG_PRINT_END
      }
      else 
# endif

      if (TYP_PTR_INCREMENT(type_idx) >= TARGET_BITS_PER_WORD) {
         PDG_DBG_PRINT_START
         PDG_DBG_PRINT_C("fei_descriptor");
         PDG_DBG_PRINT_O("(1) flags", flags);
         PDG_DBG_PRINT_S("(2) table type", p_table_type[Basic]);
         PDG_DBG_PRINT_D("(3) unused", 0);
         PDG_DBG_PRINT_S("(4) basic type", p_basic_type[CRI_Pointer]);
         PDG_DBG_PRINT_D("(5) aux info", Word_Align);
         PDG_DBG_PRINT_D("(6) alignment", 0);
         PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
         idx = fei_descriptor(flags,
                              Basic,
                              0,       
                              CRI_Pointer,
                              Word_Align, 
                              0);
# endif
         PDG_DBG_PRINT_START
         PDG_DBG_PRINT_T("(r) type", idx);
         PDG_DBG_PRINT_END
      }
      else {
         PDG_DBG_PRINT_START
         PDG_DBG_PRINT_C("fei_descriptor");
         PDG_DBG_PRINT_O("(1) flags", flags);
         PDG_DBG_PRINT_S("(2) table type", p_table_type[Basic]);
         PDG_DBG_PRINT_D("(3) unused", 0);
         PDG_DBG_PRINT_S("(4) basic type", p_basic_type[CRI_Pointer]);
         PDG_DBG_PRINT_D("(5) aux info", HWord_Align);
         PDG_DBG_PRINT_D("(6) alignment", 0);
         PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
         idx = fei_descriptor(flags,
                              Basic,
                              0,       
                              CRI_Pointer,
                              HWord_Align,
                              0);
# endif
         PDG_DBG_PRINT_START
         PDG_DBG_PRINT_T("(r) type", idx);
         PDG_DBG_PRINT_END
      }
      break;

   }

   TRACE (Func_Exit, "get_basic_type", NULL);

   return(idx);

}  /* get_basic_type */


/******************************************************************************\
|*									      *|
|* Description:								      *|
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

static TYPE get_type_desc(int	input_idx)
{
   int			bd_idx;
   int			dist_idx;
   int			distribution	= 0;
   int			kind_type	= 0;
#ifdef KEY /* Bug 10177 */
   int			attr_idx = 0;
#else /* KEY Bug 10177 */
   int			attr_idx;
#endif /* KEY Bug 10177 */
   int			temp_attr_idx;
   int          	basic_type;
   long64		extent;
   int			i		= 1;
   long64		lbound;
   long64		ubound;
   int      		flag 		= 0;
   boolean		lb_vble;
   boolean		lb_symcon	= FALSE;
   boolean              ub_vble;
   boolean              ub_symcon       = FALSE;
   boolean		xt_vble;
   boolean		xt_symcon	= FALSE;
   long			pdg_array_idx;
   int			rank;
   size_offset_type	size;
   long64		span;
   boolean		template_tmp	= FALSE;
   int			tmp_idx;
   int      		type_flag	= 0;
   TYPE			type_idx;

# if defined(_TARGET_OS_MAX)
   int			pe_bd_idx;
# endif


   TRACE (Func_Entry, "get_type_desc", NULL);

   type_idx = null_type;

   switch (AT_OBJ_CLASS(input_idx)) {

   case Data_Obj:
      attr_idx = input_idx;
      global_attr_idx = attr_idx;
      type_idx = get_basic_type(ATD_TYPE_IDX(attr_idx),
                                ATD_ALIGNMENT(attr_idx),
                                attr_idx);
      global_attr_idx = NULL_IDX;
      break;

   case Pgm_Unit:
      if (ATP_PGM_UNIT(input_idx) == Function) {
         attr_idx = ATP_RSLT_IDX(input_idx);

         /* If this is an array or character, the tmps are just templates.   */
         /* They are never defined or referenced in the IR.  We need to send */
         /* over the referenced tmps to PDGCS.  Put them in the current      */
         /* stack block so that they do not end up causing host association. */
         /* Or in the case of functions defined in interface blocks, they    */
         /* do not have valid storage.  Have to restore storage, in case     */
         /* this is an internal or module procedure.  The storage must be    */
         /* correct when that routine is sent across.                        */

         if (ATP_EXPL_ITRFC(input_idx) && !ATP_SCP_ALIVE(input_idx)) {
            template_tmp = TRUE;

            if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character &&
                TYP_CHAR_CLASS(ATD_TYPE_IDX(attr_idx)) != Const_Len_Char &&
                PDG_AT_IDX(TYP_IDX(ATD_TYPE_IDX(attr_idx))) == NULL_IDX) {
               tmp_idx = TYP_IDX(ATD_TYPE_IDX(attr_idx));
               COPY_ATTR_NTRY(AT_WORK_IDX, tmp_idx);
               AT_REFERENCED(tmp_idx) = Referenced;
               ATD_OFFSET_ASSIGNED(tmp_idx) = FALSE;
               ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
               send_attr_ntry(tmp_idx);
               COPY_ATTR_NTRY(tmp_idx, AT_WORK_IDX);
            }
         }

         type_idx = get_basic_type(ATD_TYPE_IDX(attr_idx),
                                   ATD_ALIGNMENT(attr_idx),
                                   attr_idx);
      }
      else {
         type_idx = pdg_type_void;
         goto EXIT;
      }
      break;
   }


   if (ATD_IM_A_DOPE(attr_idx)) { 
      rank = (ATD_ARRAY_IDX(attr_idx) == NULL_IDX) ? 0 :
                                         BD_RANK(ATD_ARRAY_IDX(attr_idx));

      if (ATD_STOR_BLK_IDX(attr_idx) != NULL_IDX &&
          SB_SCP_IDX(ATD_STOR_BLK_IDX(attr_idx)) != curr_scp_idx) {
         type_flag = type_flag | (1 << FEI_DOPE_VECTOR_HOSTED_TYPE);
      }

      type_flag = type_flag | 
                    (ATD_POINTER(attr_idx) << FEI_DOPE_VECTOR_POINTER);

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_dope_vector");
      PDG_DBG_PRINT_D("(1) rank", rank);
      PDG_DBG_PRINT_T("(2) type", type_idx);
      PDG_DBG_PRINT_O("(3) flags", type_flag);
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      type_idx = fei_dope_vector(rank, type_idx, type_flag,
#ifdef KEY /* Bug 6845 */
      /* Sigh. We have to count them yet again. The joys of retrofitting. */
        do_count_allocatable_cpnt(attr_idx, rank)
#endif /* KEY Bug 6845 */
        );
# endif
   }
   else if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
      bd_idx = ATD_ARRAY_IDX(attr_idx);
      dist_idx = ATD_DISTRIBUTION_IDX(attr_idx);
      basic_type = pdg_basic_type[TYP_TYPE(ATD_TYPE_IDX(attr_idx))];

      switch (TYP_LINEAR(ATD_TYPE_IDX(attr_idx))) {
      case Integer_1:
      case Logical_1:
         kind_type = 1;
         break;

      case Integer_2:
      case Logical_2:
         kind_type = 2;
         break;

      case Integer_4:
      case Logical_4:
      case Complex_4:
      case Real_4:
         kind_type = 4;
         break;

      case Integer_8:
      case Logical_8:
      case Complex_8:
      case Real_8:
         kind_type = 8;
         break;

      case Complex_16:
      case Real_16:
         kind_type = 16;
         break;
      }

      temp_attr_idx = attr_idx;

      if (ATD_CLASS(attr_idx) == CRI__Pointee &&
          ATD_PTR_IDX(attr_idx) != NULL_IDX) {
         temp_attr_idx = ATD_PTR_IDX(attr_idx);
      }
      else if (ATD_AUTOMATIC(attr_idx) &&
               ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX) {
         temp_attr_idx = ATD_AUTO_BASE_IDX(attr_idx);
      }

      if (ATD_STOR_BLK_IDX(temp_attr_idx) != NULL_IDX &&
          SB_SCP_IDX(ATD_STOR_BLK_IDX(temp_attr_idx)) != curr_scp_idx) {
         type_flag = type_flag | (1 << FEI_DESCRIPTOR_HOSTED_TYPE);
      }

      size = stor_bit_size_of(attr_idx, FALSE, FALSE);

      if (size.fld == CN_Tbl_Idx) {
         span = CN_INT_TO_C(size.idx);
      }
      else {
         span = F_INT_TO_C(size.constant, TYP_LINEAR(size.type_idx));
      }

# ifdef _DEBUG
      if (! BD_RESOLVED(bd_idx)) {
         PRINTMSG(1, 626, Internal, 1,
                  "resolved bounds entry",
                  "get_type_desc");
      }
# endif

      for (i = 1; i <= BD_RANK(bd_idx); i++) {
         if (BD_LB_FLD(bd_idx, i) == CN_Tbl_Idx) {
            lbound = CN_INT_TO_C(BD_LB_IDX(bd_idx, i));
            lb_vble = FALSE;
         }
         else {
            lbound = BD_LB_IDX(bd_idx, i);
            lb_vble = TRUE;

# ifdef _TARGET_OS_MAX
            lb_symcon = ATD_SYMBOLIC_CONSTANT(lbound);
            lb_vble = !ATD_SYMBOLIC_CONSTANT(lbound);
# endif

            if (PDG_AT_IDX(lbound) == NULL_IDX) {

               if (template_tmp) {
                  COPY_ATTR_NTRY(AT_WORK_IDX, lbound);
                  AT_REFERENCED(lbound)	= Referenced;
                  ATD_OFFSET_ASSIGNED(lbound) = FALSE;
                  ATD_STOR_BLK_IDX(lbound) = SCP_SB_STACK_IDX(curr_scp_idx);
                  send_attr_ntry(lbound);
                  COPY_ATTR_NTRY(lbound, AT_WORK_IDX);
               }
               else {
                  send_attr_ntry(lbound);
               }
            }
            lbound = PDG_AT_IDX(lbound);
         }

         if (BD_UB_FLD(bd_idx, i) == CN_Tbl_Idx) {
            ubound = CN_INT_TO_C(BD_UB_IDX(bd_idx, i));
            ub_vble = FALSE;
         }
         else {
            ubound = BD_UB_IDX(bd_idx, i);
            ub_vble = TRUE;

# ifdef _TARGET_OS_MAX
            ub_symcon = ATD_SYMBOLIC_CONSTANT(ubound);
            ub_vble = !ATD_SYMBOLIC_CONSTANT(ubound);
# endif

            if (PDG_AT_IDX(ubound) == NULL_IDX) {

               if (template_tmp) {
                  COPY_ATTR_NTRY(AT_WORK_IDX, ubound);
                  AT_REFERENCED(ubound) = Referenced;
                  ATD_OFFSET_ASSIGNED(ubound) = FALSE;
                  ATD_STOR_BLK_IDX(ubound) = SCP_SB_STACK_IDX(curr_scp_idx);
                  send_attr_ntry(ubound);
                  COPY_ATTR_NTRY(ubound, AT_WORK_IDX);
               }
               else {
                  send_attr_ntry(ubound);
               }
            }
            ubound = PDG_AT_IDX(ubound);
         }

         if (BD_XT_FLD(bd_idx, i) == CN_Tbl_Idx) {
            extent = CN_INT_TO_C(BD_XT_IDX(bd_idx, i));
            xt_vble = FALSE;
            span = extent * span;
         }
         else {
            extent = BD_XT_IDX(bd_idx, i);
            xt_vble = TRUE;

# ifdef _TARGET_OS_MAX
            xt_symcon = ATD_SYMBOLIC_CONSTANT(extent);
            xt_vble = !ATD_SYMBOLIC_CONSTANT(extent);
# endif

            if (PDG_AT_IDX(extent) == NULL_IDX) {

               if (template_tmp) {
                  COPY_ATTR_NTRY(AT_WORK_IDX, extent);
                  AT_REFERENCED(extent)	= Referenced;
                  ATD_OFFSET_ASSIGNED(extent) = FALSE;
                  ATD_STOR_BLK_IDX(extent) = SCP_SB_STACK_IDX(curr_scp_idx);
                  send_attr_ntry(extent);
                  COPY_ATTR_NTRY(extent, AT_WORK_IDX);
               }
               else {
                  send_attr_ntry(extent);
               }
            }
            extent = PDG_AT_IDX(extent);
            span = (long64) 0;
         }

         flag = (lb_symcon << FEI_ARRAY_DIMEN_SCON_LB)|
                (lb_vble   << FEI_ARRAY_DIMEN_VARY_LB)|
                (ub_symcon << FEI_ARRAY_DIMEN_SCON_UB)|
                (ub_vble   << FEI_ARRAY_DIMEN_VARY_UB)|
                (xt_symcon << FEI_ARRAY_DIMEN_SCON_EXT)|
                (xt_vble   << FEI_ARRAY_DIMEN_VARY_EXT);

         flag |= (BD_FLOW_DEPENDENT(bd_idx) << FEI_ARRAY_DIMEN_FLOW_DEPENDENT);

         temp_attr_idx = attr_idx;
         if (ATD_CLASS(attr_idx) == CRI__Pointee &&
             ATD_PTR_IDX(attr_idx) != NULL_IDX) {
            temp_attr_idx = ATD_PTR_IDX(attr_idx);
         }

         if (ATD_STOR_BLK_IDX(temp_attr_idx) != NULL_IDX &&
             SB_SCP_IDX(ATD_STOR_BLK_IDX(temp_attr_idx)) != curr_scp_idx) {
            flag = flag | (1 << FEI_ARRAY_DIMEN_HOSTED_TYPE);
         }

         if (dist_idx != NULL_IDX) {
            if (BD_CYCLIC_IDX(dist_idx, i) != NULL_IDX) { 
               cvrt_exp_to_pdg(BD_CYCLIC_IDX(dist_idx, i),
                               BD_CYCLIC_FLD(dist_idx, i));
               flag = flag | (1 << FEI_ARRAY_DIMEN_DIST_EXPR);
            }

            if (BD_ONTO_IDX(dist_idx, i) != NULL_IDX) { 
               cvrt_exp_to_pdg(BD_ONTO_IDX(dist_idx, i),
                               BD_ONTO_FLD(dist_idx, i));
               flag = flag | (1 << FEI_ARRAY_DIMEN_ONTO_EXPR);
            }

            distribution = BD_DISTRIBUTION(dist_idx, i);
            
            flag = flag | (BD_DISTRIBUTE_RESHAPE(dist_idx) << 
                                   FEI_ARRAY_DIMEN_DIST_RESHAPE);
         }

         PDG_DBG_PRINT_START
         PDG_DBG_PRINT_C("fei_array_dimen");
         PDG_DBG_PRINT_O("(1) flags", flag);
         PDG_DBG_PRINT_LLD("(2) lbound", lbound);
         PDG_DBG_PRINT_LLD("(3) extent", extent);
         PDG_DBG_PRINT_D("(4) rank", i);
         PDG_DBG_PRINT_T("(5) type", type_idx);
         PDG_DBG_PRINT_LLD("(6) span", span);
         PDG_DBG_PRINT_S("(7) distribution", p_distribution[distribution]);
         PDG_DBG_PRINT_LLD("(8) ubound", ubound);
         PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
         pdg_array_idx = fei_array_dimen(flag,
                                         lbound,
                                         extent,
                                         i,
                                         type_idx,
                                         span,
                                         distribution,
                                         ubound);
# endif

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         if (i == BD_RANK(bd_idx)) {  
# endif
            PDG_DBG_PRINT_START
            PDG_DBG_PRINT_C("fei_descriptor");
            PDG_DBG_PRINT_O("(1) flags", type_flag);
            PDG_DBG_PRINT_S("(2) table type", p_table_type[Array]);
            PDG_DBG_PRINT_LD("(3) pdg_array_idx", pdg_array_idx);
            PDG_DBG_PRINT_S("(4) basic type", p_basic_type[basic_type]);
            PDG_DBG_PRINT_D("(5) aux info", kind_type);
            PDG_DBG_PRINT_D("(6) alignment",pdg_align[ATD_ALIGNMENT(attr_idx)]);
            PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
            type_idx = fei_descriptor(type_flag,
                                      Array,
                                      pdg_array_idx,
                                      basic_type,
                                      kind_type, 
                                      pdg_align[ATD_ALIGNMENT(attr_idx)]);
# endif
            PDG_DBG_PRINT_START
            PDG_DBG_PRINT_T("(r) type", type_idx);
            PDG_DBG_PRINT_END

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         }
# endif
      } 
   }

# if defined(_F_MINUS_MINUS)
# if defined(_TARGET_OS_MAX)
   pe_bd_idx = ATD_PE_ARRAY_IDX(attr_idx);

   if (pe_bd_idx &&
       ! ATD_ALLOCATABLE(attr_idx)) {

      dist_idx = NULL_IDX;
      basic_type = pdg_basic_type[TYP_TYPE(ATD_TYPE_IDX(attr_idx))];

      switch (TYP_LINEAR(ATD_TYPE_IDX(attr_idx))) {
      case Integer_1:
      case Logical_1:
         kind_type = 1;
         break;

      case Integer_2:
      case Logical_2:
         kind_type = 2;
         break;

      case Integer_4:
      case Logical_4:
      case Complex_4:
      case Real_4:
         kind_type = 4;
         break;

      case Integer_8:
      case Logical_8:
      case Complex_8:
      case Real_8:
         kind_type = 8;
         break;

      case Complex_16:
      case Real_16:
         kind_type = 16;
         break;
      }

      lbound = (long64) 1;
      span = (long64) 0;

      if (BD_LEN_FLD(pe_bd_idx) == CN_Tbl_Idx) {
         extent = CN_INT_TO_C(BD_LEN_IDX(pe_bd_idx));
         xt_vble = FALSE;
      }
      else {
         extent = BD_LEN_IDX(pe_bd_idx);
         xt_vble = TRUE;

# ifdef _TARGET_OS_MAX
         xt_symcon = ATD_SYMBOLIC_CONSTANT(extent);
         xt_vble = !ATD_SYMBOLIC_CONSTANT(extent);
# endif

         if (PDG_AT_IDX(extent) == NULL_IDX) {
            send_attr_ntry(extent);
         }
         extent = PDG_AT_IDX(extent);
      }

      flag = (xt_symcon << FEI_ARRAY_DIMEN_SCON_EXT)|
             (xt_vble   << FEI_ARRAY_DIMEN_VARY_EXT);


      flag |= 1 << FEI_ARRAY_DIMEN_F_MINUS_MINUS_REMOTE;

      flag |= (BD_FLOW_DEPENDENT(pe_bd_idx) << FEI_ARRAY_DIMEN_FLOW_DEPENDENT);

      temp_attr_idx = attr_idx;
      if (ATD_CLASS(attr_idx) == CRI__Pointee &&
          ATD_PTR_IDX(attr_idx) != NULL_IDX) {
         temp_attr_idx = ATD_PTR_IDX(attr_idx);
      }

      if (ATD_STOR_BLK_IDX(temp_attr_idx) != NULL_IDX &&
          SB_SCP_IDX(ATD_STOR_BLK_IDX(temp_attr_idx)) != curr_scp_idx) {
         flag = flag | (1 << FEI_ARRAY_DIMEN_HOSTED_TYPE);
      }

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_array_dimen");
      PDG_DBG_PRINT_O("(1) flags", flag);
      PDG_DBG_PRINT_LLD("(2) lbound", lbound);
      PDG_DBG_PRINT_LLD("(3) extent", extent);
      PDG_DBG_PRINT_D("(4) rank", i);
      PDG_DBG_PRINT_T("(5) type", type_idx);
      PDG_DBG_PRINT_LLD("(6) span", span);
      PDG_DBG_PRINT_S("(7) distribution", p_distribution[distribution]);
      PDG_DBG_PRINT_LLD("(8) ubound", ubound);
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      pdg_array_idx = fei_array_dimen(flag,
                                      lbound,
                                      extent,
                                      i,
                                      type_idx,
                                      span,
                                      distribution,
                                      ubound);
# endif

      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_descriptor");
      PDG_DBG_PRINT_O("(1) flags", type_flag);
      PDG_DBG_PRINT_S("(2) table type", p_table_type[Array]);
      PDG_DBG_PRINT_D("(3) pdg_array_idx", pdg_array_idx);
      PDG_DBG_PRINT_S("(4) basic type", p_basic_type[basic_type]);
      PDG_DBG_PRINT_D("(5) aux info", kind_type);
      PDG_DBG_PRINT_D("(6) alignment", pdg_align[ATD_ALIGNMENT(attr_idx)]);
      PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
      type_idx = fei_descriptor(type_flag,
                                Array,
                                pdg_array_idx,
                                basic_type,
                                kind_type, 
                                pdg_align[ATD_ALIGNMENT(attr_idx)]);
# endif
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_T("(r) type", type_idx);
      PDG_DBG_PRINT_END
   }
# endif
# endif

EXIT:
#ifdef KEY /* Bug 14110 */
   /* The "volatile" member inside the TYPE variable "type_idx" seems not
    * to affect the WHIRL symbol table, but the "volatile" bit of the
    * "table_index" member does. The original SGI code in this file and
    * cwh_types.cxx attempts to set the "volatile" member (although
    * it doesn't consistently do so); we have left that alone, neither
    * removing the existing code nor attempting to make it consistent.
    * The following suffices to make the variable volatile in every case
    * I've found. */
   if (AT_OBJ_CLASS(input_idx) == Data_Obj && ATD_VOLATILE(input_idx)) {
      type_idx.table_index = fei_set_volatile(type_idx.table_index);
   }
#endif /* KEY Bug 14110 */

   TRACE (Func_Exit, "get_type_desc", NULL);

   return(type_idx);

}  /* get_type_desc */


/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static void send_stor_blk(int	 sb_idx,
			  int	*pdg_sym_class)

{
   long64	 blk_len	= (long64) 0;
   int      	 flag;
   int		 host_sb_idx	= NULL_IDX;
   int		 local_sb_idx;
   char		*name_ptr;
   int           parent_idx;
   sb_type_type	 sb_type;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int		 i;
   char		 new_name[256];
# endif


   TRACE (Func_Entry, "send_stor_blk", NULL);

   if (sb_idx == NULL_IDX) {
      goto EXIT;
   }

   sb_type = SB_BLK_TYPE(sb_idx);

   flag	= (SB_MODULE(sb_idx) 		<< FEI_SEG_MODULE) |
          (SB_SAVED(sb_idx) 		<< FEI_SEG_SAVED) |
          (SB_EQUIVALENCED(sb_idx) 	<< FEI_SEG_EQUIVALENCED) |
          (SB_DUPLICATE_COMMON(sb_idx) 	<< FEI_SEG_DUPLICATE) |
          (SB_VOLATILE(sb_idx)		<< FEI_SEG_VOLATILE) |
          (SB_SYMMETRIC(sb_idx) 	<< FEI_SEG_SHARED);


   if (sb_type == Threadprivate) {
      sb_type = Common;
      flag |= 1 << FEI_SEG_THREADPRIVATE;
   }

   if (sb_type == Coment) {
      *pdg_sym_class = Name;
   }

   switch (SB_LEN_FLD(sb_idx)) {
   case CN_Tbl_Idx :   
      blk_len = CN_INT_TO_C(SB_LEN_IDX(sb_idx));
# ifdef KEY
      if (TYP_LINEAR(CN_TYPE_IDX(SB_LEN_IDX(sb_idx))) == Integer_4 && blk_len != 0)
        blk_len = (unsigned) blk_len;
# endif
      break;

   case AT_Tbl_Idx : 
      flag = flag | (1 << FEI_SEG_SCON_LEN);
      send_attr_ntry(SB_LEN_IDX(sb_idx));
      blk_len = (long64) PDG_AT_IDX(SB_LEN_IDX(sb_idx));
      break;
   }

   if (SB_SCP_IDX(sb_idx) != curr_scp_idx) {

      if (SB_HOSTED_STACK(sb_idx)) {
         sb_type = Non_Local_Stack;
         *pdg_sym_class	= Hosted_User_Variable;
      }
      else if (sb_type == Formal) {
         sb_type = Non_Local_Formal;
         *pdg_sym_class	= Hosted_Dummy_Arg;
      }
      else if (sb_type == Based) {
         *pdg_sym_class = Hosted_User_Variable;
      }
      else if (PDG_SB_IDX(sb_idx) == NULL_IDX) {

         /* Find this block in the local scope.  It must be there, */
         /* because it was moved in during attr_link_resolution.   */
         local_sb_idx = srch_stor_blk_tbl(SB_NAME_PTR(sb_idx),
                                          SB_NAME_LEN(sb_idx),
                                          curr_scp_idx);

         /* local_sb_idx could be NULL if we have a leftover  */
         /* storage block from an inlined routine.            */
         if (local_sb_idx != NULL_IDX) {
            if (PDG_SB_IDX(local_sb_idx) != NULL_IDX) {
               PDG_SB_IDX(sb_idx) = PDG_SB_IDX(local_sb_idx);
            }
            else {
               host_sb_idx = sb_idx;
               sb_idx = local_sb_idx;
            }
         }
      }
   }
   else if (SB_HOSTED_STACK(sb_idx)) {
      sb_type = Hosted_Stack;
   }

   if (PDG_SB_IDX(sb_idx) != NULL_IDX) {
      goto EXIT;
   }

   name_ptr = SB_NAME_PTR(sb_idx);

   if (sb_type == Task_Common) {  /* -a taskcommon switches */
      /* Intentionally blank. */
   }
   else if (SB_MODULE(sb_idx)) { /* Module data is Common data. */
      sb_type = Common;
   }
   else if (sb_type == Static) {
      if (SB_HOSTED_STATIC(sb_idx)) {   /* Hosted static data is common. */
          flag = flag | (1 << FEI_SEG_DUPLICATE);
         sb_type = Common;
      }
      else {
         sb_type = Static_Local;
      }
   }

   /* All local static blocks are converted to a new kind of      */
   /* storage class for PDGCS.   Basically, the storage class is  */
   /* optimized as local common; but, externalized by the loader. */

   if ((opt_flags.inline_lvl > Inline_Lvl_0 || opt_flags.modinline) &&
       (sb_type == Static_Local || sb_type == Static_Named)) { 
      flag = flag | (1 << FEI_SEG_LOCAL_COMMON);
   }

   if (sb_type == Common || sb_type == Task_Common) {

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
#ifdef KEY /* Bug 14150 */
      if (SB_EXT_NAME_IDX(sb_idx)) {
	name_ptr = SB_EXT_NAME_PTR(sb_idx);
      }
      else
#endif /* KEY Bug 14150 */
      if (! SB_BLANK_COMMON(sb_idx) &&
          ! SB_NAME_IN_STONE(sb_idx)) {

	 int underscores = 0;

         /* Need all common block names lower case with a trailing _,  */
         /* except blank common.                                       */

         for (i = 0; i < SB_NAME_LEN(sb_idx); i++) {
            new_name[i]	= tolower(name_ptr[i]);
	    if (name_ptr[i] == '_') {
	      underscores++;
	    }
         }

#ifdef KEY /* Bug 6204 */
         i = decorate(new_name, i, underscores);
#else /* KEY Bug 6204 */
         if (on_off_flags.underscoring) {
            new_name[i++] = '_';
	    if (on_off_flags.second_underscore && (underscores > 0)) {
	       new_name[i++] = '_';
	    }
	 }
#endif /* KEY Bug 6204 */

         for ( ; i < 256; i++) {
            new_name[i]	= '\0';
         }
         name_ptr = &new_name[0];
      }
# endif

      parent_idx = NULL_IDX;
   }
   else {
      parent_idx = PDG_AT_IDX(SCP_ATTR_IDX(SB_SCP_IDX(sb_idx)));
   }
#ifdef KEY /* Bug 14150 */
   check_duplicate_external_name(SB_Tbl_Idx, sb_idx, name_ptr);
#endif /* KEY Bug 14150 */

   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_C("fei_seg"); 
   PDG_DBG_PRINT_S("(1) name_ptr", name_ptr);
   PDG_DBG_PRINT_S("(2) segment type", p_sb_blk_type_str[sb_type]);
   PDG_DBG_PRINT_S("(3) parent_idx",
                          AT_OBJ_NAME_PTR(SCP_ATTR_IDX(SB_SCP_IDX(sb_idx))));
   PDG_DBG_PRINT_D("(4) unused", 0);
   PDG_DBG_PRINT_D("(5) unused", 0);
   PDG_DBG_PRINT_O("(6) flags", flag);
   PDG_DBG_PRINT_D("(7) unused", 0);
   PDG_DBG_PRINT_LLD("(8) blk_len", blk_len);
   PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
   PDG_SB_IDX(sb_idx) = fei_seg(name_ptr,
 			        sb_type,
	                        parent_idx,
				0,
				0,
                                flag,
				0,
				blk_len);
# endif

   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_LD("(r) PDG_SB_IDX", PDG_SB_IDX(sb_idx));
   PDG_DBG_PRINT_END    

   if (host_sb_idx != NULL_IDX) {
      PDG_SB_IDX(host_sb_idx) = PDG_SB_IDX(sb_idx);
   }

EXIT: 

   TRACE (Func_Exit, "send_stor_blk", NULL);

   return;

}  /* send_stor_blk */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	send_dummy_procedure sends a dummy procedure.			      *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx   -> Index of attr entry for the dummy procedure.	      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void  send_dummy_procedure(int	attr_idx)

{
   long64		flag;
   long64 		size;
   size_offset_type	stor_size;
   TYPE			type_idx;
   int			class;


   TRACE (Func_Entry, "send_dummy_procedure", NULL);

   send_stor_blk(SCP_SB_DARG_IDX(curr_scp_idx), &class);

   class = Dummy_Procedure;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (curr_scp_idx != ATP_SCP_IDX(attr_idx)) {
      class = Hosted_Dummy_Procedure;
   }
# endif

   if (ATP_RSLT_IDX(attr_idx) == NULL_IDX) {
      type_idx = get_type_desc(attr_idx);
   }
   else {
      type_idx = get_type_desc(ATP_RSLT_IDX(attr_idx));
   }

   if (ATP_PGM_UNIT(attr_idx) == Function &&
       TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx))) == Character) {
      stor_size = stor_bit_size_of(attr_idx, FALSE, FALSE);

      if (stor_size.fld == CN_Tbl_Idx) {
         size = CN_INT_TO_C(stor_size.idx);
      }
      else {
         size = F_INT_TO_C(stor_size.constant, TYP_LINEAR(stor_size.type_idx));
      }
   }
   else {
      size = (long64) TARGET_BITS_PER_WORD;
   }


   flag = ((long64) AT_REF_IN_CHILD(attr_idx) << FEI_OBJECT_INNER_REF) |
          ((long64) AT_DEF_IN_CHILD(attr_idx) << FEI_OBJECT_INNER_DEF) |
          ((long64) AT_DEFINED(attr_idx)	   << FEI_OBJECT_DEFINED);

   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_C("fei_object");
   PDG_DBG_PRINT_S("(1) AT_OBJ_NAME", AT_OBJ_NAME_PTR(attr_idx));
   PDG_DBG_PRINT_T("(2) type", type_idx);
   PDG_DBG_PRINT_LLO("(3) flags", flag);
   PDG_DBG_PRINT_S("(4) symbol class", p_obj_sym_class[class]);
   PDG_DBG_PRINT_S("(5) block", SB_NAME_PTR(SCP_SB_DARG_IDX(curr_scp_idx)));
   PDG_DBG_PRINT_D("(6) unused", 0);
   PDG_DBG_PRINT_D("(7) unused", 0);
   PDG_DBG_PRINT_D("(8) unused", 0);
   PDG_DBG_PRINT_S("(9) intent", p_intent[Intent_Inout]);
   PDG_DBG_PRINT_LLD("(10) bit size", size);
   PDG_DBG_PRINT_D("(11) unused", 0);
   PDG_DBG_PRINT_D("(12) storage align", Word_Align);
   PDG_DBG_PRINT_D("(13) unused", 0);
   PDG_DBG_PRINT_D("(14) unused", 0);
   PDG_DBG_PRINT_D("(15) unused", 0);
   PDG_DBG_PRINT_D("(16) def line", AT_DEF_LINE(attr_idx));
   PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
   PDG_AT_IDX(attr_idx) = fei_object(AT_OBJ_NAME_PTR(attr_idx),
                                     type_idx,
                                     flag,
                                     class,
                                     PDG_SB_IDX(SCP_SB_DARG_IDX(curr_scp_idx)),
                                     0,
                                     0,
                                     0,
                                     Intent_Inout,
                                     size,
                                     0,
                                     Word_Align,
                                     0,
                                     0,
                                     0,
                                     AT_DEF_LINE(attr_idx));
# endif

   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_LD("(r) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
   PDG_DBG_PRINT_END    

   TRACE (Func_Exit, "send_dummy_procedure", NULL);

   return;

}   /* send_dummy_procedure */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Calls fei_proc to add an entry point of the routine.  It also         *|
|*      handles all the dummy arguments and the function result.              *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx      -> Attr index of entry point.                           *|
|*	alt_entry_idx -> If ATP_ALT_ENTRY, this is the PDG_AT_IDX of the main *|
|*			 entry point for the alternate entry point.  If       *|
|*			 !ATP_ALT_ENTRY and this field is not NULL_IDX this   *|
|*			 is the SCP_ENTRY_IDX for the entry point.            *|
|*	pdg_sym_idx   -> Pdgcs symbol table index where PDGCS should put this *|
|*			 should put this entry point.                         *|
|*	call_type     -> This is an enum and can be a Definition, Parent or   *|
|*			 Imported.  These determine which PDGCS inteface      *|
|*			 routine to call and what arguments need to be sent.  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void  send_procedure(int			attr_idx,
			    int			alt_entry_idx,
			    int           	call_type)

{
   int			darg_pdg_sn_fw_idx	= NULL_IDX;
   boolean		pure;
   long64		flag3			= (long64) 0;
   int			main_entry_idx;
   char		       *name_ptr;
   int			num_dargs		= 0;
   int			parent_idx;
   int			pdg_alt_entry_idx	= NULL_IDX;
   int			pgm_unit;
   long			prev_idx;
   int			proc;
#ifdef KEY /* Bug 10177 */
   TYPE			type_desc = pdg_type_void;
#else /* KEY Bug 10177 */
   TYPE			type_desc;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "send_procedure", NULL);

   if (call_type == Imported) {
      if (ATP_PROC(attr_idx) == Dummy_Proc) {
         send_dummy_procedure(attr_idx);
         goto EXIT;
      }

      if (ATP_PGM_UNIT(attr_idx) == Module && ATP_IN_CURRENT_COMPILE(attr_idx)){
         /* Only need to send across modules that are not in the current */
         /* compilation unit.                                            */
         goto EXIT;
      }

      if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
         /* Don't send across anything else from a module.  It's not needed. */
         goto EXIT;
      }

      if (AT_USE_ASSOCIATED(attr_idx) &&
          AT_REFERENCED(attr_idx) == Not_Referenced &&
          ATP_IN_INTERFACE_BLK(attr_idx) &&
          ATP_EXPL_ITRFC(attr_idx) &&
          ATP_PROC(attr_idx) == Extern_Proc) {
         name_ptr = ATP_EXT_NAME_PTR(attr_idx);
         if (strcmp(AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)), 
                    name_ptr) == 0) {
            /*
            We have a routine being USE associated that is
            defined in an INTERFACE block only.   The backend must not
            see this.
            */
            goto EXIT;
         }
      }
   }

   pgm_unit = ATP_PGM_UNIT(attr_idx);
   name_ptr = ATP_EXT_NAME_PTR(attr_idx);
   parent_idx = NULL_IDX;


   pure	= ATP_NOSIDE_EFFECTS(attr_idx) || 
          ATP_PURE(attr_idx) || 
          ATP_ELEMENTAL(attr_idx);

   flag3 = 
   ((long64) ATP_VFUNCTION(attr_idx)	    	<< FEI_PROC_VFUNC)|
   ((long64) pure		      	 	<< FEI_PROC_CNGA)|
   ((long64) pure		      	 	<< FEI_PROC_CNAA)|
   ((long64) pure		      		<< FEI_PROC_CNGO)|
   ((long64) pure		      		<< FEI_PROC_RNGO)|
   ((long64) !SCP_DOES_IO(curr_scp_idx)    	<< FEI_PROC_NOIO)|
   ((long64) !SCP_HAS_CALLS(curr_scp_idx)   	<< FEI_PROC_NOCALLS)|
   ((long64) ATP_ALL_INTENT_IN(attr_idx)     	<< FEI_PROC_DNDA)|
   ((long64) ATP_RECURSIVE(attr_idx)	      	<< FEI_PROC_RECURSE)|
   ((long64) ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx))
                                                << FEI_PROC_MICROTASK)|
   ((long64) opt_flags.taskinner	      		<< FEI_PROC_TASKINNER)|
   ((long64) !opt_flags.threshold	      	<< FEI_PROC_NOTHRESHOLDS)|
   ((long64) 1				      	<< FEI_PROC_TOLERANT)|
   ((long64) on_off_flags.alloc_autos_on_stack	<< FEI_PROC_LIMHEAP)|
   ((long64) ATP_DCL_EXTERNAL(attr_idx)	      	<< FEI_PROC_IMMORT)|
   ((long64) ATP_EXTRA_DARG(attr_idx)	      	<< FEI_PROC_HASRSLT)|
   ((long64) on_off_flags.indef_init	      	<< FEI_PROC_INDEFSTK)|
   ((long64) (cmd_line_flags.solaris_profile | cdir_switches.flow) 
						<< FEI_PROC_DOFLOWTR)|
   ((long64) opt_flags.zeroinc		      	<< FEI_PROC_ZEROINC)|
   ((long64) ATP_ALIGN(attr_idx)	      		<< FEI_PROC_ALGNINST)|
   ((long64) ATP_NAME_IN_STONE(attr_idx)      	<< FEI_PROC_PERM_NAME)|
   ((long64) on_off_flags.round_mult_operations	<< FEI_PROC_STRROUND)|
   ((long64) !on_off_flags.round_mult_operations	<< FEI_PROC_TRUNCATE)|
   ((long64) (opt_flags.over_index | 
             ATP_HAS_OVER_INDEXING(SCP_ATTR_IDX(curr_scp_idx)))
						<< FEI_PROC_NCOADDR)|
   ((long64) ((ATP_PGM_UNIT(attr_idx) == Function) & on_off_flags.recursive)
						<< FEI_PROC_RECURSE)|
   ((long64) ((ATP_PGM_UNIT(attr_idx) == Subroutine) & on_off_flags.recursive)
						<< FEI_PROC_RECURSE)|
   ((long64) ((attr_idx == SCP_ATTR_IDX(curr_scp_idx)) & 
             (ATP_PGM_UNIT(attr_idx) == Module))
						<< FEI_PROC_MOD_FIRST)|
   ((long64) ((attr_idx == SCP_ATTR_IDX(curr_scp_idx)) & 
             (ATP_PROC(attr_idx) == Module_Proc))
						<< FEI_PROC_MOD_JUST)|
   ((long64) (attr_idx == AT_WORK_IDX)  		<< FEI_PROC_MOD_LAST)|
   ((long64) ATP_ARGCHCK_CALL(attr_idx)      	<< FEI_PROC_ARG_CHECK)|
   ((long64) on_off_flags.zero_init          	<< FEI_PROC_ZERO_INIT)|
   ((long64) ATP_OPTIONAL_DIR(attr_idx)        	<< FEI_PROC_OPTIONAL_DIR)|
   ((long64) (call_type == Definition)         	<< FEI_PROC_DEFINITION)|
   ((long64) (call_type == Imported)          	<< FEI_PROC_IMPORTED)|
   ((long64) (call_type == Parent)          	<< FEI_PROC_PARENT)|
   ((long64) (ATP_NOSIDE_EFFECTS(attr_idx) | ATP_PURE(attr_idx))
						<< FEI_PROC_NOSIDE_EFFECTS)|
   ((long64) ATP_ELEMENTAL(attr_idx)         	<< FEI_PROC_ELEMENTAL);

   if (ATP_PGM_UNIT(attr_idx) == Module &&
       (ATP_SCP_IDX(attr_idx) != curr_scp_idx ||
        !ATP_IN_CURRENT_COMPILE(attr_idx))) {
      flag3 = flag3 | ((long64) 1 << FEI_PROC_IMMORT);
   }


   if (call_type == Definition || call_type == Parent) {
      if (SCP_ALT_ENTRY_CNT(ATP_SCP_IDX(attr_idx)) > 0) {
         flag3 = flag3 | ((long64) 1 << FEI_PROC_HAS_ALT_ENTRY);
      }

      if (ATP_SGI_GLOBAL_INLINE(attr_idx)) {
         flag3 = flag3 | ((long64) 1 << FEI_PROC_GLOBAL_INLINE);
      }

      proc = ATP_PROC(attr_idx);

      if (SCP_PARENT_IDX(curr_scp_idx) != NULL_IDX &&
          ATP_PGM_UNIT(SCP_ATTR_IDX(SCP_PARENT_IDX(curr_scp_idx))) != Module) {
         parent_idx = PDG_AT_IDX(SCP_ATTR_IDX(SCP_PARENT_IDX(curr_scp_idx)));
      }

      if (ATP_PROC(attr_idx) == Unknown_Proc || 
          ATP_PROC(attr_idx) == Module_Proc) {
         proc = Extern_Proc;
      }

      if (alt_entry_idx != NULL_IDX) {
         if (ATP_ALT_ENTRY(attr_idx)) {
            pdg_alt_entry_idx = alt_entry_idx;
            flag3 = flag3 | ((long64) 1 << FEI_PROC_ENTRY);
         }
         else {
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("fei_next_name");
            PDG_DBG_PRINT_D("(1) logical", TRUE);
            PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
            pdg_alt_entry_idx = fei_next_name(TRUE);
# endif
         }
      }

      if (ATP_PROC(attr_idx) == Intern_Proc) { 
         pdg_alt_entry_idx = parent_idx;
      }
   }
   else {
      proc = (ATP_PROC(attr_idx) == Intern_Proc) ? Intern_Proc_Refd :
                                                        Imported_Proc;

      if (attr_idx == glb_tbl_idx[Buffer_In_Attr_Idx]) {
         flag3 = flag3 | ((long64) 1 << FEI_PROC_BUFIN);
      }
      else if (attr_idx == glb_tbl_idx[Buffer_Out_Attr_Idx]) {
         flag3 = flag3 | ((long64) 1 << FEI_PROC_BUFOUT);
      }
   }

   switch (pgm_unit) {

   case Module:
   case Blockdata:
   case Program:
   case Pgm_Unknown:
      type_desc	= pdg_type_void;
      break;


   case Subroutine:
      num_dargs = (call_type == Imported) ? 0 : ATP_NUM_DARGS(attr_idx);

      if (ATP_HAS_ALT_RETURN(attr_idx)) {
         pgm_unit = Function;
         type_desc = get_basic_type(CG_INTEGER_DEFAULT_TYPE,
                                    type_alignment_tbl[CG_INTEGER_DEFAULT_TYPE],
                                    NULL_IDX);
      }
      else {
         type_desc = pdg_type_void;
      }
      break;


   case Function:
      num_dargs = (call_type == Imported) ? 0 : ATP_NUM_DARGS(attr_idx);

      if (ATP_EXTRA_DARG(attr_idx)) {
         pgm_unit = Subroutine;
// Bug 2204
# ifdef KEY
         if  (TYP_LINEAR(ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx))) == Structure_Type)
           type_desc = get_type_desc(attr_idx);
         else 
           type_desc = pdg_type_void;
# else
         type_desc = pdg_type_void;
# endif
      }
      else {

         /*
         Need to send the attr_idx to
         get_type_desc, because we need
         to determine if these bounds are 
         template bounds or real bounds.
         */
         type_desc = get_type_desc(attr_idx);

         if (call_type == Definition ||
             (call_type == Parent && 
              SB_HOSTED_STACK(ATD_STOR_BLK_IDX(ATP_RSLT_IDX(attr_idx))))) {

            /*
            Send the function result if this is a definition or if the
            result has been host associated.  The result gets host
            associated if a reference is made to the function or the
            result and they have the same name.  Since at the symbol
            table level we don't know if the function is being called
            or if the result is being host associated, we assume that
            both things are happening.
            */

            send_attr_ntry(ATP_RSLT_IDX(attr_idx));
         }
      }
      break;
   }

   if (num_dargs != 0) {
      PDG_DBG_PRINT_START    
      PDG_DBG_PRINT_C("fei_next_name");
      PDG_DBG_PRINT_D("(1) logical", TRUE);
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      darg_pdg_sn_fw_idx = fei_next_name(TRUE);
# endif
   }

   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_C("fei_proc");
   PDG_DBG_PRINT_S("(1) name_ptr", name_ptr);
   PDG_DBG_PRINT_D("(2) AT_DEF_LINE", AT_DEF_LINE(attr_idx));
   PDG_DBG_PRINT_S("(3) pgm_unit", p_atp_pgm_unit[pgm_unit]);
   PDG_DBG_PRINT_S("(4) proc", p_proc[proc]);
   PDG_DBG_PRINT_D("(5) num_dargs", num_dargs);
   PDG_DBG_PRINT_D("(6) parent_idx", parent_idx);
   PDG_DBG_PRINT_D("(7) pdg darg list", darg_pdg_sn_fw_idx);
   PDG_DBG_PRINT_D("(8) alt link idx", pdg_alt_entry_idx);
   PDG_DBG_PRINT_T("(9) type", type_desc);
   PDG_DBG_PRINT_LD("(10) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
   PDG_DBG_PRINT_LLO("(11) flags", flag3);
   PDG_DBG_PRINT_END    
# ifdef _ENABLE_FEI
   PDG_AT_IDX(attr_idx) = fei_proc(name_ptr,
                                   AT_DEF_LINE(attr_idx),
                                   pgm_unit,
                                   proc,
                                   num_dargs,
 				   parent_idx,
	         		   darg_pdg_sn_fw_idx,
                                   pdg_alt_entry_idx,
                                   type_desc,
                                   PDG_AT_IDX(attr_idx),
                                   flag3);
# endif

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_LD("(r) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
   PDG_DBG_PRINT_END


                                        
   if (cmd_line_flags.debug_lvl <= Debug_Lvl_2) {
      if (ATP_PROC(attr_idx) == Intrin_Proc) {
         if (ATP_INTERFACE_IDX(attr_idx) != NULL_IDX) {
            PDG_DBG_PRINT_START
            PDG_DBG_PRINT_C("fei_smt_actual_to_generic");
            PDG_DBG_PRINT_LD("PDG_AT_IDX", PDG_AT_IDX(attr_idx));
            PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
            fei_smt_actual_to_generic(PDG_AT_IDX(attr_idx),
                                  AT_OBJ_NAME_PTR(ATP_INTERFACE_IDX(attr_idx)));
# endif
         }
      }

      if (ATP_EXT_NAME_IDX(attr_idx) != AT_NAME_IDX(attr_idx)) {
         /* These may be the same even though indexes are different */

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_smt_original_to_qualified");
         PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
         PDG_DBG_PRINT_S("(2) AT_OBJ_NAME", AT_OBJ_NAME_PTR(attr_idx));
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_smt_original_to_qualified(PDG_AT_IDX(attr_idx),
                                       AT_OBJ_NAME_PTR(attr_idx));
# endif
      }
   }

   if (num_dargs != 0) {
      send_darg_list(attr_idx, darg_pdg_sn_fw_idx);
   }
      
   if (alt_entry_idx != NULL_IDX && !ATP_ALT_ENTRY(attr_idx)) {

      /* This is a main entry for a definition or a parent. */
      /* Send the alternate entries.                        */

      prev_idx = NULL_IDX;
      main_entry_idx = attr_idx;

      while (alt_entry_idx != NULL_IDX) {
         attr_idx = AL_ATTR_IDX(alt_entry_idx);
#ifdef KEY /* Bug 14150 */
	 check_duplicate_external_name(AT_Tbl_Idx, attr_idx,
	   ATP_EXT_NAME_PTR(attr_idx));
#endif /* KEY Bug 14150 */

         /* Send entry name, result name, and dummy args.  Need to */
         /* set ATP_SCP_ALIVE, so if there are any bounds tmps,    */
         /* they get the correct storage block.  If not, they will */
         /* look like they are host associated.                    */

         ATP_SCP_ALIVE(attr_idx) = TRUE;

         send_procedure(attr_idx, 
                        PDG_AT_IDX(main_entry_idx),
                        call_type);

         ATP_SCP_ALIVE(attr_idx) = FALSE;

         name_ptr = ATP_EXT_NAME_PTR(attr_idx);

         /* prev_idx is a pdgcs index to the name being entered */
         /* in the pdgcs secondary name table.                  */

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_name");
         PDG_DBG_PRINT_S("(1) name_ptr", name_ptr);
         PDG_DBG_PRINT_S("(2) table", "PDGCS_Function");
         PDG_DBG_PRINT_LD("(3) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
         PDG_DBG_PRINT_D("(4) prev_idx", prev_idx);
         PDG_DBG_PRINT_D("(5) pdg_alt_entry_idx", pdg_alt_entry_idx);
         PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
         prev_idx = fei_name(name_ptr,
                             Sym_Function,
                             PDG_AT_IDX(attr_idx),
                             prev_idx,
                             pdg_alt_entry_idx);
# endif
         alt_entry_idx = AL_NEXT_IDX(alt_entry_idx);

         if (alt_entry_idx != NULL_IDX) {
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("fei_next_name");
            PDG_DBG_PRINT_D("(1) logical", TRUE);
            PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
            pdg_alt_entry_idx = fei_next_name(TRUE);
# endif
         }
      }
   }

EXIT: 

   TRACE (Func_Exit, "send_procedure", NULL);

   return;

}   /* send_procedure */



/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static TYPE	send_derived_type(int	type_idx)

{
   int		attr_idx;
   int		cpnt_idx;
   int		dt_attr_idx;
   long		flag			= 0;
   int		dt_idx;
   long		prev_idx;
   int		sequence;
   int		sn_idx;
   long64  	size;
   TYPE		pdg_type_idx;


   TRACE (Func_Entry, "send_derived_type", NULL);

   dt_attr_idx = TYP_IDX(type_idx);

   while (AT_ATTR_LINK(dt_attr_idx) != NULL_IDX) {
      dt_attr_idx = AT_ATTR_LINK(dt_attr_idx);
   }

   if (PDG_AT_IDX(dt_attr_idx) != NULL_IDX) {
      pdg_type_idx = pdg_type_tbl[PDG_AT_TYP_IDX(dt_attr_idx)];
      pdg_type_tbl[type_idx] = pdg_type_idx;
      goto EXIT;
   }
#ifdef KEY /* Bug 14150 */
  if (c_ptr_abi_trouble(dt_attr_idx)) {
    pdg_type_idx = fei_descriptor(0, Basic, bit_size_tbl[Integer_4], Integral,
      0 /* unused */, pdg_align[Word_Align]);
    return pdg_type_idx;
  }
#endif /* KEY Bug 14150 */

   flag = ((long) (ATT_SCP_IDX(dt_attr_idx) != curr_scp_idx) 
                                           << FEI_NEXT_TYPE_IDX_HOSTED_TYPE);

   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_C("fei_next_name");
   PDG_DBG_PRINT_D("(1) logical", TRUE);
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
   cpnt_idx = fei_next_name(TRUE);
# endif

   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_C("fei_next_type_idx");
   PDG_DBG_PRINT_LO("(1) flags", flag);
   PDG_DBG_PRINT_D("(2) alignment", pdg_align[ATT_ALIGNMENT(dt_attr_idx)]);
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
   dt_idx = fei_next_type_idx(flag, pdg_align[ATT_ALIGNMENT(dt_attr_idx)]);
# endif

   sequence = (ATT_CHAR_SEQ(dt_attr_idx)) ? Seq_Char : Seq_Mixed;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (ATT_CHAR_SEQ(dt_attr_idx)) {
      sequence = Seq_Char;
   }
   else if (ATT_DCL_NUMERIC_SEQ(dt_attr_idx)) {
      sequence = Seq_Numeric;
   }
   else if (ATT_SEQUENCE_SET(dt_attr_idx)) {
      sequence = Seq_Mixed;
   }
   else {
      sequence = Seq_None;
   }
# endif

   size = CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(dt_attr_idx));
   
   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_C("fei_user_type");
   PDG_DBG_PRINT_S("(1) AT_OBJ_NAME", AT_OBJ_NAME_PTR(dt_attr_idx));
   PDG_DBG_PRINT_D("(2) ATT_NUM_CPNTS", ATT_NUM_CPNTS(dt_attr_idx));
   PDG_DBG_PRINT_D("(3) cpnt_idx", cpnt_idx);
   PDG_DBG_PRINT_LLD("(4) struct size", size);
   PDG_DBG_PRINT_S("(5) sequence", p_sequence[sequence]);
   PDG_DBG_PRINT_D("(6) dt_idx", dt_idx);
   PDG_DBG_PRINT_D("(7) alignment", pdg_align[ATT_ALIGNMENT(dt_attr_idx)]);
   PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
   fei_user_type(AT_OBJ_NAME_PTR(dt_attr_idx),
                 ATT_NUM_CPNTS(dt_attr_idx),
                 cpnt_idx,
                 size,
                 sequence,
                 dt_idx,
                 pdg_align[ATT_ALIGNMENT(dt_attr_idx)]);
# endif


   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("fei_descriptor");
   PDG_DBG_PRINT_D("(1) flags",0);
   PDG_DBG_PRINT_S("(2) table type", p_table_type[Basic]);
   PDG_DBG_PRINT_D("(3) dt_idx", dt_idx);
   PDG_DBG_PRINT_S("(4) basic type", p_basic_type[S_tructure]);
   PDG_DBG_PRINT_D("(5) aux info",0);
   PDG_DBG_PRINT_D("(6) alignment", pdg_align[ATT_ALIGNMENT(dt_attr_idx)]);
   PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
   pdg_type_idx = fei_descriptor(0,
                                 Basic,
                                 dt_idx,
                                 S_tructure,
                                 0,  
                                 pdg_align[ATT_ALIGNMENT(dt_attr_idx)]);
# endif
   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_T("(r) type", pdg_type_idx);
   PDG_DBG_PRINT_END

   pdg_type_tbl[type_idx] = pdg_type_idx;
   PDG_AT_IDX(dt_attr_idx) = dt_idx;
   PDG_AT_TYP_IDX(dt_attr_idx) = type_idx;
   dt_attr_idx = TYP_IDX(type_idx);

   while (AT_ATTR_LINK(dt_attr_idx) != NULL_IDX) {
      PDG_AT_IDX(dt_attr_idx) = dt_idx;
      dt_attr_idx = AT_ATTR_LINK(dt_attr_idx);
   }

   prev_idx = NULL_IDX;
   sn_idx = ATT_FIRST_CPNT_IDX(dt_attr_idx);

   do {
#ifdef KEY /* Bug 14150 */
      /* F2003 allows types with no components */
      if (sn_idx == NULL_IDX) {
        break;
      }
#endif /* KEY Bug 14150 */
      attr_idx = SN_ATTR_IDX(sn_idx);
      send_attr_ntry(attr_idx);

      PDG_DBG_PRINT_START    
      PDG_DBG_PRINT_C("fei_name");
      PDG_DBG_PRINT_S("(1) AT_OBJ_NAME", AT_OBJ_NAME_PTR(attr_idx));
      PDG_DBG_PRINT_S("(2) table", "PDGCS_Member");
      PDG_DBG_PRINT_LD("(3) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
      PDG_DBG_PRINT_D("(4) prev_idx", prev_idx);
      PDG_DBG_PRINT_D("(5) cpnt_idx", cpnt_idx);
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      prev_idx = fei_name(AT_OBJ_NAME_PTR(attr_idx),
                          Sym_Member,
                          PDG_AT_IDX(attr_idx),
                          prev_idx,
                          cpnt_idx);
# endif

      sn_idx = SN_SIBLING_LINK(sn_idx);

      if (sn_idx != NULL_IDX) {
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_next_name");
         PDG_DBG_PRINT_D("(1) logical", TRUE);
         PDG_DBG_PRINT_END
 
# ifdef _ENABLE_FEI
         cpnt_idx = fei_next_name(TRUE);
# endif
      }

   } while (sn_idx != NULL_IDX);

EXIT:

   TRACE (Func_Exit, "send_derived_type", NULL);

   return(pdg_type_idx);

}   /* send_derived_type */



/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static void  send_label(int	attr_idx)

{
   int      		 label_flag;
   int			 label_class		= ATL_CLASS(attr_idx);
   char			*fmt_ptr		= NULL;


   TRACE (Func_Entry, "send_label", NULL);

   if (!ATL_EXECUTABLE(attr_idx) && ATL_CLASS(attr_idx) <= Lbl_User) {

      /* Do not send labels that are on non-executable statements */

      return;
   }

   label_flag = (ATL_ALIGN(attr_idx)      << FEI_LABEL_ALGNINST) |
                (opt_flags.loopalign      << FEI_LABEL_ALGNLOOP);

   switch (ATL_CLASS(attr_idx)) {

      case Lbl_Construct:
         label_class = Lbl_User;
         break;

      case Lbl_Format:
         fmt_ptr = (char *) &CN_CONST(ATD_TMP_IDX(ATL_FORMAT_TMP(attr_idx)));
         break;

   }  /* End switch */

   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_C("fei_label");
   PDG_DBG_PRINT_S("(1) AT_OBJ_NAME", AT_OBJ_NAME_PTR(attr_idx));
   PDG_DBG_PRINT_O("(2) flags", label_flag);
   PDG_DBG_PRINT_S("(3) label class", p_label[label_class]);
   PDG_DBG_PRINT_S("(4) fmt_ptr", fmt_ptr);
   PDG_DBG_PRINT_S("(5) ATL_DEBUG_CLASS",
                          p_dbg_label[ATL_DEBUG_CLASS(attr_idx)]);
   PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
   PDG_AT_IDX(attr_idx) = fei_label(AT_OBJ_NAME_PTR(attr_idx),
                                    label_flag,
                                    label_class,
                                    fmt_ptr,
                                    ATL_DEBUG_CLASS(attr_idx));
# endif

   TRACE (Func_Exit, "send_label", NULL);

   return;
}   /* send_label */



/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static  void    send_label_def(int	ir_idx)
{
   int		attr_idx;
   int		blockable_idx;
   int		cache_bypass_count;
   int		il_idx;
   int		interchange_idx;
   int		interchange_level_idx;
   boolean	keepme;
   long64	label_flag		= (long64) 0;
   int		mark_name_idx;
   int		safe_distance_idx;
   int		safevl_idx;
   int		suppress_cnt 		= 0;
   int		task_lvl;
   int		unroll_idx;


   TRACE (Func_Entry, "send_label_def", NULL);

   if (IR_OPR(ir_idx) == Label_Opr) {
      attr_idx = IR_IDX_L(ir_idx);
      keepme = (cmd_line_flags.debug_lvl <= Debug_Lvl_2) ||
                                   ATL_IN_ASSIGN(attr_idx);
      send_attr_ntry(attr_idx);
   }
   else {
      keepme = FALSE;
      attr_idx = IR_IDX_R(ir_idx);
      label_flag = label_flag | ((long64) 1 << FEI_LABEL_DEF_NAMED_SUPPRESS);

      if (IR_IDX_L(ir_idx) == NULL_IDX) {
         /* If nothing in the list, make sure that the CNT is zero. */
         IR_LIST_CNT_L(ir_idx) = 0;
      }
      else {
         suppress_cnt = IR_LIST_CNT_L(ir_idx);
      }
   }

   task_lvl = (ATL_NOTASK(attr_idx)) ? Task_Lvl_0: opt_flags.task_lvl;

   label_flag |= 
        ((long64) ATL_IVDEP(attr_idx)       << FEI_LABEL_DEF_NAMED_IVDEP)|
        ((long64) ATL_FISSIONABLE(attr_idx) << FEI_LABEL_DEF_NAMED_FISSIONABLE)|
        ((long64) ATL_FUSABLE(attr_idx)     << FEI_LABEL_DEF_NAMED_FUSABLE)|
        ((long64) ATL_NOFISSION(attr_idx)   << FEI_LABEL_DEF_NAMED_NOFISSION)|
        ((long64) ATL_NOFUSION(attr_idx)    << FEI_LABEL_DEF_NAMED_NOFUSION)|
        ((long64) ATL_FUSION(attr_idx)      << FEI_LABEL_DEF_NAMED_FUSION)|
        ((long64) ATL_NOINTERCHANGE(attr_idx)
                            << FEI_LABEL_DEF_NAMED_NOINTERCHANGE)|
        ((long64) ATL_NOBLOCKING(attr_idx)  << FEI_LABEL_DEF_NAMED_NOBLOCKING)|
        ((long64) ATL_AGGRESSIVEINNERLOOPFISSION(attr_idx)   
                            << FEI_LABEL_DEF_NAMED_AGGRESSIVEINNERLOOPFISSION)|
        ((long64) ATL_NORECURRENCE(attr_idx)<< FEI_LABEL_DEF_NAMED_NOREDUCE)|
 	((long64) ATL_NOVECTOR(attr_idx)    << FEI_LABEL_DEF_NAMED_NOVECTOR)|
        ((long64) ATL_STREAM(attr_idx)	   << FEI_LABEL_DEF_NAMED_STREAM)|
        ((long64) ATL_PREFERSTREAM(attr_idx)
                                           << FEI_LABEL_DEF_NAMED_PREFERSTREAM)|
        ((long64) ATL_PREFERSTREAM_NOCINV(attr_idx)<< 
                                           FEI_LABEL_DEF_NAMED_PSTREAM_NOCINV)|
	((long64) ATL_SHORTLOOP(attr_idx)   << FEI_LABEL_DEF_NAMED_SHORTLOOP)|
	((long64) ATL_NOVSEARCH(attr_idx)   << FEI_LABEL_DEF_NAMED_NOVSEARCH)|
        ((long64) ATL_TOP_OF_LOOP(attr_idx) << FEI_LABEL_DEF_NAMED_LOOPCHK)|
        ((long64) ATL_INFORM_ONLY(attr_idx) << FEI_LABEL_DEF_NAMED_INFORM_ONLY)|
        ((long64) ATL_BL(attr_idx)          << FEI_LABEL_DEF_NAMED_DO_BL)|
        ((long64) (task_lvl == Task_Lvl_3)  << FEI_LABEL_DEF_NAMED_NOSYNCBARR)|
        ((long64) (opt_flags.vector_lvl == Vector_Lvl_3)
                                           << FEI_LABEL_DEF_NAMED_EXTTABLES)|
        ((long64) (opt_flags.vector_lvl == Vector_Lvl_3)
                                           << FEI_LABEL_DEF_NAMED_EXTTIME)|
        ((long64) (AT_REFERENCED(attr_idx) == Not_Referenced)
                            << FEI_LABEL_DEF_NAMED_NOT_REFERENCED)|
        ((long64) ATL_CNCALL(attr_idx)      << FEI_LABEL_DEF_NAMED_CONCCALLS)|
        ((long64) ATL_PERMUTATION(attr_idx) << FEI_LABEL_DEF_NAMED_PERMUTATION)|
	((long64) ATL_NEXTSCALAR(attr_idx)  << FEI_LABEL_DEF_NAMED_NEXTSCALAR)|
	((long64) ATL_SHORTLOOP128(attr_idx)
                                           << FEI_LABEL_DEF_NAMED_SHORTLOOP128)|
	((long64) ATL_PREFERVECTOR(attr_idx)<< FEI_LABEL_DEF_NAMED_SELECT_VEC)|
	((long64) ATL_PREFERTASK(attr_idx)  << FEI_LABEL_DEF_NAMED_SELECT_TASK)|
	((long64) ATL_NOTASK(attr_idx)	   << FEI_LABEL_DEF_NAMED_NOTASK)|
	((long64) ATL_UNROLL_DIR(attr_idx)  << FEI_LABEL_DEF_NAMED_UNROLL)|
        ((long64) ATL_SPLIT(attr_idx)       << FEI_LABEL_DEF_NAMED_STREAMSPLIT)|
        ((long64) ATL_PATTERN(attr_idx)     << FEI_LABEL_DEF_NAMED_PATTERN)|
        ((long64) ATL_CONSTRUCTOR_LOOP(attr_idx)      
                            << FEI_LABEL_DEF_NAMED_CONSTRUCTOR_LOOP)|
        ((long64) ATL_CONCURRENT(attr_idx)  << FEI_LABEL_DEF_NAMED_CONCURRENT)|
        ((long64) ATL_CASE_LABEL(attr_idx)  << FEI_LABEL_DEF_NAMED_CASE);

   /* List contains directives in this order           */
   /* safevl, Unroll, mark name, maxcpus, cache bypass */ 

   safevl_idx		= NULL_IDX;
   unroll_idx		= NULL_IDX;
   mark_name_idx	= NULL_IDX;
   cache_bypass_count	= 0;
   safe_distance_idx	= NULL_IDX;
   blockable_idx	= NULL_IDX;
   interchange_idx	= NULL_IDX;
   interchange_level_idx= NULL_IDX;

   if (ATL_DIRECTIVE_LIST(attr_idx) != NULL_IDX) {
      il_idx = IL_IDX(ATL_DIRECTIVE_LIST(attr_idx)) + Safevl_Dir_Idx;

      if (IL_FLD(il_idx) == CN_Tbl_Idx) {
         safevl_idx = IL_IDX(il_idx);
      }

      il_idx = IL_IDX(ATL_DIRECTIVE_LIST(attr_idx)) + Unroll_Dir_Idx;

      if (IL_FLD(il_idx) == CN_Tbl_Idx) {     /* UNROLL */
         unroll_idx = IL_IDX(il_idx);
      }

      il_idx = IL_IDX(ATL_DIRECTIVE_LIST(attr_idx)) + Mark_Dir_Idx;

      if (IL_FLD(il_idx) == CN_Tbl_Idx) {     /* MARK */
         mark_name_idx = IL_IDX(il_idx);
      }

      il_idx = IL_IDX(ATL_DIRECTIVE_LIST(attr_idx)) + Cache_Bypass_Dir_Idx;

      if (IL_FLD(il_idx) == IL_Tbl_Idx) {  /* Cache bypass */
         cache_bypass_count = IL_LIST_CNT(il_idx);
         il_idx	= IL_IDX(il_idx);

         while (il_idx != NULL_IDX) {
            cvrt_exp_to_pdg(IL_IDX(il_idx), AT_Tbl_Idx);
            il_idx = IL_NEXT_LIST_IDX(il_idx);
         }
      }

      il_idx = IL_IDX(ATL_DIRECTIVE_LIST(attr_idx)) + Concurrent_Dir_Idx;

      if (IL_FLD(il_idx) == CN_Tbl_Idx) {
         safe_distance_idx = IL_IDX(il_idx);
      }

      il_idx = IL_IDX(ATL_DIRECTIVE_LIST(attr_idx)) + Blockable_Dir_Idx;

      if (IL_FLD(il_idx) == CN_Tbl_Idx) {
         blockable_idx = IL_IDX(il_idx);
      }

      il_idx = IL_IDX(ATL_DIRECTIVE_LIST(attr_idx)) + Interchange_Dir_Idx;

      if (IL_FLD(il_idx) == CN_Tbl_Idx) {
         interchange_idx = IL_IDX(il_idx);
      }

      il_idx = IL_IDX(ATL_DIRECTIVE_LIST(attr_idx)) + Interchange_Level_Dir_Idx;

      if (IL_FLD(il_idx) == CN_Tbl_Idx) {
         interchange_level_idx = IL_IDX(il_idx);
      }
   }


   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_C("fei_label_def_named");
   PDG_DBG_PRINT_S("AT_OBJ_NAME", AT_OBJ_NAME_PTR(attr_idx));
   PDG_DBG_PRINT_LD("(1) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
   PDG_DBG_PRINT_LLO("(2) flags", label_flag);
   PDG_DBG_PRINT_D("(3) line num", IR_LINE_NUM(ir_idx));
   PDG_DBG_PRINT_D("(4) suppress cnt", suppress_cnt);
   PDG_DBG_PRINT_D("(5) keepme", keepme);
   PDG_DBG_PRINT_LVD("(7) safevl", (ATL_IVDEP(attr_idx)?
                                       (long) CN_INT_TO_C(safevl_idx):0));
   PDG_DBG_PRINT_LVD("(8) unroll cnt", ((ATL_UNROLL_DIR(attr_idx)) ?
                                    (long) CN_INT_TO_C(unroll_idx) : 1));
   PDG_DBG_PRINT_S("(9) mark name", mark_name_idx == NULL_IDX ? " " :
                                     (char *)&CN_CONST(mark_name_idx));
   PDG_DBG_PRINT_D("(10) cache bypass ct", cache_bypass_count);
   PDG_DBG_PRINT_LVD("(11) safe distance", (safe_distance_idx != NULL_IDX ?
                                      (long) CN_INT_TO_C(safe_distance_idx):0));
   PDG_DBG_PRINT_LVD("(12) blockable grp", (blockable_idx != NULL_IDX ?
                                      (long) CN_INT_TO_C(blockable_idx):0));
   PDG_DBG_PRINT_LVD("(13) interchange grp", (interchange_idx != NULL_IDX ?
                                      (long) CN_INT_TO_C(interchange_idx):0));
   PDG_DBG_PRINT_LVD("(13) interchange lvl",(interchange_level_idx != NULL_IDX?
                                  (long) CN_INT_TO_C(interchange_level_idx):0));
   PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
   fei_label_def_named(PDG_AT_IDX(attr_idx),
                       label_flag,
                       IR_LINE_NUM(ir_idx),
                       suppress_cnt,
                       keepme,
                       0,
                       (ATL_IVDEP(attr_idx) ? 
                                     (long) CN_INT_TO_C(safevl_idx) : 0),
                       (ATL_UNROLL_DIR(attr_idx)) ? 
                                     (long) CN_INT_TO_C(unroll_idx) : 1,
                       mark_name_idx == NULL_IDX ? NULL :
                                     (char *) &CN_CONST(mark_name_idx),
                       cache_bypass_count,
                       (safe_distance_idx != NULL_IDX) ? 
                                     (long) CN_INT_TO_C(safe_distance_idx) : 0,
                       (blockable_idx != NULL_IDX) ? 
                                     (long) CN_INT_TO_C(blockable_idx) : 0,
                       (interchange_idx != NULL_IDX) ? 
                                     (long) CN_INT_TO_C(interchange_idx) : 0,
                       (interchange_level_idx != NULL_IDX) ? 
                                 (long) CN_INT_TO_C(interchange_level_idx) : 0);
# endif

   TRACE (Func_Exit, "send_label_def", NULL);

   return;

}  /* send_label_def */



/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static void  send_namelist_group(int	ng_attr_idx)

{
   int		attr_idx;
   int		count;
   int		name_idx;
   long     	prev_idx;
   int		sn_idx;


   TRACE (Func_Entry, "send_namelist_group", NULL);


   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_C("fei_next_name");
   PDG_DBG_PRINT_D("(1) logical", TRUE);
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
   name_idx = fei_next_name(TRUE);
# endif

   prev_idx = NULL_IDX;
   sn_idx = ATN_FIRST_NAMELIST_IDX(ng_attr_idx);
   count = 0;

   do {
      attr_idx = SN_ATTR_IDX(sn_idx);
      send_attr_ntry(attr_idx);
 
      PDG_DBG_PRINT_START    
      PDG_DBG_PRINT_C("fei_name");
      PDG_DBG_PRINT_S("(1) AT_OBJ_NAME", AT_OBJ_NAME_PTR(attr_idx));
      PDG_DBG_PRINT_S("(2) table", "PDGCS_Namelist");
      PDG_DBG_PRINT_LD("(3) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
      PDG_DBG_PRINT_LD("(4) prev_idx", prev_idx);
      PDG_DBG_PRINT_D("(5) name_idx", name_idx);
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      prev_idx = fei_name(AT_OBJ_NAME_PTR(attr_idx),
                          Sym_Namelist,
                          PDG_AT_IDX(attr_idx),
                          prev_idx,
                          name_idx);
# endif

      count = count + 1;
      sn_idx = SN_SIBLING_LINK(sn_idx);

      if (sn_idx != NULL_IDX) {
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_next_name");
         PDG_DBG_PRINT_D("(1) logical", TRUE);
         PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
         name_idx = fei_next_name(TRUE);
# endif
      }
   } while (sn_idx != NULL_IDX);

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("fei_namelist");
   PDG_DBG_PRINT_S("(1) AT_OBJ_NAME", AT_OBJ_NAME_PTR(ng_attr_idx));
   PDG_DBG_PRINT_D("(2) count", count);
   PDG_DBG_PRINT_LD("(3) prev_idx", prev_idx);
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   PDG_AT_IDX(ng_attr_idx) = fei_namelist(AT_OBJ_NAME_PTR(ng_attr_idx),
                                          count,
                                          prev_idx);
# endif
# endif

   TRACE (Func_Exit, "send_namelist_group", NULL);

   return;

}   /* send_namelist_group */


/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static void allocate_pdg_link_tbls(void)

{
   register	int	 i;
   		int	 new_size;
   		long	*pdg_tbl_base;


   TRACE (Func_Entry, "allocate_pdg_link_tbls", NULL);

   /* Allocate and clear out the pdg link table.   Find the largest table  */
   /* and allocate the pdg link table to this size.                        */

   new_size = (attr_tbl_idx > const_tbl_idx) ? attr_tbl_idx : const_tbl_idx;
   new_size = (new_size > stor_blk_tbl_idx)  ? new_size : stor_blk_tbl_idx;
   new_size = (new_size > type_tbl_idx)  ? new_size : type_tbl_idx;

   new_size++;          /* Do not use entry 0, so increase size by 1 */

   CHECK_TBL_ALLOC_SIZE(pdg_link_tbl, new_size);
   pdg_link_tbl_idx = pdg_link_tbl_size - 1;
   pdg_tbl_base	= (long *) pdg_link_tbl;

   for (i = 0; i < (pdg_link_tbl_size * NUM_PDG_WDS); i++) {
       pdg_tbl_base[i] = 0;
   }

   new_size = type_tbl_idx + 1;

   CHECK_TBL_ALLOC_SIZE(pdg_type_tbl, new_size);
   pdg_type_tbl_idx = pdg_type_tbl_size - 1;
   pdg_tbl_base	= (long *) pdg_type_tbl;

   for (i = 0; i < (pdg_type_tbl_size * NUM_PDG_TYP_WDS); i++) {
       pdg_tbl_base[i] = 0;
   }


   TRACE (Func_Exit, "allocate_pdg_link_tbls", NULL);

   return;

}  /* allocate_pdg_link_tbls */



/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static void send_attr_ntry(int		attr_idx)

{
   char                 str[3000];
   int			len;
# if defined(_TARGET32)
   int			align			= HWord_Align;
# else
   int			align			= Word_Align;
# endif
   TYPE			basic;
   long64		bit_len			= 0;
   int			class			= 0;
   int			const_idx;

   /* JEFFL - This is being used as a target constant for make_io */

   long_type		constant[2];
   int			constant_class;
   long64		flag;
   int			i;
   int			il_idx;
   int			al_idx;
   int			defining_attr		= NULL_IDX;
   int			dv_alias		= NULL_IDX;
   int			intent			= Intent_Unseen;
   long			io_type_code_pdg_idx	= NULL_IDX;
   long64		offset			= 0;
   int			ptr_align		= 0;
   long			ptr_idx			= NULL_IDX;
   int			sb_idx			= NULL_IDX;
   int			child_idx		= NULL_IDX;
   size_offset_type	size;
   int			sn_idx;
   int			save_pdg_cn_const;
   boolean		save_sym_constant_expr;
   boolean		sym_offset		= FALSE;
   TYPE			type_idx;

# ifdef _TARGET_OS_MAX
   long_type    	ii;
# endif


   TRACE (Func_Entry, "send_attr_ntry", NULL);

   flag = (long64) 0;

   if (PDG_AT_IDX(attr_idx) != NULL_IDX) {
      goto EXIT;   /* already sent */
   }

   if (!AT_IGNORE_ATTR_LINK(attr_idx)) {

      if (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
         child_idx = attr_idx;

         while (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
            attr_idx = AT_ATTR_LINK(attr_idx);
         }

         if (PDG_AT_IDX(attr_idx) != NULL_IDX) {
            PDG_AT_IDX(child_idx) = PDG_AT_IDX(attr_idx);
            goto EXIT;   /* already sent */
         }
      }
   }


   switch (AT_OBJ_CLASS(attr_idx)) {
   case Data_Obj:

      switch (ATD_CLASS(attr_idx)) {

      case Variable:
# ifdef _TARGET_OS_MAX
         /* We do not need to call fei_object for this item. */
         if (ATD_SYMBOLIC_CONSTANT(attr_idx)) {
            goto EXIT;
         }
# endif

         if (ATD_FLD(attr_idx) != NO_Tbl_Idx) {

            /* Send associated data initialized temps. */

            if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {
               send_attr_ntry(ATD_VARIABLE_TMP_IDX(attr_idx));
            }
            else if (ATD_FLD(attr_idx) == IL_Tbl_Idx) {
               il_idx = ATD_VARIABLE_TMP_IDX(attr_idx);

               while (il_idx != NULL_IDX) {
                  send_attr_ntry(IL_IDX(il_idx)); 
                  il_idx = IL_NEXT_LIST_IDX(il_idx);
               }
            }
         }

         /* Intentional fall through */

      case Atd_Unknown:
         class = User_Variable;

         if (ATD_AUTOMATIC(attr_idx)) {
            send_attr_ntry(ATD_AUTO_BASE_IDX(attr_idx));
            offset = (long64) PDG_AT_IDX(ATD_AUTO_BASE_IDX(attr_idx));
         }
         break;

      case CRI__Pointee:
         class = CRI_Pointee;
         send_attr_ntry(ATD_PTR_IDX(attr_idx));
         ptr_idx = PDG_AT_IDX(ATD_PTR_IDX(attr_idx));
         break;

      case Function_Result:
         if (SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx)) == Formal) {

            /* The Function_Result has become the first dummy argument */

            intent = Intent_Out;
            class = Dummy_Arg;
            flag = flag | ((long64) 1 << FEI_OBJECT_RESULT_TEMP);
         }
         else { 
            class = Function_Rslt;
            flag = flag | ((long64) AT_HOST_ASSOCIATED(attr_idx) << 
                                                    FEI_OBJECT_RESULT_TEMP);
         }
         break;

      case Dummy_Argument:
# ifdef _NAME_SUBSTITUTION_INLINING
         if (!AT_IS_DARG(attr_idx)) {
            goto EXIT;
         }
# endif

         class = Dummy_Arg;
         flag = flag | ((long64) ATD_SF_DARG(attr_idx) << FEI_OBJECT_SF_DARG);
         flag = flag | ((long64) AT_OPTIONAL(attr_idx) << FEI_OBJECT_OPTIONAL);

         intent	= ATD_INTENT(attr_idx);
         if (intent == Intent_Unseen) {
            intent = Intent_Inout;
         }
         break;

      case Compiler_Tmp:
         if (ATD_SYMBOLIC_CONSTANT(attr_idx)) {

            /* This tmp is a placeholder for the symbolic constant. */

            save_sym_constant_expr = symbolic_constant_expr;
            symbolic_constant_expr = TRUE;

            if (ATD_FLD(attr_idx) == IR_Tbl_Idx &&
                IR_OPR(ATD_TMP_IDX(attr_idx)) == Asg_Opr) {
               cvrt_exp_to_pdg(IR_IDX_R(ATD_TMP_IDX(attr_idx)),
                               IR_FLD_R(ATD_TMP_IDX(attr_idx)));
            }
            else {
               cvrt_exp_to_pdg(ATD_TMP_IDX(attr_idx), 
                    (fld_type) ATD_FLD(attr_idx));
            }

            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_C("npex_end");
            PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
            PDG_AT_IDX(attr_idx) = npex_end();
# endif
            PDG_DBG_PRINT_START    
            PDG_DBG_PRINT_LD("(r) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
            PDG_DBG_PRINT_END    

            symbolic_constant_expr = save_sym_constant_expr;
            goto EXIT;
         }

         class = Compiler_Temp;

         if (ATD_AUTOMATIC(attr_idx)) {
            send_attr_ntry(ATD_AUTO_BASE_IDX(attr_idx));
            offset = (long64) PDG_AT_IDX(ATD_AUTO_BASE_IDX(attr_idx));
         }

# if ! (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)) && ! defined(_TARGET_OS_MAX)
         if (ATD_DEFINING_ATTR_IDX(attr_idx) != NULL_IDX) {
            send_attr_ntry(ATD_DEFINING_ATTR_IDX(attr_idx));
            defining_attr = PDG_AT_IDX(ATD_DEFINING_ATTR_IDX(attr_idx));
         }
# endif
         break;

      case Constant:
         if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {
            /* Aggregate Constant */
            /* change the tmp to a variable */

            ATD_CLASS(ATD_CONST_IDX(attr_idx)) = Variable;
            send_attr_ntry(ATD_CONST_IDX(attr_idx));
            ATD_CLASS(ATD_CONST_IDX(attr_idx)) = Compiler_Tmp;
            goto EXIT;
         }

         if (cmd_line_flags.debug_lvl > Debug_Lvl_2 &&
             !cmd_line_flags.dwarf_debug) {
            goto EXIT;
         }

         if (AT_COMPILER_GEND(attr_idx)) {
            goto EXIT;
         }

         /* Pass parameter information for DEBUG symbol tables.  They do not */
         /* need compiler generated constants.                               */

         const_idx = ATD_CONST_IDX(attr_idx);
         type_idx = get_basic_type(ATD_TYPE_IDX(attr_idx),
                                   ATD_ALIGNMENT(attr_idx),
                                   attr_idx);
         constant_class	= 1;

         if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
            constant_class = 3;
            bit_len = CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(const_idx)));
            bit_len = bit_len * CHAR_BIT;
         }
         else if (TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) == Long_Typeless) {
            constant_class = Pattern_Const;
            bit_len = TYP_BIT_LEN(CN_TYPE_IDX(const_idx));
         }

         if (PDG_CN_IDX(const_idx) == 0) {
            if (bit_len	== 0 && TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Character) {
               PDG_DBG_PRINT_START    
               PDG_DBG_PRINT_C("fei_arith_con");
               PDG_DBG_PRINT_T("(1) type", type_idx);
#if defined(_HOST32) && defined(_TARGET64)
               PDG_DBG_PRINT_VD("(2) const", CN_CONST(const_idx));
#else
               PDG_DBG_PRINT_LVD("(2) const", CN_CONST(const_idx));
#endif
               PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
# ifdef _TARGET_OS_MAX
               if (TYP_LINEAR(CN_TYPE_IDX(const_idx)) == Complex_4) {
                  /* pack the complex constant up into one word */
                  ii = CN_CONST(const_idx) << 32;
                  ii |= CP_CONSTANT(CN_POOL_IDX(const_idx) + 1);

                  PDG_CN_IDX(const_idx) = fei_arith_con(type_idx, (long *)&ii);
               }
               else {
                  PDG_CN_IDX(const_idx) = fei_arith_con(type_idx, 
                                                 (long *)&CN_CONST(const_idx));
               }
# else
               PDG_CN_IDX(const_idx) = fei_arith_con(type_idx,
                                                 (long *)&CN_CONST(const_idx));
# endif
# endif
               PDG_DBG_PRINT_START    
               PDG_DBG_PRINT_LD("(r) PDG_CN_IDX", PDG_CN_IDX(const_idx));
               PDG_DBG_PRINT_END    
            }
            else {
               PDG_DBG_PRINT_START
               PDG_DBG_PRINT_C("fei_pattern_con");
               PDG_DBG_PRINT_T("(1) type", type_idx);
#if defined(_HOST32) && defined(_TARGET64)
               PDG_DBG_PRINT_VD("(2) const", CN_CONST(const_idx));
#else
               PDG_DBG_PRINT_LVD("(2) const", CN_CONST(const_idx));
#endif
               PDG_DBG_PRINT_LLD("(3) bit length", bit_len);
               PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
               PDG_CN_IDX(const_idx) = fei_pattern_con(type_idx,
                                                   (char *)&CN_CONST(const_idx),
                                                    bit_len);
# endif
               PDG_DBG_PRINT_START    
               PDG_DBG_PRINT_LD("(r) PDG_CN_IDX", PDG_CN_IDX(const_idx));
               PDG_DBG_PRINT_END    
            }
            save_pdg_cn_const	= 0;
         }
         else {
            save_pdg_cn_const	= PDG_CN_IDX(const_idx);
         }
    

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_smt_parameter");
         PDG_DBG_PRINT_S("(1) AT_OBJ_NAME", AT_OBJ_NAME_PTR(attr_idx));
         PDG_DBG_PRINT_T("(2) type", type_idx);
         PDG_DBG_PRINT_LD("(3) PDG_CN_IDX", PDG_CN_IDX(const_idx));
         PDG_DBG_PRINT_D("(4) constant_class", constant_class);
         PDG_DBG_PRINT_D("(5) line num", AT_DEF_LINE(attr_idx));
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         fei_smt_parameter(AT_OBJ_NAME_PTR(attr_idx),
                           type_idx,
                           PDG_CN_IDX(const_idx),
                           constant_class,
                           AT_DEF_LINE(attr_idx));
# endif

         if (cmd_line_flags.dwarf_debug) {
            PDG_CN_IDX(const_idx) = save_pdg_cn_const;
         }
         goto EXIT;

      case Struct_Component:
         class = Component;
         offset = CN_INT_TO_C(ATD_CPNT_OFFSET_IDX(attr_idx));  
         break;

      }  /* End switch */

      if (ATD_OFFSET_ASSIGNED(attr_idx) && 
          ATD_CLASS(attr_idx) != Struct_Component) {

         if (ATD_OFFSET_FLD(attr_idx) == CN_Tbl_Idx) {
            offset = CN_INT_TO_C(ATD_OFFSET_IDX(attr_idx));  

# if defined(_TARGET_OS_MAX) || defined(_HOST32)  /* JEFFL ?? */
# ifdef KEY
            if (TYP_LINEAR(CN_TYPE_IDX(ATD_OFFSET_IDX(attr_idx))) == Integer_4 && offset != 0){
              offset = (unsigned)offset;
            }
            else if (TYP_LINEAR(CN_TYPE_IDX(ATD_OFFSET_IDX(attr_idx))) != Integer_8){
              SIGN_EXTEND(offset);
            }
# else
            if (TYP_LINEAR(CN_TYPE_IDX(ATD_OFFSET_IDX(attr_idx))) != Integer_8){
              SIGN_EXTEND(offset);
            }
# endif
# endif
         }
         else if (ATD_OFFSET_FLD(attr_idx) == AT_Tbl_Idx) {
            send_attr_ntry(ATD_OFFSET_IDX(attr_idx));
            sym_offset = TRUE;
            offset = (long64) PDG_AT_IDX(ATD_OFFSET_IDX(attr_idx));
         }
         else {
            PRINTMSG(AT_DEF_LINE(attr_idx), 1213, Internal,
                     AT_DEF_COLUMN(attr_idx),
                     "ATD_OFFSET_IDX", AT_OBJ_NAME_PTR(attr_idx));
         }
      }

      /* 1 element */
      size = stor_bit_size_of(attr_idx, FALSE, FALSE);

      if (size.fld == CN_Tbl_Idx) {
         bit_len = CN_INT_TO_C(size.idx);
      }
      else {
         bit_len = F_INT_TO_C(size.constant, TYP_LINEAR(size.type_idx));
      }

      sb_idx = ATD_STOR_BLK_IDX(attr_idx);

      if (SB_BLK_TYPE(sb_idx) == Based && 
          child_idx != NULL_IDX &&
          !ATD_AUTOMATIC(attr_idx)) {
         sb_idx	= ATD_STOR_BLK_IDX(child_idx);
      }

      send_stor_blk(sb_idx, &class);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         if (ATD_CLASS(attr_idx) == Compiler_Tmp) {
            if (sb_idx != NULL_IDX &&
                SB_SCP_IDX(sb_idx) != NULL_IDX &&
                SB_SCP_IDX(sb_idx) != curr_scp_idx &&
               (!SB_HOSTED_STATIC(sb_idx))) {
               class = Hosted_Compiler_Temp;
            }
         }
# endif

      type_idx = get_type_desc(attr_idx);

      if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ptr) {
         ptr_align = Word_Align;

# if defined(_TARGET32)
         if (TYP_PTR_INCREMENT(ATD_TYPE_IDX(attr_idx)) == TARGET_BITS_PER_WORD){
	   ptr_align   = HWord_Align;   /* 32 */
         }
# endif
      }
      else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ch_Ptr) {
         ptr_align = Byte_Align;
      }
      else {
         ptr_align = 0;

         if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {

            if (ATD_IN_COMMON(attr_idx) || 
                ATD_CLASS(attr_idx) == Struct_Component ||
                ATD_CLASS(attr_idx) == Function_Result) {
               align = Byte_Align;
            }
         }
         else if (ATD_CLASS(attr_idx) == Function_Result &&
                  TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure) {
            align = (ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(attr_idx)))) ?
                                          Byte_Align : Word_Align;
         }
         else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure) {
            align = pdg_align[ATT_ALIGNMENT(TYP_IDX(ATD_TYPE_IDX(attr_idx)))];
         }
# if defined(_ALIGN_REAL16_TO_16_BYTES)

         else if (TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) == Complex_16 ||
                  TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) == Real_16) {
            align = FWord_Align;
         }
# endif

# if defined(_INTEGER_1_AND_2)
         else if (on_off_flags.integer_1_and_2 &&
                  PACK_8_BIT_TEST_CONDITION(ATD_TYPE_IDX(attr_idx))) {
            align = 8;  /* Byte_Align */
         }
         else if (on_off_flags.integer_1_and_2 &&
                  PACK_16_BIT_TEST_CONDITION(ATD_TYPE_IDX(attr_idx))) {
            align = 16; /* Parcel_Align */
         }
# endif

# if defined(_TARGET_PACK_HALF_WORD_TYPES)

         /* Complex_4 does not go here because we want it aligned on a 64 bit */
         /* boundary for speed.                                               */

         else if (PACK_HALF_WORD_TEST_CONDITION(ATD_TYPE_IDX(attr_idx))) {
            align = HWord_Align;
         }
# endif
      }

      if (ATD_CLASS(attr_idx) == Variable  ||
          ATD_CLASS(attr_idx) == Dummy_Argument ||
          ATD_CLASS(attr_idx) == Function_Result ||
          ATD_CLASS(attr_idx) == Struct_Component ||
          ATD_CLASS(attr_idx) == Compiler_Tmp ||
          ATD_CLASS(attr_idx) == CRI__Pointee) {

         make_io_type_code(ATD_TYPE_IDX(attr_idx), constant);
         basic = get_basic_type(IO_TYPE_CODE_TYPE,
                                type_alignment_tbl[IO_TYPE_CODE_TYPE],
                                NULL_IDX);

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_arith_con");
         PDG_DBG_PRINT_T("(1) type", basic);
#if defined(_HOST32) && defined(_TARGET64)
         PDG_DBG_PRINT_VD("(2) value", constant[0]);
#else
         PDG_DBG_PRINT_LVD("(2) value", constant[0]);
#endif
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         io_type_code_pdg_idx = fei_arith_con(basic, 
                                              (long *)constant);
# endif
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_LD("(r) PDG_CN_IDX", io_type_code_pdg_idx);
         PDG_DBG_PRINT_END    
      }

      if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
         switch (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx))) {
         case Assumed_Size:
            flag = flag | ((long64) 1 << FEI_OBJECT_ASSUMED_SIZE);
            break;

         case Assumed_Shape:
            flag = flag | ((long64) 1 << FEI_OBJECT_ASSUMD_SHAPE);
            break;
         }
      }

# ifdef _TARGET_OS_MAX
      if ((ATD_CLASS(attr_idx) != Compiler_Tmp &&
           ATD_CACHE_ALIGN(attr_idx)) ||
          (ATD_STOR_BLK_IDX(attr_idx) != NULL_IDX &&
           SB_CACHE_ALIGN(ATD_STOR_BLK_IDX(attr_idx)))) {

         flag = flag | ((long64) 1 << FEI_OBJECT_CACHE_ALIGN);
      }
# endif

      if (ATD_IM_A_DOPE(attr_idx) &&
          ATD_DV_ALIAS(attr_idx) != NULL_IDX) {
         send_attr_ntry(AL_ATTR_IDX(ATD_DV_ALIAS(attr_idx)));
         dv_alias = PDG_AT_IDX(AL_ATTR_IDX(ATD_DV_ALIAS(attr_idx))); 

         al_idx = AL_NEXT_IDX(ATD_DV_ALIAS(attr_idx));
         if (al_idx != NULL_IDX) {
            send_attr_ntry(AL_ATTR_IDX(al_idx));
            ptr_idx = PDG_AT_IDX(AL_ATTR_IDX(al_idx)); 
         }
      }

      if (SB_DEF_MULT_SCPS(sb_idx)) {
         ATD_EQUIV(attr_idx) = TRUE;   /* this is set by inlining */
      }

      flag = (flag |
      ((long64) ATD_TARGET(attr_idx) 		<< FEI_OBJECT_TARGET) |
      ((long64) ATD_EQUIV(attr_idx)		<< FEI_OBJECT_EQUIV) |
      ((long64) ATD_SAVED(attr_idx)		<< FEI_OBJECT_SAVED) |
      ((long64) ATD_ALLOCATABLE(attr_idx) 	<< FEI_OBJECT_ALLOCATE) |
      ((long64) ATD_OFFSET_ASSIGNED(attr_idx)	<< FEI_OBJECT_OFF_ASSIGNED) |
      ((long64) sym_offset 			<< FEI_OBJECT_SYMCON_OFFST) |
      ((long64) ATD_AUXILIARY(attr_idx)		<< FEI_OBJECT_AUXILIARY) |
      ((long64) ATD_DATA_INIT(attr_idx)		<< FEI_OBJECT_INITIALD) |
      ((long64) AT_DEFINED(attr_idx) 		<< FEI_OBJECT_DEFINED) |
      ((long64) AT_REF_IN_CHILD(attr_idx) 	<< FEI_OBJECT_INNER_REF) |
      ((long64) AT_DEF_IN_CHILD(attr_idx) 	<< FEI_OBJECT_INNER_DEF) |
      ((long64) ATD_POINTER(attr_idx) 		<< FEI_OBJECT_DV_IS_PTR) |
      ((long64) ATD_IM_A_DOPE(attr_idx)		<< FEI_OBJECT_IS_DOPE_VEC) |
      ((long64) ATD_IN_COMMON(attr_idx)		<< FEI_OBJECT_IN_COMMON) |
      ((long64) ATD_PERMUTATION(attr_idx) 	<< FEI_OBJECT_PERMUTATION) |
      ((long64) ATD_SYMMETRIC(attr_idx)		<< FEI_OBJECT_SYMMETRIC) |
      ((long64) ATD_PTR_ASSIGNED(attr_idx)	<< FEI_OBJECT_PTR_ASSIGNED) |
      ((long64) AT_ACTUAL_ARG(attr_idx) 	<< FEI_OBJECT_ACTUAL_ARG) |
      ((long64) ATD_READ_ONLY_VAR(attr_idx) 	<< FEI_OBJECT_READ_ONLY) |
      ((long64) ATD_NOT_PT_UNIQUE_MEM(attr_idx) 	
                                      << FEI_OBJECT_NOT_PT_TO_UNIQUE_MEM) |
#ifdef KEY /* Bug 14150 */
      ((long64) (ATD_CLASS(attr_idx) == Dummy_Argument &&
        ATD_VALUE_ATTR(attr_idx)) 		<< FEI_OBJECT_PASS_BY_VALUE) |
#endif /* KEY Bug 14150 */
      ((long64) AT_NAMELIST_OBJ(attr_idx) 	<< FEI_OBJECT_NAMELIST_ITEM));


      if (ATD_CLASS(attr_idx) == Compiler_Tmp ||
          ATD_CLASS(attr_idx) == Variable) {
         flag = flag | ((long64) 1 << FEI_OBJECT_ADDRTAKEN);
      }

      if (ATD_TASK_PRIVATE(attr_idx) && SB_BLK_TYPE(sb_idx) != Stack) {
         flag |= 1 << FEI_SEG_THREADPRIVATE;
      }

      if (class == Component) {
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_member");
         PDG_DBG_PRINT_S("(1) AT_OBJ_NAME", AT_OBJ_NAME_PTR(attr_idx));
         PDG_DBG_PRINT_T("(2) type", type_idx);
         PDG_DBG_PRINT_LLD("(3) offset", offset);
         PDG_DBG_PRINT_LLD("(4) bit_len", bit_len);
         PDG_DBG_PRINT_D("(5) storage align", align);
         PDG_DBG_PRINT_D("(6) AT_DEF_LINE", AT_DEF_LINE(attr_idx));
         PDG_DBG_PRINT_LLO("(7) flags", flag);
         PDG_DBG_PRINT_LD("(8) io descriptor", io_type_code_pdg_idx);
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         PDG_AT_IDX(attr_idx) = fei_member(AT_OBJ_NAME_PTR(attr_idx),
                                           type_idx,
                                           offset,
                                           bit_len,
                                           align,  
                                           AT_DEF_LINE(attr_idx), 
                                           flag,
                                           io_type_code_pdg_idx);
# endif
      }
      else {

# ifdef _DEBUG

         if (dv_alias != NULL_IDX && defining_attr != NULL_IDX) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 626, Internal, 
                     AT_DEF_COLUMN(attr_idx),
                     "dv_alias or defining_attr",
                     "send_attr_ntry");
         }
# endif

# if ! (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)) && ! defined(_TARGET_OS_MAX)
         if (dv_alias == NULL_IDX) {
            dv_alias = defining_attr;  /* These two are overlayed */
         }
# endif

         /* Null terminate the CDIR$ ID string */
         if (class == Name && ATD_TMP_IDX(attr_idx) != NULL_IDX) {
            len = strlen((char *)&CN_CONST(ATD_TMP_IDX(attr_idx)));
# ifdef _DEBUG
            if (len > 3000) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 626, Internal, 
                        AT_DEF_COLUMN(attr_idx),
                        "a valid length",
                        "send_attr_ntry");
            }
# endif
            strcpy(str, (char *)&CN_CONST(ATD_TMP_IDX(attr_idx)) );
            len = CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(ATD_TMP_IDX(attr_idx))));
# ifdef _DEBUG
            if (len > 3000) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 626, Internal, 
                        AT_DEF_COLUMN(attr_idx),
                        "a valid length",
                        "send_attr_ntry");
            }
# endif
            str[len] = NULL_CHAR;
         }

         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_object");
         PDG_DBG_PRINT_S("(1) AT_OBJ_NAME", AT_OBJ_NAME_PTR(attr_idx));
         PDG_DBG_PRINT_T("(2) type", type_idx);
         PDG_DBG_PRINT_LLO("(3) flags", flag);
         PDG_DBG_PRINT_S("(4) symbol class", p_obj_sym_class[class]);
         PDG_DBG_PRINT_S("(5) PDG_SB_IDX", SB_NAME_PTR(sb_idx));
         PDG_DBG_PRINT_D("(6) dv_alias", dv_alias);
         PDG_DBG_PRINT_LD("(7) ptr_idx", ptr_idx);
         PDG_DBG_PRINT_LLD("(8) offset", offset);
         PDG_DBG_PRINT_S("(9) intent", p_intent[intent]);
         PDG_DBG_PRINT_LLD("(10) bit_len", bit_len);
         PDG_DBG_PRINT_D("(11) pointee align", ptr_align);
         PDG_DBG_PRINT_D("(12) storage align", align);
         PDG_DBG_PRINT_D("(13) unused", 0);
         PDG_DBG_PRINT_LD("(14) io descriptor", io_type_code_pdg_idx);
         PDG_DBG_PRINT_D("(15) unused", 0);
         PDG_DBG_PRINT_D("(16) AT_DEF_LINE", AT_DEF_LINE(attr_idx));
         PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
         PDG_AT_IDX(attr_idx) = fei_object(class == Name ? 
                                           str : AT_OBJ_NAME_PTR(attr_idx),
                                           type_idx,
                                           flag,
                                           class,
                                           PDG_SB_IDX(sb_idx),
                                           dv_alias,
                                           ptr_idx,
                                           offset,
                              	           intent,
                                           bit_len,
                                           ptr_align,
                                           align,
                                           0,
                                           io_type_code_pdg_idx,
                                           0, 
                                           AT_DEF_LINE(attr_idx));
# endif
      }

      PDG_DBG_PRINT_START    
      PDG_DBG_PRINT_LD("(r) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
      PDG_DBG_PRINT_END    
      break;

   case Pgm_Unit:
      if (PDG_AT_IDX(attr_idx) == NULL_IDX) {
         /* Send procedure decides what to send over and what not to send. */
         send_procedure(attr_idx, 
                        NULL_IDX,  /* No alternate entries for Imported calls */
                        Imported);
      }
      break;

   case Label:
      send_label(attr_idx);
      break;

   case Interface:
      if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Module) {

         /* This is an interface block that has the same name as one of */
         /* its program units.  The program unit has to go through the  */
         /* interface.                                                  */

         if (ATI_PROC_IDX(attr_idx) != NULL_IDX &&
             PDG_AT_IDX(ATI_PROC_IDX(attr_idx)) == NULL_IDX) {
            send_procedure(ATI_PROC_IDX(attr_idx),
                           NULL_IDX,
                           Imported);
         }

         /* Need to send generic - explicit names if generating debug tbls */

         if (cmd_line_flags.debug_lvl <= Debug_Lvl_2) {  
            sn_idx = ATI_FIRST_SPECIFIC_IDX(attr_idx);

            for (i = 0; i < ATI_NUM_SPECIFICS(attr_idx); i++) {
               if (AT_REFERENCED(SN_ATTR_IDX(sn_idx)) != Not_Referenced) {
                  if (PDG_AT_IDX(SN_ATTR_IDX(sn_idx)) == NULL_IDX) {
                     send_procedure(SN_ATTR_IDX(sn_idx),
                                    NULL_IDX,
                                    Imported);
                  }

                  PDG_DBG_PRINT_START    
                  PDG_DBG_PRINT_C("fei_smt_actual_to_generic");
                  PDG_DBG_PRINT_LD("(1) PDG_AT_IDX",
                                PDG_AT_IDX(SN_ATTR_IDX(sn_idx)));
                  PDG_DBG_PRINT_S("(2) AT_OBJ_NAME", 
                                    AT_OBJ_NAME_PTR(attr_idx));
                  PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
                  fei_smt_actual_to_generic(PDG_AT_IDX(SN_ATTR_IDX(sn_idx)),
                                            AT_OBJ_NAME_PTR(attr_idx));
# endif
               }
               sn_idx = SN_SIBLING_LINK(sn_idx);
            }
         }
      }
      break;

   case Derived_Type:
      /* These will be sent across when they are referenced. */
      goto EXIT;
               
   case Namelist_Grp:
      if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Module) {
         send_namelist_group(attr_idx);
      }
      break;

   }  /* End switch */

   if (child_idx != NULL_IDX) {
      PDG_AT_IDX(child_idx) = PDG_AT_IDX(attr_idx);
   }

EXIT:

   TRACE (Func_Exit, "send_attr_ntry", NULL);

   return;

}  /* send_attr_ntry */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This does initialization, calls the routines to pass the symbol       *|
|*	and the IR to PDG.  It then does cleanup.                             *|
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
static void send_mod_file_name (void)

{
   int		fp_idx;

#  if defined(_TARGET_OS_MAX) || defined(_TARGET_OS_UNICOS)
   long		opt_flag;
   int		pgm_code	= 0;
   int		pgm_data	= 0;
#  endif


   TRACE (Func_Entry, "send_mod_file_name", NULL);

   fp_idx = SCP_FILE_PATH_IDX(MAIN_SCP_IDX);

   if (!FP_OUTPUT_TO_O(fp_idx)) {
      return;
   }


#  if defined(_TARGET_OS_MAX) || defined(_TARGET_OS_UNICOS)

   CREATE_ID(TOKEN_ID(token), "@%%", 3);
   COPY_ATTR_NTRY(AT_WORK_IDX, SCP_ATTR_IDX(MAIN_SCP_IDX));

   if (ATP_PGM_UNIT(AT_WORK_IDX) != Module) {  /* Inlinable situation */
      CLEAR_VARIANT_ATTR_INFO(AT_WORK_IDX, Pgm_Unit);
      ATP_PGM_UNIT(AT_WORK_IDX) = Module;
   }

   AT_CIF_SYMBOL_ID(AT_WORK_IDX) = 0;
   CLEAR_TBL_NTRY(name_pool, NULL_IDX);

   name_pool[NULL_IDX].name_long = TOKEN_STR_WD(token, 0);
   AT_NAME_IDX(AT_WORK_IDX) = NULL_IDX;
   AT_NAME_LEN(AT_WORK_IDX) = 3;
   ATP_EXT_NAME_IDX(AT_WORK_IDX) = NULL_IDX;
   ATP_EXT_NAME_LEN(AT_WORK_IDX) = 3;

   PDG_DBG_PRINT_START    
   PDG_DBG_PRINT_C("fei_next_func_idx");
   PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
   PDG_AT_IDX(AT_WORK_IDX) = fei_next_func_idx(ATP_PGM_UNIT(AT_WORK_IDX),
                                               ATP_PROC(AT_WORK_IDX),
                                               ATP_ALT_ENTRY(AT_WORK_IDX));
# endif

   send_procedure(AT_WORK_IDX,
                  NULL_IDX,		/* No alt entries or parent procs */
                  Definition);

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("PDGCS_comp_unit");
   PDG_DBG_PRINT_S("(1) AT_OBJ_NAME", AT_OBJ_NAME_PTR(AT_WORK_IDX));
   PDG_DBG_PRINT_D("(2) unused", 0);
   PDG_DBG_PRINT_END

   PDGCS_comp_unit(AT_OBJ_NAME_PTR(AT_WORK_IDX), NULL_IDX);

   opt_flag = ((long) opt_flags.aggress	<< PDGCS_NEW_PROC_IGNORE_THROTTLE)|
              ((long) opt_flags.pattern	<< PDGCS_NEW_PROC_PATTERN_MATCHING)|
              ((long) (cmd_line_flags.debug_lvl != Debug_Lvl_0)
					<< PDGCS_NEW_PROC_ELIM_DEAD_CODE)|
              ((long) on_off_flags.reciprical_divide	
  					<< PDGCS_NEW_PROC_IEEE_RECIPS)|
              ((long) cmd_line_flags.runtime_conformance
					<< PDGCS_NEW_PROC_CONFORM_CHECK)|
              ((long) (opt_flags.task_lvl == Task_Lvl_3)
					<< PDGCS_NEW_PROC_TASK_INNER_LOOPS)|
              ((long) cmd_line_flags.do_UDB_checks
					<< PDGCS_NEW_PROC_DO_UBD_ANALYSIS);

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("PDGCS_new_proc");
   PDG_DBG_PRINT_D("(1) num ir", 100);
   PDG_DBG_PRINT_D("(2) PDG_AT_IDX", PDG_AT_IDX(AT_WORK_IDX));
   PDG_DBG_PRINT_D("(3) alt entries", 0);
   PDG_DBG_PRINT_D("(4) scalar level", opt_flags.scalar_lvl);
   PDG_DBG_PRINT_D("(5) vector level", opt_flags.vector_lvl);
   PDG_DBG_PRINT_D("(6) task level", opt_flags.task_lvl);
   PDG_DBG_PRINT_O("(7) flags", opt_flag);
   PDG_DBG_PRINT_D("(8) user mobes", 0);
   PDG_DBG_PRINT_D("(9) user sades", 0);
   PDG_DBG_PRINT_D("(10) pipeline level", opt_flags.pipeline_lvl);
   PDG_DBG_PRINT_D("(11) stream level", opt_flags.stream_lvl);
   PDG_DBG_PRINT_END

   PDGCS_new_proc(100,
                  PDG_AT_IDX(AT_WORK_IDX),
                  0,
                  opt_flags.scalar_lvl,
                  opt_flags.vector_lvl,
                  opt_flags.task_lvl,
                  opt_flag,
                  0,
                  0,
                  opt_flags.pipeline_lvl, 
                  opt_flags.stream_lvl);

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("PDGCS_do_proc");

# ifdef _DEBUG
   fflush(debug_file);
# endif

   PDG_DBG_PRINT_END

#ifdef KEY /* Bug 3507 */
   PDGCS_do_proc(1);
#else /* KEY Bug 3507 */
   PDGCS_do_proc();
#endif /* KEY Bug 3507 */
  
   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("PDGCS_end_procs");
   PDG_DBG_PRINT_D("(1) pgm code", pgm_code);
   PDG_DBG_PRINT_D("(2) pgm data", pgm_data);
   PDG_DBG_PRINT_END

   PDGCS_end_procs(&pgm_code,
                   &pgm_data);

   create_mod_info_tbl();
   output_mod_info_file();

# endif

   if (fp_idx != NULL_IDX) {
      PDG_DBG_PRINT_START
      PDG_DBG_PRINT_C("fei_module_file");
      PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
      fei_module_file(FP_NAME_PTR(fp_idx));
# endif
   }

# if defined(_TARGET_OS_MAX) || defined(_TARGET_OS_UNICOS)

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("PDGCS_end_comp_unit");
   PDG_DBG_PRINT_END

   PDGCS_end_comp_unit();

# endif

   TRACE (Func_Exit, "send_mod_file_name", NULL);

   return;

}  /* send_mod_file_name */



/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*    Terminate the interface.                                                *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*              none                                                          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*              none                                                          *|
|*                                                                            *|
|* Returns:     nothing                                                       *|
|*                                                                            *|
\******************************************************************************/

void terminate_PDGCS(void)

{

   TRACE (Func_Entry, "terminate_PDGCS", NULL);

   if (binary_output || assembly_output) {
      PDG_DBG_PRINT_START    
      PDG_DBG_PRINT_C("PDGCS_terminate");
      PDG_DBG_PRINT_END    

# ifdef _ENABLE_FEI
      PDGCS_terminate();
# endif
   }

   TRACE (Func_Exit, "terminate_PDGCS", NULL);

   return;

}  /* terminate_PDGCS */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	send_darg_list sends the dummy argument list for the given program.   *|
|*									      *|
|* Input parameters:							      *|
|*	pgm_attr_idx	   -> Index of attr entry for program unit, whose     *|
|*			      dummy args need to be converted.		      *|
|*	darg_pdg_sn_fw_idx -> PDG secondary name table index where the first  *|
|*			      dummy arg should go.                            *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void  send_darg_list(int		pgm_attr_idx,
                            int		darg_pdg_sn_fw_idx)

{
   int		attr_idx;
   int		darg_idx;
   int		end_idx;
   long		prev_idx;
   int		sn_idx;


   TRACE (Func_Entry, "send_darg_list", NULL);

   prev_idx = NULL_IDX;
   darg_idx = darg_pdg_sn_fw_idx;
   end_idx = ATP_NUM_DARGS(pgm_attr_idx) + ATP_FIRST_IDX(pgm_attr_idx);
   sn_idx = ATP_FIRST_IDX(pgm_attr_idx);

   do {
      attr_idx = SN_ATTR_IDX(sn_idx);

      if (PDG_AT_IDX(attr_idx) == NULL_IDX) {
         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            send_attr_ntry(attr_idx);
         }
         else {
            send_dummy_procedure(attr_idx);
         }
      }

      PDG_DBG_PRINT_START    
      PDG_DBG_PRINT_C("fei_name");
      PDG_DBG_PRINT_S("(1) AT_OBJ_NAME", AT_OBJ_NAME_PTR(attr_idx));
      PDG_DBG_PRINT_S("(2) table", "PDGCS_Object");
      PDG_DBG_PRINT_LD("(3) PDG_AT_IDX", PDG_AT_IDX(attr_idx));
      PDG_DBG_PRINT_D("(4) prev_idx", prev_idx);
      PDG_DBG_PRINT_D("(5) darg_idx", darg_idx);
      PDG_DBG_PRINT_END

      /* This sets prev_idx to the index being used on this call */

# ifdef _ENABLE_FEI
      prev_idx = fei_name(AT_OBJ_NAME_PTR(attr_idx),
                          Sym_Object,
                          PDG_AT_IDX(attr_idx),
                          prev_idx,
                          darg_idx);
# endif

      if (++sn_idx < end_idx)  {
         PDG_DBG_PRINT_START    
         PDG_DBG_PRINT_C("fei_next_name");
         PDG_DBG_PRINT_D("(1) logical", TRUE);
         PDG_DBG_PRINT_END

# ifdef _ENABLE_FEI
         darg_idx = fei_next_name(TRUE);
# endif
      }
   }
   while (sn_idx != end_idx);

   TRACE (Func_Exit, "send_darg_list", NULL);

   return;

}   /* send_darg_list */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	On IRIX systems, the alignment can differ for basic types.            *|
|*	For example if -align32 is specified, some of the real*8 types will   *|
|*	start on 32 bit boundaries rather than 64 bit boundaries.  When this  *|
|*	happens, a different type descriptor needs to get send across with    *|
|*	the alternative alignment.                                            *|
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
static	TYPE	send_non_standard_aligned_type(int		align,
					       int		type_idx)
{
#ifdef KEY /* Bug 10177 */
   int		aux_info = 0;
   int		basic_type = 0;
   int      	bit_size = 0;
#else /* KEY Bug 10177 */
   int		aux_info;
   int		basic_type;
   int      	bit_size;
#endif /* KEY Bug 10177 */
   int      	flags 		= 0;
   TYPE		pdg_type_idx;


   TRACE (Func_Entry, "send_non_standard_aligned_type", NULL);

   switch (TYP_LINEAR(type_idx)) {

   case Integer_1:
      bit_size		= bit_size_tbl[Integer_1];
      basic_type	= Integral;
      aux_info		= 1;
      flags   		= 2;
      break;
   
   case Integer_2:
      bit_size		= bit_size_tbl[Integer_2];
      basic_type	= Integral;
      aux_info		= 2;
      flags   		= 2;
      break;

   case Integer_4:
      bit_size		= bit_size_tbl[Integer_4];
      basic_type	= Integral;
      aux_info		= 4;
      flags   		= 2;
      break;

   case Integer_8:
      bit_size		= bit_size_tbl[Integer_8];
      basic_type	= Integral;
      aux_info		= 8;
      flags   		= 2;

# ifdef _TARGET_HAS_FAST_INTEGER
         if (opt_flags.set_allfastint_option ||
             (opt_flags.set_fastint_option &&
              TYP_DESC(type_idx) == Default_Typed)) {
            bit_size 	= 46;
            aux_info 	= 6;
         }
# endif
      break;

   case Logical_1:
      bit_size		= bit_size_tbl[Logical_1];
      basic_type	= L_ogical;
      aux_info		= 1;
      break;

   case Logical_2:
      bit_size		= bit_size_tbl[Logical_2];
      basic_type	= L_ogical;
      aux_info		= 2;
      break;
   
   case Logical_4:
      bit_size		= bit_size_tbl[Logical_4];
      basic_type	= L_ogical;
      aux_info		= 4;
      break;
   
   case Logical_8:
      bit_size		= bit_size_tbl[Logical_8];
      basic_type	= L_ogical;
      aux_info		= 8;
      break;
   
   case Real_4:
      bit_size		= bit_size_tbl[Real_4];
      basic_type	= Floating_Pt;
      aux_info		= 4;
      break;
   
   case Real_8:
      bit_size		= bit_size_tbl[Real_8];
      basic_type	= Floating_Pt;
      aux_info		= 8;
      break;

   case Real_16:
      bit_size		= bit_size_tbl[Real_16];
      basic_type	= Floating_Pt;
      aux_info		= 16;
      break;
   
   case Complex_4:
      bit_size		= bit_size_tbl[Complex_4];
      basic_type	= C_omplex;
      aux_info		= 4;
      break;
   
   case Complex_8:
      bit_size		= bit_size_tbl[Complex_8];
      basic_type	= C_omplex;
      aux_info		= 8;
      break;
   
   case Complex_16:
      bit_size		= bit_size_tbl[Complex_16];
      basic_type	= C_omplex;
      aux_info		= 16;
      break;
   
   case CRI_Ch_Ptr_8:
      bit_size		= bit_size_tbl[CRI_Ch_Ptr_8];
      basic_type	= CRI_Pointer_Char;
      aux_info		= 0;
      break;

   default:
      PRINTMSG(stmt_start_line, 179, Internal, stmt_start_col,
               "send_non_standard_aligned_type");
      break;
   }

   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_C("fei_descriptor");
   PDG_DBG_PRINT_O("(1) flags", flags);
   PDG_DBG_PRINT_S("(2) table type", p_table_type[Basic]);
   PDG_DBG_PRINT_D("(3) bit_size", bit_size);
   PDG_DBG_PRINT_S("(4) basic type", p_basic_type[basic_type]);
   PDG_DBG_PRINT_D("(5) aux info", aux_info);
   PDG_DBG_PRINT_D("(6) alignment", pdg_align[align]);
   PDG_DBG_PRINT_END
# ifdef _ENABLE_FEI
   pdg_type_idx = fei_descriptor(flags,
                                 Basic,
                                 bit_size,
                                 basic_type,
                                 aux_info,
                                 pdg_align[align]);
# endif
   PDG_DBG_PRINT_START
   PDG_DBG_PRINT_T("(r) type", pdg_type_idx);
   PDG_DBG_PRINT_END

   TRACE (Func_Exit, "send_non_standard_aligned_type", NULL);
   
   return(pdg_type_idx);
   
}  /* send_non_standard_aligned_type */

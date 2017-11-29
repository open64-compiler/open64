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



static char USMID[] = "\n@(#)5.0_pl/sources/cmd_line.c	5.16	10/20/99 17:17:46\n";
#ifdef KEY /* Bug 5089 */
#include <libgen.h>
#endif /* KEY Bug 5089 */
# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "src_input.m"
# include "cmd_line.m"
# include "debug.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "cmd_line.h"
# include "defs.h"
# include "../sgi/cwh_targ_info.h"

/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

extern  char                    release_level[];
extern  char                    frontend_version[];


static	void dump_help_screen (void);
static	void init_cmd_line    (void);
static	void process_A_option (char *);
static	void process_a_option (char *);
static	void process_b_option (char *);
static	void process_C_option (char *, char *[]);
static	void process_d_option (char *);
static	void process_e_option (char *);
static	void process_f_option (char *);
static	void process_G_option (char *);
static	void process_i_option (char *);
static	void process_J_option (char *);
static	void process_k_option (char *);
static	void process_m_option (char *);
static	void process_M_option (char *);
static	void process_N_option (char *);
static	void process_O_option (char *, int);
static	void add_to_fp_table  (char *, int *, int);
static	void process_P_option (char *);
static	void process_q_option (char *);
static	void process_r_option (char *);
static	void process_R_option (char *);
static	void process_s_option (char *);
static	void process_S_option (char *);
static	void process_t_option (char *);
static	void process_u_option (char *);
static	void process_v_option (char *);
static	void process_x_option (char *);
static	void process_X_option (char *);
static	void process_Y_option (char *);
static	void set_prog_file_names (char *argv[]);
static	void validate_O_option (void);
static	void validate_G_option (void);
static	void validate_R_option (void);
static	void validate_s_option (void);
static	void process_D_option(char *);
static	void process_U_option(char *);
extern	void process_v_dbg_flags(char *);
static	void set_system_module_path( void );
static	void process_reshape_array(char *);
static  void dump_options(void);

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process_cmd_line is called to accept and validate command line	      *|
|*	options and arguments to change compiler flags.			      *|
|*									      *|
|* Input parameters:							      *|
|*	argc			number of command line arguments	      *|
|*	argv			argument strings			      *|
|*	nlspath			value of getenv(NLSPATH)		      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
  
#ifdef KEY /* Bug 5089 */
void	process_cmd_line(int   argc, char *argv[], char *nlspath)
#else /* KEY Bug 5089 */
void	process_cmd_line(int   argc, char *argv[])
#endif /* KEY Bug 5089 */
{
   char		err_char;
   int		err_ind;
   extern char *optarg;					/* defined in getopt  */
   extern int	opterr;					/* defined in getopt  */
   extern int	optind;					/* defined in getopt  */
   extern int	optopt;					/* defined in getopt  */
   int		option;
   int		i;

   int		save_argc;


/******************************************************************************/
/* 									      */
/* NOTE:								      */
/*    To add a new commandline option to the compiler, you must add the letter*/
/*    to opt_string.  Any letter you add must go in the list alphabetically.  */
/*    Those that have args must have a colon follwing.  Those that do not have*/
/*    args must NOT have a colon following.  Currently they are -g, -V and -Z */
/* 									      */
/******************************************************************************/

   char	*opt_string="a:b:d:e:f:ghi:k:m:p:q:r:s:t:u:v:x:"
                    "A:C:D:FG:I:J:M:N:O:P:R:S:T:U:Y:X:VZ";

   char	 str[7];


   TRACE (Func_Entry, "process_cmd_line", NULL);

   save_argc = argc;

   opterr = 0;						/* disable lib msgs   */

   init_cmd_line ();

# ifdef _DEBUG
# ifdef _ENABLE_FEI
   str[0] = 'N';
   str[1] = '\0';
   process_v_dbg_flags (str);	/* pass -vN always */
# endif
# endif

   err_ind = optind;

   while ((option = getopt (argc, argv, opt_string)) != EOF) {

      switch (option) {

         case 'a':					/* memory options     */
	    process_a_option (optarg);
            break;

	 case 'b':					/* binary file name   */
	    process_b_option (optarg);
	    break;

	 case 'd':					/* off flags	      */
	    process_d_option (optarg);
	    break;

	 case 'e':					/* on flags	      */
	    process_e_option (optarg);
	    break;
    
	 case 'f':					/* source form	      */
	    process_f_option (optarg);
	    break;
    
	 case 'g':					/* -G 0               */
            cmd_line_flags.debug_lvl = Debug_Lvl_0;
	    break;

         case 'h':
            dump_options();
	    return; 
    
	 case 'i':					/* integer size	      */
	    process_i_option (optarg);
	    break;

         case 'k':					/* Solaris profiling  */
	    process_k_option (optarg);
            break;

      /* When the -l option is implemented (to specify what file the CIF      */
      /* output should go to), also update cif_misc_compiler_opts_rec in      */
      /* cif.c.								      */
      /*								      */
      /* case 'l':					   CIF name	      */
      /*    break;							      */

	 case 'm':					/* msg level suppr'd  */
	    process_m_option (optarg);
	    break;

	 case 'O':					/* optimization opts  */
            process_O_option (optarg, argc);
	    break;

	 case 'p':					/* module path name  */
	    add_to_fp_table (optarg, &module_path_idx, option);
	    break;

	 case 'q':					/* expression eval    */
	    process_q_option (optarg);
	    break;

	 case 'r':					/* list options       */
	    process_r_option (optarg);
	    break;

	 case 'R':					/* runtime checking   */
	    process_R_option (optarg);
	    break;

	 case 's':					/* type size option   */
	    process_s_option (optarg);
	    break;

	 case 'S':					/* cal file name      */
	    process_S_option (optarg);
	    break;

	 case 't':					/* truncate bits      */
	    process_t_option (optarg);
	    break;

	 case 'u':					/* debug dump flags   */
	    process_u_option (optarg);
	    break;

         case 'v':					/* PDGCS debug opts   */
            process_v_option (optarg);
            break;

         case 'A':					/* implicit use       */
	    process_A_option (optarg);
            break;

	 case 'C':					/* CIF flags	      */
	    process_C_option (optarg, argv);
	    break;

         case 'D':					/* DEFINE */
# ifdef _FRONTEND_CONDITIONAL_COMP
            process_D_option (optarg);
# endif
            break;

	 case 'x':					/* disregard CDIRs    */
	    process_x_option (optarg);
	    break;

	 case 'F':					/* fortran macro exp */
# ifdef _FRONTEND_CONDITIONAL_COMP
	    cmd_line_flags.pp_macro_expansion	= TRUE;
            on_off_flags.preprocess = TRUE;
# endif
	    break;
    
    
	 case 'G':					/* debug option       */
	    process_G_option (optarg);
	    break;
    
	 case 'I':					/* include option     */
            add_to_fp_table(optarg, &include_path_idx, option);
	    break;

	 case 'J':					/* .mod output locate */
	    process_J_option (optarg);
	    break;

	 case 'M':					/* disable msg numbers*/
	    process_M_option (optarg);
	    break;
    
	 case 'N':					/* fixed line size    */
	    process_N_option (optarg);
	    break;

	 case 'P':					/* position independ  */
	    process_P_option (optarg);
	    break;
    
         case 'U':					/* UNDEF */
# ifdef _FRONTEND_CONDITIONAL_COMP
            process_U_option (optarg);
# endif
            break;

	 case 'V':					/* version option     */
            cmd_line_flags.verify_option = TRUE;
	    break;

	 case 'X':					/* MPP num PE's opt   */
	    process_X_option (optarg);
	    break;

	 case 'Y':
	    process_Y_option (optarg);			/* ccg debug options  */
	    break;

# ifdef _F_MINUS_MINUS
         case 'Z':
            cmd_line_flags.co_array_fortran	= TRUE;
            dump_flags.f_minus_minus		= TRUE;
            dump_flags.fmm1			= TRUE;
          break;
# endif

	 default:     /* Command line has an invalid option. */

            err_char = argv[err_ind][1];

            if (err_char == 'O') {

               /* Accept -O with no arguments.  It means use defaults. */
               /* Intentionally blank */

               ntr_msg_queue(0, 1221, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
            }
            else {
               err_char	= optopt;
               ntr_msg_queue(0, 77, Log_Error, 0, 
                             (char *) NULL, optopt, ARG_ARG);
            }
	    break;
      }	 /* switch */

      err_ind = optind;
   }  /* while */

#  ifdef KEY /* Bug 4210 */
   /* We want to prepend the module-creation directory to the module-search-path
    * list. Note that the SGI command-line processing turns each -Idir option
    * into a -include=dir option plus a -pdir option. Here we want to imitate
    * the -pdir processing, except we prepend instead of appending.
    */
   add_to_fp_table(mod_out_path, &module_path_idx, 'J');
#  endif /* KEY Bug 4210 */
#  ifdef KEY /* Bug 5545 */
   /* Append /usr/include to include-search-path list, so that ftpp behaves
    * like cpp */
   add_to_fp_table("/usr/include", &include_path_idx, 'I');
#  endif /* KEY Bug 5545 */
#ifdef KEY /* Bug 5089 */
   /* If -intrinsic_module_path was not set but the NLSPATH environment
    * variable is set, use the last directory in NLSPATH (the driver
    * appends to any user-set NLSPATH the directory containing the file
    * cf95.cat, which also contains IEEE*.mod.) We ignore any user-specified
    * directory, which might have nothing to do with pathf90; a user who
    * wants to override the driver's choice should use -intrinsic_module_path.
    */
   if (NULL_IDX == intrinsic_module_path_idx && nlspath && *nlspath) {
     char *path = strrchr(nlspath, ':');
     path = path ? (path + 1) : nlspath;
#    define DIR_M32 "/32"
     char *copy = malloc(strlen(path) + sizeof DIR_M32);
     add_to_fp_table(
       strcat(dirname(strcpy(copy, path)), (Is_Target_32bit() ? "/32" : "")),
       &intrinsic_module_path_idx, 'p');
     free(copy);
   }
#endif /* KEY Bug 5089 */

# ifdef _DEBUG
   if (dump_flags.help_dbg) {
      dump_help_screen();
   }
# endif

   if (argc == 2  &&  cmd_line_flags.verify_option) {

      /* cft90 call only requests compiler version info.    */
      /* message will be produced by init_compiler in main. */
   }
   else {

      if (on_off_flags.MPP_apprentice) {
         cif_flags = cif_flags | ALL_RECS      |
                                 XREF_RECS     | MISC_RECS |
                                 MESSAGE_RECS  | INFO_RECS |
                                 BASIC_RECS    | COMPILER_RECS;
      }

#     if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

         if (dump_flags.cray_compatible) {
            cmd_line_flags.s_default64 = TRUE;
         }
#     endif

      if (on_off_flags.all_debug) {  /* Turn on all debugging options. */

         /* -eD implies -O0 -G0 -ei -m2 -RabcsCE -rl */

         opt_flags.support_lvl			= 0;
         cmd_line_flags.debug_lvl		= Debug_Lvl_0;
         on_off_flags.indef_init		= TRUE;
         cmd_line_flags.msg_lvl_suppressed 	= Caution_Lvl;
         cmd_line_flags.runtime_argument        = TRUE;
         cmd_line_flags.runtime_bounds          = TRUE;
         cmd_line_flags.runtime_conformance     = TRUE;
         cmd_line_flags.runtime_substring       = TRUE;
         cmd_line_flags.runtime_ptr_chk         = TRUE;
         cmd_line_flags.runtime_arg_call        = TRUE;
         cmd_line_flags.runtime_arg_entry       = TRUE;
         cif_flags = cif_flags | ALL_RECS      |
                                 XREF_RECS     | MISC_RECS |
                                 MESSAGE_RECS  | INFO_RECS |
                                 BASIC_RECS    | COMPILER_RECS;
         cif_C_opts = cif_C_opts | ALL_RECS;

         if (on_off_flags.zero_init) {  /* Conflict - issue warning */
            on_off_flags.zero_init = FALSE;
            ntr_msg_queue(0, 1313, Log_Warning, 0, "ei\ne0", 0,MULT_STR_ARG);
         }

         if (! set_scalar_option) {
            opt_flags.scalar_lvl = Scalar_Lvl_0;
         }

#        if defined(_ACCEPT_VECTOR)

         if (! set_vector_option) {
            opt_flags.vector_lvl = Vector_Lvl_0;
         }

#        endif

#        if defined(_ACCEPT_TASK)

         if (! set_task_option) {
            opt_flags.task_lvl = Task_Lvl_0;
         }
#        endif

#        if defined(_ACCEPT_STREAM)

         if (! set_stream_option) {
            opt_flags.stream_lvl = Stream_Lvl_0;
         }

#        endif
      }

      /* Validate G before O, because G can override O levels. */

      if (cmd_line_flags.debug_lvl < No_Debugging) {
         validate_G_option ();
      }

      validate_O_option ();
      validate_R_option ();
      validate_s_option ();

      if (cmd_line_flags.align32 && cmd_line_flags.align64) {
         cmd_line_flags.align32		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-a align32\n-a align64\n-a align32",
                       0, MULT_STR_ARG);
      }

      if (target_ieee) {

         if (set_ieeeconform_option && opt_flags.ieeeconform && 
             !on_off_flags.eu) {
            on_off_flags.eu = FALSE;

            if (set_eu_option) {  /* -du has been specified. */
               ntr_msg_queue(0, 1215, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
            }
            else {                /* -du is default. */
               ntr_msg_queue(0, 1216, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
            }
         }

         if (opt_flags.ieeeconform) {
            on_off_flags.reciprical_divide = FALSE;
         }
         else {
            on_off_flags.reciprical_divide = !on_off_flags.eu;
         }
      }
      else {
         on_off_flags.round_integer_divide = on_off_flags.eu;
      }

      if (on_off_flags.assembly_listing_file && !cmd_line_flags.binary_output) {
         cmd_line_flags.binary_output = TRUE;
         ntr_msg_queue(0, 913, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
      }

      if (cmd_line_flags.co_array_fortran) {
# if defined(_TASK_COMMON_EXTENSION)
         cmd_line_flags.taskcommon	= TRUE;
# endif
         opt_flags.task_lvl		= Task_Lvl_0;
      }

# if defined(_NO_BINARY_OUTPUT)
      binary_output	= FALSE;
      assembly_output	= cmd_line_flags.binary_output ||
                          cmd_line_flags.assembly_output;
# else
      binary_output	= cmd_line_flags.binary_output;
      assembly_output	= cmd_line_flags.assembly_output;
# endif

/*
      if (no_preprocessing) {
         on_off_flags.preprocess = FALSE;
         on_off_flags.preprocess_only = FALSE;
         on_off_flags.save_dot_i = FALSE;
      }
*/

      set_prog_file_names (argv);

      if (optind < (argc-1)) { /* Options not allowed after input file name. */
         ntr_msg_queue(0, 79, Log_Error, 0, (char *) NULL, 0, NO_ARG);
      }
   }

   if (cmd_line_flags.mod_out_path && !on_off_flags.module_to_mod) {

      /* Must specify -em to specify -J */

      ntr_msg_queue(0, 1658, Log_Error, 0, (char *) NULL, 0, NO_ARG);
   }

   PRINT_CMD_LINE_TBLS;  /* Will print if -u cmd is specified for a DEBUG comp*/

   if (dump_flags.show_cmd_line) {
      printf("\n");
      for (i = 0; i < save_argc; i++) {
         printf("%s ", argv[i]);
      }
      printf("\n\n");
   }

   issue_deferred_msgs();

   TRACE (Func_Exit, "process_cmd_line", NULL);

   return;
  
}  /* process_cmd_line */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Validate_G_option is called to check the combination of debug options *|
|*	and optimization options.					      *|
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

static void validate_G_option (void)
{
/* KAY - This routine needs help.  It doesn't appear to be finished. */

   int		correct_scalar_lvl;
   int		correct_vector_lvl;
   int		correct_task_lvl;
   int		debug_lvl;


   TRACE (Func_Entry, "validate_G_option", NULL);

   if (!cmd_line_flags.binary_output) {

# if !defined(_NO_BINARY_OUTPUT)

      /* Warning - binary output needs to be on for debugging. */

      ntr_msg_queue(0, 82, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
      cmd_line_flags.assembly_output	= FALSE;
      cmd_line_flags.binary_output	= TRUE;
# endif

   }

   switch (cmd_line_flags.debug_lvl) {
   case Debug_Lvl_0:

      if (set_scalar_option && opt_flags.scalar_lvl > Scalar_Lvl_0) {
         ntr_msg_queue(0, 1536, Log_Warning, 0,
                       scalar_lvl_str[Scalar_Lvl_0], 
                       Debug_Lvl_0, STR_ARG_ARG);
      }

      if (set_vector_option && opt_flags.vector_lvl > Vector_Lvl_0) {
        ntr_msg_queue(0, 1536, Log_Warning, 0,
                       vector_lvl_str[Vector_Lvl_0], 
                       Debug_Lvl_0, STR_ARG_ARG);
      }

      if (set_stream_option && opt_flags.stream_lvl > Stream_Lvl_0) {
         ntr_msg_queue(0, 1536, Log_Warning, 0,
                       stream_lvl_str[Stream_Lvl_0], 
                       Debug_Lvl_0, STR_ARG_ARG);
      }

      if (set_task_option && opt_flags.task_lvl > Task_Lvl_0) {
         ntr_msg_queue(0, 1536, Log_Warning, 0,
                       task_lvl_str[Task_Lvl_0], 
                       Debug_Lvl_0, STR_ARG_ARG);
      }

      if (set_support_lvl_option && opt_flags.support_lvl > 0) {
         ntr_msg_queue(0, 1536, Log_Warning, 0,
                       "-O0", Debug_Lvl_0, STR_ARG_ARG);
      }

      if (opt_flags.inline_lvl > Inline_Lvl_0 ||
          set_inlinefrom_option) {
         ntr_msg_queue(0, 1199, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
      }

      cmd_line_flags.do_UDB_checks	= opt_flags.scalar_lvl > Scalar_Lvl_0;
      opt_flags.scalar_lvl		= Scalar_Lvl_0;
      opt_flags.stream_lvl		= Stream_Lvl_0;
      opt_flags.vector_lvl		= Vector_Lvl_0;
      opt_flags.task_lvl		= Task_Lvl_0;
      opt_flags.inline_lvl		= Inline_Lvl_0; 
      opt_flags.support_lvl		= 0;

      if (set_aggress_option) {
         ntr_msg_queue(0, 1069, Log_Warning, 0,
                       "aggress", Debug_Lvl_0, STR_ARG_ARG);
      }

      if (set_bottom_load_option) {
         ntr_msg_queue(0, 1069, Log_Warning, 0,
                       "bl", Debug_Lvl_0, STR_ARG_ARG);
      }

      if (set_loop_align_option) {
         ntr_msg_queue(0, 1069, Log_Warning, 0,
                       "loopalign", Debug_Lvl_0, STR_ARG_ARG);
      }

      if (set_pattern_option) {
         ntr_msg_queue(0, 1069, Log_Warning, 0,
                       "pattern", Debug_Lvl_0, STR_ARG_ARG);
      }

      if (set_pipeline_option) {
         ntr_msg_queue(0, 1069, Log_Warning, 0,
                       "pipeline", Debug_Lvl_0, STR_ARG_ARG);
      }

      if (set_recurrence_option) {
         ntr_msg_queue(0, 1069, Log_Warning, 0,
                       "recurrence", Debug_Lvl_0, STR_ARG_ARG);
      }

      if (set_vsearch_option) {
         ntr_msg_queue(0, 1069, Log_Warning, 0,
                       "vsearch", Debug_Lvl_0, STR_ARG_ARG);
      }

      if (set_zeroinc_option) {
         ntr_msg_queue(0, 1069, Log_Warning, 0,
                       "zeroinc", Debug_Lvl_0, STR_ARG_ARG);
      }

      opt_flags.aggress		= FALSE;
      opt_flags.bottom_load	= FALSE;
      opt_flags.loopalign	= FALSE;
      opt_flags.recurrence	= FALSE;
      opt_flags.pattern		= FALSE;
      opt_flags.pipeline_lvl	= 0;
      opt_flags.vsearch		= FALSE;
      opt_flags.zeroinc		= FALSE;
      set_debug_option		= TRUE;
      break;


   case Debug_Lvl_1:

      debug_lvl			= Debug_Lvl_1;
      set_debug_option		= TRUE;

      if (opt_flags.scalar_lvl > Scalar_Lvl_2) {

         if (set_scalar_option) {
            ntr_msg_queue(0, 1536, Log_Warning, 0,
                          scalar_lvl_str[Scalar_Lvl_2], 
                          Debug_Lvl_1, STR_ARG_ARG);
         }
         opt_flags.scalar_lvl	= Scalar_Lvl_2;
      }

      if (opt_flags.inline_lvl > Inline_Lvl_0 ||
          set_inlinefrom_option) {
         opt_flags.inline_lvl = Inline_Lvl_0; 
         ntr_msg_queue(0, 1199, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
      }

# if defined(_ACCEPT_STREAM)

      if (opt_flags.stream_lvl > Stream_Lvl_1) {

         if (set_stream_option) {
            ntr_msg_queue(0, 1536, Log_Warning, 0,
                          stream_lvl_str[Stream_Lvl_1], 
                          Debug_Lvl_1, STR_ARG_ARG);
         }

         opt_flags.stream_lvl	= Stream_Lvl_1;
      }
# endif


# if defined(_ACCEPT_VECTOR)

      if (opt_flags.vector_lvl > Vector_Lvl_1) {

         if (set_vector_option) {
            ntr_msg_queue(0, 1536, Log_Warning, 0,
                          vector_lvl_str[Vector_Lvl_1], 
                          Debug_Lvl_1, STR_ARG_ARG);
         }

         opt_flags.vector_lvl	= Vector_Lvl_1;
      }
# endif

      if (set_support_lvl_option && opt_flags.support_lvl > 1) {
         ntr_msg_queue(0, 1536, Log_Warning, 0,
                       "-O1", Debug_Lvl_1, STR_ARG_ARG);
         opt_flags.support_lvl	= 1;
      }

      break;


   case Debug_Lvl_2:

      set_debug_option		= TRUE;
      correct_vector_lvl	= Vector_Lvl_3;
      correct_task_lvl		= Task_Lvl_3;
      correct_scalar_lvl	= Scalar_Lvl_3;
      debug_lvl			= Debug_Lvl_2;
      break;

   case Debug_Lvl_3:

      /* Ignore these for now.  gsf says -G doesn't affect levels */

      correct_vector_lvl	= Vector_Lvl_3;
      correct_task_lvl		= Task_Lvl_3;
      correct_scalar_lvl	= Scalar_Lvl_3;
      debug_lvl			= Debug_Lvl_3;
      break;
   }

   TRACE (Func_Exit, "validate_G_option", NULL);

   return;

}  /* validate_G_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
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

static void validate_R_option( void )

{


   TRACE (Func_Entry, "validate_R_option", NULL);

   if (cmd_line_flags.runtime_arg_count_only &&
       ! cmd_line_flags.runtime_argument &&
       ! cmd_line_flags.runtime_arg_call &&
       ! cmd_line_flags.runtime_arg_entry) {

      cmd_line_flags.runtime_argument = TRUE;
   }

   TRACE (Func_Exit, "validate_R_option", NULL);

   return;

}  /* validate_R_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Init_flags is called to set all flag structs to their default values. *|
|*	These flags control the compiler.  The user is able to change any of  *|
|*	these flag values through commmand line options and compiler	      *|
|*	directives.							      *|
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
	   
static void init_cmd_line (void)
 
{
   int	idx;
   char	*u_option;

   TRACE (Func_Entry, "init_cmd_line", NULL);

# if defined(_ACCEPT_STREAM)
# if defined(_TARGET_SV2)
     accept_stream = TRUE;
# else
     accept_stream = target_sv1;
# endif
# endif

   set_support_lvl_option	= FALSE;

   /* Set defaults for CIF option flags.                                      */
   cif_flags              = 0;
   cif_C_opts		  = 0;

   /* set defaults for command line level option flags */

   cmd_line_flags.dalign		= (cft90_dash_a_dalign_option == 1);
   cmd_line_flags.align32		= FALSE;
   cmd_line_flags.align64		= FALSE;
								/* -a  dalign */
   cmd_line_flags.taskcommon		= FALSE;		/*-ataskcommon*/
   cmd_line_flags.static_threadprivate	= FALSE;		/*-astatic_th */
   cmd_line_flags.solaris_profile	= FALSE;		/* -k  s/g    */

   cmd_line_flags.binary_output		= TRUE;			/* -b  name   */
   cmd_line_flags.assembly_output	= FALSE;		/* -s  name   */

   cmd_line_flags.runtime_argument	= FALSE;		/* -R a       */
   cmd_line_flags.runtime_arg_call	= FALSE;		/* -R C       */
   cmd_line_flags.runtime_arg_entry	= FALSE;		/* -R E       */
   cmd_line_flags.runtime_arg_count_only= FALSE;		/* -R n       */
   cmd_line_flags.runtime_bounds	= FALSE;		/* -R b       */
   cmd_line_flags.runtime_conformance	= FALSE;		/* -R c       */
   cmd_line_flags.runtime_intrinsics	= FALSE;		/* -R i	      */
   cmd_line_flags.runtime_substring	= FALSE;		/* -R s       */
   cmd_line_flags.runtime_ptr_chk	= FALSE;		/* -R p       */
   cmd_line_flags.s_float64		= FALSE;		/*-s float64  */
   cmd_line_flags.s_default32		= FALSE;		/*-s default32*/
   cmd_line_flags.s_default64		= FALSE;		/*-s default64*/
   cmd_line_flags.s_cf77types		= FALSE;		/*-s cf77types*/
   cmd_line_flags.s_integer8		= FALSE;		/*-s integer8 */
   cmd_line_flags.s_logical8		= FALSE;		/*-s logical8 */
   cmd_line_flags.s_real8		= FALSE;		/*-s real8    */
   cmd_line_flags.s_complex8		= FALSE;		/*-s complex8 */
   cmd_line_flags.s_doubleprecision16	= FALSE;		/*-s doublep  */
   cmd_line_flags.s_doublecomplex16	= FALSE;		/*-s doublec  */

# if defined(_TARGET_SV2) || \
       ((defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)) && defined(_TARGET_OS_UNICOS))
   cmd_line_flags.s_pointer8		= TRUE;			/*-s pointer8 */
# else
   cmd_line_flags.s_pointer8		= FALSE;		/*-s pointer8 */
# endif
   cmd_line_flags.src_form		= Fixed_Form;
   cmd_line_flags.disregard_all_mpp_cdirs	= FALSE;	/* -x mpp     */
   cmd_line_flags.disregard_all_directives	= FALSE;	/* -x all     */
   cmd_line_flags.disregard_all_dirs		= FALSE;	/* -x dir     */
   cmd_line_flags.disregard_all_mics		= FALSE;	/* -x mic     */
   cmd_line_flags.disregard_all_mips		= FALSE;        /* -x SGI     */
   cmd_line_flags.disregard_all_omps		= FALSE;        /* -x omp     */
   cmd_line_flags.disregard_conditional_omp	= FALSE;        /* -x cond_.. */
   cmd_line_flags.msg_lvl_suppressed	= (msg_lvl_type) cft90_dash_m_option;
   cmd_line_flags.truncate_bits		= 0;			/* -t  0      */
   cmd_line_flags.implicit_use_idx	= NULL_IDX;
   cmd_line_flags.debug_lvl		= (debug_lvl_type) 
                                           cft90_dash_G_debug_option;

   cmd_line_flags.mod_out_path		= FALSE;		/* -J         */
   cmd_line_flags.dwarf_debug		= FALSE;		/* -Gd        */
   cmd_line_flags.num_msgs_suppressed	= 0;			/* num -M msgs*/
   cmd_line_flags.line_size_80		= (cft90_dash_N_option == 80);
   cmd_line_flags.line_size_132         = (cft90_dash_N_option == 132);
   cmd_line_flags.verify_option		= FALSE;		/* -V  FALSE  */
   cmd_line_flags.malleable		= FALSE;		/* -Xm FALSE  */
   cmd_line_flags.MPP_num_pes		= cft90_dash_X_option;  /* -X n$pes   */
   cmd_line_flags.integer_32 		= FALSE;
   cmd_line_flags.co_array_fortran	= FALSE;
   cmd_line_flags.pp_macro_expansion	= FALSE;

   /* Set defaults for -e / -d option flags */

   on_off_flags.abort_if_any_errors	= (cft90_dash_e_a_option == 1);

   /* Hard code this as turned on by default for now because we want IEEE     */
   /* intrinsics to always be recognized.  Leave the option here in case      */
   /* customers scream too much about name space encroachment.		      */

   on_off_flags.pad_char_literals	= FALSE;		/* -dc FALSE  */
   on_off_flags.ieee			= TRUE;			/* -ee TRUE   */
   on_off_flags.flowtrace_option	= FALSE;		/* -ef FALSE  */
   on_off_flags.assembly_listing_file	= FALSE;		/* -eg FALSE  */

# if defined(_INTEGER_1_AND_2) && !defined(_ACCEPT_CMD_ed_h)
   on_off_flags.integer_1_and_2		= TRUE;
# else
   on_off_flags.integer_1_and_2		= FALSE;
# endif

   on_off_flags.indef_init		= (cft90_dash_e_i_option == 1);
   on_off_flags.exec_doloops_once	= FALSE;		/* -ej FALSE  */
   on_off_flags.module_to_mod		= (cft90_dash_e_m_option == 1);
   on_off_flags.issue_ansi_messages	= (cft90_dash_e_n_option == 1);
   on_off_flags.enable_double_precision	= (cft90_dash_e_p_option == 1);
   on_off_flags.abort_on_100_errors	= TRUE;			/* -eq TRUE   */
   on_off_flags.round_mult_operations	= (cft90_dash_e_r_option == 1);
   on_off_flags.alloc_autos_on_stack	= (cft90_dash_e_t_option == 1);

   /* This gets translated into reciprical divide /strong round flag */
   /* in process_cmd_line.   For now, turn them all off.             */

   on_off_flags.eu			= (cft90_dash_e_u_option == 1);
   on_off_flags.reciprical_divide	= FALSE;
   on_off_flags.round_integer_divide	= FALSE;
   on_off_flags.recognize_minus_zero	= FALSE;
   on_off_flags.zero_init		= FALSE;
   on_off_flags.save_all_vars		= (cft90_dash_e_v_option == 1);
   on_off_flags.MPP_apprentice		= FALSE;		/* -dA        */
   on_off_flags.shared_to_private_coer	= FALSE;		/* -dC        */
   on_off_flags.all_debug		= FALSE;		/* -dD        */
   on_off_flags.top_test_shortloops	= FALSE;		/* -dL        */
   on_off_flags.second_underscore	= TRUE;			/* -eN        */
   on_off_flags.underscoring    	= TRUE;			/* -eU        */
   on_off_flags.allow_leading_uscore    = FALSE;                /* -dQ        */
   on_off_flags.output_pound_lines      = TRUE;                 /* -dP        */
   on_off_flags.preprocess_only         = FALSE;                /*            */
   on_off_flags.preprocess              = FALSE;
   on_off_flags.save_dot_i              = FALSE;                /* -ek        */
   on_off_flags.recursive		= FALSE;		/* -dR        */
   on_off_flags.atexpert		= FALSE;		/* -dX        */
   on_off_flags.upper_case_names	= FALSE;
   on_off_flags.d_lines	                = FALSE;
 
   /* Set defaults for -O option flags.					      */

   /* If these options vary depending on platform, use cmd_line.h and the     */
   /* cft90_dash_O method to initialize.  That way all platform information   */
   /* for commandline initialization is in one place.                         */

   opt_flags.aggress			= (cft90_dash_O_aggress_option == 1);
   opt_flags.bottom_load		= (cft90_dash_O_bl_option == 1);
   opt_flags.set_fastint_option		= FALSE;
   opt_flags.set_nofastint_option	= FALSE;
   opt_flags.set_allfastint_option	= FALSE;
   opt_flags.ieeeconform		= (cft90_dash_O_ieeeconform_option==1);
   opt_flags.inline_lvl			= Inline_Lvl_0;
   opt_flags.extent_assert		= FALSE;
   opt_flags.short_circuit_lvl		= Short_Circuit_Present;
   opt_flags.jump			= (cft90_dash_O_jump_option == 1);
   opt_flags.loopalign			= (cft90_dash_O_loopalign_option == 1);
   opt_flags.modinline			= FALSE;
   opt_flags.msgs			= FALSE;
   opt_flags.neg_msgs			= FALSE;
   opt_flags.nointerchange		= FALSE;
   opt_flags.opt_info			= FALSE;
   opt_flags.over_index			= (cft90_dash_O_overindex_option == 1);
   opt_flags.pattern			= (cft90_dash_O_pattern_option == 1);
   opt_flags.pipeline_lvl		= 0;
   opt_flags.recurrence			= (cft90_dash_O_recurrence_option == 1);
   opt_flags.taskinner			= (cft90_dash_O_taskinner_option == 1);
   opt_flags.threshold			= (cft90_dash_O_threshold_option == 1);
   opt_flags.vsearch			= (cft90_dash_O_vsearch_option == 1);
   opt_flags.zeroinc			= (cft90_dash_O_zeroinc_option == 1);
   opt_flags.support_lvl		= cft90_dash_O_support_option;
   opt_flags.scalar_lvl 		= cft90_dash_O_scalar_option;
   opt_flags.split_lvl                  = cft90_dash_O_split_option;
   opt_flags.vector_lvl 		= cft90_dash_O_vector_option;
   opt_flags.task_lvl                   = cft90_dash_O_task_option;
   opt_flags.unroll_lvl			= cft90_dash_O_unroll_option;
   opt_flags.reshape_idx		= NULL_IDX;
   opt_flags.reshape			= FALSE;
   opt_flags.reshape_all_arrays		= FALSE;
   opt_flags.matmul_inline		= FALSE;
   opt_flags.mv_matmul_inline		= FALSE;
 
   /* Set defaults for -x option flags.					      */

   for (idx = 0; idx < (Tok_Dir_End-Tok_Dir_Start); idx++) {
      disregard_directive[idx]	= FALSE;
   }

   for (idx = 0; idx < (Tok_Mic_End-Tok_Mic_Start); idx++) {
      disregard_mics[idx]	= FALSE;
   }

   for (idx = 0; idx < (Tok_SGI_Dir_End-Tok_SGI_Dir_Start); idx++) {
      disregard_mips[idx]	= FALSE;
   }

   for (idx = 0; idx < (Tok_Open_Mp_Dir_End-Tok_Open_Mp_Dir_Start); idx++) {
      disregard_open_mp[idx]	= FALSE;
   }

   /* set defaults for -u option flags */
   dump_flags.pvp_test			= 0;
   dump_flags.blk_stk			= FALSE;
   dump_flags.bd_tbl			= FALSE;
   dump_flags.cmd_line_tbls		= FALSE;
   dump_flags.cn_tbl			= FALSE;
   dump_flags.fp_tbl			= FALSE;
   dump_flags.ftrace_info		= FALSE;
   dump_flags.gl_tbl			= FALSE;
   dump_flags.intrin_tbl		= FALSE;
   dump_flags.ir1_tbl			= FALSE;
   dump_flags.ir2_tbl			= FALSE;
   dump_flags.ir3_tbl			= FALSE;
   dump_flags.ir4_tbl			= FALSE;
   dump_flags.mem_report		= FALSE;
   dump_flags.mod_version		= FALSE;
   dump_flags.mtrace_info		= FALSE;
   dump_flags.name_tbls			= FALSE;
   dump_flags.pdgcs			= FALSE;
   dump_flags.pdt_dump			= FALSE;
   dump_flags.sb_tbl			= FALSE;
   dump_flags.scp_tbl			= FALSE;
   dump_flags.src_dmp			= FALSE;
   dump_flags.std_err			= FALSE;
   dump_flags.stmt_dmp			= FALSE;
   dump_flags.sytb			= FALSE;
   dump_flags.typ_tbl			= FALSE;
   dump_flags.defines			= FALSE;
   dump_flags.constant_bits		= FALSE;
   dump_flags.abort_on_ansi             = FALSE;
   dump_flags.no_dimension_padding	= FALSE;
   dump_flags.no_module_output		= FALSE;
   dump_flags.f_minus_minus		= FALSE;
   dump_flags.fmm1			= FALSE;
   dump_flags.fmm2			= FALSE;
   dump_flags.show_cmd_line		= FALSE;
   dump_flags.mp			= FALSE;
   dump_flags.open_mp			= FALSE;
   dump_flags.dsm			= FALSE;
   dump_flags.cray_compatible		= FALSE;
   dump_flags.pack_half_word		= FALSE;
#ifdef KEY /* Bug 8117 */
   dump_flags.arg_passing		= FALSE;
#endif /* KEY Bug 8117 */

   /* Set defaults for -Y CCG option flags.                                   */

   ccg_dump_flags         		= 0;			 /* All FALSE */


   /* Clear the message tables.   */

   for (idx = 0; idx < MAX_MSG_SIZE; idx++) {
      message_suppress_tbl[idx]	= 0L;
      message_error_tbl[idx]	= 0L;
      message_warning_tbl[idx]	= 0L;
   }

   u_option = getenv("FE_DASH_U_OPTION");

   if (u_option != NULL) {
      process_u_option(u_option);
   }

   TRACE (Func_Exit, "init_cmd_line", NULL);

   return;

}  /* init_cmd_line */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process_c_option handles command line CIF options passed via argv     *|
|*	(c switch).							      *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-C option arguments			      *|
|*	argv			argument list				      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
|* Algorithm notes:							      *|
|*   CIF file output is requested via the -C option.  			      *|
|*      								      *|
|*   Qualifier    Constant         Meaning			     	      *|
|*   ---------    --------         --------				      *|
|*      a         ALL_RECS         All available CIF record types	      *|
|*      c         COMPILER_RECS    Only CIF records that contain compiler     *|
|*                                   information, plus f-level records	      *|
|*      f         BASIC_RECS       Basic CIF records (file info, unit markers,*|
|*                                   include files, etc.) 		      *|
|*      i         INFO_RECS        Information records for variables, labels, *|
|*                                   constants, etc., plus f-level records    *|
|*      m         MESSAGE_RECS     Message records plus f-level records       *|
|*      o         MISC_RECS        Miscellaneous other records (stmt types,   *|
|*                                   call sites, etc.) plus f- and i-level    *|
|*                                   records				      *|
|*      s         OUTPUT_TO_STDOUT Redirect CIF output to stdout instead of   *|
|*                                   file.T 				      *|
|*      x         XREF_RECS        Cross-reference information plus f- and    *|
|*                                   i-level records			      *|
|*             								      *|
\******************************************************************************/
 
static void process_C_option (char *optargs,
		              char *argv[])

{
   int		ch;
   char		err_str[2];
   long		orig_cmd_len;

   extern int	optind;					/* defined in getopt  */


   TRACE (Func_Entry, "process_C_option", NULL);

   while (ch = *optargs++) {

      switch (ch) {

	 case 'a':
	    cif_flags = cif_flags | ALL_RECS      |
                                    XREF_RECS     | MISC_RECS |
				    MESSAGE_RECS  | INFO_RECS |
				    BASIC_RECS    | COMPILER_RECS;
            cif_C_opts = cif_C_opts | ALL_RECS;
	    break;

	 case 'c':
	    cif_flags  = cif_flags | COMPILER_RECS | BASIC_RECS;
            cif_C_opts = cif_C_opts | COMPILER_RECS;
	    break;

	 case 'f':
	    cif_flags  = cif_flags | BASIC_RECS;
            cif_C_opts = cif_C_opts | BASIC_RECS;
	    break;

	 case 'i':
	    cif_flags  = cif_flags | INFO_RECS | BASIC_RECS;
            cif_C_opts = cif_C_opts | INFO_RECS;
	    break;

	 case 'm':
	    cif_flags  = cif_flags | MESSAGE_RECS | BASIC_RECS;
            cif_C_opts = cif_C_opts | MESSAGE_RECS;
	    break;

	 case 'o':
	    cif_flags  = cif_flags | MISC_RECS | INFO_RECS | BASIC_RECS;
            cif_C_opts = cif_C_opts | MISC_RECS;
	    break;

	 case 's':
	    cif_flags  = cif_flags | OUTPUT_TO_STDOUT;
            cif_C_opts = cif_C_opts | OUTPUT_TO_STDOUT;
	    break;	

	 case 'x':
	    cif_flags  = cif_flags | XREF_RECS | INFO_RECS | BASIC_RECS;
            cif_C_opts = cif_C_opts | XREF_RECS;
	    break;

         case 'Y':

            /* -CY is NOT a user-specifiable option.  It is used by the MPP   */
            /* driver to pass the command line as entered by the user and     */
            /* added to by the driver for the Original Command Line CIF       */
            /* record which cflist requires.  The PVP driver adds this        */
            /* record when it consolidates the .T files.                      */

            orig_cmd_len = (long) strlen(argv[optind]);
            ++orig_cmd_len;
            MEM_ALLOC(orig_cmd_line, char, orig_cmd_len);
            strcpy(orig_cmd_line, argv[optind]);
            ++optind;
            break;

         case 'Z':

            /* -CZ is NOT a user-specifiable option.  It is the option the    */
            /* command line processor uses to pass the CIF name to us.  The   */
            /* command line processor issues the error if it's specified by a */
            /* user.                                                          */

            strcpy(cif_name, optargs);
            
            /* Since the string following the CZ is a file name, skip past    */
            /* the entire string to complete the while loop.  No additional   */
            /* -C options are allowed after a -CZ.                            */

            optargs = optargs + strlen(optargs);
            break;

	 default:
	    err_str[0] = ch;
	    err_str[1] = EOS;

	    /* The CIF option has an invalid argument */

            ntr_msg_queue(0, 917, Log_Error, 0, err_str, 0, STR_ARG);
	    break;

	 }  /* switch */

   }  /* while */

   if (cif_flags != 0) {
      cif_C_opts = cif_C_opts | CMD_PROVIDED_CIF;
   }

   /* If the only suboption specified was the "s" (output to stdout)          */
   /* suboption, don't attempt to produce any CIF records.		      */

   if (cif_flags == OUTPUT_TO_STDOUT) {
      cif_flags = 0;
      cif_C_opts = 0;
   }

   TRACE (Func_Exit, "process_C_option", NULL);

   return;
 
}  /* process_C_option */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process_d_option handles command line on/off options passed via argv  *|
|*	(d switch).							      *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-d option arguments			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
 
static void process_d_option (char *optargs)

{
   int	ch;
   char err_str[2];
 

   TRACE (Func_Entry, "process_d_option", NULL);

   while (ch = *optargs++) {
      switch (ch) {
      case 'a':
         on_off_flags.abort_if_any_errors = FALSE;
         break;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      case 'c':
         on_off_flags.pad_char_literals = FALSE;
         break;
# endif

# ifdef _D_LINES_SUPPORTED
      case 'd':
         on_off_flags.d_lines = FALSE;
         break;
# endif


      case 'f':

#        if defined(_ACCEPT_FLOW)
            on_off_flags.flowtrace_option = FALSE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "f", 'd', ARG_STR_ARG);
#        endif
         break;

      case 'g':

#        if defined(_ACCEPT_CMD_ed_g)
            on_off_flags.assembly_listing_file = FALSE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "g", 'd', ARG_STR_ARG);
#        endif
         break;

      case 'h':

#        if defined(_ACCEPT_CMD_ed_h)
	    on_off_flags.integer_1_and_2 = FALSE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "h", 'd', ARG_STR_ARG);
#        endif
         break;

      case 'i':

#        if defined(_ACCEPT_CMD_ed_i)
            ntr_msg_queue(0, 744, Log_Warning, 0, "i", 'd', ARG_STR_ARG);
#        else
	    on_off_flags.indef_init = FALSE;
#        endif
	 break;

      case 'j':

#        if defined(_ACCEPT_CMD_ed_j)
	    on_off_flags.exec_doloops_once = FALSE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "j", 'd', ARG_STR_ARG);
#        endif
         break;

      case 'k': on_off_flags.output_pound_lines = FALSE;
	 break;

      case 'm':

         /* If turning off -em, need an alternative to write out the module. */

#        if defined(_MODULE_TO_DOT_o) || defined(_MODULE_TO_DOT_M)
            on_off_flags.module_to_mod = FALSE;
#        else
            PRINTMSG (0, 744, Log_Warning, 0, 'd', "m");
#        endif
         break;

      case 'n':
         on_off_flags.issue_ansi_messages = FALSE;
         break;

      case 'p':
         on_off_flags.enable_double_precision = FALSE;
         break;

      case 'q':
         on_off_flags.abort_on_100_errors = FALSE;
         break;

      case 'r':

#        if defined(_ACCEPT_CMD_ed_r)

	    if (set_trunc_option) {
	       set_round_option	= FALSE;

	       /* Rounding(-dr) /truncation (-t) conflict detected. */

               ntr_msg_queue(0, 75, Log_Warning, 0, "-dr", 0, STR_ARG);
	    }

	    cmd_line_flags.truncate_bits	= 0;
	    on_off_flags.round_mult_operations	= FALSE;
	    set_round_option			= TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "r", 'd', ARG_STR_ARG);
#        endif
         break;

      case 't':
         on_off_flags.alloc_autos_on_stack = FALSE;
         break;

      case 'u':
         set_eu_option	 = TRUE;
	 on_off_flags.eu = FALSE;
         break;

      case 'v':
         on_off_flags.save_all_vars = FALSE;
         break;

      case 'z':

#        if defined(_ACCEPT_CMD_ed_z)
	    on_off_flags.recognize_minus_zero = FALSE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "z", 'd', ARG_STR_ARG);
#        endif
         break;

      case '0':
#        if defined(_ACCEPT_CMD_ed_0)
            on_off_flags.zero_init = FALSE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "0", 'd', ARG_STR_ARG);
#        endif
         break;

      case 'A':

#        if defined(_ACCEPT_CMD_ed_A)
            on_off_flags.MPP_apprentice = FALSE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "A", 'd', ARG_STR_ARG);
#        endif
         break;
    
      case 'B':
         cmd_line_flags.binary_output = FALSE;
         break;

      case 'C':

#        if defined(_ACCEPT_CMD_ed_C)
            on_off_flags.shared_to_private_coer = FALSE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "C", 'd', ARG_STR_ARG);
#        endif
         break;

      case 'D':
#        if defined(_ACCEPT_CMD_ed_i)
            on_off_flags.all_debug = FALSE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "D", 'd', ARG_STR_ARG);
#        endif
         break;

      case 'I':
         on_off_flags.implicit_none = FALSE;
         break;

      case 'L':
         on_off_flags.top_test_shortloops = FALSE;
         break;

      case 'N':
         on_off_flags.second_underscore = FALSE;
         break;

      case 'O':
         on_off_flags.underscoring = FALSE;
         break;

      case 'Q':   /* Q is undocumented */
         on_off_flags.allow_leading_uscore = FALSE;
         break;
    
      case 'R':
         on_off_flags.recursive = FALSE;
         break;

      case 'S':
         cmd_line_flags.assembly_output = FALSE;
         break;

      case 'T':
# ifdef _FRONTEND_CONDITIONAL_COMP
         no_preprocessing = TRUE;
         on_off_flags.preprocess = FALSE;
         on_off_flags.preprocess_only = FALSE;
         on_off_flags.save_dot_i = FALSE;
# endif
         break;

      case 'U':   /* U is undocumented */
#        if defined(_ACCEPT_CMD_ed_U)
            on_off_flags.upper_case_names = FALSE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "U", 'd', ARG_STR_ARG);
#        endif
         break;

      case 'X':

#        if defined(_ACCEPT_CMD_ed_X)
            on_off_flags.atexpert = FALSE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "X", 'd', ARG_STR_ARG);
#        endif
         break;

      default:
         err_str[0] = ch;
         err_str[1] = EOS;

         /* option has an invalid argument */

         ntr_msg_queue(0, 78, Log_Error, 0, err_str, 'd', ARG_STR_ARG);
         break;

      }  /* switch */
   }  /* while */

   TRACE (Func_Exit, "process_d_option", NULL);

   return;
 
}  /* process_d_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process_e_option handles command line on/off options passed via argv  *|
|*	(e switch).							      *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-e option arguments			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_e_option (char *optargs)

{
   int		 ch;
   char		 err_str[2];


   TRACE (Func_Entry, "process_e_option", NULL);

   while (ch = *optargs++) {
      switch (ch) {
      case 'a':
         on_off_flags.abort_if_any_errors = TRUE; 
         break;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      case 'c':
         on_off_flags.pad_char_literals = TRUE;
         break;
# endif

# ifdef _D_LINES_SUPPORTED
      case 'd':
         on_off_flags.d_lines = TRUE;
         break;
# endif



      case 'f':

#        if defined(_ACCEPT_FLOW)
	    on_off_flags.flowtrace_option = TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "f", 'e', ARG_STR_ARG);
#        endif
         break;

      case 'g':

#        if defined(_ACCEPT_CMD_ed_g)
	    on_off_flags.assembly_listing_file = TRUE;

            /* issue warning if cal file output is being overridden */  

            if (cmd_line_flags.assembly_output) {	 
               ntr_msg_queue(0, 388, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
               cmd_line_flags.assembly_output	= FALSE;  
               cmd_line_flags.binary_output	= TRUE;  
            }
#        else /* doc'd as -rg */
            ntr_msg_queue(0, 744, Log_Warning, 0, "g", 'r', ARG_STR_ARG);
#        endif
         break;


      case 'h':

#        if defined(_ACCEPT_CMD_ed_h)
	    on_off_flags.integer_1_and_2 = TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "h", 'e', ARG_STR_ARG);
#        endif
         break;


      case 'i':

#        if defined(_ACCEPT_CMD_ed_i)
            on_off_flags.indef_init = TRUE;

            if (on_off_flags.zero_init) {  /* Conflict - issue warning */
               on_off_flags.zero_init = FALSE;
               ntr_msg_queue(0, 1313, Log_Warning, 0, "ei\ne0", 0,MULT_STR_ARG);
            }
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "i", 'e', ARG_STR_ARG);
#        endif
         break;

      case 'j':

#        if defined(_ACCEPT_CMD_ed_j)
            on_off_flags.exec_doloops_once = TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "j", 'e', ARG_STR_ARG);
#        endif
         break;

      case 'k':
         on_off_flags.save_dot_i = TRUE;
         break;

      case 'm':
         on_off_flags.module_to_mod = TRUE;
         break;

      case 'n':
         on_off_flags.issue_ansi_messages = TRUE;
         break;

      case 'p':
         on_off_flags.enable_double_precision = TRUE;
         break;

      case 'q':
         on_off_flags.abort_on_100_errors = TRUE;
         break;

      case 'r':

#        if defined(_ACCEPT_CMD_ed_r)

            if (set_trunc_option) {

               /* Rounding(-er) /truncation (-t) conflict detected. */

               ntr_msg_queue(0, 75, Log_Warning, 0, "-er", 0, STR_ARG);
               set_trunc_option			= FALSE;
            }

            on_off_flags.round_mult_operations	= TRUE;
            cmd_line_flags.truncate_bits	= 0;
            set_round_option			= TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "r", 'e', ARG_STR_ARG);
#        endif
         break;

      case 't':
         on_off_flags.alloc_autos_on_stack = TRUE;
         break;

      case 'u':
         on_off_flags.eu	= TRUE;
         set_eu_option		= TRUE;
         break;
    
      case 'v':
         on_off_flags.save_all_vars = TRUE;
         break;

      case 'z':

#        if defined(_ACCEPT_CMD_ed_z) || defined(KEY) /* Bug 3405 */
	    on_off_flags.recognize_minus_zero = TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "z", 'e', ARG_STR_ARG);
#        endif
         break;

      case '0':
#        if defined(_ACCEPT_CMD_ed_0)
            on_off_flags.zero_init = TRUE;

            if (on_off_flags.indef_init) {  /* Conflict - issue warning */
               on_off_flags.indef_init = FALSE;
               ntr_msg_queue(0, 1313, Log_Warning, 0, "e0\nei", 0,MULT_STR_ARG);
            }

#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "0", 'e', ARG_STR_ARG);
#        endif
         break;

      case 'A':

#        if defined(_ACCEPT_CMD_ed_A)
            on_off_flags.MPP_apprentice = TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "A", 'e', ARG_STR_ARG);
#        endif
         break;
    
      case 'B':

         /* issue warning if cal output is being overridden */ 

         if (cmd_line_flags.assembly_output) { 
            ntr_msg_queue(0, 715, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
            cmd_line_flags.assembly_output = FALSE; 
         }

         cmd_line_flags.binary_output = TRUE;
         break;

      case 'C':

#        if defined(_ACCEPT_CMD_ed_C)
            on_off_flags.shared_to_private_coer = TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "C", 'e', ARG_STR_ARG);
#        endif
         break;

      case 'D':
#        if defined(_ACCEPT_CMD_ed_D)
            on_off_flags.all_debug = TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "D", 'e', ARG_STR_ARG);
#        endif
         break;

      case 'I':
         on_off_flags.implicit_none = TRUE;
         break;

      case 'L':
         on_off_flags.top_test_shortloops = TRUE;
         break;

      case 'N':
         on_off_flags.second_underscore = TRUE;
         break;

      case 'O':
         on_off_flags.underscoring = TRUE;
         break;

      case 'Q':
         on_off_flags.allow_leading_uscore = TRUE;
         break;

      case 'R':
         on_off_flags.recursive = TRUE;
         break;

      case 'S':

         /* issue warning if binary output is being overridden */  

         if (cmd_line_flags.binary_output) {	 
/*          PRINTMSG (0, 74, Log_Warning, 0);       LRR: driver now does this */
            cmd_line_flags.binary_output = FALSE;  
         }

         /* issue warning if cal file listing is being overridden */  

         if (on_off_flags.assembly_listing_file) {	 
            ntr_msg_queue(0, 911, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
            on_off_flags.assembly_listing_file = FALSE;  
         }
     
         cmd_line_flags.assembly_output = TRUE;
         break;

      case 'T':
# ifdef _FRONTEND_CONDITIONAL_COMP
         no_preprocessing = FALSE;
         on_off_flags.preprocess = TRUE;
# endif
         break;

      case 'U':
#        if defined(_ACCEPT_CMD_ed_U)
            on_off_flags.upper_case_names = TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "U", 'e', ARG_STR_ARG);
#        endif
         break;

      case 'X':
        
#        if defined(_ACCEPT_CMD_ed_X)
            on_off_flags.atexpert = TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "X", 'e', ARG_STR_ARG);
#        endif
         break;

      case 'Z':
# ifdef _FRONTEND_CONDITIONAL_COMP
         no_preprocessing = FALSE;
         on_off_flags.preprocess_only = TRUE;
         on_off_flags.preprocess = TRUE;
# endif
         break;
    
      default:
         err_str[0] = ch;
         err_str[1] = EOS;

         /* option has an invalid argument */

         ntr_msg_queue(0, 78, Log_Error, 0, err_str, 'e', ARG_STR_ARG);
         break;
      }	 /* switch */
   }  /* while */
 
   TRACE (Func_Exit, "process_e_option", NULL);

   return;

}  /* process_e_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process_O_option handles command line optimization options passed via *|
|*	argv (O switch).						      *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-O option arguments			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_O_option (char	*optargs,
			      int	 argc)

{
   		char		*cp;
   		boolean		 err;
		boolean		 first;
   extern	int		 optind;	/* defined in getopt  */


   TRACE (Func_Entry, "process_O_option", NULL);

   if (*optargs == '-') {

      /* Just a -O, issue warning, backup getopt and continue.  If      */
      /* optarg is a "-", the next item following -O is another option. */

      ntr_msg_queue(0, 1221, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
      --optind;
      return;
   }

   first	= TRUE;

   while (*optargs != EOS) {

      for (cp = optargs;
          *optargs != BLANK  &&  *optargs != COMMA  &&  *optargs != EOS;
          ++optargs);

      if (*optargs != EOS) {
	 *optargs = EOS;
	 *optargs++;
      }

      err = FALSE;

      switch (*cp) {

         case '0':
            if (EQUAL_STRS(cp, "0")) {
               opt_flags.support_lvl  = 0;
               set_support_lvl_option = TRUE;

               if (! set_scalar_option) {
                  opt_flags.scalar_lvl = Scalar_Lvl_0;
               }

#              if defined(_ACCEPT_VECTOR)

               if (! set_vector_option) {
                  opt_flags.vector_lvl = Vector_Lvl_0;
               }

#              endif

#              if defined(_ACCEPT_STREAM)

               if (! set_stream_option) {
                  opt_flags.stream_lvl = Stream_Lvl_0;
               }

#              endif


#              if defined(_ACCEPT_TASK)

               if (! set_task_option) {
                  opt_flags.task_lvl = Task_Lvl_0;
               }

#              endif

            }
            else {
               err = TRUE;
            }

            break;


         case '1':
            if (EQUAL_STRS(cp, "1")) {
               opt_flags.support_lvl  = 1;
               set_support_lvl_option = TRUE;

               if (! set_scalar_option) {
                  opt_flags.scalar_lvl = Scalar_Lvl_1;
               }

#              if defined(_ACCEPT_VECTOR)

               if (! set_vector_option) {
                  opt_flags.vector_lvl = Vector_Lvl_1;
               }

#              endif

#              if defined(_ACCEPT_STREAM)

               if (! set_stream_option) {

# if defined(_TARGET_SV2)
                  opt_flags.stream_lvl = Stream_Lvl_1;
# else
                  opt_flags.stream_lvl = Stream_Lvl_0;
# endif
               }

#              endif

#              if defined(_ACCEPT_TASK)

               if (! set_task_option) {
                  opt_flags.task_lvl = Task_Lvl_1;
               }

#              endif

            }
            else {
               err = TRUE;
            }

            break;


         case '2':
            if (EQUAL_STRS(cp, "2")) {
               opt_flags.support_lvl  = 2;
               set_support_lvl_option = TRUE;

               if (! set_scalar_option) {
                  opt_flags.scalar_lvl = Scalar_Lvl_2;
               }

#              if defined(_ACCEPT_STREAM)

               if (! set_stream_option) {

# if defined(_TARGET_SV2)
                  opt_flags.stream_lvl = Stream_Lvl_2;
# else
                  opt_flags.stream_lvl = Stream_Lvl_0;
# endif
               }

#              endif

#              if defined(_ACCEPT_VECTOR)

               if (! set_vector_option) {
                  opt_flags.vector_lvl = Vector_Lvl_2;
               }

#              endif

#              if defined(_ACCEPT_TASK)

               if (! set_task_option) {
                  opt_flags.task_lvl = Task_Lvl_1;
               }

#              endif

            }
            else {
               err = TRUE;
            }

            break;


         case '3':
            if (EQUAL_STRS(cp, "3")) {
               opt_flags.support_lvl  = 3;
               set_support_lvl_option = TRUE;
           
               if (! set_scalar_option) {
                  opt_flags.scalar_lvl = Scalar_Lvl_2;
               }

#              if defined(_ACCEPT_VECTOR)

               if (! set_vector_option) {
                  opt_flags.vector_lvl = Vector_Lvl_3;
               }

#              endif

#              if defined(_ACCEPT_STREAM)

               if (! set_stream_option) {

# if defined(_TARGET_SV2)
                  opt_flags.stream_lvl = Stream_Lvl_3;
# else
                  opt_flags.stream_lvl = Stream_Lvl_0;
# endif
               }

#              endif


#              if defined(_ACCEPT_TASK)

               if (! set_task_option) {
                  opt_flags.task_lvl = Task_Lvl_2;
               }

#              endif

            }
            else {
               err = TRUE;
            }

            break;


         case 'a':
            if (EQUAL_STRS(cp, "aggress")) {
               opt_flags.aggress = TRUE;
               set_aggress_option = TRUE;
            }
            else if (EQUAL_STRS(cp, "allfastint")) {

# if defined(_ACCEPT_CMD_O_FASTINT)
               opt_flags.set_allfastint_option = TRUE;

               if (set_i_option) {
                  ntr_msg_queue(0, 1192, Log_Warning, 0,
                                "-i\n-O allfastint\n-i",
                                0, MULT_STR_ARG);
               }
# else
               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "allfastint", 'O', ARG_STR_ARG);
# endif
            }
            else {
               err = TRUE;
            }

	    break;


         case 'b':
            if (! EQUAL_STRS(cp, "bl")) {
               err = TRUE;
            }
            else {

#              if defined(_ACCEPT_BL)

               opt_flags.bottom_load  = TRUE;
      	       set_bottom_load_option = TRUE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0, "bl", 'O', ARG_STR_ARG);

#              endif

            }

            break;
    

         case 'f':

            if (EQUAL_STRS(cp, "fastint")) {

# if defined(_ACCEPT_CMD_O_FASTINT)
               opt_flags.set_fastint_option = TRUE;
               if (set_i_option) {
                  ntr_msg_queue(0, 1192, Log_Warning, 0,
                                "-i\n-O fastint\n-i",
                                0, MULT_STR_ARG);
               }
# else
               ntr_msg_queue(0, 744, Log_Warning, 0,
                            "fastint", 'O', ARG_STR_ARG);
# endif
            }
            else if (EQUAL_STRS(cp, "fusion")) {

# if defined(_ACCEPT_CMD_O_FUSION)
               opt_flags.fusion	= TRUE;
# else
               ntr_msg_queue(0, 744, Log_Warning, 0,
                            "fusion", 'O', ARG_STR_ARG);
# endif
            }
            else {
               err = TRUE;
            }
	    break;


         case 'i': 

            if (EQUAL_STRS(cp, "ieeeconform")) {

               if (target_ieee) {
                  opt_flags.ieeeconform		= TRUE;
                  set_ieeeconform_option	= TRUE;
  	       } 
               else {
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                               "ieeeconform", 'O', ARG_STR_ARG);
  	       } 
  	    }
            else if ((strncmp (cp, "inlinefrom=", 11) == IDENTICAL)) {
               cp = cp+11;
               add_to_fp_table (cp, &inline_path_idx, 'O');
               set_inlinefrom_option = TRUE;
	    }
            else if (EQUAL_STRS(cp, "inline0")) {

#              if defined(_ACCEPT_INLINE)

               opt_flags.inline_lvl = Inline_Lvl_0;   

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "inline0", 'O', ARG_STR_ARG);

#              endif

  	    }
	    else if (EQUAL_STRS(cp, "inline1") ||
	             EQUAL_STRS(cp, "inlinee1")) {

#              if defined(_ACCEPT_INLINE)

               if (EQUAL_STRS(cp, "inlinee1")) {
                  opt_flags.extent_assert = TRUE;
               }

	       opt_flags.inline_lvl = Inline_Lvl_1;   

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "inline1", 'O', ARG_STR_ARG);

#              endif

	    }
	    else if (EQUAL_STRS(cp, "inline2") ||
	             EQUAL_STRS(cp, "inlinee2")) {

#              if defined(_ACCEPT_INLINE)

               if (EQUAL_STRS(cp, "inlinee2")) {
                  opt_flags.extent_assert = TRUE;
               }

               opt_flags.inline_lvl = Inline_Lvl_2;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "inline2", 'O', ARG_STR_ARG);

#              endif

	    }
	    else if (EQUAL_STRS(cp, "inline3") ||
	             EQUAL_STRS(cp, "inlinee3")) {

#              if defined(_ACCEPT_INLINE)

               if (EQUAL_STRS(cp, "inlinee3")) {
                  opt_flags.extent_assert = TRUE;
               }

               opt_flags.inline_lvl = Inline_Lvl_3;
               ntr_msg_queue(0, 1548, Log_Warning, 0,
                             "-O inline3", 0, STR_ARG);

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "inline3", 'O', ARG_STR_ARG);

#              endif
	    }
	    else if (EQUAL_STRS(cp, "inline4") ||
	             EQUAL_STRS(cp, "inlinee4")) {

#              if defined(_ACCEPT_INLINE)

               if (EQUAL_STRS(cp, "inlinee4")) {
                  opt_flags.extent_assert = TRUE;
               }

               opt_flags.inline_lvl = Inline_Lvl_4;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "inline4", 'O', ARG_STR_ARG);

#              endif
            }
	    else {  
	       err = TRUE;
	    }

            if (set_debug_option) {
               opt_flags.inline_lvl = Inline_Lvl_0;
            }

            break;


         case 'j':

            if (! EQUAL_STRS(cp, "jump")) {
               err = TRUE;
            }
            else {

#              if defined(_ACCEPT_CMD_O_JUMP)
               opt_flags.jump  = TRUE;
#              else
               ntr_msg_queue(0, 744, Log_Warning, 0, "jump", 'O', ARG_STR_ARG);
#              endif

            }

            break;

    
         case 'l': 

            if ( !EQUAL_STRS(cp, "loopalign")) {
               err = TRUE;
            }
            else {

#              if defined(_ACCEPT_CMD_O_LOOPALIGN)

               opt_flags.loopalign   = TRUE;
               set_loop_align_option = TRUE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "loopalign", 'O', ARG_STR_ARG);

#              endif

            }

            break;


         case 'm': 

            if (EQUAL_STRS(cp, "modinline")) {

#              if defined(_ACCEPT_INLINE)
               opt_flags.modinline = TRUE;
#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "modinline", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "msgs")) {
               opt_flags.msgs = TRUE;
            }
            else if (strncmp(cp, "mark", 4) == 0) {
               opt_flags.mark = TRUE;
               cp	      +=4;

               if (*cp != EOS) {

                  if (*cp != EQUAL) {
                     err = TRUE;
                  }
                  else {  /* Check strlen ??  KAY */
                     ++cp;
                     strcpy(opt_flags.mark_name.string, cp);
                  }
               }
            }
            else if (EQUAL_STRS(cp, "matmul_inline")) {

#              if defined(_ACCEPT_CMD_O_MATMUL_INLINE)
                  opt_flags.matmul_inline = TRUE;
#              else
                  PRINTMSG (0, 744, Log_Warning, 0, 'O', "matmul_inline");
#              endif

            }
            else if (EQUAL_STRS(cp, "mv_matmul_inline")) {

#              if defined(_ACCEPT_CMD_O_MATMUL_INLINE)
                  opt_flags.mv_matmul_inline = TRUE;
#              else
                  PRINTMSG (0, 744, Log_Warning, 0, 'O', "mv_matmul_inline");
#              endif

            }
            else { 
               err = TRUE;
            }

            break;


         case 'n': 

            if (EQUAL_STRS(cp, "negmsgs")) {
               opt_flags.neg_msgs = TRUE;
            }
            else if (EQUAL_STRS(cp, "noaggress")) {
               opt_flags.aggress = FALSE;
            }
            else if (EQUAL_STRS(cp, "nobl")) {

#              if defined(_ACCEPT_BL)

	       opt_flags.bottom_load = FALSE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0, "nobl", 'O', ARG_STR_ARG);

#              endif

            } 
            else if (EQUAL_STRS(cp, "nofastint")) {

# if defined(_ACCEPT_CMD_O_FASTINT)
               opt_flags.set_nofastint_option = TRUE;

               if (set_i_option) {
                  ntr_msg_queue(0, 1192, Log_Warning, 0,
                                "-i\n-O nofastint\n-i",
                                0, MULT_STR_ARG);
               }
# else
               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "nofastint", 'O', ARG_STR_ARG);
# endif

            }
            else if (EQUAL_STRS(cp, "nofusion")) {

# if defined(_ACCEPT_CMD_O_FUSION)
               opt_flags.fusion	= FALSE;
# else
               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "nofusion", 'O', ARG_STR_ARG);
# endif
            }
            else if (EQUAL_STRS(cp, "noieeeconform")) {

               if (target_ieee) {
                  opt_flags.ieeeconform		= FALSE;
                  set_ieeeconform_option	= TRUE;
  	       } 
               else {
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "noieeeconform", 'O', ARG_STR_ARG);
  	       } 
  	    }
            else if (EQUAL_STRS(cp, "nointerchange")) {
               opt_flags.nointerchange		= TRUE;
  	    }
            else if (EQUAL_STRS(cp, "nojump")) {

#              if defined(_ACCEPT_CMD_O_JUMP)

	       opt_flags.jump = FALSE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "nojump", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "noloopalign")) {

#              if defined(_ACCEPT_CMD_O_LOOPALIGN)

	       opt_flags.loopalign = FALSE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "noloopalign", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "nomodinline")) {

#              if defined(_ACCEPT_INLINE)
               opt_flags.modinline = FALSE;
#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "nomodinline", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "nomsgs")) {
               opt_flags.msgs = FALSE;
            }
            else if (EQUAL_STRS(cp, "nonegmsgs")) {
               opt_flags.neg_msgs = FALSE;
            }
            else if (EQUAL_STRS(cp, "nopattern")) {
	       opt_flags.pattern = FALSE;
            }
            else if (EQUAL_STRS(cp, "nooverindex")) {
               opt_flags.over_index = FALSE;
            }
            else if (EQUAL_STRS(cp, "norecurrence")) {
               opt_flags.recurrence = FALSE;
            }
            else if (EQUAL_STRS(cp, "notaskinner")) {

#              if defined(_ACCEPT_TASK)

	       opt_flags.taskinner = FALSE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "notaskinner", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "nothreshold")) {

#              if defined(_ACCEPT_TASK)

               opt_flags.threshold  = FALSE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "nothreshold", 'O', ARG_STR_ARG);

#              endif
            }
            else if (EQUAL_STRS(cp, "novsearch")) {

#              if defined(_ACCEPT_VSEARCH)

	       opt_flags.vsearch = FALSE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "novsearch", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "nozeroinc")) {

#              if defined(_ACCEPT_CMD_O_ZEROINC)

	       opt_flags.zeroinc = FALSE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "nozeroinc", 'O', ARG_STR_ARG);

#              endif

            }
            else {
               err = TRUE;
            }

            break;


         case 'o':

            if (EQUAL_STRS(cp, "overindex")) {
               opt_flags.over_index = TRUE;
            }
            else if (EQUAL_STRS(cp, "opt_info")) {

#              if defined(_ACCEPT_CMD_O_OPT_INFO)
                  opt_flags.opt_info	= TRUE;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "opt_info", 'O', ARG_STR_ARG);
#              endif
            }
            else {
               err = TRUE;
            }

            break;


         case 'p': 

            if (EQUAL_STRS(cp, "pattern")) {
               opt_flags.pattern  = TRUE;
               set_pattern_option = TRUE;
            }
            else if (EQUAL_STRS(cp, "pipeline0")) {

#              if defined(_ACCEPT_CMD_O_PIPELINE)
                  opt_flags.pipeline_lvl	= 0;
                  set_pipeline_option		= TRUE;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "pipeline0", 'O', ARG_STR_ARG);
#              endif
            }
            else if (EQUAL_STRS(cp, "pipeline1")) {

#              if defined(_ACCEPT_CMD_O_PIPELINE)
                  opt_flags.pipeline_lvl	= 1;
                  set_pipeline_option		= TRUE;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "pipeline1", 'O', ARG_STR_ARG);
#              endif
            }
            else if (EQUAL_STRS(cp, "pipeline2")) {

#              if defined(_ACCEPT_CMD_O_PIPELINE)
                  opt_flags.pipeline_lvl	= 2;
                  set_pipeline_option		= TRUE;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "pipeline2", 'O', ARG_STR_ARG);
#              endif
            }
            else if (EQUAL_STRS(cp, "pipeline3")) {

#              if defined(_ACCEPT_CMD_O_PIPELINE)
                  opt_flags.pipeline_lvl	= 3;
                  set_pipeline_option		= TRUE;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "pipeline3", 'O', ARG_STR_ARG);
#              endif
            }
            else {
               err = TRUE;
            }

            break;

    
         case 'r': 

            if (EQUAL_STRS(cp, "reshape")) {

#  if defined(_ACCEPT_CMD_O_RESHAPE)
               opt_flags.reshape_all_arrays	= TRUE;
               opt_flags.reshape		= TRUE;
# else
               ntr_msg_queue(0, 744, Log_Warning, 0,
                            "reshape", 'O', ARG_STR_ARG);
# endif
            }
            else if ((strncmp (cp, "reshape=", 8) == IDENTICAL)) {
               cp += 8;
               process_reshape_array(cp);
            }
            else if (! EQUAL_STRS(cp, "recurrence")) {
               err = TRUE;
            }
            else {
               opt_flags.recurrence  = TRUE;
               set_recurrence_option = TRUE;
            }

            break;


         case 's':

            if (EQUAL_STRS(cp, "scalar0")) {
               opt_flags.scalar_lvl = Scalar_Lvl_0;
               set_scalar_option    = TRUE;
            }
            else if (EQUAL_STRS(cp, "scalar1")) {
               opt_flags.scalar_lvl = Scalar_Lvl_1;
               set_scalar_option    = TRUE;
            }
            else if (EQUAL_STRS(cp, "scalar2")) {
               opt_flags.scalar_lvl = Scalar_Lvl_2;
               set_scalar_option    = TRUE;
            }
            else if (EQUAL_STRS(cp, "scalar3")) {
               opt_flags.scalar_lvl = Scalar_Lvl_3;
               set_scalar_option    = TRUE;
            }
            else if (EQUAL_STRS(cp, "split0")) {

#              if defined(_ACCEPT_SPLIT)
                  opt_flags.split_lvl = Split_Lvl_0;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "split0", 'O', ARG_STR_ARG);
#              endif
            }
            else if (EQUAL_STRS(cp, "split1")) {

#              if defined(_ACCEPT_SPLIT)
                  opt_flags.split_lvl = Split_Lvl_1;

#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "split1", 'O', ARG_STR_ARG);
#              endif
            }
            else if (EQUAL_STRS(cp, "split2")) {

#              if defined(_ACCEPT_SPLIT)
                  opt_flags.split_lvl = Split_Lvl_2;
#             else 
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "split2", 'O', ARG_STR_ARG);
#              endif
            }
            else if (EQUAL_STRS(cp, "shortcircuit0")) {

#              if defined(_ACCEPT_SHORTCIRCUIT)
                  opt_flags.short_circuit_lvl = Short_Circuit_Off;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "shortcircuit0", 'O', ARG_STR_ARG);
#              endif

            }
            else if (EQUAL_STRS(cp, "shortcircuit1")) {

#              if defined(_ACCEPT_SHORTCIRCUIT)
                  opt_flags.short_circuit_lvl = Short_Circuit_Present;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "shortcircuit1", 'O', ARG_STR_ARG);
#              endif

            }
            else if (EQUAL_STRS(cp, "shortcircuit2")) {

#              if defined(_ACCEPT_SHORTCIRCUIT)
                  opt_flags.short_circuit_lvl = Short_Circuit_Left_Right;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "shortcircuit2", 'O', ARG_STR_ARG);
#              endif

            }
            else if (EQUAL_STRS(cp, "shortcircuit3")) {

#              if defined(_ACCEPT_SHORTCIRCUIT)
                  opt_flags.short_circuit_lvl = Short_Circuit_Functions;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "shortcircuit3", 'O', ARG_STR_ARG);
#              endif

            }
            else if (EQUAL_STRS(cp, "stream0")) {

               if (accept_stream) { 
                  set_stream_option	= TRUE;
                  opt_flags.stream_lvl	= Stream_Lvl_0;
               }
               else {
                  PRINTMSG (0, 744, Log_Warning, 0, 'O', "stream0");
               }
            }
            else if (EQUAL_STRS(cp, "stream1")) {

               if (accept_stream) { 
                  set_stream_option	= TRUE;
                  opt_flags.stream_lvl	= Stream_Lvl_1;
               }
               else {
                  PRINTMSG (0, 744, Log_Warning, 0, 'O', "stream1");
               }
            }
            else if (EQUAL_STRS(cp, "stream2")) {

               if (accept_stream) { 
                  set_stream_option	= TRUE;
                  opt_flags.stream_lvl	= Stream_Lvl_2;
               }
               else {
                  PRINTMSG (0, 744, Log_Warning, 0, 'O', "stream2");
               }
            }
            else if (EQUAL_STRS(cp, "stream3")) {

               if (accept_stream) { 
                  set_stream_option	= TRUE;
                  opt_flags.stream_lvl	= Stream_Lvl_3;
               }
               else {
                  PRINTMSG (0, 744, Log_Warning, 0, 'O', "stream3");
               }
            }
            else {
               err = TRUE;
            }

            break;


         case 't': 

            if (EQUAL_STRS(cp, "task0")) {

#              if defined(_ACCEPT_TASK)

               opt_flags.task_lvl = Task_Lvl_0;
               set_task_option	  = TRUE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0, "task0", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "task1")) {

#              if defined(_ACCEPT_TASK)

               opt_flags.task_lvl = Task_Lvl_1;
               set_task_option	  = TRUE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0, "task1", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "task2")) {

#              if defined(_ACCEPT_TASK)

               opt_flags.task_lvl = Task_Lvl_2;
               set_task_option	  = TRUE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0, "task2", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "task3")) {

#              if defined(_ACCEPT_TASK)

               opt_flags.task_lvl = Task_Lvl_3;
               set_task_option	  = TRUE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0, "task3", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "taskinner")) {

#              if defined(_ACCEPT_TASK)

               opt_flags.taskinner  = TRUE;
               set_taskinner_option = TRUE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "taskinner", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "threshold")) {

#              if defined(_ACCEPT_TASK)
               opt_flags.threshold  = TRUE;
#              else
               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "threshold", 'O', ARG_STR_ARG);
#              endif
            }
            else {
               err = TRUE;
            }

            break;


         case 'u':

            if (EQUAL_STRS(cp, "unroll0")) {

#              if defined(_ACCEPT_CMD_O_UNROLL)
	          opt_flags.unroll_lvl = Unroll_Lvl_0;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "unroll0", 'O', ARG_STR_ARG);
#              endif
            }
            else if (EQUAL_STRS(cp, "unroll1")) {
   
#              if defined(_ACCEPT_CMD_O_UNROLL)
	          opt_flags.unroll_lvl = Unroll_Lvl_1;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "unroll1", 'O', ARG_STR_ARG);
#              endif
            }
            else if (EQUAL_STRS(cp, "unroll2")) {

#              if defined(_ACCEPT_CMD_O_UNROLL)
	          opt_flags.unroll_lvl = Unroll_Lvl_2;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "unroll2", 'O', ARG_STR_ARG);
#              endif
            }
            else {
               err = TRUE;
            }
            break;


         case 'v':

            if (EQUAL_STRS(cp, "vector0")) {

#              if defined(_ACCEPT_VECTOR)
	          opt_flags.vector_lvl = Vector_Lvl_0;
      	          set_vector_option    = TRUE;
#              else
                  ntr_msg_queue(0, 744, Log_Warning, 0,
                                "vector0", 'O', ARG_STR_ARG);
#              endif

            }
            else if (EQUAL_STRS(cp, "vector1")) {

#              if defined(_ACCEPT_VECTOR)

               opt_flags.vector_lvl = Vector_Lvl_1;
               set_vector_option    = TRUE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "vector1", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "vector2")) {

#              if defined(_ACCEPT_VECTOR)

               opt_flags.vector_lvl = Vector_Lvl_2;
               set_vector_option    = TRUE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "vector2", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "vector3")) {

#              if defined(_ACCEPT_VECTOR)

               opt_flags.vector_lvl = Vector_Lvl_3;
               set_vector_option    = TRUE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "vector3", 'O', ARG_STR_ARG);

#              endif

            }
            else if (EQUAL_STRS(cp, "vsearch")) {

#              if defined(_ACCEPT_VSEARCH)

	       opt_flags.vsearch  = TRUE;
	       set_vsearch_option = TRUE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "vsearch", 'O', ARG_STR_ARG);

#              endif

            }
            else {
               err = TRUE;
            }

            break;


         case 'z':

            if (! EQUAL_STRS(cp, "zeroinc")) {
               err = TRUE;
            }
            else {

#              if defined(_ACCEPT_CMD_O_ZEROINC)

               opt_flags.zeroinc  = TRUE;
               set_zeroinc_option = TRUE;

#              else

               ntr_msg_queue(0, 744, Log_Warning, 0,
                             "zeroinc", 'O', ARG_STR_ARG);

#              endif

            }

            break;


         default:
            err = TRUE;
            break;
      }                            /* End -O argument switch.                 */


      /* Option has an invalid argument?			              */

      if (err) {

         if (first && (argc == optind)) {

            /* We're seeing something similar to -O end.f        */
            /* This is the first time through the while loop     */
            /* and there is nothing on the commandline following */
            /* the -O and its argument.  We will assume this is  */
            /* a lone -O and that the argument is really the     */
            /* file to be compiled.  We could also be seeing     */
            /* -O vctor where the input is expected to be from   */
            /* stdin, but we'll go with the first choice because */
            /* it could be correct.  If we didn't we might be    */
            /* issuing an error for a valid commandline.         */

            ntr_msg_queue(0, 1221, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
            --optind;
         }
         else {
            ntr_msg_queue(0, 78, Log_Error, 0, cp, 'O', ARG_STR_ARG);
         }
      }

      first	= FALSE;
   }                               /* End -O argument WHILE loop.	      */

   TRACE (Func_Exit, "process_O_option", NULL);

   return;

}  /* process_O_option */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Validate_O_option is called to check the precedence of optimization   *|
|*	options selected.						      *|
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

static void validate_O_option (void)

{
   char		 msg_str[30];
   boolean	 option_conflict	= FALSE;
   int		 scalar;
   int		 task;

#  if defined(_ACCEPT_VECTOR)
   int		 vector;
#  endif


   TRACE (Func_Entry, "validate_O_option", NULL);

   /* If a general optimization option (-O<number>) was specified, no         */
   /* specific option (such as -Otask1) can be specified, even if the specific*/
   /* option is one that would be implied by the general option.	      */

   if (set_support_lvl_option) {

      if (set_scalar_option) {
         ntr_msg_queue(0, 1535, Log_Error, 0, 
                       scalar_lvl_str[opt_flags.scalar_lvl],
                       (long) opt_flags.support_lvl, ARG_STR_ARG);
         option_conflict = TRUE;
      }

      if (set_vector_option) {
         ntr_msg_queue(0, 1535, Log_Error, 0, 
                       vector_lvl_str[opt_flags.vector_lvl],
                       (long) opt_flags.support_lvl, ARG_STR_ARG);
         option_conflict = TRUE;
      }

      if (set_task_option) {
         ntr_msg_queue(0, 1535, Log_Error, 0, 
                       task_lvl_str[opt_flags.task_lvl],
                       (long) opt_flags.support_lvl, ARG_STR_ARG);
         option_conflict = TRUE;
      }
   }

   if (option_conflict) {
      goto EXIT;
   }

   /* Need to check for errors and possibly reset defaults.  An error is set  */
   /* if the user requests an invalid combination, but if a user requests a   */
   /* valid combination any options not set by the user are adjusted and a    */
   /* warning message is issued.  The scalar option is always allowed.  This  */
   /* code only needs to be executed if another option, task or vector is     */
   /* allowed.     							      */

# ifdef _ACCEPT_TASK

   if (set_taskinner_option  &&  opt_flags.taskinner) {

      if (set_task_option  ||  set_support_lvl_option) {

         if (opt_flags.task_lvl < Task_Lvl_2) {
            ntr_msg_queue(0, 1182, Log_Error, 0, (char *) NULL, 0, NO_ARG);
         }
      }
      else {
         ntr_msg_queue(0, 1182, Log_Error, 0, (char *) NULL, 0, NO_ARG);
      }
   }

# endif


#  if defined(_ACCEPT_TASK) && defined(_ACCEPT_VECTOR)

   if (! set_vector_option  &&  ! set_scalar_option  &&  ! set_task_option) {
      goto CONTINUE;   /* Nothing is set.  No checking needs to be done. */
   }

   if (set_vector_option && set_scalar_option && set_task_option) {

      if (opt_flags.scalar_lvl >= opt_flags.vector_lvl &&
          opt_flags.vector_lvl >= opt_flags.task_lvl) {

         /* okay - intentionally blank */
      }
      else if (opt_flags.task_lvl > Task_Lvl_1 &&
               opt_flags.task_lvl > opt_flags.vector_lvl) {
         COPY_LVL_STR(0, vector, opt_flags.vector_lvl, NEWLINE);
         COPY_LVL_STR(8, task, opt_flags.task_lvl, NULL_CHAR);
         ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
      }
      else if (opt_flags.scalar_lvl == Scalar_Lvl_2 &&
               opt_flags.vector_lvl == Vector_Lvl_3) {

         /* Okay - intentionally blank */
         /* Rule exception - scalar2, vector3  */
      }
      else if (opt_flags.scalar_lvl < opt_flags.vector_lvl) {
         COPY_LVL_STR(0, scalar, opt_flags.scalar_lvl, NEWLINE);
         COPY_LVL_STR(8, vector, opt_flags.vector_lvl, NULL_CHAR);
         ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
      }
   }
   else if (set_vector_option  &&  set_scalar_option) {

      if (opt_flags.scalar_lvl == Scalar_Lvl_2  &&
          opt_flags.vector_lvl == Vector_Lvl_3) {

         /* Okay - intentionally blank */
         /* Rule exception - scalar2, vector3  */
         /* If vector3, task can be anything */
      }
      else if (opt_flags.scalar_lvl < opt_flags.vector_lvl) {
         COPY_LVL_STR(0, scalar, opt_flags.scalar_lvl, NEWLINE);
         COPY_LVL_STR(8, vector, opt_flags.vector_lvl, NULL_CHAR);
         ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
      }
      else if (opt_flags.task_lvl > opt_flags.vector_lvl  &&
               opt_flags.task_lvl > Task_Lvl_1) {
         task = (opt_flags.vector_lvl == Vector_Lvl_0) ?
                 Task_Lvl_1 : opt_flags.vector_lvl;

         COPY_LVL_STR(0, scalar, opt_flags.scalar_lvl, NEWLINE);
         COPY_LVL_STR(8, vector, opt_flags.vector_lvl, NEWLINE);
         COPY_LVL_STR(16, task, opt_flags.task_lvl, NEWLINE);
         COPY_LVL_STR(22, task, task, NULL_CHAR);

         ntr_msg_queue(0, 1064, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
         opt_flags.task_lvl = task;
      }
   }
   else if (set_scalar_option  &&  set_task_option) {

      if (opt_flags.task_lvl == Task_Lvl_3  &&
          opt_flags.scalar_lvl == Scalar_Lvl_2) {

         /* Rule exception - Scalar2, Task3 is okay. */

         if (opt_flags.vector_lvl != Vector_Lvl_3) {
            COPY_LVL_STR(0, scalar, opt_flags.scalar_lvl, NEWLINE);
            COPY_LVL_STR(8, task, opt_flags.task_lvl, NEWLINE);
            COPY_LVL_STR(14, vector, opt_flags.vector_lvl, NEWLINE);
            COPY_LVL_STR(22, vector, Vector_Lvl_3, NULL_CHAR);

            ntr_msg_queue(0, 1064, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
            opt_flags.vector_lvl = Vector_Lvl_3;
         }
      }
      else if (opt_flags.task_lvl <= Task_Lvl_1) {

         /* Just check scalar and vector */

         if (opt_flags.vector_lvl > opt_flags.scalar_lvl) {
            vector = opt_flags.scalar_lvl;
            COPY_LVL_STR(0, scalar, opt_flags.scalar_lvl, NEWLINE);
            COPY_LVL_STR(8, task, opt_flags.task_lvl, NEWLINE);
            COPY_LVL_STR(14, vector, opt_flags.vector_lvl, NEWLINE);
            COPY_LVL_STR(22, vector, vector, NULL_CHAR);

            ntr_msg_queue(0, 1064, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
            opt_flags.vector_lvl = vector;
         }
      }
      else if (opt_flags.task_lvl > opt_flags.scalar_lvl) {
         COPY_LVL_STR(0, scalar, opt_flags.scalar_lvl, NEWLINE);
         COPY_LVL_STR(8, task, opt_flags.task_lvl, NULL_CHAR);
         ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
      }
      else if (opt_flags.task_lvl > opt_flags.vector_lvl) {
         vector = opt_flags.task_lvl;
         COPY_LVL_STR(0, scalar, opt_flags.scalar_lvl, NEWLINE);
         COPY_LVL_STR(8, task, opt_flags.task_lvl, NEWLINE);
         COPY_LVL_STR(14, vector, opt_flags.vector_lvl, NEWLINE);
         COPY_LVL_STR(22, vector, vector, NULL_CHAR);
         ntr_msg_queue(0, 1064, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);

         opt_flags.vector_lvl = vector;
      }
   }
   else if (set_vector_option  &&  set_task_option) {

      if (opt_flags.task_lvl > Task_Lvl_1 &&
          opt_flags.task_lvl > opt_flags.vector_lvl) {
         COPY_LVL_STR(0, vector, opt_flags.vector_lvl, NEWLINE);
         COPY_LVL_STR(8, task, opt_flags.task_lvl, NULL_CHAR);
         ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
      }
      else if (opt_flags.vector_lvl > opt_flags.scalar_lvl) {

         if (opt_flags.scalar_lvl == Scalar_Lvl_2) {

            /* Rule exception - scalar2, vector3   If vector is         */
            /* Vector_Lvl_3 then task can be anything so this is legal. */
         }
         else {
            scalar = (opt_flags.vector_lvl == Vector_Lvl_3) ?
                     Scalar_Lvl_2 : opt_flags.vector_lvl;
            COPY_LVL_STR(0, vector, opt_flags.vector_lvl, NEWLINE);
            COPY_LVL_STR(8, task, opt_flags.task_lvl, NEWLINE);
            COPY_LVL_STR(14, scalar, opt_flags.scalar_lvl, NEWLINE);
            COPY_LVL_STR(22, scalar, scalar, NULL_CHAR);
            ntr_msg_queue(0, 1064, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);

            opt_flags.scalar_lvl = scalar;
         }
      }
   }
   else if (set_scalar_option) {

      if (opt_flags.vector_lvl > opt_flags.scalar_lvl) { 
         vector = opt_flags.scalar_lvl;
         COPY_LVL_STR(0, scalar, opt_flags.scalar_lvl, NEWLINE);
         COPY_LVL_STR(8, vector, opt_flags.vector_lvl, NEWLINE);
         COPY_LVL_STR(16, vector, vector, NULL_CHAR);

         ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
         opt_flags.vector_lvl = vector;
      }

      if (opt_flags.task_lvl > opt_flags.vector_lvl  && 
          opt_flags.task_lvl > Task_Lvl_1) {
         task = (opt_flags.vector_lvl == Vector_Lvl_0) ?
                 Task_Lvl_1 : opt_flags.vector_lvl;
         COPY_LVL_STR(0, vector, opt_flags.vector_lvl, NEWLINE);
         COPY_LVL_STR(8, task, opt_flags.task_lvl, NEWLINE);
         COPY_LVL_STR(14, task, task, NULL_CHAR);

         ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
         opt_flags.task_lvl = task;
      }
   }
   else if (set_vector_option) {

      if (opt_flags.vector_lvl > opt_flags.scalar_lvl  && 
          opt_flags.scalar_lvl != Scalar_Lvl_2) {

         /* Raise scalar lvl unless vector is level 3 */

         scalar = (opt_flags.vector_lvl == Vector_Lvl_3) ?
                   Scalar_Lvl_2 : opt_flags.vector_lvl;
         COPY_LVL_STR(0, vector, opt_flags.vector_lvl, NEWLINE);
         COPY_LVL_STR(8, scalar, opt_flags.scalar_lvl, NEWLINE);
         COPY_LVL_STR(16, scalar, scalar, NULL_CHAR);

         ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
         opt_flags.scalar_lvl = scalar;
      }

      if (opt_flags.task_lvl > opt_flags.vector_lvl  && 
          opt_flags.task_lvl > Task_Lvl_1) {

         task = (opt_flags.vector_lvl == Vector_Lvl_0) ?
                 Task_Lvl_1 : opt_flags.vector_lvl;
         COPY_LVL_STR(0, vector, opt_flags.vector_lvl, NEWLINE);
         COPY_LVL_STR(8, task, opt_flags.task_lvl, NEWLINE);
         COPY_LVL_STR(14, task, task, NULL_CHAR);

         ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
         opt_flags.task_lvl = task;
      }
   }
   else if (set_task_option && opt_flags.task_lvl > Task_Lvl_1) {

      if (opt_flags.scalar_lvl < opt_flags.task_lvl  && 
          opt_flags.scalar_lvl != Scalar_Lvl_2) {
         scalar = (opt_flags.task_lvl == Task_Lvl_3) ?
                   Scalar_Lvl_2 : opt_flags.task_lvl;
         COPY_LVL_STR(0, task, opt_flags.task_lvl, NEWLINE);
         COPY_LVL_STR(6, scalar, opt_flags.scalar_lvl, NEWLINE);
         COPY_LVL_STR(14, scalar, scalar, NULL_CHAR);

         ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
         opt_flags.scalar_lvl = scalar;
      }

      if (opt_flags.task_lvl > opt_flags.vector_lvl) { 
         vector = opt_flags.task_lvl;
         COPY_LVL_STR(0, task, opt_flags.task_lvl, NEWLINE);
         COPY_LVL_STR(6, vector, opt_flags.vector_lvl, NEWLINE);
         COPY_LVL_STR(14, vector, vector, NULL_CHAR);

         ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
         opt_flags.vector_lvl = vector;
      }
   }

#  elif defined(_ACCEPT_TASK) && !defined(_ACCEPT_VECTOR)

   if (opt_flags.task_lvl > Task_Lvl_1) {

       /* If task level is 0 or 1, scalar can be anything */

      if (set_scalar_option  &&  set_task_option) {

         if (opt_flags.task_lvl > opt_flags.scalar_lvl  &&
             opt_flags.scalar_lvl != Scalar_Lvl_2) {

            /* Scalar2, Task3 is an exception and is legal. */

            COPY_LVL_STR(0, scalar, opt_flags.scalar_lvl, NEWLINE);
            COPY_LVL_STR(8, task, opt_flags.task_lvl, NULL_CHAR);
            ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
         }
      }
      else if (set_scalar_option) {

         if (opt_flags.task_lvl > opt_flags.scalar_lvl) { 
            task = (opt_flags.scalar_lvl == Scalar_Lvl_0) ?
                    Task_Lvl_1 : opt_flags.scalar_lvl;
            COPY_LVL_STR(0, scalar, opt_flags.scalar_lvl, NEWLINE);
            COPY_LVL_STR(8, task, opt_flags.task_lvl, NEWLINE);
            COPY_LVL_STR(14, task, task, NULL_CHAR);

            ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
            opt_flags.task_lvl = task;
         }
      }
      else if (set_task_option) {

         if (opt_flags.task_lvl > opt_flags.scalar_lvl) { 
            scalar = (opt_flags.task_lvl == Task_Lvl_3) ?
                      Scalar_Lvl_2 : opt_flags.task_lvl;
            COPY_LVL_STR(0, task, opt_flags.task_lvl, NEWLINE);
            COPY_LVL_STR(6, scalar, opt_flags.scalar_lvl, NEWLINE);
            COPY_LVL_STR(14, scalar, scalar, NULL_CHAR);

            ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
            opt_flags.scalar_lvl = scalar;
         }
      }
   }

#  endif
CONTINUE:


   if (opt_flags.scalar_lvl == Scalar_Lvl_0) {

      if (set_aggress_option  &&  opt_flags.aggress) {
         COPY_MSG_STR(0, (set_scalar_option) ? "scalar0" : "0", NEWLINE);
         COPY_MSG_STR((set_scalar_option) ? 8 : 2, "aggress", NULL_CHAR);
         ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
      }

      opt_flags.aggress = FALSE;

      if (set_bottom_load_option  &&  opt_flags.bottom_load) { 
         COPY_MSG_STR(0, (set_scalar_option) ? "scalar0" : "0", NEWLINE);
         COPY_MSG_STR((set_scalar_option) ? 8 : 2, "bl", NULL_CHAR);
         ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
      }

      opt_flags.bottom_load = FALSE;

      if (set_recurrence_option && opt_flags.recurrence) {
         COPY_MSG_STR(0, (set_scalar_option) ? "scalar0" : "0", NEWLINE);
         COPY_MSG_STR((set_scalar_option) ? 8 : 2, "recurrence", NULL_CHAR);
         ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
      }

      opt_flags.recurrence = FALSE;

      if (set_zeroinc_option  &&  opt_flags.zeroinc) {
         COPY_MSG_STR(0, (set_scalar_option) ? "scalar0" : "0", NEWLINE);
         COPY_MSG_STR((set_scalar_option) ? 8 : 2, "zeroinc", NULL_CHAR);
         ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
      }

      opt_flags.zeroinc = FALSE;
   }
   else if (opt_flags.scalar_lvl == Scalar_Lvl_3) {

      if (! set_bottom_load_option) {
         opt_flags.bottom_load = TRUE;
      }
   }

# if defined(_ACCEPT_VECTOR)

   if (opt_flags.vector_lvl == Vector_Lvl_0) {

      if (set_vsearch_option  &&  opt_flags.vsearch) { 
         COPY_MSG_STR(0, (set_vector_option) ? "vector0" : "0", NEWLINE);
         COPY_MSG_STR((set_vector_option) ? 8 : 2, "vsearch", NULL_CHAR);
         ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
      }

      opt_flags.vsearch = FALSE;
   
# if defined(_TARGET_OS_UNICOS)

      if (set_pattern_option  &&  opt_flags.pattern) {
         COPY_MSG_STR(0, (set_vector_option) ? "vector0" : "0", NEWLINE);
         COPY_MSG_STR((set_vector_option) ? 8 : 2, "pattern", NULL_CHAR);
         ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
      }

      opt_flags.pattern = FALSE;
# endif

   }

# if defined(_TARGET_OS_UNICOS)
   else if (opt_flags.vector_lvl == Vector_Lvl_1) {

      if (set_pattern_option  &&  opt_flags.pattern) {
         COPY_MSG_STR(0, (set_vector_option) ? "vector1" : "1", NEWLINE);
         COPY_MSG_STR((set_vector_option) ? 8 : 2, "pattern", NULL_CHAR);
         ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
      }

      opt_flags.pattern = FALSE;
   }
# endif

# endif

# if defined(_ACCEPT_STREAM)
   if (opt_flags.stream_lvl > Stream_Lvl_0 && cmd_line_flags.co_array_fortran) {
      PRINTMSG (0, 1570, Log_Error, 0, "-Z");
   }

   if (accept_stream && opt_flags.stream_lvl > Stream_Lvl_0) {

      /* Stream_Lvl_1 requires at least scalar1 and vector1                 */
      /* Stream_Lvl_2 and Stream_Lvl_3 require at least scalar2 and vector2 */

      if (opt_flags.stream_lvl > opt_flags.scalar_lvl &&
          opt_flags.scalar_lvl < Scalar_Lvl_2) {

         if (set_scalar_option && set_stream_option) {

            /* The two are set wrong - issue an error */

            COPY_LVL_STR(0, stream, opt_flags.stream_lvl, NEWLINE);
            COPY_LVL_STR(8, scalar, opt_flags.scalar_lvl, NULL_CHAR);
            ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
            option_conflict	= TRUE;
         }
         else if (!set_scalar_option  && !set_stream_option) {

            /* The defaults are set wrong - change them */
            /* Maybe this should be an INTERNAL error?? */

            opt_flags.stream_lvl = opt_flags.scalar_lvl;
         }
         else if (set_scalar_option) {
            COPY_LVL_STR(0, scalar, opt_flags.scalar_lvl, NEWLINE);
            COPY_LVL_STR(8, stream, opt_flags.stream_lvl, NEWLINE);
            COPY_LVL_STR(16, stream, opt_flags.scalar_lvl, NULL_CHAR);

            ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
            opt_flags.stream_lvl = opt_flags.scalar_lvl;
         }
         else if (set_stream_option) {
            COPY_LVL_STR(0, stream, opt_flags.stream_lvl, NEWLINE);
            COPY_LVL_STR(8, scalar, opt_flags.scalar_lvl, NEWLINE);

            if (opt_flags.stream_lvl == Stream_Lvl_3) {
               COPY_LVL_STR(16, scalar, Stream_Lvl_2, NULL_CHAR);

               ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
               opt_flags.scalar_lvl = Stream_Lvl_2;
            }
            else {
               COPY_LVL_STR(16, scalar, opt_flags.stream_lvl, NULL_CHAR);

               ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
               opt_flags.scalar_lvl = opt_flags.stream_lvl;
            }
         }
      }


      if (option_conflict) {
         goto EXIT;
      }

      if (opt_flags.stream_lvl > opt_flags.vector_lvl &&
          opt_flags.vector_lvl < Vector_Lvl_2) {

         if (set_vector_option && set_stream_option) {

            /* The two are set wrong - issue an error */

            COPY_LVL_STR(0, stream, opt_flags.stream_lvl, NEWLINE);
            COPY_LVL_STR(8, vector, opt_flags.vector_lvl, NULL_CHAR);
            ntr_msg_queue(0, 99, Log_Error, 0, msg_str, 0, MULT_STR_ARG);
            option_conflict	= TRUE;
         }
         else if (!set_vector_option  && !set_stream_option) {

            /* The defaults are set wrong - change them */
            /* Maybe this should be an INTERNAL error?? */

            opt_flags.stream_lvl = opt_flags.vector_lvl;
         }
         else if (set_vector_option) {
            COPY_LVL_STR(0, vector, opt_flags.vector_lvl, NEWLINE);
            COPY_LVL_STR(8, stream, opt_flags.stream_lvl, NEWLINE);
            COPY_LVL_STR(16, stream, opt_flags.vector_lvl, NULL_CHAR);

            ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
            opt_flags.stream_lvl = opt_flags.vector_lvl;
         }
         else if (set_stream_option) {
            COPY_LVL_STR(0, stream, opt_flags.stream_lvl, NEWLINE);
            COPY_LVL_STR(8, vector, opt_flags.vector_lvl, NEWLINE);

            if (opt_flags.stream_lvl == Stream_Lvl_3) {
               COPY_LVL_STR(16, vector, Stream_Lvl_2, NULL_CHAR);

               ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
               opt_flags.vector_lvl = Stream_Lvl_2;
            }
            else {
               COPY_LVL_STR(16, vector, opt_flags.stream_lvl, NULL_CHAR);

               ntr_msg_queue(0, 1068, Log_Warning, 0, msg_str, 0, MULT_STR_ARG);
               opt_flags.vector_lvl = opt_flags.stream_lvl;
            }
         }
      }

      if (option_conflict) {
         goto EXIT;
      }
   }

# endif

# if defined(_ACCEPT_INLINE)

   if (opt_flags.inline_lvl == Inline_Lvl_0 &&
       set_inlinefrom_option) {
      opt_flags.inline_lvl = Inline_Lvl_4;
   }

   if (set_debug_option) {
      opt_flags.inline_lvl = Inline_Lvl_0;
   }

# endif

# if defined(_TARGET_OS_UNICOS)
      /* Task0 disables open mp on pvp */

      if (opt_flags.task_lvl == Task_Lvl_0) {
         dump_flags.open_mp = FALSE;
      }
      else {
         dump_flags.open_mp = TRUE;
      }

# endif

EXIT:

   TRACE (Func_Exit, "validate_O_option", NULL);

   return;
 
}  /* validate_O_option */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process_t_option handles the command line truncation option passed via*|
|*	argv (t switch).						      *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-t option argument			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_t_option (char *optargs)
 
{
   char *opt_chk;
   int	 trunc;


   TRACE (Func_Entry, "process_t_option", NULL);

   /* issue warning if rounding option is being overridden */

   if (set_round_option) { /* Rounding/truncation conflict detected. */
      ntr_msg_queue(0, 75, Log_Warning, 0, "-t", 0, STR_ARG);
      set_round_option = FALSE;
   }

   /* validate that -t arg contains only digits */

   opt_chk = optargs;

   while (isdigit(*opt_chk)) {
      opt_chk++;
   }

   trunc = atoi (optargs);

   if (*opt_chk != EOS  ||  trunc > MAX_TRUNCATION_BITS) {
      ntr_msg_queue(0, 78, Log_Error, 0, optargs, 't', ARG_STR_ARG);
   }
   else {

# if defined(_TARGET_OS_SOLARIS)

      if (trunc > 0) {
         ntr_msg_queue(0, 744, Log_Warning, 0, optargs, 't', ARG_STR_ARG);
         trunc = 0;
      }
# endif

      on_off_flags.round_mult_operations	= FALSE;
      cmd_line_flags.truncate_bits		= trunc;
   }

   set_trunc_option = TRUE;

   TRACE (Func_Exit, "process_t_option", NULL);

   return;

}  /* process_t_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process_u_option handles command line dump options passed via argv    *|
|*	(u switch).							      *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-u option arguments			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_u_option (char *optargs)

{
   char		*cp;
   boolean	 non_debug_ok	= FALSE;

   TRACE (Func_Entry, "process_u_option", NULL);

   while (*optargs != EOS) {
#ifdef KEY
      // Bug 933
      // If user file name has ",", then "," is a legitimate part of the file
      // name in the -u option, e.g., "-ufile=b,b.l".  Spaces in file names are
      // encoded as '\ '.
      if (strncmp (optargs, "file=", 5) == IDENTICAL) {
	char *temp = malloc (strlen (optargs) + 1);
	char *t = temp;
        for (; *optargs != BLANK && *optargs != EOS; ++optargs) {
	  // Convert '\ ' to ' '.  Convert '\\' to '\'.
	  if (*optargs == '\\')
	    optargs++;
	  *t++ = *optargs;
	}
        *t = '\0';
	cp = temp;
      } else
#endif
      for (cp = optargs;
	   *optargs != BLANK  &&  *optargs != COMMA  &&  *optargs != EOS;
	   ++optargs);

      if (*optargs != EOS) {
	 *optargs = EOS;
	 *optargs++;
      }

      if (EQUAL_STRS(cp, "help")) {			/* Describe all the   */
	 dump_flags.help_dbg	= TRUE;			/* options.           */
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "abort_ansi")) {          /* Abort on first ANSI*/
         dump_flags.abort_on_ansi = TRUE;
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "no_dim_pad")) {          /* for F--  */
         dump_flags.no_dimension_padding = TRUE;
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "show")) {
         dump_flags.show_cmd_line = TRUE;
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "mp")) {
         dump_flags.mp = TRUE;
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "mod_version")) {
         dump_flags.mod_version = TRUE;
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "open_mp")) {
         dump_flags.open_mp = TRUE;
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "dsm")) {
         dump_flags.dsm = TRUE;
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "cray_compatible")) {
         dump_flags.cray_compatible = TRUE;
         non_debug_ok		= TRUE;
      }

# if defined(_TARGET_PACK_HALF_WORD_TYPES)
      else if (EQUAL_STRS(cp, "pack_half_word")) {
         dump_flags.pack_half_word = TRUE;
         non_debug_ok		= TRUE;
      }
# endif
      else if (EQUAL_STRS(cp, "fe_version")) {
         printf("This is frontend %s of release %s\n",
                 frontend_version, release_level);
         non_debug_ok		= TRUE;
      }
# if defined(_F_MINUS_MINUS)
      else if (EQUAL_STRS(cp, "fmm1")) {
         dump_flags.f_minus_minus = TRUE;
         cmd_line_flags.co_array_fortran	= TRUE;
         dump_flags.fmm1 = TRUE;
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "fmm2")) {
         dump_flags.f_minus_minus = TRUE;
         cmd_line_flags.co_array_fortran	= TRUE;
         dump_flags.fmm2 = TRUE;
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "fmm")) {
         dump_flags.f_minus_minus = TRUE;
         cmd_line_flags.co_array_fortran	= TRUE;
         dump_flags.fmm1 = TRUE;
         non_debug_ok		= TRUE;
      }
# endif
      else if (EQUAL_STRS(cp, "no_mod_output")) {	/* For inlinefrom     */
         dump_flags.no_module_output = TRUE;
         non_debug_ok		= TRUE;
      }
      else if ((strncmp (cp, "preinline=", 10) == IDENTICAL)) {
         dump_flags.preinline = TRUE;
         cp = cp + 10;
         strcpy(preinline_file, cp);
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "f")) {                   /* Fortran output     */
         dump_flags.fortran_out = TRUE;
      }
      else if (EQUAL_STRS(cp, "all")) {			/* all tbls	      */
	 dump_flags.blk_stk		= TRUE;
	 dump_flags.bd_tbl		= TRUE;
	 dump_flags.cmd_line_tbls	= TRUE;
	 dump_flags.cn_tbl		= TRUE;
	 dump_flags.fort_out		= TRUE;
	 dump_flags.fp_tbl		= TRUE;
         dump_flags.ftrace_info		= FALSE;
	 dump_flags.gl_tbl		= TRUE;
         dump_flags.intrin_tbl		= FALSE;	/* TOO much output    */
	 dump_flags.ir1_tbl		= TRUE;
	 dump_flags.ir2_tbl		= TRUE;
	 dump_flags.ir3_tbl		= FALSE;
	 dump_flags.ir4_tbl		= FALSE;
	 dump_flags.mem_report		= FALSE;
         dump_flags.mtrace_info		= TRUE;
	 dump_flags.name_tbls		= TRUE;
         dump_flags.pdgcs		= TRUE;
	 dump_flags.sb_tbl		= TRUE;
	 dump_flags.scp_tbl		= TRUE;
	 dump_flags.std_err		= FALSE;
	 dump_flags.sytb		= TRUE;
	 dump_flags.typ_tbl		= TRUE;
	 dump_flags.defines		= TRUE;
         dump_flags.abort_on_ansi       = FALSE;	/* Not needed 	      */

         /* These next 2 are mutually exclusive.  stmt_dmp is the plain one   */
         /* src_dmp is the one to use for testing out src_input.c             */

	 dump_flags.src_dmp		= FALSE;
	 dump_flags.stmt_dmp		= TRUE;
#ifdef KEY /* Bug 8117 */
	 dump_flags.arg_passing		= TRUE;
#endif /* KEY Bug 8117 */
      }
#ifdef KEY /* Bug 8117 */
      else if (EQUAL_STRS(cp, "arg_passing")) {
	 dump_flags.arg_passing		= TRUE;
      }
#endif /* KEY Bug 8117 */
      else if (EQUAL_STRS(cp, "bd")) {			/* bounds table       */
	 dump_flags.bd_tbl	= TRUE;
      }
      else if (EQUAL_STRS(cp, "blk")) {			/* block stack table  */
	 dump_flags.blk_stk	= TRUE;
      }
      else if (EQUAL_STRS(cp, "cmd")) {			/* command line info  */
	 dump_flags.cmd_line_tbls = TRUE;
      }
      else if (EQUAL_STRS(cp, "cn")) {			/* constant table     */
	 dump_flags.cn_tbl	= TRUE;
      }
      else if (EQUAL_STRS(cp, "defines")) {             /* defines info       */
         dump_flags.defines = TRUE;
      }
      else if (EQUAL_STRS(cp, "cnout")) {       /* binary dump of constants */
         dump_flags.constant_bits = TRUE;
      }
      else if ((strncmp (cp, "pvp_test=", 9) == IDENTICAL)) {
         cp = cp + 9;
         dump_flags.pvp_test = atoi (cp);
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "pvp_test")) {            /* pvp_test[=nn]     */
         dump_flags.pvp_test = 1;
         non_debug_ok		= TRUE;
      }
      else if ((strncmp (cp, "file=", 5) == IDENTICAL)) {
         cp = cp + 5;
         strcpy(debug_file_name, cp);
         non_debug_ok		= TRUE;
      }
      else if (EQUAL_STRS(cp, "fortran")) {		/* module file tbl    */
         dump_flags.fort_out = TRUE;
      }
      else if (EQUAL_STRS(cp, "fp")) {			/* module file tbl    */
         dump_flags.fp_tbl = TRUE;
      }
      else if (EQUAL_STRS(cp, "ftrace")) {		/* func trace info    */
         dump_flags.ftrace_info = TRUE;
      }
      else if (EQUAL_STRS(cp, "gl")) {			/* global line tbl    */
	 dump_flags.gl_tbl = TRUE;
      }
      else if (EQUAL_STRS(cp, "intrin")) {		/* intrinsic tbl      */
	 dump_flags.intrin_tbl = TRUE;
      }
      else if (EQUAL_STRS(cp, "ir1")) {			/* intermediate rep   */
	 dump_flags.ir1_tbl = TRUE;
      }
      else if (EQUAL_STRS(cp, "ir2")) {			/* intermediate rep   */
	 dump_flags.ir2_tbl = TRUE;
      }
      else if (EQUAL_STRS(cp, "ir3")) {			/* inlining rep       */
	 dump_flags.ir3_tbl = TRUE;
      }
      else if (EQUAL_STRS(cp, "ir4")) {			/* swap dimension dmp */
	 dump_flags.ir4_tbl = TRUE;
      }
      else if (EQUAL_STRS(cp, "mem_report")) {		/* memory usage report*/
         dump_flags.mem_report = TRUE;
      }
      else if (EQUAL_STRS(cp, "msg")) {			/* memory usage report*/
         dump_flags.msg_checking = TRUE;
      }
      else if (EQUAL_STRS(cp, "mtrace")) {		/* memory trace info  */
         dump_flags.mtrace_info = TRUE;
      }
      else if (EQUAL_STRS(cp, "names")) {               /* name tables        */
         dump_flags.name_tbls = TRUE;
      }
      else if (EQUAL_STRS(cp, "pdg")) {			/* interface calls    */
	 dump_flags.pdgcs = TRUE;
      }
      else if (EQUAL_STRS(cp, "pdt")) {			/* interface calls    */
	 dump_flags.pdt_dump = TRUE;
      }
      else if (EQUAL_STRS(cp, "sb")) {			/* storage blk tbl    */
	 dump_flags.sb_tbl = TRUE;
      }
      else if (EQUAL_STRS(cp, "scp")) {			/* scope tbl	      */
	 dump_flags.scp_tbl = TRUE;
      }
      else if (EQUAL_STRS(cp, "src")) {	  	        /* print stmts and    */
	 dump_flags.src_dmp = TRUE;			/* expanded src info  */
      }
      else if (EQUAL_STRS(cp, "stderr")) {  	        /* msgs to stderr     */
	 dump_flags.std_err = TRUE;			/* rather than ordered*/
      }
      else if (EQUAL_STRS(cp, "stmt")) {		/* print stmts        */
	 dump_flags.stmt_dmp = TRUE;
      }
      else if (EQUAL_STRS(cp, "sytb")) {		/* symbol tbl	      */
	 dump_flags.sytb = TRUE;
      }
      else if (EQUAL_STRS(cp, "typ")) {			/* type tbl	      */
	 dump_flags.typ_tbl = TRUE;
      }
      else { /* option has an invalid argument */
         ntr_msg_queue(0, 78, Log_Error, 0, cp, 'u', ARG_STR_ARG);
      }
   }  /* while */

#ifndef _DEBUG

   if (!non_debug_ok) { /* -u options ignored */
      ntr_msg_queue(0, 877, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
   }
#endif

   TRACE (Func_Exit, "process_u_option", NULL);
   return;

}  /* process_u_option */

 
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process_x_option handles the command line directive suppress options  *|
|*      passed via argv (x switch).					      *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-x option arguments			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_x_option (char *optargs) 
 
{
   char			*cp;
   boolean	 	 issue_warning	= FALSE;
   token_values_type	 tok_type;


   TRACE (Func_Entry, "process_x_option", NULL);

   while (*optargs != EOS) {

      for (cp = optargs;
           *optargs != COMMA  &&  *optargs != EOS;
           ++optargs);

      if (*optargs != EOS) {
         *optargs = EOS;
         *optargs++;
      }

      tok_type	= get_dir_token_from_str(cp);

      if (tok_type == Tok_Id) {

         /* get_dir_token_from_str special cases for ALL, MIC, MPP and DIR */
         /* If it finds one of these it returns Tok_Id, otherwise its      */
         /* Tok_Unknown if it can't find a directive or cmic name.         */

         switch(*cp) {
         case 'a':

            if (EQUAL_STRS(cp, "all")) {
               cmd_line_flags.disregard_all_directives	= TRUE;
               cmd_line_flags.disregard_all_dirs	= TRUE;
               cmd_line_flags.disregard_all_mics	= TRUE;
               cmd_line_flags.disregard_all_mips	= TRUE;
               cmd_line_flags.disregard_all_omps	= TRUE;
               cmd_line_flags.disregard_conditional_omp = TRUE;
            }
            else {
               ntr_msg_queue(0, 78, Log_Error, 0, cp, 'x', ARG_STR_ARG);
            }
            break;

         case 'c':

            if (EQUAL_STRS(cp, "conditional_omp")) {
               cmd_line_flags.disregard_conditional_omp = TRUE;
            }
            else {
               PRINTMSG (0, 78, Log_Error, 0, 'x', cp);
            }
            break;


         case 'd':

            if (EQUAL_STRS(cp, "dir")) {
               cmd_line_flags.disregard_all_dirs	= TRUE;
            }
            else {
               ntr_msg_queue(0, 78, Log_Error, 0, cp, 'x', ARG_STR_ARG);
            }
            break;


         case 'm':

            if (EQUAL_STRS(cp, "mic")) {
               cmd_line_flags.disregard_all_mics	= TRUE;
            }
            else if (EQUAL_STRS(cp, "mpp")) {
               cmd_line_flags.disregard_all_mpp_cdirs	= TRUE;
            }
            else if (EQUAL_STRS(cp, "mipspro")) {
               cmd_line_flags.disregard_all_mips	= TRUE;
            }
            else {
               ntr_msg_queue(0, 78, Log_Error, 0, cp, 'x', ARG_STR_ARG);
            }
            break;


         case 'o':

            if (EQUAL_STRS(cp, "omp")) {
               cmd_line_flags.disregard_all_omps        = TRUE;
               cmd_line_flags.disregard_conditional_omp = TRUE;
            }
            else {
               PRINTMSG (0, 78, Log_Error, 0, 'x', cp);
            }
            break;


         default:
            ntr_msg_queue(0, 78, Log_Error, 0, cp, 'x', ARG_STR_ARG);
            break;
         }
      }
      else if (tok_type == Tok_Unknown) {
         ntr_msg_queue(0, 78, Log_Error, 0, cp, 'x', ARG_STR_ARG);
      }
      else {

         if (tok_type > Tok_Dir_Start && tok_type < Tok_Dir_End) {
            disregard_directive[tok_type - Tok_Dir_Start]	= TRUE;
         }
         if (tok_type > Tok_Mic_Start && tok_type < Tok_Mic_End) {
            disregard_mics[tok_type - Tok_Mic_Start]		= TRUE;
         }
         if (tok_type > Tok_SGI_Dir_Start && tok_type < Tok_SGI_Dir_End) {
            disregard_mips[tok_type - Tok_SGI_Dir_Start]	= TRUE;
         }
         if (tok_type > Tok_Open_Mp_Dir_Start && 
             tok_type < Tok_Open_Mp_Dir_End) {
            disregard_open_mp[tok_type - Tok_Open_Mp_Dir_Start]	= TRUE;
         }

         /* We need to handle the legal directives, vs the tokens that */
         /* are actually parameters to the directives.   We also need  */
         /* to handle the situation of having the same directive  with */
         /* different prefixes.  The following list is all valid dirs  */
         /* that can be specified on the -x commandline.  We will      */
         /* issue an error if the specified token is not one of the    */
         /* listed tokens.  We also look for duplicate names and set   */
         /* the token for all possible prefixes.                       */

         switch(tok_type) {

         /* Handle duplicate directive names, by setting the bit for them all */

         case Tok_Dir_Barrier :          /* CRAFT dir - obsolete */
         case Tok_SGI_Dir_Barrier :
         case Tok_Open_Mp_Dir_Barrier :
            disregard_directive[Tok_Dir_Barrier - Tok_Dir_Start]	= TRUE;
            disregard_mips[Tok_SGI_Dir_Barrier - Tok_SGI_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Barrier - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Dir_Blockable :
         case Tok_SGI_Dir_Blockable :
            disregard_directive[Tok_Dir_Blockable - Tok_Dir_Start]	= TRUE;
            disregard_mips[Tok_SGI_Dir_Blockable - Tok_SGI_Dir_Start]	= TRUE;
            break;

         case Tok_Dir_Blockingsize :
         case Tok_SGI_Dir_Blockingsize :
            disregard_directive[Tok_Dir_Blockingsize - Tok_Dir_Start]	= TRUE;
            disregard_mips[Tok_SGI_Dir_Blockingsize - Tok_SGI_Dir_Start]= TRUE;
            break;

         case Tok_Dir_Concurrent :
         case Tok_SGI_Dir_Concurrent :
            disregard_directive[Tok_Dir_Concurrent - Tok_Dir_Start]	= TRUE;
            disregard_mips[Tok_SGI_Dir_Concurrent - Tok_SGI_Dir_Start]	= TRUE;
            break;


         case Tok_Open_Mp_Dir_Copyin :   /* Keyword */
         case Tok_SGI_Dir_Copyin :
            disregard_mips[Tok_SGI_Dir_Copyin - Tok_SGI_Dir_Start]	= TRUE;
            break;

         case Tok_Dir_Critical :      /* CRAFT dir - obsolete */
         case Tok_Open_Mp_Dir_Critical :
            disregard_directive[Tok_Dir_Critical - Tok_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Critical - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Open_Mp_Dir_Distribute :
         case Tok_SGI_Dir_Distribute :
            disregard_mips[Tok_SGI_Dir_Distribute - Tok_SGI_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Distribute - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Open_Mp_Dir_Distribute_Reshape :
         case Tok_SGI_Dir_Distribute_Reshape :
            disregard_mips[Tok_SGI_Dir_Distribute_Reshape - 
                           Tok_SGI_Dir_Start]				= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Distribute_Reshape - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Open_Mp_Dir_Do :
         case Tok_SGI_Dir_Do :              /* Keyword */
            disregard_open_mp[Tok_Open_Mp_Dir_Do - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Dir_Dynamic :             /* Obsolete */
         case Tok_Open_Mp_Dir_Dynamic :
         case Tok_SGI_Dir_Dynamic :
            disregard_directive[Tok_Dir_Dynamic - Tok_Dir_Start]	= TRUE;
            disregard_mips[Tok_Open_Mp_Dir_Dynamic - Tok_SGI_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_SGI_Dir_Dynamic - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Dir_Endcritical :        /* CRAFT directive */
         case Tok_Open_Mp_Dir_Endcritical :
            disregard_directive[Tok_Dir_Endcritical - Tok_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Endcritical - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Dir_Endmaster :          /* CRAFT directive */
         case Tok_Open_Mp_Dir_Endmaster :
            disregard_directive[Tok_Dir_Endmaster - Tok_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Endmaster - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Open_Mp_Dir_Endparallel :
         case Tok_SGI_Dir_Endparallel :
         case Tok_Mic_End_Parallel :
            disregard_mics[Tok_Mic_End_Parallel - Tok_Mic_Start]	= TRUE;
            disregard_mips[Tok_SGI_Dir_Endparallel - Tok_SGI_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Endparallel - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Open_Mp_Dir_Flush :
         case Tok_SGI_Dir_Flush :
            disregard_mips[Tok_SGI_Dir_Flush - Tok_SGI_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Flush - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Dir_Inline :
         case Tok_SGI_Dir_Inline :
            disregard_directive[Tok_Dir_Inline - Tok_Dir_Start]		= TRUE;
            disregard_mips[Tok_SGI_Dir_Inline - Tok_SGI_Dir_Start]	= TRUE;
            break;

         case Tok_Dir_Interchange :
         case Tok_SGI_Dir_Interchange :
            disregard_directive[Tok_Dir_Interchange - Tok_Dir_Start]	= TRUE;
            disregard_mips[Tok_SGI_Dir_Interchange - Tok_SGI_Dir_Start]	= TRUE;
            break;

         case Tok_Dir_Master :                 /* CRAFT directive */
         case Tok_Open_Mp_Dir_Master :
            disregard_directive[tok_type - Tok_Dir_Start]		= TRUE;
            disregard_open_mp[tok_type - Tok_Open_Mp_Dir_Start]		= TRUE;
            break;

         case Tok_Dir_Maxcpus :
         case Tok_Mic_Maxcpus :        /* Keyword */
            disregard_directive[Tok_Dir_Maxcpus - Tok_Dir_Start]	= TRUE;
            break;

         case Tok_Dir_Noblocking :
         case Tok_SGI_Dir_Noblocking :
            disregard_directive[Tok_Dir_Noblocking - Tok_Dir_Start]	= TRUE;
            disregard_mips[Tok_SGI_Dir_Noblocking - Tok_SGI_Dir_Start]	= TRUE;
            break;

         case Tok_Dir_Noinline :
         case Tok_SGI_Dir_Noinline :
            disregard_directive[Tok_Dir_Noinline - Tok_Dir_Start]	= TRUE;
            disregard_mips[Tok_SGI_Dir_Noinline - Tok_SGI_Dir_Start]	= TRUE;
            break;

         case Tok_Dir_Nointerchange :
         case Tok_SGI_Dir_Nointerchange :
            disregard_directive[Tok_Dir_Nointerchange - Tok_Dir_Start]	= TRUE;
            disregard_mips[Tok_SGI_Dir_Nointerchange - 
                           Tok_SGI_Dir_Start]				= TRUE;
            break;

         case Tok_Dir_Norecurrence :
         case Tok_SGI_Dir_Norecurrence :  /* Keyword */
            disregard_directive[Tok_SGI_Dir_Norecurrence - Tok_Dir_Start]= TRUE;
            break;


         case Tok_SGI_Dir_Ordered :   /* Keyword */
         case Tok_Open_Mp_Dir_Ordered :
            disregard_mips[Tok_SGI_Dir_Ordered - Tok_SGI_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Ordered - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_SGI_Dir_Page_Place :
         case Tok_Open_Mp_Dir_Page_Place :
            disregard_mips[Tok_SGI_Dir_Page_Place - Tok_SGI_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Page_Place - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Mic_Parallel :
         case Tok_SGI_Dir_Parallel :
         case Tok_Open_Mp_Dir_Parallel :
            disregard_mics[Tok_Mic_Parallel - Tok_Mic_Start]		= TRUE;
            disregard_mips[Tok_SGI_Dir_Parallel - Tok_SGI_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Parallel - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_SGI_Dir_Paralleldo :
         case Tok_Open_Mp_Dir_Paralleldo :
            disregard_mips[Tok_SGI_Dir_Paralleldo - Tok_SGI_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Paralleldo - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Open_Mp_Dir_Parallelworkshare :
            disregard_open_mp[tok_type - Tok_Open_Mp_Dir_Start]		= TRUE;
            break;

         case Tok_Mic_Permutation :
         case Tok_SGI_Dir_Permutation :   /* Keyword */
            disregard_mics[Tok_Mic_Permutation - Tok_Mic_Start]	= TRUE;
            break;

         case Tok_SGI_Dir_Redistribute :
         case Tok_Open_Mp_Dir_Redistribute :
            disregard_mips[Tok_SGI_Dir_Redistribute - Tok_SGI_Dir_Start]= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Redistribute - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_SGI_Dir_Section :
         case Tok_Open_Mp_Dir_Section :
            disregard_mips[Tok_SGI_Dir_Section - Tok_SGI_Dir_Start]	= TRUE;
            disregard_open_mp[Tok_Open_Mp_Dir_Section - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Dir_Shared :          /* CRAFT directive */
         case Tok_Open_Mp_Dir_Shared :  /* Keyword */
         case Tok_SGI_Dir_Shared :      /* Keyword */
            disregard_directive[Tok_Dir_Shared - Tok_Dir_Start]		= TRUE;
            break;

         case Tok_Dir_Single :           /* Keyword */
         case Tok_Open_Mp_Dir_Single :
            disregard_open_mp[Tok_Open_Mp_Dir_Single - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Open_Mp_Dir_Workshare :
            disregard_open_mp[Tok_Open_Mp_Dir_Workshare - 
                              Tok_Open_Mp_Dir_Start]			= TRUE;
            break;

         case Tok_Dir_Taskcommon :
         case Tok_Mic_Taskcommon :  /* Unsupported */
            disregard_directive[Tok_Dir_Taskcommon - Tok_Dir_Start]	= TRUE;
            disregard_mics[Tok_Mic_Taskcommon - Tok_Mic_Start]		= TRUE;
            break;

         case Tok_Dir_Unroll :
         case Tok_SGI_Dir_Unroll :
            disregard_directive[Tok_Dir_Unroll - Tok_Dir_Start]		= TRUE;
            disregard_mips[Tok_SGI_Dir_Unroll - Tok_SGI_Dir_Start]	= TRUE;
            break;

         case Tok_Dir_Vector :
         case Tok_SGI_Dir_Vector :  /* Keyword */
            disregard_directive[Tok_Dir_Vector - Tok_Dir_Start]		= TRUE;
            break;



         case Tok_Dir_Align :
         case Tok_Dir_Auxiliary :
         case Tok_Dir_Bl :
         case Tok_Dir_Bounds :
         case Tok_Dir_Cache_Align :
         case Tok_Dir_Cache_Bypass :
         case Tok_Dir_Cache_Noalloc :
         case Tok_Dir_Common :
         case Tok_Dir_Copy_Assumed_Shape :
         case Tok_Dir_Eject :
         case Tok_Dir_Flow :
         case Tok_Dir_Fixed :
         case Tok_Dir_Free :
         case Tok_Dir_Id :
         case Tok_Dir_Ignore_TKR :
         case Tok_Dir_Inline_Always :
         case Tok_Dir_Inline_Never :
         case Tok_Dir_Ivdep :
         case Tok_Dir_List :
         case Tok_Dir_Mark :
         case Tok_Dir_Modinline :
         case Tok_Dir_Name :
         case Tok_Dir_Nextscalar :
         case Tok_Dir_Nobl :
         case Tok_Dir_Nobounds :
         case Tok_Dir_Noflow :
         case Tok_Dir_Nolist :
         case Tok_Dir_Nomark :
         case Tok_Dir_Nomodinline :
         case Tok_Dir_Nopattern :
         case Tok_Dir_Nosideeffects :
         case Tok_Dir_Nosplit :
         case Tok_Dir_Nostream :
         case Tok_Dir_Notask :
         case Tok_Dir_Nounroll :
         case Tok_Dir_Novector :
         case Tok_Dir_Novsearch :
         case Tok_Dir_Pattern :
         case Tok_Dir_Preferstream :
         case Tok_Dir_Prefertask :
         case Tok_Dir_Prefervector :
         case Tok_Dir_Recurrence :
         case Tok_Dir_Shortloop :
         case Tok_Dir_Split :
         case Tok_Dir_Stack :
         case Tok_Dir_Stream :
         case Tok_Dir_Suppress :
         case Tok_Dir_Symmetric :
         case Tok_Dir_System_Module :
         case Tok_Dir_Task :
         case Tok_Dir_Uses_Eregs :
         case Tok_Dir_Vfunction :
         case Tok_Dir_Vsearch :
            break;


         case Tok_Mic_Case :
         case Tok_Mic_End_Case :
         case Tok_Mic_Cncall :
         case Tok_Mic_Do_All :
         case Tok_Mic_Do_Parallel :
         case Tok_Mic_End_Do :
         case Tok_Mic_Guard :
         case Tok_Mic_End_Guard :
         case Tok_Mic_Numcpus :
         case Tok_Mic_Wait :
         case Tok_Mic_Send :
            break;


         /* OpenMp directives. */

         case Tok_Open_Mp_Dir_Atomic :
         case Tok_Open_Mp_Dir_Enddo :
         case Tok_Open_Mp_Dir_Endordered :
         case Tok_Open_Mp_Dir_Endparalleldo :
         case Tok_Open_Mp_Dir_Endparallelsections :
         case Tok_Open_Mp_Dir_Endparallelworkshare :
         case Tok_Open_Mp_Dir_Endsections :
         case Tok_Open_Mp_Dir_Endsingle :
         case Tok_Open_Mp_Dir_Endworkshare :
         case Tok_Open_Mp_Dir_Parallelsections :
         case Tok_Open_Mp_Dir_Sections :
         case Tok_Open_Mp_Dir_Threadprivate :
            break;

         /* Mips directives */

         case Tok_SGI_Dir_Aggressiveinner :
         case Tok_SGI_Dir_Align_Symbol :
         case Tok_SGI_Dir_Assert :
         case Tok_SGI_Dir_Chunk :
         case Tok_SGI_Dir_Criticalsection :
         case Tok_SGI_Dir_Doacross :
         case Tok_SGI_Dir_Endcriticalsection :
         case Tok_SGI_Dir_Endpsections :
         case Tok_SGI_Dir_Endpsection :
         case Tok_SGI_Dir_Endpdo :
         case Tok_SGI_Dir_Endsingleprocess :
         case Tok_SGI_Dir_Fill_Symbol :
         case Tok_SGI_Dir_Fission :
         case Tok_SGI_Dir_Fissionable :
         case Tok_SGI_Dir_Fusable :
         case Tok_SGI_Dir_Fuse :
         case Tok_SGI_Dir_Ipa :
         case Tok_SGI_Dir_Limit :
         case Tok_SGI_Dir_Minconcurrent :
         case Tok_SGI_Dir_Mp_Schedtype :
         case Tok_SGI_Dir_Noconcurrentize :
         case Tok_SGI_Dir_Nofission :
         case Tok_SGI_Dir_Nofusion :
         case Tok_SGI_Dir_Noipa :
         case Tok_SGI_Dir_Opaque :
         case Tok_SGI_Dir_Optional :
#ifdef KEY /* Bug 2660 */
         case Tok_SGI_Dir_Options :
#endif /* KEY Bug 2660 */
         case Tok_SGI_Dir_Pdo :
         case Tok_SGI_Dir_Prefetch_Manual :
         case Tok_SGI_Dir_Prefetch_Ref :
         case Tok_SGI_Dir_Prefetch :
         case Tok_SGI_Dir_Prefetch_Ref_Disable :
         case Tok_SGI_Dir_Psection :
         case Tok_SGI_Dir_Psections :
         case Tok_SGI_Dir_Regionbegin :
         case Tok_SGI_Dir_Regionend :
         case Tok_SGI_Dir_Section_Gp :
         case Tok_SGI_Dir_Section_Non_Gp :
         case Tok_SGI_Dir_Singleprocess :
            break;

         /* These are obsolete directives. */

         case Tok_Dir_Block :
         case Tok_Dir_Regfile :
         case Tok_Dir_Semextern :
         case Tok_Dir_Shortsequence :
         case Tok_Dir_Static :
         case Tok_Dir_Taskhead :
            issue_warning	= TRUE;
            break;

         /* Unsupported cmics */

         case Tok_Mic_Continue :
            issue_warning	= TRUE;

         /* CRAFT directives */

         case Tok_Dir_Atomicupdate :
         case Tok_Dir_Nobarrier :
            issue_warning	= TRUE;
            break;

         }  /* end switch */

         if (issue_warning) {
            PRINTMSG (0, 604, Log_Warning, 0, 'x', cp);
            issue_warning	= FALSE;
         }
      }
   }

   TRACE (Func_Exit, "process_x_option", NULL);

   return; 
 
}  /* process_x_option */
 
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process_M_option handles the command line message suppress option     *|
|*	passed via argv (M switch).					      *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-M option arguments			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_M_option (char *optargs) 
 
{ 
   char		*cp;
   long		 msg_num;
   boolean	 suppress;
   long		*table_ptr;


   TRACE (Func_Entry, "process_M_option", NULL);

   while (*optargs != EOS) {

      for (cp = optargs;
           *optargs != BLANK  &&  *optargs != COMMA  &&  *optargs != EOS;
           ++optargs);

      if (*optargs != EOS) {
         *optargs = EOS;
         *optargs++;
      }

      table_ptr	= (long *) message_suppress_tbl;
      suppress	= TRUE;

      if (*cp == 'E') {
         *cp++;
         suppress	= FALSE;
         table_ptr	= (long *) message_error_tbl;
      }
      else if (*cp == 'W') {
         *cp++;
         suppress	= FALSE;
         table_ptr	= (long *) message_warning_tbl;
      }
      else if (*cp < '0' || *cp > '9') {    /* Not alphabetic */
         ntr_msg_queue(0, 78, Log_Error, 0, cp, 'M', ARG_STR_ARG);
      }

      msg_num = atoi(cp);

      if (msg_num < 0  || msg_num > (MAX_MSG_SIZE*HOST_BITS_PER_WORD) - 1) {
         ntr_msg_queue(0, 78, Log_Error, 0, cp, 'M', ARG_STR_ARG);
      }
      else {

         if (suppress && !GET_MESSAGE_TBL(message_suppress_tbl, msg_num)) {
            cmd_line_flags.num_msgs_suppressed++;
         }

         SET_MESSAGE_TBL(table_ptr, msg_num);
      }
   }

   TRACE (Func_Exit, "process_M_option", NULL);

   return; 
 
}  /* process_M_option */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process_Y_option handles ccg dump options.                            *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-Y option arguments			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
 
static void process_Y_option (char *optargs)

{
   int	ch;


   TRACE (Func_Entry, "process_Y_option", NULL);


   while (ch = *optargs++) {
#ifdef _TARGET32
      ccg_dump_flags = ccg_dump_flags | (1 << ((ch - 'A') & 0x1F));
#else
      ccg_dump_flags = ccg_dump_flags | (1 << (ch - 'A'));
#endif
   }  /* while */


   TRACE (Func_Exit, "process_Y_option", NULL);

   return;
 
}  /* process_Y_option */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Set_prog_file_names processes the source file name on command line,   *|
|*	if it exists, otherwise the default source file is 'stdin'.	      *|
|*									      *|
|* Input parameters:							      *|
|*	argv			last argument on cmd line is input file	      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void set_prog_file_names (char *argv[])

{
   extern	char	*optarg;		/* defined in getopt */
   extern	int	optind;			/* defined in getopt */

   		int	fp_idx;
   		int	idx;
   		int	length;
		char	*stp;


   TRACE (Func_Entry, "set_prog_file_names", NULL);

   /* optind points to the last argument;  if it exists, it is the file	 */
   /* to be compiled;  if nothing is there, input is obtained from stdin */

   optarg = argv[optind];

   if (optarg == NULL) {			/* 'stdin' is default input   */
      src_file[0] = EOS;

      /* make sure -b specified when binary file requested and input is stdin */

      if (binary_output  &&  bin_file[0] == EOS) {

	 /* Name must be specified for file when input is from stdin. */

         ntr_msg_queue(0, 80, Log_Error, 0, "binary", 0, STR_ARG);
	 binary_output = FALSE;
      }

      /* make sure -S specified when cal file requested and input is stdin */

      if (assembly_output  &&  assembly_file[0] == EOS) {

	 /* Name must be specified for file when input is from stdin. */

         ntr_msg_queue(0, 80, Log_Error, 0, "assembly", 0, STR_ARG);
	 assembly_output = FALSE;
      }

      if (on_off_flags.assembly_listing_file) {
         strcpy(assembly_listing_file, "stdout");
      }

      strcpy(debug_file_name, "stderr");
   }
   else {					/* named file[.f] is input    */
#ifdef KEY /* Bug 4469 */
      if (strlen(optarg) >= MAX_FILE_NAME_SIZE) {
	PRINTMSG(0, 57, Limit, 0, (MAX_FILE_NAME_SIZE - 1));
        }
#endif /* KEY bug 4469 */
      strncpy (src_file, optarg, MAX_FILE_NAME_SIZE);
      src_file[MAX_FILE_NAME_SIZE-1] = EOS;

      /* generate default file name for binary file from input file[.f]	 */

      if (binary_output && bin_file[0] == EOS) {
         MAKE_DEFAULT_NAME (bin_file, src_file, "o");
      }

      /* generate default file name for cal file from input file[.f]  */

      if (assembly_output  &&  assembly_file[0] == EOS) {
	 MAKE_DEFAULT_NAME (assembly_file, src_file, "s");
      }

      if (on_off_flags.assembly_listing_file) {
	 MAKE_DEFAULT_NAME (assembly_listing_file, src_file, "L");
      }

      /* Generate default file name for debug file from input file[.f]     */
      /* In some obscure cases, the debug file will have been written to   */
      /* before the commandline is processed or it may have been specified */
      /* on the command line via -ufile=<name>.  DO NOT change the name.   */
      /* Leave it as cft90_dump or as specified.  Otherwise name it	   */
      /* src_file.l						           */
      if (debug_file_name[0] == NULL_CHAR) {
         MAKE_DEFAULT_NAME (debug_file_name, src_file, "l");
      }

      if (! set_source_form_option) {   /* Set fixed/free from src file name  */
         stp = strrchr (src_file, DOT);

         if (stp != NULL) {

            if (EQUAL_STRS(stp, ".f") ||
                EQUAL_STRS(stp, ".F")) {
               cmd_line_flags.src_form	= Fixed_Form;
               source_form		= Fixed_Form;
            }
            else if (EQUAL_STRS(stp, ".f90") ||
#ifdef KEY /* Bug 5064 */
                     EQUAL_STRS(stp, ".f95") ||
                     EQUAL_STRS(stp, ".F95") ||
#endif /* KEY Bug 5064 */
                     EQUAL_STRS(stp, ".F90")) {
               cmd_line_flags.src_form	= Free_Form;
               source_form		= Free_Form;
            }
         }
      }

# if defined(_FRONTEND_CONDITIONAL_COMP)
      if (! no_preprocessing) {
         /* see if this is a .F or .F90 file */
         stp = strrchr (src_file, DOT);

         if (stp != NULL) {

            if (EQUAL_STRS(stp, ".F") ||
#ifdef KEY /* Bug 5064 */
		EQUAL_STRS(stp, ".F95") ||
#endif /* KEY Bug 5064 */
                EQUAL_STRS(stp, ".F90")) {

               on_off_flags.preprocess = TRUE;
            }
         }
      }
# endif
   }

   /* Check that any inlinefrom files will not be */ 
   /* overwritten by the main file compilation.   */

   fp_idx = inline_path_idx;

   while (fp_idx != NULL_IDX) {

      if (strcmp(bin_file, FP_NAME_PTR(fp_idx)) == 0) {

         /* This should use ntr_msg_queue but to do it would be messy. */
         /* So technically we cannot stop 1252 from issuing if it is   */
         /* used with the -M option.   kay                             */

         PRINTMSG(0, 1252, Log_Error, 0, bin_file, FP_NAME_PTR(fp_idx));
      }
      fp_idx = FP_NEXT_FILE_IDX(fp_idx);
   }

   if (cif_flags != 0  &&  cif_name[0] == '\0') {
      MAKE_DEFAULT_NAME (cif_name, src_file, "T");
   }

   if (on_off_flags.preprocess_only || on_off_flags.save_dot_i) {
      MAKE_DEFAULT_NAME (dot_i_file, src_file, "i");
   }

   /* Put the current directory into the module search path.  By putting */
   /* it here, it will be searched after all user specified files/dirs.  */

   TBL_REALLOC_CK(file_path_tbl, 1);
   CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);
   FP_NAME_LEN(file_path_tbl_idx)	= 2;
   FP_NAME_IDX(file_path_tbl_idx)	= str_pool_idx + 1;
   FP_SRCH_THE_FILE(file_path_tbl_idx)	= TRUE;
   length				= WORD_LEN(2);

   TBL_REALLOC_CK(str_pool, length);

   for (idx = FP_NAME_IDX(file_path_tbl_idx); idx <= str_pool_idx; idx++) {
      str_pool[idx].name_long		= 0;
   }

   strcpy(FP_NAME_PTR(file_path_tbl_idx), "./");

   if (module_path_idx != NULL_IDX) {
      idx = module_path_idx;

      while (FP_NEXT_FILE_IDX(idx) != NULL_IDX) {
         idx = FP_NEXT_FILE_IDX(idx);
      }
      FP_NEXT_FILE_IDX(idx)		= file_path_tbl_idx;
   }
   else {
      module_path_idx = file_path_tbl_idx;
   }

   set_system_module_path();

   TRACE (Func_Exit, "set_prog_file_names", NULL);

   return;

}  /* set_prog_file_names */


# ifdef _DEBUG
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Prints a description of the debug items available to stderr.          *|
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

static	void	dump_help_screen(void)
{
   int   i;
   char	*info_str[] = {
             "The following options are available in a compiler with -u: ",
             " ",
             "abort_ansi       Abort compilation after first ANSI msg.",
             "cray_compatible  Sgi  turns s_default64 on, int1,2 off.",
             "f                Unimplemented - controls fortran output.",
             "dsm              Set by mongoose driver - not a user option.",
             "fmm              Controls f-- prototype.",
             "fmm1             Controls f-- prototype.",
             "fmm2             Controls f-- prototype.",
             "mod_version      Prints each module version number being read in",
             "mp               Allow recognition of the SGI directives.",
             "no_dim_pad       Do not pad out missing dimensions.",
             "no_mod_output    Do not do module output.",
             "open_mp          Allow recognition of the open mp directives.",
             "pack_half_word   Turn on component packing for sdefault32",
             "preinline=       Create an inline template for this file.",
             "pvp_test         Used for meta development.",
             "show             Show input command line to frontend.",
             " ",
             "The following options are available in a debug compiler with -u:",
             " ",
             "all              Dump all tables.  Do not do any tracing.",
             "bd               Dump the bounds table.",
             "blk              Dump the Block Stack.",
             "cmd              Dump the commandline table.",
             "cn               Dump the Constant table.",
             "defines          Dump the build definitions.",
             "file=            Specify the name of the debug output file.",
             "fortran          Dump the output IR in a Fortran format.",
             "fp               Dump the File Path table.",
             "ftrace           Activate the function trace.",
             "gl               Dump the Global Line table.",
             "intrin           Dump the Intrinsic table.",
             "ir1              Dump the IR after Pass 1.",
             "ir2              Dump the IR after Pass 2.",
             "ir3              Dump the IR after inlining.",
             "ir4              Dump the IR after swapping dimensions.",
             "mem_report       Provide a report of front-end memory usage.",
             "msg              Issue an abort for all zero line numbers.",
             "mtrace           Trace front-end memory usage.",
             "names            Dump the local, global and hidden name tables.",
             "pdg              Dump all calls to the PGDCS interface.",
             "pdt              Dump PDT info while searching for modules.",
             "sb               Dump the Storage Block table.",
             "scp              Dump the scope table.",
             "src              Dump whole source program.",
             "stderr           Output msgs to stderr rather than ordering.",
             "stmt             Dump each statement.",
             "sytb             Dump the symbol table.",
             "typ              Dump the Type table.",
             "cnout            Dump constants as binary to stdout.",
             "DONE" };

   TRACE (Func_Entry, "dump_help_screen", NULL);

   i = 0;

   while (strcmp(info_str[i],"DONE")) fprintf(stderr, "%s\n", info_str[i++]);

   TRACE (Func_Exit, "dump_help_screen", NULL);

   return;

}  /* dump_help_screen */

# endif


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add path names to the file path table.  Used by both -p and -I.       *|
|*ifdef KEY Bug 4210
|*       Also used by -J to prepend the module-creation directory to the      *|
|*	list                                                                  *|
|*endif  KEY Bug 4210
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

static	void	add_to_fp_table(char	*optargs,
				int	*start_idx,
				int	 option)

{
   int		 fp_idx;
   int		 length;
   char		 path_name[MAX_FILE_NAME_SIZE];


   TRACE (Func_Entry, "add_to_fp_table", NULL);

   TBL_REALLOC_CK(file_path_tbl, 1);
   CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);

   if (*start_idx == NULL_IDX) {
      *start_idx	= file_path_tbl_idx;
   }
#ifdef KEY /* Bug 4210 */
   /* Prepend the -J directory to the module-search list */
   else if ('J' == option) {
      FP_NEXT_FILE_IDX(file_path_tbl_idx) = *start_idx;
      *start_idx = file_path_tbl_idx;
      /* We want 'J' to work like 'p' (the module-search-path option)
         henceforth */
      option = 'p';
   }
#endif /* KEY Bug 4210 */
   else {
      fp_idx = *start_idx;

      /* Find the end of the list of files/paths.  These must be ordered. */

      while (FP_NEXT_FILE_IDX(fp_idx) != NULL_IDX) {
         fp_idx = FP_NEXT_FILE_IDX(fp_idx);
      }
      FP_NEXT_FILE_IDX(fp_idx)		= file_path_tbl_idx;
   }
      
   FP_NAME_IDX(file_path_tbl_idx)	= str_pool_idx + 1;
   length				= strlen(optargs);

   if (option == 'p' || option == 'O') {   /* module path option */
      FP_SRCH_THE_FILE(file_path_tbl_idx)	= TRUE;
   }
   else {

      FP_CLASS(file_path_tbl_idx)	= Include_Fp;

      if (optargs[0] != SLASH)

      {
         /* make relative path prefix absolute */

         getcwd (path_name, MAX_FILE_NAME_SIZE);
         length += strlen(path_name) + 1;
      }
   }

   FP_NAME_LEN(file_path_tbl_idx) = length;

   TBL_REALLOC_CK(str_pool, WORD_LEN(length));

   str_pool[str_pool_idx].name_long = 0; /* Zero out last word */

   if (option == 'p' || option == 'O' || optargs[0] == SLASH)
     {
      strcpy(FP_NAME_PTR(file_path_tbl_idx), optargs);
   }
   else {  /* Hardcoding a path name here for inlining only */
      strcpy(FP_NAME_PTR(file_path_tbl_idx), path_name);
      strcat(FP_NAME_PTR(file_path_tbl_idx), "/");
      strcat(FP_NAME_PTR(file_path_tbl_idx), optargs);
   }

   TRACE (Func_Exit, "add_to_fp_table", NULL);

   return;

}  /* add_to_fp_table */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	process_P_option handles position independent code model options      *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-P option arguments			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_P_option (char *optargs)

{
#  if defined(_ACCEPT_CMD_P)
   int		 ch;
   char		 err_str[2];
#  endif


   TRACE (Func_Entry, "process_P_option", NULL);

#  if defined(_ACCEPT_CMD_P)
      while (ch = *optargs++) {

         switch (ch) {
         case 'l':
            cmd_line_flags.large_pic_model	= TRUE;
            break;

         case 's':
            cmd_line_flags.small_pic_model	= TRUE;
            break;

         default:
            err_str[0] = ch;
            err_str[1] = EOS;

            /* option has an invalid argument */

            ntr_msg_queue(0, 78, Log_Error, 0, err_str, 'P', ARG_STR_ARG);
            break;
         }	 /* switch */
      }  /* while */
#  else  /* Unsupported */
      ntr_msg_queue(0, 797, Log_Warning, 0, (char *) NULL, 'P' ,ARG_ARG);
#  endif
 
   TRACE (Func_Exit, "process_P_option", NULL);

   return;

}  /* process_P_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process memory option.                                                *|
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

static	void	process_a_option(char	*optargs)

{
   char		*cp;

#  if defined(_ACCEPT_CMD_a_pad)
 	  int		 pad_amount;
#  endif


   TRACE (Func_Entry, "process_a_option", NULL);

   while (*optargs != EOS) {

      for (cp = optargs;
	      *optargs != BLANK  &&  *optargs != COMMA  &&  *optargs != EOS;
              ++optargs) {
      }

      if (*optargs != EOS) {
          *optargs = EOS;
          *optargs++;
      }

      if (EQUAL_STRS(cp, "lign32")) {

#        if defined(_ACCEPT_CMD_align)
            cmd_line_flags.align32	= TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "align32", 'a', ARG_STR_ARG);
#        endif
      }
      else if (EQUAL_STRS(cp, "lign64")) {

#        if defined(_ACCEPT_CMD_align)
            cmd_line_flags.align64	= TRUE;
            cmd_line_flags.dalign	= TRUE;  /* Means the same thing. */
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "align64", 'a', ARG_STR_ARG);
#        endif
      }
      else if (EQUAL_STRS(cp, "dalign")) {

#        if defined(_ACCEPT_CMD_a_dalign)
            cmd_line_flags.dalign	= TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "dalign", 'a', ARG_STR_ARG);
#        endif
      }
      else if (EQUAL_STRS(cp, "static_threadprivate")) {

#        if defined(_ACCEPT_CMD_a_static_threadprivate)
            cmd_line_flags.static_threadprivate	= TRUE;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "static_threadprivate",
                                                  'a', ARG_STR_ARG);
#        endif
      }
      else if (EQUAL_STRS(cp, "taskcommon")) {

#        if !defined(_TASK_COMMON_EXTENSION)
            ntr_msg_queue(0, 744, Log_Warning, 0,
                          "taskcommon", 'a', ARG_STR_ARG);
#        else
            cmd_line_flags.taskcommon	= TRUE;
#        endif
      }
      else if (strncmp(cp, "pad", 3) == 0) {

#        if defined(_ACCEPT_CMD_a_pad)
            cp			       +=3 ;

            if (*cp != EOS) {
               pad_amount = atoi(cp);

               if (pad_amount <= 0 || pad_amount >= 4096) {
                  ntr_msg_queue(0, 1251, Log_Error, 0,
                                (char *) NULL, pad_amount, ARG_ARG);
               }
            }
            else {
               pad_amount = 0;
            }
 
            cmd_line_flags.pad		= TRUE;
            cmd_line_flags.pad_amount	= pad_amount;
#        else
            ntr_msg_queue(0, 744, Log_Warning, 0, "pad", 'a', ARG_STR_ARG);
#        endif
      }
      else { /* option has an invalid argument */
         ntr_msg_queue(0, 78, Log_Error, 0, cp, 'a', ARG_STR_ARG);
      }

   }  /* while */

            
   TRACE (Func_Exit, "process_a_option", NULL);

   return;

}  /* process_a_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add use names to the file path table for implicit use.                *|
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

static	void	process_A_option(char	*optargs)

{
   boolean	 found_one;
   int		 fp_idx;
   int		 idx;
   int		 length;
   char		*module_name;
   int		 start_idx;
   char		*str_ptr;


   TRACE (Func_Entry, "process_A_option", NULL);

   start_idx	= file_path_tbl_idx + 1;
   found_one	= FALSE;
      
   while (*optargs != EOS) {

      for (module_name = optargs;
          *optargs != BLANK  &&  *optargs != COMMA  &&  *optargs != EOS;
          ++optargs);

      if (*optargs != EOS) {
	 *optargs = EOS;
	 *optargs++;
      }

      if (found_one) {  /* Second or subsequent time through */
         FP_NEXT_FILE_IDX(file_path_tbl_idx) = file_path_tbl_idx + 1;
      }

      TBL_REALLOC_CK(file_path_tbl, 1);
      CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);

      found_one			= TRUE;
      FP_NAME_IDX(file_path_tbl_idx)	= str_pool_idx + 1;
      length				= strlen(module_name);
      FP_CLASS(file_path_tbl_idx)	= Unknown_Fp;
      FP_NAME_LEN(file_path_tbl_idx)	= length;

      TBL_REALLOC_CK(str_pool, WORD_LEN(length));

      str_pool[str_pool_idx].name_long = 0; /* Zero out last word */

      str_ptr	= FP_NAME_PTR(file_path_tbl_idx);

      for (idx = 0;  idx < length;  idx++) {
         str_ptr[idx] = toupper(module_name[idx]);
      }
   }

   if (found_one) {

      if (cmd_line_flags.implicit_use_idx == NULL_IDX) {
         cmd_line_flags.implicit_use_idx	= start_idx;
      }
      else {
         fp_idx = cmd_line_flags.implicit_use_idx;

         /* Find the end of the list of files/paths.  These must be ordered. */

         while (FP_NEXT_FILE_IDX(fp_idx) != NULL_IDX) {
            fp_idx = FP_NEXT_FILE_IDX(fp_idx);
         }
         FP_NEXT_FILE_IDX(fp_idx)		= start_idx;
      }
   }

   TRACE (Func_Exit, "process_A_option", NULL);

   return;

}  /* process_A_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process binary option.                                                *|
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

static	void	process_b_option(char	*optargs)

{

   TRACE (Func_Entry, "process_b_option", NULL);

   /* issue warning if cal output is being overridden */

   if (cmd_line_flags.assembly_output) {
      ntr_msg_queue(0, 715, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
      cmd_line_flags.assembly_output = FALSE;
    }
 
    cmd_line_flags.binary_output = TRUE;
    strncpy (bin_file, optargs, MAX_FILE_NAME_SIZE);
    bin_file[MAX_FILE_NAME_SIZE-1] = EOS;

   TRACE (Func_Exit, "process_b_option", NULL);

   return;

}  /* process_b_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process source form option.                                           *|
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

static	void	process_f_option(char	*optargs)

{

   TRACE (Func_Entry, "process_f_option", NULL);

   if (EQUAL_STRS(optargs, "fixed")) {
      cmd_line_flags.src_form	= Fixed_Form;
      source_form		= Fixed_Form;
      set_source_form_option	= TRUE;
   }
   else if (EQUAL_STRS(optargs, "free")) {
      cmd_line_flags.src_form	= Free_Form;
      source_form		= Free_Form;
      set_source_form_option	= TRUE;
   }
#ifdef KEY /* Bug 12482 */
   else if (EQUAL_STRS(optargs, "fortran2003")) {
      on_off_flags.fortran2003 = TRUE;
   }
#endif /* KEY Bug 12482 */
   else {
      ntr_msg_queue(0, 78, Log_Error, 0, optargs, 'f', ARG_STR_ARG);
   }

   TRACE (Func_Exit, "process_f_option", NULL);

   return;

}  /* process_f_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process integer option.                                               *|
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

static	void	process_i_option(char	*optargs)
{

   TRACE (Func_Entry, "process_i_option", NULL);



   set_i_option = TRUE;

#ifdef KEY /* Bug 5089 */
   /* Search-path for .mod files for F2003 intrinsic modules */
#define IMPATH "ntrinsic_module_path="
   if (EQUAL_STRS(optargs, "ntrinsic_module_gen")) {
      on_off_flags.intrinsic_module_gen = TRUE;
   }
   else if (optargs == strstr(optargs, IMPATH)) {
      /* We want the same behavior inside add_to_fp_table() which it exhibits
       * for the 'p' option for nonintrinsic module paths */
      add_to_fp_table (optargs + (sizeof IMPATH) - 1,
        &intrinsic_module_path_idx, 'p');
   }
   else
#endif /* KEY Bug 5089 */
   if (!EQUAL_STRS(optargs, "32")) {
      ntr_msg_queue(0, 78, Log_Error, 0, optargs, 'i', ARG_STR_ARG);
   }
   else if (opt_flags.set_fastint_option) {
      ntr_msg_queue(0, 1192, Log_Warning, 0,
                    "-i\n-O fastint\n-O fastint",
                    0, MULT_STR_ARG);
   }
   else if (opt_flags.set_allfastint_option) {
      ntr_msg_queue(0, 1192, Log_Warning, 0,
                    "-i\n-O allfastint\n-O allfastint",
                    0, MULT_STR_ARG);
   }
   else if (opt_flags.set_nofastint_option) {
      ntr_msg_queue(0, 1192, Log_Warning, 0,
                    "-i\n-O nofastint\n-O nofastint",
                    0, MULT_STR_ARG);
   }

   cmd_line_flags.integer_32 = TRUE;

   TRACE (Func_Exit, "process_i_option", NULL);

   return;

}  /* process_i_option */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process solaris profile option.                                       *|
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

static	void	process_k_option(char	*optargs)

{

   TRACE (Func_Entry, "process_k_option", NULL);

#  if defined(_ACCEPT_CMD_k)

      if (EQUAL_STRS(optargs, "g") || EQUAL_STRS(optargs, "s")) {
         cmd_line_flags.solaris_profile	= TRUE;
      }
      else {
         ntr_msg_queue(0, 78, Log_Error, 0, optargs, 'k', ARG_STR_ARG);
      }
#  else  /* Unsupported */
      ntr_msg_queue(0, 797, Log_Warning, 0, (char *) NULL, 'k' ,ARG_ARG);
#  endif

   TRACE (Func_Exit, "process_k_option", NULL);

   return;

}  /* process_k_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process msg level option.                                             *|
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

static	void	process_m_option(char	*optargs)

{

   TRACE (Func_Entry, "process_m_option", NULL);

   if (EQUAL_STRS(optargs, "0")) {
      cmd_line_flags.msg_lvl_suppressed = Comment_Lvl;
   }
   else if (EQUAL_STRS(optargs, "1")) {
      cmd_line_flags.msg_lvl_suppressed = Note_Lvl;
   }
   else if (EQUAL_STRS(optargs, "2")) { 
      cmd_line_flags.msg_lvl_suppressed = Caution_Lvl;
   }
   else if (EQUAL_STRS(optargs, "3")) { 
      cmd_line_flags.msg_lvl_suppressed = Warning_Lvl;
   }
   else if (EQUAL_STRS(optargs, "4")) { 
      cmd_line_flags.msg_lvl_suppressed = Error_Lvl;
   }
   else {
      ntr_msg_queue(0, 78, Log_Error, 0, optargs, 'm', ARG_STR_ARG);
   }

   TRACE (Func_Exit, "process_m_option", NULL);

   return;

}  /* process_m_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process cal file option.                                              *|
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

static	void	process_S_option(char	*optargs)
{
   TRACE (Func_Entry, "process_S_option", NULL);

   /* issue warning if binary output is being overridden */

   if (cmd_line_flags.binary_output) {  /* Driver issues this message */
      cmd_line_flags.binary_output = FALSE;
   }

   /* issue warning if cal file listing is being overridden */  

   if (on_off_flags.assembly_listing_file) {	 
      ntr_msg_queue(0, 911, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
      on_off_flags.assembly_listing_file = FALSE;  
   }

   cmd_line_flags.assembly_output = TRUE;
   strncpy (assembly_file, optargs, MAX_FILE_NAME_SIZE);
   assembly_file[MAX_FILE_NAME_SIZE-1] = EOS;

   TRACE (Func_Exit, "process_S_option", NULL);

   return;

}  /* process_S_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process debug option.                                                 *|
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

static	void	process_G_option(char	*optargs)

{

   TRACE (Func_Entry, "process_G_option", NULL);

   if (EQUAL_STRS(optargs, "0")) {
      cmd_line_flags.debug_lvl = Debug_Lvl_0;
   }
   else if (EQUAL_STRS(optargs, "1")) {
      cmd_line_flags.debug_lvl = Debug_Lvl_1;
   }
   else if (EQUAL_STRS(optargs, "2")) {
      cmd_line_flags.debug_lvl = Debug_Lvl_2;
   }
   else if (EQUAL_STRS(optargs, "3")) {  /* -G deferred */
      ntr_msg_queue(0, 886, Log_Warning, 0, (char *) NULL, 3 ,ARG_ARG);
      cmd_line_flags.debug_lvl = Debug_Lvl_3;
   }

# if defined(_ACCEPT_CMD_Gd)

   /* This is set by the sgi commandline if debug symbols are to be generated */
   /* for Dwarf.  At the moment, this just ensures that parameters are sent   */
   /* through so that named constants can be found in the debug symbol table. */

   else if (EQUAL_STRS(optargs, "d")) {
      cmd_line_flags.dwarf_debug = TRUE;
   }
# endif

   else {
      ntr_msg_queue(0, 78, Log_Error, 0, optargs, 'G', ARG_STR_ARG);
   }

   TRACE (Func_Exit, "process_G_option", NULL);

   return;

}  /* process_G_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process pdgcs debug option.                                           *|
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

static	void	process_v_option(char	*optargs)

{

   TRACE (Func_Entry, "process_v_option", NULL);

#if defined(_DEBUG) && defined(_ENABLE_FEI)
   process_v_dbg_flags (optargs);		/* pdgcs debug calls  */
#else
   /* Command line has an invalid option. */
   ntr_msg_queue(0, 77, Log_Error, 0, (char *) NULL, 'v' ,ARG_ARG);
#endif

   TRACE (Func_Exit, "process_v_option", NULL);

   return;

}  /* process_v_option */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process line size option.                                             *|
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

static	void	process_N_option(char	*optargs)

{

   TRACE (Func_Entry, "process_N_option", NULL);

   if (cmd_line_flags.src_form != Fixed_Form) {

      /* Line size option ignored.  Free source form is in effect. */

      ntr_msg_queue(0, 11, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
   }
   else if (EQUAL_STRS(optargs, "72")) {
      cmd_line_flags.line_size_80 = FALSE;
      cmd_line_flags.line_size_132 = FALSE;
   }
   else if (EQUAL_STRS(optargs, "80")) {
      cmd_line_flags.line_size_80 = TRUE;
      cmd_line_flags.line_size_132 = FALSE;
   }
   else if (EQUAL_STRS(optargs, "120")) { /* KAY - support */
      ntr_msg_queue(0, 1659, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
      cmd_line_flags.line_size_132 = TRUE;
      cmd_line_flags.line_size_80 = FALSE;
   }
   else if (EQUAL_STRS(optargs, "132")) {
      cmd_line_flags.line_size_132 = TRUE;
      cmd_line_flags.line_size_80 = FALSE;
   }
   else { /* option has an invalid argument */
      ntr_msg_queue(0, 78, Log_Error, 0, optargs, 'N', ARG_STR_ARG);
   }

   TRACE (Func_Exit, "process_N_option", NULL);

   return;

}  /* process_N_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process MPP number of PEs option.                                     *|
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

static	void	process_X_option(char	*optargs)

{
#  if defined(_ACCEPT_CMD_X)
   char		*opt_chk;
   int		 n_pes;
# endif


   TRACE (Func_Entry, "process_X_option", NULL);

#  if defined(_ACCEPT_CMD_X)

      /* validate that -X arg contains only digits */

      if (*optargs == 'm') {
         cmd_line_flags.malleable = TRUE;

         if (set_MPP_num_pes) {
            set_MPP_num_pes	= FALSE;

            /* -X # overrides -Xm */

            ntr_msg_queue(0, 1231, Log_Warning, 0, "m", 0, STR_ARG);

# if defined(_TARGET_OS_MAX)
            cmd_line_flags.MPP_num_pes = 0;
# else
            cmd_line_flags.MPP_num_pes = 1;
# endif
         }
      }
      else {

         opt_chk = optargs;

         while (isdigit(*opt_chk)) {
            opt_chk++;
         }

         n_pes = atoi (optargs);

         if (*opt_chk != EOS) {
            ntr_msg_queue(0, 78, Log_Error, 0, optargs, 'X', ARG_STR_ARG);
         }
         else if (cmd_line_flags.malleable) {
            ntr_msg_queue(0, 1231, Log_Warning, 0, optargs, 0, STR_ARG);
            cmd_line_flags.malleable = FALSE;
         }
         else if (n_pes < 1 || n_pes > 2048) {
            ntr_msg_queue(0, 1238, Log_Error, 0, optargs, 0, STR_ARG);
         }

         set_MPP_num_pes		= TRUE;
         cmd_line_flags.MPP_num_pes	= n_pes;
      }

#  else  /* Unsupported */
      ntr_msg_queue(0, 797, Log_Warning, 0, (char *) NULL, 'X' ,ARG_ARG);
#  endif

   TRACE (Func_Exit, "process_X_option", NULL);

   return;

}  /* process_X_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	process_q_option handles expression evaluator args passed via argv    *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-q option arguments			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_q_option (char *optargs)

{
   int		 ch;
   char		 err_str[2];


   TRACE (Func_Entry, "process_q_option", NULL);

   while (ch = *optargs++) {

      switch (ch) {
      case 'e':
         cmd_line_flags.expression_eval_expr = TRUE;
         break;

      case 's':
         cmd_line_flags.expression_eval_stmt = TRUE;
         break;

      default:
         err_str[0] = ch;
         err_str[1] = EOS;

         /* option has an invalid argument */

         ntr_msg_queue(0, 78, Log_Error, 0, err_str, 'q', ARG_STR_ARG);
         break;
      }

   }  /* while */
 
   TRACE (Func_Exit, "process_q_option", NULL);

   return;

}  /* process_q_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	process_r_option handles runtime checking options passed via argv     *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-r option arguments			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_r_option (char *optargs)

{
   int		 ch;
   char		 err_str[2];


   TRACE (Func_Entry, "process_r_option", NULL);

   while (ch = *optargs++) {

      switch (ch) {
      case 'g':

#        if defined(_ACCEPT_CMD_ed_g)
	    on_off_flags.assembly_listing_file = TRUE;

            /* issue warning if cal file output is being overridden */  

            if (cmd_line_flags.assembly_output) {	 
               ntr_msg_queue(0, 388, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
               cmd_line_flags.assembly_output = FALSE;  
               cmd_line_flags.binary_output = TRUE;  
            }
#        else  /* doc'd as -rg */
            ntr_msg_queue(0, 744, Log_Warning, 0, "g", 'r', ARG_STR_ARG);
#        endif
         break;

      default:
         err_str[0] = ch;
         err_str[1] = EOS;

         /* option has an invalid argument */

         ntr_msg_queue(0, 78, Log_Error, 0, err_str, 'r', ARG_STR_ARG);
         break;
      }	 /* switch */
   }  /* while */
 
   TRACE (Func_Exit, "process_r_option", NULL);

   return;

}  /* process_r_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	process_R_option handles runtime checking options passed via argv     *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-R option arguments			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_R_option (char *optargs)

{
   int		 ch;
   char		 err_str[2];
   char		*cp;


   TRACE (Func_Entry, "process_R_option", NULL);

   while (ch = *optargs++) {

      switch (ch) {
      case 'a':
         cmd_line_flags.runtime_argument	= TRUE;
         break;

      case 'b':
         cmd_line_flags.runtime_bounds		= TRUE;
         break;

      case 'c':
         cmd_line_flags.runtime_conformance	= TRUE;
         break;

      case 'C':
         cmd_line_flags.runtime_arg_call	= TRUE;
         break;

      case 'E':
         cmd_line_flags.runtime_arg_entry	= TRUE;
         break;

      case 'M':

         while (*optargs != EOS && isdigit(*optargs)) {

            for (cp = optargs;
                 *optargs != BLANK  &&  *optargs != COMMA  &&  *optargs != EOS;
                 ++optargs);

            if (*optargs != EOS) {
               *optargs = EOS;
               *optargs++;
            }

            num_argchck_suppress_msg++;
            argchck_suppress_msg[num_argchck_suppress_msg] = atoi(cp);
         }

         argchck_suppress_msg[num_argchck_suppress_msg+1] = 0;
         break;

      case 'n':
         cmd_line_flags.runtime_arg_count_only	= TRUE;
         break;

      case 'p':
         cmd_line_flags.runtime_ptr_chk		= TRUE;
         break;

      case 's':
         cmd_line_flags.runtime_substring	= TRUE;
         break;

      default:
         err_str[0] = ch;
         err_str[1] = EOS;

         /* option has an invalid argument */

         ntr_msg_queue(0, 78, Log_Error, 0, err_str, 'R', ARG_STR_ARG);
         break;
      }	 /* switch */
   }  /* while */
 
   TRACE (Func_Exit, "process_R_option", NULL);

   return;

}  /* process_R_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	process_s_option handles size options passed via argv                 *|
|*									      *|
|* Input parameters:							      *|
|*	optargs			-s option arguments			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_s_option (char *optargs)
{
   TRACE (Func_Entry, "process_s_option", NULL);

   if (EQUAL_STRS(optargs, "integer8")) {
         cmd_line_flags.s_integer8		= TRUE;
   }
   else if (EQUAL_STRS(optargs, "logical8")) {
         cmd_line_flags.s_logical8		= TRUE;
   }
   else if (EQUAL_STRS(optargs, "real8")) {
         cmd_line_flags.s_real8			= TRUE;
   }
   else if (EQUAL_STRS(optargs, "complex8")) {
         cmd_line_flags.s_complex8		= TRUE;
   }
   else if (EQUAL_STRS(optargs, "doubleprecision16")) {
         cmd_line_flags.s_doubleprecision16	= TRUE;
   }
   else if (EQUAL_STRS(optargs, "doublecomplex16")) {
         cmd_line_flags.s_doublecomplex16	= TRUE;
   }
   else if (EQUAL_STRS(optargs, "pointer8")) {
         cmd_line_flags.s_pointer8		= TRUE;
   }
   else if (EQUAL_STRS(optargs, "cf77types")) {

#     if defined(_ACCEPT_CMD_s_cf77types)
         cmd_line_flags.s_cf77types	= TRUE;
         ntr_msg_queue(0, 1172, Log_Warning, 0, (char *) NULL, 0, NO_ARG);
#     else
         ntr_msg_queue(0, 744, Log_Warning, 0, "cf77types", 's', ARG_STR_ARG);
#     endif
   }
   else if (EQUAL_STRS(optargs, "float64")) {

#     if defined(_ACCEPT_CMD_s_64)
         cmd_line_flags.s_float64	= TRUE;
#     else
         ntr_msg_queue(0, 744, Log_Warning, 0, "float64", 's', ARG_STR_ARG);
#     endif
   }
   else if (EQUAL_STRS(optargs, "default64")) {

#     if defined(_ACCEPT_CMD_s_64)
         cmd_line_flags.s_float64	= TRUE;
         cmd_line_flags.s_default64	= TRUE;
#     else
         ntr_msg_queue(0, 744, Log_Warning, 0, "default64", 's', ARG_STR_ARG);
#     endif
   }
   else if (EQUAL_STRS(optargs, "default32")) {

#     if defined(_ACCEPT_CMD_s_32)
         cmd_line_flags.s_default32	= TRUE;
#     else
         ntr_msg_queue(0, 744, Log_Warning, 0, "default32", 's', ARG_STR_ARG);
#     endif
   }
   else {
      ntr_msg_queue(0, 78, Log_Error, 0, optargs, 's', ARG_STR_ARG);
   }
 
   TRACE (Func_Exit, "process_s_option", NULL);

   return;

}  /* process_s_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	handle the -D command line option to define a preprocessing id.       *|
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

static void process_D_option(char *optargs)

{
   int			idx;
   char			str[MAX_ID_LEN+1];
   char 		*value;


   TRACE (Func_Entry, "process_D_option", NULL);

   idx = 0;

   while (optargs[idx] != EQUAL && optargs[idx] != EOS && idx < MAX_ID_LEN) {
      str[idx] = optargs[idx];
      idx++;
   }

   str[idx] = '\0';

   if (optargs[idx] != EQUAL && optargs[idx] != EOS) {
      /* id too long */
      ntr_msg_queue(0, 78, Log_Error, 0, optargs, 'D', ARG_STR_ARG);
   }

   if (optargs[idx] == EQUAL) {
      idx++;
      value = &(optargs[idx]);
   }
   else {
      value = NULL;
   }

   if (! enter_cmd_line_cc_define(str, value, TRUE)) {
      /* problem with the string */
      ntr_msg_queue(0, 78, Log_Error, 0, optargs, 'D', ARG_STR_ARG);
   }

   TRACE (Func_Exit, "process_D_option", NULL);

   return;

}  /* process_D_option */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      handle the -U command line option to undefine a preprocessing id.     *|
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

static void process_U_option(char *optargs)

{
   int                  idx;
   char                 str[MAX_ID_LEN+1];


   TRACE (Func_Entry, "process_U_option", NULL);

   idx = 0;

   while (optargs[idx] != EOS && idx < MAX_ID_LEN) {
      str[idx] = optargs[idx];
      idx++;
   }

   str[idx] = '\0';

   if (optargs[idx] != EOS) { /* id too long */
      ntr_msg_queue(0, 78, Log_Error, 0, optargs, 'U', ARG_STR_ARG);
   }

   if (! enter_cmd_line_cc_define(str, NULL, FALSE)) {
      /* problem with the string */
      ntr_msg_queue(0, 78, Log_Error, 0, optargs, 'U', ARG_STR_ARG);
   }

   TRACE (Func_Exit, "process_U_option", NULL);

   return;

}  /* process_U_option */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
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
static void validate_s_option( void )

{
   TRACE (Func_Entry, "validate_s_option", NULL);

   if (cmd_line_flags.s_float64) {

      if (cmd_line_flags.s_real8) {
         cmd_line_flags.s_real8		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s float64\n-s real8\n-s float64",
                       0, MULT_STR_ARG);
      }

      if (cmd_line_flags.s_complex8) {
         cmd_line_flags.s_complex8		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s float64\n-s complex8\n-s float64",
                       0, MULT_STR_ARG);
      }

      if (cmd_line_flags.s_doubleprecision16) {
         cmd_line_flags.s_doubleprecision16	= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s float64\n-s doubleprecision16\n-s float64",
                       0, MULT_STR_ARG);
      }

      if (cmd_line_flags.s_doublecomplex16) {
         cmd_line_flags.s_doublecomplex16	= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s float64\n-s doublecomplex16\n-s float64",
                       0, MULT_STR_ARG);
      }
   }

   if (cmd_line_flags.s_default64) {

      if (cmd_line_flags.s_integer8) {
         cmd_line_flags.s_integer8		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s default64\n-s integer8\n-s default64",
                       0, MULT_STR_ARG);
      }
    
      if (cmd_line_flags.s_logical8) {
         cmd_line_flags.s_logical8		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s default64\n-s logical8\n-s default64",
                       0, MULT_STR_ARG);
      }
    
      if (cmd_line_flags.s_real8) {
         cmd_line_flags.s_real8			= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s default64\n-s real8\n-s default64",
                       0, MULT_STR_ARG);
      }

      if (cmd_line_flags.s_complex8) {
         cmd_line_flags.s_complex8		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s default64\n-s complex8\n-s default64",
                       0, MULT_STR_ARG);
      }
    
      if (cmd_line_flags.s_doubleprecision16) {
         cmd_line_flags.s_doubleprecision16	= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s default64\n-s doubleprecision16\n-s default64",
                       0, MULT_STR_ARG);
      }

      if (cmd_line_flags.s_doublecomplex16) {
         cmd_line_flags.s_doublecomplex16	= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s default64\n-s doublecomplex16\n-s default64",
                       0, MULT_STR_ARG);
      }
   }

   if (cmd_line_flags.s_default32) {

      if (cmd_line_flags.s_integer8) {
         cmd_line_flags.s_integer8		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s default32\n-s integer8\n-s default32",
                       0, MULT_STR_ARG);
      }
    
      if (cmd_line_flags.s_logical8) {
         cmd_line_flags.s_logical8		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s default3\n-s logical8\n-s default32",
                       0, MULT_STR_ARG);
      }
    
      if (cmd_line_flags.s_real8) {
         cmd_line_flags.s_real8		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s default32\n-s real8\n-s default32",
                       0, MULT_STR_ARG);
      }
    
      if (cmd_line_flags.s_complex8) {
         cmd_line_flags.s_complex8		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s default32\n-s complex8\n-s default32",
                       0, MULT_STR_ARG);
      }

      if (cmd_line_flags.s_doubleprecision16) {
         cmd_line_flags.s_doubleprecision16	= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s default32\n-s doubleprecision16\n-s default32",
                       0, MULT_STR_ARG);
      }

      if (cmd_line_flags.s_doublecomplex16) {
         cmd_line_flags.s_doublecomplex16	= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s default32\n-s doublecomplex16\n-s default32",
                       0, MULT_STR_ARG);
      }
   }

   if (cmd_line_flags.s_cf77types) {

      if (cmd_line_flags.s_integer8) {
         cmd_line_flags.s_integer8	= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s cf77types\n-s integer8\n-s cf77types",
                       0, MULT_STR_ARG);
      }
    
      if (cmd_line_flags.s_logical8) {
         cmd_line_flags.s_logical8		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s cf77types\n-s logical8\n-s cf77types",
                       0, MULT_STR_ARG);
      }
    
      if (cmd_line_flags.s_real8) {
         cmd_line_flags.s_real8		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s cf77types\n-s real8\n-s cf77types",
                       0, MULT_STR_ARG);
      }
    
      if (cmd_line_flags.s_complex8) {
         cmd_line_flags.s_complex8		= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s cf77types\n-s complex8\n-s cf77types",
                       0, MULT_STR_ARG);
      }

      if (cmd_line_flags.s_doubleprecision16) {
         cmd_line_flags.s_doubleprecision16	= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-s cf77types\n-s doubleprecision16\n-s cf77types",
                       0, MULT_STR_ARG);
      }

      if (cmd_line_flags.s_doublecomplex16) {
         cmd_line_flags.s_doublecomplex16	= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                        "-s cf77types\n-s doublecomplex16\n-s cf77types",
                       0, MULT_STR_ARG);
      }
   }

   if (!on_off_flags.enable_double_precision) {

      if (cmd_line_flags.s_doubleprecision16) {
         cmd_line_flags.s_doubleprecision16	= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-dp\n-s doubleprecision16\n-dp",
                       0, MULT_STR_ARG);
      }

      if (cmd_line_flags.s_doublecomplex16) {
         cmd_line_flags.s_doublecomplex16	= FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-dp\n-s doublecomplex16\n-dp",
                       0, MULT_STR_ARG);
      }
   }

   if (set_i_option) {

      if (cmd_line_flags.s_integer8) {
         cmd_line_flags.s_integer8 = FALSE;
         ntr_msg_queue(0, 1353, Log_Warning, 0, 
                       "-i32\n-s integer8\n-i32",
                       0, MULT_STR_ARG);
      }
   }


   TRACE (Func_Exit, "validate_s_option", NULL);

   return;

}  /* validate_s_option */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
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
static void set_system_module_path( void )
{

   char		*directory;
   int		 idx;
   int		 length;
   char		*next_dir_ptr;
   char		*path_var;
   char		*ptr;
   int		 save_file_path_tbl_idx;

   extern	boolean	is_directory(char *);


   TRACE (Func_Entry, "set_system_module_path", NULL);

   /* Put the system directory into the module search path.   */
   /* It should end up being the very last thing searched.    */
   /* Currently we are supporting both old and new system     */
   /* variables.  So there are two places to check.           */

   path_var	= SYSTEM_MODULE_USE_VAR;
   directory	= getenv(path_var);

   if (directory != NULL) {

      /* Set up for the new implementation.  One environment variable */
      /* with a colon separate list of paths and file names.          */

      do {
         next_dir_ptr = strchr(directory, ':');

         if (next_dir_ptr != NULL) {
            *next_dir_ptr	= '\0';

            if (++next_dir_ptr == '\0') {
               next_dir_ptr	= NULL;
            }
         }

         TBL_REALLOC_CK(file_path_tbl, 1);
         CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);
         FP_NAME_IDX(file_path_tbl_idx)		= str_pool_idx + 1;
         FP_SRCH_THE_FILE(file_path_tbl_idx)	= TRUE;
         FP_CLASS(file_path_tbl_idx)       	= Unknown_Fp;
         FP_SYSTEM_FILE(file_path_tbl_idx)	= TRUE;
         length					= strlen(directory) + 1;
         FP_NAME_LEN(file_path_tbl_idx)		= length;
         length    				= WORD_LEN(length);

         TBL_REALLOC_CK(str_pool, length);

         for (idx = FP_NAME_IDX(file_path_tbl_idx); idx <= str_pool_idx; idx++){
            str_pool[idx].name_long = 0;
         }

         strcpy(FP_NAME_PTR(file_path_tbl_idx), directory);
         save_file_path_tbl_idx	= file_path_tbl_idx;

# if (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))

         if (is_directory(FP_NAME_PTR(file_path_tbl_idx))) {

            /* Add the ABI search path for system dependent ABI modules */

            TBL_REALLOC_CK(file_path_tbl, 1);
            CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);
            FP_NAME_IDX(file_path_tbl_idx)	= str_pool_idx + 1;
            FP_SRCH_THE_FILE(file_path_tbl_idx)	= TRUE;
            FP_CLASS(file_path_tbl_idx)		= Unknown_Fp;
            FP_SYSTEM_FILE(file_path_tbl_idx)	= TRUE;
            length  = (cmd_line_flags.s_pointer8) ? 3:4;
            length += FP_NAME_LEN(save_file_path_tbl_idx);
            FP_NAME_LEN(file_path_tbl_idx)	= length;
            length				= WORD_LEN(length);
            TBL_REALLOC_CK(str_pool, length);

            for (idx = FP_NAME_IDX(file_path_tbl_idx);
                 idx <= str_pool_idx; idx++) {
               str_pool[idx].name_long = 0;
            }
            strcpy(FP_NAME_PTR(file_path_tbl_idx), directory);
            ptr = FP_NAME_PTR(file_path_tbl_idx) +
                  FP_NAME_LEN(save_file_path_tbl_idx) - 1;
            strcpy(ptr, (cmd_line_flags.s_pointer8) ? "/64" : "/n32");
            idx = module_path_idx;

            while (FP_NEXT_FILE_IDX(idx) != NULL_IDX) {
               idx = FP_NEXT_FILE_IDX(idx);
            }
            FP_NEXT_FILE_IDX(idx)	= file_path_tbl_idx;
         }
# endif

         idx = module_path_idx;

         while (FP_NEXT_FILE_IDX(idx) != NULL_IDX) {
            idx = FP_NEXT_FILE_IDX(idx);
         }
         FP_NEXT_FILE_IDX(idx)	= save_file_path_tbl_idx;
         directory		= next_dir_ptr;
      }
      while (directory != NULL);
   }
   else {
      path_var	= MODULE_USE_SYSTEM_PATH_VAR;
      directory	= getenv(path_var);

      if (directory != NULL) {
         TBL_REALLOC_CK(file_path_tbl, 1);
         CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);
         FP_NAME_IDX(file_path_tbl_idx)		= str_pool_idx + 1;
         FP_SRCH_THE_FILE(file_path_tbl_idx)	= TRUE;
         FP_CLASS(file_path_tbl_idx)       	= Unknown_Fp;
         FP_SYSTEM_FILE(file_path_tbl_idx)	= TRUE;

# if (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
         length = strlen(directory) + 1;
# else
         length = strlen(directory) + strlen(MODULE_USE_SYSTEM_FILE) + 1;
# endif

         FP_NAME_LEN(file_path_tbl_idx)		= length;
         length    				= WORD_LEN(length);

         TBL_REALLOC_CK(str_pool, length);

         for (idx = FP_NAME_IDX(file_path_tbl_idx); idx <= str_pool_idx; idx++){
            str_pool[idx].name_long = 0;
         }

         strcpy(FP_NAME_PTR(file_path_tbl_idx), directory);

# if ! (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
         ptr = FP_NAME_PTR(file_path_tbl_idx) + strlen(directory);
         strcpy(ptr, "/");
         strcpy(++ptr, MODULE_USE_SYSTEM_FILE);
# endif
         idx = module_path_idx;

         while (FP_NEXT_FILE_IDX(idx) != NULL_IDX) {
            idx = FP_NEXT_FILE_IDX(idx);
         }
         FP_NEXT_FILE_IDX(idx)     = file_path_tbl_idx;
      }
   }

   TRACE (Func_Exit, "set_system_module_path", NULL);

   return;

}  /* set_system_module_path */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add array names to the file path table for the -O swap array option.  *|
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

static	void	process_reshape_array(char	*optargs)

{
   boolean	 found_one;
   int		 fp_idx;
   int		 idx;
   int		 length;
   char		*array_name;
   int		 start_idx;
   char		*str_ptr;


   TRACE (Func_Entry, "process_reshape_array", NULL);

   start_idx		= file_path_tbl_idx + 1;
   found_one		= FALSE;
   opt_flags.reshape	= TRUE;
      
   while (*optargs != EOS) {

      for (array_name = optargs;
          *optargs != BLANK  &&  *optargs != COMMA  &&  *optargs != EOS;
          ++optargs);

      if (*optargs != EOS) {
	 *optargs = EOS;
	 *optargs++;
      }

      if (found_one) {  /* Second or subsequent time through */
         FP_NEXT_FILE_IDX(file_path_tbl_idx) = file_path_tbl_idx + 1;
      }

      TBL_REALLOC_CK(file_path_tbl, 1);
      CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);

      found_one				= TRUE;
      FP_NAME_IDX(file_path_tbl_idx)	= str_pool_idx + 1;
      length				= strlen(array_name);
      FP_CLASS(file_path_tbl_idx)	= Reshape_Array_Fp;
      FP_NAME_LEN(file_path_tbl_idx)	= length;

      TBL_REALLOC_CK(str_pool, WORD_LEN(length));

      str_pool[str_pool_idx].name_long	= 0; /* Zero out last word */

      str_ptr	= FP_NAME_PTR(file_path_tbl_idx);

      for (idx = 0;  idx < length;  idx++) {
         str_ptr[idx] = toupper(array_name[idx]);
      }
   }

   if (found_one) {

      if (opt_flags.reshape_idx == NULL_IDX) {
         opt_flags.reshape_idx     = start_idx;
      }
      else {
         fp_idx = opt_flags.reshape_idx;

         /* Find the end of the list of files/paths.  These must be ordered. */

         while (FP_NEXT_FILE_IDX(fp_idx) != NULL_IDX) {
            fp_idx = FP_NEXT_FILE_IDX(fp_idx);
         }
         FP_NEXT_FILE_IDX(fp_idx)               = start_idx;
      }
   }

#  if !defined(_ACCEPT_CMD_O_RESHAPE)
      opt_flags.reshape_idx	= NULL_IDX;
      PRINTMSG (0, 744, Log_Warning, 0, 'O', "reshape");
#  endif

   TRACE (Func_Exit, "process_reshape_array", NULL);

   return;

}  /* process_reshape_array */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add path names to the file path table.  Used by both -p and -I.       *|
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

static	void	process_J_option(char	*optargs)

{

   TRACE (Func_Entry, "process_J_option", NULL);

# if defined(_ACCEPT_CMD_J) || defined(KEY) /* Bug 4210 */
   strncpy (mod_out_path, optargs, MAX_FILE_NAME_SIZE);
   mod_out_path[MAX_FILE_NAME_SIZE-1]	= EOS;
   cmd_line_flags.mod_out_path		= TRUE;
# else
   ntr_msg_queue(0, 77, Log_Error, 0, (char *) NULL, 'J', ARG_ARG);
# endif

   TRACE (Func_Exit, "process_J_option", NULL);

   return;

}  /* process_J_option */

/**************************************************************\
 * add -help option to dump out all possible opstions of
 * Open64 frontend
 * there are
 * a,b,d,e,f,g,i,k,m,o,p,q,r,R,s,S,t,u,v,A,C,D,x,F,G,I,J,M,
 * N,P,U,V,X,Y,Z options
\*************************************************************/

static void dump_options(void) 
  {
   int i;
   char *all_options[] = {
    " ",
    "The following options are available in Open64 frontend:",
    "-a: memory options ",
    "	lign32",
    "	lign64",
    "	dalign",
    "	static_threadprivate",
    "	taskcommon",
    "	pad",
    "-b: binary file name ",
    "-d: off flags",
    "-e: on flags",
    "-f: souce form ",
    "-g: -G O",
    "-i: integer size", 
    "-k: Solaris profiling",
    "-m: process message level options",
    "-O: optimization options",
    "-p: module path name ",
    "-q: expression evaluator args passed via argv",
    "-r: runtime checking options passed via argv",
    "-R: runtime checking options passed via argv",
    "-s: size options passed via argv",
    "	integer8",
    "	logical8",
    "	real8",
    "	complex8",
    "	doubleprecision16",
    "	doublecomplex16",
    "	pointer8",
    "	cf77types",
    "	float64",
    "	default64",
    "	default32",
    "-S: cal file option",
    "-t: the command line truncation option passed via argv (t switch)",
    "-u: command line dump options passed via argv(u switch)",
    "         The following options are available in a compiler with -u: ",
    "          ",
    "         abort_ansi       Abort compilation after first ANSI msg.",
    "         cray_compatible  Sgi  turns s_default64 on, int1,2 off.",
    "         f                Unimplemented - controls fortran output.",
    "         dsm              Set by mongoose driver - not a user option.",
    "         fmm              Controls f-- prototype.",
    "         fmm1             Controls f-- prototype.",
    "         fmm2             Controls f-- prototype.",
    "         mod_version      Prints each module version number being read in",
    "         mp               Allow recognition of the SGI directives.",
    "         no_dim_pad       Do not pad out missing dimensions.",
    "         no_mod_output    Do not do module output.",
    "         open_mp          Allow recognition of the open mp directives.",
    "         pack_half_word   Turn on component packing for sdefault32",
    "         preinline=       Create an inline template for this file.",
    "         pvp_test         Used for meta development.",
    "         show             Show input command line to frontend.",
    "          ",
    "         The following options are available in a debug compiler with -u:",
    "          ",
    "         all              Dump all tables.  Do not do any tracing.",
    "         bd               Dump the bounds table.",
    "         blk              Dump the Block Stack.",
    "         cmd              Dump the commandline table.",
    "         cn               Dump the Constant table.",
    "         defines          Dump the build definitions.",
    "         file=            Specify the name of the debug output file.",
    "         fortran          Dump the output IR in a Fortran format.",
    "         fp               Dump the File Path table.",
    "         ftrace           Activate the function trace.",
    "         gl               Dump the Global Line table.",
    "         intrin           Dump the Intrinsic table.",
    "         ir1              Dump the IR after Pass 1.",
    "         ir2              Dump the IR after Pass 2.",
    "         ir3              Dump the IR after inlining.",
    "         ir4              Dump the IR after swapping dimensions.",
    "         mem_report       Provide a report of front-end memory usage.",
    "         msg              Issue an abort for all zero line numbers.",
    "         mtrace           Trace front-end memory usage.",
    "         names            Dump the local, global and hidden name tables.",
    "         pdg              Dump all calls to the PGDCS interface.",
    "         pdt              Dump PDT info while searching for modules.",
    "         sb               Dump the Storage Block table.",
    "         scp              Dump the scope table.",
    "         src              Dump whole source program.",
    "         stderr           Output msgs to stderr rather than ordering.",
    "         stmt             Dump each statement.",
    "         sytb             Dump the symbol table.",
    "         typ              Dump the Type table.",
    "         cnout            Dump constants as binary to stdout.",
    "-v: pdgcs debug option",
    "-A: add use names to the files path tables for implicit use",
    "-C: CIF file output is requested via the -C option",
    "-D: define a preprocessing id",
    "-x: disregard CDIRs",
    "-F: fortran macro expansion",
    "-G: debug option",
    "-I: include option",
    "-J: .mod output locate",
    "-M: disable message numbers",
    "-N: fixed line size",
    "-P: position independent code model",
    "-U: undefine a preprocessing id",
    "-V: version option",
    "-X: MPP num PE's option",
    "-Y: ccg debug options",
    "-Z: co_array fortran",
    " ",
    "If you need to know arguments of option WHAT,mostly you can get the information by reading:",
    "\"process_WHAT_option\" in the source file cmd_line.c.",
    " ",
    "DONE"
   };

  i=0;
  while (strcmp(all_options[i],"DONE")) printf("%s\n",all_options[i++]);
  return;
 }

#ifdef KEY /* Bug 14150 */
/*
 * dt_attr_idx	AT_Tbl_Idx for a derived type
 * return	True if this is an ISO_C_BINDING type which must be treated
 *		as integer*4 when it is a function result type, so as to be
 *		compatible with C functions returning "void *" under the
 *		X8664 -m32 ABI, which normally passes even the smallest
 *		derived-type result via the parameter list instead
 */
boolean
c_ptr_abi_trouble(int dt_attr_idx) {
#if defined(TARG_X8664)
  /* No problem unless we're Intel -m32 and it's a derived type */
  if (!(is_Target_ABI_n32() && AT_OBJ_CLASS(dt_attr_idx) == Derived_Type)) {
    return FALSE;
  }
  /* When C_PTR or C_FUNPTR has arrived from an intrinsic module, p_driver.c
   * has already marked it for special handling */
  if (AT_IS_INTRIN(dt_attr_idx)) {
    return TRUE;
  }
  /* When C_PTR or C_FUNPTR appears in the ISO_C_BINDING module itself,
   * we need to handle it specially because p_driver never sees it. */
  if (on_off_flags.intrinsic_module_gen) {
    const char *name = AT_OBJ_NAME_PTR(dt_attr_idx);
    if (0 == strcmp(name, "C_PTR") || 0 == strcmp(name, "C_FUNPTR")) {
      AT_IS_INTRIN(dt_attr_idx) = TRUE;
      return TRUE;
    }
  }
#endif /* defined(TARG_X8664) */
  return FALSE;
}

boolean
is_x8664_n32() {
#if defined(TARG_X8664)
  return is_Target_ABI_n32();
#else /* defined(TARG_X8664) */
  return FALSE;
#endif /* defined(TARG_X8664) */
}
#endif /* KEY Bug 14150 */


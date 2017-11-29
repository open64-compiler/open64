/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*

  Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.

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



static char USMID[] = "\n@(#)5.0_pl/sources/main.c	5.15	10/14/99 15:25:09\n";

# include "defines.h"		/* Machine dependent ifdefs */

# if (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX)) || defined(_HOST_OS_DARWIN)	/* Needed for timing information. */
# include <sys/time.h>
# include <sys/resource.h>
# endif

# include <time.h>

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "debug.m"

# ifdef _ARITH_H
# include "arith.h"
# endif

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "main.h"
# include "type.h"
# include "intrin.h"

#include "pathscale_defs.h"

extern	const	char	*fe_vers_ID(void);
extern	const	char	*fe_vers_number(void);
extern	const	char	*fe_vers_name(void);

extern	void	print_buffered_messages (void);

/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/
 

# ifdef _DEBUG
static void check_defines_compatibility (void);
static void check_enums_for_change(void);
# endif

static void	get_machine_chars (void);
static void	init_compiler (int, char *[]);
static void	init_date_time_info (void);
static void	init_release_level (void);
static void	make_table_changes (void);
static void	print_id_line (void);
static void	set_compile_info_for_target (void);

# if defined(_WHIRL_HOST64_TARGET64) && defined(TARG_X8664)
/* OSP_467, #5, fixup the stride_multi_unit_in_bits array */
static void     fixup_unit_length_in_bits(void);
# endif


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*    main entry into the Fortran 90 compiler;	initializes the compiler,     *|
|*    processes the command line, parses the input source and converts the    *|
|*    internal representation to what VPR expects.			      *|
|*									      *|
|* Input parameters:							      *|
|*		argc		     number of command line arguments	      *|
|*		argv		     argument strings			      *|
|*									      *|
|* Output parameters:							      *|
|*		none							      *|
|*									      *|
|* Returns:	nothing							      *|
|*									      *|
\******************************************************************************/

# ifdef _TARGET_OS_MAX
void the_real_main (int	 argc,
	   	    char *argv[])
# else
int main (int	 argc,
	   char *argv[])
# endif

{
   int		column_num;
   long		field_len;
   int		line_num;
   char	       *msg_name;
   int		save_statement_number = 0;

# if (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX)) || defined(_HOST_OS_DARWIN)
   		double		end_time;
   		double		start_time;
		/* char		time[20]; */
   		double		total_cpu_time;
   struct	rusage		ru;
# else

# if !defined(_HOST_OS_UNICOS)
   long		end_clock;
# endif
   		float		end_time;
   		float		start_time;
   		float		total_cpu_time;
# endif

# if defined(_HOST_OS_UNICOS) && defined(_DEBUG)
   lowmem_check();
# endif

# if defined(_TARGET32) && defined(_DEBUG)
   setbuf(stdout, NULL);
   setbuf(stderr, NULL);
# endif


# if defined(_HOST_OS_UNICOS)

   /* Lots of start up - ignore first call.  See the comment block that       */
   /* precedes procedure cif_summary_rec in fecif.c for a discussion of the   */
   /* timing methods used by the different platforms.			      */

   SECOND(&start_time);


   /* M_LOWFIT will eventually be in malloc.h. */
   /* When it is remove this definition.       */

# define M_LOWFIT	0107	 /* Use lowest-fit algorithm for allocation. */

   mallopt(M_LOWFIT, 1);

# elif defined(_HOST_OS_MAX)

   /* Use clock() on MPP's (in particular T3E's) because at the time this     */
   /* change was made, neither SECOND() nor SECONDR() worked on T3E's.        */
   /*                                                  LRR  4 Mar 1997        */

   clock();
   start_time = 0;

   /* M_LOWFIT will eventually be in malloc.h. */
   /* When it is remove this definition.       */

# define M_LOWFIT	0107	 /* Use lowest-fit algorithm for allocation. */

   mallopt(M_LOWFIT, 1);

# elif defined(_HOST_OS_SOLARIS)

   /* clock() is only semi-useful on a Sun because it rolls over in just over */
   /* 2147 seconds (about 36 minutes).  So on a Sun, we use clock() and       */
   /* time() both.  If elapsed time <= 2147 seconds, the accounting info will */
   /* show milliseconds (from clock()), else it will show seconds (because    */
   /* that is the accuracy of time()).  This resolution should be good enough */
   /* for a compilation exceeding 36 minutes.                                 */

   start_time = (float) time(NULL);
   clock();

# elif (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX)) || defined(_HOST_OS_DARWIN)

   getrusage (RUSAGE_SELF, &ru);
   start_time = (double) ru.ru_utime.tv_sec +
                (double) ru.ru_utime.tv_usec * 1e-6 +
                (double) ru.ru_stime.tv_sec +
                (double) ru.ru_stime.tv_usec * 1e-6;

# else

   start_time = 0;

# endif


   comp_phase = Pass1_Parsing;
   stmt_start_line = 1;			/* Set in case mem problems */

   init_compiler(argc, argv);			/* init and process cmd line */

   if (on_off_flags.preprocess_only) {
      goto PREPROCESS_ONLY_SKIP;
   }

   stmt_start_line = 0;

   while (LA_CH_CLASS != Ch_Class_EOF) {

      comp_phase = Pass1_Parsing;
      num_prog_unit_errors = 0;		/* Accum errs for pgm unit */

      OUTPUT_PASS_HEADER(Syntax_Pass);

      if (save_statement_number != 0) {
         statement_number = save_statement_number;
      }

      parse_prog_unit();

      save_statement_number = statement_number;

      if (LA_CH_CLASS == Ch_Class_EOF) {
         issue_deferred_msgs();
      }

      /* get current field length and save largest value */

      field_len = (long) sbrk(0);

# if defined(_HOST_OS_MAX)
      field_len &= (1 << 32) - 1;
# endif

      if (field_len > max_field_len) {		/* Max set in init_compiler */
	 max_field_len = field_len;		/* Track max usage */
      }

      PRINT_IR_TBL;	/* If -u ir and DEBUG compiler, print ir. */

      OUTPUT_PASS_HEADER(Semantics_Pass);

      semantics_pass_driver();			/* PASS 2 */

      if (SCP_IN_ERR(curr_scp_idx)) {
         some_scp_in_err = TRUE;
      }

      PRINT_ALL_SYM_TBLS;	/* If debug print -u options */
      PRINT_FORTRAN_OUT;	/* Print ir in a fortran format */

      line_num = SH_GLB_LINE(SCP_LAST_SH_IDX(curr_scp_idx));
      column_num = SH_COL_NUM(SCP_LAST_SH_IDX(curr_scp_idx));

      if (num_prog_unit_errors == 0) {
         if (opt_flags.inline_lvl > Inline_Lvl_0) {
            comp_phase = Inlining;
            inline_processing(SCP_FIRST_SH_IDX(curr_scp_idx));
            PRINT_IR_TBL3;	
         }
      }

      insert_global_directives = TRUE;

      comp_phase = Pdg_Conversion;
      if (dump_flags.preinline) { /* Do not do a full compile */

         if (ATP_PGM_UNIT(SCP_ATTR_IDX(MAIN_SCP_IDX)) == Module ||
             ATP_PGM_UNIT(SCP_ATTR_IDX(MAIN_SCP_IDX)) == Function ||
             ATP_PGM_UNIT(SCP_ATTR_IDX(MAIN_SCP_IDX)) == Subroutine) {
            curr_scp_idx = MAIN_SCP_IDX;
#ifdef KEY /* Bug 3477 */
            if (create_mod_info_file()) {  /* Creates a name for the file. */
	      create_mod_info_tbl();        /* Creates the table. */
	      output_mod_info_file();       /* Writes the table.  */
	      }
#else
            create_mod_info_file();  /* Creates a name for the file. */
            create_mod_info_tbl();        /* Creates the table. */
            output_mod_info_file();       /* Writes the table.  */
#endif /* KEY Bug 3477 */
            free_tables();                /* Frees the tables. */
         }
      }
      else {
#ifdef KEY /* Bug 3477 */
	 int do_output_file = FALSE;
#endif /* KEY Bug 3477 */
         if (ATP_PGM_UNIT(SCP_ATTR_IDX(MAIN_SCP_IDX)) == Module) {
#ifdef KEY /* Bug 3477 */
            do_output_file = create_mod_info_file();  /* Creates a name for the file. */
#else
            create_mod_info_file();  /* Creates a name for the file. */
#endif /* KEY Bug 3477 */
         }

         if (num_prog_unit_errors == 0 && (binary_output || assembly_output)) {
            cvrt_to_pdg(compiler_gen_date);
         }
         else if (ATP_PGM_UNIT(SCP_ATTR_IDX(MAIN_SCP_IDX)) == Module) {

            if (!SCP_IN_ERR(MAIN_SCP_IDX)) {
               curr_scp_idx = MAIN_SCP_IDX;
#ifdef KEY /* Bug 3477 */
	       if (do_output_file) {
		 create_mod_info_tbl();   /* Creates the table. */
		 output_mod_info_file();  /* Writes the table.  */
		 }
#else
               create_mod_info_tbl();   /* Creates the table. */
               output_mod_info_file();  /* Writes the table.  */
#endif /* KEY Bug 3477 */
            }

            free_tables();           /* Frees the tables. */
         }
         else {
            free_tables();           /* Frees the tables. */
         }
      }

      /* ALERT - At this point, the symbol tables are invalid. */

      /* Spit out the End Unit for the current program unit.  The End Unit    */
      /* is needed if the Compiler Information File (CIF) is being produced   */
      /* and for the buffered message file.				      */

      stmt_start_line = line_num;
      stmt_start_col = column_num;

      if (scp_tbl == NULL_IDX) {                  /* Table has been freed. */
         cif_end_unit_rec(program_unit_name);
      }
      else {
         cif_end_unit_rec(AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
      }

   }  /* while */

   clean_up_module_files();

# ifdef _NAME_SUBSTITUTION_INLINING
   if (!dump_flags.preinline)
# endif
      terminate_PDGCS();

   PRINT_GL_TBL;              /* Prints to debug_file ifdef _DEBUG and -u gl */
   PRINT_GN_TBL;              /* Prints to debug_file ifdef _DEBUG and -u gn */
#if defined(_DEBUG) && defined(KEY) /* Bug 8117 */
   if (dump_flags.arg_passing) {                                  \
      print_arg_passing(stderr);                                  \
   }
#endif /* defined(_DEBUG) && defined(KEY) Bug 8117 */

PREPROCESS_ONLY_SKIP:


# if defined(_HOST_OS_UNICOS) 

   SECOND(&end_time);

# elif defined(_HOST_OS_MAX)

   end_clock = clock();
   end_time  = 0;

# elif defined(_HOST_OS_SOLARIS)

   end_time  = (float) time(NULL);
   end_clock = clock();

# elif (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX)) || defined(_HOST_OS_DARWIN)

   getrusage(RUSAGE_SELF, &ru);
   end_time = (double) ru.ru_utime.tv_sec +
              (double) ru.ru_utime.tv_usec * 1e-6 +
              (double) ru.ru_stime.tv_sec +
              (double) ru.ru_stime.tv_usec * 1e-6;

# else

   end_time = 0;

# endif


   total_cpu_time = end_time - start_time;

   if (cif_need_unit_rec  &&  cif_first_pgm_unit) {

      /* Catastrophic errors, like a free source form program was compiled    */
      /* in fixed source form mode, so no Unit record was output.  Output     */
      /* enough records to keep libcif tools happy.  This routine needs to be */
      /* called whether or not a CIF is being written because the buffered    */
      /* message file also must have the correct format.		      */

      cif_fake_a_unit();
   }


   /* CAUTION:  The following code assumes that non-Cray platforms measure    */
   /* memory usage in terms of bytes and that there are 4 bytes per word.     */

   cif_summary_rec(release_level,
                   compiler_gen_date,
                   compiler_gen_time,
                   total_cpu_time,

# if defined(_HOST_OS_UNICOS)

                   (long) 0,
                   (some_scp_in_err) ? -3 : max_field_len);

# elif defined(_HOST_OS_MAX) 

                   end_clock,
                   (some_scp_in_err) ? -3 : max_field_len);

# elif (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX)) || defined(_HOST_OS_DARWIN)

                   (long) 0,
                   (some_scp_in_err) ? -3 : max_field_len/4);

# else /* defined(_HOST_OS_SOLARIS) */

                   end_clock,
                   (some_scp_in_err) ? -3 : max_field_len/4);

# endif                         
   

   /* Output compilation summary info if the -V option was specified on the   */
   /* command line.  Also, issue the summary information if any messages were */
   /* actually issued.          					      */

   if (cmd_line_flags.verify_option || 
       num_errors > 0               || 
       num_warnings > 0             ||
       num_cautions > 0             ||
       num_notes > 0                ||
       num_comments > 0             ||
       num_ansi > 0                 ||
       (num_optz_msgs > 0  &&  opt_flags.msgs)) { 
      print_buffered_messages();
      print_id_line();

      /* Output the summary lines.  The compilation time is in seconds.       */
      /* CAUTION:  The following non-Cray code assumes a 32-bit word.         */

# if defined(_HOST_OS_UNICOS)

      PRINTMSG (0, 104, Log_Summary, 0, (double) total_cpu_time);
      msg_name	= "cf90";

# elif defined(_HOST_OS_MAX)

      PRINTMSG (0, 104, Log_Summary, 0, (double) end_clock/1000000.0);
      msg_name	= "cf90";

# elif defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)
#ifdef PSC_TO_OPEN64
      msg_name	= OPEN64_NAME_PREFIX "f95";
#endif

# elif (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX))

      /* IRIX cannot handle the int to float change necessary to get the      */
      /* time printed correctly, so we'll convert it to a character string    */
      /* and use a different message.                                         */
      /*							    	      */
      /* LRR  4/28/97  In an email message from Rich Shapiro to me, he stated */
      /* he did not want this line in the summary lines.		      */

/*    sprintf(time, "%-1.2f", (double) total_cpu_time);
      PRINTMSG (0, 1310, Log_Summary, 0, time);             */
      msg_name	= "cf90";

# elif defined(_HOST_OS_SOLARIS)

      PRINTMSG (0, 104, Log_Summary, 0,
                (total_cpu_time <= 2147.0) ? (float) end_clock/1000000.0 :
                                             (float) total_cpu_time);
      msg_name	= "cf90";

# endif


      /* Maximum field length (maximum amount of memory used) in words        */
      /* (decimal).  							      */
      /* CAUTION:  Non-Cray platforms are assumed to measure memory usage in  */
      /* bytes and we assume 4 bytes per word.         			      */

# if defined(_HOST_OS_UNICOS)

      PRINTMSG (0, 105, Log_Summary, 0, max_field_len);

# elif ! (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))

      /* LRR  4/28/97  In an email message from Rich Shapiro to me, he stated */
      /* he did not want this line in the summary lines.		      */

      PRINTMSG (0, 105, Log_Summary, 0, max_field_len/4);

# endif


      /* Number of source lines compiled.				      */

# if (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)) && !defined(_TARGET_SV2)

      PRINTMSG (0, 1401, Log_Summary, 0, --curr_glb_line);

# else

      PRINTMSG (0, 106, Log_Summary, 0, --curr_glb_line);

# endif


      /* Number of messages issued.					      */

# if (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)) && !defined(_TARGET_SV2)

      PRINTMSG (0, 1403, Log_Summary, 0,
                num_errors,
                num_warnings,
                (opt_flags.msgs == 0) ?
                   (num_cautions + num_notes + num_comments) :
                   (num_cautions + num_notes + num_comments + num_optz_msgs),
                num_ansi);

# else

      PRINTMSG (0, 107, Log_Summary, 0,
                num_errors,
                num_warnings,
                (opt_flags.msgs == 0) ?
                   (num_cautions + num_notes + num_comments) :
                   (num_cautions + num_notes + num_comments + num_optz_msgs),
                num_ansi);


      /* Code: in words; data: in words.				      */

      /* LRR  4/28/97  In an email message from Rich Shapiro to me, he stated */
      /* he did not want this line in the summary lines.		      */

# if !defined(_TARGET_SV2)   /* Prints blank for sv2 right now. */
      PRINTMSG (0, 108, Log_Summary, 0, code_size, data_size);
# endif

# endif

      if (num_errors > 0               || 
          num_warnings > 0             ||
          num_cautions > 0             ||
          num_notes > 0                ||
          num_comments > 0             ||
          num_ansi > 0                 ||
          (num_optz_msgs > 0  &&  opt_flags.msgs)) { 
         PRINTMSG (0, 1636, Log_Summary, 0, msg_name, msg_name);
      }
   }  /* End of summary printing. */


# ifdef _DEBUG

   /* Get memory usage reports for these global tables. */

   final_src_input();

   MEM_REPORT(file_path_tbl);
   MEM_REPORT(global_attr_tbl);
   MEM_REPORT(global_bounds_tbl);
   MEM_REPORT(global_line_tbl);
   MEM_REPORT(global_name_tbl);
   MEM_REPORT(global_type_tbl);
   MEM_REPORT(str_pool);

# endif

   exit_compiler ((num_errors == 0) ? RC_OKAY : RC_USER_ERROR);

}  /* main */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*    initialize the compiler;	allocate initial data structure memory on the *|
|*    heap, initialize variables, the source stack, the symbol table and do   *|
|*    command line processing.						      *|
|*									      *|
|* Input parameters:							      *|
|*		argc		     number of command line arguments	      *|
|*		argv		     argument strings			      *|
|*									      *|
|* Output parameters:							      *|
|*		none							      *|
|*									      *|
|* Returns:	nothing							      *|
|*									      *|
\******************************************************************************/

static void init_compiler (int	 argc,
			   char *argv[])
{
   extern void	init_lex (void);
#ifndef KEY /* Bug 5089 */
   extern void	init_msg_processing (char *[]);
#endif /* KEY Bug 5089 */
   extern void	init_src_input (void);
   extern void	init_type (void);
#ifndef KEY /* Bug 5089 */
   extern void	process_cmd_line (int, char *[]);
#endif /* KEY Bug 5089 */
   extern void	init_cond_comp(void);
   extern void	enter_predefined_macros(void);
   extern void	init_parse_prog_unit(void);
   extern void	init_PDGCS (void);
   extern void	set_up_token_tables(void);
   extern void  sgi_cmd_line(int *argc, char **argv[]);
   extern char *operator_str[];
   extern void	verify_semantic_tbls(void);

   	  int	idx;


   TRACE (Func_Entry, "init_compiler", NULL);

   init_date_time_info ();		/* set compilation data and time      */
#ifdef KEY /* Bug 5089 */
   /* Initialize for message-printing; must precede process_cmd_line */
   char *nlspath = init_msg_processing (argv);
#else /* KEY Bug 5089 */
   init_msg_processing (argv);		/* initialize for messages.  Must     */
					/* preceed process_cmd_line.	      */
#endif /* KEY Bug 5089 */

# ifdef _DEBUG
   check_defines_compatibility();	/* Is the compiler built correctly?   */
   check_enums_for_change();	        /* Some enums must not be changed.    */
# endif


   /* allocate memory for data structures required across compilation units.  */
   /* These must preceed process_cmd_line.                                    */

   TBL_ALLOC (global_line_tbl);
   TBL_ALLOC (global_name_tbl);
   TBL_ALLOC (global_attr_tbl);
   TBL_ALLOC (global_type_tbl);
#ifdef KEY /* Bug 10177 */
   CLEAR_TBL_NTRY(global_type_tbl, 0);
#endif /* KEY Bug 10177 */
   TBL_ALLOC (global_bounds_tbl);
   TBL_ALLOC (global_ir_tbl);
   TBL_ALLOC (global_ir_list_tbl);
   TBL_ALLOC (global_sh_tbl);
   TBL_ALLOC (file_path_tbl);
   TBL_ALLOC (str_pool);

   init_release_level ();		/* Set up release_level from system   */

   str_pool[0].name_long	= 0;
   str_pool[1].name_long	= 0;
   str_pool[2].name_long	= LARGE_WORD_FOR_TBL_SRCH;
   str_pool_idx			= 2;

   TBL_REALLOC_CK(global_name_tbl, 2);
   CLEAR_TBL_NTRY(global_name_tbl, 1);
   CLEAR_TBL_NTRY(global_name_tbl, 2);
   GN_NAME_IDX(1)	= 1;
   GN_NAME_LEN(1)	= HOST_BYTES_PER_WORD;
   GN_NAME_IDX(2)	= 2;
   GN_NAME_LEN(2)	= HOST_BYTES_PER_WORD;

   /* Initialize the bounds table for deferred shape arrays */

   TBL_REALLOC_CK(global_bounds_tbl, 7);

   for (idx = BD_DEFERRED_1_IDX; idx <= BD_DEFERRED_7_IDX; idx++) {
      CLEAR_TBL_NTRY(global_bounds_tbl, idx);
      GB_ARRAY_CLASS(idx)	= Deferred_Shape;
      GB_RANK(idx)		= idx;
   }

   /* Initialize the conditional compilation tables.  It must be done before  */
   /* the command line processing because of the -D and -U options.           */

   init_cond_comp ();

   get_machine_chars();

   set_up_token_tables();

   /* The following routines sets things such as target_ieee, target_triton   */
   /* two_word_fcd, word_byte_size ect...                                     */

   set_compile_info_for_target();


   comp_phase = Cmdline_Parsing;

   cif_name[0] = NULL_CHAR;

   assembly_listing_file[0] = NULL_CHAR;

   debug_file_name[0] = NULL_CHAR;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   /* sgi_cmd_line does some option manipulation, process SGI specific        */
   /* command line options, and strips out things that the front-end doesn't  */
   /* need to see.                                                            */

   sgi_cmd_line (&argc,&argv);
# endif


#ifdef KEY /* Bug 5089 */
   process_cmd_line (argc, argv, nlspath);
#else /* KEY Bug 5089 */
   process_cmd_line (argc, argv);	/* pass input args		      */
#endif /* KEY Bug 5089 */


# if defined(_WHIRL_HOST64_TARGET64) && defined(TARG_X8664)
  /* OSP_467, #5, fixup the stride_multi_unit_in_bits array */
  if ( Is_Target_32bit() ) {
    /* compile 32-bit application with 64-bit compiler */
    fixup_unit_length_in_bits();
  }
# endif

# if defined(_INTEGER_1_AND_2)

   if (on_off_flags.integer_1_and_2) {
      bit_size_tbl[Integer_1] = 8;
      bit_size_tbl[Integer_2] = 16;
      bit_size_tbl[Logical_1] = 8;
      bit_size_tbl[Logical_2] = 16;

      storage_bit_size_tbl[Integer_1] = 8;
      storage_bit_size_tbl[Integer_2] = 16;
      storage_bit_size_tbl[Logical_1] = 8;
      storage_bit_size_tbl[Logical_2] = 16;

      storage_bit_prec_tbl[Integer_1] = 8;
      storage_bit_prec_tbl[Integer_2] = 16;
      storage_bit_prec_tbl[Logical_1] = 8;
      storage_bit_prec_tbl[Logical_2] = 16;

      stride_mult_unit_in_bits[Integer_1] = 8;
      stride_mult_unit_in_bits[Integer_2] = 16;
      stride_mult_unit_in_bits[Logical_1] = 8;
      stride_mult_unit_in_bits[Logical_2] = 16;

      linear_to_arith[Integer_1] = AR_Int_8_S;
      linear_to_arith[Integer_2] = AR_Int_16_S;

      input_arith_type[Integer_1] = AR_Int_8_U;
      input_arith_type[Integer_2] = AR_Int_16_U;

      strcpy(arith_type_string[Integer_1], "AR_Int_8_U");
      strcpy(arith_type_string[Integer_2], "AR_Int_16_U");
   }
# endif

   comp_phase = Pass1_Parsing;

   /* only -V info requested */

   if (argc == 2 && cmd_line_flags.verify_option) {
      print_id_line();
      exit_compiler(RC_OKAY);
   }

   if (num_errors != 0) {		/* command line errors	      */
      PRINTMSG(0, 912, Log_Summary, 0, num_errors);
      exit_compiler(RC_USER_ERROR);
   }

   /* Call init_cif even if the user did NOT request Compiler Information     */
   /* File (CIF) output because the CIF is used for messaging.		      */
   
   init_cif(comp_date_time, release_level);

   some_scp_in_err	= FALSE;
   clearing_blk_stk	= FALSE;

   init_type();
        
   make_table_changes ();

   init_sytb ();		 /* Must be before src_input for err msgs */


   /* Enter conditional compilation predefined macros.  This must happen      */
   /* after process_cmd_line because it calls GETPMC (and the information     */
   /* from GETPMC is needed to set the predefined macros that depend on the   */
   /* target machine).  This call must also happen after target_triton and    */
   /* target_ieee have been set so that we can get _CRAYIEEE set correctly.   */
   /* And finally, this call must come before init_src_input because that     */
   /* procedure gets the first source line - which could be a conditional     */
   /* compilation directive.					              */

   enter_predefined_macros();

   /* Must do the first call here so that tables needed by conditional        */
   /* compilation are set up.						      */

   init_parse_prog_unit();	

   init_src_input();

   if (on_off_flags.preprocess_only) {
      preprocess_only_driver();
      issue_deferred_msgs();

      TRACE (Func_Exit, "init_compiler", NULL);

      return;
   }

   init_lex ();

   max_field_len = (long) sbrk(0);	/* Keep track of memory usage         */

# if defined(_HOST_OS_MAX)
   max_field_len &= (1 << 32) - 1;
# endif


   /* Pathological case:  The file is empty.  At least an END statement must  */
   /* be present to constitute a valid Fortran program.                       */

   if (LA_CH_CLASS == Ch_Class_EOF) {
      PRINTMSG(0, 1391, Log_Warning, 0, src_file);
      issue_deferred_msgs();
   }


# ifdef _NAME_SUBSTITUTION_INLINING
   if (!dump_flags.preinline)
# endif
      init_PDGCS();

# ifdef _DEBUG
   verify_semantic_tbls();	/* Make sure flags and messages agree. */

   if (strcmp(operator_str[The_Last_Opr], "The_Last_Opr") != 0) {
      PRINTMSG(1, 689, Internal, 0);
   }
# endif

   TRACE (Func_Exit, "init_compiler", NULL);

   return;
 
}  /* init_compiler */


/******************************************************************************\
 * |*                                                                            *|
 * |* Description:                                                               *|
 * |*      fix up the unit length in bits, OSP_467                               *|
 * |*      if we compile 32-bit application with 64bit compiler,                 *|
 * |*      some unit length in bits is wrong because the default length is only  *|
 * |*      right for building 64-bit applications                                *|
 * |*                                                                            *|
 * |* Input parameters:                                                          *|
 * |*      NONE                                                                  *|
 * |*                                                                            *|
 * |* Output parameters:                                                         *|
 * |*      NONE                                                                  *|
 * |*                                                                            *|
 * |* Returns:                                                                   *|
 * |*      NOTHING                                                               *|
 * |*                                                                            *|
 * \******************************************************************************/
# if defined(_WHIRL_HOST64_TARGET64) && defined(TARG_X8664)
static void fixup_unit_length_in_bits()
{
   if ( Is_Target_32bit() ) {
      /* These constants should be fixed up to build 32-bit apps */
      stride_mult_unit_in_bits[Typeless_8]   = 32;
      stride_mult_unit_in_bits[Integer_8]    = 32;
      stride_mult_unit_in_bits[Real_8]       = 32;
      stride_mult_unit_in_bits[Real_16]      = 32;
      stride_mult_unit_in_bits[Complex_4]    = 32;
      stride_mult_unit_in_bits[Complex_8]    = 32;
      stride_mult_unit_in_bits[Complex_16]   = 32;
      stride_mult_unit_in_bits[CRI_Ptr_8]    = 32;
      stride_mult_unit_in_bits[Logical_8]    = 32;
      stride_mult_unit_in_bits[CRI_Ch_Ptr_8] = 32;
   }
}
# endif


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      Initialize the compiler date and time info;  comp_date_time has the   *|
|*      format "Ddd Mmm dd, yyyy  hh:mm:ss\0".				      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:	       							      *|
|*      NOTHING								      *|
|*									      *|
\******************************************************************************/
 
static void init_date_time_info (void)
 
{
   		time_t	 begin_time;
   static	char	*date_time_str; /*Ptr to-> Ddd Mmm dd hh:mm:ss yyyy\n */

				  
   TRACE (Func_Entry, "init_date_time_info", NULL);

   time (&begin_time);
   date_time_str = ctime (&begin_time);
 
   /* change date_time_str in format:  Ddd Mmm dd hh:mm:ss yyyy\n\0	      */
   /* to comp_date_time in format:     Ddd Mmm dd, yyyy	 hh:mm:ss\0	      */
   /* Note:  CIF processing also depends on this format for the contents of   */
   /*        comp_date_time.						      */
 
   memcpy (comp_date_time, date_time_str, 10);			/* Ddd Mmm dd */
   comp_date_time[10] = COMMA;
   memcpy (comp_date_time+11, date_time_str+19, 5);		/* yyyy	      */
   comp_date_time[16] = BLANK;
   memcpy (comp_date_time+17, date_time_str+10, 9);		/* hh:mm:ss   */
   comp_date_time[26] = EOS;

   TRACE (Func_Exit, "init_date_time_info", NULL);

   return;

}  /* init_date_time_info */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      Do license manager stuff for solaris and dpe compilers.               *|
|*									      *|
|*      FlexLM is license manager that controls access to shipped	      *|
|*      executables according to application specific rules.		      *|
|*									      *|
|*      cray_lm_checkout obtains a license using a predefined licensing	      *|
|*      policy based on the product code passed as the first parameter.	      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:	       							      *|
|*      NOTHING								      *|
|*									      *|
\******************************************************************************/


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      Check defines compatibility.                                          *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:	       							      *|
|*      NOTHING								      *|
|*									      *|
\******************************************************************************/
 
static void check_defines_compatibility(void)
 
{
				  
   TRACE (Func_Entry, "check_defines_compatibility", NULL);

  /* Make sure that both pairs of a defines are not set. */

# if defined(_MODULE_TO_DOT_o) && defined(_MODULE_TO_DOT_M)
   PRINTMSG(1, 1114, Internal, 0,
            "_MODULE_TO_DOT_o",
            "_MODULE_TO_DOT_M");
# endif

# if defined(_HEAP_REQUEST_IN_BYTES) && defined(_HEAP_REQUEST_IN_WORDS)
   PRINTMSG(1, 1114, Internal, 0,
            "_HEAP_REQUEST_IN_BYTES",
            "_HEAP_REQUEST_IN_WORDS");
# endif

# if defined(_HOST32) && defined(_HOST64)
   PRINTMSG(1, 1114, Internal, 0,
            "_HOST32",
            "_HOST64");
# endif

# if defined(_TARGET32) && defined(_TARGET64)
   PRINTMSG(1, 1114, Internal, 0,
            "_TARGET32",
            "_TARGET64");
# endif

# if defined(_TARGET_WORD_ADDRESS) && defined(_TARGET_BYTE_ADDRESS)
   PRINTMSG(1, 1114, Internal, 0,
            "_TARGET_WORD_ADDRESS",
            "_TARGET_BYTE_ADDRESS");
# endif


# if !defined(_HEAP_REQUEST_IN_BYTES) && !defined(_HEAP_REQUEST_IN_WORDS)
   PRINTMSG(1, 1116, Internal, 0,
            "_HEAP_REQUEST_IN_BYTES",
            "_HEAP_REQUEST_IN_WORDS");
# endif

# if !defined(_HOST32) && !defined(_HOST64)
   PRINTMSG(1, 1116, Internal, 0,
            "_HOST32",
            "_HOST64");
# endif

# if !defined(_TARGET32) && !defined(_TARGET64)
   PRINTMSG(1, 1116, Internal, 0,
            "_TARGET32",
            "_TARGET64");
# endif

# if !defined(_TARGET_WORD_ADDRESS) && !defined(_TARGET_BYTE_ADDRESS)
   PRINTMSG(1, 1116, Internal, 0,
            "_TARGET_WORD_ADDRESS",
            "_TARGET_BYTE_ADDRESS");
# endif

   TRACE (Func_Exit, "check_defines_compatibility", NULL);

   return;

}  /* check_defines_compatibility */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine fills in some tables according to target or command      *|
|*      line changes.                                                         *|
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

static void	make_table_changes(void)

{
   int		i;
   int		k;

   TRACE (Func_Entry, "make_table_changes", NULL);

# ifdef _ARITH_H
# if defined(_TARGET64)

   if (target_ieee) {
# if defined(_TARGET_OS_MAX) || defined(_WHIRL_HOST64_TARGET64)
      linear_to_arith[Real_4]     = AR_Float_IEEE_NR_32;
      linear_to_arith[Real_8]     = AR_Float_IEEE_NR_64;
      linear_to_arith[Real_16]    = AR_Float_IEEE_NR_128;
      linear_to_arith[Complex_4]  = AR_Complex_IEEE_NR_32;
      linear_to_arith[Complex_8]  = AR_Complex_IEEE_NR_64;
      linear_to_arith[Complex_16] = AR_Complex_IEEE_NR_128;

      input_arith_type[Real_4]     = AR_Float_IEEE_NR_32;
      input_arith_type[Real_8]     = AR_Float_IEEE_NR_64;
      input_arith_type[Real_16]    = AR_Float_IEEE_NR_128;
      input_arith_type[Complex_4]  = AR_Complex_IEEE_NR_32;
      input_arith_type[Complex_8]  = AR_Complex_IEEE_NR_64;
      input_arith_type[Complex_16] = AR_Complex_IEEE_NR_128;

      strcpy(arith_type_string[Real_4], "AR_Float_IEEE_NR_32");
      strcpy(arith_type_string[Real_8], "AR_Float_IEEE_NR_64");
      strcpy(arith_type_string[Real_16], "AR_Float_IEEE_NR_128");
      strcpy(arith_type_string[Complex_4], "AR_Complex_IEEE_NR_32");
      strcpy(arith_type_string[Complex_8], "AR_Complex_IEEE_NR_64");
      strcpy(arith_type_string[Complex_16], "AR_Complex_IEEE_NR_128");
# else
      linear_to_arith[Real_4]     = AR_Float_IEEE_NR_64;
      linear_to_arith[Real_8]     = AR_Float_IEEE_NR_64;
      linear_to_arith[Real_16]    = AR_Float_IEEE_NR_128;
      linear_to_arith[Complex_4]  = AR_Complex_IEEE_NR_64;
      linear_to_arith[Complex_8]  = AR_Complex_IEEE_NR_64;
      linear_to_arith[Complex_16] = AR_Complex_IEEE_NR_128;

      input_arith_type[Real_4]     = AR_Float_IEEE_NR_64;
      input_arith_type[Real_8]     = AR_Float_IEEE_NR_64;
      input_arith_type[Real_16]    = AR_Float_IEEE_NR_128;
      input_arith_type[Complex_4]  = AR_Complex_IEEE_NR_64;
      input_arith_type[Complex_8]  = AR_Complex_IEEE_NR_64;
      input_arith_type[Complex_16] = AR_Complex_IEEE_NR_128;

      strcpy(arith_type_string[Real_4], "AR_Float_IEEE_NR_64");
      strcpy(arith_type_string[Real_8], "AR_Float_IEEE_NR_64");
      strcpy(arith_type_string[Real_16], "AR_Float_IEEE_NR_128");
      strcpy(arith_type_string[Complex_4], "AR_Complex_IEEE_NR_64");
      strcpy(arith_type_string[Complex_8], "AR_Complex_IEEE_NR_64");
      strcpy(arith_type_string[Complex_16], "AR_Complex_IEEE_NR_128");
# endif
   }
# endif
# endif

   if (CG_LOGICAL_DEFAULT_TYPE != LOGICAL_DEFAULT_TYPE) {

      /* change the result types for the logical comparison tables */

      for (i = 0; i < Num_Linear_Types; i++ ) {
         for (k = 0; k < Num_Linear_Types; k++ ) {

            if (eq_ne_tbl[i][k].type == CG_LOGICAL_DEFAULT_TYPE) {
               eq_ne_tbl[i][k].type = LOGICAL_DEFAULT_TYPE;
            }

            if (lg_tbl[i][k].type == CG_LOGICAL_DEFAULT_TYPE) {
               lg_tbl[i][k].type = LOGICAL_DEFAULT_TYPE;
            }

            if (gt_lt_tbl[i][k].type == CG_LOGICAL_DEFAULT_TYPE) {
               gt_lt_tbl[i][k].type = LOGICAL_DEFAULT_TYPE;
            }
         }
      }
   }

#ifdef KEY /* Bug 5710 */
   /* eq_ne_on_logicals_tbl is same as eq_ne_tbl, but as an extension also
    * allows the operations .eq. and .ne. on logical operands, as standard does 
    * via and_or_tbl (which is used for .eqv. and .neqv.) It's not clear why
    * preceding code corrects LOGICAL_DEFAULT_TYPE for eq_ne_tbl but
    * not and_or_tbl: however, we echo whatever those tables do. */
   for (i = 0; i < Num_Linear_Types; i++ ) {
      for (k = 0; k < Num_Linear_Types; k++ ) {
	 int aot_result = and_or_tbl[i][k].type;
	 if (aot_result >= Logical_1 && aot_result <= Logical_8) {
	   eq_ne_on_logical_tbl[i][k] = and_or_tbl[i][k];
	 }
	 else {
	   eq_ne_on_logical_tbl[i][k] = eq_ne_tbl[i][k];
	 }
      }
   }
#endif /* KEY Bug 5710 */

   TRACE (Func_Exit, "make_table_changes", NULL);

   return;

}  /* make_table_changes */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	The following 3 routines are to be provided so that outside compiler  *|
|*      pieces may call them to see what they're linked with.                 *|
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

const	char	*fe_vers_name(void)
{
   /* This function returns a string containing a printable  */
   /* name of the Fortran 90 frontend component.             */

   return ("fe90");

}  /* fe_vers_name */


const	char	*fe_vers_ID(void)
{
   /* This function returns a string containing the single-letter */
   /* identifier for the Fortran 90 frontend component.           */

   return ("f");

}  /* fe_vers_ID */


const	char	*fe_vers_number(void)
{

   /* This function returns a string containing the version */
   /* number of the Fortran 90 frontend component.           */

  return (frontend_version);

}  /*fe_vers_number */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Print the version information line for -V, whether it is used alone   *|
|*      or with a full compilation.                                           *|
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

static	void	print_id_line(void)
{

   char		version_string[16]	= "######";


   TRACE (Func_Exit, "print_id_line", NULL);

   /* The Cray (PVP/MPP) compiler id line is:			              */
   /*								              */
   /*    Cray CF90 Version n.n.n (levels) curr-date curr-time                 */
   /*								              */
   /* The id line for other platforms may vary somewhat from this.            */

# if (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)) && !defined(_TARGET_SV2)
   sprintf(version_string, "%s%s", fe_vers_ID(), fe_vers_number());
# elif defined(_HOST_OS_SOLARIS)
   sprintf(version_string, "%s%s%s%s%s%s%s%s",
                          fe_vers_ID(), fe_vers_number(),
                          "p", "XX",
                          "g", "XXX",
                          arith_vers_ID(), arith_vers_number());
# else
   sprintf(version_string, "%s%s%s%s%s%s%s%s",
                          fe_vers_ID(), fe_vers_number(),
                          opt_vers_ID(), opt_vers_number(),
                          be_vers_ID(), be_vers_number(),
                          arith_vers_ID(), arith_vers_number());
# endif


# if (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN)) && (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

   PRINTMSG (0, 1402, Log_Summary, 0,
             release_level,
             version_string,
             comp_date_time);

# else

   PRINTMSG (0, 103, Log_Summary, 0,
             release_level,
             version_string,
             comp_date_time);

# endif


   TRACE (Func_Exit, "print_id_line", NULL);

   return;

}  /* print_id_line */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Get_machine_chars is called to set the machine characteristics table  *|
|*	for the compiler from the environment variable TARGET.		      *|
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

static void get_machine_chars (void)

{
# if defined(_TARGET_OS_UNICOS) || defined(_TARGET_OS_MAX)

# if defined(_GETPMC_AVAILABLE)
   extern	 int	 GETPMC(long *, char *);   /* UNICOS library routine */
# else
   int		 idx;
   char		*name;
# endif


   TRACE (Func_Entry, "get_machine_chars", NULL);

# if defined(_GETPMC_AVAILABLE)

   /* Use target_machine to get information about the host machine.     */
   /* This information is used by ntr_const_tbl to choose the algorithm */
   /* it uses to convert and store floating point constants.            */

   if (GETPMC (target_machine.mc_tbl, "host") == 0) {
      PRINTMSG (0, 584, Log_Error, 0, "GETPMC");
   }

   host_ieee = target_machine.fld.mcieee;

   /* Set machine characteristics table based on the target environment.   */
   /* The target environment is either the machine the compiler is running */
   /* on or the machine specified by the TARGET environment variable.	   */

   if (GETPMC (target_machine.mc_tbl, "target") == 0) {
      PRINTMSG (0, 584, Log_Error, 0, "GETPMC");
   }

# else

   name = getenv("TARGET");

   if (name == NULL) {
      PRINTMSG(0, 1052, Log_Error, 0);
      TRACE (Func_Exit, "get_machine_chars", NULL);
      exit_compiler(RC_USER_ERROR);
   }
   else {
      strcpy(target_machine.fld.mcpmt, name);

      /* GETPMC translates the target machine name to upper case.   */

      for (idx = 0;  idx <= strlen(target_machine.fld.mcpmt);  ++idx) {
         target_machine.fld.mcpmt[idx] = toupper(target_machine.fld.mcpmt[idx]);
      }
   }

# endif

   TRACE (Func_Exit, "get_machine_chars", NULL);
  
# endif
   return;

}  /* get_machine_chars */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	set_compile_info_for_target sets flags used for compilation based on *|
|*	the target being compiled for.                          	      *|
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

static void set_compile_info_for_target (void)

{


# if defined(_TARGET_OS_UNICOS) || defined(_TARGET_OS_MAX)
   union        {long   int_form;
                 char   char_form[9];
                } cpu_type;
# endif
 
   TRACE (Func_Entry, "set_compile_info_for_target", NULL);

# if defined(_TARGET_SV2)
   target_os = Target_Sv2;
# elif defined(_TARGET_OS_UNICOS)
   target_os = Target_Unicos;
# elif defined(_TARGET_OS_LINUX)
   target_os = Target_Linux;
# elif defined(_TARGET_OS_DARWIN)
   target_os = Target_Darwin;
# elif defined(_TARGET_OS_MAX)
   target_os = Target_Max;
# elif (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX))
   target_os = Target_Irix;
# elif defined(_TARGET_OS_SOLARIS)
   target_os = Target_Solaris;
# endif


# if defined(_TWO_WORD_FCD)
   two_word_fcd		= TRUE;
# else
   two_word_fcd		= FALSE;
# endif

# if defined(_CHAR_LEN_IN_BYTES)
   char_len_in_bytes	= TRUE;
# else
   char_len_in_bytes	= FALSE;
# endif

# ifdef _TARGET_OS_UNICOS

# ifdef _GETPMC_AVAILABLE
   cpu_type.int_form     = target_machine.fld.mcpmt;
   cpu_type.char_form[8] = '\0';

   if (strcmp("CRAY-TS", cpu_type.char_form) == 0) {
      two_word_fcd				= TRUE;
      target_safevl				= 128;
      storage_bit_size_tbl[CRI_Ch_Ptr_8]	= 128;
      target_triton				= TRUE;
   }
   else if (strcmp("CRAY-C90", cpu_type.char_form) == 0) {
      target_safevl = 128;
   }
   else if (strcmp("CRAY-SV1", cpu_type.char_form) == 0) {
      target_sv1 = TRUE;
   }
   else if (strcmp("CRAY-YMP", cpu_type.char_form) == 0) {
      cpu_type.int_form     = target_machine.fld.mc_subtype1;
      cpu_type.char_form[8] = '\0';
      
      if (strcmp("CRAY-SV1", cpu_type.char_form) == 0) {
         target_sv1 = TRUE;
      }
   }

# else

   if (strcmp("CRAY-TS", target_machine.fld.mcpmt) == 0) {
      two_word_fcd                              = TRUE;
      target_safevl                             = 128;
      storage_bit_size_tbl[CRI_Ch_Ptr_8]        = 128;
      target_triton				= TRUE;
   }
   else if (strcmp("CRAY-C90", target_machine.fld.mcpmt) == 0) {
      target_safevl = 128;
   }
# if !defined(_TARGET_SV2)
   else if (strcmp("CRAY-SV1", target_machine.fld.mcpmt) == 0) {
      target_sv1 = TRUE;
   }
   else if ((strcmp("CRAY-YMP", target_machine.fld.mcpmt) == 0) &&
            (strcmp("CRAY-SV1", target_machine.fld.mc_subtype1) == 0)) {
         target_sv1 = TRUE;
   }
# endif

# endif

# endif


# ifdef _TARGET_OS_MAX

# if defined(_GETPMC_AVAILABLE)
   cpu_type.int_form     = target_machine.fld.mcpmt;
   cpu_type.char_form[8] = '\0';

   if (strcmp("CRAY-T3E", cpu_type.char_form) == 0) {
      char_len_in_bytes = TRUE;
      target_t3e	= TRUE;
   }
# else
   /* Assume this is a DPE compiler going from SOLARIS to Cray.      */

   if (strcmp("CRAY-T3E", target_machine.fld.mcpmt) == 0) {
      char_len_in_bytes = TRUE;
      target_t3e	= TRUE;
   }
# endif

# endif


# if defined(_TARGET_IEEE)
   target_ieee = TRUE;
# else
   target_ieee = target_triton && target_machine.fld.mcieee;
# endif

# if defined(_TARGET_SV2)
   true_value = 1;	                  /* TRUE_VALUE */
# else
   if (target_ieee && target_triton) {
      true_value = 1;		          /* TRUE_VALUE */
   }
# endif

   /* Set maximum character length based on machine type.		      */

# if defined(_TARGET_OS_UNICOS)

   if (target_triton) {
      max_character_length	= 2147483647;		/* (2**31) - 1 chars */
   }
   else {
      max_character_length	= 2097151;		/* In byte size */
   }

# elif defined(_TARGET_OS_MAX)

   if (target_t3e) {

      /* Set based on the maximum storage size from machine target info */

      max_character_length	= 134217727;		/* (2**27) - 1 chars */
   }
   else {
      max_character_length	= 2097151;		/* In byte size */
   }

# elif (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

   max_character_length		= 268435455;		/* (2**28) -1 chars */

# else

   max_character_length		= 268435455;		/* (2**28) -1 chars */

# endif


   TRACE (Func_Exit, "set_compile_info_for_target", NULL);
  
   return;
 
}  /* set_compile_info_for_target */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	init_release_level gets the compilers release string from a system   *|
|*	file.  It sets the global variable release_string.      	      *|
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

static void init_release_level (void)

{
   char		*char_ptr;
   int		 length;
   char		*location;
   char		 new_release[RELEASE_LEVEL_LEN];
   int		 str_idx;
   FILE		*release_file_ptr;
   char		*version_string_location	= "COMPILER";


   TRACE (Func_Entry, "init_release_level", NULL);

   location = getenv(version_string_location);

   if (location != NULL) {
      length	= WORD_LEN(strlen(location) + 14);
      str_idx	= str_pool_idx;

      TBL_REALLOC_CK(str_pool, length);

      strcpy((char *)&str_pool[str_idx], location);

      char_ptr	= strrchr((char *)&str_pool[str_idx], SLASH);

      if (char_ptr == NULL) {
         release_file_ptr = fopen("version.string", "r");
      }
      else {
         strcpy(++char_ptr, "version.string");
         release_file_ptr = fopen((char *)&str_pool[str_idx], "r");
      }

      /* If not found - default to initial value in release_level #.x.x.x */

      if (release_file_ptr != NULL) {
         fgets(new_release, RELEASE_LEVEL_LEN, release_file_ptr);

         if (new_release != NULL) {
            char_ptr	= strrchr(new_release, NEWLINE);
            *char_ptr	= EOS;
            strcpy(release_level, new_release);
         }
      }

      str_pool_idx	= str_idx;
   }

   TRACE (Func_Exit, "init_release_level", NULL);
  
   return;
 
}  /* init_release_level */

# if defined(_DEBUG)
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      Check to make sure that the opr enum has only been changed at the     *|
|*      bottom.  This is not a failsafe check, but it is designed to make     *|
|*      sure that nothing gets added or deleted at least.                     *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:	       							      *|
|*      NOTHING								      *|
|*									      *|
\******************************************************************************/
static void check_enums_for_change(void)
{
				  
   TRACE (Func_Entry, "check_enums_for_change", NULL);

   if (Null_Opr != 0 ||
       Defined_Un_Opr != 1 ||
       Alloc_Opr != 2 ||
       Eqv_Opr != 25 ||
       Nint_Opr != 50 ||
       Char_Opr != 75 ||
       Rrspacing_Opr != 100 ||
       Minval_Opr != 125 ||
       Stop_Opr != 150 ||
       Dv_Set_A_Contig != 175 ||
       Aloc_Opr != 200 ||
       Init_Reloc_Opr != 225 ||
       Prefertask_Cdir_Opr != 250 ||
       Wait_Cmic_Opr != 275 ||
       Set_Ieee_Exception_Opr != 300 ||
       Local_Pe_Dim_Opr != 325 ||
       Fissionable_Star_Opr != 350 ||
       End_Singleprocess_Par_Opr != 375 ||
       Fetch_And_Nand_Opr != 400 ||
       Endparallel_Open_Mp_Opr != 425 ||
       Omp_In_Parallel_Opr != 454 ||
       Io_Item_Type_Code_Opr != 479 ||
       Copyin_Bound_Opr != 484) { /* modified by jhs, 02.8.31 */

      PRINTMSG(1, 1643, Internal, 0, "Operator");
   }

   TRACE (Func_Exit, "check_enums_for_change", NULL);

   return;

}  /* check_enums_for_change */
# endif

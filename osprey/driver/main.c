/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007 PathScale, LLC.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


static char *rcs_id = "$Source: driver/SCCS/s.main.c $ $Revision: 1.109 $";

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <stamp.h>
#include <cmplrs/rcodes.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include "pathscale_defs.h"
#include "string_utils.h"
#include "options.h"
#include "option_seen.h"
#include "option_names.h"
#include "opt_actions.h"
#include "get_options.h"
#include "lang_defs.h"
#include "errors.h"
#include "phases.h"
#include "file_utils.h"
#include "file_names.h"
#include "run.h"
#include "objects.h"
#include "version.h"

char *help_pattern;
boolean debug;
boolean nostdinc = TRUE;
int show_version;
boolean show_copyright;
boolean dump_version;
boolean show_search_path;
boolean show_defaults;
boolean print_help = FALSE;

HUGEPAGE_DESC hugepage_desc;
boolean add_heap_limit;
int heap_limit;
int hugepage_attr;


extern void toggle_implicits(void);
extern void set_defaults(void);
extern void add_special_options (void);

static void check_old_CC_options (char *name);
static void check_makedepend_flags (void);
static void mark_used (void);
static void dump_args (char *msg);
static void print_help_msg (void);
static void print_defaults (int argc, char *argv[]);
static void append_default_options (int *argc, char *(*argv[]));
#ifdef PSC_TO_OPEN64
static void append_open64_env_flags (int *argc, char *(*argv[]), char *env_var);
#endif
static void print_search_path (void);
static void display_version (boolean dump_version_only);

static string_list_t *files;
static string_list_t *file_suffixes;
string_list_t *feedback_files;

boolean parsing_default_options = FALSE;
boolean drop_option;

#ifdef TARG_X8664
boolean link_with_mathlib;
#endif

const char compiler_version[] = INCLUDE_STAMP;
static void set_executable_dir (void);

static void prescan_options (int argc, char *argv[]);

static void no_args(void)
{
	fprintf(stderr, "%s: no input files\n", program_name);
	fprintf(stderr, "For general help: %s --help\n", program_name);
	fprintf(stderr, "To search help: %s -help:<string>\n", program_name);
	do_exit(RC_USER_ERROR);
}

int 
main (int argc, char *argv[])
{
	int i;		/* index to argv */
	int flag;
	int base_flag;
	string_item_t *p, *q;
	int num_files = 0;
	char *unrecognized_dashdash_option_name = NULL;
        hugepage_desc = NULL;

	init_error_list();
	program_name = drop_path(argv[0]);	/* don't print path */
	orig_program_name = string_copy(argv[0]);
        file_utils_set_program_name(orig_program_name);

	/* Add the contents of OPEN64_GENFLAGS to the command line */
	#ifdef PSC_TO_OPEN64
	append_open64_env_flags(&argc, &argv, "OPEN64_GENFLAGS");
	#endif

	// Append the default options in compiler.defaults to argv so that they
	// are parsed along with the command line options.
	append_default_options(&argc, &argv);

	save_command_line(argc, argv);		/* for prelinker    */	
	files = init_string_list();
	file_suffixes = init_string_list();
	feedback_files = init_string_list ();	/* for cord feedback files */
	init_options();
	init_temp_files();
	init_crash_reporting();
	init_count_files();
	init_option_seen();
	init_objects();

	invoked_lang = get_named_language(program_name);

	/* Add the contents of OPEN64_CFLAGS, OPEN64_CXXFLAGS, OPEN64_FFLAGS to the
	   command line.  Bug 7646. */
	#ifdef PSC_TO_OPEN64
	if (invoked_lang == L_cc)
	  append_open64_env_flags(&argc, &argv, "OPEN64_CFLAGS");
	else if (invoked_lang == L_CC)
	  append_open64_env_flags(&argc, &argv, "OPEN64_CXXFLAGS");
	else if (invoked_lang == L_f77 || invoked_lang == L_f90)
	  append_open64_env_flags(&argc, &argv, "OPEN64_FFLAGS");
	#endif

	check_for_driver_controls (argc, argv);

	/* Try to find where the compiler is located and set the phase
	   and library directories appropriately. */
#if !defined(TARG_SL)
	set_executable_dir();
#endif
	// "-o -" will set this to TRUE.
	dump_outfile_to_stdout = FALSE;

	// Change the phase names based on run-time info.
	init_phase_names();

	init_phase_info();	/* can't add toolroot until other prefixes */

        /* Hack for F90 ftpp; For pre processing F90 calls ftpp;
         * Unlike cpp, ftpp does not like the -Amachine(mips) and -Asystem(unix)
         * that are passed to cpp; we need to remove these if ftpp is used for
         * preprocessing. Also remove all the woff options for cpp and ftpp.
         */

	remove_phase_for_option(O_A,P_f90_cpp);
	remove_phase_for_option(O_E,P_f90_cpp);

	// First check for the existence of certain options anywhere in the
	// command line, because these options affect the behavior of other
	// actions.
	prescan_options(argc, argv);

	i = 1;
	while (i < argc) {
		option_name = argv[i];

		// See if we are parsing the compiler default options.  Ignore
		// a default option if it conflicts with any command line
		// option.
		if (!strcmp(option_name, "-default_options")) {
		  parsing_default_options = TRUE;
		  i++;
		  continue;
		}

		set_current_arg_pos(i);
		if (argv[i][0] == '-' && !dashdash_flag) {
			flag = get_option(&i, argv);
			if (flag == O_Unrecognized) { 
				if (print_warnings) {
				    // For unrecognized dashdash options, delay
				    // giving error message until we know
				    // pathcc is not called as a linker.  If
				    // pathcc is called as a linker, silently
				    // ignore all dashdash options, including
				    // unrecognized ones, in order to mimick
				    // gcc behavior.  Bug 4736.
				    if (!strncmp(option_name, "--", 2)) {
				      unrecognized_dashdash_option_name =
				        option_name;
				    }
				    else {
				      /* warn about unknown single-dash options */
				      warning("unknown flag: %s", option_name);
				    }
				}
			}
			else {
				/* reset option name to possibly include 
				 * 2nd part, e.g. -G 8 */
				option_name = get_option_name(flag);
			}
			/* sometimes is simple alias to another flag */
			flag = get_real_option_if_aliased (flag);

			/* sometimes need to look at parent flag */
			if (is_derived_option (flag)) {
				base_flag = get_derived_parent(flag);
				/* sometimes base is simple alias */
				base_flag = get_real_option_if_aliased (base_flag);
			}
			else {
				base_flag = flag;
			}

			drop_option = FALSE;

			// Drop IPA options if IPA was turned off due to
			// options conflict.  Bug 11571.
			if (ipa == FALSE &&
			    base_flag == O_IPA_) {
			  drop_option = TRUE;
			} else
			// Sets drop_option to TRUE if option should be ignored.
			opt_action(base_flag);

			// Add options that are potentially linker options, in
			// case pathcc is called as a linker.
			if (!drop_option) {
			  if (is_maybe_linker_option(base_flag)) {
			    add_maybe_linker_option(base_flag);
			  } else if (is_object_option(base_flag)) {
			    /* put in separate object list */
			    add_object (base_flag, optargs);
			    source_kind = S_o;
			  } else {
			    /* add unique real flag to list */
			    add_option_seen (flag);
			  }
			}
		} else if (argv[i][0] == '+') {
			check_old_CC_options(argv[i]);
			i++;
		} else {
			source_kind = get_source_kind(argv[i]);
			/* if -E used, then preprocess anything, even .o's */
			if (last_phase == P_any_cpp && source_kind == S_o) 
			{
				source_kind = S_c;
			}
			if (source_kind == S_o) {
				/* object file or library */
				add_object (O_object, argv[i]);
				/* Save away objects should it be necessary
				   to invoke the archive phase (-ar option). */
				    add_ar_objects(argv[i]);
			} else {
				/*
				 * Reserve place in object list for .o,
				 * so multiple .o's have same position
				 * relative to other parameters.
				 * If only one source file, then will
				 * treat this as a temp file.
				 */
				char *obj_name = get_object_file(argv[i]);
				add_object (O_object, obj_name);
				add_ar_objects(obj_name);
				add_string(files, argv[i]);
				/* Because of -x <lang> option,
				 * we need position as well as name to
				 * determine a file's source kind.
				 * So we want to record the source_kind now,
				 * before we lose the position info.
				 * Thus have parallel list of file suffixes,
				 * which will be the known suffix that we
				 * want to treat this file as, e.g.
				 * -x assembler t.asm will give suffix "s".
				 * Use string_list just cause it is readily
				 * available. */
				add_string(file_suffixes, 
					get_suffix_string(source_kind));
				num_files++;
			}
			cancel_saved_arg(1);
			i++;
		}
	}

	// By now we know if pathcc is called as a linker.  If so, turned all
	// the potential linker options into real linker options; otherwise
	// delete them.
	finalize_maybe_linker_options (num_files == 0);

	// If pathcc is not called as a linker, complain about unrecognized
	// dashdash options.  Bug 4736.
	if (num_files > 0 &&
	    unrecognized_dashdash_option_name != NULL) {
	  parse_error(unrecognized_dashdash_option_name, "unknown flag");
	}

	/* Check target specifications for consistency: */
	Check_Target ();

#if defined(TARG_NVISA)
	if (show_version) {
	    /* Echo information about the compiler version */
	    fprintf(stderr, "NVIDIA (R) CUDA Open64 Compiler\n");
	    fprintf(stderr, "%s\n", cset_id);
	    fprintf(stderr, "Built on %s\n", build_date);
	}
	if (show_copyright) {
	    fprintf(stderr, "Portions Copyright (c) 2005-2008 NVIDIA Corporation\n");
	    fprintf(stderr, "Portions Copyright (c) 2002-2005 PathScale, Inc.\n");
	    fprintf(stderr, "Portions Copyright (c) 2000-2001 Silicon Graphics, Inc.\n");
	    fprintf(stderr, "All Rights Reserved.\n");
	}
#endif //TARG_NVISA
	if (show_search_path) {
		print_search_path();
	}
	if (show_defaults) {
	  print_defaults(argc, argv);
	}

	if (argc == 1)
	{
		no_args();
	}
	
	if (print_help || help_pattern != NULL)
	{
		print_help_msg();
	}
	if ( ! show_version && ! execute_flag && ! show_flag && ! v_flag && ! dump_version) {
		do_exit(RC_OKAY);	/* just exit */
	}
	if (source_kind == S_NONE || read_stdin) {
		if (read_stdin) {
			source_file = "-";
			if (option_was_seen(O_E)) {
				// Lang might already be set with "-x lang".
				// Bug 4179.
				if (source_lang == L_NONE)
				source_lang = L_cpp;
			} else {
				char *obj_name;
				source_kind = get_source_kind(source_file);
				if (source_lang == L_NONE)
				    source_lang = invoked_lang;
				obj_name = get_object_file(
				    fix_name_by_lang(source_file));
				add_object (O_object, obj_name);
			}
		}
		else if (show_version || dump_version) {
			// do nothing
		}
		else {
			no_args();
		}
	}
	/* if toggle flags have superceded previous flags,
	 * or if option has been repeated,
	 * unmark the previous flags:
	 */
	FOREACH_OPTION_SEEN(i) {
		if (current_option_seen_later(i)) {
			set_current_option_unseen();
		} else if (flag_is_superceded(i)) {
			set_option_unseen(i);
		}
	}


	if ((option_was_seen(O_fpic) ||
	     option_was_seen(O_fPIC))
	     && mem_model == M_MEDIUM) {
	  error("unimplemented: code model medium not supported in PIC mode");
	}
	// bug 10620
	if (mem_model != M_SMALL &&
	    abi == ABI_N32) {
	  error("code model \"%s\" not support in 32-bit mode", mem_model_name);
	}
	// bug 9066
	if (option_was_seen(O_static) &&
	    mem_model != M_SMALL) {
	  error("-static can only be used with -mcmodel=small");
	}
	// bug 10031
	if (option_was_seen(O_static_data) &&
	    mpkind == NORMAL_MP) {
	  warning("-static-data specified with -mp can cause incorrect "
		  "execution if a routine with static data is called from a "
		  "parallel region.");
	}

	if (debug) {
		dump_args("user flags");
	}
	if (ipa == TRUE)
	    save_ipl_commands ();

	if (outfile != NULL && (strcmp(outfile, "-") == 0)) {
	  // Use suffix "x" just because it's not used.
	  outfile = create_temp_file_name("x");
	  dump_outfile_to_stdout = TRUE;
	}

	/* if user has specified feedback files, turn on cord */
	if (feedback_files->head) cordflag=TRUE;

	/* if cord is not on, set last phase to ld */
	if (cordflag!=TRUE) {
	     last_phase=earliest_phase(P_any_ld, last_phase);
         }

	/* mark the used options */
	mark_used();

	/* call toggle routine for implicitly-used options */
	toggle_implicits();

	/* add defaults if not already set */
	set_defaults();

	// Perform GNU4-related checks after set_defaults has run, since
	// set_defaults can change the gnu version.  Bug 10250.
	if (gnu_major_version == 4) {
	  if (option_was_seen(O_fwritable_strings) ||
	      option_was_seen(O_fno_writable_strings)) {
	    warning("ignored -fwritable-strings/-fno-writable-strings because"
		    " option not supported under GNU GCC 4");
	    set_option_unseen(O_fwritable_strings);
	    set_option_unseen(O_fno_writable_strings);
	  }
#ifndef TARG_SL
	  if ((source_lang == L_cc ||
	       source_lang == L_CC) &&
	      option_was_seen(O_mp) &&	// bug 11896
	      gnu_minor_version < 2) {
	    warning("ignored -mp because option not supported under"
		    " GNU GCC 4.0");
	    set_option_unseen(O_mp);
	  }
	  else if (gnu_minor_version >= 2 &&
	           !option_was_seen(O_fno_cxx_openmp)) {
	    add_option_seen(O_fcxx_openmp);
            toggle(&fcxx_openmp,1);
	  }
#endif
	}

	// Select the appropriate GNU version front-end.
	init_frontend_phase_names(gnu_major_version, gnu_minor_version);

	// Display version after running set_defaults, which can change
	// gnu_major_version.
        if (show_version || dump_version) {
	  display_version(dump_version);
	  if (!execute_flag ||
	      source_kind == S_NONE)
	    do_exit(RC_OKAY);
	}

	if (num_files > 1) {
		multiple_source_files = TRUE;
	}
	/* handle anything else */
	check_makedepend_flags ();
	add_library_options();
	add_special_options();

	if (debug) {
		dump_args("with defaults");
		dump_objects();
	}
	if (has_errors()) return error_status;

	catch_signals();
	remember_last_phase = last_phase;

/* for DRA (Distributed Reshape Array) templitization, we want to
 * run prelinker and ld later
 * ??? why not just have ar and dsm prelink be set in determine_phase_order?
 */
	if ((multiple_source_files || 
	     option_was_seen(O_ar)) && 
	     ((last_phase == P_any_ld) && (shared != RELOCATABLE)) || 
	     (last_phase == P_pixie)) {
		/* compile all files to object files, do ld later */
		last_phase = P_any_as;
		add_minus_c_option();	/* for .ii file */
	}

	if (Use_feedback) {
	   struct stat stat_buf;
	   time_t fb_file_mod_time;
	   time_t count_file_mod_time;
	   boolean fb_file_exists = TRUE;

	   if (fb_cdir != NULL) 
	      warning ("-fb_cdir cannot be used with -fbuse; -fb_cdir ignored");
	   save_name(&fb_file, concat_strings(drop_path(prof_file), ".x.cfb"));
	   if (!(stat(fb_file, &stat_buf) != 0 && errno == ENOENT))
		fb_file_mod_time = stat_buf.st_mtime;
           else
		fb_file_exists = FALSE;
           if (!(stat(count_files->head->name, &stat_buf) != 0 && errno == ENOENT))
		count_file_mod_time = stat_buf.st_mtime;
           else {
		internal_error("%s doesn't exist", count_files->head->name);
		perror(program_name);
           }

	   if (!fb_file_exists || (fb_file_mod_time <= count_file_mod_time))
	       run_prof();
        }

        add_heap_limit = FALSE;
        heap_limit = HUGEPAGE_LIMIT_DEFAULT;
        hugepage_attr = 0;

        if (option_was_seen(O_HP)
            && (instrumentation_invoked != TRUE)) {
            HUGEPAGE_DESC desc;
            boolean do_heap = FALSE;
            boolean BD_or_BDT_with_1GBP = FALSE; /* BD or BDT mapping with 1GB pages */
            
            for (desc = hugepage_desc; desc != NULL; desc = desc->next) {
                if (desc->alloc == ALLOC_HEAP) {
                    do_heap = TRUE;
                    heap_limit = desc->limit;

                    if (desc->size == SIZE_2M)
                        hugepage_attr |= ( 1 << HEAP_2M_BIT);
                    else if (desc->size == SIZE_1G)
                        hugepage_attr |= ( 1 << HEAP_1G_BIT);
                }
                else if (desc->size == SIZE_1G
                         && (desc->alloc == ALLOC_BD
                             || desc->alloc == ALLOC_BDT)) {
                    BD_or_BDT_with_1GBP = TRUE;
                }
                if (option_was_seen(O_pg) &&
                    (desc->alloc == ALLOC_BD || desc->alloc == ALLOC_BDT)) {
                  /* See bug 744, libhugetlbfs doesn't detect that the
                   * allocation of data structures by the profiling library
                   * code, which causes a program fault after the data section
                   * is copied and remapped using huge pages.
                   */
                  error("-pg and -HP:bdt or -HP:bd are currently incompatible");
                }
            }

            /* With BD or BDT mappings using 1GB pages, the rest
             * of the 1GB page used to map data and BSS will be
             * automatically be used for heap, thus a heap page limit
             * of 1 is assumed by default.  However to ensure, that any
             * incompatible mallopt call that could be performed by
             * the subroutine call that is generated by specifying
             * the -OPT:malloc_alg option, we always add
             * -OPT:hugepage_heap_limit when invoking the back end if
             * BD or BDT mappings use 1GB pages.  This ensures that
             * the heap allocation initialization routine will be
             * called to revert any incorrect mallopt setting that
             * will occcur when -OPT:malloc_alg is supplied.
             */
            if (BD_or_BDT_with_1GBP == TRUE && do_heap == FALSE) {
                do_heap = TRUE;
                hugepage_attr |= ( 1 << HEAP_1G_BIT);
                heap_limit = 1;
            }
            if (do_heap == FALSE)
                heap_limit = 0;
            else
                add_heap_limit = TRUE;
        }
        
	if (read_stdin) {
		if ( option_was_seen(O_E) 
			|| (source_lang != L_NONE && source_kind != S_o)) 
		{
		        run_inline = UNDEFINED;
			run_compiler(argc, argv);
		}
		else {
			error("-E or specified language required when input is from standard input");
		}
		cleanup();
		return error_status;
	}

	for (p = files->head, q=file_suffixes->head; p != NULL; p = p->next, q=q->next) 
	{
            source_file = p->name;
		run_inline = UNDEFINED;	// bug 11325

		if (show_flag == TRUE)	// bug 9096
		if (multiple_source_files) {
			fprintf(stderr, "%s:\n", source_file);
		}
		if (execute_flag && !file_exists(source_file)) {
			error("file does not exist:  %s", source_file);
			continue;
		}
		source_kind = get_source_kind_from_suffix(q->name);
		source_lang = get_source_lang(source_kind);
		if (source_lang != invoked_lang
			&& source_lang != L_as
			&& (fullwarn || (source_lang == L_f90)) )
		{
			warning("compiler not invoked with language of source file; will compile with %s but link with %s", get_lang_name(source_lang), get_lang_name(invoked_lang));
		}
		run_compiler(argc, argv);
		if (multiple_source_files) cleanup();
	}
	if (has_errors()) {
		cleanup();
		cleanup_temp_objects();
		return error_status;
	}

	if (num_files == 0 || remember_last_phase != last_phase) {
		/* run ld */
		last_phase = remember_last_phase;
		source_file = NULL;
		source_kind = S_o;
		source_lang = get_source_lang(source_kind);

		if (option_was_seen(O_ar)) {
		   run_ar();
		}
		else {
		    run_ld ();
		}
		if (Gen_feedback)
		  run_pixie();
	}
	if (dump_outfile_to_stdout == TRUE)
	  dump_file_to_stdout(outfile);
	cleanup();
	cleanup_temp_objects();
	return error_status;
}

static void set_executable_dir (void) {
  char *dir;
  size_t dirlen;
  char *ldir;

  /* Try to find where the compiler is located in the
     filesystem, and relocate the phase and library
     directories based on where the executable is found. */
  /* Note that if have a symbolic link to driver,
   * it may operate on directory containing driver not cc. */
  dir = get_executable_dir ();
  if (dir == NULL) return;	

  /* If installed in a bin directory; get phases and stuff from
   * a peer directory.
   *
   * NOTE: This is *not* the only place where file paths are defined.
   * If you change the location of a phase (such as the linker) here,
   * you must look over in the ipa/common directory for paths to
   * change when the compiler is invoked with -ipa.
   *
   * In addition, you may also need to modify other search paths here
   * in driver-land.
   */
  ldir = drop_path (dir);
  if (strcmp (ldir, "bin") == 0) {
    char *basedir = directory_path (dir);
#if defined(TARG_NVISA)
    substitute_phase_dirs ("/lib", basedir, "/lib/");
#endif
#ifdef PSC_TO_OPEN64
    substitute_phase_dirs ("/lib", basedir, "/lib/" OPEN64_FULL_VERSION);
    substitute_phase_dirs ("/lib/" OPEN64_NAME_PREFIX "cc-lib",
			   basedir, "/lib/" OPEN64_FULL_VERSION);
#endif
    substitute_phase_dirs ("/include", basedir, "/include");
    return;
  }

  /* If installed in x/lib/gcc-lib/ */
  ldir = strstr (dir, "/lib/gcc-lib");
  if (ldir != 0) {
    if (ldir[12] == '/') {
      /* In target/version subdirectory. */
      ldir = substring_copy (dir, 0, ldir+4-dir);
      substitute_phase_dirs ("/bin", dir, "");
      substitute_phase_dirs ("/lib", ldir, "");
#ifdef PSC_TO_OPEN64
      substitute_phase_dirs ("/lib/" OPEN64_NAME_PREFIX "cc-lib", dir, "");
#endif
      substitute_phase_dirs ("/include", dir, "/include");
    } else if (ldir[12] == '\0') {
      /* directly in gcc-lib */
      ldir = substring_copy (dir, 0, ldir+4-dir);
      substitute_phase_dirs ("/bin", dir, "");
      substitute_phase_dirs ("/lib", ldir, "");
#ifdef PSC_TO_OPEN64
      substitute_phase_dirs ("/lib/" OPEN64_NAME_PREFIX "cc-lib", dir, "");
#endif
      substitute_phase_dirs ("/include", dir, "/include");
    }
    return;
  }
}

static void
check_old_CC_options (char *name)
{
	if (strcmp(name, "+I") == 0) {
		warn_no_longer_supported2(name, "-keep");
	} else if (strcmp(name, "+L") == 0) {
		warn_no_longer_supported(name);
	} else if (strcmp(name, "+d") == 0) {
		warn_no_longer_supported2(name, "-INLINE:none");
	} else if (strcmp(name, "+p") == 0  ||
	           strcmp(name, "+pc") == 0 ||
	           strcmp(name, "+pa") == 0) {
		warn_ignored(name);
		warning("the effect of +p is now the default (see -anach and -cfront)");
	} else if (strcmp(name, "+v") == 0) {
		warn_no_longer_supported2(name, "-show");
	} else if (strcmp(name, "+w") == 0) {
		warn_no_longer_supported2(name, "-fullwarn");
	} else if (strcmp(name, "+a0") == 0) {
		warn_no_longer_supported(name);
	} else if (strcmp(name, "+a1") == 0) {
		warn_no_longer_supported(name);
	} else {
		parse_error(name, "bad syntax");
	}
}

/*
 * Kludges for MDupdate/target/ignore support;
 * we put this here rather than special_options
 * cause needs to refer to files list.
 */
static void
check_makedepend_flags (void)
{
	int flag;
	if (option_was_seen(O_MDupdate)) {
	    if (outfile != NULL) {
		/* if compiling to something other than .o,
		 * then add MDtarget info */
		/* ??? should we add even if user gives -MDtarget?
		 * ??? sherwood does this, so I guess is okay. */
		flag = add_string_option(O_MDtarget, outfile);
		add_option_seen (flag);
	    } 
	    if (!multiple_source_files && files->head != NULL 
		&& last_phase == P_any_ld)
	    {
		/* if compiling .c to a.out, 
		 * don't put .o in Makedepend list */
		if (outfile == NULL) {
			char *s = change_suffix(files->head->name, NULL);
			s[strlen(s)-1] = NIL;	/* drop . of suffix */
			flag = add_string_option(O_MDtarget, s);
			add_option_seen (flag);
		}
		flag = add_string_option(O_MDignore, 
			get_object_file(files->head->name) );
		add_option_seen (flag);
	    }
	} 
}

/* mark all implied options as implicitly seen */
static void
mark_used (void)
{
  int i;
  int iflag;

  FOREACH_OPTION(i) {
    if (option_was_seen(i) && !option_was_implicitly_seen(i)) {
      FOREACH_IMPLIED_OPTION(iflag, i) {
	if (option_was_seen(iflag)) {
		continue;	/* already on list */
	}
	add_option_implicitly_seen (iflag);
	if (is_object_option(iflag)
	  && option_matches_language(i, invoked_lang) )
	{
	  /* put in object list. */
	  /* can assume it is ld option. */
	  /* name is full name, so cheat
	   * by saying it is an object,
	   * even if is really -lname. */
	  add_object (O_object, get_current_implied_name());
	}
      }
    }
  }
}

static void
print_help_msg (void)
{
	int i;
	char *msg;
	char *name;
	fprintf(stderr, "usage:  %s <options> <files>\n", program_name);
	if (help_pattern != NULL)
	  fprintf(stderr, "available options that contain %s:\n", help_pattern);
	else
	  fprintf(stderr, "available options:\n");

	FOREACH_OPTION(i) {
		msg = get_option_help(i);
		if (msg != NULL) {
		    if (option_matches_language (i, invoked_lang)) {
			name = get_option_name(i);
			/* if pattern specified, only print if pattern found */
			if (help_pattern != NULL
			    && strstr(name, help_pattern) == NULL
			    && strstr(msg, help_pattern) == NULL)
			{
				continue;	/* to next option */
			}
			fprintf(stderr, "\t%s:  %s\n", name, msg);
		    }
		}
	}
	if (help_pattern == NULL && invoked_lang == L_cc) {
	  fprintf(stderr, "The environment variable OPEN64_CC is also checked\n");
	}
	do_exit(RC_OKAY);
}

static void
dump_args (char *msg)
{
	int i;
	printf("dump args %s: ", msg);
	FOREACH_OPTION_SEEN(i) {
		if (i == O_Unrecognized) continue;
		printf(" %s", get_option_name(i));
	}
	printf("\n");
}


/*
 * GCC exits with status code 1 if any phase fails, unless given
 * -pass-exit-codes.  We want to compress all of our weirdo exit codes
 * into simple 1/0 for compatiblity.
 */
void do_exit(int code)
{
	unlink(get_report_file_name());
	if (code != 0) {
		code = 1;
	}

	exit(code);
}


static struct explicit_lang {
	const char *name;
	source_kind_t kind;
	languages_t lang;
} explicit_langs[] = {
	{ "assembler", S_s, L_as, },
	{ "assembler-with-cpp", S_S, L_as, },
	{ "c", S_c, L_cc, },
	{ "c++", S_C, L_CC, },
	{ "c++-cpp-output", S_ii, L_CC, },
	{ "c++-header", S_C, L_CC, },
	{ "c-header", S_c, L_cc, },
	{ "cpp-output", S_i, L_cc, },
	{ "f77", S_f90, L_f77, },
	{ "f77-cpp-input", S_i, L_f77, },
	{ "f90", S_f90, L_f90, },
	{ "f90-cpp-input", S_i, L_f90, },
	{ "f95", S_f90, L_f90, },
	{ "f95-cpp-input", S_i, L_f90, },
	{ "none", S_NONE, L_NONE, },
	{ "ratfor", S_r, L_f77, },
	{ NULL, S_NONE, L_NONE, },
};

void set_explicit_lang(const char *flag, const char *lang)
{
	struct explicit_lang *x;
	
	for (x = explicit_langs; x->name != NULL; x++) {
		if (strcmp(lang, x->name) == 0) {
			ignore_suffix = x->lang != S_NONE;
			if (x->kind == S_NONE) {
				ignore_suffix = FALSE;
				default_source_kind = S_NONE;
			} else {
				ignore_suffix = TRUE;
				source_kind = default_source_kind = x->kind;
				source_lang = x->lang;
			}
			break;
		}
	}

	if (x->name == NULL) {
		parse_error(flag, "Unknown language");
		do_exit(RC_USER_ERROR);
	}
}

// Quick and dirty way to scan for options that must be parsed first.
static void
prescan_options (int argc, char *argv[])
{
  char *ipa_conflict_option = NULL;
  int i;

  for (i = 1; i < argc; i++) {
    if (!strcasecmp(argv[i], "-ipa") ||
	!strcmp(argv[i], "-Ofast")) {	// -Ofast implies -ipa.  Bug 3856.
      ipa = TRUE;
    } else if (!strcmp(argv[i], "-keep")) {	// bug 2181
      keep_flag = TRUE;
    } else if (!strcmp(argv[i], "-save_temps")) {
      keep_flag = TRUE;
    } else if (!strcmp(argv[i], "-S")) {
      ipa_conflict_option = argv[i];
    } else if (!strcmp(argv[i], "-fbgen")) {
      ipa_conflict_option = argv[i];
    } else if (argv[i][0] == '-' && argv[i][1] == 'g') {	// -g...
      if (!strcmp(argv[i], "-g") ||
	  !strcmp(argv[i], "-g1") ||
	  !strcmp(argv[i], "-g2") ||
	  !strcmp(argv[i], "-g3") ||
	  !strcmp(argv[i], "-gdwarf-2") ||
	  !strcmp(argv[i], "-gdwarf-21") ||
	  !strcmp(argv[i], "-gdwarf-22") ||
	  !strcmp(argv[i], "-gdwarf-23") ||
	  !strcmp(argv[i], "-ggdb") ||
	  !strcmp(argv[i], "-ggdb3")) {
	ipa_conflict_option = argv[i];
      }
    }
  }

  // Disable for SiCortex 5069.
}

static void
print_defaults(int argc, char *argv[])
{
  int i;
  boolean parsing_defaults;

  fprintf(stderr, "Optimization level and compilation target:\n  ");

  // -O level
  if (olevel == UNDEFINED)
    fprintf(stderr, " -O2");
  else
    fprintf(stderr, " -O%d", olevel);

  if (oscale == TRUE)
      fprintf(stderr, "-mso");

  // target CPU
  if (target_cpu != NULL)
    fprintf(stderr, " -mcpu=%s", target_cpu);
  else
    internal_error("no default target cpu");

#if defined(TARG_MIPS) || defined(TARG_LOONGSON)
  // ABI
  switch (abi) {
    case ABI_N32:	fprintf(stderr, " -n32"); break;
    case ABI_64:	fprintf(stderr, " -64");  break;
    default:		internal_error("unknown default ABI");
  }
#endif

#ifdef TARG_X8664
  // ABI
  switch (abi) {
    case ABI_N32:	fprintf(stderr, " -m32"); break;
    case ABI_64:	fprintf(stderr, " -m64"); break;
    default:		internal_error("unknown default ABI");
  }

  // SSE, SSE2, SSE3, 3DNow, SSE4a
  fprintf(stderr, " -msse");
  fprintf(stderr, " %s", sse2 == TRUE ? "-msse2" : "-mno-sse2");
  fprintf(stderr, " %s", sse3 == TRUE ? "-msse3" : "-mno-sse3");
  fprintf(stderr, " %s", m3dnow == TRUE ? "-m3dnow" : "-mno-3dnow");
  fprintf(stderr, " %s", sse4a == TRUE ? "-msse4a" : "-mno-sse4a");
  // SSSE3, SSE41, SSE42
  fprintf(stderr, " %s", ssse3 == TRUE ? "-mssse3" : "-mno-ssse3");
  fprintf(stderr, " %s", sse41 == TRUE ? "-msse41" : "-mno-sse41");
  fprintf(stderr, " %s", sse42 == TRUE ? "-msse42" : "-mno-sse42");
  // AES, PCLMUL
  fprintf(stderr, " %s", aes == TRUE ? "-maes" : "-mno-aes");
  fprintf(stderr, " %s", pclmul == TRUE ? "-mpclmul" : "-mno-pclmul");
  // AVX, XOP, FMA3, FMA4
  fprintf(stderr, " %s", avx == TRUE ? "-mavx" : "-mno-avx");
  fprintf(stderr, " %s", xop == TRUE ? "-mxop" : "-mno-xop");
  fprintf(stderr, " %s", fma == TRUE ? "-mfma" : "-mno-fma");
  fprintf(stderr, " %s", fma4 == TRUE ? "-mfma4" : "-mno-fma4");
#endif

  // -gnu3/-gnu4
  if ((invoked_lang == L_cc ||
       invoked_lang == L_CC) &&
      !is_toggled(gnu_major_version)) {
    int gcc_version = get_gcc_major_version();
    if (gcc_version == 3 ||
	gcc_version == 4) {
      fprintf(stderr, " -gnu%d", gcc_version);
    } else {
      internal_error("print_defaults: unknown GCC version %d\n", gcc_version);
    }
  }

  fprintf(stderr, "\n");

  // Print options from compiler.defaults file.
  fprintf(stderr, "Options from compiler.defaults file:\n  ");
  parsing_defaults = FALSE;
  for (i = 0; i < argc; i++) {
    if (!strcmp(argv[i], "-default_options")) {
      parsing_defaults = TRUE;
    } else if (parsing_defaults == TRUE) {
      fprintf(stderr, " %s", argv[i]);
    }
  }
  fprintf(stderr, "\n");
}

static int
read_compiler_defaults(FILE *f, string_list_t *default_options_list)
{
  const int max_len = 1000;
  char *p, buf[max_len];		// p indexes through buf
  char *q, option[max_len];		// q indexes through option
  int count = 0;
  int line = 0;
  boolean in_single_quotes = FALSE;
  boolean in_double_quotes = FALSE;
  boolean follows_escape_char = FALSE;
  boolean in_comment = FALSE;

  // Scan the input buffer to build the options.
  option[0] = '\0';
  q = option;	// While building the option string, put the next char at *q.
  while (fgets(buf, max_len, f) != NULL) {
    boolean end_of_line = FALSE;
    boolean continuation = FALSE;	// '\' at end of line
    line++;

    // Parse one line.
    for (p = buf; *p != '\0' && !end_of_line; p++) {
      boolean add_new_option = FALSE;

      if (follows_escape_char) {
	follows_escape_char = FALSE;
	if (*p == '\n') {
	  // Backslash as the last char on the line means continuation.
	  continuation = TRUE;
	  end_of_line = TRUE;
	} else {
	  // Interpret \x as x.
	  *q++ = *p;
	}
      } else if (in_comment) {
	if (*p == '\n') {	// Skip the rest of the line.
	  end_of_line = TRUE;
	  in_comment = FALSE;
	}
      } else {
	switch (*p) {
	  case '\n': 
	    add_new_option = TRUE;
	    end_of_line = TRUE;
	    break;
	  case '#': 
	    if (in_single_quotes || in_double_quotes) {
	      *q++ = '#';
	    } else {
	      in_comment = TRUE;
	    }
	    break;
	  case ' ': 
	    if (in_single_quotes || in_double_quotes)
	      *q++ = ' ';
	    else
	      add_new_option = TRUE;
	    break;
	  case '\'': 	// single-quote
	    if (in_single_quotes) {
	      in_single_quotes = FALSE;	// end single-quote
	    } else if (in_double_quotes) {
	      *q++ = '\'';
	    } else {
	      in_single_quotes = TRUE;	// begin single-quote
	    }
	    break;
	  case '"': 	// double-quote
	    if (in_single_quotes) {
	      *q++ = '"';
	    } else if (in_double_quotes) {
	      in_double_quotes = FALSE;	// end double-quote
	    } else {
	      in_double_quotes = TRUE;	// begin double-quote
	    }
	    break;
	  case '\\': 	// backslash
	    if (in_single_quotes)
	      *q++ = '\\';
	    else
	      follows_escape_char = TRUE;
	    break;
	  default:	// non-special char
	    *q++ = *p;
        }
      }

      // Add the last completed option.
      if (add_new_option &&
	  option[0] != '\0') {
	*q++ = '\0';
	if (strlen(option) >= max_len) {
	  internal_error("read_compiler_defaults: buffer overrun");
	  return -1;
	}
	add_string(default_options_list, option);
	option[0] = '\0';
	q = option;
	count++;
      }
    }
  }

  return count;
}

// Append the default options in compiler.defaults to the command line options.
static void
append_default_options (int *argc, char *(*argv[]))
{
  char *compiler_defaults_path =
  #ifdef PSC_TO_OPEN64
	 string_copy(getenv("OPEN64_COMPILER_DEFAULTS_PATH"));
  #endif
  int default_options_count = 0;
  string_list_t *default_options_list = init_string_list();

  if (compiler_defaults_path == NULL) {
    char *exe_dir = get_executable_dir();
    asprintf(&compiler_defaults_path, "%.*s/etc",
	     (int)(strlen(exe_dir) - 4), exe_dir);
  }

  // Search for the defaults file in the colon-separated compiler default
  // paths.  Read in the first defaults file found.
  while (compiler_defaults_path) {
    char *p, *path;
    path = compiler_defaults_path;
    for (p = path; ; p++) {
      if (*p == '\0') {
	compiler_defaults_path = NULL;
	break;
      } else if (*p == ':') {
	*p = '\0';
	compiler_defaults_path = p + 1;
	break;
      }
    }
    // Path is one path in the colon-separated paths.  See if it contains the
    // defaults file.
    FILE *f;
    char buf[1000];
    strcpy(buf, path);
    strcat(buf, "/compiler.defaults");
    if ((f = fopen(buf, "r")) != NULL) {
      default_options_count = read_compiler_defaults(f, default_options_list);
      fclose(f);
      if (default_options_count == -1)	// Quit if error.
        return;
      break;
    }
  }

  // Append the default options to the command line options.
  {
    int new_argc = *argc + default_options_count + 1;
    char **new_argv = malloc(new_argc * sizeof(char*));
    int i, index;
    string_item_t *p;

    // Copy command line options to new argv;
    for (index = 0; index < *argc; index++) {
      new_argv[index] = (*argv)[index];
    }

    // Mark the beginning of default options.
    new_argv[index++] = "-default_options";

    // Copy default options to new argv.
    for (p = default_options_list->head; p != NULL; p = p->next) {
      new_argv[index++] = p->name;
    }

    // Return new argc and argv.
    *argc = new_argc;
    *argv = new_argv;
  }
}

/* Read the contents of a OPEN64_(GEN|C|CXX|F)FLAGS environment variable and add
 * them to the command-line options. */
static void
append_open64_env_flags (int *argc, char *(*argv[]), char *env_var)
{
  char * default_opt = string_copy(getenv(env_var));
  char * p, * q;
  char ** new_argv;
  int new_argc, fin = 0;

  if (default_opt) {
    new_argc = *argc;
    new_argv = (char **) calloc (*argc, sizeof (char *));
    memcpy (new_argv, *argv, *argc * sizeof (char *));
    for (p = default_opt, q = default_opt; fin == 0; p++) {
      switch (*p) {
      case '\0':
	fin = 1;
      case ' ':
	*p = '\0';
	new_argc++;
	new_argv = (char **) realloc (new_argv, new_argc * (sizeof (char *)));
	new_argv [new_argc-1] = strdup (q);
	q = p+1;
	break;
      default:
	break;
      }
    }
    *argc = new_argc;
    *argv = new_argv;
  }

  /* We only want to do this substitution once. */
#if defined(__MINGW32__)
  // no unsetenv on mingw, but can get same effect with putenv("name=")
  {
    string tmp = concat_strings(env_var, "=");
    putenv (tmp);
  }
#else
  unsetenv (env_var);
#endif
}

static FILE *
read_gcc_output(char *cmdline)
{
	/* P_ld may not be gcc, bug gcpp should be */
	char *gcc_path = get_full_phase_name(P_gcpp);
	char *gcc_cmd = NULL;
	FILE *fp = NULL;

#ifdef TARG_X8664
	if (asprintf(&gcc_cmd, "%s %s %s", gcc_path, (abi == ABI_N32)?"-m32":"", cmdline) == -1) {
#else
	if (asprintf(&gcc_cmd, "%s %s", gcc_path, cmdline) == -1) {
#endif
		internal_error("cannot allocate memory");
		goto bail;
	}

	if ((fp = popen(gcc_cmd, "r")) == NULL) {
		internal_error("cannot execute linker");
		fp = NULL;
	}

bail:
	free(gcc_path);
	free(gcc_cmd);
	return fp;
}

/* Print the installation path and the paths searched for binaries and
 * libraries. Portions of this code are cribbed from
 * set_library_paths() in phases.c. */
static void
print_search_path ()
{
	char *exe_dir = get_executable_dir();
	string_list_t *libdirs = init_string_list();
	
	char *root_prefix = directory_path(get_executable_dir());
	char *our_path;
	FILE *fp;
	string_item_t *p;
	char *gcc_lib_ptr;
	int buflen;
	
	printf ("install: %.*s\n", (int)(strlen(exe_dir) - 4), exe_dir);
	printf ("programs: %s:%s\n", exe_dir, get_phase_dir (P_be));
	
	if (abi == ABI_N32) {
	#ifdef PSC_TO_OPEN64
		asprintf(&our_path, "%s/lib/" OPEN64_FULL_VERSION "/32",
			 root_prefix);
	} else {
		asprintf(&our_path, "%s/lib/" OPEN64_FULL_VERSION, root_prefix);
	#endif
	}
	
	/* Add our libraries */
	add_string(libdirs, our_path);

	if (abi == ABI_N32) {
		add_string(libdirs, ":/lib");
		add_string(libdirs, ":/usr/lib");
	} else {
		add_string(libdirs, ":/lib64");
		add_string(libdirs, ":/usr/lib64");
	}
	
	if ((fp = read_gcc_output ("-print-search-dirs"))) {
		char buf[BUFSIZ];
		while (fgets (buf, BUFSIZ, fp) != NULL) {
			if (strncmp (buf, "libraries", 9) == 0) {
				gcc_lib_ptr = strchr (buf, '/');
				/* Strip the newline */
				buflen = strlen (buf);
				buf[buflen-2] = '\0';
				if (gcc_lib_ptr) {
					add_string (libdirs, concat_strings (":", gcc_lib_ptr));
				}
			}
		}
		pclose (fp);
	}

	fputs ("libraries: ", stdout);
	for (p = libdirs->head; p != NULL; p = p->next) {
		fputs (p->name, stdout);
	}
	putc('\n', stdout);

	free (our_path);
}


const char *
get_gcc_version(int *v, int nv)
{
	static char version[128];
	static int major;
	static int minor;
	static int patch;

	if (version[0] == '\0') {
#if defined(__MINGW32__)
		/* cannot rely on accessing system,
		 * so just use what we were built with */
		sprintf(version, "%d.%d", __GNUC__, __GNUC_MINOR__);
#else
		FILE *fp = read_gcc_output("-dumpversion");
		char *c;
		fread(version, 1, sizeof(version) - 1, fp);
		pclose(fp);

		version[sizeof(version) - 1] = '\0';
		
		if ((c = strchr(version, '\n'))) {
			*c = '\0';
		}
#endif
	}

	if (v) {
		char *l = version + strlen(version);
		char *a;
		int i;

		for (i = 0, a = version; i < nv; i++) {
			char *d;
			if (a < l && isdigit(*a)) {
				v[i] = strtol(a, &d, 10);
				a = d + 1;
			} else {
				v[i] = 0;
			}
		}
	}
	
	return version;
}

#if defined(TARG_SL)
  unsigned int SL_version = 0x00302000;	// version 003.02.xxx
#endif
static void
display_version(boolean dump_version_only)
{
  int gcc_version;
  char *open64_gcc_version;

  // Get GCC version.

  if (is_toggled(gnu_major_version))
    gcc_version = get_gcc_major_version();

  if (gnu_major_version == 3)
    open64_gcc_version = OPEN64_GCC_VERSION;
  else if (gnu_major_version == 4) {
    if (gnu_minor_version == 0)
      open64_gcc_version = OPEN64_GCC40_VERSION;
    else if (gnu_minor_version == 2)
      open64_gcc_version = OPEN64_GCC42_VERSION;
    else
      internal_error("display_version: unexpected GCC version 4.%d\n",
		     gnu_minor_version);
  } else
    internal_error("display_version: unexpected GCC version %d\n",
		   gnu_major_version);

  if (dump_version_only == TRUE) {
    puts(OPEN64_FULL_VERSION);
    return;
  }

#if defined(TARG_SL)
  fprintf(stderr, "Open64 version: %s.\n",OPEN64_FULL_VERSION);
  fprintf(stderr, "GNU gcc version %s.\n", open64_gcc_version);
  fprintf(stderr, "Simplnano internal release version %03x.%02x.%03x\n", (SL_version>>20), (SL_version>>12)&0xFF , SL_version&0xFFF);

  fprintf(stderr, "Built on :%s \n", build_date);
  fprintf(stderr, "Portions Copyright (c) 2006-2009 Simplnano Corporation\n");
#else
  fprintf(stderr, "Open64 Compiler Suite: Version %s\n",
	  compiler_version);
  if (show_version > 1) {
    fprintf(stderr, "Changeset: %s\n", cset_id);
    fprintf(stderr, "Built by: %s@%s in %s\n", build_user, build_host,
	    build_root);
  }

  fprintf(stderr, "Built on: %s\n", build_date);
  fprintf(stderr, "Thread model: posix\n");	// Bug 4608.
  #ifdef PSC_TO_OPEN64
  fprintf(stderr, "GNU gcc version %s", open64_gcc_version);
  fprintf(stderr, " (Open64 " OPEN64_FULL_VERSION " driver)\n");
  #endif
#endif

}

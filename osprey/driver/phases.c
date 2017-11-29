/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007 Pathscale, LLC.  All Rights Reserved.
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

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <unistd.h>
#include <errno.h>
#include <sys/param.h>
#if !defined(_WIN32)
#include <sys/utsname.h>
#include <sys/resource.h>
#endif
#include <sys/time.h>
#include "pathscale_defs.h"

#include "phases.h"
#include "options.h"
#include "lang_defs.h"
#include "option_seen.h"
#include "option_names.h"
#include "string_utils.h"
#include "errors.h"
#include "file_names.h"
#include "file_utils.h"
#include "run.h"
#include "objects.h"
#include "opt_actions.h"
#include "profile_type.h"    /* for PROFILE_TYPE */
#include "lib_phase_dir.h"   /* for LIBPATH */
#include "get_options.h"

int subverbose ;

char *outfile = NULL;		/* from -o <outfile> */
char *prof_file = NULL;	/* executable file for prof to work upon */
char *fb_file = NULL;		/* from -fb_create <feedback-file> */
char *internal_fb_file = NULL;	/* from -fb <feedback-file> */
char *opt_file = NULL;		/* from -fb_opt <feedback-file> */
char *fb_xdir = NULL;		/* dir where pixie emits dso's */
char *fb_cdir = NULL; 		/* dir where pixie emits count files */
char *command_line = NULL;	/* original command line */
char *fb_phase = NULL;         /* from -fb_phase=<phase> */
char *fb_type = NULL;          /* from -fb_type=<type> */
char *source_file = NULL;
char *coco_setfile = NULL;	/* -fcoco[=setfile]	bug 9058 */

boolean multiple_source_files = FALSE;

boolean keep_mp = FALSE;
boolean keep_list = FALSE;
boolean keep_cif = FALSE;
boolean keep_listing = FALSE;
boolean auto_parallelize = FALSE;
boolean Gen_feedback = FALSE;
boolean Use_feedback = FALSE;
boolean Disable_open_mp = FALSE;
boolean Disable_old_mp = FALSE;
boolean O3_flag = FALSE;
boolean use_cpp = FALSE;
boolean expand_ftpp_macros = TRUE;	// bug 2258
int     fortran_line_length = 72; /* Fortran line length */
char roundoff=0;
boolean nocpp_flag = FALSE;

char *global_toolroot = NULL;
char *ld_library_path = NULL;
char *ld_libraryn32_path = NULL;
char *orig_program_name = NULL;
char *old_ld_library_path = NULL;
#ifdef TARG_SL
boolean ldscript_file = FALSE;
boolean Long_Long_Support = FALSE;
boolean Float_Point_Support = FALSE;
#endif

extern void turn_down_opt_level (int new_olevel, char *msg);

static string_list_t *ipl_cmds = 0; /* record the user options that needed
				       to be passed to ipl */
static boolean string_md = FALSE;
static boolean string_mmd = FALSE;
extern string_list_t *feedback_files;

static void convert_saved_command_line_into_string(void);
static char *make_ii_file_name(char *objname);
static char *make_rii_file_name(char *objname);
static int update_instantiation_info_file(char* ii_file_name, char *sourcefile);
static void write_command_string_into_file(FILE *cmdfile,
					   char *sourcefile,
					   int for_ii_file);
static void add_command_line_arg(string_list_t *args, char *sourcefile);

static void do_f90_common_args(string_list_t *args) ;
static void set_f90_source_form(string_list_t *args,boolean set_line_length) ;
static void set_stack_size();

extern int run_build;

static phases_t
post_fe_phase (void);

static boolean
previous_mf_exists (string_list_t *list)
{
	string_item_t * p;
	
	for (p = list->head; p != NULL; p = p->next) {
		if (strcmp (p->name, "-MF") == 0) {
			return TRUE;
		}
	}
	return FALSE;
}

static void
remove_previous_mf (string_list_t *list)
{
	string_item_t * p;
	
	for (p = list->head; p != NULL; p = p->next) {
		if (strcmp (p->name, "-MF") == 0) {
			replace_string (list, p->name, "");
			replace_string (list, p->next->name, "");
		}
	}
}

static void
add_implied_string (string_list_t *list, int iflag, int flag, phases_t phase)
{
	/* assume inside a FOREACH_IMPLIED_OPTION iteration */
	char *iname = get_current_implied_name();

	if (strncmp (iname, "-MF", 3) == 0) {
		remove_previous_mf (list);
	}

	// Coco Fortran preprocessor.
	if (phase == P_f_coco) {
	  // Skip implied -D options.
	  char *name = get_option_name(flag);
	  if (!strncmp(iname, "-D", 2) &&
	      strncmp(name, "-D", 2))
	    return;

	  // Skip -O options.
	  if (!strncmp(iname, "-O", 2))
	    return;
	}

	if (option_has_blank(iflag))
		/* add a string for each blank-separated sub-string. */
		/* for -MT, only recognize the first blank (bug 982) */
		add_multi_strings(list, iname, strncmp(iname, "-MT", 3) == 0);
	else {
	        /* Fix for BUG 249 */
		if ( strcmp(iname, "-MD") ==0 ) {
			if ( string_md == FALSE ) {
				add_string(list, "-M");
				if (!previous_mf_exists (list)) {
				  add_string(list, "-MF");
				  // bug 3342: if -o was specified, use it
				  // including the entire pathname and pass
				  // to -MF so that the .d file is created in
				  // the out dir. Also use -MQ to change the
				  // target of the dependency rule to use
				  // outfile.
				  if (outfile)
				  {
				    add_string(list, change_suffix(outfile, "d"));
				    add_string(list, "-MQ");
				    add_string(list, outfile);
				  }
				  else
				    add_string(list, change_suffix(drop_path(source_file), "d"));
				}
				string_md = TRUE;
			}
			else {
				string_md = FALSE;
				add_string(list, iname);
			}
		}
		/* Fix for bug 433 and friends */
		else if ( strcmp(iname, "-MMD") ==0 ) {
			if ( string_mmd == FALSE ) {
				add_string(list, "-MM");
				if (!previous_mf_exists (list)) {
				  add_string(list, "-MF");
				  // bug 3342: see comments above
				  if (outfile)
				  {
				    add_string(list, change_suffix(outfile, "d"));
				    add_string(list, "-MQ");
				    add_string(list, outfile);
				  }
				  else
				    add_string(list, change_suffix(drop_path(source_file), "d"));
				}
				string_mmd = TRUE;
			}
			else {
				string_mmd = FALSE;
				add_string(list, iname);
			}
		}
		else if (strcmp(iname, "-mp") == 0
			 && (phase == P_spin_cc1 || phase == P_spin_cc1plus)) {
			add_string(list, "-fopenmp");
		}
		else
			add_string(list, iname);
	}
}

static void
copy_phase_options (string_list_t *phase_list, phases_t phase)
{
	int flag;
	int iflag;
	FOREACH_OPTION_SEEN(flag) {
		FOREACH_IMPLIED_OPTION(iflag, flag) {
			boolean matches_phase = FALSE;

			/* Only add if option is legal for phase and lang.
			 * Make sure it matches both parent and implied lang.
			 * Also, don't add object options. */

                        // Hack to filter out -OPT: options to spin_cc1,
			// which takes all front-end options except -OPT:.  Bug
			// 10209.  (Why is -OPT classified as a front-end
			// option in the first place?)
			//
			// Pass -OPT: options to wgen for bug 10262.
			if (gnu_major_version == 4 &&
			    !strcmp("-OPT:", get_option_name(iflag))) {
			  if (phase == P_spin_cc1 ||
			      phase == P_spin_cc1plus)
			    continue;
			  else if (phase == P_wgen)
			    matches_phase = TRUE;
			  else
			    matches_phase = option_matches_phase(iflag, phase);
			} else
			matches_phase = option_matches_phase(iflag, phase);

			if (matches_phase
			    && option_matches_language(flag, source_lang)
			    && !is_object_option(iflag) )
			{
			    /* add check for cpp options that transcend real
			     * source language. */
			    if (option_matches_language(iflag, source_lang)
				|| (is_matching_phase(get_phase_mask(phase), P_any_cpp)
				    && option_matches_language(iflag, L_cpp) ) )
			    {
				add_implied_string (phase_list, iflag, flag,
						    phase);
			    }
			}
		}
	}
}

/* ====================================================================
 *
 * add_language_option
 *
 * The back end needs to know the language, so that -LIST:cite can
 * determine which of whirl2c/whirl2f to invoke.  Pass it as the
 * no-name -LANG: suboption.
 *
 * ====================================================================
 */

static void
add_language_option ( string_list_t *args )
{
  switch ( invoked_lang ) {
    case L_f77:
	add_string ( args, "-LANG:=f77" );
	break;
    case L_f90:
	add_string ( args, "-LANG:=f90" );
	break;
    case L_cc:
	if ( ansi == KR_ANSI ) {
	  add_string ( args, "-LANG:=kr_c" );
	} else {
	  add_string ( args, "-LANG:=ansi_c" );
	}
	break;
    case L_CC:
	add_string ( args, "-LANG:=cplus" );
	break;
  }
}

/* ====================================================================
 *
 * add_targ_options
 *
 * Add -TARG options to the back-end.
 *
 * ====================================================================
 */

static void
add_targ_options ( string_list_t *args )
{
  // -TARG:processor=xxx
  if (target_cpu != NULL) {
    char buf[100];
    sprintf(buf, "-TARG:processor=%s", target_cpu);
    add_string(args, buf);
  }

#ifdef TARG_X8664
  // MMX, SSE, SSE2, SSE3, 3DNow, SSE4a
  if (sse2 == TRUE) {
    add_string(args, "-TARG:sse2=on");
    mmx = TRUE;
    sse = TRUE;
  } else
    add_string(args, "-TARG:sse2=off");

  if (mmx == TRUE)
    add_string(args, "-TARG:mmx=on");
  else
    add_string(args, "-TARG:mmx=off");

  if (sse == TRUE)
    add_string(args, "-TARG:sse=on");
  else
    add_string(args, "-TARG:sse=off");

  if (sse3 == TRUE)
    add_string(args, "-TARG:sse3=on");
  else
    add_string(args, "-TARG:sse3=off");

  if (m3dnow == TRUE)
    add_string(args, "-TARG:3dnow=on");
  else
    add_string(args, "-TARG:3dnow=off");

  if (sse4a == TRUE)
    add_string(args, "-TARG:sse4a=on");
  else
    add_string(args, "-TARG:sse4a=off");

  if (ssse3 == TRUE)
    add_string(args, "-TARG:ssse3=on");
  else
    add_string(args, "-TARG:ssse3=off");

  if (sse41 == TRUE)
    add_string(args, "-TARG:sse41=on");
  else
    add_string(args, "-TARG:sse41=off");

  if (sse42 == TRUE)
    add_string(args, "-TARG:sse42=on");
  else
    add_string(args, "-TARG:sse42=off");

  if (aes == TRUE)
    add_string(args, "-TARG:aes=on");
  else
    add_string(args, "-TARG:aes=off");

  if (pclmul == TRUE)
    add_string(args, "-TARG:pclmul=on");
  else
    add_string(args, "-TARG:pclmul=off");

  if (avx == TRUE)
    add_string(args, "-TARG:avx=on");
  else
    add_string(args, "-TARG:avx=off");

  if (xop == TRUE)
    add_string(args, "-TARG:xop=on");
  else
    add_string(args, "-TARG:xop=off");

  if (fma == TRUE)
    add_string(args, "-TARG:fma=on");
  else
    add_string(args, "-TARG:fma=off");

  if (fma4 == TRUE)
    add_string(args, "-TARG:fma4=on");
  else
    add_string(args, "-TARG:fma4=off");
#endif
}

static char basebuf[4096];

char *driver_basename(char *const s)
{
    register char *p;
    register char *const t = basebuf;

    if (s == NULL || *s == 0) {
	return strcpy(t, ".");
    } else {
	p = strcpy(t, s);
	p += strlen(p);
        while( p != t  && is_dir_separator(*--p) )	/* skip trailing /s */
	    *p = '\0';

        while( p != t ) {
#if defined(__CYGWIN__) || defined(_WIN32)
            if ( p == t + 1 && t[1] == ':' )
                return ++p;
#endif
            if(  is_dir_separator(*--p) )
		return  ++p;
	}
	return p;
    }
}
static char dirbuf[4096];

char *dirname(char *const s)
{
    register char *p;
    register char *const t = dirbuf;

    if (s == NULL || *s == 0) {
	return strcpy(t, ".");
    } else {
	p = strcpy(t, s);
	p += strlen(p);
        while( p != t  && is_dir_separator(*--p) )	/* skip trailing /s */
	    ;

#if defined(__CYGWIN__) || defined(_WIN32)
        if ( p == t + 1 && t[1] == ':' ) {
            t[2] = 0;
            return t;
        }
#endif
        if ( p == t &&  is_dir_separator(*p) )
	    return strcpy(t, "/");

        while( p != t ) {
#if defined(__CYGWIN__) || defined(_WIN32)
            if ( p == t + 1 && *p == ':' ) {
                t[2] = '.';
                t[3] = 0;
                return t;
            }
#endif
            if(  is_dir_separator(*--p) ) {
                if ( p == t )
                    return strcpy(t, "/");
                while ( is_dir_separator (*p) )
		    p--;
		*++p = '\0';
		return  t;
	    }
	}
	return strcpy(t, ".");
    }
}


static char *input_source ;	/* src to next phase */

static void add_arg(string_list_t *args, const char *format, ...)
	__attribute__((format (printf, 2, 3)));

static void
add_arg(string_list_t *args, const char *format, ...)
{
	char *arg;
	va_list ap;

	va_start(ap, format);

	vasprintf(&arg, format, ap);
	add_string(args, arg);
	free(arg);

	va_end(ap);
}

/* Keep this in sync with print_file_path over in opt_actions.c. */

static void
set_library_paths(string_list_t *args)
{
	char *root_prefix = directory_path(get_executable_dir());
	char *our_path;
	
	if (abi == ABI_N32) {
#ifndef TARG_SL
		asprintf(&our_path, "%s/" LIBPATH "/32",
			 global_toolroot);
#else
    if (use_sl5 == TRUE)
      asprintf(&our_path, "%s/libsl5/",root_prefix);
    else
      asprintf(&our_path, "%s/lib/",root_prefix);
#endif
	} else {
	  asprintf(&our_path, "%s/" LIBPATH "/64", global_toolroot);
	  add_string(args, concat_strings("-L", our_path));
	  asprintf(&our_path, "%s/" LIBPATH, global_toolroot);
	}
	
	add_string(args, concat_strings("-L", our_path));

	free(our_path);
}

/*
 * If we are compiling stdin, we only pass the name we're given ("-")
 * to the preprocessor.  For every other phase, we must fake up a
 * name, so that the default output file name will have a sensible
 * extension.
 */
static char *
fix_name_by_phase (char *name, phases_t phase)
{
	char *new_name = name;
	
	if (name != NULL && strcmp(name, "-") == 0) {
		switch (phase) {
		case P_cpp:
		case P_c_cpp:
		case P_cplus_cpp:
		case P_f_cpp:
		case P_f90_cpp:
		case P_ratfor:
		case P_m4:
		case P_gcpp:
		case P_gcpp_plus:
			break;
		default:
			switch (source_kind) {
			case S_f:
			case S_F:
			case S_f90:
			case S_F90:
				new_name = "stdin.f90";
				break;
			case S_C:
			case S_ii:
				new_name = "stdin.cc";
				break;
			case S_s:
			case S_S:
				new_name = "stdin.s";
				break;
			case S_c:
			default:
				new_name = "stdin.c";
				break;
			}
		}
	}

	return new_name;
}

char *
fix_name_by_lang(char *name)
{
	char *new_name = name;

	if (name != NULL && strcmp(name, "-") == 0) {
		switch (source_lang) {
		case L_f77:
		case L_f90:
			new_name = "stdin.f";
			break;
		case L_as:
			new_name = "stdin.s";
			break;
		case L_CC:
			new_name = "stdin.cc";
			break;
		default:
			new_name = "stdin.c";
		}
	}
	return new_name;
}
		
static void
add_inc_path(string_list_t *args, const char *fmt, ...)
	__attribute__((format (printf, 2, 3)));
    
static void
add_inc_path(string_list_t *args, const char *fmt, ...)
{
	char *path;
	va_list ap;
	
	va_start(ap, fmt);
	vasprintf(&path, fmt, ap);
	if (is_directory(path)) {
		add_string(args, "-isystem");
		add_string(args, path);
	}
	free(path);
	va_end(ap);
}

boolean platform_is_64bit(void)
{
	static boolean _64bit_set;
	static boolean _64bit;

	if (!_64bit_set) {
#if defined(_WIN32)
                _64bit = FALSE;
#else
		struct utsname u;

		uname(&u);

		_64bit = strcmp(u.machine, "x86_64") == 0;
#endif		
		_64bit_set = TRUE;
	}

	return _64bit;
}

boolean
target_is_native(void)
{
	static boolean native;
	static boolean native_set;

	if (!native_set) {
		if (platform_is_64bit()) {
			native = abi != ABI_N32;
		} else {
			native = abi == ABI_N32;
		}

		native_set = TRUE;
	}
	
	return native;
}

//#ifdef CROSS_COMPILATION
/* there should be a portable way the specify the dynamic loader
 * for the targeting system.
 */
static const char* dynamic_linker
#ifdef TARG_IA64
   = "/lib/ld-linux-ia64.so.2"
#endif
;
static void
specify_dyn_linker (string_list_t *args) {
#ifdef TARG_IA64
	if (shared == CALL_SHARED) {
                add_string(args, "-dynamic-linker");
                add_string(args, (char*)dynamic_linker);
        }
#endif
}
//#endif /* CROSS_COMPILATION */

static const char* ipa_dynamic_linker
#ifdef TARG_IA64
   = "-Wl,/lib/ld-linux-ia64.so.2"
#endif
;


static void
specify_ipa_dyn_linker (string_list_t *args) {
#ifdef TARG_IA64
	if (shared == CALL_SHARED) {
                add_string(args, "-Wl,-dynamic-linker");
                add_string(args, (char*)ipa_dynamic_linker);
        }
#endif
}

// Like add_file_args but add args that must precede options specified on the
// command line.
static void
add_file_args_first (string_list_t *args, phases_t index)
{
  switch (index) {
    case P_gcpp:
    case P_gcpp_plus:
      if (run_build) {
	char p[PATH_BUF_LEN];
	sprintf(p, "-B%s", get_phase_dir(index));
	add_string(args, p);
	if (index == P_gcpp_plus) {
#if defined(TARG_X8664)
	  if (abi == ABI_N32)
	    sprintf(p, "-I%s/../%s/32/libstdc++-v3/include/%s", get_phase_dir(index),GCC_CONFIGURE_TARG,GCC_CONFIGURE_TARG);
          else
	    sprintf(p, "-I%s/../%s/libstdc++-v3/include/%s", get_phase_dir(index),GCC_CONFIGURE_TARG,GCC_CONFIGURE_TARG);
#else
	  sprintf(p, "-I%s/../%s/libstdc++-v3/include/%s", get_phase_dir(index),GCC_CONFIGURE_TARG,GCC_CONFIGURE_TARG);
#endif
	  add_string(args, p);
#if defined(TARG_X8664) || defined(TARG_NVISA)
  	  if (abi == ABI_N32)
	    sprintf(p, "-I%s/../%s/32/libstdc++-v3/include", get_phase_dir(index),GCC_CONFIGURE_TARG);
          else
	    sprintf(p, "-I%s/../%s/libstdc++-v3/include", get_phase_dir(index),GCC_CONFIGURE_TARG);
#else
	  sprintf(p, "-I%s/../%s/libstdc++-v3/include", get_phase_dir(index),GCC_CONFIGURE_TARG);
#endif
	  add_string(args, p);
	  sprintf(p, "-I%s/%s/libstdc++-v3/libsupc++", BUILD_SRC, GCC_DIR);
	  add_string(args, p);
        }
      }
      // -Dfoo before user options, since user might specify -Ufoo.  Bug 6874.
      if (option_was_seen(O_pthread))
	add_string(args, "-D_REENTRANT");
      #ifdef PSC_TO_OPEN64
      if (!option_was_seen(O_no_opencc)) {
	add_string(args, "-D__OPEN64__=\"" OPEN64_FULL_VERSION "\"");
	add_string(args, "-D__OPENCC__=" OPEN64_MAJOR_VERSION);
	add_string(args, "-D__OPENCC_MINOR__=" OPEN64_MINOR_VERSION);
	add_string(args, "-D__OPENCC_PATCHLEVEL__=" OPEN64_PATCH_LEVEL);
      }
      #endif
  }
}

#if defined(TARG_MIPS) && !defined(ARCH_MIPS)
#define MIPS_CROSS_LIB_TOP_DIR "/opt/gentoo-root"
#endif

/*
 * Normally "-isystem" can be handled entirely by cpp. For Fortran, we want
 * the -isystem directories to be inserted in the module-search list after
 * the directories specified by the user ("-I whatever", which gets mapped
 * onto "-include=whatever") but before any "system" directory(s) like
 * "-include=<PSC_FULL_VERSION>/include" specified by the driver. It seems
 * safer and simpler to do that here, since the Fortran front end doesn't
 * know where the user directories end and the system directories start.
 */
static void
add_isystem_dirs(string_list_t *args)
{
  string_item_t *p;
  if (isystem_dirs) {
    for (p = isystem_dirs->head; p != NULL; p = p->next)  {
      add_string(args, p->name);
    }
  }
}

static void
add_file_args (string_list_t *args, phases_t index)
{
	buffer_t buf;
	char *temp;
	string_item_t *p;
	char *count_file_name;
	char *the_file = fix_name_by_phase(source_file, index);

	/* current_phase is used to say which file might be the last output 
	 * file and thus might need a local name rather than a tmp name */
	current_phase = P_NONE;
	switch (index) {
	case P_m4:
		add_string(args, the_file);
		add_string(args, ">");
		input_source = construct_given_name(the_file,"p", keep_flag);
		add_string(args, input_source);
		break;
	case P_ratfor:
		if (run_m4) {
			input_source = construct_given_name(the_file,"p", keep_flag);
			add_string(args, input_source);
		} else {
			add_string(args, the_file);
		}
		add_string(args, ">");
		input_source = construct_given_name(the_file,"f", TRUE);
		add_string(args, input_source);
		break;
	case P_cpp:
		if (source_lang == L_as) {
			add_string(args, "-Xdo_linesplice");
			add_string(args, "-dollar");
		}
		/* fallthru */
	case P_c_cpp:
	case P_cplus_cpp:
	case P_f_cpp:
	case P_f90_cpp:
		if (option_was_seen(O_usegfe)) {
			add_string(args, "-E");
		}
 	        if (index == P_f90_cpp) {
		   if (expand_ftpp_macros) {
		      add_string(args, "-F");
		   }
		   set_f90_source_form(args,TRUE);
		}
		if (input_source == NULL)
		   input_source = string_copy(the_file);
		add_string(args, input_source);
		if (last_phase != P_any_cpp) {
			add_string(args, ">");
			current_phase = P_any_cpp;
			input_source = construct_name(input_source,"i");
			add_string(args, input_source);
		}
		break;
	case P_gcpp:
	case P_gcpp_plus:
		if (show_but_not_run)
			add_string(args, "-###");
#if defined(TARG_X8664) || defined(TARG_NVISA)
		if( abi == ABI_N32 ){
		  add_string(args, "-m32");
		}
#elif defined(TARG_SL)
		add_string(args, "-D__JAVI__");
		add_string(args, "-D__SL__");
		add_string(args, "-DRAND32");
		/* add uclibc micro */
		if (Float_Point_Support == TRUE)
		{
		  add_string(args, "-D__UCLIBC_HAS_FLOATS__");
		  add_string(args, "-D__HAS_FPU__");
		}
                add_string(args, "-B");
                add_string(args, concat_strings(global_toolroot,"/usr/altbin"));
#elif defined(TARG_MIPS) || defined(TARG_LOONGSON)
		if( abi == ABI_N32 )
		  add_string(args, "-mabi=n32");
		else
		  add_string(args, "-mabi=64");
#endif

		if( ospace == TRUE ){	// bug 4953
		  add_string(args, "-Os");
		}
		
		switch (source_lang) {
		case L_as:
			add_string(args, "-xassembler-with-cpp");
			break;
		case L_CC:
			add_string(args, "-xc++");
			break;
		case L_f77:
		case L_f90:
			add_string(args, "-traditional");

			if (!option_was_seen(O_Wendif_labels))
				add_string(args, "-Wno-endif-labels");
		case L_cc:
		default:
			add_string(args, "-xc");
			break;
		}

		if (!option_was_seen(O_nostdinc)) {
			char *root = directory_path(get_executable_dir());
			#ifdef PSC_TO_OPEN64
			add_inc_path(args, "%s/include/" OPEN64_FULL_VERSION,
				     root);
			#endif
			if (source_lang == L_CC) {
				int v[4];
				get_gcc_version(v, 4);
				if (v[0] > 3 || (v[0] == 3 && v[1] >= 3)) {
					add_inc_path(args, "%s/include/"
						     #ifdef PSC_TO_OPEN64
						     OPEN64_FULL_VERSION
						     #endif
						     "/backward",
						     root);
				}
			}
			add_inc_path(args, "%s/include", root);
		}
		
		// Call gcc preprocessor using "gcc -E ...".
		add_string(args, "-E");
		if (sse2 == TRUE)
			add_string(args, "-msse2");
		else if(sse == TRUE)
			add_string(args, "-msse");
		else if(mmx == TRUE)
			add_string(args, "-mmmx");
                

#if defined(TARG_SL)
		char *comp_target_root = getenv("COMP_TARGET_ROOT");
		char *root_prefix;
		char *inc_path;
		if (comp_target_root != NULL) {
			root_prefix = comp_target_root;
			asprintf(&inc_path, "%s/usr/include", root_prefix);
		}
		else {
			root_prefix = directory_path(get_executable_dir());
			asprintf(&inc_path, "%s/include", root_prefix);
		}
		add_string(args, "-isystem");
		add_string(args, inc_path);
		if (source_lang == L_CC) {
			asprintf(&inc_path, "%s/usr/include/c++", root_prefix);
			add_string(args, "-isystem");
			add_string(args, inc_path);
		}
#endif
		add_string(args, input_source);
		if (option_was_seen(O_E) && outfile != NULL) {
			add_string(args, "-o");
			add_string(args, outfile);
		}
		else if (last_phase != P_any_cpp) {
			current_phase = P_any_cpp;
			if (source_lang == L_CC)
			    input_source = construct_name(input_source,"ii");
			else if (source_lang == L_as) {
			    input_source = construct_name(input_source,"s");
			    if (!keep_flag) {
			      input_source = concat_strings(input_source, ".s");
			      mark_for_cleanup(input_source);
			    }
			}
			else
			    input_source = construct_name(input_source,"i");
			add_string(args, "-o");		// gcc -o ...
			add_string(args, input_source);
		}
		break;
	case P_f_coco:	// bug 9058
		{
		  char *fortran_source = input_source;
		  current_phase = P_any_cpp;
		  add_string(args, "-s");
		  sprintf(buf, "-#%s", coco_setfile ? coco_setfile : "");
		  add_string(args, buf);
		  if (option_was_seen(O_E)) {
		    if (outfile == NULL)
		      add_string(args, "-E");
		    else
		      add_string(args, outfile);	// coco output file
		  } else {
		      input_source = construct_name(input_source, "i");
		      add_string(args, input_source);	// coco output file
		  }
		  add_string(args, fortran_source);	// coco input file
		}
		break;
	case P_pca:
	case P_pfa:
		sprintf(buf, "-I=%s", input_source);
		add_string(args, buf);
		sprintf(buf, "-original_filename=%s", the_file);
		add_string(args, buf);
		if (index == P_pfa)
		    add_string(args, "-include=/usr/include");
		{
		  char *list_suffix, *cmp_suffix;
		  extern char *optargs;

		  if (roundoff) {
		    /* if roundoff has been specified, pass it to pfa/pca */
		    sprintf(buf, "-r=%c", roundoff);
		    add_string(args, buf);
		  } else if (O3_flag) {
		    /* if -O3 has been specified, but not roundoff, pass -r=2
		       to pfa/pca */
		    add_string(args, "-r=2");
		  }
		  if (index == P_pca) {
		     cmp_suffix = "M";
		     list_suffix = "L";
		  } else {
		     /* pfa */
		     cmp_suffix = "m";
		     list_suffix = "l";
		  }
		  if (keep_list) {
		     sprintf(buf, "-L=%s", 
		         construct_given_name(
			   the_file, list_suffix, TRUE /* keep*/));
		     add_string(args, buf);
		  } else {
		     sprintf(buf, "-L=%s", 
			  construct_name(input_source,list_suffix));
		     add_string(args, buf);
		  }
		  if (keep_mp) {
		     input_source = construct_given_name(the_file,
			 cmp_suffix, TRUE);
		     sprintf(buf, "-analysis=%snl", 
		         construct_given_name(
			   the_file, "a", TRUE /* keep*/));
		     add_string(args, buf);
		  } else {
		     input_source = construct_name(input_source,cmp_suffix);
		     add_string(args, "-noanalysis");
		  }
		}
		if (keep_listing) {
		   sprintf(buf, "-lo=ocktl");
		   add_string(args, buf);
		}
		sprintf(buf, "-CMP=%s", input_source);
		add_string(args, buf);
		add_string(args, "-cp=i");
		break;
        case P_mpc:
                sprintf(buf, "%s", input_source);
                add_string(args, buf);
                if (keep_mp) {
                   input_source = construct_given_name(the_file, "P", TRUE);
                } else {
                   input_source = construct_name(input_source, "P");
                }
                sprintf(buf, "-XK%s", input_source);
                add_string(args, buf);
                break;
	case P_f_fe:
	case P_cppf_fe:
                // Give -ansi to FTN fe and to the cpp embedded in the FTN fe.
		// Bug 8029.
		if (ansi == STRICT_ANSI) {
		  add_string(args, "-ansi");
		}
		add_targ_options ( args ); /* Bug 5089 */
		/* If doing IPA or inlining, can't do full-split: */
		if ( ipa == TRUE || inline_t == TRUE ) {
		  add_string ( args, "-FE:full_split=off" );
		}

		replace_string( args, "-fpic", "" );
		replace_string( args, "-fPIC", "" );

		sprintf(buf, "-fB,%s", construct_name(the_file,"B"));
 		add_string(args, buf); 
		if (keep_listing) {
			sprintf(buf, "-fl,%s",construct_given_name(the_file,"L",TRUE));
			add_string(args, buf);
		}

		if (index == P_cppf_fe) {
		   if (Disable_open_mp) {
		      add_string(args,"-disable_open_mp");
                   } 
		   if (Disable_old_mp) {
		      add_string(args,"-disable_old_mp");
                   }
                }

		if (use_craylibs == TRUE) {
		  add_string(args,"-TENV:io_library=cray");
		} else if (use_mipslibs == TRUE) {
		  add_string(args,"-TENV:io_library=mips");
		} else {
		  /* This is the default for f77.  For release 7.2 the
		     default is to use to old (libftn.so.1) library.  For
		     release 7.3 (?) it should be switched to use the
		     new (libftn.so.2) library. */
		  add_string(args,"-TENV:io_library=mips");
		}
                add_isystem_dirs(args);
		{
		  char *root = directory_path(get_executable_dir());
		  #ifdef PSC_TO_OPEN64
		  sprintf (buf, "-include=%s/include/" OPEN64_FULL_VERSION, root);
		  #endif
		  add_string(args, buf);
		}

		if (dashdash_flag)
		  add_string(args,"--");
		add_string(args, input_source);
		break;

        case P_lister:
		if (keep_listing) {
		   char *listing_file;
		   char *cif_file;
		   char *has_path;
		   input_source = string_copy(the_file);
		   listing_file = construct_given_name(input_source,"L", TRUE);
		   cif_file = construct_given_name(drop_path(the_file), "T", keep_flag || keep_cif);
		   add_string(args, "-rs");
		   add_string(args, "-rx");
		   add_string(args, "-o");
		   add_string(args, listing_file);
		   add_string(args, input_source);
		   has_path = strrchr (input_source, '/');
		   if (has_path != NULL)
		     /* The .f file is not in the current directory */
		     add_string(args, cif_file);
                }
		break;

	case P_f90_fe:
	case P_cppf90_fe:
                // Give -ansi to FTN fe and to the cpp embedded in the FTN fe.
		// Bug 8029.
		if (ansi == STRICT_ANSI) {
		  add_string(args, "-ansi");
		}
		add_targ_options ( args ); /* Bug 5089 */
                if (index == P_cppf90_fe) {
                   if (expand_ftpp_macros) {
                      add_string(args, "-F");
                   }

                   if (use_ftpp == 1) {
                      add_string(args, "-ftpp");
                   }
                }

		replace_string ( args, "-fpic", "" );
		replace_string ( args, "-fPIC", "" );
		replace_string ( args, "-fno-pic", "" );	// bug 8463
		replace_string ( args, "-fno-PIC", "" );

		if ( ipa == TRUE || inline_t == TRUE ) {
		  add_string ( args, "-FE:full_split=off" );
		}

		if (keep_listing) {
			add_string(args, "-Ca");
		}

		sprintf(buf, "-fB,%s", construct_name(the_file,"B"));
 		add_string(args, buf); 
		sprintf(buf, "-fC,%s", construct_given_name(the_file,"T",TRUE));
 		add_string(args, buf); 
		sprintf(buf, "-fD,%s", construct_given_name(the_file,"l",TRUE));
 		add_string(args, buf); 
		/* It's much easier for the driver to generate this */
		if (option_was_seen(O_MDupdate) && !option_was_seen(O_MDtarget)) {
		   add_string(args, "-MDtarget"); 
		   add_string(args, construct_given_name(the_file,"o",TRUE));
		}
		if (keep_listing) {
			sprintf(buf, "-fl,%s",construct_given_name(the_file,"L",TRUE));
			add_string(args, buf);
		}
		do_f90_common_args(args) ;

                add_isystem_dirs(args);
		{
		  char *root = directory_path(get_executable_dir());
		  #ifdef PSC_TO_OPEN64
		  sprintf (buf, "-include=%s/include/" OPEN64_FULL_VERSION, root);
		  #endif
		  add_string(args, buf);
		}

                if (Disable_open_mp) {
                   add_string(args,"-disable_open_mp");
                }

                if (Disable_old_mp) {
                   add_string(args,"-disable_old_mp");
                }
		if (dashdash_flag)
		  add_string(args,"--");
		add_string(args, input_source);

		if (option_was_seen(O_E) && outfile != NULL) {
			add_string(args, "-o");
			add_string(args, outfile);
		}

		break;

	case P_spin_cc1:
	case P_spin_cc1plus:
		{
#if !defined(_WIN32)
		  struct utsname uts;
		  uname(&uts);
		  if (strstr(uts.machine, "x86_64") == NULL) {
		    add_string(args, "-fi386-host");		// bug 10532
		  }
#endif
		}
#ifndef TARG_SL
		if (fcxx_openmp == 1) {
		  add_string(args, "-fcxx-openmp");
		}
		else if (fcxx_openmp == 0) {
		  add_string(args, "-fno-cxx-openmp");
		}
#endif
	        // fall through
	case P_c_gfe:
	case P_cplus_gfe:
		if (sse2 == TRUE)
			add_string(args, "-msse2");
		else if(sse == TRUE)
			add_string(args, "-msse");
		else if(mmx == TRUE)
			add_string(args, "-mmmx");
		// add -msse3 later when fe support is available

		if (show_but_not_run)
			add_string(args, "-###");
		add_string(args, "-dx");
		if (show_version) {
			add_string(args, "-version");
		}
		if (quiet_flag) 
			add_string(args, "-quiet");
#if defined(TARG_X8664) || defined(TARG_NVISA)
		if( abi == ABI_N32 )
		  add_string(args, "-m32");
#elif defined (TARG_LOONGSON)
                if( abi == ABI_N32 )
                  add_string(args, "-mabi=n32");
                else if (abi == ABI_64) 
                  add_string(args, "-mabi=64");
#elif defined(TARG_MIPS)
#ifndef TARG_SL
		// endianness
		if (endian == ENDIAN_LITTLE)
		  add_string(args, "-mel");
		else
		  add_string(args, "-meb");

		// abi
		if( abi == ABI_N32 )
		  add_string(args, "-mabi=n32");
		else
		  add_string(args, "-mabi=64");
#endif
#endif

		if (!option_was_seen(O_fpreprocessed) &&
		    !option_was_seen(O_fno_preprocessed)) {
		  add_string(args, "-fpreprocessed");
		}
#ifndef TARG_SL
		if( fbuiltin != 0 )
		  add_string(args, "-fbuiltin" );
		else
		  add_string(args, "-fno-builtin" );
#endif
		if( fmath_errno == 0 )
		  add_string(args, "-fno-math-errno");

		if( ffast_math == 1 )
		  add_string(args, "-ffast-math");
#ifdef TARG_SL
                if ((index == P_cplus_gfe) || (index == P_spin_cc1plus)) {
                  // no exception support for embedded systems
                  add_string(args, "-fno-exceptions");
                  add_string(args, "-fno-rtti");
                }
#endif
		add_string(args, "-dumpbase");
		// Bug 2458 - path to the source file should not be dropped
		// in the command to the front-end because this is used in
		// building the DST info (DW_TAG_name in DW_TAG_compile unit)
		add_string(args, the_file);
		add_string(args, input_source);

		if (index == P_spin_cc1 ||
		    index == P_spin_cc1plus) {
		  add_string(args, "-spinfile");
		  add_string(args, construct_name(the_file, "spin"));
		  break;
		}
		add_string(args, "-o");
		add_string(args, construct_name(the_file,"B"));
		break;
	case P_wgen:
		sprintf(buf, "-fS,%s", construct_name(the_file, "spin"));
		add_string(args, buf);
		sprintf(buf, "-fB,%s", construct_name(the_file, "B"));
		add_string(args, buf);
		break;
	case P_inline:
		if (source_kind == S_B)
		    sprintf (buf, "-fB,%s", the_file);
		else
		    sprintf(buf, "-fB,%s",
			    construct_name(the_file,"B"));
		add_string (args, buf);
		sprintf (buf, "-fI,%s", 
			construct_name(the_file,get_suffix_string(S_I)) );
		add_string (args, buf);
		if (dashdash_flag)
		  add_string(args,"--");
		add_string(args, the_file);
		break;
	case P_ipl:
            if (oscale == TRUE)
                add_string(args, "-OPT:scale=ON");

		add_language_option ( args );
		if (source_kind == S_B)
		    sprintf (buf, "-fB,%s", the_file);
		else
		    sprintf(buf, "-fB,%s",
			    construct_name(the_file,"B"));
		add_string (args, buf);

		if (instrumentation_invoked == TRUE) {
		  if (fb_file != NULL)
		    sprintf(buf, "-fi,%s.instr", fb_file);
		  else if (outfile != NULL)
		    sprintf (buf, "-fi,%s.instr", outfile);
                  else
		    sprintf (buf, "-fi,a.out.instr");
		  add_string(args,buf);
                }
		else if (opt_file != NULL) {
		  /* pass feedback file */
		  sprintf(buf, "-ff,%s.instr", opt_file);
		  add_string(args, buf);
		}

		current_phase = P_any_as;
		if (outfile != NULL && last_phase == current_phase
	 			    && !multiple_source_files
				    && !(remember_last_phase == P_any_ld &&
				         invoked_lang == L_CC))
			{
			  sprintf(buf, "-fo,%s", outfile);
			} else {
			  // Create unique .o files for a.c and foo/a.c.
			  // Bug 9097.
			  sprintf(buf, "-fo,%s", get_object_file(the_file));
			}
		add_string(args, buf);
		if (dashdash_flag)
		  add_string(args,"--");
		add_string(args, the_file);

		/* ipl_cmds must be added last */
		if (ipl_cmds != 0) {
		    add_string (args, "-cmds");
		    append_string_lists (args, ipl_cmds);
		}
		break; 
	case P_be:
#if defined(TARG_NVISA)
	case P_bec:
#endif
#ifdef TARG_SL
          if (use_sl1_dsp != TRUE && use_sl5 != TRUE) {
            add_string(args,"-TARG:processor=sl1_pcore");
          }
#endif           
#ifdef TARG_LOONGSON
		add_string(args,"-TENV:pic2");
#endif
		add_language_option ( args );
		add_targ_options ( args );

		if (invoked_lang == L_f77) {
		  if (use_craylibs == TRUE) {
		    add_string(args,"-TENV:io_library=cray");
		  } else if (use_mipslibs == TRUE) {
		    add_string(args,"-TENV:io_library=mips");
		  } else {
		    /* This is the default for f77.  For release 7.2 the
		       default is to use to old (libftn.so.1) library.  For
		       release 7.3 (?) it should be switched to use the
		       new (libftn.so.2) library. */
		    add_string(args,"-TENV:io_library=mips");
		  }
		}

		switch (source_kind) {
		case S_B:
		    if (post_fe_phase () == P_inline) {
			temp = construct_name(the_file,get_suffix_string(S_I));
			break;
		    }
		    /* fall through */
		case S_I:
		case S_P:
		case S_N:
		case S_O:
		    temp = the_file;
		    break;
		default:
		    if (post_fe_phase () == P_inline)
			temp = construct_name(the_file,get_suffix_string(S_I));
		    else
			temp = construct_name(the_file,"B");
		    break;
		}
		sprintf (buf, "-fB,%s", temp);
		add_string(args, buf);

		if (instrumentation_invoked == TRUE) {
		  if (fb_file != NULL)
		    sprintf(buf, "-fi,%s.instr", fb_file);
		  else if (outfile != NULL)
		    sprintf (buf, "-fi,%s.instr", outfile);
                  else
		    sprintf (buf, "-fi,a.out.instr");
		  add_string(args,buf);
                }
		else if (opt_file != NULL) {
		  /* pass feedback file */
		  sprintf(buf, "-ff,%s.instr", opt_file);
		  add_string(args, buf);
		}

		if (internal_fb_file != NULL) {
		  /* pass feedback file */
		  sprintf(buf, "-ff,%s.instr", internal_fb_file);
		  add_string(args, buf);
		}

#ifdef TARG_NVISA
		if (last_phase == P_bec && outfile != NULL) {
		  /* outfile == w2c.c name */
		  if (get_suffix(outfile) == NULL) {
		    /* outfile should end in .c */
		    error ("outfile (%s) should end in .c", outfile);
		    return;
		  }
		  sprintf(buf, "-fc,%s", outfile);
		  add_string(args, buf);
		}
#endif

		if (skip_as != TRUE || last_phase == P_be || keep_flag) {
			/* create .s file */
			add_string(args, "-s");
			current_phase = P_be;
			if (last_phase == P_be && outfile != NULL)
				input_source = outfile;
			else {
#ifdef TARG_NVISA
				input_source = construct_name(the_file,"ptx");
#else
				input_source = construct_name(the_file,"s");
#endif
			}
			if (last_phase == P_be || keep_flag) {
			  sprintf(buf, "-fs,%s", input_source);
			} else {
			  char *s = concat_strings(input_source, ".s");
			  mark_for_cleanup(s);
			  sprintf(buf, "-fs,%s.s", input_source);
			}
			add_string(args, buf);
		}
		if (skip_as == TRUE && last_phase != P_be) {
			/* generate object file directly */
			current_phase = P_any_as;
			/* cc -c -o <file> uses <file> rather than .o */
		        if (outfile != NULL
					&& last_phase == current_phase
	 				&& !multiple_source_files)
			  {
			    sprintf(buf, "-fo,%s", outfile);
			  } else {
			        sprintf(buf, "-fo,%s", 
			            construct_given_name(the_file,"o",
					(keep_flag || multiple_source_files || ((shared == RELOCATABLE) && (ipa == TRUE))) ? TRUE : FALSE));
			  }
			  add_string(args, buf);
		}
		if (dashdash_flag)
		  add_string(args,"--");
		add_string(args, the_file);
		break;
	case P_as:
	case P_gas:
		if (source_lang == L_as &&
		    glevel >= 2) {
#ifdef TARG_SL
                  if (source_kind == S_S) {
                    add_string(args, "-gdwarf2");
                  } else
#endif
		  add_string(args, "-g");	// bug 5990
		}
#if defined(TARG_X8664)
                if (strcmp(target_cpu,"bdver1") == 0 )
                  add_string(args, "-Wa,-mtune=bdver1");
#endif
		if (dashdash_flag)
		  add_string(args,"--");
		if (show_but_not_run)
			add_string(args, "-###");
		{
		  int len;
#if defined(TARG_X8664) || defined(TARG_NVISA)
		  if( abi == ABI_N32 )
		    add_string(args, "-m32");
#elif defined(TARG_LOONGSON) || defined(TARG_MIPS) && !defined(TARG_SL) 
		if( abi == ABI_N32 )
		  add_string(args, "-mabi=n32");
		else
		  add_string(args, "-mabi=64");
#endif
#if defined(TARG_LOONGSON)
               add_string(args, "-EL");
               add_string(args, "-g0");
               switch (loongson_version) {
               case ISA_LOONGSON2e: {
                          add_string(args, "-march=loongson2e");
                          break;
                       }
               case ISA_LOONGSON2f: {
                          add_string(args, "-march=loongson2f");
                          break;
                       }
               case ISA_LOONGSON3: {
                          add_string(args, "-march=loongson3");
                          break;
                       }
               default:   add_string(args, "-march=loongson2e");
               }
#endif

		  // Add input source to args.  Append .s to input source if
		  // it doesn't already end in .s.
		  len = strlen(input_source);
		  if (input_source[len - 1] != 's' ||
		      input_source[len - 2] != '.') {
		    sprintf(buf, "%s.s", input_source);
		    add_string(args, buf);
		  } else {
		    add_string(args, input_source);
		  }
		}
		current_phase = P_any_as;
#if defined TARG_X8664 || !defined(CROSS_COMPILATION)
      		if (run_build) {
			char p[PATH_BUF_LEN];
			sprintf(p, "-B%s", get_phase_dir(index));
			add_string(args, p);
      		}
		add_string(args, "-c");		// gcc -c
#endif
#ifdef TARG_SL
                if ( use_sl5 != TRUE ) {
                  add_string(args,"-qwa2");
                }
#endif
		add_string(args, "-o");
		/* cc -c -o <file> puts output from as in <file>,
		 * unless there are multiple source files. */
		if (outfile != NULL
			&& last_phase == current_phase
	 		&& !multiple_source_files)
		{
			add_string(args, outfile);
		} else {
			// bug 2025
			// Create .o files in /tmp in case the src dir is not
			// writable.
			if (!(keep_flag ||
			     (ipa == TRUE) ||
			     remember_last_phase == P_any_as)) {
			  char *temp_obj_file = get_object_file (the_file);
			  add_string(args, temp_obj_file);
			} else
			add_string(args, construct_given_name(the_file,"o",
			  (keep_flag || multiple_source_files || ((shared == RELOCATABLE) && (ipa == TRUE))) ? TRUE : FALSE));
		}
		break;
	case P_ld:
	case P_ldplus:
		/* For C/C++:
		 * gcc invokes collect2 which invokes ld.
		 * Because the path to collect2 varies, 
		 * just invoke gcc to do the link. */

		/* add lib paths for standard libraries like libgcc.a */
		append_libraries_to_list (args);
		if (show_but_not_run)
			add_string(args, "-###");
#if defined(TARG_X8664) || defined(TARG_NVISA)
		if( abi == ABI_N32 )
		  add_string(args, "-m32");
#elif defined(TARG_LOONGSON) || defined(TARG_MIPS) && !defined(TARG_SL)
		if( abi == ABI_N32 )
		  add_string(args, "-mabi=n32");
		else
		  add_string(args, "-mabi=64");
#endif
      		if (run_build) {
			char p[PATH_BUF_LEN];
			sprintf(p, "-B%s", get_phase_dir(index));
			add_string(args, p);
      		}
		set_library_paths(args);
		if (outfile != NULL) {
			add_string(args, "-o");
			add_string(args, outfile);
        	}
		if (ftz_crt) {
			add_string(args, find_obj_path("ftz.o"));
		}
#ifdef TARG_SL
		if (!option_was_seen(O_nostdlib)) {
			add_string(args, find_crt_path("crt1.o"));
			add_string(args, find_crt_path("crti.o"));
			if (index == P_ldplus) {
				// for SL, we place all crt's in the same place
				add_string(args, find_crt_path("crtbegin.o"));
			} 
		}
#endif
		break;
	case P_ipa_link:
            if (oscale == TRUE)
                add_string(args, "-IPA:scale=ON");
	case P_collect:

#if defined(TARG_X8664) || defined(TARG_NVISA)
		if( abi == ABI_N32 ) {
		  add_string(args, "-m32");
		  add_string(args, "-m");
		  add_string(args,"elf_i386");
		}
#elif defined(TARG_MIPS) || defined(TARG_LOONGSON)
		if( abi == ABI_N32 ) {
#ifndef TARG_SL
		  add_string(args, "-mabi=n32");
		  add_string(args, "-m");
		  add_string(args, "elf32ltsmipn32");
#endif
		}
		else {
		  add_string(args, "-mabi=64");
		  add_string(args, "-m");
		  add_string(args, "elf64ltsmip");
		}
		// Pass top level library dir to ipa so that
		// it finds libraries like lib32/libc.so.6
#if !defined(ARCH_MIPS) && !defined(TARG_LOONGSON)
		add_library_dir (MIPS_CROSS_LIB_TOP_DIR);
#endif
#endif
		/* TODO: Handle MIPS here */
		/* add lib paths for standard libraries */
		append_libraries_to_list (args);

		set_library_paths(args);

		/* -shared only adds user objects, no predefined stuff */
		if ((shared != DSO_SHARED) && (shared != RELOCATABLE)
		    && ! option_was_seen(O_nostartfiles)) 
		{
			if (option_was_seen(O_pie)) {
			  add_string(args, find_crt_path("Scrt1.o"));
			  add_string(args, find_crt_path("crtbeginS.o"));
			}
			else {
			  add_string(args, find_crt_path("crt1.o"));
			  add_string(args, find_crt_path("crtbegin.o"));
			}
			add_string(args, find_crt_path("crti.o"));
			if (ftz_crt) {
				add_string(args, find_obj_path("ftz.o"));
			}
		}
                if (outfile != NULL) {
			add_string(args, "-o");
			add_string(args, outfile);
		}

		if (instrumentation_invoked == TRUE && index != P_collect) {
		  if (fb_file != NULL) 
		    sprintf(buf, "-IPA:propagate_feedback_file=%s", fb_file);
		  else if (outfile != NULL)
		    sprintf (buf, "-IPA:propagate_feedback_file=%s", outfile);
		  else
		    sprintf (buf, "-IPA:propagate_feedback_file=a.out");
		  add_string(args,buf);
		}

		if (opt_file != NULL && index != P_collect){
		  sprintf(buf, "-IPA:propagate_annotation_file=%s", opt_file);
		  add_string(args,buf);
		}
#if defined(TARG_LOONGSON)

               switch (loongson_version) {
               case ISA_LOONGSON2e: {
                          add_string(args, "-loongson2e");
                          break;
                       }
               case ISA_LOONGSON2f: {
                          add_string(args, "-loongson2f");
                          break;
                       }
               case ISA_LOONGSON3: {
                          add_string(args, "-loongson3");
                          break;
                       }
               default:   add_string(args, "-loongson2e");
               }
#endif

		/* object file should be in list of options */
		break;
	case P_cord:
		/* specify the output file */
		if (outfile != NULL)
			{
			add_string(args, "-o");
			add_string(args, outfile);
			}
		else
			{
			add_string(args, "-o");
			add_string(args, "a.out");
			}
		/* specify the input file */
		add_string(args, create_temp_file_name("C"));
		/* now specify the feedback file(s) */
		if (feedback_files->head) {
			/* has user specified feedback files */
			append_string_lists(args, feedback_files);
			}
		else if (outfile != NULL) {
			/* feedback file is output file + .fb */
			add_string(args, concat_strings(outfile,".fb"));
			}
		else {
			/* feedback file is a.out.fb */
			add_string(args, "a.out.fb");
		}
		break;
          case P_pixie:
		/* Specify the output file */
		if (outfile != NULL) {
		   add_string(args, "-pixie_file");
		   if (fb_xdir != NULL) {
		     temp = concat_strings(fb_xdir,  "/");
		     temp = concat_strings(temp, drop_path(outfile));
		     add_string(args, temp);
                   } else {
		     add_string(args, outfile);
                   }
	           count_file_name = concat_strings(drop_path(outfile), ".x.Counts");
                } else {
		   add_string(args, "-pixie_file");
		   if (fb_xdir != NULL) {
		      temp = concat_strings(fb_xdir,  "/");
		      temp = concat_strings(temp, "a.out");
		      add_string(args, temp);
                   } else {
		      add_string(args, "a.out");
                   }
		   count_file_name = concat_strings("a.out", ".x.Counts");
                }
		if (fb_xdir != NULL) {
		   add_string(args, "-directory");
		   add_string(args, fb_xdir);
		   add_string(args, "-rpath");
		   add_string(args, fb_xdir);
                }
		if (fb_cdir != NULL) {
		   add_string(args, "-counts_file");
		   temp = concat_strings(fb_cdir, "/");
		   temp = concat_strings(temp, count_file_name);
		   add_string(args, temp);
                }
		if (ldpath_for_pixie != NULL) {
		   add_string(args, "-ldpath");
		   add_string(args, ldpath_for_pixie);
                }
                add_string(args, input_source);
		break;
         case P_prof:
		/* Generate the .fb files */
		add_string(args, "-pixie");
		add_string(args, "-feedback");
		if (prof_file != NULL) {
                  add_string(args, concat_strings(prof_file, ".x"));
                } else {
		   internal_error("No count file was specified for a prof run");
		   perror(program_name);
                }
		for (p = count_files->head; p != NULL; p = p->next) 
		   add_string(args, p->name);
                add_string(args, ">");
		add_string(args, "/dev/null");
		break;
	}
}

/*
 * You'd think it would be easy to figure out the name of libgcc_s,
 * but noooooo.  The gcc developers, in their infinite wisdom, call it
 * libgcc_s for a native compiler, but libgcc_s_32 for a
 * 64-bit-to-32-bit cross compiler.
 */
static void
get_libgcc_s_name(char **libgcc_s_std, char **libgcc_s_dir32)
{
	// Return the standard name in LIBGCC_S_STD.  For 64-bit-to-32-bit
	// cross compiler, libgcc_s may appear under a different name in a "32"
	// dir.  Return that name in LIBGCC_S_DIR32.

	if (abi == ABI_N32 && platform_is_64bit()) {
		int v = get_gcc_major_version();
		if (v < 4) {	// bug 11407
		  *libgcc_s_std = "gcc_s_32";
		  *libgcc_s_dir32 = "gcc_s";
		  return;
		}
	}
	*libgcc_s_std = "gcc_s";
	*libgcc_s_dir32 = NULL;
}

/*
 * Oh, and did we mention that most of gcc's little helpers go into
 * the <blah>/32 directory, with the sole exception of libgcc_s_32.so?
 * It goes in the <blah> directory.  Ya gotta love it, folks.
 *
 * But wait - there's more!  Red Hat tuck libgcc_s.so away under
 * gcc-lib somewhere, but SuSE keep it in /usr/lib{,64}.
 */
static void
add_libgcc_s(string_list_t *args)
{
	static char *libgcc_s = NULL;
	char *libgcc_s_std, *libgcc_s_dir32;
	static int path_set;
	string_item_t *p;
	char *name = NULL;

	get_libgcc_s_name(&libgcc_s_std, &libgcc_s_dir32);

	// This function may be invoked multiple times, so we only set
	// the search path once, but add the -l part whenever
	// required.

	if (!path_set) {
		for (p = get_library_dirs()->head; p != NULL; p = p->next) {
			free(name);
			asprintf(&name, "%s/lib%s.so", p->name, libgcc_s_std);
			if (file_exists(name)) {
				add_arg(args, "-L%s", p->name);
				libgcc_s = libgcc_s_std;
				path_set = 1;
				break;
			}

			free(name);
			asprintf(&name,"%s/../lib%s.so", p->name, libgcc_s_std);
			if (file_exists(name)) {
				add_arg(args, "-L%s/..", p->name);
				libgcc_s = libgcc_s_std;
				path_set = 1;
				break;
			}

			// For 64-bit-to-32-bit cross compiler, look under "32"
			// dir.  Bug 8637.
			if (libgcc_s_dir32 != NULL &&
			    // Assumes p->name always end in '/'.
			    strstr(p->name, "/32/") != NULL) {
				free(name);
				asprintf(&name,"%s/lib%s.so", p->name,
					 libgcc_s_dir32);
				if (file_exists(name)) {
					add_arg(args, "-L%s", p->name);
					libgcc_s = libgcc_s_dir32;
					path_set = 1;
					break;
				}
			}
		}

		free(name);

		// It's not an error if we don't find the library,
		// because different distros keep it in different
		// places.

		if (libgcc_s == NULL)
			libgcc_s = libgcc_s_std;
	}
	
	add_library(args, libgcc_s);
}

static void
add_final_ld_args (string_list_t *args, phases_t ld_phase)
{
#ifdef TARG_X8664 
        extern boolean link_with_mathlib;
	if (option_was_seen(O_nodefaultlibs) || option_was_seen(O_nostdlib)) {
	    return;
	}
#endif
#ifdef TARG_SL
	if (option_was_seen(O_nodefaultlibs) || option_was_seen(O_nostdlib)) {
		// link script for various SL systems
		char *cmd_path;
		char *cmp_tgt_root = NULL;
		if (ldscript_file) {
			cmp_tgt_root = getenv("LINK_SCRIPT");
			if (cmp_tgt_root == NULL) {
				error("Environment var LINK_SCRIPT not set");
			}
			else {
				add_string(args, "-T");
				asprintf(&cmd_path, "%s", cmp_tgt_root);
				add_string(args, cmd_path);
			}
		} else {
			error("No link script");
		}
		/* add soft math libray: libsl1m.a */
		if ((Long_Long_Support == TRUE) || (Float_Point_Support == TRUE))
		{
		    if (use_sl5 == TRUE) {
		        /* Support -mlong-long option for uclibc error as longlong mapping to long (ShenRuifen) */
		        //error("long long/float/double type is not supported for SL5 processor");          
		    } else {
		        add_string(args, "-lsl1m");
		    }
		}
    return;
  }
#endif // SL 

	if (shared != RELOCATABLE) {
	    if (invoked_lang == L_f90) {
        /*
		if (!option_was_seen(O_shared)) {
			add_library(args, PSC_NAME_PREFIX "fstart");
		}
		add_library(args, PSC_NAME_PREFIX "fortran");
		if (!option_was_seen(O_shared)) {
			add_library(args, PSC_NAME_PREFIX "fstart");
		}
		*/
		add_library(args, "fortran");
		add_string(args, "-lmv");
	//	add_string(args, "-lm" PSC_NAME_PREFIX);
		add_string(args, "-lm");
#ifdef TARG_X8664
		if (abi != ABI_N32)
                    add_library(args, "acml_mv");
#endif
		add_library(args, "mv");
	//	add_library(args, "m" PSC_NAME_PREFIX);
		add_library(args, "m");
		add_library(args, "ffio");
	//	add_library(args, "msgi");
	    }
	    if (option_was_seen(O_mp) ||
		option_was_seen(O_apo) ||	// bug 6334
		option_was_seen(O_fopenmp)) {
                add_string(args, "-lopenmp");
            }

            if (option_was_seen (O_fprofile_arcs))
                add_string(args, "-lgcov");    // bug 12754
	    if (option_was_seen(O_pthread) ||
		option_was_seen(O_mp) ||
		option_was_seen(O_fopenmp) ||
		option_was_seen(O_apo)) {	// bug 6334
		add_string(args, "-lpthread");
	    }
	}

#ifdef TARG_X8664
	// Put open64rt after all the libraries that are built with PathScale
	// compilers, since those libraries could use PathScale routines.
	// Bug 3995.
	if (!option_was_seen(O_fno_fast_stdlib) &&
	    !option_was_seen(O_nolibopen64rt)) {	// bug 9611
            if (option_was_seen(O_shared) || option_was_seen(O_pie) ||
                option_was_seen(O_fpie) || option_was_seen(O_fPIE)) {
                add_library(args, "open64rt_shared");
            } else {
                add_library(args, "open64rt");
            }
	}
#endif

#ifdef TARG_IA64
    /* if ld_phase != P_collet, gcc or g++ is used for the link purpose,
     * lib{stdc++,gcc,c,etc}.{so,a} are taken care by gcc/g++ itself.
     */
    if (shared != DSO_SHARED && shared != RELOCATABLE &&
        ld_phase != P_ld && ld_phase != P_ldplus) {
            if (invoked_lang == L_CC) {
            add_library(args, "stdc++");
            }
        add_library (args, "gcc");
        add_library (args, "c");
        add_library(args, "gcc");
    }
#elif !defined(TARG_SL)
	if (ipa == TRUE) {
	    	if (invoked_lang == L_CC) {
			add_library(args, "stdc++");
	    	}
		if (invoked_lang == L_CC && !option_was_seen(O_static)) {
			add_libgcc_s (args);
		}
		add_library (args, "gcc");
		add_library (args, "c");
		if (invoked_lang == L_CC && !option_was_seen(O_static))
			add_libgcc_s (args);
		add_library(args, "gcc");
	}
#endif
	if (shared != RELOCATABLE) {
	  if ( fbuiltin != 0 ) {
#ifndef TARG_SL
	    /* Once -fbuiltin is used, some functions, i.e., __sincos, are only
	       provided by libmblah.a lib.
	    */
	    if (invoked_lang != L_cc) {
#ifdef TARG_X8664
	      if (abi != ABI_N32) {
		/* Sigh, g++ removes the first -lm since it is implicitly added by g++,
		 * however adding two instances of -lm only removes one.
		 */
                if (invoked_lang == L_CC)
		add_library(args, "acml_mv");
		  add_library(args, "m");
              }
#endif
	      add_library(args, "mv");			// bug 5527
	      // add_library(args, "m" PSC_NAME_PREFIX);	// bug 3092
              // OSP -lm is needed
              add_library(args, "m");
	    }
#else
			if (invoked_lang == L_CC)
				add_string(args, "-lstdc++");
			if ((Long_Long_Support == TRUE) || (Float_Point_Support == TRUE)) {
			  if (use_sl5 == TRUE) {    
				/* Support -mlong-long option for uclibc error as longlong mapping to long (ShenRuifen) */
				add_string(args, "-lc");
				// error("long long/float/double type is not supported for SL5 processor");          
			  } else {
				/* add uclibc libray with float/double/long long supporting: libcx.a */
				add_string(args, "-lcx");

				/* add soft math libray: libsl1m.a */
				add_string(args, "-lsl1m");
			  }
			} else {
				/* add uclibc libray without float/double/long long supporting: libc.a */
				add_string(args, "-lc");
			}

			// link script for various SL systems
			char *cmd_path;
			char *cmp_tgt_root = NULL;
			if (ldscript_file) {
				cmp_tgt_root = getenv("LINK_SCRIPT");
				if (cmp_tgt_root == NULL) {
					error("Environment var LINK_SCRIPT not set");
				}
				else {
					add_string(args, "-T");
					asprintf(&cmd_path, "%s", cmp_tgt_root);
					add_string(args, cmd_path);
				}
			}
			else {
				add_string(args, "-T");
				cmp_tgt_root = getenv("COMP_TARGET_ROOT");
				if (use_sl1_dsp == TRUE)
					asprintf(&cmd_path, "%s/usr/lib/ldscripts/sl1-bb-common.ld", cmp_tgt_root);
				else if (use_sl5 == TRUE)
					asprintf(&cmd_path, "%s/usr/libsl5/ldscripts/sl5-common.ld", cmp_tgt_root);
				else // use_sl1_pcore or default target
					asprintf(&cmd_path, "%s/usr/lib/ldscripts/sl1-core-common.ld", cmp_tgt_root);
				add_string(args, cmd_path);
            }

            if (invoked_lang == L_CC) {
              add_string(args, find_crt_path("crtend.o"));
            }
            add_string(args, find_crt_path("crtn.o"));

#endif
	  }
	}
#ifdef TARG_IA64
    if (shared != DSO_SHARED && shared != RELOCATABLE &&
        ld_phase != P_ld && ld_phase != P_ldplus) {
        add_string(args, find_crt_path("crtend.o"));
        add_string(args, find_crt_path("crtn.o"));
    }
#else
	if (ipa == TRUE) {
	  if (shared != DSO_SHARED && shared != RELOCATABLE) {
	    if (option_was_seen(O_pie))
	      add_string(args, find_crt_path("crtendS.o"));
	    else
	      add_string(args, find_crt_path("crtend.o"));
#ifndef TARG_SL
	    add_string(args, find_crt_path("crtn.o"));
#endif
	  }
	}
#endif
}

#ifdef TARG_IA64
static void
postprocess_ld_args (string_list_t *args)
{
    string_item_t *p;

    if (option_was_seen(O_pg) && !option_was_seen(O_nostdlib)) {
    if (prof_lib_exists("c"))
        add_library(args, "c");
    }
}

static void
add_rpath_link_option (string_list_t *args) {

    phases_t ld_phase = determine_ld_phase (FALSE);
    if (ld_phase == P_ld || ld_phase == P_ldplus) {
        return; // gcc/g++ will take care
    }

    add_string (args,"-rpath-link");
    add_string (args,get_phase_dir(P_alt_library));
    /*
     * For some reason, our cross linker won't find libraries in some
     * directories unless it's told about them with -rpath-link.
     * Here, we scan all -L flags
     * and pass them as -rpath-link flags,
     * too.
     */
    for (string_item_t* p = args->head; p != NULL; p = p->next) {
        char *dir = NULL;
        if (strncmp(p->name, "-L", 2))
            continue;

        if (strlen(p->name) > 2) {
            dir = p->name + 2;
        } else if (p->next) {
            dir = p->next->name;
        }

        add_after_string(args, p, dir);
        add_after_string(args, p, "-rpath-link");
    }
}
#else
static void
postprocess_ld_args (string_list_t *args)
{
    string_item_t *p;
    boolean add_huge_lib = FALSE;
    boolean do_link = FALSE;

    if (option_was_seen(O_pg) && !option_was_seen(O_nostdlib)) {
	if (prof_lib_exists("c"))
	    add_library(args, "c");
    }

    /*
     * When building the compiler's libraries, do not append any rpath
     * options since these would reference the build directory.
     */
    if (run_build)
        return;

    /*
     * For some reason, our cross linker won't find libraries in some
     * directories unless it's told about them with -rpath-link.
     * Here, we scan all -L flags and pass them as -rpath-link flags,
     * too.
     */

    for (p = args->head; p != NULL; p = p->next) {
	char *dir = NULL;
	if (strncmp(p->name, "-L", 2))
	    continue;
	
	if (strlen(p->name) > 2) {
	    dir = p->name + 2;
	}
	else if (p->next) {
	    dir = p->next->name;
	}
	if (dir) {
            char * root_prefix = directory_path(get_executable_dir());
            add_after_string(args, p, concat_strings("-Wl,-rpath-link,", dir));

            if (strstr(dir, root_prefix) != NULL) {

                add_after_string(args, p, concat_strings("-Wl,-rpath,", dir));

                if (option_was_seen(O_HP) && instrumentation_invoked != TRUE) {
                    HUGEPAGE_DESC desc;

                    for (desc = hugepage_desc; desc != NULL; desc = desc->next) {
                        if ((desc->alloc == ALLOC_BD
			     || desc->alloc == ALLOC_BDT)
			    && !do_link) {
                            /* libhugetlbfs linker script only supports dynamic link. 
                             */
                            if (!option_was_seen(O_static)) {
				if (desc->alloc == ALLOC_BD) {
				    if (desc->size == SIZE_2M)
					dir = concat_strings(dir, "/elf.xBD");
				    else if (desc->size == SIZE_1G)
					dir = concat_strings(dir, "/elf_1G.xBD");
				}
				else {
				    if (desc->size == SIZE_2M)
					dir = concat_strings(dir, "/elf.xBDT");
				    else if (desc->size == SIZE_1G)
					dir = concat_strings(dir, "/elf_1G.xBDT");
				}
                                
                                add_after_string(args, p, concat_strings("-Wl,-T", dir));
                                do_link = TRUE;
                                add_huge_lib = TRUE;
                            }
                        }
                        else if (desc->alloc == ALLOC_HEAP)
                            add_huge_lib = TRUE;
                    }

                    if (add_huge_lib && option_was_seen(O_static))
                        add_after_string(args, p, "-Wl,--undefined=setup_libhugetlbfs");
                }
            }
	}
    }

    if (add_huge_lib) {
        add_library(args, "hugetlbfs_open64");
    }
}
#endif

#define MAX_PHASE_ORDER 10
static phases_t phase_order[MAX_PHASE_ORDER];
static int phase_order_index = 0;

static phases_t be_phase = P_be;

static void
add_phase (phases_t p)
{
	phase_order[phase_order_index] = p;
	phase_order_index++;
	if (phase_order_index >= MAX_PHASE_ORDER)
		internal_error("too many phases");
}

/* determine which of inline, ipl, or be should be executed after the front
   end */
/* Regarding P_inline, change the semantics of post_fe_phase to mean
   (bug 11325):
    1)  If post_fe_phase is called before the front-end executes, then it
	determines if the inline *could* be run.
    2)  If post_fe_phase is called after the front-end executes, then it
	determines if the inliner will actually run (or already did run).
*/
static phases_t
post_fe_phase (void)
{
    if (ipa == TRUE)
      return P_ipl;
    // run_inline is the final gating variable that controls whether or not to
    // run the inliner.  It is UNDEFINED before the front-end is run.
    // Afterward, it is set to inline_t if the inline setting is explicitly
    // given on the command line; otherwise it is set to the inline request
    // from the front-end.  Bug 11325
    else if (run_inline != UNDEFINED)
      return run_inline == TRUE ? P_inline : be_phase;
    else if (inline_t == TRUE || inline_t == UNDEFINED)
	return P_inline;
    else
	return be_phase;
} /* post_fe_phase */
    
/* If -INLINE:%s option was seen, pass it to ld if ipa run, or inline if
   stand-alone inliner run */

static void
add_inline_option(void)
{
  if (option_was_seen(O_INLINE_) || ( inline_t!= FALSE)) {
    switch (post_fe_phase()) {
    case P_ipl:
      add_phase_for_option(O_INLINE_, P_ipa_link);
      break;
    case P_inline:
      add_phase_for_option(O_INLINE_, P_inline);
      break;
    }
  }
}

static void
determine_phase_order (void)
{
	phases_t next_phase = P_NONE;
	phases_t cpp_phase;
	phases_t asm_phase;
	phases_t link_phase;
	phase_order[0] = P_NONE;
	phase_order_index = 0;

#ifdef TARG_NVISA
        if (option_was_seen(O_multicore)) {
	  be_phase = P_bec;
	}
#endif
 
	/* determine which cpp to use */
	if (source_lang == L_CC) {
		if (option_was_seen(O_usegfe)) {
			cpp_phase = P_gcpp_plus;
		} else {
			cpp_phase = P_cplus_cpp;
		}
	} else if (source_lang == L_cc) {
		if (option_was_seen(O_usegfe)) {
			cpp_phase = P_gcpp;
		} else {
			cpp_phase = P_c_cpp;
		}
	} else if (source_lang == L_f77) {
		if (option_was_seen(O_mp)) {
			/* power Ftn */
			cpp_phase = P_cpp;	/* default */
		} else {
			cpp_phase = P_f_cpp;
		}
	} else if (use_coco == TRUE) {		// bug 9058
		cpp_phase = P_f_coco;
	} else if (source_lang == L_f90) {
	   if (option_was_seen(O_ftpp)) {
	      cpp_phase = P_cppf90_fe;
	   } else if (option_was_seen(O_cpp)
		|| option_was_seen(O_P) 
		|| option_was_seen(O_E)
		|| (!option_was_seen(O_nocpp) &&
		    (source_kind == S_F || source_kind == S_F90)))
	   {
	      if (option_was_seen(O_usegfe))
	      	cpp_phase = P_gcpp; 
	      else
	      	cpp_phase = P_cpp; 
	   } else {
	      cpp_phase = P_NONE;
	   }
	} else if (source_lang == L_as
		&& (abi == ABI_I32 || abi == ABI_I64 || abi == ABI_IA32))
	{
		cpp_phase = P_gcpp;	/* use ansi-style cpp */
	} else if (option_was_seen(O_usegfe)) {
			cpp_phase = P_gcpp;
	} else {
			cpp_phase = P_cpp;
	}

	if (last_phase == P_any_cpp) {
		add_phase(cpp_phase);
		return;
	}

	/* determine which asm to run */
	asm_phase = P_gas;

	/* determine which linker to run */
	if (ipa == TRUE)
		link_phase = P_ipa_link;
	else if (invoked_lang == L_CC)
		link_phase = P_ldplus;
	else
		link_phase = P_ld;

#if !defined(TARG_NVISA) /* nvisa doesn't use spin yet */

	phases_t c_fe = (gnu_major_version == 4) ? P_spin_cc1 : P_c_gfe;
	phases_t cplus_fe = (gnu_major_version == 4) ? P_spin_cc1plus : P_cplus_gfe;
#else
	phases_t c_fe = P_c_gfe;
	phases_t cplus_fe = P_cplus_gfe;
#endif

	switch (source_kind) {
	case S_c:
	case S_C:
		if (first_phase != P_any_cpp) {
		    next_phase = (source_lang == L_CC ? cplus_fe : c_fe);
		} else {
		    if (source_lang == L_CC)
			add_phase(P_gcpp_plus);
		    else
		    	add_phase(P_gcpp);
		    next_phase = (source_lang == L_CC ? cplus_fe : c_fe);
		}
		break;
	case S_i:
	case S_ii:
		if (source_lang == L_f77)
			next_phase = P_f_fe;
		else if (source_lang == L_f90)
			next_phase = P_f90_fe;
		else if (source_lang == L_as)
			next_phase = asm_phase;
		else if (source_lang == L_CC)
			next_phase = cplus_fe;
		else if (source_lang == L_cc)
			next_phase = c_fe;
		else if (source_kind == S_ii)
			next_phase = cplus_fe;
		else
			next_phase = c_fe;
		break;
	case S_r:
		if (run_m4) add_phase(P_m4);
		add_phase(P_ratfor);
		/* FALLTHRU */
        case S_f:
        case S_f90:
		if (cpp_phase == P_NONE) {
			next_phase = P_f90_fe;
		} else if (cpp_phase == P_cppf90_fe) {
			/* invoke combined cpp/fe phase */
			next_phase = P_cppf90_fe;
		}
		else {
			add_phase(cpp_phase);
			next_phase = P_f90_fe;
		}
		break;
	case S_F:
	case S_F90:
		if ( option_was_seen(O_nocpp)) {
			next_phase = P_f90_fe;
		} else if (cpp_phase == P_NONE || cpp_phase == P_cppf90_fe) {
			/* invoke combined cpp/fe phase */
			next_phase = P_cppf90_fe;
		} else {
			add_phase(cpp_phase);
			next_phase = P_f90_fe;
		}
		break;
	case S_s:
		if (option_was_seen(O_cpp)) {
			add_phase(cpp_phase);
		}
		next_phase = asm_phase;
		break;
	case S_S:
		if ( ! option_was_seen(O_nocpp)) {
			add_phase(cpp_phase);
		}
		next_phase = asm_phase;
		break;
	case S_B:
		next_phase = post_fe_phase ();
		break;
	case S_I:
	case S_P:
	case S_N:
	case S_O:
		next_phase = be_phase;
		break;
	case S_o:
		next_phase = link_phase;
		break;
	}

	while (next_phase != P_NONE) {
		if (last_phase < next_phase) {
			add_phase(P_NONE);
			next_phase = P_NONE;
		}
		switch (next_phase) {
		case P_pfa:
			add_phase(next_phase);
			next_phase = P_cppf_fe;
			break;
		case P_f_fe:
		case P_cppf_fe:
		case P_lister:
		case P_c_gfe:
		case P_cplus_gfe:
			add_phase(next_phase);
			next_phase = post_fe_phase ();
			break;
		case P_spin_cc1:
		case P_spin_cc1plus:
			add_phase(next_phase);
			next_phase = P_wgen;
			break;

		case P_wgen:
			add_phase(next_phase);
			next_phase = post_fe_phase ();
			break;
		case P_f90_fe:
                case P_cppf90_fe:
			if (keep_listing) {
			   add_phase(next_phase);
			   next_phase = P_lister;
                        } else {
			   add_phase (next_phase);
			   next_phase = post_fe_phase ();
                        }
			break;
		case P_inline:
			add_phase (next_phase);
			next_phase = be_phase;
			break;
		case P_ipl:
			add_phase(next_phase);
			if (option_was_seen(O_ar)) {
			    next_phase = P_ar;
			}
			else {
			    next_phase = link_phase;
			}
			break;
		case P_be:
			add_phase(next_phase);
			/* may or may not generate objects directly */
			if (skip_as == TRUE) {
			   if (option_was_seen(O_ar)) {
			       next_phase = P_ar;
			   }
			   else {
			       next_phase = link_phase;
			   }
			}
			else next_phase = asm_phase;
			break;
#if defined(TARG_NVISA)
		case P_bec:
			add_phase(next_phase);
			next_phase = P_NONE;
			break;
#endif
		case P_as:
		case P_gas:
			add_phase(next_phase);
			if (option_was_seen(O_ar)) {
			    next_phase = P_ar;
			}
			else {
			    next_phase = link_phase;
			}
			break;
		case P_ld:
		case P_ldplus:
		case P_collect:
		case P_ipa_link:
			add_phase(next_phase);
                        if (cordflag==TRUE) {
			   next_phase = P_cord;
			} else {
			  add_phase(P_NONE);
			  next_phase = P_NONE;
			}
			break;
		case P_cord:
			add_phase(next_phase);
			add_phase(P_NONE);
			next_phase = P_NONE;
			break;
		case P_NONE:
			break;
		default:
			internal_error("unexpected phase order");
		}
	}
}

#ifdef _WIN32
#define EXE ".exe"
#define DSO ".dll"
#else
#ifdef __CYGWIN__
#define EXE ""
#define DSO ".dll"
#else /* linux */
#define EXE ""
#define DSO ".so"
#endif
#endif

static void
check_existence_of_phases (void)
{
    int i;
    for (i = 0; phase_order[i] != P_NONE; i++) {
	int give_warning = FALSE;
	switch (phase_order[i]) {

	case P_pfa:
	    if (!file_exists(get_full_phase_name(phase_order[i]))) {
		error("Power Fortran is not installed on this system");
	    }
	    break;

	case P_mpc:
	    /* pca also invokes mpc, so just check mpc */
	    if (!file_exists(get_full_phase_name(phase_order[i]))) {
		error("Power C is not installed on this system");
	    }
	    break;

			
	    /* check if be phase exists, to warn about wrong toolroot */
	case P_ipl:
	    if (!file_exists (concat_strings (get_phase_dir(phase_order[i]),
#ifndef SHARED_BUILD
					      "/ipl"EXE)))
#else
					      "/ipl"DSO)))
#endif
		give_warning = TRUE;

	    /* fall through */
	    
	case P_be:

	    if (!file_exists (concat_strings (get_phase_dir(phase_order[i]),
#ifndef SHARED_BUILD
                                            "/be"EXE)))
#else
                                            "/be"DSO)))
#endif
		give_warning = TRUE;

	    if (!file_exists(get_full_phase_name(phase_order[i])))
		give_warning = TRUE;

	    if (give_warning)
		warning ("%s does not contain all of the Open64 compiler"
			 " phases.", get_phase_dir(phase_order[i])); 
	    break;
	}
    }
}

static void
add_instr_archive (string_list_t* args)
{
  extern int profile_type;

  /* Add instrumentation archives */
  if (instrumentation_invoked != UNDEFINED && instrumentation_invoked) {

    unsigned long f = WHIRL_PROFILE | CG_EDGE_PROFILE | CG_VALUE_PROFILE |
      CG_STRIDE_PROFILE ;
    if (!(profile_type & ~f)) {
      if (profile_type & (CG_EDGE_PROFILE |
			  CG_VALUE_PROFILE | CG_STRIDE_PROFILE)) {
	add_library (args,"cginstr");
      }

      add_library (args, "instr");
#ifndef TARG_IA64
      if (!option_was_seen(O_static))
	add_libgcc_s (args);
#endif
    } else {
      fprintf (stderr, "Unknown profile types %#lx\n", profile_type & ~f);
    }
  }
}


extern char *get_binutils_lib_path(void);


static char *
find_toolroot(char *program_name, char *toolroot)
{
	char buf[PATH_BUF_LEN];
	FILE* fp;
	int tail;

#ifdef _WIN32
        if (is_absolute_file_name(program_name)) {
	    strcpy (toolroot, directory_path (program_name));
        }
	else {
		return NULL;
	}
#else
	if(strchr(program_name, '/') == NULL) {
		/* find arg0 in $PATH by cmd "which" */
		sprintf(buf, "dirname \"`which %s`\"", program_name);
	} else {
		sprintf(buf, "dirname \"%s\"", program_name);
	}

	if((fp = popen(buf, "r")) == NULL)
		return NULL;

	toolroot[0] = '\0';
	while(fgets(buf, PATH_BUF_LEN, fp) != NULL) {
		strcat(toolroot, buf);
	}
	pclose(fp);
#endif

	tail = strlen(toolroot);
	if(toolroot[tail - 1] == '\n') {
		toolroot[tail - 1] = '\0';
	}

	strcat(toolroot, "/..");
	
	/* do we need realpath? */
	strcpy(toolroot, realpath(toolroot, buf));
	
	return toolroot;
}

static char ld_library_path_found[PATH_BUF_LEN];

void
init_phase_info (void)
{
	char *toolroot;
	char *comp_target_root;

	if (getenv("_XPG") != NULL) 
	   xpg_flag = TRUE;
	ld_library_path = getenv("LD_LIBRARY_PATH");
	ld_libraryn32_path = getenv("LD_LIBRARYN32_PATH");
	old_ld_library_path = string_copy(ld_library_path);
	// Replace ":" with ";" because ":" has special meaning to -INTERNAL.
	if (old_ld_library_path) {
	  int i;
	  for (i=0; i<strlen(old_ld_library_path); i++) {
	    if (old_ld_library_path[i] == ':')
	      old_ld_library_path[i] = ';';
	  }
	  asprintf(&old_ld_library_path, "%s;%s", old_ld_library_path,
		   get_binutils_lib_path());
	} else {
	  old_ld_library_path = get_binutils_lib_path();
	}

	global_toolroot = getenv("TOOLROOT");

#ifndef TARG_NVISA /* already used program name in set_executable_dir */
	if (global_toolroot == NULL) {
		global_toolroot = (char *)malloc(PATH_BUF_LEN);
		if(find_toolroot(orig_program_name, global_toolroot) == NULL) {
			free(global_toolroot);
			global_toolroot = NULL;
		}
	}
#endif

	if (global_toolroot != NULL) {
		/* add toolroot as prefix to phase dirs */
#ifdef TARG_NVISA
		/* only prefix open64 phases, as gcc tools have separate path */
                prefix_all_phase_dirs(OPEN64_PHASE_MASK, global_toolroot);
#else
                prefix_all_phase_dirs(PHASE_MASK, global_toolroot);
#endif
	} 

#ifdef TARG_IA64
    get_phases_real_path ();
#endif
	comp_target_root = getenv("COMP_TARGET_ROOT");
	if (comp_target_root != NULL) {
		/* add comp_target_root as prefix to phase dirs */
                prefix_all_phase_dirs(LIB_MASK, comp_target_root);
	}
}

// Add prefix to the gcc name according to how keycc is invoked.  If keycc is
// invoked as mips64el-key-linux-keycc, then gcc is mips64el-key-linux-gcc.
void
init_phase_names (void)
{
#if !defined(_WIN32)
  char *prefix, *cmd, *x;
  char path[MAXPATHLEN];
  int i, j, len, rval;

  /* Look in this special place for a link to the executable. This
     only works on Linux, but it should work since keycc runs only on Linux. */
  rval = readlink ("/proc/self/exe", path, sizeof(path));
  if (rval <= 0) {
    strncpy(path, orig_program_name, sizeof(path));
    rval = strlen(path);
  } else {
    path[rval] = '\0';		// readlink doesn't append NULL
  }

  // Extract command from command path.  If command path is
  // /foo/mips64el-key-linux-keycc, then command is mips64el-key-linux-keycc.
  for (i = strlen(path)-1; i >= 0; i--) {
    if (path[i] == '/')
      break;
  }
  if (i < 0) {
    // '/' not found.  path is the command itself.
    cmd = path;
  } else {
    // Found '/'.  Command begins at the first char after '/'.
    cmd = path + i + 1;
  }

  // Get the prefix of the command.  Examples:
  //   command:  mips64el-key-linux-keycc
  //   prefix:   mips64el-key-linux-
  //
  //   command:  mips64el-key-linux-keycc-
  //   prefix:   mips64el-key-linux-
  prefix = strdup(cmd);
  #ifdef PSC_TO_OPEN64
  if ((x = strstr(prefix, "-" OPEN64_FULL_VERSION))) {
  #endif
    *x = '\0';
  }
  // Skip all trailing '-', if any.
  for (i = strlen(prefix)-1; i >= 0 && prefix[i] == '-'; i--);
  // Skip the last group of non-'-' chars.
  for (j = i; j >= 0; j--) {
    if (prefix[j] == '-') {
      prefix[j+1] = '\0';
      break;
    }
  }
  if (j < 0)
    prefix[0] = '\0';	// no prefix

  for (i = P_LAST-1; i >= (int) P_NONE; i--) {
    char *phase_name = get_phase_name(i);
    if (strcmp(phase_name, "gcc") == 0 ||
        strcmp(phase_name, "g++") == 0) {
      set_phase_name (i, concat_strings(prefix, phase_name));
    }
  }
#endif
}

// Change the front-end names to reflect the GNU version.
void
init_frontend_phase_names (int gnu_major_version, int gnu_minor_version)
{
  // Select the appropriate GNU 4 front-end.
  if ((gnu_major_version == 4) && !run_build) {
    switch (gnu_minor_version) {
      case 2:
	set_phase_name(P_spin_cc1, "cc142");
	set_phase_name(P_spin_cc1plus, "cc1plus42");
	set_phase_name(P_wgen, "wgen42");
	break;
      default:
        error("no support for GNU 4.%d front-end", gnu_minor_version);
    }
  }
}

void
run_dsm_prelink(void)
{
	string_list_t *args = init_string_list();
	copy_phase_options(args, P_dsm_prelink);
	append_objects_to_list(args);
 	run_phase(P_dsm_prelink, get_full_phase_name(P_dsm_prelink), args);
}

/* Choose correct linker */
phases_t
determine_ld_phase (boolean run_ipa) {
        phases_t ldphase;
        if (run_ipa) {
                ldphase = P_ipa_link;
        }
#if defined TARG_IA64 && defined CROSS_COMPILATION
        /* We use prebuilt ld to link objects for cross compiler,
         * TODO: build cross toolchains, let cross gcc to take care the link.
         */
        else {
                ldphase = P_collect;
        }
#else
        else if (invoked_lang == L_CC) {
                ldphase = P_ldplus; // let g++ let care
        }
#ifdef TARG_IA64 
        else if (invoked_lang == L_cc) {
                ldphase = P_ld;     // let gcc let care
        } 
        else {
                // using ld directly so we have more control on the link-phase.
                ldphase = P_collect;
        }
#else
        else {
            	ldphase = P_ld;     // let gcc let care
        }
#endif
#endif
        return ldphase;
}

void
run_ld (void)
{
	phases_t ldphase;
	char *ldpath;
	string_list_t *args = init_string_list();

	if (ipa == TRUE) {
		ldphase = P_ipa_link;
	}
	else if (invoked_lang == L_CC) {
		ldphase = P_ldplus;
	}
	else {
		ldphase = P_ld;
	}

	// Pass "-m elf_i386" and "-m elf_x86_64" to linker.  Bug 8441.
	if (option_was_seen(O_melf_i386)) {
	    add_string(args, "-m");
	    add_string(args, "elf_i386");
	}
	if (option_was_seen(O_melf_x86_64)) {
	    add_string(args, "-m");
	    add_string(args, "elf_x86_64");
	}

#ifdef BUILD_SKIP_IPA
        if (ipa == TRUE) {
            error("IPA support is not enabled in this compiler.");
            return;
        }
#endif

	if (ipa == TRUE) {
	    char *str;
	    ldpath = get_phase_dir (ldphase);
	    ldpath = concat_strings (ldpath, "/ipa.so");
	    if (!file_exists (ldpath)) {
		error ("ipa.so is not installed on %s", get_phase_dir (ldphase));
		return;
	    }
	    // Tell ipa_link about the LD_LIBRARY_PATH that was in effect
	    // before the compiler was run.
	    str = "-INTERNAL:old_ld_lib_path=\"";
	    if (old_ld_library_path) {
	      str = concat_strings (str, old_ld_library_path);
	    }
	    str = concat_strings (str, "\"");
	    add_string(args, str);

        char *root_prefix = directory_path(get_executable_dir());
        char *our_path;

        if (abi == ABI_N32) {
          asprintf(&our_path, "%s" LIBPATH "/32", root_prefix);
        } else {
          asprintf(&our_path, "%s" LIBPATH "/64", root_prefix);
        }

        add_string(args, concat_strings("-L", our_path));
        free(our_path);

	    // Tell ipa_link about the source language.
	    switch (invoked_lang) {
	      case L_f77:	str = "F77";	break;
	      case L_f90:	str = "F90";	break;
	      case L_cc:	str = "C";	break;
	      case L_CC:	str = "CC";	break;
	      default:		internal_error("run_ld: unknown language\n");
	    }
	    add_string(args, concat_strings("-INTERNAL:lang=", str));

	    init_crt_paths ();
	    if (invoked_lang == L_CC || instrumentation_invoked == TRUE) {
	      init_stdc_plus_plus_path(!option_was_seen(O_static));
	    }
	}
	ldpath = get_full_phase_name(ldphase);

	/* for ld, we first have options, then files, then objects,
	 * where objects includes object-specific options */
	add_inline_option ();
	copy_phase_options (args, ldphase);

	if (invoked_lang == L_CC) {
	    if (!multiple_source_files && !((shared == RELOCATABLE) && (ipa == TRUE) && (outfile == NULL)) && !keep_flag)
		mark_saved_object_for_cleanup();
	}
	add_file_args (args, ldphase);

	if (shared == RELOCATABLE && source_file != NULL)
	    add_string(args, construct_given_name(
			       fix_name_by_lang(source_file),
			       "o",
			       outfile == NULL ? TRUE : keep_flag));
  	else
	    append_objects_to_list (args);

    // now we use gcc/g++ to link fortran programs, so ignore these ld options
    /*if ( invoked_lang == L_f77 || invoked_lang == L_f90) {
        specify_dyn_linker (args);
    }*/

#ifdef CROSS_COMPILATION
    specify_dyn_linker (args);
#endif

#if defined(TARG_MIPS)
	add_string(args, "-mips64");	// call gcc with -mips64
#endif

	add_instr_archive (args);

    add_final_ld_args (args,ldphase);
#ifndef TARG_SL
    if ( ldphase == P_ipa_link ) {
      specify_ipa_dyn_linker(args);
    }
	postprocess_ld_args (args);
#endif

	run_phase (ldphase, ldpath, args);
}

void
run_ar(void)
{
     string_list_t *args = init_string_list();
     char *arpath;
     arpath = get_full_phase_name(P_ar);

     add_string (args, "-rc");
     copy_phase_options (args, P_ar);
     add_string (args, outfile);
     append_ar_objects_to_list(args);
     run_phase (P_ar, arpath, args);
}



void
run_pixie (void)
{
 int link_status;
 string_list_t *args = init_string_list();
 char *pixie_file;
 char *temp;

 copy_phase_options (args, P_pixie);
 add_file_args (args, P_pixie);
 run_phase (P_pixie, get_full_phase_name(P_pixie), args);
 if (outfile != NULL) 
   temp = string_copy(outfile);
 else
   temp = string_copy("a.out");
 if (fb_xdir != NULL) {
#if defined(_WIN32)
   internal_error("cannot run pixie on windows");
#else
   pixie_file = concat_strings(fb_xdir,  "/");
   pixie_file = concat_strings(pixie_file, drop_path(temp));
   link_status = symlink(pixie_file, temp);
   if (link_status != 0) {
      if (errno == EEXIST) {
          warning("%s already exists; link from %s to %s not created",
                            temp, temp, pixie_file);
      } else {
          internal_error("cannot create link from %s to %s",
                                 temp, pixie_file );
          perror(program_name);
      }
   }
#endif
 }
}

void
run_prof (void)
{
 int link_status;
 string_list_t *args = init_string_list();
 char *bin_dot_pixie, *bin_plain;
 boolean delete_link = TRUE;

#ifdef _WIN32
 internal_error("cannot run prof on windows");
#else
 if (prof_file != NULL) {
    bin_plain = string_copy (prof_file);
    bin_dot_pixie = concat_strings(prof_file, ".x.pixie");
 } else {
    internal_error("No count file was specified for a prof run");
    perror(program_name);
 }
 link_status = link(bin_plain, bin_dot_pixie);
 if (link_status != 0) {
    delete_link = FALSE;
    if (errno == EEXIST) {
        warning("Link from %s to %s already exists",
                          bin_dot_pixie, bin_plain );
    } else {
        internal_error("cannot create link from %s to %s",
                               bin_dot_pixie, bin_plain );
        perror(program_name);
    }
 }

 copy_phase_options (args, P_prof);
 add_file_args (args, P_prof);
 run_phase (P_prof, get_full_phase_name(P_prof), args);
  
 if (delete_link) {
    int unlink_status;
    unlink_status = unlink(bin_dot_pixie);
    if (unlink_status != 0 && errno != ENOENT) {
      internal_error("cannot unlink %s", bin_dot_pixie );
      perror(program_name);
    }
 }
#endif				
}

void
run_compiler (int argc, char *argv[])
{
	int i;
	string_list_t *args;
	boolean inst_info_updated = FALSE;
	boolean cmd_line_updated = FALSE;
        buffer_t rii_file_name;
        buffer_t ii_file_name;

	clear_current_errors();
	determine_phase_order();
	add_inline_option();
	if (execute_flag) {
		check_existence_of_phases();
	}
	input_source = source_file;

	// Set stack size to the hard limit.  Bug 3212.
	set_stack_size();

	for (i = 0; phase_order[i] != P_NONE; i++) {
	        /* special case where the frontend decided that
		   inliner should not be run */
	        if (run_inline == FALSE &&	// bug 11325
		    phase_order[i] == P_inline)
		    continue;

		if (is_matching_phase(get_phase_mask(phase_order[i]), P_any_ld)) {
			source_kind = S_o;
			/* reset source-lang to be invoked-lang for linking */
			source_lang = get_source_lang(source_kind);
			run_ld ();
			if (Gen_feedback)
			   run_pixie ();
		} else {
			args = init_string_list();
			add_file_args_first (args, phase_order[i]);  // bug 6874
			if (phase_order[i] == P_inline &&
			    run_inline == TRUE &&
			    olevel == 0 &&
			    !(option_was_seen(O_INLINE_) || 
			    option_was_seen(O_INLINE) || 
			    option_was_seen(O_inline) || 
			    option_was_seen(O_finline) || 
			    option_was_seen(O_finline_functions))) {
				prepend_option_seen (add_string_option(O_INLINE_, "none"));
			}
			copy_phase_options (args, phase_order[i]);
                        
			if (!cmd_line_updated &&
			    phase_order[i] > P_any_optfe &&
			    phase_order[i] != P_c_gfe &&
			    phase_order[i] != P_cplus_gfe &&
			    phase_order[i] != P_spin_cc1 &&
			    phase_order[i] != P_spin_cc1plus &&
			    phase_order[i] != P_wgen &&
			    phase_order[i] < P_any_fe)
			{
			    add_command_line_arg(args, source_file);
			    cmd_line_updated = TRUE;
			}
			add_file_args (args, phase_order[i]);
			if (has_current_errors()) break;
			run_phase (phase_order[i],
				   get_full_phase_name(phase_order[i]), args);
                        /* undefine the environment variable
                         * DEPENDENCIES_OUTPUT after the pre-processor phase -
                         * bug 386.
                         */
                        if (phase_order[i] == P_gcpp_plus)
                          unsetenv("DEPENDENCIES_OUTPUT");

			if ( i == 0 && (string_md == TRUE || string_mmd == TRUE)){
			        /* Bug# 581, bug #932, bug# 1049, bug #433 */
				/* We've run the dependency phase, so
				 * let's clean all those switches out. */
				string_item_t * p;
				for (p = args->head; p != NULL; p = p->next) {
					if (strcmp (p->name, "-MF") == 0) {
						replace_string (args, p->name, "");
						replace_string (args, p->next->name, "");
					}
					if (strcmp (p->name, "-MT") == 0) {
						replace_string (args, p->name, "");
						replace_string (args, p->next->name, "");
					}
					if (strcmp (p->name, "-MQ") == 0) {
						replace_string (args, p->name, "");
						replace_string (args, p->next->name, "");
					}
				}
				replace_string (args, "-M", "");
				replace_string (args, "-MM", "");
				replace_string (args, "-MG", "");
				replace_string (args, "-MP", "");
			 	run_phase (phase_order[i],
					   get_full_phase_name(phase_order[i]), args);
				string_md = FALSE;
				string_mmd = FALSE;
			}
			if (!inst_info_updated &&
			    phase_order[i] > P_any_optfe &&
			    phase_order[i] < P_any_fe) {
			    /*
			     * Update the instantiation information file
			     * with the new command line, if applicable.
			     */
			    if (prelink_flag && source_lang == L_CC) {
		  		if (outfile != NULL && !multiple_source_files
				      	  && remember_last_phase != P_any_ld) 
				    sprintf(ii_file_name, "%s",
					    make_ii_file_name(outfile));
		  		else
				    sprintf(ii_file_name,
				   	    "./ii_files/%s",
					    construct_file_with_extension(
					        source_file, "ii"));
			        update_instantiation_info_file(
				        ii_file_name, source_file);

                            }
			    inst_info_updated = TRUE;
			}
		}
		if (has_current_errors()) break;
	}
}

/*
 * The mechanism for updating the template instantiation info file for
 * the current object file.
 *
 * We base the .ii filename on the object file name by creating a name
 * as follows: dirname(obj)/ii_files/basename(obj,.o).ii.
 *
 * If this file exists, we look for a terminator (a line with the contents
 * "----"), and replace everything before that line with updated information
 * about the command-line used to compile the object file, and the working
 * directory from where the command was issued. If the terminator is missing,
 * it is prepended to the file.
 */


/*
 * Is "c" a character that would need quoting to the shell?
 */

#define is_shell_special(c) \
    ( (c) == '\'' || \
      (c) == '|' || \
      (c) == '&' || \
      (c) == '*' || \
      (c) == '?' || \
      (c) == '[' || \
      (c) == ']' || \
      (c) == ';' || \
      (c) == '!' || \
      (c) == '(' || \
      (c) == ')' || \
      (c) == '^' || \
      (c) == '<' || \
      (c) == '>' || \
      (c) <= ' ' || \
      (c) == '\t' || \
      is_shell_quote_special(c) )

/*
 * Is "c" a character that would have to be \-escaped inside double-quotes?
 *
 * The answer to this one actually depends on the shell. Especially
 * troublesome is '!': for csh, it *must* be \-escaped inside quotes,
 * and for sh, it *must not* be \-escaped. sh preserves the \ if it
 * is not quoting something significant. Sigh.
 *
 * However, since most "make"s and "make"-clones use /bin/sh as the shell
 * regardless of the SHELL setting, and since system() always uses /bin/sh
 * to execute the command, I am going to stick with whatever sh needs.
 */
#define is_shell_quote_special(c) \
    ( (c) == '"' || \
      (c) == '\\' || \
      (c) == '`' || \
      (c) == '$' )


/*
 * returns the length of the argument after inserting any required quoting
 * characters.
 */
int quoted_length(char *p, int *quoted)
{
    int len = 0;
    char c;

    *quoted = 0;

    while (c = *p++) {
	if (!(*quoted) &&
	    is_shell_special(c)) {
	    (*quoted) = 1;
	    len += 2;
	}
	if (is_shell_quote_special(c))
	    len ++;
	len ++;	/* the character itself */
    }
    return len;
}

/*
 * writes a quoted (if necessary) copy of p into the buffer pointed to
 * by buf, and returns the length of the quoted string written.
 */
int quote_shell_arg(char *p, char *buf)
{
    char c;
    int quoted = 0;
    int len;

    len = quoted_length(p, &quoted);

    if (quoted)
	*buf++ = '"';
    while (c = *p++) {
	if (is_shell_quote_special(c))
	    *buf++ = '\\';
	*buf++ = c;
    }
    if (quoted)
	*buf++ = '"';

    return len;
}


int saved_argc;
char **saved_argv;
static int curr_argc;
static int add_c = 0;

/*
 * Save off the command-line in a quote-protected string that can be re-fed
 * to the shell later by edg_prelink.  We save off any "-o" and its argument
 * to a separate global. This is *not* emitted into the ii_file if we are
 * doing a single-source compile and link with libraries.
 */

void add_minus_c_option(void)
{
    add_c = 1;
}

void save_command_line(int argc, char **argv)
{
    int i;
    saved_argc = argc;
    saved_argv = malloc((saved_argc+1) * sizeof(char *));

    for (i = 0; i < argc; i++) {
	if (argv[i])
	    saved_argv[i] = string_copy(argv[i]);
	else
	    saved_argv[i] = NULL;
    }
    saved_argv[saved_argc] = NULL;
}

int check_for_saved_option(char *opt)
{
    int i;

    for (i = 1; i < saved_argc; i++) {
	if (strcmp(opt, saved_argv[i]) == 0)
	    return 1;
    }

    return 0;
}

void set_current_arg_pos(int n)
{
    curr_argc = n;
}

void cancel_saved_arg(int count)
{
    int i;
    for (i = 0; i < count; i++) {
	if (saved_argv[curr_argc+i] != NULL)
	    free(saved_argv[curr_argc+i]);
	saved_argv[curr_argc+i] = NULL;
    }
}

/*
 * Save off the command-line in a quote-protected string that can be re-fed
 * to the shell later by edg_prelink.  We save off any "-o" and its argument
 * to a separate global. This is *not* emitted into the ii_file if we are
 * doing a single-source compile and link with libraries.
 */
static void convert_saved_command_line_into_string ( void )
{
    int len = 0;
    int quoted = 0;
    int i;
    char *p;

    /* first, count the length */
    for (i = 1; i < saved_argc; i++) {
	if (saved_argv[i] != NULL) {
	    len += quoted_length(saved_argv[i], &quoted) + 1;
	}
    }

    /* allocate the space */
    command_line = p = malloc(len+1);

    /* Now copy the arguments */
    for (i = 1; i < saved_argc; i++) {
	if (saved_argv[i] != NULL) {
	    p += quote_shell_arg(saved_argv[i], p);
	    *p++ = ' ';
	}
    }

    if (p > command_line)
	p[-1] = '\0';
    else
	p[0] = '\0';
}

static void write_command_string_into_file(FILE *cmdfile,
					   char *sourcefile,
					   int for_ii_file)
{
    if (for_ii_file)
	fputs("CMDLINE=", cmdfile);
    
    fprintf(cmdfile, "%s ", orig_program_name);

    if (add_c) {
	/* always emit -c into file, because we will always be
	 * recompiling only one file at a time downstream in, e.g.,
	 * the prelinker, or the Fix and Continue mechanism
	 */
	fputs("-c ", cmdfile);
    }
    fprintf(cmdfile, "%s %s", command_line, sourcefile);
    if (outfile != NULL        && 
	!multiple_source_files && 
	(invoked_lang != L_CC ||
         !for_ii_file || remember_last_phase != P_any_ld)) {
	/* Got a "cc -c -o foo foo.o", so emit the object file name */
	fprintf(cmdfile, " -o %s", outfile);
    }
    fprintf(cmdfile, "\n");
    
    /* Now write out current working directory */
    if (for_ii_file)
	fputs("PWD=", cmdfile);
    fprintf(cmdfile, "%s\n", get_cwd());

    /* trailer */
    if (for_ii_file)
	fputs("----\n", cmdfile);
}

static void add_command_line_arg(string_list_t *args, char *source_file)
{
    char *cmd_file_name;
    FILE *cmd_file;
    /*
     * convert the saved command line prefix
     * into a string (to which the filename will be
     * appended.
     */
    convert_saved_command_line_into_string();
    /*
     * Now write out a command line and pwd for
     * passing down to the frontends for DWARF.
     */
    cmd_file_name = create_temp_file_name("L");
    if (execute_flag) {
	cmd_file = fopen(cmd_file_name, "w");
    	if (cmd_file == NULL) {
	    error("write_command_string: could not create %s", cmd_file_name);
    	} else {
	    write_command_string_into_file(cmd_file, source_file,
				       /*for_ii_file=*/FALSE);
	    fclose(cmd_file);
	}
    }
    if (source_lang == L_cc ||
	source_lang == L_CC ||
	source_lang == L_f77 ||
	source_lang == L_f90) 
    {
	    add_string(args, concat_strings("-FE:cmdline=", cmd_file_name));
    }
}

/*
 * Skip over leading lines upto and including the terminator (a line that
 * starts with ----). If the terminator is not found, rewind back to start.
 */
void skip_old_ii_controls(FILE *f)
{
    int c;
    int terminator_found = 0;

    c = getc(f);
    while (c != EOF) {
	if (c == '-' &&
	    (c = getc(f)) == '-' &&
	    (c = getc(f)) == '-' &&
	    (c = getc(f)) == '-') {
	    terminator_found = 1;
	}
	while (c != '\n' && c != EOF)
	    c = getc(f);
	if (terminator_found)
	    break;
	if (c == '\n')
	    c = getc(f);
    }

    if (c == EOF)
	rewind(f);
}

/*
 * Make the ".ii" file name from the object file name.
 */
static char *make_ii_file_name(char *objname)
{
    char *base = driver_basename(objname);
    int baselen = strlen(base);

    base = concat_strings(base, "   ");
    if (base[baselen-2] == '.'&& base[baselen-1] == 'o')
	strcpy(&base[baselen-1], "ii");
    else
	strcpy(&base[baselen], ".ii");

    return concat_strings(
		dirname(objname),
		concat_strings("/ii_files/", base));
}

/*
 * Make the ".rii" file name from the object file name.
 */
static char *make_rii_file_name(char *objname)
{
    char *base = driver_basename(objname);
    int baselen = strlen(base);

    base = concat_strings(base, "    ");
    if (base[baselen-2] == '.' && base[baselen-1] == 'o')
	strcpy(&base[baselen-1], "rii");
    else
	strcpy(&base[baselen], ".rii");

    return concat_strings(
		dirname(objname),
		concat_strings("/rii_files/", base));
}

/*
 * The actual function that updates the instantiation information
 * in the .ii file.
 */
int update_instantiation_info_file(char *ii_file_name, char* sourcefile)
{
    char *new_ii_file_name = concat_strings(ii_file_name, ".NEW");
    
    FILE *old_ii = fopen(ii_file_name, "r");
    FILE *new_ii;
    int c;
    
    if (old_ii != NULL) {
	/* There is an existing .ii file (may be empty) */
	/* need to update that file! */

	if (show_flag) {
	    fprintf(stderr, "%s: update_instantiation_info_file %s\n",
		    program_name, ii_file_name);
	}

	new_ii = fopen(new_ii_file_name, "w");
	if (new_ii == NULL) {
	    /* could not open the (new) file: permission problem in
	     * directory? */
	    error("update_instantiation_info_file: error in creating file %s",
		  new_ii_file_name);
	    perror("update_instantiation_info_file");
	    return 1;
	}

	/*
	 * skip over the existing control information (upto the standard
	 * separator), if any.
	 */
	skip_old_ii_controls(old_ii);

	/*
	 * Now, write out the new control information. For now, we have
	 * only two lines:
	 * CMDLINE=<compiler name> <quote-protected command-line arguments>
	 * PWD=<current working directory>
	 */
	write_command_string_into_file(new_ii, sourcefile,
				       /*for_ii_file=*/TRUE);

	/* Now copy over the remainder of the old file. */
	while ((c = getc(old_ii)) != EOF)
	    putc(c, new_ii);

	/* Rename the new file to replace the existing .ii file */
	fclose(old_ii);
	fclose(new_ii);
	if (rename(new_ii_file_name, ii_file_name) < 0) {
	    error("update_instantiation_info_file: error in renaming %s to %s",
		  new_ii_file_name, ii_file_name);
	    perror("update_instantiation_info_file");
	    return 1;
	}
    }
    free(new_ii_file_name);
    return 0;
}


/* ====================================================================
 *
 * add_ipl_cmd_string
 *
 * Given an option identified by iflag, add the appropriate option
 * string(s) to the ipl_cmds list used by IPL as the back end command
 * list to be passed via the object file to the back end under IPA.
 *
 * ====================================================================
 */

static void
add_ipl_cmd_string (int iflag)
{
  if (option_matches_phase (iflag, P_be)) {
    /* Assume inside a FOREACH_IMPLIED_OPTION iteration */
    char *name = get_current_implied_name();
    
    /* If this is not one of the driver-recognized options, then
     * we need to prefix it with -Wb, -- doing so also requires
     * replacing internal spaces with commas:
     */
    if ( debug ) {
	fprintf ( stderr, "%s: %sternal, #%d of %d, %s\n",
		name,
		is_internal_option (iflag) ? "in" : "ex",
		iflag, LAST_PREDEFINED_OPTION,
		is_derived_option (iflag) ? "derived" : "primary"
	       );
    }

    if ( is_internal_option (iflag)
      || ( iflag >= LAST_PREDEFINED_OPTION
	&& ! is_derived_option (iflag) ) )
    {
      char *p;
      name = concat_strings ("-Wb,", name );
      for ( p = name; *p != 0; p++ ) {
	if ( *p == ' ' ) *p = ',';
      }
    }

    if (strcmp(name,"-pfa") == 0) {
      /* change pfa to -Wb,-pfa, since after IPA we only want auto-parallelism
       * from be/LNO, but not the mfef77/rii_files or be/dsm_clone, etc.
       */
      name = concat_strings ("-Wb,", name);
    }
    
    /* Add the option (possibly with blanks) to the list: */
    if (strcmp(name,"-dsm_clone")!=0)
      add_multi_strings (ipl_cmds, name, FALSE);
  }
} /* add_ipl_cmd_string */


void
save_ipl_commands (void)
{
    int i;
    int iflag;

    /* Build the string list initialized with the command name and -non_shared
     * if necessary:
     */
    if (ipl_cmds == 0) {
      ipl_cmds = init_string_list ();
      add_string (ipl_cmds, program_name);
      if ( shared == NON_SHARED ) {
	add_string (ipl_cmds, "-non_shared");
      }
    }
    
    FOREACH_OPTION_SEEN(i) {
	if ( i == O_Unrecognized || i == O_show )
	    continue;
	if (option_matches_language (i, invoked_lang)) {
	    FOREACH_IMPLIED_OPTION(iflag, i) {
	   	 add_ipl_cmd_string (iflag);
	    }
	}
    }
#ifdef TARG_LOONGSON
          switch (loongson_version) {
               case ISA_LOONGSON2e: {
                          add_string(ipl_cmds, "-loongson2e");
                          break;
                       }
               case ISA_LOONGSON2f: {
                          add_string(ipl_cmds, "-loongson2f");
                          break;
                       }
               case ISA_LOONGSON3: {
                          add_string(ipl_cmds, "-loongson3");
                          break;
                       }
               default:   add_string(ipl_cmds, "-loongson2e");
               }
#endif

    // Add -TARG options.
    add_targ_options(ipl_cmds);

} /* save_ipl_commands */



/*
 * set the freeform/fixedform switch appropriately 
 * for mfef95 and ftpp. Also set the line length for ftpp, but only in 
 * fixed form.
 */
static void
set_f90_source_form(string_list_t *args,boolean set_line_length)
{
   char buf[16];
   
   sprintf(buf,"-N%d",fortran_line_length);

   if (fortran_form == 2) {
      /* User specified -freeform */
      add_string(args,"-ffree");
   } else if (fortran_form == 1) {
      /* User specified fixed form */
      add_string(args,"-ffixed");
      if (set_line_length) add_string(args,buf);
   } else {
      /* Unspecified, need to get the extension (.f or .f90) */
      if (source_kind == S_f90 || source_kind == S_F90) {
	 add_string(args,"-ffree");
      } else {
	 add_string(args,"-ffixed");
	 if (set_line_length) add_string(args,buf);
      }
   }
}

/*
 * utility routine to set arguments used by both passes of f90
 *
 */

static void
do_f90_common_args(string_list_t *args)
{
   /* Handle the source form options */
   set_f90_source_form(args,FALSE);

   add_string(args,"-LANG:=F90");
   if (0 != f90_module_dir) {
     /* -J is already prepended */
     add_string(args, f90_module_dir);
     }
}

// Set the stack size to the hard limit.
static void
set_stack_size()
{
#if !defined(_WIN32) && !defined(__CYGWIN__)
  struct rlimit rl;
  rlim_t max_stack;

  // Read old limit.
  if (getrlimit(RLIMIT_STACK, &rl) == -1) {
    warning("cannot read stack size limit");
    return;
  }
  if (rl.rlim_cur == RLIM_INFINITY) {
    return;
  }

  // Set new limit.
  max_stack = rl.rlim_max;
  rl.rlim_cur = max_stack;
  if (setrlimit(RLIMIT_STACK, &rl) == -1) {
    warning("cannot change stack size limit");
    return;
  }
  if (getrlimit(RLIMIT_STACK, &rl) == -1 ||
      rl.rlim_cur != max_stack) {
    warning("cannot change stack size limit");
    return;
  }
#endif
}


// Get the system GCC's major version number.
int
get_gcc_major_version()
{
#ifdef __MINGW32__
  /* cannot rely on accessing system,
   * so just use what we were built with */
  return __GNUC__;
#else
  int v[4];
  get_gcc_version(v, 4);
  return v[0];
#endif
}

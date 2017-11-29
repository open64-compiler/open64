/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/*
 * OPTIONS that are not simple enough to handle in the table
 * are done by hand in these routines.
 */

#include <string.h>
#include <stdlib.h>
#include <stamp.h>
#include "string_utils.h"
#include "options.h"
#include "option_seen.h"
#include "option_names.h"
#include "lang_defs.h"
#include "errors.h"
#include "opt_actions.h"
#include "file_names.h"
#include "get_options.h"
#include "phases.h"
#include "run.h"
#include "version.h"

int endian = UNDEFINED;

void
set_defaults (void)
{
	int flag;
	#ifdef PSC_TO_OPEN64
	/* handle OPEN64_CC environment variable */
	char *open64_cc = getenv("OPEN64_CC");
	if (open64_cc != NULL && !is_toggled(ansi)) {
		/* value not set yet */
		if (strcmp(open64_cc, "-ansi") == 0) {
	#endif
			toggle(&ansi,STRICT_ANSI);
			prepend_option_seen (O_ansi);
		}
	}

	/* XPG fort77 doesn't allow -O with no explicit level */
	if (xpg_flag && invoked_lang == L_f77 && option_was_seen(O_O)) {
		error("XPG compiles must specify explicit optlevel rather than -O");
	}
	{
	  /* QA wants way to turn off this check via environment var */
	  char *ir_version_check = getenv("COMPILER_IR_VERSION_CHECK");
	  if (ir_version_check != NULL) {
		if (strcmp(ir_version_check, "off") == 0) {
			flag = add_string_option(O_DEBUG_, "ir_version_check=off");
			/* prepend so comes before user option */
			prepend_option_seen(flag);
		}
	  }
	}
	if (endian == UNDEFINED) {
#if defined(TARG_MIPS)
#if !defined(TARG_SL)
		/* Default to little-endian -JMB */
		toggle(&endian, ENDIAN_LITTLE);
		prepend_option_seen(O_EL);
		prepend_option_seen(O_mlittle_endian);
#endif
#else 
#ifdef LITTLE_ENDIAN_HOST
		toggle(&endian, ENDIAN_LITTLE);
#else
		toggle(&endian, ENDIAN_BIG);
#if !defined(TARG_PPC32)
		prepend_option_seen(O_EB);
#endif
#endif
#endif
	}

	prepend_option_seen(O_usegfe);
	prepend_option_seen(O_usef90);

	if (ansi == UNDEFINED) {
		toggle(&ansi,EXTENDED_ANSI);
	}

	prepend_option_seen(O_cpp_fortran90);
	prepend_option_seen(O_cpp_fortran77);
	prepend_option_seen(O_cpp_fortran);
	prepend_option_seen(O_cpp_assembly);
	prepend_option_seen(O_prelink);
	prepend_option_seen(O_demangle);
#ifdef TARG_IA64    	
	if (shared == UNDEFINED) {
        	if (abi == ABI_IA32) {
            		toggle(&shared,NON_SHARED);
        	} else {
            		toggle (&shared, CALL_SHARED);
            		prepend_option_seen(O_call_shared);
        	}
    	} else if (shared != CALL_SHARED) {
        	flag = add_string_option(O_OPT_, "Olegacy=TRUE");
        	add_option_seen (flag);
	}
#else
	if (shared == UNDEFINED && abi == ABI_IA32) {
		toggle(&shared,NON_SHARED);
	}
#endif
	if (!is_toggled(isstatic)) {
		toggle(&isstatic,1);
		prepend_option_seen(O_automatic);
	}

	// Make -cpp the default for Fortran.  Bug 4243.
	if (!is_toggled(use_ftpp)) {
		toggle(&use_ftpp, 0);
	}

#ifndef BUILD_GNU3
    // Always use the GCC 4.2 FE.
    toggle(&gnu_major_version, 4);
    toggle(&gnu_minor_version, 2);
#else
	// Use the system's GCC version to select -gnu3/-gnu4 as the default.
	// Bug 11426.
	if (!is_toggled(gnu_major_version)) {
	  toggle(&gnu_major_version, get_gcc_major_version());
	  switch (gnu_major_version) {
	    case 3:	// default to GCC 3.3
	      toggle(&gnu_minor_version, 3);
	      break;
	    case 4:	// default to GCC 4.2
	      toggle(&gnu_minor_version, 2);
	      break;
	    default:
	      error("no support for GCC version %d", gnu_major_version);
	  }
	}
#endif

#if defined(TARG_NVISA)
	/* stop after assembly */
	if (option_was_seen(O_multicore))
	  last_phase=earliest_phase(P_bec,last_phase);
	else
	  last_phase=earliest_phase(P_be,last_phase);

	/* no calling convention for now, so must inline everything */
	flag = add_string_option(O_INLINE_, "all");
	prepend_option_seen (flag);
	toggle_inline_on();

	/* add build_date */
	flag = add_string_option(O_LIST_, 
		concat_strings("build_date=", build_date));
	add_option_seen (flag);
#endif
}


static int
get_olevel_flag (int olevel)
{
	switch (olevel) {
	case 0: return O_O0;
	case 1: return O_O1;
	case 2: return O_O2;
	case 3: return O_O3;
	default: return O_Unrecognized;
	}
}

/* replace -O* with O0 */
void
turn_down_opt_level (int new_olevel, char *msg)
{
	int flag;
	int new_flag;
	if (fullwarn) warning(msg);
	flag = get_olevel_flag(olevel);
	new_flag = get_olevel_flag(new_olevel);
	if (option_was_seen(O_O))
		replace_option_seen (O_O, new_flag);
	else if (option_was_seen(flag))
		replace_option_seen (flag, new_flag);
	else
		internal_error("driver didn't find -O flag");
	olevel = new_olevel;
}

static void
turn_off_ipa (char *msg)
{
	int flag;
	warning (msg);
	ipa = FALSE;
	/* remove all ipa flags from option_seen list */
	FOREACH_OPTION_SEEN(flag) {
		if (flag == O_ipa)
			set_option_unseen(flag);
		else if (flag == O_IPA)
			set_option_unseen(flag);
		else if (is_derived_option (flag)
		    && get_derived_parent(flag) == O_IPA_)
			set_option_unseen(flag);
	}
}

void
add_special_options (void)
{
	int flag;
	buffer_t buf;
	char *s;
	boolean undefined_olevel_flag = FALSE; 

	/* Hack for F90 -MDupdate. We need to pass the MDupdate to mfef95, because we don't
	 * have an integrated pre-processor. I can't figure out a better way to do this, given
	 * the architecture of the phase generator. 
	 * R. Shapiro, 2/26/97
	 */
	add_phase_for_option(O_MDupdate,P_f90_fe);
	add_phase_for_option(O_MDtarget,P_f90_fe);
	remove_phase_for_option(O_MDupdate,P_f90_cpp);
	remove_phase_for_option(O_MDtarget,P_f90_cpp);

        add_phase_for_option(O_D, P_cppf90_fe);
        add_phase_for_option(O_U, P_cppf90_fe);
        add_phase_for_option(O_E, P_cppf90_fe);
        add_phase_for_option(O_P, P_cppf90_fe);

	if (use_ftpp == TRUE) {
		/* ftpp means pass defines directly to mfef95,
		 * and since not using gcc we have to pass some options
		 * that are otherwise implicit. */
		flag = add_string_option(O_D, "_LITTLE_ENDIAN");
		prepend_option_seen (flag);
    		flag = add_string_option(O_D, "__LONG_MAX__=9223372036854775807L");
		prepend_option_seen (flag);
		prepend_option_seen (O_cpp_nonansi);
		if (keep_flag) {
			add_phase_for_option (O_keep, P_cppf90_fe);
		}
	}

	if (option_was_seen(O_traditional)
		&& !option_was_seen(O_traditional_cpp)) 
	{
		/* pass -traditional to both gfe and cpp */
		add_phase_for_option(O_traditional, P_c_gfe);
		add_phase_for_option(O_traditional, P_cplus_gfe);
		add_phase_for_option(O_traditional, P_spin_cc1);
		add_phase_for_option(O_traditional, P_spin_cc1plus);
	}

#if defined(TARG_IA32)
	flag = add_string_option(O_D, "__NO_MATH_INLINES");
	prepend_option_seen (flag);
#endif

	if ((mpkind == NORMAL_MP) && !Disable_open_mp) {
		flag = add_string_option(O_D, "_OPENMP=199810");
		prepend_option_seen (flag);
	}

	if (olevel == UNDEFINED) {
		olevel = default_olevel;
		if (olevel == UNDEFINED) {
			/* if no default, use -O0 */
			olevel = 0;
		}
		flag = get_olevel_flag(olevel);
		prepend_option_seen (flag);
		// fix for bug 447
		undefined_olevel_flag = TRUE;
	}
	if (!nostdinc) {
		/* mips only: add -I path for CC */
                if (abi != ABI_I64 && abi != ABI_I32 && abi != ABI_IA32) {
                  flag = add_string_option(O_I__, 
                              concat_strings(get_phase_dir(P_include),"/CC"));
                  set_language_for_option (flag, L_CC);
                  add_option_seen (flag);
                }
	}
	if (!is_toggled(gnum)) {
		/* set gnum default */
		if (abi == ABI_RAG32) {
			/* be compatible with ucode */
			if (shared == NON_SHARED) {
				toggle(&gnum,8);
			} else {
				toggle(&gnum,0);
			}
		} else {
			toggle(&gnum,8);
		}
		sprintf(buf, "%d", gnum);
#ifdef TARG_SL
		flag = add_string_option(O_G8, buf);
#else
		flag = add_string_option(O_G__, buf);
#endif
		prepend_option_seen(flag);
	}

	/* Set default optimization to -O0 when compiling with -g.
	 * We leave ipa alone because mixing -ipa with -g is illegal
	 * and generates a separate error later on.
	 */
	/* leave -O with -g unless no -O specified */
	if (undefined_olevel_flag == TRUE && glevel > 1 && ipa != TRUE) {
		turn_down_opt_level(0, "-g changes optimization to -O0 since no optimization level is specified");
	}

	/* Turn off inlining when compiling -O0.  We definitly want
	 * this off when compiling with -g -O0, but we don't want
	 * -g to change the generated code so we leave it off always.
	 * See bugs 1917 and 7595.
	 */
        /*
          open64.net bug838, to get gnu compatibility, we open inline for O0.
        */

        /* In the SGI world, -g3 says to emit crippled debug info for use
	 * with optimized code. In the GNU/Pathscale world, -g3 says to emit
	 * additional debug info for C preprocessor macros, so changing -g to
	 * -g3 just because the optimization level is high makes no sense. In
	 * addition, when the language is Fortran, putting predefined C
	 * preprocessor macros into the preprocessor output causes trouble.
	 */
	if ((invoked_lang == L_f77 || invoked_lang == L_f90) &&
	  (option_was_seen(O_g3))) {
	  glevel = 2;
	  replace_option_seen (O_g3, O_g2);
	}

	if (option_was_seen(O_S) && ipa == TRUE) {
		turn_off_ipa ("-IPA -S combination not allowed, replaced with -S");
	}
#ifdef IPA_PROFILING_O3_NOT_COEXIST
	if (instrumentation_invoked == TRUE) {
	    if (ipa == TRUE) {
		inline_t = FALSE;
		turn_off_ipa ("-fb_create requires no -IPA");
	    }
	    if (olevel > 2)
		turn_down_opt_level (2, "-fb_create conflicts with -Ofast/-O3; changing to -O2");
	}
#endif
	if (Gen_feedback && olevel > 0) {
		turn_down_opt_level(0, "-fbgen conflicts with -O; changing to -O0");
	}
	if (Gen_feedback && ipa == TRUE) {
		turn_off_ipa ("-IPA -fbgen combination not allowed, replaced with -fbgen");
	}
	if (ipa == TRUE) {
            if (option_was_seen (O_fprofile_arcs))
	      error ("IPA not supported with -fprofile-arcs");
	    if (option_was_seen (O_ftest_coverage))
	      error ("IPA not supported with -ftest-coverage");
	    if (olevel <= 1)
		flag = add_string_option (O_PHASE_, "i");
	    else
		flag = add_string_option (O_PHASE_, "p:i");
	} else {
	    /*
	     * Determine which back end phase(s) need to be run.
	     *
	     *				-O0/-O1	-O2		-O3
	     *				===========================
	     *		.B,.I,.P:	cg	wopt/cg		lno/wopt/cg
	     *		.N:		cg	wopt/cg		wopt/cg
	     *		.O:		cg	cg		cg
	     */
	    if (source_kind == S_O)
		warning("compiles of WOPT-generated .O files will usually fail due to missing state information");
	    if (olevel <= 1 || source_kind == S_O)
		flag = add_string_option(O_PHASE_, "c");
	    else if (olevel == 2 || source_kind == S_N)
		flag = add_string_option(O_PHASE_, "w:c");
	    else 
#ifdef TARG_NVISA
		/* only add preopt for now */
		flag = add_string_option(O_PHASE_, "p:w:c");
#else
		flag = add_string_option(O_PHASE_, "l:w:c");
#endif
	}
	prepend_option_seen (flag);

	if (option_was_seen(O_ar) && outfile == NULL) {
	   error("-ar option requires archive name to be specified with -o option");
	}
	
	if (skip_as == UNDEFINED) {
		skip_as = FALSE;
	}
	if (skip_as == FALSE && ! keep_flag && last_phase != P_be) {
		/* if going thru asm and not keeping .s file,
		 * then don't print extra notes and source */
		flag = add_string_option(O_LIST_, "source=off:notes=off");
		prepend_option_seen (flag);
	}

	if (option_was_seen(O_static) && invoked_lang == L_f90) {
		/* IRIX f90 -static was fe option,
		 * but gnu makes it a link option.
		 * Warn about possible misuse. */
		warning("Under Linux, -static is a linker option for using static libraries; if you wanted to put local data in static area, use -static-data instead");
	}
}


/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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


/* this is used by both table and the driver */
#ifndef LANG_DEFS_H
#define LANG_DEFS_H

#include "basic.h"

/* languages */
typedef enum {
	L_NONE,
	L_ALL,	/* mask for all languages (but not internal) */
	L_cpp,	/* pseudo-language for cpp flags that cross languages */
	L_cc,
	L_CC,
	L_f77,
	L_f90,
	L_as,
	L_ld,
	L_internal,	/* pseudo-language to mark internal options */
	L_LAST
} languages_t;

/* kinds of source files */
typedef enum {
	S_NONE,
	S_c,
	S_C,
	S_f,
	S_F,
	S_f90,
	S_F90,
	S_r,
	S_i,
	S_ii,
	S_s,
	S_S,
	S_I,
	S_B,
	S_P,
	S_N,
	S_O,
	S_o,
	S_LAST
} source_kind_t;

/* phases */
typedef enum {
	P_NONE,
	P_All,

	P_m4,		/* m4 */
	P_ratfor,	/* ratfor */

	P_cpp,		/* old-style cpp */
	P_gcpp,		/* gcc cpp */
	P_gcpp_plus,	/* g++ cpp */
	P_c_cpp,	/* cpp for C */
	P_cplus_cpp,	/* cpp for C++ */
	P_f_cpp,	/* cpp for F77 */
	P_f90_cpp,	/* cpp for F90 */
	P_f_coco,	/* coco preprocessor for Fortran	bug 9058 */
	P_any_cpp,	/* generic union of cpp's */

	P_pfa,		/* power fortran optimizer */
	P_pca,		/* power c optimizer */
        P_mpc,          /* multi processing c */
	P_any_optfe,	/* generic union of all fe optimizers */

	P_f_fe,		/* f77 fe */
	P_cppf_fe,	/* cpp embedded in f77 fe */
	P_f90_fe,	/* f90 fe */
	P_cppf90_fe,	/* cpp embedded in f90 fe */
	P_c_gfe,	/* gcc c fe */
	P_cplus_gfe,	/* gnu c++ fe */
	P_spin_cc1,	/* gnu gcc4 C fe */
	P_spin_cc1plus,	/* gnu gcc4 C++ fe */
	P_wgen,		/* wgen */
	P_any_fe,	/* generic union of all fe's */
	P_pseudo_f_fe,	/* not a real phase, just a placeholder 
			 * for options for f_fe but not c_fe */
	P_pseudo_c_fe,	/* not a real phase, just a placeholder 
			 * for options for c{plus}_fe but not f_fe */
	P_lister,	/* F90 Lister */

	P_inline,	/* single-file inliner */
	P_ipl,		/* pre-ipa */
	p_any_ipl,	/* either ipl or inline */

	P_be,		/* composite optimizing back-end */
#if defined(TARG_NVISA)
	P_bec,		/* simple w2c back-end */
	P_any_be,	/* generic union of all be's */
#endif
	P_as,		/* gnu assembler */
	P_gas,		/* gnu assembler */
	P_any_as,	/* generic union of all asm's */

	P_dsm_prelink,  /* support for distributed reshape automatic cloning */
	P_ipa_link,	/* ipa prelinker */
	P_collect,	/* gnu linker wrapper */
	P_ld,		/* loader */
	P_ldplus,	/* loader for c++ */
	P_any_ld,	/* generic union of all ld's */
	P_cord,		/* cord */
	P_pixie,	/* pixie */
	P_prof,		/* Prof */
	P_ar,		/* Archive */

	/* because -Y can also modify libraries, we include library places */
	P_startup,
#if defined(TARG_SL)
	P_sl5_startup,
#endif
	P_include,
	P_library,
	P_alt_library,	/* alternate library path */

	P_LAST
} phases_t;

extern languages_t invoked_lang;	/* language we invoked */
extern languages_t source_lang;		/* language of source file */

extern source_kind_t source_kind;	/* kind of source file */
extern source_kind_t default_source_kind;  /* default when ignoring suffix */

extern phases_t first_phase;	/* first phase to run */
extern phases_t last_phase;	/* last phase to run */
extern phases_t remember_last_phase;

extern phases_t current_phase;	/* phase currently being run */

/* return earliest phase */
extern phases_t earliest_phase (phases_t a, phases_t b);

/* get language associated with key */
extern languages_t get_language (char key);
/* get phase associated with key */
extern phases_t get_phase (char key);

typedef long long mask_t;
extern mask_t OPEN64_PHASE_MASK; /* mask for all open64 phases */
extern mask_t PHASE_MASK;	/* mask for all phases */
extern mask_t LIB_MASK;		/* mask for all libraries */
/* get mask associated with language and phase */
extern mask_t get_language_mask (languages_t i);
extern mask_t get_phase_mask (phases_t i);

/* Replace the directory of all matching phases. */
extern void substitute_phase_dirs (char *orig_dir, char *new_dir, char *leaf);
/* set phase dir for given mask */
extern void set_phase_dir (mask_t mask, char *path);
/* prefix with path all phase dirs that match the mask */
extern void prefix_all_phase_dirs (mask_t mask, char *path);
/* replace all symbol link with real path */
extern void get_phases_real_path (void);
/* append with path all phase dirs that match the mask */
extern void append_all_phase_dirs (mask_t mask, char *path);
/* append path to end of phase dir */
extern void append_phase_dir (phases_t index, char *path);
/* return phase path */
extern char *get_phase_dir (phases_t index);
/* return LD_LIBRARY_PATH, if needed */
extern char *get_phase_ld_library_path (phases_t index);
/* return phase name */
extern char *get_phase_name (phases_t index);
/* return path and name of phase */
extern char *get_full_phase_name (phases_t index);
/* set phase name */
extern void set_phase_name (phases_t index, char *s);
/* Override a directory for a phase */
extern void override_phase(int phase, char *phase_name, char *new_path, char *new_name);

/* get language index associated with name */
extern languages_t get_named_language (char *name);
/* return language name */
extern char *get_lang_name (languages_t index);

/* get kind of source from suffix */
extern source_kind_t get_source_kind_from_suffix (char *suf);
/* get kind of source from file name */
extern source_kind_t get_source_kind (char *src);
extern boolean is_object_source_kind (char *src);
/* get suffix from source kind */
extern char *get_suffix_string (source_kind_t sk);

/* get language for source */
extern languages_t get_source_lang (source_kind_t sk);
extern boolean ignore_suffix;	/* ignore suffix when determing source lang */

/* whether the language mask matches the specified language */
extern boolean is_matching_language (mask_t lang_mask, languages_t l);
/* whether the phase mask matches the specified phase */
extern boolean is_matching_phase (mask_t phase_mask, phases_t p);

extern boolean show_but_not_run;

typedef enum {
	M_SMALL,
	M_MEDIUM,
	M_LARGE,
	M_KERNEL,
	M_LAST,
} mem_model_t;

extern mem_model_t mem_model;
extern char *mem_model_name;

#ifdef PATH_MAX
#define PATH_BUF_LEN PATH_MAX
#else
#define PATH_BUF_LEN 4096
#endif

extern char *read_cmd_out(char *cmd, char *out_buf);

#endif

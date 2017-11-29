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


#include "basic.h"

/* values of options */

#define UNDEFINED	-1	/* for any undefined option */

#define ENDIAN_BIG	0
#define ENDIAN_LITTLE	1

#define EXTENDED_ANSI	0
#define KR_ANSI		1
#define STRICT_ANSI	2
#define POSIX		3

#define CALL_SHARED	0
#define NON_SHARED	1
#define DSO_SHARED	2
#define RELOCATABLE	4

#define NORMAL_MP	0
#define CRAY_MP		1

typedef enum {
  M_CPU,
  M_ARCH,
  M_TUNE,
} m_flag;
  
typedef enum {
  ABI_RAG32,
  ABI_N32,
  ABI_64,
  ABI_I32,
  ABI_I64,
  ABI_IA32,
  ABI_W64,
  ABI_P32
} ABI;
typedef enum {
  ISA_NONE      = 0,
  ISA_MIPS1     = 1,
  ISA_MIPS2     = 2,
  ISA_MIPS3     = 3,
  ISA_MIPS4     = 4,
  ISA_MIPS5     = 5,
  ISA_MIPS6     = 6,
  ISA_MIPSX     = 9,
  ISA_MIPS64    = 10,
  ISA_IA641     = 11,
  ISA_IA32      = 12,
  ISA_X8664     = 13,
  ISA_COMPUTE_10 = 14,
  ISA_COMPUTE_11 = 15,
  ISA_COMPUTE_12 = 16,
  ISA_COMPUTE_13 = 17,
  ISA_SL         = 18,
  ISA_PPC32	= 19,
#ifdef TARG_LOONGSON /* add support for 2e,2f and 3 decoding*/
  ISA_LOONGSON2e =14,
  ISA_LOONGSON2f =15,
  ISA_LOONGSON3 =16
#endif
} ISA;

extern boolean debug;		/* debugging turned on */

extern boolean nostdinc;	/* no standard include directory */

extern boolean print_help;	/* print help message */

extern char *help_pattern;	/* pattern string for help file */

extern int inline_t;            /* toggle for inline options */

/* Before front-end: UNDEFINED.  After front-end: TRUE if inliner will be run.
   Bug 11325. */
extern int run_inline;

extern boolean dashdash_flag;   /* when you see -- set this flag to
				   indicate the end of the options */

extern boolean read_stdin;	/* read stdin for input */

extern boolean  xpg_flag;

extern int default_olevel;	/* default optimization level */

extern int ofast;		/* Ofast has been set */

extern int instrumentation_invoked;	/* Instrument whirl */

extern boolean ftz_crt;		/* add flush-to-zero crt */

extern char *f90_module_dir;	/* value of -module option */	// bug 4210
extern int malloc_algorithm;

/* return whether has been toggled yet */
extern boolean is_toggled (int obj);

/* set obj to value; allow many toggles; last toggle is final value */
extern void toggle (int *obj, int value);

/* Options for configuring the target: */

/* Verify that the target selection is consistent and set defaults: */
extern void Check_Target ( void );

/* process -F option (different for Fortran and C++) */
extern void dash_F_option(void);

/* untoggle the object, so it can be re-toggled later */
extern void untoggle (int *obj, int value);

/* save value in string */
extern void save_name (char **obj, char *value);

/* do action associated with flag */
extern void opt_action (int optflag);

/* return whether to mark this flag as unseen */
extern boolean flag_is_superceded (int optflag);

extern void check_convert_name(char *name);

/* check the tls-model is acceptable */
extern void check_opt_tls_model(char *model);

/* check if there is a -- and _XPG is set */
extern void check_dashdash ( void );

/* set options for DSM options */
extern void set_dsm_options (void);

extern void Process_Mp (void);
extern void Process_Cray_Mp (void);

extern void toggle_inline_on (void);

extern void print_file_path (char *, int);	/* print path to named file */

extern int subverbose ;

extern char *target_cpu;

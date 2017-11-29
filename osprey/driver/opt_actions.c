/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007. PathScale, LLC. All Rights Reserved.
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


#include <string.h>
#include <stdlib.h>
#include <sys/param.h>
#if !defined(_WIN32)
#include <sys/utsname.h>
#endif
#include <unistd.h>
#include "cmplrs/rcodes.h"
#include "opt_actions.h"
#include "options.h"
#include "option_names.h"
#include "option_seen.h"
#include "lang_defs.h"
#include "errors.h"
#include "file_utils.h"
#include "file_names.h"
#include "string_utils.h"
#include "get_options.h"
#include "objects.h"
#include "pathscale_defs.h"
#include "phases.h"
#include "run.h"
#include "profile_type.h" /* for enum PROFILE_TYPE */

/* keep list of previous toggled option names, to give better messages */
typedef struct toggle_name_struct {
	int *address;
	char *name;
} toggle_name;
#define MAX_TOGGLES	50
static toggle_name toggled_names[MAX_TOGGLES];
static int last_toggle_index = 0;
static int inline_on_seen = FALSE;
int inline_t = UNDEFINED;
/* Before front-end: UNDEFINED.  After front-end: TRUE if inliner will be run.
   Bug 11325. */
int run_inline;
int malloc_algorithm = UNDEFINED;
boolean dashdash_flag = FALSE;
boolean read_stdin = FALSE;
boolean xpg_flag = FALSE;
#if defined(TARG_NVISA)
// default to modified O3 (supports unrolling)
int default_olevel = 3;
#else
int default_olevel = 2;
#endif
static int default_isa = UNDEFINED;
static int default_proc = UNDEFINED;
int instrumentation_invoked = UNDEFINED;
int profile_type = 0;
boolean ftz_crt = FALSE;
//int isa = UNDEFINED;
int proc = UNDEFINED;
#ifdef TARG_X8664
static int target_supported_abi = UNDEFINED;
static boolean target_supports_mmx = FALSE;
static boolean target_supports_sse = FALSE;
static boolean target_supports_sse2 = FALSE;
static boolean target_prefers_sse3 = FALSE;
static boolean target_supports_sse4a = FALSE;
static boolean target_supports_ssse3 = FALSE;
static boolean target_supports_sse41 = FALSE;
static boolean target_supports_sse42 = FALSE;
static boolean target_supports_aes = FALSE;
static boolean target_supports_pclmul = FALSE;
static boolean target_supports_avx = FALSE;
static boolean target_supports_xop = FALSE;
static boolean target_supports_fma4 = FALSE;
static boolean target_supports_fma = FALSE;
#endif

extern boolean parsing_default_options;
extern boolean drop_option;

static void set_cpu(char *name, m_flag flag_type);
static void add_hugepage_desc(HUGEPAGE_ALLOC, HUGEPAGE_SIZE, int);

/* Run opencc from build directory (-run-build option)  */
int run_build = 0;

void set_memory_model(char *model);
static int get_platform_abi();

#ifdef TARG_X8664
static void Get_x86_ISA();
static boolean Get_x86_ISA_extensions();
#endif

/* ====================================================================
 *
 * -Ofast targets
 *
 * Given an -Ofast option, tables which map the IP numbers to
 * processors for use in Ofast_Target below.
 *
 * See common/com/MIPS/config_platform.h.
 *
 * PV 378171:  Change this and config.c to use an external table.
 *
 * ====================================================================
 */

int ofast = UNDEFINED;	/* -Ofast toggle -- implicit in Process_Ofast */


static void
add_toggle_name (int *obj, char *name)
{
	int i;
	for (i = 0; i < last_toggle_index; i++) {
		if (obj == toggled_names[i].address) {
			break;
		}
	}
	if (i == last_toggle_index) {
		if (last_toggle_index >= MAX_TOGGLES) {
			internal_error("too many toggle names\n");
		} else {
			last_toggle_index++;
		}
	}
	toggled_names[i].address = obj;
	toggled_names[i].name = string_copy(option_name);
}

static char *
get_toggle_name (int *obj)
{
	int i;
	for (i = 0; i < last_toggle_index; i++) {
		if (obj == toggled_names[i].address) {
			return toggled_names[i].name;
		}
	}
	internal_error("no previously toggled name?");
	return "<unknown>";
}

/* return whether has been toggled yet */
boolean
is_toggled (int obj)
{
	return (obj != UNDEFINED);
}

/* set obj to value; allow many toggles; last toggle is final value */
void
toggle (int *obj, int value)
{
	// Silently drop a default option if it is already toggled on the
	// command line.
	if (parsing_default_options &&
	    is_toggled(*obj)) {
	  drop_option = TRUE;
	  return;
	}

	if (*obj != UNDEFINED && *obj != value) {
		warning ("%s conflicts with %s; using latter value (%s)", 
			get_toggle_name(obj), option_name, option_name);
	}
	*obj = value;
	add_toggle_name(obj, option_name);
}

/* ====================================================================
 *
 * Get_Group_Option_Value
 *
 * Given a group option string, search for the option with the given
 * name.  Return NULL if not found, the option value if found ("" if
 * value is empty).
 *
 * ====================================================================
 */

static char *
Get_Group_Option_Value (
  char *arg,	/* Raw option string */
  char *name,	/* Suboption full name */
  char *abbrev)	/* Suboption abbreviation */
{
  char *endc = arg;
  int n;

  while ( TRUE ) {
    n = strcspn ( arg, ":=" );
    if ( strncasecmp ( arg, abbrev, strlen(abbrev) ) == 0
      && strncasecmp ( arg, name, n ) == 0 )
    {
      endc += n;
      if ( *endc == '=' ) {
	/* Duplicate value lazily: */
	char *result = strdup ( endc+1 );

	* ( result + strcspn ( result, ":=" ) ) = 0;
	return result;
      } else {
	/* No value: */
	return "";
      }
    }
    if ( ( endc = strchr ( arg, ':' ) ) == NULL ) return NULL;
    arg = ++endc;
  }

  /* Shouldn't get here, but ... */
  /* return NULL;  compiler gets better */
}

/* ====================================================================
 *
 * Bool_Group_Value
 *
 * Given a group option value string for a Boolean group value,
 * determine whether it is TRUE or FALSE.
 *
 * ====================================================================
 */

static boolean
Bool_Group_Value ( char *val )
{
  if ( *val == 0 ) {
    /* Empty string is TRUE for group options */
    return TRUE;
  }

  if ( strcasecmp ( val, "OFF" ) == 0
    || strcasecmp ( val, "NO" ) == 0
    || strcasecmp ( val, "FALSE" ) == 0
    || strcasecmp ( val, "0" ) == 0 )
  {
    return FALSE;
  } else {
    return TRUE;
  }
}

/* ====================================================================
 *
 * Routine to process "-module dirname" and pass "-Jdirname" to Fortran
 * front end
 *
 * ====================================================================
 */
char *f90_module_dir = 0;

void
Process_module ( char *dirname )
{
  if (0 != f90_module_dir)
  {
    error("Only one -module option allowed");
  }
  strcat(
    strcpy(
      f90_module_dir = malloc(sizeof "-J" + strlen(dirname)),
      "-J"),
    dirname);
}

/* ====================================================================
 *
 * Routine to manage the implications of -Ofast.
 *
 * Turn on -O3 and -IPA.  Check_Target below will deal with the ABI and
 * ISA implications later.
 *
 * ====================================================================
 */

void
Process_Ofast ( char *ipname )
{
  int flag;
  char *suboption;

  /* -O3: */
  if (!Gen_feedback) {
     O3_flag = TRUE;
     toggle ( &olevel, 3 );
     add_option_seen ( O_O3 );

#ifdef TARG_IA64
     ftz_crt = TRUE;	// flush to zero
#endif

     /* -fno-math-errno */
     toggle ( &fmath_errno, 0);
     add_option_seen (O_fno_math_errno);

     /* -ffast-math */
     toggle ( &ffast_math, 1);
     add_option_seen (O_ffast_math);

     /* -IPA: */
     toggle ( &ipa, TRUE );
     add_option_seen ( O_IPA );

     /* -OPT:Ofast=ipname
      * We will call add_string_option using O_OPT_; if the descriptor
      * for it in OPTIONS changes, this code might require change...
      * Build the "Ofast=ipname" string, then call add_string_option:
      */
     toggle ( &ofast, TRUE );
     suboption = concat_strings ( "Ofast=", ipname );
     flag = add_string_option ( O_OPT_, suboption );
     add_option_seen ( flag );
   } else {
     suboption = concat_strings ( "platform=", ipname );
     flag = add_string_option ( O_TARG_, suboption );
     add_option_seen ( flag );
   }
}

/* ====================================================================
 *
 * Process_Opt_Group
 *
 * We've found a -OPT option group.  Inspect it for -OPT:reorg_common
 * options, and set -split_common and -ivpad accordingly.
 *
 * NOTE: We ignore anything that doesn't match what's expected --
 * the compiler will produce reasonable error messages for junk.
 *
 * ====================================================================
 */

void
Process_Opt_Group ( char *opt_args )
{
  char *optval = NULL;

  if ( debug ) {
    fprintf ( stderr, "Process_Opt_Group: %s\n", opt_args );
  }
  
  /* Go look for -OPT:instrument */
  optval = Get_Group_Option_Value ( opt_args, "instrument", "instr");
  if (optval != NULL) {
     instrumentation_invoked = TRUE;
  }

  /* Go look for -OPT:reorg_common: */
  optval = Get_Group_Option_Value ( opt_args, "reorg_common", "reorg");
  if ( optval != NULL && Bool_Group_Value(optval)) {
  }

  /* Go look for -OPT:malloc_algorithm */
  optval = Get_Group_Option_Value(opt_args, "malloc_algorithm", "malloc_alg");
  if (optval != NULL &&
      atoi(optval) > 0) {
#ifdef TARG_X8664	// Option available only for x86-64.  Bug 12431.
     malloc_algorithm = atoi(optval);
#else
     warning("ignored -OPT:malloc_algorithm because option not supported on"
	     " this architecture");
#endif
  }
}

void
Process_Default_Group (char *default_args)
{
  char *s;
  int i;

  if ( debug ) {
    fprintf ( stderr, "Process_Default_Group: %s\n", default_args );
  }

  /* Go look for -DEFAULT:isa=mipsN: */
  s = Get_Group_Option_Value ( default_args, "isa", "isa");
  if (s != NULL && same_string_prefix (s, "mips")) {
	default_isa = atoi(s + strlen("mips"));
  }
  /* Go look for -DEFAULT:opt=[0-3]: */
  s = Get_Group_Option_Value ( default_args, "opt", "opt");
  if (s != NULL) {
	default_olevel = atoi(s);
  }
  /* Go look for -DEFAULT:arith=[0-3]: */
  s = Get_Group_Option_Value ( default_args, "arith", "arith");
  if (s != NULL) {
	i = add_string_option (O_OPT_, concat_strings("IEEE_arith=", s));
	add_option_seen (i);
  }
}

/* ====================================================================
 *
 * Routines to manage the target selection (ABI, ISA, and processor).
 *
 * Make sure that the driver picks up a consistent view of the target
 * selected, based either on user options or on defaults.
 *
 * ====================================================================
 */

/* ====================================================================
 *
 * Process_Targ_Group
 *
 * We've found a -TARG option group.  Inspect it for ABI, ISA, and/or
 * processor specification, and toggle the state appropriately.
 *
 * NOTE: We ignore anything that doesn't match what's expected --
 * the compiler will produce reasonable error messages for junk.
 *
 * ====================================================================
 */

void
Process_Targ_Group ( char *targ_args )
{
  char *cp = targ_args;	/* Skip -TARG: */
  char *cpeq;
  char *ftz;

  if ( debug ) {
    fprintf ( stderr, "Process_Targ_Group: %s\n", targ_args );
  }

  ftz = Get_Group_Option_Value ( targ_args, "flush_to_zero", "flush_to_zero");
  if ( ftz != NULL && Bool_Group_Value(ftz)) {
    /* link in ftz.o */
    ftz_crt = TRUE;
  }

  while ( *cp != 0 ) {
    switch ( *cp ) {
      case '3':
#ifdef TARG_X8664
	if (!strncasecmp(cp, "3dnow=on", 9)) {
	  add_option_seen(O_m3dnow);
	  toggle(&m3dnow, TRUE);
	} else if (!strncasecmp(cp, "3dnow=off", 10)) {
	  add_option_seen(O_mno_3dnow);
	  toggle(&m3dnow, FALSE);
	}
	break;
#endif

      case 'a':
	if ( strncasecmp ( cp, "abi", 3 ) == 0 && *(cp+3) == '=' ) {
#ifdef TARG_MIPS
	  if ( strncasecmp ( cp+4, "n32", 3 ) == 0 ) {
	    add_option_seen ( O_n32 );
	    toggle ( &abi, ABI_N32 );
	  }
#ifndef TARG_SL 
          else if ( strncasecmp ( cp+4, "64", 2 ) == 0 ) {
	    add_option_seen ( O_m64 );
	    toggle ( &abi, ABI_64 );
	  }
#endif
#endif
#ifdef TARG_X8664
	  // The driver needs to handle all the -TARG options that it gives to
	  // the back-end, even if these -TARG options are not visible to the
	  // user.  This is because IPA invokes the driver with back-end
	  // options.  Bug 5466.
	  if ( strncasecmp ( cp+4, "n32", 3 ) == 0 ) {
	    add_option_seen ( O_m32 );
	    toggle ( &abi, ABI_N32 );
	  } else if ( strncasecmp ( cp+4, "n64", 3 ) == 0 ) {
	    add_option_seen ( O_m64 );
	    toggle ( &abi, ABI_64 );
	  }
#endif
#ifdef TARG_LOONGSON
	  if ( strncasecmp ( cp+4, "n32", 3 ) == 0 ) {
            add_option_seen ( O_n32 );
            toggle ( &abi, ABI_N32 );
          } else if ( strncasecmp ( cp+4, "n64", 3 ) == 0 ) {
            add_option_seen ( O_n64 );
            toggle ( &abi, ABI_64 );
          }
#endif
#if defined(TARG_NVISA)
	  if ( strncasecmp ( cp+4, "w64", 3 ) == 0 ) {
	    add_option_seen ( O_w64 );
	    toggle ( &abi, ABI_W64 );
	  }
#endif
	} else {
#ifdef TARG_X8664
          // non abi TARG options
          // aes and avx
	  if (!strncasecmp(cp, "aes=on", 6)){
	    add_option_seen(O_maes);
	    toggle(&aes, TRUE);
	  }else if (!strncasecmp(cp, "aes=off", 7)){
	    add_option_seen(O_mno_aes);
	    toggle(&aes, FALSE);
	  }else if (!strncasecmp(cp, "avx=on", 6)){
	    add_option_seen(O_mavx);
	    toggle(&avx, TRUE);
	  }else if (!strncasecmp(cp, "avx=off", 7)){
	    add_option_seen(O_mno_avx);
	    toggle(&avx, FALSE);
          }
#endif
	}
	break;

      case 'f':
#ifdef TARG_X8664
	if (!strncasecmp(cp, "fma=on", 7)){
	  add_option_seen(O_mfma);
	  toggle(&fma, TRUE);
	}else if (!strncasecmp(cp, "fma=off", 8)){
	  add_option_seen(O_mfma);
	  toggle(&fma, FALSE);
        }
	if (!strncasecmp(cp, "fma4=on", 7)){
	  add_option_seen(O_mfma4);
	  toggle(&fma4, TRUE);
	}else if (!strncasecmp(cp, "fma4=off", 8)){
	  add_option_seen(O_mfma4);
	  toggle(&fma4, FALSE);
        }
#endif
	break;

      case 'i':
	/* We support both isa=mipsn and plain mipsn in group.
	 * Simply move cp to point to value, and fall through to
	 * 'm' case:
	 */
	if ( strncasecmp ( cp, "isa", 3 ) != 0 || *(cp+3) != '=' ) {
	  break;
	} else {
	  cp += 4;
	}
	/* Fall through */

      case 'm':
#ifdef TARG_MIPS
	if ( strncasecmp ( cp, "mips", 4 ) == 0 ) {
	  if ( '1' <= *(cp+4) && *(cp+4) <= '6' ) {
	    toggle ( &isa, *(cp+4) - '0' );
#ifndef TARG_SL
	    switch ( isa ) {
	      case 1:	add_option_seen ( O_mips1 );
			break;
	      case 2:	add_option_seen ( O_mips2 );
			break;
	      case 3:	add_option_seen ( O_mips3 );
			break;
	      case 4:	add_option_seen ( O_mips4 );
			break;
	      default:	error ( "invalid ISA: %s", cp );
			break;
	    }
#endif
	  }
	}
#endif
#ifdef TARG_X8664
	if (!strncasecmp(cp, "mmx=on", 6)) {
	  add_option_seen(O_mmmx);
	  toggle(&mmx, TRUE);
	} else if (!strncasecmp(cp, "mmx=off", 7)) {
	  add_option_seen(O_mno_mmx);
	  toggle(&mmx, FALSE);
	}
#endif
	break;

      case 'p':
	if (!strncasecmp(cp, "processor=", 10)) {
	  char *target = cp + 10;
	  set_cpu (target, M_ARCH);
	}
#ifdef TARG_X8664
	if (!strncasecmp(cp, "pclmul=on", 9)){
	  add_option_seen(O_mpclmul);
	  toggle(&pclmul, TRUE);
	}else if (!strncasecmp(cp, "pclmul=off", 10)){
	  add_option_seen(O_mno_pclmul);
	  toggle(&pclmul, FALSE);
        }
#endif
	break;

      case 's':
#ifdef TARG_X8664
        // all sse flags
	if (!strncasecmp(cp, "sse=on", 6)) {
	  add_option_seen(O_msse);
	  toggle(&sse, TRUE);
	} else if (!strncasecmp(cp, "sse=off", 7)) {
	  add_option_seen(O_mno_sse);
	  toggle(&sse, FALSE);
	} else if (!strncasecmp(cp, "sse2=on", 7)) {
	  add_option_seen(O_msse2);
	  toggle(&sse2, TRUE);
	} else if (!strncasecmp(cp, "sse2=off", 8)) {
	  add_option_seen(O_mno_sse2);
	  toggle(&sse2, FALSE);
	} else if (!strncasecmp(cp, "sse3=on", 7)) {
	  add_option_seen(O_msse3);
	  toggle(&sse3, TRUE);
	} else if (!strncasecmp(cp, "sse3=off", 8)) {
	  add_option_seen(O_mno_sse3);
	  toggle(&sse3, FALSE);
	}else if (!strncasecmp(cp, "sse4a=on", 8)){
	  add_option_seen(O_msse4a);
	  toggle(&sse4a, TRUE);
	}else if (!strncasecmp(cp, "sse4a=off", 9)){
	  add_option_seen(O_mno_sse4a);
	  toggle(&sse4a, FALSE);
	}else if (!strncasecmp(cp, "ssse3=on", 8)){
	  add_option_seen(O_mssse3);
	  toggle(&ssse3, TRUE);
	}else if (!strncasecmp(cp, "ssse3=off", 9)){
	  add_option_seen(O_mno_ssse3);
	  toggle(&ssse3, FALSE);
	}else if (!strncasecmp(cp, "sse41=on", 8)){
	  add_option_seen(O_msse41);
	  toggle(&sse41, TRUE);
	}else if (!strncasecmp(cp, "sse41=off", 9)){
	  add_option_seen(O_mno_sse41);
	  toggle(&sse41, FALSE);
	}else if (!strncasecmp(cp, "sse42=on", 8)){
	  add_option_seen(O_msse42);
	  toggle(&sse42, TRUE);
	}else if (!strncasecmp(cp, "sse42=off", 9)){
	  add_option_seen(O_mno_sse42);
	  toggle(&sse42, FALSE);
        }
#endif
	break;

      case 'x':
#ifdef TARG_X8664
	if (!strncasecmp(cp, "xop=on", 6)){
	  add_option_seen(O_mxop);
	  toggle(&xop, TRUE);
	}else if (!strncasecmp(cp, "xop=off", 7)){
	  add_option_seen(O_mno_xop);
	  toggle(&xop, FALSE);
	}
#endif
	break;
    }

    /* Skip to the next group option: */
    while ( *cp != 0 && *cp != ':' ) ++cp;
    if ( *cp == ':' ) ++cp;
  }
}


/* ====================================================================
 *
 * Check_Target
 *
 * Verify that the target selection is consistent and set defaults.
 *
 * ====================================================================
 */

void
Check_Target ( void )
{
#ifdef TARG_PPC32
  abi  = ABI_P32;
  isa  = ISA_PPC32;
  proc = 0;
  return;
#else
  int opt_id;
  int opt_val;

  if ( debug ) {
    fprintf ( stderr, "Check_Target ABI=%d ISA=%d Processor=%d\n",
	      abi, isa, proc );
  }

#ifdef TARG_X8664
  if (target_cpu == NULL) {
    set_cpu ("auto", M_ARCH);	// Default to auto.
  }

  // Uses ABI to determine ISA.  If ABI isn't set, it guesses and sets the ABI.
  Get_x86_ISA();
#endif

  if (abi == UNDEFINED) {
#ifdef TARG_IA64
	toggle(&abi, ABI_I64);
    	add_option_seen ( O_i64 );
#elif TARG_IA32
	toggle(&abi, ABI_IA32);
    	add_option_seen ( O_ia32 );
#elif defined(TARG_SL) || defined(TARG_LOONGSON)
        toggle(&abi, ABI_N32);
        add_option_seen ( O_n32 );
#elif TARG_MIPS
	toggle(&abi, ABI_64);
    	add_option_seen ( O_64 );
#elif TARG_X8664
	// User didn't specify ABI.  Use the ABI supported on host.  Bug 8488.
	if (target_supported_abi == ABI_N32) {
	  abi = ABI_N32;
	} else if (target_supported_abi == ABI_64) {
	  abi = (get_platform_abi() == ABI_N32) ?
		  ABI_N32 : target_supported_abi;
	} else if (target_supported_abi == UNDEFINED) {
	  abi = (get_platform_abi() == ABI_64) ? ABI_64 : ABI_N32;
	} else {
	  internal_error ("illegal target_supported_abi");
	}

	if (abi == ABI_64)
	  add_option_seen (O_m64);
	else
	  add_option_seen (O_m32);
#elif TARG_NVISA
	abi = get_platform_abi();
	if (abi == ABI_64)
	  add_option_seen (O_m64);
	else if (abi == ABI_W64)
	  add_option_seen (O_w64);
	else
	  add_option_seen (O_m32);
#else
	warning("abi should have been specified by driverwrap");
  	/* If nothing is defined, default to -n32 */
    	toggle ( &abi, ABI_N32 );
    	add_option_seen ( O_n32 );
#endif
  }

#ifdef TARG_X8664
  // ABI must be set.
  if (!Get_x86_ISA_extensions())
    return;	// If error, quit instead of giving confusing error messages.
#endif

  /* Check ABI against ISA: */
  if ( isa != UNDEFINED ) {
    switch ( abi ) {
#if defined(TARG_MIPS) && !defined(TARG_SL)
      case ABI_N32:
	if ( isa < ISA_MIPS3 ) {
	  add_option_seen ( O_mips3 );
	  warning ( "ABI specification %s conflicts with ISA "
		    "specification %s: defaulting ISA to mips3",
		    get_toggle_name (&abi),
		    get_toggle_name (&isa) );
	  option_name = get_option_name ( O_mips3 );
	  isa = UNDEFINED;	/* To avoid another message */
	  toggle ( &isa, ISA_MIPS3 );
	}
	break;

      case ABI_64:
	if ( isa < ISA_MIPS3 ) {
	  /* Default to -mips4 if processor supports it: */
	  if ( proc == UNDEFINED || proc >= PROC_R5K ) {
	    opt_id = O_mips4;
	    opt_val = ISA_MIPS4;
	    add_option_seen ( O_mips4 );
	  } else {
	    opt_id = O_mips3;
	    opt_val = ISA_MIPS3;
	    add_option_seen ( O_mips3 );
	  }
	  warning ( "ABI specification %s conflicts with ISA "
		    "specification %s: defaulting ISA to mips%d",
		    get_toggle_name (&abi),
		    get_toggle_name (&isa),
		    opt_val );
	  option_name = get_option_name ( opt_id );
	  isa = UNDEFINED;	/* To avoid another message */
	  toggle ( &isa, opt_val );
	}
	break;
#endif
    }

  } else {
    /* ISA is undefined, so derive it from ABI and possibly processor: */

    switch ( abi ) {
#ifdef TARG_SL
      case ABI_N32:
      case ABI_64:
        opt_val = ISA_MIPS64;
        opt_id = O_mips64;
        toggle ( &isa, opt_val );
        add_option_seen ( opt_id );
        option_name = get_option_name ( opt_id );
        break;
#elif TARG_MIPS
      case ABI_N32:
      case ABI_64:
        if (default_isa == ISA_MIPS3) {
	  opt_val = ISA_MIPS3;
	  opt_id = O_mips3;
	}
	else if (default_isa == ISA_MIPS4) {
	  opt_val = ISA_MIPS4;
	  opt_id = O_mips4;
	}
	else {
	  opt_val = ISA_MIPS64;
	  opt_id = O_mips64;
	}
	toggle ( &isa, opt_val );
	add_option_seen ( opt_id );
	option_name = get_option_name ( opt_id );
	break;
#elif TARG_X8664
      case ABI_N32:
      case ABI_64:
	  opt_val = ISA_X8664;
	  toggle ( &isa, opt_val );
	break;
#elif TARG_NVISA
      case ABI_N32:
      case ABI_64:
      case ABI_W64:
	opt_val = ISA_COMPUTE_10;
	toggle ( &isa, opt_val );
	break;
#elif TARG_LOONGSON
      case ABI_N32:
      case ABI_64:
	  opt_val = ISA_LOONGSON3;
	  toggle ( &isa, opt_val );
	break;
#endif
      case ABI_I32:
      case ABI_I64:
	opt_val = ISA_IA641;
	toggle ( &isa, opt_val );
	break;
      case ABI_IA32:
	opt_val = ISA_IA32;
	toggle ( &isa, opt_val );
	break;
    }
  }
  if (isa == UNDEFINED) {
	internal_error ("isa should have been defined by now");
  }

  /* Check ABI against processor: */
  if ( proc != UNDEFINED ) {
    switch ( abi ) {
#ifdef TARG_MIPS
      case ABI_N32:
      case ABI_64:
#ifndef TARG_SL
	if ( proc < PROC_R4K ) {
	  warning ( "ABI specification %s conflicts with processor "
		    "specification %s: defaulting processor to r10000",
		    get_toggle_name (&abi),
		    get_toggle_name (&proc) );
	  option_name = get_option_name ( O_r10000 );
	  proc = UNDEFINED;	/* To avoid another message */
	  add_option_seen ( O_r10000 );
	  toggle ( &proc, PROC_R10K );
	}
#endif
	break;
#endif
    }
  }

  /* Check ISA against processor: */
  if ( proc != UNDEFINED ) {
    switch ( isa ) {
#if defined(TARG_MIPS) && !defined(TARG_SL)
      case ISA_MIPS1:
	/* Anything works: */
	break;

      case ISA_MIPS2:
      case ISA_MIPS3:
	if ( proc < PROC_R4K ) {
	  warning ( "ISA specification %s conflicts with processor "
		    "specification %s: defaulting processor to r10000",
		    get_toggle_name (&isa),
		    get_toggle_name (&proc) );
	  add_option_seen ( O_r10000 );
	  proc = UNDEFINED;	/* To avoid another message */
	  option_name = get_option_name ( O_r10000 );
	  toggle ( &proc, PROC_R10K );
	}
	break;

      case ISA_MIPS4:
	if ( proc < PROC_R5K ) {
	  warning ( "ISA specification %s conflicts with processor "
		    "specification %s: defaulting processor to r10000",
		    get_toggle_name (&isa),
		    get_toggle_name (&proc) );
	  add_option_seen ( O_r10000 );
	  proc = UNDEFINED;	/* To avoid another message */
	  option_name = get_option_name ( O_r10000 );
	  toggle ( &proc, PROC_R10K );
	}
	break;
#endif
    }
  }
  else if (default_proc != UNDEFINED) {
	/* set proc if compatible */
	opt_id = 0;
#if defined(TARG_MIPS) && !defined(TARG_SL)
	switch (default_proc) {
	case PROC_R4K:
		if (isa <= ISA_MIPS3) {
			opt_id = O_r4000;
		}
		break;
	case PROC_R5K:
		opt_id = O_r5000;
		break;
	case PROC_R8K:
		opt_id = O_r8000;
		break;
	case PROC_R10K:
		opt_id = O_r10000;
		break;
	}
#endif
	if (abi == ABI_I64 || abi == ABI_IA32) {
		opt_id = 0;	/* no proc for i64, ia32 yet */
	}
	if (opt_id != 0) {
		add_option_seen ( opt_id );
		option_name = get_option_name ( opt_id );
		toggle ( &proc, default_proc);
	}
  }

  if ( debug ) {
    fprintf ( stderr, "Check_Target done; ABI=%d ISA=%d Processor=%d\n",
	      abi, isa, proc );
  }
#endif
}

/* ====================================================================
 *
 * Routines to manage inlining choices (the -INLINE group and friends).
 *
 * ====================================================================
 */

/* toggle inline for a normal option (not "=on" or "=off") */

static void
toggle_inline_normal(void)
{
  if (inline_t == UNDEFINED)
    inline_t = TRUE;
}

/* toggle inline for "=on" */

void
toggle_inline_on(void)
{
  if (inline_t == FALSE) {
    warning ("-noinline or -INLINE:=off has been seen, %s ignored",
	     option_name);
  }
  else {

    inline_t = TRUE;
    inline_on_seen = TRUE;
  }
}

/* toggle inline for "=off" */

static void
toggle_inline_off(void)
{
  if (inline_on_seen == TRUE) {
    warning ("Earlier request for inline processing has been overridden by %s",
	     option_name);
  }
  inline_t = FALSE;
}

void
Process_Profile_Arcs( void )
{
  if (strncmp (option_name, "-fprofile-arcs", 14) == 0)
    add_string_option (O_OPT_, "profile_arcs=true");
}

void
Process_Test_Coverage( void )
{
  if (strncmp (option_name, "-ftest-coverage", 15) == 0)
    add_string_option (O_CG_, "test_coverage=true");
}

/* process -INLINE option */
void
Process_Inline ( void )
{
  int more_symbols = TRUE;
  char *args = option_name+7;

  if (strncmp (option_name, "-noinline", 9) == 0
      || strncmp (option_name, "-fno-inline", 11) == 0)
      toggle_inline_off();
  else if (*args == '\0'
           || strncmp (option_name, "-finline", 8) == 0)
    /* Treat "-INLINE" like "-INLINE:=on" for error messages */
    toggle_inline_on();
  else do {
    char *endc;
    *args = ':';
    if ((endc = strchr(++args, ':')) == NULL)
      more_symbols = FALSE;
    else
      *endc = '\0';
    if (strcasecmp(args, "=off") == 0)
      toggle_inline_off();
    else if (strcasecmp(args, "=on") == 0)
      toggle_inline_on();
    else
      toggle_inline_normal();
    args = endc;
  }
  while (more_symbols);
}

/*
 * Processing -F option: ratfor-related stuff for Fortran, but
 * (obsolete) C code generation option in C++ and unknown for C.
 */
void dash_F_option(void)
{
    if (invoked_lang == L_f77) {
	last_phase=earliest_phase(P_ratfor,last_phase);
    } else if (invoked_lang == L_CC) {
	error("-F is not supported: cannot generate intermediate C code");
    } else {
	parse_error("-F", "unknown flag");
    }
}

/* untoggle the object, so it can be re-toggled later */
void
untoggle (int *obj, int value)
/*ARGSUSED*/
{
  *obj = UNDEFINED;
}

/* change path for particular phase(s), e.g. -Yb,/usr */
static void
change_phase_path (char *arg)
{
	char *dir;
	char *s;
	for (s = arg; s != NULL && *s != NIL && *s != ','; s++)
		;
	if (s == NULL || *s == NIL) {
		parse_error(option_name, "bad syntax for -Y option");
		return;
	}
	dir = s+1;
	if (dir[0] == '~' && (dir[1] == '/' || dir[1] == '\0')) {
	    char *home = getenv("HOME");
	    if (home)
		dir = concat_strings(home, dir+1);
	}
	if (!is_directory(dir))
		parse_error(option_name, "not a directory");
	for (s = arg; *s != ','; s++) {
		/* do separate check so can give better error message */
		if (get_phase(*s) == P_NONE) {
			parse_error(option_name, "bad phase for -Y option");
		} else {
			set_phase_dir(get_phase_mask(get_phase(*s)), dir);
			// Special case wgen because it is affected by -Yf but
			// is not considered a front-end (because it does not
			// take C/C++ front-end flags in OPTIONS).
			if (get_phase(*s) == P_any_fe)
			  set_phase_dir(get_phase_mask(P_wgen), dir);
		}
	}
}

/* Reset location of sub-pieces to run compiler from build directory.
   This overrides some settings from init_phase_info.  */

static void
run_from_build (char *builddir)
{
	char new_path[MAXPATHLEN];
	char new_ld_path[MAXPATHLEN];
	char ld_env[MAXPATHLEN];
	int builddir_len;

	run_build = 1;

	if (ld_library_path)
		strcpy(new_ld_path, ld_library_path);
	else
		new_ld_path[0] = NULL;

	builddir_len = strlen(builddir);
	strcpy(new_path, builddir);
	strcat(new_path, "/osprey-gcc-4.2.0/host-unknown/gcc");
	strcat(new_ld_path, ":");
	strcat(new_ld_path, new_path);
	override_phase(P_gcpp, "P_gcpp", new_path, "xgcc");
	override_phase(P_gas, "P_gas", new_path, "xgcc");
	override_phase(P_ld, "P_ld", new_path, "xgcc");
	override_phase(P_gcpp_plus, "P_gcpp_plus", new_path, "xgcc");
	override_phase(P_ldplus, "P_ldplus", new_path, "xgcc");
	override_phase(P_spin_cc1, "P_spin_cc1", new_path, "cc1");
	override_phase(P_spin_cc1plus, "P_spin_cc1plus", new_path, "cc1plus");
	new_path[builddir_len] = NULL;
	strcat(new_path, "/osprey/targdir/crayf90/sgi");
	override_phase(P_f90_fe, "P_f90_fe", new_path, "mfef95");
	new_path[builddir_len] = NULL;
        strcat(new_path, "/osprey/targdir/lw_inline");
	strcat(new_ld_path, ":");
	strcat(new_ld_path, new_path);
	override_phase(P_inline, "P_inline", new_path, "lw_inline");
	new_path[builddir_len] = NULL;
        strcat(new_path, "/osprey/targdir/wgen");
	strcat(new_ld_path, ":");
	strcat(new_ld_path, new_path);
	override_phase(P_wgen, "P_wgen", new_path, "wgen42");
	new_path[builddir_len] = NULL;
	strcat(new_path, "/osprey/targdir/be");
	strcat(new_ld_path, ":");
	strcat(new_ld_path, new_path);
	override_phase(P_be, "P_be", new_path, "be");
	new_path[builddir_len] = NULL;
	strcat(new_path, "/osprey/targdir/cg");
	strcat(new_ld_path, ":");
	strcat(new_ld_path, new_path);
	new_path[builddir_len] = NULL;
	strcat(new_path, "/osprey/targdir/lno");
	strcat(new_ld_path, ":");
	strcat(new_ld_path, new_path);
	new_path[builddir_len] = NULL;
	strcat(new_path, "/osprey/targdir/wopt");
	strcat(new_ld_path, ":");
	strcat(new_ld_path, new_path);
	new_path[builddir_len] = NULL;
	strcat(new_path, "/osprey/targdir/targ_info");
	strcat(new_ld_path, ":");
	strcat(new_ld_path, new_path);
	new_path[builddir_len] = NULL;
	strcat(new_path, "/osprey/targdir/orc_ict");
	strcat(new_ld_path, ":");
	strcat(new_ld_path, new_path);
	new_path[builddir_len] = NULL;
	strcat(new_path, "/osprey/targdir/orc_intel");
	strcat(new_ld_path, ":");
	strcat(new_ld_path, new_path);
	new_path[builddir_len] = NULL;

	ld_library_path = malloc(strlen(new_ld_path) + 1);

	strcpy(ld_library_path, new_ld_path);
	strcpy(ld_env, "LD_LIBRARY_PATH=");
	strcat(ld_env, new_ld_path);
	putenv(ld_env);
}

/* halt after a particular phase, e.g. -Hb */
/* but also process -H and warn its ignored */
static void
change_last_phase (char *s)
{
	phases_t phase;
	if (s == NULL || *s == NIL) {
		warn_ignored("-H");
	} else if ( *(s+1)!=NIL) {
		parse_error(option_name, "bad syntax for -H option");
	} else if ((phase=get_phase(*s)) == P_NONE) {
			parse_error(option_name, "bad phase for -H option");
	} else {
			last_phase=earliest_phase(phase, last_phase);
	}
}

void
save_name (char **obj, char *value)
{
	*obj = string_copy(value);
}

static void
check_output_name (char *name)
{
	if (name == NULL) return;
#if defined(TARG_NVISA)
	/* final output is assembly file, so allow overwriting that */
	if (get_source_kind(name) == S_s) 
          return;
	/* final output of -multicore is C file, so allow overwriting that */
	if (option_was_seen(O_multicore) && get_source_kind(name) == S_c) 
          return;
#endif
	if (get_source_kind(name) != S_o && file_exists(name)) {
		warning("%s %s will overwrite a file that has a source-file suffix", option_name, name);
	}
}

/* Disallow illegal name following "-convert" */
void
check_convert_name(char *name)
{
	static char *legal_names[] = {
	  "big_endian",
	  "big-endian",
	  "little_endian",
	  "little-endian",
	  "native"
	  };
	int i;
	for (i = 0; i < ((sizeof legal_names) / (sizeof *legal_names));
	  i += 1) {
	  if (0 == strcmp(name, legal_names[i])) {
	    return;
	  }
	}
	parse_error(option_name, "bad conversion name");
}

void check_opt_tls_model(char* model)
{
	if ( strcmp("global-dynamic", model) == 0 ||
	     strcmp("local-dynamic", model) == 0 ||
	     strcmp("initial-exec", model) == 0 ||
	     strcmp("local-exec", model) == 0 )
		return;
	else
	   error("`-ftls-model=%s': unknown tls-model option", model);
}

void
check_dashdash (void)
{
	if(xpg_flag)
	   dashdash_flag = 1;
	else
	   error("%s not allowed in non XPG4 environment", option_name);
}

static char *
Get_Binary_Name ( char *name)
{
  char *new;
  int len, i;
  new = string_copy(name);
  len = strlen(new);
  for ( i=0; i<len; i++ ) {
    if (strncmp(&new[i], ".x.Counts", 9) == 0) {
      new[i] = 0;
      break;
    }
  }
  return new;
}
 
void
Process_fbuse ( char *fname )
{
  static boolean is_first_count_file = TRUE;
  Use_feedback = TRUE;
  add_string (count_files, fname);
  if (is_first_count_file && (prof_file == NULL))
    prof_file = Get_Binary_Name(drop_path(fname));
  is_first_count_file = FALSE;
}

void
Process_fb_type ( char*  typename )
{
  char str[10];
  int flag, tmp;
  fb_type = string_copy(typename);
  sprintf(str,"fb_type=%s",fb_type);
  flag = add_string_option (O_OPT_, str);
  add_option_seen(flag);

  sscanf (typename, "%d", &tmp);
  profile_type |= tmp; 
}


void
Process_fb_create ( char *fname )
{
   int flag;
   fb_file = string_copy(fname);

   if (instrumentation_invoked == TRUE) {
     /* instrumentation already specified */
     flag = add_string_option (O_OPT_, "instr_unique_output=on");
   }
   else {
     toggle ( &instrumentation_invoked, TRUE );
     flag = add_string_option (O_OPT_, "instr=on:instr_unique_output=on");
   }
   add_option_seen (flag);
}


void 
Process_fb_phase(char *phase)
{
  char str[10];
  int flag;
  fb_phase = string_copy(phase);
  sprintf(str,"fb_phase=%s",fb_phase);
  flag = add_string_option (O_OPT_, str);
  add_option_seen(flag);
}


void
Process_fb_opt ( char *fname )
{
  opt_file = string_copy(fname);
  toggle ( &instrumentation_invoked, FALSE);
}


void
Process_fbexe ( char *fname )
{
  prof_file = string_copy(fname);
}

void
Process_fb_xdir ( char *fname )
{
  fb_xdir = string_copy(fname);
}

void
Process_fb_cdir ( char *fname )
{
  fb_cdir =  string_copy(fname);
}

void
Process_Tenv_Group ( char *opt_args )
{
  if ( debug ) {
    fprintf ( stderr, "Process_TENV_Group: %s\n", opt_args );
  }
  
  /* Go look for -TENV:mcmodel=xxx */
  if (strncmp (opt_args, "mcmodel=", 8) == 0) {
    set_memory_model (opt_args + 8);
  }
}

static int
print_magic_path(const char *base, const char *fname)
{
  int m32 = check_for_saved_option("-m32");
  char *slash;
  char *path;

  if (m32) {
    char *sfx;

    asprintf(&path, "%s/32/%s", base, fname);

    if (file_exists(path))
      goto good;
    
    if (ends_with(base, "/lib64")) {
      asprintf(&path, "%.*s/%s", (int)strlen(base) - 2, base, fname);

      if (file_exists(path))
	goto good;
    }

    sfx = get_suffix(fname);

    if (sfx != NULL &&	// bug 9049
	(!strcmp(sfx, "a") || !strcmp(sfx, "o") || !strcmp(sfx, "so")))
      goto bad;

    if ((slash = strrchr(path, '/')) && strstr(slash, ".so."))
      goto bad;
  }

  asprintf(&path, "%s/%s", base, fname);

  if (file_exists(path))
    goto good;
  
 bad:
  return 0;

 good:
  puts(path);
  return 1;
}

static int
print_phase_path(phases_t phase, const char *fname)
{
  return print_magic_path(get_phase_dir(phase), fname);
}

static int print_relative_path(const char *s, const char *fname)
{
  char *root_prefix = directory_path(get_executable_dir());
  char *base;

  asprintf(&base, "%s/%s", root_prefix, s);
  return print_magic_path(base, fname);
}

/* Keep this in sync with set_library_paths over in phases.c. */

void
print_file_path (char *fname, int exe)
{
  /* Search for fname in usual places, and print path when found. */
  /* gcc does separate searches for libraries and programs,
   * but that seems redundant as the paths are nearly identical,
   * so try combining into one search.
   */

  #ifdef PSC_TO_OPEN64
  if (print_relative_path("lib/" OPEN64_FULL_VERSION, fname))
  #endif
    return;

  if (print_phase_path(P_be, fname))
    return;

  if (print_phase_path(P_library, fname))
    return;

  if (print_phase_path(P_gcpp, fname))
    return;

  if (print_phase_path(P_gas, fname))
    return;

  if (print_phase_path(P_alt_library, fname))
    return;

  /* not found, so ask gcc */
  int m32 = check_for_saved_option("-m32");
  char *argv[4];
  phases_t lang = (invoked_lang == L_CC) ?  P_gcpp_plus :  P_gcpp;
  if (external_gcc == TRUE)
    argv[0] = get_phase_name(lang);
  else
    argv[0] = get_full_phase_name (lang);
  argv[1] = m32 ? "-m32" : "-m64";
  asprintf(&argv[2], "-print-%s-name=%s", exe ? "prog" : "file", fname);
  argv[3] = NULL;
  /* MINGW doesn't support execvp, everyone supports execlp */
  execlp(argv[0], argv[0], argv[1], argv[2], argv[3]);
  fprintf(stderr, "could not execute %s: %m\n", argv[0]);
  exit(1);
}

void
print_multi_lib ()
{
  char *argv[3];
  phases_t lang = (invoked_lang == L_CC) ? P_gcpp_plus :  P_gcpp;
  if (external_gcc == TRUE)
    argv[0] = get_phase_name(lang);
  else
    argv[0] = get_full_phase_name (lang);
  asprintf(&argv[1], "-print-multi-lib");
  argv[2] = NULL;
  /* MINGW doesn't support execvp, everyone supports execlp */
  execlp(argv[0], argv[0], argv[1], argv[2]);
  fprintf(stderr, "could not execute %s: %m\n", argv[0]);
  exit(1);
}

mem_model_t mem_model = M_SMALL;
char *mem_model_name = NULL;

void
set_memory_model(char *model)
{
  if (strcmp(model, "small") == 0) {
    mem_model = M_SMALL;
    mem_model_name = "small";
  }
  else if (strcmp(model, "medium") == 0) {
    mem_model = M_MEDIUM;
    mem_model_name = "medium";
  }
  else if (strcmp(model, "large") == 0) {
    mem_model = M_LARGE;
    mem_model_name = "large";
  }
  else if (strcmp(model, "kernel") == 0) {
    mem_model = M_KERNEL;
    mem_model_name = "kernel";
  } else {
    error("unknown memory model \"%s\"", model);
    mem_model_name = NULL;
  }
}

static struct 
{
  char *cpu_name;
  char *target_name;
  int abi;			// CPUs supporting ABI_64 also support ABI_N32
  boolean supports_sse2;	// TRUE if support SSE2
  boolean prefers_sse3;		// TRUE if target prefers code to use SSE3
  boolean supports_sse4a;       // TRUE if support SSE4a
  boolean supports_ssse3;       // TRUE if support SSSE3
  boolean supports_sse41;       // TRUE if support SSE41
  boolean supports_sse42;       // TRUE if support SSE42
  boolean supports_aes;         // TRUE if support AES
  boolean supports_pclmul;      // TRUE if support PCLMUL
  boolean supports_avx;         // TRUE if support AVX
  boolean supports_xop;         // TRUE if support XOP
  boolean supports_fma4;        // TRUE if support FMA4
  boolean supports_fma;         // TRUE if support FMA3
} supported_cpu_types[] = {
  { "any_64bit_x86",	"anyx86",	ABI_64,		TRUE,	FALSE, FALSE, 
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "any_32bit_x86",	"anyx86",	ABI_N32,	FALSE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "i386",	"anyx86",		ABI_N32,	FALSE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "i486",	"anyx86",		ABI_N32,	FALSE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "i586",	"anyx86",		ABI_N32,	FALSE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "athlon",	"athlon",		ABI_N32,	FALSE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "athlon-mp", "athlon",		ABI_N32,	FALSE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "athlon-xp", "athlon",		ABI_N32,	FALSE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "athlon64",	"athlon64",		ABI_64,		TRUE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "athlon64fx", "opteron",		ABI_64,		TRUE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "turion",	"athlon64",		ABI_64,		TRUE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "i686",	"pentium4",		ABI_N32,	FALSE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "ia32",	"pentium4",		ABI_N32,	TRUE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "k7",	"athlon",		ABI_N32,	FALSE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "k8",	"opteron",		ABI_64,		TRUE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "opteron",	"opteron",		ABI_64,		TRUE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "pentium4",	"pentium4",		ABI_N32,	TRUE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "xeon",	"xeon",			ABI_N32,	TRUE,	FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "em64t",	"em64t",		ABI_64,		TRUE,	TRUE,  FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "core",	"core",			ABI_64,		TRUE,	TRUE,  FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "wolfdale", "wolfdale",		ABI_64,		TRUE,	TRUE,  FALSE,
    TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { "bdver1",   "bdver1",		ABI_64,		TRUE,	TRUE,  TRUE,
    TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, FALSE  },
  { "bdver2",   "bdver2",		ABI_64,		TRUE,	TRUE,  TRUE,
    TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, TRUE  },
  { "barcelona","barcelona",		ABI_64,		TRUE,	TRUE,  TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE },
  { NULL,	NULL, },
};
  
char *target_cpu = NULL;

// Get the platform's default ABI.
static int
get_platform_abi()
{
#ifdef _WIN32
#ifdef WIN64
  return ABI_W64;
#endif
#endif

  struct utsname u;

  uname(&u);
  if (!strcmp(u.machine, "x86_64"))
    return ABI_64;
  return ABI_N32;
}

// Return the numeric value after ':' in a line in /proc/cpuinfo.
static int
get_num_after_colon (char *str)
{
  char *p;
  int num;

  p = strchr(str, ':');
  if (p == NULL) {
    error ("cannot parse /proc/cpuinfo: missing colon");
  }
  p++;
  if (sscanf(p, "%d", &num) == 0) {
    error ("cannot parse /proc/cpuinfo: missing number after colon");
  }
  return num;
}


// Return the CPU target name to default to as a last resort.
static char *
get_default_cpu_name (char *msg)
{
  char *cpu_name = NULL;
  char *abi_name = NULL;

  if (get_platform_abi() == ABI_64) {
    cpu_name = "anyx86";
    abi_name = "64-bit";
  } else {
    cpu_name = "anyx86";
    abi_name = "32-bit";
  }

  // NULL means not to warn.
  if (msg != NULL)
    warning("%s, defaulting to basic %s x86.", msg, abi_name);

  return cpu_name;
}


// Get CPU name from /proc/cpuinfo.
char *
get_auto_cpu_name ()
{
  FILE *f;
  char buf[256];
  char *cpu_name = NULL;
  char *cpu_name_64bit = NULL;		// cpu_name of 64-bit version of CPU
  int cpu_family = -1;			// CPU family number
  int model = -1;			// CPU model number
  boolean amd = FALSE;			// AMD CPU
  boolean intel = FALSE;		// Intel CPU

  f = fopen("/proc/cpuinfo", "r");
  if (f == NULL) {
    return get_default_cpu_name("cannot read /proc/cpuinfo");
  }

  // Parse /proc/cpuinfo.
  while (fgets(buf, 256, f) != NULL) {
    // vendor_id
    if (!strncmp("vendor_id", buf, 9)) {
      if (strstr(buf, "AuthenticAMD")) {
	amd = TRUE;
      } else if (strstr(buf, "GenuineIntel")) {
	intel = TRUE;
      } else {
	return get_default_cpu_name("unsupported vendor_id in /proc/cpuinfo");
      }

    // cpu family
    } else if (!strncmp("cpu family", buf, 10)) {
      cpu_family = get_num_after_colon(buf);

    // model
    } else if (!strncmp("model\t", buf, 6) ||
	       !strncmp("model   ", buf, 8)) {
      model = get_num_after_colon(buf);

    // model name
    } else if (!strncmp("model name", buf, 10)) {
      if (strstr(buf, "Opteron")) {		// AMD
	cpu_name = "opteron";
      } else if (strstr(buf, "Athlon")) {
	if (strstr(buf, "64"))
	  cpu_name = "athlon64";
	else if (strstr(buf, "MP"))
	  cpu_name = "athlon-mp";
	else
	  cpu_name = "athlon";
      } else if (strstr(buf, "Turion")) {
	cpu_name = "turion";
      } else if (strstr(buf, "Pentium") &&	// Intel
		 strstr(buf, "4")) {
	cpu_name = "pentium4";
      } else if (strstr(buf, "Xeon")) {
	cpu_name = "xeon";
	cpu_name_64bit = "em64t";
      } else if (strstr(buf, "i7")) {
          // TODO -- need-to-date machine model.
	cpu_name = "wolfdale";
	cpu_name_64bit = "wolfdale";
      }
    } else if (strstr(buf, "GenuineIntel")) {
      intel = TRUE;
    } else if (strstr(buf, "AuthenticAMD")) {
      amd = TRUE;
    }
  }

  fclose(f);

  // If /proc/cpuinfo doesn't have a supported model name, try to derive one
  // from the family and model numbers.  If that fails, fall back to a default.
  // Bug 5785.
  // need to differentiate Core-based Xeon's
  if (intel == TRUE && (cpu_name == NULL || !strcmp(cpu_name, "xeon"))) {
    switch (cpu_family) {
      case 4:			// most 80486s
      case 5:			// Intel P5, P54C, P55C, P24T
        return "i386";

      case 6:			// P6, Core, ...
        if (model == 7 ||	// Harpertown	bug 14685
            model == 23 ||	// Wolfdale
            model == 26)	// Nehalem
          return "wolfdale";

        if (model >= 15)
          return "core";

        // Treat the rest of the P6 family as generic x86 since we don't
        // optimize for them.
        return "i386";

      case 15:		// P4
        cpu_name = "xeon";
        cpu_name_64bit = "em64t";
        break;
    }

  } else if (amd == TRUE) {
    switch (cpu_family) {
      case 4:                   // 5x86
      case 5:                   // K5, K6
      case 6:                   // K7
        return "athlon";
      case 15:
        return "opteron";       // Family 0fh (K8)
      case 16:
        return "barcelona";     // Family 10h
      case 17:
      case 18:
      case 20:
        return "opteron";       // Generic tuning for Family 11h, 12h, 14h
      case 21:
        return "bdver1";        // Family 15h
    }
  }

  if (cpu_name == NULL) {
      return get_default_cpu_name("cannot deduce a supported CPU name"
                                  " from /proc/cpuinfo");
  }

  // If cpuinfo doesn't say if CPU is 32 or 64-bit, ask the OS.
  if (cpu_name_64bit != NULL) {
    if (get_platform_abi() == ABI_64) {
      cpu_name = cpu_name_64bit;
    }
  }

  return cpu_name;
}

// Find the target name from the CPU name.
static void
set_cpu(char *name, m_flag flag_type)
{
  // If parsing the default options, don't change the target cpu if it is
  // already set.
  if (parsing_default_options &&
      target_cpu != NULL) {
    drop_option = TRUE;
    return;
  }

  // Warn if conflicting CPU targets are specified.
  // XXX We are not compatible with gcc here, which assigns different meanings to the
  // -march, -mtune and -mcpu flags.  We treat them as synonyms, which we should not.
  if (target_cpu != NULL &&
      strcmp(target_cpu, name)) {
    warning("CPU target %s conflicts with %s; using latter (%s)",
	    get_toggle_name((int*)&target_cpu), name, name);
    // Reset target_cpu so that the driver will complain if a new value can't
    // be determined.
    target_cpu = NULL;
  }
  target_cpu = name;
  add_toggle_name((int*)&target_cpu, name);
}

#ifdef TARG_X8664
static void
Get_x86_ISA ()
{
  char *name = NULL;

  // Get a more specific cpu name.
  if (!strcmp(target_cpu, "auto")) {		// auto
    target_cpu = get_auto_cpu_name();		// may return anyx86
    if (target_cpu == NULL)
      return;
  }
  if (!strcmp(target_cpu, "anyx86")) {		// anyx86
    // Need ABI to select any_32bit_x86 or any_64bit_x86 ISA.
    if (abi == UNDEFINED) {
      if (get_platform_abi() == ABI_64) {
	abi = ABI_64;
	add_option_seen(O_m64);
      } else {
	abi = ABI_N32;
	add_option_seen(O_m32);
      }
    }
    switch (abi) {
      case ABI_N32:	name = "any_32bit_x86"; break;
      case ABI_64:	name = "any_64bit_x86"; break;
      default:		internal_error("illegal ABI");
    }
  } else
    name = target_cpu;

  for (int i = 0; supported_cpu_types[i].cpu_name; i++) {
    if (strcmp(name, supported_cpu_types[i].cpu_name) == 0) {
      target_cpu = supported_cpu_types[i].target_name;
      target_supported_abi = supported_cpu_types[i].abi;
      target_supports_sse2 = supported_cpu_types[i].supports_sse2;
      target_prefers_sse3 = supported_cpu_types[i].prefers_sse3;
      target_supports_sse4a = supported_cpu_types[i].supports_sse4a;
      target_supports_ssse3 = supported_cpu_types[i].supports_ssse3;
      target_supports_sse41 = supported_cpu_types[i].supports_sse41;
      target_supports_sse42 = supported_cpu_types[i].supports_sse42;
      target_supports_aes = supported_cpu_types[i].supports_aes;
      target_supports_pclmul = supported_cpu_types[i].supports_pclmul;
      target_supports_avx = supported_cpu_types[i].supports_avx;
      target_supports_xop = supported_cpu_types[i].supports_xop;
      target_supports_fma4 = supported_cpu_types[i].supports_fma4;
      target_supports_fma = supported_cpu_types[i].supports_fma;
      break;
    }
  }

  if (target_cpu == NULL) {
    error("unknown CPU type \"%s\"", name);
  }
}

// Return TRUE if there is no error, else return FALSE.
static boolean
Get_x86_ISA_extensions ()
{
  // Quit if the user requests an ISA extension that is not available on the
  // target processor.  Add extensions as necessary.  Bug 9692.
  if (sse2 == TRUE &&
      !target_supports_sse2) {
    error("Target processor does not support SSE2.");
    return FALSE;
  }

  if (sse4a == TRUE &&
      !target_supports_sse4a) {
    error("Target processor does not support SSE4a.");
    return FALSE;
  }

  if (ssse3 == TRUE &&
      !target_supports_ssse3) {
    error("Target processor does not support SSSE3.");
    return FALSE;
  }

  if (sse41 == TRUE &&
      !target_supports_sse41) {
    error("Target processor does not support SSE41.");
    return FALSE;
  }

  if (sse42 == TRUE &&
      !target_supports_sse42) {
    error("Target processor does not support SSE42.");
    return FALSE;
  }

  if (aes == TRUE &&
      !target_supports_aes) {
    error("Target processor does not support AES.");
    return FALSE;
  }

  if (pclmul == TRUE &&
      !target_supports_pclmul) {
    error("Target processor does not support PCLMUL.");
    return FALSE;
  }

  if (avx == TRUE &&
      !target_supports_avx) {
    error("Target processor does not support AVX.");
    return FALSE;
  }

  if (xop == TRUE &&
      !target_supports_xop) {
    error("Target processor does not support XOP.");
    return FALSE;
  }

  if (fma == TRUE &&
      !target_supports_fma) {
    error("Target processor does not support FMA3.");
    return FALSE;
  }

  if (fma4 == TRUE &&
      !target_supports_fma4) {
    error("Target processor does not support FMA4.");
    return FALSE;
  }

  if (abi == UNDEFINED) {
    internal_error("Get_x86_ISA_extensions: ABI undefined\n");
    return FALSE;
  }

  // For x86-64, 64-bit code always use SSE2 instructions.
  if (abi == ABI_64) {
    if (sse2 == FALSE) {
      warning("SSE2 required for 64-bit ABI; enabling SSE2.");
    }
    sse2 = TRUE;
  } else {
    // For m32, use SSE2 on systems that support it.
    if (sse2 == UNDEFINED &&
	target_supports_sse2) {
      sse2 = TRUE;
    }
  }

  // Use SSE3 on systems that prefer it.
  if (target_prefers_sse3 &&
      sse2 != FALSE &&
      sse3 != FALSE) {
    sse2 = TRUE;
    sse3 = TRUE;
  }

  if (target_supports_ssse3 &&
      sse2 != FALSE &&  
      ssse3 != FALSE){//not explicitly turned off
    sse2 = TRUE;
    ssse3 = TRUE;
  }
  if (target_supports_sse41 &&
      sse2 != FALSE &&  
      sse41 != FALSE){//not explicitly turned off
    sse2 = TRUE;
    sse41 = TRUE;
  }
  if (target_supports_sse42 &&
      sse2 != FALSE &&  
      sse42 != FALSE){//not explicitly turned off
    sse2 = TRUE;
    sse42 = TRUE;
  }
  if (target_supports_aes &&
      sse2 != FALSE &&  
      aes != FALSE){//not explicitly turned off
    sse2 = TRUE;
    aes = TRUE;
  }
  if (target_supports_pclmul &&
      sse2 != FALSE &&  
      pclmul != FALSE){//not explicitly turned off
    sse2 = TRUE;
    pclmul = TRUE;
  }
  if (target_supports_avx &&
      sse2 != FALSE &&  
      avx != FALSE){//not explicitly turned off
    sse2 = TRUE;
    avx = TRUE;
  }
  if (target_supports_xop &&
      sse2 != FALSE &&  
      xop != FALSE){//not explicitly turned off
    sse2 = TRUE;
    xop = TRUE;
  }
  if (target_supports_fma &&
      sse2 != FALSE &&  
      fma != FALSE){//not explicitly turned off
    sse2 = TRUE;
    fma = TRUE;
  }
  if (target_supports_fma4 &&
      sse2 != FALSE &&  
      fma4 != FALSE){//not explicitly turned off
    sse2 = TRUE;
    fma4 = TRUE;
  }
 
  // No error.  Don't count warnings as errors.
  return TRUE;
}
#endif

static void
accumulate_isystem(char *optargs)
{
  if (!isystem_dirs) {
    isystem_dirs = init_string_list();
  }
# define INCLUDE_EQ "-include="
  char *temp = malloc(strlen(optargs) + sizeof INCLUDE_EQ);
  add_string(isystem_dirs, strcat(strcpy(temp, INCLUDE_EQ), optargs));
}

static void add_hugepage_desc
(
    HUGEPAGE_ALLOC alloc,
    HUGEPAGE_SIZE  size,
    int            limit
)
{
    HUGEPAGE_DESC desc;

    if (((alloc == ALLOC_BDT) || (alloc == ALLOC_BSS))
        && (limit != HUGEPAGE_LIMIT_DEFAULT))
        warning("Can't set huge page limit for %s in the command line.  Use HUGETLB_ELF_LIMIT instead",
                hugepage_alloc_name[alloc]);

    /* check whether to override existing descriptors */

    for (desc = hugepage_desc; desc != NULL; desc = desc->next) {
        if (desc->alloc == alloc) {
            if ((desc->size != size) || (desc->limit != limit)) {
                warning("conflict values for huge page %s; using latter values",
                        hugepage_alloc_name[alloc]);
            }

            desc->size = size;
            desc->limit = limit;
            return;
        }
    }
    
    desc = malloc(sizeof(HUGEPAGE_DESC_TAG));
    
    desc->alloc = alloc;
    desc->size = size;
    desc->limit = limit;

    desc->next = hugepage_desc;
    hugepage_desc = desc; 
}

static void
Process_Hugepage_Default()
{
    add_hugepage_desc(HUGEPAGE_ALLOC_DEFAULT, HUGEPAGE_SIZE_DEFAULT, HUGEPAGE_LIMIT_DEFAULT);
    add_option_seen(O_HP);
}

static boolean hugepage_warn = FALSE;

static void
Process_Hugepage_Group(char * hugepage_args)
{
    char * p = hugepage_args;
    boolean has_err = FALSE;
    int process_state = 0;
    HUGEPAGE_ALLOC hugepage_alloc;
    HUGEPAGE_SIZE  hugepage_size;
    int hugepage_limit;

    /* set default values */
    hugepage_alloc = HUGEPAGE_ALLOC_DEFAULT;
    hugepage_size = HUGEPAGE_SIZE_DEFAULT;
    hugepage_limit = HUGEPAGE_LIMIT_DEFAULT;
    
    while (*p) {
        if (process_state == 1) {
            if (strncmp(p, "2m", 2) == 0) {
                hugepage_size = SIZE_2M;
                p += 2;
                process_state = 2;
            }
            else if (strncmp(p, "1g", 2) == 0) {
                hugepage_size = SIZE_1G;
                p += 2;
                process_state = 2;
            }
            else
                has_err = TRUE;
        }
        else if (process_state == 2) {
            if (strncmp(p, "limit=", 6) == 0) {
                boolean is_neg = FALSE;
                p = &p[6];

                if ((*p) && ((*p) == '-')) {
                    p++;
                    is_neg = TRUE;
                }

                if (!(*p) || !isdigit(*p))
                    has_err = TRUE;
                else {
                    sscanf(p, "%d", &hugepage_limit);

                    if (is_neg && (hugepage_limit > 0))
                        hugepage_limit = -1;
                    
                    while ((*p) && ((*p) >= '0') && ((*p) <= '9'))
                        p++;
                    
                    process_state = 3;
                }
            }
            else
                has_err = TRUE;
        }
        else if (strncmp(p, "heap=", 5) == 0) {
            p = &p[5];
            hugepage_alloc = ALLOC_HEAP;
            process_state = 1;
            if (!(*p))
                has_err = TRUE;
            else
                continue;
        }
        else if (strncmp(p, "bd=", 3) == 0) {
            p = &p[3];
            hugepage_alloc = ALLOC_BD;
            process_state = 1;
            if (!(*p))
                has_err = TRUE;
            else
                continue;
        }
        else if (strncmp(p, "bdt=", 4) == 0) {
            p = &p[4];
            hugepage_alloc = ALLOC_BDT;
            process_state = 1;
            if (!(*p))
                has_err = TRUE;
            else
                continue;
        }
        else
            has_err = TRUE;

        if (*p) {
            if ((*p) == ',') {
                if (!has_err)
                    p++;

                if ((process_state != 2) || !(*p))
                    has_err = TRUE;
            }
            else if ((*p) == ':') {
                if (!has_err) {
                    p++;
                    add_hugepage_desc(hugepage_alloc, hugepage_size, hugepage_limit);
                }
                
                hugepage_alloc = HUGEPAGE_ALLOC_DEFAULT;
                hugepage_size = HUGEPAGE_SIZE_DEFAULT;
                hugepage_limit = HUGEPAGE_LIMIT_DEFAULT;
                process_state = 0;
            }
        }
        else if (!has_err) 
            add_hugepage_desc(hugepage_alloc, hugepage_size, hugepage_limit);

        if (has_err) {
            if (!hugepage_warn) {
                hugepage_warn = TRUE;
                warning("Illegal argument: %s in -HP", p);
            }
            break;
        }
    }

    if (!has_err) 
        add_option_seen(O_HP);
}

#ifdef TARG_X8664

static void
Process_fp(char *level)
{
    if (!strcmp(level, "strict")) {
	add_option_seen(add_string_option(O_OPT_, "IEEE_arith=1"));
	add_option_seen(add_string_option(O_OPT_, "roundoff=0"));
    }
    else if (!strcmp(level, "strict-contract")) {
	/* Same as strict but allow contractions like fma (floating
	   point multiply and add) if they are available.  */
	add_option_seen(add_string_option(O_OPT_, "IEEE_arith=1"));
	add_option_seen(add_string_option(O_OPT_, "roundoff=0"));
    }
    else if (!strcmp(level, "relaxed")) {
	add_option_seen(add_string_option(O_OPT_, "IEEE_arith=2"));
	add_option_seen(add_string_option(O_OPT_, "roundoff=1"));
    }
    else if (!strcmp(level, "aggressive")) {
	add_option_seen(add_string_option(O_OPT_, "IEEE_arith=3"));
	add_option_seen(add_string_option(O_OPT_, "roundoff=2"));
	add_option_seen(add_string_option(O_TENV_, "simd_amask=off"));
	add_option_seen(add_string_option(O_TENV_, "simd_fmask=off"));
    }
    else {
	warning("Ignored illegal argument: %s in -fp-accuracy", level);
    }
}
#endif

#include "opt_action.i"
